unit sgTextRectList;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType,
{$ENDIF}
  Classes, sgConsts, sgFunction, sgLists,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, System.UITypes,
{$ELSE}
  Graphics,
{$ENDIF}
  SHX, Math;

const
  cnstGUID_TextRectFinder = '{62D87E06-D007-48BB-B30A-FA9A3B7CA0FE}';

type
  TsgFontFace = record
    Height: Double;
    PDFHeight: Double;
    FontFound: Boolean;
    FontName: string;
    FontStyle: TFontStyles;
    Orientation: Integer;
    Rotation: Double;
    SHXFont: string;
    ObliqueAngle: Double;
  end;

  TsgTextRectListItem = class
  private
    FFontFace: TsgFontFace;
    FRect: TFRect;
    FText: string;

    procedure SetFontFace(const AValue: TsgFontFace);
    procedure SetRect(const AValue: TFRect);
    procedure SetText(const AValue: string);
  public
    constructor Create;
    function IsTextInRect(ARect: TFRect; var AIsSubstring: Boolean;
      var AWeigth: Double): Boolean;
    function IsTextIsLine: Boolean;
    property FontFace: TsgFontFace read FFontFace write SetFontFace;
    property Rect: TFRect read FRect write SetRect;
    property Text: string read FText write SetText;
  end;

  IsgTextRectFinder = interface(IInterface)
    [cnstGUID_TextRectFinder]
    function GetMatrix: TFMatrix;
    function FindItem(const ARect: TFRect; var AIsSubstring: Boolean): TsgTextRectListItem;
  end;

  TsgTextRectList = class(TsgList, IsgTextRectFinder)
  private
    FMatrix: TFMatrix;
    function GetFirst: TsgTextRectListItem;
    function GetLast: TsgTextRectListItem;
    function GetItem(const AIndex: Integer): TsgTextRectListItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetMatrix: TFMatrix;
    procedure SetFirst(const Value: TsgTextRectListItem);
    procedure SetLast(const Value: TsgTextRectListItem);
    procedure SetItem(const AIndex: Integer; const Item: TsgTextRectListItem);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetMatrix(AValue: TFMatrix);
  public
    destructor Destroy; override;
    function Add(const Item: TsgTextRectListItem): Integer;
    function FindItem(const ARect: TFRect; var AIsSubstring: Boolean): TsgTextRectListItem;
    procedure Insert(Index: Integer; const Item: TsgTextRectListItem);
    property First: TsgTextRectListItem read GetFirst write SetFirst;
    property Items[const AIndex: Integer]: TsgTextRectListItem
      read GetItem write SetItem; default;
    property Last: TsgTextRectListItem read GetLast write SetLast;
    property Matrix: TFMatrix read FMatrix write SetMatrix;
  end;

function TextStyleToFontFace(const ALogFont: TLogFontW): TsgFontFace;

implementation

{ TsgTextRectListItem }

constructor TsgTextRectListItem.Create;
begin
  FRect := cnstBadRect;
  FText := '';
  FFontFace.FontName := '';
  FFontFace.Height := 0;
  FFontFace.FontStyle := [];
  FFontFace.Orientation := 0;
  FFontFace.Rotation := 0;
end;

procedure TsgTextRectListItem.SetFontFace(const AValue: TsgFontFace);
begin
  FFontFace := AValue;
end;

procedure TsgTextRectListItem.SetRect(const AValue: TFRect);
begin
  FRect := AValue;
end;

procedure TsgTextRectListItem.SetText(const AValue: string);
begin
  FText := AValue;
end;

function TsgTextRectListItem.IsTextInRect(ARect: TFRect;
  var AIsSubstring: Boolean; var AWeigth: Double): Boolean;
const
  cnstMaxDistance = 0.18;
  cnstMaxDistanceRot = 0.23;
  cnstMaxAreaRatio = 1.2;
  cnstMinCharsRatio = 0.76;
var
  vDistanceX, vDistanceY, vSizeX, vSizeY, vMaxDistance: Double;
  vIsSubX,vIsSubY,vIsCheckX,vIsCheckY: Boolean;
  vAreaText,vAreaRect,vAreaLetter: Double;
  vCenter: TFPoint;

  function CountSpaceChars: Integer;
  begin
    Result := 0;
    while (Result < Length(FText)) and (FText[Result+1] = cnstSpace) do
      inc(Result);
  end;

  function CheckParams(const ANumerator,ADenominator,AMax: Double): Boolean;
  begin
    if IsZero(ADenominator) then
      Result := False
    else
      Result := ANumerator/ADenominator < AMax;
  end;

  function CheckSubedge(ALI,ARI,ALE,ARE: Double): Boolean;
  begin
    Result := IsValInParam(ALI, ALE, ARE) and IsValInParam(ARI, ALE, ARE);
  end;

  function CheckIntersectOrSub(ALI,ARI,ALE,ARE: Double): Boolean;
  begin
    Result := IsValInParam(ALI, ALE, ARE) or IsValInParam(ARI, ALE, ARE);
  end;

begin
  AWeigth := 0;
  Result := False;
  if IsEqual(FFontFace.Rotation, 0) then
    vMaxDistance := cnstMaxDistance
  else
    vMaxDistance := cnstMaxDistanceRot;
  if CountSpaceChars = 0 then
    vDistanceX := Max(Sqr(ARect.Left - FRect.Left), Sqr(ARect.Right - FRect.Right))
  else
    vDistanceX := Sqr(ARect.Right - FRect.Right);
  vDistanceY := Max(Sqr(ARect.Top - FRect.Top), Sqr(ARect.Bottom - FRect.Bottom));
  vSizeX := Min(Sqr(FRect.Left - FRect.Right), Sqr(ARect.Left - ARect.Right));
  vSizeY := Min(Sqr(FRect.Top - FRect.Bottom), Sqr(ARect.Top - ARect.Bottom));
  vIsCheckX := CheckParams(vDistanceX, vSizeX, vMaxDistance);
  vIsCheckY := CheckParams(vDistanceY, vSizeY, vMaxDistance);
  vAreaText := GetAreaOfRect2D(FRect);
  vAreaRect := GetAreaOfRect2D(ARect);
  if vIsCheckX and vIsCheckY then
  begin
    if cnstExtractSubstrings then
    begin
      if Length(FText) = 1 then
        Result := True
      else
      begin
        vAreaLetter := vAreaText / Length(FText);
        Result := Ceil(vAreaRect/vAreaLetter) / Length(FText) >= cnstMinCharsRatio;
      end;
    end
    else
      Result := True;
  end;
  if not Result then
  begin
    if CheckIntersectOrSub(ARect.Left, ARect.Right, FRect.Left, FRect.Right) and
      CheckIntersectOrSub(ARect.Top, ARect.Bottom, FRect.Top, FRect.Bottom) then
    begin
      vIsSubX := CheckSubedge(ARect.Left, ARect.Right, FRect.Left, FRect.Right);
      vIsSubY := CheckSubedge(ARect.Top, ARect.Bottom, FRect.Top, FRect.Bottom);
      AIsSubstring := vIsSubX or vIsSubY;
      if AIsSubstring then
      begin
        if vIsSubX and vIsSubY then
          Result := True
        else
          Result := CheckParams(vAreaRect, vAreaText, cnstMaxAreaRatio);
      end;
      if Result then
      begin
        vCenter := GetCenterOfRect(ARect);
        if not IsPointInFRect2D(FRect, vCenter) then
          AWeigth := DistanceFPoint(GetCenterOfRect(FRect), vCenter);
      end;
    end;
  end
  else
    AIsSubstring := False;
end;

function TsgTextRectListItem.IsTextIsLine: Boolean;
const
  cnstMinus = '-';
  cnstUnderline = '_';

  function IsTextDup(AChr: Char): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 1 to Length(FText) do
      if FText[i] <> AChr then
      begin
        Result := False;
        Exit;
      end;
  end;

begin
  Result := IsTextDup(cnstMinus) or IsTextDup(cnstUnderline);
end;

{ TsgTextRectList }

destructor TsgTextRectList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Destroy;
end;

function TsgTextRectList.GetFirst: TsgTextRectListItem;
begin
  Result := inherited First;
end;

function TsgTextRectList.GetLast: TsgTextRectListItem;
begin
  Result := inherited Last;
end;

function TsgTextRectList.GetItem(const AIndex: Integer): TsgTextRectListItem;
begin
  Result := inherited Items[AIndex];
end;

function TsgTextRectList.GetMatrix: TFMatrix;
begin
  Result := FMatrix;
end;

procedure TsgTextRectList.SetFirst(const Value: TsgTextRectListItem);
begin
  inherited First := Pointer(Value);
end;

procedure TsgTextRectList.SetLast(const Value: TsgTextRectListItem);
begin
  inherited Last := Pointer(Value);
end;

procedure TsgTextRectList.SetItem(const AIndex: Integer;
  const Item: TsgTextRectListItem);
begin
  inherited Items[AIndex] := Pointer(Item);
end;

procedure TsgTextRectList.SetMatrix(AValue: TFMatrix);
begin
  FMatrix := AValue;
end;

function TsgTextRectList.Add(const Item: TsgTextRectListItem): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TsgTextRectList.FindItem(const ARect: TFRect; var AIsSubstring: Boolean): TsgTextRectListItem;
var
  I: Integer;
  vItem,vSubstrItem,vFullItem: TsgTextRectListItem;
  vRect: TFRect;
  vIsSubstring: Boolean;
  vWeight,vMinWeight: Double;
begin
  vFullItem := nil;
  vSubstrItem := nil;
  vRect.TopLeft := FPointXMat(ARect.TopLeft, FMatrix);
  vRect.BottomRight := FPointXMat(ARect.BottomRight, FMatrix);
  vMinWeight := fMaxDoubleValue;
  for I := 0 to Count - 1 do
  begin
    vItem := Items[I];
    if (not vItem.IsTextIsLine) and vItem.IsTextInRect(vRect, vIsSubstring, vWeight) then
    begin
      if vIsSubstring then
      begin
        if vWeight < vMinWeight then
        begin
          vMinWeight := vWeight;
          vSubstrItem := vItem;
        end;
      end
      else
      begin
        vFullItem := vItem;
        Break;
      end;
    end;
  end;
  if Assigned(vFullItem) then
  begin
    Result := vFullItem;
    AIsSubstring := False;
  end
  else
    if Assigned(vSubstrItem) then
    begin
      Result := vSubstrItem;
      AIsSubstring := True;
    end
    else
    begin
      Result := nil;
      AIsSubstring := False;
    end;
end;

procedure TsgTextRectList.Insert(Index: Integer; const Item: TsgTextRectListItem);
begin
  inherited Insert(Index, Pointer(Item));
end;

function TextStyleToFontFace(const ALogFont: TLogFontW): TsgFontFace;
begin
  Result.FontStyle := [];
  Result.FontName := string(WideString(ALogFont.lfFaceName));

  if ALogFont.lfWeight > 400 then
    Include(Result.FontStyle, fsBold);
  if ALogFont.lfItalic > 0 then
    Include(Result.FontStyle, fsItalic);
  if ALogFont.lfUnderline > 0 then
    Include(Result.FontStyle, fsUnderline);
  if ALogFont.lfStrikeOut > 0 then
    Include(Result.FontStyle, fsStrikeOut);

  Result.Height := Abs(ALogFont.lfHeight);
  Result.Orientation := ALogFont.lfOrientation;
  Result.Rotation := ALogFont.lfEscapement / 10;
end;

end.

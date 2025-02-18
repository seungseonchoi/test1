unit sgSHXResolver;

{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF SG_FIREMONKEY}
{$ELSE}
  Graphics,
{$ENDIF}
  SysUtils, Classes, Math, CADImage, DXFConv, sgLists,
  sgLines, Contnrs, sgFunction, sgConsts, TTF, CADExport, sgTextRectList,
  sgComparer, SHX, {$IFDEF SGDEL_7}StrUtils,{$ENDIF} sgXMLParser;

const
  cnstPointResolution = 0.1;
  cnstAngleResolution = 0.1;
  cnstDistanceResolution = 0.01;
  cnstMinDistanceResolution = 1.2;
  cnstArcItemLenPerDiagonal = 8;
  cnstNoStrictDistanceResolution = 1.5;

type
  TsgPointPosition = (ppUndefined, ppLeft, ppRight, ppAbove, ppBelow,
    ppLeftAbove, ppRightAbove, ppLeftBelow, ppRightBelow);
  TsgBoxSize = (bsUndefined, bsEqual, bsLittle, bsBig);
  TsgDrawLineType = (dltUndefined, dltHorizontal, dltVertical, dltDiagonal,
    dltDelimeter);
  TsgSHXCompareMetod = (scmOnlyGeomerty, scmHash);

  TsgSHXGeometry = class;
  TsgSHXIndex = class;
  TsgSHXIndexNode = class;

  TsgCriptMethod = function(const AStr, AKey: AnsiString): AnsiString;

  PsgSHXGeometrySign = ^TsgSHXGeometrySign;

  TsgSHXGeometrySign = record
    Lines: Integer;
    Arcs: Integer;
    AnglesSum: Double;
    MaxLength: Double;
    MaxAngle: Double;
    Position: TsgPointPosition;
    Size: TsgBoxSize;
    GeometryHash: Integer;
    Angles: array [0 .. 7] of Double;
    Lengths: array [0 .. 7] of Double;
  end;

  PsgSHXCharDescription = ^TsgSHXCharDescription;

  TsgSHXCharDescription = record
    Symbol: WideChar;
    Font: string;
  end;

  TsgSHXIndexNode = class
  private
    FAngle: Double;
    FObliqueAngle: Double;
    FSign: TsgSHXGeometrySign;
    FNextNodes: TList;
    FPrevNode: TsgSHXIndexNode;
    FSymbols: TList;
  public
    constructor Create(ASign: TsgSHXGeometrySign; AAngle: Double;
      AObliqueAngle: Double; APrevNode: TsgSHXIndexNode = nil);
    destructor Destroy; override;
    property Angle: Double read FAngle;
    property ObliqueAngle: Double read FObliqueAngle;
    property Sign: TsgSHXGeometrySign read FSign;
    property NextNodes: TList read FNextNodes;
    property PrevNode: TsgSHXIndexNode read FPrevNode;
    property Symbols: TList read FSymbols;
  end;

  TsgSHXIndexForLinesItem = class
  private
    FIsFreeNodes: Boolean;
    FLines: Integer;
    FNodes: TList;
  public
    constructor Create(ALines: Integer; AIsFreeNodes: Boolean);
    destructor Destroy; override;
    property IsFreeNodes: Boolean read FIsFreeNodes;
    property Lines: Integer read FLines;
    property Nodes: TList read FNodes;
  end;

  TsgSHXIndex = class
  private
    FEncript: TsgCriptMethod;
    FDecript: TsgCriptMethod;
    FIndexForLines: TList;
    FSymbols: TList;
    FNodes: TList;
    FStartNodes: TList;
  public
    constructor Create(AEncript, ADecript: TsgCriptMethod);
    destructor Destroy; override;
    procedure AddSHXFont(AFont: string);
    procedure AddSymbol(AGeometry: TsgSHXGeometry; ASymbol: WideChar;
      AFont: string; AObliqueAngle: Double);
    procedure CreateForFonts(APathsList: string);
    procedure CreateIndexForLines;
    function FindNode(ANodes: TList; ASign: TsgSHXGeometrySign;
      var ANode: TsgSHXIndexNode; AStrict: Boolean = True): Boolean;
    function FindSymbol(AGeometry: TsgSHXGeometry; var ANode: TsgSHXIndexNode;
      var ASymbols: TList; var AAngle: Double;
      var AObliqueAngle: Double): Boolean;
    procedure OpenIndex(AFileName, APathsList: string);
    function ReadFromFile(AFileName: string): Boolean;
    procedure WriteToFile(AFileName: string);
    property Encript: TsgCriptMethod read FEncript;
    property Decript: TsgCriptMethod read FDecript;
    property IndexForLines: TList read FIndexForLines;
    property Nodes: TList read FNodes;
    property StartNodes: TList read FStartNodes;
  end;

  TsgSHXGeometry = class
  private
    FAngle: Double;
    FComponents: TList;
  public
    constructor Create(AAngle: Double);
    destructor Destroy; override;
    procedure AddSign(AGS: TsgSHXGeometrySign);
    procedure Clear;
    function MaxLength: Double;
    property Angle: Double read FAngle;
    property Components: TList read FComponents;
  end;

  TsgSHXSymbol = class
  private
    FAngle: Double;
    FConverter: TsgDXFConverter;
    FEntities: TList;
    FObliqueAngle: Double;
    FSymbols: TList;

    function GetFont: string;
    function GetSymbol: WideChar;
  public
    constructor Create(AConverter: TsgDXFConverter; ASymbols: TList;
      AAngle: Double; AObliqueAngle: Double);
    destructor Destroy; override;
    function Box: TFRect;
    function ColorCAD: TsgColorCAD;
    function IsSimilarFont(ASymbol: TsgSHXSymbol): Boolean;
    property Angle: Double read FAngle;
    property Entities: TList read FEntities;
    property ObliqueAngle: Double read FObliqueAngle;
    property Font: string read GetFont;
    property Symbol: WideChar read GetSymbol;
    property Symbols: TList read FSymbols;
  end;

  TsgSHXEntitiesGroupItem = class
  private
    FBox: TFRect;
    FEntities: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AEntity: TsgDXFEntity);
    function IsEntityInGroup(AEntity: TsgDXFEntity): Boolean;
    property Box: TFRect read FBox;
    property Entities: TList read FEntities;
  end;

  TsgSHXResolver = class
  private
    FConverter: TsgDXFConverter;
    FEntities: TList;
    FResolved: TList;
    FIndex: Integer;
    FSHXInfo: TsgSHXIndex;
    FSymbols: TList;
    function FindSymbol(var ASymbols: TList; var AAngle: Double;
      var AObliqueAngle: Double; var AUseLast: Boolean;
      var ANewIndex: Integer): Boolean;
    function IsEndOfString(ASymbol: TsgSHXSymbol): Boolean;
    function IsIdenticalStyle(ASymbol: TsgSHXSymbol): Boolean;
    function IsSpace(ASymbol: TsgSHXSymbol): Boolean;
    function TestGeometry(const AGroup: TsgSHXEntitiesGroupItem;
      const AV: TFPoint; var ANode: TsgSHXIndexNode; var ASymbols: TList;
      var AAngle: Double; var AObliqueAngle: Double;
      var APrevBox: TFRect): Boolean;
  public
    constructor Create(AConverter: TsgDXFConverter; AIndexName: string;
      AEncript, ADecript: TsgCriptMethod);
    destructor Destroy; override;
    function AddEntity(AEntity: TsgDXFEntity): Boolean;
    procedure BeginRead;
    procedure Clear;
    function ReadText(var AText: string; var AFont: string; var ABox: TFRect;
      var AColor: TsgColorCAD; var AAngle: Double;
      var AObliqueAngle: Double): Boolean;
    property TextIndex: Integer read FIndex;
  end;

const
  cnstDefaultEncriptMethod: TsgCriptMethod = nil;
  cnstDefaultDecriptMethod: TsgCriptMethod = nil;
  cnstDefaultSHXIndex: string = '';

function GetEntitiesBox(AEntities: TList): TFRect;
function GetSymbolsBox(ASymbols: TList): TFRect;
function GetSymbolsFont(ASymbols: TList): string;
function GetPolylineBox(APolyline: TList): TFRect;
function GetPolyPolylineEpsilon(APolyPolyline: TList): Double;
function IsEntityInGroupList(AEntities: TList; AEntity: TsgDXFEntity): Boolean;
function IsPolylineInGroup(AGroup, APolyline: TList;
  var AIsReverseL,AIsReverseG: Boolean; AEpsilon: Double): Boolean;
function MakeGeometryForEntities(AList: TList; var APrevBox: TFRect;
  const AV: PFPoint = nil): TsgSHXGeometry;
procedure EntityToPointList(AEntity: TsgDXFEntity; AList: TList);

function BuildGeometryForPolyPolyline(APolyPolyline: TList;
  var APrevBox: TFRect; const AV: PFPoint = nil;
  ALikeEpsilon: Double = cnstArcItemLenPerDiagonal): TsgSHXGeometry;
function BuildGeometryForPolyline(APolyline: TList; AV: TFPoint; var APrevBox: TFRect;
  ALikeEpsilon: Double = cnstArcItemLenPerDiagonal): TsgSHXGeometrySign;
function BuildGeometryForSHXVertexes(AVertexes: TList;
  var APrevBox: TFRect): TsgSHXGeometry;
function GetEntityMiddlePoint(AEntity: TsgDXFEntity): TFPoint;
function GetPolylineMiddlePoint(APolyline: TList): TFPoint;
function GetNormailzeVector(AP1, AP2: TFPoint): TFPoint;
function GetGroupOrientation(AEntities: TList): TFPoint;
function GetPolylineOrientation(APolyPolyline: TList): TFPoint;
procedure InitGeometrySearch(var ADistEpsilon, AAngleEpsilon: Double;
  var AMetod: TsgSHXCompareMetod; AStrict: Boolean = True);
function IsEqualGeometry(AG1, AG2: TsgSHXGeometrySign;
  var ADistEpsilon, AAngleEpsilon: Double; var AMetod: TsgSHXCompareMetod;
  AStrict: Boolean = True): Boolean; overload;
function IsEqualGeometry(AG1, AG2: TsgSHXGeometrySign;
  AStrict: Boolean = True): Boolean; overload;
function GetBoxSize(AStartBox,ABox: TFRect): TsgBoxSize;
function GetBoxPosition(AStartBox,ABox: TFRect; AV: TFPoint): TsgPointPosition;
function GetPointPosition(AStartPoint,APoint: TFPoint; AV: TFPoint): TsgPointPosition;
function PositionToLineType(APosition: TsgPointPosition): TsgDrawLineType;
function IsEmptyGeometry(AG: TsgSHXGeometrySign): Boolean;
function MakeEmptyGeometrySign: TsgSHXGeometrySign;
function MakeEmptyGeometry(AAngle: Double = 0): TsgSHXGeometry;
procedure NormailzePolyPolyline(APolyPolyline: TList);

implementation

const
  cnstRootName = 'SHXFontsInfo';
  cnstSymbolNodeName = 'Symbol';
  cnstFontNodeName = 'Font';
  cnstLinesNodeName = 'Lines';
  cnstArcsNodeName = 'Arcs';
  cnstAngleSumNodeName = 'AnglesSum';
  cnstMaxLengthNodeName = 'MaxLength';
  cnstMaxAngleNodeName = 'MaxAngle';
  cnstPositionNodeName = 'Position';
  cnstSizeNodeName = 'Size';
  cnstGeometryHashNodeName = 'GeometryHash';
  cnstAnglesNodeName = 'Angles';
  cnstLengthsNodeName = 'Lengths';
  cnstAngleNodeName = 'Angle';
  cnstObliqueAngleNodeName = 'ObliqueAngle';
  cnstSignNodeName = 'Sign';
  cnstPrevNodeNodeName = 'PrevNode';
  cnstNextNodesNodeName = 'NextNodes';
  cnstSymbolsNodeName = 'Symbols';
  cnstSHXNodesNodeName = 'SHXNodes';
  cnstSHXSymbolsNodeName = 'SHXSymbols';
  cnstSHXStartNodesNodeName = 'SHXStartNodes';
  cnstSHXIndexForLinesNodeName = 'SHXIndexForLines';
  cnstIsFreeNodesNodeName = 'IsFreeNodes';
  cnstNodesNodeName = 'Nodes';
  cnstFormatVersion = '1.0';
  cnstVersionAttrName = 'Version';
  cnstFileFormatKey = 'xtlQGFb0rOeThplP';
  cnstStdObliqueAngles: array [1 .. 3] of Double = (0, 15, 30);
  cnstResolutions: array [False .. True] of Double =
    (cnstNoStrictDistanceResolution, cnstMinDistanceResolution);
  cnstCompareMetods: array [False .. True] of TsgSHXCompareMetod =
    (scmOnlyGeomerty, scmHash);
type
  PInteger = ^Integer;

procedure AddToList(ASrc, ADst: TList; AIsReverse: Boolean = False;
  APosition: Integer = 0);
var
  I: Integer;
begin
  if AIsReverse then
  begin
    for I := ASrc.Count - 1 downto APosition do
      ADst.Add(ASrc[I]);
  end
  else
    for I := APosition to ASrc.Count - 1 do
      ADst.Add(ASrc[I]);
end;

function IsCross(P1, P2, P3, P4: TFPoint; AEpsilon: Double): Boolean;
begin
  Result := IsCrossSegmentsPts(P1, P2, P3, P4, nil, AEpsilon);
  if not Result then
  begin
    Result := IsEqualFPoints(P1, P3, AEpsilon) or
      IsEqualFPoints(P1, P4, AEpsilon);
    if not Result then
      Result := IsEqualFPoints(P2, P3, AEpsilon) or
        IsEqualFPoints(P2, P4, AEpsilon);
  end;
end;

procedure SaveResults(AList: TsgDoubleList; var AResults: array of Double;
  var AMaxValue: Double);
var
  K: Integer;
begin
  AList.Sort;
  if AList.Count > 0 then
    AMaxValue := AList.Last;
  for K := 0 to AList.Count - 1 do
    if K < Length(AResults) then
    begin
      if K mod 2 = 0 then
        AResults[K] := AList[K]
      else
        AResults[K] := AList[MaxI(AList.Count - K, 0)];
    end
    else
      Exit;
end;

procedure AddGeometryHash(var AHash: Integer; AItem: TsgDrawLineType);
begin
  AHash := AHash * (Ord(High(AItem)) + 1) + Ord(AItem);
end;

function GetOrientationPointIndex(ACount: Integer): Integer;
begin
  case ACount of
    2, 3, 4, 5: Result := 1;
  else
    Result := 3;
  end;
end;

function IndexToName(AValue: Integer): string;
const
  cnstNamePrefix = 'Index';
begin
  Result := cnstNamePrefix + IntToStr(AValue);
end;

function NoCript(const AStr, AKey: AnsiString): AnsiString;
begin
  Result := AStr;
end;

function IsLikeValue(AV1, AV2: Double; AEpsilon: Double): Boolean;
var
  vDiv: Double;
begin
  if IsZero(AV1) and IsZero(AV2) then
    Result := True
  else
  begin
    if AV1 < AV2 then
      SwapDoubles(AV1, AV2);
    if IsZero(AV2) then
      Result := IsRange(AV1, 1 / AEpsilon)
    else
    begin
      vDiv := AV1 / AV2;
      Result := IsRange(vDiv, AEpsilon) or IsEqual(vDiv, AEpsilon);
    end;
  end;
end;

function CompareLinesItem(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpInteger(TsgSHXIndexForLinesItem(Value1).Lines,
    TsgSHXIndexForLinesItem(Value2).Lines);
end;

function FindLinesItem(Value1, Value2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpInteger(TsgSHXIndexForLinesItem(Value1).Lines,
    PInteger(Value2)^);
end;

function CompareEntities(const Value1, Value2: Pointer): Integer;
var
  P1, P2: TFPoint;
begin
  P1 := GetEntityMiddlePoint(Value1);
  P2 := GetEntityMiddlePoint(Value2);
  Result := TsgTypeComparer.CmpFPoint(P1, P2);
end;

function ComparePolylines(const Value1, Value2: Pointer): Integer;
var
  P1, P2: TFPoint;
begin
  P1 := GetPolylineMiddlePoint(Value1);
  P2 := GetPolylineMiddlePoint(Value2);
  Result := TsgTypeComparer.CmpFPoint(P1, P2);
end;

{ TsgSHXGeometry }

constructor TsgSHXGeometry.Create(AAngle: Double);
begin
  inherited Create;
  FAngle := AAngle;
  FComponents := TList.Create;
end;

destructor TsgSHXGeometry.Destroy;
begin
  FreeRecordList(FComponents);
  inherited Destroy;
end;

procedure TsgSHXGeometry.AddSign(AGS: TsgSHXGeometrySign);
var
  vSign: PsgSHXGeometrySign;
begin
  New(vSign);
  vSign^ := AGS;
  FComponents.Add(vSign);
end;

procedure TsgSHXGeometry.Clear;
begin
  FAngle := 0;
  ClearRecordList(FComponents);
end;

function TsgSHXGeometry.MaxLength: Double;
var
  I: Integer;
  vSign: PsgSHXGeometrySign;
begin
  Result := 0;
  for I := 0 to FComponents.Count - 1 do
  begin
    vSign := FComponents[I];
    if vSign^.MaxLength > Result then
      Result := vSign^.MaxLength;
  end;
end;

{ TsgSHXIndexNode }

constructor TsgSHXIndexNode.Create(ASign: TsgSHXGeometrySign; AAngle: Double;
  AObliqueAngle: Double; APrevNode: TsgSHXIndexNode = nil);
begin
  inherited Create;
  FAngle := AAngle;
  FObliqueAngle := AObliqueAngle;
  FSign := ASign;
  FPrevNode := APrevNode;
  FSymbols := TList.Create;
  FNextNodes := TList.Create;
end;

destructor TsgSHXIndexNode.Destroy;
begin
  FNextNodes.Free;
  FSymbols.Free;
  inherited Destroy;
end;

{ TsgSHXIndexForLinesItem }

constructor TsgSHXIndexForLinesItem.Create(ALines: Integer;
  AIsFreeNodes: Boolean);
begin
  inherited Create;
  FIsFreeNodes := AIsFreeNodes;
  FLines := ALines;
  FNodes := TList.Create;
end;

destructor TsgSHXIndexForLinesItem.Destroy;
begin
  if FIsFreeNodes then
    FreeList(FNodes)
  else
    FNodes.Free;
  inherited Destroy;
end;

{ TsgSHXIndex }

constructor TsgSHXIndex.Create(AEncript, ADecript: TsgCriptMethod);
begin
  inherited Create;
  if Assigned(AEncript) then
    FEncript := AEncript
  else
    FEncript := cnstDefaultEncriptMethod;
  if Assigned(ADecript) then
    FDecript := ADecript
  else
    FDecript := cnstDefaultDecriptMethod;
  FSymbols := TList.Create;
  FNodes := TList.Create;
  FStartNodes := TList.Create;
  FIndexForLines := TList.Create;
end;

destructor TsgSHXIndex.Destroy;
begin
  FreeRecordList(FSymbols);
  FreeList(FNodes);
  FStartNodes.Free;
  FreeList(FIndexForLines);
  inherited Destroy;
end;

procedure TsgSHXIndex.AddSHXFont(AFont: string);
var
  vImage: TsgCADImage;
  vFontName: string;
  vConverter: TsgDXFConverter;
  vStyle: TsgDXFStyle;
  vText: TsgDXFText;
  vFont: TsgSHXFont;
  I, J: Integer;
  vSymbol: WideChar;
  vSHXRec: PsgSHXRecord;
  vMatrix: TFMatrix;
  vPolyPolyline: TList;
  vSymbolGeom: TsgSHXGeometry;
  vStartBox: TFRect;
begin
  vFontName := ExtractFileName(AFont);
  vMatrix := FMatByScale(90);
  vImage := TsgCADImage.Create;
  try
    vConverter := vImage.Converter;
    vConverter.InitializeSectionsBegin;
    vConverter.InitializeSectionsEnd;
    vImage.CurrentLayout := vConverter.Layouts[0];
    vStyle := TsgDXFStyle.Create;
    try
      vStyle.Name := vFontName;
      vConverter.Sections[csStyles].AddEntity(vStyle);
      vStyle.PrimaryFont := AFont;
      vConverter.Loads(vStyle);
      vText := TsgDXFText.Create;
      try
        vText.Point := MakeFPoint(0, 0);
        vText.Rotation := 0;
        vText.Style := vStyle;
        vText.Height := 98;
        I := vConverter.SHXFonts.IndexFontOfName(vFontName);
        vFont := TsgSHXFont(vConverter.SHXFonts.Fonts.Objects[I]);
        vPolyPolyline := TList.Create;
        try
          for I := 0 to vFont.Shapes.Count - 1 do
          begin
            vSHXRec := PsgSHXRecord(vFont.Shapes[I]);
            vSymbol := WideChar(vSHXRec.ShapeNumber);
            vText.Text := vSymbol;
            for J := Low(cnstStdObliqueAngles) to High(cnstStdObliqueAngles) do
            begin
              vText.ObliqueAngle := cnstStdObliqueAngles[J];
              vConverter.Loads(vText);
              vText.GetSHXLinesExI(vConverter.SHXFonts, vPolyPolyline, @vMatrix, nil, nil);
              try
                vStartBox := cnstBadRect;
                vSymbolGeom := BuildGeometryForPolyPolyline(vPolyPolyline, vStartBox);
                try
                  AddSymbol(vSymbolGeom, vSymbol, vFontName,
                    cnstStdObliqueAngles[J]);
                finally
                  vSymbolGeom.Free;
                end;
              finally
                ClearRecordListOfList(vPolyPolyline);
              end;
              vText.GetSHXLinesExI(vConverter.SHXFonts, vPolyPolyline, nil, nil, nil);
              try
                vStartBox := cnstBadRect;
                vSymbolGeom := BuildGeometryForSHXVertexes(vPolyPolyline, vStartBox);
                try
                  AddSymbol(vSymbolGeom, vSymbol, vFontName,
                    cnstStdObliqueAngles[J]);
                finally
                  vSymbolGeom.Free;
                end;
              finally
                ClearRecordList(vPolyPolyline);
              end;
            end;
          end;
        finally
          vPolyPolyline.Free;
        end;
      finally
        vText.Free;
      end;
    finally
      vConverter.Sections[csStyles].RemoveEntity(vStyle);
      vStyle.Free;
    end;
  finally
    vImage.Free;
  end;
end;

procedure TsgSHXIndex.AddSymbol(AGeometry: TsgSHXGeometry; ASymbol: WideChar;
  AFont: string; AObliqueAngle: Double);
var
  I: Integer;
  vList: TList;
  vNode: TsgSHXIndexNode;
  vSign: PsgSHXGeometrySign;
  vSymbol: PsgSHXCharDescription;
  vAngle: Double;
begin
  if AGeometry.Components.Count = 0 then
    Exit;
  vList := FStartNodes;
  vNode := nil;
  vAngle := AGeometry.Angle;
  for I := 0 to AGeometry.Components.Count - 1 do
  begin
    vSign := AGeometry.Components[I];
    if not FindNode(vList, vSign^, vNode) then
    begin
      vNode := TsgSHXIndexNode.Create(vSign^, vAngle, AObliqueAngle, vNode);
      vAngle := 0;
      FNodes.Add(vNode);
      vList.Add(vNode);
    end;
    vList := vNode.NextNodes;
  end;
  New(vSymbol);
  vSymbol^.Symbol := ASymbol;
  vSymbol^.Font := AFont;
  FSymbols.Add(vSymbol);
  vNode.Symbols.Add(vSymbol);
end;

procedure TsgSHXIndex.CreateForFonts(APathsList: string);
const
  cnstSHXMask = '\*.shx';
var
  vPaths: TsgStringList;
  I: Integer;
  vPath: string;
  vSearchRec: TSearchRec;
begin
  if APathsList = '' then
    APathsList := sSHXSearchPaths;
  vPaths := TsgStringList.Create;
  try
{$IFNDEF SGDEL_6}
    vPaths.LineBreak := cnstCommaPoint;
    vPaths.Text := APathsList;
{$ELSE}
    vPaths.Delimiter := cnstCommaPoint;
    vPaths.DelimitedText := APathsList;
{$ENDIF}
    for I := 0 to vPaths.Count - 1 do
    begin
      vPath := vPaths[I];
      if FindFirst(vPath + cnstSHXMask, faAnyFile, vSearchRec) = 0 then
        repeat
          AddSHXFont(vPath + cnstBackSlash + vSearchRec.Name);
        until FindNext(vSearchRec) <> 0;
      FindClose(vSearchRec);
    end;
  finally
    vPaths.Free;
  end;
end;

procedure TsgSHXIndex.CreateIndexForLines;
var
  I: Integer;
  vIndexForLinesItem, vRootNode: TsgSHXIndexForLinesItem;
  vIndexNode: TsgSHXIndexNode;

  function FindIndexForLinesItem(AList: TList; ALines: Integer;
    var AItem: TsgSHXIndexForLinesItem): Boolean;
  var
    J: Integer;
    vItem: TsgSHXIndexForLinesItem;
  begin
    Result := False;
    for J := 0 to AList.Count - 1 do
    begin
      vItem := AList[J];
      if vItem.Lines = ALines then
      begin
        Result := True;
        AItem := vItem;
      end;
    end;
  end;

begin
  ClearList(FIndexForLines);
  for I := 0 to FStartNodes.Count - 1 do
  begin
    vIndexNode := FStartNodes[I];
    if not FindIndexForLinesItem(FIndexForLines, vIndexNode.Sign.Lines,
      vRootNode) then
    begin
      vRootNode := TsgSHXIndexForLinesItem.Create(vIndexNode.Sign.Lines, True);
      FIndexForLines.Add(vRootNode);
      vIndexForLinesItem := TsgSHXIndexForLinesItem.Create
        (vIndexNode.Sign.Arcs, False);
      vRootNode.Nodes.Add(vIndexForLinesItem);
    end
    else if not FindIndexForLinesItem(vRootNode.Nodes, vIndexNode.Sign.Arcs,
      vIndexForLinesItem) then
    begin
      vIndexForLinesItem := TsgSHXIndexForLinesItem.Create
        (vIndexNode.Sign.Arcs, False);
      vRootNode.Nodes.Add(vIndexForLinesItem);
    end;
    vIndexForLinesItem.Nodes.Add(vIndexNode);
  end;
  QSortList(FIndexForLines, CompareLinesItem);
  for I := 0 to FIndexForLines.Count - 1 do
  begin
    vRootNode := FIndexForLines[I];
    QSortList(vRootNode.Nodes, CompareLinesItem);
  end;
end;

function TsgSHXIndex.FindNode(ANodes: TList; ASign: TsgSHXGeometrySign;
  var ANode: TsgSHXIndexNode; AStrict: Boolean = True): Boolean;
var
  vNode, vStartNode: TsgSHXIndexNode;
  I: Integer;
  vDistResolution, vAngleResolution: Double;
  vMetod: TsgSHXCompareMetod;
begin
  Result := False;
  InitGeometrySearch(vDistResolution, vAngleResolution, vMetod, AStrict);
  vStartNode := ANode;
  for I := 0 to ANodes.Count - 1 do
  begin
    vNode := ANodes[I];
    if IsEqualGeometry(vNode.Sign, ASign, vDistResolution, vAngleResolution,
      vMetod, AStrict) and (vNode.PrevNode = vStartNode) then
    begin
      Result := True;
      ANode := vNode;
    end;
  end;
end;

function TsgSHXIndex.FindSymbol(AGeometry: TsgSHXGeometry;
  var ANode: TsgSHXIndexNode; var ASymbols: TList;
  var AAngle: Double; var AObliqueAngle: Double): Boolean;
var
  vList: TList;
  vSign: PsgSHXGeometrySign;
  I: Integer;
  vFirstNode: TsgSHXIndexNode;
  vIsFirstSearch: Boolean;
  vLinesIndexItem: TsgSHXIndexForLinesItem;
begin
  if AGeometry.Components.Count = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  vList := nil;
  vIsFirstSearch := ANode = nil;
  vFirstNode := nil;
  if vIsFirstSearch then
  begin
    vSign := AGeometry.Components[0];
    if BinSearch(FIndexForLines, FindLinesItem, I, vSign^.Lines) then
    begin
      vLinesIndexItem := FIndexForLines[I];
      if BinSearch(vLinesIndexItem.Nodes, FindLinesItem, I, vSign^.Arcs) then
      begin
        vLinesIndexItem := vLinesIndexItem.Nodes[I];
        if vLinesIndexItem.Nodes.Count > 1 then
          vList := vLinesIndexItem.Nodes
        else
        begin
          ANode := vLinesIndexItem.Nodes[0];
          if not IsEqualGeometry(ANode.Sign, vSign^, False) then
          begin
            Result := False;
            Exit;
          end;
          vFirstNode := ANode;
        end;
      end
      else
        vList := FStartNodes;
    end
    else
      vList := FStartNodes;
  end
  else
    vList := ANode.NextNodes;
  if vList <> nil then
    for I := 0 to AGeometry.Components.Count - 1 do
    begin
      vSign := AGeometry.Components[I];
      if FindNode(vList, vSign^, ANode, False) then
      begin
        vList := ANode.NextNodes;
        if vFirstNode = nil then
          vFirstNode := ANode;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end;
  if Result and vIsFirstSearch then
    AAngle := AGeometry.Angle - vFirstNode.Angle;
  if Result and (ANode.FSymbols.Count > 0) then
  begin
    ASymbols := ANode.FSymbols;
    AObliqueAngle := ANode.ObliqueAngle;
  end;
end;

procedure TsgSHXIndex.OpenIndex(AFileName, APathsList: string);
begin
  if (not FileExists(AFileName)) or (not ReadFromFile(AFileName)) then
  begin
    CreateForFonts(APathsList);
    CreateIndexForLines;
    WriteToFile(AFileName);
  end;
end;

function TsgSHXIndex.ReadFromFile(AFileName: string): Boolean;
var
  vXmlParser: TsgParser;
  vRootNode: TsgNodeSample;
  vStream: TStringStream;

  function GetPointer(AOwnedList: TList; AIndex: Integer): Pointer;
  begin
    Result := nil;
    if (AIndex >= 0) and (AIndex < AOwnedList.Count) then
      Result := AOwnedList[AIndex];
  end;

  procedure ReadList(AOwnedList, AList: TList; ANode: TsgNodeSample);
  var
    vIndexList: TsgIntegerList;
    I: Integer;
  begin
    vIndexList := TsgIntegerList.Create;
    try
      vIndexList.FromStr(ANode.TextAsStr);
      for I := 0 to vIndexList.Count - 1 do
        AList.Add(GetPointer(AOwnedList, vIndexList[I]));
    finally
      vIndexList.Free;
    end;
  end;

  procedure ReadDoubleArray(var AValue: array of Double; ANode: TsgNodeSample);
  var
    vDoubleList: TsgDoubleList;
    I: Integer;
  begin
    vDoubleList := TsgDoubleList.Create;
    try
      vDoubleList.FromStr(ANode.TextAsStr);
      for I := 0 to vDoubleList.Count - 1 do
        AValue[I] := vDoubleList[I];
    finally
      vDoubleList.Free;
    end;
  end;

  procedure ReadSymbol(var ASymbol: TsgSHXCharDescription;
    ANode: TsgNodeSample);
  begin
    ASymbol.Symbol := WideChar(ANode.GetChildByName(cnstSymbolNodeName).TextAsInt);
    ASymbol.Font := ANode.GetChildByName(cnstFontNodeName).TextAsStr;
  end;

  procedure ReadSign(var ASign: TsgSHXGeometrySign; ANode: TsgNodeSample);
  begin
    ASign := MakeEmptyGeometrySign;
    ASign.Lines := ANode.GetChildByName(cnstLinesNodeName).TextAsInt;
    ASign.Arcs := ANode.GetChildByName(cnstArcsNodeName).TextAsInt;
    ASign.AnglesSum := ANode.GetChildByName(cnstAngleSumNodeName).TextAsDouble;
    ASign.MaxLength := ANode.GetChildByName(cnstMaxLengthNodeName).TextAsDouble;
    ASign.MaxAngle := ANode.GetChildByName(cnstMaxAngleNodeName).TextAsDouble;
    ASign.Position := TsgPointPosition(ANode.GetChildByName(cnstPositionNodeName).TextAsInt);
    ASign.Size := TsgBoxSize(ANode.GetChildByName(cnstSizeNodeName).TextAsInt);
    ASign.GeometryHash := ANode.GetChildByName(cnstGeometryHashNodeName).TextAsInt;
    ReadDoubleArray(ASign.Angles, ANode.GetChildByName(cnstAnglesNodeName));
    ReadDoubleArray(ASign.Lengths, ANode.GetChildByName(cnstLengthsNodeName));
  end;

  function CreateSHXIndexNode(ANode: TsgNodeSample): TsgSHXIndexNode;
  var
    vSign: TsgSHXGeometrySign;
    vAngle, vObliqueAngle: Double;
    vPrevNode: TsgSHXIndexNode;
  begin
    ReadSign(vSign, ANode.GetChildByName(cnstSignNodeName));
    vAngle := ANode.GetChildByName(cnstAngleNodeName).TextAsDouble;
    vObliqueAngle := ANode.GetChildByName(cnstObliqueAngleNodeName)
      .TextAsDouble;
    vPrevNode := GetPointer(FNodes, ANode.GetChildByName(cnstPrevNodeNodeName)
      .TextAsInt);
    Result := TsgSHXIndexNode.Create(vSign, vAngle, vObliqueAngle, vPrevNode);
  end;

  procedure ReadSHXIndexNode(ASHXIndexNode: TsgSHXIndexNode;
    ANode: TsgNodeSample);
  begin
    ReadList(FSymbols, ASHXIndexNode.Symbols,
      ANode.GetChildByName(cnstSymbolsNodeName));
    ReadList(FNodes, ASHXIndexNode.NextNodes,
      ANode.GetChildByName(cnstNextNodesNodeName));
  end;

  procedure CreateSymbols(ANode: TsgNodeSample);
  var
    vSymbol: PsgSHXCharDescription;
    I: Integer;
  begin
    ClearRecordList(FSymbols);
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      New(vSymbol);
      ReadSymbol(vSymbol^, ANode.ChildNodes[I]);
      FSymbols.Add(vSymbol);
    end;
  end;

  procedure CreateSHXIndexNodes(ANode: TsgNodeSample);
  var
    vIndexNode: TsgSHXIndexNode;
    I: Integer;
  begin
    ClearList(FNodes);
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vIndexNode := CreateSHXIndexNode(ANode.ChildNodes[I]);
      FNodes.Add(vIndexNode);
    end;
  end;

  procedure ReadSHXIndexNodes(ANode: TsgNodeSample);
  var
    vIndexNode: TsgSHXIndexNode;
    I: Integer;
  begin
    for I := 0 to FNodes.Count - 1 do
    begin
      vIndexNode := FNodes[I];
      ReadSHXIndexNode(vIndexNode, ANode.GetChildByName(IndexToName(I)));
    end;
  end;

  procedure ReadSHXIndexForLines(AList: TList; ANode: TsgNodeSample);
  var
    I: Integer;
    vItem: TsgSHXIndexForLinesItem;

    function CreateSHXIndexForLinesItem(ANode: TsgNodeSample)
      : TsgSHXIndexForLinesItem;
    var
      vIsFreeNodes: Boolean;
      vLines: Integer;
    begin
      vIsFreeNodes := ANode.GetChildByName(cnstIsFreeNodesNodeName).TextAsBool;
      vLines := ANode.GetChildByName(cnstLinesNodeName).TextAsInt;
      Result := TsgSHXIndexForLinesItem.Create(vLines, vIsFreeNodes);
      if vIsFreeNodes then
        ReadSHXIndexForLines(Result.Nodes,
          ANode.GetChildByName(cnstNodesNodeName))
      else
        ReadList(FStartNodes, Result.Nodes,
          ANode.GetChildByName(cnstNodesNodeName));
    end;

  begin
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vItem := CreateSHXIndexForLinesItem(ANode.ChildNodes[I]);
      AList.Add(vItem);
    end;
  end;

begin
  Result := Assigned(FDecript);
  if not Result then
    Exit;
  vStream := TStringStream.Create('');
  try
{$IFDEF SGDEL_2009}
    vStream.LoadFromFile(AFileName);
{$ELSE}
    with TMemoryStream.Create do try vStream.Write(Memory^, Size); finally Free end;
    vStream.Seek(0, 0);
{$ENDIF}
    vXmlParser := TsgParser.Create;
    try
      vXmlParser.LoadFromString(string(FDecript(AnsiString(vStream.DataString),
        cnstFileFormatKey)));
      vRootNode := vXmlParser.ROOT.NodeByName[cnstRootName];
      if vRootNode.GetAttributeByName(cnstVersionAttrName).Value <> cnstFormatVersion
      then
      begin
        Result := False;
        Exit;
      end;
      CreateSymbols(vRootNode.GetChildByName(cnstSHXSymbolsNodeName));
      CreateSHXIndexNodes(vRootNode.GetChildByName(cnstSHXNodesNodeName));
      ReadSHXIndexNodes(vRootNode.GetChildByName(cnstSHXNodesNodeName));
      ReadList(FNodes, FStartNodes,
        vRootNode.GetChildByName(cnstSHXStartNodesNodeName));
      ClearList(FIndexForLines);
      ReadSHXIndexForLines(FIndexForLines,
        vRootNode.GetChildByName(cnstSHXIndexForLinesNodeName));
    finally
      vXmlParser.Free;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TsgSHXIndex.WriteToFile(AFileName: string);
var
  vXmlParser: TsgParser;
  vRootNode: TsgNode;
  vStream: TStringStream;

  function GetIndexesList(AOwnedList, AList: TList): string;
  var
    vIndexList: TsgIntegerList;
    I: Integer;
  begin
    vIndexList := TsgIntegerList.Create;
    try
      for I := 0 to AList.Count - 1 do
        vIndexList.Add(AOwnedList.IndexOf(AList[I]));
      Result := vIndexList.ToStr;
    finally
      vIndexList.Free;
    end;
  end;

  function CreateNode(AName: string): TsgNode;
  begin
    Result := TsgNode.Create;
    Result.Name := AName;
  end;

  function StringToNode(AValue, AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.TextAsStr := AValue;
  end;

  function IntegerToNode(AValue: Integer; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.TextAsInt := AValue;
  end;

  function DoubleToNode(AValue: Double; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.TextAsDouble := AValue;
  end;

  function BoolToNode(AValue: Boolean; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.TextAsBool := AValue;
  end;

  function DoubleArrayToNode(AValue: array of Double; AName: string): TsgNode;
  var
    vDoubleList: TsgDoubleList;
  begin
    vDoubleList := TsgDoubleList.Create;
    try
      Result := CreateNode(AName);
      vDoubleList.AssignArray(AValue);
      Result.TextAsStr := vDoubleList.ToStr;
    finally
      vDoubleList.Free;
    end;
  end;

  function SymbolToNode(ASymbol: TsgSHXCharDescription; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.AddChild(IntegerToNode(Ord(ASymbol.Symbol), cnstSymbolNodeName));
    Result.AddChild(StringToNode(ASymbol.Font, cnstFontNodeName));
  end;

  function GeometrySignToNode(ASign: TsgSHXGeometrySign; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.AddChild(IntegerToNode(ASign.Lines, cnstLinesNodeName));
    Result.AddChild(IntegerToNode(ASign.Arcs, cnstArcsNodeName));
    Result.AddChild(DoubleToNode(ASign.AnglesSum, cnstAngleSumNodeName));
    Result.AddChild(DoubleToNode(ASign.MaxLength, cnstMaxLengthNodeName));
    Result.AddChild(DoubleToNode(ASign.MaxAngle, cnstMaxAngleNodeName));
    Result.AddChild(IntegerToNode(Integer(ASign.Position), cnstPositionNodeName));
    Result.AddChild(IntegerToNode(Integer(ASign.Size), cnstSizeNodeName));
    Result.AddChild(IntegerToNode(ASign.GeometryHash, cnstGeometryHashNodeName));
    Result.AddChild(DoubleArrayToNode(ASign.Angles, cnstAnglesNodeName));
    Result.AddChild(DoubleArrayToNode(ASign.Lengths, cnstLengthsNodeName));
  end;

  function SHXIndexNodeToNode(ASHXNode: TsgSHXIndexNode; AName: string): TsgNode;
  begin
    Result := CreateNode(AName);
    Result.AddChild(DoubleToNode(ASHXNode.Angle, cnstAngleNodeName));
    Result.AddChild(DoubleToNode(ASHXNode.ObliqueAngle,
      cnstObliqueAngleNodeName));
    Result.AddChild(GeometrySignToNode(ASHXNode.Sign, cnstSignNodeName));
    Result.AddChild(IntegerToNode(FNodes.IndexOf(ASHXNode.PrevNode),
      cnstPrevNodeNodeName));
    Result.AddChild(StringToNode(GetIndexesList(FNodes, ASHXNode.NextNodes),
      cnstNextNodesNodeName));
    Result.AddChild(StringToNode(GetIndexesList(FSymbols, ASHXNode.Symbols),
      cnstSymbolsNodeName));
  end;

  function SHXIndexNodesToNode(AName: string): TsgNode;
  var
    I: Integer;
  begin
    Result := CreateNode(AName);
    for I := 0 to FNodes.Count - 1 do
      Result.AddChild(SHXIndexNodeToNode(FNodes[I], IndexToName(I)));
  end;

  function SHXSymbolsToNode(AName: string): TsgNode;
  var
    I: Integer;
  begin
    Result := CreateNode(AName);
    for I := 0 to FSymbols.Count - 1 do
      Result.AddChild(SymbolToNode(PsgSHXCharDescription(FSymbols[I])^,
        IndexToName(I)));
  end;

  function SHXIndexForLinesToNode(AList: TList; AName: string): TsgNode;
  var
    I: Integer;
    vItem: TsgSHXIndexForLinesItem;

    function SHXIndexForLinesItemToNode(AItem: TsgSHXIndexForLinesItem;
      AName: string): TsgNode;
    begin
      Result := CreateNode(AName);
      Result.AddChild(BoolToNode(AItem.IsFreeNodes, cnstIsFreeNodesNodeName));
      Result.AddChild(IntegerToNode(AItem.Lines, cnstLinesNodeName));
      if AItem.IsFreeNodes then
        Result.AddChild(SHXIndexForLinesToNode(AItem.Nodes, cnstNodesNodeName))
      else
        Result.AddChild(StringToNode(GetIndexesList(FStartNodes, AItem.Nodes),
          cnstNodesNodeName));
    end;

  begin
    Result := CreateNode(AName);
    for I := 0 to AList.Count - 1 do
    begin
      vItem := AList[I];
      Result.AddChild(SHXIndexForLinesItemToNode(vItem, IndexToName(I)));
    end;
  end;

begin
  vStream := TStringStream.Create('');
  try
    vXmlParser := TsgParser.Create;
    try
      vRootNode := vXmlParser.CreateRootNode(cnstRootName, True);
      vRootNode.AddAttribNV(cnstVersionAttrName, cnstFormatVersion);
      vRootNode.AddChild(SHXSymbolsToNode(cnstSHXSymbolsNodeName));
      vRootNode.AddChild(SHXIndexNodesToNode(cnstSHXNodesNodeName));
      vRootNode.AddChild(StringToNode(GetIndexesList(FNodes, FStartNodes),
        cnstSHXStartNodesNodeName));
      vRootNode.AddChild(SHXIndexForLinesToNode(FIndexForLines,
        cnstSHXIndexForLinesNodeName));
      if Assigned(FEncript) then
      begin
        vStream.WriteString(string(FEncript(AnsiString(vXmlParser.SaveToString),
          cnstFileFormatKey)));
{$IFDEF SGDEL_2009}
        vStream.SaveToFile(AFileName);
{$ELSE}
        with TFileStream.Create(AFileName, fmCreate) do try Write(vStream.DataString, vStream.Size); finally Free end;
{$ENDIF}
      end;
    finally
      vXmlParser.Free;
    end;
  finally
    vStream.Free;
  end;
end;

{ TsgSHXSymbol }

constructor TsgSHXSymbol.Create(AConverter: TsgDXFConverter; ASymbols: TList;
  AAngle: Double; AObliqueAngle: Double);
begin
  inherited Create;
  FConverter := AConverter;
  FSymbols := ASymbols;
  FAngle := AAngle;
  FObliqueAngle := AObliqueAngle;
  FEntities := TList.Create;
end;

destructor TsgSHXSymbol.Destroy;
var
  I: Integer;
begin
  for I := 0 to FEntities.Count - 1 do
    FConverter.RemoveEntity(TsgDXFEntity(FEntities[I]), False);
  FreeList(FEntities);
  inherited Destroy;
end;

function TsgSHXSymbol.GetFont: string;
begin
  Result := PsgSHXCharDescription(FSymbols.First)^.Font;
end;

function TsgSHXSymbol.GetSymbol: WideChar;
begin
  Result := PsgSHXCharDescription(FSymbols.First)^.Symbol;
end;

function TsgSHXSymbol.Box: TFRect;
begin
  Result := GetEntitiesBox(FEntities);
end;

function TsgSHXSymbol.ColorCAD: TsgColorCAD;
begin
  if FEntities.Count > 0 then
    Result := TsgDXFEntity(FEntities[0]).ColorCAD
  else
    Result := cnstDefaultEntityColor;
end;

function TsgSHXSymbol.IsSimilarFont(ASymbol: TsgSHXSymbol): Boolean;
var
  I,J: Integer;
  vCharDesc: PsgSHXCharDescription;
begin
  Result := False;
  for I := 0 to FSymbols.Count - 1 do
  begin
    vCharDesc := FSymbols[I];
    for J := 0 to ASymbol.FSymbols.Count - 1 do
      if PsgSHXCharDescription(ASymbol.FSymbols[J]).Font = vCharDesc^.Font then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

{ TsgSHXEntitiesGroupItem }

constructor TsgSHXEntitiesGroupItem.Create;
begin
  inherited Create;
  FBox := cnstBadRect;
  FEntities := TList.Create;
end;

destructor TsgSHXEntitiesGroupItem.Destroy;
begin
  FEntities.Free;
  inherited Destroy;
end;

procedure TsgSHXEntitiesGroupItem.Add(AEntity: TsgDXFEntity);
begin
  FEntities.Add(AEntity);
  FBox := GetEntitiesBox(FEntities);
end;

function TsgSHXEntitiesGroupItem.IsEntityInGroup(AEntity: TsgDXFEntity): Boolean;

  function LessEqValue(A, B: Double): Boolean;
  begin
    Result := IsRange(A - B, cnstPointResolution);
    if not Result then
      Result := A < B;
  end;

  function IsIntersect(R1,R2: TFRect): Boolean;
  begin
    Result := LessEqValue(R1.Left, R2.Right) and LessEqValue(R2.Left, R1.Right) and
      LessEqValue(R1.Bottom, R2.Top) and LessEqValue(R2.Bottom, R1.Top)
  end;

begin
  Result := False;
  if IsIntersect(AEntity.Box, FBox) then
    Result := IsEntityInGroupList(FEntities, AEntity);
end;

{ TsgSHXResolver }

constructor TsgSHXResolver.Create(AConverter: TsgDXFConverter;
  AIndexName: string; AEncript, ADecript: TsgCriptMethod);
begin
  inherited Create;
  FConverter := AConverter;
  FEntities := TList.Create;
  FSymbols := TList.Create;
  FResolved := TList.Create;
  FIndex := 0;
  FSHXInfo := TsgSHXIndex.Create(AEncript, ADecript);
  if AIndexName = '' then
    AIndexName := cnstDefaultSHXIndex;
  FSHXInfo.OpenIndex(AIndexName, sSHXSearchPaths);
end;

destructor TsgSHXResolver.Destroy;
begin
  FreeList(FEntities);
  FreeList(FSymbols);
  FResolved.Free;
  FSHXInfo.Free;
  inherited Destroy;
end;

function TsgSHXResolver.FindSymbol(var ASymbols: TList; var AAngle: Double;
  var AObliqueAngle: Double; var AUseLast: Boolean;
  var ANewIndex: Integer): Boolean;
var
  I, J: Integer;
  vNode: TsgSHXIndexNode;
  vSymbols: TList;
  vPrevBox: TFRect;
  V: TFPoint;
  vEntitiesGroup: TsgSHXEntitiesGroupItem;
begin
  Result := False;
  AUseLast := False;
  for I := FIndex to FEntities.Count - 1 do
  begin
    FResolved.Clear;
    vNode := nil;
    J := I;
    vSymbols := nil;
    vPrevBox := cnstBadRect;
    vEntitiesGroup := FEntities[I];
    QSortList(vEntitiesGroup.Entities, CompareEntities);
    V := GetGroupOrientation(vEntitiesGroup.Entities);
    while TestGeometry(vEntitiesGroup, V, vNode, vSymbols, AAngle,
      AObliqueAngle, vPrevBox) do
    begin
      AddToList(TsgSHXEntitiesGroupItem(FEntities[J]).Entities, FResolved);
      Inc(J);
      if J < FEntities.Count then
      begin
        vEntitiesGroup := FEntities[J];
        QSortList(vEntitiesGroup.Entities, CompareEntities);
      end
      else
        Break;
    end;
    if (FResolved.Count > 0) and (vSymbols <> nil) then
    begin
      AUseLast := (I = FIndex);
      ANewIndex := J;
      ASymbols := vSymbols;
      Result := True;
      Exit;
    end;
  end;
  FIndex := FEntities.Count;
end;

function TsgSHXResolver.IsEndOfString(ASymbol: TsgSHXSymbol): Boolean;
const
  cnstMinLettersPerDiagonal = 2.7;
  cnstAngleRange = 35;
var
  vFirstSym,vLastSym: TsgSHXSymbol;
  vDist1,vDist2,vDist3: Double;
  P1,P2: TFPoint;
  vBox,vLastBox: TFRect;
begin
  Result := False;
  if FSymbols.Count > 0 then
  begin
    vFirstSym := FSymbols.First;
    vLastSym := FSymbols.Last;
    vBox := ASymbol.Box;
    vLastBox := vLastSym.Box;
    vDist1 := DistanceFPoint(vLastBox.BottomRight, vBox.TopLeft);
    vDist2 := DistanceFPoint(vBox.TopLeft, vBox.BottomRight);
    vDist3 := DistanceFPoint(vLastBox.TopLeft, vLastBox.BottomRight);
    Result := not(IsLikeValue(vDist1, vDist2, cnstMinLettersPerDiagonal) or
      IsLikeValue(vDist1, vDist3, cnstMinLettersPerDiagonal));
    if not Result then
    begin
      if vFirstSym <> vLastSym then
      begin
        P1 := SubFPoint(GetCenterOfRect(vLastBox), GetCenterOfRect(vFirstSym.Box));
        P2 := SubFPoint(GetCenterOfRect(vBox), GetCenterOfRect(vLastBox));
        vDist1 := GetAngleOfVectors(P1, P2, False);
        Result := not IsRange(vDist1, cnstAngleRange);
      end;
    end;
  end;
end;

function TsgSHXResolver.IsIdenticalStyle(ASymbol: TsgSHXSymbol): Boolean;
var
  vFirstSym: TsgSHXSymbol;
begin
  if FSymbols.Count > 0 then
  begin
    vFirstSym := FSymbols.First;
    Result := vFirstSym.IsSimilarFont(ASymbol) and
      IsEqualColorCAD(vFirstSym.ColorCAD, ASymbol.ColorCAD) and
      (IsEqual(vFirstSym.ObliqueAngle, ASymbol.ObliqueAngle, 1) or
      IsEqual(vFirstSym.ObliqueAngle, ASymbol.ObliqueAngle - ASymbol.Angle, 1));
  end
  else
    Result := True;
end;

function TsgSHXResolver.IsSpace(ASymbol: TsgSHXSymbol): Boolean;
const
  cnstMinLettersPerSpace = 1.6;
var
  vTextBox,vLetterBox,vLastBox: TFRect;
  vSpaceWidth,vLetterWidth,vLastWidth: Double;
  vLastSym: TsgSHXSymbol;

  function IsUpper(const AChar: WideChar): Boolean;
  begin
    Result := AChar = AnsiUpperCase(AChar);
  end;

begin
  Result := False;
  if FSymbols.Count > 0 then
  begin
    vLastSym := FSymbols.Last;
    vLastBox := vLastSym.Box;
    vTextBox := GetSymbolsBox(FSymbols);
    vLetterBox := ASymbol.Box;
    vSpaceWidth := vLetterBox.Left - vTextBox.Right;
    if vSpaceWidth < 0 then
      vSpaceWidth := vTextBox.Left - vLetterBox.Right;
    vLetterWidth := Abs(vLetterBox.Right - vLetterBox.Left);
    vLastWidth := Abs(vLastBox.Right - vLastBox.Left);
    if IsUpper(ASymbol.Symbol) then
      Result := (vSpaceWidth/vLetterWidth >= cnstMinLettersPerSpace) or
        (vSpaceWidth/vLastWidth >= cnstMinLettersPerSpace)
    else
      Result := (vSpaceWidth/vLetterWidth >= cnstMinLettersPerSpace) and
        (vSpaceWidth/vLastWidth >= cnstMinLettersPerSpace);
  end;
end;

function TsgSHXResolver.TestGeometry(const AGroup: TsgSHXEntitiesGroupItem;
  const AV: TFPoint; var ANode: TsgSHXIndexNode; var ASymbols: TList;
  var AAngle: Double; var AObliqueAngle: Double;
  var APrevBox: TFRect): Boolean;
var
  vGeometry: TsgSHXGeometry;
begin
  vGeometry := MakeGeometryForEntities(AGroup.Entities, APrevBox, @AV);
  try
    Result := FSHXInfo.FindSymbol(vGeometry, ANode, ASymbols, AAngle,
      AObliqueAngle);
  finally
    vGeometry.Free;
  end;
end;

function TsgSHXResolver.AddEntity(AEntity: TsgDXFEntity): Boolean;
var
  I: Integer;
  vGroup: TsgSHXEntitiesGroupItem;
begin
  Result := False;
  if (AEntity is TsgCADBasePolyline) or (AEntity is TsgDXFLine) then
  begin
    for I := 0 to FEntities.Count - 1 do
    begin
      vGroup := FEntities[I];
      if vGroup.IsEntityInGroup(AEntity) then
      begin
        vGroup.Add(AEntity);
        Result := True;
        Exit;
      end;
    end;
    vGroup := TsgSHXEntitiesGroupItem.Create;
    vGroup.Add(AEntity);
    FEntities.Add(vGroup);
  end;
end;

procedure TsgSHXResolver.BeginRead;
begin
  FIndex := 0;
end;

procedure TsgSHXResolver.Clear;
begin
  ClearList(FEntities);
  ClearList(FSymbols);
  FIndex := 0;
end;

function TsgSHXResolver.ReadText(var AText: string; var AFont: string;
  var ABox: TFRect; var AColor: TsgColorCAD; var AAngle: Double;
  var AObliqueAngle: Double): Boolean;
var
  vSHXSymbol, vLastSym: TsgSHXSymbol;
  vSymbols: TList;
  vUseLast: Boolean;
  vNewIndex: Integer;
begin
  ClearList(FSymbols);
  AText := '';
  Result := False;
  if FindSymbol(vSymbols, AAngle, AObliqueAngle, vUseLast, vNewIndex) then
  begin
    vUseLast := True;
    repeat
      vSHXSymbol := TsgSHXSymbol.Create(FConverter, vSymbols, AAngle,
        AObliqueAngle);
{$IFNDEF SGDEL_6}
        vSHXSymbol.Entities.Count := FResolved.Count;
        Move(FResolved.List^[0], vSHXSymbol.Entities.List^[0], FResolved.Count*SizeOf(Pointer));
{$ELSE}
      vSHXSymbol.Entities.Assign(FResolved);
{$ENDIF}
      if IsEndOfString(vSHXSymbol) or (not IsIdenticalStyle(vSHXSymbol)) or
        not vUseLast then
      begin
        vSHXSymbol.Entities.Clear;
        vSHXSymbol.Free;
        Break;
      end
      else
      begin
        if IsSpace(vSHXSymbol) then
          AText := AText + cnstSpace;
        FSymbols.Add(vSHXSymbol);
        AText := AText + vSHXSymbol.Symbol;
        FIndex := vNewIndex;
        Result := True;
      end;
    until not FindSymbol(vSymbols, AAngle, AObliqueAngle, vUseLast, vNewIndex);
  end;
  if Result then
  begin
    ABox := GetSymbolsBox(FSymbols);
    vSHXSymbol := FSymbols.First;
    if FSymbols.Count > 1 then
    begin
      vLastSym := FSymbols.Last;
      AAngle := GetAngleOfVector(SubFPoint(vLastSym.Box.BottomRight,
        vSHXSymbol.Box.BottomRight), False);
    end
    else
      AAngle := vSHXSymbol.Angle;
    AFont := GetSymbolsFont(FSymbols);
    AColor := vSHXSymbol.ColorCAD;
    AObliqueAngle := vSHXSymbol.ObliqueAngle;
  end;
end;

function GetEntitiesBox(AEntities: TList): TFRect;
var
  I: Integer;
begin
  if AEntities.Count > 0 then
  begin
    Result := TsgDXFEntity(AEntities[0]).Box;
    for I := 1 to AEntities.Count - 1 do
      UnionFRect(Result, TsgDXFEntity(AEntities[I]).Box);
  end
  else
    Result := cnstBadRect;
end;

function GetSymbolsBox(ASymbols: TList): TFRect;
var
  I: Integer;
begin
  if ASymbols.Count > 0 then
  begin
    Result := TsgSHXSymbol(ASymbols[0]).Box;
    for I := 1 to ASymbols.Count - 1 do
      UnionFRect(Result, TsgSHXSymbol(ASymbols[I]).Box);
  end
  else
    Result := cnstBadRect;
end;

function GetSymbolsFont(ASymbols: TList): string;
var
  I: Integer;
  vResult,vTemp: TStringList;

  procedure MakeResultList(ASymbol: TsgSHXSymbol; ATemp,AResult: TStringList);
  var
    J: Integer;
    vFont: string;
  begin
    AResult.Clear;
    for J := 0 to ASymbol.Symbols.Count - 1 do
    begin
      vFont := PsgSHXCharDescription(ASymbol.Symbols[J]).Font;
      if ATemp = nil then
        AResult.Add(vFont)
      else
        if ATemp.IndexOf(vFont) >= 0 then
          AResult.Add(vFont);
    end;
  end;

begin
  Result := '';
  if ASymbols.Count > 0 then
  begin
    vResult := TStringList.Create;
    try
      vTemp := TStringList.Create;
      try
        MakeResultList(ASymbols.First, nil, vResult);
        for I := 1 to ASymbols.Count - 1 do
        begin
          vTemp.Clear;
          vTemp.AddStrings(vResult);
          MakeResultList(ASymbols[I], vTemp, vResult);
        end;
      finally
        vTemp.Free;
      end;
      if vResult.Count > 0 then
        Result := vResult[0];
    finally
      vResult.Free;
    end;
  end;
end;

function GetPolylineBox(APolyline: TList): TFRect;
var
  I: Integer;
  P: TFPoint;
begin
  if APolyline.Count > 0 then
  begin
    P := PFPoint(APolyline[0])^;
    Result := MakeFRectByPoints(P, P);
    for I := 1 to APolyline.Count - 1 do
      if APolyline[I] <> nil then
        ExpandFRect(Result, PFPoint(APolyline[I])^);
  end
  else
    Result := cnstBadRect;
end;

function GetPolyPolylineEpsilon(APolyPolyline: TList): Double;
const
  cnstEpsilonPercent = 0.97;
var
  I: Integer;
  vBox: TFRect;
begin
  if APolyPolyline.Count > 0 then
  begin
    vBox := GetPolylineBox(APolyPolyline[0]);
    Result := DistanceFPoint(vBox.TopLeft, vBox.BottomRight);
    for I := 1 to APolyPolyline.Count - 1 do
    begin
      vBox := GetPolylineBox(APolyPolyline[I]);
      Result := Min(Result, DistanceFPoint(vBox.TopLeft, vBox.BottomRight));
    end;
    Result := Min(Result * cnstEpsilonPercent, cnstPointResolution);
  end
  else
    Result := cnstPointResolution;
end;

function IsEntityInGroupList(AEntities: TList; AEntity: TsgDXFEntity): Boolean;
var
  I: Integer;

  function IsSegmentInGroup(P1, P2: TFPoint): Boolean;
  var
    J, K: Integer;
    vEntity: TsgDXFEntity;
  begin
    Result := False;
    for J := 0 to AEntities.Count - 1 do
    begin
      vEntity := AEntities[J];
      if vEntity is TsgCADBasePolyline then
      begin
        for K := 0 to TsgCADBasePolyline(vEntity).PointCount - 2 do
          if IsCross(P1, P2, TsgCADBasePolyline(vEntity).Points[K],
            TsgCADBasePolyline(vEntity).Points[K + 1],
            cnstPointResolution) then
          begin
            Result := True;
            Exit;
          end;
      end
      else if vEntity is TsgDXFLine then
        if IsCross(P1, P2, TsgDXFLine(vEntity).Point,
          TsgDXFLine(vEntity).Point1, cnstPointResolution) then
        begin
          Result := True;
          Exit;
        end;
    end;
  end;

begin
  Result := False;
  if AEntity is TsgCADBasePolyline then
  begin
    for I := 0 to TsgCADBasePolyline(AEntity).PointCount - 2 do
      if IsSegmentInGroup(TsgCADBasePolyline(AEntity).Points[I],
        TsgCADBasePolyline(AEntity).Points[I + 1]) then
      begin
        Result := True;
        Exit;
      end;
  end
  else if AEntity is TsgDXFLine then
    Result := IsSegmentInGroup(TsgDXFLine(AEntity).Point,
      TsgDXFLine(AEntity).Point1);
end;

function IsPolylineInGroup(AGroup, APolyline: TList;
  var AIsReverseL,AIsReverseG: Boolean; AEpsilon: Double): Boolean;
var
  I: Integer;
  vIsBadStartGroup,vIsBadEndGroup: Boolean;

  function IsSegmentInGroup(const P1, P2: TFPoint;
    var AIsBadStart,AIsBadEnd: Boolean): Boolean;
  var
    J: Integer;
    vP1,vP2: TFPoint;
  begin
    Result := False;
    AIsBadStart := False;
    AIsBadEnd := False;
    vP1 := PFPoint(AGroup[0])^;
    J := 1;
    while J < AGroup.Count do
    begin
      if AGroup[J] <> nil then
      begin
        vP2 := PFPoint(AGroup[J])^;
        if IsCross(P1, P2, vP1, vP2, AEpsilon) then
        begin
          if J = 0 then
            AIsBadStart := IsEqualFPoints(P1, vP1, cnstPointResolution);
          if J = AGroup.Count - 1 then
            AIsBadEnd := IsEqualFPoints(P2, vP2, cnstPointResolution);
          Result := True;
        end;
        vP1 := vP2;
        Inc(J);
      end
      else
      begin
        Inc(J);
        vP1 := PFPoint(AGroup[J])^;
        Inc(J);
      end;
    end;
  end;

begin
  Result := False;
  AIsReverseL := False;
  AIsReverseG := False;
  for I := 0 to APolyline.Count - 2 do
    if IsSegmentInGroup(PFPoint(APolyline[I])^, PFPoint(APolyline[I + 1])^,
      vIsBadStartGroup, vIsBadEndGroup) then
    begin
      Result := True;
      if I = 0 then
      begin
        AIsReverseG := vIsBadStartGroup;
        Exit;
      end;
      if I = APolyline.Count - 2 then
        AIsReverseL := vIsBadEndGroup;
    end;
end;

function MakeGeometryForEntities(AList: TList; var APrevBox: TFRect;
  const AV: PFPoint = nil): TsgSHXGeometry;
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vPolyPolyline, vCurList: TList;
begin
  vPolyPolyline := TList.Create;
  try
    for I := 0 to AList.Count - 1 do
    begin
      vEntity := TsgDXFEntity(AList[I]);
      vCurList := TList.Create;
      vPolyPolyline.Add(vCurList);
      EntityToPointList(vEntity, vCurList);
    end;
    Result := BuildGeometryForPolyPolyline(vPolyPolyline, APrevBox, AV);
  finally
    ClearRecordListOfList(vPolyPolyline);
    vPolyPolyline.Free;
  end;
end;

procedure EntityToPointList(AEntity: TsgDXFEntity; AList: TList);
const
  cnstBoxResolution = 1000;
var
  J: Integer;
  P1, P2: TFPoint;
  vEpsilon: Double;
  vBox: TFRect;

  procedure AddPoint(ACurList: TList; APoint: TFPoint);
  var
    vPoint: PFPoint;
  begin
    New(vPoint);
    vPoint^ := APoint;
    ACurList.Add(vPoint);
  end;

begin
  if AEntity is TsgCADBasePolyline then
  begin
    vBox := AEntity.Box;
    vEpsilon := DistanceFPoint(vBox.TopLeft, vBox.BottomRight) / cnstBoxResolution;
    P1 := TsgCADBasePolyline(AEntity).Points[0];
    AddPoint(AList, P1);
    for J := 1 to TsgCADBasePolyline(AEntity).PointCount - 1 do
    begin
      P2 := TsgCADBasePolyline(AEntity).Points[J];
      if not IsEqualFPoints(P1, P2, vEpsilon) then
      begin
        AddPoint(AList, P2);
        P1 := P2;
      end;
    end;
    if TsgCADBasePolyline(AEntity).Closed then
    begin
      P1 := TsgCADBasePolyline(AEntity).Points[0];
      if not IsEqualFPoints(P1, P2, vEpsilon) then
        AddPoint(AList, P1);
    end;
  end
  else
    if AEntity is TsgDXFLine then
    begin
      AddPoint(AList, TsgDXFLine(AEntity).Point);
      AddPoint(AList, TsgDXFLine(AEntity).Point1);
    end;
end;

function BuildGeometryForPolyPolyline(APolyPolyline: TList;
  var APrevBox: TFRect; const AV: PFPoint = nil;
  ALikeEpsilon: Double = cnstArcItemLenPerDiagonal): TsgSHXGeometry;
var
  I: Integer;
  vPoly: TList;
  V: TFPoint;
  vGeometry: TsgSHXGeometrySign;
begin
  Result := MakeEmptyGeometry;
  if APolyPolyline.Count = 0 then
    Exit;
  if AV = nil then
  begin
    QSortList(APolyPolyline, ComparePolylines);
    V := GetPolylineOrientation(APolyPolyline);
  end
  else
    V := AV^;
  NormailzePolyPolyline(APolyPolyline);
  Result.FAngle := GetAngleOfVector(V, False);
  for I := 0 to APolyPolyline.Count - 1 do
  begin
    vPoly := APolyPolyline[I];
    vGeometry := BuildGeometryForPolyline(vPoly, V, APrevBox, ALikeEpsilon);
    if not IsEmptyGeometry(vGeometry) then
      Result.AddSign(vGeometry);
  end;
end;

function BuildGeometryForPolyline(APolyline: TList; AV: TFPoint; var APrevBox: TFRect;
  ALikeEpsilon: Double = cnstArcItemLenPerDiagonal): TsgSHXGeometrySign;
var
  I, J: Integer;
  P1, P2, P3, vStartArc, vEndArc: TFPoint;
  vAngle, vDist, vDP: Double;
  vAngles, vLengths: TsgDoubleList;
  vIsArc: Boolean;
  vPolyBox: TFRect;
begin
  Result := MakeEmptyGeometrySign;
  if APolyline.Count = 0 then
    Exit;
  vPolyBox := GetPolylineBox(APolyline);
  Result.Position := GetBoxPosition(APrevBox, vPolyBox, AV);
  Result.Size := GetBoxSize(APrevBox, vPolyBox);
  APrevBox := vPolyBox;
  vDP := DistanceFPoint(vPolyBox.TopLeft, vPolyBox.BottomRight);
  vAngles := TsgDoubleList.Create;
  try
    vLengths := TsgDoubleList.Create;
    try
      if APolyline.Count >= 1 then
      begin
        P1 := PFPoint(APolyline[0])^;
        I := 1;
        while I < APolyline.Count do
        begin
          if APolyline[I] = nil then  // skip delimeter
          begin
            if I + 2 >= APolyline.Count then
              Break;
            Inc(I);
            P1 := PFPoint(APolyline[I])^;
            Inc(I);
            AddGeometryHash(Result.GeometryHash, dltDelimeter);
          end;
          P2 := PFPoint(APolyline[I])^;
          vDist := DistanceFPoint(P1, P2);
          J := I + 1;
          vIsArc := False;
          if not IsLikeValue(vDP, vDist, ALikeEpsilon) then
          begin
            vStartArc := P1;
            vEndArc := P2;
            while (J < APolyline.Count) and (APolyline[J] <> nil) do
            begin
              P3 := PFPoint(APolyline[J])^;
              if IsLikeValue(DistanceFPoint(vEndArc, P3), vDP, ALikeEpsilon) then
                Break
              else
              begin
                vEndArc := P3;
                vIsArc := True;
                Inc(J);
              end;
            end;
          end;
          if vIsArc then
          begin
            vAngle := GetAngleOfVectors(GetNormailzeVector(vStartArc, vEndArc),
              AV, False);
            vDist := DistanceFPoint(vStartArc, vEndArc);
            Inc(Result.Arcs);
            P2 := vEndArc;
            I := J;
          end
          else
          begin
            vDist := DistanceFPoint(P1, P2);
            Inc(Result.Lines);
            vAngle := GetAngleOfVectors(GetNormailzeVector(P1, P2), AV, False);
            Inc(I);
          end;
          Result.AnglesSum := Result.AnglesSum + vAngle;
          vLengths.Add(vDist);
          vAngles.Add(vAngle);
          AddGeometryHash(Result.GeometryHash,
            PositionToLineType(GetPointPosition(P1, P2, AV)));
          P1 := P2;
        end;
      end;
      SaveResults(vAngles, Result.Angles, Result.MaxAngle);
      SaveResults(vLengths, Result.Lengths, Result.MaxLength);
    finally
      vLengths.Free;
    end;
  finally
    vAngles.Free;
  end;
end;

function BuildGeometryForSHXVertexes(AVertexes: TList;
  var APrevBox: TFRect): TsgSHXGeometry;
var
  vPolyPolyline, vList: TList;
  vVertex, vVertexPrev: PsgSHXVertex;
  vFirstV, P1, P2, P3: TFPoint;
  X0, Y0, A, A1, BT: Double;
  I, J, vNumberOfCircleParts: Integer;
  SinA, CosA: Extended;
  vIsSave: Boolean;

  procedure AddPoint(AP: TFPoint);
  var
    vPoint: PFPoint;
  begin
    New(vPoint);
    vPoint^ := AP;
    vList.Add(vPoint);
  end;

  procedure AddToPolyPolyline;
  begin
    if vList.Count > 0 then
    begin
      vPolyPolyline.Add(vList);
      vList := TList.Create;
    end;
    vIsSave := False;
  end;

begin
  if AVertexes.Count = 0 then
  begin
    Result := MakeEmptyGeometry;
    Exit;
  end;
  vFirstV := cnstFPointZero;
  vPolyPolyline := TList.Create;
  try
    vIsSave := False;
    vVertexPrev := AVertexes[0];
    vList := TList.Create;
    for I := 1 to AVertexes.Count - 1 do
    begin
      vVertex := AVertexes[I];
      if vVertex^.PenDown then
      begin
        P1 := MakeFPoint(vVertexPrev^.X, vVertexPrev^.Y);
        P2 := MakeFPoint(vVertex^.X, vVertex^.Y);
        if IsZero(vVertex^.Bulge) then
        begin
          if not vIsSave then
            AddPoint(P1);
          AddPoint(P2);
          vIsSave := True;
        end
        else
        begin
          AddToPolyPolyline;
          vNumberOfCircleParts := cnstArcItemLenPerDiagonal + 1;
          A := ArcTan(vVertex^.Bulge) * 2;
          A1 := A / (vNumberOfCircleParts) * 2;
          SinCos(A1, SinA, CosA);
          BT := (1 / vVertex^.Bulge - vVertex^.Bulge)/2;
          X0 := ((P2.X + P1.X) - BT * (P2.Y - P1.Y)) / 2;
          Y0 := ((P2.Y + P1.Y) + BT * (P2.X - P1.X)) / 2;
          P3 := P1;
          AddPoint(P1);
          for J := 0 to cnstArcItemLenPerDiagonal do
          begin
            P3 := MakeFPoint(X0 + (P3.X - X0) * CosA - (P3.Y - Y0) * SinA,
              Y0 + (P3.X - X0) * SinA + (P3.Y - Y0) * CosA);
            AddPoint(P3);
          end;
          AddToPolyPolyline;
        end;
      end
      else
        if vIsSave then
          AddToPolyPolyline;
      vVertexPrev := vVertex;
    end;
    if vIsSave then
      vPolyPolyline.Add(vList);
    QSortList(vPolyPolyline, ComparePolylines);
    if vPolyPolyline.Count > 0 then
    begin
      vList := vPolyPolyline[0];
      if vList.Count >= cnstArcItemLenPerDiagonal then
      begin
        P1 := PFPoint(vList.First)^;
        P2 := PFPoint(vList.Last)^;
        vFirstV := GetNormailzeVector(P1, P2);
      end
      else
        vFirstV := GetPolylineOrientation(vPolyPolyline);
    end;
    Result := BuildGeometryForPolyPolyline(vPolyPolyline, APrevBox, @vFirstV);
  finally
    vPolyPolyline.Free;
  end;
end;

{
function BuildGeometryForSHXVertexes(AVertexes: TList;
  var APrevBox: TFRect): TsgSHXGeometry;
var
  I: Integer;
  vVertex, vVertexPrev: PsgSHXVertex;
  vDist, vAngle: Double;
  vSign: TsgSHXGeometrySign;
  vAngles, vLengths: TsgDoubleList;
  vIsSave,vIsAddDelimeter: Boolean;
  vFirstV, P1, P2: TFPoint;
  vItemBox: TFRect;

  procedure AddGeometryItem(AGeometry: TsgSHXGeometry;
    AAngles, ALengths: TsgDoubleList; AItem: TsgSHXGeometrySign);
  begin
    AItem.Position := GetBoxPosition(APrevBox, vItemBox, vFirstV);
    AItem.Size := GetBoxSize(APrevBox, vItemBox);
    APrevBox := vItemBox;
    SaveResults(AAngles, AItem.Angles, AItem.MaxAngle);
    SaveResults(ALengths, AItem.Lengths, AItem.MaxLength);
    AGeometry.AddSign(AItem);
    AAngles.Clear;
    ALengths.Clear;
  end;

begin
  Result := MakeEmptyGeometry;
  if AVertexes.Count = 0 then
    Exit;
  vVertexPrev := AVertexes[0];
  vFirstV := cnstFPointZero;
  vItemBox := cnstBadRect;
  for I := 1 to AVertexes.Count - 1 do
  begin
    vVertex := AVertexes[I];
    if vVertex^.PenDown then
    begin
      P1 := MakeFPoint(vVertexPrev^.X, vVertexPrev^.Y);
      P2 := MakeFPoint(vVertex^.X, vVertex^.Y);
      vFirstV := GetNormailzeVector(P1, P2);
      vItemBox := MakeFRectByPoints(P1, P2);
      Result.FAngle := GetAngleOfVector(vFirstV, False);
      Break;
    end;
    vVertexPrev := vVertex;
  end;
  vVertexPrev := AVertexes[0];
  vSign := MakeEmptyGeometrySign;
  vIsSave := False;
  vAngles := TsgDoubleList.Create;
  try
    vLengths := TsgDoubleList.Create;
    try
      for I := 1 to AVertexes.Count - 1 do
      begin
        vVertex := AVertexes[I];
        if vVertex^.PenDown then
        begin
          vIsAddDelimeter := False;
          if vIsSave and (not IsZero(vVertex^.Bulge) or
            not IsZero(vVertexPrev^.Bulge)) then
          begin
            AddGeometryHash(vSign.GeometryHash, dltDelimeter);
            vIsAddDelimeter := True;
          end;
          vIsSave := True;
          P1 := MakeFPoint(vVertexPrev^.X, vVertexPrev^.Y);
          P2 := MakeFPoint(vVertex^.X, vVertex^.Y);
          AddGeometryHash(vSign.GeometryHash,
            PositionToLineType(GetPointPosition(P1, P2, vFirstV)));
          vDist := DistanceFPoint(P1, P2);
          vAngle := GetAngleOfVectors(GetNormailzeVector(P1, P2), vFirstV, False);
          vSign.AnglesSum := vSign.AnglesSum + vAngle;
          vLengths.Add(vDist);
          vAngles.Add(vAngle);
          ExpandFRect(vItemBox, P1);
          ExpandFRect(vItemBox, P2);
          if IsZero(vVertex^.Bulge) then
            Inc(vSign.Lines)
          else
          begin
            Inc(vSign.Arcs);
          end;
        end
        else
          if vIsSave then
          begin
            if (not vIsAddDelimeter) and (I < AVertexes.Count - 1) then
              AddGeometryHash(vSign.GeometryHash, dltDelimeter);
            AddGeometryItem(Result, vAngles, vLengths, vSign);
            vSign := MakeEmptyGeometrySign;
            vIsSave := False;
          end;
        vVertexPrev := vVertex;
      end;
      if vIsSave then
        AddGeometryItem(Result, vAngles, vLengths, vSign);
    finally
      vLengths.Free;
    end;
  finally
    vAngles.Free;
  end;
end;
}

function GetEntityMiddlePoint(AEntity: TsgDXFEntity): TFPoint;
var
  vList: TList;
begin
  vList := TList.Create;
  try
    EntityToPointList(AEntity, vList);
    Result := GetPolylineMiddlePoint(vList);
  finally
    vList.Free;
  end;
end;

function GetPolylineMiddlePoint(APolyline: TList): TFPoint;
var
  I, vCount: Integer;
begin
  Result := cnstFPointZero;
  vCount := APolyline.Count;
  for I := 0 to vCount - 1 do
    Result := AddFPoint(Result, PFPoint(APolyline[I])^);
  Result := MakeFPoint(Result.X/vCount, Result.Y/vCount);
end;

function GetNormailzeVector(AP1, AP2: TFPoint): TFPoint;
begin
  if TsgTypeComparer.CmpFPoint(AP1, AP2) < 0 then
    Result := SubFPoint(AP1, AP2)
  else
    Result := SubFPoint(AP2, AP1);
end;

function GetGroupOrientation(AEntities: TList): TFPoint;
var
  P1, P2: TFPoint;
  vEntity: TsgDXFEntity;
  I: Integer;
begin
  P1 := cnstFPointZero;
  P2 := cnstFPointZero;
  if AEntities.Count > 0 then
  begin
    vEntity := AEntities[0];
    if vEntity is TsgDXFLine then
    begin
      P1 := TsgDXFLine(vEntity).Point;
      P2 := TsgDXFLine(vEntity).Point1;
    end
    else
      if vEntity is TsgDXFSpline then
      begin
        P1 := TsgDXFSpline(vEntity).PolyPoints.First;
        P2 := TsgDXFSpline(vEntity).PolyPoints.Last;
      end
      else
        if vEntity is TsgCADBasePolyline then
          if TsgCADBasePolyline(vEntity).PointCount > 1 then
          begin
            I := GetOrientationPointIndex(TsgCADBasePolyline(vEntity).PointCount);
            P1 := TsgCADBasePolyline(vEntity).Points[0];
            P2 := TsgCADBasePolyline(vEntity).Points[I];
          end;
  end;
  Result := GetNormailzeVector(P1, P2);
end;

function GetPolylineOrientation(APolyPolyline: TList): TFPoint;
var
  P1, P2: TFPoint;
  vList: TList;
  I: Integer;
begin
  P1 := cnstFPointZero;
  P2 := cnstFPointZero;
  if (APolyPolyline.Count > 0) and (TObject(APolyPolyline[0]) is TList) then
    vList := APolyPolyline[0]
  else
    vList := APolyPolyline;
  if vList.Count >= 2 then
  begin
    I := GetOrientationPointIndex(vList.Count);
    P1 := PFPoint(vList[0])^;
    P2 := PFPoint(vList[I])^;
  end;
  Result := GetNormailzeVector(P1, P2);
end;

procedure InitGeometrySearch(var ADistEpsilon, AAngleEpsilon: Double;
  var AMetod: TsgSHXCompareMetod; AStrict: Boolean = True);
begin
  ADistEpsilon := cnstResolutions[AStrict];
  AAngleEpsilon := cnstResolutions[False];
  AMetod := cnstCompareMetods[True];
end;

function IsEqualGeometry(AG1, AG2: TsgSHXGeometrySign;
  var ADistEpsilon, AAngleEpsilon: Double; var AMetod: TsgSHXCompareMetod;
  AStrict: Boolean = True): Boolean;
var
  vAngleDist, vLenDist, vMinAngle: Double;
  vMetod: TsgSHXCompareMetod;

  function IsLikeArray(AValues1, AValues2: array of Double;
    AMax1, AMax2: Double; var ADist: Double; AEpsilon: Double): Boolean;
  var
    I: Integer;
    V1, V2: Double;
  begin
    Result := True;
    ADist := 0;
    try
      for I := 0 to MinI(Length(AValues1), Length(AValues2)) - 1 do
      begin
        V1 := AValues1[I] / AMax1;
        V2 := AValues2[I] / AMax2;
        if V1 < V2 then
          SwapDoubles(V1, V2);
        if IsLikeValue(V1, V2, AEpsilon) then
        begin
          if not IsZero(V2) then
            ADist := Max(ADist, V1 / V2);
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      if IsZero(ADist) then
        ADist := AEpsilon;
    end;
  end;

begin
  vAngleDist := AAngleEpsilon;
  vMetod := scmOnlyGeomerty;
  Result := (AG1.Lines = AG2.Lines) and (AG1.Arcs = AG2.Arcs) and
    (AG1.Position = AG2.Position) and (AG1.Size = AG2.Size);
  if Result and (AG1.GeometryHash = AG2.GeometryHash) then
    vMetod := scmHash;
  if Result then
    Result := vMetod >= AMetod;
  if Result then
  begin
    if AStrict then
      Result := IsLikeArray(AG1.Angles, AG2.Angles, 1, 1,
        vLenDist, AAngleEpsilon)
    else
    begin
      Result := IsLikeValue(AG1.AnglesSum, AG2.AnglesSum, AAngleEpsilon);
      vMinAngle := Min(AG1.AnglesSum, AG2.AnglesSum);
      if IsZero(vMinAngle) then
        vAngleDist := AAngleEpsilon
      else
        vAngleDist := Max(AG1.AnglesSum, AG2.AnglesSum) / vMinAngle;
    end;
    if Result then
      Result := IsLikeArray(AG1.Lengths, AG2.Lengths, AG1.MaxLength,
        AG2.MaxLength, vLenDist, ADistEpsilon);
    if Result then
    begin
      AMetod := vMetod;
      ADistEpsilon := Min(vLenDist, ADistEpsilon);
      AAngleEpsilon := Min(vAngleDist, AAngleEpsilon);
    end;
  end;
end;

{
function IsEqualGeometry(AG1, AG2: TsgSHXGeometrySign;
  var ADistEpsilon, AAngleEpsilon, ADistLen, ADistAngle: Double;
  AStrict: Boolean = True): Boolean;
var
  vAngleDist, vLenDist, vAngleDistSqr, vLenDistSqr, vMinAngle: Double;

  function CalcDistance(AValues1, AValues2: array of Double;
    AMax1, AMax2: Double): Double;
  var
    I: Integer;
    vDist: Double;
  begin
    Result := 0;
    for I := 0 to MinI(Length(AValues1), Length(AValues2)) - 1 do
    begin
      vDist := AValues1[I] / AMax1 - AValues2[I] / AMax2;
      Result := Result + vDist * vDist;
    end;
  end;

  function IsLikeArray(AValues1, AValues2: array of Double;
    AMax1, AMax2: Double; var ADist: Double; AEpsilon: Double): Boolean;
  var
    I: Integer;
    V1, V2: Double;
  begin
    Result := True;
    ADist := 0;
    try
      for I := 0 to MinI(Length(AValues1), Length(AValues2)) - 1 do
      begin
        V1 := AValues1[I] / AMax1;
        V2 := AValues2[I] / AMax2;
        if V1 < V2 then
          SwapDoubles(V1, V2);
        if IsLikeValue(V1, V2, AEpsilon) then
        begin
          if not IsZero(V2) then
            ADist := Max(ADist, V1 / V2);
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      if IsZero(ADist) then
        ADist := AEpsilon;
    end;
  end;

begin
  vAngleDistSqr := ADistAngle;
  vLenDistSqr := ADistLen;
  Result := (AG1.Lines = AG2.Lines) and (AG1.Arcs = AG2.Arcs) and
    (AG1.Position = AG2.Position) and (AG1.Size = AG2.Size) and
    (AG1.GeometryHash = AG2.GeometryHash);
  if Result then
  begin
    Result := IsLikeValue(AG1.AnglesSum, AG2.AnglesSum, AAngleEpsilon);
    vMinAngle := Min(AG1.AnglesSum, AG2.AnglesSum);
    if IsZero(vMinAngle) then
      vAngleDist := AAngleEpsilon
    else
      vAngleDist := Max(AG1.AnglesSum, AG2.AnglesSum) / vMinAngle;
    if Result then
      if AStrict then
      begin
        vLenDist := CalcDistance(AG1.Lengths, AG2.Lengths, AG1.MaxLength,
          AG2.MaxLength);
        Result := IsRange(vLenDist, ADistEpsilon);
      end
      else
        Result := IsLikeArray(AG1.Lengths, AG2.Lengths, AG1.MaxLength,
          AG2.MaxLength, vLenDist, ADistEpsilon);
    if Result then
    begin
      if AStrict then
        vLenDistSqr := vLenDist
      else
        vLenDistSqr := CalcDistance(AG1.Lengths, AG2.Lengths, AG1.MaxLength,
          AG2.MaxLength);
      Result := IsRange(vLenDistSqr, ADistLen) or
        (vLenDistSqr/ADistLen < cnstNoStrictDistanceResolution);
      if Result then
      begin
        vAngleDistSqr := CalcDistance(AG1.Angles, AG2.Angles, AG1.MaxAngle,
          AG2.MaxAngle);
        Result := IsRange(vAngleDistSqr, ADistAngle) or
          (vAngleDistSqr/ADistAngle < cnstNoStrictDistanceResolution);
      end;
    end;
    if Result then
    begin
      ADistEpsilon := Min(vLenDist, ADistEpsilon);
      AAngleEpsilon := Min(vAngleDist, AAngleEpsilon);
      ADistLen := Min(vLenDistSqr, ADistLen);
      ADistAngle := Min(vAngleDistSqr, ADistAngle);
    end;
  end;
end;
}

function IsEqualGeometry(AG1, AG2: TsgSHXGeometrySign;
  AStrict: Boolean = True): Boolean; overload;
var
  vDistEps, vAngleEps: Double;
  vMetod: TsgSHXCompareMetod;
begin
  InitGeometrySearch(vDistEps, vAngleEps, vMetod, AStrict);
  Result := IsEqualGeometry(AG1, AG2, vDistEps, vAngleEps, vMetod, AStrict);
end;

function GetBoxSize(AStartBox,ABox: TFRect): TsgBoxSize;
var
  vDiag1,vDiag2: Double;
begin
  Result := bsUndefined;
  if IsEqualFRects(AStartBox, cnstBadRect) then
    Exit;
  vDiag1 := DistanceFPoint(AStartBox.TopLeft, AStartBox.BottomRight);
  vDiag2 := DistanceFPoint(ABox.TopLeft, ABox.BottomRight);
  if IsEqual(vDiag1, vDiag2, cnstPointResolution) then
    Result := bsEqual
  else
    if vDiag1 > vDiag2 then
      Result := bsLittle
    else
      Result := bsBig;
end;

function GetBoxPosition(AStartBox,ABox: TFRect; AV: TFPoint): TsgPointPosition;
var
  P1,P2: TFPoint;
begin
  Result := ppUndefined;
  if IsEqualFRects(AStartBox, cnstBadRect) then
    Exit;
  P1 := GetCenterOfRect(AStartBox);
  P2 := GetCenterOfRect(ABox);
  Result := GetPointPosition(P1, P2, AV);
end;

function GetPointPosition(AStartPoint,APoint: TFPoint; AV: TFPoint): TsgPointPosition;
var
  vAngle: Double;
begin
  Result := ppUndefined;
  if IsEqualFPoints(APoint, AStartPoint) then
    Exit;
  vAngle := GetAngleOfVectors(SubFPoint(APoint, AStartPoint), AV, False);
  if (vAngle >= 330) or (vAngle <= 30) then
    Result := ppRight
  else
    if (vAngle > 30) and (vAngle <= 60) then
      Result := ppRightAbove
    else
      if (vAngle > 60) and (vAngle <= 120) then
        Result := ppAbove
      else
        if (vAngle > 120) and (vAngle <= 150) then
          Result := ppLeftAbove
        else
          if (vAngle > 150) and (vAngle <= 210) then
            Result := ppLeft
          else
            if (vAngle > 210) and (vAngle <= 240) then
              Result := ppLeftBelow
            else
              if (vAngle > 240) and (vAngle <= 300) then
                Result := ppBelow
              else
                Result := ppRightBelow;
end;

function PositionToLineType(APosition: TsgPointPosition): TsgDrawLineType;
begin
  Result := dltUndefined;
  case APosition of
    ppLeft,ppRight:
      Result := dltHorizontal;
    ppAbove,ppBelow:
      Result := dltVertical;
    ppLeftAbove,ppLeftBelow,ppRightAbove,ppRightBelow:
      Result := dltDiagonal;
  end;
end;

function IsEmptyGeometry(AG: TsgSHXGeometrySign): Boolean;
begin
  Result := (AG.Lines = 0) and (AG.Arcs = 0);
end;

function MakeEmptyGeometrySign: TsgSHXGeometrySign;

  procedure FillZero(var AValue: array of Double);
  var
    I: Integer;
  begin
    for I := 0 to Length(AValue) - 1 do
      AValue[I] := 0;
  end;

begin
  Result.Lines := 0;
  Result.Arcs := 0;
  Result.AnglesSum := 0;
  Result.MaxLength := 0;
  Result.MaxAngle := 0;
  Result.Position := ppUndefined;
  Result.Size := bsUndefined;
  Result.GeometryHash := 0;
  FillZero(Result.Angles);
  FillZero(Result.Lengths);
end;

function MakeEmptyGeometry(AAngle: Double = 0): TsgSHXGeometry;
begin
  Result := TsgSHXGeometry.Create(AAngle);
end;

procedure NormailzePolyPolyline(APolyPolyline: TList);
var
  vNewPolyPoy, vList, vPoly: TList;
  J, K: Integer;
  vIsFound, vIsRevG, vIsRevL: Boolean;
  vEpsilon: Double;

  procedure CreateNewGroup(APoly: TList);
  var
    vNewList: TList;
  begin
    vNewList := TList.Create;
    AddToList(APoly, vNewList);
    vNewPolyPoy.Add(vNewList);
    APolyPolyline.Remove(APoly);
    APoly.Free;
  end;

begin
  vNewPolyPoy := TList.Create;
  try
    vEpsilon := GetPolyPolylineEpsilon(APolyPolyline);
    CreateNewGroup(APolyPolyline[0]);
    while APolyPolyline.Count > 0 do
    begin
      vIsFound := False;
      for K := 0 to APolyPolyline.Count - 1 do
      begin
        vPoly := APolyPolyline[K];
        for J := 0 to vNewPolyPoy.Count - 1 do
        begin
          vList := vNewPolyPoy[J];
          if IsPolylineInGroup(vList, vPoly, vIsRevL, vIsRevG, vEpsilon) then
          begin
            if vIsRevG then
            begin
              vPoly.Add(nil);
              AddToList(vList, vPoly, True);
              vPoly.Clear;
              AddToList(vPoly, vList);
            end
            else
            begin
              vList.Add(nil);
              AddToList(vPoly, vList, vIsRevL);
            end;
            vIsFound := True;
            APolyPolyline.Remove(vPoly);
            vPoly.Free;
            Break;
          end;
        end;
        if vIsFound then
          Break;
      end;
      if not vIsFound then
        CreateNewGroup(APolyPolyline[0]);
    end;
    ClearList(APolyPolyline);
{$IFNDEF SGDEL_6}
    CopyLists(APolyPolyline, vNewPolyPoy);
{$ELSE}
    APolyPolyline.Assign(vNewPolyPoy);
{$ENDIF}
  finally
    vNewPolyPoy.Free;
  end;
end;

initialization
  cnstDefaultEncriptMethod := NoCript;
  cnstDefaultDecriptMethod := NoCript;

end.

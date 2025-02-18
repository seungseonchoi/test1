{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                Drawing complex linetypes                   }
{                                                            }
{     Copyright (c) 2012 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit sgLines;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, Types, {$IFDEF SG_NON_WIN_PLATFORM}cwstring,{$ENDIF}
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics,
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, Math, SysUtils,
{$IFDEF SGDEL_XE2}
{$IFNDEF SGFPC}
  System.Types,
{$ENDIF}
{$ENDIF}
  sgConsts, sgLists;

type

  PsgLTypeElement = ^TsgLTypeElement;
  TsgLTypeElement = packed record
    // 0 = "Dash" only
    // 1 = If set, Rotation below specifies an absolute rotation (if not set, Rotation specifies a relative rotation- "including on load")
    // 2 = Embedded element is a text string
    // 4 = Embedded element is a shape
    ComplexType: Byte;
    Dash: Double;
    Scale: Double;
    Rotation: Double;
    XOffset: Double;
    YOffset: Double;
    Text: string;
    ShapeNumber: Word;
    Style: TObject;
  end;

  PsgLTypeElementArray = ^TsgLTypeElementArray;
  TsgLTypeElementArray = array[0 .. MaxInt div SizeOf(TsgLTypeElement) - 1] of TsgLTypeElement;

  TsgLines = class
  private
    FElementsCount: Integer;
    FElements: array of TsgLTypeElement;
    FUniform: Integer;
    FIsAutocadMetod: Boolean;
    FMaxSizeOfPatterns: Double;
    FPatternLength: Double;
    FScale: Double;
    FSimpleElementsCount: Integer;
    FSimpleElements: array of Pointer;
    function IndexOfSimpleElements(P: Pointer): Integer;
    function AddSimpleElement(P: Pointer): Integer;
    function AddPt(ADottedPoints: TFPointList; const APoint, ADelta: TFPoint;
      const ACurrentByLength: Double): PFPoint;
    function AddPtEx(ADottedPoints: TFPointList; const APoint, ADelta, AOffset: TFPoint;
      const ACurrentByLength, AScale, ASin, ACos: Double): PFPoint;
    function CheckPatterns(const ALen: Double;
      const AAmountPatterns: Integer): Boolean;
    procedure ClearSimpleElements;
    procedure DoTextPoints(ADottedPoints: TFPointList; APoints: PFPointArray;
      const ACounts: array of Integer; const P1, ADelta: TFPoint;
      const ACurrentByLength: Double; const AAbsoluteRotation: Boolean);
    function GetAdditionalPart(const ALength, APatternsAmount: Double): Double;
    function GetElement(const AIndex: Integer): TsgLTypeElement;
    function GetElementsCount: Integer;
    procedure SetScale(const AValue: Double);
    procedure CalcParamByCurve(const AClose: Boolean; var ALen, AScale: Double;
      var AAmountPatterns: Integer; var APartLineLength: Double);
    procedure SetUniform(const AValue: Integer);
    function GetStyles(Index: Integer): TObject;
    procedure SetStyles(Index: Integer; const Value: TObject);
    function GetList: PsgLTypeElementArray;{$IFDEF  SG_INLINE} inline;{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AElement: TsgLTypeElement);
    procedure AddTick(const AValue: Double);
    procedure Append(const AElements: array of TsgLTypeElement);
    procedure Assign(ASource: TsgLines);
    procedure Clear;
    procedure Curve(APoints, ADottedPoints: TFPointList; const AClose: Boolean);
    procedure Initialize(const AParts: array of Double);
    function IsDotted: Boolean;
    function IsSolid: Boolean;
    procedure Line(const APoint1, APoint2: TFPoint; ADottedPoints: TFPointList);
    procedure Loaded(AConverter: Pointer);
    procedure Link(AConverter: TObject);
    function MaxSizeOfPatterns: Double; //evg
    procedure Poly(APoints, ADottedPoints: TFPointList; const AClose: Boolean);
    procedure Vertexes(const APoints: TFPointList; ACounts: TsgIntegerList;
      ADottedPoints: TFPointList; const Close: Boolean); //evg
    property Elements[const AIndex: Integer]: TsgLTypeElement read GetElement;
    property ElementsCount: Integer read GetElementsCount;
    property Uniform: Integer read FUniform write SetUniform;
    property IsAutocadMetod: Boolean read FIsAutocadMetod write FIsAutocadMetod;
    property List: PsgLTypeElementArray read GetList;
    property PatternLength: Double read FPatternLength;
    property Scale: Double read FScale write SetScale;
    property Styles[Index: Integer]: TObject read GetStyles write SetStyles;
  end;

  function SGFloor(AValue: Double): Integer;

implementation

{$IFNDEF SG_CADIMPORTERDLLDEMO}
uses
  DXFConv, TTF, sgFunction;

type
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFTextAccess = class(TsgDXFText);
  TsgDXFStyleAccess = class(TsgDXFStyle);
  TsgBaseListAccess = class(TsgBaseList);
{$ELSE}
type
{$ENDIF}

  PsgSimpleLTypeElement = ^TsgSimpleLTypeElement;
  TsgSimpleLTypeElement = packed record
    // 0 = "Dash" only
    // 1 = SHX/Shape
    // 2 = TTF
    // 4 = Has absolute rotation
    ComplexType: Byte;
    Points: PFPointArray;
    Count: Integer;
    Counts: PsgIntegerArray;
    Value: Double;// rotation or dash/dot/space
  end;

function GetAngleByPoint(const APoint: TFPoint): Double;
var
  vKatetX: Double;
  vCmp: TPoint;
begin
  vCmp := Point(Integer(APoint.X < 0), Integer(APoint.Y < 0));
  vKatetX := Abs(APoint.X);
  if vKatetX < fAccuracy then
    Result := Pi*(0.5 + vCmp.Y)
  else
  begin
    Result := ArcTan(Abs(APoint.Y) / vKatetX);
    case vCmp.X shl 1 + vCmp.Y of
      1:  Result := 2*Pi - Result;
      2:  Result := Pi - Result;
      3:  Result := Pi + Result;
    end;
  end
end;

{$IFNDEF SG_CADIMPORTERDLLDEMO}
// GetTextPath
//  Creates list of lists of PFPoints text/shape's boundary.
//
//  Returns type of list:
//    0 - list not created;
//    1 - SHX, Shape;
//    2 - TTF.
function GetTextPath(AConverter: TsgDXFConverter; AText: TsgDXFText;
  ACollection: TsgCustomPolyItemsCollection): Byte;
var
  vTextGlyph: TsgTextGlyph;
  vIndex: Integer;
  vText: TsgDXFTextAccess absolute AText;
begin
  Result := 0;
  if (AConverter <> nil) and AConverter.UseSHXFonts and (not AText.WinFont) then
  begin
    ACollection.UpdateTransformation(cnstIdentityMat, {$IFDEF HAS_UNMANAGED_TYPEINFO}TypeInfo(TFMatrix){$ELSE}SizeOf(TFMatrix){$ENDIF});
    AText.GetSHXLinesEx(AConverter.SHXFonts, ACollection);
    ACollection.Normalize;
    if ACollection.Counts.Count > 0 then
    begin
      Result := 1;   // SHX
      Exit;
    end;
  end;
  vTextGlyph := TsgTextGlyph(TsgDXFStyleAccess(vText.Properties).FFontGlyphRef);
  if not (vTextGlyph is TsgTextGlyph) then
  begin
    vIndex := ContainerOfTextGlyphs.IndexOf(vText.Properties.FontName, vText.Properties.FontStyle, vText.Charset);
    if vIndex >= 0 then
      vTextGlyph := ContainerOfTextGlyphs.TextGlyph[vIndex]
    else
      vTextGlyph := nil;
  end;
  if Assigned(vTextGlyph) then
  begin
    ACollection.UpdateTransformation(cnstIdentityMat, {$IFDEF HAS_UNMANAGED_TYPEINFO}TypeInfo(TFMatrix){$ELSE}SizeOf(TFMatrix){$ENDIF});
    vTextGlyph.GetPolyPolyline(AText.Text, AText.UnicodeText, AText.GetMatrix, ACollection, AText.Tracking);
    ACollection.Normalize;
    Result := 2;// TTF
  end;
end;
{$ENDIF}
{$IFDEF SG_CADIMPORTERDLLDEMO}
function IsEqual(const A, B: Extended; AResolution: Extended = fDoubleResolution): Boolean;
var
  Epsilon: Extended;
begin
  Epsilon := Max(Min(Abs(A), Abs(B)) * AResolution, AResolution);
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
//  Result := Abs(A - B) <= AResolution
end;
{$ENDIF}
function CompareValues(const AValue1, AValue2: Double): Integer;
begin
  Result := 0;
  if (AValue1 > AValue2) then
    Result := 1;
  if IsEqual(AValue1, AValue2, fExtendedResolution) then
    Inc(Result, 2);
end;

function SGFloor(AValue: Double): Integer;
begin
  Result := Round(AValue);
  if CompareValues(Result, AValue) = 1 then
    Result := Result - 1;
end;

function CreateSimpleElement: PsgSimpleLTypeElement;
begin
  New(Result);
  FillChar(Result^, SizeOf(TsgSimpleLTypeElement), 0);
end;


{$IFDEF SG_CADIMPORTERDLLDEMO}
function AddFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := AP1.X + AP2.X;
  Result.Y := AP1.Y + AP2.Y;
  Result.Z := AP1.Z + AP2.Z;
end;

function PtXScalar(const AP: TFPoint; const AVal: TsgFloat): TFPoint;
begin
  Result.X := AP.X * AVal;
  Result.Y := AP.Y * AVal;
  Result.Z := AP.Z * AVal;
end;
{$ENDIF}

{ TsgLines }

{private}

// TsgLines.AddPt
// ACurrentByLength: [0..1], 0 - for starting point, 1 - for ending point.
function TsgLines.AddPt(ADottedPoints: TFPointList;
  const APoint, ADelta: TFPoint; const ACurrentByLength: Double): PFPoint;
begin
  ADottedPoints.Add(AddFPoint(APoint, PtXScalar(ADelta, ACurrentByLength)));
  Result := @ADottedPoints.List^[ADottedPoints.Count -1];
end;

// TsgLines.AddPtEx
// See TsgLines.AddPt.
// Rotates and offsets a point and adds it to list.
function TsgLines.AddPtEx(ADottedPoints: TFPointList; const APoint, ADelta, AOffset: TFPoint;
  const ACurrentByLength, AScale, ASin, ACos: Double): PFPoint;
begin
  Result := AddPt(ADottedPoints, APoint, ADelta, ACurrentByLength);
  Result^.X := Result^.X + (ACos * AOffset.X - ASin * AOffset.Y)*AScale;
  Result^.Y := Result^.Y + (ASin * AOffset.X + ACos * AOffset.Y)*AScale;
  Result^.Z := Result^.Z + (AOffset.Z                          )*AScale;
end;

function TsgLines.AddSimpleElement(P: Pointer): Integer;
begin
  if FSimpleElementsCount = Length(FSimpleElements) then
    SetLength(FSimpleElements, ListGrow(Length(FSimpleElements)));
  Result := FSimpleElementsCount;
  FSimpleElements[Result] := P;
  Inc(FSimpleElementsCount);
end;

procedure TsgLines.CalcParamByCurve(const AClose: Boolean; var ALen, AScale: Double;
  var AAmountPatterns: Integer; var APartLineLength: Double);
var
  vSElem: PsgSimpleLTypeElement;
  vDoubleAmountPatterns: Double;
begin
  if FPatternLength <> 0 then
  begin
    if IsAutocadMetod then
    begin
      if not AClose then
      begin
        AAmountPatterns := Trunc(ALen/(FPatternLength*FScale));
        vSElem := PsgSimpleLTypeElement(FSimpleElements[0]);
        AScale := FScale;
        ALen := ALen/AScale;
        APartLineLength := 0.5*(Abs(vSElem^.Value) * Ord(vSElem^.ComplexType = 0)+
          ALen - AAmountPatterns*FPatternLength);
      end
      else
      begin
        vDoubleAmountPatterns := ALen / (FScale*FPatternLength);
        if vDoubleAmountPatterns < 1 then
        begin
          AScale := FScale;
          APartLineLength := 0;
          AAmountPatterns := 0;
          ALen := 0;
        end
        else
        begin
          AAmountPatterns := Round(vDoubleAmountPatterns);
          if AAmountPatterns <= 1 then
            AAmountPatterns := 2;
          AScale := ((ALen/AAmountPatterns) / FPatternLength);
          ALen := AAmountPatterns * FPatternLength;
          APartLineLength := GetAdditionalPart(ALen, AAmountPatterns);
        end;
      end;
    end
    else
    begin
      // Calculates amount of patterns in the "polyline"
      AAmountPatterns := SGFloor(ALen / (FScale*FPatternLength));
      if AClose and (AAmountPatterns <> 0) then
      begin
        //vScale := FScale * ( 1 + (vLen - vAmountPatterns * FPatternLength)/vAmountPatterns/FPatternLength);
        // vScale - not recalculate
        AScale := ALen/(AAmountPatterns * FPatternLength);
        ALen := AAmountPatterns * FPatternLength;
        //vScale := FScale;
        //vLen := vLen/vScale;
      end
      else
      begin
        AScale := FScale;
        ALen := ALen/AScale;
      end;
      // Recalculates amount of patterns in the "polyline" (accuracy problem)
      //vAmountPatterns := SGFloor(vLen / FPatternLength);
      // Length of a additional part of a line
      APartLineLength := GetAdditionalPart(ALen, AAmountPatterns);
    end;
  end
  else
  begin
    AScale := FScale;
    APartLineLength := 0;
    AAmountPatterns := 0;
    ALen := 0;
  end;
end;

function TsgLines.CheckPatterns(const ALen: Double;
  const AAmountPatterns: Integer): Boolean;
begin
  Result := (ALen = 0) or (AAmountPatterns = 0) or (FPatternLength = 0)
    or (AAmountPatterns > iMaxNumDottedLines) {too many patterns}
    or (FSimpleElementsCount = 0);
end;

procedure TsgLines.ClearSimpleElements;
var
  I: Integer;
  vPSElem: PsgSimpleLTypeElement;
begin
  //if FSimpleElements = nil then Exit;
  for I := FSimpleElementsCount - 1 downto 0 do
  begin
    vPSElem := PsgSimpleLTypeElement(FSimpleElements[I]);
    FreeMemAndNil(Pointer(vPSElem^.Points));
    FreeMemAndNil(Pointer(vPSElem^.Counts));
    Dispose(vPSElem);
    FSimpleElements[I] := nil;
  end;
  FSimpleElementsCount := 0;
end;

procedure TsgLines.DoTextPoints(ADottedPoints: TFPointList; APoints: PFPointArray;
  const ACounts: array of Integer; const P1, ADelta: TFPoint;
  const ACurrentByLength: Double; const AAbsoluteRotation: Boolean);
var
  vSin, vCos: Extended;
  I, J, K, vMaxNumDottedLines: Integer;
  vPLastPoint: TFPoint;
begin
  vMaxNumDottedLines := iMaxNumDottedLines * 8;
  if not AAbsoluteRotation then
    SinCos(GetAngleByPoint(ADelta), vSin, vCos)
  else
  begin
    vSin := 0;
    vCos := 1;
  end;
  K := 0;
  I := Low(ACounts);
  while (I <= High(ACounts)) and (ADottedPoints.Count <= vMaxNumDottedLines) do
  begin
    J := 1;
    while (J < ACounts[I]) and (ADottedPoints.Count <= vMaxNumDottedLines) do
    begin
      if J = 1 then
        vPLastPoint := AddPtEx(ADottedPoints, P1, ADelta, APoints^[K + J - 1],
          ACurrentByLength, FScale, vSin, vCos)^
      else
        ADottedPoints.Add(vPLastPoint);
      vPLastPoint := AddPtEx(ADottedPoints, P1, ADelta, APoints^[K + J],
        ACurrentByLength, FScale, vSin, vCos)^;
      Inc(J);
    end;
    Inc(K, ACounts[I]);
    Inc(I);
  end;
end;

function TsgLines.GetAdditionalPart(const ALength, APatternsAmount: Double): Double;
var
  vSElem: PsgSimpleLTypeElement;
begin
  vSElem := PsgSimpleLTypeElement(FSimpleElements[0]);
  Result := Abs(vSElem^.Value) * Ord(vSElem^.ComplexType = 0);
  if FUniform = 0 then
    Result := 0.5 * (Result + (ALength - APatternsAmount * FPatternLength));
end;

function TsgLines.GetElement(const AIndex: Integer): TsgLTypeElement;
begin
  if (AIndex >= 0) and (AIndex < FElementsCount) then
    Result := FElements[AIndex]
  else
    FillChar(Result, SizeOf(Result), 0)
end;

function TsgLines.GetElementsCount: Integer;
begin
  Result := FElementsCount;
end;

function TsgLines.GetList: PsgLTypeElementArray;
begin
  Result := PsgLTypeElementArray(FElements);
end;

function TsgLines.GetStyles(Index: Integer): TObject;
begin
  Result := Elements[Index].Style;
end;

procedure TsgLines.SetScale(const AValue: Double);
begin
  FScale := Abs(AValue);
  if FScale < fAccuracy then
    FScale := 1.0;
end;

procedure TsgLines.SetStyles(Index: Integer; const Value: TObject);
begin
  FElements[Index].Style := Value;
end;

procedure TsgLines.SetUniform(const AValue: Integer);
begin
  FUniform := AValue;
end;

{public}

constructor TsgLines.Create;
begin
  FScale := 1;
end;

destructor TsgLines.Destroy;
begin
  Clear;
  ClearSimpleElements;
  inherited Destroy;
end;

procedure TsgLines.Add(const AElement: TsgLTypeElement);
begin
  if FElementsCount = Length(FElements) then
    SetLength(FElements, ListGrow(Length(FElements)));
  FElements[FElementsCount] := AElement;
  Inc(FElementsCount);
end;

procedure TsgLines.AddTick(const AValue: Double);
var
  vElement: TsgLTypeElement;
begin
  FillChar(vElement, SizeOf(vElement), 0);
  vElement.Dash := AValue;
  Add(vElement);
end;

procedure TsgLines.Append(const AElements: array of TsgLTypeElement);
var
  I: Integer;
begin
  for I := Low(AElements) to High(AElements) do
    Add(AElements[I]);
end;

procedure TsgLines.Assign(ASource: TsgLines);
var
  I, J, K: Integer;
  vElemSrc, vElemDst: PsgSimpleLTypeElement;
begin
  FUniform := ASource.FUniform;
  FIsAutocadMetod := ASource.FIsAutocadMetod;
  FPatternLength := ASource.FPatternLength;
  FScale := ASource.FScale;
  FMaxSizeOfPatterns := ASource.FMaxSizeOfPatterns;
  ClearSimpleElements;
  for I := 0 to ASource.FSimpleElementsCount - 1 do
  begin
    vElemSrc := PsgSimpleLTypeElement(ASource.FSimpleElements[I]);
    New(vElemDst);
    vElemDst^.ComplexType := vElemSrc^.ComplexType;
    vElemDst^.Counts := nil;
    vElemDst^.Points := nil;
    vElemDst^.Count := vElemSrc^.Count;
    if vElemDst^.Count > 0 then
    begin
      GetMem(vElemDst^.Counts, vElemDst^.Count * SizeOf(Integer));
      K := 0;
      for J := 0 to vElemSrc^.Count - 1 do
      begin
        vElemDst^.Counts^[J] := vElemSrc^.Counts^[J];
        Inc(K, vElemDst^.Counts^[J]);
      end;
      if K > 0 then
      begin
        GetMem(vElemDst^.Points, K * SizeOf(TFPoint));
        System.Move(vElemSrc^.Points^, vElemDst^.Points^, K * SizeOf(TFPoint));
      end;
    end;
    vElemDst^.Value := vElemSrc^.Value;
    AddSimpleElement(vElemDst);
  end;
  FElementsCount := 0;
  Append(ASource.FElements);
end;

procedure TsgLines.Clear;
begin
  //if FElements = nil then Exit;
  //SetLength(FElements, 0);
  FElementsCount := 0;
end;

procedure TsgLines.Curve(APoints, ADottedPoints: TFPointList; const AClose: Boolean);
var
  I, vIndex, vPointsCount, vCount, vAmountPatterns, vCounterPatterns: Integer;
  vCur, vTotalCur, vLen, vLineLength, vPartLineLength, vParam, vScale: Double;
  vBeginning, vIsTooManyDottedLines: Boolean;
  vSElem: PsgSimpleLTypeElement;
  vLengths: TsgDoubleList;
  vPointsCopy: TFPointList;
  vDelta, vPoint1, vPoint2: TFPoint;
  vCheckPoints: Boolean;
begin
  if (APoints.Count = 0) or (ADottedPoints = nil) then
    Exit;
  vCheckPoints := (Uniform = 0) or IsAutocadMetod;
  vPointsCopy := TFPointList.Create;
  try
    vPointsCopy.Assign(APoints);

    if AClose then//  Addition of the first point of a curve
    begin
      if not IsEqualFPoints(vPointsCopy.First,vPointsCopy.Last) then
        vPointsCopy.Add(vPointsCopy.First);
    end;
    vPointsCount := vPointsCopy.Count-1;

    vIsTooManyDottedLines := False;
    vLen := 0.0;
    vIndex := 0;
    // Creates the vLengths-list of segments of this "polyline"
    vLengths := TsgDoubleList.Create(vPointsCount);
    try
      vPoint1 := vPointsCopy[vPointsCount];
      for I := vPointsCount-1 downto 0 do
      begin
        vPoint2 := vPointsCopy[I];
        vLineLength := DistanceFPoint(vPoint2, vPoint1);
        if vLineLength = 0 then
        begin
          vPointsCopy.Delete(I);
          Continue;
        end;
        vLengths[vIndex] := vLineLength;
        vLen := vLen + vLengths[vIndex];
        Inc(vIndex);
        vPoint1 := vPoint2;
        vIsTooManyDottedLines := vIsTooManyDottedLines or (vLineLength > iMaxNumDottedLines * FScale * FPatternLength);
      end;

      if vLen = 0 then
      begin
        ADottedPoints.AppendArray([vPointsCopy[0],vPointsCopy[0]]);
        Exit;
      end
      else
        vPointsCount := vPointsCopy.Count-1;

      CalcParamByCurve(AClose, vLen, vScale, vAmountPatterns, vPartLineLength);

      if CheckPatterns(vLen, vAmountPatterns) or vIsTooManyDottedLines
        or (vAmountPatterns > iMaxNumDottedLines shl 10) then
      begin
        if IsDotted and (vAmountPatterns = 0) and (vPointsCount > 0) then
        begin
          ADottedPoints.AppendArray([vPointsCopy[0],vPointsCopy[0],
            vPointsCopy[vPointsCount],vPointsCopy[vPointsCount]]);
        end
        else
          for I := 0 to vPointsCount - 1 do
          begin
            ADottedPoints.AppendArray([vPointsCopy[I], vPointsCopy[I+1]]);
          end;
        Exit;
      end;

      I := 0;
      vIndex := 0;
      vBeginning := vCheckPoints;
      vCount := FSimpleElementsCount - 1;
      vPoint1 := vPointsCopy[I];
      vPoint2 := vPointsCopy[I+1];
      vLineLength := (vLengths.Items[vPointsCount - 1 - I])/vScale;
      vDelta.X := vPoint2.X - vPoint1.X;
      vDelta.Y := vPoint2.Y - vPoint1.Y;
      vDelta.Z := vPoint2.Z - vPoint1.Z;
      vCur := 0;
      vTotalCur := 0;
      vCounterPatterns := 0;

      while True do
      begin
        vSElem := PsgSimpleLTypeElement(FSimpleElements[vIndex]);
        case vSElem^.ComplexType of
          0:   // Dash/dot/space
            if vSElem^.Value = 0 then // dot
            begin
              if (IsAutocadMetod and (vCounterPatterns = vAmountPatterns)) or
                 ((FUniform = 0) and (vCounterPatterns = vAmountPatterns)) then
              begin
                vPoint1 := vPointsCopy[vPointsCount];// Last curve point
                vCur := 0;//vCur + vPartLineLength;//vLineLength;
                vTotalCur := vLen;
              end;
              AddPt(ADottedPoints, vPoint1, vDelta, vCur/vLineLength);
              AddPt(ADottedPoints, vPoint1, vDelta, vCur/vLineLength);
              if ( vTotalCur >= vLen)then
                Exit;
            end
            else                     // dash or space
            begin
              if vSElem^.Value > 0 then
              begin
                vParam := vSElem^.Value*Ord(not vBeginning);
                AddPt(ADottedPoints, vPoint1, vDelta, vCur/vLineLength);
              end
              else
                vParam := Abs(vSElem^.Value);
              vCur := vCur + vParam + vPartLineLength*Ord(vBeginning);
              // Skip full segments
              while (vCur >= vLineLength ) or ((FUniform = 0) and
                (vCounterPatterns = vAmountPatterns)) do
              begin
                if vSElem^.Value > 0 then
                  AddPt(ADottedPoints, vPoint1, vDelta, 1);
                vTotalCur := vTotalCur + vLineLength;
                if ( vTotalCur >= vLen) or (I = vPointsCount-1) then Exit;
                if vSElem^.Value > 0 then
                  AddPt(ADottedPoints, vPoint1, vDelta, 1);
                // Proceed to next point
                vCur := vCur - vLineLength;
                Inc(I);
                vPoint1 := vPointsCopy[I];
                vPoint2 := vPointsCopy[I+1];
                vLineLength := (vLengths.Items[vPointsCount - 1 - I])/vScale;
                vDelta.X := vPoint2.X - vPoint1.X;
                vDelta.Y := vPoint2.Y - vPoint1.Y;
                vDelta.Z := vPoint2.Z - vPoint1.Z;
              end;
              if vSElem^.Value > 0 then
                AddPt(ADottedPoints, vPoint1, vDelta, vCur/vLineLength);
              if vCheckPoints and (vCounterPatterns < vAmountPatterns) then
                vBeginning := False;
            end;
          1, 5:// SHX, Shape
            begin

              if (vCounterPatterns = vAmountPatterns) then Exit;

              DoTextPoints(ADottedPoints, vSElem^.Points, Slice(vSElem^.Counts^, vSElem^.Count), vPoint1, vDelta,
                vCur/vLineLength, vSElem^.ComplexType and 4 <> 0);
            end;
          2, 6:// TTF
            begin

              if (vCounterPatterns = vAmountPatterns) then Exit;

              DoTextPoints(ADottedPoints, vSElem^.Points, Slice(vSElem^.Counts^, vSElem^.Count), vPoint1, vDelta,
                vCur/vLineLength, vSElem^.ComplexType and 4 <> 0);
            end;
        end;
        Inc(vIndex);
        if vIndex > vCount then
        begin
          vIndex := 0;
          Inc(vCounterPatterns);// another one pattern has been added
          if vCheckPoints and (vCounterPatterns = vAmountPatterns) then
            vBeginning := True;
        end;
      end;
    finally
      vLengths.Free;
    end;
  finally
    vPointsCopy.Free;
  end;
end;

function TsgLines.IndexOfSimpleElements(P: Pointer): Integer;
var
  vFind: Boolean;
begin
  vFind := False;
  Result := Low(FSimpleElements);
  while (Result < FSimpleElementsCount) and not vFind do
    if FSimpleElements[Result] = P then
      Inc(vFind)
    else
      Inc(Result);
  if not vFind then Result := -1;
end;

procedure TsgLines.Initialize(const AParts: array of Double);
var
  I: Integer;
  vElem: TsgLTypeElement;
begin
  Clear;
  FillChar(vElem, SizeOf(vElem), 0);
  for I := Low(AParts) to High(AParts) do
  begin
    vElem.Dash := AParts[I];
    Add(vElem);
  end;
end;

function TsgLines.IsDotted: Boolean;
begin
  Result := (FSimpleElementsCount > 1) and
    (PsgSimpleLTypeElement(FSimpleElements[0])^.Value = 0) and
    (PsgSimpleLTypeElement(FSimpleElements[1])^.Value < 0);
end;

function TsgLines.IsSolid: Boolean;
begin
  Result := FElementsCount <= 1;// previous version
end;

procedure TsgLines.Line(const APoint1, APoint2: TFPoint;
  ADottedPoints: TFPointList);
var
  vAmountPatterns, vCounterPatterns, vCount, vIndex: Integer;
  vCur, vLen, vPartLineLength: Double;
  vSElem: PsgSimpleLTypeElement;
  vDeltaPoint: TFPoint;
begin
  vCount := FSimpleElementsCount-1;
  vDeltaPoint.X := APoint2.X - APoint1.X;
  vDeltaPoint.Y := APoint2.Y - APoint1.Y;
  vDeltaPoint.Z := APoint2.Z - APoint1.Z;
  if FPatternLength <> 0 then
  begin
    vLen := Sqrt(vDeltaPoint.X*vDeltaPoint.X + vDeltaPoint.Y*vDeltaPoint.Y +
      vDeltaPoint.Z*vDeltaPoint.Z) / FScale;
    if vLen < iMaxNumDottedLines * FPatternLength then
      vAmountPatterns := SGFloor(vLen / FPatternLength)
    else
    begin
      vLen := 0;
      vAmountPatterns := iMaxNumDottedLines + 1;
    end;
  end
  else
  begin
    vLen := 0;
    vAmountPatterns := 0;
  end;
  if CheckPatterns(vLen, vAmountPatterns) then
  begin
    AddPt(ADottedPoints, APoint1, vDeltaPoint, 0);// starting point
    if IsDotted and (vAmountPatterns = 0) then
    begin
      AddPt(ADottedPoints, APoint1, vDeltaPoint, 0);
      AddPt(ADottedPoints, APoint1, vDeltaPoint, 1);
    end;
    AddPt(ADottedPoints, APoint1, vDeltaPoint, 1);// ending point
    Exit;
  end;

  vCur := 0;
  vIndex := 0;
  vCounterPatterns := 0;

  // Length of a additional part of a line
  vPartLineLength := GetAdditionalPart(vLen, vAmountPatterns);

  while True do
  begin
    vSElem := PsgSimpleLTypeElement(FSimpleElements[vIndex]);
    case vSElem^.ComplexType of
      0:   // Dash/dot/space
        begin
          if vSElem^.Value > 0 then
          begin   // dash
            AddPt(ADottedPoints, APoint1, vDeltaPoint, vCur/vLen);
            if (vCounterPatterns < vAmountPatterns) or (FUniForm <> 0) then
              vCur := vCur + vSElem^.Value*Ord(vCur <> 0) +
                vPartLineLength*Ord(vCur = 0)
            else
              vCur := vLen;
            if vCur > vLen then
              vCur := vLen;
            AddPt(ADottedPoints, APoint1, vDeltaPoint, vCur/vLen);
          end
          else
            if vSElem^.Value = 0 then
            begin // dot
              if (vCounterPatterns = vAmountPatterns) and (FUniForm <> 2) then
                vCur := vLen;
              AddPt(ADottedPoints, APoint1, vDeltaPoint, vCur/vLen);
              AddPt(ADottedPoints, APoint1, vDeltaPoint, vCur/vLen);
            end
            else  // space
              vCur := vCur + Abs(vSElem^.Value) + vPartLineLength*Ord(vCur = 0);
        end;
      1, 5:// SHX, Shape
        begin
          DoTextPoints(ADottedPoints, vSElem^.Points, Slice(vSElem^.Counts^, vSElem^.Count), APoint1, vDeltaPoint,
            vCur/vLen, vSElem^.ComplexType and 4 <> 0);
        end;
      2, 6:// TTF
        begin
          DoTextPoints(ADottedPoints, vSElem^.Points, Slice(vSElem^.Counts^, vSElem^.Count), APoint1, vDeltaPoint,
            vCur/vLen, vSElem^.ComplexType and 4 <> 0);
        end;
    end;
    if CompareValues(vCur, vLen) > 0 then Exit;
    Inc(vIndex);
    if vIndex > vCount then
    begin
      vIndex := 0;
      Inc(vCounterPatterns);// another one pattern has been added
    end;
  end;
end;

procedure TsgLines.Link(AConverter: TObject);
{$IFNDEF SG_CADIMPORTERDLLDEMO}
{$ENDIF}
begin
{$IFNDEF SG_CADIMPORTERDLLDEMO}
{$ENDIF}
end;

procedure TsgLines.Loaded(AConverter: Pointer);
var
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  Converter: TsgDXFConverterAccess absolute AConverter;
  vText: TsgDXFText;
  vRect: TFRect;
  vType: Byte;
  TextLinesCollection: TsgTextLinesCollection;
{$ENDIF}
  vElem: TsgLTypeElement;
  vPSElem: PsgSimpleLTypeElement;
  I: Integer;

  procedure NewSElem;
  begin
    New(vPSElem);
    FillChar(vPSElem^, SizeOf(vPSElem^), 0);
  end;
begin
  FPatternLength := 0;
  FMaxSizeOfPatterns := 0;
  ClearSimpleElements;
  // Generates new list of simplified records
  for I := 0 to FElementsCount - 1 do
  begin
    vElem := FElements[I];
    NewSElem;
    vPSElem^.Value := vElem.Dash;
    FPatternLength := FPatternLength + Abs(vElem.Dash);
    FMaxSizeOfPatterns := FMaxSizeOfPatterns + Abs(vElem.Dash); //evg
{$IFNDEF SG_CADIMPORTERDLLDEMO}
    if vElem.ComplexType and 2 <> 0 then
      vText := TsgDXFText.Create
    else
      if (vElem.ComplexType and 4 <> 0) and (vElem.Style <> nil) then
        vText := TsgDXFShape.Create
      else
        vText := nil;
    if vText <> nil then
    begin
      if vElem.Dash <> 0.0 then
        AddSimpleElement(vPSElem);
      vText.Point := MakeFPoint(vElem.XOffset, vElem.YOffset, 0);
      vText.Rotation := vElem.Rotation;
      vText.Text := vElem.Text;
      if vText is TsgDXFShape then
        TsgDXFShape(vText).ShapeNumber := vElem.ShapeNumber;
      vText.Style := TsgDXFStyle(vElem.Style);
      if vText.Height <> 0 then
        vText.Height := vText.Height * vElem.Scale
      else
        vText.Height := vElem.Scale;
      vText.ObliqueAngle := 0;
      vText.Scale := 1;
      if Converter <> nil then
        Converter.Loads(vText);
      TextLinesCollection := TsgTextLinesCollection.Create(TFPointList.Create, True);
      vType := 0;
      if not IsBadRect(vText.Box) then
      begin //evg
        vRect := vText.Box;
        FMaxSizeOfPatterns := FMaxSizeOfPatterns + Abs(vRect.TopLeft.X - vRect.BottomRight.X); //evg
        FMaxSizeOfPatterns := FMaxSizeOfPatterns + Abs(vRect.TopLeft.Y - vRect.BottomRight.Y); //evg
        vType := GetTextPath(Converter, vText, TextLinesCollection);
      end;
      vText.Free;
      if vType = 0 then
      begin
        FreeAndNil(TextLinesCollection);
        if IndexOfSimpleElements(vPSElem) < 0 then
          Dispose(vPSElem);
        continue;
      end
      else
      begin
        if IndexOfSimpleElements(vPSElem) >= 0 then
          NewSElem;
        vPSElem^.ComplexType := vType;

        vPSElem^.Points := PFPointArray(TextLinesCollection.ReleaseItems);
        vPSElem^.Count := TextLinesCollection.Counts.Count;
        vPSElem^.Counts := TextLinesCollection.Counts.List;
        TsgBaseListAccess(TextLinesCollection.Counts).Attach(nil, 0);
        FreeAndNil(TextLinesCollection);

        vPSElem^.Value := vElem.Rotation;
        if vElem.ComplexType and 1 <> 0 then
          vPSElem^.ComplexType := vPSElem^.ComplexType or 4;// Absolute rotation
      end;
    end;
{$ENDIF}
    AddSimpleElement(vPSElem);
  end;
  if FPatternLength = 0 then
    ClearSimpleElements;
end;
    
procedure TsgLines.Poly(APoints, ADottedPoints: TFPointList;
  const AClose: Boolean);
var
  I: Integer;
  vPoint1, vPoint2: TFPoint;
begin
  for I := 0 to APoints.Count - 2 do
  begin
    vPoint1 := APoints.List[I];
    vPoint2 := APoints.List[I+1];
    if (I <> 0) and IsEqualFPoints(vPoint1, vPoint2) then
      Continue;
    Line(vPoint1, vPoint2, ADottedPoints);
  end;
  if AClose and (APoints.Count > 1) then//  Addition of the first point of a polyline
  begin
    vPoint1 := APoints.List[APoints.Count - 1];
    vPoint2 := APoints.List[0];
    if not IsEqualFPoints(vPoint1, vPoint2) then
      Line(vPoint1, vPoint2, ADottedPoints);
  end;
end;

function TsgLines.MaxSizeOfPatterns: Double;
begin
  Result := FMaxSizeOfPatterns;
end;

procedure TsgLines.Vertexes(const APoints: TFPointList; ACounts: TsgIntegerList;
    ADottedPoints: TFPointList;  const Close: Boolean);

  function EqualPoints(const AP1, AP2: TFPoint): Boolean;
  begin
    Result := (AP1.X = AP2.X) and (AP1.Y = AP2.Y) and (AP1.Z = AP2.Z);
  end;

var
  I, vAPointsIndex, K: Integer;
  Pt1, Pt2: TFPoint;
  vList: TFPointList;
  vLastPoint, vPoint: TFPoint;
begin
  vList := TFPointList.Create;
  try
    if Uniform <> 0 then
    begin
      vList.Capacity := APoints.Count + 1;
      for I := 0 to APoints.Count - 1 do
      begin
        vPoint := APoints.List[I];
        if (I = 0) or (not EqualPoints(vLastPoint, vPoint)) then
        begin
          vLastPoint := vPoint;
          vList.Add(vLastPoint);
        end;
      end;
      if Close and (vList.Count > 1) then
      begin
        if not EqualPoints(vList.First, vList.Last) then
          vList.Add(vList.First);
      end;
      if IsAutocadMetod then
        Curve(vList, ADottedPoints, Close)
      else
        Curve(vList, ADottedPoints, False);
    end
    else
    begin
      vAPointsIndex := 0;
      for I := 0 to ACounts.Count - 1 do
      begin
        if vAPointsIndex >= APoints.Count - 1then //evg this is for lines. for arcs it will go on
          Break;
        Pt1 := APoints.Items[vAPointsIndex];
        Pt2 := APoints.Items[vAPointsIndex + 1];
        if (vAPointsIndex <> 0) and EqualPoints(Pt1, Pt2) then
        begin
          Inc(vAPointsIndex);
          Continue;
        end;
        if ACounts[I] = 1 then
          Line(Pt1, Pt2, ADottedPoints)
        else
        begin
          K := 0;
          while K < ACounts[I] do
          begin
            vList.Add(APoints[vAPointsIndex]);
            Inc(vAPointsIndex);
            Inc(K);
          end;
          Curve(vList,ADottedPoints, False);
          vList.Clear(False);
          Continue;
        end;
        Inc(vAPointsIndex);
      end;

      if Close and (APoints.Count > 1) then//  Addition of the first point of a polyline
      begin
        Pt1 := APoints.Items[APoints.Count - 1];
        Pt2 := APoints.Items[0];
        if not EqualPoints(Pt1, Pt2) then
          Line(Pt1, Pt2, ADottedPoints);
      end;
    end;
  finally
    vList.Free;
  end;
end;

end.

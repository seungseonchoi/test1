{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                       HATCH parsing                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgHatch;

interface

{$INCLUDE SGDXF.inc}

uses
  Classes
{$IFDEF SGDEL_XE2}
  ,System.Types
{$ENDIF}
  ,sgConsts, SysUtils, Math, sgFunction, sgLists;

type
  PLineWithBaseP = ^TLineWithBaseP;
  TLineWithBaseP = record
    P1, P2, BP: TF2DPoint;
  end;

procedure ParseLinesHatch(ABoundaryPolylines: TsgObjectList; AHatchLine: PLineWithBaseP;
  AParsedLines: TList); overload;
procedure ParseLinesHatch(ABoundaryPolylines: TsgObjectList; AHatchLine: PLineWithBaseP;
  const AParsedLines: TF2DPointList); overload;

implementation

uses
  sgComparer;

type
  TsgListAccess = class(TsgList);
  TF2DPointListAccess = class(TF2DPointList);

  Isg2DPoints = interface
    ['{28DC8B9C-A155-4DF3-B1B0-393C40B3597E}']
    function GetProcCompare: TsgObjProcCompare;
    procedure SetProcCompare(const AValue: TsgObjProcCompare);
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TF2DPoint;
    procedure Delete(const AIndex: Integer);
    function Add(const APoint: TF2DPoint): Integer;
    procedure Sort(FirstIndex, LastIndex: Integer);
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TF2DPoint read GetItem; default;
  end;

  TsgParserLineHatch = class
  private
    FAccuracyCmp: Double;
    FBoundaryEdges: TList;
    FBoxesBoundary: TList;
    FBoundaryPoint1: TFPoint;
    FBoundaryPoint2: TFPoint;
    FBoundaryPolylines: TsgObjectList;
    FParsedLines: Isg2DPoints;
    FParsedLinesPrevCount: Integer;
    FPoint1: TFPoint;
    FPoint2: TFPoint;
    FUseYSort: Boolean;
  protected
    function IsEqual(const AP1, AP2: TFPoint): Boolean; overload;
    function IsEqual(const AP1, AP2: TF2DPoint): Boolean; overload;
    procedure SortParsedLines;
    procedure SpliteLine;
    function CalcVisibilitySegment(const AMiddlePoint: TFPoint; const ABoxes: TList): Byte;
    procedure CalcBoxBoundary;
    procedure DeleteSamePoints;
  public
    procedure Parse(const ABoundaryPolylines: TsgObjectList; const AP1, AP2: TF2DPoint;
      const AParsedLines: Isg2DPoints);
  end;

  TF2DPointListListWrapper = class(TInterfacedObject, Isg2DPoints)
  protected
    FList: TList;
    FSortList: TsgList;
    function GetCount: Integer;
    procedure Delete(const AIndex: Integer);
    procedure Sort(FirstIndex, LastIndex: Integer);
    function GetProcCompare: TsgObjProcCompare;
    procedure SetProcCompare(const AValue: TsgObjProcCompare);
    function GetItem(const AIndex: Integer): TF2DPoint;
    function Add(const APoint: TF2DPoint): Integer;
  public
    constructor Create(const AList: TList);
    destructor Destroy; override;
  end;

  TF2DPointListWrapper = class(TInterfacedObject, Isg2DPoints)
  protected
    FList: TF2DPointList;
    function GetProcCompare: TsgObjProcCompare;
    procedure SetProcCompare(const AValue: TsgObjProcCompare);
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TF2DPoint;
    procedure Delete(const AIndex: Integer);
    function Add(const APoint: TF2DPoint): Integer;
    procedure Sort(FirstIndex, LastIndex: Integer);
  public
    //property List: TF2DPointListAccess read FList implements IsgCollectionF2DPoint;
    constructor Create(const AList: TF2DPointList);
  end;

{ TF2DPointListListWrapper }

function TF2DPointListListWrapper.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TF2DPointListListWrapper.Add(const APoint: TF2DPoint): Integer;
var
  P: PF2DPoint;
begin
  New(P);
  P^ := APoint;
  Result := FList.Add(P);
end;

constructor TF2DPointListListWrapper.Create(const AList: TList);
begin
  FList := AList;
  FSortList := TsgList.Create;
  FSortList.Capacity := 0;
end;

procedure TF2DPointListListWrapper.Delete(const AIndex: Integer);
var
  P: PF2DPoint;
begin
  P := PF2DPoint(FList[AIndex]);
  FList.Delete(AIndex);
  Dispose(P);
end;

destructor TF2DPointListListWrapper.Destroy;
begin
  FSortList.Free;
  inherited Destroy;
end;

procedure TF2DPointListListWrapper.Sort(FirstIndex, LastIndex: Integer);
begin
  if GetCount > 0 then
  begin
    TsgListAccess(FSortList).Attach(FList.List, FList.Count);
    FSortList.Sort(FirstIndex, LastIndex);
    TsgListAccess(FSortList).Attach(nil, 0);
  end;
end;

procedure TF2DPointListListWrapper.SetProcCompare(const AValue: TsgObjProcCompare);
begin
  FSortList.ProcCompare := AValue;
end;

function TF2DPointListListWrapper.GetItem(const AIndex: Integer): TF2DPoint;
begin
  Result := PF2DPoint(FList[AIndex])^;
end;

function TF2DPointListListWrapper.GetProcCompare: TsgObjProcCompare;
begin
  Result := FSortList.ProcCompare;
end;

{ TF2DPointListWrapper }

function TF2DPointListWrapper.Add(const APoint: TF2DPoint): Integer;
begin
  Result := TF2DPointListAccess(FList).AddBase(@APoint);
end;

constructor TF2DPointListWrapper.Create(const AList: TF2DPointList);
begin
  FList := AList;
end;

procedure TF2DPointListWrapper.Delete(const AIndex: Integer);
begin
  FList.Delete(AIndex, 1);
end;

function TF2DPointListWrapper.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TF2DPointListWrapper.GetItem(const AIndex: Integer): TF2DPoint;
begin
  Result := FList[AIndex];
end;

function TF2DPointListWrapper.GetProcCompare: TsgObjProcCompare;
begin
  Result := FList.ProcCompare;
end;

procedure TF2DPointListWrapper.SetProcCompare(const AValue: TsgObjProcCompare);
begin
  FList.ProcCompare := AValue;
end;

procedure TF2DPointListWrapper.Sort(FirstIndex, LastIndex: Integer);
begin
  FList.Sort(FirstIndex, LastIndex);
end;

{ ParseLinesHatch

  ABoundaryPolylines is list of TF2DPointList
  AHatchLine is list of lines-coords
  AParsedLines is list for writing parsed lines

  Truncates lines of hatch.                              }

procedure ParseLinesHatch(ABoundaryPolylines: TsgObjectList; AHatchLine: PLineWithBaseP;
  const AParsedLines: TF2DPointList);
var
  vParserLineHatch: TsgParserLineHatch;
begin
  vParserLineHatch := TsgParserLineHatch.Create;
  try
    vParserLineHatch.Parse(ABoundaryPolylines, AHatchLine^.P1, AHatchLine^.P2, TF2DPointListWrapper.Create(AParsedLines));
  finally
    vParserLineHatch.Free;
  end;
end;

{ ParseLinesHatch

  ABoundaryPolylines is list of TF2DPointList
  AHatchLine is list of lines-coords
  AParsedLines is list for writing parsed lines (element==PF2DPoint)

  Truncates lines of hatch.                              }

procedure ParseLinesHatch(ABoundaryPolylines: TsgObjectList; AHatchLine: PLineWithBaseP;
  AParsedLines: TList);
var
  vParserLineHatch: TsgParserLineHatch;
begin
  vParserLineHatch := TsgParserLineHatch.Create;
  try
    vParserLineHatch.Parse(ABoundaryPolylines, AHatchLine^.P1, AHatchLine^.P2,
      TF2DPointListListWrapper.Create(AParsedLines));
  finally
    vParserLineHatch.Free;
  end;
end;

procedure TsgParserLineHatch.CalcBoxBoundary;
var
  I, J: Integer;
  vBox: PFRect;
  vBoundaryPolyline: TF2DPointList;
begin
  if FBoxesBoundary = nil then
    Exit;
  // Calculation  Box by all boundary
  for I := 0 to FBoundaryPolylines.Count - 1 do
  begin
    vBoundaryPolyline := TF2DPointList(FBoundaryPolylines[I]);
    if (vBoundaryPolyline = nil) or (vBoundaryPolyline.Count = 0) then
    begin
      FBoxesBoundary.Add(nil);
      Continue;
    end;
    New(vBox);
    vBox^ :=  cnstBadRect;
    for J := vBoundaryPolyline.Count - 1 downto 0 do
      ExpandFRect2D(vBox^, MakeFPointFrom2D(vBoundaryPolyline[J]));
    vBox^.Z1 := 0;
    vBox^.Z2 := 0;
    FBoxesBoundary.Add(vBox);
  end;
end;

function TsgParserLineHatch.CalcVisibilitySegment(
  const AMiddlePoint: TFPoint; const ABoxes: TList): Byte;
var
  I: Integer;
  vLine: PsgLineState;
begin
  for I := 0 to FBoundaryEdges.Count-1 do
  begin
    vLine := PsgLineState(FBoundaryEdges.List[I]);
    if IsPointOnSegmentPts(vLine^.Point1, vLine^.Point2, AMiddlePoint) then
    begin
      Result := 1;
      Exit;
    end;
  end;
  if IsPointInPolyPolyline(FBoundaryPolylines, AMiddlePoint, ABoxes, fDoubleResolution) then //fExtendedResolution
    Result := 254
  else
    Result := 2
end;

procedure TsgParserLineHatch.DeleteSamePoints;
var
  I: Integer;
begin
  I := FParsedLinesPrevCount;
  while I < FParsedLines.Count - 1 do
  begin
    if IsEqual(FParsedLines[I], FParsedLines[I+1]) then
      FParsedLines.Delete(I)
    else
      Inc(I);
  end;
end;

function TsgParserLineHatch.IsEqual(const AP1, AP2: TFPoint): Boolean;
begin
  Result := IsEqualFPoints2D(AP1, AP2, fAccuracy);
end;

function TsgParserLineHatch.IsEqual(const AP1, AP2: TF2DPoint): Boolean;
begin
  Result := IsEqualF2DPoints(AP1, AP2, fAccuracy);
end;

procedure TsgParserLineHatch.Parse(const ABoundaryPolylines: TsgObjectList;
  const AP1, AP2: TF2DPoint; const AParsedLines: Isg2DPoints);
var
  I, vOldState, vState: Integer;
  vMiddlePoint: TFPoint;
begin
  if ABoundaryPolylines.Count = 0 then Exit;
  FAccuracyCmp := fDoubleResolution;
  FBoundaryPolylines := ABoundaryPolylines;
  FParsedLines := AParsedLines;
  FParsedLinesPrevCount := FParsedLines.Count;// previous number of points
  FPoint1.X:= AP1.X;
  FPoint1.Y:= AP1.Y;
  FPoint2.X:= AP2.X;
  FPoint2.Y:= AP2.Y;
  FUseYSort := True;
  FBoundaryEdges := TList.Create;
  FBoxesBoundary := TList.Create;
  try
    SpliteLine;
    // Sorting of the added coordinates of points of last hatch-line
    SortParsedLines;
    DeleteSamePoints;
    CalcBoxBoundary;
    //
    vOldState := 2;
    I := FParsedLinesPrevCount;
    vMiddlePoint.Z := 0;
    while I < FParsedLines.Count - 1 do
    begin
      vMiddlePoint.Point2D := MiddleF2DPoint(FParsedLines[I], FParsedLines[I+1]);
      vState := CalcVisibilitySegment(vMiddlePoint, FBoxesBoundary);
      if vState <= 2 then
        vState := 2;
      if vState = vOldState then
        FParsedLines.Delete(I)
      else
        Inc(I);
      vOldState := vState;
    end;
  finally
    FreeRecordList(FBoundaryEdges);
    FreeRecordList(FBoxesBoundary);
  end;
end;

procedure TsgParserLineHatch.SortParsedLines;
var
  vProcCompare: TsgObjProcCompare;
begin
  if (FParsedLines.Count > 0) and (FParsedLinesPrevCount < FParsedLines.Count - 1) then
  begin
    vProcCompare := FParsedLines.GetProcCompare;
    if FUseYSort then
      FParsedLines.SetProcCompare(TsgPointerTypeComparer.CmpF2DPointByYX)
    else
      FParsedLines.SetProcCompare(TsgPointerTypeComparer.CmpF2DPointByX);
    FParsedLines.Sort(FParsedLinesPrevCount, FParsedLines.Count - 1);
    FParsedLines.SetProcCompare(vProcCompare);
  end;
end;

procedure TsgParserLineHatch.SpliteLine;
var
  I, J: Integer;
  vBoundaryPolyline: TF2DPointList;
  vCrossPoint: TFPoint;
  vP1, vP2, vTmpPoint: PF2DPoint;
  vLine: PsgLineState;
begin
  for I := 0 to FBoundaryPolylines.Count - 1 do
  begin
    vBoundaryPolyline := TF2DPointList(FBoundaryPolylines.List[I]);
    if (vBoundaryPolyline = nil) or (vBoundaryPolyline.Count = 0) then
      Continue;
    vP2 := @vBoundaryPolyline.List^[0];
    for J := 1 to vBoundaryPolyline.Count do
    begin
      if J >= vBoundaryPolyline.Count then
        vTmpPoint := @vBoundaryPolyline.List^[J - vBoundaryPolyline.Count]
      else
        vTmpPoint := @vBoundaryPolyline.List^[J];
      if IsEqual(vTmpPoint^, vP2^) then
        Continue;
      vP1 := vP2;
      vP2 := vTmpPoint;
      FBoundaryPoint1 := MakeFPointFrom2D(vP1^);
      FBoundaryPoint2 := MakeFPointFrom2D(vP2^);
      // Segments are not parallel
      if IsCrossSegmentsPts(FPoint1, FPoint2, FBoundaryPoint1, FBoundaryPoint2, @vCrossPoint) then
        FParsedLines.Add(vCrossPoint.Point2D)
      else
      begin
        if IsParalleniarLinesPts(FPoint1, FPoint2, FBoundaryPoint1, FBoundaryPoint2) then
        begin
          New(vLine);
          FBoundaryEdges.Add(vLine);
          vLine^.Point1 := FBoundaryPoint1;
          vLine^.Point2 := FBoundaryPoint2;
        end;
      end;
    end;
  end;
end;

end.

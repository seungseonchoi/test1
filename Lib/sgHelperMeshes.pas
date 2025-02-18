{************************************************************}
{       CAD.FMX Cross Platform Library  for Delphi and       }
{       CAD.VCL Cross Platform Library  for Delphi / Lazarus }
{                                                            }
{                  Helper for meshes                         }
{                                                            }
{     Copyright (c) 2002 - 2022 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgHelperMeshes;
{$INCLUDE SGDXF.inc}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Math, sgConsts, DXFConv,
{$IFDEF SG_FIREMONKEY}
  System.Generics.Collections, RTLConsts, IOUtils,
  System.Math.Vectors, FMX.Types3D, FMX.Types, sgFMXObjects,
{$ELSE}
  GLScene, GLVectorFileObjects,
{$IFDEF GLSCENE13}
  GLVectorTypes, GLVectorGeometry, GLGeometryBB, GLVectorLists,
{$ELSE}
  VectorTypes, VectorGeometry, GeometryBB, VectorLists,
{$ENDIF}
{$ENDIF}
  sgLists, sgFunction;

const
  cnstAccuracyParam= 1e-03;
  cnstFindEdgesMesh: Boolean = True;

type
{$IFDEF GLSCENE13}
  TMeshObjectList = TGLMeshObjectList;
  TMeshObject = TGLMeshObject;
{$ENDIF}

  TsgFaceData = record
    Normal: TFPoint;
    Area: Double;
    Radius: Double;
  end;

  PsgEdge = ^TsgEdge;
  TsgEdge = record
    Point1: Integer;
    Point2: Integer;
    IsUses: Boolean;
    Min: Boolean;
    FaceData: Integer;
    FLinkEdge: PsgEdge;
    BLinkEdge: PsgEdge;
  end;

  TsgVertex = class(TList)
    Point: TFPoint;
  end;

  TsgLoop = class
  private
    FOwner: TObject;
    FEdges: TsgList;
    FBoundaries: TList;
    FBox: TFRect;
    function GetVertexByIndex(const AIndex: Integer): TFPoint;{$IFDEF USE_INLINE}inline;{$ENDIF}
   protected
    procedure AddEdge(const AEdge: PsgEdge);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetEdge(const AIndex: Integer): TsgEdge;
    function GetEdgeCount: Integer;
   public
    constructor Create(const AOwner: TObject);
    destructor Destroy; override;
    procedure Clear;
    function GetBoundaries(const AExtractBoundary: Boolean = False;
      const AAccuracy: DOuble = cnstAccuracyParam): TList;
    property Edge[const AIndex: Integer]: TsgEdge read GetEdge;
    property EdgeCount: Integer read GetEdgeCount;
  end;

  type
    PFPointHash = ^TFPointHash;
    TFPointHash = type UInt64;
    PPointHashArray = ^TFPointHashArray;
    TFPointHashArray = array[Byte] of TFPointHash;

  TFPointHashList = class(TsgInt64List)
  protected
    procedure SetDefaultCompareProc(var AProc: TsgObjProcCompare); override;
    function CmpPointHash(const A, B: Pointer): Integer;
  public
    function IndexOf(AHash: TFPointHash): Integer;
  end;

  TsgLoops = class
  private
    FEdges: TsgList;
    FFacesData: array of TsgFaceData;
    FCurrendFaceIndex: Integer;
    FLoops: TsgList;
    FAccuracy: Double;
    FHVertex: TFPointHashList;
    FVertecies: TList; // list of TsgVertex
    function GetLoopCount: Integer;
    function GetLoop(const AIndex: Integer): TsgLoop;
    procedure ClearEmptyLoops;
    function UnionLoops: Boolean;
    procedure InitLoop(const APoints: array of TFPoint;
      const ANormal: TFPoint;  const AArea: Double; const ARadius: Double);
    function IsEqualEdge(const AEdge1, AEdge2: PsgEdge; const AAccuracy: Double): Boolean;
    procedure InsertEdgeLink(AEdge, AEdgeLast: PsgEdge);
    procedure UnionTwoEgesLoop(AEdge1, AEdge2: PsgEdge);
    procedure AddEdge(const AP1, AP2: TFPoint; AIndexLoop: Integer;
      const AIndexFaceData: Integer);
    function CreateEdge(const AP1, AP2: Integer): PsgEdge;
  protected
    function GetIndexByVertex(const APoint: TFPoint): Integer;
    function GetVertexByIndex(const AIndex: Integer): TFPoint;
    function GetVertexExByIndex(const AIndex: Integer): TsgVertex;
    function GetFaceDataByEdge(const AEdge: PsgEdge): TsgFaceData;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(const AParamAccuracy: Double = cnstAccuracyParam);
    destructor Destroy; override;
    function BuildLoops(const ABlock: TsgDXFBlock): Boolean; overload;
{$IFNDEF SG_FIREMONKEY}
    function BuildLoops(const AMesh: TMeshObjectList): Boolean; overload;
    function BuildLoops(const ATrianglesList: TAffineVectorList;
      const ANormals: TAffineVectorList): Boolean; overload;
{$ENDIF}
    property Loop[const AIndex: Integer]: TsgLoop read GetLoop;
    property LoopCount: Integer read GetLoopCount;
  end;

function Vect2FPoint(const Vect: TAffineVector): TFPoint; overload;
{$IFNDEF SG_FIREMONKEY}
function Vect2FPoint(const Vect: TVector4f): TFPoint; overload;
{$ENDIF}
function FPoint2Vect(const APoint: TFPoint): TAffineVector;
function FPoint2VectNoZ(const APoint: TFPoint): TAffineVector;
function HashPoint(AValue: PFPoint): TFPointHash;{$IFDEF SG_INLINE} inline; {$ENDIF}


function BuildLoops(const ABlock: TsgDXFBlock; const AMatrix: TFMatrix;
  AParamAccuracy: Double = -1): TList;
{$IFNDEF SG_FIREMONKEY}
function BuildLoopsByMeshes(const AMesh: TMeshObjectList;
  const AMatrix: TFMatrix; AParamAccuracy: Double = -1): TList;
function BuildLoopsByMesh(const AMesh: TMeshObject;
  const AMatrix: TFMatrix; AParamAccuracy: Double = -1): TList;
{$ENDIF}

implementation

type
  TsgBaseListAccess = class(TsgBaseList);

function Vect2FPoint(const Vect: TAffineVector): TFPoint; overload;
begin
  Result := MakeFPoint(Vect.V[0], Vect.V[1], Vect.V[2]);
end;
{$IFNDEF SG_FIREMONKEY}
function Vect2FPoint(const Vect: TVector4f): TFPoint; overload;
begin
  Result := MakeFPoint(Vect.V[0], Vect.V[1], Vect.V[2]);
end;
{$ENDIF}

function FPoint2Vect(const APoint: TFPoint): TAffineVector;
begin
  Result.V[0] := EnsureRange(APoint.X, cnstMinSingleValue *0.01, cnstMaxSingleValue *0.01);
  Result.V[1] := EnsureRange(APoint.Y, cnstMinSingleValue *0.01, cnstMaxSingleValue *0.01);
  Result.V[2] := EnsureRange(APoint.Z, cnstMinSingleValue *0.01, cnstMaxSingleValue *0.01);
end;

function FPoint2VectNoZ(const APoint: TFPoint): TAffineVector;
begin
  Result.V[0] := APoint.X;
  Result.V[1] := APoint.Y;
  Result.V[2] := 0;
end;

function HashPoint(AValue: PFPoint): TFPointHash;{$IFDEF SG_INLINE} inline; {$ENDIF}
const
  LShift = 5;
  RShift = (SizeOf(TFPointHash) shl 3) - LShift;
var
  I: Integer;
begin
//  Result := GetHashCodePtr192(AValue);
  Result := 0;
  for I := 0 to SizeOf(AValue^) - 1 do
  begin
    Result := Result xor PByte(AValue)^;
    Result := (Result shr RShift) or (Result shl LShift);
    Inc(PByte(AValue));
  end;
// or
//  Result := PFPointHash(@AValue^.V[0])^;
//  Result := (Result shr RShift) or (Result shl LShift);
//  Result := Result xor PFPointHash(@AValue^.V[1])^;
//  Result := (Result shr RShift) or (Result shl LShift);
//  Result := Result xor PFPointHash(@AValue^.V[2])^;
//  Result := (Result shr RShift) or (Result shl LShift);
end;

procedure SpliteLoops(const ALoops: TList; const AParamAccuracy: Double);
const
  cnstAngleDelta = 80;
var
  I, J, P: Integer;
  vPoints, vPointsNew: TFPointList;
  vType, vAdd: Boolean;
  vCenter, vPCenter: TFPoint;
  vRadius, vPRadius, vAngle: Double;
  procedure AddNewPointsList;
  begin
    vPointsNew := TFPointList.Create;
    ALoops.Add(vPointsNew);
    if vType then
      vPointsNew.Add(vPoints[J - 2]);
    vPointsNew.Add(vPoints[J - 1]);
    vPointsNew.Add(vPoints[J]);
    P := Integer(vType);
    vPCenter := vCenter;
    vPRadius := vRadius;
  end;

begin
  vAdd := False;
  for I := 0 to ALoops.Count - 1 do
  begin
    vPoints := TFPointList(ALoops[I]);
    if vPoints.Count > 2 then
    begin
      vAdd := True;
      P := -1;
      vPointsNew := nil;
      vPCenter := cnstFPointZero;
      vPRadius := -1;
      J := 2;
      while J < vPoints.Count do
      begin
        vAngle := GetAngleOfVectors(SubFPoint(vPoints[J - 2], vPoints[J - 1]), SubFPoint(vPoints[J], vPoints[J - 1]), False, True);
        if (vAngle <= cnstAngleDelta) or (vAngle >= 180 - cnstAngleDelta) then
          vType := CalcCircleParams(vPoints[J - 2], vPoints[J - 1], vPoints[J], vCenter, vRadius)
        else
          vType := False;
        if (not Assigned(vPointsNew)) or (not vType) then
          AddNewPointsList
        else
        begin
          if P = Integer(vType) then
          begin
            if vType and IsEqualFPoints(vPCenter, vCenter, AParamAccuracy) and IsEqual(vPRadius, vRadius, AParamAccuracy) then
              vPointsNew.Add(vPoints[J])
            else
            begin
              if vPointsNew.Count = 3 then
                vPointsNew.Count := 2;
              AddNewPointsList;
            end;
          end
          else
            AddNewPointsList;
        end;
        Inc(J);
      end;
      vPoints.Clear(True);
    end;
  end;
  if vAdd then
  begin
    for I := ALoops.Count - 1 downto 0 do
    begin
      vPoints := TFPointList(ALoops[I]);
      if vPoints.Count = 0 then
      begin
        vPoints.Free;
        ALoops.Delete(I);
      end;
    end;
  end;
end;

procedure ExtractLoops(const ALoops: TsgLoops; const AList: TList;
  const AMatrix: TFMatrix; AParamAccuracy: Double = -1);
var
  I, J: Integer;
  vBoundaries: TList;
  vPoints: TFPointList;
begin
  for I := 0 to ALoops.LoopCount - 1 do
  begin
    vBoundaries := ALoops.Loop[I].GetBoundaries(True, AParamAccuracy);
    if Assigned(vBoundaries) then
    begin
      for J := 0 to vBoundaries.Count - 1 do
      begin
        vPoints := TFPointList(vBoundaries[J]);
        //??? 3
        if Assigned(vPoints) (*and (vPoints.Count > 3)*) then
        begin
          vPoints.Transform(AMatrix);
          AList.Add(vPoints)
        end
        else
          vPoints.Free;
      end;
      vBoundaries.Count := 0;
    end;
    vBoundaries.Free;
  end;
end;

function BuildLoops(const ABlock: TsgDXFBlock; const AMatrix: TFMatrix; AParamAccuracy: Double = -1): TList;
var
  vLoops: TsgLoops;
begin
  if AParamAccuracy < 0 then
    AParamAccuracy := cnstAccuracyParam;
  Result := TList.Create;
  vLoops := TsgLoops.Create(AParamAccuracy);
  try
    if vLoops.BuildLoops(ABlock) then
    begin
      ExtractLoops(vLoops, Result, AMatrix, AParamAccuracy);
    end;
  finally
    vLoops.Free;
  end;
//  if Result.Count > 0 then
//    SpliteLoops(Result, cnstAccuracyDimSTL);
end;

{$IFNDEF SG_FIREMONKEY}
function BuildLoopsByMeshes(const AMesh: TMeshObjectList;
  const AMatrix: TFMatrix; AParamAccuracy: Double = -1): TList;
var
  vLoops: TsgLoops;
begin
  if AParamAccuracy < 0 then
    AParamAccuracy := cnstAccuracyParam;
  Result := TList.Create;
  vLoops := TsgLoops.Create(AParamAccuracy);
  try
    if vLoops.BuildLoops(AMesh) then
    begin
      ExtractLoops(vLoops, Result, AMatrix, AParamAccuracy);
    end;
  finally
    vLoops.Free;
  end;
end;

function BuildLoopsByMesh(const AMesh: TMeshObject;
  const AMatrix: TFMatrix; AParamAccuracy: Double = -1): TList;
var
  vLoops: TsgLoops;
  vTrianglesList: TAffineVectorList;
  vNormals: TAffineVectorList;
begin
  if AParamAccuracy < 0 then
    AParamAccuracy := cnstAccuracyParam;
  Result := TList.Create;
  vLoops := TsgLoops.Create(AParamAccuracy);
  vNormals := TAffineVectorList.Create;
  vTrianglesList := AMesh.ExtractTriangles(nil, vNormals);
  try
    if vLoops.BuildLoops(vTrianglesList, vNormals) then
    begin
      ExtractLoops(vLoops, Result, AMatrix, AParamAccuracy);
    end;
  finally
    vLoops.Free;
    vNormals.Free;
    vTrianglesList.Free;
  end;
end;

{$ENDIF}

{ TFPointHashList }

function TFPointHashList.CmpPointHash(const A, B: Pointer): Integer;
begin
  Result := 0;
  if PFPointHash(A)^ > PFPointHash(B)^ then
    Inc(Result)
  else
    if PFPointHash(A)^ < PFPointHash(B)^ then
      Dec(Result);
end;

function TFPointHashList.IndexOf(AHash: TFPointHash): Integer;
begin
  Result := IndexOfBase(@AHash);
end;

procedure TFPointHashList.SetDefaultCompareProc(var AProc: TsgObjProcCompare);
begin
  AProc := CmpPointHash;
end;

{ TsgLoop }

constructor TsgLoop.Create(const AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FBox := cnstBadRect;
  FEdges := TsgList.Create;
end;

procedure TsgLoop.Clear;
begin
  FBox := cnstBadRect;
  FEdges.Clear;
end;

function TsgLoop.GetBoundaries(const AExtractBoundary: Boolean = False;
  const AAccuracy: DOuble = cnstAccuracyParam): TList;
const
  cnstAngleDelta = 33;
var
  I, J, vIndex: Integer;
  vEdge1, vEdge2: PsgEdge;
  vBoundary: TFPointList;
  vEdge1Point1, vEdge1Point2: TFPoint;
  vTemp: TsgIntegerList;

  function IsBreak(AEdge1, AEdge2: PsgEdge): Boolean;
  var
    vAngle: Double;
    vDist1, vDist2, vDelta: Double;
  begin
    Result := True;
    vDist1 := DistanceFPoint(GetVertexByIndex(AEdge1.Point2), GetVertexByIndex(AEdge1.Point1));
    vDist2 := DistanceFPoint(GetVertexByIndex(AEdge2.Point2), GetVertexByIndex(AEdge2.Point1));
    vDelta := (Min(vDist1, vDist2)/100)*30;
    vAngle := GetAngleOfVectors(SubFPoint(GetVertexByIndex(AEdge1.Point1),
                                          GetVertexByIndex(AEdge1.Point2)),
                                SubFPoint(GetVertexByIndex(AEdge2.Point2),
                                          GetVertexByIndex(AEdge2.Point1)),
                                          False, True);
    if  ((vAngle <= cnstAngleDelta) or (vAngle >= 180 - cnstAngleDelta))
      and (Abs(vDist1 - vDist2) < vDelta) then
      Result := False;
  end;

begin
  if not Assigned(FBoundaries) then
  begin
    FBoundaries := TList.Create;
    vTemp := TsgIntegerList.Create;
    try
      if FEdges.Count > 0 then
      begin
        if FEdges.Count > 1 then
        begin
          for I := 0 to FEdges.Count - 2 do
          begin
            vEdge1 := FEdges[I];
            for J := I + 1 to FEdges.Count - 1 do
            begin
              vEdge2 := FEdges[J];
              if vEdge1^.Point2 = vEdge2^.Point1 then
              begin
                FEdges[J] := FEdges[I + 1];
                FEdges[I + 1] := vEdge2;
                Break
              end
              else
                if vEdge1^.Point2 = vEdge2^.Point2 then
                begin
                  FEdges[J] := FEdges[I + 1];
                  FEdges[I + 1] := vEdge2;
                  SwapInts(vEdge2^.Point1, vEdge2^.Point2);
                  Break;
                end;
            end;
          end;
          //

          vTemp.Add(0);
          for I := 0 to FEdges.Count-2 do
          begin
             if IsBreak(FEdges[I], FEdges[I+1]) then
               vTemp.Add(I+1);
          end;

          if vTemp.Last <> FEdges.Count then
            vTemp.Add(FEdges.Count);
          //
          if vTemp.Count > 2 then
          begin
            if not IsBreak(FEdges[FEdges.Count - 1], FEdges[0]) then
            begin
              vIndex := vTemp.Items[1];
              FEdges.CyclicShiftRight(vIndex);
              for I := 1 to vTemp.Count - 1 do
                vTemp.Items[I] := vTemp.Items[I] - vIndex;
              vTemp.Delete(vTemp.Count - 1);
              vTemp.Delete(0);
              if vTemp.Last <> FEdges.Count then
                vTemp.Add(FEdges.Count);
            end;
          end;
        end;

        for J := 0 to vTemp.Count-2 do
        begin
          vBoundary := TFPointList.Create;
          FBoundaries.Add(vBoundary);
          //vBoundary.Capacity := FEdges.Count + 1;
          vEdge1 := FEdges[vTemp[J]];
          vBoundary.Add(GetVertexByIndex(vEdge1^.Point1));
          vBoundary.Add(GetVertexByIndex(vEdge1^.Point2));
          //if vTemp[J]+1 <> vTemp[J+1] then
          begin
            for I := vTemp[J]+1 to vTemp[J+1]-1 do
            begin
              vEdge1 := FEdges[I];
              vEdge1Point1 := GetVertexByIndex(vEdge1^.Point1);
              vEdge1Point2 := GetVertexByIndex(vEdge1^.Point2);
              if IsEqualFPoints(vBoundary.Last, vEdge1Point1) then
              begin
                if (vBoundary.Count > 1) and IsPointOnLinePts3D(vBoundary[vBoundary.Count - 2], vBoundary.Last, vEdge1Point2, AAccuracy) then
                  vBoundary.Last := vEdge1Point2
                else
                  vBoundary.Add(vEdge1Point2);
              end
              else
              begin
                vBoundary := TFPointList.Create;
                FBoundaries.Add(vBoundary);
                //vBoundary.Capacity := FEdges.Count - I + 1;
                vBoundary.Add(vEdge1Point1);
                vBoundary.Add(vEdge1Point2);
              end;
            end;
          end;
        end;
      end;
    finally
      vTemp.Free;
    end;
    if Assigned(FBoundaries) then
    begin
      for I := FBoundaries.Count - 1 downto 0 do
      begin
        vBoundary := TFPointList(FBoundaries[I]);
        if (not Assigned(vBoundary)) or (vBoundary.Count < 1) then
        begin
          FBoundaries.Delete(I);
          vBoundary.Free;
        end;
      end;
    end;
  end;
  Result := FBoundaries;
  if AExtractBoundary then
    FBoundaries := nil;
end;

destructor TsgLoop.Destroy;
begin
  Clear;
  FEdges.Free;
  FreeList(FBoundaries);
  inherited Destroy;
end;

procedure TsgLoop.AddEdge(const AEdge: PsgEdge);
begin
  FEdges.Add(AEdge);
  ExpandFRect(FBox, GetVertexByIndex(AEdge^.Point1));
  ExpandFRect(FBox, GetVertexByIndex(AEdge^.Point2));
end;

function TsgLoop.GetEdge(const AIndex: Integer): TsgEdge;
begin
  Result := PsgEdge(FEdges[Aindex])^;
end;

function TsgLoop.GetEdgeCount: Integer;
begin
  Result := FEdges.Count;
end;

function TsgLoop.GetVertexByIndex(const AIndex: Integer): TFPoint;
begin
  Result := TsgLoops(FOwner).GetVertexByIndex(AIndex);
end;

{ TsgLoops }

constructor TsgLoops.Create(const AParamAccuracy: Double = cnstAccuracyParam);
begin
  inherited Create;
  FAccuracy := AParamAccuracy;
  FLoops := TsgList.Create;
  FEdges := TsgList.Create;
  FCurrendFaceIndex := -1;
  FHVertex := TFPointHashList.Create;
  FHVertex.Sorted := True;
  FVertecies := TList.Create;
end;

function TsgLoops.CreateEdge(const AP1, AP2: Integer): PsgEdge;
begin
  New(Result);
  Result^.Point1 := AP1;
  Result^.Point2 := AP2;
end;

destructor TsgLoops.Destroy;
var
  I: Integer;
begin
  for I := 0 to FVertecies.Count - 1 do
    TObject(FVertecies[I]).Free;
  FVertecies.Free;
  FLoops.ClearTypeList(ptvObject);
  FLoops.Free;
  FEdges.ClearTypeList(ptvRecord);
  FEdges.Free;
  FHVertex.Free;
  inherited Destroy;
end;

function TsgLoops.GetLoopCount: Integer;
begin
  Result := FLoops.Count;
end;

function TsgLoops.GetLoop(const AIndex: Integer): TsgLoop;
begin
  Result := TsgLoop(FLoops[AIndex]);
end;

function TsgLoops.GetIndexByVertex(const APoint: TFPoint): Integer;
begin
  Result := FHVertex.IndexOf(HashPoint(@APoint));
end;

function TsgLoops.GetVertexByIndex(const AIndex: Integer): TFPoint;
begin
  Result := TsgVertex(FVertecies[AIndex]).Point;
end;

function TsgLoops.GetVertexExByIndex(const AIndex: Integer): TsgVertex;
begin
  Result := TsgVertex(FVertecies[AIndex]);
end;

function TsgLoops.GetFaceDataByEdge(const AEdge: PsgEdge): TsgFaceData;
begin
  Result := FFacesData[AEdge^.FaceData];
end;

procedure TsgLoops.InitLoop(const APoints: array of TFPoint;
  const ANormal: TFPoint; const AArea: Double; const ARadius: Double);
var
  I, J, vIndex, vBegin, vEnd: Integer;
  vMin, vDis: Double;
  vEdge: PsgEdge;
begin
  FFacesData[FCurrendFaceIndex].Normal := ANormal;
  FFacesData[FCurrendFaceIndex].Area := AArea;
  FFacesData[FCurrendFaceIndex].Radius := ARadius;

  vBegin := FEdges.Count;
  for I := Low(APoints) to High(APoints) do
  begin
    if I = High(APoints) then
      J := 0
    else
      J := I + 1;
    if not IsEqualFPoints(APoints[I], APoints[J]) then
      AddEdge(APoints[I], APoints[J], I, FCurrendFaceIndex);
  end;
  vEnd := FEdges.Count-1;
  if vBegin <= vEnd then
  begin
    vIndex := vBegin;
    vEdge := FEdges[vIndex];
    vMin := DistanceFPoint(GetVertexByIndex(vEdge^.Point2), GetVertexByIndex(vEdge^.Point1));
    for I := vBegin+1 to vEnd do
    begin
      vEdge := FEdges[I];
      vDis := DistanceFPoint(GetVertexByIndex(vEdge^.Point2), GetVertexByIndex(vEdge^.Point1));
      if vMin > vDis then
      begin
        vMin := vDis;
        vIndex := I;
      end;
    end;
    vEdge := FEdges[vIndex];
    vEdge^.Min := True;
  end;
  Inc(FCurrendFaceIndex);
end;

function TsgLoops.IsEqualEdge(const AEdge1, AEdge2: PsgEdge; const AAccuracy: Double): Boolean;
const
  AngleDelta = 41;
  AngleDeltaPlan = 5;
var
  vAngle: Double;
  vMax, vMin:Double;
//  vDelta: Double;

begin
  Result := False;
  if ((AEdge1^.Point1 = AEdge2^.Point1) and (AEdge1^.Point2 = AEdge2^.Point2)) or
     ((AEdge1^.Point1 = AEdge2^.Point2) and (AEdge1^.Point2 = AEdge2^.Point1)) then
  begin
    vAngle := GetAngleOfVectors(GetFaceDataByEdge(AEdge1).Normal,
       GetFaceDataByEdge(AEdge2).Normal, False, True);
    if (vAngle <= AngleDeltaPlan) or (vAngle >= 180 - AngleDeltaPlan) then
      Result := True
    else
    begin
//        vMax := Max(AEdge1^.Area, AEdge2^.Area);
//        vMin := Min(AEdge1^.Area, AEdge2^.Area);
//        if (vMax <> 0) and (vMin / vMax < 0.37) then    //0.37
//        begin
//          if (vAngle <= AngleDelta) or (vAngle >= 180 - AngleDelta) then
//              Result := True;
//          vMax := Max(AEdge1^.Radius, AEdge2^.Radius);
//          vMin := Min(AEdge1^.Radius, AEdge2^.Radius);
//          if (vMax <> 0) and (vMin / vMax > 0.1) then    //0.37
//          begin
//              Result := False;
//          end;
//        end;

        vMax := Max(GetFaceDataByEdge(AEdge1).Area, GetFaceDataByEdge(AEdge2).Area);
        vMin := Min(GetFaceDataByEdge(AEdge1).Area, GetFaceDataByEdge(AEdge2).Area);
        if (vMax <> 0) and (vMin / vMax > 0.37) then    //0.37
        begin
          if (vAngle <= AngleDelta) or (vAngle >= 180 - AngleDelta) then
            Result := True;
        end


//      if  ((AEdge1^.Min = False) or (AEdge2^.Min = False)) then
//      begin
//        vMax := Max(AEdge1^.Area, AEdge2^.Area);
//        vMin := Min(AEdge1^.Area, AEdge2^.Area);
//        if (vMax <> 0) and (vMin / vMax > 0.37) then
//        begin
//          if (vAngle <= AngleDelta) or (vAngle >= 180 - AngleDelta) then
//            Result := True;
//        end
//      end
//      else
//      begin
//        if (vAngle <= AngleDelta) or (vAngle >= 180 - AngleDelta) then
//          Result := True;
//      end;
    end;
  end;
end;

procedure TsgLoops.InsertEdgeLink(AEdge, AEdgeLast: PsgEdge);
var
  vEdgeFLink: PsgEdge;
begin
  vEdgeFLink := AEdgeLast^.FLinkEdge;
  AEdgeLast^.FLinkEdge := AEdge;
  AEdge^.BLinkEdge := AEdgeLast;
  AEdge^.FLinkEdge := vEdgeFLink;
  vEdgeFLink^.BLinkEdge := AEdge;
end;

procedure TsgLoops.UnionTwoEgesLoop(AEdge1, AEdge2: PsgEdge);
begin
  AEdge1^.BLinkEdge^.FLinkEdge := AEdge2^.FLinkEdge;
  AEdge2^.FLinkEdge^.BLinkEdge := AEdge1^.BLinkEdge;
  AEdge1^.FLinkEdge^.BLinkEdge := AEdge2^.BLinkEdge;
  AEdge2^.BLinkEdge^.FLinkEdge := AEdge1^.FLinkEdge;
end;

procedure TsgLoops.AddEdge(const AP1, AP2: TFPoint; AIndexLoop: Integer;
   const AIndexFaceData: Integer);
var
  vIndex1, vIndex2: Integer;
  vEdge: PsgEdge;
begin
  vIndex1 := GetIndexByVertex(AP1);
  vIndex2 := GetIndexByVertex(AP2);
  vEdge := CreateEdge(vIndex1, vIndex2);
  vEdge^.IsUses := True;
  vEdge^.FaceData := AIndexFaceData;
  vEdge^.Min := False;
  if (AIndexLoop = 0) or (FEdges.Count = 0) then
  begin
    vEdge^.FLinkEdge := vEdge;
    vEdge^.BLinkEdge := vEdge;
  end
  else
    InsertEdgeLink(vEdge, FEdges.Last);
  FEdges.Add(vEdge);
  GetVertexExByIndex(vIndex1).Add(vEdge);
  GetVertexExByIndex(vIndex2).Add(vEdge);
end;

function GetTempFile: string;
const
  cnstBufLen = 1024;
  cnstPrefix = 'vtx';
var
  {$IFNDEF SG_FIREMONKEY}
  P: PChar;
  {$ENDIF}
  S: string;
begin
  {$IFDEF SG_FIREMONKEY}
  S := AddLastSlash(TPath.GetTempPath);
  Result := S + cnstPrefix + TPath.GetTempFileName;
  {$ELSE}
  Result := '';
  GetMem(P, cnstBufLen);
  try
    {$IFDEF SGFPC}
    S := GetTempDir;
    {$ELSE}
    SetString(S, P, GetTempPath(cnstBufLen, P));
    {$ENDIF}
    if (S <> '') and (GetTempFileName(PChar(S), cnstPrefix, 0, P) <> 0) then
      SetString(Result, P, StrLen(P));
  finally
    FreeMem(P, cnstBufLen);
  end;
  {$ENDIF}
end;

{$IFNDEF SG_FIREMONKEY}
function TsgLoops.BuildLoops(const AMesh: TMeshObjectList): Boolean;
var
  vTrianglesList: TAffineVectorList;
  vNormals: TAffineVectorList;
begin
  vNormals := TAffineVectorList.Create;
  vTrianglesList := AMesh.ExtractTriangles(nil, vNormals);
  try
    Result := BuildLoops(vTrianglesList, vNormals);
  finally
    vNormals.Free;
    vTrianglesList.Free;
  end;
end;

function TsgLoops.BuildLoops(const ATrianglesList: TAffineVectorList;
  const ANormals: TAffineVectorList): Boolean;
var
  I, J: Integer;
  H: THandle;
  vEntity: TsgDXFEntity;
  vFace: TsgDXF3dFace absolute vEntity;
  vCenter: TFPoint;
  vRadius: Double;
  vCount, vCountFaces, vCountEdges: Integer;
  vData: PsgHashItemsArray;
  vPackedData: PPointHashArray;
  vVertex: TsgVertex;
  vVertexCollection: TsgCollection;
  vStream: TStream;
  vTmpFileName: string;

  vPoint, vPoint1, vPoint2: TFPoint;

  procedure AddPoint(AData: PsgHashItemsArray; var AIndex: THandle;
    AStream: TStream; const APoint: TFPoint);{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    AStream.Write(APoint, SizeOf(TFPoint));
    AData^[AIndex].HashCode := HashPoint(@APoint);
    THandle(AData^[AIndex].Data) := AIndex + 1;
    Inc(AIndex);
  end;

  function GetPointFromID(AStream: TStream; AIndex: THandle): TFPoint;{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    AStream.Position := (AIndex - 1) * SizeOf(TFPoint);
    AStream.Read(Result, SizeOf(TFPoint));
  end;

  function CeateVertexStream(var ATempFileName: string): TStream;
  begin
{$IFDEF LOOP_FACE_FILE_BUFFERED}
    Result := nil;
    ATempFileName := GetTempFile;
    if ATempFileName <> '' then
    begin
      try
        Result := TFileStream.Create(ATempFileName, fmCreate);
      except
        on E:Exception do
        begin
          ATempFileName := '';
          Result := nil;
        end;
      end;
    end;
    if Result = nil then
{$ENDIF}
      Result := TMemoryStream.Create;
  end;

begin
  Result := False;
  vCountFaces := ATrianglesList.Count div 3;
  vCountEdges := ATrianglesList.Count;//vCountFaces * 3;
  vVertexCollection := TsgCollection.Create;
  try
    vStream := CeateVertexStream(vTmpFileName);
    try
      vStream.Size := (vCountFaces * 3) * SizeOf(TFPoint);
      vStream.Position := 0;
      vVertexCollection.Duplicates := dupAccept;
      GetMem(vData, SizeOf(TsgHashItem) * vCountEdges);
      TsgBaseListAccess(vVertexCollection).Attach(vData, vCountEdges);

      H := 0;
      for I := 0 to (ATrianglesList.Count div 3) - 1 do
      begin
        begin
          AddPoint(vData, H, vStream, Vect2FPoint(ATrianglesList[I*3]));
          AddPoint(vData, H, vStream, Vect2FPoint(ATrianglesList[I*3+1]));
          AddPoint(vData, H, vStream, Vect2FPoint(ATrianglesList[I*3+2]));
        end;
      end;
      vVertexCollection.Sort;

      J := 0;
      vCount := vVertexCollection.Count;
      while J < vVertexCollection.Count do
      begin
        I := J;
        Inc(I);
        while (I < vVertexCollection.Count) and
          (vData^[J].HashCode = vData^[I].HashCode) do
        begin
          vData^[I].Data := nil;
          Dec(vCount);
          Inc(I);
        end;
        J := I
      end;
      FHVertex.Duplicates := dupAccept;
      GetMem(vPackedData, SizeOf(TFPointHash) * vCount);
      TsgBaseListAccess(FHVertex).Attach(vPackedData, vCount);
      FVertecies.Count := vCount;
      J := 0;
      I := 0;
      while J < vVertexCollection.Count do
      begin
        if vData^[J].Data <> nil then
        begin
          vPackedData^[I] := vData^[J].HashCode;
          vVertex := TsgVertex.Create;
          vVertex.Point := GetPointFromID(vStream, THandle(vData^[J].Data));
          FVertecies[I] := vVertex;
          Inc(I);
        end;
        Inc(J);
      end;
    finally
      vStream.Free;
      if vTmpFileName <> '' then
        DeleteFile(vTmpFileName);
    end;
  finally
    vVertexCollection.Free;
  end;
  FHVertex.Sort;

  FEdges.Capacity := vCountEdges;
  SetLength(FFacesData, vCountFaces);
  FCurrendFaceIndex := 0;

  for I := 0 to (ATrianglesList.Count div 3) - 1 do
  begin
    begin
     vPoint := Vect2FPoint(ATrianglesList[I*3]);
     vPoint1 := Vect2FPoint(ATrianglesList[I*3+1]);
     vPoint2 := Vect2FPoint(ATrianglesList[I*3+2]);
        if not CalcCircleParams(vPoint, vPoint1, vPoint2,
          vCenter, vRadius) then
          vRadius := 0;

        InitLoop([vPoint, vPoint1, vPoint2],
          sgCalcPlaneNormal(vPoint, vPoint1, vPoint2),
          sgAreaOfTriangle(vPoint, vPoint1, vPoint2), vRadius);
    end;
  end;

  if UnionLoops then
    Result := True;
  ClearEmptyLoops;
end;
{$ENDIF}

function TsgLoops.BuildLoops(const ABlock: TsgDXFBlock): Boolean;
var
  I, J: Integer;
  H: THandle;
  vEntity: TsgDXFEntity;
  vFace: TsgDXF3dFace absolute vEntity;
  vCenter: TFPoint;
  vRadius: Double;
  vCount, vCountFaces, vCountEdges: Integer;
  vData: PsgHashItemsArray;
  vPackedData: PPointHashArray;
  vVertex: TsgVertex;
  vVertexCollection: TsgCollection;
  vStream: TStream;
  vTmpFileName: string;

  procedure AddPoint(AData: PsgHashItemsArray; var AIndex: THandle;
    AStream: TStream; const APoint: TFPoint);{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    AStream.Write(APoint, SizeOf(TFPoint));
    AData^[AIndex].HashCode := HashPoint(@APoint);
    THandle(AData^[AIndex].Data) := AIndex + 1;
    Inc(AIndex);
  end;

  function GetPointFromID(AStream: TStream; AIndex: THandle): TFPoint;{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    AStream.Position := (AIndex - 1) * SizeOf(TFPoint);
    AStream.Read(Result, SizeOf(TFPoint));
  end;

  function CeateVertexStream(var ATempFileName: string): TStream;
  begin
{$IFDEF LOOP_FACE_FILE_BUFFERED}
    Result := nil;
    ATempFileName := GetTempFile;
    if ATempFileName <> '' then
    begin
      try
        Result := TFileStream.Create(ATempFileName, fmCreate);
      except
        on E:Exception do
        begin
          ATempFileName := '';
          Result := nil;
        end;
      end;
    end;
    if Result = nil then
{$ENDIF}
      Result := TMemoryStream.Create;
  end;

begin
  Result := False;
  vCountFaces := 0;
  for I := 0 to ABlock.Count - 1 do
    if ABlock.Entities[I].EntType = ce3dFace then
      Inc(vCountFaces);

  vCountEdges := vCountFaces * 3;
  vVertexCollection := TsgCollection.Create;
  try
    vStream := CeateVertexStream(vTmpFileName);
    try
      vStream.Size := (vCountFaces * 3) * SizeOf(TFPoint);
      vStream.Position := 0;
      vVertexCollection.Duplicates := dupAccept;
      GetMem(vData, SizeOf(TsgHashItem) * vCountEdges);
      TsgBaseListAccess(vVertexCollection).Attach(vData, vCountEdges);

      H := 0;
      for I := 0 to ABlock.Count - 1 do
      begin
        vEntity := ABlock.Entities[I];
        if vEntity.EntType = ce3dFace then
        begin
          AddPoint(vData, H, vStream, vFace.Point);
          AddPoint(vData, H, vStream, vFace.Point1);
          AddPoint(vData, H, vStream, vFace.Point2);
        end;
      end;
      vVertexCollection.Sort;

      J := 0;
      vCount := vVertexCollection.Count;
      while J < vVertexCollection.Count do
      begin
        I := J;
        Inc(I);
        while (I < vVertexCollection.Count) and
          (vData^[J].HashCode = vData^[I].HashCode) do
        begin
          vData^[I].Data := nil;
          Dec(vCount);
          Inc(I);
        end;
        J := I
      end;
      FHVertex.Duplicates := dupAccept;
      GetMem(vPackedData, SizeOf(TFPointHash) * vCount);
      TsgBaseListAccess(FHVertex).Attach(vPackedData, vCount);
      FVertecies.Count := vCount;
      J := 0;
      I := 0;
      while J < vVertexCollection.Count do
      begin
        if vData^[J].Data <> nil then
        begin
          vPackedData^[I] := vData^[J].HashCode;
          vVertex := TsgVertex.Create;
          vVertex.Point := GetPointFromID(vStream, THandle(vData^[J].Data));
          FVertecies[I] := vVertex;
          Inc(I);
        end;
        Inc(J);
      end;
    finally
      vStream.Free;
      if vTmpFileName <> '' then
        DeleteFile(vTmpFileName);
    end;
  finally
    vVertexCollection.Free;
  end;
  FHVertex.Sort;

  FEdges.Capacity := vCountEdges;
  SetLength(FFacesData, vCountFaces);
  FCurrendFaceIndex := 0;

  for I := 0 to ABlock.Count - 1 do
  begin
    vEntity := ABlock.Entities[I];
    if vEntity.EntType = ce3dFace then
    begin
        if not CalcCircleParams(vFace.Point, vFace.Point1, vFace.Point2,
          vCenter, vRadius) then
          vRadius := 0;

        InitLoop([vFace.Point, vFace.Point1, vFace.Point2],
          sgCalcPlaneNormal(vFace.Point, vFace.Point1, vFace.Point2),
          sgAreaOfTriangle(vFace.Point, vFace.Point1, vFace.Point2), vRadius);
    end;
  end;

  if UnionLoops then
    Result := True;
  ClearEmptyLoops;
end;

function TsgLoops.UnionLoops: Boolean;
var
  I: Integer;
  vLoop: TsgLoop;
  vEdge, vEdgeAuxiliary: PsgEdge;
  vVertex: TsgVertex;

  function WorkLoop(AEdgeStart: PsgEdge; ALoop: TsgLoop): Boolean;
  var
    vEdgeCurrent: PsgEdge;
  begin
    Result := False;
    vEdgeCurrent := AEdgeStart;
    Repeat
      vEdgeCurrent := vEdgeCurrent^.FLinkEdge;
      if vEdgeCurrent^.IsUses <> False then
      begin
        vLoop.AddEdge(vEdgeCurrent);
        vEdgeCurrent^.IsUses := False;
      end;
    Until vEdgeCurrent = AEdgeStart;
  end;

  procedure IterateByPoint(const AEdge: PsgEdge; const AIndexPoint: Integer);
  var
    I: Integer;
  begin
    vVertex := GetVertexExByIndex(AIndexPoint);
    for I:= 0 to vVertex.Count - 1 do
    begin
      vEdgeAuxiliary := vVertex[I];
      if vEdge = vEdgeAuxiliary then
        Continue;
      if vEdgeAuxiliary^.IsUses = True then
      begin
        if IsEqualEdge(AEdge, vEdgeAuxiliary, FAccuracy) then
        begin
          UnionTwoEgesLoop(vEdgeAuxiliary, AEdge);
          vEdgeAuxiliary^.IsUses := False;
          AEdge^.IsUses := False;
          Break;
        end;
      end;
    end;
  end;

begin
  Result := False;

  for I := 0 to FEdges.Count - 1 do
  begin
    vEdge := FEdges[I];
    if vEdge^.IsUses = False then
      Continue;
    IterateByPoint(vEdge, vEdge^.Point1);
    if vEdge^.IsUses = False then
      Continue;
    IterateByPoint(vEdge, vEdge^.Point2);
  end;

  for I := 0 to FEdges.Count - 1 do
  begin
    vEdge := FEdges[I];
    if vEdge^.IsUses = False then
      Continue;
    vLoop := TsgLoop.Create(Self);
    FLoops.Add(vLoop);
    WorkLoop(vEdge, vLoop);
    Result := True;
  end;

end;

procedure TsgLoops.ClearEmptyLoops;
var
  I: Integer;
  vLoop: TsgLoop;
begin
  for I := FLoops.Count - 1 downto 0 do
  begin
    vLoop := FLoops[I];
    if vLoop.FEdges.Count = 0 then
    begin
      FLoops[I] := nil;
      vLoop.Free;
    end;
  end;
end;

end.

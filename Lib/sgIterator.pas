{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                TsgIteratorBase class                       }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgIterator;
{$INCLUDE SGDXF.inc}
interface

uses
  DXFConv, sgConsts, sgFunction;

const
  cnstContinueIterate = 1;

type
  TsgIteratorProc = class
  private
    FOwner: TObject;
    function GetIterateParams: PsgCADIterate;
    function GetConverter: TsgDXFConverter;
  public
    function BeginProc(Entity: TsgDXFEntity): Integer; virtual;
    function FinishProc(Entity: TsgDXFEntity): Integer; virtual;
    function GetPoint(const APoint: TFPoint): TFPoint;
    property IterateParams: PsgCADIterate read GetIterateParams;
    property Converter: TsgDXFConverter read GetConverter;
  end;

  TsgRotateImage = class(TsgIteratorProc)
  protected
    function DoImageEnt(const AImageEnt: TsgDXFImageEnt): Integer;
  public
    function BeginProc(Entity: TsgDXFEntity): Integer; override;
  end;

  TsgClosePolyline = class(TsgIteratorProc)
  protected
    function DoPolyline(const APoly: TsgDXFPolyline): Integer;
    function DoPolygon(const ACurvePoly: TsgCADCurvePolygon): Integer;
  public
    function BeginProc(Entity: TsgDXFEntity): Integer; override;
  end;

  TsgProcProcessLayout = function(const AIndex: Integer): Boolean of object;

  TsgIteratorBase = class
  private
    FIsUseIdentityMat: Boolean;
    FIsOnlyModelIterate: Boolean;
    FDrawParams: PsgCADIterate;
    FConverter: TsgDXFConverter;
    FIterateParams: TsgCADIterate;
    FIterateProc: TsgIteratorProc;
    FProc: TsgProcProcessLayout;
    FLayoutIndex: Integer;
    function GetIterateParams: PsgCADIterate;
    function IsProcessLayoutIndex(const AIndex: Integer): Boolean;
  protected
    procedure AfterIterate(const ACadImage: TObject); virtual;
    procedure BeforeIterate(const ACadImage: TObject); virtual;
    function BeginProc(Entity: TsgDXFEntity): Integer;
    function FinishProc(Entity: TsgDXFEntity): Integer;
    function GetViewMatrix: TFMatrix; virtual;
    procedure LayoutIterate(const AConverter, ALayout: TObject);
    property Converter: TsgDXFConverter read FConverter;
    property IterateParams: PsgCADIterate read GetIterateParams;
  public
    constructor Create(const AProc: TsgIteratorProc); virtual;
    destructor Destroy; override;
    function Iterate(const ACadImage: TObject): Boolean; overload;
    function Iterate(const ACadImage: TObject;
      const AIndexLayout: Integer): Boolean; overload;
    property IsUseIdentityMat: Boolean read FIsUseIdentityMat write FIsUseIdentityMat;
    property IsOnlyModelIterate: Boolean read FIsOnlyModelIterate write FIsOnlyModelIterate;
  end;

implementation

uses
  CADImage;

type
   TsgCADImageAccess = class(TsgCADImage);
   TsgDXFImageEntAccess = class(TsgDXFImageEnt);

{ TsgIteratorBase }

function TsgIteratorBase.BeginProc(Entity: TsgDXFEntity): Integer;
begin
  Result := cnstContinueIterate;
  if FIterateProc <> nil then
    Result := FIterateProc.BeginProc(Entity);
end;

constructor TsgIteratorBase.Create(const AProc: TsgIteratorProc);
begin
  inherited Create;
  FIsUseIdentityMat := False;
  FIsOnlyModelIterate := False;
  FIterateProc := AProc;
  if FIterateProc <> nil then
    FIterateProc.FOwner := Self;
end;

destructor TsgIteratorBase.Destroy;
begin
  inherited Destroy;
end;

function TsgIteratorBase.FinishProc(Entity: TsgDXFEntity): Integer;
begin
  Result := cnstContinueIterate;
  if FIterateProc <> nil then
    Result := FIterateProc.FinishProc(Entity);
end;

function TsgIteratorBase.GetIterateParams: PsgCADIterate;
begin
  Result := @FIterateParams;
end;

function TsgIteratorBase.GetViewMatrix: TFMatrix;
begin
  Result := cnstCrossYMat;
end;

function TsgIteratorBase.IsProcessLayoutIndex(const AIndex: Integer): Boolean;
begin
  Result := AIndex = FLayoutIndex
end;

function TsgIteratorBase.Iterate(const ACadImage: TObject;
  const AIndexLayout: Integer): Boolean;
begin
  FLayoutIndex := AIndexLayout;
  FProc := IsProcessLayoutIndex;
  try
    Result := Iterate(ACadImage);
  finally
    FProc := nil;
  end;
end;

function TsgIteratorBase.Iterate(const ACadImage: TObject): Boolean;
var
  vCADImage: TsgCADImageAccess absolute ACadImage;
  I: Integer;
begin
  Result := (ACadImage is TsgCADImage) and (FIterateProc <> nil);
  if Result then
  begin
    BeforeIterate(ACadImage);
    try
      for I := 0 to vCADImage.LayoutsCount - 1 do
      begin
        if Assigned(FProc) then
        begin
          if not FProc(I) then
            Continue;
        end
        else
          if FIsOnlyModelIterate and (not vCADImage.Layouts[I].IsModel) then
            Continue;
        FillChar(FIterateParams, SizeOf(FIterateParams), 0);
        if FIsUseIdentityMat then
          FIterateParams.Matrix := cnstIdentityMat
        else
          FIterateParams.Matrix := FMatXMat(vCADImage.GetRealImageMatrix, GetViewMatrix);
        FIterateParams.Additional := Integer(IsRotatedFMat(FIterateParams.Matrix));
        LayoutIterate(vCADImage.Converter, vCADImage.Layouts[I]);
      end;
    finally
      AfterIterate(ACadImage);
    end;
  end;
end;

procedure TsgIteratorBase.LayoutIterate(const AConverter, ALayout: TObject);
var
  vLauout: TsgDXFLayout absolute ALayout;
begin
  vLauout.Iterate(TsgDXFConverter(AConverter), BeginProc, FinishProc);
end;

procedure TsgIteratorBase.AfterIterate(const ACadImage: TObject);
var
  vCADImage: TsgCADImageAccess absolute ACadImage;
begin
  vCADImage.Converter.Params := FDrawParams;
  FConverter := nil;
end;

procedure TsgIteratorBase.BeforeIterate(const ACadImage: TObject);
var
  vCADImage: TsgCADImageAccess absolute ACadImage;
begin
  FDrawParams := vCADImage.Converter.Params;
  FConverter := vCADImage.Converter;
  vCADImage.Converter.Params := @FIterateParams;
end;

{ TsgIteratorProc }

function TsgIteratorProc.BeginProc(Entity: TsgDXFEntity): Integer;
begin
  Result := cnstContinueIterate;
end;

function TsgIteratorProc.FinishProc(Entity: TsgDXFEntity): Integer;
begin
  Result := cnstContinueIterate;
end;

function TsgIteratorProc.GetConverter: TsgDXFConverter;
begin
  if FOwner <> nil then
    Result := TsgIteratorBase(FOwner).Converter
  else
    Result := nil;
end;

function TsgIteratorProc.GetIterateParams: PsgCADIterate;
begin
  if FOwner <> nil then
    Result := TsgIteratorBase(FOwner).IterateParams
  else
    Result := nil;
end;

function TsgIteratorProc.GetPoint(const APoint: TFPoint): TFPoint;
begin
  Result := FPointXMat(APoint, IterateParams^.Matrix);
end;

{ TsgRotateImage }

function TsgRotateImage.BeginProc(Entity: TsgDXFEntity): Integer;
begin
  if Entity.EntType = ceImageEnt then
    Result := DoImageEnt(TsgDXFImageEnt(Entity))
  else
    Result := inherited BeginProc(Entity);
end;

function TsgRotateImage.DoImageEnt(const AImageEnt: TsgDXFImageEnt): Integer;
var
  vAngleX, vAngleY: Double;
begin
  Result := cnstContinueIterate;
  vAngleX := GetAngleByPoints(GetPoint(AImageEnt.Point),
    GetPoint(AImageEnt.Point1), False, fDoubleResolution ,1, -1);
  vAngleY :=  GetAngleByPoints(GetPoint(AImageEnt.Point),
    GetPoint(AImageEnt.Point2), False, fDoubleResolution, 1, -1);
  if not IsDrawGDI(vAngleX, vAngleY) then
  begin
    if IsEqual((Round(vAngleX) div 90) * 90, vAngleX) then
      TsgDXFImageEntAccess(AImageEnt).DoRotate(360-vAngleX, Converter);
  end;
end;

{ TsgClosePolyline }

function TsgClosePolyline.BeginProc(Entity: TsgDXFEntity): Integer;
begin
  case Entity.EntType of
    ceLWPolyline, cePolyline: Result := DoPolyline(TsgDXFPolyline(Entity));
    ceCurvePolygon: Result := DoPolygon(TsgCADCurvePolygon(Entity));
  else
    Result := inherited BeginProc(Entity);
  end;
end;

function TsgClosePolyline.DoPolygon(
  const ACurvePoly: TsgCADCurvePolygon): Integer;
//var
//  I,J: Integer;
//  vBoundary: Tsg2DBoundaryList;
//  vCurve: Tsg2DCurve;
//  v2dPoly: Tsg2DPolyline;
begin
  Result := cnstContinueIterate;
//  for I := 0 to ACurvePoly.BoundaryDataCount - 1 do
//  begin
//    vBoundary := ACurvePoly.BoundaryData[I];
//    for J := 0 to vBoundary.Count - 1 do
//    begin
//      vCurve := vBoundary.Items[J];
//      case vCurve.EdgeType of
//        cnst2DCurvePoly:
//        begin
//          v2dPoly := Tsg2DPolyline(vCurve);
//        end;
//      end;
//    end;
//  end;
end;

function TsgClosePolyline.DoPolyline(const APoly: TsgDXFPolyline): Integer;
var
  vPoint1, vPoint2: TFPoint;
  vVertex: TsgDXFEntity;
  vIndex: Integer;
begin
  Result := cnstContinueIterate;
  case APoly.Count of
    1:  AddVertexInPolyline(APoly, TsgDXFVertex(APoly.Entities[0]).Point);
    2:  begin end;
  else
    vIndex := APoly.Count - 1;
    vPoint1 := TsgDXFVertex(APoly.Entities[0]).Point;
    vPoint2 := TsgDXFVertex(APoly.Entities[vIndex]).Point;
    if IsEqualFPoints(vPoint1, vPoint2) then
    begin
      APoly.Closed := True;
      vVertex := APoly.Entities[vIndex];
      APoly.DeleteEntity(vIndex);
      vVertex.Free;
      GetConverter.Loads(APoly);
    end;
  end;
end;

end.

{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                    Export CAD to GCode                     }
{                                                            }
{     Copyright (c) 2002 - 2024 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit CADtoGCode;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPImage,
{$ENDIF}
  GCode, SysUtils, Classes, CADImage, DXFConv, sgConsts, sgFunction,
  sgLists, Math, CADExport, sgXMLParser, Graphics
{$IFDEF SG_TransformAnalogy}
  , sgTransformAnalogy
{$ENDIF}
{$IFNDEF SG_NO_USE_KERNEL3D}
 , sgMDefinitions, sgMFunctions, sgModeller
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF};

type

  TsgMinimalAngleData = record
    Angle: Double;
    Point: TFPoint;
    PointIndex: Integer;
  end;

  TsgCommonDataPoint = record
    Angle: Double;
    Point: TFPoint;
    Poly: TsgCADBasePolyline;
    Poly1: TsgCADBasePolyline;
    IndexCommnonPtPoly: Integer;
    IndexCommnonPtPoly1: Integer;
  end;

  TsgCADBaseEntTypes = (ctPolys, ctArcs, ctPolyArc, ctArcPoly);

  TsgCADtoGCode = class(TsgSimpleExportCustom)
  private
    FAbsoluteCoordIJ: Boolean;
    FAddLabelOfProgram: Boolean;
    FAddNumbering: Boolean;
    FArcToLines: Boolean;
    FBreakGenerate: Boolean;
    FCADBox: TFRect;
    FCADUnits: TsgGUnits;
    FCodeOptimize: Boolean;
    FPositioningSystemID: Integer; //0 - G90 absolute, 1 - G91 incremental
    FContourList: TList;
    FConvOnlyVisLay: Boolean;
    FCurContour: TsgGContour;
    FCurPoly: TsgDXFPolyline;
    FCurrentNumber: Integer;
    FCurrentVertex: TsgDXFVertex;
    FDelay: Integer;
    FDemoComment: string;
    FDepartureOnZ: Double;
    FEntityList: TList;
    FDepthOnZ: Double;
    FDepthPass: Double;
    FFeedOnXY: Integer;
    FFeedOnXZ: Double;
    FFeedOnZ: Integer;
    FFirstMove: Boolean;
    FForceLaserOffGRBL: Boolean;
    FGArcList: TList;
    FGCode: TStringList;
    FGCodeCount: Integer;
    FGCodeResolution: Double;
    FHacoAddCommand: string;
    FLabelOfProgram: string;
    FLaserOffCommand: string;
    FLaserOnCommand: string;
    FLaserPower: Integer;
    FLayoutName: string;
    FMachineType: TsgGCodeMachineTypeId;
    FMachineUnits: TsgGUnits;
    FMaxIndex: Integer;
    FMergePoints: Boolean;
    FMergePointsRadius: Double;
    FNumbOfPasses: Integer;
    FOnArc: Boolean;
    FOldNumberOfPartsInCircle: Integer;
    FOldNumberOfPartsInSpline: Integer;
    FRegenerateArcs: Boolean;
    FPassesCounter: Integer;
    FPassesDirection: TsgGPassesDirection;
    FPocketList: TList;
    FVertexIndex: Integer;
    FPointProgress: Integer;
    FPrecisionDigits: Byte;
    FPrecisionFactor: Byte;
    FPrevAbsoluteZ: Double;
    FPrevPoint: TFPoint;
    FPrevPointInit: Boolean;
    FPrevCommand: TsgGCommandId;
    FPrevFeed: Double;
    FPrevToolNumb: Integer;
    FRetractTool: Boolean;
    FReturnToZeroPoint: Boolean;
    FSelectedContours: TList;
    FStartNumber: Integer;
    FStartFromZeroPos: Boolean;
    FStepOfNumbering: Byte;
    FShowContourName: Boolean;
    FShowLayerName: Boolean;
    FShowPercent: Boolean;
    FSpindleSpeed: Integer;
    FStraightenContour: Boolean;
//Dispenser
    FTurnOffPumpBefore: Boolean;
    FTurnOffPumpBeforeValue: Double;
    FDispenserOffCommand: string;
    FDispenserOnCommand: string;
    FPumpOffCommand: string;
    FPumpOnCommand: string;
//    FTextSelect: Boolean;
    FTools: TsgStringList;
    FTrailingZeros: Boolean;
    FUnitMatrix: TFMatrix;
    FUseLaserPower: Boolean;
    FWorkpieceZeroPoint: TFPoint;
    FWorkpieceZeroPointID: TsgGCodeZeroPointID;
    FZeroPointOffset: TFPoint;
    FEditingBlock: TsgDXFBlock;
    FEditingLayer: TsgDXFLayer;
    FEntityByLayer: TsgDXFObjectEntity; //for auto free
    function AddEnding(AValue: Double): string;
    function AddVertexWithOptimize(const APoly: TsgDXFPolyline; const APoint: TFPoint;
      APtInd: Integer; ABulge: Double = 0): Integer;
    procedure AddToGCode(S: string);
    procedure CalcPolyPointsXMat(const APts, ANewPts: TFPointList;
      AClose: boolean = False; AIsZThick: Boolean = False);
    procedure CalcNewVertexesXMat(const ASorcePoly, ADestPoly: TsgDXFPolyline); overload;
    procedure CalcNewVertexesXMat(const APoints: TFPointList; ADestPoly: TsgDXFPolyline); overload;
    function CheckOptimize(ACommand: TsgGCommandId): string;
    function FindCommonDataByEdgePolyPoint(const APoly, APoly1: TsgCADBasePolyline; var ACommonData: TsgCommonDataPoint): Boolean;
    procedure ExcludeCommonVertexes;
    procedure ExcludeDublicatesEntities;
    procedure WriteCurContourComments;
    procedure GenerateContoursFromPolylines(const APolylineList: TList;
      AStep: Integer = 0; IsEqudistant: Boolean = False);
    procedure GetConvertbleContours(const AContours: TList);
    function GetPolylineFromPolyPoints(const AEntity: TsgCADBasePolyline;
      ALoad: Boolean = True; AddInBlock: Boolean = True): TsgDXFPolyline;
    function GetPolylineFromCircle(const AEntity: TsgDXFEntity): TsgGCodePolyline;
    function GetPolylineFromVertexes(const AEntity: TsgDXFPolyline;
      ALoad: Boolean = True; AddInBlock: Boolean = True): TsgGCodePolyline;
    function GetPolylineFromPoints(const APoints: TFPointList): TsgGCodePolyline;
    procedure GetPolylineFromLine(const AEntity: TsgDXFLine);
    procedure GetPolylinesFromText(const AText: TsgDXFText);
    procedure GetPolylinesFromHatch(const AEntity: TsgDXFEntity);
    procedure GetPolylineFromPoint(const AEntity: TsgDXFEntity);
    procedure GetPolylineFromModEntity(const AEnt: TsgModEntity; var AParams: TsgBrepModIterateParams);
    procedure GetPolylineFrom3dSolid(const AEntity: TsgDXFEntity);
    function GetTool: string;
    function MakeContourFromPolyline(const APolyline: TsgDXFPolyline; AName, ALayerName: string): TsgGContour;
//    function IsMultiSelect: Boolean;
    function MakeGArcByParam(AArcR: TsgArcR): TsgDXFGArc;
    function OptimizePolyline(var APolyline: TsgDXFPolyline): Boolean;
    procedure InitWorkpieceZepoPoint;
    procedure LoadPolyEntities(const APolylines: TList; const ADestList: TList);
    procedure LoadNewEntity(const AEntity: TsgDXFEntity; const ADestList: TList;
      AddInBlock: Boolean = True);
    procedure MakeContours;
    procedure MakeNewPolyEntities(const AEntity: TsgDXFEntity);
    procedure OptimizePolyPointsByResolution(const APolyline: TsgDXFPolyline);
    procedure OptimizeVertexesByResolution(const APolyline: TsgDXFPolyline);
    procedure StartGCode;
    procedure SetDrawingUnits(AValue: TsgGUnits);
    procedure SetMachineUnits(AValue: TsgGUnits);
    procedure SetUnitMatrix;
    procedure StartContour(APt: TFPoint);
    procedure EndGCode;
    procedure EndContour;
    procedure SetMachineType(const Value: TsgGCodeMachineTypeID);
    procedure SetEditingBlock(const Value: TsgDXFBlock);
    function GetStraightenContour: Boolean;
    procedure SetDepthAndPasses(const AParams: TsgExportParams);
  protected
    procedure BeforeExport(const S: TStream); override;
    procedure Convert;
    procedure InitializeVersionData(const AIsTrial: Boolean; const AMessage: string;
      AppId: string = ''); override;
    procedure ProgressGenerateContours(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    procedure ReadLeads(AContour: TsgGContour; var APolyline: TsgDXFPolyline);
    procedure SaveToStreamCustom(S: TStream); override;
    procedure SelectGCodeCommand(var AIndex: Integer);
    procedure ResetImageExternal;
    procedure SetPrecision(const ADigits, AFactor: Byte);
    procedure SetTools(const AValue: string);
    procedure SetDistanseOffset(AStatus: Boolean; AOffset: Double);
    procedure ReinitStartingPoints; //for Dispenser
    procedure WriteGCodeCommand(const AVertex: TsgDXFVertex; Operation: TsgOperationId);
    function ExportProc(AEntity: TsgDXFEntity): Integer;
    function FinishExportProc(AEntity: TsgDXFEntity): Integer;
    function IsClockWise(const APoly: TsgDXFPolyline): Boolean;
    function CheckFeedCommand(AValue: Double): string;
    function GetAdjGCommand(ACommand: TsgGCommandId; AValue: Integer): string; overload;
    function GetAdjGCommand(ACommand: TsgGCommandId; AValue: Double): string; overload;
    function GetAdjGCommand(ACommand: string; AValue: Double): string; overload;
    function GetAdjGCommand(ACommand: TsgGCommandId; AAdjCommand: string): string; overload;
    function GetPosGCommand(ACommand: TsgGCommandId; APoint: TFPoint; const ACenter: PFPoint = nil): string; overload;
    function GetPosGCommand(ACommand: TsgGCommandId; APt: TFPoint; AIgnoreWpZeroPt: Boolean = False;
      ASkipOptimize: Boolean = False): string; overload;
    function GetConverter: TsgDXFConverter; override;
    function GetCoordinateGCode(const APoint: TFPoint; const ACenter: PFPoint = nil;
      AIgnoreWpZeroPt: Boolean = False; ASkipOptimize: Boolean = False): string;
    function GetGcodeParams: TsgExportParams;
    function GetTools: string;
//    function IsPointsOnGArc(const AStartPoint, ANextPoint: TFPoint; AChecked: Boolean = True): TsgDXFGArc;
    function MakeGArcOfBulge(AStartPoint, AEndPoint: TFPoint; ABulgeValue: Double): TsgDXFGArc;
    function OpenEntity(AEntity: TsgDXFEntity; AClosedEntList: TList): Boolean;
    function SortContours: Boolean;
    function GeneratePocketComponents(const AContour: TObject; APocket : TsgGPocket): Boolean;
    procedure InternalRadiusCompGRBL(Contour: TObject; ARadiusComp: TsgGRadiusComp);
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    procedure GetContours;
    procedure DeletePocket(const AContour: TsgGContour);
    function MakePocket(const AContour: TsgGContour; const AParams: TsgPocketParams): Boolean;
    procedure AddPolylines(const AEnts: TsgList); //Transform
    function GetContoursByEntity(const AEntity: TsgDXFEntity; const AContours: TList): Boolean;
    property BreakGenerate: Boolean read FBreakGenerate write FBreakGenerate;
    property CADBox: TFRect read FCADBox;
    property CADImage;
    property CNCGCode: TStringList read FGCode;
    property Contours: TList read FContourList;
    property EditingBlock: TsgDXFBlock read FEditingBlock write SetEditingBlock;
    property DrawingUnits: TsgGUnits read FCADUnits write SetDrawingUnits;
    property MachineUnits: TsgGUnits read FMachineUnits write SetMachineUnits;
    property MachineType: TsgGCodeMachineTypeID read FMachineType write SetMachineType;
    property StraightenContour: Boolean read GetStraightenContour;
    property UnitMatrix: TFMatrix read FUnitMatrix;
    property WorkpieceZeroPoint: TFPoint read FWorkpieceZeroPoint;
    property WorkpieceZeroPointID: TsgGCodeZeroPointID read FWorkpieceZeroPointID write FWorkpieceZeroPointID;
    property CodeOptimize: Boolean read FCodeOptimize write FCodeOptimize;
    property GcodeParams: TsgExportParams read GetGcodeParams write ApplyParams;
    property Tools: string read GetTools write SetTools;
    property GCodeResolution: Double read FGCodeResolution;
  end;

  TsgGCodeManager = class
  private
    FGCodeExport: TsgCADtoGCode;
  public
    constructor Create(const AImage: TsgCADImage);
    destructor Destroy; override;
    procedure ResetImageExternal;
    procedure Convert;
    procedure CreateWorkpieceZeroPoint(const ABlock: TsgDXFBlock);
    property GCodeExport: TsgCADtoGCode read FGCodeExport write FGCodeExport;
  end;

//function IsCircleFromPointCrossPolyline(const APoint: TFPoint; ARadius: Double; const APolylline: TsgDXFPolyline): Boolean;

implementation

var
  cnstGCommands: TsgGCommands;

const
  cnstStartPercent = 0;
  cnstIteratePercent = 10;
  cnstDublicatePercent = 20;
  cnstCommonPointsPercent = 70;
  cnstEndPercent = 100;

type
  TsgCADImageAccess = class(TsgCADImage);
  TsgGContourAccess = class(TsgGContour);
  TsgGCodePolylineAccess = class(TsgGCodePolyline);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TsgDXFVetexAccess = class(TsgDXFVertex);
  TsgCADHelixAccess = class(TsgCADHelix);
  TsgDXFEintityAccess = class(TsgDXFEntity);
  TsgDXFPenLineAccess = class(TsgDXFPenLine);
  TsgGPocketAccess = class(TsgGPocket);

{ TsgCADtoGCode }

procedure ConvertPolyToGPolyInList(const AList: TsgList; AIndex: Integer);
var
  vPoly: TsgDXFPolyline;
  vGPoly: TsgGCodePolyline;
begin
  vPoly := AList[AIndex];
  vGPoly := TsgGCodePolyline.Create;
  vGPoly.AssignEntity(vPoly);
  AList.Insert(AIndex, vGPoly);
  AList.Delete(AIndex + 1);
  vPoly.Free;
end;

function IsClosed(const APoly: TsgCADBasePolyline; AResolution: Double = fDoubleResolution): Boolean;
var
  vStPt, vEndPt: TFPoint;
begin
  Result := False;
  vStPt := APoly.PolyPoints.First;
  vEndPt := APoly.PolyPoints.Last;
  if IsEqualFPoints(vStPt, vEndPt, AResolution) then
    Result := True;
end;

function IsEqualVertexPoint(const AVer, AVer1: TsgDXFVertex; AResolution: Double = fDoubleResolution): Boolean;
begin
  Result := IsEqualFPoints(AVer.Point, AVer1.Point, AResolution);
end;

function IsEqualVertex(const AVer, AVer1: TsgDXFVertex; AResolution: Double = fDoubleResolution): Boolean;
begin
  Result := IsEqualVertexPoint(AVer, AVer1, AResolution) and (AVer.Bulge = AVer1.Bulge);
end;

constructor TsgCADtoGCode.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  if Assigned(CADImage) then
  begin
    FOldNumberOfPartsInCircle := CADImage.Converter.NumberOfPartsInCircle;
    FOldNumberOfPartsInSpline := CADImage.Converter.NumberOfPartsInSpline;
    FRegenerateArcs := CADImage.Converter.RegenerateArcs;
    //if slow then call internal methods and "ReLoadEntities(3)"
    CADImage.RegenerateArcs := False;
    CADImage.Converter.NumberOfPartsInCircle := cnstNumberOfPartsInCircleGcode;
    CADImage.Converter.NumberOfPartsInSpline := cnstNumberOfPartsInSplineGcode;
  end;
  FLayoutName := CADImage.CurrentLayout.Name;
  FParams := @FInternalParams;
  FGCode := TStringList.Create;
  FContourList := TList.Create;
  FOnArc := False;
  FPocketList := TList.Create;
  ACADImage.GetExtents;
  FCADBox := ACADImage.Extents;
  FLabelOfProgram := '';
  FGArcList := TList.Create;
  FUnitMatrix := cnstIdentityMat;
  FTools := TsgStringList.Create;
  FSelectedContours := TList.Create;
  FEntityList := TList.Create;
  FEditingLayer := TsgDXFLayer.Create;
  FEditingLayer.Name := 'GcodeLayer';
  FEntityByLayer := TsgDXFObjectEntity.Create;
  FEntityByLayer.Layer := FEditingLayer;
  FFirstMove := True;
  ApplyParams(DefExportParams);
end;

procedure TsgCADtoGCode.DeletePocket(const AContour: TsgGContour);
var
  I: Integer;
  vPocket: TsgGPocket;
  vContour: TsgGContour;
begin
  if Assigned(AContour) and (AContour.IsFromPocket) then
  begin
    vPocket := TsgGContourAccess(AContour).Pocket;
    vPocket.SourceContour.HavePocketContour := False;
    FPocketList.Delete(FPocketList.IndexOf(vPocket));
    for I := 0 to vPocket.Contours.Count - 1 do
    begin
      vContour := vPocket.Contours[I];
      EditingBlock.RemoveEntity(vContour.GPolyline);
      vContour.GPolyline.Free;
      FContourList.Delete(FContourList.IndexOf(vContour));
      vContour.Free;
    end;
    vPocket.Free;
  end;
end;

destructor TsgCADtoGCode.Destroy;
begin
  FreeAndNil(FEntityByLayer);
  FGCode.Free;
  FreeList(FContourList);
  FreeList(FGArcList);
  FreeList(FPocketList);
  FTools.Free;
  FSelectedContours.Free;
  FEntityList.Free;
  ResetImageExternal;
  inherited Destroy;
end;

function TsgCADtoGCode.GetConverter: TsgDXFConverter;
begin
  Result := CADImage.Converter;
end;

procedure TsgCADtoGCode.InitializeVersionData(const AIsTrial: Boolean;
  const AMessage: string; AppId: string);
begin
  inherited InitializeVersionData(AIsTrial, AMessage, AppId);
  if AIsTrial then
  begin
    //FGCodeCount := 50;
    FDemoComment := '(' + AMessage + ' )';
  end;
end;

procedure FPointListToF2DPointList(const AFPointList: TFPointList; const AF2DPointList: TF2DPointList);
var
  I: Integer;
begin
  AF2DPointList.Clear(True);
  for I := 0 to AFPointList.Count - 1 do
    AF2DPointList.Add(F3DPointTo2D(AFPointList[I]));
end;

function TsgCADtoGCode.SortContours: Boolean;
var
  I, vPointIndex: Integer;
  vCurPointList, vNextPointList: TFPointList;
  vStartPoint, vEndPoint: TF2DPoint;
  vDistance, vPrevDistance: Double;
begin
  if FContourList.Count < 3 then
    Result := False
  else
  begin
    vPrevDistance := 0;
    for vPointIndex := 0 to FContourList.Count - 1 do
    begin
      vCurPointList := TsgGContour(FContourList[vPointIndex]).GPolyline.PolyPoints;
      vEndPoint := vCurPointList.Last.Point2D;
      for I := vPointIndex + 1 to FContourList.Count - 1 do
      begin
        vNextPointList := TsgGContour(FContourList.List[I]).GPolyline.PolyPoints;
        vStartPoint := vNextPointList.First.Point2D;
        vDistance := sgRoundTo(DistanceF2DPoint(vEndPoint, vStartPoint), -FPrecisionDigits, FPrecisionFactor);
        if (I = vPointIndex + 1) then
          vPrevDistance := vDistance + 1;
        if (vDistance < vPrevDistance) and (vDistance <> 0) then
        begin
          FContourList.Exchange(I, vPointIndex + 1);
          vPrevDistance := vDistance;
        end;
      end;
    end;
    Result := True;
  end;
end;

function TsgCADtoGCode.GetAdjGCommand(ACommand: TsgGCommandId; AAdjCommand: string): string;
var
  vCommand: string;
begin
  vCommand := CheckOptimize(ACommand);
  Result := vCommand + AAdjCommand;
  FPrevCommand := ACommand;
end;

function TsgCADtoGCode.GetAdjGCommand(ACommand: TsgGCommandId; AValue: Integer): string;
var
  vAdjCom, vValue: string;
begin
  Result := '';
  vAdjCom := cnstGCommands[ACommand];
  vValue := IntToStr(AValue);
  Result := vAdjCom + vValue;
end;

function TsgCADtoGCode.CheckFeedCommand(AValue: Double): string;
var
  vAdjCom, vValue: string;
begin
  Result := '';
  if FPrevFeed = AValue then
    Exit;
  vAdjCom := cnstGCommands[gcFeed];
  vValue := FloatToString(AValue);
  Result := vAdjCom + vValue;
  FPrevFeed := AValue;
end;

function TsgCADtoGCode.AddEnding(AValue: Double): string;
begin
  Result := FloatToStr(sgRoundTo(AValue, -FPrecisionDigits, FPrecisionFactor));
  if FTrailingZeros then
    Result := FloatToStrF(sgRoundTo(AValue, -FPrecisionDigits, FPrecisionFactor), ffFixed, 16, FPrecisionDigits);
  if Pos('.', Result) = 0 then
    Result := Result + '. '
  else
    Result := Result + ' ';
end;

function TsgCADtoGCode.GetAdjGCommand(ACommand: TsgGCommandId; AValue: Double): string;
var
  vCommand, vValue: string;
begin
  vCommand := cnstGCommands[ACommand];
  if (FPositioningSystemID = 1) and (ACommand = gcZ) then
  begin
    vValue := AddEnding(AValue - FPrevAbsoluteZ);
    FPrevAbsoluteZ := AValue;
  end
  else
  vValue := AddEnding(AValue);
  Result := vCommand + vValue;
end;

function TsgCADtoGCode.GetAdjGCommand(ACommand: string; AValue: Double): string;
var
  vValue: string;
begin
  vValue := FloatToString(AValue);
  Result := ACommand + ' ' + vValue;
end;

function TsgCADtoGCode.GetCoordinateGCode(const APoint: TFPoint; const ACenter: PFPoint = nil;
  AIgnoreWpZeroPt: Boolean = False; ASkipOptimize: Boolean = False): string;

  function CheckSkipOptimizeCoordinateValue(AValue, APrevValue: Double; const ACenter: PFPoint = nil): Boolean;
  var
    vSkipOptimizeGlobal, vSkipOptimizeGrbl, vSkipOptimizeIsEqual: Boolean;
  begin
    vSkipOptimizeGlobal := not FPrevPointInit or not FCodeOptimize;
    vSkipOptimizeGrbl := (MachineType in [mtGRBLRouter, mtGRBLLaser]) and (ACenter <> nil);
    vSkipOptimizeIsEqual := not IsEqual(AValue, APrevValue, -FGCodeResolution);
    Result := vSkipOptimizeGlobal or vSkipOptimizeGrbl or vSkipOptimizeIsEqual;
  end;

  function GetRealAxis(AAxes: TsgAxes): TsgAxes;
  begin
    Result := AAxes;
    if MachineType = mtLathe then
      case AAxes of
        axisX: Result := axisY;
        axisY: Result := axisZ;
        axisZ: Result := axisX;
      end;
  end;

  function GetCordByAxis(AAxes: TsgAxes; const APt, APrevPt, AWpZeroPt: PFPoint;
    AIgnoreZeroPT: Boolean; var ANeedInitPrevPt: Boolean): string;
  var
    vValueKoef, vWpZeroPtByAxes: Double;
    vRealAxis: TsgAxes;
    vCheck: Boolean;
    vValueIndex: Byte;
  begin
    vValueKoef := 1;
    vRealAxis := GetRealAxis(AAxes);
    if (MachineType = mtLathe) and (AAxes = axisX) then
      vValueKoef := 2;
    vValueIndex := Integer(vRealAxis);
    if (FFirstMove and (FPositioningSystemID = 1)) or (FPositioningSystemID = 0) then
      vWpZeroPtByAxes := AWpZeroPt.V[vValueIndex]
    else
      vWpZeroPtByAxes := 0;
    vCheck := False;
    if not AIgnoreZeroPT then
    begin
      vCheck := CheckSkipOptimizeCoordinateValue(APt.V[vValueIndex], APrevPt.V[vValueIndex], ACenter);
      if vCheck or ASkipOptimize then
      begin
        if FPositioningSystemID = 0 then
          Result := cnstAxis4Names[Integer(AAxes)] + AddEnding(APt.V[vValueIndex] * vValueKoef + vWpZeroPtByAxes)
        else
          Result := cnstAxis4Names[Integer(AAxes)] + AddEnding(APt.V[vValueIndex] * vValueKoef
          - APrevPt.V[vValueIndex] * vValueKoef + vWpZeroPtByAxes);
      end;
    end
    else
      Result := cnstAxis4Names[Integer(AAxes)] + AddEnding(APt.V[vValueIndex] * vValueKoef);
    if vCheck then
    begin
      FPrevPoint.V[vValueIndex] := APoint.V[vValueIndex];
      ANeedInitPrevPt := True;
    end;
  end;

var
  vNeedInitPrevPoint: Boolean;
  vPrevPoint, vPoint, vWorkpieceZeroPt: TFPoint;
begin
  Result := '';
  vPoint := RoundFPoint(FPointXMat(APoint, FUnitMatrix), FPrecisionDigits, FPrecisionFactor);
  if FPrevPointInit then
    vPrevPoint := RoundFPoint(FPointXMat(FPrevPoint, FUnitMatrix), FPrecisionDigits, FPrecisionFactor)
  else
    vPrevPoint := cnstFPointZero;
  vWorkpieceZeroPt := FPointXMat(FWorkpieceZeroPoint, FUnitMatrix);
  if AIgnoreWpZeroPt then
  begin
//    Result := 'X' + AddEnding(APoint.X) + 'Y' + AddEnding(APoint.Y)
    if MachineType <> mtLathe then
      Result := GetCordByAxis(axisX, @APoint, nil, nil, AIgnoreWpZeroPt, vNeedInitPrevPoint) +
        GetCordByAxis(axisY, @APoint, nil, nil, AIgnoreWpZeroPt, vNeedInitPrevPoint)
    else
      Result := GetCordByAxis(axisX, @APoint, nil, nil, AIgnoreWpZeroPt, vNeedInitPrevPoint) +
        GetCordByAxis(axisZ, @APoint, nil, nil,AIgnoreWpZeroPt, vNeedInitPrevPoint);
  end
  else
  begin
    Result := GetCordByAxis(axisX, @vPoint, @vPrevPoint, @vWorkpieceZeroPt, AIgnoreWpZeroPt, vNeedInitPrevPoint);
    if MachineType <> mtLathe then
      Result := Result + GetCordByAxis(axisY, @vPoint, @vPrevPoint, @vWorkpieceZeroPt, AIgnoreWpZeroPt, vNeedInitPrevPoint)
    else
      Result := Result + GetCordByAxis(axisZ, @vPoint, @vPrevPoint, @vWorkpieceZeroPt, AIgnoreWpZeroPt, vNeedInitPrevPoint);
    FPrevPointInit := vNeedInitPrevPoint;
    if Assigned(ACenter) then
    begin
      ACenter^ := FPointXMat(ACenter^, FUnitMatrix);
      if FAbsoluteCoordIJ or (ACenter^.V[Integer(GetRealAxis(axisX))] <> 0) then
        Result := Result + 'I' + AddEnding(ACenter^.V[Integer(GetRealAxis(axisX))]);
      if MachineType <> mtLathe then
      begin
        if FAbsoluteCoordIJ or  (ACenter^.V[Integer(GetRealAxis(axisY))] <> 0) then
          Result := Result + 'J' + AddEnding(ACenter^.V[Integer(GetRealAxis(axisY))])
      end
      else
      begin
        if FAbsoluteCoordIJ or  (ACenter^.V[Integer(GetRealAxis(axisZ))] <> 0) then
          Result := Result + 'K' + AddEnding(ACenter^.V[Integer(GetRealAxis(axisZ))])
      end;
    end;
  end;
end;

function TsgCADtoGCode.CheckOptimize(ACommand: TsgGCommandId): string;
var
  vCommand: string;
begin
  vCommand := cnstGCommands[ACommand];
  if (FPrevCommand = ACommand) and FCodeOptimize and (FMachineType <> mtHacoKompakt) then
    Result := ''
  else
    Result := vCommand + ' ';
end;

function TsgCADtoGCode.GetPosGCommand(ACommand: TsgGCommandId; APt: TFPoint;
  AIgnoreWpZeroPt: Boolean = False; ASkipOptimize: Boolean = False): string;

  function GetRadiusCorrector: string;
  var
    vRadiusCorNumb: Integer;
  begin
    if ACommand = gcG40 then
      Exit;
    vRadiusCorNumb := FCurContour.ToolParams.Number;
    Result := 'D' + IntToStr(vRadiusCorNumb) + ' ';
  end;

var
  vCoord: string;
begin
  Result := CheckOptimize(ACommand);
  vCoord := GetCoordinateGCode(APt, nil, AIgnoreWpZeroPt, ASkipOptimize);
  if vCoord = '' then
    Result := ''
  else
  begin
    Result := Result + vCoord;
//    FPrevCommand := ACommand;
  end;
  if (ACommand = gcG41) or (ACommand = gcG42) or (ACommand = gcG40) then
  begin
    Insert(cnstGCommands[gcG1] + ' ', Result, 1);
    Result := Result + GetRadiusCorrector;
  end;
    FPrevCommand := ACommand;
end;

function TsgCADtoGCode.GetStraightenContour: Boolean;
begin
  Result := (MachineType = mtDispenser) and FStraightenContour;
end;

function TsgCADtoGCode.GetPosGCommand(ACommand: TsgGCommandId; APoint: TFPoint;
  const ACenter: PFPoint = nil): string;
var
  vArcGCodeCent: TFPoint;
begin
  Result := '';
  case ACommand of
    gcG0, gcG1: Result := GetPosGCommand(ACommand, APoint, False);
    gcG2, gcG3:
      begin
        Result := CheckOptimize(ACommand);
        if FAbsoluteCoordIJ then
          vArcGCodeCent := ACenter^
        else
          vArcGCodeCent := SubFPoint(ACenter^, FPrevPoint);
        Result := Result + GetCoordinateGCode(APoint, @vArcGCodeCent);
        FPrevCommand := ACommand;
      end;
    gcG41, gcG42: Result := GetPosGCommand(ACommand, APoint, False);
    gcG40: Result := GetPosGCommand(ACommand, APoint, False);
  end;
  if ACommand <> gcG0 then
  begin
    if FMachineType in MillMachines then
      Result := Result + (CheckFeedCommand(FFeedOnXY))
    else
      if FMachineType = mtLathe then
        Result := Result + (CheckFeedCommand(FFeedOnXZ))
  end;
  if FHacoAddCommand <> '' then
  begin
    Result := Result + FHacoAddCommand;
    FHacoAddCommand := '';
  end;
end;

function TsgCADtoGCode.GetTool: string;
var
  vParams: TsgGToolParams;
begin
  vParams := FCurContour.ToolParams;
  if vParams.Number <> FPrevToolNumb then
  begin
    Result := cnstGCommands[gcToolNumb] + IntToStr(vParams.Number) + ' ' + cnstGCommands[gcM6];
    FPrevToolNumb := vParams.Number;
  end;
end;

function TsgCADtoGCode.GetTools: string;
begin
  Result := FTools.{$IFDEF SGDL_7}DelimitedText{$ELSE}Text{$ENDIF};
end;

procedure TsgCADtoGCode.WriteCurContourComments;
var
  vCommnetFlags: Byte;
  vComment: string;
begin
  vComment := '';
  vCommnetFlags := Ord(FShowLayerName) or (Ord(FShowContourName) shl 1);
  case vCommnetFlags of
    0:
      begin
//        Result := '';
      end;
    1:
      begin
        vComment := 'Layer: ' + FCurContour.LayerName;
      end;
    2:
      begin
        vComment := FCurContour.Name;
      end;
    3:
      begin
        vComment := FCurContour.Name + ')' + #13#10 + '(Layer: ' + FCurContour.LayerName;
      end;
  end;
  if vComment <> '' then
  begin
    vComment := '(' + vComment + ')';
    FGCode.Add(vComment);
  end;
end;

function TsgCADtoGCode.GetGcodeParams: TsgExportParams;
begin
  Result.MachineTypeID := Byte(FMachineType);
  Result.PassesDirectionID := Byte(FPassesDirection);
  Result.PrecisionDigits := FPrecisionDigits;
  Result.PrecisionFactor := FPrecisionFactor;
  Result.MachineUnitsID := Byte(MachineUnits);
  Result.DrawingUnitsID := Byte(DrawingUnits);
  Result.AddLabelOfProgam := FAddLabelOfProgram;
  Result.LabelOfProgram := FLabelOfProgram;
  Result.AddNumbering := FAddNumbering;
  Result.ShowContourName := FShowContourName;
  Result.ShowLayerName := FShowLayerName;
  Result.ShowPercent := FShowPercent;
  Result.StartNumber := FStartNumber;
  Result.StepOfNumbering := FStepOfNumbering;
  Result.WorkpieceZeroPointID := Byte(FWorkpieceZeroPointID);
  Result.ZeroPointOffsetX := FZeroPointOffset.X;
  Result.ZeroPointOffsetY := FZeroPointOffset.Y;
  Result.FeedOnZ := FFeedOnZ;
  Result.FeedOnXY := FFeedOnXY;
  Result.FeedOnXZ := FFeedOnXZ;
  Result.SpindleSpeed := FSpindleSpeed;
  Result.DepartureOnZ := FDepartureOnZ;
  Result.LaserOnCommand := FLaserOnCommand;
  Result.LaserOffCommand := FLaserOffCommand;
  Result.Delay := FDelay;
  Result.UseLaserPower := FUseLaserPower;
  Result.LaserPower := FLaserPower;
  Result.CodeOptimize := FCodeOptimize;
  Result.PositioningSystemID := FPositioningSystemID;
  Result.TrailingZeros := FTrailingZeros;
  Result.StartFromZeroPositon := FStartFromZeroPos;
  Result.ForceLaserOffGRBL := FForceLaserOffGRBL;
end;

procedure TsgCADtoGCode.StartContour(APt: TFPoint);
var
  vDepthOnZ: Double;
  vNeedStart: Boolean;
begin
  vNeedStart := (FPassesCounter = 1) or FRetractTool;
  if FPassesCounter = 1 then
  begin
    if FMachineType = mtDispenser then
    begin
      AddToGCode('T' + IntToStr(FCurContour.ToolDispenser));
    end
    else
    begin
      WriteCurContourComments;
      if FMachineType = mtMill then
        AddToGCode(GetTool);
    end;
  end;
  if vNeedStart and (FPassesCounter = 1) then
    AddToGCode(GetPosGCommand(gcG0, APt, nil));
  if FFirstMove then
  begin
    FFirstMove := False;
    if FPositioningSystemID = 1 then
    begin
      if MachineType in MillMachines then
      begin
        AddToGCode(GetAdjGCommand(gcG0, GetAdjGCommand(gcZ, FDepartureOnZ))+ cnstGCommands[gcG91]);
        FPrevAbsoluteZ := FDepartureOnZ;
      end
      else
        AddToGCode(cnstGCommands[gcG91]);
    end;
  end;
  case FMachineType of
    mtMill, mtGRBLRouter:
      begin
        vDepthOnZ := FCurContour.DepthOnZ;
        if FCurContour.NumbOfPassesByMachine > 1 then
          if FPassesCounter < FCurContour.NumbOfPassesByMachine then
            vDepthOnZ := FPassesCounter * FCurContour.DepthPass;
        AddToGCode(GetAdjGCommand(gcG1, GetAdjGCommand(gcZ, vDepthOnZ) + CheckFeedCommand(FFeedOnZ)));
      end;
    mtCut:
      begin
        if vNeedStart then
        begin
          if FDelay > 0 then
            AddToGCode(GetAdjGCommand(gcG4, GetAdjGCommand(gcComParam, FDelay) +
            ' ' + FLaserOnCommand))
          else
            AddToGCode(FLaserOnCommand);
          if FUseLaserPower then
            AddToGCode(GetAdjGCommand(gcM10, 'Q' + IntToStr(FLaserPower)));
        end;
      end;
    mtDispenser:
      begin
        if vNeedStart then
        begin
          AddToGCode(FDispenserOnCommand);
          AddToGCode(FPumpOnCommand);
        end;
      end;
    mtHacoKompakt:
      begin
        FHacoAddCommand := 'M07';
      end;
    mtGRBLLaser:
      begin
        if FForceLaserOffGRBL then
        begin
          AddToGCode(GetAdjGCommand(gcSpindleSpeed, FLaserPower));
        end;
      end;
  end;
end;

procedure TsgCADtoGCode.EndContour;
begin
  if not ((FPassesCounter = FCurContour.NumbOfPassesByMachine) or FRetractTool) then
    Exit;
  case FMachineType of
    mtMill, mtGRBLRouter:
      begin
        AddToGCode(GetAdjGCommand(gcG0, GetAdjGCommand(gcZ, FDepartureOnZ)));
      end;
    mtCut:
      begin
        if FUseLaserPower then
          AddToGCode(cnstGCommands[gcM11]);
        AddToGCode(FLaserOffCommand);
      end;
    mtDispenser:
      begin
        if not FTurnOffPumpBefore then
          AddToGCode(FPumpOffCommand);
        AddToGCode(FDispenserOffCommand);
      end;
    mtHacoKompakt:
      begin
        AddToGCode('M09');
      end;
    mtGRBLLaser:
      begin
        if FForceLaserOffGRBL then
        begin
          AddToGCode(GetAdjGCommand(gcSpindleSpeed, 0));
        end;
      end;
  end;
end;

procedure TsgCADtoGCode.StartGCode;

  function GetGCommandUnit: string;
  begin
    case MachineUnits of
      guMillimetre: Result := cnstGCommands[gcG21];
      guInch:       Result := cnstGCommands[gcG20];
      else
        Result := '';
    end;
  end;

  function GetGFeedUnit: string;
  begin
    case FMachineType of
      mtLathe: Result := cnstGCommands[gcG95];
      else
        Result := cnstGCommands[gcG94];
    end;
  end;

  function GetGCommandPlane: string;
  begin
    case FMachineType of
      mtLathe: Result := cnstGCommands[gcG18];
      else
        Result := cnstGCommands[gcG17];
    end;
  end;

var
  vLastString: string;
begin
  if FMachineType = mtHacoKompakt then
    FGCode.Add('%1' + ' "' + CADImage.FileName + '"')
  else
  begin
    if FMachineType = mtGRBLLaser then
      AddToGCode(cnstGCommands[gcSwitchGrblToLaser]);
    if FShowPercent then
      FGCode.Add('%');
    if FAddLabelOfProgram then
      FGCode.Add('O' + FLabelOfProgram);
    if FShowContourName then
    begin
      if (CADImage.FileName <> '') then
        FGCode.Add('(Created from file: ' + CADImage.FileName + ')');
      FGCode.Add('(Layout "' + FLayoutName + '")');
    end;
  end;
  FCurrentNumber := FStartNumber - FStepOfNumbering;
  if FDemoComment <> '' then
    FGCode.Add(FDemoComment);

  if FMachineType <> mtHacoKompakt then
    AddToGCode(GetGFeedUnit + cnstSpace + cnstGCommands[gcG90] + cnstSpace
     + GetGCommandPlane + cnstSpace + GetGCommandUnit)
  else
    AddToGCode(GetGFeedUnit + cnstSpace + cnstGCommands[gcG90]);
  FPrevCommand := gcNone;
  FPrevFeed := 0;
  FPrevToolNumb := 0;
  if FStartFromZeroPos then
  begin
    AddToGCode(GetPosGCommand(gcG0, cnstFPointZero, True));
    FPrevCommand := gcG0;
  end;
  if (FMachineType in MillMachines) or (FMachineType = mtLathe) then
    AddToGCode(cnstGCommands[gcM3] + cnstSpace + GetAdjGCommand(gcSpindleSpeed, FSpindleSpeed))
  else
  begin
    if FFeedOnXY <> 0 then
    begin
      vLastString := FGCode.Strings[FGCode.Count - 1];
      FGCode.Strings[FGCode.Count - 1] := vLastString + cnstSpace + CheckFeedCommand(FFeedOnXY);
    end;
  end;
  if FMachineType = mtGRBLLaser then
    if FForceLaserOffGRBL then
      AddToGCode(cnstGCommands[gcM3] + cnstSpace + GetAdjGCommand(gcSpindleSpeed, 0))
    else
      AddToGCode(cnstGCommands[gcM3] + cnstSpace + GetAdjGCommand(gcSpindleSpeed, FLaserPower));
end;

procedure TsgCADtoGCode.EndGCode;
var
  vLastPoint: TFPoint;
  vReturnToZeroPoint: Boolean;
begin
  vReturnToZeroPoint := FReturnToZeroPoint;
  vLastPoint := SubFPoint(cnstFPointZero, FWorkpieceZeroPoint);
  if FPositioningSystemID = 1 then
  begin
    AddToGCode(CheckOptimize(gcG90));
    vReturnToZeroPoint := vReturnToZeroPoint and (FMachineType <> mtDispenser);
    vLastPoint := FPrevPoint;
  end
  else
    vReturnToZeroPoint := vReturnToZeroPoint and (not FPrevPointInit or (not IsEqualFPoints(FPrevPoint, vLastPoint)
      and (FMachineType <> mtDispenser)));
  if vReturnToZeroPoint then
    AddToGCode(GetPosGCommand(gcG0, vLastPoint, False, FPositioningSystemID = 1));
  if (FMachineType in MillMachines) or (FMachineType = mtGRBLLaser) or (FMachineType = mtLathe) then
    AddToGCode(cnstGCommands[gcM5]);
  AddToGCode('M30');
  if FMachineType = mtHacoKompakt then
    FGCode.Add('*')
  else
    if FShowPercent then
      FGCode.Add('%');
end;

procedure TsgCADtoGCode.AddPolylines(const AEnts: TsgList);
var
  I: Integer;
  vPoly: TsgDXFPolyline;
begin
  FEntityList.Clear;
  for I := 0 to AEnts.Count - 1 do
  begin
    vPoly := TsgDXFPolyline(AEnts[I]);
    GetPolylineFromVertexes(vPoly);
  end;
  GenerateContoursFromPolylines(FEntityList, 0, True);
end;

procedure TsgCADtoGCode.AddToGCode(S: string);
var
  vNumb: string;
begin
  if S = '' then
    Exit;
  vNumb := '';
  if FAddNumbering then
  begin
    FCurrentNumber := FCurrentNumber + FStepOfNumbering;
    vNumb := 'N' + IntToStr(FCurrentNumber) + ' ';
  end;
  FGCode.Add(vNumb + S);
end;

procedure TsgCADtoGCode.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  FArcToLines := AExportParams.ArcToLines;
  MachineUnits := TsgGUnits(AExportParams.MachineUnitsID);
  DrawingUnits := TsgGUnits(AExportParams.DrawingUnitsID);
  MachineType := TsgGCodeMachineTypeID(AExportParams.MachineTypeID);
  FPassesDirection := TsgGPassesDirection(AExportParams.PassesDirectionID);
  SetPrecision(AExportParams.PrecisionDigits, AExportParams.PrecisionFactor);
  FFeedOnZ := AExportParams.FeedOnZ;
  FFeedOnXY := AExportParams.FeedOnXY;
  FFeedOnXZ := AExportParams.FeedOnXZ;
  FSpindleSpeed := AExportParams.SpindleSpeed;
  SetDepthAndPasses(AExportParams);
  FDepartureOnZ := AExportParams.DepartureOnZ;
  FZeroPointOffset.X := AExportParams.ZeroPointOffsetX;
  FZeroPointOffset.Y := AExportParams.ZeroPointOffsetY;
  WorkpieceZeroPointID := TsgGCodeZeroPointID(AExportParams.WorkpieceZeroPointID);
  FLaserOnCommand := AExportParams.LaserOnCommand;
  FLaserOffCommand := AExportParams.LaserOffCommand;
  FUseLaserPower := AExportParams.UseLaserPower;
  FLaserPower := AExportParams.LaserPower;
  FDelay := AExportParams.Delay;
  if (FMachineType = mtGRBLRouter) or (FMachineType = mtGRBLLaser) then
    FAddLabelOfProgram := False
  else
    FAddLabelOfProgram := AExportParams.AddLabelOfProgam;
  FLabelOfProgram := AExportParams.LabelOfProgram;
  if (FMachineType = mtGRBLRouter) or (FMachineType = mtGRBLLaser) then
    FAddNumbering := False
  else
    FAddNumbering := AExportParams.AddNumbering;
  FShowContourName := AExportParams.ShowContourName;
  FShowLayerName := AExportParams.ShowLayerName;
  if (FMachineType = mtGRBLRouter) or (FMachineType = mtGRBLLaser) then
    FShowPercent := False
  else
    FShowPercent := AExportParams.ShowPercent;
  FStartNumber := AExportParams.StartNumber;
  FStepOfNumbering := AExportParams.StepOfNumbering;
  FCodeOptimize := AExportParams.CodeOptimize;
  FPositioningSystemID := AExportParams.PositioningSystemID;
  FTrailingZeros := AExportParams.TrailingZeros;
  FConvOnlyVisLay := AExportParams.ConvertOnlyVisibleLayers;
  FStartFromZeroPos := AExportParams.StartFromZeroPositon;
  FForceLaserOffGRBL := AExportParams.ForceLaserOffGRBL;
  FAbsoluteCoordIJ := AExportParams.AbsoluteCoordIJ;
  FReturnToZeroPoint := AExportParams.ReturnToZeroPoints;
  FRetractTool := AExportParams.RetractTool;
//Dispenser
  SetDistanseOffset(AExportParams.TurnOffPumpBefore, AExportParams.TurnOffPumpBeforeValue);
  FDispenserOffCommand := AExportParams.DispenserOffCommand;
  FDispenserOnCommand := AExportParams.DispenserOnCommand;
  FPumpOffCommand := AExportParams.PumpOffCommand;
  FPumpOnCommand := AExportParams.PumpOnCommand;
  FStraightenContour := AExportParams.StraightenContour;
  FMergePoints := AExportParams.MergePoints;
  FMergePointsRadius := AExportParams.MergePointsRadius;
//
  Tools := AExportParams.Tools;
  InitWorkpieceZepoPoint;
end;

procedure TsgCADtoGCode.BeforeExport(const S: TStream);
begin
  inherited BeforeExport(S);
  if Contours.Count = 0 then
  begin
    GetContours;
    Convert;
  end;
end;

procedure TsgCADtoGCode.WriteGCodeCommand(const AVertex: TsgDXFVertex; Operation: TsgOperationId);

  function GetArcFromCurVertex: TsgArc;
  var
    vInd: Integer;
  begin
    vInd := FCurPoly.IndexOfEntity(AVertex);
    Result := GetArcFromVertexes(AVertex, FCurPoly.Vertexes[vInd + 1]);
  end;

  procedure WriteArcToLines(const ArcParams: TsgArc; AType: TsgGArcType);
  var
    vGArc: TsgDXFGArc;
    vArcR: TsgArcR;
    I: Integer;
  begin
    vGArc := TsgDXFGArc.Create;
    try
      vArcR := GetArcROfBulge(ArcParams.Point1, ArcParams.Point2, AVertex.Bulge);
      vGArc.Arc := vArcR;
      Converter.Loads(vGArc);
      if vGArc.PointCount >= 2 then
      begin
        if AType = atArcL then
          for I := 1 to vGArc.PointCount - 1 do
            AddToGCode(GetPosGCommand(gcG1, vGArc.Points[I], nil))
        else
          for I := vGArc.PointCount - 2 downto 0 do
            AddToGCode(GetPosGCommand(gcG1, vGArc.Points[I], nil))
      end;
    finally
      FreeAndNil(vGArc);
    end;
  end;

var
  Separator: Char;
  vRadComp: TsgGRadiusComp;
  vArc: TsgArc;
begin
  Separator := SetDecimalSeparator('.');
  try
    case Operation of
      opStart: StartGCode;
      opArcR:
        begin
          vArc := GetArcFromCurVertex;
          if FArcToLines then
            WriteArcToLines(vArc, atArcR)
          else
          AddToGCode(GetPosGCommand(gcG2, vArc.Point2, @vArc.Center));
        end;
      opArcL:
        begin
          vArc := GetArcFromCurVertex;
          if FArcToLines then
            WriteArcToLines(vArc, atArcL)
          else
          AddToGCode(GetPosGCommand(gcG3, vArc.Point2, @vArc.Center));
        end;
      opLine:  AddToGCode(GetPosGCommand(gcG1, AVertex.Point, nil));
      opRadiusCompOn:
        begin
          vRadComp := FCurContour.RadiusCompType;
          case vRadComp of
            grcLeft:  AddToGCode(GetPosGCommand(gcG41, AVertex.Point, nil));
            grcRight: AddToGCode(GetPosGCommand(gcG42, AVertex.Point, nil));
          end;
        end;
      opRadiusCompOff: AddToGCode(GetPosGCommand(gcG40, AVertex.Point, nil));
      opStartContour: StartContour(AVertex.Point);
      opEndContour: EndContour;
      opEndProgram: EndGCode;
    end;
  finally
    SetDecimalSeparator(Separator);
  end;
end;

procedure TsgCADtoGCode.SelectGCodeCommand(var AIndex: Integer);
var
  vPrevVer: TsgDXFVertex;

  procedure SetArcGCodeCommand;
  var
    vBeforePtIndex, vAfterPtIndex: Integer;
  begin
    vBeforePtIndex := -1;
    vAfterPtIndex := -1;
    if (FMachineType = mtDispenser) and FTurnOffPumpBefore
      and (FCurContour.TurnOffPumpVerInd <> -1) then
    begin
//      vBeforePtIndex := FArcCurrent.PolyPoints.IndexOf(FCurContour.BeforeDistanceOffsetPoint);
//      vAfterPtIndex := FArcCurrent.PolyPoints.IndexOf(FCurContour.AfterDistanceOffsetPoint);
    end;
    if (vBeforePtIndex <> -1) and (vAfterPtIndex <> -1)  then        //mtDispenser and FTurnOffPumpBefore
    begin
//      if FArcCurrent.ArcType = atArcR then
//      begin
//        FOutPointList.Add(FArcCurrent.PolyPoints[vBeforePtIndex]);
//        vIndexOffset := FArcCurrent.PolyPoints.Count - 1 - vAfterPtIndex;
//      end
//      else
//      begin
//        FOutPointList.Add(FArcCurrent.PolyPoints[vAfterPtIndex]);
//        vIndexOffset := vBeforePtIndex;
//      end;
        // TODO
    end                                                               //other machines
    else
    begin
      if vPrevVer.Bulge > 0 then
        WriteGCodeCommand(vPrevVer, opArcL)
      else
        WriteGCodeCommand(vPrevVer, opArcR);
    end;
  end;

begin
  vPrevVer := FCurPoly.Vertexes[AIndex - 1];
  if not FTurnOffPumpBefore and (FCurPoly.IndexOfEntity(FCurrentVertex) = FCurContour.TurnOffPumpVerInd) then
    Exit;
  if not vPrevVer.HasBulge and IsEqualF2DPoints(FCurrentVertex.Point.Point2D, FPrevPoint.Point2D) and (FMaxIndex > 1) then
    Exit;
  if (AIndex <= FMaxIndex) then
  begin
    if vPrevVer.HasBulge then
      SetArcGCodeCommand
    else
    begin
      WriteGCodeCommand(FCurrentVertex, opLine);
    end;
    if (FMachineType = mtDispenser) and FTurnOffPumpBefore and
    (FCurPoly.IndexOfEntity(FCurrentVertex) = FCurContour.TurnOffPumpVerInd) then
      AddToGCode(FPumpOffCommand);
  end;
end;

procedure TsgCADtoGCode.InitWorkpieceZepoPoint;
begin
  case FWorkpieceZeroPointID of
    zpCADZero:     FWorkpieceZeroPoint := cnstFPointZero;
    zpTopLeft:     FWorkpieceZeroPoint := SubFPoint(cnstFPointZero, CADBox.TopLeft);
    zpTopRight:    FWorkpieceZeroPoint := SubFPoint(cnstFPointZero, MakeFPoint(CADBox.Right, CADBox.Top));
    zpBottomRight: FWorkpieceZeroPoint := SubFPoint(cnstFPointZero, CADBox.BottomRight);
    zpBottomLeft:  FWorkpieceZeroPoint := SubFPoint(cnstFPointZero, MakeFPoint(CADBox.Left, CADBox.Bottom));
  end;
  FWorkpieceZeroPoint := SubFPoint(FWorkpieceZeroPoint, FZeroPointOffset);
end;

function MakeEquidistantContour(AOffset: Double; AType: TsgEquidistantType): TsgEquidistantContour;
begin
  Result := TsgEquidistantContour.Create;
  Result.Offset := AOffset;
  Result.EquidistantType := AType;
end;

procedure TsgCADtoGCode.InternalRadiusCompGRBL(Contour: TObject; ARadiusComp: TsgGRadiusComp);
{$IFDEF SG_TransformAnalogy}
var
  I: Integer;
  vEqEnts: TsgList;
  vRadius: Double;
  vName: string;
  vEqType: TsgEquidistantType;
  vContour: TsgGContour absolute Contour;
  vEquidistant: TsgEquidistantContour;
  vEqCreator: TsgAnalogyCreator;
begin
  vEqEnts := TsgList.Create;
  vRadius := vContour.ToolParams.Radius;
  vEqType := TsgEquidistantType(Ord(vContour.RadiusCompType));
  try
    vEqCreator := TsgAnalogyCreator.Create(Converter);
    try
      vEqCreator.MakeEquidistant(vContour.GPolyline, vRadius, vEqType, vEqEnts);
    finally
      vEqCreator.Free;
    end;
    if vEqEnts.Count > 0 then
    begin
      vEquidistant := MakeEquidistantContour(vRadius, vEqType);
      vName := 'Radiuscomp for ' + vContour.Name + ')' + #13#10 + '(Params^: ' +
        cnstRadiusCompGRBL[vEqType] + '||' + 'R ' + FloatToString(vRadius);
      for I := 0 to vEqEnts.Count - 1 do
      begin
        ConvertPolyToGPolyInList(vEqEnts, I);
        Converter.Loads(vEqEnts[I]);
        vEquidistant.AddContour(MakeContourFromPolyline(vEqEnts[I], vName, vContour.LayerName));
      end;
      vContour.AddRadiusCompGRBL(vEquidistant);
    end
    else
      vContour.RadiusCompType := grcNone;
  finally
    FreeAndNil(vEqEnts);
  end;
{$ELSE}
begin
{$ENDIF}
end;

function TsgCADtoGCode.OpenEntity(AEntity: TsgDXFEntity; AClosedEntList: TList): Boolean;
var
  PNeedCloseEnt: PsgNeedCloseEnt;
begin
  Result := False;
  if AEntity is TsgCADBasePolyline then
    if TsgCADBasePolyline(AEntity).Closed then
    begin
      New(PNeedCloseEnt);
      FillChar(PNeedCloseEnt^, SizeOf(PNeedCloseEnt^), 0);
      PNeedCloseEnt.Entity := AEntity;
      AClosedEntList.Add(PNeedCloseEnt);
      TsgCADBasePolyline(AEntity).Closed := False;
      if not IsEqualFPoints(TsgCADBasePolyline(AEntity).PolyPoints.First,
      TsgCADBasePolyline(AEntity).PolyPoints.Last) then
      begin
        PNeedCloseEnt.NeedRemovePoint := True;
        TsgCADBasePolyline(AEntity).PolyPoints.Add(TsgCADBasePolyline(AEntity).PolyPoints.First);
        Result := not TsgCADBasePolyline(AEntity).Closed;
      end;
    end;
end;

procedure TsgCADtoGCode.ProgressGenerateContours(Sender: TObject;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
  const Msg: string);
begin
  Progress(Stage, PercentDone, Msg);
end;

function TsgCADtoGCode.AddVertexWithOptimize(const APoly: TsgDXFPolyline; const APoint: TFPoint;
  APtInd: Integer; ABulge: Double = 0): Integer;
begin
  Result := -1;
  if APoly.Count > 0 then
  begin
    AddVertexInPolyline(APoly, APoint, ABulge);

//    if not IsEqualFPoints(APoly.Vertexes[APoly.Count - 1].Point, APoint, FGCodeResolution) then
//      AddVertexInPolyline(APoly, APoint, ABulge)
//    else
//    begin
//      Result := APtInd;
//      APoly.DeleteEntity(APoly.Count - 1);
//      AddVertexInPolyline(APoly, APoint, ABulge);
//    end;

  end
  else
    AddVertexInPolyline(APoly, APoint, ABulge);
end;

procedure TsgCADtoGCode.CalcNewVertexesXMat(const APoints: TFPointList; ADestPoly: TsgDXFPolyline);
var
  I: Integer;
  vPoint: TFPoint;
begin
  for I := 0 to APoints.Count - 1 do
  begin
    vPoint := FPointXMat(APoints[I], Params^.Matrix);
    AddVertexWithOptimize(ADestPoly, vPoint, 0);
  end;
end;

procedure TsgCADtoGCode.CalcNewVertexesXMat(const ASorcePoly, ADestPoly: TsgDXFPolyline);
var
  I: Integer;
  vPoint: TFPoint;
  vVertex: TsgDXFVertex;
begin
  for I := 0 to ASorcePoly.Count - 1 do
  begin
    vVertex := ASorcePoly.Vertexes[I];
    vPoint := FPointXMat(vVertex.Point, Params.Matrix);
    AddVertexWithOptimize(ADestPoly, vPoint, 0, vVertex.Bulge);
  end;
  if (ASorcePoly.PointCount > 0) and ASorcePoly.Closed and
    not IsEqualFPoints(ASorcePoly.Vertexes[0].Point, ASorcePoly.Vertexes[ASorcePoly.Count - 1].Point) then
    AddVertexInPolyline(ADestPoly, ADestPoly.Vertexes[0].Point, 0);
end;

procedure TsgCADtoGCode.CalcPolyPointsXMat(const APts, ANewPts: TFPointList;
  AClose: Boolean = False; AIsZThick: Boolean = False);
var
  I, vCnt: Integer;
  vPoint: TFPoint;
begin
  if not AIsZThick then
    vCnt := APts.Count - 1
  else
    vCnt := Round((APts.Count) / 2) - 1;
  for I := 0 to vCnt do
  begin
    vPoint := FPointXMat(APts[I], Params^.Matrix);
    ANewPts.Add(vPoint);
  end;
  if AClose and not IsEqualFPoints(ANewPts.First, ANewPts.Last) then
    ANewPts.Add(ANewPts.First);
end;

procedure TsgCADtoGCode.GetPolylineFromLine(const AEntity: TsgDXFLine);
var
  I: Integer;
  vPoint: TFPoint;
  vPolyline: TsgDXFPolyline;
  vList: TList;
begin
  vList := TList.Create;
  try
    vPolyline := TsgGCodePolyline.CreateEx(AEntity.Layer.Name);
    vList.Add(vPolyline);
    for I := 0 to 1 do
    begin
      vPoint := FPointXMat(AEntity.Points[I], Params^.Matrix);
      vPolyline.PolyPoints.Add(vPoint);
    end;
    LoadPolyEntities(vList, FEntityList);
  finally
    vList.Free;
  end;
end;

procedure TsgCADtoGCode.GetPolylineFromPoint(const AEntity: TsgDXFEntity);
var
  vPoint: TFPoint;
  vPolyline: TsgDXFPolyline;
  vList: TList;
  vEnt: TsgDXFPoint absolute AEntity;
begin
  vList := TList.Create;
  try
    vPolyline := TsgGCodePolyline.CreateEx(AEntity.Layer.Name);
    vList.Add(vPolyline);
    vPoint := FPointXMat(vEnt.Point, Params^.Matrix);
    vPolyline.PolyPoints.Add(vPoint);
    vPolyline.PolyPoints.Add(vPoint);
    LoadPolyEntities(vList, FEntityList);
  finally
    vList.Free;
  end;
end;

function TsgCADtoGCode.GetPolylineFromPolyPoints(const AEntity: TsgCADBasePolyline;
  ALoad: Boolean = True; AddInBlock: Boolean = True): TsgDXFPolyline;
var
  vClose, vZThick: Boolean;
  vLayerName: string;
  vPrevQuality: Integer;
begin
  if Assigned(AEntity.Layer) then
    vLayerName := AEntity.Layer.Name
  else
    vLayerName := '';

  Result := TsgGCodePolyline.CreateEx(vLayerName);
  vClose := AEntity.Closed;
  vZThick := AEntity.IsPolyZThickness;
  if (AEntity.EntType = ceHelix) and (TsgCADHelix(AEntity).Fit.Count > 0) then
  begin
    vPrevQuality := TsgCADHelixAccess(AEntity).Quality;
    TsgCADHelixAccess(AEntity).Quality := 4;      //добавить изменение качества и перегрузки примитива на создании блока!!!
    Converter.Loads(AEntity);
    CalcPolyPointsXMat(TsgCADHelix(AEntity).Fit, Result.PolyPoints, vClose, vZThick);
    if vPrevQuality = 0 then
      vPrevQuality := 1;
    TsgCADHelixAccess(AEntity).Quality := vPrevQuality;
    Converter.Loads(AEntity);
  end
  else
  CalcPolyPointsXMat(AEntity.PolyPoints, Result.PolyPoints, vClose, vZThick);
  if OptimizePolyline(Result) then
    LoadNewEntity(Result, FEntityList, AddInBlock);

//  MakeGArc(AEntity);
//  Result := TsgGCodePolyline.CreateEx(AEntity.Layer.Name);
//  vClose := AEntity.Closed;
//  vZThick := AEntity.IsPolyZThickness;
//  CalcPolyPointsXMat(AEntity.PolyPoints, Result.PolyPoints, vClose, vZThick);
//  LoadPolyEntity(Result, FEntityList, ALoad, AddInBlock);
end;

function TsgCADtoGCode.GetPolylineFromPoints(const APoints: TFPointList): TsgGCodePolyline;
begin
  Result := TsgGCodePolyline.CreateEx('');
  CalcNewVertexesXMat(APoints, Result);
  if OptimizePolyline(TsgDXFPolyline(Result)) then
    LoadNewEntity(Result, FEntityList);
end;

function TsgCADtoGCode.GetPolylineFromVertexes(const AEntity: TsgDXFPolyline;
  ALoad: Boolean = True; AddInBlock: Boolean = True): TsgGCodePolyline;
var
  vLayerName: string;
begin
  if AEntity is TsgGCodePolyline then
    vLayerName := TsgGCodePolyline(AEntity).LayerName
  else
    vLayerName := AEntity.Layer.Name;
  Result := TsgGCodePolyline.CreateEx(vLayerName);
  CalcNewVertexesXMat(AEntity, Result);
  if OptimizePolyline(TsgDXFPolyline(Result)) then
    LoadNewEntity(Result, FEntityList, AddInBlock);
end;

function TsgCADtoGCode.MakeGArcOfBulge(AStartPoint, AEndPoint: TFPoint;
  ABulgeValue: Double): TsgDXFGArc;
var
  vBulgeArcR: TsgArcR;
begin
  vBulgeArcR := GetArcROfBulge(AStartPoint, AEndPoint, ABulgeValue);
  Result := TsgDXFGArc.Create;
  Result.Arc := vBulgeArcR;
  Converter.Loads(Result);
  FGArcList.Add(Result);
end;

function TsgCADtoGCode.FinishExportProc(AEntity: TsgDXFEntity): Integer;
begin
  Result := 0;
end;

procedure TsgCADtoGCode.GetPolylinesFromText(const AText: TsgDXFText);
var
  I, J: Integer;
  vCollection: TsgTextLinesCollection;
  vPolyChar: TsgDXFPolyline;
  P: PFPointArray;
begin
  vCollection := TsgTextLinesCollection.Create(TFPointList.Create, True);
  try
    if (CADImage.Converter.GetTextPolylines(AText, vCollection) > 0) and (vCollection.Poly.Count > 0) then
    begin
      P := TFPointList(vCollection.Poly).List;
      for I := 0 to vCollection.Counts.Count - 1 do
        if vCollection.Counts[I] > 0 then
        begin
          vPolyChar := TsgGCodePolyline.CreateEx(AText.Layer.Name);
          try
            for J := 0 to vCollection.Counts[I] - 1 do
              AddVertexInPolyline(vPolyChar, P^[J]);
            Inc(PFPoint(P), vCollection.Counts[I]);
            GetPolylineFromVertexes(vPolyChar);
          finally
            vPolyChar.Free
          end;
        end;
    end;
  finally
    vCollection.Free;
  end;
end;

procedure TsgCADtoGCode.GetPolylinesFromHatch(const AEntity: TsgDXFEntity);
var
  I: Integer;
  vPts: TFPointList;
  vPolyline: TsgGCodePolyline;
begin
  for I := 0 to TsgCADCurvePolygon(AEntity).Boundaries.Count - 1 do
  begin
    vPts := TFPointList.Create;
    try
      Convert2DTo3DPtList(TF2DPointList(TsgCADCurvePolygon(AEntity).Boundaries[I]), vPts);
      vPolyline := GetPolylineFromPoints(vPts);
      if Assigned(vPolyline) then
        vPolyline.LayerName := AEntity.Layer.Name;
    finally
      vPts.Free;
    end;
  end;
end;

procedure TsgCADtoGCode.LoadPolyEntities(const APolylines: TList; const ADestList: TList);
var
  I: Integer;
  vPolyEnt: TsgDXFPolyline;
begin
  for I := 0 to APolylines.Count - 1 do
  begin
    vPolyEnt := TsgDXFPolyline(APolylines[I]);
    if OptimizePolyline(vPolyEnt) then
      LoadNewEntity(vPolyEnt, ADestList);
  end;
end;

procedure TsgCADtoGCode.OptimizeVertexesByResolution(const APolyline: TsgDXFPolyline);
var
  I: Integer;
  vVer, vNextVer, vPrevVer: TsgDXFVertex;
  vIsBulgsZero: Boolean;
begin
  vPrevVer := APolyline.Vertexes[0];
  I := 1;
  while I < APolyline.Count - 1 do
  begin
    vVer := APolyline.Vertexes[I];
    if I < APolyline.Count - 1 then
    begin
      vNextVer := APolyline.Vertexes[I + 1];
      vIsBulgsZero := (vPrevVer.Bulge = 0) and (vVer.Bulge = 0);
      if vIsBulgsZero and IsPointOnLinePts(vPrevVer.Point, vNextVer.Point, vVer.Point, fExtendedResolution) then
      begin
        APolyline.DeleteEntity(I);
        vVer.Free;
      end
      else
      begin
      vPrevVer := vVer;
        Inc(I);
    end;
  end;
  end;
end;

procedure TsgCADtoGCode.OptimizePolyPointsByResolution(const APolyline: TsgDXFPolyline);
var
  I: Integer;
  vPt, vNextPt, vPrevPt: TFPoint;
begin
  vPrevPt := APolyline.PolyPoints[0];
  AddVertexInPolyline(APolyline, vPrevPt);
  for I := 0 to APolyline.PointCount - 1 do
  begin
    vPt := APolyline.PolyPoints[I];
    if I < APolyline.PointCount - 1 then
    begin
      vNextPt := APolyline.PolyPoints[I + 1];
      if IsPointOnLinePts(vPrevPt, vNextPt, vPt, fExtendedResolution) then
        Continue;
    end;
    if not IsEqualFPoints(vPt, vPrevPt, fDoubleResolution) or (APolyline.PointCount = 2) then
    begin
      AddVertexInPolyline(APolyline, vPt);
      vPrevPt := vPt;
    end;
  end;
end;

function TsgCADtoGCode.OptimizePolyline(var APolyline: TsgDXFPolyline): Boolean;
begin
  Result := False;
  try
    if APolyline.Count > 0 then
      OptimizeVertexesByResolution(APolyline)
    else
      if APolyline.PointCount > 0 then
        OptimizePolyPointsByResolution(APolyline);
  finally
    if APolyline.Count > 1 then
      Result := True
    else
      FreeAndNil(APolyline);
  end;
end;

procedure TsgCADtoGCode.LoadNewEntity(const AEntity: TsgDXFEntity; const ADestList: TList;
  AddInBlock: Boolean = True);
begin
  AEntity.Layer := FEditingLayer;
  Converter.Loads(AEntity);
  if AddInBlock and Assigned(EditingBlock) then
    EditingBlock.AddEntity(AEntity);
  if ADestList <> nil then
    ADestList.Add(AEntity);
end;

procedure TsgCADtoGCode.GetPolylineFromModEntity(const AEnt: TsgModEntity; var AParams: TsgBrepModIterateParams);
var
  vPnt: TFPoint;
  vPPnt: PFPoint;
  I: Integer;
  vModPoly: TsgModMeshPolyline;
  vNodePoly: TsgModMeshPolylineNode;
  vPoly: TsgDXFPolyline;
begin
{$IFNDEF SG_NO_USE_KERNEL3D}
  if (AEnt.GetEntType = mtTopology) and (TsgModTopoShape(AEnt).GetShapeType = btEdge) then
  begin
    vModPoly := TsgModTopoEdge(AEnt).ExtractFirstPolyline;
    if Assigned(vModPoly) then
    begin
      vPoly := TsgGCodePolyline.CreateEx('');
      vNodePoly := vModPoly.FirstNode;
      while Assigned(vNodePoly) do
      begin
        vPnt := PointTransform(vNodePoly.Point, AParams.Matrix);
        vPnt.Z := 0;
        vPoly.PolyPoints.Add(vPnt);
        vNodePoly := vNodePoly.Next;
      end;
      if OptimizePolyline(vPoly) then
        LoadNewEntity(vPoly, FEntityList);
    end;
  end;
{$ENDIF}
end;

procedure TsgCADtoGCode.GetPolylineFrom3dSolid(const AEntity: TsgDXFEntity);
var
  vBrepModParams: TsgBrepModIterateParams;
begin
{$IFNDEF SG_NO_USE_KERNEL3D}
  vBrepModParams.CADColor := 0;
  vBrepModParams.Matrix := {$IFNDEF SG_NO_USE_KERNEL3D}IdentityMatrix4d{$ELSE}cnstIdentityMat{$ENDIF};
  vBrepModParams.PartColor := nil;
  vBrepModParams.ShapeColor := nil;
  vBrepModParams.Data := AEntity;
  TsgBrepModEntity(AEntity).DrawIterateCompound(GetPolylineFromModEntity, vBrepModParams);
{$ENDIF}
end;

function TsgCADtoGCode.GetPolylineFromCircle(const AEntity: TsgDXFEntity): TsgGCodePolyline;
var
  vArcR: TsgArcR;
  vCenter: TFPoint;
  vBulge, vRadius: Double;
  vPoint: TFPoint;
begin
  Result := nil;
  if not (AEntity.EntType in [ceCircle, ceArc]) then
    Exit;
  vRadius := TsgDXFCircle(AEntity).Radius * sgFunction.DistanceVector2D(
    Params^.Matrix.EX.Point2D.X, Params^.Matrix.EX.Point2D.Y);
  vCenter := FPointXMat(TsgDXFCircle(AEntity).Point, Params^.Matrix);
  case AEntity.EntType of
    ceArc:
      begin
        vArcR := TsgDXFArc(AEntity).Arc;
        vArcR.AngleS := GetAngleByPoints(vCenter, FPointXMat(TsgDXFArc(AEntity).StartPoint, Params^.Matrix), False);
        vArcR.AngleE := GetAngleByPoints(vCenter, FPointXMat(TsgDXFArc(AEntity).EndPoint, Params^.Matrix), False);
      end;
    ceCircle:
      begin
        vArcR.AngleS := 0;
        vArcR.AngleE := 360;
      end;
  end;
  vArcR.Radius := vRadius;
  vArcR.Center := vCenter;
  Result := TsgGCodePolyline.CreateEx(AEntity.Layer.Name);
  if (vArcR.AngleS = 0) and (vArcR.AngleE = 360) then
  begin
    vArcR.AngleE := 180;
    vPoint := GetPointOnCircle(vArcR.Center, vArcR.Radius, vArcR.AngleS);
    vBulge := BulgeByArc(vArcR, vPoint);
    AddVertexInPolyline(Result, vPoint, vBulge);

    vArcR.AngleS := 180;
    vArcR.AngleE := 360;
    vPoint := GetPointOnCircle(vArcR.Center, vArcR.Radius, vArcR.AngleS);
    vBulge := BulgeByArc(vArcR, vPoint);
    AddVertexInPolyline(Result, vPoint, vBulge);

    vPoint := GetPointOnCircle(vArcR.Center, vArcR.Radius, vArcR.AngleE);
    AddVertexInPolyline(Result, vPoint);
  end
  else
  begin
    vPoint := GetPointOnCircle(vArcR.Center, vArcR.Radius, vArcR.AngleS);
    vBulge := BulgeByArc(vArcR, vPoint);
    AddVertexInPolyline(Result, vPoint, vBulge);
    vPoint := GetPointOnCircle(vArcR.Center, vArcR.Radius, vArcR.AngleE);
    AddVertexInPolyline(Result, vPoint);
  end;
  if Assigned(Result) then
    LoadNewEntity(Result, FEntityList, True);
end;

function IsExctruded(const AEntity: TsgDXFEntity): Boolean;
begin
  Result := False;
  case AEntity.EntType of
    cePolyline, ceLWPolyline, ceArc, ceCircle, ceSpline, ceEllipse, ceLeader, ceHelix:
      begin
        Result := TsgDXFPenLineAccess(AEntity).IsExtruded;
      end;
  end;
end;

procedure TsgCADtoGCode.MakeNewPolyEntities(const AEntity: TsgDXFEntity);
var
  vIsExtruded: Boolean;
  vBackIterateMat, vExtrudMat: TFMatrix;
  vIsXYPlane: Boolean;
begin
  vIsExtruded := IsExctruded(AEntity);
  vBackIterateMat := Params^.Matrix;
  if vIsExtruded then
  begin
    vExtrudMat := ExtrusionToMatrix(TsgDXFEintityAccess(AEntity).GetExtrusion);
    FParams^.Matrix := FMatXMat(vExtrudMat, Params^.Matrix);
  end;
  case AEntity.EntType of
    ceLine:
      begin
        GetPolylineFromLine(TsgDXFLine(AEntity));
      end;
    cePolyline, ceLWPolyline:
      begin
        if not IsEqualFMatrix(vBackIterateMat, cnstIdentityMat, False) then
          GetPolylineFromPolyPoints(TsgCADBasePolyline(AEntity))
        else
          GetPolylineFromVertexes(TsgDXFPolyline(AEntity));
      end;
    ceArc, ceCircle:
      begin
        vIsXYPlane := IsEqualFPoints(Ort(AffineTransformPoint(cnstZOrtAxis, vBackIterateMat)), cnstZOrtAxis);
        //сделать проверку что масштабы по ося Х и У равны!!!!!!!
        if IsEqualFMatrix(vBackIterateMat, cnstIdentityMat, False) or vIsXYPlane then
          GetPolylineFromCircle(AEntity)
        else
          GetPolylineFromPolyPoints(TsgCADBasePolyline(AEntity));

      end;
    ceSpline, ceEllipse, ceLeader, ceHelix:
      begin
        GetPolylineFromPolyPoints(TsgCADBasePolyline(AEntity));
      end;
    ceHatch, ceCurvePolygon:
      begin
        GetPolylinesFromHatch(AEntity);
      end;
    ceText:
      begin
        GetPolylinesFromText(TsgDXFText(AEntity));
      end;
    ceRegion, ceSurface, ce3DSolid, ceBody,
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor:
      begin
        GetPolylineFrom3dSolid(AEntity);
      end;
    cePoint:
      begin
        GetPolylineFromPoint(AEntity);
      end;
  end;
  if vIsExtruded then
    FParams^.Matrix := vBackIterateMat;
end;

function TsgCADtoGCode.ExportProc(AEntity: TsgDXFEntity): Integer;
var
  vRealVisible: Boolean;
begin
  Result := 0;
  vRealVisible := TsgCADImageAccess(CADImage).EntVisibleByLayer(AEntity,
    CADImage.Converter.Params^.Insert);
  if not (FConvOnlyVisLay and not vRealVisible) then
    MakeNewPolyEntities(AEntity);
end;

procedure TsgCADtoGCode.SaveToStreamCustom(S: TStream);
begin
  if Assigned(CADImage) then
  begin
    if FGCode.Count > 0 then
      FGCode.SaveToStream(S);
  end;
end;

function GetOffsetDirection(AMaxHeight: Double;
  ABoundaries: TF2DPointList; AContourType: TsgEquidistantType): ShortInt;
var
  I, vCrossCnt1, vCrossCnt2, vContourMode: Integer;
  vPolyline: TFPointList;
  vNormPoint1, vNormPoint2, vPt1, vPt2, vMidPoint: TFPoint;
  vLine1, vLine2: TsgLine;
  vCrosses: TFPointList;
  vOffset: Double;
  vIsFindDirection: Boolean;
begin
  Result := 1;
  vIsFindDirection := False;
  vOffset := AMaxHeight / 1000;
  vContourMode := 1;
  if AContourType = eqtOutside then
    vContourMode := -1;
  vCrosses := TFPointList.Create;
  vPolyline := TFPointList.Create;
  Convert2DTo3DPtList(ABoundaries, vPolyline);
  try
    I := 0;
    while not vIsFindDirection and (I < (ABoundaries.Count - 1)) do
    begin
      vPt1 := F2DPointTo3D(ABoundaries[I]);
      vPt2 := F2DPointTo3D(ABoundaries[I + 1]);
      vMidPoint := MiddleFPoint(vPt1, vPt2);
      vNormPoint1 := GetNormalPt(vPt1, vPt2, vMidPoint, vOffset);
      vNormPoint2 := GetNormalPt(vPt1, vPt2, vMidPoint, -vOffset);
      vLine1 := MakeLine(vNormPoint1, MakeFPoint(vNormPoint1.X, vNormPoint1.Y - AMaxHeight));
      vLine2 := MakeLine(vNormPoint2, MakeFPoint(vNormPoint2.X, vNormPoint2.Y - AMaxHeight));
      vCrossCnt1 := IsCrossSegmentPolylinePts(vLine1.Point1, vLine1.Point2, vPolyLine, False, vCrosses, 0);
      vCrossCnt2 := IsCrossSegmentPolylinePts(vLine2.Point1, vLine2.Point2, vPolyLine, False, vCrosses, 0);
      vIsFindDirection := (((vCrossCnt1 mod 2) > 0) and ((vCrossCnt2 mod 2) = 0) or
        ((vCrossCnt1 mod 2) = 0) and ((vCrossCnt2 mod 2) > 0));
      if vIsFindDirection then
        if vCrossCnt2 = 0 then
          Result := vContourMode
        else
          Result := -vContourMode;
      Inc(I);
    end;
  finally
    vCrosses.Free;
    vPolyline.Free;
  end;
end;

function GetNearPointToEdge(const Arc: TsgArcR; Pt1, Pt2: TFPoint): TF2DPoint;
var
  I, J, vIndex: Integer;
  vDist, vMinDist: Double;
  vPts: TsgPoints4;
begin
  Result := cnstBadPoint.Point2D;
  vPts[0] := Pt1;
  vPts[1] := Pt2;
  vPts[2] := F2DPointTo3D(GetStartF2DPoint(Arc));
  vPts[3] := F2DPointTo3D(GetEndF2DPoint(Arc));
  vMinDist := MaxInt;
  vIndex := -1;
  for I := 0 to 1 do
    for J := 2 to 3 do
    begin
////// FIX THE SEARCH FOR THE NEED CROSSING POINT
      vDist := DistanceF2DPoint(vPts[I].Point2D, vPts[J].Point2D);
      if vDist < vMinDist then
      begin
        vMinDist := vDist;
        vIndex := I;
      end;
    end;
  if vIndex <> -1 then
    Result := vPts[vIndex].Point2D;
end;

function TsgCADtoGCode.MakePocket(const AContour: TsgGContour; const AParams: TsgPocketParams): Boolean;
var
  I: Integer;
  vPocket: TsgGPocket;
begin
  Result := False;
  vPocket := TsgGPocket.Create(AContour, Converter);
  if AParams.DisableSource then
    AContour.Enable := False;
  vPocket.ApplyParams(AParams);
  if GeneratePocketComponents(AContour, vPocket) then
  begin
    TsgGPocketAccess(vPocket).GenerateContours;
    if vPocket.Contours.Count > 0 then
      AContour.HavePocketContour := True;
    for I := 0 to vPocket.Contours.Count - 1 do
    begin
      FContourList.Add(vPocket.Contours[I]);
      TsgGContourAccess(vPocket.Contours[I]).SetTools(FTools);
      LoadNewEntity(TsgGContour(vPocket.Contours[I]).GPolyline, nil);
    end;
    FPocketList.Add(vPocket);
    Result := True;
  end
  else
    vPocket.Free;
end;

function AreaX(const APoly: TsgDXFPolyline): Double;
var
  I, vH: Integer;
  vPnt, vPrev: TFPoint;
begin
  Result := 0;
  vH := APoly.PointCount - 1;
  if vH <= 0 then
    Exit;
  vPrev := APoly[0];
  for I := 1 to vH do
  begin
    vPnt := APoly[I];
    Result := Result + (vPrev.X - vPnt.X)*(vPrev.Y + vPnt.Y);
    vPrev := vPnt;
  end;
end;

function GetAreaPoly(const APoly: TsgDXFPolyline): Double;
begin
  Result := AreaX(APoly);
end;

function TsgCADtoGCode.IsClockWise(const APoly: TsgDXFPolyline): Boolean;
begin
  Result := AreaX(APoly) < 0;
end;

function TsgCADtoGCode.GetContoursByEntity(const AEntity: TsgDXFEntity;
  const AContours: TList): Boolean;
var
  I: Integer;
  vContour: TsgGContour;
begin
  Result := False;
  if AEntity is TsgGCodePolyline then
  begin
    vContour := TsgGContour(TsgGCodePolyline(AEntity).LinkEntity);
    if Assigned(vContour) then
    begin
      AContours.Add(vContour);
      Result := True;
    end;
  end
  else
  begin
    for I := 0 to FContourList.Count - 1 do
    begin
      vContour := FContourList[I];
      if Assigned(vContour.GPolyline) and (vContour.GPolyline = TsgCADBasePolyline(AEntity)) then
      begin
        AContours.Add(vContour);
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TsgCADtoGCode.MakeContourFromPolyline(const APolyline: TsgDXFPolyline;
   AName, ALayerName: string): TsgGContour;
begin
  Result := TsgGContour.Create;
  Result.OnMakeEquidistant := InternalRadiusCompGRBL;
  if APolyline is TsgGCodePolyline then
  begin
    Result.GPolyline := TsgGCodePolyline(APolyline);
    TsgGCodePolyline(APolyline).LinkEntity := Result;
  end;
  Result.MachineType := FMachineType;
  InitContourDepthAndPasses(Result, FDepthOnZ, FDepthPass, FNumbOfPasses);
  if FTools.Count > 0 then
    TsgGContourAccess(Result).SetTools(FTools);
  if (MachineType = mtDispenser) and FTurnOffPumpBefore then
    TsgGContourAccess(Result).TurnOffPumpDist := FTurnOffPumpBeforeValue;
  Result.Name := AName;
  Result.LayerName := ALayerName;
end;

procedure TsgCADtoGCode.GenerateContoursFromPolylines(const APolylineList: TList;
  AStep: Integer = 0; IsEqudistant: Boolean = False);

  function IsCircle(const APoly: TsgGCodePolyline): PsgArcR;
  var
    vCond1, vCond2, vCond3, vCond4: Boolean;
  begin
    Result := nil;
    vCond1 := APoly.Count = 3;
    if not vCond1 then
      Exit;
    vCond2 := IsEqual(APoly.Vertexes[0].Bulge, 1, FGCodeResolution);
    vCond3 := IsEqual(APoly.Vertexes[0].Bulge, APoly.Vertexes[1].Bulge, FGCodeResolution);
    vCond4 := IsEqualVertexPoint(APoly.Vertexes[0], APoly.Vertexes[2]);
    if vCond1 and vCond2 and vCond3 and vCond4 then
    begin
      New(Result);
      Result^ := GetArcROfBulge(APoly.Vertexes[0].Point, APoly.Vertexes[1].Point, 1);
    end;
  end;

var
  I, vCounter, vFactor: Integer;
  vPolyline: TsgGCodePolyline;
  vContour: TsgGContour;
  vName: string;
  vArcR: PsgArcR;
  vFP, vLP: TFPoint;
begin
  vCounter := 0;
  vFactor := 0;
  for I := 0 to APolylineList.Count - 1 do
  begin
    Inc(vCounter);
    vPolyline := TsgGCodePolyline(APolylineList[I]);
    vArcR := IsCircle(vPolyline);

    if IsEqudistant then
      vName := 'Equidistant '// + IntToStr(FContourList.Count)
    else
      vName := 'Contour ' + IntToStr(FContourList.Count);
    vContour := MakeContourFromPolyline(vPolyline, vName, vPolyline.LayerName);
    FContourList.Add(vContour);
    vPolyline.ClosedByResolution := IsClosed(vPolyline, FGCodeResolution);
    if vArcR <> nil then
    begin
      vContour.CenterDrill := vArcR^.Center;
      vContour.ShapeType := cshCircular;
    end
    else
      if vContour.GPolyline.PointCount = 2 then
      begin
        if IsClosed(vPolyline) then
          vContour.ShapeType := cshPoint
        else
          vContour.ShapeType := cshLinear
      end
      else
      begin
        vContour.ShapeType := cshFree;
        if (MachineType = mtDispenser) then
        begin
          vFP := vPolyline.PolyPoints.First;
          vLP := vPolyline.PolyPoints.Last;
          if FMergePoints and not IsEqualFPoints(vFP, vLP) then
          begin
            if DistanceF2DPoint(vFP.Point2D, vLP.Point2D) < FMergePointsRadius then
              vPolyline.PolyPoints.Last := vFP;
          end;
        end;
      end;
    if vCounter = AStep then
    begin
      Inc(vFactor);
      vCounter := 0;
      Progress(psRunning, vFactor + cnstCommonPointsPercent, '');
    end;
    if Assigned(vArcR) then
      Dispose(vArcR);
  end;
end;

function TsgCADtoGCode.GeneratePocketComponents(const AContour: TObject;
  APocket: TsgGPocket): Boolean;
var
  vContour: TsgGContour absolute AContour;

  function MakeTransition(const APoly1, APoly2: TsgDXFPolyline; AType: TsgTransitionType): TsgTransition;
  begin
    Result := TsgTransition.Create(AType);
    Result.Poly1 := APoly1;
    Result.Poly2 := APoly2;
  end;

  function GetPocketContoursByPoly(const APoly: TsgDXFPolyline; AName: string): Boolean;
  {$IFDEF SG_TransformAnalogy}
  var
    I: Integer;
    vEqEnts: TsgList;
    vRadius: Double;
    vContour: TsgGContour;
    vEqCreator: TsgAnalogyCreator;
  begin
    Result := False;
    vEqEnts := TsgList.Create;
    if APocket.Components.Count = 0 then
      vRadius := APocket.ToolParams.Radius
    else
      vRadius := APocket.ToolParams.Radius * (100 - APocket.OverLapPercent) / 100;
    try
      vEqCreator := TsgAnalogyCreator.Create(Converter);
      try
        vEqCreator.EditingBlock := EditingBlock;
        vEqCreator.MakeEquidistant(APoly, vRadius, eqtInside, vEqEnts);
      finally
        vEqCreator.Free;
      end;
      for I := 0 to vEqEnts.Count - 1 do
      begin
        ConvertPolyToGPolyInList(vEqEnts, I);
        Converter.Loads(vEqEnts[I]);
        if APocket.Components.Count > 0 then
        begin
          if I = 0 then
            APocket.Components.Add(MakeTransition(APoly, vEqEnts[0], ttInternal))
          else
            APocket.Components.Add(MakeTransition(vEqEnts[I - 1], vEqEnts[I], ttExternal));
        end;
        vContour := MakeContourFromPolyline(vEqEnts[I], AName, APocket.SourceContour.LayerName);
        APocket.Components.Add(vContour);
        GetPocketContoursByPoly(vContour.GPolyline, AName);
      end;
    finally
      FreeAndNil(vEqEnts);
    end;
  {$ELSE}
  begin
    Result := False;
  {$ENDIF}
  end;

var
  vName: string;
begin
  GetPocketContoursByPoly(vContour.GPolyline, vName);
  Result := APocket.Components.Count > 0;
end;

procedure TsgCADtoGCode.MakeContours;
var
  vCnt, vStep: Integer;
begin
  vCnt := FEntityList.Count;
  vStep := Round(vCnt / (cnstEndPercent - cnstCommonPointsPercent));
  Progress(psRunning, cnstCommonPointsPercent, '');
  GenerateContoursFromPolylines(FEntityList, vStep);
end;

procedure TsgCADtoGCode.ExcludeDublicatesEntities;

  function ComparePolylines(const AEnt, AEnt1: TsgDXFEntity): Boolean;
  var
    vRevers: Boolean;
    vPoly: TsgDXFPolyline absolute AEnt;
    vPoly1: TsgDXFPolyline absolute AEnt1;
    I, J, vMaxInd: Integer;
  begin
    Result := False;
    if (vPoly <> vPoly1) and (vPoly.Count = vPoly1.Count) then
    begin
      Result := True;
        vRevers := False;
      I := 0;
      vMaxInd := vPoly.Count - 1;
      while I < vMaxInd + 1 do
        begin
          if vRevers then
          J := vMaxInd - I
          else
          J := I;
        if not IsEqualVertex(vPoly.Vertexes[I], vPoly1.Vertexes[J], FGCodeResolution) then
          begin
            if not vRevers then
            begin
              vRevers := True;
            I := 0;
              Continue;
            end
            else
            begin
            Result := False;
              Break;
            end;
          end
          else
          Inc(I);
        end;
    end;
  end;

  function CompareArcs(const AEnt, AEnt1: TsgDXFEntity): Boolean;
  var
    vArc: TsgDXFGArc absolute AEnt;
    vArc1: TsgDXFGArc absolute AEnt1;
        begin
    Result := False;
    if vArc <> vArc1 then
      Result := IsEqualArcR(vArc.Arc, vArc1.Arc);
  end;

var
  vDuplicate: Boolean;
  I, J: Integer;
  vEnt, vEnt1: TsgDXFEntity;
begin
  I := 0;
  while I < FEntityList.Count do
  begin
    vEnt := TsgDXFPolyline(FEntityList[I]);
    for J := 0 to FEntityList.Count - 1 do
    begin
      vEnt1 := TsgDXFPolyline(FEntityList[J]);
      if (vEnt is TsgDXFPolyline) and (vEnt1 is TsgDXFPolyline) then
        vDuplicate := ComparePolylines(vEnt, vEnt1)
      else
        if (vEnt is TsgDXFGArc) and (vEnt1 is TsgDXFGArc) then
          vDuplicate := CompareArcs(vEnt, vEnt1)
        else
          vDuplicate := False;
      if vDuplicate then
      begin
          if Assigned(EditingBlock) then
            EditingBlock.RemoveEntity(FEntityList[J]);
          TsgDXFEntity(FEntityList[J]).Free;
          FEntityList.Delete(J);
          Dec(I);
          Break;
        end;
      end;
    Inc(I);
  end;
  Converter.Loads(CurrentLayout);
end;

function TsgCADtoGCode.FindCommonDataByEdgePolyPoint(const APoly, APoly1: TsgCADBasePolyline;
  var ACommonData: TsgCommonDataPoint): Boolean;

  function GetAngleByPoints(AMidPts, APt1, APt2: TFPoint): Double;
  var
    vLine, vLine1: TsgLine;
  begin
    vLine := MakeLine(APt1, AMidPts);
    vLine1 := MakeLine(AMidPts, APt2);
    Result := GetAngleOfLines(vLine, vLine1);
    if Result = 0 then
      Result := 180;
  end;

var
  vStPt, vEndPt, vStPt1, vEndPt1: TFPoint;
begin
  Result := False;
  ACommonData.Poly := APoly;
  ACommonData.Poly1 := APoly1;
  vStPt := APoly.PolyPoints.First;
  vEndPt := APoly.PolyPoints.Last;
  vStPt1 := APoly1.PolyPoints.First;
  vEndPt1 := APoly1.PolyPoints.Last;

  if IsEqualFPoints(vStPt, vStPt1, FGCodeResolution) or
    IsEqualFPoints(vStPt, vEndPt1, FGCodeResolution) then
  begin
    Result := True;
    ACommonData.Point := vStPt;
    ACommonData.IndexCommnonPtPoly := 0;
    if IsEqualFPoints(vStPt, vEndPt1, FGCodeResolution) then
    begin
      ACommonData.Angle := GetAngleByPoints(vStPt, APoly.PolyPoints[1],
        APoly1.PolyPoints[APoly1.PointCount - 2]);
      ACommonData.IndexCommnonPtPoly1 := APoly1.Count - 1
    end
    else
    begin
      ACommonData.Angle := GetAngleByPoints(vStPt, APoly.PolyPoints[1],
        APoly1.PolyPoints[1]);
      ACommonData.IndexCommnonPtPoly1 := 0;
    end;
  end
  else
    if IsEqualFPoints(vEndPt, vStPt1, FGCodeResolution) or
      IsEqualFPoints(vEndPt, vEndPt1, FGCodeResolution) then
    begin
      Result := True;
      ACommonData.Point := vEndPt;
      ACommonData.IndexCommnonPtPoly := APoly.Count - 1;
      if IsEqualFPoints(vEndPt, vEndPt1, FGCodeResolution) then
      begin
        ACommonData.Angle := GetAngleByPoints(vEndPt, APoly.PolyPoints[APoly.PointCount - 2],
          APoly1.PolyPoints[APoly1.PointCount - 2]);
        ACommonData.IndexCommnonPtPoly1 := APoly1.Count - 1
      end
      else
      begin
        ACommonData.Angle := GetAngleByPoints(vEndPt, APoly.PolyPoints[APoly.PointCount - 2],
          APoly1.PolyPoints[1]);
        ACommonData.IndexCommnonPtPoly1 := 0;
      end;
    end;
end;

procedure TsgCADtoGCode.ExcludeCommonVertexes;

type
  TsgCommonDataVertex = record
    Angle: Double;
    Vertex: TsgDXFVertex;
    Vertex1: TsgDXFVertex;
    Poly: TsgDXFPolyline;
    Poly1: TsgDXFPolyline;
  end;

  TsgCommonPointType = (cptNone, cptStart, cptStartEnd, cptEnd, cptEndStart);

  function GetAngleByPoints(AMidPts, APt1, APt2: TFPoint): Double;
  var
    vLine, vLine1: TsgLine;
  begin
    vLine := MakeLine(APt1, AMidPts);
    vLine1 := MakeLine(AMidPts, APt2);
    Result := GetAngleOfLines(vLine, vLine1);
    if Result = 0 then
      Result := 180;
  end;

  function FindCommonPointType(const ASt, ASt1, AEnd, AEnd1: TFPoint): TsgCommonPointType;
  begin
    Result := cptNone;
    if IsEqualFPoints(ASt, ASt1, FGCodeResolution) then
      Result := cptStart
    else
      if IsEqualFPoints(ASt, AEnd1, FGCodeResolution) then
        Result := cptStartEnd
      else
        if IsEqualFPoints(AEnd, ASt1, FGCodeResolution) then
          Result := cptEndStart
        else
          if IsEqualFPoints(AEnd, AEnd1, FGCodeResolution) then
            Result := cptEnd
  end;

  function FindCommonPolyVertexPt(const APoly, APoly1: TsgDXFPolyline): TsgCommonPointType;
  begin
    Result := FindCommonPointType(APoly.Vertexes[0].Point, APoly1.Vertexes[0].Point,
      APoly.Vertexes[APoly.Count - 1].Point, APoly1.Vertexes[APoly1.Count - 1].Point);
  end;

  function FindCommonVertex(const APoly, APoly1: TsgDXFPolyline; var ACommonData: TsgCommonDataVertex): Boolean;
  var
    vStVer, vEndVer, vStVer1, vEndVer1: TsgDXFVertex;
    vCommPtType: TsgCommonPointType;
  begin
    Result := True;
    ACommonData.Poly := APoly;
    ACommonData.Poly1 := APoly1;
    vStVer := APoly.Vertexes[0];
    vEndVer := APoly.Vertexes[APoly.Count - 1];
    vStVer1 := APoly1.Vertexes[0];
    vEndVer1 := APoly1.Vertexes[APoly1.Count - 1];
    vCommPtType := FindCommonPolyVertexPt(APoly, APoly1);
    case vCommPtType of
      cptStart:
        begin
          ACommonData.Vertex := vStVer;
          ACommonData.Vertex1 := vStVer1;
          ACommonData.Angle := GetAngleByPoints(vStVer.Point, APoly.PolyPoints[1],
            APoly1.PolyPoints[1]);
        end;
      cptStartEnd:
        begin
          ACommonData.Vertex := vStVer;
          ACommonData.Vertex1 := vEndVer1;
          ACommonData.Angle := GetAngleByPoints(vStVer.Point, APoly.PolyPoints[1],
            APoly1.PolyPoints[APoly1.PointCount - 2]);

        end;
      cptEnd:
        begin
          ACommonData.Vertex := vEndVer;
          ACommonData.Vertex1 := vEndVer1;
          ACommonData.Angle := GetAngleByPoints(vEndVer.Point, APoly.PolyPoints[APoly.PointCount - 2],
            APoly1.PolyPoints[APoly1.PointCount - 2]);
        end;
      cptEndStart:
        begin
          ACommonData.Vertex := vEndVer;
          ACommonData.Vertex1 := vStVer1;
          ACommonData.Angle := GetAngleByPoints(vEndVer.Point, APoly.PolyPoints[APoly.PointCount - 2],
            APoly1.PolyPoints[1]);
        end;
    else
      Result := False;
    end;
  end;

  procedure RemoveEntity(const AEnt: TsgDXFEntity);
  begin
    if Assigned(EditingBlock) then
      EditingBlock.RemoveEntity(AEnt);
    FEntityList.Delete(FEntityList.IndexOf(AEnt));
    AEnt.Free;
  end;

  procedure CopyVertexToPoly(const APoly: TsgDXFPolyline; const APt: TFPoint; AIdx: Integer);
  begin
    if AIdx = 0 then
      InsertVertexInPolyline(APoly, 0, APt)
    else
      AddVertexInPolyline(APoly, APt);
  end;

  procedure UnitPolylines(const ACommonData: TsgCommonDataVertex);
  var
    vInd, vInd1: Integer;
    vAdding, vForward, vHaveBugleFix: Boolean;
  begin
    vInd := ACommonData.Poly.IndexOfEntity(ACommonData.Vertex);
    vInd1 := ACommonData.Poly1.IndexOfEntity(ACommonData.Vertex1);
    vForward := vInd1 = 0;
    vAdding := not (vInd = 0);
    if vAdding then
    begin
      if vForward then
        vHaveBugleFix := ACommonData.Vertex1.HasBulge
      else
        vHaveBugleFix := ACommonData.Poly1.Vertexes[vInd1 - 1].HasBulge;
      if vHaveBugleFix then
        ACommonData.Poly.DeleteEntity(ACommonData.Poly.Count - 1);
    end;
    TsgDXFPolylineAccess(ACommonData.Poly).CopyVertexesExt(ACommonData.Poly1, vAdding, vForward);
    TsgDXFPolylineAccess(ACommonData.Poly).DeleteDuplicateVertexes(-1);
    if ACommonData.Poly.Count > 1 then
    begin
      Converter.Loads(ACommonData.Poly);
      RemoveEntity(ACommonData.Poly1);
    end
    else
      RemoveEntity(ACommonData.Poly);
  end;

  function GetEntTypes(AEnt, AEnt1: TsgDXFEntity): TsgCADBaseEntTypes;
  var
    vEntIsPoly: Boolean;
  begin
    Result := ctPolys;
    vEntIsPoly := AEnt.EntType in [cePolyline, ceLWPolyline];
    case AEnt1.EntType of
      cePolyline, ceLWPolyline:
        begin
          if vEntIsPoly then
            Result := ctPolys
          else
            Result := ctArcPoly;
        end;
      ceArc:
        begin
          if vEntIsPoly then
            Result := ctPolyArc
          else
            Result := ctArcs;
        end;
    end;
  end;

  function AddVertexInPoly(const APoly: TsgDXFPolyline; const ArcR: TsgArcR; const APoint: TFPoint; AIndex: Integer): TsgDXFVertex;
  begin
    Result := TsgDXFVertex.Create;
    Result.Point := APoint;
    Result.Bulge := GetBulgeOfArcR(ArcR, APoint);
    APoly.InsertEntity(AIndex, Result);
//    APoly.AddEntity(Result);
  end;

  function GetOppositePointOnArc(const ArcR: TsgArcR; const APoint: TFPoint): TFPoint;
  var
    vStPt, vEndPt: TFPoint;
  begin
    vStPt := GetPointOnCircle(ArcR.Center, ArcR.Radius, ArcR.AngleS);
    vEndPt := GetPointOnCircle(ArcR.Center, ArcR.Radius, ArcR.AngleE);
    if IsEqualFPoints(vStPt, APoint) then
      Result := vEndPt
    else
      Result := vStPt;
  end;

  function MakeNewPolyFromArcs(const ACommonDataPt: TsgCommonDataPoint): TsgGCodePolyline;
  var
    vArcR: TsgArcR;
  begin
    Result := TsgGCodePolyline.CreateEx(TsgDXFGArc(ACommonDataPt.Poly).LayerName);
    vArcR := TsgDXFArc(ACommonDataPt.Poly).Arc;
    AddVertexInPoly(Result, vArcR, GetOppositePointOnArc(vArcR, ACommonDataPt.Point), Result.Count);
    vArcR := TsgDXFArc(ACommonDataPt.Poly1).Arc;
    AddVertexInPoly(Result, vArcR, ACommonDataPt.Point,  Result.Count);
    AddVertexInPolyline(Result, GetOppositePointOnArc(vArcR, ACommonDataPt.Point));
    LoadNewEntity(Result, FEntityList);
  end;

  function MergePolyArc(const APoly, APoly1: TsgCADBasePolyline; AComDataPt: TsgCommonDataPoint): TsgDXFPolyline;
  var
    vPoly: TsgDXFPolyline;
    vArc: TsgDXFGArc;
    vArcR: TsgArcR;
    vBulge: Double;
    vOP: TFPoint;
    vEntTypes: TsgCADBaseEntTypes;
  begin
    Result := nil;
    vEntTypes := GetEntTypes(APoly, APoly1);
    case vEntTypes of
      ctPolyArc:
        begin
          vPoly := TsgDXFPolyline(APoly);
          vArc := TsgDXFGArc(APoly1);
        end;
      ctArcPoly:
        begin
          vPoly := TsgDXFPolyline(APoly1);
          vArc := TsgDXFGArc(APoly);
        end
    else
      Exit;
    end;
    Result := vPoly;
    vArcR := TsgDXFGArc(vArc).Arc;
    vOP := GetOppositePointOnArc(vArcR, AComDataPt.Point);
    if AComDataPt.IndexCommnonPtPoly = 0 then
    begin
      vBulge := GetBulgeOfArcR(vArcR, vOP);
      InsertVertexInPolyline(TsgDXFPolyline(vPoly), 0, vOP, vBulge);
    end
    else
    begin
      vBulge := GetBulgeOfArcR(vArcR, AComDataPt.Point);
      TsgDXFPolyline(vPoly).Vertexes[vPoly.Count - 1].Bulge := vBulge;
      AddVertexInPolyline(TsgDXFPolyline(vPoly), vOP);
    end;
    LoadNewEntity(vPoly, FEntityList, False);
    RemoveEntity(vArc);
  end;

var
  vPoly, vPoly1: TsgCADBasePolyline;
  I, J, Cnt, vCounter, vFactor, vStep: Integer;
  vComDataVer, vPrevComDataVer: TsgCommonDataVertex;
begin
  I := 0;
  vCounter := 0;
  vFactor := 0;
  vStep := Round(FEntityList.Count / cnstCommonPointsPercent - cnstDublicatePercent);
  while I < FEntityList.Count do
  begin
    if FBreakGenerate then
      Break;
    Inc(vCounter);
    Cnt := FEntityList.Count - 1;
    vPoly := TsgCADBasePolyline(FEntityList[I]);
    if not IsClosed(vPoly, FGCodeResolution) then
    begin
      if StraightenContour then
        vPrevComDataVer.Angle := 0;
      for J := 0 to Cnt do
      begin
        if FBreakGenerate then
          Break;
        vPoly1 := TsgCADBasePolyline(FEntityList[J]);
        if (vPoly <> vPoly1) and not IsClosed(vPoly1) then
        begin
          if StraightenContour then
          begin
            if FindCommonVertex(TsgDXFPolyline(vPoly), TsgDXFPolyline(vPoly1), vComDataVer) then
              if (vPrevComDataVer.Angle) < (vComDataVer.Angle) then
              begin
                vPrevComDataVer := vComDataVer;
                if vPrevComDataVer.Angle = 180 then
                  Break;
              end;
          end
          else
            if FindCommonVertex(TsgDXFPolyline(vPoly), TsgDXFPolyline(vPoly1), vComDataVer) then
            begin
              UnitPolylines(vComDataVer);
              Dec(I);
              Break;
            end;
        end;
      end;
      if StraightenContour and (vPrevComDataVer.Angle <> 0) then
      begin
        UnitPolylines(vPrevComDataVer);
        Dec(I);
      end;
    end;
    if vCounter = vStep then
    begin
      Inc(vFactor);
      vCounter := 0;
      Progress(psRunning, vFactor + 10, '');
    end;
    Inc(I);
  end;
end;

procedure TsgCADtoGCode.GetContours;
var
  vRotMatrix: TFMatrix;
begin
  vRotMatrix := CurrentLayout.RotMatrix;
  CurrentLayout.SetRotMatrix(cnstIdentityMat);
  if not Assigned(CADImage) then Exit;
  FBreakGenerate := False;
  if (FContourList.Count > 0) then
  begin
    FreeList(FContourList);
    FContourList := TList.Create;
  end;
  Progress(psStarting, cnstStartPercent, '');
  CADImage.Converter.Params := InitializeIterateParams;
  CADImage.Converter.AutoInsert := True;
  Progress(psRunning, 5, '');
  CurrentLayout.Iterate(Converter, ExportProc, FinishExportProc);
  Progress(psRunning, cnstIteratePercent, '');
  ExcludeDublicatesEntities;
  Progress(psRunning, cnstDublicatePercent, '');
  ExcludeCommonVertexes;
  Progress(psRunning, cnstCommonPointsPercent, '');
  MakeContours;

  if not FBreakGenerate then
    SortContours;
  if FMachineType = mtDispenser then
    ReinitStartingPoints;
  CADImage.GetExtents;
  Progress(psEnding, cnstEndPercent, '');
  CurrentLayout.SetRotMatrix(vRotMatrix);
end;

function TsgCADtoGCode.MakeGArcByParam(AArcR: TsgArcR): TsgDXFGArc;
begin
  Result := TsgDXFGArc.Create;
  Result.Arc := AArcR;
  Converter.Loads(Result);
end;

procedure TsgCADtoGCode.ReadLeads(AContour: TsgGContour; var APolyline: TsgDXFPolyline);

  procedure AddLineLeadEntity(const ALeadEnt: TsgDXFPolyline; APurpose: TsgGLeadPurpose);
  begin
    if not Assigned(ALeadEnt) then
      Exit;
    if APurpose = glpIn then
      InsertVertexInPolyline(APolyline, 0, ALeadEnt.Vertexes[0].Point)
    else
      InsertVertexInPolyline(APolyline, APolyline.Count, ALeadEnt.Vertexes[0].Point);
  end;

  procedure AddLeadPts(ALead: TsgGLead);
  var
    vLeadPoly: TsgDXFPolyline;
  begin
    vLeadPoly := ALead.Entity;
    if Assigned(vLeadPoly) then
    begin
      if not vLeadPoly.Vertexes[0].HasBulge then
        AddLineLeadEntity(vLeadPoly, ALead.Purpose)
      else
      begin
        case ALead.Purpose of
          glpIn:
            begin
              if vLeadPoly.PolyPoints.IndexOf(ALead.PtEdge) = 0 then
                InsertVertexInPolyline(APolyline, 0, vLeadPoly.Vertexes[1].Point, -vLeadPoly.Vertexes[0].Bulge)
              else
                InsertVertexInPolyline(APolyline, 0, vLeadPoly.Vertexes[0].Point, vLeadPoly.Vertexes[0].Bulge);
            end;
          glpOut:
            begin
              if vLeadPoly.PolyPoints.IndexOf(ALead.PtEdge) = 0 then
              begin
                APolyline.Vertexes[APolyline.Count - 1].Bulge := vLeadPoly.Vertexes[0].Bulge;
                InsertVertexInPolyline(APolyline, APolyline.Count, vLeadPoly.Vertexes[1].Point, vLeadPoly.Vertexes[1].Bulge);
              end
              else
              begin
                APolyline.Vertexes[APolyline.Count - 1].Bulge := -vLeadPoly.Vertexes[0].Bulge;
                AddVertexInPolyline(APolyline, vLeadPoly.Vertexes[0].Point);
              end;
            end;
        end;
      end;
    end;
  end;

var
  vLead: TsgGLead;
begin
  vLead := AContour.LeadIn;
  AddLeadPts(vLead);
  AddLineLeadEntity(vLead.SubEntity, vLead.Purpose);
  vLead := AContour.LeadOut;
  AddLeadPts(vLead);
  AddLineLeadEntity(vLead.SubEntity, vLead.Purpose);
end;

procedure GetVertexesList(const APolyline: TsgGCodePolyline; const AVertexes: TList);
var
  I: Integer;
begin
  for I := 0 to APolyline.Count - 1 do
    AVertexes.Add(APolyline.Vertexes[I]);
end;

procedure TsgCADtoGCode.GetConvertbleContours(const AContours: TList);
var
  I, J: Integer;
  vContour: TsgGContourAccess;
begin
  for I := 0 to FContourList.Count - 1 do
  begin
    vContour := FContourList[I];
    if not vContour.Enable then
      Continue;
    if Assigned(vContour.RadiusCompGRBL) then
    begin
      for J := 0 to vContour.RadiusCompGRBL.ContourCount - 1 do
        AContours.Add(vContour.RadiusCompGRBL.Contours[J]);
    end
    else
      AContours.Add(vContour);
  end;
end;

procedure TsgCADtoGCode.Convert;
var
  vConvertibleContours: TList;
  I, J, vMaxProgressCount, vCurProgress: Integer;
begin
    FGCode.Clear;
    Progress(psStarting, 0, '');
    vMaxProgressCount := 0;
    FPointProgress := 0;
  FPrevAbsoluteZ := 0;
    WriteGCodeCommand(nil, opStart);
    FPrevPointInit := False;
  vConvertibleContours := TList.Create;
  try
    GetConvertbleContours(vConvertibleContours);
    for I := 0 to vConvertibleContours.Count - 1 do
    if FArcToLines then
        vMaxProgressCount := vMaxProgressCount + TsgGContour(vConvertibleContours[I]).GPolyline.PointCount
    else
        vMaxProgressCount := vMaxProgressCount + TsgGContour(vConvertibleContours[I]).GPolyline.Count;
    for I := 0 to vConvertibleContours.Count - 1 do
    begin
      FCurContour := vConvertibleContours[I];
      FCurPoly := TsgDXFPolyline.Create;
    try
      FCurPoly.AssignEntity(FCurContour.GPolyline);
        ReadLeads(FCurContour, FCurPoly);
      FMaxIndex := FCurPoly.Count - 1;
      FPassesCounter := 0;
        for J := 1 to FCurContour.NumbOfPassesByMachine do
      begin
        Inc(FPassesCounter);
        FVertexIndex := 0;
        if J > 1 then
        begin
          if (FPassesDirection = pdAlternate) and (J > 1) then
              ReversePolyline(FCurPoly);
        end;
        while FVertexIndex < FMaxIndex + 1 do
        begin
          FCurrentVertex := FCurPoly.Vertexes[FVertexIndex];
            if FVertexIndex > 0 then
          begin
  //            FCurrentVertex.Point.Z := 0;
            if (FVertexIndex = FMaxIndex) and (FCurContour.RadiusCompType <> grcNone) then
              WriteGCodeCommand(FCurPoly.Vertexes[FMaxIndex], opRadiusCompOff)
            else
                SelectGCodeCommand(FVertexIndex);
          end
          else
            if FVertexIndex = 0 then
            begin
              WriteGCodeCommand(FCurPoly.Vertexes[0], opStartContour);
              if FCurContour.RadiusCompType <> grcNone then
              begin
                WriteGCodeCommand(FCurPoly.Vertexes[1], opRadiusCompOn);
                Inc(FVertexIndex);
              end;
            end;
          if FVertexIndex = FMaxIndex then
            WriteGCodeCommand(nil, opEndContour);
          Inc(FVertexIndex);
          Inc(FPointProgress);
          vCurProgress := Round(100 * FPointProgress / vMaxProgressCount);
          if vCurProgress > 5 then
            Progress(psRunning, vCurProgress, '');
        end;
      end;
    finally
      FreeAndNil(FCurPoly);
    end;
  end;
  finally
    FreeAndNil(vConvertibleContours);
  end;
  FFirstMove := True;
    WriteGCodeCommand(nil, opEndProgram);
    if (FGCodeCount > 0) and (FGCode.Count > FGCodeCount) then
      FGCode.Add('(The complete set of commands in the full version Export GCode)');
    Progress(psEnding, 100, '');
end;

procedure TsgCADtoGCode.SetUnitMatrix;
var
  vKoef: Double;
begin
  vKoef := cnstUnitsKoef[FCADUnits] / cnstUnitsKoef[FMachineUnits];
  FUnitMatrix := FMatByScale(vKoef);
end;

procedure TsgCADtoGCode.SetDrawingUnits(AValue: TsgGUnits);
begin
  FCADUnits := AValue;
  SetUnitMatrix;
end;

procedure TsgCADtoGCode.SetEditingBlock(const Value: TsgDXFBlock);
begin
  FEditingBlock := Value;
end;

procedure TsgCADtoGCode.SetMachineType(const Value: TsgGCodeMachineTypeID);
var
  I: Integer;
begin
  if (FContourList.Count > 0) and (Value = mtDispenser) and (FMachineType <> Value) then
    ReinitStartingPoints;
  FMachineType := Value;
  for I := 0 to FContourList.Count - 1 do
    TsgGContourAccess(FContourList[I]).MachineType := FMachineType;
  if (Value = mtGRBLRouter) or (Value = mtGRBLLaser) then
    cnstGCommands := cnstGCommandsGRBL
  else
    cnstGCommands := cnstGCommandsConventional;
end;

procedure TsgCADtoGCode.SetMachineUnits(AValue: TsgGUnits);
begin
  FMachineUnits := AValue;
  SetUnitMatrix;
end;

procedure TsgCADtoGCode.ResetImageExternal;
begin
  if Assigned(CADImage) then
  begin
    CADImage.Converter.NumberOfPartsInCircle := FOldNumberOfPartsInCircle;
    CADImage.Converter.NumberOfPartsInSpline := FOldNumberOfPartsInSpline;
    CADImage.Converter.RegenerateArcs := FRegenerateArcs;
  end;
  SetImage(nil);
end;

procedure TsgCADtoGCode.SetPrecision(const ADigits, AFactor: Byte);
begin
  FPrecisionDigits := ADigits;
  FPrecisionFactor := AFactor;
  FGCodeResolution := sgPower10(1, -FPrecisionDigits);
end;

procedure TsgCADtoGCode.ReinitStartingPoints;

  procedure DoProgress(Stage: TProgressStage; ACurPositon: Integer);
  const
    R: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  var
    vPercentDone: Byte;
    vPercentDoneE: Extended;
  begin
    if not Assigned(OnProgress) then
      Exit;
    vPercentDoneE := ACurPositon / FContourList.Count;
    if vPercentDoneE > 1 then vPercentDoneE := 1;
    vPercentDone := Round(vPercentDoneE * 100);
    Progress(Stage, vPercentDone, 'Reinit start point');
  end;

//  function GetCrossPoints(const APoly: TsgGCodePolyline; ACrosses: TFPointList): Integer;
//  var
//    I: Integer;
//    vPoly2: TsgGCodePolyline;
//  begin
//    Result := 0;
//    for I := 0 to FContourList.Count - 1 do
//    begin
//      vPoly2 := TsgGContour(FContourList[I]).GPolyline;
//      if APoly = vPoly2 then
//        Continue;
//      Result := IsCrossPolylines(APoly.PolyPoints, False, vPoly2.PolyPoints, False,
//        ACrosses, 0, False, FGCodeResolution);
//    end;
//  end;

  function GetMinimalAngleBetweenPts(const APoly: TsgGCodePolyline): tsgMinimalAngleData;
  var
    I: Integer;
    vAngle: Double;
    vLine1, vLine2: TsgLine;
  begin
    Result.Angle := 360;
    Result.Point := cnstBadPoint;
    Result.PointIndex := 0;
    I := 0;
    while I < APoly.PointCount - 1 do
    begin
      vLine1 := MakeLine(APoly.Points[I], APoly.Points[I + 1]);
      if I = APoly.PointCount - 2 then
        vLine2 := MakeLine(APoly.Points[I + 1], APoly.Points[1])
      else
        vLine2 := MakeLine(APoly.Points[I + 1], APoly.Points[I + 2]);
      vAngle := GetAngleOfLines(vLine1, vLine2);
      if vAngle < Result.Angle then
      begin
        Result.Angle := vAngle;
        Result.Point := APoly.Points[I + 1];
        Result.PointIndex := I + 1;
      end;
      Inc(I);
    end;
  end;

var
  I: Integer;
  vCrossPoints: TFPointList;
  vPoly: TsgGCodePolyline;
  vMinAngleData: tsgMinimalAngleData;
begin
  DoProgress(psStarting, 0);
  for I := 0 to FContourList.Count - 1 do
  begin
    vPoly := TsgGContour(FContourList[I]).GPolyline;
    if vPoly.IsClosed then
    begin
      vCrossPoints := TFPointList.Create;
      try
//        GetCrossPoints(vPoly, vCrossPoints);
        vMinAngleData := GetMinimalAngleBetweenPts(vPoly);
        if vMinAngleData.Angle < 90 then
          TsgGContour(FContourList[I]).ChangeStartPoint(vPoly, vMinAngleData.Point, [osEndPt])
        else
        begin
          if vCrossPoints.Count > 0 then
            TsgGContour(FContourList[I]).ChangeStartPoint(vPoly, vCrossPoints[0], [osEndPt])
          else
            if vMinAngleData.Angle <> 360 then
              TsgGContour(FContourList[I]).ChangeStartPoint(vPoly, vMinAngleData.Point, [osEndPt]);
        end;
      finally
        FreeAndNil(vCrossPoints);
      end;
      DoProgress(psRunning, I + 1);
    end;
  end;
  DoProgress(psEnding, FContourList.Count);
end;

procedure TsgCADtoGCode.SetDistanseOffset(AStatus: Boolean; AOffset: Double);
var
  I: Integer;
  vOffset: Double;
begin
  if FTurnOffPumpBefore <> AStatus then
  begin
    FTurnOffPumpBefore := AStatus;
    vOffset := -1;
    if not FTurnOffPumpBefore then
    begin
      FTurnOffPumpBeforeValue := 0;
      vOffset := 0;
    end
    else
    begin
      if AOffset <> FTurnOffPumpBeforeValue then
      begin
        FTurnOffPumpBeforeValue := AOffset;
        vOffset := AOffset;
      end;
    end;
    if vOffset > -1 then
      for I := 0 to FContourList.Count - 1 do
        TsgGContourAccess(FContourList[I]).TurnOffPumpDist := vOffset;
  end;
end;

procedure TsgCADtoGCode.SetTools(const AValue: string);
var
  I: Integer;
begin
  FTools.Clear;
{$IFDEF SGDEL_7}
  FTools.Delimiter := cnstGToolsSeparator;
  FTools.DelimitedText := AValue;
{$ELSE}
  FTools.LineBreak := cnstGParamsSeparator;
  FTools.Text := AValue;
{$ENDIF}
  if FTools.Count > 0 then
    for I := 0 to FContourList.Count - 1 do
      TsgGContourAccess(FContourList[I]).SetTools(FTools);
end;

procedure TsgCADtoGCode.SetDepthAndPasses(const AParams: TsgExportParams);
var
  I: Integer;
begin
  FDepthOnZ := AParams.DepthOnZ;
  FDepthPass := AParams.DepthPass;
  FNumbOfPasses := AParams.NumbOfPasses;
  for I := 0 to FContourList.Count - 1 do
    if not TsgGContour(FContourList[I]).GPolyline.IsPocket then
      InitContourDepthAndPasses(TsgGContour(FContourList[I]), AParams.DepthOnZ,
      AParams.DepthPass, AParams.NumbOfPasses);
end;

var
  Paths: TsgStringList;

{ TsgGCodeManager }

procedure TsgGCodeManager.Convert;
begin
  FGCodeExport.Convert;
end;

constructor TsgGCodeManager.Create(const AImage: TsgCADImage);
begin
  inherited Create;
  FGCodeExport := TsgCADtoGCode.Create(AImage);
end;

procedure AddEntityToBlock(const AConv: TsgDXFConverter;
  const ABlock: TsgDXFEntity; const AEnt: TsgDXFEntity);
begin
  AConv.Loads(AEnt);
  ABlock.AddEntity(AEnt);
end;

procedure TsgGCodeManager.CreateWorkpieceZeroPoint(const ABlock: TsgDXFBlock);
const
  cnstBlockCreateWorkpieceZeroPoint = 'sgCWZP';
  cnstLineSize = 0.7;
  cnstRadius  = 0.5;

  procedure AddHatchSector(const AHatch: TsgCADCurvePolygon;
    const APart: Integer);
  var
    vLine2D: Tsg2DLine;
    vArc2D: Tsg2DArc;
    vBoundary: Tsg2DBoundaryList;
    vAngle: Double;
    vPStart, vPEnd: TF2DPoint;
  begin
    case APart of
      0:
        begin
          vAngle := 0;
          vPStart := MakeF2DPoint(cnstRadius, 0);
          vPEnd := MakeF2DPoint(0, cnstRadius);
        end;
      1:
        begin
          vAngle := 90;
          vPStart := MakeF2DPoint(0, cnstRadius);
          vPEnd := MakeF2DPoint(-cnstRadius, 0);
        end;
      2:
        begin
          vAngle := 180;
          vPStart := MakeF2DPoint(-cnstRadius, 0);
          vPEnd := MakeF2DPoint(0, -cnstRadius);
        end;
      3:
        begin
          vAngle := 270;
          vPStart := MakeF2DPoint(0, -cnstRadius);
          vPEnd := MakeF2DPoint(cnstRadius, 0);
        end;
    else
      Exit;
    end;
    vBoundary := AHatch.AddBoundaryList;
    vLine2D := Tsg2DLine.Create;
    vBoundary.Add(vLine2D);
    vLine2D.SetEndPoint(vPStart);
    vArc2D := Tsg2DArc.Create;
    vBoundary.Add(vArc2D);
    vArc2D.Radius := cnstRadius;
    vArc2D.StartParam := vAngle;
    vArc2D.EndParam := vAngle + 90;
    vArc2D.CounterClockWise := True;
    vLine2D := Tsg2DLine.Create;
    vBoundary.Add(vLine2D);
    vLine2D.SetStartPoint(vPEnd);
  end;

var
  vConverter: TsgDXFConverter;
  vBlock: TsgDXFBlock;
  vCircle: TsgDXFCircle;
  vLine: TsgDXFLine;
  vHatch: TsgCADCurvePolygon;
  vInsert: TsgDXFInsert;
begin
  vConverter := FGCodeExport.CADImage.Converter;
//  GCodeExport.WorkpieceZeroPoint;
  vBlock := vConverter.BlockByName(cnstBlockCreateWorkpieceZeroPoint);
  if not Assigned(vBlock) then
  begin
    vBlock := TsgDXFBlock.Create;
    try
      vBlock.Name := cnstBlockCreateWorkpieceZeroPoint;
      vConverter.Sections[csBlocks].AddEntity(vBlock);
      vCircle := TsgDXFCircle.Create;
      vCircle.Radius := cnstRadius;
      AddEntityToBlock(vConverter, vBlock, vCircle);
      vLine := TsgDXFLine.Create;
      vLine.Point1 := MakeFPoint(0, cnstLineSize);
      AddEntityToBlock(vConverter, vBlock, vLine);
      vLine := TsgDXFLine.Create;
      vLine.Point1 := MakeFPoint(0, -cnstLineSize);
      AddEntityToBlock(vConverter, vBlock, vLine);
      vLine := TsgDXFLine.Create;
      vLine.Point1 := MakeFPoint(-cnstLineSize, 0);
      AddEntityToBlock(vConverter, vBlock, vLine);
      vLine := TsgDXFLine.Create;
      vLine.Point1 := MakeFPoint(cnstLineSize, 0);
      AddEntityToBlock(vConverter, vBlock, vLine);
      vHatch := TsgCADCurvePolygon.Create;
      AddHatchSector(vHatch, 0);
      AddHatchSector(vHatch, 2);
      vHatch.ColorCAD := cnstColorCADRed;
      AddEntityToBlock(vConverter, vBlock, vHatch);
    finally
      FGCodeExport.CADImage.Converter.Loads(vBlock);
    end;
  end;
  vInsert := TsgDXFInsert.Create;
  vInsert.Block := vBlock;
  vInsert.Scale := MakeFPoint(10, 10, 10);
  if Assigned(ABlock) then
    ABlock.AddEntity(vInsert);
//  AddEntityToBlock(vConverter, vConverter.Sections[csEntities], vInsert);
end;

destructor TsgGCodeManager.Destroy;
begin
  FGCodeExport.Free;
  inherited Destroy;
end;

procedure TsgGCodeManager.ResetImageExternal;
begin
  GCodeExport.ResetImageExternal;
end;

initialization

  bUseSHXFonts := True;
  bSearchSHXPaths := True;
  Paths := TsgStringList.Create;
  try
    Paths.LineBreak := ';';
    FindSHXPaths(Paths);
    sSHXSearchPaths := Paths.Text;
  finally
    Paths.Free;
  end;

end.

{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                       GCode  classes                       }
{                                                            }
{      Copyright (c) 2002-2024 SoftGold software company     }
{                                                            }
{************************************************************}

unit GCode;
{$INCLUDE SGDXF.inc}

interface

uses
  sgConsts, sgFunction, sgLists, DXFConv, Classes, SysUtils, Math,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics
{$ELSE}
  Graphics
{$ENDIF}
  ;

const
  cnstLineWeight = 0.40;
  cnstNumberOfPartsInCircleGcode = 128;
  cnstNumberOfPartsInSplineGcode = 64;
  cnstNumberOfPartsInCircleEquidistant = 48;

type
  TsgGCrossesType = (crtNone, crtLines, crtLineArc, crtArcLine, crtArcs, crtLineDA, crtArcDA, crtDALine, crtDAArc, crtDADA);
  TsgGUnits = (guMillimetre, guCentimetre, guMetre, guInch);
  TsgContourType = (ctMaster, ctTransition, ctLeadIn, ctLeadOut);
  TsgTransitionType = (ttInternal, ttExternal);

  TsgPocketDirection = (pdIn, pdOut);

  TsgPocketParams = record
    Direction: TsgPocketDirection;
    DisableSource: Boolean;
    OverLapPercent: Integer;
    Tools: string;
    ToolNumber: Integer;
    Tool: TsgGToolParams;
    DepthOnZ: Double;
    DepthPass: Double;
  end;

  TsgCrossPointDisc = record
    Point: TFPoint;
    CrossType: TsgGCrossesType;
    SubArc: TsgArcR;
    ParentArcCenter: TFPoint;
    LineAngle: Double;
    HaveDotArc: Boolean;
    DotARCIndex: Integer;
    DAInEdge: Boolean;
  end;

  TsgSelfIntersectPtInfo = record
    Point: TFPoint;
    StVertexInd: Integer;
    EndVertexInd: Integer;
    NewVertexInd: Integer;
    DistanceFromStVer: Double;
    AngleFromCenterBulge: Double;
    ArcR: TsgArcR;
    Bulge: Double;
    Order: Integer;
  end;

  TsgGPassesDirection = (pdForward, pdAlternate);
  TsgGOpetarionPurpose = (opLeading, opWorking);
  TsgGCommandId = (gcNone, gcG94, gcG95, gcG90, gcG91, gcG0, gcG1, gcG2, gcG3, gcG4, gcG17, gcG18, gcG20, gcG21, gcG40, gcG41,
    gcG42, gcFeed, gcSpindleSpeed, gcToolNumb, gcZ, gcComParam, gcM3, gcM4, gcM5,
    gcM6, gcM10, gcM11, gcSwitchGrblToLaser);
  TsgOperationId = (opArcR, opArcL, opLine, opStart, opStartContour, opEndContour,
    opEndProgram, opRadiusCompOn, opRadiusCompOff);
  TsgGCodeMachineTypeID = (mtMill, mtCut, mtHacoKompakt, mtDispenser, mtLathe, mtGRBLRouter, mtGRBLLaser);
  TsgMachines = set of TsgGCodeMachineTypeID;
  TsgGCodeZeroPointID = (zpCADZero, zpTopLeft, zpTopRight, zpBottomLeft, zpBottomRight);
  TsgEquidistantType = (eqtNone, eqtInside, eqtOutside);
  TsgGLeadPurpose = (glpIn, glpOut);
  TsgGStartPointMode = (gspEdge, gspNearest);
  TsgGRBLCNCTypeID = (gctRouter, gctLaser);
  TsgGEqArcType = (eatNormal, eatSub, eatDot);
  PsgNeedCloseEnt = ^TsgNeedCloseEnt;
  TsgNeedCloseEnt = record
    Entity: TsgDXFEntity;
    NeedRemovePoint: Boolean;
  end;

  TsgGOperation = record
    Purpose: TsgGOpetarionPurpose;
    Command: TsgGCommandId;
    MainCoord: TF2DPoint;
    AddCoord: TF2DPoint;
  end;

  PsgIntersectVertexInfo = ^TsgIntersectVertexInfo;
  TsgIntersectVertexInfo = record
    Point: TFPoint;
    VertexInd: Integer;
    VertexInd1: Integer;
  end;


  TsgGArcType = (atArcR, atArcL);
  TsgGCommands = array [TsgGCommandId] of string;

//  TsgArcsR = array of TsgArcR;

//  PsgGArcParams = ^TsgGArcParams;
//  TsgGArcParams = record
//    Arc: TsgDXFArc;
//    ArcType: TsgGArcType;
//  end;

const
  cnstUnitsKoef: array [TsgGUnits] of Double = (1, 10, 1000, 25.4);

  cnstGCommandsConventional: TsgGCommands = ('', 'G94', 'G95', 'G90', 'G91', 'G00', 'G01', 'G02',
    'G03', 'G04', 'G17', 'G18', 'G20', 'G21', 'G40', 'G41', 'G42', 'F', 'S', 'T', 'Z', 'P', 'M03',
    'M04', 'M05', 'M06', 'M10', 'M11', '$32=1');
  cnstGCommandsGRBL: TsgGCommands = ('', 'G94', 'G95', 'G90', 'G91', 'G0', 'G1', 'G2',
    'G3', 'G04', 'G17', 'G18', 'G20', 'G21', 'G40', 'G41', 'G42', 'F', 'S', 'T', 'Z', 'P', 'M3',
    'M4', 'M5', 'M6', 'M10', 'M11', '$32=1');

  cnstRadiusCompGRBL: array [TsgEquidistantType] of string = ('none', 'inside', 'outside');

  cnstDefToolParams: TsgGToolParams = (
    Number: 0;
    Radius: 0;
    Length: 0;
  );
  cnstToolRadiusFactor = 0.9;

  DefPocketParams: TsgPocketParams = (
    Direction: pdIn;
    DisableSource: True;
    OverLapPercent: 30;
    ToolNumber: -1;
    DepthOnZ: 2;
    DepthPass: 2;
  );

var
  MillMachines: TsgMachines;
  LaserMachines: TsgMachines;
  GRBLMachines: TsgMachines;

type
  TNotifyMakeRadiusComp = procedure(AContour: TObject; AValue: TsgGRadiusComp) of object;

  TsgEquidistantContour = class;
  TsgGCodePolyline = class;
  TsgGPocket = class;

  TsgGLead = class
  private
    FAngle: Double;
    FConverter: TsgDXFConverter;
    FGRBL: Boolean;
    FEntity: TsgDXFPolyline;
    FSubEnt: TsgDXFPolyline;
    FTool: TsgDXFCircle;
    FType: TsgGLeadType;
    FPurpose: TsgGLeadPurpose;
    FPtEdge: TFPoint;
    FPtNext: TFPoint;
    FRadius: Double;
    FRadiusComp: TsgGRadiusComp;
    FReversDirection: Boolean;
    procedure ApplyProps(const AEnt: TsgDXFEntity);
    procedure CreateArcLead;
    procedure CreateLineLead;
    procedure CreateTool;
    procedure InitPts(const APtEdge, APtNext: TFPoint);
    procedure MakeVertexesByLine(const APoly: TsgDXFPolyline; ALine: TsgLine);
    function GetToolCenterPoint: TFPoint;
    function GetLineLead(APtEdge, APtNext: PFPoint; ALeadType: TsgGLeadType): TsgLine;
    function NeedRevers(APtClassify: TsgPointClassify): Boolean;
    procedure Remake;
    procedure SetType(AValue: TsgGLeadType);
    procedure SetRadiusComp(AValue: TsgGRadiusComp);
    procedure SetRadius(AValue: Double);
    procedure SetPtEdge(const AValue: TFPoint);
  protected
    property RadiusComp: TsgGRadiusComp read FRadiusComp write SetRadiusComp;
  public
    constructor Create(const AConverter: TsgDXFConverter; APurpose: TsgGLeadPurpose);
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgGLead);
    procedure ChangeDirection;
    property Entity: TsgDXFPolyline read FEntity;
    property SubEntity: TsgDXFPolyline read FSubEnt;
    property Tool: TsgDXFCircle read FTool;
    property LeadType: TsgGLeadType read FType write SetType;
    property Purpose: TsgGLeadPurpose read FPurpose;
    property Radius: Double read FRadius write SetRadius;
    property PtEdge: TFPoint read FPtEdge write SetPtEdge;
  end;

  TsgGContour = class(TsgDXFEntity)
  private
    FArcs: TList;
    FBoring: Boolean;
    FCenterDrill: TFPoint;
    FContourType: TsgContourType;
    FEqRadiusCompGRBL: TsgEquidistantContour;
    FHavePocketContour: Boolean;
    FLength: Double;
    FTurnOffPumpDist: Double;
    FTurnOffPumpVerInd: Integer;
    FTurnOffPumpVerIndAfter: Integer;
    FTurnOffPumpVerIndBefore: Integer;
//    FDelay: Integer;                  //for the future
    FEnable: Boolean;
    FGPolyline: TsgGCodePolyline;
    FPocket: TsgGPocket;
    FDepthOnZ: Double;
    FDepthPass: Double;
//    FFeedOnXY: Integer;               //for the future
//    FFeedOnZ: Integer;                //for the future
    FFeedDrill: Integer;
    FLeadIn: TsgGLead;
    FLeadOut: TsgGLead;
    FMachineType: TsgGCodeMachineTypeID;
    FOnMakeEquidistant: TNotifyMakeRadiusComp;
    FName: string;
    FNumbOfPasses: Integer;
    FLayerName: string;
    FRaduisCorNumber: Integer;
    FRadiusComp: TsgGRadiusComp;
    FReverse: Boolean;
    FRPlane: Double;                    //for Drill
    FShapeType: TsgContourShape;
    FStartPoint: TFPoint;
//    FStartPointMode: TsgGStartPointMode;
    FToolsDispenser: Byte;
    FToolParams: TsgGToolParams;
    FTools: TStringList;
    function GetIsFromPocket: Boolean;
    function GetNumbOfPassesByMachine: Integer;
    function InitDistanceOffsetPoint(ADist: Double): Double;
    function GetLeadIn: TsgGLead;
    function GetLeadOut: TsgGLead;
    procedure SetGPolyline(const AValue: TsgGCodePolyline);
    procedure SetShapeType(const AValue: TsgContourShape);
    procedure SetCenterDrill(const AValue: TFPoint);
    procedure SetDistanceOffset(const Value: Double);
    procedure SetMachineType(const Value: TsgGCodeMachineTypeID);
    procedure SetDepthOnZ(const AValue: Double);
    procedure SetDepthPass(const AValue: Double);
    procedure SetNumbOfPasses(const AValue: Integer);
  protected
    function CanDraw: Boolean; override;
//    procedure EndContour;            //for the future
//    procedure StartContour;
//    procedure MakeLead;
    function GetBoring: Boolean;
    function GetConverter: TsgDXFConverter; override;
    function GetBox: TFRect; override;
    function GetEnable: Boolean;
    function GetFeedDrill: Integer;
    function GetLeadInTypeInsp: Byte;
    function GetLeadOutTypeInsp: Byte;
    function GetName: string; override;
    function GetRadiusComp: Byte;
    function GetRPlane: Double;
    function GetReverse: Boolean;
    function GetLeadInReverse: Boolean;
    function GetLeadOutReverse: Boolean;
    function GetShapeTypeInsp: Byte;
    function GetToolDispenser: Byte;
    function GetToolParams: string;
    function GetToolNumber: Byte;
    function GetTools: TStringList;
    procedure ChangeDirection;
    procedure SetBoring(AValue: Boolean);
    procedure SetEnable(AValue: Boolean);
    procedure SetFeedDrill(AValue: Integer);
    procedure SetLeadInType(AValue: TsgGLeadType);
    procedure SetLeadOutType(AValue: TsgGLeadType);
    procedure SetName(const AName: string); override;
    procedure SetRadiusComp(AValue: TsgGRadiusComp);
    procedure SetRPlane(AValue: Double);
    procedure SetToolDispenser(AValue: Byte);
    procedure SetToolParams(AParams: TsgGToolParams);
    procedure SetTools(const AValue: TStringList);
    procedure SetReverse(AValue: Boolean);
    procedure SetLeadInReverse(AValue: Boolean);
    procedure SetLeadOutReverse(AValue: Boolean);
    property ContourType: TsgContourType read FContourType write FContourType;
    property RadiusCompGRBL: TsgEquidistantContour read FEqRadiusCompGRBL write FEqRadiusCompGRBL;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddRadiusCompGRBL(const AEquidistant: TsgEquidistantContour);
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure ChangeStartPoint(const APolyLine: TsgDXFPolyline;
      const APoint: TFPoint; ASnapMode: TObjectSnapState);
    procedure InitLeadsPts;
    property Length: Double read FLength;
    property TurnOffPumpDist: Double read FTurnOffPumpDist write SetDistanceOffset;    //for Dispenser
    property TurnOffPumpVerInd: Integer read FTurnOffPumpVerInd;                        //for Dispenser
    property TurnOffPumpVerIndBefone: Integer read FTurnOffPumpVerIndBefore;
    property TurnOffPumpVerIndAfter: Integer read FTurnOffPumpVerIndAfter;
    property Boring: Boolean read FBoring write FBoring;
    property Enable: Boolean read GetEnable write SetEnable;
    property HavePocketContour: Boolean read FHavePocketContour write FHavePocketContour;
//    property Equidistants: TList read FEquidistants write FEquidistants;
    property FeedDrill: Integer read FFeedDrill write FFeedDrill;
    property LeadIn: TsgGLead read GetLeadIn;
    property LeadOut: TsgGLead read GetLeadOut;
    property Name: string read FName write FName;
    property LayerName: string read FLayerName write FLayerName;
    property OnMakeEquidistant: TNotifyMakeRadiusComp read FOnMakeEquidistant write FOnMakeEquidistant;
    property GPolyline: TsgGCodePolyline read FGPolyline write SetGPolyline;
    property IsFromPocket: Boolean read GetIsFromPocket;
    property Pocket: TsgGPocket read FPocket;
    property RadiusCompType: TsgGRadiusComp read FRadiusComp write SetRadiusComp;
    property Reverse: Boolean read FReverse;
    property RPlane: Double read FRPlane write FRPlane;
    property ShapeType: TsgContourShape read FShapeType write SetShapeType;
    property StartPoint: TFPoint read FStartPoint write FStartPoint;
    property ToolParams: TsgGToolParams read FToolParams write SetToolParams;
    property ToolDispenser: Byte read GetToolDispenser write SetToolDispenser;
    property CenterDrill: TFPoint read FCenterDrill write SetCenterDrill;
    property MachineType: TsgGCodeMachineTypeID read FMachineType write SetMachineType;
    property NumbOfPasses: Integer read FNumbOfPasses write SetNumbOfPasses;
    property NumbOfPassesByMachine: Integer read GetNumbOfPassesByMachine;
    property DepthPass: Double read FDepthPass write SetDepthPass;
    property DepthOnZ: Double read FDepthOnZ write SetDepthOnZ;
  end;

  TsgEquidistantContour = class
  private
    FContours: TsgObjectList;
    FOffset: Double;
    FEquidistantType: TsgEquidistantType;
    function GetContour(Index: Integer): TsgGContour;
    procedure SetContour(Index: Integer; const Value: TsgGContour);
    function GetContourCount: Integer;
  protected
    procedure SetEnable(AValue: Boolean);
    procedure ChangeLeadInDirection;
    procedure ChangeLeadOutDirection;
    procedure SetRadiusCompParams(ARadiusComp: TsgGRadiusComp; const AToolsParams: TsgGToolParams);
  public
    constructor Create;
    destructor Destroy; override;
    function AddContour(const AContour: TsgGContour): Integer;
    property ContourCount: Integer read GetContourCount;
    property Contours[Index: Integer]: TsgGContour read GetContour write SetContour;
    property Offset: Double read FOffset write FOffset;
    property EquidistantType: TsgEquidistantType read FEquidistantType write FEquidistantType;
  end;

  TsgTransition = class
  private
    FType: TsgTransitionType;
    FPoly1: TsgDXFPolyline;
    FPoly2: TsgDXFPolyline;
  public
    constructor Create(AType: TsgTransitionType);
    property Poly1: TsgDXFPolyline read FPoly1 write FPoly1;
    property Poly2: TsgDXFPolyline read FPoly2 write FPoly2;
    property TranType: TsgTransitionType read FType write FType;
  end;

  TsgGPocket = class
  private
    FConverter: TsgDXFConverter;
    FSourceContour: TsgGContour;
    FComponents: TsgList;
    FToolParams: TsgGToolParams;
    FDirection: TsgPocketDirection;
    FOverLapPercent: Double;
    FContours: TsgList;
    FDepthOnZ: Double;
    FDepthPass: Double;
  protected
    procedure GenerateContours;
    function MakeTransitionPoly(const ATransiton: TsgTransition): TsgGCodePolyline;
  public
    constructor Create(const ASourceContour: TsgGContour; const AConverter: TsgDXFConverter);
    destructor Destroy; override;
    procedure AddContour(const AContour: TsgGContour);
    procedure ApplyParams(const AParams: TsgPocketParams);
    property Components: TsgList read FComponents;
    property DepthOnZ: Double read FDepthOnZ write FDepthOnZ;
    property DepthPass: Double read FDepthPass write FDepthPass;
    property ToolParams: TsgGToolParams read FToolParams write FToolParams;
    property Direction: TsgPocketDirection read FDirection write FDirection;
    property OverLapPercent: Double read FOverLapPercent write FOverLapPercent;
    property Contours: TsgList read FContours;
    property SourceContour: TsgGContour read FSourceContour;
  end;

  TsgDXFGArc = class(TsgDXFArc)
  private
    FActive: Boolean;
    FArcType: TsgGArcType;
    FParentArc: PsgArc;
    FLayerName: string;
    FOffset: Integer;
    FChecked: Boolean;
    FIsCrossCounter: Integer;
    FNeedRemove: Boolean;
    procedure SetChecked(const AValue: Boolean);
    procedure SetNeedRemove(const Value: Boolean);
    procedure SetOffset(const Value: Integer);
  protected
    procedure SetIsCrossCounter(AValue: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property ArcType: TsgGArcType read FArcType write FArcType;
    property ParentArc: PsgArc read FParentArc write FParentArc;
    property Active: Boolean read FActive write FActive;
    property IsCrossCounter: Integer read FIsCrossCounter write SetIsCrossCounter;
    property LayerName: string read FLayerName write FLayerName;
    property NeedRemove: Boolean read FNeedRemove write SetNeedRemove;
    property Checked: Boolean read FChecked write SetChecked;
    property Offset: Integer read FOffset write SetOffset;
  end;

  TsgDXFEqGArc = class(TsgDXFGArc)
  private
    FInitAngleCount: Byte;
    FEqArcType: TsgGEqArcType;
//    FSubArc: Boolean;
    FParentArcCenter: TFPoint;
    FReverse: Boolean;
//    FDotArc: Boolean;
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure IncAngleCount;
//    property DotArc: Boolean read FDotArc write FDotArc;
    property EqArcType: TsgGEqArcType read FEqArcType write FEqArcType;
    property ParentArcCenter: TFPoint read FParentArcCenter write FParentArcCenter;
    property Reverse: Boolean read FReverse write FReverse;
//    property SubArc: Boolean read FSubArc write FSubArc;
    property InitAngleCount: Byte read FInitAngleCount write FInitAngleCount;
  end;

  TsgDXFEqLine = class(TsgDXFLine)
  private
    FStDotLine: Boolean;
    FEndDotLine: Boolean;
    FInitEdgeCount: Byte;
    FSourceLine: TsgLine;
    FStartOriginPoint: TFPoint;
    FEndOriginPoint: TFPoint;
    FReverse: Boolean;
    procedure SetSourceLine(const Value: TsgLine);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure DecEdgeCount;
    procedure IncEdgeCount;
    property StDotLine: Boolean read FStDotLine write FStDotLine;
    property EndDotLine: Boolean read FEndDotLine write FEndDotLine;
    property InitEdgeCount: Byte read FInitEdgeCount;
    property Reverse: Boolean read FReverse write FReverse;
    property StartOriginPoint: TFPoint read FStartOriginPoint write FStartOriginPoint;
    property SourceLine: TsgLine read FSourceLine write SetSourceLine;
    property EndOriginPoint: TFPoint read FEndOriginPoint write FEndOriginPoint;
  end;

  TsgGCodePolyline = class(TsgDXFLWPolyline)
  private
    FContourType: TsgCADEntities;
    FLayerName: string;
    FLinkEntity: TsgGContour;
    FClosedByResolution: Boolean;
    FIsPocket: Boolean;
  protected
    function GetClosed: Boolean; override;
    function GetBoringInsp: Boolean;
    function GetEnableInsp: Boolean;
    function GetFeedDrillInsp: Integer;
    function GetNameInsp: string;
    function GetLeadInTypeInsp: Byte;
    function GetLeadOutTypeInsp: Byte;
    function GetLeadInReverseInsp: Boolean;
    function GetLeadOutReverseInsp: Boolean;
    function GetLinkEntity: TsgDXFEntity; override;
    function GetRadiusCompInsp: Byte;
    function GetReverseInsp: Boolean;
    function GetRPlaneInsp: Double;
    function GetShapeTypeInsp: Byte;
    function GetToolDispenserInsp: Byte;
    function GetToolNumberInsp: Byte;
    function GetDepthOnZInsp:Double;
    function GetDepthPassInsp: Double;
    function GetDistanceOffsetInsp: Double;
    function GetDistanceOffsetPointInsp: TFPoint;
    function GetLengthInsp: Double;
    procedure SetClosed(const AClosed: Boolean); override;
    procedure SetBoringInsp(AValue: Boolean);
    procedure SetEnableInsp(AValue: Boolean);
    procedure SetFeedDrillInsp(AValue: Integer);
    procedure SetLeadInTypeInsp(AValue: TsgGLeadType);
    procedure SetLeadOutTypeInsp(AValue: TsgGLeadType);
    procedure SetLeadInReverseInsp(AValue: Boolean);
    procedure SetLeadOutReverseInsp(AValue: Boolean);
    procedure SetLinkEntity(const AValue: TsgDXFEntity);
    procedure SetNameInsp(const AName: string);
    procedure SetRadiusCompInsp(AValue: TsgGRadiusComp);
    procedure SetReverseInsp(AValue: Boolean);
    procedure SetRPlaneInsp(AValue: Double);
    procedure SetToolDispenserInsp(AValue: Byte);
    procedure SetToolParamsInsp(AParams: TsgGToolParams);
    procedure SetDistanceOffsetInsp(AValue: Double);
    property ContourType: TsgCADEntities read FContourType write FContourType;
  public
    constructor CreateEx(LayerName: string);
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property ClosedByResolution: Boolean read FClosedByResolution write FClosedByResolution;
    property IsClosed: Boolean read GetClosed write SetClosed;
    property LinkEntity: TsgDXFEntity read GetLinkEntity write SetLinkEntity;
    property LayerName: string read FLayerName write FLayerName;
    property IsPocket: Boolean read FIsPocket write FIsPocket;
  end;

  procedure Convert2DTo3DPtList(const ABefore: TF2DPointList; AAfter: TFPointList);
  procedure Convert3DTo2DPtList(const ABefore: TFPointList; AAfter: TF2DPointList);
  procedure ReversePolyline(const APolyline: TsgDXFPolyline);
  function GetToolParamFromString(AValue: string; ADecimalSeparator: Char = cnstOptionsDecimalSeparator): TsgGToolParams;
  function ToolParamsToInspStr(const AParams: TsgGToolParams): string;
  function InspStrToToolParams(const AParams: string): TsgGToolParams;
  procedure FillToolsList(ATools: string; const AToolsList: TStringList);
  function GetArcFromVertexes(const AVertex1, AVertex2: TsgDXFVertex): TsgArc;
  procedure InitContourDepthAndPasses(const AContour: TsgGContour; ADepthOnZ,
    ADepthPass: Double; ANumbOfPasses: Integer);


implementation

type
  TsgDXFPolylineAccess = class(TsgDXFPolyline);

procedure Convert2DTo3DPtList(const ABefore: TF2DPointList; AAfter: TFPointList);
var
  I: Integer;
begin
  AAfter.Capacity := ABefore.Capacity;
  for I := 0 to ABefore.Count - 1 do
    AAfter.Add(F2DPointTo3D(ABefore[I]));
end;

procedure Convert3DTo2DPtList(const ABefore: TFPointList; AAfter: TF2DPointList);
var
  I: Integer;
begin
  AAfter.Capacity := ABefore.Capacity;
  for I := 0 to ABefore.Count - 1 do
    AAfter.Add(ABefore[I].Point2D);
end;

function ToolParamsToInspStr(const AParams: TsgGToolParams): string;
begin
  Result := '№' + IntToStr(AParams.Number) + '/' + char($0D8) +
    FloatToStr(AParams.Radius * 2) + 'mm/' + 'L' + FloatToStr(AParams.Length)+ 'mm';
end;

function InspStrToToolParams(const AParams: string): TsgGToolParams;
var
  vParams: string;
begin
  vParams := AParams;
  sgFunction.StringReplace(vParams, '№', '');
  sgFunction.StringReplace(vParams, 'L', '');
  sgFunction.StringReplace(vParams, char($0D8), '');
  sgFunction.StringReplace(vParams, 'mm', '');
  sgFunction.StringReplace(vParams, ',', '.');
  Result := GetToolParamFromString(vParams)
end;


function GetToolParamFromString(AValue: string; ADecimalSeparator: Char = cnstOptionsDecimalSeparator): TsgGToolParams;
var
  vParams: TsgStringList;
begin
  vParams := TsgStringList.Create;
  try
    {$IFDEF SGDEL_7}
    vParams.Delimiter := cnstGParamsSeparator;
    vParams.DelimitedText := AValue;
    {$ELSE}
    vParams.LineBreak := cnstGParamsSeparator;
    vParams.Text := AValue;
   {$ENDIF}
    Result.Number := StrToInt(vParams[cnstRCNumber]);
    Result.Radius := StrToDouble(vParams[cnstRCDiameter], ADecimalSeparator) / 2;
    Result.Length := StrToDouble(vParams[cnstRCLength], ADecimalSeparator);
  finally
    vParams.Free;
  end;
end;

procedure FillToolsList(ATools: string; const AToolsList: TStringList);
begin
  if not Assigned(AToolsList) then Exit;
  AToolsList.Delimiter := cnstGToolsSeparator;
  ReplaceAnsi(ATools, cnstOptionsDecimalSeparator, GetDecimalSeparator);
  AToolsList.DelimitedText := ATools;
end;

function GetGPointOfDoubleOffset(const APB1, APB2, APC: TFPoint; const AD: Double; const APoint: PFPoint): Boolean;
var
  vP1, vP2: TFPoint;
  vLine: TsgLine;
begin
  case IsCrossCirclesCR(APB1, AD, APB2, AD, @vP1, @vP2) of
    1:
      begin
        Result := True;
        if APoint <> nil then
          APoint^ := vP1;
      end;
    2:
      begin
        Result := True;
        if APoint <> nil then
        begin
          vLine.Point1 := APB1;
          vLine.Point2 := APB2;
          if PointClassifyEx(vLine, APC) = PointClassifyEx(vLine, vP1) then
            APoint^ := vP2
          else
            APoint^ := vP1;
        end;
      end;
  else
    Result := False;
  end;
end;

function GetArcFromVertexes(const AVertex1, AVertex2: TsgDXFVertex): TsgArc;
begin
  Result.Point1 := AVertex1.Point;
  Result.Point2 := AVertex2.Point;
  Result.Center := GetCenterOfBulge(AVertex1.Point, AVertex2.Point, AVertex1.Bulge);
end;

procedure InitContourDepthAndPasses(const AContour: TsgGContour; ADepthOnZ,
  ADepthPass: Double; ANumbOfPasses: Integer);
begin
  AContour.DepthOnZ := ADepthOnZ;
  AContour.DepthPass := ADepthPass;
  AContour.NumbOfPasses := ANumbOfPasses;
end;

 {TsgGCodeArc}

procedure TsgDXFGArc.AssignEntity(Source: TsgDXFEntity);
var
  vArc: TsgDXFArc absolute Source;
  vPt: TFPoint;
begin
  inherited AssignEntity(Source);

  if (Source is TsgDXFArc) and not IsEqualFPoints(vArc.Extrusion, cnstExtrusion) then
  begin
    vPt := Self.Point;
    DoExtrusion(vPt, vArc.Extrusion);
    Self.Point := vPt;
    Self.StartAngle := GetAngleByPoints(vPt, vArc.PolyPoints.Last, False);
    Self.EndAngle := GetAngleByPoints(vPt, vArc.PolyPoints.First, False);
    Self.Extrusion := cnstExtrusion;
  end;

  if Source is TsgDXFGArc then
  begin
    FArcType := TsgDXFGArc(Source).ArcType;
    FActive := True;
    FChecked := False;
    FOffset := TsgDXFGArc(Source).Offset;
  end;
end;

constructor TsgDXFGArc.Create;
begin
  inherited Create;
  FActive := True;
end;

destructor TsgDXFGArc.Destroy;
begin
  if FParentArc <> nil then
    Dispose(FParentArc);
  inherited Destroy;
end;

procedure TsgDXFGArc.SetOffset(const Value: Integer);
begin
  FOffset := Value;
end;

procedure TsgDXFGArc.SetChecked(const AValue: Boolean);
begin
  FChecked := AValue;
end;

procedure TsgDXFGArc.SetIsCrossCounter(AValue: Integer);
begin
  if AValue > -1 then
    FIsCrossCounter := AValue;
  if FIsCrossCounter > 1 then
    FNeedRemove := True;
end;

procedure TsgDXFGArc.SetNeedRemove(const Value: Boolean);
begin
  FNeedRemove := Value;
end;


{TsgGCodePolyline}

procedure TsgGCodePolyline.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgGCodePolyline then
    FLinkEntity := TsgGCodePolyline(Source).FLinkEntity
  else
    FLinkEntity := nil;
  FClosedByResolution := TsgGCodePolyline(Source).ClosedByResolution;
end;

constructor TsgGCodePolyline.CreateEx(LayerName: string);
begin
  Create;
  FLayerName := LayerName;
  FContourType := cePolyline;
end;

function TsgGCodePolyline.GetBoringInsp: Boolean;
begin
  Result := FLinkEntity.GetBoring;
end;

function TsgGCodePolyline.GetClosed: Boolean;
begin
  Result := IsEqualFPoints(PolyPoints.First, PolyPoints.Last);
end;

function TsgGCodePolyline.GetDepthOnZInsp: Double;
begin
  Result := FLinkEntity.DepthOnZ;
end;

function TsgGCodePolyline.GetDepthPassInsp: Double;
begin
  Result := Abs(FLinkEntity.FDepthPass);
end;

function TsgGCodePolyline.GetDistanceOffsetInsp: Double;
begin
  Result := FLinkEntity.TurnOffPumpDist;
end;

function TsgGCodePolyline.GetDistanceOffsetPointInsp: TFPoint;
begin
  Result := cnstBadPoint;
  if FLinkEntity.TurnOffPumpVerInd > -1 then
    Result := FLinkEntity.GPolyline.Vertexes[FLinkEntity.TurnOffPumpVerInd].Point;
end;

function TsgGCodePolyline.GetEnableInsp: Boolean;
begin
  Result := FLinkEntity.GetEnable;
end;

function TsgGCodePolyline.GetFeedDrillInsp: Integer;
begin
  Result := FLinkEntity.GetFeedDrill;
end;

function TsgGCodePolyline.GetLeadInReverseInsp: Boolean;
begin
  Result := FLinkEntity.GetLeadInReverse;
end;

function TsgGCodePolyline.GetLeadInTypeInsp: Byte;
begin
  Result := FLinkEntity.GetLeadInTypeInsp;
end;

function TsgGCodePolyline.GetLeadOutReverseInsp: Boolean;
begin
  Result := FLinkEntity.GetLeadOutReverse;
end;

function TsgGCodePolyline.GetLeadOutTypeInsp: Byte;
begin
  Result := FLinkEntity.GetLeadOutTypeInsp;
end;

function TsgGCodePolyline.GetLengthInsp: Double;
begin
  Result := FLinkEntity.Length;
end;

function TsgGCodePolyline.GetLinkEntity: TsgDXFEntity;
begin
  Result := FLinkEntity;
end;

function TsgGCodePolyline.GetNameInsp: string;
begin
  Result := FLinkEntity.GetName;
end;

function TsgGCodePolyline.GetRadiusCompInsp: Byte;
begin
  Result := FLinkEntity.GetRadiusComp;
end;

function TsgGCodePolyline.GetReverseInsp: Boolean;
begin
  Result := FLinkEntity.GetReverse;
end;

function TsgGCodePolyline.GetRPlaneInsp: Double;
begin
  Result := FLinkEntity.GetRPlane;
end;

function TsgGCodePolyline.GetShapeTypeInsp: Byte;
begin
  Result := FLinkEntity.GetShapeTypeInsp;
end;

function TsgGCodePolyline.GetToolDispenserInsp: Byte;
begin
  Result := FLinkEntity.GetToolDispenser;
end;

function TsgGCodePolyline.GetToolNumberInsp: Byte;
begin
  Result := FLinkEntity.GetToolNumber;
end;

procedure TsgGCodePolyline.SetBoringInsp(AValue: Boolean);
begin
  FLinkEntity.SetBoring(AValue);
end;

procedure TsgGCodePolyline.SetClosed(const AClosed: Boolean);
begin
end;

procedure TsgGCodePolyline.SetDistanceOffsetInsp(AValue: Double);
begin
  FLinkEntity.SetDistanceOffset(AValue);
end;

procedure TsgGCodePolyline.SetEnableInsp(AValue: Boolean);
begin
  FLinkEntity.SetEnable(AValue);
end;

procedure TsgGCodePolyline.SetFeedDrillInsp(AValue: Integer);
begin
  FLinkEntity.SetFeedDrill(AValue);
end;

procedure TsgGCodePolyline.SetLeadInReverseInsp(AValue: Boolean);
begin
  FLinkEntity.SetLeadInReverse(AValue);
end;

procedure TsgGCodePolyline.SetLeadInTypeInsp(AValue: TsgGLeadType);
begin
  FLinkEntity.SetLeadInType(AValue);
end;

procedure TsgGCodePolyline.SetLeadOutReverseInsp(AValue: Boolean);
begin
  FLinkEntity.SetLeadOutReverse(AValue);
end;

procedure TsgGCodePolyline.SetLeadOutTypeInsp(AValue: TsgGLeadType);
begin
  FLinkEntity.SetLeadOutType(AValue);
end;

procedure TsgGCodePolyline.SetLinkEntity(const AValue: TsgDXFEntity);
begin
  if AValue is TsgGContour then
    FLinkEntity := TsgGContour(AValue)
  else
    FLinkEntity := nil;
end;

procedure TsgGCodePolyline.SetNameInsp(const AName: string);
begin
  FLinkEntity.SetName(AName);
end;

procedure TsgGCodePolyline.SetRadiusCompInsp(AValue: TsgGRadiusComp);
begin
  FLinkEntity.SetRadiusComp(AValue);
end;

procedure TsgGCodePolyline.SetReverseInsp(AValue: Boolean);
begin
  FLinkEntity.SetReverse(AValue);
end;

procedure TsgGCodePolyline.SetRPlaneInsp(AValue: Double);
begin
  FLinkEntity.SetRPlane(AValue);
end;

procedure TsgGCodePolyline.SetToolDispenserInsp(AValue: Byte);
begin
  FLinkEntity.SetToolDispenser(AValue);
end;

procedure TsgGCodePolyline.SetToolParamsInsp(AParams: TsgGToolParams);
begin
  FLinkEntity.SetToolParams(AParams);
end;

{TsgGLead}

constructor TsgGLead.Create(const AConverter: TsgDXFConverter; APurpose: TsgGLeadPurpose);
begin
  FAngle := 90;
  FRadiusComp := grcNone;
  FType := gltNone;
  FReversDirection := False;
  FPurpose := APurpose;
  FConverter := AConverter;
end;

function TsgGLead.NeedRevers(APtClassify: TsgPointClassify): Boolean;
begin
  Result := False;
  case FPurpose of
    glpIn:
      begin
        if not (FRadiusComp = grcLeft) and (APtClassify = pcLEFT) or
        not (FRadiusComp = grcRight) and (APtClassify = pcRIGHT) then
          Result := True;
      end;
    glpOut:
      begin
        if (FRadiusComp = grcLeft) and (APtClassify = pcLEFT) or
        (FRadiusComp = grcRight) and (APtClassify = pcRIGHT) then
          Result := True;
      end;
  end;
end;

procedure TsgGLead.CreateArcLead;
var
  vAngle: Double;
  vPtTan: TFPoint;

  procedure CreateSubEnt(ARevers: Boolean);
  var
    vPtEdge, vPtNext: TFPoint;
    vLine: TsgLine;
    vArc: TsgDXFPolyline;
  begin
    vArc := FEntity;
    vPtEdge := vArc.Points[0];
    vPtNext := vArc.Points[1];
    if ARevers then
    begin
      vPtEdge := vArc.PolyPoints[vArc.PointCount - 1];
      if IsEqualFPoints(vPtEdge, vArc.Points[vArc.PointCount - 2]) then
        vPtNext := vArc.PolyPoints[vArc.PointCount - 3]
      else
        vPtNext := vArc.PolyPoints[vArc.PointCount - 2];
    end;
    FSubEnt := TsgDXFPolyline.Create;
    ApplyProps(FSubEnt);
    vLine := GetLineLead(@vPtEdge, @vPtNext, gltTangent);
    MakeVertexesByLine(FSubEnt, vLine);
  end;

  function GetNormalPoint(var ANeedRevers: Boolean): TFPoint;
  var
    vPtNormClassify: TsgPointClassify;
  begin
    Result := GetPointOnCircle(FPtEdge, FRadius, vAngle + FAngle);
    vPtNormClassify := PointClassify2DPts(Result.Point2D, vPtTan.Point2D, FPtNext.Point2D);
    ANeedRevers := NeedRevers(vPtNormClassify);
    if FReversDirection then
      ANeedRevers := not ANeedRevers;
    if ANeedRevers then
    begin
      FAngle := 270;
      Result := GetPointOnCircle(FPtEdge, FRadius, vAngle + FAngle);
    end;
  end;

var
  vPtArc, vPtSt, vPtNorm, vPtEn: TFPoint;
  vDist, vAngSt, vAngEnd: Double;
  vArcR: TsgArcR;
  vNeedRevers: Boolean;
begin
  vNeedRevers := False;
  FEntity := TsgDXFPolyline.Create;
  ApplyProps(FEntity);
  vDist := DistanceF2DPoint(FPtEdge.Point2D, FPtNext.Point2D);
  vPtTan := GetPointOnLine(FPtNext, FPtEdge, vDist + FRadius);
  vAngle := GetAngleByPoints(FPtEdge, FPtNext, False);
  vPtNorm := GetNormalPoint(vNeedRevers);
  GetGPointOfDoubleOffset(vPtTan, vPtNorm, FPtEdge, FRadius, @vPtArc);
  vPtSt := vPtArc;
  vPtEn := FPtEdge;
  if vNeedRevers then
    SwapFPoints(vPtSt, vPtEn);
  vAngSt := GetAngleByPoints(vPtNorm, vPtSt, False);
  vAngEnd := GetAngleByPoints(vPtNorm, vPtEn, False);
  vArcR := MakeArcR(vPtNorm, FRadius, vAngSt, vAngEnd);
  AddVertexInPolyline(FEntity, vPtSt, GetBulgeOfArcR(vArcR, vPtSt));
  AddVertexInPolyline(FEntity, vPtEn);
  FConverter.Loads(FEntity);
  if FRadiusComp <> grcNone then
    CreateSubEnt(vNeedRevers);
  FAngle := 90;
  if Assigned(FSubEnt) then
    FConverter.Loads(FSubEnt);
end;


procedure TsgGLead.CreateLineLead;
var
  vLine: TsgLine;
begin
  FEntity := TsgDXFPolyline.Create;
  ApplyProps(FEntity);
  vLine := GetLineLead(nil, nil, FType);
//  TsgDXFLine(FEntity).Line := GetLineLead(nil, nil, FType);
  MakeVertexesByLine(FEntity, vLine);
  FConverter.Loads(FEntity);
end;

procedure TsgGLead.CreateTool;
var
  vCenPt: TFPoint;
begin
  FreeAndNil(FTool);
  if FPurpose = glpIn then
  begin
    FTool := TsgDXFCircle.Create;
    vCenPt := GetToolCenterPoint;
    FTool.Point := vCenPt;
    if FGRBL then
      FTool.Radius := Radius
    else
      FTool.Radius := cnstToolRadiusFactor * Radius;
    FConverter.Loads(FTool);
  end;
end;

destructor TsgGLead.Destroy;
begin
  FreeAndNil(FEntity);
  FreeAndNil(FSubEnt);
  FreeAndNil(FTool);
  inherited Destroy;
end;

function TsgGLead.GetToolCenterPoint: TFPoint;

  function GetCenPt(ADelta: Extended): TFPoint;
  begin
    if Assigned(FEntity) then
    begin
      if FGRBL then
      begin
        if Assigned(FSubEnt) then
          Result := FSubEnt.Points[0]
        else
        begin
          if Assigned(FEntity) then
            Result := FEntity.Points[0];
        end;
      end
      else
      begin
        if Assigned(FSubEnt) then
          Result := GetPointOnLineNormal(FSubEnt.Vertexes[0].Point, FSubEnt.Vertexes[1].Point, FSubEnt.Vertexes[0].Point, ADelta)
        else
        begin
          if Assigned(FEntity) then
          begin
            Result := GetPointOnLineNormal(FEntity.Points[0],
              FEntity.Points[1], FEntity.Points[0], ADelta);
          end;
        end;
      end;
    end
    else
      Result := FPtEdge;
  end;

begin
  Result := cnstBadPoint;
  case FRadiusComp of
    grcLeft:
      Result := GetCenPt(-Radius * cnstToolRadiusFactor);
    grcRight:
      Result := GetCenPt(Radius * cnstToolRadiusFactor);
    grcNone:
      Result := FPtEdge;
  end;
end;

procedure TsgGLead.InitPts(const APtEdge, APtNext: TFPoint);
begin
  FPtEdge := APtEdge;
  FPtNext := APtNext;
end;

procedure TsgGLead.MakeVertexesByLine(const APoly: TsgDXFPolyline;
  ALine: TsgLine);
begin
  AddVertexInPolyline(APoly, ALine.Point1);
  AddVertexInPolyline(APoly, ALine.Point2);
end;

procedure TsgGLead.SetType(AValue: TsgGLeadType);
begin
  FType := AValue;
  FreeAndNil(FEntity);
  FreeAndNil(FSubEnt);
  if (FRadiusComp <> grcNone) and (FRadius > 0) then
  begin
    case FType of
      gltArc:
        CreateArcLead;
      gltNormal, gltTangent:
        CreateLineLead;
    end;
    if Assigned(FEntity) then
      CreateTool;
  end;
end;

procedure TsgGLead.SetPtEdge(const AValue: TFPoint);
begin
  FPtEdge := AValue;
end;

procedure TsgGLead.SetRadius(AValue: Double);
begin
  FRadius := AValue;
  LeadType := FType;
end;

procedure TsgGLead.SetRadiusComp(AValue: TsgGRadiusComp);
begin
  FRadiusComp := AValue;
  FReversDirection := False;
  if FRadiusComp <> grcNone then
  begin
    if not FGRBL and (LeadType = gltNone) then
      LeadType := gltTangent
    else
      LeadType := FType;
  end
  else
    LeadType := gltNone;
end;

procedure TsgGLead.ApplyProps(const AEnt: TsgDXFEntity);
begin
  Exit;
  if FPurpose = glpIn then
    AEnt.Color := cnstLeadInColor
  else
    AEnt.Color := cnstLeadOutColor;
  AEnt.LineWeight := cnstLineWeight;
end;

procedure TsgGLead.AssignEntity(Source: TsgGLead);
var
  vLead: TsgGLead absolute Source;
begin
  FAngle := Source.FAngle;
  if Assigned(Source.FEntity) then
  begin
    FEntity := TsgDXFPolyline.Create;
    FEntity.AssignEntity(Source.FEntity);
  end;
  if Assigned(Source.FSubEnt) then
  begin
    FSubEnt := TsgDXFPolyline.Create;
    FSubEnt.AssignEntity(Source.FSubEnt);
  end;
  FType := Source.FType;
  FPurpose := Source.FPurpose;
  FPtEdge := Source.FPtEdge;
  FPtNext := Source.FPtNext;
  FRadius := Source.FRadius;
  FRadiusComp := Source.FRadiusComp;
  FReversDirection := Source.FReversDirection;
end;

procedure TsgGLead.ChangeDirection;
begin
  if not Assigned(FEntity) then
    Exit;
  FReversDirection := not FReversDirection;
  Remake;
end;

function TsgGLead.GetLineLead(APtEdge, APtNext: PFPoint; ALeadType: TsgGLeadType): TsgLine;
var
  vDist, vAngle, vRadius: Double;
  vPt: TFPoint;
  vPtEdge, vPtNext: TFPoint;
  vPtClassify: TsgPointClassify;
begin
  vPtEdge := FPtEdge;
  vPtNext := FPtNext;
  vRadius := FRadius;
  if Assigned(APtEdge) then
  begin
    vPtEdge := APtEdge^;
    vPtNext := APtNext^;
  end;
  if ALeadType = gltTangent then
  begin
    vDist := DistanceF2DPoint(vPtEdge.Point2D, vPtNext.Point2D);
    if FReversDirection and (FType <> gltArc) then
      vPt := GetPointOnLine(vPtNext, vPtEdge, vDist - vRadius)
    else
      vPt := GetPointOnLine(vPtNext, vPtEdge, vDist + vRadius);
  end
  else
  begin
    vAngle := GetAngleByPoints(vPtEdge, vPtNext, False);
    vPt := GetPointOnCircle(vPtEdge, vRadius, vAngle + 90);
    vPtClassify := PointClassify2DPts(vPt.Point2D, vPtEdge.Point2D, vPtNext.Point2D);
    if NeedRevers(vPtClassify) and not FReversDirection or not NeedRevers(vPtClassify) and FReversDirection then
      vPt := GetPointOnCircle(vPtEdge, vRadius, vAngle + 270);
  end;
  Result := MakeLine(vPt, vPtEdge);
end;

procedure TsgGLead.Remake;
begin
  LeadType := FType;
end;

 {TsgGContour}

procedure TsgGContour.InitLeadsPts;
var
  vCnt: Integer;
begin
  if Assigned(FGPolyline) then
  begin
    vCnt := FGPolyline.PointCount;
    FLeadIn.InitPts(FGPolyline.Points[0], FGPolyline.Points[1]);
    if IsEqualFPoints(FGPolyline.PolyPoints.Last, FGPolyline.Points[vCnt - 2]) then
      FLeadOut.InitPts(FGPolyline.PolyPoints.Last, FGPolyline.Points[vCnt - 3])
    else
      FLeadOut.InitPts(FGPolyline.PolyPoints.Last, FGPolyline.Points[vCnt - 2]);
    if FLeadIn.LeadType <> gltNone then
      FLeadIn.Remake;
    if FLeadOut.LeadType <> gltNone then
      FLeadOut.Remake;
  end;
end;



procedure TsgGContour.AddRadiusCompGRBL(const AEquidistant: TsgEquidistantContour);
begin
  if Assigned(FEqRadiusCompGRBL) then
    FreeAndNil(FEqRadiusCompGRBL);
  FEqRadiusCompGRBL := AEquidistant;
  if Assigned(FEqRadiusCompGRBL) then
    FEqRadiusCompGRBL.SetRadiusCompParams(FRadiusComp, FToolParams);
end;

procedure TsgGContour.AssignEntity(Source: TsgDXFEntity);
var
  vContour: TsgGContour absolute Source;
begin
  inherited AssignEntity(Source);
  FBoring := vContour.FBoring;
  FCenterDrill := vContour.FCenterDrill;
  FGPolyline.Assign(vContour.FGPolyline);
  FEnable := vContour.FEnable;
  FFeedDrill := vContour.FFeedDrill;
  FLeadIn.AssignEntity(vContour.FLeadIn);
  FLeadOut.AssignEntity(vContour.FLeadOut);
  FName := vContour.FName;
  FRaduisCorNumber := vContour.FRaduisCorNumber;
  FRadiusComp := vContour.FRadiusComp;
  FRPlane := vContour.FRPlane;
  FShapeType := vContour.FShapeType;
  FToolParams := vContour.FToolParams;
end;

function TsgGContour.CanDraw: Boolean;
begin
  Result := True;
end;

procedure ReversePolyline(const APolyline: TsgDXFPolyline);
var
  vPoly: TsgDXFPolyline;
begin
  if Assigned(APolyline) then
  begin
    vPoly := TsgDXFPolyline.Create;
    try
      TsgDXFPolylineAccess(vPoly).CopyVertexesExt(APolyline, True, True);
      APolyline.Clear(True);
      TsgDXFPolylineAccess(APolyline).CopyVertexesExt(vPoly, True, False);
    finally
      vPoly.Free;
    end;
  end;
end;

procedure TsgGContour.ChangeDirection;

  function IsPointPoly: Boolean;
  begin
    Result := (FGPolyline.Count = 2) and IsEqualFPoints(FGPolyline.Vertexes[0].Point, FGPolyline.Vertexes[1].Point);
  end;

var
  I: Integer;
begin
  FReverse := not FReverse;
  if Assigned(FGPolyline) and not IsPointPoly then
  begin
    ReversePolyline(FGPolyline);
    Converter.Loads(FGPolyline);
  end;
  InitLeadsPts;
  if Assigned(FEqRadiusCompGRBL) then
    for I := 0 to FEqRadiusCompGRBL.ContourCount - 1 do
      FEqRadiusCompGRBL. Contours[I].ChangeDirection;
end;

procedure TsgGContour.ChangeStartPoint(const APolyLine: TsgDXFPolyline;
  const APoint: TFPoint; ASnapMode: TObjectSnapState);
//var
//  I, vIndex, vShiftCount: Integer;
//  vLine: TsgLine;
//  vStartPointMode: TsgGStartPointMode;
begin
  if Assigned(FEqRadiusCompGRBL) then
  begin
    FreeAndNil(FEqRadiusCompGRBL);
    OnMakeEquidistant(Self, FRadiusComp);
  end
  else
    InitLeadsPts;
   //Реализовать через Вертексы
//  if osEndPt in ASnapMode then
//    vStartPointMode := gspEdge
//  else
//    vStartPointMode := gspNearest;
//  for I := 0 to Count - 2 do
//  begin
//    vLine := MakeLine(Points[I], Points[I + 1]);
//    if IsPointOnSegment(vLine, APoint) then
//    begin
//      Points.Delete(Count - 1);
//      vIndex := I;
//      if FStartPointMode = gspNearest then
//      begin
//        Points.Delete(0);
//        Dec(vIndex);
//      end;
//      if not IsEqualFPoints(APoint, Points[vIndex]) and
//      not IsEqualFPoints(APoint, Points[vIndex + 1]) then
//      begin
//        vShiftCount := vIndex + 1;
//        Points.Insert(vShiftCount, APoint);
//      end
//      else
//        if IsEqualFPoints(APoint, Points[vIndex]) then
//          vShiftCount := vIndex
//        else
//          vShiftCount := vIndex + 1;
//      Points.CyclicShiftRight(vShiftCount);
//      Points.Add(APoint);
//      FStartPoint := APoint;
//      InitLeadsPts;
//      FStartPointMode := vStartPointMode;
//      Break;
//    end;
//  end;
end;

constructor TsgGContour.Create;
begin
  inherited Create;
  FArcs := TList.Create;
  FFeedDrill := 100;
  FRPlane := 10;
  FCenterDrill := cnstFPointZero;
  FContourType := ctMaster;
  FEnable := True;
  FRadiusComp := grcNone;
  FRaduisCorNumber := 0;
  FToolParams := cnstDefToolParams;
  FShapeType := cshFree;
//  FStartPointMode := gspEdge;
  FToolsDispenser := 1;
  FTurnOffPumpDist := 0;
  FTurnOffPumpVerInd := -1;
  FTurnOffPumpVerIndAfter := -1;
  FTurnOffPumpVerIndBefore := -1;
end;

destructor TsgGContour.Destroy;
begin
  FLeadIn.Free;
  FLeadOut.Free;
  FArcs.Free;
  FGPolyline := nil;
  FreeAndNil(FEqRadiusCompGRBL);
  inherited Destroy;
end;

procedure TsgGContour.SetCenterDrill(const AValue: TFPoint);
begin
  FCenterDrill := AValue;
end;

procedure TsgGContour.SetDepthOnZ(const AValue: Double);
begin
  FDepthOnZ := AValue;
end;

procedure TsgGContour.SetDepthPass(const AValue: Double);
begin
  if Sign(FDepthOnZ) = -1 then
    FDepthPass := -AValue
  else
    FDepthPass := AValue;
end;

procedure TsgGContour.SetDistanceOffset(const Value: Double);
begin
  FTurnOffPumpDist := InitDistanceOffsetPoint(Value);
end;

function GetPointOnArcR(const AArc: TsgArcR; AFromStartAngle: Boolean; ALength: Double): TFPoint;
var
  vOffsetAngle, vFromAngle: Double;
begin
  vOffsetAngle := (ALength / AArc.Radius) * (180 / Pi);
  if AFromStartAngle then
    vFromAngle :=  AArc.AngleS + vOffsetAngle
  else
    vFromAngle := AArc.AngleE - vOffsetAngle;
  Result := GetPointOnArcAR(AArc, vFromAngle);
end;

function TsgGContour.InitDistanceOffsetPoint(ADist: Double): Double;
var
  I: Integer;
  vArc, vArc1New, vArc2New: TsgArcR;
  vVer, vVerNext: TsgDXFVertex;
  vDist, vDistPrev, vDistFromPrevPT: Double;
  vPt: TFPoint;
begin
  Result := 0;
  if TurnOffPumpVerInd > -1 then
    GPolyline.DeleteEntity(TurnOffPumpVerInd);
  FTurnOffPumpVerInd := -1;
  if ADist > 0 then
  begin
    vDistPrev := 0;
    for I := GPolyline.Count - 2 downto 0 do
    begin
      vVer := GPolyline.Vertexes[I];
      vVerNext := GPolyline.Vertexes[I + 1];
      if vVer.HasBulge then
      begin
        vArc := GetArcROfBulge(vVer.Point, vVerNext.Point, vVer.Bulge);
        vDist := vDistPrev + GetLengthArcR(vArc);
      end
      else
        vDist := vDistPrev + DistanceFPoint(vVerNext.Point, vVer.Point);
      if vDist > ADist then
      begin
        vDistFromPrevPT := ADist - vDistPrev;
        if vVer.HasBulge then
        begin
          vPt := GetPointOnArcR(vArc, False, vDistFromPrevPT);
          SplitArcPt(vArc, vPt, vArc1New, vArc2New);
          GPolyline.DeleteEntity(I);
          InsertVertexInPolyline(GPolyline, I, vVer.Point, GetBulgeOfArcR(vArc1New, vVer.Point));
          InsertVertexInPolyline(GPolyline, I + 1, vPt, GetBulgeOfArcR(vArc2New, vPt));
        end
        else
        begin
          InsertVertexInPolyline(GPolyline, I + 1, GetPointOnLine(vVerNext.Point, vVer.Point, vDistFromPrevPT));
        end;
        Converter.Loads(GPolyline);
        FTurnOffPumpVerIndBefore := I;
        FTurnOffPumpVerInd := I + 1;
        FTurnOffPumpVerIndAfter := I + 2;
        Result := ADist;
        Break;
      end;
      vDistPrev := vDist;
    end;
  end;
end;

function TsgGContour.GetBoring: Boolean;
begin
  Result := FBoring;
end;

function TsgGContour.GetBox: TFRect;
begin
  Result := FGPolyline.PolyPoints.GetBox;
end;

function TsgGContour.GetConverter: TsgDXFConverter;
begin
  Result := nil;
  if Assigned(FGPolyline) then
    Result := FGPolyline.Converter;
end;

function TsgGContour.GetEnable: Boolean;
begin
  Result := FEnable;
  if Assigned(FEqRadiusCompGRBL) then
    Result := Result and FEqRadiusCompGRBL.Contours[0].FEnable;
end;

function TsgGContour.GetFeedDrill: Integer;
begin
  Result := FFeedDrill;
end;

function TsgGContour.GetIsFromPocket: Boolean;
begin
  Result := Assigned(FPocket);
end;

function TsgGContour.GetLeadIn: TsgGLead;
begin
  if Assigned(FEqRadiusCompGRBL) then
    Result := FEqRadiusCompGRBL.Contours[0].LeadIn
  else
    Result := FLeadIn;
end;

function TsgGContour.GetLeadInReverse: Boolean;
begin
  Result := LeadIn.FReversDirection;
end;

function TsgGContour.GetLeadInTypeInsp: Byte;
begin
  if Assigned(LeadIn) then
    Result := Byte(LeadIn.LeadType)
  else
    Result := Byte(gltNone);
end;

function TsgGContour.GetLeadOut: TsgGLead;
begin
  if Assigned(FEqRadiusCompGRBL) then
    Result := FEqRadiusCompGRBL.Contours[0].LeadOut
  else
    Result := FLeadOut;
end;

function TsgGContour.GetLeadOutReverse: Boolean;
begin
  Result := LeadOut.FReversDirection;
end;

function TsgGContour.GetLeadOutTypeInsp: Byte;
begin
  if Assigned(LeadOut) then
    Result := Byte(LeadOut.LeadType)
  else
    Result := Byte(gltNone);
end;

function TsgGContour.GetName: string;
begin
  Result := FName;
end;

function TsgGContour.GetNumbOfPassesByMachine: Integer;
begin
  if FMachineType in MillMachines then
    Result := Ceil(Abs(FDepthOnZ) / Abs(FDepthPass))
  else
    Result := FNumbOfPasses;
end;

function TsgGContour.GetRadiusComp: Byte;
begin
  Result := Byte(FRadiusComp);
end;

function TsgGContour.GetReverse: Boolean;
begin
  Result := FReverse;
end;

function TsgGContour.GetRPlane: Double;
begin
  Result := FRPlane;
end;

function TsgGContour.GetShapeTypeInsp: Byte;
begin
  Result := Byte(FShapeType);
end;

function TsgGContour.GetToolDispenser: Byte;
begin
  Result := FToolsDispenser;
end;

function TsgGContour.GetToolNumber: Byte;
begin
  Result := ToolParams.Number;
end;

function TsgGContour.GetToolParams: string;
begin
  Result := ToolParamsToInspStr(FToolParams);
end;

function TsgGContour.GetTools: TStringList;
begin
  Result := FTools;
end;

//procedure TsgGContour.EndCountour;
//begin
//
//end;
//
//function TsgGContour.GetGCode: TStrings;
//begin
//
//end;

//procedure TsgGContour.MakeLead;
//
////  procedure AddSubLeadPts(ALead: TsgGLead);
////  var
////    vLine: TsgDXFLine;
////  begin
////    if not Assigned(ALead.FSubEnt) then
////      Exit;
////    vLine := ALead.FSubEnt;
////    if ALead.Purpose = glpIn then
////      APts.Insert(0, vLine.Point.Point2D)
////    else
////      APts.Add(vLine.Point.Point2D);
////  end;
////
////  procedure AddLeadPts(ALead: TsgGLead);
////  var
////    vEnt: TsgDXFEntity;
////    vLine: TsgDXFLine;
////    vGArc: TsgDXFGArc;
////    I: Integer;
////  begin
////    vEnt := ALead.Entity;
////    if Assigned(vEnt) then
////    begin
////      case vEnt.EntType of
////        ceLine:
////          begin
////            vLine := TsgDXFLine(vEnt);
////            if ALead.FPurpose = glpIn then
////              APts.Insert(0, vLine.Point.Point2D)
////            else
////              APts.Add(vLine.Point.Point2D);
////          end;
////        ceArc:
////          begin
////            vGArc := InitGArc(vEnt);
////            vGArc.FNeedRemove := True;
////            case ALead.FPurpose of
////              glpIn:
////                begin
////                  if vGArc.PolyPoints.IndexOf(ALead.FPtEdge) <> 0 then
////                    for I := vGArc.PolyPoints.Count - 2 downto 0 do
////                      APts.Insert(0, vGArc.PolyPoints[I].Point2D)
////                  else
////                    for I := 1 to vGArc.PolyPoints.Count - 1 do
////                      APts.Insert(0, vGArc.PolyPoints[I].Point2D);
////                end;
////              glpOut:
////                begin
////                  if vGArc.PolyPoints.IndexOf(ALead.FPtEdge) = 0 then
////                    for I := 1 to vGArc.PolyPoints.Count - 1 do
////                      APts.Add(vGArc.PolyPoints[I].Point2D)
////                  else
////                    for I := vGArc.PolyPoints.Count - 2 downto 0 do
////                      APts.Add(vGArc.PolyPoints[I].Point2D);
////                end;
////            end;
////          end;
////      end;
////    end;
////  end;
//
//var
//  vLead: TsgGLead;
//begin
////  vLead := FLeadIn;
////  AddLeadPts(vLead);
////  AddSubLeadPts(vLead);
////  vLead := FLeadOut;
////  AddLeadPts(vLead);
////  AddSubLeadPts(vLead);
//end;

procedure TsgGContour.SetBoring(AValue: Boolean);
begin
  FBoring := AValue;
end;

procedure TsgGContour.SetEnable(AValue: Boolean);
begin
  FEnable := AValue;
  if Assigned(FEqRadiusCompGRBL) then
    FEqRadiusCompGRBL.SetEnable(AValue);
end;

procedure TsgGContour.SetFeedDrill(AValue: Integer);
begin
  FFeedDrill := AValue;
end;

procedure TsgGContour.SetLeadInReverse(AValue: Boolean);
begin
  if Assigned(FEqRadiusCompGRBL) then
    FEqRadiusCompGRBL.ChangeLeadInDirection
  else
    LeadIn.ChangeDirection;
end;

procedure TsgGContour.SetLeadInType(AValue: TsgGLeadType);
begin
  LeadIn.LeadType := AValue;
end;

procedure TsgGContour.SetLeadOutReverse(AValue: Boolean);
begin
  if Assigned(FEqRadiusCompGRBL) then
    FEqRadiusCompGRBL.ChangeLeadOutDirection
  else
    LeadOut.ChangeDirection;
end;

procedure TsgGContour.SetLeadOutType(AValue: TsgGLeadType);
begin
  LeadOut.LeadType := AValue;
end;

procedure TsgGContour.SetMachineType(const Value: TsgGCodeMachineTypeID);
var
  vPrevMachineType: TsgGCodeMachineTypeID;
begin
  vPrevMachineType := FMachineType;
  FMachineType := Value;
  if (Value <> vPrevMachineType) then
  begin
    if (vPrevMachineType in GRBLMachines) then
    begin
      if Assigned(FEqRadiusCompGRBL) then
      begin
        FreeAndNil(FEqRadiusCompGRBL);
        SetRadiusComp(FRadiusComp);
      end;
    end
    else
      if Value in GRBLMachines then
        SetRadiusComp(FRadiusComp);
  end;
end;

procedure TsgGContour.SetName(const AName: string);
begin
  FName := AName;
end;

procedure TsgGContour.SetNumbOfPasses(const AValue: Integer);
begin
  if AValue > 0 then
    FNumbOfPasses := AValue;
end;

procedure TsgGContour.SetGPolyline(const AValue: TsgGCodePolyline);
begin
  FGPolyline := AValue;
  if Assigned(FGPolyline) then
  begin
    FLeadIn := TsgGLead.Create(Converter, glpIn);
    FLeadOut := TsgGLead.Create(Converter, glpOut);
    if not FGPolyline.IsPocket then
    begin
      InitLeadsPts;
      FLength := TsgDXFPolylineAccess(FGPolyline).GetLength;
    end;
  end;
end;

procedure TsgGContour.SetRadiusComp(AValue: TsgGRadiusComp);
begin
  if ShapeType = cshPoint then
    Exit;
  FRadiusComp := AValue;
  LeadIn.FGRBL := FMachineType in GRBLMachines;
  LeadOut.FGRBL := LeadIn.FGRBL;
  if not (FMachineType in GRBLMachines) then
  begin
    LeadIn.RadiusComp := AValue;
    LeadOut.RadiusComp := AValue;
  end
  else
    if AValue = grcNone then
      FreeAndNil(FEqRadiusCompGRBL)
    else
      OnMakeEquidistant(Self, AValue);
end;

procedure TsgGContour.SetReverse(AValue: Boolean);
begin
  ChangeDirection;
  InitDistanceOffsetPoint(FTurnOffPumpDist);
end;

procedure TsgGContour.SetRPlane(AValue: Double);
begin
  FRPlane := AValue;
end;

procedure TsgGContour.SetShapeType(const AValue: TsgContourShape);
begin
  FShapeType := AValue;
end;

procedure TsgGContour.SetToolDispenser(AValue: Byte);
begin
  FToolsDispenser := AValue;
end;

procedure TsgGContour.SetToolParams(AParams: TsgGToolParams);
begin
  FToolParams := AParams;
  LeadIn.Radius := AParams.Radius;
  LeadOut.Radius := AParams.Radius;
  if Assigned(FEqRadiusCompGRBL) then
    OnMakeEquidistant(Self, FRadiusComp);
end;

function IsEqualToolParams(AToolParam, AToolParam1: TsgGToolParams): Boolean;
begin
  Result := (AToolParam.Number = AToolParam1.Number) and
    (AToolParam.Radius = AToolParam1.Radius) and
    (AToolParam.Length = AToolParam1.Length);
end;

procedure TsgGContour.SetTools(const AValue: TStringList);
var
  I, vIndex: Integer;
  vNumbs: TsgIntegerList;
  vParams: TsgGToolParams;
begin
  FTools := AValue;
  vNumbs := TsgIntegerList.Create;
  try
    for I := 0 to FTools.Count - 1 do
      vNumbs.Add(GetToolParamFromString(FTools[I]).Number);
    vIndex := vNumbs.IndexOf(ToolParams.Number);
    if vIndex = -1 then
    begin
      vParams := GetToolParamFromString(FTools[0]);
      ToolParams := vParams;
    end
    else
    begin
      vParams := GetToolParamFromString(FTools[vIndex]);
      if not IsEqualToolParams(ToolParams, vParams) then
        SetToolParams(vParams);
    end;
  finally
    vNumbs.Free;
  end;
end;

//procedure TsgGContour.StartContour;
//begin
//
//end;

{ TsgGEQContour }

function TsgEquidistantContour.AddContour(const AContour: TsgGContour): Integer;
begin
  Result := FContours.Add(AContour);
end;

procedure TsgEquidistantContour.ChangeLeadInDirection;
var
  I: Integer;
begin
  for I := 0 to FContours.Count - 1 do
    Contours[I].LeadIn.ChangeDirection;
end;

procedure TsgEquidistantContour.ChangeLeadOutDirection;
var
  I: Integer;
begin
  for I := 0 to FContours.Count - 1 do
    Contours[I].LeadOut.ChangeDirection;
end;

constructor TsgEquidistantContour.Create;
begin
  inherited Create;
  FContours := TsgObjectList.Create;
  FEquidistantType := eqtNone;
end;

destructor TsgEquidistantContour.Destroy;
var
  vGPoly: TsgGCodePolyline;
  I: Integer;
begin
  for I := 0 to FContours.Count - 1 do
  begin
    vGPoly := Contours[I].GPolyline;
    try
      Contours[I].GPolyline := nil;
    finally
      FreeAndNil(vGPoly);
    end;
  end;
  TsgObjectList.FreeList(FContours);
end;

function TsgEquidistantContour.GetContour(Index: Integer): TsgGContour;
begin
  Result := TsgGContour(FContours[Index]);
end;

function TsgEquidistantContour.GetContourCount: Integer;
begin
  Result := FContours.Count;
end;

procedure TsgEquidistantContour.SetContour(Index: Integer;
  const Value: TsgGContour);
begin

end;

procedure TsgEquidistantContour.SetEnable(AValue: Boolean);
var
  I: Integer;
begin
  for I := 0 to FContours.Count - 1 do
    Contours[I].Enable := AValue;
end;

procedure TsgEquidistantContour.SetRadiusCompParams(ARadiusComp: TsgGRadiusComp; const AToolsParams: TsgGToolParams);
var
  I: Integer;
  vContour: TsgGContour;
begin
  for I := 0 to FContours.Count - 1 do
  begin
    vContour := TsgGContour(FContours[I]);
    if Assigned(vContour) then
    begin
      vContour.LeadIn.RadiusComp := ARadiusComp;
      vContour.LeadOut.RadiusComp := ARadiusComp;
      vContour.LeadIn.Radius := AToolsParams.Radius;
      vContour.LeadOut.Radius := AToolsParams.Radius;
    end;
  end;
end;

{ TsgDXFEqLine }

procedure TsgDXFEqLine.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  FInitEdgeCount := TsgDXFEqLine(Source).InitEdgeCount;
  FSourceLine := TsgDXFEqLine(Source).SourceLine;
  FStartOriginPoint := TsgDXFEqLine(Source).FStartOriginPoint;
  FEndOriginPoint := TsgDXFEqLine(Source).FEndOriginPoint;
end;

procedure TsgDXFEqLine.DecEdgeCount;
begin
  Dec(FInitEdgeCount);
end;

procedure TsgDXFEqLine.IncEdgeCount;
begin
  Inc(FInitEdgeCount);
end;

procedure TsgDXFEqLine.SetSourceLine(const Value: TsgLine);
begin
  FSourceLine := Value;
end;

{ TsgDXFEqGArc }

procedure TsgDXFEqGArc.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  FArcType := TsgDXFEqGArc(Source).ArcType;
  FInitAngleCount := TsgDXFEqGArc(Source).InitAngleCount;
end;

procedure TsgDXFEqGArc.IncAngleCount;
begin
  Inc(FInitAngleCount);
end;

{ TsgGPocket }

procedure TsgGPocket.AddContour(const AContour: TsgGContour);
begin
  FContours.Add(AContour);
end;

procedure TsgGPocket.ApplyParams(const AParams: TsgPocketParams);
begin
  Direction := AParams.Direction;
  OverLapPercent := AParams.OverLapPercent;
  DepthOnZ := AParams.DepthOnZ;
  DepthPass := AParams.DepthPass;
  ToolParams := AParams.Tool;
end;

constructor TsgGPocket.Create(const ASourceContour: TsgGContour; const AConverter: TsgDXFConverter);
begin
  inherited Create;
  FConverter := AConverter;
  FComponents := TsgList.Create;
  FSourceContour := ASourceContour;
  FContours := TsgList.Create;
end;

procedure TsgGPocket.GenerateContours;

  procedure MakeContour(var AContour: TsgGContour; var APoly: TsgGCodePolyline);
  var
    vName: string;
  begin
    APoly := TsgGCodePolyline.CreateEx(FSourceContour.LayerName);
    APoly.IsPocket := True;
    AContour := TsgGContour.Create;
    APoly.LinkEntity := AContour;
    AContour.GPolyline := APoly;
    AContour.FPocket := Self;
    vName := 'Pocket_' + IntToStr(FContours.Count + 1);
    AContour.Name := vName;
  end;

var
  I, J: Integer;
  vVer: TsgDXFVertex;
  vGPoly, vTrPoly: TsgGCodePolyline;
  vComponent: TObject;
  vTransition: TsgTransition;
  vPathContour, vContour: TsgGContour;
begin
  MakeContour(vPathContour, vGPoly);
  AddContour(vPathContour);
  if Direction = pdOut then
    Components.Flip;
  for I := 0 to FComponents.Count - 1 do
  begin
    vComponent := FComponents[I];
    if vComponent is TsgTransition then
    begin
      vTransition := TsgTransition(vComponent);
      if vTransition.TranType = ttExternal then
      begin
        MakeContour(vPathContour, vGPoly);
        AddContour(vPathContour);
      end
      else
      begin
        vTrPoly := MakeTransitionPoly(vTransition);
        try
          if IsCrossPolylines(vTrPoly.PolyPoints, False, FSourceContour.GPolyline.PolyPoints, False, nil, 0, True) > 0 then
          begin
            MakeContour(vPathContour, vGPoly);
            AddContour(vPathContour);
          end
          else
          begin
            for J := 0 to vTrPoly.Count - 1 do
            begin
              vVer := vTrPoly.Vertexes[J];
              AddVertexInPolyline(vGPoly, vVer.Point, vVer.Bulge);
            end;
          end;
        finally
          vTrPoly.Free;
        end;
      end;
    end
    else
    begin
      vContour := FComponents[I];
      for J := 0 to vContour.GPolyline.Count - 1 do
      begin
        vVer := vContour.GPolyline.Vertexes[J];
        AddVertexInPolyline(vGPoly, vVer.Point, vVer.Bulge);
      end;
      if I = FComponents.Count - 1 then
      begin
        vVer := vContour.GPolyline.Vertexes[vContour.GPolyline.Count - 1];
        AddVertexInPolyline(vGPoly, vVer.Point, vVer.Bulge);
      end;
    end;
  end;
  for I := 0 to FContours.Count - 1 do
  begin
    InitContourDepthAndPasses(FContours[I], DepthOnZ, DepthPass, 1);
    TsgGContour(FContours[I]).ToolParams := ToolParams;
  end;
end;

function TsgGPocket.MakeTransitionPoly(const ATransiton: TsgTransition): TsgGCodePolyline;

  procedure SetStartNearestContourPoint(const APoly: TsgDXFPolyline; const AStartPoint: TFPoint);
  var
    I, vInd: Integer;
    vDist, vDistCur: Double;
  begin
    vInd := 0;
    vDist := DistanceFPoint2D(AStartPoint, APoly.Vertexes[0].Point);
    for I := 1 to APoly.Count - 1 do
    begin
      vDistCur := DistanceFPoint2D(AStartPoint, APoly.Vertexes[I].Point);
      if vDistCur < vDist then
      begin
        vDist := vDistCur;
        vInd := I;
      end;
    end;
    if vInd <> 0 then
      TsgDXFPolylineAccess(APoly).SetStartIndexPoint(vInd, True);
  end;

begin
  Result := TsgGCodePolyline.Create;
  if Direction = pdIn then
  begin
    AddVertexInPolyline(Result, ATransiton.Poly1.PolyPoints.Last);
    SetStartNearestContourPoint(ATransiton.Poly2, ATransiton.Poly1.PolyPoints.Last);
    FConverter.Loads(ATransiton.Poly2);
    AddVertexInPolyline(Result, ATransiton.Poly2.PolyPoints.First);
  end
  else
  begin
    AddVertexInPolyline(Result, ATransiton.Poly2.PolyPoints.Last);
    SetStartNearestContourPoint(ATransiton.Poly1, ATransiton.Poly2.PolyPoints.Last);
    FConverter.Loads(ATransiton.Poly1);
    AddVertexInPolyline(Result, ATransiton.Poly1.PolyPoints.First);
  end;
  FConverter.Loads(Result);
end;

destructor TsgGPocket.Destroy;

  procedure FreeComponents;
  var
    I: Integer;
  begin
    for I := 0 to FComponents.Count - 1 do
    begin
      if TObject(FComponents[I]) is TsgGContour then
      begin
        TsgGContour(FComponents[I]).GPolyline.Free;
      end;
      TObject(FComponents[I]).Free;
    end;
    FComponents.Free
  end;

begin
  FreeComponents;
  FContours.Free;
  inherited Destroy;
end;

{ TsgTransition }

constructor TsgTransition.Create(AType: TsgTransitionType);
begin
  inherited Create;
  FType := AType;
end;

initialization
  MillMachines := [mtMill, mtGRBLRouter];
  LaserMachines := [mtCut, mtGRBLLaser];
  GRBLMachines := [mtGRBLLaser, mtGRBLRouter];

finalization

end.
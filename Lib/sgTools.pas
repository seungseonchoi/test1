{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                      Auxiliary tools                       }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}
unit sgTools;
{$INCLUDE SGDXF.inc}

interface
{$IFDEF SGDEL_XE2}
{$DEFINE CONTROLS_TYPES}
{$ELSE}
{$IFDEF CS_USEFORM}
{$DEFINE CONTROLS_TYPES}
{$ENDIF}
{$ENDIF}

{$DEFINE SG_USEMETAFILE}
{$IFDEF SGFPC}
  {$UNDEF SG_USEMETAFILE}
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  {$UNDEF SG_USEMETAFILE}
{$ENDIF}

uses
{$IFDEF SG_FIREMONKEY}
  FMX.Types, FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Canvas.GPU,
  FMX.Surfaces, System.Math.Vectors, FMX.Controls,
{$ELSE}
  Graphics,
{$ENDIF}

{$IFDEF SG_WINAPI}
  Windows, Messages,
{$ELSE}
  {$IFNDEF SG_NON_WIN_PLATFORM}
    Windows, Messages,
  {$ENDIF}
{$ENDIF}

{$IFDEF SGFPC}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes, CADImage, DXFConv, sgConsts
  {$IFNDEF sgDXFONLY}
  , HPGL2
  {$ENDIF}
  , SysUtils, sgSelection, sgFunction, sgLists
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  {$IFDEF CS_USEFORM}
  , Controls, Forms
  {$ENDIF}
  {$IFDEF SGVER_5_2_5}
  , ExtCtrls
  {$ENDIF}
  ;

const
//  cnstEnableSearchCrossInInserts: Boolean = True;

  sUnits: string = 'Units';
  sInch: string = 'Inches';
  sMM: string = 'MM';
  cnstSnapDistance = 10;
  cnstSearchSnapPrecision = cnstSnapDistance * 2;

{$IFDEF SG_NON_WIN_PLATFORM}
  WM_USER             = $0400;
{$ENDIF}
  WM_NAVIGATOR_BASE = WM_USER + 31;
  WM_SCALECHANGING  = WM_NAVIGATOR_BASE + 1;
  WM_PICTUREMOVE    = WM_NAVIGATOR_BASE + 2;

  // notification WM_PICTURECHANGE:
  //  - WParam = Integer(Self),
  //  - LParam = Integer(FTmpGraphic); reference to previous graphic
  WM_PICTURECHANGE  = WM_NAVIGATOR_BASE + 3;

  // notification WM_GETIMAGEEXTENTS:
  //  - WParam = addres of PFRect,
  //  - LParam = Integer(FPicture.Graphic) reference to graphic
  WM_GETIMAGEEXTENTS  = WM_NAVIGATOR_BASE + 4;

  // notification WM_GETIMAGEBOX:
  //  - WParam = addres of PFRect,
  //  - LParam = Integer(FPicture.Graphic) reference to graphic
  WM_GETIMAGEBOX  = WM_NAVIGATOR_BASE + 5;

  // notification WM_ZOOM_EXTENTS:
  WM_ZOOM_EXTENTS = WM_NAVIGATOR_BASE + 6;


type
  TsgCustomControlToolProps = class;

  TSnapPointData = record
    Point: TPoint;
    FPoint: TFPoint;
    Entity: TsgDXFEntity;
    SnapState: TObjectSnapState;
  end;

  TShortIntPoint = packed record
    x, y: ShortInt;
  end;
  TShortIntPointList = array of TShortIntPoint;

  TOnSnapDraw = procedure(Sender: TObject; Canvas: TCanvas; const APoint: TPoint;
    ASnap: TObjectSnapMode; var ADefaultDraw: Boolean) of object;

  TSnapControl = class;

  TsgObjectSnap = class(TObject)
  private
    FSnapDistance: Integer;
    FCADImage: TsgCADImage;
    FHelix: TShortIntPointList;
    function FindSnap(const APoint: TPoint): PSnapData;
    procedure SetSnapDistance(const AVal: Integer);
  public
    constructor Create;
    class function SnapDataEmpty: TSnapData;
    function GetSnapData(const APoint: TPoint; var ASnap: TSnapData): Boolean;
    function GetSnapPoint(AVisiblePoint: TPoint; var AObjectSnapState: TObjectSnapState;
             var AEntity: TsgDXFEntity): TSnapData;
    function GetSnapPointUCS(AVisiblePoint: TPoint; var AObjectSnapState: TObjectSnapState;
             var AEntity: TsgDXFEntity; var CoordsInUCS: TFPoint): TSnapData; overload;
    procedure SetsgDXFImage(ACADImage: TsgCADImage);
    property SnapDistance: Integer read FSnapDistance write SetSnapDistance;
  end;

  TSnapMarker = class(TObject)
  private
    FDrawAreaSize: Integer;
    FPoint: TPoint;
    FRealPoint: TFPoint;
    FDestinationSnap: TObjectSnapMode;
    FVisible: Boolean;
    FSnapControl: TSnapControl;
{$IFNDEF SG_FIREMONKEY}
    FPen: TPen;
    FPenStore: TPen;
{$ELSE}
    FPenColor: TAlphaColor;
{$ENDIF}
    FIsTraced: Boolean;
    FBaseMatrixSnapType: TObjectSnapMode;
    procedure Repaint;
    procedure SetIsTraced(const Value: Boolean);
    procedure SetBaseMatrixSnapType(const Value: TObjectSnapMode);
  protected
    procedure DoDraw(Canvas: TCanvas; const APoint: TPoint; ASnap: TObjectSnapMode);
    property RealPoint: TFPoint read FRealPoint write FRealPoint;
    property DestinationSnap: TObjectSnapMode read FDestinationSnap write FDestinationSnap;
    property Visiblility: Boolean read FVisible write FVisible;
  public
    constructor Create(ASnapControl: TSnapControl);
    destructor Destroy; override;
    procedure ClearPoint;
    procedure Draw(Canvas: TCanvas);
    procedure Snap(APoint: TFPoint; ASnap: TObjectSnapMode);
    property BaseMatrixSnapType: TObjectSnapMode read FBaseMatrixSnapType write SetBaseMatrixSnapType;
    property IsTraced: Boolean read FIsTraced write SetIsTraced;
    property Center: TPoint read FPoint write FPoint;
  end;

  TSnapMarkerTrace = class(TSnapMarker)
  private
    FLineByShowPolar: PsgLineState;
    FLines: TList;
    FSnappedEntity: TsgDXFEntity;
    FSnappedEntityTwo: TsgDXFEntity;
    function GetRealPoint: TFPoint;
    procedure SetSnappedEntity(const Value: TsgDXFEntity);
  public
    constructor Create(ASnapControl: TSnapControl);
    destructor Destroy; override;
    property LineByShowPolar: PsgLineState read FLineByShowPolar;
    property RealPoint: TFPoint read GetRealPoint;
    property SnappedEntity: TsgDXFEntity read FSnappedEntity write SetSnappedEntity;
  end;

  TsgDXFLineTrace = class(TsgDXFLine)
  private
    FSnapType: TObjectSnapMode;
    procedure SetSnapType(const Value: TObjectSnapMode);
  public
    constructor Create; override;
    property SnapType: TObjectSnapMode read FSnapType write SetSnapType;
  end;

  TsgTracers = class
  private
    FCADImage: TsgCADImage;
    FSnapControl: TSnapControl;
    FTracers: TList;
    FTracersMatrix: TsgSelectionMatrix;
    FTracerPointsInt: array[0 .. 5] of Integer;
    FViewRect: TFRect;
    FBasePoint: TFPoint;
    FIsShiftPressed: Boolean;
    FIsShiftClicked: Boolean;
    function GetTracers(Index: Integer): TSnapMarkerTrace;
    procedure SetTracersMatrix(const Value: TsgSelectionMatrix);
    procedure SetViewRect(const Value: TFRect);
    procedure SetCADImage(const Value: TsgCADImage);
    procedure SetBasePoint(const Value: TFPoint);
    procedure SetIsShiftPressed(const Value: Boolean);
    procedure SetIsShiftClicked(const Value: Boolean);
  protected
    function Find(const APoint: TFPoint; var Index: Integer): Boolean;
    procedure ClearMarkers;
  public
    constructor Create(AsnapControl: TSnapControl);
    destructor Destroy; override;
    function Active: Boolean;
    procedure Add(Apoint: TFPoint; ASnapControl: TSnapControl; ASnappedEntity, ASnappedEntityTwo: TsgDXFEntity);
    function AddLine(ATrace: TSnapMarkerTrace; Apoint: TFPoint; ALine: TsgDXFLine): TsgDXFLine;
    procedure AddPointsToTracerMatrix(ATrace: TSnapMarkerTrace; ALine: TsgDXFLineTrace);
    {$IFDEF SG_BTI}
    procedure AddPolyPolyline2D(ATrace: TSnapMarkerTrace; Apoint: TFPoint; APoly: TsgCADPolyPolyline2D );
    {$ENDIF}
    procedure Clear;
    procedure ClearHidden;
    function Count: Integer;
    function IndexOfEntity(AEntity: TObject): Integer;
    procedure Regen;
    function Remove(Apoint: TFPoint): Integer;
    procedure Delete(Index: Integer);
    procedure SetDisable;
    function IndexOf(Apoint: TFPoint): Integer;
    function Last: TSnapMarkerTrace;
    property BasePoint: TFPoint read FBasePoint write SetBasePoint;
    property CADImage: TsgCADImage read FCADImage write SetCADImage;
    property IsShiftPressed: Boolean read FIsShiftPressed write SetIsShiftPressed;
    property IsShiftClicked: Boolean read FIsShiftClicked write SetIsShiftClicked;
    property Tracers[Index: Integer]: TSnapMarkerTrace read GetTracers; default;
    property TracersMatrix: TsgSelectionMatrix read FTracersMatrix write SetTracersMatrix;
    property ViewRect: TFRect read FViewRect write SetViewRect;
  end;

  TSnapControl = class
  private
    FBasePoint: PFPoint;
    FImg: TsgCADImage;
    FInsert: TObject;
    FIsTimeForTrace: Boolean;
    FObjectSnap: TsgObjectSnap;
    FControlProps: TsgCustomControlToolProps;
    FSelectionMatrix: TsgSelectionMatrix;
    FSnapMarker: TSnapMarker;
    FSnappedEntity: TsgDXFEntity;
    FSnappedEntityForTrace: TsgDXFEntity;
    FSnappedEntityForTraceTwo: TsgDXFEntity;
    FSnapMask: TObjectSnapState;
    FTime: Cardinal;
    FUpdating: Boolean;
    FIsNewPointAdded: Boolean;
    FTracers: TsgTracers;
    FForBiddenSnapPoint: TFPoint;
    FSnapData: TSnapData;
    FConstants: TsgConstantsCustom;
{$IFDEF SGVER_5_2_5}
    FImageOnPaint: TNotifyEvent;
{$ENDIF}
    FOnSnapDraw: TOnSnapDraw;
    function GetControlProps: TsgCustomControlToolProps;
    procedure ResetEvents;
    procedure SetControlProps(AValue: TsgCustomControlToolProps);
    procedure SetEvents;
    procedure SetSnapMask(Value: TObjectSnapState);
    function GetCanvas: TCanvas;
    procedure SetIsNewPointAdded(const Value: Boolean);
    procedure SetSnapMarker(const Value: TSnapMarker);
    procedure SetForBiddenSnapPoint(const Value: TFPoint);
    procedure SetSnapData(const Value: TSnapData);
    procedure SetTracers(const Value: TsgTracers);
    procedure SetConstants(const Value: TsgConstantsCustom);
    procedure SetBasePoint(const Value: PFPoint);
  protected
    function DoSnapDraw(Canvas: TCanvas; const APoint: TPoint; ASnap: TObjectSnapMode): Boolean; virtual;
    procedure Repaint;
    function GetSelectionMatrix: TsgSelectionMatrix; virtual;
    function GetPoint(const APoint: TFPoint): TPoint;
    property Constants: TsgConstantsCustom read FConstants write SetConstants;
  public
    constructor Create(const AConstants: TsgConstantsCustom);
    destructor Destroy; override;
    procedure Dispatch(var Message); override;
    procedure Clear;
    function GetMouseCoord(X, Y: Integer): TFPoint;
    function GetMouseCoordUCS(X, Y: Integer; var PointInUCS: TFPoint): TSnapData;
    function GetMouseCoordUCSInternal(X, Y: Integer; var PointInUCS: TFPoint): TSnapData;
    function GetMouseCoordUCSTracers(AMatrix: TsgSelectionMatrix; X, Y: Integer; var PointInUCS: TFPoint): TSnapData;
    property IsNewPointAdded: Boolean read FIsNewPointAdded write SetIsNewPointAdded;
{$IFDEF SG_FIREMONKEY}
    procedure SetColor(AIsBlackBaground: Boolean);
{$ENDIF}
    procedure Update;
    property BasePoint: PFPoint read FBasePoint write SetBasePoint;
    property ControlProps: TsgCustomControlToolProps read GetControlProps write SetControlProps;
    property ForBiddenSnapPoint: TFPoint read FForBiddenSnapPoint write SetForBiddenSnapPoint;
    property SnapData: TSnapData read FSnapData write SetSnapData;
    property SnapMarker: TSnapMarker read FSnapMarker write SetSnapMarker;
    property SnappedEntity: TsgDXFEntity read FSnappedEntity;
    property SnapMask: TObjectSnapState read FSnapMask write SetSnapMask;
    property Tracers: TsgTracers read FTracers write SetTracers;
    property Canvas: TCanvas read GetCanvas;
    property OnSnapDraw: TOnSnapDraw read FOnSnapDraw write FOnSnapDraw;
  end;

  TSnapControlTracers = class(TSnapControl)
  private
    procedure SetSelectionMatrix(const Value: TsgSelectionMatrix);
  protected
    function GetSelectionMatrix: TsgSelectionMatrix; override;
  public
    property SelectionMatrix: TsgSelectionMatrix read GetSelectionMatrix write SetSelectionMatrix;
  end;

  TsgControlTool = class
  private
    FPictureRect: TFRect;
    FInvertDrawMatrix: TFMatrix;
    FImg: TsgCADImage;
    FControlProps: TsgCustomControlToolProps;
    FLinearDimensionsMode: TsgDistUnitsMode;
    FLinearDimensionsScale: Double;
    FMatrixChanged: TNotifyEvent;
    function GetImg: TsgCADImage;
    function GetIsNotCustomDraw: Boolean;
    procedure MatrixChanged(Sender: TObject);
    function GetViewportRect: TRect;
    procedure SetViewportRect(const Value: TRect);
    procedure CADImageDestroy(Sender: TObject);
    procedure SetImg(AImage: TsgCADImage);
  public
    constructor Create(AControlProps: TsgCustomControlToolProps);
    destructor Destroy; override;
    procedure ApplyScale(const ARect: TRect); overload;
    procedure ApplyScale(const ARect: TF2DRect); overload;
    function ConvertUnits(Pt: TFPoint): TFPoint;
    procedure CoordinateParameters(var AName: string; var AFactor: Double);
    function GetActualGraphic: TGraphic;
    function GetDrawMatrix: TFMatrix;
    function GetRealImageMatrix: TFMatrix;
    procedure ResetRotCenter(const ACenter: TPoint; AOrbitAutoTarget: Boolean);
    procedure MovePositionToPoint(const APosition: TFPoint; const APoint: TPoint);
    function GetFPictureRect: TFRect;
    function GetPictureRect2D: TF2DRect;
    function GetInvertDrawMatrix: TFMatrix;
    function PointToPosition(const APoint: TPoint): TFPoint;
    function Point2DToPosition(const APoint: TF2DPoint): TFPoint;

    function UpdateImg: Boolean;
    procedure SetDrawMatrix(const AMatrix: TFMatrix);
    property Img: TsgCADImage read GetImg;
    property IsNotCustomDraw: Boolean read GetIsNotCustomDraw;
    property LinearDimensionsMode: TsgDistUnitsMode read FLinearDimensionsMode write FLinearDimensionsMode;
    property LinearDimensionsScale: Double read FLinearDimensionsScale write FLinearDimensionsScale;
    property ViewportRect: TRect read GetViewportRect write SetViewportRect;
  end;

  TsgSysMenuIcon = (smiMinimize, smiRestore, smiClose);

  TsgSysMenuIcons = class
  private
    FImageClose: TGraphic;
    FImageMinimaze: TGraphic;
    FImageRestoreDown: TGraphic;
    FColor: TColor;
    FControlProps: TsgCustomControlToolProps;
{$IFDEF CS_USEFORM}
    FControlShowHint: Boolean;
{$ENDIF}
{$IFDEF CONTROLS_TYPES}
    FCursor: TCursor;
{$ENDIF}
    FCursorNeedStore: Boolean;
    FDown: Boolean;
    FHeight: Integer;
    FHintClose: string;
    FHintMinimize: string;
    FHintRestoreDown: string;
    FInvertColor: Boolean;
    FWidth: Integer;
    procedure InitializeHints;
    procedure Invalidate;
    function GetHints(Index: TsgSysMenuIcon): string;
    function GetImage(Index: TsgSysMenuIcon): TGraphic;
    procedure SetButtonImage(var AImage: TGraphic; const AValue: TGraphic);
    procedure SetColor(const AValue: TColor);
    procedure SetHeight(const AValue: Integer);
    procedure SetHintClose(const AValue: string);
    procedure SetHintMinimize(const AValue: string);
    procedure SetHintRestoreDown(const AValue: string);
    procedure SetHints(Index: TsgSysMenuIcon; const AValue: string);
    procedure SetInvertColor(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
    procedure SetImage(Index: TsgSysMenuIcon; const AValue: TGraphic);
    procedure SetImageClose(const Value: TGraphic);
    procedure SetImageMinimaze(const Value: TGraphic);
    procedure SetImageRestoreDown(const Value: TGraphic);
  protected
    {$IFDEF SG_WINAPI}
    function DoMouseDown(var Message: TWMMouse): Boolean;
    function DoMouseMove(var Message: TWMMouse): Boolean;
    function DoMouseUp(var Message: TWMMouse): Boolean;
    {$ENDIF}
    function GetIconFromPoint(const APoint: TPoint): TsgSysMenuIcon;
    procedure Paint(ADC: HDC); virtual;
    function PtInIcon(const ASysMenuIcon: TsgSysMenuIcon;
      const APoint: TPoint): Boolean;
  public
    constructor Create(AControlProps: TsgCustomControlToolProps);
    destructor Destroy; override;
    procedure Dispatch(var Message); override;
    function GetSysMenuIconRect(const ASysMenuIcon: TsgSysMenuIcon): TRect;
    function PointInIcons(const APoint: TPoint): Boolean;
    property Color: TColor read FColor write SetColor;
    property ControlProps: TsgCustomControlToolProps read FControlProps;
    property Height: Integer read FHeight write SetHeight;
    property HintClose: string read FHintClose write SetHintClose;
    property HintMinimize: string read FHintMinimize write SetHintMinimize;
    property HintRestoreDown: string read FHintRestoreDown write SetHintRestoreDown;
    property Hints[Index: TsgSysMenuIcon]: string read GetHints write SetHints;
    property ImageClose: TGraphic read FImageClose write SetImageClose;
    property ImageMinimaze: TGraphic read FImageMinimaze write SetImageMinimaze;
    property ImageRestoreDown: TGraphic read FImageRestoreDown write SetImageRestoreDown;
    property Image[Index: TsgSysMenuIcon]: TGraphic read GetImage write SetImage; default;
    property InvertColor: Boolean read FInvertColor write SetInvertColor;
    property Width: Integer read FWidth write SetWidth;
  end;

  TsgCustomControlToolProps = class(TPersistent)
  protected
    function GetViewRect: TRect; virtual; abstract;
    function GetClipBox: TFRect; virtual; abstract;
    function GetClipRect: TRect; virtual; abstract;
    function GetGraphic: TGraphic; virtual;
    function GetCanvas: TCanvas; virtual;
    function GetViewRectMode: Boolean; virtual;
    function IsClipRectActive: Boolean; virtual;

    function ClientToWorld(X, Y: Double; AUCSPoint: PFPoint = nil): TFPoint; virtual;
    function WorldToClient(const APoint: TFPoint): TPoint; virtual;
    function ScreenToClient(P: TPoint): TPoint; virtual;
    function ClientToScreen(P: TPoint): TPoint; virtual;
    procedure Invalidate; virtual;

    function AddClient(AClient: TObject): Integer; virtual;
    function RemoveClient(AClient: TObject): Integer; virtual;
  end;

  function ActiveVPort(AGraphic: TGraphic): TFRect;
  {$IFNDEF sgDXFONLY}
  function GetAreaHPGL(AList: TList; AHPGLImage: TsgHPGLImage): Double;
  {$ENDIF}
  function GetArea(AList: TList): Double; overload;
  function GetArea(AList: TFPointList): Double; overload;

  function GetActualGraphic(ACADImage: TGraphic;
    const APictureRect, AViewRect: TRect; const AUseViewRect: Boolean;
    const AViewBox: PFRect = nil): TGraphic;
  function GetDistanceUnits(ADXFImage: TsgCADImage; ALayout: TsgDXFLayout;
         var UnitsUserName: string; var UnitsUserFactor: Double;
         var AltUnitsUserName: string; var AltUnitsUserFactor: Double): TsgDistanceUnits;
  procedure GetDrawingSize(AGraphic: TsgCADImage; ALayout: TsgDXFLayout;
    var Width, Height: TsgFloat; var Units: CADImage.TsgDistanceUnits);
{$IFDEF SGVER_5_2_5}
  function GetRealPointUsingsgImagePoint(AImage: TImage; X, Y: Integer; var PtInUCS: TFPoint): TFPoint;
{$ENDIF}
function ReloadInserts(const ABlock: TsgDXFBlock; const AConv: TsgDXFConverter;
  AEnt: TsgDXFEntity): Boolean;

implementation

uses
  Math
  {$IFDEF SGVER_5_2_5}
  , sgImage
  {$ENDIF}
{$IFDEF SG_FM_WINDOWS}
  , FMX.Platform.Win//, Winapi.GDIPAPI, Winapi.GDIPOBJ//, Winapi.GDIPUTIL
{$ENDIF}
  ;

const
  cnstEdgeSnapMask: TObjectSnapState = [osMiddle, osNearest, osNormal];
  cnstExtensionSnapMask: TObjectSnapState = [osTrace, osExtension, osParallel];
  cnstFirstSnapMask: TObjectSnapState = [osDisabled, osNearest];
  cnstIntersectionSnapMask: TObjectSnapState = [osIntersection];
  cnstMaskNotUseIterate: TObjectSnapState = [osAngle, osOrtho, osGrid, osPolar,
    osTrace];
  cnstObjectSnapMask: TObjectSnapState = [osInsert, osEndPt, osCenter,
    osQuadrant, osMiddle];
  cnstTangentSnapMask: TObjectSnapState = [osTangent];

type
  TsgCADImageAccess = class(TsgCADImage);
  TsgCADConverter = class(TsgDXFConverter);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgDXFCircleAccess = class(TsgDXFCircle);
  TsgDXFPenLineAccess = class(TsgDXFPenLine);
{$IFDEF CS_USEFORM}
  TControlAccess = class(TControl);
{$ENDIF}
  TCanvasAccess = class(TCanvas);
  PsgPoint = ^TPoint;

  TsgSnapType = (stUndefined, stArc, stLine, stSolid, stPolyline,
    stPolyPolyline, stObjects);

  TsgCrossProcs = class of TsgCrossProc;

  TsgCrossProc = class
  private
    FArc: TsgArcR;
    FBox: TFRect;
    FCrossPoint: TFPoint;
    FCrossProcArc: TsgCrossProc;
    FCrossProcLine: TsgCrossProc;
    FDistance: Double;
    FEntity1: Pointer;
    FEntity2: Pointer;
    FMatrix1: Pointer;
    FMatrix2: Pointer;
    FLine: TsgLine;
    FConstants: TsgConstantsCustom;
    class procedure CreateAndCopyMatrix(var AField: Pointer; const AValue: PFMatrix);
    class function GetMatrix(const AField: PFMatrix): TFMatrix;
    function GetMatrix1: TFMatrix;
    function GetMatrix2: TFMatrix;
    class function GetOffsetPoint(const AMatrix: PFMatrix): PFPoint;
    class procedure SetMatrix(var AField: Pointer; const AValue: TFMatrix);
    procedure SetMatrix1(const AValue: TFMatrix);
    procedure SetMatrix2(const AValue: TFMatrix);
    class procedure SetMatrixPtr(var AField: Pointer; const AValue: PFMatrix);
    procedure SetMatrixPtr1(const AValue: Pointer);
    procedure SetMatrixPtr2(const AValue: Pointer);
  protected
    class function CrossSegments(const AL1, AL2: TsgLine;
      var APoint: TFPoint): Boolean;
    function CrossArcAndArc(const AArc1,AArc2: TsgArcR): Boolean;
    function CrossLineAndArc(const ALine: TsgLine; const AArc: TsgArcR): Boolean;
    function CrossLineAndLine(const ALine1,ALine2: TsgLine): Boolean;
    function CrossWithArc(const Arc: TsgArcR): Boolean; virtual;
    function CrossWithLine(const ALine: TsgLine): Boolean; virtual;
    function CrossWithPts(const AP1, AP2: TFPoint): Boolean;
    procedure SetArc(const AArc: TsgArcR);
    function SetCrossPoint(const APoint: TFPoint): Boolean;
    function SetCrossPoints(const ACount: Integer;
      const ACross1, ACross2: TFPoint): Boolean;
    procedure SetCrossProc(const AClass: TsgCrossProcs;
      var ACrossProc: TsgCrossProc);
    procedure SetLine(const ALine: TsgLine);
    procedure SwapEntities(AIsSwap: Boolean);
  public
    constructor Create(const AEnt1, AEnt2: Pointer; const ABox: TFRect;
      const AConstants: TsgConstantsCustom); virtual;
    destructor Destroy; override;
    procedure CheckType(const AType1, AType2: TsgSnapType); virtual;
    function FindCrossPoint: Boolean; virtual; abstract;
    function Offset1: PFPoint;
    function Offset2: PFPoint;
    property CrossPoint: TFPoint read FCrossPoint;
    property Matrix1: TFMatrix read GetMatrix1 write SetMatrix1;
    property Matrix2: TFMatrix read GetMatrix2 write SetMatrix2;
    property MatrixPtr1: Pointer read FMatrix1 write SetMatrixPtr1;
    property MatrixPtr2: Pointer read FMatrix2 write SetMatrixPtr2;
  end;

  TsgSnapCross = class
  private
    FCrossPoint: TFPoint;
    FConstants: TsgConstantsCustom;
  public
    constructor Create(const AConstants: TsgConstantsCustom);
    function IsCross(const AEnt1, AEnt2: Pointer; const ABox: TFRect): Boolean;
    property CrossPoint: TFPoint read FCrossPoint;
  end;

  TsgCrossArcs = class(TsgCrossProc)
  private
    FArc2: TsgArcR;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossArcAndLine = class(TsgCrossProc)
  public
    function FindCrossPoint: Boolean; override;
    procedure CheckType(const AType1, AType2: TsgSnapType); override;
  end;

  TsgCrossLines = class(TsgCrossProc)
  private
    FLine2: TsgLine;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossSolid = class(TsgCrossProc)
  protected
    function GetEdgeOfSolid(const EdgeIndex: Integer): TsgLine;
  public
    procedure CheckType(const AType1, AType2: TsgSnapType); override;
  end;

  TsgCrossSolidAndArc = class(TsgCrossSolid)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossSolidAndLine = class(TsgCrossSolid)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossSolids = class(TsgCrossSolid)
  public
    function FindCrossPoint: Boolean; override;
  end;

  PsgCrossPolylineSegmentInfo = ^TsgCrossPolylineSegmentInfo;
  TsgCrossPolylineSegmentInfo = record
    PointIndex: Integer;
    SegmentIndex: Integer;
    SplitSegmentStart: Integer;
    SplitSegmentEnd: Integer;
    Distance: Double;
  end;

  TsgCrossPolyline = class(TsgCrossProc)
  private
    FClosed: Boolean;
    FNearests: TList;
    FPoints: TFPointList;
    FPointsWithMatrix: TFPointList;
    FPointsCounts: TsgIntegerList;
    procedure GetSegmentLimits(AOdd: Boolean; const AIndex,AMax: Integer;
      var AStart,AEnd: Integer);
    function SpliteOnPolylines: TList;
    function CrossPolylines(const APolylines: TList): Boolean;
    function CrossPolylinesInternal(const APolylines: TList): Boolean;
  protected
    function FindCrossByEntity: Boolean;
    function FindCrossByEntityWithPointsCounts: Boolean;
    function FindNearestParams: Boolean;
    procedure RenameNearestSegment(AOldSegment,ANewSegment: Integer;
      AIsStart: Boolean);
  public
    constructor Create(const AEnt1, AEnt2: Pointer; const ABox: TFRect;
      const AConstants: TsgConstantsCustom); override;
    destructor Destroy; override;
    procedure CheckType(const AType1, AType2: TsgSnapType); override;
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolylineAndArc = class(TsgCrossPolyline)
  protected
    function CrossWithLine(const ALine: TsgLine): Boolean; override;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolylineAndLine = class(TsgCrossPolyline)
  protected
    function CrossWithLine(const ALine: TsgLine): Boolean; override;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolylineAndSolid = class(TsgCrossPolyline)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolylines = class(TsgCrossPolyline)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolyline = class(TsgCrossProc)
  public
    procedure CheckType(const AType1, AType2: TsgSnapType); override;
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolylineAndArc = class(TsgCrossPolyPolyline)
  protected
    function CrossWithArc(const Arc: TsgArcR): Boolean; override;
    function CrossWithLine(const ALine: TsgLine): Boolean; override;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolylineAndLine = class(TsgCrossPolyPolyline)
  protected
    function CrossWithArc(const Arc: TsgArcR): Boolean; override;
    function CrossWithLine(const ALine: TsgLine): Boolean; override;
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolylineAndSolid = class(TsgCrossPolyPolyline)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolylineAndPolyline  = class(TsgCrossPolyPolyline)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossPolyPolylines = class(TsgCrossPolyPolyline)
  public
    function FindCrossPoint: Boolean; override;
  end;

  TsgCrossInsert = class(TsgCrossProc)
  private
    FObjects: TsgDXFEntity;
    FProxyMatrix: Pointer;
    FSnapType2: TsgSnapType;
  public
    constructor Create(const AEnt1, AEnt2: Pointer; const ABox: TFRect;
      const AConstants: TsgConstantsCustom); override;
    destructor Destroy; override;
    procedure CheckType(const AType1, AType2: TsgSnapType); override;
    function FindCrossPoint: Boolean; override;
    function OffsetMatrixPtr: PFMatrix;
  end;

const
  cnstSqrt2: Double = 1.4142135623730950488016887242097;
  cnstSqrt3: Double = 1.7320508075688772935274463415059;
  cnstMaxCountPointsInPolyLineCross: Integer = 128;
  cnstMaxCountPointsInPolyLineEdge: Integer = 4096;
  cnstMaxCrossRadius = 40;
{$IFDEF SG_BTI}
  cnsrEntTypeExBySnap: set of Byte = [cnstComplexBroad, cnstComplexLinear, cnstArea, cnstComplexArea];
{$ENDIF}
  cnstIconOffset = 2;
  cnstIconWidth = 16;
  cnstIconHeight = 12;

  cnstHintMinimize = 'Minimize';
  cnstHintClose = 'Close';
  cnstHintRestoreDown = 'Restore Down';

var
  SnapCross: TsgSnapCross = nil;
  Helix: TShortIntPointList;
  HelixRadius: Integer = cnstSnapDistance;

procedure EICError;
begin
  raise EInvalidCast.Create('Invalid image type');
end;

function ActiveVPort(AGraphic: TGraphic): TFRect;
var
  P: TsgDXFVPort;
  vM: TFMatrix;
begin
  Result := cnstBadRect;
  if AGraphic is TsgCADImage then
  begin
    P := TsgCADConverter(TsgCADImage(AGraphic).Converter).ActiveVPort;
    vM := TsgCADConverter(TsgCADImage(AGraphic).Converter).ViewTwistMatrix;
    InvertMatrix(vM);
    Result.TopLeft := AddFPoint(P.ViewCenterPoint, P.ViewTarget);
    Result.BottomRight := Result.TopLeft;
    InflateFRect(@Result, 0.5 * P.ViewHeight * P.ViewAspectRatio, 0.5 * P.ViewHeight, 0);
    TransRectCorners(Result, vM);
  end;
end;

function AddPolylineInList(const AList: TList; const ACapacity: Integer;
  const AMode: Boolean): TFPointList;
begin
  AList.Add(Pointer(AMode));
  Result := TFPointList.Create;
  AList.Add(Result);
  Result.Capacity := ACapacity;
end;

function IsBadValue(const AValue: Double): Boolean;
begin
  Result := IsNan(AValue) or IsInfinite(AValue)
end;

{ TODO : function GetActualGraphic for SGFPC }
function GetActualGraphic(ACADImage: TGraphic;
  const APictureRect, AViewRect: TRect; const AUseViewRect: Boolean;
  const AViewBox: PFRect = nil): TGraphic;
var
  vCoeffW, vCoeffH, BX, BY: Double;
  vClipRect, vExts, vNewExts, vViewBox: TFRect;
{$IFDEF SG_USEMETAFILE}
  vMC: TMetafileCanvas;
  vRGN: HRGN;
  vRWidth, vRHeight: Integer;
{$ENDIF}
  vNewImg, vImg: TsgCADImageAccess;
  vRect, vViewRect: TRect;
  vPicRect: TF2DRect;
begin
{$IFDEF SG_USEMETAFILE}
  vRGN := 0;
{$ENDIF}
  vRect := APictureRect;
  if ACADImage is TsgCADImage then
  begin
    vPicRect := MakeF2DRect(APictureRect.Left, APictureRect.Top,
      APictureRect.Right, APictureRect.Bottom);
    Result := TGraphicClass(ACADImage.ClassType).Create;
    vNewImg := TsgCADImageAccess(Result);
    vNewImg.Assign(ACADImage);
    if AUseViewRect then
    begin
      FillChar(vClipRect, SizeOf(vClipRect), 0);
      vImg := TsgCADImageAccess(ACADImage);
      vExts := vImg.Extents;
      vNewImg.IsWithoutBorder := True;
      vNewExts := vNewImg.Extents;
      if vImg.CustomDraw then
      begin
        vViewRect := AViewRect;
        // Borders
        BX := 0.5 * (vExts.Right - vExts.Left - vNewExts.Right + vNewExts.Left);
        BY := 0.5 * (vExts.Top - vExts.Bottom - vNewExts.Top + vNewExts.Bottom);
        // Scaling coefficients
        vCoeffW := (vExts.Right - vExts.Left) / (vPicRect.Right - vPicRect.Left);
        vCoeffH := (vExts.Top - vExts.Bottom) / (vPicRect.Bottom - vPicRect.Top);
        // Clipping rectangle (ignoring Z-coordinate)
        vClipRect.Left := (vViewRect.Left - vPicRect.Left) * vCoeffW - BX;
        vClipRect.Top := (vViewRect.Top - vPicRect.Top) * vCoeffH - BY;
        vClipRect.Right := vClipRect.Left + (vViewRect.Right - vViewRect.Left) * vCoeffW;
        vClipRect.Bottom := vClipRect.Top + (vViewRect.Bottom - vViewRect.Top) * vCoeffH;
      end
      else
      begin
        vPicRect := MakeF2DRect(vNewExts.Left, vNewExts.Bottom, vNewExts.Right, vNewExts.Top);
        vNewImg.ApplyScale(vPicRect.Left, vPicRect.Top, vPicRect.Right, vPicRect.Bottom);
        if Assigned(AViewBox) then
          vViewBox := AViewBox^
        else
          vViewBox := vNewImg.CurrentLayout.Box;
        TransRectCorners(vViewBox, vNewImg.DrawMatrix);
        SwapDoubles(vViewBox.Top, vViewBox.Bottom);
        // Clipping rectangle (ignoring Z-coordinate)
        vClipRect.Left := (vViewBox.Left - vPicRect.Left);
        vClipRect.Top := (vViewBox.Top - vPicRect.Top);
        vClipRect.Right := vClipRect.Left + (vViewBox.Right - vViewBox.Left);
        vClipRect.Bottom := vClipRect.Top + (vViewBox.Bottom - vViewBox.Top);
      end;
      vNewImg.SetClippingRect(@vClipRect);
    end;
  end
  else
  begin
{$IFNDEF SG_USEMETAFILE}
    Result := TGraphicClass(ACADImage.ClassType).Create;
    Result.Assign(ACADImage);
{$ELSE}
    vMC := nil;
    Result := TMetafile.Create;
    try
      vRGN := 0;
      if AUseViewRect then
      begin
        vRect.Left := vRect.Left - AViewRect.Left;
        vRect.Top := vRect.Top - AViewRect.Top;
        vRect.Right := vRect.Right - AViewRect.Left;
        vRect.Bottom := vRect.Bottom - AViewRect.Top;
        vRWidth := AViewRect.Right - AViewRect.Left;
        vRHeight := AViewRect.Bottom - AViewRect.Top;
        TMetafile(Result).Width := vRWidth;
        TMetafile(Result).Height := vRHeight;
        vRGN := CreateRectRgn(0, 0, vRWidth, vRHeight);
      end
      else
      begin
        OffsetRect(vRect, -vRect.Left, -vRect.Top);
        TMetafile(Result).Width := vRect.Right;
        TMetafile(Result).Height := vRect.Bottom;
      end;
      vMC := TMetafileCanvas.Create(TMetafile(Result), 0);
      if vRGN <> 0 then
        SelectClipRgn(vMC.Handle, vRGN);
      vMC.StretchDraw(vRect, ACADImage);
    finally
      if vRGN <> 0 then
        DeleteObject(vRGN);
      vMC.Free;
    end;
{$ENDIF}
  end;
end;

function Is3DRect(const ARect: TFRect): Boolean;
var
  vIs3DCoords: Integer;
begin
  vIs3DCoords := Integer(Abs(ARect.Right - ARect.Left) > fAccuracy);
  Inc(vIs3DCoords, Integer(Abs(ARect.Top - ARect.Bottom) > fAccuracy));
  Inc(vIs3DCoords, Integer(Abs(ARect.Z2 - ARect.Z1) > fAccuracy));
  Result := vIs3DCoords = 3;
end;

function IsEntity(const AP: Pointer): Boolean;
begin
  Result := DXFConv.IsEntity(TObject(AP));
end;

{$IFDEF SG_BTI}
function IsBTI(const AP: Pointer): Boolean;
begin
  Result := False;
  if Assigned(AP) and IsEntity(AP) then
    Result := TsgDXFEntityAccess(AP).GetEntTypeEx in cnsrEntTypeExBySnap;
end;
{$ENDIF}

function IsEntityExtruded(const AEntity: TsgDXFEntity): Boolean;
begin
  Result := False;
  if (AEntity is TsgDXFPenLine) then
    Result := TsgDXFPenLineAccess(AEntity).IsExtruded;
end;

function IsHasSnap(const AEntity: TsgDXFEntity): Boolean;
begin
  Result := not (AEntity.EntType in [ceCircle, ceArc, ceEllipse, ceSpline]);
end;

function IsCircular(const AEntity: TsgDXFEntity): Boolean;
begin
  Result := False;
  if AEntity.EntType in [ceCircle, ceArc] then
    Result := not Extruded(TsgDXFCircle(AEntity).Extrusion);
end;

function IsSolid(const AEntity: TsgDXFEntity): Boolean;
var
  vPenLine: TsgDXFPenLine absolute AEntity;
begin
  if AEntity is TsgDXFPenLine then
    Result := (vPenLine.Lines = nil) or (vPenLine.Lines.IsSolid)
  else
    Result := True;
end;

function IsCrossRects(const AR1, AR2: TFRect): Boolean;
begin
  Result := ((AR1.Left <= AR2.Right) and (AR1.Right >= AR2.Left) and
    (AR1.Bottom <= AR2.Top) and (AR1.Top >= AR2.Bottom)) or
    ((AR1.Left <= AR2.Left) and (AR1.Right >= AR2.Right)
      and (AR1.Bottom <= AR2.Bottom) and (AR1.Top >= AR2.Top));
end;

function IsPointInFRect(const R: TFRect; const P: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsValInParam(P.X, R.Left, R.Right, AAccuracy) and
    IsValInParam(P.Y, R.Bottom, R.Top, AAccuracy) and
    IsValInParam(P.Z, R.Z1, R.Z2, AAccuracy);
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function LoadUser32Str(const AID: Integer): string;
const
  cnstBuffSize = 4096;
var
  vBuf: PChar;
begin
  vBuf := StrAlloc(cnstBuffSize + 1);
  try
    SetString(Result, vBuf, LoadString(GetModuleHandle(user32), AID, vBuf, cnstBuffSize));
  finally
    StrDispose(vBuf);
  end;
end;
{$ENDIF}

function CloneAndMovePoints(const APolyline: TsgCADBasePolyline;
  const AOffs: PFPoint): TFPointList;
var
  I: Integer;
begin
  Result := TFPointList.Create;
  for I := 0 to APolyline.PolyPoints.Count - 1 do
    if Assigned(AOffs) then
      Result.Add(AddFPoint(APolyline.PolyPoints[I], AOffs^))
    else
      Result.Add(APolyline.PolyPoints[I]);
end;

function GetArcR(const AArc: TsgArcR; const AOffs: PFPoint): TsgArcR; overload;
begin
  Result := AArc;
  if Assigned(AOffs) then
    Result.Center := AddFPoint(AArc.Center, AOffs^);
end;

function GetArcR(const AEnt: TsgDXFCircle; const AOffs: PFPoint): TsgArcR; overload;
begin
  if Assigned(AOffs) then
    Result.Center := AddFPoint(AEnt.Point, AOffs^)
  else
    Result.Center := AEnt.Point;
  Result.Radius := AEnt.Radius;
  if AEnt.EntType = ceArc then
  begin
    Result.AngleS := TsgDXFArc(AEnt).StartAngle;
    Result.AngleE := TsgDXFArc(AEnt).EndAngle;
  end
  else
  begin
    Result.AngleS := 0;
    Result.AngleE := 360;
  end;
end;

function GetLine(const ALine: TsgLine; const AOffs: PFPoint): TsgLine; overload;
begin
  Result := ALine;
  if Assigned(AOffs) then
  begin
    Result.Point1 := AddFPoint(Result.Point1, AOffs^);
    Result.Point2 := AddFPoint(Result.Point2, AOffs^);
  end;
end;

function GetLine(const AEnt: TsgDXFLine; const AOffs: PFPoint): TsgLine; overload;
begin
  Result := GetLine(MakeLine(AEnt.Point, AEnt.Point1), AOffs);
end;

function CreateLine(const ALine: TsgLine;
  const ASnapType: TObjectSnapMode = osDisabled): TsgDXFLineTrace;
begin
  Result := TsgDXFLineTrace.Create;
  Result.Point := ALine.Point1;
  Result.Point1 := ALine.Point2;
  Result.SnapType := ASnapType;
end;

function GetRectByPoints(const AP1, AP2: TFPoint): TFRect;
begin
  if AP1.X < AP2.X then
  begin
    Result.Left := AP1.X;
    Result.Right := AP2.X;
  end
  else
  begin
    Result.Left := AP2.X;
    Result.Right := AP1.X;
  end;
  if AP1.Y < AP2.Y then
  begin
    Result.Bottom := AP1.Y;
    Result.Top := AP2.Y;
  end
  else
  begin
    Result.Bottom := AP2.Y;
    Result.Top := AP1.Y;
  end;
  if AP1.Z < AP2.Z then
  begin
    Result.Z1 := AP1.Z;
    Result.Z2 := AP2.Z;
  end
  else
  begin
    Result.Z1 := AP2.Z;
    Result.Z2 := AP1.Z;
  end;
end;

function GetPointInVector(const AP1, AP2: TFPoint; const ADistance: Double): TFPoint;
var
  vLengthOfVector: Double;
begin
  vLengthOfVector := DistanceFPoint(AP1, AP2);
  if Abs(vLengthOfVector) > fAccuracy then
  begin
    Result.X := AP1.X + (AP2.X - AP1.X) * ADistance / vLengthOfVector;
    Result.Y := AP1.Y + (AP2.Y - AP1.Y) * ADistance / vLengthOfVector;
    Result.Z := AP1.Z + (AP2.Z - AP1.Z) * ADistance / vLengthOfVector;
  end
  else
    Result := AP1;
end;

function GetPointByCrossEntity(const AEntity1, AEntity2: TsgDXFEntity;
  const ABox: TFRect; var APt: TFPoint;
  const AConstants: TsgConstantsCustom): TObjectSnapState;
begin
  Result := [];
  if SnapCross = nil then
    SnapCross := TsgSnapCross.Create(AConstants);
  if SnapCross.IsCross(AEntity1, AEntity2, ABox) then
  begin
    APt := SnapCross.CrossPoint;
    Result := [osIntersection];
  end;
end;

function GetPointByEdgeEntity(const AEntity: TsgDXFEntity; const ABasePt: PFPoint;
  const APoint: TFPoint; const ABox: TFRect; const ActiveSnap: TObjectSnapState;
  var APt: TFPoint): TObjectSnapState;
var
  vLine: TsgDXFLIne absolute AEntity;
  vPoint: TsgDXFPoint absolute AEntity;
  vSolid: TsgDXFSolid absolute AEntity;
  vPoly: TsgCADBasePolyline absolute AEntity;
  vFlat: TsgFlatEntity absolute AEntity;
{$IFDEF SG_BTI}
  vInsert: TsgDXFInsertAccess absolute AEntity;
  vEnt: TsgDXFEntity;
  vPoints: TFPointList;
  vSnapFound: TObjectSnapState;
{$ENDIF}
  I, J, Cnt, vIndex: Integer;
  vIsCircular: Boolean;
  vMinH: Double;
  vPts: array [0..1] of TFPoint;
  vPtsMode: Integer;
  vEntBox: TFRect;
  vTmpSnap: TObjectSnapState;

  procedure FindLine(const P1, P2: TFPoint; const AMode: Integer = 0);
  var
    A, B, C, C1, H, S, P: Double;
  begin
    C := DistanceFPoint(P1, P2);
    H := 0;
    if C > fDoubleResolution then
    begin
      A := DistanceFPoint(P1, APoint);
      B := DistanceFPoint(P2, APoint);
      P := (A + B + C) * 0.5;
      S := P * (P - A) * (P - B) * (P - C);
      if S >= 0 then
      begin
        if S > 0 then
        begin
          H := 2 * Sqrt(S) / C;
          C1 := Sqr(A) + Sqr(H);
          if C1 > 0 then
            C1 := Sqrt(C1)
          else
            C1 := 0;
          if C1 <= C then
            APt := GetPointInVector(P1, P2, C1)
          else
            Exit;
        end
        else
          APt := APoint;
        if not IsPointInFRect(ABox, APt) then
          Exit;
      end;
    end
    else
    begin
      if IsPointInFRect(ABox, P1) then
        H := DistanceFPoint(P1, APoint)
      else
        Exit;
    end;
    if H < vMinH then
    begin
      vIsCircular := False;
      vMinH := H;
      Result := Result + [osNearest];
      vPts[0] := P1;
      vPts[1] := P2;
      vPtsMode := AMode;
    end;
  end;

  function FindCircle(const ACenter: TFPoint; const ARadius, AStart,
    AEnd: Double; var AFindPoint: TFPoint; const ADelta: PDouble): Boolean;
  var
    vRadius, vAngle: Double;
  begin
    Result := False;
    if ARadius = 0 then
      Exit;
    vRadius :=  DistanceFPoint(ACenter, APoint);
    if Abs(vRadius / ARadius  - 1) < 0.25 then
    begin
      vAngle := GetAngleByPoints(ACenter, APoint, False);
      if IsAngleInAngles(vAngle, AStart, AEnd) then
      begin
        AFindPoint := GetPointOnCircle(ACenter, ARadius, vAngle);
        Result := True;
        if ADelta <> nil then
          ADelta^ := Abs(ARadius - vRadius);
      end;
    end;
  end;


  procedure FindArc(const Arc: TsgArcR);
  var
    H: Double;
    P: TFPoint;
  begin
    if FindCircle(Arc.Center, Arc.Radius, Arc.AngleS, Arc.AngleE, P, @H) then
    begin
      if vMinH > H then
      begin
        vIsCircular := True;
        vMinH := H;
        Result := Result + [osNearest];
        vPts[0] := P;
      end;
    end;
  end;

  procedure FindNearestPointByCircular(const AEntity: TsgDXFEntity);
  var
    vCircle: TsgDXFCircle absolute AEntity;
    vAngleS, vAngleE: Double;
  begin
    vAngleS := 0;
    vAngleE := 360;
    if AEntity.EntType = ceArc then
    begin
      vAngleS := TsgDXFArc(AEntity).StartAngle;
      vAngleE := TsgDXFArc(AEntity).EndAngle;
    end;
    if FindCircle(TsgDXFCircle(AEntity).Point, TsgDXFCircle(AEntity).Radius,
       vAngleS, vAngleE, APt, nil) then
      Result := Result + [osNearest];
  end;

  procedure SetPoint(const AP1, AP2: TFPoint; const APMode: Integer);
  var
    vD1, vD: Double;
    vHasNormal: Boolean;
  begin
    vHasNormal := False;
    if (ABasePt <> nil)  and (osNormal in ActiveSnap) then
      vHasNormal := PointCrossedPerpendicularSegments(MakeLine(AP1, AP2), ABasePt^, @APt);
    if (ABasePt <> nil) and vHasNormal and
       not IsEqualFPoints(APt, ABasePt^, fDoubleResolution) and
       IsPointInFRect(ABox, APt) and
       IsPointInFRect(ABox, APoint) then
      Result := Result + [osNormal]
    else
    begin
      vD1 := DistanceFPoint(AP1, APoint);
      vD := DistanceFPoint(AP1, AP2);
      if (APMode = 0) and IsHasSnap(AEntity) and (osMiddle in ActiveSnap) and
         (Abs(vD1 - vD * 0.5) < 0.25 * vD) then
      begin
        APt := MiddleFPoint(AP1, AP2);
        if IsPointInFRect(ABox, APt) then
          Result := Result + [osMiddle]
        else
        begin
          Result := Result + [osNearest];
          APt := GetPointInVector(AP1, AP2, vD1);
        end;
      end
      else
      begin
        Result := Result + [osNearest];
        APt := GetPointInVector(AP1, AP2, vD1);
      end;
    end;
  end;

  procedure FindNearestPointByPolyline(const AEntity: TsgDXFEntity);
  var
    vPoly: TsgDXFLWPolyline absolute AEntity;
    I, J, K, vPointCount: Integer;
  begin
    J := 0;
    for I := 0 to vPoly.PointsCounts.Count - 2 do
    begin
      vPointCount := Integer(vPoly.PointsCounts[I]);
      if vPointCount = 1 then
        FindLine(vPoly.PolyPoints[J], vPoly.PolyPoints[J + 1])
      else
        for K := J to J + vPointCount - 2 do
          FindLine(vPoly.PolyPoints[K], vPoly.PolyPoints[K + 1], 1);
      Inc(J, vPointCount);
    end;
    if vPoly.Closed and (vPoly.PointsCounts.Count > 1) then
      FindLine(vPoly.PolyPoints.First, vPoly.PolyPoints.Last,
        IfThen(Integer(vPoly.PointsCounts.Last) = 1, 0, 1));
    vTmpSnap := Result;
    if osNearest in Result then
    begin
      SetPoint(vPts[0], vPts[1], vPtsMode);
      if (Result <> vTmpSnap) then
        Result := Result - [osNearest];
    end;
  end;

{$IFDEF SG_BTI}
  procedure FindNearestPointByPolyPolyline2D(const PPoly: Pointer);
  var
    APPoly: TsgCADPolyPolyline2D absolute PPoly;
    J, Cnt: Integer;
    L: TsgLine;
  begin
    Cnt := APPoly.CurvesCount - 1;
    if (Cnt > 0) and (Cnt < cnstMaxCountPointsInPolyLineEdge) then
    begin
      for J := 0 to Cnt do
      begin
        if APPoly.CurveVisible[J] then
        begin
          if APPoly.IsCurveArc(J) then
            FindArc(APPoly.Arcs[J])
          else
          begin
            L := APPoly.Lines[J];
            FindLine(L.Point1, L.Point2);
          end;
        end;
      end;
      if osNearest in Result then
      begin
        if vIsCircular then
          APt := vPts[0]
        else
          SetPoint(vPts[0], vPts[1], vPtsMode);
      end
    end;
  end;

  procedure FindNearestPointByArc(const AArc: Pointer);
  var
    Poly: TsgCADBasePolyline absolute AArc;
    L: Integer;
  begin
    if (Poly.PolyPoints.Count > 0) and (Poly.PolyPoints.Count < cnstMaxCountPointsInPolyLineEdge) then
    begin
      Cnt := Poly.PolyPoints.Count - 2;
      for L := 0 to Cnt do
        FindLine(Poly.PolyPoints.List[L], Poly.PolyPoints.List[L + 1]);
      if Poly.Closed then
        FindLine(Poly.PolyPoints.First, Poly.PolyPoints.Last);
      vTmpSnap := Result;
      if osNearest in Result then
      begin
        SetPoint(vPts[0], vPts[1], vPtsMode);
        if (Result <> vTmpSnap) then
          Result := Result - [osNearest];
      end;
    end;
  end;

  procedure FindNearestPointByLine(const ALine: Pointer);
  begin
    SetPoint(TsgDXFLine(ALine).Point, TsgDXFLine(ALine).Point1, vPtsMode);
  end;

  procedure FindArcAndSetResult(const AArc: TsgArcR; var AIsFound: Boolean);
  begin
    if AIsFound then
      Exit;
    FindArc(AArc);
    if osNearest in Result then
    begin
      APt := vPts[0];
      AIsFound := True;
    end;
  end;

  procedure FindLineAndSetResult(const APt1,APt2: TFPoint; var AIsFound: Boolean);
  begin
    if AIsFound then
      Exit;
    FindLine(APt1, APt2, vPtsMode);
    if osNearest in Result then
    begin
      SetPoint(vPts[0], vPts[1], vPtsMode);
      AIsFound := True;
    end;
  end;

  procedure FindNearestPointByPtsOrArc(const APts: TFPointList;
    const AArc: TsgArcR; const AIsArc: Boolean);
  var
    vOldResult: TObjectSnapState;
    vIsFound: Boolean;
  begin
    if APts.Count > 8 then
    begin
      vOldResult := Result;
      try
        vIsFound := False;
        Result := Result - [osNearest];
        vMinH := Min(Min(DistanceFPoint(APts[1], APts[4]),
          DistanceFPoint(APts[2], APts[3])) * 0.83, vMinH);
        vMinH := Min(DistanceFPoint(ABox.TopLeft, ABox.BottomRight), vMinH);
        if AIsArc then
          FindArcAndSetResult(AArc, vIsFound)
        else
          FindLineAndSetResult(APts[7], APts[8], vIsFound);
        if vIsFound then
          vOldResult := vOldResult + Result;
      finally
        Result := vOldResult;
      end;
    end;
  end;
{$ENDIF}

begin
  Result := [];
  vPtsMode := 0;
  vMinH := MaxDouble;
  vIsCircular := False;
  vEntBox := AEntity.Box;
  if (vEntBox.Z1 = 0) and (vEntBox.Z2 = 0) then
  begin
    if AEntity is TsgDXFPoint then
    begin
      APt := vPoint.Point;
      Result := Result + [osEndPt];
    end
    else
    begin
      if AEntity is TsgDXFSolid then
      begin
        FindLine(vSolid.Point, vSolid.Point1);
        FindLine(vSolid.Point1, vSolid.Point3);
        FindLine(vSolid.Point2, vSolid.Point3);
        FindLine(vSolid.Point2, vSolid.Point);
        if osNearest in Result then
          SetPoint(vPts[0], vPts[1], vPtsMode);
      end
      else
      begin
        if AEntity is TsgDXFLine then
        begin
          SetPoint(vLine.Point, vLine.Point1, vPtsMode);
        end
        else
        begin
          if AEntity is TsgCADBasePolyline then
          begin
            if IsCircular(AEntity) then
              FindNearestPointByCircular(AEntity)
            else
            begin
              if AEntity is TsgDXFLWPolyline then
                FindNearestPointByPolyline(AEntity)
              else
              begin
                if (vPoly.PolyPoints.Count > 0) and
                  (vPoly.PolyPoints.Count < cnstMaxCountPointsInPolyLineEdge) then
                begin
                  Cnt := vPoly.PolyPoints.Count - 2;
                  for I := 0 to Cnt do
                    FindLine(vPoly.PolyPoints[I], vPoly.PolyPoints[I + 1]);
                  if vPoly.Closed then
                    FindLine(vPoly.PolyPoints.First, vPoly.PolyPoints.Last);
                  vTmpSnap := Result;
                  if osNearest in Result then
                  begin
                    SetPoint(vPts[0], vPts[1], vPtsMode);
                    if (Result <> vTmpSnap) then
                      Result := Result - [osNearest];
                  end;
                end;
              end
            end;
          end
          else
          begin
            if AEntity is TsgFlatPoly then
            begin
              Cnt := 0;
              for I := 0 to vFlat.Counts.Count - 1 do
                Inc(Cnt, Integer(vFlat.Counts[I]));
              if Cnt < cnstMaxCountPointsInPolyLineEdge then
              begin
                vIndex := 0;
                for I := 0 to vFlat.Counts.Count - 1 do
                begin
                  Cnt := Integer(vFlat.Counts[I]);
                  for J := 0 to Cnt - 2 do
                    FindLine(vFlat.XY[vIndex + J], vFlat.XY[vIndex + J + 1]);
                  Inc(vIndex, Cnt);
                end;
                vTmpSnap := Result;
                if osNearest in Result then
                begin
                  SetPoint(vPts[0], vPts[1], vPtsMode);
                  if (Result <> vTmpSnap) then
                    Result := Result - [osNearest];
                end;
              end;
            end
            else
            begin
{$IFDEF SG_BTI}
              if IsBTI(AEntity) then
              begin
                for I := vInsert.Block.Count - 1 downto 0 do
                begin
                  vEnt := vInsert.Block[I];
                  case vEnt.EntType of
                    ceArc:            FindNearestPointByArc(vEnt);
                    ceLine:           FindNearestPointByLine(vEnt);
                    cePolyPolyline2D: FindNearestPointByPolyPolyline2D(vEnt);
                    ceLWPolyline:     FindNearestPointByPolyline(vEnt);
                  end;
                end;
                vSnapFound := Result * ActiveSnap;
                if (vSnapFound = []) or ((vSnapFound = [osNearest]) and
                  IsPointInFRect(vInsert.Box,
                  MiddleFPoint(ABox.TopLeft, ABox.BottomRight))) then
                begin
                  vPoints := TFPointList.Create;
                  try
                    vInsert.GetPtsEx(vPoints);
                    FindNearestPointByPtsOrArc(vPoints, vInsert.GetArcOfCenter, vInsert.IsRealCircular);
                  finally
                    vPoints.Free;
                  end;
                end;
              end
              else
              begin
                if AEntity is TsgCADPolyPolyline2D then
                begin
                  if TsgCADPolyPolyline2D(AEntity).CurvesCount < cnstMaxCountPointsInPolyLineEdge then
                    FindNearestPointByPolyPolyline2D(TsgCADPolyPolyline2D(AEntity));
                end;
              end;
{$ENDIF}
            end;
          end
        end;
      end
    end;
  end;
end;

function GetPointByTargetEntity(const AEntity: TsgDXFEntity;
  const ABasePt: PFPoint;  const APoint: TFPoint; const ABox: TFRect;
  const ASnapDistance: Double; var APt: TFPoint): TObjectSnapState;
var
  vArc: TsgDXFArc absolute AEntity;
  vCircle: TsgDXFCircle absolute AEntity;
  vEllipse:  TsgDXFEllipse absolute AEntity;
  vPoly: TsgDXFLWPolyline absolute AEntity;
  vVertex1, vVertex2: TsgDXFVertex;
  vTangentPoint1, vTangentPoint2, vPoint1, vPoint2: TFPoint;
  vDistance, vDistance1, vDistance2: Double;
  I, Cnt, vFind, vFindBulge: Integer;
  vArcR: TsgArcR;
begin
  Result := [];
  if (ABasePt = nil) or IsEntityExtruded(AEntity) or (APoint.Z <> 0) then
    Exit;
  case AEntity.EntType of
    ceArc:
      vFind := IsTangentOfArc(vArc.Point, vArc.Radius, vArc.StartAngle,
        vArc.EndAngle, ABasePt^, vTangentPoint1, vTangentPoint2);
    ceCircle:
      vFind := IsTangentOfCircle(vCircle.Point, vCircle.Radius,
        ABasePt^, vTangentPoint1, vTangentPoint2);
    ceEllipse:
      begin
        vFind := IsTangentOfEllipse(vEllipse.Point, vEllipse.RadPt,
          vEllipse.Ratio, vEllipse.StartAngle, vEllipse.EndAngle,
          ABasePt^, vTangentPoint1, vTangentPoint2);
      end;
    ceLWPolyline:
      begin
        vFind := 0;
        vDistance := MaxDouble;
        Cnt := vPoly.Count - 1;
        for I := 0 to Cnt do
        begin
          vVertex1 := vPoly.Vertexes[I];
          if I >= Cnt then
             vVertex2 := vPoly.Vertexes[0]
          else
            vVertex2 := vPoly.Vertexes[I + 1];
          if (vVertex1.Bulge <> 0) and
             (not IsEqualFPoints(vVertex1.Point, vVertex2.Point)) then
          begin
            ApplyBulge(vVertex1.Point, vVertex2.Point, vVertex1.Bulge, nil, -1,
               @CreatePFPointProc, vArcR);
            vFindBulge := IsTangentOfArc(vArcR.Center, vArcR.Radius, vArcR.AngleS,
               vArcR.AngleE, ABasePt^, vPoint1, vPoint2);
            if vFindBulge > 0 then
            begin
              vDistance1 := DistanceFPointSqr(vPoint1, APoint);
              if vFindBulge > 1 then
              begin
                vDistance2 := DistanceFPointSqr(vPoint2, APoint);
                if vDistance1 > vDistance2 then
                begin
                  vPoint1 := vPoint2;
                  vDistance1 := vDistance2;
                end;
              end;
              if vDistance > vDistance1 then
              begin
                vFind := 1;
                vTangentPoint1 := vPoint1;
                vDistance := vDistance1;
              end;
            end;
          end;
        end;
      end;
  else
    vFind := 0;
    vTangentPoint1 := cnstFPointZero;
    vTangentPoint2 := cnstFPointZero;
  end;
  if vFind > 0 then
  begin
     vDistance1 := DistanceFPointSqr(vTangentPoint1, APoint);
    if vFind > 1 then
    begin
      vDistance2 := DistanceFPointSqr(vTangentPoint2, APoint);
      if vDistance1 > vDistance2 then
      begin
        vDistance1 := vDistance2;
        vTangentPoint1 := vTangentPoint2;
      end;
    end;
    if vDistance1 <= Sqr(ASnapDistance) then
    begin
      APt := vTangentPoint1;
      Result := [osTangent];
    end;
  end;
end;

function MakeHelix(const ARadius: Integer; var APoints: TShortIntPointList): Integer;
var
  J: Integer;
  S, C, I, vDelta, vLocalRadius, vLen: Extended;
  P: TShortIntPoint;

  function FindPoint(const APoint: TShortIntPoint; const APoints: TShortIntPointList;
    ACount: Integer; var Index: Integer): Boolean;
  begin
    Index := ACount - 1;
    Result := False;
    while (Index >= Low(APoints)) and not Result do
      if (APoints[Index].x = APoint.x) and (APoints[Index].y = APoint.y) then
        Inc(Result)
      else
        Dec(Index);
  end;

begin
  vDelta := 1/Sqrt(2);
  vLocalRadius := vDelta;
  SetLength(APoints, Round(Pi * ARadius * (ARadius + 1)));
  APoints[0].x := 0;
  APoints[0].y := 0;
  Result := 1;
  while vLocalRadius <= ARadius do
  begin
    vLen := 2 * Pi * vLocalRadius;
    I := 0;
    while I <= vLen do
    begin
      SinCos(2*Pi*I/vLen, S, C);
      P.X := Round(vLocalRadius * S);
      P.Y := Round(vLocalRadius * C);
      if not FindPoint(P, APoints, Result, J) then
      begin
        APoints[Result] := P;
        Inc(Result);
      end;
      I := I + vDelta;
    end;
    vLocalRadius := vLocalRadius + vDelta;
  end;
  SetLength(APoints, Result);
end;

{ TsgObjectSnap }

constructor TsgObjectSnap.Create;
begin
  inherited Create;
  FSnapDistance := cnstSnapDistance;
  FHelix := Helix;
end;

function TsgObjectSnap.GetSnapData(const APoint: TPoint; var ASnap: TSnapData): Boolean;
var
  SD: PSnapData;
begin
  Result := False;
  SD := FindSnap(APoint);
  if SD <> nil then
  begin
    ASnap := SD^;
    Result := True;
  end
  else
    ASnap := SnapDataEmpty;
end;

function TsgObjectSnap.GetSnapPointUCS(AVisiblePoint: TPoint;
  var AObjectSnapState: TObjectSnapState; var AEntity: TsgDXFEntity; var CoordsInUCS: TFPoint): TSnapData;
var
  SD: PSnapData;
begin
  FillChar(Result, SizeOf(Result), 0);
  SD := FindSnap(AVisiblePoint);
  if Assigned(SD) then
  begin
    Result := SD^;
    AObjectSnapState := SD^.SnapMode;
    AEntity := TsgDXFEntity(SD^.Entity);
    CoordsInUCS := SD^.CADPointInUCS;
  end
  else
  begin
    AObjectSnapState := [];
    AEntity := nil;
    Result.CADPointInUCS := MakeFPoint(MaxTsgFloat, MaxTsgFloat, MaxTsgFloat);
    CoordsInUCS := Result.CADPointInUCS;
  end;
end;

function TsgObjectSnap.GetSnapPoint(AVisiblePoint: TPoint;
  var AObjectSnapState: TObjectSnapState; var AEntity: TsgDXFEntity): TSnapData;
var
  Temp: TFPoint;
begin
  Result := GetSnapPointUCS(AVisiblePoint, AObjectSnapState, AEntity, Temp);
end;

procedure TsgObjectSnap.SetsgDXFImage(ACADImage: TsgCADImage);
begin
  FCADImage := ACADImage;
end;

function TsgObjectSnap.FindSnap(const APoint: TPoint): PSnapData;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Length(FHelix)) and ((Result = nil) or (Result^.Entity = nil)) do
  begin
    Result := FCADImage.GetSnapData(Point(APoint.X + FHelix[I].x, APoint.Y + FHelix[I].y));
    Inc(I);
  end;
  if Assigned(Result) and not Assigned(Result^.Entity) then
    Result := nil;
end;

procedure TsgObjectSnap.SetSnapDistance(const AVal: Integer);
var
  vHelixRadius: Integer;
begin
  if AVal > 0 then
    FSnapDistance := AVal
  else
    FSnapDistance := cnstSnapDistance;
  vHelixRadius := FSnapDistance;
  if vHelixRadius > 127 then
    vHelixRadius := 127;
  if vHelixRadius = 0 then
    vHelixRadius := 1;
  MakeHelix(vHelixRadius, FHelix);
end;

class function TsgObjectSnap.SnapDataEmpty: TSnapData;
begin
  Result := sgConsts.cnstSnapDataEmpty;
end;

{ TSnapMarker }

procedure TSnapMarker.Draw(Canvas: TCanvas);
begin
  if (FPoint.X <> MaxInt) and Assigned(Canvas) then
  begin
    FVisible := not FVisible;
    FPoint := FSnapControl.GetPoint(FRealPoint);
    if FSnapControl.DoSnapDraw(Canvas, FPoint, FDestinationSnap) then
      DoDraw(Canvas, FPoint, FDestinationSnap);
  end;
end;

procedure TSnapMarker.ClearPoint;
begin
  FPoint.X := MaxInt;
  FRealPoint := BadPoint;
  FBaseMatrixSnapType := osDisabled;
end;

constructor TSnapMarker.Create(ASnapControl: TSnapControl);
begin
  inherited Create;
  FIsTraced := False;
  FSnapControl := ASnapControl;
{$IFNDEF SG_FIREMONKEY}
  FPen := TPen.Create;
  FPen.Width := 1;
  FPen.Style := psSolid;
  FPen.Color := clWhite;
  FPen.Mode := pmXor;
  FPenStore := TPen.Create;
{$ELSE}
  FPenColor := TAlphaColorRec.Black;
{$ENDIF}
  FDrawAreaSize := cnstSnapMarkerSize;
  ClearPoint;
end;

destructor TSnapMarker.Destroy;
begin
{$IFNDEF SG_FIREMONKEY}
  FreeAndNil(FPen);
  FreeAndNil(FPenStore);
{$ENDIF}
  inherited Destroy;
end;

procedure TSnapMarker.DoDraw(Canvas: TCanvas; const APoint: TPoint; ASnap: TObjectSnapMode);
var
  P: TPoint;
{$IFDEF SG_FIREMONKEY}
var
  vThickness: Single;
  vColor: TAlphaColor;
{$ENDIF}

  {$IFDEF SGVER_6_0_0}
  function GetCADCoords(const APt: TPoint): TFPoint;
  begin
    Result := FSnapControl.ControlProps.ClientToWorld(APt.X, APt.Y)
  end;
  {$ENDIF}

{$IFDEF SG_FIREMONKEY}
  procedure SaveStrokeParams;
  begin
    vThickness := Canvas.Stroke.Thickness;
    vColor := Canvas.Stroke.Color;
  end;

  procedure RestoreStrokeParams;
  begin
    Canvas.Stroke.Thickness := vThickness;
    Canvas.Stroke.Color := vColor;
  end;
{$ENDIF}

  procedure DrawNormalSnap;
  var
    vPts: array [0..2] of TPoint;
  begin
   // if FSnapControl.BasePoint = nil then
   //   Exit;
    vPts[0].X := P.X - FDrawAreaSize;
    vPts[0].Y := P.Y - FDrawAreaSize;
    vPts[1].X := P.X - FDrawAreaSize;
    vPts[1].Y := P.Y + FDrawAreaSize;
    vPts[2].X := P.X + FDrawAreaSize;
    vPts[2].Y := P.Y + FDrawAreaSize;
    Canvas.Polyline(vPts);

    vPts[0].X := P.X - FDrawAreaSize + 1;
    vPts[0].Y := P.Y;
    vPts[1].X := P.X;
    vPts[1].Y := P.Y;
    vPts[2].X := P.X;
    vPts[2].Y := P.Y + FDrawAreaSize - 1;
    Canvas.Polyline(vPts);
  end;

  procedure DrawCross;
  begin
    {$IFDEF SG_FIREMONKEY}
     Canvas.Polyline([Point(P.X - FDrawAreaSize, P.Y - FDrawAreaSize),
       Point(P.X + FDrawAreaSize, P.Y + FDrawAreaSize)]);
     Canvas.Polyline([Point(P.X - FDrawAreaSize, P.Y + FDrawAreaSize),
       Point(P.X + FDrawAreaSize, P.Y - FDrawAreaSize)]);
    {$ELSE}
    Canvas.MoveTo(P.X - FDrawAreaSize, P.Y - FDrawAreaSize);
    Canvas.LineTo(P.X + FDrawAreaSize, P.Y + FDrawAreaSize);
    Canvas.MoveTo(P.X - FDrawAreaSize, P.Y + FDrawAreaSize);
    Canvas.LineTo(P.X + FDrawAreaSize, P.Y - FDrawAreaSize);
    {$ENDIF}
  end;

  procedure DrawBox;
  var
    vPts: array [0..4] of TPoint;
  begin
    vPts[0] := Point(P.X - FDrawAreaSize, P.Y - FDrawAreaSize);
    vPts[1] := Point(P.X + FDrawAreaSize, P.Y - FDrawAreaSize);
    vPts[2] := Point(P.X + FDrawAreaSize, P.Y + FDrawAreaSize);
    vPts[3] := Point(P.X - FDrawAreaSize, P.Y + FDrawAreaSize);
    vPts[4] := Point(P.X - FDrawAreaSize, P.Y - FDrawAreaSize);
    Canvas.Polyline(vPts);
  end;

  procedure DrawCircle;
  {$IFDEF SG_FIREMONKEY}
  begin
    Canvas.Ellipse(P.X - FDrawAreaSize, P.Y - FDrawAreaSize,
                    P.X + FDrawAreaSize, P.Y + FDrawAreaSize);
  {$ELSE}
  var
    vBS: TBrushStyle;
  begin
    vBS := Canvas.Brush.Style;
    try
      Canvas.Brush. Style := bsClear;
      Canvas.Ellipse(P.X - FDrawAreaSize, P.Y - FDrawAreaSize,
                              P.X + FDrawAreaSize, P.Y + FDrawAreaSize);
    finally
      Canvas.Brush.Style := vBS;
    end;
  {$ENDIF}
  end;

  procedure DrawCrossSnap;
  var
    vPts: array[0..1] of TPoint;
  begin
    vPts[0].X := P.X - FDrawAreaSize;
    vPts[0].Y := P.Y - FDrawAreaSize;
    vPts[1].X := P.X + FDrawAreaSize;
    vPts[1].Y := P.Y + FDrawAreaSize;
    Canvas.Pen.Width := 3;
    Canvas.Polyline(vPts);
    vPts[0].X := P.X - FDrawAreaSize;
    vPts[0].Y := P.Y + FDrawAreaSize;
    vPts[1].X := P.X + FDrawAreaSize;
    vPts[1].Y := P.Y - FDrawAreaSize;
    Canvas.Polyline(vPts);
    Canvas.Pen.Width := 0;
  end;

  procedure DrawNearestSnap;
  var
    vPts: array [0..4] of TPoint;
  begin
    vPts[0].X := P.X - FDrawAreaSize;
    vPts[0].Y := P.Y - FDrawAreaSize;
    vPts[1].X := P.X + FDrawAreaSize;
    vPts[1].Y := vPts[0].Y;
    vPts[2].X := vPts[0].X;
    vPts[2].Y := P.Y + FDrawAreaSize;
    vPts[3].X := vPts[1].X;
    vPts[3].Y := vPts[2].Y;
    vPts[4] := vPts[0];
    Canvas.Polyline(vPts);
  end;

  procedure DrawInsertSnap;
  var
    vPts: array [0..8] of TPoint;
  begin
    vPts[0].X := P.X - FDrawAreaSize;
    vPts[0].Y := P.Y - FDrawAreaSize;
    vPts[1].X := vPts[0].X;
    vPts[1].Y := P.Y + Round(FDrawAreaSize * 0.2);
    vPts[2].X := P.X;
    vPts[2].Y := vPts[1].Y;
    vPts[3].X := P.X;
    vPts[3].Y := P.Y + FDrawAreaSize;
    vPts[4].X := P.X + FDrawAreaSize;
    vPts[4].Y := vPts[3].Y;
    vPts[5].X := vPts[4].X;
    vPts[5].Y := P.Y - Round(FDrawAreaSize * 0.2);
    vPts[6].X := P.X;
    vPts[6].Y := vPts[5].Y;
    vPts[7].X := P.X;
    vPts[7].Y := vPts[0].Y;
    vPts[8] := vPts[0];
    Canvas.Polyline(vPts);
  end;

  procedure DrawMiddleSnap;
  var
    vPts: array [0..3] of TPoint;
    A, H, dH: Integer;
  begin
    H := FDrawAreaSize shl 1;
    A := Round(H / cnstSqrt3);
    dH := Round(H / 3);
    vPts[0].X := P.X;
    vPts[0].Y := P.Y - (H - dH);
    vPts[1].X := P.X + A;
    vPts[1].Y := P.Y + dH;
    vPts[2].X := P.X - A;
    vPts[2].Y := vPts[1].Y;
    vPts[3] := vPts[0];
    Canvas.Polyline(vPts);
  end;

  procedure DrawTangent;
  var
    vPts: array [0..1] of TPoint;
  begin
    vPts[0] := Point(P.X - FDrawAreaSize, P.Y - FDrawAreaSize - 1);
    vPts[1] := Point(P.X + FDrawAreaSize, P.Y - FDrawAreaSize - 1);
    Canvas.Polyline(vPts);
    DrawCircle;
  end;

  procedure DrawQuadrant;
  var
    vPts: array [0..4] of TPoint;
  begin
    vPts[0] := Point(P.X + FDrawAreaSize, P.Y);
    vPts[1] := Point(P.X, P.Y - FDrawAreaSize);
    vPts[2] := Point(P.X - FDrawAreaSize, P.Y);
    vPts[3] := Point(P.X, P.Y + FDrawAreaSize);
    vPts[4] := Point(P.X + FDrawAreaSize, P.Y);
    Canvas.Polyline(vPts);
  end;

begin
{$IFNDEF SG_FIREMONKEY}
  FPenStore.Assign(Canvas.Pen);
{$ELSE}
  SaveStrokeParams;
{$ENDIF}
  P := APoint;
  try
{$IFNDEF SG_FIREMONKEY}
    Canvas.Pen := FPen;
{$ELSE}
    if Canvas.Scale > 1 then
      Canvas.Clear(0);
    Canvas.Pen.Color := FPenColor;
    Canvas.Pen.Thickness := 1;
{$ENDIF}
    case ASnap of
      osInsert:       DrawInsertSnap;
      osCenter:       DrawCircle;
      osIntersection: DrawCrossSnap;
      osMiddle:       DrawMiddleSnap;
      osEndPt:        DrawBox;
      osNearest:      DrawNearestSnap;
      osNormal:       DrawNormalSnap;
      osQuadrant:     DrawQuadrant;
      osTangent:      DrawTangent;
    else
      FVisible := False;
    end;
  finally
{$IFNDEF SG_FIREMONKEY}
    Canvas.Pen := FPenStore;
{$ELSE}
    RestoreStrokeParams;
{$ENDIF}
  end;
end;

procedure TSnapMarker.Repaint;
begin
  FVisible := False;
  Draw(FSnapControl.Canvas);
end;

procedure TSnapMarker.SetBaseMatrixSnapType(const Value: TObjectSnapMode);
begin
  FBaseMatrixSnapType := Value;
end;

procedure TSnapMarker.SetIsTraced(const Value: Boolean);
begin
  FIsTraced := Value;
end;

procedure TSnapMarker.Snap(APoint: TFPoint; ASnap: TObjectSnapMode);
var
  vCanvas: TCanvasAccess;
begin
  vCanvas := TCanvasAccess(FSnapControl.Canvas);
  Draw(vCanvas); // Hide
  FPoint := FSnapControl.GetPoint(APoint);
  FRealPoint := APoint;
  FDestinationSnap := ASnap;
  Draw(vCanvas); // Show
  if FDestinationSnap = osDisabled then
    ClearPoint;
end;

{ TSnapControl }

procedure TSnapControl.SetSnapData(const Value: TSnapData);
begin
  FSnapData := Value;
end;

procedure TSnapControl.SetSnapMarker(const Value: TSnapMarker);
begin
  FSnapMarker := Value;
end;

procedure TSnapControl.SetSnapMask(Value: TObjectSnapState);
var
  M1, M2: TObjectSnapState;
  vRegen: Boolean;
  vParams: PsgCADIterate;
{$IFDEF SG_FIREMONKEY}
  vMode: TsgSelectionMode;
{$ENDIF}
begin
  M1 := FSnapMask - cnstMaskNotUseIterate;
  vRegen :=  Value * cnstExtensionSnapMask <>  FSnapMask * cnstExtensionSnapMask;
  FSnapMask := Value;
  if Assigned(FImg) then
  begin
    M2 := Value - cnstMaskNotUseIterate;
    if M1 = M2 then
      TsgCADImageAccess(FImg).ObjectSnapMask := FSnapMask
    else
    begin
      vParams := TsgCADConverter(TsgCADImageAccess(FImg).Converter).ExchangeParams(@TsgCADImageAccess(FImg).FDraw);
      try
{$IFDEF SG_FIREMONKEY}
        vMode := TsgCADImageAccess(FImg).GetMatrixMode;
        TsgCADImageAccess(FImg).Context.Canvas := FControlProps.GetCanvas;
        try
          TsgCADImageAccess(FImg).SetMatrixMode(smMatrixOnly);
{$ENDIF}
          FImg.SetObjectSnapMask(FSnapMask{$IFNDEF SG_FM_WINDOWS}, False{$ENDIF});
{$IFDEF SG_FIREMONKEY}
        finally
          TsgCADImageAccess(FImg).SetMatrixMode(vMode);
          TsgCADImageAccess(FImg).Context.Canvas := nil;
        end;
{$ENDIF}
      finally
        TsgCADConverter(TsgCADImageAccess(FImg).Converter).ExchangeParams(vParams);
      end;
    end;
  end;
  if vRegen then
    FTracers.Regen;
end;

procedure TSnapControl.SetTracers(const Value: TsgTracers);
begin
  FTracers := Value;
end;

function TSnapControl.DoSnapDraw(Canvas: TCanvas; const APoint: TPoint;
  ASnap: TObjectSnapMode): Boolean;
begin
  Result := True;
  if Assigned(FOnSnapDraw) then
    FOnSnapDraw(FSnapMarker, Canvas, APoint, ASnap, Result);
end;

procedure TSnapControl.Update;
begin
  FUpdating := True;
  try
    SetControlProps(FControlProps);
  finally
    FUpdating := False;
  end;
end;

procedure TSnapControl.SetEvents;
{$IFDEF SGVER_5_2_5}
var
  vEv: TNotifyEvent;
{$ENDIF}
begin
  {$IFDEF SGVER_6_0_0}
  FControlProps.AddClient(Self);
  {$ENDIF}
  {$IFDEF SGVER_5_2_5}
  if FControl is TsgImage then
  begin
    vEv := TsgImage(FControl).OnPaint;
    if not Assigned(FImageOnPaint) and (@FImageOnPaint <> @vEv) then
    begin
      FImageOnPaint := vEv;
      TsgImage(FControl).OnPaint := Repaint;
    end;
    vEv := TsgImage(FControl).OnScaling;
  end;
  {$ENDIF}
end;

procedure TSnapControl.SetForBiddenSnapPoint(const Value: TFPoint);
begin
  FForBiddenSnapPoint := Value;
end;

procedure TSnapControl.SetIsNewPointAdded(const Value: Boolean);
begin
  FIsNewPointAdded := Value;
end;

procedure TSnapControl.SetBasePoint(const Value: PFPoint);
begin
  FBasePoint := Value;
end;

procedure TSnapControl.SetConstants(const Value: TsgConstantsCustom);
begin
  FConstants := Value;
end;

{$IFDEF SG_FIREMONKEY}
procedure TSnapControl.SetColor(AIsBlackBaground: Boolean);
begin
  if AIsBlackBaground then
    FSnapMarker.FPenColor := TAlphaColorF.Create(1, 1, 1, 0.67).ToAlphaColor
  else
    FSnapMarker.FPenColor := TAlphaColorF.Create(0, 0, 0, 0.67).ToAlphaColor;
end;
{$ENDIF}

procedure TSnapControl.SetControlProps(AValue: TsgCustomControlToolProps);
var
  Gr: TGraphic;
  CurrGr: TGraphic;
begin
  if Assigned(FControlProps) then
    CurrGr := FControlProps.GetGraphic
  else
    CurrGr := nil;
  FControlProps := AValue;
  {$IFDEF SGVER_6_0_0}
  Gr := AValue.GetGraphic;
  {$ENDIF}
  {$IFDEF SGVER_5_2_5}
  if not (FControlProps is TsgImage) then
  begin
    if not FUpdating then
      EICError
  end
  else
    Gr := TsgImage(FControl).Picture.Graphic;
  {$ENDIF}
  if Gr is TsgCADImage then
  begin
    FObjectSnap.SetsgDXFImage(TsgCADImage(Gr));
    FImg := TsgCADImage(Gr);
    FTracers.CADImage := FImg;
{$IFDEF SG_FIREMONKEY}
    SetColor(FImg.BackgroundColor = clBlack);
{$ENDIF}
  end
  else
  begin
    FObjectSnap.SetsgDXFImage(nil);
    FImg := nil;
  end;
  if (Gr <> CurrGr) or (Gr = nil) then
  begin
    FTracers.TracersMatrix.ClearFull;
    FTracers.ClearMarkers;
  end;
  // Clear save entity
  Clear;
  SetEvents;
  if not (osDisabled in FSnapMask) then
    SetSnapMask(FSnapMask);
end;

function TSnapControl.GetCanvas: TCanvas;
begin
  {$IFDEF SGVER_6_0_0}
  Result := FControlProps.GetCanvas;
  {$ENDIF}
  {$IFDEF SGVER_5_2_5}
  if FControlProps is TsgImage then
    Result := TsgImage(FControl).Canvas;
  {$ENDIF}
end;

function TSnapControl.GetControlProps: TsgCustomControlToolProps;
begin
  Result := FControlProps;
end;

constructor TSnapControl.Create(const AConstants: TsgConstantsCustom);
begin
  //inherited Create;// ???? why not used
  FConstants := AConstants;
  FInsert := nil;
  FIsNewPointAdded := True;
  FTracers := TsgTracers.Create(Self);
  FIsTimeForTrace := False;
  FObjectSnap := TsgObjectSnap.Create;
  FSnappedEntity := nil;
  FSnappedEntityForTrace := nil;
  FSnappedEntityForTraceTwo := nil;
  FSnapMarker := TSnapMarker.Create(Self);
  FSnapMarker.FDestinationSnap := osDisabled;
  FBasePoint := nil;
end;

destructor TSnapControl.Destroy;
begin
  FTracers.CADImage := nil;
  FTracers.Free;
  ResetEvents;
  FObjectSnap.Free;
  FSnapMarker.Free;
  FConstants := nil;
  inherited Destroy;
end;

procedure TSnapControl.Repaint;
begin
  FSnapMarker.Repaint;
end;

procedure TSnapControl.ResetEvents;
begin
  {$IFDEF SGVER_6_0_0}
  FControlProps.RemoveClient(Self);
  {$ENDIF}
  {$IFDEF SGVER_5_2_5}
  if FImage is TsgImage then
  begin
    if Assigned(FImageOnPaint) then
      TsgImage(FImage).OnPaint := FImageOnPaint;
  end;
  {$ENDIF}
end;

function TSnapControl.GetMouseCoordUCS(X, Y: Integer; var PointInUCS: TFPoint): TSnapData;
var
  vObjectSnapMode: TObjectSnapMode;
  vPoint: TFPOint;
  vSnapState: TObjectSnapState;
  vEntitiesSnapData, vTracersSnapData: TSnapData;
  vSnappedEntity: TsgDXFEntity;
  vCount: Integer;
  vIsBaseMatrixUsed: Boolean;
begin
  vCount := FTracers.Count;
  vEntitiesSnapData := GetMouseCoordUCSInternal(X, Y, PointInUCS);
  if GetSelectionMatrix = nil then
  begin
    FSnappedEntityForTrace := nil;
    FSnappedEntityForTraceTwo := nil;
    FInsert := nil;
    FTracers.Clear;
    FTracers.FTracersMatrix.ClearFull;
  end;
  FSelectionMatrix := FTracers.TracersMatrix;
//  FTracers.TracersMatrix.Bitmap.SaveToFile('d:\1.bmp');
  vPoint := FSnapMarker.FRealPoint;
  vObjectSnapMode := FSnapMarker.FDestinationSnap;
  vSnapState := FSnapMask;
  FSnapMask := FSnapMask * [osDisabled, osIntersection, osNormal];
  FSnapMask := FSnapMask + [osNearest];
  vSnappedEntity := FSnappedEntity;
  try
    vTracersSnapData := GetMouseCoordUCSInternal(X, Y, PointInUCS);
  finally
    FSnappedEntity := vSnappedEntity;
    FSnapMarker.FDestinationSnap := vObjectSnapMode;
    FSnapMarker.FRealPoint :=  vPoint;
    FSelectionMatrix := nil;
    FSnapMask := vSnapState;
  end;
  vIsBaseMatrixUsed := (vEntitiesSnapData.SnapType <> osDisabled) or (vCount <> FTracers.Count) or
    ((vTracersSnapData.SnapType = osNearest) and not (vTracersSnapData.Entity is TsgDXFLineTrace));
  // if point is on Tracer then just use base matrix to make the drawing more accurate
  if not vIsBaseMatrixUsed and (FTracers.IndexOf(vTracersSnapData.CADPointInUCS) <> -1) then
  begin
    vTracersSnapData.EntityIntersected := nil;
    vTracersSnapData.SnapType := osDisabled;
  end;
  if  vIsBaseMatrixUsed then
    Result := vEntitiesSnapData
  else
  begin
    Result := vTracersSnapData;
    if Result.SnapType in [osNearest, osMiddle] then
      Result.SnapType := osDisabled; // do not show nearest snap for tracers as a default one
    FSnappedEntity := TsgDXFEntity(Result.Entity);
  end;
  FSnapMarker.Snap(Result.CADPointInUCS, Result.SnapType);
  if vIsBaseMatrixUsed then
    FSnapMarker.FBaseMatrixSnapType := FSnapMarker.FDestinationSnap;
  FSnapData := Result;
  PointInUCS := Result.CADPointInUCS;
  FSnappedEntity := TsgDXFEntity(Result.Entity);
end;

function TSnapControl.GetMouseCoordUCSInternal(X, Y: Integer; var PointInUCS: TFPoint): TSnapData;
var
  vPt, vPt1, vPt2: TFPoint;
  vFindedSnap, vExcludeSnap, vFindedSnapByMatrix: TObjectSnapState;
  vP, vP1: TPoint;
  vBox: TFRect;
  vMatrix: TsgSelectionMatrix;
  vSnappedEntity: TsgDXFEntity;
  vSearchPrecision, vSnapDistance: Integer;
  vCadSnapDistance: Double;
  vFitrstSnapMode: TObjectSnapMode;

  function GetUCSPoint(const APoint: TFPoint): TFPoint;
  begin
    Result := APoint;
    if FImg <> nil then
      TsgCADImageAccess(FImg).TransformToUCS(Result);
  end;

  function GetFPoint(const AX, AY: Integer): TFPoint;
  begin
    Result := cnstFPointZero;
    {$IFDEF SGVER_6_0_0}
    Result := FControlProps.ClientToWorld(AX, AY, @PointInUCS);
    {$ENDIF}
    {$IFDEF SGVER_5_2_5}
      if vImage is TsgImage then
        Result := GetRealPointUsingsgImagePoint(TsgImage(vImage), AX, AY, PointInUCS);
    {$ENDIF}
  end;

  function GetPoint(APoint: TFPoint): TPoint;
  begin
    Result := cnstPointZero;
    if FImg <> nil then
      Result := Self.GetPoint(APoint);
  end;

  procedure SetNonSnappedResults;
  begin
    vPt := GetFPoint(X, Y);
    PointInUCS := GetUCSPoint(vPt);
    vSnappedEntity := nil;
  end;


  procedure DoSnap;
  begin
   vFitrstSnapMode := osInsert;
    if vFindedSnap <> [] then
      repeat
        vFitrstSnapMode := Succ(vFitrstSnapMode);
      until vFitrstSnapMode in vFindedSnap
    else
      vFitrstSnapMode := osDisabled;
  end;

  procedure DoTracers;
  var
    vIsActive: Boolean;
    vPoint: TFPoint;
    vIndexOf: Integer;
  begin
    if FSnapMask * cnstExtensionSnapMask = [] then
      Exit;
    vPoint := FSnapMarker.FRealPoint; // point BEFORE current!!
    vIndexOf := FTracers.IndexOf(vPoint);
    vIsActive :=  bAutoCapturePoint or FTracers.IsShiftPressed;
    if FSnapMarker.FBaseMatrixSnapType in cnstFirstSnapMask then
      FTracers.IsShiftClicked := False;
    vIsActive := vIsActive and (not (FSnapMarker.FBaseMatrixSnapType in
     cnstFirstSnapMask)) and (vFitrstSnapMode in cnstFirstSnapMask);
    if vIsActive then
    begin
      vIsActive := not IsEqualFPoints(FForBiddenSnapPoint, FSnapMarker.FRealPoint);
      vIsActive := vIsActive and not IsEqualFPoints(FSnapMarker.FRealPoint, BadPoint);
      FForBiddenSnapPoint := BadPoint;
      FTracers.IsShiftClicked := False;
    end;
    if vIsActive then
      FIsTimeForTrace := (sgGetTickCount - FTime) > 300;
    if (FSnapMarker.FBaseMatrixSnapType in cnstFirstSnapMask) then
      FTime := sgGetTickCount;
    if vIsActive and (vIndexOf <> -1) then
    begin //delete tracer
      FTracers.Remove(vPoint);
      Exit;
    end;
    if FIsNewPointAdded then
    begin
      FIsNewPointAdded  := False;
      Exit;
    end;
    if vIsActive and FIsTimeForTrace then // create tracer
    begin
      if (vIndexOf = -1) and (FSnappedEntityForTrace <> nil) and
        not (FSnappedEntityForTrace is TsgDXFLineTrace) then
        FTracers.Add(vPoint, Self, FSnappedEntityForTrace, FSnappedEntityForTraceTwo);
    end;
  end;

  procedure CrossSnap;
{$IFDEF SG_BTI}
  const
    cnstMaxSnapPrecision = 40;
{$ENDIF}
  var
    I, J, Cnt: Integer;
    vList: TsgList;
    vEnt1, vEnt2: Pointer;
{$IFDEF SG_BTI}
    vPts: TsgPoints4;
    vNewPrecision: Integer;
{$ENDIF}
  begin
    vList := TsgList.Create;
    try
      vList.Sorted := True;
      vList.Duplicates := dupIgnore;
      vMatrix.GetElements(X, Y, vList, vList.Duplicates = dupIgnore);
      if vList.Count > 0 then
      begin
{$IFDEF SG_BTI}
        vEnt1 := vList[0];
        if IsBTI(vEnt1) and (vList.Count = 1) then
        begin
          TsgDXFInsertAccess(vEnt1).GetPts(vPts);
          vNewPrecision := MaxI(Trunc(Max(
            DistancePoint(GetPoint(vPts[0]), GetPoint(vPts[3])),
            DistancePoint(GetPoint(vPts[1]), GetPoint(vPts[2])))),
            vMatrix.Precision);
          if vNewPrecision > cnstMaxSnapPrecision then
            vNewPrecision := cnstMaxSnapPrecision;
          vMatrix.Precision := vNewPrecision;
          vMatrix.GetElements(X, Y, vList, vList.Duplicates = dupIgnore);
        end;
{$ENDIF}
        I := 0;
        Cnt := vList.Count;
        while (I < Cnt)  and not Assigned(vSnappedEntity) do
        begin
          vEnt1 := vList[I];
          if IsEntity(vEnt1) then
          begin
            J := I;
            while (J < Cnt)  and not Assigned(vSnappedEntity)  do
            begin
                vEnt2 := vList[J];
                if IsEntity(vEnt2) then
                begin
                  vFindedSnap := GetPointByCrossEntity(vEnt1, vEnt2, vBox, vPt, FConstants);
                  if vFindedSnap <> [] then
                  begin
                    PointInUCS := GetUCSPoint(vPt);
                    vSnappedEntity := vEnt1;
                    if Result.EntityIntersected = nil then
                      Result.EntityIntersected := vEnt2;
                  end;
                end
                else
                  vList[J] := nil;
              Inc(J);
            end;
          end;
          Inc(I);
        end;
      end;
    finally
      vList.Free;
    end;
  end;

  procedure EdgeSnap(ASnapActive: TObjectSnapState);
  var
    vSearchMode: TsgSearchMode;
    vEnt: Pointer;
  begin
    vSearchMode := vMatrix.SearchMode;
    try
      vMatrix.SearchMode := smSnail;
      vEnt := vMatrix.Matrix[X, Y];
      if IsEntity(vEnt) then
      begin
        vP := vMatrix.PointByEntity;
        vPt1 := GetFPoint(vP.X, vP.Y);
        if cnstTangentSnapMask * ASnapActive <> [] then
        begin
          vFindedSnap := GetPointByTargetEntity(vEnt, BasePoint, vPt1, vBox,
            vCadSnapDistance, vPt);
          if osTangent in vFindedSnap then
            ASnapActive := [];
        end;
        if cnstEdgeSnapMask * ASnapActive <> [] then
        begin
          vFindedSnap := GetPointByEdgeEntity(vEnt, BasePoint, vPt1, vBox,
            ASnapActive, vPt);
        end;
        if vFindedSnap <> [] then
          vSnappedEntity := vEnt;
      end
      else
        SetNonSnappedResults;
    finally
      vMatrix.SearchMode := vSearchMode;
    end;
  end;

  function IsValidDrawingBySnap: Boolean;
  begin
    Result := (vMatrix <> nil) and (FImg.CurrentLayout <> nil) and (not Is3DRect(FImg.CurrentLayout.Box));
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  vSnappedEntity := nil;
  vMatrix := GetSelectionMatrix;
  vExcludeSnap := [];
  vFindedSnap := [];
  {$IFDEF SGVER_5_2_5}
  if FControl.Left < 0 then
    X := X + FControl.Left;
  if FControl.Top < 0 then
    Y := Y + FControl.Top;
  {$ENDIF}
  if (FSnapMask <> []) and not (osDisabled in FSnapMask) then
  begin
    if FObjectSnap <> nil then
      vSnapDistance := FObjectSnap.SnapDistance
    else
      vSnapDistance := cnstSnapDistance;
    vCadSnapDistance := DistanceFPoint(GetFPoint(0, 0),
       GetFPoint(vSnapDistance, 0));

    vPt2 := MakeFPoint(MaxTsgFloat, MaxTsgFloat, MaxTsgFloat);
    PointInUCS := cnstFPointZero;
    if cnstObjectSnapMask * FSnapMask <> []  then
    begin
      vP1 := Point(X, Y);
      Result := FObjectSnap.GetSnapPointUCS(vP1, vFindedSnap, vSnappedEntity, PointInUCS);
      vPt2 := Result.CADPointInUCS;
      if vFindedSnap <> [] then
      begin
        vP := GetPoint(vPt2);
        if DistancePoint(vP, vP1) > cnstSearchSnapPrecision  then
        begin
          vFindedSnap := [];
          vSnappedEntity := nil;
        end;
      end;
    end;

    vFindedSnap := vFindedSnap * FSnapMask;
    vFindedSnapByMatrix := (cnstEdgeSnapMask + cnstTangentSnapMask +
       cnstIntersectionSnapMask) * FSnapMask;

    if (vFindedSnap = []) and (vFindedSnapByMatrix <> []) then
    begin
      if IsValidDrawingBySnap then
      begin
        vSearchPrecision := vMatrix.Precision;
        try
          vMatrix.Precision := vSnapDistance;
          vBox := cnstBadRect;
          ExpandFRect(vBox, GetFPoint(X + cnstSearchSnapPrecision, Y + cnstSearchSnapPrecision));
          ExpandFRect(vBox, GetFPoint(X + cnstSearchSnapPrecision, Y - cnstSearchSnapPrecision));
          ExpandFRect(vBox, GetFPoint(X - cnstSearchSnapPrecision, Y - cnstSearchSnapPrecision));
          ExpandFRect(vBox, GetFPoint(X - cnstSearchSnapPrecision, Y + cnstSearchSnapPrecision));
          if osIntersection in FSnapMask then
            CrossSnap;
          if not (osIntersection in vFindedSnap) then
            EdgeSnap(vFindedSnapByMatrix);
        finally
          vMatrix.Precision := vSearchPrecision;
        end;
      end
      else
        SetNonSnappedResults;
    end
    else
    begin
      if vFindedSnap = []  then
        SetNonSnappedResults
      else
      begin
        vPt := vPt2;
        PointInUCS := GetUCSPoint(vPt);
      end;
    end;
    vFindedSnap := vFindedSnap * FSnapMask;
    Result.CADPoint := vPt;
  end
  else
  begin
    SetNonSnappedResults;
    vFindedSnap := [];
  end;
  if vFindedSnap = [] then
    SetNonSnappedResults
  else
    Result.SnapMode := vFindedSnap;
  Result.CADPointInUCS := vPt;
  FSnappedEntity := vSnappedEntity;
  Result.Entity := vSnappedEntity;
  DoSnap;
  if IsValidDrawingBySnap and (FSelectionMatrix = nil) then
  begin
    DoTracers;
    if vSnappedEntity <> nil then
    begin
      FSnappedEntityForTrace := vSnappedEntity;
      FSnappedEntityForTraceTwo := TsgDXFEntity(Result.EntityIntersected);
      FInsert := Result.Insert;
    end
    else
      FSnappedEntityForTraceTwo := nil;
  end;
  Result.SnapType := vFitrstSnapMode;
end;

function TSnapControl.GetMouseCoordUCSTracers(AMatrix: TsgSelectionMatrix; X,
  Y: Integer; var PointInUCS: TFPoint): TSnapData;
var
  vObjectSnapMode: TObjectSnapMode;
  vPoint: TFPOint;
  vSnapState: TObjectSnapState;
begin
  FSelectionMatrix := AMatrix;
  vPoint := FSnapMarker.FRealPoint;
  vObjectSnapMode := FSnapMarker.FDestinationSnap;
  vSnapState := FSnapMask;
  FSnapMask := FSnapMask * [osIntersection, osNormal];
  Include(FSnapMask, osNearest);
  try
    Result.Entity := nil;
    Result.EntityIntersected := nil;
    Result := GetMouseCoordUCS(X, Y, PointInUCS);
  finally
    FSnapMarker.FDestinationSnap := vObjectSnapMode;
    FSnapMarker.FRealPoint :=  vPoint;
    FSelectionMatrix := nil;
    FSnapMask := vSnapState;
  end;
end;

function TSnapControl.GetPoint(const APoint: TFPoint): TPoint;
begin
  Result := FControlProps.WorldToClient(APoint);
end;

function TSnapControl.GetSelectionMatrix: TsgSelectionMatrix;
begin
  if FSelectionMatrix = nil then
    Result :=  TsgCADImageAccess(FImg).SelectionMatrix
  else // for tracers
   Result := FSelectionMatrix;
end;

function TSnapControl.GetMouseCoord(X, Y: Integer): TFPoint;
var
  Temp: TFPoint;
begin
  Result := GetMouseCoordUCS(X, Y, Temp).CADPointInUCS;
end;

procedure TSnapControl.Clear;
begin
  FSnappedEntity := nil;
  FSnappedEntityForTrace := nil;
  FSnappedEntityForTraceTwo := nil;
end;

procedure TSnapControl.Dispatch(var Message);
{$IFNDEF SG_FIREMONKEY}
var
  P: TPoint;
{$ENDIF}
begin
  inherited Dispatch(Message);
{$IFDEF SG_WINAPI}
  case TMessage(Message).Msg of
    WM_PAINT: Repaint;
{$IFNDEF SG_FIREMONKEY}
    WM_SCALECHANGING, WM_PICTUREMOVE:
      if Assigned(FImg) and GetCursorPos(P) then
      begin
        P := FControlProps.ScreenToClient(P);
        GetMouseCoord(P.X, P.Y);
      end;
{$ENDIF}
  end;
{$ENDIF}
end;

{$IFNDEF sgDXFONLY}
function GetAreaHPGL(AList: TList; AHPGLImage: TsgHPGLImage): Double;
var
  I, Cnt: Integer;
  vList: TList;
  vPPt: PFPoint;
begin
  Result := 0.0;
  if (AHPGLImage = nil) or not(AHPGLImage is TsgHPGLImage) then Exit;
  Cnt := AList.Count;
  if Cnt < 3 then Exit;
  vList := TList.Create;
  try
    for I := 0 to Cnt - 1 do
    begin
      New(vPPt);
      vPPt^ := AHPGLImage.ConvertHPGLStepsToMM{ConvertMMToHPGLSteps}(PFPoint(AList.Items[I])^);
      vList.Add(vPPt) ;
    end;
    Result := GetArea(vList);
  finally
    ClearRecordList(vList);
    vList.Free;
  end;
end;
{$ENDIF}

function GetArea(AList: TList): Double;
var
  I, Cnt, J: Integer;
  Pt: array [0 .. 1] of PFPoint;
begin
  Result := 0.0;
  Cnt := AList.Count;
  if Cnt >= 3 then
  begin
    Dec(Cnt);
    J := 0;
    Pt[J] := PFPoint(AList.Items[Cnt]);
    for I := 0 to Cnt do
    begin
      J := J xor 1;
      Pt[J] := PFPoint(AList.Items[I]);
      Result := Result + (Pt[J]^.X + Pt[J xor 1]^.X) * (Pt[J]^.Y - Pt[J xor 1]^.Y);
    end;
    Result := Abs(Result) * 0.5;
  end;
end;

function GetArea(AList: TFPointList): Double;
var
  I, Cnt, J: Integer;
  Pt: array [0 .. 1] of TFPoint;
begin
  Result := 0.0;
  Cnt := AList.Count;
  if Cnt >= 3 then
  begin
    Dec(Cnt);
    J := 0;
    Pt[J] := AList[Cnt];
    for I := 0 to Cnt do
    begin
      J := J xor 1;
      Pt[J] := AList[I];
      Result := Result + (Pt[J].X + Pt[J xor 1].X) * (Pt[J].Y - Pt[J xor 1].Y);
    end;
    Result := Abs(Result) * 0.5;
  end;
end;

function GetDistanceUnits(ADXFImage: TsgCADImage; ALayout: TsgDXFLayout;
         var UnitsUserName: string; var UnitsUserFactor: Double;
         var AltUnitsUserName: string; var AltUnitsUserFactor: Double): TsgDistanceUnits;
var
  R: Double;
begin
  Result := duNone;
  UnitsUserFactor := 1;
  AltUnitsUserFactor := -1;
  if (ADXFImage = nil) or (ALayout = nil) then Exit;
  if ALayout.IsModel then
  begin
    Result := ADXFImage.Measurement.DistanceUnits;
    UnitsUserName := ADXFImage.Converter.HeadVarStruct.DimProps.Post;
    UnitsUserFactor := ADXFImage.Converter.HeadVarStruct.DimProps.LFac;
    if ADXFImage.Converter.HeadVarStruct.DimProps.Alt then
    begin
      AltUnitsUserName := ADXFImage.Converter.HeadVarStruct.DimProps.APost;
      AltUnitsUserFactor := ADXFImage.Converter.HeadVarStruct.DimProps.AltF;
    end
    else
    begin
      AltUnitsUserName := '';
      AltUnitsUserFactor := 1; {-1;}
    end;
  end
  else //if layout
  begin
    if ALayout.PlotSettings.PlotPaperUnits = ppuPixels then
    begin
      Result := duMM;
      UnitsUserName := SMM;
    end
    else
    begin
      Result := duInch;
      UnitsUserName := SInch;
    end;
    R := ALayout.PlotSettings.DenominatorOfCustomPrintScale;
    if Abs(R) < fAccuracy then
      R := 1;
    UnitsUserFactor := 1 / R;
  end;
end;

procedure GetDrawingSize(AGraphic: TsgCADImage; ALayout: TsgDXFLayout;
  var Width, Height: TsgFloat; var Units: CADImage.TsgDistanceUnits);
var
  vUserName, vAltUserName: string;
  vUserFac, vAltUserFac: Double;
  //vDistUnitsSys: TDistanceUnits;
  vWidth, vHeight: Double;
  vRect: TFRect;
  vDPI: TPoint;
begin
  Units := GetDistanceUnits(TsgCADImage(AGraphic),
        TsgCADImage(AGraphic).CurrentLayout, vUserName, vUserFac,
        vAltUserName, vAltUserFac);
  vRect := TsgCADImage(AGraphic).Extents;
//  if not(AGraphic is TsgHPGLImage) then
//  begin
  vWidth := vRect.Right - vRect.Left;
  vHeight := vRect.Top - vRect.Bottom;
  if (AGraphic.Measurement.DistanceUnits = duNone) and (AGraphic.Measurement.AngularUnits = auNone) then
  begin
    vDPI := sgFunction.GetLogPixelsPerInch;
    vWidth := vWidth / vDPI.X;
    vHeight := vHeight / vDPI.Y;
  end;
  vUserFac := ALayout.PlotSettings.DenominatorOfCustomPrintScale;
  if (AGraphic.CurrentLayout.IsModel)or(Abs(vUserFac - 0) < fAccuracy) then
  begin
    Width := vWidth;
    Height := vHeight;
  end
  else
  begin
    Width := vWidth / vUserFac;
    Height := vHeight / vUserFac;
  end;
//  end
//  else
//  begin
//    {vLeftTop := TsgHPGLImage(AGraphic).ConvertHPGLStepsToMM(vRect.TopLeft);
//    vRightBottom := TsgHPGLImage(AGraphic).ConvertHPGLStepsToMM(vRect.BottomRight);
//    Width := vRightBottom.X - vLeftTop.X;
//    Height := vLeftTop.Y - vRightBottom.Y;}
//    Width := TsgHPGLImage(AGraphic).MMWidth / 100;
//    HEight := TsgHPGLImage(AGraphic).MMHeight / 100;
//  end;
end;

{$IFDEF SGVER_5_2_5}
function GetRealPointUsingsgImagePoint(AImage: TImage; X, Y: Integer; var PtInUCS: TFPoint): TFPoint;
var
  vRect: TRect;
  vXScaled, vYScaled: Extended;
begin
  {$IFDEF SGVER_5_2_5}
  if AImage is TsgImage then
    vRect := TsgImage(AImage).PictureRect
  else
  {$ENDIF}
    vRect := AImage.BoundsRect;
  if (not (AImage.Picture.Graphic is TsgDXFImage)) or
     (vRect.Right - vRect.Left = 0) or (vRect.Bottom - vRect.Top = 0) then
  begin
    Result := MakeFPoint(0, 0, 0);
    PtInUCS := Result;
    Exit;
  end;
  if ((vRect.Right - vRect.Left) <= AImage.Width) or ((vRect.Bottom - vRect.Top) <= AImage.Height) then
  begin
    vXScaled := X / (vRect.Right - vRect.Left);
    vYScaled := (AImage.Height - Y) / (vRect.Bottom - vRect.Top);
    Result := TsgDXFImage(AImage.Picture.Graphic).GetCADCoords(vXScaled, vYScaled, PtInUCS);
  end
  else
  begin
    vXScaled := (-(vRect.Left  - AImage.Left) + X) / (vRect.Right - vRect.Left);
    vYScaled := 1 - (Y - (vRect.Top - AImage.Top)) / (vRect.Bottom - vRect.Top);
    Result := TsgDXFImage(AImage.Picture.Graphic).GetCADCoords(vXScaled, vYScaled, PtInUCS);
  end;
end;
{$ENDIF}

function ReloadInserts(const ABlock: TsgDXFBlock; const AConv: TsgDXFConverter;
  AEnt: TsgDXFEntity): Boolean;
var
  I: Integer;
  vBlock: TsgDXFBlock;
begin
  Result := False;
  if not (Assigned(AConv) and Assigned(ABlock) and Assigned(AEnt)) then
    Exit;
  if AEnt is TsgDXFLayout then
  begin
    if ReloadInserts(ABlock, AConv,  TsgDXFLayout(AEnt).PaperSpaceBlock) then
      Result := True;
  end
  else
  begin
    if AEnt is TsgDXFBlock then
    begin
      vBlock := TsgDXFBlock(AEnt);
      for I := 0 to vBlock.Count - 1 do
        if ReloadInserts(ABlock, AConv, vBlock.Entities[I]) then
        begin
          vBlock.IsLoaded := False;
          Result := True;
        end;
    end
    else
    begin
      if AEnt is TsgDXFInsert then
      begin
        if TsgDXFInsert(AEnt).Block = ABlock then
          Result := True
        else
          if ReloadInserts(ABlock, AConv, TsgDXFInsert(AEnt).Block) then
            Result := True;
      end;
    end;
  end;
  if Result then
    AConv.Loads(AEnt);
end;


{ TsgControlTool }

constructor TsgControlTool.Create(AControlProps: TsgCustomControlToolProps);
begin
  inherited Create;
  FInvertDrawMatrix := cnstCrossYMat;
  FControlProps := AControlProps;
  FLinearDimensionsMode := umDrawing;
  FLinearDimensionsScale := 1.0;
  TsgCADImageAccess.RegisterDestroyNotification(CADImageDestroy);
end;

destructor TsgControlTool.Destroy;
begin
  FControlProps := nil;
  TsgCADImageAccess.UnRegisterDestroyNotification(CADImageDestroy);
  inherited Destroy;
end;

function TsgControlTool.GetActualGraphic: TGraphic;
var
  R: TRect;
  vVewBox: TFRect;

  function _Round(const AValue: Double): Integer;
  begin
    if Abs(AValue) > MaxInt  then
      Result := Sign(AValue) * MaxInt
    else
      Result := Round(AValue);
  end;

  function RoundRect2D(const ARect: TF2DRect): TRect;
  begin
    Result.Left := _Round(ARect.Left);
    Result.Top := _Round(ARect.Top);
    Result.Right := Result.Left + _Round(ARect.Right - ARect.Left);
    Result.Bottom := Result.Top + _Round(ARect.Bottom - ARect.Top);
  end;

begin
  if FControlProps.GetViewRectMode then
  begin
    R := FControlProps.GetViewRect;
    vVewBox.TopLeft := PointToPosition(Point(R.Left, R.Top));
    vVewBox.BottomRight := vVewBox.TopLeft;
    ExpandFRect(vVewBox, PointToPosition(Point(R.Right, R.Bottom)));
  end
  else
  begin
    R := FControlProps.GetClipRect;
    vVewBox := FControlProps.GetClipBox;
  end;
  Result := sgTools.GetActualGraphic(FControlProps.GetGraphic, RoundRect2D(GetPictureRect2D), R,
    FControlProps.IsClipRectActive or FControlProps.GetViewRectMode, @vVewBox);
end;

function TsgControlTool.GetDrawMatrix: TFMatrix;
begin
  if Assigned(FImg) then
    Result := TsgCADImageAccess(FImg).FDraw.DrawMatrix
  else
    Result := cnstCrossYMat;
end;

function TsgControlTool.GetPictureRect2D: TF2DRect;
begin
  Result := MakeF2DRect(FPictureRect.Left, FPictureRect.Bottom,
    FPictureRect.Right, FPictureRect.Top);
end;

function TsgControlTool.GetFPictureRect: TFRect;
begin
  Result := FPictureRect;
end;

function TsgControlTool.GetImg: TsgCADImage;
begin
  Result := FImg;
end;

function TsgControlTool.GetInvertDrawMatrix: TFMatrix;
begin
  Result := FInvertDrawMatrix;
end;

function TsgControlTool.GetIsNotCustomDraw: Boolean;
begin
  Result := Assigned(FImg) and not FImg.CustomDraw;
end;

function TsgControlTool.GetRealImageMatrix: TFMatrix;
begin
  if Assigned(FImg) and Assigned(FImg.CurrentLayout) then
    Result := TsgCADImageAccess(FImg).GetRealImageMatrix
  else
    Result := cnstIdentityMat;
end;

function TsgControlTool.GetViewportRect: TRect;
begin
  if Assigned(FImg) then
    Result := TsgCADImageAccess(FImg).ViewportRect
  else
    Result := FControlProps.GetViewRect;
end;

function TsgControlTool.Point2DToPosition(const APoint: TF2DPoint): TFPoint;
begin
  Result := MakeFPoint(APoint.X, APoint.Y, 0.5 * (FPictureRect.Z2 - FPictureRect.Z1));
  Result := FPointXMat(Result, FInvertDrawMatrix);
end;

function TsgControlTool.PointToPosition(const APoint: TPoint): TFPoint;
begin
  Result := Point2DToPosition(MakeF2DPoint(APoint.X, APoint.Y));
end;

procedure TsgControlTool.MatrixChanged(Sender: TObject);
  function Rnd(const V: Double): Integer;
  begin
    if (V > MaxInt) or (V < -MaxInt) then
      Result := Sign(V) * MaxInt
    else
      Result := Round(V)
  end;
begin
  FInvertDrawMatrix := GetDrawMatrix;
  if Assigned(FImg.CurrentLayout) then
    FPictureRect := FImg.CurrentLayout.Box
  else
    FPictureRect := MakeFRect(0, 1, 0, 1, 0, 0);
  TransRectCorners(FPictureRect, FInvertDrawMatrix);
  InvertMatrix(FInvertDrawMatrix);
  TsgCADImageAccess(FImg).FClientClipRect := Rect(Rnd(FPictureRect.Left),
    Rnd(FPictureRect.Bottom), Rnd(FPictureRect.Right), Rnd(FPictureRect.Top));
  if Assigned(FMatrixChanged) then
    FMatrixChanged(Sender);
end;

procedure TsgControlTool.MovePositionToPoint(const APosition: TFPoint;
  const APoint: TPoint);
var
  vPos: TFPoint;
begin
  vPos := FPointXMat(APosition, FImg.DrawMatrix);
  FImg.TranslateDrawMatrix(APoint.X - vPos.X, APoint.Y - vPos.Y,
    0.5 * (FPictureRect.Z2 - FPictureRect.Z1) - vPos.Z);
end;

procedure TsgControlTool.ResetRotCenter(const ACenter: TPoint; AOrbitAutoTarget: Boolean);
begin
  if IsNotCustomDraw then
    if not AOrbitAutoTarget then
      FImg.Offset := PointToPosition(ACenter)
    else
      FImg.Offset := FImg.Center;
end;

procedure TsgControlTool.SetDrawMatrix(const AMatrix: TFMatrix);
var
  vChange: TNotifyEvent;
begin
  vChange := FImg.OnChange;
  try
    FImg.OnChange := nil;
    TsgCADImageAccess(FImg).SetDrawingMatrixEx(AMatrix);
  finally
    FImg.OnChange := vChange;
    TsgCADImageAccess(FImg).Changed(FImg);
  end;
end;

procedure TsgControlTool.SetImg(AImage: TsgCADImage);
begin
  if AImage <> FImg then
  begin
    if Assigned(FImg) then
    begin
      TsgCADImageAccess(FImg).OnMatrixChanged := FMatrixChanged;
      FMatrixChanged := nil;
    end;
    FImg := AImage;
    if Assigned(FImg) then
    begin
{$IFNDEF SG_THREAD_DRAW}
      TsgCADImageAccess(FImg).SetCanvas(FControlProps.GetCanvas);
{$ENDIF}
      FMatrixChanged := TsgCADImageAccess(FImg).OnMatrixChanged;
      TsgCADImageAccess(FImg).OnMatrixChanged := MatrixChanged;
      MatrixChanged(Self);
    end;
  end;
end;

procedure TsgControlTool.SetViewportRect(const Value: TRect);
begin
  if Assigned(FImg) then
    TsgCADImageAccess(FImg).ViewportRect := Value;
end;

function TsgControlTool.UpdateImg: Boolean;
var
  vGraphic: TGraphic;
begin
  vGraphic := FControlProps.GetGraphic;
  if vGraphic <> FImg then
    if not Assigned(vGraphic) or (vGraphic is TsgCADImage) then
      SetImg(TsgCADImage(vGraphic));
  Result := Assigned(FImg);
end;

procedure TsgControlTool.ApplyScale(const ARect: TRect);
begin
  if Assigned(FImg) then
    TsgCADImageAccess(FImg).ApplyScale(ARect);
end;

procedure TsgControlTool.ApplyScale(const ARect: TF2DRect);
begin
  if UpdateImg and Assigned(FImg.CurrentLayout) then
    TsgCADImageAccess(FImg).ApplyScale(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TsgControlTool.CoordinateParameters(var AName: string;
  var AFactor: Double);
var
  vName, vAltName: string;
  vFactor, vAltFactor: Double;
  vSystem: TsgDistanceUnits;
begin
  vFactor := 1;
  vName := '';
  if Assigned(FImg) then
  begin
    vSystem := GetDistanceUnits(FImg, FImg.CurrentLayout, vName, vFactor, vAltName, vAltFactor);
    case LinearDimensionsMode of
      umDrawing:
        begin
          case vSystem of
            duInch: vName := SInch;
            duMM: vName := SMM;
          else
            vName := '';
          end;
          if (not FImg.CurrentLayout.IsModel)and(Abs(vFactor - 1) > fAccuracy) then
            vName := '';
          vFactor := 1;
        end; { case vSystem }
      umAltUser:
        begin
          vName := vAltName;
          vFactor := vFactor * vAltFactor;
        end;
    end; { case LinearDimensionsMode }
  end;
  if LinearDimensionsScale <> 1 then
    vFactor := vFactor * LinearDimensionsScale;
  AName := vName;
  AFactor := vFactor;
end;

procedure TsgControlTool.CADImageDestroy(Sender: TObject);
begin
  if Sender = FImg then
    SetImg(nil);
end;

function TsgControlTool.ConvertUnits(Pt: TFPoint): TFPoint;
begin
  if Assigned(FImg) then
    Result := FImg.InternalToCommonUnits(Pt)
  else Result := Pt;
end;

{ TsgSysMenuIcons }

{$IFDEF CS_USEFORM}
type
  TsgControlToolPropsHelper = class(TsgCustomControlToolProps)
  protected
    function GetParentForm(var AControl: TControl): TCustomForm;
  end;

function TsgControlToolPropsHelper.GetParentForm(var AControl: TControl): TCustomForm;
var
  P: TPersistent;
begin
  AControl := nil;
  Result := nil;
  P := GetOwner;
  if P is TControl then
  begin
    AControl := TControl(P);
    Result := Forms.GetParentForm(AControl{$IFDEF SGDEL_2005}, False{$ENDIF});
  end;
end;
{$ENDIF}

procedure TsgSysMenuIcons.SetButtonImage(var AImage: TGraphic; const AValue: TGraphic);
begin
  AImage.Free;
  if Assigned(AValue) then
  begin
    AImage := TGraphicClass(AValue.ClassType).Create;
    AImage.Assign(AValue);
  end
  else
    AImage := nil;
end;

procedure TsgSysMenuIcons.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    Invalidate;
  end;
end;

procedure TsgSysMenuIcons.SetHeight(const AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    Invalidate;
  end;
end;

procedure TsgSysMenuIcons.SetHintClose(const AValue: string);
begin
  FHintClose := AValue;
end;

procedure TsgSysMenuIcons.SetHintMinimize(const AValue: string);
begin
  FHintMinimize := AValue;
end;

procedure TsgSysMenuIcons.SetHintRestoreDown(const AValue: string);
begin
  FHintRestoreDown := AValue;
end;

procedure TsgSysMenuIcons.SetHints(Index: TsgSysMenuIcon; const AValue: string);
begin
  case Index of
    smiMinimize: HintMinimize := AValue;
    smiRestore: HintRestoreDown := AValue;
    smiClose: HintClose := AValue;
  end;
end;

procedure TsgSysMenuIcons.SetImage(Index: TsgSysMenuIcon;
  const AValue: TGraphic);
begin
  case Index of
    smiMinimize: ImageMinimaze := AValue;
    smiRestore: ImageRestoreDown := AValue;
    smiClose: ImageClose := AValue;
  end;
end;

procedure TsgSysMenuIcons.SetImageClose(const Value: TGraphic);
begin
  SetButtonImage(FImageClose, Value);
end;

procedure TsgSysMenuIcons.SetImageMinimaze(const Value: TGraphic);
begin
  SetButtonImage(FImageMinimaze, Value);
end;

procedure TsgSysMenuIcons.SetImageRestoreDown(const Value: TGraphic);
begin
  SetButtonImage(FImageRestoreDown, Value);
end;

procedure TsgSysMenuIcons.SetInvertColor(const AValue: Boolean);
begin
  if FInvertColor <> AValue then
  begin
    FInvertColor := AValue;
    Invalidate;
  end;
end;

procedure TsgSysMenuIcons.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Invalidate;
  end;
end;

{$IFDEF SG_WINAPI}
function TsgSysMenuIcons.DoMouseDown(var Message: TWMMouse): Boolean;
begin
  Result := DoMouseMove(Message);
  if Result then
    FDown := True;
end;

function TsgSysMenuIcons.DoMouseMove(var Message: TWMMouse): Boolean;
{$IFDEF CS_USEFORM}
var
  vControl: TControl;
  vForm: TCustomForm;
  vIcon: TsgSysMenuIcon;
{$ENDIF}
begin
  Result := False;
{$IFDEF CS_USEFORM}
  vIcon := Pred(smiMinimize);
  vForm := TsgControlToolPropsHelper(FControlProps).GetParentForm(vControl);
  if Assigned(vControl) then
  begin
    if Assigned(vForm) and (vForm.WindowState = wsMaximized) then
    begin
      vIcon := GetIconFromPoint(Point(Message.XPos, Message.YPos));
      if vIcon in [smiMinimize .. smiClose] then
      begin
        if vControl.Cursor <> crDefault then
        begin
          FCursor := vControl.Cursor;
          FCursorNeedStore := False;
          try
            vControl.Cursor := crDefault;
          finally
            FCursorNeedStore := True;
          end;
        end;
        Result := True;
      end
      else
        vControl.Cursor := FCursor;
    end;
    if vControl.Hint <> Hints[vIcon] then
      Application.CancelHint;
    vControl.Hint := Hints[vIcon];
  end;
{$ENDIF}
end;

function TsgSysMenuIcons.DoMouseUp(var Message: TWMMouse): Boolean;
{$IFDEF CS_USEFORM}
var
  vControl: TControl;
  vForm: TCustomForm;
{$ENDIF}
begin
  Result := False;
  if FDown then
  begin
    FDown := False;
{$IFDEF CS_USEFORM}
    vForm := TsgControlToolPropsHelper(FControlProps).GetParentForm(vControl);
    if Assigned(vForm) and (vForm.WindowState = wsMaximized) then
    begin
      Result := True;
      case GetIconFromPoint(Point(Message.XPos, Message.YPos)) of
        smiMinimize: vForm.WindowState := wsMinimized;
        smiRestore: vForm.WindowState := wsNormal;
        smiClose: vForm.Close;
      else
        Result := False;
      end;
    end;
{$ENDIF}
  end;
end;
{$ENDIF}

procedure TsgSysMenuIcons.Paint(ADC: HDC);
{$IFDEF CS_USEFORM}
var
  vRect: TRect;
  vControl: TControl;
  vForm: TCustomForm;
  vCanvas: TCanvas;
  vColor, vCtrlColor: TColor;

  procedure RectToSquare(var ARect: TRect);
  var
    vRectSize: Integer;
    vW, vH: Integer;
  begin
    vW := ARect.Right - ARect.Left;
    vH := ARect.Bottom - ARect.Top;
    vRectSize := Min(vW, vH);
    InflateRect(ARect, -((vW - vRectSize) div 2), -((vH - vRectSize) div 2));
  end;
{$ENDIF}
begin
{$IFDEF CS_USEFORM}
  vForm := TsgControlToolPropsHelper(FControlProps).GetParentForm(vControl);
  if (ADC <> 0) and Assigned(vControl) and Assigned(vForm) and (vForm.WindowState = wsMaximized) then
  begin
    vCanvas := TCanvas.Create;
    try
      vCanvas.Handle := ADC;
      if FInvertColor then
      begin
        vCtrlColor := ColorToRGB(TControlAccess(vControl).Color);
        vColor := not vCtrlColor and $FFFFFF;
        if Sqrt(Sqr(128 - vColor and $FF) +
           Sqr(128 - (vColor shr 8) and $FF) +
           Sqr(128 - (vColor shr 16) and $FF)) < 32 then
          vColor := clBlack;
      end
      else
        vColor := FColor;
      vCanvas.Pen.Width := 2;
      vCanvas.Pen.Color := vColor;
      vRect := GetSysMenuIconRect(smiClose);
      if Assigned(FImageClose) then
        vCanvas.Draw(vRect.Left, vRect.Bottom - FImageClose.Height, FImageClose)
      else
      begin
        RectToSquare(vRect);
        InflateRect(vRect, -2, -2);
        OffsetRect(vRect, 0, 1);
        vCanvas.MoveTo(vRect.Left, vRect.Top);
        vCanvas.LineTo(vRect.Right, vRect.Bottom);
        vCanvas.MoveTo(vRect.Right, vRect.Top);
        vCanvas.LineTo(vRect.Left, vRect.Bottom);
      end;
      vRect := GetSysMenuIconRect(smiRestore);
      if Assigned(FImageRestoreDown) then
        vCanvas.Draw(vRect.Left, vRect.Bottom - FImageRestoreDown.Height, FImageRestoreDown)
      else
      begin
        vCanvas.Brush.Style := bsClear;
        vCanvas.Pen.Width := 1;
        RectToSquare(vRect);
        InflateRect(vRect, -2, -3);
        OffsetRect(vRect, 0, -1);
        vCanvas.MoveTo(vRect.Left, vRect.Top);
        vCanvas.LineTo(vRect.Right, vRect.Top);
        vCanvas.LineTo(vRect.Right, vRect.Bottom);
        vCanvas.LineTo(vRect.Right - 2, vRect.Bottom);
        vCanvas.MoveTo(vRect.Right, vRect.Top + 1);
        vCanvas.LineTo(vRect.Left, vRect.Top + 1);
        vCanvas.LineTo(vRect.Left, vRect.Top + 3);
        OffsetRect(vRect, -2, 3);
        vCanvas.MoveTo(vRect.Left, vRect.Top);
        vCanvas.LineTo(vRect.Right, vRect.Top);
        vCanvas.LineTo(vRect.Right, vRect.Bottom);
        vCanvas.LineTo(vRect.Left, vRect.Bottom);
        vCanvas.LineTo(vRect.Left, vRect.Top);
        vCanvas.MoveTo(vRect.Left, vRect.Top + 1);
        vCanvas.LineTo(vRect.Right, vRect.Top + 1);
      end;

      vCanvas.Pen.Width := 2;
      vRect := GetSysMenuIconRect(smiMinimize);

      if Assigned(FImageMinimaze) then
        vCanvas.Draw(vRect.Left, vRect.Bottom - FImageMinimaze.Height + 1, FImageMinimaze)
      else
      begin
        RectToSquare(vRect);
        InflateRect(vRect, -2, 0);
        OffsetRect(vRect, -2, 0);
        vRect.Top := vRect.Bottom - 2;
        vCanvas.Brush.Style := bsSolid;
        vCanvas.Brush.Color := vColor;
        vCanvas.FillRect(vRect);
      end;
      vCanvas.Handle := 0;
    finally
      vCanvas.Free;
    end;
  end;
{$ENDIF}
end;

function TsgSysMenuIcons.PointInIcons(const APoint: TPoint): Boolean;
begin
  Result := PtInIcon(smiClose, APoint) or PtInIcon(smiRestore, APoint) or
    PtInIcon(smiMinimize, APoint);
end;

function TsgSysMenuIcons.PtInIcon(const ASysMenuIcon: TsgSysMenuIcon;
  const APoint: TPoint): Boolean;
begin
  Result := PtInRect(GetSysMenuIconRect(ASysMenuIcon), APoint);
end;

constructor TsgSysMenuIcons.Create(AControlProps: TsgCustomControlToolProps);
begin
  inherited Create;
  FColor := {$IFDEF SG_FIREMONKEY}clGray{$ELSE}clDkGray{$ENDIF};
  FWidth := cnstIconWidth;
  FHeight := cnstIconHeight;
  FInvertColor := False;
  InitializeHints;
  FControlProps := AControlProps;
{$IFDEF CS_USEFORM}
  if FControlProps.GetOwner is TControl then
  begin
    FControlShowHint := TControl(FControlProps.GetOwner).ShowHint;
    TControl(FControlProps.GetOwner).ShowHint := True;
  end;
{$ENDIF}
{$IFDEF CONTROLS_TYPES}
  FCursor := TCursor(6);
{$ENDIF}
  FCursorNeedStore := True;
  FControlProps.AddClient(Self);
end;

destructor TsgSysMenuIcons.Destroy;
begin
  if Assigned(FControlProps) then
  begin
    FControlProps.RemoveClient(Self);
{$IFDEF CS_USEFORM}
    if FControlProps.GetOwner is TControl then
      TControl(FControlProps.GetOwner).ShowHint := FControlShowHint;
{$ENDIF}
  end;
  FreeAndNil(FImageClose);
  FreeAndNil(FImageMinimaze);
  FreeAndNil(FImageRestoreDown);
  inherited Destroy;
end;

procedure TsgSysMenuIcons.Dispatch(var Message);
{$IFDEF CS_USEFORM}
var
  vControl: TControl;
{$ENDIF}
begin
  inherited Dispatch(Message);
{$IFDEF SG_WINAPI}
  case TMessage(Message).Msg of
    WM_PAINT:
      Paint(TWMPaint(Message).DC);
    WM_LBUTTONUP:
      if DoMouseUp(TWMMouse(Message)) then
        TWMMouse(Message).Result := 1;
    WM_MOUSEMOVE:
      if DoMouseMove(TWMMouse(Message)) then
        TWMMouse(Message).Result := 1;
    WM_LBUTTONDOWN:
      if DoMouseDown(TWMMouse(Message)) then
        TWMMouse(Message).Result := 1;
{$IFDEF CS_USEFORM}
    CM_CURSORCHANGED:
      begin
        TsgControlToolPropsHelper(FControlProps).GetParentForm(vControl);
        if Assigned(vControl) and FCursorNeedStore then
          FCursor := vControl.Cursor;
      end;
{$ENDIF}
  end;
{$ENDIF}
end;

function TsgSysMenuIcons.GetHints(Index: TsgSysMenuIcon): string;
begin
  case Index of
    smiMinimize: Result := HintMinimize;
    smiRestore: Result := HintRestoreDown;
    smiClose: Result := HintClose;
  else
    Result := '';
  end;
end;

function TsgSysMenuIcons.GetIconFromPoint(
  const APoint: TPoint): TsgSysMenuIcon;
begin
  Result := TsgSysMenuIcon(Ord(smiMinimize) - 1);
  if PtInIcon(smiClose, APoint) then
    Result := smiClose
  else
    if PtInIcon(smiRestore, APoint) then
      Result := smiRestore
    else
      if PtInIcon(smiMinimize, APoint) then
        Result := smiMinimize;
end;

function TsgSysMenuIcons.GetImage(Index: TsgSysMenuIcon): TGraphic;
begin
  case Index of
    smiMinimize: Result := ImageMinimaze;
    smiRestore: Result := ImageRestoreDown;
    smiClose: Result := ImageClose;
  else
    Result := nil;
  end;
end;

function TsgSysMenuIcons.GetSysMenuIconRect(
  const ASysMenuIcon: TsgSysMenuIcon): TRect;
begin
  Result := FControlProps.GetViewRect;
  Result.Left := Result.Right - FWidth;
  Result.Bottom := Result.Top + FHeight;
  OffsetRect(Result, -cnstIconOffset, cnstIconOffset);
  case ASysMenuIcon of
    smiRestore: OffsetRect(Result, -FWidth, 0);
    smiMinimize: OffsetRect(Result, -2 * FWidth, 0);
  end;
end;

procedure TsgSysMenuIcons.InitializeHints;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    FHintMinimize := LoadUser32Str(900);
    FHintClose := LoadUser32Str(905);
    FHintRestoreDown := LoadUser32Str(903);
  end
  else
{$ENDIF}
  begin
    FHintMinimize := cnstHintMinimize;
    FHintClose := cnstHintClose;
    FHintRestoreDown := cnstHintRestoreDown;
  end;
end;

procedure TsgSysMenuIcons.Invalidate;
begin
  if Assigned(FControlProps) then
    FControlProps.Invalidate;
end;

{ TsgTracers }

function TsgTracers.Active: Boolean;
begin
  Result := Count > 0;
end;

procedure TsgTracers.Add(Apoint: TFPoint; ASnapControl: TSnapControl; ASnappedEntity, ASnappedEntityTwo: TsgDXFEntity);
var
  vTrace: TSnapMarkerTrace;
  vLine: TsgDXFLineTrace;
  vsgLine: TsgLine;
  vSnapState: TObjectSnapState;
  vSnappedEntity: TsgDXFEntity;

  procedure AddTracerEntities;
  var
    vFRect: TFRect;
    vFP: TFPoint;
    vSnappedPoly: TsgDXFPolyline;
    vIntersectionMode: TIntersectionMode;
    I, vIndBefore, vIndAfter: Integer;
{$IFDEF SG_BTI}
    vInsert: TsgDXFInsertAccess absolute vSnappedEntity;
{$ENDIF}
  begin
    vFRect := ViewRect;
    if osTrace in vSnapState then
    begin
      //horizontal
      vLine := TsgDXFLineTrace.Create;
      vFP := APoint;
      vFP.X := vFRect.Left;
      vLine.Point := vFP;
      vFP := APoint;
      vFP.X := vFRect.Right;
      vLine.Point1 := vFP;
      AddPointsToTracerMatrix(vTrace, vLine);
      //vertical
      vLine := TsgDXFLineTrace.Create;
      vFP := APoint;
      vFP.Y := vFRect.Top;
      vLine.Point := vFP;
      vFP := APoint;
      vFP.Y := vFRect.Bottom;
      vLine.Point1 := vFP;
      AddPointsToTracerMatrix(vTrace, vLine);
      if not IsEqualFPoints(BasePoint, BadPoint) then
      begin
        vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(Apoint, FBasePoint), vIntersectionMode);
        if vIntersectionMode <> imNone then
          AddPointsToTracerMatrix(vTrace, CreateLine(vsgLine));
      end;
    end;
    if (FSnapControl.FInsert <> nil)
    {$IFDEF SG_BTI}
      and not (TsgDXFInsertAccess(FSnapControl.FInsert).GetEntTypeEx in cnsrEntTypeExBySnap)
    {$ENDIF}
      then
        Exit;
    if vSnappedEntity is TsgDXFLine then
    begin
      // the following is bad, normal is commented!!!!
     { vLine := TsgDXFLineTrace.Create;
      vLine.Point := TsgDXFLine(vSnappedEntity).Point;
      vLine.Point1 := TsgDXFLine(vSnappedEntity).Point1;
      AddLine(vTrace, Apoint, vLine);
      vLine.Free; }
      AddLine(vTrace, Apoint, TsgDXFLine(vSnappedEntity));
    end;
    if vSnappedEntity is TsgDXFPolyline then
    begin
      vIndBefore := -1;
      vIndAfter := -1;
      vSnappedPoly := TsgDXFPolyline(vSnappedEntity);
      for I := 0 to vSnappedPoly.Count - 1 do
        if IsEqualFPoints(vSnappedPoly.Vertexes[I].Point, Apoint) then
        begin
          if I > 0 then
            vIndBefore := I - 1;
          if I < vSnappedPoly.Count - 1 then
            vIndAfter := I + 1;
          if vSnappedPoly.Closed then
          begin
            if I = 0 then
              vIndBefore := -2;
            if I = vSnappedPoly.Count - 1 then
              vIndAfter := -2;
          end;
        end;
    //extension 1
      if vIndBefore <> -1 then
      begin
        if vIndBefore = -2 then
          vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(vSnappedPoly.Vertexes[0].Point, vSnappedPoly.Vertexes[vSnappedPoly.Count - 1].Point), vIntersectionMode)
        else
          vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(vSnappedPoly.Vertexes[vIndBefore].Point, vSnappedPoly.Vertexes[vIndBefore + 1].Point), vIntersectionMode);
        if (vIntersectionMode <> imNone) then
          AddLine(vTrace, Apoint, CreateLine(vsgLine)).Free;
      end;
    //extension 2
      if vIndAfter <> -1 then
      begin
        if vIndAfter = -2 then
          vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(vSnappedPoly.Vertexes[0].Point, vSnappedPoly.Vertexes[vSnappedPoly.Count - 1].Point), vIntersectionMode)
        else
          vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(vSnappedPoly.Vertexes[vIndAfter].Point, vSnappedPoly.Vertexes[vIndAfter - 1].Point), vIntersectionMode);
        if (vIntersectionMode <> imNone) then
          AddLine(vTrace, Apoint, CreateLine(vsgLine)).Free;
      end;
    end;
    {$IFDEF SG_BTI}
    if vSnappedEntity is TsgCADPolyPolyline2D then
      AddPolyPolyline2D(vTrace, Apoint, TsgCADPolyPolyline2D(vSnappedEntity));
    {$ENDIF}
  end;

begin
  vTrace := TSnapMarkerTrace.Create(ASnapControl);
  FTracers.Add(vTrace);
  vTrace.FRealPoint := Apoint;
  vTrace.FSnappedEntity := ASnappedEntity;
  vTrace.FSnappedEntityTwo := ASnappedEntityTwo;
  vSnapState := FSnapControl.SnapMask;
  vSnappedEntity := ASnappedEntity;
  AddTracerEntities;
  if ASnappedEntityTwo <> nil then
  begin
    vSnappedEntity := ASnappedEntityTwo;
    AddTracerEntities;
  end;
end;

function TsgTracers.AddLine(ATrace: TSnapMarkerTrace; Apoint: TFPoint; ALine: TsgDXFLine): TsgDXFLine;
var
  vFRect: TFRect;
  vsgLine: TsgLine;
  vSnapState: TObjectSnapState;
  vIntersectionMode: TIntersectionMode;

  procedure AddParallel;
  var
    vIntersectionMode: TIntersectionMode;
  begin
    if not (osParallel in vSnapState) then
      Exit;
    if IsEqualFPoints(BasePoint, BadPoint) then
      Exit;
    vsgLine := GetLineParalleniar(BasePoint, vsgLine.Point1, vsgLine.Point2, 1000);
    vsgLine := GetIntresectingRectAndLine(ViewRect, vsgLine, vIntersectionMode);
    if vIntersectionMode <> imNone then
      AddPointsToTracerMatrix(ATrace, CreateLine(vsgLine, osParallel));
  end;

begin
  Result := ALine;
  vFRect := ViewRect;
  vSnapState := FSnapControl.SnapMask;
  vsgLine := GetIntresectingRectAndLine(vFRect, MakeLine(ALine.Point, ALine.Point1), vIntersectionMode);
//extension
  if (vIntersectionMode <> imNone) and (osExtension in vSnapState) then
    AddPointsToTracerMatrix(ATrace, CreateLine(vsgLine, osExtension));
  AddParallel;
// normal
  vsgLine.Point1 := Apoint;
  vsgLine.Point2 := GetNormalPt(Result.Point , Result.Point1, Apoint, 1000);
  vsgLine := GetIntresectingRectAndLine(vFRect, vsgLine, vIntersectionMode);
  if (vIntersectionMode <> imNone) and (vSnapState * [osNormal, osTrace] = [osNormal, osTrace]) then
    AddPointsToTracerMatrix(ATrace, CreateLine(vsgLine, osOrtho));
end;

procedure TsgTracers.AddPointsToTracerMatrix(ATrace: TSnapMarkerTrace; ALine: TsgDXFLineTrace);
var
  P: TPoint;
  R: TRect;
begin
  R := FSnapControl.ControlProps.GetViewRect;
  FTracersMatrix.Size := Point(R.Right - R.Left, R.Bottom - R.Top);
  FTracersMatrix.AddElement(ALine);
  ATrace.FLines.Add(ALine);
  FTracersMatrix.Changed;
  P := FSnapControl.GetPoint(ALine.Point);
  FTracerPointsInt[0] := P.X;
  FTracerPointsInt[1] := P.Y;
  P := FSnapControl.GetPoint(ALine.Point1);
  FTracerPointsInt[2] := P.X;
  FTracerPointsInt[3] := P.Y;
  FTracerPointsInt[4] := P.X;
  FTracerPointsInt[5] := P.Y + 1;
{$IFDEF SG_FIREMONKEY}
  try
    FTracersMatrix.Canvas.BeginScene;
{$ENDIF}
  FTracersMatrix.Canvas.Polyline(Slice(PPoints(@FTracerPointsInt[0])^, 3));
{$IFDEF SG_FIREMONKEY}
  finally
    FTracersMatrix.Canvas.EndScene;
  end;
{$ENDIF}
end;

{$IFDEF SG_BTI}
procedure TsgTracers.AddPolyPolyline2D(ATrace: TSnapMarkerTrace;
  Apoint: TFPoint; APoly: TsgCADPolyPolyline2D);
var
  J, Cnt: Integer;
  L: TsgLine;
begin
  Cnt := APoly.CurvesCount - 1;
  if (Cnt > 0) and (Cnt < cnstMaxCountPointsInPolyLineEdge) then
  begin
    for J := 0 to Cnt do
    begin
      if APoly.CurveVisible[J] then
      begin
        if not APoly.IsCurveArc(J) then
        begin
          L := APoly.Lines[J];
          if IsEqualFPoints(L.Point1, Apoint) or IsEqualFPoints(L.Point2, Apoint) then
            AddLine(ATrace, Apoint, CreateLine(L)).Free;
        end;
        // else FindArc(PPoly.Arcs[J])
      end;
    end;
  end;
end;
{$ENDIF}

procedure TsgTracers.Clear;
begin
  FBasePoint := BadPoint;
  ClearMarkers;
  if (FCADImage <> nil) and (FCADImage.SelectionMatrix <> nil) then
  begin
{$IFDEF SG_THREAD_DRAW}
    MonitorEnter(FCADImage.SelectionMatrix);
    try
{$ENDIF}
      FTracersMatrix.Assign(FCADImage.SelectionMatrix);
{$IFDEF SG_THREAD_DRAW}
    finally
      MonitorExit(FCADImage.SelectionMatrix);
    end;
{$ENDIF}
  end;
end;

procedure TsgTracers.ClearHidden;
begin
  ClearMarkers;
  FTracersMatrix.ClearFull;
end;

procedure TsgTracers.ClearMarkers;
var
  vMarker: TSnapMarker;
begin
  while FTracers.Count > 0 do
  begin
    vMarker := TSnapMarker(FTracers[FTracers.Count - 1]);
    try
      FTracers.Delete(FTracers.Count - 1);
    finally
      vMarker.Free;
    end;
  end;
end;

function TsgTracers.Count: Integer;
begin
  Result := FTracers.Count;
end;

constructor TsgTracers.Create(AsnapControl: TSnapControl);
begin
  FIsShiftClicked := False;
  FIsShiftPressed := False;
  FSnapControl := AsnapControl;
  FBasePoint := BadPoint;
  FTracers := TList.Create;
  FTracersMatrix := TsgSelectionMatrix.Create;
end;

procedure TsgTracers.Delete(Index: Integer);
var
  vMarker: TSnapMarkerTrace;
begin
  vMarker := Tracers[Index];
  try
    FTracers[Index] := nil;
    FTracers.Delete(Index);
    Regen;
  finally
    vMarker.Free;
  end;
end;

destructor TsgTracers.Destroy;
begin
  Clear;
  FTracers.Free;
  FTracersMatrix.Free;
  inherited Destroy;
end;

function TsgTracers.Find(const APoint: TFPoint; var Index: Integer): Boolean;
begin
  Index := 0;
  Result := False;
  while (Index < Count) and not Result do
    if IsEqualFPoints(Tracers[Index].FRealPoint, Apoint) then
      Inc(Result)
    else
      Inc(Index);
end;

function TsgTracers.GetTracers(Index: Integer): TSnapMarkerTrace;
begin
  Result := nil;
  if (Index >= 0) and (Index < FTracers.Count) then
    Result := TSnapMarkerTrace(FTracers[Index]);
end;

function TsgTracers.IndexOf(Apoint: TFPoint): Integer;
begin
  if not Find(Apoint, Result) then
    Result := -1;
end;

function TsgTracers.IndexOfEntity(AEntity: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Tracers[I].FLines.IndexOf(AEntity) <> -1 then
      Result := I;
end;

function TsgTracers.Last: TSnapMarkerTrace;
begin
  Result := TSnapMarkerTrace(FTracers.Last);
end;

procedure TsgTracers.Regen;
var
  I: Integer;
  vList: TList;
  vMarker: TSnapMarkerTrace;
begin
  if FCADImage = nil then
    Exit;
  vList := TList.Create;
  try
    CopyLists(vList, FTracers);
    FTracers.Clear;
    if FCADImage.SelectionMatrix <> nil then
    begin
{$IFDEF SG_THREAD_DRAW}
      MonitorEnter(FCADImage.SelectionMatrix);
      try
{$ENDIF}
        TracersMatrix.Assign(FCADImage.SelectionMatrix);
        for I := 0 to vList.Count - 1 do
        begin
          vMarker := TSnapMarkerTrace(vList[I]);
          Add(vMarker.FRealPoint, vMarker.FSnapControl, vMarker.FSnappedEntity, vMarker.FSnappedEntityTwo);
          vMarker.Free;
        end;
{$IFDEF SG_THREAD_DRAW}
      finally
        MonitorExit(FCADImage.SelectionMatrix);
      end;
{$ENDIF}
    end;
  finally
    vList.Free;
  end;
end;

function TsgTracers.Remove(Apoint: TFPoint): Integer;
begin
  if Find(Apoint, Result) then
    Delete(Result)
  else
    Result := -1;
end;

procedure TsgTracers.SetBasePoint(const Value: TFPoint);
begin
  FBasePoint := Value;
end;

procedure TsgTracers.SetCADImage(const Value: TsgCADImage);
begin
  if Value = FCADImage then
    Exit;
  FCADImage := Value;
  if (Value <> nil) and (Value.SelectionMatrix <> nil) then
    FTracersMatrix.Assign(Value.SelectionMatrix);
end;

procedure TsgTracers.SetDisable;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Tracers[I].LineByShowPolar^.State := 0;
end;

procedure TsgTracers.SetIsShiftClicked(const Value: Boolean);
begin
  FIsShiftClicked := Value;
end;

procedure TsgTracers.SetIsShiftPressed(const Value: Boolean);
begin
  FIsShiftPressed := Value;
end;

procedure TsgTracers.SetTracersMatrix(const Value: TsgSelectionMatrix);
begin
  FTracersMatrix := Value;
end;

procedure TsgTracers.SetViewRect(const Value: TFRect);
begin
  FViewRect := Value;
end;

{ TSnapMarkerTrace }

constructor TSnapMarkerTrace.Create(ASnapControl: TSnapControl);
begin
  inherited Create(ASnapControl);
  New(FLineByShowPolar);
  FLines := TList.Create;
end;

destructor TSnapMarkerTrace.Destroy;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    TObject(FLines[I]).Free;
  FLines.Free;
  Dispose(FLineByShowPolar);
  inherited;
end;

function TSnapMarkerTrace.GetRealPoint: TFPoint;
begin
  Result := FRealPoint;
end;

procedure TSnapMarkerTrace.SetSnappedEntity(const Value: TsgDXFEntity);
begin
  FSnappedEntity := Value;
end;

{ TSnapControlTracers }

function TSnapControlTracers.GetSelectionMatrix: TsgSelectionMatrix;
begin
  Result := FSelectionMatrix;
end;

procedure TSnapControlTracers.SetSelectionMatrix(
  const Value: TsgSelectionMatrix);
begin
  FSelectionMatrix := Value;
end;

{ TsgDXFLineTrace }

constructor TsgDXFLineTrace.Create;
begin
  inherited;
  FSnapType := osDisabled;
end;

procedure TsgDXFLineTrace.SetSnapType(const Value: TObjectSnapMode);
begin
  FSnapType := Value;
end;

{ TsgSnapCross }

function GetSnapType(const AEntity: TObject; const AConstants: TsgConstantsCustom): TsgSnapType;

  function IsValidInsert(const AEnt: TObject; const AConstants: TsgConstantsCustom): Boolean;
  var
    vInsert: TsgDXFInsertAccess;
    vSnapInInserts: Boolean;
  begin
    Result := False;
    if (AEnt is TsgDXFInsert) then
    begin
      vInsert := TsgDXFInsertAccess(AEnt);
      vSnapInInserts := False;
      if Assigned(AConstants) then
        vSnapInInserts := AConstants.IntValue[cnstCode_SnapInserts] <> 0;
      if vSnapInInserts then
        Result := IsEqualFMatrix(vInsert.GetMatrix, cnstIdentityMat, False) and
          IsEqualFPoints(vInsert.Scale, cnstFPointSingle);
{$IFDEF SG_BTI}
      if not Result then
        Result := vInsert.GetEntTypeEx in cnsrEntTypeExBySnap;
{$ENDIF}
    end;
  end;

begin
  Result := stUndefined;
  if AEntity is TsgDXFSolid then
    Result := stSolid
  else
    if AEntity is TsgDXFLine then
      Result := stLine
    else
      if AEntity is TsgCADBasePolyline then
      begin
        Result := stPolyline;
        if IsCircular(TsgDXFEntity(AEntity)) then
          Result := stArc;
      end
      else
        if IsValidInsert(AEntity, AConstants) then
          Result := stObjects
{$IFDEF SG_BTI}
        else
          if AEntity is TsgCADPolyPolyline2D then
            Result := stPolyPolyline;
{$ENDIF}
//          if AEntity is TsgFlatPoly then
//            Result := 5;
  end;

function GetCrossClass(const AType1, AType2: TsgSnapType):  TsgCrossProcs;
const
  cnstCrossClasses: array [TsgSnapType, TsgSnapType] of TsgCrossProcs =
  ( (nil, nil, nil, nil, nil, nil, nil),//stUndefined
    (nil, TsgCrossArcs, TsgCrossArcAndLine, TsgCrossSolidAndArc,
       TsgCrossPolylineAndArc, TsgCrossPolyPolylineAndArc, TsgCrossInsert),//stArc
    (nil, nil, TsgCrossLines, TsgCrossSolidAndLine, TsgCrossPolylineAndLine,
       TsgCrossPolyPolylineAndLine, TsgCrossInsert),//stLine
    (nil, nil, nil, TsgCrossSolids, TsgCrossPolylineAndSolid,
       TsgCrossPolyPolylineAndSolid, TsgCrossInsert),//stSolid
    (nil, nil, nil, nil, TsgCrossPolylines, TsgCrossPolyPolylineAndPolyline,
       TsgCrossInsert),//stPolyline
    (nil, nil, nil, nil, nil, TsgCrossPolyPolylines, TsgCrossInsert),//stPolyPolyline
    (nil, nil, nil, nil, nil, nil, TsgCrossInsert) //stObjects
  );
begin
  Result := cnstCrossClasses[AType1][AType2];
  if Result = nil then
    Result := cnstCrossClasses[AType2][AType1];
end;

constructor TsgSnapCross.Create(const AConstants: TsgConstantsCustom);
begin
  inherited Create;
  FConstants := AConstants;
end;

function TsgSnapCross.IsCross(const AEnt1, AEnt2: Pointer;
  const ABox: TFRect): Boolean;
var
  vSnapType1, vSnapType2: TsgSnapType;
  vCrossClass: TsgCrossProcs;
  vCross: TsgCrossProc;
  vBox1, vBox2: TFRect;
begin
  Result := False;
  FCrossPoint := cnstFPointZero;
  vBox1 := TsgDXFEntity(AEnt1).Box;
  vBox2 := TsgDXFEntity(AEnt2).Box;
  if (vBox1.Z1 = 0) and (vBox1.Z2 = 0) and (vBox2.Z1 = 0) and (vBox2.Z2 = 0) then
  begin
    vSnapType1 := GetSnapType(AEnt1, FConstants);
    if vSnapType1 <> stUndefined then
    begin
      vSnapType2 := GetSnapType(AEnt2, FConstants);
      vCrossClass := GetCrossClass(vSnapType1, vSnapType2);
      if vCrossClass <> nil then
      begin
        vCross := vCrossClass.Create(AEnt1, AEnt2, ABox, FConstants);
        try
          vCross.CheckType(vSnapType1, vSnapType2);
          if vCross.FindCrossPoint then
          begin
            FCrossPoint := vCross.CrossPoint;
            Result := True;
          end;
        finally
          vCross.Free;
        end;
      end;
    end;
  end;
end;

{ TsgCrossArcs }

function TsgCrossArcs.FindCrossPoint: Boolean;
begin
  if FEntity1 <> FEntity2 then
  begin
    FArc := GetArcR(TsgDXFCircle(FEntity1), Offset1);
    FArc2 := GetArcR(TsgDXFCircle(FEntity2), Offset2);
    Result := CrossArcAndArc(FArc, FArc2);
  end
  else
    Result := False;
end;

{ TsgCrossProc }

procedure TsgCrossProc.CheckType(const AType1, AType2: TsgSnapType);
begin
end;

constructor TsgCrossProc.Create(const AEnt1, AEnt2: Pointer; const ABox: TFRect;
  const AConstants: TsgConstantsCustom);
begin
  inherited Create;
  FConstants := AConstants;
  FBox := ABox;
  FEntity1 := AEnt1;
  FEntity2 := AEnt2;
  FMatrix1 := nil;
  FMatrix2 := nil;
  FDistance := MaxDouble;
end;

class function TsgCrossProc.GetOffsetPoint(const AMatrix: PFMatrix): PFPoint;
begin
  if Assigned(AMatrix) then
    Result := @(AMatrix^.E0)
  else
    Result := nil;
end;

class procedure TsgCrossProc.SetMatrix(var AField: Pointer; const AValue: TFMatrix);
begin
  if IsEqualFPoints(AValue.E0, cnstFPointZero) then
  begin
    if Assigned(AField) then
      DisposeAndNil(AField);
  end
  else
    CreateAndCopyMatrix(AField, @AValue);
end;

class procedure TsgCrossProc.CreateAndCopyMatrix(var AField: Pointer;
  const AValue: PFMatrix);
var
  vMatrix: PFMatrix absolute AField;
begin
  if not Assigned(vMatrix) then
    New(vMatrix);
  vMatrix^ := AValue^;
end;

class function TsgCrossProc.GetMatrix(const AField: PFMatrix): TFMatrix;
begin
  if Assigned(AField) then
    Result := AField^
  else
    Result := cnstIdentityMat;
end;

function TsgCrossProc.GetMatrix1: TFMatrix;
begin
  Result := GetMatrix(FMatrix1);
end;

function TsgCrossProc.GetMatrix2: TFMatrix;
begin
  Result := GetMatrix(FMatrix2);
end;

procedure TsgCrossProc.SetMatrix1(const AValue: TFMatrix);
begin
  SetMatrix(FMatrix1, AValue);
end;

procedure TsgCrossProc.SetMatrix2(const AValue: TFMatrix);
begin
  SetMatrix(FMatrix2, AValue);
end;

class procedure TsgCrossProc.SetMatrixPtr(var AField: Pointer; const AValue: PFMatrix);
begin
  if Assigned(AValue) then
    CreateAndCopyMatrix(AField, AValue)
  else
    if Assigned(AField) then
      DisposeAndNil(AField);
end;

procedure TsgCrossProc.SetMatrixPtr1(const AValue: Pointer);
begin
  SetMatrixPtr(FMatrix1, AValue);
end;

procedure TsgCrossProc.SetMatrixPtr2(const AValue: Pointer);
begin
  SetMatrixPtr(FMatrix2, AValue);
end;

class function TsgCrossProc.CrossSegments(const AL1, AL2: TsgLine;
  var APoint: TFPoint): Boolean;
const
  cnstMax: Double = 1.0 + fAccuracy;
  cnstMin: Double = 0.0 - fAccuracy;
var
  Denom, R, S: Double;
begin
  Result := False;
  Denom := (AL1.Point2.X - AL1.Point1.X) * (AL2.Point2.Y - AL2.Point1.Y) -
    (AL1.Point2.Y - AL1.Point1.Y) * (AL2.Point2.X - AL2.Point1.X);
  if Abs(Denom) > fAccuracy then
  begin
    try
      R := ((AL1.Point1.Y - AL2.Point1.Y)*(AL2.Point2.X - AL2.Point1.X) - (AL1.Point1.X - AL2.Point1.X)*(AL2.Point2.Y - AL2.Point1.Y) ) / Denom;
      S := ((AL1.Point1.Y - AL2.Point1.Y)*(AL1.Point2.X - AL1.Point1.X) - (AL1.Point1.X - AL2.Point1.X)*(AL1.Point2.Y - AL1.Point1.Y) ) / Denom;
    except
      Exit;
    end;
    Result := (R >= cnstMin) and (R <= cnstMax) and (S >= cnstMin) and (S <= cnstMax);
    if Result then
    begin
      APoint.X := AL1.Point1.X + R * (AL1.Point2.X - AL1.Point1.X);
      APoint.Y := AL1.Point1.Y + R * (AL1.Point2.Y - AL1.Point1.Y);
      APoint.Z := AL1.Point1.Z;
    end;
  end;
end;

function TsgCrossProc.CrossArcAndArc(const AArc1,AArc2: TsgArcR): Boolean;
var
  vCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCount := IsCrossArcsAP(AArc1, AArc2, @vCross1, @vCross2);
  Result := SetCrossPoints(vCount, vCross1, vCross2);
end;

function TsgCrossProc.CrossLineAndArc(const ALine: TsgLine;
  const AArc: TsgArcR): Boolean;
var
  vCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCount := IsCrossArcAndSegmentAP(AArc, ALine, @vCross1, @vCross2);
  Result := SetCrossPoints(vCount, vCross1, vCross2);
end;

function TsgCrossProc.CrossLineAndLine(const ALine1,ALine2: TsgLine): Boolean;
var
  vCross: TFPoint;
begin
  Result := False;
  if IsCrossLines(ALine1, ALine2, @vCross) then
    Result := SetCrossPoint(vCross);
end;

function TsgCrossProc.CrossWithArc(const Arc: TsgArcR): Boolean;
begin
  FCrossProcArc.SetArc(Arc);
  Result := FCrossProcArc.FindCrossPoint;
  if Result then
    FCrossPoint := FCrossProcArc.FCrossPoint;
end;

function TsgCrossProc.CrossWithLine(const ALine: TsgLine): Boolean;
begin
  FCrossProcLine.SetLine(ALine);
  Result := FCrossProcLine.FindCrossPoint;
  if Result then
    FCrossPoint := FCrossProcLine.FCrossPoint;
end;

function TsgCrossProc.CrossWithPts(const AP1, AP2: TFPoint): Boolean;
begin
  Result := CrossWithLine(MakeLine(AP1, AP2));
end;

destructor TsgCrossProc.Destroy;
begin
  FreeAndNil(FCrossProcArc);
  FreeAndNil(FCrossProcLine);
  DisposeAndNil(FMatrix1);
  DisposeAndNil(FMatrix2);
  inherited Destroy;
end;

procedure TsgCrossProc.SetArc(const AArc: TsgArcR);
begin
  FArc := AArc;
end;

function TsgCrossProc.SetCrossPoint(const APoint: TFPoint): Boolean;
begin
  Result := IsPointInFRect(FBox, APoint);
  if Result then
    FCrossPoint := APoint;
end;

function TsgCrossProc.SetCrossPoints(const ACount: Integer; const ACross1,
  ACross2: TFPoint): Boolean;
begin
  case ACount of
    1:  Result := SetCrossPoint(ACross1);
    2:
      begin
        Result := SetCrossPoint(ACross1);
        if not Result then
          Result := SetCrossPoint(ACross2);
      end;
  else
    Result := False;
  end;
end;

procedure TsgCrossProc.SetCrossProc(const AClass: TsgCrossProcs;
  var ACrossProc: TsgCrossProc);
begin
  if ACrossProc = nil then
  begin
    ACrossProc := AClass.Create(FEntity2, nil, FBox, FConstants);
    ACrossProc.MatrixPtr1 := MatrixPtr2;
    ACrossProc.MatrixPtr2 := MatrixPtr1;
  end
  else
  begin
    ACrossProc.FEntity1 := FEntity2;
    ACrossProc.MatrixPtr1 := MatrixPtr2;
    ACrossProc.FBox := FBox;
  end;
end;

procedure TsgCrossProc.SetLine(const ALine: TsgLine);
begin
  FLine := ALine;
end;

procedure TsgCrossProc.SwapEntities(AIsSwap: Boolean);
begin
  if AIsSwap then
  begin
    SwapPointers(FEntity1, FEntity2);
    SwapPointers(FMatrix1, FMatrix2);
  end;
end;

function TsgCrossProc.Offset1: PFPoint;
begin
  Result := GetOffsetPoint(FMatrix1);
end;

function TsgCrossProc.Offset2: PFPoint;
begin
  Result := GetOffsetPoint(FMatrix2);
end;

{ TsgCrossArcAndLine }

procedure TsgCrossArcAndLine.CheckType(const AType1, AType2: TsgSnapType);
begin
  SwapEntities(AType1 <> stArc);
end;

function TsgCrossArcAndLine.FindCrossPoint: Boolean;
begin
  if FEntity1 <> nil then
    FArc := GetArcR(TsgDXFCircle(FEntity1), Offset1);
  if FEntity2 <> nil then
    FLine := GetLine(TsgDXFLine(FEntity2), Offset2);
  Result := CrossLineAndArc(FLine, FArc);
end;

{ TsgCrossLines }

function TsgCrossLines.FindCrossPoint: Boolean;
begin
  Result := False;
  if (FEntity1 <> FEntity2) or (FEntity1 = nil) then
  begin
    if FEntity1 <> nil then
      FLine := GetLine(TsgDXFLine(FEntity1), Offset1);
    if FEntity2 <> nil then
      FLine2 := GetLine(TsgDXFLine(FEntity2), Offset2);
    Result := CrossLineAndLine(FLine, FLine2);
  end;
end;

{ TsgCrossSolidAndLine }

function TsgCrossSolidAndLine.FindCrossPoint: Boolean;
var
  I: Integer;
  vCross: TFPoint;
begin
  Result := False;
  if FEntity2 <> nil then
    FLine := GetLine(TsgDXFLine(FEntity2), Offset2);
  for I := 0 to 3 do
  begin
    if IsCrossLines(FLine, GetEdgeOfSolid(I), @vCross) then
    begin
      if SetCrossPoint(vCross) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TsgCrossSolidAndArc }

function TsgCrossSolidAndArc.FindCrossPoint: Boolean;
var
  I, vCrossCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  Result := False;
  if FEntity2 <> nil then
    FArc := GetArcR(TsgDXFCircle(FEntity2), Offset2);
  for I := 0 to 3 do
  begin
    vCrossCount := IsCrossArcAndSegmentAP(FArc, GetEdgeOfSolid(I),
      @vCross1, @vCross2);
    if SetCrossPoints(vCrossCount, vCross1, vCross2) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TsgCrossSolid }

procedure TsgCrossSolid.CheckType(const AType1, AType2: TsgSnapType);
begin
  SwapEntities(AType1 <> stSolid);
end;

function TsgCrossSolid.GetEdgeOfSolid(const EdgeIndex: Integer): TsgLine;
begin
  case EdgeIndex of
    1:
      begin
        Result.Point1 := TsgDXFSolid(FEntity1).Point1;
        Result.Point2 := TsgDXFSolid(FEntity1).Point3;
      end;
    2:
      begin
        Result.Point1 := TsgDXFSolid(FEntity1).Point2;
        Result.Point2 := TsgDXFSolid(FEntity1).Point3;
      end;
    3:
      begin
        Result.Point1 := TsgDXFSolid(FEntity1).Point;
        Result.Point2 := TsgDXFSolid(FEntity1).Point2;
      end;
  else
    Result.Point1 := TsgDXFSolid(FEntity1).Point;
    Result.Point2 := TsgDXFSolid(FEntity1).Point1;
  end;
  Result := GetLine(Result, Offset1);
end;

{ TsgCrossSolids }

function TsgCrossSolids.FindCrossPoint: Boolean;
var
  I: Integer;
  vCrossSolidLine: TsgCrossSolidAndLine;
begin
  Result := False;
  vCrossSolidLine := TsgCrossSolidAndLine.Create(nil, nil, FBox, FConstants);
  try
    vCrossSolidLine.FEntity1 := FEntity2;
    for I := 0 to 3 do
    begin
      vCrossSolidLine.FLine := GetEdgeOfSolid(I);
      if vCrossSolidLine.FindCrossPoint then
      begin
        FCrossPoint := vCrossSolidLine.CrossPoint;
        Result := True;
        Break;
      end;
    end;
  finally
    vCrossSolidLine.Free;
  end;
end;

{ TsgCrossPolyline }

constructor TsgCrossPolyline.Create(const AEnt1, AEnt2: Pointer;
  const ABox: TFRect; const AConstants: TsgConstantsCustom);
begin
  inherited Create(AEnt1, AEnt2, ABox, AConstants);
  FNearests := TList.Create;
  FPointsWithMatrix := nil;
end;

destructor TsgCrossPolyline.Destroy;
begin
  FreeRecordList(FNearests);
  FreeAndNil(FPointsWithMatrix);
  inherited Destroy;
end;

procedure TsgCrossPolyline.CheckType(const AType1, AType2: TsgSnapType);
begin
  SwapEntities(AType1 <> stPolyline);
end;

function TsgCrossPolyline.CrossPolylines(const APolylines: TList): Boolean;
var
  L,M: Integer;

  function Cross(const AI1,AI2: Integer): Boolean;
  var
    I, J, PI, PJ, vMax1,vMax2: Integer;
    vPolyline: TFPointList;
    vLine1, vLine2: TsgLine;
    vCross: TFPoint;
  begin
    Result := False;
    GetSegmentLimits(True, AI1, APolylines.Count, I, vMax1);
    while (not Result) and (I < vMax1) do
    begin
      FPoints := TFPointList(APolylines[I]);
      Inc(I, 2);
      for PI := 0 to FPoints.Count - 2 do
      begin
        vLine1.Point1 := FPoints.List[PI];
        vLine1.Point2 := FPoints.List[PI + 1];
        if FNearests.Count > 0 then
          GetSegmentLimits(True, AI2, APolylines.Count, J, vMax2)
        else
        begin
          J := I;
          vMax2 := APolylines.Count;
        end;
        while (not Result) and (J < vMax2) do
        begin
          vPolyline := TFPointList(APolylines[J]);
          Inc(J, 2);
          for PJ := 0 to vPolyline.Count - 2 do
          begin
            vLine2.Point1 := vPolyline.List[PJ];
            vLine2.Point2 := vPolyline.List[PJ + 1];
            if CrossSegments(vLine1, vLine2, vCross) and SetCrossPoint(vCross) then
            begin
              Result := True;
              Break;
            end;
          end;
        end;
        if Result then
          Break;
      end;
    end;
  end;

begin
  Result := False;
  if FNearests.Count > 0 then
  begin
    for L := 0 to FNearests.Count - 1 do
      for M := 0 to FNearests.Count - 1 do
        if (L <> M) and Cross(L, M) then
        begin
          Result := True;
          Exit;
        end;
  end
  else
    Result := Cross(0, 0);
end;

function TsgCrossPolyline.CrossPolylinesInternal(
  const APolylines: TList): Boolean;
var
  L: Integer;

  function Cross(AIndex: Integer): Boolean;
  var
    I,vMax: Integer;
  begin
    Result := False;
    GetSegmentLimits(False, AIndex, APolylines.Count, I, vMax);
    while I < vMax do
    begin
      if Boolean(APolylines[I]) then
      begin
        FPoints := TFPointList(APolylines[I + 1]);
        Result := FindCrossByEntity;
        if Result then
          Break;
      end;
      Inc(I, 2);
    end;
  end;

begin
  Result := False;
  if FNearests.Count > 0 then
  begin
    for L := 0 to FNearests.Count - 1 do
      if Cross(L) then
      begin
        Result := True;
        Exit;
      end;
  end
  else
    Result := Cross(0);
end;

function TsgCrossPolyline.FindCrossByEntity: Boolean;
var
  I, J, Cnt: Integer;
  vLine1, vLine2: TsgLine;
  vCross: TFPoint;
begin
  Result := False;
  try
    if FClosed then
      FPoints.Add(FPoints.First);
    Cnt := FPoints.Count - 2;
    for I := 0 to FPoints.Count - 3 do
    begin
      vLine1.Point1 := FPoints.List[I];
      vLine1.Point2 := FPoints.List[I + 1];
      for J := I + 1 to Cnt do
      begin
        vLine2.Point1 := FPoints.List[J];
        vLine2.Point2 := FPoints.List[J + 1];
        if CrossSegments(vLine1, vLine2, vCross) and SetCrossPoint(vCross) then
        begin
          Result := True;
          Break;
        end;
      end;
      if Result then
        Break;
    end;
  finally
    if FClosed then
      FPoints.Count := FPoints.Count - 1;
  end;
end;

function TsgCrossPolyline.FindCrossByEntityWithPointsCounts: Boolean;
var
  I: Integer;
  vPolyLines: TList;
  vHasBulges: Boolean;
begin
  Result := False;
  vHasBulges := False;
  for I := 0 to FPointsCounts.Count - 1 do
    if Integer(FPointsCounts[I]) > 1 then
    begin
      vHasBulges := True;
      Break;
    end;
  if vHasBulges then
  begin
    vPolyLines := SpliteOnPolylines;
    try
      FClosed := False;
      FPointsCounts := nil;
      Result := CrossPolylinesInternal(vPolyLines);
      if not Result then
        Result := CrossPolylines(vPolyLines);
    finally
      I := 1;
      while I < vPolyLines.Count do
      begin
        TObject(vPolyLines[I]).Free;
        Inc(I, 2);
      end;
      vPolyLines.Free;
    end;
  end
  else
    if FindCrossByEntity then
      Result := True;
end;

function TsgCrossPolyline.FindNearestParams: Boolean;
var
  I,J,K: Integer;
  vMinDist: Double;
  vNearests: array [1..4] of TsgCrossPolylineSegmentInfo;
  vInfo: PsgCrossPolylineSegmentInfo;

  procedure AddSegmentInfo(const APtIndex,ASegIndex: Integer; const ADist: Double);
  var
    L,M: Integer;
    vInfo: TsgCrossPolylineSegmentInfo;
  begin
    FillChar(vInfo, SizeOf(vInfo), #0);
    vInfo.PointIndex := APtIndex;
    vInfo.SegmentIndex := ASegIndex;
    vInfo.Distance := ADist;
    for M := Low(vNearests) to High(vNearests) do
    begin
      if vNearests[M].SegmentIndex = ASegIndex then
      begin
        if vNearests[M].Distance > ADist then
          vNearests[M] := vInfo;
        Exit;
      end;
      if vNearests[M].Distance > ADist then
      begin
        for L := High(vNearests) - 1 downto M do
          vNearests[L + 1] := vNearests[L];
        vNearests[M] := vInfo;
        Exit;
      end;
    end;
  end;

  function SetNearestParams(const APtIndex,ASegIndex: Integer;
    var AMinDist: Double): Boolean;
  var
    vPt: TFPoint;
    vDist: Double;
  begin
    vPt := FPoints[APtIndex];
    if IsPointInFRect(FBox, vPt) then
      vDist := 0
    else
      vDist := Min(DistanceFPoint(vPt, MiddleFPoint(FBox.TopLeft, FBox.BottomRight)),
        Min(DistanceFPoint(vPt, FBox.TopLeft), DistanceFPoint(vPt, FBox.BottomRight)));
    if (vDist < AMinDist) or (vDist = 0) then
    begin
      AddSegmentInfo(APtIndex, ASegIndex, vDist);
      AMinDist := vDist;
      Result := True;
    end
    else
    begin
      if IsValInParam(vNearests[Low(vNearests)].Distance / vDist, 0.8, 1) or
        (vNearests[High(vNearests)].Distance > vDist) then
        AddSegmentInfo(APtIndex, ASegIndex, vDist);
      Result := False;
    end;
  end;

begin
  Result := False;
  for I := Low(vNearests) to High(vNearests) do
  begin
    FillChar(vNearests[I], SizeOf(vNearests[I]), #0);
    vNearests[I].SegmentIndex := -1;
    vNearests[I].Distance := fMaxDoubleValue;
  end;
  ClearRecordList(FNearests);
  if Assigned(FPointsCounts) and (FPointsCounts.Count > 0) and
    (FPointsCounts.Count <= cnstMaxCountPointsInPolyLineCross) then
  begin
    vMinDist := fMaxDoubleValue;
    J := 0;
    for I := 0 to FPointsCounts.Count - 1 do
    begin
      K := FPointsCounts[I];
      SetNearestParams(J, I, vMinDist);
      if K > 2 then
        SetNearestParams(J + (K shr 1), I, vMinDist);
      SetNearestParams(J + K - 1, I, vMinDist);
      Inc(J, K);
    end;
    for I := Low(vNearests) to High(vNearests) do
      if vNearests[I].SegmentIndex >= 0 then
      begin
        New(vInfo);
        vInfo^ := vNearests[I];
        FNearests.Add(vInfo);
      end;
    Result := True;
  end;
end;

procedure TsgCrossPolyline.RenameNearestSegment(AOldSegment,ANewSegment: Integer;
  AIsStart: Boolean);
var
  I: Integer;
  vInfo: PsgCrossPolylineSegmentInfo;
begin
  for I := 0 to FNearests.Count - 1 do
  begin
    vInfo := FNearests[I];
    if vInfo.SegmentIndex = AOldSegment then
    begin
      if AIsStart then
        vInfo.SplitSegmentStart := ANewSegment
      else
        vInfo.SplitSegmentEnd := ANewSegment;
    end;
  end;
end;

function TsgCrossPolyline.FindCrossPoint: Boolean;
var
  I,L: Integer;
  vInfo: PsgCrossPolylineSegmentInfo;
begin
  Result := False;
  FreeAndNil(FPointsWithMatrix);
  FPointsWithMatrix := CloneAndMovePoints(FEntity1, Offset1);
  FPoints := FPointsWithMatrix;
  FClosed := TsgCADBasePolyline(FEntity1).Closed;
  FPointsCounts := nil;
  if TObject(FEntity1) is TsgDXFLWPolyline then
    FPointsCounts := TsgDXFLWPolyline(FEntity1).PointsCounts;
  if (FPoints.Count > 1) and
    ((FPoints.Count < cnstMaxCountPointsInPolyLineCross) or FindNearestParams) then
  begin
    if FEntity1 = FEntity2 then
    begin
      if IsHasSnap(FEntity1) then
      begin
        if FPointsCounts = nil then
          Result := FindCrossByEntity
        else
          Result := FindCrossByEntityWithPointsCounts;
      end;
    end
    else
    begin
      if FNearests.Count > 0 then
        for L := 0 to FNearests.Count - 1 do
        begin
          vInfo := FNearests[L];
          for I := MaxI(0, vInfo.PointIndex - cnstMaxCrossRadius) to
            MinI(vInfo.PointIndex + cnstMaxCrossRadius, FPoints.Count - 2) do
          begin
            if CrossWithPts(FPoints.List[I], FPoints.List[I + 1]) then
              Result := True;
          end;
        end
      else
        for I := 0 to FPoints.Count - 2 do
        begin
          if CrossWithPts(FPoints.List[I], FPoints.List[I + 1]) then
            Result := True;
        end;
      if (not Result) and FClosed then
      begin
        if CrossWithPts(FPoints.First, FPoints.Last) then
          Result := True;
      end;
    end;
  end;
end;

procedure TsgCrossPolyline.GetSegmentLimits(AOdd: Boolean; const AIndex,AMax: Integer;
  var AStart,AEnd: Integer);
var
  vInfo: PsgCrossPolylineSegmentInfo;
begin
  if (AIndex >= 0) and (AIndex < FNearests.Count) then
  begin
    vInfo := FNearests[AIndex];
    AStart := MaxI(vInfo.SplitSegmentStart, 0);
    AEnd := MinI(vInfo.SplitSegmentEnd + 1, AMax);
  end
  else
  begin
    AStart := 1;
    AEnd := AMax;
  end;
  if not AOdd then
  begin
    Dec(AStart);
    Dec(AEnd);
  end;
end;

function TsgCrossPolyline.SpliteOnPolylines: TList;
var
  I, J, P, vPointsCount: Integer;
  vPolyline, vPolylineOfBulge: TFPointList;
  vInfo: PsgCrossPolylineSegmentInfo;
begin
  Result := TList.Create;
  try
    if FClosed then
    begin
      FPoints.Add(FPoints.First);
      FPointsCounts.Add(1);
    end;
    vPolyline := AddPolylineInList(Result, FPoints.Count, True);
    P := 0;
    for I := 0 to FPointsCounts.Count - 1 do
    begin
      RenameNearestSegment(I, Result.Count - 1, True);
      vPointsCount := Integer(FPointsCounts[I]);
      if vPointsCount = 1 then
        AddLastFPointUnique(vPolyline, FPoints[P])
      else
      begin
        if vPolyline.Count > 0 then
        begin
          AddLastFPointUnique(vPolyline, FPoints[P]);
          vPolylineOfBulge := AddPolylineInList(Result, vPointsCount, False);
        end
        else
        begin
          Result[Result.Count - 2] := Pointer(False);
          vPolylineOfBulge := vPolyline;
        end;
        for J := P to P + vPointsCount - 1 do
          AddLastFPointUnique(vPolylineOfBulge, FPoints[J]);
        vPolyline := AddPolylineInList(Result, FPoints.Count - P - vPointsCount, True);
        AddLastFPointUnique(vPolyline, vPolylineOfBulge.Last);
      end;
      RenameNearestSegment(I, Result.Count - 1, False);
      Inc(P, vPointsCount);
    end;
    I := 1;
    while I < Result.Count do
    begin
      if TFPointList(Result[I]).Count < 2 then
      begin
        for J := 0 to FNearests.Count - 1 do
        begin
          vInfo := FNearests[J];
          if vInfo^.SplitSegmentStart >= I then
          begin
            Dec(vInfo^.SplitSegmentStart, 2);
            Dec(vInfo^.SplitSegmentEnd, 2);
          end;
        end;
        TFPointList(Result[I]).Free;
        Result.Delete(I);
        Result.Delete(I - 1);
      end
      else
        Inc(I, 2);
    end;
  finally
    if FClosed then
    begin
      FPoints.Count := FPoints.Count - 1;
      FPointsCounts.Count := FPointsCounts.Count - 1;
    end;
  end;
end;

{ TsgCrossPolyPolyline }

procedure TsgCrossPolyPolyline.CheckType(const AType1, AType2: TsgSnapType);
begin
  SwapEntities(AType1 <> stPolyPolyline);
end;

function TsgCrossPolyPolyline.FindCrossPoint: Boolean;
{$IFDEF SG_BTI}
var
  I: Integer;
  vPPPoly: TsgCADPolyPolyline2D;
  vIsFound: Boolean;
  vNearestPoint,vCenterOfBox: TFPoint;
  vDistance,vMinDistance: Double;
{$ENDIF}
begin
  Result := False;
{$IFDEF SG_BTI}
  vMinDistance := 0;
  vPPPoly := TsgCADPolyPolyline2D(FEntity1);
  vCenterOfBox := MiddleFPoint(FBox.TopLeft, FBox.BottomRight);
  for I := 0 to vPPPoly.CurvesCount - 1 do
  begin
    if vPPPoly.CurveVisible[I] then
    begin
      if vPPPoly.IsCurveArc(I) then
        vIsFound := CrossWithArc(GetArcR(vPPPoly.Arcs[I], Offset1))
      else
        vIsFound := CrossWithLine(GetLine(vPPPoly.Lines[I], Offset1));
      if vIsFound then
        if Result then
        begin
          vDistance := DistanceFPoint(vCenterOfBox, FCrossPoint);
          if vDistance < vMinDistance then
          begin
            vMinDistance := vDistance;
            vNearestPoint := FCrossPoint;
          end;
        end
        else
        begin
          Result := True;
          vNearestPoint := FCrossPoint;
          vMinDistance := DistanceFPoint(vCenterOfBox, vNearestPoint);
        end;
    end;
  end;
  if Result then
    FCrossPoint := vNearestPoint;
{$ENDIF}
end;

{ TsgCrossPolyPolylineAndArc }

function TsgCrossPolyPolylineAndArc.CrossWithArc(const Arc: TsgArcR): Boolean;
var
  vCrossCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCrossCount := IsCrossArcsAP(FArc, Arc, @vCross1, @vCross2);
  Result := SetCrossPoints(vCrossCount, vCross1, vCross2);
end;

function TsgCrossPolyPolylineAndArc.CrossWithLine(const ALine: TsgLine): Boolean;
var
  vCrossCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCrossCount := IsCrossArcAndSegmentAP(FArc, ALine, @vCross1, @vCross2);
  Result := SetCrossPoints(vCrossCount, vCross1, vCross2);
end;

function TsgCrossPolyPolylineAndArc.FindCrossPoint: Boolean;
begin
  if FEntity2 <> nil then
    FArc := GetArcR(TsgDXFCircle(FEntity2), Offset2);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolylineAndArc }

function TsgCrossPolylineAndArc.CrossWithLine(const ALine: TsgLine): Boolean;
var
  vCrossCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCrossCount := IsCrossArcAndSegmentAP(FArc, ALine, @vCross1, @vCross2);
  Result := SetCrossPoints(vCrossCount, vCross1, vCross2);
end;

function TsgCrossPolylineAndArc.FindCrossPoint: Boolean;
begin
  if FEntity2 <> nil then
    FArc := GetArcR(TsgDXFCircle(FEntity2), Offset2);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolylineAndLine }

function TsgCrossPolylineAndLine.CrossWithLine(const ALine: TsgLine): Boolean;
var
  vCross: TFPoint;
begin
  Result := False;
  if CrossSegments(FLine, ALine, vCross) then
    Result := SetCrossPoint(vCross);
end;

function TsgCrossPolylineAndLine.FindCrossPoint: Boolean;
begin
  if FEntity2 <> nil then
    FLine := GetLine(TsgDXFLine(FEntity2), Offset2);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolylineAndSolid }

function TsgCrossPolylineAndSolid.FindCrossPoint: Boolean;
begin
  SetCrossProc(TsgCrossSolidAndLine, FCrossProcLine);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolylines }

function TsgCrossPolylines.FindCrossPoint: Boolean;
begin
  SetCrossProc(TsgCrossPolylineAndLine, FCrossProcLine);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolyPolylineAndLine }

function TsgCrossPolyPolylineAndLine.CrossWithArc(const Arc: TsgArcR): Boolean;
var
  vCrossCount: Integer;
  vCross1, vCross2: TFPoint;
begin
  vCrossCount := IsCrossArcAndSegmentAP(Arc, FLine, @vCross1, @vCross2);
  Result := SetCrossPoints(vCrossCount, vCross1, vCross2);
end;

function TsgCrossPolyPolylineAndLine.CrossWithLine(const ALine: TsgLine): Boolean;
var
  vCross: TFPoint;
begin
  Result := False;
  if CrossSegments(FLine, ALine, vCross) then
    Result := SetCrossPoint(vCross);
end;

function TsgCrossPolyPolylineAndLine.FindCrossPoint: Boolean;
begin
  if FEntity2 <> nil then
    FLine := GetLine(TsgDXFLine(FEntity2), Offset2);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolyPolylines }

function TsgCrossPolyPolylines.FindCrossPoint: Boolean;
begin
  SetCrossProc(TsgCrossPolyPolylineAndArc, FCrossProcArc);
  SetCrossProc(TsgCrossPolyPolylineAndLine, FCrossProcLine);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolyPolylineAndSolid }

function TsgCrossPolyPolylineAndSolid.FindCrossPoint: Boolean;
begin
  SetCrossProc(TsgCrossSolidAndArc, FCrossProcArc);
  SetCrossProc(TsgCrossSolidAndLine, FCrossProcLine);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossPolyPolylineAndPolyline }

function TsgCrossPolyPolylineAndPolyline.FindCrossPoint: Boolean;
begin
  SetCrossProc(TsgCrossPolylineAndArc, FCrossProcArc);
  SetCrossProc(TsgCrossPolylineAndLine, FCrossProcLine);
  Result := inherited FindCrossPoint;
end;

{ TsgCrossInsert }

constructor TsgCrossInsert.Create(const AEnt1, AEnt2: Pointer; const ABox: TFRect;
  const AConstants: TsgConstantsCustom);
begin
  inherited Create(AEnt1, AEnt2, ABox, AConstants);
  FProxyMatrix := nil;
end;

destructor TsgCrossInsert.Destroy;
begin
  DisposeAndNil(FProxyMatrix);
  inherited Destroy;
end;

procedure TsgCrossInsert.CheckType(const AType1, AType2: TsgSnapType);
begin
  if AType1 <> stObjects then
  begin
    SwapEntities(True);
    SetMatrixPtr(FProxyMatrix, nil);
  end;
end;

function TsgCrossInsert.FindCrossPoint: Boolean;
var
  I: Integer;
  vEntity: Pointer;
  vSnapType: TsgSnapType;
  vClass: TsgCrossProcs;
  vCrossProc: TsgCrossProc;
  vEntBox,vEnt2Box: TFRect;

{$IFDEF SG_BTI}
  function GetLine(AInsert: TsgDXFInsert): TsgLine;
  var
    vPts: TsgPoints4;
    vInsert: TsgDXFInsertAccess absolute AInsert;
  begin
    vInsert.GetPts(vPts);
    Result := MakeLine(MiddleFPoint(vPts[0], vPts[3]),
      MiddleFPoint(vPts[1], vPts[2]));
  end;

  function IsBoxInCross(const ABox1,ABox2: TFRect): Boolean;
  var
    vIntRect: TFRect;
    vBox,vIntRect2D: TF2DRect;
  begin
    Result := False;
    if IntersectRectF2D(MakeF2dRectFromFRect(ABox1), MakeF2dRectFromFRect(ABox2), nil) then
    begin
      vIntRect := cnstBadRect;
      UnionFRect(vIntRect, ABox1);
      UnionFRect(vIntRect, ABox2);
      vBox := MakeF2dRectFromFRect(FBox);
      vIntRect2D := MakeF2dRectFromFRect(vIntRect);
      Result := IntersectRectF2D(vIntRect2D, vBox, nil) or IsRectInRectF2D(vBox, vIntRect2D);
    end;
  end;

  function CrossCenters: Boolean;
  var
    vInsert1: TsgDXFInsertAccess;
    vInsert2: TsgDXFInsertAccess;
  begin
    Result := False;
    if IsBTI(FEntity1) and IsBTI(FEntity2) and (FEntity1 <> FEntity2) then
    begin
      vInsert1 := TsgDXFInsertAccess(FEntity1);
      vInsert2 := TsgDXFInsertAccess(FEntity2);
      if IsBoxInCross(vInsert1.Box, vInsert2.Box) then
      begin
        if vInsert1.IsRealCircular and vInsert2.IsRealCircular then
          Result := CrossArcAndArc(vInsert1.GetArcOfCenter, vInsert2.GetArcOfCenter)
        else
          if vInsert1.IsRealCircular then
            Result := CrossLineAndArc(GetLine(vInsert2), vInsert1.GetArcOfCenter)
          else
            if vInsert2.IsRealCircular then
              Result := CrossLineAndArc(GetLine(vInsert1), vInsert2.GetArcOfCenter)
            else
              Result := CrossLineAndLine(GetLine(vInsert1), GetLine(vInsert2));
      end;
    end;
  end;
{$ENDIF}

  function GetEntBox(AEntity: Pointer; AMatrix: PFMatrix): TFRect;
  var
    vEntity: TsgDXFEntity absolute AEntity;
  begin
    Result := vEntity.Box;
    if Assigned(AMatrix) then
    begin
      Result.TopLeft := AddFPoint(Result.TopLeft, AMatrix^.E0);
      Result.BottomRight := AddFPoint(Result.BottomRight, AMatrix^.E0);
    end;
  end;

begin
{$IFDEF SG_BTI}
  Result := CrossCenters;
  if Result then
    Exit;
{$ELSE}
  Result := False;
{$ENDIF}
  FObjects := TsgDXFInsert(FEntity1).Block;
  FSnapType2 := GetSnapType(FEntity2, FConstants);
  for I := 0 to FObjects.Count - 1 do
  begin
    vEntity := FObjects[I];
    vSnapType := GetSnapType(vEntity, FConstants);
    vEntBox := GetEntBox(vEntity, OffsetMatrixPtr);
    vEnt2Box := GetEntBox(FEntity2, MatrixPtr2);
    if (IntersectFRect2D(FBox, vEntBox) >= 0) and
      (IntersectFRect2D(FBox, vEnt2Box) >= 0) and
      (IntersectFRect2D(vEntBox, vEnt2Box) >= 0) then
    begin
      vClass := GetCrossClass(FSnapType2, vSnapType);
      if vClass <> nil then
      begin
        vCrossProc := vClass.Create(FEntity2, vEntity, FBox, FConstants);
        vCrossProc.MatrixPtr1 := MatrixPtr2;
        vCrossProc.MatrixPtr2 := OffsetMatrixPtr;
        try
          vCrossProc.CheckType(FSnapType2, vSnapType);
          if vCrossProc.FindCrossPoint then
          begin
            FCrossPoint := vCrossProc.FCrossPoint;
            Result := True;
            Break;
          end;
        finally
          vCrossProc.Free;
        end;
      end;
    end;
  end;
end;

function TsgCrossInsert.OffsetMatrixPtr: PFMatrix;
var
  vMatrixPtr: PFMatrix;
  vNewMatrix: TFMatrix;
begin
  if not Assigned(FProxyMatrix) then
  begin
    vMatrixPtr := MatrixPtr1;
    vNewMatrix := TsgDXFInsert(FEntity1).GetMatrix;
    if Assigned(vMatrixPtr) then
      vNewMatrix := FMatXMat(vMatrixPtr^, vNewMatrix);
    SetMatrixPtr(FProxyMatrix, @vNewMatrix);
  end;
  Result := FProxyMatrix;
end;

{ TsgCustomControlToolProps }

function TsgCustomControlToolProps.AddClient(AClient: TObject): Integer;
begin
  Result := -1;
end;

function TsgCustomControlToolProps.ClientToWorld(X, Y: Double;
  AUCSPoint: PFPoint): TFPoint;
begin
  Result := MakeFPoint(X, Y, 0);
  if Assigned(AUCSPoint) then
    AUCSPoint^ := Result;
end;

function TsgCustomControlToolProps.GetCanvas: TCanvas;
begin
  Result := nil;
end;

function TsgCustomControlToolProps.GetGraphic: TGraphic;
begin
  Result := nil;
end;

function TsgCustomControlToolProps.GetViewRectMode: Boolean;
begin
  Result := False;
end;

procedure TsgCustomControlToolProps.Invalidate;
begin
end;

function TsgCustomControlToolProps.IsClipRectActive: Boolean;
begin
  Result := False;
end;

function TsgCustomControlToolProps.RemoveClient(AClient: TObject): Integer;
begin
  Result := -1;
end;

function TsgCustomControlToolProps.ScreenToClient(P: TPoint): TPoint;
begin
  Result := P;
end;

function TsgCustomControlToolProps.ClientToScreen(P: TPoint): TPoint;
begin
  Result := P;
end;

function TsgCustomControlToolProps.WorldToClient(const APoint: TFPoint): TPoint;
begin
  if GetGraphic is TsgCADImage then
    Result := TsgCADImage(GetGraphic).GetPoint(APoint)
  else
  begin
    Result.X := Round(APoint.X);
    Result.Y := Round(APoint.Y);
  end;
end;

initialization
  MakeHelix(HelixRadius, Helix);
  SnapCross := nil;

finalization
  FreeAndNil(SnapCross);

end.
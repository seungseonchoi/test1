{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                TsgDrawingNavigator class                   }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDrawingNavigator;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LMessages, LCLIntf, LCLType, Types, IntfGraphics,
{$ENDIF}
  Messages, Classes, Forms, Controls, Graphics, StdCtrls, Menus, sgFunction,
  SysUtils, sgConsts, sgTools, sgOrbit3D, sgRectangle, CADImage, DXFConv,
  sgDrawRect
{$IFDEF SGDEL_XE2}
  , System.Types
{$ENDIF}
   ;

const
  iMinPicSize = 2;
  iScrollSmallChange = 4;
  mmPerInch = 25.4;
  iMaxNonWinNTDim = $7FFF;
  iMaxWinNTDim = $20DFFFFF; // empirical

  cnstCursorHand = 5;
  cnstCursorCross = 6;

{$IFNDEF SGDEL_4}  // for Delphi 3

  CM_MOUSEWHEEL = CM_BASE + 67;

type

  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = procedure(Sender: TObject; Shift: TShiftState;
    MousePos: TPoint; var Handled: Boolean) of object;

  TWMMouseWheel = packed record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TCMMouseWheel = record
    Msg: Cardinal;
    ShiftState: TShiftState;
    Unused: Byte;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

  TsgCustomControl = class(TCustomControl)
  private
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnResize: TNotifyEvent;
    FWheelAccumulator: Integer;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelDown(Shift: TShiftState;
      MousePos: TPoint): Boolean; dynamic;
    function DoMouseWheelUp(Shift: TShiftState;
      MousePos: TPoint): Boolean; dynamic;
    procedure Resize; dynamic;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel
      write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown
      write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp
      write FOnMouseWheelUp;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property WheelAccumulator: Integer read FWheelAccumulator
      write FWheelAccumulator;
  public
    procedure MouseWheelHandler(var Message: TMessage); dynamic;
  end;
{$ELSE}
type
  TsgCustomControl = class(TCustomControl);
{$ENDIF}

type
  TsgDrawingNavigatorStates = (dnsLoading, dnsZooming, dnsMoving,
    dnsDrawing, dnsSclollChanging, dnsSetView, dnsWheel, dnsAfterRotate,
    dnsPictureChanging);
  TsgDrawingNavigatorStateSet = set of TsgDrawingNavigatorStates;

  TGetImageExtents = function(var ALeft, ATop, AZ1, ARight, ABottom,
    AZ2: Double): Boolean of object;

  TsgDrawingNavigator = class(TsgCustomControl)
  private
    FActualGraphic: TGraphic;
    FAppActive: Boolean;
    FAutoFocus: Boolean;
    FAutoInsert: Boolean;
    FBufferWidth: Integer;
    FBufferHeight: Integer;
    FBuffer: Pointer;
    FBufferSaved: Boolean;
    FCenterAlignment: Boolean;
    FClients: TList;
    FClipRectPopupMenu: TPopupMenu;
    FDrawOnEveryMouseWheel: Boolean;
    FHorzScrollBar: TScrollBar;
    FHorzScrollBarVisibility: Boolean;
    FImageDragMode: Boolean;
    FIterateParam: TsgCADIterate;
    FLastViewName: string;
    FLockChangeVisibleArea: Boolean;
    FLoadedFromConverter: Boolean;
    FMaxDim: Integer;
    FMouseDownPt: TPoint;
    FMouseDragging: Boolean;
    FMouseMovePrevPoint: TPoint;
    FMouseMoveZoomingProcess: Boolean;
    FMouseScaling: Boolean;
    FMoveByMouse: Boolean;
    FNeedBroadcastPaintEvent: Boolean;
    FOnChangeScale: TNotifyEvent;
    FOnGetImageExtents: TGetImageExtents;
    FOnGetSysMenuIcons: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnPictureChange: TNotifyEvent;
    FOnPictureMove: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FOrbit3D: TsgOrbit3D;
    FPicture: TPicture;
    FPictureChanged: Boolean;
    FPictureRectChanged: Boolean;
    FRectangle: TsgRectangle;
    FRectZooming: Boolean;
    FRectZoomingProcess: Boolean;
    FRotOffs: TFPoint;
    FScalingOnMouseWheel: Boolean;
    FSnapControl: TSnapControl;
    FState: TsgDrawingNavigatorStateSet;
    FStretch: Boolean;
    FSysMenuIcons: TsgSysMenuIcons;
    FTmpGraphic: TGraphic;
    FTmpLayout: TObject;
    FTool: TsgControlTool;
    FUpdateCount: Integer;
    FVertScrollBar: TScrollBar;
    FVertScrollBarVisibility: Boolean;
    FViewRectMode: Boolean;
    FVPortOpened: Boolean;
    FWidthInPixels: Double;
    FZoomChangeByMouseMove: Single;
    FZoomInChange: Single;
    FZoomingRect: TRect;
    FZoomOutChange: Single;
    FBlockFocusUpdate: Boolean;
    FOnGetActualGraphic: TNotifyEvent;
    FBroadcasting: Cardinal;
    FControlToolProps: TsgCustomControlToolProps;
    FConstants: TsgConstantsCustom;
    FDrawRect: TsgDrawRect;
    FMeasureProps: TsgMeasureUnitsProps;
    procedure BroadcastClients(var Message);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    function CreateHorScroll: Boolean;
    function CreateVertScroll: Boolean;
    procedure CreateSnap;
    procedure DoChangeScrolls;
    procedure DrawScrollBarsJoint(const ADC: HDC);
    procedure DrawZoomingRect(ARect: TRect);
    function GetClipRectangle: Boolean;
    function GetClipRectCoords: TRect;
    function GetImageExtents: TFRect;
    function GetMouseMovePrevPoint: TPoint;
    function GetOrbitAutoTarget: Boolean;
    function GetPictureHeight: Integer;
    function GetPictureRect: TRect;
    function GetPictureRect2D: TF2DRect;
    function GetPictureRectCenter: TPoint;
    function GetPictureRectCenter2D: TF2DPoint;
    function GetPictureRectHeight: Double;
    function GetPictureRectWidth: Double;
    function GetPictureWidth: Integer;
    procedure GetPointToImageSizeRatio(ACoordX, ACoordY: Double;
      var AXScaled, AYScaled: Extended);
    function GetRectangle: TsgRectangle;
    function GetScale: Double;
    function GetSysMenuIconsVisible: Boolean;
    procedure HScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    function GetLoadedFromConverter: Boolean;
    procedure RectZoomingOperation(ACoordX, ACoordY: Integer; AErase: Boolean);
    procedure SetAutoFocus(const AValue: Boolean);
    procedure SetClipRectangle(const AValue: Boolean);
    procedure SetClipRectCoords(const AValue: TRect);
    procedure SetClipRectPopupMenu(const AValue: TPopupMenu);
    procedure SetDrawOnEveryMouseWheel(const AValue: Boolean);
    procedure SetHorzScrollBarVisibility(const AValue: Boolean);
    procedure SetLinearDimensionsScale(const AFactor: Double);
    procedure SetOnGetImageExtents(const AValue: TGetImageExtents);
    procedure SetOrbitAutoTarget(const AValue: Boolean);
    procedure SetPicture(const AValue: TPicture);
    procedure SetPictureChanged(const AValue: Boolean);
    procedure SetPictureRectChanged(const AValue: Boolean);
    procedure SetRectZooming(const AValue: Boolean);
    procedure SetScale(const AValue: Double);
    procedure SetScrollsPosition;
    procedure SetScrollsRange;
    procedure SetStretch(const AValue: Boolean);
    procedure SetVertScrollBarVisibility(const AValue: Boolean);
    procedure SetVisibleArea;
    procedure SetSysMenuIconsVisible(const AValue: Boolean);
    procedure StartMoving(const ACoordX, ACoordY: Integer; Button: TMouseButton);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMCancelMode(var Message: TMessage); message CM_CANCELMODE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMZoomExtents(var Message: TMessage); message WM_ZOOM_EXTENTS;
    procedure VScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    function GetOnEnterClipRect: TNotifyEvent;
    procedure SetOnEnterClipRect(const Value: TNotifyEvent);
    procedure SetBlockFocusUpdate(const Value: Boolean);
    procedure StopMoveByMouse(AForceUpdate: Boolean = False);
    procedure SetImageDragMode(const Value: Boolean);
    procedure SetCursor(Value: TCursor);
    function GetCursor: TCursor;
    function GetLinearDimensionsMode: TsgDistUnitsMode;
    function GetLinearDimensionsScaleFactor: Double;
    procedure SetLinearDimensionsMode(const Value: TsgDistUnitsMode);
    procedure SetLastViewName(const Value: string);
    procedure SetConstants(const Value: TsgConstantsCustom);
    function UpdateCursor(ADefault: TCursor): TCursor;
    procedure ClearBuffer;
    procedure DrawRectChanging(Sender: TObject);
    procedure DrawRectChanged(Sender: TObject);
    procedure UpdateDrawRect(const ABox: TFRect);
    procedure SetMeasureProps(const Value: TsgMeasureUnitsProps);
    function GetMeasurePrecisionFactor: Integer;
    function GetMeasureDisplayedUnits: TsgInsUnits;
    function GetMeasureInitialUnits: TsgInsUnits;
    function GetMeasureScaleFactor: Double;
  protected
    FMouseWheel: Boolean;
    FLockInternalMouseEventsHandler: Boolean;
    procedure AfterRotate(Sender: TObject); virtual;
    function ApplicationHook(var Message: TMessage): Boolean;
    procedure BeforeRotate(Sender: TObject); virtual;
    function ClientToWorld(X, Y: Double; AUCSPoint: PFPoint = nil): TFPoint; virtual;
    function ConvertUnits(APoint: TFPoint; var AUnits: string): TFPoint;
    function CoordinateConvertion(ACoordX, ACoordY: Integer;
      var APointInUCS: TFPoint): TFPoint; virtual;
    procedure CreateHandle; override;
    procedure CreateOrbit3D(AClass: TClass); virtual;
    function CreateSnapShotBitmap: Graphics.TBitmap; virtual;
    procedure DoBroadcastPaintEvent(ADC: HDC); virtual;
    procedure DoChangeScale; dynamic;
    procedure DoGetActualGraphic; dynamic;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;
      MousePos: TPoint): Boolean; override;
    procedure DoOnColorChanged(const AColor: TColor); virtual;
    procedure DoOnCursorChanged; virtual;
    procedure DoPictureChange(Sender: TObject); virtual;
    procedure DoPictureMove; dynamic;
    procedure DoRepaintControl(const ARects: array of TRect);
    function FinishEntity(Entity: TsgDXFEntity): Integer; virtual;
    function GetBox: TFRect; virtual;
    function GetBoxHandler(Sender: TObject; var ABox: TFRect): Boolean;
    function GetBoxOffset: TFPoint; virtual;
    function GetZoomInEnable: Boolean; virtual;
    function GetZoomOutEnable: Boolean; virtual;
    function GetActualGraphic: TGraphic; virtual;
    function GetEntity(Entity: TsgDXFEntity): Integer; virtual;
    function GetOrbit3DClass: TClass; virtual;
    function GetPoint(APoint: TFPoint): TPoint;
    function GetSizesAsString: string; virtual; //gets string with information of drawing measurement
    function GetHasTriangledMesh: Boolean; virtual;
    procedure GetView(var AMatrix: TFMatrix); virtual;
    function GetUnionPictureRectWithClientRect2D: TF2DRect;
    function ViewRectHeight: Integer;
    function ViewRectWidth: Integer;
    function InitializeIterateParams: PsgCADIterate; virtual;
    function IsClipRectActive: Boolean;
    function IsMouseInSysButtons: Boolean;
    procedure LockChangeVisibleArea;
    procedure LoadView(const AMatrix: TFMatrix;
      const APostMessage: Boolean = True); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure OrbitStateChanged(Sender: TObject);
    procedure OrbitMouseCaptureChanged(Sender: TObject);
    procedure OrbitVisibleChanging(Sender: TObject); virtual;
    procedure OrbitChange(Sender: TObject); virtual;
    procedure OrbitRotate(Sender: TObject); virtual;

    procedure ObtainFocus(var Message: TMessage); message WM_MOUSEMOVE;
    function OpenVPort: Boolean; virtual;
    procedure Paint; override;
    function PerformBroadcastEvents(const AMsg: Cardinal;
      const AWParam: WPARAM; const  ALParam: LPARAM): LRESULT;
    procedure Progress(Sender: TObject; Stage: TProgressStage; PercentDone:
      Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string{$IFDEF SGFPC}; var Continue: Boolean{$ENDIF}); dynamic;
    procedure Resize; override;
    procedure SetActualGraphic(AGraphic: TGraphic); virtual;
    procedure SaveCurrentView(ALayout: TObject = nil); virtual;
    procedure SetCommonUnitsWidth(ADC: HDC);
    procedure SetParent(AParent: TWinControl); override;
    function ShowRectConvertUnits(ARect: TFRect;
      const AConvertUnits: Boolean): Boolean; virtual;
    procedure StopMouseMoveOperationsHandler(var Message: TMessage); virtual;
    function ViewRect: TRect;
    procedure UnlockChangeVisibleArea;
{$IFDEF SGDEL_5}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
{$ENDIF}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function ZoomRect(ARect: TRect): Boolean;
    function ZoomRectEx(ARect: TRect): Boolean;
    function ZoomRectEx2D(const ARect: TF2DRect): Boolean; virtual;
    property AppActive: Boolean read FAppActive;
    property ActualGraphicRef: TGraphic read FActualGraphic;
    property OnGetActualGraphic: TNotifyEvent read FOnGetActualGraphic write FOnGetActualGraphic;
    property IterateParam: TsgCADIterate read FIterateParam;
    property MaxDim: Integer read FMaxDim write FMaxDim;
    property MouseMovePrevPoint: TPoint read GetMouseMovePrevPoint;
    property MoveByMouse: Boolean read FMoveByMouse;
    property PictureChanged: Boolean read FPictureChanged
      write SetPictureChanged;
    property PictureRectChanged: Boolean read FPictureRectChanged
      write SetPictureRectChanged;
    property SysMenuIcons: TsgSysMenuIcons read FSysMenuIcons;
    property Rectangle: TsgRectangle read GetRectangle;
    property State: TsgDrawingNavigatorStateSet read FState;
    property ControlTool: TsgControlTool read FTool;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property OnGetSysMenuIcons: TNotifyEvent read FOnGetSysMenuIcons write FOnGetSysMenuIcons;
    property Constants: TsgConstantsCustom read FConstants write SetConstants;
    property RectZoomingProcess: Boolean read FRectZoomingProcess;
    property Clients: TList read FClients;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddClient(AClient: TObject): Integer;
    function AlterScale(AFactor: Double; AIsAbsolute: Boolean;
      APosition: TPoint): Boolean; virtual;
    function BeginUpdate: Integer; virtual;
    procedure CallPreviousView; virtual;
    procedure ClearCurrentViews; virtual;
    function Center: TPoint;
    procedure Clear; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function Empty: Boolean; virtual;
    function EndUpdate: Integer; virtual;
    procedure FitToSize; virtual;
    function GetDrawingCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; virtual;
    function GetDrawingInternalCoords(ACoordX, ACoordY: Integer): TFPoint; virtual;
    function GetDrawingInternalCoordsWithoutSnap(ACoordX,
      ACoordY: Integer): TFPoint;
    function GetDrawingUCSCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; virtual;
    function GetRealPoint(ACoordX, ACoordY: Double;
      var APointInUCS: TFPoint): TFPoint; virtual;
    function HasPreviousView: Boolean;
    procedure ImgLock;
    procedure ImgUnLock;
    function IsImgLocked: Boolean;
    procedure Invalidate; override;
    function IsCanBeEdited: Boolean; virtual;
    function IsCanShowCoords: Boolean; virtual;
    function IsCanShowSizes: Boolean; virtual; //if Result is not see SizesAsString
    function IsUseGraphic: Boolean; virtual;
    procedure LoadFromConverter(const AConverter: TsgDXFConverter;
      const ACADImage: TsgCADImage); virtual;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure MovePictureRect(const ADeltaX, ADeltaY: Integer); virtual;
    function MovePictureRect2D(const ADeltaX, ADeltaY: Double): Boolean; virtual;
    function RemoveClient(AClient: TObject): Integer;
    procedure Repaint; override;
    procedure RotToView(const A3DView: TsgDXFViewDirection); virtual;
    function Rotate(const APitch, ATurn, ARoll: Extended): Boolean; overload; virtual;
    function Rotate(Axis: TsgAxes; Angle: Extended): Boolean; overload; virtual;
    function ShowPoint(const APoint: TFPoint; AScale: Double): Boolean;
    function ShowRect(ARect: TFRect): Boolean;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateSnap;
    property  AutoInsert: Boolean read FAutoInsert write FAutoInsert;
    property ActualGraphic: TGraphic read GetActualGraphic
      write SetActualGraphic;
    property BlockFocusUpdate: Boolean read FBlockFocusUpdate write SetBlockFocusUpdate;
    property OrbitAutoTarget: Boolean read GetOrbitAutoTarget write SetOrbitAutoTarget;
    property Canvas;
    property ClipRectangle: Boolean read GetClipRectangle
      write SetClipRectangle;
    property ClipRectCoords: TRect read GetClipRectCoords
      write SetClipRectCoords;
    property ClipRectPopupMenu: TPopupMenu read FClipRectPopupMenu
      write SetClipRectPopupMenu;
    property Cursor: TCursor read GetCursor write SetCursor;
    property DrawOnEveryMouseWheel: Boolean read FDrawOnEveryMouseWheel
      write SetDrawOnEveryMouseWheel;
    property DrawRect: TsgDrawRect read FDrawRect;
    property HasTriangledMesh: Boolean read GetHasTriangledMesh;
    property HorzScrollBar: TScrollBar read FHorzScrollBar;
    property HorzScrollBarVisibility: Boolean read FHorzScrollBarVisibility
      write SetHorzScrollBarVisibility;
    property LastViewName: string read FLastViewName write SetLastViewName;
    property LinearDimensionsMode: TsgDistUnitsMode read GetLinearDimensionsMode
      write SetLinearDimensionsMode;
    property LinearDimensionsScale: Double read GetLinearDimensionsScaleFactor
      write SetLinearDimensionsScale;
    property LoadedFromConverter: Boolean read GetLoadedFromConverter;
    property MeasureProps: TsgMeasureUnitsProps read FMeasureProps write
      SetMeasureProps;
    property MeasurePrecisionFactor: Integer read GetMeasurePrecisionFactor;
    property MeasureScaleFactor: Double read GetMeasureScaleFactor;
    property MeasureDisplayedUnits: TsgInsUnits read GetMeasureDisplayedUnits;
    property MeasureInitialUnits: TsgInsUnits read GetMeasureInitialUnits;
    property ImageDragMode: Boolean read FImageDragMode
      write SetImageDragMode;
    property OnEnterClipRect: TNotifyEvent read GetOnEnterClipRect
      write SetOnEnterClipRect;
    property Orbit3D: TsgOrbit3D read FOrbit3D;
    property PictureHeight: Integer read GetPictureHeight;
    property PictureRect: TRect read GetPictureRect;
    property PictureRect2D: TF2DRect read GetPictureRect2D;
    property PictureRectCenter: TPoint read GetPictureRectCenter;
    property PictureRectCenter2D: TF2DPoint read GetPictureRectCenter2D;
    property PictureRectHeight: Double read GetPictureRectHeight;
    property PictureRectWidth: Double read GetPictureRectWidth;
    property PictureWidth: Integer read GetPictureWidth;
    property ScalingOnMouseWheel: Boolean read FScalingOnMouseWheel
      write FScalingOnMouseWheel;
    property SnapControl: TSnapControl read FSnapControl;
    property SizesAsString: string read GetSizesAsString;
    property SysMenuIconsVisible: Boolean read GetSysMenuIconsVisible write SetSysMenuIconsVisible;
    property VertScrollBar: TScrollBar read FVertScrollBar;
    property VertScrollBarVisibility: Boolean read FVertScrollBarVisibility
      write SetVertScrollBarVisibility;
    property ViewRectMode: Boolean read FViewRectMode write FViewRectMode;
    property ZoomInChange: Single read FZoomInCHange write FZoomInChange;
    property ZoomInEnable: Boolean read GetZoomInEnable;
    property ZoomOutChange: Single read FZoomOutChange write FZoomOutChange;
    property ZoomOutEnable: Boolean read GetZoomOutEnable;
  published
    property Align;
    property AutoFocus: Boolean read FAutoFocus write SetAutoFocus;
    property CenterAlignment: Boolean read FCenterAlignment
      write FCenterAlignment default False;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property MouseDragging: Boolean read FMouseDragging
      write FMouseDragging default True;
    property MouseScaling: Boolean read FMouseScaling
      write FMouseScaling default False;
    property OnChangeScale: TNotifyEvent read FOnChangeScale
      write FOnChangeScale;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnGetImageExtents: TGetImageExtents read FOnGetImageExtents
      write SetOnGetImageExtents;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnPictureChange: TNotifyEvent read FOnPictureChange
      write FOnPictureChange;
    property OnPictureMove: TNotifyEvent read FOnPictureMove
      write FOnPictureMove;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDrag;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property RectZooming: Boolean read FRectZooming
      write SetRectZooming default True;
    property Scale: Double read GetScale write SetScale;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Visible;
{$IFDEF SGDEL_4}
    property Anchors;
    property Constraints;
    property DragKind;
    property OnEndDock;
    property OnResize;
    property OnStartDock;
{$ENDIF}
{$IFDEF SGDEL_5}
    property OnContextPopup;
{$ENDIF}
  end;

  TsgDrawingNavigatorClass = class of TsgDrawingNavigator;

  function CompareDouble(const AValue1, AValue2: Double): Integer;
  procedure CorrectRect(var ARect: TRect);
  function RoundRect2D(const ARect: TF2DRect): TRect;

const
  cnstUserView = 'User View';
  sUserView: string = cnstUserView;

  procedure RepaintControl(AControl: TControl; const ARects: array of TRect;
    const ADoubleBuffered: Boolean = True);

implementation

uses
  Math;

{$IFNDEF SG_NON_WIN_PLATFORM}
  {$R *.RES}
  {$R sgOrbit3D.RES}
{$ENDIF}

type
  TsgRectangleAccess = class(TsgRectangle);
  TsgCADConverter = class(TsgDXFConverter);
  TsgCADImageAccess = class(TsgCADImage);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  TControlAccess = class(TControl);
  TsgSnapControlAccess = class(TSnapControl);
  TsgDrawRectAcces = class(TsgDrawRect);

  TsgPicture = class(TPicture)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(const AOwner: TPersistent);
  end;

  TsgNavigatorToolProps = class(TsgCustomControlToolProps)
  protected
    FNavigator: TsgDrawingNavigator;
    function GetOwner: TPersistent; override;
    function GetViewRect: TRect; override;
    function GetClipBox: TFRect; override;
    function GetClipRect: TRect; override;
    function GetGraphic: TGraphic; override;
    function GetCanvas: TCanvas; override;
    function GetViewRectMode: Boolean; override;
    function IsClipRectActive: Boolean; override;
    function WorldToClient(const APoint: TFPoint): TPoint; override;
    function ScreenToClient(P: TPoint): TPoint; override;
    function ClientToWorld(X, Y: Double; AUCSPoint: PFPoint = nil): TFPoint; override;
    procedure Invalidate; override;
    function AddClient(AClient: TObject): Integer; override;
    function RemoveClient(AClient: TObject): Integer; override;
  end;

function GetBitmapSize(AHandle: HBITMAP; var ASize: TPoint): Boolean;
var
  vSize: Integer;
  vBitmap: tagBITMAP;
begin
  ASize := Point(0, 0);
  vSize := GetObject(AHandle, SizeOf(tagBITMAP), @vBitmap);
  Result := vSize = SizeOf(tagBITMAP);
  if Result then
    ASize := Point(vBitmap.bmWidth, vBitmap.bmHeight);
end;

function CompareDouble(const AValue1, AValue2: Double): Integer;
begin
  if sgIsZero(AValue1 - AValue2) then
    Result := 0
  else
    if AValue1 < AValue2 then
      Result := -1
    else
      Result := 1;
end;

procedure CopyDC(ASrc, ADst: HDC; AWidth, AHeight: Integer);
begin
  BitBlt(ADst, 0, 0, AWidth, AHeight, ASrc, 0, 0, SRCCOPY);
end;

procedure CorrectRect2D(var ARect: TF2DRect);
begin
  if ARect.Left > ARect.Right then
    SwapSGFloats(ARect.Left, ARect.Right);
  if ARect.Top > ARect.Bottom then
    SwapSGFloats(ARect.Top, ARect.Bottom);
end;

procedure CorrectRect(var ARect: TRect);
begin
  if ARect.Left > ARect.Right then
    SwapInts(ARect.Left, ARect.Right);
  if ARect.Top > ARect.Bottom then
    SwapInts(ARect.Top, ARect.Bottom);
end;

procedure DrawDottedRect(const ADC: HDC; const ARect: TRect);
var
  vPrevBkMode, vPrevPenMode: Integer;
  vPrevPen, vPrevBrush: HGDIOBJ;
begin
  vPrevPen := SelectObject(ADC, CreatePen(PS_DOT, 1, 0));
  vPrevBrush := SelectObject(ADC, GetStockObject(NULL_BRUSH));
  vPrevPenMode := SetROP2(ADC, R2_NOTXORPEN);
  vPrevBkMode := SetBkMode(ADC, TRANSPARENT);
  Rectangle(ADC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  SetBkMode(ADC, vPrevBkMode);
  SetROP2(ADC, vPrevPenMode);
  SelectObject(ADC, vPrevBrush);
  DeleteObject(SelectObject(ADC, vPrevPen));
end;

function _Round(const AValue: Double): Integer;
begin
  if Abs(AValue) > MaxInt  then
    Result := Sign(AValue) * MaxInt
  else
    Result := Round(AValue);
end;

function RoundPoint2D(const APoint: TF2DPoint): TPoint;
begin
  Result.X := _Round(APoint.X);
  Result.Y := _Round(APoint.Y);
end;

function GetRectCenter(ARect: TRect): TPoint; overload;
begin
  Result := Point(Round((ARect.Right + ARect.Left) / 2),
    Round((ARect.Bottom + ARect.Top) / 2));
end;

function GetRectCenter(ARect: TFRect): TFPoint; overload;
begin
  Result := MakeFPoint((ARect.Right + ARect.Left) / 2,
    (ARect.Bottom + ARect.Top) / 2,
    (ARect.Z1 + ARect.Z2) / 2);
end;

function GetRectCenter(ARect: TF2DRect): TF2DPoint; overload;
begin
  Result := MakeF2DPoint((ARect.Right + ARect.Left) / 2,
    (ARect.Bottom + ARect.Top) / 2);
end;

function IsEqualFRects(const ARect1, ARect2: TFRect): Boolean;
begin
  Result := IsEqualFPoints(ARect1.TopLeft, ARect2.TopLeft) and
    IsEqualFPoints(ARect1.BottomRight, ARect2.BottomRight)
end;

function IsAbsoluteEqualFRects(const ARect1, ARect2: TFRect): Boolean;
begin
  Result := (ARect1.Left = ARect2.Left) and (ARect1.Right = ARect2.Right)
    and (ARect1.Z1 = ARect2.Z2) and (ARect1.Top = ARect2.Top)
    and (ARect1.Bottom = ARect2.Bottom);
end;

function IsEqualRects(const ARect1, ARect2: TRect): Boolean;
begin
  Result := (ARect1.Left = ARect2.Left) and (ARect1.Top = ARect2.Top) and
    (ARect1.Right = ARect2.Right) and (ARect1.Bottom = ARect2.Bottom)
end;

function TransformRect2D(const ASrc: TF2DRect; const AFSrc, AFDst: TFRect): TF2DRect;
var
  vXKf, vYKf: Double;

  procedure DoTopBottom(const ASrc: TF2DRect; const AFSrc, AFDst: TFRect;
    const AYKf: Double; var ARect: TF2DRect);
  var
    vYKf: Double;
  begin
    vYKf := (ASrc.Bottom - ASrc.Top) / AYKf;
    ARect.Top := ASrc.Top - (AFDst.Top - AFSrc.Top) * vYKf;
    ARect.Bottom := ARect.Top + (AFDst.Top - AFDst.Bottom) * vYKf;
  end;

  procedure DoLeftRight(const ASrc: TF2DRect; const AFSrc, AFDst: TFRect;
    const AXKf: Double; var ARect: TF2DRect);
  var
    vXKf: Double;
  begin
    vXKf := (ASrc.Right - ASrc.Left) / AXKf;
    ARect.Left := ASrc.Left - (AFSrc.Left - AFDst.Left) * vXKf;
    ARect.Right := ARect.Left + (AFDst.Right - AFDst.Left) * vXKf;
  end;

begin
  Result := ASrc;
  if not IsBadRect(AFSrc) and not IsBadRect(AFDst) then
  begin
    vXKf := AFSrc.Right - AFSrc.Left;
    vYKf := AFSrc.Top - AFSrc.Bottom;
    if Abs(vXKf) < fAccuracy then
      vXKf := 0.0;
    if Abs(vYKf) < fAccuracy then
      vYKf := 0.0;
    case Byte(vXKf = 0) + (Byte(vYKf = 0) shl 1) of
      1: DoTopBottom(ASrc, AFSrc, AFDst, vYKf, Result);
      2: DoLeftRight(ASrc, AFSrc, AFDst, vXKf, Result);
      3:;
    else
      DoLeftRight(ASrc, AFSrc, AFDst, vXKf, Result);
      DoTopBottom(ASrc, AFSrc, AFDst, vYKf, Result);
    end;
  end;
end;

function Rect2RectF(const R: TRect): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function Rect2DFromRect(const ARect: TRect): TF2DRect;
begin
  Result := MakeF2DRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

function IsEqualF2DRects(const ARect1, ARect2: TF2DRect): Boolean;
begin
  Result := IsEqualF2DPoints(ARect1.TopLeft, ARect2.TopLeft) and
    IsEqualF2DPoints(ARect1.BottomRight, ARect2.BottomRight);
end;

procedure OffsetRect2D(var R: TF2DRect; DX, DY: Extended);
begin
  R.Left := R.Left + DX;
  R.Top := R.Top + DY;
  R.Right := R.Right + DX;
  R.Bottom := R.Bottom + DY;
end;

function RoundRect2D(const ARect: TF2DRect): TRect;
begin
  Result.Left := _Round(ARect.Left);
  Result.Top := _Round(ARect.Top);
  Result.Right := Result.Left + _Round(ARect.Right - ARect.Left);
  Result.Bottom := Result.Top + _Round(ARect.Bottom - ARect.Top);
end;

function SplitRect(const R: TFRect; var ASize: TF2DPoint): TF2DPoint; overload;
begin
  ASize.X := R.Right - R.Left;
  ASize.Y := Abs(R.Top - R.Bottom);
  Result.X := 0.5 * (R.Left + R.Right);
  Result.Y := 0.5 * (R.Top + R.Bottom);
end;

function SplitRect(const R: TF2DRect; var ASize: TF2DPoint): TF2DPoint; overload;
begin
  ASize.X := R.Right - R.Left;
  ASize.Y := Abs(R.Top - R.Bottom);
  Result.X := 0.5 * (R.Left + R.Right);
  Result.Y := 0.5 * (R.Top + R.Bottom);
end;

procedure RepaintControl(AControl: TControl; const ARects: array of TRect;
  const ADoubleBuffered: Boolean = True);
var
  R: TRect;
  vDC, vMemDC, vStockBitmap: THandle;
  vWindow: HWND;
{$IFNDEF LCLGTK2}
  vBitmapInfo: {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}TBitmapInfo;
  vBits: Pointer;
{$ENDIF}
begin
  vDC := TControlAccess(AControl).GetDeviceContext(vWindow);
  try
    // TODO: apply ARects
    if not ADoubleBuffered then
    begin
      AControl.Perform(WM_ERASEBKGND, vDC, vDC);
      AControl.Perform(WM_PAINT, vDC, 0);
    end
    else
    begin
      vMemDC := CreateCompatibleDC(0);
      try
        R := AControl.ClientRect;
        {$IFDEF LCLGTK2}
        vStockBitmap := SelectObject(vMemDC, CreateBitmap(R.Right - R.Left, R.Bottom - R.Top, 1, 32, nil));
        {$ELSE}
        MKBIH32(vBitmapInfo.bmiHeader, R.Right - R.Left, R.Bottom - R.Top);
        vStockBitmap := SelectObject(vMemDC,
          {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}CreateDIBSection(0, vBitmapInfo, DIB_RGB_COLORS, vBits, 0, 0));
        {$ENDIF}
        try
          AControl.Perform(WM_ERASEBKGND, vMemDC, vMemDC);
          AControl.Perform(WM_PAINT, vMemDC, 0);
          {$IFDEF LCLGTK2}
          BitBlt(vDC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, vMemDC, 0, 0, SRCCOPY);
          {$ELSE}
          {$IFDEF MSWINDOWS}
          SetDIBitsToDevice(vDC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, 0, 0,
            0, R.Bottom - R.Top, vBits, vBitmapInfo, DIB_RGB_COLORS);
          {$ELSE}
          StretchDIBits(vDC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, 0, 0,
            R.Right - R.Left, R.Bottom - R.Top, vBits, vBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
          {$ENDIF}
          {$ENDIF}
        finally
          DeleteObject(SelectObject(vMemDC, vStockBitmap));
        end;
      finally
        DeleteDC(vMemDC);
      end;
    end;
  finally
{$IFDEF MSWINDOWS}
    if GetClassLong(vWindow, GCL_STYLE) and CS_OWNDC = 0 then
{$ENDIF}
      ReleaseDC(vWindow, vDC);
  end;
end;

{$IFNDEF SGDEL_4} // for Delphi 3

{ TsgCustomControl }

procedure TsgCustomControl.CMMouseWheel(var Message: TCMMouseWheel);
begin
  Message.Result := 0;
  if DoMouseWheel(Message.ShiftState, Message.WheelDelta,
     SmallPointToPoint(Message.Pos)) then
    Message.Result := 1
  else
    if Parent <> nil then
      TMessage(Message).Result := Parent.Perform(CM_MOUSEWHEEL,
        TMessage(Message).WParam, TMessage(Message).LParam);
end;

procedure TsgCustomControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  vWidth, vHeight: Integer;
begin
  vWidth := AWidth;
  vHeight := AHeight;
  inherited;
  if (vWidth <> AWidth) or (vHeight <> AHeight)
     and not (csLoading in ComponentState) then
    Resize;
end;

procedure TsgCustomControl.WMMouseWheel(var Message: TWMMouseWheel);
begin
//  if not Mouse.WheelPresent then
//  begin
//    Mouse.WheelPresent := True;
//    Mouse.SettingChanged(SPI_GETWHEELSCROLLLINES);
//  end;
  TCMMouseWheel(Message).ShiftState := KeysToShiftState(Message.Keys);
  MouseWheelHandler(TMessage(Message));
  if Message.Result = 0 then
    inherited;
end;

procedure TsgCustomControl.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csLoading in ComponentState) then
    Resize;
end;

procedure TsgCustomControl.MouseWheelHandler(var Message: TMessage);
//var
//  Form: TCustomForm;
begin
//  Form := GetParentForm(Self);
//  if (Form <> nil) and (Form <> Self) then
//    Form.MouseWheelHandler(TMessage(Message))
//  else
  TMessage(Message).Result := Perform(CM_MOUSEWHEEL, TMessage(Message).WParam,
    TMessage(Message).LParam);
end;

function TsgCustomControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  vIsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      vIsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if vIsNeg then
      begin
        if FWheelAccumulator <> 0 then
          FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

function TsgCustomControl.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

function TsgCustomControl.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

procedure TsgCustomControl.Resize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;
{$ENDIF}

{ TsgDrawingNavigator }

procedure TsgDrawingNavigator.AfterRotate(Sender: TObject);
begin
  if TsgCADImageAccess(FTool.Img).RotCount = 0 then
  try
    Include(FState, dnsAfterRotate);
    begin
      FDrawRect.Load(FTool.GetRealImageMatrix,
      Power(Abs(TsgDrawRectAcces(FDrawRect).Determinant3D), 1.0/3.0),
        FRotOffs, FTool.Img.Center);
      if TsgCADImageAccess(FTool.Img).IsRotToView then
      begin
        FTool.Img.Offset := FTool.Img.Center;
        FitToSize;
      end;
      if not OrbitAutoTarget then
        FTool.ResetRotCenter(Center, OrbitAutoTarget);
    end;
  finally
    Exclude(FState, dnsAfterRotate);
  end;
end;

procedure TsgDrawingNavigator.BeforeRotate(Sender: TObject);
begin
  if TsgCADImageAccess(FTool.Img).RotCount = 1 then
  begin
    SaveCurrentView;
    if OrbitAutoTarget then
    begin
      FRotOffs := FPointXMat(FTool.Img.Center, FDrawRect.Matrix^);
      FDrawRect.Matrix^.E0 := SubFPoint(FDrawRect.Matrix^.E0, FRotOffs);
    end
    else
      FRotOffs := FTool.PointToPosition(Center);
  end;
end;

procedure TsgDrawingNavigator.BroadcastClients(var Message);

  procedure DoBroadcast(var Message);
  var
    I: Integer;
  begin
    I := 0;
    while I < FClients.Count do
    try
      try
        TObject(FClients.List[I]).Dispatch(Message);
      finally
        if TMessage(Message).Result = 0 then
          Inc(I)
        else
          I := FClients.Count;
      end
    except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
    end;
  end;

var
  vMsg: Cardinal;
begin
  if FBroadcasting <> TMessage(Message).Msg then
  begin
    vMsg := FBroadcasting;
    try
      FBroadcasting := TMessage(Message).Msg;
      case TMessage(Message).Msg of
        WM_LBUTTONDOWN..WM_MOUSELAST:
          if not FLockInternalMouseEventsHandler then
            DoBroadcast(Message);
        WM_SETFOCUS, WM_PAINT, CM_CHILDKEY, WM_SCALECHANGING .. WM_ZOOM_EXTENTS,
        CM_MOUSEENTER, CM_MOUSELEAVE:
          DoBroadcast(Message);
        WM_KEYFIRST..WM_KEYLAST:
          DoBroadcast(Message);
        WM_MOUSEMOVE:
          begin
            if ImageDragMode then
              Exit;
            if FAppActive and not FLockInternalMouseEventsHandler then
              DoBroadcast(Message);
          end;
        CM_CURSORCHANGED, CM_INVALIDATE:
          DoBroadcast(Message);
        WM_SIZE:
          DoBroadcast(Message);
      end; { case TMessage(Message).Msg }
    finally
      FBroadcasting := vMsg;
    end;
  end;
end;

procedure TsgDrawingNavigator.Clear;
begin
  FLoadedFromConverter := False;
end;

procedure TsgDrawingNavigator.ClearBuffer;
begin
  if FBuffer <> nil then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
    FBufferWidth := 0;
    FBufferHeight := 0;
  end;
end;

procedure TsgDrawingNavigator.ClearCurrentViews;
begin
  if Assigned(FTool.Img) and Assigned(FTool.Img.CurrentLayout) then
    TsgDXFLayoutAccess(FTool.Img.CurrentLayout).View.Clear;
end;

function TsgDrawingNavigator.ClientToWorld(X, Y: Double;
  AUCSPoint: PFPoint = nil): TFPoint;
var
  vXScaled, vYScaled: Extended;
begin
  GetPointToImageSizeRatio(X, Y, vXScaled, vYScaled);
  if Assigned(FTool.Img) and Assigned(FTool.Img.CurrentLayout) then
  begin
    if FTool.IsNotCustomDraw then
    begin
      Result := CADUnProject(MakeFPoint(X, Y, 0), FDrawRect.Matrix^);
      if Assigned(AUCSPoint) then
      begin
        AUCSPoint^ := Result;
        TsgCADImageAccess(FTool.Img).TransformToUCS(AUCSPoint^);
      end;
    end
    else
    begin
{$IFDEF SGDEL_4}
      if Assigned(AUCSPoint) then
        Result := FTool.Img.GetCADCoords(vXScaled, vYScaled, AUCSPoint^)
      else
        Result := FTool.Img.GetCADCoords(vXScaled, vYScaled);
{$ELSE}
      Result := FTool.Img.GetCADCoords(vXScaled, vYScaled);
      if Assigned(AUCSPoint) then
      begin
        AUCSPoint^ := Result;
        TsgCADImageAccess(FTool.Img).TransformToUCS(AUCSPoint^);
      end;
{$ENDIF}
    end;
  end
  else
  begin
    Result := MakeFPoint(FPicture.Width * vXScaled, FPicture.Height * vYScaled, 0);
    if Assigned(AUCSPoint) then
      AUCSPoint^ := Result;
  end;
end;

procedure TSGDrawingNavigator.CMColorChanged(var Message: TMessage);
begin
  inherited;
  DoOnColorChanged(Color);
end;

procedure TSGDrawingNavigator.CMCursorChanged(var Message: TMessage);
begin
  inherited;
  DoOnCursorChanged;
end;

function TsgDrawingNavigator.CoordinateConvertion(ACoordX, ACoordY: Integer;
  var APointInUCS: TFPoint): TFPoint;
begin
  if Assigned(FTool.Img) then
    Result := FSnapControl.GetMouseCoordUCS(ACoordX, ACoordY, APointInUCS).CADPointInUCS
  else
    Result := GetRealPoint(ACoordX, ACoordY, APointInUCS);
end;

function TsgDrawingNavigator.CreateHorScroll: Boolean;
begin
  Result := False;
  if FHorzScrollBar = nil then
  begin
    FHorzScrollBar := TScrollBar.Create(Self);
    FHorzScrollBar.Visible := False;
    FHorzScrollBar.Height := GetSystemMetrics(SM_CXHTHUMB);
    FHorzScrollBar.SmallChange := iScrollSmallChange;
    FHorzScrollBar.TabStop := False;
    FHorzScrollBar.OnScroll := HScrollEvent;
    Result := True;
  end;
end;

procedure TsgDrawingNavigator.CreateOrbit3D(AClass: TClass);
begin
  if Assigned(FOrbit3D) then
    RemoveClient(FOrbit3D);
  FOrbit3D.Free;
  FOrbit3D := TsgOrbit3DClass(AClass).Create(FControlToolProps);
  Orbit3D.BoundsRect := Rect(0, 0, Width, Height);
  Orbit3D.Visible := False;
  Orbit3D.OnStateChanged := OrbitStateChanged;
  Orbit3D.OnVisibleChanging := OrbitVisibleChanging;
  Orbit3D.OnChange := OrbitChange;
  Orbit3D.OnRotate := OrbitRotate;
  AddClient(Orbit3D);
end;

function TsgDrawingNavigator.CreateVertScroll: Boolean;
begin
  Result := False;
  if FVertScrollBar = nil then
  begin
    FVertScrollBar := TScrollBar.Create(Self);
    FVertScrollBar.Visible := False;
    FVertScrollBar.Kind := sbVertical;
    FVertScrollBar.Width := GetSystemMetrics(SM_CXHTHUMB);
    FVertScrollBar.SmallChange := iScrollSmallChange;
    FVertScrollBar.TabStop := False;
    FVertScrollBar.OnScroll := VScrollEvent;
    Result := True;
  end;
end;

procedure TsgDrawingNavigator.DoChangeScrolls;
var
  vGripH, vGripW: Integer;
  vHorzBarVisibleNew, vVertBarVisibleNew: Boolean;
  vRect: TF2DRect;

  procedure MakeHorScroll(const AGripW, AGripH: Integer;
    const AVisible: Boolean);
  begin
    FHorzScrollBar.Left := 0;
    FHorzScrollBar.Width := ClientWidth - AGripW;
    FHorzScrollBar.Top := ClientHeight - AGripH;
    FHorzScrollBar.Visible := AVisible;
  end;

  procedure MakeVertScroll(const AGripW, AGripH: Integer;
    const AVisible: Boolean);
  begin
    FVertScrollBar.Left := ClientWidth - AGripW;
    FVertScrollBar.Top := 0;
    FVertScrollBar.Height := ClientHeight - AGripH;
    FVertScrollBar.Visible := AVisible;
  end;

begin
  if not (dnsSclollChanging in FState) then
  try
    Include(FState, dnsSclollChanging);
    vRect := GetUnionPictureRectWithClientRect2D;
    vHorzBarVisibleNew := (((vRect.Right - vRect.Left) > ClientWidth)
      and not Empty) and FHorzScrollBarVisibility;
    vVertBarVisibleNew := (((vRect.Bottom - vRect.Top) > ClientHeight)
      and not Empty) and FVertScrollBarVisibility;
    vGripH := FHorzScrollBar.Height;
    vGripW := FVertScrollBar.Width;
    if vHorzBarVisibleNew xor vVertBarVisibleNew then
    begin
      if vHorzBarVisibleNew then
        vGripW := 0;
      if vVertBarVisibleNew then
        vGripH := 0;
    end
    else
      if not (vHorzBarVisibleNew and vVertBarVisibleNew) then
      begin
        vGripH := 0;
        vGripW := 0;
      end;
    MakeHorScroll(vGripW, vGripH, vHorzBarVisibleNew);
    MakeVertScroll(vGripW, vGripH, vVertBarVisibleNew);
    SetScrollsRange;
    SetScrollsPosition;
  finally
    Exclude(FState, dnsSclollChanging);
  end;
end;

{ TsgDrawingNavigator.DoPictureChange

  TPicture.OnChange handler. }

procedure TsgDrawingNavigator.DoPictureChange(Sender: TObject);
var
  vIsNewGraphic: Boolean;
  vCurrentLayout: TsgDXFLayoutAccess;

  procedure SetNewMaxDim;
  begin
{$IFDEF SGFPC}
    FMaxDim := iMaxWinNTDim;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
    FMaxDim := iMaxNonWinNTDim;
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
       (Assigned(FTool.Img) or (FPicture.Graphic is TMetafile)) then
{$ENDIF}
      FMaxDim := iMaxWinNTDim;
{$ENDIF}
  end;

  procedure UpdateViewCapacity(const ALayout: TsgDXFLayout);
  begin
    if Assigned(ALayout) and (TsgDXFLayoutAccess(ALayout).View.Capacity = 0) then
      TsgDXFLayoutAccess(ALayout).View.Capacity := 1024;
  end;

begin
  if not (dnsPictureChanging in FState)  then
  begin
    Include(FState, dnsPictureChanging);
    try
      Perform(WM_PICTURECHANGE, TsgNativeUInt(Self), TsgNativeInt(FTmpGraphic));
      if Assigned(FOnPictureChange) then
        FOnPictureChange(Self);
      PictureChanged := True;
      FBufferSaved := False;
      BeginUpdate;
      try
        vIsNewGraphic := FPicture.Graphic <> FTmpGraphic;
        // refresh FTool
        // refresh FSnapControl
        if vIsNewGraphic then
        begin
          FTool.UpdateImg;
          FSnapControl.Update;
          if not Assigned(FTool.Img) then
          begin
            UpdateDrawRect(GetBox);
            FDrawRect.DettachMatrix(@cnstCrossYMat);
          end;
        end;
        if Assigned(FTool.Img) then
        begin
          if vIsNewGraphic then // new FPicture.Graphic inherits from TsgCADImage
          begin
            UpdateDrawRect(GetBox);
            FOrbit3D.Visible := False;
            SetNewMaxDim;
            FTool.Img.OnAfterRotate := AfterRotate;
            FTool.Img.OnBeforeRotate := BeforeRotate;
            FTool.Img.OnGetBox := GetBoxHandler;
            FTmpLayout := FTool.Img.CurrentLayout;
            UpdateViewCapacity(TsgDXFLayout(FTmpLayout));
            Include(FState, dnsSetView);
            try
              if not OpenVPort then
                FitToSize;
            finally
              Exclude(FState, dnsSetView);
            end;
            SetCommonUnitsWidth(0);
            PictureRectChanged := True;
          end;
          vCurrentLayout := TsgDXFLayoutAccess(FTool.Img.CurrentLayout);
          if FTmpLayout <> vCurrentLayout then
          begin
            UpdateViewCapacity(vCurrentLayout);
            FBufferSaved := False;
            if not vCurrentLayout.View.HasView then
            begin
              SaveCurrentView(FTmpLayout);
              Include(FState, dnsSetView);
              try
                FDrawRect.Matrix^ := FMatScale(FTool.GetRealImageMatrix, cnstFPointSingleYMinus);
                FitToSize;
              finally
                Exclude(FState, dnsSetView);
              end;
            end
            else
            begin
              SetCommonUnitsWidth(0);
              FOrbit3D.Visible := False;
              FBufferSaved := False;
              SaveCurrentView(FTmpLayout);
              vCurrentLayout.View.PopView(FDrawRect.Matrix);
              TsgDrawRectAcces(FDrawRect).DoChanged;
            end;
            FTmpLayout := vCurrentLayout;
          end;
        end
        else
        begin
          FTmpLayout := nil;
          FOrbit3D.Visible := False;
          if vIsNewGraphic then // new Picture.Graphic
          begin
            SetCommonUnitsWidth(0);
            SetNewMaxDim;
            FitToSize;
          end;
        end;
        FTmpGraphic := FPicture.Graphic;
      finally
        EndUpdate;
      end;
    finally
      Exclude(FState, dnsPictureChanging);
    end;
  end;
end;

procedure TsgDrawingNavigator.DrawRectChanged(Sender: TObject);
begin
  DoChangeScrolls;
  if Assigned(FTool.Img) then
    FTool.SetDrawMatrix(FDrawRect.Matrix^)
  else
    DoRepaintControl([]);
end;

procedure TsgDrawingNavigator.DrawRectChanging(Sender: TObject);
begin
  if not FMoveByMouse and
     (not FOrbit3D.Visible or (FOrbit3D.Visible and (dnsZooming in FState))) and
     not (dnsAfterRotate in FState) then
    SaveCurrentView;
end;

procedure TsgDrawingNavigator.DrawScrollBarsJoint(const ADC: HDC);
var
  vRect: TRect;
begin
  if FHorzScrollBar.Visible and FVertScrollBar.Visible then
  begin
    vRect.Right := ClientWidth;
    vRect.Bottom := ClientHeight;
    vRect.Left := vRect.Right - FVertScrollBar.Width;
    vRect.Top := vRect.Bottom - FHorzScrollBar.Height;
    DrawFrameControl(ADC, vRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
    ExcludeClipRect(ADC, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
  end;
end;

procedure TsgDrawingNavigator.DrawZoomingRect(ARect: TRect);
begin
  DrawDottedRect(Canvas.Handle, ARect);
end;

function TsgDrawingNavigator.GetActualGraphic: TGraphic;
begin
  Result := FTool.GetActualGraphic;
  if Result is TsgCADImage then
    TsgCADImage(Result).Printing := pntDirectColor;
  SetActualGraphic(Result);
end;

function TsgDrawingNavigator.GetOrbitAutoTarget: Boolean;
begin
  Result := FOrbit3D.AutoTarget;
end;

function TsgDrawingNavigator.GetBox: TFRect;
begin
  if Assigned(FTool.Img) and Assigned(FTool.Img.CurrentLayout) then
    Result := FTool.Img.CurrentLayout.Box
  else
    if Assigned(FPicture.Graphic) then
      Result := MakeFRect(0, FPicture.Height, 0, FPicture.Width, 0, 0)
    else
      Result := cnstBadRect;
  PerformBroadcastEvents(WM_GETIMAGEBOX, WPARAM(@Result), LPARAM(FPicture.Graphic));
end;

function TsgDrawingNavigator.GetBoxHandler(Sender: TObject;
  var ABox: TFRect): Boolean;
begin
  Result := False;
  if IsBadRect(ABox) and Assigned(FTool.Img) then
  begin
    try
      ABox := TsgCADImageAccess(FTool.Img).GetDefaultImageSize;
    except
      ABox := SGDefaultImageSize;
    end;
    if IsBadRect(ABox) then
      ABox := SGDefaultImageSize;
    TsgDXFLayoutAccess(FTool.Img.CurrentLayout).SetBox(ABox);
    Result := True;
  end;
  UpdateDrawRect(ABox);
end;

function TsgDrawingNavigator.GetBoxOffset: TFPoint;
begin
  Result := cnstFPointZero;
end;

function TsgDrawingNavigator.GetClipRectangle: Boolean;
begin
  Result := FRectangle.Active;
end;

function TsgDrawingNavigator.GetClipRectCoords: TRect;
begin
  Result := FRectangle.Rect;
end;

function TsgDrawingNavigator.GetCursor: TCursor;
begin
  Result := TControl(Self).Cursor;
end;

function TsgDrawingNavigator.GetImageExtents: TFRect;
begin
  if Assigned(FTool.Img) then
    Result := FTool.Img.Extents
  else
    if Assigned(FPicture.Graphic) then
      Result := MakeFRect(0, FPicture.Height, 0, FPicture.Width, 0, 0)
    else
      Result := cnstBadRect;
  PerformBroadcastEvents(WM_GETIMAGEEXTENTS, WPARAM(@Result), LPARAM(FPicture.Graphic));
end;

function TsgDrawingNavigator.GetLinearDimensionsMode: TsgDistUnitsMode;
begin
  Result := FTool.LinearDimensionsMode;
end;

function TsgDrawingNavigator.GetLinearDimensionsScaleFactor: Double;
begin
  Result := FTool.LinearDimensionsScale;
end;

function TsgDrawingNavigator.GetLoadedFromConverter: Boolean;
begin
  Result := FLoadedFromConverter;
end;

function TsgDrawingNavigator.GetMeasureDisplayedUnits: TsgInsUnits;
begin
  Result := cnstDisplayedUnits;
  if Assigned(MeasureProps) then
    Result := MeasureProps.DisplayedUnits;
end;

function TsgDrawingNavigator.GetMeasureInitialUnits: TsgInsUnits;
begin
  Result := cnstInitialUnits;
  if Assigned(MeasureProps) then
    Result := MeasureProps.InitialUnits;
end;

function TsgDrawingNavigator.GetMeasurePrecisionFactor: Integer;
begin
  Result := cnstPrecisionFactorGlobal;
  if Assigned(MeasureProps) then
    Result := MeasureProps.PrecisionFactor;
end;

function TsgDrawingNavigator.GetMeasureScaleFactor: Double;
begin
  Result := cnstLinearDimensionsFactorGlobal;
  if Assigned(MeasureProps) then
    Result := MeasureProps.ScaleFactor;
end;

function TsgDrawingNavigator.GetMouseMovePrevPoint: TPoint;
begin
  Result := FMouseMovePrevPoint;
end;

function TsgDrawingNavigator.GetOrbit3DClass: TClass;
begin
  Result := TsgOrbit3D;
end;

function TsgDrawingNavigator.GetPictureHeight: Integer;
begin
  Result := Round(PictureRectHeight);
end;

function TsgDrawingNavigator.GetPictureRect: TRect;
begin
  Result := FDrawRect.RoundRect;
end;

function TsgDrawingNavigator.GetPictureRectCenter: TPoint;
begin
  Result := RoundPoint2D(GetPictureRectCenter2D);
end;

function TsgDrawingNavigator.GetPictureRectCenter2D: TF2DPoint;
begin
  Result := GetRectCenter(PictureRect2D);
end;

function TsgDrawingNavigator.GetPictureRectHeight: Double;
var
  R: TFRect;
begin
  R := FDrawRect.FRect;
  Result := R.Top - R.Bottom;
end;

function TsgDrawingNavigator.GetPictureRectWidth: Double;
var
  R: TFRect;
begin
  R := FDrawRect.FRect;
  Result := R.Right - R.Left;
end;

function TsgDrawingNavigator.GetPictureWidth: Integer;
begin
  Result := Round(PictureRectWidth);
end;

procedure TsgDrawingNavigator.GetPointToImageSizeRatio(ACoordX,
  ACoordY: Double; var AXScaled, AYScaled: Extended);
var
  vExtents: TFRect;
  R: TF2DRect;
  vSx, vSy: Double;
begin
  R := PictureRect2D;
  SwapDoubles(R.Top, R.Bottom);
  if IsRectEmptyF2D(R) or (R.Right = R.Left) or (R.Top = R.Bottom) then
  begin
    AXScaled := 0;
    AYScaled := 0;
  end
  else
  begin
    AXScaled := (ACoordX - R.Left) / (R.Right - R.Left);
    AYScaled := 1 - (ACoordY - R.Bottom) / (R.Top - R.Bottom);
    if FTool.IsNotCustomDraw and not FTool.Img.IsWithoutBorder then
    begin
      vSx := FTool.Img.BorderSize;
      vSy := vSx;
      case FTool.Img.BorderType of
        btRatio:;
        btGlobal:
          begin
            vExtents := GetImageExtents;
            vSx := vSx / (vExtents.Right - vExtents.Left - 2 * vSx);
            vSy := vSy / (vExtents.Top - vExtents.Bottom - 2 * vSy);
          end;
      end;
      AXScaled := (AXScaled + vSx) / (1 + 2 * vSx);
      AYScaled := (AYScaled + vSy) / (1 + 2 * vSy);
    end;
  end;
end;

function TsgDrawingNavigator.GetPictureRect2D: TF2DRect;
var
  R: TFRect;
begin
  R := FDrawRect.FRect;
  Result.Left := R.Left;
  Result.Top := R.Bottom;
  Result.Right := R.Right;
  Result.Bottom := R.Top;
end;

function TsgDrawingNavigator.GetRectangle: TsgRectangle;
begin
  Result := FRectangle;
end;

function TsgDrawingNavigator.GetScale: Double;
var
  R: TFRect;
begin
  Result := 1.0;
  if not Empty and (FWidthInPixels <> 0) then
  begin
    R := FDrawRect.FRect;
    Result := (R.Right - R.Left) / FWidthInPixels;
  end;
end;

function TsgDrawingNavigator.GetSizesAsString: string;
begin
  Result := '';
end;

function TsgDrawingNavigator.GetSysMenuIconsVisible: Boolean;
begin
  Result := Assigned(FSysMenuIcons);
end;

function TsgDrawingNavigator.GetUnionPictureRectWithClientRect2D: TF2DRect;
var
  CR: TRect;
  PR: TFRect;
  UR: TFRect;
begin
  CR := ClientRect;
  PR := MakeFRectFromF2dRect(GetPictureRect2D);
  SwapDoubles(PR.Top, PR.Bottom);
  UR := MakeFRect(CR.Left, CR.Bottom, 0, CR.Right, CR.Top, 0);
  UnionFRect(UR, PR);
  Result.Left := UR.Left;
  Result.Top := UR.Bottom;
  Result.Right := UR.Right;
  Result.Bottom := UR.Top;
end;

procedure TsgDrawingNavigator.GetView(var AMatrix: TFMatrix);
begin
  AMatrix := FDrawRect.Matrix^;
end;

function TsgDrawingNavigator.GetZoomInEnable: Boolean;
var
  R: TFRect;
begin
  R := FDrawRect.FRect;
  Result :=
    not((Abs(R.Right - R.Left) * FZoomInChange >= FMaxDim)
    or(Abs(R.Bottom - R.Top) * FZoomInChange >= FMaxDim));
end;

function TsgDrawingNavigator.GetZoomOutEnable: Boolean;
var
  R: TFRect;
begin
  R := FDrawRect.FRect;
  Result := not((Abs(R.Right - R.Left) <= iMinPicSize)
    or(Abs(R.Bottom - R.Top) <= iMinPicSize));
end;

procedure TsgDrawingNavigator.HScrollEvent(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if not (dnsSclollChanging in FState) and not (dnsMoving in FState) then
    case ScrollCode of
      scLineUp: MovePictureRect(FHorzScrollBar.SmallChange, 0);
      scLineDown: MovePictureRect(-FHorzScrollBar.SmallChange, 0);
      scEndScroll:; // do nothing
    else
      MovePictureRect2D(-FHorzScrollBar.Position - FDrawRect.FRect.Left, 0);
    end;
end;

procedure TsgDrawingNavigator.RectZoomingOperation(ACoordX, ACoordY: Integer;
  AErase: Boolean);
var
  vPoint: TPoint;
begin
  if FRectZoomingProcess then
  begin
    if AErase then
      DrawZoomingRect(FZoomingRect);
    vPoint := Point(ACoordX, ACoordY);
    if PtInRect(ViewRect, vPoint) then
      FZoomingRect.BottomRight := vPoint;
    DrawZoomingRect(FZoomingRect);
  end;
end;

procedure TsgDrawingNavigator.Repaint;
begin
  FPictureChanged := True;
  inherited Repaint;
end;

procedure TsgDrawingNavigator.SaveCurrentView(ALayout: TObject = nil);
var
  vLayout: TsgDXFLayoutAccess;
  vTopView: TFMatrix;
begin
  if not (dnsSetView in FState) and Assigned(FTool.Img) then
  begin
    if Assigned(ALayout) then
      vLayout := TsgDXFLayoutAccess(ALayout)
    else
      vLayout := TsgDXFLayoutAccess(FTool.Img.CurrentLayout);
    if Assigned(vLayout) then
    begin
      if not vLayout.View.HasView(@vTopView) or
         not IsEqualFMatrix(vTopView, FDrawRect.Matrix^, True) then
        vLayout.View.PushView(FDrawRect.Matrix^);
    end;
  end;
end;

procedure TsgDrawingNavigator.SetActualGraphic(AGraphic: TGraphic);
begin
  FActualGraphic.Free;
  FActualGraphic := AGraphic;
  DoGetActualGraphic;
end;

procedure TsgDrawingNavigator.SetAutoFocus(const AValue: Boolean);
begin
  FAutoFocus := AValue;
end;

procedure TsgDrawingNavigator.SetBlockFocusUpdate(const Value: Boolean);
begin
  FBlockFocusUpdate := Value;
end;

procedure TsgDrawingNavigator.SetOrbitAutoTarget(const AValue: Boolean);
begin
  FOrbit3D.AutoTarget := AValue;
end;

procedure TsgDrawingNavigator.SetClipRectangle(const AValue: Boolean);
begin
  if FRectangle.Active <> AValue then
  begin
    FRectangle.Active := AValue;
    if not FRectangle.Active then
    begin
      Perform(WM_CANCELMODE, 0, 0);
      if Assigned(FOrbit3D) and FOrbit3D.Visible then
        Cursor := TCursor(Ord(FOrbit3D.CurrentState) + 1)
      else
        Cursor := TCursor(cnstCursorCross);
    end
    else
    begin
      FRectZooming := False;
      FRectZoomingProcess := False;
    end;
  end;
end;

procedure TsgDrawingNavigator.SetClipRectCoords(const AValue: TRect);
begin
  FRectangle.Rect := AValue;
end;

procedure TsgDrawingNavigator.SetClipRectPopupMenu(
  const AValue: TPopupMenu);
begin
  FClipRectPopupMenu := AValue;
  if AValue <> nil then
  begin
{$IFDEF SGDEL_4}
  {$IFNDEF SGFPC}
    AValue.ParentBiDiModeChanged(Self);
  {$ENDIF}
{$ENDIF}
    AValue.FreeNotification(Self);
  end;
end;

procedure TsgDrawingNavigator.SetCommonUnitsWidth(ADC: HDC);
var
  vExtents: TFRect;
  vUnits: Integer;
  vUnitsUserFactor, vAltUnitsUserFactor, vmmPerPixelX: Double;
  vUnitsUserName, vAltUnitsUserName: string;
begin
  if Assigned(FTool.Img) then
  begin
    vExtents := GetImageExtents;
    FWidthInPixels := FTool.ConvertUnits(vExtents.BottomRight).X -
      FTool.ConvertUnits(vExtents.TopLeft).X;
    vUnits := Integer(GetDistanceUnits(FTool.Img, FTool.Img.CurrentLayout,
      vUnitsUserName, vUnitsUserFactor,
      vAltUnitsUserName, vAltUnitsUserFactor));
    if ADC = 0 then
      vmmPerPixelX := GetScreenMMPerPixel
    else
      vmmPerPixelX := GetDeviceCaps(ADC, HORZSIZE) / GetDeviceCaps(ADC, HORZRES);
    case vUnits of
      1: FWidthInPixels := FWidthInPixels * mmPerInch / vmmPerPixelX; // duInch
      2: FWidthInPixels := FWidthInPixels / vmmPerPixelX;             // duMM
    end;
  end
  else
    if not Empty then
      FWidthInPixels := FPicture.Width
    else
      FWidthInPixels := 0;
end;

procedure TsgDrawingNavigator.SetConstants(const Value: TsgConstantsCustom);
begin
  FConstants := Value;
  if Assigned(FSnapControl) then
    TsgSnapControlAccess(FSnapControl).Constants := FConstants;
end;

procedure TsgDrawingNavigator.SetCursor(Value: TCursor);
begin
  if FImageDragMode then
  begin
    if (Value = TCursor(cnstCursorHand)) then
      TControl(Self).Cursor := Value;
  end
  else
    TControl(Self).Cursor := Value;
end;

procedure TsgDrawingNavigator.SetDrawOnEveryMouseWheel(const AValue: Boolean);
begin
  if FDrawOnEveryMouseWheel xor AValue then
    if FUpdateCount = 0 then
      FDrawOnEveryMouseWheel := AValue;
end;

procedure TsgDrawingNavigator.SetOnEnterClipRect(const Value: TNotifyEvent);
begin
  FRectangle.OnEnterRect := Value;
end;

procedure TsgDrawingNavigator.SetHorzScrollBarVisibility(
  const AValue: Boolean);
begin
  FHorzScrollBarVisibility := AValue;
  DoChangeScrolls;
end;

procedure TsgDrawingNavigator.SetImageDragMode(const Value: Boolean);
begin
  FImageDragMode := Value;
  if FImageDragMode then
    Cursor := TCursor(cnstCursorHand)
  else
    Cursor := TCursor(cnstCursorCross);
end;

procedure TsgDrawingNavigator.SetLastViewName(const Value: string);
begin
  FLastViewName := Value;
end;

procedure TsgDrawingNavigator.SetLinearDimensionsMode(
  const Value: TsgDistUnitsMode);
begin
  FTool.LinearDimensionsMode := Value;
end;

procedure TsgDrawingNavigator.SetLinearDimensionsScale(const AFactor: Double);
begin
  if AFactor > 0 then
    FTool.LinearDimensionsScale := AFactor;
end;

procedure TsgDrawingNavigator.SetMeasureProps(
  const Value: TsgMeasureUnitsProps);
begin
  FMeasureProps := Value;
end;

procedure TsgDrawingNavigator.SetOnGetImageExtents(
  const AValue: TGetImageExtents);
begin
  FOnGetImageExtents := AValue;
end;

procedure TsgDrawingNavigator.SetPicture(const AValue: TPicture);
begin
  FPicture.Assign(AValue);
end;

procedure TsgDrawingNavigator.SetPictureChanged(const AValue: Boolean);
begin
  FPictureChanged := AValue;
end;

procedure TsgDrawingNavigator.SetPictureRectChanged(const AValue: Boolean);
begin
  FPictureRectChanged := AValue;
  FBufferSaved := False;
end;

procedure TsgDrawingNavigator.SetRectZooming(const AValue: Boolean);
begin
  if FRectZooming <> AValue then
  begin
    FRectZooming := AValue;
    if FRectZooming then
      SetClipRectangle(False);
  end;
end;

procedure TsgDrawingNavigator.SetScale(const AValue: Double);
begin
  AlterScale(AValue, True, Center);
end;

procedure TsgDrawingNavigator.SetScrollsPosition;
var
  vRect: TF2DRect;
begin
  vRect := GetUnionPictureRectWithClientRect2D;
  if vRect.Left < -MaxInt then
    vRect.Left := -MaxInt
  else
    if vRect.Left > MaxInt then
      vRect.Left := MaxInt;
  if vRect.Top < -MaxInt then
    vRect.Top := -MaxInt
  else
    if vRect.Top > MaxInt then
      vRect.Top := MaxInt;
  if FHorzScrollBar <> nil then
    FHorzScrollBar.Position := -Round(vRect.Left);
  if FVertScrollBar <> nil then
    FVertScrollBar.Position := -Round(vRect.Top);
end;

procedure TsgDrawingNavigator.SetScrollsRange;
const
  iMargin = 48;
var
  I: Integer;
  vLW: Cardinal;
  vRect: TF2DRect;

  procedure CheckRange(var ARange: Integer);
  begin
    if ARange > MaxWord shr 1 then
      ARange := MaxWord shr 1;
    if ARange < iMargin then
      ARange := iMargin;
  end;

  procedure CheckMax(var ASize: Cardinal);
  begin
    if ASize > Cardinal(FMaxDim) then
      ASize := FMaxDim;
  end;

  function PicRectHorMinPos(const ARect: TF2DRect): Integer;
  var
    vD: Double;
  begin
    vD := ClientWidth - (ARect.Right - ARect.Left);//PicRectWidth;
    if Abs(vD) > FMaxDim then
      vD := -FMaxDim;
    Result := Round(vD);
  end;

  function PicRectVertMinPos(const ARect: TF2DRect): Integer;
  var
    vD: Double;
  begin
    vD := ClientHeight - (ARect.Bottom - ARect.Top);//PicRectHeight;
    if Abs(vD) > FMaxDim then
      vD := -FMaxDim;
    Result := Round(vD);
  end;

begin
  vRect := GetUnionPictureRectWithClientRect2D;
  if FHorzScrollBar <> nil then
  begin
    FHorzScrollBar.Min := 0;
    vLW := -PicRectHorMinPos(vRect);
    CheckMax(vLW);
    FHorzScrollBar.Max := vLW;
    I := ViewRect.Right - ViewRect.Left - iMargin;
    CheckRange(I);
    FHorzScrollBar.LargeChange := I;
    FHorzScrollBar.SmallChange := I div iMargin;
  end;
  if FVertScrollBar <> nil then
  begin
    FVertScrollBar.Min := 0;
    vLW := -PicRectVertMinPos(vRect);
    CheckMax(vLW);
    FVertScrollBar.Max := vLW;
    I := ViewRect.Bottom - ViewRect.Top - iMargin;
    CheckRange(I);
    FVertScrollBar.LargeChange := I;
    FVertScrollBar.SmallChange := I div iMargin;
  end;
end;

procedure TsgDrawingNavigator.SetStretch(const AValue: Boolean);
begin
  if FStretch <> AValue then
  begin
    FStretch := AValue;
    FitToSize;
  end
end;

procedure TsgDrawingNavigator.SetSysMenuIconsVisible(const AValue: Boolean);
begin
  if SysMenuIconsVisible <> AValue then
    if AValue then
    begin
      FSysMenuIcons := TsgSysMenuIcons.Create(FControlToolProps);
      FClients.Remove(FSysMenuIcons);
      FClients.Insert(0, FSysMenuIcons);
      if Assigned(FOnGetSysMenuIcons) then
        FOnGetSysMenuIcons(Self);
      Self.Repaint;
    end
    else
      FreeAndNil(FSysMenuIcons);
end;

procedure TsgDrawingNavigator.SetVertScrollBarVisibility(const AValue: Boolean);
begin
  FVertScrollBarVisibility := AValue;
  DoChangeScrolls;
end;

procedure TsgDrawingNavigator.SetVisibleArea;
begin
  if not FLockChangeVisibleArea then
    FTool.ViewportRect := ViewRect;
end;

procedure TsgDrawingNavigator.StartMoving(const ACoordX, ACoordY: Integer; Button: TMouseButton);
begin
  if (not bMoveByRightMouse) and (not FImageDragMode) and (Button = mbRight) then Exit;
  if FMouseDragging then
  begin
    SaveCurrentView;
    FMoveByMouse := True;
    FMouseMovePrevPoint := Point(ACoordX, ACoordY);
    Cursor := TCursor(cnstCursorHand);
  end;
end;

{$IFNDEF SGDEL_6}
type
  TPopupMenuAccess = class(TPopupMenu);
{$ENDIF}
procedure TsgDrawingNavigator.StopMouseMoveOperationsHandler(
  var Message: TMessage);
var
  vIsContextPopup, vPtOnClient: Boolean;
  P: TPoint;
  vControl: TWinControl;
begin
  FMouseMoveZoomingProcess := False;
  vIsContextPopup := False;
  if Assigned(PopupMenu) then
  begin
    P := ScreenToClient({$IFNDEF SGDEL_6}TPopupMenuAccess{$ENDIF}(PopupMenu).PopupPoint);
    vIsContextPopup := (P.X = FMouseDownPt.X) and (P.Y = FMouseDownPt.Y)
  end;
  if not vIsContextPopup then // contextpopup
    UpdateCursor(TCursor(cnstCursorCross))
  else
    UpdateCursor(crArrow);
  GetCursorPos(P);
  vPtOnClient := PtInRect(ClientRect, ScreenToClient(P));
  case Message.Msg of
    WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, CM_MOUSELEAVE:
      if MouseCapture then
      begin
        vControl := FindControl(WindowFromPoint(P));
        if not vPtOnClient or (Assigned(vControl) and (Self <> vControl)) then
        begin
          MouseCapture := False;
          if Assigned(vControl) then
            Screen.Cursor := vControl.Cursor
          else
            Screen.Cursor := crDefault;
        end;
      end;
    CM_CANCELMODE:
      if not vPtOnClient then
        MouseCapture := False;
  end;
end;

procedure TsgDrawingNavigator.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  UpdateCursor(TCursor(cnstCursorCross));
end;

procedure TsgDrawingNavigator.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  StopMouseMoveOperationsHandler(Message);
end;

procedure TsgDrawingNavigator.CMCancelMode(var Message: TMessage);
begin
  inherited;
  StopMouseMoveOperationsHandler(Message);
end;

procedure TsgDrawingNavigator.StopMoveByMouse(AForceUpdate: Boolean = False);
begin
  if FMoveByMouse or AForceUpdate then
  begin
    FMoveByMouse := False;
    MouseCapture := False;
    UpdateCursor(TCursor(cnstCursorCross));
  end;
end;

procedure TsgDrawingNavigator.UnlockChangeVisibleArea;
begin
  FLockChangeVisibleArea := False;
end;

function TsgDrawingNavigator.UpdateCursor(ADefault: TCursor): TCursor;
begin
  if Assigned(FOrbit3D) and FOrbit3D.Visible then
    Result := TCursor(Ord(FOrbit3D.CurrentState) + 1)
  else
    Result := ADefault;
  Cursor := Result;
end;

procedure TsgDrawingNavigator.UpdateDrawRect(const ABox: TFRect);
begin
  if GetBoxType(ABox, fMaxResolution) in [bxEmpty] then
    FDrawRect.Box := MakeFRect(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5)
  else
    FDrawRect.Box := ABox;
end;

procedure TsgDrawingNavigator.UpdateSnap;
begin
  FreeAndNil(FSnapControl);
  CreateSnap;
  FSnapControl.Update;
end;

function TsgDrawingNavigator.ViewRectHeight: Integer;
var
  vViewRect: TRect;
begin
  vViewRect := ViewRect;
  Result := vViewRect.Bottom - vViewRect.Top;
end;

function TsgDrawingNavigator.ViewRectWidth: Integer;
var
  vViewRect: TRect;
begin
  vViewRect := ViewRect;
  Result := vViewRect.Right - vViewRect.Left;
end;

procedure TsgDrawingNavigator.VScrollEvent(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if not (dnsSclollChanging in FState) and not (dnsMoving in FState) then
    case ScrollCode of
      scLineUp: MovePictureRect(0, FVertScrollBar.SmallChange);
      scLineDown: MovePictureRect(0, -FVertScrollBar.SmallChange);
      scEndScroll:; // do nothing
    else
      MovePictureRect2D(0, -FVertScrollBar.Position - FDrawRect.RectangleF.Top);
    end;
end;

function TsgDrawingNavigator.ApplicationHook(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_ACTIVATEAPP then
    if TWMActivateApp(Message).Active then
      FAppActive := True
    else
      FAppActive := False;
  Result := False;
end;

function TsgDrawingNavigator.ConvertUnits(APoint: TFPoint;
  var AUnits: string): TFPoint;
var
  vFactor: Double;
begin
  FTool.CoordinateParameters(AUnits, vFactor);
  APoint := FTool.ConvertUnits(APoint);
  Result := MakeFPoint(APoint.X * vFactor, APoint.Y * vFactor,
    APoint.Z * vFactor);
end;

procedure TsgDrawingNavigator.CreateHandle;
begin
  inherited CreateHandle;
  FBufferSaved := False;
  ClearBuffer;
end;

procedure TsgDrawingNavigator.DoBroadcastPaintEvent(ADC: HDC);
begin
  if FNeedBroadcastPaintEvent then
  begin
    FNeedBroadcastPaintEvent := False;
    PerformBroadcastEvents(WM_PAINT, ADC, 0);
  end;
end;

procedure TsgDrawingNavigator.DoChangeScale;
begin
  FBufferSaved := False;
{$IFNDEF SG_THREAD_DRAW}
  if FUpdateCount = 0 then
    DoRepaintControl([]);
{$ELSE}
  Invalidate;
{$ENDIF}
  Perform(WM_SCALECHANGING, 0, 0);
  if Assigned(FOnChangeScale) then
    FOnChangeScale(Self);
end;

function TsgDrawingNavigator.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  vMouseWheel: Boolean;
begin
  Result := False;
  if FLockInternalMouseEventsHandler then Exit;
  vMouseWheel := FMouseWheel;
  try
    FMouseWheel := True;
  if not FDrawOnEveryMouseWheel then
    BeginUpdate;
  try
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  finally
    if not FDrawOnEveryMouseWheel then
      EndUpdate;
  end;
  finally
    FMouseWheel := vMouseWheel;
  end;
end;

function TsgDrawingNavigator.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if FLockInternalMouseEventsHandler then Exit;
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if (not Result) and not (dnsZooming in FState) then
  begin
{$IFNDEF SGFPC}
    MousePos := ScreenToClient(MousePos);
{$ENDIF}
    if (Shift = [ssShift]) or ((Shift = []) and FScalingOnMouseWheel) or
       (Shift = [ssLeft]) then
      Result := AlterScale(FZoomOutChange, False, MousePos)
    else
      if (Shift = [ssCtrl]) then
        Result := AlterScale(0.99, False, MousePos)
  end;
end;

function TsgDrawingNavigator.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if FLockInternalMouseEventsHandler then Exit;
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if (not Result) and (not (dnsZooming in FState)) then
  begin
{$IFNDEF SGFPC}
    MousePos := ScreenToClient(MousePos);
{$ENDIF}
    if (Shift = [ssShift]) or ((Shift = []) and FScalingOnMouseWheel) or
       (Shift = [ssLeft]) then
      Result := AlterScale(FZoomInChange, False, MousePos)
    else
      if (Shift = [ssCtrl]) then
        Result := AlterScale(1.01, False, MousePos)
  end;
end;

procedure TsgDrawingNavigator.DoOnColorChanged(const AColor: TColor);
begin

end;

procedure TsgDrawingNavigator.DoOnCursorChanged;
begin
  if HandleAllocated then
    Perform(WM_SETCURSOR, WindowHandle, HTCLIENT);
end;

procedure TsgDrawingNavigator.DoPictureMove;
begin
  FBufferSaved := False;
{$IFNDEF SG_THREAD_DRAW}
  if FUpdateCount = 0 then
    DoRepaintControl([]);
{$ELSE}
  Invalidate;
{$ENDIF}
  Perform(WM_PICTUREMOVE, 0, 0);
  if Assigned(FOnPictureMove) then
    FOnPictureMove(Self);
end;

procedure TsgDrawingNavigator.DoGetActualGraphic;
begin
  if Assigned(FOnGetActualGraphic) then
    FOnGetActualGraphic(Self);
end;

function TsgDrawingNavigator.GetPoint(APoint: TFPoint): TPoint;
type
  PDoubleBytes = ^TDoubleBytes;
  TDoubleBytes = array [0 .. 2 * SizeOf(Double) - 1] of Byte;
var
  vPVal: PDoubleBytes;
begin
  vPVal := @APoint;
  if Assigned(FTool.Img) and Assigned(FTmpGraphic) then
    Result := FTool.Img.GetPoint(APoint)
  else
    try
      APoint.X := 1/0;
      if not IsNan(vPVal^[0]) and not IsNan(vPVal^[SizeOf(Double)])
        and (Abs(APoint.X) < FMaxDim) and (Abs(APoint.Y) < FMaxDim) then
        Result := Point(Round(APoint.X), Round(APoint.Y))
      else
        Result := Point(0, 0);
    except
      Result := Point(0, 0);
    end;
end;

function TsgDrawingNavigator.InitializeIterateParams: PsgCADIterate;
begin
  FillChar(FIterateParam, SizeOf(FIterateParam), 0);
  FIterateParam.Matrix := cnstIdentityMat;
  Result := @FIterateParam;
end;

procedure TsgDrawingNavigator.DoRepaintControl(const ARects: array of TRect);
begin
{$IFDEF LCLGTK2}
  Invalidate;
{$ELSE}
  RepaintControl(Self, [], FDoubleBuffered);
{$ENDIF}
end;

function TsgDrawingNavigator.IsCanBeEdited: Boolean;
begin
  Result := True;
end;

function TsgDrawingNavigator.IsCanShowCoords: Boolean;
begin
  Result := True;
end;

function TsgDrawingNavigator.IsCanShowSizes: Boolean;
begin
  Result := True;
end;

function TsgDrawingNavigator.IsUseGraphic: Boolean;
begin
  Result := True;
end;

function TsgDrawingNavigator.IsClipRectActive: Boolean;
begin
  Result := FRectangle.Active and (not IsRectEmpty(FRectangle.Rect));
end;

function TsgDrawingNavigator.IsImgLocked: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TsgDrawingNavigator.IsMouseInSysButtons: Boolean;
var
  vMousePos: TPoint;
begin
  Result := False;
  if SysMenuIconsVisible and GetCursorPos(vMousePos) then
    Result := SysMenuIcons.PointInIcons(Self.ScreenToClient(vMousePos));
end;

procedure TsgDrawingNavigator.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  function RectangleOperation(const ACoordX, ACoordY: Integer): Boolean;
  var
    vPoint: TPoint;
  begin
    Result := False;
    if IsClipRectActive then
      if IsPointInRectI(FRectangle.Rect, FMouseDownPt) or
         TsgRectangleAccess(FRectangle).Capture then
        if (Button = mbRight) and (ClipRectPopupMenu <> nil) then
        begin
          vPoint := ClientToScreen(Point(X, Y));
          ClipRectPopupMenu.Popup(vPoint.X, vPoint.Y);
          StopMoveByMouse;
          Result := True;
        end;
  end;

begin
  if FLockInternalMouseEventsHandler then Exit;
  inherited MouseDown(Button, Shift, X, Y);
  FMouseDownPt := Point(X, Y);
  if not RectangleOperation(X, Y) then
  begin
    case Button of
      mbLeft:
        if not FOrbit3D.Visible then
        begin
          if FImageDragMode then
            StartMoving(X, Y, mbLeft)
          else
          if FRectZooming then
          begin
            if FRectZoomingProcess then
              RectZoomingOperation(X, Y, True)
            else
            begin
              FRectZoomingProcess := True;
              FZoomingRect := Rect(X, Y, X, Y);
            end;
          end
          else
            if FMouseScaling then
              FMouseMoveZoomingProcess := True;
        end
        else
          SaveCurrentView;
      mbMiddle:
        begin
          if (Shift * [ssMiddle, ssDouble] = [ssMiddle, ssDouble]) then
            FitToSize
          else
          begin
            if FRectangle.Active then
              MouseDown(mbLeft, [], X, Y);
            StartMoving(X, Y, mbMiddle);
          end;
        end;
      mbRight:
        begin
          if FRectangle.Active then
            MouseDown(mbLeft, [], X, Y);
          StartMoving(X, Y, mbRight);
        end;
    end;
  end;
end;

procedure TsgDrawingNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vIsMoveMode: Boolean;
begin
  if FLockInternalMouseEventsHandler then Exit;
  inherited MouseMove(Shift, X, Y);
  vIsMoveMode := (FMoveByMouse and (Shift * [ssRight, ssMiddle] <> [])) or
    (ImageDragMode and (Shift * [ssLeft] <> []));
  if FAppActive then
  begin
    if not vIsMoveMode then
      StopMoveByMouse;
    if vIsMoveMode then
    begin
      MouseCapture := True;
      Cursor := TCursor(cnstCursorHand);
      MovePictureRect(X - FMouseMovePrevPoint.X, Y - FMouseMovePrevPoint.Y);
      FMouseMovePrevPoint := Point(X, Y);
    end
    else
      if FRectZoomingProcess then
        RectZoomingOperation(X, Y, True)
      else
        if FMouseMoveZoomingProcess then
        begin
          if FZoomChangeByMouseMove <> 0 then
          begin
            if Y > FMouseMovePrevPoint.Y then
              AlterScale(FZoomChangeByMouseMove, False, FMouseDownPt)
            else
              if Y < FMouseMovePrevPoint.Y then
                AlterScale(1 / FZoomChangeByMouseMove, False, FMouseDownPt);
            FMouseMovePrevPoint.Y := Y;
          end;
        end;
    if (IsMouseInSysButtons) and (not (Self.ClassType = TsgDrawingNavigator)) then
      Cursor := crDefault;
  end;
end;

procedure TsgDrawingNavigator.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FLockInternalMouseEventsHandler then Exit;
  inherited MouseUp(Button, Shift, X, Y);
  if not FRectangle.Active and not FOrbit3D.Visible then
  begin
    if FRectZoomingProcess then
    begin
      DrawZoomingRect(FZoomingRect);
      ZoomRect(FZoomingRect);
      FRectZoomingProcess := False;
    end;
    FMouseMoveZoomingProcess := False;
  end;
  StopMoveByMouse(True);
end;

procedure TsgDrawingNavigator.ObtainFocus(var Message: TMessage);
begin
  if not FBlockFocusUpdate then
  begin
    inherited;
    if (Focused) and (not FAppActive) then
      FAppActive := True;
    if not (csDesigning in ComponentState) and AutoFocus then
      if Enabled and Visible and CanFocus and Application.Active then
      begin
        if not Focused then
          SetFocus;
        if GetFocus <> Handle then
          {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ELSE}LCLIntf.{$ENDIF}SetFocus(Handle);
      end;
  end;
end;

function TsgDrawingNavigator.OpenVPort: Boolean;
var
  P: TsgDXFVport;
begin
  Result := False;
  if Assigned(FTool.Img) and Assigned(FTool.Img.CurrentLayout) and FTool.Img.CurrentLayout.IsModel then
  begin
    P := TsgCADConverter(FTool.Img.Converter).ActiveVPort;
    if Assigned(P) and not sgIsZero(P.ViewHeight) then
      Result := TsgDrawRectAcces(FDrawRect).OpenVPort(P.ViewDirection, P.ViewTwistAngle,
        P.ViewTarget, P.ViewCenterPoint, P.ViewHeight, P.ViewAspectRatio, Rect2RectF(ClientRect));
  end;
end;

procedure TsgDrawingNavigator.OrbitStateChanged(Sender: TObject);
begin
  Cursor := TCursor(Ord(Orbit3D.CurrentState) + 1);
  Perform(WM_SETCURSOR, Handle, HTCLIENT);
end;

procedure TsgDrawingNavigator.OrbitVisibleChanging(Sender: TObject);
begin
  if FOrbit3D.Visible then
  begin
    PictureChanged := True;
    Cursor := TCursor(cnstCursorCross);
  end;
end;

procedure TsgDrawingNavigator.OrbitMouseCaptureChanged(Sender: TObject);
begin
  MouseCapture := TsgOrbit3D(Sender).MouseCapture;
end;

procedure TsgDrawingNavigator.OrbitChange(Sender: TObject);
begin
  Refresh;
end;

procedure TsgDrawingNavigator.OrbitRotate(Sender: TObject);
begin
  Rotate(FOrbit3D.Pitch, FOrbit3D.Turn, FOrbit3D.Roll);
end;

procedure TsgDrawingNavigator.Paint;
var
  vDC: HDC;

  procedure DoCustomPaint(const ADC: HDC);
  var
    I: Integer;
    R: TRect;
    vBitmap: TBitmap;
{$IFDEF LCLGTK2}
    vHandle, vTmpDC: THandle;
{$ELSE}
    vBI: {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}TBitmapInfo;
{$ENDIF}
  begin
    SetVisibleArea;
    if FDoubleBuffered then
    begin
      R := ViewRect;
      if not FBufferSaved then
      begin
        if (FBufferWidth <> (R.Right - R.Left)) or (FBufferHeight <> (R.Bottom - R.Top)) then
        begin
          if FBuffer <> nil then
            FreeMem(FBuffer);
          FBufferWidth := R.Right - R.Left;
          FBufferHeight := R.Bottom - R.Top;
          GetMem(FBuffer, FBufferWidth * FBufferHeight * 4);
        end;
        vBitmap := TBitmap.Create;
        try
          vBitmap.PixelFormat := pf32bit;
          SetSizeBmp(vBitmap, FBufferWidth, FBufferHeight);
          vBitmap.Canvas.Brush := Brush;
          vBitmap.Canvas.FillRect(Rect(0, 0, vBitmap.Width, vBitmap.Height));
          vBitmap.Canvas.StretchDraw(GetPictureRect, FPicture.Graphic);
{$IFDEF LCLGTK2}
          for I := 0 to vBitmap.Height - 1 do
            System.Move(vBitmap.ScanLine[vBitmap.Height - I - 1]^, PByteArray(FBuffer)^[I * FBufferWidth * 4], FBufferWidth * 4);
{$ELSE}
          for I := 0 to vBitmap.Height - 1 do
            System.Move(vBitmap.ScanLine[I]^, PByteArray(FBuffer)^[I * FBufferWidth * 4], FBufferWidth * 4);
{$ENDIF}
          FBufferSaved := True;
        finally
          vBitmap.Free;
        end;
      end;
{$IFDEF LCLGTK2}
      vTmpDC := CreateCompatibleDC(0);
      vHandle := SelectObject(vTmpDC, CreateBitmap(FBufferWidth, FBufferHeight, 1, 32, FBuffer));
      BitBlt(vDC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, vTmpDC, 0, 0, SRCCOPY);
      DeleteObject(SelectObject(vTmpDC, vHandle));
      DeleteDC(vTmpDC);
{$ELSE}
      MKBIH32(vBI.bmiHeader, FBufferWidth, FBufferHeight);
{$IFDEF MSWINDOWS}
      SetDIBitsToDevice(ADC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, 0, 0, 0, FBufferHeight, FBuffer, vBI, DIB_RGB_COLORS);
{$ELSE}
      StretchDIBits(vDC, 0, 0, R.Right - R.Left, R.Bottom - R.Top, 0, 0,
            FBufferWidth, FBufferHeight, FBuffer, vBI, DIB_RGB_COLORS, SRCCOPY);
{$ENDIF}
{$ENDIF}
    end
    else
      Canvas.StretchDraw(GetPictureRect, FPicture.Graphic);
    FRectZoomingProcess := False;
    FNeedBroadcastPaintEvent := True;
    DoBroadcastPaintEvent(ADC);
    PictureRectChanged := False;
    PictureChanged := False;
  end;

begin
  FTool.UpdateImg;
  vDC := Canvas.Handle;
  DrawScrollBarsJoint(vDC);
{$IFNDEF SG_THREAD_DRAW}
  if FUpdateCount = 0 then
    DoCustomPaint(vDC);
{$ELSE}
  FNeedBroadcastPaintEvent := True;
  DoBroadcastPaintEvent(vDC);
{$ENDIF}
end;

function TsgDrawingNavigator.PerformBroadcastEvents(const AMsg: Cardinal;
  const AWParam: WPARAM; const  ALParam: LPARAM): LRESULT;
var
  Message: TMessage;
  vPRect: PFRect;
begin
  Message.Msg := AMsg;
  Message.WParam := AWParam;
  Message.LParam := ALParam;
  Message.Result := 0;
  BroadcastClients(Message);
  Result := Message.Result;
  if Result = 0 then
    case AMsg of
      WM_PAINT:
        if Assigned(FOnPaint) then
          FOnPaint(Self);
      WM_SCALECHANGING:
        if Assigned(FOnChangeScale) then
          FOnChangeScale(Self);
      WM_PICTUREMOVE:
        if Assigned(FOnPictureMove) then
          FOnPictureMove(Self);
      WM_PICTURECHANGE:
        if Assigned(FOnPictureChange) then
          FOnPictureChange(Self);
      WM_GETIMAGEEXTENTS:
        if Assigned(FOnGetImageExtents) then
        begin
          vPRect := PFRect(Message.WParam);
          FOnGetImageExtents(vPRect^.Left, vPRect^.Top, vPRect^.Z1,
            vPRect^.Right, vPRect^.Bottom, vPRect^.Z2);
        end;
    end;
end;

procedure TsgDrawingNavigator.Progress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string{$IFDEF SGFPC}; var Continue: Boolean{$ENDIF});
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg{$IFDEF SGFPC}, Continue{$ENDIF});
end;

procedure TsgDrawingNavigator.Resize;
begin
  inherited Resize;
  if Assigned(FOrbit3D) then
    FOrbit3D.BoundsRect := ClientRect;
  FBufferSaved := False;
  ClearBuffer;
  DoChangeScrolls;
end;

function TsgDrawingNavigator.Rotate(Axis: TsgAxes; Angle: Extended): Boolean;
begin
  try
    Result := FDrawRect.Rotate(Axis, (2 * Ord(Axis <> axisZ) - 1) * Angle);
    FLastViewName := sUserView;
  except
    Result := False;
  end;
end;

function TsgDrawingNavigator.Rotate(const APitch, ATurn, ARoll: Extended): Boolean;
begin
  try
    Result := FDrawRect.Rotate(APitch, ATurn, -ARoll);
    FLastViewName := sUserView;
  except
    Result := False;
  end;
end;

procedure TsgDrawingNavigator.RotToView(const A3DView: TsgDXFViewDirection);
begin
  if Assigned(FTool.Img) then
  begin
    FTool.Img.RotToView(A3DView);
    FLastViewName := GetViewDirectionName(A3DView);
  end;
end;

{$IFNDEF SG_MDI}
type
  TCustomFormAccess = class(TCustomForm);
{$ENDIF}
procedure TsgDrawingNavigator.SetParent(AParent: TWinControl);
{$IFNDEF SG_MDI}
var
  vForm: TCustomFormAccess;
{$ENDIF}
begin
  inherited SetParent(AParent);
  if Assigned(AParent) then
  begin
    FVertScrollBar.Parent := Self;
    FHorzScrollBar.Parent := Self;
  end;
  if Assigned(AParent) then
  begin
{$IFNDEF SG_MDI}
    vForm := TCustomFormAccess(GetParentForm(Self{$IFDEF SGDEL_2005}, False{$ENDIF}));
    if Assigned(vForm) then
      SysMenuIconsVisible := vForm.FormStyle = fsMDIChild;
{$ELSE}
    SysMenuIconsVisible := True;
{$ENDIF}
  end;
end;

function TsgDrawingNavigator.ShowRectConvertUnits(ARect: TFRect;
  const AConvertUnits: Boolean): Boolean;
var
  vSize, vCenter: TF2DPoint;
begin
  Result := False;
  if Assigned(FTool.Img) then
  begin
    if AConvertUnits then
    begin
      ARect.TopLeft := FTool.ConvertUnits(ARect.TopLeft);
      ARect.BottomRight := FTool.ConvertUnits(ARect.BottomRight);
    end;
    ARect := GetRealBox(ARect, FDrawRect.Matrix^);
    vCenter := SplitRect(ARect, vSize);
    Result := FDrawRect.Zoom(Rect2RectF(ClientRect), vCenter, vSize);
  end;
end;

function TsgDrawingNavigator.ViewRect: TRect;
begin
  Result := ClientRect;
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FHorzScrollBar) and Assigned(FHorzScrollBar.Parent)
      and FHorzScrollBar.Visible then
      Result.Bottom := Result.Bottom - FHorzScrollBar.Height;
    if Assigned(FVertScrollBar) and Assigned(FVertScrollBar.Parent)
      and FVertScrollBar.Visible then
      Result.Right := Result.Right - FVertScrollBar.Width;
  end;
end;
{$IFDEF SGDEL_5}
procedure TsgDrawingNavigator.WMContextMenu(var Message: TWMContextMenu);
var
  vPoint, vTmpPoint1, vTmpPoint2: TPoint;
  vHandled: Boolean;
  vPopupMenu: TPopupMenu;

  function InvalidPoint(const APoint: TPoint): Boolean;
  begin
    Result := (APoint.X = -1) and (APoint.Y = -1);
  end;

begin
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then
    inherited
  else
  begin
    vHandled := False;
    vPoint.X := Message.XPos;
    vPoint.Y := Message.YPos;
    if InvalidPoint(vPoint) then
      vTmpPoint1 := vPoint
    else
    begin
      vTmpPoint1 := ScreenToClient(vPoint);
      if not PtInRect(ClientRect, vTmpPoint1) then
      begin
        inherited;
        vHandled := True;
      end;
    end;
    if not vHandled then
    begin
      DoContextPopup(vTmpPoint1, vHandled);
      Message.Result := Ord(vHandled);
      if not vHandled then
      begin
        vPopupMenu := GetPopupMenu;
        vTmpPoint2 := ScreenToClient(vPoint);
        if (vPopupMenu <> nil) and vPopupMenu.AutoPopup then
        begin
{$IFNDEF SGFPC}
          SendCancelMode(nil);
{$ENDIF}
          vPopupMenu.PopupComponent := Self;
          if InvalidPoint(vPoint) then
            vPoint := ClientToScreen(Point(0, 0));
          if (vTmpPoint2.X = FMouseDownPt.X)
            and (vTmpPoint2.Y = FMouseDownPt.Y) then
          begin
            MouseCapture := False;
            Cursor := crArrow;
            vPopupMenu.Popup(vPoint.X, vPoint.Y);
            if GetCursorPos(vPoint) and PtInRect(ClientRect, ScreenToClient(vPoint)) then
              UpdateCursor(TCursor(cnstCursorCross));
            FMouseDownPt := Point(-1, -1);
          end;
          Message.Result := 1;
        end;
        if Message.Result = 0 then
          inherited;
      end;
    end;
  end;
end;
{$ENDIF}

procedure TsgDrawingNavigator.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
{$IFDEF SGFPC}
  Include(FWinControlFlags, wcfEraseBackground);
{$ENDIF}
  inherited;
{$IFDEF SGFPC}
  Exclude(FWinControlFlags, wcfEraseBackground);
{$ENDIF}
end;

procedure TsgDrawingNavigator.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  StopMouseMoveOperationsHandler(TMessage(Message));
end;

procedure TsgDrawingNavigator.WMMButtonUp(var Message: TWMMButtonUp);
begin
  inherited;
  StopMouseMoveOperationsHandler(TMessage(Message));
end;

procedure TsgDrawingNavigator.CNKeyDown(var Message: TWMKeyDown);

  procedure MoveRect(const ADX, ADY: Double);
  begin
    MovePictureRect2D(ADX, ADY);
    Message.Result := 1;
  end;

var
  R: TFRect;
  vShiftState: TShiftState;
begin
  vShiftState := KeyDataToShiftState(Message.KeyData);
  if vShiftState = [] then
  begin
    case TWMKey(Message).CharCode of
      0:
        Message.Result := 1;
      VK_UP:
        MoveRect(0, FVertScrollBar.SmallChange);
      VK_DOWN:
        MoveRect(0, -Integer(FVertScrollBar.SmallChange));
      VK_LEFT:
        MoveRect(FHorzScrollBar.SmallChange, 0);
      VK_RIGHT:
        MoveRect(-Integer(FHorzScrollBar.SmallChange), 0);
      VK_PRIOR:
        if GetKeyState(VK_CONTROL) < 0 then
          MoveRect(0, FVertScrollBar.LargeChange);
      VK_NEXT:
        if GetKeyState(VK_CONTROL) < 0 then
          MoveRect(0, -FVertScrollBar.LargeChange);
      VK_HOME:
        begin
          R := FDrawRect.FRect;
          MoveRect(-R.Left, -R.Bottom);
        end;
      VK_END:
        begin
          R := FDrawRect.FRect;
          MoveRect(ClientWidth - R.Right, ClientHeight - R.Top);
        end;
      VK_ADD:
        begin
          AlterScale(2, False, Center);
          Message.Result := 1;
        end;
      VK_SUBTRACT:
        begin
          AlterScale(0.5, False, Center);
          Message.Result := 1;
        end;
//      VK_MULTIPLY:
//        begin
//          FitToSize;
//          Message.Result := 1;
//        end;
//implemented in the unit "Main.pas" action "actFittoWindow"
    end;
  end
  else
    if vShiftState = [ssCtrl] then
      case TWMKey(Message).CharCode of
        VK_PRIOR:
          MoveRect(0, FVertScrollBar.LargeChange);
        VK_NEXT:
          MoveRect(0, -FVertScrollBar.LargeChange);
      end;
  if Message.Result = 0 then
  begin
    inherited;
    Exit;
  end;
end;

procedure TsgDrawingNavigator.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TsgDrawingNavigator.WMRButtonUp(var Message: TWMRButtonUp);
begin
  inherited;
  StopMouseMoveOperationsHandler(TMessage(Message));
end;

procedure TsgDrawingNavigator.WMSetCursor(var Message: TWMSetCursor);
begin
  inherited;
end;

procedure TsgDrawingNavigator.WMZoomExtents(var Message: TMessage);
begin
  inherited;
  if Message.Result = 0 then
  begin
    if FTool.IsNotCustomDraw then
      FTool.Img.RefreshCurrentLayout;
    BroadcastClients(Message);
    if Message.Result = 0 then
      Message.Result := Ord(FDrawRect.FitTo(ClientRect));
  end;
end;

function TsgDrawingNavigator.ZoomRect(ARect: TRect): Boolean;
begin
  Result := ZoomRectEx2D(Rect2DFromRect(ARect));
end;

function TsgDrawingNavigator.ZoomRectEx(ARect: TRect): Boolean;
begin
  Result := ZoomRectEx2D(Rect2DFromRect(ARect));
end;

function TsgDrawingNavigator.ZoomRectEx2D(const ARect: TF2DRect): Boolean;
var
  vCenter, vSize: TF2DPoint;
begin
  Include(FState, dnsZooming);
  try
    vCenter := SplitRect(ARect, vSize);
    Result := FDrawRect.Zoom(Rect2RectF(ClientRect), vCenter, vSize);
  finally
    Exclude(FState, dnsZooming);
  end;
end;

constructor TsgDrawingNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawRect := TsgDrawRect.Create;
  FDrawRect.OnChanged := DrawRectChanged;
  FDrawRect.OnChanging := DrawRectChanging;
  FClients := TList.Create;
  FControlToolProps := TsgNavigatorToolProps.Create;
  TsgNavigatorToolProps(FControlToolProps).FNavigator := Self;
  Brush.Style := bsClear;
  FAppActive := True;
  CreateHorScroll;
  CreateVertScroll;
  Cursor := TCursor(cnstCursorCross);
  FCenterAlignment := False;
  FHorzScrollBarVisibility := True;
  FMouseDragging := True;
  FMouseMoveZoomingProcess := False;
  FMouseScaling := False;
  FMoveByMouse := False;
  CreateOrbit3D(GetOrbit3DClass);
  FPicture := TsgPicture.Create(Self);
  FPicture.Graphic := nil;
  FPicture.OnChange := DoPictureChange;
  FPicture.OnProgress := Progress;
  FRectangle := TsgRectangle.Create(Self);
  FRectZooming := True;
  FRectZoomingProcess := False;
  FScalingOnMouseWheel := True;
  CreateSnap;
  FState := [];
  FStretch := False;
  FTool := TsgControlTool.Create(FControlToolProps);
  FTmpGraphic := nil;
  FVertScrollBarVisibility := True;
  FVPortOpened := False;
  FWidthInPixels := 0.0;
  FZoomChangeByMouseMove := 1.1;
  FZoomInChange := cnstScales[False];
  FZoomOutChange := cnstScales[True];
  FLastViewName := cnstDefaultViewsName[vdDefault];
{$IFNDEF SGFPC}
  Application.HookMainWindow(ApplicationHook);
{$ENDIF}
  FLoadedFromConverter := False;
{$IFNDEF SG_NON_WIN_PLATFORM}
  FMaxDim := iMaxNonWinNTDim;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
{$ENDIF}
    FMaxDim := iMaxWinNTDim;
end;

destructor TsgDrawingNavigator.Destroy;
begin
{$IFNDEF SGFPC}
  Application.UnhookMainWindow(ApplicationHook);
{$ENDIF}
  ClearBuffer;
  FActualGraphic.Free;
  FRectangle.Free;
  FSnapControl.Free;
  FreeAndNil(FSysMenuIcons);
  FTool.Free;
  FPicture.Free;
  if Assigned(FOrbit3D) then
  begin
    RemoveClient(FOrbit3D);
    FOrbit3D.Free;
  end;
  FControlToolProps.Free;
  FClients.Free;
  FDrawRect.Free;
  inherited Destroy;
end;

function TsgDrawingNavigator.AddClient(AClient: TObject): Integer;
begin
  Result := FClients.IndexOf(AClient);
  if Result < 0 then
    Result := FClients.Add(AClient);
end;

function TsgDrawingNavigator.AlterScale(AFactor: Double;
  AIsAbsolute: Boolean; APosition: TPoint): Boolean;
begin
  Result := False;
  if AFactor > fAccuracy then
  begin
    if AIsAbsolute then
      AFactor := AFactor / Scale;
    Include(FState, dnsZooming);
    try
      Result := FDrawRect.Scale(AFactor, APosition);
    finally
      Exclude(FState, dnsZooming);
    end;
  end;
end;

function TsgDrawingNavigator.BeginUpdate: Integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

procedure TsgDrawingNavigator.CallPreviousView;
var
  vLayout: TsgDXFLayoutAccess;
begin
  if Assigned(FTool.Img) and not (dnsSetView in FState) then
  begin
    Include(FState, dnsSetView);
    try
      vLayout := TsgDXFLayoutAccess(FTool.Img.CurrentLayout);
      if vLayout.View.PopView(FDrawrect.Matrix) then
        TsgDrawRectAcces(FDrawrect).DoChanged;
    finally
      Exclude(FState, dnsSetView);
    end;
  end;
end;

function TsgDrawingNavigator.Center: TPoint;
begin
  Result := GetRectCenter(ClientRect);
end;

procedure TsgDrawingNavigator.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_GROUP;
end;

procedure TsgDrawingNavigator.CreateSnap;
begin
  FSnapControl := TSnapControl.Create(FConstants);
  FSnapControl.ControlProps := FControlToolProps;
end;

function TsgDrawingNavigator.CreateSnapShotBitmap: Graphics.TBitmap;
var
  vDoubleBuffered: Boolean;
{$IFDEF SGFPC}
  Intf: TLazIntfImage;
  NewBitmap, NewMask: HBITMAP;
{$ENDIF}
begin
  vDoubleBuffered := DoubleBuffered;
  try
    if not DoubleBuffered then
    begin
      DoubleBuffered := True;
      Repaint;
    end;
    Result := TBitmap.Create;
{$IFNDEF SG_NON_WIN_PLATFORM}
    Result.PixelFormat := pf32bit;
    Result.Width := FBufferWidth;
    Result.Height := FBufferHeight;
    Perform(WM_ERASEBKGND, Result.Canvas.Handle, Result.Canvas.Handle);
    Perform(WM_PAINT, Result.Canvas.Handle, 0);
{$ENDIF}
{$IFDEF SGFPC}
    Intf := Result.CreateIntfImage;
    try
      Intf.CreateBitmaps(NewBitmap, NewMask, True);
      Result.Handle := NewBitmap;
    finally
      Intf.Free;
    end;
{$ENDIF}
  finally
    DoubleBuffered := vDoubleBuffered;
  end;
end;

function TsgDrawingNavigator.Empty: Boolean;
begin
  Result := (FPicture = nil) or (FPicture.Graphic = nil) or
    FPicture.Graphic.Empty;
end;

function TsgDrawingNavigator.EndUpdate: Integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Result := FUpdateCount;
{$IFNDEF SG_THREAD_DRAW}
  if (FUpdateCount = 0) and not (csCustomPaint in ControlState) then
  begin
    DoRepaintControl([]);
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

function TsgDrawingNavigator.FinishEntity(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
end;

procedure TsgDrawingNavigator.FitToSize;
begin
  BeginUpdate;
  try
    Perform(WM_ZOOM_EXTENTS, 0, 0);
  finally
    EndUpdate;
  end;
end;

function TsgDrawingNavigator.GetDrawingCoords(ACoordX, ACoordY: Integer;
  var AUnits: string): TFPoint;
var
  vTmpPoint: TFPoint;
begin
  Result := CoordinateConvertion(ACoordX, ACoordY, vTmpPoint);
{  // previos version(6.1)
  vPoint1 := CoordinateConvertion(ACoordX, ACoordY, vPoint2);
  Result := ConvertUnits(vPoint1, AUnits);
}
end;

function TsgDrawingNavigator.GetDrawingInternalCoords(ACoordX,
  ACoordY: Integer): TFPoint;
var
  vTmpPoint: TFPoint;
begin
  Result := CoordinateConvertion(ACoordX, ACoordY, vTmpPoint);
end;

function TsgDrawingNavigator.GetDrawingInternalCoordsWithoutSnap(ACoordX,
  ACoordY: Integer): TFPoint;
begin
  Result := ClientToWorld(ACoordX, ACoordY);
end;

function TsgDrawingNavigator.GetDrawingUCSCoords(ACoordX, ACoordY: Integer;
  var AUnits: string): TFPoint;
begin
  CoordinateConvertion(ACoordX, ACoordY, Result);
{  // previos version(6.1)
  CoordinateConvertion(ACoordX, ACoordY, vPoint);
  Result := ConvertUnits(vPoint, AUnits);
}
end;

function TsgDrawingNavigator.GetOnEnterClipRect: TNotifyEvent;
begin
  Result := FRectangle.OnEnterRect;
end;

function TsgDrawingNavigator.GetEntity(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
end;

function TsgDrawingNavigator.GetHasTriangledMesh: Boolean;
begin
  Result := False;
end;

function TsgDrawingNavigator.GetRealPoint(ACoordX, ACoordY: Double;
  var APointInUCS: TFPoint): TFPoint;
begin
  Result := ClientToWorld(ACoordX, ACoordY, @APointInUCS);
end;

function TsgDrawingNavigator.HasPreviousView: Boolean;
begin
  Result := False;
  if Assigned(FTool.Img) and Assigned(FTool.Img.CurrentLayout) then
    Result := TsgDXFLayoutAccess(FTool.Img.CurrentLayout).View.HasView;
end;

procedure TsgDrawingNavigator.ImgLock;
begin
  Inc(FUpdateCount);
end;

procedure TsgDrawingNavigator.ImgUnLock;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

procedure TsgDrawingNavigator.Invalidate;
begin
  if not IsImgLocked then
    inherited Invalidate;
end;

type
  TsgDXFConverterAccess = class(TsgDXFConverter);

procedure TsgDrawingNavigator.LoadFromConverter(const AConverter: TsgDXFConverter;
  const ACADImage: TsgCADImage);
begin
  AConverter.AutoInsert := FAutoInsert;
  AConverter.Params := InitializeIterateParams;
  TsgDXFConverterAccess(AConverter).GetModelLayout.Iterate(AConverter, GetEntity,
    FinishEntity);
  FLoadedFromConverter := True;
  //Converter.Iterate(GetEntity, nil, FIterateParam);
end;

procedure TsgDrawingNavigator.LoadFromFile(const AFileName: string);
begin
  BeginUpdate;
  try
    Clear;
    FLoadedFromConverter := False;
    FPicture.LoadFromFile(AFileName);
  finally
    EndUpdate;
  end;
end;

procedure TsgDrawingNavigator.LoadView(const AMatrix: TFMatrix;
  const APostMessage: Boolean = True);
begin
  PictureRectChanged := True;
  FDrawRect.Matrix^ := AMatrix;
  TsgDrawRectAcces(FDrawRect).DoChanged;
end;

procedure TsgDrawingNavigator.LockChangeVisibleArea;
begin
  FLockChangeVisibleArea := True;
end;

procedure TsgDrawingNavigator.MovePictureRect(const ADeltaX, ADeltaY: Integer);
begin
  MovePictureRect2D(ADeltaX, ADeltaY);
end;

function TsgDrawingNavigator.MovePictureRect2D(const ADeltaX, ADeltaY: Double): Boolean;
begin
  Result := False;
  if not (dnsMoving in FState) and ((ADeltaX <> 0) or (ADeltaY <> 0)) then
  try
    Include(FState, dnsMoving);
    Result := FDrawRect.Offset(ADeltaX, ADeltaY);
  finally
    Exclude(FState, dnsMoving);
  end;
end;

function TsgDrawingNavigator.RemoveClient(AClient: TObject): Integer;
begin
  Result := FClients.Remove(AClient);
end;

function TsgDrawingNavigator.ShowPoint(const APoint: TFPoint;
  AScale: Double): Boolean;
var
  vViewCtrl: TsgDrawRectAcces;
  vCurViewScale, vNewViewScale: Extended;
begin
  Result := False;
  vViewCtrl := TsgDrawRectAcces(FDrawRect);
  vCurViewScale := Power(Abs(vViewCtrl.Determinant3D), 1/3);
  if AScale < 0 then
    vNewViewScale := vCurViewScale
  else
    vNewViewScale := vCurViewScale * AScale/Scale;
  if not Empty then
    Result := vViewCtrl.Load(vViewCtrl.RotMatrix, vNewViewScale, MakeFPointFromPoint(Center), APoint);
end;

function TsgDrawingNavigator.ShowRect(ARect: TFRect): Boolean;
begin
  Result := ShowRectConvertUnits(ARect, True);
end;

procedure TsgDrawingNavigator.WndProc(var Message: TMessage);
begin
  if (TMessage(Message).Msg <> WM_PAINT) and (TMessage(Message).Msg <> WM_ZOOM_EXTENTS) then
    BroadcastClients(Message);
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and CanFocus and (not Focused) then
        {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ELSE}LCLIntf.{$ENDIF}SetFocus(Handle);
  end;
  inherited WndProc(Message);
  case Message.Msg of
    CM_COLORCHANGED:
      FBufferSaved := False;
  end;
end;

{ TsgPicture }

constructor TsgPicture.Create(const AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TsgPicture.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TsgNavigatorToolProps }

function TsgNavigatorToolProps.AddClient(AClient: TObject): Integer;
begin
  Result := FNavigator.AddClient(AClient);
end;

function TsgNavigatorToolProps.ClientToWorld(X, Y: Double;
  AUCSPoint: PFPoint = nil): TFPoint;
begin
  Result := FNavigator.ClientToWorld(X, Y, AUCSPoint);
end;

function TsgNavigatorToolProps.GetCanvas: TCanvas;
begin
  Result := FNavigator.Canvas;
end;

function TsgNavigatorToolProps.GetClipRect: TRect;
begin
  Result := FNavigator.ClipRectCoords
end;

function TsgNavigatorToolProps.GetGraphic: TGraphic;
begin
  Result := FNavigator.Picture.Graphic;
end;

function TsgNavigatorToolProps.GetOwner: TPersistent;
begin
  Result := FNavigator;
end;

function TsgNavigatorToolProps.GetClipBox: TFRect;
begin
  Result := FNavigator.Rectangle.Box;
end;

function TsgNavigatorToolProps.GetViewRect: TRect;
begin
  Result := FNavigator.ViewRect;
end;

function TsgNavigatorToolProps.GetViewRectMode: Boolean;
begin
  Result := FNavigator.ViewRectMode;
end;

procedure TsgNavigatorToolProps.Invalidate;
begin
  FNavigator.Invalidate;
end;

function TsgNavigatorToolProps.IsClipRectActive: Boolean;
begin
  Result := FNavigator.IsClipRectActive;
end;

function TsgNavigatorToolProps.RemoveClient(AClient: TObject): Integer;
begin
  Result := FNavigator.RemoveClient(AClient);
end;

function TsgNavigatorToolProps.ScreenToClient(P: TPoint): TPoint;
begin
  Result := FNavigator.ScreenToClient(P);
end;

function TsgNavigatorToolProps.WorldToClient(const APoint: TFPoint): TPoint;
begin
  Result := FNavigator.GetPoint(APoint);
end;

initialization
  { TODO: Load navigator cursors }
{$IFNDEF SG_NON_WIN_PLATFORM}
  Screen.Cursors[cnstCursorHand]  := LoadCursor(HInstance, 'HANDFLAT');
  Screen.Cursors[cnstCursorCross] := LoadCursor(HInstance, 'CROSS');
  Screen.Cursors[Ord(stateX) + 1] := LoadCursor(HInstance, 'X');
  Screen.Cursors[Ord(stateY) + 1] := LoadCursor(HInstance, 'Y');
  Screen.Cursors[Ord(stateZ) + 1] := LoadCursor(HInstance, 'Z');
  Screen.Cursors[Ord(stateXY)+ 1] := LoadCursor(HInstance, 'XY');
{$ENDIF}

finalization

end.

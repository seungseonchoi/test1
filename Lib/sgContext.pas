{************************************************************}
{        Delphi / Lazarus Cross Platform Extensions          }
{                                                            }
{        The proxy classes for rendering functions           }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools?                       }
{************************************************************}

unit sgContext;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_AGG2D}
  {$IFNDEF SG_USE_AGG2D_AS_GDI}
    {$DEFINE USE_SG_AGG2D}
  {$ENDIF}
{$ENDIF}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

interface

uses
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage, FPCanvas,
{$IFDEF USE_SG_AGG2D}
  Agg_LCL,
{$ENDIF}
{$ENDIF}
  Classes, SysUtils, Math,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, FMX.TextLayout,
  System.Generics.Collections, FMX.FontGlyphs, System.Messaging, FMX.Printer,
  {$IFDEF SG_FM_MACOS}FMX.Canvas.Mac, Macapi.CoreGraphics,{$ENDIF}
  {$IFDEF SG_FM_LINUX}FMX.FontGlyphs.Linux,{$ENDIF}
{$ELSE}
  Graphics,
  {$IFDEF MSWINDOWS}
   Windows,
  {$ENDIF}
{$ENDIF}
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ,// Winapi.GDIPUTIL,
{$ELSE}
  GDIPlus,
{$ENDIF}
{$ENDIF}
  sgConsts, sgFunction, sgLists, sgSelection
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF};

type
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
{$HPPEMIT 'typedef Gdiplus::ImageAttributes TGPImageAttributes;'}
{$ENDIF}
{$ENDIF}

{$IFNDEF SGFPC}
  TRegion = class;
{$ENDIF}

{$IFDEF SGFPC}
  THandle = {$IFNDEF MSWINDOWS}LCLType.{$ELSE}System.{$ENDIF}THandle;
{$ENDIF}

{$IFNDEF SGFPC}
  TRegion = class(TGraphicsObject)
  private
    function GetHandle: THandle;
    procedure SetHandle(const Value: THandle);
    procedure ReferenceNeeded;
  protected
    FHandle: THandle;
    procedure Changed; override;
    function GetClipRect: TRect;
    procedure SetClipRect(const Value: TRect);
  public
    function HandleAllocated: Boolean;{$IFDEF SG_FIREMONKEY} override;{$ENDIF}
    property Handle: THandle read GetHandle write SetHandle;
    property ClipRect: TRect read GetClipRect write SetClipRect;
  end;
{$ENDIF}

  TsgRegionStack = class(TsgObjectList)
  private
    FData: Pointer;
    function GetHandle: THandle;
    function GetTop: TRegion;
  protected
    procedure Notify(const Obj: TObject; Action: TListNotification); override;
  public
    destructor Destroy; override;
    function Pop: Integer;
    function Push(ARegion: TRegion): Integer;
    property Handle: THandle read GetHandle;
    property Top: TRegion read GetTop;
    function GetData(ARegion: TRegion; var ARects: PRect): Integer;
    function RectInRegion(const AContext: TObject; const ARect: TRect): Boolean;
    procedure FinalizeData;
  end;

  TsgRegion = class(TRegion)
  private
{$IFDEF SGFPC}
    FIsValid: Boolean;
{$ENDIF}
  protected
    procedure Changing; {$IFDEF SGFPC}override;{$ELSE}virtual;{$ENDIF}
{$IFDEF SGFPC}
    procedure Changed; override;
    procedure SetClipRect(value: TRect);
    function GetClipRect: TRect;
{$ENDIF}
    function Combine(ARegion: TRegion; AMode: Integer): Integer;
    function DoCombine(ARegion: TRegion; AMode: Integer): Integer;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HandleAllocated: Boolean;{$IFDEF SG_FIREMONKEY} override;{$ENDIF}
    procedure FreeReference;

    procedure Intersect(ARegion: TRegion);
    procedure Exclude(ARegion: TRegion);
    procedure CopyFrom(ARegion: TRegion);
    procedure Union(ARegion: TRegion);
    procedure Substract(ARegion: TRegion);

    procedure SetPolygon(const APoints; ACount, AFillMode: Integer);
    procedure SetPolygonList(const AList: TsgBaseList; const AFillMode: Integer);

    function Paint(ACanvas: TCanvas): Integer;
    function Select(ACanvas: TCanvas): Integer;
    function Offset(ACanvas: TCanvas): TPoint; overload;
    procedure Offset(AOffset: TPoint); overload;
    function Update(ACanvas: TCanvas): Integer;
    function GetState: Integer;
{$IFDEF SGFPC}
    function IsValid: Boolean;
    property ClipRect: TRect read GetClipRect write SetClipRect;
{$ENDIF}
  end;

  TsgProxyBase2D = class(TsgProxyBase)
  private
  protected
    FPointTmp: TPoint;
    FIndex: Integer;
    FReadyPoints: TsgPointsListHelper;
    FIRect: PRect;
    FIntPoints: TsgIntegerList;
    FDotsPoints: TsgIntegerList;
    FCounts: TsgIntegerList;
    F2Counts: TsgIntegerList;
    F3Counts: TsgIntegerList;
    FPoly: TsgPointsListHelper;
    FMatrix: PFMatrix;
    function CreateList(const ACapacity: Integer = 16): TsgBaseList;
    function GetPoint(const APoint: TFPoint): TPoint;
    function GetFIRect: TRect;
    procedure SetFIRect(const ARect: TRect);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddOnePixel;

    procedure AddIntPoint(const APoint: TFPoint;
      const ACheckEqual: Boolean = False);
    procedure AddIntPointDirect(const APoint: TFPoint); overload;
    procedure AddIntPointDirect(const APoint: TPoint); overload;
    procedure AddIntPoint2(const APoint: TFPoint; const ADelta: Integer = 1);
    procedure AddIntFirstPoint(const ADelta: Integer = 1);
    procedure AddIntLastPoint(const ADelta: Integer = 1);
    function AddIntSegment(const APoint1, APoint2: TFPoint): Boolean;

    procedure AddPolyPoint(const APoint: TFPoint);
    procedure AddPolyFirstPoint(const ADelta: Integer = 1);
    procedure AddPolyLastPoint(const ADelta: Integer = 1);
    procedure AddPolyRect2D(const ARect: TFRect);
    function AddPolySegment(const APoint1, APoint2: TFPoint): Boolean;

    procedure IncRectRightBottom;
    procedure BoxPoint(const APoint: TPoint);
    procedure SetMatrix(const AMatrix: PFMatrix);
    procedure SetRectPoint(const AIndex: Integer; const APoint: TPoint);
    property Rect: TRect read GetFIRect write SetFIRect;
    property ReadyPoints: TsgPointsListHelper read FReadyPoints;
    property IndexPoints: Integer read FIndex write FIndex;
    property Counts: TsgIntegerList read FCounts;
    property Counts2: TsgIntegerList read F2Counts;
    property Counts3: TsgIntegerList read F3Counts;
    property DotsPoints: TsgIntegerList read FDotsPoints;
    property IntPoints: TsgIntegerList read FIntPoints;
    property Poly: TsgPointsListHelper read FPoly;
  end;

  TsgProxyExportCustom = class(TsgProxyBase2D)
  private
    FActive: Boolean;
    FSupportColorToLineWeight: Boolean;
    FLineTypeMode: Integer;
  protected
  public
    procedure Clear; virtual; abstract;
    procedure DoAnnotation(const AEnt: TObject; const AParams: TsgExpAnnotParam); virtual; abstract;
    procedure DoClipRgn(P: PRect; Count: Integer); virtual; abstract;
    procedure DoColor(AColor: TColor; AColorType: TsgColorType); virtual; abstract;
    procedure DoFillRgn(P: PRect; Count: Integer); virtual; abstract;
    procedure DoImageUV(const APoint1, APoint2, APoint3, APoint4: TPoint; AImage: TGraphic); virtual; abstract;
    procedure DoPenWidth(AWidth: Double); virtual; abstract;
    procedure DoProgress; virtual; abstract;
    procedure DoStyle(AStyle: Integer; AColorType: TsgColorType); virtual; abstract;
    procedure DoText(ATextType: TsgTextType; X, Y: Integer; Options: Longint;
      Rect: PRect; StrWideChar: PWideChar; StrChar: PAnsiChar; Count: Integer;
      const Dx: PInteger); virtual; abstract;
    property Active: Boolean read FActive write FActive;
    property SupportColorToLineWeight: Boolean read FSupportColorToLineWeight
      write FSupportColorToLineWeight;
    property LineTypeMode: Integer read FLineTypeMode write FLineTypeMode;
  end;

  { TsgContext }

  TsgContext = class(TsgProxyBase2D)
  private
{$IFDEF SG_FIREMONKEY}[Weak]{$ENDIF}FCanvas: {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ELSE}TCanvas{$ENDIF};
{$IFDEF SG_FIREMONKEY}
    FCanvasSaveState: TCanvasSaveState;
    FFontChanged: TNotifyEvent;
    FLayout: TTextLayout;
    FMessageID: Integer;
    FBmpTiles: TList<FMX.Graphics.TBitmap>;
    FBmpColors: TList<TAlphaColor>;
{$ENDIF}
{$IFDEF SG_DELPHI_VCL}
    FObjectType: Integer;
    FSize: TSize;
{$ENDIF}
    procedure SetCanvas(const Value: {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ELSE}TCanvas{$ENDIF});
    function GetPixel(X, Y: Integer): TColor;
{$IFDEF SG_FIREMONKEY}
    procedure FontChanged(Sender: TObject);
    procedure CanvasDestroyListener(const Sender: TObject; const M: TMessage);
    function GetTile(const AColor: TAlphaColor; const AStyle: TBrushStyle): FMX.Graphics.TBitmap;
{$ENDIF}
  protected
    function GetBrushColor: TColor; virtual;
    function GetBrushStyle: TBrushStyle; virtual;
    function GetFontColor: TColor;
    function GetFontStyle: TFontStyles;
    function GetPenColor: TColor; virtual;
    function GetPenStyle: TPenStyle; virtual;
    function GetPenWidth: Single; virtual;
    function GetFontHeight: Single;
    function GetPixelsPerInch: Integer;
    function GetPenMode: TPenMode;
    function GetCharSet: TFontCharset;
    function GetSize: TPoint; override;
    procedure SetBrushColor(const Value: TColor); virtual;
    procedure SetBrushStyle(const Value: TBrushStyle); virtual;
    procedure SetFontColor(const Value: TColor);
{$IFDEF SG_FIREMONKEY}
    procedure SetFillGradient(AColor, AColor1: TColor; const AStPt, AEndPt: TFPoint;
      AStyle: TGradientStyle);
{$ENDIF}
    procedure SetPenColor(const Value: TColor); virtual;
    procedure SetPenStyle(const Value: TPenStyle); virtual;
    procedure SetPenWidth(const Value: Single); virtual;
    procedure SetFontHeight(const Value: Single);
    procedure SetPenMode(const Value: TPenMode); virtual;
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetCharSet(const Value: TFontCharset);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DoEntityProcBegin(const Sender: TObject); override;
    procedure DoEntityProcEnd(const Sender: TObject); override;

    procedure DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer; const Arc: PsgArc); override;
    procedure DoFont(AFont: TFont); override;
    procedure DoImage(const R: TRect; AImage: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TBitmap{$ENDIF}); override;
    procedure DoPenGeometric(AStyle: DWORD; const APenColor, APenWidth: Integer); override;
    procedure DoPenMiterLimit(ANewLimit: Single); override;
    procedure DoPixel(APoint: TPoint; AColor: TColor); override;
    procedure DoPolygon(Points: PPoint; Count: Integer); override;
    procedure DoPolyline(Points: PPoint; Count: Integer); override;
    procedure DoPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
    procedure DoPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
{$IFDEF SG_USE_AGG2D_AS_GDI}
    procedure DoPolyPoly(const Points: array of TPointF; const Counts: array of Integer;
      AFillMode: Integer = 2; AUseFont: Boolean = False); override;
{$ENDIF}
    function DoRestoreDC(const AIndex: Integer = 0): Boolean; override;
    function DoSaveDC: Integer; override;
    procedure DoTextOutAW(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam;
      ACount: Longint = 0); override;
    function DoSetPolyFillMode(PolyFillMode: Integer): Integer; override;

    procedure BitBlt(const X, Y, Width, Height, SX, SY, ARop: Integer; const ABitMap: {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap);
    function ClipRect: TRect;
    function CreateGDIPGraphic:{$IFDEF SG_USEGDIPLUS}TGPGraphics{$ELSE}TObject{$ENDIF};
    procedure Ellipse(const X1, Y1, X2, Y2: Integer);
    procedure FillRect(const Rect: TRect);
    procedure FillRegion(const ARegion: TRegion);
    procedure FixLastPointsCount;
    function IsRectInRegion(const ARegion: TRegion; const ARect: TRect): Boolean;
    function GetDC: THandle;
    function GetDeviceCaps(const AType: Integer): Integer;
  {$IFDEF SG_FIREMONKEY}
    function GetMMToPixel: Double;
  {$ENDIF}
    function GetStretchBltMode: Integer;
    procedure GradientFill(const ARect: TRect; const AColor1, AColor2: Integer;
      const APoly: TsgPointsListHelper; const AGradientPositions: PFRect = nil;
      AStyle: Byte = 0; ACounts: TsgIntegerList = nil); //AStyle: TGradientStyle for Firemonkey
    procedure LinearGradFill(const AMetaPath; const ARect: TRect;
        const APositions, AColors; const AColorsCount: Integer; AIsVertical: Boolean);
    procedure RadialGradFill(const AMetaPath; x, y, r: Double; const ARect: TRect;
        const APositions, AColors; const AColorsCount: Integer);
    procedure IntersectClipRect(X1, Y1, X2, Y2: Integer);
    function IsMetafileCanvas: Boolean;
    procedure Line(const X1, Y1, X2, Y2: Integer);
    function PatBlt(const X, Y, Width, Height: Integer; Rop: DWORD): Boolean;
    procedure RealizePalette;
    procedure Rectangle(const X1, Y1, X2, Y2: Integer);
    function SelectPalette(APalette: THandle; ForceBackground: Boolean): THandle;
    function SetGraphicsMode(const AMode: Integer): Integer;
    function SetStretchBltMode(StretchMode: Integer): Integer;
    function SetTextAlign(const AFlags: DWORD): DWORD;
    procedure BeginPath;
    procedure CloseFigure;
    procedure EndPath;
    procedure StrokePath;

    function RegionPaint(const ARegion: TsgRegion): Integer;
    function RegionSelect(const ARegion: TsgRegion): Integer;
    function RegionOffset(const ARegion: TsgRegion): TPoint;
    function RegionUpdate(const ARegion: TsgRegion): Integer;

    function DoExtTextOut(X, Y: Integer; Options: Longint; Rect: PRect; Str: PWideChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;
    function DoExtTextOutA(X, Y: Integer; Options: Longint; Rect: PRect; Str: PAnsiChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;
    function DoExtTextOutW(X, Y: Integer; Options: Longint; Rect: PRect; Str: PWideChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;

    function BeginScene: Boolean; override;
    procedure EndScene; override;

    property Canvas: {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ELSE}TCanvas{$ENDIF} read FCanvas write SetCanvas;
    property CharSet: TFontCharset read GetCharSet write SetCharSet;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property PenWidth: Single read GetPenWidth write SetPenWidth;
    property PenMode: TPenMode read GetPenMode write SetPenMode;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontHeight: Single read GetFontHeight write SetFontHeight;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property PixelsPerInch: Integer read GetPixelsPerInch;
    property Pixels[X, Y: Integer]: TColor read GetPixel;
  end;

  TsgContextSelection = class(TsgContext)
  private
    FSelectionMatrix: TsgSelectionMatrix;
  protected
    function GetBrushColor: TColor; override;
    function GetBrushStyle: TBrushStyle; override;
    function GetPenColor: TColor; override;
    function GetPenStyle: TPenStyle; override;
    function GetPenWidth: Single; override;
    procedure SetBrushColor(const Value: TColor); override;
    procedure SetBrushStyle(const Value: TBrushStyle); override;
    procedure SetPenColor(const Value: TColor); override;
    procedure SetPenStyle(const Value: TPenStyle); override;
    procedure SetPenWidth(const Value: Single); override;
  public
    procedure Changed;
    procedure DoPixel(APoint: TPoint; AColor: TColor); override;
    function BeginScene: Boolean; override;
    procedure EndScene; override;
    property Selection: TsgSelectionMatrix read FSelectionMatrix write FSelectionMatrix;
  end;


  TsgProxyExport = class(TsgProxyExportCustom)
  private
    FExpAnnotation: TsgExpAnnotation;
    FExpArc: TsgExpArc;
    FExpClipRgn: TsgExpClipRgn;
    FExpCloseFigure: TsgExpCloseFigure;
    FExpFillRgn: TsgExpFillRgn;
    FExpImage: TsgExpImage;
    FExpImageUV: TsgExpImageUV;
    FExpPixel: TsgExpPixel;
    FExpPolygon: TsgExpPolygon;
    FExpPolyline: TsgExpPolyline;
    FExpPolyPolygon: TsgExpPolyPolygon;
    FExpPolyPolyline: TsgExpPolyPolyline;
    FExpProgress: TsgExpProgress;
    FExpRestoreDC: TsgExpRestoreDC;
    FExpSaveDC: TsgExpSaveDC;
    FExpSetColor: TsgExpSetColor;
    FExpSetFont: TsgExpSetFont;
    FExpSetPenGeometric: TsgExpSetPenGeometric;
    FExpSetPenMiterLimit: TsgExpSetPenMiterLimit;
    FExpSetPenWidth: TsgExpSetPenWidth;
    FExpSetStyle: TsgExpSetStyle;
    FExpText: TsgExpText;
    FExpTextOut: TsgExpTextOut;
    FExpExportBlock: TsgExpExportBlock; // ÃÖ½Â¼± ¼öÁ¤
    FExpEntityProc: TsgExpObject;
    FExpPathDataProc: TsgExpPathData;
    FExpInitView: TsgExpInitView;
  protected
  public
    procedure Clear; override;
    procedure DoAnnotation(const AEnt: TObject; const AParams: TsgExpAnnotParam); override;
    procedure DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer; const Arc: PsgArc); override;
    procedure DoClipRgn(P: PRect; Count: Integer); override;
    procedure DoColor(AColor: TColor; AColorType: TsgColorType); override;
    procedure DoEntityProcBegin(const Sender: TObject); override;
    procedure DoEntityProcEnd(const Sender: TObject); override;
    procedure DoFillRgn(P: PRect; Count: Integer); override;
    procedure DoFont(AFont: TFont); override;
    procedure DoImage(const R: TRect; AImage: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TBitmap{$ENDIF}); override;
    procedure DoImageUV(const APoint1, APoint2, APoint3, APoint4: TPoint; AImage: TGraphic); override;
    procedure DoPenGeometric(AStyle: DWORD; const APenColor, APenWidth: Integer); override;
    procedure DoPenMiterLimit(ANewLimit: Single); override;
    procedure DoPenWidth(AWidth: Double); override;
    procedure DoPixel(APoint: TPoint; AColor: TColor); override;
    procedure DoPolygon(Points: PPoint; Count: Integer); override;
    procedure DoPolyline(Points: PPoint; Count: Integer); override;
    procedure DoPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
    procedure DoPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
    procedure DoProgress; override;
    function DoRestoreDC(const AIndex: Integer = 0): Boolean; override;
    function DoSaveDC: Integer; override;
    procedure DoStyle(AStyle: Integer; AColorType: TsgColorType); override;
    procedure DoText(ATextType: TsgTextType; X, Y: Integer; Options: Longint;
      Rect: PRect; StrWideChar: PWideChar; StrChar: PAnsiChar; Count: Integer;
      const Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger);override;
    procedure DoTextOutAW(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam;
      ACount: Longint = 0); override;
    procedure DoExportBlockAW(const blockString: string); override;   // ÃÖ½Â¼± ¼öÁ¤
    property ExpAnnotation: TsgExpAnnotation read FExpAnnotation write FExpAnnotation;
    property ExpArc: TsgExpArc read FExpArc write FExpArc;
    property ExpClipRgn: TsgExpClipRgn read FExpClipRgn write FExpClipRgn;
    property ExpCloseFigure: TsgExpCloseFigure read FExpCloseFigure write FExpCloseFigure;
    property ExpFillRgn: TsgExpFillRgn read FExpFillRgn write FExpFillRgn;
    property ExpImage: TsgExpImage read FExpImage write FExpImage;
    property ExpImageUV: TsgExpImageUV read FExpImageUV write FExpImageUV;
    property ExpPixel: TsgExpPixel read FExpPixel write FExpPixel;
    property ExpPolygon: TsgExpPolygon read FExpPolygon write FExpPolygon;
    property ExpPolyline: TsgExpPolyline read FExpPolyline write FExpPolyline;
    property ExpPolyPolygon: TsgExpPolyPolygon read FExpPolyPolygon write FExpPolyPolygon;
    property ExpPolyPolyline: TsgExpPolyPolyline read FExpPolyPolyline write FExpPolyPolyline;
    property ExpProgress: TsgExpProgress read FExpProgress write FExpProgress;
    property ExpRestoreDC: TsgExpRestoreDC read FExpRestoreDC write FExpRestoreDC;
    property ExpSaveDC: TsgExpSaveDC read FExpSaveDC write FExpSaveDC;
    property ExpSetColor: TsgExpSetColor read FExpSetColor write FExpSetColor;
    property ExpSetFont: TsgExpSetFont read FExpSetFont write FExpSetFont;
    property ExpSetPenGeometric: TsgExpSetPenGeometric read FExpSetPenGeometric write FExpSetPenGeometric;
    property ExpSetPenMiterLimit: TsgExpSetPenMiterLimit read FExpSetPenMiterLimit write FExpSetPenMiterLimit;
    property ExpSetPenWidth: TsgExpSetPenWidth read FExpSetPenWidth write FExpSetPenWidth;
    property ExpSetStyle: TsgExpSetStyle read FExpSetStyle write FExpSetStyle;
    property ExpText: TsgExpText read FExpText write FExpText;
    property ExpTextOut: TsgExpTextOut read FExpTextOut write FExpTextOut;
    property ExpExportBlock: TsgExpExportBlock read FExpExportBlock write FExpExportBlock;//ÃÖ½Â¼± ¼öÁ¤
    property ExpEntProc: TsgExpObject read FExpEntityProc write FExpEntityProc;
    property ExpPathData: TsgExpPathData read FExpPathDataProc write FExpPathDataProc;
    property ExpInitView: TsgExpInitView read FExpInitView write FExpInitView;
  end;

{$IFDEF SG_FIREMONKEY}
function IsPrinterCanvas(ACanvas: TCanvas): Boolean;
{$ENDIF}

implementation

{$IFDEF SG_FM_WINDOWS}
uses {Windows, }FMX.Platform.Win, Winapi.GDIPAPI, Winapi.GDIPOBJ;// Winapi.GDIPUTIL,
{$ELSE}
{$IFDEF SG_USE_AGG2D_AS_GDI}
uses sgAGG2DIntf2, sg_agg_fpimage;
{$ELSE}
{$IFDEF SG_DELPHI_VCL}
uses Printers;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF SG_LINUX}
const
  RGN_ERROR = {$IFDEF SGFPC}Region_Error{$ELSE}0{$ENDIF};
{$ENDIF}

{$IFNDEF SG_FIREMONKEY}
type
  TGradFill = function(DC: THandle; PV: PTriVertex; NumV: Integer; PMesh: Pointer; NMesh,Mode: Integer): LongBool; stdcall;
{$ENDIF}
{$IFDEF USE_SG_AGG2D}
type
  TFPCustomFontAccess = class(TFPCustomFont);
{$ENDIF}
type
  TCanvasAcceess = class(TCanvas);
{$IFDEF SG_FM_MACOS}
  TCanvasQuartzAccess = class(TCanvasQuartz);
{$ENDIF}

var
  Msimg32: THandle = 0;
{$IFNDEF SG_FIREMONKEY}
  GradientFillProc: TGradFill = nil;
{$ENDIF}

function Point(AX, AY: Integer): TPoint;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF SGDEL_XE2}
  Result := System.Types.Point(AX, AY);
  {$ELSE}
  {$IFDEF SGDEL_6}
  Result := Types.Point(AX, AY);
  {$ELSE}
  Result := Classes.Point(AX, AY);
  {$ENDIF}
{$ENDIF}
end;

{$IFNDEF SG_FIREMONKEY}
function GFStub(DC: THandle; PV: PTriVertex; NumV: Integer; PMesh: Pointer; NMesh,Mode: Integer): LongBool; stdcall;
var
  HBr: HBrush;
  R: TRect;
begin
  Result := NumV > 1;
  if not Result then Exit;
  R.TopLeft := Point(PV^.X, PV^.Y); Inc(PV);
  R.BottomRight := Point(PV^.X, PV^.Y); Dec(PV);
  HBr := CreateSolidBrush(PV^.Red and $FF00 shr 8 + PV^.Green and $FF00 + PV^.Blue and $FF00 shl 8);
  FillRect(DC,R,HBr);
  DeleteObject(HBr);
end;

procedure InitGradFill;
begin
{$IFDEF SGFPC}
  @GradientFillProc := @LCLIntf.GradientFill;
{$ELSE}
  GradientFillProc := GFStub;
  Msimg32 := LoadLibrary('msimg32.dll');
  if Msimg32 <> 0 then
    GradientFillProc := GetProcAddress(Msimg32,'GradientFill');
{$ENDIF}
end;
{$ENDIF}

function _GetRgnBox(RGN: HRGN; var p2: TRect): Integer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_GetRgnBox(RGN, p2);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}GetRgnBox(RGN, {$IFDEF SGFPC}@{$ENDIF}p2);
{$ENDIF}
end;

function _SelectClipRgn(DC: HDC; Region: HRGN): Integer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_SelectClipRgn(DC, Region);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}SelectClipRgn(DC, Region);
{$ENDIF}
end;

function _CreatePolygonRgn(const Points; Count, FillMode: Integer): HRGN;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_CreatePolygonRgn(Points, Count, FillMode);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}CreatePolygonRgn({$IFDEF SGFPC}@{$ENDIF}Points, Count, FillMode);
{$ENDIF}
end;

function _GetClipRgn(DC: HDC; rgn: HRGN): Integer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_GetClipRgn(DC, rgn);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}GetClipRgn(DC, rgn);
{$ENDIF}
end;

function _CombineRgn(p1, p2, p3: HRGN; p4: Integer): Integer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_CombineRgn(p1, p2, p3, p4);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}CombineRgn(p1, p2, p3, p4);
{$ENDIF}
end;

function _LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_LPtoDP(DC, Points, Count);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}LPtoDP(DC, Points, Count);
{$ENDIF}
end;

function _OffsetRgn(RGN: HRGN; XOffset, YOffset: Integer): Integer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_OffsetRgn(RGN, XOffset, YOffset);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}OffsetRgn(RGN, XOffset, YOffset);
{$ENDIF}
end;

function _PaintRgn(DC: HDC; RGN: HRGN): BOOL;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_PaintRgn(DC, RGN);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}PaintRgn(DC, RGN);
{$ENDIF}
end;

function _CreateRectRgn(p1, p2, p3, p4: Integer): HRGN;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_CreateRectRgn(p1, p2, p3, p4);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}CreateRectRgn(p1, p2, p3, p4);
{$ENDIF}
end;

function _SetRectRgn(Rgn: HRgn; X1, Y1, X2, Y2: Integer): BOOL;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_SetRectRgn(Rgn, X1, Y1, X2, Y2);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}SetRectRgn(Rgn, X1, Y1, X2, Y2);
{$ENDIF}
end;

function _CreateRectRgnIndirect(const p1: TRect): HRGN;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_CreateRectRgnIndirect(p1);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}CreateRectRgnIndirect(p1);
{$ENDIF}
end;

function _DeleteObject(p1: HGDIOBJ): BOOL;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := FMX_DeleteObject(p1);
{$ELSE}
  Result := {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFDEF MSWINDOWS}Windows.{$ENDIF}{$ENDIF}DeleteObject(p1);
{$ENDIF}
end;

{ TsgRegionStack }

destructor TsgRegionStack.Destroy;
begin
  FinalizeData;
  inherited Destroy;
end;

procedure TsgRegionStack.FinalizeData;
begin
  ReallocMem(FData, 0);
end;

function TsgRegionStack.GetData(ARegion: TRegion; var ARects: PRect): Integer;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vSize: Integer;
{$ENDIF}
begin
  Result := 0;
  ARects := nil;
{$IFNDEF SG_NON_WIN_PLATFORM}
  vSize := GetRegionData(ARegion.Handle, 0, nil);
  if vSize > 0 then
  begin
    ReallocMem(FData, vSize);
    if {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}GetRegionData(ARegion.Handle, vSize, PRgnData(FData)) > 0 then
    begin
      ARects := PRect(@PRgnData(FData)^.Buffer[0]);
      Result := PRgnData(FData)^.rdh.nCount;
    end
    else
      FinalizeData;
  end;
{$ENDIF}
end;

function TsgRegionStack.GetHandle: THandle;
var
  Region: TsgRegion;
begin
  if Count > 0 then
  begin
    Region := TsgRegion(Last);
    Result := Region.Handle;
  end
  else
    Result := 0;
end;

function TsgRegionStack.GetTop: TRegion;
begin
  if Count > 0 then
    Result := TRegion(Last)
  else
    Result := nil;
end;

procedure TsgRegionStack.Notify(const Obj: TObject; Action: TListNotification);
var
  vObj: TObject;
begin
  case Action of
    lnAdded:
      begin
        inherited Notify(Obj, Action);
      end;
    lnDeleted, lnExtracted:
      begin
        vObj := Obj;
        inherited Notify(Obj, Action);
        vObj.Free;
      end;
  end;
end;

function TsgRegionStack.Pop: Integer;
begin
  Result := Count;
  Dec(Result);
  if Result >= 0 then
    Delete(Result);
end;

function TsgRegionStack.Push(ARegion: TRegion): Integer;
var
  Region: TsgRegion;
begin
  Region := TsgRegion.Create;
  if Count > 0 then
    Region.Assign(ARegion);
  Result := Add(Region);
end;

function TsgRegionStack.RectInRegion(const AContext: TObject; const ARect: TRect): Boolean;
begin
  Result := False;
  if Assigned(AContext) and (Count > 0) then
    Result := TsgContext(AContext).IsRectInRegion(TsgRegion(Last), ARect);
end;

{$IFNDEF SGFPC}
{ TRegion }

procedure TRegion.Changed;
begin
  inherited Changed;
end;

function TRegion.GetClipRect: TRect;
begin
  if not HandleAllocated then
    Result := cnstRectZero//Rect(-MAXSHORT, -MAXSHORT, MAXSHORT, MAXSHORT);
  else
    _GetRgnBox(FHandle, {$IFDEF SGFPC}@{$ENDIF}Result);
end;

function TRegion.GetHandle: THandle;
begin
  ReferenceNeeded;
  Result := FHandle;
end;

function TRegion.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TRegion.ReferenceNeeded;
begin
  if FHandle = 0 then
  begin
    FHandle := _CreateRectRgn(0, 0, 0, 0);
    Changed;
  end;
end;

procedure TRegion.SetClipRect(const Value: TRect);
begin
  if HandleAllocated then
    _SetRectRgn(FHandle, Value.Left, Value.Top, Value.Right, Value.Bottom)
  else
    FHandle := _CreateRectRgnIndirect(Value);
  Changed;
end;

procedure TRegion.SetHandle(const Value: THandle);
begin
  if Value <> FHandle then
  begin
    if FHandle <> 0 then _DeleteObject(FHandle);
    FHandle := Value;
    Changed;
  end;
end;
{$ENDIF}

{ TsgRegion }

function TsgRegion.GetState: Integer;
var
  R: TRect;
begin
  if HandleAllocated then
    Result := _GetRgnBox({$IFDEF SGFPC}Reference.{$ENDIF}Handle, R)
  else
    Result := ERROR;
end;

function TsgRegion.Select(ACanvas: TCanvas): Integer;
begin
  if HandleAllocated then
    Result := _SelectClipRgn(ACanvas.Handle, Handle)
  else
    Result := _SelectClipRgn(ACanvas.Handle, 0);
end;

procedure TsgRegion.SetPolygon(const APoints; ACount, AFillMode: Integer);
begin
  Changing;
  Handle := _CreatePolygonRgn(APoints, ACount, AFillMode);
end;

procedure TsgRegion.SetPolygonList(const AList: TsgBaseList; const AFillMode: Integer);
begin
  case AList.ListType of
    ltInteger: SetPolygon(TsgIntegerList(AList).List^, AList.Count div 2, AFillMode);
  end;
end;

function TsgRegion.Update(ACanvas: TCanvas): Integer;
{$IFDEF SGFPC}
var
  vContextRGN: HRGN;
{$ENDIF}
begin
{$IFDEF SGFPC}
  vContextRGN := _CreateRectRgn(0, 0, 0, 0);
  Result := _GetClipRgn(ACanvas.Handle, vContextRGN);
  if Result <= 0 then
  begin
    _DeleteObject(vContextRGN);
    FIsValid := False;
  end
  else
  begin
    Changing;
    Handle := vContextRGN;
  end;
{$ELSE}
  Changing;
  Result := _GetClipRgn(ACanvas.Handle, Handle);
  if Result <= 0 then
    FreeReference;
  Changed;
{$ENDIF}
end;

procedure TsgRegion.FreeReference;
begin
{$IFDEF SG_DELPHI_VCL}
  if FHandle <> 0 then
  begin
    _DeleteObject(FHandle);
    FHandle := 0;
  end;
{$ELSE}
  Changing;
  Handle := 0;
{$ENDIF}
end;

procedure TsgRegion.Changing;
begin
{$IFDEF SGFPC}
  inherited Changing;
{$ENDIF}
end;

{$IFDEF SGFPC}
procedure TsgRegion.Changed;
var
  R: TRect;
begin
  inherited Changed;
  FIsValid := _GetRgnBox({$IFDEF SGFPC}Reference.{$ENDIF}Handle, R) > NullRegion; // external changed
end;

function TsgRegion.IsValid: Boolean;
begin
  Result := FIsValid;
end;

procedure TsgRegion.SetClipRect(value: TRect);
begin
  Changing;
  Handle := _CreateRectRgnIndirect(value);
end;

function TsgRegion.GetClipRect: TRect;
begin
  if HandleAllocated then
    _GetRgnBox({$IFDEF SGFPC}Reference.{$ENDIF}Handle, Result)
  else
    Result := cnstRectZero;
end;
{$ENDIF}

function TsgRegion.Combine(ARegion: TRegion; AMode: Integer): Integer;
begin
  Changing;
  Result := DoCombine(ARegion, AMode);
  if Result <= ERROR then
    FreeReference
  else
    Changed;
end;

destructor TsgRegion.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

function TsgRegion.DoCombine(ARegion: TRegion; AMode: Integer): Integer;
begin
  if TsgRegion(ARegion).HandleAllocated then
  begin
    case AMode of
      RGN_COPY: Result := _CombineRgn(Handle, ARegion.Handle, ARegion.Handle, AMode);
    else
      if HandleAllocated then
        Result := _CombineRgn(Handle, Handle, ARegion.Handle, AMode)
      else
        Result := _CombineRgn(Handle, ARegion.Handle, ARegion.Handle, RGN_COPY);
    end;
  end
  else
  begin
    if AMode = RGN_COPY then
      FreeReference;
    Result := GetState;
  end;
end;

procedure TsgRegion.CopyFrom(ARegion: TRegion);
begin
  Combine(ARegion, RGN_COPY);
end;

procedure TsgRegion.Exclude(ARegion: TRegion);
begin
  Combine(ARegion, RGN_XOR);
end;

procedure TsgRegion.Intersect(ARegion: TRegion);
begin
  Combine(ARegion, RGN_AND);
end;

procedure TsgRegion.Offset(AOffset: TPoint);
begin
  if HandleAllocated then
    _OffsetRgn(Handle, AOffset.X, AOffset.Y);
end;

procedure TsgRegion.Union(ARegion: TRegion);
begin
  Combine(ARegion, RGN_OR);
end;

procedure TsgRegion.Substract(ARegion: TRegion);
begin
  Combine(ARegion, RGN_DIFF);
end;

procedure TsgRegion.Assign(Source: TPersistent);
begin
  if Source is TsgRegion then
    CopyFrom(TsgRegion(Source))
  else
    if Source is TCanvas then
      Update(TCanvas(Source))
    else
      if Source = nil then
        FreeReference
      else
        inherited Assign(Source);
end;

function TsgRegion.HandleAllocated: Boolean;
begin
{$IFNDEF SGFPC}
  Result := inherited HandleAllocated;
{$ELSE}
  Result := FIsValid and Reference.Allocated;
{$ENDIF}
end;

function TsgRegion.Offset(ACanvas: TCanvas): TPoint;
begin
  Result := Point(0, 0);
  _LPtoDP(ACanvas.Handle, Result, 1);
  if (Result.X <> 0) or (Result.Y <> 0) then
  begin
    Changing;
    _OffsetRgn(Handle, Result.X, Result.Y);
    Changed;
  end;
end;

function TsgRegion.Paint(ACanvas: TCanvas): Integer;
begin
  if HandleAllocated then
    Result := Integer(_PaintRgn(ACanvas.Handle, Handle))
  else
    Result := 0;
end;

{ TsgProxyBase2D }

procedure TsgProxyBase2D.AddIntPoint2(const APoint: TFPoint; const ADelta: Integer = 1);
begin
  AddIntPoint(APoint);
  FIntPoints.Add(FPointTmp.X + ADelta);
  FIntPoints.Add(FPointTmp.Y);
end;

procedure TsgProxyBase2D.AddIntPointDirect(const APoint: TPoint);
begin
  FPointTmp := APoint;
  FIntPoints.Add(FPointTmp.X);
  FIntPoints.Add(FPointTmp.Y);
end;

procedure TsgProxyBase2D.AddIntPointDirect(const APoint: TFPoint);
begin
  if Abs(APoint.X) < MaxInt then
    FPointTmp.X := Round(APoint.X)
  else
    FPointTmp.X := MaxInt;
  if Abs(FPointTmp.Y) < MaxInt then
    FPointTmp.Y := Round(APoint.Y)
  else
    FPointTmp.Y := MaxInt;
  FIntPoints.Add(FPointTmp.X);
  FIntPoints.Add(FPointTmp.Y);
end;

function TsgProxyBase2D.AddPolySegment(const APoint1, APoint2: TFPoint): Boolean;
var
  vP1, vP2: TPoint;
begin
  Result := False;
  vP1 := GetPoint(APoint1);
  vP2 := GetPoint(APoint2);
  if IsEqualPoints(vP1, vP2) then
  begin
    Result := True;
    vP2.X := vP2.X + 1;
  end;
  FPoly.Add(vP1.X);
  FPoly.Add(vP1.Y);
  FPoly.Add(vP2.X);
  FPoly.Add(vP2.Y);
end;

procedure TsgProxyBase2D.AddOnePixel;
var
  vPoint: TPoint;
begin
  vPoint := FReadyPoints.Points[FIndex - 1];
  Inc(vPoint.X);
  FReadyPoints.Points[FIndex] := vPoint;
  Inc(FIndex);
end;

procedure TsgProxyBase2D.AddPolyFirstPoint(const ADelta: Integer = 1);
begin
  FPoly.Add(FPoly[0] + ADelta);//x
  FPoly.Add(FPoly[1]);//y
end;

procedure TsgProxyBase2D.AddPolyLastPoint(const ADelta: Integer = 1);
begin
  FPoly.Add(FPoly[FPoly.Count - 2] + ADelta);//x
  FPoly.Add(FPoly[FPoly.Count - 2]);//y
end;

procedure TsgProxyBase2D.AddPolyPoint(const APoint: TFPoint);
begin
  FPoly.AddPoint(GetPoint(APoint));
end;

procedure TsgProxyBase2D.AddPolyRect2D(const ARect: TFRect);
var
  vTopLeft: TPoint;
begin
  vTopLeft := GetPoint(ARect.TopLeft);
  FPoly.AddPoint(vTopLeft);
  FPoly.AddPoint(GetPoint(MakeFPoint(ARect.Right, ARect.Top, 0)));
  FPoly.AddPoint(GetPoint(ARect.BottomRight));
  FPoly.AddPoint(GetPoint(MakeFPoint(ARect.Left, ARect.Bottom, 0)));
  FPoly.AddPoint(vTopLeft);
end;

procedure TsgProxyBase2D.BoxPoint(const APoint: TPoint);
begin
  if FIndex < 8 then
    FReadyPoints.Points[FIndex] := APoint;
  Inc(FIndex);
end;

constructor TsgProxyBase2D.Create;
const
  cnstDefaultCapacity = 100000;
begin
  inherited Create;
  SetMatrix(nil);
  FReadyPoints := TsgPointsListHelper(CreateList(cnstDefaultCapacity));
  FIntPoints := TsgIntegerList(CreateList(cnstDefaultCapacity));
  FDotsPoints := TsgIntegerList(CreateList(cnstDefaultCapacity));
  FPoly := TsgPointsListHelper(CreateList(cnstDefaultCapacity));
  FCounts := TsgIntegerList.Create;
  F2Counts := TsgIntegerList.Create;
  F3Counts := TsgIntegerList.Create;
  FCounts.Capacity := 100;
  FReadyPoints.Count := cIPointsCnt + 2;
end;

function TsgProxyBase2D.CreateList(const ACapacity: Integer = 16): TsgBaseList;
begin
  Result := TsgIntegerList.Create;
  Result.Capacity := ACapacity;
end;

destructor TsgProxyBase2D.Destroy;
begin
  FreeAndNil(FReadyPoints);
  FreeAndNil(FIntPoints);
  FreeAndNil(FDotsPoints);
  FreeAndNil(FPoly);
  FreeAndNil(FCounts);
  FreeAndNil(F2Counts);
  FreeAndNil(F3Counts);
  inherited Destroy;
end;

procedure TsgProxyBase2D.AddIntFirstPoint(const ADelta: Integer = 1);
begin
  FIntPoints.Add(Integer(FIntPoints[0]) + ADelta);
  FIntPoints.Add(FIntPoints[1]);
end;

procedure TsgProxyBase2D.AddIntLastPoint(const ADelta: Integer = 1);
begin
  FIntPoints.Add(Integer(FIntPoints[FIntPoints.Count - 2]) + ADelta);
  FIntPoints.Add(FIntPoints[FIntPoints.Count - 2]);
end;

function TsgProxyBase2D.AddIntSegment(const APoint1, APoint2: TFPoint): Boolean;
var
  vPointTmp: TPoint;
begin
  Result := False;
  vPointTmp := GetPoint(APoint1);
  FPointTmp := GetPoint(APoint2);
  if IsEqualPoints(vPointTmp, FPointTmp) then
  begin
    Result := True;
    FPointTmp.X := FPointTmp.X + 1;
  end;
  FIntPoints.Add(vPointTmp.X);
  FIntPoints.Add(vPointTmp.Y);
  FIntPoints.Add(FPointTmp.X);
  FIntPoints.Add(FPointTmp.Y);
end;

procedure TsgProxyBase2D.AddIntPoint(const APoint: TFPoint;
  const ACheckEqual: Boolean = False);
var
  vPointTmp: TPoint;
begin
  if ACheckEqual then
  begin
    vPointTmp := GetPoint(APoint);
    if not IsEqualPoints(FPointTmp, vPointTmp) then
    begin
      FPointTmp := vPointTmp;
      FIntPoints.Add(FPointTmp.X);
      FIntPoints.Add(FPointTmp.Y);
    end;
  end
  else
  begin
    FPointTmp := GetPoint(APoint);
    FIntPoints.Add(FPointTmp.X);
    FIntPoints.Add(FPointTmp.Y);
  end;
end;

function TsgProxyBase2D.GetFIRect: TRect;
begin
  Result := FIRect^;
end;

function TsgProxyBase2D.GetPoint(const APoint: TFPoint): TPoint;
begin
  Result := FPointXMat2DLongint(APoint, FMatrix^);
end;

procedure TsgProxyBase2D.IncRectRightBottom;
begin
  Inc(FIRect^.Right);
  Inc(FIRect^.Bottom);
end;

procedure TsgProxyBase2D.SetFIRect(const ARect: TRect);
begin
  if Assigned(FIRect) then
    FIRect^ := ARect;
end;

procedure TsgProxyBase2D.SetMatrix(const AMatrix: PFMatrix);
begin
  if AMatrix = nil then
    FMatrix := @cnstIdentityMat
  else
    FMatrix := AMatrix;
end;

procedure TsgProxyBase2D.SetRectPoint(const AIndex: Integer; const APoint: TPoint);
begin
  if AIndex < 8 then
    FReadyPoints.Points[AIndex] := APoint;
end;


{ TsgContext }

//constructor TsgContext.Create(const ACanvas: TCanvas);
//begin
//  FCanvas := ACanvas;
//end;

procedure TsgContext.BeginPath;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Windows.BeginPath(FCanvas.Handle);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.CloseFigure;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Windows.CloseFigure(FCanvas.Handle);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.EndPath;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Windows.EndPath(FCanvas.Handle);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.EndScene;
begin
{$IFDEF SG_FIREMONKEY}
  if Assigned(FCanvas) then
    FCanvas.EndScene;
{$ENDIF}
end;

function TsgContext.DoExtTextOut(X, Y: Integer; Options: Longint; Rect: PRect; Str: PWideChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;
{$IFDEF USE_SG_AGG2D}
begin
  FCanvas.AggTextOut(X, Y - FontHeight, string(WideString(Str)));
  Result := True;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
begin
  Result :=  Windows.ExtTextOutW(FCanvas.Handle, X, Y, Options, Rect, Str,
    Count, {$IFDEF SGFPC}LPINT{$ENDIF}(Dx));
end;
{$ELSE}
{$IFDEF SG_FIREMONKEY}
var
  vSign: Integer;
  vRect, vPadding: TRectF;
  vScale, vBaseLine: Single;
  vTextRegion: FMX.Graphics.TRegion;
{$ENDIF}
{$IFDEF SGFPC}
var
  W: WideString;
  S: string;
{$ENDIF}
begin
{$IFDEF SGFPC}
  if Count <> -1 then
  begin
    SetString(W, Str, Count);
    S := string(W);
    Count := strlen(PChar(S));
  end
  else
  begin
    W := WideString(Str);
    S := string(W);
  end;
  Result := LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, Options, Rect,
    PChar(S), Count, ObjPas.PInteger(Dx));
{$ELSE}
  vScale := FCanvas.Font.PixelsPerInch / 72.0;
  vRect := TRectF.Create(Rect^, True);
  FLayout.BeginUpdate;
  try
    FLayout.Font.Family := FCanvas.Font.Family;
    FLayout.Font.StyleExt := FCanvas.Font.StyleExt;
    vSign := {$IFDEF SG_FM_LINUX}-1{$ELSE}{$IFDEF SG_FM_WINDOWS}Ord(not GlobalUseDirect2D) shl 1 - 1{$ELSE}1{$ENDIF}{$ENDIF};
    FLayout.Font.Height := vSign * FCanvas.Font.Height{$IFNDEF SG_FM_LINUX} * vScale{$ENDIF};
    vBaseLine := TFontGlyphManager.Current.GetBaseline(FLayout.Font, 1);
    FLayout.Padding.Rect := FLayout.Padding.DefaultValue;
    FLayout.TopLeft := TPointF.Create(X, Y - vBaseLine{ * vScale});
    FLayout.MaxSize := PointF(vRect.Width * vScale, vRect.Height * vScale);
    FLayout.Text := string(WideString(Str));
    FLayout.WordWrap := False;
    FLayout.Opacity := 1.0;
    FLayout.HorizontalAlign := TTextAlign.Leading;
    FLayout.VerticalAlign := TTextAlign.Leading;

    FLayout.Color := FCanvas.Fill.Color;
    FLayout.RightToLeft := False;
  finally
    FLayout.EndUpdate;
  end;

  vTextRegion := FLayout.RegionForRange(TTextRange.Create(0, Length(FLayout.Text)));
  if Length(vTextRegion) > 0 then
  begin
    vPadding := TRectF.Create(TPointF.Create(vScale * (X-vTextRegion[0].Left), 0));
    vPadding.Bottom := vScale * (vRect.Height - vTextRegion[0].Height);// expand bottom text clipping
    FLayout.Padding.Rect := vPadding;
  end;
  FLayout.RenderLayout(FCanvas);
  Result := True;
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}

function TsgContext.DoExtTextOutA(X, Y: Integer; Options: Longint; Rect: PRect; Str: PAnsiChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;
{$IFDEF USE_SG_AGG2D}
begin
  FCanvas.AggTextOut(X, Y - FontHeight, string(AnsiString(Str)));
  Result := True;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
begin
  Result :=  Windows.ExtTextOutA(FCanvas.Handle, X, Y, Options, Rect, Str,
    Count, {$IFDEF SGFPC}LPINT{$ENDIF}(Dx));
end;
{$ELSE}
begin
{$IFDEF SGFPC}
  Result := LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, Options, Rect,
    PChar(string(AnsiString(Str))), Count, ObjPas.PInteger(Dx));
{$ELSE}
  FCanvas.FillText(TRectF.Create(Rect^), string(AnsiString(Str)), False, 1.0,
    [TFillTextFlag.RightToLeft], TTextAlign.Center);
  Result := True;
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}

function TsgContext.DoExtTextOutW(X, Y: Integer; Options: Longint; Rect: PRect; Str: PWideChar; Count: Longint; Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger): Boolean;
begin
  Result := Self.DoExtTextOut(X, Y, Options, Rect, Str, Count, Dx);
end;

procedure TsgContext.StrokePath;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Windows.StrokePath(FCanvas.Handle);
{$ENDIF}
{$ENDIF}
end;

function TsgContext.BeginScene: Boolean;
begin
  Result := False;
{$IFDEF SG_FIREMONKEY}
  if Assigned(FCanvas) then
  begin
    Result := FCanvas.BeginScene;
  {$IFDEF SG_FM_MACOS}
    CGContextSetAllowsAntialiasing(TCanvasQuartzAccess(FCanvas).Context, {$IF CompilerVersion < 34.0}Ord{$IFEND}(False));
  {$ENDIF}
  end;
{$ENDIF}
end;

procedure TsgContext.BitBlt(const X, Y, Width, Height, SX, SY, ARop: Integer; const ABitMap: {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap);
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  { TODO: AGG2D TsgContext.BitBlt }
{$ELSE}
  {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ELSE}LCLIntf.{$ENDIF}BitBlt(FCanvas.Handle, X, Y, Width, Height, ABitMap.Canvas.Handle, SX, SY, ARop);
{$ENDIF}
{$ELSE}
  FCanvas.DrawBitmap(ABitMap, ABitMap.BoundsF, TRectF.Create(X, Y, X + Width, Y + Height), 1);
{$ENDIF}
end;

{$IFDEF SG_FIREMONKEY}
procedure TsgContext.CanvasDestroyListener(const Sender: TObject;
  const M: TMessage);
begin
  if Sender = FCanvas then
    SetCanvas(nil);
end;

function TsgContext.GetTile(const AColor: TAlphaColor; const AStyle: TBrushStyle): FMX.Graphics.TBitmap;
const
  cnstStep = 8;
var
  vIndex, I, vSize: Integer;
  vP1, vP2, vP3, vP4: TPointF;
  vObj: TObject;
  vCanvas: TCanvas;
begin
  vIndex := Ord(AStyle);
  if FBmpColors[vIndex] <> AColor then
  begin
    vObj := FBmpTiles[vIndex];
    FBmpTiles[vIndex] := nil;
    FreeAndNil(vObj);
  end;
  Result := FBmpTiles[vIndex];
  if not Assigned(Result) then
  begin
    Result := FMX.Graphics.TBitmap.Create(128, 128);
    FBmpTiles[vIndex] := Result;
    FBmpColors[vIndex] := AColor;
    try
      vCanvas := Result.GetClassOfCanvas.CreateFromBitmapEx(Result, TCanvasQuality.HighPerformance);
    except
      vCanvas := Result.Canvas;
    end;
    try
      if Assigned(vCanvas) and vCanvas.BeginScene() then
      begin
        try
          Result.Clear(TAlphaColorF.Create(1, 1, 1, 0).ToAlphaColor);
          vCanvas.Pen.Thickness := 1;
          vCanvas.Pen.Color := AColor;
          vCanvas.Pen.Kind := TBrushKind.Solid;
          vCanvas.Brush.Kind := TBrushKind.None;
          I := 0;
          vSize := Result.Width;
          case AStyle of
            bsSolid:
              begin
                Result.Clear(AColor);
              end;
            bsHorizontal:
              begin
                vP1.X := 0;
                vP2.X := vSize;
                while I <= vSize do
                begin
                  vP1.Y := I;
                  vP2.Y := I;
                  vCanvas.DrawLine(vP1, vP2, 1);
                  Inc(I, cnstStep);
                end;
              end;
            bsVertical:
              begin
                vP1.Y := 0;
                vP2.Y := vSize;
                while I <= vSize do
                begin
                  vP1.X := I;
                  vP2.X := I;
                  vCanvas.DrawLine(vP1, vP2, 1);
                  Inc(I, cnstStep);
                end;
              end;
            bsFDiagonal, bsBDiagonal:
              begin
                vP1 := TPointF.Zero;
                vP2 := TPointF.Zero;
                I := 0;
                vSize := vSize * 2;
                while I <= vSize do
                begin
                  vP1.X := I;
                  vP2.Y := I;
                  vCanvas.DrawLine(vP1, vP2, 1);
                  Inc(I, cnstStep);
                end;
                if AStyle = bsFDiagonal then
                  Result.FlipVertical;
              end;
            bsCross:
              begin
                vP1.X := 0;
                vP2.X := vSize;
                vP3.Y := 0;
                vP4.Y := vSize;
                while I <= vSize do
                begin
                  vP1.Y := I;
                  vP2.Y := vP1.Y;
                  vP3.X := vP1.Y;
                  vP4.X := vP1.Y;
                  vCanvas.DrawLine(vP1, vP2, 1);
                  vCanvas.DrawLine(vP3, vP4, 1);
                  Inc(I, cnstStep);
                end;
              end;
            bsDiagCross:
              begin
                vP1 := TPointF.Zero;
                vP2 := TPointF.Zero;
                vSize := vSize * 2;
                while I <= vSize do
                begin
                  vP1.X := I;
                  vP2.Y := I;
                  vCanvas.DrawLine(vP1, vP2, 1);
                  Inc(I, cnstStep);
                end;
                vP1.X := -vSize;
                vP1.Y := 0;
                vP2.X := 0;
                vP2.Y := vSize;
                for I := 0 to (vSize * 3) div cnstStep do
                begin
                  vCanvas.DrawLine(vP1, vP2, 1);
                  vP1.X := vP1.X + cnstStep;
                  vP2.X := vP2.X + cnstStep;
                end;
              end;
          end;
        finally
          Result.Canvas.EndScene;
        end;
      end;
    finally
      if vCanvas <> Result.Canvas then
        vCanvas.Free;
    end;
  end;
end;
{$ENDIF}

function TsgContext.ClipRect: TRect;
begin
{$IFDEF USE_SG_AGG2D}
  if FCanvas.Clipping then
    Result := FCanvas.ClipRect
  else
    Result := Types.Rect(0, 0, FCanvas.Width, FCanvas.Height);
{$ELSE}
  Result := FCanvas.ClipRect;
{$ENDIF}
end;

constructor TsgContext.Create;
begin
  inherited Create;
{$IFDEF SG_FIREMONKEY}
  FMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TCanvasDestroyMessage,
    CanvasDestroyListener);
  FBmpTiles := TList<FMX.Graphics.TBitmap>.Create;
  FBmpTiles.Count := Ord(High(TBrushStyle)) + 1;
  FBmpColors := TList<TAlphaColor>.Create;
  FBmpColors.Count := FBmpTiles.Count;
{$ENDIF}
end;

function TsgContext.CreateGDIPGraphic:{$IFDEF SG_USEGDIPLUS}TGPGraphics{$ELSE}TObject{$ENDIF};
begin
{$IFDEF SG_USEGDIPLUS}
  Result := TGPGraphics.Create(FCanvas.Handle);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

destructor TsgContext.Destroy;
{$IFDEF SG_FIREMONKEY}
var
  I: Integer;
  vBmp: TObject;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  if FMessageId <> 0 then
  begin
    TMessageManager.DefaultManager.Unsubscribe(TCanvasDestroyMessage, FMessageId);
    FMessageId := 0;
  end;
  FreeAndNil(FLayout);
  if Assigned(FBmpTiles) then
  begin
    for I := FBmpTiles.Count - 1 downto 0 do
    begin
      vBmp := FBmpTiles[I];
      FBmpTiles[I] := nil;
      FBmpTiles.Delete(I);
      if Assigned(vBmp) then
        FreeAndNil(vBmp);
    end;
    FreeAndNil(FBmpTiles);
  end;
  FreeAndNil(FBmpColors);
{$ENDIF}
  inherited Destroy;
end;

procedure TsgContext.DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  const Arc: PsgArc);
begin
{$IFNDEF SG_FIREMONKEY}
  FCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4)
{$ELSE}
  FCanvas.DrawArc(TPointF.Create(Arc^.Center.X, Arc^.Center.Y),
    TPointF.Create(Arc^.Rx, Arc^.Ry), 360 - Arc^.Ang1, Arc^.Ang2 - Arc^.Ang1, 1);
{$ENDIF}
end;

procedure TsgContext.DoEntityProcBegin(const Sender: TObject);
begin
end;

procedure TsgContext.DoEntityProcEnd(const Sender: TObject);
begin
end;

procedure TsgContext.DoFont(AFont: TFont);
{$IFDEF SG_FIREMONKEY}
var
  vFontOnChanged: TNotifyEvent;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  TFPCustomFontAccess(FCanvas.Font).DoCopyProps(AFont);
{$ELSE}
  FCanvas.Font := AFont;
{$ENDIF}
{$ELSE}
  vFontOnChanged := FCanvas.Font.OnChanged;
  try
    FCanvas.Font.OnChanged := nil;
    FCanvas.Font.Assign(AFont);
    FCanvas.Font.Height := AFont.Height;
  finally
    FCanvas.Font.OnChanged := vFontOnChanged;
    FCanvas.Font.Change;
  end;
{$ENDIF}
end;

procedure TsgContext.DoImage(const R: TRect; AImage: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}{$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap{$ENDIF});
begin
{$IFDEF USE_SG_AGG2D}
  { TODO: TsgContext.DoImage for AGG2D}
  //FCanvas.StretchDraw(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, AImage);
{$ELSE}
{$IFDEF SG_FIREMONKEY}
  try
    FCanvas.BeginScene;
    FCanvas.DrawBitmap(AImage, AImage.BoundsF, TRectF.Create(R), 1,
      not (FCanvas.Quality = TCanvasQuality.HighQuality));
  finally
    FCanvas.EndScene;
  end;
{$ELSE}
  FCanvas.StretchDraw(R, AImage);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.DoPenGeometric(AStyle: DWORD;
  const APenColor, APenWidth: Integer);
{$IFDEF USE_SG_AGG2D}
begin
 { TODO: TsgCADImage.SetPenGeometric for AGG2D }
end;
{$ELSE}
{$IFNDEF SG_FIREMONKEY}
var
  vLB: TLogBrush;
begin
  vLB.lbStyle := BS_SOLID;
  vLB.lbColor := ColorToRGB(APenColor);
  vLB.lbHatch := 0;
  FCanvas.Pen.Handle := ExtCreatePen(AStyle, APenWidth, vLB, 0, nil);
{$ELSE}
begin
  case AStyle and PS_JOIN_MASK of
    PS_JOIN_ROUND: FCanvas.Pen.Join := TStrokeJoin.Round;
    PS_JOIN_BEVEL: FCanvas.Pen.Join := TStrokeJoin.Bevel;
    PS_JOIN_MITER: FCanvas.Pen.Join := TStrokeJoin.Miter;
  end;
  case AStyle and PS_ENDCAP_MASK of
    PS_ENDCAP_ROUND:  FCanvas.Pen.Cap := TStrokeCap.Round;
    PS_ENDCAP_SQUARE: FCanvas.Pen.Cap := TStrokeCap.Flat;
    PS_ENDCAP_FLAT:   FCanvas.Pen.Cap := TStrokeCap.Flat;
  end;
  FCanvas.Pen.Width := APenWidth;
{$ENDIF}
end;
{$ENDIF}

procedure TsgContext.DoPenMiterLimit(ANewLimit: Single);
begin
{$IFNDEF SG_FIREMONKEY}
  {$IFNDEF SG_NON_WIN_PLATFORM}
  {$IFNDEF USE_SG_AGG2D}
  SetMiterLimit(FCanvas.Handle, ANewLimit, nil);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

procedure TsgContext.DoPixel(APoint: TPoint; AColor: TColor);
begin
{$IFDEF USE_SG_AGG2D}
  if (APoint.x >= 0) and (APoint.x < FCanvas.Width) and
     (APoint.y >= 0) and (APoint.y < FCanvas.Height) then
    FCanvas.Image.IntfImg.TColors[APoint.X, APoint.Y] := AColor;
{$ELSE}
  FCanvas.Pixels[APoint.X, APoint.Y] := AColor;
{$ENDIF}
end;

procedure TsgContext.DoPolygon(Points: PPoint; Count: Integer);
begin
  FCanvas.Polygon(Slice({$IFDEF SGFPC}sgConsts.{$ENDIF}PPoints(Points)^, Count){$IFDEF SGFPC}, False{$ENDIF});
end;

procedure TsgContext.DoPolyline(Points: PPoint; Count: Integer);
begin
  FCanvas.Polyline(Slice({$IFDEF SGFPC}sgConsts.{$ENDIF}PPoints(Points)^, Count));
end;

procedure TsgContext.DoPolyPolyline(const Points; Counts: PInteger; Count: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
  PolyPolyline(GetDC, Points, Counts^, Count);
{$ELSE}
  FCanvas.PolyPolyline(PPoint(@Points), Counts, Count);
{$ENDIF}
end;

{$IFDEF SG_USE_AGG2D_AS_GDI}
procedure TsgContext.DoPolyPoly(const Points: array of TPointF; const Counts: array of Integer;
  AFillMode: Integer = 2; AUseFont: Boolean = False);
var
  I, C: Integer;
  vDC: TFPGUIDeviceContext;
begin
  if sgAgg2dWidgetSet.IsValidDC(GetDC, @vDC) then
  begin
    C := 0;
    for I := Low(Counts) to High(Counts) do Inc(C, Counts[I]);
    case AFillMode and 3 of
      0:;
      1: vDC.Canvas.DrawPolyPoly(Points, Counts, AGG_StrokeOnly, AUseFont);
      2: vDC.Canvas.DrawPolyPoly(Points, Counts, AGG_FillOnly, AUseFont);
      3: vDC.Canvas.DrawPolyPoly(Points, Counts, AGG_FillAndStroke, AUseFont);
    end;
  end
  else
    inherited DoPolyPoly(Points, Counts, AFillMode, AUseFont);
end;
{$ENDIF}

function TsgContext.DoRestoreDC(const AIndex: Integer): Boolean;
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  Result := False;
{$ELSE}
  Result := RestoreDC(FCanvas.Handle, AIndex);
{$ENDIF}
{$ELSE}
  try
    FCanvas.BeginScene;
    FCanvas.RestoreState(FCanvasSaveState);
  finally
    FCanvas.EndScene;
  end;
  Result := True;
{$ENDIF}
end;

function TsgContext.DoSaveDC: Integer;
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  Result := 0;
{$ELSE}
  Result := SaveDC(FCanvas.Handle);
{$ENDIF}
{$ELSE}
  try
    FCanvas.BeginScene;
    FCanvasSaveState := FCanvas.SaveState;
  finally
    FCanvas.EndScene;
  end;
  Result := FCanvas.SavingStateCount;
{$ENDIF}
end;

procedure TsgContext.DoTextOutAW(X, Y: Integer; const ATextA: AnsiString;
  const ATextW: WideString; const ATextParams: PsgExpTextParam;
  ACount: Longint = 0);
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  if Length(ATextW) > 0 then
  begin
    if ACount = 0 then
      ACount := Length(ATextW);
{$IFDEF USE_SG_AGG2D}
  FCanvas.TextOut(X, Y, ATextW);
{$ELSE}
   TextOutW(FCanvas.Handle, X, Y, PWideChar(ATextW), ACount)
{$ENDIF}
  end
  else
    if Length(ATextA) > 0 then
    begin
      if ACount = 0 then
        ACount := Length(ATextA);
{$IFDEF USE_SG_AGG2D}
      FCanvas.TextOut(X, Y, ATextA);
{$ELSE}
      TextOutA(FCanvas.Handle, X, Y, PAnsiChar(ATextA),  ACount);
{$ENDIF}
    end;
{$ELSE}
  FCanvas.TextOut(X, Y, {$IFDEF UNICODE}ATextW{$ELSE}ATextA{$ENDIF});
{$ENDIF}
end;

procedure TsgContext.DoPolyPolygon(const Points; Counts: PInteger; Count: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
  PolyPolygon(GetDC, PPointer(@Points)^, Counts^, Count);
{$ELSE}
  FCanvas.PolyPolygon(PPoint(@Points), Counts, Count);
{$ENDIF}
end;

procedure TsgContext.Ellipse(const X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TsgContext.FillRect(const Rect: TRect);
begin
{$IFNDEF SG_FIREMONKEY}
  FCanvas.FillRect(Rect);
{$ELSE}
  FCanvas.FillRect(TRectF.Create(Rect), 0, 0, [], 1);
{$ENDIF}
end;

procedure TsgContext.FillRegion(const ARegion: TRegion);
begin
{$IFDEF SG_FIREMONKEY}
  FMX_FillRgn(FCanvas.Handle, ARegion.Handle, THandle(FCanvas.Brush));
{$ELSE}
{$IFNDEF USE_SG_AGG2D}
  FillRgn(FCanvas.Handle, ARegion.Handle, FCanvas.Brush.Handle);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.FixLastPointsCount;
begin
{$IFNDEF SG_FIREMONKEY}
  if (FCounts.Count > 0) and (FCounts.Last = 1) then
  begin
    Counts.Last := 2;
    Poly.AddPoint(Poly.Points[Poly.PointsCount - 1]);
  end;
{$ENDIF}
end;

function TsgContext.IsRectInRegion(const ARegion: TRegion; const ARect: TRect): Boolean;
begin
  {$IFDEF SG_FIREMONKEY}
  Result := FMX_RectInRegion(ARegion.Handle, ARect); //TODO: IsEntityInRectVisible for Firemonkey
  {$ELSE}
  Result := RectInRegion(ARegion.Handle, ARect);
  {$ENDIF}
end;

{$IFDEF SG_FIREMONKEY}
procedure TsgContext.FontChanged(Sender: TObject);
begin
  if Assigned(FFontChanged) then
    FFontChanged(Sender);
  FCanvas.Fill.Color := FCanvas.Font.Color.AsBGRA;
end;
{$ENDIF}

function TsgContext.GetBrushColor: TColor;
begin
{$IFDEF SG_FIREMONKEY}
  Result := TAlphaColorRec.ColorToRGB(FCanvas.Fill.Color);
{$ELSE}
  Result := FCanvas.Brush.Color;
{$ENDIF}
end;

function TsgContext.GetBrushStyle: TBrushStyle;
begin
{$IFDEF SG_FIREMONKEY}
  Result := bsSolid;
  case FCanvas.Fill.Kind of
    TBrushKind.None:     Result := bsClear;
    TBrushKind.Solid:    Result := bsSolid;
    TBrushKind.Gradient: ;
    TBrushKind.Bitmap: ;
    TBrushKind.Resource: ;
  end;
{$ELSE}
  Result := FCanvas.Brush.Style;
{$ENDIF}
end;

function TsgContext.GetCharSet: TFontCharset;
begin
{$IFNDEF USE_SG_AGG2D}
  if Assigned(FCanvas) then//!!!
    Result := FCanvas.Font.Charset
  else
{$ENDIF}
    Result := ANSI_CHARSET;
end;

function TsgContext.GetDC: THandle;
begin
{$IFDEF USE_SG_AGG2D}
  Result := THandle(FCanvas);
{$ELSE}
  Result := FCanvas.Handle;
{$ENDIF}
end;

{$IFDEF SG_FIREMONKEY}
function IsPrinterCanvas(ACanvas: TCanvas): Boolean;
begin
  Result := (ACanvas <> nil) and Assigned(TCanvasAcceess(ACanvas).FPrinter);
end;


function TsgContext.GetMMToPixel: Double;
var
  vPrinter: TPrinterDevice;
  vDPIX: Integer;
begin
  if IsPrinterCanvas(FCanvas) then
  begin
    vPrinter := TPrinter(TCanvasAcceess(FCanvas).FPrinter).ActivePrinter;
    vDPIX := vPrinter.DPI[vPrinter.ActiveDPIIndex].X;
    Result := cnstMMPerInch / vDPIX;
  end
  else
   Result := cnstMMPerInch / PixelsPerInch;
end;
{$ENDIF}

function TsgContext.GetDeviceCaps(const AType: Integer): Integer;
begin
{$IFDEF USE_SG_AGG2D}
  Result := 0;
{$ELSE}
{$IFDEF SG_DELPHI_VCL}
  Result := Windows.GetDeviceCaps(FCanvas.Handle, AType);
{$ELSE}
  {$IFDEF SGFPC}
  Result := LCLIntf.GetDeviceCaps(FCanvas.Handle, AType);
  {$ELSE}
  Result := 0;
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function TsgContext.GetFontColor: TColor;
begin
  Result := FCanvas.Font.Color;
end;

function TsgContext.GetFontHeight: Single;
begin
{$IFDEF USE_SG_AGG2D}
  Result := FCanvas.Font.AggHeight;
{$ELSE}
  Result := FCanvas.Font.Height;
{$ENDIF}
end;

function TsgContext.GetFontStyle: TFontStyles;
begin
{$IFDEF USE_SG_AGG2D}
  Result := [];
  if FCanvas.Font.Bold then Include(Result, fsBold);
  if FCanvas.Font.Italic then Include(Result, fsItalic);
  if FCanvas.Font.Underline then Include(Result, fsUnderline);
  if FCanvas.Font.StrikeThrough then Include(Result, fsStrikeOut);
{$ELSE}
  Result := FCanvas.Font.Style;
{$ENDIF}
end;

function TsgContext.GetPenColor: TColor;
begin
{$IFDEF SG_FIREMONKEY}
  Result := TAlphaColorRec.ColorToRGB(FCanvas.Stroke.Color);
{$ELSE}
  Result := FCanvas.Pen.Color;
{$ENDIF}
end;

function TsgContext.GetPenMode: TPenMode;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := FCanvas.Pen.Mode;
{$ELSE}
  Result := pmBlack; //only to turn the compiller's warnging off
{$ENDIF}
end;

function TsgContext.GetPenStyle: TPenStyle;
begin
{$IFDEF SG_FIREMONKEY}
  Result := psSolid;
  case FCanvas.Stroke.Dash of
    TStrokeDash.Solid:      Result := psSolid;
    TStrokeDash.Dash:       Result := psDash;
    TStrokeDash.Dot:        Result := psDot;
    TStrokeDash.DashDot:    Result := psDashDot;
    TStrokeDash.DashDotDot: Result := psDashDotDot;
    TStrokeDash.Custom: ;
  end;
{$ELSE}
  Result := FCanvas.Pen.Style;
{$ENDIF}
end;

function TsgContext.GetPenWidth: Single;
begin
{$IFDEF SG_FIREMONKEY}
  Result := FCanvas.Stroke.Thickness;
{$ELSE}
{$IFDEF USE_SG_AGG2D}
  Result := FCanvas.Pen.AggLineWidth;
{$ELSE}
{$IFDEF SG_USE_AGG2D_AS_GDI}
  Result := sgAGG2DIntf2.sgAgg2dWidgetSet.GetDCPenWidth(GetDC);
{$ELSE}
  Result := FCanvas.Pen.Width;
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function TsgContext.GetPixel(X, Y: Integer): TColor;
begin
{$IFDEF USE_SG_AGG2D}
  Result := FCanvas.Image.IntfImg.TColors[X, Y];
{$ELSE}
  Result := FCanvas.Pixels[X, Y];
{$ENDIF}
end;

function TsgContext.GetPixelsPerInch: Integer;
begin
{$IFDEF SG_FIREMONKEY}
  Result := Round(FCanvas.Scale * cnstStdDPI);
{$ELSE}
  Result := FCanvas.Font.PixelsPerInch;
{$ENDIF}
end;

function TsgContext.GetSize: TPoint;
begin
{$IFDEF SG_DELPHI_VCL}
  Result := Point(FSize.cx, FSize.cy);
{$ELSE}
  Result := Point(FCanvas.Width, FCanvas.Height);
{$ENDIF}
end;

function TsgContext.GetStretchBltMode: Integer;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Result := Windows.GetStretchBltMode(FCanvas.Handle);
{$ELSE}
  Result := 0;
{$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TsgContext.GradientFill(const ARect: TRect; const AColor1, AColor2: Integer;
  const APoly: TsgPointsListHelper; const AGradientPositions: PFRect = nil; AStyle: Byte = 0;
  ACounts: TsgIntegerList = nil); //AStyle: TGradientStyle for Firemonkey
{$IFDEF SG_FIREMONKEY}
var
  vPathData: FMX.Graphics.TPathData;

  function GetPathDataFromPoly: FMX.Graphics.TPathData;
  var
    vPolygonsCount: Integer;
  begin
    Result := FMX.Graphics.TPathData.Create;
    vPolygonsCount := 1;
    if Assigned(ACounts) then
      vPolygonsCount := ACounts.Count;
    if vPolygonsCount = 1 then
      FCanvas.AppendPathData(Result, PPoint(APoly.List), [APoly.Count shr 1], pdtPolygone)
    else
      FCanvas.AppendPathData(Result, PPoint(APoly.List), Slice(PIntegerArray(ACounts.List)^, vPolygonsCount), pdtPolyPolygone);
  end;

begin
  SetFillGradient(AColor1, AColor2, AGradientPositions^.TopLeft,
      AGradientPositions^.BottomRight, TGradientStyle(AStyle));
  vPathData := GetPathDataFromPoly;
  try
    FCanvas.FillPath(vPathData, 1.0);
  finally
    vPathData.Free;
  end;
end;
{$ELSE}
{$IFDEF USE_SG_AGG2D}
begin
  FCanvas.GradientFill(ARect, AColor1, AColor2, gdHorizontal);
end;
{$ELSE}
var
  vMesh: TGradientTriangle;
  Vert: array[0..1] of TTriVertex;

  procedure SetVert(var V: TTriVertex; const P: TPoint; C: TColor);
  begin
    V.X := P.X;
    V.Y := P.Y;
    V.Red := (C and $FF) shl 8;
    V.Green := C and $FF00;
    V.Blue := (C and $FF0000) shr 8;
    V.Alpha := 0;
  end;

begin
  vMesh.Vertex1 := 0;
  vMesh.Vertex2 := 1;
  vMesh.Vertex3 := 2;
  SetVert(Vert[0], ARect.TopLeft, AColor1);
  SetVert(Vert[1], ARect.BottomRight, AColor2);
  GradientFillProc(FCanvas.Handle, @Vert, 2, @vMesh, 1, AStyle);
end;
{$ENDIF}
{$ENDIF}

procedure TsgContext.RadialGradFill(const AMetaPath; x, y, r: Double; const ARect: TRect;
  const APositions, AColors; const AColorsCount: Integer);
{$IFDEF SG_USE_AGG_INTF2}
var
  vDC: TFPGUIDeviceContext;
begin
  if TFPGUIDeviceContext.IsValidHandle(GetDC, False, @vDC) and (AColorsCount > 1) then
  begin
    vDC.Canvas.AggFillRadialGradient(x, y, r, PSingle(@APositions),
      PfpgColor(@AColors), AColorsCount);
    TfpgPath(vDC.Canvas.Path).Clear;
    if Assigned(@AMetaPath) and (TObject(AMetaPath) = Self) then
      TfpgPath(vDC.Canvas.Path).AddPolyPolygon(IntPoints.List^, Counts.List^, Counts.Count)
    else
      TfpgPath(vDC.Canvas.Path).AddRect(ARect);
    vDC.Canvas.AggDrawPath(AGG_FillOnly);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TsgContext.LinearGradFill(const AMetaPath; const ARect: TRect; const APositions,
  AColors; const AColorsCount: Integer; AIsVertical: Boolean);
{$IFDEF SG_USE_AGG_INTF2}
var
  vDC: TFPGUIDeviceContext;
begin
  if TFPGUIDeviceContext.IsValidHandle(GetDC, False, @vDC) and (AColorsCount > 1) then
  begin
    if AIsVertical then
      vDC.Canvas.AggFillLinearGradient(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom,
        PSingle(@APositions), PfpgColor(@AColors), AColorsCount)
    else
      vDC.Canvas.AggFillLinearGradient(ARect.Left, ARect.Top, ARect.Right, ARect.Top,
        PSingle(@APositions), PfpgColor(@AColors), AColorsCount);
    TfpgPath(vDC.Canvas.Path).Clear;
    if Assigned(@AMetaPath) and (TObject(AMetaPath) = Self) then
      TfpgPath(vDC.Canvas.Path).AddPolyPolygon(IntPoints.List^, Counts.List^, Counts.Count)
    else
      TfpgPath(vDC.Canvas.Path).AddRect(ARect);
    vDC.Canvas.AggDrawPath(AGG_FillOnly);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TsgContext.IntersectClipRect(X1, Y1, X2, Y2: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  FCanvas.ClipRegion.Free;
  FCanvas.ClipRegion := TFPRectRegion.Create;
  TFPRectRegion(FCanvas.ClipRegion).Rect := Types.Rect(X1, Y1, X2, Y2);
{$ELSE}
  {$IFDEF SG_NON_WIN_PLATFORM}LCLIntf.{$ELSE}Windows.{$ENDIF}IntersectClipRect(FCanvas.Handle, X1, Y1, X2, Y2);
{$ENDIF}
{$ELSE}
  FCanvas.IntersectClipRect(TRectF.Create(X1, Y1, X2, Y2));
{$ENDIF}
end;

function TsgContext.IsMetafileCanvas: Boolean;
begin
{$IFDEF SG_DELPHI_VCL}
  Result := FObjectType in [OBJ_METADC, OBJ_ENHMETADC];
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgContext.Line(const X1, Y1, X2, Y2: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
  FCanvas.MoveTo(X1, Y1);
  FCanvas.LineTo(X2, Y2);
{$ELSE}
  FCanvas.DrawLine(PointF(X1, Y1), PointF(X2, Y2), 1);
{$ENDIF}
end;

function TsgContext.PatBlt(const X, Y, Width, Height: Integer;
  Rop: DWORD): Boolean;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
  Result := Windows.PatBlt(FCanvas.Handle, X, Y, Width, Height, Rop);
{$ELSE}
  Result := False;
{$ENDIF}
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgContext.RealizePalette;
begin
{$IFNDEF SG_FIREMONKEY}
{$IFNDEF USE_SG_AGG2D}
  {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ELSE}LCLIntf.{$ENDIF}RealizePalette(FCanvas.Handle);
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.Rectangle(const X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Rectangle(X1, Y1, X2, Y2);
end;
{ TODO: clipping region for AGG2D }
function TsgContext.RegionOffset(const ARegion: TsgRegion): TPoint;
begin
{$IFNDEF USE_SG_AGG2D}
  Result := ARegion.Offset(FCanvas);
{$ELSE}
  Result := Point(0, 0);
{$ENDIF}
end;

function TsgContext.RegionPaint(const ARegion: TsgRegion): Integer;
begin
{$IFNDEF USE_SG_AGG2D}
  Result := ARegion.Paint(FCanvas);
{$ELSE}
  Result := {$IFNDEF SG_NON_WIN_PLATFORM}Region_Error{$ELSE}RGN_ERROR{$ENDIF};
{$ENDIF}
end;

function TsgContext.RegionSelect(const ARegion: TsgRegion): Integer;
begin
{$IFNDEF USE_SG_AGG2D}
  Result := ARegion.Select(FCanvas);
{$ELSE}
  Result := {$IFNDEF SG_NON_WIN_PLATFORM}Region_Error{$ELSE}RGN_ERROR{$ENDIF};
{$ENDIF}
end;

function TsgContext.RegionUpdate(const ARegion: TsgRegion): Integer;
begin
{$IFNDEF USE_SG_AGG2D}
  Result := ARegion.Update(FCanvas);
{$ELSE}
  Result := {$IFNDEF SG_NON_WIN_PLATFORM}Region_Error{$ELSE}RGN_ERROR{$ENDIF};
{$ENDIF}
end;

function TsgContext.SelectPalette(APalette: THandle; ForceBackground: Boolean): THandle;
begin
{$IFNDEF SG_FIREMONKEY}
{$IFNDEF USE_SG_AGG2D}
  Result :=  {$IFDEF SGFPC}LCLIntf.{$ELSE}{$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}{$ENDIF}SelectPalette(GetDC, APalette, ForceBackground);
{$ELSE}
  Result := 0;
{$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TsgContext.SetBrushColor(const Value: TColor);
begin
{$IFDEF SG_FIREMONKEY}
  FCanvas.Fill.Color := Value.AsBGRA;
{$ELSE}
{$IFDEF USE_SG_AGG2D}
  FCanvas.Brush.FPColor := TColorToFPColor(Value);
{$ELSE}
  FCanvas.Brush.Color := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.SetBrushStyle(const Value: TBrushStyle);
begin
{$IFDEF SG_FIREMONKEY}
  case Value of
    bsSolid: FCanvas.Fill.Kind := TBrushKind.Solid;
    bsClear: FCanvas.Fill.Kind := TBrushKind.None;
    bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross:
      begin
        FCanvas.Brush.Bitmap.WrapMode :=  FMX.Graphics.TWrapMode.Tile;
        FCanvas.Brush.Bitmap.Bitmap := GetTile(FCanvas.Stroke.Color, Value);
        FCanvas.Fill.Kind := TBrushKind.Bitmap;
      end;
  end;
{$ELSE}
  FCanvas.Brush.Style := Value;
{$ENDIF}
end;

procedure TsgContext.SetCanvas(const Value: {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ELSE}TCanvas{$ENDIF});
{$IFDEF SG_DELPHI_VCL}
var
  DIB: PDIBSection;
  R: TRect;

  function GetSizeFromCliprect: TSize;
  begin
    R := FCanvas.ClipRect;
    Result.cx := R.Right - R.Left;
    Result.cy := R.Bottom - R.Top;
  end;
{$ENDIF}

begin
  if FCanvas <> Value then
  begin
{$IFDEF SG_FIREMONKEY}
    if Assigned(FCanvas) then
    begin
      FCanvas.Font.OnChanged := FFontChanged;
      FFontChanged := nil;
      if Assigned(FLayout) then
        FLayout.LayoutCanvas := nil;
    end;
{$ENDIF}
{$IFDEF SG_DELPHI_VCL}
    FObjectType := 0;
    FSize.cx := 0;
    FSize.cy := 0;
{$ENDIF}
    FCanvas := Value;
{$IFDEF SG_FIREMONKEY}
    if Assigned(FCanvas) then
    begin
      FCanvas.Stroke.Kind := TBrushKind.Solid;
      FFontChanged := FCanvas.Font.OnChanged;
      FCanvas.Font.OnChanged := FontChanged;
      if FLayout = nil then
        FLayout := TTextLayoutManager.TextLayoutByCanvas(FCanvas.ClassType).Create(FCanvas)
      else
        FLayout.LayoutCanvas := FCanvas;
      FLayout.Font.PixelsPerInch := PixelsPerInch;
    end;
{$ENDIF}
{$IFDEF SG_DELPHI_VCL}
    if Assigned(FCanvas) then
    begin
      FObjectType := GetObjectType(FCanvas.Handle);
      case FObjectType of
        OBJ_MEMDC:
          begin
            GetMem(DIB, SizeOf(TDIBSection));
            try
              if GetObject(GetCurrentObject(FCanvas.Handle, OBJ_BITMAP), SizeOf(TDIBSection), DIB) >= SizeOf(DIB^.dsBm) then
              begin
                FSize.cx := DIB^.dsBm.bmWidth;
                FSize.cy := DIB^.dsBm.bmHeight;
              end
              else
                FSize := GetSizeFromCliprect;
            finally
              FreeMem(DIB);
            end;
          end;
        OBJ_DC:
          if GetClientRect(WindowFromDC(FCanvas.Handle), R) then
          begin
            FSize.cx := R.Right - R.Left;
            FSize.cy := R.Bottom - R.Top;
          end
          else
            if Printer.Printing and (Printer.Handle = FCanvas.Handle) then
            begin
              FSize.cx := Printer.PageWidth;
              FSize.cy := Printer.PageHeight;
            end
            else
              FSize := GetSizeFromCliprect;
      else
        FSize := GetSizeFromCliprect;
      end;
    end;
{$ENDIF}
  end;
end;

procedure TsgContext.SetCharSet(const Value: TFontCharset);
begin
{$IFNDEF USE_SG_AGG2D}
  if Assigned(FCanvas) then
    FCanvas.Font.Charset := Value;
{$ENDIF}
end;

function TsgContext.DoSetPolyFillMode(PolyFillMode: Integer): Integer;
begin
{$IFNDEF  SG_NON_WIN_PLATFORM}
{$IFDEF USE_SG_AGG2D}
  Result := inherited DoSetPolyFillMode(PolyFillMode);
{$ELSE}
  Result := SetPolyFillMode(FCanvas.Handle, PolyFillMode);
{$ENDIF}
{$ELSE}
  Result := inherited DoSetPolyFillMode(PolyFillMode);
{$ENDIF}
end;

{$IFDEF SG_FIREMONKEY}
procedure TsgContext.SetFillGradient(AColor, AColor1: TColor; const AStPt, AEndPt: TFPoint;
  AStyle: TGradientStyle);
begin
  FCanvas.Fill.Gradient.Color := AColor.AsBGRA;
  FCanvas.Fill.Gradient.Color1 := AColor1.AsBGRA;
  FCanvas.Fill.Gradient.StartPosition.Point := TPointF.Create(AStPt.X, AStPt.Y);
  FCanvas.Fill.Gradient.StopPosition.Point := TPointF.Create(AEndPt.X, AEndPt.Y);
  FCanvas.Fill.Gradient.Style := AStyle;
  FCanvas.Fill.Kind := TBrushKind.Gradient;
end;
{$ENDIF}

procedure TsgContext.SetFontColor(const Value: TColor);
begin
{$IFDEF USE_SG_AGG2D}
  FCanvas.Font.FPColor := TColorToFPColor(Value);
{$ELSE}
  FCanvas.Font.Color := Value;
{$ENDIF}
end;

procedure TsgContext.SetFontHeight(const Value: Single);
begin
{$IFDEF USE_SG_AGG2D}
  FCanvas.Font.AggHeight := Value;
{$ELSE}
  FCanvas.Font.Height := Round(Value);
{$ENDIF}
end;

procedure TsgContext.SetFontStyle(const Value: TFontStyles);
begin
{$IFDEF USE_SG_AGG2D}
  FCanvas.Font.Bold := fsBold in Value;
  FCanvas.Font.Italic := fsItalic in Value;
  FCanvas.Font.Underline := fsUnderline in Value;
  FCanvas.Font.StrikeThrough := fsStrikeOut in Value;
{$ELSE}
  FCanvas.Font.Style := Value;
{$ENDIF}
end;

function TsgContext.SetGraphicsMode(const AMode: Integer): Integer;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF USE_SG_AGG2D}
  Result := 0;
{$ELSE}
  Result := Windows.SetGraphicsMode(FCanvas.Handle, AMode);
{$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TsgContext.SetPenColor(const Value: TColor);
begin
{$IFDEF SG_FIREMONKEY}
  FCanvas.Stroke.Color := Value.AsBGRA;
  //FCanvas.Stroke.Kind := TBrushKind.Solid;
{$ELSE}
{$IFDEF USE_SG_AGG2D}
  FCanvas.Pen.FPColor := TColorToFPColor(Value);
{$ELSE}
  FCanvas.Pen.Color := Value;
{$ENDIF}
{$ENDIF}
end;

procedure TsgContext.SetPenMode(const Value: TPenMode);
begin
{$IFNDEF SG_FIREMONKEY}
  FCanvas.Pen.Mode := Value;
{$ELSE}
{$ENDIF}
end;

procedure TsgContext.SetPenStyle(const Value: TPenStyle);
begin
{$IFDEF SG_FIREMONKEY}
  case Value of
    psSolid:      FCanvas.Stroke.Dash := TStrokeDash.Solid;
    psDash:       FCanvas.Stroke.Dash := TStrokeDash.Dash;
    psDot:        FCanvas.Stroke.Dash := TStrokeDash.Dot;
    psDashDot:    FCanvas.Stroke.Dash := TStrokeDash.DashDot;
    psDashDotDot: FCanvas.Stroke.Dash := TStrokeDash.DashDotDot;
    psClear:      ;
    psInsideFrame: ;
    psUserStyle: ;
    psAlternate: ;
  end;
{$ELSE}
  FCanvas.Pen.Style := Value;
{$ENDIF}
end;

procedure TsgContext.SetPenWidth(const Value: Single);
begin
{$IFDEF SG_FIREMONKEY}
  FCanvas.Stroke.Thickness := Ceil(Value);
{$ELSE}
{$IFDEF USE_SG_AGG2D}
  FCanvas.Pen.AggLineWidth := Value;
{$ELSE}
{$IFDEF SG_USE_AGG2D_AS_GDI}
  sgAGG2DIntf2.sgAgg2dWidgetSet.SetDCPenWidth(GetDC, Value);
{$ELSE}
  FCanvas.Pen.Width := Ceil(Value);
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function TsgContext.SetStretchBltMode(StretchMode: Integer): Integer;
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  Result := 0;
{$ELSE}
  Result :={$IFDEF SG_NON_WIN_PLATFORM}LCLIntf.{$ELSE}Windows.{$ENDIF}SetStretchBltMode(FCanvas.Handle, StretchMode);
{$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TsgContext.SetTextAlign(const AFlags: DWORD): DWORD;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF USE_SG_AGG2D}
  Result := 0;
{$ELSE}
  Result := Windows.SetTextAlign(FCanvas.Handle, AFlags);
{$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

{ TsgProxyExport }

procedure TsgProxyExport.DoEntityProcBegin(const Sender: TObject);
begin
  if Assigned(FExpEntityProc) then
    FExpEntityProc(Sender, 1);
end;

procedure TsgProxyExport.Clear;
begin
  FExpAnnotation := nil;
  FExpArc := nil;
  FExpClipRgn := nil;
  FExpCloseFigure := nil;
  FExpFillRgn := nil;
  FExpImage := nil;
  FExpImageUV := nil;
  FExpPixel := nil;
  FExpPolygon := nil;
  FExpPolyline := nil;
  FExpPolyPolygon := nil;
  FExpPolyPolyline := nil;
  FExpProgress := nil;
  FExpRestoreDC := nil;
  FExpSaveDC := nil;
  FExpSetColor := nil;
  FExpSetFont := nil;
  FExpSetPenGeometric := nil;
  FExpSetPenMiterLimit := nil;
  FExpSetPenWidth := nil;
  FExpSetStyle := nil;
  FExpText := nil;
  FExpTextOut := nil;
  FExpEntityProc := nil;
  SupportColorToLineWeight := False;
end;

procedure TsgProxyExport.DoAnnotation(const AEnt: TObject;
  const AParams: TsgExpAnnotParam);
begin
  if Assigned(FExpAnnotation) then
    FExpAnnotation(AEnt, AParams);
end;

procedure TsgProxyExport.DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer; const Arc: PsgArc);
begin
  if Assigned(FExpArc) then
    FExpArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TsgProxyExport.DoClipRgn(P: PRect; Count: Integer);
begin
  if Assigned(FExpClipRgn) then
    FExpClipRgn(P, Count);
end;

procedure TsgProxyExport.DoColor(AColor: TColor; AColorType: TsgColorType);
begin
  if Assigned(FExpSetColor) then
    FExpSetColor(AColor, AColorType);
end;

procedure TsgProxyExport.DoFillRgn(P: PRect; Count: Integer);
begin
  if Assigned(FExpFillRgn) then
    FExpFillRgn(P, Count);
end;

procedure TsgProxyExport.DoFont(AFont: TFont);
begin
  if Assigned(FExpSetFont) then
    FExpSetFont(AFont);
end;

procedure TsgProxyExport.DoImage(const R: TRect; AImage: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}{$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap{$ENDIF});
begin
  if Assigned(FExpImage) then
    FExpImage(R, AImage);
end;

procedure TsgProxyExport.DoImageUV(const APoint1, APoint2, APoint3,
  APoint4: TPoint; AImage: TGraphic);
begin
  if Assigned(FExpImageUV) then
    FExpImageUV(APoint1, APoint2, APoint3, APoint4, AImage);
end;

procedure TsgProxyExport.DoPenGeometric(AStyle: DWORD;
  const APenColor, APenWidth: Integer);
begin
  if Assigned(FExpSetPenGeometric) then
    FExpSetPenGeometric(AStyle);
end;

procedure TsgProxyExport.DoPenMiterLimit(ANewLimit: Single);
begin
  if Assigned(FExpSetPenMiterLimit) then
    FExpSetPenMiterLimit(ANewLimit);
end;

procedure TsgProxyExport.DoPenWidth(AWidth: Double);
begin
  if Assigned(FExpSetPenWidth) then
    FExpSetPenWidth(AWidth);
end;

procedure TsgProxyExport.DoPixel(APoint: TPoint; AColor: TColor);
begin
  if Assigned(FExpPixel) then
    FExpPixel(APoint, AColor);
end;

procedure TsgProxyExport.DoPolygon(Points: PPoint; Count: Integer);
begin
  if Assigned(FExpPolygon) then
    FExpPolygon(Points, Count);
end;

procedure TsgProxyExport.DoPolyline(Points: PPoint; Count: Integer);
begin
  if Assigned(FExpPolyline) then
    FExpPolyline(Points, Count);
end;

procedure TsgProxyExport.DoPolyPolygon(const Points; Counts: PInteger; Count: Integer);
begin
  if Assigned(FExpPolyPolygon) then
    FExpPolyPolygon(Points, Counts, Count);
end;

procedure TsgProxyExport.DoPolyPolyline(const Points; Counts: PInteger; Count: Integer);
begin
  if Assigned(FExpPolyPolyline) then
    FExpPolyPolyline(Points, Counts, Count);
end;

procedure TsgProxyExport.DoProgress;
begin
  if Assigned(FExpProgress) then
    FExpProgress;
end;

function TsgProxyExport.DoRestoreDC(const AIndex: Integer = 0): Boolean;
begin
  Result := False;
  if Assigned(FExpRestoreDC) then
    FExpRestoreDC;
end;

function TsgProxyExport.DoSaveDC: Integer;
begin
  Result := 0;
  if Assigned(FExpSaveDC) then
    FExpSaveDC;
end;

procedure TsgProxyExport.DoStyle(AStyle: Integer;
  AColorType: TsgColorType);
begin
  if Assigned(FExpSetStyle) then
    FExpSetStyle(AStyle, AColorType);
end;

procedure TsgProxyExport.DoText(ATextType: TsgTextType; X, Y: Integer; Options: Longint;
  Rect: PRect; StrWideChar: PWideChar; StrChar: PAnsiChar; Count: Integer;
  const Dx: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger);
begin
  if Assigned(FExpText) then
    FExpText(ATextType, X, Y, Options, Rect, StrWideChar, StrChar, Count, Dx);
end;

procedure TsgProxyExport.DoTextOutAW(X, Y: Integer; const ATextA: AnsiString;
  const ATextW: WideString; const ATextParams: PsgExpTextParam;
  ACount: Longint = 0);
begin
  if Assigned(FExpTextOut) then
    FExpTextOut(X, Y, ATextA, ATextW, ATextParams);
end;

// ÃÖ½Â¼± ¼öÁ¤
procedure TsgProxyExport.DoExportBlockAW(const blockString: string);
begin
  if Assigned(FExpExportBlock) then
    FExpExportBlock(blockString);
end;


procedure TsgProxyExport.DoEntityProcEnd(const Sender: TObject);
begin
  if Assigned(FExpEntityProc) then
   FExpEntityProc(Sender, 2);
end;

{ TsgContextSelection }

function TsgContextSelection.BeginScene: Boolean;
begin
  Result := inherited BeginScene;
  if Assigned(FSelectionMatrix) then
    FSelectionMatrix.BeginDraw;
end;

procedure TsgContextSelection.Changed;
begin
  FSelectionMatrix.Changed;
end;

procedure TsgContextSelection.DoPixel(APoint: TPoint; AColor: TColor);
begin
  FSelectionMatrix.BitPixel[APoint.X, APoint.Y] := AColor;
end;

procedure TsgContextSelection.EndScene;
begin
  inherited EndScene;
  if Assigned(FSelectionMatrix) then
    FSelectionMatrix.EndDraw;
end;

function TsgContextSelection.GetBrushColor: TColor;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := FSelectionMatrix.Brush.Color;
{$ELSE}
  Result := TAlphaColorRec.ColorToRGB(Canvas.Fill.Color);
{$ENDIF}
end;

function TsgContextSelection.GetBrushStyle: TBrushStyle;
begin
  Result := FSelectionMatrix.Brush.Style;
end;

function TsgContextSelection.GetPenColor: TColor;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := FSelectionMatrix.Pen.Color;
{$ELSE}
  Result := TAlphaColorRec.ColorToRGB(Canvas.Stroke.Color);
{$ENDIF}
end;

function TsgContextSelection.GetPenStyle: TPenStyle;
begin
  Result := FSelectionMatrix.Pen.Style;
end;

function TsgContextSelection.GetPenWidth: Single;
begin
  Result := FSelectionMatrix.Pen.Width;
end;

procedure TsgContextSelection.SetBrushColor(const Value: TColor);
begin
  //inherited SetBrushColor(Value);
  FSelectionMatrix.Brush.Color := Value;
end;

procedure TsgContextSelection.SetBrushStyle(const Value: TBrushStyle);
begin
  //inherited SetBrushStyle(Value);
  FSelectionMatrix.Brush.Style := Value;
end;

procedure TsgContextSelection.SetPenColor(const Value: TColor);
begin
  //inherited SetPenColor(Value);
  FSelectionMatrix.Pen.Color := Value;
end;

procedure TsgContextSelection.SetPenStyle(const Value: TPenStyle);
begin
  //inherited SetPenStyle(Value);
  FSelectionMatrix.Pen.Style := Value;
end;

procedure TsgContextSelection.SetPenWidth(const Value: Single);
begin
  //inherited SetPenWidth(Value);
  FSelectionMatrix.Pen.Width := Value;
end;

initialization
{$IFNDEF SG_FIREMONKEY}
  InitGradFill;
{$ENDIF}

finalization
{$IFNDEF SG_FIREMONKEY}
{$IFDEF MSWINDOWS}
  if Msimg32 <> 0 then
    FreeLibrary(Msimg32);
{$ENDIF}
{$ENDIF}

end.

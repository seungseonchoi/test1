{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   CAD export interface                     }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools�                        }
{************************************************************}

unit CADExport;
{$INCLUDE SGDXF.inc}
//{$DEFINE DEMO}
interface

uses

{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SG_HAS_ACTIVEX}
  ActiveX,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, FMX.Dialogs,
  SVG,
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, SysUtils, DXFConv, SHX, TTF, sgLists,
  CADImage, sgConsts, sgBitmap, sgFunction, sgXMLParser, sgContext,// CADtoSVG // 최승선 수정   CADtoSVG 추가
{$IFDEF SGDEL_XE2}
  System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF};

//For working with SaveToStream the definition sgFR should be uncommented.
//This is necessary for Fast Report Add-in.
//{$DEFINE sgFR}

type
  TsgBitmapExportMode = (bemNone, bem24bit,  bem1bit, bem8bit);

type
  PsgExportParams = ^TsgExportParams;
  TsgExportParams = record
    Version: TsgDWGVersion;     // TsgDWGVersion
    PageWidth: Double;    // mm
    PageHeight: Double;   // mm
    Margin: Double;       // mm
    DrawMode: Integer;    // color/bw/gray
    BackgroundColor: Integer;
    DefaultColor: Integer;
    NullWidth: Double;    // mm
    LineWeightScale: Double;
    SaveLineWeight: Boolean;
    UseHighQuality: Boolean;
    ExportTexts: Boolean;
    SHXAnnotations: Boolean;
    UseOutlines: Boolean; // Add bookmarks with layouts names
    UseSHXFonts: Integer;// -1 - as Converter.UseSHXFonts; 0 - not use shx; 1 - use shx
    TrueTypeOptimization: Boolean;
    LayoutExportMode: Integer;
    LayoutNameExportMode: string;
    Title, Author, Subjct, Keywords: string;
    LineWeights: string;  // Namevalueseprated commatext
    SizeAsExtents: Boolean;
    MMInCad: Double;
    UnitsType: TsgInsUnits;
    UseVectorLineWeight: Boolean;
    UseExtentsFromPlotSettings: Boolean;
    HPGLXScale: Double;
    WritePJLHeader: Boolean;
    UsePECommand: Boolean;
    IsConvertImageToOLE: Boolean;
    ConvertAcis: Boolean;//will be saved in the old version
                                       //of the recording for ACIS
    FillingEntity: Boolean;  // Used in TsgConvertMetafileToCad class
    OffsetPoint: TF2DPoint;
    AlternateBlack: Boolean;
    UnitSize: Double;
    Use01MM: Boolean;
    LayersMode: Integer;
    Scale: Double;
    ACISColorFlags: Int64;  //0 bit - WriteTclColors, 1 bit - WriteAdskColors, 2 bit - WriteAdskIndexColors
    CreateMtl : Boolean;
{$IFDEF SG_VECTORIZATION}
    VectorizationMode: Integer;
{$ENDIF}
    //For GCode Export
    AbsoluteCoordIJ: Boolean;
    ArcToLines: Boolean;
    CodeOptimize: Boolean;
    PositioningSystemID: Integer; //0 - G90 absolute, 1 - G91 incremental
    MachineTypeID: Integer;       // need cast to TsgGCodeMachineTypeID from Unit Gcode
    PassesDirectionID: Integer;   // need cast to TsgGPassesDirection from Unit Gcode
    PrecisionDigits: Byte;
    PrecisionFactor: Byte;
    FeedOnZ: Integer;             // mm/min
    FeedOnXY: Integer;            // mm/min
    FeedOnXZ: Double;             // mm/rev
    SpindleSpeed: Integer;        // rpm
    DepthOnZ: Double;             // mm
    DepthPass: Double;            // mm
    PassInfeedDepth: Boolean;
    NumbOfPasses: Byte;
    DepartureOnZ: Double;         // mm
    WorkpieceZeroPointID: Byte;   // nedd cast to TsgGCodeZeroPointID from Unit Gcode
    ZeroPointOffsetX: Double;
    ZeroPointOffsetY: Double;
    LaserOnCommand: string;
    LaserOffCommand: string;
    UseLaserPower: Boolean;
    LaserPower: Integer;          // 0-1000
    MaxLaserPower: Integer;       // 0-1000
    Delay: Integer;               // ms
    MachineUnitsID: Byte;         //need cast to TsgGUnits from Unit Gcode
    DrawingUnitsID: Byte;         //need cast to TsgGUnits from Unit Gcode
    AddLabelOfProgam: Boolean;
    LabelOfProgram: string;
    AddNumbering: Boolean;
    ShowContourName: Boolean;
    ShowLayerName: Boolean;
    ShowPercent: Boolean;
    StartNumber: Integer;
    StepOfNumbering: Integer;
    Tools: string;
    GcodeExtID: Integer;
    TrailingZeros: Boolean;
    ConvertOnlyVisibleLayers: Boolean;
    StartFromZeroPositon: Boolean;
    ReturnToZeroPoints: Boolean;
    RetractTool: Boolean;
    // GRBL
    ForceLaserOffGRBL: Boolean;
    //GCode Dispenser
    TurnOffPumpBefore: Boolean;
    TurnOffPumpBeforeValue: Double;
    DispenserOffCommand: string;
    DispenserOnCommand: string;
    PumpOffCommand: string;
    PumpOnCommand: string;
    StraightenContour: Boolean;
    MergePoints: Boolean;
    MergePointsRadius: Double;
    AxisYtoZ: Boolean;
    ColorToLineWeight: string;//
    // for import from metafile
    ExtData: Pointer;
    XMLParamsExtended: string;
    TTFMode: Integer;
    PdfBitmapMode: TsgBitmapExportMode;
{$IFDEF SG_BTI}
    UseFillStyleIndex: Boolean;
{$ENDIF}
    SourceGraphicWidth: Double;
    SourceGraphicHeight: Double;
    ColorToLineWeightMode: Integer;//1 - ApplyPrintStylesToRGB
    LineTypeMode: Integer;//0 - default, 1 - export, 2 - export as solid
    NullWeightMode: Integer;//for svg export: 1- weight is not used while exporting,  2 - NullWidth is not used
    IgnoreTexts: Boolean;
    IgnoreImages: Boolean;
    IgnoreLayerNoPlotting: Boolean;
    ColorSpace: TsgColorSpace;
    TextAsPolygons: Boolean;
    FillBackground: Boolean;//uset in svg Transparency = not FillBackground;
  end;

const
  DefIsConvertImageToOLE = {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF};

  DefExportFormat: TsgExportFormat = efDxf;//only DXF/DWG

{$IFDEF SG_FIREMONKEY}

{$ELSE}
  DefExportParams: TsgExportParams = (
    Version: cnstSaveCADVersionDefault;
    PageWidth: 210;       // mm
    PageHeight: 297;      // mm
    Margin: 5;            // mm
    DrawMode: 0;          // color
    BackgroundColor: clWhite;
    DefaultColor: clBlack;
    NullWidth: 0.3;       // mm
    LineWeightScale: 1;
    SaveLineWeight: True;
    UseHighQuality: False;
    ExportTexts: True;
    SHXAnnotations: True;
    UseOutlines: True;
    UseSHXFonts: -1;
    TrueTypeOptimization: False;
    LayoutExportMode: 3;  //TsgLayoutExportMode.lemAllPaperSpaces
    LayoutNameExportMode: '';
    Title: '';
    Author: cnstCompanyCST;
    Subjct: '';
    Keywords: '';
    LineWeights: '';
    SizeAsExtents: False;
    MMInCad: 1;
    UnitsType: iuMillimeters;
    UseVectorLineWeight: False;
    HPGLXScale: 40;
    WritePJLHeader: True;
    UsePECommand: True;
    IsConvertImageToOLE: DefIsConvertImageToOLE;
    FillingEntity: True;
    OffsetPoint: (X: 0; Y: 0);
    AlternateBlack: True;
    UnitSize: 1.0;
    Use01MM: True;
    LayersMode: 1;
    Scale: 0;
    ACISColorFlags: 0;
    CreateMtl: True;
{$IFDEF SG_VECTORIZATION}
    VectorizationMode: 2;
{$ENDIF}
    //For GCode Export
    AbsoluteCoordIJ: False;
    ArcToLines: False;
    CodeOptimize: True;
    PositioningSystemID: 0;    //0 - G90 absolute, 1 - G91 incremental
    MachineTypeID: 0;        // need cast to TsgGCodeMachineTypeID from Unit Gcode
    PassesDirectionID: 0;    // need cast to TsgGPassesDirection from Unit Gcode
    PrecisionDigits: 3;
    PrecisionFactor: 1;
    FeedOnZ: 150;            // mm/min
    FeedOnXY: 450;           // mm/min
    FeedOnXZ: 0.2;           // mm/rev
    SpindleSpeed: 3000;      // rpm
    DepthOnZ: -2;            // mm
    DepthPass: 2;            // mm
    PassInfeedDepth: False;
    NumbOfPasses: 1;
    DepartureOnZ: 5;         // mm
    WorkpieceZeroPointID: 0; // need cast to TsgGCodeZeroPointID from Unit Gcode
    ZeroPointOffsetX: 0;
    ZeroPointOffsetY: 0;
    LaserOnCommand: 'M3';
    LaserOffCommand: 'M5';
    UseLaserPower: False;
    LaserPower: 128;         // 0-1000
    Delay: 0;                // ms
    MachineUnitsID: 0;       //need cast to TsgGUnits from Unit Gcode
    DrawingUnitsID: 0;       //need cast to TsgGUnits from Unit Gcode
    AddLabelOfProgam: False;
    LabelOfProgram: '001';
    AddNumbering: False;
    ShowContourName: True;
    ShowLayerName: False;
    ShowPercent: True;
    StartNumber: 5;
    StepOfNumbering: 5;
    Tools: '1/2/10';
    GcodeExtID: 0;           //need cst to TsgGcodeExts
    TrailingZeros: False;
    ConvertOnlyVisibleLayers: True;
    StartFromZeroPositon: False;
    ReturnToZeroPoints: True;
    RetractTool: False;
    //GRBL
    ForceLaserOffGRBL: False;
    //GCode Dispenser
    TurnOffPumpBefore: False;
    TurnOffPumpBeforeValue: 0;
    DispenserOffCommand: 'M9';
    DispenserOnCommand: 'M7';
    PumpOffCommand: 'M11';
    PumpOnCommand: 'M10';
    StraightenContour: True;
    MergePoints: False;
    MergePointsRadius: 0.05;
    AxisYtoZ: True;
    // for import from metafile
    ExtData: nil;
    XMLParamsExtended: '';
    TTFMode: 0;
    PdfBitmapMode: bemNone;
{$IFDEF SG_BTI}
    UseFillStyleIndex: False;
{$ENDIF}
    SourceGraphicWidth: -1;
    SourceGraphicHeight: -1;
    ColorToLineWeightMode: 0;
    LineTypeMode: 0;
    NullWeightMode: 0;
    IgnoreTexts: False;
    IgnoreImages: False;
    IgnoreLayerNoPlotting: False;
    TextAsPolygons: False;
    FillBackground: False;
    );
{$ENDIF}

  cnstPageSizes: array [0..39] of string
    = (
    'A0 1189 x 841 (46.81 x 33.11 ")',
    'A1 841 x 594 %s (33.11 x 23.39 ")',
    'A2 594 x 420 %s (23.39 x 16.54 ")',
    'A3 420 x 297 %s (16.54 x 11.69 ")',
    'A4 297 x 210 %s (11.69 x 8.27 ")',
    'A5 210 x 148 %s (8.27 x 5.83 ")',
    'A6 148 x 105 %s (5.83 x 4.13 ")',
    'A7 105 x 74 %s (4.13 x 2.91 ")',
    'ANSI A 11 x 8.5 " (279.4 x 215.9 %s)',
    'ANSI B 17 x 11 " (431.8 x 279.4 %s)',
    'ANSI C 22 x 17 " (538.8 x 431.8 %s)',
    'ANSI D 34 x 22 " (863.6 x 558.8 %s)',
    'ANSI E 44 x 34 " (1117.6 x 863.6 %s)',
    'ANSI E1 68 x 44 " (1727.2 x 1117.6 %s)',
    'ANSI F 40 x 28 " (1016 x 711.2 %s)',
    'ARCH A 12 x 9 " (304.8 x 228.6 %s)',
    'ARCH B 18 x 12 " (457.2 x 304.8 %s)',
    'ARCH C 24 x 18 " (609.6 x 457.2 %s)',
    'ARCH D 36 x 24 " (914.4 x 609.6 %s)',
    'ARCH E 48 x 36 " (1219.2 x 914.4 %s)',
    'B0 1414 x 1000 %s (55.67 x 39.37 ")',
    'B1 1000 x 707 %s (39.37 x 27.83 ")',
    'B2 707 x 500 %s (27.83 x 19.69 ")',
    'B3 500 x 353 %s (19.69 x 13.90 ")',
    'B4 353 x 250 %s (13.90 x 9.84 ")',
    'B5 250 x 176 %s (9.84 x 6.93 ")',
    'B6 176 x 125 %s (6.93 x 4.92 ")',
    'C0 1297 x 917 %s (51.06 x 36 ")',
    'C1 917 x 648 %s (36 x 25.60 ")',
    'C2 648 x 458 %s (25.51 x 18.03 ")',
    'C3 458 x 324 %s (18.03 x 12.75 ")',
    'C4 324 x 229 %s (12.76 x 9.02 ")',
    'C5 229 x 162 %s (9.02 x 6.38 ")',
    'C6 162 x 114 %s (6.38 x 4.49 ")',
    'Flsa 330.2 x 215.9 %s',
    'Half Letter 8.5 x 5.5 " (215.9 x 139.7 %s)',
    'Legal 14 x 8.50 " (355.6 x 215.9 %s)',
    'Letter 11 x 8.50 " (279.4 x 215.9 %s)',
    'Note 254 x 190.5 %s',
    'Tabloid / Ledger 17 x 11 " (431.8 x 279.4 %s)'
   );

type
  TsgLayoutExportMode = (lemModel, lemAllLayouts, lemLayoutByName, lemAllPaperSpaces, lemCurrentLayout,
    lemLayoutsByIndexes);

  TsgApplyBlocksProc = procedure(ABlock: TsgDXFBlock; const AName: string;
    Data: Pointer; ACurrPaperSpaceIndex: Integer = -1) of object;

  TsgSHXFontsProc = function: TsgSHXFontList of object;
  TsgRgnProc = procedure(P: PRect; Count: Integer) of object;
  TsgLayoutProc = procedure(Index: Integer; ALayout: TsgDXFLayout) of object;

  TsgCADBasePolyLineAccess = class(TsgCADBasePolyline);
  TsgCADCurvePolygonAccess = class(TsgCADCurvePolygon);

  // Obsolete type
  TScaling = record
    Offset: TFPoint;
    Scale: TFPoint;
    DX: Integer;
    DY: Integer;
  end;

  PsgCanvasParams = ^TsgCanvasParams;
  TsgCanvasParams = record
    Brush: TBrush;
    Font: TFont;
    Pen: TPen;
  end;

  TsgCalcCoordsStruct = record
    XScaled: Extended;
    YScaled: Extended;
    X: TsgFloat;
    Y: TsgFloat;
    Rot: TFMatrix;
    RealLayoutCenter: TFPoint;
    NullCoord: Integer;
  end;

  TsgDistanceUnits = (duNone, duInch, duMM);
  TsgAngularUnits = (auNone);
  TsgMeasurement = record
    DistanceUnits: TsgDistanceUnits;
    AngularUnits: TsgAngularUnits;
  end;

  TsgCommonExportParams = class
  private
    FClipBoardFormat: Word;
    FDestination: TsgStorage;
    FDestinationCookie: Integer;
    FOnBeforeSaveToStream: TNotifyEvent;
    FOnAfterSaveToStream: TNotifyEvent;
    FXmlParams: Boolean;
    procedure InitDefaultCommonExportParams(AStorage: TsgStorage;
      const ADefaultParams: TsgCommonExportParams);
    function GetSaveFileName: string;
    procedure SetSaveFileName(const Value: string);
    procedure StorageDestroy(Sender: TObject);
    procedure SetDestination(const Value: TsgStorage);
    function GetDestination: TsgStorage;
  protected
    class procedure SetExportParamsDefault(var ACADExportParams: TsgExportParams;
      var ARasterExportParams: TsgRastrExportParams);
    procedure DoBeforeSaveToStream(Sender: TObject); dynamic;
    procedure DoAfterSaveToStream(Sender: TObject); dynamic;
    procedure AssignTo(const AExp: TsgCommonExportParams; const AParams: Int64 = 0);
  public
    Format: TsgExportFormat;
    Cliping: TsgClipping;
    CADExportParams: TsgExportParams;
    RasterExportParams: TsgRastrExportParams;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AObject: TsgCommonExportParams; const AIgnoreDestination: Boolean = False);
    procedure CheckVersionOnFormat;
    function FromNode(const AExportParamsNode: TsgNode;
      AStorage: TsgStorage = nil;
      const ADefaultParams: TsgCommonExportParams = nil): Boolean;
    function FromString(const AExportParam: string;
      AStorage: TsgStorage = nil;
      const ADefaultParams: TsgCommonExportParams = nil): Boolean;
    function IsXmlParams: Boolean;
    function IsDestinationFile: Boolean;
    function IsSupportVersionOnFormat: Boolean;
    function ReadFormatFromNode(const AExportParamsNode: TsgNode): TsgExportFormat;
    function TryCreateMultipageImage(const AExportParamsNode: TsgNode;
      out ANewImages{array of object}): TObject;
    function Save(AObject: TPersistent): Boolean;
    procedure SetDefaultParams;
    procedure SetCadVersion(const AStr: string; const AOnlyDigits: Boolean = False);
    property ClipBoardFormat: Word read FClipBoardFormat write FClipBoardFormat;
    property SaveFileName: string read GetSaveFileName write SetSaveFileName;
    property Destination: TsgStorage read GetDestination write SetDestination;
    property OnBeforeSaveToStream: TNotifyEvent read FOnBeforeSaveToStream write FOnBeforeSaveToStream;
    property OnAfterSaveToStream: TNotifyEvent read FOnAfterSaveToStream write FOnAfterSaveToStream;
  end;

  TsgCADExport = class(TInterfacedPersistent, IStreamPersist)
  private
    FAppName: string;
  protected
    FFileName: string;
    FIgnoreFileName: Boolean;
    FOnProgress: TProgressEvent;
    FSGBlock: TsgDXFBlock;
{$IFDEF SGFPC}
    FContinue: Boolean;
{$ENDIF}
    procedure DoInitializeView(var AView: TFMatrix); virtual;
    procedure AddSGInserts(const ALayout: TsgDXFLayout; const AInserts: TsgObjectList); //{$IFDEF SG_INLINE} inline; {$ENDIF}
    procedure SaveToStreamCustom(S: TStream); virtual; abstract;
    procedure AfterExport(const S: TStream); virtual;
    procedure BeforeExport(const S: TStream); virtual;
    function ExpSGInsert(const ALayout: TsgDXFLayout): TsgObjectList;
    procedure DoError(Error: Boolean; const ErrorMessage: string); overload;
    procedure DoError(Error: Boolean; const ErrorMessage: string;
      const Args: array of const); overload;
    function GetSourceId: UInt64;
    function IsFormatMultiPages: Boolean; virtual;
    procedure LoadSGBlock; virtual;
    function GetConverter: TsgDXFConverter; virtual;
    function GetExportExceptionClass: ExceptClass; virtual;
    procedure InitializeVersionData(const AIsTrial: Boolean;
      const AMessage: string; AppId: string = ''); virtual;
    procedure LoadFromStream(S: TStream); virtual;
    procedure Progress(Stage: TProgressStage; PercentDone: Byte; const Msg: string); virtual;
    procedure SetAppName(const AValue: string);
    property Converter: TsgDXFConverter read GetConverter;
  public
    constructor Create(ACADImage: TsgCADImage); virtual;
    destructor Destroy; override;
    class function ExportType: TsgExportFormat; virtual;
    procedure ApplyParams(const AExportParams: TsgExportParams); virtual;
    procedure SaveToStream(S: TStream); virtual;
    procedure SaveToFile(const AFileName: String); virtual;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property AppName: string read FAppName write SetAppName;
    property IgnoreFileName: Boolean read FIgnoreFileName write FIgnoreFileName;//for internal using
  end;

  TsgCADExportClass = class of TsgCADExport;

  TsgSimpleExportCustom = class(TsgCADExport)
  private
    FCADImage: TsgCADImage;  //evg
    function GetLayout(Index: Integer): TsgDXFLayout;
    function GetLayoutsCount: Integer;
  protected
    FInternalParams: TsgCADIterate;
    FParams: PsgCADIterate;
    function InitializeIterateParams: PsgCADIterate; virtual;
    procedure DoInitializeView(var AView: TFMatrix); override;
    function GetCurrentLayout: TsgDXFLayout;
    procedure SetCurrentLayout(const ALayout: TsgDXFLayout);
    procedure SetImage(const AImage: TsgCADImage);//for internnal using
    property CADImage: TsgCADImage read FCADimage;
    property Params: PsgCADIterate read FParams;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    function GetLayoutByName(const AName: string): TsgDXFLayout;
    property CurrentLayout: TsgDXFLayout read GetCurrentLayout write SetCurrentLayout;
    property Layouts[Index: Integer]: TsgDXFLayout read GetLayout;
    property LayoutsCount: Integer read GetLayoutsCount;
  end;

  TsgSimpleExportImage = class(TsgSimpleExportCustom)
  protected
    function GetBackgroundColor: TColor;
    function GetDefaultColor: Integer;
    function GetDrawMode: TsgDXFDrawMode;
    function GetIsShowLineWeight: Boolean;
    function GetLineWeightScale: TsgFloat;
    function GetNullWidth: Double;
{$IFDEF SG_BTI}
    function GetUseFillStyleIndex: Boolean;
{$ENDIF}
    procedure SetBackgroundColor(Value: TColor);
    procedure SetDefaultColor(const Value: Integer);
    procedure SetDrawMode(const Value: TsgDXFDrawMode);
    procedure SetIsShowLineWeight(Value: Boolean);
    procedure SetLineWeightScale(Value: TsgFloat);
    procedure SetNullWidth(Value: Double);
{$IFDEF SG_BTI}
    procedure SetUseFillStyleIndex(AValue: Boolean);
{$ENDIF}
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    function AbsHeight: Extended;
    function AbsWidth: Extended;
    procedure GetExtents;
    function GetFPoint(const P: TFPoint): TFPoint;
    function GetPoint(const P: TFPoint): TPoint;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property DefaultColor: Integer read GetDefaultColor write SetDefaultColor;
    property DrawMode: TsgDXFDrawMode read GetDrawMode write SetDrawMode;
    property IsShowLineWeight: Boolean read GetIsShowLineWeight write SetIsShowLineWeight;
    property LineWeightScale: TsgFloat read GetLineWeightScale write SetLineWeightScale;
    property NullWidth: Double read GetNullWidth write SetNullWidth;
{$IFDEF SG_BTI}
    property UseFillStyleIndex: Boolean read GetUseFillStyleIndex write SetUseFillStyleIndex;
{$ENDIF}
    //procedure DoExportBlockAW(const blockString: string); override;     //최승선 수정
  end;

  TsgSimpleExport = class(TsgSimpleExportImage)
  private
    FEntitiesStack: TsgObjectList;
    FColor: TColor;
    FCount: Integer;
    FCurrent: Integer;
    FStep: Integer;
    FCurrentStep: Integer;
    FFillColor: TColor;
    FFontColor: TColor;
    FLayoutExportMode: TsgLayoutExportMode;
    FLayoutNameExportMode: string;
    FMiterLimit: Single;
    FXMLParamsExtended: string;
    FXScale: Double;
    FXSize: Integer;
    FPenStyle: DWORD;
    FPenWidthInt: Double;
    FStampBlock: TsgDXFBlock;
    FStampBoldStyle: TsgDXFStyle;
    FStampDefStyle: TsgDXFStyle;
    FTempEntities: TsgObjectList;
    FUsePlotSetting: Boolean;
    FLayoutsExportCount: Integer;
    FOnPageEnd: TNotifyEvent;
    FOnPageStart: TNotifyEvent;
    FSelectedLayouts: TsgStringList;
    FColorToLineWeight: TStringList;
    FColorToLineWeightMode: Integer;
    FIgnoreLayerNoPlotting: Boolean;
    FTTFMode: TsgTTFMode;
    procedure CntLayout(Index: Integer; ALayout: TsgDXFLayout);
    procedure CntSGBlock;
    procedure CreateStampBlock;
    function DrawInsert(Sender: TObject): Integer;
    procedure EnterInsert(Sender: TObject);
    procedure EnterViewport(Sender: TObject);
    procedure EnterXRef(Sender: TObject);
    procedure ExpStampInsert(const ALayout: TsgDXFLayout; var AList: TsgObjectList);
    function GetBorderSize: TsgBorderSize;
    function GetBorderType: TsgBorderType;
    function GetDimensionsVisible: Boolean;
    function GetExtent: TFRect;
    function GetIsWithoutBorder: Boolean;
    function GetLayoutExportModeInt: TsgLayoutExportMode;
    function GetLayoutsExportCount: Integer;
    function GetMiterLimit: Single;
    function GetPenStyle: DWORD;
    function GetPenWidth: Double;
    function GetTextVisible: Boolean;
    function GetUseSHXFonts: Boolean;
    function GetColorToLineWeightMode: Boolean;
    function GetIgnoreLayerNoPlotting: Boolean;
    function IncCount(Entity: TsgDXFEntity): Integer;
    procedure Iterate(Proc: TsgLayoutProc);
    procedure ReadResults(const AResults: IsgResultNode);
    procedure ReadXMLResults(ANode: TsgNode);
    procedure SetBorderType(Value: TsgBorderType);
    procedure SetDimensionsVisible(AVisible: Boolean);
    procedure SetIsWithoutBorder(Value: Boolean);
    procedure SetLayoutExportMode(const Value: TsgLayoutExportMode);
    procedure SetTextVisible(AVisible: Boolean);
    procedure SetUsePlotSetting(const Value: Boolean);
    procedure SetUseSHXFonts(const AValue: Boolean);
    procedure SetApplyPrintStylesToRGB(const Value: Boolean);
    procedure SetIgnoreLayerNoPlotting(const Value: Boolean);
  protected
    FPenWidth: Double;
    FLineWeightScaleByExport: Double;
    FUseSHXFonts: Integer;
    procedure ApplyTTFMode; virtual;
    procedure CheckColorToLineWeight(ACADImage: TsgCADImage);
    procedure SetExpProc(ACADImage: TsgCADImage);
    procedure SetExpProcProxy(const AExpProcs: TsgProxyExport);
    procedure ClearExpProc(ACADImage: TsgCADImage);
    procedure DoDrawCurrentLayout; virtual;
    procedure ExpCloseFigure; virtual;
    procedure ExpLayout(Index: Integer; ALayout: TsgDXFLayout); virtual;
    function DoFinish(Entity: TsgDXFEntity): Integer; virtual;
    procedure DoExpPathData(const APathData: TObject;
      const Edge, Fill: Boolean); virtual;
    procedure DoExpPolyPoints(const APointsLists: TsgObjectList;
      const Edge, Fill: Boolean);
    procedure ExpClipRgn(P: PRect; Count: Integer); virtual;
    procedure ExpEntityProc(AEnt: TObject; const AState: Integer); virtual;
    procedure ExpFillRgn(P: PRect; Count: Integer); virtual;
    procedure ExpImage(const R: TRect; AImage: TPersistent); virtual;
    procedure ExpImageUV(const APoint1, APoint2, APoint3,  APoint4: TPoint;
      AImage: TPersistent); virtual;
    procedure ExpPixel(APoint: TPoint; AColor: TColor); virtual;
    procedure ExpPolygon(Points: PPoint; Count: Integer); virtual;
    procedure ExpPolyline(Points: PPoint; Count: Integer); virtual;
    procedure ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer); virtual;
    procedure ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer); virtual;
    procedure ExpRestoreDC; virtual;
    procedure ExpSaveDC; virtual;
    procedure ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam); virtual;
    procedure ExpExportBlock(const blockString: string); virtual;    // 최승선 수정

    procedure ExpProgress; virtual;
    procedure ExpSetColor(AColor: TColor; AColorType: TsgColorType); virtual;
    procedure ExpSetPenMiterLimit(ANewLimit: Single); virtual;
    procedure ExpSetPenGeometric(AStyle: DWORD); virtual;
    //procedure ExpSetColorBrush(AColor: TColor);
    procedure ExpSetFont(AFont: TFont); virtual;
    procedure ExpSetPenWidth(AWidth: Double); virtual;
    procedure ExpAnnotation(AEnt: TObject; const AParams: TsgExpAnnotParam); virtual;
    procedure ExpPathDataProc(const AHandle: THandle; const Edge, Fill: Boolean);
    function IsSupportColorToLineWeight: Boolean; virtual;
    function IsSupportPathData: Boolean; virtual;
    function GetBitmapMode: TsgBitmapExportMode; virtual;
    function GetBitmapRect: TRect; virtual;
    function GetClippingRect: TFRect; virtual;
    function GetConverter: TsgDXFConverter; override;
    function GetExportRect: TRect; virtual;
    function GetLineTypeMode: Integer; virtual;
    function GetFillColor: TColor; virtual;
    function GetFontColor: TColor; virtual;
    function GetLineScale: Double; virtual;
    function GetMmToPixelX: Double; virtual;
    function GetStrokeColor: TColor; virtual;
    function GetImageType: TsgExportFormat;
    function GetTTFMode: TsgTTFMode;//virtual;
    function GetTransparency: Boolean; virtual;
    function Flipping: Boolean; virtual;
    function HasClippingRect: Boolean;
    procedure PageEnd(N: Integer); virtual;
    procedure PageStart(N: Integer); virtual;
    function RotPict(AGraphic: TGraphic; Angle: Integer): Boolean; virtual;
    function ReloadTexts: Boolean;
    procedure SaveToStreamCustom(S: TStream); override;
    procedure SetColor;
    procedure SetFColor;
    procedure SetFillColor(AColor: TColor); virtual;
    procedure SetStrokeColor(AColor: TColor); virtual;
    procedure SetParamsForImage(const AImage: TsgCADImage); virtual;
    procedure SetNormalsInternal(const AX, AY: Integer);
    procedure SetTTFMode(const Value: TsgTTFMode);//virtual;
    procedure SetTransparency(const AValue: Boolean); virtual;
    procedure SetLineTypeMode(const AValue: Integer); virtual;
    function IsGreatImage(const AImage: TPersistent): Boolean; virtual;
    property ClippingRect: TFRect read GetClippingRect;
    property XMLParamsExtended: string read FXMLParamsExtended;
    property OnPageStart: TNotifyEvent read FOnPageStart write FOnPageStart;
    property OnPageEnd: TNotifyEvent read FOnPageEnd write FOnPageEnd;
    property EntitiesStack: TsgObjectList read FEntitiesStack;
    property PenWidthInt: Double read FPenWidthInt;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    procedure SaveToStream(S: TStream); override;
    property ApplyPrintStylesToRGB: Boolean read GetColorToLineWeightMode write SetApplyPrintStylesToRGB;
    property BorderSize: TsgBorderSize read GetBorderSize;//FBorderSize write FBorderSize;
    property BorderType: TsgBorderType read GetBorderType write SetBorderType;
    property DimensionsVisible: Boolean read GetDimensionsVisible write SetDimensionsVisible;
    property Extents: TFRect read GetExtent;
    property IsWithoutBorder: Boolean read GetIsWithoutBorder write SetIsWithoutBorder;
    property LayoutExportMode: TsgLayoutExportMode read FLayoutExportMode write SetLayoutExportMode;
    property LayoutNameExportMode: string read FLayoutNameExportMode write FLayoutNameExportMode;
    property LayoutsExportCount: Integer read FLayoutsExportCount;
    class function LayoutNameSeparator: Char;
    property MiterLimit: Single read GetMiterLimit;
    property PenStyle: DWORD read GetPenStyle;
    property PenWidth: Double read GetPenWidth;
    property TTFMode: TsgTTFMode read GetTTFMode write SetTTFMode;
    property TextVisible: Boolean read GetTextVisible write SetTextVisible;
    property XScale: Double read FXScale write FXScale;
    property XSize: Integer read FXSize write FXSize;
    property UsePlotSetting: Boolean read FUsePlotSetting write SetUsePlotSetting;
    property UseSHXFonts: Boolean read GetUseSHXFonts write SetUseSHXFonts;//use in pdf export
    property LineTypeMode: Integer read GetLineTypeMode write SetLineTypeMode;
    property IgnoreLayerNoPlotting: Boolean read GetIgnoreLayerNoPlotting write SetIgnoreLayerNoPlotting;
    property Transparency: Boolean read GetTransparency write SetTransparency;
  end;

  TsgSimpleExportMetafile = class(TsgSimpleExportImage)
  protected
    procedure SaveToStreamCustom(S: TStream); override;
  public
    property CADImage;
  end;

  TsgMultipageExportImage = class(TsgCADImage)
  private
    FLayouts: TsgDXFGroup;
    FPagesExportParams: TStringList;
  protected
    function CreateConverter: TsgDXFConverter; override;
    procedure PageStart(Sender: TObject);
    procedure PageEnd(Sender: TObject);
    procedure InitializeMultipage(const ACADImages: array of TsgCADImage;
      const AExportParamsList: array of TsgExportParams);
    property PagesExportParams: TStringList read FPagesExportParams;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function CreateMultipage(const ACADImages: array of TsgCADImage;
      const AExportParamsList: array of TsgExportParams): TsgCADImage;
  end;

  TsgMultipageSimpleExport = class(TsgSimpleExport)
  public
    constructor Create(ACADImage: TsgCADImage); override;
    constructor CreateMultipage(const ACADImages: array of TsgCADImage;
      const AExportParamsList: array of TsgExportParams); overload;
  end;

function GetXMLNodeTextAsVersion(const ANode: TsgNodeSample;
  const ADefaultValue: TsgDWGVersion = acR2004): TsgDWGVersion;
function GetExportParamsNode(const AParser: TsgParser): TsgNode;

function DerefStream(S: TStream): TCustomMemoryStream;
{$IFDEF SG_FIREMONKEY}
function DefExportParams: TsgExportParams;
{$ENDIF}

var
  Drawing: TsgSimpleExport;
  MinDXFSize: Integer;

implementation

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  ComObj, sgOle,
{$ENDIF}
  Math, sgLines,
  XMLInterface;

const
  cnstTrialMessagesCount: Integer = 3;
  cnstMultWidthHeightMax = 5000*5000;

type
  PTriVertex = ^TTriVertex;
  TTriVertex = record
    X,Y: Integer;
    Red,Green,Blue,Alpha: Word;
  end;
  TGradFill = function(DC: THandle; PV: PTriVertex; NumV: Integer; PMesh: Pointer; NMesh,Mode: Integer): LongBool; stdcall;
  TsgDXFPointAccess = class(TsgDXFPoint);
  TsgConverterAccess = class(TsgDXFConverter);
  TsgStorageAccess = class(TsgStorage);

  TsgDXFInsertProtect = class(TsgDXFInsert)
  private
    FParent: TsgDXFEntity;
    procedure SetParent(const Value: TsgDXFEntity);
  protected
    property Parent: TsgDXFEntity read FParent write SetParent;
    destructor Destroy; override;
  end;

  TsgDXFMultipageLayouts = class(TsgDXFDictionary)
  protected
    procedure ListNotify(const Obj: TObject; Action: TListNotification); override;
  end;

  TsgDXFMultipageConverter = class(TsgDXFConverter)
  protected
    procedure ClearBegin; override;
  end;

  TsgMultipageExportParams = class
    ExportParams: TsgExportParams;
  end;

  TEntityAccess = class(TsgDXFEntity);
  TBlockAccess = class(TsgDXFBlock);
  TsgCADImageAccess = class(TsgCADImage);
  TsgVectorImageAccess = class(TsgVectorImage);

  TsgObjExportParams = class
    Fields: TsgExportParams;
  private
    class procedure InitDefValues(var AFields: TsgExportParams);
  public
    constructor Create;
  end;

{$IFDEF SG_FIREMONKEY}
const
  DefExportParamsObj: TsgObjExportParams = nil;
{$ENDIF}

function DerefStream(S: TStream): TCustomMemoryStream;
begin
  if S is TCustomMemoryStream then
    Result := TCustomMemoryStream(S)
  else
    Result := TMemoryStream.Create;
end;

function GetExportParamsNode(const AParser: TsgParser): TsgNode;
begin
  Result := TsgNode(AParser.ROOT.NodeByName[cnstXMLExportParams]);
  if not Assigned(Result) then
  begin
    Result := TsgNode(AParser.ROOT.NodeByName[cnstXMLRootNode]);
    if Assigned(Result) then
      Result := TsgNode(Result.GetChildByName(cnstXMLExportParams));
  end;
end;

function GetXMLNodeTextAsPixelFormat(const ANode: TsgNodeSample;
{$IFDEF SG_FIREMONKEY}
  const ADefaultValue: sgFMXTypes.TPixelFormat = pf24bit): sgFMXTypes.TPixelFormat;
{$ELSE}
  const ADefaultValue: TPixelFormat = pf24bit): TPixelFormat;
{$ENDIF}
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
  begin
    Result := GetPixelFormat(ANode.TextAsInt);
    if Result = pfCustom then
      Result := ADefaultValue;
  end;
end;

{$IFDEF SG_FIREMONKEY}
function DefExportParams: TsgExportParams;
begin
  Result := DefExportParamsObj.Fields;
end;
{$ENDIF}

function GetXMLNodeTextAsSizeMode(const ANode: TsgNodeSample;
  const ADefaultValue: TSizeMode = smCustom): TSizeMode;
var
  I: TSizeMode;
  vTextValue: string;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
  begin
    vTextValue := AnsiLowerCase(ANode.TextAsStr);
    for I := smActual to smSame do
    begin
      if vTextValue = AnsiLowerCase(cnstSizeModeName[I]) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function GetXMLNodeTextAsVersion(const ANode: TsgNodeSample;
  const ADefaultValue: TsgDWGVersion = acR2004): TsgDWGVersion;
begin
  if Assigned(ANode) then
    Result := sgFunction.sgStringToDWGVersion(ANode.TextAsStr, ADefaultValue)
  else
    Result := ADefaultValue;
end;

function GetXMLNodeTextAsTiffComprassion(const ANode: TsgNodeSample;
  const ADefaultValue: TsgTIFFCompressionType = ctAuto): TsgTIFFCompressionType;
var
  I: TsgTIFFCompressionType;
  vTextValue: string;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
  begin
    vTextValue := AnsiLowerCase(ANode.TextAsStr);
    for I := ctLZW to ctNone do
    begin
      if vTextValue = AnsiLowerCase(cnstTiffCompessionName[I]) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

{TsgSimpleExportImage}

constructor TsgSimpleExportImage.Create(ACADImage: TsgCADImage);
begin
  FCADImage := TsgCADImageClass(ACADImage.ClassType).Create;
  FCADImage.Assign(ACADImage);
  inherited Create(FCADImage);
end;

destructor TsgSimpleExportImage.Destroy;
begin
  FCADImage.OnProgress := nil;
  FCADImage.OnChange := nil;
  FreeAndNil(FCADImage);
  inherited Destroy;
end;

function TsgSimpleExportImage.GetBackgroundColor: TColor;
begin
  Result := FCADImage.BackgroundColor;
end;

function TsgSimpleExportImage.GetDefaultColor: Integer;
begin
  Result := FCADImage.DefaultColor;
end;

function TsgSimpleExportImage.GetDrawMode: TsgDXFDrawMode;
begin
  Result := FCADImage.DrawMode;
end;

function TsgSimpleExportImage.GetIsShowLineWeight: Boolean;
begin
  Result := TsgCADImageAccess(FCADImage).FShowLineWeightExport;
end;

function TsgSimpleExportImage.GetLineWeightScale: TsgFloat;
begin
  Result := FCADImage.LineWeightScale;
end;

function TsgSimpleExportImage.GetNullWidth: Double;
begin
  Result := FCADImage.NullWidthExt;
end;

{$IFDEF SG_BTI}
function TsgSimpleExportImage.GetUseFillStyleIndex: Boolean;
begin
  Result := TsgCADImageAccess(FCADImage).UseFillStyleIndex;
end;
{$ENDIF}

procedure TsgSimpleExportImage.SetBackgroundColor(Value: TColor);
begin
  FCADImage.BackgroundColor := Value;
end;

procedure TsgSimpleExportImage.SetDefaultColor(const Value: Integer);
begin
  FCADImage.DefaultColor := Value;
end;

procedure TsgSimpleExportImage.SetDrawMode(const Value: TsgDXFDrawMode);
begin
  FCADImage.DrawMode := Value;
end;

procedure TsgSimpleExportImage.SetIsShowLineWeight(Value: Boolean);
begin
  TsgCADImageAccess(FCADImage).FShowLineWeightExport := Value;
end;

procedure TsgSimpleExportImage.SetLineWeightScale(Value: TsgFloat);
begin
  FCADImage.LineWeightScale := Value;
end;

procedure TsgSimpleExportImage.SetNullWidth(Value: Double);
begin
  FCADImage.NullWidthExt := Value;//??????
end;

{$IFDEF SG_BTI}
procedure TsgSimpleExportImage.SetUseFillStyleIndex(AValue: Boolean);
begin
  TsgCADImageAccess(FCADImage).UseFillStyleIndex := AValue;
end;
{$ENDIF}

procedure TsgSimpleExportImage.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  BackgroundColor := AExportParams.BackgroundColor;
  DefaultColor := AExportParams.DefaultColor;
  NullWidth := AExportParams.NullWidth;
  LineWeightScale := AExportParams.LineWeightScale;
  IsShowLineWeight := AExportParams.SaveLineWeight;
  DrawMode := TsgDXFDrawMode(AExportParams.DrawMode);
{$IFDEF SG_BTI}
  UseFillStyleIndex := AExportParams.UseFillStyleIndex;
{$ENDIF}
end;

function TsgSimpleExportImage.AbsHeight: Extended;
begin
  Result := FCADImage.AbsHeight;
end;

function TsgSimpleExportImage.AbsWidth: Extended;
begin
  Result := FCADImage.AbsWidth;
end;

procedure TsgSimpleExportImage.GetExtents;
begin
  FCADImage.GetExtents;
end;

function TsgSimpleExportImage.GetFPoint(const P: TFPoint): TFPoint;
begin
  Result := FPointXMat(P,FCADImage.Converter.Params.Matrix);
end;

function TsgSimpleExportImage.GetPoint(const P: TFPoint): TPoint;
begin
  Result := FCADImage.GetPoint(P);// FPointXMat2DLongint(P, FDraw.Matrix);
end;

// 최승선 수정   DoExportBlockAW
{
procedure TsgSimpleExportImage.DoExportBlockAW(const blockString: string);
begin
  if Self is TsgCADtoSVG then
    TsgCADtoSVG(Self).ExportBlock(blockString);
end;
}

{TsgCADExport}

{protected}

function TsgCADExport.GetConverter: TsgDXFConverter;
begin
  Result := nil;
end;

function TsgCADExport.GetExportExceptionClass: ExceptClass;
begin
  Result := Exception;
end;

function TsgCADExport.GetSourceId: UInt64;
begin
  Result := UInt64(GetConverter);
end;

procedure TsgCADExport.InitializeVersionData(const AIsTrial: Boolean;
  const AMessage: string; AppId: string);
var
  vMText: TsgDXFMText;
begin
  FSGBlock.Free;
  FSGBlock := nil;
  if not AIsTrial then Exit;
  if AppId = '' then
    AppId := cnstCompany;
  // Works only for Trial version
  FSGBlock := TsgDXFBlock.Create;
  FSGBlock.Name := AppId;
  vMText := TsgDXFMText.Create;
  vMText.LineSpacingFactor := 1;
  vMText.Height := 1 / 20;
  vMText.Color := clRed;
  vMText.Text := AMessage;
  FSGBlock.AddEntity(vMText);
end;

function TsgCADExport.IsFormatMultiPages: Boolean;
begin
  Result := True;
end;

procedure TsgCADExport.LoadFromStream(S: TStream);
begin
end;

procedure TsgCADExport.LoadSGBlock;
var
  I: Integer;
  vIsCrossoverMatrix: Boolean;
begin
  if Assigned(FSGBlock) and Assigned(Converter) then
  begin
    vIsCrossoverMatrix := Converter.IsCrossoverMatrix;
    try
      Converter.IsCrossoverMatrix := True;
      for I := 0 to FSGBlock.Count - 1 do
        TEntityAccess(FSGBlock[I]).Loaded(Converter);
      TEntityAccess(FSGBlock).Loaded(Converter);
    finally
      Converter.IsCrossoverMatrix := vIsCrossoverMatrix;
    end;
  end;
end;

procedure TsgCADExport.Progress(Stage: TProgressStage; PercentDone: Byte; const Msg: string);
var
  R: TRect;
begin
  if Assigned(FOnProgress) then FOnProgress(Self,Stage,PercentDone,False,R, Msg{$IFDEF SGFPC}, FContinue{$ENDIF});
end;

{public}

procedure TsgCADExport.AfterExport(const S: TStream);
var
  vParam: TsgLogParam;
begin
  FillChar(vParam, SizeOf(vParam), 0);
  vParam.Operation := loSave;
  vParam.Obj := S;
  vParam.Id := GetSourceId;
  FileLoging(vParam);
end;

procedure TsgCADExport.ApplyParams(const AExportParams: TsgExportParams);
begin
end;

procedure TsgCADExport.BeforeExport(const S: TStream);
begin
{$IFDEF SGFPC}
  FContinue := True;
{$ENDIF}
end;

constructor TsgCADExport.Create(ACADImage: TsgCADImage);
begin
  inherited Create;
{$IFDEF SGFPC}
  FContinue := True;
{$ENDIF}
  FAppName := 'CAD VCL';
{$IFDEF DEMO}
  {$IFNDEF SG_FIREMONKEY}
  MessageBox(0, 'This function is avialable in CAD VCL Export and CAD VCL Enterprise only',
    'CAD VCL demo', {$IFDEF MSWINDOWS}MB_TASKMODAL or {$ENDIF}MB_ICONINFORMATION);
  {$ELSE}
  ShowMessage('This function is avialable in CAD VCL Export and CAD VCL Enterprise only');
  {$ENDIF}
{$ENDIF}
{$IFDEF SG_EXPORTDEMO}
  InitializeVersionData(True, 'Trial version CAD VCL - www.cadsofttools.com');
{$ENDIF}
end;

destructor TsgCADExport.Destroy;
begin
  FOnProgress := nil;
  FSGBlock.Free;
  inherited Destroy;
end;

class function TsgCADExport.ExportType: TsgExportFormat;
begin
  Result := efAuto;
end;

procedure TsgCADExport.AddSGInserts(const ALayout: TsgDXFLayout; const AInserts: TsgObjectList);
var
  I: Integer;
begin
  if Assigned(AInserts) then
    for I := 0 to AInserts.Count - 1 do
      ALayout.PaperSpaceBlock.AddEntity(TsgDXFEntity(AInserts[I]));
end;

procedure TsgCADExport.DoError(Error: Boolean; const ErrorMessage: string);
begin
  if Error then
    raise GetExportExceptionClass.Create(ErrorMessage);
end;

procedure TsgCADExport.DoError(Error: Boolean; const ErrorMessage: string;
  const Args: array of const);
begin
  if Error then
    raise GetExportExceptionClass.CreateFmt(ErrorMessage, Args);
end;

procedure TsgCADExport.DoInitializeView(var AView: TFMatrix);
begin
  AView := cnstIdentityMat;
end;

function TsgCADExport.ExpSGInsert(const ALayout: TsgDXFLayout): TsgObjectList;
const
  cnstBorderScale = 3;
var
  vBox: TFRect;
  vDelta, vScale: TFPoint;
  vInsert: TsgDXFInsertProtect;
  vWidth, vHeight: Double;
begin
  Result := nil;
  if Assigned(FSGBlock) and Assigned(ALayout) and (not IsBadRect(FSGBlock.Box)) then
  begin
    Result := TsgObjectList.Create;
    vBox := ALayout.Box;
    vDelta.X := GetSizeFRect2D(vBox).X;
    vDelta.Y := GetSizeFRect2D(vBox).Y;

    vWidth := GetSizeFRect2D(FSGBlock.Box).X * cnstBorderScale;
    vHeight := GetSizeFRect2D(FSGBlock.Box).Y * cnstBorderScale;

    if vWidth = 0 then
      vWidth := 1;
    if vHeight = 0 then
      vHeight := 1;
    vScale := MakeFPoint(vDelta.X / vWidth,  vDelta.Y / vHeight, 1);
    if vScale.X < vScale.Y then
      vScale.Y := vScale.X
    else
      vScale.X := vScale.Y;
    vInsert := TsgDXFInsertProtect.Create;
    vInsert.Parent := ALayout;
    vInsert.Block := FSGBlock;
    vInsert.Scale := vScale;
    vInsert.Loaded(Converter);
    vDelta.X := GetSizeFRect2D(vInsert.Box).X;
    vDelta.Y := GetSizeFRect2D(vInsert.Box).Y;
    vInsert.Point := MakeFPoint(vBox.Right - vDelta.X, vBox.Bottom + 2 * vDelta.Y,
      (vBox.Z2 + vBox.Z1) / 2);
    vInsert.Loaded(Converter);
    Result.Add(vInsert);
  end;
end;

procedure TsgCADExport.SaveToFile(const AFileName: string);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TsgCADExport.SaveToStream(S: TStream);
begin
  BeforeExport(S);
  try
    FFileName := '';
{$IFDEF SGDEL_2006}
    if (not FIgnoreFileName) and (S is TFileStream) then
      FFileName := TFileStream(S).FileName;
{$ENDIF}
    SaveToStreamCustom(S);
  finally
    AfterExport(S);
  end;
end;

procedure TsgCADExport.SetAppName(const AValue: string);
begin
  FAppName := AValue;
end;

{TsgSimpleExport}

{private}

procedure TsgSimpleExport.CntLayout(Index: Integer; ALayout: TsgDXFLayout);
begin
  ALayout.Iterate(FCADImage.Converter,IncCount, DoFinish);
end;

procedure TsgSimpleExport.CntSGBlock;
var
  PrevCount: Integer;
  vParams: PsgCADIterate;
begin
  if FSGBlock <> nil then
  begin
    vParams := FCADImage.Converter.Params;
    try
      FCADImage.Converter.Params := InitializeIterateParams;
      PrevCount := FCount;
      FSGBlock.Iterate(FCADImage.Converter, IncCount, DoFinish);
      Inc(FCount, Sqr(cnstTrialMessagesCount) * (FCount - PrevCount));
    finally
      FCADImage.Converter.Params := vParams;
    end;
  end;
end;

procedure TsgSimpleExport.CreateStampBlock;
const
  cnstMeasuresName = 'Measures';
  cnstScaleNameName = 'ScaleName';
  cnstStampItemsName = 'StampItems';
  cnstCADsInUnitName = 'CADsInUnit';
  cnstTextSizeName = 'TextSize';
  cnstCustomTextName = 'CustomText';
  cnstStampTableItemNameName = 'Name';
  cnstStampTableItemValueName = 'Value';
  cnstStampTableItemIsBoldFlagName = 'IsFontBold';
  cnstStampTableItemIsOnlyValueFlagName = 'IsOnlyValue';
  cnstRulerItems = 4;
  cnstRulerHeightRatio = 0.1;
  cnstRulerVisibilityName = 'RulerVisibility';
  cnstStampTextHeight = 0.7;
var
  vParser: TsgParser;
  vRoot,vTempNode: TsgNode;
  vScaleName: string;
  vCADsInRulerUnit,vTextSize: Double;
  vRuler,vTable,vInnerStampInsert: TsgDXFInsert;
  vInnerStampBlock: TsgDXFBlock;
  vHatch: TsgCADCurvePolygon;
  vSizeRuler,vSizeTable: TF2DPoint;
  vInnerBox: TFRect;

  procedure AddAndLoad(ABlock: TsgDXFBlock; AEntity: TsgDXFEntity);
  begin
    if not Assigned(AEntity) then
      Exit;
    ABlock.AddEntity(AEntity);
    TEntityAccess(AEntity).Loaded(Converter);
  end;

  procedure LoadInsert(AInsert: TsgDXFInsert);
  begin
    Converter.Loads(AInsert.Block);
    Converter.Loads(AInsert);
  end;

  function CreateHatch(const APoints: array of TFPoint): TsgCADCurvePolygon;
  var
    vPoly: TsgDXFPolyline;
    vList: TsgObjectList;
  begin
    vPoly := TsgDXFPolyline.Create;
    try
      AddVertexesInPolyline(vPoly, APoints);
      Result := TsgCADCurvePolygon.Create;
      vList := TsgObjectList.Create;
      try
        vList.Add(vPoly);
        TsgCADCurvePolygonAccess(Result).GenerateListOfBoundaries(vList);
      finally
        vList.Free;
      end;
    finally
      vPoly.Free;
    end;
  end;

  procedure CreateStyle(const AIsBold: Boolean; var AStyle: TsgDXFStyle);
  const
    cnstDefFontName = 'Arial';
    //cnstDefPrimaryFont = 'arial.ttf';
  var
    vStyle: TmvFontStyles;
  begin
    if Assigned(AStyle) then
      Exit;
    AStyle := TsgDXFStyle.Create;
    AStyle.Name := 'STAMPTEXTSTYLE_' + IntToStr(Integer(AStyle));
    CADImage.Converter.Sections[csStyles].AddEntity(AStyle);
    //AStyle.PrimaryFont := cnstDefPrimaryFont;
    AStyle.FontName := cnstDefFontName;
    vStyle := AStyle.FontStyle;
    if AIsBold then
      Include(vStyle, fmBold)
    else
      Exclude(vStyle, fmBold);
    AStyle.FontStyle := vStyle;
    CADImage.Converter.Loads(AStyle);
  end;

  function CreateText(AText: string; APoint: TFPoint; AHeight: Double;
    AIsBold: Boolean; const Point1: PFPoint = nil;
    const HAlign: Byte = 0): TsgDXFText;
  begin
    Result := TsgDXFText.Create;
    if AIsBold then
      Result.Style := FStampBoldStyle
    else
      Result.Style := FStampDefStyle;
    Result.Point := APoint;
    Result.Text := Trim(AText);
    Result.Height := AHeight;
    if Assigned(Point1) then
    begin
      Result.Point1 := Point1^;
      Result.HAlign := HAlign;
    end;
  end;

  function CreateBlock: TsgDXFBlock;
  begin
    Result := TsgDXFBlock.Create;
    Result.Name := 'STAMP' + IntToHex(Integer(Result), 0);
    //Converter.Sections[csBlocks].AddEntity(Result);
  end;

  procedure CreateBlockAndInsert(var AInsert: TsgDXFInsert; var ABlock: TsgDXFBlock);
  begin
    AInsert := TsgDXFInsert.Create;
    ABlock := CreateBlock;
    AInsert.Point := cnstFPointZero;
    AInsert.Block := ABlock;
  end;

  function CreateStampRuler(ANode: TsgNode; ACADsInUnit: Double;
    AScaleName: string; ATextHeight: Double): TsgDXFInsert;
  const
    cnstColorCADGray: TsgColorCAD = (Active: acRGBColor; Color: clGray;);
    cnstRulerColors: array[Boolean] of PsgColorCAD = (@cnstColorCADBlack, @cnstColorCADGray);
  var
    vBlock: TsgDXFBlock;
    vList: TsgObjectList;
    I: Integer;
    vDelta: TFPoint;
    vP1,vP2,vP3,vP4,vP5: TFPoint;
    vHeight: Double;
    vPoly: TsgDXFLWPolyline;
    vCurve: TsgCADCurvePolygon;
    vVisibilityNode: TsgNodeSample;
    vText: TsgDXFText;
    vIsVisible: Boolean;
  begin
    vVisibilityNode := ANode.GetAttributeByName(cnstRulerVisibilityName);
    if Assigned(vVisibilityNode) then
      vIsVisible := vVisibilityNode.ValueAsBool
    else
      vIsVisible := False;
    if not vIsVisible then
    begin
      Result := nil;
      Exit;
    end;
    vList := TsgObjectList.Create;
    try
      CreateBlockAndInsert(Result, vBlock);
      vDelta := MakeFPoint(vCADsInRulerUnit, 0, 0);
      vHeight := ACADsInUnit * cnstRulerHeightRatio;
      vP1 := MakeFPoint(ACADsInUnit, 2*vHeight);
      vP2 := MakeFPoint(ACADsInUnit, 0);
      vP3 := cnstFPointZero;
      vP4 := MakeFPoint(0, vHeight);
      vP5 := MakeFPoint(ACADsInUnit, vHeight);
      vText := CreateText('0', MakeFPoint(0, 2*vHeight), ATextHeight, False);
      AddAndLoad(vBlock, vText);
      for I := 1 to cnstRulerItems do begin
        vPoly := TsgDXFLWPolyline.Create;
        AddVertexesInPolyline(vPoly, [vP1,vP2,vP3,vP4,vP5]);
        AddAndLoad(vBlock, vPoly);
        vCurve := TsgCADCurvePolygon.Create;
        vCurve.ColorCAD := cnstRulerColors[Boolean(I and 1)]^;
        vList.Add(vPoly);
        TsgCADCurvePolygonAccess(vCurve).GenerateListOfBoundaries(vList);
        vList.Clear;
        AddAndLoad(vBlock, vCurve);
        TEntityAccess(vCurve).Loaded(Converter);
        vText := CreateText(IntToStr(I) + AScaleName, vP1, ATextHeight, False);
        AddAndLoad(vBlock, vText);
        vP1 := AddFPoint(vP1, vDelta);
        vP2 := AddFPoint(vP2, vDelta);
        vP3 := AddFPoint(vP3, vDelta);
        vP4 := AddFPoint(vP4, vDelta);
        vP5 := AddFPoint(vP5, vDelta);
      end;
      LoadInsert(Result);
    finally
      vList.Free;
    end;
  end;

  procedure GetNodeParams(const ANode: TsgNode; var AName,AValue: string;
    var AIsBold,AIsOnlyValue: Boolean);

    procedure GetBoolParam(const AAttrName: string; var AValue: Boolean);
    var
      vAttr: TsgNodeSample;
    begin
      vAttr := ANode.GetAttributeByName(AAttrName);
      if Assigned(vAttr) then
        AValue := vAttr.ValueAsBool;
    end;

    procedure GetStrParam(const AAttrName: string; var AValue: string);
    var
      vAttr: TsgNodeSample;
    begin
      vAttr := ANode.GetAttributeByName(AAttrName);
      if Assigned(vAttr) then
        AValue := vAttr.ValueAsStr;
    end;

  begin
    GetStrParam(cnstStampTableItemNameName, AName);
    GetStrParam(cnstStampTableItemValueName, AValue);
    GetBoolParam(cnstStampTableItemIsBoldFlagName, AIsBold);
    GetBoolParam(cnstStampTableItemIsOnlyValueFlagName, AIsOnlyValue);
  end;

  function GetNormalTextSize: Double;
  var
    vBox: TFRect;
  begin
    if Assigned(CADImage.ClippingRect) then
      vBox := CADImage.ClippingRect^
    else
      vBox := CADImage.Extents;
    Result := 0.002 * DistanceFPoint(vBox.TopLeft, vBox.BottomRight);
  end;

  procedure CallBoxes(const AText: string; const ATextHeight: Double;
    const AIsBold: Boolean; ARect1,ARect2: PFRect);
  var
    vText: TsgDXFText;
  begin
    vText := CreateText(AText, cnstFPointZero, ATextHeight, AIsBold);
    try
      Converter.Loads(vText);
      if Assigned(ARect1) then
        UnionFRect(ARect1^, vText.Box);
      if Assigned(ARect2) then
        UnionFRect(ARect2^, vText.Box);
    finally
      vText.Free;
    end;
  end;

  function CreateStampTable(const ANode: TsgNode;
    const ATextHeight,AColumnWidth: Double): TsgDXFInsert;
  var
    vBlock: TsgDXFBlock;
    I: Integer;
    vP1,vP2,vP3: TFPoint;
    vStampTableInterlacedHeight: Double;
    vRowNode: TsgNode;
    vText1,vText2: TsgDXFText;
    vBox,vBox1,vBox2: TFRect;
    vColumnWidth,vTmpLen,vTmpLen2,vColWidth2: Double;
    vName,vValue: string;
    vIsBold,vIsOnlyValue: Boolean;
  begin
    Result := nil;
    if not Assigned(ANode) then
      Exit;
    CreateBlockAndInsert(Result, vBlock);
    vStampTableInterlacedHeight := ATextHeight * 0.3;
    vBox := cnstBadRect;
    vBox1 := cnstBadRect;
    vBox2 := cnstBadRect;
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vRowNode := TsgNode(ANode.ChildNodes[I]);
      GetNodeParams(vRowNode, vName, vValue, vIsBold, vIsOnlyValue);
      if vIsOnlyValue then
        CallBoxes(vValue, ATextHeight, vIsBold, @vBox, nil)
      else
      begin
        CallBoxes(vName, ATextHeight, vIsBold, @vBox, @vBox1);
        CallBoxes(vValue, ATextHeight, vIsBold, @vBox, @vBox2);
      end;
    end;
    vTmpLen2 := GetSizeFRect2D(vBox).X;
    if IsBadRect(vBox1) or IsBadRect(vBox2) then
    begin
      vColumnWidth := 0;
      vColWidth2 := vTmpLen2;
    end
    else
    begin
      vColumnWidth := Max(GetSizeFRect2D(vBox1).X, AColumnWidth);
      vColWidth2 := GetSizeFRect2D(vBox2).X + vColumnWidth * 0.1;
    end;
    vTmpLen := vTmpLen2 - (vColumnWidth + vColWidth2);
    if vTmpLen > 0 then
      vColumnWidth := vColumnWidth + vTmpLen;
    vP1 := cnstFPointZero;
    vTmpLen := 0;
    for I := ANode.ChildNodesCount - 1 downto 0 do
    begin
      vP2 := MakeFPoint(vP1.X + vColumnWidth, vP1.Y);
      vP3 := MakeFPoint(vP1.X + vColumnWidth + vColWidth2, vP1.Y);
      vRowNode := TsgNode(ANode.ChildNodes[I]);
      GetNodeParams(vRowNode, vName, vValue, vIsBold, vIsOnlyValue);
      if vIsOnlyValue then
      begin
        vText1 := CreateText(vValue, cnstFPointZero, ATextHeight, vIsBold, @vP3, 2);
        AddAndLoad(vBlock, vText1);
        vBox := vText1.Box;
      end
      else
      begin
        vText1 := CreateText(vName, vP1, ATextHeight, vIsBold, @vP2, 2);
        AddAndLoad(vBlock, vText1);
        vText2 := CreateText(vValue, vP2, ATextHeight, vIsBold, @vP3, 2);
        AddAndLoad(vBlock, vText2);
        vBox := vText1.Box;
        UnionFRect(vBox, vText2.Box);
      end;
      vTmpLen := Max(vTmpLen, vBox.Right);
      vP1.Y := vBox.Top + vStampTableInterlacedHeight;
    end;
    LoadInsert(Result);
  end;

  procedure LoadAddingEntities(const ANode: TsgNode);
  var
    vResults: TsgXMLOut;
  begin
    if Assigned(ANode) then
    begin
      vResults := TsgXMLOut.Create;
      try
        CADImage.Converter.FromXML(ANode, vResults);
        ReadResults(vResults);
      finally
        vResults.Free;
      end;
    end
    else
      Exit;
  end;

begin
  if Assigned(FStampBlock) then
    Exit;
  if FXMLParamsExtended <> '' then
  begin
    vParser := TsgParser.Create;
    try
      if vParser.LoadFromString(FXMLParamsExtended) then
      begin
        vRoot := TsgNode(vParser.ROOT.NodeByName[cnstXMLPlottingScale]);
        if not Assigned(vRoot) then
          Exit;
        if vRoot.ChildNodesCount = 0 then
          Exit;
        FStampBlock := CreateBlock;
        CreateStyle(False, FStampDefStyle);
        CreateStyle(True, FStampBoldStyle);
        vScaleName := GetAttributeStr(vRoot, cnstScaleNameName, '');
        vCADsInRulerUnit := GetAttributeFloat(vRoot, cnstCADsInUnitName, 1);
        vTextSize := GetAttributeFloat(vRoot, cnstTextSizeName, 1);
        vRuler := CreateStampRuler(vRoot, vCADsInRulerUnit, vScaleName,
          vCADsInRulerUnit * cnstRulerHeightRatio);
        if Assigned(vRuler) then
          vSizeRuler := GetSizeFRect2D(vRuler.Box)
        else
          vSizeRuler := cnstF2DPointZero;
        vTempNode := TsgNode(vRoot.GetChildByName(cnstStampItemsName));
        if not Assigned(vTempNode) then
          vTempNode := vRoot;
        vTable := CreateStampTable(vTempNode, GetNormalTextSize * vTextSize, 0.3 * vSizeRuler.X);
        if Assigned(vTable) then
          vSizeTable := GetSizeFRect2D(vTable.Box)
        else
          vSizeTable := cnstF2DPointZero;
        if vSizeRuler.X >= vSizeTable.X then
        begin
          if Assigned(vRuler) then
            vRuler.Point := cnstFPointZero;
          if Assigned(vTable) then
            vTable.Point := MakeFPoint(vSizeRuler.X - vSizeTable.X, 1.2*vSizeRuler.Y);
        end
        else
        begin
          if Assigned(vRuler) then
            vRuler.Point := MakeFPoint(vSizeTable.X - vSizeRuler.X, 0);
          if Assigned(vTable) then
            vTable.Point := MakeFPoint(0, 1.2*vSizeRuler.Y);
        end;
        CreateBlockAndInsert(vInnerStampInsert, vInnerStampBlock);
        AddAndLoad(vInnerStampBlock, vTable);
        AddAndLoad(vInnerStampBlock, vRuler);
        vInnerBox := cnstBadRect;
        if Assigned(vTable) then
          UnionFRect(vInnerBox, vTable.Box);
        if Assigned(vRuler) then
          UnionFRect(vInnerBox, vRuler.Box);
        if IsBadRect(vInnerBox) then
          vHatch := nil
        else
        begin
          //vCADsInRulerUnit := 0.1*DistanceFPoint(vInnerBox.TopLeft, vInnerBox.BottomRight);
          vCADsInRulerUnit := 0.0002*DistanceFPoint(CADImage.CurrentLayout.Box.TopLeft,
            CADImage.CurrentLayout.Box.BottomRight);
          vInnerStampInsert.Point := AddFPoint(vInnerStampInsert.Point,
            MakeFPoint(vCADsInRulerUnit, vCADsInRulerUnit));
          vCADsInRulerUnit := 2*vCADsInRulerUnit;
          vInnerBox.Right := vInnerBox.Right + vCADsInRulerUnit;
          vInnerBox.Top := vInnerBox.Top + vCADsInRulerUnit;
          vHatch := CreateHatch([
            MakeFPoint(vInnerBox.Left, vInnerBox.Top),
            MakeFPoint(vInnerBox.Right, vInnerBox.Top),
            MakeFPoint(vInnerBox.Right, vInnerBox.Bottom),
            MakeFPoint(vInnerBox.Left, vInnerBox.Bottom),
            MakeFPoint(vInnerBox.Left, vInnerBox.Top)]);
          vHatch.ColorCAD := MakeColorCAD(acRGBColor, clWhite);
        end;
        AddAndLoad(FStampBlock, vHatch);
        AddAndLoad(FStampBlock, vInnerStampInsert);
        Converter.Loads(FStampBlock);
        LoadAddingEntities(TsgNode(vRoot.GetChildByName(cnstMeasuresName)));
      end;
    finally
      vParser.Free;
    end;
  end;
end;

function TsgSimpleExport.DrawInsert(Sender: TObject): Integer;
begin
  Result := Integer(TsgDXFInsert(Sender).Block <> nil);
end;

procedure TsgSimpleExport.EnterInsert(Sender: TObject);
begin//FI:W519 ignore
end;

procedure TsgSimpleExport.EnterViewport(Sender: TObject);
begin//FI:W519 ignore
end;

procedure TsgSimpleExport.EnterXRef(Sender: TObject);
begin//FI:W519 ignore
end;

procedure TsgSimpleExport.ExpLayout(Index: Integer; ALayout: TsgDXFLayout);
var
  vBitmap: TBitmap;
  vDrawRect: TRect;
  vDrawRectWidth, vDrawRectHeight: Integer;
  vXScale: Double;
begin
  FColor := clNone;
  FFillColor := clNone;
  vXScale := FXScale;
  CurrentLayout := ALayout;
  PageStart(Index);
  try
    if GetBitmapMode <> bemNone then
    begin
      FCADImage.IsIterator := False;
      vBitmap := TBitmap.Create;
      try
        case GetBitmapMode of
         bem1bit: vBitmap.PixelFormat := pf1bit;
         bem8bit: vBitmap.PixelFormat := pf8bit;
         else
           vBitmap.PixelFormat := pf24Bit;
        end;
        vDrawRect := GetBitmapRect;
        vDrawRectWidth := vDrawRect.Right - vDrawRect.Left;
        vDrawRectHeight := vDrawRect.Bottom - vDrawRect.Top;
        if vDrawRectWidth * vDrawRectHeight > cnstMultWidthHeightMax then
        begin
         if vDrawRectWidth > vDrawRectHeight then
         begin
           vDrawRectHeight := Round((vDrawRectHeight * 5000)/vDrawRectWidth);
           vDrawRectWidth := 5000;
         end
         else
         begin
           vDrawRectWidth := Round((vDrawRectWidth * 5000)/vDrawRectHeight);
           vDrawRectHeight := 5000;
         end;
        end;
        vBitmap.Width := vDrawRectWidth;
        vBitmap.Height := vDrawRectHeight;
        SetBitmapTransparent(vBitmap, True, FCADImage.BackgroundColor);
        vDrawRect := Rect(0, 0, vDrawRectWidth, vDrawRectHeight);
        vBitmap.Canvas.StretchDraw(vDrawRect, FCADImage);
        ExpImage(GetExportRect, vBitmap);
      finally
        vBitmap.Free;
      end;
    end
    else
    begin
      FCADImage.IsIterator := True;
      try
        DoDrawCurrentLayout;
      finally
        FCADImage.IsIterator := False;
      end;
    end;
  finally
    PageEnd(Index);
    FXScale := vXScale;
  end;
end;

procedure TsgSimpleExport.ExpStampInsert(const ALayout: TsgDXFLayout;
  var AList: TsgObjectList);
var
  vBox,vExtents: TFRect;
  vSizeInsert,vSizeBox: TF2DPoint;
  vInsert: TsgDXFInsertProtect;
begin
  if Assigned(FStampBlock) and Assigned(ALayout) and (not IsBadRect(FStampBlock.Box)) then
  begin
    if HasClippingRect then
    begin
      vBox := ClippingRect;
      vExtents := CADImage.CurrentLayout.Box;
      vBox := MakeFRect(vExtents.Left + vBox.Left, vExtents.Top - vBox.Top, 0,
        vExtents.Left + vBox.Right, vExtents.Top - vBox.Bottom, 0);
    end
    else
      vBox := ALayout.Box;
    if not Assigned(AList) then
      AList := TsgObjectList.Create;
    vInsert := TsgDXFInsertProtect.Create;
    vInsert.Parent := ALayout;
    vInsert.Block := FStampBlock;
    vInsert.Point := cnstFPointZero;
    Converter.Loads(vInsert);
    vSizeInsert := GetSizeFRect2D(vInsert.Box);
    vSizeBox := GetSizeFRect2D(vBox);
    vInsert.Point := MakeFPoint(vBox.Right - vSizeInsert.X, vBox.Bottom);
    Converter.Loads(vInsert);
    AList.Add(vInsert);
  end;
end;

function TsgSimpleExport.GetBorderSize: TsgBorderSize;
begin
  Result := FCADImage.BorderSize;
end;

function TsgSimpleExport.GetBorderType: TsgBorderType;
begin
  Result := FCADImage.BorderType;
end;

function TsgSimpleExport.GetBitmapMode: TsgBitmapExportMode;
begin
  Result := bemNone;
end;

function TsgSimpleExport.GetBitmapRect: TRect;
begin
  Result := cnstRectZero;
end;

function TsgSimpleExport.GetClippingRect: TFRect;
var
  vExtents: TFRect;
  vCX, vCY, vW, vH: Double;
begin
  if HasClippingRect then
  begin
    Result := FCADImage.ClippingRect^;
    if Flipping then
    begin
      if Assigned(FCADImage.CurrentLayout) then
        vExtents := GetRealBox(FCADImage.CurrentLayout.Box, FCADImage.CurrentLayout.RotMatrix)
      else
        vExtents := FCADImage.Extents;

      vW := 0.5 * Abs(Result.Right - Result.Left);
      vH := 0.5 * Abs(Result.Top - Result.Bottom);
      vCX := 0.5 * (Result.Right + Result.Left);
      vCY := 0.5 * (Result.Top + Result.Bottom);
      vCY := (vExtents.Top - vExtents.Bottom) - vCY;

      Result.Left := vCX - vW;
      Result.Right := vCX + vW;
      Result.Top := vCY - vH;
      Result.Bottom := vCY + vH;
    end;
  end
  else
    Result := FCADImage.Extents;
end;

function TsgSimpleExport.GetDimensionsVisible: Boolean;
begin
  Result := FCADImage.DimensionsVisible;
end;

function TsgSimpleExport.GetExtent: TFRect;
begin
  Result := FCADImage.GetDrawingExtents;
end;

function TsgSimpleExport.GetIgnoreLayerNoPlotting: Boolean;
begin
  Result := FIgnoreLayerNoPlotting;
end;

function TsgSimpleExport.GetImageType: TsgExportFormat;
begin
  Result := efAuto;
  if Assigned(FCADImage) then
    Result := TsgCADImageAccess(FCADImage).GetImageType;
end;

function TsgSimpleExport.GetIsWithoutBorder: Boolean;
begin
  Result := FCADImage.IsWithoutBorder;
end;

function TsgSimpleExportCustom.GetLayout(Index: Integer): TsgDXFLayout;
begin
  Result := FCADImage.Converter.Layouts[Index];
end;

function TsgSimpleExportCustom.GetLayoutByName(const AName: string): TsgDXFLayout;
begin
  Result := FCADImage.Converter.LayoutByName(AName);
end;

function TsgSimpleExport.GetLayoutExportModeInt: TsgLayoutExportMode;
begin
  Result := FLayoutExportMode;
  if (not IsFormatMultiPages) and (Result in [lemAllLayouts, lemAllPaperSpaces]) then
    Result := lemCurrentLayout;
end;

function TsgSimpleExportCustom.GetLayoutsCount: Integer;
begin
  Result := FCADImage.Converter.LayoutsCount;
end;

function TsgSimpleExport.GetLayoutsExportCount: Integer;
var
  I, vIndex, vLayoutsCount: Integer;
  vSkipExportLayout: Boolean;
  vLayoutExportMode: TsgLayoutExportMode;
  vLayout: TObject;
begin
  Result := 0;
  FreeAndNil(FSelectedLayouts);
  if HasClippingRect then
    Inc(Result)
  else
  begin
    vLayoutsCount := LayoutsCount;
    vLayoutExportMode := GetLayoutExportModeInt;
    if vLayoutExportMode in [lemLayoutsByIndexes, lemLayoutByName] then
    begin
      if Assigned(FSelectedLayouts) then
        FSelectedLayouts.Clear
      else
        FSelectedLayouts := TsgStringList.Create;
      try
        TsgStringList(FSelectedLayouts).LineBreak := LayoutNameSeparator;
        FSelectedLayouts.Text := FLayoutNameExportMode;
        sgFunction.DeleteEmptyStrings(FSelectedLayouts);
        if FSelectedLayouts.Count > 0 then
        begin
          FSelectedLayouts.Sorted := True;
          FSelectedLayouts.CaseSensitive := False;
          if vLayoutExportMode = lemLayoutsByIndexes then
          begin
            for I := 0 to FSelectedLayouts.Count - 1 do
            begin
              vLayout := nil;
              vIndex := StrToIntDef(FSelectedLayouts[I], -1);
              if (vIndex > -1) and (vIndex < vLayoutsCount) then
                vLayout := Layouts[vIndex];
              FSelectedLayouts.Objects[I] := vLayout;
            end;
          end
          else
          begin
            for I := 0 to FSelectedLayouts.Count - 1 do
            begin
              vLayout := GetLayoutByName(FSelectedLayouts[I]);
              FSelectedLayouts.Objects[I] := vLayout;
            end;
          end;
          for I := FSelectedLayouts.Count - 1 downto 0 do
          begin
            if not Assigned(FSelectedLayouts.Objects[I]) then
              FSelectedLayouts.Delete(I);
          end;
        end;
        if FSelectedLayouts.Count < 1 then
          FreeAndNil(FSelectedLayouts);
      except
        FreeAndNil(FSelectedLayouts);
      end;
    end;
    if Assigned(FSelectedLayouts) then
      Result := FSelectedLayouts.Count
    else
    begin
      for I := 0 to LayoutsCount - 1 do
      begin
        case vLayoutExportMode of
          lemModel:          vSkipExportLayout := not Layouts[I].IsModel;
          lemAllLayouts:     vSkipExportLayout := Layouts[I].IsModel;
          lemAllPaperSpaces: vSkipExportLayout := False;
          lemCurrentLayout:  vSkipExportLayout := CurrentLayout <> Layouts[I];
        else
          vSkipExportLayout := True;
        end;
        if not vSkipExportLayout then
          Inc(Result);
      end;
    end;
    if (not IsFormatMultiPages) and (Result > 1) then
      Result := 1;
  end;
end;

function TsgSimpleExport.GetMiterLimit: Single;
begin
  Result := FMiterLimit;
end;

function TsgSimpleExport.GetPenStyle: DWORD;
begin
  Result := FPenStyle;
end;

function TsgSimpleExport.GetPenWidth: Double;
begin
  Result := FPenWidth;
end;

function TsgSimpleExport.GetMmToPixelX: Double;
begin
  Result := 1;
end;

function TsgSimpleExport.GetTextVisible: Boolean;
begin
  Result := FCADImage.TextVisible;
end;

function TsgSimpleExport.GetTransparency: Boolean;
begin
  Result := True;
end;

function TsgSimpleExport.GetTTFMode: TsgTTFMode;
begin
  Result := FTTFMode;
end;

function TsgSimpleExport.GetUseSHXFonts: Boolean;
begin
  if FUseSHXFonts < 0 then
  begin
    if Assigned(FCADImage) then
      Result := FCADImage.Converter.UseSHXFonts
    else
      Result := bUseSHXFonts;
  end
  else
    Result := FUseSHXFonts > 0;
end;

function TsgSimpleExport.HasClippingRect: Boolean;
begin
  Result := FCADImage.ClippingRect <> nil;
end;

function TsgSimpleExport.IncCount(Entity: TsgDXFEntity): Integer;
begin
  Inc(FCount);
  if Entity is TsgDXFInsert then
    Result := DrawInsert(Entity)
  else
    Result := 0;
end;

function TsgSimpleExport.IsGreatImage(const AImage: TPersistent): Boolean;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  {$IFDEF SG_DELPHI_VCL}
  Result := (AImage is TMetafile) and
    ((TMetafile(AImage).Height * TMetafile(AImage).Width) > cnstMultWidthHeightMax);
  {$ELSE}
  Result := False;
  {$ENDIF}
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TsgSimpleExport.IsSupportColorToLineWeight: Boolean;
begin
  Result := False;
end;

function TsgSimpleExport.IsSupportPathData: Boolean;
begin
  Result := False;
end;

constructor TsgSimpleExportCustom.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  SetImage(ACADImage);
end;

function TsgSimpleExportCustom.InitializeIterateParams: PsgCADIterate;
begin
  FParams^.BlockStack := nil;
  FillChar(FParams^, SizeOf(FParams^), 0);
  DoInitializeView(FParams^.DrawMatrix);
  FParams^.Matrix := FParams^.DrawMatrix;
  FParams^.BlockStack := nil;
  Result := FParams;
end;

procedure TsgSimpleExport.Iterate(Proc: TsgLayoutProc);
var
  I: Integer;
  vSkipExportLayout: Boolean;
  vLayouts, vInserts: TsgObjectList;
  vLayout: TsgDXFLayout;
  vParams: PsgCADIterate;
  vLayoutExportMode: TsgLayoutExportMode;
  vBlockStack: TObject;
  vCurrentLayout: TsgDXFLayout;
begin
  vLayouts := TsgObjectList.Create;
  try
    if HasClippingRect then
      vLayouts.Add(CurrentLayout)
    else
    begin
      if Assigned(FSelectedLayouts) then//[lemLayoutsByIndexes, lemLayoutByName] see in function TsgSimpleExport.GetLayoutsExportCount: Integer;
      begin
        for I := 0 to FSelectedLayouts.Count - 1 do
          vLayouts.Add(FSelectedLayouts.Objects[I]);
      end
      else
      begin
        vLayoutExportMode := GetLayoutExportModeInt;
        if vLayoutExportMode in [lemLayoutsByIndexes, lemLayoutByName] then
        begin
          try
            GetLayoutsExportCount;
            for I := 0 to FSelectedLayouts.Count - 1 do
              vLayouts.Add(FSelectedLayouts.Objects[I]);
          except
          end;
        end
        else
        begin
          for I := 0 to LayoutsCount - 1 do
          begin
            case vLayoutExportMode of
              lemModel: vSkipExportLayout := not Layouts[I].IsModel;
              lemAllLayouts: vSkipExportLayout := Layouts[I].IsModel;
              lemLayoutByName: vSkipExportLayout := CompareText(Layouts[I].Name, FLayoutNameExportMode) <> 0;
              lemAllPaperSpaces: vSkipExportLayout := False;
              lemCurrentLayout: vSkipExportLayout := CurrentLayout <> Layouts[I];
            else
              vSkipExportLayout := True;
            end;
            if not vSkipExportLayout then
              vLayouts.Add(Layouts[I]);
          end;
        end;
      end;
    end;
    if (not IsFormatMultiPages) and (vLayouts.Count > 1) then
      vLayouts.Count := 1;
    vCurrentLayout := CurrentLayout;
    try
      CreateStampBlock;
      for I := 0 to vLayouts.Count - 1 do
      begin
        vLayout := TsgDXFLayout(vLayouts[I]);
        CurrentLayout := vLayout;
        vInserts := ExpSGInsert(vLayout);
        vBlockStack := nil;
        try
          ExpStampInsert(vLayout, vInserts);
          AddSGInserts(vLayout, vInserts);

          vParams := FCADImage.Converter.Params;
          if FCADImage.Converter.Params <> nil then
          begin
            vBlockStack := FCADImage.Converter.Params^.BlockStack;
            FCADImage.Converter.Params^.BlockStack := nil;
          end
          else
            FCADImage.Converter.Params := InitializeIterateParams;
          try
            Proc(I, vLayout);
          finally
            FCADImage.Converter.Params := vParams;
            if FCADImage.Converter.Params <> nil  then
              TObject(FCADImage.Converter.Params^.BlockStack) := vBlockStack;
          end;
        finally
          TsgObjectList.FreeList(vInserts);
        end;
      end;
    finally
      CurrentLayout := vCurrentLayout;
    end;
  finally
    vLayouts.Free;
  end;
end;

class function TsgSimpleExport.LayoutNameSeparator: Char;
begin
  Result := sgConsts.cnstEnumValuesDelimiter;
end;

procedure TsgSimpleExport.ReadResults(const AResults: IsgResultNode);
begin
  ReadXMLResults(AResults.Output);
end;

procedure TsgSimpleExport.ReadXMLResults(ANode: TsgNode);
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vHandleAttr: TsgNodeSample;
begin
  for I := 0 to ANode.ChildNodesCount - 1 do
    ReadXMLResults(ANode.ChildNodes[I] as TsgNode);
  if SameText(ANode.Name, cnstXMLCreated) then
  begin
    vHandleAttr := ANode.GetAttributeByName(cnstXMLNames[xmlHandle].Name);
    if Assigned(vHandleAttr) then
    begin
      vEntity := CADImage.Converter.FindObjByHandle(vHandleAttr.ValueAsHandle);
      if Assigned(vEntity) and (FTempEntities.IndexOf(vEntity) < 0) then
        FTempEntities.Add(vEntity);
    end;
  end;
end;

procedure TsgSimpleExport.SetApplyPrintStylesToRGB(const Value: Boolean);
begin
  FColorToLineWeightMode := (FColorToLineWeightMode and Integer($FFFFFFFE)) or Ord(Value);
end;

procedure TsgSimpleExport.SetBorderType(Value: TsgBorderType);
begin
  FCADImage.BorderType := Value;
end;

procedure TsgSimpleExport.SetColor;
//var C: TColor;
begin//FI:W519 ignore
  //C := ColorToRGB(FCanvas.Pen.Color);
  //if C = FColor then Exit;
  //FColor := C;
  //SetStrokeColor(C);
end;

procedure TsgSimpleExport.SetDimensionsVisible(AVisible: Boolean);
begin
  FCADImage.DimensionsVisible := AVisible;
end;

procedure TsgSimpleExport.SetFColor;
begin
  {if FCanvas.Brush.Style = bsClear then Exit;
  C := ColorToRGB(FCanvas.Brush.Color);
  if C = FFillColor then Exit;
  FFillColor := C; }  //evg
  SetFillColor(FFillColor);//evg    there is no sence here!!!
end;

procedure TsgSimpleExport.SetIgnoreLayerNoPlotting(const Value: Boolean);
begin
  FIgnoreLayerNoPlotting := Value;
end;

procedure TsgSimpleExport.SetIsWithoutBorder(Value: Boolean);
begin
  FCADImage.IsWithoutBorder := Value;
end;

procedure TsgSimpleExport.SetLayoutExportMode(const Value: TsgLayoutExportMode);
begin
  FLayoutExportMode := Value;
end;

procedure TsgSimpleExport.SetNormalsInternal(const AX, AY: Integer);
begin
  TsgCADImageAccess(FCADImage).FNormalsInternal := Point(AX, AY);
end;

procedure TsgSimpleExport.SetParamsForImage(const AImage: TsgCADImage);
begin
  case GetImageType of
    efCgm:
      begin
        FLineWeightScaleByExport := 0.0254 / 3;// 1 / 72;
      end;
    efPlt:
      begin
        FLineWeightScaleByExport := 0.075 / 3;
      end;
  end;
end;

procedure TsgSimpleExport.SetTextVisible(AVisible: Boolean);
begin
  FCADImage.TextVisible := AVisible;
end;

procedure TsgSimpleExport.SetTransparency(const AValue: Boolean);
begin
end;

{protected}

procedure TsgSimpleExport.ExpCloseFigure;
begin
end;

procedure TsgSimpleExport.ExpEntityProc(AEnt: TObject; const AState: Integer);
begin
  if not Assigned(AEnt) then
    Exit;
  case AState of
    1://begin
      begin
        FEntitiesStack.Add(AEnt);
      end;
    2://end
      begin
        if (FEntitiesStack.Count > 0) and (FEntitiesStack.Last = AEnt) then
          FEntitiesStack.Count := FEntitiesStack.Count - 1;
      end;
  end;
end;

procedure TsgSimpleExport.DoDrawCurrentLayout;
begin
  FCADImage.DrawRectInt(GetExportRect);  //evg
end;

procedure TsgSimpleExport.DoExpPathData(const APathData: TObject;
  const Edge, Fill: Boolean);
begin
end;

procedure TsgSimpleExport.DoExpPolyPoints(const APointsLists: TsgObjectList; const Edge,
  Fill: Boolean);
var
  I, J: Integer;
  vPointsF: TFPointList;
  vPoint: TPoint;
  vCounts, vPoints: TsgIntegerList;
begin
  vPoints := TsgIntegerList.Create;
  vCounts := TsgIntegerList.Create;
  try
    vCounts.Capacity := APointsLists.Count;
    for I := 0 to APointsLists.Count - 1 do
    begin
      vPointsF := TFPointList(APointsLists[I]);
      if vPointsF.Count > 0 then
      begin
        vCounts.Add(vPointsF.Count);
        vPoints.Capacity := vPoints.Capacity + vPointsF.Count * 2;
        for J := 0 to vPointsF.Count -1 do
        begin
          vPoint := MakePointFrom3D(vPointsF[J]);
          vPoints.Add(vPoint.X);
          vPoints.Add(vPoint.Y);
        end;
      end;
    end;
    case vCounts.Count of
      0:  begin end;
      1:
      begin
        if Edge then
          ExpPolyline(PPoint(vPoints.List), vPoints.Count div 2);
        if Fill then
          ExpPolygon(PPoint(vPoints.List), vPoints.Count div 2);
      end;
    else
      if Edge then
        ExpPolyPolyline(vPoints.List^, PInteger(vCounts.List), vCounts.Count);
      if Fill then
        ExpPolyPolygon(vPoints.List^, PInteger(vCounts.List), vCounts.Count);
    end;
  finally
    FreeAndNil(vCounts);
    FreeAndNil(vPoints);
  end;
end;

function TsgSimpleExport.DoFinish(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
  case Entity.EntType of
    ceInsert, ceDimension, ceMText: EnterInsert(Entity);
    ceViewport:                     EnterViewport(Entity);
    ceXRef:                         EnterXRef(Entity);
  end;
end;

procedure TsgSimpleExport.ExpClipRgn(P: PRect; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpFillRgn(P: PRect; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpImage(const R: TRect; AImage: TPersistent);
begin
end;

//
// APoint1 - insert point
// APoint2, APoint3, APoint4 - the following points in a clockwise
//
procedure TsgSimpleExport.ExpImageUV(const APoint1, APoint2, APoint3, APoint4: TPoint;
  AImage: TPersistent);
var
  vR: TRect;
begin
  vR.TopLeft := APoint1;
  vR.BottomRight := APoint1;
  ExpandRect(vR, APoint2);
  ExpandRect(vR, APoint3);
  ExpandRect(vR, APoint4);
  ExpImage(vR, AImage);
end;

procedure TsgSimpleExport.ExpPixel(APoint: TPoint; AColor: TColor);
var
  vPoints: array[0..3] of Integer;
begin
  vPoints[0] := APoint.X;
  vPoints[1] := APoint.Y;
  vPoints[2] := vPoints[0];
  vPoints[3] := vPoints[1];
  ExpSetColor(AColor, ctPen);
  ExpPolyline(@vPoints[0], 2);
end;

procedure TsgSimpleExport.ExpPolygon(Points: PPoint; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpPolyline(Points: PPoint; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpPolyPolygon(const Points; Counts: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpPolyPolyline(const Points; Counts: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger; Count: Integer);
begin
end;

procedure TsgSimpleExport.ExpProgress;
begin
  Inc(FCurrent);
  if FCurrent - FCurrentStep >= FStep then
  begin
    FCurrentStep := FCurrent;
    Progress(psRunning, FCurrentStep * 100 div FCount, '');
  end;
end;

procedure TsgSimpleExport.ExpRestoreDC;
begin
  FColor := clNone;
end;

procedure TsgSimpleExport.ExpSaveDC;
begin
end;

procedure TsgSimpleExport.ExpAnnotation(AEnt: TObject; const AParams: TsgExpAnnotParam);
begin
end;

procedure TsgSimpleExport.ExpSetColor(AColor: TColor; AColorType: TsgColorType);
begin
 //evg
  case AColorType of
    ctPen:
    begin
      if AColor <> FColor then
      begin
        FColor := AColor;
        SetStrokeColor(AColor);
      end;
    end;
    ctBrush, ctFont:
    begin
      FFillColor := AColor;
      SetFillColor(AColor);
      if AColorType = ctFont then
        FFontColor := FFillColor;
    end;
  end;

   //evg
end;

{
procedure TsgSimpleExport.ExpSetColorBrush(AColor: TColor);                     {
begin
  FFillColor := AColor;
  SetFillColor(AColor);
end;}

procedure TsgSimpleExport.ExpSetFont(AFont: TFont);
begin
//do nothing
end;

procedure TsgSimpleExport.ExpSetPenGeometric(AStyle: DWORD);
begin
  FPenStyle := AStyle;
end;

procedure TsgSimpleExport.ExpSetPenMiterLimit(ANewLimit: Single);
begin
  FMiterLimit := ANewLimit;
end;

procedure TsgSimpleExport.ExpSetPenWidth(AWidth: Double);
begin
  FPenWidthInt := AWidth;
  FPenWidth := FPenWidthInt * FLineWeightScaleByExport;
end;

procedure TsgSimpleExport.ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
  const ATextW: WideString; const ATextParams: PsgExpTextParam);
begin
end;

procedure TsgSimpleExport.ExpExportBlock(const blockString: string);
begin
end;


procedure TsgSimpleExport.ExpPathDataProc(const AHandle: THandle;
  const Edge, Fill: Boolean);
{$IFDEF SG_FIREMONKEY}
var
  vPathData: TPathData;
  vPathNode: TsgSVGPath;
  vPolyPoints: TsgObjectList;
begin
  vPathData := FMX_GetPathData(AHandle);
  try
    if not vPathData.IsEmpty then
    begin
      if IsSupportPathData then
        DoExpPathData(vPathData, Edge, Fill)
      else
      begin
        vPolyPoints := TsgObjectList.Create;
        vPathNode := TsgSVGPath.Create;
        try
          vPathNode.SetData(vPathData.Data);
          vPathNode.GetPolyPoints(vPolyPoints, Self.CADImage.Converter);
          if vPolyPoints.Count > 0 then
            DoExpPolyPoints(vPolyPoints, Edge, Fill);
        finally
          TsgObjectList.FreeList(vPolyPoints);
        end;
      end;
    end;
  finally
    FreeAndNil(vPathData);
  end;
{$ELSE}
begin
{$ENDIF}
end;

function TsgSimpleExport.Flipping: Boolean;
begin
  Result := False;
end;

function TsgSimpleExport.GetColorToLineWeightMode: Boolean;
begin
  Result := FColorToLineWeightMode and 1 <> 0;
end;

function TsgSimpleExport.GetConverter: TsgDXFConverter;
begin
  Result := FCADImage.Converter;
end;

destructor TsgSimpleExportCustom.Destroy;
begin
  inherited Destroy;
end;

procedure TsgSimpleExportCustom.DoInitializeView(var AView: TFMatrix);
begin
  if Assigned(CurrentLayout) then
    AView := CurrentLayout.RotMatrix
  else
    inherited DoInitializeView(AView);
end;

function  TsgSimpleExportCustom.GetCurrentLayout: TsgDXFLayout;
begin
  Result := FCADImage.CurrentLayout;
end;

function TsgSimpleExport.GetLineTypeMode: Integer;
begin
  Result := 0;
end;

function TsgSimpleExport.GetExportRect: TRect;
var
  W,H,K,Max: Double;
  R: TFRect;
begin
  R := ClippingRect;
  Result := cnstRectZero;
  W := R.Right - R.Left;
  H := Abs(R.Top - R.Bottom);
  if W < H then
    Max := H
  else
    Max := W;
  if (FXSize > 0) and (Max <> 0) then
     K := FXSize/Max
   else
     K := 1.0;
  K := K * FXScale;
  Result.Right := Ceil(K*W);
  Result.Bottom := Ceil(K*H);
end;

function TsgSimpleExport.GetFillColor: TColor;
begin
  Result := FFillColor;
end;

function TsgSimpleExport.GetFontColor: TColor;
begin
  Result := FFontColor;
end;

function TsgSimpleExport.GetLineScale: Double;
begin
  Result := {30 * }fLineWeightFactor;
end;

function TsgSimpleExport.GetStrokeColor: TColor;
begin
  Result := FColor;
end;

procedure TsgSimpleExport.PageEnd(N: Integer);
begin
  if Assigned(FOnPageEnd) then
    FOnPageEnd(Self);
end;

procedure TsgSimpleExport.PageStart(N: Integer);
begin
  if Assigned(FOnPageStart) then
    FOnPageStart(Self);
  if FCADImage is TsgVectorImage then
    SetParamsForImage(FCADImage);
end;

function IsText(const Value1, Value2: Pointer): Boolean;
begin
  Result := False;
  if Value1 <> nil then
    Result := TsgDXFEntity(Value1) is TsgDXFText;
  if Value2 <> nil then
    Result := TsgDXFEntity(Value2) is TsgDXFText;
end;

function TsgSimpleExport.ReloadTexts: Boolean;
var
  vReload: Integer;
begin
  Result := True;
  vReload := 0;
  DXFConv.ReLoadEntities(Self.CADImage, nil, IsText, @vReload);
  if vReload = 0 then
    Result := False;
end;

function TsgSimpleExport.RotPict(AGraphic: TGraphic; Angle: Integer): Boolean;
begin
  Result := AGraphic is TsgBitmap;
  if Result then TsgBitmap(AGraphic).Rotate(Angle);
end;

procedure TsgSimpleExportCustom.SetCurrentLayout(const ALayout: TsgDXFLayout);
begin
  FCADImage.CurrentLayout := ALayout;
end;

procedure TsgSimpleExportCustom.SetImage(const AImage: TsgCADImage);
begin
  FCADImage := AImage;
  if Assigned(FCADImage) then
    FParams := @TsgCADImageAccess(FCADImage).FDraw
  else
    FParams := @FInternalParams;
end;

procedure TsgSimpleExport.SetFillColor(AColor: TColor);
begin
end;

procedure TsgSimpleExport.SetTTFMode(const Value: TsgTTFMode);
begin
  FTTFMode := Value;
end;

procedure TsgSimpleExport.SetUsePlotSetting(const Value: Boolean);
begin
  FUsePlotSetting := Value;
end;

procedure TsgSimpleExport.SetUseSHXFonts(const AValue: Boolean);
begin
  FUseSHXFonts := Integer(AValue);
end;

procedure TsgSimpleExport.SetStrokeColor(AColor: TColor);
begin
end;

procedure TsgSimpleExport.SetLineTypeMode(const AValue: Integer);
begin

end;

procedure TsgSimpleExport.SetExpProc(ACADImage: TsgCADImage);
begin
  SetExpProcProxy(ACADImage.ExpProcs);
end;

procedure TsgSimpleExport.SetExpProcProxy(const AExpProcs: TsgProxyExport);
begin
  if not Assigned(AExpProcs) then
    Exit;
  AExpProcs.ExpPolyline := ExpPolyline;
  AExpProcs.ExpPolyPolyline := ExpPolyPolyline;
  AExpProcs.ExpPolygon := ExpPolygon;
  AExpProcs.ExpPolyPolygon := ExpPolyPolygon;
  AExpProcs.ExpPixel := ExpPixel;
  AExpProcs.ExpImage := ExpImage;
  AExpProcs.ExpImageUV := ExpImageUV;
  AExpProcs.ExpFillRgn := ExpFillRgn;
  AExpProcs.ExpClipRgn := ExpClipRgn;
  AExpProcs.ExpCloseFigure := ExpCloseFigure;
  AExpProcs.ExpSaveDC := ExpSaveDC;
  AExpProcs.ExpRestoreDC := ExpRestoreDC;
  AExpProcs.ExpProgress := ExpProgress;
  AExpProcs.ExpSetColor := ExpSetColor;
  AExpProcs.ExpSetPenWidth := ExpSetPenWidth;
  AExpProcs.ExpSetFont := ExpSetFont;
  AExpProcs.ExpSetPenMiterLimit := ExpSetPenMiterLimit;
  AExpProcs.ExpSetPenGeometric := ExpSetPenGeometric;
  AExpProcs.ExpEntProc := ExpEntityProc;
  AExpProcs.SupportColorToLineWeight := IsSupportColorToLineWeight;
{$IFDEF SG_FM_WINDOWS}
  AExpProcs.ExpPathData := ExpPathDataProc;
{$ENDIF}
  //ACADImage.ExpSetColorBrush := ExpSetColorBrush;
  //ACADImage.ExpTextOut := ExpTextOut;
  AExpProcs.LineTypeMode := LineTypeMode;
end;

procedure TsgSimpleExport.ApplyTTFMode;
begin
  FCADImage.TTFMode := FTTFMode;
end;

procedure TsgSimpleExport.CheckColorToLineWeight(ACADImage: TsgCADImage);
begin
  TsgCADImageAccess(ACADImage).CheckColorToLineWeight;
end;

procedure TsgSimpleExport.ClearExpProc(ACADImage: TsgCADImage);
begin
  if Assigned(ACADImage.ExpProcs) then
    ACADImage.ExpProcs.Clear;
end;

{public}
constructor TsgSimpleExport.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FTempEntities := TsgObjectList.Create;
  SetExpProc(FCADImage);
  FCADImage.TTFMode := ttfPolyPolygon; //evg
  FTTFMode := FCADImage.TTFMode;
  FCADImage.mmToPixelX := GetMmToPixelX;
  FCADImage.BackgroundColor := clWhite;
  FCADImage.DefaultColor := clBlack;
  FCADImage.LineWeightScale := 1.0;
//  FCADImage.BorderType := btRatio;  // Bad result in simple export
  IsShowLineWeight := True;
  FCADImage.ShowImages := True;
  FCADImage.UseWinEllipse := False;
  FCADImage.NullWidthExt := DefExportParams.NullWidth;
  SetNormalsInternal(1, -1);
  FXScale := 1.0;
  FLayoutExportMode := lemCurrentLayout; //lemAllPaperSpaces;
  FLayoutsExportCount := 0;
  FLineWeightScaleByExport := 1;
  FUseSHXFonts := -1;//Integer(FCADImage.Converter.UseSHXFonts);

  FEntitiesStack := TsgObjectList.Create;
end;

destructor TsgSimpleExport.Destroy;

  procedure RemoveFromTempEntities(ASection: TConvSection);
  var
    I: Integer;
    vEntity: TsgDXFEntity;
  begin
    for I := 0 to FTempEntities.Count - 1 do
    begin
      vEntity := TsgDXFEntity(FTempEntities[I]);
      if CADImage.Converter.Sections[ASection].IndexOfEntity(vEntity) >= 0 then
        try
          FTempEntities[I] := nil;
          CADImage.Converter.Sections[ASection].RemoveEntity(vEntity);
        finally
          FreeAndNil(vEntity);
        end;
    end;
  end;

begin
  FOnPageStart := nil;
  FOnPageEnd := nil;
  FreeAndNil(FEntitiesStack);
  RemoveFromTempEntities(csEntities);
  RemoveFromTempEntities(csBlocks);
  RemoveFromTempEntities(csStyles);
  RemoveFromTempEntities(csTableStyles);
  RemoveFromTempEntities(csLayers);
  RemoveFromTempEntities(csLayouts);
  TsgObjectList.FreeList(FTempEntities);
  FreeAndNil(FStampBlock);
  CADImage.Converter.Sections[csStyles].RemoveEntity(FStampBoldStyle);
  CADImage.Converter.Sections[csStyles].RemoveEntity(FStampDefStyle);
  FreeAndNil(FStampBoldStyle);
  FreeAndNil(FStampDefStyle);
  FreeAndNil(FSelectedLayouts);
  FreeAndNil(FColorToLineWeight);
  inherited Destroy;
end;

procedure TsgSimpleExport.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  LayoutExportMode := TsgLayoutExportMode(AExportParams.LayoutExportMode);
  LayoutNameExportMode := AExportParams.LayoutNameExportMode;
  FXMLParamsExtended := AExportParams.XMLParamsExtended;
  FUseSHXFonts := AExportParams.UseSHXFonts;
  LineTypeMode := AExportParams.LineTypeMode;
  IgnoreLayerNoPlotting := AExportParams.IgnoreLayerNoPlotting;
  FreeAndNil(FColorToLineWeight);
  if Length(AExportParams.ColorToLineWeight) > 0 then
  begin
    FColorToLineWeight := TStringList.Create;
    FColorToLineWeight.Text := AExportParams.ColorToLineWeight;
    FColorToLineWeightMode := AExportParams.ColorToLineWeightMode;
  end;
  if (AExportParams.TTFMode >= Integer(Low(TsgTTFMode))) and
     (AExportParams.TTFMode <= Integer(High(TsgTTFMode))) then
    TTFMode := TsgTTFMode(AExportParams.TTFMode);
  Transparency := not AExportParams.FillBackground;
end;

procedure TsgSimpleExport.SaveToStream(S: TStream);
begin
  FreeAndNil(FSelectedLayouts);
  if Assigned(FCADImage) and Assigned(FCADImage.ExpProcs) and (LineTypeMode >= 0) then
    FCADImage.ExpProcs.LineTypeMode := LineTypeMode;
  FEntitiesStack.Count := 0;
  FLayoutsExportCount := GetLayoutsExportCount;
  if FLayoutsExportCount > 0 then
    inherited SaveToStream(S);
end;

procedure TsgSimpleExport.SaveToStreamCustom(S: TStream);
var
  vHasClippingRect: Boolean;
  vClippingRect, vTmpClippingRect: TFRect;
  vCadImagePrinting: TsgPrintMode;
  vCadImageUsePlotSetting: Boolean;
  vUpdateTexts, vTextsReloaded: Boolean;
  vExportUseLayerPlottingPrev: Boolean;
  vCADImageTTFMode: TsgTTFMode;
begin
  if Assigned(FColorToLineWeight) then
  begin
    AssignColorToLineWeights(FColorToLineWeight, Self.CADImage.ColorToLineWeight);
    CheckColorToLineWeight(Self.CADImage);
    Self.CADImage.UseColorRGBInColorMap := ApplyPrintStylesToRGB;
  end
  else
    Self.CADImage.ColorToLineWeight.Clear;
  vCADImageTTFMode := CADImage.TTFMode;
  vCadImagePrinting := CADImage.Printing;
  vCadImageUsePlotSetting := CADImage.UsePlotSetting;
  vHasClippingRect := HasClippingRect;
  vExportUseLayerPlottingPrev := TsgCADImageAccess(FCADImage).FExportUseLayerPlotting;
  vUpdateTexts := False;
  vTextsReloaded := False;
  try
    ApplyTTFMode;
    CADImage.Printing := pntDirectColor;
    if vHasClippingRect then
    begin
      vTmpClippingRect := FCADImage.ClippingRect^;
      vClippingRect := ClippingRect;
      FCADImage.ClippingRect := @vClippingRect;
    end;
    FCADImage.UsePlotSetting := UsePlotSetting;
    TsgCADImageAccess(FCADImage).FExportUseLayerPlotting := IgnoreLayerNoPlotting;
    if FUseSHXFonts > -1 then
    begin
      if Self.CADImage.Converter.UseSHXFonts <> UseSHXFonts then
      begin
        vUpdateTexts := True;
        Self.CADImage.Converter.UseSHXFonts := UseSHXFonts;
        vTextsReloaded := ReloadTexts;
      end;
    end;
    if FCount = 0 then
    begin
      Iterate(CntLayout);
      CntSGBlock;
    end;
    LoadSGBlock;
    FCurrent := 0;
    FCurrentStep := 0;
    FStep := FCount*5 div 100;
    Progress(psStarting, 0, '');
    Iterate(ExpLayout);
    Progress(psEnding, 100, '');
  finally
    TsgCADImageAccess(FCADImage).FExportUseLayerPlotting := vExportUseLayerPlottingPrev;
    if vUpdateTexts then
    begin
      Self.CADImage.Converter.UseSHXFonts := not UseSHXFonts;
      if vTextsReloaded then
        ReloadTexts;
    end;
    CADImage.TTFMode := vCADImageTTFMode;
    CADImage.Printing := vCadImagePrinting;
    CADImage.UsePlotSetting := vCadImageUsePlotSetting;
    if vHasClippingRect then
      FCADImage.ClippingRect := @vTmpClippingRect;
  end;
end;

{TsgSimpleExportMetafile}

procedure TsgSimpleExportMetafile.SaveToStreamCustom(S: TStream);
begin

end;

{ TsgDXFInsertProtect }

destructor TsgDXFInsertProtect.Destroy;
begin
  Block := nil;
  if Assigned(FParent) then
  begin
    FParent.RemoveEntity(Self);
    FParent := nil;
  end;
  inherited Destroy;
end;

procedure TsgDXFInsertProtect.SetParent(const Value: TsgDXFEntity);
begin
  FParent := Value;
end;

{ TsgCommonExportParams }

procedure TsgCommonExportParams.Assign(const AObject: TsgCommonExportParams;
  const AIgnoreDestination: Boolean = False);
begin
  if not AIgnoreDestination then
  begin
    if Assigned(AObject.FDestination) then
      Destination := AObject.Destination
    else
      FreeAndNil(FDestination);
  end;
  Format := AObject.Format;
  Cliping := AObject.Cliping;
  CADExportParams := AObject.CADExportParams;
  RasterExportParams := AObject.RasterExportParams;
end;

procedure TsgCommonExportParams.AssignTo(const AExp: TsgCommonExportParams;
  const AParams: Int64 = 0);
begin
  AExp.Assign(Self, AParams and 1 <> 0);
  if AParams and 2 <> 0 then
    AExp.FXmlParams := Self.FXmlParams;
end;

procedure TsgCommonExportParams.CheckVersionOnFormat;
begin
  case Format of
    efDxf:
      begin
        if not IsSupportVersionOnFormat then
          CADExportParams.Version := DefExportParams.Version;
      end;
    efDwg:
      begin
        if not IsSupportVersionOnFormat then
          CADExportParams.Version := DefExportParams.Version;
      end;
  end;
  case Format of
    efDxf, efDwg:
      begin
        if not IsSupportVersionOnFormat then
          CADExportParams.Version := acR2004;
      end;
  end;
end;

constructor TsgCommonExportParams.Create;
begin
  inherited Create;
  SetDefaultParams;
end;

destructor TsgCommonExportParams.Destroy;
begin
  SetDestination(nil);
  inherited Destroy;
end;

procedure TsgCommonExportParams.DoAfterSaveToStream(Sender: TObject);
begin
  if Assigned(FOnAfterSaveToStream) then
    FOnAfterSaveToStream(Sender);
end;

procedure TsgCommonExportParams.DoBeforeSaveToStream(Sender: TObject);
begin
  if Assigned(FOnBeforeSaveToStream) then
    FOnBeforeSaveToStream(Sender);
end;

function TsgCommonExportParams.ReadFormatFromNode(const AExportParamsNode: TsgNode): TsgExportFormat;
var
  vExt, vFileName: string;
  vNode: TsgNodeSample;
  vFormat: TsgExportFormat;
begin
  vFormat := efAuto;
  if Assigned(AExportParamsNode) then
  begin
    vNode := AExportParamsNode.GetAttributeByName(cnstXMLFileName);
    if Assigned(vNode) then
      vFileName := vNode.ValueAsStr
    else
      vFileName := '';
    vExt := ExtractFileExt(vFileName);
    vNode :=  AExportParamsNode.GetAttributeByName(cnstXMLFormat);
    if Assigned(vNode) then
    begin
      vExt := vNode.ValueAsStr;
      if Length(vExt) > 0 then
      begin
        if vExt[1] <> cnstPoint then
          vExt := cnstPoint + vExt;
      end;
    end;
    vFormat := GetExportFormat(vExt);
  end;
  Result := vFormat;
end;

procedure CheckPDFPageSize(var ACADExportParams: TsgExportParams;
  ADefaultParams: TsgExportParams; const ANodes: TsgNodeList);

  function IsSizeUnitsMM(const S: string; var W, H: Double): Boolean;
  var
    vPos, I: Integer;
    vS: string;
  begin
    Result := False;
    vPos := Pos(' x ', S);
    if vPos > 0 then
    begin
      I := vPos + 3;
      repeat
        vS := S[vPos - 1] + vS;
        Dec(vPos);
      until S[vPos - 1] = ' ';
      W := StrToDouble(vS);
//      StrToD(vS, W);
      vS := '';
      repeat
        vS := vS + S[I];
        Inc(I);
      until S[I] = ' ';
      H := StrToDouble(vS);
//      StrToD(vS, H);
      Inc(I);
      if S[I] <> '"' then
        Result := True;
    end;
  end;

var
  I: Integer;
  vPageSize: string;
  vOrientation: Integer;
begin
  vOrientation := GetXMLNodeTextAsInt(ANodes[cnstXMLPageOrientation]);
  vPageSize := GetXMLNodeTextAsString(ANodes[cnstXMLPageSize]);
  if vPageSize <> '' then
  begin
    for I := 0 to Length(cnstPageSizes) - 1 do
      if Pos(vPageSize, cnstPageSizes[I]) > 0 then
      begin
        IsSizeUnitsMM(cnstPageSizes[I], ACADExportParams.PageWidth,
          ACADExportParams.PageHeight);
        if vOrientation <> 0 then
          SwapDoubles(ACADExportParams.PageWidth, ACADExportParams.PageHeight);
        Break;
      end;
  end;
end;

function TsgCommonExportParams.FromNode(const AExportParamsNode: TsgNode;
  AStorage: TsgStorage = nil;
  const ADefaultParams: TsgCommonExportParams = nil):  Boolean;

  function GetGcodeExtID(AFileExt: string): Integer;
  var
    I: Integer;
    vExt: string;
  begin
    Result := 0;
    for I := 0 to 2 do
    begin
      vExt := cnstGcodeExts[TsgGcodeExts(I)];
      if Pos(AFileExt, vExt) > 0 then
        Result := I
    end;
  end;

  function ColorToLineWeightFromNode(const ANode: TsgNodeSample;
    const ADefault: string): string;
  var
    vParams: string;
    vStrings: TStringList;
  begin
    Result := ADefault;
    vParams := GetXMLNodeTextAsString(ANode);
    if Length(vParams) > 0 then
    begin
      vStrings := TStringList.Create;
      try
        try
          if AnsiPos('=', vParams) > 0 then
            Result := vParams
          else
          begin
            if IsFileExists(vParams) then
              vStrings.LoadFromFile(vParams);
          end;
          if vStrings.Count > 0 then
          begin
            CorrectFileCws(vStrings);
            Result := vStrings.Text;
          end;
        except
        end;
      finally
        FreeAndNil(vStrings);
      end;
    end;
  end;

var
  vNode: TsgNodeSample;
  vFileName, vExt, vExtFromFileName: string;
  vFormat: TsgExportFormat;
  vFileNameEmpty: Boolean;
  vACISColorFlags: UInt64;
  vNodes: TsgNodeList;
  vRastrExportParams: TsgRastrExportParams;
  vCADExportParams: TsgExportParams;
  vImageSize: TF2DPoint;
  vPdfBitmapExportMode: Integer;
begin
  Result := False;
  FXmlParams := False;
  try
  InitDefaultCommonExportParams(AStorage, ADefaultParams);
  if Assigned(AExportParamsNode) then//version 2
  begin
// Common attributes
    vNode := AExportParamsNode.GetAttributeByName(cnstXMLFileName);
    if Assigned(vNode) then
      vFileName := vNode.ValueAsStr
    else
      vFileName := '';
    vExtFromFileName := ExtractFileExt(vFileName);
    vExt := vExtFromFileName;
    Format := GetExportFormat(vExt);
    vFileNameEmpty := ChangeFileExt(vFileName, '') = '';
    vNode := AExportParamsNode.GetAttributeByName(cnstXMLFormat);
    if Assigned(vNode) then
    begin
      vExt := vNode.ValueAsStr;
      if Length(vExt) > 0 then
      begin
        if vExt[1] <> cnstPoint then
          vExt := cnstPoint + vExt;
        vFormat := GetExportFormat(vExt);
        if vFormat <> Format then
        begin
          Format := vFormat;
          vFileName := vFileName + vExt;
        end;
      end;
    end
    else
    begin
      {$IFDEF SGABVIEWER}
      if Format = efAuto then
      begin
        Format := DefExportFormat;
        vExt := GetExportExt(Format);
        vFileName := vFileName + vExt;
      end;
      {$ENDIF}
    end;
    if (vExt = '') and (vExtFromFileName <> '') then
      vExt := vExtFromFileName;
    Format := GetExportFormat(vExt);
    if AStorage = nil then
    begin
      if not vFileNameEmpty then
        SaveFileName := vFileName;
    end;
    if (AExportParamsNode.ChildNodesCount > 0) then
    begin
      SetExportParamsDefault(vCADExportParams, vRastrExportParams);
      if Assigned(ADefaultParams) then
      begin
        vRastrExportParams := ADefaultParams.RasterExportParams;
        vCADExportParams := ADefaultParams.CADExportParams;
      end;
      vNodes := AExportParamsNode.Childs;
      try
  //  Clipping
        Cliping.ClipMode := GetXMLNodeTextAsInt(
          vNodes[cnstXMLClipMode], {$IFNDEF SG_FIREMONKEY}0{$ELSE}7{$ENDIF});
        Cliping.ClipRect := GetXMLNodeTextAsFRect(
          vNodes[cnstXMLClipRect], cnstBadRect);
  //  Raster
        RasterExportParams.ExportFormat := Format;
        RasterExportParams.Width := GetXMLNodeTextAsInt(
          vNodes[cnstXMLWidth], vRastrExportParams.Width);
        RasterExportParams.Height := GetXMLNodeTextAsInt(
          vNodes[cnstXMLHeight], vRastrExportParams.Height);
        RasterExportParams.MaxDim := GetXMLNodeTextAsInt(
          vNodes[cnstXMLMaxDim], vRastrExportParams.MaxDim);
        RasterExportParams.Quality := GetXMLNodeTextAsInt(
          vNodes[cnstXMLQuality], vRastrExportParams.Quality);//iJPEGQuality
        RasterExportParams.MeasureInPixels := GetXMLNodeTextAsBool(
          vNodes[cnstXMLMeasureInPixels], vRastrExportParams.MeasureInPixels);
        RasterExportParams.Transparent := GetXMLNodeTextAsBool(
          vNodes[cnstXMLTransparent], vRastrExportParams.Transparent);
        RasterExportParams.DPU.X := GetXMLNodeTextAsInt(
          vNodes[cnstXMLDpuX], vRastrExportParams.DPU.X);
        RasterExportParams.DPU.Y := GetXMLNodeTextAsInt(
          vNodes[cnstXMLDpuY], vRastrExportParams.DPU.Y);
        RasterExportParams.SizeMode := GetXMLNodeTextAsSizeMode(
          vNodes[cnstXMLSizeMode], vRastrExportParams.SizeMode);
        RasterExportParams.Compression := GetXMLNodeTextAsTiffComprassion(
          vNodes[cnstXMLCompression], vRastrExportParams.Compression);
        RasterExportParams.Depth := GetXMLNodeTextAsPixelFormat(
          vNodes[cnstXMLPixelFormat], vRastrExportParams.Depth);
        RasterExportParams.SaveProportion := GetXMLNodeTextAsBool(
          vNodes[cnstXMLSaveProportion], vRastrExportParams.SaveProportion);
        vImageSize.X := GetXMLNodeTextAsDouble(vNodes[cnstXMLWidthM],
          vRastrExportParams.Size.X);
        vImageSize.Y := GetXMLNodeTextAsDouble(vNodes[cnstXMLHeightM],
          vRastrExportParams.Size.Y);
        RasterExportParams.Size := vImageSize;
        RasterExportParams.DPM := GetXMLNodeTextAsDouble(vNodes[cnstXMLDPM],
          vRastrExportParams.DPM);
        RasterExportParams.UserScale := GetXMLNodeTextAsString(vNodes[cnstXMLUserScale],
          vRastrExportParams.UserScale);
        RasterExportParams.ImageQuality := GetXMLNodeTextAsDouble(vNodes[cnstXMLImageQuality],
          vRastrExportParams.ImageQuality);
        RasterExportParams.BlackWhitePic := GetXMLNodeTextAsBool(vNodes[cnstXMLBlackWhitePic],
          vRastrExportParams.BlackWhitePic);
        RasterExportParams.Smooth := GetXMLNodeTextAsBool(vNodes[cnstXMLSmoothing],
          vRastrExportParams.Smooth);
  // CAD
    // DWG/DXF
        CADExportParams.Version := GetXMLNodeTextAsVersion(
          vNodes[cnstXMLVersion], vCADExportParams.Version);
        CADExportParams.AlternateBlack := GetXMLNodeTextAsBool(
          vNodes[cnstXMLAlternateBlack], vCADExportParams.AlternateBlack);
        CADExportParams.UnitSize := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLUnitSize], vCADExportParams.UnitSize);
        CADExportParams.Use01MM := GetXMLNodeTextAsBool(
          vNodes[cnstXMLUse01MM], vCADExportParams.Use01MM);
        CADExportParams.LineWeightScale := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLLineWeightScale], vCADExportParams.LineWeightScale);
        CADExportParams.FillingEntity := GetXMLNodeTextAsBool(
          vNodes[cnstXMLFillingEntity], vCADExportParams.FillingEntity);
        CADExportParams.OffsetPoint := GetXMLNodeTextAsTF2DPoint(
          vNodes[cnstXMLOffsetPoint], vCADExportParams.OffsetPoint);
        CADExportParams.LayersMode := GetXMLNodeTextAsInt(
          vNodes[cnstXMLLayersMode], vCADExportParams.LayersMode);
        //CheckVersionOnFormat;
    //
    // SAT/SAB
        SetBit64(0, Byte(GetXMLNodeTextAsBool(vNodes[cnstXmlWriteTclColors], False)), vACISColorFlags);
        SetBit64(1, Byte(GetXMLNodeTextAsBool(vNodes[cnstXmlWriteAdskColors], False)), vACISColorFlags);
        SetBit64(2, Byte(GetXMLNodeTextAsBool(vNodes[cnstXmlWriteAdskIndexColors], False)), vACISColorFlags);
        CADExportParams.ACISColorFlags := vACISColorFlags;
    //
    // OBJ
        CADExportParams.CreateMtl := GetXMLNodeTextAsBool(vNodes[cnstXmlCreateMtl],
          vCADExportParams.CreateMtl);
    //

// PDF Bitmap Export Mode
       vPdfBitmapExportMode := GetXMLNodeTextAsInt(
         vNodes[cnstXMLPdfBitmapExportMode], 0);
       if vPdfBitmapExportMode <= 0 then
       begin
         CADExportParams.PdfBitmapMode := bemNone;
       end
       else
       begin
         case vPdfBitmapExportMode of
           1:
             CADExportParams.PdfBitmapMode := bem1bit;
           8:
             CADExportParams.PdfBitmapMode := bem8bit;
           24:
             CADExportParams.PdfBitmapMode := bem24bit;
           else
             CADExportParams.PdfBitmapMode := bem24bit;
         end;
       end;
//
        CADExportParams.PageWidth := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLPageWidth], vCADExportParams.PageWidth);
        CADExportParams.PageHeight := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLPageHeight], vCADExportParams.PageHeight);
//        if Format = efPdf then
          CheckPDFPageSize(CADExportParams, vCADExportParams, vNodes);
        CADExportParams.Margin := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLMargin], vCADExportParams.Margin);
        CADExportParams.DrawMode := GetXMLNodeTextAsInt(
          vNodes[cnstXMLDrawMode], vCADExportParams.DrawMode);
        CADExportParams.BackgroundColor := GetXMLNodeTextAsInt(
          vNodes[cnstXMLBackgroundColor], vCADExportParams.BackgroundColor);
        CADExportParams.DefaultColor := GetXMLNodeTextAsInt(
          vNodes[cnstXMLDefaultColor], vCADExportParams.DefaultColor);
        CADExportParams.NullWidth := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLNullWidth], vCADExportParams.NullWidth);
        CADExportParams.SaveLineWeight := GetXMLNodeTextAsBool(
          vNodes[cnstXMLSaveLineWeight], vCADExportParams.SaveLineWeight);
        CADExportParams.UseHighQuality := GetXMLNodeTextAsBool(
          vNodes[cnstXMLUseHighQuality], vCADExportParams.UseHighQuality);
        CADExportParams.ExportTexts := GetXMLNodeTextAsBool(
          vNodes[cnstXMLExportTexts], vCADExportParams.ExportTexts);
        CADExportParams.SHXAnnotations := GetXMLNodeTextAsBool(
          vNodes[cnstXMLSHXAnnotations], vCADExportParams.SHXAnnotations);
        CADExportParams.UseOutlines := GetXMLNodeTextAsBool(
          vNodes[cnstXMLUseOutlines], vCADExportParams.UseOutlines);
        CADExportParams.UseSHXFonts := GetXMLNodeTextAsInt(
          vNodes[cnstXMLUseSHXFonts], vCADExportParams.UseSHXFonts);
        CADExportParams.TrueTypeOptimization := GetXMLNodeTextAsBool(
          vNodes[cnstXMLTrueTypeOptimization], vCADExportParams.TrueTypeOptimization);
        CADExportParams.LayoutExportMode := GetXMLNodeTextAsInt(
          vNodes[cnstXMLLayoutExportMode], vCADExportParams.LayoutExportMode);
        CADExportParams.LayoutNameExportMode := GetXMLNodeTextAsString(
          vNodes[cnstXMLLayoutNameExportMode], vCADExportParams.LayoutNameExportMode);
        CADExportParams.Title := GetXMLNodeTextAsString(
          vNodes[cnstXMLTitle], vCADExportParams.Title);
        CADExportParams.Author := GetXMLNodeTextAsString(
          vNodes[cnstXMLAuthor], vCADExportParams.Author);
        CADExportParams.Subjct := GetXMLNodeTextAsString(
          vNodes[cnstXMLSubjct], vCADExportParams.Subjct);
        CADExportParams.Keywords := GetXMLNodeTextAsString(
          vNodes[cnstXMLKeywords], vCADExportParams.Keywords);
        CADExportParams.LineWeights := GetXMLNodeTextAsString(
          vNodes[cnstXMLLineWeights], vCADExportParams.LineWeights);
        CADExportParams.UseVectorLineWeight := GetXMLNodeTextAsBool(
          vNodes[cnstXMLUseVectorLineWeight], vCADExportParams.UseVectorLineWeight);
        CADExportParams.UseExtentsFromPlotSettings := GetXMLNodeTextAsBool(
          vNodes[cnstXMLUseExtentsFromPlotSettings], vCADExportParams.UseExtentsFromPlotSettings);
        CADExportParams.HPGLXScale := GetXMLNodeTextAsDouble(
          vNodes[cnstXMLHPGLXScale], vCADExportParams.HPGLXScale);
        CADExportParams.IsConvertImageToOLE := GetXMLNodeTextAsBool(
          vNodes[cnstXMLIsConvertImageToOLE], vCADExportParams.IsConvertImageToOLE);
        CADExportParams.SizeAsExtents:= GetXMLNodeTextAsBool(
          vNodes[cnstXMLSizeAsExtents], vCADExportParams.SizeAsExtents);
        CADExportParams.MMInCad:= GetXMLNodeTextAsDouble(
          vNodes[cnstXMLMMInCad], vCADExportParams.MMInCad);
        CADExportParams.XMLParamsExtended := SaveNodeToXMLString(
          vNodes[cnstXMLPlottingScale], CADExportParams.XMLParamsExtended);
        CADExportParams.XMLParamsExtended := SaveNodeToXMLString(
          vNodes[cnstXMLExtendedParams], CADExportParams.XMLParamsExtended);
        CADExportParams.ColorToLineWeight := ColorToLineWeightFromNode(
          vNodes[cnstXmlColorToLineWeight], '');
        CADExportParams.ColorToLineWeightMode := GetXMLNodeTextAsInt(vNodes[cnstXmlColorToLineWeightMode],
          vCADExportParams.ColorToLineWeightMode);
        CADExportParams.ColorSpace := TsgColorSpace(GetXMLNodeTextAsIntWithRang(
          vNodes[cnstXmlColorSpace],  Ord(Low(TsgColorSpace)), Ord(High(TsgColorSpace)),
          Ord(vCADExportParams.ColorSpace)));
        CADExportParams.TextAsPolygons := GetXMLNodeTextAsBool(
          vNodes[cnstXmlTextAsPolygons], vCADExportParams.TextAsPolygons);
  {$IFDEF SG_VECTORIZATION}
        CADExportParams.VectorizationMode := GetXMLNodeTextAsInt(
          vNodes[cnstVectorizationMode], vCADExportParams.VectorizationMode);
  {$ENDIF}
        CADExportParams.TTFMode := GetXMLNodeTextAsInt(
          vNodes[cnstTTFMode], vCADExportParams.TTFMode);
        CADExportParams.IgnoreLayerNoPlotting := GetXMLNodeTextAsBool(
          vNodes[cnstXmlIgnoreLayerNoPlotting], vCADExportParams.IgnoreLayerNoPlotting);
//      SVG Params
        CADExportParams.LineTypeMode := GetXMLNodeTextAsInt(
          vNodes[cnstXmlLineTypeMode], vCADExportParams.LineTypeMode);
        CADExportParams.NullWeightMode := GetXMLNodeTextAsInt(
          vNodes[cnstXmlNullWeightMode], vCADExportParams.NullWeightMode);
        CADExportParams.FillBackground := GetXMLNodeTextAsBool(
          vNodes[cnstXmlFillBackground], vCADExportParams.FillBackground);
  {$IFDEF SG_BTI}
        CADExportParams.UseFillStyleIndex := GetXMLNodeTextAsBool(
          vNodes[cnstUseFillStyleIndex], vCADExportParams.UseFillStyleIndex);
  {$ENDIF}
    // Gcode Params
        CADExportParams.ArcToLines := GetXMLNodeTextAsBool(
          vNodes[cnstArcToLines], vCADExportParams.ArcToLines);
        CADExportParams.MachineTypeID := GetXMLNodeTextAsInt(
          vNodes[cnstMachineType], vCADExportParams.MachineTypeID);
        CADExportParams.PassesDirectionID := GetXMLNodeTextAsInt(
          vNodes[cnstPassesDirection], vCADExportParams.PassesDirectionID);
        CADExportParams.PrecisionDigits := GetXMLNodeTextAsInt(
          vNodes[cnstPrecisionDigits], vCADExportParams.PrecisionDigits);
        CADExportParams.PrecisionFactor := GetXMLNodeTextAsInt(
          vNodes[cnstPrecisionFactor], vCADExportParams.PrecisionFactor);
        CADExportParams.FeedOnZ := GetXMLNodeTextAsInt(
          vNodes[cnstFeedOnZ], vCADExportParams.FeedOnZ);
        CADExportParams.FeedOnXY := GetXMLNodeTextAsInt(
          vNodes[cnstFeedOnXY], vCADExportParams.FeedOnXY);
        CADExportParams.SpindleSpeed := GetXMLNodeTextAsInt(
          vNodes[cnstSpindleSpeed], vCADExportParams.SpindleSpeed);
        CADExportParams.DepthOnZ := GetXMLNodeTextAsDouble(
          vNodes[cnstDepthOnZ], vCADExportParams.DepthOnZ);
        CADExportParams.DepthPass := GetXMLNodeTextAsDouble(
          vNodes[cnstDepthPass], vCADExportParams.DepthPass);
        CADExportParams.NumbOfPasses := GetXMLNodeTextAsInt(
          vNodes[cnstNumbOfPasses], vCADExportParams.NumbOfPasses);
        CADExportParams.DepartureOnZ := GetXMLNodeTextAsDouble(
          vNodes[cnstDepartureOnZ], vCADExportParams.DepartureOnZ);
        CADExportParams.LaserOnCommand := GetXMLNodeTextAsString(
          vNodes[cnstLaserOnCommand], vCADExportParams.LaserOnCommand);
        CADExportParams.LaserOffCommand := GetXMLNodeTextAsString(
          vNodes[cnstLaserOffCommand], vCADExportParams.LaserOffCommand);
        CADExportParams.UseLaserPower := GetXMLNodeTextAsBool(
          vNodes[cnstUseLaserPower], vCADExportParams.UseLaserPower);
        CADExportParams.LaserPower := GetXMLNodeTextAsInt(
          vNodes[cnstLaserPower], vCADExportParams.LaserPower);
        CADExportParams.Delay := GetXMLNodeTextAsInt(vNodes[cnstDelay],
          vCADExportParams.Delay);
        CADExportParams.MachineUnitsID := GetXMLNodeTextAsInt(
          vNodes[cnstMachineUnits], vCADExportParams.MachineUnitsID);
        CADExportParams.DrawingUnitsID := GetXMLNodeTextAsInt(
          vNodes[cnstDrawingUnits], vCADExportParams.DrawingUnitsID);
        CADExportParams.AddLabelOfProgam := GetXMLNodeTextAsBool(
          vNodes[cnstAddLabelOfProgam], vCADExportParams.AddLabelOfProgam);
        CADExportParams.LabelOfProgram := GetXMLNodeTextAsString(
          vNodes[cnstLabelOfProgram], vCADExportParams.LabelOfProgram);
        CADExportParams.AddNumbering := GetXMLNodeTextAsBool(
          vNodes[cnstAddNumbering], vCADExportParams.AddNumbering);
        CADExportParams.ShowContourName := GetXMLNodeTextAsBool(
          vNodes[cnstShowContourName], vCADExportParams.ShowContourName);
        CADExportParams.ShowPercent := GetXMLNodeTextAsBool(
          vNodes[cnstShowPercent], vCADExportParams.ShowPercent);
        CADExportParams.StartNumber := GetXMLNodeTextAsInt(
          vNodes[cnstStartNumber], vCADExportParams.StartNumber);
        CADExportParams.StepOfNumbering := GetXMLNodeTextAsInt(
          vNodes[cnstStepOfNumbering], vCADExportParams.StepOfNumbering);
        CADExportParams.Tools := GetXMLNodeTextAsString(
          vNodes[cnstToolParams], vCADExportParams.Tools);
        CADExportParams.WorkpieceZeroPointID := GetXMLNodeTextAsInt(
          vNodes[cnstWorkpieceZeroPointID], vCADExportParams.WorkpieceZeroPointID);
        CADExportParams.ZeroPointOffsetX := GetXMLNodeTextAsDouble(
          vNodes[cnstZeroPointOffsetX], vCADExportParams.ZeroPointOffsetX);
        CADExportParams.ZeroPointOffsetY := GetXMLNodeTextAsDouble(
          vNodes[cnstZeroPointOffsetY], vCADExportParams.ZeroPointOffsetY);
        CADExportParams.CodeOptimize := GetXMLNodeTextAsBool(
          vNodes[cnstCodeOptizime], vCADExportParams.CodeOptimize);
        CADExportParams.PositioningSystemID := GetXMLNodeTextAsInt(
          vNodes[cnstPositioningSystem], vCADExportParams.PositioningSystemID);
        CADExportParams.TrailingZeros := GetXMLNodeTextAsBool(
          vNodes[cnstTrailingZeros], vCADExportParams.TrailingZeros);
        CADExportParams.GcodeExtID := GetGcodeExtID(vExt);
        CADExportParams.ConvertOnlyVisibleLayers := GetXMLNodeTextAsBool(
          vNodes[cnstConvOnlyVisLayers], vCADExportParams.ConvertOnlyVisibleLayers);
        //Dispenser
        CADExportParams.TurnOffPumpBefore := GetXMLNodeTextAsBool(
          vNodes[cnstTurnOffPumpBefore], vCADExportParams.TurnOffPumpBefore);
        CADExportParams.TurnOffPumpBeforeValue := GetXMLNodeTextAsDouble(
          vNodes[cnstTurnOffPumpBeforeValue], vCADExportParams.TurnOffPumpBeforeValue);
        CADExportParams.DispenserOffCommand := GetXMLNodeTextAsString (
          vNodes[cnstDispenserOffCommand], vCADExportParams.DispenserOffCommand);
        CADExportParams.DispenserOnCommand := GetXMLNodeTextAsString(
          vNodes[cnstDispenserOnCommand], vCADExportParams.DispenserOnCommand);
        CADExportParams.PumpOffCommand := GetXMLNodeTextAsString(
          vNodes[cnstPumpOffCommand], vCADExportParams.PumpOffCommand);
        CADExportParams.PumpOnCommand := GetXMLNodeTextAsString(
          vNodes[cnstPumpOnCommand], vCADExportParams.PumpOnCommand);
        CADExportParams.StraightenContour := GetXMLNodeTextAsBool(
          vNodes[cnstStraightenContour], vCADExportParams.StraightenContour);
    //
      except
        SetExportParamsDefault(CADExportParams, RasterExportParams);
      end;
    end;
    Result := True;
  end;
  finally
    FXmlParams := Result;
  end;
end;

function TsgCommonExportParams.FromString(const AExportParam: string;
  AStorage: TsgStorage = nil;
  const ADefaultParams: TsgCommonExportParams = nil): Boolean;
var
  vParser: TsgParser;
  vRoot: TsgNode;
begin
  Result := False;
  vParser := TsgParser.Create;
  try
    InitDefaultCommonExportParams(AStorage, ADefaultParams);
    if vParser.LoadFromString(AExportParam) then
    begin
      vRoot := GetExportParamsNode(vParser);
      if Assigned(vRoot) then
        Result := FromNode(vRoot, AStorage)
    end;
  finally
    vParser.Free;
  end;
end;

function TsgCommonExportParams.GetDestination: TsgStorage;
begin
  if not Assigned(FDestination) then
  begin
    FDestination := TsgStorage.Create;
    FDestinationCookie := FDestination.AddListener(StorageDestroy);
    TsgStorageAccess(FDestination).OnBeforeSaveToStream := DoBeforeSaveToStream;
    TsgStorageAccess(FDestination).OnAfterSaveToStream := DoAfterSaveToStream;
  end;
  Result := FDestination;
end;

function TsgCommonExportParams.GetSaveFileName: string;
begin
  Result := Destination.FileName;
end;

procedure TsgCommonExportParams.InitDefaultCommonExportParams(AStorage: TsgStorage;
  const ADefaultParams: TsgCommonExportParams);
begin
  Destination := AStorage;
  if Assigned(ADefaultParams) then
  begin
    if Self <> ADefaultParams then
      Assign(ADefaultParams, True);
  end
  else
    SetExportParamsDefault(CADExportParams, RasterExportParams);
  Format := efAuto;
  Cliping.ClipMode := {$IFNDEF SG_FIREMONKEY}0{$ELSE}7{$ENDIF};
  Cliping.ClipRect := cnstFRectZero;
end;

function TsgCommonExportParams.IsDestinationFile: Boolean;
begin
  Result := Destination.IsFile;
end;

function TsgCommonExportParams.IsSupportVersionOnFormat: Boolean;
begin
  Result := True;
  if Format in [efDxf, efDwg] then
    Result := CADExportParams.Version in cnstCADVersionsSupported[Format = efDwg];
end;

function TsgCommonExportParams.IsXmlParams: Boolean;
begin
  Result := FXmlParams;
end;

function TsgCommonExportParams.Save(AObject: TPersistent): Boolean;
begin
  Result := Destination.Save(AObject);
end;

procedure TsgCommonExportParams.SetCadVersion(const AStr: string;
  const AOnlyDigits: Boolean = False);
begin
  CADExportParams.Version := sgStringToDWGVersion(AStr, DefExportParams.Version);
  CheckVersionOnFormat;
end;

procedure TsgCommonExportParams.SetDefaultParams;
begin
  FClipBoardFormat := 0;
  SetExportParamsDefault(CADExportParams, RasterExportParams);
end;

procedure TsgCommonExportParams.SetDestination(const Value: TsgStorage);
begin
  if FDestination <> Value then
  begin
    if Assigned(FDestination) then
    begin
      if FDestinationCookie > 0 then
        FDestination.RemoveListener(FDestinationCookie)
      else
        FDestination.RemoveListener(StorageDestroy);
      FDestinationCookie := 0;
      if not FDestination.Connected then
        FreeAndNil(FDestination)
      else
      begin
        if IsEqualEvent(TsgStorageAccess(FDestination).OnBeforeSaveToStream, DoBeforeSaveToStream) then
          TsgStorageAccess(FDestination).OnBeforeSaveToStream := nil;
        if IsEqualEvent(TsgStorageAccess(FDestination).OnAfterSaveToStream, DoAfterSaveToStream) then
          TsgStorageAccess(FDestination).OnAfterSaveToStream := nil;
      end;
    end;
    FDestination := Value;
    if Assigned(FDestination) then
    begin
      FDestinationCookie := FDestination.AddListener(StorageDestroy);
      TsgStorageAccess(FDestination).OnBeforeSaveToStream := DoBeforeSaveToStream;
      TsgStorageAccess(FDestination).OnAfterSaveToStream := DoAfterSaveToStream;
    end;
  end;
end;

class procedure TsgCommonExportParams.SetExportParamsDefault(
  var ACADExportParams: TsgExportParams;
  var ARasterExportParams: TsgRastrExportParams);
begin
  ACADExportParams := DefExportParams;
  ARasterExportParams := DefRastrExportParams;
  ARasterExportParams.Quality := iJPEGQuality;
end;

procedure TsgCommonExportParams.SetSaveFileName(const Value: string);
begin
  Destination.FileName := Value;
end;

procedure TsgCommonExportParams.StorageDestroy(Sender: TObject);
begin
  if Sender = FDestination then
  begin
    FDestination := nil;
    FDestinationCookie := 0;
  end;
end;

function TsgCommonExportParams.TryCreateMultipageImage(
  const AExportParamsNode: TsgNode; out ANewImages{array of object}): TObject;
var
  I: Integer;
  vSources, vFileName: TsgNodeSample;
  vParams: TsgCommonExportParams;
  vPicture: TPicture;
  vCADImage: TsgCADImage;
  vExportParamsList: array of TsgExportParams;
  vNewImages: array of TsgCADImage;
begin
  Result := nil;
  if Assigned(AExportParamsNode) then
  begin
    vSources := AExportParamsNode.GetChildByName('sources');
    if Assigned(vSources) and (vSources.ChildNodesCount > 0) then
    begin
      for I := 0 to vSources.ChildNodesCount - 1 do
      begin
        vParams := TsgCommonExportParams.Create;
        try
          if vParams.FromNode(TsgNode(vSources.ChildNodes[I]), nil, Self) then
          begin
            vFileName := vSources.ChildNodes[I].GetAttributeByName(cnstXMLFileName);
            if Assigned(vFileName) then
            begin
              vPicture := TPicture.Create;
              try
                try
                  vPicture.LoadFromFile(vFileName.Value);
                  if vPicture.Graphic is TsgCADImage then
                  begin
                    vCADImage := TsgCADImageClass(vPicture.Graphic.ClassType).Create;
                    vCADImage.Assign(vPicture.Graphic);
                    SetLength(vExportParamsList, Length(vExportParamsList) + 1);
                    vExportParamsList[High(vExportParamsList)] := vParams.CADExportParams;
                    SetLength(vNewImages, Length(vNewImages) + 1);
                    vNewImages[High(vNewImages)] := vCADImage;
                    vCADImage.IsWithoutBorder := True;
                  end;
                except
                  raise;
                end;
              finally
                vPicture.Free;
              end;
            end;
          end;
        finally
          vParams.Free;
        end;
      end;
      if Length(vNewImages) > 0 then
      begin
        vCADImage := TsgMultipageExportImage.CreateMultipage(vNewImages, vExportParamsList);
        vCADImage.IsWithoutBorder := True;
        if vCADImage.LayoutsCount > 0 then
          vCADImage.CurrentLayout := vCADImage.Layouts[0];
        Result := vCADImage;
        Pointer(ANewImages) := vNewImages;
        Pointer(vNewImages) := nil;
      end;
    end;
  end;
end;

{ TsgDXFMultipageLayouts }

procedure TsgDXFMultipageLayouts.ListNotify(const Obj: TObject; Action: TListNotification);
begin
  // do nothing
end;

{ TsgMultipageSimpleExport }

constructor TsgMultipageSimpleExport.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  if FCADImage is TsgMultipageExportImage then
  begin
    OnPageStart := TsgMultipageExportImage(FCADImage).PageStart;
    OnPageEnd := TsgMultipageExportImage(FCADImage).PageEnd;
  end;
end;

constructor TsgMultipageSimpleExport.CreateMultipage(
  const ACADImages: array of TsgCADImage;
  const AExportParamsList: array of TsgExportParams);
var
  vMultipageExportImage: TsgCADImage;
begin
  vMultipageExportImage := TsgMultipageExportImage.CreateMultipage(ACADImages, AExportParamsList);
  try
    Create(vMultipageExportImage);
  finally
    if FCADImage <> vMultipageExportImage then
      vMultipageExportImage.Free;
  end;
end;

{ TsgMultipageExportImage }

procedure TsgMultipageExportImage.Assign(Source: TPersistent);
var
  I: Integer;
  vParam: TsgMultipageExportParams;
begin
  inherited Assign(Source);
  if Source is TsgMultipageExportImage then
  begin
    ClearObjects(FPagesExportParams);
    for I := 0 to TsgMultipageExportImage(Source).FPagesExportParams.Count - 1 do
    begin
      vParam := TsgMultipageExportParams.Create;
      vParam.ExportParams := TsgMultipageExportParams(TsgMultipageExportImage(Source).FPagesExportParams.Objects[I]).ExportParams;
      FPagesExportParams.AddObject(TsgMultipageExportImage(Source).PagesExportParams[I], vParam);
    end;
  end;
end;

constructor TsgMultipageExportImage.Create;
var
  vLayouts: TsgDXFEntity;
begin
  inherited Create;
  Converter.InitializeSectionsBegin;
  Converter.InitializeSectionsEnd;
  vLayouts := Converter.Sections[csLayouts];
  Converter.Sections[csObjects].RemoveEntity(vLayouts);
  vLayouts.Free;
  vLayouts := Converter.NewNamedEntity(Converter.Sections[csObjects], TsgDXFMultipageLayouts, cnstObjectsLAYOUTS);
  Converter.Loads(vLayouts);
  FPagesExportParams := TStringList.Create;
  FPagesExportParams.Sorted := True;
end;

function TsgMultipageExportImage.CreateConverter: TsgDXFConverter;
begin
  Result := TsgDXFMultipageConverter.CreateEx(GetConverterParams);
end;

class function TsgMultipageExportImage.CreateMultipage(
  const ACADImages: array of TsgCADImage;
  const AExportParamsList: array of TsgExportParams): TsgCADImage;
begin
  Result := TsgCADImageClass(Self).Create;
  TsgMultipageExportImage(Result).InitializeMultipage(ACADImages, AExportParamsList);
end;

destructor TsgMultipageExportImage.Destroy;
begin
  ClearObjects(FPagesExportParams);
  FPagesExportParams.Free;
  inherited Destroy;
end;

procedure TsgMultipageExportImage.InitializeMultipage(
  const ACADImages: array of TsgCADImage;
  const AExportParamsList: array of TsgExportParams);

  function _GetLayoutsExportCount(ACADImage: TsgCADImage;
    const AExportParams: TsgExportParams; ALayouts: TsgDXFEntity): Integer;
  var
    J: Integer;
    vSkipExportLayout: Boolean;
    vStrings: TsgStringList;
    vLayoutExportMode: TsgLayoutExportMode;
  begin
    Result := 0;
    if ACADImage.ClippingRect <> nil then
    begin
      Inc(Result);
      ALayouts.AddEntity(ACADImage.CurrentLayout);
    end
    else
    begin
      vStrings := nil;
      try
        vLayoutExportMode := TsgLayoutExportMode(AExportParams.LayoutExportMode);
        if vLayoutExportMode in [lemLayoutsByIndexes, lemLayoutByName] then
        begin
          vStrings := TsgStringList.Create;
          try
            TsgStringList(vStrings).LineBreak := TsgSimpleExport.LayoutNameSeparator;
            vStrings.Text := AExportParams.LayoutNameExportMode;
            sgFunction.DeleteEmptyStrings(vStrings);
            if vStrings.Count > 0 then
            begin
              vStrings.Sorted := True;
              vStrings.CaseSensitive := False;
              if vLayoutExportMode = lemLayoutsByIndexes then
                for J := 0 to vStrings.Count - 1 do
                  vStrings.Objects[J] := TObject(StrToIntDef(vStrings[J], -1));
            end;
          except
            FreeAndNil(vStrings);
          end;
        end;
        for J := 0 to ACADImage.LayoutsCount - 1 do
        begin
          case vLayoutExportMode of
            lemModel:          vSkipExportLayout := not ACADImage.Layouts[J].IsModel;
            lemAllLayouts:     vSkipExportLayout := ACADImage.Layouts[J].IsModel;
            lemAllPaperSpaces: vSkipExportLayout := False;
            lemCurrentLayout:  vSkipExportLayout := ACADImage.CurrentLayout <> ACADImage.Layouts[J];
            lemLayoutByName:
              begin
                if Assigned(vStrings) then
                  vSkipExportLayout := vStrings.IndexOf(ACADImage.Layouts[J].Name) < 0
                else
                  vSkipExportLayout := CompareText(ACADImage.Layouts[J].Name, AExportParams.LayoutNameExportMode) <> 0;
              end;
            lemLayoutsByIndexes:
              begin
                if Assigned(vStrings) then
                  vSkipExportLayout := vStrings.IndexOfObject(Pointer(J)) < 0
                else
                  vSkipExportLayout := True;
              end;
          else
            vSkipExportLayout := True;
          end;
          if not vSkipExportLayout then
          begin
            Inc(Result);
            ALayouts.AddEntity(ACADImage.Layouts[J]);
          end;
        end;
//        if (not IsFormatMultiPages) and (Result > 1) then
//          Result := 1;
      finally
        vStrings.Free;
      end;
    end;
  end;

var
  I: Integer;
  vParam: TsgMultipageExportParams;
begin
  ClearObjects(FPagesExportParams);
  for I := Low(AExportParamsList) to High(AExportParamsList) do
  begin
    vParam := TsgMultipageExportParams.Create;
    vParam.ExportParams := AExportParamsList[I];
    FPagesExportParams.AddObject(ACADImages[I].Converter.FileName, vParam);
  end;
  for I := Low(ACADImages) to High(ACADImages) do
    _GetLayoutsExportCount(ACADImages[I], AExportParamsList[I],
    Converter.Sections[csLayouts]);
end;

procedure TsgMultipageExportImage.PageEnd(Sender: TObject);
begin
  Converter.Sections[csLayouts] := FLayouts;
end;

procedure TsgMultipageExportImage.PageStart(Sender: TObject);
var
  I: Integer;
begin
  FLayouts := Converter.Sections[csLayouts];
  Converter.Sections[csLayouts] := CurrentLayout.Converter.Sections[csLayouts];
  if Sender is TsgCADExport then
  begin
    I := FPagesExportParams.IndexOf(CurrentLayout.Converter.FileName);
    if I >= 0 then
      TsgCADExport(Sender).ApplyParams(TsgMultipageExportParams(FPagesExportParams.Objects[I]).ExportParams);
  end;
end;

{ TsgDXFMultipageConverter }

procedure TsgDXFMultipageConverter.ClearBegin;
begin
  inherited ClearBegin;
  Sections[csLayouts].Clear;//all layouts is external. clear only, do not dispose!!!
end;

{ TsgObjExportParams }

constructor TsgObjExportParams.Create;
begin
  inherited Create;
  InitDefValues(Fields);
end;

class procedure TsgObjExportParams.InitDefValues(var AFields: TsgExportParams);
begin
  AFields.Version := cnstSaveCADVersionDefault;
  AFields.PageWidth := 210;       // mm
  AFields.PageHeight := 297;      // mm
  AFields.Margin := 5;            // mm
  AFields.DrawMode := 0;          // color
  AFields.BackgroundColor := clWhite;
  AFields.DefaultColor := clBlack;
  AFields.NullWidth := 0.3;       // mm
  AFields.LineWeightScale := 1;
  AFields.SaveLineWeight := True;
  AFields.UseHighQuality := False;
  AFields.ExportTexts := True;
  AFields.SHXAnnotations := True;
  AFields.UseOutlines := True;
  AFields.UseSHXFonts := -1;
  AFields.TrueTypeOptimization := False;
  AFields.LayoutExportMode := 3;  //TsgLayoutExportMode.lemAllPaperSpaces
  AFields.LayoutNameExportMode := '';
  AFields.Title := '';
  AFields.Author := cnstCompanyCST;
  AFields.Subjct := '';
  AFields.Keywords := '';
  AFields.LineWeights := '';
  AFields.SizeAsExtents := False;
  AFields.MMInCad := 1;
  AFields.UnitsType := iuMillimeters;
  AFields.UseVectorLineWeight := False;
  AFields.HPGLXScale := 40;
  AFields.IsConvertImageToOLE := DefIsConvertImageToOLE;
  AFields.FillingEntity := True;
  AFields.OffsetPoint := MakeF2DPoint(0, 0);
  AFields.AlternateBlack := True;
  AFields.UnitSize := 1.0;
  AFields.Use01MM := True;
  AFields.LayersMode := 1;
  AFields.Scale := 0;
  AFields.ACISColorFlags := 0;
  AFields.CreateMtl := True;
{$IFDEF SG_VECTORIZATION}
    AFields.VectorizationMode := 2;
{$ENDIF}
  //For GCode Export
  AFields.ArcToLines := False;
  AFields.CodeOptimize := True;
  AFields.MachineTypeID := 0;        // need cast to TsgGCodeMachineTypeID from Unit CADtoGCode
  AFields.PassesDirectionID := 0;    // need cast to TsgGPassesDirection from Unit CADtoGCode
  AFields.PrecisionDigits := 3;
  AFields.PrecisionFactor := 1;
  AFields.FeedOnZ := 150;            // mm/min
  AFields.FeedOnXY := 450;           // mm/min
  AFields.SpindleSpeed := 3000;      // rpm
  AFields.DepthOnZ := -2;            // mm
  AFields.DepthPass := 2;            // mm
  AFields.NumbOfPasses := 1;
  AFields.DepartureOnZ := 5;         // mm
  AFields.WorkpieceZeroPointID := 0; // need cast to TsgGCodeZeroPointID from Unit CADtoGCode
  AFields.ZeroPointOffsetX := 0;
  AFields.ZeroPointOffsetY := 0;
  AFields.LaserOnCommand := 'M3';
  AFields.LaserOffCommand := 'M5';
  AFields.UseLaserPower := False;
  AFields.LaserPower := 128;         // 0-255
  AFields.Delay := 0;                // ms
  AFields.MachineUnitsID := 0;       //need cast to TsgGUnits from Unit CADtoGCode
  AFields.DrawingUnitsID := 0;       //need cast to TsgGUnits from Unit CADtoGCode
  AFields.AddLabelOfProgam := False;
  AFields.LabelOfProgram := '001';
  AFields.AddNumbering := False;
  AFields.ShowContourName := True;
  AFields.ShowPercent := True;
  AFields.StartNumber := 5;
  AFields.StepOfNumbering := 5;
  AFields.Tools := '1/2/10';
  AFields.GcodeExtID := 0;           //need cst to TsgGcodeExts
  AFields.TrailingZeros := False;
  AFields.ConvertOnlyVisibleLayers := True;
  // for import from metafile
  AFields.ExtData := nil;
  AFields.XMLParamsExtended := '';
  AFields.TTFMode := 0;
  AFields.PdfBitmapMode := bemNone;
{$IFDEF SG_BTI}
  AFields.UseFillStyleIndex := False;
{$ENDIF}
  AFields.SourceGraphicWidth := -1;
  AFields.SourceGraphicHeight := -1;
  AFields.LineTypeMode := 0;
  AFields.IgnoreLayerNoPlotting := False;
end;

initialization
{$IFDEF SG_FIREMONKEY}
  DefExportParamsObj := TsgObjExportParams.Create;
{$ELSE}
  TsgObjExportParams.InitDefValues(DefExportParams);
{$ENDIF}

finalization
{$IFDEF SG_FIREMONKEY}
  FreeAndNil(DefExportParamsObj);
{$ENDIF}

end.

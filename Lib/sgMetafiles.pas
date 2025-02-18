unit sgMetafiles;

{$INCLUDE SGDXF.inc}

//{$DEFINE SG_METAFILE_DEBUG}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$IFDEF SGFPC}
  JwaWinGDI,
{$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, Types, {$IFDEF SG_NON_WIN_PLATFORM} cwstring,{$ENDIF}
{$ENDIF}
  SysUtils, Classes, Graphics, Math, CADImage, DXFConv, sgLists, sgImportFunctions,
  sgLines, Contnrs, sgFunction, sgConsts, TTF, CADExport, sgTextRectList,
  sgComparer, SHX, sgSHXResolver {$IFDEF SGDEL_6}, StrUtils  {$ENDIF}
{$IFDEF SGDEL_XE2}
  , Types
{$ENDIF};

const
  cnstResolveSHX: Boolean = False;
  cnstTestTextLayers: Boolean = True;
  cnstExtractTexts: Boolean = True;
  cnstWhiteToGRBWhiteColor: Boolean = True;
  cnstEnableFitProportional: Boolean = False;
  cnstGlobalLTScaleRatio: Double = 0.01;
  cnstDefaultKoefPenSize: Single = 1;
  cnstCheckTransparent: Boolean = True;
  cnstUseMetafileRatio: Boolean = True;

{$IFDEF SG_VECTORIZATION}
const
  cnstMakeFixSizeImage: Boolean = True;
{$ENDIF}
const
  cnstMaxRecordsInScan = 100;

type
  TsgConvertMetafileToCad = class;
  TsgRegionClip = class;
  TsgRegion = class;
  TypeGDIObject = (tgStandart,tgPen,tgExtPen,tgBrush,tgPalette,tgFont);
  TsgArcType = (atCircle, atArc, atEllipse);
  PsgSmallPoint = ^TSmallPoint;

  PsgLogPen = ^TsgLogPen;
  TsgLogPen = record
    lp: TLogPen;
    lb: TLogBrush;
    Styles: array of DWORD;
  end;

  PGDIObject = ^TGDIObject;
  TGDIObject = record
    index: Integer;
    tpObject: TypeGDIObject;
    iHandle: Integer;
    PData: Pointer;
  end;

  TDataRegion = record
    rct: TRect;
    SizeRgn: DWORD;
    RgnData: PRgnData;
    hBrush: TsgNativeUInt;
    szStroke: TSize;
  end;

  TParamsDraw = record
    ArcDirection: Byte;
    BeginViewPortCoord: TPoint;
    BeginWindowCoord: TPoint;
    BkMode: Byte;
    BkColor: TColor;
    MapMode: Byte;
    PolyFillMode: Integer;
    Rop: Integer;
    sBltMode: Integer;
    SizeViewPort: TSize;
    SizeWindow: TSize;
    StrenchBltMode: Integer;
    TextAlign: Byte;
    TextColor: TColor;
  end;

  TsgParamsPath = class
  private
    FBeginPath: Boolean;
    FClosingLine: PsgLine;
    FConverter: TsgConvertMetafileToCad;
    FCountHatches: array of Integer;
    FEndPathLine: TsgLine;
    FHatchInPath: TsgObjectList;
    FIndexFirstFigure: Integer;
    FIsInitRect: Boolean;
    FListFiguresInPath: TsgObjectList;
    FPathFRect: TFRect;
    FPathRect: TRect;
    FPointCount: Integer;
    procedure GetCorrectIndexes(var AStart,AStop: Integer);
    class function GetEntityPoints(const AEntity: TsgDXFEntity; APoints: TFPointList): Boolean;
    function GetPointTypeFigure(AEnt: TsgDXFEntity; AFirst: Boolean = True): Boolean;
    procedure SetBeginPath(AValue: Boolean);
    function IsClosablePath(const AStart,AStop: Integer): Boolean;
  public
    constructor Create(AConverter: TsgConvertMetafileToCad);
    destructor Destroy; override;
    function ActivePath: TsgObjectList;
    procedure Add(AEntity: TsgDXFEntity);
    procedure AddPoint(AIPoint: TPoint; AFPoint: TFPoint);
    procedure Assign(AParamsPath: TsgParamsPath);
    procedure Clear(AFreeEntities: Boolean);
    procedure ClearLists(AFreeEntities: Boolean);
    procedure ClearParams;
    procedure CloseFigure;
    function IsNeedColse: Boolean;
    procedure Remove(AEntity: TsgDXFEntity);
    procedure SaveStartFigure;
    procedure StartPath;
    procedure StopPath;
    property BeginPath: Boolean read FBeginPath write SetBeginPath;
    property ClosingLine: PsgLine read FClosingLine;
    property HatchInPath: TsgObjectList read FHatchInPath;
    property IsInitRect: Boolean read FIsInitRect;
    property ListFiguresInPath: TsgObjectList read FListFiguresInPath;
    property PathFRect: TFRect read FPathFRect;
    property PathRect: TRect read FPathRect;
    property PointCount: Integer read FPointCount;
  end;

  PdxfExportData = ^TdxfExportData;
  TdxfExportData = packed record
    Tag: Word;                 // classindex, DXF_LINE, DXF_SOLID etc.
    Count: Word;               // number of child entities
    Flags: Byte;               // flags byte
    Style: Byte;               // style (pen, brush...)
    SelfType: Byte;            // SelfType defines type of this entity
    Color: TsgColorCAD;        // color of entity CAD style
    ColorCAD: TsgColorCAD;     // color for CurvePolygon
    RGBColor: TColor;
    HatchStyle: Integer;
    Boundary: TsgObjectList;
    FillStyle: TsgFillStyle;
    Closed: Boolean;
    Points: TFPointList;
    Ratio: Double;
    Radius: Double;
    StartAngle: Double;
    EndAngle: Double;
    Thickness: Single;         // thickness of lines
    Rotation: Single;          // rotation angle (TEXT, INSERT)
    LineType: TsgDXFLineType;  // line type
    Text: string;              // text string (TEXT), block name (INSERT)
    Point: TFPoint;            // coordinates of the first point
    Point1: TFPoint;           // coordinates of the second point
    Point2: TFPoint;           // coordinates of the third point
    Point3: TFPoint;           // coordinates of the fourth point
  end;

  TsgDCStackItem = class
  private
    FClipRgn: TsgRegion;
    FClipPath: TFPointList;
    FDefaultBrush: TLogBrush;
    FDefaultPen: TsgLogPen;
    FDefaultFont: TLogFontW;
    FKoefMapMode: Single;
    FMatrix: TFMatrix;
    FParamsDraw: TParamsDraw;
    FParamsDrawBrush: TGDIObject;
    FParamsDrawPen: TGDIObject;
    FParamsPath: TsgParamsPath;
    FParamsText: TGDIObject;
    FPenPos: TPoint;
    procedure SetClipRgn(AValue: TsgRegion);
    procedure SetClipPath(AValue: TFPointList);
    procedure SetParamsPath(AValue: TsgParamsPath);
  public
    constructor Create(AMetafile: TsgConvertMetafileToCad);
    destructor Destroy; override;
    property ClipRgn: TsgRegion read FClipRgn write SetClipRgn;
    property ClipPath: TFPointList read FClipPath write SetClipPath;
    property DefaultBrush: TLogBrush read FDefaultBrush write FDefaultBrush;
    property DefaultPen: TsgLogPen read FDefaultPen write FDefaultPen;
    property DefaultFont: TLogFontW read FDefaultFont write FDefaultFont;
    property KoefMapMode: Single read FKoefMapMode write FKoefMapMode;
    property Matrix: TFMatrix read FMatrix write FMatrix;
    property ParamsDraw: TParamsDraw read FParamsDraw write FParamsDraw;
    property ParamsDrawBrush: TGDIObject read FParamsDrawBrush write FParamsDrawBrush;
    property ParamsDrawPen: TGDIObject read FParamsDrawPen write FParamsDrawPen;
    property ParamsPath: TsgParamsPath read FParamsPath write SetParamsPath;
    property ParamsText: TGDIObject read FParamsText write FParamsText;
    property PenPos: TPoint read FPenPos write FPenPos;
  end;

  TsgRegionClip = class
  private
    FClipMode: Integer;
    FFilter: TsgCADSpatialFilter;
    FInsert: TsgDXFInsert;
    FIsOwnedBlock: Boolean;
    FIsWorkedClip: Boolean;
    FMF: TsgConvertMetafileToCad;
    FOwner: TsgRegionClip;
    procedure ClearClip;
    procedure CreateClip(const AClipMode: Integer; const AParentClip: TsgRegionClip;
      const AMF: TsgConvertMetafileToCad; const ABlock: TsgDXFBlock);
    function GetBlock: TsgDXFBlock;
    function GetBounds: TFPointList;
    function GetFilterBox: TFRect;
    procedure SetBlock(const AValue: TsgDXFBlock);
  public
    constructor Create(const AClipPath: TFPointList; const AClipMode: Integer;
      const AParentClip: TsgRegionClip; const AMF: TsgConvertMetafileToCad;
      const ABlock: TsgDXFBlock); overload;
    constructor Create(const AClipRect: TFRect; const AClipMode: Integer;
      const AParentClip: TsgRegionClip; const AMF: TsgConvertMetafileToCad;
      const ABlock: TsgDXFBlock); overload;
    constructor Create(const AMF: TsgConvertMetafileToCad); overload;
    destructor Destroy; override;
    procedure Assign(AClip: TsgRegionClip);
    function ExternalBlock: TsgDXFBlock;
    procedure Load(AConverter: TsgDXFConverter);
    procedure MoveToTop;
    function OwnedBlock: TsgDXFEntity;
    property Block: TsgDXFBlock read GetBlock write SetBlock;
    property Bounds: TFPointList read GetBounds;
    property ClipMode: Integer read FClipMode;
    property Filter: TsgCADSpatialFilter read FFilter;
    property FilterBox: TFRect read GetFilterBox;
    property Insert: TsgDXFInsert read FInsert;
    property IsOwnedBlock: Boolean read FIsOwnedBlock;
    property IsWorkedClip: Boolean read FIsWorkedClip;
    property MF: TsgConvertMetafileToCad read FMF;
    property Owner: TsgRegionClip read FOwner;
  end;

  TsgRegion = class
  private
    FClip: TsgRegionClip;
    FIsClipping: Boolean;
    FIsClipSet: Boolean;
    FLoadStack: TList;
    FParentMetaFile: TsgConvertMetafileToCad;
    FWorkset: TList;
    procedure CorrectEmptyClips;
    function GetBoundsRect: TFRect;
    function GetClip(const AClipRect: TFRect;
      var AIsEqual: Boolean): TsgRegionClip;
    function GetCurClip: TsgRegionClip;
    function GetCurClipMode: Integer;
    function GetCurInsert: TsgDXFInsert;
    function GetOwnedInsert: TsgDXFInsert;
    procedure LoadInternal(AOwnedClip: TsgRegionClip);
    function MakeNewClip(const AClipPath: TFPointList; const AClipMode: Integer;
      const AParentInsert: TsgRegionClip; const ABlock: TsgDXFBlock): TsgRegionClip; overload;
    function MakeNewClip(const AClipRect: TFRect; const AClipMode: Integer;
      const AParentInsert: TsgRegionClip; const ABlock: TsgDXFBlock): TsgRegionClip; overload;
    procedure ResetAndAddToWorkset(AClip: TsgRegionClip);
    procedure SetCurClip(AValue: TsgRegionClip);
    procedure SetIsClipping(AValue: Boolean);
    //procedure SetTopClip(AClip: TsgRegionClip);
  public
    constructor Create(AParentMetaFile: TsgConvertMetafileToCad);
    destructor Destroy; override;
    procedure Assign(ASource: TsgRegion);
    procedure InitFromData(APRgnData: PRgnData; AClipMode: Integer);
    procedure InitFromPath(const APath: TFPointList; const AClipMode: Integer;
      const ABlock: TsgDXFBlock);
    procedure InitFromRect(ARect: TRect; AClipMode: Integer; AOther: TsgRegion);
    function IsRectNotBounds(const ARect: TRect): Boolean; overload;
    function IsRectNotBounds(const ARect: TFRect): Boolean; overload;
    function IsValidClip: Boolean;
    procedure Clear(const AIsAll: Boolean = False);
    procedure Load;
    procedure MakeClip(const AClipPath: TFPointList; const AClipMode: Integer;
      const ABlock: TsgDXFBlock); overload;
    procedure MakeClip(const AClipRect: TFRect; const AClipMode: Integer;
      const ABlock: TsgDXFBlock); overload;
    function MakeFRectForRect(const ARect: TRect): TFRect;
    procedure MarkIsWorked;
    property BoundsRect: TFRect read GetBoundsRect;
    property CurClip: TsgRegionClip read GetCurClip write SetCurClip;
    property CurClipMode: Integer read GetCurClipMode;
    property CurInsert: TsgDXFInsert read GetCurInsert;
    property OwnedInsert: TsgDXFInsert read GetOwnedInsert;
    property IsClipping: Boolean read FIsClipping write SetIsClipping;
  end;

  TsgMetafileTextListItem = class
  private
    FRect: TFRect;
    FText: string;
    FTextObject: TsgDXFText;
  public
    constructor Create(ARect: TFRect; const AText: string; AObject: TsgDXFText);
    function IsEqual(ARect: TFRect; const AText: string): Boolean;
    property Rect: TFRect read FRect;
    property Text: string read FText;
    property TextObject: TsgDXFText read FTextObject;
  end;

  TsgMetafileTextList = class(TList)
  private
    FImage: TsgCADImage;
  public
    constructor Create(AImage: TsgCADImage);
    destructor Destroy; override;
    procedure AddItem(AImage: TsgCADImage; ARect: TFRect; const AText: string;
      AObject: TsgDXFText);
    function Find(AImage: TsgCADImage; ARect: TFRect;
      const AText: string): TsgMetafileTextListItem;
    property Image: TsgCADImage read FImage;
  end;

  TsgDXFEntityTag = class
  private
    FEntity: TsgDXFEntity;
    FTag: Integer;
  public
    constructor Create(AEntity: TsgDXFEntity; ATag: Integer = 0);
    property Entity: TsgDXFEntity read FEntity;
    property Tag: Integer read FTag write FTag;
  end;

  TsgConvertMetafileToCad = class
  private
    FBeginPath: Boolean;
    FBounds: TFRect;
    FCADImage: TsgCADImage;
    FLayer: TsgDXFLayer;
    FCommandIndex: Integer;  // for debug
    FCompleted: Byte;
    FCorrectonModelName: Boolean;
    FData: TdxfExportData;
    FDC: HDC;
    FDefaultBkColor: TColor;
    FDefaultBrush: TLogBrush;
    FDefaultPen: TsgLogPen;
    FDefaultFont: TLogFontW;
    FDefaultStyle: TsgDXFStyle;
    FEnableFitProportional: Boolean;
    FExtractedTexts: TsgMetafileTextList;
    FFillingEntity: Boolean;
    FAlternateBlack: Boolean;
    FOffsetPoint: TF2DPoint;
    FOffsetPointInternal: TF2DPoint;
    FOwner: TsgObjectList;
    FOwnerBox: TFRect;
    FEntitiesBox: TFRect;
    FDCStack: TObjectList;
    FDrawedPath: Boolean;
    FPenPos: TPoint;
    FLastSubstringColor: TsgColorCAD;
    FLastSubstringClip: TsgRegionClip;
    FLastSubstringItem: TsgTextRectListItem;
    FLastSubstringRect: TRect;
    FListTextContour: TsgObjectList;
    FListTextFigures: TsgObjectList;
    FMatrix: TFMatrix;
    FMetafile: TMetafile;
    FMetafileBox: TRect;
    FMetaFileHeader: TEnhMetaHeader;
    FOnProgress: TProgressEvent;
    FParamsDraw: TParamsDraw;
    FParamsDrawBrush: TGDIObject;
    FParamsDrawPen: TGDIObject;
    FParamsPath: TsgParamsPath;
    FParamsText: TGDIObject;
    FSHXResolver: TsgSHXResolver;
    FTextsList: IsgTextRectFinder;
    FListBlocks: TsgObjectList;
    FListEnts: TsgObjectList;
    FKoefMapMode: Single;
    FKoefPenSize: Single;
    FKoefScales: TF2DPoint;
    FFileName: String;
    FICMMode: Integer;
    FIsImageInClip: Boolean;
    FEXForm: TXForm;
    FCurrentEntity: TsgDxfEntity;
    FClipRgn: TsgRegion;
    FRegionRect: TRect;
{$IFDEF SG_METAFILE_DEBUG}
    FLogMetafile: TStringList;
{$ENDIF}
    FMapGDIObject: TList;
    FMetafileXRatio: Double;
    FGDIObjectsSelect: TList;
    FScale: Double;
{$IFDEF SG_VECTORIZATION}
    FVectorizationMode: TsgPDFVectorizationMode;
{$ENDIF}
    FUnitSize: Double;
    FUse01MM: Boolean;
    FUseBackground: Boolean;
    FUseMetafileRatio: Boolean;
    procedure AddContour(AList: TsgObjectList; ACADImage: TsgCADImage = nil);
    procedure AddImageEnt(APBitmapInfo: PBitmapInfo; AData: Pointer;
      APos,ASize,AEntSize: TPoint; ABkColor: TColor; AUsage: Integer;
      ARop: DWORD);
    procedure AddSHXTextEnt;
    function AddTextEnt(ACADImage: TsgCADImage; const AText: string; ARect: TRect;
      AFontFace: TsgFontFace; AColor: TsgColorCAD; AIsResize: Boolean;
      AClip: TObject = nil): TsgDXFEntity; overload;
    function AddTextEnt(ACADImage: TsgCADImage; const AText: string; const ARect: TFRect;
      const AFontFace: TsgFontFace; const AColor: TsgColorCAD; const AIsResize: Boolean;
      AClip: TObject = nil): TsgDXFEntity; overload;
    function ClipPoint2DByRect(const ARect: TRect;
      const AP: TF2DPoint): TF2DPoint;
    function CreateAndAddStyle(const ACADImage: TsgCADImage;
      const AName: string): TsgDXFStyle;
    function CreateBrush: THandle;
    function CreatePen: THandle;
    function ExtractText(const AList: TsgObjectList; const AIsContour: Boolean): Boolean;
    procedure GDIObjectAdd(AObject: PGDIObject);
    procedure GDIObjectDelete(AHandle: Integer);
    function GenerateArc(const AStartPt,AEndPt: TPoint; const ARect: TRect): TdxfExportData;
    procedure GetBlocksAndInserts(const AImage: TsgCADImage;
      var ABlock,AOwner: TsgDXFBlock; var ACurInsert: TsgDXFInsert);
    procedure DrawPoint(const ACoord: TPoint; AColor: TColor);
    procedure DoHeader(P: PEnhMetaHeader);
    //procedure DoSetICMMode(P: PEnhSetICMMode);
    //procedure DoExtTextOutW(P: PEnhTextOutW);
    procedure InitParamsOfContextDeviceValuesDefault;

    function GetStandartGDIObject(const AIHandle: DWORD): DWORD;
    function DoArc(P: PEMRArc; IsTo: Boolean = False;
      ATryToFill: Boolean = False; AHatch: Boolean = False): TsgDXFEntity;
    procedure DoChord(P: PEMRChord);
    procedure DoPie(P: PEMRPie);
    procedure DoGDIComment(P: PEMRGDIComment);
    procedure DoSaveDC(P: PEMRSaveDC);
    procedure DoCreatePen(P: PEMRCreatePen);
    procedure DoEllipse(P: PEMREllipse);
    procedure DoMoveTo(P: PEMRLineTo);
    function AddLine(const APt1,APt2: TFPoint; ATryToFill: Boolean = False;
      AHatch: Boolean = False): TsgDXFEntity;
    procedure AddRectangle(const ARect: TFRect; AIsSolid: Boolean = False);
    function CreateRectangle(const ARect: TRect; AColor: TColor;
      AIsSolid: Boolean = False): TsgDXFEntity; overload;
    function CreateRectangle(const ARect: TFRect; AColor: TColor;
      AIsSolid: Boolean = False; ATryToFill: Boolean = True;
      AHatch: Boolean = False): TsgDXFEntity; overload;
    procedure MyLineTo(const APt1,APt2: TPoint; IsTo: Boolean = True); overload;
    procedure DoLineTo(P: PEMRLineTo);
    procedure DoPolylineAnyType(P: Pointer;
      Is16: Boolean = False; IsTo: Boolean = False; AClosed: Boolean = False);
    procedure DoPolyPolylineAnyType(P: Pointer;
      Is16: Boolean = False; AClosed: Boolean = False);
    //procedure InsideRegion(const ARect: TRect);
    procedure DoRectangle(P: PEMRRectangle);
    procedure DoPolyBezier(P: Pointer;
      Is16: Boolean = False; IsTo: Boolean = False);
    procedure DoBitBlt(P: PEMRBitBlt);
    procedure DoCreateBrushIndirect(P: PEMRCreateBrushIndirect);
    procedure DoStretchBlt(P: PEMRStretchblt);

    procedure DoSetExtEx(const AExtent: TSize; var ASaveTo: TSize);
    procedure DoSetWindowExtex(P: PEMRSetWindowExtex);
    procedure DoSetWindowOrGex(P: PEMRSetWindowOrGex);
    procedure DoSetViewPortExtex(P: PEMRSetViewPortExtex);
    procedure DoSetViewPortOrGex(P: PEMRSetViewPortOrGex) ;
    procedure DoSetBrushOrGex(P: PEMRSetBrushOrGex);
    //EMR_EOF: ;
    procedure DoSetPixelv(P: PEMRSetPixelv);
    procedure DoSetMapperFlags(P: PEMRSetMapperFlags);
    procedure DoSetMapMode(P: PEMRSetMapMode);
    procedure DoSetBkMode(P: PEMRSetBkMode);
    procedure DoSetPolyFillMode(P: PEMRSetPolyFillMode);
    procedure DoSetRop2(P: PEMRSetRop2);
    procedure DoSetStretchBltMode(P: PEMRSetStretchBltMode);
    procedure DoSetColorAdjustment(P: PEMRSetColorAdjustment);
    procedure DoSetTextAlign(P: PEMRSetTextAlign);
    procedure DoSetTextColor(P: PEMRSetTextColor);
    procedure DoSetBkColor(P: PEMRSetBkColor);
    procedure DoOffsetClipRgn(P: PEMROffsetClipRgn);
    procedure DoSetMetaRgn(P: PEMRSetMetaRgn);
    procedure DoExcludeClipRect(P: PEMRExcludeClipRect);
    procedure DoInterSectClipRect(P: PEMRInterSectClipRect) ;
    procedure DoScaleViewPortExtex(P: PEMRScaleViewPortExtex);
    procedure DoScaleWindowExtex(P: PEMRScaleWindowExtex);
    procedure DoRestoreDC(P: PEMRRestoreDC);
    procedure DoSetWorldTransform(P: PEMRSetWorldTransform);
    procedure DoModifyWorldTransform(P: PEMRModifyWorldTransform);
    procedure DoSelectObject(P: PEMRSelectObject);
    procedure DoDeleteObject(P: PEMRDeleteObject);

    procedure DoAngleArc(P: PEMRAngleArc);
    procedure DoRoundRect(P: PEMRRoundRect);
    procedure DoSelectPalette(P: PEMRSelectPalette);
    procedure DoCreatePalette(P: PEMRCreatePalette);
    procedure DoSetPaletteEntries(P: PEMRSetPaletteEntries);
    procedure DoResizePalette(P: PEMRResizePalette);
    procedure DoRealizePalette(P: PEMRRealizePalette);
    procedure DoExtFloodFill(P: PEMRExtFloodFill);
    procedure DoPolyDraw(P: PEMRPolyDraw);
    procedure DoPolyDraw16(P: PEMRPolyDraw16);
    procedure DoCreateDibPatternBrushPt(P: PEMRCreateDibPatternBrushPt);
    procedure DoCreateMonoBrush(P: PEMRCreateMonoBrush);
    procedure DoSetArcDirection(P: PEMRSetArcDirection);
    procedure DoSetMiterLimit(P: PEMRSetMiterLimit);
    procedure DoBeginPath(P: PEMRBeginPath);
    procedure DoEndPath(P: PEMREndPath);
    procedure DoCloseFigure(P: PEMRCloseFigure);
    procedure DoFillPath(P: PEMRFillPath);
    procedure DoStrokeAndFillPath(P: PEMRStrokeAndFillPath);
    procedure DoStrokePath(P: PEMRStrokePath;
      Contour: Boolean = True; Fill: Boolean = False);
    procedure DoFlattenPath(P: PEMRFlattenPath);
    procedure DoWidenPath(P: PEMRWidenPath);
    procedure DoSelectClipPath(P: PEMRSelectClipPath) ;
    procedure DoAbortPath(P: PEMRAbortPath);
    procedure DoFillRgn(P: PEMRFillRgn);
    procedure DoFrameRgn(P: PEMRFrameRgn);
    procedure DoInvertRgn(P: PEMRInvertRgn);
    procedure DoPaintRgn(P: PEMRPaintRgn);
    procedure DoRegion(var DataRegion: TDataRegion; TypeRgn: Byte = 0);
    procedure DoExtSelectClipRgn(P:PEMRExtSelectClipRgn);
    procedure DoMaskBlt(P: PEMRMaskBlt);  // ?
    procedure DoPlgBlt(P: PEMRPlgBlt);  // ?
    procedure DoSetDIBItstoDevice(P: PEMRSetDIBItstoDevice);  // ?
    procedure DoStretchDIBIts(P: PEMRStretchDIBIts);
    procedure DoExtCreareFontIndirectw(P: PEMRExtCreateFontIndirect);
    procedure DoExtTextOut(P: PEMRExtTextOut; const AText: WideString);
    procedure DoExtTextOutA(P: PEMRExtTextOut);
    procedure DoExtTextOutW(P: PEMRExtTextOut);

    procedure DoEOF(P: PEMREof);
    procedure DoExtCreatePen(P: PEMRExtCreatePen);
    procedure DoPolyTextOut(P: PEMRPolyTextOut);  // ?
    procedure DoSetICMMode(P: PEMRSetICMMode);
    procedure DoCreateColorSpace(P: PEMRCreateColorSpace);  // ?
    procedure DoSetColorSpace(P: PEMRSelectColorSpace);  // ?
    procedure DoDeleteColorSpace(P: PEMRDeleteColorSpace);  // ?
    procedure DoGLSRecord(P: PEMRGLSRecord);  // ?
    procedure DoGLSBoundedRecord(P: PEMRGLSBoundedRecord);  // ?
    procedure DoPixelFormat(P: PEMRPixelFormat);  // ?
    {procedure DoDrawEscape(P: PEMRDrawEscape);
    procedure DoExtEscape(P: PEMRExtEscape);}
    //procedure DoStartDoc(P: PEMRStartDoc);
    //procedure DoSmallTextOut(P: PEMRSmallTextOut);
    //procedure DoForceUfiMapping(P: PEMRForceUfiMapping);
    //procedure DoNamedEscape(P: PEMRNamedEscape);
    //procedure DoColorCorrectPalette(P: PEMRColorCorrectPalette);
    //procedure DoSetICMProfile(P: PEMRSetICMProfile);
    procedure DoAlphaBlend(P: Pointer);  // ?PEMRAlphaBlend
    //procedure DoAlphaDIBBlend(P: PEMRAlphaDIBBlend);
    procedure DoTransparentBlt(P: Pointer);  // ?PEMRTransparentBlt
    //procedure DoTransparentDIB(P: PEMRTransparentDIB);
    procedure DoGradientFill(P: Pointer);  // ?PEMGradientFill
    //procedure DoSetLinkedUfIs(P: PEMRSetLinkedUfIs);
    //procedure DoSetTextJustification(P: PEMRSetTextJustification);
    function DoAddHatch(AList: TsgObjectList; const AIsCheckTransparent: Boolean;
      ACADImage: TsgCADImage = nil): TsgDXFEntity;
    function GetConverter: TsgDXFConverter;
    function InitAndAddEntity(ACADImage: TsgCADImage; AEntityClass: TsgDXFEntityClass;
      ATryToFill: Boolean = False; AHatch: Boolean = False): TsgDXFEntity;
    procedure InitConverter(const AImage: TsgCADImage; const AOwner: TsgObjectList;
      const AOwnerBox: TFRect; const AIndexName: string;
      AEncript,ADecript: TsgCriptMethod; AOnProgress: TProgressEvent;
      ATextsList: IsgTextRectFinder);
    procedure InitEntity(AEntity: TsgDXFEntity);
    function IsFillContour(AFill: Boolean): Boolean;
    function IsFitProportional: Boolean;
    function IsTransparentHatch: Boolean;
    procedure AddHatch;
    procedure AddOneHatch;
    procedure AddTextFromSubstrings(const ATextItem: TsgTextRectListItem;
      const AFigures: TsgObjectList; const AIsContour: Boolean);
    //procedure AddOneSolid(const ARect: TRect);
    function KoefScale: Double;
    function KoefScaleX: Double;
    function KoefScaleY: Double;
    function SetCircle(const R: TRect): Boolean;
    procedure SetDPtoLP(var ADevicePt: TPoint);
    procedure SetLType(AStyle: TPenStyle);
    procedure SetHatch(AColor: TColor; AFillStyle: Integer = 0;
      AStyle: Integer = BS_SOLID; AIsTextPath: Boolean = False);
    procedure SetFillingEntity(AValue: Boolean);
    procedure SetAlternateBlack(AValue: Boolean);
    procedure SetKoefPenSize(AValue: Single);
    procedure SetMatrix(AXForm: TXForm);
    procedure SetOffsetPoint(AValue: TF2DPoint);
    procedure SetOffsetX(AValue: Double);
    procedure SetOffsetY(AValue: Double);
    procedure SetOnlyPt(var APt: TFPoint);
    procedure SetPen;
    procedure SetPolyline(APoints: TFPointList; ACapacity: Integer;
      AClosed: Boolean = False);
    procedure SetPosition(P: TPoint); overload;
    procedure SetPosition(P: TSmallPoint); overload;
    procedure SetScale(AValue: Double);
    procedure SetUnitSize(AValue: Double);
    procedure SetUse01MM(AValue: Boolean);
    procedure SetUseMetafileRatio(AValue: Boolean);
    function TestTextImage(const AList: TsgObjectList; const ATextItem: TsgTextRectListItem;
      const AIsSubstring: Boolean; var AResText: string;
      ABoundBox: PFrect = nil): Boolean;
    function TranslateColor(AColor: TColor; AObjectType: TypeGDIObject = tgPen): TsgColorCAD;
  protected
    function AddEntity(const ACADImage: TsgCADImage; const AEntity: TsgDXFEntity;
      const ATryToFill: Boolean = False; const AHatch: Boolean = False;
      AClip: TObject = nil): TsgDXFEntity;
    procedure AddPoints(APointList: TFPointlist; APoints: PPoint;
      ACount: Integer; AIsTo: Boolean = False); overload;
    procedure AddPoints(APointList: TFPointlist; APoints: PsgSmallPoint;
      ACount: Integer; AIsTo: Boolean = False); overload;
    //function AnalizJpeg(ADate: PByte): Boolean;
    procedure DoOnProgress(AStage: TProgressStage; ADone, ACount: Integer);
    function GetTextStyle(const AFontFace: TsgFontFace;
      ACADImage: TsgCADImage = nil): TsgDXFStyle;
    function IsDrawContour(AList: TsgObjectList; AIsContour: Boolean): Boolean;
    function IsScanOfDocument(AImageEnt: TsgDXFImageEnt): Boolean;
    function IsSolidContour(ASolid: Boolean): Boolean;
    function PointToFPoint(const AX, AY: Double;
      AIsSize: Boolean = False): TFPoint; overload;
    function PointToFPoint(const P: TPoint;
      AIsSize: Boolean = False): TFPoint; overload;
    function PointToFPoint(const P: TSmallPoint): TFPoint; overload;
    procedure UnionLinesAndPolylines(const AFull: Boolean);
  public
    constructor Create(const AImage: TsgCADImage; const AOwner: TsgObjectList;
      const AOwnerBox: TFRect; const AIndexName: string;
      AEncript,ADecript: TsgCriptMethod;
      AOnProgress: TProgressEvent = nil;
      ATextsList: IsgTextRectFinder = nil); overload;
    constructor Create(const AImage: TsgCADImage; const AOwner: TsgObjectList;
      const AOwnerBox: TFRect; const AIndexName: string;
      ATextsList: Pointer); overload;
    destructor Destroy; override;
    function Convert(const AMetafile: TMetafile;
      const AOutBox: PFRect = nil): Boolean; overload;
    function Convert(const AList: TStrings;
      const AOutBox: PFRect = nil): Boolean; overload;
    procedure FreeUnusedBlocks;
    function GetFillColor(const AColor: TColor; var AOutColor: TColor;
      var AOutColorCAD: TsgColorCAD): Boolean;
    property Bounds: TFRect read FBounds;
    property CADImage: TsgCADImage read FCADImage;
    property CommandIndex: Integer read FCommandIndex;
    property Converter: TsgDXFConverter read GetConverter;
    property OffsetPoint: TF2DPoint read FOffsetPoint write SetOffsetPoint;
    property OffsetX: Double read FOffsetPoint.X write SetOffsetX;
    property OffsetY: Double read FOffsetPoint.Y write SetOffsetY;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Owner: TsgObjectList read FOwner write FOwner;
    property FileName: string read FFileName write FFileName;
    property AlternateBlack: Boolean read FAlternateBlack write SetAlternateBlack;
    property FillingEntity: Boolean read FFillingEntity write SetFillingEntity;
    property IsImageInClip: Boolean read FIsImageInClip;
    property LineWeightScale: Single read FKoefPenSize write SetKoefPenSize;
    property ListBlocks: TsgObjectList read FListBlocks;
    property Scale: Double read FScale write SetScale;
{$IFDEF SG_VECTORIZATION}
    property VectorizationMode: TsgPDFVectorizationMode read FVectorizationMode
      write FVectorizationMode;
{$ENDIF}
    property UnitSize: Double read FUnitSize write SetUnitSize;
    property Use01MM: Boolean read FUse01MM write SetUse01MM;
    property UseMetafileRatio: Boolean read FUseMetafileRatio write SetUseMetafileRatio;
  end;

  TsgMetafiles = class(TsgVectorImage)
  private
  public
    procedure LoadFromStream(S: TStream); override;
  end;

function ConvertToCad(const AMetafile: TMetafile; const AList: TStrings;
  const ACadImage: TsgCadImage; const ABox: TFRect; const AEntities: TList;
  const AIndexName: string; AEncript, ADecript: TsgCriptMethod;
  var AIsImageInClip: Boolean; const AOutBox: PFRect = nil;
  AOnProgress: TProgressEvent = nil;
  AParams: TsgCommonExportParams = nil;
  ATextsList: IsgTextRectFinder = nil): Integer;

function ConvertToCadList(const AMetafile: TMetafile; const AList: TStrings;
  const ACadImage: TsgCadImage; const ABox: TFRect; const AEntities: TsgObjectList;
  const AIndexName: string; AEncript, ADecript: TsgCriptMethod;
  var AIsImageInClip: Boolean; const AOutBox: PFRect = nil;
  AOnProgress: TProgressEvent = nil;
  AParams: TsgCommonExportParams = nil;
  ATextsList: IsgTextRectFinder = nil): Integer;

var
  vVectorizationTextStyleImage: TsgCADImage = nil;

implementation
{$IFDEF SG_VECTORIZATION}
uses
sgVectorization;
{$ENDIF}

type
  TypeCompression = (tcRGB,tcRLE8,tcRLE4,tcBitFields,tcJPEG,tcPNG);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgCurvePolygonAccess = class(TsgCadHatch); //TsgCADCurvePolygon;
  TsgCADHatchAccess = class(TsgCADHatch);
  TsgDXFSplineAccess = class(TsgDXFSpline);
  TsgCadImageAccess = class(TsgCadImage);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  Tsg2DCurveAccess = class(Tsg2DCurve);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgDXFBlockRecordsAccess = class(TsgDXFBlockRecords);

  TsgEdgeType = (etTOUCHING, etCROSSING, etINESSENTIAL);

{$IFDEF SG_TRY_CORRECT_RUS}
  TsgBadCharDescription = record
    RightChar: Char;
    IsEuro: Boolean;
  end;
{$ENDIF}

const
  cnstLTypeNames: array[TPenStyle] of string = (sContinuous, '_DASH', '_DOT', '_DASHDOT',
    '_DASHDOTDOT', '', '' {$IFDEF SGDEL_2006}, '', ''{$ENDIF});
  cnstFillingEntity: Boolean = True;

function ConvertToCadList(const AMetafile: TMetafile; const AList: TStrings;
  const ACadImage: TsgCadImage; const ABox: TFRect; const AEntities: TsgObjectList;
  const AIndexName: string; AEncript, ADecript: TsgCriptMethod;
  var AIsImageInClip: Boolean; const AOutBox: PFRect = nil;
  AOnProgress: TProgressEvent = nil;
  AParams: TsgCommonExportParams = nil;
  ATextsList: IsgTextRectFinder = nil): Integer;

  procedure SetCount(var ACount: Integer);
  begin
    if Assigned(AEntities) then
      ACount := AEntities.Count
    else
      ACount := ACadImage.CurrentLayout.Count;
  end;

var
  vMF: TsgConvertMetafileToCad;
  vCount1, vCount2: Integer;
  vResult: Boolean;
begin
  Result := 0;
  SetCount(vCount1);
  vMF := TsgConvertMetafileToCad.Create(ACadImage, AEntities, ABox, AIndexName,
    AEncript, ADecript, AOnProgress, ATextsList);
  try
    if Assigned(AParams) then
    begin
      vMF.FillingEntity := AParams.CADExportParams.FillingEntity;
      vMF.AlternateBlack := AParams.CADExportParams.AlternateBlack;
      vMF.Scale := AParams.CADExportParams.Scale;
      vMF.Use01MM := AParams.CADExportParams.Use01MM;
      vMF.LineWeightScale := AParams.CADExportParams.LineWeightScale;
      vMF.OffsetPoint := AParams.CADExportParams.OffsetPoint;
      vMF.UnitSize := AParams.CADExportParams.UnitSize;
{$IFDEF SG_VECTORIZATION}
      vMF.VectorizationMode := TsgPDFVectorizationMode(AParams.CADExportParams.VectorizationMode);
{$ENDIF}
    end
{$IFDEF SG_VECTORIZATION}
    else
      vMF.VectorizationMode := cnstDefaultVectorizationMode
{$ENDIF}
    ;
{$IFDEF SG_VECTORIZATION}
    if vMF.VectorizationMode in [pdfvmAuto,pdfvmAll] then
      SetDefaultSettings;
{$ENDIF}
    if Assigned(AList) and (AList.Count > 0) then
      vResult := vMF.Convert(AList, AOutBox)
    else
      vResult := vMF.Convert(AMetafile, AOutBox);
    AIsImageInClip := vMF.IsImageInClip;
    if vResult then
    begin
      SetCount(vCount2);
      Result := vCount2 - vCount1;
    end;
  finally
    vMF.Free;
  end;
end;

function ConvertToCad(const AMetafile: TMetafile; const AList: TStrings;
  const ACadImage: TsgCadImage; const ABox: TFRect; const AEntities: TList;
  const AIndexName: string; AEncript, ADecript: TsgCriptMethod;
  var AIsImageInClip: Boolean; const AOutBox: PFRect = nil;
  AOnProgress: TProgressEvent = nil;
  AParams: TsgCommonExportParams = nil;
  ATextsList: IsgTextRectFinder = nil): Integer;
var
  vEntities: TsgObjectList;
begin
  if Assigned(AEntities) then
  begin
    vEntities := TsgObjectList.Create;
    try
      vEntities.CopyFrom(AEntities);
      try
        Result := ConvertToCadList(AMetafile, AList, ACadImage, ABox, vEntities,
          AIndexName, AEncript, ADecript, AIsImageInClip, AOutBox, AOnProgress,
          AParams, ATextsList);
      finally
        vEntities.CopyTo(AEntities);
      end;
    finally
      vEntities.Free;
    end;
  end
  else
    Result := ConvertToCadList(AMetafile, AList, ACadImage, ABox, nil,
      AIndexName, AEncript, ADecript, AIsImageInClip, AOutBox, AOnProgress,
      AParams, ATextsList);
end;

function DecodeStdObject(AIHandle: DWORD): DWORD;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := AIHandle and $7FFFFFFF;
end;

function FindObject(AList: TList; var AIndex: Integer): PGDIObject;
var
  I: Integer;
  vTypeObject: PGDIObject;
begin
  Result := nil;
  for I := 0 to Pred(AList.Count) do
  begin
    vTypeObject := PGDIObject(AList[I]);
    if vTypeObject^.iHandle = AIndex then
    begin
      AIndex := I;
      Result := vTypeObject;
      Exit;
    end;
  end;
end;

procedure GetUnderEntittiesFromBlock(const ABlock: TsgDXFBlock;
  const AEntitiesInHatch: TsgObjectList; const AList: TsgObjectList;
  const aClasses: array of TsgDXFEntityClass);
var
  I,J: Integer;
  vEntity: TsgDXFEntity;
  vEntBox: TFRect;
  vIsHole: Boolean;
begin
  vIsHole := False;
  vEntBox := cnstBadRect;
  for I := 0 to AEntitiesInHatch.Count - 1 do
  begin
    vEntity := TsgDXFEntity(AEntitiesInHatch[I]);
    if vEntity is TsgCADBasePolyline and TsgCADBasePolyline(vEntity).Closed then
      vIsHole := True;
    UnionFRect(vEntBox, vEntity.Box);
  end;
  AList.Clear;
  if not(vIsHole and (AEntitiesInHatch.Count > 1)) then
    for I := 0 to ABlock.Count - 1 do
    begin
      vEntity := ABlock.Entities[I];
      if (IntersectFRect2D(vEntBox, vEntity.Box) > 0) and
        (AEntitiesInHatch.IndexOf(vEntity) < 0) then
        for J := 0 to Length(aClasses) - 1 do
          if vEntity is aClasses[J] then
          begin
            AList.Add(vEntity);
            Break;
          end;
    end;
end;

function GetPointerByOffset(const P: Pointer; AOffset: DWORD): Pointer;
begin
  if AOffset = 0 then
    Result := nil
  else
    Result := Pointer(PAnsiChar(P) + AOffset);
end;

function TranslateToMatrix(AXForm: TXForm): TFMatrix;
begin
  Result := FMat2DByParams(AXForm.eM11, AXForm.eM12, AXForm.eM21,
    AXForm.eM22, AXForm.eDx, AXForm.eDy);
end;

function TranslateToXForm(AMatrix: TFMatrix): TXForm;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.eM11 := AMatrix.V1[0];
  Result.eM12 := AMatrix.V1[1];
  Result.eM21 := AMatrix.V2[0];
  Result.eM22 := AMatrix.V2[1];
  Result.eDx := AMatrix.V4[0];
  Result.eDy := AMatrix.V4[1];
end;

function FindObjectSelect(AList: TList; const ATypeObj: TypeGDIObject): Integer;
var
  I: Integer;
  vTypeObject: PGDIObject;
begin
  Result := -1;
  for I := 0 to Pred(AList.Count) do
  begin
    vTypeObject := PGDIObject(AList[I]);
    if vTypeObject^.tpObject = ATypeObj then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function GetDiagonalLength(const ARect: TFRect): Double;
begin
  Result := DistanceFPoint(ARect.TopLeft, ARect.BottomRight);
end;

function GetEpsForFRect(const ARect: TFRect): Double;
const
  cnstRatio = 100;
begin
  Result := GetDiagonalLength(ARect) / cnstRatio;
end;

function GetEMRText(const EMR_CONST:byte):string;
begin
  case EMR_CONST of
    1: Result:='EMR_HEADER';
    2: Result:='EMR_POLYBEZIER';
    3: Result:='EMR_POLYGON';
    4: Result:='EMR_POLYLINE';
    5: Result:='EMR_POLYBEZIERTO';
    6: Result:='EMR_POLYLINETO';
    7: Result:='EMR_POLYPOLYLINE';
    8: Result:='EMR_POLYPOLYGON';
    9: Result:='EMR_SETWINDOWEXTEX';
    10: Result:='EMR_SETWINDOWORGEX';
    11: Result:='EMR_SETVIEWPORTEXTEX';
    12: Result:='EMR_SETVIEWPORTORGEX';
    13: Result:='EMR_SETBRUSHORGEX';
    14: Result:='EMR_EOF';
    15: Result:='EMR_SETPIXELV';
    16: Result:='EMR_SETMAPPERFLAGS';
    17  : Result:='EMR_SETMAPMODE';
    18  : Result:='EMR_SETBKMODE';
    19  : Result:='EMR_SETPOLYFILLMODE';
    20  : Result:='EMR_SETROP2';
    21  : Result:='EMR_SETSTRETCHBLTMODE';
    22  : Result:='EMR_SETTEXTALIGN';
    23  : Result:='EMR_SETCOLORADJUSTMENT';
    24  : Result:='EMR_SETTEXTCOLOR';
    25  : Result:='EMR_SETBKCOLOR';
    26  : Result:='EMR_OFFSETCLIPRGN';
    27  : Result:='EMR_MOVETOEX';
    28  : Result:='EMR_SETMETARGN';
    29  : Result:='EMR_EXCLUDECLIPRECT';
    30  : Result:='EMR_INTERSECTCLIPRECT';
    31  : Result:='EMR_SCALEVIEWPORTEXTEX';
    32  : Result:='EMR_SCALEWINDOWEXTEX';
    33  : Result:='EMR_SAVEDC';
    34  : Result:='EMR_RESTOREDC';
    35  : Result:='EMR_SETWORLDTRANSFORM';
    36  : Result:='EMR_MODIFYWORLDTRANSFORM';
    37  : Result:='EMR_SELECTOBJECT';
    38  : Result:='EMR_CREATEPEN';
    39  : Result:='EMR_CREATEBRUSHINDIRECT';
    40  : Result:='EMR_DELETEOBJECT';
    41  : Result:='EMR_ANGLEARC';
    42  : Result:='EMR_ELLIPSE';
    43  : Result:='EMR_RECTANGLE';
    44  : Result:='EMR_ROUNDRECT';
    45  : Result:='EMR_ARC';
    46  : Result:='EMR_CHORD';
    47  : Result:='EMR_PIE';
    48  : Result:='EMR_SELECTPALETTE';
    49  : Result:='EMR_CREATEPALETTE';
    50  : Result:='EMR_SETPALETTEENTRIES';
    51  : Result:='EMR_RESIZEPALETTE';
    52  : Result:='EMR_REALIZEPALETTE';
    53  : Result:='EMR_EXTFLOODFILL';
    54  : Result:='EMR_LINETO';
    55  : Result:='EMR_ARCTO';
    56  : Result:='EMR_POLYDRAW';
    57  : Result:='EMR_SETARCDIRECTION';
    58  : Result:='EMR_SETMITERLIMIT';
    59  : Result:='EMR_BEGINPATH';
    60  : Result:='EMR_ENDPATH';
    61  : Result:='EMR_CLOSEFIGURE';
    62  : Result:='EMR_FILLPATH';
    63  : Result:='EMR_STROKEANDFILLPATH';
    64  : Result:='EMR_STROKEPATH';
    65  : Result:='EMR_FLATTENPATH';
    66  : Result:='EMR_WIDENPATH';
    67  : Result:='EMR_SELECTCLIPPATH';
    68  : Result:='EMR_ABORTPATH';
    70  : Result:='EMR_GDICOMMENT';
    71  : Result:='EMR_FILLRGN';
    72  : Result:='EMR_FRAMERGN';
    73  : Result:='EMR_INVERTRGN';
    74  : Result:='EMR_PAINTRGN';
    75  : Result:='EMR_EXTSELECTCLIPRGN';
    76  : Result:='EMR_BITBLT';
    77  : Result:='EMR_STRETCHBLT';
    78  : Result:='EMR_MASKBLT';
    79  : Result:='EMR_PLGBLT';
    80  : Result:='EMR_SETDIBITSTODEVICE';
    81  : Result:='EMR_STRETCHDIBITS';
    82  : Result:='EMR_EXTCREATEFONTINDIRECTW';
    83  : Result:='EMR_EXTTEXTOUTA';
    84  : Result:='EMR_EXTTEXTOUTW';
    85  : Result:='EMR_POLYBEZIER16';
    86  : Result:='EMR_POLYGON16';
    87  : Result:='EMR_POLYLINE16';
    88  : Result:='EMR_POLYBEZIERTO16';
    89  : Result:='EMR_POLYLINETO16';
    90  : Result:='EMR_POLYPOLYLINE16';
    91  : Result:='EMR_POLYPOLYGON16';
    92  : Result:='EMR_POLYDRAW16';
    93  : Result:='EMR_CREATEMONOBRUSH';
    94  : Result:='EMR_CREATEDIBPATTERNBRUSHPT';
    95  : Result:='EMR_EXTCREATEPEN';
    96  : Result:='EMR_POLYTEXTOUTA';
    97  : Result:='EMR_POLYTEXTOUTW';
    98  : Result:='EMR_SETICMMODE';
    99  : Result:='EMR_CREATECOLORSPACE';
    100  : Result:='EMR_SETCOLORSPACE';
    101  : Result:='EMR_DELETECOLORSPACE';
    102  : Result:='EMR_GLSRECORD';
    103  : Result:='EMR_GLSBOUNDEDRECORD';
    104  : Result:='EMR_PIXELFORMAT';
    105  : Result:='EMR_DRAWESCAPE';
    106  : Result:='EMR_EXTESCAPE';
    107  : Result:='EMR_STARTDOC';
    108  : Result:='EMR_SMALLTEXTOUT';
    109  : Result:='EMR_FORCEUFIMAPPING';
    110  : Result:='EMR_NAMEDESCAPE';
    111  : Result:='EMR_COLORCORRECTPALETTE';
    112  : Result:='EMR_SETICMPROFILEA';
    113  : Result:='EMR_SETICMPROFILEW';
    114  : Result:='EMR_ALPHABLEND' ;
    115  : Result:='EMR_ALPHADIBBLEND';
    116  : Result:='EMR_TRANSPARENTBLT';
    117  : Result:='EMR_TRANSPARENTDIB';
    118  : Result:='EMR_GRADIENTFILL';
    119  : Result:='EMR_SETLINKEDUFIS';
    120  : Result:='EMR_SETTEXTJUSTIFICATION';
  else
    Result:='Unknown value';
  end;
end;

function GetTextBox(const AText: TsgDXFText; const ACADImage: TsgCADImage): TFRect;
var
  vPolylines: TsgObjectList;
  vPoints: TFPointList;
  I: Integer;
begin
  Result := cnstBadRect;
  vPolylines := TsgObjectList.Create;
  try
    ACADImage.Converter.GetTextPolylinesList(AText, vPolylines);
    for I := 0 to vPolylines.Count - 1 do
    begin
      vPoints := TFPointList(vPolylines[I]);
      UnionFRect(Result, vPoints.GetBox);
    end;
  finally
    TsgObjectList.FreeList(vPolylines);
  end;
end;

function GetZAngleFormMatrix(AMatrix: TFMatrix): Double;
var
  P1,P2,P3,P4: TFPoint;
begin
  P1 := MakeFPoint(0, 0);
  P2 := MakeFPoint(1, 0);
  P3 := FPointXMat(P1, AMatrix);
  P4 := FPointXMat(P2, AMatrix);
  Result := GetAngleOfVectors(SubFPoint(P2,P1), SubFPoint(P4,P3), False);
end;

function GetEMFBounds(hEMF: HENHMETAFILE):TRect;
var
  BufSize: UINT;
  pEMH:PEnhMetaHeader;
begin
  Result := Rect(0, 0, 0, 0);
  BufSize := 0;
  BufSize := GetEnhMetaFileHeader(hEMF, BufSize, nil);
  if BufSize = 0 then
    Exit;
  GetMem(pEMH,BufSize);
  try
    GetEnhMetaFileHeader(hEMF, BufSize, pEMH);
    Result:=pEMH^.rclBounds;
  finally
    if pEMH <> nil then
      FreeMem(pEMH);
  end;
end;

function GetPointsList(AHatch: TsgCADCurvePolygon; AIsPolyPolygon: Boolean): TObject;
var
  K,L,M: Integer;
  vPoints: TList;
  vCurFPointsList: TFPointList;
  vCurve: Tsg2DCurve;
  vBoundary: Tsg2DBoundaryList;
begin
  vPoints := nil;
  vCurFPointsList := nil;
  if AIsPolyPolygon then
  begin
    vPoints := TList.Create;
    Result := vPoints;
  end
  else
  begin
    vCurFPointsList := TFPointList.Create;
    Result := vCurFPointsList;
  end;
  for K := 0 to AHatch.BoundaryDataCount - 1 do
  begin
    vBoundary := AHatch.BoundaryData[K];
    if AIsPolyPolygon then
    begin
      vCurFPointsList := TFPointList.Create;
      vPoints.Add(vCurFPointsList);
    end;
    for L := 0 to vBoundary.Count - 1 do
    begin
      vCurve := vBoundary[L];
      Tsg2DCurveAccess(vCurve).Loaded(AHatch);
      for M := 0 to vCurve.Count - 1 do
        vCurFPointsList.Add(MakeFPointFrom2D(vCurve.Points[M]));
    end;
  end;
end;

function GetRectPoint(const ARect: TFRect; const AAngle: Double;
  const AIsSwapY,AIsNotBase: Boolean; const ASizes: PF2DPoint;
  var AQuadrant,AVertexNumber: Integer): TFPoint;
var
  vRect: TFRect;
begin
  vRect := ARect;
  if AIsSwapY then
    SwapDoubles(vRect.Top, vRect.Bottom);
  AQuadrant := Trunc(AAngle / cnstAnglePiDiv2[False]);
  if AIsNotBase then
    case AQuadrant of
      0:
        begin
          if Assigned(ASizes) then
            Result := MakeFPoint(vRect.Left, vRect.Top - ASizes.Y)
          else
            Result := MakeFPoint(vRect.Left, vRect.Bottom);
          AVertexNumber := 3;
        end;
      1:
        begin
          Result := vRect.BottomRight;
          AVertexNumber := 2;
        end;
      2:
        begin
          Result := MakeFPoint(vRect.Right, vRect.Top);
          AVertexNumber := 1;
        end;
    else
      Result := vRect.TopLeft;
      AVertexNumber := 0;
    end
  else
  begin
    Result := vRect.TopLeft;
    AVertexNumber := 0;
  end;
end;

function GetStdObjectColor(AIHandle: DWORD): TColor;
var
  vObject: DWORD;
begin
  vObject := DecodeStdObject(AIHandle);
  case vObject of
    BLACK_BRUSH,BLACK_PEN: Result := clBlack;
    DKGRAY_BRUSH,GRAY_BRUSH: Result := clGray;
    DC_BRUSH,WHITE_BRUSH,DC_PEN,WHITE_PEN: Result := clWhite;
  else
    Result := clNone;
  end;
end;

function MFProc(DC: HDC; HTable,P: Pointer;
  N: Integer; MF: TsgConvertMetafileToCad): Integer; stdcall;
begin
  Result := 1;
  try
    case PEMR(P)^.iType of
      EMR_HEADER:
        MF.DoHeader(PEnhMetaHeader(P));
      EMR_POLYBEZIER:
        MF.DoPolyBezier(P);
      EMR_POLYGON:
        MF.DoPolylineAnyType(P, False, False, True);
      EMR_POLYLINE:
        MF.DoPolylineAnyType(P);
      EMR_POLYBEZIERTO:
        MF.DoPolyBezier(P, False, True);
      EMR_POLYLINETO:
        MF.DoPolylineAnyType(P, False, True, True);
      EMR_POLYPOLYLINE:
        MF.DoPolyPolylineAnyType(P);
      EMR_POLYPOLYGON:
        MF.DoPolyPolylineAnyType(P, False, True);
      EMR_SETWINDOWEXTEX:
        MF.DoSetWindowExtex(PEMRSetViewPortExtEx(P));
      EMR_SETWINDOWORGEX:
        MF.DoSetWindowOrGex(PEMRSetWindowOrGex(P));
      EMR_SETVIEWPORTEXTEX:
        MF.DoSetViewPortExtex(PEMRSetViewPortExtEx(P));
      EMR_SETVIEWPORTORGEX:
        MF.DoSetViewPortOrGex(PEMRSetViewPortOrgEx(P));
      EMR_SETBRUSHORGEX:
        MF.DoSetBrushOrGex(PEMRSetBrushOrgEx(P));
      EMR_EOF:
        MF.DoEof(PEMREof(P));
      EMR_SETPIXELV:
        MF.DoSetPixelv(PEMRSetPixelV(P));
      EMR_SETMAPPERFLAGS:
        MF.DoSetMapperFlags(PEMRSetMapperFlags(P));
      EMR_SETMAPMODE:
        MF.DoSetMapMode(PEMRSetMapMode(P));
      EMR_SETBKMODE:
        MF.DoSetBkMode(PEMRSetBkMode(P));
      EMR_SETPOLYFILLMODE:
        MF.DoSetPolyFillMode(PEMRSetPolyFillMode(P));
      EMR_SETROP2:
        MF.DoSetRop2(PEMRSetRop2(P));
      EMR_SETSTRETCHBLTMODE:
        MF.DoSetStretchBltMode(PEMRSetStretchBltMode(P));
      EMR_SETTEXTALIGN:
        MF.DoSetTextAlign(PEMRSetTextAlign(P));
      EMR_SETCOLORADJUSTMENT:
        MF.DoSetColorAdjustment(PEMRSetColorAdjustment(P));
      EMR_SETTEXTCOLOR:
        MF.DoSetTextColor(PEMRSetTextColor(P));
      EMR_SETBKCOLOR:
        MF.DoSetBkColor(PEMRSetBkColor(P));
      EMR_OFFSETCLIPRGN:
        MF.DoOffsetClipRgn(PEMROffsetClipRgn(P));
      EMR_MOVETOEX:
        MF.DoMoveTo(PEMRLineTo(P));
      EMR_SETMETARGN:
        MF.DoSetMetaRgn(PEMRSetMetaRgn(P));
      EMR_EXCLUDECLIPRECT:
        MF.DoExcludeClipRect(PEMRExcludeClipRect(P));
      EMR_INTERSECTCLIPRECT:
        MF.DoIntersectClipRect(PEMRIntersectClipRect(P));
      EMR_SCALEVIEWPORTEXTEX:
        MF.DoScaleViewPortExtex(PEMRScaleViewportExtEx(P));
      EMR_SCALEWINDOWEXTEX:
        MF.DoScaleWindowExtex(PEMRScaleWindowExtEx(P));
      EMR_SAVEDC:
        MF.DoSaveDC(PEMRSaveDC(P));
      EMR_RESTOREDC:
        MF.DoRestoreDC(PEMRRestoreDC(P));
      EMR_SETWORLDTRANSFORM:
        MF.DoSetWorldTransform(PEMRSetWorldTransform(P));
      EMR_MODIFYWORLDTRANSFORM:
        MF.DoModifyWorldTransform(PEMRModifyWorldTransform(P));
      EMR_SELECTOBJECT:
        MF.DoSelectObject(PEMRSelectObject(P));
      EMR_CREATEPEN:
        MF.DoCreatePen(PEMRCreatePen(P));
      EMR_CREATEBRUSHINDIRECT:
        MF.DoCreateBrushIndirect(PEMRCreateBrushIndirect(P));
      EMR_DELETEOBJECT:
        MF.DoDeleteObject(PEMRDeleteObject(P));
      EMR_ANGLEARC:
        MF.DoAngleArc(PEMRAngleArc(P));
      EMR_ELLIPSE:
        MF.DoEllipse(PEMREllipse(P));
      EMR_RECTANGLE:
        MF.DoRectangle(PEMRRectangle(P));
      EMR_ROUNDRECT:
        MF.DoRoundRect(PEMRRoundRect(P));
      EMR_ARC:
        MF.DoArc(PEMRArc(P));
      EMR_CHORD:
        MF.DoChord(PEMRArc(P));
      EMR_PIE:
        MF.DoPie(PEMRArc(P));
      EMR_SELECTPALETTE:
        MF.DoSelectPalette(PEMRSelectPalette(P));
      EMR_CREATEPALETTE:
        MF.DoCreatePalette(PEMRCreatePalette(P));
      EMR_SETPALETTEENTRIES:
        MF.DoSetPaletteEntries(PEMRSetPaletteEntries(P));
      EMR_RESIZEPALETTE:
        MF.DoResizePalette(PEMRResizePalette(P));
      EMR_REALIZEPALETTE:
        MF.DoRealizePalette(PEMRRealizePalette(P));
      EMR_EXTFLOODFILL:
        MF.DoExtFloodFill(PEMRExtFloodFill(P));
      EMR_LINETO:
        MF.DoLineTo(PEMRLineTo(P));
      EMR_ARCTO:
        MF.DoArc(PEMRArcTo(P),True);
      EMR_POLYDRAW:
        MF.DoPolyDraw(PEMRPolyDraw(P));
      EMR_SETARCDIRECTION:
        MF.DoSetArcDirection(PEMRSetArcDirection(P));
      EMR_SETMITERLIMIT:
        MF.DoSetMiterLimit(PEMRSetMiterLimit(P));
      EMR_BEGINPATH:
        MF.DoBeginPath(PEMRBeginPath(P));
      EMR_ENDPATH:
        MF.DoEndPath(PEMREndPath(P));
      EMR_CLOSEFIGURE:
        MF.DoCloseFigure(PEMRCloseFigure(P));
      EMR_FILLPATH:
        MF.DoFillPath(PEMRFillPath(P));
      EMR_STROKEANDFILLPATH:
        MF.DoStrokeAndFillPath(PEMRStrokeAndFillPath(P));
      EMR_STROKEPATH:
        MF.DoStrokePath(PEMRStrokePath(P));
      EMR_FLATTENPATH:
        MF.DoFlattenPath(PEMRFlattenPath(P));
      EMR_WIDENPATH:
        MF.DoWidenPath(PEMRWidenPath(P));
      EMR_SELECTCLIPPATH:
        MF.DoSelectClipPath(PEMRSelectClipPath(P));
      EMR_ABORTPATH:
        MF.DoAbortPath(PEMRABortPath(P));
      EMR_GDICOMMENT:
        MF.DoGDIComment(PEMRGDIComment(P));
      EMR_FILLRGN:
        MF.DoFillRgn(PEMRFillRgn(P));
      EMR_FRAMERGN:
        MF.DoFrameRgn(PEMRFrameRgn(P));
      EMR_INVERTRGN:
        MF.DoInvertRgn(PEMRInvertRgn(P));
      EMR_PAINTRGN:
        MF.DoPaintRgn(PEMRPaintRgn(P));
      EMR_EXTSELECTCLIPRGN:
        MF.DoExtSelectClipRgn(PEMRExtSelectClipRgn(P));
      EMR_BITBLT:
        MF.DoBitBlt(PEMRBitBlt(P));
      EMR_STRETCHBLT:
        MF.DoStretchBlt(PEMRStretchBlt(P));
      EMR_MASKBLT:
        MF.DoMaskBlt(PEMRMaskBlt(P));
      EMR_PLGBLT:
        MF.DoPlgBlt(PEMRPLGBlt(P));
      EMR_SETDIBITSTODEVICE:
        MF.DoSetDIBItstoDevice(PEMRSetDIBitsToDevice(P));
      EMR_STRETCHDIBITS:
        MF.DoStretchDIBIts(PEMRStretchDIBits(P));
      EMR_EXTCREATEFONTINDIRECTW:
        MF.DoExtCreareFontIndirectw(PEMRExtCreateFontIndirect(P));
      EMR_EXTTEXTOUTA:
        MF.DoExtTextOutA(PEMRextTextOut(P));
      EMR_EXTTEXTOUTW:
        MF.DoExtTextOutW(PEMRExtTextOut(P));
      EMR_POLYBEZIER16:
        MF.DoPolyBezier(P, True);
      EMR_POLYGON16:
        MF.DoPolylineAnyType(P, True, False, True);
      EMR_POLYLINE16:
        MF.DoPolylineAnyType(P, True);
      EMR_POLYBEZIERTO16:
        MF.DoPolyBezier(P, True, True);
      EMR_POLYLINETO16:
        MF.DoPolylineAnyType(P, True, True);
      EMR_POLYPOLYLINE16:
        MF.DoPolyPolylineAnyType(P, True);
      EMR_POLYPOLYGON16:
        MF.DoPolyPolylineAnyType(P, True, True);
      EMR_POLYDRAW16:
        MF.DoPolyDraw16(PEMRPolyDraw16(P));
      EMR_CREATEMONOBRUSH:
        MF.DoCreateMonoBrush(PEMRCreateMonoBrush(P));
      EMR_CREATEDIBPATTERNBRUSHPT:
        MF.DoCreateDibPatternBrushPt(PEMRCreateDibPatternBrushPt(P));
      EMR_EXTCREATEPEN:
        MF.DoExtCreatePen(PEMRExtCreatePen(P));
      EMR_POLYTEXTOUTA:
        MF.DoPolyTextOut(PEMRPolyTextOut(P));
      EMR_POLYTEXTOUTW:
        MF.DoPolyTextOut(PEMRPolyTextOut(P));
      EMR_SETICMMODE:
        MF.DoSetICMMode(PEMRSetICMMode(P));
      EMR_CREATECOLORSPACE:
        MF.DoCreateColorSpace(PEMRCreateColorSpace(P));
      EMR_SETCOLORSPACE:
        MF.DoSetColorSpace(PEMRSelectColorSpace(P));
      EMR_DELETECOLORSPACE:
        MF.DoDeleteColorSpace(PEMRDeleteColorSpace(P));
      EMR_GLSRECORD:
        MF.DoGLSRecord(PEMRGLSRecord(P));
      EMR_GLSBOUNDEDRECORD:
        MF.DoGLSBoundedRecord(PEMRGLSBoundedRecord(P));
      EMR_PIXELFORMAT:
        MF.DoPixelFormat(PEMRPixelFormat(P));
      EMR_DRAWESCAPE:
        {TsgMetafiles(Data).DoDrawEscape(PEMRDrawEscape(P))};
      EMR_EXTESCAPE:
        {TsgMetafiles(Data).DoExtEscape(PEMRExtEscape(P))};
      EMR_STARTDOC:
        {TsgMetafiles(Data).DoStartDoc(PEMRStartDoc(P))};
      EMR_SMALLTEXTOUT:
        {TsgMetafiles(Data).DoSmallTextOut(PEMRSmallTextOut(P))};
      EMR_FORCEUFIMAPPING:
        {TsgMetafiles(Data).DoForceUfiMapping(PEMRForceUfImapping(P))};
      EMR_NAMEDESCAPE:
        {TsgMetafiles(Data).DoNamedEscape(PEMRNamedSpace(P))};
      EMR_COLORCORRECTPALETTE:
        {TsgMetafiles(Data).DoColorCorrectPalette(PEMRColorCorrectPalette(P))};
      EMR_SETICMPROFILEA:
        {TsgMetafiles(Data).DoSetICMProfile(PEMRSetICMProfile(P))};
      EMR_SETICMPROFILEW:
        {TsgMetafiles(Data).DoSetICMProfile(PEMRSetICMProfile(P))};
      EMR_ALPHABLEND:
        MF.DoAlphaBlend(PEMRAlphaBlend(P));
      EMR_ALPHADIBBLEND:
        {DoAlphaDIBBlend(PEMRAlphaDIBBlend(P))};
      EMR_TRANSPARENTBLT:
        MF.DoTransparentBlt(PEMRTransparentBlt(P));
      EMR_TRANSPARENTDIB:
        {DoTransparentDIB(PEMRTransparentDIB(P))};
      EMR_GRADIENTFILL:
        MF.DoGradientFill(PEMGradientFill(P));
      EMR_SETLINKEDUFIS:
        {TsgMetafiles(Data).DoSetLinkedUfIs(PEMRSetLinkedUfIs)};
      EMR_SETTEXTJUSTIFICATION:
        {TsgMetafiles(Data).DoSetTextJustification(PEMRSetTextJustification(P))};
    end;
  except
  end;
  Inc(MF.FCommandIndex);
  MF.DoOnProgress(psRunning, MF.FCommandIndex, MF.FMetaFileHeader.nRecords);
end;

function IsBlackWhiteColorCAD(AColor: TsgColorCAD): Boolean;
begin
  Result := IsEqualColorCAD(AColor, cnstColorCADByBlackWhite);
end;

function IsEmptyColorCAD(AColor: TsgColorCAD): Boolean;
begin
  Result := IsEqualColorCAD(AColor, cnstColorCADNone);
end;

function IsPointInsideRect(ARect: TFRect; APoint: TFPoint): Boolean;
begin
  Result := IsPointInFRect2D(ARect, APoint, GetEpsForFRect(ARect));
end;

function IsRectInsideRect(ABigR, ASmallR: TFRect): Boolean;
begin
  Result := IsPointInsideRect(ABigR, ASmallR.TopLeft) and
    IsPointInsideRect(ABigR, ASmallR.BottomRight);
end;

function IsMiddlePoint(const AP1, AP2, APoint: TFPoint): Boolean;
begin
  if IsEqual(AP1.X, AP2.X) then
    Result := ((AP1.Y <= APoint.Y) and (AP2.Y >= APoint.Y)) or
      ((AP2.Y <= APoint.Y) and (AP1.Y >= APoint.Y))
  else
    Result := ((AP1.X <= APoint.X) and (AP2.X >= APoint.X)) or
      ((AP2.X <= APoint.X) and (AP1.X >= APoint.X));
end;

procedure DoPureRect(var ARect: TRect);
begin
  if ARect.Left > ARect.Right then
    SwapInts(ARect.Right, ARect.Left);
  if ARect.Top > ARect.Bottom then
    SwapInts(ARect.Top, ARect.Bottom);
  //IncreaseRect(ARect);
end;

function ClipPointByFRect(const ARect: TFRect; const AP: TFPoint): TFPoint;
begin
  Result := AP;
  if AP.X < ARect.Left then
    Result.X := ARect.Left;
  if AP.X > ARect.Right then
    Result.X := ARect.Right;
  if AP.Y > ARect.Top then
    Result.Y := ARect.Top;
  if AP.Y < ARect.Bottom then
    Result.Y := ARect.Bottom;
end;

function ClipPointByRect(const ARect: TRect; const AP: TFPoint): TFPoint;
begin
  Result := AP;
  if AP.X < ARect.Left then
    Result.X := ARect.Left;
  if AP.X > ARect.Right then
    Result.X := ARect.Right;
  if AP.Y < ARect.Top then
    Result.Y := ARect.Top;
  if AP.Y > ARect.Bottom then
    Result.Y := ARect.Bottom;
end;

{ TsgParamsPath }

constructor TsgParamsPath.Create(AConverter: TsgConvertMetafileToCad);
begin
  inherited Create;
  FConverter := AConverter;
  FListFiguresInPath := TsgObjectList.Create;
  FHatchInPath := TsgObjectList.Create;
  Clear(False);
end;

destructor TsgParamsPath.Destroy;
begin
  Clear(True);
  FHatchInPath.Free;
  FListFiguresInPath.Free;
  inherited Destroy;
end;

procedure TsgParamsPath.GetCorrectIndexes(var AStart,AStop: Integer);
var
  I: Integer;
  vIsSetFirst: Boolean;
  vEnt: TsgDXFEntity;
  vPoly: TsgCADBasePolyline absolute vEnt;
begin
  AStart := FIndexFirstFigure;
  AStop := FIndexFirstFigure;
  vIsSetFirst := False;
  for I := FIndexFirstFigure to FListFiguresInPath.Count - 1 do
  begin
    vEnt := TsgDXFEntity(FListFiguresInPath[I]);
    case vEnt.EntType of
      ceCircle, ceArc, ceEllipse, cePolyline, ceLWPolyline, ceSpline:
        begin
          if not IsEqualFPoints(vPoly.Points[0], vPoly.Points[vPoly.PointCount - 1]) then
          begin
            if not vIsSetFirst then
            begin
              AStart := I;
              vIsSetFirst := True;
            end;
            AStop := I;
          end;
        end;
      ceLine:
        begin
          if not vIsSetFirst then
          begin
            AStart := I;
            vIsSetFirst := True;
          end;
          AStop := I;
        end;
    end;
  end;
end;

class function TsgParamsPath.GetEntityPoints(const AEntity: TsgDXFEntity;
  APoints: TFPointList): Boolean;
var
  vLine: TsgDXFLine absolute AEntity;
  vPoly: TsgCADBasePolyline absolute AEntity;
begin
  APoints.Clear;
  Result := False;
  if AEntity is TsgCADBasePolyline then
  begin
    Result := True;
    APoints.AppendArray(vPoly.PolyPoints);
  end
  else
    case AEntity.EntType of
      ceLine:
        begin
          Result := True;
          APoints.Add(vLine.Point);
          APoints.Add(vLine.Point1);
        end;
    end;
end;

function TsgParamsPath.GetPointTypeFigure(AEnt: TsgDXFEntity;
  AFirst: Boolean = True): Boolean;
var
  vCountPoints: Integer;
begin
  Result := False;
  case AEnt.EntType of
    ceLine:
      begin
        if AFirst then
          FEndPathLine.Point1 := TsgDXFLine(AEnt).Point
        else
          FEndPathLine.Point2 := TsgDXFLine(AEnt).Point1;
        Result := True;
      end;
    cePolyline, ceLWPolyline:
      begin
        if AFirst then
          FEndPathLine.Point1 := TsgDXFVertex(TsgDXFPolyline(AEnt).Entities[0]).Point
        else
        begin
          vCountPoints := Pred(TsgDXFPolyline(AEnt).Count);
          FEndPathLine.Point2 := TsgDXFVertex(TsgDXFPolyline(AEnt).Entities[vCountPoints]).Point;
        end;
        Result := True;
      end;
    ceSpline, ceArc, ceCircle, ceEllipse:
      begin
        vCountPoints := Pred(TsgDXFArc(AEnt).PointCount);
        if vCountPoints >= 0 then
        begin
          if AFirst then
            FEndPathLine.Point1 := TsgCADBasePolyline(AEnt).Points[0]
          else
            FEndPathLine.Point2 := TsgCADBasePolyline(AEnt).Points[vCountPoints];
          Result := True;
        end;
      end;
  end;
end;

procedure TsgParamsPath.SetBeginPath(AValue: Boolean);
begin
  FBeginPath := AValue;
end;

function IsCrossOrTouchPolies(const APoly1,APoly2: TFPointList;
  const AAccuracy: Double): Boolean;
var
  I,J: Integer;
  vPt11,vPt12,vPt21,vPt22: TFPoint;
begin
  Result := False;
  for I := 1 to APoly1.Count - 1 do
  begin
    vPt11 := APoly1[I - 1];
    vPt12 := APoly1[I];
    for J := 1 to APoly2.Count - 1 do
    begin
      vPt21 := APoly2[J - 1];
      vPt22 := APoly2[J];
      if IsCrossSegmentsPts(vPt11, vPt12, vPt21, vPt22, nil, AAccuracy, [1,2]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TsgParamsPath.IsClosablePath(const AStart,AStop: Integer): Boolean;
var
  vEntities: TList;
  vEntity: TsgDXFEntity;
  vPoly1,vPoly2: TFPointList;
  I: Integer;
  vIsIntersected: Boolean;
  vBox: TFRect;
  vAccuracy: Double;
begin
  Result := True;
  vEntities := TList.Create;
  try
    vBox := cnstBadRect;
    for I := AStart to AStop do
    begin
      vEntity := TsgDXFEntity(FListFiguresInPath[I]);
      vEntities.Add(vEntity);
      UnionFRect(vBox, vEntity.Box);
    end;
    vPoly1 := TFPointList.Create;
    try
      vPoly2 := TFPointList.Create;
      try
        vAccuracy := GetAccuracyByBox(vBox, fAccuracy);
        while vEntities.Count > 0 do
        begin
          if GetEntityPoints(vEntities.First, vPoly1) then
          begin
            vIsIntersected := False;
            if vEntities.Count = 1 then
              vIsIntersected := not IsEqualFPoints(vPoly1.First, vPoly1.Last)
            else
            begin
              for I := 1 to vEntities.Count - 1 do
                if GetEntityPoints(vEntities[I], vPoly2) then
                begin
                  if IsCrossOrTouchPolies(vPoly1, vPoly2, vAccuracy) then
                  begin
                    vIsIntersected := True;
                    Break;
                  end;
                end;
            end;
            if not vIsIntersected then
            begin
              Result := False;
              Break;
            end;
          end;
          vEntities.Remove(vEntities.First);
        end;
      finally
        vPoly2.Free;
      end;
    finally
      vPoly1.Free;
    end;
  finally
    vEntities.Free;
  end;
end;

function TsgParamsPath.ActivePath: TsgObjectList;
begin
  if FHatchInPath.Count > 0 then
    Result := FHatchInPath
  else
    Result := FListFiguresInPath;
end;

procedure TsgParamsPath.Add(AEntity: TsgDXFEntity);
begin
  FListFiguresInPath.Add(AEntity);
end;

procedure TsgParamsPath.AddPoint(AIPoint: TPoint; AFPoint: TFPoint);
begin
  Inc(FPointCount);
  if FIsInitRect then
  begin
    ExpandFRect(FPathFRect, AFPoint);
    ExpandRect(FPathRect, AIPoint);
  end
  else
  begin
    FIsInitRect := True;
    FPathRect.TopLeft := AIPoint;
    FPathRect.BottomRight := AIPoint;
    FPathFRect.TopLeft := AFPoint;
    FPathFRect.BottomRight := AFPoint;
  end;
end;

procedure TsgParamsPath.Assign(AParamsPath: TsgParamsPath);
var
  I,J: Integer;
  vEntity,vNewEnt: TsgDXFEntity;
begin
  Clear(True);
  FListFiguresInPath.Capacity := AParamsPath.ListFiguresInPath.Count;
  FHatchInPath.Capacity := AParamsPath.HatchInPath.Count;
  FBeginPath := AParamsPath.FBeginPath;
  FClosingLine := AParamsPath.FClosingLine;
  FConverter := AParamsPath.FConverter;
  FCountHatches := AParamsPath.FCountHatches;
  FEndPathLine := AParamsPath.FEndPathLine;
  FIsInitRect := AParamsPath.FIsInitRect;
  FPathFRect := AParamsPath.FPathFRect;
  FPathRect := AParamsPath.FPathRect;
  FPointCount := AParamsPath.FPointCount;
  for I := 0 to AParamsPath.ListFiguresInPath.Count - 1 do
  begin
    vEntity := TsgDXFEntity(AParamsPath.ListFiguresInPath[I]);
    vNewEnt := TsgDXFEntityClass(vEntity.ClassType).Create;
    vNewEnt.AssignEntity(vEntity);
    FConverter.Converter.Loads(vNewEnt);
    FListFiguresInPath.Add(vNewEnt);
  end;
  J := 0;
  while J < Length(FCountHatches) do
  begin
    for I := FCountHatches[J] to FCountHatches[J + 1] do
      FHatchInPath.Add(FListFiguresInPath[I]);
    Inc(J, 2);
  end;
  FIndexFirstFigure := AParamsPath.FIndexFirstFigure;
end;

procedure TsgParamsPath.Clear(AFreeEntities: Boolean);
begin
  ClearLists(AFreeEntities);
  ClearParams;
end;

procedure TsgParamsPath.ClearLists(AFreeEntities: Boolean);
begin
  if AFreeEntities then
    TsgObjectList.ClearList(FListFiguresInPath)
  else
    FListFiguresInPath.Clear;
  FHatchInPath.Clear;
  FIndexFirstFigure := 0;
  SetLength(FCountHatches, 0);
end;

procedure TsgParamsPath.ClearParams;
begin
  FBeginPath := False;
  FIsInitRect := False;
  FPathFRect := cnstBadRect;
  FPathRect := cnstRectZero;
  FPointCount := 0;
end;

procedure TsgParamsPath.CloseFigure;
var
  I,vStart,vStop: Integer;
  vClosed, vFirstFig, vLastFig: Boolean;
  vPoly: TsgCADBasePolyline;
begin
  vClosed := False;
  FClosingLine := nil;
  if (FListFiguresInPath.Count < 1) or (FIndexFirstFigure >= FListFiguresInPath.Count) then
    Exit
  else
    if FListFiguresInPath.Count = 1 then
      case TsgDXFEntity(FListFiguresInPath[0]).EntType of
        cePolyline, ceLWPolyline, ceSpline:
        begin
          vPoly := TsgCADBasePolyline(FListFiguresInPath[0]);
          FEndPathLine.Point1 := vPoly.PolyPoints.First;
          FEndPathLine.Point2 := vPoly.PolyPoints.Last;
          if (not vPoly.Closed) and (not IsEqualFPoints(FEndPathLine.Point1, FEndPathLine.Point2)) then
          begin
            FClosingLine := @FEndPathLine;
            vClosed := True;
          end;
        end;
      end
    else
    begin
      GetCorrectIndexes(vStart, vStop);
      FEndPathLine.Point1 := cnstFPointZero;
      FEndPathLine.Point2 := cnstFPointZero;
      vFirstFig := GetPointTypeFigure(TsgDXFEntity(FListFiguresInPath[vStart]));
      vLastFig := GetPointTypeFigure(tsgDXFEntity(FListFiguresInPath[vStop]), False);
      if (not vFirstFig) and (not vLastFig) then
        Exit;
      if not IsEqualFPoints(FEndPathLine.Point1, FEndPathLine.Point2) then
        if vFirstFig and vLastFig and IsClosablePath(vStart, vStop) then
          FClosingLine := @FEndPathLine;
      vClosed := True;
    end;
  if vClosed then
  begin
    if (FListFiguresInPath.Count - FIndexFirstFigure) > 0 then
    begin
      SetLength(FCountHatches, Length(FCountHatches) + 2);
      FCountHatches[Length(FCountHatches) - 2] := FIndexFirstFigure;
      FCountHatches[Length(FCountHatches) - 1] := Pred(FListFiguresInPath.Count);
      for I := FIndexFirstFigure to Pred(FListFiguresInPath.Count) do
        FHatchInPath.Add(FListFiguresInPath[I]);
    end;
  end;
  SaveStartFigure;
end;

function TsgParamsPath.IsNeedColse: Boolean;
begin
  Result := (FHatchInPath.Count <= 0) and (FListFiguresInPath.Count > 0);
end;

procedure TsgParamsPath.Remove(AEntity: TsgDXFEntity);
begin
  FHatchInPath.Remove(AEntity);
  FListFiguresInPath.Remove(AEntity);
end;

procedure TsgParamsPath.SaveStartFigure;
begin
  FIndexFirstFigure := FListFiguresInPath.Count;
end;

procedure TsgParamsPath.StartPath;
begin
  FBeginPath := True;
  FIsInitRect := False;
  ClearLists(True);
end;

procedure TsgParamsPath.StopPath;
begin
  FIndexFirstFigure := 0;
end;

{ TsgDCStackItem }

constructor TsgDCStackItem.Create(AMetafile: TsgConvertMetafileToCad);
begin
  inherited Create;
  FClipRgn := TsgRegion.Create(AMetafile);
  FClipPath := TFPointList.Create;
  FParamsPath := TsgParamsPath.Create(AMetafile);
end;

destructor TsgDCStackItem.Destroy;
begin
  FClipRgn.Free;
  FClipPath.Free;
  FParamsPath.Free;
  inherited Destroy;
end;

procedure TsgDCStackItem.SetClipRgn(AValue: TsgRegion);
begin
  FClipRgn.Assign(AValue);
end;

procedure TsgDCStackItem.SetClipPath(AValue: TFPointList);
begin
  FClipPath.Assign(AValue);
end;

procedure TsgDCStackItem.SetParamsPath(AValue: TsgParamsPath);
begin
  FParamsPath.Assign(AValue);
end;

{ TsgRegionClip }

constructor TsgRegionClip.Create(const AClipPath: TFPointList;
  const AClipMode: Integer; const AParentClip: TsgRegionClip;
  const AMF: TsgConvertMetafileToCad; const ABlock: TsgDXFBlock);
begin
  inherited Create;
  FIsOwnedBlock := False;
  CreateClip(AClipMode, AParentClip, AMF, ABlock);
  FFilter.Bounds.Assign(AClipPath);
end;

constructor TsgRegionClip.Create(const AClipRect: TFRect;
  const AClipMode: Integer; const AParentClip: TsgRegionClip;
  const AMF: TsgConvertMetafileToCad; const ABlock: TsgDXFBlock);
begin
  inherited Create;
  FIsOwnedBlock := False;
  CreateClip(AClipMode, AParentClip, AMF, ABlock);
  FFilter.Bounds.Add(AClipRect.TopLeft);
  FFilter.Bounds.Add(AClipRect.BottomRight);
end;

constructor TsgRegionClip.Create(const AMF: TsgConvertMetafileToCad);
begin
  inherited Create;
  FIsOwnedBlock := False;
  CreateClip(0, nil, AMF, nil);
end;

destructor TsgRegionClip.Destroy;
begin
  ClearClip;
  inherited Destroy;
end;

procedure TsgRegionClip.ClearClip;
begin
  if FInsert.Handle <> 0 then
    Exit;
  OwnedBlock.RemoveEntity(FInsert);
  if IsOwnedBlock then
    FInsert.Block := nil;
  OwnedBlock.RemoveEntity(FInsert);
  FreeAndNil(FInsert);
  FreeAndNil(FFilter);
  FIsOwnedBlock := False;
end;

procedure TsgRegionClip.CreateClip(const AClipMode: Integer;
  const AParentClip: TsgRegionClip; const AMF: TsgConvertMetafileToCad;
  const ABlock: TsgDXFBlock);
var
  vColor: TColor;
  vColorCAD: TsgColorCAD;
  vBlock: TsgDXFBlock;
begin
  FMF := AMF;
  FClipMode := AClipMode;
  FOwner := AParentClip;
  FIsWorkedClip := False;
  FInsert := TsgDXFInsert.Create;
  AMF.GetFillColor(clNone, vColor, vColorCAD);
  FInsert.ColorCAD := vColorCAD;
  //FInsert.FIndex := AMF.CommandIndex;
  OwnedBlock.AddEntity(FInsert);
  if Assigned(ABlock) then
  begin
    FIsOwnedBlock := False;
    FInsert.Block := ABlock;
  end
  else
  begin
    FIsOwnedBlock := True;
    vBlock := TsgDXFBlock.Create;
    FInsert.Block := vBlock;
    vBlock.Name := 'MF' + IntToStr(AMF.CommandIndex);
    //vBlock.FIndex := AMF.FCommandIndex;
    AMF.Converter.Sections[csBlocks].AddEntity(vBlock);
    AMF.ListBlocks.Add(vBlock);
  end;
  FFilter := TsgCADSpatialFilter.Create;
  FFilter.Name := cnstSpatial;
  FFilter.DisplayBounds := True;
  FInsert.Scale := cnstFPointSingle;
end;

function TsgRegionClip.GetBlock: TsgDXFBlock;
begin
  Result := FInsert.Block;
end;

function TsgRegionClip.GetBounds: TFPointList;
begin
  Result := FFilter.Bounds;
end;

function TsgRegionClip.GetFilterBox: TFRect;
begin
  if FFilter.Bounds.Count = 2 then
    Result := MakeFRectByPoints(FFilter.Bounds.First, FFilter.Bounds.Last)
  else
    Result := GetBoxOfFPoints(FFilter.Bounds);
end;

procedure TsgRegionClip.SetBlock(const AValue: TsgDXFBlock);
begin
  if IsOwnedBlock then
  begin
    FInsert.Block := nil;
    FIsOwnedBlock := False;
  end;
  FInsert.Block := AValue;
end;

procedure TsgRegionClip.Assign(AClip: TsgRegionClip);
begin
  ClearClip;
  CreateClip(AClip.ClipMode, AClip.Owner, AClip.MF, AClip.ExternalBlock);
  FFilter.Bounds.Assign(AClip.Bounds);
end;

function TsgRegionClip.ExternalBlock: TsgDXFBlock;
begin
  if IsOwnedBlock then
    Result := FInsert.Block
  else
    Result := nil;
end;

procedure TsgRegionClip.Load(AConverter: TsgDXFConverter);
var
  vDict: TsgDXFDictionary;

  function CreateDict(const AName: string; const AEnt: TsgDXFEntity): TsgDXFDictionary;
  begin
    Result := TsgDXFDictionary.Create;
    Result.Name := AName;
    Result.AddEntity(AEnt);
    AConverter.Loads(Result);
  end;

begin
  AConverter.Loads(FInsert.Block);
  vDict := CreateDict(sAcadFilterDictionary, FFilter);
  AConverter.Loads(FFilter);
  vDict := CreateDict(cnstDictionary, vDict);
  AConverter.Sections[csObjects].AddEntity(vDict);
  TsgDXFConverterAccess(AConverter).SetHandle(FInsert);
  FInsert.Dictionary := vDict;
  AConverter.Loads(FInsert);
end;

procedure TsgRegionClip.MoveToTop;
begin
  if Assigned(FOwner) then
  begin
    FOwner.Insert.Block.RemoveEntity(FInsert);
    FOwner.Insert.Block.AddEntity(FInsert);
    FOwner.MoveToTop;
  end;
end;

function TsgRegionClip.OwnedBlock: TsgDXFEntity;
begin
  if Assigned(FOwner) then
    Result := FOwner.Insert.Block
  else
    Result := FMF.CADImage.CurrentLayout;
end;

{ TsgRegion }

constructor TsgRegion.Create(AParentMetaFile: TsgConvertMetafileToCad);
begin
  FParentMetaFile := AParentMetaFile;
  FLoadStack := TList.Create;
  FWorkset := TList.Create;
  FIsClipping := False;
  FIsClipSet := False;
  FClip := nil;
end;

destructor TsgRegion.Destroy;
begin
  FParentMetaFile := nil;
  FreeList(FLoadStack);
  FWorkset.Free;
  inherited Destroy;
end;

procedure TsgRegion.Assign(ASource: TsgRegion);
begin
  if ASource = nil then
    Exit;
  Clear;
  FParentMetaFile := ASource.FParentMetaFile;
  FIsClipping := ASource.FIsClipping;
  FIsClipSet := ASource.FIsClipSet;
  FClip := ASource.FClip;
  {$IFDEF SGDEL_7}
  FWorkset.Assign(ASource.FWorkset);
  {$ELSE}
  CopyLists(FWorkset, ASource.FWorkset);
  {$ENDIF}
end;

procedure TsgRegion.Clear(const AIsAll: Boolean = False);
begin
  FIsClipping := False;
  FIsClipSet := False;
  FWorkset.Clear;
  if AIsAll then
    ClearList(FLoadStack);
end;

procedure TsgRegion.Load;
begin
  CorrectEmptyClips;
  LoadInternal(nil);
  FParentMetaFile.FreeUnusedBlocks;
end;

function TsgRegion.MakeFRectForRect(const ARect: TRect): TFRect;
begin
  if Assigned(FParentMetaFile) then
  begin
    Result := MakeFRectByPoints(
      FParentMetaFile.PointToFPoint(ARect.TopLeft, False),
      FParentMetaFile.PointToFPoint(ARect.BottomRight, False));
    if Result.Left > Result.Right then
      SwapDoubles(Result.Left, Result.Right);
    if Result.Top > Result.Bottom then
      SwapDoubles(Result.Top, Result.Bottom);
  end
  else
    Result := cnstBadRect;
end;

procedure TsgRegion.MarkIsWorked;
var
  vClip: TsgRegionClip;
  I: Integer;
begin
  if not IsClipping then
    Exit;
  for I := 0 to FWorkset.Count - 1 do
  begin
    vClip := FWorkset[I];
    vClip.FIsWorkedClip := True;
  end;
end;

procedure TsgRegion.MakeClip(const AClipPath: TFPointList; const AClipMode: Integer;
  const ABlock: TsgDXFBlock);
//var
  //vClip: TsgRegionClip;
  //vIsEqual: Boolean;
begin
  FClip := MakeNewClip(AClipPath, AClipMode, nil, ABlock);
end;

procedure TsgRegion.MakeClip(const AClipRect: TFRect; const AClipMode: Integer;
  const ABlock: TsgDXFBlock);
var
  vClipBox: TFRect;
  //vClip: TsgRegionClip;
  //vIsEqual: Boolean;
begin
  vClipBox := AClipRect;
  NormFRect(vClipBox);
  FClip := MakeNewClip(vClipBox, AClipMode, nil, ABlock);
end;

procedure TsgRegion.InitFromRect(ARect: TRect; AClipMode: Integer; AOther: TsgRegion);
var
  vHRGN: HRGN;
  vRgnData: PRgnData;
  vBoundsRect: TFRect;
  vCount: Integer;
begin
  Clear;
  DoPureRect(ARect);
  FIsClipSet := True;
  if not AOther.IsClipping then
  begin
    vBoundsRect := MakeFRectForRect(ARect);
    MakeClip(vBoundsRect, AClipMode, nil);
    IsClipping := Assigned(FClip);
  end
  else
  begin
    vHRGN := CreateRectRgnIndirect(ARect);
    try
      vCount := GetRegionData(vHRGN, 0, nil);
      vRgnData := AllocMem(vCount);
      try
        GetRegionData(vHRGN, vCount, vRgnData);
        InitFromData(vRgnData, AClipMode);
      finally
        FreeMem(vRgnData);
      end;
    finally
      DeleteObject(vHRGN);
    end;
  end;
end;

procedure TsgRegion.InitFromData(APRgnData: PRgnData; AClipMode: Integer);
var
  vRgnDataHdr: TRgnDataHeader;
  vRectPrev: TRect;
  vPRect: PRect;
  vRightCoords, vRects, vPoints, vTmpList: TList;
  I, K, vRectCount: Cardinal;
  isAdded: Boolean;
  vRect: TRect;
  vBoundsRect: TFRect;
  vBounds: TFPointList;

  procedure DoNextPoint(const ARectTopPt, ARectBottomPt: TPoint; AListPoints: TList);
  var
    vPt: PFPoint;
    Pt1, Pt2: TPoint;
  begin
    New(vPt);
    Pt2 := ARectTopPt;
    FParentMetaFile.SetDPtoLP(Pt2);
    vPt^ := FParentMetaFile.PointToFPoint(Pt2);
    if (AListPoints.Count > 0) and IsEqualFPoints2D(PFPoint(AListPoints.Last)^, vPt^) then
    begin
      Dispose(vPt);
      Pt1 := ARectBottomPt;
      PFPoint(AListPoints.Last)^ := FParentMetaFile.PointToFPoint(Pt1);
    end
    else
    begin
      AListPoints.Add(vPt);
      New(vPt);
      Pt2 := ARectBottomPt;
      FParentMetaFile.SetDPtoLP(Pt2);
      vPt^ := FParentMetaFile.PointToFPoint(Pt2);
      AListPoints.Add(vPt);
    end;
  end;

begin
  Clear;
  IsClipping := False;
  if APRgnData = nil then
    Exit;
  vRgnDataHdr := TRgnDataHeader(APRgnData^.rdh);
  // Wrong region
  try
    if (vRgnDataHdr.nRgnSize=0) or ((4*SizeOf(DWORD)*vRgnDataHdr.nCount) <> vRgnDataHdr.nRgnSize) then
      Exit;
  except
    Exit;
  end;
  vRectCount := vRgnDataHdr.nCount;
  vRect := APRgnData^.rdh.rcBound;
  FParentMetaFile.SetDPtoLP(vRect.TopLeft);
  FParentMetaFile.SetDPtoLP(vRect.BottomRight);
  DoPureRect(vRect);
  vBoundsRect := MakeFRectForRect(vRect);
  vRightCoords := TList.Create;
  vRects := TList.Create;
  vPoints := TList.Create;
  try
    vRects.Add(TList.Create);
    TList(vRects[0]).Add(PRect(@APRgnData.Buffer[0]));
    for I := 1 to vRectCount - 1 do
    begin
      vPRect := PRect(@APRgnData.Buffer[I * SizeOf(TRect)]);
      isAdded := False;
      for K := 0 to vRects.Count - 1 do
      begin
        vRectPrev := PRect(TList(vRects[K]).Last)^;
        if (vPRect^.Top = vRectPrev.Bottom) and (vPRect^.Right > vRectPrev.Left)
          and (vPRect^.Left < vRectPrev.Right) then
        begin
          TList(vRects[K]).Add(vPRect);
          isAdded := True;
          Break;
        end;
      end;
      if not isAdded then
      begin
        vRects.Add(TList.Create);
        TList(vRects.Last).Add(vPRect);
      end;
    end;
    repeat
      if TList(vRects.First).Count <> 0 then
      begin
        vPoints.Add(TList.Create);
        vRightCoords.Add(TList.Create);
        for K := 0 to TList(vRects.First).Count - 1 do
        begin
          vPRect := PRect(TList(vRects.First)[K]);
          DoNextPoint(vPRect^.TopLeft, Point(vPRect^.Left, vPRect^.Bottom), vPoints.Last);
          DoNextPoint(Point(vPRect^.Right, vPRect^.Top), vPRect^.BottomRight, vRightCoords.Last);
        end;
      end;
      TList(vRects.First).Free;
      vRects.Delete(0);
    until vRects.Count = 0;
    vBounds := TFPointList.Create;
    try
      for I := 0 to vPoints.Count - 1 do
      begin
        vTmpList := vPoints[I];
        for K := 0 to vTmpList.Count - 1 do
          vBounds.Add(PFPoint(vTmpList[K])^);
      end;
      for I := vRightCoords.Count - 1 downto 0 do
      begin
        vTmpList := vRightCoords[I];
        for K := vTmpList.Count - 1 downto 0 do
          vBounds.Add(PFPoint(vTmpList[K])^);
      end;
      vBounds.Add(vBounds.First);
      MakeClip(vBounds, AClipMode, nil);
    finally
      vBounds.Free;
    end;
  finally
    vRects.Free;
    FreeRecordListOfList(vPoints);
    FreeRecordListOfList(vRightCoords);
  end;
  IsClipping := Assigned(FClip);
  FIsClipSet := True;
end;

procedure TsgRegion.InitFromPath(const APath: TFPointList; const AClipMode: Integer;
  const ABlock: TsgDXFBlock);
begin
  if APath.Count >= 2 then
  begin
    MakeClip(APath, AClipMode, ABlock);
    IsClipping := Assigned(FClip);
    FIsClipSet := True;
  end;
end;

function TsgRegion.IsRectNotBounds(const ARect: TRect): Boolean;
var
  vRect: TFRect;
begin
  vRect := MakeFRectByPoints(FParentMetaFile.PointToFPoint(ARect.TopLeft),
    FParentMetaFile.PointToFPoint(ARect.BottomRight));
  Result := IsRectNotBounds(vRect);
end;

function TsgRegion.IsRectNotBounds(const ARect: TFRect): Boolean;
begin
  if IsClipping then
    Result := not IsRectInsideRect(BoundsRect, ARect)
  else
    Result := False;
end;

function TsgRegion.IsValidClip: Boolean;
var
  vBounds: TFRect;
begin
  if FIsClipSet then
  begin
    Result := False;
    vBounds := GetBoundsRect;
    if IsBadRect(vBounds) then
      Exit;
    if IsZero(GetAreaOfRect2D(vBounds)) then
      Exit;
    Result := True;
  end
  else
    Result := True;
end;

procedure TsgRegion.CorrectEmptyClips;
var
  I,J,K: Integer;
  vIsEqual: Boolean;
  vInsert,vCombineInsert: TsgRegionClip;
  vGroups,vGroup,vTempStack: TList;
begin
  vGroups := TList.Create;
  try
    vTempStack := TList.Create;
    try
      {$IFDEF SGDEL_7}
      vTempStack.Assign(FLoadStack);
     {$ELSE}
      CopyLists(vTempStack, FLoadStack);
     {$ENDIF}
      while vTempStack.Count > 0 do
      begin
        vInsert := vTempStack[0];
        vGroup := TList.Create;
        vGroups.Add(vGroup);
        vGroup.Add(vInsert);
        vTempStack.Delete(0);
        I := 0;
        while vTempStack.Count > I do
        begin
          vCombineInsert := vTempStack[I];
          if vCombineInsert.Block = vInsert.Block then
          begin
            vGroup.Add(vCombineInsert);
            vTempStack.Delete(I);
          end
          else
            Inc(I);
        end;
      end;
    finally
      vTempStack.Free;
    end;
    for I := 0 to vGroups.Count - 1 do
    begin
      vGroup := vGroups[I];
      //vCombineInsert := nil;
      for J := 0 to vGroup.Count - 1 do
      begin
        vInsert := vGroup[J];
        if vInsert.IsWorkedClip and (vInsert.Block.Count = 0) then
        begin
          vCombineInsert := GetClip(vInsert.FilterBox, vIsEqual);
          if Assigned(vCombineInsert) then
          begin
            for K := 0 to vGroup.Count - 1 do
            begin
              vInsert := vGroup[K];
              vInsert.Block := vCombineInsert.Block;
            end;
            Break;
          end;
        end;
      end;
    end;
  finally
    FreeList(vGroups);
  end;
end;

function TsgRegion.GetBoundsRect: TFRect;
var
  vSpatialFiler: TsgCADSpatialFilter;
begin
  if Assigned(FClip) then
  begin
    vSpatialFiler := TsgCADSpatialFilter(FClip.Filter);
    Result := MakeFRectByPoints(vSpatialFiler.Bounds.First, vSpatialFiler.Bounds.Last)
  end
  else
    Result := cnstBadRect;
end;

function TsgRegion.GetClip(const AClipRect: TFRect;
  var AIsEqual: Boolean): TsgRegionClip;
var
  I,J: Integer;
  vInsert: TsgRegionClip;
  vDiagonal,vMinDiagonal: Double;
  vClip: TFRect;
  vBlock: TsgDXFBlock;
  vEntity: TsgDXFEntity;
begin
  AIsEqual := False;
  Result := nil;
  vMinDiagonal := MaxDouble;
  for I := 0 to FLoadStack.Count - 1 do
  begin
    vInsert := FLoadStack[I];
    vClip := cnstBadRect;
    vBlock := vInsert.Block;
    for J := 0 to vBlock.Count - 1 do
    begin
      vEntity := vBlock[J];
      UnionFRect(vClip, vEntity.Box);
    end;
    if not IsBadRect(vClip) and (IntersectFRect2D(vClip, AClipRect) >= 0) then
    begin
      vDiagonal := GetDiagonalLength(vClip);
      if vDiagonal < vMinDiagonal then
      begin
        vMinDiagonal := vDiagonal;
        Result := vInsert;
        AIsEqual := IsEqualFRects(vClip, AClipRect);
        if AIsEqual then
          Exit;
      end;
    end;
  end;
end;

function TsgRegion.GetCurClip: TsgRegionClip;
begin
  if IsClipping then
    Result := FClip
  else
    Result := nil;
end;

function TsgRegion.GetCurClipMode: Integer;
begin
  if Assigned(FClip) then
    Result := FClip.ClipMode
  else
    Result := -1;
end;

function TsgRegion.GetCurInsert: TsgDXFInsert;
begin
  if Assigned(FClip) then
    Result := FClip.Insert
  else
    Result := nil;
end;

function TsgRegion.GetOwnedInsert: TsgDXFInsert;
begin
  Result := nil;
  if Assigned(FClip) and Assigned(FClip.Owner) then
    Result := FClip.Owner.Insert;
end;

procedure TsgRegion.LoadInternal(AOwnedClip: TsgRegionClip);
var
  I: Integer;
  vInsert: TsgRegionClip;
  vConv: TsgDXFConverter;
begin
  vConv := FParentMetaFile.FCADImage.Converter;
  for I := FLoadStack.Count - 1 downto 0 do
  begin
    vInsert := FLoadStack[I];
    if vInsert.Owner = AOwnedClip then
    begin
      LoadInternal(vInsert);
      vInsert.Load(vConv);
    end;
  end;
end;

function TsgRegion.MakeNewClip(const AClipPath: TFPointList; const AClipMode: Integer;
  const AParentInsert: TsgRegionClip; const ABlock: TsgDXFBlock): TsgRegionClip;
var
  vCorrectPath: TFPointList;
  I: Integer;
begin
  vCorrectPath := GetCorrectContour(AClipPath);
  try
    if not IsSelfIntersectingPolyline(vCorrectPath) and not IsZero(GetAreaOfList(vCorrectPath)) then
    begin
      for I := 0 to vCorrectPath.Count - 1 do
        vCorrectPath[I] := ClipPointByFRect(FParentMetaFile.Bounds, vCorrectPath[I]);
      Result := TsgRegionClip.Create(vCorrectPath, AClipMode, AParentInsert,
        FParentMetaFile, ABlock);
      FLoadStack.Add(Result);
      ResetAndAddToWorkset(Result);
    end
    else
      Result := nil;
  finally
    vCorrectPath.Free;
  end;
end;

function TsgRegion.MakeNewClip(const AClipRect: TFRect; const AClipMode: Integer;
  const AParentInsert: TsgRegionClip; const ABlock: TsgDXFBlock): TsgRegionClip;
var
  vClipRect: TFRect;
begin
  if not IsZero(GetAreaOfRect2D(AClipRect)) then
  begin
    vClipRect := MakeFRectByPoints(
      ClipPointByFRect(FParentMetaFile.Bounds, AClipRect.TopLeft),
      ClipPointByFRect(FParentMetaFile.Bounds, AClipRect.BottomRight));
    Result := TsgRegionClip.Create(vClipRect, AClipMode, AParentInsert,
      FParentMetaFile, ABlock);
    FLoadStack.Add(Result);
    ResetAndAddToWorkset(Result);
  end
  else
    Result := nil;
end;

procedure TsgRegion.ResetAndAddToWorkset(AClip: TsgRegionClip);
begin
  if AClip.ClipMode = RGN_COPY then
    FWorkset.Clear;
  FWorkset.Add(AClip);
end;

procedure TsgRegion.SetCurClip(AValue: TsgRegionClip);
begin
  if Assigned(AValue) then
  begin
    FClip := TsgRegionClip.Create(AValue.Bounds, AValue.ClipMode, AValue.Owner,
      FParentMetaFile, AValue.ExternalBlock);
    FLoadStack.Add(FClip);
    FIsClipping := True;
    FIsClipSet := True;
  end
  else
  begin
    FIsClipping := False;
    FIsClipSet := False;
    FClip := nil;
  end;
end;

procedure TsgRegion.SetIsClipping(AValue: Boolean);
begin
  FIsClipping := AValue;
end;

{
procedure TsgRegion.SetTopClip(AClip: TsgRegionClip);
begin
  AClip.MoveToTop;
  FClip := AClip;
end;
}

{ TsgMetafileTextListItem }
constructor TsgMetafileTextListItem.Create(ARect: TFRect; const AText: string;
  AObject: TsgDXFText);
begin
  FRect := ARect;
  FText := AText;
  FTextObject := AObject;
end;

function TsgMetafileTextListItem.IsEqual(ARect: TFRect; const AText: string): Boolean;
var
  vEps: Double;
begin
  vEps := GetEpsForFRect(FRect);
  Result := IsEqualFPoints(FRect.TopLeft, ARect.TopLeft, vEps) and
    IsEqualFPoints(FRect.BottomRight, ARect.BottomRight, vEps) and
    sgSameText(FText, AText);
end;

{ TsgMetafileTextList }

constructor TsgMetafileTextList.Create(AImage: TsgCADImage);
var
  I: Integer;
  vEntity: TsgDXFEntity;
begin
  inherited Create;
  FImage := AImage;
  for I := 0 to AImage.CurrentLayout.Count - 1 do
  begin
    vEntity := AImage.CurrentLayout.Entities[I];
    if vEntity is TsgDXFText then
    begin
      AddItem(AImage, TsgDXFText(vEntity).Box, TsgDXFText(vEntity).Text,
        TsgDXFText(vEntity));
    end;
  end;
end;

destructor TsgMetafileTextList.Destroy;
begin
  ClearList(self);
  inherited Destroy;
end;

procedure TsgMetafileTextList.AddItem(AImage: TsgCADImage; ARect: TFRect;
  const AText: string; AObject: TsgDXFText);
begin
  if FImage = AImage then
    Add(TsgMetafileTextListItem.Create(ARect, AText, AObject));
end;

function TsgMetafileTextList.Find(AImage: TsgCADImage; ARect: TFRect;
  const AText: string): TsgMetafileTextListItem;
var
  I: Integer;
  vItem: TsgMetafileTextListItem;
begin
  Result := nil;
  if not cnstTestTextLayers then
    Exit;
  if FImage = AImage then
    for I := 0 to Count - 1 do
    begin
      vItem := Items[I];
      if vItem.IsEqual(ARect, AText) then
      begin
        Result := vItem;
        Exit;
      end;
    end;
end;

{ TsgDXFEntityTag }

constructor TsgDXFEntityTag.Create(AEntity: TsgDXFEntity; ATag: Integer = 0);
begin
  FEntity := AEntity;
  FTag := ATag;
end;

{ TsgConvertMetafileToCad }

constructor TsgConvertMetafileToCad.Create(const AImage: TsgCADImage;
  const AOwner: TsgObjectList; const AOwnerBox: TFRect; const AIndexName: string;
  AEncript,ADecript: TsgCriptMethod; AOnProgress: TProgressEvent = nil;
  ATextsList: IsgTextRectFinder = nil);
begin
  inherited Create;
  InitConverter(AImage, AOwner, AOwnerBox, AIndexName, AEncript, ADecript,
    AOnProgress, ATextsList);
end;

constructor TsgConvertMetafileToCad.Create(const AImage: TsgCADImage;
  const AOwner: TsgObjectList; const AOwnerBox: TFRect; const AIndexName: string;
  ATextsList: Pointer);
var
  vTextsListO: TObject;
  vTextsListI: IsgTextRectFinder;
begin
  inherited Create;
  vTextsListI := nil;
  vTextsListO := ATextsList;
  if not Assigned(vTextsListO) then
    vTextsListI := nil
  else
    if not vTextsListO.GetInterface(IsgTextRectFinder, vTextsListI) then
      vTextsListI := nil;
  InitConverter(AImage, AOwner, AOwnerBox, AIndexName, nil, nil, nil, vTextsListI);
end;

destructor TsgConvertMetafileToCad.Destroy;
var
  i: Integer;
  vGDIObj: PGDIObject;
begin
  FListEnts.Free;
  FListBlocks.Free;
  FListTextFigures.Free;
  FListTextContour.Free;
{$IFDEF SG_METAFILE_DEBUG}
  FLogMetafile.Free;
{$ENDIF}
  FClipRgn.Free;
  for i := 0 to FMapGDIObject.Count - 1 do
  begin
    vGDIObj := FMapGDIObject[i];
    if vGDIObj <> nil then
    begin
      if vGDIObj^.tpObject <> tgStandart then
      begin
        if (vGDIObj^.PData <> nil) and (vGDIObj^.PData <> @FDefaultBrush)
          and (vGDIObj^.PData <> @FDefaultPen) then
          Dispose(vGDIObj^.PData);
        vGDIObj^.PData := nil;
      end;
      Dispose(vGDIObj);
    end;
  end;
  FMapGDIObject.Free;
  FGDIObjectsSelect.Free;
  FDCStack.Free;
  FreeAndNil(FSHXResolver);
  FreeAndNil(FExtractedTexts);
  FParamsPath.Free;
  inherited;
end;

procedure TsgConvertMetafileToCad.AddContour(AList: TsgObjectList;
  ACADImage: TsgCADImage = nil);
var
  I: Integer;
  vEntity: TsgDXFEntity;
begin
  for I := 0 to Pred(AList.Count) do
  begin
    vEntity := TsgDXFEntity(AList[I]);
    if (not FFillingEntity) and IsEmptyColorCAD(vEntity.ColorCAD) then
      if IsEmptyColorCAD(FData.ColorCAD) then
        vEntity.ColorCAD := TranslateColor(FParamsDraw.BkColor, tgBrush)
      else
        vEntity.ColorCAD := FData.ColorCAD;
    AddEntity(ACADImage, vEntity, False, True);
  end;
end;

procedure TsgConvertMetafileToCad.AddImageEnt(APBitmapInfo: PBitmapInfo;
  AData: Pointer; APos,ASize,AEntSize: TPoint; ABkColor: TColor;
  AUsage: Integer; ARop: DWORD);
const
  //cnstImageVectSize = 295 * 300 / 25.4;  //A4 mm X dpi 300 / mm per inch
  cnstImageVectSize = 3600;
  cnstMaxRasterImageSize = 5000.0*5000.0;
var
  vBmp: TBitmap;
  vImageEnt: TsgDXFImageEnt;
  vBitCount: Integer;
  vSizeData: Integer;
  vLogBrush: TLogBrush;
  vRect: TRect;
  vEntBox: TFRect;
  vTransparentColor: TColor;
  vAngle: Double;
  vBurshColor: TColor;
  vQuadrant,vVertexNumber: Integer;
{$IFDEF SG_VECTORIZATION}
  vNewSize: TPoint;
  I: Integer;
{$ENDIF}

  function MoveCoord(ACoord: Integer; AOrientation: Integer): Integer;
  begin
    if AOrientation < 0 then
      Result := ACoord
    else
      Result := 0;
  end;

  function CreateAndInitBitmap(const AImgSize: TPoint; const ABitCount: Integer;
    const ABurshColor: TColor; const ALogBrush: TLogBrush;
    AStretchBltMode: Integer = -1): TBitmap;
  begin
    Result := TBitmap.Create;
    try
      Result.Canvas.Brush.Style := TBrushStyle(ALogBrush.lbStyle);
      if ((ABurshColor = clBlack) or (ABurshColor = clNone)) and Assigned(AData) and
        (ARop = SRCAND) then
        Result.Canvas.Brush.Color := IfThen(FParamsDraw.BkColor = clBlack, clWhite, FParamsDraw.BkColor)
      else
        Result.Canvas.Brush.Color := ABurshColor;
      Result.Width := AImgSize.X;
      Result.Height := AImgSize.Y;
      Result.PixelFormat := sgFunction.GetPixelFormat(ABitCount);
      if Result.PixelFormat in [pfDevice, pf15bit, pf16bit] then
        Result.PixelFormat := pf24bit;
      if AStretchBltMode > 0 then
        SetStretchBltMode(Result.Canvas.Handle, AStretchBltMode);
      Result.Canvas.FillRect(Rect(0, 0, AImgSize.X - 1, AImgSize.Y - 1));
      StretchDIBits(Result.Canvas.Handle, MoveCoord(AImgSize.X, AEntSize.X),
        MoveCoord(AImgSize.Y, AEntSize.Y), Sign(AEntSize.X)*AImgSize.X,
        Sign(AEntSize.Y)*AImgSize.Y, 0, 0, ASize.X, ASize.Y,
        AData, APBitmapInfo^, AUsage, ARop);
    except
      Result.Free;
      raise;
    end;
  end;

begin
  vLogBrush := TLogBrush(FParamsDrawBrush.PData^);
  vRect.TopLeft := APos;
  vRect.BottomRight := AddPoint(APos, AEntSize);
  vTransparentColor := ABkColor;
  if Assigned(APBitmapInfo) then
    vBitCount := APBitmapInfo^.bmiHeader.biBitCount
  else
    vBitCount := 0;
  vBurshColor := TColor(vLogBrush.lbColor);
  if (not Assigned(AData)) and ((vBurshColor = FDefaultBkColor) or
    (vBurshColor = clNone)) then
    Exit;
  if ARop = 0 then
    ARop := SRCCOPY;
  vSizeData := Round((Int64(ASize.X) * Int64(ASize.Y) * Int64(vBitCount)) / 8);

  if vSizeData > 0 then
  begin
    if (ASize.X = 1) and (ASize.Y = 1) then
    begin
      vBmp := CreateAndInitBitmap(ASize, vBitCount, vBurshColor, vLogBrush);
      try
        DrawPoint(APos, vBmp.Canvas.Pixels[0,0]);
      finally
        vBmp.Free;
      end;
    end
    else
    begin
      if (ASize.X * ASize.Y >= cnstMaxRasterImageSize) and not IsScanOfDocument(nil) then
        Exit;
      DoPureRect(vRect);
      vEntBox := MakeFRectByPoints(PointToFPoint(vRect.TopLeft),
        PointToFPoint(vRect.BottomRight));
      vAngle := -GetZAngleFormMatrix(FMatrix);
      vImageEnt := TsgDXFImageEnt.Create;
      vImageEnt.Transparency := True;
      vImageEnt.TransparentColor := vTransparentColor;
      vImageEnt.Point := GetRectPoint(vEntBox, vAngle, False, True, nil,
        vQuadrant, vVertexNumber);
      vImageEnt.Size := PointToFPoint(AEntSize, True);
      vImageEnt.UVector := MakeFPoint(Sign(AEntSize.X), 0, 0);
      vImageEnt.VVector := MakeFPoint(0, -Sign(AEntSize.Y), 0);
      TsgDXFImageEntAccess(vImageEnt).DoRotateVectors(vAngle);
      vImageEnt.ClippingBoundaryType := 0;
      {$IFDEF SG_VECTORIZATION}
        if ((VectorizationMode = pdfvmAuto) and IsScanOfDocument(vImageEnt)) or
          (VectorizationMode = pdfvmAll) then
        begin
          //I := MaxI(ASize.X, ASize.Y);
          I := MaxI(
            MaxI(Abs(FMetafileBox.Right - FMetafileBox.Left), ASize.X),
            MaxI(Abs(FMetafileBox.Bottom - FMetafileBox.Top), ASize.Y));
          vNewSize := PtXScalar(ASize, cnstImageVectSize / I);
          if cnstMakeFixSizeImage then
            vBmp := CreateAndInitBitmap(vNewSize, vBitCount, vBurshColor, vLogBrush, HALFTONE)
          else
          begin
            CorrectSkipSegment(ASize, vNewSize);
            vBmp := CreateAndInitBitmap(ASize, vBitCount, vBurshColor, vLogBrush, HALFTONE);
          end;
          try
            ApplyBitmapSmooth(vBmp);
            vImageEnt.SetImage(vBmp);
            if FClipRgn.IsClipping then
              AddLoopByImageNew(vImageEnt, FCADImage.Converter, FOwner, @FEntitiesBox,
                True, nil, FCADImage.CurrentLayout.Index, FClipRgn.CurInsert.Block)
            else
              AddLoopByImageNew(vImageEnt, FCADImage.Converter, FOwner, @FEntitiesBox,
                True, nil, FCADImage.CurrentLayout.Index);
            vImageEnt.Free;
          finally
            vBmp.Free;
          end;
        end
        else
        begin
      {$ENDIF}
          vBmp := CreateAndInitBitmap(ASize, vBitCount, vBurshColor, vLogBrush);
          try
            vImageEnt.IsMonoChrome := vBmp.Monochrome;
            vImageEnt.SetImage(vBmp);
            TsgDXFImageEntAccess(vImageEnt).ChangeGraphic;
            AddEntity(nil, vImageEnt);
            if FClipRgn.IsClipping then
              FIsImageInClip := True;
          finally
            vBmp.Free;
          end;
      {$IFDEF SG_VECTORIZATION}
        end;
      {$ENDIF}
    end;
  end
  else
  begin
    if FClipRgn.IsValidClip then
      CreateRectangle(vRect, ABkColor, True);
  end;
end;

procedure TsgConvertMetafileToCad.AddSHXTextEnt;
var
  vText: string;
  vFontFace: TsgFontFace;
  vBox: TFRect;
  vColor: TsgColorCAD;
  //vIndex: Integer;
  //vEntity: TsgDXFEntity;
begin
  if not cnstResolveSHX then
    Exit;
  FSHXResolver.BeginRead;
  //vIndex := FSHXResolver.TextIndex;
  while FSHXResolver.ReadText(vText, vFontFace.SHXFont, vBox,
    vColor, vFontFace.Rotation, vFontFace.ObliqueAngle) do
  begin
    vFontFace.Height := Round(Abs(vBox.Bottom - vBox.Top));
    {vEntity := }
    AddTextEnt(nil, vText, vBox, vFontFace, vColor, True);
    //vEntity.FIndex := vIndex;
    //vIndex := FSHXResolver.TextIndex;
  end;
end;

function TsgConvertMetafileToCad.AddTextEnt(ACADImage: TsgCADImage; const AText: string;
  ARect: TRect; AFontFace: TsgFontFace; AColor: TsgColorCAD; AIsResize: Boolean;
  AClip: TObject = nil): TsgDXFEntity;
var
  vCountChars: Integer;
  vPt1,vPt2: TFPoint;
  vRect: TFRect;
begin
  Result := nil;
  vCountChars := Length(AText);
  if vCountChars > 0 then
  begin
    vPt1 := PointToFPoint(Point(ARect.Left, ARect.Top));
    vPt2 := PointToFPoint(Point(ARect.Right, ARect.Bottom));
    vRect := MakeFRectByPoints(vPt1, vPt2);
    Result := AddTextEnt(ACADImage, AText, vRect, AFontFace,
      AColor, AIsResize, AClip);
  end;
end;

function TsgConvertMetafileToCad.AddTextEnt(ACADImage: TsgCADImage; const AText: string;
  const ARect: TFRect; const AFontFace: TsgFontFace; const AColor: TsgColorCAD;
  const AIsResize: Boolean; AClip: TObject = nil): TsgDXFEntity;
var
  vText: TsgDXFText;
  vConverter: TsgDXFConverter;
  vScale,vHeight: Double;
  vPoint: TFPoint;
  vBox: TFRect;
  I,vVertexNumber,vQuadrant: Integer;
  vSizes: TF2DPoint;

  function GetBoxOffset(const ARect1,ARect2: TFRect;
    const AQuadrant,AVertexNumber: Integer): TFPoint;

    function GetOffset(const AS1,AE1,AS2,AE2: Double; const AIsStart: Boolean): Double;
    begin
      if AIsStart then
        Result := AS1 - AS2
      else
        Result := AE1 - AE2;
    end;

  begin
    Result := MakeFPoint(
      GetOffset(ARect1.Left, ARect1.Right, ARect2.Left, ARect2.Right, AQuadrant in [0,3]),
      GetOffset(ARect1.Top, ARect1.Bottom, ARect2.Top, ARect2.Bottom, AVertexNumber in [0,1]));
  end;

  function CalcScale(const ARect1,ARect2: TFRect; const ARotation: Double): Double;

    function GetAverageSize(const ASizes: TF2DPoint; const ARot: Double): Double;
    var
      vSin,vCos: Extended;
    begin
      SinCos(Radian(ARot), vSin, vCos);
      Result := vCos * ASizes.Y - vSin * ASizes.X;
    end;

  var
    vSize1,vSize2: Double;
  begin
    vSize1 := GetAverageSize(GetSizeFRect2D(ARect1), ARotation);
    vSize2 := GetAverageSize(GetSizeFRect2D(ARect2), ARotation);
    Result := Abs(vSize1 / vSize2);
  end;

begin
  Result := nil;
  if ACADImage = nil then
    ACADImage := FCADImage;
  if Assigned(FExtractedTexts.Find(ACADImage, ARect, AText)) then
    Exit;
  vConverter := ACADImage.Converter;
  vText := TsgDXFText.Create;
  vText.Text := AText;
  vText.ColorCad := AColor;
  vText.Rotation := AFontFace.Rotation;
  vText.Style := GetTextStyle(AFontFace, ACADImage);
  if not IsZero(vText.Style.ObliqueAngle) then
    vText.ObliqueAngle := 0;
  if AIsResize and IsZero(vText.Style.FixedHeight) then
  begin
    vPoint := GetRectPoint(ARect, AFontFace.Rotation, AFontFace.SHXFont <> '',
      (FParamsDraw.TextAlign and (TA_BASELINE or TA_BOTTOM)) = 0,
      nil, vQuadrant, vVertexNumber);
    vText.Point := vPoint;
    vText.Height := 1;
    vConverter.Loads(vText);
    vBox := GetTextBox(vText, ACADImage);
    vScale := CalcScale(ARect, vBox, AFontFace.Rotation);
    vHeight := vText.Height;
    if AFontFace.FontFound then
      I := 4
    else
      I := 18;
    while (not sgIsZero(1 - vScale, 0.001)) and (I > 0) do
    begin
      vHeight := vScale * vHeight;
      vPoint := AddFPoint(vPoint, PtXScalar(GetBoxOffset(ARect, vBox,
        vQuadrant, vVertexNumber), vScale));
      vText.Height := vHeight;
      vText.Point := vPoint;
      vConverter.Loads(vText);
      vBox := GetTextBox(vText, ACADImage);
      vScale := CalcScale(ARect, vBox, AFontFace.Rotation);
      Dec(I);
    end;
  end
  else
  begin
    vText.Point := cnstFPointZero;
    vConverter.Loads(vText);
    vSizes := GetSizeFRect2D(vText.Box);
    vPoint := GetRectPoint(ARect, AFontFace.Rotation, AFontFace.SHXFont <> '',
      (FParamsDraw.TextAlign and (TA_BASELINE or TA_BOTTOM)) = 0,
      @vSizes, vQuadrant, vVertexNumber);
    vText.Point := vPoint;
  end;
  Result := AddEntity(ACADImage, vText, False, True, AClip);
  FExtractedTexts.AddItem(ACADImage, ARect, AText, TsgDXFText(Result));
end;

function TsgConvertMetafileToCad.ExtractText(const AList: TsgObjectList;
  const AIsContour: Boolean): Boolean;
var
  vTextItem: TsgTextRectListItem;
  vIsSubstring: Boolean;
  vResText: string;
begin
  Result := False;
  if FParamsPath.IsInitRect and not FBeginPath and cnstExtractTexts then
  begin
    vTextItem := nil;
    vIsSubstring := False;
    if Assigned(FTextsList) then
      vTextItem := FTextsList.FindItem(FParamsPath.PathFRect, vIsSubstring);
    if Assigned(vTextItem) then
    begin
      vResText := vTextItem.Text;
      if vIsSubstring then
      begin
        if cnstExtractSubstrings then
        begin
          AddTextFromSubstrings(vTextItem, AList, AIsContour);
          FParamsPath.ClearParams;
        end;
      end
      else
      begin
        if TestTextImage(AList, vTextItem, False, vResText) then
        begin
          Result := True;
          AddTextEnt(nil, vResText, FParamsPath.PathRect,
            vTextItem.FontFace, FData.ColorCAD, True);
          FParamsPath.ClearParams;
        end;
      end;
    end;
  end;
end;

function TsgConvertMetafileToCad.ClipPoint2DByRect(const ARect: TRect;
  const AP: TF2DPoint): TF2DPoint;
begin
  if FClipRgn.IsClipping then
    Result := AP
  else
    Result := MakeF2DPointFrom3D(ClipPointByRect(ARect, MakeFPointFrom2D(AP)));
end;

function TsgConvertMetafileToCad.CreateAndAddStyle(const ACADImage: TsgCADImage;
  const AName: string): TsgDXFStyle;
begin
  Result := TsgDXFStyle.Create;
  Result.Name := AName;
  ACADImage.Converter.Sections[csStyles].AddEntity(Result);
end;

function TsgConvertMetafileToCad.CreateBrush: THandle;
var
  vLogBrish: PLogBrush;
begin
  vLogBrish := FParamsDrawBrush.PData;
  Result := CreateBrushIndirect(vLogBrish^);
  if Result = 0 then
    Result := GetStandartGDIObject(FParamsDrawBrush.iHandle);
end;

function TsgConvertMetafileToCad.CreatePen: THandle;
var
  vLogPen: PsgLogPen;
begin
  Result := 0;
  vLogPen := FParamsDrawPen.PData;
  case FParamsDrawPen.tpObject of
    tgPen:
      Result := CreatePenIndirect(vLogPen^.lp);
    tgExtPen:
      begin
        if Length(vLogPen^.Styles) > 0 then
          Result := ExtCreatePen(vLogPen^.lp.lopnStyle, vLogPen^.lp.lopnWidth.X,
            vLogPen^.lb, Length(vLogPen^.Styles), @(vLogPen^.Styles[0]))
        else
          Result := ExtCreatePen(vLogPen^.lp.lopnStyle, vLogPen^.lp.lopnWidth.X,
            vLogPen^.lb, 0, nil);
      end;
  end;
  if Result = 0 then
    Result := GetStandartGDIObject(FParamsDrawPen.iHandle);
end;

procedure TsgConvertMetafileToCad.GDIObjectAdd(AObject: PGDIObject);
begin
  GDIObjectDelete(AObject^.iHandle);
  FMapGDIObject.Add(AObject);
end;

procedure TsgConvertMetafileToCad.GDIObjectDelete(AHandle: Integer);
var
  vGDIObject: PGDIObject;
begin
  vGDIObject := FindObject(FMapGDIObject, AHandle);
  if vGDIObject <> nil then
  begin
    FMapGDIObject.Remove(vGDIObject);
    if vGDIObject.PData = FParamsDrawBrush.PData then
      FParamsDrawBrush.PData := @FDefaultBrush;
    if vGDIObject.PData = FParamsDrawPen.PData then
      FParamsDrawPen.PData := @FDefaultPen;
    if vGDIObject.PData = FParamsText.PData then
      FParamsText.PData := @FDefaultFont;
    if Assigned(vGDIObject.PData) then
    begin
      Dispose(vGDIObject.PData);
      vGDIObject.PData := nil;
    end;
    Dispose(vGDIObject);
  end;
end;

function TsgConvertMetafileToCad.Convert(const AMetafile: TMetafile;
  const AOutBox: PFRect = nil): Boolean;
begin
  Result := True;
  try
    FCommandIndex := 1;
    FDC := CreateCompatibleDC(0);
    try
      FMetafile := AMetafile;
      FillChar(FMetaFileHeader, SizeOf(FMetaFileHeader), #0);
      DoOnProgress(psStarting, 0, 0);
      InitParamsOfContextDeviceValuesDefault;
      EnumEnhMetaFile(0, AMetafile.Handle, @MFProc, Self, Rect(0,0,0,0));
      if FCorrectonModelName and (FCADImage.Converter.Layouts[0].Name <> sModel) then
        FCADImage.Converter.Layouts[0].Name := sModel;
      DoOnProgress(psEnding, 0, 0);
      if AOutBox <> nil then
      begin
        AOutBox^ := FOwnerBox;
        UnionFRect(AOutBox^, FEntitiesBox);
      end;
      if FListEnts.Count > 0 then
        AddHatch;
      AddTextFromSubstrings(nil, nil, False);
      AddSHXTextEnt;
      FParamsPath.Clear(True);
      FClipRgn.Load;
      FCADImage.GetExtents;
      if not IsZero(cnstGlobalLTScaleRatio) then
        FCADImage.Converter.LTScale := GetAccuracyByBox(FCADImage.Extents, cnstGlobalLTScaleRatio);
      //UnionLinesAndPolylines(False);
      if FCADImage.CurrentLayout.Count > 0 then
        FCADImage.GetExtents;
      TsgCadImageAccess(FCADImage).SetDefaultPlotSettings(True);
    finally
      FClipRgn.Clear(True);
      FMetafile := nil;
      DeleteDC(FDC);
    end;
  except
    Result := False;
  end;
end;

function TsgConvertMetafileToCad.Convert(const AList: TStrings;
  const AOutBox: PFRect = nil): Boolean;
var
  I: Integer;
  vRect,vOut: TFRect;
  vLayerName: string;
begin
  Result := False;
  vOut := FOwnerBox;
  for I := 0 to AList.Count - 1 do
  begin
    vLayerName := AList.Strings[I];
    if Trim(vLayerName) = '' then
      vLayerName := FCADImage.Converter.HeadVarStruct.CLayer;
    FLayer := FCADImage.Converter.LayerByName(vLayerName);
    Result := Convert(TMetafile(AList.Objects[I]), @vRect);
    UnionFRect(vOut, vRect);
  end;
  if AOutBox <> nil then
    AOutBox^ := vRect;
end;

procedure TsgConvertMetafileToCad.FreeUnusedBlocks;
var
  I: Integer;
  vBlock: TsgDXFBlock;
begin
  I := 0;
  while FListBlocks.Count > I do
  begin
    vBlock := TsgDXFBlock(FListBlocks[I]);
    if vBlock.References.Count = 0 then
    begin
      FListBlocks.Delete(I);
      Converter.Sections[csBlocks].RemoveEntity(vBlock);
      vBlock.Free;
    end
    else
      Inc(I);
  end;
end;

function TsgConvertMetafileToCad.GetFillColor(const AColor: TColor; var AOutColor: TColor;
  var AOutColorCAD: TsgColorCAD): Boolean;
var
  vLogBrush: PLogBrush;
begin
  Result := False;
  if FUseBackground then
    AOutColor := FParamsDraw.BkColor
  else
  begin
    vLogBrush := FParamsDrawBrush.PData;
    if vLogBrush <> nil then
      AOutColor := vLogBrush^.lbColor
    else
    begin
      AOutColor := AColor;
      Result := True;
    end;
  end;
  AOutColorCAD := TranslateColor(AOutColor, tgBrush);
end;

{function TsgConvertMetafileToCad.AnalizJpeg(ADate: PByte): Boolean;
begin
  if AData = nil then
    Exit;
  while AData <> $D9 do
  begin
  end;
end;}

procedure TsgConvertMetafileToCad.InitParamsOfContextDeviceValuesDefault;
const
  cnstTahoma: WideString = 'Tahoma';
var
  vTm: TextMetric;
begin
  ZeroMemory(@FPenPos,SizeOf(FPenPos));

  ZeroMemory(@FParamsDraw,SizeOf(FParamsDraw));

  FParamsDraw.SizeViewPort.cx := 1;
  FParamsDraw.SizeViewPort.cy := 1;
  FParamsDraw.BeginViewPortCoord := Point(0, 0);
  FParamsDraw.SizeWindow.cx := 1;
  FParamsDraw.SizeWindow.cy := 1;
  FParamsDraw.BeginWindowCoord := Point(0, 0);

  FParamsDraw.Rop := GetRop2(FDC);
  FParamsDraw.BkMode := GetBkMode(FDC);
  FParamsDraw.PolyFillMode := GetPolyFillMode(FDC);
  FParamsDraw.BkColor := GetBkColor(FDC);
  FParamsDraw.sBltMode := GetStretchBltMode(FDC);
  FParamsDraw.ArcDirection := GetArcDirection(FDC);
  FParamsDraw.TextColor := GetTextColor(FDC);
  FParamsDraw.TextAlign := GetTextAlign(FDC);

  ZeroMemory(@FParamsDrawPen, SizeOf(FParamsDrawPen));
  FParamsDrawPen.tpObject := tgStandart;
  FDefaultPen.lp.lopnStyle := PS_SOLID and PS_GEOMETRIC;
  FDefaultPen.lp.lopnWidth.X := 1;
  FDefaultPen.lp.lopnColor := GetDCPenColor(FDC);
  FParamsDrawPen.PData := @FDefaultPen;

  ZeroMemory(@FParamsDrawBrush, SizeOf(FParamsDrawBrush));
  FParamsDrawBrush.tpObject := tgStandart;
  FDefaultBrush.lbStyle := BS_SOLID;
  FDefaultBrush.lbColor := GetDCBrushColor(FDC);
  FDefaultBrush.lbHatch := 0;
  FParamsDrawBrush.PData := @FDefaultBrush;

  FDefaultBkColor := GetBkColor(FDC);

  ZeroMemory(@FParamsText,SizeOf(FParamsText));
  GetTextMetrics(FDC,vTm);
  ZeroMemory(@FDefaultFont, SizeOf(FDefaultFont));
  FDefaultFont.lfHeight := -11;
  FDefaultFont.lfWeight := 400;
  Move(cnstTahoma[1], FDefaultFont.lfFaceName, Length(cnstTahoma) shl 1);
  FParamsText.PData := @FDefaultFont;

  FKoefMapMode := -1;
  GetWorldTransform(FDC,FEXForm);
  SetMatrix(FEXForm);
end;

function TsgConvertMetafileToCad.GetStandartGDIObject(const AIHandle: DWORD): DWORD;
var
  vHandle: THandle;
begin
  Result := DecodeStdObject(AIHandle);
  if Result <> 0 then
  begin
    vHandle := GetStockObject(Result);
    if vHandle > 0 then
      Result := vHandle;
  end;
end;

procedure TsgConvertMetafileToCad.DoHeader(P: PEnhMetaHeader);
var
  vOwnSize, vExts: TF2DPoint;
  vMFRatioX, vMFRatioY: Double;
begin
  FMetaFileHeader := P^;
  FMetafileBox := FMetaFileHeader.rclBounds;
  vExts := GetSizeFRect2D(MakeFRect2D(FMetafileBox.Left, FMetafileBox.Top,
    FMetafileBox.Right, FMetafileBox.Bottom));
  if IsEqualFPoints(FOwnerBox.TopLeft, FOwnerBox.BottomRight) then
  begin
    if FScale = 0 then
      vOwnSize := GetMetafileSize(FMetafile)
    else
      vOwnSize := Pt2XScalar(vExts, FScale);
  end
  else
    if FScale <> 0 then
      vOwnSize := Pt2XScalar(GetSizeFRect2D(FOwnerBox), FScale)
    else
      vOwnSize := GetSizeFRect2D(FOwnerBox);
  if vExts.X <> 0 then
  begin
    FKoefScales.X := vOwnSize.X / vExts.X;
    if vExts.Y <> 0 then
      FKoefScales.Y := vOwnSize.Y / vExts.Y
    else
      FKoefScales.Y := FKoefScales.X;
  end
  else
  begin
    if vExts.Y <> 0 then
    begin
      FKoefScales.Y := vOwnSize.Y / vExts.Y;
      FKoefScales.X := FKoefScales.Y;
    end
    else
    begin
      FKoefScales.Y := 1;
      FKoefScales.X := 1;
    end;
  end;
  FOffsetPointInternal := MakeF2DPointFrom3D(PointToFPoint(FMetafileBox.TopLeft, True));
  FBounds := MakeFRectByPoints(
    PointToFPoint(FMetafileBox.TopLeft),
    PointToFPoint(FMetafileBox.BottomRight));
  if UseMetafileRatio then
  begin
    vMFRatioX := FMetafile.Width / (FMetaFileHeader.rclFrame.Right - FMetaFileHeader.rclFrame.Left);
    vMFRatioY := FMetafile.Height / (FMetaFileHeader.rclFrame.Bottom - FMetaFileHeader.rclFrame.Top);
    FMetafileXRatio := vMFRatioX / vMFRatioY;
  end;
end;

procedure TsgConvertMetafileToCad.DoEllipse(P: PEMREllipse);
var
  vEllipseClass: TsgDXFEntityClass;
  vRect: TFRect;
  vDiagonal1,vDiagonal2: Double;
  vCenter: TFPoint;
  vRadPt: TFPoint;
begin
  SetPen;
  SetHatch(FParamsDraw.BkColor);
  vRect.TopLeft := PointToFPoint(P^.rclBox.TopLeft);
  vRect.BottomRight := PointToFPoint(P^.rclBox.BottomRight);
  FData.StartAngle := 0;
  FData.EndAngle := 360;
  vDiagonal1 := Abs(vRect.Top - vRect.Bottom) / 2;
  vDiagonal2 := Abs(vRect.Right - vRect.Left) / 2;
  vCenter := MiddleFPoint(vRect.TopLeft, vRect.BottomRight);
  FData.Point := vCenter;
  if vDiagonal1 = vDiagonal2 then
  begin
    vEllipseClass := TsgDXFCircle;
    FData.Radius := vDiagonal1;
  end
  else
  begin
    vEllipseClass := TsgDXFEllipse;
    if vDiagonal1 < vDiagonal2 then
    begin
      vRadPt := MakeFPoint(vDiagonal2, 0);
      FData.Ratio := vDiagonal1 / vDiagonal2;
    end else
    begin
      vRadPt := MakeFPoint(0, vDiagonal1);
      FData.Ratio := vDiagonal2 / vDiagonal1;
    end;
    FData.Radius := Sqrt(vRadPt.X * vRadPt.X + vRadPt.Y * vRadPt.Y);
    FData.Point1 := vRadPt;
  end;
  InitAndAddEntity(nil, vEllipseClass, IsFillContour(True));
end;

{procedure TsgConvertMetafileToCad.DoEllipse(P: PEMREllipse);
var
  vEllipse: TsgDXFEllipse;
  vCountPoints: Integer;
  vRect: TRect;
  vDiagonal1,vDiagonal2: Double;
  vCenter: TFPoint;
  I,J: Integer;
begin

  if TLogPen(FParamsDrawPen.PData^).lopnStyle = ps_Null then
    Exit;

  vRect := P^.rclBox;
  if FRegion > 0 then
  begin
    if RectInRegion(FRegion,vRect) then
      InsideRegion(vRect);
    Exit;
  end;

  vDiagonal1 := Abs(vRect.Top - vRect.Bottom);
  vDiagonal2 := Abs(vRect.Right - vRect.Left);

  vCenter :=
    MiddleFPoint(PointToFPoint(vRect.TopLeft), PointToFPoint(vRect.BottomRight));
  if vDiagonal1 = vDiagonal2 then
  begin
    TsgDXFCircle(vEllipse) := TsgDXFCircle.Create;
    vEllipse.Radius := vDiagonal1 / 2;
  end
  else
  begin
    vEllipse := TsgDXFEllipse.Create;
    if vDiagonal1 < vDiagonal2 then
    begin
      vEllipse.RadPt := PointToFPoint(Point(vRect.Left,Round(vCenter.Y)));
      vEllipse.Ratio := vDiagonal1 / vDiagonal2;
    end else
    begin
      vEllipse.RadPt := PointToFPoint(Point(Round(vCenter.X),vRect.Top));
      vEllipse.Ratio := vDiagonal2 / vDiagonal1;
    end;
  end;
  vEllipse.Point := vCenter;
  AddEntity(vEllipse,True);
end;}

function TsgConvertMetafileToCad.AddLine(const APt1,APt2: TFPoint;
  ATryToFill: Boolean = False; AHatch: Boolean = False): TsgDXFEntity;
begin
  Result := nil;
  if IsEqualFPoints(APt1, APt2) then
    Exit;
  SetPen;
  FData.Point := APt1;
  FData.Point1 := APt2;
  Result := InitAndAddEntity(nil, TsgDXFLine, ATryToFill, AHatch);
end;

procedure TsgConvertMetafileToCad.AddRectangle(const ARect: TFRect;
  AIsSolid: Boolean = False);
begin
  if AIsSolid then
  begin
    FData.Point := MakeFPoint(ARect.Left, ARect.Top);
    FData.Point1 := MakeFPoint(ARect.Right, ARect.Top);
    FData.Point2 := MakeFPoint(ARect.Left, ARect.Bottom);
    FData.Point3 := MakeFPoint(ARect.Right, ARect.Bottom);
  end
  else
  begin
    FData.Points.Add(MakeFPoint(ARect.Left, ARect.Top));
    FData.Points.Add(MakeFPoint(ARect.Right, ARect.Top));
    FData.Points.Add(MakeFPoint(ARect.Right, ARect.Bottom));
    FData.Points.Add(MakeFPoint(ARect.Left, ARect.Bottom));
  end;
end;

function TsgConvertMetafileToCad.CreateRectangle(const ARect: TRect;
  AColor: TColor; AIsSolid: Boolean = False): TsgDXFEntity;
begin
  Result := CreateRectangle(MakeFRectByPoints(PointToFPoint(ARect.TopLeft),
    PointToFPoint(ARect.BottomRight)), AColor, AIsSolid);
end;

function TsgConvertMetafileToCad.CreateRectangle(const ARect: TFRect;
  AColor: TColor; AIsSolid: Boolean = False;
  ATryToFill: Boolean = True;
  AHatch: Boolean = False): TsgDXFEntity;
var
  vPoints: TFPointList;
begin
  SetPen;
  if ATryToFill and not AHatch then
    SetHatch(AColor);
  if AIsSolid then
  begin
    AddRectangle(ARect, True);
    Result := InitAndAddEntity(nil, TsgDXFSolid, False, True);
  end
  else
  begin
    vPoints := TFPointList.Create;
    try
      SetPolyline(vPoints, 4, True);
      AddRectangle(ARect, False);
      Result := InitAndAddEntity(nil, TsgDXFLWPolyline, IsFillContour(ATryToFill), AHatch);
    finally
      vPoints.Free;
    end;
  end;
end;

procedure TsgConvertMetafileToCad.MyLineTo(const APt1, APt2: TPoint; IsTo: Boolean = True);
begin
  AddLine(PointToFPoint(APt1), PointToFPoint(APt2));
  if isTo then
    SetPosition(APt2);
end;

procedure TsgConvertMetafileToCad.DoLineTo(P: PEMRLineTo);
begin
  MyLineTo(FPenPos,P^.ptl);
end;

procedure TsgConvertMetafileToCad.DoMoveTo(P: PEMRLineTo);
begin
  SetPosition(P^.ptl);
end;

procedure TsgConvertMetafileToCad.AddHatch;
begin
  if FListEnts.Count > 0 then
    if (FParamsPath.PointCount >= 8) and ExtractText(FListEnts, False) then
      TsgObjectList.ClearList(FListEnts, True)
    else
    begin
      if (not FFillingEntity) or IsDrawContour(FListEnts, False) then
      begin
        DoAddHatch(FListEnts, True);
        AddContour(FListEnts);
        FListEnts.Clear;
      end
      else
      begin
        DoAddHatch(FListEnts, True);
        TsgObjectList.ClearList(FListEnts, True);
      end;
    end;
end;

procedure TsgConvertMetafileToCad.AddOneHatch;
begin
  AddHatch;
end;

procedure TsgConvertMetafileToCad.AddTextFromSubstrings(
  const ATextItem: TsgTextRectListItem; const AFigures: TsgObjectList;
  const AIsContour: Boolean);
var
  I: Integer;
  vItem: TsgDXFEntity;
  vFigures: TsgObjectList;
  vBoundBox: TFRect;
  vResText: string;
  vClipOrBlock: TObject;

  procedure SetLastSubstrValues;
  begin
    FLastSubstringItem := ATextItem;
    FLastSubstringClip := FClipRgn.CurClip;
  end;

begin
  if Assigned(FLastSubstringItem) then
  begin
    if FLastSubstringItem <> ATextItem then
    begin
      vResText := FLastSubstringItem.Text;
      if TestTextImage(FListTextFigures, FLastSubstringItem, True, vResText, @vBoundBox) then
      begin
        if Assigned(FLastSubstringClip) then
          vClipOrBlock := FLastSubstringClip
        else
          vClipOrBlock := FCADImage.CurrentLayout.PaperSpaceBlock;
        AddTextEnt(nil, vResText, vBoundBox, FLastSubstringItem.FontFace,
          FLastSubstringColor, True, vClipOrBlock);
        TsgObjectList.ClearList(FListTextContour, True);
      end
      else
      begin
        DoAddHatch(FListTextFigures, True);
        AddContour(FListTextContour);
        FListTextContour.Clear;
      end;
      TsgObjectList.ClearList(FListTextFigures, True);
      SetLastSubstrValues;
    end
  end
  else
  begin
    if Assigned(AFigures) then
    begin
      vResText := ATextItem.Text;
      if TestTextImage(AFigures, ATextItem, False, vResText, @vBoundBox) then
      begin
        AddTextEnt(nil, vResText, vBoundBox, ATextItem.FontFace, FData.ColorCAD,
          True, FCADImage.CurrentLayout.PaperSpaceBlock);
        TsgObjectList.ClearList(AFigures);
        Exit;
      end
      else
        SetLastSubstrValues;
    end
    else
      SetLastSubstrValues;
  end;
  if Assigned(AFigures) then
  begin
    if FListTextFigures.Count = 0 then
    begin
      FLastSubstringRect := FParamsPath.PathRect;
      FLastSubstringColor := FData.ColorCAD;
    end
    else
      UnionRect(FLastSubstringRect, FLastSubstringRect, FParamsPath.PathRect);
    vFigures := TsgObjectList.Create;
    try
      vFigures.Assign(AFigures);
      for I := 0 to vFigures.Count - 1 do
      begin
        vItem := TsgDXFEntity(vFigures[I]);
        AFigures.Remove(vItem);
        FParamsPath.Remove(vItem);
        if AIsContour then
          FListTextContour.Add(vItem)
        else
          FListTextFigures.Add(vItem);
      end;
    finally
      vFigures.Free;
    end;
  end;
end;

{procedure TsgConvertMetafileToCad.AddOneSolid(const ARect: TRect);
var
  vSolid: TsgDXFSolid;
begin
  vSolid := TsgDxfSolid.Create;
  vSolid.Point2 := PointToFPoint(ARect.TopLeft);
  vSolid.Point3 := PointToFPoint(ARect.BottomRight);
  vSolid.ColorCAD := ConvertColortoColorCAD(FParamsDraw.Color);
  AddEntity(vSolid);
end;}

function TsgConvertMetafileToCad.GetConverter: TsgDXFConverter;
begin
  if Assigned(FCADImage) then
    Result := FCADImage.Converter
  else
    Result := nil;
end;

function TsgConvertMetafileToCad.InitAndAddEntity(ACADImage: TsgCADImage;
  AEntityClass: TsgDXFEntityClass; ATryToFill: Boolean = False;
  AHatch: Boolean = False): TsgDXFEntity;
var
  vFillContour: Boolean;

  function CreateAndAddEntity(AFill: Boolean): TsgDXFEntity;
  var
    vEntity: TsgDXFEntity;
  begin
    vEntity := AEntityClass.Create;
    //vEntity.FIndex := FCommandIndex;
    InitEntity(vEntity);
    AddEntity(ACADImage, vEntity, AFill, AHatch);
    Result := vEntity;
    if cnstResolveSHX then
      FSHXResolver.AddEntity(vEntity);
  end;

begin
  Result := nil;
  vFillContour := IsSolidContour(ATryToFill);
  if FBeginPath or AHatch or vFillContour or (not IsEmptyColorCAD(FData.Color)) then
    Result := CreateAndAddEntity(vFillContour);
end;

procedure TsgConvertMetafileToCad.InitConverter(const AImage: TsgCADImage;
  const AOwner: TsgObjectList; const AOwnerBox: TFRect; const AIndexName: string;
  AEncript,ADecript: TsgCriptMethod; AOnProgress: TProgressEvent;
  ATextsList: IsgTextRectFinder);
var
  vSrcDefStyle: TsgDXFEntity;
  vDefStyleName: string;
  vDefConverter: TsgDXFConverter;
begin
  FUseMetafileRatio := cnstUseMetafileRatio;
  FMetafileXRatio := 1;
  FBounds := cnstBadRect;
  FDefaultStyle := nil;
  FMetafile := nil;
  FLastSubstringItem := nil;
  FLastSubstringClip := nil;
  FOffsetPointInternal := cnstF2DPointZero;
  FListTextFigures := TsgObjectList.Create;
  FListTextContour := TsgObjectList.Create;
  FIsImageInClip := False;
  FTextsList := ATextsList;
  FEntitiesBox := cnstBadRect;
  FOwnerBox := AOwnerBox;
  FCADImage := AImage;
  TsgDXFBlockRecordsAccess(FCADImage.Converter.Sections[csBlockRecords]).Blocks :=
    FCADImage.Converter.Sections[csBlocks];
  FOwner := AOwner;
  {if not Assigned(FOwner) then
    FOwner := FCADImage.CurrentLayout;}
  FBeginPath := False;
  FParamsPath := TsgParamsPath.Create(Self);
  FCurrentEntity := nil;
  if cnstResolveSHX then
    FSHXResolver := TsgSHXResolver.Create(AImage.Converter, AIndexName,
      AEncript, ADecript)
  else
    FSHXResolver := nil;
  //FAddFigureInList := False;
  FExtractedTexts := TsgMetafileTextList.Create(AImage);
  FListEnts := TsgObjectList.Create;
  FListBlocks := TsgObjectList.Create;
  FClipRgn := TsgRegion.Create(Self);
  SetRect(FRegionRect,0,0,0,0);
  FDrawedPath := False;
{$IFDEF SG_METAFILE_DEBUG}
  FLogMetafile := TStringList.Create;
{$ENDIF}
  FMapGDIObject := TList.Create;
  FGDIObjectsSelect := TList.Create;
  FParamsDraw.MapMode := MM_TEXT;
  FKoefMapMode := -1;
  FParamsDraw.SizeViewPort.cx := 1;
  FParamsDraw.SizeViewPort.cy := 1;
  FParamsDraw.BeginViewPortCoord := Point(0, 0);
  FParamsDraw.SizeWindow.cx := 1;
  FParamsDraw.SizeWindow.cy := 1;
  FParamsDraw.BeginWindowCoord := Point(0, 0);
  FUseBackground := False;
  FKoefPenSize := cnstDefaultKoefPenSize;
  FDCStack := TObjectList.Create;
  FDCStack.OwnsObjects := True;
  FillChar(FDefaultBrush, SizeOf(FDefaultBrush), 0);
  FillChar(FDefaultPen, SizeOf(FDefaultPen), 0);
  FEnableFitProportional := cnstEnableFitProportional;
  FKoefScales := MakeF2DPoint(1, 1);
  FScale := 0;
  FUse01MM := True;
  FAlternateBlack := False;
  FFillingEntity := cnstFillingEntity;
  FUnitSize := 1.0;
  FOffsetPoint := cnstF2DPointZero;
  FLayer := AImage.Converter.LayerByName(AImage.Converter.HeadVarStruct.CLayer);
  FOnProgress := AOnProgress;
  FCorrectonModelName := True;
  if Assigned(vVectorizationTextStyleImage) then
  begin
    vDefConverter := vVectorizationTextStyleImage.Converter;
    vDefStyleName := vDefConverter.HeadVarStruct.TextStyle;
    vSrcDefStyle := vDefConverter.Sections[csStyles].FindEntByName(vDefStyleName);
    if Assigned(vSrcDefStyle) then
    begin
      FDefaultStyle := TsgDXFStyle(Converter.Sections[csStyles].FindEntByName(vDefStyleName));
      if not Assigned(FDefaultStyle) then
        FDefaultStyle := CreateAndAddStyle(FCADImage, vDefStyleName);
      FDefaultStyle.AssignEntity(vSrcDefStyle);
      Converter.Loads(FDefaultStyle);
    end;
  end;
end;

procedure TsgConvertMetafileToCad.InitEntity(AEntity: TsgDXFEntity);
const
  cnstAmount = 4;
var
  i, j, vCount, vPoints: Integer;
  vNumberOfControlPoints: Integer;
  vKnot, vInc: Single;
  vClosed: Boolean;
begin
  AEntity.LineType := FData.LineType;
  if IsEqual(FData.Thickness, 0) then
    AEntity.LineWeight := fDXFLineWeightByLayer
  else
    AEntity.LineWeight := FData.Thickness;
  AEntity.ColorCAD := FData.Color;
  if AEntity is TsgDXFCustomVertex then
    TsgDXFCustomVertex(AEntity).Point := FData.Point;
  if AEntity is TsgDXFLine then
    TsgDXFLine(AEntity).Point1 := FData.Point1;
  if AEntity is TsgDXFPolyline then
  begin
    vClosed := FData.Closed;
    vCount := FData.Points.Count - 1;
    while (vCount >= 2) and IsEqualFPoints(FData.Points[0], FData.Points[vCount]) do
    begin
      vClosed := True;
      Dec(vCount);
    end;
    for i := 0 to vCount do
      AddVertexInPolyline(TsgDXFPolyline(AEntity), FData.Points[i]);
    TsgDXFPolyline(AEntity).Closed := vClosed;
  end;
  if AEntity is TsgDXFCircle then
    TsgDXFCircle(AEntity).Radius := FData.Radius;
  if AEntity is TsgDXFArc then
  begin
    TsgDXFArc(AEntity).StartAngle := FData.StartAngle;
    TsgDXFArc(AEntity).EndAngle := FData.EndAngle;
  end;
  if AEntity is TsgDXFEllipse then
  begin
    TsgDXFEllipse(AEntity).Ratio := FData.Ratio;
    TsgDXFEllipse(AEntity).RadPt := FData.Point1;
  end;
  if AEntity is TsgDXFSpline then
  begin
    TsgDXFSpline(AEntity).BeginningTangent := FData.Point;
    TsgDXFSpline(AEntity).EndingTangent := FData.Point;
    TsgDXFSpline(AEntity).Degree := 3;
    vNumberOfControlPoints := FData.Points.Count - 1 + FData.Points.Count div 3;
    vInc := 1.0 / vNumberOfControlPoints;
    i := 0;
    vKnot := 0;
    while i < vNumberOfControlPoints do
    begin
      for j := 0 to cnstAmount - 1 do
        TsgDXFSpline(AEntity).Knots.Add(vKnot);
      vKnot := vKnot + vInc;
      i := i + cnstAmount;
    end;
    vKnot := 1;
    for j := 0 to cnstAmount - 1 do
      TsgDXFSpline(AEntity).Knots.Add(vKnot);
    vPoints := Pred(FData.Points.Count);
    for I := 0 to vPoints do
    begin
      TsgDXFSpline(AEntity).Controls.Add(FData.Points[i]);
      if (i mod 3 = 0) and (i <> 0) and (i <> vPoints) then
        TsgDXFSpline(AEntity).Controls.Add(FData.Points[i]);
    end;
    I := TsgDXFSpline(AEntity).Controls.Count + 4;
    while I < TsgDXFSpline(AEntity).Knots.Count do
    begin
      TsgDXFSpline(AEntity).Controls.Add(FData.Points[vPoints]);
      Inc(I);
    end;
    TsgDXFSplineAccess(AEntity).GetSpline(Converter.NumberOfPartsInSpline);
  end;
  if AEntity is TsgDXFSolid then
  begin
    TsgCADCurvePolygon(AEntity).ColorCAD := FData.ColorCAD;
    TsgDXFSolid(AEntity).Point2 := FData.Point2;
    TsgDXFSolid(AEntity).Point3 := FData.Point3;
  end;
  if AEntity is TsgCADCurvePolygon then
  begin
    TsgCADCurvePolygon(AEntity).ColorCAD := FData.ColorCAD;
    TransformToCorrectBoundary(FData.Boundary, Converter);
    TsgCurvePolygonAccess(AEntity).GenerateListOfBoundaries(FData.Boundary);
    TsgCurvePolygonAccess(AEntity).Loaded(Converter);
  end;
  if AEntity is TsgCADHatch then
    TsgCADHatch(AEntity).FillStyle := FData.FillStyle;
end;

function TsgConvertMetafileToCad.IsFillContour(AFill: Boolean): Boolean;
begin
  Result := AFill;
end;

function TsgConvertMetafileToCad.IsFitProportional: Boolean;
begin
  Result := Assigned(FTextsList) and FEnableFitProportional;
end;

function TsgConvertMetafileToCad.IsTransparentHatch: Boolean;
var
  vClipMode: Integer;
begin
  Result := False;
  if cnstCheckTransparent then
  begin
    vClipMode := FClipRgn.CurClipMode;
    if (vClipMode > 0) and (vClipMode < RGN_COPY) or (vClipMode < 0) then
      Result := not(IsEmptyColorCAD(FData.ColorCAD) or IsBlackWhiteColorCAD(FData.ColorCAD));
  end;
end;

function TsgConvertMetafileToCad.KoefScale: Double;
begin
  Result := Min(FKoefScales.X, FKoefScales.Y);
end;

function TsgConvertMetafileToCad.KoefScaleX: Double;
begin
  if IsFitProportional then
    Result := FKoefScales.X
  else
    Result := KoefScale;
end;

function TsgConvertMetafileToCad.KoefScaleY: Double;
begin
  if IsFitProportional then
    Result := FKoefScales.Y
  else
    Result := KoefScale;
end;

function TsgConvertMetafileToCad.SetCircle(const R: TRect): Boolean;
var
  vRect: TRect;
  PX, PY: TFPoint;
  vExpandedRect: TFRect;
  vDiagonal1,vDiagonal2: Double;

  function GetFPoint(const AX, AY: Integer): TFPoint;
  begin
    Result := PointToFPoint(Point(AX, AY));
  end;

  function GetFRect(const ARct: TRect): TFRect;
  begin
    Result := MakeFRect(1E20, -1E20, 1E20, -1E20, 1E20, -1E20);// BadRect
    ExpandFRect(Result, GetFPoint(ARct.Left, ARct.Top));
    ExpandFRect(Result, GetFPoint(ARct.Left, ARct.Bottom));
    ExpandFRect(Result, GetFPoint(ARct.Right, ARct.Bottom));
    ExpandFRect(Result, GetFPoint(ARct.Right, ARct.Top));
  end;

  function VectorLength(AFPoint: TFPoint): Extended;
  begin
    try
      Result := Sqrt(AFPoint.X*AFPoint.X + AFPoint.Y*AFPoint.Y);
    except
      Result := 0.0;
    end;
  end;

  procedure DoAxes(AX, AY: Double);
  begin
    PX := PointToFPoint(AX, 0, True);
    PY := PointToFPoint(0, AY, True);
    //PX := MakeFPoint(AX * FMatrix.V1[0], FKoefMapMode * AX * FMatrix.V1[1], 0);
    //PY := MakeFPoint(AY * FMatrix.V2[0], FKoefMapMode * AY * FMatrix.V2[1], 0);
  end;

begin
  vRect := R;
  Result := True;
  vExpandedRect := GetFRect(vRect);
  vDiagonal1 := Abs(vExpandedRect.Top - vExpandedRect.Bottom) / 2;
  vDiagonal2 := Abs(vExpandedRect.Right - vExpandedRect.Left) / 2;
  FData.Point.X := (vExpandedRect.Left + vExpandedRect.Right) / 2;
  FData.Point.Y := (vExpandedRect.Top + vExpandedRect.Bottom) / 2;
  FData.Point.Z := 0;
  DoAxes(Abs(vRect.Right - vRect.Left) / 2, Abs(vRect.Bottom - vRect.Top) / 2);
  FData.Point2 := MakeFPoint(VectorLength(PX), VectorLength(PY), 0);
  if Abs(FData.Point2.X-FData.Point2.Y) > 1 then
  begin  // ellipse (accuracy for integer GDI-coordinates)
    Result := False;
    if FData.Point2.X > FData.Point2.Y then
    begin
      FData.Point1 := PX;
      FData.Radius := FData.Point2.Y/FData.Point2.X;
    end
    else
    begin
      FData.Point1 := PY;
      FData.Radius := FData.Point2.X/FData.Point2.Y;
    end;
    FData.Ratio := Min(vDiagonal1,vDiagonal2) / Max(vDiagonal1,vDiagonal2);
  end
  else // circle
    FData.Radius := FData.Point2.X;
  FData.StartAngle := 0;
  FData.EndAngle := 360;
end;

procedure TsgConvertMetafileToCad.SetDPtoLP(var ADevicePt: TPoint);
begin
  ADevicePt.X := Round(FParamsDraw.BeginViewPortCoord.X +
    ADevicePt.X * FParamsDraw.SizeWindow.cx / Abs(FParamsDraw.SizeViewPort.cx));
  ADevicePt.Y := Round(FParamsDraw.BeginViewPortCoord.Y +
    ADevicePt.Y * FParamsDraw.SizeWindow.cy / Abs(FParamsDraw.SizeViewPort.cy));
end;

procedure TsgConvertMetafileToCad.SetLType(AStyle: TPenStyle);
var
  vName: String;
  vConv: TsgDXFConverterAccess;
  vLType: TsgDXFLineType;
begin
  vName := cnstLTypeNames[AStyle];
  if vName = '' then
    vName := cnstLTypeNames[psSolid];
  FData.Text := vName;
  vConv := TsgDXFConverterAccess(GetConverter);
  if Assigned(vConv) then
  begin
    vLType := vConv.LTypeByName(vName);
    if not Assigned(vLType) then
    begin
      case AStyle of
        psDash:
          vLType := TsgDXFLineType(vConv.AddLineType(vName, 0, [KoefScale*8,KoefScale*-2]));
        psDot:
          vLType := TsgDXFLineType(vConv.AddLineType(vName, 0, [KoefScale*2,KoefScale*-2]));
        psDashDot:
          vLType := TsgDXFLineType(vConv.AddLineType(vName, 0, [KoefScale*8,KoefScale*-2,KoefScale*2,KoefScale*-2]));
        psDashDotDot:
          vLType := TsgDXFLineType(vConv.AddLineType(vName, 0, [KoefScale*8,KoefScale*-2,KoefScale*2,KoefScale*-2,KoefScale*2,KoefScale*-2]));
      else
        vLType := TsgDXFLineType(vConv.AddLineType(vName, 0, []));
      end;
    end;
    FData.LineType := vLType;
  end;
end;

procedure TsgConvertMetafileToCad.SetHatch(AColor: TColor;
  AFillStyle: Integer = 0; AStyle: Integer = BS_SOLID;
  AIsTextPath: Boolean = False);
begin
  AddOneHatch;
  GetFillColor(AColor, FData.RGBColor, FData.ColorCAD);
  FData.FillStyle := TsgFillStyle(AFillStyle + 2);
  FData.HatchStyle := AStyle;
  if not FBeginPath then
    FParamsPath.BeginPath := AIsTextPath;
end;

procedure TsgConvertMetafileToCad.SetFillingEntity(AValue: Boolean);
begin
  FFillingEntity := AValue;
end;

procedure TsgConvertMetafileToCad.SetAlternateBlack(AValue: Boolean);
begin
  FAlternateBlack := AValue;
end;

procedure TsgConvertMetafileToCad.SetKoefPenSize(AValue: Single);
begin
  FKoefPenSize := AValue;
end;

procedure TsgConvertMetafileToCad.SetMatrix(AXForm: TXForm);
begin
  AddOneHatch;
  FMatrix := TranslateToMatrix(AXForm);
end;

procedure TsgConvertMetafileToCad.SetOffsetPoint(AValue: TF2DPoint);
begin
  FOffsetPoint := AValue;
end;

procedure TsgConvertMetafileToCad.SetOffsetX(AValue: Double);
begin
  FOffsetPoint.X := AValue;
end;

procedure TsgConvertMetafileToCad.SetOffsetY(AValue: Double);
begin
  FOffsetPoint.Y := AValue;
end;

procedure TsgConvertMetafileToCad.SetOnlyPt(var APt: TFPoint);
var
  vKX,vKY: Double;
begin
  if FParamsDraw.MapMode <> MM_TEXT then
  begin
    vKX := Abs(FParamsDraw.SizeViewPort.cx) / FParamsDraw.SizeWindow.cx;
    vKY := Abs(FParamsDraw.SizeViewPort.cy) / FParamsDraw.SizeWindow.cy;
    APt.X := APt.X * vKX;
    APt.Y := APt.Y * vKY;
  end;
end;

procedure TsgConvertMetafileToCad.SetPen;
var
  vPen: TsgLogPen;
  vWidth: Double;
begin
  if FParamsDrawPen.PData <> nil then
  begin
    vPen := TsgLogPen(FParamsDrawPen.PData^);
    FData.Color := TranslateColor(vPen.lp.lopnColor);
    if vPen.lp.lopnWidth.X > 0 then
    begin
      vWidth := GetLengthVector(PointToFPoint(vPen.lp.lopnWidth.X, 0, True));
      FData.Thickness := ConvertLineWeightToDXF(FKoefPenSize * vWidth + 0.02) / 100;
      if not FUse01MM then
        FData.Thickness := 10*FData.Thickness;
    end
    else
      FData.Thickness := 0;
    if Length(vPen.Styles) > 0 then  // to-do add custom pen style
      SetLType(psDash)
    else
      if vPen.lp.lopnStyle > 6 then
        SetLType(psSolid)
      else
        SetLType(TPenStyle(vPen.lp.lopnStyle));
  end;
end;

procedure TsgConvertMetafileToCad.SetPolyline(APoints: TFPointList;
  ACapacity: Integer; AClosed: Boolean = False);
begin
  APoints.Capacity := ACapacity;
  FData.Points := APoints;
  FData.Closed := AClosed;
end;

procedure TsgConvertMetafileToCad.SetPosition(P: TPoint);
begin
  FPenPos := P;
end;

procedure TsgConvertMetafileToCad.SetPosition(P: TSmallPoint);
begin
  SetPosition(Point(p.x, p.y));
end;

procedure TsgConvertMetafileToCad.SetScale(AValue: Double);
begin
  FScale := AValue;
end;

procedure TsgConvertMetafileToCad.SetUnitSize(AValue: Double);
begin
  FUnitSize := AValue;
end;

procedure TsgConvertMetafileToCad.SetUse01MM(AValue: Boolean);
begin
  FUse01MM := AValue;
end;

procedure TsgConvertMetafileToCad.SetUseMetafileRatio(AValue: Boolean);
begin
  FUseMetafileRatio := AValue;
end;

function TsgConvertMetafileToCad.TestTextImage(const AList: TsgObjectList;
  const ATextItem: TsgTextRectListItem; const AIsSubstring: Boolean;
  var AResText: string; ABoundBox: PFRect): Boolean;
const
  cnstDiffsRatio = 1.45;
  cnstMinChecks = 1.6;
  cnstMinDiffsPerPixel: array[Boolean] of Double = (0.09, 0.06);
  cnstMinHitsPerPixelHiDiffs: array[Boolean] of Double = (0.21, 0.32);
  cnstMinHitsPerPixelLoDiffs: array[Boolean] of Double = (0.1, 0.14);
  cnstMaxHitsPerPixel: array[Boolean] of Double = (0.25, 0.5);
  cnstMaxDiffsPerPixel: array[Boolean] of Double = (0.6, 0.34);
  cnstMaxDiffsPerPixelRotated: array[Boolean] of Double = (0.44, 0.41);
  cnstRadiusesPerMinSize: array[Boolean] of Integer = (10, 11);
  cnstMaxHeightPerWidth = 7;
  cnstMaxTextImageSize = 480000;
  cnstLoDiffs = 0.24;
  cnstRadius = 4;
var
  vBmp1,vBmp2,vBmp3: TBitmap;
  vCADImage: TsgCADImage;
  vEntity: TsgDXFEntity;
  vRect: TRect;
  vFontFace: TsgFontFace;
  vRotMatrix: TFMatrix;
  vSaveColor: TsgColorCAD;
  vSaveEnableFitProportional: Boolean;
  vRadius: Integer;
  vText: string;
  vBoundaryBox: TFRect;

  procedure InitRect(const AEntity: TsgDXFEntity; const AIsSubstring: Boolean;
    var ARect: TRect; var ARadius: Integer);
  var
    vBox: TFRect;
    vSize: TF2DPoint;
    vMinSize,vImageMul: Double;
  begin
    vBox := AEntity.Box;
    vSize := GetSizeFRect2D(vBox);
    vMinSize := Min(vSize.X, vSize.Y);
    if IsZero(vMinSize) then
    begin
      ARadius := cnstRadius;
      ARect := Rect(0, 0, 0, 0);
      Exit;
    end;
    vImageMul := Round(Max(cnstRadiusesPerMinSize[AIsSubstring]*cnstRadius / vMinSize, 2));
    if (vSize.X * vSize.Y >= cnstMaxTextImageSize) or
      (vImageMul * vSize.X * vImageMul * vSize.Y >= cnstMaxTextImageSize) then
    begin
      if cnstRadius * vSize.X * cnstRadius * vSize.Y >= cnstMaxTextImageSize then
        ARect := Rect(0, 0, Round(vSize.X), Round(vSize.Y))
      else
        ARect := Rect(0, 0, cnstRadius*Round(vSize.X), cnstRadius*Round(vSize.Y));
      ARadius := 1;
    end
    else
    begin
      ARect := Rect(0, 0, Round(vImageMul*vSize.X), Round(vImageMul*vSize.Y));
      ARadius := cnstRadius;
    end;
  end;

  procedure InitBitmap(const ABitmap: TBitmap; const AEntity: TsgDXFEntity;
    AImage: TsgCADImage; const ARect: TRect; const ABox: PFRect);
  begin
    ABitmap.PixelFormat := pf24bit;
    ABitmap.Width := ARect.Right;
    ABitmap.Height := ARect.Bottom;
    if AEntity <> nil then
    begin
      AImage.GetExtents;
      if Assigned(ABox) then
        AImage.DrawRect(ABitmap.Canvas.Handle, ABox^, ARect)
      else
        ABitmap.Canvas.StretchDraw(ARect, AImage);
    end;
  end;

  procedure InitTextBitmap(const ABitmap: TBitmap; const AEntity: TsgDXFEntity;
    AImage: TsgCADImage; const ARect: TRect);
  var
    vBox: TFRect;
  begin
    vBox := GetTextBox(TsgDXFText(AEntity), AImage);
    InitBitmap(ABitmap, AEntity, AImage, ARect, @vBox);
  end;

  procedure RemoveAndFreeEntity(AImage: TsgCADImage; AEntity: TsgDXFEntity);
  begin
    AImage.CurrentLayout.RemoveEntity(AEntity);
    AEntity.Free;
  end;

  function CloneAndTransformBoundary(const AImage: TsgCADImage;
    const ASourceList: TsgObjectList; const AMatrix: TFMatrix;
    var ARect: TFRect): TsgObjectList;
  var
    K,L: Integer;
    vSrcEntity: TsgDXFEntity;
    vEntity: TsgDXFEntity;
  begin
    ARect := cnstBadRect;
    Result := TsgObjectList.Create;
    for K := 0 to ASourceList.Count - 1 do
    begin
      vSrcEntity := TsgDXFEntity(ASourceList[K]);
      UnionFRect(ARect, vSrcEntity.Box);
      vEntity := nil;

      if vSrcEntity is TsgDXFSpline then
      begin
        vEntity := TsgDXFSpline.Create;
        TsgDXFSpline(vEntity).BeginningTangent :=
          FPointXMat(TsgDXFSpline(vSrcEntity).BeginningTangent, AMatrix);
        TsgDXFSpline(vEntity).EndingTangent :=
          FPointXMat(TsgDXFSpline(vSrcEntity).EndingTangent, AMatrix);
        for L := 0 to TsgDXFSpline(vSrcEntity).Knots.Count - 1 do
          TsgDXFSpline(vEntity).Knots.Add(TsgDXFSpline(vSrcEntity).Knots[L]);
        for L := 0 to TsgDXFSpline(vSrcEntity).Controls.Count - 1 do
          TsgDXFSpline(vEntity).Controls.Add(
            FPointXMat(TsgDXFSpline(vSrcEntity).Controls[L], AMatrix));
        TsgDXFLWPolyline(vEntity).Closed := TsgCADBasePolyLine(vSrcEntity).Closed;
      end
      else
        if vSrcEntity is TsgCADBasePolyLine then
        begin
          vEntity := TsgDXFLWPolyline.Create;
          for L := 0 to TsgCADBasePolyLine(vSrcEntity).PointCount - 1 do
            AddVertexInPolyline(TsgDXFLWPolyline(vEntity),
              FPointXMat(TsgCADBasePolyLine(vSrcEntity).Points[L], AMatrix));
          TsgDXFLWPolyline(vEntity).Closed := TsgCADBasePolyLine(vSrcEntity).Closed;
        end
        else
          if vSrcEntity is TsgDXFLine then
          begin
            vEntity := TsgDXFLWPolyline.Create;
            AddVertexInPolyline(TsgDXFLWPolyline(vEntity),
              FPointXMat(TsgDXFLine(vSrcEntity).Point, AMatrix));
            AddVertexInPolyline(TsgDXFLWPolyline(vEntity),
              FPointXMat(TsgDXFLine(vSrcEntity).Point1, AMatrix));
          end;
      if vEntity <> nil then
      begin
        AImage.Converter.Loads(vEntity);
        Result.Add(vEntity);
      end;
    end;
  end;

  function AddTextHatch(const ACADImage: TsgCADImage; const ASourceList: TsgObjectList;
    const AMatrix: TFMatrix; var ARect: TFRect): TsgDXFEntity;
  var
    vList: TsgObjectList;
  begin
    vList := CloneAndTransformBoundary(ACADImage, ASourceList, AMatrix, ARect);
    try
      Result := DoAddHatch(vList, True, ACADImage);
    finally
      TsgObjectList.FreeList(vList);
    end;
  end;

  function GetMinColor(const ABitmap: TBitmap; const X,Y,R: Integer): TColor;
  var
    K,L: Integer;
  begin
    Result := clWhite;
    for K := MaxI(X - R, 0) to MinI(X + R, ABitmap.Width - 1) do
      for L := MaxI(Y - R, 0) to MinI(Y + R, ABitmap.Height - 1) do
        if ABitmap.Canvas.Pixels[K,L] <> clWhite then
        begin
          Result := clBlack;
          Exit;
        end;
  end;

  function CompareBitmapRegion(const ABmp1,ABmp2: TBitmap;
    const AX1,AY1,AX2,AY2,AR: Integer; var AWeight: Double): Boolean;
  var
    K,L: Integer;
    vTotal,vHits,vDiff,vArea1,vArea2,vArea3: Integer;
    vX1,vY1,vX2,vY2: Integer;
    vColor1,vColor2: TColor;

    function CheckLims(const AV1,AV2,AL1,AL2: Integer): Boolean;
    begin
      Result := True;
      if AV1 < 0 then
        Result := False
      else
        if AV2 < 0 then
          Result := False
        else
          if AV1 >= AL1 then
            Result := False
          else
            if AV2 >= AL2 then
              Result := False;
    end;

    function EqualColors(AColor1,AColor2: TColor): Boolean;
    begin
      if AColor1 <> clWhite then
        Result := AColor2 <> clWhite
      else
        Result := AColor2 = clWhite;
    end;

  begin
    Result := False;
    vHits := 0;
    vDiff := 0;
    vTotal := 0;
    vArea1 := 0;
    vArea2 := 0;
    vArea3 := 0;
    for K := -AR to AR do
    begin
      vX1 := AX1 + K;
      vX2 := AX2 + K;
      if CheckLims(vX1, vX2, ABmp1.Width, ABmp2.Width) then
        for L := -AR to AR do
        begin
          vY1 := AY1 + L;
          vY2 := AY2 + L;
          if CheckLims(vY1, vY2, ABmp1.Height, ABmp2.Height) then
          begin
            vColor1 := ABmp1.Canvas.Pixels[vX1, vY1];
            vColor2 := ABmp2.Canvas.Pixels[vX2, vY2];
            if vColor1 <> clWhite then
              Inc(vArea1);
            if vColor2 <> clWhite then
              Inc(vArea2);
            if EqualColors(vColor1, vColor2) then
            begin
              if vColor1 <> clWhite then
                Inc(vArea3);
              Result := True;
              Inc(vHits);
            end
            else
              Inc(vDiff);
            Inc(vTotal);
          end;
        end;
    end;
    if ATextItem.FontFace.FontFound then
    begin
      if vTotal = 0 then
        AWeight := 1
      else
        AWeight := (vHits - vDiff) / vTotal;
    end
    else
    begin
      AWeight := 2*AR*AR;
      AWeight := (AWeight - (vArea1 + vArea2 - 2*vArea3)) / AWeight;
    end;
  end;

  function FillBlock(const ABitmap: TBitmap; const X,Y,R: Integer;
    AColor: TColor): TColor;
  var
    K,L: Integer;
  begin
    Result := clWhite;
    for K := MaxI(X - R, 0) to MinI(X + R, ABitmap.Width - 1) do
      for L := MaxI(Y - R, 0) to MinI(Y + R, ABitmap.Height - 1) do
        ABitmap.Canvas.Pixels[K,L] := AColor;
  end;

  procedure AddCompareRatio(const ARatio: Double; var AHits,ADiff: Double);
  begin
    if ARatio >= 0 then
      AHits := AHits + ARatio
    else
      ADiff := ADiff - ARatio;
  end;

  function TestBitmaps(const ABmp1,ABmp2,ABmp3: TBitmap;
    const ATextItem: TsgTextRectListItem; const ARadius: Integer;
    var AWeigth: Double): Boolean;
  var
    I,J,N,vPixels,vMaxWidth,vMaxHeight,vStep: Integer;
    vBmp1Color,vBmp2Color: TColor;
    vMaxDiffsPerPixel: Double;
    vRatio,vDiff,vDiffWhites,vHits: Double;
  begin
    Result := False;
    vDiff := 0;
    vHits := 0;
    vPixels := 0;
    vDiffWhites := 0;
    if IsZero(ATextItem.FontFace.Rotation) then
      vMaxDiffsPerPixel := cnstMaxDiffsPerPixel[ATextItem.FontFace.FontFound]
    else
      vMaxDiffsPerPixel := cnstMaxDiffsPerPixelRotated[ATextItem.FontFace.FontFound];
    vMaxWidth := MinI(ABmp1.Width, ABmp2.Width);
    vMaxHeight := MinI(ABmp1.Height, ABmp2.Height);
    vStep := MaxI(2*ARadius, 1);
    N := ARadius;
    I := ARadius;
    while I < vMaxWidth do
    begin
      J := ARadius;
      while J < vMaxHeight do
      begin
        vBmp1Color := GetMinColor(ABmp1, N, J, ARadius);
        vBmp2Color := GetMinColor(ABmp2, I, J, ARadius);
        if vBmp2Color = clBlack then
        begin
          if vBmp1Color = vBmp2Color then
          begin
            if CompareBitmapRegion(ABmp1, ABmp2, N, J, I, J, ARadius, vRatio) then
              FillBlock(vBmp3, N, J, ARadius, clBlack);
            AddCompareRatio(vRatio, vHits, vDiff);
          end
          else
            vDiff := vDiff + 1;
        end
        else
        begin
          if vBmp1Color <> vBmp2Color then
          begin
            vDiffWhites := vDiffWhites + 1;
            //CompareBitmapRegion(ABmp1, ABmp2, N, J, I, J, ARadius, vRatio);
            //AddCompareRatio(vRatio, vHits, vDiffWhites);
          end
          else
            vHits := vHits + 1;
        end;
        Inc(vPixels);
        Inc(J, vStep);
      end;
      Inc(N, vStep);
      Inc(I, vStep);
    end;
    if vPixels <> 0 then
    begin
      AWeigth := (vHits + vPixels - (vDiff + vDiffWhites)) / vPixels;
      vHits := vHits / vPixels;
      vRatio := vDiff / vPixels;
      if (vRatio < cnstMinDiffsPerPixel[ATextItem.FontFace.FontFound]) and
        (vHits > cnstMaxHitsPerPixel[ATextItem.FontFace.FontFound]) then
      begin
        Result := True;
      end
      else
      begin
        vRatio := (vDiff + vDiffWhites) / vPixels;
        if vRatio < vMaxDiffsPerPixel then
        begin
          if vRatio <= cnstLoDiffs then
            Result := vHits >= cnstMinHitsPerPixelLoDiffs[ATextItem.FontFace.FontFound]
          else
            Result := vHits >= cnstMinHitsPerPixelHiDiffs[ATextItem.FontFace.FontFound];
        end
        else
          if vHits > cnstMaxHitsPerPixel[ATextItem.FontFace.FontFound] then
            Result := True;
      end;
    end
    else
      AWeigth := 0;
  end;

  {$IFDEF SG_TRY_CORRECT_RUS}
  function TranslateStr(const AStr: string; const ABase1,ABase2: Integer;
    var AIsTranslated: Boolean): string;
  const
    cnstRus1: array[0..31] of TsgBadCharDescription = (
      (RightChar: 'A'; IsEuro: False),  // 207
      (RightChar: 'Á'; IsEuro: False),  // 208
      (RightChar: 'Â'; IsEuro: False),  // 209
      (RightChar: 'Ã'; IsEuro: False),  // 210
      (RightChar: 'Ä'; IsEuro: False),  // 211
      (RightChar: 'Å'; IsEuro: False),  // 212
      (RightChar: 'Æ'; IsEuro: False),  // 213
      (RightChar: 'Ç'; IsEuro: False),  // 214
      (RightChar: 'È'; IsEuro: False),  // 215
      (RightChar: 'É'; IsEuro: False),  // 216
      (RightChar: 'Ê'; IsEuro: False),  // 217
      (RightChar: 'Ë'; IsEuro: False),  // 218
      (RightChar: 'Ì'; IsEuro: False),  // 219
      (RightChar: 'Í'; IsEuro: False),  // 220
      (RightChar: 'Î'; IsEuro: False),  // 221
      (RightChar: 'Ï'; IsEuro: False),  // 222
      (RightChar: 'Ð'; IsEuro: True),  // 223
      (RightChar: 'Ñ'; IsEuro: False),  // 224
      (RightChar: 'Ò'; IsEuro: False),  // 225
      (RightChar: 'Ó'; IsEuro: True),  // 226
      (RightChar: 'Ô'; IsEuro: False),  // 227
      (RightChar: 'Õ'; IsEuro: False),  // 228
      (RightChar: 'Ö'; IsEuro: False),  // 229
      (RightChar: '×'; IsEuro: False),  // 230
      (RightChar: 'Ø'; IsEuro: False),  // 231
      (RightChar: 'Ù'; IsEuro: True),  // 232
      (RightChar: 'Ú'; IsEuro: True),  // 232
      (RightChar: 'Û'; IsEuro: True),  // 233
      (RightChar: 'Ü'; IsEuro: True),  // 234
      (RightChar: 'Ý'; IsEuro: True),  // 235
      (RightChar: 'Þ'; IsEuro: False),  // 237
      (RightChar: 'ß'; IsEuro: True));  // 238

    cnstRus2: array[0..31] of TsgBadCharDescription = (
      (RightChar: 'à'; IsEuro: True),  // 239
      (RightChar: 'á'; IsEuro: False),  // 240
      (RightChar: 'â'; IsEuro: False),  // 241
      (RightChar: 'ã'; IsEuro: False),  // 242
      (RightChar: 'ä'; IsEuro: False),  // 243
      (RightChar: 'å'; IsEuro: True),  // 244
      (RightChar: 'æ'; IsEuro: False),  // 245
      (RightChar: 'ç'; IsEuro: False),  // 246
      (RightChar: 'è'; IsEuro: False),  // 247
      (RightChar: 'é'; IsEuro: False),  // 248
      (RightChar: 'ê'; IsEuro: False),  // 249
      (RightChar: 'ë'; IsEuro: False),  // 250
      (RightChar: 'ì'; IsEuro: True),  // 251
      (RightChar: 'í'; IsEuro: True),  // 252
      (RightChar: 'î'; IsEuro: False),  // 253
      (RightChar: 'ï'; IsEuro: False),  // 254
      (RightChar: 'ð'; IsEuro: False),  // 255
      (RightChar: 'ñ'; IsEuro: False),  // 256
      (RightChar: 'ò'; IsEuro: False),  // 257
      (RightChar: 'ó'; IsEuro: False),  // 258
      (RightChar: 'ô'; IsEuro: False),  // 259
      (RightChar: 'õ'; IsEuro: False),  // 260
      (RightChar: 'ö'; IsEuro: False),  // 261
      (RightChar: '÷'; IsEuro: False),  // 262
      (RightChar: 'ø'; IsEuro: False),  // 263
      (RightChar: 'ù'; IsEuro: False),  // 264
      (RightChar: 'ú'; IsEuro: False),  // 265
      (RightChar: 'û'; IsEuro: False),  // 266
      (RightChar: 'ü'; IsEuro: False),  // 267
      (RightChar: 'ý'; IsEuro: False),  // 268
      (RightChar: 'þ'; IsEuro: False),  // 269
      (RightChar: 'ÿ'; IsEuro: False));  // 270
  var
    vChar: Integer;
    I,vRusLetters,vEuroLetters: Integer;
    vTrStr: string;

    function TranslateChar(const AChar: Integer;
      const ABadDescs: array of TsgBadCharDescription;
      var ARusLetters,AEuroLetters: Integer): Char;
    var
      vBadCharDesc: TsgBadCharDescription;
    begin
      vBadCharDesc := ABadDescs[AChar];
      if vBadCharDesc.IsEuro then
        Inc(AEuroLetters)
      else
        Inc(ARusLetters);
      Result := vBadCharDesc.RightChar;
    end;
  begin
    vTrStr := '';
    vRusLetters := 0;
    vEuroLetters := 0;
    AIsTranslated := False;
    for I := 1 to Length(AStr) do
    begin
      vChar := Ord(AStr[I]);
      if (vChar >= ABase1) and (vChar <= ABase1 + 31) then
      begin
        vTrStr := vTrStr + TranslateChar(vChar - ABase1, cnstRus1,
          vRusLetters, vEuroLetters);
        AIsTranslated := True;
      end
      else
        if (vChar >= ABase2) and (vChar <= ABase2 + 31) then
        begin
          vTrStr := vTrStr + TranslateChar(vChar - ABase2, cnstRus2,
            vRusLetters, vEuroLetters);
          AIsTranslated := True;
        end
        else
          vTrStr := vTrStr + AStr[I];
    end;
    Result := vTrStr;
  end;
  {$ENDIF}

  function CompareText(const AImage: TsgCADImage; const ABmp1,ABmp2,ABmp3: TBitmap;
    const ATextItem: TsgTextRectListItem; AFontFace: TsgFontFace;
    const ARect: TRect; const ABounds: TFRect; const ARadius: Integer;
    const AText: string; var AResText: string): Boolean;
  var
    vWeigth: Double;

    procedure TestAndSetResult(const ATestStr: string; var AWeigth: Double;
      var AIsCorrect: Boolean);
    var
      vNewWeigth: Double;
      vTextEnt: TsgDXFEntity;
    begin
      vTextEnt := AddTextEnt(AImage, ATestStr, ABounds, AFontFace, FData.ColorCAD, True);
      if vTextEnt = nil then
        Exit;
      InitTextBitmap(ABmp1, vTextEnt, AImage, ARect);
      RemoveAndFreeEntity(AImage, vTextEnt);
      if TestBitmaps(ABmp1, ABmp2, ABmp3, ATextItem, ARadius, vNewWeigth) then
      begin
        if (vNewWeigth > AWeigth) or (not AIsCorrect) then
        begin
          AResText := ATestStr;
          AWeigth := vNewWeigth;
          AIsCorrect := True;
        end;
      end;
    end;

  {$IFDEF SG_TRY_CORRECT_RUS}
    procedure TryCorrectRus(const ABase1,ABase2: Integer; var AWeigth: Double;
      var AIsCorrect: Boolean);
    var
      vRusText: string;
      vIsTranslated: Boolean;
    begin
      vRusText := TranslateStr(AText, ABase1, ABase2, vIsTranslated);
      if vIsTranslated then
        TestAndSetResult(vRusText, AWeigth, AIsCorrect);
    end;
  {$ENDIF}

  begin
    Result := False;
    vWeigth := 0;
    TestAndSetResult(AText, vWeigth, Result);
    TestAndSetResult(ReverseString(AText), vWeigth, Result);
  {$IFDEF SG_TRY_CORRECT_RUS}
    TryCorrectRus(207, 239, vWeigth, Result);
    TryCorrectRus(192, 224, vWeigth, Result);
  {$ENDIF}
  end;

begin
  vBmp1 := nil;
  vBmp2 := nil;
  vBmp3 := nil;
  vCADImage := nil;
  Result := False;
  if not cnstExtractTexts then
    Exit;
  vSaveEnableFitProportional := FEnableFitProportional;
  vSaveColor := FData.ColorCAD;
  try
    FData.ColorCAD := cnstColorCADByBlackWhite;
    FEnableFitProportional := False;
    vFontFace := ATextItem.FontFace;
    vFontFace.Rotation := 0;
    vBmp1 := TBitmap.Create;
    vBmp2 := TBitmap.Create;
    vBmp3 := TBitmap.Create;
    vCADImage := TsgCADImage.Create;
    vCADImage.IsWithoutBorder := True;
    vCADImage.CurrentLayout := vCADImage.Converter.Layouts[0];
    vCADImage.Converter.InitializeSectionsBegin;
    vCADImage.Converter.InitializeSectionsEnd;
    vCADImage.Stretch := True;
    if IsZero(ATextItem.FontFace.Rotation) then
      vCADImage.TTFMode := ttfGDI
    else
      vCADImage.TTFMode := FCADImage.TTFMode;
    vRotMatrix := BuildRotMatrix(axisZ, DegToRad(-ATextItem.FontFace.Rotation));
    vEntity := AddTextHatch(vCADImage, AList, vRotMatrix, vBoundaryBox);
    if vEntity = nil then
      Exit;
    if Assigned(ABoundBox) then
      ABoundBox^ := vBoundaryBox;
    InitRect(vEntity, AIsSubstring, vRect, vRadius);
    InitBitmap(vBmp2, vEntity, vCADImage, vRect, nil);
    RemoveAndFreeEntity(vCADImage, vEntity);
    vText := Trim(ATextItem.Text);
    Result := CompareText(vCADImage, vBmp1, vBmp2, vBmp3, ATextItem, vFontFace,
      vRect, vBoundaryBox, vRadius, vText, AResText);
  finally
    FreeAndNil(vBmp1);
    FreeAndNil(vBmp2);
    FreeAndNil(vBmp3);
    FreeAndNil(vCADImage);
    FData.ColorCAD := vSaveColor;
    FEnableFitProportional := vSaveEnableFitProportional;
  end;
end;

function TsgConvertMetafileToCad.TranslateColor(AColor: TColor;
  AObjectType: TypeGDIObject = tgPen): TsgColorCAD;
var
  vColor: Integer;
  vLogPen: PsgLogPen;
begin
  vColor := AColor;
  if AObjectType in [tgPen, tgExtPen] then
  begin
    vLogPen := FParamsDrawPen.PData;
    if vLogPen.lp.lopnStyle = PS_NULL then
      vColor := clNone;
  end;
  if vColor <> clNone then
    vColor := vColor and $FFFFFF;
  if FAlternateBlack then
    case vColor of
      clBlack: Result := MakeColorCAD(acIndexColor, clDXFBlackWhite);
      clWhite:
        if cnstWhiteToGRBWhiteColor then
          Result := MakeColorCAD(acRGBColor, clWhite)
        else
          Result := MakeColorCAD(acIndexColor, clDXFWhite);
      clNone: Result := cnstColorCADNone;
    else
      Result := ConvertColortoColorCAD(vColor);
    end
  else
    case vColor of
      clBlack: Result := MakeColorCAD(acRGBColor, clBlack);
      clWhite: Result := MakeColorCAD(acRGBColor, clWhite);
      clNone: Result := cnstColorCADNone;
    else
      Result := ConvertColortoColorCAD(vColor);
    end;
end;

procedure TsgConvertMetafileToCad.UnionLinesAndPolylines(const AFull: Boolean);

  function CheckType(const AEntity: TsgDXFEntity; const ABase: TsgDXFEntity = nil): Boolean;
  begin
    case AEntity.EntType of
      ceLine: Result := True;
      cePolyline, ceLWPolyline:
        begin
          if (TsgDXFPolyline(AEntity).Count > 0) and (not TsgCADBasePolyline(AEntity).Closed) then
            Result := True
          else
            Result := False;
        end
    else
      Result := False;
    end;
    if Result and Assigned(ABase) then
    begin
      Result := (AEntity.Layer = ABase.Layer) and
       (AEntity.LineType = ABase.LineType) and
       (AEntity.LineTypeScale = ABase.LineTypeScale) and
       IsEqualColorCAD(AEntity.ColorCAD, ABase.ColorCAD);
    end;
  end;

  procedure SetStartEndPoint(const AEntity: TsgDXFEntity; var AStart: TFPoint; var AEnd: TFPoint);
  begin
    case AEntity.EntType of
      ceLine:
        begin
          AStart := TsgDXFLine(AEntity).Point;
          AEnd := TsgDXFLine(AEntity).Point1;
        end;
      cePolyline, ceLWPolyline:
        begin
          AStart := TsgDXFVertex(AEntity.Entities[0]).Point;
          AEnd := TsgDXFVertex(AEntity.Entities[AEntity.Count - 1]).Point;
        end;
    else
      AStart := cnstFPointZero;
      AEnd := cnstFPointZero;
    end;
  end;

  function UnionEntity(const ALayout: TsgDXFLayout; const AStartIndex: Integer;
    var AEntity: TsgDXFEntity; const AFull: Boolean): Integer;
  var
    I, J, vEqual, vCount: Integer;
    vEntity: TsgDXFEntity;
    vEntityNew: TsgDXFPolyline;
    vP1Start, vP1End, vP2Start, vP2End: TFPoint;
  begin
    Result := 0;
    if not CheckType(AEntity) then
      Exit;
    SetStartEndPoint(AEntity, vP1Start, vP1End);
    vCount := ALayout.Count - 1;
    if not AFull then
      vCount := MinI(AStartIndex, vCount);
    for I := AStartIndex to vCount do
    begin
      vEntity := ALayout.Entities[I];
      if CheckType(vEntity, AEntity) then
      begin
        SetStartEndPoint(vEntity, vP2Start, vP2End);
        vEqual := Integer(IsEqualFPoints(vP1Start, vP2Start));
        if (vEqual = 0) and IsEqualFPoints(vP1Start, vP2End) then
          vEqual := 2;
        if (vEqual = 0) and IsEqualFPoints(vP1End, vP2Start) then
          vEqual := 3;
        if (vEqual = 0) and IsEqualFPoints(vP1End, vP2End) then
          vEqual := 4;
        if vEqual > 0 then
        begin
          case AEntity.EntType of
            ceLine:
              begin
                Result := 2;
                if vEntity.EntType = ceLine then
                begin
                  vEntityNew := TsgDXFLWPolyline.Create;
                  vEntityNew.AssignEntity(AEntity);
                  case vEqual of
                    1:
                      begin
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point1);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point1);
                      end;
                    2:
                      begin
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point1);
                      end;
                    3:
                      begin
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point1);
                        AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point1);
                      end;
                  else
                    AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point);
                    AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point1);
                    AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point);
                  end;
                end
                else
                begin
                  vEntityNew := TsgDXFPolyline(vEntity);
                  case vEqual of
                    1:  InsertVertexInPolyline(vEntityNew, 0, TsgDXFLine(AEntity).Point1);
                    2:  InsertVertexInPolyline(vEntityNew, 0, TsgDXFLine(AEntity).Point);
                    3:  AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point1);
                  else
                    AddVertexInPolyline(vEntityNew, TsgDXFLine(AEntity).Point);
                  end;
                end;
                ALayout.DeleteEntity(I);
                if vEntityNew <> vEntity then
                  vEntity.Free;
                AEntity.Free;
                AEntity := vEntityNew;
              end;
            cePolyline, ceLWPolyline:
              begin
                Result := 1;
                vEntityNew := TsgDXFPolyline(AEntity);
                if vEntity.EntType = ceLine then
                begin
                  case vEqual of
                    1:  InsertVertexInPolyline(vEntityNew, 0, TsgDXFLine(vEntity).Point1);
                    2:  InsertVertexInPolyline(vEntityNew, 0, TsgDXFLine(vEntity).Point);
                    3:  AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point1);
                  else
                    AddVertexInPolyline(vEntityNew, TsgDXFLine(vEntity).Point);
                  end;
                end
                else
                begin
                  case vEqual of
                    1:
                       begin
                         for J := 1 to vEntity.Count - 1 do
                           vEntityNew.InsertEntity(0, vEntity.Entities[J]);
                       end;
                    2:
                      begin
                         for J := vEntity.Count - 2 downto 0 do
                           vEntityNew.InsertEntity(0, vEntity.Entities[J]);
                       end;
                    3:
                       begin
                         for J := 1 to vEntity.Count - 1 do
                           vEntityNew.AddEntity(vEntity.Entities[J]);
                       end;
                  else
                    for J := vEntity.Count - 2 downto 0 do
                      vEntityNew.AddEntity(vEntity.Entities[J]);
                  end;
                end;
                vEntity.Clear(False);
                ALayout.DeleteEntity(I);
                vEntity.Free;
              end;
          end;
          Break;
        end;
      end;
    end;
  end;

var
  I, vCountEntity, vCurrentEntity, vUnion: Integer;
  vUnionLayout: Boolean;
  vEntity: TsgDXFEntity;
  vLayout: TsgDXFLayout;
begin
  if not (Assigned(FCADImage) and Assigned(FCADImage.CurrentLayout)) then
    Exit;
  DoOnProgress(psStarting, 0, 0);
  try
    vLayout := FCADImage.CurrentLayout;
    vCurrentEntity := 0;
    vCountEntity := vLayout.Count;
    vUnionLayout := False;
    I := 0;
    while I < vLayout.Count - 2 do
    begin
      Inc(vCurrentEntity);
      vEntity := vLayout.Entities[I];
      vUnion := UnionEntity(vLayout, I + 1, vEntity, AFull);
      if vUnion > 0 then
      begin
        vUnionLayout := True;
        FCADImage.Converter.Loads(vEntity);
        if vUnion and 2 <> 0 then
          TsgDXFLayoutAccess(vLayout).SetEntity(I, vEntity);
      end
      else
        Inc(I);
      DoOnProgress(psRunning, vCurrentEntity, vCountEntity);
    end;
    if vUnionLayout then
      FCADImage.Converter.Loads(vLayout);
  finally
    DoOnProgress(psEnding, 0, 0);
  end;
end;

function TsgConvertMetafileToCad.DoAddHatch(AList: TsgObjectList;
  const AIsCheckTransparent: Boolean;
  ACADImage: TsgCADImage = nil): TsgDXFEntity;
var
  vUnderEnts,vUnderBoxes,vNewList,vSaveEnts: TsgObjectList;
  vBlock,vOwner: TsgDXFBlock;
  vSaveData: TdxfExportData;
  vCurInsert: TsgDXFInsert;
  I: Integer;

  procedure CreateHatch(const AEntities: TsgObjectList);
  begin
    FData.Boundary := AEntities;
    case FData.HatchStyle of
      bs_Solid:
        Result := InitAndAddEntity(ACADImage, TsgCADCurvePolygon, False, True);
      bs_Hatched:
        Result := InitAndAddEntity(ACADImage, TsgCADHatch, False, True);
    end;
  end;

begin
  Result := nil;
  if FFillingEntity and Assigned(AList) and (AList.Count > 0) and
    (not IsEmptyColorCAD(FData.ColorCAD)) then
  begin
    if AIsCheckTransparent and IsTransparentHatch then
    begin
      vUnderEnts := TsgObjectList.Create;
      try
        vNewList := TsgObjectList.Create;
        try
          vUnderBoxes := TsgObjectList.Create;
          try
            GetBlocksAndInserts(ACADImage, vBlock, vOwner, vCurInsert);
            GetUnderEntittiesFromBlock(vBlock, AList, vUnderEnts, [TsgDXFText, TsgCADCurvePolygon]);
            vSaveData := FData;
            vSaveEnts := FListEnts;
            try
              FListEnts := vUnderBoxes;
              for I := 0 to vUnderEnts.Count - 1 do
                CreateRectangle(TsgDXFEntity(vUnderEnts[I]).Box, FData.RGBColor, False, True, True);
            finally
              FListEnts := vSaveEnts;
              FData := vSaveData;
            end;
            vNewList.Sorted := True;
            vNewList.AssignList(AList, loOr);
            vNewList.AssignList(vUnderBoxes, loOr);
            CreateHatch(vNewList);
          finally
            vUnderBoxes.Free;
          end;
        finally
          vNewList.Free;
        end;
      finally
        vUnderEnts.Free;
      end;
    end
    else
      CreateHatch(AList);
  end;
end;

procedure TsgConvertMetafileToCad.DoCreateBrushIndirect(P: PEMRCreateBrushIndirect);
var
  vTypeObject: PGDIObject;
  vLogBrush: PLogBrush;
begin
  New(vLogBrush);
  vLogBrush^.lbStyle := P^.lb.lbStyle;
  vLogBrush^.lbColor := P^.lb.lbColor;
  vLogBrush^.lbHatch := P^.lb.lbHatch;

  New(vTypeObject);
  vTypeObject^.index := FCommandIndex;
  vTypeObject^.tpObject := tgBrush;
  vTypeObject^.iHandle := P^.ihBrush;
  vTypeObject^.PData := vLogBrush;
  GDIObjectAdd(vTypeObject);

  if FListEnts.Count > 0 then
    AddHatch;
end;

procedure TsgConvertMetafileToCad.DoPolyBezier(P: Pointer;
  Is16: Boolean = False; IsTo: Boolean = False);
var
  vPointCount: Integer;
  vPt: TFPoint;
  vPoints: TFPointList;
begin
  SetPen;
  vPoints := TFPointList.Create;
  try
    if Is16 then
      vPointCount := PEMRPolyBezier16(P)^.cpts
    else
      vPointCount := PEMRPolyBezier(P)^.cptl;
    SetPolyline(vPoints, vPointCount + 1);
    vPt.X := 0; vPt.Y := 0; vPt.Z := 0;
    FData.Point := vPt;
    vPointCount := vPointCount - 1;
    if Is16 then
      AddPoints(vPoints, PsgSmallPoint(@(PEMRPolyBezier16(P).apts)), vPointCount, IsTo)
    else
      AddPoints(vPoints, PPoint(@(PEMRPolyBezier(P).aptl)), vPointCount, IsTo);
    InitAndAddEntity(nil, TsgDXFSpline);
  finally
    vPoints.Free;
  end;
end;

procedure TsgConvertMetafileToCad.DoPolylineAnyType(P: Pointer;
  Is16: Boolean = False; IsTo: Boolean = False; AClosed: Boolean = False);
var
  vPointCount: Integer;
  vPoints: TFPointList;
begin
  SetPen;
  if AClosed then
    SetHatch(FParamsDraw.BkColor, 0, BS_SOLID, True);
  vPoints := TFPointList.Create;
  try
    if Is16 then
    begin
      vPointCount := PEMRPolyline16(P)^.cpts;
      SetPolyline(vPoints, vPointCount + 1, AClosed);
      vPointCount := vPointCount - 1;
      AddPoints(vPoints, PsgSmallPoint(@PEMRPolyline16(P)^.apts), vPointCount, IsTo);
    end
    else
    begin
      vPointCount := PEMRPolyline(P)^.cptl;
      SetPolyline(vPoints, vPointCount + 1, AClosed);
      vPointCount := vPointCount - 1;
      AddPoints(vPoints, PPoint(@PEMRPolyline(P)^.aptl), vPointCount, IsTo);
    end;
    if vPoints.Count > 1 then
      InitAndAddEntity(nil, TsgDXFLWPolyline, IsFillContour(AClosed));
  finally
    vPoints.Free;
    FParamsPath.BeginPath := FBeginPath;
  end;
end;

procedure TsgConvertMetafileToCad.DoPolyPolylineAnyType(P: Pointer;
  Is16: Boolean = False; AClosed: Boolean = False);
var
  I: Integer;
  vCurrentCounts,vCountPolys,vCountPoints: Integer;
  vNumPoly: Integer;
  vSizeDataOfPoint: Integer;
  vP: PByte;
  vPoints: TFPointList;
  P1,P2: TFPoint;
begin
  SetPen;
  if AClosed then
    SetHatch(FParamsDraw.BkColor, 0, BS_SOLID, True);
  if Is16 then
  begin
    vCountPolys := PEMRPolyPolyline16(P)^.nPolys;
    vCountPoints := PEMRPolyPolyline16(P)^.cpts;
    vSizeDataOfPoint := SizeOf(TSmallPoint);
    vP := @(PEMRPolyPolyline16(P).apts);
  end
  else
  begin
    vCountPolys := PEMRPolyPolyline(P)^.nPolys;
    vCountPoints := PEMRPolyPolyline(P)^.cptl;
    vSizeDataOfPoint := SizeOf(TPoint);
    vP := @(PEMRPolyPolyline(P).aptl);
  end;
  Inc(vP, SizeOf(TSmallPoint)*(vCountPolys - 1));
  vNumPoly := 0;
  vPoints := TFPointList.Create;
  try
    SetPolyline(vPoints, vCountPoints, False);
    I := 0;
    while I < vCountPoints do
    begin
      vCurrentCounts := PEMRPolyPolyline(P)^.aPolyCounts[vNumPoly];
      if Is16 then
        AddPoints(vPoints, PsgSmallPoint(IntPtr(vP) + I * vSizeDataOfPoint), vCurrentCounts - 1)
      else
        AddPoints(vPoints, PPoint(IntPtr(vP) + I * vSizeDataOfPoint), vCurrentCounts - 1);
      if I = 0 then
        P1 := vPoints.First;
      Inc(I, vCurrentCounts);
      FData.Points := vPoints;
      InitAndAddEntity(nil, TsgDXFLWPolyline, IsFillContour(AClosed), not FBeginPath);
      if AClosed and (I + 1 = vCountPoints) then
      begin
        P2 := vPoints.Last;
        vPoints.Clear;
        vPoints.Add(P2);
        vPoints.Add(P1);
        FData.Points := vPoints;
        InitAndAddEntity(nil, TsgDXFLWPolyline, IsFillContour(AClosed), not FBeginPath);
        if not FBeginPath then
          AddOneHatch;
      end;
      vPoints.Clear;
      Inc(vNumPoly);
    end;
  finally
    vPoints.Free;
    FParamsPath.BeginPath := FBeginPath;
  end;
end;

{
procedure TsgConvertMetafileToCad.InsideRegion(const ARect: TRect);
var
  I,J: Integer;
begin
  if FClipRgn.IsClipping then
  begin
    for I := 0 to Abs(ARect.Left - ARect.Right) do
      for J := 0 to Abs(ARect.Top - ARect.Bottom) do
        if PtInRegion(FClipRgn.FHRGN, ARect.Left + I,ARect.Top + J) then
          DrawPoint(Point(ARect.Left + I,ARect.Top + J),clRed);
  end;
end;
}

procedure TsgConvertMetafileToCad.DoRectangle(P: PEMRRectangle);
var
  vRect: TRect;
begin
  vRect := P^.rclBox;
  CreateRectangle(vRect, FParamsDraw.BkColor);
end;

procedure TsgConvertMetafileToCad.DoBitBlt(P: PEMRBitBlt);
var
  vData: Pointer;
  vImgHeader: PBitmapInfo;
begin
  vData := GetPointerByOffset(P, P^.offBitsSrc);
  vImgHeader := GetPointerByOffset(P, P^.offBmiSrc);
  AddImageEnt(vImgHeader, vData, Point(P^.xDest, P^.yDest),
    Point(P^.cxDest, P^.cyDest), Point(P^.cxDest, P^.cyDest),
    P^.crBkColorSrc, P^.iUsageSrc, P^.dwRop);
end;

procedure TsgConvertMetafileToCad.DoStretchBlt(P: PEMRStretchblt);
var
  vData: Pointer;
  vImgHeader: PBitmapInfo;
begin
  vData := GetPointerByOffset(P, P^.offBitsSrc);
  vImgHeader := GetPointerByOffset(P, P^.offBmiSrc);
  AddImageEnt(vImgHeader, vData, Point(P^.xDest, P^.yDest),
    Point(P^.cxSrc, P^.cySrc), Point(P^.cxDest, P^.cyDest),
    P^.crBkColorSrc, P^.iUsageSrc, P^.dwRop);
end;

procedure TsgConvertMetafileToCad.DoSetExtEx(const AExtent: TSize;
  var ASaveTo: TSize);
begin
  if (FParamsDraw.MapMode <> MM_TEXT) and (FParamsDraw.MapMode <> MM_HIMETRIC)
    and (FParamsDraw.MapMode <> MM_LOENGLISH) and (FParamsDraw.MapMode <> MM_LOMETRIC)
    and (FParamsDraw.MapMode <> MM_HIENGLISH) and (FParamsDraw.MapMode <> MM_TWIPS) then
  begin
    if (AExtent.cx = 0) or (AExtent.cy = 0) then
    begin
      FParamsDraw.SizeViewPort.cx := 1;
      FParamsDraw.SizeViewPort.cy := 1;
      FParamsDraw.SizeWindow.cx := 1;
      FParamsDraw.SizeWindow.cy := 1;
    end
    else
      ASaveTo := AExtent;
  end;
end;

procedure TsgConvertMetafileToCad.DoSetWindowExtex(P: PEMRSetWindowExtex);
begin
  DoSetExtEx(P^.szlExtent, FParamsDraw.SizeWindow);
end;

procedure TsgConvertMetafileToCad.DoSetWindowOrGex(P: PEMRSetWindowOrGex);
begin
  FParamsDraw.BeginWindowCoord := P^.ptlOrigin;
end;

procedure TsgConvertMetafileToCad.DoSetViewPortExtex(P: PEMRSetViewPortExtex);
begin
  DoSetExtEx(P^.szlExtent, FParamsDraw.SizeViewPort);
end;

procedure TsgConvertMetafileToCad.DoSetViewPortOrGex(P: PEMRSetViewPortOrGex);
begin
  FParamsDraw.BeginViewPortCoord := P^.ptlOrigin;
end;

procedure TsgConvertMetafileToCad.DoSetBrushOrGex(P: PEMRSetBrushOrGex);
begin

end;

procedure TsgConvertMetafileToCad.DoSetPixelv(P: PEMRSetPixelv);
begin
  DrawPoint(P^.ptlPixel, P^.crColor);
end;

procedure TsgConvertMetafileToCad.DoSetMapperFlags(P: PEMRSetMapperFlags);
begin
end;

procedure TsgConvertMetafileToCad.DoSetMapMode(P: PEMRSetMapMode);
begin
  if FParamsDraw.MapMode = P^.iMode then
    Exit;
  FParamsDraw.MapMode := P^.iMode;
  Case P^.iMode of
    MM_TEXT: FKoefMapMode := -1;
    MM_LOMETRIC: FKoefMapMode := 1;
    MM_HIMETRIC: FKoefMapMode := 1;
    MM_LOENGLISH: FKoefMapMode := 1;
    MM_HIENGLISH: FKoefMapMode := 1;
    MM_TWIPS: FKoefMapMode := 1;
    MM_ISOTROPIC:
      FKoefMapMode := -1;
    MM_ANISOTROPIC:
      FKoefMapMode := -1;
  end;
  if (P^.iMode <> MM_ISOTROPIC) and (P^.iMode <> MM_ANISOTROPIC) then
  begin
    FParamsDraw.SizeViewPort.cx := 1;
    FParamsDraw.SizeViewPort.cy := 1;
    FParamsDraw.BeginViewPortCoord := Point(0, 0);
    FParamsDraw.SizeWindow.cx := 1;
    FParamsDraw.SizeWindow.cy := 1;
    FParamsDraw.BeginWindowCoord := Point(0, 0);
  end;
end;

procedure TsgConvertMetafileToCad.DoSetBkMode(P: PEMRSetBkMode);
begin
  FParamsDraw.BkMode := P^.iMode; // 0 - disable 1 - enable
  if Assigned(FParamsDrawBrush.PData) then
    FUseBackground := FParamsDraw.BkMode = TRANSPARENT
  else
    FUseBackground := True;
end;

procedure TsgConvertMetafileToCad.DoSetPolyFillMode(P: PEMRSetPolyFillMode);
begin
  FParamsDraw.PolyFillMode := P^.iMode;
end;

procedure TsgConvertMetafileToCad.DoSetRop2(P: PEMRSetRop2);
begin
  //SetRop2(FDC,P^.iMode);
  FParamsDraw.Rop := P^.iMode;
end;

procedure TsgConvertMetafileToCad.DoSetStretchBltMode(P: PEMRSetStretchBltMode);
begin
  //SetStretchBltMode(FDC, P^.iMode);
  FParamsDraw.StrenchBltMode := P^.iMode;
end;

procedure TsgConvertMetafileToCad.DoSetColorAdjustment(P: PEMRSetColoradjustment);
begin
  //SetColorAdjustment(FDC, p^.ColorAdjustment);
end;

procedure TsgConvertMetafileToCad.DoSetTextAlign(P: PEMRSetTextAlign);
begin
  FParamsDraw.TextAlign := P^.iMode;
end;

procedure TsgConvertMetafileToCad.DoSetTextColor(P: PEMRSetTextColor);
begin
  FParamsDraw.TextColor := P^.crColor;
end;

procedure TsgConvertMetafileToCad.DoSetBkColor(P: PEMRSetBkColor);
begin
  FParamsDraw.BkColor := P^.crColor;
end;

procedure TsgConvertMetafileToCad.DoOffsetClipRgn(P: PEMROffsetClipRgn);
var
  vPoint: TFPoint;
  vClipBox: TFRect;
begin
  vPoint := PointToFPoint(Point(P^.ptlOffset.X, P^.ptlOffset.Y), True);
  vClipBox := FClipRgn.BoundsRect;
  OffsetFRect(vClipBox, vPoint.X, vPoint.Y, 0);
  FClipRgn.MakeClip(vClipBox, FClipRgn.CurClipMode, nil);
end;

procedure TsgConvertMetafileToCad.DoSetMetaRgn(P: PEMRSetMetaRgn);
begin
  //SetMetaRgn(FDC);
end;

procedure TsgConvertMetafileToCad.DoExcludeClipRect(P: PEMRExcludeClipRect);
begin
  //ExcludeClipRect(FDC,P^.rclClip.Left,P^.rclClip.Top,
  //  P^.rclClip.Right, P^.rclClip.Bottom)
end;

procedure TsgConvertMetafileToCad.DoInterSectClipRect(P: PEMRInterSectClipRect);
begin
  AddOneHatch;
  FClipRgn.InitFromRect(PEMRIntersectClipRect(P)^.rclClip, FClipRgn.CurClipMode, FClipRgn);
end;

procedure TsgConvertMetafileToCad.DoScaleViewPortExtex(P: PEMRScaleViewPortExtex);
begin
end;

procedure TsgConvertMetafileToCad.DoScaleWindowExtex(P: PEMRScaleWindowExtex);
begin
end;

procedure TsgConvertMetafileToCad.DoRestoreDC(P: PEMRRestoreDC);
var
  vItem: TsgDCStackItem;
begin
  AddOneHatch;
  if FDCStack.Count > 0 then
  begin
    AddOneHatch;
    vItem := TsgDCStackItem(FDCStack.Last);
    FDefaultBrush := vItem.DefaultBrush;
    FDefaultPen := vItem.DefaultPen;
    FDefaultFont := vItem.DefaultFont;
    FKoefMapMode := vItem.KoefMapMode;
    FMatrix := vItem.Matrix;
    FParamsDraw := vItem.ParamsDraw;
    FPenPos := vItem.PenPos;
    FParamsDrawBrush := vItem.ParamsDrawBrush;
    FParamsDrawPen := vItem.ParamsDrawPen;
    FParamsText := vItem.ParamsText;
    FClipRgn.Assign(vItem.ClipRgn);
    FParamsPath.Assign(vItem.ParamsPath);
    FDCStack.Remove(vItem);
  end
  else
    FClipRgn.Clear;
end;

procedure TsgConvertMetafileToCad.DoSetWorldTransform(P: PEMRSetWorldTransform);
begin
  SetMatrix(P^.xform);
end;

procedure TsgConvertMetafileToCad.DoModifyWorldTransform(P: PEMRModifyWorldTransform);
var
  vMatrix: TFMatrix;
begin
  AddOneHatch;
  case P^.iMode of
    MWT_IDENTITY:
      SetMatrix(FEXForm);
    MWT_LEFTMULTIPLY:
      begin // modify * current
        vMatrix := TranslateToMatrix(P^.xform);
        FMatrix := FMatXMat(vMatrix, FMatrix);
      end;
    MWT_RIGHTMULTIPLY:
      begin // current * modify
        vMatrix := TranslateToMatrix(P^.xform);
        FMatrix := FMatXMat(FMatrix, vMatrix);
      end;
    else
      SetMatrix(P.xform);
  end;
end;

procedure TsgConvertMetafileToCad.DoSelectObject(P: PEMRSelectObject);
var
  vIndexObj: Integer;
  vGDIObj: PGDIObject;
  vIH: DWORD;

  procedure AddSelectObj;
  begin
    vIndexObj := FindObjectSelect(FGDIObjectsSelect,vGDIObj^.tpObject);
    if vIndexObj > -1 then
      FGDIObjectsSelect.Delete(vIndexObj);
    FGDIObjectsSelect.Add(vGDIObj);
    case vGDIObj^.tpObject of
    tgPen:
      begin
        FParamsDrawPen.tpObject := tgPen;
        FParamsDrawPen.PData := vGDIObj^.PData;
        FParamsDrawPen.index := vGDIObj^.index;
      end;
    tgExtPen:
      begin
        FParamsDrawPen.tpObject := tgExtPen;
        FParamsDrawPen.PData := vGDIObj^.PData;
        FParamsDrawPen.index := vGDIObj^.index;
      end;
    tgBrush:
      begin
        FParamsDrawBrush.tpObject := tgBrush;
        FParamsDrawBrush.PData := vGDIObj^.PData;
        FParamsDrawBrush.index := vGDIObj^.index;
        FUseBackground := False;
      end;
    tgPalette:;
    tgFont:
      begin
        FParamsText.tpObject := tgFont;
        FParamsText.PData := vGDIObj^.PData;
      end;
    end;
  end;

begin
  vIndexObj := P^.ihObject;
  vGDIObj := FindObject(FMapGDIObject, vIndexObj);
  if (vGDIObj <> nil) and (vGDIObj^.tpObject <> tgStandart) then
    AddSelectObj
  else
  begin
    vIndexObj := GetStandartGDIObject(P^.ihObject);
    if vIndexObj > 0 then
    begin
      New(vGDIObj);
      vGDIObj^.tpObject := tgStandart;
      vGDIObj^.iHandle := P^.ihObject;
      vGDIObj^.PData := nil;
      GDIObjectAdd(vGDIObj);
      vIH := DecodeStdObject(vGDIObj^.iHandle);
      case vIH of
        BLACK_BRUSH,DKGRAY_BRUSH,DC_BRUSH,GRAY_BRUSH,WHITE_BRUSH,NULL_BRUSH:
          begin
            AddTextFromSubstrings(nil, nil, False);
            FParamsDrawBrush.tpObject := tgStandart;
            FParamsDrawBrush.PData := @FDefaultBrush;
            FDefaultBrush.lbColor := GetStdObjectColor(vGDIObj^.iHandle);
          end;
        BLACK_PEN,DC_PEN,WHITE_PEN,NULL_PEN:
          begin
            FParamsDrawPen.tpObject := tgStandart;
            FParamsDrawPen.PData := @FDefaultPen;
            FDefaultPen.lp.lopnColor := GetStdObjectColor(vGDIObj^.iHandle);
          end;
      end;
    end;
  end;
end;

procedure TsgConvertMetafileToCad.DoDeleteObject(P: PEMRDeleteObject);
begin
  GDIObjectDelete(P^.ihObject);
end;

procedure TsgConvertMetafileToCad.DoAngleArc(P: PEMRAngleArc);
var
  vNewPenPos: TPoint;
begin
  SetPen;
  FData.Radius := P.nRadius * KoefScale;
  FData.StartAngle := P.eStartAngle;
  FData.EndAngle := P.eStartAngle + P.eSweepAngle;
  FData.Point := PointToFPoint(P^.ptlCenter);
  vNewPenPos := MakePointFrom3D(
    GetPointOnCircle(MakeFPointFromPoint(P^.ptlCenter), P.nRadius,
    FData.EndAngle));
  if P^.eSweepAngle < 0 then
    SwapDoubles(FData.StartAngle, FData.EndAngle);
  InitAndAddEntity(nil, TsgDXFArc);
  SetPosition(vNewPenPos);
end;

procedure TsgConvertMetafileToCad.DoRoundRect(P: PEMRRoundRect);
begin

end;

procedure TsgConvertMetafileToCad.DoSelectPalette(P: PEMRSelectPalette);
{
var
  vIndexObj: Integer;
  vGDIObj: PGDIObject;
}
begin
{
  vIndexObj := P^.ihPal;
  vGDIObj := FindObject(FMapGDIObject,vIndexObj);
  if vGDIObj <> nil then
    SelectObject(FDC,vGDIObj^.Handle);
}
end;

procedure TsgConvertMetafileToCad.DoCreatePalette(P: PEMRCreatePalette);
var
  vPalette: PLogPalette;
  vTypeObject: PGDIObject;
begin
  New(vPalette);
  vPalette^ := P^.lgpl;
  New(vTypeObject);
  vTypeObject^.tpObject := tgPalette;
  vTypeObject^.iHandle := P^.ihPal;
  vTypeObject^.PData := vPalette;
  GDIObjectAdd(vTypeObject);
end;

procedure TsgConvertMetafileToCad.DoSetPaletteEntries(P: PEMRSetPaletteEntries);
{
var
  vIndexObj: Integer;
  vGDIObj: PGDIObject;
}
begin
{
  vIndexObj := P^.ihPal;
  vGDIObj := FindObject(FMapGDIObject,vIndexObj);
  if vGDIObj <> nil then
    SetPaletteEntries(vGDIObj^.Handle,
      P^.iStart,P^.cEntries,P^.aPalEntries);
}
end;

procedure TsgConvertMetafileToCad.DoResizePalette(P: PEMRResizePalette);
{
var
  vIndexObj: Integer;
  vGDIObj: PGDIObject;
}
begin
{
  vIndexObj := P^.ihPal;
  vGDIObj := FindObject(FMapGDIObject,vIndexObj);
  if vGDIObj <> nil then
    ResizePalette(vGDIObj^.Handle, P^.cEntries);
}
end;

procedure TsgConvertMetafileToCad.DoRealizePalette(P: PEMRRealizePalette);
begin
end;

procedure TsgConvertMetafileToCad.DoExtFloodFill(P: PEMRExtFloodFill);
begin
end;

procedure TsgConvertMetafileToCad.DoPolyDraw16(P: PEMRPolyDraw16);
begin
end;

procedure TsgConvertMetafileToCad.DoPolyDraw(P: PEMRPolyDraw);
begin
end;

procedure TsgConvertMetafileToCad.DoSetArcDirection(P: PEMRSetArcDirection);
begin
  FParamsDraw.ArcDirection := P^.iArcDirection;
end;

procedure TsgConvertMetafileToCad.DoCreateDibPatternBrushPt(P: PEMRCreateDibPatternBrushPt);
begin
end;

procedure TsgConvertMetafileToCad.DoCreateMonoBrush(P: PEMRCreateMonoBrush);
begin
end;

procedure TsgConvertMetafileToCad.DoSetMiterLimit(P: PEMRSetMiterLimit);
begin
end;

procedure TsgConvertMetafileToCad.DoBeginPath(P: PEMRBeginPath);
begin
  FBeginPath := True;
  FDrawedPath := False;
  FParamsPath.StartPath;
end;

procedure TsgConvertMetafileToCad.DoEndPath(P: PEMREndPath);
begin
  FParamsPath.StopPath;
  FBeginPath := False;
end;

procedure TsgConvertMetafileToCad.DoCloseFigure(P: PEMRCloseFigure);
var
  vLine: PsgLine;
begin
  FParamsPath.CloseFigure;
  vLine := FParamsPath.ClosingLine;
  if Assigned(vLine) then
  begin
    AddLine(vLine.Point1, vLine.Point2);
    FParamsPath.SaveStartFigure;
  end;
end;

procedure TsgConvertMetafileToCad.DoFillPath(P: PEMRFillPath);
begin
  DoStrokePath(P, not FFillingEntity, FFillingEntity);
end;

procedure TsgConvertMetafileToCad.DoStrokeAndFillPath(P: PEMRStrokeAndFillPath);
begin
  DoStrokePath(P, FFillingEntity, FFillingEntity);
end;

procedure TsgConvertMetafileToCad.DoFlattenPath(P: PEMRFlattenPath);
begin
end;

procedure TsgConvertMetafileToCad.DoStrokePath(P: PEMRStrokePath;
  Contour: Boolean = True; Fill: Boolean = False);
var
  vLogBrush: TLogBrush;
  vList: TsgObjectList;

  procedure DrawContour(AList: TsgObjectList);
  begin
    FDrawedPath := True;
    AddContour(AList);
  end;

begin
  vLogBrush := TLogBrush(FParamsDrawBrush.PData^);
  SetHatch(vLogBrush.lbColor, vLogBrush.lbHatch + 2, vLogBrush.lbStyle);
  vList := nil;
  case FParamsDraw.PolyFillMode of
    Alternate:
      begin
        if FParamsPath.IsNeedColse and Fill then
          DoCloseFigure(nil);
        vList := FParamsPath.ActivePath;
      end;
    Winding:
      vList := FParamsPath.ListFiguresInPath;
  end;
  if not ExtractText(vList, (not Fill) and Contour) then
  begin
    if (vList <> nil) and Fill then
    begin
      if IsFillContour(Fill) then
        DoAddHatch(vList, FParamsPath.HatchInPath.Count <= 1)
      else
        Contour := True;
    end;
    if Contour and IsDrawContour(FParamsPath.ListFiguresInPath, Contour) then
      DrawContour(FParamsPath.ListFiguresInPath);
  end;
  FParamsPath.Clear(not FDrawedPath);
end;

procedure TsgConvertMetafileToCad.DoWidenPath(P: PEMRWidenPath);
begin
end;

procedure TsgConvertMetafileToCad.DoSelectClipPath(P: PEMRSelectClipPath);
var
  i: Integer;
  vEntity: TsgDXFEntity;
  vPoly: TsgCADBasePolyline absolute vEntity;
  vClipPath: TFPointList;
  vBlock: TsgDXFBlock;

  procedure AddBasePolyline(APolyline: TsgCADBasePolyline;
    AClipPath: TFPointList);
  var
    J: Integer;
  begin
    for J := 0 to APolyline.PointCount - 1 do
      AClipPath.Add(APolyline.Points[J]);
  end;

begin
  vBlock := nil;
  vClipPath := TFPointList.Create;
  try
    for i := 0 to FParamsPath.ListFiguresInPath.Count - 1 do
    begin
      vEntity := TsgDXFEntity(FParamsPath.ListFiguresInPath[i]);
      if vEntity is TsgCADBasePolyline then
      begin
        AddBasePolyline(vPoly, vClipPath);
        if vPoly.Closed and (vPoly.PointCount > 2) then
        begin
          FClipRgn.InitFromPath(vClipPath, P.iMode, vBlock);
          if Assigned(FClipRgn.CurInsert) then
            vBlock := FClipRgn.CurInsert.Block;
          vClipPath.Clear;
        end;
      end
      else
        if vEntity is TsgDXFLine then
        begin
          vClipPath.Add(TsgDXFLine(vEntity).Point);
          vClipPath.Add(TsgDXFLine(vEntity).Point1);
        end;
    end;
    if vClipPath.Count > 0 then
      FClipRgn.InitFromPath(vClipPath, P.iMode, vBlock);
  finally
    vClipPath.Free;
    FParamsPath.ClearLists(True);
  end;
end;

procedure TsgConvertMetafileToCad.DoAbortPath(P: PEMRAbortPath);
begin
  FParamsPath.Clear(True);
end;

{procedure TsgConvertMetafileToCad.SetDefaultDataRegion(var DataRegion: TDataRegion);
begin
end;}

procedure TsgConvertMetafileToCad.DoPaintRgn(P: PEMRPaintRgn);
var
  DataRegion: TDataRegion;
begin
  FillMemory(@DataRegion,SizeOf(TDataRegion),255);
  DataRegion.rct := P^.rclBounds;
  DataRegion.SizeRgn := P^.cbRgnData;
  DataRegion.RgnData := @(P^.RgnData);
  DoRegion(DataRegion);
end;

procedure TsgConvertMetafileToCad.DoInvertRgn(P: PEMRInvertRgn);
var
  DataRegion: TDataRegion;
begin
  FillMemory(@DataRegion,SizeOf(TDataRegion),255);
  DataRegion.rct := P^.rclBounds;
  DataRegion.SizeRgn := P^.cbRgnData;
  DataRegion.RgnData := @(P^.RgnData);
  DoRegion(DataRegion,1);
end;

procedure TsgConvertMetafileToCad.DoFillRgn(P: PEMRFillRgn);
var
  DataRegion: TDataRegion;
  vGDIObj: PGDIObject;
  vIndex: Integer;
begin
  FillMemory(@DataRegion,SizeOf(TDataRegion),255);
  DataRegion.rct := P^.rclBounds;
  DataRegion.SizeRgn := P^.cbRgnData;
  DataRegion.RgnData := @(P^.RgnData);
  vIndex := P^.ihBrush;
  vGDIObj := FindObject(FMapGDIObject,vIndex);
  if vGDIObj <> nil then
  begin
    DataRegion.hBrush := CreateBrushIndirect(PLogBrush(vGDIObj.PData)^);
    try
      DoRegion(DataRegion,2);
    finally
      DeleteObject(DataRegion.hBrush)
    end;
  end
  else
  begin
    vIndex := GetStandartGDIObject(P^.ihBrush);
    DataRegion.hBrush := vIndex;
    DoRegion(DataRegion,2);
  end;
end;

procedure TsgConvertMetafileToCad.DoFrameRgn(P: PEMRFrameRgn);
var
  DataRegion: TDataRegion;
  vGDIObj: PGDIObject;
  vIndex: Integer;
begin
  FillMemory(@DataRegion,SizeOf(TDataRegion),255);
  DataRegion.rct := P^.rclBounds;
  DataRegion.SizeRgn := P^.cbRgnData;
  DataRegion.RgnData := @(P^.RgnData);
  vIndex := P^.ihBrush;
  vGDIObj := FindObject(FMapGDIObject,vIndex);
  DataRegion.szStroke := P^.szlStroke;
  if vGDIObj <> nil then
  begin
    DataRegion.hBrush := CreateBrushIndirect(PLogBrush(vGDIObj.PData)^);
    try
      DoRegion(DataRegion, 3);
    finally
      DeleteObject(DataRegion.hBrush);
    end;
  end
  else
  begin
    vIndex := GetStandartGDIObject(P^.ihBrush);
    DataRegion.hBrush := vIndex;
    DoRegion(DataRegion,3);
  end;
end;

procedure TsgConvertMetafileToCad.DoRegion(var DataRegion: TDataRegion; TypeRgn: Byte = 0);
var
  vBmp: TBitmap;
  vRgn: HRGN;
  vImageEnt: TsgDXFImageEnt;
  vWidth,vHeight: Integer;
  vXForm: TXForm;
  vPen,vBrush: THandle;
begin
  DataRegion.rct.Right := DataRegion.rct.Right + 1;
  DataRegion.rct.Bottom := DataRegion.rct.Bottom + 1;

  vXForm := TranslateToXForm(FMatrix);
  vRgn := ExtCreateRegion(@vXForm, DataRegion.SizeRgn, DataRegion.RgnData^);

  if vRgn > 0 then
  begin
    vPen := CreatePen;
    vBrush := CreateBrush;
    vBmp := TBitmap.Create;
    try
      vWidth := Abs(DataRegion.rct.Right - DataRegion.rct.Left);
      vHeight := Abs(DataRegion.rct.Bottom - DataRegion.rct.Top);

      vBmp.Width := vWidth;
      vBmp.Height := vHeight;
      vBmp.Canvas.Pen.Handle := vPen;
      vBmp.Canvas.Brush.Handle := vBrush;

      SetWindowOrgEx(vBmp.Canvas.Handle,
        DataRegion.rct.Left,DataRegion.rct.Top,nil);

      case TypeRgn of
        0:
        PaintRgn(vBmp.Canvas.Handle,vRgn);
        1:
        InvertRgn(vBmp.Canvas.Handle,vRgn);
        2:
          if DataRegion.hBrush > 0 then
          begin
            if not FillRgn(vBmp.Canvas.Handle,vRgn,DataRegion.hBrush) then
              Exit;
          end
          else
            if not FillRgn(vBmp.Canvas.Handle,vRgn,GetStockObject(BLACK_BRUSH)) then
              Exit;
        3:
          if DataRegion.hBrush > 0 then
          begin
            if not FrameRgn(vBmp.Canvas.Handle,vRgn,DataRegion.hBrush,
              DataRegion.szStroke.cx,DataRegion.szStroke.cy) then
                Exit;
          end
          else
            if not FrameRgn(vBmp.Canvas.Handle,vRgn,
              GetStockObject(BLACK_BRUSH),
              DataRegion.szStroke.cx,DataRegion.szStroke.cy) then
                Exit;
      end;

      vImageEnt := TsgDXFImageEnt.Create;
      vImageEnt.SetImage(vBmp);
      vImageEnt.Point := PointToFPoint(Point(DataRegion.rct.Left, DataRegion.rct.Top));
      vImageEnt.Size := PointToFPoint(vBmp.Width, vBmp.Height, True);

      AddEntity(nil, vImageEnt);
    finally
      vBmp.Free;
      DeleteObject(vPen);
      DeleteObject(vBrush);
    end;
  end;
end;

procedure TsgConvertMetafileToCad.DoExtSelectClipRgn(P: PEMRExtSelectClipRgn);
begin
  AddOneHatch;
  FClipRgn.InitFromData(PRgnData(@PEMRExtSelectClipRgn(P).RgnData), P.iMode);
end;

procedure TsgConvertMetafileToCad.DoMaskBlt(P: PEMRMaskBlt);
begin

end;

procedure TsgConvertMetafileToCad.DoPlgBlt(P: PEMRPlgBlt);
begin

end;

procedure TsgConvertMetafileToCad.DoSetDIBItstoDevice(P: PEMRSetDIBItstoDevice);
begin
end;

procedure TsgConvertMetafileToCad.DoStretchDIBIts(P: PEMRStretchDIBIts);
var
  vData: Pointer;
  vImgHeader: PBitmapInfo;
begin
  vData := GetPointerByOffset(P, P^.offBitsSrc);
  vImgHeader := GetPointerByOffset(P, P^.offBmiSrc);
  AddImageEnt(vImgHeader, vData, Point(P^.xDest, P^.yDest),
    Point(P^.cxSrc, P^.cySrc), Point(P^.cxDest, P^.cyDest),
    FParamsDraw.BkColor, P^.iUsageSrc, P^.dwRop);
end;

procedure TsgConvertMetafileToCad.DoExtCreareFontIndirectw(P: PEMRExtCreateFontIndirect);
var
  vTypeObject: PGDIObject;
  vLogFont: PLogFontW;
begin
  New(vLogFont);
  vLogFont^ := P^.elfw.elfLogFont;
  New(vTypeObject);
  vTypeObject^.tpObject := tgFont;
  vTypeObject^.iHandle := P^.ihFont;
  vTypeObject^.PData := vLogFont;
  GDIObjectAdd(vTypeObject);
end;

procedure TsgConvertMetafileToCad.DoExtTextOutA(P: PEMRExtTextOut);
var
  S: sgRawByteString;
begin
  SetString(S, PAnsiChar(UIntPtr(P) + P^.emrtext.offString), P^.emrtext.nChars);
  DoExtTextOut(P, WideString(S));
end;

procedure TsgConvertMetafileToCad.DoExtTextOutW(P: PEMRExtTextOut);
var
  S: WideString;
begin
  SetString(S, PWideChar(UIntPtr(P) + P^.emrtext.offString), P^.emrtext.nChars);
  DoExtTextOut(P, S);
end;

procedure TsgConvertMetafileToCad.DoEOF(P: PEMREof);
begin
end;

procedure TsgConvertMetafileToCad.DoExtTextOut(P: PEMRExtTextOut;
  const AText: WideString);

  function GetOffset(const AHeight,AEscapement: Double): TF2DPoint;
  var
    vAbsH: Double;
    vAngle: Double;
  begin
    vAbsH := Abs(AHeight) * 0.1975 * KoefScale;
    vAngle := Radian(0.1* AEscapement);
    Result := MakeF2DPoint(vAbsH * Sin(vAngle), vAbsH * Cos(vAngle));
  end;

  function GetTextHeight(AFont: TLogFontW): Double;
  var
    vPt: TFPoint;
  begin
    vPt := MakeFPoint(0, AFont.lfHeight * KoefScale * 0.605);
    if IsZero(FMatrix.V2[1]) then
      vPt.Y := vPt.Y * FMatrix.V1[1]
    else
      vPt.Y := vPt.Y * FMatrix.V2[1];
    SetOnlyPt(vPt);
    Result := Abs(vPt.Y);
  end;

  function GetTextScale(AFont: TLogFontW): Double;
  var
    vPt: TFPoint;
  begin
    vPt := MakeFPoint(Abs(AFont.lfWidth) * KoefScale / 0.64, 0, 0);
    if IsZero(FMatrix.V1[1]) then
      vPt.X := vPt.X * FMatrix.V2[1]
    else
      vPt.X := vPt.X * FMatrix.V1[1];
    SetOnlyPt(vPt);
    Result := Abs(vPt.X)/GetTextHeight(AFont);
    if Result <= fAccuracy then
      Result := 1.0;
  end;

  function GetSignYExt: Integer;
  begin
    Result := 1;
    if FParamsDraw.SizeWindow.cy <> 0 then
    begin
      Result := sgSignZ(FParamsDraw.SizeViewPort.cy / FParamsDraw.SizeWindow.cy);
      if Result = 0 then
        Result := 1;
    end;
  end;

var
  vLogFont: TLogFontW;
  vOffset: TF2DPoint;
  vIsUpdatePos: Boolean;
  vPos: TFPoint;
  vText: TsgDXFText;
  vFontFace: TsgFontFace;
begin
  if Trim(AText) = '' then
    Exit;
  vIsUpdatePos := FParamsDraw.TextAlign and TA_UPDATECP <> 0;
  if vIsUpdatePos then
    vPos := PointToFPoint(FPenPos, False)
  else
    vPos := PointToFPoint(P^.emrtext.ptlReference, False);
  vText := TsgDXFText.Create;
  try
    vText.Text := AText;
    vText.Point := vPos;
    if Assigned(FParamsText.PData) then
    begin
      vLogFont := TLogFontW(FParamsText.PData^);
      vFontFace := TextStyleToFontFace(vLogFont);
    end
    else
    begin
      vFontFace := TextStyleToFontFace(FDefaultFont);
      vLogFont := FDefaultFont;
    end;
    vOffset := GetOffset(vLogFont.lfHeight, vLogFont.lfEscapement);
    vText.Style := GetTextStyle(vFontFace, FCADImage);
    vText.Scale := GetTextScale(vLogFont);
    vText.Rotation := vLogFont.lfEscapement / 10;
    vText.ColorCad := TranslateColor(FParamsDraw.TextColor, tgFont);
    vText.Height := GetTextHeight(vLogFont);
    {
    case FParamsDraw.TextAlign of
      TA_CENTER: vText.HAlign := 1;
      TA_RIGHT: vText.HAlign := 2;
      TA_BASELINE, TA_BOTTOM: vText.HAlign := 6;
      (TA_CENTER or TA_BOTTOM): vText.HAlign := 7;
      (TA_RIGHT or TA_BOTTOM): vText.HAlign := 8;
    end;
    }
    if FParamsDraw.TextAlign = 0 then
    begin
      vText.VAlign := 3;
      vPos.Y := vPos.Y - vOffset.Y * GetSignYExt;
      vText.Point1 := vPos;
      vPos.Y := vPos.Y - vText.Height * GetSignYExt;
      vText.Point := vPos;
    end
    else
      if (FParamsDraw.TextAlign = TA_BASELINE) or (FParamsDraw.TextAlign and TA_BOTTOM = 0) then
      begin
        vPos.X := vPos.X + vOffset.X;
        vPos.Y := vPos.Y - vOffset.Y * GetSignYExt;
        vText.Point := vPos;
      end;
    AddEntity(FCADImage, vText, False, True);
  except
    vText.Free;
  end;
end;

procedure TsgConvertMetafileToCad.DoPolyTextOut(P: PEMRPolyTextOut);
begin
  //PolyTextOut(FDC,P^.aemrtext,P^.cStrings);
end;

procedure TsgConvertMetafileToCad.DoSetICMMode(P: PEMRSetICMMode);
begin
  {SetICMMode(FDC,P^.iMode);
  Exit;}

  case P^.iMode of
  Icm_On:;
  Icm_Off:;
  Icm_Query:;
  //Icm_Done_OutSideDC:;
  end;
  FICMMode := P^.iMode;
end;

procedure TsgConvertMetafileToCad.DoCreateColorSpace(P: PEMRCreateColorSpace);
begin
  //CreateColorSpace(
end;

procedure TsgConvertMetafileToCad.DoSetColorSpace(P: PEMRSelectColorSpace);
begin
  //SetColorSpace(FDC,
end;

procedure TsgConvertMetafileToCad.DoDeleteColorSpace(P: PEMRDeleteColorSpace);
begin
end;

procedure TsgConvertMetafileToCad.DoGLSRecord(P: PEMRGLSRecord);
begin

end;

procedure TsgConvertMetafileToCad.DoGLSBoundedRecord(P: PEMRGLSBoundedRecord);
begin
end;

procedure TsgConvertMetafileToCad.DoPixelFormat(P: PEMRPixelFormat);
begin
  //SetPixelFormat(FDC,0,@(P^.pfd));
end;

{procedure TsgMetafiles.DoDrawEscape(P: DrawEscape);
begin
end;

procedure TsgMetafiles.DoExtEscape(P: PEMRExtEscape);
begin
end;}

{procedure TsgMetafiles.DoStartDoc(P: PEMRStartDoc);
begin
end;}

{procedure TsgMetafiles.DoSmallTextOut(P: PEMRSmallTextOut);
begin
end;}

{procedure TsgMetafiles.DoForceUfiMapping(P: PEMRForceUfiMapping);
begin
end;}

{procedure TsgMetafiles.DoNamedEscape(P: PEMRNamedEscape);
begin
end;}

{procedure TsgMetafiles.DoColorCorrectPalette(P: PEMRColorCorrectPalette);
begin
end;}

{procedure TsgMetafiles.DoSetICMProfile(P: PEMRSetICMProfile);
begin
end;}

procedure TsgConvertMetafileToCad.DoAlphaBlend(P: Pointer);// ?PEMRAlphaBlend
begin
  // not support before Windows 98 and Windows 2000...

end;

{procedure TsgMetafiles.DoAlphaDIBBlend(P: PEMRAlphaDIBBlend);
begin
end;}

procedure TsgConvertMetafileToCad.DoTransparentBlt(P: Pointer);// ?PEMRTransparentBlt
var
  vData: Pointer;
  vImgHeader: PBitmapInfo;
  vP: PEMRTransparentBlt;
begin
  vP := PEMRTransparentBlt(P);
  vData := GetPointerByOffset(P, vP^.offBitsSrc);
  vImgHeader := GetPointerByOffset(P, vP^.offBmiSrc);
  AddImageEnt(vImgHeader, vData, Point(vP^.xDest, vP^.yDest),
    Point(vP^.cxDest, vP^.cyDest), Point(vP^.cxDest, vP^.cyDest),
    vP^.crBkColorSrc, vP^.iUsageSrc, vP^.dwRop);
end;

{procedure TsgMetafiles.DoTransparentDIB(P: PEMRTransparentDIB);
begin
end;}

procedure TsgConvertMetafileToCad.DoGradientFill(P: Pointer);// ?PEMGradientFill
begin
end;

{procedure TsgMetafiles.DoSetLinkedUfIs(P: PEMRSetLinkedUfIs);
begin
end;}

{procedure TsgMetafiles.DoSetTextJustification(P: PEMRSetTextJustification);
begin
end;}

procedure TsgConvertMetafileToCad.DoGDIComment(P: PEMRGDIComment);
begin

end;

function TsgConvertMetafileToCad.GenerateArc(const AStartPt,AEndPt: TPoint; const ARect: TRect): TdxfExportData;
var
  vPt: TFPoint;

  function AngleCircle(const A: TFPoint): Extended;
  begin
    vPt := A;
    Result := 180 / Pi * ArcTan2(vPt.Y - FData.Point.Y, vPt.X - FData.Point.X);
  end;

begin
  SetPen;
  if (ARect.Right = ARect.Left) or (ARect.Bottom = ARect.Top) then
    Exit;
  if SetCircle(ARect) then
  begin
    // Default StartAngle=0 and EndAngle=360
    if not ((AStartPt.X = AEndPt.X) and (AStartPt.Y = AEndPt.Y)) then
    begin
      FData.StartAngle := AngleCircle(PointToFPoint(AStartPt));
      FData.EndAngle := AngleCircle(PointToFPoint(AEndPt));
      FData.SelfType := Byte(atArc);
    end
    else
      FData.SelfType := Byte(atCircle);
  end
  else
  begin
    if not ((AStartPt.X = AEndPt.X) and (AStartPt.Y = AEndPt.Y)) then
    begin
      FData.StartAngle := GetAngleParam(FData.Point, FData.Point1,
        FData.Ratio, PointToFPoint(AStartPt));
      FData.EndAngle := GetAngleParam(FData.Point, FData.Point1,
        FData.Ratio, PointToFPoint(AEndPt));
    end
    else
    begin
      // StartAngle=0 and EndAngle=2*Pi for full Ellipse
      FData.StartAngle := 0;
      FData.EndAngle := 360;
    end;
    FData.SelfType := Byte(atEllipse);
  end;
  Result := FData;
end;

procedure TsgConvertMetafileToCad.GetBlocksAndInserts(const AImage: TsgCADImage;
  var ABlock,AOwner: TsgDXFBlock; var ACurInsert: TsgDXFInsert);
var
  vOwnedInsert: TsgDXFInsert;
  vImage: TsgCADImage;
begin
  ABlock := nil;
  AOwner := nil;
  ACurInsert := nil;
  if Assigned(AImage) then
    vImage := AImage
  else
    vImage := FCADImage;
  if vImage = FCADImage then
  begin
    if FClipRgn.IsClipping then
    begin
      vOwnedInsert := FClipRgn.OwnedInsert;
      ACurInsert := FClipRgn.CurInsert;
      ABlock := ACurInsert.Block;
      if Assigned(vOwnedInsert) then
        AOwner := vOwnedInsert.Block
      else
        AOwner := vImage.CurrentLayout.PaperSpaceBlock;
    end
    else
      ABlock := vImage.CurrentLayout.PaperSpaceBlock;
  end
  else
    ABlock := vImage.CurrentLayout.PaperSpaceBlock;
end;

function TsgConvertMetafileToCad.DoArc(P: PEMRArc; IsTo: Boolean = False;
  ATryToFill: Boolean = False; AHatch: Boolean = False): TsgDXFEntity;
var
  vArcR: TdxfExportData;
  vArc: TsgDXFEntityClass;
  vNewPenPos: TPoint;
begin
  Result := nil;
  if FParamsDraw.ArcDirection = AD_COUNTERCLOCKWISE then
  begin
    vArcR := GenerateArc(P^.ptlStart , P^.ptlEnd, P^.rclBox);
    vNewPenPos := P^.ptlEnd;
  end
  else
  begin
    vArcR := GenerateArc(P^.ptlStart, P^.ptlEnd, P^.rclBox);
    vNewPenPos := P^.ptlStart;
  end;

  case TsgArcType(vArcR.SelfType) of
    atCircle:
      vArc := TsgDXFCircle;
    atArc:
      vArc := TsgDXFArc;
    atEllipse:
      vArc := TsgDXFEllipse;
  else
    vArc := nil;
  end;

  if vArc <> nil then
    Result := InitAndAddEntity(nil, vArc, ATryToFill, AHatch);

  if IsTo then
    SetPosition(vNewPenPos);
end;

procedure TsgConvertMetafileToCad.DoChord(P: PEMRChord);
var
  vPoints: TFPointList;
begin
  SetPen;
  SetHatch(FParamsDraw.BkColor);
  vPoints := TFPointList.Create;
  try
    SetPolyline(vPoints, 2, True);
    vPoints.Add(PointToFPoint(P^.ptlStart));
    vPoints.Add(PointToFPoint(P^.ptlEnd));
    InitAndAddEntity(nil, TsgDXFLWPolyline, IsFillContour(True));
  finally
    vPoints.Free;
  end;
end;

procedure TsgConvertMetafileToCad.DoPie(P: PEMRPie);
var
  vRadius,vStartAngle,vEndAngle: Double;
  vCenter: TFPoint;
begin
  AddOneHatch;
  DoArc(PEMRArc(P), False, True, True);
  vCenter := FData.Point;
  vRadius := FData.Radius;
  vStartAngle := FData.StartAngle;
  vEndAngle := FData.EndAngle;
  AddLine(vCenter, GetPointOnCircle(vCenter, vRadius, vStartAngle), True, True);
  AddLine(vCenter, GetPointOnCircle(vCenter, vRadius, vEndAngle), True, True);
  FData.ColorCAD := TranslateColor(FParamsDraw.BkColor, tgBrush);
  AddOneHatch;
end;

procedure TsgConvertMetafileToCad.DoSaveDC(P: PEMRSaveDC);
var
  vItem: TsgDCStackItem;
begin
  vItem := TsgDCStackItem.Create(self);
  vItem.DefaultBrush := FDefaultBrush;
  vItem.DefaultPen := FDefaultPen;
  vItem.DefaultFont := FDefaultFont;
  vItem.KoefMapMode := FKoefMapMode;
  vItem.Matrix := FMatrix;
  vItem.ParamsDraw := FParamsDraw;
  vItem.PenPos := FPenPos;
  vItem.ParamsDrawBrush := FParamsDrawBrush;
  vItem.ParamsDrawPen := FParamsDrawPen;
  vItem.ParamsText := FParamsText;
  vItem.ClipRgn := FClipRgn;
  vItem.ParamsPath := FParamsPath;
  FDCStack.Add(vItem);
end;

procedure TsgConvertMetafileToCad.DoCreatePen(P: PEMRCreatePen);
var
  vTypeObject: PGDIObject;
  vLogPen: PsgLogPen;
begin
  New(vLogPen);
  FillChar(vLogPen^, SizeOf(vLogPen^), #0);
  vLogPen^.lp := P^.lopn;
  New(vTypeObject);
  vTypeObject^.index := FCommandIndex;
  vTypeObject^.tpObject := tgPen;
  vTypeObject^.iHandle := P^.ihPen;
  vTypeObject^.PData := vLogPen;
  GDIObjectAdd(vTypeObject);
end;

procedure TsgConvertMetafileToCad.DoExtCreatePen(P: PEMRExtCreatePen);
var
  vTypeObject: PGDIObject;
  vLogPen: PsgLogPen;
begin
  New(vLogPen);
  vLogPen^.lb.lbStyle := P^.elp.elpBrushStyle;
  vLogPen^.lb.lbColor := P^.elp.elpColor;
  vLogPen^.lb.lbHatch := P^.elp.elpHatch;
  vLogPen^.lp.lopnStyle := P^.elp.elpPenStyle;
  vLogPen^.lp.lopnWidth.X := P^.elp.elpWidth;
  vLogPen^.lp.lopnColor := P^.elp.elpColor;

  if P^.elp.elpNumEntries > 0 then
  begin
    SetLength(vLogPen^.Styles, P^.elp.elpNumEntries);
    CopyMemory(@(vLogPen^.Styles[0]), @(P^.elp.elpStyleEntry),
      P^.elp.elpNumEntries*SizeOf(DWORD));
  end;

  New(vTypeObject);
  vTypeObject^.index := FCommandIndex;
  vTypeObject^.tpObject := tgExtPen;
  vTypeObject^.iHandle := P^.ihPen;
  vTypeObject^.PData := vLogPen;
  GDIObjectAdd(vTypeObject);
end;

function TsgConvertMetafileToCad.AddEntity(const ACADImage: TsgCADImage;
  const AEntity: TsgDXFEntity; const ATryToFill: Boolean = False;
  const AHatch: Boolean = False; AClip: TObject = nil): TsgDXFEntity;
var
  vImage: TsgCADImage;
  vBlock,vOwner: TsgDXFBlock;
  vCurInsert: TsgDXFInsert;
  vClip: TsgRegionClip absolute AClip;
  vAbsBlock: TsgDXFBlock absolute AClip;
begin
  if ACADImage = nil then
    vImage := FCADImage
  else
    vImage := ACADImage;
  if not AHatch then
    AddOneHatch;
  if (not FBeginPath) or AHatch then
  begin
    if ATryToFill then
      FListEnts.Add(AEntity)
    else
    begin
      AEntity.Layer := FLayer;
      if Assigned(FOwner) then
        FOwner.Add(AEntity)
      else
      begin
        vBlock := nil;
        vOwner := nil;
        vCurInsert := nil;
        if Assigned(AClip) then
        begin
          if AClip is TsgRegionClip then
          begin
            vCurInsert := vClip.Insert;
            vBlock := vCurInsert.Block;
            if Assigned(vClip.Owner) then
              vOwner := vClip.Owner.Insert.Block
            else
              vOwner := vImage.CurrentLayout.PaperSpaceBlock;
          end
          else
            if AClip is TsgDXFBlock then
              vBlock := vAbsBlock;
        end
        else
          GetBlocksAndInserts(vImage, vBlock, vOwner, vCurInsert);
        //vBlock.FIndex := FCommandIndex;
        if (AEntity is TsgCADCurvePolygon) and IsTransparentHatch then
        begin
          if Assigned(vCurInsert) and Assigned(vOwner) and (vBlock <> vOwner) then
            if vOwner.RemoveEntity(vCurInsert) then
              vOwner.AddEntity(vCurInsert);
        end;
        vBlock.AddEntity(AEntity);
        FClipRgn.MarkIsWorked;
      end;
    end;
  end
  else
    FParamsPath.Add(AEntity);
  vImage.Converter.Loads(AEntity);
  UnionFRect(FEntitiesBox, AEntity.Box);
  Result := AEntity;
  FData.Points := nil;
  //AEntity.FIndex := FCommandIndex;
end;

procedure TsgConvertMetafileToCad.AddPoints(APointList: TFPointlist;
  APoints: PPoint; ACount: Integer; AIsTo: Boolean = False);
var
  I: Integer;
begin
  if AIsTo then
    APointList.Add(PointToFPoint(FPenPos));
  for I := 0 to ACount - 1 do
  begin
    APointList.Add(PointToFPoint(APoints^));
    Inc(APoints);
  end;
  APointList.Add(PointToFPoint(APoints^));
  if AIsTo then
    SetPosition(APoints^);
end;

procedure TsgConvertMetafileToCad.AddPoints(APointList: TFPointlist;
  APoints: PsgSmallPoint; ACount: Integer; AIsTo: Boolean = False);
var
  I: Integer;
begin
  if AIsTo then
    APointList.Add(PointToFPoint(FPenPos));
  for I := 0 to ACount - 1 do
  begin
    APointList.Add(PointToFPoint(APoints^));
    Inc(APoints);
  end;
  APointList.Add(PointToFPoint(APoints^));
  if AIsTo then
    SetPosition(APoints^);
end;

procedure TsgConvertMetafileToCad.DoOnProgress(AStage: TProgressStage;
  ADone, ACount: Integer);
var
  vProgress: Byte;
begin
  if AStage = psStarting then
    FCompleted := 0;
  if Assigned(FOnProgress) then
  begin
    if ACount = 0 then
      vProgress := 0
    else
      vProgress := Byte(MulDiv(ADone, 100, ACount));
    if vProgress - FCompleted >= 5 then
    begin
      FCompleted := vProgress;
      FOnProgress(Self, AStage, vProgress, False, cnstRectZero, Format('%d/%d', [ADone, ACount]));
    end;
  end;
end;

function TsgConvertMetafileToCad.PointToFPoint(const AX, AY: Double;
  AIsSize: Boolean = False): TFPoint;
var
  vM: TFMatrix;
  vOffset: TF2DPoint;
begin
  Result := cnstFPointZero;
  vOffset := SubF2DPoint(FOffsetPoint, FOffsetPointInternal);
  if AIsSize then
    Result := MakeFPoint(AX, AY)
  else
  begin
    Result := MakeFPoint(
      AX + FParamsDraw.BeginViewPortCoord.X - FParamsDraw.BeginWindowCoord.X,
      AY + FParamsDraw.BeginViewPortCoord.Y - FParamsDraw.BeginWindowCoord.Y);
    SetOnlyPt(Result);
  end;
  vM := FMatByScales(KoefScaleX*FMetafileXRatio, KoefScaleY*FKoefMapMode, KoefScale);
  if AIsSize then
  begin
    Result := MultiplyFPoint(AffineTransformPoint(AbsFPoint(Result), FMatrix), SignFPoint(Result));
    Result := AffineTransformPoint(Result, vM);
  end
  else
  begin
    vM.E0 := MakeFPoint(FOwnerBox.Left + vOffset.X, FOwnerBox.Top + vOffset.Y);
    Result.Point2D := ClipPoint2DByRect(FMetafileBox, FPointXMat2D(Result.Point2D, FMatrix));
    Result.Point2D := FPointXMat2D(Result.Point2D, vM);
  end;
end;

function TsgConvertMetafileToCad.PointToFPoint(const P: TPoint;
  AIsSize: Boolean = False): TFPoint;
var
  vPoint: TFPoint;
  vMat: TFMatrix;
begin
  Result := PointToFPoint(P.X, P.Y, AIsSize);
  if FParamsPath.BeginPath and (not AIsSize) then
  begin
    vMat := FMatXMat(FMatByScales(1, 1, 1), FMatrix);
    vPoint := FPointXMat(MakeFPointFromPoint(P), vMat);
    FParamsPath.AddPoint(P, vPoint);
  end;
end;

function TsgConvertMetafileToCad.PointToFPoint(const P: TSmallPoint): TFPoint;
var
  vPt: TPoint;
begin
  vPt.X := P.x;
  vPt.Y := P.y;
  Result := PointToFPoint(vPt);
end;

procedure TsgConvertMetafileToCad.DrawPoint(const ACoord: TPoint; AColor: TColor);
begin
  SetPen;
  FData.Point := PointToFPoint(ACoord);
  InitAndAddEntity(nil, TsgDXFPoint);
end;

function TsgConvertMetafileToCad.GetTextStyle(const AFontFace: TsgFontFace;
  ACADImage: TsgCADImage = nil): TsgDXFStyle;
var
  vStyleId, vPrimaryFont: string;

  function GetStyleName: string;
  const
    cnstEntNamePartDelimeter = 'I';
  begin
    if AFontFace.SHXFont <> '' then
    begin
      Result := ExtractFileName(AFontFace.SHXFont);
      Result := Result + 'A' + DoubleToStr(AFontFace.ObliqueAngle, cnstEntNamePartDelimeter);
    end
    else
    begin
      Result := TTF.GetFontIdByNameAndStyle(AFontFace.FontName, AFontFace.FontStyle, cnstCharSetDef);
      Result := Result + 'H' + DoubleToStr(AFontFace.Height, cnstEntNamePartDelimeter);
      if fsItalic in AFontFace.FontStyle then
        Result := Result + 'IF';
    end;
  end;

  function IsEqualStyle(AStyle: TsgDXFStyle; var APrimaryFont: string): Boolean;
  begin
    if Assigned(AStyle) then
    begin
      if AFontFace.SHXFont <> '' then
      begin
        Result := (AStyle.PrimaryFont = AFontFace.SHXFont) and
          (AStyle.FontName = AFontFace.SHXFont) and
          (AStyle.ObliqueAngle = AFontFace.ObliqueAngle);
      end
      else
      begin
        APrimaryFont := TTF.GetFileNameByFontName(AFontFace.FontName, AFontFace.FontStyle);
        Result := (AStyle.FontName = AFontFace.FontName) and
          (AStyle.FontStyle = TmvFontStyles(AFontFace.FontStyle)) and
          (AStyle.FixedHeight = AFontFace.Height);
        if Result and (fsItalic in AFontFace.FontStyle) then
          Result := AStyle.ObliqueAngle = 15;
      end;
    end
    else
      Result := False;
  end;

begin
  if ACADImage = nil then
    ACADImage := FCADImage;
  if (ACADImage = FCADImage) and Assigned(FDefaultStyle) then
  begin
    Result := FDefaultStyle;
    Exit;
  end;
  vStyleId := GetStyleName;
  Result := TsgDXFStyle(ACADImage.Converter.Sections[csStyles].FindEntByName(vStyleId));
  if not IsEqualStyle(Result, vPrimaryFont) then
  begin
    if not Assigned(Result) then
    begin
      Result := CreateAndAddStyle(ACADImage, vStyleId);
      if AFontFace.SHXFont <> '' then
      begin
        Result.PrimaryFont := AFontFace.SHXFont;
        Result.FontName := AFontFace.SHXFont;
        Result.ObliqueAngle := AFontFace.ObliqueAngle;
      end
      else
      begin
        Result.PrimaryFont := vPrimaryFont;
        Result.FontName := AFontFace.FontName;
        Result.FontStyle := TmvFontStyles(AFontFace.FontStyle);
        Result.FixedHeight := AFontFace.Height;
        if fsItalic in AFontFace.FontStyle then
          Result.ObliqueAngle := 15;
      end;
      ACADImage.Converter.Loads(Result);
    end;
  end;
end;

function TsgConvertMetafileToCad.IsDrawContour(AList: TsgObjectList;
  AIsContour: Boolean): Boolean;
var
  I: Integer;
  vEqColors: Boolean;
  vContourColor: TsgColorCAD;
begin
  Result := True;
  vEqColors := True;
  for I := 0 to AList.Count - 1 do
  begin
    vContourColor := TsgDXFEntity(AList[I]).ColorCAD;
    if not IsEmptyColorCAD(vContourColor) then
      if (not IsEqualColorCAD(vContourColor, FData.ColorCAD)) or AIsContour then
      begin
        vEqColors := False;
        Break;
      end;
  end;
  if vEqColors then
    Result := False;
end;

function TsgConvertMetafileToCad.IsScanOfDocument(AImageEnt: TsgDXFImageEnt): Boolean;
begin
  Result := FMetaFileHeader.nRecords <= cnstMaxRecordsInScan;
end;

function TsgConvertMetafileToCad.IsSolidContour(ASolid: Boolean): Boolean;
begin
  if ASolid then
    Result := not IsEmptyColorCAD(FData.ColorCAD) and ((FParamsDraw.BkMode <> TRANSPARENT)
      or (FData.RGBColor <> FParamsDraw.BkColor))
  else
    Result := False;
end;

{ TsgMetafiles }

procedure TsgMetafiles.LoadFromStream(S: TStream);
var
  I: Integer;
  vEnt: TsgDXFEntity;
  vMetafile: TMetafile;
  vBox: TFRect;
  vEntities: tList;
  vIsImageInClip: Boolean;
begin
  inherited LoadFromStream(S);
  {$IFNDEF SG_OPENING_IN_THEADS}
  Loading := Self;
  {$ENDIF}
  //TsgDXFConverterAccess(Converter).SetLoading(True, nil);
  try
    CurrentLayout := Layouts[0];
    if S.Size = 0 then
    begin
      {$IFNDEF SG_OPENING_IN_THEADS}
      Loading := nil;
      {$ENDIF}
      GetExtents;
      Exit;
    end;

    vEntities := TList.Create;
    vMetafile := TMetafile.Create;
    try
      vMetafile.LoadFromStream(S);
      ConvertToCad(vMetafile, nil, Self, vBox, vEntities, '', nil, nil, vIsImageInClip);
      for I := 0 to vEntities.Count - 1  do
      begin
        vEnt:= vEntities.List[I];
        Converter.Loads(vEnt);
        CurrentLayout.AddEntity(vEnt);
      end;

      SetDefaultViewPort(Converter);
      GetExtents;
      SetDefaultPlotSettings;
    finally
      vMetafile.Free;
      vEntities.Free;
    end;
  finally
    TsgDXFConverterAccess(Converter).SetLoading(False, nil);
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := nil;
    {$ENDIF}
  end;
end;

initialization
//  TPicture.UnRegisterGraphicClass(TMetafile);
//  TPicture.RegisterFileFormat('emf', 'METAFILE', TsgMetafiles);
//  TPicture.RegisterFileFormat('wmf', 'METAFILE', TsgMetafiles);

finalization
//  TPicture.UnRegisterGraphicClass(TsgMetafiles);
//  TPicture.RegisterFileFormat('emf', 'METAFILE', TMetafile);
//  TPicture.RegisterFileFormat('wmf', 'METAFILE', TMetafile);

end.

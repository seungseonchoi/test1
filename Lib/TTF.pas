{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{    Parsing and drawing TTF texts by curves and polygons    }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit TTF;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$IFNDEF SG_NON_WIN_PLATFORM}
  {$DEFINE SG_WIN_PLATFORM}
{$ENDIF}

{$IFDEF SG_FM_WINDOWS}
  {$DEFINE SG_WIN_PLATFORM}
{$ENDIF}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF SGFPC}
  JwaWinGDI,
{$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, EasyLazFreeType,
  TTTypes, LazFreeType, AvgLvlTree, FileUtil,
{$ENDIF}
  Classes, SysUtils,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, FMX.Objects,
  FMX.FontGlyphs, System.IOUtils, Generics.Collections,
  {$IFDEF LINUX}
  FMX.FontGlyphs.Linux,
  {$ENDIF}
  {$IFDEF SG_FM_WINDOWS}
  FMX.Helpers.Win,
  {$ENDIF}
{$ELSE}
  Graphics, MVFont,
{$ENDIF}
{$IFDEF SG_WIN_PLATFORM}
  Windows, ActiveX, sgShellApi, Registry,
{$ENDIF}
  sgConsts, Math,
{$IFDEF SGDEL_XE2}
  System.Types, System.UITypes,
{$ELSE}
{$IFDEF SGDEL_6}
  Types,
{$ENDIF}
{$ENDIF}
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  AnsiStrings,
{$ENDIF}
{$IFDEF SGDEL_XE6}
  System.Math.Vectors,
{$ENDIF}
  sgFunction, sgLists, SyncObjs;

const
  cnstTrueType = ' (TrueType)';
  cnstCharSetDef = DEFAULT_CHARSET;
  cnstInvalidGlyphIndex = Word($FFFF);

type
{$IFDEF SGFPC}
  THandle = {$IFDEF LINUX}LCLType.THandle{$ELSE}System.THandle{$ENDIF};
{$ENDIF}
  TsgFontType = (ftUndefined, ftTTF, ftOTF, ftTTC, ftFON, ftSHX, ftDFT);
  TsgFontTypes = set of TsgFontType;

  PsgTTVector = ^TsgTTVector;
{$IFNDEF SG_NON_WIN_PLATFORM}
  TsgTTVector = TPointfx;
{$ELSE}
  TsgTTVector = TT_Vector;
{$ENDIF}
{$IFDEF SGFPC}
  PByte = System.PByte;
{$ENDIF}
  PsgTTVectors = ^TsgTTVectors;
  TsgTTVectors = array[0..100] of TsgTTVector;

  PsgTTFFloat = ^TsgTTFFloat;
  TsgTTFFloat = Single;

  PsgTTFPoint = PPointF;
  TsgTTFPoint = TPointF;

  PsgTTFRect = ^TsgTTFRect;
  TsgTTFRect = record
    Left, Top, Right, Bottom: TsgTTFFloat;
  end;

  TFontStylesList = array of TFontStyles;

  TsgContainerOfTextGlyphs = class;
  TsgGlyph = class;
  TsgTextGlyph = class;

  TsgGlyphQueryMode = (gqmOutline, gqmPath);

  TsgPointsArray = array of TPoint;
  TsgCountsArray = array of Integer;

  { TsgContainerOfTextGlyphs }

  TsgContainerOfTextGlyphs = class
  private
    FDefaults: array[0..3] of TsgTextGlyph;
    FLock: TMultiReadExclusiveWriteSynchronizer; //SyncObjs.TCriticalSection;
    FContainer: TsgCollection;
    FRefs: Integer;
    function GetDefaults(Style: TFontStyles): TsgTextGlyph;
    procedure CorrectFontParams(var AName: string; var AStyle: TFontStyles; var ACharset: TFontCharset);
    function DoAddTextGlyphsByFont(AFontName: string; AFontStyle: TFontStyles; ACharSet: TFontCharset): TsgTextGlyph;
  protected
    function IsDefault(ATextGlyph: TsgTextGlyph): Boolean;
    function GetCount: Integer;
    function GetTextGlyph(Index: Integer): TsgTextGlyph;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTextGlyphsByFont(const AFont: {$IFDEF SG_NON_WIN_PLATFORM}TFont{$ELSE}TmvExtFont{$ENDIF}): TsgTextGlyph; overload;
    function AddTextGlyphsByFont(AFontName: string; AFontStyle: TFontStyles;
      ACharSet: TFontCharset = cnstCharSetDef): TsgTextGlyph; overload;
    procedure Clear;
    function IndexOf(AFontName: string; AStyles: TFontStyles; ACharSet: TFontCharset): Integer; overload;
    function IndexOf(AFontName: string; AStyles: TmvFontStyles; ACharSet: TFontCharset): Integer; overload;
    procedure Reference;
    procedure Release;
    procedure Lock;
    procedure Unlock;
    function IsFontValid(const AFamilyName: string; AStyle: TFontStyles): Boolean;
    property Count: Integer read GetCount;
    property TextGlyph[Index: Integer]: TsgTextGlyph read GetTextGlyph;
    property Defaults[Style: TFontStyles]: TsgTextGlyph read GetDefaults;
  end;

  { TsgGlyph }

  TGlyphParamMethod = procedure(ATransformation: TObject; AParam: THandle) of object;

{$IFDEF SGFPC}
  TPolygon = array of TPointF;
{$ENDIF}
  TPolygons = array of TPolygon;

  TsgGlyph = class
  private
    FKey: Word;
    FAdvance: Single;
    FBlackBox: TsgTTFRect;
    FPolygons: TPolygons;
    function UpdateGlyphPoints(ATransformation: TObject; var ADrawGlyphPts: TsgPointsArray;
      var ADrawGlyphPtsCount: TsgCountsArray; var ADrawGlyphPolyCount: Integer): Integer;
//    procedure DrawBox(ATransformation: TObject);
    function GetBlackX: Single;
    function GetBlackY: Single;
    function GetRightSpace: Single;
    procedure DoDraw(ATransformation: TObject; AParam: THandle);
    procedure DoGetPolyPointsSingle(ATransformation: TObject; AParam: THandle);
    procedure DoGetPolyPointsCustom(ATransformation: TObject; AParam: THandle);
    procedure DoGetPolyPointsCustomEpsilon(ATransformation: TObject; AParam: THandle);
  protected
    function AddPolygon(const APolygon: TPolygon): Integer;
    procedure ClearPoints;
    property Polygons: TPolygons read FPolygons;
    property Key: Word read FKey;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(AContext: TsgProxyBase; const AXOffset: Double;
      const AMatrix: TFMatrix; const AFill: Boolean);
    property Advance: Single read FAdvance;
    property BlackBox: TsgTTFRect read FBlackBox;
    property BlackX: Single read GetBlackX;
    property BlackY: Single read GetBlackY;
    property LeftSpace: Single read FBlackBox.Left;
    property RightSpace: Single read GetRightSpace;
  end;

  { TsgTextGlyph }

  TsgTextGlyph = class
  private
    FAbove: Single;
    FBelow: Single;
    FFontItem: TObject;
    FGlyphsCount: Integer;
    FGlyphs: array of TsgGlyph;
    FHKoef: Double;
    FAveCharWidth: Single;
    FCharset: TFontCharset;
    FGlyphQueryMode: TsgGlyphQueryMode;
{$IFNDEF SG_NON_WIN_PLATFORM}
    FDefaultIndex: Word;
{$ENDIF}
{$IFDEF SG_OPENING_IN_THEADS}
    FOwnerLock: TMultiReadExclusiveWriteSynchronizer;//SyncObjs.TCriticalSection;
{$ENDIF}
{$IFDEF SG_WINAPI}
    FWinMesureContext: TObject;//TWinMesureContext
{$ENDIF}
    function GetGlyph(AKey: Word): TsgGlyph;
    function DoEnum(ATextItems: TStringBuilder; ATracking: Single;
      const ATransformation: TObject; const AStyleMode: Boolean;
      AGlyphParamMethodOffset: Pointer; AParam: THandle): TFPoint;
    procedure DoDrawLine(AProc: TsgPointsProc; ATransformation: TObject; const APoint1, APoint2: TFPoint);

    function GetFontName: string;
    function GetFontStyle: TFontStyles;
    function InternalGetGlyph(AKey: Integer): TsgGlyph;
    function InternalLoad(ATextItems: TStringBuilder): TsgGlyph;
  protected
    FFont: TFont;
    function GetTextBox(ATextItems: TStringBuilder; ATracking: Single): TFRect;
    function GetAbovePosition: Single;
    function GetStrikeoutPosition: Single;
    function GetUnderlinePosition: Single;
    function GetFileName: string;
    function AddGlyph(AKey: Word): TsgGlyph; overload;
    function AddGlyph(AKey: sgUnicodeChar): TsgGlyph; overload;
    function Find(AKey: Integer; var Index: Integer): Boolean;
    procedure LoadGlyph(AGlyph: TsgGlyph);

    function CreateFont(AHeightFactor: Double): TFont;
    procedure InsertGlyph(Index: Integer; AGlyph: TsgGlyph);

    procedure DoClear;
    property Glyph[AKey: Word]: TsgGlyph read GetGlyph;
  public
    constructor Create(const AFontName: string;
      const AFontStyle: TFontStyles;
      ACharSet: TFontCharset); virtual;
    destructor Destroy; override;
    procedure LockList;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure UnlockList;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure ClearPointsInGplyphs;
    procedure DrawText(AContext: TsgProxyBase; ATextItems: TStringBuilder;
      AUnderline, AStrikeOut: Boolean; const AMatrix: TFMatrix; const ATracking: Single;
      const ATextMode: Byte);
    procedure InitKoef;
    function GetBox(const AText: string; const AMatrix: TFMatrix;
      const APFontParams: PsgFontParams; const ATracking: Single): TFRect;
    function GetBoxW(const AText: string; const ATextW: WideString;
      const AMatrix: TFMatrix; const APFontParams: PsgFontParams; const ATracking: Single): TFRect;
    function GetGlyphParams(const AChar: Char; const AFontStyles: {$IFDEF SGFPC}Integer{$ELSE}Byte{$ENDIF};
      const AMatrix: TFMatrix; const AList: TList; APBox: PFRect; ATracking: Single): Boolean;

    function GetPolyPolyline(const AText: string; const ATextW: WideString;
      const AMatrix: TFMatrix; ACollection: TsgCustomPolyItemsCollection;
      const ATracking: Single = 1.0; AEpsilon: PFPoint = nil;
      const AUnderline: Boolean = False; const AStrikeOut: Boolean = False;
      const AStyleMode: Boolean = False): TFPoint; overload;

    function GetPolyPolyline(ATextItems: TStringBuilder;
      const AMatrix: TFMatrix; ACollection: TsgCustomPolyItemsCollection;
      const ATracking: Single = 1.0; AEpsilon: PFPoint = nil;
      const AUnderline: Boolean = False; const AStrikeOut: Boolean = False;
      const AStyleMode: Boolean = False): TFPoint; overload;

    function LoadedW(const AText: string; const ATextW: WideString = ''): TsgGlyph;
    function GetFontParams(const AMatrix: TFMatrix): TsgFontParams;
    function GetIndices(const AText: string; const AWideText: WideString; out Indices{: array of Word}): Integer;
    function GetIndex(AChar: WideChar): Word;
    //function GetText(const Indices: array of Word): WideString;
    property FontName: string read GetFontName;
    property FontStyle: TFontStyles read GetFontStyle;
    property CharSet: TFontCharset read FCharset;
    property HKoef: Double read FHKoef;
    property AveCharWidth: Single read FAveCharWidth;
    property GlyphQueryMode: TsgGlyphQueryMode read FGlyphQueryMode write FGlyphQueryMode default gqmOutline;
    function IsDefault: Boolean;
  end;

procedure InitializeCollections;
procedure FindGDIFonts(const AFonts: TStrings);
function GetFileNameByFontName(const AFontName: string; AStyle: TFontStyles): string;
function GetFontIdByNameAndStyle(const AFontName: string;
  const AStyle: TFontStyles; ACharset: TFontCharset = cnstCharSetDef): string;
function GetFontNameByFileName(const AFileName: string; var AStyle: TFontStyles): string;
function GetFontNameWithoutStyle(const AName: string): string;
function GetFontStyleByFontName(const AFontName: string): TmvFontStyles;
function GetFontTypeByFileNameExt(const AFileName: string): TsgFontType;
function GetFontTypeByFontName(const AFontName: string): TsgFontType;
function GetFontTypeEx(const AFontName: string): TsgFontTypes;
function GetNameOfFont(const AFont: TFont): string;
function GetFPointFromPointFx(const AFixedPoint: PsgTTVector): TFPoint;
function GetQualityApproximation: Byte;
function IsNotTTFFont(const AFontName: string): Boolean;
function IsSystemNotRasterFont(const AFontName: string): Integer;
function IsSystemNotRasterFontEx(const AFontName: string): Integer;
function FindForPDFName(const AFontName: string): string;
function MakeFPointFromTTF(const APoint: TPointF): TFPoint;{$IFDEF USE_INLINE} inline;{$ENDIF}
procedure SetQualityApproximation(const AValue: Byte);
function SwapBufferOfBytes(APBuffer: Pointer; ACount: Integer): Pointer;
function SymbolExistFont(const AFont: TFont; const AKey: WORD): Boolean;
function SymbolExistDC(const ADC: HDC; const AKey: WORD): Boolean;
{$IFDEF SG_NON_WIN_PLATFORM}
{$IFDEF SGFPC}
function SymbolExist(const AFamilyName: string; AStyle: TFontStylesbase;
  const AKey: Integer): Boolean;
{$ENDIF}
{$ELSE}
function SymbolExistDCA(const ADC: HDC; const AKey: WORD; const AMat: TMat2): Boolean;
function SymbolExistDCW(const ADC: HDC; const AKey: WORD; const AMat: TMat2): Boolean;
function SymbolExistFontName(const AFontName: string; AStyle: TFontStyles; ACharset: TFontCharset; const AKey: WORD): Boolean;
{$ENDIF}
function QueryFamilyStyles(const AFamilyName: string; var AStyles: TFontStylesList): Boolean;
function SplitFullName(var AName: string; var AStyle: TFontStyles): Boolean;

function IsFaceSupportsCharset(const AFaceName: string; ACharset: TFontCharset): Boolean;
function UpdateTextGlyph(const AFontName: string; AStyles: TFontStyles; ACharset: TFontCharset; APitch: Byte): TsgTextGlyph;
function GetFontPitch(const AFontName: string): Byte;

function CreateTextItems(const AText: string; const ATextW: WideString): TStringBuilder;

var
  ContainerOfTextGlyphs: TsgContainerOfTextGlyphs = nil;

implementation

{$DEFINE NEED_CVT_MVFONTSTYLE}
{$IFDEF SGFPC}
  {$IFDEF FPC_OBJFPC}
  {$UNDEF NEED_CVT_MVFONTSTYLE}
  {$ENDIF}
{$ELSE}
  {$UNDEF NEED_CVT_MVFONTSTYLE}
{$ENDIF}

{$IFDEF SG_DLL}
{$DEFINE SG_LIBRARY_OR_APPLICATION}
{$ENDIF}
{$IFDEF SGABVIEWER}
{$DEFINE SG_LIBRARY_OR_APPLICATION}
{$ENDIF}
{$IFDEF SG_CAD_NAVIGATOR}
{$DEFINE SG_LIBRARY_OR_APPLICATION}
{$ENDIF}

{$IFDEF SG_FM_MACOS}
  {$DEFINE _TTF_USE_NEG_FONT_HEIGHT}
{$ENDIF}
{$IFDEF SG_FM_LINUX}
  {$DEFINE _TTF_USE_NEG_FONT_HEIGHT}
{$ENDIF}

uses sgEncoding, sgEncMgr
{$IFDEF SGFPC}
  , LazFreeTypeFontCollection
{$ENDIF}
  ;

const
  cnstHeigthDef = {$IFNDEF SG_FIREMONKEY}{$IFDEF SGFPC}256{$ELSE}5472{$ENDIF}{$ELSE}1280{$ENDIF};
  cnstBold = ' Bold';
  cnstTTFBadRect: TsgTTFRect = (Left: 1E20; Top: -1E20; Right: -1E20;
    Bottom: 1E20);
  cnstCurveRecursionLimit = 16;
  cnstDefaultQualityApproximation = 4;
  cnstGlyphsCount = 256;
  cnstAnsiGlyphMax = 256;
  constCSIDL_FONTS = $0014;
  cnstHiChar = Ord('Q');
  cnstItalic = ' Italic';
  cnstOblique = ' Oblique';
  cnstLangIDOfEnglish = 1033;
  cnstLeftBracket = ' (';
  cnstLowChar = Ord('y');
  cnstName = 'name';
  cnstTTFPointZero: TPointF =();
  cnstTTFRectZero: TsgTTFRect = ();
  cnstRegKeyCurrentVersion = 'CurrentVersion';
  cnstRegKeyFonts = 'Fonts';
{$IFDEF SG_WIN_PLATFORM}
  cnstRegKeyHKLM = HKEY_LOCAL_MACHINE;
{$ENDIF}
  cnstRegKeyMicrosoft = 'Microsoft';
  cnstRegKeyMicrosoft9X = 'Windows';
  cnstRegKeyMicrosoftNT = 'Windows NT';
  cnstRegKeySoftware = 'SOFTWARE';
  cnstSpace = ' ';
{$IFDEF SG_WINAPI}
  cnstSizeOfOutlineTextmetric = SizeOf(TOutlineTextmetric);
  cnstGlyphOutlineMatrix: TMat2 = (
    eM11: (fract: 0; value: 1);
    eM12: (fract: 0; value: 0);
    eM21: (fract: 0; value: 0);
    eM22: (fract: 0; value: 1));
{$ENDIF}
  cnstDelimiter = '=';

  cnstFonts = 'Fonts';
  cnstFaceName = 'FaceName';
  cnstFullName = 'FullName';
  cnstStyle = 'Style';
  cnstFontType = 'FontType';
  cnstFontFlags = 'FontFlags';
  cnstFontCharSet = 'CharSet';
  cnstScript = 'Script';

  cnstNameBold = 'Bold';
  cnstNameItalic = 'Italic';
  cnstNameOblique = 'Oblique';

type
  TsgBaseListAccess = class(TsgBaseList);
{$IFNDEF SGFPC}
  ArrayOfString = array of string;
{$ENDIF}

  { TsgListAccess }

  TsgListAccess = class(TsgList)
  private
    function GetList: PPointerArray;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    property List: PPointerArray read GetList;
  end;

  PsgNameTableHeader = ^TsgNameTableHeader;
  TsgNameTableHeader = packed record
    Selector, NRCount, StorageOffset: Word;
  end;

  PsgNameRecord = ^TsgNameRecord;
  TsgNameRecord = packed record
    PlatformID, EncodingID, LanguageID: Word;
    NameID, StringLength, StringOffset: Word;
  end;

  PsgOffsetTable = ^TsgOffsetTable;
  TsgOffsetTable = packed record
    MajorVersion, MinorVersion, NumOfTables: Word;
    SearchRange, EntrySelector, RangeShift: Word;
  end;

  PsgTableDirectory = ^TsgTableDirectory;
  TsgTableDirectory = packed record
    Tag: array[0..3] of AnsiChar;//Char;
    CheckSum, Offset, Length: DWord;
  end;

  PsgTTCHeader = ^TsgTTCHeader;
  TsgTTCHeader = packed record
    Tag: array[0 .. 3] of AnsiChar; //'ttfc'
    MajorVersion, MinorVersion: Word;//version: TFixed;
    FontsCount: Cardinal;
  end;

  TTTFontItemData = record
    Name: string;
    EMSquare: Word;
    Pitch: Byte;
    Style: TFontStyles;
  end;

  TTTFontItemDataList = array of TTTFontItemData;

  { GetFontResourceInfoW

    undocumented feature

    Parameters:
    AFilename - font file name
    ABufferSize - size of buffer for resouce information
    ABuffer - buffer for returned resouce information
    AQueryType - resouce information query type:
      - QFR_0 = 0 	         DWORD or LPVOID unknown
      - QFR_DESCRIPTION = 1  The function provides a string that an NT-based
                             operating system will use to describe the font file.
                             A null-terminated Unicode string is written to
                             the buffer pointed to by lpBuffer.
      - QFR_LOGFONT = 2 	   Array of LOGFONTW
      - QFR_PDEV = 3         PDEV ?
      - QFR_FONTFILE = 4     scalable font file name
      - QFR_5 - 5 	         DWORD (Windows XP: Always returns 0);
  }
  TGetFontResourceInfo = function(AFilename: PWideChar; var ABufferSize: DWORD;
    ABuffer: PWideChar; AQueryType: DWORD): Cardinal; stdcall;

  TGlyphIndices = array of Word;

  TLogFontWList = array of TLogFontW;

{$IFDEF SGFPC}
  TFreeTypeFontAccess = class(TFreeTypeFont);
{$ENDIF}

  TCustomTransformation = class;

{$IFNDEF SG_FIREMONKEY}
  TsgGlyphOutlineData = {$IFNDEF SG_NON_WIN_PLATFORM}Pointer{$ELSE}TT_Outline{$ENDIF};
{$ELSE}
  TsgGlyphOutlineData = TPolygon;
{$ENDIF}

  TsgGlyphBuilder = class
  private
    FCount: Integer;
    FPolylines: array of TsgPointFList;
    function CurveTo(const APointCurve1, APointCurve2: TPointF): TPointF;
    function CurveToEx(ALast: TsgPointFList; const APointCurveStart, APointCurve1, APointCurve2: TPointF): TPointF;
{$IFDEF SG_WINAPI}
    function GetPointFromPointFX(const AVector: TPointFX): TPointF;
{$ENDIF}
    function AddPolygon(APolygon: TsgPointFList): Integer;
    function Last: TsgPointFList;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure Pack;
    procedure DoExtract(var APolygons: TPolygons);
  protected
    procedure AddDecomposeCubicSplineInList(AList: TsgPointFList;
      const APoints: array of TPointF);
{$IFDEF SG_WINAPI}
    procedure AddDecomposeLineInList(AList: TsgPointFList;
      const APoints: array of TPointfx);
    procedure AddDecomposeQSplineInList(AList: TsgPointFList;
      const APoints: array of TPointfx);
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
    procedure Decompose(const APathData: TPathData); overload;
{$ENDIF}
    procedure Decompose(const ABuffer: TsgGlyphOutlineData; ASize: Integer);{$IFDEF SG_FIREMONKEY} overload;{$ENDIF}
    procedure DecomposePath(const APoints; const ATypes; ACount: Integer);
    function GetPoint(const AVector: TsgTTVector): TPointF;
    procedure MoveTo(const APointMove: TPointF);
{$IFDEF SG_WINAPI}
    procedure DecomposeOutline(ABuffer: Pointer; ASize: Integer);
{$ENDIF}
  public
    function Extract(ATransformation: TCustomTransformation;
      var ABox: TsgTTFRect; var APolygons: TPolygons): Boolean;
    procedure Transform(ATransform: TCustomTransformation);
  end;

  TCustomTransformation = class
  protected
    function GetMartix: TFMatrix; virtual;
    procedure SetMatrix(const Value: TFMatrix); virtual;
    function GetOffset: TFPoint; virtual;
    procedure SetOffset(const Value: TFPoint); virtual;
    property Matrix: TFMatrix read GetMartix write SetMatrix;
  public
    constructor Create(const AMatrix: TFMatrix); virtual;
    function AffineTransform(const APoint: TFPoint): TFPoint; virtual;
    procedure Scale(const AScale: TFPoint); virtual;
    procedure TransformPolygon(var APolygon: array of TPointF);
    procedure TransformPolygons(var APolygons: array of TPolygon);
    function Transform(const APoint: TFPoint): TFPoint; overload; virtual;
    function Transform(const X, Y: Double): TFPoint; overload; virtual;
    function Transform(const APoint: TPointF): TFPoint; overload; virtual;
    function Transform2D(const APoint: TPointF): TPointF; overload; virtual;
    function Transform2D(const X, Y: Double): TPoint; overload; virtual;
    function TransformBox(const ABox: TFRect): TFRect; virtual;
    procedure Translate(const ADelta: TFPoint); virtual;
    property Offset: TFPoint read GetOffset write SetOffset;
  end;

  TOffsetTransformation = class(TCustomTransformation)
  private
    FOffset: TFPoint;
  protected
    function GetMartix: TFMatrix; override;
    procedure SetMatrix(const Value: TFMatrix); override;
    function GetOffset: TFPoint; override;
    procedure SetOffset(const Value: TFPoint); override;
  public
    constructor Create(const AMatrix: TFMatrix); override;
    function Transform(const APoint: TFPoint): TFPoint; override;
    function Transform(const X, Y: Double): TFPoint; override;
    function Transform(const APoint: TPointF): TFPoint; override;
    function Transform2D(const APoint: TPointF): TPointF; override;
    function Transform2D(const X, Y: Double): TPoint; override;
    function TransformBox(const ABox: TFRect): TFRect; override;
    procedure Translate(const ADelta: TFPoint); override;
  end;

  TTransformation = class(TCustomTransformation)
  private
    FMatrix: TFMatrix;
  protected
    function GetMartix: TFMatrix; override;
    procedure SetMatrix(const Value: TFMatrix); override;
    function GetOffset: TFPoint; override;
    procedure SetOffset(const Value: TFPoint); override;
  public
    constructor Create(const AMatrix: TFMatrix); override;
    function AffineTransform(const APoint: TFPoint): TFPoint; override;
    procedure Scale(const AScale: TFPoint); override;
    function Transform(const APoint: TFPoint): TFPoint; override;
    function Transform(const X, Y: Double): TFPoint; override;
    function Transform(const APoint: TPointF): TFPoint; override;
    function Transform2D(const APoint: TPointF): TPointF; override;
    function Transform2D(const X, Y: Double): TPoint; override;
    function TransformBox(const ABox: TFRect): TFRect; override;
    procedure Translate(const ADelta: TFPoint); override;
  end;

  PsgGlyphDrawParam = ^TsgGlyphDrawParam;
  TsgGlyphDrawParam = record
    Context: TsgProxyBase;
    TextMode: Integer; //$01 - polyline; $02 - polygon;
    Epsilon: PFPoint;
    Collection: TObject;
  end;

  TsgFontItem = class;

  TsgFamilyItem = class
  private
    FFonts: TFontStylesList;
    FFamilyName: string;
    FPitch: Byte;
    FCharset: TFontCharset;
    function GetFont(Style: TFontStyles): TsgFontItem;
  protected
    function BuildKey(AStyle: TFontStyles): string;
    procedure FillLogFont(var ALogFont: TLogFont);
    function CreateFont(AStyle: TFontStyles): TFont; virtual;
    function AddStyle(AStyle: TFontStyles): Integer;
    function RemoveStyle(AStyle: TFontStyles): Integer;
  public
    constructor Create; overload; virtual;
    constructor Create(const AFamilyName: string; APitch: Byte); overload; virtual;
    destructor Destroy; override;
    function Find(AStyle: TFontStyles; var Index: Integer): Boolean;
    property Fonts: TFontStylesList read FFonts;
    property Font[Style: TFontStyles]: TsgFontItem read GetFont;
    property FamilyName: string read FFamilyName;
    property Pitch: Byte read FPitch;
    property Charset: TFontCharset read FCharset default DEFAULT_CHARSET;
  end;

  { TsgFontItem }

  TsgFontItem = class
  private
    FHash: THandle;
    FFileName: string;
    FStyle: TFontStyles;
    FFlags: Integer;
    FEMSquare: Word;
    function GetName: string;
{$IFDEF SGFPC}
    procedure FreeTypeFontDestroing;
    procedure SetFontCollectionItem(AValue: TCustomFontCollectionItem);
{$ENDIF}
  protected
{$IFDEF SGFPC}
    FFontCollectionItem: TCustomFontCollectionItem;
    FFreeTypeFont: TFreeTypeFont;
{$ENDIF}
    FFamily: TsgFamilyItem;
    function CreateFont: TFont; virtual;
    procedure FillLogFont(var ALogFont: TLogFont);
    function GetFileName: string; virtual;
    property Flags: Integer read FFlags write FFlags;
  public
    constructor Create; overload; virtual;
    constructor Create(const AFamily: TsgFamilyItem;
      ABold, AItalic: Boolean; AEMSquare: Word); overload; virtual;
    destructor Destroy; override;
    procedure Init(const AFamily: TsgFamilyItem; ABold, AItalic: Boolean; AEMSquare: Word);
    function BuildKey: string;
    property Family: TsgFamilyItem read FFamily write FFamily;
    property Name: string read GetName;
    property Style: TFontStyles read FStyle;
    property FileName: string read GetFileName;
    property EMSquare: Word read FEMSquare;
{$IFDEF SGFPC}
    property FontCollectionItem: TCustomFontCollectionItem read FFontCollectionItem write SetFontCollectionItem;
{$ENDIF}
  end;

  TFamiliesList = class(TStringList)
  end;

  TsgTableFonts = class
  private
    FFontFiles: TsgObjectList;
    FFontItems: TsgObjectList;
    FFontAliases: TsgObjectList;
    FFamilies: TFamiliesList;
    FCharsets: array[Byte] of TStringList;
    FFontItemCache: TObject;
    function AddFontFilesInfoEx(AFileName, APath: string): Boolean;
{$IFDEF SG_WIN_PLATFORM}
    function OpenRegistryKeyFonts(const ARegistry: TRegistry; ACurrentUser: Boolean): Boolean;
{$ENDIF}
    function CompareFileName(const A, B: Pointer): Integer;
    function CompareHash(const A, B: Pointer): Integer;
    function GetFileNameProp(const AFontName: string;
      AStyle: TFontStyles): string;
    function UpdateFontDataItems(const AName: string; ABold, AItalic: Boolean;
      APitch: Byte; AEMSquare: Word): TsgFontItem;
    function Find(AHash: THandle; var Index: Integer): Boolean; overload;
    function Find(const AKey: string; var Index: Integer): Boolean; overload;
    function GetFamily(AName: string): TsgFamilyItem;
  protected
    procedure Initialize;
    procedure InitializeFromRegister;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFileName(const AFontName: string): string;
    function GetFontName(const AFileName: string): string;
    function GetFontType(const AFontName: string): TsgFontType;
    function FindForPDFName(const AFontName: string): string;
    property FileName[const AFontName: string; AStyle: TFontStyles]: string read GetFileNameProp;
    property Family[AName: string]: TsgFamilyItem read GetFamily;
    function GetFont(const AFileName: string; var AFontName: string; var AStyle: TFontStyles): Boolean;
  end;

  IFamilyCharset = interface(IUnknown)
    ['{85172A33-FC0E-4F2B-BEC6-8E5D8EB5442A}']
    function GetCharset: TFontCharset;
  end;
{$IFDEF SG_DELPHI_VCL}
  TFamilyCharsetAdapter = class(TInterfacedObject, IChangeNotifier, IFamilyCharset)
  private
    FFont: TFont;
    FFamily: TsgFamilyItem;
    FCharset: TFontCharset;
    procedure SetFamily(const Value: TsgFamilyItem);
  protected
    { IChangeNotifier }
    procedure Changed;
    { IFamilyCharset }
    function GetCharset: TFontCharset;
    property Family: TsgFamilyItem read FFamily write SetFamily;
  public
    constructor Create(const AFont: TFont);
    property Font: TFont read FFont;
  end;
{$ENDIF}

{$IFDEF SG_WINAPI}
  // win: fpc/delphi vcl/fmx
  TWinMesureContext = class
  private
    FFont: TFont;
    FDC: HDC;
    FOTM: POutlineTextmetric;
    FFontChanged: TNotifyEvent;
    FIsRaster: Boolean;
    function GetAbove: Integer;
    function GetAveCharWidth: Integer;
    function GetBelow: Integer;
    function GetEMSquare: Word;
    function GetHandle: HDC;
  protected
    procedure OutlineTextmetricNeeded;
    procedure DestroyHandle;
    procedure HandleNeeded;
    procedure ForgetOTM;
    procedure FontChanged(Sender: TObject);
    function GetGlyphIndex(AChar: Cardinal; var Index: Word): Cardinal;
    property IsRaster: Boolean read FIsRaster;
    property OTM: POutlineTextmetric read FOTM;
  public
    constructor Create(const AFont: TFont);
    destructor Destroy; override;

    function DoGetIndices(const ATextItems: TStringBuilder; out AIndices: TGlyphIndices): Integer;
    function GetDefaultCharsetForAnsi: TFontCharset;
    function GetGlyphOutline(AChar: Integer; out ABytes{: TBytes}; AOrigin: PPoint = nil): TPoint;
    function GetGlyphPath(AChar: Integer; out ABytes{: TBytes}): Integer;

    property Font: TFont read FFont;
    property Handle: HDC read GetHandle;
    property Above: Integer read GetAbove;
    property Below: Integer read GetBelow;
    property AveCharWidth: Integer read GetAveCharWidth;
    property EMSquare: Word read GetEMSquare;
  end;
{$ENDIF}

var
  _Initialized: Boolean = False;
  {$IFDEF SG_WIN_PLATFORM}
  FontsPaths: array[0 .. 0] of string = ('');
  {$ELSE}
  {$IFDEF SG_FM_MACOS}
  FontsPaths: array[0 .. 1] of string = ('/System/Library/Fonts/', '/Library/Fonts/');
  {$ELSE}
  {$IFDEF SG_FM_ANDROID}
  FontsPaths: array[0 .. 0] of string = ('/system/fonts/');
  {$ELSE}
  FontsPaths: array[0 .. 0] of string = ('/usr/share/fonts/');
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  RegKeyMicrosoftWindows: string = cnstRegKeyMicrosoftNT;
  TableFonts: TsgTableFonts = nil;
  Generator: TsgGeneratorShapeEdge = nil;
  GetFontResourceInfo: TGetFontResourceInfo = nil; // undocumented feature


{$IFDEF SG_FIREMONKEY}

{$IFDEF SGDEL_10_SYDNEY}
type
  TFontGlyphManagerHelper = class helper{$IFDEF SG_FM_LINUX} (TLinuxFontGlyphManagerHelper){$ENDIF} for TFontGlyphManager
  public
    function GetGlyph(const AChar: UCS4Char; const Font: TFont; const Scale: Single;
      const Settings: TFontGlyphSettings): TFontGlyph; overload;
  end;

function TFontGlyphManagerHelper.GetGlyph(const AChar: UCS4Char; const Font: TFont; const Scale: Single;
  const Settings: TFontGlyphSettings): TFontGlyph;
var
  UCS4Chars: UCS4String;
begin
  SetLength(UCS4Chars, 2);
  UCS4Chars[0] := AChar;
  UCS4Chars[1] := 0;
  Result := GetGlyph(UCS4Chars, Font, Scale, Settings);
end;
{$ENDIF}

{$ENDIF}

{$IFNDEF SGDEL_2009}
//Rewrote using UnicodeFromLocalChars which is a wrapper for MultiByteToWideChar on Windows and emulates it on non windows
function HashNameMBCS(Name: PAnsiChar): Cardinal;
var
  Len, NameLen: Cardinal;
  Data: WideString;
  I: Integer;
begin
  NameLen := Length(Name);
  Len := UnicodeFromLocaleChars(CP_UTF8, 0, Name, NameLen, nil, 0);
  SetLength(Data, Len);
  UnicodeFromLocaleChars(CP_UTF8, 0, Name, NameLen, PWideChar(Data), Len);
  Data := WideUpperCase(Data);
  Result := 0;
  for I := 0 to Len - 1 do
  begin
    Result := (Result shl 5) or (Result shr 27); //ROL Result, 5
    Result := Result xor Cardinal(Data[I+1]);
  end;
end;

function HashName(Name: PAnsiChar): Cardinal;{$IFDEF USE_INLINE} inline;{$ENDIF}
var
  LCurr: PAnsiChar;
begin
  { ESI -> Name }
  Result := 0;
  LCurr := Name;

  while LCurr^ <> #0 do
  begin
    { Abort on a MBCS character }
    if Ord(LCurr^) > 127 then
    begin
      Result := HashNameMBCS(LCurr);
      Exit;
    end;

    { Update the hash. Lowercase the uppercased charaters in the process }
    if LCurr^ in ['A' .. 'Z'] then
      Result := Result xor (Ord(LCurr^) or $20)
    else
      Result := Result xor Ord(LCurr^);

    { Go to next }
    Inc(LCurr);

    { Update the hashed value }
    Result := (Result shr 27) or (Result shl 5);
  end;
end;
{$ENDIF}
function _HashName(const S: string): THandle;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_LINUX_FPC}
  Result := HashName(PAnsiChar(S));
{$ELSE}
  Result := HashName(PAnsiChar({$IFDEF SGDEL_6}UTF8Encode{$ENDIF}(S)));
{$ENDIF}
end;

function SwapBufferOfBytes(APBuffer: Pointer; ACount: Integer): Pointer;
begin
  Result := Pointer(TsgNativeInt(APBuffer) + ACount);
  while TsgNativeInt(APBuffer) < TsgNativeInt(Result) do
  begin
    PWord(APBuffer)^ := Swap(PWord(APBuffer)^);
    Inc(PWord(APBuffer));
  end;
end;

function SwapBufferOfWords(APBuffer: Pointer; ACount: Integer): Pointer;
begin
  Result := Pointer(TsgNativeInt(APBuffer) + ACount);
  while TsgNativeInt(APBuffer) < TsgNativeInt(Result) do
  begin
    PCardinal(APBuffer)^ := SwapWords(PCardinal(APBuffer)^);
    Inc(PCardinal(APBuffer));
  end;
end;

{$IFDEF SG_WINAPI}
{ TWinMesureContext }

constructor TWinMesureContext.Create(const AFont: TFont);
begin
  FFont := AFont;
  if Assigned(FFont) then
  begin
    FFontChanged := FFont.{$IFDEF SG_FIREMONKEY}OnChanged{$ELSE}OnChange{$ENDIF};
    FFont.{$IFDEF SG_FIREMONKEY}OnChanged{$ELSE}OnChange{$ENDIF} := FontChanged;
  end;
end;

destructor TWinMesureContext.Destroy;
begin
  DestroyHandle;
  ForgetOTM;
  if Assigned(FFont) then
    FFont.{$IFDEF SG_FIREMONKEY}OnChanged{$ELSE}OnChange{$ENDIF} := FFontChanged;
  inherited Destroy;
end;

procedure TWinMesureContext.ForgetOTM;
begin
  if FOTM <> nil then
  begin
    FreeMem(FOTM);
    FOTM := nil;
  end;
end;

procedure TWinMesureContext.FontChanged(Sender: TObject);
begin
  if Assigned(FFontChanged) then
    FFontChanged(Self);
  DestroyHandle;
  ForgetOTM;
end;

function TWinMesureContext.GetGlyphIndex(AChar: Cardinal; var Index: Word): Cardinal;
//var
//  Charset, DefaultFamilyCharset: TFontCharset;
begin
  if AChar < cnstAnsiGlyphMax then
  begin
//    Charset := Font.Charset;
    try
//      DefaultFamilyCharset := GetDefaultCharsetForAnsi;
//      Font.Charset := DefaultFamilyCharset;
      if FFont.Charset{DefaultFamilyCharset} = SYMBOL_CHARSET then
        Result := GetGlyphIndicesA(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS)
      else
        Result := GetGlyphIndicesW(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS);
    finally
//      Font.Charset := Charset;
    end;
  end
  else
  begin
    Result := GetGlyphIndicesW(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS);
//    if Index = cnstInvalidGlyphIndex then
//    begin
//      DefaultFamilyCharset := sgEncMgr.EncMgr.FromChar(AChar).Charset;
//      if DefaultFamilyCharset <> 0 then
//      begin
//        Charset := Font.Charset;
//        try
//          Font.Charset := DefaultFamilyCharset;
//          Result := GetGlyphIndicesW(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS);
//        finally
//          Font.Charset := Charset;
//        end;
//      end;
//    end;
  end;
end;

function TWinMesureContext.GetAbove: Integer;
begin
  OutlineTextmetricNeeded;
  if FIsRaster then
    Result := FOTM^.otmTextMetrics.tmAscent
  else
    Result := FOTM^.otmAscent;
end;

function TWinMesureContext.GetAveCharWidth: Integer;
begin
  OutlineTextmetricNeeded;
  Result := FOTM^.otmTextMetrics.tmAveCharWidth;
end;

function TWinMesureContext.GetBelow: Integer;
begin
  OutlineTextmetricNeeded;
  if FIsRaster then
    Result := FOTM^.otmTextMetrics.tmDescent
  else
    Result := FOTM^.otmDescent;
end;

function TWinMesureContext.GetEMSquare: Word;
begin
  OutlineTextmetricNeeded;
  Result := FOTM^.otmEMSquare;
end;

function TWinMesureContext.GetHandle: HDC;
begin
  HandleNeeded;
  Result := FDC;
end;

procedure TWinMesureContext.DestroyHandle;
begin
  if FDC <> 0 then
  begin
    DeleteObject(SelectObject(FDC, GetStockObject(SYSTEM_FONT)));
    DeleteDC(FDC);
    FDC := 0;
  end;
end;

procedure TWinMesureContext.HandleNeeded;
{$IFNDEF SG_FM_WINDOWS}
const
  cnstWeight: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
{$ENDIF}
begin
  if FDC <> 0 then Exit;
  FDC := CreateCompatibleDC(0);
  SetBkMode(FDC, OPAQUE);
  SetTextAlign(FDC, TA_BASELINE);
{$IFDEF SG_FM_WINDOWS}
  SelectObject(FDC, Windows.CreateFont(Round(FFont.Height), 0, 0, 0,
    FontWeightToWinapi(FFont.StyleExt.Weight),
    Cardinal(not FFont.StyleExt.Slant.IsRegular), 0, 0, FFont.Charset,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
    DEFAULT_PITCH or FF_DONTCARE, PChar(FFont.Family)));
{$ELSE}
  SelectObject(FDC, Windows.CreateFont(FFont.Height, 0, 0, 0,
    cnstWeight[fsBold in FFont.Style],
    Cardinal(fsItalic in FFont.Style), 0, 0, FFont.Charset,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
    DEFAULT_PITCH or FF_DONTCARE, PChar(FFont.Name)));
{$ENDIF}
end;

procedure TWinMesureContext.OutlineTextmetricNeeded;
begin
  HandleNeeded;
  if FOTM <> nil then Exit;
  GetMem(FOTM, SizeOf(TOutlineTextmetric));
  FIsRaster := GetOutlineTextMetrics(FDC, SizeOf(TOutlineTextmetric), FOTM) = 0;
end;

function TWinMesureContext.DoGetIndices(const ATextItems: TStringBuilder; out AIndices: TGlyphIndices): Integer;
var
  I: Integer;
begin
  Result := ATextItems.Length;
  SetLength(AIndices, Result);
  for I := 0 to ATextItems.Length - 1 do
    GetGlyphIndex(Ord(ATextItems[I]), AIndices[I]);
end;

// need to make as single property by field
function TWinMesureContext.GetDefaultCharsetForAnsi: TFontCharset;
{$IFDEF SG_DELPHI_VCL}
var
  f: IFamilyCharset;
{$ENDIF}
begin
{$IFDEF SG_DELPHI_VCL}
  if Font.FontAdapter = nil then Font.FontAdapter := TFamilyCharsetAdapter.Create(Font);
  if Font.FontAdapter.QueryInterface(IFamilyCharset, f) = S_OK then
    Result := f.GetCharset
  else
    Result := ANSI_CHARSET;
{$ELSE}
   Result := TableFonts.Family[Font.Name].Charset;
{$ENDIF}
end;

function TWinMesureContext.GetGlyphOutline(AChar: Integer; out ABytes{: TBytes};
  AOrigin: PPoint = nil): TPoint;
type
  TGetGlyphOutline = function (DC: HDC; uChar, uFormat: UINT;
    {$IFDEF SGFPC}var{$ELSE}const{$ENDIF} lpgm: TGlyphMetrics;
    cbBuffer: DWORD; lpvBuffer: Pointer; const lpmat2: TMat2): DWORD; stdcall;
var
  vSize: Integer;
  vGlyphMetrics: TGlyphMetrics;
//  vCharset, DefaultFamilyCharset: TFontCharset;
  GGlyphOutline: TGetGlyphOutline;
  vFormat: Cardinal;
//  Index: Word;
begin
  vFormat := GGO_NATIVE;
//  vCharset := Font.Charset;
  if AChar >= cnstAnsiGlyphMax then
  begin
    GGlyphOutline := @GetGlyphOutlineW;
//    GetGlyphIndicesW(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS);
//    if Index = cnstInvalidGlyphIndex then
//    begin
//      DefaultFamilyCharset := sgEncMgr.EncMgr.FromChar(AChar).Charset;
//      if DefaultFamilyCharset <> 0 then
//      begin
//        Font.Charset := DefaultFamilyCharset;
//        GetGlyphIndicesW(Handle, @AChar, 1, @Index, GGI_MARK_NONEXISTING_GLYPHS);
//        if Index <> cnstInvalidGlyphIndex then
//        begin
//          AChar := Index;
//          vFormat := vFormat or GGO_GLYPH_INDEX;
//        end;
//      end;
//    end
//    else
//    begin
//      AChar := Index;
//      vFormat := vFormat or GGO_GLYPH_INDEX;
//    end;
  end
  else
  begin
//    DefaultFamilyCharset := GetDefaultCharsetForAnsi;
//    Font.Charset := DefaultFamilyCharset;
    if FFont.Charset{DefaultFamilyCharset} = SYMBOL_CHARSET then
      GGlyphOutline := @GetGlyphOutlineA
    else
      GGlyphOutline := @GetGlyphOutlineW
  end;
  vSize := GGlyphOutline(Handle, AChar, vFormat, vGlyphMetrics, 0, nil, cnstGlyphOutlineMatrix);
  Result.X := vGlyphMetrics.gmCellIncX;
  Result.Y := vGlyphMetrics.gmCellIncY;
  if AOrigin <> nil then
    AOrigin^ := vGlyphMetrics.gmptGlyphOrigin;
  if (vSize > 0) and (@ABytes <> nil) then
  begin
    SetLength(TBytes(ABytes), vSize);
    GGlyphOutline(Handle, AChar, vFormat, vGlyphMetrics, vSize, @TBytes(ABytes)[0], cnstGlyphOutlineMatrix);
  end;
//  Font.Charset := vCharset;
end;


function TWinMesureContext.GetGlyphPath(AChar: Integer; out ABytes{: TBytes}): Integer;
type
{$IFNDEF SGDEL_6}
  PCustomPoint = PPoint;
{$ELSE}
{$IFDEF SGDEL_10_SEATTLE}
  PCustomPoint = {$IF ((CompilerVersion >= 30.0) and (CompilerVersion < 31.0))}PPointL{$ELSE}PPoint{$IFEND};
{$ELSE}
  PCustomPoint = PPoint;
{$ENDIF}
{$ENDIF}
var
  P: PCustomPoint absolute ABytes;
begin
  BeginPath(Handle);
  if FFont.Charset = SYMBOL_CHARSET then
  begin
    if AChar >= cnstAnsiGlyphMax then
      ExtTextOutW(Handle, 0, 0, ETO_OPAQUE, nil, @AChar, 1, nil)
    else
      ExtTextOutA(Handle, 0, 0, ETO_OPAQUE, nil, @AChar, 1, nil);
  end
  else
    ExtTextOutW(Handle, 0, 0, ETO_OPAQUE, nil, @AChar, 1, nil);
  EndPath(Handle);
  Result := {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}GetPath(Handle, nil{$IFNDEF SGDEL_10_SEATTLE}^{$ENDIF}, nil{$IFNDEF SGDEL_10_SEATTLE}^{$ENDIF}, 0);
  if (Result > 4) and (@ABytes <> nil) then
  begin
    SetLength(TBytes(ABytes), Result * (SizeOf(TPoint) + SizeOf(Byte)));
    {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}GetPath(Handle, P{$IFNDEF SGDEL_10_SEATTLE}^{$ENDIF}, {$IFDEF SGDEL_10_SEATTLE}@{$ENDIF}PPoints(@TBytes(ABytes)[0])^[Result], Result);
  end;
end;

{$ENDIF}

function _MkGlyphMethodOffs(const AGlyphParamMethod: TGlyphParamMethod): Pointer;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := TMethod(AGlyphParamMethod).Code;
end;

function sgRound(const AValue: Double): Integer; overload;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := Round(AValue);
end;

function sgRound(const AValue: Single): Integer; overload;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := Round(AValue);
end;

function RoundPoint(const APoint: TPointF): TPoint;
begin
  Result.X := Round(APoint.X);
  Result.Y := Round(APoint.Y);
end;

procedure ClearStringList(AList: TStrings);
var
  Obj: TObject;
begin
  if Assigned(AList) then
    while AList.Count > 0 do
    begin
      Obj := AList.Objects[AList.Count - 1];
      AList.Delete(AList.Count - 1);
      Obj.Free;
    end;
end;

procedure AddLine(const AList: TList; const APoint1, APoint2: TFPoint);{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vList: TList;
begin
  vList := TList.Create;
  AList.Add(vList);
  AddFPointInList(vList, APoint1);
  AddFPointInList(vList, APoint2);
end;

procedure AddLineTransformed(ATransformation: TObject;
  const AList: TList; const APoint1, APoint2: TFPoint);{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vList: TList;
begin
  vList := TList.Create;
  AList.Add(vList);
  AddFPointInList(vList, TCustomTransformation(ATransformation).Transform(APoint1));
  AddFPointInList(vList, TCustomTransformation(ATransformation).Transform(APoint2));
end;

procedure AddListTTFPoint(const AList: TList; const APoint: TFPoint); overload;
var
  vPPoint: PPointF;
begin
  New(vPPoint);
  AList.Add(vPPoint);
  vPPoint^.X := APoint.X;
  vPPoint^.Y := APoint.Y;
end;
procedure AddListTTFPoint(const AList: TList; const APoint: TF2DPoint); overload;
var
  vPPoint: PPointF;
begin
  New(vPPoint);
  AList.Add(vPPoint);
  vPPoint^.X := APoint.X;
  vPPoint^.Y := APoint.Y;
end;

function CompareStrVal(const AVal1, AVal2: string): Integer;
begin
  Result := AnsiCompareText(AVal1, AVal2);//without case sensitivity
//  Result := AnsiCompareStr(AVal1, AVal2);//with case sensitivity
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function CompareGlyphMetrics(const ASize1, ASize2: Integer;
  const AGlyphM1, AGlyphM2: TGlyphMetrics): Boolean;
begin
  Result := (ASize1 = ASize2) and CompareMem(@AGlyphM1, @AGlyphM2,
    SizeOf(TGlyphMetrics));
end;
{$ENDIF}

{$IFDEF SGFPC}
procedure _FreeTypeFontCollection_AddFolder(AFolder: string;
  AIncludeSubdirs: Boolean = false);
var
  files: TStringList;
  i: integer;
begin
  AFolder := IncludeTrailingBackslash(AFolder);
  files := TStringList.Create;
  try
    FontCollection.BeginUpdate;
    try
      FindAllFiles(files, AFolder, '*.ttf;*.ttc;*.otf', AIncludeSubdirs);
      files.Sort;
      for i := 0 to files.Count-1 do
      try
        FontCollection.AddFile(files[i]);
      except
      end;
    finally
      FontCollection.EndUpdate;
    end;
  finally
    files.Free;
  end;
end;

procedure UpdateFontDataItemsFromFontCollection(ATableFonts: TsgTableFonts);
var
  I: Integer;
  FontEnum: IFreeTypeFontEnumerator;
  LazFontItem: TCustomFontCollectionItem;
  LazFamilyItem: TCustomFamilyCollectionItem;
  SGFontItem: TsgFontItem;
  SGFamilyItem: TsgFamilyItem;
  FontStyles: TFontStyles;
begin
  ATableFonts.FFontItems.Sorted := False;
  ATableFonts.FFontItems.Duplicates := dupAccept;
  ATableFonts.FFontFiles := TsgObjectList.Create;
  ATableFonts.FFontFiles.Duplicates := dupAccept;
  try
    FontEnum := FontCollection.FontFileEnumerator;
    try
      while FontEnum.MoveNext do
      begin
        LazFontItem := FontEnum.Current;
        FontStyles := [];
        if LazFontItem.Bold then Include(FontStyles, fsBold);
        if LazFontItem.Italic then Include(FontStyles, fsItalic);
        LazFamilyItem := LazFontItem.Family;
        I := ATableFonts.FFamilies.AddObject(LazFamilyItem.FamilyName, nil);
        if ATableFonts.FFamilies.Objects[I] = nil then
        begin
           SGFamilyItem := TsgFamilyItem.Create(LazFamilyItem.FamilyName, DEFAULT_PITCH);
           ATableFonts.FFamilies.Objects[I] := SGFamilyItem;
        end
        else
          SGFamilyItem := TsgFamilyItem(ATableFonts.FFamilies.Objects[I]);
        SGFontItem := TsgFontItem.Create(SGFamilyItem, fsBold in FontStyles, fsItalic in FontStyles, 0);
        SGFontItem.FHash := _HashName(SGFontItem.BuildKey);
        SGFontItem.FFileName := LazFontItem.Filename;
        ATableFonts.FFontItems.Add(SGFontItem);
        ATableFonts.FFontFiles.Add(SGFontItem);
      end;
    finally
      FontEnum := nil;
    end;
  finally
    ATableFonts.FFontItems.Duplicates := dupIgnore;
    ATableFonts.FFontItems.Sorted := True;
    ATableFonts.FFontFiles.Duplicates := dupIgnore;
    ATableFonts.FFontFiles.Sorted := True;
  end;
end;

procedure LoadFontCollection;
var
  J: Integer;
  vosid: string;
begin
  if FontCollection.FamilyCount <= 0 then
  begin
    FontCollection.BeginUpdate;
    try
      for J := Low(FontsPaths) to High(FontsPaths) do
        _FreeTypeFontCollection_AddFolder(FontsPaths[J], True);
    finally
      FontCollection.EndUpdate;
    end;
  end;
  if FontCollection.Family[sDefaultTTFFont] = nil then
  begin
    vosid := LowerCase(getosid);
    if vosid = 'ubuntu' then
    begin
      sDefaultTTFFont := 'Ubuntu';
      sDefaultSHXBigFont := sDefaultTTFFont;
    end
    else
      if vosid = 'astralinux' then
      begin
        sDefaultTTFFont := 'Noto Sans';
        sDefaultSHXBigFont := sDefaultTTFFont;
      end;
  end;
  if TableFonts.FFontItems.Count = 0 then
     UpdateFontDataItemsFromFontCollection(TableFonts);
end;
{$ENDIF}

function QueryFamilyStyles(const AFamilyName: string; var AStyles: TFontStylesList): Boolean;
var
  I: Integer;
begin
  I := TableFonts.FFamilies.IndexOf(AFamilyName);
  Result := I >= 0;
  if Result then
    AStyles := TsgFamilyItem(TableFonts.FFamilies.Objects[I]).Fonts;
end;

function _IsFaceSupportsCharset(const ACharsets: array of TStringList;
  const AFaceName: string; ACharset: TFontCharset): Boolean;
begin
  Result := Assigned(ACharsets[ACharset]) and (ACharsets[ACharset].IndexOf(AFaceName) >= 0);
end;

function IsFaceSupportsCharset(const AFaceName: string; ACharset: TFontCharset): Boolean;
begin
  Result := _IsFaceSupportsCharset(TableFonts.FCharsets[ACharset], AFaceName, ACharset);
end;

{$IFDEF SG_HAS_WINAPI_INTERFACE}
{$IFNDEF SGFPC}
type
  TNewTextMetricEx = {$IFDEF  UNICODE}TNewTextMetricExW{$ELSE}TNewTextMetricExA{$ENDIF};
{$ENDIF}
function GetFontDataItem(var AEnumLogFontEx: TEnumLogFontEx;
  var AEnumTextMetric: TNewTextMetricEx; AFontType: Longint;
  Data: LPARAM):Longint; stdcall;
type
  PStringListArray = ^TStringListArray;
  TStringListArray = array[TFontCharset] of TStringList;
var
  P: PLogFont;
  S: string;
  FontItem: TsgFontItem;
begin
  Result := 1;
  P := @AEnumLogFontEx.elfLogFont;
  S := string(PChar(@P^.lfFaceName[0]));
  FontItem := TsgTableFonts(Data).UpdateFontDataItems(S, P^.lfWeight >= FW_BOLD,
    P^.lfItalic <> 0, P^.lfPitchAndFamily,
    {$IFDEF SGFPC}AEnumTextMetric.ntmentm.ntmSizeEM{$ELSE}AEnumTextMetric.ntmTm.ntmSizeEM{$ENDIF});
  FontItem.Flags :=
{$IFDEF SGFPC}
    AEnumTextMetric.ntmentm.ntmFlags
{$ELSE}
    Integer(AEnumTextMetric.ntmTm.ntmFlags)
{$ENDIF}
      or (AFontType shl 24);
  TsgTableFonts(Data).FCharsets[P^.lfCharSet].AddObject(S, FontItem.FFamily);
end;
{$ENDIF}

{$IFDEF SG_HAS_WINAPI_INTERFACE}
{$IFNDEF SGFPC}
procedure UpdateFontDataItemsFromEnumerator(ATableFonts: TsgTableFonts);
var
  I, C: Integer;
  vDC: HDC;
  vEnumLogFont: TEnumLogFontEx;
  vTmp: array of TsgFontItem;
  Item: TsgFontItem;
begin
  FillChar(vEnumLogFont, SizeOf(vEnumLogFont), 0);
  vEnumLogFont.elfLogFont.lfCharSet := DEFAULT_CHARSET;
  vDC := CreateCompatibleDC(0);
  try
    EnumFontFamiliesEx(vDC, {$IFDEF SGFPC}@{$ENDIF}vEnumLogFont.elfLogFont, @GetFontDataItem, LPARAM(ATableFonts), 0);
    SetLength(vTmp, ATableFonts.FFontItems.Count);
    try
      System.Move(ATableFonts.FFontItems.List^[0], vTmp[0], ATableFonts.FFontItems.Count * SizeOf(Pointer));
      FillChar(vEnumLogFont, SizeOf(vEnumLogFont), 0);
      C := Length(vTmp);
      for I := 0 to C - 1 do
      begin
        Item := TsgFontItem(vTmp[I]);
        Item.FillLogFont(vEnumLogFont.elfLogFont);
        case Ord(fsBold in Item.Style) or (Ord(fsItalic in Item.Style) shl 1) of
          1: vEnumLogFont.elfLogFont.lfItalic := 1;
          2: vEnumLogFont.elfLogFont.lfWeight := FW_BOLD;
        end;
        EnumFontFamiliesEx(vDC, {$IFDEF SGFPC}@{$ENDIF}vEnumLogFont.elfLogFont, @GetFontDataItem, LPARAM(ATableFonts), 0);
      end;
    finally
      SetLength(vTmp, 0);
    end;
  finally
    DeleteDC(vDC);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure InitializeFamilyDefaultCharset(const ACharsets: array of TStringList;
  AFamily: TsgFamilyItem);
var
  J, CharsetsCount: Integer;
  DafaultCharset: TFontCharset;
begin
  DafaultCharset := DEFAULT_CHARSET;
  CharsetsCount := 0;
  J := Low(ACharsets);
  while (J <= High(ACharsets)) and (CharsetsCount <= 1) do
  begin
    if (J <> DEFAULT_CHARSET) and _IsFaceSupportsCharset(ACharsets, AFamily.FamilyName, J) then
    begin
      Inc(CharsetsCount);
      DafaultCharset := J;
    end;
    Inc(J);
  end;
  if CharsetsCount = 1 then
    AFamily.FCharset := DafaultCharset;
end;

procedure InitFontDataItems(ATableFonts: TsgTableFonts);
var
  I: Integer;
{$IFDEF MANUALLOADFONTS}
  procedure DoUpdateFonts;
  var
    J: Integer;
  begin
    ATableFonts.FFontFiles := TsgObjectList.Create;
    for J := Low(FontsPaths) to High(FontsPaths) do
      UpdateFontItemsFromFiles(ATableFonts, FontsPaths[J]);
    ATableFonts.FFontFiles.ProcCompare := ATableFonts.CompareFileName;
    ATableFonts.FFontFiles.Sorted := True;
  end;
{$ENDIF}
begin
  if not Assigned(ATableFonts.FFontItems) then
  begin
    ATableFonts.FFontItems := TsgObjectList.Create;
    ATableFonts.FFontItems.ProcCompare := {$IFDEF FPC_OBJFPC}@{$ENDIF}ATableFonts.CompareHash;
    ATableFonts.FFontItems.Duplicates := dupIgnore;
    ATableFonts.FFontItems.Sorted := True;

    ATableFonts.FFontAliases := TsgObjectList.Create;
    ATableFonts.FFontAliases.ProcCompare := {$IFDEF FPC_OBJFPC}@{$ENDIF}ATableFonts.CompareHash;
    ATableFonts.FFontAliases.Duplicates := dupIgnore;
    ATableFonts.FFontAliases.Sorted := True;

    ATableFonts.FFamilies := TFamiliesList.Create({$IFDEF SGDEL_2009}True{$ENDIF});
    ATableFonts.FFamilies.Sorted := True;
    for I := Low(ATableFonts.FCharsets) to High(ATableFonts.FCharsets) do
      ATableFonts.FCharsets[I] := CreateStringListSorted(dupIgnore);
{$IFNDEF SG_FIREMONKEY}
  {$IFDEF SGFPC}
    UpdateFontDataItemsFromFontCollection(ATableFonts);
  {$ELSE}
    UpdateFontDataItemsFromEnumerator(ATableFonts);
  {$ENDIF}
{$ELSE}
    {$IFDEF MANUALLOADFONTS}
    DoUpdateFonts;
    {$ENDIF}
{$ENDIF}
    for I := Low(ATableFonts.FCharsets) to High(ATableFonts.FCharsets) do
      if ATableFonts.FCharsets[I].Count = 0 then
        FreeAndNil(ATableFonts.FCharsets[I]);
    for I := 0 to ATableFonts.FFamilies.Count - 1 do
      InitializeFamilyDefaultCharset(ATableFonts.FCharsets, TsgFamilyItem(ATableFonts.FFamilies.Objects[I]));
  end;
end;

procedure ExpandTTFRect(var ARect: TsgTTFRect; const APoint: TPointF);
begin
  if ARect.Left > APoint.X then
    ARect.Left := APoint.X;
  if ARect.Top < APoint.Y then
    ARect.Top := APoint.Y;
  if ARect.Right < APoint.X then
    ARect.Right := APoint.X;
  if ARect.Bottom > APoint.Y then
    ARect.Bottom := APoint.Y;
end;

function TTFBytesToString(const ABytes: TBytes; const ALanguageID: Word): string;
var
  UnicodeType: Integer;
  Enc: IEncoding;
begin
  UnicodeType := Ord(ABytes[0] = 0) or (Ord(ABytes[1] = 0) shl 1);
  case UnicodeType of
    1: Enc := EncMgr.FromCP(1201);
    2: Enc := EncMgr.FromCP(1200);
  else
    if ALanguageID = 0 then
      Enc := EncMgr.FromCP(0)
    else
      Enc := EncMgr.FromCP(1201);
  end;
  Result := Enc.GetString(ABytes);
end;

function FindFontNameInTTFStream(const AStream: Tstream;
  const ANameRecord: TsgNameRecord): string;
var
  vBuf: TBytes;
begin
  Result := '';
  SetLength(vBuf, ANameRecord.StringLength);
  try
    if AStream.Read(vBuf[0], ANameRecord.StringLength) = ANameRecord.StringLength then
      Result := TTFBytesToString(vBuf, ANameRecord.LanguageID);
  finally
    Finalize(vBuf);
  end;
end;

function FindNameRecordByLanguageID(const AStream: TStream;
  const ALangID, ANameID: Integer; const ANameTableHeader: TsgNameTableHeader;
  var ANameRecord: TsgNameRecord): Boolean; overload;
var
  I: Integer;
  vNameRecordTmp: TsgNameRecord;
begin
  Result := False;
  I := 0;
  while I < ANameTableHeader.NRCount do
  begin
    if AStream.Read(vNameRecordTmp, 12) <> 12 then
      Exit;
    SwapBufferOfBytes(@vNameRecordTmp, 12);
    if ((vNameRecordTmp.NameID = ANameID) and (vNameRecordTmp.StringLength > 0)) and
       ( (not Result) or
         ((vNameRecordTmp.LanguageID = ALangID) and
          (ANameRecord.LanguageID <> ALangID)) or
         ((vNameRecordTmp.LanguageID = cnstLangIDOfEnglish) and
          (ANameRecord.LanguageID <> cnstLangIDOfEnglish)) or
         ((vNameRecordTmp.LanguageID = 0) and (ANameRecord.LanguageID <> 0))
       ) then
    begin
      ANameRecord := vNameRecordTmp;
      Result := True;
    end;
    Inc(I);
  end;
end;

function FindNameRecordByLanguageID(const AStream: TStream;
  const ALangID: Integer; const ANameTableHeader: TsgNameTableHeader;
  var ANameRecord: TsgNameRecord): Boolean; overload;
begin
  Result := FindNameRecordByLanguageID(AStream, ALangID, 4, ANameTableHeader, ANameRecord);
end;

function FindNameRecordByLanguageID(const ANameRecords: array of TsgNameRecord;
  ALangID, ANameID: Integer; var Index: Integer): Boolean; overload;
var
  I, K: Integer;
  Indexes, Langs: array of Integer;

  function FindByIndexes(const AIndexes: array of Integer;
    const ANameRecords: array of TsgNameRecord; ALangID: Integer; var AIndex: Integer): Boolean;
  var
    J: Integer;
  begin
    Result := False;
    J := Low(AIndexes);
    while (J <= High(AIndexes)) and not Result do
      if ANameRecords[AIndexes[J]].LanguageID <> ALangID then
        Inc(J)
      else
        Inc(Result);
    if Result then
      AIndex := AIndexes[J];
  end;

begin
  Result := False;
  K := 0;
  SetLength(Indexes, Length(ANameRecords));
  for I := Low(ANameRecords) to High(ANameRecords) do
    if (ANameRecords[I].NameID = ANameID) and (ANameRecords[I].StringLength > 0) then
    begin
      Indexes[K] := I;
      Inc(K);
    end;
  SetLength(Indexes, K);
  SetLength(Langs, 1);
  Langs[High(Langs)] := ALangID;
  if ALangID <> cnstLangIDOfEnglish then
  begin
    SetLength(Langs, Length(Langs) + 1);
    Langs[High(Langs)] := cnstLangIDOfEnglish;
  end;
  if ALangID <> 0 then
  begin
    SetLength(Langs, Length(Langs) + 1);
    Langs[High(Langs)] := 0;
  end;
  I := Low(Langs);
  while (I <= High(Langs)) and not Result do
    if FindByIndexes(Indexes, ANameRecords, Langs[I], Index) then
      Inc(Result)
    else
      Inc(I);
end;

function FindTableDir(AStream: TStream; const AOffsetTable: TsgOffsetTable;
  const ATableName: AnsiString; var ATableDir: TsgTableDirectory): Boolean; overload;
var
  I: Integer;
begin
  Result := False;
  I := 0;
  while (I < AOffsetTable.NumOfTables) and not Result do
    if (AStream.Read(ATableDir, SizeOf(TsgTableDirectory)) = SizeOf(TsgTableDirectory)) and
       (PCardinal(@ATableDir.Tag[0])^ = PCardinal(ATableName)^) then
      Inc(Result)
    else
      Inc(I);
end;

function FindTableDir(const ATableDirs: array of TsgTableDirectory;
  const ATableName: AnsiString; var Index: Integer): Boolean; overload;
begin
  Result := False;
  Index := Low(ATableDirs);
  while (Index <= High(ATableDirs)) and not Result do
    if PCardinal(@ATableDirs[Index].Tag[0])^ = PCardinal(ATableName)^ then
      Inc(Result)
    else
      Inc(Index);
end;

function FindTableDirName(const AStream: TStream;
  const AOffsetTable: TsgOffsetTable; var ATableDir: TsgTableDirectory): Boolean;
begin
  Result := FindTableDir(AStream, AOffsetTable, AnsiString(cnstName), ATableDir);
end;

function TryAdd(AFonts: TStrings; const AFaceName: string;
  const AMetricFlags: Cardinal): Integer;
begin
  Result := AFonts.IndexOf(AFaceName);
  if Result < 0 then
    Result := AFonts.AddObject(AFaceName, TObject(AMetricFlags))
  else
    AFonts.Objects[Result] := TObject(Cardinal(AFonts.Objects[Result]) or AMetricFlags);
end;

procedure FindGDIFonts(const AFonts: TStrings);
var
  I: Integer;
  FontItem: TsgFontItem;
begin
  if Assigned(TableFonts) then
    for I := 0 to TableFonts.FFontItems.Count - 1 do
    begin
      FontItem := TsgFontItem(TableFonts.FFontItems[I]);
      TryAdd(AFonts, FontItem.Family.FamilyName, FontItem.Flags);
    end;
end;

function GetFileNameByFontName(const AFontName: string; AStyle: TFontStyles): string;
begin
  if TableFonts <> nil then
    Result := TableFonts.FileName[AFontName, AStyle]
  else
    Result := '';
end;

function GetFontIdByNameAndStyle(const AFontName: string;
  const AStyle: TFontStyles; ACharset: TFontCharset = cnstCharSetDef): string;
var
  P: PChar;
  Len: Integer;
  S: string;
begin
  S := HexDisplayPrefix + IntToHex(ACharset or (Ord(fsBold in AStyle) shl 8) or
    (Ord(fsItalic in AStyle) shl 9), 3);
  Len := Length(AFontName);
  SetLength(Result, Len + Length(S));
  P := PChar(Result);
  Move(PPointer(AFontName)^, P^, Len * SizeOf(Char));
  Inc(P, Len);
  Move(PPointer(S)^, P^, Length(S) * SizeOf(Char));
end;

function GetFontNameByFileName(const AFileName: string; var AStyle: TFontStyles): string;
begin
  Result := '';
  AStyle := [];
  if TableFonts <> nil then
    TableFonts.GetFont(AFileName, Result, AStyle);
end;

function SplitFullName(var AName: string; var AStyle: TFontStyles): Boolean;
var
  S: string;

  function ExcludeKey(var S: string; const AKey: string): Integer;
  begin
    Result := AnsiPos(AnsiUpperCase(AKey), AnsiUpperCase(S));
    if Result > 0 then
    begin
      if (Result + Length(AKey) <= Length(S)) and (S[Result + Length(AKey)] <> ' ') then
        Delete(S, Result + 1, Length(AKey) - 1)
      else
        Delete(S, Result, Length(AKey));
    end;
  end;

begin
  AStyle := [];
  S := AName;
  ExcludeKey(S, cnstTrueType);
  if ExcludeKey(S, cnstBold) > 0 then
    Include(AStyle, fsBold);
  if ExcludeKey(S, cnstItalic) > 0 then
    Include(AStyle, fsItalic);
  if ExcludeKey(S, cnstOblique) > 0 then
    Include(AStyle, fsItalic);
  Result := (S <> AName) or (AStyle <> []);
  AName := S;
end;

function GetFontNameWithoutStyle(const AName: string): string;
var
  vStyle: TFontStyles;
begin
  Result := AName;
  SplitFullName(Result, vStyle);
end;

function GetFontStyleByFontName(const AFontName: string): TmvFontStyles;
var
  S: string;
  vStyle: TFontStyles;
begin
  S := AFontName;
  SplitFullName(S, vStyle);
  Result := FontStylesToMVFontStyles(vStyle);
end;

function GetFontTypeByFileNameExt(const AFileName: string): TsgFontType;
const
  cnstExts: array [TsgFontType] of string = ('', '.TTF', '.OTF', '.TTC', '.FON',
    '.SHX', '');
var
  I: TsgFontType;
  vExt: string;
begin
  Result := ftUndefined;
  vExt := AnsiUpperCase(ExtractFileExt(AFileName));
  if Length(vExt) > 0 then
  begin
    for I := Low(TsgFontType) to High(TsgFontType) do
      if cnstExts[I] = vExt then
      begin
        Result := I;
        Break;
      end;
  end;
end;

function GetFontTypeByFontName(const AFontName: string): TsgFontType;
var
  I: Integer;
  FamilyItem: TsgFamilyItem;
  FontItem: TsgFontItem;
begin
  Result := ftUndefined;
  if TableFonts <> nil then
  begin
    FamilyItem := TableFonts.Family[AFontName];
    if Assigned(FamilyItem) then
    begin
      I := 0;
      while (I <= High(FamilyItem.Fonts)) and (Result = ftUndefined) do
      begin
        FontItem := FamilyItem.Font[FamilyItem.Fonts[I]];
        if Assigned(FontItem) then
          Result := GetFontTypeByFileNameExt(FontItem.FileName);
        Inc(I);
      end;
    end
    else
      Result := TableFonts.GetFontType(AFontName);
  end;
end;

function GetFontTypeEx(const AFontName: string): TsgFontTypes;
var
  I: Integer;
  FamilyItem: TsgFamilyItem;
  FontItem: TsgFontItem;
  vFontType: Cardinal;
begin
  Result := [];
  if TableFonts <> nil then
  begin
    FamilyItem := TableFonts.Family[AFontName];
    if Assigned(FamilyItem) then
    begin
      I := 0;
      while (I <= High(FamilyItem.Fonts)) do
      begin
        FontItem := FamilyItem.Font[FamilyItem.Fonts[I]];
        if Assigned(FontItem) then
        begin
          vFontType := FontItem.Flags shr 24;
          if vFontType and RASTER_FONTTYPE <> 0 then
            Include(Result, ftFON);
          if vFontType and DEVICE_FONTTYPE <> 0 then
            Include(Result, ftDFT);
          if vFontType and TRUETYPE_FONTTYPE <> 0 then
            Include(Result, ftTTF);
          if vFontType and SHX_FONTTYPE <> 0 then
            Include(Result, ftSHX);
        end;
        Inc(I);
      end;
    end
    else
      Result := [TableFonts.GetFontType(AFontName)];
  end;
  if Result = [] then
    Result := [ftUndefined];
end;

function GetNameOfFont(const AFont: TFont): string;
begin
  Result := AFont.Name;
  SetLength(Result, StrLen(PChar(Result)));
end;

procedure TT_ToFPoint(const AFixedPoint: TsgTTVector;
  out APointF: TPointF);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  APointF.X := AFixedPoint.X.value + AFixedPoint.X.fract * cnstFract16;
  APointF.Y := AFixedPoint.Y.value + AFixedPoint.Y.fract * cnstFract16;
{
  if IsHiY then
    Result.Y := -Result.Y;
}
{$ELSE}
  APointF.X := AFixedPoint.x / 64;
  APointF.Y := AFixedPoint.y / 64;
{$ENDIF}
end;

function GetFPointFromPointFx(const AFixedPoint: PsgTTVector): TFPoint;
var
  vPointF: TPointF;
begin
  TT_ToFPoint(AFixedPoint^, vPointF);
  Result.X := vPointF.X;
  Result.Y := vPointF.Y;
  Result.Z := 0;
end;

function GetQualityApproximation: Byte;
begin
  if Generator = nil then
    SetQualityApproximation(cnstDefaultQualityApproximation);
  Result := Generator.QualityApproximation;
end;

function GetLocaleFontFullNameByFileName(const AFontFileName: string): string;
var
  vBuf: array[0 .. LF_FULLFACESIZE - 1] of WideChar;
  vSize: DWORD;
begin
  Result := '';
  if Assigned(@GetFontResourceInfo) then
  begin
    vSize := SizeOf(vBuf);
    if GetFontResourceInfo(PWideChar(WideString(AFontFileName)), vSize, @vBuf[0], 1) <> 0 then
      Result := string(WideString(PWideChar(@vBuf[0])));
  end;
end;

function GetLogFontDataFromFile(const AFontFileName: string;
  var ALogFonts: TLogFontWList): Cardinal;
begin
  SetLength(ALogFonts, 0);
  Result := 0;
  if Assigned(@GetFontResourceInfo) then
  begin
    if GetFontResourceInfo(PWideChar(WideString(AFontFileName)), Result, nil, 2) <> 0 then
    begin
      SetLength(ALogFonts, Result div SizeOf(TLogFontW));
      if GetFontResourceInfo(PWideChar(WideString(AFontFileName)), Result, PWideChar(ALogFonts), 2) = 0 then
      begin
        SetLength(ALogFonts, 0);
        Result := 0;
      end;
    end;
  end;
end;

function GetFontDataFromFile(const AFontFileName: string;
  var AData: TTTFontItemDataList): Boolean;
var
  I: Integer;
  vLogFonts: TLogFontWList;
begin
  SetLength(AData, 0);
  if GetLogFontDataFromFile(AFontFileName, vLogFonts) > 0 then
  begin
    SetLength(AData, Length(vLogFonts));
    for I := Low(vLogFonts) to High(vLogFonts) do
    begin
      AData[I].Name := string(WideString(vLogFonts[I].lfFaceName));
      if vLogFonts[I].lfWeight >= FW_BOLD then Include(AData[I].Style, fsBold);
      if vLogFonts[I].lfItalic <> 0 then Include(AData[I].Style, fsItalic);
      AData[I].Pitch := vLogFonts[I].lfPitchAndFamily;
    end;
  end;
  Result := Length(AData) > 0;
end;

function QueryFontTables(AStream: TStream; ACount: Integer; var AFontTablesArray): Integer;
var
  Tables: array of TsgTableDirectory;
begin
  SetLength(Tables, ACount);
  Result := AStream.Read(Tables[0], ACount * SizeOf(TsgTableDirectory));
  Pointer(AFontTablesArray) := Pointer(Tables);
  Pointer(Tables) := nil;
end;

function QueryNamesFontTable(AStream: TStream; ACount: Integer; var ANamesTableArray): Integer;
var
  Names: array of TsgNameRecord;
begin
  SetLength(Names, ACount);
  Result := AStream.Read(Names[0], ACount * SizeOf(Names[0]));
  Pointer(ANamesTableArray) := Pointer(Names);
  Pointer(Names) := nil;
end;

function DoTTFontName(AStream: TStream; AStyle: PByte; var AEMSquare: Word; ANumOfTables: Integer): string;
var
  I, J: Integer;
  vNameTableHeader: TsgNameTableHeader;
  vMagic: Integer;
  vMacStyle: Word;
  vTables: array of TsgTableDirectory;
  vNames: array of TsgNameRecord;
begin
  Result := '';
  if QueryFontTables(AStream, ANumOfTables, vTables) = Length(vTables) * SizeOf(vTables[0]) then
  begin
    if FindTableDir(vTables, AnsiString(cnstName), I) then
    begin
      vTables[I].Offset := SwapWords(vTables[I].Offset);
      AStream.Position := vTables[I].Offset;
      if (AStream.Read(vNameTableHeader, 6) = 6) and (vNameTableHeader.Selector = 0) then
      begin
        SwapBufferOfBytes(@vNameTableHeader, 6);
        if QueryNamesFontTable(AStream, vNameTableHeader.NRCount, vNames) = Length(vNames) * SizeOf(vNames[0]) then
        begin
          SwapBufferOfBytes(@vNames[0], Length(vNames) * SizeOf(vNames[0]));
          if FindNameRecordByLanguageID(vNames, cnstLangIDOfEnglish, 1, J) then
          begin
            AStream.Position := vTables[I].Offset + vNameTableHeader.StorageOffset + vNames[J].StringOffset;
            Result := FindFontNameInTTFStream(AStream, vNames[J]);
          end;
        end;
      end;
    end;
    if Assigned(AStyle) then
    begin
      AStyle^ := AStyle^ and not $3;
      if FindTableDir(vTables, AnsiString('head'), I) then
      begin
        AStream.Position := SwapWords(vTables[I].Offset);
        AStream.Position := AStream.Position + 4+4+4;//version+revision+checksum
        AStream.Read(vMagic, SizeOf(vMagic));//0x5F0F3CF5
        AStream.Position := AStream.Position + 2;//flags
        AStream.Read(AEMSquare, SizeOf(AEMSquare));
        SwapBufferOfBytes(@AEMSquare, SizeOf(AEMSquare));
        AStream.Position := AStream.Position + 8*2 + 4*2;// dates + bounds
        AStream.Read(vMacStyle, SizeOf(vMacStyle));
        SwapBufferOfBytes(@vMacStyle, SizeOf(vMacStyle));
        AStyle^ := AStyle^ or (vMacStyle and $3);
      end;
    end;
  end;
end;

function GetTTInfo(AStream: TStream; ANumOfTables: Integer; ALangID: Integer; var A: ArrayOfString): Integer;
//type
//  TsgFreeTypeInformation = (ftiCopyrightNotice, ftiFamily, ftiStyle, ftiIdentifier, ftiFullName,
//     ftiVersionString, ftiPostscriptName, ftiTrademark, ftiManufacturer, ftiDesigner,
//     ftiVendorURL, ftiDesignerURL, ftiLicenseDescription, ftiLicenseInfoURL);
var
  I, J: Integer;
  vNameTableHeader: TsgNameTableHeader;
  vTables: array of TsgTableDirectory;
  vNames: array of TsgNameRecord;
  vBuff: TBytes;
  S: string;
begin
  Result := 0;
  if QueryFontTables(AStream, ANumOfTables, vTables) = Length(vTables) * SizeOf(vTables[0]) then
  begin
    if FindTableDir(vTables, AnsiString(cnstName), I) then
    begin
      vTables[I].Offset := SwapWords(vTables[I].Offset);
      AStream.Position := vTables[I].Offset;
      if (AStream.Read(vNameTableHeader, 6) = 6) and (vNameTableHeader.Selector = 0) then
      begin
        SwapBufferOfBytes(@vNameTableHeader, 6);
        if QueryNamesFontTable(AStream, vNameTableHeader.NRCount, vNames) = Length(vNames) * SizeOf(vNames[0]) then
        begin
          SetLength(A, 14{Ord(High(TsgFreeTypeInformation)) + 1});
          SwapBufferOfBytes(@vNames[0], Length(vNames) * SizeOf(vNames[0]));
          for J := 0 to High(vNames) do
          begin
            if vNames[J].LanguageID = ALangID then
            begin
              AStream.Position := vTables[I].Offset + vNameTableHeader.StorageOffset + vNames[J].StringOffset;
              SetLength(vBuff, vNames[J].StringLength);
              AStream.Read(vBuff[0], Length(vBuff));
              SwapBufferOfBytes(@vBuff[0], Length(vBuff));
              WideCharLenToStrVar(PWideChar(@vBuff[0]), Length(vBuff) shr 1, S);
              if vNames[J].NameID > High(A) then
                SetLength(A, vNames[J].NameID + 1);
              A[vNames[J].NameID] := S;
              Inc(Result);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GetTTFFontName(AStream: TStream; AStyle: PByte; var AEMSquare: Word): string; overload;
var
  vOffsetTable: TsgOffsetTable;
begin
  Result := '';
  if Assigned(AStyle) then AStyle^ := AStyle^ and not $3;
  if AStream.Read(vOffsetTable, 12) = 12 then
  begin
    SwapBufferOfBytes(@vOffsetTable, 12);
    if ((vOffsetTable.MajorVersion = 1) and (vOffsetTable.MinorVersion = 0)) or
       ((vOffsetTable.MajorVersion = $4F54) and (vOffsetTable.MinorVersion = $544F)) then
      Result := DoTTFontName(AStream, AStyle, AEMSquare, vOffsetTable.NumOfTables)
  end;
end;

function GetTTFFontName(const AFileName: string): string; overload;
var
  vOffsetTable: TsgOffsetTable;
  vTableDir: TsgTableDirectory;
  vNameTableHeader: TsgNameTableHeader;
  vNameRecord: TsgNameRecord;
  vLangID: Integer;
  vStream: TStream;
  vHandle: THandle;
begin
  Result:='';
  if not FileExists(AFileName) then Exit;
  vHandle := FileOpen(AFileName, fmOpenRead or fmShareDenyWrite);
  if vHandle <> THandle(INVALID_HANDLE_VALUE) then
  begin
    vStream := THandleStream.Create(vHandle);
    try
      if vStream.Read(vOffsetTable, 12) <> 12 then
        Exit;
      SwapBufferOfBytes(@vOffsetTable, 12);
      if (vOffsetTable.MajorVersion <> 1) or (vOffsetTable.MinorVersion <> 0) then
        Exit;
      if FindTableDirName(vStream, vOffsetTable, vTableDir) then
      begin
        vTableDir.Offset := SwapWords(vTableDir.Offset);
        vStream.Position := vTableDir.Offset;
        if (vStream.Read(vNameTableHeader,6) <> 6) or (vNameTableHeader.Selector <> 0) then
          Exit;
        SwapBufferOfBytes(@vNameTableHeader, 6);
        vLangID := cnstLangIDOfEnglish;//GetSystemDefaultvLangID;
        if FindNameRecordByLanguageID(vStream, vLangID, vNameTableHeader, vNameRecord) then
        begin
          vStream.Position := vTableDir.Offset + vNameTableHeader.StorageOffset +
            vNameRecord.StringOffset;
          Result := FindFontNameInTTFStream(vStream, vNameRecord);
        end;
      end;
    finally
      FileClose(vHandle);
      vStream.Free;
    end;
  end;
end;

function GetTTFFontName(const AFileName: string; var AStyle: TFontStyles; var AEMSquare: Word): string; overload;
var
  vStream: THandleStream;
begin
  Result := '';
  AStyle := [];
  vStream := THandleStream.Create(FileOpen(AFileName, fmOpenRead or fmShareDenyWrite));
  try
    if THandle(vStream.Handle) <> THandle(INVALID_HANDLE_VALUE) then
      Result := GetTTFFontName(vStream, PByte(@AStyle), AEMSquare);
  finally
{$IFNDEF SG_NON_WIN_PLATFORM}
    CloseHandle(vStream.Handle);
{$ELSE}
    FileClose(vStream.Handle);
{$ENDIF}
    vStream.Free;
  end;
end;

function GetTTFont(AStream: TStream; var AData: TTTFontItemDataList; Index: Integer = -1): Boolean; overload;
var
  vPreamble: Cardinal;
  vOffsetTable: TsgOffsetTable;
  vTTCHeader: TsgTTCHeader;
  vOffsets: array of Integer;
begin
  if Index = -1 then
    SetLength(AData, 0);
  if AStream.Read(vPreamble, 4) = 4 then
    case vPreamble of
      $66637474://'ttfc'
        begin
          PCardinal(@vTTCHeader.Tag)^ := vPreamble;
          AStream.Read(vTTCHeader.MajorVersion, SizeOf(vTTCHeader) - SizeOf(vPreamble));
          SwapBufferOfBytes(@vTTCHeader, SizeOf(vTTCHeader) - SizeOf(vTTCHeader.FontsCount));
          if (vTTCHeader.MajorVersion in [1, 2]) and (vTTCHeader.MinorVersion = 0) then
          begin
            vTTCHeader.FontsCount := SwapWords(vTTCHeader.FontsCount);
            SetLength(vOffsets, vTTCHeader.FontsCount);
            AStream.Read(vOffsets[0], vTTCHeader.FontsCount * SizeOf(vOffsets[0]));
            SwapBufferOfWords(@vOffsets[0], vTTCHeader.FontsCount * SizeOf(vOffsets[0]));
            if Index <> -1 then
            begin
              AStream.Position := vOffsets[Index];
              SetLength(AData, Length(AData) + 1);
              GetTTFont(AStream, AData, Index);
            end
            else
              for Index := 0 to vTTCHeader.FontsCount - 1 do
              begin
                AStream.Position := vOffsets[Index];
                SetLength(AData, Length(AData) + 1);
                GetTTFont(AStream, AData, Index);
              end;
          end;
        end;
      $00000100, $4F54544F://0x00010000; 'OTTO'
        begin
          if Index = -1 then
            SetLength(AData, Length(AData) + 1);
          PCardinal(@vOffsetTable)^ := vPreamble;
          AStream.Read(vOffsetTable.NumOfTables, SizeOf(vOffsetTable) - SizeOf(vPreamble));
          SwapBufferOfBytes(@vOffsetTable, SizeOf(vOffsetTable));
          AData[High(AData)].Name := DoTTFontName(AStream, PByte(@AData[High(AData)].Style),
            AData[High(AData)].EMSquare, vOffsetTable.NumOfTables);
        end;
    end;
  Result := Length(AData) > 0;
end;

function GetTTFont(const AFileName: string; var AData: TTTFontItemDataList; Index: Integer = -1): Boolean; overload;
var
  vStream: THandleStream;
begin
  if Index = -1 then
    SetLength(AData, 0);
  vStream := THandleStream.Create(FileOpen(AFileName, fmOpenRead or fmShareDenyWrite));
  try
    if THandle(vStream.Handle) <> THandle(INVALID_HANDLE_VALUE) then
      GetTTFont(vStream, AData, Index);
  finally
{$IFNDEF SG_NON_WIN_PLATFORM}
    CloseHandle(vStream.Handle);
{$ELSE}
    FileClose(vStream.Handle);
{$ENDIF}
    vStream.Free;
  end;
  Result := Length(AData) > 0;
end;

procedure InitFontsPath;
begin
  {$IFDEF SG_WIN_PLATFORM}
  if not GetSpecialFolderPath(constCSIDL_FONTS, FontsPaths[0]) then
    FontsPaths[0] := '';
  {$ENDIF}
end;

procedure InitRegKeyMicrosoftWindows;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vOSVersionInfo: TOSVersionInfo;
begin
  vOSVersionInfo.dwOSVersionInfoSize := Sizeof(TOSVersionInfo);
  GetVersionEx(vOSVersionInfo);
  if vOSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    RegKeyMicrosoftWindows := cnstRegKeyMicrosoftNT
  else
    RegKeyMicrosoftWindows := cnstRegKeyMicrosoft9X;
end;
{$ELSE}
begin

end;

{$ENDIF}

function InValues(const AValue, AMinValue, AMaxValue: WORD): Boolean;
begin
  Result := (AMinValue <= AValue) and (AValue <= AMaxValue);
end;

function IsNotTTFFont(const AFontName: string): Boolean;
var
  vFontType: TsgFontType;
begin
  vFontType := GetFontTypeByFontName(AFontName);
  Result := not (vFontType in [ftTTF, ftOTF, ftTTC]);
end;

function IsSystemNotRasterFont(const AFontName: string): Integer;
var
  I: Integer;
  FamilyItem: TsgFamilyItem;
  FontItem: TsgFontItem;
  FontType: TsgFontType;
begin
  FontType := ftUndefined;
  if TableFonts <> nil then
  begin
    FamilyItem := TableFonts.Family[AFontName];
    if Assigned(FamilyItem) then
    begin
      I := 0;
      while (I <= High(FamilyItem.Fonts)) and (FontType = ftUndefined) do
      begin
        FontItem := FamilyItem.Font[FamilyItem.Fonts[I]];
        if Assigned(FontItem) then
          FontType := GetFontTypeByFileNameExt(FontItem.FileName);
        Inc(I);
      end;
    end
    else
      FontType := TableFonts.GetFontType(AFontName);
  end;
  Result := 0;
  case FontType of
    ftUndefined, ftFON: Result := 1;
  end;
end;

function IsSystemNotRasterFontEx(const AFontName: string): Integer;
var
  vFontTypes: TsgFontTypes;
begin
  Result := -1;
  vFontTypes := GetFontTypeEx(AFontName);
  if not (ftUndefined in vFontTypes) then
    Result := Integer([ftTTF, ftOTF, ftTTC] * vFontTypes <> []);
end;

function FindForPDFName(const AFontName: string): string;
begin
  if TableFonts <> nil then
    Result := TableFonts.FindForPDFName(AFontName)
  else
    Result := '';
end;

function MakeFPointFromTTF(const APoint: TPointF): TFPoint;
begin
  Result := MakeFPointFromPoint(APoint);
end;

function MakeOffsetMat(const AX, AY: Single; const AMatrix: TFMatrix): TFMatrix;
begin
  Result := AMatrix;
  Result.M[3,0] := AMatrix.M[3,0] + AX * AMatrix.M[0, 0] + AY * AMatrix.M[1, 0];
  Result.M[3,1] := AMatrix.M[3,1] + AX * AMatrix.M[0, 1] + AY * AMatrix.M[1, 1];
end;

function TTFPointXMat(const AX, AY: Single;
  const AMatrix: TFMatrix): TPointF; overload;
begin
  Result.X := AX * AMatrix.M[0,0] + AY * AMatrix.M[1,0] + AMatrix.M[3,0];
  Result.Y := AX * AMatrix.M[0,1] + AY * AMatrix.M[1,1] + AMatrix.M[3,1];
end;

function TTFPointXMat(const AX, AY: Double;
  const AMatrix: TFMatrix): TPointF; overload;
begin
  Result.X := AX * AMatrix.M[0,0] + AY * AMatrix.M[1,0] + AMatrix.M[3,0];
  Result.Y := AX * AMatrix.M[0,1] + AY * AMatrix.M[1,1] + AMatrix.M[3,1];
end;

procedure TTFPointXMat(const AX, AY: Double;
  const AMatrix: TFMatrix; var ATX, ATY: Double); overload;
begin
  ATX := AX * AMatrix.M[0,0] + AY * AMatrix.M[1,0] + AMatrix.M[3,0];
  ATY := AX * AMatrix.M[0,1] + AY * AMatrix.M[1,1] + AMatrix.M[3,1];
end;

procedure TTFPointXMat(const AX, AY: Double;
  const AMatrix: TFMatrix; var ATX, ATY: Single); overload;
begin
  ATX := AX * AMatrix.M[0,0] + AY * AMatrix.M[1,0] + AMatrix.M[3,0];
  ATY := AX * AMatrix.M[0,1] + AY * AMatrix.M[1,1] + AMatrix.M[3,1];
end;

function TTFPointXMatto3D(const APoint: TPointF;
  const AMatrix: TFMatrix): TFPoint;
var
  vTmpPoint: TPointF;
begin
  vTmpPoint := TTFPointXMat(APoint.X, APoint.Y, AMatrix);
  Result.X := vTmpPoint.X;
  Result.Y := vTmpPoint.Y;
  Result.Z := AMatrix.M[3,2];
end;

procedure SetQualityApproximation(const AValue: Byte);
begin
  if Generator = nil then
  begin
    Generator := TsgGeneratorShapeEdge.Create;
    Generator.NumberSplinePart := 8;
  end;
  Generator.QualityApproximation := AValue;
end;

{$IFNDEF SG_FIREMONKEY}
function GetDCFontData(ADC: HDC): TFontData;
var
  hFnt: THandle;
  LogFont: TLogFont;
begin
  hFnt := SelectObject(ADC, GetStockObject(SYSTEM_FONT));
  try
{$IFNDEF SG_NON_WIN_PLATFORM}
    Result := DefFontData;
    if GetObject(hFnt, SizeOf(LogFont), @LogFont) <> 0 then
    begin
      Result.Handle := hFnt;
      Result.Height := LogFont.lfHeight;
{$IFDEF SGDEL_2006}
      Result.Orientation := LogFont.lfOrientation;
{$ENDIF}
      case LogFont.lfPitchAndFamily and $F of
        FIXED_PITCH: Result.Pitch := fpFixed;
        VARIABLE_PITCH: Result.Pitch := fpVariable;
      else
        Result.Pitch := fpDefault;
      end;
      Result.Style := [];
      if LogFont.lfWeight >= FW_BOLD then Include(Result.Style, fsBold);
      if LogFont.lfItalic <> 0 then Include(Result.Style, fsItalic);
      if LogFont.lfStrikeOut <> 0 then Include(Result.Style, fsStrikeOut);
      if LogFont.lfUnderline <> 0 then Include(Result.Style, fsUnderline);
      Result.Charset := LogFont.lfCharSet;
      Result.Name := ShortString(string(LogFont.lfFaceName));
    end;
{$ELSE}
    Result := GetFontData(hFnt);
{$ENDIF}
  finally
    SelectObject(ADC, hFnt);
  end;
end;
{$ENDIF}

{$IFDEF SGFPC}
function GetFreeTypeFont(const AFamilyName: string; AStyle: TFontStylesbase): TFreeTypeFont;
var
  FamilyItem: TCustomFamilyCollectionItem;
  FontItem: TCustomFontCollectionItem;
  Styles: ArrayOfString;
  J: Integer;
  SGFontItem: TsgFontItem;

  function GetSGFontItem: TsgFontItem;
  var
    SGFamilyItem: TsgFamilyItem;
  begin
    Result := nil;
    SGFamilyItem := TableFonts.GetFamily(AFamilyName);
    if Assigned(SGFamilyItem) then
      Result := SGFamilyItem.Font[AStyle];
  end;

begin
  Exclude(AStyle, fsUnderline);
  Exclude(AStyle, fsStrikeOut);
  Result := nil;
  LoadFontCollection;
  SGFontItem := GetSGFontItem;
  if Assigned(SGFontItem) and Assigned(SGFontItem.FFreeTypeFont) then
  begin
    Result := SGFontItem.FFreeTypeFont;
    Exit;
  end;
  FamilyItem := FontCollection.Family[AFamilyName];
  if Assigned(FamilyItem) then
  begin
    if AStyle = [] then
    begin
      SetLength(Styles, 6);
      Styles[0] := 'Regular';
      Styles[1] := 'Normal';
      Styles[2] := 'Roman';
      Styles[3] := 'Plain';
      Styles[4] := 'Book';
      Styles[5] := 'ExtraLight';
    end
    else
    begin
      SetLength(Styles, 2);
      Styles[0] := '';
      Styles[1] := '';
      if fsBold in AStyle then
      begin
        Styles[0] := cnstNameBold;
        Styles[1] := cnstNameBold;
      end;
      if fsItalic in AStyle then
      begin
        Styles[0] := Styles[0] + ' ' + cnstNameItalic;
        Styles[1] := Styles[1] + ' ' + cnstNameOblique;
      end;
    end;
    FontItem := FamilyItem.GetFont(Styles);
    if Assigned(FontItem) and Assigned(SGFontItem) then
    begin
      SGFontItem.FontCollectionItem := FontItem;
      Result := SGFontItem.FFreeTypeFont;
    end;
  end;
end;

function GetFreeTypeGlyph(const AFamilyName: string; AStyle: TFontStylesbase;
  const AKey: Integer; out AFreeTypeGlyph: TFreeTypeGlyph): Boolean;
var
  GlyphIndex: Integer;
  FreeTypeFont: TFreeTypeFont;
begin
  AFreeTypeGlyph := nil;
  FreeTypeFont := GetFreeTypeFont(AFamilyName, AStyle);
  if Assigned(FreeTypeFont) then
  begin
    GlyphIndex := FreeTypeFont.CharIndex[AKey];
    if (GlyphIndex >= 0) and (GlyphIndex < FreeTypeFont.GlyphCount) then
      AFreeTypeGlyph := FreeTypeFont.Glyph[GlyphIndex];
  end;
  Result := Assigned(AFreeTypeGlyph);
end;

function SymbolExist(const AFamilyName: string; AStyle: TFontStylesbase;
  const AKey: Integer): Boolean;
var
  FreeTypeGlyph: TFreeTypeGlyph;
begin
  Result := GetFreeTypeGlyph(AFamilyName, AStyle, AKey, FreeTypeGlyph);
end;
{$ENDIF}

function GetFontPitch(const AFontName: string): Byte;
var
  I: Integer;
begin
  Result := DEFAULT_PITCH;
  I := TableFonts.FFamilies.IndexOf(AFontName);
  if I >= 0 then
    Result := TsgFamilyItem(TableFonts.FFamilies.Objects[I]).Pitch;
end;

function UpdateTextGlyph(const AFontName: string; AStyles: TFontStyles; ACharset: TFontCharset; APitch: Byte): TsgTextGlyph;
var
  I: Integer;
begin
  I := ContainerOfTextGlyphs.IndexOf(AFontName, AStyles, ACharset);
  if I >= 0 then
    Result := ContainerOfTextGlyphs.TextGlyph[I]
  else
    if ContainerOfTextGlyphs.IsFontValid(AFontName, AStyles) then
      Result := ContainerOfTextGlyphs.AddTextGlyphsByFont(AFontName, AStyles, ACharset)
    else
      Result := ContainerOfTextGlyphs.Defaults[AStyles];
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function SymbolExistDCA(const ADC: HDC; const AKey: WORD;
  const AMat: TMat2): Boolean;
var
  vTextMetric: TTextMetricA;
  vGlyphMetrixcsByKey, vGlyphMetricsByDefault: TGlyphMetrics;
  vSizeByKey, vSizeByDefault: Integer;
  vBufferByKey, vBufferByDefault: AnsiString;
  vDefaultKey: WORD;
begin
  Result := False;
  Windows.GetTextMetricsA(ADC, {$IFDEF SGFPC}@{$ENDIF}vTextMetric);
  vDefaultKey := WORD(vTextMetric.tmDefaultChar);
  if InValues(AKey, WORD(vTextMetric.tmFirstChar), WORD(vTextMetric.tmLastChar)) then
  begin
    vSizeByKey := GetGlyphOutline(ADC, AKey, GGO_NATIVE,
      vGlyphMetrixcsByKey, 0, nil, AMat);
    vSizeByDefault := GetGlyphOutline(ADC, vDefaultKey,
      GGO_NATIVE , vGlyphMetricsByDefault, 0, nil, AMat);
    if CompareGlyphMetrics(vSizeByKey, vSizeByDefault,
      vGlyphMetrixcsByKey, vGlyphMetricsByDefault) then
    begin
      SetLength(vBufferByKey, vSizeByKey);
      GetGlyphOutline(ADC, AKey, GGO_NATIVE, vGlyphMetrixcsByKey,
        vSizeByKey, @vBufferByKey[1], AMat);
      SetLength(vBufferByDefault, vSizeByDefault);
      GetGlyphOutline(ADC, vDefaultKey, GGO_NATIVE, vGlyphMetricsByDefault,
        vSizeByDefault, @vBufferByDefault[1], AMat);
      Result := not CompareMem(@vBufferByKey[1], @vBufferByDefault[1],
        vSizeByKey);
    end
    else
      Result := True;
  end;
end;

function SymbolExistDCW(const ADC: HDC; const AKey: WORD;
  const AMat: TMat2): Boolean;
var
  vTextMetric: TTextMetricW;
  vGlyphMetricsByKey, vGlyphMetricsByDefault: TGlyphMetrics;
  vSizeByKey, vSizeByDefault: Integer;
  vBufferByKey, vBufferByDefault: AnsiString;
  vDefaultKey: WORD;
begin
  Result := False;
  Windows.GetTextMetricsW(ADC, {$IFDEF SGFPC}@{$ENDIF}vTextMetric);
  vDefaultKey := WORD(vTextMetric.tmDefaultChar);
  if InValues(AKey, WORD(vTextMetric.tmFirstChar), WORD(vTextMetric.tmLastChar)) then
  begin
    vSizeByKey := GetGlyphOutlineW(ADC, AKey, GGO_NATIVE , vGlyphMetricsByKey,
      0, nil, AMat);
    vSizeByDefault := GetGlyphOutlineW(ADC, vDefaultKey, GGO_NATIVE,
      vGlyphMetricsByDefault, 0, nil, AMat);
    if CompareGlyphMetrics(vSizeByKey, vSizeByDefault, vGlyphMetricsByKey,
     vGlyphMetricsByDefault) then
    begin
      SetLength(vBufferByKey, vSizeByKey);
      GetGlyphOutlineW(ADC, AKey, GGO_NATIVE, vGlyphMetricsByKey,
        vSizeByKey, @vBufferByKey[1], AMat);
      SetLength(vBufferByDefault, vSizeByDefault);
      GetGlyphOutlineW(ADC, vDefaultKey, GGO_NATIVE, vGlyphMetricsByDefault,
        vSizeByDefault, @vBufferByDefault[1], AMat);
      Result := not CompareMem(@vBufferByKey[1], @vBufferByDefault[1],
        vSizeByKey);
    end
    else
      Result := True;
  end;
end;
{$ENDIF}

function SymbolExistDC(const ADC: HDC; const AKey: WORD): Boolean;
{$IFDEF SG_FIREMONKEY}
begin
  Result := True;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
begin
  if AKey >= cnstGlyphsCount then
    Result := SymbolExistDCW(ADC, AKey, cnstGlyphOutlineMatrix)
  else
    Result := SymbolExistDCA(ADC, AKey, cnstGlyphOutlineMatrix);
end;
{$ELSE}
var
  FontData: TFontData;
begin
  FontData := GetDCFontData(ADC);
  Result := SymbolExist(FontData.Name, FontData.Style, AKey);
end;
{$ENDIF}
{$ENDIF}

function SymbolExistFontName(const AFontName: string; AStyle: TFontStyles;
  ACharset: TFontCharset; const AKey: WORD): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := ContainerOfTextGlyphs.IndexOf(AFontName, AStyle, ACharset);
  if I >= 0 then
    Result := ContainerOfTextGlyphs.TextGlyph[I].GetIndex(WideChar(AKey)) <> cnstInvalidGlyphIndex;
end;

function SymbolExistFont(const AFont: TFont; const AKey: WORD): Boolean;
begin
  Result := SymbolExistFontName(AFont.Name, AFont.Style,
    AFont.Charset, AKey);
end;

procedure TransTTFRectCorners(var ARect: TsgTTFRect; const AMatrix: PFMatrix);
var
  R: TsgTTFRect;
begin
  R := cnstTTFBadRect;
  ExpandTTFRect(R, TTFPointXMat(ARect.Left, ARect.Top, AMatrix^));
  ExpandTTFRect(R, TTFPointXMat(ARect.Left, ARect.Bottom, AMatrix^));
  ExpandTTFRect(R, TTFPointXMat(ARect.Right, ARect.Top, AMatrix^));
  ExpandTTFRect(R, TTFPointXMat(ARect.Right, ARect.Bottom, AMatrix^));
  ARect := R;
end;

type
  TStringBuilderAccess = class(TStringBuilder);

function CreateTextItems(const AText: string; const ATextW: WideString): TStringBuilder;
var
  I, J, K: Integer;
  vTmp: TStringBuilder;
  vChars: array of WideChar;
  vEnc: IEncoding;
  vIsRTL: Boolean;

  procedure DoRTLCpy;
  begin
    while K > 0 do
    begin
      vChars[J + K - 1] := vTmp[I - K];
      Dec(K);
    end;
    J := -1;
    K := 0;
  end;

begin
{$IFDEF SGDEL_7}
  if Length(ATextW) > 0 then
    vTmp := TStringBuilder.Create(ATextW)
  else
    vTmp := TStringBuilder.Create(AText);
{$ELSE}
  if Length(ATextW) > 0 then
    vTmp := TStringBuilder.CreateFromWideString(ATextW)
  else
    vTmp := TStringBuilder.CreateFromAnsiString(AText);
{$ENDIF}
  try
    SetLength(vChars, vTmp.Length);
    I := 0;
    J := -1;
    K := 0;
    while I < vTmp.Length do
    begin
      vEnc := EncMgr.FromChar(Ord(vTmp[I]));
      vIsRTL := (vEnc.Charset in [HEBREW_CHARSET, ARABIC_CHARSET]);
      if J >= 0 then
      begin
        if vIsRTL or IsSpace(vTmp[I]) then
          Inc(K)
        else
        begin
          DoRTLCpy;
          vChars[I] := vTmp[I];
        end;
      end
      else
        if vIsRTL then
        begin
          K := 1;
          J := I;
        end
        else
          vChars[I] := vTmp[I];
      Inc(I);
    end;
    if K > 0 then
      DoRTLCpy;
  finally
    vTmp.Free;
  end;
  Result := TStringBuilder.Create;
{$IFDEF SGDEL_10_RIO}
  SetString(TStringBuilderAccess(Result).FData, PChar(@vChars[0]), Length(vChars));
{$ELSE}
  SetLength(TStringBuilderAccess(Result).FData, Length(vChars));
  System.Move(vChars[0], TStringBuilderAccess(Result).FData[0], Length(vChars) * SizeOf(vChars[0]));
{$ENDIF}
  TStringBuilderAccess(Result).FLength := Length(vChars);
end;

{ TsgListAccess }

function TsgListAccess.GetList: PPointerArray;
begin
  Result := GetItemBase(0);
end;

{TsgContainerOfTextGlyphs}

procedure TsgContainerOfTextGlyphs.CorrectFontParams(var AName: string;
  var AStyle: TFontStyles; var ACharset: TFontCharset);
begin
  AName := string(PChar(AName));
  AStyle := AStyle * [fsBold, fsItalic];
end;

function TsgContainerOfTextGlyphs.GetCount: Integer;
begin
  FLock.BeginRead;
  try
    Result := FContainer.Count;
  finally
    FLock.EndRead;
  end;
end;

function TsgContainerOfTextGlyphs.GetDefaults(Style: TFontStyles): TsgTextGlyph;
var
  J: Integer;
begin
  FLock.BeginWrite;
  try
    if FDefaults[PByte(@Style)^ and $3] = nil then
    begin
      J := IndexOf(sDefaultTTFFont, Style, cnstCharSetDef);
      if J >= 0 then
        FDefaults[PByte(@Style)^ and $3] := TextGlyph[J]
      else
        FDefaults[PByte(@Style)^ and $3] := AddTextGlyphsByFont(sDefaultTTFFont, Style)
    end;
    Result := FDefaults[PByte(@Style)^ and $3];
  finally
    FLock.EndWrite;
  end;
end;

function TsgContainerOfTextGlyphs.GetTextGlyph(Index: Integer): TsgTextGlyph;
begin
  FLock.BeginRead;
  try
  if Index < FContainer.Count then
    Result := TsgTextGlyph(FContainer[Index].Data)
  else
    Result := nil;
  finally
    FLock.EndRead;
  end;
end;

constructor TsgContainerOfTextGlyphs.Create;
begin
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;//TCriticalSection.Create;
  FContainer := TsgCollection.Create;
  FContainer.Duplicates := dupIgnore;
  FRefs := 0;
end;

destructor TsgContainerOfTextGlyphs.Destroy;
begin
  Clear;
  FreeAndNil(FContainer);
  FLock.Free;
  inherited Destroy;
end;

function TsgContainerOfTextGlyphs.DoAddTextGlyphsByFont(AFontName: string;
  AFontStyle: TFontStyles; ACharSet: TFontCharset): TsgTextGlyph;
var
  I, C: Integer;
  Hash: THandle;
  vTextGlyph: TsgTextGlyph;
begin
  vTextGlyph := TsgTextGlyph.Create(AFontName, AFontStyle, ACharset);
{$IFDEF SG_OPENING_IN_THEADS}
  vTextGlyph.FOwnerLock := FLock;
{$ENDIF}
  Hash := _HashName(GetFontIdByNameAndStyle(vTextGlyph.FontName, vTextGlyph.FontStyle, vTextGlyph.Charset));
  C := FContainer.Count;
  I := FContainer.Add(Hash, vTextGlyph);
  if C = FContainer.Count then
  begin
    vTextGlyph.Free;
    vTextGlyph := TsgTextGlyph(FContainer[I].Data);
  end;
  Result := vTextGlyph;
{$IFDEF SG_FM_MOBILE}
  Pointer(vTextGlyph) := nil;
{$ENDIF}
end;

{  AddTextGlyphsByFont
   AFont - Describes font characteristics used when displaying text
   This function creates and adds container of symbols for the specified text
   style                                                                       }
function TsgContainerOfTextGlyphs.AddTextGlyphsByFont(const AFont: {$IFDEF SG_NON_WIN_PLATFORM}TFont{$ELSE}TmvExtFont{$ENDIF}):
  TsgTextGlyph;
begin
  Result := AddTextGlyphsByFont(AFont.Name,
    {$IFDEF SG_NON_WIN_PLATFORM}AFont.Style{$ELSE}MVFontStylesToFontStyles(AFont.Style){$ENDIF},
    AFont.Charset);
end;

function TsgContainerOfTextGlyphs.AddTextGlyphsByFont(AFontName: string;
  AFontStyle: TFontStyles; ACharSet: TFontCharset = cnstCharSetDef): TsgTextGlyph;
begin
  Lock;
  try
    CorrectFontParams(AFontName, AFontStyle, ACharSet);
    Result := DoAddTextGlyphsByFont(AFontName, AFontStyle, ACharSet);
  finally
    Unlock;
  end;
end;

procedure TsgContainerOfTextGlyphs.Clear;
begin
  FContainer.ClearTypeList(ptvObject);
{$IFDEF AUTOREFCOUNT}
  Finalize(FDefaults);
{$ENDIF}
  FillChar(FDefaults, SizeOf(FDefaults), 0);
end;

function TsgContainerOfTextGlyphs.IndexOf(AFontName: string;
  AStyles: TFontStyles; ACharSet: TFontCharset): Integer;
var
  Hash: THandle;
begin
  FLock.BeginRead;
  try
    CorrectFontParams(AFontName, AStyles, ACharSet);
    Hash := _HashName(GetFontIdByNameAndStyle(AFontName, AStyles, ACharSet));
    Result := FContainer.IndexOf(Hash);
  finally
    FLock.EndRead;
  end;
end;

function TsgContainerOfTextGlyphs.IndexOf(AFontName: string;
  AStyles: TmvFontStyles; ACharSet: TFontCharset): Integer;
begin
  Result := IndexOf(AFontName, MVFontStylesToFontStyles(AStyles), ACharSet);
end;

function TsgContainerOfTextGlyphs.IsDefault(ATextGlyph: TsgTextGlyph): Boolean;
begin
  Result := False;
  if Assigned(ATextGlyph) then
    Result := AnsiSameText(ATextGlyph.FontName, sDefaultTTFFont) or
      ((ATextGlyph = FDefaults[0]) or (ATextGlyph = FDefaults[1]) or
       (ATextGlyph = FDefaults[2]) or (ATextGlyph = FDefaults[3]));
end;

function TsgContainerOfTextGlyphs.IsFontValid(const AFamilyName: string;
  AStyle: TFontStyles): Boolean;
var
  I: Integer;
  FamilyItem: TsgFamilyItem;
begin
  Result := False;
  FamilyItem := TableFonts.Family[AFamilyName];
  if Assigned(FamilyItem) then
  begin
    Result := True;
    AStyle := AStyle * [fsBold, fsItalic];
    if not FamilyItem.Find(AStyle, I) and (Length(FamilyItem.Fonts) > 1) then
      Result := False;
  end;
end;

procedure TsgContainerOfTextGlyphs.Lock;
begin
  FLock.BeginWrite;//FLock.Enter;
end;

procedure TsgContainerOfTextGlyphs.Reference;
begin
  Inc(FRefs);
end;

procedure TsgContainerOfTextGlyphs.Release;
begin
  if Self = nil then
    Exit;
  Dec(FRefs, Integer(FRefs > 0));
  if FRefs < 1 then
    Clear;
end;

procedure TsgContainerOfTextGlyphs.Unlock;
begin
  FLock.EndWrite;// FLock.Leave;
end;

{ TsgGlyphBuilder }

function IsBadTTFRect(ARect: TsgTTFRect): Boolean;
begin
  Result := False;
  if ARect.Left = cnstTTFBadRect.Left then
    if ARect.Right = cnstTTFBadRect.Right then
      if ARect.Top = cnstTTFBadRect.Top then
        if ARect.Bottom = cnstTTFBadRect.Bottom then
          Result := True;
end;

function GetBounds(const APolygons: TPolygons): TsgTTFRect;
var
  I, J: Integer;
begin
  Result := cnstTTFBadRect;
  for I := Low(APolygons) to High(APolygons) do
    for J := Low(APolygons[I]) to High(APolygons[I]) do
      ExpandTTFRect(Result, APolygons[I][J]);
end;

procedure TsgGlyphBuilder.AddDecomposeCubicSplineInList(AList: TsgPointFList;
  const APoints: array of TPointF);
begin
  Generator.SetBaseList(Last);
  Generator.CreateCubicSpline(MakeFPointFromTTF(APoints[0]),
    MakeFPointFromTTF(APoints[1]),
    MakeFPointFromTTF(APoints[2]),
    MakeFPointFromTTF(APoints[3]));
end;

{$IFDEF SG_WINAPI}
procedure TsgGlyphBuilder.AddDecomposeLineInList(AList: TsgPointFList;
  const APoints: array of TPointfx);
var
  I: Integer;
begin
  for I := Low(APoints) to High(APoints) do
    AList.Add(GetPointFromPointFX(APoints[I]));
end;

procedure TsgGlyphBuilder.AddDecomposeQSplineInList(AList: TsgPointFList;
  const APoints: array of TPointfx);
var
  I: Integer;
  vPoint1, vPoint2: TPointF;
begin
  if Length(APoints) > 0 then
  begin
    I := 0;
    vPoint1 := GetPointFromPointFX(APoints[I]);
    Inc(I);
    while I <= High(APoints) do
    begin
      vPoint2 := GetPointFromPointFX(APoints[I]);
      if I < High(APoints) then
        CurveTo(vPoint1, MiddlePoint(vPoint1, vPoint2))
      else
        CurveTo(vPoint1, vPoint2);
      vPoint1 := vPoint2;
      Inc(I);
    end;
  end;
end;
{$ENDIF}

function TsgGlyphBuilder.CurveTo(const APointCurve1,
  APointCurve2: TPointF): TPointF;
begin
  Result := Last.Last;
  Last.Add(Result);
  Generator.SetBaseList(Last);
  CurveToEx(Last, Result, APointCurve1, APointCurve2);
end;

function TsgGlyphBuilder.CurveToEx(ALast: TsgPointFList; const APointCurveStart,
  APointCurve1, APointCurve2: TPointF): TPointF;
begin
  Generator.CreateQuadraticSpline(MakeFPointFromPoint(APointCurveStart),
    MakeFPointFromPoint(APointCurve1), MakeFPointFromPoint(APointCurve2));
  ALast.Add(APointCurve2);
  Result := APointCurve2;
end;

function TsgGlyphBuilder.GetPoint(const AVector: TsgTTVector): TPointF;
begin
  TT_ToFPoint(AVector, Result);
end;

{$IFDEF SG_WINAPI}
function TsgGlyphBuilder.GetPointFromPointFX(const AVector: TPointFX): TPointF;
begin
  Result.X := AVector.X.value + AVector.X.fract * cnstFract16;
  Result.Y := AVector.Y.value + AVector.Y.fract * cnstFract16;
end;
{$ENDIF}

function TsgGlyphBuilder.Last: TsgPointFList;
begin
  Result := FPolylines[FCount - 1];
end;

procedure TsgGlyphBuilder.MoveTo(const APointMove: TPointF);
begin
  if FCount > 0 then
    Last.Add(Last.First);
  AddPolygon(TsgPointFList.Create);
  Generator.SetBaseList(Last);
  Last.Add(APointMove);
end;

procedure TsgGlyphBuilder.Pack;
const
  cntEps = 0.1 / $10000;// 0.1 * cnstFract16
var
  I, J, C, K: Integer;
  P: PsgPointFArray;
begin
  for I := FCount - 1 downto 0 do
  begin
    C := FPolylines[I].Count;
    if C > 0 then
    begin
      P := FPolylines[I].List;
      K := 0;
      for J := 1 to C - 1 do
        if not IsEqualPoints(P^[J], P^[K], cntEps) then
        begin
          Inc(K);
          P^[K] := P^[J];
        end;
      FPolylines[I].Count := K + 1;
    end;
  end;
end;

procedure TsgGlyphBuilder.Transform(ATransform: TCustomTransformation);
var
  I, J: Integer;
begin
  for I := 0 to FCount - 1 do
    for J := 0 to FPolylines[I].Count - 1 do
      FPolylines[I].List^[J] := ATransform.Transform2D(FPolylines[I].List^[J]);
end;

procedure TsgGlyphBuilder.DoExtract(var APolygons: TPolygons);
var
  I, C: Integer;
begin
  SetLength(APolygons, FCount);
  for I := FCount - 1 downto 0 do
  begin
    C := FPolylines[I].Count;
    if C > 0 then
    begin
      SetLength(APolygons[I], C);
      Move(FPolylines[I].List^[0], APolygons[I][0], C * SizeOf(TPointF));
    end;
    FPolylines[I].Free;
  end;
end;

function TsgGlyphBuilder.Extract(ATransformation: TCustomTransformation;
  var ABox: TsgTTFRect; var APolygons: TPolygons): Boolean;
begin
  Pack;
  DoExtract(APolygons);
  ATransformation.TransformPolygons(APolygons);
  ABox := GetBounds(APolygons);
  Result := not IsBadTTFRect(ABox);
end;

function TsgGlyphBuilder.AddPolygon(APolygon: TsgPointFList): Integer;
begin
  Result := FCount;
  if Result = Length(FPolylines) then
    SetLength(FPolylines, ListGrow(Length(FPolylines)));
  FPolylines[Result] := APolygon;
  Inc(FCount);
end;

{$IFDEF SG_WINAPI}
procedure TsgGlyphBuilder.DecomposeOutline(ABuffer: Pointer; ASize: Integer);
type
  PPointsfx = ^TPointsfx;
  TPointsfx = array[Byte] of TPointfx;
var
  vPoint: TPointF;
  vPolyHeader: PTTPolygonHeader;
  vPPolyCurve: PTTPolyCurve;
  vGlyphCurrent, vGlyphEnd, vPolyEnd, vPolyCurrent: PAnsiChar;
begin
  vGlyphCurrent := ABuffer;
  vGlyphEnd := vGlyphCurrent + ASize;
  while TsgNativePointer(@vGlyphCurrent[0]) < TsgNativePointer(@vGlyphEnd[0]) do
  begin
    vPolyHeader := @vGlyphCurrent[0];
    vPolyEnd := vGlyphCurrent + vPolyHeader^.cb;
    vPolyCurrent := vGlyphCurrent + SizeOf(TTTPolygonHeader);
    vPoint := GetPointFromPointFX(vPolyHeader^.pfxStart);
    MoveTo(vPoint);
    while TsgNativePointer(@vPolyCurrent[0]) < TsgNativePointer(@vPolyEnd[0]) do
    begin
      vPPolyCurve := @vPolyCurrent[0];
      case vPPolyCurve^.wType of
        TT_PRIM_LINE:
          begin
            AddDecomposeLineInList(Last,
              Slice(PPointsfx(@vPPolyCurve^.apfx)^, vPPolyCurve^.cpfx));
          end;
        TT_PRIM_QSPLINE:
          begin
            AddDecomposeQSplineInList(Last,
              Slice(PPointsfx(@vPPolyCurve^.apfx)^, vPPolyCurve^.cpfx));
          end;
      end;
      vPolyCurrent := vPolyCurrent + Sizeof(WORD) shl 1 + SizeOf(vPPolyCurve^.apfx[0]) *
        vPPolyCurve^.cpfx;
    end;
    vGlyphCurrent := vGlyphCurrent + vPolyHeader^.cb;
  end;
  if FCount > 0 then
    Last.Add(Last.First);
end;
{$ENDIF}

{  Decompose

ABuffer - pointer to the memory space that contains information about the symbol
ASize  -  size to the memory space that contains information about the symbol

   Decodes information about the symbol's structure extracting its outline  }

procedure TsgGlyphBuilder.Decompose(const ABuffer: TsgGlyphOutlineData; ASize: Integer);
{$IFDEF SG_FIREMONKEY}
var
  I, J: Integer;
  vList: TsgPointFList;
  vPolyBreak: Boolean;
  P: TPointF;
begin
  if Length(ABuffer) > 0 then
  begin
    I := Low(ABuffer);
    repeat
      SetLength(FPolylines, Length(FPolylines) + 1);
      vList := TsgPointFList.Create;
      FPolylines[High(FPolylines)] := vList;
      J := I;
      vPolyBreak := False;
      while (J <= High(ABuffer)) and not vPolyBreak do
      begin
        P := ABuffer[J];
        vPolyBreak := (P.X = PolygonPointBreak.X) and (P.Y = PolygonPointBreak.Y);
        if not vPolyBreak then
        begin
          vList.Add(P);
          Inc(I);
          Inc(J);
        end;
      end;
      Inc(I);
    until I > High(ABuffer);
    FCount := Length(FPolylines);
  end;
end;
{$ELSE}
{$IFDEF SG_NON_WIN_PLATFORM}
var
  vPoint: TPointF;
  I, J, Cnt, vStart: Integer;
  vLast: TsgPointFList;
  vPointPrev, vPointLast: TPointF;
  vFlag: Byte;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  DecomposeOutline(ABuffer, ASize);
{$ELSE}
  I := 0;
  vStart := 0;
  vLast := nil;
  while I < ABuffer.n_contours do
  begin
    AddPolygon(TsgPointFList.Create);
    vLast := Last;
    Generator.SetBaseList(vLast);
    J := 0;
    Cnt := ABuffer.conEnds^[I] - vStart + 1;
    if J < Cnt then
    begin
      vPoint := GetPoint(ABuffer.points^[vStart + J mod Cnt]);
      vFlag := ABuffer.flags^[vStart + J mod Cnt];
      repeat
        vPointPrev := vPoint;
        Inc(J);
        vPoint := GetPoint(ABuffer.points^[vStart + J mod Cnt]);
        if (vFlag and 1 <> 0) or (vLast.Count = 0) then
        begin
          if vFlag and 1 <> 0 then
            vPointLast := vPointPrev
          else
            vPointLast := MiddlePoint(vPointPrev, vPoint);
          vLast.Add(vPointLast);
          vFlag := ABuffer.flags^[vStart + J mod Cnt];
        end
        else
        begin
          vFlag := ABuffer.flags^[vStart + J mod Cnt];
          if vFlag and 1 <> 0 then
            vPointLast := CurveToEx(vLast, vPointLast, vPointPrev, vPoint)
          else
            vPointLast := CurveToEx(vLast, vPointLast, vPointPrev, MiddlePoint(vPointPrev, vPoint));
        end;
      until J > Cnt;
    end;
    vStart := vStart + Cnt;
    Inc(I);
  end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF SG_FIREMONKEY}
procedure TsgGlyphBuilder.Decompose(const APathData: TPathData);
var
  I: Integer;
begin
  if APathData.Count > 0 then
  begin
    I := 0;
    while I < APathData.Count do
    begin
      case APathData[I].Kind of
        TPathPointKind.MoveTo: MoveTo(APathData[I].Point);
        TPathPointKind.LineTo: Last.Add(APathData[I].Point);
        TPathPointKind.CurveTo:
          begin
            AddDecomposeCubicSplineInList(Last, [Last.Last, APathData[I].Point,
              APathData[I + 1].Point, APathData[I + 2].Point]);
            Inc(I, 2);
          end;
      end;
      Inc(I);
    end;
    if FCount > 0 then
      Last.Add(Last.First);
  end;
end;
{$ENDIF}

procedure TsgGlyphBuilder.DecomposePath(const APoints; const ATypes; ACount: Integer);
{$IFDEF SG_WINAPI}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF SG_WINAPI}
  if PInteger(@ATypes)^ = $03020206 then // is rect
    I := 4
  else
    I := 0; //wine do not draw "OPAQUE" rect ??
  while I < ACount do
  begin
    case PByteArray(@ATypes)^[I] and not PT_CLOSEFIGURE of
      PT_MOVETO: MoveTo(MakePointF(PPoints(@APoints)^[I]));
      PT_LINETO: Last.Add(MakePointF(PPoints(@APoints)^[I]));
      PT_BEZIERTO:
        begin
          AddDecomposeCubicSplineInList(Last, [Last.Last, MakePointF(PPoints(@APoints)^[I]),
            MakePointF(PPoints(@APoints)^[I+1]), MakePointF(PPoints(@APoints)^[I+2])]);
          Inc(I, 2);
        end;
    end;
    Inc(I);
  end;
  if (FCount > 0) and not IsEqualPoints(Last.First, Last.Last) then
    Last.Add(Last.First);
{$ENDIF}
end;

{ TsgGlyph }

function TsgGlyph.AddPolygon(const APolygon: TPolygon): Integer;
begin
  Result := Length(FPolygons);
  SetLength(FPolygons, Result + 1);
  FPolygons[High(FPolygons)] := APolygon;
end;

procedure TsgGlyph.ClearPoints;
begin
  Finalize(FPolygons);
end;

constructor TsgGlyph.Create;
begin
  FBlackBox := cnstTTFRectZero;
end;

destructor TsgGlyph.Destroy;
begin
  ClearPoints;
  inherited Destroy;
end;

{$IFDEF SG_USE_AGG2D_AS_GDI}
procedure TsgGlyph.DoDraw(ATransformation: TObject; AParam: THandle);
var
  I, J, vLen, vPointsCount: Integer;
  vPoly: TPolygon;
  vDrawGlyphPolyCount: Integer;
  vDrawGlyphPtsCount: array of Integer;
  vDrawGlyphPts: TPolygon;
begin
  vPointsCount := 0;
  vDrawGlyphPolyCount := Length(FPolygons);
  SetLength(vDrawGlyphPtsCount, vDrawGlyphPolyCount);
  vLen := 0;
  for I := 0 to vDrawGlyphPolyCount - 1 do
  begin
    vDrawGlyphPtsCount[I] := Length(FPolygons[I]);
    Inc(vLen, vDrawGlyphPtsCount[I]);
  end;
  SetLength(vDrawGlyphPts, vLen);
  for I := 0 to vDrawGlyphPolyCount - 1 do
  begin
    vPoly := FPolygons[I];
    for J := Low(vPoly) to High(vPoly) do
    begin
      vDrawGlyphPts[vPointsCount] := TCustomTransformation(ATransformation).Transform2D(vPoly[J]);
      Inc(vPointsCount);
    end;
  end;
  if vPointsCount > 0 then
    PsgGlyphDrawParam(AParam)^.Context.DoPolyPoly(vDrawGlyphPts[0], vDrawGlyphPtsCount,
      PsgGlyphDrawParam(AParam)^.TextMode, True);
end;
{$ELSE}
procedure TsgGlyph.DoDraw(ATransformation: TObject; AParam: THandle);
var
  vDrawGlyphPts: TsgPointsArray;
  vDrawGlyphPtsCount: TsgCountsArray;
  vDrawGlyphPolyCount: Integer;
begin
  if UpdateGlyphPoints(ATransformation, vDrawGlyphPts, vDrawGlyphPtsCount, vDrawGlyphPolyCount) > 0 then
  try
    if PsgGlyphDrawParam(AParam)^.TextMode and 2 <> 0 then
      PsgGlyphDrawParam(AParam)^.Context.DoPolyPolygon(vDrawGlyphPts[0], @vDrawGlyphPtsCount[0], vDrawGlyphPolyCount);
    if PsgGlyphDrawParam(AParam)^.TextMode and 1 <> 0 then
      PsgGlyphDrawParam(AParam)^.Context.DoPolyPolyline(vDrawGlyphPts[0], @vDrawGlyphPtsCount[0], vDrawGlyphPolyCount);
  finally
    Finalize(vDrawGlyphPts);
    Finalize(vDrawGlyphPtsCount);
  end;
//  DrawBox(ATransformation);
end;
{$ENDIF}

procedure TsgGlyph.DoGetPolyPointsCustom(ATransformation: TObject;
  AParam: THandle);
var
  vTransformation: TCustomTransformation absolute ATransformation;
  J, K, CntJ, CntK: Integer;
  vPoints: TPolygon;
  AddPt: TAddPt;
  P: TFPoint;
begin
  CntJ := Length(FPolygons) - 1;
  for J := 0 to CntJ do
  begin
    vPoints := FPolygons[J];
    AddPt := TsgCustomPolyItemsCollection(AParam).NewCollection();
    CntK := Length(vPoints);
    Dec(CntK);
    for K := 0 to CntK do
    begin
      P := vTransformation.Transform(vPoints[K]);
      AddPt(P);
    end;
  end;
end;

procedure TsgGlyph.DoGetPolyPointsCustomEpsilon(ATransformation: TObject;
  AParam: THandle);
var
  vTransformation: TCustomTransformation absolute ATransformation;
  vParam: PsgGlyphDrawParam absolute AParam;
  J, K, CntJ, CntK: Integer;
  vPoints: TPolygon;
  AddPt: TAddPt;
  P, vPrev: TFPoint;
begin
  CntJ := Length(FPolygons) - 1;
  for J := 0 to CntJ do
  begin
    vPoints := FPolygons[J];
    AddPt := TsgCustomPolyItemsCollection(vParam^.Collection).NewCollection();
    CntK := Length(vPoints);
    K := 0;
    vPrev := vTransformation.Transform(vPoints[K]);
    AddPt(vPrev);
    Inc(K);
    while K < CntK do
    begin
      P := vTransformation.Transform(vPoints[K]);
      if not IsRangeFPoints(vPrev, P, vParam^.Epsilon^) then
      begin
        AddPt(P);
        vPrev := P;
      end;
      Inc(K);
    end;
  end;
end;

procedure TsgGlyph.DoGetPolyPointsSingle(ATransformation: TObject; AParam: THandle);
var
  vTransformation: TCustomTransformation absolute ATransformation;
  vParam: PsgGlyphDrawParam absolute AParam;
  J, K, CntJ, CntK: Integer;
  vListNew: TList;
  vPoints: TPolygon;
begin
  CntJ := Length(FPolygons) - 1;
  for J := 0 to CntJ do
  begin
    vPoints := FPolygons[J];
    vListNew := TList.Create;
    TList(vParam^.Collection).Add(vListNew);
    CntK := Length(vPoints);
    vListNew.Capacity := CntK;
    Dec(CntK);
    for K := 0 to CntK do
      AddFPointInList(vListNew, vTransformation.Transform(vPoints[K]));
  end;
end;

procedure TsgGlyph.Draw(AContext: TsgProxyBase; const AXOffset: Double;
  const AMatrix: TFMatrix; const AFill: Boolean);
var
  vTransformation: TCustomTransformation;
  vGlyphParam: TsgGlyphDrawParam;
begin
  vTransformation := TTransformation.Create(AMatrix);
  try
    vTransformation.Translate(MakeFPoint(AXOffset, 0, 0));
    vGlyphParam.Context := AContext;
    if AFill then
      vGlyphParam.TextMode := 2
    else
      vGlyphParam.TextMode := 1;
    DoDraw(vTransformation, THandle(@vGlyphParam));
  finally
    vTransformation.Free;
  end;
end;

{procedure TsgGlyph.DrawBox(ATransformation: TObject);
const
  cnstBrush: TLogBrush = (lbStyle: BS_SOLID; lbColor: clRed; lbHatch: 0);
var
  vPen: HPEN;
  vBrush: HBRUSH;
  vBox: array[0..4] of TPoint;
  vTransformation: TCustomTransformation absolute ATransformation;
begin
  vBox[0] := vTransformation.Transform2D(FBlackBox.Left, FBlackBox.Top);
  vBox[1] := vTransformation.Transform2D(FBlackBox.Right, FBlackBox.Top);
  vBox[2] := vTransformation.Transform2D(FBlackBox.Right, FBlackBox.Bottom);
  vBox[3] := vTransformation.Transform2D(FBlackBox.Left, FBlackBox.Bottom);
  vBox[4] := vBox[0];
  //GDIAdapter.Polyline(vBox, Length(vBox));

  vPen := SelectObject(GDIAdapter.DC, CreatePen(PS_SOLID, 1, clRed));
  //vBrush := SelectObject(GDIAdapter.DC, GetStockObject(NULL_BRUSH));
  GDIAdapter.Polyline(vBox, Length(vBox));
  //SelectObject(GDIAdapter.DC, vBrush);
  DeleteObject(SelectObject(GDIAdapter.DC, vPen));

  vBox[0] := vTransformation.Transform2D(0, FBlackBox.Top);
  vBox[1] := vTransformation.Transform2D(FAdvance, FBlackBox.Top);
  vBox[2] := vTransformation.Transform2D(FAdvance, FBlackBox.Bottom);
  vBox[3] := vTransformation.Transform2D(0, FBlackBox.Bottom);
  vBox[4] := vBox[0];
  //GDIAdapter.Polyline(vBox, Length(vBox));

  vPen := SelectObject(GDIAdapter.DC, CreatePen(PS_SOLID, 1, clBlue));
  vBrush := SelectObject(GDIAdapter.DC, GetStockObject(NULL_BRUSH));
  //GDIAdapter.Polyline(vBox, Length(vBox));
  RoundRect(GDIAdapter.DC, vBox[0].X, vBox[0].Y - 4, vBox[2].X, vBox[2].Y + 4, 10, 10);
  SelectObject(GDIAdapter.DC, vBrush);
  DeleteObject(SelectObject(GDIAdapter.DC, vPen));

  vBrush := SelectObject(GDIAdapter.DC, CreateBrushIndirect(cnstBrush));
  vBox[4] := vTransformation.Transform2D((FBlackBox.Left + FBlackBox.Right)/2, 0);
  Rectangle(GDIAdapter.DC, vBox[4].X - 4, vBox[4].Y, vBox[4].X + 4, vBox[4].Y + 8);
  DeleteObject(SelectObject(GDIAdapter.DC, vBrush));
end;}

function TsgGlyph.GetBlackX: Single;
begin
  Result := FBlackBox.Right - FBlackBox.Left
end;

function TsgGlyph.GetBlackY: Single;
begin
  Result := FBlackBox.Top - FBlackBox.Bottom;
end;

function TsgGlyph.GetRightSpace: Single;
begin
  Result := FAdvance - FBlackBox.Right;
end;

function TsgGlyph.UpdateGlyphPoints(ATransformation: TObject; var ADrawGlyphPts: TsgPointsArray;
  var ADrawGlyphPtsCount: TsgCountsArray; var ADrawGlyphPolyCount: Integer): Integer;
var
  I, J, vPolyCount, vLen: Integer;
  vPoly: TPolygon;
  vDrawGlyphPolyCount: Integer;
  P: TPoint;
begin
  Result := 0;
  ADrawGlyphPolyCount := 0;
  vDrawGlyphPolyCount := Length(FPolygons);
  if Length(ADrawGlyphPtsCount) < vDrawGlyphPolyCount then
    SetLength(ADrawGlyphPtsCount, vDrawGlyphPolyCount);
  vLen := 0;
  for I := 0 to vDrawGlyphPolyCount - 1 do
  begin
    ADrawGlyphPtsCount[I] := Length(FPolygons[I]);
    Inc(vLen, ADrawGlyphPtsCount[I]);
  end;
  if Length(ADrawGlyphPts) < vLen then
    SetLength(ADrawGlyphPts, vLen);
  for I := 0 to vDrawGlyphPolyCount - 1 do
  begin
    vPoly := FPolygons[I];
    if Length(vPoly) > 0 then
    begin
      vPolyCount := 1;
      J := Low(vPoly);
      P := RoundPoint(TCustomTransformation(ATransformation).Transform2D(vPoly[J]));
      ADrawGlyphPts[Result] := P;
      Inc(Result);
      Inc(J);
      while J <= High(vPoly) do
      begin
        ADrawGlyphPts[Result] := RoundPoint(TCustomTransformation(ATransformation).Transform2D(vPoly[J]));
        if (ADrawGlyphPts[Result].X <> P.X) or (ADrawGlyphPts[Result].Y <> P.Y) then
        begin
          P := ADrawGlyphPts[Result];
          Inc(Result);
          Inc(vPolyCount);
        end;
        Inc(J);
      end;
      ADrawGlyphPtsCount[I] := vPolyCount;
      if vPolyCount = 1 then
        Dec(Result);
    end;
  end;
  if Result > 0 then
    for I := 0 to vDrawGlyphPolyCount - 1 do
      if ADrawGlyphPtsCount[I] > 1 then
      begin
        ADrawGlyphPtsCount[ADrawGlyphPolyCount] := ADrawGlyphPtsCount[I];
        Inc(ADrawGlyphPolyCount);
      end;
end;

{TsgTableFonts}

function TsgTableFonts.AddFontFilesInfoEx(AFileName, APath: string): Boolean;
var
  I, J: Integer;
  vPath: string;
  vFontItem, vFontAlias: TsgFontItem;
  vData: TTTFontItemDataList;
begin
  if APath = '' then APath := FontsPaths[0];
  vPath := GetCurrentDir;
  try
    SetCurrentDir(APath);
    if not GetTTFont(AFileName, vData) then
      GetFontDataFromFile(AFileName, vData);
    for I := Low(vData) to High(vData) do
    begin
      vFontItem := UpdateFontDataItems(vData[I].Name, fsBold in vData[I].Style, fsItalic in vData[I].Style, vData[I].Pitch,
        vData[I].EMSquare);
      if vFontItem.FFileName <> AFileName then
      begin
        if vFontItem.FFileName <> '' then
        begin
          if not TsgListAccess(FFontAliases).FindBase(@vFontItem, J) then
          begin
            vFontAlias := TsgFontItem.Create(vFontItem.Family, fsBold in vData[I].Style, fsItalic in vData[I].Style, vData[I].EMSquare);
            vFontAlias.FFileName := AFileName;
            vFontAlias.FHash := vFontItem.FHash;
            FFontAliases.Add(vFontAlias);
            FFontFiles.Add(vFontAlias);
          end;
        end
        else
        begin
          vFontItem.FFileName := AFileName;
          FFontFiles.Add(vFontItem);
        end;
      end;
    end;
    Result := True;
  finally
    SetCurrentDir(vPath);
  end;
end;

{$IFDEF SG_WIN_PLATFORM}
function TsgTableFonts.OpenRegistryKeyFonts(const ARegistry: TRegistry; ACurrentUser: Boolean): Boolean;
begin
  Result := False;
{$IFDEF SGDEL_5}
  ARegistry.Access := KEY_READ;
{$ENDIF}
  if ACurrentUser then
    ARegistry.RootKey := HKEY_CURRENT_USER
  else
    ARegistry.RootKey := cnstRegKeyHKLM;
  if ARegistry.OpenKey(cnstRegKeySoftware, False) then
    if ARegistry.OpenKey(cnstRegKeyMicrosoft, False) then
      if ARegistry.OpenKey(RegKeyMicrosoftWindows, False) then
        if ARegistry.OpenKey(cnstRegKeyCurrentVersion, False) then
          Result := ARegistry.OpenKey(cnstRegKeyFonts, False);
end;
{$ENDIF}

procedure TsgTableFonts.Initialize;
{$IFDEF SG_FIREMONKEY}
{$IFNDEF SG_FM_WINDOWS}
var
  I: Integer;
  vFontItem: TsgFontItem;
{$ENDIF}
{$ENDIF}
begin
  InitFontDataItems(Self);
  if Assigned(FFontFiles) then
    Exit;
  FFontFiles := TsgObjectList.Create;
{$IFNDEF SG_NON_WIN_PLATFORM}
  InitializeFromRegister;
{$ELSE}
  {$IFDEF SG_FM_WINDOWS}
  InitializeFromRegister;
  {$ELSE}
    {$IFDEF SG_FIREMONKEY}
    FFontFiles.Duplicates := dupAccept;
    FFontFiles.ProcCompare := CompareFileName;
    for I := 0 to FFontItems.Count - 1 do
    begin
      vFontItem := TsgFontItem(FFontItems[I]);
      vFontItem.FFileName := GetFontIdByNameAndStyle(vFontItem.Name, vFontItem.Style);
      FFontFiles.Add(vFontItem);
    end;
    FFontFiles.Sorted := True;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

procedure TsgTableFonts.InitializeFromRegister;
{$IFDEF SG_WIN_PLATFORM}
var
  I, vNewCapacity: Integer;
  vReg:  TRegistry;
  vRegInfo: TRegDataInfo;
  vFileName: string;
  vFontNames: TStringList;
  vCurrUser: Boolean;
begin
  vReg := TRegistry.Create;
  try
    for vCurrUser := False to True do
      if OpenRegistryKeyFonts(vReg, vCurrUser) then
      begin
        vFontNames := TStringList.Create;
        try
          vReg.GetValueNames(vFontNames);
          FFontFiles.Duplicates := dupAccept;
          vNewCapacity := FFontFiles.Count + vFontNames.Count;
          if vNewCapacity > FFontFiles.Capacity then
            FFontFiles.Capacity := vNewCapacity;
          FFontFiles.ProcCompare := {$IFDEF FPC_OBJFPC}@{$ENDIF}CompareFileName;
          I := 0;
          while I < vFontNames.Count do
          try
            try
              if vReg.GetDataInfo(vFontNames[I], vRegInfo) and (vRegInfo.RegData = rdString) then
              begin
                vFileName := vReg.ReadString(vFontNames[I]);
                AddFontFilesInfoEx(ExtractFileName(vFileName), ExtractFilePath(vFileName));
              end;
            finally
              Inc(I);
            end;
          except
          end;
        finally
          vFontNames.Free;
        end;
      end;
    FFontFiles.Sorted := True;
  finally
    vReg.CloseKey;
    vReg.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

function TsgTableFonts.UpdateFontDataItems(const AName: string; ABold, AItalic: Boolean; APitch: Byte; AEMSquare: Word): TsgFontItem;
var
  K: Integer;
  FamilyItem: TsgFamilyItem;
  FontStyles: TFontStyles;
begin
{$IFDEF SGFPC}
  LoadFontCollection;
{$ENDIF}
  K := FFamilies.AddObject(AName, nil);
  if FFamilies.Objects[K] = nil then
    FFamilies.Objects[K] := TsgFamilyItem.Create(AName, APitch);
  FamilyItem := TsgFamilyItem(FFamilies.Objects[K]);
  if not Assigned(FFontItemCache) then FFontItemCache := TsgFontItem.Create;
  FontStyles := [];
  if ABold then Include(FontStyles, fsBold);
  if AItalic then Include(FontStyles, fsItalic);
  TsgFontItem(FFontItemCache).FHash := _HashName(GetFontIdByNameAndStyle(AName, FontStyles));
  if not TsgListAccess(FFontItems).FindBase(@FFontItemCache, K) then
  begin
    Result := TsgFontItem.Create(FamilyItem, ABold, AItalic, AEMSquare);
    Result.FHash := TsgFontItem(FFontItemCache).FHash;
    TsgListAccess(FFontItems).InsertBase(K, @Result);
    TsgListAccess(FFontItems).Flags := TsgListAccess(FFontItems).Flags or cnstSortedBit;
  end
  else
    Result := TsgFontItem(FFontItems[K]);
end;

function TsgTableFonts.CompareFileName(const A, B: Pointer): Integer;
begin
  Result := AnsiCompareText(TsgFontItem(A^).FFileName, TsgFontItem(B^).FFileName);
end;

function TsgTableFonts.CompareHash(const A, B: Pointer): Integer;
begin
  Result := 0;
  if TsgFontItem(A^).FHash > TsgFontItem(B^).FHash then
    Inc(Result)
  else
    if TsgFontItem(A^).FHash < TsgFontItem(B^).FHash then
      Dec(Result);
end;

constructor TsgTableFonts.Create;
begin
  Initialize;
end;

destructor TsgTableFonts.Destroy;
var
  I: Integer;
begin
  if Assigned(FFontItems) then
    TsgObjectList.ClearList(FFontItems);
  if Assigned(FFontAliases) then
    TsgObjectList.ClearList(FFontAliases);
{$IFNDEF SGDEL_2009}
  ClearStringList(FFamilies);
{$ENDIF}
  FFamilies.Free;
  FFontItems.Free;
  FFontAliases.Free;
  for I := Low(FCharsets) to High(FCharsets) do
    FCharsets[I].Free;
  FFontFiles.Free;
  FFontItemCache.Free;
  inherited Destroy;
end;

function TsgTableFonts.GetFamily(AName: string): TsgFamilyItem;
var
  I: Integer;
begin
  if FFamilies.Find(AName, I) then
    Result := TsgFamilyItem(FFamilies.Objects[I])
  else
    Result := nil;
end;

function TsgTableFonts.GetFileName(const AFontName: string): string;
begin
  Result := FileName[AFontName, []];
  //FontItem := FamilyItem.GetFont(['Regular', 'Normal', 'Roman', 'Plain', 'Book', 'ExtraLight']);
end;

function TsgTableFonts.GetFileNameProp(const AFontName: string;
  AStyle: TFontStyles): string;
var
  I: Integer;
begin
  Result := '';
  if Find(GetFontIdByNameAndStyle(AFontName, AStyle), I) then
    Result := TsgFontItem(FFontItems[I]).FFileName;
end;

function TsgTableFonts.GetFont(const AFileName: string; var AFontName: string;
  var AStyle: TFontStyles): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not Assigned(FFontItemCache) then
    FFontItemCache := TsgFontItem.Create;
  TsgFontItem(FFontItemCache).FFileName := AFileName;
  if TsgListAccess(FFontFiles).FindBase(@FFontItemCache, I) then
  begin
    AFontName := TsgFontItem(FFontFiles[I]).Name;
    AStyle := TsgFontItem(FFontFiles[I]).Style;
    Result := True;
  end;
end;

function TsgTableFonts.GetFontName(const AFileName: string): string;
var
{$IFDEF SGFPC}
  FontItem: TCustomFontCollectionItem;
{$ELSE}
  vStyle: TFontStyles;
{$ENDIF}
begin
{$IFNDEF SGFPC}
  Result := '';
  GetFont(AFileName, Result, vStyle);
{$ELSE}
  LoadFontCollection;
  FontItem := FontCollection.FontFile[AFileName];
  if Assigned(FontItem) then
    Result := FontItem.Information[ftiFullName];
{$ENDIF}
end;

function TsgTableFonts.GetFontType(const AFontName: string): TsgFontType;
begin
  Result := GetFontTypeByFileNameExt(GetFileName(AFontName));
end;

function TsgTableFonts.Find(AHash: THandle; var Index: Integer): Boolean;
begin
  if not Assigned(FFontItemCache) then FFontItemCache := TsgFontItem.Create;
  TsgFontItem(FFontItemCache).FHash := AHash;
  Result := TsgListAccess(FFontItems).FindBase(@FFontItemCache, Index);
end;

function TsgTableFonts.Find(const AKey: string; var Index: Integer): Boolean;
begin
  Result := Find(_HashName(AKey), Index);
end;

function TsgTableFonts.FindForPDFName(const AFontName: string): string;
var
  I: Integer;
  vFont: string;
  vName: string;
{$IFNDEF SGFPC}
  vEnum: TStringsEnumerator;
{$ELSE}
  vEnum: IFreeTypeFamilyEnumerator;
{$ENDIF}

  function MatchName(AName,AFont: string): Boolean;
  var
    j,k: Integer;
  begin
    Result := True;
    j := 1;
    k := 1;

    while k <= Length(AFont) do
    begin
      if j > Length(AName) then
      begin
        Result := False;
        Break;
      end
      else
      begin
        if AFont[k] = cnstSpace then
          inc(k)
        else
        begin
          if AName[j] = cnstSpace then
            inc(j)
          else
          begin
            if AFont[k] <> AName[j] then
            begin
              Result := False;
              Break;
            end;
            inc(k);
            inc(j);
          end;
        end;
      end;
    end;
  end;

begin
  I := Pos(cnstPlus, AFontName);
  if I > 0 then
    vName := Copy(AFontName, I + 1, Length(AFontName) - I)
  else
    vName := AFontName;
  Result := '';
{$IFNDEF SGFPC}
  vEnum := {$IFNDEF SGDEL_2005}TStringsEnumerator.Create(FFamilies){$ELSE}FFamilies.GetEnumerator{$ENDIF};
{$ELSE}
  vEnum := FontCollection.FamilyEnumerator;
{$ENDIF}
  try
    while vEnum.MoveNext do
    begin
      vFont := GetFontNameWithoutStyle(vEnum.Current{$IFDEF SGFPC}.FamilyName{$ENDIF});
      if MatchName(vName, vFont) then
      begin
        if vFont = vName then
        begin
          Result := vFont;
          Exit;
        end
        else
          if Length(Result) < Length(vFont) then
            Result := vFont;
      end;
    end;
  finally
{$IFNDEF SGFPC}
    vEnum.Free;
{$ELSE}
    vEnum := nil;
{$ENDIF}
  end;
end;

{TsgTextGlyph}

function TsgTextGlyph.GetGlyph(AKey: Word): TsgGlyph;
begin
  LockList;
  try
    Result := InternalGetGlyph(AKey);
  finally
    UnlockList;
  end;
end;

function TsgTextGlyph.AddGlyph(AKey: Word): TsgGlyph;
begin
  LockList;
  try
    Result := InternalGetGlyph(AKey);
  finally
    UnlockList;
  end;
end;

procedure TsgTextGlyph.UnlockList;
begin
{$IFDEF SG_OPENING_IN_THEADS}
  if Assigned(FOwnerLock) then FOwnerLock.EndWrite;// FOwnerLock.Leave;
{$ENDIF}
end;

{$IFDEF SG_WINAPI}
procedure TsgTextGlyph.LoadGlyph(AGlyph: TsgGlyph);
var
  vGlyphBuilder: TsgGlyphBuilder;
  vKey: Word;
  vTransformation: TTransformation;
  vCellInc: TPoint;
  vSize: Integer;
  vBuf: TBytes;
  vIndex: Word;
begin
  vKey := AGlyph.Key;
  Finalize(AGlyph.FPolygons);
  LockList;
  try
    vTransformation := TTransformation.Create(FMatByScale(1.0 / FFont.Height));
    vGlyphBuilder := TsgGlyphBuilder.Create;
    try
      vCellInc := TWinMesureContext(FWinMesureContext).GetGlyphOutline(vKey, nil^);
      case FGlyphQueryMode of
        gqmPath:
          begin
            vTransformation.Scale(cnstFPointSingleYMinus);
            vSize := TWinMesureContext(FWinMesureContext).GetGlyphPath(vKey, vBuf);
            if vSize > 4 then
            begin
              vGlyphBuilder.DecomposePath(vBuf[0], PPoints(@vBuf[0])^[vSize], vSize);
              TWinMesureContext(FWinMesureContext).GetGlyphIndex(vKey, vIndex);
              if vCellInc.X = 0 then
                vTransformation.Offset := AddFPoint(vTransformation.Offset,
                  MakeFPoint(vTransformation.Matrix.EX.X * (PPoints(@vBuf[0])^[2].X - PPoints(@vBuf[0])^[0].X), 0, 0));
              // todo!!! calc offset exactly after DecomposePath
              if vIndex = cnstInvalidGlyphIndex then
                vCellInc.X := PPoints(@vBuf[0])^[2].X - PPoints(@vBuf[0])^[0].X;
              vGlyphBuilder.Extract(vTransformation, AGlyph.FBlackBox, AGlyph.FPolygons);
            end
            else
              AGlyph.FBlackBox.Right := vTransformation.Matrix.EX.X * vCellInc.X;
          end;
        gqmOutline:
          begin
            TWinMesureContext(FWinMesureContext).GetGlyphOutline(vKey, vBuf);
            if Length(vBuf) > 0 then
            begin
              vGlyphBuilder.DecomposeOutline(@vBuf[0], Length(vBuf) * SizeOf(vBuf[0]));
              vGlyphBuilder.Extract(vTransformation, AGlyph.FBlackBox, AGlyph.FPolygons);
            end
            else
              AGlyph.FBlackBox.Right := vTransformation.Matrix.EX.X * vCellInc.X;
            SetLength(vBuf, 0);
          end;
      end; { case FGlyphQueryMode }
      AGlyph.FAdvance := vTransformation.Matrix.EX.X * vCellInc.X;
    finally
      vGlyphBuilder.Free;
      vTransformation.Free;
    end;
  finally
    UnlockList;
  end;
end;
{$ELSE}
{$IFDEF SGFPC}
procedure TsgTextGlyph.LoadGlyph(AGlyph: TsgGlyph);
var
  vGlyphBuilder: TsgGlyphBuilder;
  R: TRect;
  vTransformation: TTransformation;
  vFreeTypeGlyph: TFreeTypeGlyph;
  vOutline: TT_Outline;
  vGlyphMetrics: TT_Glyph_Metrics;
begin
  Finalize(AGlyph.FPolygons);
  LockList;
  try
    vTransformation := TTransformation.Create(FMatByScale(1.0 / FFont.Height));
    vGlyphBuilder := TsgGlyphBuilder.Create;
    try
      if GetFreeTypeGlyph(FontName, FontStyle, AGlyph.FKey, vFreeTypeGlyph) then
      begin
        AGlyph.FAdvance := vTransformation.Matrix.EX.X * vFreeTypeGlyph.Advance;
        if TT_Get_Glyph_Outline(vFreeTypeGlyph.Data, vOutline) = TT_Err_Ok then
        begin
          vGlyphBuilder.Decompose(vOutline, 0);
          vGlyphBuilder.Extract(vTransformation, AGlyph.FBlackBox, AGlyph.FPolygons);
          if IsBadTTFRect(AGlyph.FBlackBox) then
            if (TT_Get_Glyph_Metrics(vFreeTypeGlyph.Data, vGlyphMetrics) = TT_Err_Ok) and (vGlyphMetrics.bbox.xMin < vGlyphMetrics.bbox.xMax) then
            begin
              AGlyph.FBlackBox.Left := vTransformation.Matrix.EX.X * vGlyphMetrics.bbox.xMin / 64;
              AGlyph.FBlackBox.Right := vTransformation.Matrix.EX.X * vGlyphMetrics.bbox.xMax / 64;
              AGlyph.FBlackBox.Top := vTransformation.Matrix.EY.Y * vGlyphMetrics.bbox.yMin / 64;
              AGlyph.FBlackBox.Bottom := vTransformation.Matrix.EY.Y * vGlyphMetrics.bbox.yMax / 64;
            end;
        end;
      end;
      if IsBadTTFRect(AGlyph.FBlackBox) then
      begin
        AGlyph.FBlackBox.Top := 0;
        AGlyph.FBlackBox.Bottom := 0;
        AGlyph.FBlackBox.Left := 0;
        AGlyph.FBlackBox.Right := AGlyph.FAdvance;
      end;
    finally
      vGlyphBuilder.Free;
      vTransformation.Free;
    end;
  finally
    UnlockList;
  end;
end;
{$ELSE}
{$IFDEF SG_FIREMONKEY}
procedure TsgTextGlyph.LoadGlyph(AGlyph: TsgGlyph);
var
  vGlyphBuilder: TsgGlyphBuilder;
  vKey: Word;
  vTransformation: TTransformation;
  vFontGlyph: TFontGlyph;
  vBuffer: TPolygon;
  vFontGlyphSettings: TFontGlyphSettings;
begin
  vKey := AGlyph.Key;
  Finalize(AGlyph.FPolygons);
  LockList;
  try
    vTransformation := TTransformation.Create(FMatByScale({$IFDEF _TTF_USE_NEG_FONT_HEIGHT}-{$ENDIF}1.0 / FFont.Height));
    vGlyphBuilder := TsgGlyphBuilder.Create;
    try
      vFontGlyphSettings := [TFontGlyphSetting.Path];
{$IFDEF SG_FM_WINDOWS}
      // TFontGlyphSetting.Bitmap - need to calculate TFontGlyph.Origin
      if not IsSpace(Char(vKey)) then
        Include(vFontGlyphSettings, TFontGlyphSetting.Bitmap);
{$ENDIF}
      vFontGlyph := TFontGlyphManager.Current.GetGlyph(UCS4Char(vKey), FFont, 1, vFontGlyphSettings);
      vFontGlyph.Path.Scale(1, -1);
  {$IFDEF SG_FM_WINDOWS}
      vFontGlyph.Path.Translate(0, TFontGlyphManager.Current.GetBaseline(FFont, 1) * FFont.PixelsPerInch/72.0);
  {$ENDIF}
  {$IFDEF SG_FM_ANDROID}
      vFontGlyph.Path.Translate(0, TFontGlyphManager.Current.GetBaseline(FFont, 1));
  {$ENDIF}
  {$IFDEF SG_FM_LINUX}
      vFontGlyph.Path.Translate(0, TFontGlyphManager.Current.GetBaseline(FFont, 1));
  {$ENDIF}
      AGlyph.FAdvance := {$IFDEF _TTF_USE_NEG_FONT_HEIGHT}-{$ENDIF}vFontGlyph.Advance/FFont.Height;
      if FGlyphQueryMode = gqmPath then
        vGlyphBuilder.Decompose(vFontGlyph.Path)
      else
      begin
        vFontGlyph.Path.FlattenToPolygon(vBuffer, 1);
        vGlyphBuilder.Decompose(vBuffer, 0);
      end;
      vGlyphBuilder.Extract(vTransformation, AGlyph.FBlackBox, AGlyph.FPolygons);
      if IsBadTTFRect(AGlyph.FBlackBox) then
      begin
        AGlyph.FBlackBox.Top := 0;
        AGlyph.FBlackBox.Bottom := 0;
        AGlyph.FBlackBox.Left := 0;
        AGlyph.FBlackBox.Right := AGlyph.FAdvance;
      end;
    finally
      FreeAndNil(vFontGlyph);
      vGlyphBuilder.Free;
      vTransformation.Free;
    end;
  finally
    UnlockList;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}//fmx
{$ENDIF}//fpc
{$ENDIF}//winapi

procedure TsgTextGlyph.LockList;
begin
{$IFDEF SG_OPENING_IN_THEADS}
  if Assigned(FOwnerLock) then FOwnerLock.BeginWrite;// FOwnerLock.Enter;
{$ENDIF}
end;

function TsgTextGlyph.GetFileName: string;
begin
  Result := TsgFontItem(FFontItem).FileName;
end;

function TsgTextGlyph.GetFontName: string;
begin
  Result := TsgFontItem(FFontItem).Name;
end;

function TsgTextGlyph.GetFontParams(const AMatrix: TFMatrix): TsgFontParams;
begin
  Result := cnstFontParamsZero;
  Result.Above := DistanceFVector(AffineTransformPoint(MakeFPoint(0, FAbove, 0), AMatrix));
  Result.Below := DistanceFVector(AffineTransformPoint(MakeFPoint(0, FBelow, 0), AMatrix));
  Result.Height := DistanceFVector(AffineTransformPoint(MakeFPoint(0, 1, 0), AMatrix));
end;

function TsgTextGlyph.GetFontStyle: TFontStyles;
begin
  Result := TsgFontItem(FFontItem).Style;
end;

function TsgTextGlyph.InternalGetGlyph(AKey: Integer): TsgGlyph;
var
  I: Integer;
begin
  Result := nil;
  if Find(AKey, I) then
    Result := FGlyphs[I];
  if not Assigned(Result) then
  begin
    Result := TsgGlyph.Create;
    Result.FKey := AKey;
    if I < cnstGlyphsCount then
      FGlyphs[I] := Result
    else
      InsertGlyph(I, Result);
    LoadGlyph(Result);
  end;
end;

function TsgTextGlyph.GetAbovePosition: Single;
begin
  Result := GetGlyph(cnstHiChar).FBlackBox.Top - GetGlyph(cnstLowChar).FBlackBox.Bottom;
end;

function TsgTextGlyph.GetStrikeoutPosition: Single;
begin
  Result := (GetGlyph(cnstHiChar).FBlackBox.Top + GetGlyph(cnstLowChar).FBlackBox.Bottom) * 0.5;
end;

function TsgTextGlyph.GetUnderlinePosition: Single;
begin
  Result := GetGlyph(cnstLowChar).FBlackBox.Bottom;
end;

constructor TsgTextGlyph.Create(const AFontName: string;
  const AFontStyle: TFontStyles; ACharSet: TFontCharset);
begin
  FGlyphQueryMode := gqmPath;
  SetLength(FGlyphs, cnstGlyphsCount);
  FGlyphsCount := cnstGlyphsCount;
  FFontItem := TableFonts.UpdateFontDataItems(AFontName, fsBold in AFontStyle,
    fsItalic in AFontStyle, DEFAULT_PITCH, 0);
  if ACharSet = DEFAULT_CHARSET then
    FCharset := TsgFontItem(FFontItem).Family.Charset
  else
    FCharset := ACharset;
  FFont := TsgFontItem(FFontItem).CreateFont;
  FFont.Charset := FCharset;
  FFont.Height := {$IFDEF _TTF_USE_NEG_FONT_HEIGHT}-{$ENDIF}cnstHeigthDef;
{$IFDEF SG_WINAPI}
  FWinMesureContext := TWinMesureContext.Create(FFont);
  FFont.Height := TWinMesureContext(FWinMesureContext).EMSquare;
  if FFont.Height = 0 then
    FFont.Height := cnstHeigthDef;
{$ENDIF}
  InitKoef;
end;

destructor TsgTextGlyph.Destroy;
begin
  LockList;
  try
    while FGlyphsCount > 0 do
    begin
      FreeAndNil(FGlyphs[FGlyphsCount - 1]);
      Dec(FGlyphsCount);
    end;
    Finalize(FGlyphs);
  finally
    UnlockList;
  end;
{$IFDEF SG_WINAPI}
  FWinMesureContext.Free;
{$ENDIF}
  FFont.Free;
  inherited Destroy;
end;

function TsgTextGlyph.AddGlyph(AKey: sgUnicodeChar): TsgGlyph;
begin
  LockList;
  try
    Result := InternalGetGlyph(Ord(AKey));
  finally
    UnlockList;
  end;
end;

procedure TsgTextGlyph.ClearPointsInGplyphs;
var
  I: Integer;
begin
  LockList;
  try
    for I := FGlyphsCount - 1 downto 0 do
      if FGlyphs[I] <> nil then
        FGlyphs[I].ClearPoints;
  finally
    UnlockList;
  end;
end;

function TsgTextGlyph.DoEnum(ATextItems: TStringBuilder; ATracking: Single;
  const ATransformation: TObject; const AStyleMode: Boolean;
  AGlyphParamMethodOffset: Pointer; AParam: THandle): TFPoint;
var
  I: Integer;
  vTransformation: TCustomTransformation;
  vMethod: TMethod;
  GlyphParamInvoke: TGlyphParamMethod absolute vMethod;
  vSpacing, vShift: Single;
begin
  vTransformation := TCustomTransformation(ATransformation);
  Result := cnstFPointZero;
  if ATextItems.Length > 0 then
  begin
    vMethod.Code := AGlyphParamMethodOffset;
    if ATracking < 0.75 then
      ATracking := 0.75
    else
      if ATracking > 4 then
        ATracking := 4;
    vSpacing := FAveCharWidth * (ATracking - 1);
    Result.X := 0;
    I := 0;
    while I < ATextItems.Length do
    begin
      vMethod.Data := GetGlyph(Ord(ATextItems[I]));
      if vMethod.Data <> nil then
      begin
        if not AStyleMode then
          GlyphParamInvoke(vTransformation, AParam);
        vShift := TsgGlyph(vMethod.Data).Advance + vSpacing;
      end
      else
        vShift := vSpacing{+ FAveCharWidth};
      Result.X := Result.X + vShift;
      vTransformation.Translate(MakeFPoint(vShift, 0, 0));
      Inc(I);
    end;
  end;
end;

procedure TsgTextGlyph.DrawText(AContext: TsgProxyBase;
  ATextItems: TStringBuilder; AUnderline, AStrikeOut: Boolean; const AMatrix: TFMatrix;
  const ATracking: Single; const ATextMode: Byte);
var
  vP1, vP2: TFPoint;
  vTransformation: TCustomTransformation;
  vOffset: TFPoint;
  vGlyphParam: TsgGlyphDrawParam;
begin
  AContext.DoSetPolyFillMode(WINDING); // erroneously changed in revision #34106
  vTransformation := TTransformation.Create(AMatrix);
  try
    vGlyphParam.Context := AContext;
    vGlyphParam.TextMode := ATextMode;
    vOffset := DoEnum(ATextItems, ATracking, vTransformation, False,
      _MkGlyphMethodOffs({$IFDEF FPC_OBJFPC}@{$ENDIF}TsgGlyph(nil).DoDraw),
      THandle(@vGlyphParam));
    if AUnderline then
    begin
      vP1 := MakeFPoint(-vOffset.X, GetUnderlinePosition, 0);
      vP2 := MakeFPoint(0, vP1.Y, 0);
      DoDrawLine(AContext.DoPolyline, vTransformation, vP1, vP2);
    end;
    if AStrikeOut then
    begin
      vP1 := MakeFPoint(-vOffset.X, GetStrikeoutPosition, 0);
      vP2 := MakeFPoint(0, vP1.Y, 0);
      DoDrawLine(AContext.DoPolyline, vTransformation, vP1, vP2);
    end;
  finally
    vTransformation.Free;
  end;
end;

procedure TsgTextGlyph.DoClear;
begin
  LockList;
  try
    while FGlyphsCount > 0 do
    try
      FreeAndNil(FGlyphs[FGlyphsCount - 1]);
    finally
      Dec(FGlyphsCount);
    end;
    SetLength(FGlyphs, cnstGlyphsCount);
    FGlyphsCount := cnstGlyphsCount;
    FillChar(FGlyphs[0], FGlyphsCount * SizeOf(Pointer), 0);
  finally
    UnlockList;
  end;
end;

function TsgTextGlyph.Find(AKey: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if AKey < cnstGlyphsCount then
  begin
    Result := True;
    Index := AKey;
  end
  else
  begin
    Result := False;
    LockList;
    try
      L := cnstGlyphsCount;
      H := FGlyphsCount - 1;
      while L <= H do
      begin
        I := (L + H) shr 1;
        C := FGlyphs[I].Key - AKey;
        if C < 0 then
          L := I + 1
        else
        begin
          H := I - 1;
          if C = 0 then
          begin
            Result := True;
            L := I;
          end;
        end;
      end;
      Index := L;
    finally
      UnlockList;
    end;
  end;
end;

procedure TsgTextGlyph.InitKoef;
{$IFDEF SG_FIREMONKEY}
var
{$IFDEF SG_WINAPI}
  vOrigin: TPoint;
{$ELSE}
  FontGlyph: TFontGlyph;
  S: UCS4String;
  vBaseLine: Single;
{$ENDIF}
begin
  FHKoef := 1.0;
{$IFDEF SG_WINAPI}
  FAbove := Abs(TWinMesureContext(FWinMesureContext).Above / FFont.Height);
  FBelow := Abs(TWinMesureContext(FWinMesureContext).Below / FFont.Height);
  FAveCharWidth := TWinMesureContext(FWinMesureContext).AveCharWidth / FFont.Height;
  TWinMesureContext(FWinMesureContext).GetGlyphOutline(Ord('A'), nil^, @vOrigin);
  if vOrigin.Y <> 0 then
    FHKoef := FFont.Height / vOrigin.Y;
  TWinMesureContext(FWinMesureContext).ForgetOTM;
{$ELSE}
  S := UnicodeStringToUCS4String('x');
  FontGlyph := TFontGlyphManager.Current.GetGlyph(S[0], FFont, 1, [TFontGlyphSetting.Path]);
  try
    FAveCharWidth := FontGlyph.Path.GetBounds.Width;
    vBaseLine := TFontGlyphManager.Current.GetBaseline(FFont, 1);
    FAbove := vBaseLine / cnstHeigthDef;
    FBelow := (FontGlyph.VerticalAdvance - vBaseLine) / cnstHeigthDef;
  finally
    FreeAndNil(FontGlyph);
  end;
  S := UnicodeStringToUCS4String('A');
  FontGlyph := TFontGlyphManager.Current.GetGlyph(S[0], FFont, 1, [TFontGlyphSetting.Path]);
  try
    FHKoef := cnstHeigthDef / FontGlyph.Path.GetBounds.Height;
  finally
    FreeAndNil(FontGlyph);
  end;
{$ENDIF}
end;
{$ELSE}{$IFDEF SGFPC}
var
  FreeTypeFont: TFreeTypeFontAccess;
  FaceProps: TT_Face_Properties;
  GlyphIndex: Integer;
  FreeTypeGlyph: TFreeTypeGlyph;
  R: TRect;
begin
  FHKoef := 1.0;
  FreeTypeFont := TFreeTypeFontAccess(GetFreeTypeFont(FontName, FontStyle));
  if not Assigned(FreeTypeFont) then
  begin
    FreeTypeFont := TFreeTypeFontAccess(GetFreeTypeFont(sDefaultTTFFont, []));
    if FreeTypeFont = nil then
    begin
      FAbove := 1;
      FBelow := 0;
      FAveCharWidth := 0.7;
      Exit;
    end;
  end;
  if FreeTypeFont.DPI = 0 then
    FreeTypeFont.DPI := 72;
  FreeTypeFont.SizeInPoints := cnstHeigthDef;
  TT_Get_Face_Properties(FreeTypeFont.FFace, FaceProps);
  if Assigned(FaceProps.os2) and (FaceProps.os2^.version >= 3) then
    FAveCharWidth := FaceProps.os2^.xAvgCharWidth / FaceProps.header^.units_per_EM
  else
    FAveCharWidth := FreeTypeFont.CharWidthFromUnicode(Ord('x')) / cnstHeigthDef;
  FAbove := Abs(FreeTypeFont.Ascent / cnstHeigthDef);
  FBelow := Abs(FreeTypeFont.Descent / cnstHeigthDef);
  GlyphIndex := FreeTypeFont.CharIndex[Ord('A')];
  if (GlyphIndex >= 0) and (GlyphIndex < FreeTypeFont.GlyphCount) then
  begin
     FreeTypeGlyph := FreeTypeFont.Glyph[GlyphIndex];
     if Assigned(FreeTypeGlyph) then
     begin
       R := FreeTypeGlyph.Bounds;
       if R.Bottom <> R.Top then
          FHKoef := cnstHeigthDef / (R.Bottom - R.Top);//for notwin todo: need analog GlyphMetrics.gmptGlyphOrigin.Y !!
     end;
  end;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
(*var
  vGlyphMetrics: TGlyphMetrics;
  DC: HDC;
  vSize: TSize;
  vOTM: Windows.TOutlineTextmetric;
begin
  FHKoef := 1.0;
  DC := CreateStockDC;
  try
    SelectObject(DC, FFont.Handle);
    if GetOutlineTextMetrics(DC, SizeOf(vOTM), @vOTM) = 0 then // raster font...
    begin
      Windows.GetTextMetrics(DC, vOTM.otmTextMetrics);
      GetTextExtentPoint32(DC, 'x', 1, vSize);
      FAveCharWidth := vSize.cx{vOTM.otmTextMetrics.tmAveCharWidth} / vOTM.otmTextMetrics.tmHeight;
      FAbove := Abs(vOTM.otmTextMetrics.tmAscent / vOTM.otmTextMetrics.tmHeight);
      FBelow := Abs(vOTM.otmTextMetrics.tmDescent / vOTM.otmTextMetrics.tmHeight);
    end
    else
    begin
      FAveCharWidth := vOTM.otmTextMetrics.tmAveCharWidth / FFont.Height;
      FAbove := Abs(vOTM.otmAscent / FFont.Height);
      FBelow := Abs(vOTM.otmDescent / FFont.Height);
    end;
    GetGlyphIndicesW(DC, '?', 1, @FDefaultIndex, GGI_MARK_NONEXISTING_GLYPHS);
    if FDefaultIndex = cnstInvalidGlyphIndex then
    begin
      GetGlyphIndicesW(DC, @vOTM.otmTextMetrics.tmFirstChar, 1, @FDefaultIndex, GGI_MARK_NONEXISTING_GLYPHS);
      if FDefaultIndex = cnstInvalidGlyphIndex then
        FDefaultIndex := 0;
    end;
    FillChar(vGlyphMetrics, SizeOf(vGlyphMetrics), 0);
    GetGlyphOutline(DC, Ord('A'), GGO_METRICS, vGlyphMetrics, 0, nil,
      cnstGlyphOutlineMatrix);
    if vGlyphMetrics.gmptGlyphOrigin.Y <> 0 then
      FHKoef := FFont.Height / vGlyphMetrics.gmptGlyphOrigin.Y;
  finally
    ReleaseStockDC(DC);
  end;
end;*)
var
  vWinMesureContext: TWinMesureContext;
  vOrigin: TPoint;
begin
  FHKoef := 1.0;
  vWinMesureContext := TWinMesureContext(FWinMesureContext);
  FAveCharWidth := vWinMesureContext.AveCharWidth / FFont.Height;
  FAbove := Abs(vWinMesureContext.Above / FFont.Height);
  FBelow := Abs(vWinMesureContext.Below / FFont.Height);
  vWinMesureContext.GetGlyphIndex(Ord('?'), FDefaultIndex);
  if FDefaultIndex = cnstInvalidGlyphIndex then
  begin
    vWinMesureContext.GetGlyphIndex(Ord(vWinMesureContext.OTM^.otmTextMetrics.tmFirstChar), FDefaultIndex);
    if FDefaultIndex = cnstInvalidGlyphIndex then
      FDefaultIndex := 0;
  end;
  vWinMesureContext.GetGlyphOutline(Ord('A'), nil^, @vOrigin);
  if vOrigin.Y <> 0 then
    FHKoef := FFont.Height / vOrigin.Y;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TsgTextGlyph.InsertGlyph(Index: Integer; AGlyph: TsgGlyph);
begin
  if FGlyphsCount = Length(FGlyphs) then
    SetLength(FGlyphs, ListGrow(Length(FGlyphs)));
  if Index < FGlyphsCount then
    System.Move(FGlyphs[Index], FGlyphs[Index + 1], (FGlyphsCount - Index) * SizeOf(Pointer));
  Pointer(FGlyphs[Index]) := nil;
  FGlyphs[Index] := AGlyph;
  Inc(FGlyphsCount);
end;

function TsgTextGlyph.InternalLoad(ATextItems: TStringBuilder): TsgGlyph;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(ATextItems) and (ATextItems.Length > 0) then
  begin
    Result := AddGlyph(Ord(ATextItems[0]));
    for I := 1 to ATextItems.Length - 1 do
      AddGlyph(Ord(ATextItems[I]));
  end;
end;
function TsgTextGlyph.IsDefault: Boolean;
begin
  Result := ContainerOfTextGlyphs.IsDefault(Self);
end;

//delete in future version
function TsgTextGlyph.GetBox(const AText: string; const AMatrix: TFMatrix;
  const APFontParams: PsgFontParams; const ATracking: Single): TFRect;
begin
{$IFDEF UNICODE}
  Result := GetBoxW('', WideString(AText), AMatrix, APFontParams, ATracking);
{$ELSE}
  Result := GetBoxW(AText, '', AMatrix, APFontParams, ATracking);
{$ENDIF}
end;

function TsgTextGlyph.GetBoxW(const AText: string; const ATextW: WideString;
  const AMatrix: TFMatrix; const APFontParams: PsgFontParams; const ATracking: Single): TFRect;
var
  vTextItems: TStringBuilder;
begin
  vTextItems := CreateTextItems(AText, ATextW);
  try
    Result := GetTextBox(vTextItems, ATracking);
    TransRectCorners(Result, AMatrix);
    if APFontParams <> nil then
      if vTextItems.Length > 0 then
        APFontParams^ := GetFontParams(AMatrix)
      else
        APFontParams^ := cnstFontParamsZero;
  finally
    vTextItems.Free;
  end;
end;

function TsgTextGlyph.GetGlyphParams(const AChar: Char; const AFontStyles: {$IFDEF SGFPC}Integer{$ELSE}Byte{$ENDIF};
  const AMatrix: TFMatrix; const AList: TList; APBox: PFRect; ATracking: Single): Boolean;
var
  S: string;
  vOffset: TFPoint;
  vP1, vP2: TFPoint;
  vTextItems: TStringBuilder;
  vTransformation: TCustomTransformation;
  vGlyphParam: TsgGlyphDrawParam;
begin
  Result := GetGlyph(Integer(AChar)) <> nil;
  if Result then
  begin
    S := AChar;
    vTextItems := CreateTextItems(S, '');
    try
      vTransformation := TTransformation.Create(AMatrix);
      try
        vGlyphParam.Collection := AList;
        vGlyphParam.Epsilon := nil;
        vOffset := DoEnum(vTextItems, ATracking, vTransformation, False,
          _MkGlyphMethodOffs({$IFDEF FPC_OBJFPC}@{$ENDIF}TsgGlyph(nil).DoGetPolyPointsSingle), THandle(@vGlyphParam));

        if fmUnderline in {$IFDEF NEED_CVT_MVFONTSTYLE}FontStylesToMVFontStyles(TFontStyles(AFontStyles)){$ELSE}TmvFontStyles(AFontStyles){$ENDIF} then
        begin
          vP1 := MakeFPoint(-vOffset.X, GetUnderlinePosition);
          vP2 := MakeFPoint(0, vP1.Y);
          AddLineTransformed(vTransformation, AList, vP1, vP2);
        end;
        if fmStrikeOut in {$IFDEF NEED_CVT_MVFONTSTYLE}FontStylesToMVFontStyles(TFontStyles(AFontStyles)){$ELSE}TmvFontStyles(AFontStyles){$ENDIF} then
        begin
          vP1 := MakeFPoint(-vOffset.X, GetStrikeoutPosition);
          vP2 := MakeFPoint(0, vP1.Y);
          AddLineTransformed(vTransformation, AList, vP1, vP2);
        end;
        if APBox <> nil then
        begin
          APBox^ := GetTextBox(vTextItems, ATracking);
          TransRectCorners(APBox^, AMatrix);
        end;
      finally
        vTransformation.Free;
      end;
    finally
      vTextItems.Free;
    end;
  end;
end;

function TsgTextGlyph.GetTextBox(ATextItems: TStringBuilder; ATracking: Single): TFRect;
var
  I: Integer;
  vGlyph: TsgGlyph;
begin
  Result := cnstFRectZero;
  if ATextItems.Length > 0 then
  begin
    vGlyph := GetGlyph(Ord(ATextItems[0]));
    Result.Top := vGlyph.BlackBox.Top;
    Result.Bottom := vGlyph.BlackBox.Bottom;
    Result.Right := ATextItems.Length * FAveCharWidth * (ATracking - 1) + vGlyph.Advance;
    for I := 1 to ATextItems.Length - 1 do
    begin
      vGlyph := GetGlyph(Ord(ATextItems[I]));
      if Result.Top < vGlyph.BlackBox.Top then
        Result.Top := vGlyph.BlackBox.Top;
      if Result.Bottom > vGlyph.BlackBox.Bottom then
        Result.Bottom := vGlyph.BlackBox.Bottom;
      Result.Right := Result.Right + vGlyph.Advance;
    end;
  end;
end;

function TsgTextGlyph.GetPolyPolyline(const AText: string;
  const ATextW: WideString; const AMatrix: TFMatrix;
  ACollection: TsgCustomPolyItemsCollection; const ATracking: Single = 1.0;
  AEpsilon: PFPoint = nil;
  const AUnderline: Boolean = False; const AStrikeOut: Boolean = False;
  const AStyleMode: Boolean = False): TFPoint;
var
  vTextItems: TStringBuilder;
begin
  vTextItems := CreateTextItems(AText, ATextW);
  try
    Result := GetPolyPolyline(vTextItems, AMatrix, ACollection, ATracking, AEpsilon, AUnderline, AStrikeOut);
  finally
    vTextItems.Free;
  end;
end;

function TsgTextGlyph.LoadedW(const AText: string; const ATextW: WideString = ''): TsgGlyph;
var
  vTextItems: TStringBuilder;
begin
  vTextItems := CreateTextItems(AText, ATextW);
  try
    Result := InternalLoad(vTextItems);
  finally
    vTextItems.Free;
  end;
end;

procedure TsgTextGlyph.DoDrawLine(AProc: TsgPointsProc; ATransformation: TObject; const APoint1, APoint2: TFPoint);
var
  vPts: array[0..1] of TPoint;
begin
  vPts[0] := TCustomTransformation(ATransformation).Transform2D(APoint1.X, APoint1.Y);
  vPts[1] := TCustomTransformation(ATransformation).Transform2D(APoint2.X, APoint2.Y);
  AProc(@vPts[0], Length(vPts));
end;

function TsgTextGlyph.CreateFont(AHeightFactor: Double): TFont;
begin
  Result := TsgFontItem(FFontItem).CreateFont;
  Result.Height := {$IFNDEF SG_FIREMONKEY}Round{$ENDIF}(FHKoef * AHeightFactor);
end;

function TsgTextGlyph.GetIndices(const AText: string; const AWideText: WideString; out Indices{: array of Word}): Integer;
{$IFDEF  SG_WINAPI}
var
  I: Integer;
  vTextItems: TStringBuilder;
begin
  Result := 0;
  vTextItems := CreateTextItems(AText, AWideText);
  try
    SetLength(TGlyphIndices(Indices), vTextItems.Length);
    for I := 0 to vTextItems.Length - 1 do
      TWinMesureContext(FWinMesureContext).GetGlyphIndex(Ord(vTextItems[I]), TGlyphIndices(Indices)[I]);
  finally
    vTextItems.Free;
  end;
end;
{$ELSE}
{$IFDEF SGFPC}
var
  I: Integer;
  vTextItems: TStringBuilder;
  FreeFontType: TFreeTypeFont;
begin
  FreeFontType := GetFreeTypeFont(FFont.Name, FFont.Style);
  vTextItems := CreateTextItems(AText, AWideText);
  try
    SetLength(TGlyphIndices(Indices), vTextItems.Length);
    if FreeFontType = nil then
    begin
      for I := 0 to vTextItems.Length - 1 do
        TGlyphIndices(Indices)[I] := cnstInvalidGlyphIndex;
    end
    else
      for I := 0 to vTextItems.Length - 1 do
        TGlyphIndices(Indices)[I] := FreeFontType.CharIndex[Ord(vTextItems[I])];
  finally
    vTextItems.Free;
  end;
end;
{$ELSE}
var
  I: Integer;
  vTextItems: TStringBuilder;
begin
  Result := 0;
  vTextItems := CreateTextItems(AText, AWideText);
  try
    SetLength(TGlyphIndices(Indices), vTextItems.Length);
    for I := 0 to vTextItems.Length - 1 do
      TGlyphIndices(Indices)[I] := 1;
  finally
    vTextItems.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

function TsgTextGlyph.GetPolyPolyline(ATextItems: TStringBuilder;
  const AMatrix: TFMatrix; ACollection: TsgCustomPolyItemsCollection;
  const ATracking: Single = 1.0; AEpsilon: PFPoint = nil;
  const AUnderline: Boolean = False; const AStrikeOut: Boolean = False;
  const AStyleMode: Boolean = False): TFPoint;
var
  vTransformation: TCustomTransformation;
  vGlyphParam: TsgGlyphDrawParam;

  procedure DoLine(const AX1, AY1, AX2, AY2: Double);
  var
    vP1, vP2: TFPoint;
    vAdd: TAddPt;
  begin
    if Assigned(vTransformation) then
    begin
      vP1 := TCustomTransformation(vTransformation).Transform(AX1, AY1);
      vP2 := TCustomTransformation(vTransformation).Transform(AX2, AY2);
    end
    else
    begin
      vP1 := MakeFPoint(AX1, AY1);
      vP2 := MakeFPoint(AX2, AY2);
    end;
    vAdd := ACollection.NewCollection();
    vAdd(vP1);
    vAdd(vP2);
  end;

begin
  vTransformation := TTransformation.Create(AMatrix);
  try
    if AEpsilon <> nil then
    begin
      vGlyphParam.Epsilon := AEpsilon;
      vGlyphParam.Collection := ACollection;
      Result := DoEnum(ATextItems, ATracking, vTransformation, AStyleMode,
        _MkGlyphMethodOffs({$IFDEF FPC_OBJFPC}@{$ENDIF}TsgGlyph(nil).DoGetPolyPointsCustomEpsilon),
          THandle(@vGlyphParam));
    end
    else
      Result := DoEnum(ATextItems, ATracking, vTransformation, AStyleMode,
        _MkGlyphMethodOffs({$IFDEF FPC_OBJFPC}@{$ENDIF}TsgGlyph(nil).DoGetPolyPointsCustom),
          THandle(ACollection));
    if AUnderline then
      DoLine(-Result.X, GetUnderlinePosition, 0, GetUnderlinePosition);
    if AStrikeOut then
      DoLine(-Result.X, GetStrikeoutPosition, 0, GetStrikeoutPosition);
  finally
    vTransformation.Free;
  end;
end;

{$IFDEF SG_WINAPI}
function TsgTextGlyph.GetIndex(AChar: WideChar): Word;
begin
  TWinMesureContext(FWinMesureContext).GetGlyphIndex(Ord(AChar), Result);
end;
{$ELSE}
{$IFDEF SGFPC}
function TsgTextGlyph.GetIndex(AChar: WideChar): Word;
var
  FreeFontType: TFreeTypeFont;
begin
  FreeFontType := GetFreeTypeFont(FFont.Name, FFont.Style);
  if FreeFontType = nil then
    Result := cnstInvalidGlyphIndex
  else
    Result := FreeFontType.CharIndex[Ord(AChar)];
end;
{$ELSE}
function TsgTextGlyph.GetIndex(AChar: WideChar): Word;
begin
  Result := 1;//
end;
{$ENDIF}
{$ENDIF}

{ TCustomTransformation }

function TCustomTransformation.AffineTransform(const APoint: TFPoint): TFPoint;
begin
  Result := APoint;
end;

constructor TCustomTransformation.Create(const AMatrix: TFMatrix);
begin
  inherited Create;
end;

function TCustomTransformation.GetMartix: TFMatrix;
begin
  Result := cnstIdentityMat;
end;

function TCustomTransformation.GetOffset: TFPoint;
begin
  Result := Matrix.E0;
end;

procedure TCustomTransformation.Scale(const AScale: TFPoint);
begin
end;

procedure TCustomTransformation.SetMatrix(const Value: TFMatrix);
begin
end;

procedure TCustomTransformation.SetOffset(const Value: TFPoint);
begin
end;

procedure TCustomTransformation.Translate(const ADelta: TFPoint);
begin
end;

function TCustomTransformation.Transform(const APoint: TFPoint): TFPoint;
begin
  Result := APoint;
end;

function TCustomTransformation.Transform(const X, Y: Double): TFPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := 0;
end;

function TCustomTransformation.Transform2D(const APoint: TPointF): TPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function TCustomTransformation.Transform(const APoint: TPointF): TFPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := 0;
end;

function TCustomTransformation.Transform2D(const X, Y: Double): TPoint;
begin
  Result.X := sgRound(X);
  Result.Y := sgRound(Y);
end;

function TCustomTransformation.TransformBox(const ABox: TFRect): TFRect;
begin
  Result := ABox;
end;

procedure TCustomTransformation.TransformPolygon(var APolygon: array of TPointF);
var
  I: Integer;
begin
  for I := Low(APolygon) to High(APolygon) do APolygon[I] := Transform2D(APolygon[I]);
end;

procedure TCustomTransformation.TransformPolygons(var APolygons: array of TPolygon);
var
  I: Integer;
begin
  for I := Low(APolygons) to High(APolygons) do TransformPolygon(APolygons[I]);
end;

{ TOffsetTransformation }

constructor TOffsetTransformation.Create(const AMatrix: TFMatrix);
begin
  inherited Create(AMatrix);
  FOffset := AMatrix.E0;
end;

function TOffsetTransformation.GetMartix: TFMatrix;
begin
  Result := StdMat(cnstFPointSingle, FOffset);
end;

function TOffsetTransformation.GetOffset: TFPoint;
begin
  Result := FOffset;
end;

procedure TOffsetTransformation.SetMatrix(const Value: TFMatrix);
begin
  FOffset := Value.E0;
end;

procedure TOffsetTransformation.SetOffset(const Value: TFPoint);
begin
  FOffset := Value;
end;

procedure TOffsetTransformation.Translate(const ADelta: TFPoint);
begin
  FOffset := AddFPoint(FOffset, ADelta);
end;

function TOffsetTransformation.Transform(const APoint: TFPoint): TFPoint;
begin
  Result := AddFPoint(APoint, FOffset);
end;

function TOffsetTransformation.Transform(const X, Y: Double): TFPoint;
begin
  Result.X := X + FOffset.X;
  Result.Y := Y + FOffset.Y;
  Result.Z := FOffset.Z;
end;

function TOffsetTransformation.Transform2D(const APoint: TPointF): TPointF;
begin
  Result.X := APoint.X + FOffset.X;
  Result.Y := APoint.Y + FOffset.Y;
end;

function TOffsetTransformation.Transform(const APoint: TPointF): TFPoint;
begin
  Result.X := APoint.X + FOffset.X;
  Result.Y := APoint.Y + FOffset.Y;
  Result.Z := FOffset.Z;
end;

function TOffsetTransformation.Transform2D(const X, Y: Double): TPoint;
begin
  Result.X := sgRound(X + FOffset.X);
  Result.Y := sgRound(Y + FOffset.Y);
end;

function TOffsetTransformation.TransformBox(const ABox: TFRect): TFRect;
begin
  Result.TopLeft := AddFPoint(ABox.TopLeft, FOffset);
  Result.BottomRight := AddFPoint(ABox.BottomRight, FOffset);
end;

{ TTransformation }

function TTransformation.AffineTransform(const APoint: TFPoint): TFPoint;
begin
  Result := AffineTransformPoint(APoint, FMatrix);
end;

constructor TTransformation.Create(const AMatrix: TFMatrix);
begin
  inherited Create(AMatrix);
  FMatrix := AMatrix;
end;

function TTransformation.GetMartix: TFMatrix;
begin
  Result := FMatrix;
end;

function TTransformation.GetOffset: TFPoint;
begin
  Result := FMatrix.E0;
end;

procedure TTransformation.Scale(const AScale: TFPoint);
begin
  FMatrix.EX := MultiplyFPoint(FMatrix.EX, AScale);
  FMatrix.EY := MultiplyFPoint(FMatrix.EY, AScale);
  FMatrix.EZ := MultiplyFPoint(FMatrix.EZ, AScale);
end;

procedure TTransformation.SetMatrix(const Value: TFMatrix);
begin
  FMatrix := Value;
end;

procedure TTransformation.SetOffset(const Value: TFPoint);
begin
  FMatrix.E0 := Value;
end;

procedure TTransformation.Translate(const ADelta: TFPoint);
begin
  FMatrix.E0 := AddFPoint(FMatrix.E0, AffineTransform(ADelta));
end;

function TTransformation.Transform(const APoint: TFPoint): TFPoint;
begin
  Result := FPointXMat(APoint, FMatrix);
end;

function TTransformation.Transform(const X, Y: Double): TFPoint;
begin
  Result := FPointXMat(MakeFPoint(X, Y), FMatrix);
end;

function TTransformation.Transform2D(const APoint: TPointF): TPointF;
begin
  TTFPointXMat(APoint.X, APoint.Y, FMatrix, Result.X, Result.Y);
end;

function TTransformation.Transform(const APoint: TPointF): TFPoint;
begin
  Result := FPointXMat(MakeFPoint(APoint.X, APoint.Y), FMatrix);
end;

function TTransformation.Transform2D(const X, Y: Double): TPoint;
begin
  Result := RoundPoint(TTFPointXMat(X, Y, FMatrix));
end;

function TTransformation.TransformBox(const ABox: TFRect): TFRect;
begin
  Result := ABox;
  TransRectCorners(Result, FMatrix);
end;

{ TsgFontItem }

function TsgFontItem.BuildKey: string;
begin
  Result := FFamily.BuildKey(Style);
end;

constructor TsgFontItem.Create(const AFamily: TsgFamilyItem;
  ABold, AItalic: Boolean; AEMSquare: Word);
begin
  Create;
  Init(AFamily, ABold, AItalic, AEMSquare);
end;

function TsgFontItem.CreateFont: TFont;
begin
  Result := FFamily.CreateFont(Style);
end;

destructor TsgFontItem.Destroy;
begin
  if Assigned(FFamily) then
  begin
    FFamily.RemoveStyle(FStyle);
    FFamily := nil;
  end;
{$IFDEF SGFPC}
  FreeAndNil(FFreeTypeFont);
  if Assigned(FFontCollectionItem) then
  begin
    FFontCollectionItem.ReleaseFace(FontCollectionItemDestroyListener(Self, FreeTypeFontDestroing));
    FFontCollectionItem := nil;
  end;
{$ENDIF}
  inherited Destroy;
end;

constructor TsgFontItem.Create;
begin
  inherited Create;
end;

procedure TsgFontItem.FillLogFont(var ALogFont: TLogFont);
begin
  FFamily.FillLogFont(ALogFont);
  if fsBold in Style then
    ALogFont.lfWeight := FW_BOLD
  else
    ALogFont.lfWeight := FW_NORMAL;
  ALogFont.lfItalic := Ord(fsItalic in Style);
end;

function TsgFontItem.GetName: string;
begin
  Result := FFamily.FamilyName;
end;

{$IFDEF SGFPC}
procedure TsgFontItem.FreeTypeFontDestroing;
begin
  FFontCollectionItem := nil;
  FFreeTypeFont := nil;
end;

procedure TsgFontItem.SetFontCollectionItem(AValue: TCustomFontCollectionItem);
begin
  if FFontCollectionItem <> AValue then
  begin
    FreeAndNil(FFreeTypeFont);
    if Assigned(FFontCollectionItem) then
      FFontCollectionItem.ReleaseFace(FontCollectionItemDestroyListener(Self, FreeTypeFontDestroing));
    FFontCollectionItem := AValue;
    if Assigned(FFontCollectionItem) then
    begin
      FFreeTypeFont := TFreeTypeFont.Create;
      FFreeTypeFont.SetNameAndStyle(FFontCollectionItem.Family.FamilyName, FFontCollectionItem.Styles);
      FFreeTypeFont.DPI := 72;
      FFreeTypeFont.SizeInPoints := cnstHeigthDef;
      FFreeTypeFont.CheckInstance;
      FFontCollectionItem.QueryFace(FontCollectionItemDestroyListener(Self, FreeTypeFontDestroing));
    end;
  end;
end;
{$ENDIF}

procedure TsgFontItem.Init(const AFamily: TsgFamilyItem;
  ABold, AItalic: Boolean; AEMSquare: Word);
begin
  FFamily := AFamily;
  FStyle := [];
  if ABold then Include(FStyle, fsBold);
  if AItalic then Include(FStyle, fsItalic);
  if Assigned(FFamily) then
    FFamily.AddStyle(FStyle);
  FEMSquare := AEMSquare;
end;

function TsgFontItem.GetFileName: string;
begin
  Result := FFileName;
end;

{ TsgFamilyItem }

constructor TsgFamilyItem.Create;
begin
  FCharset := DEFAULT_CHARSET;
end;

function TsgFamilyItem.AddStyle(AStyle: TFontStyles): Integer;
begin
  if not Find(AStyle, Result) then
  begin
    SetLength(FFonts, Length(FFonts) + 1);
    FFonts[High(FFonts)] := AStyle;
  end;
end;

function TsgFamilyItem.BuildKey(AStyle: TFontStyles): string;
begin
  Result := GetFontIdByNameAndStyle(FamilyName, AStyle);
end;

constructor TsgFamilyItem.Create(const AFamilyName: string; APitch: Byte);
begin
  Create;
  FFamilyName := AFamilyName;
  FPitch := APitch;
end;

function TsgFamilyItem.CreateFont(AStyle: TFontStyles): TFont;
begin
  Result := TFont.Create;
  Result.Name := FamilyName;
  Result.Style := AStyle;
  case Pitch and $F of
    VARIABLE_PITCH: Result.Pitch := fpVariable;
    FIXED_PITCH: Result.Pitch := fpFixed;
  else
    Result.Pitch := fpDefault;
  end;
end;

destructor TsgFamilyItem.Destroy;
begin

  inherited Destroy;
end;

procedure TsgFamilyItem.FillLogFont(var ALogFont: TLogFont);
begin
  StrPLCopy(PChar(@ALogFont.lfFaceName[0]), FamilyName, Length(ALogFont.lfFaceName) - 1);
  ALogFont.lfPitchAndFamily := Pitch;
end;

function TsgFamilyItem.Find(AStyle: TFontStyles; var Index: Integer): Boolean;
var
  vHigh: Integer;
begin
  Result := False;
  Index := 0;
  vHigh := High(FFonts);
  while (Index <= vHigh) and not Result do
    if FFonts[Index] = AStyle then
      Inc(Result)
    else
      Inc(Index);
end;

function TsgFamilyItem.GetFont(Style: TFontStyles): TsgFontItem;
var
  I: Integer;
begin
  Result := nil;
  if TableFonts.Find(BuildKey(Style), I) then
    Result := TsgFontItem(TableFonts.FFontItems[I]);
end;

function TsgFamilyItem.RemoveStyle(AStyle: TFontStyles): Integer;
var
  I: Integer;
begin
  if Find(AStyle, Result) then
  begin
    for I := Result to High(FFonts) - 1 do
      FFonts[I] := FFonts[I + 1];
    SetLength(FFonts, Length(FFonts) - 1);
  end;
end;

{$IFDEF SG_DELPHI_VCL}
{ TFamilyCharsetAdapter }

procedure TFamilyCharsetAdapter.Changed;
begin
  if not Assigned(Family) or (Family.FamilyName <> Font.Name) then
    Family := TableFonts.Family[Font.Name];
end;

constructor TFamilyCharsetAdapter.Create(const AFont: TFont);
begin
  FFont := AFont;
  Family := TableFonts.Family[Font.Name];
end;

function TFamilyCharsetAdapter.GetCharset: TFontCharset;
begin
  Result := FCharset;
end;

procedure TFamilyCharsetAdapter.SetFamily(const Value: TsgFamilyItem);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    if Assigned(FFamily) then
      FCharset := FFamily.Charset
    else
      FCharset := ANSI_CHARSET;
  end;
end;
{$ENDIF}

procedure InitializeCollections;
begin
  if _Initialized then Exit;
  _Initialized := True;
  Generator := nil;
  SetQualityApproximation(cnstDefaultQualityApproximation);
  InitRegKeyMicrosoftWindows;
  InitFontsPath;
{$IFNDEF SG_NON_WIN_PLATFORM}
  Pointer({$IFNDEF FPC_OBJFPC}@{$ENDIF}GetFontResourceInfo) := GetProcAddress(GetModuleHandle(gdi32), 'GetFontResourceInfoW');
{$ENDIF}
  TableFonts := TsgTableFonts.Create;
  ContainerOfTextGlyphs := TsgContainerOfTextGlyphs.Create;
end;

initialization
  InitializeCollections;

finalization
  FreeAndNil(ContainerOfTextGlyphs);
  FreeAndNil(TableFonts);
  FreeAndNil(Generator);

end.

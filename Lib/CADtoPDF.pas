{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                    Export CAD to PDF                       }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoPDF;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF SG_WINDOWS}
   Windows,
{$ENDIF}
{$IFDEF SG_FM_WINDOWS}
  Winapi.Windows, FMX.Helpers.Win, // FMX.FontGlyphs.Win
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage, LazFreeType, EasyLazFreeType, TTTypes, TTObjs, dynlibs,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  System.Generics.Collections,
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, System.Devices,
{$ELSE}
  Graphics,
{$ENDIF}
  SysUtils, Classes, CADExport, CADImage, DXFConv, sgBitmap, sgConsts, sgFunction
  ,sgLists
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  , AnsiStrings
{$ENDIF}
  , sgDrawRect;

{$IFDEF SG_FM_MACOS}
  {$DEFINE SG_ObjStr}
{$ENDIF}

const
  sPDFLineBreak = #13#10;
  cnstPDFLineBreak: Word = $0D0A;

type
{$IFDEF SGFPC}
  PInteger = System.PInteger;
{$ENDIF}
  TsgPageSaveLogEvent = procedure(AInputSize: TFPoint; AOutputSize: TFPoint;
    ALayoutName: string) of object;

  TASCII85 = class
  private
    FContainer: TStrings;
    FValue: string;
    FBuf: Integer;
    FCount: Byte;
  public
    procedure PutByte(Value: Byte);
    procedure PutChar(Value: Byte);
    procedure Flush;
    procedure Complete;
  end;

  TpdfStream = class(TMemoryStream);
  TpdfStrings = class
  private
    FCountPdfObject: Integer;
    {$IFDEF SG_ObjStr}
    FCount: TsgIntegerList;
    FObjItems: TsgList;
    {$ELSE}
    FCount: array of Integer;
    FItems: array of array of AnsiString;
    {$ENDIF}
    FBufferStream: TpdfStream;
    function GetStrings(IndexObject: Integer; IndexContent: Integer): AnsiString;
    procedure SetStrings(IndexObject: Integer; IndexContent: Integer; const Value: AnsiString);
    function GetCountContent(IndexObject: Integer): Integer;
    function GetCountPdfObject: Integer;
    procedure GrowObject;
    procedure GrowContext(const IndexPdfObject: Integer);
    function LastIndexPdfObject: Integer;
  protected
    FStream: TpdfStream;
    procedure BeginStream;
    procedure EndStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    function Add(const S: AnsiString): Integer; overload;
    function Add(const IndexPdfObject: Integer;
      const S: AnsiString): Integer; overload;
    procedure AddStringsArray(const AStrings: array of AnsiString); overload;
    procedure AddStringsArray(const IndexPdfObject: Integer;
      const AStrings: array of AnsiString); overload;
    procedure AddXref(const ABlockLast: TpdfStrings; var Len, Cnt: Integer);
    procedure Insert(IndexContent: Integer; const S: AnsiString); overload;
    procedure Insert(IndexObject: Integer;
      IndexContent: Integer; const S: AnsiString); overload;
    function NewBlock: Integer;
    function NewDictObj(ANum: Integer; const S: AnsiString = ''): Integer;
    procedure EndDictObj;overload;
    procedure EndDictObj(const IndexObject: Integer);overload;
    property CountPdfObject: Integer read GetCountPdfObject;
    property CountContent[Index: Integer]: Integer read GetCountContent;
    property Strings[IndexObject: Integer; IndexContent: Integer]: AnsiString read GetStrings write SetStrings;
  end;

  TsgPDFCollection = class
    FCollection: TsgCollection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AData: TpdfStrings);
    procedure AddXref(const ABlockLast: TpdfStrings; var Len, Cnt: Integer);
    procedure SaveToStream(Stream: TStream);
    procedure Sort;
  end;


  TFlate = class
  private
    FContainer: TpdfStrings;
    FIndex: Integer;
  public
    procedure PutByte(Value: Byte);
    procedure PutChar(Value: Byte);
  end;

  TpdfFontData = packed record
    Hash: Word;
    Pitch: TFontPitch;
    Style: TFontStyles;
    Charset: TFontCharset;
    NameHash: Word;
  end;

  TpdfFont = class
  private
    FName: String;
    FFullName: AnsiString;
    FFont: TFont;
    FChars: TBits;
    FReferred: TpdfFont;
    FHeight: Integer;
    FCount: Integer;
    FCMap: Pointer;
    FData: TpdfFontData;
  public
    constructor Create(ANum: Integer; AFont: TFont; const AData: TpdfFontData);
    destructor Destroy; override;
    procedure FindCMap(P: Pointer; Offset: Cardinal);
    procedure CIdToGId(P: Pointer);
    function GlyphIndex(Code: Word): Word;
    function HexUnicode(const WS: WideString): AnsiString;
    property Name: String read FName;
    property FullName: AnsiString read FFullName;
    property Font: TFont read FFont;
    property Chars: TBits read FChars;
    property Referred: TpdfFont read FReferred;
    property CMap: Pointer read FCMap;
    property Count: Integer read FCount;
    property Height: Integer read FHeight write FHeight;
  end;

  TpdfFontCache = class(TList{$IFDEF SG_FIREMONKEY}<TpdfFont>{$ENDIF})
  {$IFNDEF SG_FIREMONKEY}
  protected
    function Get(Index: Integer): TpdfFont;
    procedure Put(Index: Integer; Value: TpdfFont);
  public
    property Items[Index: Integer]: TpdfFont read Get write Put; default;
{$ENDIF}
    function ByName(const AName: AnsiString): TpdfFont;
  end;

  TsgPDFExport = class;

  TsgOutlineRoot = class;

  TsgPDFObject = class
  private
    FPosition: Integer;
    FIndex: Integer;
    FKernel: TsgPDFExport;
    function GetIndex: Integer;
    function GetRefIndex: AnsiString;
  protected
    procedure Save; virtual; abstract;
    property Kernel: TsgPDFExport read FKernel;
  public
    constructor Create(AKernel: TsgPDFExport);
    property Position: Integer read FPosition;
    property PdfObjectIndex: Integer read GetIndex;
    property RefPdfObjectIndex:AnsiString read GetRefIndex;
  end;

  TsgPDFAction = class(TsgPDFObject)
  private
    FPageIndex:Integer;
    FTopOffset:Integer;
  protected
    procedure Save;override;
  public
    constructor Create( AKernel: TsgPDFExport; PageIndex,TopOffset:Integer);
  end;

  TsgOutlineNode = class(TsgPDFObject)
  private
    FChild: TList;
    FOwner: TsgOutlineRoot;
    FParent: TsgOutlineNode;
    FPrev: TsgOutlineNode;
    FNext: TsgOutlineNode;
    FTitle: string;
    FAction: TsgPDFAction;
    FExpanded: Boolean;
    FStyle: TFontStyles;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetItem ( Index: Integer ): TsgOutlineNode;
    procedure SetExpanded ( const Value: Boolean );
  protected
    procedure Save;  override;
  public
    constructor Create (AKernel: TsgPDFExport; AOwner: TsgOutlineRoot );
    destructor Destroy; override;
    procedure Delete;
    procedure DeleteChildren;
    function GetFirstChild: TsgOutlineNode;
    function GetLastChild: TsgOutlineNode;
    function GetNext: TsgOutlineNode;
    function GetNextChild ( Node: TsgOutlineNode ): TsgOutlineNode;
    function GetNextSibling: TsgOutlineNode;
    function GetPrev: TsgOutlineNode;
    function GetPrevChild ( Node: TsgOutlineNode ): TsgOutlineNode;
    function GetPrevSibling: TsgOutlineNode;
    property Style: TFontStyles read FStyle write FStyle;
    property Count: Integer read GetCount;
    property HasChildren: Boolean read GetHasChildren;
    property Item [ Index: Integer ]: TsgOutlineNode read GetItem;
    property Expanded: Boolean read FExpanded write SetExpanded;
  end;

  TsgOutlineRoot = class(TsgPDFObject)
  private
    FList: TList;
    function GetItem ( Index: Integer ): TsgOutlineNode;
    function Add ( Node: TsgOutlineNode ): TsgOutlineNode; overload;
    function AddChild ( Node: TsgOutlineNode ): TsgOutlineNode; overload;
    function AddChildFirst ( Node: TsgOutlineNode ): TsgOutlineNode; overload;
    function AddFirst ( Node: TsgOutlineNode ): TsgOutlineNode; overload;
    function Insert ( Node: TsgOutlineNode ): TsgOutlineNode; overload;
  protected
    function GetCount: Integer;
    procedure Clear;
    procedure Save;override;
  public
    constructor Create (AKernel: TsgPDFExport);
    destructor Destroy; override;
    procedure Delete ( Node: TsgOutlineNode );
    function GetFirstNode: TsgOutlineNode;
    function Add ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode; overload;
    function AddChild ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode; overload;
    function AddChildFirst ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode; overload;
    function AddFirst ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode; overload;
    function Insert ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode; overload;
    property Count: Integer read GetCount;
    property Item [ Index: Integer ]: TsgOutlineNode read GetItem; default;
  end;

  TsgPDFCurrentView = class(TsgDrawRect)
  end;

  TsgPDFPageBounds = class;

  { TsgPDFExport }

  TsgPDFExport = class(TsgMultipageSimpleExport)
  private
    FData: TpdfStrings;
    FResources: TpdfStrings;
    FImages: TpdfStrings;
    FXRefs: TpdfStrings;
    FAnnotsData: TpdfStrings;
    FLayers: TStrings;
    FFontCache: TpdfFontCache;
    FOutlineRoot: TsgOutlineRoot;
    FPageRect: TFPointList;
    FTitleList: TStringList;
    FCurrentFont: TpdfFont;
    FImgNum: Integer;
    FCompressor: TFlate;
    FQuality: Double;
    FUseHighQuality: Boolean;
    FPageWidth: Double;
    FPageHeight: Double;
    FUserWidth: Double;
    FUserHeight: Double;
    FMargins: TF2DRect;
    FTmpLW: Double;
    FTmpML: Single;
    FTmpPS: DWORD;
    FFillColor: TColor;
    FAuthor: string;
    FProducer: string;
    FCreator: string;
    FTitle: string;
    FSubject: string;
    FKeywords: string;
    FAnnots: AnsiString;
    FNotPages: Boolean;
    FExportTexts: Boolean;
    FSHXAnnotations: Boolean;
    FFntOpt: Boolean;
    FLogEvent: TsgPageSaveLogEvent;
    FPdfSizeAsExtents: Boolean;
    FPdfSizeAsExtentsKoef: Double;
    FBitmapExportMode: TsgBitmapExportMode;
    FUseOutlines: boolean;
    FColorSpace: TsgColorSpace;
    FLayersMode: Integer;
    FLayerIndex: Integer;
    FCurrentView: TsgPDFCurrentView;
    FSaveParams: PsgCADIterate;
    FPageBounds: TsgPDFPageBounds;
    function SetLayer(AReset: Boolean = False): Boolean;
    function GetLayerIndex: Integer;
    function GetLayoutsCount: Integer;
    function GetPdfFont(AFont: TFont): TPdfFont;
    procedure ClearFontCache;
    procedure ExpFontData(PF: TPdfFont; Data: Pointer; Size,Num: Integer);
    procedure ExpPdfFont(PF: TPdfFont);
    procedure ExportFonts;
    procedure PolyPoints(Points: PPoint; Count: Integer);
    procedure ProcRgn(P: PRect; Count: Integer);
    procedure DoSetColor(AColor: TColor; const S: string);
    procedure PutBytes(Ptr: Pointer; Count: Integer);
    procedure PutWords(Ptr: Pointer; Count,Sh: Integer);
    procedure PutPalette(Ptr: Pointer; Count: Integer);
    procedure PutBinRGB(const AColor: TColor);{$IFDEF SG_INLINE} inline;{$ENDIF}
    procedure ResetLW;
    procedure SetMargin(const Value: Double);
    procedure SetAuthor(const Value: string);
    procedure SetCreator(const Value: string);
    procedure SetProducer(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetSubject(const Value: string);
    procedure SetKeywords(const Value: string);
    procedure SetMargins(const Value: TF2DRect);
    procedure SetPdfSizeAsExtents(const Value: Boolean);
    procedure SetPdfSizeAsExtentsKoef(const Value: Double);
    procedure SetExportTexts(Value: Boolean);
    procedure SetSHXAnnotations(Value: Boolean);
    procedure SetUseHighQuality(Value: Boolean);
    function GetOutlineRoot: TsgOutlineRoot;
    procedure SetUseoutlines(const Value: boolean);
    procedure SetColorSpace(const Value: TsgColorSpace);
    procedure SetLayersMode(const Value: Integer);
    procedure DoGetCharWidths(APDFFontMetrics: TObject; AFirst, ALast: Integer; var AWidths);
    procedure UpdateCurrentViewBox;
  protected
    procedure AfterExport(const S: TStream); override;
    procedure BeforeExport(const S: TStream); override;
    procedure CreateOutlines;
    procedure DoInitializeView(var AView: TFMatrix); override;
    procedure DoDrawCurrentLayout; override;
    procedure ExpCharWidths(APDFFontMetrics: TObject; AFirst,ALast: Integer); overload;
    procedure ExpCharWidths(APDFFontMetrics: TObject; PF: TpdfFont); overload;
    procedure CorrectQuality;
    function GetExportRect: TRect; override;
    function GetMmToPixelX: Double; override;
    function GetBitmapRect: TRect; override;
    function GetBitmapMode: TsgBitmapExportMode; override;
    function GetColorSpaceStr(const ASpace: TsgColorSpace): AnsiString;
    procedure SetBitmapMode(const Value: TsgBitmapExportMode);
    procedure ExpPolyline(Points: PPoint; Count: Integer); override;
    procedure ExpPolygon(Points: PPoint; Count: Integer); override;
    procedure ExpPolyPolyline(const Points; Counts: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger; Count: Integer); override;
    procedure ExpPolyPolygon(const Points; Counts: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger; Count: Integer); override;
    procedure ExpSetFont(AFont: TFont); override;
    procedure ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam); override;
    procedure ExpFillRgn(P: PRect; Count: Integer); override;
    procedure ExpClipRgn(P: PRect; Count: Integer); override;
    procedure ExpSaveDC; override;
    procedure ExpRestoreDC; override;
    procedure ExpImage(const R: TRect; AImage: TPersistent); override;
    procedure ExpImageUV(const APoint1,APoint2,APoint3,APoint4: TPoint; AImage: TPersistent); override;
    procedure ExpAnnotation(AEnt: TObject; const AParams: TsgExpAnnotParam); override;
    function IsSupportColorToLineWeight: Boolean; override;
    procedure PageStart(N: Integer); override;
    procedure PageEnd(N: Integer); override;
    procedure SetStrokeColor(AColor: TColor); override;
    procedure SetFillColor(AColor: TColor); override;
    procedure ExpCloseFigure; override;
    function Flipping: Boolean; override;
    procedure SaveToStreamCustom(S: TStream); override;
    procedure SetParamsForImage(const AImage: TsgCADImage); override;
    property OutlineRoot: TsgOutlineRoot read GetOutlineRoot;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    property BitmapMode: TsgBitmapExportMode read GetBitmapMode write SetBitmapMode;
    property Title: string read FTitle write SetTitle;
    property Author: string read FAuthor write SetAuthor;
    property Creator: string read FCreator write SetCreator;
    property Producer: string read FProducer write SetProducer;
    property Subject: string read FSubject write SetSubject;
    property Keywords: string read FKeywords write SetKeywords;
    property LayoutsCount: Integer read GetLayoutsCount;
    property PageWidth: Double read FPageWidth;
    property PageHeight: Double read FPageHeight;
    property ExportTexts: Boolean read FExportTexts write SetExportTexts;
    property SHXAnnotations: Boolean read FSHXAnnotations write SetSHXAnnotations;
    property TTFFontsOptimization: Boolean read FFntOpt write FFntOpt;
    property PdfSizeAsExtents: Boolean read FPdfSizeAsExtents write SetPdfSizeAsExtents;
    property PdfSizeAsExtentsKoef: Double read FPdfSizeAsExtentsKoef write SetPdfSizeAsExtentsKoef;
    property UserWidth: Double read FUserWidth write FUserWidth;
    property UserHeight: Double read FUserHeight write FUserHeight;
    //property Quality: Double read FQuality write SetQuality;
    property UseHighQuality: Boolean read FUseHighQuality write SetUseHighQuality;
    property Margins: TF2DRect read FMargins write SetMargins;
    property OnLogEvent: TsgPageSaveLogEvent read FLogEvent write FLogEvent;
    property UseOutlines: boolean read FUseoutlines write SetUseoutlines;
    property ColorSpace: TsgColorSpace read FColorSpace write SetColorSpace;
    property LayersMode: Integer read FLayersMode write SetLayersMode;
  end;

  TsgPDFPageBounds = class(TPersistent)
  private
    FPageSize: TF2DPoint;
    FPlotArea: TF2DRect;
    FCurrentLayoutSize: TF2DPoint;
  public
    constructor Create(APDFExport: TsgPDFExport);
    property PageSize: TF2DPoint read FPageSize;
    property PlotArea: TF2DRect read FPlotArea;
    property CurrentLayoutSize: TF2DPoint read FCurrentLayoutSize;
  end;

const
  cnstPDFDefaultQualityValue = 0.025;
  cnstPDFHighQualityValue = 0.01;

implementation

uses sgZip, math;

{$IFDEF MSWINDOWS}
{$DEFINE SG_HAS_GLYPHOUTLINE_API}
{$ENDIF}

type
  PWordArray = ^TWordArray;
  TWordArray = array[Word] of Word;
  TAllocProc = function(Size: Cardinal): Pointer; cdecl;
  TReallocProc = function(P: Pointer; Size: Cardinal): Pointer; cdecl;
  TFreeProc = function(P: Pointer): Integer; cdecl;
  TCreateFontPackage = function(SrcBuffer: Pointer;
  				SrcBufferSize: Cardinal;
                                var FontPackageBuffer: Pointer;
                                var FontPackageBufferSize: Cardinal;
                                var BytesWritten: Cardinal;
                                Flags: Word;
                                TTCIndex: Word;
                                SubsetFormat: Word;
                                SubsetLanguage: Word;
                                SubsetPlatform: Word;
                                SubsetEncoding: Word;
                                SubsetKeepList: PWord;
                                SubsetKeepListCount: Word;
                                fnAllocate: TAllocProc;
                                fnReAllocate: TReallocProc;
                                fnFree: TFreeProc;
                                Reserved: Pointer = nil): Cardinal; cdecl;

{$IFDEF SGFPC}
  TFreeTypeFontAccess = class(TFreeTypeFont);
{$ENDIF}
  TsgCADImageAccess = class(TsgCADImage);
  TsgBMAdapterAccess = class(TsgBMAdapter);

  TpdfFontMetrics = class
  private
    FFont: TFont;
    FOffset: Cardinal;
    FAscent: Integer;
    FDescent: Integer;
    FCapHeight: Integer;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    FDC: HDC;
{$IFDEF SG_FM_WINDOWS}
    FTempFont: HFONT;
{$ENDIF}
{$ENDIF}
    FSubType: AnsiString;
    FName: AnsiString;
    FItalicAngle: Integer;
    FDataSize: Cardinal;
    FBounds: TRect;
    FData: Pointer;
{$IFDEF SGFPC}
    FFreeFont: TFreeTypeFontAccess;
{$ENDIF}
    function TTCIndex: Integer;
  protected
    FHandle: THandle;
  public
    constructor Create(AFont: TFont);
    destructor Destroy; override;
    function GetData: Boolean;
    procedure Squeeze(CMap: TBits);
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    property DC: HDC read FDC;
{$ENDIF}
    property SubType: AnsiString read FSubType;
    property Name: AnsiString read FName;
    property ItalicAngle: Integer read FItalicAngle;
    property DataSize: Cardinal read FDataSize;
    property Offset: Cardinal read FOffset;
    property Bounds: TRect read FBounds;
    property Data: Pointer read FData;
    property Ascent: Integer read FAscent;
    property Descent: Integer read FDescent;
    property CapHeight: Integer read FCapHeight;
  end;

var
  FontSub: THandle = 0;
  CreateFontPackage: TCreateFontPackage = nil;

const
  MMPerInch = 25.4;
  MMPerPDFUnit = MMPerInch / 72; // 0,35277777777777777777777777777778
  cnstDefaultPDFPageWidth = 210.0;
  cnstDefaultPDFPageHeight = 297.0;
  cnstMinRectSize = 3000;
  cnstDroppingSize = 8 * 1024 * 1024;

const
  TTFCFP_FLAGS_SUBSET		= 1;
  TTFCFP_FLAGS_COMPRESS		= 2;
  TTFCFP_FLAGS_TTC		= 4;
  TTFCFP_FLAGS_GLYPHLIST	= 8;

  TTFCFP_SUBSET			= 0;
  TTFCFP_SUBSET1		= 1;
  TTFCFP_DELTA			= 2;

  TTFCFP_UNICODE_PLATFORMID	= 0;
  TTFCFP_APPLE_PLATFORMID	= 1;
  TTFCFP_ISO_PLATFORMID		= 2;
  TTFCFP_MS_PLATFORMID		= 3;

  TTFCFP_STD_MAC_CHAR_SET	= 0;
  TTFCFP_SYMBOL_CHAR_SET	= 0;
  TTFCFP_UNICODE_CHAR_SET	= 1;
  TTFCFP_DONT_CARE		= $FFFF;

  cnstSG = 'SoftGold';
  cnstSGPDFExporter = cnstCompanyCST;
  cnstStartCount = 4;

  PDFHead: array[0..9] of AnsiString =
  ( '%PDF-1.5',
    '1 0 obj',
    '<< /Type /Catalog',
    '/ViewerPreferences << /PrintScaling /None >>',
    '/Pages 2 0 R',
    '>>',
    'endobj',
    '2 0 obj',
    '<< /Type /Pages',
    '/Kids');

  PDFFoot: array[0..6] of AnsiString =
  ( 'trailer',
    '',
    '/Root 1 0 R',
    '/Info 3 0 R',
    '>>',
    'startxref',
    '');

  cnstStrokeColorRGB = 'RG';
  cnstFillColorRGB = 'rg';
  cnstStrokeColorCMYK = 'K';
  cnstFillColorCMYK = 'k';

{$IFDEF SG_CPUX64}
  sBinToHex: AnsiString = '0123456789ABCDEF';
{$ENDIF}
function HexDigit(Value: Byte): AnsiChar;
{$IFNDEF SG_ASSEMBLER}
begin
  Result := AnsiChar(sBinToHex[Value and $F + 1]);
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
        AND     rcx,$F
        ADD     rcx,sBinToHex
        MOV     al,byte ptr [rcx]
{$ELSE}
        AND     AL,$F
        ADD     AL,$90
        DAA
        ADC     AL,$40
        DAA
{$ENDIF}
end;
{$ENDIF}

procedure DivMod(Dividend, Divisor: Integer; var Result, Remainder: Integer);
{$IFNDEF SG_ASSEMBLER}
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$ELSE}
asm
  {$IFDEF SG_CPUX64}
        MOV     EAX,ECX
        MOV     ECX,EDX
        XOR     EDX,EDX
        DIV     ECX
        MOV     [Result],EAX
        MOV     [Remainder],EDX
  {$ELSE}

        PUSH    EBX
        MOV     EBX,EDX
        XOR     EDX,EDX
        DIV     EBX
        MOV     EBX,Remainder
        MOV     [ECX],EAX
        MOV     [EBX],EDX
        POP     EBX
  {$ENDIF}
{$ENDIF}
end;

function GetPdfHashCode(const Buffer; Count: Integer): Word;
{$IFNDEF SG_ASSEMBLER}
var
  I,Hash: Integer;
  P: PByte;
begin
  P := PByte(@Buffer);
  Hash := 0;
  for I := 0 to Count-1 do
  begin
    Hash := Hash shl 5 and $1FFFFF;
    Hash := Hash or Hash shr 16 xor P^;
    Inc(P);
  end;
  Result := Hash;
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
        XOR     RAX,RAX
@@1:    ROL     AX,5
        XOR     AL,[RCX]
        INC     RCX
        DEC     RDX
        JNE     @@1
{$ELSE}
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
{$ENDIF}
end;
{$ENDIF}

function GetPdfStrHashCode(const AStr: string): Word;
begin
  Result := GetPdfHashCode(AStr[1], Length(AStr));
end;

procedure PutByte(var S: AnsiString; Value: Byte);
begin
  S := S + HexDigit(Value shr 4);
  S := S + HexDigit(Value);
end;

procedure PutRGB(var S: AnsiString; P: PByte);
begin
  Inc(P,2); PutByte(S, P^);
  Dec(P); PutByte(S, P^);
  Dec(P); PutByte(S, P^);
end;

function FormatA(const Format: AnsiString; const Args: array of const): AnsiString;
{$IFNDEF HAS_FEATURE_ANSISTRINGS}
var
  S: string;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  AnsiStrings.FmtStr(Result, Format, Args);
{$ELSE}
  FmtStr(S, Format, Args);
  Result := AnsiString(S);
{$ENDIF}
end;

function RealFontName(const AFontName: AnsiString): string; overload;
begin
  Result := string(AnsiString(PAnsiChar(AFontName)));
end;

function RealFontName(const AFontName: WideString): string; overload;
begin
  Result := string(WideString(PWideChar(AFontName)));
end;

function BEShort(P: Pointer): Word;
begin
  PByteArray(@Result)^[1] := PByteArray(P)^[0];
  PByteArray(@Result)^[0] := PByteArray(P)^[1];
end;

function BELong(P: Pointer): Cardinal;
var
  I: Integer;
  PC: PByte absolute P;
begin
  Result := 0;
  for I := 0 to 3 do
  begin
    Result := Result shl 8 + PC^;
    Inc(PC);
  end;
end;

procedure DoBigEndian(P: Pointer; AStart,AEnd: Integer);
var
  PW: ^Word absolute P;
  W: Word;
begin
  Inc(PW,AStart);
  while AStart <= AEnd do
  begin
    W := BEShort(PW);
    PW^ := W;
    Inc(PW);
    Inc(AStart);
  end;
end;

function HexUnicode(const WS: WideString; const Start: AnsiString = ''): AnsiString;
var I: Integer;
begin
  Result := '<' + Start;
  for I := 1 to Length(WS)
  do Result := Result +  AnsiString(IntToHex(Ord(WS[I]),4));
  Result := Result + '>';
end;

function EscapeStr(P: PAnsiChar): AnsiString;
var Esc: AnsiChar;
begin
  Result := '(';
  while P^ <> #0 do
  begin
    Esc := #0;
    case P^ of
      '(',')','\':	Esc := P^;
      #08:		Esc := 'b';
      #09:		Esc := 't';
      #10:		Esc := 'n';
      #12:		Esc := 'f';
      #13:		Esc := 'r';
    end;
    if Esc = #0 then Result := Result + P^
    else Result := Result + '\' + Esc;
    Inc(P);
  end;
  Result := Result + ')';
end;

function EscapeName(P: PAnsiChar): AnsiString; overload;
const
  {$IFDEF SG_FM_MOBILE}EC: TSysCharSet{$ELSE}EC{$ENDIF} = ['#','%','/','(',')','<','>','[',']','{','}'];
var
  I: Integer;
begin
  Result := '';
  while True do
  begin
    I := Ord(P^);
    if I = 0 then Exit;
    if (I < 33) or (I > 126) or CharInSet({$IFDEF SG_FM_MOBILE}Char{$ENDIF}(P^), EC) then
      Result := Result + '#' + AnsiString(IntToHex(I,2))
    else
      Result := Result + P^;
    Inc(P);
  end;
end;

function EscapeName(const AName: string): AnsiString; overload;
begin
  Result := EscapeName(PAnsiChar(AnsiString(AName)));
end;

{$IFDEF SGFPC}
function StyleToArray(AStyle: TFontStyles): ArrayOfString;
begin
  if fsBold in AStyle then
  begin
     SetLength(Result, 1);
     Result[High(Result)] := 'Bold';
  end;
  if fsItalic in AStyle then
  begin
     SetLength(Result, Length(Result) + 1);
     Result[High(Result)] := 'Italic';
  end;
end;
{$ENDIF}

{$IFDEF SG_ObjStr}
function CreateStringList: TStringList;
begin
  Result := TStringList.Create;
  Result.LineBreak := #1;
//  Result.DefaultEncoding := TEncoding.Unicode;
end;

function pdfUniToAnsi(const AStr: string): AnsiString;
var
  I, L: Integer;
  vArray: PWordArray;
  C: Byte;
begin
  L := Length(AStr);
  if L > 0 then
  begin
    SetLength(Result, L);
    vArray := PWordArray(@AStr[1]);
    for I := 1 to L do
    begin
      C :=  vArray^[I-1];
      Result[I] := AnsiChar(C);
    end;
  end
  else
    Result := '';
end;

function pdfAnsiToUni(const AStr: AnsiString): string;
var
  I, L: Integer;
  C: Char;
begin
  L := Length(AStr);
  if L > 0 then
  begin
    C := #0;
    SetLength(Result, L);
    for I := 1 to L do
    begin
      PWord(@C)^ := Byte(AStr[I]);
      Result[I] := C;
    end;
  end
  else
    Result := '';
end;
{$ENDIF}

{ TsgPDFPageBounds }

constructor TsgPDFPageBounds.Create(APDFExport: TsgPDFExport);
var
  vPlotSettings: TsgDXFPlotSettings;
  vPageStartLayoutSize: TF2DPoint;
begin
  if IsBadRect(APDFExport.CurrentLayout.Box) then
    vPageStartLayoutSize := cnstF2DPointZero
  else
    vPageStartLayoutSize := GetSizeFRect2D(APDFExport.CurrentLayout.Box);// maybe extents??
  if APDFExport.UsePlotSetting and APDFExport.CurrentLayout.UsingPlotSettings then
  begin
    vPlotSettings := APDFExport.CurrentLayout.PlotSettings;
    FPageSize := vPlotSettings.PlotPaperSize;
    if (vPlotSettings.PlotRotation = pr90DegreesCounterCW) or
       (vPlotSettings.PlotRotation = pr90DegreesCW) then
      SwapDoubles(FPageSize.X, FPageSize.Y);
    FPlotArea := MakeF2dRectFromFRect(vPlotSettings.PlotPaperArea);
    if vPlotSettings.PlotPaperUnits = ppuInches then
    begin
      FPlotArea.TopLeft := Pt2XScalar(FPlotArea.TopLeft, MMPerInch);
      FPlotArea.BottomRight := Pt2XScalar(FPlotArea.BottomRight, MMPerInch);
    end
    else
      FPageSize := Pt2XScalar(FPageSize, vPlotSettings.CustomPrintScaleKoef);
  end
  else
  begin
    if APDFExport.PdfSizeAsExtents then
    begin
      FPageSize.X := vPageStartLayoutSize.X * APDFExport.PdfSizeAsExtentsKoef;
      FPageSize.Y := vPageStartLayoutSize.Y * APDFExport.PdfSizeAsExtentsKoef;
    end
    else
    begin
      FPageSize.X := APDFExport.UserWidth;
      FPageSize.Y := APDFExport.UserHeight;
    end;
    FPlotArea := MakeF2DRect(APDFExport.Margins.Left, APDFExport.Margins.Top,
      FPageSize.X - APDFExport.Margins.Right - APDFExport.Margins.Left,
      FPageSize.Y - APDFExport.Margins.Bottom - APDFExport.Margins.Top);
  end;
end;

{ TpdfStrings }

constructor TpdfStrings.Create;
begin
  inherited Create;
  {$IFDEF SG_ObjStr}
  FCount := TsgIntegerList.Create;
  FObjItems := TsgList.Create;
  {$ENDIF}
  FBufferStream := TpdfStream.Create;
end;

destructor TpdfStrings.Destroy;
begin
  Clear;
  {$IFDEF SG_ObjStr}
  FreeAndNil(FObjItems);
  FreeAndNil(FCount);
  {$ENDIF}
  FBufferStream.Free;
  inherited Destroy;
end;

procedure TpdfStrings.GrowObject;
{$IFDEF SG_ObjStr}
var
  I: Integer;
  {$IFDEF SG_FM_MACOS}
  vList: TsgList;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF SG_ObjStr}
    {$IFDEF SG_FM_MACOS}
    vList := TsgList.Create;
    vList.Count := ListGrow(FObjItems.Count);
    for I := 0 to FObjItems.Count - 1 do
    begin
      vList[I] := FObjItems[I];
      FObjItems[I] := nil;
    end;
    FreeAndNil(FObjItems);
    FObjItems := vList;
    {$ELSE}
    FObjItems.Count := ListGrow(FObjItems.Count);
    {$ENDIF}
    for I := FObjItems.Count -  1 downto 0 do
    begin
      if FObjItems[I] = nil then
        FObjItems[I] := CreateStringList
      else
        Break;
    end;
  {$ELSE}
  SetLength(FItems, ListGrow(Length(FItems)));
  {$ENDIF}
end;

procedure TpdfStrings.GrowContext(const IndexPdfObject: Integer);
{$IFDEF SG_ObjStr}
var
  I: Integer;
  vStrings: TStrings;
{$ENDIF}
begin
  {$IFDEF SG_ObjStr}
  if not Assigned(FObjItems[IndexPdfObject]) then
    FObjItems[IndexPdfObject] := CreateStringList;
  vStrings := TStrings(FObjItems[IndexPdfObject]);
  for I := vStrings.Count to ListGrow(vStrings.Count) do
    vStrings.Add('');
  {$ELSE}
  SetLength(FItems[IndexPdfObject], ListGrow(Length(FItems[IndexPdfObject])));
  {$ENDIF}
end;

function TpdfStrings.LastIndexPdfObject: Integer;
begin
  Result := FCountPdfObject - 1;
end;

function TpdfStrings.Add(const IndexPdfObject: Integer; const S: AnsiString): Integer;
{$IFDEF SG_ObjStr}
var
  vStrings: TStrings;
{$ENDIF}
begin
  Result := FCount[IndexPdfObject];
  {$IFDEF SG_ObjStr}
  vStrings := TStrings(FObjItems[IndexPdfObject]);
  if (not Assigned(vStrings)) or (Result = vStrings.Count) then
    GrowContext(IndexPdfObject);
   vStrings := TStrings(FObjItems[IndexPdfObject]);
   vStrings[Result] := pdfAnsiToUni(S);
   FCount[IndexPdfObject] := FCount[IndexPdfObject] + 1;
  {$ELSE}
  if Result = Length(FItems[IndexPdfObject]) then
    GrowContext(IndexPdfObject);
  AnsiString(FItems[IndexPdfObject][Result]) := S;
  Inc(FCount[IndexPdfObject]);
  {$ENDIF}
end;

function TpdfStrings.Add(const S: AnsiString): Integer;
begin
  if FStream = nil then
  begin
    Result := Add(LastIndexPdfObject, S);
  end
  else
  begin
    FStream.Write(PByte(@S[1])^, Length(S));
    FStream.Write(cnstPDFLineBreak, SizeOf(cnstPDFLineBreak));
    Result := FCount[LastIndexPdfObject] - 1;
  end;
end;

procedure TpdfStrings.AddStringsArray(const IndexPdfObject: Integer; const AStrings: array of AnsiString);
var
  I: Integer;
begin
  for I := Low(AStrings) to High(AStrings) do
    Add(IndexPdfObject,AStrings[I]);
end;

procedure TpdfStrings.AddStringsArray(const AStrings: array of AnsiString);
begin
  AddStringsArray(LastIndexPdfObject, AStrings);
end;

procedure TpdfStrings.AddXref(const ABlockLast: TpdfStrings; var Len, Cnt: Integer);
var
  I,J,L: Integer;
  S: AnsiString;
begin
  for I := 0 to CountPdfObject - 1 do
  begin
    for J := 0 to CountContent[I] -1 do
    begin
      {$IFDEF SG_ObjStr}
      S := GetStrings(I, J);
      {$ELSE}
      S := FItems[I,J];
      {$ENDIF}

      if S = 'xref' then Exit;
      L := Length(S);
      if (L > 6) and (Copy(S,L-5,6) = ' 0 obj') then
      begin
        ABlockLast.Add(FormatA('%.10d 00000 n', [Len - 1]));
        Inc(Cnt);
      end;
      Inc(Len, L + Length(sPDFLineBreak));
    end;
  end;
end;

procedure TpdfStrings.BeginStream;
begin
  Add('');
  Add('>>');
  Add('stream');
  FStream := FBufferStream;
end;

procedure TpdfStrings.EndStream;
var
  S, vFilter: AnsiString;
begin
  Compress(FStream);
  vFilter :=  FormatA('/Filter /FlateDecode /Length %d', [FStream.Size]);
  {$IFDEF SG_ObjStr}
  SetStrings(LastIndexPdfObject, FCount[LastIndexPdfObject] - 3, vFilter);
  {$ELSE}
  AnsiString(FItems[LastIndexPdfObject][FCount[LastIndexPdfObject] - 3]) := vFilter;
  {$ENDIF}
  SetLength(S, FStream.Size);
  Move(FStream.Memory^, PByte(@S[1])^, FStream.Size);
  FBufferStream.Position := 0;
  FBufferStream.SetPointer(FBufferStream.Memory, 0);
  FStream := nil;
  Add(S);
  Add('endstream');
  Add('endobj');
end;

procedure TpdfStrings.SaveToStream(Stream: TStream);
var
  I,J: Integer;
  vBuffer, vStr: AnsiString;
begin
  vBuffer := '';
  for I := 0 to CountPdfObject - 1 do
    for J := 0 to CountContent[I] - 1 do
    begin
      vStr := Strings[I,J];
      vBuffer := vBuffer + vStr + sPDFLineBreak;
      if Length(vBuffer) > cnstDroppingSize then
      begin
        Stream.Write(PByte(@vBuffer[1])^, Length(vBuffer));
        vBuffer := '';
      end;
    end;
  if Length(vBuffer) > 0 then
    Stream.Write(PByte(@vBuffer[1])^, Length(vBuffer));
end;

procedure TpdfStrings.Insert(IndexContent: Integer; const S: AnsiString);
begin
  Insert(LastIndexPdfObject, IndexContent, S);
end;

procedure TpdfStrings.Insert(IndexObject: Integer;
  IndexContent: Integer; const S: AnsiString);
var
  I: Integer;
  {$IFDEF SG_ObjStr}
  vStrings: TSTrings;
  {$ENDIF}
begin
  if FCountPdfObject = {$IFDEF SG_ObjStr}FObjItems.Count{$ELSE}Length(FItems){$ENDIF} then
    GrowObject;
  if FCount[IndexObject] = {$IFDEF SG_ObjStr}FObjItems.Count{$ELSE}Length(FItems){$ENDIF} then
    GrowContext(IndexObject);
  if IndexContent < FCount[IndexObject] then
  begin
    {$IFDEF SG_ObjStr}
    vStrings := TStrings(FObjItems[IndexObject]);
    if Assigned(vStrings) then
    begin
      for I := FCount[IndexObject] downto IndexContent do
        vStrings[I + 1] := vStrings[I];
      vStrings[IndexContent] := '';
    end;
    {$ELSE}
    for I := FCount[IndexObject] downto IndexContent do
      System.Move(FItems[IndexObject][I], FItems[IndexObject][I+1], SizeOf(FItems[IndexObject][I]));
    Pointer(FItems[IndexObject][IndexContent]) := nil;
    {$ENDIF}
  end;
  {$IFDEF SG_ObjStr}
  SetStrings(IndexObject, IndexContent, S);
  FCount[IndexObject] := FCount[IndexObject] + 1;
  {$ELSE}
  AnsiString(FItems[IndexObject][IndexContent]) := S;
  Inc(FCount[IndexObject]);
  {$ENDIF}
end;


procedure TpdfStrings.Clear;
begin
  FStream := nil;
  FCountPdfObject := 0;
  {$IFDEF SG_ObjStr}
  ClearsgList(FObjItems);
  FCount.Count := 0;
  {$ELSE}
  SetLength(FItems, 0);
  SetLength(FCount, 0);
  {$ENDIF}
end;

function TpdfStrings.GetStrings(IndexObject: Integer; IndexContent: Integer): AnsiString;
{$IFDEF SG_ObjStr}
var
  vStrings: TStrings;
  vStr: string;
{$ENDIF}
begin
  {$IFDEF SG_ObjStr}
  Result := '';
  vStrings := TStrings(FObjItems[IndexObject]);
  if Assigned(vStrings) then
  begin
    vStr := vStrings[IndexContent];
    Result := pdfUniToAnsi(vStr);
  end;
  {$ELSE}
  Result := FItems[IndexObject][IndexContent];
  {$ENDIF}
end;

procedure TpdfStrings.SetStrings(IndexObject: Integer; IndexContent: Integer; const Value: AnsiString);
{$IFDEF SG_ObjStr}
var
  vStrings: TStrings;
  vStr: string;
{$ENDIF}
begin
  {$IFDEF SG_ObjStr}
  vStrings := TStrings(FObjItems[IndexObject]);
  if Assigned(vStrings) then
  begin
    vStr := pdfAnsiToUni(Value);
    vStrings[IndexContent] := vStr;
  end;
  {$ELSE}
  FItems[IndexObject][IndexContent] := Value;
  {$ENDIF}
end;

function TpdfStrings.GetCountContent(IndexObject: Integer): Integer;
begin
  Result := FCount[IndexObject];
end;

function TpdfStrings.GetCountPdfObject: Integer;
begin
  Result := FCountPdfObject;
end;

function TpdfStrings.NewBlock: Integer;
{$IFDEF SG_ObjStr}
var
  vStrings: TStrings;
{$ENDIF}
begin
  Result := FCountPdfObject;
  if Result = {$IFDEF SG_ObjStr}FObjItems.Count{$ELSE}Length(FItems){$ENDIF} then
    GrowObject;
  {$IFDEF SG_ObjStr}
   vStrings := TStrings(FObjItems[Result]);
   if Assigned(vStrings) then
     vStrings.Clear;
  {$ELSE}
  FItems[Result] := nil;
  {$ENDIF}
  if Result = {$IFDEF SG_ObjStr}FCount.Count{$ELSE}Length(FCount){$ENDIF} then
    {$IFDEF SG_ObjStr}FCount.Count := ListGrow(FCount.Count){$ELSE}SetLength(FCount, ListGrow(Length(FCount))){$ENDIF};
  FCount[Result] := 0;
  Inc(FCountPdfObject);
end;

function TpdfStrings.NewDictObj(ANum: Integer; const S: AnsiString = ''): Integer;
begin
  Result := NewBlock;
  //
  Add(FormatA('%d 0 obj', [ANum]));
  Add('<<' + S);
end;

procedure TpdfStrings.EndDictObj;
begin
  EndDictObj(LastIndexPdfObject);
end;

procedure TpdfStrings.EndDictObj(const IndexObject: Integer);
begin
  Add(IndexObject,'>>');
  Add(IndexObject,'endobj');
end;

{ TASCII85 }

procedure TASCII85.PutByte(Value: Byte);
begin
  FBuf := FBuf shl 8 or Value;
  Inc(FCount,8);
  if FCount >= 32 then Flush;
end;

procedure TASCII85.PutChar(Value: Byte);
begin
  FBuf := FBuf shl 4 or Value and $F;
  Inc(FCount,4);
  if FCount >= 32 then Flush;
end;

procedure TASCII85.Flush;
var
  I,AMod: Integer;
  Data: array[0..5] of Char;
begin
  if FCount = 0 then Exit;
  if FCount > 32 then raise EIntOverflow.Create('ACSII85 overflow');
  if (FCount=32) and (FBuf=0) then FValue := FValue + 'z'
  else begin
    if FCount < 32 then FBuf := FBuf shl (32 - FCount);
    FCount := (FCount + 15) shr 3;
    for I := 4 downto 0 do begin
      DivMod(FBuf,85,FBuf,AMod);
      Data[I] := Chr(AMod + $21);
    end;
    Data[FCount] := #0;
    FValue := FValue + Data;
  end;
  FBuf := 0; FCount := 0;
  if (FContainer <> nil) and (Length(FValue) > 1000) then begin
    FContainer.Add(FValue);
    FValue := '';
  end;
end;

procedure TASCII85.Complete;
begin
  Flush;
  FValue := FValue + '~>';
  if FContainer <> nil then begin
    FContainer.Add(FValue);
    FValue := '';
  end;
end;

{ TFlate }

procedure TFlate.PutByte(Value: Byte);
var P: PByte;
begin
  P := PByte(FContainer.FStream.Memory);
  Inc(P, FIndex shr 1);
  P^ := Value;
  Inc(FIndex,2);
end;

procedure TFlate.PutChar(Value: Byte);
var P: PByte;
begin
  P := PByte(FContainer.FStream.Memory);
  Inc(P, FIndex shr 1);
  if FIndex and 1 = 0 then
    Value := Value shl 4
  else
    Value := Value and $0F or P^;
  P^ := Value;
  Inc(FIndex);
end;

{ TpdfFontMetrics }

constructor TpdfFontMetrics.Create(AFont: TFont);
{$IFDEF SGFPC}
var
  Family: TCustomFamilyCollectionItem;
  FontItem: TCustomFontCollectionItem;
  prop: TT_Face_Properties;
  s: Single;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  N: Integer;
{$ENDIF}
{$IFDEF SG_HAS_GLYPHOUTLINE_API}
var
  vOutline: POutlineTextMetricA;
  vMetrics: TTextMetric;
{$ENDIF}
begin
  inherited CReate;
  FFont := AFont;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
  FDC := CreateCompatibleDC(0);
{$IFDEF SG_FM_WINDOWS}
  FTempFont := CreateFont(-1000, 0, 0, 0, FontWeightToWinapi(AFont.StyleExt.Weight),
                      Cardinal(not AFont.StyleExt.Slant.IsRegular), 0, 0,
                      DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                      DEFAULT_PITCH or FF_DONTCARE, PChar(AFont.Family));
  FHandle := SelectObject(FDC, FTempFont);
{$ELSE}
  FHandle := SelectObject(FDC, FFont.Handle);
{$ENDIF}
{$ENDIF}
  FSubType := 'Type1';
  FName := AnsiString(RealFontName(AFont.Name));
{$IFDEF SGFPC}
  { TODO: check TpdfFontMetrics.Create on Linux}
  Family := FontCollection.Family[AFont.Name];
  if Assigned(Family) then
  begin
    FontItem := Family.GetFont(StyleToArray(AFont.Style));
    if Assigned(FontItem) then
    begin
      FFreeFont := TFreeTypeFontAccess(FontItem.CreateFont);
      FName := EscapeName(FFreeFont.Information[ftiFullName]);
      FFreeFont.DPI := 72;
      FFreeFont.SizeInPoints := Abs(FFont.Height);

      TT_Get_Face_Properties(FFreeFont.FFace, prop);
      FAscent := Round(FFreeFont.Ascent);
      FDescent := Round(FFreeFont.Descent);
      FCapHeight := Round(FFreeFont.CapHeight);
      s := Abs(FFreeFont.SizeInPoints / prop.header^.units_per_EM);
      FBounds.Left := Round(s * prop.header^.xMin);
      FBounds.Right := Round(s * prop.header^.xMax);
      FBounds.Bottom := Round(s * prop.header^.yMin);
      FBounds.Top := Round(s * prop.header^.yMax);
      FItalicAngle := prop.postscript^.italicAngle;
      FSubType := 'Type0';
    end;
  end;
{$ENDIF}
{$IFDEF SG_HAS_GLYPHOUTLINE_API}
  N := GetOutlineTextMetricsA(FDC,0,nil);
  if N > 0 then
  begin
    GetMem(vOutline,N);
    try
      GetOutlineTextMetricsA(FDC,N,vOutline);
      FItalicAngle := vOutline^.otmItalicAngle;
      FBounds := vOutline^.otmrcFontBox;
      FAscent := vOutline^.otmTextMetrics.tmAscent;
      FDescent := vOutline^.otmTextMetrics.tmDescent;
      FCapHeight := vOutline^.otmsCapEmHeight;
      FSubType := 'Type0';
      FName := EscapeName(PAnsiChar(IntPtr(vOutline) + IntPtr(vOutline^.otmpFullName)));
    finally
      FreeMem(vOutline);
    end;
  end
  else
  begin
    GetTextMetrics(FDC,vMetrics);
    FAscent := vMetrics.tmAscent;
    FDescent := vMetrics.tmDescent;
    FCapHeight := FAscent;
  end
{$ENDIF}
end;

destructor TpdfFontMetrics.Destroy;
begin
  FreeMem(FData);
{$IFDEF SG_HAS_WINAPI_INTERFACE}
  if FHandle <> 0 then
    SelectObject(FDC,FHandle);
  DeleteDC(FDC);
{$IFDEF SG_FM_WINDOWS}
  DeleteObject(FTempFont);
{$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  FFreeFont.Free;
{$ENDIF}
  inherited Destroy;
end;

function TpdfFontMetrics.GetData: Boolean;
{$IFDEF SGFPC}
begin
  Result := False;
  if Assigned(FFreeFont) then
    if FFreeFont.CheckInstance and (TT_Get_Font_Data(FFreeFont.FFace, 0, 0, nil^, LongInt(FDataSize)) = TT_Err_Ok) then
    begin
      GetMem(FData, FDataSize);
      Result := TT_Get_Font_Data(FFreeFont.FFace, 0, 0, FData^, LongInt(FDataSize)) = TT_Err_Ok;
    end;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var Ttcf: DWORD;
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
  Ttcf := $66637474;
  { TODO: LINUX: function TpdfFontMetrics.GetData: Boolean; }
  FDataSize := GetFontData(FDC,Ttcf,0,nil,0);
  FOffset := GetFontData(FDC,0,0,nil,0);
{$ENDIF}
  if Integer(FDataSize) > 0
  then FOffset := FDataSize - FOffset
  else begin
    FDataSize := FOffset;
    FOffset := 0;
{$IFDEF MSWINDOWS}
    Ttcf := 0;
{$ENDIF}
  end;
  if Integer(FDataSize) <= 0 then Exit;
{$IFDEF MSWINDOWS}
  GetMem(FData,FDataSize);
  GetFontData(FDC,Ttcf,0,FData,FDataSize);
{$ENDIF}
  Result := True;
end;
{$ENDIF}

function TpdfFontMetrics.TTCIndex: Integer;
var P: ^Cardinal;
begin
  Result := 0;
  if FOffset = 0 then Exit;
  P := FData;
  Inc(P,3);
  while FOffset > BELong(P) do
  begin
    Inc(P);
    Inc(Result);
  end;
end;

procedure TpdfFontMetrics.Squeeze(CMap: TBits);
var
  Buf: Pointer;
  Chars: PWordArray;
  BufSize, Written: Cardinal;
  I,Flags,Count: Word;
begin
  Buf := nil;
  Count := 0;
  Flags := TTFCFP_FLAGS_SUBSET;
  if FOffset <> 0
  then Flags := Flags or TTFCFP_FLAGS_TTC;
  GetMem(Chars, CMap.Size shl 1);
  try
    for I := 0 to CMap.Size-1 do
    if CMap[I] then
    begin
      Chars^[Count] := I;
      Inc(Count);
    end;
    CreateFontPackage(FData,FDataSize,Buf,BufSize,Written,Flags,TTCIndex,TTFCFP_SUBSET,0,
    		      TTFCFP_MS_PLATFORMID,TTFCFP_UNICODE_CHAR_SET,PWord(Chars),Count,
                      @GetMemory,@ReallocMemory,@FreeMemory);
  finally
    FreeMem(Chars);
  end;
  if Buf = nil then Exit;
  FreeMem(FData);
  FData := Buf;
  FDataSize := Written;
  FOffset := 0;
end;

{ TpdfFont }

constructor TpdfFont.Create(ANum: Integer; AFont: TFont; const AData: TpdfFontData);
begin
  FData := AData;
  FName := '/Ft' + IntToStr(ANum+1);
  FFullName := EscapeName(PAnsiChar(AnsiString(AFont.Name)));
  FChars := TBits.Create;
  FFont := TFont.Create;
  FFont.Assign(AFont);
  FFont.Height := -1000;
end;

destructor TpdfFont.Destroy;
begin
  FreeMem(FCMap);
  FChars.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TpdfFont.FindCMap(P: Pointer; Offset: Cardinal);
var
  PC: ^Byte absolute P;
  PS: ^Word;
  PL: ^DWord absolute PS;
  N: Integer;
begin
  if Assigned(FCMap) then Exit;
  Inc(PAnsiChar(P), Offset);
  PL := P;
  Inc(PL);
  N := BEShort(PL);
  Dec(PL,2);
  while N > 0 do
  begin
    Inc(PL,4);
    if PL^ = $70616D63 then Break;	// cmap
    Dec(N);
  end;
  if N = 0 then Exit;
  Inc(PL,2);
  Inc(PC, BELong(PL) - Offset);
  PS := P;
  if PS^ <> 0 then Exit;  		// Table version number (0)
  Inc(PS);
  N := BEShort(PS);
  Dec(PS,3);
  while N > 0 do
  begin
    Inc(PL,2);
    if PL^ = $01000300 then Break;	// Platform & Encoding ID (Unicode)
    Dec(N);
  end;
  if N = 0 then Exit;
  Inc(PL);
  Inc(PC, BELong(PL));
  PS := P;
  if PS^ <> $400 then Exit;  		// Format (4)
  Inc(PS);
  N := BEShort(PS);
  GetMem(FCMap,N);
  Move(P^, FCMap^, N);
  DoBigEndian(FCMap, 0, N shr 1 - 1);
  PS := FCMap;
  Inc(PS,3);
  FCount := PS^ shr 1;
end;

procedure TpdfFont.CIdToGId(P: Pointer);
var
  P0: PWordArray;
  P1: PWordArray absolute P;
  I,J,AStart,AEnd,Delta,Range: Integer;
begin
  FillChar(P^, FChars.Size shl 1, 0);
  P0 := FCMap;
  if P0=nil then Exit;
  for I := 0 to Count-1 do
  begin
    AEnd := P0^[I+7];
    AStart := P0^[I+8+Count];
    if AStart >= FChars.Size then Exit;
    if AEnd >= FChars.Size
    then AEnd := FChars.Size - 1;
    Delta := P0^[I+8+2*Count];
    Range := P0^[I+8+3*Count];
    if Range <> 0 then
    begin
      Range := Range shr 1 + I+8+3*Count - AStart;
      for J := AStart to AEnd do
      if FChars[J] then P1^[J] := P0^[J+Range];
    end
    else for J := AStart to AEnd do
    if FChars[J] then P1^[J] := J+Delta;
    DoBigEndian(P,AStart,AEnd);
  end;
end;

function TpdfFont.GlyphIndex(Code: Word): Word;
var
  PW: ^Word;
  P: PWordArray absolute PW;
  A,B,C: Integer;
  Start,Range: Word;
begin
  Result := Code;
  PW := FCMap;
  if PW=nil then Exit;
  Inc(PW,7);
  A := 0; B := Count-1;
  while A < B do
  begin
    C := (A + B) shr 1;
    Start := P^[C];
    if Code > Start then A := C+1 else B := C;
  end;
  Inc(A, Count+1);
  Start := P^[A];
  Result := 0;
  if Code < Start then Exit;
  Range := P^[A+2*Count];
  if Range=0 then Result := Code + P^[A+Count]
  else Result := P^[A+2*Count + Range shr 1 + Code - Start];
end;

function TpdfFont.HexUnicode(const WS: WideString): AnsiString;
var
  vSrcLen: Integer;
  vPSrc, vPSrcEnd: PByte;
  vPDst: PWord;
begin
  vSrcLen := Length(WS);
  vPSrc := PByte(Pointer(WS));
  vPSrcEnd := vPSrc;
  Inc(vPSrcEnd, SizeOf(WideChar) * vSrcLen);
  SetLength(Result, 2 * SizeOf(WideChar) * vSrcLen + 2);
  vPDst := PWord(Pointer(Result));
  PByte(vPDst)^ := Ord('<');
  Inc(PByte(vPDst));
  while IntPtr(vPSrc) < IntPtr(vPSrcEnd) do
  begin
    vPDst^ := ByteBinToHex^[PByteArray(vPSrc)^[1]];
    Inc(vPDst);
    vPDst^ := ByteBinToHex^[PByteArray(vPSrc)^[0]];
    Inc(vPDst);
    FChars[PWord(vPSrc)^] := True;
    Inc(PWord(vPSrc));
  end;
  PByte(vPDst)^ := Ord('>');
end;

{ TpdfFontCache }

{$IFNDEF SG_FIREMONKEY}
procedure TpdfFontCache.Put(Index: Integer; Value: TpdfFont);
begin
  inherited Put(Index,Value);
end;

function TpdfFontCache.Get(Index: Integer): TpdfFont;
begin
  Result := TpdfFont(inherited Get(Index));
end;
{$ENDIF}

function TpdfFontCache.ByName(const AName: AnsiString): TpdfFont;
var I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := Items[I];
    if Result.FullName = AName then Exit;
  end;
  Result := nil;
end;

{ TsgPDFExport }

constructor TsgPDFExport.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  CADImage.CustomDraw := False;
  FCurrentView := TsgPDFCurrentView.Create;
  FCurrentView.AttachMatrix(@FParams^.DrawMatrix);
  InitByteBinToHex;
  FPdfSizeAsExtentsKoef := 1;
  FNotPages := True;
  FFntOpt := True;
  FData := TpdfStrings.Create;
  FResources := TpdfStrings.Create;
  FImages := TpdfStrings.Create;
  FPageRect := TFPointList.Create;
  FTitleList := TStringList.Create;
  FAnnotsData := TpdfStrings.Create;
  FXRefs := TpdfStrings.Create;
  FFontCache := TpdfFontCache.Create;
  FCompressor := TFlate.Create;
  FCompressor.FContainer := FImages;
  LayoutExportMode := lemAllPaperSpaces;
  SetUseHighQuality(DefExportParams.UseHighQuality);
  FUserWidth := cnstDefaultPDFPageWidth;
  FUserHeight := cnstDefaultPDFPageHeight;
  FPageWidth := cnstDefaultPDFPageWidth;   // mm
  FPageHeight := cnstDefaultPDFPageHeight; // mm
  SetMargin(DefExportParams.Margin);   // mm
  FTitle := 'Layout';
  FAuthor := cnstSG;
{$IFNDEF SG_FIREMONKEY}
  FCreator := cnstSGPDFExporter;
{$ELSE}
  FCreator := cnstCSTCADNavigator;
{$ENDIF}
  FProducer := cnstSGPDFExporter;
  ExportTexts := True;
  SHXAnnotations := True;
  FBitmapExportMode := bemNone;
  FLayersMode := DefExportParams.LayersMode;
end;

destructor TsgPDFExport.Destroy;
begin
  ClearFontCache;
  FCompressor.Free;
  FData.Free;
  FResources.Free;
  FImages.Free;
  FPageRect.Free;
  FTitleList.Free;
  FAnnotsData.Free;
  FXRefs.Free;
  FFontCache.Free;
  FreeObjectStringList(FLayers);
  FCurrentView.Free;
  inherited Destroy;
end;

procedure TsgPDFExport.SetExportTexts(Value: Boolean);
begin
  FExportTexts := Value;
  if FExportTexts then
  begin
    CADImage.ExpProcs.ExpSetFont := ExpSetFont;
    CADImage.ExpProcs.ExpTextOut := ExpTextOut;
    SetNormalsInternal(1, 1);
  end
  else
  begin
    CADImage.ExpProcs.ExpSetFont := nil;
    CADImage.ExpProcs.ExpTextOut := nil;
  end;
end;

procedure TsgPDFExport.SetSHXAnnotations(Value: Boolean);
begin
  if Value = FSHXAnnotations then Exit;
  FSHXAnnotations := Value;
  if Value then
    CADImage.ExpProcs.ExpAnnotation := ExpAnnotation
  else
    CADImage.ExpProcs.ExpAnnotation := nil;
end;

procedure TsgPDFExport.SetUseHighQuality(Value: Boolean);
begin
  FUseHighQuality := Value;
  if FUseHighQuality then
    FQuality := cnstPDFHighQualityValue
  else
    FQuality := cnstPDFDefaultQualityValue;
end;

procedure TsgPDFExport.SetUseoutlines(const Value: boolean);
begin
  FUseoutlines := Value;
end;

procedure TsgPDFExport.UpdateCurrentViewBox;
begin
  if Assigned(CurrentLayout) then
    FCurrentView.Box := CurrentLayout.Box
  else
    FCurrentView.Box := MakeFRect(-0.5, 0.5, 0, 0.5, -0.5, 0);
end;

function TsgPDFExport.GetOutlineRoot: TsgOutlineRoot;
begin
  Result := FOutlineRoot;
end;

procedure TsgPDFExport.AfterExport(const S: TStream);
begin
  inherited AfterExport(S);
  CADImage.Converter.Params := FSaveParams;
end;

procedure TsgPDFExport.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  FUserWidth := AExportParams.PageWidth;
  FUserHeight := AExportParams.PageHeight;
  SetMargin(AExportParams.Margin);
  Title := AExportParams.Title;
  Author := AExportParams.Author;
  Subject := AExportParams.Subjct;
  Keywords := AExportParams.Keywords;
  UsePlotSetting := AExportParams.UseExtentsFromPlotSettings;
  PdfSizeAsExtents := AExportParams.SizeAsExtents;
  PdfSizeAsExtentsKoef := AExportParams.MMInCad;
  ExportTexts := AExportParams.ExportTexts;
  SHXAnnotations := AExportParams.SHXAnnotations;
  TTFFontsOptimization := AExportParams.TrueTypeOptimization;
  SetUseHighQuality(AExportParams.UseHighQuality);
  CorrectQuality;
  BitmapMode := AExportParams.PdfBitmapMode;
  UseOutlines := AExportParams.UseOutlines;
  ColorSpace := AExportParams.ColorSpace;
  LayersMode := AExportParams.LayersMode;
//  Creator := GetProgNameAndVer(True);
//  Producer := 'Soft Gold';
end;

procedure TsgPDFExport.BeforeExport(const S: TStream);
begin
  inherited BeforeExport(S);
  FSaveParams := CADImage.Converter.Params;
  CADImage.Converter.Params := nil;
end;

procedure TsgPDFExport.ClearFontCache;
var I: Integer;
begin
  for I := 0 to FFontCache.Count-1 do FFontCache[I].Free;
  FFontCache.Count := 0;
  FCurrentFont := nil;
end;

procedure TsgPDFExport.CorrectQuality;
var
  vMargins: TF2DRect;
  vHeight,vWidth,vPageSize: Double;
  vMax: Integer;
  vExportRect: TRect;
begin
  vMargins := Margins;
  vWidth := FPageWidth - Abs(vMargins.Left + vMargins.Right);
  vHeight := FPageHeight - Abs(vMargins.Top + vMargins.Bottom);
  vPageSize := Max(vWidth, vHeight);
  XSize := Round(vPageSize / FQuality);
  vExportRect := inherited GetExportRect;
  vMax := Max(vExportRect.Right - vExportRect.Left,
    vExportRect.Bottom - vExportRect.Top);
  if vMax < cnstMinRectSize then
    FQuality := FQuality * vMax / cnstMinRectSize;
end;

function TsgPDFExport.GetExportRect: TRect;
begin
  XScale := 1 / MMPerPDFUnit;
  XSize := Round(FPageWidth / FQuality);
  FData.Insert(FData.LastIndexPdfObject -1, 3 , FormatA('/MediaBox [0 0 %f %f]',
    [FPageWidth / MMPerPDFUnit, FPageHeight  / MMPerPDFUnit]));
  FPageRect.Add(MakeFPoint(FPageWidth / MMPerPDFUnit, FPageHeight  / MMPerPDFUnit));
  Result := FCurrentView.RoundRect;
end;

function TsgPDFExport.GetLayerIndex: Integer;
var
  vIndex: Integer;
  vEntity: TsgDXFEntity;
  vLayer: TsgDXFLayer;
begin
  Result := -1;
  if Assigned(FLayers) and (EntitiesStack.Count > 0) then
  begin
    vEntity := TsgDXFEntity(EntitiesStack.Last);
    if Assigned(vEntity) then
    begin
      vLayer := EntLayer(vEntity, Params^.Insert);
      if Assigned(vLayer) then
      begin
        vIndex := FLayers.IndexOf(vLayer.Name);
        if vIndex < 0 then
          vIndex := FLayers.AddObject(vLayer.Name, TsgObjectWithField.CreateInt(FLayers.Count + 1));
        Result := TsgObjectInt64(FLayers.Objects[vIndex]).FieldInt;
      end;
    end;
  end;
end;

function TsgPDFExport.GetLayoutsCount: Integer;
begin
  Result := LayoutsExportCount;
end;

function TsgPDFExport.GetMmToPixelX: Double;
begin
  Result := MMPerPDFUnit;
end;

function TsgPDFExport.GetBitmapRect: TRect;
begin
  Result := GetExportRect;
end;

function TsgPDFExport.GetColorSpaceStr(const ASpace: TsgColorSpace): AnsiString;
begin
  case ASpace of
    clspCMYK:  Result := 'DeviceCMYK';
  else
    Result := 'DeviceRGB';
  end;
end;

function TsgPDFExport.GetBitmapMode: TsgBitmapExportMode;
begin
  Result := FBitmapExportMode;
end;

procedure TsgPDFExport.SetBitmapMode(const Value: TsgBitmapExportMode);
begin
  if FBitmapExportMode <> Value then
  begin
    FBitmapExportMode := Value;
    case FBitmapExportMode of
      bemNone: SetExpProc(CADImage);
      else
        ClearExpProc(CADImage);
    end;
  end;
end;

function TsgPDFExport.GetPdfFont(AFont: TFont): TPdfFont;
var
  I: Integer;
  FM: TpdfFontMetrics;
  FD: TpdfFontData;
begin
  FillChar(FD, SizeOf(FD), 0);
  FD.Pitch := AFont.Pitch;
  FD.Style := AFont.Style;
  FD.Charset := AFont.Charset;
  FD.NameHash := GetPdfStrHashCode(RealFontName(AFont.Name));
  FD.Hash := GetPdfHashCode(FD, sizeof(FD));
  for I := 0 to FFontCache.Count-1 do
  begin
    Result := FFontCache[I];
    if Result.FData.Hash <> FD.Hash then Continue;
    if not CompareMem(@Result.FData, @FD, sizeof(FD)) then
      Continue;
    if Assigned(Result.Referred) then
      Result := Result.Referred;
    Exit;
  end;
  Result := TpdfFont.Create(FFontCache.Count, AFont, FD);
  FM := TpdfFontMetrics.Create(Result.Font);
  try
    Result.FReferred := FFontCache.ByName(FM.Name);
    FFontCache.Add(Result);
    if Assigned(Result.Referred) then
      Result := Result.Referred
    else
      Result.FFullName := FM.Name;
  finally
    FM.Free;
  end;
end;

function TsgPDFExport.IsSupportColorToLineWeight: Boolean;
begin
  Result := True;
end;

procedure TsgPDFExport.CreateOutlines;
begin
  FOutlineRoot := TsgOutlineRoot.Create(Self);
end;

procedure TsgPDFExport.ExpCharWidths(APDFFontMetrics: TObject; AFirst,ALast: Integer);
var
  I: Integer;
  S: AnsiString;
  Buf: array[Byte] of Integer;
begin
  DoGetCharWidths(APDFFontMetrics, AFirst, ALast, Buf);
  FImages.Add('/Widths [');
  for I := 0 to ALast-AFirst do
  begin
    S := S + FormatA('%5d', [Buf[I]]);
    if I and $F <> $F then Continue;
    FImages.Add(S);
    S := '';
  end;
  if S <> '' then FImages.Add(S);
  FImages.Add(']');
end;

procedure TsgPDFExport.ExpCharWidths(APDFFontMetrics: TObject; PF: TpdfFont);
  procedure ExpRange(AStart,AEnd: Integer);
  var
    S: AnsiString;
    E,I: Integer;
    W: array[0..19] of Integer;
  begin
    S := FormatA('%5d [', [AStart]);
    while AStart <= AEnd do
    begin
      E := AStart + 19;
      if E > AEnd then E := AEnd;
      DoGetCharWidths(APDFFontMetrics,AStart,E,W);
      Dec(E,AStart);
      for I := 0 to E do S := S + FormatA('%5d', [W[I]]);
      Inc(AStart,20);
      if AStart > AEnd then S := S + ' ]';
      FImages.Add(S);
      S := '       ';
    end;
  end;
var
  PS,PE: Integer;
begin
  PS := 0;
  FImages.Add('/W [');
  while PS < PF.Chars.Size do
  begin
    while (PS < PF.Chars.Size) and not PF.Chars[PS] do Inc(PS);
    PE := PS;
    while (PE < PF.Chars.Size) and PF.Chars[PE] do Inc(PE);
    ExpRange(PS,PE-1);
    PS := PE + 1;
  end;
  FImages.Add(']');
end;

procedure TsgPDFExport.ExpFontData(PF: TPdfFont; Data: Pointer; Size,Num: Integer);
begin
  Inc(FImgNum,2);
  FImages.NewDictObj(Num);
  FImages.BeginStream;
  FImages.FStream.Size := PF.Chars.Size shl 1;
  PF.CIdToGId(FImages.FStream.Memory);
  FImages.EndStream;
  FImages.NewDictObj(Num+1, FormatA(' /Length1 %d', [Size]));
  FImages.BeginStream;
  FImages.FStream.WriteBuffer(Data^,Size);
  FImages.EndStream;
end;

procedure TsgPDFExport.ExpPdfFont(PF: TPdfFont);
var
  N: Integer;
  FM: TpdfFontMetrics;
  R: TRect;
begin
  if Assigned(PF.Referred) or (PF.Chars.Size = 0) then Exit;
  N := FImgNum;
  FResources.Add(FormatA('%s %d 0 R', [PF.FName, N]));
  FImages.NewDictObj(N, ' /Type /Font');
  FM := TpdfFontMetrics.Create(PF.Font);
  try
    FImages.Add('/Subtype /' + FM.SubType);
    FImages.Add('/BaseFont /' + FM.Name);
    Inc(FImgNum);
    if FM.GetData then
    begin
      FImages.Add('/Encoding /Identity-H');
      FImages.Add('/ToUnicode /Identity-H');
      FImages.Add(FormatA('/DescendantFonts [ %d 0 R ]', [N+1]));
      FImages.Add('>> endobj');
      Inc(FImgNum);
      FImages.NewDictObj(N+1, ' /Type /Font');
      FImages.Add('/Subtype /CIDFontType2');
      FImages.Add('/BaseFont /' + FM.Name);
      Fimages.Add('/CIDSystemInfo << /Ordering (Identity) /Registry (Adobe) /Supplement 0 >>');
      FImages.Add(FormatA('/FontDescriptor %d 0 R', [N+2]));
      if FFntOpt and Assigned(CreateFontPackage) then
        FM.Squeeze(PF.Chars);
      PF.FindCMap(FM.Data, FM.Offset);
      FImages.Add(FormatA('/CIDToGIDMap %d 0 R', [N+3]));
{$IFDEF SG_HAS_WINAPI_INTERFACE}
      ExpCharWidths(FM, PF);
{$ENDIF}
      FImages.Add('>> endobj');
      Inc(FImgNum);
      FImages.NewDictObj(N+2, ' /Type /FontDescriptor');
      FImages.Add('/FontName /' + FM.Name);
      FImages.Add('/Flags 32');
      FImages.Add(FormatA('/CapHeight %d', [FM.CapHeight]));
      FImages.Add(FormatA('/Ascent %d', [FM.Ascent]));
      FImages.Add(FormatA('/Descent %d', [FM.Descent]));
      FImages.Add(FormatA('/ItalicAngle %d', [FM.ItalicAngle]));
      R := FM.Bounds;
      FImages.Add(FormatA('/FontBBox [%d %d %d %d]', [R.Left, R.Bottom, R.Right, R.Top]));
      FImages.Add('/StemV 1');
      if Assigned(FM.Data) then FImages.Add(FormatA('/FontFile2 %d 0 R', [N+4]));
    end;
    FImages.Add('>> endobj');
    if Assigned(FM.Data) then ExpFontData(PF, FM.Data, FM.DataSize, N+3);
  finally
    FM.Free;
  end;
end;

procedure TsgPDFExport.PolyPoints(Points: PPoint; Count: Integer);
var
  vCnt: Integer;
  PrevPt: TPoint;
  P: PPoint;
  S: AnsiChar;
  vTmpLW: Double;
  vLineCap, vLineJoin, vTmpPS: DWORD;

  procedure DecodeStyle(const AStyle: DWORD; var ALineCap,ALineJoin: DWORD);
  begin
    ALineCap := 1;
    ALineJoin := 1;
    if AStyle and PS_ENDCAP_FLAT <> 0 then
      ALineCap := 0;
    if AStyle and PS_ENDCAP_SQUARE <> 0 then
      ALineCap := 2;
    if AStyle and PS_JOIN_MITER <> 0 then
      ALineJoin := 0;
    if AStyle and PS_JOIN_BEVEL <> 0 then
      ALineJoin := 2;
  end;

begin
  vTmpLW := PenWidth;

  if FTmpLW <> vTmpLW then
  begin
    FData.Add(FormatA('%f w', [vTmpLW]));
    FTmpLW := vTmpLW;
  end;

  vTmpPS := PenStyle;

  if FTmpPS <> vTmpPS then
  begin
    DecodeStyle(vTmpPS, vLineCap, vLineJoin);
    FData.Add(FormatA('%d J', [vLineCap]));
    FData.Add(FormatA('%d j', [vLineJoin]));
    FTmpPS := vTmpPS;
  end;

  P := Points;
  PrevPt := Point(MaxInt, MaxInt);
  S := 'm';
  vCnt := Count;
  while vCnt > 0 do
  begin
    if (P^.X <> PrevPt.X) or (P^.Y <> PrevPt.Y) or (Count <= 2) then
    begin
      FData.Add(FormatA('%f %f %s', [FQuality * P^.X, FQuality * P^.Y, S]));
      PrevPt := P^;
    end;
    Inc(P);
    S := 'l';
    Dec(vCnt);
  end;
end;

procedure TsgPDFExport.ExpPolyline(Points: PPoint; Count: Integer);
begin
   SetLayer;
   PolyPoints(Points, Count);
   FData.Add('S');
   ResetLW;
end;

procedure TsgPDFExport.ExpPolygon(Points: PPoint; Count: Integer);
begin
  SetLayer;
  PolyPoints(Points, Count);
  FData.Add('f');
  FData.Add('s');
  ResetLW;
end;

procedure TsgPDFExport.ExpPolyPolyline(const Points; Counts: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger; Count: Integer);
var
  PP: PPoint;
  PC: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger;
  C: Integer;
begin
  PP := PPoint(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    ExpPolyline(PP, C);
    Inc(PP, C);
    Dec(Count);
  end;
end;

procedure TsgPDFExport.ExpPolyPolygon(const Points; Counts: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger; Count: Integer);
var
  PP: PPoint;
  PC: {$IFDEF SGFPC}objpas.{$ENDIF}PInteger;
  C: Integer;
begin
  if Count > 0 then
    SetLayer;
  PP := PPoint(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    PolyPoints(PP, C);
    FData.Add('h');
    Inc(PP, C);
    Dec(Count);
  end;
  FData.Add('f*');
  ResetLW;
end;

procedure TsgPDFExport.ProcRgn(P: PRect; Count: Integer);
begin
  if Count > 0 then
    SetLayer;
  while Count > 0 do
  begin
    FData.Add(FormatA('%f %f %f %f re',
                    [FQuality*P.Left, FQuality*P.Top, FQuality*(P.Right-P.Left), FQuality*(P.Bottom-P.Top)]));
    Inc(P);
    Dec(Count);
  end;
end;

procedure TsgPDFExport.ExpFillRgn(P: PRect; Count: Integer);
begin
  if Count > 0 then
  begin
    ProcRgn(P,Count);
    FData.Add('f');
  end;
end;

procedure TsgPDFExport.ExpClipRgn(P: PRect; Count: Integer);
begin
  if Count > 0 then
  begin
    ProcRgn(P,Count);
    FData.Add('W n');
  end;
end;

procedure TsgPDFExport.ExpSaveDC;
begin
  FData.Add('q');
end;

function TsgPDFExport.Flipping: Boolean;
begin
  Result := True;
end;

procedure TsgPDFExport.ExpRestoreDC;
begin
  FFillColor := clNone;
  FData.Add('Q');
end;

procedure TsgPDFExport.DoSetColor(AColor: TColor; const S: string);
var
  vColorPal: array[0..3] of Byte absolute AColor;
  C, M, Y, K: Double;
  vType: string;
begin
  case ColorSpace of
    clspCMYK:
      begin
        ConvertRGBtoCMYKFloat(vColorPal[0], vColorPal[1], vColorPal[2], C, M, Y, K);
        vType := cnstFillColorCMYK;
        if S = cnstStrokeColorRGB then
          vType := cnstStrokeColorCMYK;
        FData.Add(FormatA('%f %f %f %f %s', [C, M, Y, K, vType]));
      end;
  else
    FData.Add(FormatA('%f %f %f %s', [vColorPal[0]/255, vColorPal[1]/255, vColorPal[2]/255, S]));
  end;
end;

procedure TsgPDFExport.SetStrokeColor(AColor: TColor);
begin
  DoSetColor(AColor, cnstStrokeColorRGB);
end;

procedure TsgPDFExport.SetSubject(const Value: string);
begin
  FSubject := Value;
end;

procedure TsgPDFExport.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TsgPDFExport.SetColorSpace(const Value: TsgColorSpace);
begin
  FColorSpace := Value;
end;

procedure TsgPDFExport.SetCreator(const Value: string);
begin
  FCreator := Value;
end;

procedure TsgPDFExport.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TsgPDFExport.SetFillColor(AColor: TColor);
begin
  if AColor = FFillColor then Exit;
  FFillColor := AColor;
  DoSetColor(AColor, cnstFillColorRGB);
end;

procedure TsgPDFExport.SetKeywords(const Value: string);
begin
  FKeywords := Value;
end;

//0 - none
//1 - layers used
//2 - all
procedure TsgPDFExport.SetLayersMode(const Value: Integer);
begin
  FLayersMode := Value;
  if FLayersMode > 2 then
    FLayersMode := 0;
end;

procedure TsgPDFExport.DoDrawCurrentLayout;
var
  vScale: Double;
  vPlotPaperArea: TF2DRect;
begin
  if FPageBounds = nil then
    FPageBounds := TsgPDFPageBounds.Create(Self);
  FPageWidth := FPageBounds.PageSize.X;
  FPageHeight := FPageBounds.PageSize.Y;
  vScale := 1 / (MMPerPDFUnit * FQuality);
  vPlotPaperArea.TopLeft := Pt2XScalar(FPageBounds.PlotArea.TopLeft, vScale);
  vPlotPaperArea.BottomRight := Pt2XScalar(FPageBounds.PlotArea.BottomRight, vScale);
  FCurrentView.FitTo(vPlotPaperArea.Left, vPlotPaperArea.Top,
    vPlotPaperArea.Right - vPlotPaperArea.Left, vPlotPaperArea.Bottom - vPlotPaperArea.Top);
  FParams^.Matrix := FCurrentView.Matrix^;
  CADImage.DrawRectInt(GetExportRect);
end;

procedure TsgPDFExport.DoGetCharWidths(APDFFontMetrics: TObject; AFirst, ALast: Integer;
  var AWidths);
{$IFNDEF MSWINDOWS}
{$IFDEF SGFPC}
var
  I, vLen, vDefaultWidth: Integer;
  vChars: UnicodeString;
  vPWideChars: PWordArray;
  vS: string;
  vPositions: ArrayOfCharPosition;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetCharWidth32(TpdfFontMetrics(APDFFontMetrics).DC, AFirst, ALast, AWidths);
{$ELSE}
{$IFDEF SGFPC}
  vLen := ALast - AFirst + 1;
  if vLen <= 0 then Exit;
  if not Assigned(TpdfFontMetrics(APDFFontMetrics).FFreeFont) then
  begin
    vDefaultWidth := Abs(Round(TpdfFontMetrics(APDFFontMetrics).FFont.Height * 0.5));
    for I := 0 to vLen - 1 do
      PIntegerArray(@AWidths)^[I] := vDefaultWidth;
    Exit;
  end;
  SetLength(vChars, vLen);
  vPWideChars := PWordArray(Pointer(vChars));
  for I := 0 to vLen - 1 do
    vPWideChars^[I] := I + AFirst;
  vS := string(UTF8Encode(vChars));
  vPositions := TpdfFontMetrics(APDFFontMetrics).FFreeFont.CharsPosition(vS);
  for I := Low(vPositions) to High(vPositions) do
    PIntegerArray(@AWidths)^[I] := Round(vPositions[I].width);
{$ENDIF}
{$ENDIF}
end;

procedure TsgPDFExport.DoInitializeView(var AView: TFMatrix);
begin
  inherited DoInitializeView(AView);
  UpdateCurrentViewBox;
end;

function TsgPDFExport.SetLayer(AReset: Boolean = False): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  if AReset then
  begin
    if FLayerIndex > -1 then
      FData.Add('EMC');
    FLayerIndex := -1;
  end
  else
  begin
    vIndex := GetLayerIndex;
    if FLayerIndex <> vIndex then
    begin
      Result := True;
      if FLayerIndex > -1 then
        FData.Add('EMC');
      FLayerIndex := vIndex;
      if FLayerIndex > -1  then
        FData.Add(FormatA('/OC /oc%d BDC', [FLayerIndex]));
    end;
  end;
end;

procedure TsgPDFExport.SetMargin(const Value: Double);
begin
  FMargins.Left := Value;
  FMargins.Right := Value;
  FMargins.Top := Value;
  FMargins.Bottom := Value;
end;

procedure TsgPDFExport.SetMargins(const Value: TF2DRect);
begin
  FMargins := Value;
end;

procedure TsgPDFExport.SetParamsForImage(const AImage: TsgCADImage);
var
  vScale, vLayoutDist, vSizeDist: Double;
begin
  case GetImageType of
    efCgm:
      begin
        vLayoutDist := DistanceVector2D(FPageBounds.CurrentLayoutSize.X, FPageBounds.CurrentLayoutSize.Y);
        if vLayoutDist <> 0 then
        begin
          vSizeDist := DistanceVector2D(FPageBounds.PageSize.X, FPageBounds.PageSize.Y);
          vScale := vSizeDist / vLayoutDist;
          FLineWeightScaleByExport := vScale / 40;
        end
        else
          inherited SetParamsForImage(AImage);
      end;
  else
    inherited SetParamsForImage(AImage);
  end;
end;

procedure TsgPDFExport.SetPdfSizeAsExtents(const Value: Boolean);
begin
  FPdfSizeAsExtents := Value;
end;

procedure TsgPDFExport.SetPdfSizeAsExtentsKoef(const Value: Double);
begin
  FPdfSizeAsExtentsKoef := Value;
end;

procedure TsgPDFExport.SetProducer(const Value: string);
begin
  FProducer := Value;
end;

procedure TsgPDFExport.ResetLW;
begin
  if FTmpLW > 0 then
  begin
    FData.Add('0 w');
    FTmpLW := 0;
  end;
end;

procedure TsgPDFExport.ExpCloseFigure;
begin
  with FData.FStream do Size := Size-3;
  FData.Add('h');
  FData.Add('S');
end;

procedure TsgPDFExport.PutBytes(Ptr: Pointer; Count: Integer);
var P: PByte absolute Ptr;
begin
  while Count > 0 do begin
    FCompressor.PutByte(P^);
    Inc(P);
    Dec(Count);
  end;
end;

procedure TsgPDFExport.PutWords(Ptr: Pointer; Count,Sh: Integer);
var
  P: PWord absolute Ptr;
  Fill: Boolean;
begin
  Fill := Count and 1 <> 0;
  while Count > 0 do begin
    FCompressor.PutChar(P^ shr (Sh+5));
    FCompressor.PutChar(P^ shr Sh);
    FCompressor.PutChar(P^ shr 1);
    Inc(P);
    Dec(Count);
  end;
  if Fill then FCompressor.PutChar(0);
end;

procedure TsgPDFExport.PutPalette(Ptr: Pointer; Count: Integer);
var
  P: PByte absolute Ptr;
  S: AnsiString;
  Mask: Byte;
begin
  FImages.Add(FormatA('/ColorSpace [/Indexed /%s %d', [GetColorSpaceStr(clspRGB), Count-1]));
  if Count > 16 then Mask := 15 else Mask := 3;
  S := '<';
  while Count > 0 do
  begin
    if Count and Mask = 0 then
    begin
      FImages.Add(S);
      S := '';
    end;
    S := S + ' ';
    PutRGB(S,P);
    Inc(P,4);
    Dec(Count);
  end;
  FImages.Add(S);
  FImages.Add('>]');
end;

procedure TsgPDFExport.PutBinRGB(const AColor: TColor);
begin
  PutBytes(@AColor, 3);
end;

procedure TsgPDFExport.ExpImageUV(const APoint1, APoint2, APoint3,  APoint4: TPoint;
  AImage: TPersistent);
const
  BPCs: array[{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat] of Byte = (0,1,4,8,4,4,8,8,0);
var
  I,W,X,Y: Integer;
  P: Pointer;
  S: string;
  BM: TsgBMAdapter;
  Pal: array[Byte] of Integer;
  vDX, vDY: TPoint;
  vMatrix: TFMatrix;
  vGraphicSize: TSize;
  vPixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
  vMap: TsgMap;

  function WrapGraphicFromBitmapAdapter(ABMAdapter: TsgBMAdapter; out AMap: TsgMap): Boolean;
  begin
    AMap := nil;
    if TsgBMAdapterAccess(ABMAdapter).IsSG then
      AMap := TsgMap.Wrap(ABMAdapter.Width, ABMAdapter.Height, vPixelFormat,
       TsgBitmap(TsgBMAdapterAccess(ABMAdapter).Source).RowData, nil, 0)
    else
      if TsgBMAdapterAccess(ABMAdapter).IsBM then
        AMap := TsgMap.Wrap(ABMAdapter.Bitmap, True);
    Result := Assigned(AMap);
  end;

begin
  vGraphicSize := GetSizeGraphic(AImage);
  if (vGraphicSize.cx < 6) or (vGraphicSize.cy < 6) then
    Exit;
  if IsGreatImage(AImage) then
    Exit;
  SetLayer;
  vDX := SubPoint(APoint4, APoint1);
  vDY := SubPoint(APoint2, APoint1);
  if (vDX.X = vDX.Y) or (vDY.X = vDY.Y) then
    vMatrix := cnstIdentityMat
  else
    vMatrix := FMat2DByImage(MakeFPointFromPoint(APoint1),
      MakeFPointFromPoint(vDX), MakeFPointFromPoint(vDY), 1, 1);
  BM := TsgBMAdapterAccess(TsgBMAdapter.Create);
  try
    BM.Assign(AImage);
    BM.HandleType := bmDIB;
    I := BPCs[BM.PixelFormat];
    if I=0 then Exit;
    if (BM.Width < 1000) and (BM.Height < 1000) then
      BM.ROP := SRCCOPY;	// if ???
    S := IntToStr(FImgNum);
    FResources.Add(FormatA(' /Im%s %s 0 R', [S,S]));
    FImages.NewDictObj(FImgNum, FormatA(' /Type /XObject /Subtype /Image /Width %d /Height %d',
      [BM.Width, BM.Height]));
    FImages.Add(FormatA('/BitsPerComponent %d', [I]));
    if BM.PixelFormat > pf8bit then I := I*3;
    FImages.FBufferStream.Capacity := BM.Height * (I*BM.Width shr 3 + 1);
    FCompressor.FIndex := 0;
    I := BM.GetColorTable(@Pal);
    if I > 0 then
      PutPalette(@Pal, I)
    else
      FImages.Add('/ColorSpace /' + GetColorSpaceStr(clspRGB));
    FImages.BeginStream;
    W := BM.Width;
    vPixelFormat := BM.PixelFormat;
    if vPixelFormat in [pf24bit, pf32bit] then
    begin
      if WrapGraphicFromBitmapAdapter(BM, vMap) then
      try
        for Y := 0 to vMap.Height - 1 do
          for X := 0 to vMap.Width - 1 do
            PutBinRGB(vMap.Pixels[X, Y]);
      finally
        TsgMap.Unwrap(vMap);
      end;
    end
    else
      for I := 0 to BM.Height - 1 do
      begin
        P := BM.ScanLine[I];
        case vPixelFormat of
          pf1bit:         PutBytes(P, (W+7) shr 3);
          pf4bit:         PutBytes(P, (W+1) shr 1);
          pf8bit:         PutBytes(P,W);
          pf15bit:        PutWords(P,W,6);
          pf16bit:        PutWords(P,W,7);
        end;
      end;
    FImages.FStream.Size := (FCompressor.FIndex + 1) shr 1;
    FImages.EndStream;
    FData.Add('q');
    FData.Add(FormatA('%f %f %f %f %f %f cm',
                    [FQuality * vMatrix.V1[0], FQuality * vMatrix.V1[1],
                     FQuality * vMatrix.V2[0], FQuality * vMatrix.V2[1],
                     FQuality * vMatrix.V4[0], FQuality * vMatrix.V4[1]]));
    FData.Add(FormatA('/Im%s Do', [S]));
    FData.Add('Q');
    Inc(FImgNum);
  finally
    BM.Free;
  end;
end;

procedure TsgPDFExport.ExpSetFont(AFont: TFont);
begin
  FCurrentFont := GetPdfFont(AFont);
  FCurrentFont.Height := {$IFDEF SG_FIREMONKEY}Ceil({$ELSE}({$ENDIF}Abs(AFont.Height));
end;

procedure TsgPDFExport.ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
  const ATextW: WideString; const ATextParams: PsgExpTextParam);
var
  S: WideString;
  M: TFMatrix;
  vHeight: Double;
begin
  if FCurrentFont=nil then Exit;
  S := ATextW;
  if S = '' then S :=  WideString(ATextA);
  if S = '' then Exit;
  SetLayer;
  M := FMatScale(ATextParams.Draw,
    ReverseScale(ExtractMatrixAbsoluteScale(ATextParams.Draw)));
  vHeight := ATextParams.Height/(ATextParams.HKoef * DistanceFVector(M.EY));
  FData.Add(FormatA('BT %s %f Tf', [FCurrentFont.Name, FQuality*vHeight*1.3985]));
  FData.Add(FormatA('%f %f %f %f %f %f Tm', [M.EX.X, M.EX.Y, M.EY.X, M.EY.Y,
    FQuality*X, FQuality*Y]));
  FData.Add(FCurrentFont.HexUnicode(S) + ' Tj ET');
end;

procedure TsgPDFExport.ExpImage(const R: TRect; AImage: TPersistent);
begin
  ExpImageUV(R.TopLeft, Point(R.Right,R.Top),R.BottomRight,
    Point(R.Left,R.Bottom), AImage);
end;

procedure TsgPDFExport.ExpAnnotation(AEnt: TObject; const AParams: TsgExpAnnotParam);
var
  T: TsgDXFText absolute AEnt;
  S: WideString;
begin
  if (AParams.Subtype <> atSquare) or not (AEnt is TsgDXFText)
  then Exit;
  FAnnots := FAnnots + FormatA(' %d 0 R', [FImgNum]);
  FAnnotsData.NewDictObj(FImgNum, ' /Subtype /Square /F 64 /Border [0 0 0]');
  FAnnotsData.Add('/T ' + EscapeStr(AParams.Title));
  with AParams.Rect
    do FAnnotsData.Add(FormatA('/Rect [%f %f %f %f]', [FQuality*Left,FQuality*Bottom,FQuality*Right,FQuality*Top]));
  S := T.UnicodeText;
  if S = '' then S := T.Text;
  FAnnotsData.Add('/Contents ' + HexUnicode(S,'FEFF'));
  FAnnotsData.EndDictObj;
  Inc(FImgNum);
end;

procedure TsgPDFExport.ExportFonts;
var I: Integer;
begin
  FResources.Add('/Font');
  FResources.Add('<<');
  for I := 0 to FFontCache.Count-1
  do ExpPdfFont(FFontCache[I]);
  FResources.Add('>>');
end;

procedure TsgPDFExport.PageStart(N: Integer);
var
  R: TRect;
begin
  if FPageBounds = nil then
    FPageBounds := TsgPDFPageBounds.Create(Self);
  inherited PageStart(N);
  SetLayer(True);
  FNotPages := False;
  FTitleList.Add(CurrentLayout.Name);
  if Assigned(FLogEvent) then
    FLogEvent(MakeFPointFrom2D(FPageBounds.CurrentLayoutSize),
      MakeFPointFrom2D(FPageBounds.PageSize),
      CurrentLayout.Name);
  FAnnots := '';
  N := N shl 1 + cnstStartCount;
  FData.NewDictObj(N, ' /Type /Page');
  FData.Add(FormatA('/Parent %d 0 R', [cnstStartCount - 2]));
  FData.Add(FormatA('/Contents %d 0 R', [N+1]));
  FData.Add(FormatA('/Resources %d 0 R', [LayoutsCount*2+cnstStartCount]));
//  if FColorSpace = clspCMYK then
//     FData.Add('/ColorSpace ' + GetColorSpaceStr(clspCMYK));
  FData.EndDictObj;
  FData.NewDictObj(N+1);
  FTmpLW := -1;
  FTmpML := -1;
  FTmpPS := $FFFF;
  FFillColor := clNone;
  FData.BeginStream;
  if BackgroundColor <> clWhite then
  begin
    R.Left := 0;
    R.Top := 0;
    R.Right := Round((FPageBounds.PageSize.X / MMPerPDFUnit) / FQuality);
    R.Bottom := Round((FPageBounds.PageSize.Y / MMPerPDFUnit) / FQuality);
    SetFillColor({$IFDEF SG_FIREMONKEY}TColorRec.ColorToRGB {$ELSE}ColorToRGB{$ENDIF}(BackgroundColor));
    ExpFillRgn(@R, 1);
  end;
end;

procedure TsgPDFExport.PageEnd(N: Integer);
begin
  FreeAndNil(FPageBounds);
  SetLayer(True);
  FData.EndStream;
  if FAnnots <> '' then
    FData.Insert(FData.LastIndexPdfObject -1, 6, '/Annots [' + FAnnots + ' ]');
  inherited PageEnd(N);
end;

procedure TsgPDFExport.SaveToStreamCustom(S: TStream);
var
  I,Len,Cnt: Integer;
  DS: Char;
  vTemp: AnsiString;
  vTempAnsi: PAnsiChar;
  vCADImageTTFMode: TsgTTFMode;
  vBegin: TsgOutlineNode;
  vOutlines, OldData, vPDFLayers: TpdfStrings;
  vCollection: TsgPDFCollection;
  vSecLayers: TsgDXFGroup;
  vLayersArr, vLayersArrOC: AnsiString;
begin
  vPDFLayers := nil;
  FLayerIndex := -1;
  FreeObjectStringList(FLayers);
  //UseOutlines := True;
  vOutlines := nil;
  if UseOutlines then
  begin
    vOutlines := TpdfStrings.Create;
    vOutlines.Clear;
  end;
  FPageRect.Clear;
  FData.Clear;
  FResources.Clear;
  FAnnotsData.Clear;
  FXRefs.Clear;
  FImages.Clear;
  ClearFontCache;
  FImgNum := LayoutsCount*2+cnstStartCount;
  FData.NewBlock;
  for I := Low(PDFHead) to High(PDFHead) do
  begin
    if I = 7 then // Next object
      FData.NewBlock;
    FData.Add(PDFHead[i]);
  end;
  vTemp := '[';
  for I:=0 to LayoutsCount-1 do
    vTemp := vTemp + FormatA(' %d 0 R', [I+I+cnstStartCount]);
  vTemp := vTemp + ' ]';
  FData.Add(vTemp);
  FData.Add(FormatA('/Count %d', [LayoutsCount]));
  FData.EndDictObj;
  FData.NewDictObj(3);
  FData.Add(FormatA('/Title(%s)', [FTitle]));
  FData.Add(FormatA('/Author(%s)', [FAuthor]));
  FData.Add(FormatA('/Creator(%s)', [FCreator]));
  FData.Add(FormatA('/Producer(%s)', [FProducer]));
  FData.Add(FormatA('/Subject(%s)', [FSubject]));
  FData.Add(FormatA('/Keywords(%s)', [FKeywords]));
  FData.EndDictObj;
  FResources.NewDictObj(FImgNum);
  FResources.Add('/XObject');
  FResources.Add('<<');
  Inc(FImgNum);

  if FLayersMode > 0 then
  begin
    FLayers := TStringList.Create;
    if FLayersMode > 1 then
    begin
      vSecLayers := Self.CADImage.Converter.Sections[csLayers];
      for I := 0 to vSecLayers.Count - 1 do
        FLayers.AddObject(vSecLayers.Entities[I].Name, TsgObjectWithField.CreateInt(I + 1));
    end;
    TStringList(FLayers).Sorted := True;
  end;

  DS := SetDecimalSeparator('.');
  vCADImageTTFMode := Self.CADImage.TTFMode;
  try
    if FBitmapExportMode <> bemNone then
      Self.CADImage.TTFMode := ttfAuto
    else
      if ExportTexts then
        Self.CADImage.TTFMode := ttfGDI
      else
        Self.CADImage.TTFMode := ttfPolyPolygon;
    inherited SaveToStreamCustom(S);
  finally
    Self.CADImage.TTFMode := vCADImageTTFMode;
    SetDecimalSeparator(DS);
    SetLayer(True);
  end;

  FResources.Add('>>');
  if Assigned(FLayers) and (FLayers.Count > 0) then
  begin
    vLayersArr := '[';
    vLayersArrOC := '';
    vPDFLayers := TpdfStrings.Create;
    for I := 0 to FLayers.Count - 1 do
    begin
      vPDFLayers.NewDictObj(FImgNum);
      vLayersArr := vLayersArr + AnsiString(IntToStr(FImgNum)) + ' 0 R ';
      vLayersArrOC := vLayersArrOC + FormatA('/oc%d %d 0 R ', [TsgObjectInt64(FLayers.Objects[I]).FieldInt, FImgNum]);
      Inc(FImgNum);
      vPDFLayers.Add('/Type /OCG /Name ' + HexUnicode(FLayers[I],'FEFF'));
      vPDFLayers.EndDictObj;
    end;
    vLayersArr := vLayersArr + ']';
    FData.Insert(0, 5 , '>>');
    FData.Insert(0, 5 , '<<  /OCGs' + vLayersArr + ' /D <</Order ' + vLayersArr + ' /OFF [] >>');
    FData.Insert(0, 5 , '/OCProperties');

    FResources.Add('/Properties');
    FResources.Add('<<' + vLayersArrOC + '>>');
  end;

  ExportFonts;
  FResources.EndDictObj;

  if UseOutlines then
  begin
    FData.Insert(0, 5 , '/PageMode /UseOutlines');
    FData.Insert(0, 5 , '/Outlines ' + FormatA('%d',[FImgNum]) + ' 0 R');
    OldData := FData;
    try
      FData := vOutlines;
      FData.NewBlock;
      CreateOutlines;
      for I := 0 to LayoutsCount-1 do
      begin
        vBegin := OutlineRoot.Add(nil, FTitleList[I],
          TsgPDFAction.Create(Self,I+I+cnstStartCount,Round(FPageRect[I].Y)));
        vBegin.Expanded := True;
      end;
      OutlineRoot.Save;
    finally
      FData := OldData;
    end;
  end;

  FXRefs.NewBlock;
  FXRefs.Add('xref');
  FXRefs.Add('');
  FXRefs.Add('0000000000 65535 f');
  Len := 0;
  Cnt := 0;

  vCollection := TsgPDFCollection.Create;
  try
    vCollection.Add(FData);
    vCollection.Add(FResources);
    vCollection.Add(FAnnotsData);
    vCollection.Add(FImages);
    if UseOutlines then
      vCollection.Add(vOutlines);
    if Assigned(vPDFLayers) then
      vCollection.Add(vPDFLayers);
    vCollection.Sort;
    vCollection.AddXref(FXRefs, Len, Cnt);

    FXRefs.Strings[FXRefs.LastIndexPdfObject,FXRefs.CountContent[FXRefs.LastIndexPdfObject] - Cnt-2] := FormatA('0 %d', [Cnt+1]);
    FXRefs.AddStringsArray(PDFFoot);
    FXRefs.Strings[FXRefs.LastIndexPdfObject,FXRefs.CountContent[FXRefs.LastIndexPdfObject] - 6] := FormatA('<< /Size %d', [Cnt+1]);
    FXRefs.Strings[FXRefs.LastIndexPdfObject,FXRefs.CountContent[FXRefs.LastIndexPdfObject] - 1] := AnsiString(IntToStr(Len - 1));
    vCollection.SaveToStream(S);
    if UseOutlines then
    begin
      vOutlines.Free;
    end;
  finally
    vCollection.Free;
  end;
  vPDFLayers.Free;

  FXRefs.SaveToStream(S);
  vTempAnsi := '%%EOF';
  S.Write(vTempAnsi^, Length(vTempAnsi));
end;

{ Util}

function UnicodeChar( Text:string):AnsiString;
{$IFDEF HAS_FEATURE_ENCODING}
var
  Preamble, Bytes: TBytes;
begin
{$IFDEF UNICODE}
  Bytes := TEncoding.Unicode.GetBytes(Text);
{$ELSE}
  Bytes := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, BytesOf(RawByteString(Text)));
{$ENDIF}
  Preamble := TEncoding.Unicode.GetPreamble;
  SetLength(Result, Length(Bytes) + Length(Preamble));
  System.Move(Preamble[0], Result[1], Length(Preamble));
  System.Move(Bytes[0], Result[Length(Preamble) + 1], Length(Bytes));
end;
{$ELSE}
var
  i, L: integer;
begin
  L := Length( Text );
  SetLength(Result,(L + 1) shl 1);
  Result[1] := AnsiChar($FE);
  Result[2] := AnsiChar($FF);
  for i:= 1 to L  do
  begin
    Result[1+i shl 1] := AnsiChar(Word(Text[i]) shr 8);
    Result[2+i shl 1] := AnsiChar(Word(Text[i]) and $FF);
  end;
end;
{$ENDIF}

{ TsgPDFObject }

constructor TsgPDFObject.Create(AKernel: TsgPDFExport);
begin
  FKernel := AKernel;
  FIndex := FKernel.FImgNum;
  FPosition := FKernel.FData.NewDictObj(FIndex);
  Inc(FKernel.FImgNum);
end;

function TsgPDFObject.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TsgPDFObject.GetRefIndex: AnsiString;
begin
  Result := FormatA('%d',[PdfObjectIndex]) + ' 0 R';
end;


{ TPDFOutlines }

function TsgOutlineRoot.Add ( Node: TsgOutlineNode ): TsgOutlineNode;
var
  N, T, M: TsgOutlineNode;
  I: Integer;
begin
  N := TsgOutlineNode.Create ( Kernel, Self );
  if Node <> nil then
    T := Node.FParent
  else
    T := nil;
  N.FParent := T;
  N.FNext := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TsgOutlineNode ( FList [ I ] ).FParent = T ) and ( TsgOutlineNode ( FList [ I ] ).FNext = nil ) then
    begin
      M := TsgOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FNext := N;
  N.FPrev := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TsgOutlineRoot.AddChild ( Node: TsgOutlineNode ): TsgOutlineNode;
var
  N, T, M: TsgOutlineNode;
  I: Integer;
begin
  N := TsgOutlineNode.Create ( Kernel, Self );
  T := Node;
  N.FParent := T;
  N.FNext := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TsgOutlineNode ( FList [ I ] ).FParent = T ) and ( TsgOutlineNode ( FList [ I ] ).FNext = nil ) then
    begin
      M := TsgOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FNext := N;
  N.FPrev := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TsgOutlineRoot.AddChildFirst (
  Node: TsgOutlineNode ): TsgOutlineNode;
var
  N, T, M: TsgOutlineNode;
  I: Integer;
begin
  N := TsgOutlineNode.Create ( Kernel, Self );
  T := Node;
  N.FParent := T;
  N.FPrev := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TsgOutlineNode ( FList [ I ] ).FParent = T ) and ( TsgOutlineNode ( FList [ I ] ).FPrev = nil ) then
    begin
      M := TsgOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FPrev := N;
  N.FNext := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TsgOutlineRoot.AddFirst ( Node: TsgOutlineNode ): TsgOutlineNode;
var
  N, T, M: TsgOutlineNode;
  I: Integer;
begin
  N := TsgOutlineNode.Create ( Kernel,Self );
  if Node <> nil then
    T := Node.FParent
  else
    T := nil;
  N.FParent := T;
  N.FPrev := nil;
  M := nil;
  for I := 0 to FList.Count - 1 do
    if ( TsgOutlineNode ( FList [ I ] ).FParent = T ) and ( TsgOutlineNode ( FList [ I ] ).FPrev = nil ) then
    begin
      M := TsgOutlineNode ( FList [ I ] );
      Break;
    end;
  if M <> nil then
    M.FPrev := N;
  N.FNext := M;
  FList.Add ( Pointer ( N ) );
  if T <> nil then
    T.FChild.Add ( Pointer ( N ) );
  Result := N;
end;


procedure TsgOutlineRoot.Clear;
begin
  while FList.Count <> 0 do
    TsgOutlineNode ( FList [ 0 ] ).Delete;
end;

constructor TsgOutlineRoot.Create (AKernel: TsgPDFExport);
begin
  inherited Create(AKernel);
  FList := TList.Create;
end;

procedure TsgOutlineRoot.Delete ( Node: TsgOutlineNode );
begin
  Node.Delete;
end;

destructor TsgOutlineRoot.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TsgOutlineRoot.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TsgOutlineRoot.GetFirstNode: TsgOutlineNode;
begin
  if FList.Count <> 0 then
    Result := TsgOutlineNode ( FList [ 0 ] )
  else
    Result := nil;
end;

function TsgOutlineRoot.GetItem ( Index: Integer ): TsgOutlineNode;
begin
  Result := TsgOutlineNode ( FList [ Index ] );
end;

function TsgOutlineRoot.Insert ( Node: TsgOutlineNode ): TsgOutlineNode;
var
  N, Ne: TsgOutlineNode;
begin
  if Node = nil then
  begin
    Result := Add ( nil );
    Exit;
  end;
  N := TsgOutlineNode.Create ( Kernel, Self );
  Ne := Node.FNext;
  N.FParent := Node.FParent;
  N.FPrev := Node;
  N.FNext := Node.FNext;
  Node.FNext := N;
  if Ne <> nil then
    Ne.FPrev := N;
  FList.Add ( Pointer ( N ) );
  if N.FParent <> nil then
    N.FParent.FChild.Add ( Pointer ( N ) );
  Result := N;
end;

function TsgOutlineRoot.Add ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode;
begin
  Result := Add ( Node );
  Result.FTitle := Title;
  Result.FAction := Action;
end;

function TsgOutlineRoot.AddChild ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode;
begin
  Result := AddChild ( Node );
  Result.FTitle := Title;
  Result.FAction := Action;
end;

function TsgOutlineRoot.AddChildFirst ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode;
begin
  Result := AddChildFirst ( Node );
  Result.FTitle := Title;
  Result.FAction := Action;
end;

function TsgOutlineRoot.AddFirst ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode;
begin
  Result := AddFirst ( Node );
  Result.FTitle := Title;
  Result.FAction := Action;
end;

function TsgOutlineRoot.Insert ( Node: TsgOutlineNode; Title: string; Action: TsgPDFAction): TsgOutlineNode;
begin
  Result := Insert ( Node );
  Result.FTitle := Title;
  Result.FAction := Action;
end;

procedure TsgOutlineRoot.Save;
var
  i: Integer;
begin
  if Count = 0 then
    Exit;
  for i := 0 to Count - 1 do
    TsgOutlineNode(FList [ i ]).Save;

  Kernel.FData.Add(Position, '/Type /Outlines' );
  Kernel.FData.Add(Position, '/Count ' +  FormatA('%d',[Count]));
  for i := 0 to Count - 1 do
  begin
    if ( TsgOutlineNode(FList [ i ]).FParent = nil ) and ( TsgOutlineNode(FList [ i ]).FPrev = nil ) then
      Kernel.FData.Add(Position, '/First ' + TsgOutlineNode(FList [ i ]).RefPdfObjectIndex );
    if ( TsgOutlineNode(FList [ i ]).FParent = nil ) and ( TsgOutlineNode(FList [ i ]).FNext = nil ) then
      Kernel.FData.Add(Position, '/Last ' + TsgOutlineNode(FList [ i ]).RefPdfObjectIndex );
  end;
  Kernel.FData.EndDictObj(Position);
end;

{ TPDFOutlineNode }

constructor TsgOutlineNode.Create (AKernel: TsgPDFExport; AOwner: TsgOutlineRoot );
begin
  inherited Create (AKernel);
  FOwner := AOwner;
  FChild := TList.Create;
end;

procedure TsgOutlineNode.Delete;
var
  I: Integer;
  P, N: TsgOutlineNode;
begin
  DeleteChildren;
  P := GetPrev;
  N := GetNext;
  if P <> nil then
    P.FNext := N;
  if N <> nil then
    N.FPrev := P;
  I := FOwner.FList.IndexOf ( Pointer ( Self ) );
  if I <> -1 then
    FOwner.FList.Delete ( I );
  if FParent <> nil then
  begin
    I := FParent.FChild.IndexOf ( Pointer ( Self ) );
    if I <> -1 then
      FParent.FChild.Delete ( I );
  end;
  Free;
end;

procedure TsgOutlineNode.DeleteChildren;
begin
  while FChild.Count <> 0 do
    TsgOutlineNode ( FChild [ 0 ] ).Delete;
end;

destructor TsgOutlineNode.Destroy;
begin
  FChild.Free;
  inherited;
end;


function TsgOutlineNode.GetCount: Integer;
begin
  Result := FChild.Count;
end;


function TsgOutlineNode.GetFirstChild: TsgOutlineNode;
var
  I: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit;
  for I := 0 to FChild.Count - 1 do
    if TsgOutlineNode ( FChild [ I ] ).FPrev = nil then
    begin
      Result := TsgOutlineNode ( FChild [ I ] );
      Exit;
    end;
end;

function TsgOutlineNode.GetHasChildren: Boolean;
begin
  Result := Count <> 0;
end;

function TsgOutlineNode.GetItem ( Index: Integer ): TsgOutlineNode;
begin
  Result := TsgOutlineNode ( FChild [ Index ] );
end;

function TsgOutlineNode.GetLastChild: TsgOutlineNode;
var
  I: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit;
  for I := 0 to FChild.Count - 1 do
    if TsgOutlineNode ( FChild [ I ] ).FNext = nil then
    begin
      Result := TsgOutlineNode ( FChild [ I ] );
      Exit;
    end;
end;

function TsgOutlineNode.GetNext: TsgOutlineNode;
var
  I: Integer;
begin
  I := FOwner.FList.IndexOf ( Self );
  if I <> FOwner.FList.Count - 1 then
    Result := FOwner [ i + 1 ]
  else
    Result := nil;
end;

function TsgOutlineNode.GetNextChild (
  Node: TsgOutlineNode ): TsgOutlineNode;
var
  i: Integer;
begin
  i := FChild.IndexOf ( Pointer ( Node ) );
  if ( i = -1 ) or ( i = FChild.Count - 1 ) then
    Result := nil
  else
    Result := TsgOutlineNode ( FChild [ i + 1 ] );
end;

function TsgOutlineNode.GetNextSibling: TsgOutlineNode;
begin
  Result := FNext;
end;

function TsgOutlineNode.GetPrev: TsgOutlineNode;
var
  I: Integer;
begin
  I := FOwner.FList.IndexOf ( Self );
  if I <> 0 then
    Result := FOwner [ i - 1 ]
  else
    Result := nil;
end;

function TsgOutlineNode.GetPrevChild (
  Node: TsgOutlineNode ): TsgOutlineNode;
var
  i: Integer;
begin
  i := FChild.IndexOf ( Pointer ( Node ) );
  if ( i = -1 ) or ( i = 0 ) then
    Result := nil
  else
    Result := TsgOutlineNode ( FChild [ i - 1 ] );
end;

function TsgOutlineNode.GetPrevSibling: TsgOutlineNode;
begin
  Result := FPrev;
end;

procedure TsgOutlineNode.Save;
var
  I: Integer;
begin
  Kernel.FData.Add(Position, '/Title ' + '(' +UnicodeChar ( FTitle )  +')');
  I := 0;
  if fsbold in Style then
    I := I or 2;
  if fsItalic in Style then
    I := I or 1;
  if I <> 0 then
    Kernel.FData.Add(Position,  '/F ' + FormatA('%d',[I]) );

  if FChild.Count <> 0 then
  begin
    if FExpanded then
      Kernel.FData.Add(Position, '/Count ' + FormatA('%d',[FChild.Count]) )
    else
      Kernel.FData.Add(Position,'/Count -' + FormatA('%d',[FChild.Count]) );
    Kernel.FData.Add(Position, '/First ' + GetFirstChild.RefPdfObjectIndex );
    Kernel.FData.Add(Position, '/Last ' + GetLastChild.RefPdfObjectIndex );
  end;
  if FParent = nil then
    Kernel.FData.Add(Position,'/Parent ' + FOwner.RefPdfObjectIndex )
  else
    Kernel.FData.Add(Position, '/Parent ' + FParent.RefPdfObjectIndex );
  if FNext <> nil then
    Kernel.FData.Add(Position, '/Next ' + FNext.RefPdfObjectIndex );
  if FPrev <> nil then
    Kernel.FData.Add(Position, '/Prev ' + FPrev.RefPdfObjectIndex );
  if FAction <> nil then
  begin
    Kernel.FData.Add(Position, '/A ' + FAction.RefPdfObjectIndex );
    FAction.Save;
  end;
  Kernel.FData.EndDictObj(Position);
end;

procedure TsgOutlineNode.SetExpanded ( const Value: Boolean );
begin
  FExpanded := Value;
end;

{ TsgPDFAction }

constructor TsgPDFAction.Create( AKernel: TsgPDFExport; PageIndex,TopOffset:Integer);
begin
  inherited Create(AKernel);
  FPageIndex := PageIndex;
  FTopOffset := TopOffset;
end;

procedure TsgPDFAction.Save;
begin
  Kernel.FData.Add(Position, '/S /GoTo /D [' +
   FormatA('%d',[FPageIndex]) + ' 0 R' +
      '/FitH ' + FormatA('%d',[FTopOffset]) + ']' );
  Kernel.FData.EndDictObj(Position);
end;

{ TsgPDFCollection }

type
{$IFDEF SG_ObjStr}
  TsgDataCollection = record
    TS: TStrings;
    Count: Integer;
  end;
  PsgDataCollection = ^TsgDataCollection;
{$ELSE}
  TsgAnsiStringArray =  array of AnsiString;
  PsgAnsiStringArray = ^TsgAnsiStringArray;
  TsgDataCollection = record
    PSA: PsgAnsiStringArray;
    Count: Integer;
  end;
  PsgDataCollection = ^TsgDataCollection;
{$ENDIF}

constructor TsgPDFCollection.Create;
begin
  inherited Create;
  FCollection := TsgCollection.Create;
  FCollection.Sorted := False;
end;

destructor TsgPDFCollection.Destroy;
var
  I: Integer;
  vData: PsgDataCollection;
begin
  for I := 0 to FCollection.Count - 1 do
  begin
    vData := PsgDataCollection(FCollection.Items[I].Data);
    {$IFDEF SG_ObjStr}
    vData^.TS := nil;
    {$ENDIF}
    Dispose(vData);
  end;
  FCollection.Free;
  inherited Destroy;
end;

procedure TsgPDFCollection.Add(const AData: TpdfStrings);
var
  I,J,L,vIndex, LL: Integer;
  S: AnsiString;
  vData: PsgDataCollection;
begin
  for I := 0 to AData.CountPdfObject - 1 do
  begin
    for J := 0 to AData.CountContent[I] -1 do
    begin
      S := AData.Strings[I,J];
      if S = 'xref' then Exit;
      L := Length(S);
//      if (I = 0) and (J = 0) then
//      begin
//        if (L = 8) and (Copy(S,L-7,8) = '%PDF-1.5') then
//        begin
//          New(vData);
//          vData^.PSA := @AData.FItems[I];
//          vData^.Count := 1;
//          FCollection.Add(0, vData);
//          Continue;
//        end;
//      end;
      if (L > 6) and (Copy(S,L-5,6) = ' 0 obj') then
      begin
        LL := StringPosA(' 0 obj',S, 0);
        vIndex := StrToInt(StringTrimLeftRight(Copy(string(S), 0, LL)));
        New(vData);
        {$IFDEF SG_ObjStr}
        vData^.TS := TStrings(AData.FObjItems[I]);
        {$ELSE}
        vData^.PSA := @AData.FItems[I];
        {$ENDIF}
        vData^.Count := AData.CountContent[I];
        FCollection.Add(vIndex, vData);
        Continue;
      end;
    end;
  end;
end;

procedure TsgPDFCollection.AddXref(const ABlockLast: TpdfStrings; var Len, Cnt: Integer);
var
  I,J,L: Integer;
  vData: PsgDataCollection;
  S: AnsiString;
begin
  for I := 0 to FCollection.Count - 1 do
  begin
    vData := PsgDataCollection(FCollection.Items[I].Data);
    for J := 0 to vData^.Count -1  do
    begin
      {$IFDEF SG_ObjStr}
      S := pdfUniToAnsi(vData^.TS[J]);
      {$ELSE}
      S := PsgAnsiStringArray(vData^.PSA)^[J];
      {$ENDIF}
      if S = 'xref' then Exit;
      L := Length(S);
      if (L > 6) and (Copy(S,L-5,6) = ' 0 obj') then
      begin
        ABlockLast.Add(FormatA('%.10d 00000 n', [Len - 1]));
        Inc(Cnt);
      end;
      Inc(Len, L + Length(sPDFLineBreak));
    end;
  end;
end;

procedure TsgPDFCollection.SaveToStream(Stream: TStream);
var
  I,J: Integer;
  vBuffer, vStr: AnsiString;
  vData: PsgDataCollection;
begin
  vBuffer := '';
  for I := 0 to FCollection.Count - 1 do
  begin
    vData := PsgDataCollection(FCollection.Items[I].Data);
    for J := 0 to vData^.Count - 1 do
    begin
      {$IFDEF SG_ObjStr}
      vStr := pdfUniToAnsi(vData^.TS[J]);
      {$ELSE}
      vStr := PsgAnsiStringArray(vData^.PSA)^[J];
      {$ENDIF}
      vBuffer := vBuffer + vStr + sPDFLineBreak;
      if Length(vBuffer) > cnstDroppingSize then
      begin
        Stream.Write(PByte(@vBuffer[1])^, Length(vBuffer));
        vBuffer := '';
      end;
    end;
  end;
  if Length(vBuffer) > 0 then
    Stream.Write(PByte(@vBuffer[1])^, Length(vBuffer));
end;

procedure TsgPDFCollection.Sort;
begin
  FCollection.Sort;
end;

initialization

{$IFDEF SG_HAS_WINAPI_INTERFACE}
  FontSub := LoadLibrary('FontSub');
  if FontSub <> 0 then
    CreateFontPackage := GetProcAddress(FontSub, 'CreateFontPackage');
{$ENDIF}

finalization

{$IFDEF SG_HAS_WINAPI_INTERFACE}
  if FontSub <> 0 then
    FreeLibrary(FontSub);
{$ENDIF}

end.


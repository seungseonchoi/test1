{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   DWG files TGraphic class                 }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools�                        }
{************************************************************}

unit DWG;
{$INCLUDE SGDXF.inc}
{.$DEFINE SG_DWG_IMPORT_ACAD_TABLE}
{$IFDEF SGDEL_2009}

{$IFNDEF SG_FIREMONKEY}
  {$DEFINE SG_INCLUDE_PNG}
{$ENDIF}

{$ENDIF}
interface

uses

{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  {$IFDEF SG_LINUX_FPC}
  LCLIntf, LCLType,
  {$ENDIF}
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
 FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
 Graphics,
{$ENDIF}
 DXFConv, CADImage, sgFunction,
 sgConsts, sgLines, sgLists, TTF, ExtData, sgDataSegFile, sgDWGUtils
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGDEL_2009}
  , {$IFDEF SGDEL_XE2}Vcl.Imaging.PNGImage{$ELSE}PNGImage{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_6}
  , StrUtils, DateUtils
{$ENDIF}
{$IFDEF SGDEL_XE4}
  {$IFNDEF SG_FIREMONKEY}
  , AnsiStrings
  {$ENDIF}
{$ENDIF}
  , sgDWGBits
  {$IFDEF SGFPC}
    , FPimage
  {$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  ;

//For working with SaveToStream the definition sgFR should be uncommented.
//This is necessary for Fast Report Add-in.
//{$DEFINE sgFR}

type
  TdwgHeaderHandles = packed record
    CLAYER: UInt64;
    CELTYPE: UInt64;
    CMLSTYLE: UInt64;
    DIMSTYLE: UInt64;
    DIMBLK: UInt64;
    DIMBLK1: UInt64;
    DIMBLK2: UInt64;
    DIMLDRBLK: UInt64;
    DIMTXTSTY: UInt64;
    TEXTSTYLE: UInt64;
  end;

  EdwgError = class(Exception);
  EsgRSCoderError = class(Exception);

  TdwgObject = class;
  TdwgReader = class;
  TdwgProgressEvent = procedure(AStage: TProgressStage; ADone, ACount: Integer) of object;

//  TdwgCustomObjectReader = class
//  private
//    FOwner: TdwgObject;
//  protected
//    constructor Create(AOwner: TdwgObject); virtual;
//  end;
//
//  Tdwg13_15ObjectReader = class(TdwgCustomObjectReader)
//  end;
//
//  Tdwg2004ObjectReader = class(TdwgCustomObjectReader)
//  end;

  TdwgStringReader = class
  private
    FPBit: PsgInt64;
    FCodePage: Word;
    procedure DoReadUnicodeText(ALen: Integer; var S: sgUnicodeStr);
    procedure DoReadRawText(ALen: Integer; ACP: Word;
      var S: sgRawByteString);
    procedure TrimTextLen(ALen: Integer; var S: string);
  protected
    procedure Update(APPBit: PsgInt64; ACodePage: Word);
    function ReadCodePage: Word; virtual;
    function ReadXrecordTextLen: Integer; virtual;
    function ReadTextLen: Integer; virtual;
    function ReadText: string; virtual; abstract;
    function ReadXrecordText: string; virtual; abstract;
    function ReadInfoText: string; virtual; abstract;
  public
    constructor Create(APPBit: PsgInt64; ACodePage: Word); virtual;
  end;

  IColorReader = interface
    ['{E91511D0-A4DC-49C0-8F2B-4F8E1C9FC515}']
    function GetFlags: Word;
    procedure Update(AObj: Pointer);
    function ReadColor(APBit: PsgInt64): TsgColorCAD;
    function ReadENC(APBit: PsgInt64): TsgColorCAD;
    property Flags: Word read GetFlags;
  end;

  TdwgObject = class
  private
    FChildren: TsgBaseList;
    FChildIndex: Integer;
//    FColorBlockName: string;
//    FColorName: string;
    FEEDataList: TsgObjectCollection;
    FDataEnd: TsgInt64;
    FEntity: TsgDXFEntity;
    FHandle: UInt64;
    FHndsStart: TsgInt64;
    FPHnds: PsgInt64;
    FOwner: UInt64;
    FOwnerObject: TsgDXFEntity;
    FReactors: TsgInt64List;
    FReactorsCount: Integer;
    FReader: TdwgReader;
    FStrsStart: TsgInt64;
    FSubEnts: Integer;
    FXDicHandlePresent: Boolean;
    FXDict: UInt64;
    //FObj: TdwgObject;
    FLineWeight: Double;
    FSibling: UInt64;
    FFirstChildHandle: UInt64;
    FLastChildHandle: UInt64;
    FCorrectData: Boolean;
    FAnnotativeFlags: Integer;
//    FSwapped: Boolean;
//    FObjMap: TsgObjectsMap;
    FStringReader: TdwgStringReader;
    FColorReader: IColorReader;
    FMapIndex: Integer;
    FVersion: TsgDWGVersion;
    FCode: Byte;
    FACADExtData: TsgCADExtendedData;
    function ReadID(var APBit: TsgInt64): UInt64; overload;
    function ReadID(var APBit: TsgInt64; var ACode: Byte): UInt64; overload;
    function ReadID: UInt64; overload;
    function ReadColor(var APBit: TsgInt64): TsgColorCAD;
    function ReadText: string;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; virtual;
    procedure ApplyAnnotativeData(const AEEData: TsgCADExtendedData); virtual;
    function CreateStringReader(var APBit: TsgInt64; AVersion: TsgDWGVersion): TdwgStringReader; virtual;
    function GetBitSize: Integer; virtual;

    // --------- FChildren control functions ---------
    function GetNewObject(AHandle: UInt64; AEntClass: TsgDXFEntityClass): TsgDXFEntity; virtual;
    function Find(AHandle: UInt64; var I: Integer): Boolean; virtual;
    procedure ForceChildrenList(const ASubEnts: Integer = 0); virtual;
    function GetChild(Handle: UInt64): TsgDXFEntity; virtual;
    function GetChildEx(Handle: UInt64; MapIndex: Integer): TsgDXFEntity; virtual;
    function GetChildren(Index: Integer): UInt64; virtual;
    function GetChildrenCount: Integer; virtual;
    function ChildrenAdd(Handle: UInt64): Integer; virtual;
    // --------- FChildren control functions ---------

    function GetObjectBeginning: TsgInt64; virtual;
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; virtual;
    procedure ReadData(var APosition: TsgInt64); virtual; abstract;
    procedure ReadEED(var APBit, APEnd: TsgInt64; const AAppID: UInt64;
      const AEEData: TsgCADExtendedData); virtual;
    procedure ReadEEDItems(var APBit: TsgInt64); virtual;
    procedure ReadObjHeader(var APBit: TsgInt64); virtual;
    function ReadObjName(var APBit: TsgInt64; var Flags: Integer): string; virtual;
    procedure ReadLineWeight(AIndex: Integer);
    procedure References(AReader: TdwgReader); virtual;
    procedure PostReferences(AReader: TdwgReader); virtual;
    procedure SetBitSize(const AValue: Integer); virtual;
    procedure SetObjectBeginning(const APosition: TsgInt64); virtual;
    procedure SkipHandles(var APBit: TsgInt64);
    procedure ReadError;
    procedure ReadOwner(AReader: TdwgReader); virtual;
    procedure ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer = 0); virtual;
    procedure ReadSubEnts(AReader: TdwgReader); virtual;
    procedure ReadSubEntsR2000(AReader: TdwgReader); virtual;
    procedure ReadSubEntsR2004(AReader: TdwgReader); virtual;
    procedure ReadHandleRefs(var APBit: TsgInt64); virtual;

    function ReadChild(ASeek: Integer; AHandle: UInt64;
      AIndex: Integer): TdwgObject; virtual;

    procedure ReadPredefined(ASeek: Integer; AObj: TdwgObject;
      AIndex: Integer); virtual;

    function ReadExtrusion(var APBit: TsgInt64): TFPoint; virtual;

    procedure NewObject(ACode: Integer; AHandle: UInt64; var AObj: TdwgObject); virtual;
    function IsEntity: Boolean; virtual;

    procedure SetTransparency(AValue: Integer); virtual;

    procedure SetReader(const Value: TdwgReader); virtual;
    function OwnerAdd(AOwner: TsgDXFEntity): Integer; virtual;
    property BitSize: Integer read GetBitSize write SetBitSize;
    property EEDataList: TsgObjectCollection read FEEDataList;
    property ObjectBeginning: TsgInt64 read GetObjectBeginning write SetObjectBeginning;
    property Reader: TdwgReader read FReader write SetReader;
    property Child[Handle: UInt64]: TsgDXFEntity read GetChild;
    property Children[Index: Integer]: UInt64 read GetChildren;
    property ChildrenCount: Integer read GetChildrenCount;
    property StringReader: TdwgStringReader read FStringReader;
    property Version: TsgDWGVersion read FVersion;
  public
    constructor Create(const AVersion: TsgDWGVersion); virtual;
    destructor Destroy; override;
  end;

  TsgDWGMeatadata = record
    ReleaseVersion: Byte;
    PreviewAddress: Cardinal;
    OrigFileSavedVer: Byte;
    OrigFileSavedReleaseVer: Byte;
    CodePage: Word;
    SecurityFlags: Cardinal;
    SummaryInfoAddress: Cardinal;
    VBAProjectAddress: Cardinal;
  end;

  TdwgReader = class
  private
    FAcDbClasses: TsgStringList;
    FClasses: TsgIntegerList;
    FConverter: TsgDXFConverter;
    FHeaderHandles: TdwgHeaderHandles;
    FExceptionClass: ExceptClass;
    FExceptionEntityHandle: UInt64;
    FImage: Integer;
    FMaxHandle: UInt64;
    FMem: PAnsiChar;
    FObjMap: TsgObjectsMap;
    FOnProgress: TdwgProgressEvent;
    FSize: Integer;
    FSource: TCustomMemoryStream;
    FStream: TStream;
    FUnits: Integer;
    FVersion: TsgDWGVersion;
    FMetadata: TsgDWGMeatadata;
    FSegmentedFile: TsgSegmentedFile;
    FBlockRecords: TdwgObject;
    FStyles: TdwgObject;
    FVPortEntHdrs: TdwgObject;
    FLayerControl: UInt64;
    FLTypeControl: UInt64;
    FAppIDControl: UInt64;
    FVPortControl: UInt64;
    FDimStyleControl: UInt64;
    FLayoutsDict: UInt64;
    FImageDefDict: UInt64;
    FMLStylesDict: UInt64;
    FMLeaderStylesDict: UInt64;
    FTableStyleDict: UInt64;
    FVariableDict: UInt64;
    FMaterialDict: UInt64;
    FPlotSettingsDict: UInt64;
    FModel: TsgDXFBlock;
    FPaper: TsgDXFBlock;
    FLTByLayer: UInt64;
    FLTByBlock: UInt64;
    FLTContinious: UInt64;
    FNamedObjects: UInt64;
    FTables: TdwgObject;
    FDataBase: TdwgObject;
    FACADAppID: UInt64;
    FURLAppID: UInt64;
    FAnnotativeAppID: UInt64;
    FAppID: UInt64;
    FAcDbAttrAppID: UInt64;
    FACADMLeaderVerAppID: UInt64;
    FColors: UInt64;
    FIrdDscDict: UInt64;
    FViewports: TsgObjectCollection;
    FActiveViewport: UInt64;
    FArrows: TStringList;
    FStringReader: TdwgStringReader;
    FColorReader: IColorReader;
    FReaderGroupsStack2004: TList;
    FSGSaved: Boolean;
    FProxyReaderImpl: TsgProxyReaderImpl;
    FAnnotationProps: IInterfaceList;
    FMaterials: TsgObjectCollection;
    FAppInfo: TsgDWGAppInfo;
    FGlobalNames: TStrings;
    FGroups: UInt64;
    FLinksCount: Integer;
    FLinks: Pointer;// TObjectLinkRecArray;
    function Code2007ByClassName(const AObjectBeginning, APBit: TsgInt64;
      AReadedCode: Integer): Integer;
    procedure CRead(ASeek, ASize: Integer);
    function CheckSentinel(P: Pointer; const ASentinel: TsgDWGSentinel): Boolean;
    procedure DoException;
    procedure DoProgress(AStage: TProgressStage);
    function DoReadData(AObj: TdwgObject; var APBit: TsgInt64): Boolean;
//    function FindLocation(const AHandle: UInt64; var AIndex: Integer): Integer;
//    function FinishObject(AObj: TdwgObject; AOwner: TsgDXFEntity;
//      AIndex: Integer): TsgDXFEntity;
    function GetArrowTypeByHandle(const ABlockRecordHandle: UInt64): TsgDimArrowType;
    function GetTableNameByHandle(const AHandle: UInt64;
      AClass: TsgDXFEntityClass): string;
    procedure Init2007ObjectData(var AStartBit, APStrStart,
      APHndStart: TsgInt64; var AEndBit: Cardinal;
      ANeedReadEndBit: Boolean = True);
    procedure Init2010ObjectData(var AStartBit, APStrStart,
      APHndStart: TsgInt64; const AIdsPos: Cardinal; var AEndBit: Cardinal);
    procedure LoadHeaderHandles;
//    function LoadEntity(const AObject: TdwgObject): Boolean;
    function LTypeByIndex(AIndex: Integer): TsgDXFLineType;
//    procedure NewObject(ACode: Integer; var AObj: TdwgObject);
    function ObjByHandle(AHandle: UInt64; AClass: TsgDXFEntityClass): TsgDXFEntity;
    function PMSpace(var APBit: TsgInt64; AObj: TdwgObject;
      var APHeadVar: PsgHeadVarStruct; var ALimMin, ALimMax: TFPoint): TFRect;
    procedure Read(ASeek, ASize: Integer);
    procedure Read2007StrDataLength(const APStart: TsgInt64; var AEndBit,
      AStrSize: Cardinal);
    procedure ReadAppInfo(ASeek, ASize: Integer);
    procedure ReadClasses(ASeek, ASize: Integer);
    procedure ReadHeadVars(ASeek, ASize: Integer);
    procedure ReadMeasurement(ASeek: Integer);
    procedure ReadMetadata(const Stream: TStream; const FileHeader: TdwgFileHeader);
//    function ReadObject(ASeek: Integer; AHandle: UInt64;
//      AIndex: Integer): TsgDXFEntity;
    procedure ReadObjMap(ASeek, ASize: Integer);
//    procedure ReadPart(AFirst, ALast: Integer);
    procedure ReadSummaryInfo(ASeek, ASize: Integer);
    procedure ReadTemplate(ASeek, ASize: Integer);
    procedure ReadPrototype(ASeek, ASize: Integer);
    procedure ReadPlotSettings(Obj: TdwgObject;
      const APlotSettings: PsgPlotSettingsData; var APBit: TsgInt64);
    function VarCode(ACode: Integer): Integer;
    function GetPHeadVarStruct: PsgHeadVarStruct;
    procedure InitLayouts(var AModel, APaper: TsgDXFBlock);
    function GetDictionary(const Name: string): TsgDXFDictionary;
    procedure InitViewportsByList;
    function EntAddRef(AEnt: TsgDXFEntity): Integer;
    function EntRelease(AEnt: TsgDXFEntity): Integer;
    function GetStringReader(var APBit: TsgInt64): TdwgStringReader;
    function GetXDataProcs: TsgXDataProcs;
    procedure SetVersion(const Value: TsgDWGVersion);
    function GetProxyReaderImpl: TsgProxyReaderImpl;
    function CreatePredefinedControl(ACode: Integer): TdwgObject;
    function AddEEDItems(AEntityHandle: UInt64; AEEDItems: TsgObjectCollection): Integer;
    function GetEntityEEDItemsSorted: Boolean;
    procedure SetEntityEEDItemsSorted(const Value: Boolean);
    function CreateStringReader(APBit: PsgInt64): TdwgStringReader;
    procedure LinkMaterials;
    procedure AddMaterialLink(AMaterial: UInt64; AEntity: UInt64);
    function ExtractFromDatabase(AHandle: UInt64): Integer;
    function GetGlobalName(const AName: string): string;

    function AddLink(obj: TObject; const APropName: string; const AValue: string; AProxyClass: TClass): Integer; overload;
    function AddLink(obj: TObject; AProp: Pointer; const AValue: string; AProxyClass: TClass): Integer; overload;
    //function AddLink(obj: TObject; const APropName: string; const AValue: UInt64; AProxyClass: TClass): Integer; overload;
    //function AddLink(obj: TObject; AProp: Pointer; const AValue: UInt64; AProxyClass: TClass): Integer; overload;
    procedure PrelinkObjectLinkCollections;
    procedure LinkObjectLinkCollections;
  protected
    function IsACADAppID(const AAppID: UInt64): Boolean;
    function IsRelatedAppID(const AAppID: UInt64): Boolean;
    function IsURLAppID(const AAppID: UInt64): Boolean;
    function IsAnnotativeAppID(const AAppID: UInt64): Boolean;
    function IsAcDbAttrAppID(const AAppID: UInt64): Boolean;

    function ConvExtDataToDXF(const AExtData: TsgCADExtendedData): Boolean;

    procedure ReadHeader; virtual;
    procedure ReadImage; virtual;
    procedure ReadObjects; virtual;
    function Stopped: Boolean;

    property MaxHandle: UInt64 read FMaxHandle;
    property Version: TsgDWGVersion read FVersion write SetVersion;
    property Model: TsgDXFBlock read FModel;
    property Paper: TsgDXFBlock read FPaper;

    property XDataProcs: TsgXDataProcs read GetXDataProcs;
    property ProxyReaderImpl: TsgProxyReaderImpl read GetProxyReaderImpl;
    property EntityEEDItemsSorted: Boolean read GetEntityEEDItemsSorted write SetEntityEEDItemsSorted;
    property GlobalName[const AName: string]: string read GetGlobalName;
  public
    constructor Create(AConv: TsgDXFConverter; S: TStream);
    destructor Destroy; override;
    procedure ReadFile;
    property Conv: TsgDXFConverter read FConverter;
    property OnProgress: TdwgProgressEvent read FOnProgress;// write FOnProgress;
    property Stream: TStream read FStream;
    property Dictionary[const Name: string]: TsgDXFDictionary read GetDictionary;
    property AppInfo: TsgDWGAppInfo read FAppInfo;
  end;

  TsgDWGConverter = class(TsgDXFConverter)
  private
    FImage: TBitmap;
  protected
    function GetDWGVersion(const AString: AnsiString;
      var AVersion: TsgDWGVersion): Integer; override;
    procedure Initialize; override;
  public
    destructor Destroy; override;
  end;

  TsgDWGImage = class(TsgImageWithHandles)
  private
    function GetPreview: TBitmap;
  protected
    function CreateConverter: TsgDXFConverter; override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function IsAutocadLType: Boolean; override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadPreviewFromFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream); override;
    property Preview: TBitmap read GetPreview;
  end;

function ObjStreamPos(const ABegin, ACurr: TsgInt64): TsgInt64;

var
  CF_DWG: Word;
implementation

uses
  Math, DWG12, {$IFDEF SGDEL_6} Variants,{$ENDIF} TypInfo, sgBitmap;

{$IFDEF SGFPC}
  {$IFDEF LINUX}
    {$IFDEF HAS_FEATURE_ENCODING}
      {$DEFINE USE_FEATURE_ENCODING}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

const
  cnstProxyEntityIndex = 83;

  cnstTolerancePropNameDimlineColor = 'DimlineColor';
  cnstTolerancePropNameDimScaleOverall = 'DimScaleOverall';
  cnstTolerancePropNameHeight = 'Height';
  cnstTolerancePropNameGAP = 'GAP';

  cnstTextAnnotationWidth = 'TextAnnotationWidth';

//  cnstSnHeaderVariables: array[0..15] of Byte = ($CF, $7B, $1F, $23, $FD, $DE, $38, $A9, $5F,
//    $7C, $68, $B8, $4E, $6D, $33, $5F);
//  cnstSnImageData: array[0..15] of Byte = ($1F, $25, $6D, $07, $D4, $36, $28, $28, $9D, $57,
//    $CA, $3F, $9D, $44, $10, $2B);

  cnstAPPID = 'APPID';
  cnstEntityHandle = ': Entity handle = $';

type
  TsgSeekDWG = (seB,seBB,seBS,seBL,seBD,se2RD,se3RD,se2D,se3D,seRS,seRC,seRL,seRGB,seUInt64,seRD);

  TsgDXFViewportEntityHeader = class(TsgDXFPenTableItem)
  end;

  TsgDXFViewportEntityHeaderControl = class(TsgDXFTable)
  end;

  TdwgReadPoint = function(var APBit: TsgInt64): TFPoint;

  TdwgImage = packed record
    Sentinel: array[0..15] of Byte;
    Size: Integer;
    Count: Byte;
    Images: array[Byte] of TdwgLocator;
  end;

{ Begin DWG 2004 support }

  TdwgLocators = packed array[0..cntDWGSectMaxIndex] of TdwgLocator;

  Pdwg2004FileHeader = ^Tdwg2004FileHeader;
  Tdwg2004FileHeader = packed record
    VersionID: array[0..5] of AnsiChar;
    Pad: array[0..6] of AnsiChar;
    ImageSeeker: Integer;
    Unknown: Word;
    CodePage: Word;
    LocCount: Integer;
    Locators: TdwgLocators;
  end;

  PHead2004Rec = ^THead2004Rec;
  THead2004Rec = packed record
    UnpackSize: Integer;
    Empty: Integer;
    Extra: Integer;
    Unused: array[0..24] of Byte;
    Name: array[0..58] of AnsiChar;
    Index: Integer;
    PackSize: Integer;
  end;

  TsgTableArray = array[0..4095] of Integer;
  TsgSectionsArray = array[0..4] of PHead2004Rec;


{ End DWG 2004 support }

{ Begin DWG 2007 support }

  TsgDwg2007ChData = packed record
    Crc1: Int64;
    Crc2: Int64;
    Random1: Int64;
    Random2: Int64;
    CrcSeedEncoded: Int64;
  end;

  TsgDwg2007FileHeader = packed record
    HeaderSize: Int64;
    FileSize: Int64;
    PagesMapCrcCompr: Int64;
    PagesMapFactor: Int64;
    PagesMapCrcSeed: Int64;
    PagesMap2Offset: Int64;
    PagesMap2Id: Int64;
    PagesMapOffset: Int64;
    PagesMapId: Int64;
    Header2Offset: Int64;
    PagesMapSizeCompr: Int64;
    PagesMapSizeSrc: Int64;
    PagesAmount: Int64;
    PagesMaxId: Int64;
    Unknown15: Int64;
    Unknown16: Int64;
    PagesMapCrcSrc: Int64;
    Unknown18: Int64;
    Unknown19: Int64;
    Unknown20: Int64;
    SectionsAmount: Int64;
    SectionsMapCrcSrc: Int64;
    SectionsMapSizeCompr: Int64;
    SectionsMap2Id: Int64;
    SectionsMapId: Int64;
    SectionsMapSizeSrc: Int64;
    SectionsMapCrcCompr: Int64;
    SectionsMapFactor: Int64;
    SectionsMapCrcSeed: Int64;
    StreamVersion: Int64;
    CrcSeed: Int64;
    CrcSeedEncoded: Int64;
    RandomSeed: Int64;
    HeaderCrc64: Int64;
  end;

  PsgDWG2007Page = ^TsgDWG2007Page;
  TsgDWG2007Page = record
    Id: Int64;
    Size: Int64;
    Offset: Int64;
  end;

  TsgDWG2007Section = packed record
    DataSize: Int64;
    PageSize: Int64;
    Encrypted: Int64;
    Unknown1: Int64;
    SecNameLen: Int64;
    Unknown2: Int64;
    Encoded: Int64;
    PagesAmount: Int64;
    SecName: WideString;
    Pages: TList;
    Data: TMemoryStream;
  end;

  PsgDWG2007StreamPage = ^TsgDWG2007StreamPage;
  TsgDWG2007StreamPage = packed record
    DataOffset: Int64;
    PageSize: Int64;
    PageId: Int64;
    DataSize: Int64;
    ComprSize: Int64;
    Checksum: Int64;
    Crc: Int64;
  end;

  TsgDWG2007Decompressor = class
  private
    FDstEnd: PAnsiChar;
    FLength: Cardinal;
    FOffset: Cardinal;
    FOpCode: Cardinal;
    FSrc: PAnsiChar;
    FSrcEnd: PAnsiChar;
    procedure GetCmpData(ADest: PAnsiChar; ALength, AOffset: Integer);
    procedure GetCmpOpcode;
    procedure GetLiteralData(ADest, ASource: PAnsiChar; ALength: Integer);
    procedure GetLiteralOpcode;
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    procedure Decompress(ASource: PAnsiChar; ASrcSize: Cardinal;
      ADest: PAnsiChar; ADstSize: Cardinal); virtual;
  end;

  TsgRSCoder = class
  private
    FAlfaTo: array[0..255] of Cardinal;
    FBlockSize: Cardinal;
    FDSize: Cardinal;
    FErr: Integer;
    FGPoly: array[0..16] of Cardinal;
    FIndexOf: array[0..255] of Cardinal;
    FMod255: PAnsiChar;
    FPSize: Integer;
    procedure Generate(const APoly: array of Byte; AM, AErrors: Cardinal);
    function GetDSize: Integer;
    procedure InitRSCoder1100;
    procedure InitRSCoder500;
  public
    constructor Create;
    destructor Destroy; override;
    function Decode(AData: PAnsiChar): Integer;
    property DSize: Integer read GetDSize;
  end;

{ End DWG 2007 support }

  TDWGProxyReader = class(TCustomAlignedDataReader)
  protected
    function ReadDouble: Double; override;
    function ReadInteger: Integer; override;
    function ReadSmallInt: SmallInt; override;
    function ReadWord: Word; override;
    function ReadByte: Byte; override;
    procedure PushPosition; override;
    procedure PopPosition; override;
    procedure Seek(ASize: Integer); override;
    function ReadHandle: UInt64; override;
    procedure ReadBytes(var Dest; const ACount: UInt64); override;
    function PositionAsInt64: TsgInt64; override;
  public
    constructor Create(APos: Pointer; AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TProxyLWPLineReader = class(TCustomProxyObjReader)
  protected
    function ReadObject(ProxyReader: IProxyReader; obj: TObject): Boolean; override;
  end;

  TdwgClass = class of TdwgObject;
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgCADHatchAccess = class(TsgCADHatch);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgDXFTextAccess = class(TsgDXFText);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgDXFBlockRecordsAccess = class(TsgDXFBlockRecords);
  TsgDXFPlotSettingsAccess = class(TsgDXFPlotSettings);
  TsgDXFSplineAccess = class(TsgDXFSpline);
  TsgDXFObjectEntityAccess = class(TsgDXFObjectEntity);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  TsgDXFViewportAccess = class(TsgDXFViewport);
  TsgBitmapAccess = class(TsgBitmap);
  TsgObjectsMapAccess = class(TsgObjectsMap);
  TMemoryStreamAccess = class(TMemoryStream);
  TsgProxyReaderImplAcceess = class(TsgProxyReaderImpl);
  TsgCollectionAccess = class(TsgCollection);
  TsgObjectCollectionAccess = class(TsgObjectCollection);
  TsgCADMLeaderAnnotContextAccess = class(TsgCADMLeaderAnnotContext);
  TsgMLeaderAccess = class(TsgMLeader);
  TsgMLeaderLineAccess = class(TsgMLeaderLine);
  TsgMLeaderContextDataAccess = class(TsgMLeaderContextData);
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgCADExtendedDataAccess = class(TsgCADExtendedData);

  TdwgBlock = class;
  TdwgBitmap = class(TBitmap)
  end;

  TsgObjCollection = class(TInterfacedObject, IsgObjCollection)
  protected
    FMap: TsgObjectsMapAccess;
    { IsgObjCollection }
    function GetObj(Handle: UInt64): TObject;
  end;

{$IFDEF SGDEL_2010}
  {$RTTI INHERIT METHODS([]) PROPERTIES([vcPublished]) FIELDS([])}
{$ENDIF}

  {$M+}
  TsgDXFDimensionStyleAccess = class(TsgDXFDimensionStyle)
  published
    property DIMLDRBLK;
    property DIMBLK;
    property DIMBLK1;
    property DIMBLK2;
  end;

  TsgDXFLeaderAccess = class(TsgDXFLeader)
  published
    property DIMLDRBLK;
    property DIMBLK;
    property DIMBLK1;
    property DIMBLK2;
  end;
  {$M-}

  TsgObjectLink = class
  protected
    FValue: UInt64;
    FCollection: TObject;
    function GetPropItem: TObject; virtual; abstract;
    function GetAsHandle: UInt64; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    procedure SetAsHandle(const Value: UInt64); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
  public
    property AsHandle: UInt64 read GetAsHandle write SetAsHandle;
    property AsString: string read GetAsString write SetAsString;
    property Collection: TObject read FCollection;
    property PropItem: TObject read GetPropItem;
  end;

  TsgObjectLinkClass = class of TsgObjectLink;

  TsgHandleLink = class(TsgObjectLink)
  protected
    function GetPropItem: TObject; override;
    function GetAsHandle: UInt64; override;
    function GetAsString: string; override;
    procedure SetAsHandle(const Value: UInt64); override;
    procedure SetAsString(const Value: string); override;
  end;

  TsgDatabaseHandleLink = class(TsgHandleLink)
  protected
    function GetPropItem: TObject; override;
  end;

  TsgNameLink = class(TsgObjectLink)
  protected
    function GetPropItem: TObject; override;
    function GetAsHandle: UInt64; override;
    function GetAsString: string; override;
    procedure SetAsHandle(const Value: UInt64); override;
    procedure SetAsString(const Value: string); override;
  public
    destructor Destroy; override;
  end;

  TsgBlockRecordLink = class(TsgNameLink)
  end;

  TObjectLinkRec = record
    obj: TObject;
    Prop: PPropInfo;
    Link: TsgObjectLink;
  end;

  TObjectLinkRecArray = array of TObjectLinkRec;

  TdwgIndexColorReader = class(TInterfacedObject, IColorReader)
  private
    FObj: TdwgObject;
    FFlags: Word;
  protected
    function GetFlags: Word;
    procedure Update(AObj: Pointer);
    function ReadFlags(var APBit: TsgInt64): Integer; virtual;
    function ReadENCSection(var APBit: TsgInt64): TsgColorCAD; virtual;
  public
    function ReadColor(APBit: PsgInt64): TsgColorCAD; virtual;
    function ReadENC(APBit: PsgInt64): TsgColorCAD;
    property Obj: TdwgObject read FObj;
  end;

  TdwgR18ColorReader = class(TdwgIndexColorReader)
  private
    function ReadCMCSection(var APBit: TsgInt64): TsgColorCAD;
  protected
    function ReadENCSection(var APBit: TsgInt64): TsgColorCAD; override;
  public
    function ReadColor(APBit: PsgInt64): TsgColorCAD; override;
  end;

  TdwgWideStringReader = class(TdwgStringReader)
  protected
    function ReadInfoText: string; override;
    function ReadText: string; override;
    function ReadXrecordText: string; override;
  end;
  TdwgWideStringReaderParse = class(TdwgWideStringReader)
  protected
    function ReadText: string; override;
  end;

  TdwgAnsiStringReader = class(TdwgStringReader)
  protected
    function ReadCodePage: Word; override;
    function ReadInfoText: string; override;
    function ReadText: string; override;
    function ReadXrecordText: string; override;
  end;
  TdwgAnsiStringReaderParse = class(TdwgAnsiStringReader)
  protected
    function ReadText: string; override;
  end;

  TdwgCustomControl = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure InitializeTable(ASection: TConvSection; AName: string);
    procedure ReadOwner(AReader: TdwgReader); override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadSubEnts(AReader: TdwgReader); override;
    procedure ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer = 0); override;
  end;

  TdwgTables = class(TdwgCustomControl)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure NewObject(ACode: Integer; AHandle: UInt64; var AObj: TdwgObject); override;
  end;

  TdwgDataBase = class(TdwgObject)
  protected
    function GetChildren(Index: Integer): UInt64; override;
    function GetChildrenCount: Integer; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadSubEnts(AReader: TdwgReader); override;
  end;

  TdwgTableItem = class(TdwgObject)
  private
    FEntryName: string;
    FFlags: Integer;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
  end;

  TdwgBlockRecord = class(TdwgTableItem)
  private
    FBlockEnd: UInt64;
    FBlock: TdwgBlock;
    FLayout: UInt64;
    function IsAnonymous: Boolean;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    function OwnerAdd(AOwner: TsgDXFEntity): Integer; override;
    procedure SetReader(const Value: TdwgReader); override;
    procedure ReadSubEntsR2000(AReader: TdwgReader); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TdwgArrows = class(TPersistent)
  private
    FLink: array[vnDIMBLK .. vnDIMLRBLK] of TsgExtData;
    function GetHandle(Index: TsgDimNameVal): UInt64;
    function GetName(Index: TsgDimNameVal): string;
    procedure SetHandle(Index: TsgDimNameVal; const Value: UInt64);
    procedure SetName(Index: TsgDimNameVal; const Value: string);
    function Get_Type(Index: TsgDimNameVal): TsgExtDataType;
    function GetArrows(Index: TsgDimNameVal): TsgExtData;
    procedure SetArrows(Index: TsgDimNameVal; const Value: TsgExtData);
  protected
    property Arrows[Index: TsgDimNameVal]: TsgExtData read GetArrows write SetArrows;
  public
    constructor Create;
    destructor Destroy; override;
    property Name[Index: TsgDimNameVal]: string read GetName write SetName;
    property Handle[Index: TsgDimNameVal]: UInt64 read GetHandle write SetHandle;
    property _Type[Index: TsgDimNameVal]: TsgExtDataType read Get_Type;
    function IsEqualLinks(Index1, Index2: TsgDimNameVal): Boolean;
    function IsEmptyLink(Index: TsgDimNameVal): Boolean;
  end;

  TdwgDimStyle = class(TdwgTableItem)
  private
    FArrows: TdwgArrows;
    FTextStyle: UInt64;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TdwgEntity = class(TdwgObject)
  private
    FColor: TsgColorCAD;
    FDbColor: UInt64;
    FLayer: UInt64;
    FLType: UInt64;
    FMode: Byte;
    FStandardLType: UInt64;
    FLineTypeScale: Double;
    FVisibility: Boolean;
    FMaterialHandle: UInt64;
  protected
    procedure ReadEntProxyData(var APBit: TsgInt64); virtual;
    function IsEntity: Boolean; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); virtual;
    procedure DoReadEntProxyData(var APBit: TsgInt64); virtual;
    procedure ReadOwner(AReader: TdwgReader); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReferencesColor(AReader: TdwgReader); virtual;
    function ReadElevation(var APBit: TsgInt64): Double; virtual;
    function ReadZThik(var APBit: TsgInt64): Double; virtual;
    property MaterialHandle: UInt64 read FMaterialHandle;
  end;

  TdwgCustomVertex = class(TdwgEntity)
  protected
    function ReadExtrusion(var APBit: TsgInt64): TFPoint; override;
    function ReadZThik(var APBit: TsgInt64): Double; override;
  end;

  TdwgBlock = class(TdwgEntity)
  private
    FBlockRecord: TdwgBlockRecord;
    FBlockRecordNoLocation: Boolean;
    FEntryName: string;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure ReadOwner(AReader: TdwgReader); override;
    function OwnerAdd(AOwner: TsgDXFEntity): Integer; override;
  end;

  TdwgLayer = class(TdwgTableItem)
  private
    FLType: UInt64;
    FColor: TsgColorCAD;
    FIsPlotting: Boolean;
    FVisible: Boolean;
    FMaterialHandle: UInt64;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
  end;

  TdwgAppIDControl = class(TdwgCustomControl)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgLTControl = class(TdwgCustomControl)
  private
    FByLayer: UInt64;
    FByBlock: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgStyleControl = class(TdwgCustomControl)
  private
    FSearchStyle: TsgDXFStyle;
    FSortedList: TsgObjectList;
    function CompareEntHandles(const A, B: Pointer): Integer;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer = 0); override;

    function GetNewObject(AHandle: UInt64; AEntClass: TsgDXFEntityClass): TsgDXFEntity; override;
    function ChildrenAdd(Handle: UInt64): Integer; override;
    function QFind(Handle: UInt64): TsgDXFEntity;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TdwgLayerControl = class(TdwgCustomControl)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgBlockControl = class(TdwgCustomControl)
  private
    FModelHandle: UInt64;
    FPaperHandle: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgDimStyleControl = class(TdwgCustomControl)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgVPortControl = class(TdwgCustomControl)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgViewportEntityHeaderControl = class(TdwgCustomControl)
  end;

  TdwgLType = class(TdwgTableItem)
  private
    FElements: array of TsgLTypeElement;
    FStyles: array of UInt64;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgMLStyle = class(TdwgObject)
  private
    FLTypes: TsgInt64List;
  protected
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgSortEntsTable = class(TdwgObject)
  private
    FObject: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgVPort = class(TdwgTableItem)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgWipeoutVar = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  Tdwg3DFace = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgCircle = class(TdwgCustomVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDbColor = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgDictionary = class;

  TdwgDictionaryItem = class(TdwgObject)
  protected
    FOwnerDictionary: TdwgDictionary;
  end;

  TdwgXRecord = class(TdwgDictionaryItem)
  private
    procedure AddStringData(AExtData: TsgCADExtendedData; ACode: SmallInt; S: string);
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadOwner(AReader: TdwgReader); override;
  end;

  TdwgDictionary = class(TdwgObject)
  private
    FCloningFlag: Integer;
    FNames: array of string;
    procedure PerformName;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadSubEnts(AReader: TdwgReader); override;
    procedure ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer = 0); override;
    procedure References(AReader: TdwgReader); override;
    procedure NewObject(ACode: Integer; AHandle: UInt64;
      var AObj: TdwgObject); override;
    function OwnerAdd(AOwner: TsgDXFEntity): Integer; override;
  end;

  TdwgDataLink = class(TdwgObject)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgEllipse = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
    function ReadExtrusion(var APBit: TsgInt64): TFPoint; override;
  end;

  TdwgHatch = class(TdwgEntity)
  private
    procedure AddBoundaryDataCurve(const ABoundaryList: Tsg2DBoundaryList;
      const ACurve: Tsg2DCurve);
    function Read2DArc(const ACircle: Boolean; var APBit: TsgInt64): Tsg2DArc;
    function Read2DLine(var APBit: TsgInt64): Tsg2DLine;
    function Read2DPolyline(var APBit: TsgInt64; AIsMPolygon: Boolean): Tsg2DPolyline;
    function Read2DSpline(var APBit: TsgInt64): Tsg2DSpline;
    procedure ReadGradietParams(var APBit: TsgInt64);
    procedure ReadPatternData(var APBit: TsgInt64);
  protected
    function DoSolidHatch(const AIsSolid: Boolean): TsgCADCurvePolygon; virtual;
    procedure ReadAssociative(var APBit: TsgInt64); virtual;
    function ReadBoundaryData(var APBit: TsgInt64): Boolean; virtual;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure ReadFillColorAndOffset(var APBit: TsgInt64); virtual;
    procedure ReadStyleAndPatternType(var APBit: TsgInt64); virtual;
    procedure ReferencesColor(AReader: TdwgReader); override;
    procedure SetTransparency(AValue: Integer); override;
  end;

  TdwgMPolygon = class(TdwgHatch)
  protected
    function DoSolidHatch(const AIsSolid: Boolean): TsgCADCurvePolygon; override;
    procedure ReadAssociative(var APBit: TsgInt64); override;
    function ReadBoundaryData(var APBit: TsgInt64): Boolean; override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure ReadFillColorAndOffset(var APBit: TsgInt64); override;
    procedure ReadStyleAndPatternType(var APBit: TsgInt64); override;
  end;

  TdwgImageDef = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgInsert = class(TdwgEntity)
  private
    FComplex: Boolean;
    FBlockRecord: UInt64;
    function ChangeEntEx(AExtData: TsgCADExtendedData; var AEntTypeNum: Integer): string;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgCustomApplicationBlockReference = class(TdwgInsert)
  private
    FViewport: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgAcIdBlockReference = class(TdwgCustomApplicationBlockReference)
  end;

  TdwgAcDbViewRepBlockReference = class(TdwgCustomApplicationBlockReference)
  end;

  TdwgLayout = class(TdwgObject)
  private
    FActiveViewport: UInt64;
    FPaperSpaceHandle: UInt64;
    FViewports: TsgInt64List;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgLeader = class(TdwgEntity)
  private
    FArrows: TdwgArrows;
    FAnnotationType: Integer;
    FArrowSize: Double;
    FArrowSizeOverrides: Double;
    FArrowScaleOverrides: Double;
    FTextAnnotationWidth: Double;
    FGAP: Double;
    FDimStyle: UInt64;
    FHooklineVector: TFPoint;
    FTAD: Integer;
    FCanApplyEED: Boolean;
    FOverrides: TsgDimNameVals;
    FAssociatedAnnotation: UInt64;
    FHookLine: PInteger;
    //FArrowHeadType: Integer; // TODO: apply FArrowHeadType
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TdwgLine = class(TdwgCustomVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgLWPline = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgMLine = class(TdwgEntity)
  private
    FStyle: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgMText = class(TdwgEntity)
  private
    FStyle: UInt64;
    FTextAnnotationWidth: Double;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    function CreateStringReader(var APBit: TsgInt64;
      AVersion: TsgDWGVersion): TdwgStringReader; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure ReadStyleHandleData(var APBit: TsgInt64); virtual;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgEmbeddedMText = class(TdwgMText)
  protected
    procedure ReadStyleHandleData(var APBit: TsgInt64); override;
    procedure ReadEntProxyData(var APBit: TsgInt64); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
  end;

  TdwgOLE2Frame = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgPoint = class(TdwgCustomVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgPolyline = class(TdwgCustomVertex)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
  end;

  TdwgPlotSettings = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgRasterVariables = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgSolid = class(TdwgCustomVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgSpline = class(TdwgEntity)
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgStyle = class(TdwgTableItem)
  private
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
  end;

  TdwgText = class(TdwgCustomVertex)
  private
    FExtrusion: TFPoint;
    FDataFlags: Integer;
    FGeneration: Byte;
    FHAlign: Byte;
    FHeight: Double;
    FObliqueAngle: Single;
    FPoint: TFPoint;
    FPoint1: TFPoint;
    FRotation: Single;
    FScale: Double;
    FStyle: UInt64;
    FText: string;
    FVAlign: Byte;
  protected
    procedure ApplyValue; virtual;
    function CreateStringReader(var APBit: TsgInt64;
      AVersion: TsgDWGVersion): TdwgStringReader; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    function ReadElevation(var APBit: TsgInt64): Double; override;
    function ReadExtrusion(var APBit: TsgInt64): TFPoint; override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
  end;

  {$M+}
  TsgDXFToleranceProxy = class(TsgDXFTolerance)
  private
    function GetDimlineColor: Variant;
    procedure SetDimlineColor(const AValue: Variant);
  published
    property Height;
    property GAP;
    property DimScaleOverall;
    property DimlineColor: Variant read GetDimlineColor write SetDimlineColor;
  end;
  {$M-}

  TdwgTolerance = class(TdwgEntity)
  private
    FHeightR13: Double;
    FGAPR13: Double;
    FDimStyle: UInt64;
    FOverrideProps: TStringList;
    function AddProp(const AName: string; AValue: OleVariant): Integer;
    function GetValue(Index: Variant): Variant;
    procedure SetValue(Index: Variant; AValue: Variant);
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    function CreateStringReader(var APBit: TsgInt64;
      AVersion: TsgDWGVersion): TdwgStringReader; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    property Value[Index: Variant]: Variant read GetValue write SetValue;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
  end;

  TdwgUndefinedEntity = class(TdwgEntity)
  private
    FBitSize: Integer;
    FObjectBeginning: TsgInt64;
  protected
    function GetBitSize: Integer; override;
    function GetObjectBeginning: TsgInt64; override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure SetBitSize(const AValue: Integer); override;
    procedure SetObjectBeginning(const AValue: TsgInt64); override;
  end;

  TdwgVertex = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgViewport = class(TdwgEntity)
  private
    FCount_10: Integer;
    FCount_40: Integer;
    FEntHeaderHandle: UInt64;
    FFrozenLayerCount: Integer;
    FClippingBoundaryHandle: UInt64;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure ReadOwner(AReader: TdwgReader); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgViewportEntityHeader = class(TdwgTableItem)
  protected
    procedure References(AReader: TdwgReader); override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgWipeOut = class(TdwgEntity)
  private
    procedure ReadEntDataEx(var APBit: TsgInt64);
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgAppID = class(TdwgTableItem)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgAcadTable = class(TdwgUndefinedEntity)
  private
    FBlockRecord: UInt64;
    FTableStyle: UInt64;
    FHasAttribs: Boolean;
    FObjectsCount: Integer;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgAcadTableStyle = class(TdwgObject)
  private
    FDataTextStyle: UInt64;
    FTitleTextStyle: UInt64;
    FHeaderTextStyle: UInt64;
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgACISEntity = class(TdwgUndefinedEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgRegion = class(TdwgACISEntity)
  end;

  Tdwg3dSolid = class(TdwgACISEntity)
  end;

  TdwgBody = class(TdwgACISEntity)
  end;

  TdwgSurface = class(TdwgACISEntity)
  end;

  TdwgArc = class(TdwgCircle)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgAttributeType = (atSingleLine, atAttribMLine, atAttdefMLine);

  TdwgAttdef = class(TdwgText)
  private
    FAttVersion: Byte;
    FTag: string;
    FFlags: Byte;
    FAttributeType: TdwgAttributeType;
    FMText: TdwgMText;
    procedure ReadMTextFields(var APBit: TsgInt64);
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ApplyValue; override;
    procedure ApplyFlags; virtual;
    procedure ReadPrompt(var APBit: TsgInt64); virtual;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  public
    destructor Destroy; override;
  end;

  TdwgAttrib = class(TdwgAttdef)
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadPrompt(var APBit: TsgInt64); override;
  end;

  TdwgDimension = class(TdwgInsert)
  private
    FStyle: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgImageEnt = class(TdwgWipeout)
  private
    FImageDef: UInt64;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgPolyMesh = class(TdwgPolyline)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgMInsert = class(TdwgInsert)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgPFace = class(TdwgPolyline)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgPolyline3D = class(TdwgPolyline)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgProxy = class(TdwgUndefinedEntity)
  protected
    procedure DoReadEntProxyData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TsgDXFCADWorxSteelComponent = class(TsgDXFProxy)
  end;
  TdwgCADWorxSteelComponent = class(TdwgProxy)
  protected
    procedure ReadOwner(AReader: TdwgReader); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
  end;

  TdwgMesh = class(TdwgProxy)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgShape = class(TdwgText)
  private
    FShapeNumber: Integer;
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    function ReadExtrusion(var APBit: TsgInt64): TFPoint; override;
  end;

  TdwgTrace = class(TdwgSolid)
  end;

  TdwgVertex2D = class(TdwgVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgVertexFace = class(TdwgVertex)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimAligned = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimAngular2L = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimAngular3P = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimLinear = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimOrdinate = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDimRadius = class(TdwgDimension)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
    procedure SetFlags; virtual;
  end;

  TdwgDimDiameter = class(TdwgDimRadius)
  protected
    procedure SetFlags; override;
  end;

  TdwgArcDimension = class(TdwgDimAngular3P)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgDictionaryVar = class(TdwgObject)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgMaterial = class(TdwgDictionaryItem)
  private
    procedure ReadMaterialColor(var APBit: TsgInt64;
      AMaterialColor: TsgCustomMaterialColor);
    procedure ReadTexMatrix(var APBit: TsgInt64; AMaterialMap: TsgMaterialMap);
    function ReadMapTail(var APBit: TsgInt64;
      AMaterialMap: TsgMaterialMap): Boolean;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadOwner(AReader: TdwgReader); override;
  end;

  TdwgScale = class(TdwgObject)
  protected
    function GetOwner(AReader: TdwgReader): TsgDXFEntity; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
  end;

  TdwgObjectContextData = class(TdwgObject)
  private
    FScale: UInt64;
    FBitSize: Integer;
    FObjectBeginning: TsgInt64;

    FObjVersion: Integer;
    //FHasFile: Integer;
    FDefaultFlag: Integer;
  protected
    function GetBitSize: Integer; override;
    function GetObjectBeginning: TsgInt64; override;
    procedure SetBitSize(const AValue: Integer); override;
    procedure SetObjectBeginning(const AValue: TsgInt64); override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure ReadContext(var APBit: TsgInt64); virtual;
  end;

  TdwgBlkRefObjectContextData = class(TdwgObjectContextData)
  protected
    procedure ReadContext(var APBit: TsgInt64); override;
  end;

  TsgMLeaderHandles = record
    LineType: UInt64;
    Arrow: UInt64;
  end;

  TsgMLeaderHandlesArray = array of TsgMLeaderHandles;

  TdwgMLeaderObjectContextData = class(TdwgObjectContextData)
  private
    FTextStyle: UInt64;
    FBlockRecord: UInt64;
    FHandles: array of TsgMLeaderHandlesArray;
  protected
    procedure ReadContentHandles(var APBit: TsgInt64);
    procedure ReferencesContext(AReader: TdwgReader);
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadContext(var APBit: TsgInt64); override;
  end;

  TdwgMLeaderStyle = class(TdwgObject)
  private
    FObjectVersion: Integer;
    FLType: UInt64;
    FArrow: UInt64;
    FStyle: UInt64;
    FBlockRecord: UInt64;
  protected
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgMultiLeader = class(TdwgUndefinedEntity)
  private
    FStyle: UInt64;
    FArrow: UInt64;
    FTextStyle: UInt64;
    FLineType: UInt64;
    FBlockRecord: UInt64;
    FContext: TdwgMLeaderObjectContextData;
    FArrows: array of UInt64;
    FAttdefs: array of UInt64;
  protected
    constructor Create(const AVersion: TsgDWGVersion); override;
    destructor Destroy; override;
    function ApplyEED(const AEEData: TsgCADExtendedData;
      const AAppID: UInt64): Boolean; override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure References(AReader: TdwgReader); override;
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgRay = class(TdwgEntity)
  protected
    procedure ReadEntData(var APBit: TsgInt64); override;
  end;

  TdwgXLine = class(TdwgRay)
  end;

  TdwgSpatialFilter = class(TdwgObject)
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgGroup = class(TdwgDictionaryItem)
  protected
    procedure ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer = 0); override;
    procedure ReadData(var APBit: TsgInt64); override;
    procedure ReadSubEnts(AReader: TdwgReader); override;
  end;

  TdwgCustomIrdRecord = class(TdwgDictionaryItem)
  private
    FClassIndex: Integer;
    FID: Integer;
  protected
    procedure ReadData(var APBit: TsgInt64); override;
  end;

  TdwgIrdDscRecord = class(TdwgCustomIrdRecord)
  end;

  TdwgIrdObjRecord = class(TdwgCustomIrdRecord)
  private
    FIrdDscRecordHandle: UInt64;
  protected
    procedure References(AReader: TdwgReader); override;
    procedure ReadHandleRefs(var APBit: TsgInt64); override;
  end;

const
  cnstDataLinkCode = 106;
  cnstDictionaryCode = 42;
  cnstLimit = 109;
  cnstDwgClassTosgDwgClass: array[0..cnstLimit] of record
    Objs: TdwgClass;
    Ents: TsgDXFEntityClass;
  end = (( Objs: nil; Ents: nil),
         ( Objs: TdwgText; Ents: TsgDXFText),
         ( Objs: TdwgAttrib; Ents: TsgDXFAttrib),
         ( Objs: TdwgAttdef; Ents: TsgDXFAttdef),
         ( Objs: TdwgBlock; Ents: TsgDXFBlock),
         ( Objs: nil; Ents: TsgDXFSeqend),
         ( Objs: nil; Ents: TsgDXFSeqend),
         ( Objs: TdwgInsert; Ents: TsgDXFInsert),
         ( Objs: TdwgMInsert; Ents: TsgCADMInsert),
         ( Objs: nil; Ents: nil),
         ( Objs: TdwgVertex2D; Ents: TsgDXFVertex),//VERTEX (2D)
         ( Objs: TdwgVertex; Ents: TsgDXFVertex), //VERTEX (3D)
         ( Objs: TdwgVertex; Ents: TsgDXFVertex), //VERTEX (MESH)
         ( Objs: TdwgVertex; Ents: TsgDXFVertex), //VERTEX (PFACE)
         ( Objs: TdwgVertexFace; Ents: TsgDXFVertex), //VERTEX (PFACE FACE)
         ( Objs: TdwgPolyline; Ents: TsgDXFPolyline),
         ( Objs: TdwgPolyline3D; Ents: TsgDXFPolyline),
         ( Objs: TdwgArc; Ents: TsgDXFArc),
         ( Objs: TdwgCircle; Ents: TsgDXFCircle),
         ( Objs: TdwgLine; Ents: TsgDXFLine), 
         ( Objs: TdwgDimOrdinate; Ents: TsgDXFDimension), //20
         ( Objs: TdwgDimLinear; Ents: TsgDXFDimension),
         ( Objs: TdwgDimAligned; Ents: TsgDXFDimension),
         ( Objs: TdwgDimAngular3P; Ents: TsgDXFDimension),
         ( Objs: TdwgDimAngular2L; Ents: TsgDXFDimension),
         ( Objs: TdwgDimRadius; Ents: TsgDXFDimension),
         ( Objs: TdwgDimDiameter; Ents: TsgDXFDimension),
         ( Objs: TdwgPoint; Ents: TsgDXFPoint),
         ( Objs: Tdwg3DFace; Ents: TsgDXF3dFace),
         ( Objs: TdwgPFace; Ents: TsgDXFPolyline),
         ( Objs: TdwgPolyMesh; Ents: TsgDXFPolyline), //30
         ( Objs: TdwgSolid; Ents: TsgDXFSolid),
         ( Objs: TdwgTrace; Ents: TsgDXFTrace),
         ( Objs: TdwgShape; Ents: TsgDXFShape),
         ( Objs: TdwgViewport; Ents: TsgDXFViewport),
         ( Objs: TdwgEllipse; Ents: TsgDXFEllipse),
         ( Objs: TdwgSpline; Ents: TsgDXFSpline),
         ( Objs: TdwgRegion; Ents: TsgDXFRegion),
         ( Objs: Tdwg3dSolid; Ents: TsgDXF3dSolid),
         ( Objs: TdwgBody; Ents: TsgDXFBody), 
         ( Objs: TdwgRay; Ents: TsgDXFRay),       //40
         ( Objs: TdwgXLine; Ents: TsgDXFXline),   //41
         ( Objs: TdwgDictionary; Ents: TsgDXFDictionary), //( Objs: nil; Ents: nil), //42
         ( Objs: nil; Ents: nil), //43
         ( Objs: TdwgMText; Ents: TsgDXFMText),
         ( Objs: TdwgLeader; Ents: TsgDXFLeader),
         ( Objs: TdwgTolerance; Ents: TsgDXFTolerance),
         ( Objs: TdwgMLine; Ents: TsgCADMLine), //47
         ( Objs: TdwgBlockControl; Ents: TsgDXFBlockRecords),
         ( Objs: TdwgBlockRecord; Ents: TsgDXFBlockRecord), //49
         ( Objs: TdwgLayerControl; Ents: TsgDXFTable),
         ( Objs: TdwgLayer; Ents: TsgDXFLayer),
         ( Objs: TdwgStyleControl; Ents: TsgDXFTable),
         ( Objs: TdwgStyle; Ents: TsgDXFStyle),
         ( Objs: TdwgImageEnt; Ents: TsgDXFImageEnt),
         ( Objs: TdwgImageDef; Ents: TsgDXFImageDef),
         ( Objs: TdwgLTControl; Ents: TsgDXFTable),
         ( Objs: TdwgLType; Ents: TsgDXFLineType),
         ( Objs: TdwgSortEntsTable; Ents: TsgDXFSortEntsTable),
         ( Objs: nil; Ents: nil),
         ( Objs: nil; Ents: nil), //60
         ( Objs: nil; Ents: nil),
         ( Objs: nil; Ents: nil),
         ( Objs: nil; Ents: nil),
         ( Objs: TdwgVPortControl; Ents: TsgDXFTable),
         ( Objs: TdwgVPort; Ents: TsgDXFVPort),
         ( Objs: TdwgAppIDControl; Ents: TsgDXFTable),
         ( Objs: TdwgAppID; Ents: TsgDXFAppID), //67 - APPID
         ( Objs: TdwgDimStyleControl; Ents: TsgDXFTable),
         ( Objs: TdwgDimStyle; Ents: TsgDXFDimensionStyle),
         ( Objs: TdwgViewportEntityHeaderControl; Ents: TsgDXFViewportEntityHeaderControl),  //70
         ( Objs: TdwgViewportEntityHeader; Ents: TsgDXFViewportEntityHeader),
         ( Objs: TdwgGroup; Ents: TsgCADGroup),
         ( Objs: TdwgMLStyle; Ents: TsgMLineStyle),
         ( Objs: TdwgOLE2Frame; Ents: TsgDXFOLE2Frame),
         ( Objs: nil; Ents: nil),
         ( Objs: nil; Ents: nil),
         ( Objs: TdwgLWPline; Ents: TsgDXFLWPolyline),
         ( Objs: TdwgHatch; Ents: TsgCADHatch),
         ( Objs: TdwgXRecord; Ents: TsgDXFXRecord),
         ( Objs: nil; Ents: nil), //80
         ( Objs: nil; Ents: nil),
         ( Objs: TdwgLayout; Ents: TsgDXFLayout),
         ( Objs: TdwgProxy; Ents: TsgDXFProxy),  //83
         ( Objs: TdwgAcadTable; Ents: {$IFDEF SG_DWG_IMPORT_ACAD_TABLE}TsgDXFAcadTable{$ELSE}TsgDXFInsert{$ENDIF}),
         ( Objs: TdwgWipeout; Ents: TsgCADWipeout),
         ( Objs: TdwgWipeoutVar; Ents: TsgCADWipeoutVariables),
         ( Objs: TdwgMPolygon; Ents: TsgCADMPolygon), //this position undefined
         ( Objs: TdwgSurface; Ents: TsgDXFSurface),
         ( Objs: TdwgDbColor; Ents: TsgDXFCustomColor),
         ( Objs: TdwgAcadTableStyle; Ents: TsgDXFAcadTableStyle), // 90
         ( Objs: TdwgRasterVariables; Ents: TsgCADRasterVariables), // 91
         ( Objs: TdwgPlotSettings; Ents: TsgDXFPlotSettings), // 92
         ( Objs: TdwgDictionaryVar; Ents: TsgDXFDictionaryVar), // 93
         ( Objs: TdwgMesh; Ents: TsgDXFMesh), // 94
         ( Objs: TdwgScale; Ents: TsgDXFScale), // 95
         ( Objs: TdwgMaterial; Ents: TsgDXFMaterial), // 96
         ( Objs: TdwgObjectContextData; Ents: TsgDXFObjectContextData), // 97
         ( Objs: TdwgSpatialFilter; Ents: TsgCADSpatialFilter), // 98
         ( Objs: TdwgAcIdBlockReference; Ents: TsgCADAcIdBlockReference), // 99
         ( Objs: TdwgBlkRefObjectContextData; Ents: TsgCADBlkRefObjectContextData), // 100
         ( Objs: TdwgCADWorxSteelComponent; Ents: TsgDXFCADWorxSteelComponent), // 101
         ( Objs: TdwgMLeaderObjectContextData; Ents: TsgCADMLeaderAnnotContext), // 102
         ( Objs: TdwgMLeaderStyle; Ents: TsgCADMLeaderStyle), // 103
         ( Objs: TdwgMultiLeader; Ents: TsgCADMultiLeader), // 104
         ( Objs: TdwgAcDbViewRepBlockReference; Ents: TsgCADAcDbViewRepBlockReference), // 105
         ( Objs: TdwgDataLink; Ents: TsgCADDataLink), //106
         ( Objs: TdwgArcDimension; Ents: TsgDXFDimension), //107
         ( Objs: TdwgIrdDscRecord; Ents: TsgCADIrdDscRecord), //108
         ( Objs: TdwgIrdObjRecord; Ents: TsgCADIrdObjRecord) //109
         );

  cnstNameToCodeCount = 43;
  cnstNameToCode: array[0 .. cnstNameToCodeCount - 1] of record
    Name: string;
    Code: Byte;
  end = ((Name:'LWPOLYLINE'; Code: 77), (Name:'HATCH'; Code: 78),
         (Name:'LAYOUT'; Code: 82), (Name:'IMAGE'; Code: 54),
         (Name:'IMAGEDEF'; Code: 55), (Name:'SORTENTSTABLE'; Code: 58),
         (Name:'OLE2FRAME'; Code: 74), (Name:'ACAD_TABLE'; Code: 84),
         (Name:'WIPEOUT'; Code: 85), (Name:'WIPEOUTVARIABLES'; Code: 86),
         (Name:'MPOLYGON'; Code: 87),

         (Name:'SURFACE'; Code: 88),
         (Name:'PLANESURFACE'; Code: 88),
         (Name:'EXTRUDEDSURFACE'; Code: 88),
         (Name:'LOFTEDSURFACE'; Code: 88),
         (Name:'NURBSURFACE'; Code: 88),
         (Name:'REVOLVEDSURFACE'; Code: 88),
         (Name:'SWEPTSURFACE'; Code: 88),

         (Name:'DBCOLOR'; Code: 89), (Name:'TABLESTYLE'; Code: 90),
         (Name:sRasterVariables; Code: 91), (Name:'PLOTSETTINGS'; Code: 92),
         (Name:sAcIdBlockReference; Code: 99), (Name:'DICTIONARYVAR'; Code: 93),
         (Name:'XRECORD'; Code: 79),
         (Name:'MESH'; Code: 94),
         (Name:'SCALE'; Code: 95),
         (Name:'MATERIAL'; Code: 96),
         (Name:'ACDB_MTEXTATTRIBUTEOBJECTCONTEXTDATA_CLASS'; Code: 97),
         (Name:'ACDB_TEXTOBJECTCONTEXTDATA_CLASS'; Code: 97),
         (Name:'ACDB_LEADEROBJECTCONTEXTDATA_CLASS'; Code: 97),
         (Name:'ACDB_MTEXTOBJECTCONTEXTDATA_CLASS'; Code: 97),
         (Name:'ACDB_ALDIMOBJECTCONTEXTDATA_CLASS'; Code: 97),
         (Name:'ACDB_BLKREFOBJECTCONTEXTDATA_CLASS'; Code: 100),
         (Name:'ACDB_MLEADEROBJECTCONTEXTDATA_CLASS'; Code: 102),
         (Name:'SPATIAL_FILTER'; Code: 98),
         (Name:'CADWORXSTEELCOMPONENT'; Code: 101),
         (Name:'MLEADERSTYLE'; Code: 103),
         (Name:'MULTILEADER'; Code: 104),
         (Name:'ACDBVIEWREPBLOCKREFERENCE'; Code: 105),
         (Name:cntClassDXFARC_DIMENSION; Code: 107),
         (Name:cnstIrdDscRecord; Code: 108),
         (Name:cnstIrdObjRecord; Code: 109)
          );

var
  FiderCodeFromName: TStringList = nil;
  ToleranceProps: TStringList = nil;
  ProxyLWPLineReader: TProxyLWPLineReader = nil;
  Read3RD: TdwgReadPoint = nil;
  Read3D: TdwgReadPoint = nil;
  DictionaryName: string = cnstDictionary;
  DimBlkPropNames: array[vnDIMBLK..vnDIMLRBLK] of string = ('DIMBLK', 'DIMBLK1', 'DIMBLK2', 'DIMLDRBLK');

procedure SeekGeneral(var APBit: TsgInt64; const AParam: array of TsgSeekDWG); forward;

{Fuctions}

procedure InitFiderCodeFromName;
var
  I: Integer;
begin
  if FiderCodeFromName <> nil then
    Exit;
  FiderCodeFromName := TStringList.Create;
  FiderCodeFromName.Capacity := High(cnstNameToCode);
  FiderCodeFromName.Sorted := True;
  FiderCodeFromName.Duplicates := dupIgnore;
{$IFDEF SGDEL_6}
  FiderCodeFromName.CaseSensitive := False;
{$ENDIF}
  for I := Low(cnstNameToCode) to High(cnstNameToCode) do
    FiderCodeFromName.AddObject(cnstNameToCode[I].Name, TsgObjectWithField.CreateInt(cnstNameToCode[I].Code));
{$IFDEF DATA_LINK}
  FiderCodeFromName.AddObject('DATALINK', TsgObjectWithField.CreateInt(cnstDataLinkCode));
{$ENDIF}
  FiderCodeFromName.AddObject(cnstIrdDscDict, TsgObjectWithField.CreateInt(cnstDictionaryCode));
end;

procedure InitToleranceProps;

  procedure AddProp(const AName: string);
  var
    P: PPropInfo;
  begin
    P := GetPropInfo(TsgDXFToleranceProxy, AName);
    if P <> nil then
      ToleranceProps.AddObject(AName, TsgObjectWithField.CreatePointer(P));
  end;

begin
  if ToleranceProps = nil then
  begin
    ToleranceProps := TsgStringList.Create;
    TsgStringList(ToleranceProps).CaseSensitive := False;
    AddProp(cnstTolerancePropNameDimlineColor);
    AddProp(cnstTolerancePropNameDimScaleOverall);
    AddProp(cnstTolerancePropNameHeight);
    AddProp(cnstTolerancePropNameGAP);
    TsgStringList(ToleranceProps).Sorted := True;
  end;
end;

function sgVarIsEmpty(const AValue: Variant): Boolean;
begin
  Result := TVarData(AValue).VType = varEmpty;
end;

function CodeFromName(const AKey: string): Byte;
var
  vIndex: Integer;
begin
  if FiderCodeFromName = nil then
    InitFiderCodeFromName;
  vIndex := FiderCodeFromName.IndexOf(AKey);
  if vIndex > -1 then
    Result := TsgObjectInt64(FiderCodeFromName.Objects[vIndex]).FieldInt
  else
    Result := 0;
end;

{ DWG 2004 support }

function CreateDWGStream(ACADVer: array of AnsiChar; ALocCount,
  AStreamCapacity: Integer): TMemoryStream;
var
  I, vSize, vCountCharInVersion: Integer;
begin
  vSize := SizeOf(Tdwg2004FileHeader);
  Result := TMemoryStream.Create;
  Result.Size := vSize;
  TMemoryStreamAccess(Result).Capacity := vSize + AStreamCapacity;
  Result.Position := vSize;
  Tdwg2004FileHeader(Result.Memory^).LocCount := ALocCount;
  vCountCharInVersion := High(ACADVer);
  FillChar(Result.Memory^, vSize, 0);
  if High(Tdwg2004FileHeader(Result.Memory^).VersionID) < vCountCharInVersion then
    vCountCharInVersion := High(Tdwg2004FileHeader(Result.Memory^).VersionID);
  for I := 0 to vCountCharInVersion do
    Tdwg2004FileHeader(Result.Memory^).VersionID[I] := ACADVer[I];
end;

procedure GetCount(const AValue: TsgNativeUInt; var ASrc: PByte; var ACount: TsgNativeInt);
begin
  ACount := AValue;
  while ASrc^ = 0 do
  begin
    Inc(ACount, 255);
    Inc(ASrc);
  end;
  Inc(ACount, ASrc^);
  Inc(ASrc);
end;

//procedure DoMove(var ASrc, ADst: PByte; ACount, ATail: TsgNativeUInt;
//  AOffset: TsgNativeInt); register;
//var
//  vSrcTemp: PByte;
//begin
//  if AOffset >= 0 then
//  begin
//    vSrcTemp := ASrc;
//    ASrc := ADst;
//    Dec(ASrc, AOffset);
//    if AOffset < ACount then
//    begin
//      while ACount > 0 do
//      begin
//        ADst^ := ASrc^;
//        Inc(ASrc);
//        Inc(ADst);
//        Dec(ACount);
//      end;
//    end
//    else
//    begin
//      Move(ASrc^, ADst^, ACount);
//      Inc(ADst, ACount);
//    end;
//    ASrc := vSrcTemp;
//  end
//  else
//  begin
//    Move(ASrc^, ADst^, ACount);
//    Inc(ASrc, ACount);
//    Inc(ADst, ACount);
//  end;
//  Move(ASrc^, ADst^, ATail);
//  Inc(ASrc, ATail);
//  Inc(ADst, ATail);
//end;
procedure DoMove(var ASrc, ADst: PByte; ACount, ATail: TsgNativeUInt;
  AOffset: TsgNativeInt); register;
{$IFNDEF SG_ASSEMBLER}
var
  vSrcTemp: PByte;
begin
  if AOffset >= 0 then
  begin
    vSrcTemp := ASrc;
    ASrc := ADst;
    Dec(ASrc, AOffset);
    if AOffset < ACount then
    begin
      while ACount > 0 do
      begin
        ADst^ := ASrc^;
        Inc(ASrc);
        Inc(ADst);
        Dec(ACount);
      end;
    end
    else
    begin
      Move(ASrc^, ADst^, ACount);
      Inc(ADst, ACount);
    end;
    ASrc := vSrcTemp;
  end
  else
  begin
    Move(ASrc^, ADst^, ACount);
    Inc(ASrc, ACount);
    Inc(ADst, ACount);
  end;
  Move(ASrc^, ADst^, ATail);
  Inc(ASrc, ATail);
  Inc(ADst, ATail);
end;
{$ELSE}
{$IFDEF SG_CPUX64}
asm
    PUSH      RSI
    PUSH      RDI
    MOV       RSI,[RCX]     // ASrc
    XCHG      RCX,R8        // R8=ASrc, RCX=ACount
    MOV       RDI,[RDX]     // ADst
    MOV       RAX,AOffset
    OR        RAX,RAX
    JS        @@1
    MOV       RSI,RDI
    SUB       RSI,RAX
@@1: REP       MOVSB
    OR        RAX,RAX
    JNS       @@2
    MOV       [R8],RSI
@@2: MOV       RSI,[R8]
    MOV       RCX,R9        // ATail
    REP       MOVSB
    MOV       [R8],RSI
    MOV       [RDX],RDI
    POP       RDI
    POP       RSI
end;
{$ELSE}
// EAX = ASrc
// EDX = ADst
// ECX = ACount
asm
    PUSH      ESI
    PUSH      EDI
    PUSH      EBX
    MOV       EBX,EAX
    MOV       ESI,[EAX]
    MOV       EDI,[EDX]
    MOV       EAX,AOffset
    OR        EAX,EAX
    JS        @@1
    MOV       ESI,EDI
    SUB       ESI,EAX
@@1: REP       MOVSB
    OR        EAX,EAX
    JNS       @@2
    MOV       [EBX],ESI
@@2: MOV       ESI,[EBX]
    MOV       ECX,ATail
    REP       MOVSB
    MOV       [EBX],ESI
    MOV       [EDX],EDI
    POP       EBX
    POP       EDI
    POP       ESI
end;
{$ENDIF}
{$ENDIF}
procedure DoDecode2004(AStream: TMemoryStream; ASrc: PByte;
  APSize, AUSize: Integer);
var
  vDst, vLimit, vLimit2: PByte;
  vCount: TsgNativeInt;
  vCount1, vTail: TsgNativeUInt;
  vOffset: TsgNativeInt;
  vCode: Byte;
begin
  vCount := AStream.Position + AUSize;
//  if USize > $7400 then
//    USize := $7400;
  if AStream.Size < vCount then
    AStream.Size := vCount;
  vDst := AStream.Memory;
  Inc(vDst, AStream.Position);
  AStream.Position := AStream.Position + AUSize;
  vLimit := vDst;
  Inc(vLimit, AUSize);
  vLimit2 := ASrc;
  Inc(vLimit2,APSize);
  while (PAnsiChar(vDst) < PAnsiChar(vLimit)) and
        (PAnsiChar(ASrc) < PAnsiChar(vLimit2)) do
  begin
    vCode := ASrc^;
    Inc(ASrc);
    case vCode of
      $00..$0F:
        begin
          if vCode=0 then
            GetCount(18, ASrc, vCount)
          else
            vCount := vCode + 3;
          vOffset := TsgNativeInt(1) shl (SizeOf(TsgNativeInt) shl 3 - 1);
        end;
      $10..$1F:
        begin
          if vCode=$10 then
            GetCount(9, ASrc, vCount)
          else
            vCount := vCode and $F + 2;
          vOffset := PWord(ASrc)^ + $10000 - 4; Inc(ASrc,2);
        end;
      $20..$3F:
        begin
          if vCode=$20 then
            GetCount(33, ASrc, vCount)
          else
            vCount := vCode and $1F + 2;
          vOffset := PWord(ASrc)^; Inc(ASrc,2);
        end;
      else
        begin
          vCount := vCode shr 4 - 1;
          vOffset := ASrc^ shl 4 + vCode and $F;
          Inc(ASrc);
        end;
    end;
    vTail := vOffset and 3;
    if vOffset >= 0 then
      vOffset := vOffset shr 2 + 1;
    vCount1 := TsgNativeUInt(vLimit) - TsgNativeUInt(vDst) - vTail;
    if vCount > TsgNativeInt(vCount1) then
    begin
      vCount := vCount1;
      if vCount < 0 then
      begin
        vCount := TsgNativeUInt(vLimit) - TsgNativeUInt(vDst);
        vTail := 0;
      end;
    end;
    DoMove(ASrc, vDst, vCount, vTail, vOffset);
  end;
end;

procedure Decode2004(const APointer: Pointer; AStream: TMemoryStream);
var
  vSrc, vDst: PByte;
  vPSize, vUSize: Integer;
  vPHead: PInteger;
begin
  vSrc := APointer;
  Inc(vSrc,$14);
  Move(APointer, vPHead, SizeOf(vPHead));
//  vPHead := P;
  Inc(vPHead);
  vUSize := PInteger(vPHead)^ + 4;
  Inc(vPHead);
  vPSize := vPHead^ - 3;
  AStream.Size := 0;
  DoDecode2004(AStream, vSrc, vPSize, vUSize);
  vDst := AStream.Memory;
  Inc(vDst, AStream.Size-4);
  PInteger(vDst)^ := 0;
end;

function ExpandLWeight(const ALWeight: Double): Double;
begin
  if ALWeight <= 0 then
    Result := ALWeight
  else
    Result := 0.01 * ALWeight;
end;

procedure ScanHeader2004(AStream: TMemoryStream; var ADst: array of PHead2004Rec);
var
  vPHead2004,vLimit: PHead2004Rec;
  I: Integer;
  vName: AnsiString;
begin
  for I := 0 to High(ADst) do
    ADst[I] := nil;
  vPHead2004 := AStream.Memory;
  vLimit := vPHead2004;
  Inc(PByte(vPHead2004), $74);
  Inc(PByte(vLimit), AStream.Size-$60);
  while PAnsiChar(vPHead2004) < PAnsiChar(vLimit) do
  begin
    vName := vPHead2004.Name;
    if vName = '' then
      Exit;
    if vName = 'Header' then
      I := cntDWGSectHeaderIndex
    else
      if vName = 'Classes' then
        I := cntDWGSectClassesIndex
    else
      if vName = 'Handles' then
        I := cntDWGSectHandlesIndex
    else
      if vName = 'AcDbObjects' then
        I := cntDWGSectObjectsIndex
    else
      if vName = 'SummaryInfo' then
        I := cntDWGSectSummaryInfoIndex
    else
      I := -1;
    if (I >= 0) and (vPHead2004.Extra > 0) then
    begin
      ADst[I] := vPHead2004;
      if I=1 then Dec(vPHead2004.UnpackSize, 8);
    end;
    Inc(PByte(vPHead2004), vPHead2004.Extra shl 4 + $60);
  end;
end;

function SecSize(ANumber: array of Integer;
  ASections: array of PHead2004Rec): Integer;
var
  I : Integer;
begin
  Result := 0;
  for I := Low(ANumber) to High(ANumber) do
  begin
    if ASections[ANumber[I]] <> nil then
      Inc(Result,ASections[ANumber[I]].UnpackSize);
  end;
end;

procedure DoSection2004(AStream: TCustomMemoryStream; ANumber: array of Integer;
  ADwgSteram: TMemoryStream; ASections: TsgSectionsArray;
  vTable: TsgTableArray);
var
  vPFileHeader2004: Pdwg2004FileHeader;
  vPHead: PHead2004Rec;
  vSrc: PByte;
  I, U,V,E: Integer;
begin
  for I := Low(ANumber) to High(ANumber) do
  begin
    vPFileHeader2004 := ADwgSteram.Memory;
    vPHead := ASections[ANumber[I]];
    if vPHead <>nil then
    begin
      U := vPHead.UnpackSize;
      E := vPHead.Extra;
      vPFileHeader2004.Locators[ANumber[I]].RecNumber := ANumber[I];
      vPFileHeader2004.Locators[ANumber[I]].Seeker := ADwgSteram.Size;
      vPFileHeader2004.Locators[ANumber[I]].Size := vPHead.UnpackSize;
      while E > 0 do
      begin
        if vPHead.Index and not 4095 <> 0 then
          Break;
        vSrc := AStream.Memory;
        Inc(vSrc, vTable[vPHead.Index]);
        V := U;
        if V > $7400 then
          V := $7400;
        DoDecode2004(ADwgSteram, vSrc, vPHead.PackSize, V);
        Inc(PAnsiChar(vPHead),$10);
        Dec(U,$7400);
        Dec(E);
      end;
    end;
  end;
end;

function Unpack2004(AVesrsion: TsgDWGVersion; AStream: TCustomMemoryStream;
  var ASecCount: Integer): TMemoryStream;
var
  vOutStream: TMemoryStream;
  vDwgSteram: TMemoryStream;
  vPByte: PByte;
  vPInteger: PInteger;
  I,N,RS: Integer;
  vRoot: array[0..127] of Byte;
  vTable: TsgTableArray;
  vSections: TsgSectionsArray;
begin
//  Result := nil;
  vOutStream := TMemoryStream.Create;
  try
    ASecCount := 5;

    vPByte := PByte(AStream.Memory);
    Inc(vPByte,$80);
    RS := 1;
    for I := 0 to $7F do begin
      RS := RS * $343FD + $269EC3;
      vRoot[I] := vPByte^ xor (RS shr 16);
      Inc(vPByte);
    end;
    vPByte := @vRoot[$54];
    I := PInteger(vPByte)^ + $100;
    vPByte := PByte(AStream.Memory);
    Inc(vPByte,I);
    Decode2004(vPByte,vOutStream);
    FillChar(vTable[0], SizeOf(vTable), 0);
    vPInteger := Pinteger(vOutStream.Memory);
    RS := $120;
    N := vOutStream.Size shr 3;
    while N > 0 do
    begin
      I := vPInteger^; Inc(vPInteger);
      if I and not 4095 = 0 then vTable[I] := RS;
      I := vPInteger^; Inc(vPInteger);
      if (I >= 0) and (I < AStream.Size) then Inc(RS,I);
      Dec(N);
    end;
    vPByte := @vRoot[$5C];
    I := vTable[PInteger(vPByte)^] - $20;
    vPByte := PByte(AStream.Memory);
    Inc(vPByte,I);
    Decode2004(vPByte,vOutStream);
    ScanHeader2004(vOutStream,vSections);
    vDwgSteram := CreateDWGStream(['A', 'C', '1', '0', '1', '8'], ASecCount,
      SecSize([0, 1, 2, 3, 4], vSections));
    DoSection2004(AStream,[0, 1, 2, 3, 4],vDwgSteram,vSections,vTable);
    Result := vDwgSteram;
  finally
    vOutStream.Free;
  end;
end;


{ DWG 2007 support }

{$IFNDEF SGDEL_4}
function UnpackR18Plus(AStream: TCustomMemoryStream): TMemoryStream;
begin
  Result := nil;
  raise EdwgError.CreateFmt(sDWGCompilerError, ['2007']);
end;

function UnpackR24(AStream: TCustomMemoryStream): TMemoryStream;
begin
  Result := nil;
  raise EdwgError.CreateFmt(sDWGCompilerError, ['2010']);
end;

{$ELSE}

function Page2007Decode(Dest, Source: PAnsiChar; DstSize, SrcSize, Factor: Cardinal;
  const ACoder: TsgRSCoder): Boolean;
var
  vBuf: array[0..$FF + 1] of Byte;
  I, vDataSize, vCopySize: Cardinal;
  vLen: Integer;
  vCurDst, vCurSrc, vDstEnd, vSrcEnd: PAnsiChar;
begin
  Result := True;
  vDataSize := DstSize;
  vDstEnd := Dest;
  Inc(vDstEnd, DstSize);
  vSrcEnd := Source;
  Inc(vSrcEnd, SrcSize);
  I := 0;
  while I < Factor do
  begin
    if Dest > vDstEnd then Exit;
    if Source > vSrcEnd then Exit;
    vCurDst := @vBuf;
    vCurSrc := Source;
    vLen := $FF - 1;
    while vLen >= 0 do
    begin
      if vCurSrc > vSrcEnd then Exit;
      vCurDst^ := vCurSrc^;
      Inc(vCurSrc, Factor);
      Inc(vCurDst);
      Dec(vLen);
    end;
    if ACoder.Decode(PAnsiChar(@vBuf)) < 0 then
      raise EsgRSCoderError.Create(sRSDecoderError);
    vCopySize := Math.Min(vDataSize, ACoder.DSize);
    Dec(vDataSize, vCopySize);
    Move(vBuf, Dest^, vCopySize);
    Inc(Dest, vCopySize);
    Inc(Source);
    Inc(I);
  end;
end;

function LoadPage(FileOffset: Cardinal; SizeCompr, SizeSrc, Factor: Int64;
  AStream: TCustomMemoryStream; ACoder: TsgRSCoder;
  ADecomp: TsgDWG2007Decompressor; var Data: PByte; var DataSize: Int64): Integer;
var
  vSizeSrc, vSizeCompr, vAlignedSize, vDstSize: Int64;
  vFactor, vFilePageSize: Cardinal;
  vPageData, vCmprData: PByte;
begin
  Result := 0;
  vSizeCompr := SizeCompr;
  vSizeSrc := SizeSrc;
  vAlignedSize := (vSizeCompr + 7) and $FFFFFFF8;
  vDstSize := vAlignedSize * Factor;
  vFactor := (ACoder.DSize + vDstSize - 1) div ACoder.DSize;
  vFilePageSize := (vFactor * $FF + $FF) and $FFFFFF00;
  GetMem(vPageData, vFilePageSize);
  vCmprData := nil;
  try
    AStream.Position := $80 + $400 + FileOffset;
    AStream.Read(vPageData^, vFilePageSize);
    GetMem(vCmprData, vDstSize);
    Page2007Decode(PAnsiChar(vCmprData), PAnsiChar(vPageData), vDstSize, vFilePageSize,
      vFactor, ACoder);
    ADecomp.Decompress(PAnsiChar(vCmprData), vSizeCompr, PAnsiChar(vPageData), vSizeSrc);
    GetMem(Data, vSizeSrc);
    Move(vPageData^, Data^, vSizeSrc);
    DataSize := vSizeSrc;
  finally
    if Assigned(vCmprData) then
      FreeMem(vCmprData);
    FreeMem(vPageData);
  end;
end;

procedure CreateDWG2007PgsLst(var APages: TList; const APagesMaxId: UInt64);
var
  I: Cardinal;
  vPPage: PsgDWG2007Page;
begin
  if APages = nil then
    APages := TList.Create;
  I := 0;
  while I <= APagesMaxId + 1 do
  begin
    New(vPPage);
    FillChar(vPPage^, SizeOf(TsgDWG2007Page), 0);
    APages.Add(vPPage);
    Inc(I);
  end;
end;

procedure ClearDWG2007PgsLst(var APages: TList);
begin
  ClearRecordList(APages);
  APages.Free;
end;

function IsSectonAllowed(const ASectName: string;
  const AddSections: array of string): Boolean;
var
  I: Integer;
  S: string;
begin
  S := UpperCase(ASectName);
  Result := False;
  I := 0;
  while not Result and (I <= High(cntDWGSections)) do
    if UpperCase(string(cntDWGSections[I])) = S then
      Inc(Result)
    else
      Inc(I);
  if not Result then
    for I := Low(AddSections) to High(AddSections) do
    begin
      Result := UpperCase(AddSections[I]) = S;
      if Result then Break;
    end;
end;

function GetSecIndByName(const ASectionName: WideString): Integer; overload;
begin
  Result := -1;
  if ASectionName = sAcadSectHeader then
    Result := cntDWGSectHeaderIndex;
  if ASectionName = sAcadSectClasses then
    Result := cntDWGSectClassesIndex;
  if ASectionName = sAcadSectHandles then
    Result := cntDWGSectHandlesIndex;
  if ASectionName = sAcadSectObjects then
    Result := cntDWGSectObjectsIndex;
  if ASectionName = sAcadSectSummaryInfo then
    Result := cntDWGSectSummaryInfoIndex;
  if ASectionName = sAcadSectTemplate then
    Result := cntDWGSectTemplateIndex;
  if ASectionName = sAcadSectAppInfo then
    Result := cntDWGSectAppInfoIndex;
end;

function GetSecIndByName(const ASectionName: PAnsiChar): Integer; overload;
begin
  Result := GetSecIndByName(WideString(AnsiString(ASectionName)));
end;

procedure DoSection(PHead: Pdwg2004FileHeader; ANumber: Integer;
  const ASecStream, AStream: TStream);
begin
  PHead^.Locators[ANumber].RecNumber := ANumber;
  if ASecStream = nil then
  begin
    PHead^.Locators[ANumber].Seeker := 0;
    PHead^.Locators[ANumber].Size := 0;
  end
  else
  begin
    PHead^.Locators[ANumber].Seeker := AStream.Size;
    PHead^.Locators[ANumber].Size := ASecStream.Size;
    AStream.CopyFrom(ASecStream, 0);
  end;
end;

function UnpackR18Plus(AVersion: TsgDWGVersion; AStream: TCustomMemoryStream;
  var ASecCount: Integer): TMemoryStream;
const
  cFactor = 3;
  cPoly0: array[0..7] of Byte = (1, 0, 1, 1, 1, 0, 0, 0);
  cPoly1: array[0..7] of Byte = (1, 0, 0, 1, 0, 1, 1, 0);
  cFileHeaderSize = $110;
  cInt64SysSize = SizeOf(Int64);
var
  vChData: TsgDWG2007ChData;
  vSize, vId, vOffset: Int64;
  vStream: TMemoryStream;
  vDstSize, vVal, vLen1, vLen2: Cardinal;
  vRSSys, vRSSec: TsgRSCoder;
  vCmprData, vData: PByte;
  vDecomp: TsgDWG2007Decompressor;
  vHeader: TsgDwg2007FileHeader;
  vPages: TList;
  I, J: Integer;
  vPage: TsgDWG2007Page;
  PPage: PsgDWG2007Page;
  vSection: TsgDwg2007Section;
  vSections: array[0..cntDWGSectCount] of TsgDwg2007Section;
  PSrmPage: PsgDwg2007StreamPage;
  PHead: Pdwg2004FileHeader;

  procedure StreamPageDecode(var ASection: TsgDwg2007Section;
    const APage: TsgDWG2007StreamPage);
  var
    vId, vPos, vOffset, vPageDstSize, vFactor, vAlignedSize: Int64;
    vSecData, P: PByte;
    PPage: PsgDwg2007Page;
  begin
    vId := APage.PageId;
    if vId < 0 then
      PPage := vPages[0 - vId]
    else
      PPage := vPages[vId];
    vOffset := $80 + $400 + PPage^.Offset;

    GetMem(vSecData, PPage^.Size);
    try
      AStream.Position := vOffset;
      AStream.Read(vSecData^, PPage^.Size);

      vAlignedSize := ((APage.ComprSize + 8) - 1) and $FFFFFFF8;
      if ASection.Encoded = 4 then
      begin
        vFactor := (vRSSec.DSize + vAlignedSize - 1) div vRSSec.DSize;
        vPageDstSize := vFactor * vRSSec.DSize;
        GetMem(P, vPageDstSize);
        try
          Page2007Decode(PAnsiChar(P), PAnsiChar(vSecData), vPageDstSize, PPage^.Size,
            vFactor, vRSSec);
          FreeMem(vSecData);
          GetMem(vSecData, vPageDstSize);
          Move(P^, vSecData^, vPageDstSize);
        finally
          FreeMem(P);
        end;
      end;
      if ASection.Data = nil then
        ASection.Data := TMemoryStream.Create;
      ASection.Data.Position := ASection.Data.Size;
      vPos := ASection.Data.Position;
      ASection.Data.Size := ASection.Data.Size + APage.DataSize;
      P := ASection.Data.Memory;
      Inc(P, vPos);
      FillChar(P^, APage.DataSize, 0);
      if APage.DataSize <> APage.ComprSize then
        vDecomp.Decompress(PAnsiChar(vSecData), APage.ComprSize, PAnsiChar(P), APage.DataSize)
      else
        Move(vSecData^, P^, APage.DataSize);
    finally
      FreeMem(vSecData);
    end;
  end;

  function GetSectionsSummmary: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to High(vSections) do
      if (vSections[I].Data <> nil) then
        Result := Result + vSections[I].Data.Size;
  end;

  procedure ClearSection(var ASection: TsgDwg2007Section);
  begin
    FillChar(ASection, cInt64SysSize * 8, 0);
    ASection.SecName := '';
    ASection.Pages := nil;
    ASection.Data := nil;
  end;

begin
  FillChar(vChData, SizeOf(TsgDWG2007ChData), 0);
  vStream := TMemoryStream.Create;
  {$IFNDEF SGDEL_10_SEATTLE}
  Result := nil;
  {$ENDIF}
  vCmprData := nil;
  vData := nil;
  vDecomp := nil;
  vRSSys := nil;
  vRSSec := nil;
  vPages := nil;
  ASecCount := 7;
  try
    AStream.Position := $80;
    vStream.CopyFrom(AStream, $400);
    GetMem(vData, vStream.Size);
    vStream.Position := 0;
    vStream.Read(vData^, vStream.Size);

    vStream.Position := $3D8;
    vStream.Read(vChData, SizeOf(TsgDWG2007ChData));

    vRSSec := TsgRSCoder.Create;
    vRSSec.Generate(cPoly0, 8, 2);

    vRSSys := TsgRSCoder.Create;
    vRSSys.Generate(cPoly1, 8, 8);

    vDstSize := vRSSys.DSize + 2 * vRSSys.DSize;

    GetMem(vCmprData, vDstSize);
    Page2007Decode(PAnsiChar(vCmprData), PAnsiChar(vData), vDstSize, $400, cFactor, vRSSys);

    vStream.Clear;
    vStream.Write(vCmprData^, vDstSize);
    (*
      vCmprData is Header page info
      The first 16 bytes are CRC of Header page (two Int64 values CheckCrc and CheckVal), we skip this information.
      Next is CRC of compressed data, we skip it also
    *)
    vStream.Position := 24;
    vStream.Read(vLen1, SizeOf(vLen1));
    vStream.Read(vLen2, SizeOf(vLen2));
    if vLen1 > cFileHeaderSize then
      raise EdwgError.Create(sDWG2007HeaderError);

    vVal := vDstSize - vStream.Position;
    FreeMem(vData);
    GetMem(vData, cFileHeaderSize);

    vStream.Read(vCmprData^, vVal);
    vDecomp := TsgDWG2007Decompressor.Create;
    vDecomp.Decompress(PAnsiChar(vCmprData), vLen1, PAnsiChar(vData), cFileHeaderSize);
    vStream.Clear;
    vStream.Write(vData^, cFileHeaderSize);
    vStream.Position := 0;
    vStream.Read(vHeader, SizeOf(TsgDwg2007FileHeader));

    FreeMem(vData);//PagesMap load begin
    LoadPage(vHeader.PagesMapOffset, vHeader.PagesMapSizeCompr,
      vHeader.PagesMapSizeSrc, vHeader.PagesMapFactor,
      AStream, vRSSys, vDecomp, vData, vSize);

    vStream.Clear;
    vStream.Write(vData^, vSize);
    vStream.Position := 0;

    CreateDWG2007PgsLst(vPages, vHeader.PagesMaxId);

    vOffset := 0;
    while True do
    begin
      if vStream.Read(vSize, cInt64SysSize) = 0 then Break;
      if vStream.Read(vId, cInt64SysSize) = 0 then Break;
      if Abs(vId) > vHeader.PagesMaxId then Continue;
      if vId < 0 then
        PPage := vPages[0 - vId]
      else
        PPage := vPages[vId];
      PPage^.Id := vId;
      PPage^.Size := vSize;
      PPage^.Offset := vOffset;
      Inc(vOffset, vSize);
    end;//PagesMap is loaded

    vPage := PsgDwg2007Page(vPages[vHeader.SectionsMapId])^;
    FreeMem(vData);

    LoadPage(vPage.Offset, vHeader.SectionsMapSizeCompr,
      vHeader.SectionsMapSizeSrc, vHeader.SectionsMapFactor,
      AStream, vRSSys, vDecomp, vData, vSize);

    vStream.Clear;
    vStream.Write(vData^, vSize);
    vStream.Position := 0;
    for I := 0 to High(vSections) do
      ClearSection(vSections[I]);
    while True do
    begin
      ClearSection(vSection);
      if vStream.Position = vStream.Size then Break;

      if vStream.Read(vSection, cInt64SysSize * 8) <> cInt64SysSize * 8 then Break;
      if (vSection.SecNameLen = 0) and (vSection.PagesAmount = 0) then Continue;

      if vSection.SecNameLen > 0 then
      begin
        SetLength(vSection.SecName, vSection.SecNameLen shr 1);
        if vStream.Read(vSection.SecName[1], vSection.SecNameLen) <> vSection.SecNameLen then Break;
        if vSection.SecName[Length(vSection.SecName)] = #0 then
            vSection.SecName := Copy(vSection.SecName, 1, Length(vSection.SecName) - 1);
      end;

      if vStream.Position = vStream.Size then Break;

      if (vSection.PagesAmount = 0) or (vSection.PagesAmount < 0) or
        (vSection.PagesAmount > vHeader.PagesMaxId + 1) then Continue;

      if not IsSectonAllowed(string(vSection.SecName), []) then
      begin
        vStream.Position := vStream.Position + vSection.PagesAmount * SizeOf(TsgDWG2007StreamPage);
        Continue;
      end;

      vSection.Pages := TList.Create;
      for I := 0 to vSection.PagesAmount - 1 do
      begin
        New(PSrmPage);
        vStream.Read(PSrmPage^, SizeOf(TsgDWG2007StreamPage));
        vSection.Pages.Add(PSrmPage);
      end;

      for I := 0 to vSection.Pages.Count - 1 do
      begin
        try
          StreamPageDecode(vSection, PsgDwg2007StreamPage(vSection.Pages[I])^);
        except
          Continue;
        end;
      end;
      vSections[GetSecIndByName(vSection.SecName)] := vSection;
    end;
    Result := CreateDWGStream(['A', 'C', '1', '0', '2', '1'], ASecCount,
      GetSectionsSummmary);
    for I := 0 to High(vSections) do
    begin
      if vSections[I].Data = nil then
        Continue;
      PHead := Result.Memory;
      DoSection(PHead, I, vSections[I].Data, Result);
      vSections[I].Data.Free;
      for J := 0 to vSections[I].Pages.Count - 1 do
        Dispose(vSections[I].Pages[J]);
      vSections[I].Pages.Free;
      vSections[I].SecName := '';
    end;
  finally
    if vCmprData <> nil then
      FreeMem(vCmprData);
    if vData <> nil then
      FreeMem(vData);
    vDecomp.Free;
    vStream.Free;
    vRSSec.Free;
    vRSSys.Free;
    ClearDWG2007PgsLst(vPages);
  end;
end;

{ TsgDWG2007Decompressor }

{private}

procedure TsgDWG2007Decompressor.GetCmpData(ADest: PAnsiChar; ALength,
  AOffset: Integer);
var
  vDstPrev: PAnsiChar;
begin
  vDstPrev := ADest;
  Dec(vDstPrev, AOffset);
  if ADest > FDstEnd then
    Exit;
  if ALength < AOffset then
  begin
    if ADest + ALength <= FDstEnd then
      Move(vDstPrev^, ADest^, ALength)
  end
  else
    while ALength > 0 do
    begin
      Dec(ALength);
      ADest^ := vDstPrev^;
      Inc(ADest);
      if ADest > FDstEnd then Break;
      Inc(vDstPrev);
    end;
end;

procedure TsgDWG2007Decompressor.GetCmpOpcode;
var
  vVal: Cardinal;
begin
  vVal := FOpCode shr 4;
  case vVal of
    0:
      begin
        FLength := (FOpCode and $0F) + $13;
        FOffset := Byte(FSrc^);
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FOpCode := Byte(FSrc^);
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FLength := ((FOpCode shr 3) and $10) + FLength;
        FOffset := ((FOpCode and $78) shl 5) + 1 + FOffset;
      end;
    1:
      begin
        FLength := (FOpCode and $0F) + 3;
        FOffset := Byte(FSrc^);
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FOpCode := Byte(FSrc^);
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FOffset := ((FOpCode and $F8) shl 5) + FOffset + 1;
      end;
    2:
      begin
        FOffset := Byte(FSrc^);
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FOffset := ((Byte(FSrc^) shl 8) and $0FF00) or FOffset;
        Inc(FSrc);
        if FSrc > FSrcEnd then
          Exit;
        FLength := FOpCode and 7;
        if (FOpCode and 8) = 0 then
        begin
          FOpCode := Byte(FSrc^);
          Inc(FSrc);
          if FSrc > FSrcEnd then Exit;
          FLength := (FOpCode and $0F8) + FLength;
        end
        else
        begin
          Inc(FOffset);
          FLength := Byte(FSrc^) * 8 + FLength;
          Inc(FSrc);
          if FSrc > FSrcEnd then Exit;
          FOpCode := Byte(FSrc^);
          Inc(FSrc);
          if FSrc > FSrcEnd then Exit;
          FLength := ((FOpCode and $0F8) shl 8) + FLength + $100;
        end;
      end;
  else
    begin
      FLength := FOpCode shr 4;
      FOffset := FOpCode and $0F;
      FOpCode := Byte(FSrc^);
      Inc(FSrc);
      if FSrc > FSrcEnd then Exit;
      FOffset := (FOpCode and $0F8) * 2 + FOffset + 1;
    end;
  end;
end;


type
  TsgMover = (mv0, mv1, mv2, mv3, mv4, mv5, mv6, mv7, mv8, mv9, mvA, mvB, mvC,
    mvD, mvE, mvF, mv10, mv11, mv12, mv13, mv14, mv15, mv16, mv17, mv18, mv19,
    mv1A, mv1B, mv1C, mv1D, mv1E, mv1F, mvO1, mvO2, mvO3, mvO4, mvO8, mvO16);

procedure MassOperation(ADest, ASource: PAnsiChar; AMassOper: array of TsgMover);
var
  I: Integer;
  vDest, vSource: PAnsiChar;
begin
  for I := Low(AMassOper) to (High(AMassOper) div 3) do
  begin
    vDest := ADest+Ord(AMassOper[I*3+1]);
    vSource := ASource+Ord(AMassOper[I*3+2]);
    case TsgMover(AMassOper[I*3]) of
      mvO1: vDest^ := vSource^;
      mvO2: begin
               vDest[0] := vSource[1];
               vDest[1] := vSource[0];
             end;
      mvO3: begin
               vDest[0] := vSource[2];
               vDest[1] := vSource[1];
               vDest[2] := vSource[0];
             end;
      mvO4: PCardinal(vDest)^ := PCardinal(vSource)^;
      mvO8: begin
               PCardinal(vDest)^ := PCardinal(vSource)^;
               PCardinal(vDest+4)^ := PCardinal(vSource+4)^;
             end;
      mvO16: begin
                PCardinal(vDest)^ := PCardinal(vSource+8)^;
                PCardinal(vDest+4)^ := PCardinal(vSource+8+4)^;
                PCardinal(vDest+8)^ := PCardinal(vSource)^;
                PCardinal(vDest+8+4)^ := PCardinal(vSource+4)^;
             end;
    end;
  end;
end;

procedure TsgDWG2007Decompressor.GetLiteralData(ADest, ASource: PAnsiChar; ALength: Integer);
var
  vLen: Integer;
begin
  while ALength >= $20 do
  begin
    MassOperation(ADest, ASource,[mvO4, mv0, mv18, mvO4, mv4, mv1C,
      mvO4, mv8, mv10, mvO4, mvC, mv14, mvO4, mv10, mv8, mvO4, mv14, mvC,
      mvO4, mv14, mvC, mvO4, mv18, mv0, mvO4, mv1C, mv4]);
    Inc(ASource, $20);
    Inc(ADest, $20);
    Dec(ALength, $20);
  end;
  vLen := ALength - 1;

  if vLen > $1E then
    Exit;

  case vLen of
    0: MassOperation(ADest, ASource,[mvO1,mv0,mv0]);
    1: MassOperation(ADest, ASource,[mvO2,mv0,mv0]);
    2: MassOperation(ADest, ASource,[mvO3,mv0,mv0]);
    3: MassOperation(ADest, ASource,[mvO4,mv0,mv0]);
    4: MassOperation(ADest, ASource,[mvO1,mv0,mv4,mvO4,mv1,mv0]);
    5: MassOperation(ADest, ASource,[mvO1,mv0,mv5,mvO4,mv1,mv1,mvO1,mv5,mv0]);
    6: MassOperation(ADest, ASource,[mvO2,mv0,mv5,mvO4,mv2,mv1,mvO1,mv6,mv0]);
    7: MassOperation(ADest, ASource,[mvO8,mv0,mv0]);
    8: MassOperation(ADest, ASource,[mvO1,mv0,mv8,mvO8,mv1,mv0]);
    9: MassOperation(ADest, ASource,[mvO1,mv0,mv9,mvO8,mv1,mv1,mvO1,mv9,mv0]);
    10: MassOperation(ADest, ASource,[mvO2,mv0,mv9,mvO8,mv2,mv1,mvO1,mvA,mv0]);
    11: MassOperation(ADest, ASource,[mvO4,mv0,mv8,mvO8,mv4,mv0]);
    12: MassOperation(ADest, ASource,[mvO1,mv0,mvC,mvO4,mv1,mv8,mvO8,mv5,mv0]);
    13: MassOperation(ADest, ASource,[mvO1,mv0,mvD,mvO4,mv1,mv9,
          mvO8,mv5,mv1,mvO1,mvD,mv0]);
    14: MassOperation(ADest, ASource,[mvO2,mv0,mvD,mvO4,mv2,mv9,
          mvO8,mv6,mv1,mvO1,mvE,mv0]);
    15: MassOperation(ADest, ASource,[mvO16,mv0,mv0]);
    16: MassOperation(ADest, ASource,[mvO8,mv0,mv9,mvO1,mv8,mv8,mvO8,mv9,mv0]);
    17: MassOperation(ADest, ASource,[mvO1,mv0,mv11,mvO16,mv1,mv1,
          mvO1,mv11,mv0]);
    18: MassOperation(ADest, ASource,[mvO3,mv0,mv10,mvO16,mv3,mv0]);
    19: MassOperation(ADest, ASource,[mvO4,mv0,mv10,mvO8,mv4,mv8,mvO8,mvC,mv0]);
    20: MassOperation(ADest, ASource,[mvO1,mv0,mv14,mvO4,mv1,mv10,
          mvO8,mv5,mv8,mvO8,mvD,mv0]);
    21: MassOperation(ADest, ASource,[mvO2,mv0,mv14,mvO4,mv2,mv10,
          mvO8,mv6,mv8,mvO8,mvE,mv0]);
    22: MassOperation(ADest, ASource,[mvO3,mv0,mv14,mvO4,mv3,mv10,
          mvO8,mv7,mv8,mvO8,mvF,mv0]);
    23: MassOperation(ADest, ASource,[mvO8,mv0,mv10,mvO16,mv8,mv0]);
    24: MassOperation(ADest, ASource,[mvO8,mv0,mv11,mvO1,mv8,mv10,
          mvO16,mv9,mv0]);
    25: MassOperation(ADest, ASource,[mvO1,mv0,mv19,mvO8,mv1,mv11,
          mvO1,mv9,mv10,mvO16,mvA,mv0]);
    26: MassOperation(ADest, ASource,[mvO2,mv0,mv19,mvO8,mv2,mv11,
          mvO1,mvA,mv10,mvO16,mvB,mv0]);
    27: MassOperation(ADest, ASource,[mvO4,mv0,mv18,mvO8,mv4,mv10,
          mvO8,mvC,mv8,mvO8,mv14,mv0]);
    28: MassOperation(ADest, ASource,[mvO1,mv0,mv1C,mvO4,mv1,mv18,
          mvO8,mv5,mv10,mvO8,mvD,mv8,mvO8,mv15,mv0]);
    29: MassOperation(ADest, ASource,[mvO2,mv0,mv1C,mvO4,mv2,mv18,
          mvO8,mv6,mv10,mvO8,mvE,mv8,mvO8,mv16,mv0]);
    30: MassOperation(ADest, ASource,[mvO1,mv0,mv1E,mvO4,mv1,mv1A,
          mvO8,mv5,mv12,mvO8,mvD,mvA,mvO8,mv15,mv2,mvO2,mv1D,mv0]);
  end;
end;

procedure TsgDWG2007Decompressor.GetLiteralOpcode;
var
  vVal1, vVal2: Cardinal;
begin
  Inc(FLength, 8);
  if FLength <> $17 then
    Exit;

  vVal1 := Byte(FSrc^);
  Inc(FSrc);
  if FSrc > FSrcEnd then
    Exit;
  Inc(FLength, vVal1);

  if vVal1 <> $FF then
    Exit;
  repeat
    vVal1 := Byte(FSrc^);
    Inc(FSrc);
    if FSrc > FSrcEnd then
      Exit;
    vVal2 := Byte(FSrc^);
    vVal1 := (vVal2 shl 8) or vVal1;
    Inc(FSrc);
    Inc(FLength, vVal1);
    if FSrc > FSrcEnd then
      Break;
  until vVal1 <> $FFFF;
end;

{public}

constructor TsgDWG2007Decompressor.Create;
begin
  Clear;
end;

procedure TsgDWG2007Decompressor.Clear;
begin
  FOpCode := 0;
  FOffset := 0;
  FLength := 0;
end;

procedure TsgDWG2007Decompressor.Decompress(ASource: PAnsiChar; ASrcSize: Cardinal;
  ADest: PAnsiChar; ADstSize: Cardinal);
var
  vDst: PAnsiChar;
begin
  FSrc := ASource;
  vDst := ADest;
  FDstEnd := vDst;
  Inc(FDstEnd, ADstSize);
  FSrcEnd := FSrc;
  Inc(FSrcEnd, ASrcSize);
  FOffset := 0;
  FLength := 0;
  FOpCode := Byte(FSrc^);
  Inc(FSrc, 1);
  if FSrc > FSrcEnd then Exit;
  if (FOpCode and $F0) = $20 then
  begin
    Inc(FSrc, 3);
    FLength := PByte(TsgNativeUInt(FSrc) - 1)^;
    FLength := FLength and 7;
  end;

  while True do
  begin
    if FSrc >= FSrcEnd then Exit;
    if FLength = 0 then
    begin
      FLength := FOpCode;
      GetLiteralOpcode;
    end;
    if vDst + FLength <= FDstEnd then
      GetLiteralData(vDst, FSrc, FLength);
    Inc(vDst, FLength);
    if vDst > FDstEnd then Exit;
    Inc(FSrc, FLength);
    FLength := 0;
    if FSrc >= FSrcEnd then Exit;

    FOpCode := Byte(FSrc^);
    Inc(FSrc);
    GetCmpOpcode;

    while True do
    begin
      GetCmpData(vDst, FLength, FOffset);
      Inc(vDst, FLength);
      if vDst > FDstEnd then Exit;
      FLength := FOpCode and 7;
      if FLength <> 0 then Break;
      if FSrc >= FSrcEnd then Break;
      FOpCode := Byte(FSrc^);
      Inc(FSrc);
      if FSrc > FSrcEnd then Break;
      if (FOpCode shr 4) = 0 then Break;
      if (FOpCode shr 4) = $0F then
        FOpCode := FOpCode and $0F;
      GetCmpOpcode;
    end;
  end;
end;

{ TsgRSCoder }

{private}

procedure TsgRSCoder.Generate(const APoly: array of Byte; AM, AErrors: Cardinal);
const
  cntMaxErrors = 8;
var
  I, vPBSize, vMask: Cardinal;
  PBCur, PBEnd: PAnsiChar;
begin
  FBlockSize := (1 shl AM) - 1;
  if FBlockSize <> $FF then
    raise EsgRSCoderError.Create(sRSBlockSizeError);
  if AErrors > cntMaxErrors then
    raise EsgRSCoderError.CreateFmt(sRSMaxErrors, [cntMaxErrors]);
  FErr := AErrors;
  FPSize := FErr shl 1;
  FDSize := Integer(FBlockSize) - FPSize;
  vPBSize := (FPSize + 1) shl 8;
  GetMem(FMod255, vPBSize);
  FillChar(FMod255^, vPBSize, #0);
  PBCur := FMod255;
  PBEnd := PBCur;
  Inc(PBEnd, vPBSize);
  I := 0;
  while PBCur < PBEnd do
  begin
    PBCur^ := AnsiChar(I);
    Inc(I);
    if I = FBlockSize then
      I := 0;
    Inc(PBCur);
  end;
  vMask := 1;
  FAlfaTo[AM] := 0;
  for I := 0 to AM - 1 do
  begin
    FAlfaTo[I] := vMask;
    FIndexOf[FAlfaTo[I]] := I;
    if APoly[I] <> 0 then
      FAlfaTo[AM] := FAlfaTo[AM] xor vMask;
    vMask := vMask shl 1 ;
  end;
  FIndexOf[FAlfaTo[AM]] := AM;
  vMask := vMask shr 1;
  for I := AM + 1 to $FF - 1 do
  begin
    if FAlfaTo[I - 1] >= vMask then
      FAlfaTo[I] := FAlfaTo[AM] xor ((FAlfaTo[I - 1] xor vMask) shl 1)
    else
      FAlfaTo[I] := FAlfaTo[I - 1] shl 1;
    FIndexOf[FAlfaTo[I]] := I;
  end;
  FIndexOf[0] := $FFFFFFFF;
  if vPBSize = $500 then
    InitRSCoder500;
  if vPBSize = $1100 then
    InitRSCoder1100;
end;

function TsgRSCoder.GetDSize: Integer;
begin
  Result := FDSize;
end;

procedure TsgRSCoder.InitRSCoder1100;
begin
  FGPoly[0] := 136; FGPoly[1] := 105; FGPoly[2] := 57; FGPoly[3] := 25;
  FGPoly[4] := 160; FGPoly[5] := 222; FGPoly[6] := 95; FGPoly[7] := 66;
  FGPoly[8] := 119; FGPoly[9] := 49; FGPoly[10] := 61; FGPoly[11] := 171;
  FGPoly[12] := 92; FGPoly[13] := 195; FGPoly[14] := 210; FGPoly[15] := 241;
  FGPoly[16] := 0;
end;

procedure TsgRSCoder.InitRSCoder500;
//var
//  _FGPoly: array[0..16] of Cardinal;
//   I, J, vBlockSize: Integer;
begin
  FGPoly[0] := 10; FGPoly[1] := 81; FGPoly[2] := 251; FGPoly[3] := 76;
  FGPoly[4] := 0;
//  FillChar(_FGPoly, SizeOf(_FGPoly), 0);
//  _FGPoly[0] := 2;
//  _FGPoly[1] := 1;
//  vBlockSize := Integer(FBlockSize);
//
//  for I := 2 to vBlockSize - (vBlockSize - 2 * FErr) do
//  begin
//   _FGPoly[I] := 1;
//
//   for J := I - 1 downto 1 do
//   begin
//     if _FGPoly[J] <> 0 then
//       _FGPoly[J] := _FGPoly[J - 1] xor FAlfaTo[(FIndexOf[_FGPoly[J]] + Cardinal(I)) mod FBlockSize]
//     else
//       _FGPoly[J] := _FGPoly[J - 1];
//   end;
//  end;
//  for I := 0 to 2 * FErr do
//   _FGPoly[I] := FIndexOf[_FGPoly[I]] ;
end;

{public}

constructor TsgRSCoder.Create;
begin
  inherited Create;
  FillChar(FGPoly, SizeOf(FGPoly), 0);
  FillChar(FIndexOf, SizeOf(FIndexOf), 0);
  FillChar(FAlfaTo, SizeOf(FAlfaTo), 0);
  FMod255 := nil;
end;

destructor TsgRSCoder.Destroy;
begin
  if FMod255 <> nil then
    FreeMem(FMod255);
  inherited Destroy;
end;

//RS(255, 239) n = 255, k = 239
function TsgRSCoder.Decode(AData: PAnsiChar): Integer;
const
  cntN = 255;
  cntK = 239;
  cntNminK = cntN - cntK; //n - k = 255 - 239 = 16
  cntMaxCardinal = $FFFFFFFF;

var
  vData: Cardinal;
  I, J, U, Q, vCount: Integer;
  vIndex: array[Byte] of Cardinal;
  vSyn: array[0..cntNminK] of Cardinal;
  vErr: Boolean;
  vElp: array[0..cntNminK + 2, 0..cntNminK] of Integer;
  D: array[0..cntNminK + 2] of Integer;
  L: array[0..cntNminK + 2] of Integer;
  vUL: array[0..cntNminK + 2] of Integer;
  vErrs, vRoot, vLoc, Z, vReg: array of Integer;

begin
  vCount := 0;
  FillChar(vSyn, SizeOf(vSyn), $FF);
  FillChar(vIndex, SizeOf(vIndex), $FF);

  for I := 0 to $FF - 1 do
    vIndex[I] := FIndexOf[Byte(AData[I])];
  vErr := False;
  I := 1;
  while I <= FPSize do
  begin
    vSyn[I] := 0;
    for J := 0 to $FF - 1 do
    begin
      vData := vIndex[J];
      if vData <> cntMaxCardinal then
      begin
        vData := Byte(FMod255[vData + Cardinal(I * J)]);
        vData := FAlfaTo[vData];
        vSyn[I] := vData xor vSyn[I];
      end
      else
        Continue;
    end;
    if vSyn[I] <> 0 then
      vErr := True;
    vSyn[I] := FIndexOf[vSyn[I]];
    Inc(I);
  end;
  Result := I;
  if vErr then
  begin
    Result := 0;
    SetLength(vErrs, cntN);
    SetLength(vRoot, FErr);
    SetLength(vLoc, FErr);
    SetLength(Z, FErr + 1);
    SetLength(vReg, FErr + 1);

    D[0] := 0;
    D[1] := vSyn[1];
    vElp[0][0] := 0;
    vElp[1][0] := 1;

    I := 1;
    while I < cntNminK do
    begin
      vElp[0][I] := -1;
      vElp[1][I] := 0;
      Inc(I);
    end;

    L[0] := 0;
    L[1] := 0;
    vUL[0] := -1;
    vUL[1] := 0;
    U := 0;

    while (U < FPSize) and (L[U + 1] <= FErr) do
    begin
      Inc(U);
      if D[U] = -1 then
      begin
        L[U + 1] := L[U];
        for I := 0 to L[U] do
        begin
          vElp[U + 1][I] := vElp[U][I];
          vElp[U][I]   := FIndexOf[vElp[U][I]];
        end;
      end
      else
      begin
        Q := U - 1;
        while (D[Q] = -1) and (Q > 0) do
          Dec(Q);
        if Q > 0 then
        begin
          J := Q;
          while J > 0 do
          begin
            Dec(J);
            if (D[J] <> -1) and (vUL[Q] < vUL[J]) then
              Q := J;
          end;
        end;
        if L[U] > L[Q] + U - Q then
          L[U + 1] := L[U]
        else
          L[U + 1] := L[Q] + U - Q;
        for I := 0 to FPSize - 1 do
          vElp[U + 1][I] := 0;

        for I := 0 to L[Q] do
          if (vElp[Q][I] <> -1) then
            vElp[U + 1][I+U-Q] := FAlfaTo[(D[U] + cntN - D[Q] + vElp[Q][I]) mod cntN];

        for I := 0 to L[U] do
        begin
          vElp[U + 1][I] := vElp[U + 1][I] xor vElp[U][I];
          vElp[U][I] := FIndexOf[vElp[U][I]];
        end;
      end;

      vUL[U + 1] := U - L[U + 1];

      if U < FPSize then
      begin
        if vSyn[U + 1] <> cntMaxCardinal then
          D[U + 1] := FAlfaTo[vSyn[U + 1]]
        else
          D[U + 1] := 0;
        for I := 1 to L[U + 1] do
          if (vSyn[U + 1 - I] <> cntMaxCardinal) and (vElp[U + 1][I] <> 0) then
            D[U + 1] := D[U + 1] xor Integer(FAlfaTo[(vSyn[U + 1 - I] + FIndexOf[vElp[U + 1][I]]) mod cntN]);
        D[U + 1] := FIndexOf[D[U + 1]];
      end;
    end;

    Inc(U);
    if L[U] <= FErr then
    begin
      for I := 0 to L[U]do
        vElp[U][I] := FIndexOf[vElp[U][I]];

      for I := 1 to L[U] do
        vReg[I] := vElp[U][I];

      for I := 1 to cntN do
      begin
        Q := 1;
        for J := 1 to L[U] do
          if vReg[J] <> -1 then
          begin
            vReg[J] := (vReg[J] + J) mod cntN;
            Q := Q xor Integer(FAlfaTo[vReg[J]]);
          end;

        if Q = 0 then
        begin
          vRoot[vCount] := I;
          vLoc[vCount] := cntN - I;
          Inc(vCount);
        end;
      end;

      if vCount = L[U] then
      begin
        Result := vCount;
        for I := 1 to L[U] do
        begin
          if (vSyn[I]<> cntMaxCardinal) and (vElp[U][I] <> -1) then
            Z[I] := FAlfaTo[vSyn[I]] xor FAlfaTo[vElp[U][I]]
          else
            if (vSyn[I] <> cntMaxCardinal) and (vElp[U][I] = -1) then
              Z[I] := FAlfaTo[vSyn[I]]
            else
              if (vSyn[I] = cntMaxCardinal) and (vElp[U][I] <> -1) then
                Z[I] := FAlfaTo[vElp[U][I]]
              else
                Z[I] := 0;

          for J := 1 to I - 1 do
            if (vSyn[J] <> cntMaxCardinal) and (vElp[U][I - J] <> -1) then
              Z[I] := Z[I] xor Integer(FAlfaTo[(Cardinal(vElp[U][I - J]) + vSyn[J]) mod cntN]);
          Z[I] := FIndexOf[Z[I]];
        end;
        for I := 0 to cntN - 1 do
        begin
          vErrs[I] := 0;
          if vIndex[I] <> cntMaxCardinal then
             vIndex[I] := FAlfaTo[vIndex[I]]
          else
            vIndex[I] := 0;
        end;
        for I := 0 to L[U] - 1 do
        begin
          vErrs[vLoc[I]] := 1;
          for J := 1 to L[U] do
            if Z[J] <> -1 then
              vErrs[vLoc[I]] := vErrs[vLoc[I]] xor Integer(FAlfaTo[(Z[J] + J * vRoot[I]) mod cntN]);
          if vErrs[vLoc[I]] <> 0 then
          begin
            vErrs[vLoc[I]] := FIndexOf[vErrs[vLoc[I]]];
            Q := 0;
            for J := 0 to L[U] - 1 do
              if J <> I then
                Q := Q + Integer(FIndexOf[1 xor FAlfaTo[(vLoc[J] + vRoot[I]) mod cntN]]);
            Q := Q mod cntN;
            vErrs[vLoc[I]] := Integer(FAlfaTo[(vErrs[vLoc[I]] - Q + cntN) mod cntN]);
            vIndex[vLoc[I]] := vIndex[vLoc[I]] xor Cardinal(vErrs[vLoc[I]]);
          end;
        end;
      end
      else
      begin
        Result := -1;
        for I := 0 to cntN do
          if vIndex[I] <> cntMaxCardinal then
            vIndex[I] := FAlfaTo[vIndex[I]]
          else
            vIndex[I] := 0;
      end;
    end
    else
      for I := 0 to cntN do
      begin
        if vIndex[I] <> cntMaxCardinal then
          vIndex[I] := FAlfaTo[vIndex[I]]
        else
          vIndex[I] := 0;
      end;
    for I := 0 to $FF - 1 do
      if vIndex[I] <> cntMaxCardinal then
        AData[I] := AnsiChar(vIndex[I])
      else
        AData[I] := #0;
  end;
end;

{UnpackR24}

procedure DoHeaderError;
begin
  raise EdwgError.Create(sCorruptedDWGFileHeader);
end;

function LoadSysPage(AStream: TCustomMemoryStream; ASecType: Cardinal;
  ASecOffset: Integer; var AData: PByte): Cardinal;
var
  vSysPg: TsgDWG2010SysPageHeader;
  vCRC, vCRCHdr: Cardinal;
  vData: PByte;
  vDecomp: TsgDWG2010Compressor;
  vSize: Cardinal;
begin
  AStream.Position := ASecOffset;
  AStream.Read(vSysPg, SizeOf(TsgDWG2010SysPageHeader));
  if vSysPg.PageType <> ASecType then DoHeaderError;
  if vSysPg.CompType <> 2 then DoHeaderError;
  vCRC := vSysPg.CRC;
  vSysPg.CRC := 0;
  vCRCHdr := Checksum(PByte(@vSysPg), 0, SizeOf(TsgDWG2010SysPageHeader));
  vData := AStream.Memory;
  Inc(vData, AStream.Position);
  vSysPg.CRC := Checksum(vData, vCRCHdr, vSysPg.CompSize);
  if vCRC <> vSysPg.CRC then DoHeaderError;
  vSize := vSysPg.DecompSize;
  if vSize < vSysPg.CompSize then
    vSize := vSysPg.CompSize;
  GetMem(vData, vSize);
  try
    AStream.Read(vData^, vSysPg.CompSize);
    vDecomp := TsgDWG2010Compressor.Create;
    try
      GetMem(AData, vSysPg.DecompSize);
      Result := vDecomp.Decompress(vData, PByte(AData), vSysPg.CompSize, vSysPg.DecompSize);
      if Result <> vSysPg.DecompSize then DoHeaderError;
    finally
      vDecomp.Free;
    end;
  finally
    FreeMem(vData, vSize);
  end;
end;

{procedure LoadSection(AStream: TStream; var ASection: TsgDWGR18Section;
  const APages: TList; AData: TMemoryStream);
var
  vPageID, I: Integer;
  vTmpSysPage: TsgDWG2007Page;
  P: Pointer;
  vPageHeader: TsgDWG2004DataPageHeader;
  vDecom: TsgDWG2010Compressor;
  vVal: Int64;
  vDataRead: Integer;
  vData: Pointer;
begin
  vDecom := TsgDWG2010Compressor.Create;
  GetMem(vData, $8000);
  try
    for I := 0 to ASection.PagesAmount - 1 do
    begin
      vPageID := ASection.Pages[I].PageId;
      if (vPageID < 0) or (vPageID > APages.Count - 1) then Continue;
      vTmpSysPage := PsgDWG2007Page(APages[vPageID])^;
      if (vTmpSysPage.Offset = 0) or (vTmpSysPage.Size = 0) then Continue;
      vVal := AData.Size;
      AData.Size := AData.Size + ASection.PageSize;
      P := AData.Memory;
      Inc(PByte(P), vVal);
      AStream.Position := vTmpSysPage.Offset;
      AStream.Read(vPageHeader, SizeOf(TsgDWG2004DataPageHeader));
      sgDWG2004PageHeaderDecode(@vPageHeader, vTmpSysPage.Offset);
      if vPageHeader.SectionType <> cntDWG2004SectionData then DoHeaderError;
      vDataRead := AStream.Read(vData^, vPageHeader.DataSize);
      if ASection.Compressed = 2 then
        vDecom.Decompress(vData, P, vDataRead, ASection.PageSize)
      else
        AData.Write(vData^, vDataRead);
    end;
    AData.Size := ASection.DataSize;
  finally
    vDecom.Free;
    FreeMem(vData);
  end;
end;}

type
  TR24UpPageReader = class
  private
    FSource: TStream;
    FData: TMemoryStream;
    FBuffer: PByte;
    FPageSizes: array of Int64;
    FSourceOffsets: array of Int64;
    FPosition: Int64;
  protected
    function DoReadPage(ADataSize: Integer; ACheckSum2: Cardinal): Integer;
    procedure Initialize(const APages: TList; const ASection: TsgDWGR18Section); virtual;
    procedure ReadPage(const AIndex: Integer);
  public
    constructor Create(const ASource: TStream; const AData: TMemoryStream); virtual;
    destructor Destroy; override;
    function Read(ADataSize, APageSize: Integer; ACheckSum2: Cardinal): Integer; virtual;
    procedure ReadPages(const APages: TList; const ASection: TsgDWGR18Section);
  end;

  { TR24UpCompressedPageReader }

  TR24UpCompressedPageReader = class(TR24UpPageReader)
  private
    FDecompressor: TsgDWG2010Compressor;
  protected
    procedure Initialize(const APages: TList; const ASection: TsgDWGR18Section); override;
  public
    constructor Create(const ASource: TStream; const AData: TMemoryStream); override;
    destructor Destroy; override;
    function Read(ADataSize, APageSize: Integer; ACheckSum2: Cardinal): Integer; override;
  end;

{ TR24UpPageReader }

constructor TR24UpPageReader.Create(const ASource: TStream;
  const AData: TMemoryStream);
begin
  inherited Create;
  FSource := ASource;
  FData := AData;
  GetMem(FBuffer, $8000);
end;

destructor TR24UpPageReader.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TR24UpPageReader.DoReadPage(ADataSize: Integer; ACheckSum2: Cardinal): Integer;
var
  vCalcChecksum2: Cardinal;
begin
  Result := FSource.Read(FBuffer^, ADataSize);
  vCalcChecksum2 := Checksum(FBuffer, 0, ADataSize);
  if vCalcChecksum2 <> ACheckSum2 then DoHeaderError;
end;

function TR24UpPageReader.Read(ADataSize, APageSize: Integer; ACheckSum2: Cardinal): Integer;
begin
  Result := FData.Write(FBuffer^, DoReadPage(ADataSize, ACheckSum2));
end;

procedure TR24UpPageReader.ReadPage(const AIndex: Integer);
var
  vPageHeader: TsgDWG2004DataPageHeader;
  vCalcChecksum1, vChecksum1: Cardinal;
begin
  if FSourceOffsets[AIndex] <> 0 then
  begin
    FData.Position := FPosition;
{$IFNDEF SGFPC}
    FData.Size := FData.Size + FPageSizes[AIndex];
{$ENDIF}
    FSource.Position := FSourceOffsets[AIndex];
    FSource.Read(vPageHeader, SizeOf(TsgDWG2004DataPageHeader));
    sgDWG2004PageHeaderDecode(@vPageHeader, FSourceOffsets[AIndex]);
    if vPageHeader.SectionType <> cntDWG2004SectionData then DoHeaderError;
    vChecksum1 := vPageHeader.Checksum1;
    vPageHeader.Checksum1 := 0;
    vCalcChecksum1 := Checksum(PByte(@vPageHeader), vPageHeader.Checksum2, SizeOf(TsgDWG2004DataPageHeader));
    if vCalcChecksum1 <> vChecksum1 then DoHeaderError;
    Read(vPageHeader.DataSize, FPageSizes[AIndex], vPageHeader.Checksum2);
    Inc(FPosition, FPageSizes[AIndex]);
  end;
end;

procedure TR24UpPageReader.Initialize(const APages: TList; const ASection: TsgDWGR18Section);
var
  I: Integer;

  function GetSourceOffset(const ASectionPageIndex: Integer): Int64;
  var
    vPageID: Integer;
    vSysPage: PsgDWG2007Page;
  begin
    Result := 0;
    vPageID := ASection.Pages[ASectionPageIndex].PageId;
    if (vPageID >= 0) and (vPageID <= APages.Count - 1) then
    begin
      vSysPage := PsgDWG2007Page(APages[vPageID]);
      if (vSysPage^.Offset <> 0) and (vSysPage^.Size <> 0) then
        Result := vSysPage^.Offset;
    end;
  end;

begin
  SetLength(FPageSizes, ASection.PagesAmount);
  SetLength(FSourceOffsets, ASection.PagesAmount);
  if Length(FPageSizes) > 0 then
  begin
    I := 0;
    while I < High(FPageSizes) do
    begin
      FPageSizes[I] := ASection.Pages[I + 1].DataOffset - ASection.Pages[I].DataOffset;
      FSourceOffsets[I] := GetSourceOffset(I);
      Inc(I);
    end;
    FPageSizes[I] := ASection.PageSize;
    FSourceOffsets[I] := GetSourceOffset(I);
  end;
end;

procedure TR24UpPageReader.ReadPages(const APages: TList;
  const ASection: TsgDWGR18Section);
var
  I: Integer;
begin
  Initialize(APages, ASection);
  for I := Low(FPageSizes) to High(FPageSizes) do
    ReadPage(I);
  FData.Size := ASection.DataSize;
end;

{ TR24UpCompressedPageReader }

procedure TR24UpCompressedPageReader.Initialize(const APages: TList;
  const ASection: TsgDWGR18Section);
{$IFDEF SGFPC}
var
  I: Integer;
  vDestSize: Int64;
{$ENDIF}
begin
  inherited Initialize(APages, ASection);
  FPosition := 0;
{$IFDEF SGFPC}
  vDestSize := 0;
  for I := Low(FSourceOffsets) to High(FSourceOffsets) do
    if FSourceOffsets[I] <> 0 then
      Inc(vDestSize, FPageSizes[I]);
  FData.Size := vDestSize;
{$ENDIF}
end;

constructor TR24UpCompressedPageReader.Create(const ASource: TStream;
  const AData: TMemoryStream);
begin
  inherited Create(ASource, AData);
  FDecompressor := TsgDWG2010Compressor.Create;
end;

destructor TR24UpCompressedPageReader.Destroy;
begin
  FDecompressor.Free;
  inherited Destroy;
end;

function TR24UpCompressedPageReader.Read(ADataSize, APageSize: Integer; ACheckSum2: Cardinal): Integer;
begin
  Result := FDecompressor.Decompress(FBuffer,
    PByte(TsgNativeUInt(FData.Memory) + {$IFDEF SGDEL_7}UInt64{$ENDIF}(FData.Position)),
    DoReadPage(ADataSize, ACheckSum2), APageSize);
end;

procedure LoadSection(AStream: TStream; const ASection: TsgDWGR18Section;
  const APages: TList; const AData: TMemoryStream);
var
  vPageReader: TR24UpPageReader;
begin
  if ASection.Compressed = 2 then
    vPageReader := TR24UpCompressedPageReader.Create(AStream, AData)
  else
    vPageReader := TR24UpPageReader.Create(AStream, AData);
  try
    vPageReader.ReadPages(APages, ASection);
  finally
    vPageReader.Free;
  end;
end;

function UnpackR24Up(AVersion: TsgDWGVersion; AStream: TCustomMemoryStream;
  var ASecCount: Integer): TMemoryStream;
const
  cntEncHdrSz = SizeOf(TsgDWG2004EncryptedFileHeader);
var
  vFileOffset: Cardinal;
  vEncFileHeader, vEncFileHeader2: TsgDWG2004EncryptedFileHeader;
  vCRC32: Cardinal;
  vPages: TList;
  I, vPageId, vGetSecIndByName: Integer;
  vPage: TsgDWG2007Page;
  vSections: array of TMemoryStream;
  vSection: PsgDWGR18Section;
  PHead: Pdwg2004FileHeader;
  vAdditionalSections: array of string;
  vSectionsSummmary: Int64;
  vSysPageSize, vEncFileHeaderRead: Cardinal;
  vData: PByte;
  vSysPageItem: PsgDWGR18SysPageItem;
begin
  vPages := nil;
  vData := nil;
  SetLength(vSections, cntDWGSectCount + 1);
  FillChar(vSections[0], SizeOf(vSections), 0);
  if AVersion >= acR2013 then
  begin
    SetLength(vAdditionalSections, 1);
    vAdditionalSections[Low(vAdditionalSections)] := sAcadSectPrototype;
  end;
  try
    AStream.Position := SizeOf(TsgDWG2004NonCryptedFileHeader);//AStream.Seek(SizeOf(TsgDWG2004NonCryptedFileHeader), 0{soFromBeginning});
    AStream.Read(vEncFileHeader, cntEncHdrSz);
    sgDWG2004Decode(@vEncFileHeader, cntEncHdrSz);
    // check encrypted file header
    vCRC32 := 0;
    SwapInts(vEncFileHeader.CRC32, vCRC32);
    vEncFileHeader.CRC32 := CRC32(PByte(@vEncFileHeader), 0, cntEncHdrSz);
    if vEncFileHeader.CRC32 <> vCRC32 then DoHeaderError;
    if vEncFileHeader.ACADVer <> cntDWG2004Id then DoHeaderError;
    if vEncFileHeader.Field1 <> $00 then DoHeaderError;
    if vEncFileHeader.Field2 <> $6C then DoHeaderError;
    if vEncFileHeader.Field3 <> $04 then DoHeaderError;
    //if vEncFileHeader.Field4 <> $01 then DoHeaderError;
    if vEncFileHeader.Field5 <> $20 then DoHeaderError;
    if vEncFileHeader.Field6 <> $80 then DoHeaderError;
    if vEncFileHeader.Field7 <> $40 then DoHeaderError;
    AStream.Position := vEncFileHeader.SecondHeaderAddress;
    FillChar(vEncFileHeader2, SizeOf(vEncFileHeader2), 0);
    vEncFileHeaderRead := AStream.Read(vEncFileHeader2, cntEncHdrSz);
    if vEncFileHeaderRead < cntEncHdrSz - 1 then DoHeaderError;
    sgDWG2004Decode(@vEncFileHeader2, cntEncHdrSz);
    if not CompareMem(@vEncFileHeader, @vEncFileHeader2, vEncFileHeaderRead) then DoHeaderError;

    CreateDWG2007PgsLst(vPages, vEncFileHeader.PagesMaxId);
    vSysPageSize := LoadSysPage(AStream, cntDWG2004SectionPageMap,
      vEncFileHeader.SectionPageMapAddress + $100, vData);
    try
      vSysPageItem := PsgDWGR18SysPageItem(vData);
      vFileOffset := $100;
      vPageId := -1;
      while TsgNativeUInt(vSysPageItem) < TsgNativeUInt(vData) + vSysPageSize do
      begin
        vPageId := Integer(vSysPageItem^.Number);
        if vPageId >= 0 then
        begin
          if vPageId > Integer(vEncFileHeader.PagesMaxId) then
            DoHeaderError;
          PsgDWG2007Page(vPages[vPageId])^.Id := vPageId;
          PsgDWG2007Page(vPages[vPageId])^.Size := vSysPageItem^.Size;
          PsgDWG2007Page(vPages[vPageId])^.Offset := vFileOffset;
          Inc(vFileOffset, vSysPageItem^.Size);
        end
        else
        begin
          Inc(vFileOffset, vSysPageItem^.Size);
          Inc(PByte(vSysPageItem), SizeOf(Cardinal) * 4);
        end;
        Inc(vSysPageItem);
      end;
    finally
      FreeMemAndNil(Pointer(vData));
    end;
    if (Integer(vEncFileHeader.LastSectionPageId) <> vPageId) or
       (vEncFileHeader.LastSectionPageEndAddress <> (vFileOffset - $100)) then
      DoHeaderError;
    if vEncFileHeader.SectionMapId > Cardinal(vPages.Count) then
      DoHeaderError;
    vPage := PsgDWG2007Page(vPages[vEncFileHeader.SectionMapId])^;
    vSysPageSize := LoadSysPage(AStream, cntDWG2004SectionMap, vPage.Offset, vData);
    try
      if PsgDWGR18SectionMapInfo(vData)^.Fileld1 <> 2 then DoHeaderError;
      if PsgDWGR18SectionMapInfo(vData)^.Fileld2 <> $7400 then DoHeaderError;
      if PsgDWGR18SectionMapInfo(vData)^.Fileld3 <> 0 then DoHeaderError;
      vSectionsSummmary := 0;
      vSection := PsgDWGR18Section(vData);
      Inc(PByte(vSection), 20);
      while TsgNativeUInt(vSection) < TsgNativeUInt(vData) + vSysPageSize do
      begin
        if (vSection^.PagesAmount > 0) and
           (vSection^.PagesAmount <= vEncFileHeader.PagesMaxId + 1) then
          if IsSectonAllowed(string(vSection^.SecName), vAdditionalSections) then
          begin
            vGetSecIndByName := GetSecIndByName(vSection^.SecName);
            if vGetSecIndByName < 0 then
            begin
              SetLength(vSections, Length(vSections) + 1);
              vGetSecIndByName := High(vSections);
              vSections[vGetSecIndByName] := nil;
            end;
            try
              vSections[vGetSecIndByName] := TMemoryStream.Create;
              LoadSection(AStream, vSection^, vPages, vSections[vGetSecIndByName]);
              Inc(vSectionsSummmary, vSections[vGetSecIndByName].Size);
              if vSections[vGetSecIndByName].Size = 0 then
                FreeAndNil(vSections[vGetSecIndByName]);
            except
              on E: Exception do
              begin
                if (E is EdwgError) or (E is TsgDWGCompressorError) then
                begin
                  Inc(vSectionsSummmary, vSection^.DataSize);
                  FreeAndNil(vSections[vGetSecIndByName]);
                end;
              end;
            end;
          end;
        Inc(PByte(vSection),
          (Integer(vSection^.PagesAmount) - 1) * SizeOf(TsgDWG2010StreamPage));
        Inc(vSection);
      end;
    finally
      FreeMemAndNil(Pointer(vData));
    end;
    ASecCount := High(vSections) + 1;
    Result := CreateDWGStream(['A', 'C', '1', '0', '2', '4'], ASecCount, vSectionsSummmary);
    for I := Low(vSections) to High(vSections) do
      if vSections[I] <> nil then
      begin
        PHead := Result.Memory;
        DoSection(PHead, I, vSections[I], Result);
        FreeAndNil(vSections[I]);
      end;
  finally
    for I := Low(vSections) to High(vSections) do
      vSections[I].Free;
    ClearDWG2007PgsLst(vPages);
  end;
end;
{$ENDIF}

function ReadMC(var P: Pointer): UInt64; register;
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromMemory(P, MaxInt);
  try
    Result := vDWGBits.ReadMCHandle;
    Inc(PByte(P), vDWGBits.CurByte);
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
{$IFDEF SG_CPUX64}
asm
      PUSH    R8
      PUSH    R9
      MOV     R9,RCX
      MOV     R8,[R9]
      XOR     RAX,RAX
      XOR     RCX,RCX
@@1:  XOR     RDX,RDX
      MOV     DL,[R8]
      INC     R8
      OR      DL,DL
      JNS     @@2
      CMP     CL,63
      JA      @@3
      AND     DL,$7F
      SHL     RDX,CL
      OR      RAX,RDX
@@3:  ADD     CL,7
      JMP     @@1
@@2:  MOV     [R9],R8
      CMP     CL,63
      JA      @@4
      AND     DL,$7F
      SHL     RDX,CL
      OR      RAX,RDX
@@4:  POP     R9
      POP     R8
end;
{$ELSE}
var
  Next: UInt64;
  Shift: Byte;
begin
  Result := 0;
  Shift := 0;
  repeat
    Next := PByte(P)^;
    Inc(PByte(P));
    if Shift <= 63 then // 63 = 64 bit - 1
      Result := Result or (Next and $7F) shl Shift;
    Inc(Shift, 7);
  until Next and $80 = 0;
end;
{$ENDIF}
{$ENDIF}

function ReadMCSigned(var P: Pointer): Integer; register;
{var
  Next: Integer;
  Shift: Byte;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    Next := PByte(P)^;
    Inc(PByte(P));
    if Next and $80 = 0 then Break;
    Result := Result or (Next and $7F) shl Shift;
    Inc(Shift,7);
  end;
  Result := Result or (Next and $3F) shl Shift;
  if Next and $40 <> 0 then
    Result := -Result;
end;
}
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromMemory(P, MaxInt);
  try
    Result := vDWGBits.ReadMC;
    Inc(PByte(P), vDWGBits.CurByte);
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
    PUSH    R8
    PUSH    R9
    MOV     R9,RCX
    MOV     R8,[R9]
    XOR     RAX,RAX
    XOR     RCX,RCX
@@1:
    XOR     RDX,RDX
    MOV     DL,[R8]
    INC     R8
    OR      DL,DL
    JNS     @@2
    AND     DL,$7F
    SHL     RDX,CL
    OR      RAX,RDX
    ADD     CL,7
    JMP     @@1
@@2:
    MOV     [R9],R8
    MOV     R8,RDX
    AND     DL,$3F
    SHL     RDX,CL
    OR      RAX,RDX
    TEST    R8,$40
    JZ      @@3
    NEG     RAX
@@3:
    POP     R9
    POP     R8
{$ELSE}
    PUSH    EBX
    PUSH    EBP
    MOV     EBP,EAX
    MOV     EBX,[EBP]
    XOR     EAX,EAX
    XOR     ECX,ECX
@@1:        XOR     EDX,EDX
    MOV     DL,[EBX]
    INC     EBX
    OR      DL,DL
    JNS     @@2
    AND     DL,$7F
    SHL     EDX,CL
    OR      EAX,EDX
    ADD     CL,7
    JMP     @@1
@@2:        MOV     [EBP],EBX
    MOV     BL,DL
    AND     DL,$3F
    SHL     EDX,CL
    OR      EAX,EDX
    TEST    BL,$40
    JZ      @@3
    NEG     EAX
@@3:        POP     EBP
    POP     EBX
{$ENDIF}
end;
{$ENDIF}

function ReadMS(var P: Pointer): Integer; register;
{var
  vFirstByte, vSecondByte: Byte;
  vResult: UInt64;
  I, J: Integer;
  vP: PByte;
begin
  vP := P;
  I := 0; J := 0;
  vResult := 0;
  repeat
    vFirstByte := vP^;
    Inc(vP);
    vSecondByte := vP^;
    Inc(vP);
    Inc(J,2);
    vResult := vResult +
      ( (((vSecondByte shl 8) + vFirstByte) and 32767) shl  (15 * I) );
    Inc(I);
  until vSecondByte <= 127;
  P := vP;
  Result := vResult;
end;}
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromMemory(P, MaxInt);
  try
    Result := vDWGBits.ReadMS;
    Inc(PByte(P), vDWGBits.CurByte);
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
{$IFDEF SG_CPUX64}
asm
        PUSH    R8
        PUSH    R9
        MOV     R8,RCX
        MOV     R9,[R8]
        XOR     RAX,RAX
        XOR     RCX,RCX
@@1:    XOR     RDX,RDX
        MOV     DX,[R9]
        INC     R9
        INC     R9
        OR      DX,DX
        JNS     @@2
        CMP     CL,31
        JA      @@3
        AND     DX,$7FFF
        SHL     RDX,CL
        OR      RAX,RDX
@@3:    ADD     CL,15
        JMP     @@1
@@2:    MOV     [R8],R9
        CMP     CL,31
        JA      @@4
        AND     DX,$7FFF
        SHL     RDX,CL
        OR      RAX,RDX
@@4:
        POP     R9
        POP     R8
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    EBP
        MOV     EBP,EAX
        MOV     EBX,[EBP]
        XOR     EAX,EAX
        XOR     ECX,ECX
@@1:    XOR     EDX,EDX
        MOV     DX,[EBX]
        INC     EBX
        INC     EBX
        OR      DX,DX
        JNS     @@2
        CMP     CL,31
        JA      @@3
        AND     DX,$7FFF
        SHL     EDX,CL
        OR      EAX,EDX
@@3:    ADD     CL,15
        JMP     @@1
@@2:    MOV     [EBP],EBX
        CMP     CL,31
        JA      @@4
        AND     DX,$7FFF
        SHL     EDX,CL
        OR      EAX,EDX
@@4:    POP     EBP
        POP     EBX
end;
{$ENDIF}
{$ENDIF}

procedure Inc2(var APBit: TsgInt64; Value: TsgNativeInt);
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromTsgInt64(APBit, Value);
  try
    APBit := vDWGBits.TosgInt64;
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
        ADD     [RCX],RDX
        ADC     DWORD PTR 8[rcx],0
{$ELSE}
        ADD     [EAX],EDX
        ADC     DWORD PTR 4[EAX],0
{$ENDIF}
end;
{$ENDIF}

procedure Dec2(var APBit: TsgInt64; Value: TsgNativeInt);
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromTsgInt64(APBit, -Value);
  try
    APBit := vDWGBits.TosgInt64;
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
        SUB     [RCX],RDX
        SBB     DWORD PTR 8[RCX],0
{$ELSE}
        SUB     [EAX],EDX
        SBB     DWORD PTR 4[EAX],0
{$ENDIF}
end;
{$ENDIF}

function MakeBit(const APBit: TsgInt64; AOffs: TsgNativeInt): TsgInt64;
begin
  Result := APBit;
  if AOffs > 0 then
    Inc2(Result, AOffs)
  else
    if AOffs < 0 then
      Dec2(Result, -AOffs);
end;

function Comp2(var A,B: TsgInt64): Boolean;
{$IFNDEF SG_ASSEMBLER}
begin
  Result := (B.Hi = A.Hi) and (B.Lo > A.Lo);
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
        MOV     EAX,[RCX+8]
        CMP     EAX,[RDX+8]
        JNZ     @@1
        MOV     RAX,[RCX]
        CMP     RAX,[RDX]
{$ELSE}
        MOV     ECX,[4 +EAX]
        CMP     ECX,[4 + EDX]
        JNZ     @@1
        MOV     ECX,[EAX]
        CMP     ECX,[EDX]
{$ENDIF}
@@1:    SETB    AL
        AND     EAX,1
end;
{$ENDIF}

function Comp2WithAccuracy(const A, B: TsgInt64; Accuracy: Integer): Boolean;
begin
  Result := (B.Hi = A.Hi) and (B.Lo > A.Lo) and ((B.Lo - A.Lo) > Accuracy);
end;

procedure ReadBytes(var APBit: TsgInt64; var Dest; Count: Integer); register;
{var
  Src: PByte;
  Next: WordRec;
  Shift: Byte;
  P: PByte;
begin
  PByte(Src) := PByte(APBit.Lo shr cnstAddressBit or APBit.Hi shl cnstAddressHi);
  Shift := APBit.Lo and 7;
  Inc2(APBit, Count shl 3);
  P := @Dest;
  while Count > 0 do
  begin
    Next.Hi := Src^;
    Inc(Src);
    Next.Lo := Src^;
    Word(Next) := Word(Next) shl Shift;
    P^ := Next.Hi;
    Inc(P);
    Dec(Count);
  end;
end;}
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromTsgInt64(APBit);
  try
    vDWGBits.ReadBytes(@Dest, Count);
    APBit := vDWGBits.TosgInt64;
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
{$IFDEF SG_CPUX64}
asm
    PUSH    R9
    PUSH    RDX
    MOV     RDX,RCX
    MOV     RCX,[RDX]
    MOVSXD   RAX,[RDX+8]
    MOV     R9,RCX
    SHRD    R9,RAX,3
    AND     CL,7
    MOV     RAX,R8
    SHL     RAX,3
    ADD     [RDX],RAX
    ADC	    DWORD PTR [RDX+8],0
    POP     RDX
@@1:
    MOV     AX,[R9]
    XCHG    AL,AH
    SHL     AX,CL
    MOV     AL,AH
    MOV     [RDX],AL
    INC     R9
    INC     RDX
    DEC     R8
    JNZ     @@1
    POP     R9
end;
{$ELSE}
asm
    PUSH    EBX
    PUSH    ESI
    PUSH    EDX
    MOV     EDX,EAX
    MOV     EBX,ECX
    MOV     ECX,[EDX]
    MOV	    EAX,[4 + EDX]
    MOV     ESI,ECX
    SHRD    ESI,EAX,3
    AND     CL,7
    MOV     EAX,EBX             // Count
    SHL     EAX,3
    ADD     [EDX],EAX
    ADC	    DWORD PTR 4[EDX],0
    POP     EDX
@@1:MOV     AX,[ESI]
    XCHG    AL,AH
    SHL     AX,CL
    MOV     [EDX],AH
    INC     ESI
    INC     EDX
    DEC     EBX
    JNZ     @@1
    POP     ESI
    POP     EBX
end;
{$ENDIF}
{$ENDIF}

procedure ReadBytesPair(var APBit: TsgInt64; var Dest; Count: Integer); register;
{$IFNDEF SG_ASSEMBLER}
begin
  ReadBytes(APBit, Dest, Count * 2);
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
    PUSH    R9
    PUSH    RDX
    MOV     RDX,RCX
    MOV     RCX,[RDX]
    MOVSXD   RAX,[RDX+8]
    MOV     R9,RCX
    SHRD    R9,RAX,3
    AND     CL,7
    MOV     RAX,R8
    SHL     RAX,4
    ADD     [RDX],RAX
    ADC	    DWORD PTR [RDX+8],0
    POP     RDX
@@1:
    MOV     EAX,[R9]
    BSWAP   EAX
    SHL     EAX,CL
    BSWAP   EAX
    MOV     [RDX],AX
    ADD     R9,2
    ADD     RDX,2
    DEC     R8
    JNZ     @@1
    POP     R9
{$ELSE}
    PUSH    EBX
    PUSH    ESI
    PUSH    EDX
    MOV     EDX,EAX
    MOV     EBX,ECX
    MOV     ECX,[EDX]
    MOV	    EAX,[4 + EDX]
    MOV     ESI,ECX
    SHRD    ESI,EAX,3
    AND     CL,7
    MOV     EAX,EBX
    SHL     EAX,4
    ADD     [EDX],EAX
    ADC	    DWORD PTR 4[EDX],0
    POP     EDX
@@1:MOV     EAX,[ESI]
    BSWAP   EAX
    SHL     EAX,CL
    BSWAP   EAX
    MOV     [EDX],AX
    ADD     ESI,2
    ADD     EDX,2
    DEC     EBX
    JNZ     @@1
    POP     ESI
    POP     EBX
{$ENDIF}
end;
{$ENDIF}

procedure ReadBytesDoublePair(var APBit: TsgInt64; var Dest; Count: Integer); register;
{$IFNDEF SG_ASSEMBLER}
begin
  ReadBytes(APBit, Dest, Count * 4);
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
    PUSH    R9
    PUSH    RDX
    MOV     RDX,RCX
    MOV     RCX,[RDX]
    MOVSXD  RAX,[RDX+8]
    MOV     R9,RCX
    SHRD    R9,RAX,3
    AND     CL,7
    MOV     RAX,R8
    SHL     RAX,5
    ADD     [RDX],RAX
    ADC	    DWORD PTR 8[RDX],0
    POP     RDX
@@1:
    MOV     RAX,[R9]
    BSWAP   RAX
    SHL     RAX,CL
    BSWAP   RAX
    MOV     [RDX],EAX
    ADD     R9,4
    ADD     RDX,4
    DEC     R8
    JNZ     @@1
    POP     R9
{$ELSE}
    PUSH    EBX
    PUSH    ESI
    PUSH    EDX
    MOV     EDX,EAX
    MOV     EBX,ECX
    SHL     EBX,1
    MOV     ECX,[EDX]
    MOV	    EAX,[4 + EDX]
    MOV     ESI,ECX
    SHRD    ESI,EAX,3
    AND     CL,7
    MOV     EAX,EBX
    SHL     EAX,4
    ADD     [EDX],EAX
    ADC	    DWORD PTR 4[EDX],0
    POP     EDX
@@1:MOV     EAX,[ESI]
    BSWAP   EAX
    SHL     EAX,CL
    BSWAP   EAX
    MOV     [EDX],AX
    ADD     ESI,2
    ADD     EDX,2
    DEC     EBX
    JNZ     @@1
    POP     ESI
    POP     EBX
{$ENDIF}
end;
{$ENDIF}

{$IFNDEF SGDEL_6}
procedure DivMod(AValue, ADivide: Integer; var ADiv, AMod: Word);
begin
  ADiv := AValue div ADivide;
  AMod := AValue mod ADivide;
end;
{$ENDIF}

procedure ReadBytesCustom(var APBit: TsgInt64; var Dest; Count: Integer); register;
var
  vDiv, vMod: Cardinal;
begin
  vDiv := Count shr 2;
  vMod := Count and $3;
  if vDiv > 0 then
  begin
    ReadBytesDoublePair(APBit, Dest, vDiv);
    if vMod > 0 then
      ReadBytesCustom(APBit, PByte(TsgNativeUInt(@Dest) + vDiv shl 2)^, vMod);
  end
  else
  begin
    vDiv := Count shr 1;
    vMod := Count and $1;
    if vDiv > 0 then
    begin
      ReadBytesPair(APBit, Dest, vDiv);
      if vMod = 1 then
        ReadBytes(APBit, PByte(TsgNativeUInt(@Dest) + vDiv shl 1)^, 1);
    end
    else
      ReadBytes(APBit, Dest, Count);
  end;
end;

function ReadBit(var APBit: TsgInt64): Integer; register;
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromTsgInt64(APBit);
  try
    Result := vDWGBits.ReadBit;
    APBit := vDWGBits.TosgInt64;
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
{$IFDEF SG_CPUX64}
{var
  Src: PByte;
  Mask: Byte;
begin
  NativeUInt(Src) := APBit.Lo shr cnstAddressBit or APBit.Hi shl cnstAddressHi;
  Mask := $80 shr (APBit.Lo and 7);
  Inc2(APBit,1);
  Result := 0;
  if Src^ and Mask <> 0
  then Result := 1;
end;}
asm
    MOV     RAX,RCX
    MOV     RDX,[RAX]
    MOV     CL,DL
    AND     CL,7
    MOV     CH,$80
    SHR     CH,CL
    MOV	    CL,[RAX+8]
    SHRD    RDX,RCX,3
    ADD     QWORD PTR [RAX],1
    ADC     DWORD PTR [RAX+8],0
    TEST    [RDX],CH
    SETNZ   AL
    AND	    RAX,1
end;
{$ELSE}
asm
    MOV     EDX,[EAX]
    MOV     CL,DL
    AND     CL,7
    MOV     CH,$80
    SHR     CH,CL
    MOV	    CL,[4 + EAX]
    SHRD    EDX,ECX,3
    ADD     DWORD PTR [EAX],1
    ADC     DWORD PTR 4[EAX],0
    TEST    [EDX],CH
    SETNZ   AL
    AND	    EAX,1
end;
{$ENDIF}
{$ENDIF}

function ReadBB(var APBit: TsgInt64): Integer;
//begin
//  Result := ReadBit(APBit);
//  Result := Result + Result;
//  Result := Result + ReadBit(APBit);
//end;
{$IFNDEF SG_ASSEMBLER}
var
  vDWGBits: TsgDWGBits;
begin
  vDWGBits := TsgDWGBits.CreateFromTsgInt64(APBit);
  try
    Result := vDWGBits.ReadBB;
    APBit := vDWGBits.TosgInt64;
  finally
    vDWGBits.Free;
  end;
end;
{$ELSE}
asm
{$IFDEF SG_CPUX64}
    MOV     RDX,RCX
    MOV     RCX,[RDX]
    MOVSXD   RAX,[RDX+8]
    ADD     QWORD PTR [RDX],2
    ADC	    DWORD PTR [RDX+8],0
    MOV     RDX,RCX
    SHRD    RDX,RAX,3
    AND     CL,7
    INC     CL
    MOV     AX,[RDX]
    XCHG    AL,AH
    SHL     AX,CL
    RCL     AL,1
    SHL     AH,1
    RCL     AL,1
    AND     RAX,3
{$ELSE}
    MOV     EDX, EAX
    MOV     ECX, [EDX]
    MOV	    EAX, [4 + EDX]
    ADD     DWORD PTR [EDX],2
    ADC	    DWORD PTR 4[EDX],0
    MOV     EDX,ECX
    SHRD    EDX,EAX,3
    AND     CL,7
    INC     CL
    MOV     AX,[EDX]
    XCHG    AL,AH
    SHL     AX,CL
    RCL     AL,1
    SHL     AH,1
    RCL     AL,1
    AND     EAX,3
{$ENDIF}
end;
{$ENDIF}

function Read3Bit(var APBit: TsgInt64): Integer;
begin
  Result := ReadBB(APBit);
{$IFDEF _FIXINSIGHT_}
  Result := Result shl 1;
{$ELSE}
  Result := Result + Result;
{$ENDIF}
  Result := Result + ReadBit(APBit);
end;


function ReadRC(var APBit: TsgInt64): Integer;
begin
  Result := 0;
  ReadBytes(APBit, Result, 1);
end;

function ReadRS(var APBit: TsgInt64): Integer;
var
  S: Smallint;
begin
  //ReadBytes(APBit, S, 2);
  ReadBytesPair(APBit, S, 1);
  Result := S;
end;

function ReadRL(var APBit: TsgInt64): Integer;
begin
  //ReadBytes(APBit, Result, 4);
  //ReadBytesPair(APBit, Result, 2);
  ReadBytesDoublePair(APBit, Result, 1);
end;

function ReadRD(var APBit: TsgInt64): Double;
begin
  //ReadBytes(APBit, Result, 8);
  //ReadBytesPair(APBit, Result, 4);
  ReadBytesDoublePair(APBit, Result, 2);
  if IsNan(Result) or (Abs(Result) > 1E+101) then
    Result := 0;
end;

function ReadRDCheck(var APBit: TsgInt64; const AExp10: Integer = 101): Double;
begin
  //ReadBytes(APBit, Result, 8);
  //ReadBytesPair(APBit, Result, 4);
  ReadBytesDoublePair(APBit, Result, 2);
  if not IsNan(Result) and not IsInfinite(Result) then
  begin
    if (Result < 0) and (Abs(Log10(-Result)) > AExp10) then
      Result := 0
    else
      if (Result > 0) and (Abs(Log10(Result)) > AExp10) then
        Result := 0;
  end
  else
    Result := 0;
end;

function ReadBS(var APBit: TsgInt64): Integer;
begin
  case ReadBB(APBit) of
    0: Result := ReadRS(APBit);
    1: Result := ReadRC(APBit);
    2: Result := 0;
    else Result := 256;
  end;
end;

function ReadBL(var APBit: TsgInt64): Integer;
begin
  case ReadBB(APBit) of
    0: Result := ReadRL(APBit);
    1: Result := ReadRC(APBit);
    else Result := 0;
  end;
end;

function ReadBUL(var APBit: TsgInt64): Int64;
var
  vBytesCount: Byte;
begin
  Result := 0;
  vBytesCount := Read3Bit(APBit);
  if vBytesCount > 0 then
    ReadBytesCustom(APBit, Result, vBytesCount);
end;

function ReadBD(var APBit: TsgInt64): Double;
begin
  case ReadBB(APBit) of
    0: Result := ReadRD(APBit);
    1: Result := 1.0;
    else Result := 0.0;
  end;
end;

function ReadBDCheck(var APBit: TsgInt64; const AExp10: Integer = 101): Double;
begin
  case ReadBB(APBit) of
    0: Result := ReadRDCheck(APBit, AExp10);
    1: Result := 1.0;
    else Result := 0.0;
  end;
end;

function Read2RD(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadRD(APBit);
  Result.Y := ReadRD(APBit);
  Result.Z := 0.0;
end;

function Read2RDElevation(var APBit: TsgInt64; const AElevation: Double): TFPoint;
begin
  Result.X := ReadRD(APBit);
  Result.Y := ReadRD(APBit);
  Result.Z := AElevation;
end;

function ReadF2DPoint(var APBit: TsgInt64): TF2DPoint;
begin
  Result.X := ReadRD(APBit);
  Result.Y := ReadRD(APBit);
end;

function _Read3RD(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadRD(APBit);
  Result.Y := ReadRD(APBit);
  Result.Z := ReadRD(APBit);
end;

function _Read3RDNoElevation(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadRD(APBit);
  Result.Y := ReadRD(APBit);
  SeekGeneral(APBit, [seRD]);
  Result.Z := 0;
end;

function Read2D(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadBD(APBit);
  Result.Y := ReadBD(APBit);
  Result.Z := 0.0;
end;

function _Read3D(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadBD(APBit);
  Result.Y := ReadBD(APBit);
  Result.Z := ReadBD(APBit);
end;

function _Read3DNoElevation(var APBit: TsgInt64): TFPoint;
begin
  Result.X := ReadBD(APBit);
  Result.Y := ReadBD(APBit);
  SeekGeneral(APBit, [seBD]);
  Result.Z := 0;
end;

function Read3Bits(var APBit: TsgInt64): Byte;
begin
  Result := (((ReadBit(APBit) shl 1) or ReadBit(APBit)) shl 1) or ReadBit(APBit);
end;

procedure AddBinaryChunk(var APBit: TsgInt64; const ACode: Integer;
  AExtData: TsgCADExtendedData);
var
  vBytesCount: Integer;
  vPBytes: PByte;
begin
  vBytesCount := ReadRC(APBit);
  if vBytesCount > 0 then
  begin
    GetMem(vPBytes, vBytesCount);
    try
      ReadBytesCustom(APBit, vPBytes^, vBytesCount);
      AExtData.AddBinary(ACode, vBytesCount, vPBytes);
    finally
      FreeMem(vPBytes);
    end;
  end
  else
    AExtData.AddBinary(ACode, 0, nil);
end;

{optimization}
// <--

procedure SeekGeneral(var APBit: TsgInt64; const AParam: array of TsgSeekDWG);
var
  I,J: Integer;
  vMasSeek: Integer;
begin
  vMasSeek :=0;
  for I := Low(AParam) to High(AParam) do
  begin
    if AParam[I] in [seBS,seBL,seBD,se2D,se3D,seRGB,seUInt64] then
      if vMasSeek <> 0 then
      begin
        Inc2(APBit,vMasSeek);
        vMasSeek :=0;
      end;
    case AParam[I] of
      seB: Inc(vMasSeek,1);
      seBB: Inc(vMasSeek,2);
      seBS: case ReadBB(APBit) of
              0: Inc(vMasSeek,16);// seek 2 bytes
              1: Inc(vMasSeek,8);// seek 1 bytes
            end;
      seBL: case ReadBB(APBit) of
              0: Inc(vMasSeek,32);// seek 4 bytes
              1: Inc(vMasSeek,8);// seek 1 bytes
            end;
      seBD: case ReadBB(APBit) of
              0: Inc(vMasSeek,64);// seek 8 bytes
            end;
      seRD:  Inc(vMasSeek,64);// seek 8 bytes
      se2RD: Inc(vMasSeek,128);// seek 16 bytes
      se3RD: Inc(vMasSeek,192);// seek 8 bytes
      se2D,se3D: for J := 0 to 1+ Byte(AParam[I] = se3D) do
                 case ReadBB(APBit) of
                   0: Inc2(APBit, 64); // seek 8 bytes
                 end;
      seRS: Inc(vMasSeek,16);// seek 2 bytes
      seRC: Inc(vMasSeek,8);// seek 1 bytes
      seRL: Inc(vMasSeek,32);// seek 4 bytes
      seRGB: begin
               case ReadBB(APBit) of
                 0: Inc2(APBit,16); // seek 2 bytes
                 1: Inc2(APBit,8); // seek 1 bytes
               end;
               case ReadBB(APBit) of
                 0: Inc(vMasSeek,32);// seek 4 bytes
                 1: Inc(vMasSeek,8);// seek 1 bytes
               end;
               Inc(vMasSeek,8);// seek 1 bytes
             end;
      seUInt64: Inc(vMasSeek,8*Read3Bits(APBit));// seek 1*Read3Bits(APBit) bytes
    end;
  end;
  if vMasSeek <> 0 then
    Inc2(APBit,vMasSeek);
end;
//<----

function ReadR21Text(var APBit: TsgInt64): WideString;
var
  Len: Integer;
begin
  Result := '';
  Len := ReadBS(APBit);
  if Len > 0 then
  begin
    SetLength(Result, Len);
    ReadBytesCustom(APBit, Result[1], Len shl 1);
  end;
end;

(*procedure DoTextRead(ALen: Integer; ACodepage: Word;
  var APBit: TsgInt64; var AStr: string);
var
  vAnsi: sgRawByteString;
begin
  AStr := '';
  if ALen > 0 then
  begin
    SetLength(vAnsi, ALen);
{$IFDEF SGDEL_2009}
    SetCodePage(vAnsi, $FFFF, False);
{$ENDIF}
    ReadBytesCustom(APBit, vAnsi[1], ALen); // ReadBytes(APBit, vAnsi[1], Len);
{$IFDEF SGDEL_2009}
    SetCodePage(vAnsi, ACodePage, False);
{$ENDIF}
    AStr := string(vAnsi);
    ALen := Length(AStr);
    if AStr[ALen] = #0 then
      SetLength(AStr, ALen - 1);
  end;
end;

function _ReadText(Obj: TdwgObject; var APBit: TsgInt64; AParseUnicode: Boolean = True;
  CodePage: Word = CP_ACP; AXRecordText: Boolean = False; SizeOfLen: Integer = 1): string;

  procedure RdText;
  var
    vLen: Integer;
    vCodePage: Word;
  begin
    Result := '';
    if Obj <> nil then
      vCodePage := Obj.Reader.FConverter.CodePage
    else
      vCodePage := CodePage;
    if AXRecordText then
    begin
      vLen := ReadRS(APBit);
      vCodePage := sgCodePageFromDWG(ReadRC(APBit));
    end
    else
      if SizeOfLen = 1 then
        vLen := ReadBS(APBit)
      else
        if SizeOfLen = 2 then
          vLen := ReadRS(APBit)
        else
          vLen := 0;
    DoTextRead(vLen, vCodePage, APBit, Result);
  end;

begin
  if (Obj = nil) or (Obj.Version < acR2007) then
    RdText
  else
    Result := ReadR21Text(Obj.FStrsStart);
{$IFDEF SGDEL_2009}
  if AParseUnicode then
    Result := ParseUnicode(Result, [puU, puM]);
{$ENDIF}
end;*)

function ReadJulianDateInt(var APBit: TsgInt64; AReadBL: Boolean = True): TDateTime;
var
  vJDDay, vJDMilSecInDay: Cardinal;
begin
  if AReadBL then
  begin
    vJDDay := ReadBL(APBit);
    vJDMilSecInDay := ReadBL(APBit);
  end
  else
  begin
    vJDDay := ReadRL(APBit);
    vJDMilSecInDay := ReadRL(APBit);
  end;
  Result := JulianDateIntToDateTime(vJDDay, vJDMilSecInDay)
end;

function ReadMText(Obj: TdwgObject): string;
var
  L: Integer;
begin
  Result := Obj.ReadText;
  // Replacement of tabulations accordingly DXF
  ReplaceAnsi(Result, #$9, '^I');
  ReplaceAnsi(Result, #$D#$A, '\P');
  ReplaceAnsi(Result, #$A, '^J');
  L := Length(Result);
  if (L > 0) and (Result[L] = #0) then
    SetLength(Result, L-1);
end;

function ReadR21TextWithCheck(var APBit, PEnd: TsgInt64; Accuracy: Integer): WideString;
var
  Len: Integer;
begin
  Result := '';
  if not Comp2WithAccuracy(APBit, PEnd, Accuracy) then Exit;
  Len := ReadBS(APBit);
  if not Comp2WithAccuracy(APBit, PEnd, Accuracy) then Exit;
  if Len <= 0 then
    Exit;
  SetLength(Result,Len);
  ReadBytesCustom(APBit,Result[1],Len shl 1); // ReadBytes(APBit,Result[1],Len shl 1);
end;

function DoReadDD(var APBit: TsgInt64; var AResult: Double): Boolean;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  case ReadBB(APBit) of
    1: ReadBytesDoublePair(APBit,AResult,1); //ReadBytes(APBit,Result,4);
    2: // Read6;
      begin
        ReadBytesPair(APBit,PByte(TsgNativeInt(@AResult) + 4)^,1); //ReadBytes(APBit,P^,2);
        ReadBytesDoublePair(APBit,AResult,1); //ReadBytes(APBit,Result,4);
      end;
    3: ReadBytesDoublePair(APBit,AResult,2); //ReadBytes(APBit,Result,8);
  end;
  Result := (not IsNan(AResult)) and (Abs(AResult) <= 1E+101);
end;

function ReadDD(var APBit: TsgInt64; ADefault: Double): Double; overload;
begin
  Result := ADefault;
  if not DoReadDD(APBit, Result) then
    Result := 0;
end;

function ReadDD(var APBit: TsgInt64; const ABaseValue, ADefault: Double): Double; overload;
begin
  Result := ABaseValue;
  if not DoReadDD(APBit, Result) then
    Result := ADefault;
end;

function ReadBT(var APBit: TsgInt64; Ver: TsgDWGVersion): Double;
begin
  if (Ver >= acR2000) and (ReadBit(APBit) <> 0) then
    Result := 0.0
  else
    Result := ReadBD(APBit);
end;

function ReadBE(var APBit: TsgInt64; AVer: TsgDWGVersion): TFPoint;
begin
  if (AVer >= acR2000) and (ReadBit(APBit) <> 0) then
    Result := MakeFPoint(0,0,1)
  else
    Result := Read3D(APBit);
end;

procedure DoReadExtrusion(var APBit: TsgInt64; var AExtrusion: TFPoint);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  if cnstLoadCad2D then
  begin
    AExtrusion := cnstExtrusion;
    SeekGeneral(APBit, [se3D]);
  end
  else
    AExtrusion := Read3D(APBit);
end;

function ReadBytesHandle(var APBit: TsgInt64; const ABytesCount: Byte): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABytesCount - 1 do
    Result := Result shl 8 + Cardinal(ReadRC(APBit));
end;

function Read8BytesHandle(var APBit: TsgInt64): UInt64;
begin
  Result := ReadBytesHandle(APBit, 8);
end;

procedure MakeHandle(ARef: UInt64; var ACode: Byte; var AResult: UInt64);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  ACode := ACode shr 4;
  case ACode of
    6:      AResult := ARef + 1;
    8:      AResult := ARef - 1;
    $A:     AResult := ARef + AResult;
    $C:     AResult := ARef - AResult;
  end;
end;

function ReadHandle(var APBit: TsgInt64; var ACode: Byte; ARef: UInt64): UInt64;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  ACode := ReadRC(APBit);
  Result := ReadBytesHandle(APBit, ACode and $F);
  MakeHandle(ARef, ACode, Result);
end;

{function ReadObjName(const Obj: TdwgObject; var APBit: TsgInt64;
  var Flags: Integer; Ver: TsgDWGVersion): string;
begin
  if Obj <> nil then
    Obj.ReadObjHeader(APBit);
  Result := _ReadText(Obj, APBit);
  if Ver > acR2004 then
    Flags := ReadBS(APBit)
  else
  begin
    Flags := ReadBit(APBit) shl 6;
    SeekGeneral(APBit, [seBS]);
    Flags := Flags or ReadBit(APBit) shl 4;
  end;
end;

procedure ReadLimits(var APBit: TsgInt64; Obj: TdwgObject; const AFlags: Integer = 0);
var
  I: Integer;
  HC: Byte;
begin
  if Obj.Version < acR2004 then
  begin
    if AFlags and $C = 0 then
    begin
      Obj.FSrcStart := Obj.ReadID(APBit);
      Obj.FSrcEnd := Obj.ReadID(APBit);
    end;
    Exit;
  end;
  if Obj.FSubEnts = 0 then
    Exit;
  Obj.FChildren := TsgInt64List.Create;
  for I := 0 to Obj.FSubEnts - 1 do
    Obj.ChildrenAdd(Obj.ReadID(APBit));
  Obj.FSrcStart := Obj.FChildren.First;
  Obj.FSrcEnd := Obj.FChildren.Last;
end;}

function ReadCmEntityColor(var APBit: TsgInt64): TsgColorCAD;
begin
  Result := CmEntityColorToColorCAD(ReadBL(APBit));
end;

(*procedure ReadProxyData(AProxyEntList, ASubEntPropsList: TList;
  var APBit: TsgInt64; AConv: TsgDXFConverter);
var
  I, J, K, Count, vDataSize, vSubEntityType, vSubEntCount, vSymbolCounter,
    vFact, vRowsNumber, vColumnsNumber, vEntriesNumber: Integer;
  vRadius, vSweepAngle: Double;
  vPTransform: PsgCADIterate;
  pt1, pt2, pt3: TFPoint;
  vPF2DPoint: TF2DPoint;
  vPSubProps: PsgSubProxyProps;
  vTmpPBit: TsgInt64;
  vSymbol: Byte;
  vFill: Boolean;
  Str: string;
  vAStr: AnsiString;
  vTransforms: TList;
  vEntries: TsgIntegerList;
  vPolygon: TsgCADPolyPolygon;
  vPolyline: TsgDXFPolyLine;
  vVertex: TsgDXFVertex;
  vText: TsgDXFText;
  vLine: TsgDXFLine;
{ vAng1, vAng2, vAng3, vAng4: Double;
  eLayerID, fLayerID, meshEdgesNumber, meshFacesNumber, vertNumber, rrr, epf, fpf, vpf: Integer;
  vPt: PFPoint;
  pp: TList;   }

  function GetTextString(var APBit: TsgInt64): string;
  var
    WCh: Integer;
  begin
    Result := '';
    while True do // Reading text
    begin
      WCh := ReadRS(APBit);
      if WCh = 0 then
        Break;
      if (WCh shr 8 = 0) and CharInSet(Char(WCh), ['!'..'~', ' ']) then
        Result := Result + Char(WCh)
      else
        Result := Result + '\U+' + IntToHex(WCh shr 8, 2) + IntToHex(WCh and $00FF, 2);
    end;
    SetLength(Result, Length(Result));// to exclude special symbols
  end;

  function GetTransformPoint(const APoint: TFPoint): TFPoint;
  begin
    if vTransforms = nil then
      Result := APoint
    else
      Result := FPointXMat(APoint, PsgCADIterate(vTransforms.Last)^.Matrix);
  end;

  function GetTransformSize(const ASize: Double): Double;
  begin
    if vTransforms = nil then
      Result := ASize
    else
      Result := ASize*PsgCADIterate(vTransforms.Last)^.YScale;
  end;

  procedure InitSubProps;// first initialization
  begin
    New(vPSubProps);
    FillChar(vPSubProps^, SizeOf(vPSubProps^), 0);
	  vPSubProps^.LayerIndex := -1;
    vPSubProps^.LTypeIndex := -1;
  	vPSubProps^.ColorID := -1;
  end;

  procedure NewSubProps;
  var
    vTempPSubProps: PsgSubProxyProps;
  begin
    New(vTempPSubProps);
    CopyMemory(vTempPSubProps, vPSubProps, SizeOf(vTempPSubProps^));
    vPSubProps := vTempPSubProps;
  end;

  procedure AddEntityToProxy(AEntity: TsgDXFEntity);
  begin
    AProxyEntList.Add(AEntity);
    ASubEntPropsList.Add(vPSubProps);
    NewSubProps;
  end;

  function GetAngle(p, a: TFPoint): Single;
  var
    hip, leg: Double;
  begin
    Result := 0;
    if ((a.X > p.X) and (a.Y > p.Y)) then // quadrant 1
    begin
      hip := Sqrt(Sqr(a.X - p.X) + Sqr(a.Y - p.Y));
      leg := a.X - p.X;
      Result := (ArcCos(leg / hip) * f180DividedByPi);
      Exit;
    end
    else if ((a.X < p.X) and (a.Y > p.Y)) then// quadrant 2
    begin
      hip := Sqrt(Sqr(p.X - a.X) + Sqr(a.Y - p.Y));
      leg := p.X - a.X;
      Result := (180 - ArcCos(leg / hip) * f180DividedByPi);
      Exit;
    end
    else if ((a.X < p.X) and (a.Y < p.Y)) then// quadrant 3
    begin
      hip := Sqrt(Sqr(p.X - a.X) + Sqr(p.Y - a.Y));
      leg := p.X - a.X;
      Result := (180 + ArcCos(leg / hip) * f180DividedByPi);
      Exit;
    end
    else if ((a.X > p.X) and (a.Y < p.Y)) then// quadrant 4
    begin
      hip := Sqrt(Sqr(a.X - p.X) + Sqr(p.Y - a.Y));
      leg := a.X - p.X;
      Result := (360 - ArcCos(leg / hip) * f180DividedByPi);
      Exit;
    end
    else if((a.Y = p.Y) and (a.X > p.X)) then
      Exit// Result := 0;
    else if ((a.Y > p.Y) and (a.X = p.X)) then
    begin
      Result := 90;
      Exit;
    end
    else if((a.Y = p.Y) and (a.X < p.X)) then
    begin
      Result := 180;
      Exit;
    end
    else// if((a.Y < p.Y) and (a.X = p.X)) then
    begin
      Result := 270;
      Exit;
    end;
  end;

  procedure DoPolyline(const AClosed: Boolean);
  var
    N: Integer;
  begin
    Count := ReadRL(APBit);
    if Count = 2 then
    begin
      vLine := TsgDXFLine.Create;
      vLine.Point := GetTransformPoint(Read3RD(APBit));
      vLine.Point1 := GetTransformPoint(Read3RD(APBit));
      AddEntityToProxy(vLine);
    end
    else
    begin
      vPolyline := TsgDXFPolyLine.Create;
      vPolyline.Polyline3D := True; //3d Polyline
      for N := 0 to Count-1 do
      begin
        vVertex := TsgDXFVertex.Create;
        vVertex.Point := GetTransformPoint(Read3RD(APBit));
        vPolyline.AddEntity(vVertex);
      end;
      vPolyline.Closed := AClosed;
      AddEntityToProxy(vPolyline);
    end;
  end;

  procedure SetSHXFont(const AText: TsgDXFText; const ASHXFont: string);// to be changed in the future versions
  begin
    TsgDXFTextAccess(AText).SetFontName(ASHXFont);
  end;

  function GetString(const AStr: AnsiString): string;
  begin
    Result := ConvertToWideString(AStr, AConv.CodePage);
  end;

begin
  SeekGeneral(APBit,[seRL]);
  vSubEntCount := ReadRL(APBit);
  InitSubProps;
  vFill := False;
  vTransforms := nil;
  for I := 0 to vSubEntCount-1 do
  begin
    vDataSize := ReadRL(APBit);
    vSubEntityType := ReadRL(APBit);
    vTmpPBit := APBit;
    Inc2(vTmpPBit, (vDataSize - 8) shl 3);
    case vSubEntityType of
{      1:
        begin
          pt1 := Read3RD(APBit);
          pt2 := Read3RD(APBit);
        end;                       }
      2:// CIRCLE (normal circle)
        begin
          vPolyline := TsgDXFPolyline.Create;
          vPolyline.Polyline3D := True; //3d Polyline
          pt1 := GetTransformPoint(Read3RD(APBit));
          vRadius := ReadRD(APBit);
          pt2 := Read3RD(APBit);
          // Definition of the start vector direction
          if (pt2.X <= 1 + fAccuracy) and not IsEqual(Abs(pt2.Y), 1.0) then
            pt3 := Ort(Vector(pt2, cnstYOrtAxis))
          else
            pt3 := Ort(Vector(pt2, cnstXOrtAxis));
          if ArcFromTwoVectors(vPolyline, vRadius, 2*Pi, pt1, pt2, pt3, GetNumberOfCircleParts) then
            AddEntityToProxy(vPolyline)
          else
            vPolyline.Free;
        end;
      3:// CIRCLE3PT (3 point circle)
        begin
          vPolyline := TsgDXFPolyline.Create;
          vPolyline.Polyline3D := True; //3d Polyline
          pt1 := GetTransformPoint(Read3RD(APBit));
          pt2 := GetTransformPoint(Read3RD(APBit));
          pt3 := GetTransformPoint(Read3RD(APBit));
          if CircleFromThreePoints(vPolyline, pt1, pt2, pt3, GetNumberOfCircleParts, True) then
            AddEntityToProxy(vPolyline)
          else
            vPolyline.Free;
        end;
      4:// CIRCULARARC (normal and start vector arc)
        begin
          vPolyline := TsgDXFPolyline.Create;
          vPolyline.Polyline3D := True; //3d Polyline
          pt1 := GetTransformPoint(Read3RD(APBit));
          vRadius := ReadRD(APBit);
          pt2 := Read3RD(APBit);
          pt3 := Ort(Read3RD(APBit));// start vector direction
          vSweepAngle := ReadRD(APBit);
          SeekGeneral(APBit,[seRL]);
          if ArcFromTwoVectors(vPolyline, vRadius, vSweepAngle, pt1, pt2, pt3, GetNumberOfCircleParts) then
            AddEntityToProxy(vPolyline)
          else
            vPolyline.Free;
        end;
      5:// CIRCULARARC3PT (3 point arc)
        begin
          vPolyline := TsgDXFPolyline.Create;
          vPolyline.Polyline3D := True; //3d Polyline
          pt1 := GetTransformPoint(Read3RD(APBit));
          pt2 := GetTransformPoint(Read3RD(APBit));
          pt3 := GetTransformPoint(Read3RD(APBit));
          SeekGeneral(APBit,[seRL]);
          if CircleFromThreePoints(vPolyline, pt1, pt2, pt3, GetNumberOfCircleParts, False) then
            AddEntityToProxy(vPolyline)
          else
            vPolyline.Free;
        end;
      6:// POLYLINE
        DoPolyline(False);
      7:// POLYGON
        begin
          if vFill then
          begin
            vPolygon := TsgCADPolyPolygon.Create;
            vPolygon.Boundaries.Add(TF2DPointList.Create);
            Count := ReadRL(APBit);
            for J := 0 to Count-1 do
            begin
              pt1 := GetTransformPoint(Read3RD(APBit));
              vPF2DPoint.X := pt1.X;
              vPF2DPoint.Y := pt1.Y;
              TF2DPointList(vPolygon.Boundaries[0]).Add(vPF2DPoint);
            end;
            AddEntityToProxy(vPolygon);
          end
          else
            DoPolyline(True);
        end;
      8:// MESH
        begin
          vPolyline := TsgDXFPolyline.Create;
          vRowsNumber := ReadRL(APBit);
          vColumnsNumber := ReadRL(APBit);
          for J := 0 to vRowsNumber - 1 do
            for K := 0 to vColumnsNumber - 1 do
            begin
              vVertex := TsgDXFVertex.Create;
              vVertex.Point := GetTransformPoint(Read3RD(APBit));
              vPolyline.AddEntity(vVertex);
            end;
          vPolyline.MeshM := vRowsNumber;
          vPolyline.MeshN := vColumnsNumber;
          AddEntityToProxy(vPolyline);
          APBit := vTmpPBit;// if it is commented downwards
          { epf := ReadRL(APBit);
          if epf and $FFFF <> 0 then
          begin
            meshEdgesNumber := (vRowsNumber - 1) * vColumnsNumber +
              (vColumnsNumber - 1) * vRowsNumber;
            if epf and $00000001 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                ReadRL(APBit);
            if epf and $00000002 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                eLayerID := ReadRL(APBit);
            if epf and $00000004 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                ReadRL(APBit);
            if epf and $00000020 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                ReadRL(APBit);
            if epf and $00000040 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                K := ReadRL(APBit);// visibility
          end;
          fpf := ReadRL(APBit);
          if fpf and $FFFF <> 0 then
          begin
            meshFacesNumber := (vRowsNumber - 1) * (vColumnsNumber - 1);
            if fpf and $00000001 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                ReadRL(APBit);
            if fpf and $00000002 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                fLayerID := ReadRL(APBit);
            if fpf and $00000020 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                ReadRL(APBit);
            if fpf and $00000080 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                pt1 := Read3RD(APBit);
            if fpf and $00000040 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                ReadRL(APBit);
            end;
            vpf := ReadRL(APBit);
            if vpf and $FFFF <> 0 then
            begin
              vertNumber := vRowsNumber * vColumnsNumber;
              if vpf and $00000080 <> 0 then
                for J := 0 to vertNumber - 1 do
                  pt1 := Read3RD(APBit);
              if vpf and $00000400 <> 0 then
                ReadRL(APBit);
          end;                                              }
        end;
      9:// SHELL
        begin
          vPolyline := TsgDXFPolyline.Create;
          vPolyline.Flags := 64;
          Count := ReadRL(APBit);
          for J := 0 to Count - 1 do
          begin
            vVertex := TsgDXFVertex.Create;
            vVertex.Point := GetTransformPoint(Read3RD(APBit));
            vPolyline.AddEntity(vVertex);
          end;
          vPolyline.MeshM := Count;
          vPolyline.MeshN := 1;// only for correct TsgDXFPolyline loading
          vEntriesNumber := ReadRL(APBit);
          vEntries := TsgIntegerList.Create;
          try
            for J := 0 to vEntriesNumber - 1 do
              vEntries.Add(ReadRL(APBit));
            J := 0;
            while J < vEntriesNumber do
            begin
              vVertex := TsgDXFVertex.Create;
              vVertex.Flags := 128;
              K := vEntries[J];
              for vFact := 1 to K do
                case vFact of
                  1: vVertex.PolyFaceVertexIndex1 := vEntries[J+vFact]+1;
                  2: vVertex.PolyFaceVertexIndex2 := vEntries[J+vFact]+1;
                  3: vVertex.PolyFaceVertexIndex3 := vEntries[J+vFact]+1;
                  4: vVertex.PolyFaceVertexIndex4 := vEntries[J+vFact]+1;
                else
                  // ERROR number
                end;
              Inc(J, K+1);
              vPolyline.AddEntity(vVertex);
            end;
          finally
            vEntries.Free;
          end;
          AddEntityToProxy(vPolyline);
          APBit := vTmpPBit;// if it is commented downwards
          { meshEdgesNumber := vEntriesNumber - 1;
          meshFacesNumber := Integer(vEntries[0]);
          epf := ReadRL(APBit);
          if epf and $0000FFFF <> 0 then
          begin
            if epf and $00000001 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                rrr := ReadRL(APBit);
            if epf and $00000002 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                eLayerID := ReadRL(APBit);
            if epf and $00000004 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                rrr := ReadRL(APBit);
            if epf and $00000020 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                rrr := ReadRL(APBit);
            if epf and $00000040 <> 0 then
              for J := 0 to meshEdgesNumber - 1 do
                rrr := ReadRL(APBit);
          end;
          fpf := ReadRL(APBit);
          if fpf and $0000FFFF <> 0 then
          begin
            if fpf and $00000001 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                rrr := ReadRL(APBit);
            if fpf and $00000002 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                fLayerID := ReadRL(APBit);
            if fpf and $00000020 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                rrr := ReadRL(APBit);
            if fpf and $00000080 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                pt1 := Read3RD(APBit);
            if fpf and $00000040 <> 0 then
              for J := 0 to meshFacesNumber - 1 do
                rrr := ReadRL(APBit);
          end;
          vpf := ReadRL(APBit);
          if vpf and $0000FFFF <> 0 then
          begin
            if vpf and $00000080 <> 0 then
              for J := 0 to Count - 1 do
                pt1 := Read3RD(APBit);
            if vpf and $00000400 <> 0 then
              rrr := ReadRL(APBit);
          end;                               }
        end;
      {10:
        begin
          vText := TsgDXFText.Create;
          vText.Point := GetTransformPoint(Read3RD(APBit));
          vText.Extrusion := Read3RD(APBit);
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit));// Read3RD!
          vText.Height := ReadRD(APBit);
          vText.Scale := ReadRD(APBit);
          vText.ObliqueAngle := ReadRD(APBit);
          Str := '';
          vSymbolCounter := 0;
          while True do // Reading text
          begin
            vSymbol := ReadRC(APBit);
            if vSymbol = 0 then
              Break;
            Str := Str + Char(vSymbol);
          end;
          APBit := vTmpPBit;
          vText.Text := Str;
          AddEntityToProxy(vText);
        end;                                       }
      11:
        begin
          vText := TsgDXFText.Create;
          vText.Point := GetTransformPoint(Read3RD(APBit));
          vText.Extrusion := Read3RD(APBit);
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit));// Read3RD!
          vAStr := '';
          vSymbol := ReadRC(APBit);
          vSymbolCounter := 1;
          while vSymbol <> 0 do // Reading text
          begin
            vAStr := vAStr + AnsiChar(vSymbol);
            vSymbol := ReadRC(APBit);
            Inc(vSymbolCounter);
          end;
          if (vSymbolCounter mod 4) <> 0 then
            Inc2(APBit, (4 - (vSymbolCounter mod 4)) * 8);
          Count := ReadRL(APBit); // Text length
          if Count = -1 then
            Count := Length(vAStr);
          SetLength(vAStr, Count);
          Str := GetString(vAStr);
          vText.Text := Str;
          SeekGeneral(APBit,[seRL]);
          vText.Height := GetTransformSize(ReadRD(APBit));
          vText.Scale := ReadRD(APBit);
          vText.ObliqueAngle := ReadRD(APBit) * f180DividedByPi;
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit)) + vText.Rotation;// Read3RD!
          SeekGeneral(APBit,[seRL]);
          vAStr := '';
          vSymbol := ReadRC(APBit);
          {vSymbolCounter := 1;}
          while vSymbol <> 0 do // Reading text
          begin
            vAStr := vAStr + AnsiChar(vSymbol);
            vSymbol := ReadRC(APBit);
            {Inc(vSymbolCounter);}
          end;
          // For future versions (vSymbolCounter)
          {if ((vSymbolCounter mod 4) <> 0) then
            Inc2(APBit, (4 - (vSymbolCounter mod 4)) * 8);
          ReadRL(APBit);}
          Str := GetString(vAStr);
          SetSHXFont(vText, Str);// to be changed in the future versions
          APBit := vTmpPBit;
          AddEntityToProxy(vText);
        end;
{      12:
        begin
          pt1 := Read3RD(APBit);
          pt2 := Read3RD(APBit);
        end;
      13:
        begin
          pt1 := Read3RD(APBit);
          pt2 := Read3RD(APBit);
        end;                      }
      14:
        vPSubProps^.ColorID := ReadRL(APBit);
      16:
        vPSubProps^.LayerIndex := ReadRL(APBit);
      18:
        begin
          vPSubProps^.LTypeIndex := ReadRL(APBit);
          case vPSubProps^.LTypeIndex of
            $7FFF: vPSubProps^.LTypeIndex := -1;
            $7FFE: vPSubProps^.LTypeIndex := -2;
          end;
        end;
      19:
        vPSubProps^.Marker := ReadRL(APBit);
      20:// FILL (on/off)
        vFill := ReadRL(APBit) = 1;
      22:
        begin
          vPSubProps^.ColorID := ReadRC(APBit);
          SeekGeneral(APBit,[seRC,seRC]);
          Inc2(APBit, 8);
        end;
      23:
        vPSubProps^.LWeight := ReadRL(APBit);
      24:
        vPSubProps^.LTScale := ReadRD(APBit);
      26:
        begin
          vPSubProps^.ProxyType := ReadRL(APBit);
          vPSubProps^.PlotStyleIndex := ReadRL(APBit);
        end;
{      27:
        begin
          pt1 := Read3RD(APBit);
          pt2 := Read3RD(APBit);
          ReadRL(APBit);
          ReadRD(APBit);
          ReadRD(APBit);
          for J := 0 to 15 do
            ReadRD(APBit);
          for J := 0 to 15 do
            ReadRD(APBit);
          ReadRL(APBit);
          ReadRL(APBit);
          ReadRD(APBit);
          ReadRD(APBit);
          ReadRL(APBit);
        end;                       }
      29:// PUSH_MODELXFORM
        begin
          if vTransforms = nil then
            vTransforms := TList.Create;
          New(vPTransform);
          FillChar(vPTransform^, SizeOf(vPTransform^), 0);
          for J := 0 to 2 do
            for K := 0 to 3 do
              vPTransform^.Matrix.M[K, J] := ReadRD(APBit);
          if vTransforms.Count > 0 then
            vPTransform^.Matrix := FMatXMat(vPTransform^.Matrix, PsgCADIterate(vTransforms.Last)^.Matrix);
          DoScale2D(vPTransform^);
          vTransforms.Add(vPTransform);
          Inc2(APBit, 256);
        end;
{      30:
        for J := 0 to 15 do
          ReadRD(APBit);      }
      31:// POP_MODELXFORM
        if vTransforms <> nil then
        begin
          if vTransforms.Count > 0 then
          begin
            Dispose(vTransforms.Last);
            vTransforms.Delete(vTransforms.Count-1);
          end;
          if vTransforms.Count = 0 then
            FreeRecordList(vTransforms);
        end;
      32:// ACAD_TABLE line (not documented)
        begin
          DoPolyline(False);
          SeekGeneral(APBit, [se3RD]);
        end;
      {34:
        begin
          ReadRL(APBit);
          ReadRL(APBit);
        end;  }
      36:// AEC undocumented data
        begin
          vText := TsgDXFText.Create;
          vText.Point := GetTransformPoint(Read3RD(APBit));
          vText.Extrusion := Read3RD(APBit);
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit));// Read3RD!
          vText.Height := GetTransformSize(ReadRD(APBit));
          vText.Scale := ReadRD(APBit);
          vText.ObliqueAngle := ReadRD(APBit) * f180DividedByPi;
          vText.Text := GetTextString(APBit);
          // ... unknow data ...
          APBit := vTmpPBit;
          AddEntityToProxy(vText);
        end;
      38:// ACAD_TABLE text (not documented)
        begin
          vText := TsgDXFText.Create;
          vText.Point := GetTransformPoint(Read3RD(APBit));
          vText.Extrusion := Read3RD(APBit);
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit));// Read3RD!
          vText.Text := GetTextString(APBit);
          SeekGeneral(APBit,[seRL]);
          vFact := ReadRL(APBit);
          if vFact > 1 then
            SeekGeneral(APBit,[seRS]);
            //ReadRS(APBit);
          vText.Height := GetTransformSize(ReadRD(APBit));
          vText.Scale := ReadRD(APBit);
          vText.ObliqueAngle := ReadRD(APBit) * f180DividedByPi;
          vText.Rotation := GetAngle(cnstFPointZero, Read3RD(APBit)) + vText.Rotation;// Read3RD!
          SeekGeneral(APBit,[seRL,seRL,seRL,seRL,seRL,seRL]);
          Str := '';
          while True do // Reading font name
          begin
            vSymbol := ReadRS(APBit);
            if (vSymbol = 0) then
              Break;
            Str := Str + string(AnsiChar(vSymbol));
          end;
          SetSHXFont(vText, Str);// to be changed in the future versions
          APBit := vTmpPBit;
          AddEntityToProxy(vText);
        end;
      else
        Inc2(APBit, (vDataSize - 8) shl 3);
    end;
  end;
  FreeRecordList(vTransforms);
  Dispose(vPSubProps);
end; *)

function ObjStreamPos(const ABegin, ACurr: TsgInt64): TsgInt64;
begin
  Result.Hi := (ACurr.Lo - ABegin.Lo) div 8;
  Result.Lo := (ACurr.Lo - ABegin.Lo) mod 8;
end;

function ReadDWGType(var APBit: TsgInt64): Integer;
begin
  Result := 0;
  case ReadBB(APBit) of
    0:
      Result := ReadRC(APBit);
    1:
      Result := ReadRC(APBit) + $1F0;
    2:
      Result := ReadRS(APBit);
    3:
      Result := ReadRS(APBit);
  end;
end;

function ReadUInt64(var APBit: TsgInt64): UInt64;
var
  I, vCount: Byte;
begin
  Result := 0;
  vCount := Read3Bits(APBit);
  for I := 0 to vCount - 1 do
    Result := Result + Cardinal(ReadRC(APBit)) shl (I shl 3);
end;

function GetLineWeight(AIndex: Integer): Double;
const
  LW: array[-3..31] of Integer =
  (-3, -2, -1, 000, 005, 009, 013, 015, 018, 020, 025, 030, 035, 040, 050,
   053, 060, 070, 080, 090, 100, 106, 120, 140, 158, 200, 211,
   -1, -1, -1, -1, -1, -1, -2, -3);
begin
  if AIndex > 31 then
    Result := fLineWeightByLayer
  else
    Result := LW[AIndex];
  if Result > 0 then
    Result := 0.01*Result;
end;

{TdwgObject}

{protected}

function TdwgObject.GetBitSize: Integer;
begin
  Result := 0;
end;

function TdwgObject.GetChild(Handle: UInt64): TsgDXFEntity;
var
  I: Integer;
begin
  Result := nil;
  I := Reader.FObjMap.IndexOfHandle(Handle);
  if I >= 0 then
    Result := GetChildEx(Handle, I);
end;

function TdwgObject.GetChildEx(Handle: UInt64; MapIndex: Integer): TsgDXFEntity;
var
  vOwner: TsgDXFEntity;
  vObj: TdwgObject;
  vElem: PdwgMapElementList;
begin
  vElem := TsgObjectsMapAccess(Reader.FObjMap).GetPMapElement(MapIndex);
  Result := TsgDXFEntity(vElem^.Obj);
  if not Assigned(Result) then
  begin
    vObj := ReadChild(vElem^.Location, vElem^.Handle, MapIndex);
    if Assigned(vObj) then
    try
      vObj.References(Reader);
      vObj.PostReferences(Reader);
      vOwner := vObj.GetOwner(Reader);
      if not Assigned(vOwner) or (vObj.OwnerAdd(vOwner) < 0) then
        FreeAndNil(vObj.FEntity);
      Result := vObj.FEntity;
      Reader.FObjMap.Objects[MapIndex] := Result;
    finally
      FreeAndNil(vObj);
    end;
  end;
end;

function TdwgObject.GetChildren(Index: Integer): UInt64;
begin
  Result := TsgInt64List(FChildren)[Index];
end;

function TdwgObject.GetChildrenCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0
end;

function TdwgObject.GetNewObject(AHandle: UInt64;
  AEntClass: TsgDXFEntityClass): TsgDXFEntity;
begin
  if AEntClass = TsgDXFBlockRecord then
  begin
    if AHandle = Reader.Model.BlockRecord.Handle then
      Result := Reader.Model.BlockRecord
    else
      if AHandle = Reader.Paper.BlockRecord.Handle then
        Result := Reader.Paper.BlockRecord
      else
        Result := AEntClass.Create;
  end
  else
    Result := AEntClass.Create;
end;

function TdwgObject.GetObjectBeginning: TsgInt64;
begin
  Result.Lo := 0;
  Result.Hi := 0;
end;

function TdwgObject.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := FOwnerObject;
end;

function TdwgObject.IsEntity: Boolean;
begin
  Result := False;
end;

procedure TdwgObject.NewObject(ACode: Integer; AHandle: UInt64; var AObj: TdwgObject);
var
  vClass: TdwgClass;
begin
  AObj := nil;
  if (ACode=54) or (ACode=55) then
    Exit;
  if ACode >= 500 then
    ACode := Reader.VarCode(ACode);
  if ACode = 498 then
    ACode := cnstProxyEntityIndex;
  if ACode > cnstLimit then
    Exit;
  vClass := cnstDwgClassTosgDwgClass[ACode].Objs;
  if Assigned(vClass) then
  begin
    AObj := vClass.Create(Version);
    AObj.Reader := Reader;
    AObj.FEntity := GetNewObject(AHandle, cnstDwgClassTosgDwgClass[ACode].Ents);
    AObj.FHandle := AHandle;
    Reader.Conv.DoCreate(AObj.FEntity);
  end;
end;

function TdwgObject.OwnerAdd(AOwner: TsgDXFEntity): Integer;
begin
  Result := AOwner.AddEntity(FEntity);
end;

procedure TdwgObject.PostReferences(AReader: TdwgReader);
begin
  if Assigned(FACADExtData) then
  begin
    FEEDataList.Add(AReader.FACADAppID, FACADExtData);
    FACADExtData := nil;
  end;
  if Assigned(FEEDataList) and (FEEDataList.Count > 0) then
  begin
    AReader.AddEEDItems(FHandle, FEEDataList);
    FEEDataList := nil;
  end;
end;

function TdwgObject.ReadID(var APBit: TsgInt64): UInt64;
begin
  Result := ReadHandle(FPHnds^, FCode, FHandle);
end;

function TdwgObject.ReadID(var APBit: TsgInt64; var ACode: Byte): UInt64;
begin
  Result := ReadHandle(FPHnds^, ACode, FHandle);
end;

function TdwgObject.ReadChild(ASeek: Integer;
  AHandle: UInt64; AIndex: Integer): TdwgObject;
var
  P: Pointer;
  vSize, vBitSize, vCode: Integer;
  vIdsBitSize, vIdsPos: UInt64;
  vEndBit: Cardinal;
  vPBit, vPStart, vObjectBeginning: TsgInt64;
  vHCode: Byte;
  vStrsStart, vHndsStart: TsgInt64;
begin
  Reader.Read(ASeek, 256);
  vSize := ReadMS(Pointer(Reader.FMem));
  vBitSize := vSize shl 3;
  P := Pointer(Reader.FMem);
  InitInt64(P, vPBit);
  vObjectBeginning := vPBit;

  Result := nil;
  if Version >= acR2010 then
  begin
    vIdsBitSize := ReadMC(P);
    InitInt64(P, vPBit);
    vIdsPos := Cardinal(vBitSize) - vIdsBitSize;
    Reader.Init2010ObjectData(vPBit, vStrsStart, vHndsStart, vIdsPos, vEndBit);
    vCode := ReadDWGType(vPBit);
  end
  else
  begin
    vCode := ReadBS(vPBit);
    if Version = acR2007 then
      vCode := Reader.Code2007ByClassName(vObjectBeginning, vPBit, vCode);
  end;
  NewObject(vCode, AHandle, Result);
  Reader.DoProgress(psRunning);
  if Assigned(Result) then
  begin
    Result.FMapIndex := AIndex;
    case Version of
      acR2007..DWGVersionHigh:
        begin
          if Version < acR2010 then
          begin
            Result.FStrsStart := vPBit;
            vPStart := vObjectBeginning;
            Reader.Init2007ObjectData(vPStart, Result.FStrsStart, Result.FHndsStart,
              vEndBit);
            vPBit := vPStart;
          end
          else
          begin
            Result.FStrsStart := vStrsStart;
            Result.FHndsStart := vHndsStart;
          end;
          Result.FDataEnd := MakeBit(vObjectBeginning, vBitSize);
          Result.FPHnds := @Result.FHndsStart;
        end;
      else
        Result.FPHnds := @vPBit;
    end;
    Result.FStringReader := Result.CreateStringReader(vPBit, Version);
    Reader.FObjMap.Objects[AIndex] := Result.FEntity;
    Result.ObjectBeginning := vObjectBeginning;
    case Version of
      acR2000..acR2004:
        Result.BitSize := ReadRL(vPBit);
    end;
    ReadHandle(vPBit, vHCode, AHandle);
    Result.FEntity.Handle := AHandle;
    Result.ReadEEDItems(vPBit);
    if not Reader.DoReadData(Result, vPBit) then
    begin
      Result.FEntity.Free;
      FreeAndNil(Result);
      Reader.FObjMap.Objects[AIndex] := nil;
    end;
  end;
end;

function TdwgObject.ReadColor(var APBit: TsgInt64): TsgColorCAD;
begin
  Result := FColorReader.ReadColor(@APBit);
end;

procedure TdwgObject.ReadEED(var APBit, APEnd: TsgInt64;
  const AAppID: UInt64; const AEEData: TsgCADExtendedData);
var
  vCode: Integer;
  vStringReader: TdwgStringReader;
begin
  vStringReader := Reader.GetStringReader(APBit);
  AEEData.AddInt64(1001, AAppID);
  while Comp2(APBit, APEnd) do
  begin
    vCode := ReadRC(APBit) + 1000;
    case vCode of
      1000: AEEData.AddString(vCode, vStringReader.ReadXrecordText);
      1002: AEEData.AddByte(vCode, ReadRC(APBit)); //Inc2(APBit, 8);
      1003: AEEData.AddInt64(vCode, Read8BytesHandle(APBit));
      1004: AddBinaryChunk(APBit, vCode, AEEData); //Inc2(APBit, ReadRC(APBit) shl 3);
      1005: AEEData.AddInt64(vCode, Read8BytesHandle(APBit));
      1040 .. 1042: AEEData.AddDouble(vCode, ReadRD(APBit)); //Inc2(APBit, 64);
      1010 .. 1013: AEEData.AddPoint(vCode, Read3RD(APBit)); //Inc2(APBit, 192);
      1060..1069, 1070: AEEData.AddInt16(vCode, ReadRS(APBit)); //Inc2(APBit, 16);
      1071: AEEData.AddInt(vCode, ReadRL(APBit)); //Inc2(APBit, 32);
    end;
  end;
end;

procedure TdwgObject.ReadEEDItems(var APBit: TsgInt64);
var
  I: Integer;
  vHCode: Byte;
  vEEData: TsgCADExtendedData;
  vAppID: UInt64;
  vPEnd: TsgInt64;
begin
  // |Length|Application handle|Data items|
  I := ReadBS(APBit) shl 3;  // |Length|
  while I <> 0 do
  begin
    vAppID := ReadHandle(APBit, vHCode, FHandle); // |Application handle|
    vPEnd := APBit;
    Inc2(vPEnd, I);
    try
      vEEData := TsgCADExtendedData.Create(Version);
      ReadEED(APBit, vPEnd, vAppID, vEEData);   // |Data items|
      if ApplyEED(vEEData, vAppID) or (vEEData.DataCount <= 1) then
        FreeAndNil(vEEData)
      else
      begin
        if Reader.IsACADAppID(vAppID) then
          FACADExtData := vEEData
        else
          EEDataList.Add(vAppID, vEEData);
      end;
    finally
      APBit := vPEnd;
    end;
    I := ReadBS(APBit) shl 3;  // |Length|
  end;
  EEDataList.Sorted := True;
end;

procedure TdwgObject.ReadError;
begin
  FCorrectData := False;
end;

function TdwgObject.ReadExtrusion(var APBit: TsgInt64): TFPoint;
begin
  DoReadExtrusion(APBit, Result);
end;

procedure TdwgObject.ReadHandleRefs(var APBit: TsgInt64);
begin
  FOwner := ReadID(APBit);
  SkipHandles(APBit);
end;

function TdwgObject.ReadID: UInt64;
begin
  Result := ReadHandle(FPHnds^, FCode, FHandle);
end;

procedure TdwgObject.ReadLineWeight(AIndex: Integer);
begin
  FLineWeight := GetLineWeight(AIndex);
end;

procedure TdwgObject.ReadObjHeader(var APBit: TsgInt64);
begin
  if Version < acR2000 then
    SeekGeneral(APBit, [seRL]);
  FReactorsCount := ReadBL(APBit);
  if Version >= acR2004 then
  begin
    FXDicHandlePresent := ReadBit(APBit) <> 1;
    if Version >= acR2013 then
      Inc2(APBit, 1);
  end;
end;

function TdwgObject.ReadObjName(var APBit: TsgInt64; var Flags: Integer): string;
begin
  ReadObjHeader(APBit);
  Result := ReadText;
  if Version > acR2004 then
    Flags := ReadBS(APBit)
  else
  begin
    Flags := ReadBit(APBit) shl 6;
    SeekGeneral(APBit, [seBS]);
    Flags := Flags or ReadBit(APBit) shl 4;
  end;
end;

procedure TdwgObject.ReadOwner(AReader: TdwgReader);
begin
  FOwnerObject := AReader.ObjByHandle(FOwner, nil);
end;

procedure TdwgObject.ReadPredefined(ASeek: Integer; AObj: TdwgObject;
  AIndex: Integer);
var
  vSize, vBitSize, vCode: Integer;
  vBitSizeVal: Byte;
  vBitSizeShift: Cardinal;
  vIdsBitSize, vIdsPos: UInt64;
  vEndBit: Cardinal;
  vPBit, vPStart, vObjectBeginning: TsgInt64;
  vHCode: Byte;
  vStrsStart, vHndsStart, vIdsStrart: TsgInt64;
begin
  Reader.Read(ASeek, 256);
  vSize := ReadMS(Pointer(Reader.FMem));
  InitInt64(Reader.FMem, vPBit);
  vObjectBeginning := vPBit;
  if Version > acR2007 then
  begin
    vBitSizeShift := 0;
    vIdsBitSize := 0;
    while True do
    begin
      vBitSizeVal := Byte(ReadRC(vPBit));
      vIdsBitSize := UInt64(vBitSizeVal and $7F shl vBitSizeShift) or vIdsBitSize;
      Inc(vBitSizeShift, 7);
      if vBitSizeVal and $80 = 0 then
        Break;
    end;
    vBitSize := vSize shl 3;
    vIdsPos := Cardinal(vBitSize) - vIdsBitSize;
    vIdsStrart := vPBit;
    Inc2(vIdsStrart, vIdsPos);
    Reader.Init2010ObjectData(vPBit, vStrsStart, vHndsStart, vIdsPos, vEndBit);
    {vCode := }ReadDWGType(vPBit);
  end
  else
  begin
    vCode := ReadBS(vPBit);
    if Version = acR2007 then
      {vCode := }Reader.Code2007ByClassName(vObjectBeginning, vPBit, vCode);
  end;
  Reader.DoProgress(psRunning);
  AObj.FMapIndex := AIndex;
  case Version of
    acR2007..DWGVersionHigh:
      begin
        if Version < acR2010 then
        begin
          AObj.FStrsStart := vPBit;
          vPStart := vObjectBeginning;
          Reader.Init2007ObjectData(vPStart, AObj.FStrsStart, AObj.FHndsStart,
            vEndBit);
          vPBit := vPStart;
        end
        else
        begin
          AObj.FStrsStart := vStrsStart;
          AObj.FHndsStart := vHndsStart;
        end;
        AObj.FDataEnd := vObjectBeginning;
        Inc2(AObj.FDataEnd, vSize shl 3);
        AObj.FPHnds := @AObj.FHndsStart;
      end;
    else
      AObj.FPHnds := @vPBit;
  end;
  Reader.FObjMap.Objects[AIndex] := AObj.FEntity;
  AObj.ObjectBeginning := vObjectBeginning;
  AObj.FStringReader := AObj.CreateStringReader(vPBit, Version);
  case Version of
    acR2000..acR2004:
      AObj.BitSize := ReadRL(vPBit);
  end;
  ReadHandle(vPBit, vHCode, AObj.FHandle);
  AObj.FEntity.Handle := AObj.FHandle;
  AObj.ReadEEDItems(vPBit);
  Reader.DoReadData(AObj, vPBit);
end;

procedure TdwgObject.ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer);
var
  I{, J, K}: Integer;
begin
  if Version < acR2004 then
  begin
    if AFlags and $C = 0 then
    begin
      FFirstChildHandle := ReadID(APBit);
      FLastChildHandle := ReadID(APBit);
//      J := FObjMap.IndexOfHandle(FFirstChildHandle);
//      K := FObjMap.IndexOfHandle(FLastChildHandle);
//      if J > K then
//      begin
//        SwapInts(J, K);
//        SwapUInt64(FLastChildHandle, FFirstChildHandle);
//        FSwapped := True;
//      end;
//      FSubEnts := K - J + 1;
//      if (J >= 0) and (K >= 0) then
//      begin
//        FChildren := TsgInt64List.Create(0, FSubEnts);
//        for I := J to K do
//          ChildrenAdd(Reader.FObjMap.MapElement[I].Handle);
//      end;
    end;
  end
  else
    if FSubEnts > 0 then
    begin
      ForceChildrenList(FSubEnts);
      FFirstChildHandle := ReadID(APBit);
      ChildrenAdd(FFirstChildHandle);
      for I := 1 to FSubEnts - 1 do
        ChildrenAdd(ReadID(APBit));
    end;
end;

procedure TdwgObject.ReadSubEnts(AReader: TdwgReader);
begin
  if Version >= acR2004 then
    ReadSubEntsR2004(AReader)
  else
    ReadSubEntsR2000(AReader);
end;

procedure TdwgObject.ReadSubEntsR2000(AReader: TdwgReader);
var
  I: Integer;
  vOwner: TsgDXFEntity;
  vNext: UInt64;
  vObj: TdwgObject;
  vElem: PdwgMapElementList;
  vObjectsMap: TsgObjectsMapAccess;

  procedure Next(AObjectsMap: TsgObjectsMapAccess; var ANext: UInt64);
  begin
    repeat
      Inc(I);
    until (I > AObjectsMap.Count) or (AObjectsMap.GetPMapElement(I)^.Obj = nil);
    if (I <= AObjectsMap.Count) and (AObjectsMap.GetPMapElement(I)^.Handle <> ANext) then
      ANext := AObjectsMap.GetPMapElement(I)^.Handle
    else
      ANext := cnstBadHandle;
  end;

begin
//  if FSwapped then
//    vNext := FLastChildHandle
//  else
  vObjectsMap := TsgObjectsMapAccess(Reader.FObjMap);
  vNext := FFirstChildHandle;
  repeat
    I := vObjectsMap.IndexOfHandle(vNext);
    if I >= 0 then
    begin
      vElem := vObjectsMap.GetPMapElement(I);
      if not Assigned(vElem^.Obj) then
      begin
        vObj := ReadChild(vElem^.Location, vNext, I);
        if Assigned(vObj) then
        try
          vNext := vObj.FSibling;
          vObj.References(AReader);
          vObj.PostReferences(Reader);
          vOwner := vObj.GetOwner(AReader);
          if not Assigned(vOwner) or (vObj.OwnerAdd(vOwner) < 0) then
            FreeAndNil(vObj.FEntity);
          vObjectsMap.Objects[I] := vObj.FEntity;
        finally
          FreeAndNil(vObj);
        end
        else
          Next(vObjectsMap, vNext);
      end
      else
        Next(vObjectsMap, vNext);
    end;
  until (vNext = cnstBadHandle) or (I < 0);
end;

procedure TdwgObject.ReadSubEntsR2004(AReader: TdwgReader);
var
  I, C: Integer;
begin
  I := AReader.FReaderGroupsStack2004.Add(Self);
  try
    C := GetChildrenCount;
    FChildIndex := 0;
    while (FChildIndex < C) and not AReader.Stopped do
    begin
      Child[Children[FChildIndex]];
      Inc(FChildIndex);
    end;
  finally
    AReader.FReaderGroupsStack2004.Delete(I);
  end;
end;

function TdwgObject.ReadText: string;
begin
  Result := FStringReader.ReadText;
end;

procedure TdwgObject.References(AReader: TdwgReader);
begin
  ReadOwner(AReader);
  FReactors.Remove(FOwner);
  ReadSubEnts(AReader);
end;

procedure TdwgObject.SetBitSize(const AValue: Integer);
begin
end;

procedure TdwgObject.SetObjectBeginning(const APosition: TsgInt64);
begin
end;

procedure TdwgObject.SetReader(const Value: TdwgReader);
begin
  if FReader <> Value then
  begin
    FReader := Value;
    if Assigned(FReader) then
    begin
      FColorReader := FReader.FColorReader;
      FColorReader.Update(Self);
    end;
  end;
end;

procedure TdwgObject.SetTransparency(AValue: Integer);
begin
end;

procedure TdwgObject.SkipHandles(var APBit: TsgInt64);
var
  I: Integer;
  vHCode: Byte;
  vHandle: UInt64;
begin
  if Version >= acR2004 then
  begin
    for I := 0 to FReactorsCount - 1 do
      FReactors.Add(ReadID(APBit));
    if FXDicHandlePresent then
      FXDict := ReadID(APBit); //xdicobjhandle (hard owner)
  end
  else
    repeat
      vHandle := ReadID(APBit, vHCode);
      case vHCode of
        cntDWGObjHandleSoftPointer: FReactors.Add(vHandle);
        cntDWGObjHandleHardOwner: FXDict := vHandle;
      end;
    until vHCode = cntDWGObjHandleHardOwner;
end;

{public}

procedure TdwgObject.ApplyAnnotativeData(const AEEData: TsgCADExtendedData);
var
  I: Integer;
begin
  FEntity.Annotative := True;
  FAnnotativeFlags := 0;
  I := 1;
  while I < AEEData.DataCount do
  begin
    if (AEEData.DataType[I] = edtInt16) and (AEEData.DataCode[I] = 1070) then
      FAnnotativeFlags := FAnnotativeFlags shl 16 or AEEData.DataInt16[I];
    Inc(I);
  end;
end;

function TdwgObject.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
begin
  if Reader.IsAnnotativeAppID(AAppID) then
  begin
    ApplyAnnotativeData(AEEData);
    Result := True;
  end
  else
    if Reader.IsURLAppID(AAppID) then
    begin
      Reader.ConvExtDataToDXF(AEEData);
      FEntity.SetExtDataHyperLink(AEEData);
      Result := True;
    end
    else
      Result := False;
end;

function TdwgObject.ChildrenAdd(Handle: UInt64): Integer;
begin
  Result := TsgInt64List(FChildren).Add(Handle);
end;

constructor TdwgObject.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create;
  FVersion := AVersion;
  FCorrectData := True;
  FReactors := TsgInt64List.Create;
  FLineWeight := fLineWeightByLayer;
  FEEDataList := TsgObjectCollection.Create;
  FEEDataList.Duplicates := dupIgnore;
end;

function TdwgObject.CreateStringReader(var APBit: TsgInt64;
  AVersion: TsgDWGVersion): TdwgStringReader;
begin
  case AVersion of
    acR2007..DWGVersionHigh:
      Result := TdwgWideStringReaderParse.Create(@FStrsStart, Reader.Conv.CodePage);
  else
    Result := TdwgAnsiStringReaderParse.Create(@APBit, Reader.Conv.CodePage);
  end;
end;

destructor TdwgObject.Destroy;
begin
  FChildren.Free;
  FReactors.Free;
  if Assigned(FEEDataList) then
    FEEDataList.Clear;
  FEEDataList.Free;
  FStringReader.Free;
  FColorReader := nil;
  FACADExtData.Free;
  inherited Destroy;
end;

function TdwgObject.Find(AHandle: UInt64; var I: Integer): Boolean;
begin
  if Assigned(FChildren) then
    I := TsgInt64List(FChildren).IndexOf(AHandle)
  else
    I := -1;
  Result := I >= 0;
end;

procedure TdwgObject.ForceChildrenList(const ASubEnts: Integer);
begin
  FChildren := TsgInt64List.Create(0, ASubEnts);
end;

{ TdwgReader }

function TdwgReader.AddEEDItems(AEntityHandle: UInt64; AEEDItems: TsgObjectCollection): Integer;
var
  vEntityEEDItems: TsgObjectCollectionAccess;
  vHashItem: TsgHashItemObject;
begin
  vEntityEEDItems := TsgObjectCollectionAccess(TsgDXFConverterAccess(Conv).EntityEEDItems);
  vHashItem.HashCode := AEntityHandle;
  vHashItem.Data := AEEDItems;
  Result := vEntityEEDItems.Count;
  vEntityEEDItems.InsertBase(Result, @vHashItem);
end;

procedure TdwgReader.LinkObjectLinkCollections;
var
  I: Integer;
  L: TObjectLinkRecArray;
begin
  Pointer(L) := FLinks;
  I := Low(L);
  while I < FLinksCount do
  begin
    if L[I].obj <> nil then
      SetOrdProp(L[I].obj, L[I].Prop, IntPtr(L[I].Link.PropItem));
    Inc(I);
  end;
  Pointer(L) := nil;
end;

procedure TdwgReader.PrelinkObjectLinkCollections;
var
  I: Integer;
begin
  for I := 0 to FLinksCount - 1 do
  begin
    if TObjectLinkRecArray(FLinks)[I].Link.Collection = nil then
      if TObjectLinkRecArray(FLinks)[I].Link.ClassType = TsgBlockRecordLink then
        TObjectLinkRecArray(FLinks)[I].Link.FCollection := Conv.Sections[csBlockRecords];
  end;
end;

function TdwgReader.AddLink(obj: TObject; const APropName, AValue: string;
  AProxyClass: TClass): Integer;
begin
  if AProxyClass = nil then
    AProxyClass := obj.ClassType;
  Result := AddLink(obj, GetPropInfo(AProxyClass, APropName), AValue, AProxyClass);
end;

function TdwgReader.AddLink(obj: TObject; AProp: Pointer; const AValue: string;
  AProxyClass: TClass): Integer;
var
  ObjectLinkClass: TsgObjectLinkClass;
  vTypeData: PTypeData;
begin
  Result := -1;
  if AValue <> '' then
  begin
    ObjectLinkClass := nil;
    if PPropInfo(AProp)^.PropType^{$IFNDEF SGFPC}^{$ENDIF}.Kind = tkClass then
    begin
      vTypeData := GetTypeData(PPropInfo(AProp)^.PropType{$IFNDEF SGFPC}^{$ENDIF});
      if (vTypeData^.ClassType = TsgDXFBlockRecord) or (vTypeData^.ClassType = TsgDXFBlock) then
        ObjectLinkClass := TsgBlockRecordLink;
    end;
    if Assigned(ObjectLinkClass) then
    begin
      Result := FLinksCount;
      if FLinksCount = Length(TObjectLinkRecArray(FLinks)) then
        SetLength(TObjectLinkRecArray(FLinks), ListGrow(Length(TObjectLinkRecArray(FLinks))));
      Inc(FLinksCount);
      TObjectLinkRecArray(FLinks)[Result].obj := obj;
      TObjectLinkRecArray(FLinks)[Result].Prop := AProp;
      TObjectLinkRecArray(FLinks)[Result].Link := ObjectLinkClass.Create;
      TObjectLinkRecArray(FLinks)[Result].Link.AsString := AValue;
    end;
  end;
end;

//function TdwgReader.AddLink(obj: TObject; AProp: Pointer; const AValue: UInt64;
//  AProxyClass: TClass): Integer;
//begin
//  Result := -1;
//  if AValue <> cnstBadHandle then
//  begin
//    Result := FLinksCount;
//    if FLinksCount = Length(TObjectLinkRecArray(FLinks)) then
//      SetLength(TObjectLinkRecArray(FLinks), ListGrow(Length(TObjectLinkRecArray(FLinks))));
//    Inc(FLinksCount);
//    TObjectLinkRecArray(FLinks)[Result].obj := obj;
//    TObjectLinkRecArray(FLinks)[Result].Prop := AProp;
//    TObjectLinkRecArray(FLinks)[Result].Link := TsgDatabaseHandleLink.Create;
//    TObjectLinkRecArray(FLinks)[Result].Link.AsHandle := AValue;
//    TsgDatabaseHandleLink(TObjectLinkRecArray(FLinks)[Result].Link).FCollection := FObjMap;
//  end;
//end;

//function TdwgReader.AddLink(obj: TObject; const APropName: string;
//  const AValue: UInt64; AProxyClass: TClass): Integer;
//begin
//  if AProxyClass = nil then
//    AProxyClass := obj.ClassType;
//  Result := AddLink(obj, GetPropInfo(AProxyClass, APropName), AValue, AProxyClass);
//end;

procedure TdwgReader.AddMaterialLink(AMaterial: UInt64; AEntity: UInt64);
var
  I: Integer;
  vObjects: TsgInt64List;
begin
  if AMaterial <> cnstBadHandle then
  begin
    I := FMaterials.IndexOf(AMaterial);
    if I < 0 then
      I := FMaterials.Add(AMaterial, TsgInt64List.Create);
    vObjects := TsgInt64List(FMaterials[I].Data);
    vObjects.Add(AEntity)
  end;
end;

function TdwgReader.CheckSentinel(P: Pointer; const ASentinel: TsgDWGSentinel): Boolean;
begin
  Result := CompareMem(P, @ASentinel, SizeOf(TsgDWGSentinel));
end;

function TdwgReader.Code2007ByClassName(const AObjectBeginning, APBit: TsgInt64;
  AReadedCode: Integer): Integer;
var
  I: Integer;
  vcnAcDbName: string;
  vPStart, vPBit, vPHndStart: TsgInt64;
  vEndBit: Cardinal;
begin
  case AReadedCode of
    498 .. 499: // entity or object
      begin
        Result := AReadedCode;
        vPBit := APBit;
        vPStart := AObjectBeginning;
        Init2007ObjectData(vPStart, vPBit, vPHndStart, vEndBit);
        vcnAcDbName := ReadR21Text(vPBit);
        if Pos('cn:', vcnAcDbName) = 1 then
        begin
          Delete(vcnAcDbName, 1, 3);
          if Copy(vcnAcDbName, 1, 4) <> 'CIrd' then
          begin
            I := FAcDbClasses.IndexOf(vcnAcDbName);
            if I >= 0 then
              Result := TsgObjectInt64(FAcDbClasses.Objects[I]).FieldInt;
          end;
        end;
      end;
  else
    Result := AReadedCode;
  end;
end;

function TdwgReader.ConvExtDataToDXF(const AExtData: TsgCADExtendedData): Boolean;
var
  vHandle: UInt64;
begin
  Result := False;
  if (AExtData.DataCount > 0) and (AExtData.DataType[0] = edtInt64) and
     (AExtData.DataCode[0] = String_1001) then
  begin
    vHandle := AExtData.DataInt64[0];
    AExtData.ChangeType(0, edtString);
    AExtData.DataString[0] := GetTableNameByHandle(vHandle, nil);
    Result := True;
  end;
end;

procedure TdwgReader.CRead(ASeek, ASize: Integer);
var
  vStream: TStream;
begin
  vStream := FSource;
  if vStream = nil then
    vStream := FStream;
  if (ASeek < 0) or (ASize < 0) or (ASeek + ASize > vStream.Size) then
    raise EReadError.Create(sCorruptedDWGFileHeader);
  Read(ASeek, ASize);
end;

procedure TdwgReader.DoException;
var
  vMessage: string;
begin
  if Assigned(FExceptionClass) then
  begin
    TsgDXFConverterAccess(FConverter).ClearBlocks;
    vMessage := FExceptionClass.ClassName;
    if FExceptionEntityHandle > 0 then
      vMessage := vMessage + cnstEntityHandle + IntToHex(FExceptionEntityHandle, 0);
    vMessage := vMessage + '.';
    raise FExceptionClass.Create(vMessage);
  end;
end;

procedure TdwgReader.DoProgress(AStage: TProgressStage);
begin
  if Assigned(FOnProgress) then
    FOnProgress(AStage, FObjMap.ObjectsLoaded, FObjMap.Count + 1);
end;

function TdwgReader.DoReadData(AObj: TdwgObject;
  var APBit: TsgInt64): Boolean;
begin
  Result := False;
  try
    AObj.ReadData(APBit);
    Result := AObj.FCorrectData;
  except
    on E: Exception do
    begin
      if not Assigned(FExceptionClass) then
      begin
        FExceptionClass := ExceptClass(E.ClassType);
        FExceptionEntityHandle := AObj.FHandle;
      end;
    end;
  end;
end;

function TdwgReader.EntAddRef(AEnt: TsgDXFEntity): Integer;
var
  Item: TsgOwneredItem;
begin
  Item := TsgOwneredItem(TsgDXFEntityAccess(AEnt).GetNamedItem);
  if Assigned(Item) then
    Result := Item.ObjAddRef
  else
    Result := -1;
end;

function TdwgReader.EntRelease(AEnt: TsgDXFEntity): Integer;
var
  Item: TsgOwneredItem;
begin
  Result := -1;
  if Assigned(AEnt) then
  begin
    Item := TsgOwneredItem(TsgDXFEntityAccess(AEnt).GetNamedItem);
    if Assigned(Item) then
      Result := Item.ObjRelease;
  end;
end;

function TdwgReader.ExtractFromDatabase(AHandle: UInt64): Integer;
begin
  Result := FObjMap.IndexOfHandle(AHandle);
  if Result >= 0 then
    FObjMap.Objects[Result] := nil;
end;

{function TdwgReader.FindLocation(const AHandle: UInt64;
  var AIndex: Integer): Integer;
begin
   AIndex := FObjMap.IndexOfHandle(AHandle);
   if AIndex > -1 then
     Result := FObjMap.MapElement[AIndex].Location
   else
     Result := 0;
end;}

{function TdwgReader.FinishObject(AObj: TdwgObject;
  AOwner: TsgDXFEntity; AIndex: Integer): TsgDXFEntity;
begin
  Result := nil;
  if Assigned(AOwner) and (AObj.OwnerAdd(AOwner) >= 0) then
  begin
    Result := AObj.FEntity;
    FObjMap.Objects[AIndex] := Result;
  end
  else
    FreeAndNil(AObj.FEntity)
end;}

function TdwgReader.GetArrowTypeByHandle(const ABlockRecordHandle: UInt64): TsgDimArrowType;
var
  vBlockRecord: TsgDXFBlockRecord;
begin
  if ABlockRecordHandle <> cnstBadHandle then
  begin
    vBlockRecord := TsgDXFBlockRecord(ObjByHandle(ABlockRecordHandle, TsgDXFBlockRecord));
    Result := GetArrowTypeByName(vBlockRecord.Name, datUndefined);
  end
  else
    Result := datClosedfilled;
end;

function TdwgReader.GetDictionary(const Name: string): TsgDXFDictionary;
begin
  Result := TsgDXFDictionary(Conv.Sections[csObjects].FindEntByName(Name));
  if not Assigned(Result) then
  begin
    Result := TsgDXFDictionary.Create;
    Result.Name := Name;
    Conv.Sections[csObjects].AddEntity(Result);
  end;
end;

function TdwgReader.GetEntityEEDItemsSorted: Boolean;
begin
  Result := TsgDXFConverterAccess(Conv).EntityEEDItems.Sorted;
end;

function TdwgReader.GetGlobalName(const AName: string): string;
begin
  Result := FGlobalNames[FGlobalNames.Add(AName)];
end;

function TdwgReader.GetPHeadVarStruct: PsgHeadVarStruct;
begin
  Result := TsgDWGConverter(FConverter).PHeadVarStruct;
end;

function TdwgReader.GetProxyReaderImpl: TsgProxyReaderImpl;
const
  cnstEmptyPos: TsgInt64 = ();
begin
  if not Assigned(FProxyReaderImpl) then
    FProxyReaderImpl := TsgProxyReaderImpl.Create(Conv,
      TDWGProxyReader.Create(@cnstEmptyPos, Version));
  Result := FProxyReaderImpl;
end;

function TdwgReader.GetTableNameByHandle(const AHandle: UInt64;
  AClass: TsgDXFEntityClass): string;
var
  vEntity: TsgDXFEntity;
begin
  vEntity := ObjByHandle(AHandle, AClass);
  if Assigned(vEntity) then
    Result := vEntity.Name
  else
    Result := '';
end;

function TdwgReader.GetXDataProcs: TsgXDataProcs;
begin
  Result := TsgDXFConverterAccess(Conv).XDataProcs;
end;

procedure TdwgReader.Init2007ObjectData(var AStartBit, APStrStart, APHndStart: TsgInt64;
  var AEndBit: Cardinal; ANeedReadEndBit: Boolean = True);
var
  vPBit: TsgInt64;
  vStrsSize: Cardinal;
begin
  vPBit := APStrStart;
  if ANeedReadEndBit then
    AEndBit := Cardinal(ReadRL(vPBit));
  APHndStart := MakeBit(AStartBit, AEndBit);
  vStrsSize := 0;
  Read2007StrDataLength(AStartBit, AEndBit, vStrsSize);
  Dec(AEndBit, vStrsSize);
  APStrStart := MakeBit(AStartBit, AEndBit);
  AStartBit := vPBit;
end;

procedure TdwgReader.Init2010ObjectData(var AStartBit, APStrStart,
  APHndStart: TsgInt64; const AIdsPos: Cardinal; var AEndBit: Cardinal);
begin
  AEndBit := AIdsPos;
  APStrStart := AStartBit;
  Init2007ObjectData(AStartBit, APStrStart, APHndStart, AEndBit, False);
end;

procedure TdwgReader.InitLayouts(var AModel, APaper: TsgDXFBlock);
var
  vLayout: TsgDXFEntity;
  vPaper, vModel: TsgDXFBlock;
  vLayouts: TsgDXFEntity;

  // ALayouts - section
  // ACurrentLayout - in/out
  procedure _CleaningLayouts(ALayouts: TsgDXFEntity;
    var ACurrentLayout: TsgDXFLayout);
  var
    I, J: Integer;
    vTmpLayout: TsgDXFLayoutAccess;
    vTmpBlock: TsgDXFEntity;
  begin
    I := ALayouts.Count - 1;
    while I >= 1 do
    begin
      vTmpLayout := TsgDXFLayoutAccess(ALayouts[I]);
      if vTmpLayout.IsEmpty then
      begin
        vTmpBlock := nil;
        if Assigned(vTmpLayout.BlockRecord) then
          vTmpBlock := TsgDXFBlockRecordAccess(vTmpLayout.BlockRecord).Block;
        vTmpLayout.ClearReferences;
        if vTmpLayout = ACurrentLayout then
          ACurrentLayout := nil;
        J := FObjMap.IndexOfHandle(vTmpLayout.Handle);
        if J >= 0 then
          FObjMap.Objects[J] := nil;
        ALayouts.DeleteEntity(I);
        vTmpLayout.Free;
        if I >= 2 then // do not delete AModel and APaper blocks
          if Assigned(vTmpBlock) then
          begin
            if Assigned(vTmpBlock.Owner) then
              vTmpBlock.Owner.RemoveEntity(vTmpBlock);
            if (EntRelease(vTmpBlock) <= 0) and (vTmpBlock.Owner = nil) then
            begin
              if vTmpBlock = FPaper then
                FPaper := nil;
              if vTmpBlock = FModel then
                FModel := nil;
              ExtractFromDatabase(vTmpBlock.Handle);
              vTmpBlock.Free;
            end;
          end;
      end;
      Dec(I);
    end;
  end;

begin
  vLayouts := Conv.Sections[csLayouts];
  vModel := AModel;
  if not Assigned(vModel.Layout) then
  begin
    vLayout := TsgDXFLayout.Create;
    vLayouts.AddEntity(vLayout);
    TsgDXFLayout(vLayout).PaperSpaceBlock := vModel;
  end;
  vPaper := APaper;
  if not Assigned(vPaper.Layout) then
  begin
    vLayout := TsgDXFLayout.Create;
    vLayout.Name := sLayout + '1';
    vLayouts.AddEntity(vLayout);
    TsgDXFLayout(vLayout).PaperSpaceBlock := vPaper;
  end;
  vLayouts.RemoveEntity(vModel.Layout);
  TsgDXFGroupAccess(vLayouts).SortEntByHandle;
  vLayouts.InsertEntity(0, vModel.Layout);
  if Conv.HeadVarStruct.TileMode = 0 then
  begin
    vLayout := nil;
    vPaper := APaper;
    if Assigned(vPaper) then
      vLayout := vPaper.Layout;
  end
  else
    vLayout := vModel.Layout;
  _CleaningLayouts(vLayouts, TsgDXFLayout(vLayout));
  if Assigned(vLayout) then
    Conv.DefaultLayoutIndex := vLayouts.IndexOfEntity(vLayout);
end;

procedure TdwgReader.InitViewportsByList;
var
  I, J, K: Integer;
  vList: TsgInt64List;
  vHashItem: TsgHashItemObject;
  vPaperSpaceBlockRecordHandle: UInt64;
  vBlockRecord: TsgDXFBlockRecordAccess;
  vBlock: TsgDXFBlockAccess;
  vViewport: TsgDXFEntity;
begin
  for I := 0 to FViewports.Count - 1 do
  begin
    vHashItem := FViewports[I];
    vPaperSpaceBlockRecordHandle := vHashItem.HashCode;
    J := FObjMap.IndexOfHandle(vPaperSpaceBlockRecordHandle);
    if J >= 0 then
    begin
      vBlockRecord := TsgDXFBlockRecordAccess(FObjMap.MapElement[J].Obj);
      vBlock := TsgDXFBlockAccess(vBlockRecord.Block);
      vBlock.FViewPortOrdIndex := -1;
      vBlock.FViewPortWithMinIndex := nil;
      vList := TsgInt64List(vHashItem.Data);
      for K := 0 to vList.Count - 1 do
      begin
        J := FObjMap.IndexOfHandle(vList[K]);
        if J >= 0 then
        begin
          vViewport := TsgDXFEntity(FObjMap.MapElement[J].Obj);
          if Assigned(vViewport) and (vViewport.EntType = ceViewport) then
          begin
            Inc(vBlock.FViewPortOrdIndex);
            TsgDXFViewport(vViewport).OrdIndex := vBlock.FViewPortOrdIndex;
            TsgDXFViewport(vViewport).ThisID := vBlock.FViewPortOrdIndex + 1;
            if vBlock.FViewPortWithMinIndex = nil then
              vBlock.FViewPortWithMinIndex := TsgDXFViewport(vViewport);
          end;
        end;
      end;
    end;
  end;
end;

function TdwgReader.IsACADAppID(const AAppID: UInt64): Boolean;
begin
  Result := AAppID = FACADAppID;
end;

function TdwgReader.IsRelatedAppID(const AAppID: UInt64): Boolean;
begin
  Result := AAppID = FAppID;
end;

function TdwgReader.IsURLAppID(const AAppID: UInt64): Boolean;
begin
  Result := AAppID = FURLAppID;
end;

function TdwgReader.IsAnnotativeAppID(const AAppID: UInt64): Boolean;
var A: TsgDXFEntity;
begin
//  Result := AAppID = FAnnotativeAppID;
  A := ObjByHandle(AAppID,TsgDXFAppID);
  Result := Assigned(A) and A.Annotative;
end;

function TdwgReader.IsAcDbAttrAppID(const AAppID: UInt64): Boolean;
begin
  Result := AAppID = FAcDbAttrAppID;
end;

{function TdwgReader.LoadEntity(const AObject: TdwgObject): Boolean;
begin
  Result := False;
  try
    AObject.Loads(Self);
    Result := True;
  except
    on E: Exception do
      if not Assigned(FExceptionClass) then
      begin
        FExceptionClass := ExceptClass(E.ClassType);
        FExceptionEntityHandle := AObject.FEntity.Handle;
      end;
  end;
end;}

procedure TdwgReader.LoadHeaderHandles;
var
  I: Integer;
  vPHeadVS: PsgHeadVarStruct;
  vBlock: TsgDXFBlock;
begin
  vPHeadVS := GetPHeadVarStruct;
  vPHeadVS.CLayer := GetTableNameByHandle(FHeaderHandles.CLAYER, TsgDXFLayer);
  vPHeadVS.CELType := GetTableNameByHandle(FHeaderHandles.CELTYPE, TsgDXFLineType);
  vPHeadVS.TextStyle := GetTableNameByHandle(FHeaderHandles.TEXTSTYLE,
    TsgDXFStyle);
  vPHeadVS.DimStyle := GetTableNameByHandle(FHeaderHandles.DIMSTYLE,
    TsgDXFDimensionStyle);
  if Version >= acR2000 then
  begin
    vPHeadVS.DimProps.Arrows.Blk := GetArrowTypeByHandle(FHeaderHandles.DIMBLK);
    vPHeadVS.DimProps.Arrows.Blk1 := GetArrowTypeByHandle(FHeaderHandles.DIMBLK1);
    vPHeadVS.DimProps.Arrows.Blk2 := GetArrowTypeByHandle(FHeaderHandles.DIMBLK2);
    vPHeadVS.DimProps.Arrows.LrBlk := GetArrowTypeByHandle(FHeaderHandles.DIMLDRBLK);
  end;
  if FArrows.Count > 0 then
  begin
    for I := 0 to FArrows.Count - 1 do
    begin
      vBlock := FConverter.BlockByName(FArrows[I]);
      if not Assigned(vBlock) then
      begin
        vBlock := TsgDXFBlock.Create;
        vBlock.Name := FArrows[I];
        GenerateArrow(FConverter, vBlock, GetArrowTypeByName(vBlock.Name, datClosedfilled));
        FConverter.Sections[csBlocks].AddEntity(vBlock);
      end;
    end;
  end;
end;

procedure TdwgReader.ReadPlotSettings(Obj: TdwgObject; const APlotSettings: PsgPlotSettingsData;
  var APBit: TsgInt64);
begin
  APlotSettings^.PageSetupName := Obj.ReadText;
  APlotSettings^.PrintOrConfigName := Obj.ReadText;
  APlotSettings^.PlotLayoutFlags := ConvertIntegerToPlotLayoutFlags(ReadBS(APBit));
  APlotSettings^.UnprintableMargin.Left := ReadBD(APBit);
  APlotSettings^.UnprintableMargin.Bottom := ReadBD(APBit);
  APlotSettings^.UnprintableMargin.Right := ReadBD(APBit);
  APlotSettings^.UnprintableMargin.Top := ReadBD(APBit);
  APlotSettings^.PlotPaperSize.X := ReadBD(APBit);
  APlotSettings^.PlotPaperSize.Y := ReadBD(APBit);
  APlotSettings^.PaperSize := Obj.ReadText;
  APlotSettings^.PlotOrigin.X := ReadBD(APBit);
  APlotSettings^.PlotOrigin.Y := ReadBD(APBit);
  APlotSettings^.PlotPaperUnits := TsgPlotPaperUnits(ReadBS(APBit));
  APlotSettings^.PlotRotation := GetPlotRotation(ReadBS(APBit));
  APlotSettings^.PlotType := TsgPlotType(ReadBS(APBit));
  APlotSettings^.PlotWindowAreaMin.X := ReadBD(APBit);
  APlotSettings^.PlotWindowAreaMin.Y := ReadBD(APBit);
  APlotSettings^.PlotWindowAreaMax.X := ReadBD(APBit);
  APlotSettings^.PlotWindowAreaMax.Y := ReadBD(APBit);
  if Version < acR2004 then
    APlotSettings^.PlotViewName := Obj.ReadText;
  APlotSettings^.NumeratorOfCustomPrintScale := ReadBD(APBit);
  APlotSettings^.DenominatorOfCustomPrintScale := ReadBD(APBit);
  APlotSettings^.CurrentStyleSheet := Obj.ReadText;
  APlotSettings^.StandardScaleType := ReadBS(APBit);
  APlotSettings^.FloatingPointScaleFactor := ReadBD(APBit);
  APlotSettings^.PaperImageOrigin.X := ReadBD(APBit);
  APlotSettings^.PaperImageOrigin.Y := ReadBD(APBit);
  if Version >= acR2004 then
  begin
    APlotSettings^.ShadePlotMode := ReadBS(APBit);
    APlotSettings^.ShadePlotResolutionLevel := ReadBS(APBit);
    APlotSettings^.ShadePlotCustomDPI := ReadBS(APBit);
  end;
end;

procedure TdwgReader.ReadPrototype(ASeek, ASize: Integer);
begin
  CRead(ASeek, ASize);
  FSegmentedFile := TsgSegmentedFile.CreateFromMemory(FConverter, FMem, ASize);
  FSegmentedFile.ReadFile;
end;

function TdwgReader.LTypeByIndex(AIndex: Integer): TsgDXFLineType;
begin
  Result := Conv.LTypes[AIndex + 2];
end;

procedure TdwgReader.LinkMaterials;
var
  I, J: Integer;
  vObjects: TsgInt64List;
  vMaterial: TsgDXFMaterial;
  vEntity: TsgDXFEntity;
begin
  for I := FMaterials.Count - 1 downto 0 do
  begin
    vMaterial := TsgDXFMaterial(FDataBase.Child[FMaterials[I].HashCode]);
    vObjects := TsgInt64List(FMaterials[I].Data);
    if Assigned(vMaterial) then
    begin
      for J := 0 to vObjects.Count - 1 do
      begin
        vEntity := FDataBase.Child[vObjects[J]];
        if Assigned(vEntity) then
          vEntity.Material := vMaterial;
      end;
    end;
    FMaterials.Delete(I);
    vObjects.Free;
  end;
end;

{procedure TdwgReader.NewObject(ACode: Integer; var AObj: TdwgObject);
var
  vClass: TdwgClass;
begin
  if (ACode=54) or (ACode=55) then
    Exit;
  if ACode >= 500 then
    ACode := VarCode(ACode);
  if ACode = 498 then
    ACode := cnstProxyEntityIndex;
  if ACode > cnstLimit then
    Exit;
  vClass := cnstDwgClassTosgDwgClass[ACode].Objs;
  if vClass=nil then
    Exit;
  AObj := vClass.Create;
  AObj.Reader := Self;
  AObj.FEntity := cnstDwgClassTosgDwgClass[ACode].Ents.Create;
  FConverter.DoCreate(AObj.FEntity);
end;}

function TdwgReader.ObjByHandle(AHandle: UInt64;
  AClass: TsgDXFEntityClass): TsgDXFEntity;
begin
  Result := FDataBase.Child[AHandle];
  if (AClass <> nil) and not (Result is AClass) then
    Result := nil;
end;

function TdwgReader.PMSpace(var APBit: TsgInt64; AObj: TdwgObject;
  var APHeadVar: PsgHeadVarStruct; var ALimMin, ALimMax: TFPoint): TFRect;
var
  I: Integer;
begin
  APHeadVar^.InsBase := Read3D(APBit);
  Result.TopLeft := Read3D(APBit);
  Result.BottomRight := Read3D(APBit);
  SwapDoubles(Result.Top, Result.Bottom);
  ALimMin := Read2RD(APBit);
  ALimMax := Read2RD(APBit);
  SeekGeneral(APBit,[seBD]);
  APHeadVar.UCSORG := Read3D(APBit);
  APHeadVar.UCSXDir := Read3D(APBit);
  APHeadVar.UCSYDir := Read3D(APBit);
  AObj.ReadID(APBit);
  if Version >= acR2000 then
  begin
    AObj.ReadID(APBit);
    SeekGeneral(APBit, [seBS]);
    AObj.ReadID(APBit);
    for I := 0 to 5 do
      SeekGeneral(APBit,[se3D]);
  end;
end;

procedure TdwgReader.Read(ASeek,ASize: Integer);
begin
  if FSource <> nil then
  begin
    FMem := FSource.Memory;
    Inc(FMem, ASeek);
    Exit;
  end;
  if ASeek >= 0 then
    FStream.Position := ASeek;//FStream.Seek(ASeek,soFromBeginning);
  if ASize > FSize then
  begin
    ReallocMem(FMem,ASize);
    FSize := ASize;
  end;
  FStream.ReadBuffer(FMem^,ASize);
end;

procedure TdwgReader.Read2007StrDataLength(const APStart: TsgInt64;
  var AEndBit, AStrSize: Cardinal);
var
  vPBit: TsgInt64;
begin
  Dec(AEndBit);
  vPBit := MakeBit(APStart, AEndBit);
  if ReadBit(vPBit) = 1 then
  begin
    Dec(AEndBit, $10);
    vPBit := MakeBit(APStart, AEndBit);
    AStrSize := Word(ReadRS(vPBit));
    if AStrSize and $8000 <> 0 then
    begin
      Dec(AEndBit, $10);
      vPBit := MakeBit(APStart, AEndBit);
      AStrSize := AStrSize and $7FFF;
      AStrSize := (Word(ReadRS(vPBit)) shl $F) or AStrSize;
    end;
  end;
end;

procedure TdwgReader.ReadAppInfo(ASeek, ASize: Integer);
const
  cntProdInfoElemName = 'ProductInformation';
var
  vPBit: TsgInt64;
  vStringReader: TdwgStringReader;
  S: string;
  vProductInfoAttribs: TsgStringList;
begin
  CRead(ASeek, ASize);
  InitInt64(FMem, vPBit);
  vStringReader := nil;
  try
    if PInteger(FMem)^ > 3 then
    begin
      vStringReader := TdwgAnsiStringReader.Create(@vPBit, FConverter.CodePage);
      FAppInfo.InfoName := vStringReader.ReadInfoText;
      Inc2(vPBit, 4 shl 3);
      FAppInfo.ID := vStringReader.ReadInfoText;
      FAppInfo.ProductXMLElement := vStringReader.ReadInfoText;
      FAppInfo.Version := vStringReader.ReadInfoText;
    end
    else
    begin
      vStringReader := TdwgWideStringReader.Create(@vPBit, FConverter.CodePage);
      Inc2(vPBit, 4 shl 3);
      FAppInfo.InfoName := vStringReader.ReadInfoText;
      Inc2(vPBit, (4 + 16) shl 3);
      FAppInfo.Version := vStringReader.ReadInfoText;
      Inc2(vPBit, 16 shl 3);
      FAppInfo.Comment := vStringReader.ReadInfoText;
      Inc2(vPBit, 16 shl 3);
      if Ord2(vPBit) - TsgNativeInt(FMem) < ASize then
        FAppInfo.ProductXMLElement := vStringReader.ReadInfoText;
    end;
    S := Copy(FAppInfo.Comment, 1, 2);
    if (S = '"<') or (S = '<') then
      SwapString(FAppInfo.Comment, FAppInfo.ProductXMLElement);
    S := FAppInfo.ProductXMLElement;
    if (Length(S) > 0) and (S[1] = '"') then
      Delete(S, 1, 1);
    if (Length(S) > 0) and (S[Length(S)] = '"') then
      Delete(S, Length(S), 1);
    sgFunction.StringReplace(S, '\"', '"');
    if Length(S) > 3 then
    begin
      if S[1] = '<' then
        Delete(S, 1, 1);
      if S[Length(S)] = '>' then
        Delete(S, Length(S), 1);
      if S[Length(S)] = '/' then
        S[Length(S)] := ' ';
      S := TrimLeft(S);
      if SameText(Copy(S, 1, Length(cntProdInfoElemName)), cntProdInfoElemName) then
      begin
        S := TrimLeft(Copy(S, Length(cntProdInfoElemName) + 1, MaxInt));
        sgFunction.StringReplace(S, ' ="', '=');
        sgFunction.StringReplace(S, '="', '=');
        vProductInfoAttribs := TsgStringList.Create;
        try
          vProductInfoAttribs.LineBreak := '" ';
          vProductInfoAttribs.Text := S;
          FSGSaved := vProductInfoAttribs.Values['name'] = 'CAD VCL';
        finally
          vProductInfoAttribs.Free;
        end;
      end;
    end;
  finally
    vStringReader.Free;
  end;
end;

procedure TdwgReader.ReadClasses(ASeek, ASize: Integer);
var
  I, vN, vC, vClassID, vDwgVersion: Integer;
  vEndBit: Cardinal;
  vPBit, vPEnd, vPR21Strs, vPR21StrsEnd, vPHndsStart: TsgInt64;
  vString, vAcDbClass: string;
  vSize64: UInt64;
begin
  CRead(ASeek, ASize);
  if not CheckSentinel(FMem, cnstClassesSectionBeginSentinel) then
    Exit;
  Inc(FMem, SizeOf(cnstClassesSectionBeginSentinel));
//R2010+ (only present if the maintenance version is greater than 3!)
//RL : unknown, possibly the high 32 bits of a 64-bit size?
  if ((Version >= acR2010) and (FMetadata.ReleaseVersion > 3)) or (Version >= acR2018) then
  begin
    vSize64 := PUInt64(FMem)^;
    Inc(PByte(FMem), SizeOf(UInt64));
  end
  else
  begin
    vSize64 := PCardinal(FMem)^;
    Inc(PByte(FMem), SizeOf(Cardinal));
  end;
  InitInt64(FMem, vPBit);
  if (vSize64 < 20) or (vSize64 > ASize - 20) then
    Exit;
  vPR21Strs := vPBit;
  vPEnd := vPBit;
  Inc2(vPEnd, vSize64 shl 3 - 20);
  case Version of
    acR2004:
        SeekGeneral(vPBit,[seBS,seRC,seRC,seB]);
    acR2007..DWGVersionHigh:
      begin
        //ReadRL(vPBit);
        vPR21Strs := vPBit;
        Init2007ObjectData(vPBit, vPR21Strs, vPHndsStart, vEndBit);
        vPEnd := vPBit;
        Inc2(vPEnd, vEndBit);
        vPR21StrsEnd := vPR21Strs;
        Inc2(vPR21StrsEnd, Cardinal(vSize64) shl 3 - vEndBit - 20);
        SeekGeneral(vPBit,[seBS,seRC,seRC,seB]);
      end;
  end;
  if FClasses=nil then
    FClasses := TsgIntegerList.Create;
  FStringReader.Update(@vPBit, Conv.CodePage);
  while Comp2(vPBit, vPEnd) do
  begin
    vN := ReadBS(vPBit);
    SeekGeneral(vPBit, [seBS]);
    case Version of
      acR2007..DWGVersionHigh:
        begin
          ReadR21TextWithCheck(vPR21Strs, vPR21StrsEnd, 8);
          vAcDbClass := ReadR21TextWithCheck(vPR21Strs, vPR21StrsEnd, 8);
          vString := ReadR21TextWithCheck(vPR21Strs, vPR21StrsEnd, 8);
        end;
    else
      FStringReader.ReadText;
      vAcDbClass := FStringReader.ReadText;
      vString := FStringReader.ReadText;
    end;
    SeekGeneral(vPBit, [seB]); // vZombie
    vClassID := ReadBS(vPBit);
    case Version of
      acR2004..DWGVersionHigh:
        begin
          ReadBL(vPBit); //Number of objects created of this type in the current DB (DXF 91).
          vDwgVersion := ReadBS(vPBit);//  Dwg Version
          ReadBS(vPBit);//  Maintenance release version.
          if (FMetadata.OrigFileSavedReleaseVer = 255) or
             ((Version >= acR2018) and (vDwgVersion = FMetadata.OrigFileSavedVer)) then
            SeekGeneral(vPBit, [seBS, seBS]) //bz id=8307
          else
            SeekGeneral(vPBit, [seBL, seBL]); //  Unknown (normally 0L) //  Unknown (normally 0L)
        end;
    end;
    Dec(vN,500);
    if vN < 0 then
      Continue;
    vString := AnsiUpperCase(vString);
    vC := CodeFromName(vString);
    if vC=0 then
      if (vClassID = 498) {and (vZombie = 1)} then
        vC := cnstProxyEntityIndex // see NewObject
      else
        Continue;
    if FClasses.Count <= vN then
      FClasses.Count := vN+1;
    if (vAcDbClass <> '') and (vC <> cnstProxyEntityIndex) then
    begin
      I := FAcDbClasses.AddObject(vAcDbClass, nil);
      if FAcDbClasses.Objects[I] = nil then
        FAcDbClasses.Objects[I] := TsgObjectWithField.CreateInt(vN + 500);
    end;
    FClasses[vN] := vC;
  end;
end;

procedure TdwgReader.ReadHeadVars(ASeek,ASize: Integer);
var
  vPBit: TsgInt64;
  I, vViewFlags: Integer;
  vEndBit: Cardinal;
  vCode: Byte;
  vPHeadVar: PsgHeadVarStruct;
  vObj: TdwgObject;

  function ReadTimeStamp: TTimeStamp;
  begin
    Result.Date := ReadBL(vPBit);
    Result.Time := ReadBL(vPBit);
  end;

begin
  CRead(ASeek, ASize);
  if not CheckSentinel(FMem, cnstHeaderVariablesBeginSentinel) then
    Exit;
  vObj := TdwgEntity.Create(Version);
  try
    vObj.Reader := Self;
    vPHeadVar := GetPHeadVarStruct;
    Inc(PByte(FMem), SizeOf(TsgDWGSentinel));
    if ((Version in [acR2010, acR2013]) and (FMetadata.ReleaseVersion > 3)) or (Version >= acR2018) then
      Inc(PByte(FMem), SizeOf(UInt64))
    else
      Inc(PByte(FMem), SizeOf(Cardinal));
    InitInt64(FMem, vPBit);
    if Version > acR2004 then
    begin
      vObj.FStrsStart := vPBit;
      Init2007ObjectData(vPBit, vObj.FStrsStart, vObj.FHndsStart, vEndBit);
      FStringReader.Update(@vObj.FStrsStart, Conv.CodePage);
      vObj.FPHnds := @vObj.FHndsStart;
    end
    else
    begin
      FStringReader.Update(@vPBit, Conv.CodePage);
      vObj.FPHnds := @vPBit;
    end;
    vObj.FStringReader := FStringReader;

    if Version > acR2010 then
      ReadBUL(vPBit); //REQUIREDVERSIONS
    {
       if the DWG is correct the following 4 floating point values must be
         ReadBD 412148564080.0
         ReadBD 1.0
         ReadBD 1.0
         ReadBD 1.0
    }
    SeekGeneral(vPBit,[se3D,seBD]);
    for I := 0 to 3 do vObj.ReadText;
    SeekGeneral(vPBit,[seBL,seBL]);
    if Version < acR2000 then
      SeekGeneral(vPBit, [seBS]);
    if Version < acR2004 then
      vObj.ReadID;
    Inc2(vPBit, 2); //DIMASO, DIMSHO
    if Version in [acR13 .. acR14] then
      Inc2(vPBit, 1); //DIMSAV
    Inc2(vPBit, 3); //PLINEGEN, ORTHOMODE, REGENMODE
    vPHeadVar^.FillMode := ReadBit(vPBit) <> 0; // FILLMODE
    Inc2(vPBit, 3); //QTEXTMODE, PSLTSCALE, LIMCHECK
    if Version in [acR13 .. acR14] then
      Inc2(vPBit, 1); //BLIPMODE
    if Version >= acR2004 then
      Inc2(vPBit, 1); //Undocumented
    Inc2(vPBit, 4); //USRTIMER (User timer on/off), SKPOLY, ANGDIR, SPLFRAME
    if Version in [acR13 .. acR14] then
      Inc2(vPBit, 2); //ATTREQ, ATTDIA
    Inc2(vPBit, 2); //MIRRTEXT, WORLDVIEW
    if Version in [acR13 .. acR14] then
      Inc2(vPBit, 1);//WIREFRAME
    FConverter.DefaultLayoutIndex := ReadBit(vPBit) xor 1;
    vPHeadVar.TileMode := FConverter.DefaultLayoutIndex xor 1;
    Inc2(vPBit,4);
    if Version < acR2000 then
      Inc2(vPBit,1);
    SeekGeneral(vPBit, [seBS]);
    if Version < acR2000 then
      SeekGeneral(vPBit, [seBS]);
    for I := 0 to 3 do
      SeekGeneral(vPBit, [seBS]);

    SeekGeneral(vPBit, [seBS]);//AUPREC
    if Version < acR2000 then
      SeekGeneral(vPBit, [seBS]);//OSMODE

    vPHeadVar.AttMode := GetAttributeMode(ReadBS(vPBit));
    if Version < acR2000 then
      SeekGeneral(vPBit, [seBS]);//COORDS

    FConverter.PointDisplayMode := ReadBS(vPBit);
    vPHeadVar.PointDisplayMode := FConverter.PointDisplayMode;
    if Version < acR2000 then
      SeekGeneral(vPBit, [seBS]);
  //  if Version >= acR2004 then Inc2(P,102);
    if Version >= acR2004 then
      for I := 0 to 2 do
        SeekGeneral(vPBit,[seBL]);
    for I := 0 to 15 do
      SeekGeneral(vPBit, [seBS]);
    FUnits := ReadBS(vPBit);
    SeekGeneral(vPBit, [seBS,seBS]);
    FConverter.LTScale := ReadBD(vPBit);
    vPHeadVar.LTScale := FConverter.LTScale;
    vPHeadVar.TextSize := ReadBD(vPBit);
    SeekGeneral(vPBit, [se2D]);
    vPHeadVar.FilletRadius := ReadBD(vPBit);
    SeekGeneral(vPBit, [se2D]);
    vPHeadVar.PointDisplaySize := ReadBD(vPBit);
    for I := 0 to 11 do
      SeekGeneral(vPBit,[seBD]);
    vPHeadVar.CELTScale := ReadBD(vPBit);
    if Version in [acR13..{acR2004}DWGVersionHigh] then
      vObj.ReadText; //MENUNAME
    // timestamps: TDCREATE, TDCREATE
    Conv.DrwPropCreatedDateTime := JulianTimeStampToDateTime(ReadTimeStamp, Now);
    Conv.DrwPropModifiedDateTime := JulianTimeStampToDateTime(ReadTimeStamp, Now);
    TsgDXFConverterAccess(Conv).LastSave := 0;
    // 3 unknown
    if Version >= acR2004 then
      SeekGeneral(vPBit, [seBL, seBL, seBL]);
    // timestamps: TDINDWG, TDUSRTIMER
    Conv.DrwPropTotalEditingTime := ReadTimeStamp;
    ReadTimeStamp;
    vPHeadVar.CEColor :=  vObj.ReadColor(vPBit);
    FMaxHandle := ReadHandle(vPBit, vCode, 0);
    TsgDXFConverterAccess(FConverter).SetHandSeed(FMaxHandle);
    FHeaderHandles.CLAYER := vObj.ReadID;
    FHeaderHandles.TEXTSTYLE := vObj.ReadID;
    FHeaderHandles.CELTYPE := vObj.ReadID;
    if Version >= acR2007 then
      vObj.ReadID;
    FHeaderHandles.DIMSTYLE := vObj.ReadID;
    FHeaderHandles.CMLSTYLE := vObj.ReadID;
    if Version >= acR2000 then
      SeekGeneral(vPBit,[seBD]); // PSVPSCALE
    PMSpace(vPBit, vObj, vPHeadVar, vPHeadVar^.PLimMin, vPHeadVar^.PLimMax);
    FConverter.Extents := PMSpace(vPBit, vObj, vPHeadVar, vPHeadVar^.LimMin, vPHeadVar^.LimMax);
    if Version >= acR2000 then
    begin
      vPHeadVar.DimProps.Post := vObj.ReadText;
      vPHeadVar.DimProps.APost := vObj.ReadText;
    end
    else
    begin
      SeekGeneral(vPBit, [seB,seB]); // DIMTOL, DIMLIM
      vPHeadVar.DimProps.Tih := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.Toh := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.SE1 := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.SE2 := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.Alt := ReadBit(vPBit) <> 0;
      Inc2(vPBit, 1);// DIMTOFL
      vPHeadVar.DimProps.Sah := ReadBit(vPBit) <> 0; // DIMSAH
      vPHeadVar.DimProps.Tix := ReadBit(vPBit);
      Inc2(vPBit, 17);
      vPHeadVar.DimProps.SD1 := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.SD2 := ReadBit(vPBit) <> 0;
      Inc2(vPBit, 49);
      vPHeadVar.DimProps.Tad := ReadRC(vPBit);
      vPHeadVar.DimProps.LUnit := TsgDimLimitUnits(ReadBS(vPBit)); // DIMUNIT
      SeekGeneral(vPBit, [seBS]); // DIMAUNIT
      vPHeadVar.DimProps.Dec := ReadBS(vPBit);
      SeekGeneral(vPBit, [seBS,seBS,seBS]); // DIMTDEC, DIMALTU, DIMALTTD
      vObj.ReadID;
    end;
    vPHeadVar.DimProps.Scale := ReadBD(vPBit);
    vPHeadVar.DimProps.Asz := ReadBD(vPBit);
    vPHeadVar.DimProps.Exo := ReadBD(vPBit);
    SeekGeneral(vPBit,[seBD]);
    vPHeadVar.DimProps.Exe := ReadBD(vPBit);
    SeekGeneral(vPBit,[seBD, seBD]);//DIMRND, DIMDLE
    vPHeadVar.DimProps.Tp := ReadBD(vPBit);//DIMTP
    vPHeadVar.DimProps.Tm := ReadBD(vPBit);//DIMTM
    if Version >= acR2007 then //DimtFill, DimtFillColor
    begin
      SeekGeneral(vPBit, [seBD,seBD,seBS]);
      vObj.ReadColor(vPBit);
    end;
    if Version >= acR2000 then
    begin
      SeekGeneral(vPBit, [seB,seB]); // DIMTOL, DIMLIM
      vPHeadVar.DimProps.Tih := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.Toh := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.SE1 := ReadBit(vPBit) <> 0; // DIMSE1
      vPHeadVar.DimProps.SE2 := ReadBit(vPBit) <> 0; // DIMSE2
      vPHeadVar.DimProps.Tad := ReadBS(vPBit);
      SeekGeneral(vPBit, [seBS,seBS]);
    end;
    if Version >= acR2007 then
      SeekGeneral(vPBit, [seBS]);
    vPHeadVar.DimProps.Txt := ReadBD(vPBit);
    vPHeadVar.DimProps.Cen := ReadBD(vPBit); // DIMCEN
    SeekGeneral(vPBit, [seBD]); // DIMTSZ
    vPHeadVar.DimProps.AltF := ReadBD(vPBit);
    vPHeadVar.DimProps.LFac := ReadBD(vPBit);
    SeekGeneral(vPBit,[seBD,seBD]); // DIMTVP, DIMTFAC
    vPHeadVar.DimProps.Gap := ReadBD(vPBit);
    if Version < acR2000 then
    begin
      vPHeadVar.DimProps.Post := vObj.ReadText;
      vPHeadVar.DimProps.APost := vObj.ReadText;
      vPHeadVar.DimProps.Arrows.Blk := GetArrowTypeByName(vObj.ReadText, datClosedfilled);
      vPHeadVar.DimProps.Arrows.Blk1 := GetArrowTypeByName(vObj.ReadText, datUndefined);
      vPHeadVar.DimProps.Arrows.Blk2 := GetArrowTypeByName(vObj.ReadText, datUndefined);
      if (vPHeadVar.DimProps.Arrows.Blk = datUndefined) and
         (vPHeadVar.DimProps.Arrows.Blk1 <> datUndefined) and
         (vPHeadVar.DimProps.Arrows.Blk1 = vPHeadVar.DimProps.Arrows.Blk2) then
        vPHeadVar.DimProps.Arrows.Blk := vPHeadVar.DimProps.Arrows.Blk1;
    end
    else
    begin
      SeekGeneral(vPBit,[seBD]);
      vPHeadVar.DimProps.Alt := ReadBit(vPBit) <> 0;
      SeekGeneral(vPBit, [seBS]);
      Inc2(vPBit,1); // DIMTOFL
      vPHeadVar.DimProps.Sah := ReadBit(vPBit) <> 0; // DIMSAH
      vPHeadVar.DimProps.Tix := ReadBit(vPBit);
      Inc2(vPBit,1);
    end;
    vPHeadVar.DimProps.ClrD := vObj.ReadColor(vPBit);
    vPHeadVar.DimProps.ClrE := vObj.ReadColor(vPBit);
    vPHeadVar.DimProps.ClrT := vObj.ReadColor(vPBit);
    if Version >= acR2000 then
    begin
      SeekGeneral(vPBit, [seBS]);//DIMADEC
      vPHeadVar.DimProps.Dec := ReadBS(vPBit);
      SeekGeneral(vPBit, [seBS, seBS, seBS, seBS]);//DIMTDEC, DIMALTU, DIMALTTD, DIMAUNIT
      vPHeadVar.DimProps.Frac := ReadBS(vPBit);
      vPHeadVar.DimProps.LUnit := GetDimLimitUnitsType(ReadBS(vPBit));
      vPHeadVar.DimProps.DSep := Char(ReadBS(vPBit));
      SeekGeneral(vPBit, [seBS, seBS]);//DIMTMOVE, DIMJUST
      vPHeadVar.DimProps.SD1 := ReadBit(vPBit) <> 0;
      vPHeadVar.DimProps.SD2 := ReadBit(vPBit) <> 0;
      for I := 0 to 3 do
        SeekGeneral(vPBit, [seBS]);
      Inc2(vPBit,1);
      SeekGeneral(vPBit, [seBS]);
      if Version >= acR2007 then
      begin
        Inc2(vPBit,1);
        if Version >= acR2010 then
        begin
          SeekGeneral(vPBit, [seB, seBD]);//DIMTXTDIRECTION B 295;//DIMALTMZF BD ?
          vObj.ReadText;//DIMALTMZS T ?
          SeekGeneral(vPBit, [seBD]);//DIMMZF BD ?
          vObj.ReadText;//DIMMZS T ?
        end;
      end;
      FHeaderHandles.DIMTXTSTY := vObj.ReadID;
      FHeaderHandles.DIMLDRBLK := vObj.ReadID;
      FHeaderHandles.DIMBLK := vObj.ReadID;
      FHeaderHandles.DIMBLK1 := vObj.ReadID;
      FHeaderHandles.DIMBLK2 := vObj.ReadID;
      if Version >= acR2007 then
        for I := 0 to 2 do
          vObj.ReadID;
      vPHeadVar.DimProps.LwD := GetLineWeight(ReadBS(vPBit));
      vPHeadVar.DimProps.LwE := GetLineWeight(ReadBS(vPBit));
    end;
    // BLOCK CONTROL OBJECT (hard owner)
    FBlockRecords.FHandle := vObj.ReadID;
    // LAYER CONTROL OBJECT (hard owner)
    FLayerControl := vObj.ReadID;
    // STYLE CONTROL OBJECT (hard owner)
    FStyles.FHandle := vObj.ReadID;
    // LINETYPE CONTROL OBJECT (hard owner)
    FLTypeControl := vObj.ReadID;
    for I := 0 to 1 do
      vObj.ReadID;
    // VPORT CONTROL OBJECT (hard owner)
    FVPortControl := vObj.ReadID;
    // APPID CONTROL OBJECT (hard owner)
    FAppIDControl := vObj.ReadID;
    // DIMSTYLE CONTROL OBJECT (hard owner)
    FDimStyleControl := vObj.ReadID;
    if Version <= acR2000 then
      FVPortEntHdrs.FHandle := vObj.ReadID;
    FGroups := vObj.ReadID; // DICTIONARY (ACAD_GROUP) (hard pointer)
    FMLStylesDict := vObj.ReadID; // DICTIONARY (ACAD_MLINESTYLE) (hard pointer)
    FNamedObjects := vObj.ReadID; //DICTIONARY (NAMED OBJECTS) (hard owner)
    if Version >= acR2000 then
    begin
      SeekGeneral(vPBit, [seBS,seBS]);
      vObj.ReadText; //TV : HYPERLINKBASE
      vObj.ReadText; //TV : STYLESHEET
      FLayoutsDict := vObj.ReadID;
      FPlotSettingsDict := vObj.ReadID;
      vObj.ReadID;
      if Version >= acR2004 then
      begin
        FMaterialDict := vObj.ReadID;
        FColors := vObj.ReadID; // DICTIONARY (ACAD_COLOR) (hard pointer)
        if Version >= acR2007 then
          vObj.ReadID;
        if Version >= acR2013 then
          vObj.ReadID;
      end;
      vViewFlags := ReadBL(vPBit);
      vPHeadVar.CELWeight := GetLineWeight(vViewFlags and $1F);  //R2013 !!!
      if Version >= acR2000 then
        vPHeadVar.LwDisplay := Ord((vViewFlags and $0200) = 0);

      vPHeadVar.InsUnits := ReadBS(vPBit);

      if ReadBS(vPBit) = 3 then
        vObj.ReadID;
      vObj.ReadText;
      vObj.ReadText;

      if Version >= acR2004 then
      begin
        SeekGeneral(vPBit, [seRC,seRC,seRC]);
        vPHeadVar.XClipFrame := ReadRC(vPBit);
        vPHeadVar.DimAssoc := ReadRC(vPBit);
        SeekGeneral(vPBit, [seRC,seBS,seBS,seRC,seRC]);
        vObj.ReadText;
      end;
    end;

    FPaper.BlockRecord.Handle := vObj.ReadID; //BLOCK_RECORD (*PAPER_SPACE) (hard pointer)
    FModel.BlockRecord.Handle := vObj.ReadID; //BLOCK_RECORD (*MODEL_SPACE) (hard pointer)
    FLTByLayer := vObj.ReadID;     //LTYPE (BYLAYER) (hard pointer)
    FLTByBlock := vObj.ReadID;     //LTYPE (BYBLOCK) (hard pointer)
    FLTContinious := vObj.ReadID;  //LTYPE (CONTINUOUS) (hard pointer)
  finally
    vObj.FStringReader := nil;
    vObj.Free;
  end;
end;

procedure TdwgReader.ReadMeasurement(ASeek: Integer);
var
  vPHeadVar: PsgHeadVarStruct;
begin
  Read(ASeek, 4);
  vPHeadVar := GetPHeadVarStruct;
  vPHeadVar.Measurement := PInteger(FMem)^ <> 0;
end;

procedure TdwgReader.ReadMetadata(const Stream: TStream; const FileHeader: TdwgFileHeader);
var
  vPos: Int64;
  vPBit: TsgInt64;
  vBuf: array[0..5] of Byte;
  vBuffer: TMemoryStream;
begin
  if Version >= acR2004 then
  begin
    vPos := Stream.Position;
    vBuffer := TMemoryStream.Create;
    try
      Stream.Position := 0;
      vBuffer.CopyFrom(Stream, 0);
      InitInt64(vBuffer.Memory, vPBit);
      ReadBytesCustom(vPBit, vBuf, 6);  //ReadBytes(vPBit, vBuf, 6);
      ReadBytesCustom(vPBit, vBuf, 5);  //ReadBytes(vPBit, vBuf, 5);
      FMetadata.ReleaseVersion := ReadRC(vPBit);
      ReadRC(vPBit);//unknown
      FMetadata.PreviewAddress := ReadRL(vPBit);
      FMetadata.OrigFileSavedVer := ReadRC(vPBit);
      FMetadata.OrigFileSavedReleaseVer := ReadRC(vPBit);
      FMetadata.CodePage := ReadRS(vPBit);
      ReadRC(vPBit);//must to be zero
      ReadRC(vPBit);//not sure must be equal OrigFileSavedVer?
      ReadRC(vPBit);//not sure must be OrigFileSavedReleaseVer?
      FMetadata.SecurityFlags := ReadRL(vPBit);
      ReadRL(vPBit);//must to be zero
      FMetadata.SummaryInfoAddress := ReadRL(vPBit);
      FMetadata.VBAProjectAddress := ReadRL(vPBit);
      ReadRL(vPBit);//must to be $00000080
    finally
       vBuffer.Free;
       Stream.Position := vPos;
    end;
  end;
end;

(*function TdwgReader.ReadObject(ASeek: Integer; AHandle: UInt64;
  AIndex: Integer): TsgDXFEntity;
var
  vSize, vBitSize, vCode: Integer;
  vBitSizeVal: Byte;
  vBitSizeShift: Cardinal;
  vIdsBitSize, vIdsPos: UInt64;
  vEndBit: Cardinal;
  vPBit, vPStart, vObjectBeginning: TsgInt64;
  vObj: TdwgObject;
  vOwner: TsgDXFEntity;
  vHCode: Byte;
  vStrsStart, vHndsStart, vIdsStrart: TsgInt64;

begin
  Result := nil;
  Read(ASeek, 256);
  //repeat
    //I := PWord(FMem)^;
    //Inc(FMem, 2);
  //until I <= $7FFF;



  vSize := ReadMS(Pointer(FMem));// for future versions instead of repeat...until above
  InitInt64(FMem, vPBit);
  vObjectBeginning := vPBit;
  vObj := nil;
  if Version > acR2007 then
  begin
    vBitSizeShift := 0;
    vIdsBitSize := 0;
    while True do
    begin
      vBitSizeVal := Byte(ReadRC(vPBit));
      vIdsBitSize := UInt64(vBitSizeVal and $7F shl vBitSizeShift) or vIdsBitSize;
      Inc(vBitSizeShift, 7);
      if vBitSizeVal and $80 = 0 then
        Break;
    end;
    vBitSize := vSize shl 3;
    vIdsPos := vBitSize - vIdsBitSize;
    vIdsStrart := vPBit;
    Inc2(vIdsStrart, vIdsPos);
    Init2010ObjectData(vPBit, vStrsStart, vHndsStart, vIdsPos, vEndBit);
    NewObject(ReadDWGType(vPBit), vObj);
  end
  else
  begin
    vCode := ReadBS(vPBit);
    if Version = acR2007 then
      vCode := Code2007ByClassName(vObjectBeginning, vPBit, vCode);
    NewObject(vCode, vObj);
  end;
  DoProgress(psRunning);
  if Assigned(vObj) then
  try
    case Version of
      acR2007..DWGVersionHigh:
        begin
          if Version < acR2010 then
          begin
            vObj.FStrsStart := vPBit;
            vPStart := vObjectBeginning;
            Init2007ObjectData(vPStart, vObj.FStrsStart, vObj.FHndsStart,
              vEndBit);
            vPBit := vPStart;
          end
          else
          begin
            vObj.FStrsStart := vStrsStart;
            vObj.FHndsStart := vHndsStart;
          end;
          vObj.FDataEnd := vObjectBeginning;
          Inc2(vObj.FDataEnd, vSize shl 3);
        end;
    end;
    FObjMap.Objects[AIndex] := vObj.FEntity;
    vObj.ObjectBeginning := vObjectBeginning;
    vObj.FHandle := AHandle;
    vObj.FEntity.Handle := AHandle;
    case Version of
      acR2000..acR2004:
        vObj.BitSize := ReadRL(vPBit);
    end;
    ReadHandle(vPBit, vHCode, AHandle);
    vObj.ReadEEDItems(vPBit);
    DoReadData(vObj, vPBit);
    vObj.References(Self);
    vOwner := vObj.GetOwner(Self);
    if not Assigned(vOwner) or (vObj.OwnerAdd(vOwner) < 0) then
      FreeAndNil(vObj.FEntity);
    Result := vObj.FEntity;
    FObjMap.Objects[AIndex] := Result;
  finally
    vObj.Free;
  end;
end;*)

procedure TdwgReader.ReadObjMap(ASeek, ASize: Integer);
var
  vPStart, vPEnd: PAnsiChar;
  vH: UInt64;
  vLenght: Integer;
  vSize: Word;
begin
  CRead(ASeek, ASize);
  vPStart := FMem;
  vSize := Swap(PWord(vPStart)^);
  while vSize > 2 do
  begin
    vPEnd := vPStart + vSize;
    Inc(vPStart, 2);
    vH := 0;
    vLenght := 0;
    if Version >= acR2004 then
      vLenght := ASeek + ASize;
    while vPStart < vPEnd do
    begin
      vH := vH + ReadMC(Pointer(vPStart));
      vLenght := vLenght + ReadMCSigned(Pointer(vPStart));
      FObjMap.AddDwgElement(vH, vLenght);
    end;
    Inc(vPStart,2);
    vSize := Swap(PWord(vPStart)^);
  end;
  FObjMap.Sort;
end;

{procedure TdwgReader.ReadPart(AFirst,ALast: Integer; AList: TsgInt64List);
var
  vHandle,N: Int64;
  vIndex, vLocation: Integer;
  J: Integer;
  procedure GetNext;
  begin
    if AList <> nil then
    begin
      if J >= AList.Count then
        FNext := 0
      else
        FNext := AList[J];
    end;
  end;
begin
  N := FNext;
  FNext := 0;
  vLocation := 0;
  J := 0;
  vHandle := 0;
  if AList <> nil then
    AFirst := FObjMap.Count;
  GetNext;
  repeat
    if Stopped then Exit;
    if FNext <> 0 then
    begin
      vLocation := FindLocation(FNext,vIndex);
      if vIndex < 0 then
        FNext := 0;
      if (vIndex > -1) and (FObjMap.MapElement[vIndex].Obj <> nil) then
        FNext := 0
      else
        vHandle := FNext;
    end;
    if (FNext = 0) and (AFirst <= FObjMap.Count) then
    begin
      vHandle := FObjMap.MapElement[AFirst].Handle;
      vLocation := FObjMap.MapElement[AFirst].Location;
      vIndex := AFirst;
      Inc(AFirst);
    end;
    if (vIndex > -1) and (FObjMap.MapElement[vIndex].Obj = nil) then
      FObjMap.Objects[vIndex] := ReadObject(vLocation, vHandle, vIndex);
    if vIndex = ALast then Break;// Hide by commenting this line for previous version
    Inc(J);
    GetNext;
  until ((FNext = 0) and (AFirst > ALast));
  FNext := N;
end;}

{procedure TdwgReader.ReadPart(AFirst, ALast: Integer);
var
  I: Integer;
begin
  for I := 0 to FObjMap.Count do
    if FObjMap.MapElement[I].Obj = nil then
      FObjMap.Objects[I] := ReadObject(FObjMap.MapElement[I].Location, FObjMap.MapElement[I].Handle, I);
end;}

procedure TdwgReader.ReadSummaryInfo(ASeek, ASize: Integer);
var
  vDWGBits: TsgDWGBits;
  vPropCount: Word;
  I: Integer;
  vKey, vValue: string;
begin
  CRead(ASeek, ASize);
  vDWGBits :=  CreateDWGBitsByVersion(Version, FMem, Cardinal(ASize));
  if vDWGBits = nil then
    Exit;
  try
    FConverter.ClearDrawingProp;
    FConverter.DrwPropTitle := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropSubject := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropAuthor := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropKeywords := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropComments := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropSavedBy := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropRevisionNumber := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropHyperlinkBase := vDWGBits.ReadSummaryInfoText;
    FConverter.DrwPropTotalEditingTime := vDWGBits.ReadJulianDateIntAsTimeStamp(True, False);
    FConverter.DrwPropCreatedDateTime := vDWGBits.ReadJulianDateIntAsDateTime(True, False);
    FConverter.DrwPropModifiedDateTime := vDWGBits.ReadJulianDateIntAsDateTime(True, False);
    TsgDXFConverterAccess(FConverter).LastSave := 0;
    vPropCount := vDWGBits.ReadRS;
    for I := 1 to vPropCount do
    begin
      vKey := vDWGBits.ReadSummaryInfoText;
      vValue := vDWGBits.ReadSummaryInfoText;
      FConverter.CustomSummaryInfoAdd(vKey, vValue);
    end;
    vDWGBits.ReadRL;
    vDWGBits.ReadRL;
  finally
    vDWGBits.Free;
  end;
end;

procedure TdwgReader.ReadTemplate(ASeek, ASize: Integer);
var
  vSize: Word;
  vDWGBits: TsgDWGBits;
  vHeadVar: PsgHeadVarStruct;
begin
  CRead(ASeek, ASize);
  vDWGBits := TsgDWGBits.CreateFromMemory(FMem, ASize);
  try
    vSize := vDWGBits.ReadRS;
    if Version > acR2004 then
      vSize := 2 * vSize;
    if vSize > vDWGBits.Size then
      Exit;
    vDWGBits.SeekInBytes(vSize);
    vHeadVar := GetPHeadVarStruct;
    vHeadVar^.Measurement := vDWGBits.ReadRS <> 0;
  finally
    vDWGBits.Free;
  end;
end;

function TdwgReader.VarCode(ACode: Integer): Integer;
begin
  Result := ACode;
  Dec(ACode, 500);
  if (FClasses <> nil) and (ACode < FClasses.Count) then
    Result := FClasses[ACode];
  if Result = 0 then
    Result := ACode + 500;
end;

{protected}

procedure TdwgReader.ReadHeader;
type
  TUnpackDWG = function(Version: TsgDWGVersion; MS: TCustomMemoryStream; var SecCount: Integer): TMemoryStream;
var
  vLCount,vPos: Integer;
  vPFileHeader: PdwgFileHeader;
  vLocator,vLocator0: PdwgLocator;
  vUnpackFunc: TUnpackDWG;
  vPosition: Int64;
begin
  vPos := FStream.Position;
  CRead(vPos, SizeOf(TdwgFileHeader));
  vPFileHeader := PdwgFileHeader(FMem);
  FImage := vPFileHeader.ImageSeeker;
  if CADPreview then
  begin
    vPosition := FStream.Position;
    try
      ReadImage;
    finally
      if TsgDWGConverter(FConverter).FImage = nil then
        FStream.Position := vPosition;
    end;
  end;
  if TsgDWGConverter(FConverter).FImage <> nil then Exit;
  if vPFileHeader.CodePage <= High(arDWGCodePages) then
    TsgDWGConverter(FConverter).SetCodePage(arDWGCodePages[vPFileHeader.CodePage])
  else
    TsgDWGConverter(FConverter).SetCodePage(CP_ACP);
  vLCount := vPFileHeader.Locators;
  if (Version >= acR2004) and (vLCount and $FF000000 <> 0) then
    raise EdwgError.Create(sEncryptedByPassword);
  if Version >= acR2004 then
    vLCount := 0;
  Read(-1, vLCount * SizeOf(TdwgLocator));
  vLocator := PdwgLocator(FMem);
  vLocator0 := vLocator;
  FMem := nil;
  FSize := 0;
  FStream.Position := vPos;
  FConverter.SetSourceStream(FStream);
  FConverter.ClearDrawingProp;
  FSource := FConverter.Source;
  @vUnpackFunc := nil;
  case Version of
    //acR2004: vUnpackFunc := Unpack2004;
    acR2007: vUnpackFunc := UnpackR18Plus;
    acR2004, acR2010..DWGVersionHigh:
      vUnpackFunc := UnpackR24Up;
  end;
  if @vUnpackFunc <> nil then
  begin
    FSource := vUnpackFunc(Version, FSource, vLCount);
    FConverter.OwnSource := True;
    FConverter.Source := FSource;
    Read(SizeOf(Tdwg2004FileHeader) - SizeOf(TdwgLocators), SizeOf(TdwgLocator));
    vLocator := PdwgLocator(FMem);
  end;
  FStringReader := CreateStringReader(nil);
  try
    while vLCount > 0 do
    begin
      case vLocator.RecNumber of
        cntDWGSectHeaderIndex:
          begin
            ReadMetadata(FStream, vPFileHeader^);
            ReadHeadVars(vLocator.Seeker, vLocator.Size);
          end;
        cntDWGSectClassesIndex: ReadClasses(vLocator.Seeker, vLocator.Size);
        cntDWGSectHandlesIndex: ReadObjMap(vLocator.Seeker, vLocator.Size);
        cntDWGSectSummaryInfoIndex:
          case Version of
             acR2004..DWGVersionHigh:
               ReadSummaryInfo(vLocator.Seeker, vLocator.Size);
             else
               ReadMeasurement(vLocator.Seeker);
          end;
        cntDWGSectTemplateIndex:
          case Version of
             acR2007..DWGVersionHigh: ReadTemplate(vLocator.Seeker, vLocator.Size);
          end;
        cntDWGSectAppInfoIndex:
          ReadAppInfo(vLocator.Seeker, vLocator.Size);
        cntDWGSectPrototypeIndex:
          ReadPrototype(vLocator.Seeker, vLocator.Size);
      end;
      Inc(vLocator);
      Dec(vLCount);
    end;
  finally
    FreeMem(vLocator0);
  end;
end;

procedure TdwgReader.ReadImage;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  I,vOffset: Integer;
  vPointer: PAnsiChar;
  vStream: TMemoryStream;
  vBitmap: TBitmap;
{$IFDEF SG_INCLUDE_PNG}
  vPngImage: TPngImage;
{$ENDIF}
  vImage: TdwgImage;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  FStream.Position := FImage;
  FStream.Read(vImage,SizeOf(vImage));
  if not CheckSentinel(@vImage, cnstImageDataBeginSentinel) then
    Exit;
  for I := 0 to vImage.Count - 1 do
    if vImage.Images[I].RecNumber in [2{$IFDEF SG_INCLUDE_PNG} .. 6{$ENDIF}] then
    begin
      FStream.Position := vImage.Images[I].Seeker;
      vStream := TMemoryStream.Create;
      try
{$IFDEF SG_INCLUDE_PNG}
        if vImage.Images[I].RecNumber = 6 then
        begin
          vStream.Size := vImage.Images[I].Size;
          FStream.ReadBuffer(vStream.Memory^, vStream.Size);
        end
        else
{$ENDIF}
        begin
          vStream.Size := vImage.Images[I].Size + SizeOf(TBitmapFileHeader);
          vPointer := PAnsiChar(vStream.Memory) + SizeOf(TBitmapFileHeader);
          FStream.ReadBuffer(vPointer^, vImage.Images[I].Size);
          vOffset := PBitmapInfoHeader(vPointer).biSizeImage;
          PBitmapFileHeader(vStream.Memory)^.bfType := $4D42;
          PBitmapFileHeader(vStream.Memory)^.bfSize := vStream.Size;
          PBitmapFileHeader(vStream.Memory)^.bfReserved1 := 0;
          PBitmapFileHeader(vStream.Memory)^.bfReserved2 := 0;
          PBitmapFileHeader(vStream.Memory)^.bfOffBits := vStream.Size - vOffset;
        end;
        vStream.Position := 0;
        vBitmap := TBitmap.Create;
        try
{$IFDEF SG_INCLUDE_PNG}
          if vImage.Images[I].RecNumber = 6 then
          begin
            vPngImage := TPngImage.Create;
            try
              vPngImage.LoadFromStream(vStream);
              vPngImage.AssignTo(vBitmap);
            finally
              vPngImage.Free;
            end;
          end
          else
{$ENDIF}
            vBitmap.LoadFromStream(vStream);
          TsgDWGConverter(FConverter).FImage := vBitmap;
        except
          vBitmap.Free;
        end;
      finally
        vStream.Free;
      end;
      Exit;
    end;
{$ENDIF}
end;

procedure TdwgReader.ReadObjects;
var
  I: Integer;
  vEnt, vGroup: TsgDXFEntity;
  vVPorts: TsgDXFEntity;
  vVport: TsgDXFEntity;
  vNamedObjects: TsgDXFEntity;
  S: string;
  vFreeList: TsgObjectList;
  vObj: TsgOwneredItem;
  vProxyClipItem: TsgProxyClipItem;

  procedure DoReadPredefinedControl(AObj: TdwgObject);
  var
    J: Integer;
  begin
    J := FObjMap.IndexOfHandle(AObj.FHandle);
    if J >= 0 then
    begin
      FTables.ReadPredefined(FObjMap.MapElement[J].Location, AObj, J);
      AObj.OwnerAdd(AObj.GetOwner(Self));
      AObj.References(Self);
      AObj.PostReferences(Self);
    end;
  end;

begin
  if FObjMap.Count < 0 then
    Exit;
  DoProgress(psStarting);
  try
  Conv.Clear;
  vGroup := Conv.Sections[csLayouts];
  Conv.Sections[csObjects].RemoveEntity(vGroup);
  Conv.Sections[csLayouts] := nil;
  vGroup.Free;
  Conv.FillMainSection;

//  FDataBase.Child[FAppIDControl];
//  FDataBase.Child[FStyleControl];
//  FDataBase.Child[FLTypeControl];
//  FDataBase.Child[FLayerControl];
//  FDataBase.Child[FBlockControl];
//  FDataBase.Child[FDimStyleControl];
//
//  FDataBase.Child[FNamedObjects];

  ObjByHandle(FAppIDControl, nil);
  DoReadPredefinedControl(FStyles);
  ObjByHandle(FLTypeControl, nil);
  ObjByHandle(FLayerControl, nil);
  DoReadPredefinedControl(FVPortEntHdrs);
  DoReadPredefinedControl(FBlockRecords);

  ObjByHandle(FDimStyleControl, nil);

  vNamedObjects := ObjByHandle(FNamedObjects, nil);
  vNamedObjects.Owner.ExchangeEntity(vNamedObjects.Owner.IndexOfEntity(vNamedObjects), 0);

  FDataBase.ReadSubEnts(Self);
  EntityEEDItemsSorted := True;
  //ReadPart(0, FObjMap.Count);

  if (Version in [acR13..acR14]) and (FLayoutsDict = 0) then
  begin
    if vNamedObjects.FindEntByName(sAcadLayoutDictionary) = nil then
      if Assigned(FModel.Layout) and Assigned(FModel.Layout.Owner) then
        FModel.Layout.Owner.Name := sAcadLayoutDictionary
      else
        if Assigned(FPaper.Layout) and Assigned(FPaper.Layout.Owner) then
          FPaper.Layout.Owner.Name := sAcadLayoutDictionary;
  end;

  for I := 0 to FObjMap.Count - 1 do
  begin
    if FObjMap.HasName[I] then
    begin
      vEnt := TsgDXFEntity(TsgObjectsMapAccess(FObjMap).GetPMapElement(I)^.Obj);
      if Assigned(vEnt) then
      begin
        S := FObjMap.Name[I];
        if S <> '' then
          vEnt.Name := S;
      end;
    end;
  end;

  ForceStdDictionaries(Conv, FNamedObjects);
  InitLayouts(FModel, FPaper);

  vVport := nil;
  vVPorts := Conv.Sections[csVPorts];
  if Assigned(vVPorts) then
    if FActiveViewport <> cnstBadHandle then
      vVport := vVPorts.FindEntByHandle(FActiveViewport)
    else
    begin
      // do not use vVPorts.FindEntByName(sActiveVPort)
      // because first entlist item needed
      I := 0;
      while (I < vVPorts.Count) and not Assigned(vVport) do
        if sgSameText(vVPorts[I].Name, sActiveVPort) then
          vVport := vVPorts[I]
        else
          Inc(I);
    end;
  if not Assigned(vVport) then
    vVport := Conv.ActiveVPort;
  Conv.ActiveVPort := TsgDXFVport(vVport); // perform initialize viewtwistmatrix

  PrelinkObjectLinkCollections;
  LoadHeaderHandles;
  LinkObjectLinkCollections;

{$IFDEF SG_BTI}
  if XDataProcs <> nil then
    XDataProcs.Initialize(Conv); // sort ents
{$ENDIF}

  LinkMaterials;
  LinkDataBase(Conv);

{$IFDEF SG_BTI}
  vGroup := Conv.Sections[csBlocks];
  for I := 0 to vGroup.Count - 1 do
    if TsgDXFEntityAccess(vGroup[I]).GetEntTypeEx >= 0 then
    begin
      S := vGroup[I].Description;
      if S <> '' then
      begin
        vGroup[I].Name := S;
        if S[1] = cnstAsterisk then
          vGroup[I].Flags := vGroup[I].Flags or 1;
      end;
    end;
{$ENDIF}
  if Assigned(FProxyReaderImpl) then
  begin
    FProxyReaderImpl.UpdateHandles;
    FProxyReaderImpl.AddToDataBase;
    // update FObjMap, ApplyXRecodData may relese not existing objects links
    TsgObjectsMapAccess(FObjMap).Sorted := False;
    try
      for I := 0 to FProxyReaderImpl.ProxyClipsCount - 1 do
      begin
        vProxyClipItem := FProxyReaderImpl.ProxyClipItem[I];
        // FProxyReaderImpl.UpdateHandles SetHandle order Ent,BlkRec,Dict
        FObjMap.AddDwgElement(vProxyClipItem.Ent.Handle, -1, vProxyClipItem.Ent);
        FObjMap.AddDwgElement(vProxyClipItem.Dict.Handle, -1, vProxyClipItem.Dict);
      end;
    finally
      TsgObjectsMapAccess(FObjMap).Sorted := True; // do not need sort
    end;
  end;
  vFreeList := TsgObjectList.Create;
  try
    TsgDXFConverterAccess(Conv).ApplyXRecodData(vFreeList);
    for I := vFreeList.Count - 1 downto 0 do
    begin
      vObj := TsgOwneredItem(vFreeList[I]);
      if vObj.ObjRelease = 0 then
        vObj.Ancestor.Free;
    end;
  finally
    vFreeList.Free;
  end;
  TsgDXFConverterAccess(Conv).ApplyVariables;
  TsgDXFConverterAccess(Conv).CompositeMLAtt;
  LoadDataBase(Conv);
  //FConv.Loads(FConv.Sections[csEntities]);// previous version, now see TsgDXFLayout.Loaded
  finally
    DoProgress(psEnding);
  end;
end;

procedure TdwgReader.SetEntityEEDItemsSorted(const Value: Boolean);
var
  Items: TsgObjectCollectionAccess;
begin
  Items := TsgObjectCollectionAccess(TsgDXFConverterAccess(Conv).EntityEEDItems);
  if Items.Sorted <> Value then
  begin
    if Value then
    begin
      Items.Sort;
      Items.Flags := Items.Flags or cnstSortedBit
    end
    else
      Items.Flags := Items.Flags and not cnstSortedBit;
  end;
end;

procedure TdwgReader.SetVersion(const Value: TsgDWGVersion);
begin
  FVersion := Value;
  FDataBase.FVersion := FVersion;
  FTables.FVersion := FVersion;
  FBlockRecords.FVersion := FVersion;
  FVPortEntHdrs.FVersion := FVersion;
end;

function TdwgReader.Stopped: Boolean;
begin
  Result := TsgDWGConverter(FConverter).LoadStopped or Assigned(FExceptionClass);
end;

{public}

constructor TdwgReader.Create(AConv: TsgDXFConverter; S: TStream);
var
  ObjCollection: TsgObjCollection;
begin
  FAnnotationProps := TInterfaceList.Create;
  FGlobalNames := TsgStringList.Create;
  TsgStringList(FGlobalNames).CaseSensitive := False;
  TsgStringList(FGlobalNames).Sorted := True;
  TsgStringList(FGlobalNames).Duplicates := dupIgnore;
  FGlobalNames.Add(DictionaryName);
  FObjMap := TsgObjectsMap.Create;
  FConverter := AConv;
  FStream := S;
  FVersion := TsgDWGVersion(Conv.HeadVarStruct.Version);
  if FVersion <= acR2000 then
    FColorReader := TdwgIndexColorReader.Create
  else
    FColorReader := TdwgR18ColorReader.Create;
  FMetadata.ReleaseVersion := 6;
  FAcDbClasses := TsgStringList.Create;
  FAcDbClasses.CaseSensitive := False;
  FAcDbClasses.Sorted := True;
  FSegmentedFile := nil;

  FDataBase := TdwgDataBase.Create(Version);
  FDataBase.Reader := Self;

  FTables := TdwgTables.Create(Version);
  FTables.Reader := Self;
  FTables.FEntity := Conv.Sections[csTables];

  FBlockRecords := CreatePredefinedControl(48);
  FVPortEntHdrs := CreatePredefinedControl(70);
  FStyles := CreatePredefinedControl(52);
  TsgDXFBlockRecordsAccess(FBlockRecords.FEntity).Blocks := Conv.Sections[csBlocks];

  FModel := TsgDXFBlock.Create;
  FModel.Name := sModelSpace;
  EntAddRef(FModel);

  FPaper := TsgDXFBlock.Create;
  FPaper.Name := sPaperSpace;
  EntAddRef(FPaper);

  FViewports := TsgObjectCollection.Create;
  FViewports.Sorted := True;
  FViewports.Duplicates := dupIgnore;

  FArrows := TStringList.Create;
  FArrows.Sorted := True;
  FReaderGroupsStack2004 := TList.Create;

  FMaterials := TsgObjectCollection.Create;

  SetLength(TObjectLinkRecArray(FLinks), 1024);

  ObjCollection := TsgObjCollection.Create;
  ObjCollection.FMap := TsgObjectsMapAccess(FObjMap);
  TsgDXFConverterAccess(Conv).FMapExternal := ObjCollection as IsgObjCollection;
end;

function TdwgReader.CreatePredefinedControl(ACode: Integer): TdwgObject;
begin
  Result :=  cnstDwgClassTosgDwgClass[ACode].Objs.Create(Version);
  Result.Reader := Self;
  Result.FEntity := cnstDwgClassTosgDwgClass[ACode].Ents.Create;
  EntAddRef(Result.FEntity);
  Result.FOwnerObject := Conv.Sections[csTables];
end;

function TdwgReader.CreateStringReader(APBit: PsgInt64): TdwgStringReader;
begin
  case Version of
    acR2007..DWGVersionHigh:
      Result := TdwgWideStringReaderParse.Create(APBit, Conv.CodePage);
  else
    Result := TdwgAnsiStringReaderParse.Create(APBit, Conv.CodePage);
  end;
end;

function TdwgReader.GetStringReader(
  var APBit: TsgInt64): TdwgStringReader;
begin
  Result := FStringReader;
  Result.Update(@APBit, Conv.CodePage);
end;

destructor TdwgReader.Destroy;
var
  I: Integer;
  procedure DoReleaseEntity(var E);
  begin
    if (EntRelease(TsgDXFEntity(E)) = 0) and (TsgDXFEntity(E).Owner = nil) then
      FreeAndNil(TsgDXFEntity(E));
  end;
begin
  TsgDXFConverterAccess(Conv).FMapExternal := nil;
  FObjMap.Free;
  FClasses.Free;
  ClearObjects(FAcDbClasses);
  FAcDbClasses.Free;
  FSegmentedFile.Free;
  if FSource = nil then
    FreeMem(FMem);
  DoReleaseEntity(FPaper);
  DoReleaseEntity(FModel);
  DoReleaseEntity(FBlockRecords.FEntity);
  FBlockRecords.Free;
  if FVPortEntHdrs.FEntity.Owner <> nil then
    FVPortEntHdrs.FEntity.Owner.RemoveEntity(FVPortEntHdrs.FEntity);
  DoReleaseEntity(FVPortEntHdrs.FEntity);
  FVPortEntHdrs.Free;
  DoReleaseEntity(FStyles.FEntity);
  FStyles.Free;
  FTables.Free;
  FDataBase.Free;
  FViewports.Clear;
  FViewports.Free;
  FArrows.Free;
  FStringReader.Free;
  FReaderGroupsStack2004.Free;
  FColorReader := nil;
  FProxyReaderImpl.Free;
  for I := 0 to FAnnotationProps.Count - 1 do
    (FAnnotationProps[I] as IsgGlobalPropProvider).RemoveItem(cnstTextAnnotationWidth);
  TsgObjectCollection.FreeList(FMaterials);
  FGlobalNames.Free;
  for I := 0 to FLinksCount - 1 do
    TObjectLinkRecArray(FLinks)[I].Link.Free;
  Finalize(TObjectLinkRecArray(FLinks));
  inherited Destroy;
end;

procedure TdwgReader.ReadFile;
begin
  ClearAppInfo(FAppInfo);
  ReadHeader;
  if TsgDWGConverter(FConverter).FImage = nil then
    ReadObjects;
end;

{ TsgDWGConverter }

{protected}

function TsgDWGConverter.GetDWGVersion(const AString: AnsiString; var AVersion: TsgDWGVersion): Integer;
begin
  Result := inherited GetDWGVersion(AString, AVersion);
  case Result of
    0: ;
    1, 2:  raise EdwgError.CreateFmt(sUnsupportedOldDWGVer + ' "%s"', [AString])
  else
    raise EdwgError.CreateFmt(sUnsupportedNewDWGVer + ' "%s"', [AString]);
  end;
end;

procedure TsgDWGConverter.Initialize;
begin
  inherited Initialize;
  InitializeSectionsBegin;
end;

{public}

destructor TsgDWGConverter.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

{ TsgDWGImage }

{private}

function TsgDWGImage.GetPreview: TBitmap;
begin
  Result := TsgDWGConverter(Converter).FImage;
end;

{protected}

function TsgDWGImage.CreateConverter: TsgDXFConverter;
begin
  CBFormat := CF_DWG;
  Result := TsgDWGConverter.CreateEx(GetConverterParams);
end;

procedure TsgDWGImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if GetPreview = nil then
    inherited Draw(ACanvas, ARect)
  else
    ACanvas.StretchDraw(ARect, GetPreview);
end;

function TsgDWGImage.GetEmpty: Boolean;
begin
  Result := inherited GetEmpty and (GetPreview=nil);
end;

function TsgDWGImage.GetHeight: Integer;
begin
  if GetPreview <> nil then
    Result := GetPreview.Height
  else
    Result := inherited GetHeight;
end;

function TsgDWGImage.GetWidth: Integer;
begin
  if GetPreview <> nil then
    Result := GetPreview.Width
  else
    Result := inherited GetWidth;
end;

function TsgDWGImage.IsAutocadLType: Boolean;
begin
  Result := True;
end;

{public}

procedure TsgDWGImage.Assign(ASource: TPersistent);
begin
  if TsgDWGImage(ASource).GetPreview = nil then
    inherited Assign(ASource)
  else
  begin
    TsgDWGConverter(Converter).FImage := TBitmap.Create;
    TsgDWGConverter(Converter).FImage.Assign(TsgDWGImage(ASource).GetPreview);
  end;
end;

procedure TsgDWGImage.LoadFromStream(AStream: TStream);
var
  vReader: TdwgReader;
  vDWGVersion: TsgDWGVersion;
  vVersion: array[0..5] of AnsiChar;
  vHeadVar: PsgHeadVarStruct;
  vRead3D, vRead3RD: TdwgReadPoint;
//  vSortentsTable, vSortentsVar, vVarDict: TsgDXFEntity;

  {procedure SortPapersByHandle;
  var
    I: Integer;
    vBlock: TsgDXFBlockAccess;
  begin
    for I := 0 to Converter.Counts[csLayouts] - 1 do
    begin
      vBlock := TsgDXFBlockAccess(TsgDXFLayout(Converter.Sections[csLayouts][I]).PaperSpaceBlock);
      vBlock.SortEntByHandle;
    end;
  end;

  procedure CustomSort;
  begin
    if not Converter.SortEntities then
      SortPapersByHandle
  end;}

  procedure InitReaderProc(out ARead3D: TdwgReadPoint; out ARead3RD: TdwgReadPoint);
  begin
    @ARead3D := @Read3D;
    @ARead3RD := @Read3RD;
    if cnstLoadCad2D then
    begin
      @Read3D := @_Read3DNoElevation;
      @Read3RD := @_Read3RDNoElevation;
    end
    else
    begin
      @Read3D := @_Read3D;
      @Read3RD := @_Read3RD;
    end;
  end;

  procedure RestoreReaderProc(const ARead3D, ARead3RD: TdwgReadPoint);
  begin
    @Read3D := @ARead3D;
    @Read3RD := @ARead3RD;
  end;

begin
  inherited LoadFromStream(AStream);
  {$IFNDEF SG_OPENING_IN_THEADS}
  Loading := Self;
  {$ENDIF}
  try
    if AStream.Size = 0 then
    begin
      {$IFNDEF SG_OPENING_IN_THEADS}
      Loading := nil;
      {$ENDIF}
      GetExtents;
      Exit;
    end;
    AStream.ReadBuffer(vVersion,6);
    AStream.Position := AStream.Position - 6;
    TsgDWGConverter(Converter).GetDWGVersion(vVersion, vDWGVersion);
    vHeadVar := TsgDWGConverter(Converter).PHeadVarStruct;
    if vHeadVar <> nil then
      vHeadVar^.Version := Byte(vDWGVersion);
    case vDWGVersion of
      acR09..acR11: vReader := Tdwg11Reader.Create(Converter, AStream);
      acR12: vReader := Tdwg12Reader.Create(Converter, AStream);
    else
      vReader := TdwgReader.Create(Converter, AStream);
    end;
    TsgDXFConverterAccess(Converter).CreateXDataProcs;
    try
      TsgDXFConverterAccess(Converter).SetLoading(True, nil, DoUpdateEvent);
      vReader.Version := vDWGVersion;
      vReader.FOnProgress := DoOnProgress;
      InitReaderProc(vRead3D, vRead3RD);
      try
        vReader.ReadFile;
        FAppInfo := vReader.AppInfo;
      finally
        RestoreReaderProc(vRead3D, vRead3RD);
      end;
      Millimetres := vReader.FUnits <> 0;
      TsgDXFConverterAccess(Converter).SetLoading(False, nil, nil);
      vReader.DoException;
//      TsgDXFConverterAccess(vReader.Converter).ResetBlockRecords;
      Converter.SortEntities;
      vReader.InitViewportsByList;
      // sort entities
      (*
      vVarDict := Converter.Sections[csObjects].FindEntByName(sAcDbVariableDictionary);
      vSortentsVar := nil;
      if Assigned(vVarDict) then
        vSortentsVar := vVarDict.FindEntByName(sDictionaryVarSORTENTS);
      vSortentsTable := Converter.Sections[csObjects].FindEntByName(sDictionaryVarSORTENTS);
      if Assigned(vSortentsTable) then // dictionary
        CustomSort
      else
      begin
        if Assigned(vVarDict) then
        begin
          if Assigned(vSortentsVar) {and (StrToIntDef(TsgDXFDictionaryVar(vSortentsVar).Value, 0) and $03 <> 0)} then // dictionaryvar
            SortPapersByHandle
          else
            CustomSort
        end
        else
          CustomSort;
      end;
      *)
      SetDefaultViewPort(Converter);
      if Preview = nil then
        GetExtents;
    finally
      TsgDXFConverterAccess(Converter).DestroyXDataProcs;
      TsgDXFConverterAccess(Converter).ClearEED(vReader.FAppID);
      vReader.Free;
    end;
  finally
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := nil;
    {$ENDIF}
  end;
end;

procedure TsgDWGImage.LoadPreviewFromFile(const AFileName: string);
var
  vOld: Boolean;
begin
  vOld := CADPreview;
  CADPreview := True;
  try
    LoadFromFile(AFileName);
  finally
    CADPreview := vOld;
  end;
end;

procedure TsgDWGImage.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);
end;

{ ************* Objects ***************}

{TdwgBlock}

{protected}

constructor TdwgBlockRecord.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FBlock := TdwgBlock.Create(AVersion);
  FBlock.FBlockRecord := Self;
end;

destructor TdwgBlockRecord.Destroy;
begin
  FBlock.Free;
  inherited Destroy;
end;

function TdwgBlockRecord.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csBlockRecords];
end;

function TdwgBlockRecord.IsAnonymous: Boolean;
begin
  Result := FFlags and 1 <> 0;
  if not Result then
    if (Length(FEntryName) = 2) and (FEntryName[1] = cnstAsterisk) then
      Result := CharInSet(FEntryName[2], ['E', 'D']); // ? = *[A|B|C|...]
end;

function TdwgBlockRecord.OwnerAdd(AOwner: TsgDXFEntity): Integer;
begin
  Result := AOwner.IndexOfEntity(FEntity);
  if Result < 0 then
    Result := AOwner.AddEntity(FEntity);
end;

procedure TdwgBlockRecord.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vPaperIndex: Integer;
  vPreviewSize, vInsertCount: Integer;
{$IFDEF SG_BLOCK_PREVIEW}
  vData: Pointer;
  vBitmap: TsgBitmapAccess;
{$ENDIF}
begin
  inherited ReadData(APBit);
  FFlags := FFlags or ReadBit(APBit);
  FFlags := FFlags or ReadBit(APBit) shl 1;
  FFlags := FFlags or ReadBit(APBit) shl 2;
  FFlags := FFlags or ReadBit(APBit) shl 3;
  if IsAnonymous then
  begin
    if Reader.FBlockRecords.Find(FHandle, I) then
    begin
      FEntryName := FEntryName + IntToStr(I);
      if FFlags and 1 = 0 then
        FFlags := FFlags or 1;
    end;
  end
  else
  begin
    vPaperIndex := GetPaperIndex(FEntryName);
    case vPaperIndex of
      0:; // [*|$]Model_space
      1:
        begin
          if FHandle <> Reader.Paper.BlockRecord.Handle then
          begin
            if Reader.FBlockRecords.Find(FHandle, I) then
              FEntryName := FEntryName + IntToStr(I);
          end
          else
          begin
            if Reader.Paper.BlockRecord <> FEntity then
            begin
              FEntity.Free;
              FEntity := Reader.Paper.BlockRecord;
            end;
          end;
          // else [*|$]Paper_space
        end;
    end;
  end;
  FBlock.FEntity := TsgDXFBlockRecordAccess(FEntity).Block;
  FEntity.Name := FEntryName;
  if Version >= acR2000 then
    SeekGeneral(APBit, [seB]);
  if (FFlags and $C = 0) and (Version >= acR2004) then
    FSubEnts := ReadBL(APBit);
  TsgDXFBlockAccess(FBlock.FEntity).SetFlags(FFlags);
  TsgDXFBlock(FBlock.FEntity).Offset := Read3D(APBit);
  TsgDXFBlock(FBlock.FEntity).XrefPath := ReadText;
  vInsertCount := 0;
  if Version >= acR2000 then
  begin
    while ReadRC(APBit) <> 0 do
      Inc(vInsertCount);
    FEntity.Description := ReadText;
    vPreviewSize := ReadBL(APBit);
{$IFDEF SG_BLOCK_PREVIEW}
    if vPreviewSize > SizeOf(TBitmapInfoHeader) then
    begin
      GetMem(vData, vPreviewSize);
      try
        ReadBytesCustom(APBit, vData^, vPreviewSize);
        vBitmap := TsgBitmapAccess(TsgBitmap.Create);
        try
          vBitmap.LoadFromBitmapInfo(PBitmapInfo(vData));
          TsgDXFBlockRecord(FEntity).Preview.Graphic := vBitmap;
        finally
          vBitmap.Free;
        end;
      finally
        FreeMem(vData);
      end;
    end
    else
{$ENDIF}
      Inc2(APBit, vPreviewSize shl 3);

    if Version >= acR2007 then
    begin
      //Insert units BS 70; Explodable B 280; Block scaling RC 281
      SeekGeneral(APBit, [seBS, seB, seRC]);
    end;
  end;
  //-------------------------- Handle refs --------------------------
  // Block control handle (soft pointer)
  //[Reactors (soft pointer)]
  // +xdicobjhandle (hard owner) for Version < acR2004
  // NULL (hard pointer)
  ReadHandleRefs(APBit);
  //BLOCK entity. (hard owner)
  FBlock.FHandle := ReadID(APBit);
  //-------------------------- Handle refs --------------------------
  ReadSubEntIDs(APBit, FFlags);
  FBlockEnd := ReadID(APBit); // ENDBLK entity. (hard owner)
  if Version >= acR2000 then
  begin
    // Insert Handles H N insert handles, where N corresponds to the number
    // of insert count entries above (soft pointer).
    while vInsertCount > 0 do
    begin
      ReadID(APBit);
      Dec(vInsertCount);
    end;
    //Layout Handle H (hard pointer)
    FLayout := ReadID(APBit);
  end;
end;

procedure TdwgBlockRecord.ReadSubEntsR2000(AReader: TdwgReader);
var
  I: Integer;
begin
  I := -1;
  if Version <= acR2000 then
    I := AReader.FReaderGroupsStack2004.Add(Self);
  try
    inherited ReadSubEntsR2000(AReader);
  finally
    if I <> -1 then
      AReader.FReaderGroupsStack2004.Delete(I);
  end;
end;

procedure TdwgBlockRecord.References(AReader: TdwgReader);
var
  I: Integer;
begin

  I := AReader.FObjMap.IndexOfHandle(FBlock.FHandle);
  if (I >= 0) and (AReader.FObjMap.MapElement[I].Obj = nil) then
  begin
    ReadPredefined(AReader.FObjMap.MapElement[I].Location, FBlock, I);
    FBlock.References(AReader);
    FBlock.PostReferences(AReader);
  end;

  inherited References(AReader);

  //AReader.ObjByHandle(FLayout, TsgDXFLayout);
//  if (FLayout = cnstBadHandle) and (FFlags and 1 = 0) then // non anonomus blocks
//    TsgDXFGroupAccess(TsgDXFBlockRecordAccess(FEntity).Block).SortEntByHandle;
end;

procedure TdwgBlockRecord.SetReader(const Value: TdwgReader);
begin
  inherited SetReader(Value);
  FBlock.Reader := Reader;
end;

{TdwgDimStyle}

{protected}

constructor TdwgDimStyle.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FArrows := TdwgArrows.Create;
end;

destructor TdwgDimStyle.Destroy;
begin
  FArrows.Free;
  inherited Destroy;
end;

function TdwgDimStyle.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csDimStyles];
end;

procedure TdwgDimStyle.ReadData(var APBit: TsgInt64);
var
  vDS: TsgDXFDimensionStyleAccess;

  procedure ReadGroupCode_40_48;
  var
    vScale: Double;
  begin
    vScale := ReadBD(APBit);//DIMSCALE
    if vScale = 0 then
      vScale := Reader.Conv.HeadVarStruct.DimProps.Scale;
    vDS.Scale := vScale;
    vDS.ArrowSize := ReadBD(APBit);//DIMASZ
    vDS.ExtLineOffset := ReadBD(APBit);//DIMEXO
    SeekGeneral(APBit,[seBD]); //DIMDLI
    vDS.ExtLineExt := ReadBD(APBit);//DIMEXE
    // DIMRND, DIMDLE
    SeekGeneral(APBit,[seBD,seBD]);
    vDS.DIMTP := ReadBD(APBit);
    vDS.DIMTM := ReadBD(APBit);
  end;
  procedure ReadGroupCode_71_76;
  begin
    SeekGeneral(APBit, [seB,seB]); // DIMTOL, DIMLIM
    vDS.DIMTIH := ReadBit(APBit) = 1;
    vDS.DIMTOH := ReadBit(APBit) = 1;
    vDS.DIMSE1 := ReadBit(APBit) = 1;
    vDS.DIMSE2 := ReadBit(APBit) = 1;
  end;
  procedure ReadGroupCode_140_147;
  begin
    vDS.TextHeight := ReadBD(APBit);//DIMTXT
    vDS.SizeCenterMark := ReadBD(APBit);//DIMCEN
    SeekGeneral(APBit,[seBD,seBD]); //DIMTSZ, DIMALTF
    vDS.DIMLFAC := ReadBD(APBit);
    SeekGeneral(APBit,[seBD,seBD]); //DIMTVP, DIMTFAC
    vDS.TextOffset := ReadBD(APBit);//DIMGAP
  end;
  procedure ReadGroupCode_172_175;
  begin
    Inc2(APBit, 1); // DIMTOFL
    vDS.DIMSAH := ReadBit(APBit) <> 0; // DIMSAH
    vDS.DIMTIX := ReadBit(APBit);
    SeekGeneral(APBit, [seB]); //DIMSOXD
  end;
  procedure ReadGroupCode_176_178;
  begin
    vDS.DIMCLRD := ReadColor(APBit);//FReader.FConv.IntToColor(ReadBS(APBit));
    vDS.DIMCLRE := ReadColor(APBit);//FReader.FConv.IntToColor(ReadBS(APBit));
    vDS.DIMCLRT := ReadColor(APBit);//FReader.FConv.IntToColor(ReadBS(APBit));
  end;
  procedure ReadGroupCode_271_274;
  begin
    vDS.DIMDEC := ReadBS(APBit);
    SeekGeneral(APBit, [seBS,seBS,seBS]); //DIMTDEC, DIMALTU, DIMALTTD
  end;
  procedure ReadGroupCode_281_282;
  begin
    vDS.DIMSD1 := ReadBit(APBit) = 1;
    vDS.DIMSD2 := ReadBit(APBit) = 1;
  end;
  procedure ReadGroupCode_3_7;
  var
    vIsDimStyleActive: Boolean;

    procedure DoSetBlock(AStyle: TsgDXFDimensionStyle; AArrowName: string;
      AType: TsgDimNameVal; AIsDimStyleActive: Boolean);
    var
      vArrowType: TsgDimArrowType;
    begin
      vArrowType := GetArrowTypeByName(AArrowName, datUndefined);
      if AIsDimStyleActive and (AArrowName = '') then
      begin
        if vArrowType = datUndefined then
        begin
          if Reader.Conv.HeadVarStruct.DimProps.Arrows.Blks[AType] <> datUndefined then
            vArrowType := Reader.Conv.HeadVarStruct.DimProps.Arrows.Blks[AType];
        end;
      end;
      if vArrowType <> datUndefined then
        AArrowName := sgDimensionArrowTypeNames[vArrowType];
      case AType of
        vnDIMBLK: TsgDXFDimensionStyleAccess(AStyle).DIMBLKT := vArrowType;
        vnDIMBLK1: TsgDXFDimensionStyleAccess(AStyle).DIMBLK1T := vArrowType;
        vnDIMBLK2: TsgDXFDimensionStyleAccess(AStyle).DIMBLK2T := vArrowType;
      end;
      //TsgDXFDimensionStyleAccess(AStyle).SetBlocks(AType, AArrowName, Reader.Conv);
    end;

  begin
    ReadText;
    ReadText;
    vIsDimStyleActive := Reader.FHeaderHandles.DIMSTYLE = vDS.Handle;
    FArrows.Name[vnDIMBLK] := ReadText;
    FArrows.Name[vnDIMBLK1] := ReadText;
    FArrows.Name[vnDIMBLK2] := ReadText;
    DoSetBlock(vDS, FArrows.Name[vnDIMBLK], vnDIMBLK, vIsDimStyleActive);
    DoSetBlock(vDS, FArrows.Name[vnDIMBLK1], vnDIMBLK1, vIsDimStyleActive);
    DoSetBlock(vDS, FArrows.Name[vnDIMBLK2], vnDIMBLK2, vIsDimStyleActive);
  end;

begin
  inherited ReadData(APBit);
  FEntity.Name := FEntryName;
  vDS := TsgDXFDimensionStyleAccess(FEntity);
  if SameText(vDS.Name, Reader.Conv.HeadVarStruct.DimStyle)  then
    vDS.InitProps(@Reader.Conv.HeadVarStruct);
  if Version < acR2000 then
  begin
    ReadGroupCode_71_76;
    vDS.FDimProps.Alt := ReadBit(APBit) <> 0; //DIMALT
    ReadGroupCode_172_175;
    SeekGeneral(APBit, [seRC,seRC]); //DIMALTD, DIMZIN
    ReadGroupCode_281_282;
    // DIMTOLJ, DIMJUST, DIMFIT, DIMUPT
    SeekGeneral(APBit, [seRC,seRC,seRC,seB]);
    SeekGeneral(APBit, [seRC,seRC,seRC]); //DIMTZIN, DIMALTZ, DIMALTTZ
    vDS.TextPosVert := TsgDimTextPosVert(ReadRC(APBit));//DIMTAD
    vDS.DIMLUNIT := TsgDimLimitUnits(ReadBS(APBit)); //DIMUNIT
    SeekGeneral(APBit, [seBS]); // DIMAUNIT
    ReadGroupCode_271_274;
    ReadGroupCode_40_48;
    ReadGroupCode_140_147;
    ReadGroupCode_3_7;
    ReadGroupCode_176_178;
  end
  else
  begin
    vDS.FDimProps.Post := ReadText;  //DIMPOST
    vDS.FDimProps.APost := ReadText; //DIMAPOST
    ReadGroupCode_40_48;
    if Version >= acR2007 then
    begin
      // DIMFXL, DIMJOGANG, DIMTFILL, DIMTFILLCLR  (ReadCMC)
      SeekGeneral(APBit,[seBD,seBD,seBS,seRGB]);
    end;
    ReadGroupCode_71_76;
    vDS.TextPosVert := TsgDimTextPosVert(ReadBS(APBit));//DIMTAD
    SeekGeneral(APBit, [seBS,seBS]); //DIMZIN, DIMAZIN
    if Version >= acR2007 then
      SeekGeneral(APBit, [seBS]); //DIMARCSYM
    ReadGroupCode_140_147;
    SeekGeneral(APBit,[seBD,seB]); //DIMALTRND, DIMALT
    SeekGeneral(APBit, [seBS]); //DIMALTD
    ReadGroupCode_172_175;
    ReadGroupCode_176_178;
    SeekGeneral(APBit, [seBS]); //DIMADEC
    ReadGroupCode_271_274;
    // DIMAUNIT, DIMFRAC
    SeekGeneral(APBit, [seBS]);
    vDS.DIMFRAC := ReadBS(APBit);
    vDS.DIMLUNIT := GetDimLimitUnitsType(ReadBS(APBit));
    vDS.DIMDSEP := Char(ReadBS(APBit));
    SeekGeneral(APBit, [seBS,seBS]);// DIMTMOVE, DIMJUST
    ReadGroupCode_281_282;
    //DIMTOLJ, DIMTZIN, DIMALTZ, DIMALTTZ, DIMUPT, DIMFIT
    SeekGeneral(APBit, [seBS,seBS,seBS,seBS,seB,seBS]);
    if Version >= acR2007 then
    begin
      SeekGeneral(APBit, [seB]); //DIMFXLON
      if Version >= acR2010 then
      begin
        SeekGeneral(APBit, [seB, seBD]);//DIMTXTDIRECTION B 295;//DIMALTMZF BD ?
        ReadText;//DIMALTMZS T ?
        SeekGeneral(APBit, [seBD]);//DIMMZF BD ?
        ReadText;//DIMMZS T ?
      end;
    end;
    vDS.DIMLWD := ExpandLWeight(ReadBS(APBit));
    vDS.DIMLWE := ExpandLWeight(ReadBS(APBit));
  end;
  vDS.Flags := vDS.Flags or ReadBit(APBit); // code 70, bit 0
  ReadHandleRefs(APBit);
  FTextStyle := ReadID(APBit);
  if Version >= acR2000 then
  begin
    FArrows.Handle[vnDIMLRBLK] := ReadID(APBit);
    FArrows.Handle[vnDIMBLK] := ReadID(APBit);
    FArrows.Handle[vnDIMBLK1] := ReadID(APBit);
    FArrows.Handle[vnDIMBLK2] := ReadID(APBit);
  end;
  if Version >= acR2007 then
  begin
    ReadID(APBit);//345 dimltype (hard pointer)
    ReadID(APBit);//346 dimltex1 (hard pointer)
    ReadID(APBit);//347 dimltex2 (hard pointer)
  end;
end;

procedure TdwgDimStyle.References(AReader: TdwgReader);
var
  vDS: TsgDXFDimensionStyleAccess;

  function GetBlock(AType: TsgDimNameVal): TsgDXFEntity;
  begin
    Result := nil;
    case FArrows._Type[AType] of
      edtInt64: Result := AReader.ObjByHandle(FArrows.Handle[AType], TsgDXFBlockRecord);
      edtString:
        begin
          Result := AReader.Conv.Sections[csBlocks].FindEntByName(FArrows.Name[AType]);
          if not Assigned(Result) and (FArrows.Name[AType] <> '') then
          begin
            AReader.AddLink(vDS, DimBlkPropNames[AType], FArrows.Name[AType], TsgDXFDimensionStyleAccess);
            AReader.FArrows.Add(FArrows.Name[AType]);
          end;
        end;
    end;
    if Assigned(Result) then
      TsgDXFDimensionStyleAccess(FEntity).SetBlocks(AType, TsgDXFBlock(Result));
  end;

begin
  inherited References(AReader);
  vDS := TsgDXFDimensionStyleAccess(FEntity);
  if FTextStyle = cnstBadHandle then
    FTextStyle := Reader.FHeaderHandles.DIMTXTSTY;
  vDS.TextStyle := TsgDXFStyle(AReader.ObjByHandle(FTextStyle, TsgDXFStyle));
  if not Assigned(vDS.TextStyle) then
    vDS.TextStyle := Reader.Conv.StyleByName(AReader.Conv.HeadVarStruct.DimTextStyle);

  GetBlock(vnDIMBLK);
  if Version >= acR2000 then
    GetBlock(vnDIMLRBLK);
  GetBlock(vnDIMBLK1);
  GetBlock(vnDIMBLK2);
end;

{TdwgEntity}

{protected}

procedure TdwgEntity.ReadEntProxyData(var APBit: TsgInt64);
begin
  if ReadBit(APBit) <> 0 then
    DoReadEntProxyData(APBit);
end;

function TdwgEntity.IsEntity: Boolean;
begin
  Result := True;
end;

procedure TdwgEntity.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vPlot, vMaterial, vIsByLayer: Boolean;
  vNeedDbColorFlag: Integer;

  procedure ReadLL;
  begin
    FLayer := ReadID(APBit);
    if not vIsByLayer then
      FLType := ReadID(APBit);
  end;

begin
  ReadEntProxyData(APBit);
  if Version < acR2000 then
    BitSize := ReadRL(APBit);
  FMode := ReadBB(APBit);
  FReactorsCount := ReadBL(APBit);
  if Version < acR2000 then
    vIsByLayer := ReadBit(APBit) <> 0
  else
    vIsByLayer := False;
  FXDicHandlePresent := ReadBit(APBit) = 0;
  if Version >= acR2013 then
    Inc2(APBit, 1); //Unknown DWG 2013 flag
  FColor := FColorReader.ReadENC(@APBit);
  vNeedDbColorFlag := FColorReader.Flags;
  FLineTypeScale := ReadBD(APBit);
  vPlot := False;
  vMaterial := False;
  if Version >= acR2000 then
  begin
    FStandardLType := ReadBB(APBit);
    vIsByLayer := FStandardLType <> 3;
    if Version >= acR2007 then //ID Material
    begin
      vMaterial := ReadBB(APBit) = 3;
      SeekGeneral(APBit, [seRC]);
    end;
    vPlot := ReadBB(APBit) = 3;
  end;
  if Version > acR2007 then
  begin
    for I := 1 to 3 do
      if ReadBit(APBit) = 1 then //hasVstyle
        ReadID(APBit); //needs to check
  end;
  FVisibility := (ReadBS(APBit) <> 1);
  if Version >= acR2000 then
    ReadLineWeight(ReadRC(APBit));
  ReadEntData(APBit);
  if not FCorrectData then
    Exit;
  ReadHandleRefs(APBit);
  if Version < acR2000 then
    ReadLL;
  FSibling := FHandle + 1;
  if FXDicHandlePresent then
  begin
    if Version < acR2004 then
    begin
      ReadID(APBit);
      FSibling := ReadID(APBit);
    end;
  end;
  if vNeedDbColorFlag and $4000 <> 0 then
    FDbColor := ReadID(APBit);// (Version >= acR2004)
  if Version >= acR2000 then
    ReadLL;
  if vPlot then
    ReadID(APBit);
  if vMaterial then// (Version >= acR2007)
    FMaterialHandle := ReadID(APBit);
end;

function TdwgEntity.ReadElevation(var APBit: TsgInt64): Double;
begin
  if cnstLoadCad2D then
  begin
    Result := 0;
    SeekGeneral(APBit, [seBD]);
  end
  else
    Result := ReadBD(APBit);
end;

function TdwgEntity.ReadZThik(var APBit: TsgInt64): Double;
begin
  if cnstLoadCad2D then
  begin
    Result := 0;
    SeekGeneral(APBit, [seBD]);
  end
  else
    Result := ReadBD(APBit);
end;

procedure TdwgEntity.ReadEntData(var APBit: TsgInt64);
begin
end;

procedure TdwgEntity.DoReadEntProxyData(var APBit: TsgInt64);
var
  vSize: Integer;
begin
  // Skipping PROXY DATA
  if Version > acR2007 then
    vSize := ReadUInt64(APBit) shl 3
  else
    vSize := ReadRL(APBit) shl 3;
  Inc2(APBit, vSize);
end;

procedure TdwgEntity.ReadHandleRefs(var APBit: TsgInt64);
begin
  if FMode = 0 then
    FOwner := ReadID(APBit);
  SkipHandles(APBit);
end;

procedure TdwgEntity.ReadOwner(AReader: TdwgReader);
begin
  case FMode of
    0: FOwnerObject := AReader.ObjByHandle(FOwner, nil);
    1: FOwnerObject := AReader.Paper;
  else
    FOwnerObject := AReader.Model;
  end;
end;

procedure TdwgEntity.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  FEntity.LineWeight := FLineWeight;
  FEntity.Visibility := FVisibility;
  FEntity.LineTypeScale := FLineTypeScale;
  case FStandardLType of
    1: FEntity.LineType := AReader.LTypeByIndex(-2); // ByBlock
    2: FEntity.LineType := AReader.LTypeByIndex(0);  // continious
    3: FEntity.LineType := TsgDXFLineType(AReader.ObjByHandle(FLType, TsgDXFLineType));
  else
    if FLType <> cnstBadHandle then
      FEntity.LineType := TsgDXFLineType(AReader.ObjByHandle(FLType, TsgDXFLineType))
    else
      FEntity.LineType := AReader.LTypeByIndex(-1); // 0: 'ByLayer'
  end;
  FEntity.Layer := TsgDXFLayer(AReader.ObjByHandle(FLayer, TsgDXFLayer));
  ReferencesColor(AReader);
  AReader.AddMaterialLink(FMaterialHandle, FHandle);
end;

procedure TdwgEntity.ReferencesColor(AReader: TdwgReader);
var
  vDbColor: TsgDXFEntity;
begin
  vDbColor := AReader.ObjByHandle(FDbColor, nil);
  if Assigned(vDbColor) then
    FEntity.ColorCAD := vDbColor.ColorCAD
  else
    FEntity.ColorCAD := FColor;
end;

{TdwgLayer}

{protected}

constructor TdwgLayer.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FLineWeight := fLineWeightDefault;
  FIsPlotting := True;
end;

function TdwgLayer.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csLayers];
end;

procedure TdwgLayer.ReadData(var APBit: TsgInt64);
var
  vFlagsR2000Plus: Integer;
begin
  inherited ReadData(APBit);
  if Version >= acR2000 then
  begin
    vFlagsR2000Plus := ReadBS(APBit);
    FIsPlotting := vFlagsR2000Plus and $10 <> 0;
    ReadLineWeight(vFlagsR2000Plus and $3E0 shr 5);
    FFlags := FFlags or (vFlagsR2000Plus and $1); //contains frozen (1 bit)
    FFlags := FFlags or ((vFlagsR2000Plus and $4) shr 1 ); //rozen by default in new viewports (4 bit)
    FFlags := FFlags or ((vFlagsR2000Plus and $8) shr 1 ); //locked (8 bit)
    FVisible := vFlagsR2000Plus and $2 = 0;
  end
  else
  begin
    FFlags := FFlags or ReadBit(APBit); //if frozen (1 bit)
    FVisible := ReadBit(APBit) = 0; //if on.
    FFlags := FFlags or ReadBit(APBit) shl 1; //if frozen by default in new viewports (2 bit)
    FFlags := FFlags or ReadBit(APBit) shl 2; //if locked (4 bit)
  end;
  FColor := ReadColor(APBit);
  ReadHandleRefs(APBit);
  if Version >= acR2000 then
  begin
    ReadID(APBit);
    if Version >= acR2007 then
      FMaterialHandle := ReadID(APBit);
  end;
  FLType := ReadID(APBit);
end;

procedure TdwgLayer.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgDXFConverterAccess.SetLayerNameOfPassingInspection(TsgDXFLayer(FEntity), FEntryName);
  FEntity.LineWeight := FLineWeight;
  TsgDXFLayer(FEntity).IsPlotting := FIsPlotting;
  FEntity.Visible := FVisible;
  FEntity.ColorCAD := FColor;
  TsgDXFLayer(FEntity).Flags := FFlags;
  FEntity.LineType := TsgDXFLineType(AReader.ObjByHandle(FLType, TsgDXFLineType));
  AReader.AddMaterialLink(FMaterialHandle, FHandle);
end;

{ TdwgCustomControl }

{protected}

procedure TdwgCustomControl.ReadOwner(AReader: TdwgReader);
begin
  FOwnerObject := AReader.Conv.Sections[csTables];
end;

function TdwgCustomControl.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := AReader.Conv.Sections[csTables];
end;

procedure TdwgCustomControl.InitializeTable(ASection: TConvSection;
  AName: string);
var
  vEnt: TsgDXFEntity;
begin
  FEntity.Name := AName;
  if Assigned(Reader.Conv.Sections[ASection]) then
  begin
    vEnt := Reader.Conv.Sections[ASection];
    Reader.Conv.Sections[csTables].RemoveEntity(vEnt);
    Reader.Conv.Sections[ASection] := nil; // not need
    vEnt.Free;
    Reader.Conv.Sections[ASection] := TsgDXFGroup(FEntity);
  end;
end;

procedure TdwgCustomControl.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  FSubEnts := ReadBL(APBit);
  ReadHandleRefs(APBit);
  ReadSubEntIDs(APBit);
end;

procedure TdwgCustomControl.ReadSubEntIDs(var APBit: TsgInt64;
  const AFlags: Integer);
var
  I: Integer;
  vHandle: UInt64;
  vHCode: Byte;
begin
  if FSubEnts > 0 then
    ForceChildrenList(FSubEnts);
  for I := 0 to FSubEnts - 1 do
  begin
    vHandle := ReadID(APBit, vHCode);
    if (vHCode = cntDWGObjHandleSoftOwner) and (vHandle <> cnstBadHandle) then
      ChildrenAdd(vHandle);
  end;
end;

procedure TdwgCustomControl.ReadSubEnts(AReader: TdwgReader);
begin
  ReadSubEntsR2004(AReader);
end;

procedure TdwgCustomControl.References(AReader: TdwgReader);
begin
  inherited References(AReader);
end;

{TdwgLType}

{protected}

function TdwgLType.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csLTypes];
end;

procedure TdwgLType.ReadData(var APBit: TsgInt64);
const
  cnstAreaSizeMax = 512;
  cnstAreaSizeR18 = 256;
var
  I, vCount: Integer;
  vHasStrArea: Boolean;
  vAreaSize: Integer;
  vStrArea: array [0 .. cnstAreaSizeMax - 1] of Byte;
  S: string;
begin
  inherited ReadData(APBit);
  FEntity.Name := FEntryName;
  FEntity.Flags := FFlags;
  ReadText;
  SeekGeneral(APBit, [seBD,seRC]);
  vCount := ReadRC(APBit);
  vAreaSize := cnstAreaSizeR18;
  vHasStrArea := False;
  SetLength(FElements, vCount);
  SetLength(FStyles, vCount);
  for I := 0 to vCount - 1 do
  begin
    FElements[I].Text := '';
    FElements[I].Dash := ReadBD(APBit);
    FElements[I].ShapeNumber := ReadBS(APBit);
    FElements[I].XOffset := ReadRD(APBit);
    FElements[I].YOffset := ReadRD(APBit);
    FElements[I].Scale := ReadBD(APBit);
    FElements[I].Rotation := Degree(ReadBD(APBit));
    FElements[I].ComplexType := ReadBS(APBit);
    vHasStrArea := vHasStrArea or (FElements[I].ComplexType and $02 <> 0);
  end;
  if Version <= acR2004 then vHasStrArea := True;
  if vHasStrArea and (Version >= acR2007) then vAreaSize := cnstAreaSizeMax;
  if vHasStrArea then
  begin
    ReadBytesCustom(APBit, vStrArea, vAreaSize);
    if Version >= acR2007 then
      S := string(WideString(PWideChar(@vStrArea[0])))
    else
      S := string(AnsiString(PAnsiChar(@vStrArea[0])));
  end;
  ReadHandleRefs(APBit);
  for I := 0 to vCount - 1 do
  begin
    if FElements[I].ComplexType and 2 <> 0 then
    begin
      FElements[I].Text := S;
      FElements[I].ShapeNumber := 0;
    end;
    FStyles[I] := ReadID(APBit);
  end;
end;

procedure TdwgLType.References(AReader: TdwgReader);
var
  I: Integer;
begin
  inherited References(AReader);

  I := AReader.FObjMap.IndexOfHandle(AReader.FStyles.FHandle);
  if (I >= 0) and (AReader.FObjMap.MapElement[I].Obj = nil) then
  begin
    AReader.FTables.ReadPredefined(AReader.FObjMap.MapElement[I].Location, AReader.FStyles, I);
    AReader.FStyles.OwnerAdd(AReader.FStyles.GetOwner(AReader));
    AReader.FStyles.References(AReader);
    AReader.FStyles.PostReferences(AReader);
  end;
  for I := High(FElements) downto Low(FElements) do
  begin
    if FStyles[I] <> cnstBadHandle then
      FElements[I].Style := TdwgStyleControl(AReader.FStyles).QFind(FStyles[I]);
  end;
  TsgDXFLineType(FEntity).Lines.Append(FElements);
end;

{TdwgMLStyle}

{protected}

constructor TdwgMLStyle.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FLTypes := TsgInt64List.Create;
end;

destructor TdwgMLStyle.Destroy;
begin
  FLTypes.Free;
  inherited Destroy;
end;

function TdwgMLStyle.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csMLineStyles];
end;

procedure TdwgMLStyle.ReadData(var APBit: TsgInt64);
var
  vMLineStyle: TsgMLineStyle;
  vOffset: Double;
  vLines: Integer;
  vColor: TsgColorCAD;

  function DWGCodesToDXF(AFlags: SmallInt): SmallInt;
    procedure ChangeBit(Source: SmallInt; var Dest: SmallInt; si, di: Word);
    var
      val: Boolean;
    begin
      val := Word(Source) and si <> 0;
      if val then
        Dest := Dest or SmallInt(di)
      else
        Dest := Dest and not SmallInt(di);
    end;
  begin
    Result := AFlags;
    ChangeBit(AFlags, Result, 2, 1);
    ChangeBit(AFlags, Result, 1, 2);

    ChangeBit(AFlags, Result, 32, 64);
    ChangeBit(AFlags, Result, 64, 32);

    ChangeBit(AFlags, Result, 512, 1024);
    ChangeBit(AFlags, Result, 1024, 512);
  end;

begin
  ReadObjHeader(APBit);
  FEntity.Name := ReadText;
  ReadText;
  vMLineStyle := TsgMLineStyle(FEntity);
  vMLineStyle.Flags := DWGCodesToDXF(ReadBS(APBit));
  vMLineStyle.ColorCAD := ReadColor(APBit); //MakeColorCAD(acRGBColor, ReadCMC(Self, APBit));
  SeekGeneral(APBit,[seBD,seBD]);
  vLines := ReadRC(APBit);
  while vLines > 0 do
  begin
    vOffset := ReadBD(APBit);
    vColor := ReadColor(APBit);
    if Version < acR2018 then
      FLTypes.Add(ReadBS(APBit));
    vMLineStyle.NewEntry(vOffset, vColor, nil);
    Dec(vLines);
  end;
  ReadHandleRefs(APBit);
end;

procedure TdwgMLStyle.ReadHandleRefs(var APBit: TsgInt64);
var
  I: Integer;
begin
  inherited ReadHandleRefs(APBit);
  if Version >= acR2018 then
    for I := 0 to FEntity.Count - 1 do
      FLTypes.Add(ReadID(APBit));
end;

procedure TdwgMLStyle.References(AReader: TdwgReader);
var
  I, vIndex: Integer;
begin
  inherited References(AReader);
  if Version < acR2018 then
  begin
    for I := 0 to FEntity.Count - 1 do
    begin
      vIndex := FLTypes[I];
      case vIndex of
        $7FFF: vIndex := -1;
        $7FFE: vIndex := -2;
      end;
      Inc(vIndex, 2);
      if vIndex >= AReader.Conv.Sections[csLTypes].Count then
        vIndex := vIndex mod AReader.Conv.Sections[csLTypes].Count;
      if vIndex < AReader.Conv.Sections[csLTypes].Count then
        FEntity[I].LineType := AReader.LTypeByIndex(vIndex - 2)
      else
        FEntity[I].LineType := AReader.Conv.LTypeByName(sByLayer);
    end;
  end
  else
    for I := 0 to FLTypes.Count - 1 do
      FEntity[I].LineType := TsgDXFLineType(AReader.ObjByHandle(FLTypes[I], nil));
end;

{TdwgSortEntsTable}

{protected}

procedure TdwgSortEntsTable.ReadData(var APBit: TsgInt64);
var
  I, vNumEntries: Integer;
  vHCode: Byte;
begin
  ReadObjHeader(APBit);
  vNumEntries := ReadBL(APBit);
  for I := 0 to vNumEntries - 1 do
    TsgDXFSortEntsTable(FEntity).HandlesNew.Add(ReadHandle(APBit, vHCode, FHandle));
  ReadHandleRefs(APBit);
  FObject := ReadID(APBit, vHCode);
  for I := 0 to vNumEntries - 1 do
    TsgDXFSortEntsTable(FEntity).HandlesOld.Add(ReadID(APBit, vHCode));
end;

procedure TdwgSortEntsTable.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgDXFSortEntsTable(FEntity).SortObject := AReader.ObjByHandle(FObject, nil);
end;

{TdwgVPort}

{protected}

function TdwgVPort.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csVPorts];
end;

procedure TdwgVPort.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vVPort: TsgDXFVPort;
begin
  inherited ReadData(APBit);
  FEntity.Name := AnsiUpperCase(FEntryName);
  vVPort := TsgDXFVPort(FEntity);
  vVPort.ViewHeight := ReadBD(APBit);
  vVPort.ViewAspectRatio := ReadBD(APBit);
  if vVPort.ViewHeight <> 0 then
    vVPort.ViewAspectRatio := vVPort.ViewAspectRatio / vVPort.ViewHeight;
  vVPort.ViewCenterPoint := Read2RD(APBit);
  vVPort.ViewTarget := Read3D(APBit);
  vVPort.ViewDirection := _Read3D(APBit);
  vVPort.ViewTwistAngle := ReadBD(APBit) * f180DividedByPi;
  SeekGeneral(APBit,[se3D]);
  Inc2(APBit,4);
  if Version >= acR2000 then
    Inc2(APBit,8);
  if Version >= acR2007 then
  begin
    //ReadID(APBit);
    //ReadID(APBit);
    //Inc2(APBit, 9);
    //SeekGeneral(APBit, [seBD,seBD,seBS,seBL,seRC]);
    //ReadID(APBit);
    SeekGeneral(APBit, [seB, seRC, seBD, seBD]);
    ReadColor(APBit);
  end;
  SeekGeneral(APBit,[se2RD,se2RD]);
  Inc2(APBit,1);
  vVPort.CircleZoomPercent := ReadBS(APBit);
  Inc2(APBit,4);
  SeekGeneral(APBit,[se2RD]);
  Inc2(APBit,2);
  SeekGeneral(APBit, [seBS,seBD,se2RD,se2RD]);
  if Version >= acR2000 then
  begin
    Inc2(APBit,1);
    vVPort.UCSVP := ReadBit(APBit) <> 0;
    vVPort.UCSOrigin := _Read3D(APBit);
    vVPort.UCSXDir := _Read3D(APBit);
    vVPort.UCSYDir := _Read3D(APBit);
    SeekGeneral(APBit, [seBD, seBS]);
    if Version >= acR2007 then
      SeekGeneral(APBit, [seBS, seBS]);
  end;
  ReadHandleRefs(APBit);
//
//  R2007+:
//Background handle H 332 soft pointer
//Visual Style handle H 348 hard pointer
//Sun handle H 361 hard owner
  if Version >= acR2007 then
    for I := 0 to 2 do ReadID(APBit);
//R2000+:
//Named UCS Handle H 345 hard pointer
//Base UCS Handle H 346 hard pointer
  if Version >= acR2000 then
    for I := 0 to 1 do ReadID(APBit);
end;

procedure TdwgVPort.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  if (AReader.FActiveViewport = cnstBadHandle) and sgSameText(FEntryName, sActiveVPort) then
    AReader.FActiveViewport := FHandle;
end;

{ TdwgWipeoutVar }

{protected}

function TdwgWipeoutVar.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csObjects];
end;

procedure TdwgWipeoutVar.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  TsgCADWipeoutVariables(FEntity).DisplayImageFrame := ReadBL(APBit) <> 0;
  ReadHandleRefs(APBit);
end;

{Tdwg3DFace}

{protected}

procedure Tdwg3DFace.ReadEntData(var APBit: TsgInt64);
  function ReadDP(const DP: TFPoint): TFPoint;
  begin
    Result.X := ReadDD(APBit, DP.X);
    Result.Y := ReadDD(APBit, DP.Y);
    Result.Z := ReadDD(APBit, DP.Z);
  end;
var
  v3dFace: TsgDXF3dFace;
  vPoint, vPoint1, vPoint2, vPoint3: TFPoint;
  vHasF,vZ0: Boolean;
begin
  v3dFace := TsgDXF3dFace(FEntity);
  if Version < acR2000 then
  begin
    vPoint := Read3D(APBit);
    vPoint1 := Read3D(APBit);
    vPoint2 := Read3D(APBit);
    vPoint3 := Read3D(APBit);
    vHasF := True;
  end
  else
  begin
    vHasF := ReadBit(APBit) = 0;
    vZ0 := ReadBit(APBit) <> 0;
    vPoint := Read2RD(APBit);
    if not vZ0 then
      vPoint.Z := ReadRD(APBit);
    vPoint := vPoint;
    vPoint1 := ReadDP(vPoint);
    vPoint2 := ReadDP(vPoint1);
    vPoint3 := ReadDP(vPoint2);
    if cnstLoadCad2D then
    begin
      vPoint.Z := 0;
      vPoint1.Z := 0;
      vPoint2.Z := 0;
      vPoint3.Z := 0;
    end;
  end;
  v3dFace.Point := vPoint;
  v3dFace.Point1 := vPoint1;
  v3dFace.Point2 := vPoint2;
  v3dFace.Point3 := vPoint3;
  if vHasF then
    v3dFace.Flags := ReadBS(APBit);
end;

{TdwgBlock}

{protected}

function TdwgBlock.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
{$IFDEF SG_BTI}
var
  vGroupName: string;
  vEntTypeNum: Integer;
  vEntity: TsgDXFEntity;
{$ENDIF}
begin
  Result := inherited ApplyEED(AEEData, AAppID);
{$IFDEF SG_BTI}
  if not Result and Reader.IsRelatedAppID(AAppID) and (Reader.XDataProcs <> nil) and Reader.FSGSaved then
  begin
    TsgDXFEntityAccess.ExtractEEDTypeEx(AEEData, vGroupName, vEntTypeNum);
    if vEntTypeNum <> cnstUndefined then
    begin
      vEntity := Reader.XDataProcs.Import(vEntTypeNum, nil);
      if Assigned(vEntity) then
      begin
        TsgDXFBlock(vEntity).Clone(FEntity);
        TsgDXFBlock(vEntity).BlockRecord.Clone(FBlockRecord.FEntity);
        FEntity.Free;
        FEntity := vEntity;
        FBlockRecord.FEntity := TsgDXFBlock(vEntity).BlockRecord;
        TsgDXFEntityAccess(FEntity).SetConverter(Reader.Conv);
        FEntity.SetExtData(AEEData);

        Reader.FObjMap.Objects[FMapIndex] := FEntity;
        Reader.FObjMap.Objects[FBlockRecord.FMapIndex] := FBlockRecord.FEntity;
        Result := True;
      end;
    end;
  end;
{$ENDIF}
end;

function TdwgBlock.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := AReader.Conv.Sections[csBlocks];
end;

function TdwgBlock.OwnerAdd(AOwner: TsgDXFEntity): Integer;
var
  vEnt: TsgDXFEntity;
begin
  if not Assigned(FOwnerObject) then
    Result := -1
  else
  begin
    Result := -1;
    if Assigned(FEntity) then
    begin
      if FBlockRecordNoLocation then
        Result := AOwner.AddEntity(FEntity)
      else
      begin
        Result := AOwner.IndexOfEntity(FEntity);
        if Result < 0 then
        begin
          vEnt := AOwner.FindEntByName(FEntity.Name);
          if not Assigned(vEnt) then
            Result := AOwner.AddEntity(FEntity)
          else
            Result := AOwner.IndexOfEntity(vEnt);
        end;
      end;
    end;
  end;
end;

procedure TdwgBlock.ReadEntData(var APBit: TsgInt64);
begin
  FEntryName := ReadText;
end;

procedure TdwgBlock.ReadOwner(AReader: TdwgReader);
var
  I: Integer;
begin
  // ------------------------- fix -------------------------
  // "FOwner" - blockrecord handle not exixts in ObjMap,
  // but entities has reference to.
  // Add new map element to closure all referential entities
  if (FMode = 0) and (FOwner < AReader.FMaxHandle) then
  begin
    I := AReader.FObjMap.IndexOfHandle(FOwner);
    if I < 0 then
    begin
      FOwnerObject := TsgDXFBlockAccess(FEntity).BlockRecord;
      FOwnerObject.Name := FEntryName;
      FOwnerObject.Handle := FOwner;
      AReader.FObjMap.AddDwgElement(FOwner, -1, FOwnerObject);
      FBlockRecordNoLocation := True;
    end;
  end;
  // ------------------------- fix -------------------------
  inherited ReadOwner(AReader);
end;

{TdwgCircle}

{protected}

procedure TdwgCircle.ReadEntData(var APBit: TsgInt64);
var
  vCircle: TsgDXFCircle;
begin
  vCircle := TsgDXFCircle(FEntity);
  vCircle.Point := Read3D(APBit);
  vCircle.Radius := ReadBD(APBit);
  vCircle.ZThick := ReadZThik(APBit);
  vCircle.Extrusion := ReadExtrusion(APBit);
end;

{TdwgEllipse}

{protected}

procedure TdwgEllipse.ReadEntData(var APBit: TsgInt64);
var
  vEllipse: TsgDXFEllipse;
begin
  vEllipse := TsgDXFEllipse(FEntity);
  vEllipse.Point := Read3D(APBit);
  vEllipse.EndPoint := Read3D(APBit);
  vEllipse.Extrusion := ReadExtrusion(APBit);
  vEllipse.Ratio := ReadBD(APBit);
  vEllipse.StartAngle := ReadBD(APBit) * f180DividedByPi;
  vEllipse.EndAngle := ReadBD(APBit) * f180DividedByPi;
end;

function TdwgEllipse.ReadExtrusion(var APBit: TsgInt64): TFPoint;
begin
  Result := Read3D(APBit);
  if IsEqualFPoints(Result, cnstFPointZero) then
    Result.Z := 1;
end;

{TdwgHatch}

{private}

procedure TdwgHatch.AddBoundaryDataCurve(const ABoundaryList: Tsg2DBoundaryList;
  const ACurve: Tsg2DCurve);
begin
  if ACurve <> nil then
  begin
    ABoundaryList.Add(ACurve)
  end;
end;

function TdwgHatch.Read2DArc(const ACircle: Boolean;
  var APBit: TsgInt64): Tsg2DArc;
begin
  if ACircle then
  begin
    Result := Tsg2DArc.Create;
    Result.CenterPoint := ReadF2DPoint(APBit);
  end
  else
  begin
    Tsg2DEllipse(Result) := Tsg2DEllipse.Create;
    Tsg2DEllipse(Result).CenterPoint := ReadF2DPoint(APBit);
    Tsg2DEllipse(Result).MajorPoint := ReadF2DPoint(APBit);
    Tsg2DEllipse(Result).IsAngleInParam := False;
  end;
  Result.Radius := ReadBD(APBit);
  Result.StartParam := ReadBD(APBit) * f180DividedByPi;
  Result.EndParam := ReadBD(APBit) * f180DividedByPi;
  Result.CounterClockWise := ReadBit(APBit) <> 0;
end;

function TdwgHatch.Read2DLine(var APBit: TsgInt64): Tsg2DLine;
begin
  Result := Tsg2DLine.Create;
  Result.SetStartPoint(ReadF2DPoint(APBit));
  Result.SetEndPoint(ReadF2DPoint(APBit));
end;

function TdwgHatch.Read2DPolyline(var APBit: TsgInt64; AIsMPolygon: Boolean): Tsg2DPolyline;
var
  I, vNumVectexes: Integer;
  vBulges: Boolean;
begin
  Result := Tsg2DPolyline.Create;
  if AIsMPolygon then
  begin
    Result.Closed := ReadBit(APBit) <> 0;
    vBulges := ReadBit(APBit) <> 0;
  end
  else
  begin
    vBulges := ReadBit(APBit) <> 0;
    Result.Closed := ReadBit(APBit) <> 0;
  end;
  vNumVectexes := ReadBL(APBit);
  for I := 1 to vNumVectexes do
  begin
    Result.AddVertex(ReadF2DPoint(APBit));
    if vBulges then
      Result.AddBulge(ReadBD(APBit));
  end;
end;

function TdwgHatch.Read2DSpline(var APBit: TsgInt64): Tsg2DSpline;
var
  I, vNumKnots, vNumControlPoints: Integer;
  vFitPtsNum: Integer;
begin
  Result := Tsg2DSpline.Create;
  Result.Degree := ReadBL(APBit); // degree of the Read2DSpline
  Result.Rational := ReadBit(APBit) <> 0; //1 if rational (has weights), else 0
  Result.Periodic :=  ReadBit(APBit) <> 0;  //1 if periodic, else 0
  vNumKnots := ReadBL(APBit);
  vNumControlPoints := ReadBL(APBit);
  for I := 0 to vNumKnots-1 do
    Result.AddKnot(ReadBD(APBit));
  for I := 0 to vNumControlPoints-1 do
  begin
    Result.AddControl(ReadF2DPoint(APBit));
    if Result.Rational then
      Result.AddWeight(ReadBD(APBit));
  end;
  if Version > acR2007 then
  begin
    vFitPtsNum := ReadBL(APBit);//new feature builds curve by fit points
    if vFitPtsNum > 0 then //this code needs to be checked
    begin
      for I := 0 to vFitPtsNum - 1 do
        SeekGeneral(APBit,[se2RD]);
      SeekGeneral(APBit,[se2RD,se2RD]);
    end;
  end;
end;

procedure TdwgHatch.ReadGradietParams(var APBit: TsgInt64);
var
  vCurvePolygon: TsgCADCurvePolygon;
  vGrad: Boolean;
  I, vNumColor: Integer;
begin
  vCurvePolygon := TsgCADCurvePolygon(FEntity);
  vGrad := ReadBL(APBit) <> 0;
  SeekGeneral(APBit,[seBL]);
  vCurvePolygon.GradientAngle := RadToDeg(ReadBD(APBit));
  vCurvePolygon.GradientUseCenter :=  Abs(ReadBD(APBit)) < fAccuracy;
  // | seBL - Gradient one color mode Boolean
  // v seBD - Luminance value
  SeekGeneral(APBit,[seBL,seBD]);
  vNumColor := ReadBL(APBit);//Number of colors
  for I := 1 to vNumColor do
  begin
    SeekGeneral(APBit,[seBD]);
    case I of
      1, 2: vCurvePolygon.GradientColorCAD[I-1] := ReadColor(APBit);
    else
      ReadColor(APBit);
    end;
  end;
  vCurvePolygon.GradientName := ReadText;
  if vGrad then
    FColor := vCurvePolygon.ColorCAD
  else
    vCurvePolygon.GradientName := '';
end;

procedure TdwgHatch.ReadPatternData(var APBit: TsgInt64);
var
  vHatch: TsgCADHatchAccess;
  I, J, vNumPatterns, vDashNum: Integer;
  vPData: PsgHatchPatternData;
begin
  vHatch := TsgCADHatchAccess(FEntity);
  vHatch.PatternData.Angle := Degree(ReadBD(APBit));
  vHatch.PatternData.Scale := ReadBD(APBit);
  vHatch.DoubleHatch := ReadBit(APBit) = 1;//is doublehatch
  vNumPatterns := ReadBS(APBit);
  for I := 1 to vNumPatterns do
  begin
    New(vPData);
    vHatch.HatchPatternData.Add(vPData);
    vPData.IsDash := False;
    vPData.Lines := nil;
    vPData.LineAngle := ReadBD(APBit) * f180DividedByPi;
    vPData.BaseP := Read2D(APBit);
    vPData.Offset := Read2D(APBit);
    vDashNum := ReadBS(APBit);
    vPData.DashNum := vDashNum;
    if vDashNum = 0 then
      Continue;
    vPData.IsDash := True;
    vPData.Lines := TsgDoubleList.Create(vDashNum);
    for J := 0 to vDashNum - 1 do
      vPData.Lines[J] := ReadBD(APBit);
  end;
end;

{protected}

function TdwgHatch.DoSolidHatch(const AIsSolid: Boolean): TsgCADCurvePolygon;
begin
  Result := TsgCADCurvePolygon(FEntity);
  if AIsSolid then
  begin
    Result := TsgCADCurvePolygon.Create;
    try
      Result.Clone(TsgCADCurvePolygon(FEntity));
      //AReader.FConv.RemoveEntity(FEnt, True);
      //if Assigned(AReader.FConv.OnCreate) then AReader.FConv.OnCreate(P);
      FEntity.Free;
    finally
      FEntity := Result;
    end;
  end
end;

procedure TdwgHatch.ReadAssociative(var APBit: TsgInt64);
begin
  Inc2(APBit,1);
end;

function TdwgHatch.ReadBoundaryData(var APBit: TsgInt64): Boolean;
var
  vCurvePolygon: TsgCADCurvePolygon;
  I, J, vNumPaths, vNumPathSegments, vPathFlag: Integer;
  vCurve: Tsg2DCurve;
  vBoundaryList: Tsg2DBoundaryList;
begin
  Result := False;
  vCurvePolygon := TsgCADCurvePolygon(FEntity);
  vNumPaths := ReadBL(APBit);
  for I := 1 to vNumPaths do
  begin
    vPathFlag := ReadBL(APBit);
    vBoundaryList := vCurvePolygon.AddBoundaryList(vPathFlag);
    vBoundaryList.BoundaryType := vPathFlag;
    Result := Result or (vPathFlag and 4 <> 0);
    if vPathFlag and 2 = 0 then
    begin
      vNumPathSegments := ReadBL(APBit);
      for J := 0 to vNumPathSegments - 1 do
      begin
        case ReadRC(APBit) of
          1: vCurve := Read2DLine(APBit);
          2: vCurve := Read2DArc(True, APBit);
          3: vCurve := Read2DArc(False, APBit);
          4: vCurve := Read2DSpline(APBit);
        else
          vCurve := nil;
        end;
        AddBoundaryDataCurve(vBoundaryList, vCurve);
      end;
    end
    else
      AddBoundaryDataCurve(vBoundaryList, Read2DPolyline(APBit,
        vCurvePolygon.EntType = ceMPolygon));
    SeekGeneral(APBit,[seBL]);
  end;
end;
                        
procedure TdwgHatch.ReadEntData(var APBit: TsgInt64);
var
  I, vNumSeedPoints: Integer;
  vCurvePolygon: TsgCADCurvePolygon;
  vPixSize: Boolean;
begin
  vCurvePolygon := TsgCADCurvePolygon(FEntity);
  if Version >= acR2004 then
    ReadGradietParams(APBit);
//  if Version >= acR2007 then
//    vCurvePolygon.HatchName := ReadText;
  vCurvePolygon.Elevation := MakeFPoint(0, 0, ReadElevation(APBit));
  vCurvePolygon.Extrusion := ReadExtrusion(APBit);
//  if Version < acR2007 then
    vCurvePolygon.HatchName := ReadText;
  vCurvePolygon := DoSolidHatch(ReadBit(APBit) <> 0);
  ReadAssociative(APBit);
  vPixSize := ReadBoundaryData(APBit);
  ReadStyleAndPatternType(APBit);
  if not vCurvePolygon.SolidFill then
    ReadPatternData(APBit);
  if vPixSize then
    SeekGeneral(APBit,[seBD]);
  ReadFillColorAndOffset(APBit);
  vNumSeedPoints := ReadBL(APBit);
  for I := 1 to vNumSeedPoints do
    SeekGeneral(APBit,[se2RD]);
  if TsgDWGConverter(FReader.Conv).ApplyCADHatchSettings(vCurvePolygon) then
    DoSolidHatch(True);
end;

procedure TdwgHatch.ReadFillColorAndOffset(var APBit: TsgInt64);
begin
end;

procedure TdwgHatch.ReadStyleAndPatternType(var APBit: TsgInt64);
begin
  SeekGeneral(APBit, [seBS, seBS]);
end;

procedure TdwgHatch.ReferencesColor(AReader: TdwgReader);
var
  vcmColor: Integer;
  vRTTcAlStarted: Boolean;
  I: Integer;
begin
  if Assigned(FACADExtData) then
  begin
    I := 0;
    vcmColor := 0;
    vRTTcAlStarted := False;
    while (I < FACADExtData.DataCount) and (vcmColor = 0) do
    begin
      case FACADExtData.DataType[I] of
        edtString:
          if (FACADExtData.DataCode[I] = 1000) and (FACADExtData.DataString[I] = 'RTTcAl') then
            vRTTcAlStarted := True;
        edtInt:
          if vRTTcAlStarted and (FACADExtData.DataType[I] = edtInt) and (FACADExtData.DataCode[I] = 1071) then
            vcmColor := FACADExtData.DataInt[I];
      end;
      Inc(I);
    end;
    if vcmColor <> 0 then
      FColor := CmEntityColorToColorCAD(Cardinal(vcmColor));
  end;
  inherited ReferencesColor(AReader);
end;

procedure TdwgHatch.SetTransparency(AValue: Integer);
begin
  if AValue and $02000000 <> 0 then
    TsgCADCurvePolygon(FEntity).Transparency := (100 * (255 - AValue and $FF)) div 255
  else
    if AValue = $01000000 then
      TsgCADCurvePolygon(FEntity).Transparency := fTransparencyByBlock;
end;

{ TdwgMPolygon }

{protected}

function TdwgMPolygon.DoSolidHatch(const AIsSolid: Boolean): TsgCADCurvePolygon;
begin
  Result := TsgCADCurvePolygon(FEntity);
  Result.SolidFill := AIsSolid;
end;

procedure TdwgMPolygon.ReadAssociative(var APBit: TsgInt64);
begin
end;

function TdwgMPolygon.ReadBoundaryData(var APBit: TsgInt64): Boolean;
var
  vMPolygon: TsgCADMPolygon;
  I, vNumPaths: Integer;
  vBoundaryList: Tsg2DBoundaryList;
begin
  Result := False;
  vMPolygon := TsgCADMPolygon(FEntity);
  vNumPaths := ReadBL(APBit);
  for I := 1 to vNumPaths do
  begin
    vBoundaryList := vMPolygon.AddBoundaryList(7);
    AddBoundaryDataCurve(vBoundaryList, Read2DPolyline(APBit, True));
  end;
end;

procedure TdwgMPolygon.ReadEntData(var APBit: TsgInt64);
begin
  SeekGeneral(APBit, [seBL]);
  inherited ReadEntData(APBit);
end;

procedure TdwgMPolygon.ReadFillColorAndOffset(var APBit: TsgInt64);
begin
  TsgCADMPolygon(FEntity).FillColor := ReadColor(APBit);
  TsgCADMPolygon(FEntity).Offset := Read2RD(APBit);
end;

procedure TdwgMPolygon.ReadStyleAndPatternType(var APBit: TsgInt64);
begin
  SeekGeneral(APBit, [seBS]);
end;

{TdwgImageDef}

{protected}

function TdwgImageDef.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csImageDefs];
end;

procedure TdwgImageDef.ReadData(var APBit: TsgInt64);
var
  vClsVer: Integer;
  vCode: Integer;
begin
  ReadObjHeader(APBit);
  vClsVer := ReadBL(APBit);
  if (vClsVer >= 500) and (FReader.VarCode(vClsVer) = 55) then
  begin
    Inc2(APBit, 103); //??
    vCode := 1;
    while Comp2(APBit, FDataEnd) and (vCode <> 0) do
    begin
      vCode := ReadBS(APBit);
      case vCode of
        1:  TsgDXFImageDef(FEntity).FileName := ReadText;
        10: SeekGeneral(APBit, [se3D]); // Image size in pixels
        11: SeekGeneral(APBit, [se3D]); // Default size of one pixel in AutoCAD units
        90: SeekGeneral(APBit, [seBL]); // Class version 0
        100:
          begin
            ReadText; // 'cn:AcDbRasterImageDef'
            ReadText; // 'AcDbRasterImageDef'
          end;
        280: SeekGeneral(APBit, [seBS]); // Image-is-loaded flag. 0 = Unloaded; 1 = Loaded
        281: SeekGeneral(APBit, [seBS]); // Resolution units. 0 = No units; 2 = Centimeters; 5 = Inch
      end;
    end
  end
  else
  begin
    SeekGeneral(APBit, [se2RD]);
    TsgDXFImageDef(FEntity).FileName := ReadText;
    SeekGeneral(APBit, [seB, seRC, se2RD]);
  end;
  ReadHandleRefs(APBit);
  if FEntity.Name = '' then
    FEntity.Name := FEntity.EntName;
end;

{TdwgInsert}

{protected}

function TdwgInsert.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  vEntity: TsgDXFEntity;
  vEntTypeNum: Integer;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) then
  begin
    if Reader.IsRelatedAppID(AAppID) and Reader.ConvExtDataToDXF(AEEData) and Reader.FSGSaved then
    begin
      vEntity := FEntity;
      ChangeEntEx(AEEData, vEntTypeNum);
      Result := vEntity <> FEntity;
{$IFDEF SG_BTI}
      if not Result and (Reader.XDataProcs <> nil) and (vEntTypeNum <> cnstUndefined) then
      begin
        vEntity := Reader.XDataProcs.Import(vEntTypeNum, nil);
        if Assigned(vEntity) then
        begin
          TsgDXFInsert(vEntity).Block.Free;
          vEntity.Clone(FEntity);
          TsgDXFInsert(vEntity).BlockRecord := TsgDXFInsert(FEntity).BlockRecord;
          FEntity.Free;
          FEntity := vEntity;
          TsgDXFEntityAccess(FEntity).SetConverter(Reader.Conv);
          FEntity.SetExtData(AEEData);
          Reader.FObjMap.Objects[FMapIndex] := FEntity;
        end;
        case FEntity.EntClass.EG of
          gtBTI:
            begin
              if TsgDXFEntityAccess(FEntity).GetEntTypeEx <> cnstLabel then
                Reader.XDataProcs.AddEntity(FEntity);
            end;
        end;
      end;
{$ENDIF}
    end;
  end;
end;

function TdwgInsert.ChangeEntEx(AExtData: TsgCADExtendedData; var AEntTypeNum: Integer): string;
var
  vEntity: TsgDXFEntity;
begin
  Result := '';
  TsgDXFEntityAccess.ExtractEEDTypeEx(AExtData, Result, AEntTypeNum);
  if (Result <> '') and (AEntTypeNum <> cnstUndefined) then
  begin
    vEntity := DXFConv.CreateRegEntity(Result, AEntTypeNum, Reader.Conv, AExtData);
    if vEntity <> nil then
    begin
      Reader.Conv.DeleteBlock(TsgDXFInsert(vEntity).Block, False);
      TsgDXFInsert(vEntity).Block.Free;
      TsgDXFInsert(vEntity).Clone(FEntity);
      TsgDXFInsert(vEntity).Block := TsgDXFInsert(FEntity).Block;
      FEntity.Free;
      FEntity := vEntity;
    end
    else
      Result := '';
  end;
end;

procedure TdwgInsert.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FBlockRecord := ReadID(APBit);
  if FComplex then
  begin
    ReadSubEntIDs(APBit);// ReadLimits(APBit,Self);
    ReadID(APBit); //[SEQEND (hard owner)] if 66 bit set
  end;
end;

procedure TdwgInsert.ReadEntData(var APBit: TsgInt64);
var
  vInsert: TsgDXFInsert;
  vScale: TFPoint;
  vDataFlag: Byte;
begin
  vInsert := TsgDXFInsert(FEntity);
  vInsert.Point := Read3D(APBit);
  if Version < acR2000 then
    vScale := _Read3D(APBit) // X Y Z scale
  else
  begin
    vDataFlag := ReadBB(APBit);
    if vDataFlag and 1 <> 0 then
      vScale.X := 1.0
    else
      vScale.X := ReadRD(APBit);
    vScale.Y := vScale.X; vScale.Z := vScale.X;
    if vDataFlag < 2 then
    begin
      vScale.Y := ReadDD(APBit, vScale.X);
      vScale.Z := ReadDD(APBit, vScale.X);
    end;
  end;
  vInsert.Scale := vScale;
  vInsert.Angle := ReadBD(APBit) * f180DividedByPi;
  vInsert.Extrusion := ReadExtrusion(APBit);
  FComplex := ReadBit(APBit) <> 0;
  if FComplex and (Version >= acR2004) then
    FSubEnts := ReadBL(APBit);
end;

procedure TdwgInsert.References(AReader: TdwgReader);
begin
  inherited References(Reader);
  TsgDXFInsert(FEntity).BlockRecord :=
    TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecord, TsgDXFBlockRecord));
end;

{TdwgLayout}

{protected}

procedure TdwgLayout.ReadData(var APBit: TsgInt64);
var
  I, vViewPortCount: Integer;
  vLayout: TsgDXFLayout;
  vPlotSettingsData : PsgPlotSettingsData;
begin
  vLayout := TsgDXFLayout(FEntity);
  vPlotSettingsData := TsgDXFPlotSettingsAccess(vLayout.PlotSettings).PlotData;
  ReadObjHeader(APBit);
  //PlotSettings
  Reader.ReadPlotSettings(Self, vPlotSettingsData, APBit);
  vLayout.Name := ReadText;
  SeekGeneral(APBit,[seBS,seBS,se3D,se2RD,se2RD,se3D,se3D,se3D,
    seBD,seBS,se3D,se3D]);
  if Version >= acR2004 then
    vViewPortCount := ReadBS(APBit) // RL??
  else
    vViewPortCount := 0;
// Handle refs:
  // ParentHandle (soft pointer)
  // reactors (soft pointer)
  ReadHandleRefs(APBit);
  if Version >= acR2004 then
  begin
    //plot view handle (hard pointer)
    ReadID(APBit);
    //Visual Style handle (soft pointer)
    if Version >= acR2007 then
      ReadID(APBit);
  end;
  //330 associated paperspace block record handle (soft pointer)
  FPaperSpaceHandle := ReadID(APBit);
  FActiveViewport := ReadID(APBit);//331 last active viewport handle (soft pointer)
  //346 base ucs handle (hard pointer)
  //345 named ucs handle (hard pointer)
  for I := 0 to 1 do
    ReadID(APBit);
  //Viewport handle (repeats Viewport count times) (soft pointer)
  if (Version >= acR2004) and (vViewPortCount > 0) then
  begin
    FViewports := TsgInt64List.Create(0, vViewPortCount);
    for I := 0 to vViewPortCount - 1 do
      FViewports.Add(ReadID(APBit));
  end;
// --------- old code ---------------
//  if Version >= acR2007 then
//  begin
//    repeat
//      FPaperSpaceHandle := ReadID(APBit)
//    until (FPaperSpaceHandle = 0) and (vHCode = 4);
//    FPaperSpaceHandle := ReadID(APBit)
//  end
//  else
//  begin
//    repeat
//      FPaperSpaceHandle := ReadID(APBit)
//    until (FPaperSpaceHandle = 0) or (vHCode = 3) or (vHCode = 5);
//    repeat
//      FPaperSpaceHandle := ReadID(APBit)
//    until vHCode = 4;
//  end;
// --------- old code ---------------
end;

procedure TdwgLayout.References(AReader: TdwgReader);
var
  E: TsgDXFEntity;
  vPaperBlockRec: TsgDXFBlockRecordAccess absolute E;
begin
  inherited References(AReader);
  if FPaperSpaceHandle = AReader.FPaper.BlockRecord.Handle then
    E := AReader.FPaper.BlockRecord
  else
    if FPaperSpaceHandle = AReader.FModel.BlockRecord.Handle then
    begin
      E := AReader.FModel.BlockRecord;
      if FActiveViewport <> cnstBadHandle then
        AReader.FActiveViewport := FActiveViewport;
    end
    else
      E := AReader.ObjByHandle(FPaperSpaceHandle, TsgDXFBlockRecord);
  if not Assigned(vPaperBlockRec.Block.Layout) then//dublicate FPaperSpaceHandle
  begin
     TsgDXFLayout(FEntity).PaperSpaceBlock := vPaperBlockRec.Block;
     if Assigned(FViewports) then
       AReader.FViewports.Add(FPaperSpaceHandle, FViewports);
  end;
end;

{TdwgLeader}

{protected}

function TdwgLeader.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I: Integer;
  vCode: Integer;
  vFlag: Boolean;
  vLeader: TsgDXFLeaderAccess;
begin
  Result := False;
  if not FCanApplyEED then Exit;
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) and Reader.IsACADAppID(AAppID) then
  begin
    vLeader := TsgDXFLeaderAccess(FEntity);
    I := 0;
    vFlag := False;
    vCode := 0;
    while I < AEEData.DataCount do
    begin
      case AEEData.DataType[I] of
        edtString:
          if vFlag then
          begin
            case vCode of
              5: FArrows.Name[vnDIMBLK] := AEEData.DataString[I];
              6: FArrows.Name[vnDIMBLK1] := AEEData.DataString[I];
              7: FArrows.Name[vnDIMBLK2] := AEEData.DataString[I];
            end;
          end;
        edtDouble:
          if vFlag then
          begin
            case (*AEEData.DataCode[I - 1]*) vCode of
              40:
                begin
                  Include(FOverrides, vnDIMSCALE);
                  FArrowScaleOverrides := AEEData.DataDouble[I];
                end;
              41:
                begin
                  Include(FOverrides, vnDIMASZ);
                  FArrowSizeOverrides := AEEData.DataDouble[I];
                end;
              147: FGAP := AEEData.DataDouble[I];
            end;
          end;
        edtInt64:
          case vCode of
            340: vLeader.TextStyle := TsgDXFStyle(Reader.ObjByHandle(AEEData.DataInt64[I], TsgDXFStyle));
            341: FArrows.Handle[vnDIMLRBLK] := AEEData.DataInt64[I];
            342: FArrows.Handle[vnDIMBLK] := AEEData.DataInt64[I];
            343: FArrows.Handle[vnDIMBLK1] := AEEData.DataInt64[I];
            344: FArrows.Handle[vnDIMBLK2] := AEEData.DataInt64[I];
          end;
        edtInt16:
        begin
          if vFlag then
          begin
            case (*AEEData.DataInt16[I - 1]*) vCode of
              75:
                begin
                  if not Assigned(FHookLine) then
                    New(FHookLine);
                  FHookLine^ := AEEData.DataInt16[I];
                end;
              77: FTAD := AEEData.DataInt16[I];
            end;
          end;
          case AEEData.DataCode[I] of
            1070: vCode := AEEData.DataInt16[I];
          end;
        end;
        edtByte:
          case AEEData.DataCode[I] of
            1002: begin vFlag := not vFlag; end;
          end;
      end;
      Inc(I);
    end;
    Result := True;
  end;
end;

constructor TdwgLeader.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FArrows := TdwgArrows.Create;
  FHooklineVector := cnstXOrtAxis;
  //FArrowHeadType := -1;
  FTAD := -1;
  FGAP := 0;
  FAnnotationType := 3;
  FArrowSize := 0;
  FArrowScaleOverrides := 0;
  FCanApplyEED := False;
end;

destructor TdwgLeader.Destroy;
begin
  FArrows.Free;
  if FHookLine <> nil then
    Dispose(FHookLine);
  inherited Destroy;
end;

procedure TdwgLeader.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FAssociatedAnnotation := ReadID(APBit);
  FDimStyle := ReadID(APBit);
end;

procedure TdwgLeader.ReadEntData(var APBit: TsgInt64);
var
  vLeader: TsgDXFLeader;
  vNumPoints: Integer;
begin
  vLeader := TsgDXFLeader(FEntity);
  Inc2(APBit,1);
  FAnnotationType := ReadBS(APBit);
  vLeader.IsSpline := ReadBS(APBit) = 1; //path type
  vNumPoints := ReadBL(APBit);
  while vNumPoints > 0 do
  begin
    vLeader.Controls.Add(Read3D(APBit));
    Dec(vNumPoints);
  end;
  SeekGeneral(APBit,[se3D]);
  vLeader.Extrusion := ReadExtrusion(APBit);
  FHooklineVector := Read3D(APBit);
  SeekGeneral(APBit,[se3D]);
  if Version > acR13 then
    SeekGeneral(APBit,[se3D]);
  if Version < acR2000 then
    FGAP := ReadBD(APBit);
  if Version < acR2010 then
  begin
    SeekGeneral(APBit,[seBD]);
    FTextAnnotationWidth := ReadBD(APBit);
  end
  else
    FTextAnnotationWidth := 0.0;
  FHooklineVector := PtXScalar(FHooklineVector, 2 * ReadBit(APBit) - 1);
  vLeader.Arrowhead := ReadBit(APBit) <> 0;
  if Version < acR2000 then
  begin
    SeekGeneral(APBit, [seBS]);{FArrowHeadType := ReadBS(APBit);}
    FArrowSize := ReadBD(APBit);// contains a size including the scale
    Inc2(APBit,2);
    SeekGeneral(APBit, [seBS]);
  end;
  SeekGeneral(APBit, [seBS]);
  Inc2(APBit,2);
end;

procedure TdwgLeader.References(AReader: TdwgReader);
const
  cnstHookLineLeaderAccuracy = 0.05;
var
//  I, J: Integer;
  vLeader: TsgDXFLeaderAccess;
  vScale: Double;
//  vEEDItems: TsgObjectCollection;
  vValue: Variant;
  vHookLine: Integer;
  vP1, vP2, vVector: TFPoint;
  vAngleFactor: Double;

  A: TsgDimNameVal;
  vBlockRecord: TsgDXFEntity;
  vDimArrowType: TsgDimArrowType;
  P: PsgDimStyle;
  vAnnotationObject: TsgDXFEntity;
  vAttributes: IsgGlobalPropProvider;
begin
  inherited References(Reader);
  vLeader := TsgDXFLeaderAccess(FEntity);
  vHookLine := 1;
  if Assigned(FHookLine) then
    vHookLine := FHookLine^
  else
  begin
    if vLeader.Controls.Count > 1 then
    begin
      vP1 := vLeader.Controls[vLeader.Controls.Count - 2];
      vP2 := vLeader.Controls[vLeader.Controls.Count - 1];
      vVector := SubFPoint(vP2, vP1);
      DoExtrusion(vVector, vLeader.Extrusion);
      vAngleFactor := Abs(Frac(GetAngleOfVector(vVector, True) / Pi));
      if IsZero((vAngleFactor - 1) * vAngleFactor, cnstHookLineLeaderAccuracy) or IsZero(DistanceVectorSqr(vVector.X, vVector.Y, vVector.Z)) then
        vHookLine := 1
      else
        vHookLine := 0;
    end;
  end;

  vLeader.DimStyle := TsgDXFDimensionStyle(AReader.ObjByHandle(FDimStyle, TsgDXFDimensionStyle));
  if vLeader.DimStyle = nil then
    vLeader.DimStyle := AReader.Conv.DimensionStyleByName(sStandardName);

  if Assigned(FACADExtData) then
  begin
    FCanApplyEED := True;
    if ApplyEED(FACADExtData, Reader.FACADAppID) then
      FreeAndNil(FACADExtData);
  end;

  vScale := vLeader.ArrowScale;
  if Version < acR2000 then
    vLeader.ArrowSize := FArrowSize;

  if vnDIMASZ in FOverrides then
  begin
    vLeader.ArrowSize := FArrowSizeOverrides;
    vLeader.ArrowScale := vScale;
  end;

  if ([vnDIMSCALE, vnDIMASZ] * FOverrides = [vnDIMSCALE, vnDIMASZ]) or
     ((Version >= acR2000) and (vnDIMSCALE in FOverrides)) then
  begin
    if FArrowScaleOverrides <> 0 then
      vLeader.ArrowScale := FArrowScaleOverrides
    else
      vLeader.ArrowScale := vScale;
  end;

  P := @TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).FDimProps;
  for A := vnDIMBLK to vnDIMLRBLK do
    if not FArrows.IsEmptyLink(A) then
      Include(FOverrides, A);
  // always perform DIMLDRBLK in this case ??
  if not (vnDIMLRBLK in FOverrides) then
    if FArrows.IsEmptyLink(vnDIMLRBLK) then
    begin
      if not FArrows.IsEmptyLink(vnDIMBLK1) and FArrows.IsEqualLinks(vnDIMBLK1, vnDIMBLK2) then
      begin
        if not FArrows.IsEmptyLink(vnDIMBLK) or ([vnDIMBLK1, vnDIMBLK2] * FOverrides = [vnDIMBLK1, vnDIMBLK2]) then
        begin
          if FArrows.IsEmptyLink(vnDIMBLK) then
            FArrows.Arrows[vnDIMLRBLK] := FArrows.Arrows[vnDIMBLK1];
        end
        else
          if TsgDXFDimensionStyleAccess(vLeader.DimStyle).FDimProps.Arrows.Blk <> datUndefined then
            FArrows.Name[vnDIMLRBLK] := sgDimensionArrowTypeNames[datClosedfilled]
          else
            FArrows.Arrows[vnDIMLRBLK] := FArrows.Arrows[vnDIMBLK1];
      end
      else
        if Version < acR2000 then
        begin
          if FArrows.IsEmptyLink(vnDIMBLK1) and FArrows.IsEmptyLink(vnDIMBLK2) and not FArrows.IsEmptyLink(vnDIMBLK) then
            FArrows.Arrows[vnDIMLRBLK] := FArrows.Arrows[vnDIMBLK]
          else
            if (FOverrides * [vnDIMBLK, vnDIMBLK1, vnDIMBLK2, vnDIMLRBLK] = []) and (P^.Arrows.Blks[vnDIMBLK1] = P^.Arrows.Blks[vnDIMBLK2]) then
              if P^.Arrows.Blks[vnDIMBLK1] <> datUndefined then
                FArrows.Name[vnDIMLRBLK] := sgDimensionArrowTypeNames[P^.Arrows.Blks[vnDIMBLK1]]
              else
                if P^.Arrows.Blks[vnDIMLRBLK] <> datUndefined then
                  FArrows.Name[vnDIMLRBLK] := sgDimensionArrowTypeNames[P^.Arrows.Blks[vnDIMLRBLK]];
        end;
    end
    else
    begin
      if (vnDIMBLK in FOverrides) and (Version <= acR2000) then
        FArrows.Arrows[vnDIMLRBLK] := FArrows.Arrows[vnDIMBLK]
    end;
  for A := vnDIMBLK to vnDIMLRBLK do
    if not FArrows.IsEmptyLink(A) then
    begin
      vBlockRecord := nil;
      case FArrows._Type[A] of
        edtInt64:
          begin
            if FArrows.Handle[A] = cnstBadHandle then
            begin
              FArrows.Name[A] := sgDimensionArrowTypeNames[datClosedfilled];
              vBlockRecord := AReader.Conv.Sections[csBlockRecords].FindEntByName(FArrows.Name[A]);
              if not Assigned(vBlockRecord) then
              begin
                case A of
                  vnDIMBLK: TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).ArrowType := Ord(datClosedfilled);
                  vnDIMBLK1: TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).ArrowType1 := Ord(datClosedfilled);
                  vnDIMBLK2: TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).ArrowType2 := Ord(datClosedfilled);
                  vnDIMLRBLK: TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).ArrowTypeL := Ord(datClosedfilled);
                end;
              end;
            end
            else
            begin
              vBlockRecord := AReader.ObjByHandle(FArrows.Handle[A], nil);
              if not (vBlockRecord is TsgDXFBlockRecord) then
              begin
                vBlockRecord := nil;
                FArrows.Name[A] := sgDimensionArrowTypeNames[datNone];
              end;
            end;
          end;
        edtString:
          begin
            vBlockRecord := AReader.Conv.Sections[csBlockRecords].FindEntByName(FArrows.Name[A]);
            if not Assigned(vBlockRecord) then
            begin
              vDimArrowType := GetArrowTypeByName(FArrows.Name[A], datUndefined);
              if vDimArrowType <> datUndefined then
                P^.Arrows.Blks[A] := vDimArrowType;
            end;
          end;
      end;
      if Assigned(vBlockRecord) then
        TsgDXFDimensionStyleAccess(vLeader.ActualDimStyle).SetBlocks(A, TsgDXFBlockRecordAccess(vBlockRecord).Block)
      else
        if (FArrows._Type[A] = edtString) and (FArrows.Name[A] <> '') then
        begin
          AReader.AddLink(vLeader, DimBlkPropNames[A], FArrows.Name[A], TsgDXFLeaderAccess);
          AReader.FArrows.Add(FArrows.Name[A]);
        end;
    end;

  if (FTAD < 0) and (vLeader.DimStyle <> nil) then
    FTAD := Byte(vLeader.DimStyle.TextPosVert);
  if (Version >= acR2000) and (FGAP = 0) then
    FGAP := vLeader.DimStyle.DIMGAP;

  if (Version >= acR2010) and (FAssociatedAnnotation <> cnstBadHandle) then
  begin
    vAnnotationObject := Reader.ObjByHandle(FAssociatedAnnotation, nil);
    if Assigned(vAnnotationObject) and GetGlobalPropProvider(vAnnotationObject, False, vAttributes) then
      if vAttributes.TryGetValue(cnstTextAnnotationWidth, vValue) and not sgVarIsEmpty(vValue) then
      begin
        FTextAnnotationWidth := vValue;
        vAttributes.RemoveItem(cnstTextAnnotationWidth);
      end;
  end;

  if FAnnotationType < 2 then
    TsgDXFLeaderAccess(vLeader).DoAdditionalLine(vHookLine, FHooklineVector, FTAD, FGAP, FTextAnnotationWidth);
end;

{TdwgLine}

{protected}

procedure TdwgLine.ReadEntData(var APBit: TsgInt64);
var
  vLine: TsgDXFLine;
  vZ: Boolean;
  vStartPoint,vEndPoint: TFPoint;
begin
  vLine := TsgDXFLine(FEntity);
  if Version < acR2000 then
  begin
    vStartPoint := Read3D(APBit);
    vEndPoint := Read3D(APBit);
  end
  else
  begin
    vZ := ReadBit(APBit) = 0;
    vStartPoint.X := ReadRD(APBit);
    vEndPoint.X := ReadDD(APBit, vStartPoint.X);
    vStartPoint.Y := ReadRD(APBit);
    vEndPoint.Y := ReadDD(APBit, vStartPoint.Y);
    vStartPoint.Z := 0; vEndPoint.Z := 0;
    if vZ then
    begin
      vStartPoint.Z := ReadRD(APBit);
      vEndPoint.Z := ReadDD(APBit, vStartPoint.Z);
      if cnstLoadCad2D then
      begin
        vStartPoint.Z := 0;
        vEndPoint.Z := 0;
      end;
    end;
  end;
  vLine.Point := vStartPoint;
  vLine.Point1 := vEndPoint;
  vLine.ZThick := ReadZThik(APBit);
  vLine.Extrusion := ReadExtrusion(APBit);
end;

procedure TdwgLine.References(AReader: TdwgReader);
var
  I: Integer;
begin
  inherited References(AReader);
  if Assigned(FACADExtData) then
  begin
    I := FACADExtData.IndexOfCode(1071);
    if I >= 0 then
    begin
      FEntity.ColorCAD := CmEntityColorToColorCAD(FACADExtData.DataInt[I]);
      FreeAndNil(FACADExtData);
    end;
  end;
end;

{TdwgLWPline}

{protected}

procedure TdwgLWPline.ReadEntData(var APBit: TsgInt64);
var
  vFlag, vNumPoints, vNumBulges, vNumWidths, vNumVertexIDs: Integer;
  vPolyline: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
  vPoint: TFPoint;
  I: Integer;
begin
  vPolyline := TsgDXFLWPolyline(FEntity);
  vNumBulges := 0;
  vNumWidths := 0;
  vNumVertexIDs := 0;
  vFlag := ReadBS(APBit);
  vPolyline.Closed := vFlag and 512 <> 0;
  vPolyline.Flags := (vPolyline.Flags and $FF7F) or
    (Byte(vFlag and 256 <> 0) shl 7);
  if vFlag and 4 <> 0 then
    vPolyline.GlobalWidth := ReadBD(APBit);
  if vFlag and 8 <> 0 then
    vPolyline.Elevation := ReadElevation(APBit);
  if vFlag and 2 <> 0 then
    vPolyline.ZThick := ReadZThik(APBit);
  if vFlag and 1 <> 0 then
    vPolyline.Extrusion := ReadExtrusion(APBit);
  vNumPoints := ReadBL(APBit);
  if vFlag and 16 <> 0 then
    vNumBulges := ReadBL(APBit);
  if vFlag and 32 <> 0 then
    vNumWidths := ReadBL(APBit);
  if (Version >= acR2010) and (vFlag and $400 <> 0) then
    vNumVertexIDs := ReadBL(APBit); // R2010+: number of vertex identifiers (when present, always the same as number of vertes)
  vPoint.Z := 0.0;
  for I := 0 to vNumPoints - 1 do
  begin
    if (Version < acR2000) or (I = 0) then
    begin
      vPoint.X := ReadRD(APBit);
      vPoint.Y := ReadRD(APBit);
    end
    else
    begin
      vPoint.X := ReadDD(APBit, vPoint.X, vPoint.X);
      vPoint.Y := ReadDD(APBit, vPoint.Y, vPoint.Y);
    end;
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := vPoint;
    vPolyline.AddEntity(vVertex);
  end;
  for I := 0 to vNumBulges - 1 do
  begin
    vPoint.X := ReadBD(APBit);
    if I < vNumPoints then
    begin
      if sgIsZero(vPoint.X, 1E-200) then
        vPoint.X := 0;
      vVertex := TsgDXFVertex(vPolyline.Entities[I]);
      vVertex.Bulge := vPoint.X;
    end;
  end;
  for I := 0 to vNumVertexIDs - 1 do
    SeekGeneral(APBit, [seBL]);
  for I := 0 to vNumWidths - 1 do
  begin
    vPoint := Read2D(APBit);
    if I < vNumPoints then
    begin
      vVertex := TsgDXFVertex(vPolyline.Entities[I]);
      vVertex.StartWidth := vPoint.X;
      vVertex.EndWidth := vPoint.Y;
    end;
  end;
end;

{TdwgMLine}

{protected}

procedure TdwgMLine.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FStyle := ReadID(APBit);
end;

procedure TdwgMLine.ReadEntData(var APBit: TsgInt64);
var
  vMLine: TsgCADMLine;
  vVertex: TsgMVertex;
  I,J,vNumSegparms,vNumAreaFillParms,vLinesInStyle,vNumVerts: Integer;
begin
  vMLine := TsgCADMLine(FEntity);
  vMLine.ScaleFactor := ReadBD(APBit);
  vMLine.Justify := ReadRC(APBit);
  vMLine.StartPoint := Read3D(APBit);
  vMLine.Extrusion := ReadExtrusion(APBit);
  vMLine.Flags := ReadBS(APBit);
  vLinesInStyle := ReadRC(APBit);
  vNumVerts := ReadBL(APBit);// verified experimentally. possibly an error in the documentation (BS 72)
  while vNumVerts > 0 do
  begin
    vVertex := TsgMVertex.Create;
    vVertex.Point := Read3D(APBit);
    vVertex.Direction := Read3D(APBit);
    vVertex.Miter := Read3D(APBit);
    for I:=0 to vLinesInStyle-1 do
    begin
      vNumSegparms := ReadBS(APBit);
      vVertex.NewList;
      for J:=0 to vNumSegparms-1 do
        vVertex.Add(ReadBD(APBit));
      vNumAreaFillParms := ReadBS(APBit);
      for J:=0 to vNumAreaFillParms-1 do
        SeekGeneral(APBit,[seBD]);
    end;
    vVertex.Complete;
    vMLine.AddEntity(vVertex);
    Dec(vNumVerts);
  end;
end;

procedure TdwgMLine.References(AReader: TdwgReader);
begin
  inherited References(Reader);
  TsgCADMLine(FEntity).Style :=
    TsgMLineStyle(AReader.ObjByHandle(FStyle, TsgMLineStyle));
end;

{TdwgMText}

{protected}

function TdwgMText.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if AAppID = Reader.FACADAppID then
  begin
    FACADExtData := AEEData;
    FACADExtData.ChangeType(0, edtString);
    FACADExtData.DataString[0] := sACADXDataAppName;
  end;
end;

function TdwgMText.CreateStringReader(var APBit: TsgInt64;
  AVersion: TsgDWGVersion): TdwgStringReader;
begin
  case AVersion of
    acR2007..DWGVersionHigh:
      Result := TdwgWideStringReader.Create(@FStrsStart, Reader.Conv.CodePage);
  else
    Result := TdwgAnsiStringReader.Create(@APBit, Reader.Conv.CodePage);
  end;
end;

procedure TdwgMText.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  ReadStyleHandleData(APBit);
end;

procedure TdwgMText.ReadEntData(var APBit: TsgInt64);
var
  vMtext: TsgDXFMtext;
//  vDir: Integer;
//  vExt: TFPoint;
  vIsNotAnnotative, vAutoHeight: Boolean;
  vColumnType, vColumnHeightCount: Integer;
begin
  vMtext := TsgDXFMtext(FEntity);
  vMtext.Point := Read3D(APBit);
  vMtext.Extrusion := ReadExtrusion(APBit);
  vMtext.Point1 := Read3D(APBit);
  vMtext.RectWidth := ReadBD(APBit);
  if Version >= acR2007 then
    SeekGeneral(APBit,[seBD]);
  vMtext.Height := ReadBD(APBit);
  vMtext.Align := ReadBS(APBit);
{ TODO: Read mtext direction flag and extents
  vDir := ReadBS(APBit);}
  SeekGeneral(APBit, [seBS, seBD]);
  FTextAnnotationWidth := ReadBD(APBit);
  vMtext.Text := ReadMText(Self);
  //FStyle := ReadID(APBit);
  if Version >= acR2000 then
  begin
    vMtext.LineSpacingStyle := ReadBS(APBit);
    vMtext.LineSpacingFactor := ReadBD(APBit);
    SeekGeneral(APBit, [seB]);//Unknown bit
    if Version >= acR2004 then
    begin
      vMtext.BackgroundFlags := ReadBL(APBit);//BL 90: 0 = no background, 1 = background fill, 2 = background fill with drawing fill color; 0x10 = text frame (R2018+)
      if vMtext.BackgroundFlags and $11 <> 0 then
      begin
        vMtext.BackgroundScaleFactor := ReadBD(APBit);//BD 45 default = 1.5
        vMtext.BackgroundColor := ReadColor(APBit);//CMC 63
        vMtext.BackgroundTransparency := ReadBL(APBit);//BL 441
      end;
      if Version >= acR2018 then
      begin
        vIsNotAnnotative := ReadBit(APBit) = 1;
        if vIsNotAnnotative then
        begin
          SeekGeneral(APBit, [seBS, seB]);//Version BS Default 0;Default flag B Default true
          //BEGIN REDUNDANT FIELDS
          //ReadID(APBit);//??Registered application H Hard pointer
          vMtext.Align := ReadBL(APBit);//Attachment point
          vMtext.Point1 := Read3D(APBit);//X-axis dir 3BD 10
          vMtext.Point := Read3D(APBit);//Insertion point 3BD 11
          vMtext.RectWidth := ReadBD(APBit);//Rect width BD 40
          SeekGeneral(APBit, [seBD, seBD, seBD]);//Rect height BD 41;Extents width BD 42;Extents height BD 43
          //END REDUNDANT FIELDS
          vColumnType := ReadBS(APBit);//Column type BS 71 0 = No columns, 1 = static columns, 2 = dynamic columns
          if vColumnType <> 0 then
          begin
            vColumnHeightCount := ReadBL(APBit);//Column height count BL 72
            SeekGeneral(APBit, [seBD, seBD]);//Columnn width BD 44;Gutter BD 45
            vAutoHeight := ReadBit(APBit) = 1;//Auto height? B 73
            SeekGeneral(APBit, [seB]);//Flow reversed? B 74
            if not vAutoHeight and (vColumnType = 2) then
              while vColumnHeightCount > 0 do
              begin
                SeekGeneral(APBit, [seBD]);
                Dec(vColumnHeightCount);
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TdwgMText.ReadHandleRefs(var APBit: TsgInt64);
var
  vAttributes: IsgGlobalPropProvider;
begin
  inherited ReadHandleRefs(APBit);
  if (FReactors.Count > 0) and GetGlobalPropProvider(FEntity, False, vAttributes) then
  begin
    vAttributes[cnstTextAnnotationWidth] := FTextAnnotationWidth;
    Reader.FAnnotationProps.Add(vAttributes);
  end;
end;

procedure TdwgMText.ReadStyleHandleData(var APBit: TsgInt64);
begin
  FStyle := ReadID(APBit);
end;

procedure TdwgMText.References(AReader: TdwgReader);
begin
  inherited References(Reader);
  TsgDXFMText(FEntity).Style := TsgDXFStyle(AReader.ObjByHandle(FStyle, TsgDXFStyle));
  if Assigned(FACADExtData) then
  begin
    FEntity.SetExtData(FACADExtData);
    FreeAndNil(FACADExtData);
  end;
  if Version <= acR14 then
    if (TsgDXFMText(FEntity).RectWidth <> 0) and (FTextAnnotationWidth/TsgDXFMText(FEntity).RectWidth > 30) then // width picked by the user far less extents width!!!
      TsgDXFMText(FEntity).RectWidth := 0;
end;

{TdwgOLE2Frame}

{protected}

procedure TdwgOLE2Frame.ReadEntData(var APBit: TsgInt64);
var
  vOle2Frame: TsgDXFOle2Frame;
  vRawData: AnsiString;
  vDataLength{, vVersion, vUnknown}: Integer;
  vOle2FrameHeader: TsgOle2FrameHeaderData;
begin
  vOle2Frame := TsgDXFOle2Frame(FEntity);
  SeekGeneral(APBit, [seBS]);//vVersion := ReadBS(APBit);// OLE Version number (OLE 1 or OLE 2)
  if Version >= acR2000 then
    SeekGeneral(APBit, [seBS]);
  vDataLength := ReadBL(APBit);
  if vDataLength >= SizeOf(TsgOle2FrameHeaderData) then
  begin
    Dec(vDataLength, SizeOf(TsgOle2FrameHeaderData));
    ReadBytesCustom(APBit, vOle2FrameHeader, SizeOf(TsgOle2FrameHeaderData));
    vOle2Frame.Point := vOle2FrameHeader.Point1;
    vOle2Frame.Point1 := vOle2FrameHeader.Point3;
    vOle2Frame.OLEObjectType := vOle2FrameHeader.OLEObjectType;//1 = Link; 2 = Embedded; 3 = Static
    vOle2Frame.Aspect := vOle2FrameHeader.DrawAspect;
    if vDataLength > 0 then
    begin
      SetLength(vRawData, vDataLength);
      ReadBytesCustom(APBit, PPointer(vRawData)^, vDataLength);
      vOle2Frame.BinaryData := vRawData;
    end
    else
      ReadError;
  end
  else
    ReadError;
  if Version >= acR2000 then
    SeekGeneral(APBit, [seRC]);//{vUnknown := }ReadRC(APBit); //1 or 2??
end;

{TdwgPoint}

{protected}

procedure TdwgPoint.ReadEntData(var APBit: TsgInt64);
var
  vPoint: TsgDXFPoint;
begin
  vPoint := TsgDXFPoint(FEntity);
  vPoint.Point := Read3D(APBit);
  vPoint.ZThick := ReadZThik(APBit);
  vPoint.Extrusion := ReadExtrusion(APBit);
  SeekGeneral(APBit,[seBD]);
end;

{TdwgPolyline}

{protected}

constructor TdwgPolyline.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
end;

procedure TdwgPolyline.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  ReadSubEntIDs(APBit);// ReadLimits(APBit,Self);
  //ReadID(APBit); // SEQEND
end;

procedure TdwgPolyline.ReadEntData(var APBit: TsgInt64);
var
  vPolyline: TsgDXFPolyline;
begin
  vPolyline := TsgDXFPolyline(FEntity);
  vPolyline.Closed := ReadBS(APBit) and 1 <> 0;
  SeekGeneral(APBit, [seBS]);
  vPolyline.StartWidth := ReadBD(APBit);
  vPolyline.EndWidth := ReadBD(APBit);
  vPolyline.ZThick := ReadZThik(APBit);
  vPolyline.Elevation := ReadElevation(APBit);
  vPolyline.Extrusion := ReadExtrusion(APBit);
  if Version >= acR2004 then
    FSubEnts := ReadBL(APBit);
end;

{ TdwgPlotSettings }

function TdwgPlotSettings.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Dictionary[cnstObjectsPLOTSETTINGS];
end;

procedure TdwgPlotSettings.ReadData(var APBit: TsgInt64);
var
  vPlotSettings: TsgDXFPlotSettings;
  vPlotSettingsData : PsgPlotSettingsData;
begin
  vPlotSettings := TsgDXFPlotSettings(FEntity);
  vPlotSettingsData := TsgDXFPlotSettingsAccess(vPlotSettings).PlotData;
  ReadObjHeader(APBit);
  //PlotSettings
  Reader.ReadPlotSettings(Self, vPlotSettingsData, APBit);
  ReadHandleRefs(APBit);
end;

{ TdwgRasterVariables }

function TdwgRasterVariables.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csObjects];
end;

procedure TdwgRasterVariables.ReadData(var APBit: TsgInt64);
var
  vRasterVars: TsgCADRasterVariables;
begin
  ReadObjHeader(APBit);
  ReadBL(APBit); // Classver BL 90 classversion
  vRasterVars := TsgCADRasterVariables(FEntity);
  vRasterVars.DisplayImageFrame := ReadBS(APBit) = 1;   // Dispfrm BS 70 displayframe
  vRasterVars.ImageDisplayQuality := ReadBS(APBit) = 1; // Dispqual BS 71 display quality
  vRasterVars.UnitsForInsertingImages := ReadBS(APBit); // Units BS 72 units
  ReadHandleRefs(APBit);
end;

{TdwgSolid}

{protected}

procedure TdwgSolid.ReadEntData(var APBit: TsgInt64);
var
  vSolid: TsgDXFSolid;
  vElevation: Double;
begin
  vSolid := TsgDXFSolid(FEntity);
  vSolid.ZThick := ReadZThik(APBit);
  vElevation := ReadElevation(APBit);
  vSolid.Point := Read2RDElevation(APBit, vElevation);
  vSolid.Point1 := Read2RDElevation(APBit, vElevation);
  vSolid.Point2 := Read2RDElevation(APBit, vElevation);
  vSolid.Point3 := Read2RDElevation(APBit, vElevation);
  vSolid.Extrusion := ReadExtrusion(APBit);
end;

{TdwgSpline}

{protected}

function TdwgSpline.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I, vCode: Integer;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  // FEntity.SetExtData(AEEData);
  if not Result and Assigned(AEEData) and Reader.IsRelatedAppID(AAppID) then
  begin
    I := 0;
    vCode := 0;
    while I < AEEData.DataCount do
    begin
      case AEEData.DataType[I] of
        edtInt16: vCode := AEEData.DataInt16[I];
        edtInt:
          begin
            case vCode of
              70: TsgDXFSplineAccess(FEntity).FlagsExtented := AEEData.DataInt[I];
            end;
            vCode := 0;
          end;
      else
        vCode := 0;
      end;
      Inc(I);
    end;
    Result := True;
  end;
end;

procedure TdwgSpline.ReadEntData(var APBit: TsgInt64);
var
  I, vScenario, vNumFit, vNumControlPoints, vNumKnots: Integer;
  vSpline: TsgDXFSpline;
  vWeight: Boolean;
  vFlags1, vKnotParameter: Cardinal;
begin
  vSpline := TsgDXFSpline(FEntity);
  vScenario := ReadBS(APBit);    //maybe ReadBL(APBit)
  if Version >= acR2013 then
  begin
    vFlags1 := ReadBL(APBit); //vFlags1 := ReadBL(APBit) and 4;
    vKnotParameter := ReadBL(APBit);
    //if vScenario <> 1 then raise EdwgError.Create
    //vFlags1 == 9: method fit points = 1, Use knot parameter = 8
    if (vFlags1 and 9 = 9) and (vKnotParameter <> $F) then
      Inc(vScenario);
  end;
  vSpline.Degree := ReadBS(APBit); //maybe ReadBL(APBit)
  vNumFit := 0;
  vNumControlPoints := 0;
  vNumKnots := 0;
  vWeight := False;
  if vScenario and 2 <> 0 then
  begin
    vSpline.FitTol := ReadBD(APBit);
    vSpline.BeginningTangent := Read3D(APBit);
    vSpline.EndingTangent := Read3D(APBit);
    vNumFit := ReadBL(APBit);
  end;
  if vScenario and 1 <> 0 then
  begin
    vSpline.Flags := vSpline.Flags or (4 * Ord(ReadBit(APBit) <> 0));// 3 bit Flags
    vSpline.Closed := ReadBit(APBit) <> 0;                           // 2 bit Flags
    vSpline.Flags := vSpline.Flags or Ord(ReadBit(APBit) <> 0);      // 1 bit Flags
    vSpline.KnotTol := ReadBD(APBit);
    vSpline.CtrlTol := ReadBD(APBit);
    vNumKnots := ReadBL(APBit);
    vNumControlPoints := ReadBL(APBit);
    vWeight := ReadBit(APBit) <> 0;
  end;
  for I := 0 to vNumKnots-1 do
    vSpline.Knots.Add(ReadBD(APBit));
  for I := 0 to vNumControlPoints-1 do
  begin
    vSpline.Controls.Add(Read3D(APBit));
    if vWeight then
      vSpline.AddWeight(ReadBD(APBit));
  end;
  for I := 0 to vNumFit-1 do
  begin
    vSpline.Fit.Add(Read3D(APBit));
  end;
end;

{TdwgStyle}

{protected}

function TdwgStyle.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I, vFontStyleData: Integer;
  vmvFontStyles: TmvFontStyles;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) and Reader.IsACADAppID(AAppID) then
  begin
    I := 0;
    while I < AEEData.DataCount do
    begin
      case AEEData.DataType[I] of
        edtstring:
          begin
            TsgDXFStyle(FEntity).FontName := AEEData.DataString[I];
          end;
        edtInt:
          begin
            vmvFontStyles := [];
            vFontStyleData := AEEData.DataInt[I];
            if vFontStyleData and $1000000 <> 0 then
              Include(vmvFontStyles, fmItalic);
            if vFontStyleData and $2000000 <> 0 then
              Include(vmvFontStyles, fmBold);
            TsgDXFStyle(FEntity).FontStyle := vmvFontStyles;
          end;
      end;
      Inc(I);
    end;
    Result := True;
  end;
end;

function TdwgStyle.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csStyles];
end;

procedure TdwgStyle.ReadData(var APBit: TsgInt64);
var
  vStyle: TsgDXFStyle;
begin
  inherited ReadData(APBit);
  FEntity.Name := FEntryName;
  FFlags := FFlags or ReadBit(APBit);
  FFlags := FFlags or ReadBit(APBit) shl 3;
  vStyle := TsgDXFStyle(FEntity);
  vStyle.Flags := FFlags;
  vStyle.FixedHeight := ReadBD(APBit);
  vStyle.WidthFactor := ReadBD(APBit);
  vStyle.ObliqueAngle := ReadBD(APBit) * f180DividedByPi;
  vStyle.TextGenFlags := ReadRC(APBit);
  vStyle.LastHeightUsed := ReadBD(APBit);
  vStyle.PrimaryFont := ReadText;
  vStyle.BigFont := ReadText;
  ReadHandleRefs(APBit);
end;

procedure TdwgStyle.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  // update style control handle if it is bad
  if Reader.FStyles.FHandle = cnstBadHandle then
    Reader.FStyles.FHandle := FOwner;
end;

{TdwgText}

{protected}

procedure TdwgText.ApplyValue;
begin
  TsgDXFText(FEntity).Text := FText;
end;

constructor TdwgText.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FScale := 1.0;
end;

function TdwgText.CreateStringReader(var APBit: TsgInt64;
  AVersion: TsgDWGVersion): TdwgStringReader;
begin
  case AVersion of
    acR2007..DWGVersionHigh:
      Result := TdwgWideStringReader.Create(@FStrsStart, Reader.Conv.CodePage);
  else
    Result := TdwgAnsiStringReader.Create(@APBit, Reader.Conv.CodePage);
  end;
end;

procedure TdwgText.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FStyle := ReadID(APBit);
end;

procedure TdwgText.ReadEntData(var APBit: TsgInt64);
begin
  if Version < acR2000 then
  begin
    FDataFlags := $FF;
    FPoint.Z := ReadElevation(APBit);
    FPoint := Read2RDElevation(APBit, FPoint.Z);
    FPoint1 := Read2RDElevation(APBit, FPoint.Z);
    FExtrusion := ReadExtrusion(APBit);
    SeekGeneral(APBit,[seBD]);
    FObliqueAngle := ReadBD(APBit) * f180DividedByPi;
    FRotation := ReadBD(APBit) * f180DividedByPi;
    FHeight := ReadBD(APBit);
    FScale := ReadBD(APBit);
    FText := ReadText;
    FGeneration := ReadBS(APBit);
    FHAlign := ReadBS(APBit);
    FVAlign := ReadBS(APBit);
  end
  else
  begin
    FDataFlags := not ReadRC(APBit);
    FPoint := Read2RDElevation(APBit, ReadElevation(APBit));
    if FDataFlags and $02 <> 0 then
    begin
      FPoint1.X := ReadDD(APBit, FPoint.X);
      FPoint1.Y := ReadDD(APBit, FPoint.Y);
      FPoint1.Z := FPoint.Z;
    end;
    FExtrusion := ReadExtrusion(APBit);
    ReadZThik(APBit);
    if FDataFlags and $04 <> 0 then
      FObliqueAngle := ReadRD(APBit) * f180DividedByPi;
    if FDataFlags and $08 <> 0 then
      FRotation := ReadRD(APBit) * f180DividedByPi;
    FHeight := ReadRD(APBit);
    if FDataFlags and $10 <> 0 then
      FScale := ReadRD(APBit);
    FText := ReadText;
    if FDataFlags and $20 <> 0 then
      FGeneration := ReadBS(APBit);
    if FDataFlags and $40 <> 0 then
      FHAlign := ReadBS(APBit);
    if FDataFlags and $80 <> 0 then
      FVAlign := ReadBS(APBit);
  end;
end;

function TdwgText.ReadElevation(var APBit: TsgInt64): Double;
begin
  if Version < acR2000 then
    Result := inherited ReadElevation(APBit)
  else
  begin
    if FDataFlags and $01 <> 0 then
    begin
      if cnstLoadCad2D then
      begin
        Result := 0;
        SeekGeneral(APBit, [seRD]);
      end
      else
        Result := ReadRD(APBit);
    end
    else
      Result := 0;
  end;
end;

function TdwgText.ReadExtrusion(var APBit: TsgInt64): TFPoint;
begin
  if Version < acR2000 then
    DoReadExtrusion(APBit, Result)
  else
    Result := inherited ReadExtrusion(APBit);
end;

procedure TdwgText.References(AReader: TdwgReader);
var
  vText: TsgDXFText;
begin
  inherited References(Reader);
  vText := TsgDXFText(FEntity);
  vText.Style := TsgDXFStyle(AReader.ObjByHandle(FStyle, TsgDXFStyle));
  vText.Point := FPoint;
  vText.Point1 := FPoint1;
  vText.Extrusion := FExtrusion;
  vText.ObliqueAngle := FObliqueAngle;
  vText.Rotation := FRotation;
  vText.Height := FHeight;
  vText.Scale := FScale;
  vText.Generation := FGeneration;
  vText.HAlign := FHAlign;
  vText.VAlign := FVAlign;
  ApplyValue;
end;

{ TsgDXFToleranceProxy }

function ColorCADToVariant(const AColor: TsgColorCAD): Variant;
begin
  Result := VarArrayCreate([0, 2], varVariant);
  Result[0] := AColor.Active;
  Result[1] := AColor.Color;
  Result[2] := AColor.AlbumString;
end;

function VariantToColorCAD(const AValue: Variant): TsgColorCAD;
begin
  Result.Active := AValue[0];
  Result.Color := AValue[1];
  Result.AlbumString := AValue[2];
end;

function TsgDXFToleranceProxy.GetDimlineColor: Variant;
begin
  Result := ColorCADToVariant(inherited GetDimlineColor);
end;

procedure TsgDXFToleranceProxy.SetDimlineColor(const AValue: Variant);
begin
  inherited SetDimlineColor(VariantToColorCAD(AValue));
end;

{TdwgTolerance}

{protected}

constructor TdwgTolerance.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  InitToleranceProps;
  FOverrideProps := TsgStringList.Create;
  TsgStringList(FOverrideProps).CaseSensitive := False;
  TsgStringList(FOverrideProps).Sorted := True;
end;

function TdwgTolerance.CreateStringReader(var APBit: TsgInt64;
  AVersion: TsgDWGVersion): TdwgStringReader;
begin
  case AVersion of
    acR2007..DWGVersionHigh:
      Result := TdwgWideStringReader.Create(@FStrsStart, Reader.Conv.CodePage);
  else
    Result := TdwgAnsiStringReader.Create(@APBit, Reader.Conv.CodePage);
  end;
end;

destructor TdwgTolerance.Destroy;
begin
  ClearObjects(FOverrideProps);
  FOverrideProps.Free;
  inherited Destroy;
end;

function TdwgTolerance.GetValue(Index: Variant): Variant;
var
  I: Integer;
  S: string;
begin
  VarClear(Result);
  I := -1;

{$IFDEF SGDEL_6}
  if VarIsOrdinal(Index) then
{$ELSE}
  if TVarData(Index).VType in [varInteger] then
{$ENDIF}
    I := Index
  else
{$IFDEF SGDEL_6}
    if VarIsStr(Index) then
{$ENDIF}
    begin
      S := Index;
      I := FOverrideProps.IndexOf(S);
    end;
  if (I >= 0) and (I < FOverrideProps.Count) then
    if Assigned(FOverrideProps.Objects[I]) then
      Result := TsgObjectVariant(FOverrideProps.Objects[I]).FieldVariant;
end;

procedure TdwgTolerance.SetValue(Index, AValue: Variant);
var
  I: Integer;
  S: string;
begin
  I := -1;
{$IFDEF SGDEL_6}
  if VarIsOrdinal(Index) then
{$ELSE}
  if TVarData(Index).VType in [varInteger] then
{$ENDIF}
    I := Index
  else
{$IFDEF SGDEL_6}
    if VarIsStr(Index) then
{$ENDIF}
    begin
      S := Index;
      I := FOverrideProps.IndexOf(S);
      if I < 0 then
        AddProp(S, AValue);
    end;
  if (I >= 0) and (I < FOverrideProps.Count) then
  begin
    if not Assigned(FOverrideProps.Objects[I]) then
      FOverrideProps.Objects[I] := TsgObjectWithField.CreateVariant(AValue)
    else
      TsgObjectVariant(FOverrideProps.Objects[I]).FieldVariant := AValue;
  end;
end;

function TdwgTolerance.AddProp(const AName: string;
  AValue: OleVariant): Integer;
begin
  Result := FOverrideProps.AddObject(AName, TsgObjectWithField.CreateVariant(AValue));
end;

function TdwgTolerance.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I: Integer;
  vStop, vNeedCode: Boolean;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) and (AEEData.DataCount > 0) and
     Reader.IsACADAppID(AAppID) then
  begin
    I := 0;
    vStop := False;
    vNeedCode := False;
    repeat
      vNeedCode := not vNeedCode;
      case AEEData.DataType[I] of
        edtByte:
          begin
            vStop := AEEData.DataByte[I] = 1;
            vNeedCode := False; // if not vStop, then next string is 'DSTYLE'
          end;
        edtInt16:
          if not vNeedCode then
            case AEEData.DataInt16[I - 1] of
              176: Value[cnstTolerancePropNameDimlineColor] :=
                ColorCADToVariant(MakeColorCAD(acIndexColor, AEEData.DataInt16[I]));
            end;
        edtDouble:
          case AEEData.DataInt16[I - 1] of
            40: Value[cnstTolerancePropNameDimScaleOverall] := AEEData.DataDouble[I];
            140: Value[cnstTolerancePropNameHeight] := AEEData.DataDouble[I];
            147: Value[cnstTolerancePropNameGAP] := AEEData.DataDouble[I];
          end;
      end;
      Inc(I);
    until (I >= AEEData.DataCount) or vStop;
    Result := True;
  end;
end;

procedure TdwgTolerance.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FDimStyle := ReadID(APBit);
end;

procedure TdwgTolerance.ReadEntData(var APBit: TsgInt64);
var
  vTolerance: TsgDXFTolerance;
begin
  vTolerance := TsgDXFTolerance(FEntity);
  if Version < acR2000 then
  begin
    SeekGeneral(APBit, [seBS]);  // Unknown short S
    FHeightR13 := ReadBD(APBit); // Height BD --
    FGAPR13 := ReadBD(APBit);    // Dimgap(?) BD dimgap at time of creation, * dimscale
  end;
  vTolerance.Point := Read3D(APBit);
  vTolerance.Point1 := Read3D(APBit);
  vTolerance.Extrusion := Read3D(APBit);
  vTolerance.Text := ReadMText(Self);
end;

procedure TdwgTolerance.References(AReader: TdwgReader);
var
  I, J: Integer;
  P: PPropInfo;
  vDimStyleScale: Double;
  vTolerance: TsgDXFTolerance;
  vVariantScaleOverrides, vGAP, vHeight: Variant;
  vDimStyleScaleOverall: Variant;

{$IFNDEF SGDEL_2005}
  procedure SetTolerancePropValues;
  begin
    if ToleranceProps[J] = cnstTolerancePropNameDimlineColor then
      vTolerance.DimlineColor := VariantToColorCAD(Value[I])
    else
    if ToleranceProps[J] = cnstTolerancePropNameDimScaleOverall then
      vTolerance.DimScaleOverall := Value[I]
    else
    if ToleranceProps[J] = cnstTolerancePropNameHeight then
      vTolerance.Height := Value[I]
    else
    if ToleranceProps[J] = cnstTolerancePropNameGAP then
      vTolerance.GAP := Value[I];
  end;
{$ENDIF}

begin
  inherited References(Reader);
  vTolerance := TsgDXFTolerance(FEntity);
  vTolerance.DimStyle := TsgDXFDimensionStyle(AReader.ObjByHandle(FDimStyle, TsgDXFDimensionStyle));
  vDimStyleScale := 1.0;
  if Assigned(vTolerance.DimStyle) then
    vDimStyleScale := vTolerance.DimStyle.Scale;
  vDimStyleScaleOverall := Value[cnstTolerancePropNameDimScaleOverall];
  if VarType(vDimStyleScaleOverall) = varDouble then
    vDimStyleScale := vDimStyleScaleOverall;
  //----------------------------------------------------------------------------
  //---------- code testing with all known internal files ----------------------
  if Version < acR2000 then
  begin
    vGAP := Value[cnstTolerancePropNameGAP];
//    if VarType(vGAP) = varDouble then
//      Value[cnstTolerancePropNameGAP] := vGAP / vDimStyleScale
//    else
      if FGAPR13 <> 0.0 then
        Value[cnstTolerancePropNameGAP] := FGAPR13 / vDimStyleScale;
    vHeight := Value[cnstTolerancePropNameHeight];
//    if VarType(vHeight) = varDouble then
//      Value[cnstTolerancePropNameHeight] := vHeight / vDimStyleScale
//    else
      if FHeightR13 <> 0.0 then
        Value[cnstTolerancePropNameHeight] := FHeightR13 / vDimStyleScale;
   // Value[cnstTolerancePropNameDimScaleOverall] := 1.0;
  end
  else
  begin
    vVariantScaleOverrides := Value[cnstTolerancePropNameDimScaleOverall];
    if VarType(vVariantScaleOverrides) = varDouble then
    begin
      vGAP := Value[cnstTolerancePropNameGAP];
//      if VarType(vGAP) = varDouble then
//        Value[cnstTolerancePropNameGAP] := vGAP / vDimStyleScale;
      vHeight := Value[cnstTolerancePropNameHeight];
//      if VarType(vHeight) = varDouble then
//        Value[cnstTolerancePropNameHeight] := vHeight / vDimStyleScale
//      else
//        Value[cnstTolerancePropNameHeight] := vTolerance.DimStyle.TextHeight / vDimStyleScale;
    end;
  end;
  //--------------------------------------------------------------------------
  for I := 0 to FOverrideProps.Count - 1 do
  begin
    J := ToleranceProps.IndexOf(FOverrideProps[I]);
    if J >= 0 then
    begin
{$IFDEF SGDEL_2005}
      P := PPropInfo(TsgObjectPointer(ToleranceProps.Objects[J]).FieldPointer);
      SetPropValue(vTolerance, P, Value[I]);
{$ELSE}
   {$IFDEF SGFPC}
      {$IFNDEF SGDEL_6}
         SetPropValue(vTolerance, ToleranceProps[J], Value[I]);
      {$ELSE}
         SetTolerancePropValues;
      {$ENDIF}
   {$ELSE}
      SetTolerancePropValues;
{$ENDIF}
{$ENDIF}
    end;
  end;
end;

{TdwgUndefinedEntity}

{protected}

function TdwgUndefinedEntity.GetBitSize: Integer;
begin
  Result := FBitSize;
end;

function TdwgUndefinedEntity.GetObjectBeginning: TsgInt64;
begin
  Result := FObjectBeginning;
end;

procedure TdwgUndefinedEntity.ReadEntData(var APBit: TsgInt64);
var
  vPBit: TsgInt64;
begin
  vPBit := FObjectBeginning;
  Inc2(vPBit, BitSize);
  APBit := vPBit;
end;

procedure TdwgUndefinedEntity.SetBitSize(const AValue: Integer);
begin
  FBitSize := AValue;
end;

procedure TdwgUndefinedEntity.SetObjectBeginning(const AValue: TsgInt64);
begin
  FObjectBeginning := AValue;
end;

{TdwgVertex}

{protected}

procedure TdwgVertex.ReadEntData(var APBit: TsgInt64);
begin
  TsgDXFVertex(FEntity).Flags := ReadRC(APBit);
  TsgDXFVertex(FEntity).Point := Read3D(APBit);
end;

{TdwgViewport}

{protected}

function TdwgViewport.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I: Integer;
  vViewPort: TsgDXFViewport;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) and Reader.IsACADAppID(AAppID) then
  begin
    vViewPort := TsgDXFViewport(FEntity);
    I := 0;
    while I < AEEData.DataCount do
    begin
      case AEEData.DataType[I] of
        edtInt64:
          case AEEData.DataCode[I] of
            1003: vViewPort.FrozenLayers.Add(AEEData.DataInt64[I]);
            1005: FHandle := AEEData.DataInt64[I];
          end;
        edtF3DPoint:
          begin
            Inc(FCount_10);
            case FCount_10 of
              1: vViewPort.ViewTarget := AEEData.DataPoint[I];
              2: vViewPort.ViewDirection := AEEData.DataPoint[I];
            end;
          end;
        edtDouble:
          begin
            Inc(FCount_40);
            case FCount_40 of
              1: vViewPort.ViewTwistAngle := AEEData.DataDouble[I] * f180DividedByPi;
              2: vViewPort.MSpaceHeight := AEEData.DataDouble[I];
              3: vViewPort.Offset(MakeFPoint(AEEData.DataDouble[I], 0, 0));
              4: vViewPort.Offset(MakeFPoint(0, AEEData.DataDouble[I], 0));
              6: vViewPort.FrontClipPlane := AEEData.DataDouble[I];
              7: vViewPort.BackClipPlane := AEEData.DataDouble[I];
            end;
          end;
      end;
      Inc(I);
    end;
    Result := True;
  end;
end;

procedure TdwgViewport.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vViewport: TsgDXFViewport;
begin
  inherited ReadData(APBit);
  vViewport := TsgDXFViewport(FEntity);
  if Version >= acR2000 then
  begin
    for I := 0 to FFrozenLayerCount - 1 do
      vViewPort.FrozenLayers.Add(ReadID(APBit));
    FClippingBoundaryHandle := ReadID(APBit);
    if Version = acR2000 then
      FEntHeaderHandle := ReadID(APBit);//  VIEWPORT ENT HEADER ((hard pointer))
    ReadID(APBit);// 345 Named UCS Handle (hard pointer)
    ReadID(APBit);// 346 Base UCS Handle (hard pointer)
    // 332 Background (soft pointer)
    // 348 Visual Style (hard pointer)
    // 333 Shadeplot ID (soft pointer)
    // 361 Sun (hard owner)
    if Version >= acR2007 then
      for I := 0 to 3 do
        ReadID(APBit);
  end
  else
    FEntHeaderHandle := ReadID(APBit);
end;

procedure TdwgViewport.ReadEntData(var APBit: TsgInt64);
var
  vViewport: TsgDXFViewport;
begin
  vViewport := TsgDXFViewport(FEntity);
  vViewport.PSpaceCenter := Read3D(APBit);
  vViewport.PSpaceWidth := ReadBD(APBit);
  vViewport.PSpaceHeight := ReadBD(APBit);
  if Version >= acR2000 then
  begin
    vViewport.ViewTarget := Read3D(APBit);
    vViewport.ViewDirection := _Read3D(APBit);
    vViewport.ViewTwistAngle := ReadBD(APBit) * f180DividedByPi;
    vViewport.MSpaceHeight := ReadBD(APBit);  //View Height
    SeekGeneral(APBit,[seBD]); //Lens Length
    vViewport.FrontClipPlane := ReadBD(APBit); //Front Clip Z
    vViewport.BackClipPlane := ReadBD(APBit);  //Back Clip Z
    SeekGeneral(APBit,[seBD]);  //Snap Angle
    vViewport.MSpaceCenter := Read2RD(APBit); //View Center
    // Snap Base, Snap Spacing, Grid Spacing, Circle Zoom
    SeekGeneral(APBit,[se2RD,se2RD,se2RD,seBS]);
    if Version >= acR2007 then
      SeekGeneral(APBit, [seBS]); // Grid Major
    FFrozenLayerCount := ReadBL(APBit);
    vViewport.Flags := ReadBL(APBit);
    ReadText; //Style Sheet
    // Render Mode, UCS at origin, UCS per Viewport, UCS Origin
    // UCS X Axis, UCS Y Axis, UCS Elevation,UCS Ortho View Type
    SeekGeneral(APBit,[seRC,seBB,se3D,se3D,se3D,seBD,seBS]);
    if Version >= acR2004 then
      SeekGeneral(APBit,[seBS]); //ShadePlot Mode BS 170
  end;
end;

procedure TdwgViewport.ReadOwner(AReader: TdwgReader);
var
  I, C: Integer;
  vViewports: PsgHashItemsObjectsArray;
begin
  inherited ReadOwner(AReader);
  if not Assigned(FOwnerObject) then
  begin
    if AReader.FReaderGroupsStack2004.Count > 0 then
      FOwnerObject := TdwgObject(AReader.FReaderGroupsStack2004.Last).FEntity
    else
    begin
      I := 0;
      vViewports := AReader.FViewports.List;
      C := AReader.FViewports.Count;
      while (I < C) and not Assigned(FOwnerObject) do
      begin
        if TsgInt64List(vViewports^[I].Data).IndexOf(FHandle) >= 0 then
          FOwnerObject := AReader.ObjByHandle(vViewports^[I].HashCode, nil);
        Inc(I);
      end;
    end;
  end;
end;

procedure TdwgViewport.References(AReader: TdwgReader);
var
  vViewport: TsgDXFViewportAccess;
  vViewportEntityHeader: TsgDXFViewportEntityHeader;
begin
  inherited References(Reader);
  vViewport := TsgDXFViewportAccess(FEntity);
  vViewport.ClippingBoundary := AReader.ObjByHandle(FClippingBoundaryHandle, nil);
  vViewportEntityHeader := TsgDXFViewportEntityHeader(AReader.ObjByHandle(FEntHeaderHandle, nil));
  if Assigned(vViewportEntityHeader) then
    vViewport.TurnsOff := vViewportEntityHeader.Flags and 1 = 0;
end;

{ TdwgViewportEntityHeader }

procedure TdwgViewportEntityHeader.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FFlags := FFlags or ReadBit(APBit);
  ReadHandleRefs(APBit);
  ReadID(APBit);// viewport entity
  ReadID(APBit);//objhandle of previous vport ent header in chain (hard pointer)
  //sometimes points to self; change those to NULL.
  //NULL indicates end of chain.
end;

procedure TdwgViewportEntityHeader.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  FEntity.Name := FEntryName;
  FEntity.Flags := FFlags;
end;

{TdwgWipeOut}

{protected}

procedure TdwgWipeOut.ReadEntData(var APBit: TsgInt64);
var
  vPoint: TFPoint;
  I: Integer;
  vWipeout: TsgCADWipeout;
  //vIsClipInverted: Boolean;
  vClsVer, vCount: Integer;
begin
  vWipeout := TsgCADWipeout(FEntity);
  vClsVer := ReadBL(APBit); //SeekGeneral(APBit,[seBL]); //  (90)  Class version
  if (vClsVer >= 500) and (FReader.VarCode(vClsVer) = 54) then
    ReadEntDataEx(APBit)
  else
  begin
    vWipeout.Point := Read3D(APBit);
    vWipeout.UVector := Read3D(APBit);
    vWipeout.VVector := Read3D(APBit);
    vWipeout.Size := Read2RD(APBit);
    vWipeout.Flags := ReadBS(APBit);
    vWipeout.UseClipping := (ReadBit(APBit) = 1);
    // |  (281) Brightness value (0-100; default = 50)
    // |  (282) Contrast value (0-100; default = 50)
    // v  (283) Fade value (0-100; default = 50)
    SeekGeneral(APBit, [seRC,seRC,seRC]);
    if Version > acR2007 then
      SeekGeneral(APBit, [seB]); //vIsClipInverted := ReadBit(APBit) <> 0;
    vWipeout.ClippingBoundaryType := ReadBS(APBit);
    case vWipeout.ClippingBoundaryType of
      1:
        begin
          vPoint := Read2RD(APBit);
          vWipeout.AddPoint(vPoint.X, vPoint.Y);
          vPoint := Read2RD(APBit);
          vWipeout.AddPoint(vPoint.X, vPoint.Y);
        end;
      2:
        begin
          vCount := ReadBL(APBit);
          for I := 0 to vCount - 1 do
          begin
            vPoint := Read2RD(APBit);
            vWipeout.AddPoint(vPoint.X, vPoint.Y);//  (14,24) Clip boundary vertex
          end;
        end;
    end;
  end;
end;

procedure TdwgWipeOut.ReadEntDataEx(var APBit: TsgInt64);
var
  vPoint: TFPoint;
  vWipeout: TsgCADWipeout;
  //vIsClipInverted: Boolean;
  vCount, vCode: Integer;
begin
  vWipeout := TsgCADWipeout(FEntity);
  Inc2(APBit, 103); // ??
  vCount := 1;
  vCode := 1;
  while Comp2(APBit, FDataEnd) and (vCount <> 0) and (vCode <> 0) do
  begin
    vCode := ReadBS(APBit);
    case vCode of
      10: vWipeout.Point := Read3D(APBit);
      11: vWipeout.UVector := Read3D(APBit);
      12: vWipeout.VVector := Read3D(APBit);
      13: vWipeout.Size := Read3D(APBit);
      14:
        begin
          vPoint := Read3D(APBit);
          vWipeout.AddPoint(vPoint.X, vPoint.Y);
          Dec(vCount);
        end;
      70: vWipeout.Flags := ReadBS(APBit);
      71: vWipeout.ClippingBoundaryType := ReadBS(APBit);
      91: vCount := ReadBL(APBit);
      100:
        begin
          ReadText; // 'cn:AcDb:classname'
          ReadText; // 'AcDb:classname'
        end;
      280: vWipeout.UseClipping := (ReadBS(APBit) = 1);
      281..283: ReadBS(APBit); //Brightness value; Contrast value; Fade value
      290: if Version > acR2007 then SeekGeneral(APBit, [seB]);
      340:;
      360:;
    end;
  end;
end;

{TdwgAcadTable}

{protected}

procedure TdwgAcadTable.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FBlockRecord := ReadID(APBit);
  FTableStyle := ReadID(APBit);
end;

procedure TdwgAcadTable.ReadEntData(var APBit: TsgInt64);
var
  vScaleFlags: Integer;
  vScale: TFPoint;
  vAcadTable: {$IFDEF SG_DWG_IMPORT_ACAD_TABLE}TsgDXFAcadTable{$ELSE}TsgDXFInsert{$ENDIF};
{$IFDEF SG_DWG_IMPORT_ACAD_TABLE}
  vTableFlags: Integer;
{$ENDIF}
begin
  vAcadTable := {$IFDEF SG_DWG_IMPORT_ACAD_TABLE}TsgDXFAcadTable{$ELSE}TsgDXFInsert{$ENDIF}(FEntity);
  vAcadTable.Point := Read3D(APBit);
  if Version <= acR14 then
    vAcadTable.Scale := Read3RD(APBit)
  else
  begin
    vScaleFlags := ReadBB(APBit);
    case vScaleFlags of
      3:; // scale 1,1,1
      1:
        begin
          vScale.X := 1;
          vScale.Y := ReadDD(APBit, vScale.X);
          vScale.Z := ReadDD(APBit, vScale.X);
          vAcadTable.Scale := vScale;
        end;
      2:
        begin
          vScale.X := ReadRD(APBit);
          vScale.Y := vScale.X;
          vScale.Z := vScale.Y;
          vAcadTable.Scale := vScale;
        end;
      0:
        begin
          vScale.X := ReadRD(APBit);
          vScale.Y := ReadDD(APBit, vScale.X);
          vScale.Z := ReadDD(APBit, vScale.X);
          vAcadTable.Scale := vScale;
        end;
    end;
  end;
  vAcadTable.Angle := ReadBD(APBit) * f180DividedByPi;
  vAcadTable.Extrusion := ReadExtrusion(APBit);
  FHasAttribs := ReadBit(APBit) <> 0;
  if Version > acR2004 then
    FObjectsCount := ReadBL(APBit);
{$IFDEF SG_DWG_IMPORT_ACAD_TABLE}
  vTableFlags := ReadBS(APBit);
  { TODO: read acadtable content }
{$ENDIF}
  inherited ReadEntData(APBit);
end;

procedure TdwgAcadTable.References(AReader: TdwgReader);
{$IFDEF SG_DWG_IMPORT_ACAD_TABLE}
var
  vTableStyle: TsgDXFEntity;
{$ENDIF}
begin
  inherited References(Reader);
  TsgDXFAcadTable(FEntity).BlockRecord :=
    TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecord, TsgDXFBlockRecord));
{$IFDEF SG_DWG_IMPORT_ACAD_TABLE}
  vTableStyle := AReader.ObjByHandle(FTableStyle, TsgDXFAcadTableStyle);
  TsgDXFAcadTable(FEntity).Style := TsgDXFAcadTableStyle(vTableStyle);
{$ENDIF}
end;

{ TdwgAcadTableStyle }

function TdwgAcadTableStyle.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Dictionary[cnstObjectsTABLESTYLES];
end;

procedure TdwgAcadTableStyle.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  J: TsgAcadTableCellType;
  vAcTblStyle: TsgDXFAcadTableStyle;
  vCellStyle: TsgAcadTableCellStyle;
  vSmallInt: Smallint;
begin
  { TODO: TdwgAcadTableStyle.ReadData ac2010 }
  if Version >= acR2010 then
    Exit;
  ReadObjHeader(APBit);
  vAcTblStyle := TsgDXFAcadTableStyle(FEntity);
  vAcTblStyle.Description := ReadText;
  vAcTblStyle.FlowDirection := ReadBS(APBit);
  vAcTblStyle.Flags := ReadBS(APBit);
  vAcTblStyle.HorzCellMargin := ReadBD(APBit);
  vAcTblStyle.VertCellMargin := ReadBD(APBit);
  vAcTblStyle.TitleSuppressed := ReadBit(APBit) <> 0;
  vAcTblStyle.HeaderSuppressed := ReadBit(APBit) <> 0;
  for J := Low(TsgAcadTableCellType) to High(TsgAcadTableCellType) do
  begin
    vCellStyle := vAcTblStyle.CellStyle[J];
    vCellStyle.TextHeight := ReadBD(APBit);
    vCellStyle.CellAlignment := ReadBS(APBit);
    vCellStyle.TextColor := ReadColor(APBit);
    vCellStyle.FillColor := ReadColor(APBit);
    vCellStyle.BkColorOn := ReadBit(APBit) <> 0;
    for I := 0 to vCellStyle.BorderCount - 1 do
    begin
      vSmallInt := ReadBS(APBit);
      if vSmallInt < 0 then
        vCellStyle.BorderLineWeight[I] := vSmallInt
      else
        vCellStyle.BorderLineWeight[I] := 0.01 * vSmallInt;
      vCellStyle.BorderVisible[I] := ReadBit(APBit) <> 0;
      vCellStyle.BorderColor[I] := ReadColor(APBit);
    end;
    if Version >= acR2007 then
    begin
      SeekGeneral(APBit, [seBL, seBL]);
      ReadText;
    end;
  end;
  ReadHandleRefs(APBit);
  FDataTextStyle := ReadID(APBit);
  FTitleTextStyle := ReadID(APBit);
  FHeaderTextStyle := ReadID(APBit);
end;

procedure TdwgAcadTableStyle.References(AReader: TdwgReader);
var
  vAcTblStyle: TsgDXFAcadTableStyle;
begin
  inherited References(AReader);
  { TODO: TdwgAcadTableStyle.References ac2010 }
  if Version >= acR2010 then
    Exit;
  vAcTblStyle := TsgDXFAcadTableStyle(FEntity);
  vAcTblStyle.CellStyle[cttData].TextStyle :=
    TsgDXFStyle(AReader.ObjByHandle(FDataTextStyle, TsgDXFStyle));
  vAcTblStyle.CellStyle[cttTitle].TextStyle :=
    TsgDXFStyle(AReader.ObjByHandle(FTitleTextStyle, TsgDXFStyle));
  vAcTblStyle.CellStyle[cttHeader].TextStyle :=
    TsgDXFStyle(AReader.ObjByHandle(FHeaderTextStyle, TsgDXFStyle));
end;

{TdwgACISEntity}

{protected}

procedure TdwgACISEntity.ReadEntData(var APBit: TsgInt64);
var
  vString, vBuffer: AnsiString;
  N, vStartPos, vLength, vLenEnd, vIndex: Integer;
  vCode: Byte;
  vEncoded: Boolean;
  vDataItem: TsgDataItem;
  vSaveVer: Word;
  vAcisStream: TMemoryStream;
begin
  vBuffer := '';
  try
    vAcisStream := TsgBrepModAcis(FEntity).QueryAcisStream;
    vAcisStream.Clear;
    if (Version >= acR2013) and Assigned(FReader.FSegmentedFile) then
    begin
      vIndex := FReader.FSegmentedFile.IndexOf(FHandle);
      if vIndex < 0 then
        Exit;
      vDataItem := TsgDataItem(FReader.FSegmentedFile.DataItems[vIndex]);
      if not (vDataItem is TsgACISItem) then
        Exit;
      vAcisStream.CopyFrom(vDataItem.Stream, 0);
      vAcisStream.Position := 0;
      Exit;
    end;
    if ReadBit(APBit) = 1 then
      Exit;
    ReadBit(APBit);
    vSaveVer := ReadBS(APBit);
    vEncoded := vSaveVer = 1;
    if vEncoded then
    begin
      while True do
      begin
        N := ReadBL(APBit);
        if N = 0 then
          Break;
        while N > 0 do
        begin
          Dec(N);
          vCode := ACIS_Cipher(ReadRC(APBit));
          vAcisStream.Write(vCode, 1);
        end;
      end;
    end
    else
    begin
      vStartPos := 1;
      while True do
      begin
        N := 1024;
        vString := '';
        while N > 0 do
        begin
          Dec(N);
          vCode := ReadRC(APBit);
          vString := vString + AnsiChar(Chr(vCode));
        end;
        vBuffer := vBuffer + vString;

        vLenEnd := Length(cnstACISEndSignatureAutocad);
        N := StringPosA(cnstACISEndSignatureAutocad, vBuffer, vStartPos);
        if N = 0 then
        begin
          N := StringPosA(cnstACISEndSignatureSpatial, vBuffer, vStartPos);
          vLenEnd := Length(cnstACISEndSignatureSpatial);
        end;
        if N > 0 then
        begin
          Dec2(APBit, (Length(vBuffer ) - N - vLenEnd) shl 3);
          //SetLength(vBuffer, N + vLenEnd - 1);
          Break;
        end;
        vLength := Length(vBuffer);
        if vLength > Length(cnstACISEndSignatureAutocad) then
          vStartPos := vLength - Length(cnstACISEndSignatureAutocad) - 1;
      end;
      vAcisStream.Write(vBuffer[1], Length(vBuffer));
      //if not LoadSatFromMemory(@vBuffer[1], Length(vBuffer)) then Exit;
    end;
  finally
    inherited ReadEntData(APBit);
  end;
end;

{TdwgArc}

{protected}

procedure TdwgArc.ReadEntData(var APBit: TsgInt64);
begin
  inherited ReadEntData(APBit);
  TsgDXFArc(FEntity).StartAngle := ReadBD(APBit) * f180DividedByPi;
  TsgDXFArc(FEntity).EndAngle := ReadBD(APBit) * f180DividedByPi;
end;

{TdwgAttdef}

{protected}

procedure TdwgAttdef.ReadPrompt(var APBit: TsgInt64);
begin
  if Version >= acR2010 then
    FAttVersion := ReadRC(APBit);
  ReadText;
end;

procedure TdwgAttdef.References(AReader: TdwgReader);
begin
  ApplyFlags;
  inherited References(AReader);
  if Assigned(FMText) then
  begin
    TsgDXFTextAccess(FEntity).MTextRef := TsgDXFMText(FMText.FEntity);
    FMText.FEntity := nil;
  end;
end;

function TdwgAttdef.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and Assigned(AEEData) and Reader.IsAcDbAttrAppID(AAppID) then
  begin
    Reader.ConvExtDataToDXF(AEEData);
    FEntity.SetExtData(AEEData);
    Result := True;
  end;
end;

procedure TdwgAttdef.ApplyFlags;
begin
  if FAttributeType in [atAttribMLine, atAttdefMLine] then
    TsgDXFAttdef(FEntity).TypeValue := atMText;
  TsgDXFAttdef(FEntity).Flags := FFlags;
end;

procedure TdwgAttdef.ApplyValue;
begin
  TsgDXFAttdef(FEntity).Tag := FTag;
  TsgDXFAttdef(FEntity).Value := FText;
end;

destructor TdwgAttdef.Destroy;
begin
  if Assigned(FMText) then
  begin
    FMText.FStringReader := nil;
    FMText.FEntity.Free;
    FMText.Free;
  end;
  inherited Destroy;
end;

procedure TdwgAttdef.ReadEntData(var APBit: TsgInt64);
begin
  inherited ReadEntData(APBit);
  if Version > acR2007 then
    FAttVersion := ReadRC(APBit);//raise an exception if result is not zero
  if Version >= acR2018 then
  begin
    case ReadRC(APBit) of
      1: FAttributeType := atSingleLine;
      2: FAttributeType := atAttribMLine;
      4: FAttributeType := atAttdefMLine;
    end;
    if FAttributeType in [atAttribMLine, atAttdefMLine] then
      ReadMTextFields(APBit);
  end;
  FTag := ReadText;
  SeekGeneral(APBit, [seBS]);//Field length BS 73 unused
  FFlags := ReadRC(APBit);//RC 70: 0 = None, 1 = Invisible, 2 = Constant, 4 = Input verification required, 8 = Preset
  if Version >= acR2007 then
    TsgDXFAttdef(FEntity).LockPosition := ReadBit(APBit) <> 0;
  ReadPrompt(APBit);
end;

procedure TdwgAttdef.ReadMTextFields(var APBit: TsgInt64);
begin
  FMText := TdwgEmbeddedMText.Create(Version);
  FMText.Reader := Reader;
  FMText.FEntity := TsgDXFMText.Create;
  FMText.FStringReader := FStringReader;
  FMText.FHndsStart := FHndsStart;
  FMText.FPHnds := FPHnds;
  FMText.FStrsStart := FStrsStart;
  FMText.FDataEnd := FDataEnd;
  Reader.Conv.DoCreate(FMText.FEntity);

  FMText.ReadError; // do not read common handle data values
  Reader.DoReadData(FMText, APBit);
  Inc2(APBit, ReadBS(APBit) shl 3); //?? Annotative data size BS
  //Annotative data bytes RC Byte array with length Annotative data size.
  //Registered application H Hard pointer.
  //Unknown BS 72? Value 0.
end;

{TdwgAttrib}

{protected}

function TdwgAttrib.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
{$IFDEF SG_BTI}
var
  vExtData: TsgCADExtendedData;
  vEntity: TsgDXFEntity;
{$ENDIF}
begin
  Result := inherited ApplyEED(AEEData, AAppID);
{$IFDEF SG_BTI}
  if not Result and Assigned(AEEData) and Reader.IsRelatedAppID(AAppID) then
  begin
    if Reader.ConvExtDataToDXF(AEEData) and Reader.FSGSaved then
    begin
      vExtData := AEEData;
      vEntity := CreateRegEntity(cnstGroupTypeName[gtBTI], Ord(ceAttrib),
        Reader.Conv, vExtData);
      if Assigned(vEntity) then
      begin
        vEntity.Clone(FEntity);
        FEntity.Free;
        FEntity := vEntity;

        TsgDXFEntityAccess(FEntity).SetConverter(Reader.Conv);
        Reader.FObjMap.Objects[FMapIndex] := FEntity;
        Result := True;
      end;
    end;
  end;
{$ENDIF}
end;

procedure TdwgAttrib.ReadPrompt(var APBit: TsgInt64);
begin
end;

{TdwgDimension}

{protected}

procedure TdwgDimension.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FStyle := FBlockRecord;
  FBlockRecord := ReadID(APBit);
end;

procedure TdwgDimension.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
  vFlags: Integer;
begin
  vDimension := TsgDXFDimension(FEntity);
  if Version > acR2007 then
    SeekGeneral(APBit, [seRC]); //if ReadRC(APBit) <> 0 then raise
  vDimension.Extrusion := ReadExtrusion(APBit);
  vDimension.MiddlePoint := Read2RD(APBit);
  vDimension.Elevation := ReadBD(APBit);
  vFlags := ReadRC(APBit);
  if vFlags and 1 = 0 then
    vDimension.Flags := 128;
  if vFlags and 2 <> 0 then
    vDimension.Flags := vDimension.Flags or 32;
  vDimension.TextOverride := ReadText;
  SeekGeneral(APBit, [se2D]);
  vDimension.Scale := _Read3D(APBit);
  vDimension.Angle := ReadBD(APBit) * f180DividedByPi;
  if Version >= acR2000 then
    SeekGeneral(APBit, [seBS,seBS,se2D]);
  if Version >= acR2007 then //b73
    SeekGeneral(APBit, [seB,seB,seB]);
  vDimension.Point := Read2RD(APBit);
end;

procedure TdwgDimension.References(AReader: TdwgReader);
var
  S: string;
begin
  inherited References(Reader);
  TsgDXFDimension(FEntity).Style :=
    TsgDXFDimensionStyle(AReader.ObjByHandle(FStyle, TsgDXFDimensionStyle));
  if Assigned(FACADExtData) then
  begin
    S := AReader.GetTableNameByHandle(FACADExtData.DataInt64[0], nil);
    FACADExtData.ChangeType(0, edtString);
    FACADExtData.DataString[0] := S;
    FEntity.SetExtData(FACADExtData);
    FreeAndNil(FACADExtData);
  end;
end;

{TdwgImageEnt}

{protected}

procedure TdwgImageEnt.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FImageDef := ReadID(APBit);
end;

procedure TdwgImageEnt.References(AReader: TdwgReader);
begin
  inherited References(Reader);
  TsgDXFImageEnt(FEntity).ImageDef :=
    TsgDXFImageDef(AReader.ObjByHandle(FImageDef, TsgDXFImageDef));
end;

{TdwgMesh}

{protected}

procedure TdwgPolyMesh.ReadEntData(var APBit: TsgInt64);
var
  vPolyline: TsgDXFPolyline;
  I: Integer;
begin
  vPolyline := TsgDXFPolyline(FEntity);
  vPolyline.Flags := ReadBS(APBit);
  vPolyline.Closed := vPolyline.Flags and 1 <> 0;
  SeekGeneral(APBit, [seBS]);
  vPolyline.MeshM := ReadBS(APBit);
  vPolyline.MeshN := ReadBS(APBit);
  I := ReadBS(APBit);
  if I <> 0 then
    vPolyline.MeshM := I;
  I := ReadBS(APBit);
  if I <> 0 then
    vPolyline.MeshN := I;
  if Version >= acR2004 then
    FSubEnts := ReadBL(APBit);
end;

{TdwgMInsert}

{protected}

procedure TdwgMInsert.ReadEntData(var APBit: TsgInt64);
var
  vMInsert: TsgCADMInsert;
begin
  inherited ReadEntData(APBit);
  vMInsert := TsgCADMInsert(FEntity);
  vMInsert.NumCols := ReadBS(APBit);
  vMInsert.NumRows := ReadBS(APBit);
  vMInsert.ColSpacing := ReadBD(APBit);
  vMInsert.RowSpacing := ReadBD(APBit);
end;

{TdwgPFace}

{protected}

procedure TdwgPFace.ReadEntData(var APBit: TsgInt64);
var
  vPolyline: TsgDXFPolyline;
begin
  vPolyline := TsgDXFPolyline(FEntity);
  vPolyline.Flags := 64;
  vPolyline.MeshM := ReadBS(APBit);
  vPolyline.MeshN := ReadBS(APBit);
  if Version >= acR2004 then
    FSubEnts := ReadBL(APBit);
end;

{TdwgPolyline3D}

{protected}

procedure TdwgPolyline3D.ReadEntData(var APBit: TsgInt64);
begin
  SeekGeneral(APBit, [seRC]);
  TsgDXFPolyline(FEntity).Polyline3D := True;
  TsgDXFPolyline(FEntity).Closed := ReadRC(APBit) and 1 <> 0;
  if Version >= acR2004 then
    FSubEnts := ReadBL(APBit);
end;

{TdwgProxy}

{protected}

procedure TdwgProxy.DoReadEntProxyData(var APBit: TsgInt64);
begin
  if Version > acR2007 then //Length of the proxy data
    SeekGeneral(APBit, [seUInt64])
  else
    SeekGeneral(APBit,[seRL]);
  FReader.ProxyReaderImpl.Reader.UpdatePosition(@APBit);
  FReader.ProxyReaderImpl.Read(FEntity);
end;

procedure TdwgProxy.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  FReader.ProxyReaderImpl.Apply(FEntity);
end;

{TdwgShape}

{protected}

procedure TdwgShape.ReadEntData(var APBit: TsgInt64);
begin
  FPoint := Read3D(APBit);
  FHeight := ReadBD(APBit);// Scale
  FRotation := ReadBD(APBit) * f180DividedByPi;
  FScale := ReadBD(APBit);// Width factor
  FObliqueAngle := ReadBD(APBit) * f180DividedByPi;
  SeekGeneral(APBit,[seBD]);
  FShapeNumber := ReadBS(APBit);
  FExtrusion := ReadExtrusion(APBit);
end;

function TdwgShape.ReadExtrusion(var APBit: TsgInt64): TFPoint;
begin
  DoReadExtrusion(APBit, Result);
end;

procedure TdwgShape.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgDXFShape(FEntity).ShapeNumber := FShapeNumber;
end;

{TdwgVertex2D}

{protected}

procedure TdwgVertex2D.ReadEntData(var APBit: TsgInt64);
var
  vVertex: TsgDXFVertex;
  vStartWidth: Double;
begin
  inherited ReadEntData(APBit);
  vVertex := TsgDXFVertex(FEntity);
  vStartWidth := ReadBD(APBit);
  if vStartWidth < 0.0 then
  begin
    vVertex.StartWidth := Abs(vStartWidth);
    vVertex.EndWidth := vVertex.StartWidth;
  end
  else
  begin
    vVertex.StartWidth := vStartWidth;
    vVertex.EndWidth := ReadBD(APBit);
  end;
  vVertex.Bulge := ReadBDCheck(APBit);
  if Version >= acR2010 then
    SeekGeneral(APBit, [seBL]); // Vertex ID
  SeekGeneral(APBit, [seBD]);
end;

{TdwgVertexFace}

{protected}

procedure TdwgVertexFace.ReadEntData(var APBit: TsgInt64);
var
  vVertex: TsgDXFVertex;
begin
  vVertex := TsgDXFVertex(FEntity);
  vVertex.Flags := 128;
  vVertex.PolyFaceVertexIndex1 := ReadBS(APBit);
  vVertex.PolyFaceVertexIndex2 := ReadBS(APBit);
  vVertex.PolyFaceVertexIndex3 := ReadBS(APBit);
  vVertex.PolyFaceVertexIndex4 := ReadBS(APBit);
end;

{TdwgDimAligned}

{protected}

procedure TdwgDimAligned.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.LinDefPoint1 := Read3D(APBit);
  vDimension.LinDefPoint2 := Read3D(APBit);
  vDimension.DefPoint := Read3D(APBit);
  SeekGeneral(APBit,[seBD]);
  vDimension.Flags := vDimension.Flags or 1;
end;

{TdwgDimAngular2L}

{protected}

procedure TdwgDimAngular2L.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.ArcDefPoint := Read2RD(APBit);
  vDimension.LinDefPoint1 := Read3D(APBit);
  vDimension.LinDefPoint2 := Read3D(APBit);
  vDimension.RadDefPoint := Read3D(APBit);
  vDimension.DefPoint := Read3D(APBit);
  vDimension.Flags := vDimension.Flags or 2;
end;

{TdwgDimAngular3P}

{protected}

procedure TdwgDimAngular3P.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.DefPoint := Read3D(APBit);
  vDimension.LinDefPoint1 := Read3D(APBit);
  vDimension.LinDefPoint2 := Read3D(APBit);
  vDimension.RadDefPoint := Read3D(APBit);
  vDimension.Flags := vDimension.Flags or 5;
end;

{TdwgDimLinear}

{protected}

procedure TdwgDimLinear.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.LinDefPoint1 := Read3D(APBit);
  vDimension.LinDefPoint2 := Read3D(APBit);
  vDimension.DefPoint := Read3D(APBit);
  SeekGeneral(APBit, [seBD]);
  vDimension.DimRot := ReadBD(APBit) * f180DividedByPi;
end;

{TdwgDimOrdinate}

{protected}

procedure TdwgDimOrdinate.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
  vFlags: Integer;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.DefPoint := Read3D(APBit);
  vDimension.LinDefPoint1 := Read3D(APBit);
  vDimension.LinDefPoint2 := Read3D(APBit);
  vFlags := ReadRC(APBit);
  if vFlags and 1 <> 0 then
    vDimension.Flags := vDimension.Flags or 64;
  vDimension.Flags := vDimension.Flags or 6;
end;

{TdwgDimRadius}

{protected}

procedure TdwgDimRadius.ReadEntData(var APBit: TsgInt64);
var
  vDimension: TsgDXFDimension;
begin
  vDimension := TsgDXFDimension(FEntity);
  inherited ReadEntData(APBit);
  vDimension.DefPoint := Read3D(APBit);
  vDimension.RadDefPoint := Read3D(APBit);
  SeekGeneral(APBit,[seBD]);
  SetFlags;
end;

procedure TdwgDimRadius.SetFlags;
begin
  TsgDXFDimension(FEntity).Flags := TsgDXFDimension(FEntity).Flags or 4;
end;

{TdwgDimDiameter}

{protected}

procedure TdwgDimDiameter.SetFlags;
begin
  TsgDXFDimension(FEntity).Flags := TsgDXFDimension(FEntity).Flags or 3;
end;

{ TdwgArcDimension }

procedure TdwgArcDimension.ReadEntData(var APBit: TsgInt64);
begin
  inherited ReadEntData(APBit);
  SeekGeneral(APBit, [seB, seBD, seBD, seB, se3D, se3D]);
  TsgDXFDimension(FEntity).Flags := TsgDXFDimension(FEntity).Flags or 7;
end;

{ TdwgDictionary }

function TdwgDictionary.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if Assigned(Result) then
  begin
    if Result.Handle <> AReader.FNamedObjects then
    begin
      TsgDXFEntityAccess(FEntity).SetOwner(Reader.Conv.Sections[csObjects]);
      TsgDXFConverterAccess(AReader.Conv).SetEntDict(Result, FEntity);
    end;
    if (Result.ClassType = TsgDXFBlockRecord) or (Result.ClassType = TsgDXFLayout) or
       (Result.ClassType = TsgDXFLayer) or (Result.ClassType = TsgDXFBlockRecords) or
       (FOwner = AReader.FLayoutsDict) or
       (FOwner = AReader.FLayerControl) or
       (FOwner = AReader.FTableStyleDict) then
      Result := AReader.Conv.Sections[csObjects];
  end
  else
  begin
    if FReactors.Count > 0 then
      Result := AReader.ObjByHandle(FReactors.First, nil);
    if not Assigned(Result) then
      Result := AReader.Conv.Sections[csObjects];
  end;
end;

procedure TdwgDictionary.NewObject(ACode: Integer; AHandle: UInt64;
  var AObj: TdwgObject);
begin
  inherited NewObject(ACode, AHandle, AObj);
  if Assigned(AObj) then
  begin
    FNames[FChildIndex] := Reader.GlobalName[FNames[FChildIndex]];
    AObj.FEntity.Name := FNames[FChildIndex];
    if (FHandle = Reader.FMaterialDict) and AObj.InheritsFrom(TdwgMaterial) then
    begin
      TdwgMaterial(AObj).FOwnerDictionary := Self;
      TdwgMaterial(AObj).FOwnerObject := FEntity;
      TdwgMaterial(AObj).FOwner := FHandle;
    end
    else
      if AObj.InheritsFrom(TdwgDictionaryItem) then
        TdwgDictionaryItem(AObj).FOwnerDictionary := Self;
  end;
end;

function TdwgDictionary.OwnerAdd(AOwner: TsgDXFEntity): Integer;
begin
  if Assigned(AOwner) then
    if not AOwner.InheritsFrom(TsgDXFTable) or
       (AOwner.Handle = Reader.FMLStylesDict) or
       (AOwner.Handle = Reader.FPlotSettingsDict) or
       (AOwner.Handle = Reader.FVPortControl) or
       (AOwner.Handle = Reader.FMLeaderStylesDict) or
       (AOwner.Handle = Reader.FTableStyleDict) then
      AOwner := Reader.Conv.Sections[csObjects];
  Result := inherited OwnerAdd(AOwner);
end;

procedure TdwgDictionary.PerformName;

  function UpdateEntName(const AHandle: UInt64; const AName: string): Boolean;
  begin
    Result := FHandle = AHandle;
    if Result then
      FEntity.Name := AName;
  end;

begin
  if not UpdateEntName(Reader.FLayoutsDict, sAcadLayoutDictionary) then
  if not UpdateEntName(Reader.FMLStylesDict, sAcadMLineStyleDictionary) then
  if not UpdateEntName(Reader.FImageDefDict, sAcadImageDict) then
  if not UpdateEntName(Reader.FVariableDict, sAcDbVariableDictionary) then
  if not UpdateEntName(Reader.FColors, sAcadColor) then
  if not UpdateEntName(Reader.FMaterialDict, sAcadMaterialDictionary) then
  if not UpdateEntName(Reader.FPlotSettingsDict, sAcadPlotSettingsDictionary) then
  if not UpdateEntName(Reader.FMLeaderStylesDict, sAcadMLeaderStyleDictionary) then
  if not UpdateEntName(Reader.FGroups, sAcadGroupDictionary) then
  if not UpdateEntName(Reader.FTableStyleDict, sAcadTableStyleDictionary) then
  if not UpdateEntName(Reader.FIrdDscDict, sIrdDscDictionary) then
  if FEntity.Name = '' then FEntity.Name := DictionaryName;
end;

procedure TdwgDictionary.ReadData(var APBit: TsgInt64);
var
  I: Integer;
//  vHardOwnerFlag: Integer;
begin
  ReadObjHeader(APBit);
  FSubEnts := ReadBL(APBit);
  if Version = acR14 then
    SeekGeneral(APBit, [seRC]);
  if Version >= acR2000 then
  begin
    FCloningFlag := ReadBS(APBit);
    SeekGeneral(APBit, [seRC]);   //vHardOwnerFlag := ReadRC(APBit);
    if FCloningFlag = 256 then
      SeekGeneral(APBit, [seRC]); //?? not documented
  end;
  if FSubEnts > 0 then
  begin
    SetLength(FNames, FSubEnts);
    ForceChildrenList(FSubEnts);
  end;
  for I := 0 to FSubEnts - 1 do
    FNames[I] := ReadText;
  ReadHandleRefs(APBit);
  ReadSubEntIDs(APBit);
end;

procedure TdwgDictionary.ReadSubEntIDs(var APBit: TsgInt64;
  const AFlags: Integer);
var
  I: Integer;

  function UpdateHandle(var AHandle: UInt64; const AName: string): Integer;
  begin
    if (AHandle = cnstBadHandle) and Assigned(FNames) then
    begin
      Result := IndexOfStrings(AName, FNames, {$IFDEF SGDEL_10_SEATTLE}SysUtils.{$ENDIF}CompareText);
      if Result >= 0 then
        AHandle := Children[Result];
    end
    else
      Result := -1;
  end;

begin
  for I := 0 to FSubEnts - 1 do
    ChildrenAdd(ReadID(APBit));
  if FHandle = Reader.FNamedObjects then
  begin
    UpdateHandle(Reader.FLayoutsDict, sAcadLayoutDictionary);
    UpdateHandle(Reader.FMLStylesDict, sAcadMLineStyleDictionary);
    UpdateHandle(Reader.FImageDefDict, sAcadImageDict);
    UpdateHandle(Reader.FVariableDict, sAcDbVariableDictionary);
    UpdateHandle(Reader.FColors, sAcadColor);
    UpdateHandle(Reader.FMaterialDict, sAcadMaterialDictionary);
    UpdateHandle(Reader.FPlotSettingsDict, sAcadPlotSettingsDictionary);
    UpdateHandle(Reader.FMLeaderStylesDict, sAcadMLeaderStyleDictionary);
    UpdateHandle(Reader.FGroups, sAcadGroupDictionary);
    UpdateHandle(Reader.FTableStyleDict, sAcadTableStyleDictionary);
    UpdateHandle(Reader.FIrdDscDict, sIrdDscDictionary);
  end;
  PerformName;
end;

procedure TdwgDictionary.ReadSubEnts(AReader: TdwgReader);
begin
  ReadSubEntsR2004(AReader);
end;

procedure TdwgDictionary.References(AReader: TdwgReader);
var
  I, J, C: Integer;
  vEnt: TsgDXFEntity;
  vObjLoadedFirst: Boolean;
begin
  inherited References(AReader);
  FEntity.Flags := FCloningFlag;
  C := ChildrenCount;
  if C > 0 then
    if FEntity.Handle <> AReader.FLayoutsDict then
    begin
      vObjLoadedFirst := AReader.FObjMap.ObjectLoadedCount[FMapIndex] <= 0;
      for I := 0 to C - 1 do
      begin
        vEnt := nil;
        J := AReader.FObjMap.IndexOfHandle(Children[I]);
        if J >= 0 then
          vEnt := TsgDXFEntity(AReader.FObjMap.MapElement[J].Obj);
        if Assigned(vEnt) and (vEnt.Owner = FEntity) then
          vEnt.Name := FNames[I]
        else
          if vObjLoadedFirst and (J >= 0) then
            AReader.FObjMap.Name[J] := FNames[I];
      end;
    end;
  PerformName;
end;

{ TdwgDbColor }

function TdwgDbColor.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
  if not Assigned(Result) then
    Result := AReader.Conv.Sections[csObjects];
end;

procedure TdwgDbColor.ReadData(var APBit: TsgInt64);
var
  vColorCAD: TsgColorCAD;
begin
  ReadObjHeader(APBit);
  vColorCAD := ReadColor(APBit);
  //vColorCAD.AlbumString := Self.FColorName;
  FEntity.ColorCAD := vColorCAD;
  ReadHandleRefs(APBit);
end;

{ TdwgAppID }

procedure TdwgAppID.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FEntity.Name := FEntryName;
  FEntity.Annotative := IsAnnotative(FEntryName);
  TsgDXFPenTableItem(FEntity).Flags := FFlags;
  SeekGeneral(APBit, [seRC]); // Unknown
  ReadHandleRefs(APBit);
end;

procedure TdwgLTControl.ReadData(var APBit: TsgInt64);
var
  I: Integer;
begin
  InitializeTable(csLTypes, cnstTableLTYPE);
  inherited ReadData(APBit);
  FByBlock := ReadID(APBit); // BYBLOCK hard owner
  FByLayer := ReadID(APBit); // BYLAYER hard owner

  if not Assigned(FChildren) then
    ForceChildrenList;
  if not Find(Reader.FLTContinious, I) then
    ChildrenAdd(Reader.FLTContinious);
  if not Find(FByBlock, I) then
    ChildrenAdd(FByBlock);
  if not Find(FByLayer, I) then
    ChildrenAdd(FByLayer);
end;

procedure TdwgLTControl.References(AReader: TdwgReader);

  function PopEntByHandle(const AHandle: UInt64): TsgDXFEntity;
  begin
    Result := FEntity.FindEntByHandle(AHandle);
    if Assigned(Result) then
    begin
      FEntity.RemoveEntity(Result);
      FEntity.InsertEntity(0, Result);
    end;
  end;

begin
  inherited References(AReader);
  TsgDXFGroupAccess(FEntity).SortEntByHandle;
  PopEntByHandle(AReader.FLTContinious);
  PopEntByHandle(FByLayer);
  PopEntByHandle(FByBlock);
end;

{ TdwgLayersControl }

procedure TdwgLayerControl.ReadData(var APBit: TsgInt64);
begin
  InitializeTable(csLayers, cnstTableLAYER);
  inherited ReadData(APBit);
end;

procedure TdwgLayerControl.References(AReader: TdwgReader);
var
  vLayer0: TsgDXFEntity;
begin
  inherited References(AReader);
  TsgDXFGroupAccess(FEntity).SortEntByHandle;
  vLayer0 := FEntity.FindEntByName('0');
  if not Assigned(vLayer0) then
  begin
    vLayer0 := TsgDXFLayer.Create;
    vLayer0.Name := '0';
    TsgDXFConverterAccess(AReader.Conv).SetHandle(vLayer0);
  end
  else
    FEntity.RemoveEntity(vLayer0);
  FEntity.InsertEntity(0, vLayer0);
end;

{ TdwgStyleControl }

function TdwgStyleControl.CompareEntHandles(const A, B: Pointer): Integer;
begin
  Result := sgCompareHandles(TsgDXFEntity(A^).Handle, TsgDXFEntity(B^).Handle);
end;

constructor TdwgStyleControl.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FSearchStyle := TsgDXFStyle.Create;
  FSortedList := TsgObjectList.Create;
  FSortedList.ProcCompare := CompareEntHandles;
  FSortedList.Duplicates := dupIgnore;
  FSortedList.Sorted := False;
end;

destructor TdwgStyleControl.Destroy;
var
  I: Integer;
  vStyle: TsgDXFStyle;
begin
  for I := FSortedList.Count - 1 downto 0 do
  begin
    vStyle := TsgDXFStyle(FSortedList[I]);
    if not Assigned(vStyle.Owner) then
    begin
      FSortedList.Delete(I);
      vStyle.Free;
    end;
  end;
  FSearchStyle.Free;
  FSortedList.Free;
  inherited Destroy;
end;

function TdwgStyleControl.ChildrenAdd(Handle: UInt64): Integer;
var
  vStyle: TsgDXFStyle;
begin
  Result := inherited ChildrenAdd(Handle);
  vStyle := TsgDXFStyle.Create;
  vStyle.Handle := Handle;
  FSortedList.Add(vStyle);
end;

function TdwgStyleControl.GetNewObject(AHandle: UInt64;
  AEntClass: TsgDXFEntityClass): TsgDXFEntity;
begin
  Result := TsgDXFEntity(QFind(AHandle));
  if not Assigned(Result) then
    Result := inherited GetNewObject(AHandle, AEntClass);
end;

function TdwgStyleControl.QFind(Handle: UInt64): TsgDXFEntity;
var
  I: Integer;
begin
  Result := nil;
  FSearchStyle.Handle := Handle;
  I := FSortedList.IndexOf(FSearchStyle);
  if I >= 0 then
    Result := TsgDXFEntity(FSortedList[I])
end;

procedure TdwgStyleControl.ReadData(var APBit: TsgInt64);
begin
  InitializeTable(csStyles, cnstTableSTYLE);
  inherited ReadData(APBit);
end;

procedure TdwgStyleControl.ReadSubEntIDs(var APBit: TsgInt64;
  const AFlags: Integer);
begin
  inherited ReadSubEntIDs(APBit, AFlags);
  FSortedList.Sorted := True;
end;

procedure TdwgStyleControl.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgProxyReaderImplAcceess(AReader.ProxyReaderImpl).UpdateStyleByFont(FEntity);
end;

{ TDWGProxyReader }

function TDWGProxyReader.PositionAsInt64: TsgInt64;
begin
  Result := PsgInt64(FPos)^;
end;

constructor TDWGProxyReader.Create(APos: Pointer; AVersion: TsgDWGVersion);
begin
  inherited Create(APos, AVersion);
  GetMem(FStack, SizeOf(TsgInt64));
  PsgInt64(FStack)^ := PsgInt64(FPos)^;
end;

destructor TDWGProxyReader.Destroy;
begin
  FreeMem(FStack, SizeOf(TsgInt64));
  inherited Destroy;
end;

procedure TDWGProxyReader.PopPosition;
begin
  PsgInt64(FPos)^ := PsgInt64(FStack)^;
end;

procedure TDWGProxyReader.PushPosition;
begin
  PsgInt64(FStack)^ := PsgInt64(FPos)^;
end;

function TDWGProxyReader.ReadByte: Byte;
begin
  Result := ReadRC(PsgInt64(FPos)^);
end;

procedure TDWGProxyReader.ReadBytes(var Dest; const ACount: UInt64);
begin
  ReadBytesCustom(PsgInt64(FPos)^, Dest, ACount);
end;

function TDWGProxyReader.ReadDouble: Double;
begin
  Result := ReadRD(PsgInt64(FPos)^);
end;

function TDWGProxyReader.ReadHandle: UInt64;
begin
  ReadBytesDoublePair(PsgInt64(FPos)^, Result, 2);
end;

function TDWGProxyReader.ReadInteger: Integer;
begin
  Result := ReadRL(PsgInt64(FPos)^);
end;

function TDWGProxyReader.ReadSmallInt: SmallInt;
begin
  Result := ReadRS(PsgInt64(FPos)^);
end;

function TDWGProxyReader.ReadWord: Word;
begin
  Result := Word(ReadRS(PsgInt64(FPos)^));
end;

procedure TDWGProxyReader.Seek(ASize: Integer);
begin
  Inc2(PsgInt64(FPos)^, ASize shl 3);
end;

{ TdwgBlockControl }

procedure TdwgBlockControl.ReadData(var APBit: TsgInt64);
var
  I: Integer;
begin
  InitializeTable(csBlockRecords, cnstTableBLOCK_RECORD);
  TsgDXFEntityAccess(FEntity).SetConverter(Reader.Conv);
  inherited ReadData(APBit);
  FModelHandle := ReadID(APBit);
  FPaperHandle := ReadID(APBit);

  if FModelHandle = cnstBadHandle then
    FModelHandle := Reader.FModel.BlockRecord.Handle;
  if FPaperHandle = cnstBadHandle then
    FPaperHandle := Reader.FPaper.BlockRecord.Handle;

  if not Assigned(FChildren) then
    ForceChildrenList;

  if not Find(FModelHandle, I) then
    ChildrenAdd(FModelHandle);
  if not Find(FPaperHandle, I) then
    ChildrenAdd(FPaperHandle);
end;

{ TdwgAppIDControl }

procedure TdwgAppIDControl.ReadData(var APBit: TsgInt64);
begin
  InitializeTable(TConvSection(Ord(High(TConvSection)) + 1), cnstAPPID);
  inherited ReadData(APBit);
end;

procedure TdwgAppIDControl.References(AReader: TdwgReader);

  function UpdateID(var AHandle: UInt64; const AName: string): TsgDXFEntity;
  begin
    Result := FEntity.FindEntByName(AName);
    if Assigned(Result) then
      AHandle := Result.Handle;
  end;

begin
  inherited References(AReader);
  UpdateID(AReader.FACADAppID, sACADXDataAppName);
  UpdateID(AReader.FAppID, GetAppID);
  UpdateID(AReader.FAnnotativeAppID, cnstAcadAnnotative);
  UpdateID(AReader.FURLAppID, sURLXDataName);
  UpdateID(AReader.FAcDbAttrAppID, sACADXDataAttribAppName);
  UpdateID(AReader.FACADMLeaderVerAppID, sAcadMLeaderVer);
end;

{ TdwgDimStyleControl }

procedure TdwgDimStyleControl.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vHandle: UInt64;
  vHCode: Byte;
begin
  InitializeTable(csDimStyles, cnstTableDIMSTYLE);
  if Version > acR2004 then
    inherited ReadData(APBit)
  else
  begin
    ReadObjHeader(APBit);
    FSubEnts := ReadBL(APBit);
    ReadID(APBit);
    ReadID(APBit); // NULL (soft pointer)
    ForceChildrenList;
    I := FSubEnts;
    repeat
      vHandle := ReadID(APBit, vHCode);
      case vHCode of
        cntDWGObjHandleSoftPointer: FReactors.Add(vHandle);
        cntDWGObjHandleSoftOwner:
          begin
            ChildrenAdd(vHandle);
            Dec(I);
          end;
      end;
    until (vHCode in [cntDWGObjHandleHardOwner, cntDWGObjHandleHardPointer]) or (I <= 0);
    while (I > 0) and (vHCode <> cntDWGObjHandleHardPointer) do
    begin
      vHandle := ReadID(APBit, vHCode);
      case vHCode of
        cntDWGObjHandleSoftOwner:
          begin
            ChildrenAdd(vHandle);
            Dec(I);
          end;
      end;
    end;
    if ChildrenCount <= 0 then
      FreeAndNil(FChildren);
  end;
end;

{ TdwgVPortControl }

procedure TdwgVPortControl.ReadData(var APBit: TsgInt64);
begin
  InitializeTable(csVPorts, cnstTableVPORT);
  inherited ReadData(APBit);
  if Reader.FVPortControl = cnstBadHandle then
    Reader.FVPortControl := FHandle;
end;

{ TdwgDictionaryVar }

procedure TdwgDictionaryVar.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  TsgDXFDictionaryVar(FEntity).IntVal := ReadRC(APBit);
  TsgDXFDictionaryVar(FEntity).Value := ReadText;
  ReadHandleRefs(APBit);
end;

{ TdwgDataBase }

function TdwgDataBase.GetChildren(Index: Integer): UInt64;
begin
  Result := Reader.FObjMap.MapElement[Index].Handle;
end;

function TdwgDataBase.GetChildrenCount: Integer;
begin
  Result := Reader.FObjMap.Count + 1;
end;

procedure TdwgDataBase.ReadData(var APBit: TsgInt64);
begin
end;

procedure TdwgDataBase.ReadSubEnts(AReader: TdwgReader);
var
  C: Integer;
  vOwner: TsgDXFEntity;
  vObj: TdwgObject;
  vElem: PdwgMapElementList;
  vIndex: Integer;
  vMap: TsgObjectsMapAccess;

  procedure DoFreeEntity(AObj: TdwgObject; AMapIndex: Integer);
  var
    J, K: Integer;
    vChildElem: PdwgMapElementList;
  begin
    vElem^.Obj := nil;
    for J := AObj.ChildrenCount - 1 downto 0 do
    begin
      K := vMap.IndexOfHandle(AObj.Children[J]);
      if K >= 0 then
      begin
        vChildElem := vMap.GetPMapElement(K);
        vChildElem^.Obj := nil;
      end;
    end;
    FreeAndNil(AObj.FEntity);
  end;

  procedure DoAdd(AOwner: TsgDXFEntity; AMapIndex: Integer);
  begin
    if not Assigned(AOwner) or (vObj.OwnerAdd(AOwner) < 0) then
      DoFreeEntity(vObj, AMapIndex);
  end;

begin
  vMap := TsgObjectsMapAccess(Reader.FObjMap);
  FChildIndex := 0;
  C := ChildrenCount;
  while (FChildIndex < C) and not AReader.Stopped do
  begin
    vElem := vMap.GetPMapElement(FChildIndex);
    if vElem^.Obj = nil then
    begin
      vIndex := FChildIndex;
      vObj := ReadChild(vElem^.Location, vElem^.Handle, FChildIndex);
      if Assigned(vObj) then
      try
        vObj.References(AReader);
        vObj.PostReferences(AReader);
        vOwner := vObj.GetOwner(AReader);
        if Version <= acR14 then
        begin
          if (vOwner = AReader.Model) or (vOwner = AReader.Paper) or (vOwner = AReader.Conv.Sections[csBlocks]) or
             (Assigned(vOwner) and (GetPaperIndex(vOwner.Name) >= 0)) then
            DoAdd(vOwner, vIndex)
          else
            if vObj.IsEntity then
              DoFreeEntity(vObj, vIndex)
            else
              DoAdd(vOwner, vIndex);
        end
        else
          DoAdd(vOwner, vIndex);
        vMap.Objects[FChildIndex] := vObj.FEntity;
      finally
        FreeAndNil(vObj);
      end;
    end;
    Inc(FChildIndex);
  end;
end;

{ TdwgTableItem }

procedure TdwgTableItem.ReadData(var APBit: TsgInt64);
begin
  FEntryName := ReadObjName(APBit, FFlags);
end;

procedure TdwgTableItem.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  ReadID(APBit); // NULL (hard pointer)
end;

{ TdwgTables }

function TdwgTables.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := AReader.Conv.Main;
end;

procedure TdwgTables.NewObject(ACode: Integer; AHandle: UInt64;
  var AObj: TdwgObject);
begin
  inherited NewObject(ACode, AHandle, AObj);
  case ACode of
    48:
      begin
        AObj.FEntity.Free;
        AObj.Free;
        AObj := Reader.FBlockRecords;
      end;
  end;
end;

procedure TdwgDataLink.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vValuesCount, vDataType: Integer;
//  vValueName: string;
  vStringReader: TdwgStringReader;
begin
  ReadObjHeader(APBit);
  ReadText; // 1
  ReadText; // 300
  ReadText; // 301
  TsgCADDataLink(FEntity).FileName := ReadText; // 302
  SeekGeneral(APBit, [seBL, seBL, seBL, seBS, seBS, seBS, seBS, seBS, seBS,
    seBS, seBS, seBL]);  // 90, 91, 92, 170..177, 93
  ReadText; // 304
  SeekGeneral(APBit, [seBL]); // 94
  // DATAMAP_BEGIN
  vValuesCount := ReadBL(APBit); // 90
  vStringReader := Reader.GetStringReader(APBit);
  for I := 0 to vValuesCount - 1 do
  begin
    {vValueName := }ReadText;  // value name - 300
    if Version >= acR2007 then
      SeekGeneral(APBit, [seBL]); // 93
    vDataType := ReadBL(APBit); // 90
    case vDataType of
      // Unknown
      0: SeekGeneral(APBit, [seBL]);
      // Int - 90
      1: SeekGeneral(APBit, [seBL]);
      // Double
      2: SeekGeneral(APBit, [seBD]);
      // text - 1
      4: vStringReader.ReadText; // nil - for not read acr2007+ strings
      // Date - BL data size N, followed by N bytes (Int64 value)
      8: Inc2(APBit, ReadBL(APBit) shl 3);
      // –Point BL data size, followed by 2RD
      16: Inc2(APBit, ReadBL(APBit) shl 3);
      // –3D Point BL data size, followed by 3RD
      32: Inc2(APBit, ReadBL(APBit) shl 3);
      // –Object Id H Read from appropriate place in handles section (soft pointer).
      64: ReadID(APBit);
      // –Buffer Unknown.
      128:;
      // –Result Buffer Unknown.
      256:;
      // –General General, BL containing the byte count followed by a byte array.
      // (introduced in R2007, use Unknown before R2007).
      512:
        if Version >= acR2007 then
          Inc2(APBit, ReadBL(APBit) shl 3);
    end;
    if Version >= acR2007 then
    begin
      SeekGeneral(APBit, [seBL]); // 94
      ReadText;     // 300
      ReadText;     // 302
    end;
  end;
  // handle references
  ReadHandleRefs(APBit);
  ReadID(APBit); // ACAD_TABLE   - 330
  ReadID(APBit); // TABLECONTENT - 330
  ReadID(APBit); // TABLECONTENT - 360
end;

{ TdwgXRecord }

procedure TdwgXRecord.AddStringData(AExtData: TsgCADExtendedData; ACode: SmallInt; S: string);
begin
  repeat
    AExtData.AddString(ACode, Copy(S, 1, High(Byte)));
    Delete(S, 1, High(Byte));
  until S = '';
end;

procedure TdwgXRecord.ReadData(var APBit: TsgInt64);
var
  vLen: Integer;
  vCode: SmallInt;
  vExData: TsgCADExtendedData;
  vEnd: TsgInt64;
  vHandle: UInt64;
  vStringReader: TdwgStringReader;
begin
  ReadObjHeader(APBit);
  vLen := ReadBL(APBit);
  if vLen > 0 then
  begin
    vStringReader := Reader.GetStringReader(APBit);
    vEnd := APBit;
    Inc2(vEnd, vLen shl 3);
    vExData := TsgCADExtendedData(TsgDXFXRecord(FEntity).Data);
    while Comp2(APBit, vEnd) do
    begin
      vCode := ReadRS(APBit);
      case CodeTypes^[vCode] of
        ctInteger:
          case vCode of
            280..282, 284, 290..299: vExData.AddByte(vCode, ReadRC(APBit));
            60..79, 170..179, 270..279, 283, 285..289, 370..389, 400..409, 1060..1070:
              vExData.AddInt16(vCode, ReadRS(APBit));
          else
            vExData.AddInt(vCode, ReadRL(APBit));
          end;
        ctDouble:
          case vCode of
            10 .. 13: vExData.AddPoint(vCode, Read3RD(APBit));
          else
            vExData.AddDouble(vCode, ReadRD(APBit));
          end;
        ctString:
          case vCode of
            1004: AddBinaryChunk(APBit, vCode, vExData);
          else
            AddStringData(vExData, vCode, vStringReader.ReadXrecordText);
          end;
        ctHex:
          begin
            if vCode = 5 then
              AddStringData(vExData, vCode, vStringReader.ReadXrecordText)
            else
            begin
              ReadBytesDoublePair(APBit, vHandle, 2);
              vExData.AddInt64(vCode, vHandle);
            end;
          end;
      else
        case vCode of
          -2:
            begin
              ReadBytesCustom(APBit, vHandle, 8);
              vExData.AddInt64(vCode, vHandle);
            end;
          310 .. 319: AddBinaryChunk(APBit, vCode, vExData);
        //else
          //raise EdwgError.CreateFmt('Unknown XRecord(%x) code "%d"', [FHandle, vCode]);
        end;
      end;
    end;
  end;
  if Version >= acR2000 then
    SeekGeneral(APBit, [seBS]); //Cloning flag BS 280
  ReadHandleRefs(APBit);
end;

procedure TdwgXRecord.ReadOwner(AReader: TdwgReader);
begin
  inherited ReadOwner(AReader);
  if not (FOwnerObject is TsgDXFDictionary) then
    if Assigned(FOwnerDictionary) then
      FOwnerObject := FOwnerDictionary.FEntity;
end;

procedure TdwgXRecord.References(AReader: TdwgReader);
begin
  inherited References(AReader);
end;

{ TdwgMesh }

procedure TdwgMesh.ReadEntData(var APBit: TsgInt64);
var
//  vVer, vBlendCrease, vSubdivisionLevel: Integer;
  I: Integer;
begin
  {vVer := ReadBS(APBit);
  vBlendCrease := ReadBit(APBit);
  vSubdivisionLevel := ReadBL(APBit);}
  SeekGeneral(APBit, [seBS, seB, seBL]);
  TsgDXFMesh(FEntity).Vertices.Count := ReadBL(APBit);
  for I := 0 to TsgDXFMesh(FEntity).Vertices.Count - 1 do
    TsgDXFMesh(FEntity).Vertices[I] := Read3D(APBit);
  TsgDXFMesh(FEntity).VertexIndices.Count := ReadBL(APBit);
  for I := 0 to TsgDXFMesh(FEntity).VertexIndices.Count - 1 do
    TsgDXFMesh(FEntity).VertexIndices[I] := ReadBL(APBit);
  inherited ReadEntData(APBit);
end;

{ TdwgMaterial }

procedure TdwgMaterial.ReadMaterialColor(var APBit: TsgInt64;
  AMaterialColor: TsgCustomMaterialColor);
begin
  AMaterialColor.Method := ReadRC(APBit);
  AMaterialColor.Factor := ReadBD(APBit);
  if AMaterialColor.Method and $01 <> 0 then
    AMaterialColor.Color := ReadCmEntityColor(APBit);
end;

procedure TdwgMaterial.ReadTexMatrix(var APBit: TsgInt64; AMaterialMap: TsgMaterialMap);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      AMaterialMap.Mapper[I, J] := ReadBD(APBit);
end;

function TdwgMaterial.ReadMapTail(var APBit: TsgInt64; AMaterialMap: TsgMaterialMap): Boolean;
//var
//  vPreamble: Integer;
begin
  AMaterialMap.Blend := ReadBD(APBit);
  Inc2(APBit, 24);
//    vPreamble := 0;
//    ReadBytes(APBit, vPreamble, 3);
//    case vPreamble of
//      $00010101:;
//      //$00020101:;
//      $00020102: asm nop end;
//      $00010102: asm nop end;
//    else
//    end;
  ReadTexMatrix(APBit, AMaterialMap);
  AMaterialMap.MapSource := ReadRC(APBit);
  if AMaterialMap.MapSource = 1 then
    AMaterialMap.FileName := ReadText;
  Result := AMaterialMap.MapSource <> 2;
end;

procedure TdwgMaterial.ReadData(var APBit: TsgInt64);
var
  vMat: TsgDXFMaterial;
begin
  ReadObjHeader(APBit);
  FEntity.Name := ReadText;
  ReadText; // description - 2
  vMat := TsgDXFMaterial(FEntity);
//--------------------------------------------------------------------
  ReadMaterialColor(APBit, vMat.Ambient.MaterialColor);  // 70,40,90
//--------------------------------------------------------------------
  ReadMaterialColor(APBit, vMat.Diffuse.MaterialColor);  // 71,41,91
  if not ReadMapTail(APBit, vMat.Diffuse) then
    case ReadBB(APBit) of
      $02:
        begin
          ReadMaterialColor(APBit, vMat.Diffuse.Bleed);
          ReadMaterialColor(APBit, vMat.Diffuse.IndirectDump);
          vMat.Diffuse.ReflectanceScale := ReadBD(APBit);
          vMat.Diffuse.TransmittanceScale := ReadBD(APBit);
          vMat.Diffuse.Luminance := ReadBD(APBit);
        end;
      $01:
        begin
          vMat.Diffuse.Xrecord := TsgDXFXRecord.Create;
          vMat.Diffuse.Xrecord.Data.AddDouble(277, ReadRC(APBit));
          ReadMaterialColor(APBit, vMat.Diffuse.Normal1);
          ReadMaterialColor(APBit, vMat.Diffuse.Normal2);
          vMat.Diffuse.Xrecord.Data.AddDouble(466, ReadBD(APBit));
          vMat.Diffuse.Xrecord.Data.AddDouble(467, ReadBD(APBit));
        end;
    end;
//--------------------------------------------------------------------
  ReadMaterialColor(APBit, vMat.Specular.MaterialColor);  // 76,45,92
  ReadMapTail(APBit, vMat.Specular);
  vMat.Specular.Gloss := ReadBD(APBit);            // 44
//--------------------------------------------------------------------
  ReadMapTail(APBit, vMat.Reflection);
//--------------------------------------------------------------------
  vMat.Opacity.Percent := ReadBD(APBit);
  ReadMapTail(APBit, vMat.Opacity);
//--------------------------------------------------------------------
  ReadMapTail(APBit, vMat.Bump);
//--------------------------------------------------------------------
  vMat.Refraction.Index := ReadBD(APBit);
  ReadMapTail(APBit, vMat.Refraction);
end;

procedure TdwgMaterial.ReadOwner(AReader: TdwgReader);
begin
  if (FOwnerDictionary = nil) and (FOwner <> cnstBadHandle) then
    inherited;
end;

{ TdwgStringReader }

constructor TdwgStringReader.Create(APPBit: PsgInt64; ACodePage: Word);
begin
  Update(APPBit, ACodePage);
end;

procedure TdwgStringReader.DoReadRawText(ALen: Integer;
  ACP: Word; var S: sgRawByteString);
begin
  if ALen > 0 then
  begin
    SetLength(S, ALen);
{$IFDEF SG_HAS_CPSTRING}
    SetCodePage(S, $FFFF, False);
{$ENDIF}
    ReadBytesCustom(FPBit^, PPointer(S)^, ALen);
{$IFDEF SG_HAS_CPSTRING}
    SetCodePage(S, ACP, False);
{$ENDIF}
  end
  else
    S := '';
end;

procedure TdwgStringReader.DoReadUnicodeText(ALen: Integer; var S: sgUnicodeStr);
begin
  if ALen > 0 then
  begin
    SetLength(S, ALen);
    ReadBytesCustom(FPBit^, PPointer(S)^, ALen * SizeOf(S[1]));
    if S[ALen] = #0 then
      SetLength(S, ALen - 1);
  end
  else
    S := '';
end;

function TdwgStringReader.ReadCodePage: Word;
begin
  Result := FCodePage;
end;

function TdwgStringReader.ReadTextLen: Integer;
begin
  Result := ReadBS(FPBit^);
end;

function TdwgStringReader.ReadXrecordTextLen: Integer;
begin
  Result := ReadRS(FPBit^);
end;

procedure TdwgStringReader.TrimTextLen(ALen: Integer; var S: string);
begin
  if (ALen > 0) and (S[ALen] = #0) then
    SetLength(S, ALen - 1);
end;

procedure TdwgStringReader.Update(APPBit: PsgInt64; ACodePage: Word);
begin
  FPBit := APPBit;
  FCodePage := ACodePage;
end;

{ TdwgWideStringReader }

function TdwgWideStringReader.ReadText: string;
{$IFNDEF SGDEL_2009}
var
  S: sgUnicodeStr;
{$ENDIF}
begin
{$IFDEF SGDEL_2009}
  DoReadUnicodeText(ReadTextLen, Result);
{$ELSE}
  DoReadUnicodeText(ReadTextLen, S);
  Result := ConvertToAnsiString(S, FCodePage);
{$ENDIF}
end;

function TdwgWideStringReader.ReadXrecordText: string;
{$IFNDEF SGDEL_2009}
var
  S: sgUnicodeStr;
{$ENDIF}
begin
{$IFDEF SGDEL_2009}
  DoReadUnicodeText(ReadXrecordTextLen, Result);
{$ELSE}
  DoReadUnicodeText(ReadXrecordTextLen, S);
  Result := ConvertToAnsiString(S, FCodePage);
{$ENDIF}
end;

function TdwgWideStringReader.ReadInfoText: string;
begin
  Result := ReadXrecordText;
end;

{ TdwgAnsiStringReader }

function TdwgAnsiStringReader.ReadCodePage: Word;
begin
  Result := sgCodePageFromDWG(ReadRC(FPBit^), FCodePage);
end;

function TdwgAnsiStringReader.ReadInfoText: string;
var
  S: sgRawByteString;
  Len: Integer;
begin
  DoReadRawText(ReadXrecordTextLen, FCodePage, S);
  Result := string(S);
  Len := Length(Result);
  TrimTextLen(Len, Result);
end;

function TdwgAnsiStringReader.ReadText: string;
var
  Len: Integer;
{$IFDEF SG_HAS_CPSTRING}
  S: sgRawByteString;
{$ENDIF}
begin
  Len := ReadTextLen;
{$IFDEF SG_HAS_CPSTRING}
  DoReadRawText(Len, FCodePage, S);
{$IFDEF USE_FEATURE_ENCODING}
   with TEncoding.GetEncoding(FCodePage) do
   try
     Result := GetString(BytesOf(S));
   finally
     Free;
   end;
{$ELSE}
  Result := string(S);
{$ENDIF}
  Len := Length(Result);
{$ELSE}
  DoReadRawText(Len, FCodePage, Result);
{$ENDIF}
  TrimTextLen(Len, Result);
end;

function TdwgAnsiStringReader.ReadXrecordText: string;
var
  Len: Integer;
{$IFDEF SG_HAS_CPSTRING}
  S: sgRawByteString;
{$ENDIF}
begin
  Len := ReadXrecordTextLen;
{$IFDEF SG_HAS_CPSTRING}
  DoReadRawText(Len, ReadCodePage, S);
  Result := string(S);
  Len := Length(Result);
{$ELSE}
  DoReadRawText(Len, ReadCodePage, Result);
{$ENDIF}
  TrimTextLen(Len, Result);
end;

{ TdwgWideStringReaderParse }

function TdwgWideStringReaderParse.ReadText: string;
begin
  Result := inherited ReadText;
{$IFDEF UNICODE}
  Result := ParseUnicode(Result, [puU, puM]);
{$ENDIF}
end;

{ TdwgAnsiStringReaderParse }

function TdwgAnsiStringReaderParse.ReadText: string;
begin
  Result := inherited ReadText;
{$IFDEF UNICODE}
  Result := ParseUnicode(Result, [puU, puM]);
{$ENDIF}
end;

{ TdwgScale }

function TdwgScale.GetOwner(AReader: TdwgReader): TsgDXFEntity;
begin
  Result := inherited GetOwner(AReader);
end;

procedure TdwgScale.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  Inc2(APBit, 2);                                             // 70 ??
  TsgDXFScale(FEntity).Description := ReadText; // 300
  TsgDXFScale(FEntity).LayoutScale := ReadBD(APBit);          // 140
  TsgDXFScale(FEntity).ModelScale := ReadBD(APBit);           // 141
  TsgDXFScale(FEntity).Flags := ReadBit(APBit);               // 290
  ReadHandleRefs(APBit);
end;

procedure TdwgScale.References(AReader: TdwgReader);
begin
  inherited References(AReader);
end;

{ TdwgObjectContextData }

function TdwgObjectContextData.GetBitSize: Integer;
begin
  Result := FBitSize;
end;

function TdwgObjectContextData.GetObjectBeginning: TsgInt64;
begin
  Result := FObjectBeginning;
end;

procedure TdwgObjectContextData.ReadContext(var APBit: TsgInt64);
var
  vPBit: TsgInt64;
begin
  vPBit := FObjectBeginning;
  Inc2(vPBit, BitSize);
  APBit := vPBit;
end;

procedure TdwgObjectContextData.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  //if Version <> acR2007 then // check on acR2007!!!
  begin
    FObjVersion := ReadBS(APBit); //BS 70 Version (default value is 3).
    //FHasFile := ReadBit(APBit); //B-Has file to extension dictionary (default value is true).??
    FDefaultFlag := ReadBit(APBit); //B 290 Default flag (default value is false).
  end;
  ReadContext(APBit);
  ReadHandleRefs(APBit);
end;

procedure TdwgObjectContextData.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  // AcDbAnnotScaleObjectContextData
  FScale := ReadID(APBit); //H 340 Handle to scale (AcDbScale) object (hard pointer).
end;

procedure TdwgObjectContextData.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgDXFObjectContextData(FEntity).Scale := TsgDXFScale(Reader.ObjByHandle(FScale, nil));
end;

procedure TdwgObjectContextData.SetBitSize(const AValue: Integer);
begin
  FBitSize := AValue;
end;

procedure TdwgObjectContextData.SetObjectBeginning(const AValue: TsgInt64);
begin
  FObjectBeginning := AValue;
end;

{ TdwgIndexColorReader }

function TdwgIndexColorReader.GetFlags: Word;
begin
  Result := FFlags;
end;

function TdwgIndexColorReader.ReadColor(APBit: PsgInt64): TsgColorCAD;
begin
  Result := ReadENC(APBit);
end;

function TdwgIndexColorReader.ReadENC(APBit: PsgInt64): TsgColorCAD;
begin
  FFlags := ReadFlags(APBit^);
  Result := ReadENCSection(APBit^);
end;

function TdwgIndexColorReader.ReadENCSection(var APBit: TsgInt64): TsgColorCAD;
begin
  Result := MakeColorCAD(acIndexColor, FFlags and $1FF);
end;

function TdwgIndexColorReader.ReadFlags(var APBit: TsgInt64): Integer;
begin
  Result := ReadBS(APBit);
end;

procedure TdwgIndexColorReader.Update(AObj: Pointer);
begin
  FObj := AObj;
  FFlags := 0;
end;

{ TdwgR18ColorReader }

function TdwgR18ColorReader.ReadENCSection(var APBit: TsgInt64): TsgColorCAD;
begin
  // if FFlags and $C000 = $C000 then handle to the color
  // is written in the handle stream.
  if FFlags and $C000 = $8000 then
    Result := ReadCmEntityColor(APBit)
  else
    Result := inherited ReadENCSection(APBit);
  if FFlags and $2000 <> 0 then
    if Assigned(FObj) then
      FObj.SetTransparency(ReadBL(APBit))
    else
      SeekGeneral(APBit, [seBL]);
end;

function TdwgR18ColorReader.ReadCMCSection(var APBit: TsgInt64): TsgColorCAD;
var
  vTextFlags: Integer;
  {vColorBlockName: string;}
begin
  Result := ReadCmEntityColor(APBit);
  vTextFlags := ReadRC(APBit);// Color Byte
  if vTextFlags and 1 <> 0 then
    Result.AlbumString := Obj.ReadText;// color name
  if vTextFlags and 2 <> 0 then
    {vColorBlockName := }Obj.ReadText;// book name
end;

function TdwgR18ColorReader.ReadColor(APBit: PsgInt64): TsgColorCAD;
begin
  FFlags := ReadFlags(APBit^);
  if FFlags = 0 then
    Result := ReadCMCSection(APBit^)
  else
    Result := ReadENCSection(APBit^);
end;

{ TdwgRay }

procedure TdwgRay.ReadEntData(var APBit: TsgInt64);
begin
  TsgDXFRay(FEntity).StartPoint := Read3D(APBit);
  TsgDXFRay(FEntity).Direction := Read3D(APBit);
end;

{ TdwgSpatialFilter }

procedure TdwgSpatialFilter.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  vSpatialFilter: TsgCADSpatialFilter;

  function ReadMatrix: TFMatrix;
  var
    L, K: Integer;
  begin
{$IFDEF _FIXINSIGHT_}
    FillChar(Result, SizeOf(Result), 0);
{$ENDIF}
    for K := 0 to 2 do
      for L := 0 to 3 do
        Result.M[L][K] := ReadBD(APBit);
  end;

begin
  ReadObjHeader(APBit);
  vSpatialFilter := TsgCADSpatialFilter(FEntity);
  I := ReadBS(APBit) - 1;
  while I >= 0 do
  begin
    vSpatialFilter.Bounds.Add(Read2RD(APBit));
    Dec(I);
  end;
  vSpatialFilter.Extrusion := ReadExtrusion(APBit);
  vSpatialFilter.Clipbdorg := Read3D(APBit);
  vSpatialFilter.DisplayBounds := ReadBS(APBit) <> 0;
  vSpatialFilter.FrontClip := ReadBS(APBit) = 1;
  if vSpatialFilter.FrontClip then
    vSpatialFilter.FrontDist := ReadBD(APBit);
  vSpatialFilter.BackClip := ReadBS(APBit) = 1;
  if vSpatialFilter.BackClip then
    vSpatialFilter.BackDist := ReadBD(APBit);
  vSpatialFilter.InvBlkTransform := ReadMatrix;
  vSpatialFilter.Clip := ReadMatrix;
  ReadHandleRefs(APBit);
end;

{ TdwgCustomApplicationBlockReference }

procedure TdwgCustomApplicationBlockReference.ReadData(var APBit: TsgInt64);
begin
  inherited ReadData(APBit);
  FViewport := ReadID(APBit); // viewport soft pointer ID
end;

procedure TdwgCustomApplicationBlockReference.References(AReader: TdwgReader);
var
  vAttributes: IsgGlobalPropProvider;
begin
  inherited References(AReader);
  if GetGlobalPropProvider(FEntity, False, vAttributes) then
    vAttributes[cnstViewport] := {$IFNDEF SGDEL_6}sgUInt64AsVar{$ENDIF}(FViewport);
end;

{ TdwgCustomVertex }

function TdwgCustomVertex.ReadExtrusion(var APBit: TsgInt64): TFPoint;
begin
  Result := ReadBE(APBit, Version);
  if cnstLoadCad2D then
    Result := cnstExtrusion;
end;

function TdwgCustomVertex.ReadZThik(var APBit: TsgInt64): Double;
begin
  Result := ReadBT(APBit, Version);
  if cnstLoadCad2D then
    Result := 0;
end;

{ TProxyLWPLineReader }

function TProxyLWPLineReader.ReadObject(ProxyReader: IProxyReader; obj: TObject): Boolean;
var
  vLWPLine: TdwgLWPline;
  vBits: TsgInt64;
begin
  Result := False;
  if (ProxyReader <> nil) and (obj is TsgDXFLWPolyline) then
  begin
    vLWPLine := TdwgLWPline.Create(TsgDWGVersion(ProxyReader.GetVersion));
    try
      vLWPLine.FEntity := TsgDXFLWPolyline(obj);
      vBits := ProxyReader.PositionAsInt64;
      vLWPLine.ReadEntData(vBits);
      Result := True;
    finally
      vLWPLine.FEntity := nil;
      vLWPLine.Free;
    end;
  end;
end;

{ TdwgCADWorxSteelComponent }

procedure TdwgCADWorxSteelComponent.ReadHandleRefs(var APBit: TsgInt64);
var
  I: Integer;
begin
  inherited ReadHandleRefs(APBit);
  if ((FMode = 0) and (FOwner <> cnstBadHandle)) and not Reader.FBlockRecords.Find(FOwner, I) then
    FOwner := Reader.Model.BlockRecord.Handle;
end;

procedure TdwgCADWorxSteelComponent.ReadOwner(AReader: TdwgReader);
begin
  inherited ReadOwner(AReader);
  if (FMode = 0) and not (FOwnerObject is TsgDXFBlockRecord) then
    FOwnerObject := AReader.Model;
end;

{ TdwgBlkRefObjectContextData }

procedure TdwgBlkRefObjectContextData.ReadContext(var APBit: TsgInt64);
begin
  TsgCADBlkRefObjectContextData(FEntity).Angle := ReadBD(APBit) * f180DividedByPi;
  TsgCADBlkRefObjectContextData(FEntity).Point := Read3D(APBit);
  TsgCADBlkRefObjectContextData(FEntity).ScaleFactor := Read3D(APBit);
end;

{ TdwgMLeaderObjectContextData }

procedure TdwgMLeaderObjectContextData.ReadContext(var APBit: TsgInt64);
var
  I, J, K, L: Integer;
  vLeadersCount, vStartEndCount, vBreaksCount,
  vLinesCount, vPointsCount, vColumnSizesCount: Integer;
  vMLLine: TsgMLeaderLineAccess;
  vMLeader: TsgMLeaderAccess;
  vItem: TsgMLeaderContextDataAccess;

  function ReadMatrix: TFMatrix;
  var
    Row, Col: Integer;
  begin
    for Col := 0 to 2 do
      for Row := 0 to 3 do
        Result.M[Row, Col] := ReadBD(APBit);
    SeekGeneral(APBit, [seBD, seBD, seBD, seBD]);
  end;

begin
  vItem := TsgMLeaderContextDataAccess(TsgCADMLeaderAnnotContextAccess(FEntity).Item);

  vLeadersCount := ReadBL(APBit);
  for I := 0 to vLeadersCount - 1 do
  begin
    vMLeader := TsgMLeaderAccess(TsgMLeader.Create);
    vItem.AddMLeader(vMLeader);
    SeekGeneral(APBit, [seB, seB]);//B 290 Is content valid (sg writes true)  B 291 Unknown (sg writes true)
    vMLeader.ConnectionPoint := Read3D(APBit);//10,20,30
    vMLeader.Direction := Read3D(APBit);//11,21,31
    vStartEndCount := ReadBL(APBit);
    for J := 0 to vStartEndCount - 1 do
      vMLeader.AddStartEnd(MakeLine(Read3D(APBit), Read3D(APBit)));
    vMLeader.Idx := ReadBL(APBit);
    vMLeader.LandingDistance := ReadBD(APBit);
    vLinesCount := ReadBL(APBit);
    for J := 0 to vLinesCount - 1 do
    begin
      vMLLine := TsgMLeaderLineAccess(TsgMLeaderLine.Create);
      vMLeader.AddMLeaderLine(vMLLine);
      vPointsCount := ReadBL(APBit);
      for K := 0 to vPointsCount - 1 do
        vMLLine.Points.Add(Read3D(APBit));
      SeekGeneral(APBit, [seBL]);//Break info count
      if Version >= acR2010 then
      begin
        vMLLine.SegmentIndex := ReadBL(APBit);//90 Segment index
        vBreaksCount := ReadBL(APBit);//Start/end point pair count
        for L := 0 to vBreaksCount - 1 do
          vMLLine.AddStartEnd(MakeLine(Read3D(APBit), Read3D(APBit)));
      end;
      vMLLine.Idx := ReadBL(APBit);
      if Version >= acR2010 then
      begin
        SeekGeneral(APBit, [seBS, seBS]);//?
        {vMLLine.LeaderType := ReadBS(APBit);
        vMLLine.Color := ReadColor(APBit);
        //vMLLine.LineType := ;
        vMLLine.LineWeight := GetLineWeight(ReadBL(APBit));
        vMLLine.ArrowSize := ReadBD(APBit);
        //vMLLine.Arrow := ;
        vMLLine.OverrideFlags := ReadBL(APBit);}
      end;
    end;
    if Version >= acR2010 then
      vMLeader.IsVertical := ReadBS(APBit) = 1;
  end;
  vItem.OverallScale := ReadBD(APBit);
  vItem.ContentBasePoint := Read3D(APBit);
  vItem.TextHeight := ReadBD(APBit);
  vItem.ArrowHeadSize := ReadBD(APBit);
  vItem.LandingGap := ReadBD(APBit);
  vItem.LeftAttachment := ReadBS(APBit);
  vItem.RightAttachment := ReadBS(APBit);
  vItem.TextAlignType := ReadBS(APBit);
  vItem.AttachmentType := ReadBS(APBit);
  vItem.HasTextContents := ReadBit(APBit) = 1;
  if vItem.HasTextContents then
  begin
    vItem.TextLabel := ReadText;
    vItem.TextNormal := Read3D(APBit);
    //vItem.TextStyle := ; // text style handle
    vItem.TextLocation := Read3D(APBit);
    vItem.TextDirection := Read3D(APBit);
    vItem.TextRotation := ReadBD(APBit) * f180DividedByPi;
    vItem.BoundaryWidth := ReadBD(APBit);
    vItem.BoundaryHeight := ReadBD(APBit);
    vItem.LineSpacingFactor := ReadBD(APBit);
    vItem.LineSpacingStyle := ReadBS(APBit);
    vItem.TextColor := ReadColor(APBit);
    vItem.Alignment := ReadBS(APBit);
    vItem.FlowDirection := ReadBS(APBit);
    vItem.BackgroundColor := ReadColor(APBit);
    vItem.BackgroundScaleFactor := ReadBD(APBit);
    vItem.BackgroundTransparency := ReadBL(APBit);
    vItem.IsBackgroundEnabled := ReadBit(APBit) = 1;
    vItem.IsBackgroundMaskFillOn := ReadBit(APBit) = 1;
    vItem.ColumnType := ReadBS(APBit);
    vItem.IsTextHeightAutomatic := ReadBit(APBit) = 1;
    vItem.ColumnWidth := ReadBD(APBit);
    vItem.ColumnGutter := ReadBD(APBit);
    vItem.ColumnFlowReversed := ReadBit(APBit) = 1;
    vColumnSizesCount := ReadBL(APBit);
    for J := 0 to vColumnSizesCount - 1 do
      vItem.ColumnSizes.Add(ReadBD(APBit));
    vItem.WordBreak := ReadBit(APBit) = 1;
    SeekGeneral(APBit, [seB]);
  end
  else
  begin
    vItem.HasContentsBlock := ReadBit(APBit) = 1;
    if vItem.HasContentsBlock then
    begin
      //vItem.BlockRecord := ; // BlockRecord handle
      vItem.BlkNormal := Read3D(APBit);
      vItem.BlkLocation := Read3D(APBit);
      vItem.BlkScale := Read3D(APBit);
      vItem.BlkRotation := ReadBD(APBit);
      vItem.BlockColor := ReadColor(APBit);
      vItem.BlkMatrix := ReadMatrix;
    end;
  end;
  vItem.BasePoint := Read3D(APBit);
  vItem.BaseDirection := Read3D(APBit);
  vItem.BaseVertical := Read3D(APBit);
  vItem.IsNormalReversed := ReadBit(APBit) = 1;
  if Version >= acR2010 then
  begin
    vItem.TopAttachment := ReadBS(APBit);
    vItem.BottomAttachment := ReadBS(APBit);
  end;
end;

procedure TdwgMLeaderObjectContextData.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  ReadContentHandles(APBit);
end;

procedure TdwgMLeaderObjectContextData.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  ReferencesContext(AReader);
end;

procedure TdwgMLeaderObjectContextData.ReadContentHandles(var APBit: TsgInt64);
var
  J, K: Integer;
  E: TsgCADMLeaderAnnotContextAccess;
  vItem: TsgMLeaderContextDataAccess;
  vMLeader: TsgMLeaderAccess;
begin
  E := TsgCADMLeaderAnnotContextAccess(FEntity);
  vItem := TsgMLeaderContextDataAccess(E.Item);
  if Version >= acR2010 then
  begin
    SetLength(FHandles, vItem.MLeadersCount);
    for J := 0 to vItem.MLeadersCount - 1 do
    begin
      vMLeader := TsgMLeaderAccess(vItem.MLeaders[J]);
      SetLength(FHandles[J], vMLeader.LinesCount);
      for K := 0 to vMLeader.LinesCount - 1 do
      begin
        FHandles[J][K].LineType := ReadID(APBit);
        FHandles[J][K].Arrow := ReadID(APBit);
      end;
    end;
  end;
  if vItem.HasTextContents then
    FTextStyle := ReadID(APBit)
  else
    if vItem.HasContentsBlock then
      FBlockRecord := ReadID(APBit);
end;

procedure TdwgMLeaderObjectContextData.ReferencesContext(AReader: TdwgReader);
var
  J, K: Integer;
  E: TsgCADMLeaderAnnotContextAccess;
  vItem: TsgMLeaderContextDataAccess;
  vMLeader: TsgMLeaderAccess;
begin
  E := TsgCADMLeaderAnnotContextAccess(FEntity);
  vItem := TsgMLeaderContextDataAccess(E.Item);
  for J := 0 to vItem.MLeadersCount - 1 do
  begin
    if Version >= acR2010 then
    begin
      vMLeader := TsgMLeaderAccess(vItem.MLeaders[J]);
      for K := 0 to vMLeader.LinesCount - 1 do
      begin
        vMLeader.Lines[K].LineType := TsgDXFLineType(AReader.ObjByHandle(FHandles[J][K].LineType, TsgDXFLineType));
        vMLeader.Lines[K].Arrow := TsgDXFBlockRecord(AReader.ObjByHandle(FHandles[J][K].Arrow, TsgDXFBlockRecord));
      end;
    end;
    if vItem.HasTextContents then
      vItem.TextStyle := TsgDXFStyle(AReader.ObjByHandle(FTextStyle, TsgDXFStyle))
    else
      if vItem.HasContentsBlock then
        vItem.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecord, TsgDXFBlockRecord));
  end;
end;

{ TdwgMLeaderStyle }

function TdwgMLeaderStyle.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I: System.Integer;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and (AAppID = Reader.FACADMLeaderVerAppID) then
  begin
    Result := True;
    if AEEData.HasCode(1070, @I) and (AEEData.DataType[I] = edtInt16) then
       FObjectVersion := AEEData.DataInt16[I];
  end;
end;

procedure TdwgMLeaderStyle.ReadData(var APBit: TsgInt64);
var
  E: TsgCADMLeaderStyle;
begin
  ReadObjHeader(APBit);
  E := TsgCADMLeaderStyle(FEntity);
  if Version >= acR2010 then
    FObjectVersion := ReadBS(APBit); // 179
  E.ContentType := ReadBS(APBit); // 170
  E.DrawMLeaderOrder := ReadBS(APBit); // 171
  E.DrawLeaderOrder := ReadBS(APBit); // 172
  E.MaxNumberOfPoints := ReadBL(APBit); // 90
  E.FirstSegmentAngle := ReadBD(APBit) * f180DividedByPi; // 40
  E.SecondSegmentAngle := ReadBD(APBit) * f180DividedByPi; // 41
  E.LeaderLineType := ReadBS(APBit); // 173
  E.ColorCAD := ReadColor(APBit); // 91
//  FLType := ReadID(APBit); // 340
  E.LineWeight := GetLineWeight(ReadBL(APBit)); // 92
  E.LandingEnabled := ReadBit(APBit) = 1; // 290
  E.LandingGap := ReadBD(APBit); // 42
  E.DogLegEnabled := ReadBit(APBit) = 1; // 291
  E.LandingDistance := ReadBD(APBit); // 43
  E.StyleDescription := ReadText; // 3
//  FArrow := ReadID(APBit); // 341
  E.ArrowHeadSize := ReadBD(APBit); // 44
  E.Text := ReadText; // 300
//  FStyle := ReadID(APBit); // 342
  E.LeftAttachment := ReadBS(APBit); // 174
  E.RightAttachment := ReadBS(APBit); // 178
  if Version <> acR2007 then
    E.TextAngleType := ReadBS(APBit); // 175
  E.TextAlignmentType := ReadBS(APBit); // 176
  E.TextColor := ReadColor(APBit); // 93
  E.TextHeight := ReadBD(APBit); // 45
  E.TextFrameEnabled := ReadBit(APBit) = 1; // 292
  if Version <> acR2007 then
    E.AlwaysAlignTextLeft := ReadBit(APBit) = 1; // 297
  E.AlignSpace := ReadBD(APBit); // 46
//  FBlockRecord := ReadID(APBit); // 343
  E.BlockColor := ReadColor(APBit); // 94
  E.BlockScale := Read3D(APBit); //47,49,140
  E.IsBlockScaleEnabled := ReadBit(APBit) = 1; // 293
  E.BlockRotation := ReadBD(APBit) * f180DividedByPi; // 141
  E.IsBlockRotationEnabled := ReadBit(APBit) = 1; // 294
  E.BlockConnectionType := ReadBS(APBit); // 177
  E.ScaleFactor := ReadBD(APBit); // 142
  if E.ScaleFactor = 0 then E.ScaleFactor := 1.0;  
  SeekGeneral(APBit, [seB]); // 295 Property changed...
  E.Annotative := ReadBit(APBit) = 1; // 296
  E.BreakSize := ReadBD(APBit); // 143
  if Version >= acR2010 then
  begin
    E.AttachmentDirection := ReadBS(APBit); // 271
    E.TopAttachment := ReadBS(APBit); // 273
    E.BottomAttachment := ReadBS(APBit); // 272
  end;
  ReadHandleRefs(APBit);
end;

procedure TdwgMLeaderStyle.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  FLType := ReadID(APBit); // 340
  FArrow := ReadID(APBit); // 341
  FStyle := ReadID(APBit); // 342
  FBlockRecord := ReadID(APBit); // 343
end;

procedure TdwgMLeaderStyle.References(AReader: TdwgReader);
var
  E: TsgCADMLeaderStyle;
begin
  inherited References(AReader);
  E := TsgCADMLeaderStyle(FEntity);
  E.LineType := TsgDXFLineType(AReader.ObjByHandle(FLType, TsgDXFLineType));
  E.Arrow := TsgDXFBlockRecord(AReader.ObjByHandle(FArrow, TsgDXFBlockRecord));
  E.TextStyle := TsgDXFStyle(AReader.ObjByHandle(FStyle, TsgDXFStyle));
  E.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecord, TsgDXFBlockRecord));
end;

{ TdwgMultiLeader }

function TdwgMultiLeader.ApplyEED(const AEEData: TsgCADExtendedData;
  const AAppID: UInt64): Boolean;
var
  I: Integer;
begin
  Result := inherited ApplyEED(AEEData, AAppID);
  if not Result and (AAppID = Reader.FACADMLeaderVerAppID) then
  begin
    Result := True;
    if AEEData.HasCode(1070, @I) and (AEEData.DataType[I] = edtInt16) then
       FContext.FObjVersion := AEEData.DataInt16[I];
  end;
end;

constructor TdwgMultiLeader.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create(AVersion);
  FContext := TdwgMLeaderObjectContextData.Create(Version);
end;

destructor TdwgMultiLeader.Destroy;
begin
  FContext.FEntity := nil;
  FContext.FStringReader := nil;
  FContext.Free;
  inherited Destroy;
end;

procedure TdwgMultiLeader.ReadEntData(var APBit: TsgInt64);
var
  I, vArrowHeadsCount, vLabelsCount: Integer;
  E: TsgCADMultiLeaderAccess;
  vArrowHeads: TsgDXFEntityArray;
  vLabels: TsgMLeaderLabels;

  function IntToMLeaderProps(const AValue: Integer): TsgDXFMLeaderProps;
  begin
    Result := TsgDXFMLeaderProps(AValue);
  end;

begin
  E := TsgCADMultiLeaderAccess(FEntity);

  FContext.FOwnerObject := FEntity;
  FContext.FOwner := FHandle;
  FContext.FEntity := TsgCADMultiLeaderAccess(FEntity).Context;
  FContext.FHndsStart := FHndsStart;
  FContext.FPHnds := FPHnds;
  FContext.FStrsStart := FStrsStart;
  FContext.Reader := Reader;
  FContext.FStringReader := FStringReader;
  if Version >= acR2010 then
    FContext.FObjVersion := ReadBS(APBit);// 270 Version (expected to be 2).
  FContext.ReadContext(APBit);

  // 340 mleaderstyle
  E.OverrideFlags := IntToMLeaderProps(ReadBL(APBit)); // 90
  E.Properties.LeaderLineType := ReadBS(APBit); // 170
  E.Properties.ColorCAD := ReadColor(APBit); // 91
  // 341 ltype
  E.Properties.LineWeight := GetLineWeight(ReadBL(APBit)); // 171
  E.Properties.LandingEnabled := ReadBit(APBit) = 1; // 290
  E.Properties.DogLegEnabled := ReadBit(APBit) = 1; // 291
  E.Properties.LandingDistance := ReadBD(APBit); // 41
  // 342 arrow
  E.Properties.ArrowHeadSize := ReadBD(APBit); // 42
  E.Properties.ContentType := ReadBS(APBit); // 172
  // 343 text style handle
  E.Properties.LeftAttachment := ReadBS(APBit); // 173
  E.Properties.RightAttachment := ReadBS(APBit); // 95
  E.Properties.TextAngleType := ReadBS(APBit); // 174
  SeekGeneral(APBit, [seBS]); // 175
  E.Properties.TextColor := ReadColor(APBit); // 92
  E.Properties.TextFrameEnabled := ReadBit(APBit) = 1; // 292
  // 344 blockrecord handle
  E.Properties.BlockColor := ReadColor(APBit); // 93
  E.Properties.BlockScale := Read3D(APBit); // 10,20,30
  E.Properties.BlockRotation := ReadBD(APBit) * f180DividedByPi; // 43
  E.Properties.BlockConnectionType := ReadBS(APBit); // 176
  E.Annotative := ReadBit(APBit) = 1; // 293

  if Version <= acR2007 then
  begin
    vArrowHeadsCount := ReadBL(APBit);
    SetLength(vArrowHeads, vArrowHeadsCount);
    E.Arrows := vArrowHeads;
    for I := 0 to vArrowHeadsCount - 1 do
    begin
      SeekGeneral(APBit, [seB]); // 94 (Is default?); 345
      //E.Arrows[I] := TsgDXFEntity(TsgLinkObject.Create(ReadID(APBit)));345
    end;
  end;

  vLabelsCount := ReadBL(APBit);
  SetLength(vLabels, vLabelsCount);
  E.Labels := vLabels;
  for I := 0 to vLabelsCount - 1 do
  begin
    //E.Labels[I].Attdef := TsgDXFAttdef(TsgLinkObject.Create(ReadID(APBit))); // 330 attdef
    E.Labels[I].Text := ReadText; // 302
    E.Labels[I].ID := ReadBS(APBit); // 177
    E.Labels[I].Width := ReadBD(APBit); // 44
  end;

  E.IsTextDirectionNegative := ReadBit(APBit) = 1; // 294
  E.TextAlignIPE := ReadBS(APBit); // 178
  E.Justification := ReadBS(APBit); // 179
  E.Properties.ScaleFactor := ReadBD(APBit); // 45

  if Version >= acR2010 then
  begin
    E.Properties.AttachmentDirection := ReadBS(APBit); // 271
    E.Properties.TopAttachment := ReadBS(APBit); // 273
    E.Properties.BottomAttachment := ReadBS(APBit); // 272
    if Version >= acR2013 then
      E.LeaderExtendedToText := ReadBit(APBit) = 1; // 295
  end;
end;

procedure TdwgMultiLeader.ReadData(var APBit: TsgInt64);
var
  I: Integer;
  E: TsgCADMultiLeaderAccess;
begin
  inherited ReadData(APBit);
  if Assigned(FContext) then
    FContext.ReadContentHandles(APBit);

  FStyle := ReadID(APBit);
  FLineType := ReadID(APBit);
  FArrow := ReadID(APBit);
  FTextStyle := ReadID(APBit);
  FBlockRecord := ReadID(APBit);

  E := TsgCADMultiLeaderAccess(FEntity);
  SetLength(FArrows, Length(E.Arrows));
  for I := Low(FArrows) to High(FArrows) do
    FArrows[I] := ReadID(APBit);
  SetLength(FAttdefs, Length(E.Labels));
  for I := Low(FAttdefs) to High(FAttdefs) do
    FAttdefs[I] := ReadID(APBit);
end;

procedure TdwgMultiLeader.References(AReader: TdwgReader);
var
  I: Integer;
  E: TsgCADMultiLeaderAccess;
begin
  inherited References(AReader);
  if Assigned(FContext) then
    FContext.ReferencesContext(AReader);
  E := TsgCADMultiLeaderAccess(FEntity);
  E.Style := TsgCADMLeaderStyle(AReader.ObjByHandle(FStyle, TsgCADMLeaderStyle));
  E.Properties.LineType := TsgDXFLineType(AReader.ObjByHandle(FLineType, TsgDXFLineType));
  E.Properties.Arrow := TsgDXFBlockRecord(AReader.ObjByHandle(FArrow, TsgDXFBlockRecord));
  E.Properties.TextStyle := TsgDXFStyle(AReader.ObjByHandle(FTextStyle, TsgDXFStyle));
  E.Properties.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecord, TsgDXFBlockRecord));

  for I := Low(FArrows) to High(FArrows) do
    E.Arrows[I] := AReader.ObjByHandle(FArrows[I], TsgDXFBlockRecord);
  for I := Low(FAttdefs) to High(FAttdefs) do
    E.Labels[I].Attdef := TsgDXFAttdef(AReader.ObjByHandle(FAttdefs[I], TsgDXFAttdef));
end;

{ TdwgEmbeddedMText }

procedure TdwgEmbeddedMText.ReadEntProxyData(var APBit: TsgInt64);
begin// do nothing
end;

procedure TdwgEmbeddedMText.ReadHandleRefs(var APBit: TsgInt64);
begin// do nothing
end;

procedure TdwgEmbeddedMText.ReadStyleHandleData(var APBit: TsgInt64);
begin// do nothing
end;

{ TdwgGroup }

procedure TdwgGroup.ReadData(var APBit: TsgInt64);
begin
  ReadObjHeader(APBit);
  FEntity.Description := ReadText;
  TsgCADGroup(FEntity).Unnamed := ReadBS(APBit) = 1;
  TsgCADGroup(FEntity).Selectable := ReadBS(APBit) = 1;
  FSubEnts := ReadBL(APBit);
  ReadHandleRefs(APBit);
  ReadSubEntIDs(APBit);
end;

procedure TdwgGroup.ReadSubEntIDs(var APBit: TsgInt64; const AFlags: Integer);
var
  I: Integer;
begin
  if FSubEnts > 0 then
  begin
    ForceChildrenList(FSubEnts);
    FFirstChildHandle := ReadID(APBit);
    ChildrenAdd(FFirstChildHandle);
    for I := 1 to FSubEnts - 1 do
      ChildrenAdd(ReadID(APBit));
  end;
end;

procedure TdwgGroup.ReadSubEnts(AReader: TdwgReader);
var
  I, C: Integer;
begin
  I := AReader.FReaderGroupsStack2004.Add(Self);
  try
    C := GetChildrenCount;
    FChildIndex := 0;
    while (FChildIndex < C) and not AReader.Stopped do
    begin
      FEntity.AddEntity(Child[Children[FChildIndex]]);
      Inc(FChildIndex);
    end;
  finally
    AReader.FReaderGroupsStack2004.Delete(I);
  end;
end;

{ TdwgArrows }

constructor TdwgArrows.Create;
begin
  inherited Create;
  FillChar(FLink, SizeOf(FLink), 0);
end;

destructor TdwgArrows.Destroy;
var
  I: TsgDimNameVal;
begin
  for I := Low(FLink) to High(FLink) do
    TsgCADExtendedDataAccess.DisposeData(@FLink[I], True);
  inherited Destroy;
end;

procedure TdwgArrows.SetArrows(Index: TsgDimNameVal; const Value: TsgExtData);
begin
  TsgCADExtendedDataAccess.DisposeData(@FLink[Index], True);
  FillChar(FLink[Index], SizeOf(FLink[Index]), 0);
  case Value.EType of
    edtInt64: Handle[Index] := Value.EInt64;
    edtString: Name[Index] := TsgCADExtendedDataAccess.GetStringFromExtData(Value);
  end;
end;

procedure TdwgArrows.SetHandle(Index: TsgDimNameVal;
  const Value: UInt64);
begin
  if FLink[Index].EType = edtString then
    TsgCADExtendedDataAccess.DisposeData(@FLink[Index], True);
  FLink[Index].EType := edtInt64;
  FLink[Index].EInt64 := Value;
end;

procedure TdwgArrows.SetName(Index: TsgDimNameVal; const Value: string);
begin
  if FLink[Index].EType <> edtString then
  begin
    TsgCADExtendedDataAccess.DisposeData(@FLink[Index], True);
    FLink[Index].EType := edtString;
  end;
  TsgCADExtendedDataAccess.SetExtDataString(@FLink[Index], Value);
end;

function TdwgArrows.GetArrows(Index: TsgDimNameVal): TsgExtData;
begin
  Result := FLink[Index];
end;

function TdwgArrows.GetHandle(Index: TsgDimNameVal): UInt64;
begin
  Result := cnstBadHandle;
  if FLink[Index].EType = edtInt64 then
    Result := FLink[Index].EInt64;
end;

function TdwgArrows.GetName(Index: TsgDimNameVal): string;
begin
  Result := '';
  if FLink[Index].EType = edtString then
    Result := TsgCADExtendedDataAccess.GetStringFromExtData(FLink[Index]);
end;

function TdwgArrows.Get_Type(Index: TsgDimNameVal): TsgExtDataType;
begin
  Result := FLink[Index].EType;
end;

function TdwgArrows.IsEmptyLink(Index: TsgDimNameVal): Boolean;
begin
  Result := not (FLink[Index].EType in [edtInt64, edtString]);
end;

function TdwgArrows.IsEqualLinks(Index1, Index2: TsgDimNameVal): Boolean;
begin
  Result := False;
  if FLink[Index1].EType = FLink[Index2].EType then
  begin
    if FLink[Index1].EType = edtString then
      Result := sgSameText(Name[Index1], Name[Index2])
    else
      Result := FLink[Index1].EInt64 = FLink[Index2].EInt64
  end;
end;

{ TsgHandleLink }

function TsgHandleLink.GetAsHandle: UInt64;
begin
  Result := FValue;
end;

function TsgHandleLink.GetAsString: string;
begin
  Result := '';
end;

function TsgHandleLink.GetPropItem: TObject;
begin
  Result := TsgDXFEntity(Collection).FindEntByHandle(AsHandle);
end;

procedure TsgHandleLink.SetAsHandle(const Value: UInt64);
begin
  FValue := Value;
end;

procedure TsgHandleLink.SetAsString(const Value: string);
begin
end;

{ TsgDatabaseHandleLink }

function TsgDatabaseHandleLink.GetPropItem: TObject;
var
  I: Integer;
begin
  Result := nil;
  I := TsgObjectsMapAccess(Collection).IndexOfHandle(AsHandle);
  if I >= 0 then
    Result := TsgObjectsMapAccess(Collection).GetPMapElement(I)^.Obj;
end;

{ TsgNameLink }

destructor TsgNameLink.Destroy;
begin
  Finalize(PString(@FValue)^);
  inherited Destroy;
end;

function TsgNameLink.GetAsHandle: UInt64;
begin
  Result := cnstBadHandle;
end;

function TsgNameLink.GetAsString: string;
begin
  Result := PString(@FValue)^;
end;

function TsgNameLink.GetPropItem: TObject;
begin
  Result := TsgDXFEntity(Collection).FindEntByName(AsString);
end;

procedure TsgNameLink.SetAsHandle(const Value: UInt64);
begin
end;

procedure TsgNameLink.SetAsString(const Value: string);
begin
  PString(@FValue)^ := Value;
end;

{ TsgObjCollection }

function TsgObjCollection.GetObj(Handle: UInt64): TObject;
var
  I: Integer;
begin
  Result := nil;
  I := FMap.IndexOfHandle(Handle);
  if I >= 0 then
    Result := FMap.GetPMapElement(I)^.Obj;
end;

{ TdwgCustomIrdRecord }

procedure TdwgCustomIrdRecord.ReadData(var APBit: TsgInt64);
var
  vAttributes: IsgGlobalPropProvider;
  vVarData: TVarData;
  vArr: TVarArray;
  vDataSize: Integer;
begin
  ReadObjHeader(APBit);
  FClassIndex := ReadBS(APBit); //1070: 516 == $204
  FID := ReadBL(APBit);//1071: 0 - dsc; > 0 - obj;
  SeekGeneral(APBit, [seBL]);// unknown 0 (or BB??)
  FillChar(vArr, SizeOf(vArr), 0);
  vArr.DimCount := 1;
  vArr.Flags := $0001;//FADF_AUTO;
  vArr.ElementSize := SizeOf(Word);
  vDataSize := ReadBL(APBit) - 2 * (Ord(FID <> 0));
  vArr.Bounds[0].ElementCount := vDataSize div vArr.ElementSize;
  FillChar(vVarData, SizeOf(TVarData), 0);
  vVarData.VType := varWord or varArray;
  vVarData.VArray := @vArr;
  GetMem(vArr.Data, vArr.ElementSize * vArr.Bounds[0].ElementCount);
  ReadBytes(APBit, vArr.Data^, vArr.ElementSize * vArr.Bounds[0].ElementCount);//1004
  if GetGlobalPropProvider(FEntity, False, vAttributes) then
    vAttributes[cnst_ird_data] := Variant(vVarData);
  FreeMem(vArr.Data);
  if FID = 0 then // desc
  begin
    SeekGeneral(APBit, [seBB, seRS]); //BB==0..3, RS==0
    FEntity.Description := ReadText; // class ird ...
    ReadText;
  end;
  ReadHandleRefs(APBit);
end;

{ TdwgIrdObjRecord }

procedure TdwgIrdObjRecord.ReadHandleRefs(var APBit: TsgInt64);
begin
  inherited ReadHandleRefs(APBit);
  FIrdDscRecordHandle := ReadID(APBit);
end;

procedure TdwgIrdObjRecord.References(AReader: TdwgReader);
begin
  inherited References(AReader);
  TsgCADIrdObjRecord(FEntity).IrdDscRecord :=
    TsgCADIrdDscRecord(AReader.ObjByHandle(FIrdDscRecordHandle, nil));
end;

initialization
  @Read3RD := @_Read3RD;
  @Read3D := @_Read3D;
  FiderCodeFromName := nil;
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_DWG := RegisterClipboardFormat('SoftGold DWG Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_DWG, TsgDWGImage);
  TPicture.RegisterFileFormat('dwg', 'AutoCAD DWG', TsgDWGImage);
  TPicture.RegisterFileFormat('dwt', 'AutoCAD DWT', TsgDWGImage);
  TPicture.RegisterFileFormat('gp2', 'AutoCAD DWG', TsgDWGImage);

  TsgDXFConverterAccess.RegisterXRefGraphicClass('dwg', TsgDWGImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('dwt', TsgDWGImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('gp2', TsgDWGImage);

  ProxyLWPLineReader := TProxyLWPLineReader.Create;
  RegisterProxyObjectReader(cnstProxyTypeLWPLine, ProxyLWPLineReader);

finalization
  ClearObjects(FiderCodeFromName);
  FreeAndNil(FiderCodeFromName);
  ClearObjects(ToleranceProps);
  FreeAndNil(ToleranceProps);
  TPicture.UnRegisterGraphicClass(TsgDWGImage);
  TsgDXFConverterAccess.UnRegisterXRefGraphicClass(TsgDWGImage);
  UnRegisterProxyObjectReader(ProxyLWPLineReader);
  ProxyLWPLineReader.Free;

end.

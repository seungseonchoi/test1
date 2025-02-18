{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                  DXF files TGraphic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit DXF;
{$INCLUDE SGDXF.inc}
{$IFDEF SG_BTI}
  {$DEFINE SG_BTI_HANDELS}
{$ENDIF}
{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}
interface

uses

{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LMessages, LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, MVFont,
{$ENDIF}
  sgConsts, CADImage, DXFConv, sgLines,
  sgLists, ExtData, sgFunction
{$IFDEF SGDEL_6}
  ,Variants
{$ENDIF}
{$IFDEF SGDEL_6}
  , DateUtils
{$ENDIF}
  {$IFDEF SG_DECRYPT_FILES}
  , sgStmCrpt
  {$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  , sgEncMgr, sgEncoding;

type
  TsgCADdxfImage = class(TsgImageWithHandles)
  protected
    function CreateConverter: TsgDXFConverter; override;
    function IsAutocadLType: Boolean; override;
  public
    constructor Create; override;
    procedure LoadFromClipboardFormat(AFormat: Word;
      AClipboardObject: THandle; APalette: HPALETTE); {$IFNDEF SGFPC} override; {$ENDIF}
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToClipboardFormat(var AFormat: Word;
      var AClipboardObject: THandle; var APalette: HPALETTE); {$IFNDEF SGFPC} override; {$ENDIF}
  end;

  TsgDXFImage = TsgCADdxfImage;
  TsgDXFImageClass = class of TsgDXFImage;

implementation

uses sgUnicode, Math, sgBitmap, TypInfo, sgComparer;

const
  cnstBlockRefHandleDefault = 0;
  cnstBadBlockRefHandle = 1;

  cnstNumberOfFitDataNeedRead = -1;
  cnstNumberOfFitDataUndefined = -2;

  cnstSection = 'SECTION';
  cnstTable = 'TABLE';
  cnstEndSec = 'ENDSEC';
  cnstSeqEnd = 'SEQEND';
  cnstATTRIBExtdata = 'ACDBATTR';
  cnstEOF = 'EOF';
  cnstVertex = 'VERTEX';

  // if QHashName change need update this hsh-constants!
  hshObjectsPLOTSETTINGS = Cardinal($D9146252);
  hshSectionTABLES = Cardinal($C4179A7D);
  hshTable = Cardinal($EE20BCA0);
  hshAcadXDictionary = Cardinal($552E0715);
  hshAcadReactors = Cardinal($8069D794);
  hshBlkRefs = Cardinal($D113156B);
  hshEOF = Cardinal($3330C0);
  hshEndSec = Cardinal($9A781879);
  hshSeqEnd  = Cardinal($0D23349C);
  hshSectionBLOCKS = Cardinal($5EC02278);
  hshSection = Cardinal($81753241);
  hshVertex = Cardinal($4D1B9B1D);

//  _hshObjectsPLOTSETTINGS: Cardinal = ($0);
//  _hshSectionTABLES: Cardinal = ($0);
//  _hshTable: Cardinal = ($0);
//  _hshAcadXDictionary: Cardinal = ($0);
//  _hshAcadReactors: Cardinal = ($0);
//  _hshBlkRefs: Cardinal = ($0);
//  _hshEOF: Cardinal = ($0);
//  _hshEndSec: Cardinal = ($0);
//  _hshSeqEnd: Cardinal = ($0);
//  _hshSectionBLOCKS: Cardinal = ($0);
//  _hshSection: Cardinal = Cardinal($0);
//  _hshVertex: Cardinal = Cardinal($0);

  cnstCodeValueUndefined = -1;

  cnstLine: TsgLine = ();

type
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgCADHatchAccess = class(TsgCADHatch);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgDXFTableAccess = class(TsgDXFTable);
  TsgDXFDimensionPropertiesAccess = class(TsgDXFDimensionProperties);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFAcadTableStyleAccess = class(TsgDXFAcadTableStyle);
  TsgDXFPlotSettingsAccess = class(TsgDXFPlotSettings);
  TsgDXFSplineAccess = class(TsgDXFSpline);
  TsgDXFScaleAccess = class(TsgDXFScale);

  TDXFProxyReader = class(TCustomAlignedDataReader);
  TsgCADExtendedDataAccess = class(TsgCADExtendedData);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  TsgCADMLeaderAnnotContextAccess = class(TsgCADMLeaderAnnotContext);
  TsgMLeaderLineAccess = class(TsgMLeaderLine);
  TsgMLeaderAccess = class(TsgMLeader);
  TsgCADMLeaderStyleAccess = class(TsgCADMLeaderStyle);
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgMLeaderContextDataAccess = class(TsgMLeaderContextData);
{$IFDEF SG_BLOCK_PREVIEW}
  TsgBitmapAccess = class(TsgBitmap);
{$ENDIF}
  TsgProxyReaderImplAcceess = class(TsgProxyReaderImpl);
  TsgCollectionAccess = class(TsgCollection);
  TsgObjectCollectionAccess = class(TsgObjectCollection);
  TdxfObject = class;
  TdxfEntity = class;

  TdxfObjectClass = class of TdxfObject;

  PsgHandleNameLinkItem = ^TsgHandleNameLinkItem;
  TsgHandleNameLinkItem = packed record
    ID: UInt64;
    Name: string;
  end;

  TsgHandleNameLinkItemArray = array of TsgHandleNameLinkItem;

{$IFDEF SGDEL_2010}
  {$RTTI INHERIT METHODS([]) PROPERTIES([vcPublished]) FIELDS([])}
{$ENDIF}

  {$M+}
  TsgDXFEntityAccess = class(TsgDXFEntity)
  published
    property Layer;
    property LineType;
    property Material;
  end;

  TsgDXFInsertAccess = class(TsgDXFInsert)
  published
    property Layer;
    property LineType;
    property BlockRecord;
  end;

  TsgDXFDimStyleAccess = class(TsgDXFDimensionStyle)
  published
    property TextStyle;
    property DIMLDRBLK;
    property DIMBLK;
    property DIMBLKT;
    property DIMBLK1;
    property DIMBLK2;
  end;

  TsgDXFImageEntAccess = class(TsgDXFImageEnt)
  published
    property ImageDef;
  end;

  TsgDXFLeaderAccess = class(TsgDXFLeader)
  published
    property DimStyle;
    property DIMLDRBLK;
  end;

  TsgDXFViewportAccess = class(TsgDXFViewport)
  published
    property ClippingBoundary;
  end;

  TsgDXFDimensionAccess = class(TsgDXFDimension)
  published
    property Style;
  end;

  TsgDXFMTextAccess = class(TsgDXFMText)
  published
    property Style;
  end;

  TsgDXFTextAccess = class(TsgDXFText)
  published
    property StyleRef;
  end;

  TsgAcadTableCellAccess = class(TsgAcadTableCell)
  published
    property TextStyle;
  end;

  TsgAcadTableCellStyleAccess = class(TsgAcadTableCellStyle)
  published
    property TextStyle;
  end;

  TsgCADMLineAccess = class(TsgCADMLine)
  published
    property Style;
  end;

  TsgCADIrdObjRecordAccess = class(TsgCADIrdObjRecord)
  published
    property IrdDscRecord;
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
    procedure DoLink(Instance: TObject; P: PPropInfo); virtual;
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

  TsgLTElementStyleLink = class(TsgHandleLink)
  protected
    FIndex: Integer;
    procedure DoLink(Instance: TObject; P: PPropInfo); override;
  public
    constructor Create(const AHandle: UInt64; AIndex: Integer = -1);
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

  TsgLayerLink = class(TsgNameLink)
  end;

  TsgLTypeLink = class(TsgNameLink)
  end;

  TsgBlockRecordLink = class(TsgNameLink)
  end;

  TsgDimStyleLink = class(TsgNameLink)
  end;

  TsgStyleLink = class(TsgNameLink)
  end;

  TsgImageDefLink = class(TsgNameLink)
  end;

  TObjectLinkRec = record
    obj: TObject;
    Prop: PPropInfo;
    Link: TsgObjectLink;
  end;

  TObjectLinkRecArray = array of TObjectLinkRec;

  TsgDXFPaperBlocksRefs = class(TsgDXFGroup)
  protected
    FHandlers: TStringList;
    function FindBlock(const ABlock: TsgDXFBlock; var Index: Integer): Boolean;
    procedure ListNotify(const Obj: TObject; Action: TListNotification); override;
    procedure UpdateViewportsOrdIndex;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TObjectArray = array of TObject;

  TsgQObjectsList = class
  private
    FCount: Integer;
    FList: TObjectArray;
    FSearchItem: TObject;
    FSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
    function GetObject(Index: Integer): TObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function CustomFindItem(const AItem: TObject; var Index: Integer): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function FindItem(AHandle: UInt64; var Index: Integer): Boolean; virtual; abstract;
    procedure Clear;
    function AddItem(const AItem: TObject): Integer;
    procedure InsertFinded(Index: Integer; const Item: TObject);
    procedure Delete(Index: Integer);
    procedure Sort; virtual;
    property Sorted: Boolean read FSorted write SetSorted;
    property List: TObjectArray read FList;
    property Count: Integer read FCount;
    property Objects[Index: Integer]: TObject read GetObject; default;
  end;

  TsgQReaderMetaclsssList = class(TsgQObjectsList)
  protected
    function CustomFindItem(const AItem: TObject; var Index: Integer): Boolean; override;
  public
    constructor Create; override;
    procedure Sort; override;
    function FindItem(AHandle: UInt64; var Index: Integer): Boolean; override;
  end;

  TsgQCADItemsList = class(TsgQObjectsList)
  protected
    function CustomFindItem(const AItem: TObject; var Index: Integer): Boolean; override;
  public
    constructor Create; override;
    procedure Sort; override;
    function FindItem(AHandle: UInt64; var Index: Integer): Boolean; override;
  end;

  TsgObjCollection = class(TInterfacedObject, IsgObjCollection)
  protected
    FMap: TsgQCADItemsList;
    { IsgObjCollection }
    function GetObj(Handle: UInt64): TObject;
  end;

  TsgPaperSpaceBlockListNotifyHandler = class
  private
    FBlock: TsgDXFBlock;
    FViewportWithMinID: TsgDXFViewport;
    FListNotify: TsgObjectListNotify;
  protected
    procedure DoListNotify(const Obj: TObject; Action: TListNotification);
    procedure ListNotifyHandler(const Obj: TObject; Action: TListNotification);
    function UpdateViewportOrdIndex: Boolean;
  public
    constructor Create(const ABlock: TsgDXFBlock); virtual;
    destructor Destroy; override;
    property Block: TsgDXFBlock read FBlock;
  end;

  TdxfReaderMetaclass = class
    Handle: UInt64;
    Name: string;
    EntClass: TsgDXFEntityClass;
  end;

  TCheckHandle = function (var AHandle: UInt64): Boolean of object;

  // Reader

  TdxfReader = class
  private
    FCode: Integer;
    FConverter: TsgDXFConverter;
    FFileInfo: TStringList;
    FLimit: PAnsiChar;
    FLine: Integer;
    FMain: TdxfEntity;
    FPos: PAnsiChar;
    FPrev: PAnsiChar;
    FInt: Integer;
    FInt64: Int64;
    FDouble: Double;
    FHandle: UInt64;
    FValueStartPos: PAnsiChar;
    FValueSize: Integer;
    FNamedObjects: TsgDXFDictionary;
    FLayouts: UInt64;
    FImageDefs: UInt64;
    FGroups: UInt64;
    FTableStyles: UInt64;
    FDictionaryVariables: UInt64;
    FFieldList: TsgDXFFieldList;
    FPapers: TsgDXFGroup;
    FBlocksListNotify: TsgObjectListNotify;
    FActiveVPort: TsgDXFVport;
    FSortents: Integer;
    FAppIDs: TsgDXFEntity;
    FDXFEED: TsgObjectCollection;
    FCSTSaved: Boolean;
    FProxyReaderImpl: TsgProxyReaderImpl;
    FXInfoOffs: Int64;
    FXInfo: TsgDXFXRecord;
    FXInfoHandle: UInt64;
{$IFDEF SG_BTI_FIX_ENT}
    FBlockForBTIObjectBad: TsgDXFBlock;
{$ENDIF}
    FNameHash: Cardinal;
    FRefsHash: Cardinal;
    FDataBase: TsgQCADItemsList;
    FLinksCount: Integer;
    FLinks: TObjectLinkRecArray;
    FSections: array[TConvSection] of TStrings;
    FNamesCount: Integer;
    FNames: TsgHandleNameLinkItemArray;
    FGlobalNames: TStrings;
    FNoLayersTable: TStrings;
    FClasses: TsgQReaderMetaclsssList;
    FUpdateEvent: TNotifyEvent;
    FLayoutsPerBlockRecord: TsgObjectCollection;
    FCheckHandle: TCheckHandle;
    FEnc: IEncoding;
    procedure ConvertDXFEED;
    procedure DoEvent(Event: TNotifyEvent);
    function GetStrValue: string;
    function GetUValue: string;
    function GetFileInfo: string;
    procedure ReadDouble(var AValue: Double);
    procedure ReadHandle(var AValue: UInt64);
    procedure ReadInt(var AValue: Integer);
    procedure ReadCode(var AValue: Integer);
    procedure ReadHash(var AValue: Cardinal);
    procedure ReadFileInfo;
    procedure SeekEOL;{$IFDEF SG_INLINE} inline;{$ENDIF}

    function GetConv: TsgDXFConverterAccess;

    function GetXDataProcs: TsgXDataProcs;
    function GetVersion: Byte;
    function IsACADAppID(const AAppId: string): Boolean;
    function IsRelatedAppID(AAppId: string): Boolean;
    function IsURLAppID(const AAppId: string): Boolean;

    procedure Coord(APoints: PFPointArray);

    procedure InitLayouts(const AModel, APaper: TsgDXFBlock);
    function GetAppIDEnt(const AName: string): TsgDXFEntity;
    procedure DoSetLoading;
    procedure UpdateViewportsOrdIndex;
    procedure BlocksListNotifyHandler(const Obj: TObject; Action: TListNotification);
    function GetProxyReaderImpl: TsgProxyReaderImpl;
    procedure PrelinkObjectLinkCollections;
    procedure LinkObjectLinkCollections;
    function DoAddLink(obj: TObject; AProp: Pointer; ALink: TsgObjectLink): Integer;
    function AddLink(obj: TObject; const APropName: string; const AValue: string; AProxyClass: TClass): Integer; overload;
    function AddLink(obj: TObject; AProp: Pointer; const AValue: string; AProxyClass: TClass): Integer; overload;
    function AddLink(obj: TObject; const APropName: string; const AValue: UInt64; AProxyClass: TClass): Integer; overload;
    function AddLink(obj: TObject; AProp: Pointer; const AValue: UInt64; AProxyClass: TClass): Integer; overload;
    function GetItemByName(Section: TConvSection;
      const Name: string): TsgDXFEntity;
    procedure BlockRecordDestroy(Sender: TObject);
    procedure EntityDestroy(Sender: TObject);
    function AppendLinkNames(Handle: UInt64; const AName: string): Integer;
    function GetGlobalNames(const Name: string): string;{$IFDEF SG_INLINE} inline;{$ENDIF}
    procedure DoSetNames;
    procedure FixDimStyleBlockRecordRefsToPaperSpace;
    function NoCheckHandle(var AHandle: UInt64): Boolean;
    procedure UpdateLimits;
    function IsValueEqual(const AValue: AnsiString): Boolean;{$IFDEF UNICODESTRING_TYPE_DEFINED} overload;{$ENDIF}
{$IFDEF UNICODESTRING_TYPE_DEFINED}
    function IsValueEqual(const AValue: UnicodeString): Boolean; overload;
{$ENDIF}
  protected
    function AppendString(var AStr: AnsiString): Integer;
    procedure AddBinData(var AData: Pointer; var ASize: Integer); virtual;
    function CreateBlock(const AName: string): TsgDXFBlock;
    procedure DoCreate(AEntity: TsgDXFEntity);
    function ObjByHandle(const AHandle: UInt64; AEntityClass: TsgDXFEntityClass = nil): TsgDXFEntity;
    function EOF: Boolean;
    function IsInternalCADImage: Boolean;
    procedure ForcePaperSpaces;
    procedure ForceStandardObjects; virtual;
    procedure ForceNamedObjectsDictionary; virtual;
    procedure InitPaper(const ABlock: TsgDXFEntity);
    procedure ReadEED(AObj: TdxfObject);
    procedure StoreEED(AObj: TdxfObject);
    procedure UpdateDatabase(AObj: TdxfObject);
    procedure UpdateMaterials(AObj: TdxfObject);
    function FindObject(AHandle: UInt64; var Index: Integer): Boolean;
    function CheckXInfo: UInt64;
    procedure UpdateXInfo(AXrec: TsgDXFXRecord);
    procedure UpdateClasses(AClasses: TdxfObject);
    function RemoveLayoutPerBlockRecord(ALayout: TsgDXFEntity): Integer;
    property XDataProcs: TsgXDataProcs read GetXDataProcs;
    property ProxyReaderImpl: TsgProxyReaderImpl read GetProxyReaderImpl;
    property DataBase: TsgQCADItemsList read FDataBase;
    property Names: TsgHandleNameLinkItemArray read FNames;
    property GlobalNames[const Name: string]: string read GetGlobalNames;
  public
    constructor Create(Conv: TsgDXFConverter);
    destructor Destroy; override;
    function FloatValue: Double; virtual;
    function HandleValue: UInt64; virtual;
    function IntValue: Integer; virtual;
    procedure Read;
    procedure Next; virtual;
    class function ACIS_Row_Decode(const AEncoded: AnsiString): AnsiString; virtual;

    property Code: Integer read FCode write FCode;
    property FileInfo: string read GetFileInfo;
    property StrValue: string read GetStrValue;
    property UValue: string read GetUValue;
    property Version: Byte read GetVersion;

    property Conv: TsgDXFConverterAccess read GetConv;
    property AppIDEnt[const AName: string]: TsgDXFEntity read GetAppIDEnt;
    property ItemByName[Section: TConvSection; const Name: string]: TsgDXFEntity read GetItemByName;
  end;

  TdxfBinReader = class(TdxfReader)
  private
    FGroupCodeSize: Integer;
    procedure Read(var ABuffer; ACount: Integer);
  protected
    procedure AddBinData(var AData: Pointer; var ASize: Integer); override;
  public
    constructor Create(AConverter: TsgDXFConverter; const AGroupCodeSize: Integer);
    function FloatValue: Double; override;
    function HandleValue: UInt64; override;
    function IntValue: Integer; override;
    procedure Next; override;
    class function ACIS_Row_Decode(const AEncoded: AnsiString): AnsiString; override;
  end;

  TdxfReadProperty = procedure(const AReader: TdxfReader) of object;

  TdxfObject = class
  private
    FAdded: Boolean;
    FDatabaseAdded: Boolean;
    FComplex: Boolean;
    FEnt: TsgDXFEntity;
    FHandle: UInt64;
    FHardOwnerIDs: TsgCollection;//not sorted
    FPaperSpace: Integer;
    FPointerIDs: TsgInt64List;
    FRctrsHardIDs: TsgInt64List;
    FRctrsSoftIDs: TsgInt64List;
    FXDictionaryIDs: TsgInt64List;
    FSoftOwnerIDs: TsgInt64List;
{$IFDEF SG_USEDXFSRC}
    FSrcEnd: PAnsiChar;
    FSrcStart: PAnsiChar;
{$ENDIF}
    FTexts: array[1..9] of string; // FTexts[5] - not fill, use AReader.FHandle
    FOwner: TdxfObject;
    FObj: TdxfObject;
    FEEDItems: TsgStringList;
    FReadProperty: TdxfReadProperty;
    FACADExtData: TsgCADExtendedData;
    FStackReadPropertyCount: Integer;
    FStackReadProperty: array of TdxfReadProperty;
    FMaterial: UInt64;
    FLayerLinkIndex: Integer;
    FLTypeLinkIndex: Integer;
    FMarker: string;
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    function ApplyXDictionary(AReader: TdxfReader): TsgDXFDictionary; virtual;
    procedure DoCoord(AReader: TdxfReader; P: Pointer; ABaseCode: Integer = 0);
    procedure Pop;
    function Push(const AReadMethod: TdxfReadProperty): Integer;
    procedure ApplyEED(AReader: TdxfReader); virtual;
    function SetLayerByName(AReader: TdxfReader; const AName: string): Integer; virtual;
    function SetLTypeByName(AReader: TdxfReader; const AName: string): Integer; virtual;
    function EraseEED(var AData: TsgCADExtendedData): Integer;
    function ReplaceFromDatabase(AReader: TdxfReader; var Index: Integer;
      out ASoftOwner: TsgDXFEntity): Integer; virtual;
    procedure UpdateLinks(AReader: TdxfReader); virtual;
    procedure UpdateOwner(AReader: TdxfReader); virtual;
  protected
    constructor Create; virtual;
    destructor Destroy; override;
    function AddEntity(const E: TdxfObject): Boolean; virtual;
    procedure ApplyHandle(const AReader: TdxfReader); virtual;
    procedure Commit(const AReader: TdxfReader); virtual;
    function CreateSubEntity(const AReader: TdxfReader; ANameHash: Cardinal): TdxfObject; virtual;
    procedure DestroyObj; virtual;
    function EOFEnt(AObj: TdxfObject): Boolean; virtual;
    function GetConv: TsgDXFConverterAccess; virtual;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); virtual;
    function OwnerAdd: Integer; virtual;
    procedure ReadEntities(AReader: TdxfReader); virtual;
    function ConvertEEDHandle(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; virtual;
    function ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; virtual;
    function ReadEntity(AReader: TdxfReader; ANameHash: Cardinal): Boolean; virtual;
    procedure ReadProps(AReader: TdxfReader); virtual;
    procedure ReadProperty(const AReader: TdxfReader); virtual;
    procedure ReadState(AReader: TdxfReader); virtual;
    function ReleaseEntity: TsgDXFEntity; virtual;
    property Conv: TsgDXFConverterAccess read GetConv;
    property Name: string read GetName write SetName;
  end;

  // Entity

  TdxfEntity = class(TdxfObject)
  private
    FElevation: Double;
    FExtrusion: TFPoint;
    FFloats: array[0..19] of Double;
    FInts: array[0..19] of Integer;
    FIntsRGBColor: TColor;
    FLineWeight: Double;
    FPoints: array[0..9] of TFPoint;
    FZThick: Double;
    FURLExtData: TsgCADExtendedData;
    procedure Coord(AReader: TdxfReader);
    function LoadColor(const AReader: TdxfReader): TsgColorCAD;
    procedure SetLWeight(const AValue: Double);
  protected
    constructor Create; override;
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    function GetExtrusion: TFPoint;
    function GetElevation: Double;
    function GetPoints(const AIndex: Integer;
      const ANeed3D: Boolean = False): TFPoint;
    function GetZThick: Double;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure SkipEmbeddedObject(const AReader: TdxfReader); virtual;
    procedure ReadEmbeddedObjectProperty(const AReader: TdxfReader); virtual;
    property Flags: Integer read FInts[10];
  end;

  TdxfMain = class(TdxfEntity)
  private
    FConv: TsgDXFConverterAccess;
    FSectionObjects: TdxfObject;
    procedure Initialize(AReader: TdxfReader; AObj: TdxfObject);
  protected
    constructor Create; override;
    destructor Destroy; override;
    procedure Commit(const AReader: TdxfReader); override;
    function GetConv: TsgDXFConverterAccess; override;
    function CreateSubEntity(const AReader: TdxfReader; ANameHash: Cardinal): TdxfObject; override;
    procedure DestroyObj; override;
    procedure ReadEntities(AReader: TdxfReader); override;
  end;

  TdxfCustomSection = class(TdxfObject)
  protected
    FReader: TdxfReader;
    constructor Create; override;
    function EOFEnt(AObj: TdxfObject): Boolean; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfTables = class(TdxfCustomSection)
  protected
    function CreateSubEntity(const AReader: TdxfReader; ANameHash: Cardinal): TdxfObject; override;
    procedure ReadEntities(AReader: TdxfReader); override;
  end;

  TdxfBlocks = class(TdxfCustomSection)
  protected
    function EOFEnt(AObj: TdxfObject): Boolean; override;
    function AddEntity(const E: TdxfObject): Boolean; override;
  end;

  TdxfAcdsData = class(TdxfCustomSection)
  end;

  TdxfEntities = class(TdxfCustomSection)
  private
    FPapers: TsgDXFGroup;
  protected
    function AddEntity(const E: TdxfObject): Boolean; override;
  end;

  TdxfObjects = class(TdxfCustomSection)
  protected
    function CreateSubEntity(const AReader: TdxfReader; ANameHash: Cardinal): TdxfObject; override;
    function AddEntity(const E: TdxfObject): Boolean; override;
  end;

  TdxfClasses = class(TdxfCustomSection)
  end;

  TdxfHeader = class(TdxfEntity)
  private
    FFlag_280: Integer;
    FFlag_290: Integer;
    FHeadVarIndex: Integer;
    FHeadVars: PsgHeadVarStruct;
    procedure DoHeadVars(const AReader: TdxfReader);
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    function EOFEnt(AObj: TdxfObject): Boolean; override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    property HeadVarsStruct: PsgHeadVarStruct read FHeadVars write FHeadVars;
  end;

  TdxfEndTab = class(TdxfObject)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfEndBlock = class(TdxfObject)
  end;

  TdxfEndSeq = class(TdxfObject)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfSeqend = class(TdxfObject)
  end;

  TdxfBasePolyLine = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfBlock = class(TdxfEntity)
{$IFDEF SG_BTI}
  private
    FEEDVersionInternal: Integer;
    FExtData: TsgCADExtendedData;
    procedure Reinit(AReader: TdxfReader);
{$ENDIF}
  protected
    constructor Create; override;
{$IFDEF SG_BTI}
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
    function ConvertEEDHandle(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; override;
    function ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; override;
{$ENDIF}
    function IsAcadTableBlock: Boolean;
    procedure Commit(const AReader: TdxfReader); override;
    function EOFEnt(AObj: TdxfObject): Boolean; override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfBlockR9 = class(TdxfBlock)
  protected
    procedure ApplyHandle(const AReader: TdxfReader); override;
  end;

  TdxfCustomDictionaryItem = class(TdxfObject)
  private
    FSoftOwner: TsgDXFEntity;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProps(AReader: TdxfReader); override;
    function OwnerAdd: Integer; override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    function ReplaceFromDatabase(AReader: TdxfReader; var Index: Integer;
      out ASoftOwner: TsgDXFEntity): Integer; override;
    procedure UpdateOwner(AReader: TdxfReader); override;
  end;

  TdxfDictionary = class(TdxfCustomDictionaryItem)
  private
    FNamesCount: Integer;
  protected
    function AddItem(AReader: TdxfReader; const AHandle: UInt64; const AName: string): Integer; virtual;
    procedure Commit(const AReader: TdxfReader); override;
    function OwnerAdd: Integer; override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure ReadState(AReader: TdxfReader); override;
    procedure UpdateOwner(AReader: TdxfReader); override;
  end;

  TcstDictionary = class(TdxfDictionary)
  private
    FXInfoHandle: UInt64;
  protected
    function AddItem(AReader: TdxfReader; const AHandle: UInt64; const AName: string): Integer; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfProxyObject = class(TdxfCustomDictionaryItem)
  protected
    procedure UpdateOwner(AReader: TdxfReader); override;
  end;

  TdxfDimAssoc = class(TdxfObject)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfClass = class(TdxfObject)
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfHatch = class(TdxfEntity)
  private
    FCurentIndexColorCAD: Byte;
    FCurentIndexColor: Byte;
    FBoundNum: Integer;
    FNumberOfFitData: Integer;
    FBoundType: Integer;
    FCurType: Pointer;
    FIsSolid: Boolean;
    FPointCount: Integer;
    FFillColorReaded: Boolean;
    FTransparency: Integer;
    function AddNewPattern(const AHatch: TsgCADHatch): PsgHatchPatternData;
    procedure DoSolidHatch(const AReader: TdxfReader);
    function GetLastHatchSegment(const AHatch: TsgCADHatch): Tsg2DCurve;
    function Pattern(const AHatch: TsgCADHatch): PsgHatchPatternData;
    procedure ReadGrad(const AReader: TdxfReader; const  AHatch: TsgCADHatch);
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfImageDef = class(TdxfCustomDictionaryItem)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure UpdateOwner(AReader: TdxfReader); override;
  end;

  TdxfInsert = class(TdxfEntity)
  private
    FEEDVersionInternal: Integer;
    FExtData: TsgCADExtendedData;
    FBlockRecordLinkIndex: Integer;
    function ChangeEntEx(AReader: TdxfReader): string;
{$IFDEF SG_BTI}
    procedure ChangeEntity(AReader: TdxfReader);
{$ENDIF}
{$IFDEF SG_BTI_FIX_ENT}
    function GetBlockForBTIObjectBad(const AReader: TdxfReader): TsgDXFBlock;
{$ENDIF}
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
{$IFDEF SG_BTI}
    function ConvertEEDHandle(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; override;
    function ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
      AReader: TdxfReader): Integer; override;
{$ENDIF}
    procedure UpdateLinks(AReader: TdxfReader); override;
  end;

  TdxfPlotSettings = class(TdxfCustomDictionaryItem)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure ReadProps(AReader: TdxfReader); override;
  end;

  TdxfDictionaryVar = class(TdxfCustomDictionaryItem)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfLayout = class(TdxfCustomDictionaryItem)
  private
    FBlockRecord: UInt64;
    FPoints: array[0 .. 7] of TFPoint;
    FPlotSettings: TdxfPlotSettings;
  protected
    constructor Create; override;
    destructor Destroy; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure UpdateOwner(AReader: TdxfReader); override;
  end;

  TdxfMLine = class(TdxfEntity)
  private
    FVertex: TsgMVertex;
    FStyle: UInt64;
    procedure NewVertex;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfMLStyle = class(TdxfCustomDictionaryItem)
  private
    function CurrEntry: TsgMLineEntry;
    function NewEntry: TsgMLineEntry;
    procedure ReadEntry(const AReader: TdxfReader);
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfMText = class(TdxfEntity)
  protected
    constructor Create; override;
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfOLE = class(TdxfEntity)
  private
    FData: Pointer;
    FDataSize: Integer;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TdxfPenLine = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfPenTableItem = class(TdxfEntity)
  private
    FPenTableItemWithSameNameAlreadyExistsInOwnerTable: TsgDXFPenTableItem;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    function SetLayerByName(AReader: TdxfReader; const AName: string): Integer; override;
  public
    destructor Destroy; override;
  end;

  TdxfView = class(TdxfPenTableItem)
  end;

  TdxfPoint = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfProxy = class(TdxfEntity)
  private
    FData: PByte;
    FSize: Integer;
  protected
    destructor Destroy; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    function SetLayerByName(AReader: TdxfReader; const AName: string): Integer; override;
  end;

  TdxfMetaProxy = class(TdxfProxy)
  end;

  TdxfMesh = class(TdxfProxy)
  private
    FIndexVertex: Integer;
    FIndexFace: Integer;
    FVertexArray: TFPointList;
    FFaceArray: TsgIntegerList;
    FEdgeArray: TsgIntegerList;
    FMode: Integer;  // 1 - Face; 2 - Edge
  protected
    constructor Create; override;
    destructor Destroy; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfShape = class(TdxfEntity)
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfSortEntsTable = class(TdxfCustomDictionaryItem)
  private
    FStartTable: Boolean;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure ReadState(AReader: TdxfReader); override;
  end;

  TdxfTable = class(TdxfObject)
  private
    FSortEntList: TStrings;
    FConvSection: TConvSection;
  protected
    constructor Create; override;
    destructor Destroy; override;
    function AddEntity(const E: TdxfObject): Boolean; override;
    procedure Commit(const AReader: TdxfReader); override;
    function EOFEnt(AObj: TdxfObject): Boolean; override;
    property SortEntList: TStrings read FSortEntList write FSortEntList;
  end;

  TdxfText = class(TdxfEntity)
  private
    FMText: TdxfMText;
    FStyleLinkIndex: Integer;
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure SkipEmbeddedObject(const AReader: TdxfReader); override;
    procedure UpdateLinks(AReader: TdxfReader); override;
  public
    destructor Destroy; override;
  end;

  TdxfTolerance = class(TdxfEntity)
  private
    procedure ApplyACADEED(const AReader: TdxfReader);
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfVertex = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfViewport = class(TdxfEntity)
  private
    FCount_1010: Integer;
    FCount_1040: Integer;
    procedure Next3Reals(const AReader: TdxfReader; APoint: TFPoint);
    procedure NextReal(const AReader: TdxfReader; AValue: Double);
  protected
    constructor Create; override;
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfVport = class(TdxfEntity)
  private
    FOrigin: TFPoint;
    FUCSVP: Integer;
    FXDir: TFPoint;
    FYDir: TFPoint;
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    function SetLayerByName(AReader: TdxfReader; const AName: string): Integer; override;
    function SetLTypeByName(AReader: TdxfReader; const AName: string): Integer; override;
  end;

  TdxfWipeout = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfWipeoutVars = class(TdxfCustomDictionaryItem)
  protected
    procedure UpdateOwner(AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfCircle = class(TdxfBasePolyLine)
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfLine = class(TdxfPenLine)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfSolid = class(TdxfLine)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfSpline = class(TdxfBasePolyLine)
  private
    FNewCtl: Boolean;
    FNewFit: Boolean;
    FCodeValue: Integer;
    FFlagsExtented: Integer;
    FExtData: TsgCADExtendedData;
    procedure AddControl;
    procedure AddFit;
    procedure AddKnot(const AValue: Double);
    procedure AddWeight(const AValue: Double);
  protected
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure SetDegreeSplineCurve; virtual;
  public
    constructor Create; override;
  end;

  TdxfPolyLine = class(TdxfBasePolyLine)
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure SetElevation; virtual;
    procedure ReadEntities(AReader: TdxfReader); override;
  public
    constructor Create; override;
  end;

  TdxfAcadTable = class(TdxfInsert)
  private
    FBlockRecordHandle: UInt64;
    FColIndex: Integer;
    FEofCommon: Boolean;
    FIsTableSubClass: Boolean;
    FRowIndex: Integer;
    FTableStyleHandle: UInt64;
    function CurCell: TsgAcadTableCell;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfAcadTableStyle = class(TdxfCustomDictionaryItem)
  private
    FCellTypeIndex: TsgAcadTableCellType;
    FTemporaryDictionary: TsgDXFEntity;
    function GetCell: TsgAcadTableCellStyle;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    function ReplaceFromDatabase(AReader: TdxfReader; var Index: Integer;
      out ASoftOwner: TsgDXFEntity): Integer; override;
    property Cell: TsgAcadTableCellStyle read GetCell;
  public
    constructor Create; override;
  end;

  TdxfACIS = class(TdxfPenLine)
  protected
    FEncoded: AnsiString;
    procedure DoDecode(const AReader: TdxfReader);
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;
  
  TdxfArc = class(TdxfCircle)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfAttdef = class(TdxfText)
  private
    FACADAttrExtData: TsgCADExtendedData;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

{$IFDEF SG_BTI}
  TdxfAttrib = class(TdxfAttdef)
  private
    FExtData: TsgCADExtendedData;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
  end;
{$ENDIF}

  TdxfBlockRecord = class(TdxfObject)
  private
    FFlags: Integer;
    FLayout: UInt64;
{$IFDEF SG_BLOCK_PREVIEW}
    FData: Pointer;
    FPreviewSize: Integer;
{$ENDIF}
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfDimension = class(TdxfInsert)
  private
    FDimRot: Double;
    FIsArcDimension: Boolean;
    procedure SetDStyle(const AReader: TdxfReader);
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfDimStyle = class(TdxfPenTableItem)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfEllipse = class(TdxfArc)
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfFace = class(TdxfSolid)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfField = class(TdxfCustomDictionaryItem)
  private
    FBuffSize: Integer;
    FBytesRead: Integer;
    FData: PByte;
    FReadCache: Boolean;
    FReadDataSet: Boolean;
    FReadFieldObject: Boolean;
    FDictionary: UInt64;
  protected
    destructor Destroy; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfFieldList = class(TdxfCustomDictionaryItem)
  private
    FAcDbIdSet: Boolean;
    FStartSoftHandles: Integer;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfHelix = class(TdxfSpline)
  private
    FLock: Boolean;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfImageEnt = class(TdxfWipeout)
  private
    FImageDef: UInt64;
    FData: PByte;
    FSize: Integer;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    destructor Destroy; override;
  end;

  TdxfLayer = class(TdxfPenTableItem)
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfLeader = class(TdxfSpline)
  private
    FArrowScale: Double;
    FArrowSize: Double;
    FBlockRefHandle: UInt64;
    FTextAnnotationHeight: Double;
    FTextAnnotationWidth: Double;
    FHookline: Integer;
    FHooklineDirectional: Integer;
    FHooklineVector: TFPoint;
    FAnnotationType: Integer;
    FTAD: Integer;
    FGAP: Double;
    FHRefAsAnnotation: UInt64;
  protected
    constructor Create; override;
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure SetDegreeSplineCurve; override;
  end;

  TdxfLType = class(TdxfPenTableItem)
  private
    FIndex: Integer;
    FCount: Integer;
    FElements: array of TsgLTypeElement;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    constructor Create; override;
  end;

  TdxfLWPline = class(TdxfPolyLine)
  private
    FNewVertex: Boolean;
    procedure AddVertex;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure SetElevation; override;
  public
    constructor Create; override;
  end;

  TdxfStyle = class(TdxfPenTableItem)
  protected
    constructor Create; override;
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    function GetPrimaryFont: string;
  end;

  TdxfAppID = class(TdxfPenTableItem)
  end;

  TdxfTrace = class(TdxfSolid)
  end;

  TdxfRasterVariables = class(TdxfWipeoutVars)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfDataLink = class(TdxfCustomDictionaryItem)
  private
    FCustomData: Boolean;
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfXRecord = class(TdxfCustomDictionaryItem)
  private
    FStartData: Boolean;
    procedure AddExData(const AReader: TdxfReader; AData: TsgCADExtendedData);
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TcstXRecord = class(TdxfXRecord)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfAcdsRecord = class(TdxfXRecord)
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfRay = class(TdxfEntity)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfScale = class(TdxfCustomDictionaryItem)
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfSpatialFilter = class(TdxfCustomDictionaryItem)
  private
    FExtrision: TFPoint;
    FPoints: PFPointArray;
    FPointIndex: Integer;
    FMatIndex: Integer;
    FClipbdorg: TFPoint;
    FFrontClipNeeded: Boolean;
    FMatrices: array[0 .. 23] of Double;
  protected
    constructor Create; override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfObjectContextData = class(TdxfCustomDictionaryItem)
  private
    FScale: UInt64;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfMLeaderAnnoContext = class(TdxfObjectContextData)
  private
    FItem: TsgMLeaderContextDataAccess;
    FMatrix: TsgDoubleList;
    FMLeader: TsgMLeaderAccess;
    FLine: TsgMLeaderLineAccess;
    procedure ReadLineGroup(const AReader: TdxfReader);
    procedure ReadMLeaderNodeGroup(const AReader: TdxfReader);
    procedure ReadContext(const AReader: TdxfReader);
    procedure EndContext;
    procedure SkipCode(const AReader: TdxfReader);
  protected
    constructor Create; override;
    destructor Destroy; override;
    function BeginGroup(AClass: TsgCustomMLeaderItemClass;
      const AReadMethod: TdxfReadProperty; out obj): Integer;
    procedure LeaveGroup(const AReader: TdxfReader);
    procedure ReadProperty(const AReader: TdxfReader); override;
    procedure ReadProps(AReader: TdxfReader); override;
  end;

  TdxfMLeaderStyle = class(TdxfCustomDictionaryItem)
  private
    FObjVersion: Integer;
  protected
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfMLeader = class(TdxfEntity)
  private
    FLabelsCount: Integer;
    FLabels: TsgMLeaderLabels;
    FArrowsCount: Integer;
    FArrows: TsgDXFEntityArray;
    FVer: TsgCADExtendedData;
    procedure ReadContextData(const AReader: TdxfReader);
    procedure ReadMLeaderProperty(const AReader: TdxfReader);
  protected
    procedure ApplyEED(AReader: TdxfReader); override;
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    procedure InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader); override;
  end;

  TdxfBlkRefObjectContextData = class(TdxfObjectContextData)
  private
    FPoint: TFPoint;
    FScaleFactor: TFPoint;
    FAngle: Double;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  TdxfGroup = class(TdxfCustomDictionaryItem)
  private
    FItems: TsgInt64List;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TdxfCustomApplicationBlockReference = class(TdxfInsert)
  protected
    procedure Commit(const AReader: TdxfReader); override;
  end;

  TdxfAcIdBlockReference = class(TdxfCustomApplicationBlockReference)
  end;

  TdxfAcDbViewRepBlockReference = class(TdxfCustomApplicationBlockReference)
  end;

  TdxfCustomIrdRecord = class(TdxfCustomDictionaryItem)
  private
    FClassIndex: Integer;
    FIrdFlags: Integer;
    FDataSize: Integer;
    FData: Pointer;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TdxfIrdDscRecord = class(TdxfCustomIrdRecord)
  end;

  TdxfIrdObjRecord = class(TdxfCustomIrdRecord)
  private
    FIrdDscRecordHandle: UInt64;
  protected
    procedure Commit(const AReader: TdxfReader); override;
    procedure ReadProperty(const AReader: TdxfReader); override;
  end;

  PsgNameToClasses = ^TsgNameToClasses;
  TsgNameToClasses = record
    Name: string;
    Entity: TsgDXFEntityClass;
    EntityClass: TdxfObjectClass;
  end;

const
  cnstNamesToEntsAndClasses: array [0 .. 107] of
    TsgNameToClasses =
 ((Name: cnstSeqEnd; Entity: nil; EntityClass: TdxfSeqend),
  (Name: 'VERTEX'; Entity: TsgDXFVertex; EntityClass: TdxfVertex),
  (Name: 'LINE'; Entity: TsgDXFLine; EntityClass: TdxfLine),
  (Name: 'SOLID'; Entity: TsgDXFSolid; EntityClass: TdxfSolid),
  (Name: 'CIRCLE'; Entity: TsgDXFCircle; EntityClass: TdxfCircle),
  (Name: 'ARC'; Entity: TsgDXFArc; EntityClass: TdxfArc),
  (Name: 'ELLIPSE'; Entity: TsgDXFEllipse; EntityClass: TdxfEllipse),
  (Name: 'DIMENSION'; Entity: TsgDXFDimension; EntityClass: TdxfDimension),
  (Name: 'LEADER'; Entity: TsgDXFLeader; EntityClass: TdxfLeader),
  (Name: 'LTYPE'; Entity: TsgDXFLineType; EntityClass: TdxfLType),
  (Name: 'LAYER'; Entity: TsgDXFLayer; EntityClass: TdxfLayer),
  (Name: 'BLOCK_RECORD'; Entity: TsgDXFBlockRecord; EntityClass: TdxfBlockRecord),
  (Name: 'POLYLINE'; Entity: TsgDXFPolyline; EntityClass: TdxfPolyline),
  (Name: 'LWPOLYLINE'; Entity: TsgDXFLWPolyline; EntityClass: TdxfLWPline),
  (Name: 'SPLINE'; Entity: TsgDXFSpline; EntityClass: TdxfSpline),
  (Name: 'HATCH'; Entity: TsgCADHatch; EntityClass: TDXFHatch),
  (Name: 'INSERT'; Entity: TsgDXFInsert; EntityClass: TdxfInsert),
  (Name: 'TEXT'; Entity: TsgDXFText; EntityClass: TdxfText),
  (Name: 'MTEXT'; Entity: TsgDXFMText; EntityClass: TdxfMText),
  (Name: 'ATTDEF'; Entity: TsgDXFAttdef; EntityClass: TdxfAttdef),
  (Name: 'VIEWPORT'; Entity: TsgDXFViewport; EntityClass: TdxfViewport),
  (Name: 'TABLE'; Entity: TsgDXFTable; EntityClass: TdxfTable),
  (Name: 'BLOCK'; Entity: TsgDXFBlock; EntityClass: TdxfBlockR9),
  (Name: 'VPORT'; Entity: TsgDXFVport; EntityClass: TdxfVport),
  (Name: '3DFACE'; Entity: TsgDXF3dFace; EntityClass: TdxfFace),
  (Name: 'POINT'; Entity: TsgDXFPoint; EntityClass: TdxfPoint),
  (Name: 'ATTRIB'; Entity: TsgDXFAttrib; EntityClass: {$IFDEF SG_BTI}
    TdxfAttrib {$ELSE} TdxfAttdef {$ENDIF}),
  (Name: 'SECTION'; Entity: TsgDXFSection; EntityClass: TdxfTable),
  (Name: 'LAYOUT'; Entity: TsgDXFLayout; EntityClass: TdxfLayout),
  (Name: cnstEndSec; Entity: nil; EntityClass: TdxfEndSeq),
  (Name: 'ENDTAB'; Entity: nil; EntityClass: TdxfEndTab),
  (Name: 'ENDBLK'; Entity: nil; EntityClass: TdxfEndBlock),
  (Name: 'EOF'; Entity: nil; EntityClass: TdxfSeqend),
  (Name: 'IMAGE'; Entity: TsgDXFImageEnt; EntityClass: TdxfImageEnt),
  (Name: 'IMAGEDEF'; Entity: TsgDXFImageDef; EntityClass: TdxfImageDef),
  (Name: 'STYLE'; Entity: TsgDXFStyle; EntityClass: TdxfStyle),
  (Name: 'DIMSTYLE'; Entity: TsgDXFDimensionStyle; EntityClass: TdxfDimStyle),
  (Name: 'REGION'; Entity: TsgDXFRegion; EntityClass: TdxfACIS),
  (Name: '3DSOLID'; Entity: TsgDXF3dSolid; EntityClass: TdxfACIS),
  (Name: 'BODY'; Entity: TsgDXFBody; EntityClass: TdxfACIS),
  (Name: 'SORTENTSTABLE'; Entity: TsgDXFSortEntsTable; EntityClass: TdxfSortEntsTable),
  (Name: 'ACAD_TABLE'; Entity: TsgDXFAcadTable; EntityClass: TdxfAcadTable),
  (Name: 'OLEFRAME'; Entity: TsgDXFOle2Frame; EntityClass: TdxfOLE),
  (Name: 'OLE2FRAME'; Entity: TsgDXFOle2Frame; EntityClass: TdxfOLE),
  (Name: 'DIMASSOC'; Entity: nil; EntityClass: TdxfDimAssoc),
  (Name: '3DLINE'; Entity: TsgDXFLine; EntityClass: TdxfLine),
  (Name: 'ACAD_PROXY_ENTITY'; Entity: TsgDXFProxy; EntityClass: TdxfProxy),
  (Name: 'TOLERANCE'; Entity: TsgDXFTolerance; EntityClass: TdxfTolerance),
  (Name: 'TRACE'; Entity: TsgDXFTrace; EntityClass: TdxfTrace),
  (Name: 'SHAPE'; Entity: TsgDXFShape; EntityClass: TdxfShape),
  (Name: 'WIPEOUT'; Entity: TsgCADWipeout; EntityClass: TdxfWipeout),
  (Name: 'WIPEOUTVARIABLES'; Entity: TsgCADWipeoutVariables; EntityClass: TdxfWipeoutVars),
  (Name: 'HELIX'; Entity: TsgCADHelix; EntityClass: TdxfHelix),
  (Name: 'MLINE'; Entity: TsgCADMLine; EntityClass: TdxfMLine),
  (Name: 'MLINESTYLE'; Entity: TsgMLineStyle; EntityClass: TdxfMLStyle),
  (Name: 'MPOLYGON'; Entity: TsgCADMPolygon; EntityClass: TdxfHatch),
  (Name: cntClassDXFTABLESTYLE; Entity: TsgDXFAcadTableStyle; EntityClass: TdxfAcadTableStyle),
  (Name: cnstDictionary; Entity: TsgDXFDictionary; EntityClass: TdxfDictionary),
  (Name: ''; Entity: nil; EntityClass: TdxfEntity),
  (Name: 'MESH'; Entity: TsgDXFMesh; EntityClass: TdxfMesh),
  (Name: 'PLANESURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'FIELD'; Entity: TsgDXFField; EntityClass: TdxfField),
  (Name: cnstObjectsFIELDLIST; Entity: TsgDXFFieldList; EntityClass: TdxfFieldList),
  (Name: sRasterVariables; Entity: TsgCADRasterVariables; EntityClass: TdxfRasterVariables),
  (Name: cnstObjectsPLOTSETTINGS; Entity: TsgDXFPlotSettings; EntityClass: TdxfPlotSettings),
  (Name: cnstDictionaryVar; Entity: TsgDXFDictionaryVar; EntityClass: TdxfDictionaryVar),
  (Name: 'SURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'SWEPTSURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'REVOLVEDSURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'EXTRUDEDSURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'LOFTEDSURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: 'NURBSURFACE'; Entity: TsgDXFSurface; EntityClass: TdxfACIS),
  (Name: cnstSectionTABLES; Entity: TsgDXFSectionTables; EntityClass: TdxfTables),
  (Name: cnstSectionBLOCKS; Entity: TsgDXFSectionBlocks; EntityClass: TdxfBlocks),
  (Name: cnstSectionENTITIES; Entity: TsgDXFSection; EntityClass: TdxfEntities),
  (Name: cnstSectionOBJECTS; Entity: TsgDXFSectionObjects; EntityClass: TdxfObjects),
  (Name: cnstSectionCLASSES; Entity: TsgDXFSection; EntityClass: TdxfClasses),
  (Name: cnstSectionHEADER; Entity: TsgDXFSection; EntityClass: TdxfHeader),
  (Name: 'THUMBNAILIMAGE'; Entity: TsgDXFSection; EntityClass: TdxfCustomSection),
  (Name: 'XRECORD'; Entity: TsgDXFXRecord; EntityClass: TdxfXRecord),
  (Name: 'RAY'; Entity: TsgDXFRay; EntityClass: TdxfRay),
  (Name: 'XLINE'; Entity: TsgDXFXline; EntityClass: TdxfRay),
  (Name: cnstSPATIAL_FILTER; Entity: TsgCADSpatialFilter; EntityClass: TdxfSpatialFilter),
  (Name: 'APPID'; Entity: TsgDXFAppID; EntityClass: TdxfAppID),
  (Name: 'SCALE'; Entity: TsgDXFScale; EntityClass: TdxfScale),
  (Name: 'CLASS'; Entity: TsgDXFClass; EntityClass: TdxfClass),
  (Name: cnstSectionACDSDATA; Entity: TsgDXFSection; EntityClass: TdxfAcdsData),
  (Name: cnstACDSRECORD; Entity: TsgDXFXRecord; EntityClass: TdxfAcdsRecord),
  (Name: sAcIdBlockReference; Entity: TsgCADAcIdBlockReference; EntityClass: TdxfCustomApplicationBlockReference),
  (Name: 'ACDB_MTEXTATTRIBUTEOBJECTCONTEXTDATA_CLASS'; Entity: TsgDXFObjectContextData; EntityClass: TdxfObjectContextData),
  (Name: 'ACDB_TEXTOBJECTCONTEXTDATA_CLASS'; Entity: TsgDXFObjectContextData; EntityClass: TdxfObjectContextData),
  (Name: 'ACDB_LEADEROBJECTCONTEXTDATA_CLASS'; Entity: TsgDXFObjectContextData; EntityClass: TdxfObjectContextData),
  (Name: 'ACDB_MTEXTOBJECTCONTEXTDATA_CLASS'; Entity: TsgDXFObjectContextData; EntityClass: TdxfObjectContextData),
  (Name: 'ACDB_ALDIMOBJECTCONTEXTDATA_CLASS'; Entity: TsgDXFObjectContextData; EntityClass: TdxfObjectContextData),
  (Name: 'ACDB_BLKREFOBJECTCONTEXTDATA_CLASS'; Entity: TsgCADBlkRefObjectContextData; EntityClass: TdxfBlkRefObjectContextData),
  (Name: 'ACDB_MLEADEROBJECTCONTEXTDATA_CLASS'; Entity: TsgCADMLeaderAnnotContext; EntityClass: TdxfMLeaderAnnoContext),
  (Name: 'MLEADERSTYLE'; Entity: TsgCADMLeaderStyle; EntityClass: TdxfMLeaderStyle),
  (Name: 'MULTILEADER'; Entity: TsgCADMultiLeader; EntityClass: TdxfMLeader),
  (Name: 'GROUP'; Entity: TsgCADGroup; EntityClass: TdxfGroup),
  (Name: 'ACDBDICTIONARYWDFLT'; Entity: TsgDXFDictionary; EntityClass: TdxfDictionary),
  (Name: 'DBCOLOR'; Entity: TsgDXFCustomColor; EntityClass: TdxfCustomDictionaryItem),
  (Name: 'ACAD_PROXY_OBJECT'; Entity: TsgCADProxyObject; EntityClass: TdxfProxyObject),
  (Name: 'VIEW'; Entity: TsgDXFPenTableItem; EntityClass: TdxfView), // TODO: read TsgDXFView
  (Name: 'ACDBVIEWREPBLOCKREFERENCE'; Entity: TsgCADAcDbViewRepBlockReference; EntityClass: TdxfAcDbViewRepBlockReference),
  (Name: cntClassDXFARC_DIMENSION; Entity: TsgDXFDimension; EntityClass: TdxfDimension),
  (Name: cnstIrdDscDict; Entity: TsgDXFDictionary; EntityClass: TdxfDictionary),
  (Name: cnstIrdDscRecord; Entity: TsgCADIrdDscRecord; EntityClass: TdxfIrdDscRecord),
  (Name: cnstIrdObjRecord; Entity: TsgCADIrdObjRecord; EntityClass: TdxfIrdObjRecord)
  );

  sHexBin: array[Byte] of Byte = ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, 10, 11, 12, 13, 14, 15, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, 10, 11, 12, 13, 14, 15, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF);

var
  CF_DXF: Word;
  HeadVars: TStringList = nil;
  DataBaseSections: TsgStringList = nil;
  ObjectItemClasses: TsgStringList = nil;
  LayerProp: PPropInfo;
  LTypeProp: PPropInfo;
  BlockRecordProp: PPropInfo;
  IrdDscRecordProp: PPropInfo;
  DictionaryName: string = cnstDictionary;

{Fuctions}

function BytesOf(p: Pointer; c: Integer): TBytes;
begin
  SetLength(Result, c);
  if c > 0 then
    System.Move(p^, Result[0], c);
end;

function DXFStringToString(const ADXFString: string): string;
begin
  Result := ADXFString;
  StringReplace(Result, '^M', #13);
  StringReplace(Result, '^J', #10);
  StringReplace(Result, '^I', #9);
end;

procedure DoAddBinData(AHexBinData: Pointer; ACount: Integer; var AData: Pointer; var ASize: Integer); overload;
var
  C: Integer;
  vData: PByte;
  vSrc: TsgNativeUInt;
begin
  C := ACount div 2;
  ReallocMem(AData, ASize + C);
  vData := AData;
  Inc(vData, ASize);
  Inc(ASize, C);
  vSrc := TsgNativeUInt(AHexBinData);
  while C > 0 do
  begin
    vData^ := sHexBin[PByte(vSrc)^] shl 4 or sHexBin[PByte(vSrc + 1)^];
    Inc(vData);
    Inc(vSrc, 2);
    Dec(C);
  end;
end;

procedure DoAddBinData(const S: AnsiString; var AData: Pointer; var ASize: Integer); overload;
begin
  DoAddBinData(Pointer(S), Length(S), AData, ASize);
end;

procedure BinStr(var AString: AnsiString; var APPosition: PAnsiChar;
  APLimitParse: PAnsiChar);
var
  vPStartPosition: PAnsiChar;
  vLength: Integer;
begin
  vPStartPosition := APPosition;
  while (APPosition < APLimitParse)  and (APPosition^ <> #0) do
    Inc(APPosition);
  vLength := APPosition - vPStartPosition;
  if APPosition <> APLimitParse  then
    Inc(APPosition);
  SetLength(AString, vLength);
  if vLength > 0 then
    Move(vPStartPosition^, AString[1], vLength);
end;

procedure ClearStringsObjects(const AList: TStrings);
var
  I: Integer;
  vObj: TObject;
begin
  if Assigned(AList) then
    while AList.Count > 0 do
    begin
      I := AList.Count - 1;
      vObj := AList.Objects[I];
      AList.Delete(I);
      vObj.Free;
    end;
end;

function ExpandLWeight(const ALWeight: Double): Double;
begin
  if ALWeight <= 0 then
    Result := ALWeight
  else
    Result := 0.01 * ALWeight;
end;

function GetArrowTypeByHandle(const ABlockRefHandle: UInt64;
  const AConverter: TsgDXFConverter): Integer;
var
  vName: string;
begin
  vName := Trim(TsgDXFConverterAccess(AConverter).BlockRecordNameByHandle(
    ABlockRefHandle));
  Result := StrIndex(vName, sgDimensionArrowTypeNames);
end;

function GetOnefromZero(const AValue: Double): Double;
begin
  if AValue <> 0 then
    Result := AValue
  else
    Result := 1;
end;

procedure InitDataBaseSections;
begin
  DataBaseSections := TsgStringList.Create;
  DataBaseSections.CaseSensitive := False;

  DataBaseSections.AddObject(cnstTableBLOCK_RECORD, TsgObjectWithField.CreateInt(Integer(csBlockRecords)));
  DataBaseSections.AddObject(cnstTableDIMSTYLE, TsgObjectWithField.CreateInt(Integer(csDimStyles)));
  DataBaseSections.AddObject(cnstTableLTYPE, TsgObjectWithField.CreateInt(Integer(csLTypes)));
  DataBaseSections.AddObject(cnstTableLAYER, TsgObjectWithField.CreateInt(Integer(csLayers)));
  DataBaseSections.AddObject(cnstTableSTYLE, TsgObjectWithField.CreateInt(Integer(csStyles)));
  DataBaseSections.AddObject(cnstTableVPORT, TsgObjectWithField.CreateInt(Integer(csVPorts)));
  DataBaseSections.AddObject(cnstTableAPPID, TsgObjectWithField.CreateInt(Integer(csAppID)));

  DataBaseSections.Sorted := True;
end;

procedure InitObjectItemClasses;
begin
  ObjectItemClasses := TsgStringList.Create;
  ObjectItemClasses.CaseSensitive := False;

  ObjectItemClasses.AddObject(sAcadGroupDictionary, TsgObjectWithField.CreateClass(TsgCADGroup));
  ObjectItemClasses.AddObject(sAcadMaterialDictionary, TsgObjectWithField.CreateClass(TsgDXFMaterial));
  ObjectItemClasses.AddObject(sAcadLayoutDictionary, TsgObjectWithField.CreateClass(TsgDXFLayout));
  ObjectItemClasses.AddObject(sAcadImageDict, TsgObjectWithField.CreateClass(TsgDXFImageDef));
  ObjectItemClasses.AddObject(sAcadMLineStyleDictionary, TsgObjectWithField.CreateClass(TsgMLineStyle));
  ObjectItemClasses.AddObject(sAcadScaleListDictionary, TsgObjectWithField.CreateClass(TsgDXFScale));
  ObjectItemClasses.AddObject(sAcadMLeaderStyleDictionary, TsgObjectWithField.CreateClass(TsgCADMLeaderStyle));
  ObjectItemClasses.AddObject(sAcadPlotSettingsDictionary, TsgObjectWithField.CreateClass(TsgDXFPlotSettings));
  ObjectItemClasses.AddObject(sAcadFieldListDictionary, TsgObjectWithField.CreateClass(TsgDXFField));
  ObjectItemClasses.AddObject(sAcadTableStyleDictionary, TsgObjectWithField.CreateClass(TsgDXFAcadTableStyle));

  ObjectItemClasses.Sorted := True;
end;

function ObjectItemClassByName(const AObjectsName: string): TsgDXFEntityClass;
var
  I: Integer;
begin
  Result := nil;
  I := ObjectItemClasses.IndexOf(AObjectsName);
  if I >= 0 then
    Result := TsgDXFEntityClass(TsgObjectTClass(ObjectItemClasses.Objects[I]).FieldClass);
end;

procedure QHashName(var APos: PAnsiChar; var ANameHash: Cardinal);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  ANameHash := 0;
  while not (APos^ in [#0, #10, #13]) do
  begin
    ANameHash := ANameHash xor (Ord(APos^) or $20); // lowercase
    ANameHash := (ANameHash shl 5) or (ANameHash shr 27);
    Inc(APos);
  end;
end;

function DoReadHash(const AName: string): Cardinal;{$IFDEF SG_INLINE} inline;{$ENDIF}
var
  P: PAnsiChar;
begin
  P := PAnsiChar(AnsiString(AName));
  QHashName(P, Result);
end;

//procedure InitListOfEntName;
//begin
//  _hshObjectsPLOTSETTINGS := DoReadHash(cnstObjectsPLOTSETTINGS);
//  _hshSectionTABLES := DoReadHash(cnstSectionTABLES);
//  _hshTable := DoReadHash(cnstTable);
//  _hshAcadXDictionary := DoReadHash(sAcadXDictionary);
//  _hshAcadReactors := DoReadHash(sAcadReactors);
//  _hshBlkRefs := DoReadHash(sBlkRefs);
//  _hshEOF := DoReadHash(cnstEOF);
//  _hshEndSec := DoReadHash(cnstEndSec);
//  _hshSeqEnd := DoReadHash(cnstSeqEnd);
//  _hshSectionBLOCKS := DoReadHash(cnstSectionBLOCKS);
//  _hshVertex := DoReadHash(cnstVertex);
//end;

function IndexHeadVar(const AName: string): Integer;
const
  cnstHVCount = 79;
  cnstHVars: array[0 .. cnstHVCount - 1] of string = (
    '$HANDSEED', '$DIMALT', '$DIMALTF',
    '$DIMAPOST', '$DIMCLRD', '$DIMCLRE', '$DIMCLRT', '$DIMEXE', '$DIMEXO',
    '$DIMGAP', '$DIMLFAC', '$DIMPOST', '$MEASUREMENT', '$UCSORG', '$UCSXDIR',
    '$UCSYDIR', '$DWGCODEPAGE', '$PDMODE', '$TILEMODE', '$LTSCALE', '$DIMASZ',
    '$DIMSCALE', '$DIMSTYLE', '$PDSIZE', '$ACADVER', '$CECOLOR', '$CLAYER',
    '$CELTYPE', '$CELTSCALE', '$CELWEIGHT', '$DIMDEC', '$DIMTAD', '$DIMTIH',
    '$DIMTIX', '$DIMTOH', '$DIMBLK', '$DIMBLK1', '$DIMBLK2', '$DIMLDRBLK',
    '$DIMTXT', '$TEXTSIZE', '$FILLETRAD', '$INSUNITS', '$DIMTXSTY',
    '$TEXTSTYLE', '$EXTMIN', '$EXTMAX', '$INSBASE', cnstAttMode,
    '$DIMSE1', '$DIMSE2', '$DIMSD1', '$DIMSD2', '$DIMLWD', '$DIMLWE', '$DIMSAH',
    '$XCLIPFRAME',
    '$TDCREATE',//57
    '$TDINDWG', //58
    '$TDUCREATE',//59
    '$TDUPDATE',//60
    '$TDUSRTIMER',//61
    '$TDUUPDATE',//62
    '$DIMTP', '$DIMTM', '$DIMLUNIT', '$DIMDSEP',//66
    '$DIMASSOC', //67
    '$FILLMODE', //68
    '$USERR4', //69
    '$USERR5', //70
    '$DIMFRAC', //71
    '$LWDISPLAY',//72
    '$LIMMIN',//73
    '$LIMMAX',//74
    '$PEXTMIN',//75
    '$PEXTMAX',//76
    '$PLIMMIN',//77
    '$PLIMMAX'//78
    );
var
  I: Integer;
begin
  Result := -1;
  if HeadVars = nil then
  begin
    HeadVars := TsgStringList.Create;
    TsgStringList(HeadVars).CaseSensitive := True;
    HeadVars.Duplicates := dupIgnore;
    for I := Low(cnstHVars) to High(cnstHVars) do
       HeadVars.AddObject(cnstHVars[I], TsgObjectWithField.CreateInt(I));
    HeadVars.Sorted := True;
  end;
  I := HeadVars.IndexOf(AnsiUpperCase(AName));
  if I > -1 then
    Result := TsgObjectInt64(HeadVars.Objects[I]).FieldInt;
end;

function IsBinaryDXF(AStream: TStream; var AGroupCodeSize: Integer): Boolean;
const
  cnstBin: array[0..21] of AnsiChar = 'AutoCAD Binary DXF'#13#10#26#0;
var
  vSize: Integer;
  Buf: array[0..21] of AnsiChar;
begin
  vSize := AStream.Read(Buf,22);
  Result := (vSize=22) and CompareMem(@cnstBin,@Buf,22);
  if Result then
  begin
    // Binary DXF-format version detection
    AGroupCodeSize := 0;
    vSize := AStream.Read(AGroupCodeSize, 2);
    if AGroupCodeSize <> 0 then
      AGroupCodeSize := 1
    else
      AGroupCodeSize := 2;
  end;
  AStream.Position := AStream.Position - vSize;
end;

procedure SeekEoln(var APos: PAnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  while not (APos^ in [#10, #13]) do
    Inc(APos);
end;

procedure SeekEoStr(var APos: PAnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  while APos^ <> #0 do
    Inc(APos);
end;

procedure SkeepBlanks(var APos: PAnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  while (APos^ = ' ') or (APos^ = #9) do
    Inc(APos);
end;

procedure SkeepLimits(var APos: PAnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  //if APos < ALimit then // not need because additional eoln (see procedure TsgDXFConverter.SetSourceStream)
  begin
    if APos^ = #13 then
      Inc(APos);
    if APos^ = #10 then
      Inc(APos);
  end;
end;

function IsNeg(var APos: PAnsiChar): Boolean;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := APos^ = '-';
  if Result then
    Inc(APos)
  else
    if APos^ = '+' then
      Inc(APos);
end;

function DoIntValue(var APos: PAnsiChar;
  var AValue: Integer): Boolean;{$IFDEF SG_INLINE} inline;{$ENDIF}
var
  D: Integer;
begin
  D := Ord(APos^) - Ord('0');
  while (D >= 0) and (D <= 9) do
  begin
    Inc(AValue, AValue);
    AValue := AValue * 5 + D;//lea eax,[eax+eax*4]!
    Inc(APos);
    D := Ord(APos^) - Ord('0');
  end;
  Result := APos^ in [#10, #13];
end;

function DoInt64Value(var APos: PAnsiChar;
  var AValue: UInt64): Boolean;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  while APos^ in ['0' .. '9'] do
  begin
    AValue := AValue * 10 + Ord(APos^) - Ord('0');
    Inc(APos);
  end;
  Result := APos^ in [#10, #13];
end;

procedure DoDoubleValue(var APos: PAnsiChar;
  var AValue: Double);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  while APos^ in ['0' .. '9'] do
  begin
    AValue := AValue * 10 + Ord(APos^) - Ord('0');
    Inc(APos);
  end;
end;

function DoHandleValue(var APos: PAnsiChar;
  var AValue: UInt64): Boolean;{$IFDEF SG_INLINE} inline;{$ENDIF}
var
  vHexBin: Byte;
begin
  AValue := 0;
  repeat
    vHexBin := sHexBin[Ord(APos^)];
    Result := vHexBin <> $FF;
    if Result then
    begin
      AValue := AValue shl 4 or vHexBin;
      Inc(APos);
    end;
  until not Result;
  Result := APos^ in [#10, #13];
end;

{ TsgNameLink }

destructor TsgNameLink.Destroy;
begin
  PString(@FValue)^ := '';
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
  Result := TsgDXFEntity(Collection).FindChildByHandle(AsHandle);
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
  if TsgQCADItemsList(Collection).FindItem(AsHandle, I) then
    Result := TObject(TsgQCADItemsList(Collection)[I])
  else
    Result := nil;
end;

{ TsgObjectLink }

procedure TsgObjectLink.DoLink(Instance: TObject; P: PPropInfo);
begin
  if Instance <> nil then
    SetObjectProp(Instance, P, PropItem);
end;

{ TsgLTElementStyleLink }

constructor TsgLTElementStyleLink.Create(const AHandle: UInt64;
  AIndex: Integer);
begin
  FValue := AHandle;
  FIndex := AIndex;
end;

procedure TsgLTElementStyleLink.DoLink(Instance: TObject; P: PPropInfo);
begin
  TsgLines(Instance).Styles[FIndex] := PropItem;
end;

{ TsgCADdxfImage }

{public}

constructor TsgCADdxfImage.Create;
begin
  inherited Create;
  CBFormat := CF_DXF;
end;

function TsgCADdxfImage.CreateConverter: TsgDXFConverter;
begin
  Result := TsgDXFConverter.CreateEx(GetConverterParams);
end;

function TsgCADdxfImage.IsAutocadLType: Boolean;
begin
  Result := True;
end;

{ TODO: TsgCADdxfImage.LoadFromClipboardFormat for SG_NON_WIN_PLATFORM }
procedure TsgCADdxfImage.LoadFromClipboardFormat(AFormat: Word;
  AClipboardObject: THandle; APalette: HPALETTE);
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF SG_WIN_FPC}
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    ClipboardGetData(ctClipboard, CF_TEXT, Stream);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ELSE}
var
  vClipboardStream: TClipboardStream;
begin
  if AFormat = CF_DXF then
    AClipboardObject := GetClipboardData(CF_TEXT);
  if (AFormat <> CBFormat) or (AClipboardObject = 0)
  then
    raise EInvalidGraphic.Create('Invalid DXF data');
  vClipboardStream := TClipboardStream.Create(AClipboardObject);
  try
    LoadFromStream(vClipboardStream)
  finally
    vClipboardStream.Free
  end;
end;
{$ENDIF}
{$ELSE}
begin
end;
{$ENDIF}

procedure TsgCADdxfImage.LoadFromStream(AStream: TStream);
var
  vReader: TdxfReader;
  vGroupCodeSize: Integer;
{$IFDEF SG_DECRYPT_FILES}
  vStm, vOrigStream: TStream;
  vPos, vSrcPos: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF};
  vIsDecoded: Boolean;
{$ENDIF}
begin
  inherited LoadFromStream(AStream);
{$IFDEF SG_DECRYPT_FILES}
  vOrigStream := nil;
  vPos := 0;
  vSrcPos := sgStmCrpt.DecryptStm(AStream, vStm);
  vIsDecoded := Assigned(vStm);
  try
    if vIsDecoded then
    begin
      vPos := vStm.Position;
      vOrigStream := AStream;
      AStream := vStm;
    end;
{$ENDIF}
    if IsBinaryDXF(AStream, vGroupCodeSize) then
      vReader := TdxfBinReader.Create(Converter, vGroupCodeSize)
    else
      vReader := TdxfReader.Create(Converter);
    vReader.FUpdateEvent := DoUpdateEvent;
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := Self;
    {$ENDIF}
    TsgDXFConverterAccess(Converter).CreateXDataProcs;
    try
      FMsg := sLoadFile;     // for OnProgress
      Converter.SetSourceStream(AStream);
      Converter.ClearDrawingProp;
  //    FConverter.Load;
      vReader.Read;
  {$IFNDEF SG_USEDXFSRC}
      Converter.Source := nil;//evg
  {$ENDIF}
      SetDefaultViewPort(Converter);
      GetExtents;
      if vReader.IsInternalCADImage then
        CheckActiveVPort;
    finally
      TsgDXFConverterAccess(Converter).DestroyXDataProcs;
      TsgDXFConverterAccess(Converter).ClearEED(GetAppID);
      FileInfo := vReader.FileInfo;
      {$IFNDEF SG_OPENING_IN_THEADS}
      Loading := nil;
      {$ENDIF}
      vReader.Free;
    end;
{$IFDEF SG_DECRYPT_FILES}
  finally
    if vIsDecoded then
    begin
      vOrigStream.Position := vSrcPos + (vStm.Position - vPos);
      vStm.Free;
    end;
  end;
{$ENDIF}
end;

procedure TsgCADdxfImage.SaveToClipboardFormat(var AFormat: Word;
  var AClipboardObject: THandle; var APalette: HPALETTE);
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vClipboardStream: TClipboardStream;
begin
  AFormat := 0; AClipboardObject := 0; APalette := 0;
  if Converter.Source = nil then
    Exit;
  AFormat := CBFormat;
  vClipboardStream := TClipboardStream.Create(0);
  try
    SaveToStream(vClipboardStream);
    AClipboardObject := vClipboardStream.Handle;
  finally
    vClipboardStream.Free;
  end;
  if AFormat = CF_DXF then
  begin
    AFormat := CF_TEXT;
    SetClipboardData(CF_DXF, GlobalAlloc(GMEM_MOVEABLE, 4));
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

{ TdxfReader }

{private}

function TdxfReader.DoAddLink(obj: TObject; AProp: Pointer;
  ALink: TsgObjectLink): Integer;
begin
  Result := FLinksCount;
  if FLinksCount = Length(FLinks) then
    SetLength(FLinks, ListGrow(Length(FLinks)));
  Inc(FLinksCount);
  FLinks[Result].obj := obj;
  FLinks[Result].Prop := AProp;
  FLinks[Result].Link := ALink;
end;

procedure TdxfReader.DoCreate(AEntity: TsgDXFEntity);
begin
  FConverter.DoCreate(AEntity);
end;

procedure TdxfReader.DoEvent(Event: TNotifyEvent);
begin
  if Assigned(Event) then
    Event(FConverter);
end;

procedure TdxfReader.DoSetLoading;
begin
  Conv.SetLoading(True, FPos, FUpdateEvent);
  DoEvent(Conv.OnRead);
end;

procedure TdxfReader.DoSetNames;
var
  I: Integer;
  vEnt: TsgDXFEntity;
begin
  for I := Low(FNames) to FNamesCount - 1 do
    if FNames[I].Name <> '' then
    begin
      vEnt := ObjByHandle(FNames[I].ID, nil);
      if Assigned(vEnt) then
        vEnt.Name := FNames[I].Name;
    end;
end;

procedure TdxfReader.EntityDestroy(Sender: TObject);
var
  I: Integer;
begin
  if DataBase.CustomFindItem(TsgDXFEntity(Sender), I) then
    DataBase.Delete(I);
end;

function TdxfReader.EOF: Boolean;
begin
  Result := (FNameHash = hshEOF) or (FPos >= FLimit);
end;

function TdxfReader.GetConv: TsgDXFConverterAccess;
begin
  Result := TsgDXFConverterAccess(FConverter);
end;

function TdxfReader.GetFileInfo: string;
begin
  if FFileInfo.Count > 0 then
    Result := FFileInfo[FFileInfo.Count - 1]
  else
    Result := '';
end;

function TdxfReader.GetGlobalNames(const Name: string): string;
begin
  Result := FGlobalNames[FGlobalNames.Add(Name)];
end;

function TdxfReader.GetItemByName(Section: TConvSection;
  const Name: string): TsgDXFEntity;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(FSections[Section]) then
  begin
    I := FSections[Section].IndexOf(Name);
    if I >= 0 then
      Result := TsgDXFEntity(FSections[Section].Objects[I]);
  end
  else
    if Assigned(Conv.Sections[Section]) then
      Result := Conv.Sections[Section].FindEntByName(Name);
end;

function TdxfReader.GetProxyReaderImpl: TsgProxyReaderImpl;
begin
  if not Assigned(FProxyReaderImpl) then
    FProxyReaderImpl := TsgProxyReaderImpl.Create(Conv,
      TDXFProxyReader.Create(nil, TsgDWGVersion(Version)));
  Result := FProxyReaderImpl;
end;

function TdxfReader.GetAppIDEnt(const AName: string): TsgDXFEntity;

  procedure DoForceAppID(var AAppID: TsgDXFEntity);
  begin
    if AAppID = nil then
    begin
      AAppID := TsgDXFAppID.Create;
      FAppIDs.AddEntity(AAppID);
      AAppID.Name := AName;
    end;
    if AAppID.Handle = cnstBadHandle then
      Conv.SetHandle(AAppID);
  end;

begin
  if FAppIDs = nil then
    FAppIDs := Conv.NewNamedTable(cnstTableAPPID, TsgDXFTable);
  Result := FAppIDs.FindEntByName(AName);
  DoForceAppID(Result);
end;

function TdxfReader.GetStrValue: string;
begin
  Result := FEnc.GetString(BytesOf(FValueStartPos, FValueSize));
end;

function TdxfReader.GetUValue: string;
begin
{$IFDEF SGDEL_2009}
  Result := ParseUnicode(StrValue, [puU, puM]);
{$ELSE}
  Result := StrValue;
{$ENDIF}
end;

function TdxfReader.GetVersion: Byte;
begin
  Result := FConverter.HeadVarStruct.Version;
end;

function TdxfReader.GetXDataProcs: TsgXDataProcs;
begin
  Result := TsgDXFConverterAccess(FConverter).XDataProcs;
end;

procedure TdxfReader.ReadDouble(var AValue: Double);
var
  vNeg, vNegExp: Boolean;
  vDigits, vExp: Integer;
  vTmp: Pointer;
  vResult: Boolean;
begin
  AValue := 0;
  vNeg := IsNeg(FPos);
  DoDoubleValue(FPos, AValue);
  vDigits := 0;
  if (FPos^ = '.') or (FPos^ = ',') then
  begin
    repeat
      Inc(FPos);
    until FPos^ in [#10, #13, '0' .. '9', 'E', 'e'];
    vTmp := FPos;
    DoDoubleValue(FPos, AValue);
    vDigits := vTmp - FPos;
  end;
  vExp := 0;
  if Ord(FPos^) or $20 = Ord('e') then
  begin
    Inc(FPos);
    vNegExp := IsNeg(FPos);
    vResult := DoIntValue(FPos, vExp);
    if vNegExp then
      vExp := -vExp;
  end
  else
    vResult := FPos^ in [#10, #13];
  if FPos^ = ' ' then
  begin
    SkeepBlanks(FPos);
    vResult := True;
  end;
  if vResult then
  begin
    vDigits := vDigits + vExp;
    if vDigits <> 0 then
      AValue := sgPower10(AValue, vDigits);
    if vNeg then
      AValue := -AValue;
  end
  else
  begin
    //AValue := 0;
    SeekEOL;
  end;
end;

// 1000: ASCII string (up to 255 bytes long) in extended data
// 1001: Registered application name (ASCII string up to 31 bytes long) for extended data
// 1002: Extended data control string (“{” or “}”)
// 1003: Extended data layer name
// 1004: Chunk of bytes (up to 127 bytes long) in extended data
// 1005: Entity handle in extended data; text string of up to 16 hexadecimal digits
// 1010: A point in extended data DXF: X value (followed by 1020 and 1030 groups) APP: 3D point
// 1020, 1030: DXF: Y and Z values of a point
// 1011: A 3D world space position in extended data DXF: X value (followed by 1021 and 1031 groups) APP: 3D point
// 1021, 1031: DXF: Y and Z values of a world space position
// 1012: A 3D world space displacement in extended data DXF: X value (followed by 1022 and 1032 groups) APP: 3D vector
// 1022, 1032: DXF: Y and Z values of a world space displacement
// 1013: A 3D world space direction in extended data DXF: X value (followed by 1022 and 1032 groups) APP: 3D vector
// 1023, 1033: DXF: Y and Z values of a world space direction
// 1040: Extended data double-precision floating-point value
// 1041: Extended data distance value
// 1042: Extended data scale factor
// 1070: Extended data 16-bit signed integer
// 1071: Extended data 32-bit signed long
procedure TdxfReader.ReadEED(AObj: TdxfObject);
var
  vData: TsgCADExtendedData;
  vPPt: PFPoint;
  J, C: Integer;

  procedure AddPoint(ACode: SmallInt; AData: TsgCADExtendedData;
    var APPoint: PFPoint);
  var
    I: Integer;
  begin
    I := (ACode - 1000) div 10 - 1;
    if I = 0 then
    begin
      APPoint := PFPoint(AData.Data[AData.AddPoint(ACode, cnstFPointZero)].EData);
      APPoint^.V[I] := FloatValue;
    end
    else
      if APPoint <> nil then
      begin
        APPoint^.V[I] := FloatValue;
        if I = 2 then
          APPoint := nil;
      end;
  end;

begin
  vData := TsgCADExtendedData.Create(TsgDWGVersion(Version));
  C := AObj.FEEDItems.Count;
  J := AObj.FEEDItems.AddObject(UValue, vData);
  if AObj.FEEDItems.Count = C then
  begin
    vData.Free;
    vData := TsgCADExtendedData(AObj.FEEDItems.Objects[J]);
  end
  else
    AObj.InitEEDAppName(vData, Self);
  vPPt := nil;
  repeat
    Next;
    case Code of
      1000: vData.AddString(Code, UValue);
      1001: ReadEED(AObj);// ?? start next EED
      1002: vData.AddString(Code, UValue);
      1003: vData.AddString(Code, UValue);
      1004: vData.AddBinary(Code, AnsiString(StrValue));
      1005: AObj.ConvertEEDHandle(Code, vData, Self);
      1010..1039: AddPoint(Code, vData, vPPt);
      1040..1042: vData.AddDouble(Code, FloatValue);
      1070: AObj.ConvertEEDInt16(Code, vData, Self);
      1071: vData.AddInt(Code, IntValue);
    end;
  until (Code = Info_0) or EOF;
end;

procedure TdxfReader.ReadFileInfo;
var
  vValue: string;
  I, vCodePage: Integer;
  vCSTSaved: Boolean;
begin
  vCSTSaved := FCSTSaved;
  vValue := AnsiUpperCase(UValue);
  if (AnsiPos('INVENTORY', vValue) > 0) then//for version inventory
  begin
    vCodePage := GetDWGCodePageByName('ANSI_1251');
    TsgDXFConverterAccess(FConverter).SetCodePage(vCodePage);
  end;
  if AnsiPos(AnsiUpperCase(cnstExporterSoftwareInfo), vValue) > 0 then
    FCSTSaved := True
  else
    if AnsiPos(AnsiUpperCase(cnstCreatedFileInfo), vValue) > 0 then
      FCSTSaved := True;
  FFileInfo.Add(vValue);
  if FCSTSaved and (vCSTSaved <> FCSTSaved) then
  begin
    if FClasses.FindItem(DoReadHash(cnstDictionary), I) then
      PPointer(FClasses.List[I])^ := TcstDictionary;
    if FClasses.FindItem(DoReadHash(cnstXRecord), I) then
      PPointer(FClasses.List[I])^ := TcstXRecord;
  end;
end;

procedure TdxfReader.ReadCode(var AValue: Integer);
var
  vNeg: Boolean;
{$IFDEF CS_PLUGINS}
  S: string;
  P: PAnsiChar;
{$ENDIF}
begin
  AValue := 0;
{$IFDEF CS_PLUGINS}
  P := FPos;
{$ENDIF}
  vNeg := IsNeg(FPos);
  if not DoIntValue(FPos, AValue) then
  begin
    SkeepBlanks(FPos);
    SeekEOL;
{$IFDEF CS_PLUGINS}
    SetString(S, P, FPos - P);
    raise EReadError.CreateFmt('Invalid group code %s', [S]);
{$ENDIF}
  end;
  if vNeg then
    AValue := -AValue;
end;

procedure TdxfReader.ReadInt(var AValue: Integer);
var
  vNeg: Boolean;
begin
  AValue := 0;
  vNeg := IsNeg(FPos);
  if not DoIntValue(FPos, AValue) then
  begin
    if FPos^ = ' ' then
    begin
      SkeepBlanks(FPos);
      if vNeg then
        AValue := -AValue;
    end
    else
    begin
      AValue := 0;
      SeekEOL;
    end;
  end
  else
    if vNeg then
      AValue := -AValue;
end;

function TdxfReader.RemoveLayoutPerBlockRecord(ALayout: TsgDXFEntity): Integer;
begin
  Result := -1;
  if Assigned(ALayout) then
  begin
    Result := FLayoutsPerBlockRecord.IndexOf(ALayout.Handle);
    if Result >= 0 then
      FLayoutsPerBlockRecord.Delete(Result);
  end;
end;

procedure TdxfReader.ReadHash(var AValue: Cardinal);
begin
  QHashName(FPos, AValue);
end;

procedure TdxfReader.ReadHandle(var AValue: UInt64);
begin
  AValue := 0;
  if not DoHandleValue(FPos, AValue) then
  begin
    if FPos^ = ' ' then
      SkeepBlanks(FPos)
    else
    begin
      AValue := 0;
      SeekEOL;
    end;
  end;
end;

procedure TdxfReader.SeekEOL;
begin
  SeekEoln(FPos);
end;

procedure TdxfReader.StoreEED(AObj: TdxfObject);
begin
  if AObj.FEEDItems.Count > 0 then
  begin
    FDXFEED.Add(AObj.FHandle, AObj.FEEDItems);
    AObj.FEEDItems := nil;
  end;
end;

{public}

function TdxfReader.CheckXInfo: UInt64;
const
  cnstXInfoSeed: array[0 .. SizeOf(UInt64) shl 1 - 1] of AnsiChar = '0000000000000000';
var
  vStore: AnsiString;
  vBase, vStrSeedStart: PByte;
  vLen: Integer;
begin
  Result := 0;
  if FXInfoOffs > 0 then
  begin
    vBase := PByte(Conv.Source.Memory);
    vStrSeedStart := vBase;
    Inc(vStrSeedStart, FXInfoOffs);
    vLen := Length(cnstXInfoSeed);
    SetLength(vStore, vLen);
    Move(vStrSeedStart^, PPointer(vStore)^, vLen);
    try
      Move(cnstXInfoSeed[0], vStrSeedStart^, vLen);
      Result := GetHash64(vBase, Conv.Source.Size, 0);
    finally
      Move(PPointer(vStore)^, vStrSeedStart^, vLen);
    end;
    if Assigned(FXInfo) then
    begin
      TsgCADExtendedDataAccess(FXInfo.Data).AddBinary(312, SizeOf(Result), PByte(@Result));
      Conv.SetEntDict(AppIDEnt[GetAppID], FXInfo.Owner);
    end;
  end;
end;

procedure TdxfReader.ConvertDXFEED;
var
  I, J: Integer;
  vEntityEEDItems, vItem: TsgObjectCollectionAccess;
  vStrItems: TStringList;
  vAppIDEnt: TsgDXFEntity;
  Obj: TObject;
begin
  vEntityEEDItems := TsgObjectCollectionAccess(Conv.EntityEEDItems);
  vEntityEEDItems.Flags := vEntityEEDItems.Flags and not cnstSortedBit
    and not cnstDuplicatesMask or Ord(dupAccept);
  try
    I := FDXFEED.Count - 1;
    while I >= 0 do
    begin
      vStrItems := TStringList(FDXFEED.Objects[I]);
      try
        vItem := TsgObjectCollectionAccess(TsgObjectCollection.Create);
        vItem.Duplicates := dupIgnore;
        vItem.Flags := vItem.Flags and not cnstSortedBit
          and not cnstDuplicatesMask or Ord(dupAccept);
        J := vStrItems.Count - 1;
        while J >= 0 do
        begin
          vAppIDEnt := AppIDEnt[vStrItems[J]];
          Obj := vStrItems.Objects[J];
          vStrItems.Delete(J);
          if Assigned(vAppIDEnt) then
            vItem.Add(vAppIDEnt.Handle, Obj)
          else
            Obj.Free; // AppID obj not exists
          Dec(J);
        end;
        if vItem.Count > 0 then
        begin
          vEntityEEDItems.Add(FDXFEED[I].HashCode, vItem);
          vItem.Flags := vItem.Flags or cnstSortedBit or
            not cnstDuplicatesMask or Ord(dupIgnore);
          vItem.Sort;
        end
        else
          vItem.Free;
        FDXFEED.Delete(I);
        Dec(I);
      finally
        vStrItems.Free;
      end;
    end;
  finally
    vEntityEEDItems.Flags := vEntityEEDItems.Flags or cnstSortedBit or
      not cnstDuplicatesMask or Ord(dupIgnore);
    vEntityEEDItems.Sort;
  end;
end;

procedure TdxfReader.Coord(APoints: PFPointArray);
var
  A, B: Integer;
begin
  A := Code div 10 - 1;
  B := Code mod 10;
  APoints^[B].V[A] := FloatValue;
end;

constructor TdxfReader.Create(Conv: TsgDXFConverter);
var
  I: Integer;
  ObjectCollection: TsgObjCollection;
  Item: TdxfReaderMetaclass;

  function MakeReaderMetaClass(const AEntName: string;
    AEntClass: TsgDXFEntityClass; AObjClass: TdxfObjectClass): TdxfReaderMetaclass;
  begin
    Result := TdxfReaderMetaclass.Create;
    Result.Name := cnstNamesToEntsAndClasses[I].Name;
    Result.EntClass := cnstNamesToEntsAndClasses[I].Entity;
    Result.Handle := DoReadHash(Result.Name);
    PPointer(Result)^ := cnstNamesToEntsAndClasses[I].EntityClass;
  end;

begin
  FEnc := EncMgr.FromLANGID({$IFDEF SG_FIREMONKEY}TEncoding.Default.CodePage{$ELSE}SysLocale.DefaultLCID{$ENDIF});
  FCheckHandle := TsgDXFConverterAccess(Conv).CheckHandle;
  FClasses := TsgQReaderMetaclsssList.Create;
  FClasses.Sorted := False;
  for I := Low(cnstNamesToEntsAndClasses) to High(cnstNamesToEntsAndClasses) do
  begin
    Item := MakeReaderMetaClass(cnstNamesToEntsAndClasses[I].Name,
      cnstNamesToEntsAndClasses[I].Entity, cnstNamesToEntsAndClasses[I].EntityClass);
    FClasses.InsertFinded(FClasses.Count, Item);
  end;
{$IFDEF DATA_LINK}
  Item := MakeReaderMetaClass('DATALINK', TsgCADDataLink, TdxfDataLink);
  FClasses.InsertFinded(FClasses.Count, Item);
{$ENDIF}

  FClasses.Sorted := True;

  FGlobalNames := TsgStringList.Create;
  TsgStringList(FGlobalNames).Duplicates := dupIgnore;
  TsgStringList(FGlobalNames).CaseSensitive := True;
  TsgStringList(FGlobalNames).Sorted := True;
  DictionaryName := FGlobalNames[FGlobalNames.Add(DictionaryName)];
  FNoLayersTable := TsgStringList.Create;
  TsgStringList(FNoLayersTable).Duplicates := dupIgnore;
  TsgStringList(FNoLayersTable).CaseSensitive := False;
  TsgStringList(FNoLayersTable).Sorted := True;
  FDataBase := TsgQCADItemsList.Create;
  FDataBase.Sorted := True;
  SetLength(FNames, 1024);
  FSortents := 127;
  FConverter := Conv;
  FMain := TdxfMain.Create;
  TdxfMain(FMain).FConv := TsgDXFConverterAccess(Conv);
  FMain.FEnt := Conv.Main;
  ObjectCollection := TsgObjCollection.Create;
  ObjectCollection.FMap := FDataBase;
  TsgDXFConverterAccess(Conv).FMapExternal := ObjectCollection as IsgObjCollection;
  FFileInfo := TStringList.Create;
  FValueSize := 0;
  FPapers := TsgDXFPaperBlocksRefs.Create;
  FDXFEED := TsgObjectCollection.Create;
{$IFDEF SG_READER_DEBUG}
  GlobalEntityDestroyNotification := EntityDestroy;
{$ENDIF}
  FLayoutsPerBlockRecord := TsgObjectCollection.Create;
end;

function TdxfReader.CreateBlock(const AName: string): TsgDXFBlock;
begin
  Result := TsgDXFBlock.Create;
  Result.Name := AName;
end;

destructor TdxfReader.Destroy;
var
  I: Integer;
  vCADItem: TsgDXFEntityAccess;
  vSection: TConvSection;
  vLayout: TsgDXFLayoutAccess;
  vBlockRecord: TsgDXFBlockRecordAccess;
  vClass: TClass;
begin
  FFileInfo.Free;
  FMain.Free;
  for I := 0 to FPapers.Count - 1 do
  begin
    vBlockRecord := TsgDXFBlockRecordAccess(TsgDXFBlock(FPapers[I]).BlockRecord);
    vBlockRecord.RemoveDestroyNotification(BlockRecordDestroy);
    RemoveLayoutPerBlockRecord(vBlockRecord.Block.Layout);
  end;
  FPapers.Clear(False);
  FPapers.Free;
  FDXFEED.Free;
  FProxyReaderImpl.Free;
  for I := 0 to FLinksCount - 1 do
    FLinks[I].Link.Free;
  for I := FDataBase.Count - 1 downto 0 do
  begin
    vCADItem := TsgDXFEntityAccess(FDataBase[I]);
    FDataBase.Delete(I);
    vClass := vCADItem.ClassType;
    if vClass = TsgDXFBlockRecord then
      TsgDXFBlockRecordAccess(vCADItem).RemoveDestroyNotification(BlockRecordDestroy)
    else
      if vClass = TsgDXFLayout then
        vCADItem.RemoveDestroyNotification(EntityDestroy);
  end;
  if Assigned(FSections[csBlockRecords]) then
    for I := 0 to FSections[csBlockRecords].Count - 1 do
    begin
      vBlockRecord := TsgDXFBlockRecordAccess(FSections[csBlockRecords].Objects[I]);
      vBlockRecord.RemoveDestroyNotification(BlockRecordDestroy);
    end;
  for I := 0 to Conv.Sections[csBlockRecords].Count - 1 do
  begin
    vBlockRecord := TsgDXFBlockRecordAccess(Conv.Sections[csBlockRecords][I]);
    vLayout := TsgDXFLayoutAccess(vBlockRecord.Block.Layout);
    if RemoveLayoutPerBlockRecord(vLayout) >= 0 then
    begin
      vLayout.PaperSpaceBlock := nil;
      vLayout.Free;
    end;
  end;
{$IFDEF SG_READER_DEBUG}
  GlobalEntityDestroyNotification := nil;
{$ENDIF}
  TsgDXFConverterAccess(Conv).FMapExternal := nil;
  FDataBase.Free;
  for vSection := Low(TConvsection) to High(TConvsection) do
    FSections[vSection].Free;
  FNoLayersTable.Free;
  Finalize(FNames);
  FGlobalNames.Free;
  for I := 0 to FClasses.Count - 1 do
  begin
    PPointer(FClasses.List[I])^ := TdxfReaderMetaclass;
    FreeAndNil(FClasses.List[I]);
  end;
  FClasses.Free;
  FLayoutsPerBlockRecord.Free;
  inherited Destroy;
end;

function TdxfReader.FindObject(AHandle: UInt64; var Index: Integer): Boolean;
begin
  Result := FDataBase.FindItem(AHandle, Index);
end;

procedure TdxfReader.FixDimStyleBlockRecordRefsToPaperSpace;
var
  I: Integer;
  J: TsgDimNameVal;
  vDimStyles: TsgDXFEntity;
  vDimStyle: TsgDXFDimStyleAccess;
  vBlock: TsgDXFBlock;
begin
  vDimStyles := Conv.Sections[csDimStyles];
  if Assigned(vDimStyles) then
    for I := 0 to vDimStyles.Count - 1 do
    begin
      vDimStyle := TsgDXFDimStyleAccess(vDimStyles[I]);
      for J := vnDIMBLK to vnDIMLRBLK do
      begin
        vBlock := vDimStyle.GetBlocks(J);
        if Assigned(vBlock) and Assigned(vBlock.Layout) then
          vDimStyle.SetBlocks(J, nil);
      end;
    end;
end;

function TdxfReader.FloatValue: Double;
begin
  if CodeTypes^[FCode] <> ctDouble then
    FDouble := ConvToFloatDef(Trim(UValue), 0);
  Result := FDouble;
end;

procedure TdxfReader.ForceNamedObjectsDictionary;
var
  vStat: TConvStatus;
begin
  if FNamedObjects = nil then
  begin
    vStat := Conv.Status;
    try
      Conv.Status := stDefault;
      if Conv.Counts[csObjects] <= 0 then
      begin
        FNamedObjects := TsgDXFDictionary.Create;
        FNamedObjects.Name := cnstDictionary;
        Conv.SetHandle(FNamedObjects);
        Conv.Sections[csObjects].InsertEntity(0, FNamedObjects);
      end;
    finally
      Conv.Status := vStat;
    end;
  end;
end;

procedure TdxfReader.ForcePaperSpaces;
begin
  if FPapers.Count = 0 then
    InitPaper(CreateBlock(sModelSpace));
  if FPapers.Count = 1 then
    InitPaper(CreateBlock(sPaperSpace));
end;

procedure TdxfReader.ForceStandardObjects;
var
  I: Integer;
  vLayer0: TsgDXFEntity;
  vLTypes: array[0..2] of TsgDXFEntity;
  vVPort: TsgDXFVport;
  vArrows: TsgStringList;
  vArrowType: TsgDimArrowType;
  vArrowBlock: TsgDXFBlock;
begin
  Conv.CustomEntByName(sStandardName, csStyles);
  vLTypes[0] := Conv.CustomEntByName(sByLayer, csLTypes);
  vLTypes[1] := Conv.CustomEntByName(sByBlock, csLTypes);
  vLTypes[2] := Conv.CustomEntByName(sContinuous, csLTypes);
  TsgDXFTableAccess(Conv.Sections[csLTypes]).SortEntByHandle;
  Conv.Sections[csLTypes].RemoveEntity(vLTypes[0]);
  Conv.Sections[csLTypes].RemoveEntity(vLTypes[1]);
  Conv.Sections[csLTypes].RemoveEntity(vLTypes[2]);
  Conv.Sections[csLTypes].InsertEntity(0, vLTypes[2]); // 0
  Conv.Sections[csLTypes].InsertEntity(0, vLTypes[0]);    //-1
  Conv.Sections[csLTypes].InsertEntity(0, vLTypes[1]);    //-2
  vLayer0 := Conv.LayerByName('0');
  TsgDXFTableAccess(Conv.Sections[csLayers]).SortEntByHandle;
  Conv.Sections[csLayers].RemoveEntity(vLayer0);
  Conv.Sections[csLayers].InsertEntity(0, vLayer0);

  if Conv.Sections[csDimStyles] = nil then
    Conv.CustomEntByName(sStandardName, csDimStyles);

  vArrows := TsgStringList.Create;
  try
    vArrows.CaseSensitive := False;
    vArrows.Duplicates := dupIgnore;
    vArrows.Sorted := True;
    for I := 0 to FLinksCount - 1 do
      if (FLinks[I].Link.ClassType = TsgBlockRecordLink) and Assigned(FLinks[I].Link.Collection) then
        if vArrows.IndexOf(FLinks[I].Link.AsString) < 0 then
        begin
          vArrowType := GetArrowTypeByName(FLinks[I].Link.AsString, datUndefined);
          if (vArrowType <> datUserarrow) and (vArrowType <> datUndefined) then
            vArrows.AddObject(FLinks[I].Link.AsString, FLinks[I].Link.PropItem);
        end;
    for I := 0 to vArrows.Count - 1 do
    begin
      vArrowBlock := TsgDXFBlockRecordAccess(vArrows.Objects[I]).Block;
      if not vArrowBlock.IsLoaded and (vArrowBlock.Count = 0) then
        GenerateArrow(FConverter, vArrowBlock, GetArrowTypeByName(vArrows[I], datClosedfilled));
    end;
  finally
    vArrows.Free;
  end;
  if not Assigned(FActiveVPort) then
  begin
    for I := 0 to Conv.Counts[csVPorts] - 1 do
    begin
      vVPort := TsgDXFVport(Conv.Sections[csVPorts].Entities[I]);
      if SameText(vVPort.Name, sActiveVPort) then
      begin
        FActiveVPort := vVPort;
        break;
      end;
    end;
  end;
  if Assigned(FActiveVPort) then
    Conv.ActiveVPort := FActiveVPort
end;

function TdxfReader.HandleValue: UInt64;
begin
  if CodeTypes^[FCode] <> ctHex then
    FHandle := StringHexToIntDef(Trim(UValue), cnstBadHandle);
  Result := FHandle;
end;

procedure TdxfReader.InitPaper(const ABlock: TsgDXFEntity);
var
  I: Integer;
begin
  I := GetPaperIndex(ABlock.Name);
  case I of
    -1:; // not space
    0:   // [*|$]model_space
      begin
        if FPapers.IndexOfEntity(ABlock) < 0 then
          FPapers.InsertEntity(0, ABlock);
      end;
    1:  // [*|$]paper_space
      begin
        if FPapers.IndexOfEntity(ABlock) < 0 then
          if FPapers.Count <= 0 then
            FPapers.AddEntity(ABlock)
          else
            FPapers.InsertEntity(1, ABlock);
      end
  else // [*|$]paper_space[number + 2]
    if FPapers.IndexOfEntity(ABlock) < 0 then
      FPapers.AddEntity(ABlock);
  end;
end;

procedure TdxfReader.UpdateClasses(AClasses: TdxfObject);
var
  I, C: Integer;
  vMetaObj: TdxfReaderMetaclass;
begin
  for I := 0 to AClasses.FEnt.Count - 1 do
    if TsgDXFClass(AClasses.FEnt[I]).IsEntity then
    begin
      vMetaObj := TdxfReaderMetaclass.Create;
      vMetaObj.Name := AClasses.FEnt[I].Name;
      vMetaObj.EntClass := TsgDXFProxy;
      vMetaObj.Handle := DoReadHash(AClasses.FEnt[I].Name);
      C := FClasses.Count;
      FClasses.AddItem(vMetaObj);
      if C = FClasses.Count then
        vMetaObj.Free
      else
        PPointer(vMetaObj)^ := TdxfMetaProxy;
    end;
end;

procedure TdxfReader.UpdateDatabase(AObj: TdxfObject);
begin
  if not AObj.FDatabaseAdded and Assigned(AObj.FEnt) and
     (AObj.FEnt.Handle <> cnstBadHandle) then
  begin
    FDataBase.AddItem(AObj.FEnt);
    AObj.FDatabaseAdded := True;
  end;
end;

procedure TdxfReader.UpdateLimits;
var
  P: PsgHeadVarStruct;
begin
  if FPapers.Count > 0 then
  begin
    P := TsgDXFConverterAccess(FConverter).PHeadVarStruct;
    TsgDXFLayout(TsgDXFBlock(FPapers[0]).Layout).LimMin := P^.LimMin;
    TsgDXFLayout(TsgDXFBlock(FPapers[0]).Layout).LimMax := P^.LimMax;
    if (FPapers.Count > 1) and Assigned(TsgDXFBlock(FPapers[1]).Layout) then
    begin
      TsgDXFLayout(TsgDXFBlock(FPapers[1]).Layout).LimMin := P^.PLimMin;
      TsgDXFLayout(TsgDXFBlock(FPapers[1]).Layout).LimMax := P^.PLimMax;
    end;
  end;
end;

procedure TdxfReader.UpdateMaterials(AObj: TdxfObject);
begin
  if (AObj.FMaterial <> cnstBadHandle) and Assigned(AObj.FEnt) then
    AObj.FEnt.Material := TsgDXFMaterial(ObjByHandle(AObj.FMaterial, TsgDXFMaterial));
end;

procedure TdxfReader.UpdateViewportsOrdIndex;
begin
  TsgDXFPaperBlocksRefs(FPapers).UpdateViewportsOrdIndex;
end;

procedure TdxfReader.UpdateXInfo(AXrec: TsgDXFXRecord);
begin
  if FCSTSaved and not Assigned(FXInfo) and (FXInfoHandle <> cnstBadHandle) then
    if AXrec.Handle = FXInfoHandle then
    begin
      FXInfo := AXrec;
      FXInfoOffs := TsgNativeInt(FPrev) - TsgNativeInt(Conv.Source.Memory)
        -  SizeOf(UInt64) shl 1 - 2;
    end;
end;

function TdxfReader.AddLink(obj: TObject; const APropName, AValue: string;
  AProxyClass: TClass): Integer;
begin
  if AProxyClass = nil then
    AProxyClass := obj.ClassType;
  Result := AddLink(obj, GetPropInfo(AProxyClass, APropName), AValue, AProxyClass);
end;

function TdxfReader.AddLink(obj: TObject; AProp: Pointer; const AValue: string;
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
      if vTypeData^.ClassType = TsgDXFLayer then
        ObjectLinkClass := TsgLayerLink
      else
      if vTypeData^.ClassType = TsgDXFLineType then
        ObjectLinkClass := TsgLTypeLink
      else
      if vTypeData^.ClassType = TsgDXFDimensionStyle then
        ObjectLinkClass := TsgDimStyleLink
      else
      if vTypeData^.ClassType = TsgDXFImageDef then
        ObjectLinkClass := TsgImageDefLink
      else
      if vTypeData^.ClassType = TsgDXFStyle then
        ObjectLinkClass := TsgStyleLink
      else
      if (vTypeData^.ClassType = TsgDXFBlockRecord) or (vTypeData^.ClassType = TsgDXFBlock) then
        ObjectLinkClass := TsgBlockRecordLink;
    end;
    if Assigned(ObjectLinkClass) then
    begin
      Result := DoAddLink(obj, AProp, ObjectLinkClass.Create);
      FLinks[Result].Link.AsString := GlobalNames[AValue];
    end;
  end;
end;

procedure TdxfReader.AddBinData(var AData: Pointer; var ASize: Integer);
begin
  DoAddBinData(FValueStartPos, FValueSize, AData, ASize);
end;

function TdxfReader.AddLink(obj: TObject; AProp: Pointer; const AValue: UInt64;
  AProxyClass: TClass): Integer;
begin
  Result := -1;
  if AValue <> cnstBadHandle then
  begin
    Result := DoAddLink(obj, AProp, TsgDatabaseHandleLink.Create);
    FLinks[Result].Link.AsHandle := AValue;
    FLinks[Result].Link.FCollection := FDataBase;
  end;
end;

function TdxfReader.AppendLinkNames(Handle: UInt64;
  const AName: string): Integer;
var
  Capacity: Integer;
begin
  Result := FNamesCount;
  Capacity := Length(FNames);
  if FNamesCount = Capacity then
    SetLength(FNames, ListGrow(Capacity));
  FNames[Result].ID := Handle;
  FNames[Result].Name := AName;
  Inc(FNamesCount);
end;

function TdxfReader.AppendString(var AStr: AnsiString): Integer;
begin
  Result := Length(AStr);
  if FValueSize > 0 then
  begin
    SetLength(AStr, Result + FValueSize);
    System.Move(FValueStartPos^, PAnsiChar(AStr)[Result], FValueSize);
  end;
end;

function TdxfReader.AddLink(obj: TObject; const APropName: string;
  const AValue: UInt64; AProxyClass: TClass): Integer;
begin
  if AProxyClass = nil then
    AProxyClass := obj.ClassType;
  Result := AddLink(obj, GetPropInfo(AProxyClass, APropName), AValue, AProxyClass);
end;

procedure TdxfReader.BlockRecordDestroy(Sender: TObject);
var
  I: Integer;
begin
  EntityDestroy(Sender);
  if Assigned(TsgDXFBlockRecordAccess(Sender).Block) then
    EntityDestroy(TsgDXFBlockRecordAccess(Sender).Block);
  if Assigned(FSections[csBlockRecords]) then
  begin
    I := FSections[csBlockRecords].IndexOfObject(Sender);
    if I >= 0 then
    begin
      FSections[csBlockRecords].Delete(I);
      RemoveLayoutPerBlockRecord(TsgDXFBlockRecordAccess(Sender).Block.Layout);
    end;
  end;
end;

procedure TdxfReader.BlocksListNotifyHandler(const Obj: TObject; Action: TListNotification);
begin
  if Action in [lnDeleted, lnExtracted] then
    TsgDXFPaperBlocksRefs(FPapers).RemoveEntity(TsgDXFEntity(Obj));
  if Assigned(FBlocksListNotify) then
    FBlocksListNotify(Obj, Action);
end;

function TdxfReader.IntValue: Integer;
begin
  if CodeTypes^[FCode] <> ctInteger then
    FInt := StrToIntDef(Trim(UValue), 0);
  Result := FInt;
end;

function TdxfReader.IsInternalCADImage: Boolean;
begin
  Result := (XDataProcs <> nil) and (FFileInfo.Count > 0) and
   ((FFileInfo[0] = AnsiUpperCase(cnstExporterSoftwareInfo)) or
    (FFileInfo[0] = AnsiUpperCase(cnstCreatedFileInfo)));
end;

function TdxfReader.IsACADAppID(const AAppId: string): Boolean;
begin
  Result := sgSameText(AAppId, sACADXDataAppName);
end;

function TdxfReader.IsRelatedAppID(AAppId: string): Boolean;
begin
  AAppId := AnsiUpperCase(AAppId);
  Result := (FCSTSaved and (AAppId = AnsiUpperCase(GetAppID))) or
    (AnsiPos(AAppId, FileInfo) > 0);
end;

function TdxfReader.IsURLAppID(const AAppId: string): Boolean;
begin
  Result := UpperCase(AAppId) = UpperCase(sURLXDataName);
end;

function TdxfReader.IsValueEqual(const AValue: AnsiString): Boolean;
begin
  Result := (Length(AValue) = FValueSize) and
    CompareMem(FValueStartPos, Pointer(AValue), FValueSize);
end;

{$IFDEF UNICODESTRING_TYPE_DEFINED}
function TdxfReader.IsValueEqual(const AValue: UnicodeString): Boolean;
begin
  Result := IsValueEqual(AnsiString(AValue));
end;
{$ENDIF}

procedure TdxfReader.LinkObjectLinkCollections;
var
  I: Integer;
begin
  I := Low(FLinks);
  while I < FLinksCount do
  begin
    FLinks[I].Link.DoLink(FLinks[I].obj, FLinks[I].Prop);
    Inc(I);
  end;
end;

procedure TdxfReader.InitLayouts(const AModel, APaper: TsgDXFBlock);
var
  vLayout: TsgDXFEntity;
  vPaper, vModel: TsgDXFBlock;
  vLayouts: TsgDXFEntity;

  // ALayouts - section
  // ACurrentLayout - in/out
  procedure _CleaningLayouts(ALayouts: TsgDXFEntity;
    var ACurrentLayout: TsgDXFLayout);
  var
    I: Integer;
    vTmpLayout: TsgDXFLayoutAccess;
    vTmpBlock: TsgDXFEntity;
  begin
    I := ALayouts.Count - 1;
    while I >= 1 do
    begin
      vTmpLayout := TsgDXFLayoutAccess(ALayouts[I]);
      if vTmpLayout is TsgDXFLayout then
        if vTmpLayout.IsEmpty then
        begin
          vTmpBlock := nil;
          if Assigned(vTmpLayout.BlockRecord) then
            vTmpBlock := TsgDXFBlockRecordAccess(vTmpLayout.BlockRecord).Block;
          vTmpLayout.ClearReferences;
          if vTmpLayout = ACurrentLayout then
            ACurrentLayout := nil;
          ALayouts.DeleteEntity(I);
          RemoveLayoutPerBlockRecord(vTmpLayout);
          vTmpLayout.Free;
          if I >= 2 then // do not delete AModel and APaper blocks
            if Assigned(vTmpBlock) then
            begin
              if Assigned(vTmpBlock.Owner) then
                vTmpBlock.Owner.RemoveEntity(vTmpBlock);
              vTmpBlock.Free;
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
  end
  else
    if vPaper.Layout.Owner = nil then
    begin
      vPaper.Layout.Name := sLayout + '1';
      vLayouts.AddEntity(vPaper.Layout);
      TsgDXFLayout(vPaper.Layout).PaperSpaceBlock := vPaper;
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

procedure TdxfReader.Read;
var
  vDS: Char;
  I: Integer;
  vObj: TsgOwneredItem;
  vFreeList: TsgObjectList;
//  vObj: TsgDXFEntity;
//  vSortentsTable, vSortentsVar, vVarDict: TsgDXFEntity;

  {procedure SortPapersByHandle;
  var
    I: Integer;
    vBlock: TsgDXFBlockAccess;
  begin
    for I := 0 to Conv.Counts[csLayouts] - 1 do
    begin
      vBlock := TsgDXFBlockAccess(TsgDXFLayout(Conv.Sections[csLayouts][I]).PaperSpaceBlock);
      vBlock.SortEntByHandle;
    end;
  end;

  procedure CustomSort;
  begin
    if not Conv.SortEntities then
      SortPapersByHandle
  end;}

  procedure FreeSection(AConvSection: TConvSection);
  var
    vSection: TsgDXFEntity;
  begin
    vSection := Conv.Sections[AConvSection];
    Conv.Sections[csObjects].RemoveEntity(vSection);
    Conv.Sections[AConvSection] := nil;
    vSection.Free;
  end;

begin
  Conv.Clear;

//  vObj := Conv.Sections[csLayouts];
//  Conv.Sections[csLayouts] := nil;
//  vObj.Free;

  FBlocksListNotify := TsgDXFGroupAccess(Conv.Sections[csBlocks]).OnListNotify;
  TsgDXFGroupAccess(Conv.Sections[csBlocks]).OnListNotify := BlocksListNotifyHandler;
  try
  Conv.SetLoading(True, Conv.Source.Memory, FUpdateEvent);
  try
    FPos := Conv.Source.Memory;
    FPrev := FPos;
    FLimit := FPos + Conv.Source.Size;
    Conv.HeadEnd := FLimit;
    vDS := SetDecimalSeparator('.');
    try
      DoEvent(Conv.BeforeRead);
      try
        FMain.ReadState(Self);
      except
        on E: Exception do
        begin
          E.Message := Format('DXF read error on line %d: %s'#13#10'%s',
            [FLine, StrValue, E.Message]);
          raise;
        end;
      end;
    finally
      SetDecimalSeparator(vDS);
    end;
    DoSetNames;
    CheckXInfo;
    ForceNamedObjectsDictionary;
    FreeSection(csImageDefs);
    FreeSection(csLayouts);
    Conv.FillMainSection;
    ForceStandardObjects;
    ForceStdDictionaries(Conv, FNamedObjects.Handle);

    PrelinkObjectLinkCollections;
    LinkObjectLinkCollections;

    ForcePaperSpaces;
    UpdateViewportsOrdIndex;
    FixDimStyleBlockRecordRefsToPaperSpace;
    InitLayouts(TsgDXFBlock(FPapers[0]), TsgDXFBlock(FPapers[1]));
    if (FPapers.Count > 0) and (FPapers[0].Owner = nil) then
      Conv.Sections[csBlocks].InsertEntity(0, FPapers[0]);
    if (FPapers.Count > 1) and (FPapers[1].Owner = nil) then
      Conv.Sections[csBlocks].InsertEntity(1, FPapers[1]);
{$IFDEF SG_BTI}
    if Conv.XDataProcs <> nil then
      Conv.XDataProcs.Initialize(Conv); // sort ents
{$ENDIF}
    LinkDataBase(Conv);
    vFreeList := TsgObjectList.Create;
    try
      Conv.ApplyXRecodData(vFreeList);
      for I := vFreeList.Count - 1 downto 0 do
      begin
        vObj := TsgOwneredItem(vFreeList[I]);
        if vObj.ObjRelease = 0 then
        begin
          EntityDestroy(vObj.Ancestor);
          vObj.Ancestor.Free;
        end;
      end;
    finally
      vFreeList.Free;
    end;
    Conv.ApplyVariables;
    if Assigned(FProxyReaderImpl) then
    begin
      FProxyReaderImpl.UpdateHandles;
      FProxyReaderImpl.AddToDataBase;
    end;
    Conv.CompositeMLAtt;
    LoadDataBase(Conv);
    UpdateLimits;
    ConvertDXFEED;
    DoEvent(Conv.AfterRead);
  finally
    Conv.SetLoading(False,FLimit, nil);
    // sort entities
    (*
    vVarDict := Conv.Sections[csObjects].FindEntByName(sAcDbVariableDictionary);
    vSortentsVar := nil;
    if Assigned(vVarDict) then // dictionaryvar
    begin
      vSortentsVar := vVarDict.FindEntByName(sDictionaryVarSORTENTS);
      if Assigned(vSortentsVar) then
        FSortents := StrToIntDef(TsgDXFDictionaryVar(vSortentsVar).Value, FSortents);
    end;
    vSortentsTable := Conv.Sections[csObjects].FindEntByName(sDictionaryVarSORTENTS);
    if Assigned(vSortentsTable) then // dictionary
      CustomSort
    else
    begin
      if Assigned(vVarDict) then
      begin
        if Assigned(vSortentsVar) then // dictionaryvar
          SortPapersByHandle
        else
          CustomSort;
      end
      else
        CustomSort;
    end;
    *)
    Conv.SortEntities;
  end;
  finally
    TsgDXFGroupAccess(Conv.Sections[csBlocks]).OnListNotify := FBlocksListNotify;
    FBlocksListNotify := nil;
  end;
end;

procedure TdxfReader.Next;
begin
  FPrev := FPos;
  // read code
  SkeepBlanks(FPos);
  ReadCode(FCode);
  SkeepLimits(FPos);
  Inc(FLine);
  // read data
  FValueStartPos := FPos;
  if FPos < FLimit then
  begin
    SkeepBlanks(FPos);
    case FCode of
      0: ReadHash(FNameHash);
      102:
        begin
          case FPos^ of
            '{':
              begin
                Inc(FPos);
                ReadHash(FRefsHash);
              end;
            '}':
              begin
                FRefsHash := 0;
                SeekEOL;
              end;
          else
            SeekEOL;
          end;
        end;
    else
      case CodeTypes^[FCode] of
        ctInteger: ReadInt(FInt);
        ctDouble: ReadDouble(FDouble);
        ctHex: ReadHandle(FHandle);
      else
        SeekEOL;
      end;
    end; { case FCode }
    FValueSize := FPos - FValueStartPos;
    repeat
      SkeepLimits(FPos);
    until (FPos >= FLimit) or not (FPos^ in [#10, #13]);
    Inc(FLine);
    // check limits
    if FConverter.Pos = 0 then
      FPos := FLimit;
    if FPos >= FLimit then
    begin
      FValueSize := 0;
      FCode := 0;
    end;
  end;
end;

function TdxfReader.NoCheckHandle(var AHandle: UInt64): Boolean;
begin
  Result := True;
  if Conv.MaxHandle < AHandle then
    Conv.MaxHandle := AHandle;
end;

function TdxfReader.ObjByHandle(const AHandle: UInt64;
  AEntityClass: TsgDXFEntityClass = nil): TsgDXFEntity;
var
  I: Integer;
begin
  Result := nil;
  if AHandle <> cnstBadHandle then
    if FindObject(AHandle, I) then
      Result := TsgDXFEntity(FDataBase[I])
    else
      if AEntityClass <> nil  then
      begin
        Result := AEntityClass.Create;
        Result.Handle := AHandle;
        DoCreate(Result);
        DataBase.InsertFinded(I, Result);
      end;
end;

procedure TdxfReader.PrelinkObjectLinkCollections;
var
  I: Integer;
  vLayer: TsgDXFLayer;
  vClass: TClass;
begin
  for I := 0 to FLinksCount - 1 do
  begin
    if FLinks[I].Link.Collection = nil then
    begin
      vClass := FLinks[I].Link.ClassType;
      if vClass = TsgLayerLink then
        FLinks[I].Link.FCollection := Conv.Sections[csLayers]
      else
      if vClass = TsgLTypeLink then
        FLinks[I].Link.FCollection := Conv.Sections[csLTypes]
      else
      if vClass = TsgBlockRecordLink then
        FLinks[I].Link.FCollection := Conv.Sections[csBlockRecords]
      else
      if vClass = TsgDimStyleLink then
        FLinks[I].Link.FCollection := Conv.Sections[csDimStyles]
      else
      if vClass = TsgImageDefLink then
        FLinks[I].Link.FCollection := Conv.Sections[csImageDefs]
      else
      if (vClass = TsgStyleLink) or (vClass = TsgLTElementStyleLink) then
        FLinks[I].Link.FCollection := Conv.Sections[csStyles]
    end;
  end;
  if Assigned(FNoLayersTable) then
    for I := 0 to FNoLayersTable.Count - 1 do
    begin
      vLayer := TsgDXFLayer.Create;
      vLayer.Name := FNoLayersTable[I];
      Conv.Sections[csLayers].AddEntity(vLayer);
    end;
end;

class function TdxfReader.ACIS_Row_Decode(const AEncoded: AnsiString): AnsiString;
var
  vSpecial: Boolean;
  vChar: AnsiChar;
  I: Integer;
begin
  Result := '';
  vSpecial := False;
  for I := cnstStrBegin to Length(AEncoded) + (cnstStrBegin - 1) do
  begin
    vChar := AEncoded[I];
    if vSpecial then
    begin
      vSpecial := False;
      if vChar in ['@'..'_'] then //[#64..#95]
      begin
        Result := Result + AnsiChar(Byte(vChar) - Byte('@'));
        Continue;
      end;
      Result := Result + 'A';
      if vChar = ' ' then
        Continue;
    end;
    if vChar = '^' then
    begin
      vSpecial := True;
      Continue;
    end;
    Result := Result + ACIS_Cipher(vChar);
  end;
end;

{ TdxfBinReader }

{private}

procedure TdxfBinReader.Read(var ABuffer; ACount: Integer);
begin
  Move(FPos^, ABuffer, ACount);
  Inc(FPos,ACount);
end;

{public}

procedure TdxfBinReader.AddBinData(var AData: Pointer; var ASize: Integer);
var
  P: PByte;
begin
  if FValueSize > 0 then
  begin
    Inc(ASize, FValueSize);
    ReallocMem(AData, ASize);
    P := PByte(AData);
    Inc(P, ASize - FValueSize);
    System.Move(FValueStartPos^, P^, FValueSize);
  end;
end;

constructor TdxfBinReader.Create(AConverter: TsgDXFConverter;
  const AGroupCodeSize: Integer);
begin
  inherited Create(AConverter);
  FGroupCodeSize := AGroupCodeSize;
  sgFunction.InitByteBinToHex;
end;

function TdxfBinReader.FloatValue: Double;
begin
  Result := FDouble;
end;

function TdxfBinReader.HandleValue: UInt64;
begin
  Result := StringHexToIntDef(Trim(UValue), cnstBadHandle);
end;

function TdxfBinReader.IntValue: Integer;
begin
  Result := FInt;
end;

procedure TdxfBinReader.Next;
  procedure UpdateValueSize;
  begin
    SeekEoStr(FPos);
    FValueSize := FPos - FValueStartPos;
    Inc(FPos);
  end;
var
  vGroupCodeSize: Integer;
begin
  FCode := 0;
  FInt := 0;
  FDouble := 0;
  if FPos >= FLimit then
  begin
    Exit;
  end;
  // Group code is a 2-byte binary value (1 byte in DXF files prior
  // to AutoCAD Release 14)
  Read(FCode, FGroupCodeSize);
  // Value
  FValueSize := 0;
  FValueStartPos := FPos;
  case FCode of
    0:
      begin
        ReadHash(FNameHash);
        FValueSize := FPos - FValueStartPos;
        Inc(FPos);
      end;
    102:
        begin
          case FPos^ of
            '{':
              begin
                Inc(FPos);
                ReadHash(FRefsHash);
                FValueSize := FPos - FValueStartPos;
                Inc(FPos);
              end;
            '}':
              begin
                FRefsHash := 0;
                UpdateValueSize;
              end;
          else
            UpdateValueSize;
          end;
        end;
    1..9, 83..84, 100..101, 103..109, 300..309, 320..369,
      390, 430..437, 470..479, 1000..1003, 1005..1009:
        begin
          UpdateValueSize;
          //BinStr(FValue, FPos, FLimit);
        end;
    10..59, 110..149, 210..249, 460..469, 1010..1059:
      begin
        Read(FDouble, 8);
        if IsNan(FDouble) then
        begin
          PUInt64(@FDouble)^ := 0;
        end;
      end;
    290..299, {undocumented} 256..257, 512 {}:         Read(FInt, 1);
    90..99, 420..429, 440..449, 450..459, 1071:        Read(FInt, 4);
    310..319, 1004:
      begin
        FValueSize := 0;
        Read(FValueSize, 1);
        FValueStartPos := FPos;
        Inc(FPos, FValueSize);
      end;
    60..79, 170..179, 270..289, 370..389, 1060..1070:
      begin
        Read(FInt, 2);
        FInt := Smallint(FInt);
      end;
    160..169:
     begin
       Read(FInt64, 8);//add using the value in future version
     end;
    255:
      begin
        vGroupCodeSize := FGroupCodeSize;
        try
          FGroupCodeSize := 2;
          Next;
        finally
          FGroupCodeSize := vGroupCodeSize;
        end;
      end;
  else
    raise EReadError.CreateFmt('Illegal group code %d at %.8x',
      [FCode, TsgNativeInt(FPos) - TsgNativeInt(FPrev) + 20]);
  end;
end;

class function TdxfBinReader.ACIS_Row_Decode(const AEncoded: AnsiString): AnsiString;
var
  I: Integer;
begin
  SetLength(Result, Length(AEncoded));
  for I := cnstStrBegin to Length(AEncoded) + (cnstStrBegin - 1) do
    Result[I] := ACIS_Cipher(AEncoded[I]);
end;

{ TdxfEntity }

{private}

procedure TdxfEntity.Coord(AReader: TdxfReader);
begin
  AReader.Coord(@FPoints);
end;

procedure TdxfEntity.SetLWeight(const AValue: Double);
begin
  FEnt.LineWeight := ExpandLWeight(AValue);
end;

{protected}

constructor TdxfEntity.Create;
begin
  inherited Create;
  FFloats[8] := 1.0;
  FExtrusion.Z := 1.0;
  FInts[2] := 256;
  FIntsRGBColor := clNone;
  FLineWeight := fLineWeightByLayer;
  FTexts[6] := sByLayer;
end;

function TdxfEntity.GetElevation: Double;
begin
  if cnstLoadCad2D then
    Result := 0
  else
    Result := FElevation;
end;

function TdxfEntity.GetExtrusion: TFPoint;
begin
  if cnstLoadCad2D then
    Result := cnstExtrusion
  else
    Result := FExtrusion;
end;

function TdxfEntity.GetPoints(const AIndex: Integer;
  const ANeed3D: Boolean = False): TFPoint;
begin
  Result := FPoints[AIndex];
  if cnstLoadCad2D and (not ANeed3D) then
    Result.Z := 0;
end;

function TdxfEntity.GetZThick: Double;
begin
  if cnstLoadCad2D then
    Result := 0
  else
    Result := FZThick;
end;

procedure TdxfEntity.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
begin
  inherited InitEEDAppName(AData, AReader);
  if AReader.IsURLAppID(AReader.UValue) then
    FURLExtData := AData;
end;

procedure TdxfEntity.ApplyEED(AReader: TdxfReader);
begin
  inherited ApplyEED(AReader);
  if Assigned(FURLExtData) then
  begin
    if Assigned(FEnt) then
      FEnt.SetExtDataHyperLink(FURLExtData);
    EraseEED(FURLExtData);
  end;
end;

procedure TdxfEntity.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  FComplex := FComplex and (FEnt <> nil);
  if FEnt=nil then
    Exit;
  FLayerLinkIndex := SetLayerByName(AReader, FTexts[8]);
  FLTypeLinkIndex := SetLTypeByName(AReader, FTexts[6]);
  FEnt.LineTypeScale := FFloats[8];
  FEnt.Visibility := FInts[0] = 0;
  FEnt.ColorCAD := LoadColor(AReader);
  FComplex := FComplex or FEnt.Complex;
  SetLWeight(FLineWeight);
{$IFDEF SG_USEDXFSRC}
  if FEnt <> nil then
  begin
    FEnt.SrcStart := FSrcStart;
    FEnt.SrcEnd := FSrcEnd;
  end;
{$ENDIF}
end;

function TdxfEntity.LoadColor(const AReader: TdxfReader): TsgColorCAD;
begin
  if FInts[2] > 256 then
    Result :=  MakeColorCAD(acRGBColor, BGRToRGB(FInts[2]))
  else
    Result :=  MakeColorCAD(acIndexColor, FInts[2]);
  if FIntsRGBColor <> clNone then
    Result := MakeColorCAD(acRGBColor, FIntsRGBColor);
end;

procedure TdxfEntity.ReadProperty(const AReader: TdxfReader);
var
  vCode: Integer;
begin
  vCode := AReader.Code;
  case vCode of
    10..37:     Coord(AReader);
    38:         FElevation      := AReader.FloatValue;
    39:         FZThick         := AReader.FloatValue;
    40..59:     FFloats[vCode-40]   := AReader.FloatValue;
    60..79:     FInts[vCode-60]     := AReader.IntValue;
    210:        FExtrusion.X    := AReader.FloatValue;
    220:        FExtrusion.Y    := AReader.FloatValue;
    230:        FExtrusion.Z    := AReader.FloatValue;
    370:        FLineWeight     := AReader.IntValue;
    420:        FIntsRGBColor := BGRToRGB(AReader.IntValue);
    999:        AReader.ReadFileInfo;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfEntity.ReadEmbeddedObjectProperty(const AReader: TdxfReader);//skip unknown data
begin
end;

procedure TdxfEntity.SkipEmbeddedObject(const AReader: TdxfReader);
var
  vReadProperty: TdxfReadProperty;
begin
  vReadProperty := FReadProperty;
  try
    FReadProperty := ReadEmbeddedObjectProperty;
    ReadProps(AReader);
  finally
    FReadProperty := vReadProperty;
  end;
end;

{ TdxfBasePolyLine }

procedure TdxfBasePolyLine.Commit(const AReader: TdxfReader);
var
  vBasePolyLine: TsgCADBasePolyLine;
begin
  inherited Commit(AReader);
  vBasePolyLine := TsgCADBasePolyLine(FEnt);
  vBasePolyLine.Extrusion       := GetExtrusion;
  vBasePolyLine.ZThick          := GetZThick;
  vBasePolyLine.Flags           := Flags;
  vBasePolyLine.Closed          := vBasePolyLine.Flags and 1 <> 0;
end;

{ TdxfBlock }

{$IFDEF SG_BTI}
procedure TdxfBlock.Reinit(AReader: TdxfReader);
var
  vEntTypeNum, I, J: Integer;
  vGroupName: string;
  vBlockRecordHandle: UInt64;
  vItem, vNewItem: TsgTableItem;
  vEntity, vEnt: TsgDXFEntity;
begin
  if (AReader.XDataProcs <> nil) and Assigned(FExtData) then
  begin
    TsgDXFEntityAccess.ExtractEEDTypeEx(FExtData, vGroupName, vEntTypeNum);
    if vEntTypeNum <> cnstUndefined then
    begin
      vEntity := AReader.XDataProcs.Import(vEntTypeNum, nil);
      if vEntity <> nil then
      begin
        vBlockRecordHandle := TsgDXFBlock(FEnt).BlockRecord.Handle;
        vNewItem := nil;
        J := AReader.Conv.EntityDictionaries.IndexOf(vBlockRecordHandle);
        if J >= 0 then
        begin
          vItem := TsgTableItem(AReader.Conv.EntityDictionaries[J].Data);
          AReader.Conv.EntityDictionaries.Delete(J);
          vNewItem := TsgTableItem.Create;
          vNewItem.Item := vItem.Item; // add ref
          vItem.Item := nil; // release ref
          vItem.Free;
        end;

        AReader.Conv.Sections[csBlockRecords].RemoveEntity(TsgDXFBlock(FEnt).BlockRecord);
        if FEnt.Description = '' then
          FEnt.Description := FEnt.Name;
        FAdded := False;
        vEntity.AssignEntity(FEnt);
        vEntity.Handle := FEnt.Handle;
        TsgDXFBlock(vEntity).BlockRecord.Handle := vBlockRecordHandle;

        AReader.DoCreate(TsgDXFBlock(vEntity).BlockRecord);
        if AReader.FindObject(vBlockRecordHandle, I) then
          AReader.DataBase.List[I] := TsgDXFBlock(vEntity).BlockRecord
        else
          AReader.DataBase.InsertFinded(I, TsgDXFBlock(vEntity).BlockRecord);

        vEnt := ReleaseEntity;
        FEnt := vEntity;
        TsgDXFEntityAccess(FEnt).SetConverter(AReader.Conv);
        FEnt.SetExtData(FExtData);
        EraseEED(FExtData);
        if (FEnt.Name <> '') and (FEnt.Name[1] = cnstAsterisk) then
          FEnt.Flags := FEnt.Flags or 1;

        UpdateLinks(AReader);

        if Assigned(vNewItem) then
        begin
          AReader.Conv.EntityDictionaries.Add(vBlockRecordHandle, vNewItem);
          Pointer(vNewItem) := nil;
        end;

        vEnt.Free;
      end;
    end;
  end;
end;
{$ENDIF}
{$IFDEF SG_BTI}
function TdxfBlock.ConvertEEDHandle(ACode: SmallInt; AData: TsgCADExtendedData;
  AReader: TdxfReader): Integer;
var
  I: Integer;
  vDataCode: SmallInt;
begin
  Result := -1;
  if FEEDVersionInternal < cnstEEDInternalVersion then
    case ACode of
      String_1005:
        begin
          I := AData.DataCount - 1;
          if I > -1 then
          begin
            vDataCode := -1;
            if AData.DataType[I] = edtInt16 then
              vDataCode := AData.DataInt16[I]
            else
              if AData.DataType[I] = edtInt then
                vDataCode := AData.DataInt[I]
              else
                if AData.DataType[I] = edtString then
                  Result := AData.AddString(String_1000, AReader.UValue);
            case vDataCode of
              String_331, String_410, String_411, String_412, String_430:
                Result := AData.AddString(String_1000, AReader.UValue)
            end;
          end;
        end;
    end;
  if Result = -1 then
    Result := inherited ConvertEEDHandle(ACode, AData, AReader);
end;

function TdxfBlock.ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
  AReader: TdxfReader): Integer;
begin
  Result := -1;
  if FEEDVersionInternal < cnstEEDInternalVersion then
    case ACode of
      Integer_1070:
        begin
          if (AReader.IntValue > $7FFF) or (AReader.IntValue < SmallInt($8000)) then
            Result := AData.AddInt(ACode, AReader.IntValue);
        end;
    end; { case }
  if Result = -1 then
    Result := inherited ConvertEEDInt16(ACode, AData, AReader);
end;

procedure TdxfBlock.InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader);
var
  vAppIDName: string;
begin
  inherited InitEEDAppName(AData, AReader);
  vAppIDName := AReader.UValue;
  if AReader.IsRelatedAppID(vAppIDName) then
  begin
    FExtData := AData;
    FEEDVersionInternal := Ord(vAppIDName = GetAppID);
  end;
end;
{$ENDIF}

constructor TdxfBlock.Create;
begin
  inherited Create;
  FComplex := True;
end;

function TdxfBlock.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := AObj.ClassType = TdxfEndBlock;
end;

function TdxfBlock.IsAcadTableBlock: Boolean;
begin
  Result := Assigned(FEnt) and TsgDXFBlockAccess(FEnt).IsAcadTableBlock;
end;

procedure TdxfBlock.Commit(const AReader: TdxfReader);
var
  vBlock: TsgDXFBlock;
begin
  inherited Commit(AReader);
  vBlock := TsgDXFBlock(FEnt);
  vBlock.XrefPath    := Trim(FTexts[1]);
  vBlock.Name        := Name;
  vBlock.Offset      := GetPoints(0);
  TsgDXFBlockAccess(vBlock).SetFlags(Flags);
  if FTexts[4] <> '' then
  begin
    StringReplace(FTexts[4], '^M', #13);
    StringReplace(FTexts[4], '^J', #10);
    vBlock.Description := FTexts[4];
  end;
{$IFDEF SG_BTI}
  Reinit(AReader);
{$ENDIF}
  AReader.InitPaper(FEnt);
end;

procedure TdxfBlock.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
  vObj: TsgDXFBlockRecord;
begin
  case AReader.Code of
    Name_2:
      begin
        inherited ReadProperty(AReader);
        if not FAdded then
        begin
          vObj := TsgDXFBlockRecord(AReader.ItemByName[csBlockRecords, Name]);
          if Assigned(vObj) then
          begin
            ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
            FEnt := TsgDXFBlockRecordAccess(vObj).Block;
            FAdded := Assigned(FEnt.Owner);
          end;
        end;
      end;
    330:
      begin
        inherited ReadProperty(AReader);
        if not FAdded and (AReader.FRefsHash = 0) and (FSoftOwnerIDs.Last <> cnstBadHandle) then
        begin
          if AReader.FindObject(FSoftOwnerIDs.Last, I) then
          begin
            vObj := TsgDXFBlockRecord(AReader.DataBase[I]);
            Name := TsgDXFBlockRecord(vObj).Name;
            ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
            FEnt := TsgDXFBlockRecordAccess(vObj).Block;
            FAdded := Assigned(FEnt.Owner);
          end;
        end;
      end
    else
      inherited ReadProperty(AReader);
  end;
end;

{ TdxfDictionary }

function TdxfDictionary.OwnerAdd: Integer;
begin
  if FEnt.Owner = nil then
    Result := inherited OwnerAdd
  else
    Result := FEnt.Owner.IndexOfEntity(FEnt);
end;

function TdxfDictionary.AddItem(AReader: TdxfReader; const AHandle: UInt64;
  const AName: string): Integer;
begin
  Result := AReader.AppendLinkNames(AHandle, AName);
end;

procedure TdxfDictionary.Commit(const AReader: TdxfReader);
var
  I: Integer;
begin
  inherited Commit(AReader);
  if AReader.FNamedObjects = nil then
  begin
    AReader.FNamedObjects := TsgDXFDictionary(FEnt);
    for I := FNamesCount to AReader.FNamesCount - 1 do
      if AReader.FNames[I].Name = sAcadLayoutDictionary then
        AReader.FLayouts := AReader.FNames[I].ID
      else
        if AReader.FNames[I].Name = sAcadTableStyleDictionary then
          AReader.FTableStyles := AReader.FNames[I].ID
        else
          if AReader.FNames[I].Name = sAcadGroupDictionary then
            AReader.FGroups := AReader.FNames[I].ID
          else
            if AReader.FNames[I].Name = sAcadImageDict then
              AReader.FImageDefs := AReader.FNames[I].ID
            else
              if AReader.FNames[I].Name = sAcDbVariableDictionary then
                AReader.FDictionaryVariables := AReader.FNames[I].ID;
  end;
  if FEnt.Name = '' then
    FEnt.Name := DictionaryName;
end;

procedure TdxfDictionary.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
      3:
        begin
          inherited ReadProperty(AReader);
          FTexts[3] := AReader.GlobalNames[FTexts[3]];
        end;
    281: FEnt.Flags := AReader.IntValue;
    350: AddItem(AReader, AReader.HandleValue, FTexts[3]);
    360:
      begin
        if AReader.FRefsHash = 0 then
          AddItem(AReader, AReader.HandleValue, FTexts[3])
        else
          inherited ReadProperty(AReader);
      end;
    330:
      begin
        inherited ReadProperty(AReader);
        if AReader.FRefsHash = 0 then
        begin
          if (AReader.FLayouts <> cnstBadHandle) and (AReader.FLayouts = AReader.HandleValue) then
            FSoftOwner := AReader.Conv.Sections[csObjects];
        end;
      end;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfDictionary.ReadState(AReader: TdxfReader);
begin
  FNamesCount := AReader.FNamesCount;
  inherited ReadState(AReader);
end;

procedure TdxfDictionary.UpdateOwner(AReader: TdxfReader);
begin
  inherited UpdateOwner(AReader);
  if Assigned(FSoftOwner) then
  begin
    if (AReader.Conv.Dictionary[FSoftOwner.Handle] = FEnt) then
      FSoftOwner := AReader.Conv.Sections[csObjects];
  end;
  if (FHandle <> cnstBadHandle) and ((AReader.FLayouts = FHandle) or (AReader.FGroups = FHandle) or
     (AReader.FImageDefs = FHandle) or (AReader.FDictionaryVariables = FHandle) or (AReader.FTableStyles = FHandle)) then
    FSoftOwner := AReader.FNamedObjects;
end;

{ TdxfCustomDictionaryItem }

procedure TdxfCustomDictionaryItem.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
end;

function TdxfCustomDictionaryItem.OwnerAdd: Integer;
begin
  if FSoftOwner <> nil then
  begin
    Result := FSoftOwner.AddEntity(FEnt);
    if Result < 0 then
      Result := Conv.Sections[csObjects].AddEntity(FEnt);
  end
  else
    Result := inherited OwnerAdd;
end;

procedure TdxfCustomDictionaryItem.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
begin
  case AReader.Code of
    5:
      if FHandle = cnstBadHandle then
      begin
        inherited ReadProperty(AReader);
        ReplaceFromDatabase(AReader, I, FSoftOwner);
      end;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfCustomDictionaryItem.ReadProps(AReader: TdxfReader);
begin
  repeat
    AReader.Next;
    FReadProperty(AReader);
  until AReader.Code = Info_0;
end;

function TdxfCustomDictionaryItem.ReplaceFromDatabase(AReader: TdxfReader;
  var Index: Integer; out ASoftOwner: TsgDXFEntity): Integer;
begin
  Result := inherited ReplaceFromDatabase(AReader, Index, ASoftOwner);
  if (Result = 0) and not Assigned(FEnt) then
  begin
    FEnt := TsgDXFDictionary.Create;
    FEnt.Handle := FHandle;
    AReader.DoCreate(FEnt);
    AReader.DataBase.InsertFinded(Index, FEnt);
    FDatabaseAdded := True;
    Result := 3;
  end;
end;

procedure TdxfCustomDictionaryItem.UpdateOwner(AReader: TdxfReader);
var
  I: Integer;
  vOwnerHandle: UInt64;
begin
  if not Assigned(FSoftOwner) and (FSoftOwnerIDs.Count > 0) then
  begin
    vOwnerHandle := FSoftOwnerIDs.First;
    if vOwnerHandle <> cnstBadHandle then
    begin
      if AReader.FindObject(vOwnerHandle, I) then
        FSoftOwner := TsgDXFEntity(AReader.DataBase[I])
      else
      begin
        FSoftOwner := TsgDXFDictionary.Create;
        FSoftOwner.Handle := vOwnerHandle;
        AReader.DoCreate(FSoftOwner);
        AReader.DataBase.InsertFinded(I, FSoftOwner);
        AReader.Conv.Sections[csObjects].AddEntity(FSoftOwner);
      end;
    end;
  end;
  if not (FSoftOwner is TsgDXFDictionary) then
    FSoftOwner := AReader.Conv.Sections[csObjects];
  if Assigned(FEnt) and (FEnt.Name = '') then
    if Name <> '' then
      FEnt.Name := Name
    else
      FEnt.Name := FMarker;
end;

{ TdxfDimAssoc }

procedure TdxfDimAssoc.Commit(const AReader: TdxfReader);
var
  vEntity, vAssocEntity: TsgDXFEntity;
  vHDim, vHAssoc: UInt64;
begin
  inherited Commit(AReader);
  if (FSoftOwnerIDs.Count > 0) and (FPointerIDs.Count > 0) then
  begin
    vHDim := FSoftOwnerIDs.Last;
    vHAssoc := FPointerIDs.Last;
    if (vHDim <> cnstBadHandle) and (vHAssoc <> cnstBadHandle) then
    begin
      vEntity := AReader.ObjByHandle(vHDim);
      vAssocEntity := AReader.ObjByHandle(vHAssoc);
      if vEntity is TsgDXFDimension then
        TsgDXFDimension(vEntity).Associated := vAssocEntity;
    end;
  end;
end;

// TdxfClass

procedure TdxfClass.Commit(const AReader: TdxfReader);
var
  vEnt: TsgDXFEntity;
begin
  vEnt := ReleaseEntity;
  inherited Commit(AReader);
  FEnt := vEnt;
  FEnt.Name := FTexts[1];
  TsgDXFClass(FEnt).CppClassName := FTexts[2];
  FEnt.Description := FTexts[3];
end;

procedure TdxfClass.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    281: TsgDXFClass(FEnt).IsEntity := AReader.IntValue = 1;
    90:  FEnt.Flags := AReader.IntValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfHatch }

{private}

function TdxfHatch.AddNewPattern(const AHatch: TsgCADHatch):PsgHatchPatternData;
begin
  New(Result);
  AHatch.HatchPatternData.Add(Result);
  Result.Lines := nil;
end;

procedure TdxfHatch.DoSolidHatch(const AReader: TdxfReader);
var
  vEnt: TsgDXFEntity;
begin
  vEnt := ReleaseEntity;
  FEnt := TsgCADCurvePolygon.Create;
  FEnt.Clone(vEnt);
  UpdateLinks(AReader);
  vEnt.Handle := 0;
  vEnt.Free;
  AReader.DoCreate(FEnt);
end;

function TdxfHatch.GetLastHatchSegment(const AHatch: TsgCADHatch): Tsg2DCurve;
var
  vBoundary: Tsg2DBoundaryList;
begin
  Result := nil;
  if AHatch.BoundaryDataCount > 0 then
  begin
    vBoundary := AHatch.BoundaryDataLast;
    if vBoundary.Count > 0 then
      Result := vBoundary[vBoundary.Count - 1];
  end;
end;

function TdxfHatch.Pattern(const AHatch: TsgCADHatch): PsgHatchPatternData;
begin
  Result := PsgHatchPatternData(AHatch.HatchPatternData.Last);
end;

procedure TdxfHatch.ReadGrad(const AReader: TdxfReader; const  AHatch: TsgCADHatch);
begin
  case AReader.Code of
    421:
      begin
        AHatch.GradientColorCAD[FCurentIndexColorCAD] :=
          MakeColorCAD(acRGBColor, BGRToRGB(AReader.IntValue));
        Inc(FCurentIndexColorCAD);
      end;
    Integer_63:
      begin
        AHatch.GradientColorCAD[FCurentIndexColorCAD+FCurentIndexColor] :=
          MakeColorCAD(acIndexColor, AReader.IntValue);
        Inc(FCurentIndexColor);
      end;
  end;
end;

{protected}

procedure TdxfHatch.Commit(const AReader: TdxfReader);
//var
  //vColor: TColor;
begin
  //vColor := FEnt.Color;
  inherited Commit(AReader);
  //if vHatch.GradientName <> '' then
  // FEnt.Color := vColor;
  TsgCADHatch(FEnt).Extrusion := GetExtrusion;
  TsgCADHatch(FEnt).Offset := GetPoints(1);
  TsgCADHatch(FEnt).HatchName := FTexts[2];
  if TsgCADHatch(FEnt).EntType = ceMPolygon then
    TsgCADHatch(FEnt).SolidFill := FInts[11] and 1 <> 0
  else
    if Flags and 1 <> 0 then
      DoSolidHatch(AReader);
  if FTransparency <> 0 then
  begin
    case (FTransparency shr 24) and $FF of
      1: TsgCADCurvePolygon(FEnt).Transparency := fTransparencyByBlock;
      2: TsgCADCurvePolygon(FEnt).Transparency := (100 * (255 - FTransparency and $FF)) div 255;
    end;
  end;
  if AReader.Conv.ApplyCADHatchSettings(TsgCADHatch(FEnt)) then
    DoSolidHatch(AReader);
end;

constructor TdxfHatch.Create;
begin
  inherited Create;
  FCurentIndexColor := 0;
  FCurentIndexColorCAD := 0;
  FNumberOfFitData := cnstNumberOfFitDataUndefined;
end;

procedure TdxfHatch.ReadProperty(const AReader: TdxfReader);
var
  vHatch: TsgCADHatch;
  v2DCurve: Tsg2DCurve;

  function IsReadCurve: Boolean;
  begin
    Result := (v2DCurve <> nil) and (vHatch.EntType <> ceMPolygon);
  end;

begin
  vHatch := TsgCADHatch(FEnt);
  v2DCurve := GetLastHatchSegment(vHatch);
  inherited ReadProperty(AReader);
  case AReader.Code of
    421:  ReadGrad(AReader, vHatch);
    440:  FTransparency := AReader.IntValue;
    450:  FIsSolid := AReader.IntValue = 0;
    460:  vHatch.GradientAngle := RadToDeg(AReader.FloatValue);
    461:  vHatch.GradientUseCenter := Abs(AReader.FloatValue) < fAccuracy;
    470:
      if FIsSolid then
        vHatch.GradientName := ''
      else
        vHatch.GradientName := AReader.UValue;
    Integer_91:  FBoundNum := AReader.IntValue;
    Integer_92:
      begin
        FBoundType := AReader.IntValue;
        FCurType := nil;
      end;
    Integer_93:  FPointCount := AReader.IntValue;
    Integer_94, Integer_63:
      begin
        if (AReader.Code = Integer_94) and IsReadCurve and (v2DCurve is Tsg2DSpline) then
          Tsg2DSpline(v2DCurve).Degree := AReader.IntValue
        else
        begin
          case vHatch.EntType of
            ceHatch, ceGradientPolygon: ReadGrad(AReader, vHatch);
            ceMPolygon:
              begin
                if not FFillColorReaded then
                begin
                  FFillColorReaded := True;
                  vHatch.FillColor := MakeColorCAD(acIndexColor,AReader.IntValue);
                end
                else
                  ReadGrad(AReader, vHatch);
              end;
          end;
        end;
      end;
    Integer_97:
      begin
        if FNumberOfFitData = cnstNumberOfFitDataNeedRead then
          FNumberOfFitData := AReader.IntValue
        else
          FCurType := nil;
      end;
    Integer_72:
      begin
        //if boundary is polyline read has bulge flag
        //else read edge type
        if FCurType = nil then
          FCurType := vHatch.AddBoundaryList(FBoundType);
        if vHatch.EntType <> ceMPolygon then
        begin
          FNumberOfFitData := cnstNumberOfFitDataUndefined;
          case AReader.IntValue of
            cnst2DCurvePoly:  v2DCurve := Tsg2DCurve(Tsg2DPolyline.Create);
            cnst2DCurveLine:
              begin
                if FBoundType and 2 = 0 then
                  v2DCurve := Tsg2DCurve(Tsg2DLine.Create)
                else
                  v2DCurve := Tsg2DCurve(Tsg2DPolyline.Create);
              end;
            cnst2DCurveArc:  v2DCurve  := Tsg2DCurve(Tsg2DArc.Create);
            cnst2DCurveEllipse: v2DCurve := Tsg2DCurve(Tsg2DEllipse.Create);
            cnst2DCurveSpline:
              begin
                v2DCurve := Tsg2DCurve(Tsg2DSpline.Create);
                if AReader.Version >= Byte(acR2010) then
                  FNumberOfFitData := cnstNumberOfFitDataNeedRead;
              end;
          else
            v2DCurve := nil;
          end;
        end
        else
        begin
          v2DCurve := Tsg2DCurve(Tsg2DPolyline.Create);
          Tsg2DPolyline(v2DCurve).Closed := True;
        end;
        if v2DCurve <> nil then
          vHatch.BoundaryDataLast.Add(v2DCurve);
      end;
    Integer_73:
      begin
        if IsReadCurve then
        begin
          case v2DCurve.EdgeType of
            cnst2DCurvePoly:
              Tsg2DPolyline(v2DCurve).Closed := FInts[13] = 1;
            cnst2DCurveArc, cnst2DCurveEllipse:
              Tsg2DArc(v2DCurve).CounterClockWise := FInts[13] = 1;
            cnst2DCurveSpline:
              Tsg2DSpline(v2DCurve).Rational := FInts[13] = 1;
          end;
        end;
      end;
    Integer_74:
      begin
        if IsReadCurve then
        begin
          case v2DCurve.EdgeType of
            cnst2DCurveSpline:
              Tsg2DSpline(v2DCurve).Periodic := FInts[14] = 1;
          end;
        end;
      end;
    XOther_11:
      begin
        if vHatch.EntType <> ceMPolygon then
          case v2DCurve.EdgeType of
            cnst2DCurveLine:
               Tsg2DLine(v2DCurve).SetEndPoint(MakeF2DPoint(GetPoints(1).X,
                  Tsg2DLine(v2DCurve).EndPoint.Y));
            cnst2DCurveEllipse:
              Tsg2DEllipse(v2DCurve).MajorPoint := MakeF2DPoint(GetPoints(1).X,
                  Tsg2DEllipse(v2DCurve).MajorPoint.Y);
          end;
      end;
    YOther_21:
      begin
        if vHatch.EntType <> ceMPolygon then
          case v2DCurve.EdgeType of
            cnst2DCurveLine:
              Tsg2DLine(v2DCurve).SetEndPoint(MakeF2DPoint(
                  Tsg2DLine(v2DCurve).EndPoint.X, GetPoints(1).Y));
            cnst2DCurveEllipse:
              Tsg2DEllipse(v2DCurve).MajorPoint := MakeF2DPoint(
                  Tsg2DEllipse(v2DCurve).MajorPoint.X, GetPoints(1).Y);
          end;
      end;
    Float_40:
      case v2DCurve.EdgeType of
        cnst2DCurveArc, cnst2DCurveEllipse:
          Tsg2DArc(v2DCurve).Radius := FFloats[0];
        cnst2DCurveSpline:
          Tsg2DSpline(v2DCurve).AddKnot(FFloats[0]);
      end;
    Float_41:  TsgCADHatchAccess(vHatch).PatternData.Scale := FFloats[1];
    Float_42:
      case v2DCurve.EdgeType of
        cnst2DCurvePoly:
          Tsg2DPolyline(v2DCurve).AddBulge(FFloats[2]);
        cnst2DCurveSpline:
          Tsg2DSpline(v2DCurve).AddWeight(FFloats[2]);
      end;
    Angle_50:           Tsg2DArc(v2DCurve).StartParam := FFloats[10];
    Angle_51:           Tsg2DArc(v2DCurve).EndParam := FFloats[11];
    Angle_52:           TsgCADHatchAccess(vHatch).PatternData.Angle := FFloats[12];
    Angle_53:           AddNewPattern(vHatch).LineAngle := FFloats[13];
    Float_43:           Pattern(vHatch).BaseP.X := FFloats[3];
    Float_44:           Pattern(vHatch).BaseP.Y := FFloats[4];
    Float_45:           Pattern(vHatch).Offset.X := FFloats[5];
    Float_46:           Pattern(vHatch).Offset.Y := FFloats[6];
    FloatRepeated_49:   Pattern(vHatch).Lines.Add(FFloats[9]);
    Integer_77:         vHatch.DoubleHatch := FInts[17] = 1;
    Integer_79:
      begin
        Pattern(vHatch).DashNum := FInts[19];
        Pattern(vHatch).IsDash := FInts[19] > 0;
        if FInts[19] > 0 then
          Pattern(vHatch).Lines := TsgDoubleList.Create(0, FInts[19]);
      end;
    XFirst_10, YFirst_20, ZFirst_30:
      begin
        if FBoundNum = 0 then
          vHatch.Elevation := GetPoints(0)
        else
          if FCurType = nil then
            Exit
          else
            case AReader.Code of
              XFirst_10:
                  case v2DCurve.EdgeType of
                    cnst2DCurvePoly:
                      Tsg2DPolyline(v2DCurve).AddVertex(MakeF2DPoint(GetPoints(0).X, 0));
                    cnst2DCurveLine:
                      Tsg2DLine(v2DCurve).SetStartPoint(MakeF2DPoint(GetPoints(0).X, 0));
                    cnst2DCurveArc, cnst2DCurveEllipse:
                      Tsg2DArc(v2DCurve).CenterPoint := MakeF2DPoint(GetPoints(0).X, 0);
                    cnst2DCurveSpline:
                      Tsg2DSpline(v2DCurve).AddControl(MakeF2DPoint(GetPoints(0).X, 0));
                  end;
              YFirst_20:
                  case v2DCurve.EdgeType of
                    cnst2DCurvePoly:
                      Tsg2DPolyline(v2DCurve).GetLastVertex^.Y := GetPoints(0).Y;
                    cnst2DCurveLine:
                      Tsg2DLine(v2DCurve).SetStartPoint(MakeF2DPoint(Tsg2DLine(v2DCurve).StartPoint.X,
                             GetPoints(0).Y));
                    cnst2DCurveArc, cnst2DCurveEllipse:
                       Tsg2DArc(v2DCurve).CenterPoint :=
                         MakeF2DPoint(Tsg2DArc(v2DCurve).CenterPoint.X,
                             GetPoints(0).Y);
                    cnst2DCurveSpline:
                      Tsg2DSpline(v2DCurve).GetLastVertex^.Y := GetPoints(0).Y;
                  end;
            end;
      end;
  end;
end;

{ TdxfImageDef }

procedure TdxfImageDef.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
end;

procedure TdxfImageDef.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    Text_1: TsgDXFImageDef(FEnt).FileName := AReader.UValue;
  end;
end;

procedure TdxfImageDef.UpdateOwner(AReader: TdxfReader);
var
  vHandle: UInt64;
begin
  inherited UpdateOwner(AReader);
  if (not Assigned(FSoftOwner) or (FSoftOwner = AReader.Conv.Sections[csObjects])) and Assigned(AReader.FNamedObjects) then
  begin
    if AReader.FImageDefs <> cnstBadHandle then
      FSoftOwner := AReader.FNamedObjects.FindEntByHandle(AReader.FImageDefs);
    if not Assigned(FSoftOwner) then
      FSoftOwner := AReader.FNamedObjects.FindEntByName(sAcadImageDict);
    // if Named Objects dictionary has no ACAD_IMAGE_DICT
    if not Assigned(FSoftOwner) then
    begin
      vHandle := cnstBadHandle;
      Conv.CheckHandle(vHandle);
      FSoftOwner := TsgDXFDictionary.Create;
      FSoftOwner.Handle := vHandle;
      FSoftOwner.Name := sAcadImageDict;
      AReader.DataBase.AddItem(FSoftOwner);
      AReader.FNamedObjects.AddEntity(FSoftOwner);
    end;
  end;
end;

{ TdxfInsert }

{private}

function TdxfInsert.ChangeEntEx(AReader: TdxfReader): string;
var
  I: Integer;
  vEntity, vEnt: TsgDXFEntity;
  vEntTypeNum: Integer;
  vBlockRecord: TsgDXFBlockRecord;
begin
  Result := '';
  I := FExtData.IndexOfCode(Integer_1070);
  if I >= 0 then
  begin
    if FExtData.DataType[I] = edtInt16 then
      vEntTypeNum := FExtData.DataInt16[I]
    else
      vEntTypeNum := FExtData.DataInt[I];
    I := FExtData.IndexOfCode(String_1000);
    if I >= 0 then
      Result := FExtData.DataString[I];
    vEntity := DXFConv.CreateRegEntity(Result, vEntTypeNum, AReader.Conv, FExtData);
    if vEntity <> nil then
    begin
      vBlockRecord := TsgDXFInsert(vEntity).BlockRecord;
      if Assigned(vBlockRecord) and Assigned(vBlockRecord.Owner) then
        vBlockRecord.Owner.RemoveEntity(vBlockRecord);
      vBlockRecord.Free;
      TsgDXFInsert(vEntity).Clone(FEnt);
      TsgDXFInsert(vEntity).BlockRecord := TsgDXFInsert(FEnt).BlockRecord;
      vEnt := ReleaseEntity;
      FEnt := vEntity;
      UpdateLinks(AReader);
      EraseEED(FExtData);
      vEnt.Free;
    end;
  end;
end;

procedure TdxfInsert.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
var
  vAppIDName: string;
begin
  inherited InitEEDAppName(AData, AReader);
  vAppIDName := AReader.UValue;
  if AReader.IsRelatedAppID(vAppIDName) then
  begin
    FExtData := AData;
    FEEDVersionInternal := Ord(vAppIDName = GetAppID);
  end;
end;

procedure TdxfInsert.UpdateLinks(AReader: TdxfReader);
begin
  inherited UpdateLinks(AReader);
  if FBlockRecordLinkIndex <> -1 then
    AReader.FLinks[FBlockRecordLinkIndex].obj := FEnt;
end;

{$IFDEF SG_BTI}
function TdxfInsert.ConvertEEDHandle(ACode: SmallInt; AData: TsgCADExtendedData;
  AReader: TdxfReader): Integer;
var
  I: Integer;
  vDataCode: SmallInt;
begin
  Result := -1;
  if FEEDVersionInternal < cnstEEDInternalVersion then
    case ACode of
      String_1005:
        begin
          I := AData.DataCount - 1;
          if I > -1 then
          begin
            if AData.DataType[I] = edtInt16 then
            begin
              vDataCode := AData.DataInt16[I];
              case vDataCode of
                String_100, // BlockPattern Name
                String_436, // Data.xStringArray
                String_437, // Data.xStringArray
                String_438, // BTIExtendedData
                String_430..String_434, //label texts
                470, //SVG cnstED_FontName
                471: //cnstED_StrokeDashArray;
                  Result := AData.AddString(String_1000, AReader.UValue);
              end;
            end;
          end;
        end;
    end;
  if Result = -1 then
    Result := inherited ConvertEEDHandle(ACode, AData, AReader);
end;

function TdxfInsert.ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
  AReader: TdxfReader): Integer;
begin
  Result := -1;
  if FEEDVersionInternal < cnstEEDInternalVersion then
    case ACode of
      Integer_1070:
        begin
          if (AReader.IntValue > $7FFF) or (AReader.IntValue < SmallInt($8000)) then
            Result := AData.AddInt(ACode, AReader.IntValue);
        end;
    end; { case }
  if Result = -1 then
    Result := inherited ConvertEEDInt16(ACode, AData, AReader);
end;

procedure TdxfInsert.ChangeEntity(AReader: TdxfReader);
var
  vEntTypeNum: Integer;
  vGroupName: string;
  vEnt: TsgDXFEntity;
  vEntity: TsgDXFInsertAccess;
  vBlockRecord: TsgDXFBlockRecord;
begin
  TsgDXFEntityAccess.ExtractEEDTypeEx(FExtData, vGroupName, vEntTypeNum);
  if vEntTypeNum <> cnstUndefined then
  begin
    vEntity := TsgDXFInsertAccess(AReader.XDataProcs.Import(vEntTypeNum, nil));
    if vEntity <> nil then
    begin
      vBlockRecord := vEntity.BlockRecord;
      vEntity.BlockRecord := nil;
      vEntity.Clone(FEnt);
      vEntity.BlockRecord := TsgDXFInsert(FEnt).BlockRecord;
      TsgDXFInsert(FEnt).BlockRecord := nil;
      vEntity.SetConverter(AReader.Conv);
      vEnt := ReleaseEntity;
      FEnt := vEntity;
      UpdateLinks(AReader);
      vEntity.SetExtData(FExtData);
      EraseEED(FExtData);
      vBlockRecord.Free;
      vEnt.Free;
    end;
  end;
end;
{$ENDIF}

{$IFDEF SG_BTI_FIX_ENT}
function TdxfInsert.GetBlockForBTIObjectBad(const AReader: TdxfReader): TsgDXFBlock;
const
  cnstNameOfBlockForBTIObjectBad = 'BadBtiObject';
begin
  Result := AReader.FBlockForBTIObjectBad;
  if not Assigned(Result) then
  begin
    Result := TsgDXFBlock.Create;
    AReader.FBlockForBTIObjectBad := Result;
    Result.Name := cnstNameOfBlockForBTIObjectBad;
    AReader.Conv.Loads(Result);
    AReader.Conv.Sections[csBlocks].AddEntity(Result);
  end;
end;
{$ENDIF}

{protected}

constructor TdxfInsert.Create;
begin
  inherited Create;
  FFloats[1] := 1.0;
  FFloats[2] := 1.0;
  FFloats[3] := 1.0;
  FBlockRecordLinkIndex := -1;
end;

procedure TdxfInsert.Commit(const AReader: TdxfReader);
var
  vBlockRecord: TsgDXFBlockRecord;
  vInsert: TsgDXFInsertAccess;
  vMInsert: TsgCADMInsert;
  vEntGroup: string;
{$IFDEF SG_BTI_FIX_ENT}
  vBlockName: string;
{$ENDIF}
begin
  vMInsert := nil;
  if (ClassType = TdxfInsert) and (((FInts[10] >= 1) and (FInts[11] > 1))
    or ((FInts[10] > 1) and (FInts[11] >= 1))) then
  begin
    vMInsert := TsgCADMInsert.Create;
    vMInsert.Clone(FEnt);
    ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    FEnt := vMInsert;
    AReader.DoCreate(FEnt);
  end;
  inherited Commit(AReader);
  vInsert := TsgDXFInsertAccess(FEnt);
  vInsert.Point       := GetPoints(0);
  vInsert.Extrusion   := GetExtrusion;
  vInsert.Scale       := MakeFPoint(FFloats[1], FFloats[2], FFloats[3]);
  vInsert.Angle       := FFloats[10];
  FBlockRecordLinkIndex := AReader.AddLink(vInsert, BlockRecordProp, Name, TsgDXFInsertAccess);
  vBlockRecord := TsgDXFBlockRecord(AReader.ItemByName[csBlockRecords, Name]);
  vInsert.BlockRecord := vBlockRecord;
  if FExtData <> nil then
  begin
    vEntGroup := ChangeEntEx(AReader);
{$IFDEF SG_BTI}
    if (AReader.XDataProcs <> nil) and (vEntGroup <> cnstGroupTypeName[gtSVG]) then
    begin
//      if (FPXData <> nil) and (FAttributes <> '') then
//      begin
//        if FPXData^.Attribs = nil then
//          FPXData^.Attribs := TsgStringListNV.Create;
//        ApplyAttribsList(FPXData^.Attribs, FAttributes, FAttribsTypes);
//      end;
      ChangeEntity(AReader);
    end;
{$ENDIF}
  end
{$IFDEF SG_BTI_FIX_ENT}
  else
  begin
    vBlockName := Name;
    if AnsiPos('*U', vBlockName) = 1 then
      TsgDXFInsert(FEnt).Block := GetBlockForBTIObjectBad(AReader);
  end
{$ENDIF}
  ;
{$IFDEF SG_BTI}
  case FEnt.EntClass.EG of
    gtBTI:
      begin
        if TsgDXFEntityAccess(FEnt).GetEntTypeEx <> cnstLabel then
          if AReader.XDataProcs <> nil then
            AReader.XDataProcs.AddEntity(FEnt);
      end;
  end;
{$ENDIF}
  if Assigned(vMInsert) then
  begin
    vMInsert.NumCols := FInts[10];
    vMInsert.NumRows := FInts[11];
    vMInsert.ColSpacing := FFloats[4];
    vMInsert.RowSpacing := FFloats[5];
  end;
end;

{ TdxfLayout }

constructor TdxfLayout.Create;
begin
  inherited Create;
  FPlotSettings := nil;
end;

destructor TdxfLayout.Destroy;
begin
  FPlotSettings.FEnt.Free;
  FPlotSettings.Free;
  inherited Destroy;
end;

procedure TdxfLayout.Commit(const AReader: TdxfReader);
var
  I: Integer;
  vLayout: TsgDXFLayout;
  vBlockRecord: TsgDXFBlockRecordAccess;
  vHandleVPort: UInt64;
  vVPort: TsgDXFVport;
begin
  vLayout := TsgDXFLayout(FEnt);
  vLayout.PlotSettings.AssignEntity(FPlotSettings.FEnt);
  inherited Commit(AReader);
  vLayout.UCSOrigin := FPoints[3];
  vLayout.UCSXDir := FPoints[6];
  vLayout.UCSYDir := FPoints[7];

  vBlockRecord := nil;
  if (FBlockRecord <> cnstBadHandle) and AReader.FindObject(FBlockRecord, I) then
    if TObject(AReader.DataBase[I]) is TsgDXFBlockRecord then
      vBlockRecord := TsgDXFBlockRecordAccess(AReader.DataBase[I]);
  if Assigned(vBlockRecord) then
  begin
    vLayout.PaperSpaceBlock := vBlockRecord.Block;
    AReader.RemoveLayoutPerBlockRecord(vLayout);
  end;
  if Assigned(vBlockRecord) and (AReader.FPapers.Entities[0] = vBlockRecord.Block) then
  begin
    if Assigned(FPointerIDs) and (FPointerIDs.Count > 0) then
    begin
      vHandleVPort := FPointerIDs.Last;
      vVPort := TsgDXFVport(AReader.Conv.Sections[csVPorts].FindEntByHandle(vHandleVPort));
      if Assigned(vVPort) then
        AReader.FActiveVPort := vVPort;
    end;
  end;
end;

procedure TdxfLayout.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Text_1:     FEnt.Name := AReader.UValue;
    10..37: AReader.Coord(@FPoints);
    String_100:
      begin
        if (AReader.UValue = sAcDbPlotSettings) and (FPlotSettings = nil) then
        begin
          FPlotSettings := TdxfPlotSettings(CreateSubEntity(AReader, hshObjectsPLOTSETTINGS));
          FPlotSettings.ReadState(AReader);
          if AReader.Code = String_100 then
            inherited ReadProperty(AReader);
        end;
      end;
    330:
      if FMarker = cntCPlusPlusClassLayout then
        FBlockRecord := AReader.HandleValue
      else
        inherited ReadProperty(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfLayout.UpdateOwner(AReader: TdxfReader);
begin
  inherited UpdateOwner(AReader);
  if (not Assigned(FSoftOwner) or (FSoftOwner = AReader.Conv.Sections[csObjects])) and Assigned(AReader.FNamedObjects) then
  begin
    if AReader.FLayouts <> cnstBadHandle then
      FSoftOwner := AReader.FNamedObjects.FindEntByHandle(AReader.FLayouts);
    if not Assigned(FSoftOwner) then
      FSoftOwner := AReader.FNamedObjects.FindEntByName(sAcadLayoutDictionary);
  end;
  if not Assigned(FSoftOwner) and Assigned(AReader.FNamedObjects) then
  begin
    AReader.Conv.CheckHandle(AReader.FLayouts);
    FSoftOwner := TsgDXFDictionary.Create;
    FSoftOwner.Handle := AReader.FLayouts;
    FSoftOwner.Name := sAcadLayoutDictionary;
    AReader.DoCreate(FSoftOwner);
    AReader.DataBase.AddItem(FSoftOwner);
    AReader.FNamedObjects.AddEntity(FSoftOwner);
  end;
end;

{ TdxfMLine }

{private}

procedure TdxfMLine.NewVertex;
begin
  if FVertex <> nil then
  begin
    FVertex.Complete;
    FVertex.Point := GetPoints(1);
    FVertex.Direction := GetPoints(2);
    FVertex.Miter := GetPoints(3);
    TsgCADMLine(FEnt).AddEntity(FVertex);
  end;
  FVertex := TsgMVertex.Create;
end;

{protected}

procedure TdxfMLine.Commit(const AReader: TdxfReader);
var
  vMLine: TsgCADMLine;
begin
  inherited Commit(AReader);
  NewVertex;
  FVertex.Free;
  vMLine := TsgCADMLine(FEnt);
  vMLine.StartPoint := GetPoints(0);
  vMLine.Extrusion := GetExtrusion;
  vMLine.ScaleFactor := FFloats[0];
  vMLine.Justify := FInts[10];
  vMLine.Flags := FInts[11];
  vMLine.Style := TsgMLineStyle(AReader.ObjByHandle(FStyle, TsgMLineStyle));
  if vMLine.Style = nil then
    AReader.AddLink(vMLine, 'Style', Name, TsgCADMLineAccess);
end;

procedure TdxfMLine.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    XOther_11:  NewVertex;
  end;
  inherited ReadProperty(AReader);
  case AReader.Code of
    Float_41:   FVertex.Add(FFloats[1]);
    Integer_74: FVertex.NewList;
    340: FStyle := AReader.HandleValue;
  end;
end;

{ TdxfMLStyle }

function TdxfMLStyle.CurrEntry: TsgMLineEntry;
begin
  if TsgMLineStyle(FEnt).Count = 0 then
    NewEntry;
  Result := TsgMLineStyle(FEnt).Entries[TsgMLineStyle(FEnt).Count - 1];
end;

function TdxfMLStyle.NewEntry: TsgMLineEntry;
begin
  TsgMLineStyle(FEnt).NewEntry(0, cnstColorCADByLayer, nil);
  Result := TsgMLineStyle(FEnt).Entries[TsgMLineStyle(FEnt).Count - 1];
end;

procedure TdxfMLStyle.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
end;

procedure TdxfMLStyle.ReadEntry(const AReader: TdxfReader);
begin
  case AReader.Code of
    FloatRepeated_49: NewEntry.Offset := AReader.FloatValue;
    62:  CurrEntry.ColorCAD := MakeColorCAD(acIndexColor, AReader.IntValue);
    420: CurrEntry.ColorCAD := MakeColorCAD(acRGBColor, BGRToRGB(AReader.IntValue));
    6:   AReader.AddLink(CurrEntry, 'LineType', Trim(AReader.UValue), TsgDXFEntityAccess);
  else
    ReadProperty(AReader);
  end;
end;

procedure TdxfMLStyle.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Name_2:          FEnt.Name := AReader.UValue;
    Integer_70: TsgMLineStyle(FEnt).Flags := AReader.IntValue;
    62:         TsgMLineStyle(FEnt).ColorCAD := MakeColorCAD(acIndexColor, AReader.IntValue);
    420:        TsgMLineStyle(FEnt).ColorCAD := MakeColorCAD(acRGBColor, BGRToRGB(AReader.IntValue));
    Integer_71:
      begin
        FReadProperty := ReadEntry;
        try
          ReadProps(AReader);
        finally
          FReadProperty := ReadProperty;
        end;
      end;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfMText }

constructor TdxfMText.Create;
begin
  inherited Create;
  FPoints[1] := cnstFPointIllegal;
  FFloats[4] := 1;
  FTexts[7] := sStandardName;
end;

procedure TdxfMText.ApplyEED(AReader: TdxfReader);
begin
  inherited ApplyEED(AReader);
  if Assigned(FACADExtData) then
  begin
    FEnt.SetExtData(FACADExtData);
    EraseEED(FACADExtData);
  end;
end;

procedure TdxfMText.Commit(const AReader: TdxfReader);
var
  vStyle: TsgDXFStyle;
  vMText: TsgDXFMText;
begin
  inherited Commit(AReader);
  vMText := TsgDXFMText(FEnt);
  vMText.Angle       := FFloats[10];//take into account the order Angle & Point1
  vMText.Text        := FTexts[3] + FTexts[1];
  vStyle := TsgDXFStyle(AReader.ItemByName[csStyles, Trim(FTexts[7])]);
  if Assigned(vStyle) then
    vMText.Style := vStyle
  else
    AReader.AddLink(vMText, 'Style', Trim(FTexts[7]), TsgDXFMTextAccess);
  vMText.Point       := GetPoints(0);
  vMText.Point1      := GetPoints(1);
  vMText.Extrusion   := GetExtrusion;
  vMText.Height      := FFloats[0];
  vMText.RectWidth   := FFloats[1];
  vMText.Align       := FInts[11];
  if ((FOwner.ClassType = TdxfBlock) or (FOwner.ClassType = TdxfBlockR9)) and
     TdxfBlock(FOwner).IsAcadTableBlock then
    TsgDXFMText(FEnt).Hyphenation := True;
end;

procedure TdxfMText.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Text_1:     FTexts[1] := AReader.StrValue;
    Text_3:     FTexts[3] := FTexts[3] + AReader.StrValue;
    Float_44:   TsgDXFMText(FEnt).LineSpacingFactor := AReader.FloatValue;
    Integer_73: TsgDXFMText(FEnt).LineSpacingStyle := AReader.IntValue;
    Float_45:   TsgDXFMText(FEnt).BackgroundScaleFactor := AReader.FloatValue;
    Integer_63:
        TsgDXFMText(FEnt).BackgroundColor := MakeColorCAD(acIndexColor, AReader.IntValue);
    Integer_421:
        TsgDXFMText(FEnt).BackgroundColor := MakeColorCAD(acRGBColor, BGRToRGB(AReader.IntValue));
    Integer_441:
      TsgDXFMText(FEnt).BackgroundTransparency := AReader.IntValue;
    Integer_90:
      TsgDXFMText(FEnt).BackgroundFlags := AReader.IntValue;
    101: if AReader.IsValueEqual(cnstEmbeddedObject) then SkipEmbeddedObject(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfObject }

constructor TdxfObject.Create;
begin
  FHardOwnerIDs := TsgCollection.Create;
  TsgCollectionAccess(FHardOwnerIDs).Flags :=
    TsgCollectionAccess(FHardOwnerIDs).Flags and not cnstSortedBit;
  FPointerIDs := TsgInt64List.Create;
  FRctrsHardIDs := TsgInt64List.Create;
  FRctrsSoftIDs := TsgInt64List.Create;
  FSoftOwnerIDs := TsgInt64List.Create;
  FXDictionaryIDs := TsgInt64List.Create;
  FReadProperty := ReadProperty;

  FEEDItems := TsgStringList.Create;
  FEEDItems.Duplicates := dupIgnore;
  FEEDItems.CaseSensitive := False;
  FEEDItems.Sorted := True;

  FLayerLinkIndex := -1;
  FLTypeLinkIndex := -1;
end;

function TdxfObject.CreateSubEntity(const AReader: TdxfReader;
  ANameHash: Cardinal): TdxfObject;
var
  I: Integer;
  vMetaObj: TdxfReaderMetaclass;
  vClass: TdxfObjectClass;
begin
  if AReader.FClasses.FindItem(ANameHash, I) then
  begin
    TObject(vMetaObj) := AReader.FClasses[I];
    vClass := TdxfObjectClass(vMetaObj.ClassType);
    Result := vClass.Create;
    if Assigned(vMetaObj.EntClass) then
    begin
      Result.FEnt := vMetaObj.EntClass.Create;
      AReader.DoCreate(Result.FEnt);
    end;
  end
  else
  begin
    Result := TdxfEntity.Create;
    Result.ReleaseEntity;
  end;
  Result.FOwner := Self;
end;

destructor TdxfObject.Destroy;
begin
  FreeAndNil(FHardOwnerIDs);
  FreeAndNil(FPointerIDs);
  FreeAndNil(FRctrsHardIDs);
  FreeAndNil(FRctrsSoftIDs);
  FreeAndNil(FSoftOwnerIDs);
  FreeAndNil(FXDictionaryIDs);
  ClearStringsObjects(FEEDItems);
  FreeAndNil(FEEDItems);
  inherited Destroy;
end;

procedure TdxfObject.DestroyObj;
begin
  FreeAndNil(FObj);
end;

function TdxfObject.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := AObj.ClassType = TdxfSeqend;
end;

function TdxfObject.EraseEED(var AData: TsgCADExtendedData): Integer;
begin
  Result := -1;
  if Assigned(AData) then
  begin
    Result := FEEDItems.IndexOfObject(AData);
    if Result >= 0 then
    begin
      FEEDItems.Delete(Result);
      AData.Free;
      AData := nil;
    end;
  end;
end;

function TdxfObject.GetConv: TsgDXFConverterAccess;
begin
  if Assigned(FOwner) then
    Result := TsgDXFConverterAccess(FOwner.Conv)
  else
    Result := nil;
end;

function TdxfObject.GetName: string;
begin
  Result := Trim(FTexts[2]);
end;

procedure TdxfObject.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
var
  vAppIDName: string;
begin
  vAppIDName := AReader.UValue;
  AData.AddString(AReader.Code, vAppIDName);//1001
  if Assigned(FEnt) and IsAnnotative(vAppIDName) then
    FEnt.Annotative := True;
  if AReader.IsACADAppID(vAppIDName) then
    FACADExtData := AData;
end;

function TdxfObject.OwnerAdd: Integer;
begin
  Result := FOwner.FEnt.AddEntity(FEnt);
end;

procedure TdxfObject.Pop;
begin
  FReadProperty := FStackReadProperty[FStackReadPropertyCount - 1];
  Dec(FStackReadPropertyCount);
end;

function TdxfObject.Push(
  const AReadMethod: TdxfReadProperty): Integer;
begin
  Result := Length(FStackReadProperty);
  if Result = FStackReadPropertyCount then
    SetLength(FStackReadProperty, ListGrow(Length(FStackReadProperty)));
  Inc(FStackReadPropertyCount);
  FStackReadProperty[FStackReadPropertyCount - 1] := FReadProperty;
  FReadProperty := AReadMethod;
end;

function TdxfObject.AddEntity(const E: TdxfObject): Boolean;
begin
  Result := False;
  if Assigned(FEnt) then
  begin
    if E.FAdded then
      Result := True
    else
      Result := FEnt.AddEntity(E.FEnt) >= 0;
  end;
end;

procedure TdxfObject.ApplyEED(AReader: TdxfReader);
begin
end;

procedure TdxfObject.ApplyHandle(const AReader: TdxfReader);
var
  vClass: TClass;
begin
  if Assigned(FEnt) then
  begin
    AReader.FCheckHandle(FHandle);
    FEnt.Handle := FHandle;
    vClass := FEnt.ClassType;
    if vClass = TsgDXFBlockRecord then
      TsgDXFBlockRecordAccess(FEnt).AddDestroyNotification(AReader.BlockRecordDestroy)
    else
    if vClass = TsgDXFLayout then
      TsgDXFEntityAccess(FEnt).AddDestroyNotification(AReader.EntityDestroy);
  end
  else
    if AReader.Conv.MaxHandle < FHandle then
      AReader.Conv.MaxHandle := FHandle;
end;

function TdxfObject.ApplyXDictionary(AReader: TdxfReader): TsgDXFDictionary;

  function DoXDictionary(AXDictionary: UInt64): TsgDXFDictionary;
  var
    I: Integer;
    vItem: TsgTableItem;
  begin
    Result := TsgDXFDictionary(AReader.ObjByHandle(AXDictionary, TsgDXFDictionary));
    if Assigned(FEnt) then
    begin
//      I := AReader.Conv.EntityDictionaries.IndexOf(FHandle);
//      if I < 0 then
      begin
        vItem := TsgTableItem.Create;
        I := AReader.Conv.EntityDictionaries.Add(FHandle, nil);
        TObject(AReader.Conv.EntityDictionaries.List^[I].Data) := vItem;
        vItem.Item := Result;
      end;
    end;
  end;

begin
  Result := nil;
  if (FXDictionaryIDs.Count > 0) and (FXDictionaryIDs.First <> cnstBadHandle) then
    Result := DoXDictionary(FXDictionaryIDs.First);
end;

procedure TdxfObject.Commit(const AReader: TdxfReader);
begin
  ApplyHandle(AReader);
  ApplyEED(AReader);
end;

function TdxfObject.ConvertEEDHandle(ACode: SmallInt;
  AData: TsgCADExtendedData; AReader: TdxfReader): Integer;
begin
  Result := AData.AddInt64(ACode, AReader.HandleValue);
end;

function TdxfObject.ConvertEEDInt16(ACode: SmallInt; AData: TsgCADExtendedData;
  AReader: TdxfReader): Integer;
begin
  Result := AData.AddInt16(ACode, AReader.IntValue);
end;

procedure TdxfObject.DoCoord(AReader: TdxfReader; P: Pointer; ABaseCode: Integer);
begin
  PFPoint(P)^.V[(AReader.Code - ABaseCode) div 10 - 1] := AReader.FloatValue;
end;

procedure TdxfObject.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    1..4, 6..9:
      begin
        FTexts[AReader.Code] := AReader.UValue;
      end;
    5:
      begin
        FHandle := AReader.HandleValue;
      end;
    100: FMarker := AReader.GlobalNames[AReader.UValue];
    330:
      begin
        case AReader.FRefsHash of
          hshAcadXDictionary: FXDictionaryIDs.Add(AReader.HandleValue);
          hshAcadReactors: FRctrsSoftIDs.Add(AReader.HandleValue);
        else
          FSoftOwnerIDs.Add(AReader.HandleValue);
        end;
      end;
    331:
      begin
        case AReader.FRefsHash of
          hshAcadXDictionary: FXDictionaryIDs.Add(AReader.HandleValue);
          hshAcadReactors: FRctrsSoftIDs.Add(AReader.HandleValue);
        else
          FPointerIDs.Add(AReader.HandleValue);
        end;
      end;
    360:
      begin
        case AReader.FRefsHash of
          hshAcadXDictionary: FXDictionaryIDs.Add(AReader.HandleValue);
          hshAcadReactors: FRctrsHardIDs.Add(AReader.HandleValue);
        else
          FHardOwnerIDs.Add(AReader.HandleValue, nil);
        end;
      end;
    347:
      begin
        FMaterial := AReader.HandleValue;
      end;
    1001:
      begin
        AReader.ReadEED(Self);
      end;
  end;
end;

procedure TdxfObject.ReadState(AReader: TdxfReader);
begin
{$IFDEF SG_USEDXFSRC}
  FSrcStart := AReader.FPrev;
{$ENDIF}
  ReadProps(AReader);
{$IFDEF SG_USEDXFSRC}
  FSrcEnd := AReader.FPrev;
{$ENDIF}
  Commit(AReader);
  AReader.StoreEED(Self);
  AReader.UpdateDatabase(Self);
  ApplyXDictionary(AReader);
  AReader.UpdateMaterials(Self);
  UpdateOwner(AReader);
  AReader.DoSetLoading;
  if FComplex then
    ReadEntities(AReader);
end;

function TdxfObject.ReleaseEntity: TsgDXFEntity;
begin
  Result := FEnt;
  FEnt := nil;
end;

function TdxfObject.ReplaceFromDatabase(AReader: TdxfReader; var Index: Integer;
  out ASoftOwner: TsgDXFEntity): Integer;
var
  DatabaseItem: TsgDXFEntityAccess;
begin
  Result := 0;
  if AReader.FindObject(FHandle, Index) then
  begin
    DatabaseItem := TsgDXFEntityAccess(AReader.DataBase[Index]);
    ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    FEnt := DatabaseItem;
    Result := 1;
    FDatabaseAdded := True;
    FAdded := Assigned(FEnt.Owner);
  end;
end;

function TdxfObject.SetLayerByName(AReader: TdxfReader; const AName: string): Integer;
begin
  Result := AReader.AddLink(FEnt, LayerProp, AName, TsgDXFEntityAccess);
  if Assigned(AReader.FNoLayersTable) then
    AReader.FNoLayersTable.Add(AName);
end;

function TdxfObject.SetLTypeByName(AReader: TdxfReader;
  const AName: string): Integer;
begin
  Result := AReader.AddLink(FEnt, LTypeProp, AName, TsgDXFEntityAccess);
end;

procedure TdxfObject.SetName(const Value: string);
begin
  FTexts[2] := Value;
end;

procedure TdxfObject.UpdateLinks(AReader: TdxfReader);
begin
  if FLayerLinkIndex <> -1 then
    AReader.FLinks[FLayerLinkIndex].obj := FEnt;
  if FLTypeLinkIndex <> -1 then
    AReader.FLinks[FLTypeLinkIndex].obj := FEnt;
end;

procedure TdxfObject.UpdateOwner(AReader: TdxfReader);
begin
end;

procedure TdxfObject.ReadProps(AReader: TdxfReader);
begin
  repeat
    AReader.Next;
    case AReader.Code of
      EntitysBegin_66:  FComplex := AReader.IntValue <> 0;
      PaperSpace_67:    FPaperSpace := Ord(AReader.IntValue <> 0);
      else              FReadProperty(AReader);
    end;
  until AReader.Code = Info_0;
end;

procedure TdxfObject.ReadEntities(AReader: TdxfReader);
begin
  while not AReader.EOF and not ReadEntity(AReader, AReader.FNameHash) do;
end;

function TdxfObject.ReadEntity(AReader: TdxfReader; ANameHash: Cardinal): Boolean;
begin
  FObj := CreateSubEntity(AReader, ANameHash);
  try
    FObj.ReadState(AReader);
    Result := EOFEnt(FObj);
    if Assigned(FObj.FEnt) then
      if not AddEntity(FObj) then
        FObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
  finally
    DestroyObj;
  end;
end;

{ TdxfOLE }

procedure TdxfOLE.Commit(const AReader: TdxfReader);
const
  cnstOleDataSignature = $46433044; //'D0CF'
var
  S: AnsiString;
  vOLE2Frame: TsgDXFOle2Frame;
  vOleHeader: PsgOle2FrameHeaderData;
begin
  inherited Commit(AReader);
  vOLE2Frame := TsgDXFOle2Frame(FEnt);
  if Assigned(FData) then
  begin
    vOleHeader := PsgOle2FrameHeaderData(FData);
    if FInts[10] >= 2 then
    begin
      vOLE2Frame.Point              := GetPoints(0);
      vOLE2Frame.Point1             := GetPoints(1);
      vOLE2Frame.OLEObjectType      := FInts[11];
      vOLE2Frame.TileModeDescriptor := FInts[12];
      if Assigned(vOleHeader) then
        vOLE2Frame.Aspect := vOleHeader^.DrawAspect;
    end
    else
      if Assigned(vOleHeader) then
      begin
        vOLE2Frame.Point := vOleHeader^.Point1;
        vOLE2Frame.Point1 := vOleHeader^.Point3;
        vOLE2Frame.TileModeDescriptor := vOleHeader^.TileModeDescriptor xor 1;
        vOLE2Frame.Aspect := vOleHeader^.DrawAspect;
      end;
    SetString(S, PAnsiChar(TsgNativeInt(FData) + SizeOf(TsgOle2FrameHeaderData)), FDataSize - SizeOf(TsgOle2FrameHeaderData));
    vOLE2Frame.BinaryData := S;
  end;
end;

constructor TdxfOLE.Create;
begin
  inherited Create;
  FDataSize := 0;
  FData := nil;
end;

destructor TdxfOLE.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

procedure TdxfOLE.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    String_310: AReader.AddBinData(FData, FDataSize);
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfPenLine }

procedure TdxfPenLine.Commit(const AReader: TdxfReader);
var P: TsgDXFPenLine;
begin
  inherited Commit(AReader);
  P := TsgDXFPenLine(FEnt);
  P.Point       := GetPoints(0);
  P.Extrusion   := GetExtrusion;
  P.ZThick      := GetZThick;
end;

{ TdxfPenTableItem }

procedure TdxfPenTableItem.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if FEnt is TsgDXFLayer then
    TsgDXFConverterAccess.SetLayerNameOfPassingInspection(TsgDXFLayer(FEnt), Name)
  else
    TsgDXFPenTableItem(FEnt).Name := Name;
  TsgDXFPenTableItem(FEnt).Flags := Flags;
  if Assigned(FPenTableItemWithSameNameAlreadyExistsInOwnerTable) then
  begin
    if FPenTableItemWithSameNameAlreadyExistsInOwnerTable.Handle = FEnt.Handle then
      FEnt.Handle := cnstBadHandle // fix: do not add to database
    else
    begin
      FPenTableItemWithSameNameAlreadyExistsInOwnerTable := nil;
      FAdded := Assigned(FEnt.Owner);
    end;
  end;
end;

destructor TdxfPenTableItem.Destroy;
begin
  if Assigned(FPenTableItemWithSameNameAlreadyExistsInOwnerTable) then
    ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
  inherited Destroy;
end;

procedure TdxfPenTableItem.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
  vSoftOwner: TsgDXFEntity;
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    5: ReplaceFromDatabase(AReader, I, vSoftOwner);
    Name_2:
      begin
        if FOwner is TdxfTable then
        begin
          I := TdxfTable(FOwner).SortEntList.IndexOf(Name);
          if I >= 0 then
            FPenTableItemWithSameNameAlreadyExistsInOwnerTable :=
              TsgDXFPenTableItem(TdxfTable(FOwner).SortEntList.Objects[I])
        end
        else
          FPenTableItemWithSameNameAlreadyExistsInOwnerTable :=
            TsgDXFPenTableItem(FOwner.FEnt.FindEntByName(Name));
        FAdded := Assigned(FPenTableItemWithSameNameAlreadyExistsInOwnerTable);
      end;
  end;
end;

function TdxfPenTableItem.SetLayerByName(AReader: TdxfReader;
  const AName: string): Integer;
begin
  Result := -1;
end;

{ TdxfPoint }

procedure TdxfPoint.Commit(const AReader: TdxfReader);
var
  P: TsgDXFPoint;
begin
  inherited Commit(AReader);
  P := TsgDXFPoint(FEnt);
  P.Point     := GetPoints(0);
  P.Extrusion := GetExtrusion;
  P.ZThick    := GetZThick;
end;

{ TdxfProxy }

{private}

{protected}

destructor TdxfProxy.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

procedure TdxfProxy.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if FData=nil then
    Exit;
  AReader.ForceStandardObjects;
  AReader.ProxyReaderImpl.Reader.UpdatePosition(FData);
  AReader.ProxyReaderImpl.Read(FEnt);
  AReader.ProxyReaderImpl.Apply(FEnt);
end;

procedure TdxfProxy.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    String_310: AReader.AddBinData(Pointer(FData), FSize);
  else
    inherited ReadProperty(AReader);
  end;
end;

function TdxfProxy.SetLayerByName(AReader: TdxfReader;
  const AName: string): Integer;
var
  I: Integer;
begin
  Result := inherited SetLayerByName(AReader, AName);
  if Assigned(AReader.FSections[csLayers]) then
  begin
    I := AReader.FSections[csLayers].IndexOf(AName);
    if I >= 0 then
      FEnt.Layer := TsgDXFLayer(AReader.FSections[csLayers].Objects[I]);
  end;
end;

{ TdxfShape }

constructor TdxfShape.Create;
begin
  inherited Create;
  FPoints[1] := cnstFPointIllegal;
  FFloats[1] := 1.0;
end;

procedure TdxfShape.Commit(const AReader: TdxfReader);
var
  vShape: TsgDXFShape;
begin
  inherited Commit(AReader);
  vShape := TsgDXFShape(FEnt);
  vShape.ShapeName    := Name;
  vShape.Point        := GetPoints(0);
  vShape.Extrusion    := GetExtrusion;
  vShape.Height       := FFloats[0];// Size
  vShape.Scale        := FFloats[1];// Width factor
  vShape.Rotation     := FFloats[10];
  vShape.ObliqueAngle := FFloats[11];
end;

{ TdxfSortEntsTable }

procedure TdxfSortEntsTable.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if FEnt.Name = '' then
    FEnt.Name := sAcadSortEnts;
  FPointerIDs := nil; // release list
  TsgDXFSortEntsTable(FEnt).SortObject := AReader.ObjByHandle(FSoftOwnerIDs.Last, TsgDXFBlockRecord);
end;

procedure TdxfSortEntsTable.ReadProperty(const AReader: TdxfReader);
const
  cnstAcDbSortentsTable = 'AcDbSortentsTable';
begin
  case AReader.Code of
    String_100: FStartTable := AReader.UValue = cnstAcDbSortentsTable;
    Text_5:
      if FStartTable then
        TsgDXFSortEntsTable(FEnt).HandlesNew.Add(AReader.HandleValue)
      else
        inherited ReadProperty(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfSortEntsTable.ReadState(AReader: TdxfReader);
begin
  FPointerIDs.Free;
  FPointerIDs := TsgDXFSortEntsTable(FEnt).HandlesOld;
  inherited ReadState(AReader);
end;

{ TdxfTable }

constructor TdxfTable.Create;
begin
  inherited Create;
  FConvSection := TConvSection(Ord(Low(TConvSection)) - 1);
  FComplex := True;
  FSortEntList := TsgStringList.Create;
  TsgStringList(FSortEntList).Duplicates := dupIgnore;
  TsgStringList(FSortEntList).CaseSensitive := False;
  TsgStringList(FSortEntList).Sorted := True;
end;

destructor TdxfTable.Destroy;
begin
  FSortEntList.Free;
  inherited Destroy;
end;

function TdxfTable.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := AObj.ClassType = TdxfEndTab;
end;

function TdxfTable.AddEntity(const E: TdxfObject): Boolean;
begin
  Result := inherited AddEntity(E);
  if Result then
    FSortEntList.AddObject(E.Name, E.FEnt);
end;

procedure TdxfTable.Commit(const AReader: TdxfReader);
var
  I: Integer;
  vEnt: TsgDXFEntity;
begin
  inherited Commit(AReader);
  FEnt.Name := Name;
  I := DataBaseSections.IndexOf(Name);
  if I >= 0 then
  begin
    FConvSection := TConvSection(TsgObjectInt64(DataBaseSections.Objects[I]).FieldInt);
    case FConvSection of
      csBlockRecords:
        begin
          vEnt := ReleaseEntity;
          FEnt := TsgDXFBlockRecords.Create;
          FEnt.Handle := FHandle;
          FEnt.Name := Name;
          AReader.Conv.Sections[FConvSection] := TsgDXFGroup(FEnt);
          TsgDXFEntityAccess(FEnt).SetConverter(AReader.Conv);
          vEnt.Handle := 0;
          vEnt.Free;
        end;
      csStyles:
        begin
          AReader.Conv.Sections[FConvSection] := TsgDXFGroup(FEnt);
          TsgProxyReaderImplAcceess(AReader.ProxyReaderImpl).UpdateStyleByFont(FEnt);
        end;
      csAppID:
        begin
          AReader.Conv.Sections[FConvSection] := TsgDXFGroup(FEnt);
          AReader.FAppIDs := FEnt;
        end;
      csLayers:
        begin
          AReader.Conv.Sections[FConvSection] := TsgDXFGroup(FEnt);
          FreeAndNil(AReader.FNoLayersTable);
        end
    else
      AReader.Conv.Sections[FConvSection] := TsgDXFGroup(FEnt);
    end;
  end;
end;

{ TdxfEndTab }

procedure TdxfEndTab.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if (FOwner is TdxfTable) and (ShortInt(Ord(TdxfTable(FOwner).FConvSection)) >= Ord(Low(TConvSection))) then
  begin
    if Assigned(AReader.FSections[TdxfTable(FOwner).FConvSection]) then
    begin
      AReader.FSections[TdxfTable(FOwner).FConvSection].AddStrings(TdxfTable(FOwner).FSortEntList);
      TdxfTable(FOwner).FSortEntList.Free;
    end
    else
      AReader.FSections[TdxfTable(FOwner).FConvSection] := TdxfTable(FOwner).FSortEntList;
    TdxfTable(FOwner).FSortEntList := nil;
  end;
end;

{ TdxfText }

constructor TdxfText.Create;
begin
  inherited Create;
  FPoints[1] := cnstFPointIllegal;
  FFloats[1] := 1.0;
  FStyleLinkIndex := -1;
  FTexts[7] := sStandardName;
end;

destructor TdxfText.Destroy;
begin
  if Assigned(FMText) then
  begin
    FMText.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    FMText.Free;
  end;
  inherited Destroy;
end;

procedure TdxfText.Commit(const AReader: TdxfReader);
var
  vStyle: TsgDXFStyle;
  vText: TsgDXFTextAccess;
  vElevation: Double;
  vOffsetZ: TFPoint;
begin
  inherited Commit(AReader);
  vText := TsgDXFTextAccess(FEnt);
  vText.Text         := FTexts[1];
  vStyle := TsgDXFStyle(AReader.ItemByName[csStyles, Trim(FTexts[7])]);
  if Assigned(vStyle) then
    vText.Style := vStyle
  else
    FStyleLinkIndex := AReader.AddLink(vText, 'StyleRef', Trim(FTexts[7]), TsgDXFTextAccess);
  vText.Point        := GetPoints(0);
  vText.Point1       := GetPoints(1);
  vElevation := GetElevation;
  if vElevation <> 0 then
  begin
    vOffsetZ := MakeFPoint(0, 0, vElevation);
    vText.Point := AddFPoint(vText.Point, vOffsetZ);
    if TsgDXFTextAccess(vText).HasSecond then
      vText.Point1 := AddFPoint(vText.Point1, vOffsetZ);
  end;
  vText.Extrusion    := GetExtrusion;
  vText.Height       := FFloats[0];
  vText.Scale        := FFloats[1];
  vText.Rotation     := AsSingle(FFloats[10], 0);
  vText.ObliqueAngle := AsSingle(FFloats[11], 0);
  vText.Generation   := FInts[11];
  vText.HAlign       := FInts[12];
  vText.VAlign       := FInts[13];
  if Assigned(FMText) then
    vText.MTextRef := TsgDXFMText(FMText.ReleaseEntity);
end;

procedure TdxfText.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Text_1:
      begin
        FTexts[1] := AReader.StrValue;
        ReplaceAnsi(FTexts[1], cnstCaretAndSpace, cnstCaret);
      end;
    101: if AReader.IsValueEqual(cnstEmbeddedObject) then SkipEmbeddedObject(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfText.SkipEmbeddedObject(const AReader: TdxfReader);
begin
  FMText := TdxfMText.Create;
  FMText.FOwner := Self;
  FMText.FEnt := TsgDXFMText.Create;
  FMText.ReadProps(AReader);
  FMText.Commit(AReader);
end;

procedure TdxfText.UpdateLinks(AReader: TdxfReader);
begin
  inherited UpdateLinks(AReader);
  if FStyleLinkIndex <> - 1 then
    AReader.FLinks[FStyleLinkIndex].obj := FEnt;
end;

{ TdxfTolerance }

constructor TdxfTolerance.Create;
begin
  inherited Create;
  FPoints[1] := cnstFPointIllegal;
end;

procedure TdxfTolerance.ApplyACADEED(const AReader: TdxfReader);
var
  I: Integer;
  vIsDStyle, vEndOfData: Boolean;
  vCode: SmallInt;
  vTolerance: TsgDXFTolerance;

  function NextInt(var ACode: SmallInt; const AValue: Integer): Boolean;
  begin
    Result := True;
    case ACode of
      Integer_176: vTolerance.DimlineColor := MakeColorCAD(acIndexColor, AValue);
    else
      Result := False;
    end;
    ACode := 0;
  end;

  function NextReal(var ACode: SmallInt; AValue: Double): Boolean;
  begin
    Result := True;
    case ACode of
      Float_40:  vTolerance.DimScaleOverall := Abs(AValue);
      Float_140:  vTolerance.Height := Abs(AValue);
      Float_147:  vTolerance.GAP := AValue;
    else
      Result := False;
    end;
    ACode := 0;
  end;

  function NextInt64(var ACode: SmallInt; AValue: UInt64): Boolean;
  begin
    Result := True;
    case ACode of
      String_340: vTolerance.Style := TsgDXFStyle(AReader.ObjByHandle(AValue, TsgDXFStyle));
    else
      Result := False;
    end;
    ACode := 0;
  end;

begin
  if Assigned(FACADExtData) then
  begin
    I := 0;
    vIsDStyle := False;
    while not vIsDStyle and (I < FACADExtData.DataCount) do
    begin
      vIsDStyle := (FACADExtData.DataType[I] = edtString) and
        (FACADExtData.DataCode[I] = 1000) and (FACADExtData.DataString[I] = cnstDStyle);
      Inc(I);
    end;
    if vIsDStyle then
    begin
      vCode := 0;
      vTolerance := TsgDXFTolerance(FEnt);
      vEndOfData := False;
      while not vEndOfData and (I < FACADExtData.DataCount) do
      begin
        case FACADExtData.DataType[I] of
          edtInt16:
            if (FACADExtData.DataCode[I] = 1070) and (vCode = 0) then
              vCode := FACADExtData.DataInt16[I]
            else
              NextInt(vCode, FACADExtData.DataInt16[I]);
          edtDouble: NextReal(vCode, FACADExtData.DataDouble[I]);
          edtInt64: NextInt64(vCode, FACADExtData.DataInt64[I]);
          edtString: vEndOfData := FACADExtData.DataString[I] = cnstRigthBracket;
        else
          vCode := 0;
        end;
        Inc(I);
      end;
      EraseEED(FACADExtData);
    end;
  end;
end;

procedure TdxfTolerance.Commit(const AReader: TdxfReader);
var
  vTolerance: TsgDXFTolerance;
begin
  inherited Commit(AReader);
  vTolerance := TsgDXFTolerance(FEnt);
  vTolerance.Text        := FTexts[1];
  vTolerance.Point       := GetPoints(0);
  vTolerance.Point1      := GetPoints(1);
  vTolerance.Extrusion   := GetExtrusion;
  vTolerance.DimStyle := AReader.Conv.DimensionStyleByName(FTexts[3]);
  ApplyACADEED(AReader);
end;

{ TdxfVertex }

procedure TdxfVertex.Commit(const AReader: TdxfReader);
var
  vVertex: TsgDXFVertex;
begin
  inherited Commit(AReader);
  FComplex := False;
  vVertex := TsgDXFVertex(FEnt);
  vVertex.Point                       := GetPoints(0);
  vVertex.StartWidth                  := FFloats[0];
  vVertex.EndWidth                    := FFloats[1];
  vVertex.Bulge                       := FFloats[2];
  vVertex.Flags                       := Flags;
end;

procedure TdxfVertex.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    71:  TsgDXFVertex(FEnt).PolyFaceVertexIndex1 := FInts[11];
    72:  TsgDXFVertex(FEnt).PolyFaceVertexIndex2 := FInts[12];
    73:  TsgDXFVertex(FEnt).PolyFaceVertexIndex3 := FInts[13];
    74:  TsgDXFVertex(FEnt).PolyFaceVertexIndex4 := FInts[14];
  end;
end;

{ TdxfViewport }

{private}

procedure TdxfViewport.Next3Reals(const AReader: TdxfReader; APoint: TFPoint);
begin
  Inc(FCount_1010);
  case FCount_1010 of
    1: FPoints[7] := APoint;
    2: FPoints[6] := APoint;
  end;
end;

procedure TdxfViewport.NextReal(const AReader: TdxfReader; AValue: Double);
begin
  Inc(FCount_1040);
  case FCount_1040 of
    1: FFloats[11]    := AValue;
    2: FFloats[5]     := AValue;
    3: FPoints[2].X   := AValue;
    4: FPoints[2].Y   := AValue;
  end;
end;

{protected}

constructor TdxfViewport.Create;
begin
  inherited Create;
  FInts[8] := 1;
  FInts[9] := 1;
  FPoints[6].Z := 1.0;
end;

procedure TdxfViewport.ApplyEED(AReader: TdxfReader);
var
  I: Integer;
  S: string;
  vViewport: TsgDXFViewport;
begin
  if Assigned(FACADExtData) then
  begin
    vViewport := TsgDXFViewport(FEnt);
    I := 0;
    while I < FACADExtData.DataCount do
    begin
      case FACADExtData.DataType[I] of
        edtString:
          begin
            S := FACADExtData.DataString[I];
            case FACADExtData.DataCode[I] of
              String_1003: vViewport.FrozenLayers.Add(AReader.Conv.LayerByName(S).Handle);
            end;
          end;
        edtDouble:
          if FACADExtData.DataCode[I] = Float_1040 then
            NextReal(AReader, FACADExtData.DataDouble[I]);
        edtF3DPoint: Next3Reals(AReader, FACADExtData.DataPoint[I]);
      end;
      Inc(I);
    end;
    EraseEED(FACADExtData);
  end;
end;

procedure TdxfViewport.Commit(const AReader: TdxfReader);
var
  vViewport: TsgDXFViewport;
begin
  inherited Commit(AReader);
  vViewport := TsgDXFViewport(FEnt);
  vViewport.PSpaceCenter        := GetPoints(0);
  vViewport.MSpaceCenter        := GetPoints(2);
  vViewport.ViewDirection       := GetPoints(6, True);
  vViewport.ViewTarget          := GetPoints(7);
  vViewport.PSpaceWidth         := FFloats[0];
  vViewport.PSpaceHeight        := FFloats[1];
  vViewport.FrontClipPlane      := FFloats[3];
  vViewport.BackClipPlane       := FFloats[4];
  vViewport.MSpaceHeight        := FFloats[5];
  vViewport.ViewTwistAngle      := FFloats[11];
  vViewport.StatusField         := FInts[8];
  vViewport.ThisID              := FInts[9];
  AReader.Conv.SetHandle(vViewport);
end;

procedure TdxfViewport.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    Integer_90:  TsgDXFViewport(FEnt).Flags := AReader.IntValue;
    String_340:
      begin
        if AReader.HandleValue <> cnstBadHandle then
          AReader.AddLink(FEnt, 'ClippingBoundary', AReader.HandleValue, TsgDXFViewportAccess);
      end;
    String_341,
    String_331: TsgDXFViewport(FEnt).FrozenLayers.Add(AReader.HandleValue);
      // acR2000 - String_341; acR2004+ - String_331; acR12 - 1003 (see below)
  end;
end;

{ TdxfVport }

constructor TdxfVport.Create;
begin
  inherited Create;
  FPoints[6] := MakeFPoint(0,0,1);
end;

procedure TdxfVport.Commit(const AReader: TdxfReader);
var
  vVPort: TsgDXFVport;
begin
  inherited Commit(AReader);
  vVPort := TsgDXFVport(FEnt);
  vVPort.CircleZoomPercent   := FInts[12];
  vVPort.UCSOrigin           := FOrigin;
  vVPort.UCSXDir             := FXDir;
  vVPort.UCSYDir             := FYDir;
  vVPort.UCSVP               := FUCSVP = 1;
  vVPort.ViewAspectRatio     := FFloats[1];
  vVPort.ViewCenterPoint     := GetPoints(2);
  vVPort.ViewDirection       := GetPoints(6, True);
  vVPort.ViewTarget          := GetPoints(7);
  vVPort.ViewHeight          := FFloats[0];
  vVPort.ViewTwistAngle      := FFloats[11];
  vVPort.Name := Name;
end;

procedure TdxfVport.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    UCSVP_65:   FUCSVP := AReader.IntValue;
    XOther_110: FOrigin.X := AReader.FloatValue;
    YOther_120: FOrigin.Y := AReader.FloatValue;
    ZOther_130: FOrigin.Z := AReader.FloatValue;
    XOther_111: FXDir.X := AReader.FloatValue;
    YOther_121: FXDir.Y := AReader.FloatValue;
    ZOther_131: FXDir.Z := AReader.FloatValue;
    XOther_112: FYDir.X := AReader.FloatValue;
    YOther_122: FYDir.Y := AReader.FloatValue;
    ZOther_132: FYDir.Z := AReader.FloatValue;
  end;
end;

function TdxfVport.SetLayerByName(AReader: TdxfReader; const AName: string): Integer;
begin
  Result := -1;
end;

function TdxfVport.SetLTypeByName(AReader: TdxfReader;
  const AName: string): Integer;
begin
  Result := 1;
end;

{ TdxfWipeout }

procedure TdxfWipeout.Commit(const AReader: TdxfReader);
var
  vWipeout: TsgCADWipeout;
begin
  inherited Commit(AReader);
  vWipeout := TsgCADWipeout(FEnt);
  vWipeout.Flags := Flags;
  vWipeout.ClippingBoundaryType := FInts[11];
  vWipeout.Point := GetPoints(0);
  vWipeout.UVector := GetPoints(1);
  vWipeout.VVector := GetPoints(2);
  vWipeout.Size := GetPoints(3);
end;

procedure TdxfWipeout.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    YOther_24:
      TsgCADWipeout(FEnt).AddPoint(GetPoints(4));
    Word_280:
      TsgCADWipeout(FEnt).UseClipping := AReader.IntValue <> 0;
  end;
end;

{ TdxfWipeoutVars }

procedure TdxfWipeoutVars.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Integer_70: TsgCADWipeoutVariables(FEnt).DisplayImageFrame := AReader.IntValue = 1;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfWipeoutVars.UpdateOwner(AReader: TdxfReader);
begin
  inherited UpdateOwner(AReader);
  if Assigned(AReader.FNamedObjects) then
    FSoftOwner := AReader.FNamedObjects;
end;

{ TdxfCircle }

constructor TdxfCircle.Create;
begin
  inherited Create;
  FFloats[11] := 360;
end;

procedure TdxfCircle.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFCircle(FEnt).Point  := GetPoints(0);
  TsgDXFCircle(FEnt).Radius := IfThen(FFloats[0] > fDoubleResolution,
    FFloats[0], 1.0);
end;

{ TdxfLine }

procedure TdxfLine.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFLine(FEnt).Point1 := GetPoints(1);
end;

{ TdxfSolid }

procedure TdxfSolid.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFSolid(FEnt).Point2 := GetPoints(2);
  TsgDXFSolid(FEnt).Point3 := GetPoints(3);
end;

{ TdxfSpline }

{private}

procedure TdxfSpline.AddControl;
begin
  if FNewCtl then
  begin
    TsgDXFSpline(FEnt).Controls.Add(GetPoints(0));
  end;
  FNewCtl := True;
end;

procedure TdxfSpline.AddFit;
begin
  if FNewFit then
  begin
    TsgDXFSpline(FEnt).Fit.Add(GetPoints(1));
  end;
  FNewFit := True;
end;

procedure TdxfSpline.AddKnot(const AValue: Double);
begin
  TsgDXFSpline(FEnt).Knots.Add(AValue);
end;

procedure TdxfSpline.AddWeight(const AValue: Double);
begin
  TsgDXFSpline(FEnt).AddWeight(AValue);
end;

{protected}

procedure TdxfSpline.ApplyEED(AReader: TdxfReader);
var
  I: Integer;
  vCode: SmallInt;
  vExtData: TsgCADExtendedDataAccess;
begin
  if Assigned(FExtData) then
  begin
    vExtData := TsgCADExtendedDataAccess(FExtData);
    I := 1;
    while I < vExtData.DataCount do
    begin
      case vExtData.DataType[I] of
        edtInt:
          if vExtData.GetCodeFor(I, vCode) and (vCode = Integer_70) then
            FFlagsExtented := vExtData.DataInt[I];
      end;
      Inc(I);
    end;
    EraseEED(FExtData);
  end;
end;

procedure TdxfSpline.Commit(const AReader: TdxfReader);
begin
  if FNewCtl then
    AddControl;
  if FNewFit then
    AddFit;
  inherited Commit(AReader);
  TsgDXFSpline(FEnt).BeginningTangent := GetPoints(2);
  TsgDXFSpline(FEnt).EndingTangent := GetPoints(3);
  TsgDXFSpline(FEnt).KnotTol := FFloats[2];
  TsgDXFSpline(FEnt).CtrlTol := FFloats[3];
  TsgDXFSpline(FEnt).FitTol := FFloats[4];
  SetDegreeSplineCurve;
  if FFlagsExtented > -1 then
    TsgDXFSplineAccess(FEnt).FlagsExtented := FFlagsExtented;
end;

constructor TdxfSpline.Create;
begin
  inherited Create;
  FCodeValue := cnstCodeValueUndefined;
  FFloats[4] := cnstFitTol;
  FFloats[2] := cnstKnotTol;
  FFloats[3] := cnstCtrlTol;
  FFlagsExtented := -1;
end;

procedure TdxfSpline.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
begin
  inherited InitEEDAppName(AData, AReader);
  if AReader.IsRelatedAppID(AReader.UValue) then
    FExtData := AData;
end;

procedure TdxfSpline.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    XFirst_10:         AddControl;
    XOther_11:         AddFit;
    Float_40:          AddKnot(AReader.FloatValue);
    Float_41:          AddWeight(AReader.FloatValue);
  end;
  inherited ReadProperty(AReader);
end;

procedure TdxfSpline.SetDegreeSplineCurve;
begin
  TsgDXFSpline(FEnt).Degree := FInts[11];
end;

{ TdxfPolyLine }

procedure TdxfPolyLine.Commit(const AReader: TdxfReader);
var
  vPolyLine: TsgDXFPolyLine;
begin
  inherited Commit(AReader);
  vPolyLine := TsgDXFPolyLine(FEnt);
  vPolyLine.StartWidth      := ConvertDoubleToSingle(FFloats[0]);
  vPolyLine.EndWidth        := ConvertDoubleToSingle(FFloats[1]);
  vPolyLine.GlobalWidth     := ConvertDoubleToSingle(FFloats[3]);
  if FInts[13] <> 0 then
    FInts[11] := FInts[13];
  if FInts[14] <> 0 then
    FInts[12] := FInts[14];
  vPolyLine.MeshM := FInts[11];
  vPolyLine.MeshN := FInts[12];
  SetElevation;
end;

constructor TdxfPolyLine.Create;
begin
  inherited Create;
  FComplex := True;
end;

procedure TdxfPolyLine.ReadEntities(AReader: TdxfReader);
begin
  while not AReader.EOF and (AReader.FNameHash = hshVertex) and not ReadEntity(AReader, AReader.FNameHash) do;
  if (FMarker = 'AcDb3dPolyline') and (FInts[10] and not 1 = 0) then
  begin
    {$IFDEF SG_CLIENT_CLNOFSFL}
    FMarker := 'AcDb2dPolyline';
    {$ELSE}
    if not TsgDXFPolyLine(FEnt).HasWidth then //no Widths
      TsgDXFPolyLine(FEnt).Flags := TsgDXFPolyLine(FEnt).Flags or 8;
    {$ENDIF}
  end;
end;

procedure TdxfPolyLine.SetElevation;
begin
  TsgDXFPolyLine(FEnt).Elevation := GetPoints(0).Z;
end;

{ TdxfAcadTable }

procedure TdxfAcadTable.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  FComplex := False;
  TsgDXFAcadTable(FEnt).Style := TsgDXFAcadTableStyle(AReader.ObjByHandle(FTableStyleHandle, TsgDXFAcadTableStyle));
  TsgDXFAcadTable(FEnt).BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(FBlockRecordHandle, TsgDXFBlockRecord));
end;

function TdxfAcadTable.CurCell: TsgAcadTableCell;
begin
  Result := TsgDXFAcadTable(FEnt).Cell[FColIndex, FRowIndex];
end;

procedure TdxfAcadTable.ReadProperty(const AReader: TdxfReader);
const
  cnstAcDbTable = 'AcDbTable';
var
  vFlags: Byte;
  vAcadTable: TsgDXFAcadTable;
begin
  vAcadTable := TsgDXFAcadTable(FEnt);
  case AReader.Code of
    1:  CurCell.Text := CurCell.Text + AReader.UValue;
    3:  CurCell.Text := CurCell.Text + AReader.UValue;
    7:  AReader.AddLink(CurCell, 'TextStyle', AReader.UValue, TsgAcadTableCellAccess);
    40: vAcadTable.Properties.HorzCellMargin := AReader.FloatValue;
    41: vAcadTable.Properties.VertCellMargin := AReader.FloatValue;
    63: CurCell.FillColor := MakeColorCAD(acIndexColor, AReader.IntValue);
    64: CurCell.TextColor := MakeColorCAD(acIndexColor, AReader.IntValue);
    65..69: CurCell.BorderColor[AReader.Code - 64] :=
      MakeColorCAD(acIndexColor, AReader.IntValue);
    70: vAcadTable.Properties.FlowDirection := AReader.IntValue;
    90: vAcadTable.Flags := AReader.IntValue;
    91:
      if not FEofCommon then
        vAcadTable.NumRows := AReader.IntValue;
    92:
      if FIsTableSubClass and not FEofCommon then
        vAcadTable.NumCols := AReader.IntValue;
    93..96:
      if not FEofCommon then
      begin
        vFlags := vAcadTable.OverrideFlags;
        vFlags := vFlags or AReader.IntValue shl (AReader.Code - 93);
        vAcadTable.OverrideFlags := vFlags;
      end;
    100: FIsTableSubClass := AReader.IsValueEqual(cnstAcDbTable);
    140: CurCell.TextHeight := AReader.FloatValue;
    141:
      begin
        vAcadTable.RowHeight[FRowIndex] := AReader.FloatValue;
        Inc(FRowIndex);
        if FRowIndex = vAcadTable.NumRows then
          FRowIndex := 0;
      end;
    142:
      begin
        vAcadTable.ColWidth[FColIndex] := AReader.FloatValue;
        Inc(FColIndex);
        if FColIndex = vAcadTable.NumCols then
        begin
          FColIndex := -1;
          FEofCommon := True;
        end;
      end;
    144: CurCell.BlockScale := AReader.FloatValue;
    145: CurCell.Rotation := AReader.FloatValue;
    170: CurCell.CellAlignment := AReader.IntValue;
    171:
      begin
        Inc(FColIndex);
        if FColIndex = vAcadTable.NumCols then
        begin
          FColIndex := 0;
          Inc(FRowIndex);
        end;
        CurCell.IsTextType := AReader.IntValue = 1;
      end;
    172: CurCell.Flags := AReader.IntValue;
    173: CurCell.MergedValue := AReader.IntValue;
    174: CurCell.AutoFit := AReader.IntValue = 1;
    175: CurCell.MergedWidth := AReader.IntValue;
    176: CurCell.MergedHeight := AReader.IntValue;
    177: CurCell.OverrideFlag := AReader.IntValue;
    178: CurCell.VirtualEgdeFlag := AReader.IntValue = 1;
    179: CurCell.NumAttdefs := AReader.IntValue;
    274..279: CurCell.BorderLineWeight[AReader.Code - 274] :=
      ExpandLWeight(AReader.IntValue);
    280: vAcadTable.Properties.TitleSuppressed := AReader.IntValue = 1;
    281: vAcadTable.Properties.HeaderSuppressed := AReader.IntValue = 1;
    283: CurCell.BkColorOn := AReader.IntValue = 1;
    284..289: CurCell.BorderVisible[AReader.Code - 284] :=
      AReader.IntValue = 1;
    300: CurCell.AttdefValue := AReader.UValue;
    331: CurCell.AttdefHandle := AReader.HandleValue;
    340: CurCell.BlockRecordHandle := AReader.HandleValue;
    // Read TableStyle Handle for AcadTable
    342: FTableStyleHandle := AReader.HandleValue;
    343: FBlockRecordHandle := AReader.HandleValue;
    // Field handle
    344: CurCell.Field := TsgDXFField(AReader.ObjByHandle(AReader.HandleValue, TsgDXFField));
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfAcadTableStyle }

procedure TdxfAcadTableStyle.Commit(const AReader: TdxfReader);
var
  I: Integer;
  vDictionaryOwner, vDictionaryItem: TsgDXFEntity;
begin
  inherited Commit(AReader);
  if FEnt.Name = '' then
    FEnt.Name := Name;
  if Assigned(FTemporaryDictionary) then
  begin
    vDictionaryOwner := FTemporaryDictionary.Owner;
    FTemporaryDictionary.Handle := cnstBadHandle;
    for I := FTemporaryDictionary.Count - 1 downto 0 do
    begin
      vDictionaryItem := FTemporaryDictionary[I];
      FTemporaryDictionary.DeleteEntity(I);
      AReader.Conv.Sections[csObjects].AddEntity(vDictionaryItem);
    end;
    if Assigned(vDictionaryOwner) then
      vDictionaryOwner.RemoveEntity(FTemporaryDictionary);
    FreeAndNil(FTemporaryDictionary);
  end;
end;

constructor TdxfAcadTableStyle.Create;
begin
  inherited Create;
  FCellTypeIndex := TsgAcadTableCellType(Ord(cttData) - 1);
end;

function TdxfAcadTableStyle.GetCell: TsgAcadTableCellStyle;
begin
  if FCellTypeIndex = TsgAcadTableCellType(Ord(cttData) - 1) then
    FCellTypeIndex := Succ(FCellTypeIndex);
  Result := TsgDXFAcadTableStyle(FEnt).CellStyle[FCellTypeIndex];
end;

procedure TdxfAcadTableStyle.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    3: TsgDXFAcadTableStyle(FEnt).Description := AReader.UValue;
    7:
      begin
        FCellTypeIndex := Succ(FCellTypeIndex);
        AReader.AddLink(Cell, 'TextStyle', Trim(AReader.UValue), TsgAcadTableCellStyleAccess);
      end;
    40: TsgDXFAcadTableStyle(FEnt).HorzCellMargin := AReader.FloatValue;
    41: TsgDXFAcadTableStyle(FEnt).VertCellMargin := AReader.FloatValue;
    62: Cell.TextColor := MakeColorCAD(acIndexColor, AReader.IntValue);
    63: Cell.FillColor := MakeColorCAD(acIndexColor, AReader.IntValue);
    64..69: Cell.BorderColor[AReader.Code - 64] := MakeColorCAD(acIndexColor, AReader.IntValue);
    70: TsgDXFAcadTableStyle(FEnt).FlowDirection := AReader.IntValue;
    71: TsgDXFAcadTableStyleAccess(FEnt).Flags := AReader.IntValue;
    90: Cell.DataType := AReader.IntValue;
    91: Cell.UnitType := AReader.IntValue;
    140: Cell.TextHeight := AReader.FloatValue;
    170: Cell.CellAlignment := AReader.IntValue;
    274..279: Cell.BorderLineWeight[AReader.Code - 274] := ExpandLWeight(AReader.FloatValue);
    284..289: Cell.BorderVisible[AReader.Code - 284] := AReader.IntValue = 1;
    280: TsgDXFAcadTableStyle(FEnt).TitleSuppressed := AReader.IntValue = 1;
    281: TsgDXFAcadTableStyle(FEnt).HeaderSuppressed := AReader.IntValue = 1;
    283: Cell.BkColorOn := AReader.IntValue = 1;
  else
    inherited ReadProperty(AReader);
  end;
end;

function TdxfAcadTableStyle.ReplaceFromDatabase(AReader: TdxfReader;
  var Index: Integer; out ASoftOwner: TsgDXFEntity): Integer;
begin
  Result := 0;
  if AReader.FindObject(FHandle, Index) then
  begin
    FTemporaryDictionary := TsgDXFEntity(AReader.DataBase[Index]);
    if FTemporaryDictionary is TsgDXFAcadTableStyle then
    begin
      SwapPointers(Pointer(FEnt), Pointer(FTemporaryDictionary));
      FreeAndNil(FTemporaryDictionary);
    end
    else
    begin
      FEnt.Handle := FHandle;
      AReader.DataBase.List[Index] := FEnt;
    end;
    Result := 1;
    FDatabaseAdded := True;
    FAdded := Assigned(FEnt.Owner);
  end;
end;

{ TdxfACIS }

procedure TdxfACIS.DoDecode(const AReader: TdxfReader);
var
  vDecoded: AnsiString;
  vStream: TMemoryStream;
begin
  if FEncoded = '' then
    Exit;
  vStream := TsgBrepModAcis(FEnt).QueryAcisStream;
  vDecoded := AReader.ACIS_Row_Decode(FEncoded) + #13#10;
  vStream.Write(vDecoded[cnstStrBegin], Length(vDecoded));
  FEncoded := '';
end;

procedure TdxfACIS.Commit(const AReader: TdxfReader);
begin
  DoDecode(AReader);
  inherited Commit(AReader);
end;

procedure TdxfACIS.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Text_1:
      begin
        AReader.AppendString(FEncoded);
        DoDecode(AReader);
      end;
    Text_3:
      AReader.AppendString(FEncoded);
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfArc }

procedure TdxfArc.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFArc(FEnt).StartAngle := FFloats[10];
  TsgDXFArc(FEnt).EndAngle   := FFloats[11];
end;

{ TdxfAttdef }

procedure TdxfAttdef.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
begin
  inherited InitEEDAppName(AData, AReader);
  if sgSameText(AReader.UValue, sACADXDataAttribAppName) then
    FACADAttrExtData := AData;
end;

procedure TdxfAttdef.Commit(const AReader: TdxfReader);
var
  vAttdef: TsgDXFAttdef;
begin
  FInts[13] := FInts[14];
  inherited Commit(AReader);
  vAttdef := TsgDXFAttdef(FEnt);
  vAttdef.Value       := FTexts[1];
  vAttdef.Tag         := Name;
  vAttdef.Flags       := Flags;
  if Assigned(FACADAttrExtData) then
  begin
    vAttdef.SetExtData(FACADAttrExtData);
    EraseEED(FACADAttrExtData);
  end;
  if Assigned(FMText) then
    vAttdef.TypeValue := atMText;
end;

procedure TdxfAttdef.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
  vSoftOwner: TsgDXFEntity;
begin
  case AReader.Code of
    Text_1:
      begin
        FTexts[1] := AReader.GetUValue;
        ReplaceAnsi(FTexts[1], cnstCaretAndSpace, cnstCaret);
      end;
    5:
      begin
        inherited ReadProperty(AReader);
        ReplaceFromDatabase(AReader, I, vSoftOwner);
      end;
    Word_280:
      begin
        if AReader.Version >= Byte(acR2007) then
          TsgDXFAttdef(FEnt).LockPosition := AReader.IntValue <> 0;
      end;
  else
    inherited ReadProperty(AReader);
  end;
end;

{$IFDEF SG_BTI}
{ TdxfAttrib }

procedure TdxfAttrib.InitEEDAppName(AData: TsgCADExtendedData; AReader: TdxfReader);
begin
  inherited InitEEDAppName(AData, AReader);
  if AReader.IsRelatedAppID(AReader.UValue) then
    FExtData := AData;
end;

{protected}

procedure TdxfAttrib.Commit(const AReader: TdxfReader);
var
  I: Integer;
  vEntity, vEnt: TsgDXFEntity;
begin
  inherited Commit(AReader);
  if Assigned(FExtData) then
  begin
    I := FExtData.IndexOfCode(Integer_1070);
    if (I > 0) and (FExtData.DataType[I] = edtInt16) and (FExtData.DataInt16[I] > 0) then
    begin
      vEntity := DXFConv.CreateRegEntity(cnstGroupTypeName[gtBTI], Ord(ceAttrib),
        AReader.Conv, FExtData);
      if Assigned(vEntity) then
      begin
        vEntity.Clone(FEnt);
        vEnt := ReleaseEntity;
        FEnt := vEntity;
        UpdateLinks(AReader);
        EraseEED(FExtData);
        vEnt.Free;
      end;
    end;
  end;
end;
{$ENDIF}

{ TdxfDimension }

{protected}

procedure TdxfDimension.Commit(const AReader: TdxfReader);
var
  vDimensionStyle: TsgDXFDimensionStyle;
  vDimension: TsgDXFDimension;
  vStyleName: string;
begin
  FFloats[10] := FFloats[14];
  inherited Commit(AReader);
  vDimension := TsgDXFDimension(FEnt);
  vStyleName := Trim(FTexts[3]);
  if vStyleName = '' then
    vStyleName := AReader.Conv.HeadVarStruct.DimStyle;
  vDimensionStyle := TsgDXFDimensionStyle(AReader.ItemByName[csDimStyles, vStyleName]);
  if Assigned(vDimensionStyle) then
    vDimension.Style := vDimensionStyle
  else
    AReader.AddLink(vDimension, 'Style', vStyleName, TsgDXFDimensionAccess);
  vDimension.Scale        := cnstFPointSingle;
  vDimension.DefPoint     := GetPoints(0);
  vDimension.MiddlePoint  := GetPoints(1);
  vDimension.Point        := GetPoints(2);
  vDimension.LinDefPoint1 := GetPoints(3);
  vDimension.LinDefPoint2 := GetPoints(4);
  vDimension.RadDefPoint  := GetPoints(5);
  vDimension.ArcDefPoint  := GetPoints(6);
  vDimension.Flags        := Flags;
  if FIsArcDimension then
    vDimension.Flags := vDimension.Flags or 7;
  vDimension.TextOverride := FTexts[1];
  vDimension.TextRotation := FFloats[13];
  vDimension.DimRot       := FDimRot;
  SetDStyle(AReader);
end;

procedure TdxfDimension.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    String_100:
      FIsArcDimension := FIsArcDimension or (FMarker = cntCPlusPlusClassArcDimesion);
    Angle_50:
      if FMarker = 'AcDbAlignedDimension' then
        FDimRot := FFloats[10];
  end;
end;

procedure TdxfDimension.SetDStyle(const AReader: TdxfReader);
var
  vDimension: TsgDXFDimensionAccess;
  vDimProps: TsgDXFDimensionStyle;
  I: Integer;
  vCode: SmallInt;
  vInt64Value: Uint64;
begin
  if Assigned(FACADExtData) and FACADExtData.IsEqualSubName(cnstDStyle) then
  begin
    vDimension := TsgDXFDimensionAccess(FEnt);
    vDimension.SetExtData(FACADExtData);
    vDimProps := vDimension.Properties;
    I := 1;
    vCode := -1;
    while I < FACADExtData.DataCount do
    begin
      if vCode > -1 then
      begin
        case FACADExtData.DataCode[I] of//create linkobject for unfound entity
          String_1005:
            begin
              vInt64Value := FACADExtData.DataInt64[I];
              case vCode of
                340: vDimProps.TextStyle := TsgDXFStyle(AReader.ObjByHandle(vInt64Value, TsgDXFStyle));
                341: vDimProps.DIMLDRBLK := TsgDXFBlock(AReader.ObjByHandle(vInt64Value, TsgDXFBlockRecord));
                342: vDimProps.DIMBLK    := TsgDXFBlock(AReader.ObjByHandle(vInt64Value, TsgDXFBlockRecord));
                343: vDimProps.DIMBLK1   := TsgDXFBlock(AReader.ObjByHandle(vInt64Value, TsgDXFBlockRecord));
                344: vDimProps.DIMBLK2   := TsgDXFBlock(AReader.ObjByHandle(vInt64Value, TsgDXFBlockRecord));
              end;
            end;
        end;
        vCode := -1;
      end
      else
      begin
        case FACADExtData.DataCode[I] of
          Integer_1070:  vCode := FACADExtData.DataInt16[I];
        end;
      end;
      Inc(I);
    end;
    EraseEED(FACADExtData);
  end;
end;

{ TdxfDimStyle }

procedure TdxfDimStyle.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
  vDimStyle: TsgDXFDimensionStyle;
  vSoftOwner: TsgDXFEntity;
begin
  vDimStyle := TsgDXFDimensionStyle(FEnt);
  inherited ReadProperty(AReader);
  case AReader.Code of
    Name_2:
      begin
        if AnsiCompareText(AReader.Conv.HeadVarStruct.DimStyle, Name) = 0 then
        begin
          TsgDXFDimStyleAccess(vDimStyle).FDimProps := AReader.Conv.HeadVarStruct.DimProps;
          AReader.AddLink(vDimStyle, 'TextStyle', AReader.Conv.HeadVarStruct.DimTextStyle, TsgDXFDimStyleAccess);
        end;
      end;
    Text_3:       vDimStyle.DIMPOST        := AReader.StrValue;
    5..7:         Exit;
    105:
      begin
        FHandle := AReader.HandleValue;
        ReplaceFromDatabase(AReader, I, vSoftOwner);
      end;
    Float_40:     if FFloats[0] <> 0 then vDimStyle.Scale := FFloats[0];
    Float_41:     if FFloats[1] <> 0 then vDimStyle.ArrowSize := FFloats[1];
    Float_42:     vDimStyle.ExtLineOffset  := FFloats[2];
    Float_44:     vDimStyle.ExtLineExt     := FFloats[4];
    Float_47:     vDimStyle.DIMTP          := FFloats[7];
    Float_48:     vDimStyle.DIMTM          := FFloats[8];
    Integer_73:   vDimStyle.DIMTIH         := AReader.IntValue = 1;
    Integer_74:   vDimStyle.DIMTOH         := AReader.IntValue = 1;
    Integer_75:   vDimStyle.DIMSE1         := AReader.IntValue = 1;
    Integer_76:   vDimStyle.DIMSE2         := AReader.IntValue = 1;
    Integer_77:   vDimStyle.TextPosVert    := TsgDimTextPosVert(FInts[17]);
    Float_140:    vDimStyle.TextHeight     := AReader.FloatValue;
    Float_141:    vDimStyle.SizeCenterMark := AReader.FloatValue;
    Float_144:    vDimStyle.DIMLFAC        := AReader.FloatValue;
    Float_147:    vDimStyle.TextOffset     := AReader.FloatValue;
    Integer_173:  vDimStyle.DIMSAH         := AReader.IntValue = 1;
    Integer_174:  vDimStyle.DIMTIX         := AReader.IntValue;
    Integer_176:  vDimStyle.DIMCLRD        := MakeColorCAD(acIndexColor, AReader.IntValue);
    Integer_177:  vDimStyle.DIMCLRE        := MakeColorCAD(acIndexColor, AReader.IntValue);
    Integer_178:  vDimStyle.DIMCLRT        := MakeColorCAD(acIndexColor, AReader.IntValue);
    Integer_271:  vDimStyle.DIMDEC         := AReader.IntValue;
    Integer_276:  vDimStyle.DIMFRAC        := AReader.IntValue;
    Integer_277:  vDimStyle.DIMLUNIT       := GetDimLimitUnitsType(AReader.IntValue);
    Integer_278:  vDimStyle.DIMDSEP        := Char(AReader.IntValue);
    Word_281:     vDimStyle.DIMSD1         := AReader.IntValue = 1;
    Word_282:     vDimStyle.DIMSD2         := AReader.IntValue = 1;
    String_340:   vDimStyle.TextStyle      := TsgDXFStyle(AReader.ObjByHandle(AReader.HandleValue, TsgDXFStyle));
    341:          vDimStyle.DIMLDRBLK      := TsgDXFBlock(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    342:          vDimStyle.DIMBLK         := TsgDXFBlock(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    343:          vDimStyle.DIMBLK1        := TsgDXFBlock(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    344:          vDimStyle.DIMBLK2        := TsgDXFBlock(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    371:          vDimStyle.DIMLWD         := ExpandLWeight(AReader.IntValue);
    372:          vDimStyle.DIMLWE         := ExpandLWeight(AReader.IntValue);
  end;
end;

{ TdxfEllipse }

constructor TdxfEllipse.Create;
begin
  inherited Create;
  FFloats[2] := 2*Pi;
end;

procedure TdxfEllipse.Commit(const AReader: TdxfReader);
var
  vEllipse: TsgDXFEllipse;
begin
  FFloats[10] := Degree(FFloats[1]);
  FFloats[11] := Degree(FFloats[2]);
  inherited Commit(AReader);
  vEllipse := TsgDXFEllipse(FEnt);
  vEllipse.EndPoint := GetPoints(1);
  vEllipse.Ratio := FFloats[0];
end;

{ TdxfFace }

procedure TdxfFace.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXF3DFace(FEnt).Flags := Flags;
end;

{ TdxfField }

procedure TdxfField.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if FEnt.Name = '' then
    FEnt.Name := 'FIELD';
//  FEnt.Dictionary := FOwner.FEnt;
  TsgDXFField(FEnt).ObjectIDs.Assign(FPointerIDs);
end;

destructor TdxfField.Destroy;
begin
  if FData <> nil then
    ReallocMem(FData, 0);
  inherited Destroy;
end;

procedure TdxfField.ReadProperty(const AReader: TdxfReader);
const
  cnstAcDbField = 'AcDbField';
var
  vField: TsgDXFField;
begin
  vField := TsgDXFField(FEnt);
  case AReader.Code of
    1:
      if FReadDataSet then
        vField.DataSet.AddString(AReader.Code, AReader.StrValue)
      else
        if FReadCache then
          vField.Cache.AddString(AReader.Code, AReader.StrValue)
        else
          vField.EvaluatorID := AReader.StrValue;
    Name_2: vField.FieldCode := AReader.StrValue;
    3, 4: vField.OverflowFieldCode := AReader.StrValue;
    100: FReadFieldObject := AReader.UValue = cnstAcDbField;
    330:
      begin
        inherited ReadProperty(AReader);
        if AReader.FRefsHash = 0 then
        begin
          if not FReadFieldObject then
          begin
            //FSoftOwnerIDs.Free;
            //FSoftOwnerIDs := TsgInt64List.Create;
          end
          else
            if FReadCache then
              vField.Cache.AddInt64(AReader.Code, FSoftOwnerIDs.Last)
            else
              if FReadDataSet then
                vField.DataSet.AddInt64(AReader.Code, FSoftOwnerIDs.Last);
        end
        else
          FDictionary := FRctrsSoftIDs.Last;
      end;
    93: FReadDataSet := AReader.IntValue > 0;
    90, 91:
      if FReadCache then
        vField.Cache.AddInt(AReader.Code, AReader.IntValue)
      else
        if FReadDataSet then
          vField.DataSet.AddInt(AReader.Code, AReader.IntValue);
    140:
      if FReadCache then
        vField.Cache.AddDouble(AReader.Code, AReader.FloatValue)
      else
        if FReadDataSet then
          vField.DataSet.AddDouble(AReader.Code, AReader.FloatValue);
    92:
      if FReadCache or FReadDataSet then
      begin
        FBuffSize := AReader.IntValue;
        ReallocMem(FData, FBuffSize);
        FBytesRead := 0;
      end;
    310:
      begin
        AReader.AddBinData(Pointer(FData), FBytesRead);
        if FBytesRead = FBuffSize then
        begin
          if FReadCache then
            vField.Cache.AddBinary(AReader.Code, FBuffSize, FData)
          else
            if FReadDataSet then
              vField.DataSet.AddBinary(AReader.Code, FBuffSize, FData);
          ReallocMem(FData, 0);
          FBuffSize := 0;
          FBytesRead := 0;
        end;
      end;
    // Key string for the field data
    6: vField.DataSet.AddString(AReader.Code, AReader.UValue);
    7:
      begin
        FReadCache := True;
        FReadDataSet := False;
        // Key string for the evaluated cache, always ACFD_FIELD_VALUE
        vField.Cache.AddString(AReader.Code, AReader.UValue);
      end;
    9, 301:
      if FReadCache then
        vField.Cache.AddString(AReader.Code, AReader.UValue);
    98:
      if FReadCache then
        vField.Cache.AddInt(AReader.Code, AReader.IntValue);
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfFieldList }

procedure TdxfFieldList.Commit(const AReader: TdxfReader);
var
  I, J: Integer;
  vField: TsgDXFField;
begin
  inherited Commit(AReader);
  if AReader.FFieldList = nil then
    AReader.FFieldList := TsgDXFFieldList(FEnt);
  if FEnt.Name = '' then
    FEnt.Name := sAcadFieldListDictionary;
  for I := FStartSoftHandles to FSoftOwnerIDs.Count - 1 do
  begin
    if not AReader.FindObject(FSoftOwnerIDs[I], J) then
    begin
      vField := TsgDXFField.Create;
      vField.Handle := FSoftOwnerIDs[I];
      vField.Name := 'FIELD';
      FEnt.AddEntity(vField);
      AReader.DataBase.InsertFinded(J, vField);
    end;
  end;
end;

procedure TdxfFieldList.ReadProperty(const AReader: TdxfReader);
const
  cnstAcDbIdSet = 'AcDbIdSet';
begin
  case AReader.Code of
    String_100:
      begin
        FAcDbIdSet := AReader.UValue = cnstAcDbIdSet;
        if FAcDbIdSet then
          FStartSoftHandles := FSoftOwnerIDs.Count;
      end;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfHelix }

procedure TdxfHelix.Commit(const AReader: TdxfReader);
var
  vHelix: TsgCADHelix;
begin
  inherited Commit(AReader);
  vHelix := TsgCADHelix(FEnt);
  vHelix.Point := GetPoints(0);
  vHelix.Start := GetPoints(1);
  vHelix.Turns := FFloats[1];
  vHelix.TurnHeight := FFloats[2];
end;

procedure TdxfHelix.ReadProperty(const AReader: TdxfReader);
var
  vHelix: TsgCADHelix;
begin
  if AReader.Code = 100 then
  begin
    FLock := AReader.IsValueEqual('AcDbHelix');
    if FLock then
    begin
      if FNewCtl then
        AddControl;
      if FNewFit then
        AddFit;
    end;
  end;
  if not FLock then
    inherited ReadProperty(AReader)
  else
  begin
    FNewCtl := False;
    FNewFit := False;
    vHelix := TsgCADHelix(FEnt);
    case AReader.Code of
      Float_40:         vHelix.Radius := AReader.FloatValue;
      Integer_290:      vHelix.Clockwise := AReader.IntValue = 0;
    else
      inherited ReadProperty(AReader);
    end;
  end;
end;

{ TdxfImageEnt }

procedure TdxfImageEnt.Commit(const AReader: TdxfReader);
//var
//  vProxy: TsgDXFProxy;
//  vProxyReader: IProxyReader;
//  vEnts: TList;
//  vProps: TList;
begin
  inherited Commit(AReader);
  TsgDXFImageEnt(FEnt).ImageDef := TsgDXFImageDef(AReader.ObjByHandle(FImageDef, TsgDXFImageDef));
// for future version
//  if (FData <> nil) then
//  begin
//    vProxy := TsgDXFProxy.Create;
//    try
//      vEnts := TList.Create;
//      vProps := TList.Create;
//      try
//        vProxyReader := TCustomAlignedDataReader.Create(FData, TsgDWGVersion(AReader.Version));
//        ReadProxyData(vEnts, vProps, AReader.Conv, vProxyReader);
//      finally
//        vEnts.Free;
//        FreeRecordList(vProps);
//      end;
//    finally
//      vProxy.Free;
//    end;
//  end;
end;

destructor TdxfImageEnt.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

procedure TdxfImageEnt.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    String_310: AReader.AddBinData(Pointer(FData), FSize);
    String_340: FImageDef := AReader.HandleValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfLayer }

procedure TdxfLayer.Commit(const AReader: TdxfReader);
var
  I: Integer;
begin
  inherited Commit(AReader);
  FEnt.Visible := FInts[2] >= 0;
  if Assigned(FPenTableItemWithSameNameAlreadyExistsInOwnerTable) then
  begin
    if FOwner is TdxfTable then
    begin
      I := TdxfTable(FOwner).FSortEntList.IndexOfObject(FEnt);
      if I >= 0 then
        TdxfTable(FOwner).FSortEntList.Delete(I);
    end;
    SwapPointers(Pointer(FEnt), Pointer(FPenTableItemWithSameNameAlreadyExistsInOwnerTable));
    UpdateLinks(AReader);
    FreeAndNil(FPenTableItemWithSameNameAlreadyExistsInOwnerTable);
  end;
end;

constructor TdxfLayer.Create;
begin
  inherited Create;
  FLineWeight := fLineWeightDefault;
end;

procedure TdxfLayer.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Integer_290: TsgDXFLayer(FEnt).IsPlotting := AReader.IntValue <> 0;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfLeader }

constructor TdxfLeader.Create;
begin
  inherited Create;
  FHookline := 1;
  FHooklineDirectional := 1;
  FHooklineVector := cnstXOrtAxis;
  FInts[11] := 1;
  FTAD := -1;
  FGAP := 0;
  FAnnotationType := 3;
  FBlockRefHandle := cnstBadBlockRefHandle;
end;

procedure TdxfLeader.ApplyEED(AReader: TdxfReader);
var
  I: Integer;
  vCode: SmallInt;
  vExtData: TsgCADExtendedDataAccess;
begin
  if Assigned(FACADExtData) then
  begin
    vExtData := TsgCADExtendedDataAccess(FACADExtData);
    I := 1;
    while I < vExtData.DataCount do
    begin
      case vExtData.DataType[I] of
        edtDouble:
          if vExtData.DataCode[I] = Float_1040 then
            if vExtData.GetCodeFor(I, vCode) then
              case vCode of
                Float_40: FArrowScale := Abs(vExtData.DataDouble[I]);
                Float_41: FArrowSize  := Abs(vExtData.DataDouble[I]);
                Float_147: FGAP := vExtData.DataDouble[I];
              end;
        edtInt64:
          if vExtData.DataCode[I] = String_1005 then
            if vExtData.GetCodeFor(I, vCode) and (vCode = String_341) then
              FBlockRefHandle := vExtData.DataInt64[I];
        edtInt16:
          if vExtData.DataCode[I] = Integer_1070 then
            if vExtData.GetCodeFor(I, vCode) and (vCode = Integer_77) then
              FTAD := vExtData.DataInt16[I];
      end;
      Inc(I);
    end;
    EraseEED(FACADExtData);
  end;
end;

procedure TdxfLeader.Commit(const AReader: TdxfReader);
var
  vDimStyle: TsgDXFDimensionStyle;
  vLeader: TsgDXFLeaderAccess;
begin
  inherited Commit(AReader);
  vLeader := TsgDXFLeaderAccess(FEnt);
  vLeader.IsSpline  := FInts[12] = 1;
  FTexts[3] := Trim(FTexts[3]);
  vDimStyle := nil;
  if Assigned(AReader.Conv.Sections[csDimStyles]) then
    vDimStyle := TsgDXFDimensionStyle(AReader.ItemByName[csDimStyles, FTexts[3]]);
  if Assigned(vDimStyle) then
    vLeader.DimStyle := vDimStyle
  else
    AReader.AddLink(vLeader, 'DimStyle', FTexts[3], TsgDXFLeaderAccess);
  case FBlockRefHandle of
    cnstBadBlockRefHandle:      begin end;
    cnstBlockRefHandleDefault: AReader.AddLink(vLeader, 'DIMLDRBLK', sgDimensionArrowTypeNames[datClosedfilled], TsgDXFLeaderAccess);
  else
    vLeader.DIMLDRBLK := TsgDXFBlock(AReader.ObjByHandle(FBlockRefHandle, TsgDXFBlockRecord));
  end;
  if FArrowSize <> 0 then
    vLeader.ArrowSize := FArrowSize;
  if FArrowScale <> 0 then
    vLeader.ArrowScale := FArrowScale;
  if (FTAD < 0) and (vLeader.DimStyle <> nil) then
    FTAD := Byte(vLeader.DimStyle.TextPosVert);
  if FAnnotationType <> 3 then
  begin
    TsgDXFLeaderAccess(vLeader).DoAdditionalLine(
      1(*FHookline*), PtXScalar(FHooklineVector, 2 * FHooklineDirectional - 1),
      FTAD, FGAP, FTextAnnotationWidth);
  end;
end;

procedure TdxfLeader.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    //Float_40, Float_41: Exit;
    Float_40: FTextAnnotationHeight := AReader.FloatValue;
    Float_41: FTextAnnotationWidth := AReader.FloatValue;
    Integer_73: FAnnotationType := AReader.IntValue;
    Integer_75: FHookline := AReader.IntValue;
    Integer_74: FHooklineDirectional := AReader.IntValue;

    Float_211:  FHooklineVector.X := AReader.FloatValue;
    Float_221:  FHooklineVector.Y := AReader.FloatValue;
    Float_231:  FHooklineVector.Z := AReader.FloatValue;

    String_340: FHRefAsAnnotation := AReader.HandleValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfLeader.SetDegreeSplineCurve;
begin
  TsgDXFLeader(FEnt).Arrowhead := FInts[11] <> 0;
end;

{ TdxfLType }

{private}

{protected}

procedure TdxfLType.Commit(const AReader: TdxfReader);
var
  I: Integer;
begin
  inherited Commit(AReader);
  for I := Low(FElements) to FCount - 1 do
  begin
    if (FElements[I].ComplexType and 7 = 2) and (FElements[I].Text <> '') then
      FElements[I].ShapeNumber := 0;
    TsgDXFLineType(FEnt).Lines.Add(FElements[I]);
    if TsgDXFLineType(FEnt).Lines.Elements[I].Style is TsgLTElementStyleLink then
      AReader.DoAddLink(TsgDXFLineType(FEnt).Lines, nil,
        TsgLTElementStyleLink(TsgDXFLineType(FEnt).Lines.Elements[I].Style));
  end;
end;

constructor TdxfLType.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FElements, 1);// capaity 1
end;

procedure TdxfLType.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    Float_44:          FElements[FIndex].XOffset := FFloats[4];
    Float_45:          FElements[FIndex].YOffset := FFloats[5];
    Float_46:          FElements[FIndex].Scale := FFloats[6];
    FloatRepeated_49:
    begin
      Inc(FCount);
      SetLength(FElements, FCount);
      FIndex := High(FElements);
      FElements[FIndex].Scale := 1.0;
      FElements[FIndex].Dash := FFloats[9];
    end;
    Angle_50:          FElements[FIndex].Rotation := FFloats[10];
    Integer_74:        FElements[FIndex].ComplexType := FInts[14];
    Integer_75:        FElements[FIndex].ShapeNumber := FInts[15];
    String_340:        FElements[FIndex].Style := TsgLTElementStyleLink.Create(AReader.HandleValue, FIndex);
    VarName_9:         FElements[FIndex].Text := FTexts[9];
  end;
end;

{ TdxfLWPline }

{private}

procedure TdxfLWPline.AddVertex;
var
  vVertex: TsgDXFVertex;
begin
  vVertex := TsgDXFVertex.Create;
  vVertex.Point       := GetPoints(0);
  vVertex.StartWidth  := FFloats[0];
  vVertex.EndWidth    := FFloats[1];
  vVertex.Bulge       := FFloats[2];
  TsgDXFLWPolyLine(FEnt).AddEntity(vVertex);
  FFloats[0] := 0;
  FFloats[1] := 0;
  FFloats[2] := 0;
end;

{protected}

procedure TdxfLWPline.Commit(const AReader: TdxfReader);
begin
  if FNewVertex then AddVertex;
  inherited Commit(AReader);
  FComplex := False;
end;

constructor TdxfLWPline.Create;
begin
  inherited Create;
  FComplex := False;
end;

procedure TdxfLWPline.ReadProperty(const AReader: TdxfReader);
begin
  if AReader.Code = 10 then
  begin
    if FNewVertex then AddVertex;
    FNewVertex := True;
  end;
  inherited ReadProperty(AReader);
end;

procedure TdxfLWPline.SetElevation;
begin
  TsgDXFLWPolyline(FEnt).Elevation := GetElevation;
end;

{ TdxfStyle }

constructor TdxfStyle.Create;
begin
  inherited Create;
  Name := sStandardName;
  FTexts[3] := 'txt';
  FFloats[1] := 1.0;
  FFloats[2] := 2.5;
  FTexts[6] := '';
end;

function TdxfStyle.GetPrimaryFont: string;
begin
  Result := Trim(FTexts[3]);
end;

procedure TdxfStyle.ApplyEED(AReader: TdxfReader);
var
  I, vFlags: Integer;
  vFontStyles: TmvFontStyles;
begin
  if Assigned(FACADExtData) then
  begin
    I := 0;
    while I < FACADExtData.DataCount do
    begin
      case FACADExtData.DataType[I] of
        edtString: TsgDXFStyle(FEnt).FontName := Trim(FACADExtData.DataString[I]);
        edtInt:
          begin
            vFlags := FACADExtData.DataInt[I];
            vFontStyles := [];
            if vFlags and $1000000 <> 0 then Include(vFontStyles, fmItalic);
            if vFlags and $2000000 <> 0 then Include(vFontStyles, fmBold);
            TsgDXFStyle(FEnt).FontStyle := vFontStyles;
          end;
      end; { case }
      Inc(I);
    end;
    EraseEED(FACADExtData);
  end;
end;

procedure TdxfStyle.Commit(const AReader: TdxfReader);
var
  vStyle: TsgDXFStyle;
begin
  inherited Commit(AReader);
  vStyle := TsgDXFStyle(FEnt);
  vStyle.FixedHeight         := FFloats[0];
  vStyle.WidthFactor         := FFloats[1];
  vStyle.LastHeightUsed      := FFloats[2];
  vStyle.ObliqueAngle        := FFloats[10];
  vStyle.TextGenFlags        := FInts[11];
  vStyle.PrimaryFont         := GetPrimaryFont;
  vStyle.BigFont             := Trim(FTexts[4]);
  if vStyle.PrimaryFont = '' then
    vStyle.PrimaryFont := sDefaultSHXFont;
end;

{ TdxfRasterVariables }

procedure TdxfRasterVariables.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Integer_71: TsgCADRasterVariables(FEnt).ImageDisplayQuality := AReader.IntValue = 1;
    Integer_72: TsgCADRasterVariables(FEnt).UnitsForInsertingImages := AReader.IntValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfDataLink }

procedure TdxfDataLink.ReadProperty(const AReader: TdxfReader);
begin
  if FCustomData then
  begin
    case AReader.Code of
      String_309:
       begin
         if SameText(AReader.StrValue, 'DATAMAP_END') then
           FCustomData := False;
       end;
    else
      inherited ReadProperty(AReader);
    end;
  end
  else
  begin
    case AReader.Code of
      String_302: TsgCADDataLink(FEnt).FileName := AReader.StrValue;
      Text_1:
       begin
         if SameText(AReader.StrValue,'DATAMAP_BEGIN') then
           FCustomData := True;
       end;
    else
      inherited ReadProperty(AReader);
    end;
  end;
end;

{ TdxfPlotSettings }

procedure TdxfPlotSettings.ReadProperty(const AReader: TdxfReader);
var
  vPlotSettings : TsgDXFPlotSettings;
  vPlotSettingsData : PsgPlotSettingsData;
begin
  vPlotSettings := TsgDXFPlotSettings(FEnt);
  vPlotSettingsData := TsgDXFPlotSettingsAccess(vPlotSettings).PlotData;
  case AReader.Code of
    Text_1: vPlotSettingsData^.PageSetupName := AReader.StrValue;
    Name_2: vPlotSettingsData^.PrintOrConfigName := AReader.StrValue;
    Text_4: vPlotSettingsData^.PaperSize := AReader.StrValue;
    LineTypeName_6: vPlotSettingsData^.PlotViewName := AReader.StrValue;
    TextFont_7: vPlotSettingsData^.CurrentStyleSheet := AReader.StrValue;
    Float_40: vPlotSettingsData^.UnprintableMargin.Left := AReader.FloatValue;
    Float_41: vPlotSettingsData^.UnprintableMargin.Bottom := AReader.FloatValue;
    Float_42: vPlotSettingsData^.UnprintableMargin.Right := AReader.FloatValue;
    Float_43: vPlotSettingsData^.UnprintableMargin.Top := AReader.FloatValue;
    Float_44: vPlotSettingsData^.PlotPaperSize.X := AReader.FloatValue;
    Float_45: vPlotSettingsData^.PlotPaperSize.Y := AReader.FloatValue;
    Float_46: vPlotSettingsData^.PlotOrigin.X := AReader.FloatValue;
    Float_47: vPlotSettingsData^.PlotOrigin.Y := AReader.FloatValue;
    Float_48: vPlotSettingsData^.PlotWindowAreaMin.X := AReader.FloatValue;
    FloatRepeated_49: vPlotSettingsData^.PlotWindowAreaMin.Y := AReader.FloatValue;
    Integer_70: vPlotSettingsData^.PlotLayoutFlags :=
      ConvertIntegerToPlotLayoutFlags(AReader.IntValue);
    Integer_72: vPlotSettingsData^.PlotPaperUnits := TsgPlotPaperUnits(AReader.IntValue);
    Integer_73: vPlotSettingsData^.PlotRotation := GetPlotRotation(AReader.IntValue);
    Integer_74: vPlotSettingsData^.PlotType := TsgPlotType(AReader.IntValue);
    Integer_75: vPlotSettingsData^.StandardScaleType := AReader.IntValue;
    Integer_76: vPlotSettingsData^.ShadePlotMode := AReader.IntValue;
    Integer_77: vPlotSettingsData^.ShadePlotResolutionLevel := AReader.IntValue;
    Integer_78: vPlotSettingsData^.ShadePlotCustomDPI := AReader.IntValue;
    Float_140: vPlotSettingsData^.PlotWindowAreaMax.X := AReader.FloatValue;
    Float_141: vPlotSettingsData^.PlotWindowAreaMax.Y := AReader.FloatValue;
    Float_142: vPlotSettingsData^.NumeratorOfCustomPrintScale := AReader.FloatValue;
    Float_143: vPlotSettingsData^.DenominatorOfCustomPrintScale := AReader.FloatValue;
    Float_147: vPlotSettingsData^.FloatingPointScaleFactor := AReader.FloatValue;
    Float_148: vPlotSettingsData^.PaperImageOrigin.X := AReader.FloatValue;
    Float_149: vPlotSettingsData^.PaperImageOrigin.Y := AReader.FloatValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfPlotSettings.ReadProps(AReader: TdxfReader);
var
  vDoBreak: Boolean;
begin
  vDoBreak := False;
  repeat
    AReader.Next;
    case AReader.Code of
      Info_0:           vDoBreak := True;
      String_100:
        begin
          if FOwner.ClassType = TdxfLayout then
            vDoBreak := AReader.UValue <> sAcDbPlotSettings;
        end;
    else
      FReadProperty(AReader);
      vDoBreak := AReader.Code = Info_0;
    end;
  until vDoBreak;
end;

{ TdxfDictionaryVar }

procedure TdxfDictionaryVar.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    Text_1: TsgDXFDictionaryVar(FEnt).Value := AReader.UValue;
  end;
end;

{ TdxfBlockRecord }

procedure TdxfBlockRecord.Commit(const AReader: TdxfReader);
var
  vName: string;
  vLayout: TsgDXFLayoutAccess;
  vBlkRec: TsgDXFBlockRecordAccess;
{$IFDEF SG_BLOCK_PREVIEW}
  vBitmap: TsgBitmapAccess;
{$ENDIF}
begin
  inherited Commit(AReader);
  vBlkRec := TsgDXFBlockRecordAccess(FEnt);
  vLayout := TsgDXFLayoutAccess(AReader.ObjByHandle(FLayout, TsgDXFLayout));
  if Assigned(vLayout) and (vLayout.BlockRecord = nil) then
  begin
    vBlkRec.Block.Layout := vLayout;
    TsgDXFLayout(vBlkRec.Block.Layout).PaperSpaceBlock := vBlkRec.Block;
  end;
  vName := Name;
  if vName = '' then
    vName := '*U$' + IntToHex(FHandle, 0);
  if (Length(vName) = 2) and (vName[1] = '*') then
    vName := vName + IntToStr(AReader.Conv.Sections[csBlocks].Count);
  vBlkRec.Name := vName;
  vBlkRec.SetFlags(FFlags);
  AReader.InitPaper(vBlkRec.Block);
{$IFDEF SG_BLOCK_PREVIEW}
  if FPreviewSize > SizeOf(TBitmapInfoHeader) then
  begin
    vBitmap := TsgBitmapAccess(TsgBitmap.Create);
    try
      vBitmap.LoadFromBitmapInfo(PBitmapInfo(FData));
      vBlkRec.Preview.Graphic := vBitmap;
    finally
      vBitmap.Free;
    end;
  end;
  FreeMemAndNil(FData);
{$ENDIF}
  if Assigned(vBlkRec.Block.Layout) then
    AReader.FLayoutsPerBlockRecord.Add(vBlkRec.Block.Layout.Handle, vBlkRec.Block.Layout);
end;

procedure TdxfBlockRecord.ReadProperty(const AReader: TdxfReader);
var
  I: Integer;
  vSoftOwner: TsgDXFEntity;
begin
  case AReader.Code of
    Name_2:
      begin
        inherited ReadProperty(AReader);
      end;
    5:
      begin
        inherited ReadProperty(AReader);
        ReplaceFromDatabase(AReader, I, vSoftOwner);
      end;
    340: FLayout := AReader.HandleValue;
     70: FFlags := AReader.IntValue;
{$IFDEF SG_BLOCK_PREVIEW}
    310:
      while not AReader.EOF and (AReader.Code = 310) do
      begin
        AReader.AddBinData(FData, FPreviewSize);
        AReader.Next;
      end;
{$ENDIF}
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfHeader }

procedure TdxfHeader.Commit(const AReader: TdxfReader);
var
  vEnt: TsgDXFEntity;
begin
  vEnt := ReleaseEntity;
  inherited Commit(AReader);
  FEnt := vEnt;
  FComplex := True;
end;

constructor TdxfHeader.Create;
begin
  inherited Create;
  FComplex := True;
end;

procedure TdxfHeader.DoHeadVars(const AReader: TdxfReader);
var
  I: Integer;
  vDateTime: TDateTime;
  vTimeStamp: TTimeStamp;

  function DoJDTToDT(AJDT: Double; out ADateTime: TDateTime): Boolean;
  begin
    Result := False;
    if TryJulianDateToDateTime(Trunc(AJDT), ADateTime) and
      (ADateTime > EncodeDateTime(1, 1, 1, 12, 0, 0, 0)) then
      Result := TryJulianDateToDateTime(AJDT - 1, ADateTime);
    if Result then
      ADateTime := IncHour(ADateTime, 12)
    else
      ADateTime := Now;
  end;

begin
  case FHeadVarIndex of
    0: AReader.Conv.MaxHandle := FHandle;
    1: FHeadVars^.DimProps.Alt := FInts[10] <> 0;
    2: FHeadVars^.DimProps.AltF := FFloats[0];
    3: FHeadVars^.DimProps.APost := FTexts[1];
    4: FHeadVars^.DimProps.Clrd := MakeColorCAD(acIndexColor,FInts[10]);
    5: FHeadVars^.DimProps.Clre := MakeColorCAD(acIndexColor,FInts[10]);
    6: FHeadVars^.DimProps.Clrt := MakeColorCAD(acIndexColor,FInts[10]);
    7: FHeadVars^.DimProps.Exe := FFloats[0];
    8: FHeadVars^.DimProps.Exo := FFloats[0];
    9: FHeadVars^.DimProps.Gap := FFloats[0];
    10: FHeadVars^.DimProps.LFac := FFloats[0];
    11: FHeadVars^.DimProps.POST := FTexts[1];
    20: FHeadVars^.DimProps.Asz := FFloats[0];
    21: FHeadVars^.DimProps.Scale := FFloats[0];
    22: FHeadVars^.DimStyle := Name;
    30: FHeadVars^.DimProps.Dec := FInts[10];
    31: FHeadVars^.DimProps.Tad := FInts[10];
    32: FHeadVars^.DimProps.Tih := FInts[10] = 1;
    33: FHeadVars^.DimProps.Tix := FInts[10];
    34: FHeadVars^.DimProps.Toh := FInts[10] = 1;
    35: FHeadVars^.DimProps.Arrows.Blk := GetArrowTypeByName(FTexts[1], datClosedfilled);
    36: FHeadVars^.DimProps.Arrows.Blk1 := GetArrowTypeByName(FTexts[1], datClosedfilled);
    37: FHeadVars^.DimProps.Arrows.Blk2 := GetArrowTypeByName(FTexts[1], datClosedfilled);
    38: FHeadVars^.DimProps.Arrows.LrBlk := GetArrowTypeByName(FTexts[1], datClosedfilled);
    39: FHeadVars^.DimProps.Txt := FFloats[0];
    43: FHeadVars^.DimTextStyle := FTexts[7];
    49: FHeadVars^.DimProps.SE1 := FInts[10] = 1;
    50: FHeadVars^.DimProps.SE2 := FInts[10] = 1;
    51: FHeadVars^.DimProps.SD1 := FInts[10] = 1;
    52: FHeadVars^.DimProps.SD2 := FInts[10] = 1;
    53: FHeadVars^.DimProps.LwD := ExpandLWeight(FInts[10]);
    54: FHeadVars^.DimProps.LwE := ExpandLWeight(FInts[10]);
    55: FHeadVars^.DimProps.Sah := FInts[10] = 1;
    63: FHeadVars^.DimProps.Tp := FFloats[0];
    64: FHeadVars^.DimProps.Tm := FFloats[0];
    65: FHeadVars^.DimProps.LUnit := GetDimLimitUnitsType(FInts[10]);
    66: FHeadVars^.DimProps.DSep := Char(FInts[10]);

    12: FHeadVars^.Measurement := FInts[10] <> 0;
    13: FHeadVars^.UCSORG := GetPoints(0);
    14: FHeadVars^.UCSXDir := GetPoints(0);
    15: FHeadVars^.UCSYDir := GetPoints(0);
    16: if FTexts[3] <> '' then
        begin
          Conv.SetCodePage(GetDWGCodePageByName(FTexts[3]));
          if TsgDWGVersion(FHeadVars^.Version) < acR2007 then
            AReader.FEnc := EncMgr.FromCP(Conv.CodePage);
        end;
    17: FHeadVars^.PointDisplayMode := FInts[10];
    18: FHeadVars^.TileMode := FInts[10];
    19: FHeadVars^.LTScale := GetOnefromZero(FFloats[0]);
    23: FHeadVars^.PointDisplaySize := FFloats[0];
    24:
      begin
        AReader.Conv.GetDWGVersion(AnsiString(FTexts[1]), TsgDWGVersion(FHeadVars^.Version));
        if TsgDWGVersion(FHeadVars^.Version) <= acR10 then
          AReader.FCheckHandle := AReader.Conv.CheckHandle
        else
        begin
          AReader.FCheckHandle := AReader.NoCheckHandle;
          if AReader.FClasses.FindItem(DoReadHash('BLOCK'), I) then
            PPointer(AReader.FClasses.List[I])^ := TdxfBlock;
        end;
        if TsgDWGVersion(FHeadVars^.Version) >= acR2007 then
          AReader.FEnc := EncMgr.FromCP(CP_UTF8);
      end;
    25: FHeadVars^.CEColor := MakeColorCAD(acIndexColor, FInts[2]);
    26: FHeadVars^.CLayer := FTexts[8];
    27: FHeadVars^.CELType := FTexts[6];
    28: FHeadVars^.CELTScale := FFloats[0];
    29: FHeadVars^.CELWeight := ExpandLWeight(FLineWeight);
    40: FHeadVars^.TextSize := FFloats[0];
    41: FHeadVars^.FilletRadius := FFloats[0];
    42: FHeadVars^.InsUnits := FInts[10];
    44: FHeadVars^.TextStyle := FTexts[7];
    45: FHeadVars^.ExtMin := GetPoints(0);
    46: FHeadVars^.ExtMax := GetPoints(0);
    47: FHeadVars^.InsBase := GetPoints(0);
    48: FHeadVars^.AttMode := GetAttributeMode(FInts[10]);
    56: FHeadVars^.XClipFrame := FFlag_290;
    57:
      begin
        DoJDTToDT(FFloats[0], vDateTime);
        Conv.DrwPropCreatedDateTime := vDateTime;//'$TDCREATE'
      end;
    58: //'$TDINDWG'
      begin
        vTimeStamp.Date := Trunc(FFloats[0]);
        vTimeStamp.Time := MilliSecondOfTheDay(Abs(FFloats[0] - vTimeStamp.Date));
        Conv.DrwPropTotalEditingTime := vTimeStamp;
      end;
    60:
      begin
        DoJDTToDT(FFloats[0], vDateTime);
        Conv.DrwPropModifiedDateTime := vDateTime;//'$TDUPDATE'
        Conv.LastSave := 0;
      end;
    67: FHeadVars^.DimAssoc := FFlag_280; //'$DIMASSOC'
    68: FHeadVars^.FillMode := FInts[10] <> 0;
    71: FHeadVars^.DimProps.Frac := FInts[10];
    72: FHeadVars^.LwDisplay := FFlag_290 and 1;
    73: FHeadVars^.LimMin := GetPoints(0);
    74: FHeadVars^.LimMax := GetPoints(0);
    75: FHeadVars^.PExtMin := GetPoints(0);
    76: FHeadVars^.PExtMax := GetPoints(0);
    77: FHeadVars^.PLimMin := GetPoints(0);
    78: FHeadVars^.PLimMax := GetPoints(0);
  end;
end;

function TdxfHeader.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := AObj.ClassType = TdxfEndSeq;
end;

procedure TdxfHeader.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    280:
      begin
        FFlag_280 := AReader.IntValue;
        DoHeadVars(AReader);
      end;
    290:
      begin
        FFlag_290 := AReader.IntValue;
        DoHeadVars(AReader);
      end;
    9: FHeadVarIndex := IndexHeadVar(FTexts[9]);
  else
    DoHeadVars(AReader);
  end;
end;

{ TdxfMain }

procedure TdxfMain.Commit(const AReader: TdxfReader);
var
  vEnt: TsgDXFEntity;
begin
  vEnt := ReleaseEntity;
  inherited Commit(AReader);
  FEnt := vEnt;
  FComplex := True;
end;

constructor TdxfMain.Create;
begin
  inherited Create;
  FComplex := True;
end;

function TdxfMain.CreateSubEntity(const AReader: TdxfReader;
  ANameHash: Cardinal): TdxfObject;
var
  I: Integer;
begin
  Result := inherited CreateSubEntity(AReader, ANameHash);
  if Assigned(Result.FEnt) then
    if AReader.FClasses.FindItem(ANameHash, I) then
      Result.FEnt.Name := TdxfReaderMetaclass(AReader.FClasses[I]).Name;
  Initialize(AReader, Result);
end;

destructor TdxfMain.Destroy;
begin
  FSectionObjects.Free;
  inherited Destroy;
end;

procedure TdxfMain.DestroyObj;
begin
  if FSectionObjects <> FObj then
    inherited DestroyObj;
end;

function TdxfMain.GetConv: TsgDXFConverterAccess;
begin
  Result := FConv;
end;

procedure TdxfMain.Initialize(AReader: TdxfReader; AObj: TdxfObject);
var
  vBlock: TsgDXFBlock;

  procedure ForceBlockRecords(const AConv: TsgDXFConverterAccess);
  begin
    if AConv.Sections[csBlockRecords] = nil then
    begin
      AConv.Sections[csBlockRecords] := TsgDXFBlockRecords.Create;
      TsgDXFGroupAccess(AConv.Sections[csBlockRecords]).SetConverter(AConv);
    end;
  end;

begin
  if AObj.ClassType = TdxfHeader then
  begin
    TdxfHeader(AObj).HeadVarsStruct := AReader.Conv.PHeadVarStruct;
  end
  else
  if AObj.ClassType = TdxfEntities then
  begin
    TdxfEntities(AObj).ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    TdxfEntities(AObj).FPapers := AReader.FPapers;
    ForceBlockRecords(AReader.Conv);
    if AReader.FPapers.Count <= 0 then
    begin
      vBlock := AReader.CreateBlock(sModelSpace);
      AReader.Conv.Sections[csBlocks].AddEntity(vBlock);
      AReader.InitPaper(vBlock);
    end;
    if AReader.FPapers.Count <= 1 then
    begin
      vBlock := AReader.CreateBlock(sPaperSpace);
      AReader.Conv.Sections[csBlocks].AddEntity(vBlock);
      AReader.InitPaper(vBlock);
    end;
    if AReader.Conv.Sections[csLTypes] = nil then
      AReader.Conv.NewNamedTable(cnstTableLTYPE, TsgDXFTable);
    if AReader.Conv.Counts[csLTypes] = 0 then
    begin
      AReader.Conv.NewNamedEntity(AReader.Conv.Sections[csLTypes], TsgDXFLineType, sByLayer);
    end;
  end
  else
  if AObj.ClassType = TdxfBlocks then
  begin
    AObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    AObj.FEnt := AReader.Conv.Sections[csBlocks];
    ForceBlockRecords(AReader.Conv);
  end
  else
  if AObj.ClassType = TdxfTables then
  begin
    AObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    AObj.FEnt := AReader.Conv.Sections[csTables];
  end
  else
  if AObj.ClassType = TdxfClasses then
  begin
    AObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    AObj.FEnt := AReader.Conv.Sections[csClasses];
  end
  else
  if AObj.ClassType = TdxfObjects then
  begin
    AObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    AObj.FEnt := AReader.Conv.Sections[csObjects];
    FSectionObjects := AObj;
  end;
  if AObj is TdxfCustomSection then
    TdxfCustomSection(AObj).FReader := AReader;
end;

procedure TdxfMain.ReadEntities(AReader: TdxfReader);
var
  S: string;
begin
  while not AReader.EOF do
  begin
    S := Trim(AReader.UValue);
    if S = cnstSection then
    begin
      AReader.Next;
      if (AReader.Code = Name_2) and not AReader.EOF then
        ReadEntity(AReader, DoReadHash(AReader.StrValue));
    end
    else
    begin
      if (AReader.Code = Info_0) and (S = 'BLOCK') then
        FObj := CreateSubEntity(AReader, hshSectionBLOCKS)
      else
        if S = cnstTable then
          FObj := CreateSubEntity(AReader, hshSectionTABLES);
      if Assigned(FObj) then
      try
        FObj.ReadEntities(AReader);
        if Assigned(FObj.FEnt) then
          if not AddEntity(FObj) then
            FObj.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
      finally
        DestroyObj;
      end
      else
        if S = cnstEndSec then // invalid file structure!
        begin
          ReadEntity(AReader, hshEndSec);
          if AReader.UValue <> cnstSection then
            inherited ReadEntities(AReader);
        end
        else
          inherited ReadEntities(AReader);
    end;
  end;
end;

{ TdxfCustomSection }

procedure TdxfCustomSection.Commit(const AReader: TdxfReader);
var
  vEnt: TsgDXFEntity;
begin
  vEnt := ReleaseEntity;
  inherited Commit(AReader);
  FEnt := vEnt;
  FComplex := True;
end;

constructor TdxfCustomSection.Create;
begin
  inherited Create;
  FComplex := True;
end;

function TdxfCustomSection.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := AObj.ClassType = TdxfEndSeq;
end;

{ TdxfTables }

function TdxfTables.CreateSubEntity(const AReader: TdxfReader;
  ANameHash: Cardinal): TdxfObject;
var
  I: Integer;
  vTable: TsgDXFEntity;
  vTableName: string;
begin
  Result := inherited CreateSubEntity(AReader, ANameHash);
  vTableName := AReader.UValue;
  vTable := FEnt.FindEntByName(vTableName);
  if Assigned(vTable) then
  begin
    Result.ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
    Result.FEnt := vTable;
    Result.FAdded := True;
  end;
  Result.Name := vTableName;
  if Result is TdxfTable then
    for I := 0 to Result.FEnt.Count - 1 do
      TdxfTable(Result).SortEntList.AddObject(Result.FEnt[I].Name, Result.FEnt[I]);
end;

procedure TdxfTables.ReadEntities(AReader: TdxfReader);
var
  vDoBreak: Boolean;
begin
  vDoBreak := False;
  while not AReader.EOF and not vDoBreak do
  begin
    if AReader.FNameHash = hshTable then
    begin
      AReader.Next;
      if (AReader.Code = Name_2) and not AReader.EOF then
        vDoBreak := ReadEntity(AReader, hshTable);
    end
    else
      if AReader.FNameHash = hshEndSec then
        vDoBreak := ReadEntity(AReader, AReader.FNameHash)
      else
        if (AReader.Code = 0) and (AReader.FNameHash = hshSection) then // invalid file structure!!!
          vDoBreak := True
        else
        begin
          inherited ReadEntities(AReader);
          vDoBreak := True;
        end;
  end;
end;

{ TdxfEntities }

function TdxfEntities.AddEntity(const E: TdxfObject): Boolean;
begin
  Result := TsgDXFBlock(FPapers[E.FPaperSpace]).AddEntity(E.FEnt) >= 0;
end;

{ TdxfObjects }

function TdxfObjects.AddEntity(const E: TdxfObject): Boolean;
begin
  if E.FAdded then
    Result := True
  else
    Result := E.OwnerAdd >= 0;
end;

function TdxfObjects.CreateSubEntity(const AReader: TdxfReader;
  ANameHash: Cardinal): TdxfObject;
var
  I: Integer;
  vMetaObj: TdxfReaderMetaclass;
  vClass: TdxfObjectClass;
begin
  if AReader.FClasses.FindItem(ANameHash, I) then
  begin
    TObject(vMetaObj) := AReader.FClasses[I];
    vClass := TdxfObjectClass(vMetaObj.ClassType);
    Result := vClass.Create;
    if Assigned(vMetaObj.EntClass) then
    begin
      Result.FEnt := vMetaObj.EntClass.Create;
      AReader.DoCreate(Result.FEnt);
    end;
  end
  else
  begin
    Result := TdxfCustomDictionaryItem.Create;
    Result.ReleaseEntity;
  end;
  Result.FOwner := Self;
end;

{ TdxfXRecord }

procedure TdxfXRecord.AddExData(const AReader: TdxfReader; AData: TsgCADExtendedData);
const
  cnstXrecBinDataSize = 127;
  cnstXrecStrLen = cnstXrecBinDataSize * 2;
var
  vCode: SmallInt;
  I, C, vTailLen: Integer;
  S: string;
  vItemData: TsgExtData;
  vHandle: UInt64;
  vErr: Integer;
begin
  vCode := AReader.Code;
  case CodeTypes^[vCode] of
    ctInteger:
      case vCode of
        280..282, 284, 290..299: AData.AddByte(vCode, AReader.IntValue);
        60..79, 170..179, 270..279, 283, 285..289, 370..389, 400..409, 1060..1070:
          AData.AddInt16(vCode, AReader.IntValue);
      else
        AData.AddInt(vCode, AReader.IntValue);
      end;
    ctDouble:
      begin
        case vCode of
          10..19:   AData.AddPoint(vCode, MakeFPoint(AReader.FloatValue, 0, 0));
          210..219: AData.AddPoint(vCode, MakeFPoint(AReader.FloatValue, 0, 0));
          20..39:
            begin
              vItemData := AData.Data[AData.DataCount - 1];
              if vItemData.EType = edtF3DPoint then
                PFPoint(vItemData.EData)^.V[vCode div 10 - 1] := AReader.FloatValue
              else
                AData.AddDouble(vCode, AReader.FloatValue);
            end;
          220..239: PFPoint(AData.Data[AData.DataCount - 1].EData)^.V[(vCode - 200) div 10 - 1] := AReader.FloatValue;
        else
          AData.AddDouble(vCode, AReader.FloatValue);
        end;
      end;
    ctString:
      begin
        case vCode of
          300: // ?need test other codes
            begin
              S := DXFStringToString(AReader.UValue);
              C := Length(S) div cnstXrecStrLen;
              I := 0;
              while I < C do
              begin
                AData.AddString(vCode, Copy(S, (I * cnstXrecStrLen) + 1, cnstXrecStrLen));
                Inc(I);
              end;
              vTailLen := Length(S) - C * cnstXrecStrLen;
              if vTailLen > 0 then
                AData.AddString(vCode, Copy(S, (I * cnstXrecStrLen) + 1, vTailLen));
            end;
        else
          AData.AddString(vCode, AReader.UValue);
        end;
      end;
    ctHex:
      case vCode of
        160..169:
          begin
            Val(AReader.UValue, vHandle, vErr);
            AData.AddInt64(vCode, vHandle);
          end
      else
        AData.AddInt64(vCode, AReader.HandleValue);
      end;
  else
    case vCode of
      310, 311: AData.AddBinary(vCode, AnsiString(AReader.StrValue));
    //else
      //raise EError.CreateFmt('Unknown XRecord(%x) code "%d"', [FHandle, vCode]);
    end;
  end;
end;

procedure TdxfXRecord.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Name_2:
      begin
        if FStartData then
          AddExData(AReader, TsgDXFXRecord(FEnt).Data)
        else
          inherited ReadProperty(AReader);
      end;
    100:
      begin
        inherited ReadProperty(AReader);
        FStartData := AReader.IsValueEqual(sAcDbXrecord);
        AReader.Next;
        if AReader.Code <> 280 then
          ReadProperty(AReader);
      end;
    0:
      begin
        inherited ReadProperty(AReader);
        FStartData := False;
      end;
    1001:
      begin
        inherited ReadProperty(AReader);
        FStartData := AReader.Code <> 0;
      end
  else
    inherited ReadProperty(AReader);
    if FStartData then
      AddExData(AReader, TsgDXFXRecord(FEnt).Data);
  end;
end;

{ TdxfAcdsRecord }

constructor TdxfAcdsRecord.Create;
begin
  inherited Create;
  FStartData := True;
end;

procedure TdxfAcdsRecord.Commit(const AReader: TdxfReader);
const
  cnstThumbnail_Data = 'Thumbnail_Data';// preview for layout (png)
var
  I, J: Integer;
  vD: TsgCADExtendedDataAccess;
  vDataTypeName: string;
  vHandle: UInt64;
  vEnt: TsgBrepModAcis;
  S: TMemoryStream;
  vData: TsgExtData;
  vFindHandle: Boolean;
begin
  inherited Commit(AReader);
  vD := TsgCADExtendedDataAccess(TsgDXFXRecord(FEnt).Data);
  I := 0;
  while (I < vD.DataCount) and not ((vD.DataType[I] = edtString) and (vD.DataString[I] = cnstAcDbDs_ID)) do Inc(I);
  vFindHandle := False;
  vHandle := cnstBadHandle;
  while (I < vD.DataCount) and not vFindHandle do
  begin
    if vD.DataType[I] = edtInt64 then
    begin
      vHandle := vD.DataInt64[I];
      if vHandle <> cnstBadHandle then
        Inc(vFindHandle)
      else
        Inc(I);
    end
    else
      if (vD.DataType[I] = edtString) and (vD.DataCode[I] = 9) then
      begin
        vHandle := StringHexToIntDef(vD.DataString[I], 0);
        if vHandle <> cnstBadHandle then
          Inc(vFindHandle)
        else
          Inc(I);
      end
      else
        Inc(I);
  end;
  if (I < vD.DataCount) and vFindHandle then
  begin
    Inc(I);
    vDataTypeName := '';//'ASM_Data'
    if (I < vD.DataCount) and (vD.DataType[I] = edtString) then
      vDataTypeName := vD.DataString[I];
    if (vDataTypeName <> cnstThumbnail_Data) and AReader.FindObject(vHandle, J) then
    begin
      vEnt := TsgBrepModAcis(AReader.DataBase[J]);
      while (I < vD.DataCount) and (vD.DataType[I] <> edtBinary) do Inc(I);
      S := vEnt.QueryAcisStream;
      while (I < vD.DataCount) and (vD.DataType[I] = edtBinary) do
      begin
        vData := vD.Data[I];
        S.Write(vData.EBytes^, vData.ECount);
        Inc(I);
      end;
    end;
  end;
  ReleaseEntity{$IFNDEF SG_FM_MOBILE}.Free{$ENDIF};
end;

procedure TdxfAcdsRecord.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
end;

{ TdxfMesh }

procedure TdxfMesh.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFMesh(FEnt).Vertices := FVertexArray;
  TsgDXFMesh(FEnt).VertexIndices := FFaceArray;
end;

constructor TdxfMesh.Create;
begin
  inherited Create;
  FVertexArray := TFPointList.Create;
  FFaceArray := TsgIntegerList.Create;
  FEdgeArray := TsgIntegerList.Create;
  FIndexFace := 0;
  FIndexVertex := 0;
  FMode := 0;
end;

destructor TdxfMesh.Destroy;
begin
  FEdgeArray.Free;
  FFaceArray.Free;
  FVertexArray.Free;
  inherited Destroy;
end;

procedure TdxfMesh.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Integer_92: FVertexArray.Count := AReader.IntValue;
    Integer_93:
      begin
        FFaceArray.Count := AReader.IntValue;
        FMode := 1;
      end;
    Integer_94:
      begin
        FEdgeArray.Count := AReader.IntValue;
        FMode := 2;
      end;
    (*95: //Edge crease count of level 0
      inherited ReadProperty(AReader); *)
    Integer_90:
      begin
        case FMode of
          1:
            begin
              FFaceArray[FIndexFace] :=  AReader.IntValue;
              Inc(FIndexFace);
            end;
          2:
            begin
              AReader.IntValue;
            end;
          else
            inherited ReadProperty(AReader);
        end;
      end;
    XFirst_10: FVertexArray.List[FIndexVertex].X :=  AReader.FloatValue;
    YFirst_20: FVertexArray.List[FIndexVertex].Y :=  AReader.FloatValue;
    ZFirst_30:
      begin
        FVertexArray.List[FIndexVertex].Z :=  AReader.FloatValue;
        Inc(FIndexVertex);
      end
    else
      inherited ReadProperty(AReader);
  end;
end;

{ TdxfRay }

procedure TdxfRay.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFRay(FEnt).StartPoint := GetPoints(0);
  TsgDXFRay(FEnt).Direction := GetPoints(1);
end;

{ TdxfScale }

procedure TdxfScale.ReadProperty(const AReader: TdxfReader);
var
  S: TsgDXFScaleAccess;
begin
  S := TsgDXFScaleAccess(FEnt);
  case AReader.Code of
    140:  S.LayoutScale := AReader.FloatValue;
    141:  S.ModelScale := AReader.FloatValue;
    290:  S.Flags := AReader.IntValue;
    300:  S.Description := AReader.UValue;
    else  inherited ReadProperty(AReader);
  end;
end;

{ TsgDXFPaperBlocksRefs }

function TsgDXFPaperBlocksRefs.FindBlock(const ABlock: TsgDXFBlock; var Index: Integer): Boolean;
begin
  Result := False;
  Index := FHandlers.Count - 1;
  while (Index >= 0) and not Result do
    if TsgPaperSpaceBlockListNotifyHandler(FHandlers.Objects[Index]).Block = ABlock then
      Inc(Result)
    else
      Dec(Index);
end;

procedure TsgDXFPaperBlocksRefs.ListNotify(const Obj: TObject; Action: TListNotification);
var
  I: Integer;
  vObj: TObject;
begin
  case Action of
    lnAdded:
      begin
        if not FindBlock(TsgDXFBlock(Obj), I) then
          FHandlers.AddObject(TsgDXFBlock(Obj).Name, TsgPaperSpaceBlockListNotifyHandler.Create(TsgDXFBlock(Obj)));
      end;
    lnExtracted, lnDeleted:
      begin
        if FindBlock(TsgDXFBlock(Obj), I) then
        begin
          vObj := FHandlers.Objects[I];
          FHandlers.Objects[I] := nil;
          vObj.Free;
          FHandlers.Delete(I);
        end;
      end;
  end;
end;

procedure TsgDXFPaperBlocksRefs.UpdateViewportsOrdIndex;
var
  I: Integer;
begin
  for I := 0 to FHandlers.Count - 1 do
    TsgPaperSpaceBlockListNotifyHandler(FHandlers.Objects[I]).UpdateViewportOrdIndex;
end;

constructor TsgDXFPaperBlocksRefs.Create;
begin
  inherited Create;
  FHandlers := TStringList.Create;
  FHandlers.Duplicates := dupAccept;
end;

destructor TsgDXFPaperBlocksRefs.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

{ TsgPaperSpaceBlockListNotifyHandler }

constructor TsgPaperSpaceBlockListNotifyHandler.Create(const ABlock: TsgDXFBlock);
begin
  inherited Create;
  FBlock := ABlock;
  FListNotify := TsgDXFBlockAccess(Block).OnListNotify;
  TsgDXFBlockAccess(Block).OnListNotify := ListNotifyHandler;
end;

destructor TsgPaperSpaceBlockListNotifyHandler.Destroy;
begin
  FViewportWithMinID := nil;
  TsgDXFBlockAccess(Block).OnListNotify := FListNotify;
  FListNotify := nil;
  FBlock := nil;
  inherited Destroy;
end;

procedure TsgPaperSpaceBlockListNotifyHandler.DoListNotify(const Obj: TObject; Action: TListNotification);
begin
  if Assigned(FListNotify) then
    FListNotify(Obj, Action);
end;

function TsgPaperSpaceBlockListNotifyHandler.UpdateViewportOrdIndex: Boolean;
var
  I: Integer;
begin
  Result := False;
  if (TsgDXFBlockAccess(Block).FViewPortWithMinIndex <> nil) and (FViewportWithMinID <> nil) then
  begin
    if (TsgDXFBlockAccess(Block).FViewPortWithMinIndex <> FViewportWithMinID) and (FViewportWithMinID.ThisID = 1) then
    begin
      I := FViewportWithMinID.OrdIndex;
      FViewportWithMinID.OrdIndex := TsgDXFBlockAccess(Block).FViewPortWithMinIndex.OrdIndex;
      TsgDXFBlockAccess(Block).FViewPortWithMinIndex.OrdIndex := I;
      SwapObjects(TObject(TsgDXFBlockAccess(Block).FViewPortWithMinIndex), TObject(FViewportWithMinID));
      Result := True;
    end;
  end;
end;

procedure TsgPaperSpaceBlockListNotifyHandler.ListNotifyHandler(const Obj: TObject; Action: TListNotification);
begin
  case Action of
    lnAdded:
      begin
        DoListNotify(Obj, Action);
        if TsgDXFEntity(Obj).EntType = ceViewport then
        begin
          if FViewportWithMinID = nil then
            FViewportWithMinID := TsgDXFViewport(Obj)
          else
            if TsgDXFViewport(Obj).ThisID < FViewportWithMinID.ThisID then
              FViewportWithMinID := TsgDXFViewport(Obj);
        end;
      end;
    lnDeleted, lnExtracted:
      begin
        if FViewportWithMinID = TsgDXFViewport(Obj) then
          FViewportWithMinID := nil;
        DoListNotify(Obj, Action);
      end;
  end;
end;

{ TdxfSpatialFilter }

procedure TdxfSpatialFilter.Commit(const AReader: TdxfReader);

  function MakeMatrix(const AArray: PsgDoubleArray): TFMatrix;
  begin
    Result.V[0] := MakeFPoint(AArray^[0], AArray^[4], AArray^[8]);
    Result.V[1] := MakeFPoint(AArray^[1], AArray^[5], AArray^[9]);
    Result.V[2] := MakeFPoint(AArray^[2], AArray^[6], AArray^[10]);
    Result.V[3] := MakeFPoint(AArray^[3], AArray^[7], AArray^[11]);
  end;

begin
  inherited Commit(AReader);
  TsgCADSpatialFilter(FEnt).Clipbdorg := FClipbdorg;
  TsgCADSpatialFilter(FEnt).Extrusion := FExtrision;
  if FMatIndex >= 12 then
  begin
    TsgCADSpatialFilter(FEnt).InvBlkTransform := MakeMatrix(@FMatrices);
    if FMatIndex >= 24 then
      TsgCADSpatialFilter(FEnt).Clip := MakeMatrix(PsgDoubleArray(@FMatrices[12]));
  end;
end;

constructor TdxfSpatialFilter.Create;
begin
  inherited Create;
  FExtrision := cnstExtrusion;
end;

procedure TdxfSpatialFilter.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    Integer_70:
      begin
        TsgCADSpatialFilter(FEnt).Bounds.Count := AReader.IntValue;
        FPoints := TsgCADSpatialFilter(FEnt).Bounds.List;
      end;
    XFirst_10: FPoints^[FPointIndex].X := AReader.FloatValue;
    YFirst_20:
      begin
        FPoints^[FPointIndex].Y := AReader.FloatValue;
        Inc(FPointIndex);
      end;
    XOther_11: FClipbdorg.X := AReader.FloatValue;
    YOther_21: FClipbdorg.Y := AReader.FloatValue;
    ZOther_31: FClipbdorg.Z := AReader.FloatValue;
    Extrusion_210: FExtrision.X := AReader.FloatValue;
    Extrusion_220: FExtrision.Y := AReader.FloatValue;
    Extrusion_230: FExtrision.Z := AReader.FloatValue;
    Integer_71: TsgCADSpatialFilter(FEnt).DisplayBounds := AReader.IntValue = 1;
    Integer_72:
      begin
        TsgCADSpatialFilter(FEnt).FrontClip := AReader.IntValue = 1;
        if TsgCADSpatialFilter(FEnt).FrontClip then
        begin
          FFrontClipNeeded := True;
          ReadProps(AReader);
        end;
      end;
    Integer_73: TsgCADSpatialFilter(FEnt).BackClip := AReader.IntValue = 1;
    Float_40:
      begin
        if FFrontClipNeeded then
        begin
          FFrontClipNeeded := False;
          TsgCADSpatialFilter(FEnt).FrontDist := AReader.FloatValue;
        end
        else
        begin
          FMatrices[FMatIndex] := AReader.FloatValue;
          Inc(FMatIndex);
        end;
      end;
    Float_41: TsgCADSpatialFilter(FEnt).BackDist := AReader.FloatValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TsgQObjectsList }

function TsgQObjectsList.AddItem(const AItem: TObject): Integer;
begin
  Sorted := True;
  if not CustomFindItem(AItem, Result) then
    InsertFinded(Result, AItem);
end;

constructor TsgQObjectsList.Create;
begin
  inherited Create;
  SetLength(FList, 1024);
end;

function TsgQObjectsList.CustomFindItem(const AItem: TObject;
  var Index: Integer): Boolean;
begin
  Result := QFindArrayItem(FList, 0, FCount - 1, AItem, sgComparePointers, Index);
end;

procedure TsgQObjectsList.Delete(Index: Integer);
begin
  Dec(FCount);
{$IFDEF AUTOREFCOUNT}
  FList[Index] := nil;
{$ENDIF}
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Pointer));
end;

procedure TsgQObjectsList.Clear;
begin
  while FCount > 0 do
    Delete(FCount - 1);
  Finalize(FList);
end;

destructor TsgQObjectsList.Destroy;
begin
  Clear;
  FSearchItem.Free;
  inherited Destroy;
end;

function TsgQObjectsList.GetObject(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

procedure TsgQObjectsList.InsertFinded(Index: Integer;
  const Item: TObject);
var
  Capacity: Integer;
begin
  Capacity := Length(FList);
  if FCount = Capacity then
    SetLength(FList, ListGrow(Capacity));
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Pointer));
{$IFDEF AUTOREFCOUNT}
  Pointer(FList[Index]) := nil;
{$ENDIF}
  FList[Index] := Item;
  Inc(FCount);
end;

procedure TsgQObjectsList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

procedure TsgQObjectsList.Sort;
begin
  FSorted := True;
  if FCount > 1 then
    QSortArrayBounds(Pointer(FList), sgComparePointers, 0, FCount - 1);
end;

{ TsgQReaderMetaclsssList }

function CmpMetaclassObj(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpUInt64(TdxfReaderMetaclass(A).Handle, TdxfReaderMetaclass(B).Handle);
end;

constructor TsgQReaderMetaclsssList.Create;
begin
  inherited Create;
  FSearchItem := TdxfReaderMetaclass.Create;
end;

procedure TsgQReaderMetaclsssList.Sort;
begin
  FSorted := True;
  if FCount > 1 then
    QSortArrayBounds(Pointer(FList), CmpMetaclassObj, 0, FCount - 1);
end;

function TsgQReaderMetaclsssList.CustomFindItem(const AItem: TObject;
  var Index: Integer): Boolean;
begin
  Result := QFindArrayItem(FList, 0, FCount - 1, AItem, CmpMetaclassObj, Index);
end;

function TsgQReaderMetaclsssList.FindItem(AHandle: UInt64;
  var Index: Integer): Boolean;
begin
  TdxfReaderMetaclass(FSearchItem).Handle := AHandle;
  Result := CustomFindItem(FSearchItem, Index);
end;

{ TsgQCADItemsList }

function CmpEntHandle(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpUInt64(TsgDXFEntity(A).Handle, TsgDXFEntity(B).Handle);
end;

constructor TsgQCADItemsList.Create;
begin
  inherited Create;
  FSearchItem := TsgDXFOwneredItem.Create;
end;

function TsgQCADItemsList.CustomFindItem(const AItem: TObject;
  var Index: Integer): Boolean;
begin
  Result := QFindArrayItem(FList, 0, FCount - 1, AItem, CmpEntHandle, Index);
end;

function TsgQCADItemsList.FindItem(AHandle: UInt64;
  var Index: Integer): Boolean;
begin
  TsgDXFEntity(FSearchItem).Handle := AHandle;
  Result := CustomFindItem(FSearchItem, Index);
end;

procedure TsgQCADItemsList.Sort;
begin
  FSorted := True;
  if FCount > 1 then
    QSortArrayBounds(Pointer(FList), CmpEntHandle, 0, FCount - 1);
end;

{ TdxfObjectContextData }

procedure TdxfObjectContextData.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgDXFObjectContextData(FEnt).Scale := TsgDXFScale(AReader.ObjByHandle(FScale, TsgDXFScale));
end;

procedure TdxfObjectContextData.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    340: FScale := AReader.HandleValue;
  end;
end;

{ TdxfBlkRefObjectContextData }

procedure TdxfBlkRefObjectContextData.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  TsgCADBlkRefObjectContextData(FEnt).Angle := FAngle;
  TsgCADBlkRefObjectContextData(FEnt).Point := FPoint;
  TsgCADBlkRefObjectContextData(FEnt).ScaleFactor := FScaleFactor;
end;

procedure TdxfBlkRefObjectContextData.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    10,20,30: AReader.Coord(PFPointArray(@FPoint));
    41..43: FScaleFactor.V[AReader.Code - 41] := AReader.FloatValue;
    50: FAngle := AReader.FloatValue;
  end;
end;

{ TdxfBlocks }

function TdxfBlocks.AddEntity(const E: TdxfObject): Boolean;
begin
  Result := inherited AddEntity(E);
  if Result and Assigned(FReader) then
  begin
    if not Assigned(FReader.FSections[csBlockRecords]) then
    begin
      FReader.FSections[csBlockRecords] := TsgStringList.Create;
      TsgStringList(FReader.FSections[csBlockRecords]).Duplicates := dupIgnore;
      TsgStringList(FReader.FSections[csBlockRecords]).CaseSensitive := False;
      TsgStringList(FReader.FSections[csBlockRecords]).Sorted := True;
    end;
    TsgDXFBlockRecordAccess(TsgDXFBlock(E.FEnt).BlockRecord).RemoveDestroyNotification(FReader.BlockRecordDestroy);
    FReader.FSections[csBlockRecords].AddObject(TsgDXFBlock(E.FEnt).BlockRecord.Name, TsgDXFBlock(E.FEnt).BlockRecord);
    TsgDXFBlockRecordAccess(TsgDXFBlock(E.FEnt).BlockRecord).AddDestroyNotification(FReader.BlockRecordDestroy);
  end;
end;

function TdxfBlocks.EOFEnt(AObj: TdxfObject): Boolean;
begin
  Result := inherited EOFEnt(AObj);
  if not Result then
    Result := AObj.ClassType = TdxfEndBlock;
end;

{ TdxfMLeaderAnnoContext }

function TdxfMLeaderAnnoContext.BeginGroup(AClass: TsgCustomMLeaderItemClass;
  const AReadMethod: TdxfReadProperty; out obj): Integer;
begin
  Result := Push(AReadMethod);
  TsgCustomMLeaderItem(obj) := AClass.Create;
end;

constructor TdxfMLeaderAnnoContext.Create;
begin
  inherited Create;
  FMatrix := TsgDoubleList.Create;
end;

destructor TdxfMLeaderAnnoContext.Destroy;
begin
  FMatrix.Free;
  inherited Destroy;
end;

procedure TdxfMLeaderAnnoContext.EndContext;
var
  M: TFMatrix;
begin
  if FMatrix.Count = 16 then
  begin
    M.V[0] := PFPoint(@FMatrix.List^[0])^;
    M.V[1] := PFPoint(@FMatrix.List^[4])^;
    M.V[2] := PFPoint(@FMatrix.List^[8])^;
    M.V[3] := PFPoint(@FMatrix.List^[12])^;
    FItem.BlkMatrix := M;
  end;
  FMatrix.Count := 0;
  FItem := nil;
  Pop; // end context;
end;

procedure TdxfMLeaderAnnoContext.LeaveGroup(const AReader: TdxfReader);
var
  vStackReadPropertyCount: Integer;
begin
  vStackReadPropertyCount := FStackReadPropertyCount;
  try
    Dec(FStackReadPropertyCount);
    FStackReadProperty[FStackReadPropertyCount](AReader);
  finally
    FStackReadPropertyCount := vStackReadPropertyCount;
  end;
end;

procedure TdxfMLeaderAnnoContext.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    300:
      begin
        FItem := TsgMLeaderContextDataAccess(TsgCADMLeaderAnnotContextAccess(FEnt).Item);
        Push(ReadContext); //"CONTEXT_DATA{"
      end;
    301: EndContext;
  else
    inherited ReadProperty(AReader);
  end;
end;

procedure TdxfMLeaderAnnoContext.ReadProps(AReader: TdxfReader);
begin
  repeat
    AReader.Next;
    FReadProperty(AReader);
  until (AReader.Code = Info_0) or
    ((AReader.Code = 301) and (FOwner.ClassType = TdxfMLeader));
end;

procedure TdxfMLeaderAnnoContext.SkipCode(const AReader: TdxfReader);
begin
end;

procedure TdxfMLeaderAnnoContext.ReadLineGroup(const AReader: TdxfReader);
var
  I: Integer;
begin
  case AReader.Code of
    305:
      begin
        FMLeader.AddMLeaderLine(FLine);
        FLine := nil;
        Pop;
      end;
    10:
      begin
        I := FLine.Points.Add(cnstFPointZero);
        DoCoord(AReader, @FLine.Points.List^[I].V[0]);
      end;
    20,30: DoCoord(AReader, @FLine.Points.List^[FLine.Points.Count - 1].V[0]);
    90: FLine.SegmentIndex := AReader.IntValue;
    91: FLine.Idx := AReader.IntValue;
    11: DoCoord(AReader, @PsgMLeaderBreakLine(FLine.Item(FLine.AddStartEnd(cnstLine)))^.Point1.V[0]);
    21,31: DoCoord(AReader, @PsgMLeaderBreakLine(FLine.Item(FLine.StartEndCount - 1))^.Point1.V[0]);
    12,22,32: DoCoord(AReader, @PsgMLeaderBreakLine(FLine.Item(FLine.StartEndCount - 1))^.Point2.V[0]);
  else
    SkipCode(AReader);
  end;
end;

procedure TdxfMLeaderAnnoContext.ReadMLeaderNodeGroup(const AReader: TdxfReader);
begin
  case AReader.Code of
    304: BeginGroup(TsgMLeaderLine, ReadLineGroup, FLine);//"LEADER_LINE{"
    303:
      begin
        FItem.AddMLeader(FMLeader);
        FMLeader := nil;
        Pop;
      end;
    90: FMLeader.Idx := AReader.IntValue;
    10,20,30: DoCoord(AReader, @FMLeader.ConnectionPoint.V[0]);
    11,21,31: DoCoord(AReader, @FMLeader.Direction.V[0]);
    12: DoCoord(AReader, @PsgMLeaderBreakLine(FMLeader.Item(FMLeader.AddStartEnd(cnstLine)))^.Point1.V[0]);
    22,32: DoCoord(AReader, @PsgMLeaderBreakLine(FMLeader.Item(FMLeader.StartEndCount - 1))^.Point1.V[0]);
    13,23,33: DoCoord(AReader, @PsgMLeaderBreakLine(FMLeader.Item(FMLeader.StartEndCount - 1))^.Point2.V[0]);
    40: FMLeader.LandingDistance := AReader.FloatValue;
    290:;// Has Set Last Leader Line Point
    291:;// Has Set Dogleg Vector
    271: FMLeader.IsVertical := AReader.IntValue = 1;
  else
    SkipCode(AReader);
  end;
end;

procedure TdxfMLeaderAnnoContext.ReadContext(const AReader: TdxfReader);
begin
  case AReader.Code of
    40:  FItem.OverallScale := AReader.FloatValue;
    10,20,30: DoCoord(AReader, @FItem.ContentBasePoint.V[0]);
    41:  FItem.TextHeight := AReader.FloatValue;
    140: FItem.ArrowHeadSize := AReader.FloatValue;
    145: FItem.LandingGap := AReader.FloatValue;
    174: FItem.LeftAttachment := AReader.IntValue;
    175: FItem.RightAttachment := AReader.IntValue;
    176: FItem.TextAlignType := AReader.IntValue;
    177: FItem.AttachmentType := AReader.IntValue;

    290: FItem.HasTextContents := AReader.IntValue = 1;
    // --------------- has text content ---------------
    304: FItem.TextLabel := AReader.UValue;
    11,21,31: DoCoord(AReader, @FItem.TextNormal.V[0]);
    340: FItem.TextStyle := TsgDXFStyle(AReader.ObjByHandle(AReader.HandleValue, TsgDXFStyle));
    12,22,32: DoCoord(AReader, @FItem.TextLocation.V[0]);
    13,23,33: DoCoord(AReader, @FItem.TextDirection.V[0]);
    42:  FItem.TextRotation := AReader.FloatValue;// radians?
    43:  FItem.BoundaryWidth := AReader.FloatValue;
    44:  FItem.BoundaryHeight := AReader.FloatValue;
    45:  FItem.LineSpacingFactor := AReader.FloatValue;
    170: FItem.LineSpacingStyle := AReader.IntValue;
    90:  FItem.TextColor := CmEntityColorToColorCAD(AReader.IntValue);
    171: FItem.Alignment := AReader.IntValue;
    172: FItem.FlowDirection := AReader.IntValue;
    91:  FItem.BackgroundColor := CmEntityColorToColorCAD(AReader.IntValue);
    141: FItem.BackgroundScaleFactor := AReader.FloatValue;
    92:  FItem.BackgroundTransparency := AReader.IntValue;
    291: FItem.IsBackgroundEnabled := AReader.IntValue = 1;
    292: FItem.IsBackgroundMaskFillOn := AReader.IntValue = 1;
    173: FItem.ColumnType := AReader.IntValue;
    293: FItem.IsTextHeightAutomatic := AReader.IntValue = 1;
    142: FItem.ColumnWidth := AReader.FloatValue;
    143: FItem.ColumnGutter := AReader.FloatValue;
    294: FItem.ColumnFlowReversed := AReader.IntValue = 1;
    144: FItem.ColumnSizes.Add(AReader.FloatValue);
    295: FItem.WordBreak := AReader.IntValue = 1;
    // --------------- has text content ---------------

    296: FItem.HasContentsBlock := AReader.IntValue = 1;
    // --------------- has block content ---------------
    341: FItem.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    14,24,34: DoCoord(AReader, @FItem.BlkNormal.V[0]);
    15,25,35: DoCoord(AReader, @FItem.BlkLocation.V[0]);
    16,26,36: DoCoord(AReader, @FItem.BlkScale.V[0]);
    46: FItem.BlkRotation := AReader.FloatValue;// radians?
    93: FItem.BlockColor := CmEntityColorToColorCAD(AReader.IntValue);
    47: FMatrix.Add(AReader.FloatValue);
    110,120,130: DoCoord(AReader, @FItem.BasePoint.V[0], 100);
    111,121,131: DoCoord(AReader, @FItem.BaseDirection.V[0], 100);
    112,122,132: DoCoord(AReader, @FItem.BaseVertical.V[0], 100);
    297: FItem.IsNormalReversed := AReader.IntValue = 1;
    // --------------- has block content ---------------

    301: EndContext; // end context;
    302: BeginGroup(TsgMLeader, ReadMLeaderNodeGroup, FMLeader);//"LEADER{"
  else
    SkipCode(AReader);
  end;
end;

{ TdxfMLeaderStyle }

procedure TdxfMLeaderStyle.ReadProperty(const AReader: TdxfReader);
var
  E: TsgCADMLeaderStyleAccess;
begin
  E := TsgCADMLeaderStyleAccess(FEnt);
  case AReader.Code of
    179: FObjVersion := AReader.IntValue; // Version (expected to have value 2)
    170: E.ContentType := AReader.IntValue;
    171: E.DrawMLeaderOrder := AReader.IntValue;
    172: E.DrawLeaderOrder := AReader.IntValue;
    90:  E.MaxNumberOfPoints := AReader.IntValue;
    40:  E.FirstSegmentAngle := AReader.FloatValue;
    41:  E.SecondSegmentAngle := AReader.FloatValue;
    173: E.LeaderLineType := AReader.IntValue;
    91:  E.ColorCAD := CmEntityColorToColorCAD(AReader.IntValue);
    340: E.LineType := TsgDXFLineType(AReader.ObjByHandle(AReader.HandleValue, TsgDXFLineType));
    92:  E.LineWeight := ExpandLWeight(AReader.IntValue);
    290: E.LandingEnabled := AReader.IntValue = 1;
    42:  E.LandingGap := AReader.FloatValue;
    291: E.DogLegEnabled := AReader.IntValue = 1;
    43:  E.LandingDistance := AReader.FloatValue;
    3:   E.StyleDescription := AReader.UValue;
    341: E.Arrow := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    44:  E.ArrowHeadSize := AReader.FloatValue;
    300: E.Text := AReader.UValue;
    342: E.TextStyle := TsgDXFStyle(AReader.ObjByHandle(AReader.HandleValue, TsgDXFStyle));
    174: E.LeftAttachment := AReader.IntValue;
    175: E.TextAngleType := AReader.IntValue;
    176: E.TextAlignmentType := AReader.IntValue;
    178: E.RightAttachment := AReader.IntValue;
    93:  E.TextColor := CmEntityColorToColorCAD(AReader.IntValue);
    45:  E.TextHeight := AReader.FloatValue;
    292: E.TextFrameEnabled := AReader.IntValue = 1;
    297: E.AlwaysAlignTextLeft := AReader.IntValue = 1;
    46:  E.AlignSpace := AReader.FloatValue;
    343: E.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    94:  E.BlockColor := CmEntityColorToColorCAD(AReader.IntValue);
    47:  PDouble(@E.BlockScale.X)^ := AReader.FloatValue;
    49:  PDouble(@E.BlockScale.Y)^ := AReader.FloatValue;
    140: PDouble(@E.BlockScale.Z)^ := AReader.FloatValue;
    293: E.IsBlockScaleEnabled := AReader.IntValue = 1;
    141: E.BlockRotation := AReader.FloatValue;
    294: E.IsBlockRotationEnabled := AReader.IntValue = 1;
    177: E.BlockConnectionType := AReader.IntValue;
    142: E.ScaleFactor := AReader.FloatValue;
    295:;// Property changed, meaning not totally clear, might be set to true
    //if something changed after loading, or might be used to trigger updates
    //in dependent MLeaders.
    296: E.Annotative := AReader.IntValue = 1;
    143: E.BreakSize := AReader.FloatValue;
    271: E.AttachmentDirection := AReader.IntValue;
    272: E.BottomAttachment := AReader.IntValue;
    273: E.TopAttachment := AReader.IntValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfMLeader }

procedure TdxfMLeader.ApplyEED(AReader: TdxfReader);
begin
  inherited ApplyEED(AReader);
  if Assigned(FVer) then
    EraseEED(FVer);
end;

procedure TdxfMLeader.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  SetLength(FLabels, FLabelsCount);
  TsgCADMultiLeaderAccess(FEnt).Labels := FLabels;
  SetLength(FArrows, FArrowsCount);
  TsgCADMultiLeaderAccess(FEnt).Arrows := FArrows;
end;

procedure TdxfMLeader.InitEEDAppName(AData: TsgCADExtendedData;
  AReader: TdxfReader);
begin
  inherited InitEEDAppName(AData, AReader);
  if AReader.UValue = sAcadMLeaderVer then
    FVer := AData;
end;

procedure TdxfMLeader.ReadContextData(const AReader: TdxfReader);
begin
  FObj := TdxfMLeaderAnnoContext.Create;
  try
    FObj.FEnt := TsgCADMultiLeaderAccess(FEnt).Context;
    FObj.FOwner := Self;
    FObj.FReadProperty(AReader);
    FObj.ReadProps(AReader);
    FObj.ReleaseEntity;
  finally
    FreeAndNil(FObj);
  end;
end;

procedure TdxfMLeader.ReadMLeaderProperty(const AReader: TdxfReader);
var
  E: TsgCADMultiLeaderAccess;

  function IntToMLeaderProps(const AValue: Integer): TsgDXFMLeaderProps;
  begin
    Result := TsgDXFMLeaderProps(AValue);
  end;

begin
  E := TsgCADMultiLeaderAccess(FEnt);
  case AReader.Code of
    270:; // Version (expected to be 2).
    340: E.Style := TsgCADMLeaderStyle(AReader.ObjByHandle(AReader.HandleValue, TsgCADMLeaderStyle));
    90:  E.OverrideFlags := IntToMLeaderProps(AReader.IntValue);
    170: E.Properties.LeaderLineType := AReader.IntValue;
    91:  E.Properties.ColorCAD := CmEntityColorToColorCAD(AReader.IntValue);
    341: E.Properties.LineType := TsgDXFLineType(AReader.ObjByHandle(AReader.HandleValue, TsgDXFLineType));
    171: E.Properties.LineWeight := AReader.FloatValue;
    290: E.Properties.LandingEnabled := AReader.IntValue = 1;
    291: E.Properties.DogLegEnabled := AReader.IntValue = 1;
    41:  E.Properties.LandingDistance := AReader.FloatValue;
    342: E.Properties.Arrow := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    42:  E.Properties.ArrowHeadSize := AReader.FloatValue;
    172: E.Properties.ContentType := AReader.IntValue;
    343: E.Properties.TextStyle := TsgDXFStyle(AReader.ObjByHandle(AReader.HandleValue, TsgDXFStyle));
    173: E.Properties.LeftAttachment := AReader.IntValue;
    95:  E.Properties.RightAttachment := AReader.IntValue;
    174: E.Properties.TextAngleType := AReader.IntValue;
    175: E.Properties.TextAlignmentType := AReader.IntValue;
    92:  E.Properties.TextColor := CmEntityColorToColorCAD(AReader.IntValue);
    292: E.Properties.TextFrameEnabled := AReader.IntValue = 1;
    344: E.Properties.BlockRecord := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
    93:  E.Properties.BlockColor := CmEntityColorToColorCAD(AReader.IntValue);
    10,20,30: DoCoord(AReader, @E.Properties.BlockScale.V[0]);
    43:  E.Properties.BlockRotation := AReader.FloatValue;
    176: E.Properties.ContentType := AReader.IntValue;
    293: E.Annotative := AReader.IntValue = 1;
    345:
      begin
        if Length(FArrows) = FArrowsCount then
          SetLength(FArrows, ListGrow(Length(FArrows)));
        Inc(FArrowsCount);
        FArrows[FArrowsCount - 1] := TsgDXFBlockRecord(AReader.ObjByHandle(AReader.HandleValue, TsgDXFBlockRecord));
      end;
    300: ReadContextData(AReader);
    330:
      begin
        if Length(FLabels) = FLabelsCount then
          SetLength(FLabels, ListGrow(Length(FLabels)));
        Inc(FLabelsCount);
        FLabels[FLabelsCount - 1].Attdef := TsgDXFAttdef(AReader.ObjByHandle(AReader.HandleValue, TsgDXFAttdef));
      end;
    177: FLabels[FLabelsCount - 1].ID := AReader.IntValue;
    44:  FLabels[FLabelsCount - 1].Width := AReader.FloatValue;
    302: FLabels[FLabelsCount - 1].Text := AReader.UValue;
    294: E.IsTextDirectionNegative := AReader.IntValue = 1;
    178: E.TextAlignIPE := AReader.IntValue;
    179: E.Justification := AReader.IntValue;
    45:  E.Properties.ScaleFactor := AReader.FloatValue;
    271: E.Properties.AttachmentDirection := AReader.IntValue;
    273: E.Properties.TopAttachment := AReader.IntValue;
    272: E.Properties.BottomAttachment := AReader.IntValue;
  else
    FStackReadProperty[FStackReadPropertyCount - 1](AReader);
  end;
end;

procedure TdxfMLeader.ReadProperty(const AReader: TdxfReader);
begin
  inherited ReadProperty(AReader);
  case AReader.Code of
    100:
      if AReader.UValue = sAcDbMLeader then
        Push(ReadMLeaderProperty);
    0:
      if FStackReadPropertyCount > 0 then
        Pop;
  end;
end;

{ TdxfGroup }

procedure TdxfGroup.Commit(const AReader: TdxfReader);
var
  I, J: Integer;
begin
  inherited Commit(AReader);
  for I := 0 to FItems.Count - 1 do
    if AReader.FindObject(FItems[I], J) then
      FEnt.AddEntity(TsgDXFEntity(AReader.DataBase[J]));
end;

constructor TdxfGroup.Create;
begin
  inherited Create;
  FItems := TsgInt64List.Create;
end;

destructor TdxfGroup.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TdxfGroup.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    340: FItems.Add(AReader.HandleValue);
    70: TsgCADGroup(FEnt).Unnamed := AReader.IntValue = 1;
    71: TsgCADGroup(FEnt).Selectable := AReader.IntValue = 1;
    300: FEnt.Description := AReader.UValue;
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TsgObjCollection }

function TsgObjCollection.GetObj(Handle: UInt64): TObject;
var
  I: Integer;
begin
  if FMap.FindItem(Handle, I) then
    Result := FMap[I]
  else
    Result := nil;
end;

{ TdxfEndSeq }

procedure TdxfEndSeq.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if FOwner is TdxfClasses then
    AReader.UpdateClasses(FOwner);
end;

{ TdxfProxyObject }

procedure TdxfProxyObject.UpdateOwner(AReader: TdxfReader);
begin
  inherited UpdateOwner(AReader);
  if Assigned(FSoftOwner) and (FSoftOwner.Handle = AReader.FTableStyles) then
    FSoftOwner := AReader.Conv.Sections[csObjects];
end;

{ TdxfCustomApplicationBlockReference }

procedure TdxfCustomApplicationBlockReference.Commit(const AReader: TdxfReader);
var
  vAttributes: IsgGlobalPropProvider;
begin
  inherited Commit(AReader);
  if (FSoftOwnerIDs.Count > 1) and GetGlobalPropProvider(FEnt, False, vAttributes) then
    vAttributes[cnstViewport] := {$IFNDEF SGDEL_6}sgUInt64AsVar{$ENDIF}(FSoftOwnerIDs.Last);
end;

{ TcstDictionary }

function TcstDictionary.AddItem(AReader: TdxfReader; const AHandle: UInt64;
  const AName: string): Integer;
begin
  Result := inherited AddItem(AReader, AHandle, AName);
  if AReader.FCSTSaved and (AReader.FXInfoHandle = cnstBadHandle) and (AName = 'XINFO') then
    FXInfoHandle := AHandle;
end;

procedure TcstDictionary.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  if AReader.FCSTSaved and (AReader.FXInfoHandle = cnstBadHandle) then
    AReader.FXInfoHandle := FXInfoHandle;
end;

{ TcstXRecord }

procedure TcstXRecord.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  AReader.UpdateXInfo(TsgDXFXRecord(FEnt));
end;

{ TdxfBlockR9 }

procedure TdxfBlockR9.ApplyHandle(const AReader: TdxfReader);
var
  vBlockrecordHandle: UInt64;
begin
  inherited ApplyHandle(AReader);
  vBlockrecordHandle := TsgDXFBlock(FEnt).BlockRecord.Handle;
  if AReader.Conv.CheckHandle(vBlockrecordHandle) then
    TsgDXFBlock(FEnt).BlockRecord.Handle := vBlockrecordHandle;
end;

{ TdxfCustomIrdRecord }

procedure TdxfCustomIrdRecord.Commit(const AReader: TdxfReader);
var
  vAttributes: IsgGlobalPropProvider;
  vVarData: TVarData;
  vArr: TVarArray;
begin
  inherited Commit(AReader);
  FillChar(vArr, SizeOf(vArr), 0);
  vArr.DimCount := 1;
  vArr.Flags := $0001;//FADF_AUTO;
  vArr.ElementSize := SizeOf(Word);
  vArr.Bounds[0].ElementCount := FDataSize div vArr.ElementSize;
  FillChar(vVarData, SizeOf(TVarData), 0);
  vVarData.VType := varByte or varArray;
  vVarData.VArray := @vArr;
  vArr.Data := FData;
  if GetGlobalPropProvider(FEnt, False, vAttributes) then
    vAttributes[cnst_ird_data] := Variant(vVarData);
end;

constructor TdxfCustomIrdRecord.Create;
begin
  inherited Create;
  FData := nil;
  FDataSize := 0;
end;

destructor TdxfCustomIrdRecord.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

procedure TdxfCustomIrdRecord.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    1071:
      if FMarker = cnstCIrdBaseRecord then
        FClassIndex := AReader.IntValue
      else
        if FMarker = cnstCIrdDscRecord then
          FIrdFlags := AReader.IntValue;
    330:
      if FMarker = '' then
        inherited ReadProperty(AReader);
    1004:
      if FMarker = cnstCIrdBaseRecord then
        AReader.AddBinData(FData, FDataSize)
      else
        inherited ReadProperty(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

{ TdxfIrdObjRecord }

procedure TdxfIrdObjRecord.Commit(const AReader: TdxfReader);
begin
  inherited Commit(AReader);
  AReader.AddLink(FEnt, IrdDscRecordProp, FIrdDscRecordHandle, TsgCADIrdObjRecordAccess);
end;

procedure TdxfIrdObjRecord.ReadProperty(const AReader: TdxfReader);
begin
  case AReader.Code of
    330:
      if FMarker = cnstCIrdObjRecord then
        FIrdDscRecordHandle := AReader.HandleValue
      else
        inherited ReadProperty(AReader);
  else
    inherited ReadProperty(AReader);
  end;
end;

initialization
  LayerProp := GetPropInfo(TsgDXFEntityAccess, 'Layer');
  LTypeProp := GetPropInfo(TsgDXFEntityAccess, 'LineType');
  BlockRecordProp := GetPropInfo(TsgDXFInsertAccess, 'BlockRecord');
  IrdDscRecordProp := GetPropInfo(TsgCADIrdObjRecordAccess, 'IrdDscRecord');
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_DXF := RegisterClipboardFormat('SoftGold DXF Image');
{$ENDIF}
  TPicture.RegisterFileFormat('dxf', 'AutoCAD DXF', TsgCADdxfImage);
{$IFDEF SG_DXT}
  TPicture.RegisterFileFormat('dxt', 'CADSoftTools drawing template',
    TsgCADdxfImage);
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_DXF, TsgCADdxfImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('dxf', TsgCADdxfImage);
  InitDataBaseSections;
  InitObjectItemClasses;

finalization
  TPicture.UnRegisterGraphicClass(TsgCADdxfImage);
  TsgDXFConverterAccess.UnRegisterXRefGraphicClass(TsgCADdxfImage);
  ClearObjects(HeadVars);
  HeadVars.Free;
  ClearObjects(DataBaseSections);
  DataBaseSections.Free;
  ClearObjects(ObjectItemClasses);
  ObjectItemClasses.Free;

end.
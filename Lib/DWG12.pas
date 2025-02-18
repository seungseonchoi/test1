{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   DWG reader classes                       }
{          Supports DWG R9/R10/R12 file formats              }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit DWG12;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
   Windows,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, System.Types,
{$ELSE}
  Graphics,
  {$IFDEF SGDEL_XE3}
  System.Types, System.UITypes,
  {$ENDIF}
{$ENDIF}
  DXFConv, CADImage, sgConsts,
  sgFunction, DWG
  {$IFDEF SGFPC}
    , FPimage
  {$ENDIF}
  ;

type
  Tdwg12Notify = (dwg12CreateChild, dwg12Add, dwg12Load,
    dwg12Layer, dwg12LType, dwg12BlockLoaded, dwg12Elevation, dwg12Style);

  Tdwg12ObjectState = set of Tdwg12Notify;

  Tdwg12Object = class;

  Tdwg11Reader = class;

  Tdwg12Class = class of Tdwg12Object;

  Tdwg12TableRecord = packed record
    Size: Word;
    Count: SmallInt;
    Unused: SmallInt;
    Start: Integer;
  end;

  Tdwg12HeadVars = packed record
    W1: Word;
    inbase: TFPoint;
    extmin: TFPoint;
    extmax: TFPoint;
    limmin: TF2DPoint;
    limmax: TF2DPoint;
    P1, P2: TF2DPoint;
    B1: array[0..1] of Byte;
    P3: TF2DPoint;
    B2: array[0..29] of Byte;
    P4: TF2DPoint;
    B3: array[0..9] of Byte;
    LTScale: Double;
    TextSize: Double;
    TraceWid: Double;
    B4: array[0..17] of Byte;
    D1: Double;
  end;

  Tdwg12Header = packed record
    Version: array[0..11] of AnsiChar;
    Byte1: Byte;
    Word1, Word2: Word;
    DrawingVersion: Word;
    Byte2: Byte;
    Entities: Integer;
    EntEnd: Integer;
    Blocks: Integer;
    Unused1: Integer;
    BlockEnd: Integer;
    Unused2: Integer;
    BlockRecords: Tdwg12TableRecord;
    Layers: Tdwg12TableRecord;
    Styles: Tdwg12TableRecord;
    LTypes: Tdwg12TableRecord;
    View: Tdwg12TableRecord;
    TableHeader: Tdwg12HeadVars;
  end;

  Tdwg12Object = class
  private
    FOptions: Word;
    FFlags: Byte;
    FState: Tdwg12ObjectState;
  protected
    function GetSubEntKindRead: Word; virtual;
    procedure SetSubEntKindRead(const AValue: Word); virtual;
    function GetPaperSpace: Integer; virtual;
    function AddRefsTo(const AReader: Tdwg11Reader;
      const ASection: TConvSection; const AIndex: Integer): Tdwg12Object;
    function GetLength: Integer; virtual;
    function GetEntity: TsgDXFEntity; virtual;
    function GetName: string; virtual;
    procedure SetName(const AValue: string); virtual;
    // object list methods
    function GetCount: Integer; virtual;
    function GetItems(Index: Integer): Tdwg12Object; virtual;
    procedure SetItems(Index: Integer; const AValue: Tdwg12Object); virtual;
    function Find(const AName: string; var I: Integer): Boolean;
    function AddItem(AObj: Tdwg12Object): Integer; virtual;
    function RemoveItem(AObj: Tdwg12Object): Integer; virtual;
    function IndexOf(AObj: Tdwg12Object): Integer; virtual;
    procedure InsertItem(AIndex: Integer; AObj: Tdwg12Object); virtual;
    procedure DeleteItem(AIndex: Integer); virtual;
    procedure Clear; virtual;
    procedure ReadEntityData(const AReader: Tdwg11Reader); virtual;
    procedure ReadState(const AReader: Tdwg11Reader; var AStart, AEnd: Integer); virtual;
    function ReadHeader(const AReader: Tdwg11Reader): Integer; virtual;
    procedure ReadXData(const AReader: Tdwg11Reader); virtual;
    class function GetEntityClass: TsgDXFEntityClass; virtual;
    class function CreateEntity: TsgDXFEntity; virtual;
    procedure SetEntity(const AEntity: TsgDXFEntity); virtual;
    procedure ReadSubEnts(const AReader: Tdwg11Reader;
      var AStart, AEnd: Integer; ALength: Integer); virtual;
    function EOSection(const AReader: Tdwg11Reader): Boolean; virtual;
    function AddEntity(const AObj: Tdwg12Object): Boolean; virtual;
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); virtual;
    procedure Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); virtual;
    procedure LoadItems(const AReader: Tdwg11Reader); virtual;
    procedure ReadElevation(const AReader: Tdwg11Reader); virtual;
    procedure Commit(const AReader: Tdwg11Reader); virtual;
    procedure Notify(const AReader: Tdwg11Reader;
      AObj: Tdwg12Object; ANotify: Tdwg12Notify); virtual;
    function GetRefs: TList; virtual;
    procedure NotifyRefs(const AReader: Tdwg11Reader;
      const ANotify: Tdwg12Notify); virtual;
    function SubEntCreate(const AReader: Tdwg11Reader;
      const ALength: Integer): Tdwg12Object; virtual;
    procedure ReadKind(const AReader: Tdwg11Reader); virtual;
    property SubEntKindRead: Word read GetSubEntKindRead write SetSubEntKindRead;
  public
    constructor Create; overload; virtual;
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); overload; virtual;
    property Entity: TsgDXFEntity read GetEntity write SetEntity;
    property Items[Index: Integer]: Tdwg12Object read GetItems write SetItems; default;
    property Count: Integer read GetCount;
    property Length: Integer read GetLength;
    property Name: string read GetName write SetName;
    property Refs: TList read GetRefs;
    property PaperSpace: Integer read GetPaperSpace;
  end;

  Tdwg11Reader = class(TdwgReader)
  private
    FBuff: AnsiString;
    FCount: Integer;
    FDone: Integer;
    FHeader: Tdwg12Header;
    FVPortTableRecord: Tdwg12TableRecord;
    FAppIDTableRecord: Tdwg12TableRecord;
    FDimStyleTableRecord: Tdwg12TableRecord;
    FViewportEntHeaderTableRecord: Tdwg12TableRecord;
    FBlocks: Tdwg12Object;
    FLayers: Tdwg12Object;
    FStyles: Tdwg12Object;
    FLTypes: Tdwg12Object;
    FVPorts: Tdwg12Object;
    FAppIDs: Tdwg12Object;
    FViewportEntHdrs: Tdwg12Object;
    FBlockRecords: Tdwg12Object;
    FElevIndex: Integer;
    FElevations: array[0 .. 4] of Double;
    FOwner: Tdwg12Object;
    FReadPaperSpace: TNotifyEvent;
    FReadModelSpace: TNotifyEvent;
    FLayouts: Tdwg12Object;
    FPaperCount: Integer;
    FPaperIndex: Integer;
    FBlock: Tdwg12Object;
    FActiveViewportEntHdr: Tdwg12Object;
    procedure ReadEntities(AStart, AEnd: Integer);
    procedure ReadTable(const ATable: Tdwg12TableRecord; ASection: Tdwg12Object);
    function ReadName: string;
    function ReadStringLen(ALength: Integer): string;
    function GetSection(Index: TConvSection): Tdwg12Object;
    function GetElevation: Double;
    procedure SetReadPaperSpace(const AValue: TNotifyEvent);
    procedure SetReadModelSpace(const AValue: TNotifyEvent);
    function CreateBlockRecord(const AName: string;
      var AIndex: Integer): Tdwg12Object;
  protected
    procedure DoProgress(Stage: TProgressStage);
    procedure DoRunning(const ALength: Integer; var APos: Integer);
    function ReadAngle: Double;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function ReadByte: Byte;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function ReadInteger: Integer;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function ReadDouble: Double;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function ReadWord: Word;{$IFDEF SG_INLINE} inline;{$ENDIF}

    function GetNameLen: Integer; virtual;
    function ReadLTypeIndex: Integer; virtual;
    function ReadString: string;
    procedure ReadHeader; override;
    procedure ReadObjects; override;
    procedure ReadImage; override;
    procedure Skip(const ACount: Integer);

    procedure PushElevation;
    procedure PopElevation;

    procedure ReadEntityPaper(Sender: TObject);
    procedure ReadBlockPaper(Sender: TObject);
    procedure InitModelPaper(Sender: TObject);

    procedure ForceLayout(const APaperSpace: Integer);

    procedure UpdateViewportEntHeader(AViewport: Tdwg12Object);

    property Sections[Index: TConvSection]: Tdwg12Object read GetSection; default;
    property Elevation: Double read GetElevation;
    property ReadPaperSpace: TNotifyEvent read FReadPaperSpace write SetReadPaperSpace;
    property ReadModelSpace: TNotifyEvent read FReadModelSpace write SetReadModelSpace;
    property ActiveViewportEntHdr: Tdwg12Object read FActiveViewportEntHdr write FActiveViewportEntHdr;
  public
    constructor Create(Conv: TsgDXFConverter; S: TStream);
    destructor Destroy; override;
  end;

  Tdwg12Reader = class(Tdwg11Reader)
  protected
    function GetNameLen: Integer; override;
    function ReadLTypeIndex: Integer; override;
  end;

implementation

uses
  sgLines, ExtData;

const
  cnstBlockRecordIndex = 256;
  cnstLTypeIndex = 257;
  cnstLayerIndex = 258;
  cnstVPortIndex = 259;
  cnstStyleIndex = 260;
  cnstAppIDIndex = 261;
  cnstViewportEntHdrIndex = 262;
  cnstMaxClassIndex = 262;

type
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFObjectEntityAccess = class(TsgDXFObjectEntity);
  TsgDXFTextAccess = class(TsgDXFText);

  Tdwg12BlockRecord = class;

  {TLayerRecord = packed record
    Flag: Byte;
    Name: array[0..31] of AnsiChar;
    Used: Word;
    Color: Smallint;
    Style: Smallint;
  end;

  TLtypeRecord = packed record
    Flag: Byte;
    Name: array[0..31] of AnsiChar;
    Unused: array[0..50] of Byte;
    Count: Byte;
    Ticks: array[0..12] of Double;
  end;

  TEntitiesRecord = packed record
    Kind: Byte;
    Flag: Byte;
    Length: Word;
    Layer: Word;
    Opts: Word;
  end;

  TBlockRecord = packed record
    Flag: Byte;
    Name: array[0..31] of AnsiChar;
  end;

  TVPortRecord = packed record
    Flag: Byte;
    Name: array[0..31] of AnsiChar;
    Used: Word;
    Values: array[0..9] of Double;
  end;}

  Tdwg12Dummy = class(Tdwg12Object)
  protected
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    class function CreateEntity: TsgDXFEntity; override;
    procedure Commit(const AReader: Tdwg11Reader); override;
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
  end;

  Tdwg12Entity = class(Tdwg12Object)
  private
    FEntity: TsgDXFEntity;
    function ReadPoint(const AReader: Tdwg11Reader; HasZ: Boolean): TFPoint;
  protected
    function GetEntity: TsgDXFEntity; override;
    function GetPaperSpace: Integer; override;
    procedure SetEntity(const AEntity: TsgDXFEntity); override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  public
    destructor Destroy; override;
  end;

  Tdwg12Group = class(Tdwg12Entity)
  private
    FSubEntKindRead: Word;
    FItems: TList;
  protected
    // object list methods
    function GetCount: Integer; override;
    function GetItems(Index: Integer): Tdwg12Object; override;
    procedure SetItems(Index: Integer; const AValue: Tdwg12Object); override;
    function AddItem(AObj: Tdwg12Object): Integer; override;
    function RemoveItem(AObj: Tdwg12Object): Integer; override;
    procedure InsertItem(AIndex: Integer; AObj: Tdwg12Object); override;
    function IndexOf(AObj: Tdwg12Object): Integer; override;
    procedure DeleteItem(AIndex: Integer); override;

    procedure LoadItems(const AReader: Tdwg11Reader); override;

    function GetCheckSize: Integer; virtual;
    function GetSubEntKindRead: Word; override;
    procedure SetSubEntKindRead(const AValue: Word); override;
    procedure ReadSubEnts(const AReader: Tdwg11Reader;
      var AStart, AEnd: Integer; ALength: Integer); override;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
    destructor Destroy; override;
  end;

  Tdwg12Table = class(Tdwg12Group)
  protected
    function GetCheckSize: Integer; override;
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    procedure Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    procedure ReadKind(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12BlockRecords = class(Tdwg12Table)
  protected
    FReader: Tdwg11Reader;
    function AddEntity(const AObj: Tdwg12Object): Boolean; override;
    function AddItem(AObj: Tdwg12Object): Integer; override;
    procedure InsertItem(AIndex: Integer; AObj: Tdwg12Object); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    function SubEntCreate(const AReader: Tdwg11Reader;
      const ALength: Integer): Tdwg12Object; override;
  end;

  Tdwg12Arc = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Attdef = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure ReadDataStr(const AReader: Tdwg11Reader); virtual;
  end;

  Tdwg12Attrib = class(Tdwg12Attdef)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadDataStr(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Blocks = class(Tdwg12Group)
  private
    FBlocksStart: Integer;
    function BlockIndexByOffset(const AReader: Tdwg11Reader;
      const APos: Integer; var AIndex: Integer): Boolean;
  protected
    FReader: Tdwg11Reader;
    function AddEntity(const AObj: Tdwg12Object): Boolean; override;
    function AddItem(AObj: Tdwg12Object): Integer; override;
    function RemoveItem(AObj: Tdwg12Object): Integer; override;
    procedure DeleteItem(AIndex: Integer); override;
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    function GetCheckSize: Integer; override;
    procedure ReadState(const AReader: Tdwg11Reader; var AStart, AEnd: Integer); override;
    function SubEntCreate(const AReader: Tdwg11Reader;
      const ALength: Integer): Tdwg12Object; override;
    property BlocksStart: Integer read FBlocksStart write FBlocksStart;
  end;

  Tdwg12Block = class(Tdwg12Group)
  private
    FRefs: TList;
    FBlockRecord: Tdwg12BlockRecord;
  protected
    function EOSection(const AReader: Tdwg11Reader): Boolean; override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    function GetRefs: TList; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    property BlockRecord: Tdwg12BlockRecord read FBlockRecord write FBlockRecord;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
    destructor Destroy; override;
  end;

  Tdwg12Circle = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12CustomInsert = class(Tdwg12Group)
  private
    FBlock: Tdwg12Block;
  protected
    function EOSection(const AReader: Tdwg11Reader): Boolean; override;
    procedure ReadBlock(const AReader: Tdwg11Reader);
    procedure Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    procedure Notify(const AReader: Tdwg11Reader;
      AObj: Tdwg12Object; ANotify: Tdwg12Notify); override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Insert = class(Tdwg12CustomInsert)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Dim = class(Tdwg12CustomInsert)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  {Tdwg12EndBlock = class(Tdwg12Dummy)
  end;}

  Tdwg12Face = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure ReadElevation(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Line = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure ReadElevation(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12PLine = class(Tdwg12Group)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    function EOSection(const AReader: Tdwg11Reader): Boolean; override;
  end;

  Tdwg12Point = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure ReadElevation(const AReader: Tdwg11Reader); override;
  end;

  {Tdwg12Seqend = class(Tdwg12Dummy)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
  end;}

  Tdwg12Solid = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Trace = class(Tdwg12Solid)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
  end;

  Tdwg12Text = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Vertex = class(Tdwg12Entity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Viewport = class(Tdwg12Entity)
  private
    procedure ApplyEED(const AReader: Tdwg11Reader;
      const AEEData: TsgCADExtendedData; const AAppID: UInt64);
  protected
    FStart: Integer;
    procedure Commit(const AReader: Tdwg11Reader); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    procedure ReadState(const AReader: Tdwg11Reader; var AStart, AEnd: Integer); override;
    procedure ReadXData(const AReader: Tdwg11Reader); override;
  end;

  // Table items

  Tdwg12NamedEntity = class(Tdwg12Object)
  private
    FEntity: TsgDXFEntity;
    FName: string;
    FRefs: TList;
    FLength: Integer;
  protected
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    function GetEntity: TsgDXFEntity; override;
    procedure SetEntity(const AEntity: TsgDXFEntity); override;
    function GetLength: Integer; override;
    function GetName: string; override;
    procedure SetName(const AValue: string); override;
    function ReadHeader(const AReader: Tdwg11Reader): Integer; override;
    procedure Commit(const AReader: Tdwg11Reader); override;
    function GetRefs: TList; override;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
    destructor Destroy; override;
  end;

  Tdwg12AppID = class(Tdwg12NamedEntity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
  end;

  Tdwg12Layer = class(Tdwg12NamedEntity)
  protected
    procedure Commit(const AReader: Tdwg11Reader); override;
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
  end;

  Tdwg12LType = class(Tdwg12NamedEntity)
  protected
    procedure DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
  end;

  Tdwg12BlockRecord = class(Tdwg12NamedEntity)
  private
    FBlock: Tdwg12Block;
    FOffset: Integer;
  protected
    procedure Commit(const AReader: Tdwg11Reader); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
    property Block: Tdwg12Block read FBlock write FBlock;
    property Offset: Integer read FOffset write FOffset;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
    destructor Destroy; override;
  end;

  // ucs table item
  Tdwg12VPort = class(Tdwg12NamedEntity)
  protected
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12ViewportEntHdrTable = class(Tdwg12Table)
  protected
    function IndexOfOffset(AOffset: Integer; var AIndex: Integer): Boolean;
  end;

  Tdwg12ViewportEntHdr = class(Tdwg12NamedEntity)
  private
    FOffset: Integer;
  protected
    FPaper: Tdwg12Object;
    procedure Commit(const AReader: Tdwg11Reader); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  end;

  Tdwg12Style = class(Tdwg12NamedEntity)
  protected
    procedure DoAddEntities(const AReader: Tdwg11Reader;
      const AOwner: Tdwg12Object); override;
    class function GetEntityClass: TsgDXFEntityClass; override;
    procedure ReadEntityData(const AReader: Tdwg11Reader); override;
  public
    constructor Create(const AEntity: TsgDXFEntity; ALength: Integer); override;
  end;

const
  cnstDwgClassCount = 25;
  cnstDwgClasses: array[0 .. cnstDwgClassCount - 1] of record
    Obj: Tdwg12Class;
    Ent: TsgDXFEntityClass;
  end = (( Obj: nil;             Ent: nil),             // 0:
         ( Obj: Tdwg12Line;      Ent: TsgDXFLine),      // 1:  LINE
         ( Obj: Tdwg12Point;     Ent: TsgDXFPoint),     // 2:  POINT
         ( Obj: Tdwg12Circle;    Ent: TsgDXFCircle),    // 3:  CIRCLE
         ( Obj: nil;             Ent: nil),             // 4:  SHAPE
         ( Obj: nil;             Ent: nil),             // 5:
         ( Obj: nil;             Ent: nil),             // 6:
         ( Obj: Tdwg12Text;      Ent: TsgDXFText),      // 7:  TEXT
         ( Obj: Tdwg12Arc;       Ent: TsgDXFArc),       // 8:  ARC
         ( Obj: Tdwg12Trace;     Ent: TsgDXFTrace),     // 9:  TRACE
         ( Obj: nil;             Ent: nil),             // 10:
         ( Obj: Tdwg12Solid;     Ent: TsgDXFSolid),     // 11: SOLID
         ( Obj: Tdwg12Block;     Ent: TsgDXFBlock),     // 12: BLOCK
         ( Obj: nil{Tdwg12EndBlock};  Ent: nil{TsgDXFEntity}),    // 13: ENDBLK
         ( Obj: Tdwg12Insert;    Ent: TsgDXFInsert),    // 14: INSERT
         ( Obj: Tdwg12Attdef;    Ent: TsgDXFAttdef),    // 15: ATTDEF
         ( Obj: Tdwg12Attrib;    Ent: TsgDXFAttrib),    // 16: ATTRIB
         ( Obj: nil{Tdwg12Seqend};    Ent: nil{TsgDXFSeqend}),    // 17: S/BEND
         ( Obj: nil;             Ent: nil),             // 18:
         ( Obj: Tdwg12PLine;     Ent: TsgDXFPolyline),  // 19: PLINE
         ( Obj: Tdwg12Vertex;    Ent: TsgDXFVertex),    // 20: VERTEX
         ( Obj: nil;             Ent: nil),             // 21:
         ( Obj: Tdwg12Face;      Ent: TsgDXF3DFace),    // 22: 3DFACE
         ( Obj: Tdwg12Dim;       Ent: TsgDXFDimension), // 23: DIM
         ( Obj: Tdwg12Viewport;  Ent: TsgDXFViewport)); // 24: VIEWPORT



type
  Pdwg12Classes = ^Tdwg12Classes;
  Tdwg12Classes = array[0 .. cnstMaxClassIndex] of Tdwg12Class;

var
  DWG12Classes: Pdwg12Classes;

procedure InitDWG12Classes;
var
  I: Integer;
begin
  New(DWG12Classes);
  I := Low(cnstDwgClasses);
  while I <= High(cnstDwgClasses) do
  begin
    if cnstDwgClasses[I].Obj <> nil then
      DWG12Classes[I] := cnstDwgClasses[I].Obj
    else
      DWG12Classes[I] := Tdwg12Dummy;
    Inc(I);
  end;
  repeat
    DWG12Classes[I] := Tdwg12Dummy;
    Inc(I);
  until I > High(Byte);
  DWG12Classes[cnstBlockRecordIndex] := Tdwg12BlockRecord;
  DWG12Classes[cnstLTypeIndex] := Tdwg12LType;
  DWG12Classes[cnstLayerIndex] := Tdwg12Layer;
  DWG12Classes[cnstVPortIndex] := Tdwg12VPort;
  DWG12Classes[cnstStyleIndex] := Tdwg12Style;
  DWG12Classes[cnstAppIDIndex] := Tdwg12AppID;
  DWG12Classes[cnstViewportEntHdrIndex] := Tdwg12ViewportEntHdr;
end;

procedure FinalDWG12Classes;
begin
  Dispose(DWG12Classes);
end;

function GetPSName(const AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := sModel
  else
    Result := sLayout + IntToStr(AIndex);
  Result := '*[' + Result + ']';
end;

procedure ReadDwg12EED(const AMemory: Pointer; const ASize: Cardinal;
  const AExtData: TsgCADExtendedData; const AReserved: Byte = 0); overload;
type
  Pdwg12ExtData = ^Tdw12ExtData;
  Tdw12ExtData = packed record
    Code: Byte;
    case Byte of
      //0:( Name: ShortString); //Len: Byte; Name: array[0..0] of AnsiChar
      //1:( AppName: ShortString); //Len: Byte; Name: array[0..0] of AnsiChar; LastChar: AnsiChar = #0;
      2:( VByte: Byte);
      3,5:( Handle: UInt64);
      0,1,4:( Count: Byte; Chunk: array[0..255] of Byte);
      10,11,12,13: (Point: TFPoint);
      40,41,42: (VDouble: Double);
      60,61,62,63,64,65,66,67,68,69,70: (VSmallInt: SmallInt);
      71: (VInteger: Integer);
  end;
var
  vData, vEnd: Pdwg12ExtData;
  vCode: Word;
  S: string;
begin
  vData := Pdwg12ExtData(AMemory);
  vEnd := Pdwg12ExtData(THandle(vData) + ASize);
  while THandle(vData) < THandle(vEnd) do
  begin
    vCode := vData^.Code + 1000;
    case vData^.Code of
      0:
        begin
          SetString(S, PAnsiChar(@vData^.Chunk[0]), vData^.Count);
          AExtData.AddString(vCode, S);
          Inc(PByte(vData), (vData^.Count + 1) * SizeOf(vData^.Chunk[0]));
        end;
      1:
        begin
          if vData^.Chunk[0] <> 0 then
          begin
            SetString(S, PAnsiChar(@vData^.Chunk[1]), vData^.Count);
            Inc(PByte(vData), (vData^.Count + 2) * SizeOf(vData^.Chunk[0]));
          end
          else
            Inc(PByte(vData), 2);
        end;
      2:
        begin
          AExtData.AddByte(vCode, vData^.VByte);
          Inc(PByte(vData), SizeOf(vData^.VByte));
        end;
      3, 5:
        begin
          AExtData.AddInt64(vCode, vData^.Handle);
          Inc(PByte(vData), SizeOf(vData^.Handle));
        end;
      4:
        begin
          AExtData.AddBinary(vCode, vData^.Count, @vData^.Chunk[0]);
          Inc(PByte(vData), vData^.Count * SizeOf(vData^.Chunk[0]) + SizeOf(vData^.Count));
        end;
      10..13:
        begin
          AExtData.AddPoint(vCode, vData^.Point);
          Inc(PByte(vData), SizeOf(vData^.Point));
        end;
      40..42:
        begin
          AExtData.AddDouble(vCode, vData^.VDouble);
          Inc(PByte(vData), SizeOf(vData^.VDouble));
        end;
      60..70:
        begin
          AExtData.AddInt16(vCode, vData^.VSmallInt);
          Inc(PByte(vData), SizeOf(vData^.VSmallInt));
        end;
      71:
        begin
          AExtData.AddInt(vCode, vData^.VInteger);
          Inc(PByte(vData), SizeOf(vData^.VInteger));
        end;
    end;
    Inc(PByte(vData), SizeOf(vData^.Code));
  end;
end;

function ReadDwg12EED(AMemory: Pointer; const AExtData: TsgCADExtendedData;
  const AReserved: Byte = 0): Word; overload;
begin
  Result := PWord(AMemory)^;
  Inc(PWord(AMemory));
  ReadDwg12EED(AMemory, Result, AExtData, AReserved);
end;

procedure ReadDwg12EED(AStream: TStream; const ASize: Cardinal;
  const AExtData: TsgCADExtendedData; const AReserved: Byte = 0); overload;
var
  vData: PByte;
begin
  GetMem(vData, ASize);
  AStream.ReadBuffer(vData^, ASize);
  ReadDwg12EED(vData, ASize, AExtData, AReserved);
  FreeMem(vData);
end;

function ReadDwg12EED(AStream: TStream; const AExtData: TsgCADExtendedData;
  const AReserved: Byte = 0): Word; overload;
begin
  AStream.ReadBuffer(Result, SizeOf(Result));
  ReadDwg12EED(AStream, Result, AExtData, AReserved);
end;

{ Tdwg11Reader }

procedure Tdwg11Reader.PushElevation;
begin
  Inc(FElevIndex);
  FElevations[FElevIndex] := ReadDouble;
end;

constructor Tdwg11Reader.Create(Conv: TsgDXFConverter; S: TStream);
begin
  inherited Create(Conv, S);
  SetLength(FBuff, High(Byte) + 2);
  FBlocks := Tdwg12Blocks.Create(Conv.Sections[csBlocks], 0);
  Tdwg12Blocks(FBlocks).FReader := Self;
  FBlockRecords := Tdwg12BlockRecords.Create(Conv.Sections[csBlockRecords], 0);
  Tdwg12Table(FBlockRecords).SubEntKindRead := cnstBlockRecordIndex;
  Tdwg12BlockRecords(FBlockRecords).FReader := Self;
  FVPorts := Tdwg12Table.Create(Conv.Sections[csVPorts], 0);
  Tdwg12Table(FVPorts).SubEntKindRead := cnstVPortIndex;
  FLTypes := Tdwg12Table.Create(Conv.Sections[csLTypes], 0);
  Tdwg12Table(FLTypes).SubEntKindRead := cnstLTypeIndex;
  FLayers := Tdwg12Table.Create(Conv.Sections[csLayers], 0);
  Tdwg12Table(FLayers).SubEntKindRead := cnstLayerIndex;
  FLayouts := Tdwg12Group.Create(TsgDXFTable.Create, 0);
  FStyles := Tdwg12Table.Create(Conv.Sections[csStyles], 0);
  Tdwg12Table(FStyles).SubEntKindRead := cnstStyleIndex;
  FAppIDs := Tdwg12Table.Create(Conv.Sections[csAppID], 0);
  Tdwg12Table(FAppIDs).SubEntKindRead := cnstAppIDIndex;
  FViewportEntHdrs := Tdwg12Table.Create(TsgDXFTable.Create, 0);
  Tdwg12Table(FViewportEntHdrs).SubEntKindRead := cnstViewportEntHdrIndex;
end;

destructor Tdwg11Reader.Destroy;
begin
  Finalize(FBuff);
  while FLayouts.Count > 0 do
    FLayouts.DeleteItem(FLayouts.Count - 1);
  FLayouts.Free;
  FBlocks.Free;
  FLayers.Free;
  FLTypes.Free;
  FVPorts.Free;
  FStyles.Free;
  FBlockRecords.Free;
  FAppIDs.Free;
  FViewportEntHdrs.Free;
  inherited Destroy;
end;

procedure Tdwg11Reader.DoProgress(Stage: TProgressStage);
begin
  if Assigned(OnProgress) then
    OnProgress(Stage,FDone,FCount);
end;

procedure Tdwg11Reader.DoRunning(const ALength: Integer; var APos: Integer);
begin
  Inc(APos, ALength);
  Inc(FDone, ALength);
  DoProgress(psRunning);
end;

procedure Tdwg11Reader.ForceLayout(const APaperSpace: Integer);
var
  vLayoutBlockRecord: Tdwg12BlockRecord;
  I, J: Integer;
begin
  for I := FPaperCount to APaperSpace do
  begin
    vLayoutBlockRecord := Tdwg12BlockRecord(CreateBlockRecord(GetPSName(I), J));
    FLayouts.InsertItem(1, vLayoutBlockRecord.Block);
    Inc(FPaperCount);
  end;
  FPaperIndex := APaperSpace;
  FOwner := FLayouts[FPaperIndex];
end;

function Tdwg11Reader.GetElevation: Double;
begin
  Result := FElevations[FElevIndex];
end;

function Tdwg11Reader.GetNameLen: Integer;
begin
  Result := 32;
end;

function Tdwg11Reader.GetSection(Index: TConvSection): Tdwg12Object;
begin
  Result := nil;
  case Index of
    csStyles:         Result := FStyles;
    csLTypes:         Result := FLTypes;
    csLayers:         Result := FLayers;
    csBlocks:         Result := FBlocks;
    csBlockRecords:   Result := FBlockRecords;
    csVPorts:         Result := FVPorts;
    csAppID:          Result := FAppIDs;
  end;
end;

procedure Tdwg11Reader.InitModelPaper(Sender: TObject);
begin
  FOwner := FLayouts[FPaperIndex];
end;

procedure Tdwg11Reader.PopElevation;
begin
  Dec(FElevIndex);
end;

function Tdwg11Reader.ReadAngle: Double;
begin
  Result := ReadDouble * f180DividedByPi;
end;

procedure Tdwg11Reader.ReadEntities(AStart, AEnd: Integer);
var
  Obj: Tdwg12Object;
begin
  while not Stopped and (AStart < AEnd) do
  begin
    Stream.Position := AStart;
    Obj := DWG12Classes[ReadByte].Create;
    FPaperIndex := 0;
    InitModelPaper(nil);
    FBlock := FLayouts[0];
    Obj.ReadState(Self, AStart, AEnd);
    if FOwner.AddItem(Obj) < 0 then
      Obj.Free;
  end;
end;

procedure Tdwg11Reader.ReadBlockPaper(Sender: TObject);
begin
  ReadWord;
end;

function Tdwg11Reader.ReadByte: Byte;
begin
  Stream.ReadBuffer(Result, 1);
end;

function Tdwg11Reader.ReadDouble: Double;
begin
  Stream.ReadBuffer(Result, 8);
end;

function Tdwg11Reader.ReadLTypeIndex: Integer;
begin
  Result := ReadByte;
end;

function Tdwg11Reader.ReadName: string;
begin
  Result := ReadStringLen(GetNameLen);
end;

procedure Tdwg11Reader.ReadTable(const ATable: Tdwg12TableRecord; ASection: Tdwg12Object);
var
  vStart, vEnd: Integer;
begin
  vStart := ATable.Start;
  vEnd := vStart + ATable.Count * ATable.Size;
  ASection.ReadSubEnts(Self, vStart, vEnd, ATable.Size);
end;

function Tdwg11Reader.ReadWord: Word;
begin
  Stream.ReadBuffer(Result, 2);
end;

procedure Tdwg11Reader.SetReadModelSpace(const AValue: TNotifyEvent);
begin
  FReadModelSpace := AValue;
end;

procedure Tdwg11Reader.SetReadPaperSpace(const AValue: TNotifyEvent);
begin
  FReadPaperSpace := AValue;
end;

procedure Tdwg11Reader.Skip(const ACount: Integer);
begin
  Stream.Position := Stream.Position + ACount;//Stream.Seek(ACount, soFromCurrent);
end;

procedure Tdwg11Reader.UpdateViewportEntHeader(AViewport: Tdwg12Object);
var
  I: Integer;
begin
  if Tdwg12ViewportEntHdrTable(FViewportEntHdrs).IndexOfOffset(Tdwg12Viewport(AViewport).FStart, I) then
    Tdwg12ViewportEntHdr(FViewportEntHdrs[I]).FPaper := FOwner;
end;

function Tdwg11Reader.CreateBlockRecord(const AName: string;
  var AIndex: Integer): Tdwg12Object;
var
  vBlockRecord: Tdwg12BlockRecord;
begin
  Result := FBlockRecords.SubEntCreate(Self, 0);
  vBlockRecord := Tdwg12BlockRecord(Result);

  vBlockRecord.Block.Name := AName;
  FBlocks.Notify(Self, vBlockRecord.Block, dwg12CreateChild);
  FBlocks.AddItem(vBlockRecord.Block);

  Result.Name := AName;
  Tdwg12BlockRecord(Result).Offset := -1;
  Result.FFlags := $40;
  FBlockRecords.Notify(Self, Result, dwg12CreateChild);
  Result.Commit(Self);
  AIndex := FBlockRecords.AddItem(Result);
end;

procedure Tdwg11Reader.ReadHeader;
var
  vHeadVarStruct: TsgHeadVarStruct;
begin
  Stream.ReadBuffer(FHeader, SizeOf(FHeader));
  vHeadVarStruct := Conv.HeadVarStruct;
  vHeadVarStruct.LTScale := FHeader.TableHeader.LTScale;
  vHeadVarStruct.TextSize := FHeader.TableHeader.TextSize;
  vHeadVarStruct.ExtMin := FHeader.TableHeader.extmin;
  vHeadVarStruct.ExtMax := FHeader.TableHeader.extmax;
  vHeadVarStruct.InsBase := FHeader.TableHeader.inbase;
  Conv.HeadVarStruct := vHeadVarStruct;
  if Version > acR09 then
  begin
    Stream.Position := $500;//Stream.Seek($500, soFromBeginning);
    Stream.ReadBuffer(FVPortTableRecord, SizeOf(Tdwg12TableRecord));
    Stream.Position := Stream.Position + 8;
    Stream.ReadBuffer(FAppIDTableRecord, SizeOf(Tdwg12TableRecord));
    if FHeader.DrawingVersion >= 205 then
    begin
      Stream.Position := Stream.Position + 6;
      Stream.ReadBuffer(FDimStyleTableRecord, SizeOf(Tdwg12TableRecord));
       Stream.Position := $69F;//Stream.Seek($69F, soFromBeginning);
      Stream.ReadBuffer(FViewportEntHeaderTableRecord, SizeOf(Tdwg12TableRecord));
    end;
  end;
  if FHeader.BlockEnd = 0 then FHeader.BlockEnd := Stream.Size;
  Tdwg12Blocks(FBlocks).BlocksStart := FHeader.Blocks;
end;

procedure Tdwg11Reader.ReadObjects;
var
  I, J: Integer;
  vBlock: Tdwg12Block;
  vBlockRecord: Tdwg12BlockRecord;
  vStart, vEnd: Integer;
  vActiveVPort: TsgDXFVport;

  procedure DoClearUnnamedTableItems(ATable: TsgDXFEntity);
  var
    K: Integer;
    Ent: TsgDXFEntityAccess;
    Item: TsgOwneredItem;
  begin
    if Assigned(ATable) then
      for K := ATable.Count - 1 downto 0 do
      begin
        Ent := TsgDXFEntityAccess(ATable[K]);
        if (Ent.Flags and 128 <> 0) and (Ent.Name = '') then
        begin
          Item := TsgOwneredItem(Ent.GetNamedItem);
          if Assigned(Item) and (Item.RefCount = 1) then
          begin
            ATable.DeleteEntity(K);
            Ent.Free;
          end;
        end;
      end;
  end;

begin
  FDone := 0;
  with FHeader do
    FCount := Layers.Size * Layers.Count + LTypes.Size * LTypes.Count +
      EntEnd - Entities + BlockEnd - Blocks +
      FVPortTableRecord.Size * FVPortTableRecord.Count +
      FAppIDTableRecord.Size * FAppIDTableRecord.Count;

  DoProgress(psStarting);

  ReadTable(FHeader.BlockRecords, FBlockRecords);
  if not FBlockRecords.Find(sModelSpace, J) then
    CreateBlockRecord(sModelSpace, J);
  FPaperCount := 1;
  for I := 0 to FBlockRecords.Count - 1 do
  begin
    vBlockRecord := Tdwg12BlockRecord(FBlockRecords[I]);
    vBlock := vBlockRecord.Block;
    if I = J then
    begin
      vBlockRecord.Name := GetPSName(0);
      vBlockRecord.Commit(Self);
      vBlock.Name := vBlockRecord.Name;
      FLayouts.InsertItem(0, vBlock);
    end
    else
      if GetPaperIndex(vBlockRecord.Name) >= 1 then
      begin
        vBlockRecord.Name := GetPSName(FPaperCount);
        vBlockRecord.Commit(Self);
        vBlock.Name := vBlockRecord.Name;
        Inc(FPaperCount);
        FLayouts.AddItem(vBlock);
      end;
  end;
  if FPaperCount = 1 then
  begin
    ForceLayout(FPaperCount);
  end;

  ReadTable(FViewportEntHeaderTableRecord, FViewportEntHdrs);

  ReadTable(FHeader.Styles, FStyles);
  ReadTable(FHeader.LTypes, FLTypes);
  ReadTable(FHeader.Layers, FLayers);
  ReadTable(FVPortTableRecord, FVPorts);
  ReadTable(FAppIDTableRecord, FAppIDs);

  ReadModelSpace := InitModelPaper;
  ReadPaperSpace := ReadBlockPaper;

  vStart := FHeader.Blocks;
  vEnd := FHeader.BlockEnd;
  FBlocks.ReadState(Self, vStart, vEnd);

  ReadPaperSpace := ReadEntityPaper;
  FPaperIndex := 0;
  InitModelPaper(nil);
  ReadEntities(FHeader.Entities, FHeader.EntEnd);
  try
    if Version = acR12 then
      ReadEntities(FHeader.BlockEnd, Stream.Size - 2);
  except
  end;
  FBlockRecords.DoAddEntities(Self, nil);
  if FAppIDs.Entity = nil then
  begin
    FAppIDs.Entity := TsgDXFConverterAccess(Conv).CustomEntByName(sAppIDTableRecord, csTables);
    Include(FAppIDs.FState, dwg12Add);
  end;
  FAppIDs.DoAddEntities(Self, nil);
  FStyles.DoAddEntities(Self, nil);
  FLTypes.DoAddEntities(Self, nil);
  FLayers.DoAddEntities(Self, nil);
  FVPorts.DoAddEntities(Self, nil);
  FViewportEntHdrs.DoAddEntities(Self, nil);
  FBlocks.DoAddEntities(Self, nil);

  TsgDXFConverterAccess(Conv).Status := stDefault;

  FAppIDs.Load(Self, nil);
  FBlockRecords.Load(Self, nil);
  FStyles.Load(Self, nil);
  FLTypes.Load(Self, nil);
  FLayers.Load(Self, nil);
  FVPorts.Load(Self, nil);
  FBlocks.Load(Self, nil);

  if not Assigned(FActiveViewportEntHdr) and FViewportEntHdrs.Find('1', I) then
    FActiveViewportEntHdr := FViewportEntHdrs[I];
  if Assigned(FActiveViewportEntHdr) and (FActiveViewportEntHdr.FFlags and 1 <> 0) then
  begin
    I := FLayouts.IndexOf(Tdwg12ViewportEntHdr(FActiveViewportEntHdr).FPaper);
    if I >= 0 then
    begin
      Conv.DefaultLayoutIndex := I;
      TsgDXFConverterAccess(Conv).PHeadVarStruct^.TileMode := Ord(I = 0);
    end;
  end;

  for I := Conv.LayoutsCount to FPaperCount do
    Conv.NewNamedEntity(Conv.Sections[csLayouts], TsgDXFLayout, sLayout + IntToStr(I));

  for I := 0 to FPaperCount - 1 do
    Conv.Layouts[I].PaperSpaceBlock := TsgDXFBlock(FLayouts[I].Entity);

  vActiveVPort := TsgDXFVport(Conv.Sections[csVPorts].FindEntByName(sActiveVPort));
  if Assigned(vActiveVPort) then
    Conv.ActiveVPort := vActiveVPort;
  Conv.Loads(Conv.Sections[csEntities]);

  DoClearUnnamedTableItems(Conv.Sections[csLayers]);
  DoClearUnnamedTableItems(Conv.Sections[csLTypes]);
  DoClearUnnamedTableItems(Conv.Sections[csStyles]);
  DoClearUnnamedTableItems(Conv.Sections[csDimStyles]);
  DoClearUnnamedTableItems(Conv.Sections[csVPorts]);

  Exclude(FViewportEntHdrs.FState, dwg12Add);
  TsgDXFConverterAccess(Conv).Status := stLoading;
  DoProgress(psEnding);
end;

procedure Tdwg11Reader.ReadEntityPaper(Sender: TObject);
var
  vPaperSpace: Word;
begin
  vPaperSpace := ReadWord;
  FOwner := FLayouts[vPaperSpace];
end;

function Tdwg11Reader.ReadString: string;
var
  vLength: Integer;
begin
  Result := '';
  vLength := ReadWord;
  if vLength > 0 then
    Result := ReadStringLen(vLength);
end;

function Tdwg11Reader.ReadStringLen(ALength: Integer): string;
begin
  if ALength > High(Byte) + 1 then
    ALength := High(Byte) + 1;
  Stream.ReadBuffer(FBuff[1], ALength);
  FBuff[ALength + 1] := #0;
  Result := string(AnsiString(PAnsiChar(FBuff)));
end;

procedure Tdwg11Reader.ReadImage;
begin
  ReadObjects;
end;

function Tdwg11Reader.ReadInteger: Integer;
begin
  Stream.ReadBuffer(Result, 4);
end;

{ Tdwg12Reader }

function Tdwg12Reader.GetNameLen: Integer;
begin
  Result := inherited GetNameLen + 2;
end;

function Tdwg12Reader.ReadLTypeIndex: Integer;
begin
  Result := ReadWord;
end;

{ Tdwg12Object }

function Tdwg12Object.AddItem(AObj: Tdwg12Object): Integer;
begin
  Result := -1;
end;

procedure Tdwg12Object.DeleteItem(AIndex: Integer);
begin
end;

procedure Tdwg12Object.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoAddEntities(AReader, Self);
  if AOwner.AddEntity(Self) then
    Include(FState, dwg12Add);
end;

procedure Tdwg12Object.Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  if not (dwg12Load in FState) then
  begin
    Include(FState, dwg12Load);
    LoadItems(AReader);
    AReader.Conv.Loads(Entity);
  end;
end;

procedure Tdwg12Object.LoadItems(const AReader: Tdwg11Reader);
begin
end;

procedure Tdwg12Object.Notify(const AReader: Tdwg11Reader;
  AObj: Tdwg12Object; ANotify: Tdwg12Notify);
begin
  case ANotify of
    dwg12CreateChild: AReader.Conv.DoCreate(AObj.Entity);
    dwg12Layer: Entity.Layer := TsgDXFLayer(AObj.Entity);
    dwg12LType: Entity.SetLType(AObj.Entity);
    dwg12BlockLoaded:
      begin
        Include(FState, dwg12BlockLoaded);
        TsgDXFInsert(Entity).Block := TsgDXFBlock(AObj.Entity);
      end;
    dwg12Style:
      begin
        Include(FState, dwg12Style);
        TsgDXFTextAccess(Entity).StyleRef := TsgDXFStyle(AObj.Entity);
      end;
  end;
end;

procedure Tdwg12Object.NotifyRefs(const AReader: Tdwg11Reader;
  const ANotify: Tdwg12Notify);
var
  I: Integer;
begin
  if Assigned(Refs) then
  begin
    for I := 0 to Refs.Count - 1 do
      Tdwg12Object(Refs[I]).Notify(AReader, Self, ANotify);
    Refs.Clear;
  end;
end;

function Tdwg12Object.GetCount: Integer;
begin
  Result := 0
end;

function Tdwg12Object.GetItems(Index: Integer): Tdwg12Object;
begin
  Result := nil;
end;

function Tdwg12Object.GetSubEntKindRead: Word;
begin
  Result := 0;
end;

function Tdwg12Object.GetLength: Integer;
begin
  Result := 8;
end;

function Tdwg12Object.GetName: string;
begin
  Result := TsgDXFEntityAccess(Entity).GetName;
end;

procedure Tdwg12Object.SetItems(Index: Integer; const AValue: Tdwg12Object);
begin
end;

procedure Tdwg12Object.SetName(const AValue: string);
begin
  TsgDXFEntityAccess(Entity).SetName(AValue);
end;

procedure Tdwg12Object.SetSubEntKindRead(const AValue: Word);
begin
end;

function Tdwg12Object.SubEntCreate(const AReader: Tdwg11Reader;
  const ALength: Integer): Tdwg12Object;
begin
  Result := DWG12Classes[SubEntKindRead].Create(
    DWG12Classes[SubEntKindRead].CreateEntity, ALength);
  Notify(AReader, Result, dwg12CreateChild);
end;

procedure Tdwg12Object.Clear;
begin
  while Count > 0 do
  begin
    Items[Count - 1].Free;
    DeleteItem(Count - 1);
  end;
end;

procedure Tdwg12Object.Commit(const AReader: Tdwg11Reader);
begin
  if dwg12Elevation in FState then
    AReader.PopElevation;
end;

constructor Tdwg12Object.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  Entity := AEntity;
end;

constructor Tdwg12Object.Create;
begin
  Create(CreateEntity, 0);
end;

class function Tdwg12Object.CreateEntity: TsgDXFEntity;
begin
  Result := GetEntityClass.Create
end;

function Tdwg12Object.EOSection(const AReader: Tdwg11Reader): Boolean;
begin
  Result := AReader.Stopped;
end;

function Tdwg12Object.Find(const AName: string; var I: Integer): Boolean;
begin
  Result := False;
  I := 0;
  while (I < Count) and not Result do
    if SameText(AName, Items[I].Name) then
      Inc(Result)
    else
      Inc(I);
end;

function Tdwg12Object.GetEntity: TsgDXFEntity;
begin
  Result := nil;
end;

class function Tdwg12Object.GetEntityClass: TsgDXFEntityClass;
begin
  Result := nil;
end;

function Tdwg12Object.GetPaperSpace: Integer;
begin
  Result := 0;
end;

function Tdwg12Object.GetRefs: TList;
begin
  Result := nil;
end;

function Tdwg12Object.IndexOf(AObj: Tdwg12Object): Integer;
begin
  Result := -1;
end;

procedure Tdwg12Object.InsertItem(AIndex: Integer; AObj: Tdwg12Object);
begin
end;

function Tdwg12Object.AddEntity(const AObj: Tdwg12Object): Boolean;
begin
  Result := Entity.AddEntity(AObj.Entity) >= 0;
end;

function Tdwg12Object.AddRefsTo(const AReader: Tdwg11Reader;
  const ASection: TConvSection; const AIndex: Integer): Tdwg12Object;
var
  vSection: Tdwg12Object;
begin
  vSection := AReader[ASection];
  if (AIndex >= 0) and (AIndex < vSection.Count) then
  begin
    Result := vSection[AIndex];
    Result.Refs.Add(Self);
  end
  else
    Result := nil;
end;

procedure Tdwg12Object.ReadState(const AReader: Tdwg11Reader; var AStart, AEnd: Integer);
var
  vLength: Integer;
begin
  vLength := ReadHeader(AReader);
  ReadEntityData(AReader);
  AReader.DoRunning(vLength, AStart);
  ReadSubEnts(AReader, AStart, AEnd, 0);
  Commit(AReader);
end;

procedure Tdwg12Object.ReadSubEnts(const AReader: Tdwg11Reader;
  var AStart, AEnd: Integer; ALength: Integer);
begin
end;

procedure Tdwg12Object.ReadElevation(const AReader: Tdwg11Reader);
begin
  AReader.PushElevation;
  Include(FState, dwg12Elevation);
end;

procedure Tdwg12Object.ReadEntityData(const AReader: Tdwg11Reader);
begin
end;

function Tdwg12Object.ReadHeader(const AReader: Tdwg11Reader): Integer;
begin
  FFlags := AReader.ReadByte;
  Result := AReader.ReadWord;
  if Result = 0 then
    Result := 8; // Kind, Flags: Byte; Length, Layer, Options: Word;
end;

procedure Tdwg12Object.ReadKind(const AReader: Tdwg11Reader);
begin
  SubEntKindRead := AReader.ReadByte;
end;

function Tdwg12Object.RemoveItem(AObj: Tdwg12Object): Integer;
begin
  Result := -1;
end;

procedure Tdwg12Object.SetEntity(const AEntity: TsgDXFEntity);
begin
end;

procedure Tdwg12Object.ReadXData(const AReader: Tdwg11Reader);
begin
  AReader.Skip(AReader.ReadWord);
end;

{ Tdwg12Entity }

destructor Tdwg12Entity.Destroy;
begin
  if not (dwg12Add in FState) then
    FEntity.Free;
  inherited Destroy;
end;

function Tdwg12Entity.GetEntity: TsgDXFEntity;
begin
  Result := FEntity;
end;

function Tdwg12Entity.GetPaperSpace: Integer;
begin
  Result := FEntity.PaperSpace;
end;

procedure Tdwg12Entity.ReadEntityData(const AReader: Tdwg11Reader);
var
  I, N: Integer;
  vExtra: Byte;
  vHandle: UInt64;
begin
  I := AReader.ReadWord;
  AddRefsTo(AReader, csLayers, I);
  FOptions := AReader.ReadWord;
  if FFlags and $40 <> 0 then
    vExtra := AReader.ReadByte
  else
    vExtra := 0;
  if vExtra and $02 <> 0 then
    ReadXData(AReader);
  if FFlags and $01 <> 0 then
    Entity.ColorCAD := MakeColorCAD(acIndexColor, AReader.ReadByte);
  if FFlags and $02 <> 0 then
  begin
    I := AReader.ReadLTypeIndex;
    AddRefsTo(AReader, csLTypes, I);
  end;
  if FFlags and $04 <> 0 then
    ReadElevation(AReader);
  if FFlags and $08 <> 0 then
    if FEntity is TsgDXFPenLine then
      TsgDXFPenLine(FEntity).ZThick := AReader.ReadDouble
    else
      AReader.Skip(SizeOf(Double));
  if FFlags and $20 <> 0 then
  begin
    N := AReader.ReadByte;
    vHandle := 0;
    for I := 0 to N - 1 do
      vHandle := AReader.ReadByte or (vHandle shl 8);
    Entity.Handle := vHandle;
    TsgDXFConverterAccess(AReader.Conv).SetHandle(Entity);
  end;
  if vExtra and $04 <> 0 then
    AReader.ReadPaperSpace(Self)
  else
    AReader.ReadModelSpace(Self);
end;

function Tdwg12Entity.ReadPoint(const AReader: Tdwg11Reader; HasZ: Boolean): TFPoint;
begin
  if not HasZ then
  begin
    AReader.Stream.ReadBuffer(Result, SizeOf(Double) * 2);
    Result.Z := AReader.Elevation;
  end
  else
    AReader.Stream.ReadBuffer(Result, SizeOf(TFPoint));
end;

procedure Tdwg12Entity.SetEntity(const AEntity: TsgDXFEntity);
begin
  FEntity := AEntity;
end;

{ Objects }

class function Tdwg12Arc.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFArc;
end;

procedure Tdwg12Arc.ReadEntityData(const AReader: Tdwg11Reader);
var
  vArc: TsgDXFArc;
  P: TFPoint;
begin
  inherited ReadEntityData(AReader);
  vArc := TsgDXFArc(FEntity);
  P := ReadPoint(AReader, False);
  vArc.Radius := AReader.ReadDouble;
  vArc.StartAngle := AReader.ReadAngle;
  vArc.EndAngle := AReader.ReadAngle;
  if FOptions and 1 <> 0 then
    vArc.Extrusion := ReadPoint(AReader, True);
  if FOptions and 2 <> 0 then
    P.Z := AReader.ReadDouble;
  vArc.Point := P;
end;

procedure Tdwg12Attdef.ReadEntityData(const AReader: Tdwg11Reader);
var
  vAttdef: TsgDXFAttdef;
begin
  inherited ReadEntityData(AReader);
  vAttdef := TsgDXFAttdef(FEntity);
  vAttdef.Point := ReadPoint(AReader, False);
  vAttdef.Height := AReader.ReadDouble;
  ReadDataStr(AReader);
  vAttdef.Flags := AReader.ReadByte;
  if FOptions and 1 <> 0 then//field_length
    AReader.Skip(1); //seek byte
  if FOptions and 2 <> 0 then
    vAttdef.Rotation := AReader.ReadAngle;
  if FOptions and 4 <> 0 then
    vAttdef.Scale := AReader.ReadDouble;
  if FOptions and 8 <> 0 then//obliquing_angle
    AReader.Skip(8); //seek double
  if FOptions and $10 <> 0 then//text_style
    AReader.Skip(1); //seek byte
  if FOptions and $20 <> 0 then//text_generation_flags
    AReader.Skip(1); //seek byte
  if FOptions and $40 <> 0 then
    vAttdef.HAlign := AReader.ReadByte;
  if FOptions and $80 <> 0 then
    vAttdef.Point1 := ReadPoint(AReader, False);
  if (AReader.Version >= acR10) and (FOptions and $100 <> 0) then
    vAttdef.Extrusion := ReadPoint(AReader, True);
end;

class function Tdwg12Attdef.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFAttdef;
end;

procedure Tdwg12Attdef.ReadDataStr(const AReader: Tdwg11Reader);
begin
  TsgDXFAttdef(FEntity).Text := AReader.ReadString;//default_value
  AReader.ReadString;//prompt_string
  TsgDXFAttdef(FEntity).Tag := AReader.ReadString;//tag_string
end;

class function Tdwg12Attrib.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFAttrib;
end;

procedure Tdwg12Attrib.ReadDataStr(const AReader: Tdwg11Reader);
begin
  TsgDXFAttrib(FEntity).Value := AReader.ReadString;
  TsgDXFAttrib(FEntity).Tag := AReader.ReadString;
end;

constructor Tdwg12Block.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FRefs := TList.Create;
  FBlockRecord := Tdwg12BlockRecord.Create(TsgDXFBlock(AEntity).BlockRecord, ALength);
  FBlockRecord.Block := Self;
end;

destructor Tdwg12Block.Destroy;
begin
  FRefs.Free;
  FBlockRecord.Free;
  inherited Destroy;
end;

procedure Tdwg12Block.Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  inherited Load(AReader, AOwner);
  if dwg12Load in FState then
    NotifyRefs(AReader, dwg12BlockLoaded);
end;

function Tdwg12Block.EOSection(const AReader: Tdwg11Reader): Boolean;
begin
  Result := inherited EOSection(AReader) or (SubEntKindRead = 13);
end;

class function Tdwg12Block.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFBlock;
end;

function Tdwg12Block.GetRefs: TList;
begin
  Result := FRefs;
end;

procedure Tdwg12Block.ReadEntityData(const AReader: Tdwg11Reader);
var
  vXRefPath: string;
  vBlock: TsgDXFBlockAccess;
begin
  inherited ReadEntityData(AReader);
  vBlock := TsgDXFBlockAccess(FEntity);
  vBlock.Name := BlockRecord.Name;
  TsgDXFBlockAccess(vBlock).SetFlags(BlockRecord.FFlags and $7F);
  vBlock.Offset := ReadPoint(AReader, False);
  if FOptions and $06 = 4 then
    {vBlock.Name := }AReader.ReadString
  else
    if FOptions and $02 <> 0 then
    begin
      vXRefPath := AReader.ReadString;
      if (vXRefPath <> '') and (vXRefPath[1] <> '*') and (vBlock.IsBlockByXRef) then
        vBlock.XrefPath := vXRefPath;
    end;
end;

class function Tdwg12Circle.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFCircle;
end;

procedure Tdwg12Circle.ReadEntityData(const AReader: Tdwg11Reader);
var
  vCircle: TsgDXFCircle;
  P: TFPoint;
begin
  inherited ReadEntityData(AReader);
  vCircle := TsgDXFCircle(FEntity);
  P := ReadPoint(AReader, False);
  vCircle.Radius := AReader.ReadDouble;
  if FOptions and 1 <> 0 then
    vCircle.Extrusion := ReadPoint(AReader, True);
  if FOptions and 2 <> 0 then
    P.Z := AReader.ReadDouble;
  vCircle.Point := P;
end;

class function Tdwg12Dim.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFDimension;
end;

procedure Tdwg12Dim.ReadEntityData(const AReader: Tdwg11Reader);
const
  cnstOptions: array[0 .. 9] of record
    BitPole: Word;
    Seek: Byte;
  end  = ((BitPole: $8; Seek: 24),(BitPole: $10; Seek: 24),(BitPole: $20; Seek: 24),
    (BitPole: $40; Seek: 24),(BitPole: $80; Seek: 8),(BitPole: $100; Seek: 8),
    (BitPole: $200; Seek: 8),(BitPole: $400; Seek: 8),(BitPole: $800; Seek: 8),
    (BitPole: $2000; Seek: 8));
var
  vDimension: TsgDXFDimension;
  vSeek: Integer;
  I: Integer;
begin
  inherited ReadEntityData(AReader);
  vDimension := TsgDXFDimension(FEntity);
  AReader.Skip(40);
  (*// Previous version
  ReadPoint(True);//(AReader.Version = acR12);
  ReadPoint(False);   *)
  if FOptions and 1 <> 0 then vDimension.Point := ReadPoint(AReader, False);
  if AReader.Version < acR12 then FOptions := FOptions and -2;
  if FOptions and 3 = 2 then vDimension.Flags := AReader.ReadByte;  // ????? !!!
  if FOptions and 4 <> 0 then AReader.ReadString;
  vSeek := 0;
  for I := Low(cnstOptions) to High(cnstOptions) do
  begin
    if FOptions and cnstOptions[I].BitPole <> 0 then
      Inc(vSeek,cnstOptions[I].Seek)
  end;
  AReader.Skip(vSeek);
  if FOptions and $4000 <> 0 then vDimension.Extrusion := ReadPoint(AReader, True);
end;

class function Tdwg12Face.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXF3dFace;
end;

procedure Tdwg12Face.ReadElevation(const AReader: Tdwg11Reader);
begin
  // not read
end;

procedure Tdwg12Face.ReadEntityData(const AReader: Tdwg11Reader);
var
  vFace: TsgDXF3dFace;
  function GetPoint(AFlagInfo: Word): TFPoint;
  begin
    Result.X := AReader.ReadDouble;
    Result.Y := AReader.ReadDouble;
    Result.Z := 0;
    if AReader.Version >= acR10 then
    begin
      if FFlags and 4 = 0 then
        Result.Z := AReader.ReadDouble;
    end
    else
      if FOptions and AFlagInfo <> 0 then
        Result.Z := AReader.ReadDouble;
  end;
begin
  inherited ReadEntityData(AReader);
  vFace := TsgDXF3dFace(FEntity);
  vFace.Point := GetPoint(1);
  vFace.Point1 := GetPoint(2);
  vFace.Point2 := GetPoint(4);
  vFace.Point3 := GetPoint(8);
end;

class function Tdwg12Insert.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFInsert;
end;

procedure Tdwg12Insert.ReadEntityData(const AReader: Tdwg11Reader);
var
  vInsert: TsgDXFInsert;
  vMInsert: TsgCADMInsert absolute vInsert;
  vScale: TFPoint;
begin
  inherited ReadEntityData(AReader);
  if FOptions and $F0 <> 0 then
  begin
    vMInsert := TsgCADMInsert.Create;
    vMInsert.AssignEntity(FEntity);
    FEntity.Free;
    FEntity := vMInsert;
  end
  else
    vInsert := TsgDXFInsert(FEntity);
  vScale := cnstFPointSingle;
  vInsert.Point := ReadPoint(AReader, False);
  if FOptions and $01 <> 0 then vScale.X := AReader.ReadDouble;
  if FOptions and $02 <> 0 then vScale.Y := AReader.ReadDouble;
  if FOptions and $04 <> 0 then vInsert.Angle := AReader.ReadAngle;
  if FOptions and $08 <> 0 then vScale.Z := AReader.ReadDouble;
  if FOptions and $0100 <> 0 then vInsert.Extrusion := ReadPoint(AReader, True);
  if FOptions and $10 <> 0 then vMInsert.NumCols := AReader.ReadWord;
  if FOptions and $20 <> 0 then vMInsert.NumRows := AReader.ReadWord;
  if FOptions and $40 <> 0 then vMInsert.ColSpacing := AReader.ReadDouble;
  if FOptions and $80 <> 0 then vMInsert.RowSpacing := AReader.ReadDouble;
  vInsert.Scale := vScale;
end;

class function Tdwg12Line.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFLine;
end;

procedure Tdwg12Line.ReadElevation(const AReader: Tdwg11Reader);
begin
  if AReader.Version < acR10 then
  begin
    AReader.PushElevation;
    Include(FState, dwg12Elevation);
  end;
end;

procedure Tdwg12Line.ReadEntityData(const AReader: Tdwg11Reader);
var
  vLine: TsgDXFLine;
  P, P1: TFPoint;
begin
  inherited ReadEntityData(AReader);
  vLine := TsgDXFLine(FEntity);
  if AReader.Version < acR10 then FFlags := FFlags or 4;
  P := ReadPoint(AReader, FFlags and 4 = 0);
  P1 := ReadPoint(AReader, FFlags and 4 = 0);
  if FOptions and 1 <> 0 then
    vLine.Extrusion := ReadPoint(AReader, True);
  if FOptions and 2 <> 0 then
  begin
    P.Z := AReader.ReadDouble;
    P1.Z := AReader.ReadDouble;
  end;
  vLine.Point := P;
  vLine.Point1 := P1;
end;

function Tdwg12PLine.EOSection(const AReader: Tdwg11Reader): Boolean;
begin
  Result := inherited EOSection(AReader) or (SubEntKindRead = 17);
end;

class function Tdwg12PLine.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFPolyline;
end;

procedure Tdwg12PLine.ReadEntityData(const AReader: Tdwg11Reader);
var
  vPolyline: TsgDXFPolyline;
begin
  inherited ReadEntityData(AReader);
  vPolyline := TsgDXFPolyline(FEntity);
  if FOptions and 1 <> 0 then
  begin
    vPolyline.Flags := AReader.ReadByte;
    vPolyline.Closed := vPolyline.Flags and 1 <> 0;
  end;
  if FOptions and 2 <> 0 then
    vPolyline.StartWidth := AReader.ReadDouble;
  if FOptions and 4 <> 0 then
    vPolyline.EndWidth := AReader.ReadDouble;
  if FOptions and 8 <> 0 then
    vPolyline.Extrusion := ReadPoint(AReader, True);
  if FOptions and $10 <> 0 then
    vPolyline.MeshM := AReader.ReadWord;
  if FOptions and $20 <> 0 then
    vPolyline.MeshN := AReader.ReadWord;
  if FOptions and $40 <> 0 then
  begin
    vPolyline.MeshM := AReader.ReadWord;
    vPolyline.MeshN := AReader.ReadWord;
  end;
end;

class function Tdwg12Point.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFPoint;
end;

procedure Tdwg12Point.ReadElevation(const AReader: Tdwg11Reader);
begin
  if AReader.Version < acR10 then
  begin
    AReader.PushElevation;
    Include(FState, dwg12Elevation);
  end;
end;

procedure Tdwg12Point.ReadEntityData(const AReader: Tdwg11Reader);
var
  vPoint: TsgDXFPoint;
  P: TFPoint;
begin
  inherited ReadEntityData(AReader);
  vPoint := TsgDXFPoint(FEntity);
  if AReader.Version < acR10 then FFlags := FFlags or 4;
  P := ReadPoint(AReader, FFlags and 4 = 0);
  if FOptions and 1 <> 0 then
    vPoint.Extrusion := ReadPoint(AReader, True);
  if FOptions and 2 <> 0 then
    P.Z := AReader.ReadDouble;
  vPoint.Point := P;
end;

{ Tdwg12Seqend

class function Tdwg12Seqend.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFSeqend;
end;
}

{ Tdwg12Solid }

class function Tdwg12Solid.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFSolid;
end;

procedure Tdwg12Solid.ReadEntityData(const AReader: Tdwg11Reader);
var
  vSolid: TsgDXFSolid;
begin
  inherited ReadEntityData(AReader);
  vSolid := TsgDXFSolid(FEntity);
  vSolid.Point := ReadPoint(AReader, False);
  vSolid.Point1 := ReadPoint(AReader, False);
  vSolid.Point2 := ReadPoint(AReader, False);
  vSolid.Point3 := ReadPoint(AReader, False);
  if FOptions and 1 <> 0 then
    vSolid.Extrusion := ReadPoint(AReader, True);
end;

class function Tdwg12Text.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFText;
end;

procedure Tdwg12Text.ReadEntityData(const AReader: Tdwg11Reader);
var
  vText: TsgDXFText;
  vIndex: Integer;
begin
  inherited ReadEntityData(AReader);
  vText := TsgDXFText(FEntity);
  vText.Point := ReadPoint(AReader, False);
  vText.Height := AReader.ReadDouble;
  vText.Text := AReader.ReadString;
  if FOptions and 1 <> 0 then
    vText.Rotation := AReader.ReadAngle;
  if FOptions and 2 <> 0 then
    vText.Scale := AReader.ReadDouble;
  if FOptions and 4 <> 0 then
    vText.ObliqueAngle := AReader.ReadAngle;
  if FOptions and 8 <> 0 then
    vIndex := AReader.ReadByte
  else
    AReader.FStyles.Find(cnstStandard, vIndex);
  if FOptions and $10 <> 0 then
    vText.Generation := AReader.ReadByte;
  if FOptions and $20 <> 0 then
    vText.HAlign := AReader.ReadByte;
  if FOptions and $40 <> 0 then
    vText.Point1 := ReadPoint(AReader, False);
  if (AReader.Version >= acR10) and (FOptions and $80 <> 0) then
    vText.Extrusion := ReadPoint(AReader, True);
  if FOptions and $100 <> 0 then
    vText.VAlign := AReader.ReadByte;
  AddRefsTo(AReader, csStyles, vIndex);
end;

class function Tdwg12Vertex.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFVertex
end;

procedure Tdwg12Vertex.ReadEntityData(const AReader: Tdwg11Reader);
var
  vVertex: TsgDXFVertex;
begin
  inherited ReadEntityData(AReader);
  vVertex := TsgDXFVertex(FEntity);
  if FOptions and $20 = 0 then
    vVertex.Point := ReadPoint(AReader, False);
  if FOptions and 1 <> 0 then
    vVertex.StartWidth := AReader.ReadDouble;
  if FOptions and 2 <> 0 then
    vVertex.EndWidth := AReader.ReadDouble;
  if FOptions and 4 <> 0 then
    vVertex.Bulge := AReader.ReadDouble;
  if FOptions and 8 <> 0 then
    vVertex.Flags := AReader.ReadByte;
  if vVertex.Flags = $80 then
  begin
    vVertex.PolyFaceVertexIndex1 := Smallint(AReader.ReadWord);
    vVertex.PolyFaceVertexIndex2 := Smallint(AReader.ReadWord);
    vVertex.PolyFaceVertexIndex3 := Smallint(AReader.ReadWord);
    if FOptions and $100 <> 0 then
      vVertex.PolyFaceVertexIndex4 := Smallint(AReader.ReadWord);
  end;
  if vVertex.Flags and $C0 <> 0 then
  begin
    vVertex.StartWidth := 0;
    vVertex.EndWidth := 0;
  end;
end;

{ Tdwg12NamedEntity }

constructor Tdwg12NamedEntity.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FLength := ALength;
end;

procedure Tdwg12NamedEntity.Commit(const AReader: Tdwg11Reader);
begin
  TsgDXFPenTableItem(Entity).Flags := FFlags;
  if Entity is TsgDXFLayer then
    TsgDXFConverterAccess.SetLayerNameOfPassingInspection(TsgDXFLayer(Entity), FName)
  else
    TsgDXFEntityAccess(Entity).SetName(FName);
end;

destructor Tdwg12NamedEntity.Destroy;
begin
  FRefs.Free;
  if not (dwg12Add in FState) then
    FEntity.Free;
  inherited Destroy;
end;

procedure Tdwg12NamedEntity.DoAddEntities(const AReader: Tdwg11Reader;
  const AOwner: Tdwg12Object);
begin
  if AOwner.AddEntity(Self) then
    Include(FState, dwg12Add);
end;

function Tdwg12NamedEntity.GetEntity: TsgDXFEntity;
begin
  Result := FEntity;
end;

function Tdwg12NamedEntity.GetLength: Integer;
begin
  Result := FLength;
  if Result = 0 then
    Result := 8;
end;

function Tdwg12NamedEntity.GetName: string;
begin
  Result := FName;
end;

function Tdwg12NamedEntity.GetRefs: TList;
begin
  Result := FRefs;
end;

function Tdwg12NamedEntity.ReadHeader(const AReader: Tdwg11Reader): Integer;
begin
  FFlags := AReader.ReadByte;
  Name := AReader.ReadName;
  Result := FLength;
end;

procedure Tdwg12NamedEntity.SetEntity(const AEntity: TsgDXFEntity);
begin
  FEntity := AEntity;
end;

procedure Tdwg12NamedEntity.SetName(const AValue: string);
begin
  FName := AValue;
end;

{ Tdwg12Layer }

procedure Tdwg12Layer.Commit(const AReader: Tdwg11Reader);
begin
  TsgDXFLayer(Entity).Flags := FFlags;
  TsgDXFConverterAccess.SetLayerNameOfPassingInspection(TsgDXFLayer(Entity), FName);
end;

constructor Tdwg12Layer.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FRefs := TList.Create;
end;

procedure Tdwg12Layer.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  inherited DoAddEntities(AReader, AOwner);
  NotifyRefs(AReader, dwg12Layer);
end;

class function Tdwg12Layer.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFLayer;
end;

procedure Tdwg12Layer.ReadEntityData(const AReader: Tdwg11Reader);
var
  vColor: SmallInt;
  vIndex: SmallInt;
begin
  vColor := AReader.ReadWord;
  vIndex := AReader.ReadWord;
  if vColor < 0 then
  begin
    Entity.Visible := False;
    vColor := -vColor;
  end;
  Entity.ColorCAD := MakeColorCAD(acIndexColor, vColor);
  AddRefsTo(AReader, csLTypes, vIndex);
end;

{ Tdwg12LType }

constructor Tdwg12LType.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FRefs := TList.Create;
end;

procedure Tdwg12LType.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  inherited DoAddEntities(AReader, AOwner);
  NotifyRefs(AReader, dwg12LType);
end;

class function Tdwg12LType.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFLineType;
end;

procedure Tdwg12LType.ReadEntityData(const AReader: Tdwg11Reader);
var
  vTicks: array[0 .. 12] of Double;
  vCount: Byte;
  I: Integer;
begin
  AReader.Skip(49);
  vCount := AReader.ReadByte;
  if vCount > 12 then
    vCount := 12;
  if vCount > 0 then
  begin
    AReader.Stream.ReadBuffer(vTicks[0], SizeOf(Double) * (vCount + 1));
    //LR.vTicks[0] contains PatternLength
    for I := 1 to vCount do
      TsgDXFLineType(Entity).Lines.AddTick(vTicks[I]);
  end;
end;

{ Tdwg12BlockRecord }

procedure Tdwg12BlockRecord.Commit(const AReader: Tdwg11Reader);
begin
  TsgDXFObjectEntityAccess(Entity).SetFlags(FFlags);
  TsgDXFObjectEntityAccess(Entity).SetName(Name);
end;

constructor Tdwg12BlockRecord.Create(const AEntity: TsgDXFEntity;
  ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FOffset := -1;
end;

destructor Tdwg12BlockRecord.Destroy;
begin
  FBlock := nil;
  inherited Destroy;
end;

class function Tdwg12BlockRecord.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFBlockRecord;
end;

procedure Tdwg12BlockRecord.ReadEntityData(const AReader: Tdwg11Reader);
begin
  FOffset := AReader.ReadInteger and $3FFFFFFF;
  if SameText(FName, sModelSpace12) or SameText(FName, sPaperSpace12) then
    FName[1] := '*';
  if FFlags and $01 <> 0 then
    FName := FName + IntToStr(AReader[csBlockRecords].Count);
  if FFlags and $80 <> 0 then
    FName := '*U' + IntToStr(AReader[csBlockRecords].Count);
end;

{ Tdwg12VPort }

class function Tdwg12VPort.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFVport;
end;

procedure Tdwg12VPort.ReadEntityData(const AReader: Tdwg11Reader);
type
  TVPortData = packed record
    Data1, Data2, Data3, Data4: Double;
    ViewTarget: TFPoint;
    ViewDirrection: TFPoint;
    ViewTwistAngle: Double;
    ViewHeight: Double;
    VewCenter: TF2DPoint;
    ViewAspectRatio: Double;
    LensLength: Double;
    FrontPlane: Double;
    BackPlan: Double;
    Data5: array[0 .. 1] of Byte;
    CircleZoomPercent: Byte;
    Data6: array[0 .. 4] of Byte;
    Data7: array[0 .. 7] of Double;
  end;
var
  vValues: TVPortData;
  vVPort: TsgDXFVport;
begin
  vVPort := TsgDXFVport(Entity);
  AReader.Stream.ReadBuffer(vValues, SizeOf(vValues));
  vVPort.ViewTarget := vValues.ViewTarget;
  vVPort.ViewDirection := vValues.ViewDirrection;
  vVPort.ViewTwistAngle := vValues.ViewTwistAngle * f180DividedByPi;
  vVPort.ViewHeight := vValues.ViewHeight;
  vVPort.ViewCenterPoint := MakeFPointFrom2D(vValues.VewCenter);
  vVPort.ViewAspectRatio := vValues.ViewAspectRatio;
  vVPort.CircleZoomPercent := vValues.CircleZoomPercent;
end;

{ Tdwg12Dummy }

procedure Tdwg12Dummy.Commit(const AReader: Tdwg11Reader);
begin
  // do nothing
end;

class function Tdwg12Dummy.CreateEntity: TsgDXFEntity;
begin
  Result := nil;
end;

procedure Tdwg12Dummy.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  // do nothing
end;

procedure Tdwg12Dummy.ReadEntityData(const AReader: Tdwg11Reader);
begin
  // do nothing
end;

{ Tdwg12Trace }

class function Tdwg12Trace.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFTrace;
end;

{ Tdwg12Table }

procedure Tdwg12Table.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoAddEntities(AReader, Self);
  Include(FState, dwg12Add);
end;

function Tdwg12Table.GetCheckSize: Integer;
begin
  Result := 32;
end;

procedure Tdwg12Table.Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  if not (dwg12Load in FState) then
  begin
    Include(FState, dwg12Load);
    LoadItems(AReader);
  end;
end;

procedure Tdwg12Table.ReadKind(const AReader: Tdwg11Reader);
begin
end;

{ Tdwg12Group }

function Tdwg12Group.AddItem(AObj: Tdwg12Object): Integer;
begin
  if AObj.ClassType = Tdwg12Dummy then
    Result := -1
  else
    Result := FItems.Add(AObj);
end;

constructor Tdwg12Group.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FItems := TList.Create;
end;

procedure Tdwg12Group.DeleteItem(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor Tdwg12Group.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function Tdwg12Group.GetCheckSize: Integer;
begin
  Result := 0;
end;

function Tdwg12Group.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function Tdwg12Group.GetItems(Index: Integer): Tdwg12Object;
begin
  Result := FItems[Index];
end;

function Tdwg12Group.GetSubEntKindRead: Word;
begin
  Result := FSubEntKindRead;
end;

function Tdwg12Group.IndexOf(AObj: Tdwg12Object): Integer;
begin
  Result := FItems.IndexOf(AObj);
end;

procedure Tdwg12Group.InsertItem(AIndex: Integer; AObj: Tdwg12Object);
begin
  FItems.Insert(AIndex, AObj);
end;

procedure Tdwg12Group.LoadItems(const AReader: Tdwg11Reader);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Load(AReader, Self);
end;

procedure Tdwg12Group.ReadSubEnts(const AReader: Tdwg11Reader;
  var AStart, AEnd: Integer; ALength: Integer);
var
  Obj: Tdwg12Object;
begin
  Dec(AEnd, GetCheckSize); // check_32
  while not EOSection(AReader) and (AStart < AEnd) do
  begin
    AReader.Stream.Position := AStart;
    ReadKind(AReader);
    Obj := SubEntCreate(AReader, ALength);
    Obj.ReadState(AReader, AStart, AEnd);
    if AddItem(Obj) < 0 then
      Obj.Free;
  end;
end;

function Tdwg12Group.RemoveItem(AObj: Tdwg12Object): Integer;
begin
  Result := FItems.Remove(AObj);
end;

procedure Tdwg12Group.SetItems(Index: Integer; const AValue: Tdwg12Object);
begin
  FItems[Index] := AValue
end;

procedure Tdwg12Group.SetSubEntKindRead(const AValue: Word);
begin
  FSubEntKindRead := AValue;
end;

{ Tdwg12Blocks }

procedure Tdwg12Blocks.DeleteItem(AIndex: Integer);
begin
  inherited DeleteItem(AIndex);
  FReader[csBlockRecords].DeleteItem(AIndex);
end;

procedure Tdwg12Blocks.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoAddEntities(AReader, Self);
  Include(FState, dwg12Add);
end;

function Tdwg12Blocks.GetCheckSize: Integer;
begin
  Result := 32;
end;

procedure Tdwg12Blocks.ReadState(const AReader: Tdwg11Reader; var AStart, AEnd: Integer);
begin
  ReadSubEnts(AReader, AStart, AEnd, 0);
end;

function Tdwg12Blocks.RemoveItem(AObj: Tdwg12Object): Integer;
begin
  Result := inherited RemoveItem(AObj);
  FReader[csBlockRecords].RemoveItem(Tdwg12Block(AObj).BlockRecord);
end;

function Tdwg12Blocks.AddEntity(const AObj: Tdwg12Object): Boolean;
begin
  Result := True;
  if not (dwg12Add in AObj.FState) then
    Result := inherited AddEntity(AObj);
end;

function Tdwg12Blocks.AddItem(AObj: Tdwg12Object): Integer;
begin
  Result := IndexOf(AObj);
  if Result < 0 then
    Result := inherited AddItem(AObj);
end;

function Tdwg12Blocks.BlockIndexByOffset(const AReader: Tdwg11Reader;
  const APos: Integer; var AIndex: Integer): Boolean;
var
  vOffset: Integer;
  vBlockRecords: Tdwg12Object;
begin
  vOffset := APos - FBlocksStart;
  AIndex := 0;
  Result := False;
  vBlockRecords := AReader[csBlockRecords];
  while not Result and (AIndex < vBlockRecords.Count) do
    if Tdwg12BlockRecord(vBlockRecords[AIndex]).Offset = vOffset then
      Inc(Result)
    else
      Inc(AIndex);
end;

function Tdwg12Blocks.SubEntCreate(const AReader: Tdwg11Reader;
  const ALength: Integer): Tdwg12Object;
var
  I: Integer;
begin
  if BlockIndexByOffset(AReader, AReader.Stream.Position - 1, I) then
    Result := Tdwg12BlockRecord(AReader[csBlockRecords][I]).Block
  else
    Result := Tdwg12Dummy.Create;
  AReader.FBlock := Result;
  AReader.FOwner := Result;
  AReader.FPaperIndex := AReader.FLayouts.IndexOf(Result);
  if AReader.FPaperIndex < 0 then
    AReader.FPaperIndex := 0;
end;

{ Tdwg12CustomInsert }

function Tdwg12CustomInsert.EOSection(const AReader: Tdwg11Reader): Boolean;
begin
  Result := inherited EOSection(AReader) or (FFlags and $80 = 0) or (SubEntKindRead = 17);
end;

procedure Tdwg12CustomInsert.Load(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  if not (dwg12Load in FState) then
  begin
    if not (dwg12BlockLoaded in FState) then
    begin
      if Assigned(FBlock) then
        FBlock.Load(AReader, Self);
    end;
    if not (dwg12Load in FState) then
    begin
      Include(FState, dwg12Load);
      LoadItems(AReader);
      AReader.Conv.Loads(Entity);
    end;
  end;
end;

procedure Tdwg12CustomInsert.Notify(const AReader: Tdwg11Reader;
  AObj: Tdwg12Object; ANotify: Tdwg12Notify);
begin
  inherited Notify(AReader, AObj, ANotify);
  if ANotify = dwg12BlockLoaded then
    Load(AReader, nil);
end;

procedure Tdwg12CustomInsert.ReadBlock(const AReader: Tdwg11Reader);
begin
  FBlock := Tdwg12Block(AddRefsTo(AReader, csBlocks, AReader.ReadWord));
end;

procedure Tdwg12CustomInsert.ReadEntityData(const AReader: Tdwg11Reader);
begin
  inherited ReadEntityData(AReader);
  ReadBlock(AReader);
end;

{ Tdwg12Viewport }

procedure Tdwg12Viewport.Commit(const AReader: Tdwg11Reader);
begin
  inherited Commit(AReader);
  AReader.UpdateViewportEntHeader(Self);
end;

class function Tdwg12Viewport.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFViewport;
end;

procedure Tdwg12Viewport.ReadXData(const AReader: Tdwg11Reader);
var
  vExtData: TsgCADExtendedData;
begin
  vExtData := TsgCADExtendedData.Create(AReader.Version);
  try
    ReadDwg12EED(AReader.Stream, vExtData);
    ApplyEED(AReader, vExtData, 0);
  finally
    vExtData.Free;
  end;
end;

procedure Tdwg12Viewport.ApplyEED(const AReader: Tdwg11Reader;
  const AEEData: TsgCADExtendedData; const AAppID: UInt64);
var
  I: Integer;
  vViewPort: TsgDXFViewport;
  vXPaper, vCount_10, vCount_40, vCount_70: Integer;
begin
  vCount_10 := 0;
  vCount_40 := 0;
  vCount_70 := 0;
  vViewPort := TsgDXFViewport(FEntity);
  I := 0;
  while I < AEEData.DataCount do
  begin
    case AEEData.DataType[I] of
      edtint64:
        case AEEData.DataCode[I] of
          1003: vViewPort.FrozenLayers.Add(AEEData.DataInt64[I]);
          1005: vViewPort.Handle := AEEData.DataInt64[I];
        end;
      edtF3DPoint:
        begin
          Inc(vCount_10);
          case vCount_10 of
            1: vViewPort.ViewTarget := AEEData.DataPoint[I];
            2: vViewPort.ViewDirection := AEEData.DataPoint[I];
          end;
        end;
      edtDouble:
        begin
          Inc(vCount_40);
          case vCount_40 of
            1: vViewPort.ViewTwistAngle := AEEData.DataDouble[I] * f180DividedByPi;
            2: vViewPort.MSpaceHeight := AEEData.DataDouble[I];
            3: vViewPort.Offset(MakeFPoint(AEEData.DataDouble[I], 0, 0));
            4: vViewPort.Offset(MakeFPoint(0, AEEData.DataDouble[I], 0));
            6: vViewPort.FrontClipPlane := AEEData.DataDouble[I];
            7: vViewPort.BackClipPlane := AEEData.DataDouble[I];
          end;
        end;
      edtInt16:
        begin
          Inc(vCount_70);
          case vCount_70 of
            5:
              begin
                vXPaper := AEEData.DataInt16[I];
                if vXPaper = AReader.FLayouts.IndexOf(AReader.FBlock) then
                begin
                  if vXPaper = 1 then
                    AReader.ForceLayout(AReader.FPaperCount)
                  else
                    AReader.ForceLayout(vXPaper);
                end
                else
                  AReader.ForceLayout(vXPaper);
                AReader.FPaperIndex := AReader.FLayouts.IndexOf(AReader.FBlock);
                AReader.FOwner := AReader.FBlock;
              end;
          end;
        end;
    end;
    Inc(I);
  end;
end;

procedure Tdwg12Viewport.ReadEntityData(const AReader: Tdwg11Reader);
var
  vViewport: TsgDXFViewport;
begin
  inherited ReadEntityData(AReader);
  vViewport := TsgDXFViewport(Entity);
  vViewport.PSpaceCenter := ReadPoint(AReader, True);
  vViewport.PSpaceWidth := AReader.ReadDouble;
  vViewport.PSpaceHeight := AReader.ReadDouble;
  vViewport.StatusField := AReader.ReadWord;
  { TODO: Def layout Viewport StatusField = 1 ??}
end;

procedure Tdwg12Viewport.ReadState(const AReader: Tdwg11Reader; var AStart,
  AEnd: Integer);
begin
  FStart := AStart;
  inherited ReadState(AReader, AStart, AEnd);
end;

{ Tdwg12BlockRecords }

function Tdwg12BlockRecords.AddEntity(const AObj: Tdwg12Object): Boolean;
begin
  Result := inherited AddEntity(AObj);
  if Result then
    Include(Tdwg12BlockRecord(AObj).Block.FState, dwg12Add);
end;

function Tdwg12BlockRecords.AddItem(AObj: Tdwg12Object): Integer;
begin
  Result := inherited AddItem(AObj);
  FReader[csBlocks].AddItem(Tdwg12BlockRecord(AObj).Block);
end;

class function Tdwg12BlockRecords.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFBlockRecords;
end;

procedure Tdwg12BlockRecords.InsertItem(AIndex: Integer; AObj: Tdwg12Object);
begin
  inherited InsertItem(AIndex, AObj);
  FReader[csBlocks].InsertItem(AIndex, Tdwg12BlockRecord(AObj).Block);
end;

function Tdwg12BlockRecords.SubEntCreate(const AReader: Tdwg11Reader;
  const ALength: Integer): Tdwg12Object;
var
  vBlock: Tdwg12Block;
begin
  vBlock := Tdwg12Block.Create(Tdwg12Block.CreateEntity, ALength);
  Result := vBlock.BlockRecord;
end;

{ Tdwg12Style }

constructor Tdwg12Style.Create(const AEntity: TsgDXFEntity; ALength: Integer);
begin
  inherited Create(AEntity, ALength);
  FRefs := TList.Create;
end;

class function Tdwg12Style.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFStyle;
end;

procedure Tdwg12Style.ReadEntityData(const AReader: Tdwg11Reader);
var
  vStyle: TsgDXFStyle;
begin
  vStyle := TsgDXFStyle(FEntity);
  vStyle.FixedHeight := AReader.ReadDouble;
  vStyle.WidthFactor := AReader.ReadDouble;
  vStyle.ObliqueAngle := AReader.ReadAngle;
  vStyle.TextGenFlags := AReader.ReadByte;
  vStyle.LastHeightUsed := AReader.ReadDouble;
  vStyle.PrimaryFont := AReader.ReadStringLen(128);
end;

procedure Tdwg12Style.DoAddEntities(const AReader: Tdwg11Reader; const AOwner: Tdwg12Object);
begin
  inherited DoAddEntities(AReader, AOwner);
  NotifyRefs(AReader, dwg12Style);
end;

{ Tdwg12AppID }

class function Tdwg12AppID.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFAppID;
end;

{ Tdwg12ViewportEntHdr }

procedure Tdwg12ViewportEntHdr.Commit(const AReader: Tdwg11Reader);
begin
  inherited Commit(AReader);
  if Name = '1' then
    AReader.ActiveViewportEntHdr := Self;
end;

class function Tdwg12ViewportEntHdr.GetEntityClass: TsgDXFEntityClass;
begin
  Result := TsgDXFOwneredItem;
end;

procedure Tdwg12ViewportEntHdr.ReadEntityData(const AReader: Tdwg11Reader);
begin
  FOffset := AReader.ReadInteger;
end;

{ Tdwg12ViewportEntHdrTable }

function Tdwg12ViewportEntHdrTable.IndexOfOffset(AOffset: Integer;
  var AIndex: Integer): Boolean;
begin
  AIndex := 0;
  Result := False;
  while not Result and (AIndex < Count) do
    if Tdwg12ViewportEntHdr(Items[AIndex]).FOffset = AOffset then
      Inc(Result)
    else
      Inc(AIndex);
end;

initialization

  InitDWG12Classes;

finalization

  FinalDWG12Classes;

end.

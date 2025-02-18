{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{             Common classes, types and functions            }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools?                       }
{************************************************************}

unit sgFunction;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF SGDEL_6}
    {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  {$DEFINE SG_SYSTEM_PROCS}
{$ENDIF}

{$IFDEF SGFPC}
  {$ASMMODE intel}
  {$DEFINE SG_SYSTEM_PROCS}
{$ENDIF}

{$IFNDEF SG_NON_WIN_PLATFORM}
  {$DEFINE SG_USE_DC}
{$ELSE}
   {$IFDEF SG_CADSERVICE_CLIENT}//linux
      {$DEFINE SG_USE_DC}
   {$ENDIF}
{$ENDIF}

{$IFNDEF SGDEL_XE}
  {$DEFINE SG_RegExpr}
{$ENDIF}

{$DEFINE SG_USE_VIEW_RECORD}

interface

uses
  sgLists, //must be here only for compiller warning off
{$IFDEF MSWINDOWS}Windows, ShellApi,{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, LCLProc, FPimage, GraphType, IntfGraphics,{$IFDEF LINUX} BaseUnix,{$ENDIF}
{$ENDIF}
  SysUtils, SyncObjs,
{$IFDEF SGDEL_XE}
  {$IFDEF SGDEL_XE2}System.RegularExpressions{$ELSE}RegularExpressions{$ENDIF},
{$ELSE}
  {$IFDEF SG_RegExpr}
   RegExpr,
  {$ENDIF}
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, IOUtils, FMX.Types, sgProxyGraphics, System.UIConsts,
  System.Devices, FMX.Platform,
  {$IFDEF SG_FM_WINDOWS}
  Registry, Messages,
  {$ENDIF}
{$ELSE}
  Graphics, Registry, Messages,
  {$IFDEF SGDEL_2009}
    {$IFDEF SGDEL_XE2}Vcl.Imaging.PNGImage{$ELSE}PNGImage{$ENDIF},
  {$ENDIF}
{$IFDEF SG_DELPHI_VCL}
  {$IFDEF SGDEL_XE2}Vcl.Imaging.jpeg,{$ELSE}jpeg,{$ENDIF}
{$ENDIF}
{$ENDIF}
  {$IFNDEF SG_NON_WIN_PLATFORM}
   ShlObj,
  {$ENDIF}
  Classes, Math
  {$IFNDEF SGFPC}
    {$IFNDEF SG_FIREMONKEY}
  ,Consts
    {$ENDIF}
  {$ENDIF}
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  {$IFNDEF SG_FIREMONKEY}
  ,sgBitmap
  {$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_5}
{$IFDEF SGDEL_6}
  , DateUtils
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  , ActiveX
{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_XE8}
  , System.Net.HttpClient, System.Net.HTTPClientComponent, System.Net.URLClient
{$ELSE}
{$IFDEF SGDEL_7}
{$IFDEF SG_USE_SYNAPSE}
  ,HTTPSend, ssl_openssl
{$ELSE}
{$IFDEF SG_USE_INDY}
  ,IdHTTP, IdSSLOpenSSL, IdAuthentication, IdComponent
{$IFDEF SG_INDY_HAS_ANTIFREEZE}
  ,IdAntiFreeze, IdAntiFreezeBase
{$ENDIF}
{$ELSE}
{$IFDEF SG_USE_WEB_LAZ}
  , fphttp, fphttpclient, ssockets, sslsockets, openssl, opensslsockets
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF HAS_FEATURE_ANSISTRINGS}
  , AnsiStrings
{$ENDIF}
{$IFDEF SGDEL_2009}
  , Character
{$ENDIF}
{$IFDEF SGDEL_6}
  {$IFDEF SGDEL_XE2}, System.Types, System.UITypes{$ELSE}, Types{$ENDIF}
{$ENDIF}
  ,sgConsts, {$IFNDEF SG_FIREMONKEY}Clipbrd, {$ENDIF}IniFiles;

const
  cnstDoubleSeparator = ',';
  cnstDecimalSeparatorPoint = '.';
  cnstPointSeparator = ';';
  cnstFilterSeparator = ';*';
  cnstVertLine = '|';
  sBinToHex: AnsiString = '0123456789ABCDEF';
  fDimAccuracy = 0.000000000001;
  cnstMetafileDefRatio = 1000;
  MaxWord = {$IFDEF SGDEL_6}MAXWORD{$ELSE}Word($FFFF){$ENDIF};
  cnstDirectorySeparator: string = '';
  cnstExtensionSeparator: string = '';

type
  PByteBinToHex = ^TByteBinToHex;
  TByteBinToHex = array[Byte] of Word;
  TsgBoxType = (bxEmpty, bxX, bxY, bxZ, bxXY, bxXZ, bxYZ, bxXYZ);

  TsPositionOnEntities = (peNone, peOnArc, peOnSegment, peOnArcAndSegment);

  TsgVersionWindows = (vrUndefined, vrWinXP, vrWinVista, vrWin7,
    vrWin8, vrWin8_1, vrWin10, vrWin11, vrWin2000, vrWinServer2003,
    vrWinServer2003R2, vrWinServer2008, vrWinServer2008R2, vrWinServer2012,
    vrWinServer2012R2, vrWinServer2016, vrWinServer2019, vrWinServer2022);

{$IFDEF SGDEL_11_ALEXANDRIA}
(*$HPPEMIT 'class TsgUTF8String : public System::UnicodeString {};' *)
{$ENDIF}
  TsgUTF8String = type string;
{$IFDEF SGDEL_11_ALEXANDRIA}
  {$NODEFINE TsgUTF8String}
{$ENDIF}
  PsgUTF8String = ^TsgUTF8String;

{$IFNDEF SGDEL_6}
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);
  TValueSign = -1..1;
{$ENDIF}

  PMethod = ^TMethod;

{$IFNDEF SGFPC}
{$IFDEF SGDEL_6}
  PRect = {$IFDEF SGDEL_XE2}System.{$ENDIF}Types.PRect;
{$ENDIF}
{$ENDIF}

{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
  PFormatSettings = ^TFormatSettings;
{$ELSE}
  PFormatSettings = Pointer;
{$ENDIF}

  TsgDataHttpParams = record
    {$IFDEF SGDEL_XE8}
    OnReceiveData: TReceiveDataEvent;
    {$IFDEF SGDEL_10_SYDNEY}
    OnSendData: TSendDataEvent;
    {$ENDIF}
    {$ENDIF}
  end;
  PsgDataHttpParams = ^TsgDataHttpParams;


const
  cnstBoxTypes2D: set of TsgBoxType = [bxXY, bxXZ, bxYZ];
  cnstBoxTypesIncorrect: set of TsgBoxType = [bxEmpty, bxX, bxY, bxZ];

{ Codes by TsgConstantsCustom }
  cnstCodeType_Bool = $10000;
  cnstCodeType_Int = $20000;
  cnstCodeType_Str = $30000;

  cnstCode_AddBTIEntityMode = cnstCodeType_Int + 1;
  cnstCode_AreaCreateMode = cnstCodeType_Int + 2;
  cnstCode_OnSelectBTIEntities = cnstCodeType_Int + 3;
  cnstCode_SelectByInsideArea = cnstCodeType_Int + 4;
  cnstCode_SnapInserts = cnstCodeType_Int + 5;
  cnstCode_UserAppCaption = cnstCodeType_Str + 6;
  cnstCode_MarkPoints = cnstCodeType_Int + 7;

type
{$IFDEF SGDEL_2005}
  TStringsDefined = Classes.TStringsDefined;
{$ELSE}
  TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator, sdLineBreak);
{$ENDIF}
  TsgProc = function(const Value: Pointer): Pointer;
  TsgProcMode = function(const Value: Pointer; const AMode: Integer): Pointer;
  TsgProcAddPointInList = procedure(const AList: TObject; const APoint: TFPoint);
  TsgProcedure = procedure of object;
  TsgProcObjCreate = function: TObject;
  TsgProcString = function: string;
  TsgProcCopy = function(const Value1, Value2: Pointer): Pointer;
  TsgProcCopyList = procedure(const Value1, Value2, Value3: Pointer);
  TsgProcCompare = function(const Value1, Value2: Pointer): Integer;
  TsgProcCompareStrings = function(const Value1, Value2: string): Integer;
  TsgProcEqual = function(const Value1, Value2: Pointer): Boolean;
  TsgProcObjectEqual = function(const Value1, Value2: Pointer): Boolean;
  TsgProcObjectByName = function(const ASender: TObject; const AName: string): TObject;
  TsgProcAddVertex = function(const AEntity: TObject; const APoint: TFPoint;
    const ABulge: Double): TObject;
  TsgProcBool = function(const Value: Pointer): Boolean;
  TsgCalcMargins = function (const APrinterName: string; vFormat: PPaperFormat): TF2DRect;
  TsgIsMatch = function (const AFilter, AIgnoreFilter, AName: string): Boolean;

  TsgProcOfListAddPoint = procedure(const AList: Pointer; const APoint: TFPoint);
  TsgProcOfListGetPoint = function(const AList: Pointer; const AIndex: Integer): TFPoint;
  TsgProcOfListAddSingle = procedure(const AList: Pointer; const AValue: Single);
  TsgProcOfListGetSingle = function(const AList: Pointer; const AIndex: Integer): Single;

  TsgProcOfPointerGetPoint = function(const AObject: Pointer): TFPoint;
  TsgProcOfPointerGetF2DPoint = function(const APointer: Pointer): TF2DPoint;
  TsgProcCopyData = procedure(const AData, AList: Pointer; const Added: Boolean);
  TsgObjProc = function(const Value: Pointer): Pointer of object;
  TsgObjProcBool = function: Boolean of object;
  TsgObjProcFPoint = function: TFPoint of object;
  TsgObjProcCompare = sgConsts.TsgObjProcCompare;
  TsgObjProcString = function(const AKey: string): string of object;
  TsgObjProcTranslate = function(const AKey: string): string of object;
  TsgObjProcStr = procedure(const Value: string) of object;
  TsgObjProcMode = function(const Value: Pointer; const AMode: Integer): Pointer of object;
  TsgObjProcGetPoint = function(const Value: TFPoint): TPoint of object;
  TsgObjChangeProperties = procedure(const Sender: TObject; const AChange: TsgPropertyType) of object;
  TsgObjProcPFPoint = procedure(const Value: PFPoint) of object;
  TsgObjProcDraw = procedure(const ACanvas: TCanvas; const ARect: TRect) of object;
  TsgObjProcObjectByName = function(const AName: string): TObject of object;

  TsgObjProgressStage = (stStarting, stRunning, stEnding);
  TsgObjProgressEvent = procedure (Sender: TObject; Stage: TsgObjProgressStage;
    PercentDone: Byte; const Msg: string) of object;

  TsgCreatePPoint = function(X, Y, Z: TsgFloat): Pointer;

  TsgObjMetafileExport = procedure(const ACanvas: TCanvas; const ARect: TRect) of object;

  TCADNotifyEvent = procedure(const Image, AEntities: TObject) of object;
  TCADGetInfoEvent = procedure(const Image: TObject;
    const AName, AXML: string;
    const AttribsData: TStrings) of object;
  TCADConfirmEvent = procedure(const Image, AEntities: TObject;
    var AValue: Integer) of object;
  TCADImageLoad = procedure(ASender: TObject; const AState: Integer;
    const AAttribData: TStrings) of object;
  TCADImageSave = procedure(ASender: TObject; const AFileName: string;
    const AFormat: TsgExportFormat; const AStream: TStream;
    const AAttribData: TStrings) of object;
  TCADImageClose = procedure(ASender: TObject;
    const AAttribData: TStrings) of object;
  TCommandResult = (crFulfilled, crNotAccessible, crNotfound);
  TsgActionEvent = function (S: string; AParCount: Integer): TCommandResult of object;

  TsgExpPixel = procedure(APoint: TPoint; AColor: TColor) of object;
  TsgExpPolyline = TsgPointsProc;
  TsgExpPolygon = TsgPointsProc;
  TsgExpPolyPolyline = TsgPolyPointsProc;
  TsgExpPolyPolygon = TsgPolyPointsProc;
  TsgExpText = procedure(ATextType: TsgTextType; X, Y: Integer; Options: Longint; Rect: PRect; StrWideChar: PWideChar; StrChar: PAnsiChar; Count: Integer; const Dx: PInteger) of object;
  TsgExpTextOut = procedure (X, Y: Integer; const ATextA: AnsiString;
    const ATextW: WideString; const ATextParams: PsgExpTextParam) of object;
  TsgExpExportBlock = procedure (const blockString: string) of object; // 최승선 수정    TsgExpExportBlock 추가
  TsgExpArc = procedure(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer) of object;
  TsgExpFillRgn = procedure(P: PRect; Count: Integer) of object;
  TsgExpClipRgn = procedure(P: PRect; Count: Integer) of object;
  TsgExpCloseFigure = procedure of object;
  TsgExpImage = procedure(const R: TRect; AImage: TPersistent) of object;
  TsgExpImageUV = procedure(const APoint1, APoint2, APoint3, APoint4: TPoint;
    AImage: TPersistent) of object;
  TsgExpInitView = procedure(Sender: TObject; R: TRect) of object;
  TsgExpSaveDC = procedure of object;
  TsgExpRestoreDC = procedure of object;
  TsgExpProgress = procedure of object;
  TsgExpSetColor = procedure(AColor: TColor; AColorType: TsgColorType) of object;
  TsgExpSetFont = procedure(AFont: TFont) of object;
  TsgExpSetStyle = procedure(AStyle: Integer; AColorType: TsgColorType) of object;
  TsgExpSetPenWidth = procedure(AWidth: Double) of object;
  TsgExpSetPenMiterLimit = procedure(ANewLimit: Single) of object;
  TsgExpSetPenGeometric = procedure(AStyle: DWORD) of object;
  TsgExpAnnotation = procedure(AEnt: TObject; const AParams: TsgExpAnnotParam) of object;
  TsgExpObject = procedure(AEnt: TObject; const AState: Integer) of object;
  TsgExpPathData = procedure(const AHandle: THandle; const Edge, Fill: Boolean)
   of object;

  TsgCriptMethod = function(const AStr, AKey: AnsiString): AnsiString;

  EJoinInfLoop = class(Exception);

  TsgClipboardStream = class(TMemoryStream)
  public
    SourceId: UInt64;
    SourceName: string;
  end;

  TsgClipBordFormat = (cfUndefined, cfText, cfCadImage, cfBitmap, cfMetafile,
    cfPicture);

  TsgClipboardBase = class
  private
{$IFNDEF SG_FIREMONKEY}
    FClip: TClipboard;
{$ENDIF}
  protected
    procedure SaveAppHandle; virtual;
    procedure RestoreAppHandle; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);
    procedure AssignTo(Target: TPersistent);
    procedure Clear;
    function GetAsHandle(Format: Word): THandle;
    function HasFormat(Format: Word): Boolean;
    procedure Open;
    function GetRefCount: Integer;
    procedure Close;
    function GetClipBordFormat: TsgClipBordFormat;
    function GetBufferText(const AConvertToDXFUnicode: Boolean): string;
  end;

  TsgConstantsCustom = class
  private
    FNames: TStringList;
    FCodes: TsgCollection;
    FChangeValues: Boolean;
    procedure SetChangeValues(const AValue: Boolean);
  protected
    function GetCodeByName(const AName: string): Integer;
    function GetValueByCode(const ACode: Integer;
      const AType: Integer = 0): Pointer;
    procedure RegsitredCode(const ACode: Integer;
      const AName: string; const AValue: Pointer);

    function GetBoolValue(const ACode: Integer): Boolean; virtual;
    function GetIntValue(const ACode: Integer): Integer; virtual;
    function GetStrValue(const ACode: Integer): string; virtual;
    procedure SetBoolValue(const ACode: Integer; const AValue: Boolean); virtual;
    procedure SetIntValue(const ACode: Integer; const AValue: Integer); virtual;
    procedure SetStrValue(const ACode: Integer; const AValue: string); virtual;

    function LoadFromIni(const AOptions: TCustomIniFile): Boolean; virtual; abstract;
    function SaveToIni(const AOptions: TCustomIniFile): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetDefaults; virtual;

    function Load(const AOptions: TObject): Boolean;
    function Save(const AOptions: TObject): Boolean;

    function SetValue(const ASection, APropName, AValue: string): Boolean; overload;
    function SetValue(const ACode: Integer; AValue: string): Boolean; overload;
    function GetValue(const ASection, APropName: string; var AValue: string): Boolean; overload;
    function GetValue(const ACode: Integer; var AValue: string): Boolean; overload;

    function GetCodeType(const ACode: Integer): Integer; virtual;
    property BoolValue[const ACode: Integer]: Boolean read GetBoolValue write SetBoolValue;
    property IntValue[const ACode: Integer]: Integer read GetIntValue write SetIntValue;
    property StrValue[const ACode: Integer]: string read GetStrValue write SetStrValue;

    property ChangeValues: Boolean read FChangeValues write SetChangeValues;
  end;

  TsgLock = class
  private
    FLocks: LongWord;
    function GetLock(const AIndex: Integer): Boolean;
    function GetLockAll: Boolean;
    procedure SetLock(const AIndex: Integer; const AVal: Boolean);
    procedure SetLockAll(const AVal: Boolean);
  public
    property LockAll: Boolean read GetLockAll write SetLockAll;
    property Locks[const AIndex: Integer]: Boolean read GetLock write SetLock; default;
  end;

  { TsgProxyBase }

  TsgProxyBase = class
  private
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    function GetSize: TPoint; virtual;
  public
    procedure DoEntityProcBegin(const Sender: TObject); virtual; abstract;
    procedure DoEntityProcEnd(const Sender: TObject); virtual; abstract;
    procedure DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
      const Arc: PsgArc); virtual; abstract;
    procedure DoFont(AFont: TFont); virtual; abstract;
    procedure DoImage(const R: TRect; AImage: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TBitmap{$ENDIF}); virtual; abstract;
    procedure DoPenGeometric(AStyle: DWORD; const APenColor, APenWidth: Integer); virtual; abstract;
    procedure DoPenMiterLimit(ANewLimit: Single); virtual; abstract;
    procedure DoPixel(APoint: TPoint; AColor: TColor); virtual; abstract;
    procedure DoPixelList(const APoints: TsgBaseList; AColor: TColor);
    procedure DoPolygonList(const APoints: TsgBaseList);
    procedure DoPolylineList(const APoints: TsgBaseList);
    procedure DoPolyPolygonList(const APoints: TsgIntegerList; const ACounts: TsgIntegerList);
    procedure DoPolyPolylineList(const APoints: TsgIntegerList; const ACounts: TsgIntegerList);
    procedure DoPolygon(Points: PPoint; Count: Integer); virtual; abstract;
    procedure DoPolyline(Points: PPoint; Count: Integer); virtual; abstract;
    procedure DoPolyPolygon(const Points; Counts: PInteger; Count: Integer); virtual; abstract;
    procedure DoPolyPolyline(const Points; Counts: PInteger; Count: Integer); virtual; abstract;
    procedure DoPolyPoly(const Points: array of TPointF; const Counts: array of Integer;
      AFillMode: Integer = 2; AUseFont: Boolean = False); virtual;
    function DoRestoreDC(const AIndex: Integer = 0): Boolean; virtual; abstract;
    function DoSaveDC: Integer; virtual; abstract;
    procedure DoTextOut(X, Y: Integer; const AText: string;
      const ACount: Longint = 0);
    procedure DoTextOutAW(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam;
      ACount: Longint = 0); virtual; abstract;
    procedure DoExportBlockAW(const blockString: string); virtual; abstract; // 최승선 수정   DoExportBlockAW 추가
    procedure DoTextOutW(X, Y: Integer; const AText: WideString;
      const ACount: Longint = 0);
    function DoSetPolyFillMode(PolyFillMode: Integer): Integer; virtual;
    function BeginScene: Boolean; virtual;
    procedure EndScene; virtual;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Size: TPoint read GetSize;
  end;

  TsgGraph = class
  private
    FVertexes: TList;
    FEdges: TList;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TsgEdgeArc = class;
  TsgEdgeClass = class of TsgEdge;
  TsgEdgeEntity = class;
  TsgEdgeLine = class;
  TsgVertex = class;
  TsgVertexClass = class of TsgVertex;
  TsgVertexPoint = class;

  TsgEdgeType = (eetLine, eetArc);

  TsgContourGraph = class(TsgGraph)
  private
    FEpsilon: Double;
    FUserPoint: TFPoint;
    function GetEdge(AIndex: Integer): TsgEdgeEntity;
    function GetVertexes(AIndex: Integer): TsgVertexPoint;
    procedure InitVertex(const AEdge: TsgEdgeEntity; const AVertexNum: Integer);
    function HookPoint(const AEdge: TsgEdgeEntity;
      const AVertexNum: Integer): Boolean;
  protected
    procedure AddEdgeList(const AEdges: TList; const APos: Integer);
    procedure CreateEdgesByType(const AEdgeType: TsgEdgeType;
      const ASource, AEdges: TList);
    procedure CreateEdgesFromLines(const ALines, AEdges: TList);
    procedure CreateEdgesFromArcs(const AArcs, AEdges: TList);
    procedure CreateEdgesFromCurves(const ACurves: TList; const AEdges: TsgObjectList);
    procedure DeleteEdge(const AEdge: TsgEdgeEntity);
    procedure HookEdge(const AEdge: TsgEdgeEntity);
    procedure RemoveMultiplicityEdges;
    function CrossEdges(const AEdge1, AEdge2: TsgEdgeEntity;
      const AList1, AList2: TList): Boolean;
    function CreateEdgeLine(const ALine: TsgLine): TsgEdgeLine;
    function CreateEdgeArc(const AArc: TsgArcR): TsgEdgeArc;
    property Vertexes[AIndex: Integer]: TsgVertexPoint read GetVertexes;
    property Edges[AIndex: Integer]: TsgEdgeEntity read GetEdge;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessContour(const AList: TsgObjectList): Boolean;
    procedure InsertEdge(const AEdge: TsgEdgeEntity);
    procedure ImportCurverList(const ACurves: TList);
    property Epsilon: Double read FEpsilon write FEpsilon;
    property UserPoint: TFPoint read FUserPoint write FUserPoint;
  end;

  TsgEdge = class(TObject)
  private
    FVertexes: TList;
  protected
    function GetVertexes(AIndex: Integer): TsgVertex;
    procedure SetVertexes(AIndex: Integer; const Value: TsgVertex);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function HasVertex(AVertex: TsgVertex): Integer;
    function IsLoop: Boolean;
    function IsIdentical(AEdge: TsgEdge): Boolean; virtual;
    property Vertexes[AIndex: Integer]: TsgVertex read GetVertexes write SetVertexes;
  end;

  TsgEdgeEntity = class(TsgEdge)
  private
    FVertex1Num: Integer;
    FVertex2Num: Integer;
    FBypassAngle: TsgFloat;
    FBypassCount: Integer;
    FBypassNext: TsgEdgeEntity;
    FBypassNormal: TsgLine;
    FBypassSign: TValueSign;
    function GetVertex1: TsgVertexPoint;
    function GetVertex2: TsgVertexPoint;
  protected
    procedure GetAngleLineAndNormal(AVertex: TsgVertexPoint;
      const ARadius: Double; var ALine, ANormal: TsgLine); virtual; abstract;
    function CalcBypassNormal(ALine: TsgLine; APt: TFPoint; ABypassSign: TsgFloat): TsgLine;
    function CalcBypassAngle(ALine1, ALine2, ANormal1, ANormal2: TsgLine): TsgFloat;
    function GetEdgeAngle(APrevEdge: TsgEdgeEntity; ARadius: Double): TsgFloat; virtual;
    procedure GetBypassPriorityList(AList: TList);
    function GetSimpleEdge: TsgLine;
    function GetVetrexPoints(AIndex: Integer): TFPoint; virtual; abstract;
    function IsCrossLine(const ALine: TsgLine; ACrossPts: TList): Boolean; virtual; abstract;
    function IsCrossArc(const AArc: TsgArcR; ACrossPts: TList): Boolean; virtual; abstract;
    function SplitEdge(const ACutPts: TList; const AResult: TList; AEpsilon: Double): Boolean; virtual; abstract;
    function VectorProduct2d(AL1, AL2: TsgLine): Extended;
    property Vertex1: TsgVertexPoint read GetVertex1;
    property Vertex2: TsgVertexPoint read GetVertex2;
    property BypassSign: TValueSign read FBypassSign write FBypassSign;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearBypass;
    procedure CalcBypass(APrevBypassSign: TValueSign; APrevVertex: TsgVertexPoint);
    function IsIdentical(AEdge: TsgEdge): Boolean; override;
    function BypassJoin(AEdges: TsgObjectList): Boolean;
    function BypassPossible: Boolean;
    function EdgeType: TsgEdgeType; virtual; abstract;
    property BypassNormal: TsgLine read FBypassNormal write FBypassNormal;
    property BypassCount: Integer read FBypassCount;
    property BypassNext: TsgEdgeEntity read FBypassNext write FBypassNext;
    property VetrexPoints[AIndex: Integer]: TFPoint read GetVetrexPoints;
  end;

  TsgEdgeArc = class(TsgEdgeEntity)
  private
    FArc: TsgArcR;
  protected
    procedure GetAngleLineAndNormal(AVertex: TsgVertexPoint;
      const ARadius: Double; var ALine, ANormal: TsgLine); override;
    function GetVetrexPoints(AIndex: Integer): TFPoint; override;
    function IsCrossLine(const ALine: TsgLine; ACrossPts: TList): Boolean; override;
    function IsCrossArc(const AArc: TsgArcR; ACrossPts: TList): Boolean; override;
    function SplitEdge(const ACutPts: TList; const AResult: TList; AEpsilon: Double): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsIdentical(AEdge: TsgEdge): Boolean; override;
    function EdgeType: TsgEdgeType; override;
    property Arc: TsgArcR read FArc write FArc;
  end;

  TsgEdgeLine = class(TsgEdgeEntity)
  private
    FLine: TsgLine;
  protected
    procedure GetAngleLineAndNormal(AVertex: TsgVertexPoint;
      const ARadius: Double; var ALine, ANormal: TsgLine); override;
    function GetVetrexPoints(AIndex: Integer): TFPoint; override;
    function FastLineCrossingCheck(ALine: TsgLine): Boolean;
    function IsCrossLine(const ALine: TsgLine; ACrossPts: TList): Boolean; override;
    function IsCrossArc(const AArc: TsgArcR; ACrossPts: TList): Boolean; override;
    function SplitEdge(const ACutPts: TList; const AResult: TList; AEpsilon: Double): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsIdentical(AEdge: TsgEdge): Boolean; override;
    function EdgeType: TsgEdgeType; override;
    property Line: TsgLine read FLine write FLine;
  end;

  TsgGeneratorShapeEdge = class
  private
    FAddPoint: TsgProcAddPointInList;
    FCreatePoint: TsgCreatePPoint;
    FDistanceToleranceManhattan: Double;
    FDistanceToleranceSquare: Double;
    FIsImportMode: Boolean;
    FList: Pointer;//TList;
    FListType: TsgListType;
    {$IFDEF SG_FIREMONKEY}
    FLock: SyncObjs.TCriticalSection;
    {$ELSE}
    FLock: TRTLCriticalSection;
    {$ENDIF}
    FNumberCirclePart: Cardinal;
    FNumberSplinePart: Cardinal;
    FQualityApproximation: Cardinal;
    FRecursionLimit: Byte;
    procedure CalculateBSplineBasis(const AKnots: TsgDoubleList;
      N, ALeftKnotIndex: Integer; const UU: Double;
      var ABasisValues: array of Double);
    function CalculateBSplinePoints(const AControls: IsgArrayFPoint;
      const AKnots, AWeights: TsgDoubleList; const ADegree: Integer;
      const Param: Double; var AValue: TFPoint): Boolean;
    function Normalize(const AOffsetIndex, AKnotIndex: Integer;
      const AParam: Double): Double;
    procedure SetIsImportMode(AValue: Boolean);
    procedure SetNumberCirclePart(const AValue: Cardinal);
    procedure SetNumberSplinePart(const AValue: Cardinal);
    procedure SetQualityApproximation(const AValue: Cardinal);
    procedure InitializeCriticalSectionInt;
    procedure DeleteCriticalSectionInt;
  protected
    procedure AddPoint(const APoint: TFPoint);
    function CheckParams: Boolean;
    procedure GenerateBSpline(AControls: IsgArrayFPoint;
      const AKnots, AWeights: TsgDoubleList; const ADegree: Integer);
    procedure GenerateEllipticCurve(const AX, AY, AZ, ARadiusX, ARadiusY,
      AStart, AEnd, AAxisSin, AAxisCos: Extended; AReverse: Boolean = False);
    procedure GenerateQuadraticSpline(const APoint1, APoint2, APoint3: TFPoint;
      const ALevel: Byte);
    function GetNumberPart(const AValue: Cardinal): Byte;
    procedure SetQualityParams;
    class procedure SetStartEndAngles(const AStartAngle, AEndAngle: Double;
      var AStart, AEnd: Extended);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateBulgesArc(const P1,P2: TFPoint; const Bulge: Double): TsgArcR;
    procedure CreateCircularArc(const ACenter: TFPoint; const ARadius,
      AStartAngle, AEndAngle: Double);
    procedure CreateBSpline(AControls: IsgArrayFPoint;
      const AKnots, AWeights: TsgDoubleList; const ADegree: Integer);
    procedure CreateCubicSpline(const APoint1, APoint2, APoint3,
      APoint4: TFPoint);
    procedure CreateEllipticArc(const ACenter, ARatPoint: TFPoint;
      const ARation, AStartAngle, AEndAngle: Double);
    procedure CreateEllipticArcByRadiuses(const ACenter: TFPoint;
      const ARadiusX, ARadiusY, AStartAngle, AEndAngle,
      AAxisSin, AAxisCos: Double; AReverse: Boolean = False);
    procedure CreateEllipticArcByPoints(const ACenter, ARatPoint1, ARatPoint2: TFPoint;
      const AStartPoint, AEndPoint: PFPoint);
    procedure CreateSplineByKnots(AControls: IsgArrayFPoint;
      const AKnots: TsgDoubleList; const AIncT: Double);
    procedure CreateQuadraticSpline(const APoint1, APoint2, APoint3: TFPoint);
    procedure SetListAndProc(const AList: TObject;
      const AProc: TsgProcAddPointInList;
      const ACreatePoint: TsgCreatePPoint = nil);
    procedure SetBaseList(const AList: TsgBaseList);
    function Lock: Integer;
    procedure Unlock;
    class function IsBSpline(const ADegree: Integer): Boolean;
    class function GetNumberArcPart(const ANumberOfCirclePart: Integer;
      const AStartAngle,AEndAngle: Double; const AOldMode: Boolean;
      var ADelta: Double): Integer;
    property IsImportMode: Boolean read FIsImportMode write SetIsImportMode;
    property NumberCirclePart: Cardinal read FNumberCirclePart write SetNumberCirclePart;
    property NumberSplinePart: Cardinal read FNumberSplinePart write SetNumberSplinePart;
    property QualityApproximation: Cardinal read FQualityApproximation write SetQualityApproximation;
  end;

  PdwgMapElementList = ^TdwgMapElementList;
  PPdwgMapElementList = ^PdwgMapElementList;
  TdwgMapElementList = record
    Handle: UInt64;
    Obj: TObject;
    Location: Integer;
  end;

  TsgMapList = class(TsgPointerList)
  protected
    function ComparePointers(const A, B: Pointer): Integer; override;
  end;

  TsgObjectsMap = class
  private
    FMap: TsgMapList;
    FObjectsLoaded: Integer;
    function GetCount: Integer;
    function GetMapElement(AIndex: Integer): TdwgMapElementList;
    function GetObjectsLoaded: Integer;
    function GetObjectLoaded(const Index: Integer): Boolean;
    function GetObjectLoadedCount(const Index: Integer): Integer;
    procedure SetObject(AIndex: Integer; const Value: TObject);
    procedure SetObjectLoaded(const Index: Integer; const Value: Boolean);
    function GetHasName(Index: Integer): Boolean;
    function GetName(Index: Integer): string;
    procedure SetName(Index: Integer; const Value: string);
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  protected
    function GetPMapElement(Index: Integer): PdwgMapElementList;
    property Sorted: Boolean read GetSorted write SetSorted; // changing to True do not call Sort method!
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDwgElement(const AHandle: UInt64; const ALocation: Integer;
      const AObj: TObject = nil);
    procedure Clear;
    function IndexOfHandle(const AHandle: UInt64): Integer;
    procedure Sort;
    property Count: Integer read GetCount;
    property ObjectsLoaded: Integer read GetObjectsLoaded;
    property ObjectLoaded[const Index: Integer]: Boolean read GetObjectLoaded
      write SetObjectLoaded;
    property ObjectLoadedCount[const Index: Integer]: Integer read GetObjectLoadedCount;
    property MapElement[AIndex: Integer]: TdwgMapElementList read GetMapElement;
    property Objects[AIndex: Integer]: TObject write SetObject;
    property HasName[Index: Integer]: Boolean read GetHasName;
    property Name[Index: Integer]: string read GetName write SetName;
  end;

{$IFNDEF SGDEL_2005}
  TStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    function GetCurrent: string;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;
{$ENDIF}

{$IFNDEF SGDEL_6}
  TsgStringListV6 = class(TStringList)
  private
    FDefined: TStringsDefined;
    FDelimiter: Char;
    FQuoteChar: Char;
    FCaseSensitive: Boolean;
    function GetDelimiter: Char;
    function GetQuoteChar: Char;
    procedure SetDelimiter(const Value: Char);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetQuoteChar(const Value: Char);
  protected
    function CompareStrings(const S1, S2: string): Integer; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
  end;
{$ELSE}
  TsgStringListV6 = class(TStringList)
  end;
{$ENDIF}

{$IFNDEF SGDEL_7}
  TsgStringListV7 = class(TsgStringListV6)
  private
    FDefined: TStringsDefined;
    FNameValueSeparator: Char;
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): string;
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
  end;
{$ELSE}
  TsgStringListV7 = class(TsgStringListV6)
  end;
{$ENDIF}

{$IFNDEF SGDEL_2005}
  TsgStringListV2005 = class(TsgStringListV7)
  private
    FDefined: TStringsDefined;
    FLineBreak: string;
    function GetLineBreak: string;
    procedure SetLineBreak(const Value: string);
  protected
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
  public
    function GetEnumerator: TStringsEnumerator;
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property Text: string read GetTextStr write SetTextStr;
  end;
{$ELSE}
  TsgStringListV2005 = class(TsgStringListV7)
  end;
{$ENDIF}

  TsgStackRegions = class(TList)
  public
    procedure Clear;{$IFNDEF SG_FIREMONKEY} override;{$ENDIF}
    function Pop: HRGN;
    procedure Push(const RGN: HRGN);
  end;

  TsgListWithType = class(TList)
    TypeList: Integer;
  end;

  TsgStringList = class(TsgStringListV2005);

  TsgStringListNamed = class(TsgStringList)
  protected
    function FindName(const S: string; var Index: Integer): Boolean;
  public
    constructor Create;
    function AddNameValue(const AName, AValue: string; const AType: Cardinal): Integer;
    function IndexOfName(const Name: string): Integer; {$IFDEF SGDEL_6} override; {$ELSE} reintroduce; {$ENDIF}
  end;

{This classes for old versions of an source code}
(*
  TsgStringListNV = TsgStringListNamed;
*)

  TsgVertex = class(TObject)
  private
    FEdges: TList;
  protected
    function GetEdge(AIndex: Integer): TsgEdge;
    function GetEdgesCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddEdge(AEdge: TsgEdge; APointIndex: Integer); virtual;
    procedure DeleteEdge(AEdge: TsgEdge); virtual;
    function IsEqual(AVertex: TsgVertex): Boolean; virtual; {$IFNDEF SGFPC} abstract; {$ENDIF}
    property Edges[AIndex: Integer]: TsgEdge read GetEdge;
    property EdgesCount: Integer read GetEdgesCount;
  end;

  TsgVertexPoint = class(TsgVertex)
  private
    FPoint: TFPoint;
  public
    function SelectEdgeBySecondVertex(ASecondVertex: TsgVertexPoint): TsgEdgeEntity;
    function IsEqual(AVertex: TsgVertex): Boolean; override;
    property Point: TFPoint read FPoint write FPoint;
  end;

  TsgPolyPolygonItem = class;

  TsgPolyPolygon = class
  private
    FObjectItems: TsgObjectList;
  protected
    function AddPoints(const APoints: IsgArrayFPoint): TsgPolyPolygonItem;
    procedure FindHoles;
  public
    constructor Create(const APolyPolylines: TsgObjectList = nil);
    destructor Destroy; override;
    property ObjectItems: TsgObjectList read FObjectItems;
  end;

  TsgPolyPolygonItem = class
  private
    FArea: Double;
    FBox: TFRect;
    FPoints: IsgArrayFPoint;
    FChilds: TsgPolyPolygon;
  protected
    class procedure FindHoles(const APoly: TsgObjectList);
  public
    constructor Create(const APoints: IsgArrayFPoint);
    destructor Destroy; override;
    function ToStr: string;
    property Area: Double read FArea;
    property Box: TFRect read FBox;
    property Points: IsgArrayFPoint read FPoints;
    property Childs: TsgPolyPolygon read FChilds;
  end;

  TsgRegistryAccess = (raNone, rax86, rax64);

  TsgProgress = class(TInterfacedObject, IsgProgress)
  private
    FOnProgress: TProgressEvent;
    FProgressBreak: Boolean;
  protected
    function GetBreak: Boolean;
    procedure SetBreak(const AValue: Boolean);
    procedure DoProgress(Sender: TObject; Stage: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string{$IFDEF SGFPC}; var Continue: Boolean{$ENDIF});
  public
    constructor Create(const AOnProgress: TProgressEvent);
    destructor Destroy; override;
  end;

  TsgTransformationArray = array of Double;

  TsgCustomPolyItemsCollection = class(TObject)
  private
    FCounts: TsgIntegerList;
    FOwnsPoly: Boolean;
  protected
    FIsIdentityTransformation: Boolean;
    FTransformation: TsgTransformationArray;
    FPoly: TsgBaseList;
    function DoAddPt(const P): Integer; virtual;
    function DoAddPtDirrect(const P): Integer; virtual;
    function GetAsPolyPolyline: Boolean; virtual;
  public
    constructor Create(APoly: TsgBaseList; AOwnsPoly: Boolean); virtual;
    destructor Destroy; override;
    function NewCollection: TAddPt; virtual;
    procedure ChangeItemsCount(ADelta: Integer); virtual;
    procedure Normalize; virtual;
    function ReleaseItems: Pointer;
    function UpdateTransformation(const AValue; {$IFDEF HAS_UNMANAGED_TYPEINFO}AType: Pointer{$ELSE}ASize: Integer{$ENDIF}): Boolean; virtual;
    property Transformation: TsgTransformationArray read FTransformation;
    property IsIdentityTransformation: Boolean read FIsIdentityTransformation;
    property Counts: TsgIntegerList read FCounts;
    property OwnsPoly: Boolean read FOwnsPoly write FOwnsPoly;
    property Poly: TsgBaseList read FPoly;
  end;

  TsgAliasList = class
  private
    FAliasies: TStringList;
    FSourceName: TStringList;
  public
    procedure AddAlias(const ASource, AAlias: string; AType: Integer);
    procedure Clear;
    function GetAliasByOriginalName(const AOriginalName: string; AType: Integer): string;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TsgMap }

  TsgMapDescription = packed record
    case Integer of
      0: (ShiftA, ShiftR, ShiftG, ShiftB, A, R, G, B: Byte);
      1: (IsComponent: Integer; BitsPerPixel: Integer);
  end;

  TsgMap = class(TPersistent)
  private
    FOwner: TPersistent;
    FDescription: TsgMapDescription;
    FWidth, FHeight: Integer;
    FBytesPerScanline: Integer;
    function GetScanline(Y: Integer): Pointer;{$IFDEF USE_INLINE} inline;{$ENDIF}
    function BSwap(var A, B: Byte): Byte;
    function GetPixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;{$IFDEF USE_INLINE} inline;{$ENDIF}
  protected
    FData: Pointer;
    Pal: array of Integer;
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AllocMap; virtual;
    procedure ReallocMap; virtual;
    function ReleaseData: Pointer;
    function GetPalIndex(AColor: TColor): Integer; virtual;
    function GetPixel(X, Y: Integer): TColor; virtual;
    procedure SetPixel(X, Y: Integer; AValue: TColor); virtual;
{$IFNDEF SG_FIREMONKEY}
    function CreateHPalette: HPALETTE;
{$ENDIF}
    procedure CopyTo(AMap: TsgMap);
    procedure CopyToBitmap(ABitmap: TBitmap);
    procedure CopyFromBitmap(ABitmap: TBitmap);
{$IFDEF SGFPC}
    procedure InitPal(APalette: TFPPalette); overload;
{$ENDIF}
    procedure InitPal(Colors: Pointer; Count: Integer; AElementTypeInfo: Pointer = nil);{$IFNDEF SG_FIREMONKEY} overload;{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
    procedure InitPal(HPal: HPALETTE); overload;
{$ENDIF}
    procedure UpdatePal;
    class function InitDescription(APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
      var ADescription: TsgMapDescription): Boolean;
  public
    constructor Create(AWidth, AHeight: Integer; APixelFormat:
      {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat = pf32bit; AAlignment: Integer = 32); overload;
    constructor Create(AWidth, AHeight: Integer; const ADescription: TsgMapDescription; AAlignment: Integer = 32); overload;
    destructor Destroy; override;
    class function Wrap(AWidth, AHeight: Integer;
      const ADescription: TsgMapDescription;
      Data: Pointer; AColors: Pointer; ACount: Integer;
      AAlignment: Integer = 32): TsgMap; overload;
    class function Wrap(AWidth, AHeight: Integer;
      APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
      Data: Pointer; AColors: Pointer; ACount: Integer;
      AAlignment: Integer = 32): TsgMap; overload;
    class function Wrap(ABitmap: TBitmap; AReadOnly: Boolean = False): TsgMap; overload;
    class function Unwrap(var AMap: TsgMap): Pointer;
    class function GetNearestPalIndex(AColor: TColor; const APal: array of Integer; var Index: Integer): Boolean;
    procedure Assign(Source: TPersistent); override;
    property Description: TsgMapDescription read FDescription;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property PalIndex[AColor: TColor]: Integer read GetPalIndex;
    property Scanline[Y: Integer]: Pointer read GetScanline;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BytesPerScanline: Integer read FBytesPerScanline;
    property PixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat read GetPixelFormat;
    property Data: Pointer read FData;
  end;

{$IFDEF SG_HAS_WINAPI_INTERFACE}
  TsgDIBBitmap32 = class
  private
    FMap: TsgMap;
    FDIBHandle: THandle;
    FBufferHandle: THandle;
    FStockHandle: THandle;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: Integer);
    property Map: TsgMap read FMap;
    property DIBHandle: THandle read FDIBHandle;
    property BufferHandle: THandle read FBufferHandle;
  end;
{$ENDIF}
  TsgColorToLineWeightMap = class
  private
    function GetCount: Integer;
    function GetColorKey(const AStrKeyValue: string;
      var AKey: TsgColorCAD; var AWeight: Double): Boolean;
  public
    Strings: TStringList;
    Keys: TsgColorCADList;
    Weights: TsgDoubleList;
    Colors: TsgIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AMap: TsgColorToLineWeightMap);
    procedure Update(const AClear: Integer);
    procedure MakeStrings;
    property Count: Integer read GetCount;
    procedure CorrectStrings;
    procedure LoadFromStrings(AColors: TStrings);
    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
  end;

  TsgConverterObj = class
  private
    FDestination: TObject;
    FSource: TObject;
  protected
    function Convert: Integer; virtual;
  public
    constructor Create(const ADest: TObject);
    function Execute(const ASource: TObject): Integer;
    property Destination: TObject read FDestination;
    property Source: TObject read FSource;
  end;

{$IFDEF SG_USE_VIEW_RECORD}
  TsgViewRec = packed record
    Up, Right: array[0 .. 2] of Single;
    Scale: Double;
    Offset: array[0 .. 2] of TsgFloat;
  end;
{$ENDIF}

  TsgView = class(TPersistent)
  private
    FPos: Integer;
    FCount: Integer;
    FStack: array of {$IFDEF SG_USE_VIEW_RECORD}TsgViewRec{$ELSE}TFMatrix{$ENDIF};
    procedure SetCapacity(const Value: Integer);
    function GetCapacity: Integer;
{$IFDEF SG_USE_VIEW_RECORD}
    class function ViewRecToMatrix(const AViewRec: TsgViewRec): TFMatrix;
    class function MatrixToViewRec(const AMatrix: TFMatrix): TsgViewRec;
{$ENDIF}
  public
    constructor Create(const ACapacity: Integer); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function HasView(AView: PFMatrix = nil): Boolean;
    function PopView(AView: PFMatrix = nil): Boolean;
    procedure PushView(const AView: TFMatrix);
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TsgMeasureUnitsProps = class
  private
    FDefaults: TsgUnitsProps;

    FScaleFactor: Double;
    FDisplayedUnits: TsgInsUnits;
    FInitialUnits: TsgInsUnits;
    FPrecisionFactor: Integer;

    FIsUserSet: Boolean;
    FFlags: Integer;
    procedure SetIsUserSet(const Value: Boolean);
    procedure SetPrecisionFactor(const Value: Integer);
    procedure SetDisplayedUnits(const Value: TsgInsUnits);
    procedure SetInitialUnits(const Value: TsgInsUnits);
    procedure SetScaleFactor(const Value: Double);
    procedure SetFlags(const Value: Integer);
  protected
    procedure ApplyParamsDefault;
  public
    constructor Create; virtual;
    constructor CreateWithParams(ADisplayedUnits, AInitialUnits: TsgInsUnits;
      AScaleFactor: Double; APrecision: Integer); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TsgMeasureUnitsProps);
    function IsEqual(ACompareObj: TsgMeasureUnitsProps): Boolean;
    procedure SetProps(const ADisplayedUnits, AInitialUnits: TsgInsUnits;
      const AScaleFactor: Double; const APrecisionFactor: Integer);
    property IsUserSet: Boolean read FIsUserSet write SetIsUserSet;
    property PrecisionFactor: Integer read FPrecisionFactor write SetPrecisionFactor;
    property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
    property DisplayedUnits: TsgInsUnits read FDisplayedUnits write SetDisplayedUnits;
    property InitialUnits: TsgInsUnits read FInitialUnits write SetInitialUnits;
    property Flags: Integer read FFlags write SetFlags;
    property Defaults: TsgUnitsProps read FDefaults;
  end;

{operations on matrices}

function AffineTransformPoint(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
function AffineTransformPoint(const APoint: TFPoint; const AMatrix: TsgAffineMatrix): TFPoint; {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF} overload;
function CADUnProject(const X, Y, Z: Double; const ADrawMatrix: TFMatrix): TFPoint; overload;
function CADUnProject(const AClientPos: TPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
function CADUnProject(const X, Y: Integer; const ADrawMatrix: TFMatrix): TFPoint; overload;
function CADUnProject(const AClientPos: TF2DPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
function CADUnProject(const AClientPos: TFPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
function InvertMatrix(var AMatrix: TFMatrix;
  const AResolution: Double = fMaxResolution): Extended; overload;
function InvertMatrix(var AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): Extended; overload;
function FMat2DByParams(const A, B, C, D, E, F: Double): TFMatrix;
function FMatByAngle(const Angle: Double): TFMatrix;
function FMatByMirror(const AP1, AP2: TFPoint): TFMatrix;
function FMatByScale(const Scale: Double): TFMatrix;
function FMatByScales(const SX, SY, SZ: Double): TFMatrix; overload;
function FMatByScales(const AScale: TFPoint): TFMatrix; overload;
procedure FMatByScales(var AMatrix: TsgAffineMatrix; const SX, SY, SZ: Double);{$IFDEF USE_INLINE} inline;{$ENDIF} overload;
procedure FMatByScales(var AMatrix: TsgAffineMatrix; const AScale: TFPoint); overload;
function FMatBySkew(const ASkewX, ASkewY: Double): TFMatrix;
function FMatByTranslate(const AX, AY, AZ: Double): TFMatrix; overload;
function FMatByTranslate(const AOffset: TFPoint): TFMatrix; overload;
function FMat2DByImage(const APoint, UVector, VVector: TFPoint;
  const AWidth, AHeight: Double): TFMatrix;
function FMatByImage(const APoint, UVector, VVector: TFPoint;
  const AWidth, AHeight: Double): TFMatrix;
function FMat2DNormalize(const AMatrix: TFMatrix; const AScale: PDouble = nil):  TFMatrix;
function FMatExtractParams2D(const M: TFMatrix; var APoint,
  AScale: TFPoint; var AAngle: Double): Boolean;
function FMatInverse(const AMatrix: TFMatrix;
  const AResolution: Double = fMaxResolution): TFMatrix; overload;
function FMatInverse(const AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): TsgAffineMatrix; overload;
function FMatInverseGausse_Jordan(const AMatrix: TFMatrix;
  const AResolution: Double = fMaxResolution): TFMatrix;
function FMatInverse3x3(const AMatrixIn: TFMatrix;
  var AMatrixOut: TFMatrix): Boolean;
procedure FMatOffset(var AMatrix: TFMatrix; const APoint: TFPoint);
function FMatScale(const A: TFMatrix; const AScale: TFPoint): TFMatrix; overload;
function FMatScale(const A: TFMatrix; const AScale: Double): TFMatrix; overload;
function FMatScale(const A: TsgAffineMatrix; const AScale: TFPoint): TsgAffineMatrix; overload;
function FMatScale(const A: TsgAffineMatrix; const AScale: Double): TsgAffineMatrix; overload;
function FMatTranspose(const AMatrix: TFMatrix): TFMatrix; overload;
function FMatTranspose(const AMatrix: TsgAffineMatrix): TsgAffineMatrix; overload;
function FMatTranslate(const AMatrix: TFMatrix; const AX, AY, AZ: Double): TFMatrix; overload;
function FMatTranslate(const AMatrix: TFMatrix; const ADelta: TFPoint): TFMatrix; overload;
function FMatXMat(const A, B: TFMatrix): TFMatrix; overload;
function FMatXMat(const A, B: TsgAffineMatrix): TsgAffineMatrix; overload;
function FMatXMat2D(const A, B: TFMatrix): TFMatrix;
function FMatXMatBase(const A, B: TFMatrix; const P: TFPoint): TFMatrix;
function FPointXMat(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint; {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
function FPointXMat2D(const AP: TFPoint; const M: TFMatrix; const Affine: Boolean): TFPoint; overload;
function FPointXMat2D(const AP: TFPoint; const M: TFMatrix): TF2DPoint; overload;
function FPointXMat2D(const AP: TF2DPoint; const M: TFMatrix): TF2DPoint; overload;
function FPointXMat2DLongint(const APoint: TFPoint; const AMatrix: TFMatrix): TPoint; {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
function FPointXMatInverse(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint;
function FRectXMat(ARect: TFRect; AMatrix: TFMatrix): TFRect;
function BoxXMat2D(const ARect: TFRect; const AMatrix: TFMatrix): TFRect;
function BuildRotMatrix(const AAxis: TsgAxes; ARadAngle: Double;
  const AResolution: Double = fExtendedResolution): TFMatrix; overload;
procedure BuildRotMatrix(const AAxis: TsgAxes; ARadAngle: Double;
  var AAffineMatrix: TsgAffineMatrix;
  const AResolution: Double = fExtendedResolution); overload;
procedure BuildRotMatrix(const AAxisVector: TFPoint; AAngle: Double;
  var AResult: TFMatrix); overload;
function StdMat(const AScale, AOffset: TFPoint): TFMatrix;
procedure TransformPointTo3f(const APoint: TFPoint; const AMatrix: TFMatrix; AAffineVector: PSingle); {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
function AffineMatrixAdjoint(const AAfineMatrix: TsgAffineMatrix): TsgAffineMatrix;
function AffineMatrixDeterminant(const AAfineMatrix: TsgAffineMatrix): Extended;


{geometric transformations and calculations, and associated calculations}

function AbsFPoint(const AP: TFPoint): TFPoint;
function AbsF2DPoint(const AP: TF2DPoint): TF2DPoint;
function AbsFPoint2D(const AP: TFPoint): TFPoint;
function AddF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
function AddFPoint(const AP1, AP2: TFPoint): TFPoint;
function AddFPoint2D(const AP1, AP2: TFPoint): TFPoint;
function AddPoint(const AP1, AP2: TPoint): TPoint; overload;
function AddPoint(const AP1, AP2: TPointF): TPointF; overload;
procedure ArcExpandFRect2D(var R: TFRect; const Arc: TsgArcR; const UseAngles: Boolean);
function AreaOfTriangle(const AP1, AP2, AP3: TFPoint): Double;
function AreaOfTriangleEx(const L12, L23, L31: Extended): Double;
function ChangeLineLength(const ALine: TsgLine; const ANewLength: Double): TsgLine;
function CheckArcPnt(const AArc: TsgArcR; const AP: TFPoint): Boolean;
function CheckLinePnt(const AL: TsgLine; const AP: TFPoint): Boolean;
function CorrectFPointOnZero(const AP: TFPoint; const AIs3D: Boolean = True): TFPoint;
function DenomFPoint2D(const AP1, AP2: TFPoint): Extended;
function DenomVectors(const AX1, AY1, AX2, AY2: Extended): Extended;{$IFDEF USE_INLINE}inline;{$ENDIF}
function DistanceF2DPoint(const AP1, AP2: TF2DPoint): Double;
function DistanceF2DPointSqr(const AP1, AP2: TF2DPoint): Extended;
function DistanceFPoint2D(const AP1, AP2: TFPoint): Double;
function DistanceFPoint2DSqr(const AP1, AP2: TFPoint): Extended;
function DistanceFPointSqr(const AP1, AP2: TFPoint): Extended;
function DistanceFPointToLine(const ALine: TsgLine; const APoint: TFPoint): Double;
function DistanceFPoints(APoints: IsgArrayFPoint): Double;
function DistanceFVector(const AV: TFPoint): Double;
function DistancePoint(const AP1, AP2: TPoint): Double;
function DistancePointSqr(const AP1, AP2: TPoint): Double;
function DistanceLine(const ALine: TsgLine): Double;
function DistanceLine2D(const ALine: TsgLine): Double;
function DistanceVector(const AX, AY, AZ: Extended): Extended;
function DistanceVector2D(const AX, AY: Extended): Extended;
function DistanceVectorSqr(const AX, AY, AZ: Extended): Extended;
function DivFPoint(const AP1, AP2: TFPoint): TFPoint;
function DSplitArcAndArc(const Arc1, Arc2: TsgArcR; AArcs1, AArcs2: TList): Byte;
function DSplitLineAndArc(const Line: TsgLine; const Arc: TsgArcR; ALines, AArcs: TList): Byte;
function DSplitLineAndLine(const Line1, Line2: TsgLine; var ALines1, ALines2: TList): Byte;
procedure ExpandF2DRect(var R: TF2DRect; const P: TF2DPoint);
procedure ExpandFRect(var R: TFRect; const P: TFPoint);
procedure ExpandFRect2D(var R: TFRect; const P: TFPoint); overload;
procedure ExpandFRect2D(var R: TFRect; const P: TF2DPoint); overload;
procedure ExpandRect(var R: TRect; const P: TPoint); overload;
procedure ExpandRect(var R: TRectF; const P: TPointF); overload;
procedure ExpandPts4(var R: TsgPoints4; const P: TFPoint);
function ExtrutionFPoint(const APoint, AExtrusion: TFPoint): TFPoint;
function PreExtrutionFPoint(const APoint, AExtrusion: TFPoint): TFPoint;
function ScaleRect(const ARect: TRect; const AValue: Double): TRect;
function ScaleFRect2D(const ARect: TFRect; const AValue: Double): TFRect;
function F2DLineTo3D(const AL: TF2DLine): TsgLine;
function F2DPointTo3D(const AP: TF2DPoint): TFPoint;
function F3DLineTo2D(const AL: TsgLine): TF2DLine;
function F3DPointTo2D(const AP: TFPoint): TF2DPoint;

procedure IncPoints4(const ADelta: TFPoint; var APoints: TsgPoints4);
procedure RotatePoints4(const AAngle: Double; var APoints: TsgPoints4);

function GetAngleByLen(const ARadius, ALen: Double; const AIsRadian: Boolean): Double;
function GetAngleByPoints(const ACenter, APoint: TFPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution;
  const AOrtoX: Integer = 1; const AOrtoY :Integer = 1): Double;
function GetAngleByF2DPoints(const ACenter, APoint: TF2DPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution): Double;
function GetAngleByPointsI(const ACenter, APoint: TPoint; const AIsRadian: Boolean): Double;
function GetAngleOfLines(const ALine1, ALine2: TsgLine; const Epsilon: Double = fDoubleResolution): Double;
function GetAngleOfLinesEx(const Base, P1, P2: Pointer;
  const AProc: TsgProcOfPointerGetPoint): Double; overload;
function GetAngleOfLinesEx(const Base, P1, P2: TFPoint): Double; overload;
function GetAngleOfVector(const V: TFPoint; const AIsRadian: Boolean): Double;
function GetAngleOfVectors(const V1, V2: TFPoint; const AIsRadian: Boolean;
  const A3D: Boolean = False): Double;
function GetAngleOfF2DVectors(const V1, V2: TF2DPoint; const AIsRadian: Boolean): Double;
function GetAngleOfVectorsDirect(const V1, V2: TFPoint; const AIsRadian: Boolean): Double;
function GetAngleParam(const Center, RadPt: TFPoint; const Ratio: Double;
  const Point: TFPoint; const AAbsPoint: Boolean = True): Double;

function GetAngleByCoords(const AAngle: Double;
  const AxisX, AxisY: Integer): Double;
//returns angles from rotation matrix
//Roll - X, Yaw - Y, Pitch - Z
//angles is returned in degrees
procedure GetAnglesFromRotMatrix(const Matrix: TFMatrix; var Roll, Yaw, Pitch: Double);
function GetArcROfBulge(const AP1, AP2: TFPoint; const ABulge: Double; const ACheckAngles: Boolean = True): TsgArcR;
function GetBoxOf2DLine(const ALine: TF2DLine): TF2DRect;
function GetBoxOfArcs2D(const Arcs: TsgArcsParams): TFRect;
function GetBoxOfCircle2D(const Arcs: TsgArcR): TFRect;
function GetBoxOfLine(const ALine: TsgLine): TFRect;
function GetBoxOfLine2D(const ALine: TsgLine): TFRect;
function GetBoxOfList(const AList: TList): TFRect;
function GetBoxOfObjectList(const AList: TsgObjectList): TFRect;
function GetBoxOfArray(AList: IsgArrayFPoint): TFRect; overload;
function GetBoxOfPointI(const AP1, AP2: TPoint): TRect;
function GetBoxOfPts2D(const APts: TsgPoints4): TFRect;
function GetBoxOfFPoints(APoints: IsgArrayFPoint): TFRect;
function GetBoxOfF2Rect(const ARect: TF2DRect): TRect;
function GetBoxOfArray(const APoints: array of TFPoint): TFRect; overload;
function GetBoxOfArray(const APoints; ACount: Integer): TFRect; overload;
function GetBulgeOfArcR(const AArc: TsgArcR; const AVetrexPoint: TFPoint): Double;
function GetBulgeWithCorrect(const AP1, AP2, APN: TFPoint;
  var ABulge: Double): Boolean;
function GetRealBox(const ASourceBox: TFRect; const AMatrix: TFMatrix): TFRect;
procedure IniBox(var ABox: TFRect; const APoints: IsgArrayFPoint;
  const ADefaultBox: PFRect);
function GetCenterOfBulge(const AP1, AP2: TFPoint; const ABulge: Double): TFPoint;
function GetCenterOfCircle(const AR: Double; const AP1, AP2, AP3: TFPoint): TFPoint;
function GetCenterOfRect(const ARect: TFRect): TFPoint;
function GetCenterPts(const APts: TsgPoints4): TsgLine;
function GetCenterPPts(const APts: TsgPFPoints4): TsgLine;
function GetCenterArcs(const AArc: TsgArcsParams): TsgArcR;
function GetArcParams(const AP1, AP2, AP3: TFPoint; var Center: TFPoint;
  var Radiaus, AngleS, AngleE: Double; const AAccuracy: Double = fDoubleResolution): Boolean;
function GetArcParamsAP(const AP1, AP2, AP3: TFPoint; var Arc: TsgArcR;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function GetCircleParams(const AP1, AP2, AP3: TFPoint; var Center: TFPoint; var Radiaus: Double;
  const AAccuracy: Double = fDoubleResolution;
  const AIsFixDiameter: Boolean = False): Boolean;
function GetCosAngleOfVectors(const V1, V2: TFPoint; const A3D: Boolean = False): Double;
function GetFPointSinCosByAngle(const AAngle: Double): TFPoint;
function GetFPointSinCosByFPoints(const AP1, AP2: TFPoint): TFPoint;
function GetEndF2DPoint(const Arc: TsgArcR): TF2DPoint;
function GetEllipseParams(const APCenter, ACDP1, ACDP2: TFPoint;
  var ARadPoint1, ARadPoint2: TFPoint): Boolean;
function GetIntresectingRectAndLine(const ARect: TFRect; const ALine: TsgLine; var AIntersectionMode: TIntersectionMode): TsgLine;
function GetLandScale(const AWidth, AHeight: Double): Boolean; overload;
function GetLandScale(const ASize: TF2DPoint): Boolean; overload;
function GetLengthArc(const ARadius, AngleStart, AngleEnd: Double): Double;
function GetLengthArcR(const Arc: TsgArcR): Double;
function GetLengthArcByPoints(const APCenter, APStart, APEnd: TFPoint): Double;
function GetLengthCircle(const ARadius: Double): Double;
function GetLengthLine(const ALine: TsgLine): Double;
function GetLengthVector(const V: TFPoint; AResolution: Double = fDoubleResolution): Double;
function GetLineOfFRect(const ARect: TFRect; const AIndex: Integer): TsgLine;
function GetLineParalleniar(const AP, P1, P2: TFpoint; const ALen: Double): TsgLine;
function GetLinePerpendicular2D(const AP, P1, P2: TFPoint; const ALen: Double): TsgLine;
function GetLineFromFPoints(APts: IsgArrayFPoint; AI, ACnt: Integer): TsgLine;
function GetMiddleFPointOfBulge(const AP1, AP2: TFPoint; const ABulge: Double): TFPoint;
function GetNormalPt(const APt1, APt2, ANormPt: TFPoint; ADelta: TsgFloat): TFPoint;
function GetParamByAngle(const Ratio, Angle: Double): Double;
function GetPointOfDoubleOffset(const APB1, APB2, APC: TFPoint; const AD1, AD2: Double; const APoint: PFPoint): Boolean;
function GetPointOfItem(const APointer: Pointer): TFPoint;
function GetPointOfPFPoint(const APointer: Pointer): TFPoint;
function GetPointOnArcAR(const AArc: TsgArcR; const AAngle: Double;
  const ApplyZ: Boolean = True): TFPoint;
function GetPointOnCircle(const ACenter: TFPoint; const ARadius, Angle: Double;
  const ApplyZ: Boolean = True): TFPoint;
function GetPointOnCircleI(const ACenter: TPoint; const ARadius, Angle: Double): TPoint;
function GetPointOnEllipse(const ACenter: TFPoint; const A, B, ASin, ACos, AAngle: Double): TFPoint;
function GetPointOnLine(const AP1, AP2: TFPoint; const ALength: Double;
  const AEpsilon: Extended = fDoubleResolution): TFPoint; overload;
function GetPointOnLine(const ALine: TsgLine; const ALength: Double;
  const AEpsilon: Extended = fDoubleResolution): TFPoint; overload;
function GetPointOnLineI(const AP1, AP2: TPoint; const ALength: Double): TPoint;
function GetPointOnLineF(const AP1, AP2: TPointF; const ALength: Double): TPointF;
function GetPointOnLineNormal(const APt1, APt2, ANormPt: TFPoint;
  const ADelta: Extended): TFPoint;
function GetPositionOnEntities(const APoint: TFPoint; const ALine: PsgLine;
  const AArc: PsgArcR): TsPositionOnEntities;
function GetRadiusOfCircle(const AP1, AP2, AP3: TFPoint): Double;
procedure GetRectangle(const ABox: TFRect; var APoints: TsgPoints5);
procedure GetPts4ByBox(const ABox: TFRect; var AResult: TsgPoints4);
procedure GetPts4ByPointAndSize(const APoint: TFPoint;
  const Angle, AWidth, AHeight: Double; var AResult: TsgPoints4);
function GetResXFunction(const X1, Y1, X2, Y2, Y: Double): Double;
function GetResYFunction(const X1, Y1, X2, Y2, X: Double): Double;
function GetRezOfQuadraticEquation(const A, B, C: Double; var Val1, Val2: Double): Integer;
function GetScaledRect(const APictureRect: TRect; const AExtents: TFRect;
  const ABox: TFRect): TRect;
function GetStartF2DPoint(const Arc: TsgArcR): TF2DPoint;
function GetSizeFRect2D(const ARect: TFRect): TF2DPoint;
function GetSizeFRect(const ARect: TFRect): TFPoint;
function GetUnionLine(const ALine1, ALine2: TsgLine): TsgLine;
function IndexOfPts2D(const APts: TsgPoints4; const P: TFPoint): Integer;
function IndexOfPoint(const AList: TList;
  const APoint: Pointer; const AProc: TsgObjProcCompare): Integer;
function IndexOfFPoint(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
function IndexOfFPointNearest(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const ANearestOrFar: Boolean; const AFindPoint: PFPoint): Integer;
function IndexOfArrayFPoint(const APoints: array of TFPoint; const APoint: TFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
procedure sgInflateRect(var ARect: TRect; const ADX, ADY: Integer);
procedure InflateFRect(APRect: PFRect; const ADX, ADY, ADZ: Double); overload;
procedure InflateFRect(APRect: PFRect; APPoint: PFPoint); overload;
function IntersectBox(const ABox: TFRect; const AP1, AP2: TFPoint): Boolean; overload;
function IntersectBox(const ABox: TFRect; const ACenter: TFPoint; const ARadius, AngleS, AngleE: Double): Boolean; overload;
function IntersectBox(const ABox: TFRect; APoints: IsgArrayFPoint; const AClosed: Boolean = False): Boolean; overload;
function IntersectFRect2D(const R1, R2: TFRect;
  const Epsilon: Double = fDoubleResolution): Integer;
function IntersectFRect(const R1, R2: TFRect;
  const Epsilon: Double = fDoubleResolution): Integer;
function IntersectRectF2D(const R1, R2: TF2DRect; const AR: PF2DRect): Boolean;
function IsArcClosed(const AStartAngle, AEndAngle: Double): Boolean;
function IsArcClosedAR(const AArc: TsgArcR): Boolean;
function IsRayIntersectsBox(const ARayStart, ARayVector: TFPoint;
  const ABox: TFRect; AIntersectPoint: PFPoint = nil) : Boolean;
function MiddleFPoint(const AP1, AP2: TFPoint): TFPoint;
function MiddleF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
function MiddleFPoint2D(const AP1, AP2: TFPoint): TFPoint;
function MiddlePoint(const AP1, AP2: TPoint): TPoint; overload;
function MiddlePoint(const AP1, AP2: TPointF): TPointF; overload;
function MinorFPoint(const AP, APB: TFPoint; const AMode: Byte): TFPoint;
function MirrorFPoint(const APoint, ACenter: TFPoint): TFPoint;
function MirrorFPointOfLine(const AP, AP1, AP2: TFPoint): TFPoint;
function MultiplyFPoint(const AP1, AP2: TFPoint): TFPoint;
function MultiplyFPointParam(const AP: TFPoint; const AM: Double): TFPoint;
function SignFPoint(const APoint: TFPoint): TFPoint;
function NormalizeArcAngels(const AArc: TsgArcR): TsgArcR;
function Ort(const P: TFPoint): TFPoint;
{$IFNDEF SG_NO_EXTENDED}
function NormalizeVector(var A: array of Extended): Extended; overload;
{$ENDIF}
function NormalizeVector(var A: array of Double): Extended; overload;
function NormalizeVector(var A: array of Single): Extended; overload;
function NormalizeVectorFPoint(var APoint: TFPoint): Extended;
function PerpendicularLine(const ALine: TsgLine; const APointOnLine: TFPoint): TsgLine;
function PointClassify(const APoint, AStartPoint, AEndPoint: TFPoint;
  const AAccuracy: Double = 0): TsgPointClassify;
function PointClassifyCoords(const APX, APY, AP1X, AP1Y, AP2X, AP2Y: Extended;
  const AAccuracy: Extended = 0): TsgPointClassify;
function PointClassify2DPts(const APoint, AStartPoint, AEndPoint: TF2DPoint;
  const AAccuracy: Double = 0): TsgPointClassify;
function PointClassifyTest(const APX, APY, AP1X, AP1Y, AP2X, AP2Y: Double;
  const AAccuracy: Extended = -1): TsgPointClassify;
function PointClassifyByArc(const Arc: TsgArcR; const APoint: TFPoint): TsgPointClassify;
function PointClassifyEx(const ALine: TsgLine; const APoint: TFPoint): TsgPointClassify;
function PointDotProduct(const P1, P2 : TFPoint): TsgFloat;
function F2DPointClassify(const APoint, AStartPoint, AEndPoint: TF2DPoint): TsgPointClassify;
function PointCrossedPerpendicularLines(const P1, P2, P: TFPoint): TFPoint;
function PointCrossedPerpendicularSegments(const ALine: TsgLine; const APoint: TFPoint; const AC: PFPoint): Boolean;
function Proportional(const AXInput, AYInput, AOutput: TsgFloat; AOutputIsX : Boolean): TsgFloat; //For internal using
function Pt2DXMat(const AP: TF2dPoint; const M: TFMatrix): TF2dPoint;
function PtXScalar(const AP: TFPoint; const AVal: TsgFloat): TFPoint; overload;
function PtXScalar(const AP: TPoint; const AVal: TsgFloat): TPoint; overload;
function PtXScalar(const AP: TPointF; const AVal: TsgFloat): TPointF; overload;
function Pt2XScalar(const AP: TF2DPoint; const AVal: TsgFloat): TF2DPoint;
function RelativePtXMat(const ABasePt, APt: TFPoint; M: TFMatrix): TFPoint;
function Reverse(const P: TFPoint): TFPoint;
function ReverseScale(const AScale: TFPoint): TFPoint;
function RotateAroundFPoint(const APoint, ABasePoint: TFPoint; const Abs: Boolean; const Angle: Double): TFPoint;
function RotateFPoint(const P: TFPoint; const S, C: Extended): TFPoint; overload;
function RotateFPoint(const P: TFPoint; Angle: Extended): TFPoint; overload;
function RotateFPointOfLine(const AP, AP1, AP2: TFPoint): TFPoint;
function RotatePoint(const P: TPoint; const Angle: Extended): TPoint;
procedure RotateFRect(var R: TFRect; Angle: Extended);
function RoundFPoint(const APoint: TFPoint; const ADigits: Byte): TFPoint; overload;
function RoundFPoint(const APoint: TFPoint; const ADigits, AFactor: Byte): TFPoint; overload;
function RoundF2DPoint(const APoint: TF2DPoint; const ADigits: Byte): TF2DPoint;
function SafeSplitArcPt(const AArc: TsgArcR; const ACutPt: TFPoint;
  var AArc1, AArc2: PsgArcR; AEpsilon: Double = fDoubleResolution): Boolean;
function SafeSplitArcPtsToList(const ASourceArc: TsgArcR; const ACutPts: array of TFPoint;
  const AList: TList; AEpsilon: Double = fDoubleResolution): Boolean;
function SafeSplitLinePt(const ALine: TsgLine; const ACutPt: TFPoint;
  var ALine1, ALine2: PsgLine; AEpsilon: Double): Boolean;
function SafeSplitLinePtsToList(const ASourceLine: TsgLine; const ACutPts: array of TFPoint;
  const AList: TList; AEpsilon: Double = fDoubleResolution): Boolean;
function ScalarMultiplyVectorsCoords(const AX1, AY1, AX2, AY2: Extended): Extended; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function ScalarMultiplyVectorsCoords(const AX1, AY1, AZ1, AX2, AY2, AZ2: Extended): Extended; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
function ScalarMultiplyVectors(const Vector1, Vector2: TFPoint): Extended; overload;
function ScalarXMat(const AValue: TsgFloat; AType: Byte;
  const AMatrix: TFMatrix; const ASing: Boolean = False): TsgFloat; overload;
function ScaleFPointOfLine(const AP, APBase: TFPoint; const AScale: Double): TFPoint;
procedure CheckMinValueFPoint2D(var APoint: TFPoint; const AAccuracy: Double = fDoubleResolution);
function SetAnglesCorrect(var Angle1, Angle2: Double; const AMiddle: Double;
  const Correct: Boolean): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function SetAnglesByPoints(const ACenter, AP1, AP2, AP3: TFPoint;
  const Correct: Boolean; var Angle1, Angle2: Double): Boolean;
function SetRatioAndMajorPoint(const ARadiusX, ARadiusY: Double; var ARatio: DOuble; var AMajorPoint: TFPoint): Boolean;
function SetRatioAndMajorPointByPoints(const APoint, ARatPt1, ARatPt2: TFPoint; var ARatio: DOuble; var AMajorPoint: TFPoint): Boolean;
procedure SplitArcPt(const Arc: TsgArcR; const AP: TFPoint; var Arc1, Arc2: TsgArcR);//this point(s) on arc
procedure SplitArcPt2(const Arc: TsgArcR; const AP1, AP2: TFPoint; var Arc1, Arc2, Arc3: TsgArcR);//this point(s) on arc
function SplitArcWithArc(const Arc1, Arc2: TsgArcR; var AR1, AR2, AR3: TsgArcR): Byte;
function SplitArcWithLine(const Arc: TsgArcR; const Line: TsgLine;
  var AR1, AR2, AR3: TsgArcR; const ACross1: PFPoint = nil; const ACross2: PFPoint = nil): Byte;
function SplitCircleWithLine(const ACenter: TFPoint; const ARadius: Double; const Line: TsgLine; var AL1, AL2: TsgArcR): Byte;
procedure SplitLinePt(const ALB: TsgLine; const AP: TFPoint; var AL1, AL2: TsgLine);//this point(s) on line
procedure SplitLinePt2(const ALB: TsgLine; const AP1, AP2: TFPoint; var AL1, AL2, AL3: TsgLine);//this point(s) on line
function SplitLineWithArc(const Line: TsgLine; const Arc: TsgArcR; var AL1, AL2, AL3: TsgLine): Byte;
function SplitLineWithLine(const Line1, Line2: TsgLine; var AL1, AL2: TsgLine): Byte;
function SubF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
function SubFPoint(const AP1, AP2: TFPoint): TFPoint;
function SubFPoint2D(const AP1, AP2: TFPoint): TFPoint;
function SubPoint(const AP1, AP2: TPoint): TPoint; overload;
function SubPoint(const AP1, AP2: TPointF): TPointF; overload;
procedure TransRectCorners(var ARect: TFRect; const AMatrix: TFMatrix);
function TurnAroundOfVector(const APoint, AVect: TFPoint; AAngle: Double): TFPoint;
function UnionArcWithArc(const Arc1, Arc2: TsgArcR; var AR: TsgArcR): Boolean;
procedure UnionFRect(var Dst: TFRect; const Src: TFRect);
function Vector(const P1, P2: TFPoint): TFPoint;
function FPointTrimExtents(const APoint: TFPoint): TFPoint;

function CreatePolyPolygon(const APolylines: TsgObjectList): TsgPolyPolygon;

function ExtractBoxParams(const ABox: TFRect; var ACenter, ASizeDir: TFPoint): Extended;
function MakeFRectByParams(const ACenter, ASizeDir: TFPoint; const ARadius: Extended): TFRect;

{AutoCAD specificity}

function ApplyScale(const ARealImgMatrix: TFMatrix; const AOffset: TFPoint;
  const AExtents: TFRect; const ARectLeft, ARectTop, ARectRight, ARectBottom: Double;
  const AStretch, ANotYCrossover: Boolean): TFMatrix;{$IFDEF USE_INLINE}inline;{$ENDIF}
function BGRToRGB(ABGR: Integer): Integer; register; {$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
procedure CheckHeadVar(const APHeadVarStruct: PsgHeadVarStruct);
function IsNameCorrect(const AName: string;
  const AExcludeCharSet: TSysCharSet): Boolean; overload;
function IsNameCorrect(const AName: string): Boolean; overload;
function GetNameCorrect(const AName: string): string; overload;
function GetNameCorrect(const AName: string; const AExcludeCharSet: TSysCharSet): string; overload;
function ColorToDXF(const Value: TsgColorCAD): Cardinal;
procedure DoExtrusion(var P: TFPoint; const N: TFPoint);
procedure DoNewAutoCADAxes(const AV: TFPoint; var AUX, AUY, AUZ: TFPoint);
procedure DoPreExtrusion(var P: TFPoint; const N: TFPoint);
function Extruded(const P: TFPoint): Boolean;

// Finding shx paths functions
function FindAutoCADSHXPaths(APaths: TStringList): Boolean;
function FindDWGTrueViewSHXPaths(APaths: TStringList): Boolean; overload;
function FindDWGTrueViewSHXPaths(APaths: TStringList; ARegistryAccess: TsgRegistryAccess): Boolean; overload;
function FindSGSHXPaths(const APaths: TStringList): Boolean;
function FindSHXPaths(const APaths: TStringList): Boolean; // Custom finding
function BuildMatrix(const APoint, AScale, AExtrusion, AOffset: TFPoint;
  const AAngle: Double; const AInitMatrix: TFMatrix): TFMatrix;
function ExtractMatrixAbsoluteScale(const AMatrix: TFMatrix;
  const AResolution: Double = fMaxResolution): TFPoint; overload;
function ExtractMatrixAbsoluteScale(const AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): TFPoint; overload;
procedure ExtractMatrixParams(const AMatrix: TFMatrix; var APoint, AScale,
  AExtrusion: TFPoint; var AAngle: Double);
function ExtrusionToMatrix(const P: TFPoint): TFMatrix;
function ExtrusionToMatrixT(const P: TFPoint): TFMatrix;
function GetArrowTypeByName(AName: string; const DefValue: TsgDimArrowType): TsgDimArrowType;
function GetAxisByLine(const AP1, AP2: TFPoint; const Width: Double; const AxisType: TsgAxisType): TsgLine;
function GetCADColor(const AColor: Cardinal; const ASkipByBlock: Boolean;
  APalett: PsgCADPalett = nil): Cardinal;
function GetCADPixelSize(const AMatrix: TFMatrix; const ANumberPixels: Double): Double;
function GetLineByAxis(const AP1, AP2: TFPoint; const Width: Double; const AxisType: TsgAxisType): TsgLine;
function GetNameByArrowType(const AType: TsgDimArrowType; const ADefValue: string): string;
function GetNameByEntTypeEx(const AType: Integer): string;
function GetViewTwistMatrix(const ADirect: TFPoint; const AAngle: Double): TFMatrix;
function GetColorByBackgroud(const BackgroundColor: TColor): TColor;
function GetColorByLeftTopPixel(AGraphic: TGraphic; var ATransparency: Boolean;
  ADefColor: TColor = clWhite): TColor;
function IntToColor(AValue: Integer; APalett: PsgCADPalett = nil): TColor;
function MatFromExtr(const P: TFPoint; Angle: Double): TFMatrix;
function IsScaled(const AScale: TFPoint): Boolean;
function SetArcByAxis(const AP1, AP2, APM: TFPoint; const Width: Double; const AxisType: TsgAxisType;
  var APts: TsgPoints3; const AAccuracy: Double = fDoubleResolution): Boolean;
function SetAxisByArc(const AP1, AP2, APM: TFPoint; const Width: Double; const AxisType: TsgAxisType;
  var APts: TsgPoints3; const AAccuracy: Double = fDoubleResolution): Boolean;
procedure SetSHXSearchPaths(const APaths: TStringList);
function StringToACADPresentation(const AString: sgUnicodeStr; const ACodePage: Integer): sgRawByteString;
function sgCodePageFromDWG(const DWGCodePage: Byte; const ADefaultCodePage: Word = CP_ACP): Integer;

//common DWG supporting functions
function sgDWGCodePageFromCP(const CodePage: Integer; ConverToRealCP: Boolean = False): Word;
function sgDWGVerToString(const Version: TsgDWGVersion;
  const AName: Boolean = False; const AUseAutoCADName: Boolean = True): string;
function sgStringToDWGVersion(const AString: string;
  const ADefault: TsgDWGVersion = acR2000): TsgDWGVersion;
function sgDWG2004Coding(Data: Pointer; Size: Cardinal; ASeed: Cardinal = 1): Cardinal;
function sgDWG2004Decode(Data: Pointer; Size: Cardinal): Cardinal;
function sgDWG2004Encode(Data: Pointer; Size: Cardinal): Cardinal;
procedure sgDWG2004PageHeaderDecode(AData: Pointer; AOffset: Cardinal);
procedure sgDWG2004PageHeaderEncode(AData: Pointer; AOffset: Cardinal);
function sgDWGAlignSize(const Length: Cardinal; const Step: Cardinal = $20): Integer;
function sgDXFCodePageName(const CodePage: Integer): string;
function sgDWG2004SectionCheckSum(const Data: Pointer; Seed, Size: Cardinal): Cardinal;

{ Text align }

function AlignByBox(const AAlign: Integer; const ABox: TFRect;
  const AHeight: Double = 0): TFPoint;
function GetTextWidthByCanvas(ACanvas: TCanvas; AText: string): Integer;
{is-functions}

function GetAccuracy(const ALength: Double; const AResolution: Double = fDoubleResolution): Double;
function GetAccuracyByFPoints(const APoint1, APoint2: TFPoint;
  const AResolution: Double = fDoubleResolution): Double;
function GetAccuracyByPoint(const APoint: TFPoint; const AResolution: Double = fDoubleResolution): Double;
function GetAccuracyByLine(const ALine: TsgLine;
  const AResolution: Double = fDoubleResolution): Double;
function GetAccuracyByBox(const ABox: TFRect;
  const AResolution: Double = fDoubleResolution): Double; overload;
function GetAccuracyByBox(const ABox: PFRect;
  const AResolution: Double = fDoubleResolution): Double; overload;
function GetBoxType(const ABox: TFRect; const Resolution: Double = fDoubleResolution): TsgBoxType; overload;
function GetBoxType(const ASize: TFPoint; const Resolution: Double = fDoubleResolution): TsgBoxType; overload;
function GetMatrixDirection(const AMatrix: TFMatrix): TPoint;
procedure InitInt64(const P: Pointer; var PBit: TsgInt64);
function InDeltaRange(AValue, ABase, ADelta: Double): Boolean;
function Is3DRect(const ARect: TFRect): Boolean;
function IsAngleInAngles(const Angle, A1, A2:Double; AEpsilon: Double = fDoubleResolution): Boolean;
function IsAngleInAnglesAP(const Angle: Double; const Arc: TsgArcR;
  const AEpsilon: Double = fDoubleResolution): Boolean;
function IsAnglesInAngles(const AIn1, AIn2, AOut1, AOut2:Double): Boolean;
function IsBadFPoint(const APoint: TFPoint): Boolean;
function IsBadFPoint2D(const APoint: TFPoint): Boolean;
function IsBadRect(ARect: TFRect): Boolean;
function IsBadRectF2D(const R: TF2DRect): Boolean;
function IsBadRectI(const R: TRect; const ALess: Boolean = False): Boolean;
function IsBadMatrix(const AMatrix: TFMatrix): Boolean;
function IsClass(const AClass, AClassParent: TClass): Boolean;
function IsCrossArcAndPntsAP(const Arc: TsgArcR; const AP1, AP2: TFPoint; const Cross: PFPoint): Boolean;
function IsCrossArcAndSegment(const ArcPts: TsgPoints3; const ALine: TsgLine; const ACross1, ACross2: PFPoint): Integer;
function IsCrossArcAndSegmentAP(const AP: TsgArcR; const ALine: TsgLine; const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
function IsCrossArcAndSegmentCR(const C: TFPoint; const Radius, Angle1, Angle2: Double; const ALine: TsgLine; const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
function IsCrossArcs(const ArcPts1, ArcPts2: TsgPoints3; const ACross1, ACross2: PFPoint): Integer;
function IsCrossArcsAP(const A1, A2: TsgArcR; const ACross1, ACross2: PFPoint; Epsilon: Double = fDoubleResolution): Integer;
function IsCrossCircleAndArcAP(const ACircle, AArc: TsgArcR; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCircleAndLine(const ArcPts: TsgPoints3; const ALine: TsgLine; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCircleAndLinePntsAP(const AP: TsgArcR; const AP1, AP2: TFPoint; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCircleAndLineAP(const AP: TsgArcR; const ALine: TsgLine; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCircleAndLineCR(const Center: TFPoint; const ARadius: Double;
  const ALine: TsgLine; const ACross1, ACross2: PFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
function IsCrossCircleAndSegmentAP(const AP: TsgArcR; const ALine: TsgLine; const ACross1, ACross2: PFPoint;
  const AAccuracy: Double = 0): Integer;
function IsCrossCircleAndSegmentCR(const Center: TFPoint; const ARadius: Double; const ALine: TsgLine; const ACross1, ACross2: PFPoint;
  const AAccuracy: Double = 0): Integer;
function IsCrossCircles(const ArcPts1, ArcPts2: TsgPoints3; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCirclesAP(const Arc1, Arc2: TsgArcR; const ACross1, ACross2: PFPoint): Integer;
function IsCrossCirclesCR(const AC1: TFPoint; const AR1: Double; const AC2: TFPoint;
  const AR2: Double; const ACross1, ACross2: PFPoint; Epsilon: Double = fDoubleResolution): Integer;
function IsCrossCoords(const X11, Y11, X12, Y12, X21, Y21, X22, Y22: Double;
  const ACross: PFPoint;
  const AAccuracy: Double = fDoubleResolution): Integer;//{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsCrossLines(const ALine1, ALine2: TsgLine; const ACross: PFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsCrossLines2D(const ALine1, ALine2: TF2DLine; const ACross: PF2DPoint): Boolean;
function IsCrossLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution): Boolean;
function IsCrossSegmentAndLinesPts(const APS1, APS2, APL1, APL2: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution): Boolean;
function IsCrossArcAndBoxAR(const AArc: TsgArcR; const ABox: TFRect): TFPointList;
function IsCrossLineAndBox(const AP1, AP2: TFPoint; const ABox: TFRect;
  var ACross1, ACross2: TFPoint; const AMode: Byte = 1): Integer;
function IsCrossPerpendicularLines(const AP1, AP2, AP3: TFPoint; const ACross: PFPoint): Boolean;
function IsCrossPoints4AndPolyPolygon(APoints: TsgPoints4; AClosed: Boolean;
  ABoundaries: TList; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
function IsCrossPoints4AndPolyPolygonList(APoints: TsgPoints4; AClosed: Boolean;
  ABoundaries: TsgObjectList; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
function IsCrossPointsI(const AP11, AP12, AP21, AP22: TPoint; const ACross: PPoint): Integer;
function IsCrossPolyPolygonAndPolyPolygon(APoly,ABoundaries: TList;
  AClosed: Boolean; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
function IsCrossPolyPolygonAndPolyPolygonList(APoly ,ABoundaries: TsgObjectList;
  AClosed: Boolean; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
function IsCrossSegments(const ALine1, ALine2: TsgLine; const ACross: PFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsCrossSegments2D(const ALine1, ALine2: TF2DLine; const ACross: PF2DPoint): Boolean;
function IsCrossSegmentsPts(const AP11, AP12, AP21, AP22: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution;
  const AModes: TsgByteSet = [2]): Boolean;
function IsCrossSegmentsPts2D(const AP11, AP12, AP21, AP22: TF2DPoint; const ACross: PF2DPoint): Boolean;
function IsCrossPointsPts(const AP11, AP12, AP21, AP22: TFPoint;
  const AsSegment: Boolean; const APoint: PFPoint): Boolean;
function IsCrossSegmentPolylinePts(const AP1, AP2: TFPoint;
  APts: IsgArrayFPoint; const ACloesd: Boolean; const ACroses: TFPointList;
  const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False;
  AAccuracy: Double = 0): Integer;
function IsCrossSegmentPolylineAndRect(const ARect: TsgPoints4;
  APts: IsgArrayFPoint; const ACloesd: Boolean; const ACroses: TFPointList;
  const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False): Integer;
function IsCrossPolylines(APts1: IsgArrayFPoint; const AClosed1: Boolean;
  APts2: IsgArrayFPoint; const AClosed2: Boolean;
  const ACroses: TFPointList; const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False;
  AAccuracy: Double = 0; AStartFromEnd: Boolean = False): Integer;
function IsCrossVertexes(const AVertexes1, AVertexes2: IsgArrayVertexes;
  Epsilon: Double = fDoubleResolution; const ACrosses: TFPointList = nil;
  AGetAllPoints: Boolean = False; const AIndexes: TsgIntegerList = nil): Boolean;
function IsPointsInHalfPlane(const AP1,AP2: TFPoint; APts: IsgArrayFPoint;
  const AAccuracy: Double = 0): Boolean;
function IsTangentOfArc(const ACenter: TFPoint; const AR, AStart, AEnd: Double;
  const APoint: TFPoint;  var AP1, AP2: TFPoint): Integer;
function IsTangentOfCircle(const ACenter: TFPoint; const AR: Double;
  const APoint: TFPoint; var AP1, AP2: TFPoint): Integer;
function IsTangentOfEllipse(const ACenter, ARatPt: TFPoint; const ARatio,
  AStart, AEnd: Double; const APoint: TFPoint; var AP1, AP2: TFPoint): Integer;
function IsHexDigit(AChar: Char): Boolean;
function IsEvent(const AValue: Integer): Boolean;
function IsEqualAngle(const AVal1, AVal2: TsgFloat;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsEqualArcR(AArc1, AArc2: TsgArcR): Boolean;
function IsEqualDirection(const P1, P2, PB: TFPoint): Boolean;
function IsEqualCirclesAP(AArc1, AArc2: TsgArcR): Boolean;
function IsEqualColorCAD(const AValue1,  AValue2: TsgColorCAD): Boolean;
function IsEqualF2DPoints(const Point1, Point2: TF2DPoint; Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualFMatrix(const AM1,AM2: TFMatrix; const AIsWithE0: Boolean;
  Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualFPoints(const Point1, Point2: TFPoint; Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualFPointsEps(const Point1, Point2: TFPoint; const AResolution: Double): Boolean;
function IsEqualFPoints2D(const Point1, Point2: TFPoint; Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualFRects(const AR1, AR2: TFRect): Boolean;
function IsEqualFRects2D(const AR1, AR2: TFRect; Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualLists(const AL1, AL2: TList; Compare: TsgProcEqual = nil): Boolean;
function IsEqualListsEnum(const AL1, AL2: TList; Compare: TsgProcEqual = nil): Boolean;
function IsEqualObjectListsEnum(const AL1, AL2: TsgObjectList;
  Compare: TsgProcObjectEqual = nil): Boolean;
function IsEqualFPointsClassify(const ALinePoint1, ALinePoint2,
  APoint1, APoint2: TFPoint): Boolean; overload;
function IsEqualF2DPointsClassify(const ALinePoint1, ALinePoint2,
  APoint1, APoint2: TF2DPoint): Boolean; overload;
function IsEqualPointClassify(const ALine: TsgLine;
  const APoint1, APoint2: TFPoint): Boolean;
function IsEqualPoints(const Point1, Point2: TPoint): Boolean; overload;
function IsEqualPoints(const Point1, Point2: TPointF; Epsilon: Double = fAccuracy): Boolean; overload;
function IsEqualsgLines(ALine1, ALine2: TsgLine; Epsilon: Double = fDoubleResolution): Boolean;
function IsEqualStringLists(const AStrings1, AStrings2: TStrings; ACaseSensitive: Boolean = False): Boolean;
function IsEqualEntClass(const AValue1, AValue2: TsgEntClass): Boolean;
function IsHasSubstring(const AValue, AData: string): Integer;//if find substring return 0, else 1!!!
function IsMatch(const AFilter, AIgnoreFilter, AName: string): Boolean; //returns result based on regular expressions

function IsUniquePoints(const APoints: IsgArrayFPoint;
  const Epsilon: Double = fDoubleResolution): Boolean;
function IsUniquePointsIndex(const APoints: IsgArrayFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
function IsUniquePointsIndeses(const APoints: IsgArrayFPoint;
  const ACheckFirstAndLastPoints: Boolean = True;
  const Epsilon: Double = fDoubleResolution): TPoint;

function IsLineOnLine(const ALine1, ALine2: TsgLine): Boolean;
function IsLineVerticalPts(const AP1, AP2: TFPoint): Boolean;
function IsSelfIntersectingPolyline(APts: IsgArrayFPoint;
  const AClosed: Boolean = False; Epsilon: Double = fDoubleResolution;
  const ACrosses: TFPointList = nil; AGetAllPoints: Boolean = False;
  const AIndexes: TsgIntegerList = nil): Boolean;
function IsSelfIntersectingVertexes(AVertexes: IsgArrayVertexes; var AIsNearest: Boolean;
  const AClosed: Boolean = False; Epsilon: Double = fDoubleResolution;
  const ACrosses: TFPointList = nil; AGetAllPoints: Boolean = False;
  const AIndexes: TsgIntegerList = nil): Boolean;
function IsParalleniarLines(const L1, L2: TsgLine;
  const Accuracy: Double = fAccuracy): Boolean;
function IsParalleniarLinesCoords(const X11, Y11, X12, Y12, X21,
  Y21, X22, Y22, Accuracy: Double): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsPerpendicularLinesCoords(const X11, Y11, X12, Y12, X21,
  Y21, X22, Y22, Accuracy: Double): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsParalleniarLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const Accuracy: Double = fAccuracy): Boolean;
function IsParalleniarLinesPts2D(const AP11, AP12, AP21, AP22: TF2DPoint;
  const Accuracy: Double = fAccuracy): Boolean;
function IsPerpendicularLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const Accuracy: Double = fAccuracy): Boolean;
function IsPerpendicularLinesPts2D(const AP11, AP12, AP21, AP22: TF2DPoint;
  const Accuracy: Double = fAccuracy): Boolean;
function IsPointInArc(const Arc: TsgPoints3; const Width: Double; const APoint: TFPoint): Boolean;
function IsPointInCircle(const ACenter: TFPoint; const ARadius: Double; const APoint: TFPoint): Boolean;
function IsPointInF2DRect(const R: TF2DRect; const P: TF2DPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointInFRect(const R: TFRect; const P: TFPoint): Boolean;
function IsPointInFRect2D(const R: TFRect; const P: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointInPolyline(const APts: TList; const AP: TFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Boolean; overload;
function IsPointInPolyline(APts: IsgArrayFPoint; const AP: TFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer; overload;
function IsPointInPolyPolyline(const APolylines: TsgObjectList; const AP: TFPoint;
  const ABoxes: TList = nil; const AAccuracy: Double = 0): Boolean;
function IsPointInPolyPolylineInt(const APolylines: TList; const AP: TFPoint;
  const AAccuracy: Double = 0): Integer;
function IsPointInPolyPolylineListInt(const APolylines: TsgObjectList;
  const AP: TFPoint; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer;
function IsPointInRectangle(const APts: TsgPoints4;
  const AP: TFPoint; Box: PFRect = nil;
  const AAccuracy: Double = fDoubleResolution;
  const ACheckEdge: Boolean = False): Integer;
function IsPointsInRectangle(const APts: TsgPoints4;
   const APoints: TsgPoints4;
    Box: PFRect = nil; const AAccuracy: Double = fDoubleResolution;
    const ACheckEdge: Boolean = False): Integer;
function IsPointInRectI(const R: TRect; const P: TPoint): Boolean;
function IsPointInSector(const Arcs: PsgArcsParams; const AP: TFPoint; Box: PFRect = nil): Byte;
function IsPointInTriangle(const AP1, AP2, AP3, AP: TFPoint): Boolean;
function IsFRectInPolyline(APts: IsgArrayFPoint; const ABox: TFRect;
  Box: PFRect = nil; const AAccuracy: Double = 0): Boolean;
function IsBox2DInPolyline(APts: IsgArrayFPoint; const ABox: TFRect;
  Box: PFRect = nil; const AAccuracy: Double = 0): Integer;
function IsPointsInPolyline(const APoints, APolyline: IsgArrayFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer;
function IsPolyPointsInPolyline(const APolyPolyline: TsgObjectList;
  const APolyline: IsgArrayFPoint; Box: PFRect = nil;
  const AAccuracy: Double = 0): Integer;
function IsPointOnArc(const APC, AP1, AP2, APoint: TFPoint): Boolean;
function IsPointOnArcs(const ArcPts1, ArcPts2: TsgPoints3; const APoint: TFPoint): Boolean;
function IsPointOnArcAP(const Arc: TsgArcR; const APoint: TFPoint; AEpsilon: Double = fDoubleResolution): Boolean;
function IsPointOnArcCR(const APC: TFPoint; const R, A1, A2: Double;
  const APoint: TFPoint; AEpsilon: Double = fDoubleResolution): Boolean;
function IsPointOnCircle(const APC, AP1, APoint: TFPoint): Boolean;
function IsPointOnCircleCR(const ACenter: TFPoint; const ARadius: Double; const APoint: TFPoint): Boolean;
function IsPointOnLine(const ALine: TsgLine; const APoint: TFPoint): Boolean;
function IsPointOnLineI(const AP1, AP2, APoint: TPoint;
  const AAccuracy: Double = 1): Boolean;
function IsPointOnLinePts(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = 0): Boolean;
function IsPointOnLinePts3D(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointOnSegment(const ALine: TsgLine; const APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointOnSegmentI(const AP1, AP2, APoint: TPoint): Boolean;
function IsPointOnSegmentPts(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointOnSegmentPts2D(const AP1, AP2, APoint: TF2DPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPointOnPolyline(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
function IsPolygonClockwise2D(APoly: IsgArrayFPoint): Boolean;
function IsRange(const AVal, ARange: Double): Boolean; overload;
{$IFNDEF SG_NO_EXTENDED}
function IsRange(const AVal, ARange: Extended): Boolean; overload;
{$ENDIF}
function IsRangeFPoints(const AP1, AP2, ARange: TFPoint): Boolean;
function IsRangeIncBoundaries(const AVal, ARange: Double): Boolean;
function IsRectangle(const APoints: IsgArrayFPoint; const AClosed: Boolean;
  const ACheckRotated: Boolean; const Accuracy: Double = fDoubleResolution): Boolean;
function IsRectanglePts4(const APoints: TsgPoints4;
  const Accuracy: Double = fDoubleResolution): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsRectEmptyF2D(const R: TF2DRect): Boolean;
function IsRectIntersectPolyPolygon(const ARect: TFRect;
  const APolygons: TList): Integer;
function IsRectInRectF2D(const R1, R2: TF2DRect): Boolean;
function IsRotatedFMat(const M: TFMatrix; const IsFullCheck: Boolean = False): Boolean;
function IsRotatedFMatEx(const M: TFMatrix; const ANormals: PPoint;
  const IsFullCheck: Boolean): Boolean;
function IsValInParam(AVal, V1, V2: Double;
  const AAccuracy: Double = fDoubleResolution): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsValInParamI(const AVal, V1, V2: Integer): Boolean;
function IsSatBinaryFile(P: Pointer; Size: Integer): Boolean;
function IsValidEmail(const Value: string): Boolean;
{$IFNDEF SGDEL_6}
function SameValue(const A, B: Extended; Epsilon: Extended = 0): Boolean; overload;
function SameValue(const A, B: Double; Epsilon: Double = 0): Boolean; overload;
function SameValue(const A, B: Single; Epsilon: Single = 0): Boolean; overload;
function Sign(const AValue: Integer): TValueSign; overload;
function Sign(const AValue: Int64): TValueSign; overload;
function Sign(const AValue: Double): TValueSign; overload;
function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload;
function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload;
function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0.0): Double; overload;
function InRange(const AValue, AMin, AMax: Integer): Boolean; overload;
function InRange(const AValue, AMin, AMax: Int64): Boolean; overload;
function InRange(const AValue, AMin, AMax: Double): Boolean; overload;
function EnsureRange(const AValue, AMin, AMax: Integer): Integer; overload;
function EnsureRange(const AValue, AMin, AMax: Int64): Int64; overload;
function EnsureRange(const AValue, AMin, AMax: Double): Double; overload;
function IsZero(const A: Extended; Epsilon: Extended = 0): Boolean; overload;
function IsZero(const A: Single; Epsilon: Single = 0): Boolean; overload;
{$ENDIF}
 function ExtendLinePtsByPoint(var ALine: TsgLine; const APoint: TFPoint;
  const ABoundPolygons: TList): Byte;
function ExtendLinePtsByPointList(var ALine: TsgLine; const APoint: TFPoint;
  const ABoundPolygons: TsgObjectList): Byte;
function ExtendArcPtsByPoint(var AArc: TsgArcR; const APoint: TFPoint;
  const ABoundPolygons: TList): Byte;
function ExtendArcPtsByPointList(var AArc: TsgArcR; const APoint: TFPoint;
  const ABoundPolygons: TsgObjectList): Byte;
function TrimmingLinePts(const ALine: TsgLine; const APts: TsgPoints4; const ACutLine: PsgLine = nil): TsgLine;
procedure TrimmingLineRect(var AP1, AP2: TPoint; const ARect: TRect);
function IsHandleStr(const AStr: string): Boolean;
function sgSameText(const S1, S2: string): Boolean;

//by 3d
function InvExtrusionToMatrix(const P: TFPoint): TFMatrix;
procedure MakePlane(const AP1, AP2, AP3: TFPoint; var APlane: TFPoint; ALenSgr: PExtended = nil);{$IFDEF USE_INLINE} inline;{$ENDIF}
function sgCalcPlaneNormal(const AP1, AP2, AP3: TFPoint): TFPoint;
function sgAreaOfTriangle(const AP1, AP2, AP3: TFPoint): Double;
function CalcCircleParams(const APoint1, APoint2, APoint3: TFPoint;
    var ACenter: TFPoint; var ARadiaus: Double): Boolean; overload;
function CalcCircleParams(const AList: TFPointList; var ACenter: TFPoint;
  var ARadiaus: Double; const AAccuracy: Double = 1E-03): Boolean; overload;
procedure CalcPlaneParams(const Points: TFPointList; var ACenter, ANormal: TFPoint);
function sgSegmentSegmentDistance(const AP0Start, AP0Stop,
  AP1Start, AP1Stop : TFPoint) : double;
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TFPoint;
  var Segment0Closest, Segment1Closest : TFPoint);
function sgPointLineClosestPoint(const APoint, APoint1, APoint2: TFPoint): TFPoint;



{operations on List and TObject}

function AddElementsToList(AList: TList; AAddCount: Integer;
  const AValues: array of Pointer; ADisposeOthers: Boolean = False): Boolean;
procedure AddFirstF2DPointUnique(const AList: TFPointList; const AP: TF2DPoint);
procedure AddFirstFPointUnique(const AList: TFPointList; const AP: TFPoint);
function AddFPointInList(const AList: TList; const APoint: TFPoint): PFPoint;
procedure AddFPointList(const AList: TList; const APoint: TFPoint);
function AddF2DPointInList(const AList: TList; const APoint: TF2DPoint): PF2DPoint; overload;
function AddF2DPointInList(const AList: TList; const APoint: TFPoint): PF2DPoint; overload;
procedure AddLastF2DPointUnique(const AList: TFPointList; const AP: TF2DPoint);
procedure AddLastFPointUnique(const AList: TFPointList; const AP: TFPoint);
procedure AssignListToPos(ASource, ADest: TList; APos: Integer);
function BinSearch(AList: TList; ACompareFunc: TListSortCompare; var AIndex: Integer; var AParam): Boolean;
procedure ScaleList(const APoints: TFPointList; const AScale: TFPoint);
{$IFNDEF SGDEL_2009}
function CharInSet(AChar: AnsiChar; const ACharSet: TSysCharSet): Boolean; overload;
function CharInSet(AChar: WideChar; const ACharSet: TSysCharSet): Boolean; overload;
{$ENDIF}
procedure CheckListOfObject(const AListNew, AListOrg: TList;
  const AClass: TClass; const AIsList: Integer = -1); overload;
procedure CheckListOfObject(const AListNew, AListOrg: TsgObjectList;
  const AClass: TClass; const AIsList: Integer = -1); overload;
procedure CheckListOfPF2DPoint(const AListNew, AListOrg: TList);
procedure CheckListOfPFPoint(const AListNew, AListOrg: TList);
procedure ClearBoundariesLists(const ABoundaries: TList);
procedure CopyBackwardPF2DCurveEx(const AData, AList: Pointer; const Added: Boolean);
procedure CopyBackwardPsgLine(const AData, AList: Pointer; const Added: Boolean);
procedure CopyForwardPF2DCurveEx(const AData, AList: Pointer; const Added: Boolean);
procedure CopyForwardPsgLine(const AData, AList: Pointer; const Added: Boolean);

function CreateTsgList: TsgList;
function GetAreaOfList(const AList: TList; const AProc: TsgProcOfPointerGetPoint): Double; overload;
function GetAreaOfList(AList: IsgArrayFPoint; ACount: Integer = -1): Double; overload;
function AddAnsiStringToArray(const S: AnsiString; var A: TAnsiStringArray; var Count: Integer): Integer;

function GetAreaOfCircle(const ARadius: Double): Double;
function GetAreaOfArc(const AArc: TsgArcR): Double;
function GetAreaOfRect2D(ARect: TFRect): Double;
function GetDistanceOfNearestPointByPointList(APts: TList; AStartPt: TFPoint;
  var AMinDistPt: TFPoint): TsgFloat;
function GetLastFPoint(const AList: TList): TFPoint;
function GetListCount(const AList: TList; ALevel: Integer = 0): Integer;
function GetMinBoundary(const ABounds: TList; const APoint: PFPoint; var AAreaMin: Double): TList;
function GetMinBoundaryCustom(const ABounds: TsgObjectList; const APoint: PFPoint; var AAreaMin: Double): Integer;
function GetPerimeterOfList(const AList: TList; const Closed: Boolean; const AProc: TsgProcOfPointerGetPoint): Double; overload;
function GetPerimeterOfList(AList: IsgArrayFPoint; const Closed: Boolean;
  ACount: Integer = -1): Double; overload;
function GetPoint1OfPF2DCurveEx(const APointer: Pointer): TFPoint;
function GetPoint1OfPsgLine(const APointer: Pointer): TFPoint;
function GetPoint2OfPF2DCurveEx(const APointer: Pointer): TFPoint;
function GetPoint2OfPsgLine(const APointer: Pointer): TFPoint;
function IndexOfStrings(const AValue: string; const AStrings: array of string;
  ProcCompare: TsgProcCompareStrings = nil): Integer;
procedure LoadListNils(const AList: TList);
procedure QSortList(const AList: TList; const AProc: TsgProcCompare);
procedure QSortArrayBounds(AArray: Pointer{PPointerArray}; const AProc: TsgProcCompare; L, R: Integer);
function QFindArrayItem(AArray: Pointer{PPointerArray}; L, R: Integer; AItem: Pointer; const AProc: TsgProcCompare; var Index: Integer): Boolean;
function QFindItem(const AArray; L, R: Integer; const AItem;
  const AProc: TsgCustomProcCompare; ElemSize: Integer; var Index: Integer): Boolean;
procedure QSortListBounds(const AList: TList; const AProc: TsgProcCompare; ALeftPos, ARightPos: Integer);
procedure SortBoundariesAsCurve(const ACurves: TList; const ABoundaries: TsgObjectList);//list ACurves has PsgCurveLnk
procedure SortBoundariesLists(const ALines, ABoundaries: TList;
  const AProc1, AProc2: TsgProcOfPointerGetPoint;
  const AProcCopyDataForward, AProcCopyDataBackward: TsgProcCopyData;
  const Clear: Boolean);
procedure SortPointsOnPoint(const APoint: TFPoint; const APoints: TFPointList);

function CreateStringsValues(const AValues: string = '';
  const AValuesDelimiter: Char = cnstEnumValuesDelimiter): TStrings;

{mathematical formulas}

procedure CheckAngles(var AStart, AEnd: Double);
procedure CheckValueExtents(var AValue: Double);{$IFDEF USE_INLINE} inline;{$ENDIF}
function Degree(ARadian: Extended): Extended;
function GetAngleByChordAndRadius(const A, R: Double): Double;
function GetHeightByChordAndLength(const A, L: Double): Double;
function GetHeightByChordAndRadius(const A, R: Extended): Double;
function GetLengthByChordAndHeight(const A, H: Double): Double;
function GetMiddleAngle(AStart, AEnd: Double): Double;
function GetMiddleAngleAP(const AP: TsgArcR): Double;
function GetRadiusByChordAndHeight(const A, H: Double): Double;
function GetBit64(ABit: Byte; const ADigit: UInt64): Byte;
procedure SetBit64(ABitNum, ABit: Byte; var ADigit: UInt64);
function Radian(AAngle: Extended): Extended;
function sgArcSin(const AValue: Double): Double;
function sgArcTan2(const AX, AY: Extended): Double;
function sgFloorZ(const AValue: Double): Int64;
function sgIsNan(const AValue: Double): Boolean;
function sgMod(const AValue, ABase: Double): Double;
function sgModAapproxim(const AValue, ABase: Double; AAapproxim: Double): Double;
function sgModAngle(const AValue: Double; var AModValue: Double;
  const ARadian: Boolean = False): Boolean;
function sgPower10(AValue: Extended; APower: Integer): Extended;{$IFDEF USE_INLINE} inline;{$ENDIF}
function sgSign(const AVal: Double): Integer;
function sgSignZ(const AVal: Double; const AAccuracy: Double = fDoubleResolution): Integer;
function sgSqrt(const AValue: Extended): Double;
function sgIsZero(const AVal: Double;
  const AAccuracy: Double = fDoubleResolution): Boolean;
procedure SinCosRot90(var S, C: Extended);
procedure FPointSinCosRot90(var ASinCos: TFPoint);
function RoundToInt(AValue: Double): Integer;
{$IFNDEF SGDEL_7}
function IsNan(const AValue: Single): Boolean; overload;
function IsNan(const AValue: Extended): Boolean; overload;
{$IFNDEF SGDEL_6}
function IsNan(const AValue: Double): Boolean; overload;
function IsInfinite(const AValue: Double): Boolean;
function DateTimeToJulianDate(const AValue: TDateTime): Double;
function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
function DirectoryExists(const Directory: string): Boolean;
function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word);
{$ENDIF}
{$ENDIF}
function IsFileExists(const AFile: string): Boolean;
function sgGetCompressedFileSize(lpFileName: PChar; lpFileSizeHigh: PSGCardinal): Cardinal;
function sgSimpleRoundTo(const AValue: Double;
  const ADigit: TRoundToRange = 0): Double;
function sgRoundTo(const AValue: Double; const ADigit: Integer): Double; overload;
function sgRoundTo(const AValue: Double; const ADigit, AFactor: Integer): Double; overload;
function AsSingle(const AValue: Extended; const ADefaultMax: Single = 0): Single;
function sgMulDiv(const ANumber, ANumerator, ADenominator: Integer): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}

{function ???}
function AddLastSlash(const S: string; Slash: string = ''): string;
procedure CheckLastSlash(var AStr: string);
function CreateFolders(const AFilePath: string): Integer;
procedure CalcDistanceParameters(AP1,AP2: TFPoint; ADimScale: Double;
  var ADistance,AXYAngle,AZAngle: Double; var ADelta: TFPoint);
function CaptionToCommand(const S: string): string;
function CompareCommand(const ACmd,AEnCaption,ACaption: string): Boolean;
function ConvertIntegerToPlotLayoutFlags(const AValue: Integer): TsgPlotLayoutFlags;
function ConvertPlotLayoutFlagsToInteger(const AValue: TsgPlotLayoutFlags): Integer;
function ConvertToAnsiString(const AText: WideString; const ACodePage: Integer): sgRawByteString;
function ConvertToDXFUnicode(const AText: WideString): sgRawByteString;
function ConvertToRawString(const AText: WideString; const ACurrentCodePage: Integer): sgRawByteString;
function ConvertToWideChar(const  AHi, ALow: AnsiChar): WideChar; overload;
function ConvertToWideChar(const  AHi, ALow: WideChar): WideChar; overload;
function ConvertToWideChar(const AChars: sgRawByteString): WideChar; overload;
function ConvertToWideString(const AText: AnsiString; const ACodePage: Integer): WideString;
function ConvertToUtf8(const AText: WideString): sgRawByteString;
{$IFNDEF SGDEL_XE}
function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: {$IFNDEF SG_NON_WIN_PLATFORM}PBOOL{$ELSE}PInteger{$ENDIF}): Integer;
function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer;
{$ENDIF}
function ConvToFloatDef(const AStr: string; const AValue: Extended): Extended;
function CalcScaleFactor(const AInitial, ADisplayed: TsgInsUnits): Double;
function DoubleToStr(const AVal: Double;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil): string;
function DoubleToStrF(const AVal: Double; const APrecision: Byte;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil): string;
function DoubleToStrFEx(const AVal: Double; const APrecision: Byte;
  const AUseSystemSeparator: Boolean = True;
  AFormatSettings: PFormatSettings = nil): string;
function DoubleToStringWithPrecision(const AValue: Extended;
  const APrecision: Integer): string;
function ExceptionToString(const E: Exception): string;
function StringSentenceCase(const AStr: string): string;
//function StringCapitalizeEachWordCase(const AStr: string): string;
//function StringtOGGLEcASE(const AStr: string): string;
function ChangeSeparator(const AStr: string; const Separator: Char =
  cnstDoubleSeparator): string;
function DoubleToStrVisualRound_(const AVal: Double; const APrecision: Byte;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;
function AreaToStr(const AValue: Double; ADimDec: Integer;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;
function PerimetrToStr(const AValue: Double; ADimDec: Integer;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;

function DeleteEmptyStrings(const AStrings: TStrings): Integer;
function FPointToStr(const APoint: TFPoint; ASeparator: Char = ',';
  AFormatSettings: PFormatSettings = nil): string;
function FPointsToStr(APoints: IsgArrayFPoint): string;
function FPointToStrEx(const APoint: TFPoint; ASeparator: Char; ADelimiter: Char;
  AFormatSettings: PFormatSettings = nil): string;
function ExtractWebFileName(const FileName: string): string;
function GetLongFileName(const FileName: string): string;
function GetLongModuleName(ACheckLibrary: Boolean; AModule: HMODULE): string;
function GetFileName: string;
function GetValidFileName(const AFileName: string): string;
function GetLongFileNameIsNeeded(const AFileName: string): string;
function GetNearestIndex(const AVaules: array of Double;
  const ALen: Integer; const AValue: Double): Integer;
function IntToRomanNumerals(AValue: Cardinal; const IsExtended: Boolean): string;
function AddInt32(AValue: Int64; const ADelta: Integer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
function SubInt32(AValue: Int64; const ADelta: Integer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
function CheckInt32Range(const AValue: Int64): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
function LispDoubleToStr(AValue: Double): string;
function LispStrToDouble(const AStr: string): Double;
function Max(const AValue1, AValue2: Double): Double;
function MaxI(const AValue1, AValue2: Integer): Integer;
function Min(const AValue1, AValue2: Double): Double;
function MinI(const AValue1, AValue2: Integer): Integer;
function IsMaxRangeInt32(const AValue: Int64): Boolean;
//function SingleToPointer(const AValue: Single): Pointer;
function ShortToLongFileName(const ShortName:string): string;
function ShortToLongPath(const ShortName: string): string;
function StringHexToIntDef(const AStr: string; ADefault: Integer): UInt64;
function StringPos(const ASubStr, AStr: string; AStart: Integer): Integer; overload;
function StringPosA(const ASubStr, AStr: AnsiString; AStart: Integer): Integer;
function StringPosNum(const SubStr, Str: string; SubNum, Start: Integer): Integer;
function StringReplace(var Str: string; const Old, New: string): Boolean;
function StringScan(Chr: Char; const AStr: string; AStart: Integer;
  const AReverse: Boolean = False): Integer; overload;
function StringTrim(const AStr: string; const ABegin, AEnd: Char;
  const AInternal: Boolean = True): string;
function StringTrimLeftRight(const S: string; const AChr: Char = ' '): string; {$IFDEF SGDEL_2009} overload; {$ENDIF}
{$IFDEF SGDEL_2009}
function StringTrimLeftRight(const S: AnsiString; const AChr: AnsiChar = ' '): AnsiString; overload;
{$ENDIF}
function StringTrimLeftRightInside(const S: string; DoAll: Boolean = True): string;
function ReplaceToPos(var S: string; const S1, S2: string; ATo: Integer): Integer;
procedure ReplaceEvenUnEven(var S: string; const AChng, AUneven, AEven: string);
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
function CreateFormatSettings(const ASeparator: Char = cnstDoubleSeparator): TFormatSettings;
{$ENDIF}
function StrToDouble(const AStr: string;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil):  Double;
function StrToFPoint(AStr: string; var APoint: TFPoint;
  const ASeparator: Char = cnstDoubleSeparator;
  const ADelimeter: Char = cnstPointSeparator;
  AFormatSettings: PFormatSettings = nil): Boolean;
procedure StrToStrings(const AStr: string; const ADelimiter: string;
  const AStrings: TStrings);
function StrToVal(const AStr: string; var ACode: Integer): Double;
function SubStrCount(const SubStr, Str: string): Integer;
procedure SwapArcs(var A,B: TsgArcR);
procedure SwapBoolean(var A,B: Boolean);
procedure SwapByte(var A,B: Byte);
function SwapBytes(AValue: Word): Word; register;
procedure SwapDoubles(var A,B: Double);
procedure SwapExtendeds(var A,B: Extended);
procedure SwapF2DPoints(var A,B: TF2DPoint);
procedure SwapFLines(var A,B: TsgLine);
procedure SwapFMatrix(var A,B: TFMatrix);
procedure SwapFPoints(var A,B: TFPoint);
procedure SwapObjects(var A,B: TObject);
procedure SwapPointers(var A,B: Pointer);
procedure SwapPoints(var A,B: TPoint);
procedure SwapSGFloats(var AValue1, AValue2: TsgFloat);
procedure SwapSingles(var A,B: Single);
function SwapWords(AValue: DWORD): DWORD; register;
procedure SwapUInt64(var A,B: UInt64); {$IFDEF USE_INLINE}inline;{$ENDIF}
procedure SwapString(var AStr1, AStr2: string);
function sgRawStringToDXFUnicode(const AStr: sgRawByteString;
  const ACodePage: Integer; const AMode: Byte;
  const ACurrentCodePage: Integer): sgRawByteString;
function sgUnicodeToDXFUnicode(const AStr: WideString; const Check: Boolean = False): string;
function Utf8ToUni(ADst: PWideChar; AMaxDstLen: DWORD; ASrc: PChar;
  ASrcLen: DWORD): DWORD;
function sgUTF8ToUnicode(const S: TsgUTF8String): WideString;
function StrToDoubleEx(const AStr: string;
  const AUseSystemSeparator: Boolean = True): Double;
function sgFloatToStr(const AValue: Double): string;
function sgStrToFloat(const AValue: string): Double;
function CreateStringListSorted(const ADuplicates: TDuplicates;
  const ACaseSensitive: Boolean = False): TStringList;
function ExtendedToInt(const AValue: Extended; const AMode: Integer = 0;
  const ARange: Integer = MaxInt): Integer;
function Ord2(const A: TsgInt64): TsgNativeInt; register;

function sgTryStrToInt(const AValue: string; var AResult: Integer): Boolean;
function sgTryStrToFloat(const AValue: string; var AResult: Double;
  const ASeparator: Char = cnstPoint;
  AFormatSettings: PFormatSettings = nil): Boolean;

function ColorRGBToStr(AColor: TColor;
  const AHexPrefix: Char = '$'): string;
function ColorCADToStr(const AColor: TsgColorCAD): string;
function StrToColorRGB(AString: string;
  const AHexPrefix: Char = '#'): TColor;
function StrToColorCAD(const AStr: string): TsgColorCAD;
function SnapModeToInt(const AMode: TObjectSnapState): DWORD;
function IntToSnapMode(const AFlags: DWORD): TObjectSnapState;
function IsXMLHelp(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsFullProps(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function IsFullPropsOrXMLHelp(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function sgIntToXMLModes(const AMode: Integer): TsgXMLModes; {$IFDEF USE_INLINE}inline;{$ENDIF}
function sgXMLModesToInt(const AMode: TsgXMLModes): Integer;
function sgGetXMLFromCommand(const ACommand: string;
  const ARootAttrib: string = ''): string;
function CreatePointsWithBoundary(const APoints: IsgArrayFPoint; const ABoundary: Double): TFPointList;
function MMToText(Value: Extended; Digits: Integer;  AUnitsKf: TsgFloat;
  const AMethod: Byte = 0): string;
function DtoS(D: Double; Digits: Integer = 4): string;
function StrToD(var S: string; var D: Double): LongBool;
function IsSizeUnitsMM(const S: string; var W, H: Double): Boolean;
function CheckPathDelim(const APath: string): string;

{OLE and Image Export support}
function GetCountBlocksForOLEExport(const AWidth, AHeight: Integer;
  const APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat; var Chunk: TPoint): TPoint;
{$IFNDEF SG_NON_WIN_PLATFORM}
function CheckExportOLEAsMetafile(Handle: HENHMETAFILE): Boolean;
{$ENDIF}

{OS function}


{$IFNDEF SGDEL_6}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToBool(const S: string; out Value: Boolean): Boolean;
function TryJulianDateToDateTime(const AValue: Double;
  out ADateTime: TDateTime): Boolean;
function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime;
function ReverseString(const AText: string): string;
function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
{$ENDIF}

function GetApplicationName: string;
{$IFNDEF SG_NON_WIN_PLATFORM}
function GetFileVerFixed(const AFileName: string; var AFI: TVSFixedFileInfo): Boolean;
{$ENDIF}
function GetLanguageId(const AStr: string): TsgLangugeID;
function GetDataFromHttp(const AHost: string;
  AStream: TStream; ATimeOut: Integer; var AFileName: string;
  var AErrorMessage: string; const AProxyInfo: TsgHTTPProxyInfo;
  AUseProxy: Boolean = False; AHandleRedirects: Boolean = True;
  AGetMethod: Boolean = True;
  const AEvents: PsgDataHttpParams = nil): Boolean;
function GetClassEntName(AObject: TObject; PrefixLength: Integer = 6): string;
function GetDeviceCap(const AType: Integer): Integer;
function CodepageToMCodeIndex(ACodePage: Cardinal): Integer;
function GetCodePageByIndex(const AIndex: Integer): Integer;
function CodepageToCharset(ACodePage: Cardinal): TFontCharset;
function GetMultibyteSymbol(const ALeft, ARight: Byte; const ANumber: Integer): WideChar; overload;
function GetLogPixelsPerInch: TPoint;
{$IFDEF SG_FIREMONKEY}
function GetPixelsPerInch: Integer;
{$ENDIF}
function GetScreenMMPerPixel: Double;
function GetScreenResolution: TPoint;
function GetMMtoPixel: TF2DPoint;
function GetScreenDpi: Integer;
{$IFDEF SG_DELPHI_VCL}
function GetMetafileScale(const AMetafile: TMetafile): Double;
function ResizeMetafileByDpi(var AMetafile: TMetafile): Boolean;
function ResizeMetafileByScale(var AMetafile: TMetafile; const AScale: Double): Boolean;
{$ENDIF}
function GetTempDir: string;
function CreateFolder(AName: string): Boolean;
function sgCreateDir(S: string): Boolean;
function sgDirectoryExists(const ADir: string): Boolean;
function IsNetworkDir(const ADir: string): Boolean;
function LCIDToCodePage(const ALCID:{$IFDEF SG_SYSTEM_PROCS}DWORD{$ELSE}SysUtils.TLocaleID{$ENDIF}): Integer;
function ParseUnicode(AStr: sgUnicodeStr; AModes: TParseUnicodeModes): sgUnicodeStr;
function ReplaceAnsi(var AText: string; const AFromText, AToText: string;
  const ACaseSensitive: Boolean = False): Boolean;
procedure PluginsReplaceAnsis(var AStr: string);
function ReplaceChar(var AString: string;
  const ACharOld, ACharNew: Char): string;
procedure sgNOP; {$IFDEF SG_ASSEMBLER}assembler;{$ENDIF} {$IFNDEF _FIXINSIGHT_}{$IFDEF SGDEL_2006} experimental; {$ENDIF}{$ENDIF}
{$IFDEF SGDEL_XE2}
function PointerAdd(APointer: Pointer; Add: Integer): Pointer;
function PointerDec(APointer: Pointer; Dec: Integer): Pointer;
{$ENDIF}
function HasAccessByWrite(ADir: string): Boolean;
function GetFileNameByParam(const ADelimetrString: string;
  const AFullName: string): string;
function GetFileExtByParam(const ADelimetrString: string;
  const AFullName: string): string;
function NormalDir(const DirName: string): string;

{$IFDEF SG_SYSTEM_PROCS}
function GetFileSize(const FileName: string): Int64;
{$ENDIF}

procedure sgOpenURL(const AUrl: string);
procedure sgOpenEMail(const AMail: string);

{BTI specificity}

function GetBTITypeByEntTypeEx(const ATypeEx: Integer): TbtiEntity;
function GetBTITypeByCADEntType(const AType: TsgCADEntityType): TbtiEntity;
function GetEntTypeExByBTIType(const AType: TbtiEntity): Integer;
function GetTypeExByCADEntType(const AType: TsgCADEntityType): Integer;
function GetCADEntTypeByBTIType(const AType: TbtiEntity): TsgCADEntityType;
function GetCADEntTypeByTypeEx(const ATypeEx: Integer): TsgCADEntityType;
function GetBlockShortNameByBTIType(const AType: TbtiEntity): string;
function GetBlockShortNameByEntTypeEx(const AType: Integer): string;
function GetEntTypeExByBlockShortName(const AName: string;
  const AHasAsterisk: Boolean = True): Integer;
function IsBTIBlockNameOld(const AName: string): Boolean;
function IsBTIBlockName(const AName: string;
  const AHasAsterisk: Boolean = True): Boolean;
function IsBTIBlockInternal(const AName: string): Boolean;
function StrToBTITypes(const AStr: string): TbtiEntities;
function BTITypesToStr(const ATypes: TbtiEntities): string;

{compare function}

function IsEqualMethod(AMethod1, AMethod2: TMethod): Boolean;
function IsEqualEvent(AEvent1, AEvent2: TNotifyEvent): Boolean;

function CompareDXFPointByXCoord(AItem1, AItem2: Pointer): Integer;
function CompareDXFPointByYCoord(AItem1, AItem2: Pointer): Integer;
function ProcCompareColorCAD(const Value1, Value2: Pointer): Integer;
function ProcComparePFPoint(const Value1, Value2: Pointer): Integer;
function ProcCompareF2DPoint(const Value1, Value2: Pointer): Integer;
function ProcCompareF2DPointByX(const Value1, Value2: Pointer): Integer;
function ProcCompareF2DPointByY(const Value1, Value2: Pointer): Integer;
function ProcCompareF2DPointByYX(const Value1, Value2: Pointer): Integer;
function ProcCompareFPointByX(const Value1, Value2: Pointer): Integer;
function ProcCompareFPointByY(const Value1, Value2: Pointer): Integer;
function ProcCompareFPoint(const Value1, Value2: TFPoint): Integer;
function ProcCompareDouble(const Value1, Value2: Double): Integer;
function ProcCompareSingle(const Value1, Value2: Single): Integer;

function sgCompareCardinal(const AValue1, AValue2: Cardinal): Integer;
function sgCompareHandles(const AH1, AH2: UInt64): Integer;
function sgCompareNativeUInts(const AValue1, AValue2: sgConsts.TsgNativeUInt): Integer;
function sgCompareObjects(const AValue1, AValue2: TObject): Integer;
function sgComparePointers(const AValue1, AValue2: Pointer): Integer;
function sgCompareStr(const AValue1, AValue2: string): Integer;
function sgCompareTxt(const AValue1, AValue2: string): Integer;

function sgCompareDoubleAcc(const AValue1, AValue2: Double;
  const Epsilon: Double = 0): Integer;{$IFDEF SG_INLINE} inline;{$ENDIF}

function AnsiCmprStr(const AStr1, AStr2: string;
  const ACaseSensitivity: Boolean): Integer;

procedure ApplyRangDouble(var AValue: Double; const AMin, AMax: Double);{$IFDEF SG_INLINE} inline;{$ENDIF}

{$IFNDEF SG_FIREMONKEY}
function GetsgKeyBoardState(const AMsg: TMessage): TShiftState;
{$ENDIF}
function IsInsUnits(const AValue: Integer): Boolean;


{This functions for old versions of an source codee}
(*
function CalcPoint(const APoint: TFPoint; const AMatrix: TFMatrix): TPoint;
function CompareFPointByXCoord(Item1, Item2: Pointer): Integer;
function CompareFPointByYCoord(Item1, Item2: Pointer): Integer;
function Create2dPPointProc(const AX, AY, AZ: TsgFloat): Pointer;
function Create3dPPointProc(const AX, AY, AZ: TsgFloat): Pointer;
function DistanceF(const APoint1, APoint2: TFPoint): Double;
function DistanceI(const APoint1, APoint2: TPoint): Double;
function FloatPoint(const AX, AY: TsgFloat): TFPoint;
function FltPoint3D(const AX, AY, AZ: TsgFloat): TFPoint;
function GetIntersectingPoint(const ALine1, ALine2: TsgLine;
  var AIntersectionMode: TIntersectionMode): TFPoint;
function MatInverse(const AMatrix: TFMatrix): TFMatrix;
function IdentityMat: TFMatrix;
function IsRotated(const AMatrix: TFMatrix): Boolean;
procedure MatOffset(var AMatrix: TFMatrix; const APoint: TFPoint);
function MatXMat(const AMatrix1, AMatrix2: TFMatrix): TFMatrix;
function PtXMat(const APoint: TFPoint; const AMAtrix: TFMatrix): TFPoint;
function ScalarXMat(const AScalar: Double; const AMatrix: TFMatrix): Double; overload;
function ScalarXMatByY(const AScalar: Double; const AMatrix: TFMatrix): Double;
function ScalarXMatByZ(const AScalar: Double; const AMatrix: TFMatrix): Double;
procedure SortList(AList: PPointerList; ACompareFunc: TListSortCompare;
  ALeftPos, ARightPos: Integer);
*)
function ColorToRGBString(const AColor: TColor): string;
function ColorCADToCADString(const AColor: TsgColorCAD; const AByBlock,
  AByLayer, ARed, AYellow, AGreen, AAqua, ABlue, AFuchsia, ABlackWhite,
  APrefix: string): string;
function ConvertARGBToColorCAD(const AColor: TColor;
  const ACadNone: PsgColorCAD = nil): TsgColorCAD;
function ConvertColorCADToIndexColor(const AColor: TsgColorCAD;
  const ASkipByBlock: Boolean; APalett: PsgCADPalett = nil): Cardinal;
function ConvertColorCADToRGB(const AColor: TsgColorCAD; APalett: PsgCADPalett = nil): Cardinal;
function ConvertRGBtoColor(const R, G, B: Byte; const C: Byte = 0): Integer;
function ConvertColortoColorCAD(const AColor: Cardinal; APalett: PsgCADPalett = nil): TsgColorCAD;
procedure ConvertColorToRGB(const AColor: Cardinal; var R, G, B: Byte);
procedure ConvertRGBtoCMYK(const R, G, B: Byte; var C, M, Y, K: Byte);
procedure ConvertRGBtoCMYKFloat(const R, G, B: Byte; var C, M, Y, K: Double);
function ConvertCMYKtoColor(const C, M, Y, K: Integer): Integer;
procedure ConvertCMYKtoRGB(const C, M, Y, K: Integer; var R, G, B: Byte);
function ConvertColortoGray(const AColor: Cardinal; const AMode: Byte = 3): Integer;
function ConvertRGBtoGray(const R, G, B: Byte; const AMode: Byte = 3): Integer;

function IsColorCADByBlock(const AColor: TsgColorCAD): Boolean;
function IsColorCADByLayer(const AColor: TsgColorCAD): Boolean;
function IsColorCADEqual(const AColor1, AColor2: TsgColorCAD;
  APalett: PsgCADPalett = nil): Boolean;

function CreateColorCAD(const AColor: TsgColorCAD): PsgColorCAD;
function MakeColorCAD(const AType: TsgActiveColor;
  const AValue: Cardinal; const AAlbumString: string = ''): TsgColorCAD;
function NormalizeColor(const AColor, AcolorMin, AColorMax: Cardinal): Integer;
function CmEntityColorToColorCAD(ACmEntityColor: Cardinal): TsgColorCAD;
function ColorCADToCmEntityColor(const AColor: TsgColorCAD): Cardinal;
function MakeMessageParams(ACode: Integer; AMessage: string): TsgMessageParams;

{ Export DWG/DXF support }
function ConvertDXFToLineWeight(const ADXFWeight: Integer; var ALineWeight: Double): Boolean;
function ConvertLineWeightToDXF(const ALineWeight: Double): Integer;
function ConvertLineWeightToDWG(const ALineWeight: Double): Integer;
function DimSubclassNum(Flags: Byte): Byte;
function GetCrc32FromStream(const AStream: TStream): Cardinal;
function GetHash64(var P: PByte; Count: TsgNativeUInt; ASeed: UInt64): UInt64;
function IsXRefInName(const AName: string): Boolean;

function GetHexTextValue(const AStr: string): string;
function SetHexTextValue(const AStr: string): string;
{$IFNDEF SG_CADIMPORTERDLLDEMO}
function CodingTextValue(const AStr: string; const AEncode: Boolean): string;
{$ENDIF}

{ Function converts table indexes }

function ClearBrackets(const AStr: string): string;
function ParseTableCellString(const AStr: string; var ACol, ARow: Integer): Integer;
function TableCellStringToCol(const ASym: string; var ARow: string): Integer;
function TableIndexToStringCol(AIndex: Integer): string;

{ Parser functions for property TOpenDialog.Filter }

function BuildDialogFilter(const AFilterList: TStrings): string;
procedure ClearFilterList(const AFilterList: TStrings);
function GetExtsByFilterIndex(const AFilter: string; AFilterIndex: Integer): string;
function GetExtByFilterIndex(const AFilter: string; AFilterIndex: Integer): string;
function GetFilter(const AFilter: string; const AFilterIndex: Integer): string;
function GetFilterFromList(const AFilterList: TStrings; const AFilterIndex: Integer): string;
function GetFilterIndexByExt(const AFilter: string; const AExt: string): Integer;
function GetFilterIndexFromList(const AFilterList: TStrings; const AExt: string): Integer; overload;
function GetFilterIndexFromList(const AFilterList: TStrings; const ADWGVer: TsgDWGVersion): Integer; overload;
procedure ParseDialogFilter(const AFilter: string; const AFilterList: TStrings);

{ Graphics functions }
function CheckMaxSize(const AMaxSize: Double; var AWidth, AHeight: Double): Boolean; overload;
function CheckMaxSize(const AMaxSize: Double; var AWidth, AHeight: Integer): Boolean; overload;
function CreateCopyGraphic(const AGraphic: TGraphic): TGraphic; overload;
function CreateCopyGraphic(const AGraphic: TPersistent): TPersistent; overload;
function CreateCopyGraphicAsBitmap(const AGraphic: TGraphic;
  const AIsGdiObject: Boolean = true): TGraphic;
function GetGraphicPixelFormat(const AGraphic: TGraphic; AIsBitmap: PBoolean = nil):  {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
{$IFNDEF SG_NON_WIN_PLATFORM}
function GetDefaultMetafileSize(const AIsEnhaned: Boolean): Integer;
function GetMaxMetafileSize(const AIsEnhaned: Boolean): Integer;
{$ENDIF}
{$IFNDEF SGFPC}
  {$IFNDEF SG_FIREMONKEY}
function GetMetafileSize(AMF: TMetafile;
  ARatio: Double = cnstMetafileDefRatio): TF2DPoint;
  {$ENDIF}
{$ENDIF}
function GetPixelFormat(const ASize: Integer): {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
function GetPixelSize(const AFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat): Integer;
function GetSizeProportionForExport(const AGraphic: TGraphic;
 const AExportWidth, AExportHeight: Integer): TPoint;
function SetBitmapInCenterForExport(const ABitmap: TBitmap;
  const AExportWidth, AExportHeight: Integer;
  const AQuality: Double = 1): Boolean;
procedure SetSizeGraphic(const AGraphic: TPersistent; const Width, Height: Integer); overload;
procedure SetSizeGraphic(ADst, ASrc: TPersistent); overload;
procedure SetSizeBmp(const ABmp: TBitmap; Width, Height: Integer;
  const AMode: Byte = 0; const AMaxImgSize: Integer = cnstMaxImgSize);
function OpenPictureByMemmoryStream(const AMemStream: TMemoryStream;
  const AFullFileName: string; const AResult: PInteger = nil): TPersistent;
function OpenPictureByMemmoryStreamSafe(const AMemStream: TMemoryStream;
  const AFileName: string; const AFileExt: string;
  const AResult: PInteger = nil): TPersistent;
procedure SetMaxSize(const AMaxSize: Double; var AWidth, AHeight: Double); overload;
procedure SetMaxSize(const AMaxSize: Double; var AWidth, AHeight: Integer); overload;
procedure SetMaxMetafileExtents(AWidth, AHeight: Double;
  var AMetWidth, AMetHeight: Integer; AMaxExtents: Integer = -1);
{$IFNDEF SG_NON_WIN_PLATFORM}
function GraphicClass(AExt: string): TGraphicClass;
{$ENDIF}
procedure ApplyBitmapTransparent(const ABitmap: TBitmap;
  const ATransparentColor: Integer);
procedure SetBitmapTransparent(const ABitmap: TBitmap;
  const ATransparent: Boolean; const ATransparentColor: Integer = clEmpty);
function GetSizeGraphic(const AGraphic: TPersistent): TSize;
procedure DoRotateBitmap(ABitmap: TBitmap; AAngle: Extended; ARotToSource: PFMatrix = nil);
procedure IniScreenParams;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
function MKBIH32(var ABitmapInfoHeader: {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}TBitmapInfoHeader; AWidth, AHeight: Integer): PBitmapInfoHeader;{$IFDEF SG_INLINE} inline;{$ENDIF}
{$ENDIF}
{ TList function}
procedure SetPFPointsListCount(var APoints: TList; const ACount: Integer);


{ List functions}
procedure AddFPointInFPointList(const AList: Pointer; const APoint: TFPoint);
function GetFPointFromFPointList(const AList: Pointer; const AIndex: Integer): TFPoint;
procedure AddItemInSingleList(const AList: Pointer; const AItem: Single);
function GetItemFromSingleList(const AList: Pointer; const AIndex: Integer): Single;

procedure AddNotifyEventToList(const AList: TList; const AEvent: TNotifyEvent);
function GetNotifyEventFromList(const AList: TList; const AIndex: Integer): TNotifyEvent;
function RemoveNotifyEventFromList(const AList: TList; const AEvent: TNotifyEvent): Boolean;
procedure ClearNotifyEventList(const AList: TList);

{ CRC }
{ CRC and Checksum}
function CRC8(P: PByte; Init: Word; N: Cardinal): Word;
function CRC32(const P: PByte; Seed, N: Cardinal): Cardinal;
function Checksum(const P: PByte; Seed, N: Cardinal): Cardinal;

{ DWG }
function CreateDWGHandle(const Handle: UInt64; const Code: Byte): TsgDWGHandle;
function CreateDWGHandleRef(const Handle, RefHandle: UInt64): TsgDWGHandle;

{ Memory }
function StrPosLen(const Str1, Str2: PByte; Str1Len, Str2Len: TsgPrNativeUInt;
  var AFoundAdr: sgConsts.TsgNativeUInt): PByte;
procedure FindAddresses(SourceAdd, FindAdd: PByte; SourceLen, FindLen: TsgNativeUInt;
  AFoundAddresses, AIgnoredAddresses: TList);
procedure sgZeroMemory(Destination: Pointer; Length: TsgNativeUInt); {$IFDEF SG_INLINE}inline;{$ENDIF}

{ Bits }
function BoolToBit(const Value: Boolean): Byte;

{ DateTime }

function sgGetTickCount: Cardinal;
function sgGetTickCount64: UInt64;

procedure DateTimeToJulianDateInt(const Date: TDateTime; var JDDay,
  JDMilSecInDay: Cardinal);
function JulianDateIntToDateTime(const JDDay, JDMilSecInDay: Cardinal;
  DefDate: TDateTime = 0): TDateTime;

function DateTimeToJulianTimeStamp(const ADateTime: TDateTime;
  const AHourOffs: Integer = cntDwgJulianDateHourDif): TTimeStamp;
function JulianTimeStampToDateTime(const AJDTimeStamp: TTimeStamp;
  const ADefaultDateTime: TDateTime = 0;
  const AHourOffs: Integer = cntDwgJulianDateHourDif): TDateTime;

function CreateTimeStamp(const Day: Integer = 0; const Milliseconds: Integer = 0): TTimeStamp;
{$IFNDEF SG_NON_WIN_PLATFORM}
function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;
{$ENDIF}

procedure sgBinToHex(Buffer, Text: Pointer; BufSize: Integer);
function sgHexToBin(Text: PAnsiChar; Buffer: PAnsiChar; BufSize: Integer): Integer;

{ Initialization ByteBinToHex array }
procedure InitByteBinToHex;

{ Print function}

procedure AddPrinterPaperSize(const APaper: TPaperFormat);
function FindPaperIndexBySizes(const APaperFormats: TStrings;
  const APaperSize: TF2DPoint; const AFindRot: Boolean): Integer;
function FindPaperIndex(const APaperFormats: TStrings;
  const APaperName: string; const APaperSize: TF2DPoint;
  const AFindRot: Boolean): Integer;
function GetPrinterActualPaperSize(const ABox: TFRect; var AName:string;
  const AMargins: PF2DRect = nil): TF2DPoint;
function CalcFitScale(AInputRect: TFRect; AMargins: TF2DRect;
  APlotSize: TF2DPoint; AIsLandscape: Boolean; AIsMM: Boolean): TsgPlotScale;
function GetDefaultPrinterPapers: TstringList;
function GetPaperImageOriginPoint(const AIsMM: Boolean;
  const APlotType: TsgPlotType; const APrintRect: TFRect): TF2DPoint;
function GetPaperSizesFromBox(const AIndexFormat: Integer;
  const AFormats: TStrings): TF2DPoint;

{ From MVFont.pas}

function RoundToPowerOf2(Value: Integer): Integer;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
function ImgRect(G: TGraphic): TRect;
procedure NormRect(var Rect: TRect);
procedure AssignToBitmap(Src: TGraphic; BM: TBitmap);
{$IFDEF SG_DELPHI_VCL}
procedure AssignToMetafile(Src: TGraphic; MF: TMetafile);
{$ENDIF}

{$IFDEF SGFPC}
function CopyPalette(Palette: HPALETTE): HPALETTE;
{$ENDIF}


{ Global configuration function }
function GetGlobalConstant(const AName: string): string;
procedure SetGlobalConstant(const AName, AValue: string);

{ LineWeight for Color}
function ColorCADtoColorLW(const AColor: TsgColorCAD): string;
function ColorLWtoColorCAD(const AStr: string): TsgColorCAD;
function ColorLineWeightToString(const AColor: TsgColorCAD; const ALineWeght: Double;
  const AColorNew: Integer): string;
function ColorLineWeightToStr(const AColor, ALineWeght: string;
  const AColorNew: string): string;
function StringToColorLineWeight(const Source: string; var AColor: TsgColorCAD;
  var ALineWeght: TsgFloat; var AColorNew: Integer): Boolean;
function StringToColorLineWeightStr(const Source: string; var AColor: string;
  var ALineWeght: string; var AColorNew: string): Boolean;
procedure CorrectFileCws(const AColors: TStringList);
function LoadFromFileCws(const AFileName: string; const AColors: TStringList): Boolean;
function SaveToFileCws(const AColors: TStringList; const AFileName: string): Boolean;
procedure AssignColorToLineWeights(const ASource, ADest: TStringList);

{$IFNDEF SG_NON_WIN_PLATFORM}
{ RTL functions }
{ function CodeMove:
    CodeSource - protected memory;
    Dest - output buffer;
    Count - output buffer size
    Result - current memory protect flags }
function CodeMove(CodeSource: Pointer; var Dest; Count: Integer): Cardinal;
{$ENDIF}

{ EmptyExceptionHandler - do nothig. for FixInsight empty EXCEPT blocks }
{$IFDEF _FIXINSIGHT_}
function EmptyExceptionHandler(E: Exception): Boolean;
{$ENDIF}

function IsFileOLEStorage(const AFileName: string): Boolean;
function IsStreamOLEStorage(const AStream: TStream): Boolean;

function IsSpace(const AChar: WideChar): Boolean; overload;
function IsSpace(const AChar: AnsiChar): Boolean; overload;

// ASM SAR implementation
function SAR(AValue: Integer; const ACount: Byte): Integer;
function SAR16(AValue: SmallInt; const ACount: Byte): SmallInt;
function SAR8(AValue: Byte; const ACount: Byte): Byte;

function ACIS_Cipher(const AChar: AnsiChar): AnsiChar; overload;
function ACIS_Cipher(const AByte: Byte): Byte; overload;

{ kernel functions }
function OpenThread(dwDesiredAccess: Cardinal; bInheritHandle: LongBool; dwThreadId: Cardinal): THandle; stdcall;

{ platform helper functions }
function NeedFixForWine: Boolean;
function GetVersionWindowsName(AVer: TsgVersionWindows): string;
function GetVersionWindows: TsgVersionWindows;
{$IFNDEF SGDEL_2009}
{ system functions }
{ TMonitor <= Delphi 2007 helper functions
Use GlobalCriticalSection object for Delphi 2007 and early }
function MonitorEnter(const AObject: TObject; Timeout: Cardinal = INFINITE): Boolean;
function MonitorTryEnter(AObject: TObject): Boolean;
procedure MonitorExit(const AObject: TObject);
{$ENDIF}

{$IFNDEF SGDEL_6}
function sgVarIsStr(const V: Variant): Boolean;
function sgVarIsOrdinal(const V: Variant): Boolean;
function sgVarAsInt64(const V: Variant): Int64;
function sgVarAsUInt64(const V: Variant): UInt64;
function sgUInt64AsVar(const V: UInt64): Variant;
{$ENDIF}

{ function getosid

  Result:
      - 'android';
      - 'macos';
      - 'ios';
      - 'windows';
      - 'linux' ('Ubuntu', 'AstraLinux'... or DISTRIB_ID value from /etc/lsb_release).
}
function getosid: string;

type
  TClassArray = array of TClass;

function SetDynamicArray(const AOpenClassArray: array of TClass): TClassArray;

var
  ByteBinToHex: PByteBinToHex = nil;


implementation

uses
  sgComparer
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  ,sgUnicode, sgZip
  {$IFNDEF SG_NON_WIN_PLATFORM}
  ,sgShellAPI
  {$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  {$IFDEF LINUX},dl {$ENDIF}, variants
{$ENDIF}
{$IFDEF  SG_FM_ANDROID}
  ,Androidapi.Helpers, FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes
{$ENDIF}
{$IFDEF SG_FM_IOS}
  ,Macapi.Helpers, iOSapi.Foundation, FMX.Helpers.iOS, IdURI
{$ENDIF}
{$IFDEF SG_FM_MACOS}
  ,Posix.Stdlib
{$ENDIF}
{$IFDEF SG_FM_LINUX}
  ,Posix.Stdlib, FMUX.API
{$ENDIF}
  ;
type
  TOpenThread = function (dwDesiredAccess: Cardinal; bInheritHandle: LongBool; dwThreadId: Cardinal): THandle; stdcall;

  TsgProp = packed record
    Hash: Int64;
    Value: Variant;
  end;

const
  cnstNotPtInLine = [pcLEFT, pcRIGHT];
  cnstPtInLine = [pcBETWEEN, pcORIGIN, pcDESTINATION];
  cnstKnotsCubicSpline: array [0..9] of Double = (0, 0, 0, 0, 1, 1, 1, 1, 0, 0);
  cnstMaxJoinEdgesCnt = 15000;
{$IFNDEF MSWINDOWS}
  SW_SHOWNORMAL = 1;
{$ENDIF}
{$IFNDEF SGDEL_6}
  FuzzFactor = 1000;
  ExtendedResolution = 1E-19 * FuzzFactor;
  DoubleResolution   = 1E-15 * FuzzFactor;
  SingleResolution   = 1E-7 * FuzzFactor;

  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);
{$ENDIF}
  cnstCharHexSize = 4;

{$IFDEF SG_CHKRECURSIVE}
  iItemsRecursive: LongWord = 0;
  cnstMaxItemsRecursive = MAXWORD;
{$ENDIF}
  cnstAlphaCount = 26; // size of english alphabet
var
  LanguageIds: TStrings = nil;
{$IFNDEF SGDEL_7}
  NameValueSeparator: Char = '=';
{$ENDIF}
  PrinterPapers: TStringList = nil;
  iScreenDpi: Integer = -1;
  _OpenThread: TOpenThread = nil;
{$IFDEF SG_OPENING_IN_THEADS}
  GlobalCriticalSection: TCriticalSection = nil;
{$ENDIF}
  PropsCount: Integer;
  Props: array of TsgProp;
  sgOSId: string = '';

{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
  FormatSettings_DoubleSeparator: TFormatSettings = ();
  FormatSettings_Point: TFormatSettings = ();
{$ENDIF}
{$IFDEF SG_INDY_HAS_ANTIFREEZE}
  GlobalAntiFreezeRefcount: Integer = 0;
{$ENDIF}

type
  PString = ^string;
{$IFNDEF SG_FIREMONKEY}
  TClipBoardAccess = class(TClipboard);
{$ENDIF}
  TsgBaseListAccess = class(TsgBaseList);
{$IFDEF SGFPC}
  TPngImage = TPortableNetworkGraphic;
{$ENDIF}
{$IFDEF SG_DELPHI_VCL}
  TJPEGImageAccess = class(TJPEGImage);
{$ENDIF}

  TsgItemLnk = class
  private
    FEdges: TList;
    FPoint: TFPoint;
  protected
    function GetActEdge(const AIndex: Integer): Boolean;
    function GetEdge(const AIndex: Integer): TsgCurveLnk;
    function GetEdgesCount: Integer;
    function GetUsedEdge(const AIndex: Integer): Boolean;
    procedure SetActEdge(const AIndex: Integer; const Value: Boolean);
    procedure SetUsedEdge(const AIndex: Integer; const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEdge(const AEdge: Pointer);
    property Edges[const AIndex: Integer]: TsgCurveLnk read GetEdge;
    property ActEdge[const AIndex: Integer]: Boolean read GetActEdge write SetActEdge;
    property UsedEdge[const AIndex: Integer]: Boolean read GetUsedEdge write SetUsedEdge;
    property EdgesCount: Integer read GetEdgesCount;
    property Point: TFPoint read  FPoint write FPoint;
  end;

  TsgGlobalPropProvider = class(TInterfacedObject, IsgGlobalPropProvider)
  private
    FOwnsProps: Boolean;
    function MakeHash(name: PChar): Int64;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function Find(Name: PChar; var Index: Integer): Boolean; overload;
    function Find(const Item: TsgProp; var Index: Integer): Boolean; overload;
    procedure DeleteItem(Index: Integer);
  protected
    FHash: Integer;
    procedure Clear;
    function TryGetValue(Name: PChar; out Value: Variant): Boolean;
    function GetItem(Name: PChar): Variant;
    function RemoveItem(Name: PChar): Integer;
    procedure SetItem(Name: PChar; const Value: Variant);
  public
    constructor Create(const AInstance: TObject; AOwnsProps: Boolean); overload;
    constructor Create(const AHash: Integer; AOwnsProps: Boolean); overload;
    destructor Destroy; override;
    class function hash_name(name: PChar): Integer;
  end;

{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
function CreateFormatSettings(const ASeparator: Char = cnstDoubleSeparator): TFormatSettings;
begin
  case ASeparator of
    cnstDoubleSeparator:       Result := FormatSettings_DoubleSeparator;
    cnstDecimalSeparatorPoint: Result := FormatSettings_Point;
  else
    Result := {$IFDEF SGFPC}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};//GetLocaleFormatSettings(GetThreadLocale, Result);
    Result.DecimalSeparator := ASeparator;
  end;
end;

function CreateFormatSettingsInternal(AFormatSettings: PFormatSettings;
  const Separator: Char = cnstDoubleSeparator): TFormatSettings;
begin
  if AFormatSettings = nil then
    Result := CreateFormatSettings(Separator)
  else
    Result := AFormatSettings^;
end;
{$ENDIF}

function SetDynamicArray(const AOpenClassArray: array of TClass): TClassArray;
begin
  SetLength(Result, Length(AOpenClassArray));
  if Length(Result) > 0 then
    System.Move(AOpenClassArray[0], Result[0], Length(Result) * SizeOf(TClass));
end;

(*
function CalcPoint(const APoint: TFPoint; const AMatrix: TFMatrix): TPoint;
begin
  Result := FPointXMat2DLongint(APoint, AMatrix);
end;

function CompareFPointByXCoord(Item1, Item2: Pointer): Integer;
begin
  if(PFPoint(Item1)^.X> PFPoint(Item2)^.X) then
    Result := +1
  else if(PFPoint(Item1)^.X = PFPoint(Item2)^.X) then
    Result := 0
  else
    Result := -1;
end;

function CompareFPointByYCoord(Item1, Item2: Pointer): Integer;
begin
  if(PFPoint(Item1)^.Y> PFPoint(Item2)^.Y) then
    Result := +1
  else if(PFPoint(Item1)^.Y = PFPoint(Item2)^.Y) then
    Result := 0
  else
    Result := -1;
end;

function Create2dPPointProc(const AX, AY, AZ: TsgFloat): Pointer;
begin
  Result := CreatePFPoint2DProc(AX, AY, AZ);
end;

function Create3dPPointProc(const AX, AY, AZ: TsgFloat): Pointer;
begin
  Result := CreatePFPointProc(AX, AY, AZ);
end;

function DistanceI(const APoint1, APoint2: TPoint): Double;
begin
  Result := DistancePoint(APoint1, APoint2);
end;

function DistanceF(const APoint1, APoint2: TFPoint): Double;
begin
  Result := DistanceFPoint(APoint1, APoint2);
end;

function FltPoint3D(const AX, AY, AZ: TsgFloat): TFPoint;
begin
  Result := MakeFPoint(AX, AY, AZ);
end;

function FloatPoint(const AX, AY: TsgFloat): TFPoint;
begin
  Result := MakeFPoint(AX, AY, 0);
end;

function MatInverse(const AMatrix: TFMatrix): TFMatrix;
begin
  Result := FMatInverse(AMatrix);
end;

function IdentityMat: TFMatrix;
begin
  Result := cnstIdentityMat;
end;

function IsRotated(const AMatrix: TFMatrix): Boolean;
begin
  Result := IsRotatedFMat(AMatrix);
end;

procedure MatOffset(var AMatrix: TFMatrix; const APoint: TFPoint);
begin
  FMatOffset(AMatrix, APoint)
end;

function MatXMat(const AMatrix1, AMatrix2: TFMatrix): TFMatrix;
begin
  Result := FMatXMat(AMatrix1, AMatrix2);
end;

function PtXMat(const APoint: TFPoint; const AMAtrix: TFMatrix): TFPoint;
begin
  Result := FpointXmat(APoint, AMAtrix);
end;

function ScalarXMat(const AScalar: Double; const AMatrix: TFMatrix): Double;
begin
  Result := ScalarXMat(AScalar, 0, AMatrix);
end;

function ScalarXMatByY(const AScalar: Double; const AMatrix: TFMatrix): Double;
begin
  Result := ScalarXMat(AScalar, 1, AMatrix);
end;

function ScalarXMatByZ(const AScalar: Double; const AMatrix: TFMatrix): Double;
begin
  Result := ScalarXMat(AScalar, 2, AMatrix);
end;

procedure SortList(AList: PPointerList; ACompareFunc: TListSortCompare;
  ALeftPos, ARightPos: Integer);
var
  K, N: Integer;
  PItem, PElem: Pointer;
begin
  repeat
    K := ALeftPos;
    N := ARightPos;
    PItem := AList^[(ALeftPos + ARightPos) shr 1];
    repeat
      while ACompareFunc(AList^[K], PItem) < 0 do
        Inc(K);
      while ACompareFunc(AList^[N], PItem) > 0 do
        Dec(N);
      if K <= N then
      begin
        PElem := AList^[K];
        AList^[K] := AList^[N];
        AList^[N] := PElem;
        Inc(K);
        Dec(N);
      end;
    until (K > N);
    if ALeftPos < N then
      SortList(AList, ACompareFunc, ALeftPos, N);
    ALeftPos := K;
  until (K >= ARightPos);
end;
*)

procedure AddBoundary(const AItems, ABoundaries: TsgObjectList);
var
  J, K, Cnt: Integer;
  vBoundary: TFPointList;
  vItem, vItemNext: TsgItemLnk;
  vCurve: TsgCurveLnk;
  vCurveTmp: TF2DCurveEx;
begin
  if AItems.Count > 0 then
  begin
    vBoundary := TFPointList.Create;
    ABoundaries.Add(vBoundary);
    Cnt := AItems.Count - 1;
    for J := 0 to Cnt do
    begin
      vItem := TsgItemLnk(AItems[J]);
      if J = Cnt then
        vItemNext := TsgItemLnk(AItems[0])
      else
        vItemNext := TsgItemLnk(AItems[J + 1]);
      for K := 0 to vItem.EdgesCount - 1 do
      begin
        if vItem.ActEdge[K] and (not vItem.UsedEdge[K]) then
        begin
          vItem.UsedEdge[K] := True;
          vCurve := vItem.Edges[K];
          if ((vCurve.P1 = Pointer(vItem)) and (vCurve.P2 = Pointer(vItemNext))) or
             ((vCurve.P2 = Pointer(vItem)) and (vCurve.P1 = Pointer(vItemNext))) then
          begin
            if vCurve.Flags and 1 <> 0 then
            begin
              vCurveTmp.Flags := [lpArc];
              vCurveTmp.Arc := vCurve.Arc;
            end
            else
            begin
              vCurveTmp.Flags := [];
              vCurveTmp.Line.Point1.X := vCurve.Line.Point1.X;
              vCurveTmp.Line.Point1.Y := vCurve.Line.Point1.Y;
              vCurveTmp.Line.Point2.X := vCurve.Line.Point2.X;
              vCurveTmp.Line.Point2.Y := vCurve.Line.Point2.Y;
            end;
            if vCurve.P1 = Pointer(vItem) then
              CopyForwardPF2DCurveEx(@vCurveTmp, vBoundary, True)
            else
              CopyBackwardPF2DCurveEx(@vCurveTmp, vBoundary, True);
            Break;
          end;
        end;
      end;
    end;
    for J := 0 to Cnt do
    begin
      vItem := TsgItemLnk(AItems[J]);
      for K := 0 to vItem.EdgesCount - 1 do
         vItem.UsedEdge[K] := False;
    end;
  end;
end;

procedure AddItem(const P: TsgItemLnk; const S, G: TsgObjectList);
var
  I: Integer;
  vCurve: TsgCurveLnk;
begin
{$IFDEF SG_CHKRECURSIVE}
  if iItemsRecursive > cnstMaxItemsRecursive then Exit;
  Inc(iItemsRecursive);
{$ENDIF}
  G.Add(P);
  S.Remove(P);
  for I := 0 to P.EdgesCount - 1 do
  begin
    vCurve := P.Edges[I];
    if G.IndexOf(vCurve.P1) < 0 then
      AddItem(TsgItemLnk(vCurve.P1), S, G);
    if G.IndexOf(vCurve.P2) < 0 then
      AddItem(TsgItemLnk(vCurve.P2), S, G);
  end;
end;

function JoinItem(const P: TsgItemLnk; const G, ABoundaries: TsgObjectList): Boolean;
var
  I, vIndex: Integer;
  vCurve: TsgCurveLnk;
begin
  Result := False;
{$IFDEF SG_CHKRECURSIVE}
  if iItemsRecursive > cnstMaxItemsRecursive then Exit;
  Inc(iItemsRecursive);
{$ENDIF}
  vIndex := G.Count;
  G.Add(P);
  I := 0;
  while I < P.EdgesCount do
  begin
    if not P.ActEdge[I] then
    begin
      vCurve := P.Edges[I];
      if (G.Count > 1) and ((vCurve.P1 = G[0]) or (vCurve.P2 = G[0])) then
      begin
        Result := True;
        P.ActEdge[I] := True;
        AddBoundary(G, ABoundaries);
        P.ActEdge[I] := False;
      end
      else
      begin
        P.ActEdge[I] := True;
        if vCurve.P1 = Pointer(P) then
        begin
          if JoinItem(TsgItemLnk(vCurve.P2), G, ABoundaries) then
             Result := True;
        end
        else
        begin
          if JoinItem(TsgItemLnk(vCurve.P1), G, ABoundaries) then
            Result := True;
        end;
        P.ActEdge[I] := False;
      end;
    end;
    Inc(I);
  end;
  G.Delete(vIndex);
end;

{TsgItemLnk}

{protected}

function TsgItemLnk.GetActEdge(const AIndex: Integer): Boolean;
begin
  Result := PsgCurveLnk(FEdges[AIndex])^.Flags and 128 <> 0;
end;

function TsgItemLnk.GetEdge(const AIndex: Integer): TsgCurveLnk;
begin
  Result := PsgCurveLnk(FEdges[AIndex])^
end;

function TsgItemLnk. GetEdgesCount: Integer;
begin
  Result := FEdges.Count;
end;

function TsgItemLnk.GetUsedEdge(const AIndex: Integer): Boolean;
begin
  Result := PsgCurveLnk(FEdges[AIndex])^.Flags and 64 <> 0;
end;

procedure TsgItemLnk.SetActEdge(const AIndex: Integer; const Value: Boolean);
var
  vCurve: PsgCurveLnk;
begin
  vCurve := FEdges[AIndex];
  vCurve^.Flags := (vCurve^.Flags and 127) or (Byte(Value) shl 7);
end;

procedure TsgItemLnk.SetUsedEdge(const AIndex: Integer; const Value: Boolean);
var
  vCurve: PsgCurveLnk;
begin
  vCurve := FEdges[AIndex];
  vCurve^.Flags := (vCurve^.Flags and 191) or (Byte(Value) shl 6);
end;

{public}

constructor TsgItemLnk.Create;
begin
  inherited Create;
  FEdges := TList.Create
end;

destructor TsgItemLnk.Destroy;
begin
  FEdges.Free;
  inherited Destroy;
end;

procedure TsgItemLnk.AddEdge(const AEdge: Pointer);
begin
  FEdges.Add(AEdge);
end;

{ TsgClipboard }

procedure TsgClipboardBase.Assign(Source: TPersistent);
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    FClip.Assign(Source);
  finally
    RestoreAppHandle;
  end;
{$ENDIF}
end;

procedure TsgClipboardBase.AssignTo(Target: TPersistent);
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    Target.Assign(FClip);
  finally
    RestoreAppHandle;
  end;
{$ENDIF}
end;

procedure TsgClipboardBase.Clear;
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    FClip.Clear;
  finally
    RestoreAppHandle;
  end;
{$ENDIF}
end;

procedure TsgClipboardBase.Close;
begin
{$IFNDEF SG_FIREMONKEY}
  FClip.Close;
{$ENDIF}
end;

constructor TsgClipboardBase.Create;
begin
  inherited Create;
{$IFNDEF SG_FIREMONKEY}
  FClip := Clipboard;
{$ENDIF}
end;

destructor TsgClipboardBase.Destroy;
begin
{$IFNDEF SG_FIREMONKEY}
  FClip := nil;
{$ENDIF}
  inherited;
end;

function TsgClipboardBase.GetAsHandle(Format: Word): THandle;
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    Result := {$IFNDEF SGFPC}FClip.GetAsHandle(Format){$ELSE}0{$ENDIF};
  finally
    RestoreAppHandle;
  end;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TsgClipboardBase.GetBufferText(const AConvertToDXFUnicode: Boolean): string;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vData: THandle;
  vPWideChar: PWideChar;
  vPAnsiChar: PAnsiChar;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  Result := '';
  Open;
  try
    if HasFormat(CF_UNICODETEXT) then
    begin
      vData := GetAsHandle(CF_UNICODETEXT);
      vPWideChar := GlobalLock(vData);
      try
{$IFDEF UNICODE}
        Result := string(vPWideChar);
{$ELSE}
        if AConvertToDXFUnicode then
          Result := sgUnicodeToDXFUnicode(vPWideChar, True)
        else
          Result := string(vPWideChar);
{$ENDIF}
      finally
        GlobalUnlock(vData);
      end;
    end
    else
    begin
      vData := GetAsHandle(CF_TEXT);
      vPAnsiChar := GlobalLock(vData);
      try
        Result := string(AnsiString(vPAnsiChar));
      finally
        GlobalUnlock(vData);
      end;
    end;
  finally
    Close;
  end;
{$ELSE}
{$IFNDEF SG_FIREMONKEY}
  if AConvertToDXFUnicode then
    Result := sgUnicodeToDXFUnicode(FClip.AsText, True)
  else
    Result := FClip.AsText;
{$ELSE}
  Result := '';
{$ENDIF}
{$ENDIF}
end;

function TsgClipboardBase.GetClipBordFormat: TsgClipBordFormat;
begin
  Result := cfUndefined;
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    if FClip.HasFormat(CF_CADIMG) then
      Result := cfCadImage
    else
      if FClip.HasFormat(CF_TEXT) then
        Result := cfText
      else
        if FClip.HasFormat(CF_BITMAP) then
          Result := cfBitmap
        else
{$IFNDEF SG_NON_WIN_PLATFORM}
          if FClip.HasFormat(CF_ENHMETAFILE) then
            Result := cfMetafile
          else
{$ENDIF}
            if FClip.HasFormat(CF_PICTURE) then
              Result := cfPicture;
  finally
    RestoreAppHandle;
  end;
{$ENDIF}
end;

function TsgClipboardBase.HasFormat(Format: Word): Boolean;
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    Result := FClip.HasFormat(Format);
  finally
    RestoreAppHandle;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgClipboardBase.Open;
begin
{$IFNDEF SG_FIREMONKEY}
  SaveAppHandle;
  try
    FClip.Open;
  finally
    RestoreAppHandle;
  end;
{$ENDIF}
end;

procedure TsgClipboardBase.RestoreAppHandle;
begin
end;

procedure TsgClipboardBase.SaveAppHandle;
begin
end;

function TsgClipboardBase.GetRefCount: Integer;
begin
{$IFNDEF SG_FIREMONKEY}
  {$IFNDEF SGDEL_7}
    Result := 0;
  {$ELSE}
  {$IFNDEF SGFPC}
     Result := TClipBoardAccess(FClip).OpenRefCount;
  {$ELSE}
     Result := 0;
  {$ENDIF}
  {$ENDIF}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

  {TsgLock32}

function TsgLock.GetLock(const AIndex: Integer): Boolean;
begin
  Result := (FLocks and (1 shl AIndex)) <> 0;
end;

function TsgLock.GetLockAll: Boolean;
begin
  Result := FLocks <> 0;
end;

procedure TsgLock.SetLock(const AIndex: Integer; const AVal: Boolean);
begin
  FLocks := (FLocks and ($FFFFFFFF - (1 shl AIndex)) or (LongWord(AVal) shl AIndex));
end;

procedure TsgLock.SetLockAll(const AVal: Boolean);
begin
  FLocks := LongWord(AVAl);
end;

{TsgGraph}

{public}

constructor TsgGraph.Create;
begin
  inherited Create;
  FVertexes := TList.Create;
  FEdges := TList.Create;
end;

destructor TsgGraph.Destroy;
begin
  FreeAndNil(FVertexes);
  FreeAndNil(FEdges);
  inherited Destroy;
end;

{TsgContourGraph}

{private}

function TsgContourGraph.GetEdge(AIndex: Integer): TsgEdgeEntity;
begin
  Result := TsgEdgeEntity(FEdges[AIndex]);
end;

function TsgContourGraph.GetVertexes(AIndex: Integer): TsgVertexPoint;
begin
  Result := TsgVertexPoint(FVertexes[AIndex]);
end;

procedure TsgContourGraph.InitVertex(const AEdge: TsgEdgeEntity;
  const AVertexNum: Integer);
var
  vVertex: TsgVertexPoint;
begin
  if AEdge.Vertexes[AVertexNum] <> nil then
    Exit;
  vVertex := TsgVertexPoint.Create;
  FVertexes.Add(vVertex);
  vVertex.Point := AEdge.VetrexPoints[AVertexNum];
  vVertex.AddEdge(AEdge, AVertexNum);
end;

function TsgContourGraph.HookPoint(const AEdge: TsgEdgeEntity;
  const AVertexNum: Integer): Boolean;
var
  I: Integer;
  vPoint: TFPoint;
  vVertex: TsgVertexPoint;
begin
  Result := False;
  vPoint := AEdge.VetrexPoints[AVertexNum];
  for I := 0 to FVertexes.Count - 1 do
  begin
    vVertex := TsgVertexPoint(FVertexes[I]);
    Result := DistanceFPoint(vVertex.Point, vPoint) < FEpsilon;
    if Result then
    begin
      vVertex.AddEdge(AEdge, AVertexNum);
      if AEdge is TsgEdgeLine then
      begin
        case AVertexNum of
          1: TsgEdgeLine(AEdge).FLine.Point1 := vVertex.Point;
          2: TsgEdgeLine(AEdge).FLine.Point2 := vVertex.Point;
        end;
      end;
      Break;
    end;
  end;
end;

{protected}

procedure TsgContourGraph.AddEdgeList(const AEdges: TList;const  APos: Integer);
var
  I: Integer;
begin
  for I := 0 to AEdges.Count - 1 do
    HookEdge(TsgEdgeEntity(AEdges[I]));
  AssignListToPos(AEdges, FEdges, APos);
end;

procedure TsgContourGraph.CreateEdgesByType(const AEdgeType: TsgEdgeType;
  const ASource, AEdges: TList);
begin
  case AEdgeType of
    eetLine: CreateEdgesFromLines(ASource, AEdges);
    eetArc: CreateEdgesFromArcs(ASource, AEdges);
  end;
end;

procedure TsgContourGraph.CreateEdgesFromLines(const ALines, AEdges: TList);
var
  I: Integer;
  vLine: PsgLine;
  vEdge: TsgEdgeLine;
begin
  for I := 0 to ALines.Count - 1 do
  begin
    vLine := ALines[I];
    vEdge := CreateEdgeLine(vLine^);
    AEdges.Add(vEdge);
  end;
end;

procedure TsgContourGraph.CreateEdgesFromArcs(const AArcs, AEdges: TList);
var
  I: Integer;
  vArc: PsgArcR;
  vEdge: TsgEdgeArc;
begin
  for I := 0 to AArcs.Count - 1 do
  begin
    vArc := AArcs[I];
    vEdge := CreateEdgeArc(vArc^);
    AEdges.Add(vEdge);
  end;
end;

procedure TsgContourGraph.CreateEdgesFromCurves(const ACurves: TList;
  const AEdges: TsgObjectList);
var
  I: Integer;
  vCurve: PsgCurveLnk;
  vEdge: TsgEdgeEntity;
begin
  for I := 0 to ACurves.Count - 1 do
  begin
    vCurve := ACurves[I];
    case vCurve^.Flags of
      0:  vEdge := TsgEdgeEntity(CreateEdgeLine(vCurve^.Line));
      1:  vEdge := TsgEdgeEntity(CreateEdgeArc(vCurve^.Arc));
    else
      vEdge := nil;
    end;
    if vEdge <> nil then
      AEdges.Add(vEdge);
  end;
end;

procedure TsgContourGraph.DeleteEdge(const AEdge: TsgEdgeEntity);
begin
  FEdges.Remove(AEdge);
  if AEdge.Vertexes[1] <> nil then
    AEdge.Vertexes[1].DeleteEdge(AEdge);
  if AEdge.Vertexes[2] <> nil then
    AEdge.Vertexes[2].DeleteEdge(AEdge);
end;

procedure TsgContourGraph.HookEdge(const AEdge: TsgEdgeEntity);
begin
  AEdge.Vertexes[1] := nil;
  AEdge.Vertexes[2] := nil;
  if not HookPoint(AEdge, 1) then
    InitVertex(AEdge, 1);
  if not HookPoint(AEdge, 2) then
    InitVertex(AEdge, 2);
end;

procedure TsgContourGraph.RemoveMultiplicityEdges;
var
  I, J: Integer;
  vEdge, vCheckEdge: TsgEdgeEntity;
  vDelList: TList;
begin
  vDelList := TList.Create;
  try
    for I := 0 to FEdges.Count - 1 do
    begin
      vCheckEdge := Edges[I];
      for J := I + 1 to FEdges.Count - 1 do
      begin
        vEdge := Edges[J];
        if (vEdge.HasVertex(vCheckEdge.Vertexes[1]) >= 0) and
          (vEdge.HasVertex(vCheckEdge.Vertexes[2]) >= 0) and
          vEdge.IsIdentical(vCheckEdge) then
        begin
          vDelList.Add(vCheckEdge);
          Break;
        end;
      end;
    end;
    for I := 0 to vDelList.Count - 1 do
    begin
      vEdge := TsgEdgeEntity(vDelList[I]);
      DeleteEdge(vEdge);
      FreeAndNil(vEdge);
    end;
  finally
    FreeAndNil(vDelList);
  end;
end;

function TsgContourGraph.CrossEdges(const AEdge1, AEdge2: TsgEdgeEntity;
  const AList1, AList2: TList): Boolean;
var
  vList1, vList2: TList;
  vCrossPts: TList;
begin
  Result := False;
  vCrossPts := TList.Create;
  try
    case AEdge2.EdgeType of
      eetLine: Result := AEdge1.IsCrossLine(TsgEdgeLine(AEdge2).Line, vCrossPts);
      eetArc:  Result := AEdge1.IsCrossArc(TsgEdgeArc(AEdge2).Arc, vCrossPts);
    end;
    if not Result then
      Exit;
    vList1 := TList.Create;
    vList2 := TList.Create;
    try
      AEdge1.SplitEdge(vCrossPts, vList1, FEpsilon);
      AEdge2.SplitEdge(vCrossPts, vList2, FEpsilon);
      CreateEdgesByType(AEdge1.EdgeType, vList1, AList1);
      CreateEdgesByType(AEdge2.EdgeType, vList2, AList2);
      Result := (AList1.Count > 0) or (AList2.Count > 0);
    finally
      FreeRecordList(vList1);
      FreeRecordList(vList2);
    end;
  finally
    FreeRecordList(vCrossPts);
  end;
end;

function TsgContourGraph.CreateEdgeLine(const ALine: TsgLine): TsgEdgeLine;
begin
  Result := TsgEdgeLine.Create;
  Result.Line := ALine;
end;

function TsgContourGraph.CreateEdgeArc(const AArc: TsgArcR): TsgEdgeArc;
begin
  Result := TsgEdgeArc.Create;
  Result.Arc := NormalizeArcAngels(AArc);
end;

{public}

constructor TsgContourGraph.Create;
begin
  inherited Create;
  FEpsilon := fDoubleResolution;
  FUserPoint := cnstFPointZero;
end;

destructor TsgContourGraph.Destroy;
begin
  FreeList(FVertexes);
  FreeList(FEdges);
  inherited Destroy;
end;

function TsgContourGraph.ProcessContour(const AList: TsgObjectList): Boolean;
var
  I: Integer;
  pPt: PFPoint;
  vLine: TsgLine;
  vStartEdge: TsgEdgeLine;
  vDist, vDistMin: TsgFloat;
  vCross, vLines, vEdges: TList;
  vEdgeSplited, vCanJoin: Boolean;
  vMinDistEdge, vEdgeFirst, vEdgeLast: TsgEdgeEntity;
begin
  Result := False;
  if (FEdges.Count = 0) or (FVertexes.Count = 0) or (AList = nil) then
    Exit;
  AList.Clear;
  if (FEdges.Count = 1) and (TsgEdgeEntity(FEdges[0]).IsLoop) then
  begin // can't run process contour for 1 loop edge
    AList.Add(FEdges[0]);
    Exit;
  end;
  vLine.Point1 := UserPoint;
  pPt := @vLine.Point2;
  pPt^ := Vertexes[0].Point;
  vCross := TList.Create;
  vEdges := TList.Create;
  vLines := TList.Create;
  try
    vMinDistEdge := nil;
    vDistMin := MaxDouble;
    for I := 0 to FEdges.Count - 1 do
    begin
      ClearRecordList(vCross);
      if (not Edges[I].IsCrossLine(vLine, vCross)) or (vCross.Count = 0) then
        Continue;
      vDist := GetDistanceOfNearestPointByPointList(vCross, vLine.Point1, pPt^);
      if (vDist < vDistMin) or (vMinDistEdge = nil) then
      begin
        vMinDistEdge := Edges[I];
        vDistMin := vDist;
      end;
    end;
    ClearRecordList(vCross);
    vCross.Add(pPt);
    vMinDistEdge.SplitEdge(vCross, vLines, FEpsilon);
    vEdgeSplited := vLines.Count > 0;
    if vEdgeSplited then
    begin
      CreateEdgesByType(vMinDistEdge.EdgeType, vLines, vEdges);
      DeleteEdge(vMinDistEdge);
      AddEdgeList(vEdges, -1);
    end;
      // start bypass
    vStartEdge := TsgEdgeLine.Create;
    try
      vStartEdge.Line := vLine;
      HookEdge(vStartEdge);
      vStartEdge.CalcBypass(1, TsgVertexPoint(vStartEdge.Vertexes[1]));
      try
        Result := vStartEdge.BypassJoin(AList);
      except
        on E: EJoinInfLoop do
        begin
          Result := False;
          AList.Clear;
          Exit;
        end;
      end;
      if Result and (AList.Count > 0) then
        AList.Delete(0);
    finally
      DeleteEdge(vStartEdge);
      FreeAndNil(vStartEdge);
    end;
      // join splitted edges
    vCanJoin := vEdgeSplited and (vEdges.Count > 0) and (AList.Count > 2);
    if vCanJoin then
    begin
      vEdgeFirst := TsgEdgeEntity(AList[0]);
      vEdgeLast := TsgEdgeEntity(AList[AList.Count - 1]);
      vCanJoin := vCanJoin and (vEdgeFirst.EdgeType = vEdgeLast.EdgeType) and
        (AList.IndexOf(vEdges[0]) >= 0) and (AList.IndexOf(vEdges[1]) >= 0);
    end;
    if vCanJoin then
    begin
      vEdges.Clear;
      vEdges.Add(vMinDistEdge);
      AddEdgeList(vEdges, -1);
      vMinDistEdge.CalcBypass(vEdgeLast.BypassSign, vEdgeLast.Vertex1);
      AList.Delete(0);
      AList.Delete(AList.Count - 1);
      DeleteEdge(vEdgeFirst);
      DeleteEdge(vEdgeLast);
      FreeAndNil(vEdgeFirst);
      FreeAndNil(vEdgeLast);
      AList.Insert(0, vMinDistEdge);
    end
    else
      if vEdgeSplited then
        FreeAndNil(vMinDistEdge);
  finally
    FreeRecordList(vLines);
    FreeAndNil(vCross);
    FreeAndNil(vEdges);
  end;
end;

procedure TsgContourGraph.InsertEdge(const AEdge: TsgEdgeEntity);
var
  I, J: Integer;
  vCurEdges, vCrossListReady, vCrossListCur: TList;
  vReadyEdge, vCurEdge: TsgEdgeEntity;
begin
  vCurEdges := TList.Create;
  vCrossListReady := TList.Create;
  vCrossListCur := TList.Create;
  vCurEdges.Add(AEdge);
  try
    I := 0;
    while I < FEdges.Count do
    begin
      vReadyEdge := TsgEdgeEntity(FEdges[I]);
      J := 0;
      while J < vCurEdges.Count do
      begin
        vCurEdge := TsgEdgeEntity(vCurEdges[J]);
        CrossEdges(vReadyEdge, vCurEdge, vCrossListReady, vCrossListCur);
        if vCrossListReady.Count > 0 then
        begin
          DeleteEdge(vReadyEdge);
          FreeAndNil(vReadyEdge);
          AddEdgeList(vCrossListReady, I);
          vReadyEdge := TsgEdgeEntity(FEdges[I]);
          vCrossListReady.Clear;
        end;
        if vCrossListCur.Count > 0 then
        begin
          vCurEdges.Delete(J);
          FreeAndNil(vCurEdge);
          AssignListToPos(vCrossListCur, vCurEdges, J);
          Inc(J, vCrossListCur.Count - 1);
          vCrossListCur.Clear;
        end;
        Inc(J)
      end;
      Inc(I);
    end;
    AddEdgeList(vCurEdges, -1);
  finally
    vCurEdges.Free;
    vCrossListReady.Free;
    vCrossListCur.Free;
  end;
end;

procedure TsgContourGraph.ImportCurverList(const ACurves: TList);
var
  I: Integer;
  vEdges: TsgObjectList;
begin
  vEdges := TsgObjectList.Create;
  try
    CreateEdgesFromCurves(ACurves, vEdges);
    for I := 0 to vEdges.Count - 1 do
      InsertEdge(TsgEdgeEntity(vEdges[I]));
    RemoveMultiplicityEdges;
  finally
    vEdges.Free;
  end;
end;

function IsClass(const AClass, AClassParent: TClass): Boolean;
begin
  if AClass = AClassParent then
    Result := True
  else
    if AClass <> nil then
      Result := IsClass(AClass.ClassParent, AClassParent)
    else
      Result := False;
end;

//function SingleToPointer(const AValue: Single): Pointer;
//begin
//  Result := Pointer(AValue);
//end;

function CreateTsgList: TsgList;
begin
  Result := TsgList.Create;
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;
end;

function AddAnsiStringToArray(const S: AnsiString; var A: TAnsiStringArray; var Count: Integer): Integer;
begin
  Result := Count;
  if Count = Length(A) then
    SetLength(A, ListGrow(Length(A)));
  A[Result] := S;
  Inc(Count);
end;

function AbsFPoint(const AP: TFPoint): TFPoint;
begin
  Result.X := Abs(AP.X);
  Result.Y := Abs(AP.Y);
  Result.Z := Abs(AP.Z);
end;

function AbsF2DPoint(const AP: TF2DPoint): TF2DPoint;
begin
  Result.X := Abs(AP.X);
  Result.Y := Abs(AP.Y);
end;

function AbsFPoint2D(const AP: TFPoint): TFPoint;
begin
  Result.X := Abs(AP.X);
  Result.Y := Abs(AP.Y);
  Result.Z := AP.Z;
end;

function AddPoint(const AP1, AP2: TPoint): TPoint;
begin
  Result.X := AP1.X + AP2.X;
  Result.Y := AP1.Y + AP2.Y;
end;

function AddPoint(const AP1, AP2: TPointF): TPointF;
begin
  Result.X := AP1.X + AP2.X;
  Result.Y := AP1.Y + AP2.Y;
end;

function MiddlePoint(const AP1, AP2: TPoint): TPoint;
begin
  Result.X := Round((AP1.X + AP2.X) / 2);
  Result.Y := Round((AP1.Y + AP2.Y) / 2);
end;

function MiddlePoint(const AP1, AP2: TPointF): TPointF;
begin
  Result.X := (AP1.X + AP2.X) * 0.5;
  Result.Y := (AP1.Y + AP2.Y) * 0.5;
end;

function MiddleFPoint2D(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := (AP1.X + AP2.X) * 0.5;
  Result.Y := (AP1.Y + AP2.Y) * 0.5;
  Result.Z := 0;
end;

function AddFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := AP1.X + AP2.X;
  Result.Y := AP1.Y + AP2.Y;
  Result.Z := AP1.Z + AP2.Z;
end;

function AddFPoint2D(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := (AP1.X + AP2.X);
  Result.Y := (AP1.Y + AP2.Y);
  Result.Z := 0;
end;

function AddF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
begin
  Result.X := (AP1.X + AP2.X);
  Result.Y := (AP1.Y + AP2.Y);
end;

function DenomFPoint2D(const AP1, AP2: TFPoint): Extended;
begin
  Result := DenomVectors(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

function MiddleFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := (AP1.X + AP2.X) * 0.5;
  Result.Y := (AP1.Y + AP2.Y) * 0.5;
  Result.Z := (AP1.Z + AP2.Z) * 0.5;
end;

function MiddleF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
begin
  Result.X := (AP1.X + AP2.X) * 0.5;
  Result.Y := (AP1.Y + AP2.Y) * 0.5;
end;

function MinorFPoint(const AP, APB: TFPoint; const AMode: Byte): TFPoint;
begin
  if AMode < 3 then
  begin
    Result := AP;
    Result.V[AMode] := APB.V[AMode] * 2 - Result.V[AMode];
  end
  else
  begin
    Result.X := APB.X * 2 - AP.X;
    Result.Y := APB.Y * 2 - AP.Y;
    Result.Z := APB.Z * 2 - AP.Z;;
  end;
end;

function MirrorFPoint(const APoint, ACenter: TFPoint): TFPoint;
begin
  Result.X := ACenter.X - APoint.X + ACenter.X;
  Result.Y := ACenter.Y - APoint.Y + ACenter.Y;
  Result.Z := ACenter.Z - APoint.Z + ACenter.Z;
end;

function MultiplyFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := AP1.X * AP2.X;
  Result.Y := AP1.Y * AP2.Y;
  Result.Z := AP1.Z * AP2.Z;
end;

function MultiplyFPointParam(const AP: TFPoint; const AM: Double): TFPoint;
begin
  Result.X := AM * AP.X;
  Result.Y := AM * AP.Y;
  Result.Z := AM * AP.Z;
end;

function SignFPoint(const APoint: TFPoint): TFPoint;
begin
  Result.X := sgSign(APoint.X);
  Result.Y := sgSign(APoint.Y);
  Result.Z := sgSign(APoint.Z);
end;

function NormalizeArcAngels(const AArc: TsgArcR): TsgArcR;
begin
  Result := AArc;
  Result.AngleS := sgMod(Result.AngleS, 360);
  if Result.AngleE > 360 then // (AngleE = 360) - correct arc
    Result.AngleE := sgMod(Result.AngleE, 360);
end;

function F2DPointTo3D(const AP: TF2DPoint): TFPoint;
begin
  Result.X := AP.X;
  Result.Y := AP.Y;
  Result.Z := 0;
end;

function F2DLineTo3D(const AL: TF2DLine): TsgLine;
begin
  Result.Point1.X := AL.Point1.X;
  Result.Point1.Y := AL.Point1.Y;
  Result.Point1.Z := 0;
  Result.Point2.X := AL.Point2.X;
  Result.Point2.Y := AL.Point2.Y;
  Result.Point2.Z := 0;
end;

function F3DPointTo2D(const AP: TFPoint): TF2DPoint;
begin
  Result.X := AP.X;
  Result.Y := AP.Y;
end;

function F3DLineTo2D(const AL: TsgLine): TF2DLine;
begin
  Result.Point1.X := AL.Point1.X;
  Result.Point1.Y := AL.Point1.Y;
  Result.Point2.X := AL.Point2.X;
  Result.Point2.Y := AL.Point2.Y;
end;

function DivFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  if not sgIsZero(AP2.X) then
    Result.X := AP1.X / AP2.X
  else
    Result.X :=  AP1.X * fDivDoubleResolution;
  if not sgIsZero(AP2.Y) then
    Result.Y := AP1.Y / AP2.Y
  else
    Result.Y := AP1.Y * fDivDoubleResolution;
  if not sgIsZero(AP2.Z) then
    Result.Z := AP1.Z / AP2.Z
  else
    Result.Z := AP1.Z * fDivDoubleResolution;
end;

function GetLengthArc(const ARadius, AngleStart, AngleEnd: Double): Double;
begin
   Result := ARadius * Abs(AngleStart - AngleEnd) * fPiDividedBy180;
end;

function GetLengthArcR(const Arc: TsgArcR): Double;
begin
  Result := GetLengthArc(Arc.Radius, Arc.AngleS, Arc.AngleE);
end;

function GetLengthArcByPoints(const APCenter, APStart, APEnd: TFPoint): Double;
begin
  Result := (GetAngleByPoints(APCenter, APEnd, True) - GetAngleByPoints(APCenter, APStart, True)) *
    DistanceFPoint2D(APCenter, APStart);
end;

function GetLengthCircle(const ARadius: Double): Double;
begin
  Result := 2 * cnstPi * ARadius;
end;

function GetLengthLine(const ALine: TsgLine): Double;
begin
  Result := DistanceFPoint(ALine.Point1, ALine.Point2);
end;

function GetLengthVector(const V: TFPoint; AResolution: Double = fDoubleResolution): Double;
begin
  Result := Sqr(V.X) + Sqr(V.Y) + Sqr(V.Z);
  if Result > AResolution then
    Result := Sqrt(Result)
  else
    Result := 0;
end;

function Ort(const P: TFPoint): TFPoint;
var
  L: Double;
begin
  L := Sqrt(Sqr(P.X) + Sqr(P.Y) + Sqr(P.Z));
  if L = 0 then L := 1;
  Result.X := P.X / L;
  Result.Y := P.Y / L;
  Result.Z := P.Z / L;
end;

{$IFNDEF SG_NO_EXTENDED}
function NormalizeVector(var A: array of Extended): Extended; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(A) to High(A) do
    Result := Result + Sqr(A[I]);
  if Result > 0 then
  begin
    Result := Sqrt(Result);
    for I := Low(A) to High(A) do
      A[I] := A[I] / Result;
  end;
end;
{$ENDIF}

function NormalizeVector(var A: array of Double): Extended; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(A) to High(A) do
    Result := Result + Sqr(A[I]);
  if Result > 0 then
  begin
    Result := Sqrt(Result);
    for I := Low(A) to High(A) do
      A[I] := A[I] / Result;
  end;
end;

function NormalizeVector(var A: array of Single): Extended; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(A) to High(A) do
    Result := Result + Sqr(A[I]);
  if Result > 0 then
  begin
    Result := Sqrt(Result);
    for I := Low(A) to High(A) do
      A[I] := A[I] / Result;
  end;
end;

function NormalizeVectorFPoint(var APoint: TFPoint): Extended;
begin
{$IFDEF SGDEL_7}
  Result := NormalizeVector(APoint.V);
{$ELSE}
  Result := Sqr(APoint.X) + Sqr(APoint.Y) + Sqr(APoint.Z);
  if Result > 0 then
  begin
    Result := Sqrt(Result);
    if Result <> 0 then
    begin
      APoint.X := APoint.X / Result;
      APoint.Y := APoint.Y / Result;
      APoint.Z := APoint.Z / Result;
    end;
  end;
{$ENDIF}
end;

function Vector(const P1, P2: TFPoint): TFPoint;
begin
  Result.X := P1.Y * P2.Z - P1.Z * P2.Y;
  Result.Y := P1.Z * P2.X - P1.X * P2.Z;
  Result.Z := P1.X * P2.Y - P1.Y * P2.X;
end;

function Reverse(const P: TFPoint): TFPoint;
begin
  Result := MakeFPoint(-P.X, -P.Y, -P.Z);
end;

function ReverseScale(const AScale: TFPoint): TFPoint;
begin
  if (AScale.X <> 0) and (AScale.Y <> 0) and (AScale.Z <> 0) then
    Result := MakeFPoint(1/AScale.X, 1/AScale.Y, 1/AScale.Z)
  else
    Result := cnstFPointSingle;
end;

function FPointTrimExtents(const APoint: TFPoint): TFPoint;
begin
  Result := APoint;
  CheckValueExtents(Result.X);
  CheckValueExtents(Result.Y);
  CheckValueExtents(Result.Z);
end;

function ExtractBoxParams(const ABox: TFRect; var ACenter, ASizeDir: TFPoint): Extended;
begin
  ACenter := GetCenterOfRect(ABox);
  ASizeDir.X := ACenter.X - ABox.Left;
  ASizeDir.Y := ABox.Top - ACenter.Y;
  ASizeDir.Z := ACenter.Z - ABox.Z1;
  Result := NormalizeVectorFPoint(ASizeDir);
end;

function MakeFRectByParams(const ACenter, ASizeDir: TFPoint; const ARadius: Extended): TFRect;
var
  vHSize: TFPoint;
begin
  vHSize := PtXScalar(ASizeDir, ARadius);
  Result.Left := ACenter.X - vHSize.X;
  Result.Top := ACenter.Y + vHSize.Y;
  Result.Z1 := ACenter.Z - vHSize.Z;

  Result.Right := Result.Left + ASizeDir.X * ARadius;
  Result.Bottom := Result.Top - ASizeDir.Y * ARadius;
  Result.Z2 := Result.Z1 + ASizeDir.Z * ARadius;
end;

{ DoNewAutoCADAxes

 Calculates new axes by "Arbitrary Axis Algorithm"
 (see DXF documentation).                          }
procedure DoNewAutoCADAxes(const AV: TFPoint; var AUX, AUY, AUZ: TFPoint);
begin
  AUZ := Ort(AV);
  if (Abs(AUZ.X) < 1/64) and (Abs(AUZ.Y) < 1/64) then
    AUX := Ort(Vector(cnstYOrtAxis, AV))
  else
    AUX := Ort(Vector(cnstZOrtAxis, AV));
  AUY := Ort(Vector(AV, AUX));
end;

procedure DoPreExtrusion (var P: TFPoint; const N: TFPoint);
var
  R: TFPoint;
  AX, AY, AZ: TFPoint;
begin
  DoNewAutoCADAxes(N, AX, AY, AZ);
  R.X := P.X * AX.X + P.Y * AX.Y + P.Z * AX.Z;
  R.Y := P.X * AY.X + P.Y * AY.Y + P.Z * AY.Z;
  R.Z := P.X * AZ.X + P.Y * AZ.Y + P.Z * AZ.Z;
  P := R;
end;

procedure DoExtrusion (var P: TFPoint; const N: TFPoint);
var
  R: TFPoint;
  AX, AY, AZ: TFPoint;
begin
  AX := cnstXOrtAxis;
  AY := cnstYOrtAxis;
  AZ := cnstZOrtAxis;
  DoPreExtrusion(AX, N);
  DoPreExtrusion(AY, N);
  DoPreExtrusion(AZ, N);

  AX := ort(AX);
  AY := ort(AY);
  AZ := ort(AZ);

  R.X := P.X * AX.X + P.Y * AX.Y + P.Z * AX.Z;
  R.Y := P.X * AY.X + P.Y * AY.Y + P.Z * AY.Z;
  R.Z := P.X * AZ.X + P.Y * AZ.Y + P.Z * AZ.Z;
  P := R;
end;

function Extruded(const P: TFPoint): Boolean;
//const fDoubleAccuracy = 0.000000000001;
begin
  Result := (P.X <> 0) or (P.Y <> 0) or (P.Z <> 1.0);
  //Result := (Abs(P.X) > fDoubleAccuracy) or (Abs(P.Y) > fDoubleAccuracy) or (Abs(P.Z-1) > fDoubleAccuracy);
end;

function ExtrusionToMatrixT(const P: TFPoint): TFMatrix;
var
  N: TFPoint;
begin
  Result := cnstIdentityMat;
  N := ort(P);
  DoPreExtrusion(Result.EX, N);
  DoPreExtrusion(Result.EY, N);
  DoPreExtrusion(Result.EZ, N);
end;

function ExtrusionToMatrix(const P: TFPoint): TFMatrix;
begin
  Result := FMatTranspose(ExtrusionToMatrixT(P));
end;

function MatFromExtr(const P: TFPoint; Angle: Double): TFMatrix;
begin
  Result := FMatXMat(FMatByAngle(Angle), ExtrusionToMatrix(P));
end;

function BuildMatrix(const APoint, AScale, AExtrusion, AOffset: TFPoint;
  const AAngle: Double; const AInitMatrix: TFMatrix): TFMatrix;
var
  vPoint: TFPoint;
begin
  vPoint := APoint;
  DoExtrusion(vPoint, AExtrusion);
  Result := FMatTranslate(AInitMatrix, AOffset);
  Result := FMatScale(Result, AScale);
  Result := FMatXMat(Result, MatFromExtr(AExtrusion, AAngle));
  Result := FMatTranslate(Result, vPoint);
end;

function ExtractMatrixAbsoluteScale(const AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): TFPoint;
var
  vDet: Extended;
begin
  vDet := Abs(AffineMatrixDeterminant(AMatrix));
  if vDet > AResolution then
    vDet := Power(vDet, 1.0/3.0)
  else
    vDet := AResolution * 10;
  Result := MakeFPoint(vDet, vDet, vDet);
end;

function ExtractMatrixAbsoluteScale(const AMatrix: TFMatrix;
  const AResolution: Double): TFPoint;
begin
  Result := ExtractMatrixAbsoluteScale(AMatrix.Affine, AResolution);
end;

procedure ExtractMatrixParams(const AMatrix: TFMatrix; var APoint, AScale,
  AExtrusion: TFPoint; var AAngle: Double);
var
  vM, vExtrM: TsgAffineMatrix;
begin
  vM := AMatrix.Affine;
  AScale := ExtractMatrixAbsoluteScale(AMatrix.Affine, fMaxResolution);
  vM := FMatScale(vM, ReverseScale(AScale));
  AExtrusion := vM.EZ;
  DoNewAutoCADAxes(AExtrusion, vExtrM.EX, vExtrM.EY, vExtrM.EZ);
  vExtrM := FMatTranspose(vExtrM);
  APoint := AffineTransformPoint(AMatrix.E0, vExtrM);
  vM := FMatXMat(vM, vExtrM);
  if Abs(vM.M[0, 0]) > 1 then
    AAngle := 0
  else
  begin
    if 1 - Abs(vM.M[0, 0]) > fDoubleResolution then
    begin
      AAngle := ArcCos(vM.M[0, 0]);
      if vM.EX.Y < 0 then
        AAngle := cnstPi - AAngle;
    end
    else
      AAngle := 0;
  end;
  AAngle := RadToDeg(AAngle);
  vM := FMatXMat(vM, FMatByAngle(-AAngle).Affine);
  if (vM.EX.X < 0) and (vM.EY.Y < 0) then
    AAngle := AAngle + 180;
end;

{ TurnAroundOfVector

  Turn (rotation) APoint on angle AAngle around of vector AVect(k, m, n). }
function TurnAroundOfVector(const APoint, AVect: TFPoint; AAngle: Double): TFPoint;
var
  k, m, n, x, y, z, len, Cs, Ss: Double;
begin
  Result := APoint;
  if Abs(AAngle) < fAccuracy then
    Exit;
  k := AVect.X;
  m := AVect.Y;
  n := AVect.Z;
  x := APoint.X;
  y := APoint.Y;
  z := APoint.Z;
  Cs := Cos(AAngle);
  Ss := Sin(AAngle);
  len:= k*k + n*n + m*m;
  if len = 0.0 then
    Exit
  else
    if len <> 1.0 then
    begin
      len := Sqrt(len);
      k := k / len;
      m := m / len;
      n := n / len;
    end;
  Result.X := x*(k*k*(1-Cs) + Cs)   + y*(k*m*(1-Cs) - n*Ss) + z*(k*n*(1-Cs) + m*Ss);
  Result.Y := x*(k*m*(1-Cs) + n*Ss) + y*(m*m*(1-Cs) + Cs)   + z*(m*n*(1-Cs) - k*Ss);
  Result.Z := x*(k*n*(1-Cs) - m*Ss) + y*(m*n*(1-Cs) + k*Ss) + z*(n*n*(1-Cs) + Cs);

end;

{ GetViewTwistMatrix

  ADirect - direction vector,
  AAngle - roll angle in degrees. }
function GetViewTwistMatrix(const ADirect: TFPoint; const AAngle: Double): TFMatrix;
begin
  Result.E0 := cnstFPointZero;
  DoNewAutoCADAxes(ADirect, Result.EX, Result.EY, Result.EZ);
  Result.EX := TurnAroundOfVector(Result.EX, Result.EZ, -Radian(AAngle));
  Result.EY := TurnAroundOfVector(Result.EY, Result.EZ, -Radian(AAngle));
  // Transposed
  Result := FMatTranspose(Result);
  { alternate fill
  Result.E0 := cnstFPointZero;
  DoNewAutoCADAxes(ADirect, Result.EX, Result.EY, Result.EZ);
  Result := FMatXMat(FMatTranspose(Result), FMatByAngle(AAngle)); }
end;

function GetColorByBackgroud(const BackgroundColor: TColor): TColor;
var
  R, G, B: Byte;
  vGrey: Byte;
begin
  Result := BackgroundColor;
  if Result <> clBlack then
  begin
    ConvertColorToRGB(Result, R, G, B);
    vGrey := Round(0.7 * (0.2126 * R + 0.7152 * G + 0.0722 * B));
    Result :=  ConvertRGBtoColor(vGrey, vGrey, vGrey);
  end
  else
    Result :=  ConvertRGBtoColor(255, 255, 255);
end;

function GetColorByLeftTopBitmapPixel(ABitmap: TBitmap): TColor;
var
  vMap: TsgMap;
begin
  vMap := TsgMap.Wrap(ABitmap);
  try
    Result := vMap.Pixels[0, {$IFDEF SG_FIREMONKEY}vMap.Height - 1{$ELSE}0{$ENDIF}];
  finally
    TsgMap.Unwrap(vMap);
  end;
end;

function GetColorByLeftTopPixel(AGraphic: TGraphic; var ATransparency: Boolean;
  ADefColor: TColor = clWhite): TColor;
//  procedure SetTransparentColorFromBitmap(ABitmap: TBitmap);
//  begin
//    Result := ABitmap.Canvas.Pixels[0, 0];
//    if Result = -1 then
//      Result := ADefColor;
//  end;
//
//var
//  vBitmap: TBitmap;
{$IFNDEF SG_FIREMONKEY}
var
  I, vColorsCount: Integer;
  vColors: array[Byte] of Integer;
  vMap: TsgMap;
{$ENDIF}
begin
  Result := ADefColor;
  if AGraphic = nil then Exit;
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGDEL_2009}
  if AGraphic is TPngImage then
  begin
    Result := TPngImage(AGraphic).TransparentColor;
    ATransparency := TPngImage(AGraphic).Transparent;
    Exit;
  end
  else
{$ENDIF}
{$IFNDEF SG_CADIMPORTERDLLDEMO}
    if AGraphic is TsgBitmap then
    begin
      vColorsCount := TsgBitmap(AGraphic).GetColors(@vColors[0]);
      for I := 0 to vColorsCount - 1 do SwapByte(PByteArray(@vColors[I])^[0], PByteArray(@vColors[I])^[2]);
      vMap := TsgMap.Wrap(AGraphic.Width, AGraphic.Height, TsgBitmap(AGraphic).PixelFormat, TsgBitmap(AGraphic).ImageData.Data, @vColors[0], vColorsCount);
      try
        Result := vMap.Pixels[0, 0];
      finally
        TsgMap.Unwrap(vMap);
      end;
    end
    else
{$ENDIF}
{$ENDIF}
      if TObject(AGraphic) is TBitmap then
        Result := GetColorByLeftTopBitmapPixel(TBitmap(AGraphic))
{$IFDEF SG_DELPHI_VCL}
      else
        if AGraphic is TJPEGImage then
          Result := GetColorByLeftTopBitmapPixel(TJPEGImageAccess(AGraphic).Bitmap);
{$ENDIF};

  //do not need to change TranparentColor to Color of Left Top Pixel
//  if AGraphic is TBitmap then
//    SetTransparentColorFromBitmap(TBitmap(AGraphic))
//  else
//  begin
//    vBitmap := TBitmap.Create;
//    try
//      try
//        vBitmap.Assign(AGraphic);
//      except
//        FreeAndNil(vBitmap);
//        Exit;
//      end;
//      SetTransparentColorFromBitmap(vBitmap);
//    finally
//      vBitmap.Free;
//    end;
//  end;
end;

function DistanceFPointSqr(const AP1, AP2: TFPoint): Extended;
begin
  Result := DistanceVectorSqr(AP1.X - AP2.X, AP1.Y - AP2.Y, AP1.Z - AP2.Z);
end;

function DistanceF2DPoint(const AP1, AP2: TF2DPoint): Double;
begin
  Result := DistanceVector(AP1.X - AP2.X, AP1.Y - AP2.Y, 0);
end;

function DistanceF2DPointSqr(const AP1, AP2: TF2DPoint): Extended;
begin
  Result := DistanceVectorSqr(AP1.X - AP2.X, AP1.Y - AP2.Y, 0);
end;

function DistanceFPoint2D(const AP1, AP2: TFPoint): Double;
begin
  Result := DistanceVector(AP1.X - AP2.X, AP1.Y - AP2.Y, 0);
end;

function DistanceFPoint2DSqr(const AP1, AP2: TFPoint): Extended;
begin
  Result := DistanceVectorSqr(AP2.X - AP1.X, AP2.Y - AP1.Y, 0);
end;

function DistanceFPointToLine(const ALine: TsgLine; const APoint: TFPoint): Double;
var
  dx, dy, K1, K2, L: Extended;
begin
  dx := ALine.Point2.X - ALine.Point1.X;
  dy := ALine.Point2.Y - ALine.Point1.Y;
  L := Sqr(dx) + Sqr(dy);
  if not sgIsZero(L) then
  begin
    K1 := ALine.Point1.Y - APoint.Y;
    K2 := ALine.Point1.X - APoint.X;
    Result := Abs(K1 * dx - K2 * dy) / Sqrt(Abs(L));
  end
  else
    Result := DistanceFPoint2D(ALine.Point1, APoint);
end;

function DistanceFPoints(APoints: IsgArrayFPoint): Double;
var
  I, vCount: Integer;
  vPoint1, vPoint2: TFPoint;
begin
  Result := 0;
  vCount := APoints.FPointCount;
  if vCount > 1 then
  begin
    vPoint1 := APoints.FPoints[0];
    for I := 1 to vCount - 1 do
    begin
      vPoint2 := APoints.FPoints[I];
      Result := Result + DistanceFPoint(vPoint1, vPoint2);
      vPoint1 := vPoint2;
    end;
  end;
end;

function DistanceFVector(const AV: TFPoint): Double;
begin
  Result := DistanceVector(AV.X, AV.Y, AV.Z);
end;

function DistancePoint(const AP1, AP2: TPoint): Double;
begin
  Result := DistanceVector(AP2.X - AP1.X, AP2.Y - AP1.Y, 0);
end;

function DistancePointSqr(const AP1, AP2: TPoint): Double;
begin
  Result := DistanceVectorSqr(AP2.X - AP1.X, AP2.Y - AP1.Y, 0);
end;

function DistanceLine(const ALine: TsgLine): Double;
begin
  Result := DistanceFPoint(ALine.Point1, ALine.Point2);
end;

function DistanceLine2D(const ALine: TsgLine): Double;
begin
  Result := DistanceFPoint2D(ALine.Point1, ALine.Point2);
end;

function DistanceVector(const AX, AY, AZ: Extended): Extended;
begin
  Result := Sqr(AX) + Sqr(AY) + Sqr(AZ);
  if Result > fDoubleResolution then
    Result := Sqrt(Result)
  else
    Result := 0;
end;

function DistanceVector2D(const AX, AY: Extended): Extended;
begin
  try
    Result := Sqrt(Abs(Sqr(AX) + Sqr(AY)));
  except
    Result := 0.0;
  end;
end;

function DistanceVectorSqr(const AX, AY, AZ: Extended): Extended;
begin
  Result := Sqr(AX) + Sqr(AY) + Sqr(AZ);
end;

function ScalarMultiplyVectorsCoords(const AX1, AY1, AX2, AY2: Extended): Extended;
begin
  Result := AX1 * AX2 + AY1 * AY2;
end;

function ScalarMultiplyVectorsCoords(const AX1, AY1, AZ1, AX2, AY2, AZ2: Extended): Extended;
begin
  Result := AX1 * AX2 + AY1 * AY2 + AZ1 * AZ2;
end;

function ScalarMultiplyVectors(const Vector1, Vector2: TFPoint): Extended;
begin
  Result := ScalarMultiplyVectorsCoords(Vector1.X, Vector1.Y, Vector1.Z,
   Vector2.X, Vector2.Y, Vector2.Z);
end;

function SubPoint(const AP1, AP2: TPoint): TPoint;
begin
  Result.X := AP1.X - AP2.X;
  Result.Y := AP1.Y - AP2.Y;
end;

function SubPoint(const AP1, AP2: TPointF): TPointF;
begin
  Result.X := AP1.X - AP2.X;
  Result.Y := AP1.Y - AP2.Y;
end;

function SubFPoint(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := AP1.X - AP2.X;
  Result.Y := AP1.Y - AP2.Y;
  Result.Z := AP1.Z - AP2.Z;
end;

function SubFPoint2D(const AP1, AP2: TFPoint): TFPoint;
begin
  Result.X := AP1.X - AP2.X;
  Result.Y := AP1.Y - AP2.Y;
  Result.Z := 0;
end;

function SubF2DPoint(const AP1, AP2: TF2DPoint): TF2DPoint;
begin
  Result.X := AP1.X - AP2.X;
  Result.Y := AP1.Y - AP2.Y;
end;

function RotateFPoint(const P: TFPoint; const S, C: Extended): TFPoint;
begin
  Result.X := C * P.X - S * P.Y;
  Result.Y := S * P.X + C * P.Y;
  Result.Z := P.Z;
end;

function RotateFPoint(const P: TFPoint; Angle: Extended): TFPoint;
var
  S, C: Extended;
begin
  SinCos(Radian(Angle), S, C);
  Result := RotateFPoint(P, S, C);
end;

procedure RotateFRect(var R: TFRect; Angle: Extended);
var
  R1: TFRect;
  S, C: Double;
  procedure DoPoint(X, Y, Z: Extended);
  begin
    ExpandFRect(R1, MakeFPoint(X * C - Y * S, X * S + Y * C , Z));
  end;
begin
  if Angle = 0 then
    Exit;
  Angle := Radian(Angle);
  S := Sin(Angle);
  C := Cos(Angle);
  R1 := cnstBadRect;
  DoPoint(R.Left, R.Top, R.Z1);
  DoPoint(R.Right, R.Top, R.Z1);
  DoPoint(R.Left, R.Bottom, R.Z1);
  DoPoint(R.Right, R.Bottom, R.Z1);
  DoPoint(R.Left, R.Top, R.Z2);
  DoPoint(R.Right, R.Top, R.Z2);
  DoPoint(R.Left, R.Bottom, R.Z2);
  DoPoint(R.Right, R.Bottom, R.Z2);
  R := R1;
end;

function RotateAroundFPoint(const APoint, ABasePoint: TFPoint; const Abs: Boolean;
  const Angle: Double): TFPoint;
var
  vDX, vDY, vDistance, vAngle: Double;
  S, C: Extended;
begin
  vDX := APoint.X - ABasePoint.X;
  vDY := APoint.Y - ABasePoint.Y;
  vDistance := Sqr(vDX) + Sqr(vDY);
  if vDistance > fDoubleResolution then
  begin
    vDistance := Sqrt(vDistance);
    if Abs then
      vAngle := Angle
    else
      vAngle := Angle + GetAngleByPoints(ABasePoint, APoint, True);
    SinCos(vAngle, S, C);
    Result.X := ABasePoint.X + vDistance * C;
    Result.Y := ABasePoint.Y + vDistance * S;
    Result.Z := APoint.Z;
  end
  else
    Result := APoint;
end;

function RoundFPoint(const APoint: TFPoint; const ADigits: Byte): TFPoint;
begin
  Result.X := RoundTo(APoint.X, -ADigits);
  Result.Y := RoundTo(APoint.Y, -ADigits);
  Result.Z := RoundTo(APoint.Z, -ADigits);
end;

function RoundFPoint(const APoint: TFPoint; const ADigits, AFactor: Byte): TFPoint; overload;
begin
  Result.X := sgRoundTo(APoint.X, -ADigits, AFactor);
  Result.Y := sgRoundTo(APoint.Y, -ADigits, AFactor);
  Result.Z := sgRoundTo(APoint.Z, -ADigits, AFactor);
end;

function RoundF2DPoint(const APoint: TF2DPoint; const ADigits: Byte): TF2DPoint;
begin
  Result.X := RoundTo(APoint.X, -ADigits);
  Result.Y := RoundTo(APoint.Y, -ADigits);
end;


function FMatXMat(const A, B: TFMatrix): TFMatrix;
begin
  Result.M[0,0] := A.M[0,0] * B.M[0,0] + A.M[0,1] * B.M[1,0] + A.M[0,2] * B.M[2,0];
  Result.M[1,0] := A.M[1,0] * B.M[0,0] + A.M[1,1] * B.M[1,0] + A.M[1,2] * B.M[2,0];
  Result.M[2,0] := A.M[2,0] * B.M[0,0] + A.M[2,1] * B.M[1,0] + A.M[2,2] * B.M[2,0];
  Result.M[3,0] := A.M[3,0] * B.M[0,0] + A.M[3,1] * B.M[1,0] + A.M[3,2] * B.M[2,0] + B.M[3,0];

  Result.M[0,1] := A.M[0,0] * B.M[0,1] + A.M[0,1] * B.M[1,1] + A.M[0,2] * B.M[2,1];
  Result.M[1,1] := A.M[1,0] * B.M[0,1] + A.M[1,1] * B.M[1,1] + A.M[1,2] * B.M[2,1];
  Result.M[2,1] := A.M[2,0] * B.M[0,1] + A.M[2,1] * B.M[1,1] + A.M[2,2] * B.M[2,1];
  Result.M[3,1] := A.M[3,0] * B.M[0,1] + A.M[3,1] * B.M[1,1] + A.M[3,2] * B.M[2,1] + B.M[3,1];

  Result.M[0,2] := A.M[0,0] * B.M[0,2] + A.M[0,1] * B.M[1,2] + A.M[0,2] * B.M[2,2];
  Result.M[1,2] := A.M[1,0] * B.M[0,2] + A.M[1,1] * B.M[1,2] + A.M[1,2] * B.M[2,2];
  Result.M[2,2] := A.M[2,0] * B.M[0,2] + A.M[2,1] * B.M[1,2] + A.M[2,2] * B.M[2,2];
  Result.M[3,2] := A.M[3,0] * B.M[0,2] + A.M[3,1] * B.M[1,2] + A.M[3,2] * B.M[2,2] + B.M[3,2];
end;

function FMatXMat(const A, B: TsgAffineMatrix): TsgAffineMatrix;
begin
  Result.M[0,0] := A.M[0,0] * B.M[0,0] + A.M[0,1] * B.M[1,0] + A.M[0,2] * B.M[2,0];
  Result.M[1,0] := A.M[1,0] * B.M[0,0] + A.M[1,1] * B.M[1,0] + A.M[1,2] * B.M[2,0];
  Result.M[2,0] := A.M[2,0] * B.M[0,0] + A.M[2,1] * B.M[1,0] + A.M[2,2] * B.M[2,0];

  Result.M[0,1] := A.M[0,0] * B.M[0,1] + A.M[0,1] * B.M[1,1] + A.M[0,2] * B.M[2,1];
  Result.M[1,1] := A.M[1,0] * B.M[0,1] + A.M[1,1] * B.M[1,1] + A.M[1,2] * B.M[2,1];
  Result.M[2,1] := A.M[2,0] * B.M[0,1] + A.M[2,1] * B.M[1,1] + A.M[2,2] * B.M[2,1];

  Result.M[0,2] := A.M[0,0] * B.M[0,2] + A.M[0,1] * B.M[1,2] + A.M[0,2] * B.M[2,2];
  Result.M[1,2] := A.M[1,0] * B.M[0,2] + A.M[1,1] * B.M[1,2] + A.M[1,2] * B.M[2,2];
  Result.M[2,2] := A.M[2,0] * B.M[0,2] + A.M[2,1] * B.M[1,2] + A.M[2,2] * B.M[2,2];
end;

function FMatXMat2D(const A, B: TFMatrix): TFMatrix;
begin
  Result := cnstIdentityMat;

  Result.M[0,0] := A.M[0,0] * B.M[0,0] + A.M[1,0] * B.M[0,1];
  Result.M[1,0] := A.M[0,0] * B.M[1,0] + A.M[1,0] * B.M[1,1];
  Result.M[3,0] := A.M[0,0] * B.M[3,0] + A.M[1,0] * B.M[3,1] + A.M[3,0];

  Result.M[0,1] := A.M[0,1] * B.M[0,0] + A.M[1,1] * B.M[0,1];
  Result.M[1,1] := A.M[0,1] * B.M[1,0] + A.M[1,1] * B.M[1,1];
  Result.M[3,1] := A.M[0,1] * B.M[3,0] + A.M[1,1] * B.M[3,1] + A.M[3,1];
end;

function FMatXMatBase(const A, B: TFMatrix; const P: TFPoint): TFMatrix;
var
  M1, M2: TFMatrix;
begin
  M1 := FMatXMat(A, FMatByTranslate(-P.X, -P.Y, -P.Z));
  M2 := FMatXMat(M1, B);
  Result := FMatXMat(M2, FMatByTranslate(P.X, P.Y, P.Z));
end;

function FMatByAngle(const Angle: Double): TFMatrix;
begin
  Result := BuildRotMatrix(axisZ, DegToRad(Angle), fDoubleResolution);
end;

function FMatByMirror(const AP1, AP2: TFPoint): TFMatrix;
var
  P: TFPoint;
  vAngle: Double;
begin
  Result := cnstIdentityMat;
  P := MirrorFPointOfLine(cnstFPointZero, AP1, AP2);
  vAngle := 2 * GetAngleByPoints(AP1, AP2, False) - 180;
  Result.M[0, 0] := -1;
  Result := FMatXMat(Result, FMatByAngle(vAngle));
  Result := FMatXMat(Result, StdMat(cnstFPointSingle, P));
end;

function FMatByScale(const Scale: Double): TFMatrix;
begin
  Result := FMatByScales(Scale, Scale, Scale);
end;

function FMatByScales(const SX, SY, SZ: Double): TFMatrix;
begin
  FMatByScales(Result.Affine, SX, SY, SZ);
  Result.E0 := cnstFPointZero;
end;

function FMatByScales(const AScale: TFPoint): TFMatrix;
begin
  Result := FMatByScales(AScale.X, AScale.Y, AScale.Z);
end;

procedure FMatByScales(var AMatrix: TsgAffineMatrix; const SX, SY, SZ: Double);
begin
  FillChar(AMatrix, SizeOf(AMatrix), 0);
  AMatrix.M[0, 0] := SX;
  AMatrix.M[1, 1] := SY;
  AMatrix.M[2, 2] := SZ;
end;

procedure FMatByScales(var AMatrix: TsgAffineMatrix; const AScale: TFPoint);
begin
  FMatByScales(AMatrix, AScale.X, AScale.Y, AScale.Z);
end;

function InvertMatrix(var AMatrix: TFMatrix; const AResolution: Double): Extended;
begin
  Result := AffineMatrixDeterminant(AMatrix.Affine);
  if Abs(Result) > AResolution then
  begin
    AMatrix.Affine := FMatScale(AffineMatrixAdjoint(AMatrix.Affine), 1/Result);
    AMatrix.E0 := Reverse(AffineTransformPoint(AMatrix.E0, AMatrix.Affine));
  end
  else
    AMatrix := cnstIdentityMat;
end;

function InvertMatrix(var AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): Extended;
begin
  Result := AffineMatrixDeterminant(AMatrix);
  if Abs(Result) > AResolution then
    AMatrix := FMatScale(AffineMatrixAdjoint(AMatrix), 1/Result)
  else
    AMatrix := cnstIdentityMat.Affine;
end;

function FMatInverseGausse_Jordan(const AMatrix: TFMatrix;
  const AResolution: Double = fMaxResolution): TFMatrix;
var
  I, J: Integer;
  vMatrix: TFMatrix;
  vScale: Double;

  procedure ScalePoint(var APoint: TFPoint; const AScale: Double);{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    APoint.X := APoint.X * AScale;
    APoint.Y := APoint.Y * AScale;
    APoint.Z := APoint.Z * AScale;
  end;

  procedure SubScaledPoint(var AResult: TFPoint; const APoint: TFPoint;
    const AScale: Double);{$IFDEF USE_INLINE} inline;{$ENDIF}
  begin
    AResult.X := AResult.X - APoint.X * AScale;
    AResult.Y := AResult.Y - APoint.Y * AScale;
    AResult.Z := AResult.Z - APoint.Z * AScale;
  end;

begin
  vMatrix := AMatrix;
  Result := cnstIdentityMat;
  for I := 0 to 2 do
  begin
    J := I;
    repeat
      if not (Abs(vMatrix.M[J, I]) < AResolution) then
      begin
        if J <> I then
        begin
          // swap rows I and J
          SwapFPoints(vMatrix.V[I], vMatrix.V[J]);
          SwapFPoints(Result.V[I], Result.V[J]);
        end;
        J := 2; // break while J <= 2 do loop
      end;
      Inc(J);
    until J > 2;
    if vMatrix.M[I, I] <> 1.0 then
    begin
      if vMatrix.M[I, I] <> 0 then
        vScale := 1.0 / vMatrix.M[I, I]
      else
        vScale := 1.0 / AResolution;
      // scale row I
      ScalePoint(vMatrix.V[I], vScale);
      ScalePoint(Result.V[I], vScale);
    end;
    for J := 0 to 3 do
    begin
      vScale := vMatrix.M[J, I];
      if (J <> I) and (vScale <> 0) then
      begin
        // scale row I by M[J, I] and sub from row J
        SubScaledPoint(vMatrix.V[J], vMatrix.V[I], vScale);
        SubScaledPoint(Result.V[J], Result.V[I], vScale);
      end;
    end;
  end;
end;

function FMat2DByParams(const A, B, C, D, E, F: Double): TFMatrix;
begin
  Result := cnstIdentityMat;
  Result.V1[0] := A;
  Result.V1[1] := B;
  Result.V2[0] := C;
  Result.V2[1] := D;
  Result.V4[0] := E;
  Result.V4[1] := F;
end;

function FMatBySkew(const ASkewX, ASkewY: Double): TFMatrix;
begin
  Result := cnstIdentityMat;
  try
    Result.M[1, 0] := Tan(ASkewX * fPiDividedBy180);
    Result.M[0, 1] := Tan(ASkewY * fPiDividedBy180);
  except
    Result := cnstIdentityMat;
  end;
end;

function FMatByTranslate(const AX, AY, AZ: Double): TFMatrix;
begin
  Result := FMatByTranslate(MakeFPoint(AX, AY, AZ));
end;

function FMatByTranslate(const AOffset: TFPoint): TFMatrix;
begin
  Result := cnstIdentityMat;
  Result.E0 := AOffset;
end;

function FMat2DByImage(const APoint, UVector, VVector: TFPoint;
  const AWidth, AHeight: Double): TFMatrix;
var
  vMatrixRot: TFMatrix;
  vDistanceU, vDistanceV: Extended;
begin
  vDistanceU := DistanceVector2D(UVector.X, UVector.Y);
  vDistanceV := DistanceVector2D(VVector.X, VVector.Y);

  vMatrixRot := cnstIdentityMat;
  vMatrixRot.V1[0] := Vvector.X / vDistanceV;
  vMatrixRot.V1[1] := Vvector.Y / vDistanceV;
  vMatrixRot.V2[0] := Uvector.X / vDistanceU;
  vMatrixRot.V2[1] := Uvector.Y / vDistanceU;

  Result := FMatByTranslate(APoint.X, APoint.Y, 0);  
  Result := FMatXMat2D(Result, vMatrixRot);
  Result := FMatXMat2D(Result, FMatByScales(vDistanceV/AWidth,
    vDistanceU/AHeight, 1));
end;

function FMatByImage(const APoint, UVector, VVector: TFPoint;
  const AWidth, AHeight: Double): TFMatrix;
//U-cnstXOrtAxis//1;0;0
//V-cnstYOrtAxis//0;1;0
  procedure SetDirection(const AVector: TFPoint; var ADirection: TFPoint; var ADistance: Extended);
  begin
    ADistance := DistanceVector(AVector.X, AVector.Y, AVector.Z);
    if ADistance <> 0 then
      ADirection := MultiplyFPointParam(AVector, 1 / ADistance)
    else
      ADistance := 1;
  end;

var
  vMatrixRot: TFMatrix;
  vDistanceU, vDistanceV: Extended;
begin
  vMatrixRot := cnstIdentityMat;
  SetDirection(UVector, vMatrixRot.EX, vDistanceU);
  SetDirection(VVector, vMatrixRot.EY, vDistanceV);
  Result := FMatXMat(FMatByTranslate(APoint), vMatrixRot);
  if (AWidth <> 0) and (AHeight <> 0) then
    Result := FMatXMat(Result, FMatByScales(vDistanceU/AWidth, vDistanceV/AHeight, 1));
end;

function FMat2DNormalize(const AMatrix: TFMatrix; const AScale: PDouble = nil):  TFMatrix;
var
  vMax: Double;
begin
  vMax := Max(Max(Abs(AMatrix.M[0,0]), Abs(AMatrix.M[0,1])),
              Max(Abs(AMatrix.M[1,0]), Abs(AMatrix.M[1,1])));
  if AScale <> nil then
    AScale^ :=  vMax;
  if vMax  <> 0 then
  begin
    vMax := 1. / vMax;
    FillChar(Result, SizeOf(Result), 0);
    Result.M[0, 0] := AMatrix.M[0, 0] * vMax;
    Result.M[0, 1] := AMatrix.M[0, 1] * vMax;
    Result.M[1, 0] := AMatrix.M[1, 0] * vMax;
    Result.M[1, 1] := AMatrix.M[1, 1] * vMax;
    Result.M[3, 0] := AMatrix.M[3, 0];
    Result.M[3, 1] := AMatrix.M[3, 1];
  end
  else
    Result := AMatrix;
end;

function FMatExtractParams2D(const M: TFMatrix; var APoint, AScale: TFPoint;
  var AAngle: Double): Boolean;
var
  vX, vY, vC, vT: TFPoint;
  vAxisAngle: Double;
  vMatrix2D, vTestMatrix: TFMatrix;
begin
  vMatrix2D := M;
  vMatrix2D.M[0, 2] := 0;
  vMatrix2D.M[1, 2] := 0;
  vMatrix2D.M[2, 2] := 1;
  vMatrix2D.M[3, 2] := 0;

  vX := FPointXMat(cnstXOrtAxis, vMatrix2D);
  vY := FPointXMat(cnstYOrtAxis, vMatrix2D);
  vC := FPointXMat(cnstFPointZero, vMatrix2D);
  vAxisAngle := GetAngleOfVectors(SubFPoint(vX, vC), SubFPoint(vY, vC), False);
  Result := IsEqual(vAxisAngle, 90);
  AAngle := GetAngleByPoints(vC, vX, False);
  APoint := MakeFPoint(vMatrix2D.M[3, 0], vMatrix2D.M[3, 1], 0);
  AScale := MakeFPoint(DistanceFPoint(vX, vC), DistanceFPoint(vY, vC), 1);

  vTestMatrix := StdMat(AScale, cnstFPointZero);
  vTestMatrix := FMatXMat(vTestMatrix, FMatByAngle(AAngle));
  vTestMatrix := FMatXMat(vTestMatrix, StdMat(cnstFPointSingle, APoint));

  vT := FPointXMat(cnstXOrtAxis, vTestMatrix);
  if not IsEqualDirection(vX, vT, vC) then
    AScale.X := -AScale.X;

  vT := FPointXMat(cnstYOrtAxis, vTestMatrix);
  if not IsEqualDirection(vY, vT, vC) then
    AScale.Y := -AScale.Y;
end;

function FPointXMatInverse(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint;
var
  vX, vY, vZ: Double;
begin
  vX := APoint.X + AMatrix.M[3,0];
  vY := APoint.Y + AMatrix.M[3,1];
  vZ := APoint.Z + AMatrix.M[3,2];
  Result.X := vX * AMatrix.M[0,0] + vY * AMatrix.M[1,0] + vZ * AMatrix.M[2,0];
  Result.Y := vX * AMatrix.M[0,1] + vY * AMatrix.M[1,1] + vZ * AMatrix.M[2,1];
  Result.Z := vX * AMatrix.M[0,2] + vY * AMatrix.M[1,2] + vZ * AMatrix.M[2,2];
end;

function FPointXMat2D(const AP: TFPoint; const M: TFMatrix; const Affine: Boolean): TFPoint; overload;
begin
  Result.Z := 0;
  Result.X := AP.X * M.M[0, 0] + AP.Y * M.M[1, 0];
  Result.Y := AP.X * M.M[0, 1] + AP.Y * M.M[1, 1];
  if not Affine then
  begin
    Result.X := Result.X + M.M[3, 0];
    Result.Y := Result.Y + M.M[3, 1];
  end;
end;

function FPointXMat2D(const AP: TFPoint; const M: TFMatrix): TF2DPoint; overload;
begin
  Result.X := AP.X * M.M[0, 0] + AP.Y * M.M[1, 0] + AP.Z * M.M[2, 0] + M.M[3, 0];
  Result.Y := AP.X * M.M[0, 1] + AP.Y * M.M[1, 1] + AP.Z * M.M[2, 1] + M.M[3, 1];
end;

function FPointXMat2D(const AP: TF2DPoint; const M: TFMatrix): TF2DPoint; overload;
begin
  Result.X := AP.X * M.M[0, 0] + AP.Y * M.M[1, 0] + M.M[3, 0];
  Result.Y := AP.X * M.M[0, 1] + AP.Y * M.M[1, 1] + M.M[3, 1];
end;

function FRectXMat(ARect: TFRect; AMatrix: TFMatrix): TFRect;
begin
  Result.TopLeft := FPointXMat(ARect.TopLeft, AMatrix);
  Result.BottomRight := FPointXMat(ARect.BottomRight, AMatrix);
end;

function BoxXMat2D(const ARect: TFRect; const AMatrix: TFMatrix): TFRect;
begin
  Result := cnstBadRect;
  ExpandFRect(Result, FPointXMat(MakeFPoint(ARect.Left, ARect.Top), AMatrix));
  ExpandFRect(Result, FPointXMat(MakeFPoint(ARect.Left, ARect.Bottom), AMatrix));
  ExpandFRect(Result, FPointXMat(MakeFPoint(ARect.Right, ARect.Top), AMatrix));
  ExpandFRect(Result, FPointXMat(MakeFPoint(ARect.Right, ARect.Bottom), AMatrix));
end;

function IsRotatedFMatEx(const M: TFMatrix; const ANormals: PPoint;
  const IsFullCheck: Boolean): Boolean;

  function CheckNormal(const AANormal: Integer; const AMatrixValue: Double): Boolean;
  begin
    if AANormal > 0 then
      Result := (AMatrixValue <= fAccuracy)
    else
      if AANormal < 0 then
        Result := (AMatrixValue >= fAccuracy)
      else
        Result := False;
  end;

var
  I, J: Integer;
  K: Double;
begin
  Result := False;
  if IsFullCheck then
  begin
    if ANormals = nil then
       Result := (M.M[0,0] <= fAccuracy) or (M.M[1,1] >= fAccuracy)
    else
    begin
      if CheckNormal(ANormals^.X, M.M[0,0]) or CheckNormal(ANormals^.Y, M.M[1,1]) then
        Result := True;
    end;
    if Result then
      Exit;
    if Abs(M.M[1,1]) > fAccuracy then
    begin
      K := Abs(M.M[0,0] / M.M[1,1]);
      Result := Abs(K - 1) > 0.15;
    end
    else
      Result := Abs(M.M[0,0]) > fAccuracy;
    if Result then
      Exit;
  end;
  for I := 0 to 2 do
  begin
    for J := 0 to 2 do
    begin
      if I <> J then
      begin
        Result := Abs(M.M[I,J]) > fAccuracy;
        if Result then
          Exit;
      end;
    end;
  end;
end;

function IsRotatedFMat(const M: TFMatrix; const IsFullCheck: Boolean = False): Boolean;
begin
  Result := IsRotatedFMatEx(M, nil, IsFullCheck);
end;

function Pt2DXMat(const AP: TF2dPoint; const M: TFMatrix): TF2dPoint;
begin
  Result.X := AP.X * M.M[0,0] + AP.Y * M.M[1,0] + M.M[3,0];
  Result.Y := AP.X * M.M[0,1] + AP.Y * M.M[1,1] + M.M[3,1];
end;

function PtXScalar(const AP: TFPoint; const AVal: TsgFloat): TFPoint;
begin
  Result.X := AP.X * AVal;
  Result.Y := AP.Y * AVal;
  Result.Z := AP.Z * AVal;
end;

function PtXScalar(const AP: TPoint; const AVal: TsgFloat): TPoint;
begin
  Result.X := Round(AP.X * AVal);
  Result.Y := Round(AP.Y * AVal);
end;

function Pt2XScalar(const AP: TF2DPoint; const AVal: TsgFloat): TF2DPoint;
begin
  Result.X := AP.X * AVal;
  Result.Y := AP.Y * AVal;
end;

function PtXScalar(const AP: TPointF; const AVal: TsgFloat): TPointF;
begin
  Result.X := AP.X * AVal;
  Result.Y := AP.Y * AVal;
end;

function RelativePtXMat(const ABasePt, APt: TFPoint; M: TFMatrix): TFPoint;
begin
  Result := SubFpoint(FPointXMat(APt, M), FPointXMat(cnstFPointZero, M));
end;

function DenomVectors(const AX1, AY1, AX2, AY2: Extended): Extended;
begin
  Result :=  AX1 * AY2 - AX2 * AY1;
end;
(*
function Mat2x2Det(const M: TFMatrix2x2): Double;
begin
  Result := M.M[0, 0] * M.M[1, 1] - M.M[0, 1] * M.M[1, 0];
end;
*)
(*
function Mat2x2Inverse(const M: TFMatrix2x2): TFMatrix3x3;
var
  vDet, vKoef: Double;
begin
  vDet := Mat2x2Det(M);
  if vDet <> 0 then
  begin
    vKoef := 1 / vDet;
    Result.M[0, 0] :=  vKoef * Result.M[1, 1];
    Result.M[0, 1] := -vKoef * Result.M[0, 1];
    Result.M[1, 0] := -vKoef * Result.M[1, 0];
    Result.M[1, 1] :=  vKoef * Result.M[0, 0];
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function Mat2x2Transform(const M: TFMatrix2x2): TFMatrix3x3;
begin
  Result.M[0, 0] := Result.M[0, 0];
  Result.M[0, 1] := Result.M[1, 0];
  Result.M[1, 0] := Result.M[0, 1];
  Result.M[1, 1] := Result.M[1, 1];
end;
*)
(*
function Mat2x2Mul(const AM1, AM2: TFMatrix2x2): TFMatrix2x2;
begin
  Result.M[0, 0] := AM1.M[0, 0] * AM2.M[0, 0] + AM1.M[0, 1] * AM2.M[1, 0];
  Result.M[0, 1] := AM1.M[0, 0] * AM2.M[0, 1] + AM1.M[0, 1] * AM2.M[1, 1];
  Result.M[1, 0] := AM1.M[1, 0] * AM2.M[0, 0] + AM1.M[1, 1] * AM2.M[1, 0];
  Result.M[1, 1] := AM1.M[1, 0] * AM2.M[0, 1] + AM1.M[1, 1] * AM2.M[1, 1];
end;
*)
(*
function Mat3x3Minor(const M: TFMatrix3x3; const AC, AR: Integer): TFMatrix2x2;
var
  I, J, II, JJ: Integer;
begin
  II := 0;
  for I := 0 to 2 do
  begin
    if I <> AR then
    begin
      JJ := 0;    
      for J := 0 to 2 do
      begin
        if J <> AC then
        begin
          Result.M[II, JJ] := M.M[I, J];
          Inc(JJ);
        end;
      end;
      Inc(II);
    end;
  end;
end;
*)
function GetDetSign(const I, J: Integer): Integer;
begin
  if (I + J) mod 2 = 1  then
    Result := -1
  else
    Result := +1;
end;
(*
function Mat3x3Det(const M: TFMatrix3x3): Double;
var
  J: Integer;
begin
  Result := 0;
  for J := 0 to 2 do
    Result := Result + GetDetSign(0, J) * M.M[0, J] * Mat2x2Det(Mat3x3Minor(M, 0, J));
end;

function Mat3x3Transform(const M: TFMatrix3x3): TFMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.M[I, J] := M.M[J, I];
end;

function Mat3x3Inverse(const M: TFMatrix3x3): TFMatrix3x3;
var
  vDet, vKoef: Double;
  I, J: Integer;
begin
  vDet := Mat3x3Det(M);
  if vDet <> 0 then
  begin
    vKoef := 1 / vDet;
    for I := 0 to 2 do
      for J := 0 to 2 do
        Result.M[J, I] := vKoef * GetDetSign(I, J) * Mat2x2Det(Mat3x3Minor(M, I, J));
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function MatToMat3x3(const M: TFMatrix): TFMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.M[I, J] := M.M[I, J];
end;

function Mat3x3ToMat(const M: TFMatrix3x3; const Offset: TFPoint): TFMatrix;
begin
  Result.M[0, 0] := M.M[0, 0];
  Result.M[0, 1] := M.M[0, 1];
  Result.M[0, 2] := M.M[0, 2];
  Result.M[1, 0] := M.M[1, 0];
  Result.M[1, 1] := M.M[1, 1];
  Result.M[1, 2] := M.M[1, 2];
  Result.M[2, 0] := M.M[2, 0];
  Result.M[2, 1] := M.M[2, 1];
  Result.M[2, 2] := M.M[2, 2];
  Result.V4[0] := Offset.X;
  Result.V4[1] := Offset.Y;
  Result.V4[2] := Offset.Z;
end;

function Mat3x3Mul(const AM1, AM2: TFMatrix3x3): TFMatrix3x3;
begin
  Result.M[0, 0] := AM1.M[0, 0] * AM2.M[0, 0] + AM1.M[0, 1] * AM2.M[1, 0] + AM1.M[0, 2] * AM2.M[2, 0];
  Result.M[0, 1] := AM1.M[0, 0] * AM2.M[0, 1] + AM1.M[0, 1] * AM2.M[1, 1] + AM1.M[0, 2] * AM2.M[2, 1];
  Result.M[0, 2] := AM1.M[0, 0] * AM2.M[0, 2] + AM1.M[0, 1] * AM2.M[1, 2] + AM1.M[0, 2] * AM2.M[2, 2];

  Result.M[1, 0] := AM1.M[1, 0] * AM2.M[0, 0] + AM1.M[1, 1] * AM2.M[1, 0] + AM1.M[1, 2] * AM2.M[2, 0];
  Result.M[1, 1] := AM1.M[1, 0] * AM2.M[0, 1] + AM1.M[1, 1] * AM2.M[1, 1] + AM1.M[1, 2] * AM2.M[2, 1];
  Result.M[1, 2] := AM1.M[1, 0] * AM2.M[0, 2] + AM1.M[1, 1] * AM2.M[1, 2] + AM1.M[1, 2] * AM2.M[2, 2];

  Result.M[2, 0] := AM1.M[2, 0] * AM2.M[0, 0] + AM1.M[2, 1] * AM2.M[1, 0] + AM1.M[2, 2] * AM2.M[2, 0];
  Result.M[2, 1] := AM1.M[2, 0] * AM2.M[0, 1] + AM1.M[2, 1] * AM2.M[1, 1] + AM1.M[2, 2] * AM2.M[2, 1];
  Result.M[2, 2] := AM1.M[2, 0] * AM2.M[0, 2] + AM1.M[2, 1] * AM2.M[1, 2] + AM1.M[2, 2] * AM2.M[2, 2];
end;
*)
(*
function MatInverse(const M: TFMatrix): TFMatrix;
var
  vMat, vMatInv: TFMatrix3x3;
begin
  vMat := MatToMat3x3(M);
  vMatInv := Mat3x3Inverse(MatToMat3x3(M));
  Result := Mat3x3ToMat(vMatInv, MakeFPoint(-M.V4[0], -M.V4[1], -M.V4[2]));
end;
*)

function FMatInverse(const AMatrix: TFMatrix; const AResolution: Double): TFMatrix;
begin
  Result := AMatrix;
  InvertMatrix(Result, AResolution);
end;

function FMatInverse(const AMatrix: TsgAffineMatrix;
  const AResolution: Double = fMaxResolution): TsgAffineMatrix;
begin
  Result := AMatrix;
  InvertMatrix(Result, AResolution);
end;

function FMatInverse3x3(const AMatrixIn: TFMatrix;
  var AMatrixOut: TFMatrix): Boolean;
//var
//  vMatrix: TFMatrix;
//begin
//  vMatrix := AMatrix;
//  FMatOffset(vMatrix, cnstFPointZero);
//  Result := FMatInverse(vMatrix);
//  FMatOffset(Result, PtXScalar(AMatrix.E0, -1));
//end;
type
  PFMatrix2x2 = ^TFMatrix2x2;
  TFMatrix2x2 = record
    M: array[0..1, 0..1] of Extended;
  end;

  PFMatrix3x3 = ^TFMatrix3x3;
  TFMatrix3x3 = record
    M: array[0..2, 0..2] of Extended;
  end;

  function GetDetSign(const I, J: Integer): Integer;
  begin
    if (I + J) mod 2 = 1  then
      Result := -1
    else
      Result := +1;
  end;

  function Mat2x2Det(const M: TFMatrix2x2): Extended;
  begin
    Result := M.M[0, 0] * M.M[1, 1] - M.M[0, 1] * M.M[1, 0];
  end;

  function Mat3x3Minor(const M: TFMatrix3x3; const AC, AR: Integer): TFMatrix2x2;
  var
    I, J, II, JJ: Integer;
  begin
{$IFDEF _FIXINSIGHT_}
    FillChar(Result, SizeOf(Result), 0);
{$ENDIF}
    II := 0;
    for I := 0 to 2 do
    begin
      if I <> AR then
      begin
        JJ := 0;
        for J := 0 to 2 do
        begin
          if J <> AC then
          begin
            Result.M[II, JJ] := M.M[I, J];
            Inc(JJ);
          end;
        end;
        Inc(II);
      end;
    end;
  end;

  function Mat3x3Det(const M: TFMatrix3x3): Extended;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 0 to 2 do
      Result := Result + GetDetSign(0, J) * M.M[0, J] * Mat2x2Det(Mat3x3Minor(M, 0, J));
  end;

  function Mat3x3Inverse(const M: TFMatrix3x3; const ADet: Extended): TFMatrix3x3;
  var
    vKoef: Extended;
    I, J: Integer;
  begin
{$IFDEF _FIXINSIGHT_}
    FillChar(Result, SizeOf(Result), 0);
{$ENDIF}
    vKoef := 1 / ADet;
    for I := 0 to 2 do
      for J := 0 to 2 do
        Result.M[J, I] := vKoef * GetDetSign(I, J) * Mat2x2Det(Mat3x3Minor(M, I, J));
  end;

  function MatToMat3x3(const M: TFMatrix): TFMatrix3x3;
  var
    I, J: Integer;
  begin
{$IFDEF _FIXINSIGHT_}
    FillChar(Result, SizeOf(Result), 0);
{$ENDIF}
    for I := 0 to 2 do
      for J := 0 to 2 do
        Result.M[I, J] := M.M[I, J];
  end;

  function Mat3x3ToMat(const M: TFMatrix3x3; const Offset: TFPoint): TFMatrix;
  var
    I, J: Integer;
  begin
    for I := 0 to 2 do
      for J := 0 to 2 do
        Result.M[I, J] := M.M[I, J];
    Result.E0 := Offset;
  end;

var
  vMat: TFMatrix3x3;
  vDet: Extended;
begin
  Result := False;
  AMatrixOut := cnstIdentityMat;
  vMat := MatToMat3x3(AMatrixIn);
  vDet := Mat3x3Det(vMat);
  if not sgIsZero(vDet) then
  begin
    AMatrixOut := Mat3x3ToMat(Mat3x3Inverse(vMat, vDet), PtXScalar(AMatrixIn.E0, -1));
    Result := True;
  end;
end;

function FMatScale(const A: TFMatrix; const AScale: TFPoint): TFMatrix;
begin
  Result.M[0,0] := A.M[0,0] * AScale.X;
  Result.M[1,0] := A.M[1,0] * AScale.X;
  Result.M[2,0] := A.M[2,0] * AScale.X;
  Result.M[3,0] := A.M[3,0] * AScale.X;

  Result.M[0,1] := A.M[0,1] * AScale.Y;
  Result.M[1,1] := A.M[1,1] * AScale.Y;
  Result.M[2,1] := A.M[2,1] * AScale.Y;
  Result.M[3,1] := A.M[3,1] * AScale.Y;

  Result.M[0,2] := A.M[0,2] * AScale.Z;
  Result.M[1,2] := A.M[1,2] * AScale.Z;
  Result.M[2,2] := A.M[2,2] * AScale.Z;
  Result.M[3,2] := A.M[3,2] * AScale.Z;
end;

function FMatScale(const A: TFMatrix; const AScale: Double): TFMatrix; overload;
begin
  Result := FMatScale(A, MakeFPoint(AScale, AScale, AScale));
end;

function FMatScale(const A: TsgAffineMatrix; const AScale: TFPoint): TsgAffineMatrix;
begin
  Result.M[0,0] := A.M[0,0] * AScale.X;
  Result.M[1,0] := A.M[1,0] * AScale.X;
  Result.M[2,0] := A.M[2,0] * AScale.X;

  Result.M[0,1] := A.M[0,1] * AScale.Y;
  Result.M[1,1] := A.M[1,1] * AScale.Y;
  Result.M[2,1] := A.M[2,1] * AScale.Y;

  Result.M[0,2] := A.M[0,2] * AScale.Z;
  Result.M[1,2] := A.M[1,2] * AScale.Z;
  Result.M[2,2] := A.M[2,2] * AScale.Z;
end;

function FMatScale(const A: TsgAffineMatrix; const AScale: Double): TsgAffineMatrix;
begin
  Result := FMatScale(A, MakeFPoint(AScale, AScale, AScale));
end;

procedure AssignListToPos(ASource, ADest: TList; APos: Integer);
var
  I: Integer;
begin
  if (ASource = nil) or (ADest = nil) then
    Exit;
  for I := 0 to ASource.Count - 1 do
    if APos < 0 then
      ADest.Add(ASource[I])
    else
    begin
      ADest.Insert(APos, ASource[I]);
      Inc(APos);
    end;
end;

procedure LoadListNils(const AList: TList);
begin
  FillChar(AList.List[0], AList.Count shl 2, 0);
end;

function sgSign(const AVal: Double): Integer;
begin
  if AVal >= 0 then
    Result := 1
  else
    Result := -1;
end;

function sgSignZ(const AVal: Double; const AAccuracy: Double = fDoubleResolution): Integer;
begin
  Result := sgComparer.sgSignZ(AVal, AAccuracy);
end;

function sgArcSin(const AValue: Double): Double;
begin
  if AValue >= 1 then
    Result := cnstPiDiv2
  else
    if AValue <= -1 then
      Result := -cnstPiDiv2
    else
      Result := ArcSin(AValue);
end;

function sgArcTan2(const AX, AY: Extended): Double;
var
  vKatetX, vKatetY: Extended;
  vCmp: TPoint;
begin
  vKatetX := Abs(AX);
  vKatetY := Abs(AY);
  vCmp.X := Integer(AX < 0);
  vCmp.Y := Integer(AY < 0);
  if vKatetX < fDoubleResolution then
    Result := 90 + 180 * vCmp.Y
  else
  begin
    Result := f180DividedByPi * ArcTan(vKatetY / vKatetX);
    case vCmp.X shl 1 + vCmp.Y of
      1:  Result := 360 - Result;
      2:  Result := 180 - Result;
      3:  Result := 180 + Result;
    end;
  end
end;

function sgIsNan(const AValue: Double): Boolean;
begin
  Result := IsEqual(AValue, cnstNan);
end;

function sgFloorZ(const AValue: Double): Int64;
begin
  Result := sgConsts.sgFloorZ(AValue);
end;

function sgMod(const AValue, ABase: Double): Double;
begin
  Result := sgConsts.sgMod(AValue, ABase);
end;

function sgModAapproxim(const AValue, ABase: Double; AAapproxim: Double): Double;
begin
  Result := sgConsts.sgMod(AValue, ABase);
  if Result <> 0 then
  begin
    if IsEqual(Result, ABase, AAapproxim) or IsEqual(Result, 0, AAapproxim) then
      Result := 0;
  end;
end;

function sgModAngle(const AValue: Double; var AModValue: Double;
  const ARadian: Boolean = False): Boolean;
begin
  Result := sgConsts.sgModAngle(AValue, AModValue, ARadian);
end;

function sgPower10(AValue: Extended; APower: Integer): Extended;
begin
  Result :=
{$IFDEF SGDEL_XE2}
    Power10(AValue, APower)
{$ELSE}
    AValue * IntPower(10, APower)
{$ENDIF}
    ;
end;

function sgSqrt(const AValue: Extended): Double;
begin
  if AValue > fDoubleResolution then
    Result := Sqrt(AValue)
  else
    Result := 0;
end;

function GetSubPointByBox(const AP: TFPoint; const ABox: TFRect): TFPoint;
var
  vDelta: Double;
  vSize: TF2DPoint;
begin
  vSize := GetSizeFRect2D(ABox);
  vDelta := Max(vSize.X, vSize.Y);
  if vDelta < fDoubleResolution then
    vDelta := MaxSingle
  else
    vDelta := vDelta * 2;
  Result.X := AP.X + vDelta;
  Result.Y := AP.Y;
  Result.Z := 0;
end;

function PointCrossedPerpendicularLines(const P1, P2, P: TFPoint): TFPoint;
var
  K, B: Double;
begin
  if not IsEqual(P2.X, P1.X) then
  begin
    K := (P2.Y - P1.Y) / (P2.X - P1.X);
    B := P1.Y - K * P1.X;
    Result.X := (- B * K + P.Y * K + P.X) / (1 + K * K);
    Result.Y := K * Result.X + B;
    Result.Z := P.Z;
  end
  else
    Result := MakeFPoint(P1.X, P.Y, P1.Z);
end;

function PointCrossedPerpendicularSegments(const ALine: TsgLine; const APoint: TFPoint; const AC: PFPoint): Boolean;
var
  vPt: TFPoint;
begin
  vPt := PointCrossedPerpendicularLines(ALine.Point1, ALine.Point2, APoint);
  Result := IsPointInFRect2D(GetBoxOfLine2D(ALine), vPt);
  if AC <> nil then
    AC^ := vPt;
end;

function PointClassifyByArc(const Arc: TsgArcR; const APoint: TFPoint): TsgPointClassify;
var
  PS, PE: TFPoint;
  vRadius, vAngle: Double;
begin
  PS := GetPointOnCircle(Arc.Center, Arc.Radius, Arc.AngleS);
  if IsEqualFPoints2D(APoint, PS) then
    Result := pcORIGIN
  else
  begin
    PE := GetPointOnCircle(Arc.Center, Arc.Radius, Arc.AngleE);
    if IsEqualFPoints2D(APoint, PE) then
      Result := pcDESTINATION
    else
    begin
      vRadius := DistanceFPoint2D(Arc.Center, APoint);
      if IsEqual(Arc.Radius, vRadius) then
      begin
        vAngle := GetAngleByPoints(Arc.Center, APoint, False);
        if IsAngleInAnglesAP(vAngle, Arc) then
          Result := pcBETWEEN
        else
        begin
          if IsAngleInAngles(vAngle, 0, Arc.AngleS) then
            Result := pcBEYOND
          else
            Result := pcBETWEEN;
        end;
      end
      else
        if Arc.Radius < vRadius then
          Result := pcLEFT
        else
          Result := pcRIGHT;
    end;
  end;
end;


function PointClassifyEx(const ALine: TsgLine; const APoint: TFPoint): TsgPointClassify;
(*
var
  vDX, vDY, vLDX, vLDY, SA: Double;

  function GetLen(const AXD, ADY: Extended): Extended;
  begin
    Result := Sqr(AXD) + Sqr(ADY);
    if Result > fDoubleResolution then
      Result := Sqrt(Result)
    else
      Result := 0;
  end;
*)
begin
  Result := PointClassify(APoint, ALine.Point1, ALine.Point2);
(*
  vLDX := ALine.Point2.X - ALine.Point1.X;
  vLDY := ALine.Point2.Y - ALine.Point1.Y;
  vDX := APoint.X - ALine.Point1.X;
  vDY := APoint.Y - ALine.Point1.Y;
  SA := vLDX * vDY - vDX * vLDY;
  Result := pcBETWEEN;
  if sa > fDoubleResolution then
    Result := pcLEFT
  else
    if sa < -fDoubleResolution then
      Result := pcRIGHT
    else
      if (vLDX * vDX < 0.0) or (vLDY * vDY < 0.0) then
        Result := pcBEHIND
      else
        if GetLen(vLDX, vLDY) < GetLen(vDX, vDY) then
          Result := pcBEYOND
        else
          if IsEqualFPoints2D(ALine.Point1, APoint) then
            Result := pcORIGIN
          else
            if IsEqualFPoints2D(ALine.Point2, APoint) then
              Result := pcDESTINATION;
*)
end;

function PointDotProduct(const P1, P2 : TFPoint): TsgFloat;
begin
  Result := P1.V[0] * P2.V[0] + P1.V[1] * P2.V[1] + P1.V[2] * P2.V[2];
end;

function F2DPointClassify(const APoint, AStartPoint, AEndPoint: TF2DPoint): TsgPointClassify;
begin
  Result := PointClassify(MakeFPointFrom2D(APoint),
    MakeFPointFrom2D(AStartPoint), MakeFPointFrom2D(AEndPoint));
end;

function PerpendicularLine(const ALine: TsgLine; const APointOnLine: TFPoint): TsgLine;
var
  S, C: Extended;
  vDistance: Double;
begin
  SinCos(GetAngleByPoints(ALine.Point1, ALine.Point2, True), S, C);
  vDistance := DistanceFPoint(ALine.Point1, ALine.Point2);
  if vDistance < fDoubleResolution then
    vDistance := MaxSingle;
  Result.Point1 := APointOnLine;
  Result.Point2.X := APointOnLine.X + S * vDistance;
  Result.Point2.Y := APointOnLine.Y - C * vDistance;
  Result.Point2.Z := APointOnLine.Z;
end;


function MirrorFPointOfLine(const AP, AP1, AP2: TFPoint): TFPoint;
var
  vPtXY: TFPoint;
begin
  vPtXY := PointCrossedPerpendicularLines(AP1, AP2, AP);
  if IsEqualFPoints(vPtXY, AP) then
    Result := vPtXY
  else
  begin
    Result.X := 2 * vPtXY.X - AP.X;
    Result.Y := 2 * vPtXY.Y - AP.Y;
    Result.Z := AP.Z;
  end;
end;

function RotateFPointOfLine(const AP, AP1, AP2: TFPoint): TFPoint;
begin
  Result := RotateAroundFPoint(AP, AP1, False, GetAngleByPoints(AP1, AP2, True));
end;

function RotatePoint(const P: TPoint; const Angle: Extended): TPoint;
var
  S, C: Extended;
begin
  SinCos(Angle * fPiDividedBy180, S, C);
  Result.X := Round(C * P.X - S * P.Y);
  Result.Y := Round(S * P.X + C * P.Y);
end;

function ScaleFPointOfLine(const AP, APBase: TFPoint; const AScale: Double): TFPoint;
begin
  if AScale > fDoubleResolution then
  begin
    Result.X := APBase.X + AScale * (AP.X - APBase.X);
    Result.Y := APBase.Y + AScale * (AP.Y - APBase.Y);
    Result.Z := APBase.Z + AScale * (AP.Z - APBase.Z);
  end
  else
    Result := AP;
end;

procedure CheckMinValueFPoint2D(var APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution);
begin
  if (APoint.X = 0) or (APoint.Y = 0) then
  begin
    APoint.X := AAccuracy;
    APoint.Y := AAccuracy;
  end
  else
  begin
    if APoint.X > APoint.Y then
    begin
      if APoint.Y < AAccuracy then
      begin
        APoint.X := APoint.X * AAccuracy / APoint.Y;
        APoint.Y := AAccuracy;
      end;
    end
    else
    begin
      if APoint.X < AAccuracy then
      begin
        APoint.Y := APoint.Y * AAccuracy / APoint.X;
        APoint.X := AAccuracy;
      end;
    end;
  end;
end;

function SetAnglesCorrect(var Angle1, Angle2: Double; const AMiddle: Double;
  const Correct: Boolean): Boolean;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  if Angle1 > Angle2 then
  begin
    SwapSGFloats(Angle1, Angle2);
    Result := True;
  end
  else
    Result := False;
  if (AMiddle < Angle1) or (AMiddle > Angle2) then
  begin
    SwapSGFloats(Angle1, Angle2);
    Result := not Result;
  end;
  if Correct then
  begin
    if Angle1 > Angle2 then
      Angle1 := Angle1 - Round(Angle1 / 360) * 360;
    if Angle2 < Angle1 then
      Angle2 := Angle2 + 360;
  end;
end;

function SetAnglesByPoints(const ACenter, AP1, AP2, AP3: TFPoint;
  const Correct: Boolean; var Angle1, Angle2: Double): Boolean;
begin
  Angle1 := GetAngleByPoints(ACenter, AP1, False);
  Angle2 := GetAngleByPoints(ACenter, AP2, False);
  Result := SetAnglesCorrect(Angle1, Angle2, GetAngleByPoints(ACenter, AP3,
    False), Correct);
end;

procedure ExpandRect(var R: TRect; const P: TPoint);
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top > P.Y then
    R.Top := P.Y;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom < P.Y then
    R.Bottom := P.Y;
end;

procedure ExpandRect(var R: TRectF; const P: TPointF);
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top > P.Y then
    R.Top := P.Y;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom < P.Y then
    R.Bottom := P.Y;
end;

procedure ExpandFRect(var R: TFRect; const P: TFPoint);
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top < P.Y then
    R.Top := P.Y;
  if R.Z1 > P.Z then
    R.Z1 := P.Z;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom > P.Y then
    R.Bottom := P.Y;
  if R.Z2 < P.Z then
    R.Z2 := P.Z;
end;

procedure ExpandF2DRect(var R: TF2DRect; const P: TF2DPoint);
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top < P.Y then
    R.Top := P.Y;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom > P.Y then
    R.Bottom := P.Y;
end;

procedure ExpandFRect2D(var R: TFRect; const P: TFPoint); overload;
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top < P.Y then
    R.Top := P.Y;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom > P.Y then
    R.Bottom := P.Y;
end;

procedure ExpandFRect2D(var R: TFRect; const P: TF2DPoint); overload;
begin
  if R.Left > P.X then
    R.Left := P.X;
  if R.Top < P.Y then
    R.Top := P.Y;
  if R.Right < P.X then
    R.Right := P.X;
  if R.Bottom > P.Y then
    R.Bottom := P.Y;
end;

procedure ExpandPts4(var R: TsgPoints4; const P: TFPoint);
var
  vD, vP: TFPoint;
begin
  if Abs(P.X) > fAccuracy then
  begin
    vP := GetPointOnLine(R[0], R[1], -P.X);
    vD := SubFPoint(vP, R[0]);
    R[0] := vP;
    R[1] := SubFPoint(R[1], vD);
    R[2] := SubFPoint(R[2], vD);
    R[3] := AddFPoint(R[3], vD);
  end;
  if Abs(P.Y) > fAccuracy then
  begin
    vP := GetPointOnLine(R[0], R[3], -P.Y);
    vD := SubFPoint(vP, R[0]);
    R[0] := vP;
    R[1] := AddFPoint(R[1], vD);
    R[2] := SubFPoint(R[2], vD);
    R[3] := SubFPoint(R[3], vD);
  end;
end;

function ExtrutionFPoint(const APoint, AExtrusion: TFPoint): TFPoint;
begin
  Result := APoint;
  DoExtrusion(Result, AExtrusion);
end;

function PreExtrutionFPoint(const APoint, AExtrusion: TFPoint): TFPoint;
begin
  Result := APoint;
  DoPreExtrusion(Result, AExtrusion);
end;

function ScaleRect(const ARect: TRect; const AValue: Double): TRect;
var
  vWidth, vHeight: Integer;
  vKoef: Double;
begin
  Result := ARect;
  vWidth := ARect.Right - ARect.Left;
  vHeight := ARect.Bottom - ARect.Top;
  if (AValue <> 1) and (vWidth <> 0) and (vHeight <> 0) then
  begin
    vKoef := (AValue - 1) * 0.5;
    vWidth := Round(vWidth * vKoef);
    vHeight := Round(vHeight * vKoef);
    InflateRect(Result, vWidth, vHeight);
  end;
end;

function ScaleFRect2D(const ARect: TFRect; const AValue: Double): TFRect;
var
  vWidth, vHeight: Double;
  vKoef: Double;
begin
  Result := ARect;
  if (AValue <> 1) and (AValue <> 0) then
  begin
    vWidth := ARect.Right - ARect.Left;
    vHeight := ARect.Top - ARect.Bottom;
    if (vWidth <> 0) and (vHeight <> 0) then
    begin
      vKoef := (AValue - 1) * 0.5;
      vWidth := vWidth * vKoef;
      vHeight := vHeight * vKoef;
      InflateFRect(@Result, vWidth, vHeight, 0);
    end;
  end;
end;

procedure ArcExpandFRect2D(var R: TFRect; const Arc: TsgArcR; const UseAngles: Boolean);
begin
  if UseAngles then
  begin
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, Arc.AngleS));
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, Arc.AngleE));
  end;
  if IsAngleInAnglesAP(0, Arc) then
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, 0));
  if IsAngleInAnglesAP(90, Arc) then
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, 90));
  if IsAngleInAnglesAP(180, Arc) then
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, 180));
  if IsAngleInAnglesAP(270, Arc) then
    ExpandFRect2D(R, GetPointOnCircle(Arc.Center, Arc.Radius, 270));
end;

function IndexOfPts2D(const APts: TsgPoints4; const P: TFPoint): Integer;
begin
  Result := -1;
  if IsEqualFPoints2D(P, APts[0]) then
    Result := 0
  else
   if IsEqualFPoints2D(P, APts[1]) then
     Result := 1
   else
     if IsEqualFPoints2D(P, APts[2]) then
         Result := 2
     else
       if IsEqualFPoints2D(P, APts[3]) then
         Result := 3;
end;

function IndexOfPoint(const AList: TList;
  const APoint: Pointer; const AProc: TsgObjProcCompare): Integer;
begin
  Result := 0;
  while (Result < AList.Count) and (AProc(APoint, AList.List{$IFDEF LIST_PTR}^{$ENDIF}[Result]) <> 0) do
    Inc(Result);
  if Result >= AList.Count then
    Result := -1;
end;

function IndexOfFPoint(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Epsilon < 0 then
    Result := IndexOfFPointNearest(APoints, APoint, True, nil)
  else
  begin
    for I := 0 to APoints.FPointCount - 1 do
    begin
      if IsEqualFPoints(APoint, APoints.FPoints[I], Epsilon) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function IndexOfFPointNearest(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const ANearestOrFar: Boolean; const AFindPoint: PFPoint): Integer;
var
  I:  Integer;
  vDistance, vDist: Extended;
  vFind: Boolean;
begin
  Result := -1;
  vDistance := IfThen(ANearestOrFar, MaxDouble, -1);
  for I := 0 to APoints.FPointCount - 1 do
  begin
    vDist := DistanceFPointSqr(APoint, APoints.FPoints[I]);
    vFind := vDistance > vDist;
    if not ANearestOrFar then
      vFind := not vFind;
    if vFind then
    begin
      Result := I;
      vDistance := vDist;
      if ANearestOrFar and (vDistance <= 0) then
        Break;
    end;
  end;
  if (Result > -1) and (AFindPoint <> nil) then
   AFindPoint^ := APoints.FPoints[Result];
end;

function IndexOfArrayFPoint(const APoints: array of TFPoint;
  const APoint: TFPoint; const Epsilon: Double = fDoubleResolution): Integer;
var
  I: Integer;
  vDistance, vDist: Extended;
begin
  Result := -1;
  vDistance := MaxDouble;
  for I := Low(APoints) to High(APoints) do
  begin
    if Epsilon < 0 then
    begin
      vDist := DistanceFPointSqr(APoint, APoints[I]);
      if vDistance > vDist then
      begin
        Result := I;
        vDistance := vDist;
        if vDistance <= 0 then
          Break;
      end;
    end
    else
    begin
      if IsEqualFPoints(APoint, APoints[I], Epsilon) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure InflateFRect(APRect: PFRect; const ADX, ADY, ADZ: Double);
begin
  APRect^.Left := APRect^.Left - ADX;
  APRect^.Right := APRect^.Right + ADX;
  APRect^.Top := APRect^.Top + ADY;
  APRect^.Bottom := APRect^.Bottom - ADY;
  APRect^.Z1 := APRect^.Z1 - ADZ;
  APRect^.Z2 := APRect^.Z2 + ADZ;
end;

procedure sgInflateRect(var ARect: TRect; const ADX, ADY: Integer);
begin
  ARect.Left := SubInt32(ARect.Left, ADX);
  ARect.Top := SubInt32(ARect.Top, ADY);
  ARect.Right := AddInt32(ARect.Right, ADX);
  ARect.Bottom := AddInt32(ARect.Bottom, ADY);
end;

procedure InflateFRect(APRect: PFRect; APPoint: PFPoint);
begin
  InflateFRect(APRect, APPoint^.X, APPoint^.Y, APPoint^.Z);
end;

function IsBadRectF2D(const R: TF2DRect): Boolean;
begin
  Result := (R.Right < R.Left) or (R.Top < R.Bottom);
end;

function IsBadRectI(const R: TRect; const ALess: Boolean = False): Boolean;
begin
  if ALess then
    Result := (R.Right <= R.Left) or (R.Top >= R.Bottom)
  else
    Result := (R.Right < R.Left) or (R.Top > R.Bottom);
end;

function IsRectInRectF2D(const R1, R2: TF2DRect): Boolean;
begin
  Result := (R1.Left >= R2.Left) and (R1.Right <= R2.Right) and
            (R1.Top <= R2.Top) and (R1.Bottom >= R2.Bottom);
end;

function IsRectEmptyF2D(const R: TF2DRect): Boolean;
begin
  Result := IsBadRectF2D(R);
end;

function IsRectIntersectPolyPolygon(const ARect: TFRect;
  const APolygons: TList): Integer;
var
  p11,p12,p21,p22,l1,l2: TFPoint;
  vCnt,i,j: Integer;
  vCross: TFPoint;
  vPolygon: IsgArrayFPoint;
  vObj: TObject;
begin
  p11 := ARect.TopLeft;
  p22 := ARect.BottomRight;
  p12.X := p22.X;
  p12.Y := p11.Y;
  p21.X := p11.X;
  p21.Y := p22.Y;
  for j := 0 to APolygons.Count - 1 do
  begin
    vObj := TObject(APolygons.List{$IFDEF LIST_PTR}^{$ENDIF}[j]);
    if vObj is TF2DPointList then
      vPolygon := TF2DPointList(vObj)
    else
      vPolygon := TFPointList(vObj);
    vCnt := vPolygon.FPointCount - 2;
    if (IsPointInPolyline(vPolygon,p11) > 0) and (IsPointInPolyline(vPolygon,p12) > 0) and
      (IsPointInPolyline(vPolygon,p21) > 0) and (IsPointInPolyline(vPolygon,p22) > 0)
    then
    begin
      Result := 1;
      Exit;
    end;
    for i := 0 to vCnt do
    begin
      l1 := vPolygon.FPoints[i];
      l2 := vPolygon.FPoints[i+1];
      if IsCrossCoords(p11.X, p11.Y, p12.X, p12.Y, l1.X, l1.Y, l2.X, l2.Y, @vCross) = 2 then
      begin
        Result := 2;
        Exit;
      end;
      if IsCrossCoords(p12.X, p12.Y, p22.X, p22.Y, l1.X, l1.Y, l2.X, l2.Y, @vCross) = 2 then
      begin
        Result := 2;
        Exit;
      end;
      if IsCrossCoords(p22.X, p22.Y, p21.X, p21.Y, l1.X, l1.Y, l2.X, l2.Y, @vCross) = 2 then
      begin
        Result := 2;
        Exit;
      end;
      if IsCrossCoords(p21.X, p21.Y, p11.X, p11.Y, l1.X, l1.Y, l2.X, l2.Y, @vCross) = 2 then
      begin
        Result := 2;
        Exit;
      end;
    end;
  end;
  Result := 0;
end;

function IntersectBox(const ABox: TFRect; const AP1, AP2: TFPoint): Boolean;
var
  I: Integer;
  vLine: TsgLine;
begin
  Result := False;
  for I := 0 to 3 do
  begin
    vLine := GetLineOfFRect(ABox, I);
    if IsCrossSegmentsPts(AP1, AP2, vLine.Point1, vLine.Point2, nil) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IntersectBox(const ABox: TFRect; const ACenter: TFPoint; const ARadius, AngleS, AngleE: Double): Boolean;
var
  I: Integer;
  vLine: TsgLine;
begin
  Result := False;
  for I := 0 to 3 do
  begin
    vLine := GetLineOfFRect(ABox, I);
    if IsCrossArcAndSegmentCR(ACenter, ARadius, AngleS, AngleE, vLine, nil, nil) > 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IntersectBox(const ABox: TFRect; APoints: IsgArrayFPoint; const AClosed: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to APoints.FPointCount - 2 do
  begin
    if IntersectBox(ABox, APoints.FPoints[I], APoints.FPoints[I + 1]) then
    begin
      Result := True;
      Break;
    end;
  end;
  if (not Result) and AClosed and (APoints.FPointCount > 2) then
    Result := IntersectBox(ABox, APoints.FPoints[APoints.FPointCount - 1], APoints.FPoints[0]);
end;

function sgCompareDoubleAcc(const AValue1, AValue2: Double;
  const Epsilon: Double = 0): Integer;{$IFDEF SG_INLINE} inline;{$ENDIF}
var
  vDelta: Double;
begin
  vDelta := AValue1 - AValue2;
  if vDelta > Epsilon then
    Result := 1
  else
  begin
    if vDelta < -Epsilon then
      Result := -1
    else
      Result := 0
  end;
end;

function IntersectFRect2D(const R1, R2: TFRect;
  const Epsilon: Double = fDoubleResolution): Integer;
begin
  Result := -1;
  //  if (R1.Left <= R2.Right) and (R1.Right >= R2.Left) and (R1.Bottom <= R2.Top) and (R1.Top >= R2.Bottom) then
  if (sgCompareDoubleAcc(R1.Left, R2.Right, Epsilon) <= 0) and
     (sgCompareDoubleAcc(R1.Right, R2.Left, Epsilon) >= 0) and
     (sgCompareDoubleAcc(R1.Bottom, R2.Top, Epsilon) <= 0) and
     (sgCompareDoubleAcc(R1.Top, R2.Bottom, Epsilon) >= 0) then
  begin
    Result := 0;
   //    if (R1.Left <= R2.Left) and (R1.Right >= R2.Right) and (R1.Bottom <= R2.Bottom) and (R1.Top >= R2.Top) then
    if (sgCompareDoubleAcc(R1.Left, R2.Left, Epsilon) <= 0) and
       (sgCompareDoubleAcc(R1.Right, R2.Right, Epsilon) >= 0) and
       (sgCompareDoubleAcc(R1.Bottom, R2.Bottom, Epsilon) <= 0) and
       (sgCompareDoubleAcc(R1.Top, R2.Top, Epsilon) >= 0) then
      Result := 1;
  end;
end;

function IntersectFRect(const R1, R2: TFRect;
  const Epsilon: Double = fDoubleResolution): Integer;
begin
  Result := -1;
  if (sgCompareDoubleAcc(R1.Z1, R2.Z2, Epsilon) <= 0) and
     (sgCompareDoubleAcc(R1.Z2, R2.Z1, Epsilon) >= 0) then
  begin
    Result := IntersectFRect2D(R1, R2);
    if (Result = 1) and (not ((sgCompareDoubleAcc(R1.Z1, R2.Z1, Epsilon) <= 0) and
                              (sgCompareDoubleAcc(R1.Z2, R2.Z2, Epsilon) >= 0))) then
      Result := 0;
  end;
end;

function IntersectRectF2D(const R1, R2: TF2DRect; const AR: PF2DRect): Boolean;
var
  R: TF2DRect;
begin
  if R1.Left > R2.Left then
    R.Left := R1.Left
  else
    R.Left := R2.Left;

  if R1.Bottom > R2.Bottom then
    R.Bottom := R1.Bottom
  else
    R.Bottom := R2.Bottom;

  if R1.Right > R2.Right then
    R.Right := R2.Right
  else
    R.Right := R1.Right;

  if R1.Top > R2.Top then
    R.Top := R2.Top
  else
    R.Top := R1.Top;

  Result := not IsBadRectF2D(R);
  if AR <> nil then
    AR^ := R
end;

function IsArcClosed(const AStartAngle, AEndAngle: Double): Boolean;
begin
  Result := IsEqual(sgMod(AStartAngle, 360), sgMod(AEndAngle, 360));
end;

function IsArcClosedAR(const AArc: TsgArcR): Boolean;
begin
  Result := IsArcClosed(AArc.AngleS, AArc.AngleE);
end;

function GetNearestIndex(const AVaules: array of Double;
  const ALen: Integer; const AValue: Double): Integer;
var
  I: Integer;
  vDelta,vMin: Double;
begin
  Result := 0;
  vMin := MaxDouble;
  for I := 0 to ALen do
  begin
    vDelta := Abs(AVaules[I] - AValue);
    if vDelta < vMin then
    begin
      Result := I;
      vMin := vDelta;
    end;
  end;
end;

function IntToRomanNumerals(AValue: Cardinal; const IsExtended: Boolean): string;
const
  cnstDiginals: array [0..12] of Cardinal = (1000, 900,  500, 400, 100,    90,  50,   40,  10,    9,   5,    4,   1);
  cnstSymvols: array [0..12] of string =    ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I');

var
  vR: string;

  function GetNumerals(AValue: Cardinal): string;
  var
    I: Cardinal;
  begin
    Result := '';
    I := 0;
    while AValue > 0 do
    begin
      while cnstDiginals[I] <= AValue do
      begin
        Result := Result + cnstSymvols[I];
        Dec(AValue, cnstDiginals[I]);
      end;
      Inc(I);
    end;
    if Length(Result) < 1 then
      Result := '0';
  end;

begin
  Result := '';
  if IsExtended then
  begin
    if AValue < 1000 then
      Result := GetNumerals(AValue)
    else
    begin
      vR := IntToRomanNumerals(AValue div 1000, True);
      if vR <> '0' then
        Result := vR + ' M ';
      vR := IntToRomanNumerals(AValue mod 1000, True);
      if vR <> '0' then
        Result := Result + vR;
    end;
  end
  else
  begin
    if AValue < 4000 then
      Result := GetNumerals(AValue)
    else
      Result := IntToRomanNumerals(AValue, True);
  end;
end;

procedure InitInt64(const P: Pointer; var PBit: TsgInt64);
begin
  PBit.Lo := TsgNativeUInt(P) shl cnstAddressBit;
  PBit.Hi := TsgNativeUInt(P) shr cnstAddressHi;
end;

function InDeltaRange(AValue, ABase, ADelta: Double): Boolean;
begin
  Result := InRange(AValue, ABase - ADelta, ABase + ADelta);
end;

function Is3DRect(const ARect: TFRect): Boolean;
begin
  Result := GetBoxType(ARect) = bxXYZ;
end;

function IsBadFPoint(const APoint: TFPoint): Boolean;
begin
  Result := IsBadFPoint2D(APoint) and sgIsNan(APoint.Z);
end;

function IsBadFPoint2D(const APoint: TFPoint): Boolean;
begin
  REsult := sgIsNan(APoint.X) and sgIsNan(APoint.Y);
end;

function IsEvent(const AValue: Integer): Boolean;
begin
//  Result := AValue shr 1 shl 1 = AValue;
  Result := AValue mod 2 = 0;
end;

function IsEqualAngle(const AVal1, AVal2: TsgFloat;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := sgMod(Abs(AVal1 - AVal2), 360) < AAccuracy;
end;

function IsEqualFPointsClassify(const ALinePoint1, ALinePoint2,
  APoint1, APoint2: TFPoint): Boolean;
begin
  Result := PointClassify(APoint1, ALinePoint1, ALinePoint2) =
    PointClassify(APoint2, ALinePoint1, ALinePoint2)
end;

function IsEqualF2DPointsClassify(const ALinePoint1, ALinePoint2,
  APoint1, APoint2: TF2DPoint): Boolean;
begin
  Result := IsEqualFPointsClassify(MakeFPointFrom2D(ALinePoint1),
    MakeFPointFrom2D(ALinePoint2), MakeFPointFrom2D(APoint1),
    MakeFPointFrom2D(APoint2));
end;

function IsEqualPointClassify(const ALine: TsgLine;
  const APoint1, APoint2: TFPoint): Boolean;
begin
  Result := IsEqualFPointsClassify(ALine.Point1, ALine.Point2, APoint1, APoint2);
end;

function IsEqualPoints(const Point1, Point2: TPoint): Boolean;
begin
  Result := (Point1.X = Point2.X) and (Point1.Y = Point2.Y);
end;

function IsEqualPoints(const Point1, Point2: TPointF; Epsilon: Double = fAccuracy): Boolean;
begin
  Result := IsZero(Point1.X - Point2.X, Epsilon) and
    IsZero(Point1.Y - Point2.Y, Epsilon);
end;

function IsEqualFMatrix(const AM1,AM2: TFMatrix; const AIsWithE0: Boolean;
  Epsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsEqualFPoints(AM1.EX, AM2.EX, Epsilon) and
    IsEqualFPoints(AM1.EY, AM2.EY, Epsilon) and
    IsEqualFPoints(AM1.EZ, AM2.EZ, Epsilon);
  if AIsWithE0 and Result then
    Result := IsEqualFPoints(AM1.E0, AM2.E0, Epsilon);
end;

function IsEqualFPoints(const Point1, Point2: TFPoint;
  Epsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsZero(Point1.X - Point2.X, Epsilon) and
   IsZero(Point1.Y - Point2.Y, Epsilon) and IsZero(Point1.Z - Point2.Z, Epsilon);
end;

function IsEqualFPointsEps(const Point1, Point2: TFPoint; const AResolution: Double): Boolean;
begin
  Result := IsEqual(Point1.X, Point2.X, AResolution) and IsEqual(Point1.Y, Point2.Y, AResolution) and IsEqual(Point1.Z, Point2.Z, AResolution);
end;

function IsEqualFPoints2D(const Point1, Point2: TFPoint; Epsilon: Double = fDoubleResolution): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := fDoubleResolution;
  Result := IsRange(Point1.X - Point2.X,Epsilon) and IsRange(Point1.Y - Point2.Y,Epsilon);
end;

function IsEqualF2DPoints(const Point1, Point2: TF2DPoint; Epsilon: Double = fDoubleResolution): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := fDoubleResolution;
  Result := IsRange(Point1.X - Point2.X, Epsilon) and
    IsRange(Point1.Y - Point2.Y, Epsilon);
end;

function IsEqualFRects(const AR1, AR2: TFRect): Boolean;
begin
  Result := IsEqualFRects2D(AR1, AR2) and IsEqual(AR1.Z1, AR2.Z1) and
    IsEqual(AR1.Z2, AR2.Z2);
end;

function IsEqualFRects2D(const AR1, AR2: TFRect; Epsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsEqual(AR1.Left, AR2.Left, Epsilon) and IsEqual(AR1.Top, AR2.Top, Epsilon) and
    IsEqual(AR1.Right, AR2.Right, Epsilon) and IsEqual(AR1.Bottom, AR2.Bottom, Epsilon);
end;

function IsEqualDirection(const P1, P2, PB: TFPoint): Boolean;
var
  V1, V2: TFPoint;
begin
  V1 := SubFPoint2D(P1, PB);
  V2 := SubFPoint2D(P2, PB);
  Result := GetCosAngleOfVectors(V1, V2) > 0;
end;

function IsEqualColorCAD(const AValue1,  AValue2: TsgColorCAD): Boolean;
begin
  Result := (AVAlue1.Active = AVAlue2.Active) and (AValue1.Color = AValue2.Color);
end;

function IsUniquePointsIndex(const APoints: IsgArrayFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
var
  vRez: TPoint;
begin
  vRez := IsUniquePointsIndeses(APoints, True, Epsilon);
  Result := vRez.X;
end;

function IsUniquePointsIndeses(const APoints: IsgArrayFPoint;
  const ACheckFirstAndLastPoints: Boolean = True;
  const Epsilon: Double = fDoubleResolution): TPoint;
var
  I, J, vCnt: Integer;
  vPoint: TFPoint;
begin
  Result.X := -1;
  Result.Y := Result.X;
  vCnt := APoints.FPointCount;
  for I := 0 to vCnt - 2 do
  begin
    vPoint := APoints.FPoints[I];
    for J := I + 1 to vCnt - 1 do
    begin
      if IsEqualFPoints(vPoint, APoints.FPoints[J], Epsilon) then
      begin
        if ACheckFirstAndLastPoints or (not ((I = 0) and (J = vCnt - 1))) then
        begin
          Result.X := I;
          Result.Y := J;
          Break;
        end;
      end;
    end;
    if Result.X > -1 then
      Break;
  end;
end;

function IsUniquePoints(const APoints: IsgArrayFPoint;
  const Epsilon: Double = fDoubleResolution): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  vIndex := IsUniquePointsIndex(APoints, Epsilon);
  if vIndex < 0 then
    Result := True;
end;


function IsIndexColor(const AColor: Cardinal; APalett: PsgCADPalett = nil): Boolean;
var
  vColor: TsgColorCAD;
begin
  vColor := ConvertColortoColorCAD(AColor, APalett);
  Result := vColor.Active = acIndexColor;
end;

function IsEqualIndexColorAndRGB(const AIndexColor: Cardinal; AColor: TColor;
  APalett: PsgCADPalett = nil ): Boolean;
begin
  if APalett = nil then
    APalett := @arrDXFtoRGBColors;
  Result := APalett^[AIndexColor] = Cardinal(AColor);
end;

function IsEqualLists(const AL1, AL2: TList; Compare: TsgProcEqual = nil): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not ((AL1 = nil) xor (AL2 = nil)) then
  begin
    if AL1 <> nil then
    begin
      if AL1.Count = AL2.Count then
      begin
        I := 0;
        if Assigned(Compare) then
        begin
          while (I < AL1.Count) and (not Compare(AL1[I], AL2[I])) do
            Inc(I);
        end
        else
        begin
          while (I < AL1.Count) and (AL1[I] = AL2[I]) do
            Inc(I);
        end;
        Result := I = AL1.Count;
      end;
    end
    else
      Result := True;
  end;
end;

function IsEqualListsEnum(const AL1, AL2: TList; Compare: TsgProcEqual = nil): Boolean;
var
  I, J, L: Integer;
  vL2C: TList;
  vItem: Pointer;
begin
  Result := False;
  if not ((AL1 = nil) xor (AL2 = nil)) then
  begin
    if AL1 <> nil then
    begin
      if AL1.Count = AL2.Count then
      begin
        L := 0;
        if Assigned(Compare) then
        begin
          vL2C := TList.Create;
          try
            CopyLists(TList(vL2C), AL2);
            for I := 0 to AL1.Count - 1 do
            begin
              vItem := AL1[I];
              J := 0;
              while (J < vL2C.Count) and ((vL2C[J] = nil) or (not Compare(vItem, vL2C[J]))) do
                Inc(J);
              if J < vL2C.Count then
              begin
                vL2C[J] := nil;
                Inc(L);
              end
              else
                Break;
            end;
          finally
            vL2C.Free;
          end;
        end
        else
        begin
          for I := 0 to AL1.Count - 1 do
          begin
            if AL2.IndexOf(AL1[I])> -1 then
              Inc(L)
            else
              Break;
          end;
        end;
        Result := L = AL1.Count;
      end;
    end
    else
      Result := True;
  end;
end;

function IsEqualObjectListsEnum(const AL1, AL2: TsgObjectList; Compare: TsgProcObjectEqual = nil): Boolean;
var
  I, J, L: Integer;
  vL2C: TsgObjectList;
  vItem: Pointer;
begin
  Result := False;
  if not ((AL1 = nil) xor (AL2 = nil)) then
  begin
    if AL1 <> nil then
    begin
      if AL1.Count = AL2.Count then
      begin
        L := 0;
        if Assigned(Compare) then
        begin
          vL2C := TsgObjectList.Create;
          try
            vL2C.AssignList(AL2, loCopy);
            for I := 0 to AL1.Count - 1 do
            begin
              vItem := AL1[I];
              J := 0;
              while (J < vL2C.Count) and ((vL2C[J] = nil) or (not Compare(vItem, vL2C[J]))) do
                Inc(J);
              if J < vL2C.Count then
              begin
                vL2C[J] := nil;
                Inc(L);
              end
              else
                Break;
            end;
          finally
            vL2C.Free;
          end;
        end
        else
        begin
          for I := 0 to AL1.Count - 1 do
          begin
            if AL2.IndexOf(AL1[I])> -1 then
              Inc(L)
            else
              Break;
          end;
        end;
        Result := L = AL1.Count;
      end;
    end
    else
      Result := True;
  end;
end;

function IsEqualsgLines(ALine1, ALine2: TsgLine; Epsilon: Double = fDoubleResolution): Boolean;
begin
  Result :=
    ((IsEqualFPoints(ALine1.Point1, ALine2.Point1, Epsilon)) and (IsEqualFPoints(ALine1.Point2, ALine2.Point2, Epsilon))) or
    ((IsEqualFPoints(ALine1.Point1, ALine2.Point2, Epsilon)) and (IsEqualFPoints(ALine1.Point2, ALine2.Point1, Epsilon)));
end;

function IsEqualStringLists(const AStrings1, AStrings2: TStrings; ACaseSensitive: Boolean = False): Boolean;
var
  I: Integer;
  vS1, vS2: string;
begin
  I := 0;
  Result := AStrings1 = AStrings2;
  if Result or (AStrings1 = nil) or (AStrings2 = nil) then
    Exit;
  Result := AStrings1.Count = AStrings2.Count;
  while Result and (I < AStrings1.Count) do
  begin
    vS1 := AStrings1[I];
    vS2 := AStrings2[I];
    if not ACaseSensitive then
    begin
      vS1 := AnsiUpperCase(vS1);
      vS2 := AnsiUpperCase(vS2);
    end;
    Result := Result and (vS1 = vS2);
    Inc(I);
  end;
end;

function IsEqualCirclesAP(AArc1, AArc2: TsgArcR): Boolean;
begin
  Result := SameValue(AArc1.Radius, AArc2.Radius) and IsEqualFPoints(AArc1.Center, AArc2.Center);
end;

function IsEqualArcR(AArc1, AArc2: TsgArcR): Boolean;
begin
  Result :=
    SameValue(AArc1.Radius, AArc2.Radius) and
    SameValue(AArc1.AngleS, AArc2.AngleS) and
    SameValue(AArc1.AngleE, AArc2.AngleE) and
    IsEqualFPoints(AArc1.Center, AArc2.Center);
end;

function IsEqualEntClass(const AValue1, AValue2: TsgEntClass): Boolean;
begin
  Result := AValue1.ID = AValue2.ID;
end;

function IsValInParam(AVal, V1, V2: Double;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  if V1 > V2 then
    SwapDoubles(V1, V2);
  Result := ((AVal >= V1) or IsRange(AVal - V1, AAccuracy)) and
            ((AVal <= V2) or IsRange(V2 - AVal, AAccuracy));
end;

function IsValInParamI(const AVal, V1, V2: Integer): Boolean;
begin
  if V1 = V2 then
    Result := (AVal = V1)
  else
    if V1 < V2 then
      Result := (AVal >= V1) and (AVal <= V2)
    else
      Result := (AVal >= V2) and (AVal <= V1);
end;

function FindPreamble(ARaw: Pointer; ASize: Integer;
  const AIdents: array of AnsiString; AIndex: PInteger = nil): Boolean;
var
  I, L: Integer;
  S: AnsiString;
begin
  Result := False;
  if AIndex = nil then AIndex := @I;
  AIndex^ := Low(AIdents);
  while not Result and (AIndex^ <= High(AIdents)) do
  begin
    S := AIdents[AIndex^];
    L := Length(S);
    if (ASize > L) and CompareMem(Pointer(S), ARaw, L) then
      Inc(Result)
    else
      Inc(AIndex^);
  end;
  if not Result then
    AIndex^ := -1;
end;

function IsSatBinaryFile(P: Pointer; Size: Integer): Boolean;
begin
  Result := FindPreamble(P, Size, [cnstSatBinFileBeginIndent, cnstASMBinFileBeginIndent]);
end;

function IsValidEmail(const Value: string): Boolean;
  function CheckAllowed(const S: string): Boolean;
  var
    I: Integer;

  begin
    Result:= False;
    for I := 1 to Length(S) do
    begin
      {$IFDEF SGDEL_2009}
      if not CharInSet(S[I], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
      {$ELSE}
      if not (S[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
      {$ENDIF}
        Exit;
    end;
    Result:= True;
  end;

var
  I: Integer;
  NamePart, ServerPart: string;

begin
  Result := False;
  I := Pos(AnsiString('@'), Value);
  if I = 0 then
    Exit;
  NamePart := Copy(Value, 1, I - 1);
  ServerPart := Copy(Value, I + 1, Length(Value));
  if (Length(NamePart) = 0) or ((Length(ServerPart) < 4)) then
    Exit;

  I := Pos(AnsiString('.'), ServerPart);

  if (I = 0) or (I > (Length(ServerPart) - 2)) then
    Exit;

  if ServerPart[I + 1] = '.' then
   Exit;
  Result:= CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;


{$IFNDEF SGDEL_6}
function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Min(Abs(A), Abs(B)) * ExtendedResolution;
  Result := Abs(A - B) <= Epsilon;
end;

function SameValue(const A, B: Double; Epsilon: Double): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Min(Abs(A), Abs(B)) * DoubleResolution;
  Result := Abs(A - B) <= Epsilon;
end;

function SameValue(const A, B: Single; Epsilon: Single): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Min(Abs(A), Abs(B)) * SingleResolution;
  Result := Abs(A - B) <= Epsilon;
end;

function Sign(const AValue: Integer): TValueSign;
begin
  Result := ZeroValue;
  if AValue < 0 then
    Result := NegativeValue
  else if AValue > 0 then
    Result := PositiveValue;
end;

function Sign(const AValue: Int64): TValueSign;
begin
  Result := ZeroValue;
  if AValue < 0 then
    Result := NegativeValue
  else if AValue > 0 then
    Result := PositiveValue;
end;

function Sign(const AValue: Double): TValueSign;
begin
  if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000) then
    Result := ZeroValue
  else if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000) then
    Result := NegativeValue
  else
    Result := PositiveValue;
end;

function InRange(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Int64): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Double): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function IsZero(const A: Extended; Epsilon: Extended): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := ExtendedResolution;
  Result := Abs(A) <= Epsilon;
end;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Int64): Int64;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Double): Double;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function IsZero(const A: Single; Epsilon: Single): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := SingleResolution;
  Result := Abs(A) <= Epsilon;
end;

function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;
{$ENDIF}

function ExtendLinePtsByPoint(var ALine: TsgLine; const APoint: TFPoint;
  const ABoundPolygons: TList): Byte;
var
  vBoundPolygons: TsgObjectList;
begin
  vBoundPolygons := TsgObjectList.Create;
  try
    vBoundPolygons.CopyFrom(ABoundPolygons);
    Result := ExtendLinePtsByPointList(ALine, APoint, vBoundPolygons);
  finally
    vBoundPolygons.Free;
  end;
end;

function ExtendLinePtsByPointList(var ALine: TsgLine; const APoint: TFPoint;
  const ABoundPolygons: TsgObjectList): Byte;
const
  fExtendLineResolution = fDoubleResolution * 10;

var
  I, J: Integer;
  vLine1, vLine2: TsgLine;
  vPoints: TF2DPointList;
  vLinerMiddle, vCross: TFPoint;
  vBase, vActivePoint: PFPoint;
  vDistanceMin, vDistance, vLength: Extended;
  vCrossPoints: TFPointList;
  vAccuracy1, vAccuracy2: Double;
begin
  Result := 0;
  vLine1 := ALine;
  vAccuracy1 := GetAccuracy(DistanceLine2D(vLine1), fExtendLineResolution);
  vCrossPoints := TFPointList.Create;
  try
    vCrossPoints.Duplicates := dupIgnore;
    vCrossPoints.Sorted := True;
    vActivePoint := @vLine1.Point2;
    if DistanceFpointSqr(vLine1.Point1, APoint) < DistanceFpointSqr(vLine1.Point2, APoint) then
      vActivePoint := @vLine1.Point1;
    vLinerMiddle := MiddleFPoint(vLine1.Point1, vLine1.Point2);
    for I := 0 to ABoundPolygons.Count - 1 do
    begin
      vPoints := TF2DPointList(ABoundPolygons[I]);
      for J := 0 to vPoints.Count - 2 do
      begin
        vLine2.Point1 := MakeFPointFrom2D(vPoints[J]);
        vLine2.Point2 := MakeFPointFrom2D(vPoints[J + 1]);
        if IsCrossLines(vLine1, vLine2, @vCross) then
        begin
          if IsEqualDirection(vActivePoint^, vCross, vLinerMiddle) then
          begin
            if not IsPointOnSegment(vLine1, vCross, vAccuracy1) then
            begin
              vAccuracy2 := GetAccuracy(DistanceLine2D(vLine2), fExtendLineResolution);
              if IsPointOnSegment(vLine2, vCross, vAccuracy2) then
                vCrossPoints.Add(vCross);
            end;
          end;
        end;
      end;
    end;
    if vCrossPoints.Count > 0 then
    begin
      Result := IfThen(vActivePoint = @vLine1.Point2, 2, 1);
      vBase := @ALine.Point1;
      vActivePoint := @ALine.Point2;
      if Result = 1 then
      begin
        vBase := @ALine.Point2;
        vActivePoint := @ALine.Point1;
      end;
      J := -1;
      vCross := cnstFPointZero;
      vDistanceMin := MaxDouble;
      vLength := DistanceFPointSqr(ALine.Point1, ALine.Point2);
      for I := 0 to vCrossPoints.Count - 1 do
      begin
        vDistance := DistanceFPointSqr(vBase^, vCrossPoints[I]);
        if (vDistance > vLength) and (vDistance < vDistanceMin) then
        begin
          J := I;
          vCross := vCrossPoints[I];
          vDistanceMin := vDistance;
        end;
      end;
      if J > -1 then
        vActivePoint^ := vCross
      else
        Result := 0;
    end;
  finally
    vCrossPoints.Free;
  end;
end;

function ExtendArcPtsByPoint(var AArc: TsgArcR; const APoint: TFPoint;
  const ABoundPolygons: TList): Byte;
var
  vBoundPolygons: TsgObjectList;
begin
  vBoundPolygons := TsgObjectList.Create;
  try
    vBoundPolygons.CopyFrom(ABoundPolygons);
    Result := ExtendArcPtsByPointList(AArc, APoint, vBoundPolygons);
  finally
    vBoundPolygons.Free;
  end;
end;

function ExtendArcPtsByPointList(var AArc: TsgArcR; const APoint: TFPoint;
  const ABoundPolygons: TsgObjectList): Byte;

  function GetAngleDelta(AStart, AEnd: Double): Double;
  begin
    Result := AEnd - AStart;
    if Result < 0 then
      Result := Result + 360;
  end;

var
  I, J, C, vCrossCnt: Integer;
  vArc: TsgArcR;
  vLine: TsgLine;
  vPoints: TF2DPointList;
  vLinerMiddle: TFPoint;
  vDistanceMin, vDistance, vLength: Extended;
  vCrossPoints: TFPointList;
  vArcPointS, vArcPointE, vPointNormal, vCrossTest: TFPoint;
  vCross: array [0..1] of TFPoint;
  vAngle, vAngleTest: Double;
  vActivePoint: PFPoint;
begin
  Result := 0;
  vArc := AArc;
  vCrossPoints := TFPointList.Create;
  try
    vCrossPoints.Duplicates := dupIgnore;
    vCrossPoints.Sorted := True;

    vArcPointS := GetPointOnArcAR(vArc, vArc.AngleS);
    vArcPointE := GetPointOnArcAR(vArc, vArc.AngleE);
    vPointNormal := PointCrossedPerpendicularLines(vArcPointS, vArcPointE, APoint);

    vActivePoint := @vArcPointS;
    if DistanceFpointSqr(vArcPointE, APoint) < DistanceFpointSqr(vArcPointS, APoint) then
      vActivePoint := @vArcPointE;
    vLinerMiddle := MiddleFPoint(vArcPointS, vArcPointE);

    for I := 0 to ABoundPolygons.Count - 1 do
    begin
      vPoints := TF2DPointList(ABoundPolygons[I]);
      for J := 0 to vPoints.Count - 2 do
      begin
        vLine.Point1 := MakeFPointFrom2D(vPoints[J]);
        vLine.Point2 := MakeFPointFrom2D(vPoints[J + 1]);
        vCrossCnt := IsCrossCircleAndLineAP(vArc, vLine, @vCross[0], @vCross[1]);
        C := 0;
        while C < vCrossCnt do
        begin
          vCrossTest := PointCrossedPerpendicularLines(vArcPointS, vArcPointE, vCross[C]);
          //if IsEqualDirection(vActivePoint^, vCrossTest, vLinerMiddle) then
          begin
            if IsPointOnSegment(vLine, vCross[C], 0)  then
            begin
              vAngle := GetAngleByPoints(vArc.Center, vCross[C], False);
              if not IsAngleInAngles(vAngle, vArc.AngleS, vArc.AngleE) then
                vCrossPoints.Add(vCross[C]);
            end;
          end;
          Inc(C);
        end;
      end;
    end;
    if vCrossPoints.Count > 0 then
    begin
      Result := IfThen(vActivePoint = @vArcPointS, 1, 2);
      J := -1;
      vAngleTest := 0;
      vDistanceMin := MaxDouble;
      vLength := GetAngleDelta(AArc.AngleS, AArc.AngleE);
      for I := 0 to vCrossPoints.Count - 1 do
      begin
        vAngle := GetAngleByPoints(vArc.Center, vCrossPoints[I], False);
        if Result = 1 then
        begin
//          if not IsAngleInAngles(vArc.AngleS, vAngle, vArc.AngleE) then
//            vAngle := vAngle + 180;
          vArc.AngleS := vAngle;
        end
        else
        begin
//          if not IsAngleInAngles(vArc.AngleE, vArc.AngleS, vAngle) then
//            vAngle := vAngle + 180;
          vArc.AngleE := vAngle;
        end;
        vDistance := GetAngleDelta(vArc.AngleS, vArc.AngleE);
        if (vDistance > vLength) and (vDistance < vDistanceMin) then
        begin
          J := I;
          vAngleTest := vAngle;
          vDistanceMin := vDistance;
        end;
      end;
      if J > -1 then
      begin
        if Result = 1 then
          AArc.AngleS := vAngleTest
        else
          AArc.AngleE := vAngleTest;
      end
      else
        Result := 0;
    end;
  finally
    vCrossPoints.Free;
  end;
end;

function TrimmingLinePts(const ALine: TsgLine; const APts: TsgPoints4; const ACutLine: PsgLine = nil): TsgLine;
var
  I, vCrossCount: Integer;
  vBox: TFRect;
  vEdge: TsgLine;
  vCrossPoint: TFPoint;
begin
  Result := ALine;
  vBox := GetBoxOfPts2D(APts);
  vCrossCount := 0;
  for I := 0 to 3 do
  begin
    vEdge.Point1 := APts[I];
    if I = 3 then
      vEdge.Point2 := APts[0]
    else
      vEdge.Point2 := APts[I + 1];
    if IsCrossSegments(Result, vEdge, @vCrossPoint) then
    begin
      if not (IsEqualFPoints(Result.Point1, vCrossPoint) or
              IsEqualFPoints(Result.Point2, vCrossPoint)) then
      begin
        if IsPointInRectangle(APts, Result.Point1, @vBox) <> 0 then
          Result.Point2 := vCrossPoint
        else
          Result.Point1 := vCrossPoint;
        Inc(vCrossCount);
        if vCrossCount > 1 then
          Break;
      end;
    end;
  end;
end;

procedure TrimmingLineRect(var AP1, AP2: TPoint; const ARect: TRect);
var
  vLine, vTrimLine: TsgLine;
  vPts: TsgPoints4;
begin
  vLine.Point1 := MakeFPoint(AP1.X, AP1.Y);
  vLine.Point2 := MakeFPoint(AP2.X, AP2.Y);
  vPts[0] := MakeFPoint(ARect.Left, ARect.Top);
  vPts[1] := MakeFPoint(ARect.Right, ARect.Top);
  vPts[2] := MakeFPoint(ARect.Right, ARect.Bottom);
  vPts[3] := MakeFPoint(ARect.Left, ARect.Bottom);
  vTrimLine := TrimmingLinePts(vLine, vPts);
  AP1.X := Round(vTrimLine.Point1.X);
  AP1.Y := Round(vTrimLine.Point1.Y);
  AP2.X := Round(vTrimLine.Point2.X);
  AP2.Y := Round(vTrimLine.Point2.Y);
end;

function IsHandleStr(const AStr: string): Boolean;
begin
  Result := False;
  if (Length(AStr) > 0) and (AStr[1] = '$')  then
    Result := True;
end;

function sgSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;

function InvExtrusionToMatrix(const P: TFPoint): TFMatrix;
begin
  Result := cnstIdentityMat;
  DoPreExtrusion(Result.EX, P);
  DoPreExtrusion(Result.EY, P);
  DoPreExtrusion(Result.EZ, P);
end;

procedure MakePlane(const AP1, AP2, AP3: TFPoint; var APlane: TFPoint; ALenSgr: PExtended = nil);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  APlane := Vector(SubFPoint(AP2, AP1), SubFPoint(AP3, AP1));
  if ALenSgr <> nil then
    ALenSgr^ := Sqr(APlane.X) + Sqr(APlane.Y) + Sqr(APlane.Z);
end;

function sgCalcPlaneNormal(const AP1, AP2, AP3: TFPoint): TFPoint;
var
  vLen: Extended;
begin
  MakePlane(AP1, AP2, AP3, Result, @vLen);
  if not IsZero(vLen) then
  begin
    vLen := Sqrt(vLen);
    Result.X := Result.X / vLen;
    Result.Y := Result.Y / vLen;
    Result.Z := Result.Z / vLen;
  end;
end;

function sgAreaOfTriangle(const AP1, AP2, AP3: TFPoint): Double;
var
  V: TFPoint;
  vLen: Extended;
begin
  MakePlane(AP1, AP2, AP3, V, @vLen);
  if not IsZero(vLen) then
    Result := 0.5 * Sqrt(vLen)
  else
    Result := 0.0;
end;

function sgPointLineClosestPoint(const APoint, APoint1, APoint2: TFPoint): TFPoint;
var
  vDir: TFPoint;
  W : TFPoint;
  C1, C2, B: Double;
begin
  W:= SubFPoint(APoint, APoint1);
  vDir := SubFPoint(APoint2, APoint1);

  C1 := PointDotProduct(W, vDir);
  C2 := PointDotProduct(vDir, vDir);
  B:= C1/C2;

  Result := AddFPoint(APoint1, PtXScalar(vDir, B));
end;

procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TFPoint;
  var Segment0Closest, Segment1Closest : TFPoint);
const
  cSMALL_NUM = 0.000000001;
var
  u, v,w : TFPoint;
  a,b,c,smalld,e, largeD, sc, sn, sD, tc, tN, tD : double;
begin
  u :=  SubFPoint(S0Stop, S0Start);
  v :=  SubFPoint(S1Stop, S1Start);
  w :=  SubFPoint(S0Start, S1Start);

  a := PointDotProduct(u,u);
  b := PointDotProduct(u,v);
  c := PointDotProduct(v,v);
  smalld := PointDotProduct(u,w);
  e := PointDotProduct(v,w);
  largeD := a*c - b*b;

  sD := largeD;
  tD := largeD;

  if LargeD<cSMALL_NUM then
  begin
    sN := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end else
  begin
    sN := (b*e - c*smallD);
    tN := (a*e - b*smallD);
    if (sN < 0.0) then
    begin
      sN := 0.0;
      tN := e;
      tD := c;
    end
    else if (sN > sD) then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if (tN < 0.0) then
  begin
      tN := 0.0;
      // recompute sc for this edge
      if (-smalld < 0.0) then
          sN := 0.0
      else if (-smalld > a) then
          sN := sD
      else
      begin
          sN := -smalld;
          sD := a;
      end;
  end
  else if (tN > tD) then
  begin
      tN := tD;
      // recompute sc for this edge
      if ((-smallD + b) < 0.0) then
          sN := 0
      else if ((-smallD + b) > a) then
          sN := sD
      else
      begin
          sN := (-smallD + b);
          sD := a;
      end;
   end;

  // finally do the division to get sc and tc
  //sc := (abs(sN) < SMALL_NUM ? 0.0 : sN / sD);
  if abs(sN) < cSMALL_NUM then
    sc := 0
  else
    sc := sN/sD;

  //tc := (abs(tN) < SMALL_NUM ? 0.0 : tN / tD);
  if abs(tN) < cSMALL_NUM then
    tc := 0
  else
    tc := tN/tD;

  // get the difference of the two closest points
  //Vector   dP = w + (sc * u) - (tc * v);  // = S0(sc) - S1(tc)

  Segment0Closest := AddFPoint(S0Start, PtXScalar(u, sc));
  Segment1Closest := AddFPoint(S1Start, PtXScalar(v, tc));
end;

function sgSegmentSegmentDistance(const AP0Start, AP0Stop,
  AP1Start, AP1Stop : TFPoint) : double;
var
  Pb0, PB1 : TFPoint;
begin
  SegmentSegmentClosestPoint(AP0Start, AP0Stop, AP1Start, AP1Stop, PB0, PB1);
  result := DistanceFVector(SubFPoint( PB1, PB0));
end;

function CalcCircleParams(const APoint1, APoint2, APoint3: TFPoint;
  var ACenter: TFPoint; var ARadiaus: Double): Boolean;
var
  vPoint1, vPoint2, vPoint3: TFPoint;
  vNormal: TFPoint;
  vMatrix: TFMatrix;
begin
  Result := False;
  vNormal := sgCalcPlaneNormal(APoint1, APoint2, APoint3);
  vMatrix := InvExtrusionToMatrix(vNormal);
  vPoint1 := AffineTransformPoint(APoint1, vMatrix);
  vPoint2 := AffineTransformPoint(APoint2, vMatrix);
  vPoint3 := AffineTransformPoint(APoint3, vMatrix);
  if GetCircleParams(vPoint1, vPoint2, vPoint3, ACenter, ARadiaus) then
  begin
    Result := True;
    ACenter := AffineTransformPoint(ACenter, FMatTranspose(vMatrix));
  end;
end;


function CalcCircleParams(const AList: TFPointList; var ACenter: TFPoint;
  var ARadiaus: Double; const AAccuracy: Double = 1E-03): Boolean;
var
  I, vValCount: Integer;
  vCenter, vPrevCenter, vAverageCenter: TFPoint;
  vRadius, vPrevRadius, vAverageRadius: Double;
  vTempList: TFPointList;
begin
  Result := True;
  vPrevRadius := 0;
  if AList.Count >= 6 then
  begin
    vTempList := TFPointList.Create;
    try
      vTempList.Capacity := AList.Count;
      vTempList.Add(AList[0]);
      for I := 1 to AList.Count - 1 do
      begin
        if not IsEqualFPoints(vTempList.Last, AList[I]) then
          vTempList.Add(AList[I]);
      end;

      vAverageCenter := cnstFPointZero;
      vAverageRadius := 0;
      vValCount := 0;
      for I := 1 to vTempList.Count - 4 do
      begin
        if CalcCircleParams(vTempList[I], vTempList[I+1], vTempList[I+2],
             vCenter, vRadius) then
        begin
          if (I = 1) or (IsEqualFPoints(vCenter, vPrevCenter, AAccuracy) and
             IsEqual(vRadius, vPrevRadius, AAccuracy)) then
          begin
            vPrevCenter := vCenter;
            vPrevRadius := vRadius;
            vAverageCenter := AddFPoint(vAverageCenter, vCenter);
            vAverageRadius := vAverageRadius + vRadius;
            Inc(vValCount);
          end
          else
          begin
            Result := False;
            Exit;
          end;
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      vTempList.Free;
    end;
    vAverageCenter.X := vAverageCenter.X / vValCount;
    vAverageCenter.Y := vAverageCenter.Y / vValCount;
    vAverageCenter.Z := vAverageCenter.Z / vValCount;
    vAverageRadius := vAverageRadius / vValCount;
    ACenter := vAverageCenter;
    ARadiaus := vAverageRadius;
  end
  else
  Result := False;
end;

procedure CalcPlaneParams(const Points: TFPointList; var ACenter, ANormal: TFPoint);
var
  I, N: Integer;
  Sum, R: TFPoint;
  Centr, Dir: TFPoint;
  xx, yy, xy, yz, xz, zz: TsgFloat;
  det_x, det_y, det_z, det_max: TsgFloat;
begin
  N := Points.Count;
  Sum := cnstFPointZero;
  for I := 0 to Points.Count - 1 do
    Sum := AddFPoint(Sum, Points[I]);
  Centr := PtXScalar(Sum, 1.0/N);
  xx := 0.0; xy := 0.0; xz := 0.0;
  yy := 0.0; yz := 0.0; zz := 0.0;
  for I := 0 to Points.Count - 1 do
  begin
    R := SubFPoint(Points[I], Centr);
    xx := xx + r.x * r.x;
    xy := xy + r.x * r.y;
    xz := xz + r.x * r.z;
    yy := yy + r.y * r.y;
    yz := yz + r.y * r.z;
    zz := zz + r.z * r.z;
  end;

  det_x := yy*zz - yz*yz;
  det_y := xx*zz - xz*xz;
  det_z := xx*yy - xy*xy;

  det_max := Max(det_x, Max(det_y, det_z));

  if det_max = det_x then
    Dir := MakeFPoint(det_x, xz*yz - xy*zz, xy*yz - xz*yy)
  else if det_max = det_y then
    Dir := MakeFPoint(xz*yz - xy*zz, det_y, xy*xz - yz*xx)
  else Dir := MakeFPoint(xy*yz - xz*yy, xy*xz - yz*xx, det_z);

  ACenter := Centr;
  ANormal := Dir;
end;

function sgIsZero(const AVal: Double;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  if AVal < 0 then
   Result := AVal > -AAccuracy
  else
   Result := AVal < AAccuracy;
end;

function IsRange(const AVal, ARange: Double): Boolean;
begin
  if AVal < 0 then
    Result := AVal > -ARange
  else
    Result := AVal < ARange;
end;

{$IFNDEF SG_NO_EXTENDED}
function IsRange(const AVal, ARange: Extended): Boolean;
begin
  if AVal < 0 then
    Result := AVal > -ARange
  else
    Result := AVal < ARange;
end;
{$ENDIF}

function IsRangeFPoints(const AP1, AP2, ARange: TFPoint): Boolean;
begin
  Result := IsRange(AP1.X - AP2.X, ARange.X) and
    IsRange(AP1.Y - AP2.Y, ARange.Y) and IsRange(AP1.Z - AP2.Z, ARange.Z);
end;

function IsRangeIncBoundaries(const AVal, ARange: Double): Boolean;
begin
  if AVal < 0 then
   Result := AVal >= -ARange
  else
   Result := AVal <= ARange;
end;

function IsRectangle(const APoints: IsgArrayFPoint; const AClosed: Boolean;
  const ACheckRotated: Boolean; const Accuracy: Double = fDoubleResolution): Boolean;
var
  I, J: Integer;
  vPts: TsgPoints4;
  vRez: Integer;
begin
  Result := False;
  if (APoints <> nil) and ((APoints.FPointCount = 5) or ((APoints.FPointCount = 4) and AClosed)) then
  begin
    vPts[0] := APoints.FPoints[0];
    J := 1;
    for I := 1 to APoints.FPointCount - 1 do
    begin
      vPts[J] := APoints.FPoints[I];
      if not IsEqualFPoints(vPts[J - 1], vPts[J], Accuracy) then
      begin
        Inc(J);
        if J > 3 then
          Break;
      end;
    end;
    vRez := 0;
    if J > 3 then
      vRez := IsRectanglePts4(vPts, Accuracy);
    if ACheckRotated then
      Result := vRez > 0
    else
      Result := vRez = 1;
  end;
end;

function IsRectanglePts4(const APoints: TsgPoints4;
  const Accuracy: Double = fDoubleResolution): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vIndexX, vIndexY: Integer;
  vDist1, vDist2: Extended;
  vAngle: Double;
begin
  Result := 0;
  vIndexX := -1;
  vIndexY := -1;
  if IsEqual(APoints[0].Y, APoints[1].Y, Accuracy) then
  begin
    vIndexX := 0;
    vIndexY := 1;
  end
  else
    if IsEqual(APoints[0].X, APoints[1].X, Accuracy) then
    begin
      vIndexX := 1;
      vIndexY := 0;
    end
    else
    begin
      vDist1 := DistanceFPointSqr(APoints[0], APoints[1]);
      vDist2 := DistanceFPointSqr(APoints[2], APoints[3]);
      if IsEqual(vDist1, vDist2, Accuracy) then
      begin
        vDist1 := DistanceFPointSqr(APoints[0], APoints[3]);
        vDist2 := DistanceFPointSqr(APoints[1], APoints[2]);
        if IsEqual(vDist1, vDist2, Accuracy) then
        begin
          if IsParalleniarLinesPts(APoints[0], APoints[1], APoints[3], APoints[2], Accuracy) then
          begin
            if IsParalleniarLinesPts(APoints[0], APoints[3], APoints[1], APoints[2], Accuracy) then
            begin
              vAngle := GetAngleOfLinesEx(APoints[0], APoints[3], APoints[1]);
              if IsEqualAngle(vAngle, 90, Accuracy) then
                Result := 3;
            end;
          end;
        end;
      end;
    end;
  if vIndexX > -1 then
  begin
    if IsEqual(APoints[1].V[vIndexX], APoints[2].V[vIndexX], Accuracy) and
       IsEqual(APoints[2].V[vIndexY], APoints[3].V[vIndexY], Accuracy) and
       IsEqual(APoints[3].V[vIndexX], APoints[0].V[vIndexX], Accuracy) and
       (not IsEqual(APoints[0].V[vIndexX], APoints[1].V[vIndexX], Accuracy)) and
       (not IsEqual(APoints[1].V[vIndexY], APoints[2].V[vIndexY], Accuracy)) then
    begin
      Result := 1 + vIndexX;
    end;
  end;
end;

function IsPointInF2DRect(const R: TF2DRect; const P: TF2DPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsValInParam(P.X, R.Left, R.Right, AAccuracy) and
    IsValInParam(P.Y, R.Top, R.Bottom, AAccuracy);
end;

function IsPointInFRect2D(const R: TFRect; const P: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsValInParam(P.X, R.Left, R.Right, AAccuracy) and
    IsValInParam(P.Y, R.Top, R.Bottom, AAccuracy);
end;

function IsPointInRectI(const R: TRect; const P: TPoint): Boolean;
begin
  Result := IsValInParamI(P.X, R.Left, R.Right) and IsValInParamI(P.Y, R.Top, R.Bottom);
end;

function IsPointInFRect(const R: TFRect; const P: TFPoint): Boolean;
begin
  Result := IsPointInFRect2D(R, P) and IsValInParam(P.Z, R.Z1, R.Z2)
end;

function IsPointOnCircle(const APC, AP1, APoint: TFPoint): Boolean;
var
  V1, V2: Extended;
begin
  V1 := DistanceFPoint2DSqr(AP1, APC);
  V2 := DistanceFPoint2DSqr(APoint, APC);;
  Result := IsEqual(V1, V2);
end;

function IsPointOnCircleCR(const ACenter: TFPoint; const ARadius: Double; const APoint: TFPoint): Boolean;
begin
  Result := IsEqual(DistanceFPoint2D(ACenter, APoint),  ARadius);
end;

function IsPointOnArc(const APC, AP1, AP2, APoint: TFPoint): Boolean;
begin
  Result := IsPointOnCircle(APC, AP1, APoint);
  if Result then
  begin
    Result := IsAngleInAngles(GetAngleByPoints(APC, APoint, False), GetAngleByPoints(APC, AP1, False),
      GetAngleByPoints(APC, AP2, False));
  end;
end;

function IsPointOnArcs(const ArcPts1, ArcPts2: TsgPoints3; const APoint: TFPoint): Boolean;
begin
  Result := IsPointOnArc(ArcPts1[0], ArcPts1[1], ArcPts1[2], APoint) and
    IsPointOnArc(ArcPts2[0], ArcPts2[1], ArcPts2[2], APoint);
end;

function IsPointOnArcAP(const Arc: TsgArcR; const APoint: TFPoint; AEpsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsPointOnArcCR(Arc.Center, Arc.Radius, Arc.AngleS, Arc.AngleE, APoint, AEpsilon);
end;

function IsPointOnArcCR(const APC: TFPoint; const R, A1, A2: Double;
  const APoint: TFPoint; AEpsilon: Double = fDoubleResolution): Boolean;
var
  vD: Double;
begin
  vD := DistanceFPoint2D(APC, APoint);
  Result := IsEqual(R, vD, AEpsilon) and IsAngleInAngles(GetAngleByPoints(APC, APoint, False), A1, A2, AEpsilon);
end;

function IsPointOnLine(const ALine: TsgLine; const APoint: TFPoint): Boolean;
begin
  Result := IsPointOnLinePts(ALine.Point1, ALine.Point2, APoint);
end;

function IsPointOnLineI(const AP1, AP2, APoint: TPoint;
  const AAccuracy: Double = 1): Boolean;
begin
  Result := IsPointOnLinePts(MakeFPoint(AP1.X, AP1.Y),
    MakeFPoint(AP2.X, AP2.Y), MakeFPoint(APoint.X, APoint.Y), AAccuracy);
end;

function IsPointOnLinePts(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = 0): Boolean;
begin
  Result := not (PointClassify(APoint, AP1, AP2, AAccuracy) in [pcLEFT, pcRIGHT]);
end;

function IsPointOnLinePts3D(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
var
  vV1, vV2: TFPoint;
  vAngle: Double;
begin
  Result := False;
  vV1 := SubFPoint(AP2, AP1);
  vV2 := SubFPoint(APoint, AP1);
  vAngle := GetAngleOfVectors(vV1, vV2, False, True);
  if (vAngle <= AAccuracy) or (vAngle >= 180 - AAccuracy) then
    Result := True;
end;

function IsPointOnSegment(const ALine: TsgLine; const APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsPointOnSegmentPts(ALine.Point1, ALine.Point2, APoint, AAccuracy);
end;

function IsPointOnSegmentI(const AP1, AP2, APoint: TPoint): Boolean;
begin
  if IsPointInRectI(Rect(AP1.X, AP1.Y, AP2.X, AP2.Y), APoint) then
    Result := IsPointOnLineI(AP1, AP2, APoint)
  else
    Result := False;
end;

function IsPointOnSegmentPts(const AP1, AP2, APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
var
  vBox: TFRect;
begin
  vBox.TopLeft := AP1;
  vBox.BottomRight := vBox.TopLeft;
  ExpandFRect2D(vBox, AP2);
  if IsPointInFRect2D(vBox, APoint, AAccuracy) then
    Result := IsPointOnLinePts(AP1, AP2, APoint, AAccuracy)
  else
    Result := False;
end;

function IsPointOnSegmentPts2D(const AP1, AP2, APoint: TF2DPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
var
  vBox: TF2DRect;
begin
  vBox.TopLeft := AP1;
  vBox.BottomRight := vBox.TopLeft;
  ExpandF2DRect(vBox, AP2);
  if IsPointInF2DRect(vBox, APoint, AAccuracy) then
    Result := not (PointClassify2DPts(APoint, AP1, AP2, AAccuracy) in [pcLEFT, pcRIGHT])
  else
    Result := False;
end;

function IsPointOnPolyline(APoints: IsgArrayFPoint; const APoint: TFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to APoints.FPointCount - 2 do
  begin
    if IsPointOnSegmentPts(APoints.FPoints[I], APoints.FPoints[I + 1], APoint, AAccuracy) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IsPolygonClockwise2D(APoly: IsgArrayFPoint): Boolean;
var
  I,vMinI: Integer;
  vMinX,vX: Double;
  vP1,vP2,vP3: TFPoint;

  function GetIndex(AIndex,ACount: Integer): Integer;
  begin
    if AIndex < 0 then
      Result := ACount - AIndex
    else
      if AIndex >= ACount then
        Result := AIndex mod ACount
      else
        Result := AIndex;
  end;

begin
  Result := False;
  if APoly.FPointCount <= 2 then
    Exit;
  vMinX := APoly.FPoints[0].X;
  vMinI := 0;
  for I := 1 to APoly.FPointCount - 1 do
  begin
    vX := APoly.FPoints[I].X;
    if vX <= vMinX then
    begin
      vMinI := I;
      vMinX := vX;
    end;
  end;
  vP1 := APoly.FPoints[GetIndex(vMinI - 1, APoly.FPointCount)];
  vP2 := APoly.FPoints[GetIndex(vMinI, APoly.FPointCount)];
  vP3 := APoly.FPoints[GetIndex(vMinI + 1, APoly.FPointCount)];
  Result := DenomFPoint2D(SubFPoint(vP2, vP1), SubFPoint(vP3, vP2)) < 0;
end;

function CheckPointOnArc(const AP: TFPoint; const Arc: TsgArcR; const APointVector: TsgLine; var ACounter: Double): Boolean;
var
  vCross1, vCross2: TFPoint;

  function CheckPt(const AArc: TsgArcR; const APt: TFPoint): Boolean;
  var
    vAngle: Double;
  begin
    vAngle := GetAngleByPoints(AArc.Center, APt, False);
    Result := IsEqualAngle(AArc.AngleS, vAngle) or IsEqualAngle(AArc.AngleE, vAngle);
  end;

begin
  Result := IsPointOnArcAP(Arc, AP);
  if not Result then
  begin
    case IsCrossArcAndSegmentAP(Arc, APointVector, @vCross1, @vCross2) of
      1:
        begin
          if CheckPt(Arc, vCross1) then
            ACounter := ACounter + 0.5
          else
            ACounter := ACounter + 1.0;
        end;
      2:
        begin
          if CheckPt(Arc, vCross1) then
            ACounter := ACounter + 0.5
          else
            ACounter := ACounter + 1.0;
          if CheckPt(Arc, vCross2) then
            ACounter := ACounter + 0.5
          else
            ACounter := ACounter + 1.0;
        end;
    end;
  end;
end;

function CheckPos(const AVal, AMin, AMax: Double;
  const AAcuracy: Double): Integer;
var
  vDelta: Double;
begin
  Result := -1;
  if (AMin < AVal) then
  begin
    vDelta := AMax - AVal;
    if (vDelta > 0) or (Abs(vDelta) <= AAcuracy) then
      Result := +1;
  end;
end;

function CheckPointOnLine(const AP: TFPoint; const ALine: TsgLine;
  const AAccuracy: Double = 0): TsgEdgeState;
var
  vClassify: TsgPointClassify;
  vAngle: Double;

  procedure CheckSide(const ALeft: Boolean);
  var
    vInside: Integer;
  begin
    if ALeft then
      vInside := CheckPos(AP.Y, ALine.Point1.Y, ALine.Point2.Y, AAccuracy)
    else
      vInside := CheckPos(AP.Y, ALine.Point2.Y, ALine.Point1.Y, AAccuracy);
    case vInside of
      -1:  Result := esInessential;
      +1:  Result := esCrossing;
    else
      Result := esInessential;
    end;
  end;

begin
  vAngle := GetAngleByPoints(ALine.Point1, ALine.Point2, False);
  if IsEqual(vAngle, 0, fAccuracy) or IsEqual(vAngle, 180, fAccuracy) or IsEqual(vAngle, 360, fAccuracy) then
  begin
    Result := esInessential;
    Exit;
  end;
  vClassify := PointClassify(AP, ALine.Point1, ALine.Point2, AAccuracy);
  case vClassify of
    pcLEFT:   CheckSide(True);
    pcRIGHT:  CheckSide(False);
    pcBETWEEN, pcORIGIN, pcDESTINATION:
      Result := esTouching;
  else
    Result := esInessential;
  end;
end;

function IsPointInArc(const Arc: TsgPoints3; const Width: Double; const APoint: TFPoint): Boolean;
var
  vDistance, vWidth: Double;
begin
  vWidth := Width * 0.5;
  vDistance := Abs(DistanceFPoint2D(Arc[0], APoint) - DistanceFPoint2D(Arc[0], Arc[1]));
  if (vDistance < vWidth) or IsEqual(vDistance, vWidth) then
    Result := IsAngleInAngles(GetAngleByPoints(Arc[0], APoint, False),
      GetAngleByPoints(Arc[0], Arc[1], False),
      GetAngleByPoints(Arc[0], Arc[2], False))
  else
    Result := False;
end;

function IsPointInCircle(const ACenter: TFPoint; const ARadius: Double; const APoint: TFPoint): Boolean;
var
  vRadius: Double;
begin
  Result := False;
  vRadius := DistanceFPoint(APoint, ACenter);
  if (vRadius < ARadius) or IsEqual(vRadius, ARadius) then
    Result := True;
end;

function IsPointInTriangle(const AP1, AP2, AP3, AP: TFPoint): Boolean;
var
  S1, S2: Double;
begin
  S1 := AreaOfTriangle(AP1, AP2, AP) + AreaOfTriangle(AP1, AP3, AP) +
    AreaOfTriangle(AP2, AP3, AP);
  S2 := AreaOfTriangle(AP1, AP2, AP3);
  Result := sgIsZero(S2 - S1);
end;

procedure IniBox(var ABox: TFRect; const APoints: IsgArrayFPoint;
  const ADefaultBox: PFRect);
var
  I: INteger;
begin
  if ADefaultBox <> nil then
    ABox := ADefaultBox^
  else
  begin
    ABox := cnstBadRect;
    for I := 0 to APoints.FPointCount - 1 do
      ExpandFRect2D(ABox, APoints.FPoints[I]);
    ABox.Z1 := 0;
    ABox.Z2 := 0;
  end;
end;

function IsPointInRectangle(const APts: TsgPoints4; const AP: TFPoint;
  Box: PFRect = nil; const AAccuracy: Double = fDoubleResolution;
  const ACheckEdge: Boolean = False): Integer;
var
  vBox: TFRect;
  vPoints: TFPointList;
begin
  Result := 0;
  if Box = nil then
    vBox := GetBoxOfPts2D(APts)
  else
    vBox := Box^;
  vPoints := TFPointList.Create;
  try
    vPoints.Capacity := 5;
    vPoints.AssignArray([APts[0], APts[1], APts[2], APts[3]]);
    if ACheckEdge then
    begin
      vPoints.Add(APts[0]);
      Result := Integer(IsPointOnPolyline(vPoints, AP, AAccuracy));
      vPoints.Count := vPoints.Count - 1;
    end;
    if Result = 0 then
    begin
      case IsPointInPolyline(vPoints, AP, @vBox, AAccuracy) of
        1:  Result := 2;
        2:  Result := 1;
      end;
    end;
  finally
    vPoints.Free;
  end;
end;


function IsPointsInRectangle(const APts: TsgPoints4;
  const APoints: TsgPoints4; Box: PFRect = nil;
  const AAccuracy: Double = fDoubleResolution;
  const ACheckEdge: Boolean = False): Integer;
var
  I: Integer;
  vBox: TFRect;
  vPoints: TFPointList;
begin
  Result := 0;
  if Box = nil then
    vBox := GetBoxOfPts2D(APts)
  else
    vBox := Box^;
  vPoints := TFPointList.Create;
  try
    vPoints.Capacity := 5;
    vPoints.AssignArray([APts[0], APts[1], APts[2], APts[3]]);
    if ACheckEdge then
    begin
      vPoints.Add(APts[0]);
      for I := Low(APoints) to High(APoints) do
      begin
        Result := Integer(IsPointOnPolyline(vPoints, APoints[I], AAccuracy));
        if Result > 0 then
          Break;
      end;
      vPoints.Count := vPoints.Count - 1;
    end;
    if Result = 0 then
    begin
      for I := Low(APoints) to High(APoints) do
      begin
        case IsPointInPolyline(vPoints, APoints[I], @vBox, AAccuracy) of
          1:  Result := 2;
          2:  Result := 1;
        end;
        if Result = 2 then
          Break;
      end;
    end;
  finally
    vPoints.Free;
  end;
end;

function IsPointInPolyline(const APts: TList; const AP: TFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Boolean;
begin
  Result := False;
  if APts.Count > 2 then
{$IFDEF SGFPC}
    Result := IsPointInPolyline(TsgListInterface.Create(APts, TsgProcOfPointerGetPoint(@TsgListInterface.DereferenceToPFPoint)), AP, Box, AAccuracy, AMode) = 1;
{$ELSE}
    Result := IsPointInPolyline(TsgListInterface.Create(APts, TsgListInterface.DereferenceToPFPoint), AP, Box, AAccuracy, AMode) = 1;
{$ENDIF}
end;

function GetLineFromFPoints(APts: IsgArrayFPoint; AI, ACnt: Integer): TsgLine;
begin
  Result.Point1 := APts.FPoints[AI];
  if AI = ACnt then
    Result.Point2 := APts.FPoints[0]
  else
    Result.Point2 := APts.FPoints[AI + 1];
end;

function IsPointInPolyline(APts: IsgArrayFPoint; const AP: TFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer;
var
  I, Cnt, vPtsCount: Integer;
  vBox: TFRect;
  vPtLine,vDiagLine: TsgLine;
  vEdgeType: TsgEdgeState;
  vPoint1, vPoint2, vCross: TFPoint;

  function IsPointOnLine2D(ALine: TsgLine; AP: TFPoint; const AAccuracy: Double = 0): Boolean;
  var
    vA,vB: Double;
  begin
    if IsEqualFPoints(ALine.Point1, ALine.Point2, AAccuracy) then
      Result := False
    else
      if IsEqual(ALine.Point1.X, ALine.Point2.X, AAccuracy) then
        Result := IsEqual(ALine.Point1.X, AP.X, AAccuracy)
      else
        if IsEqual(ALine.Point1.Y, ALine.Point2.Y, AAccuracy) then
          Result := IsEqual(ALine.Point1.Y, AP.Y, AAccuracy)
        else
        begin
          vA := (AP.X - ALine.Point1.X)/(ALine.Point2.X - ALine.Point1.X);
          vB := (AP.Y - ALine.Point1.Y)/(ALine.Point2.Y - ALine.Point1.Y);
          Result := IsEqual(vA, vB, AAccuracy);
        end;
  end;

  function IsPointOnSegment2D(ALine: TsgLine; AP: TFPoint; const AAccuracy: Double = 0): Boolean;
  begin
    if IsPointInFRect2D(GetBoxOfLine2D(ALine), AP, AAccuracy) then
      Result := IsPointOnLine2D(ALine, AP, AAccuracy)
    else
      Result := False;
  end;

  function GetSubPoint(const APoint: TFPoint; const ABox: TFRect): TFPoint;
  begin
    Result := MakeFPoint(IfThen(ABox.Left > 0, ABox.Left * 0.25, ABox.Left * 1.25) , APoint.Y * 0.95, APoint.Z);
    if ABox.Left = 0 then
    begin
      if ABox.Left <> ABox.Right then
        Result.X := ABox.Left - ABox.Right
      else
        Result.X := 25;
    end;
  end;

begin
  Result := 0;
  if APts.FPointCount < 2 then Exit;
  IniBox(vBox, APts, Box);
  if IsPointInFRect2D(vBox, AP, AAccuracy) then
  begin
    Cnt := 0;
    vPtsCount := APts.FPointCount - 1;
    case AMode of
      1, 3:
        begin
          if AMode = 3 then
          begin
            for I := 0 to vPtsCount do
            begin
              vPtLine := GetLineFromFPoints(APts, I, vPtsCount);
              if IsEqualFPoints(vPtLine.Point1, AP, AAccuracy) then
              begin
                Result := 2;
                Break;
              end
              else
              begin
                if IsPointOnSegment2D(vPtLine, AP, AAccuracy) then
                begin
                  Result := 2;
                  Break;
                end;
              end;
            end;
            if Result = 2 then
              Exit;
          end;
          vPoint1 := AP;
          vPoint2 := GetSubPoint(AP, vBox);
          Cnt := IsCrossSegmentPolylinePts(vPoint1, vPoint2, APts, True, nil, 1, false, AAccuracy);
          if Cnt mod 2 = 1 then
            Result := 1;
        end;
      2:
        begin
          vDiagLine.Point1 := AP;
//          vDiagLine.Point2 := AddFPoint(vBox.BottomRight, MakeFPoint(10, -24));
          vDiagLine.Point2 := MakeFpoint(vBox.Right, vBox.Bottom * 0.93);
          vDiagLine.Point2 := GetPointOnLine(vDiagLine.Point1, vDiagLine.Point2, DistanceFPoint(vDiagLine.Point1, vDiagLine.Point2) * 2);
          for I := 0 to vPtsCount do
          begin
            vPtLine := GetLineFromFPoints(APts, I, vPtsCount);
            if IsPointOnSegment2D(vPtLine, AP, AAccuracy) then
            begin
              Result := 2; //BOUNDARY
              Exit;
            end
            else
              if IsCrossSegmentsPts(vPtLine.Point1, vPtLine.Point2, vDiagLine.Point1, vDiagLine.Point2, @vCross, AAccuracy) then
                if not IsEqualFPoints(vPtLine.Point2, vCross, GetAccuracyByLine(vPtLine, 0.00001)) then
                  Inc(Cnt);
          end;
          Result := Integer((Cnt and 1) = 1);
        end;
    else
      for I := 0 to vPtsCount do
      begin
        vPtLine := GetLineFromFPoints(APts, I, vPtsCount);
        vEdgeType := CheckPointOnLine(AP, vPtLine, AAccuracy);
        case vEdgeType of
          esTouching:
            begin
              Result := 2; //BOUNDARY
              Exit;
            end;
          esCrossing: Cnt := 1 - Cnt;
        end;
      end;
      Result := Integer(Cnt = 1);
    end;
  end;
end;

function IsPointInPolyPolyline(const APolylines: TsgObjectList; const AP: TFPoint;
  const ABoxes: TList = nil; const AAccuracy: Double = 0): Boolean;
var
  I, vIsPointInPolylineCnt: Integer;
  vObj: TObject;
  vPolyline: IsgArrayFPoint;
  vBox: PFRect;
begin
  Result := False;
  vIsPointInPolylineCnt := 0;
  vBox := nil;
  for I := 0 to APolylines.Count - 1 do
  begin
    vObj := TObject(APolylines.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
    if vObj.GetInterface(IsgArrayFPoint, vPolyline) and (vPolyline.FPointCount > 2) then
    begin
      if ABoxes <> nil then
        vBox := PFRect(ABoxes[I]);
      if IsPointInPolyline(vPolyline, AP, vBox, AAccuracy) = 1 then
        Inc(vIsPointInPolylineCnt);
    end;
  end;
  if (vIsPointInPolylineCnt mod 2 > 0) then
    Result := True;
end;

function IsPointInPolyPolylineInt(const APolylines: TList; const AP: TFPoint;
  const AAccuracy: Double = 0): Integer;
var
  vObjList: TsgObjectList;
begin
  vObjList := TsgObjectList.Create;
  try
    vObjList.CopyFrom(APolylines);
    Result := IsPointInPolyPolylineListInt(vObjList, AP, AAccuracy);
  finally
    vObjList.Free;
  end;
end;

function IsPointInPolyPolylineListInt(const APolylines: TsgObjectList;
  const AP: TFPoint; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer;
var
  I: Integer;
  vObj: TObject;
  vPolyline: IsgArrayFPoint;
begin
  Result := 0;
  for I := 0 to APolylines.Count - 1 do
  begin
    vObj := APolylines[I];
    vPolyline := nil;
    if vObj is TF2DPointList then
      vPolyline := TF2DPointList(vObj)
    else
      vPolyline := TFPointList(vObj);
    if Assigned(vPolyline) and (vPolyline.FPointCount > 2) then
      Result := MaxI(Result, IsPointInPolyline(vPolyline, AP, nil, AAccuracy, AMode));
  end;
end;

function IsPointInSector(const Arcs: PsgArcsParams; const AP: TFPoint; Box: PFRect = nil): Byte;
var
  vIndex, Cnt: Integer;
  vDelta: Double;
  vArc1, vArc2: TsgArcR;
  vCheckP, vCross1, vCross2: TFPoint;
  vFlags: array [0..3] of Boolean;
  vPts: TsgPoints4;

  function CheckPos(const APt1, APt2, ACr: PFPoint): Boolean;
  var
    CL1, CL2: TsgPointClassify;
  begin
    CL1 := PointClassify(APt1^, AP, ACr^);
    CL2 := PointClassify(APt2^, AP, ACr^);
    Result := CL1 <> CL2;
  end;

  function GetArcPt(const Arc: TsgArcR; const AIsStart: Boolean): TFPoint;
  var
    A, E, D: Double;
  begin
    E := Arc.AngleE;
    if E < Arc.AngleS then
      E := E + 180;
    D := (E - Arc.AngleS) / GetNumberOfCircleParts;
    if AIsStart then
      A := Arc.AngleS + D
    else
      A := E - D;
    Result := GetPointOnCircle(Arc.Center, Arc.Radius, A);
  end;

  procedure CheckCrossPt(const APnt: TFPoint);
  var
    vP1, vP2: TFPoint;
  begin
    vIndex := IndexOfPts2D(vPts, APnt);
    if (vIndex > -1) and (not vFlags[vIndex]) then
    begin
      case vIndex of
        0:
          begin
            vP1 := GetArcPt(vArc1, True);
            vP2 := vPts[3];
          end;
        1:
          begin
            vP1 := vPts[2];
            vP2 := GetArcPt(vArc1, False);
          end;
        2:
          begin
            vP1 := GetArcPt(vArc2, False);
            vP2 := vPts[1];
          end;
      else//3
        vP1 := vPts[0];
        vP2 := GetArcPt(vArc2, True);
      end;
      if CheckPos(@vP1, @vP2, @APnt) then
      begin
        vFlags[vIndex] := True;
        Inc(Cnt);
      end;
    end
    else
      Inc(Cnt);
  end;

begin
  Result := 0;
  if Arcs <> nil then
  begin
    if (Box <> nil) and (not IsPointInFRect2D(Box^, AP)) then
      Exit;
    MakeArcs(Arcs^, vArc1, vArc2);
    vPts[0] := GetPointOnCircle(vArc1.Center, vArc1.Radius, vArc1.AngleS);
    vPts[1] := GetPointOnCircle(vArc1.Center, vArc1.Radius, vArc1.AngleE);
    vPts[2] := GetPointOnCircle(vArc2.Center, vArc2.Radius, vArc2.AngleE);
    vPts[3] := GetPointOnCircle(vArc2.Center, vArc2.Radius, vArc2.AngleS);
    if (PointClassify(AP, vPts[0], vPts[3]) in  cnstPtInLine) or
       (PointClassify(AP, vPts[1], vPts[2]) in  cnstPtInLine) or
       IsPointOnArcAP(vArc1, AP) or IsPointOnArcAP(vArc2, AP) then
      Result := 1
    else
    begin
      Cnt := 0;
      FillChar(vFlags, SizeOf(vFlags), 0);
      vDelta := Max(Arcs^.Radius1, Arcs^.Radius2);
      if vDelta < fDoubleResolution then
        vDelta := MaxSingle
      else
        vDelta := vDelta * 4;
      vCheckP := MakeFPoint(AP.X + vDelta, AP.Y, 0);
      
      if IsCrossSegmentsPts(AP, vCheckP, vPts[0], vPts[3], @vCross1) then
        CheckCrossPt(vCross1);
      if IsCrossSegmentsPts(AP, vCheckP, vPts[1], vPts[2], @vCross1) then
        CheckCrossPt(vCross1);
      case IsCrossArcAndSegmentAP(vArc1, MakeLine(AP, vCheckP), @vCross1, @vCross2) of
        1:  CheckCrossPt(vCross1);
        2:
          begin
            CheckCrossPt(vCross1);
            CheckCrossPt(vCross2);
          end;
      end;
      case IsCrossArcAndSegmentAP(vArc2, MakeLine(AP, vCheckP), @vCross1, @vCross2) of
        1:  CheckCrossPt(vCross1);
        2:
          begin
            CheckCrossPt(vCross1);
            CheckCrossPt(vCross2);
          end;
      end;
    end;
    if (Cnt > 0) and (Cnt mod 2 > 0) then
      Result := 2;
  end;
end;

function IsFRectInPolyline(APts: IsgArrayFPoint; const ABox: TFRect;
  Box: PFRect = nil; const AAccuracy: Double = 0): Boolean;
begin
  Result := False;
  if IsBox2DInPolyline(APts, ABox, Box, AAccuracy) > -1 then
    Result := True;
end;

function IsBox2DInPolyline(APts: IsgArrayFPoint; const ABox: TFRect;
  Box: PFRect = nil; const AAccuracy: Double = 0): Integer;
var
  vPointInPolylineCnt: Integer;
begin
  vPointInPolylineCnt := 0;
  try
    if IsPointInPolyline(APts, ABox.TopLeft, Box, AAccuracy) <> 0 then
      Inc(vPointInPolylineCnt);
    if IsPointInPolyline(APts, ABox.BottomRight, Box, AAccuracy) <> 0 then
      Inc(vPointInPolylineCnt);
    if (vPointInPolylineCnt = 0) or (vPointInPolylineCnt = 2) then
    begin
      if IsPointInPolyline(APts, MakeFPoint(ABox.Right, ABox.Top), Box, AAccuracy) <> 0 then
        Inc(vPointInPolylineCnt);
      if (vPointInPolylineCnt = 0) or (vPointInPolylineCnt = 3) then
      begin
        if IsPointInPolyline(APts, MakeFPoint(ABox.Left, ABox.Bottom), Box, AAccuracy) <> 0 then
          Inc(vPointInPolylineCnt);
      end;
    end;
  finally
    case vPointInPolylineCnt of
      0:  Result := -1;
      4:  Result := 1;
    else
      Result := 0;
    end;
  end;
end;

function IsPointsInPolyline(const APoints, APolyline: IsgArrayFPoint;
  Box: PFRect = nil; const AAccuracy: Double = 0;
  const AMode: Integer = 0): Integer;
var
  I, vCnt,vFront,vOut: Integer;
  vBox: TFRect;
  vPt1,vPt2: TFPoint;

  procedure IncsCount(const AP: TFPoint; const ABox: TFRect;
    var ACnt,AFront,AOut: Integer);
  begin
    case IsPointInPolyline(APolyline, AP, @ABox, AAccuracy, AMode) of
      1: Inc(ACnt);
      2: Inc(AFront);
    else
      Inc(AOut);
    end;
  end;

begin
  Result := -1;
  IniBox(vBox, APolyline, Box);
  vCnt := 0;
  vFront := 0;
  vOut := 0;
  for I := 0 to APoints.FPointCount - 1 do
  begin
    if AMode and 2 <> 0 then
    begin
      vPt1 := APoints.FPoints[I];
      vPt2 := APoints.FPoints[(I + 1) mod APoints.FPointCount];
      IncsCount(vPt1, vBox, vCnt, vFront, vOut);
      IncsCount(MiddleFPoint(vPt1, vPt2), vBox, vCnt, vFront, vOut);
    end
    else
    begin
      if IsPointInPolyline(APolyline, APoints.FPoints[I], @vBox, AAccuracy) <> 0 then
        Inc(vCnt)
      else
        Break;
    end;
  end;
  if AMode and 2 <> 0 then
  begin
    if vFront + vCnt = 2 * APoints.FPointCount then
      Result := 1
    else
      if vFront + vOut = 2 * APoints.FPointCount then
        Result := -1
      else
        Result := 0;
  end
  else
    if vCnt > 0 then
    begin
      Result := 0;
      if vCnt = APoints.FPointCount then
        Result := 1;
    end;
end;

function IsPolyPointsInPolyline(const APolyPolyline: TsgObjectList;
  const APolyline: IsgArrayFPoint; Box: PFRect = nil;
  const AAccuracy: Double = 0): Integer;
var
  I, vRez: Integer;
  vBox: TFRect;
  vObjPoints: TInterfacedObject;
  vPoints: IsgArrayFPoint;
begin
  Result := -1;
  IniBox(vBox, APolyline, Box);
  for I := 0 to APolyPolyline.Count - 1 do
  begin
    vObjPoints := TInterfacedObject(APolyPolyline[I]);
    if not vObjPoints.GetInterface(GUID_ArrayFPoints, vPoints) then
    begin
      Result := -1;
      Break;
    end;
    vRez := IsPointsInPolyline(vPoints, APolyline, @vBox, AAccuracy);
    case vRez of
      0:
        begin
          Result := 0;
          Break;
        end;
      1:
        begin
          Result := 1;
        end;
    else
      Break;
    end;
  end;
end;

function IsLineOnLine(const ALine1, ALine2: TsgLine): Boolean;
begin
  Result := False;
  if IsParalleniarLines(ALine1, ALine2) then
    Result := IsPointOnSegment(ALine1, ALine2.Point1) and
      IsPointOnSegment(ALine1, ALine2.Point2);
end;


function IsLineVerticalPts(const AP1, AP2: TFPoint): Boolean;
var
  vAngle: Double;
begin
  Result := False;
  vAngle := GetAngleByPoints(AP1, AP2, False);
  if ((vAngle >= 45) and (vAngle <= 135)) or
     ((vAngle >= 225) and (vAngle <= 315)) then
    Result := True;
end;

function IsSelfIntersectingPolyline(APts: IsgArrayFPoint;
  const AClosed: Boolean = False; Epsilon: Double = fDoubleResolution;
  const ACrosses: TFPointList = nil; AGetAllPoints: Boolean = False;
  const AIndexes: TsgIntegerList = nil): Boolean;
var
  I, J, vCnt, vPointsCount: Integer;
  vP11, vP12, vP21, vP22, vCross: TFPoint;
begin
  Result := False;
  vPointsCount := APts.FPointCount;
  vCnt := vPointsCount + Integer(AClosed);
  if vCnt > 2 then
  begin
    vP11 := APts.FPoints[0];
    for I := 1 to vCnt - 2 do
    begin
      vP12 := APts.FPoints[I mod vPointsCount];
      if not IsEqualFPoints(vP11, vP12) then
      begin
        vP21 := APts.FPoints[I + 1];
        for J := I + 2 to vCnt - 1 do
        begin
          vP22 := APts.FPoints[J mod vPointsCount];
          if not IsEqualFPoints(vP21, vP22) then
          begin
            if not (IsEqualFPoints(vP12, vP21, Epsilon) or IsEqualFPoints(vP12, vP22, Epsilon) or
              IsEqualFPoints(vP11, vP21, Epsilon) or IsEqualFPoints(vP11, vP22, Epsilon)) then//not a line junction point
            begin
              if IsCrossCoords(vP11.X, vP11.Y, vP12.X, vP12.Y, vP21.X, vP21.Y, vP22.X, vP22.Y, @vCross) = 2 then
              begin
                Result := True;
                if Assigned(ACrosses) then
                  ACrosses.Add(vCross);
                if Assigned(AIndexes) then
                begin
                  AIndexes.Add(I - 1);
                  AIndexes.Add(I);
                  AIndexes.Add(J - 1);
                  AIndexes.Add(J);
                end;
                if not AGetAllPoints then
                  Break;
              end;
            end;
            vP21 := vP22;
          end;
        end;
        if not AGetAllPoints and Result then
          Break;
        vP11 := vP12;
      end;
    end;
  end;
end;

function AddCrossData(const AInds: TsgIntegerList; const ACrossPts: TFPointList;
  ACross1, ACross2: TFPoint; ACount, ACurrVerInd, ANextVerInd: Integer): Boolean;
var
    I: Integer;
begin
  Result := True;
  if Assigned(ACrossPts) then
  begin
    ACrossPts.Add(ACross1);
    if ACount = 2 then
      ACrossPts.Add(ACross2);
  end;
    for I := 0 to ACount - 1 do
    if Assigned(AInds) then
      begin
      AInds.Add(ACurrVerInd - 1);
      AInds.Add(ACurrVerInd);
      AInds.Add(ANextVerInd - 1);
      AInds.Add(ANextVerInd);
      end;
end;

function IsCrossVertexesInternal(const AVerPt11, AVerPt12, AVerPt21, AVerPt22: TFPoint; ABul11, ABul21: Double;
  const AArc1, AArc2: TsgArcR; const ACross1, ACross2: PFPoint; var ACrossCnt: Byte;
  Epsilon: Double = fDoubleResolution): Boolean;
var
  vFlags: Integer;
  vIsCross: Boolean;
begin
  Result := False;
  vFlags := Ord(ABul11 <> 0) or (Ord(ABul21 <> 0) shl 1);
  if not IsEqualFPoints(AVerPt21, AVerPt22) then
  begin
    if not (IsEqualFPoints(AVerPt12, AVerPt21, Epsilon) or IsEqualFPoints(AVerPt12, AVerPt22, Epsilon) or
      IsEqualFPoints(AVerPt11, AVerPt21, Epsilon) or IsEqualFPoints(AVerPt11, AVerPt22, Epsilon)) then //not a line junction point
    begin
      vIsCross := False;
      case vFlags of
        0: ACrossCnt := IsCrossCoords(AVerPt11.X, AVerPt11.Y, AVerPt12.X, AVerPt12.Y, AVerPt21.X, AVerPt21.Y, AVerPt22.X, AVerPt22.Y, ACross1);
        1: ACrossCnt := IsCrossArcAndSegmentAP(AArc1, MakeLine(AVerPt21, AVerPt22), ACross1, ACross2, Epsilon);
        2: ACrossCnt := IsCrossArcAndSegmentAP(AArc2, MakeLine(AVerPt11, AVerPt12), ACross1, ACross2, Epsilon);
        3: ACrossCnt := IsCrossArcsAP(AArc1, AArc2, ACross1, ACross2, Epsilon);
  end;
      case vFlags of
        0:
          begin
            vIsCross := ACrossCnt = 2;
            ACrossCnt := 1;
          end;
        1,2,3:
          begin
            vIsCross := ACrossCnt > 0;
          end;
      end;
      Result := vIsCross;
    end;
  end;
end;

function IsCrossVertexes(const AVertexes1, AVertexes2: IsgArrayVertexes;
  Epsilon: Double = fDoubleResolution; const ACrosses: TFPointList = nil;
  AGetAllPoints: Boolean = False; const AIndexes: TsgIntegerList = nil): Boolean;

var
  vCrossCnt: Byte;
  vTick, vTickSt, vTickEnd: Cardinal;
  vCrossBoxesCount, X: Integer;
  vBox1, vBox2: TFRect;
  I, J, vMaxInd1, vMaxInd2: Integer;
  vArc1, vArc2: TsgArcR;
  vP11, vP12, vP21, vP22, vCross, vCross2: TFPoint;
  vBul11, {vBul12, }vBul21, vBul22: Double;
begin
  Result := False;
  vCrossCnt := 0;
  vMaxInd1 := AVertexes1.FPointCount - 1;
  vMaxInd2 := AVertexes2.FPointCount - 1;
  vP12 := AVertexes1.FPoints[vMaxInd1];
  //vBul12 := AVertexes1.Bulges[vMaxInd1];
  for I := vMaxInd1 - 1 downto 0 do
  begin
    vP11 := AVertexes1.FPoints[I];
    vBul11 := AVertexes1.Bulges[I];
    if vBul11 <> 0 then
      vArc1 := GetArcROfBulge(vP11, vP12, vBul11);
    if not IsEqualFPoints(vP11, vP12) then
    begin
      vP21 := AVertexes2.FPoints[0];
      vBul21 := AVertexes2.Bulges[0];
      for J := 0 to vMaxInd2 do
      begin
        vP22 := AVertexes2.FPoints[J];
        vBul22 := AVertexes2.Bulges[J];
        if vBul21 <> 0 then
          vArc2 := GetArcROfBulge(vP21, vP22, vBul21);
        if vBul11 <> 0 then
          vBox1 := GetBoxOfCircle2D(vArc1)
        else
          vBox1 := GetBoxOfLine2D(MakeLine(vP11, vP12));
        if vBul21 <> 0 then
          vBox2 := GetBoxOfCircle2D(vArc2)
        else
          vBox2 := GetBoxOfLine2D(MakeLine(vP21, vP22));
        vCrossBoxesCount := IntersectFRect2D(vBox1, vBox2);
        if (vCrossBoxesCount <> -1) and IsCrossVertexesInternal(vP11, vP12, vP21, vP22, vBul11, vBul21, vArc1,
        vArc2, @vCross, @vCross2, vCrossCnt, Epsilon) then
        begin
          Result := AddCrossData(AIndexes, ACrosses, vCross, vCross2, vCrossCnt, I + 1, J);
              if not AGetAllPoints then
                Break;
            end;
        vP21 := vP22;
        vBul21 := vBul22;
      end;
      if not AGetAllPoints and Result then
        Break;
      vP12 := vP11;
      //vBul12 := vBul11;
    end;
  end;
end;

function IsSelfIntersectingVertexes(AVertexes: IsgArrayVertexes; var AIsNearest: Boolean;
  const AClosed: Boolean = False; Epsilon: Double = fDoubleResolution;
  const ACrosses: TFPointList = nil; AGetAllPoints: Boolean = False;
  const AIndexes: TsgIntegerList = nil): Boolean;

  function IsPointsInEdges(const AArc: TsgArcR; const APt1, APt2: TFPoint): Boolean;
  begin
    Result := IsEqualF2DPoints(GetEndF2DPoint(AArc), APt1.Point2D) or
      IsEqualF2DPoints(GetStartF2DPoint(AArc), APt1.Point2D);
    Result := Result and (IsEqualF2DPoints(GetEndF2DPoint(AArc), APt2.Point2D) or
      IsEqualF2DPoints(GetStartF2DPoint(AArc), APt2.Point2D));
  end;

  function IsCrossNearestBulge(const AArc1, AArc2: TsgArcR; var APoint: TFPoint): Boolean;
  var
    vCrossCnt: Integer;
    vCross2: TFPoint;
  begin
    Result := False;
    vCrossCnt := IsCrossArcsAP(AArc1, AArc2, @APoint, @vCross2, Epsilon);
    if vCrossCnt > 1 then
    begin
      if not ((AVertexes.FPointCount = 3) and IsPointsInEdges(AArc1, APoint, vCross2) and IsPointsInEdges(AArc2, APoint, vCross2)) then
      begin
        if IsEqualF2DPoints(GetEndF2DPoint(AArc1), APoint.Point2D) or IsEqualF2DPoints(GetStartF2DPoint(AArc1), APoint.Point2D) then
          APoint := vCross2;
        Result := True;
      end;
    end;
  end;

  function IsIntersectNearestVertexes(ABulge1, ABulge2: Double; const APt1, APt2, APt3, APt4: TFPoint; AInd1, AInd2: Integer): Boolean;
  var
    vCross, vCross2: TFPoint;
    vArc1, vArc2: TsgArcR;
    vLine: TsgLine;
  begin
    Result := False;
    if (ABulge1 <> 0) or (ABulge2 <> 0) then
    begin
      if (ABulge1 <> 0) and (ABulge2 <> 0)  then  //is nearest vertexes have bulge
      begin
        vArc1 := GetArcROfBulge(APt1, APt2, ABulge1);
        vArc2 := GetArcROfBulge(APt3, APt4, ABulge2);
        if IsCrossNearestBulge(vArc1, vArc2, vCross) then
        begin
          Result := AddCrossData(AIndexes, ACrosses, vCross, vCross2, 1, AInd1, AInd2);
          AIsNearest := True;
        end;
      end
      else
      begin     //is one of nearest vertexes have bulge
        if ABulge2 <> 0  then
        begin
          vArc1 := GetArcROfBulge(APt3, APt4, ABulge2);
          vLine := MakeLine(APt1, APt2);
        end
        else
        begin
          vArc1 := GetArcROfBulge(APt1, APt2, ABulge1);
          vLine := MakeLine(APt3, APt4);
        end;
        if IsCrossArcAndSegmentAP(vArc1, vLine, @vCross, @vCross2, Epsilon) > 1 then
        begin
          if IsEqualF2DPoints(GetEndF2DPoint(vArc1), vCross.Point2D) or IsEqualF2DPoints(GetStartF2DPoint(vArc1), vCross.Point2D) then
            vCross := vCross2;
          Result := AddCrossData(AIndexes, ACrosses, vCross, vCross2, 1, AInd1, AInd2);
          AIsNearest := True;
        end;
      end;
    end;
  end;

var
  vCrossCnt: Byte;
  vCrossBoxesCount: Integer;
  vBox1, vBox2: TFRect;
  I, J, vCnt: Integer;
  vArc1, vArc2: TsgArcR;
  vP11, vP12, vP21, vP22, vCross, vCross2: TFPoint;
  vBul11, vBul12, vBul21, vBul22: Double;
  vCallCount: Integer;
begin
  Result := False;
  AIsNearest := False;
  vCallCount := 0;
  vCnt := AVertexes.FPointCount + Integer(AClosed);
  if vCnt > 2 then
  begin
    vCrossCnt := 0;
    vP11 := AVertexes.FPoints[0];
    vBul11 := AVertexes.Bulges[0];
    for I := 1 to vCnt - 2 do
    begin
      vP12 := AVertexes.FPoints[I];
      vBul12 := AVertexes.Bulges[I];
      if vBul11 <> 0 then
        vArc1 := GetArcROfBulge(vP11, vP12, vBul11);
      Inc(vCallCount);
      if not IsEqualFPoints(vP11, vP12) then
      begin
        vP21 := AVertexes.FPoints[I + 1];
        vBul21 := AVertexes.Bulges[I + 1];
        if (AVertexes.FPointCount > 3) and IsIntersectNearestVertexes(vBul11, vBul12, vP11, vP12, vP12, vP21, I, I + 1) then
        begin
          Result := True;
          if not AGetAllPoints then
            Break;
        end;

        for J := I + 2 to vCnt - 1 do
        begin
          vP22 := AVertexes.FPoints[J];
          vBul22 := AVertexes.Bulges[J];
          if vBul21 <> 0 then
            vArc2 := GetArcROfBulge(vP21, vP22, vBul21);
          if (I = 1) and (J = vCnt - 1) and ((vBul11 <> 0) or (vBul21 <> 0)) then   //check cross first whith last bulge
          begin
            if (vBul11 <> 0) and (vBul21 <> 0) then   //check cross first whith last bulge
            begin
            if IsCrossNearestBulge(vArc1, vArc2, vCross) then
                Result := AddCrossData(AIndexes, ACrosses, vCross, vCross2, 1, I, J);
          end
          else
            begin
              if IsIntersectNearestVertexes(vBul11, vBul21, vP11, vP12, vP21, vP22, I, J) then
                Result := True;
              end;
          end
          else
            begin
            if vBul11 <> 0 then
              vBox1 := GetBoxOfCircle2D(vArc1)
            else
              vBox1 := GetBoxOfLine2D(MakeLine(vP11, vP12));
            if vBul21 <> 0 then
              vBox2 := GetBoxOfCircle2D(vArc2)
            else
              vBox2 := GetBoxOfLine2D(MakeLine(vP21, vP22));
            vCrossBoxesCount := IntersectFRect2D(vBox1, vBox2);
            if (vCrossBoxesCount <> -1) and IsCrossVertexesInternal(vP11, vP12, vP21, vP22, vBul11, vBul21,
            vArc1, vArc2, @vCross, @vCross2, vCrossCnt, Epsilon) then
              begin
                  Result := AddCrossData(AIndexes, ACrosses, vCross, vCross2, vCrossCnt, I, J);
                  if not AGetAllPoints then
                    Break;
                end;
              end;
              vP21 := vP22;
              vBul21 := vBul22;
            end;
        if not AGetAllPoints and Result then
          Break;
        vP11 := vP12;
        vBul11 := vBul12;
      end;
    end;
  end;
end;

function IsCrossSegments2D(const ALine1, ALine2: TF2DLine; const ACross: PF2DPoint): Boolean;
begin
  Result := IsCrossSegmentsPts2D(ALine1.Point1, ALine1.Point2, ALine2.Point1,
    ALine2.Point2, ACross);
end;

//
//0 does not intersect
//1 direct overlap
//2 line segments intersect
//
function IsCrossCoords(const X11, Y11, X12, Y12, X21, Y21, X22, Y22: Double;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution): Integer;
var
  vDX, vDY, vDX1, vDX2, vDY1, vDY2, vDenom, r, s: Extended;
  vAccuracy: Double;
begin
  Result := 0;
  vDX1 := X12 - X11;
  vDY1 := Y12 - Y11;
  vDX2 := X22 - X21;
  vDY2 := Y22 - Y21;
  vDenom := vDX1 * vDY2 - vDX2 * vDY1;
  vAccuracy := AAccuracy;
  if vAccuracy = 0 then
    vAccuracy := fDoubleResolution;
  if not IsRange(vDenom, vAccuracy) then
  begin
    Result := 1;
    vDX := X11 - X21;
    vDY := Y11 - Y21;
    r := (vDX2 * vDY - vDX * vDY2) / vDenom;
    if IsValInParam(r, 0.0, 1.0, AAccuracy) then
    begin
      s := (vDX1 * vDY - vDX * vDY1) / vDenom;
      if IsValInParam(s, 0.0, 1.0, AAccuracy) then
        Result := 2;
    end;
    if ACross <> nil then
    begin
      ACross^.X := X11 + r * vDX1;
      ACross^.Y := Y11 + r * vDY1;
      ACross^.Z := 0;
    end;
  end;
end;

function GetDeterminant2x2(const a1, b1, a2, b2: Double): Double;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;

function GetDeterminant3x3(const a1, b1, c1, a2, b2, c2, a3, b3, c3: Double): Extended;
var
  V: array[0..5] of Extended;
begin
  V[0] :=  a1*b2*c3;
  V[1] :=  a3*b1*c2;
  V[2] :=  a2*b3*c1;
  V[3] := -a3*b2*c1;
  V[4] := -a1*b3*c2;
  V[5] := -a2*b1*c3;
  Result := V[0] + V[1] + V[2] + V[3] + V[4] + V[5];
end;

//x, y, z - first point
//p, q, r - direction(end point - start point)
function IsCrossCoordsParm(const x0, y0, z0, p0, q0, r0, x1, y1, z1, p1, q1, r1: Double;
  var ACross: TFPoint; const AAccuracy: Double = fDoubleResolution): Integer;
var
  vDx, vDy, vDz: Double;
//  vTx, vTy, vTz: Double;
//  vDet: Double;
begin
  Result := 0;
  ACross.X := x0;
  ACross.Y := y0;
  ACross.Z := z0;
//  vTx := p0 / p1;
//  vTy := q0 / q1;
//  vTZ := r0 / r1;
//  if IsEqual(vTx, vTy, AAccuracy) and IsEqual(vTx, vTz, AAccuracy) then
//    Result := -1
//  else
  begin
    vDx := GetDeterminant2x2(q0, q1, p0, p1);
    vDy := GetDeterminant2x2(p0, p1, q0, q1);
    vDz := GetDeterminant2x2(q0, q1, r0, r1);
    if vDx <> 0 then
      ACross.X := (x0*q0*p1-x1*q1*p0-y0*p0*p1+y1*p0*p1)/vDx;
    if vDy <> 0 then
      ACross.Y := (y0*p0*q1-y1*p1*q0-x0*q0*q1+x1*q0*q1)/vDy;
    if vDz <> 0 then
      ACross.Z := (z0*q0*r1-z1*q1*r0-y0*r0*r1+y1*r0*r1)/vDz;
  end
end;

function IsCrossSegmentsPts(const AP11, AP12, AP21, AP22: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution;
  const AModes: TsgByteSet = [2]): Boolean;
begin
  Result := IsCrossCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X, AP21.Y,
    AP22.X, AP22.Y, ACross, AAccuracy) in AModes;
end;

function IsCrossSegmentsPts2D(const AP11, AP12, AP21, AP22: TF2DPoint;
  const ACross: PF2DPoint): Boolean;
var
  Pnt: TFPoint;
begin
  if ACross = nil then
    Result := IsCrossCoords(AP11.X, AP11.Y, AP12.X, AP12.Y,
      AP21.X, AP21.Y, AP22.X, AP22.Y, nil) = 2
  else
  begin
    Result := IsCrossCoords(AP11.X, AP11.Y, AP12.X,  AP12.Y, AP21.X, AP21.Y,
      AP22.X, AP22.Y, @Pnt) = 2;
    if Result then
    begin
      ACross^.X := Pnt.X;
      ACross^.Y := Pnt.Y;
    end;
  end;
end;

function IsParralelinarSegments3D(const AP11,AP12,AP21,AP22: TFPoint; AAccuracy: Double = 0): Boolean;
var
  vV1,vV2: TFPoint;
  vAngle: Double;
begin
  Result := False;
  vV1 := SubFPoint(AP11, AP12);
  vV2 := SubFPoint(AP21, AP22);
  vAngle := GetAngleOfVectors(vV1, vV2, False, True);
  if (vAngle <= AAccuracy) or (vAngle >= 180 - AAccuracy) then
    Result := True;
end;

function IsCrossSegmentPolylinePts(const AP1, AP2: TFPoint;
  APts: IsgArrayFPoint; const ACloesd: Boolean;
  const ACroses: TFPointList; const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False;
  AAccuracy: Double = 0): Integer;

  function AddCrossPoint(const AP, APS1, APS2: TFPoint; const APoints: TFPointList): Boolean;
  var
    vCnt: Integer;
    vSegAccuracy: Double;
  begin
    Result := True;
    if AMode and 1 <> 0 then
    begin
      if IsEqualFPoints(AP1, AP, AAccuracy) then
        Result := False;
      if Result and IsEqualFPoints(AP2, AP, AAccuracy) then
        Result := False;
      if Result and (IndexOfFPoint(APts, AP, AAccuracy) > -1) then
        Result := False;
    end;
    if Result and (AMode and 2 <> 0) then
    begin
      vSegAccuracy := GetAccuracyByFPoints(APS1, APS2, AAccuracy);
      if IsEqualFPoints(AP, AP1, vSegAccuracy) then
        Result := False;
      if IsEqualFPoints(AP, AP2, vSegAccuracy) then
        Result := False;
      if IsEqualFPoints(AP, APS1, vSegAccuracy) then
        Result := False;
      if IsEqualFPoints(AP, APS2, vSegAccuracy) then
        Result := False;
      if IsParralelinarSegments3D(APS1, APS2, AP1, AP2, vSegAccuracy) then
        Result := False;
    end;
    if Result and (AMode and 4 <> 0) then
    begin
      if IndexOfFPoint(APoints, AP, AAccuracy) > -1 then
        Result := False;
    end;
    if Result then
    begin
      vCnt := APoints.Count;
      APoints.Add(AP);
      Result := APoints.Count > vCnt;
    end;
  end;

var
  vPt1, vPt2, vCross: TFPoint;
  vPoints: TFPointList;
  I: Integer;
  vCrossModes: TsgByteSet;
begin
  Result := 0;
  if APts.FPointCount > 1 then
  begin
    if AMode and 2 <> 0 then
    begin
      if IsPointsInHalfPlane(AP1, AP2, APts, AAccuracy) then
        Exit;
    end;
    vPoints := TFPointList.Create;
    try
      vPoints.Capacity := APts.FPointCount div 2 + 1;
      vPoints.Sorted := True;
      vPoints.Duplicates := dupIgnore;
      vCrossModes := [2];
      if AMode and 4 <> 0 then
        Include(vCrossModes, 1);
      vPt1 := APts.FPoints[0];
      for I := 1 to APts.FPointCount - 1 do
      begin
        vPt2 := APts.FPoints[I];
        if not IsEqualFPoints(vPt1, vPt2) then
        begin
          if IsCrossSegmentsPts(AP1, AP2, vPt1, vPt2, @vCross, AAccuracy, vCrossModes) then
          begin
            if AddCrossPoint(vCross, vPt1, vPt2, vPoints) and AOnlyFirstPoint then
              Break;
          end;
          vPt1 := vPt2;
        end;
      end;
      if ACloesd and (APts.FPointCount > 2) then
      begin
        if (Result = 0) or (not AOnlyFirstPoint) then
        begin
          vPt1 := APts.FPoints[0];
          vPt2 := APts.FPoints[APts.FPointCount - 1];
          if IsCrossSegmentsPts(AP1, AP2, vPt1, vPt2, @vCross, AAccuracy) then
          begin
            AddCrossPoint(vCross, vPt1, vPt2, vPoints);
          end;
        end;
      end;
      Result := vPoints.Count;
      if Assigned(ACroses) then
        ACroses.Assign(vPoints);
    finally
      vPoints.Free;
    end;
  end;
end;

function IsCrossSegmentPolylineAndRect(const ARect: TsgPoints4;
  APts: IsgArrayFPoint; const ACloesd: Boolean; const ACroses: TFPointList;
  const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False): Integer;
var
  vPolyline: TFPointList;
begin
  vPolyline := TFPointList.Create;
  try
    vPolyline.Capacity := 4;
    vPolyline.Add(ARect[0]);
    vPolyline.Add(ARect[1]);
    vPolyline.Add(ARect[2]);
    vPolyline.Add(ARect[3]);
    Result := IsCrossPolylines(vPolyline, True, APts, ACloesd, ACroses, AMode,
      AOnlyFirstPoint);
  finally
    vPolyline.Free;
  end;
end;

function IsCrossPolylines(APts1: IsgArrayFPoint; const AClosed1: Boolean;
  APts2: IsgArrayFPoint; const AClosed2: Boolean;
  const ACroses: TFPointList; const AMode: Integer;
  const AOnlyFirstPoint: Boolean = False; AAccuracy: Double = 0;
  AStartFromEnd: Boolean = False): Integer;
var
  I, vCrossCnt, vInc: Integer;
  vIsRange: Boolean;
  vPt1, vPt2: TFPoint;
  vCrossPoints: TFPointList;
begin
  Result := 0;
  vCrossPoints := nil;
  try
    if APts1.FPointCount > 1 then
    begin
      if Assigned(ACroses) then
      begin
        vCrossPoints := TFPointList.Create;
        vCrossPoints.Capacity := 16;
      end;

      if AStartFromEnd then
      begin
        vInc := -1;
        I := APts1.FPointCount - 2;
        vPt1 := APts1.FPoints[APts1.FPointCount - 1]
      end
      else
      begin
        vInc := 1;
        I := 1;
        vPt1 := APts1.FPoints[0];
      end;
      vIsRange := True;
      while vIsRange do
      begin
        vPt2 := APts1.FPoints[I];
        if not IsEqualFPoints(vPt1, vPt2) then
        begin
          vCrossCnt := IsCrossSegmentPolylinePts(vPt1, vPt2, APts2, AClosed2,
            vCrossPoints, AMode, AOnlyFirstPoint, AAccuracy);
          if vCrossCnt > 0 then
          begin
            Inc(Result, vCrossCnt);
            if Assigned(vCrossPoints) then
            begin
              ACroses.AppendArray(vCrossPoints);
              vCrossPoints.Count := 0;
            end;
            if AOnlyFirstPoint then
              Break;
          end;
          vPt1 := vPt2;
        end;
        Inc(I, vInc);
        if AStartFromEnd then
          vIsRange := I > -1
        else
          vIsRange := I < APts1.FPointCount;
      end;
      if AClosed1 and (APts1.FPointCount > 2) then
      begin
        if (Result = 0) or (not AOnlyFirstPoint) then
        begin
          vCrossCnt := IsCrossSegmentPolylinePts(APts1.FPoints[0],
            APts1.FPoints[APts1.FPointCount - 1], APts2, AClosed2, vCrossPoints,
            AMode, AOnlyFirstPoint, AAccuracy);
          Inc(Result, vCrossCnt);
          if Assigned(vCrossPoints) then
            ACroses.AppendArray(vCrossPoints);
        end;
      end;
    end;
  finally
    FreeAndNil(vCrossPoints);
  end;
end;

function IsPointsInHalfPlane(const AP1,AP2: TFPoint; APts: IsgArrayFPoint;
  const AAccuracy: Double = 0): Boolean;
var
  I: Integer;
  vClassify, vFirst: TsgPointClassify;
begin
  Result := True;
  if APts.FPointCount > 0 then
  begin
    vFirst := pcBETWEEN;
    for I := 0 to APts.FPointCount - 1 do
    begin
      vClassify := PointClassify(APts.FPoints[I], AP1, AP2, AAccuracy);
      if not (vClassify in [pcBEYOND, pcBEHIND, pcBETWEEN, pcORIGIN, pcDESTINATION]) then
      begin
        if vFirst = pcBETWEEN then
          vFirst := vClassify
        else
          if vFirst <> vClassify  then
          begin
            Result := False;
            Break;
          end;
      end;
    end;
  end;
end;

function IsCrossSegments(const ALine1, ALine2: TsgLine; const ACross: PFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsCrossCoords(ALine1.Point1.X, ALine1.Point1.Y,
   ALine1.Point2.X, ALine1.Point2.Y, ALine2.Point1.X, ALine2.Point1.Y,
   ALine2.Point2.X, ALine2.Point2.Y, ACross, AAccuracy) = 2;
end;

function IsCrossLines2D(const ALine1, ALine2: TF2DLine; const ACross: PF2DPoint): Boolean;
var
  Pnt: TFPoint;
begin
  if ACross = nil then
    Result := IsCrossCoords(ALine1.Point1.X, ALine1.Point1.Y,
      ALine1.Point2.X, ALine1.Point2.Y, ALine2.Point1.X, ALine2.Point1.Y,
      ALine2.Point2.X, ALine2.Point2.Y, nil) > 0
  else
  begin
    Result := IsCrossCoords(ALine1.Point1.X, ALine1.Point1.Y,
      ALine1.Point2.X, ALine1.Point2.Y, ALine2.Point1.X, ALine2.Point1.Y,
      ALine2.Point2.X, ALine2.Point2.Y, @Pnt) > 0;
    if Result then
    begin
      ACross^.X := Pnt.X;
      ACross^.Y := Pnt.Y;
    end;
  end;
end;

function IsCrossLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsCrossCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X, AP21.Y,
    AP22.X, AP22.Y, ACross, AAccuracy) > 0;
end;

function IsCrossSegmentAndLinesPts(const APS1, APS2, APL1, APL2: TFPoint;
  const ACross: PFPoint; const AAccuracy: Double = fDoubleResolution): Boolean;
var
  vCross: TFPoint;
begin
  Result := False;
  if IsCrossCoords(APS1.X, APS1.Y, APS2.X, APS2.Y, APL1.X, APL1.Y,
     APL2.X, APL2.Y, @vCross, AAccuracy) > 0 then
  begin
    if IsPointOnSegmentPts(APS1, APS2, vCross, AAccuracy) then
    begin
      Result := True;
      if ACross <> nil then
        ACross^ := vCross;
    end;
  end;
end;

function IsCrossArcAndBoxAR(const AArc: TsgArcR; const ABox: TFRect): TFPointList;
var
  vCross1, vCross2: TFPoint;
  vCrossRez: Integer;
  vBoxPoints: TsgPoints5;
  I: Integer;
begin
  Result := nil;
  GetRectangle(ABox, vBoxPoints);
  for I := Low(vBoxPoints) to High(vBoxPoints) - 1 do
  begin
    vCrossRez := IsCrossArcAndSegmentAP(AArc, MakeLine(vBoxPoints[I], vBoxPoints[I + 1]), @vCross1, @vCross2);
    if vCrossRez > 0 then
    begin
      if not Assigned(Result) then
      begin
        Result := TFPointList.Create;
        Result.Duplicates := dupIgnore;
        Result.Sorted := True;
      end;
      Result.Add(vCross1);
      if (vCrossRez > 1) and (not IsEqualFPoints(vCross1, vCross2, 0)) then
        Result.Add(vCross2);
    end;
  end;
end;

function IsCrossLineAndBox(const AP1, AP2: TFPoint; const ABox: TFRect;
  var ACross1, ACross2: TFPoint; const AMode: Byte = 1): Integer;
var
  vCrossBox: array [0..3] of TFPoint;
  vCrossRez: array [0..3] of Integer;
  vBoxPoints: TsgPoints5;
  I: Integer;
begin
  Result := 0;
  GetRectangle(ABox, vBoxPoints);
  for I := Low(vBoxPoints) to High(vBoxPoints) - 1 do
  begin
    case AMode of
      1, 2:
        begin
          vCrossRez[I] := Integer(IsCrossLinesPts(AP1, AP2, vBoxPoints[I], vBoxPoints[I + 1], @vCrossBox[I]));
          if vCrossRez[I] > 0 then
          begin
            if AMode = 1 then
            begin
              if not IsPointOnSegmentPts(vBoxPoints[I], vBoxPoints[I + 1], vCrossBox[I]) then
                vCrossRez[I] := -vCrossRez[I];
            end
            else
            begin
              if not IsPointOnSegmentPts(AP1, AP2, vCrossBox[I]) then
                vCrossRez[I] := -vCrossRez[I];
            end;
          end;
        end;
      3: vCrossRez[I] := Integer(IsCrossSegmentsPts(AP1, AP2, vBoxPoints[I], vBoxPoints[I + 1], @vCrossBox[I]));
    else//0
      VCrossRez[I] := Integer(IsCrossLinesPts(AP1, AP2, vBoxPoints[I], vBoxPoints[I + 1], @vCrossBox[I]));
    end;
    if vCrossRez[I] > 0 then
    begin
      if Result = 0 then
      begin
        Result := 1;
        ACross1 := vCrossBox[I];
      end
      else
      begin
        if not IsEqualFPoints(ACross1, vCrossBox[I]) then
        begin
          Result := 2;
          ACross2 := vCrossBox[I];
          Break;
        end;
      end;
    end;
  end;
end;

function IsCrossLines(const ALine1, ALine2: TsgLine; const ACross: PFPoint;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := IsCrossCoords(ALine1.Point1.X, ALine1.Point1.Y, ALine1.Point2.X,
    ALine1.Point2.Y, ALine2.Point1.X, ALine2.Point1.Y, ALine2.Point2.X,
    ALine2.Point2.Y, ACross, AAccuracy) > 0;
end;

function IsCrossPointsPts(const AP11, AP12, AP21, AP22: TFPoint;
  const AsSegment: Boolean; const APoint: PFPoint): Boolean;
begin
  if AsSegment then
    Result := IsCrossSegmentsPts(AP11, AP12, AP21, AP22, APoint)
  else
    Result := IsCrossLinesPts(AP11, AP12, AP21, AP22, APoint);
end;

function IsParalleniarLines(const L1, L2: TsgLine;
  const Accuracy: Double = fAccuracy): Boolean;
begin
  Result := IsParalleniarLinesCoords(L1.Point1.X, L1.Point1.Y, L1.Point2.X,
    L1.Point2.Y, L2.Point1.X, L2.Point1.Y, L2.Point2.X, L2.Point2.Y, Accuracy);
end;

function IsParalleniarLinesCoords(const X11, Y11, X12, Y12, X21, Y21, X22,
  Y22, Accuracy: Double): Boolean;
var
  vDenom: Extended;
begin
  vDenom := DenomVectors((X12 - X11), (Y12 - Y11), (X22 - X21), (Y22 - Y21));
  Result := Abs(vDenom) <= Accuracy;
end;

function IsPerpendicularLinesCoords(const X11, Y11, X12, Y12, X21,
  Y21, X22, Y22, Accuracy: Double): Boolean;
var
  vScalar: Extended;
begin
  vScalar := ScalarMultiplyVectorsCoords((X12 - X11), (Y12 - Y11), (X22 - X21),
    (Y22 - Y21));
  Result := Abs(vScalar) <= Accuracy;
end;

function IsParalleniarLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const Accuracy: Double = fAccuracy): Boolean;
begin
  Result := IsParalleniarLinesCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X,
    AP21.Y, AP22.X, AP22.Y, Accuracy);
end;

function IsParalleniarLinesPts2D(const AP11, AP12, AP21, AP22: TF2DPoint;
  const Accuracy: Double = fAccuracy): Boolean;
begin
  Result := IsParalleniarLinesCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X,
    AP21.Y, AP22.X, AP22.Y, Accuracy);
end;

function IsPerpendicularLinesPts(const AP11, AP12, AP21, AP22: TFPoint;
  const Accuracy: Double = fAccuracy): Boolean;
begin
  Result := IsPerpendicularLinesCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X,
    AP21.Y, AP22.X, AP22.Y, Accuracy);
end;

function IsPerpendicularLinesPts2D(const AP11, AP12, AP21, AP22: TF2DPoint;
  const Accuracy: Double = fAccuracy): Boolean;
begin
  Result := IsPerpendicularLinesCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X,
    AP21.Y, AP22.X, AP22.Y, Accuracy);
end;

function IsCrossCirclesAP(const Arc1, Arc2: TsgArcR; const ACross1, ACross2: PFPoint): Integer;
begin
  Result := IsCrossCirclesCR(Arc1.Center, Arc1.Radius, Arc2.Center, Arc2.Radius,
    ACross1, ACross2);
end;

function IsCrossCircles(const ArcPts1, ArcPts2: TsgPoints3; const ACross1, ACross2: PFPoint): Integer;
var
  vRadius1, vRadius2: Double;
begin
  vRadius1 := DistanceFPoint2D(ArcPts1[0], ArcPts1[1]);
  vRadius2 := DistanceFPoint2D(ArcPts2[0], ArcPts2[1]);
  Result := IsCrossCirclesCR(ArcPts1[0], vRadius1, ArcPts2[0], vRadius2, ACross1, ACross2);
end;

function IsCrossCirclesCR(const AC1: TFPoint; const AR1: Double;
  const AC2: TFPoint; const AR2: Double; const ACross1, ACross2: PFPoint;
  Epsilon: Double = fDoubleResolution): Integer;
var
  A, R1, R2, R12, D, H, vDX, vDY, vDXdivD, vDYdivD, vPX, vPY: Extended;
begin
  Result := 0;
  vDX := AC2.X - AC1.X;
  vDY := AC2.Y - AC1.Y;
  D := Sqr(vDX) + Sqr(vDY);
  if D > fDoubleResolution then
  begin
    R1 := AR1;
    R2 := AR2;
    R12 := R1 + R2;
    D := Sqrt(D);
    vDXdivD := vDX / D;
    vDYdivD := vDY / D;
    if IsEqual(D, R12) then
    begin
      Result := 1;
      if ACross1 <> nil then
      begin
        ACross1^.X := AC1.X + AR1 * vDXdivD;
        ACross1^.Y := AC1.Y + AR1 * vDYdivD;
        ACross1^.Z := 0;
      end;
    end
    else
    begin
      if (D < R12) and ((R1 <= D + R2) or IsZero(R1 - D - R2, Epsilon)) and
        ((R2 <= D + R1) or IsZero(R2 - D - R1, Epsilon)) then
      begin
        Result := 2;
        if (ACross1 <> nil) and (ACross2 <> nil) then
        begin
          A := (Sqr(R1) - Sqr(R2) + Sqr(D)) / (2 * D);
          H := Sqr(R1) - Sqr(A);
          if H > fDoubleResolution then
          begin
            H := Sqrt(H);
            vPX := AC1.X + A * vDXdivD;
            vPY := AC1.Y + A * vDYdivD;
            ACross1^.X := vPX + H * vDYdivD;
            ACross1^.Y := vPY - H * vDXdivD;
            ACross1^.Z := 0;
            ACross2^.X := vPX - H * vDYdivD;
            ACross2^.Y := vPY + H * vDXdivD;
            ACross2^.Z := 0;
          end
          else
          begin
            Result := 1;
            if ACross1 <> nil then
            begin
              ACross1^.Z := 0;            
              if AR1 < AR2 then
              begin
                ACross1^.X := AC2.X - AR2 * vDXdivD;
                ACross1^.Y := AC2.Y - AR2 * vDYdivD;
              end
              else
              begin
                ACross1^.X := AC1.X + AR1 * vDXdivD;
                ACross1^.Y := AC1.Y + AR1 * vDYdivD;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function IsCrossCircleAndArcAP(const ACircle, AArc: TsgArcR; const ACross1, ACross2: PFPoint): Integer;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossCirclesAP(ACircle, AArc, @vCross1, @vCross2) of
    1:
      begin
        if IsPointOnArcAP(AArc, vCross1) then
        begin
          Result := 1;
          if ACross1 <> nil then
            ACross1^ := vCross1;
        end;
      end;
    2:
      begin
        if IsPointOnArcAP(AArc, vCross1) then
        begin
          Result := 1;
          if ACross1 <> nil then
            ACross1^ := vCross1;
        end;
        if IsPointOnArcAP(AArc, vCross2) then
        begin
          Inc(Result);
          case Result of
            1:
              begin
                if ACross1 <> nil then
                  ACross1^ := vCross2;
              end;
            2:
              begin
                if ACross2 <> nil then
                  ACross2^ := vCross2;
              end;
          end;
        end;
      end;
  end;
end;


function IsCrossCircleAndLine(const ArcPts: TsgPoints3; const ALine: TsgLine;
  const ACross1, ACross2: PFPoint): Integer;
var
  vRadius: Double;
begin
  vRadius := DistanceFPoint2D(ArcPts[0], ArcPts[1]);
  Result := IsCrossCircleAndLineCR(ArcPts[0], vRadius, ALine, ACross1, ACross2);
end;

function IsCrossCircleAndLineAP(const AP: TsgArcR; const ALine: TsgLine;
 const ACross1, ACross2: PFPoint): Integer;
begin
  Result := IsCrossCircleAndLineCR(AP.Center, AP.Radius, ALine, ACross1, ACross2);
end;

function IsCrossCircleAndSegmentAP(const AP: TsgArcR; const ALine: TsgLine;
  const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
begin
  Result := IsCrossCircleAndSegmentCR(AP.Center, AP.Radius, ALine, ACross1, ACross2, AAccuracy);
end;

function IsCrossCircleAndLinePntsAP(const AP: TsgArcR; const AP1, AP2: TFPoint;
  const ACross1, ACross2: PFPoint): Integer;
var
  vLine: TsgLine;
begin
  vLine.Point1 := AP1;
  vLine.Point2 := AP2;
  Result := IsCrossCircleAndLineCR(AP.Center, AP.Radius, vLine, ACross1, ACross2);
end;

function IsCrossCircleAndLineCR(const Center: TFPoint; const ARadius: Double;
  const ALine: TsgLine; const ACross1, ACross2: PFPoint;
  const Epsilon: Double = fDoubleResolution): Integer;
var
  vP: TFPoint;
  A, B, C, H, kk, bb, Tmp, V1, V2: Double;
  vMode, vModeEx: Integer;
begin
  Result := 0;
  vP := PointCrossedPerpendicularLines(ALine.Point1, ALine.Point2, Center);
  H := DistanceFPoint2D(vP, Center);
  if IsEqual(H, ARadius, Epsilon) then
  begin
    Result := 1;
    if ACross1 <> nil then
      ACross1^ := vP;
  end
  else
  begin
    if H < ARadius then
    begin
      vP := SubFPoint2D(ALine.Point2, ALine.Point1);
      if not sgIsZero(vP.X, Epsilon) then
      begin
        vMode := 0;
        if Abs(vP.Y) > Abs(vP.X) then
          vMode := 1;
      end
      else
        if not sgIsZero(vP.Y, Epsilon) then
          vMode := 1
        else
          vMode := -1;
      if vMode > -1 then
      begin
        vModeEx := Integer(not Boolean(vMode));
        kk := vP.V[vModeEx] / vP.V[vMode];
        bb := ALine.Point1.V[vModeEx] - kk * ALine.Point1.V[vMode];
        Tmp := bb - Center.V[vModeEx];
        A := kk * kk + 1;
        B := 2 * (kk * Tmp - Center.V[vMode]);
        C := Tmp * Tmp + Center.V[vMode] * Center.V[vMode] - ARadius * ARadius;
        Result := GetRezOfQuadraticEquation(A, B, C, V1, V2);
        if (Result > 0) and (ACross1 <> nil) and (ACross2 <> nil) then
        begin
          ACross1^.V[vMode] := V1;
          ACross1^.V[vModeEx] := kk * V1 + bb;
          ACross1^.Z := 0;
          if Result > 1 then
          begin
            ACross2^.V[vMode] := V2;
            ACross2^.V[vModeEx] := kk * V2 + bb;
            ACross2^.Z := 0;
          end
          else
            ACross2^ := ACross1^;
        end;
      end;
    end;
  end;
end;

function IsCrossCircleAndSegmentCR(const Center: TFPoint; const ARadius: Double; const ALine: TsgLine;
 const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossCircleAndLineCR(Center, ARadius, ALine, @vCross1, @vCross2) of
    1:
      begin
        if IsPointOnSegment(ALine, vCross1, AAccuracy) then
        begin
          Result := 1;
          if ACross1 <> nil then
            ACross1^ := vCross1;
        end;
      end;
    2:
      begin
        case Byte(IsPointOnSegment(ALine, vCross1, AAccuracy)) + Byte(IsPointOnSegment(ALine, vCross2, AAccuracy)) shl 1 of
          1:
            begin
              Result := 1;
              if ACross1 <> nil then
                ACross1^ := vCross1;
            end;
          2:
            begin
              Result := 1;
              if ACross1 <> nil then
                ACross1^ := vCross2;
            end;
          3:
            begin
              Result := 2;
              if ACross1 <> nil then
                ACross1^ := vCross1;
              if ACross2 <> nil then
                ACross2^ := vCross2;
            end;
        end;
      end;
  end;
end;

function IsCrossArcs(const ArcPts1, ArcPts2: TsgPoints3; const ACross1, ACross2: PFPoint): Integer;
var
  vCross1, vCross2: TFPoint;
  vIsSetParams: Boolean;
begin
  vIsSetParams := (ACross1 <> nil) and (ACross2 <> nil);
  Result := IsCrossCircles(ArcPts1, ArcPts2, @vCross1, @vCross2);
  case Result of
    1:
      begin
        if IsPointOnArcs(ArcPts1, ArcPts2, vCross1) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1
        end
        else
          Result := 0;
      end;
    2:
      begin
        if IsPointOnArcs(ArcPts1, ArcPts2, vCross1) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1;
        end
        else
          Result := 1;
        if IsPointOnArcs(ArcPts1, ArcPts2, vCross2) then
        begin
          if vIsSetParams then
          begin
            if Result = 1 then
              ACross1^ := vCross2
            else
              ACross2^ := vCross2;
          end;
        end
        else
          Dec(Result);
      end;
  end;
end;

function IsCrossArcsAP(const A1, A2: TsgArcR; const ACross1, ACross2: PFPoint; Epsilon: Double = fDoubleResolution): Integer;
var
  vCross1, vCross2: TFPoint;
  vIsSetParams: Boolean;
begin
  vIsSetParams := (ACross1 <> nil) and (ACross2 <> nil);
  Result := IsCrossCirclesCR(A1.Center, A1.Radius, A2.Center, A2.Radius, @vCross1, @vCross2, Epsilon);
  case Result of
    1:
      begin
        if IsPointOnArcAP(A1, vCross1, Epsilon) and IsPointOnArcAP(A2, vCross1, Epsilon) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1
        end
        else
          Result := 0;
      end;
    2:
      begin
        if IsPointOnArcAP(A1, vCross1, Epsilon) and IsPointOnArcAP(A2, vCross1, Epsilon) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1;
        end
        else
          Result := 1;
        if IsPointOnArcAP(A1, vCross2, Epsilon) and IsPointOnArcAP(A2, vCross2, Epsilon) then
        begin
          if vIsSetParams then
          begin
            if Result = 1 then
              ACross1^ := vCross2
            else
              ACross2^ := vCross2;
          end;
        end
        else
          Dec(Result);
      end;
  end;
end;

function IsCrossArcAndSegment(const ArcPts: TsgPoints3; const ALine: TsgLine; const ACross1, ACross2: PFPoint): Integer;
var
  vCross1, vCross2: TFPoint;
  vIsSetParams: Boolean;
begin
  vIsSetParams := (ACross1 <> nil) and (ACross2 <> nil);
  Result := IsCrossCircleAndLine(ArcPts, ALine, @vCross1, @vCross2);
  case Result of
    1:
      begin
        if IsPointOnSegment(ALine, vCross1) and
           IsPointOnArc(ArcPts[0], ArcPts[1], ArcPts[2], vCross1) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1
        end
        else
          Result := 0;
      end;
    2:
      begin
        if IsPointOnSegment(ALine, vCross1) and IsPointOnArc(ArcPts[0], ArcPts[1], ArcPts[2], vCross1) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1;
        end
        else
          Result := 1;
        if IsPointOnSegment(ALine, vCross2) and IsPointOnArc(ArcPts[0], ArcPts[1], ArcPts[2], vCross2) then
        begin
          if vIsSetParams then
          begin
            if Result = 1 then
              ACross1^ := vCross2
            else
              ACross2^ := vCross2;
          end;
        end
        else
          Dec(Result);
      end;
  end;
end;

function IsCrossArcAndSegmentAP(const AP: TsgArcR; const ALine: TsgLine; const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
begin
  Result := IsCrossArcAndSegmentCR(AP.Center, AP.Radius, AP.AngleS, AP.AngleE, ALine, ACross1, ACross2, AAccuracy);
end;

function IsCrossArcAndSegmentCR(const C: TFPoint; const Radius, Angle1, Angle2: Double; const ALine: TsgLine;
  const ACross1, ACross2: PFPoint; const AAccuracy: Double = 0): Integer;
var
  vCross1, vCross2: TFPoint;
  vIsSetParams: Boolean;
begin
  vIsSetParams := (ACross1 <> nil) and (ACross2 <> nil);
  Result := IsCrossCircleAndLineCR(C, Radius, ALine, @vCross1, @vCross2);
  case Result of
    1:
      begin
        if IsPointOnSegment(ALine, vCross1, AAccuracy) and IsAngleInAngles(GetAngleByPoints(C, vCross1, False), Angle1, Angle2, AAccuracy) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1
        end
        else
          Result := 0;
      end;
    2:
      begin
        if IsPointOnSegment(ALine, vCross1, AAccuracy) and IsAngleInAngles(GetAngleByPoints(C, vCross1, False), Angle1, Angle2, AAccuracy) then
        begin
          if vIsSetParams then
            ACross1^ := vCross1;
        end
        else
          Result := 1;
        if IsPointOnSegment(ALine, vCross2, AAccuracy) and IsAngleInAngles(GetAngleByPoints(C, vCross2, False), Angle1, Angle2, AAccuracy) then
        begin
          if vIsSetParams then
          begin
            if Result = 1 then
              ACross1^ := vCross2
            else
              ACross2^ := vCross2;
          end;
        end
        else
          Dec(Result);
      end;
  end;
end;

function IsCrossArcAndPntsAP(const Arc: TsgArcR; const AP1, AP2: TFPoint; const Cross: PFPoint): Boolean;
var
  vC1, vC2: TFPoint;
begin
  case IsCrossCircleAndLineCR(Arc.Center, Arc.Radius, MakeLine(AP1, AP2), @vC1, @vC2) of
    1:
      begin
        if Cross <> nil then
          Cross^ := vC1;
        Result := True;
      end;
    2:
      begin
        if Cross <> nil then
        begin
          if IsPointOnCircleCR(Arc.Center, Arc.Radius, AP1) then
          begin
            if DistanceFPoint2DSqr(AP1, vC1) < DistanceFPoint2DSqr(AP1, vC2) then
             Cross^ := vC1
            else
             Cross^ := vC2;
          end
          else
            if IsPointOnCircleCR(Arc.Center, Arc.Radius, AP2) then
            begin
              if DistanceFPoint2DSqr(AP2, vC1) < DistanceFPoint2DSqr(AP2, vC2) then
               Cross^ := vC1
              else
               Cross^ := vC2;
            end
            else
            begin
              if IsEqualDirection(vC1, AP1, Arc.Center) then
               Cross^ := vC1
              else
               Cross^ := vC2;
            end;
        end;    
        Result := True;
      end;
  else
    Result := False;
  end;
end;

function IsCrossPoints4AndPolyPolygon(APoints: TsgPoints4; AClosed: Boolean;
  ABoundaries: TList; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
var
  vBoundaries: TsgObjectList;
begin
  vBoundaries := TsgObjectList.Create;
  try
    Result := IsCrossPoints4AndPolyPolygonList(APoints, AClosed, vBoundaries,
      ACrosses, AMode, AAccuracy);
    vBoundaries.CopyTo(ABoundaries);
  finally
    vBoundaries.Free;
  end;
end;

function IsCrossPoints4AndPolyPolygonList(APoints: TsgPoints4; AClosed: Boolean;
  ABoundaries: TsgObjectList; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
var
  vPolyPolyline: TsgObjectList;
  vPolinline: TFPointList;
begin
  vPolyPolyline := TsgObjectList.Create;
  try
    vPolinline := TFPointList.Create;
    vPolyPolyline.Add(vPolinline);
    vPolinline.AssignArray(APoints);
    Result := IsCrossPolyPolygonAndPolyPolygonList(vPolyPolyline, ABoundaries,
      AClosed, ACrosses, AMode, AAccuracy);
  finally
    TsgObjectList.FreeList(vPolyPolyline);
  end;
end;

function IsCrossPointsI(const AP11, AP12, AP21, AP22: TPoint; const ACross: PPoint): Integer;
var
  vCrossTmp: TFPoint;
  vCross: TPoint;
  vBox12, vBox22: TRect;
begin
  Result := 0;
  if IsCrossCoords(AP11.X, AP11.Y, AP12.X, AP12.Y, AP21.X, AP21.Y,
    AP22.X, AP22.Y, @vCrossTmp, fAccuracy) = 2 then
  begin
    Result := 1;
    vCross.X := RoundToInt(vCrossTmp.X);
    vCross.Y := RoundToInt(vCrossTmp.Y);
    vBox12 := GetBoxOfPointI(AP11, AP12);
    if IsPointInRectI(vBox12, vCross) then
    begin
      vBox22 := GetBoxOfPointI(AP21, AP22);
      if IsPointInRectI(vBox22, vCross) then
        Result := 2;
    end;
    if ACross <> nil then
      ACross^ := vCross;
  end;
end;

function IsEqualArraysFPoints(APts1,APts2: IsgArrayFPoint;
  AAccuracy: Double = 0): Boolean;
var
  I: Integer;
  vPts1,vPts2: IsgArrayFPoint;
begin
  if APts1.FPointCount <= APts2.FPointCount then
  begin
    vPts1 := APts1;
    vPts2 := APts2;
  end
  else
  begin
    vPts1 := APts2;
    vPts2 := APts1;
  end;
  if vPts1.FPointCount = vPts2.FPointCount then
    Result := True
  else
    Result := (vPts1.FPointCount + 1 = vPts2.FPointCount);
  if Result then
    for I := 0 to vPts1.FPointCount - 1 do
      if not IsEqualFPoints(vPts1.FPoints[I], vPts2.FPoints[I], AAccuracy) then
      begin
        Result := False;
        Break;
      end;
  if Result and (vPts1.FPointCount < vPts2.FPointCount) then
    Result := IsEqualFPoints(vPts1.FPoints[0], vPts2.FPoints[vPts2.FPointCount - 1], AAccuracy);
end;

function IsCrossPolyPolygonAndPolyPolygon(APoly, ABoundaries: TList;
  AClosed: Boolean; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
var
  vPoly, vBoundaries: TsgObjectList;
begin
  vPoly := TsgObjectList.Create;
  vBoundaries := TsgObjectList.Create;
  try
    vPoly.CopyFrom(APoly);
    vBoundaries.CopyFrom(ABoundaries);
    Result := IsCrossPolyPolygonAndPolyPolygonList(vPoly, vBoundaries,
      AClosed, ACrosses, AMode, AAccuracy);
  finally
    vBoundaries.Free;
    vPoly.Free;
  end;
end;

function IsCrossPolyPolygonAndPolyPolygonList(APoly, ABoundaries: TsgObjectList;
  AClosed: Boolean; ACrosses: TFPointList; AMode: Integer;
  AAccuracy: Double = 0): Integer;
var
  I,J,vCross,vLoc0,vLoc1: Integer;
  vBoundary,vPolygon: IsgArrayFPoint;
begin
  vLoc0 := 0;
  vLoc1 := 0;
  for I := 0 to APoly.Count - 1 do
  begin
    TInterfacedObject(APoly[I]).GetInterface(GUID_ArrayFPoints, vPolygon);
    for J := 0 to ABoundaries.Count - 1 do
    begin
      TInterfacedObject(ABoundaries[J]).GetInterface(GUID_ArrayFPoints, vBoundary);
      vCross := IsCrossPolylines(vPolygon, AClosed, vBoundary, True, ACrosses,
        AMode, False, AAccuracy);
      if vCross > 0 then
      begin
        Result := 2;
        Exit;
      end
      else
        if IsEqualArraysFPoints(vPolygon, vBoundary, AAccuracy) then
        begin
          Result := 3;
          Exit;
        end
        else
          case IsPointsInPolyline(vPolygon, vBoundary, nil, AAccuracy, AMode) of
            -1: Inc(vLoc0);
            1: Inc(vLoc1);
          else
            Result := 2;
            Exit;
          end;
    end;
  end;
  if (vLoc0 > 0) and (vLoc1 = 0) then
    Result := 0
  else
    if (vLoc0 = 0) and (vLoc1 > 0) then
      Result := 1
    else
      Result := 2;
end;

function IsCrossPerpendicularLines(const AP1, AP2, AP3: TFPoint; const ACross: PFPoint): Boolean;
var
  K1, K2, K3, K4, L, R, S: Extended;
begin
  L := DistanceFPoint2DSqr(AP1, AP2);
  if not sgIsZero(L) then
  begin
    K1 := AP1.Y - AP3.Y;
    K2 := AP2.Y - AP1.Y;
    K3 := AP1.X - AP3.X;
    K4 := AP1.X - AP2.X;
    S := (K1 * K4 + K3 * K2) / L;
    Result := sgIsZero(S);
    if Result and (ACross <> nil) then
    begin
      R := (K1 * K2 - K3 * K4) / L;
      ACross^.X := AP1.X - R * K4;
      ACross^.Y := AP1.Y + R * K2;
      ACross^.Z := 0;
    end;
  end
  else
  begin
    Result := IsEqualFPoints2D(AP1, AP3);
    if Result and (ACross <> nil) then
      ACross^ := AP1;
  end;
end;

procedure CheckTangents(const ACenter: TFPoint; AStart, AEnd: Double;
  var ATangentCount: Integer; var AP1, AP2: TFPoint);
var
  vAngle: Double;
begin
  if ATangentCount < 0 then Exit;
  CheckAngles(AStart, AEnd);
  vAngle := GetAngleByPoints(ACenter, AP1, False);
  if IsAngleInAngles(vAngle, AStart, AEnd) then
  begin
    if ATangentCount > 1 then
    begin
      vAngle := GetAngleByPoints(ACenter, AP2, False);
      if not IsAngleInAngles(vAngle, AStart, AEnd) then
        Dec(ATangentCount);
    end;
  end
  else
  begin
    Dec(ATangentCount);
    if ATangentCount > 0 then
    begin
      vAngle := GetAngleByPoints(ACenter, AP2, False);
      if IsAngleInAngles(vAngle, AStart, AEnd) then
        AP1 := AP2
      else
        Dec(ATangentCount);
    end;
  end;
end;

function IsTangentOfArc(const ACenter: TFPoint; const AR, AStart, AEnd: Double;
  const APoint: TFPoint; var AP1, AP2: TFPoint): Integer;
begin
  Result := IsTangentOfCircle(ACenter, AR, APoint, AP1, AP2);
  if Result > 0 then
    CheckTangents(ACenter, AStart, AEnd, Result, AP1, AP2);
end;

function IsTangentOfCircle(const ACenter: TFPoint; const AR: Double;
   const APoint: TFPoint; var AP1, AP2: TFPoint): Integer;
var
  AC, AB, BC: Extended;
  vP1, vP2: TFPoint;
begin
  Result := 0;
  AC := DistanceFPoint(ACenter, APoint);
  if (AR > fDoubleResolution) and (AC > AR) then
  begin
    BC := AR;
    AB := Sqrt(Sqr(AC) - Sqr(BC));
    Result := IsCrossCirclesCR(ACenter, AR, APoint, AB, @vP1, @vP2);
    case Result of
      0:  begin end;
      1:  AP1 := vP1;
    else
      AP1 := vP1;
      AP2 := vP2;
    end;
  end;
end;

function IsTangentOfEllipse(const ACenter, ARatPt: TFPoint; const ARatio,
  AStart, AEnd: Double; const APoint: TFPoint; var AP1, AP2: TFPoint): Integer;
var
  A, B, D, x1, y1, tmp1, tmp2, tmp3: Extended;
  K, M, X, Y: array [0..1] of Extended;
  vAxisAngle: Double;
  vPoint: TFPoint;
  I, vPointsCount: Integer;
begin
  Result := 0;
  A := DistanceFVector(ARatPt);
  if ARatio = 1 then
    Result := IsTangentOfCircle(ACenter, A, APoint, AP1, AP2)
  else
  begin
    B := A * ARatio;
    vAxisAngle := GetAngleByPoints(cnstFPointZero, ARatPt, False);
    vPoint := RotateFPoint(SubFPoint(APoint, ACenter), -vAxisAngle);
    x1 := vPoint.X;
    y1 := vPoint.Y;
    tmp1 := Sqr(x1) - Sqr(A);
    if tmp1 <> 0 then
    begin
      D := Sqr(A * y1) + Sqr(B * x1) - Sqr(A * B);
      if D >= 0 then
      begin
        tmp2 := x1 * y1;
        if D = 0 then
        begin
          vPointsCount := 1;
          K[0] := tmp2 / tmp1;
          K[1] := K[0];
        end
        else
        begin
          vPointsCount := 2;
          D := Sqrt(D);
          K[0] := (tmp2 - D) / tmp1;
          K[1] := (tmp2 + D) / tmp1;
        end;
        for I := 0 to vPointsCount - 1 do
        begin
          tmp3 := Sqr(B) + Sqr(A * K[I]);
          if tmp3 <> 0 then
          begin
            M[I] := y1 - K[I] * x1;
            X[Result] := - Sqr(A) * K[I] * M[I] / tmp3;
            Y[Result] := K[I] * X[Result] + M[I];
            Inc(Result);
          end;
        end;
        AP1 := AddFPoint(RotateFPoint(MakeFPoint(X[0], Y[0], vPoint.Z),
          vAxisAngle), ACenter);
        if Result > 1 then
          AP2 := AddFPoint(RotateFPoint(MakeFPoint(X[1], Y[1], vPoint.Z),
            vAxisAngle), ACenter)
        else
          AP2 := AP1;
      end;
    end;
  end;
  CheckTangents(ACenter, AStart, AEnd, Result, AP1, AP2);
end;

function IsAngleInAngles(const Angle, A1, A2: Double; AEpsilon: Double = fDoubleResolution): Boolean;
begin
  if IsEqual(A1, A2, AEpsilon) then
    Result := IsEqual(A1, Angle, AEpsilon)
  else
    if A1 < A2 then
    begin
      Result := (Angle >= A1) and (Angle <= A2);
      if not Result then
        Result := IsEqual(Angle, A1, AEpsilon) or IsEqual(Angle, A2, AEpsilon) or
          (IsEqual(Angle, 0, AEpsilon) and IsEqual(A2, 360, AEpsilon));
    end
    else
    begin
      Result := ((Angle >= A1) and (Angle <= 360)) or ((Angle >= 0) and (Angle <= A2));
      if not Result then
        Result := IsEqual(Angle, A1, AEpsilon) or IsEqual(Angle, A2, AEpsilon) or
          IsEqual(Angle, 360, AEpsilon) or IsEqual(Angle, 0, AEpsilon);
    end;
end;

function IsAngleInAnglesAP(const Angle: Double; const Arc: TsgArcR;
  const AEpsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsAngleInAngles(Angle, Arc.AngleS, Arc.AngleE, AEpsilon);
end;

function IsAnglesInAngles(const AIn1, AIn2, AOut1, AOut2: Double): Boolean;
begin
  Result := IsAngleInAngles(AIn1, AOut1, AOut2);
  if Result and (not IsEqual(AIn1, AIn2)) then
    Result := not IsAngleInAngles(AIn1, AOut2, AOut1);
end;

function GetAngleByPoints(const ACenter, APoint: TFPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution;
  const AOrtoX: Integer = 1; const AOrtoY :Integer = 1): Double;
begin
  Result := sgAngleByPoints(ACenter, APoint, AIsRadian, Epsilon, AOrtoX, AOrtoY);
end;

function GetAngleByF2DPoints(const ACenter, APoint: TF2DPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution): Double;
begin
  Result := GetAngleByPoints(MakeFPointFrom2D(ACenter), MakeFPointFrom2D(APoint),
    AIsRadian, Epsilon);
end;

function GetAngleByPointsI(const ACenter, APoint: TPoint; const AIsRadian: Boolean): Double;
begin
  Result := GetAngleByPoints(MakeFPoint(ACenter.X, ACenter.Y, 0), MakeFPoint(APoint.X, APoint.Y, 0), AIsRadian);
end;

function GetAngleOfLines(const ALine1, ALine2: TsgLine; const Epsilon: Double = fDoubleResolution): Double;
var
  vBase, vP1, vP2: TFPoint;
  A, B, C, BC, vCosA: Extended;

  function GetActPts(const PLine: PsgLine): TFPoint;
  var
    vD1, vD2: Double;
  begin
    vD1 := DistanceFPoint2D(PLine^.Point1, vBase);
    vD2 := DistanceFPoint2D(PLine^.Point2, vBase);
    case Integer(vD1 > Epsilon) shl 1 + Integer(vD2 > Epsilon) of
      0, 2:  Result := PLine^.Point1;
      1:     Result := PLine^.Point2;
    else
      if vD1 > vD2 then
        Result := PLine^.Point2
      else
        Result := PLine^.Point1;
    end;
  end;

begin
  if IsCrossLines(ALine1, ALine2, @vBase) then
  begin
    vP1 := GetActPts(@ALine1);
    vP2 := GetActPts(@ALine2);
    B := DistanceFPoint2DSqr(vBase, vP1);
    C := DistanceFPoint2DSqr(vBase, vP2);
    BC := B * C * 4;
    if BC > Epsilon then
    begin
      A := DistanceFPoint2DSqr(vP1, vP2);
      vCosA := (B + C - A) / Sqrt(BC);
      if not IsRange(vCosA,1) then
      begin
        if vCosA > 0 then
          Result := 0
        else
          Result := 180
      end
      else
        Result := ArcCos(vCosA) * f180DividedByPi;
    end
    else
      Result := 90;
  end
  else
    Result := 0;
end;

function GetAngleOfLinesEx(const Base, P1, P2: Pointer; const AProc: TsgProcOfPointerGetPoint): Double;
var
  APBase, AP1, AP2: TFPoint;
begin
  APBase := AProc(Base);
  AP1 := AProc(P1);
  AP2 := AProc(P2);
  Result := GetAngleOfLinesEx(APBase, AP1, AP2);
end;

function GetAngleOfLinesEx(const Base, P1, P2: TFPoint): Double;
var
  Asqr, Bsqr, Csqr, ABx2, vCos: Extended;
begin
  Asqr := DistanceFPoint2DSqr(Base, P1);
  Bsqr := DistanceFPoint2DSqr(Base, P2);
  ABx2 := sgSqrt(Asqr * Bsqr) * 2;
  if ABx2 > fDoubleResolution then
  begin
    Csqr := DistanceFPoint2DSqr(P1, P2);
    vCos := (Asqr + Bsqr - Csqr) / ABx2;
    if vCos >= 1 then
      Result := 0
    else
      if vCos <= -1 then
         Result := 180
      else
      begin
        Result := ArcCos(vCos) * f180DividedByPi;
        if Result > 270 then
          Result := 360 - Result;
      end;
  end
  else
    Result := 90;
end;

function GetCosAngleOfVectors(const V1, V2: TFPoint; const A3D: Boolean = False): Double;
var
  vV1, vV2: TFPoint;
  vDiv: Extended;
begin
  vV1 := V1;
  vV2 := V2;
  if not A3D then
  begin
    vV1.Z := 0;
    vV2.Z := 0;
  end;
  vDiv := DistanceVectorSqr(vV1.X, vV1.Y, vV1.Z) * DistanceVectorSqr(vV2.X, vV2.Y, vV2.Z);
  if vDiv > fDoubleResolution then
  begin
    Result := ScalarMultiplyVectors(vV1, vV2) / Sqrt(vDiv);
    if Result > 1 then
      Result := 1
    else
      if Result < -1 then
        Result := -1;
  end
  else
    Result := 1;
end;

function GetFPointSinCosByAngle(const AAngle: Double): TFPoint;
var
  vSin, vCos: Extended;
begin
  SinCos(AAngle, vSin, vCos);
  Result.X := vCos;
  Result.Y := vSin;
  Result.Z := 0;
end;

function GetFPointSinCosByFPoints(const AP1, AP2: TFPoint): TFPoint;
var
  vDeltaX, vDeltaY, vDeltaZ, v1DivDistance, vDistanceSqr: Extended;
begin
  vDeltaX := AP2.X - AP1.X;
  vDeltaY := AP2.Y - AP1.Y;
  vDeltaZ := AP2.Z - AP1.Z;
  vDistanceSqr := Sqr(vDeltaX) + Sqr(vDeltaY) + Sqr(vDeltaZ);
  if vDistanceSqr > 0{fDoubleResolution} then
  begin
    v1DivDistance := 1 / Sqrt(vDistanceSqr);
    Result.X := vDeltaX * v1DivDistance;
    Result.Y := vDeltaY * v1DivDistance;
    Result.Z := vDeltaZ * v1DivDistance;
  end
  else
    Result := cnstFPointZero;
end;

function GetAngleOfVector(const V: TFPoint; const AIsRadian: Boolean): Double;
begin
  Result := GetAngleByPoints(cnstFPointZero, V, AIsRadian);
end;

function GetAngleOfVectors(const V1, V2: TFPoint; const AIsRadian: Boolean;
  const A3D: Boolean = False): Double;
var
  vCos: Double;
begin
  vCos := GetCosAngleOfVectors(V1, V2, A3D);
  if Abs(vCos) > 1 then
  begin
    if vCos > 0 then
      Result := 0
    else
    begin
      if AIsRadian then
        Result := cnstPi
      else
        Result := 180;
    end;
  end
  else
  begin
    Result := ArcCos(vCos);
    if not AIsRadian then
      Result := Result * f180DividedByPi;
  end;
end;

function GetAngleOfF2DVectors(const V1, V2: TF2DPoint; const AIsRadian: Boolean): Double;
begin
  Result := GetAngleOfVectors(MakeFPointFrom2D(V1), MakeFPointFrom2D(V2), AIsRadian);
end;

function GetAngleOfVectorsDirect(const V1, V2: TFPoint; const AIsRadian: Boolean): Double;
begin
  Result := GetAngleByPoints(cnstFPointZero, V2, AIsRadian) -
    GetAngleByPoints(cnstFPointZero, V1, AIsRadian);
  if Result < 0 then
    if AIsRadian then
      Result := cnstPiMul2 + Result
    else
      Result := 360 + Result;
end;

function GetAngleByLen(const ARadius, ALen: Double; const AIsRadian: Boolean): Double;
begin
  Result := ALen / ARadius;
  if not AIsRadian then
    Result := Result * f180DividedByPi;
end;

function GetAngleParam(const Center, RadPt: TFPoint; const Ratio: Double;
  const Point: TFPoint; const AAbsPoint: Boolean = True): Double;
const
  cntsKoef = 1024;
var
  RA, dX, dY, vCos, vSin, X, Y: Extended;
begin
  RA :=  DistanceVectorSqr(RadPt.X, RadPt.Y, 0);
  if (RA > fDoubleResolution) and (not IsZero(Ratio)) then
  begin
    RA := Sqrt(RA);
    vSin := RadPt.Y / RA;
    vCos := RadPt.X / RA;
    dX := Point.X;
    dY := Point.Y;
    if AAbsPoint then
    begin
      dX := dX - Center.X;
      dY := dY - Center.Y;
    end;
    X := cntsKoef * (dX*vCos + dY*vSin) / RA;
    Y := cntsKoef * (dY*vCos - dX*vSin) / (RA * Ratio);
    Result := sgArcTan2(X, Y);
  end
  else
    Result := 90;
end;

function GetParamByAngle(const Ratio, Angle: Double): Double;
var
  vTan: Double;
  S, C: Extended;
begin
  SinCos(Radian(Angle), S, C);
  if not sgIsZero(C) then
  begin
    vTan :=  S / C ;
    Result := Degree(ArcTan(vTan / Ratio));
    case Integer(S < 0) shl 1 + Integer(C < 0) of
      1, 3:
         begin
           Result := Result + 180;
           Result := Result - 360 * Floor(Result / 360);
         end;
    end;
  end
  else
    Result := Angle;
end;

function GetAngleByCoords(const AAngle: Double;
  const AxisX, AxisY: Integer): Double;
var
  vRotPoint: TFPoint;
begin
  vRotPoint := GetPointOnCircle(cnstFPointZero, 100, AAngle);
  vRotPoint.X := AxisX * vRotPoint.X;
  vRotPoint.Y := AxisY * vRotPoint.Y;
  Result := GetAngleByPoints(cnstFPointZero, vRotPoint, False);
end;

function GetSizeFRect2D(const ARect: TFRect): TF2DPoint;
begin
  Result.X := Abs(ARect.Right - ARect.Left);
  Result.Y := Abs(ARect.Top - ARect.Bottom);
end;

function GetSizeFRect(const ARect: TFRect): TFPoint;
begin
  Result.X := Abs(ARect.Right - ARect.Left);
  Result.Y := Abs(ARect.Top - ARect.Bottom);
  Result.Z := Abs(ARect.Z1 - ARect.Z2);
end;

function GetStartF2DPoint(const Arc: TsgArcR): TF2DPoint;
var
  S, C: Extended;
begin
  SinCos(Arc.AngleS * fPiDividedBy180, S, C);
  Result.X := Arc.Center.X + Arc.Radius * C;
  Result.Y := Arc.Center.Y + Arc.Radius * S;
end;

function GetEndF2DPoint(const Arc: TsgArcR): TF2DPoint;
var
  S, C: Extended;
begin
  SinCos(Arc.AngleE * fPiDividedBy180, S, C);
  Result.X := Arc.Center.X + Arc.Radius * C;
  Result.Y := Arc.Center.Y + Arc.Radius * S;
end;

//  See ISO/IEC 8632-1:1999(E) - D.4.5.10 Elliptical Elements
function GetEllipseParams(const APCenter, ACDP1, ACDP2: TFPoint;
  var ARadPoint1, ARadPoint2: TFPoint): Boolean;
var
  vYc, vXc: Double;
  vYz, vXv, vYv: Double;
  vD, vF, vLF: Double;
  vA,vB,vAlfa: Double;
  vPoint1, vPoint2: TFPoint;
begin
  Result := False;
  vPoint1 := SubFPoint(ACDP1, APCenter);
  vPoint2 := SubFPoint(ACDP2, APCenter);
  vYc := (vPoint2.Y - vPoint1.X)/2;
  vXc := (vPoint2.X + vPoint1.Y)/2;
  if not sgIsZero(vXc) then
  begin
    vF := ArcTan(vYc/vXc);
    if not sgIsZero(Cos(vF)) then
    begin
      vD := vXc / Cos(vF);
      if not sgIsZero(vPoint2.X - vXc) then
      begin
        vLF := ArcTan((vYc - vPoint2.Y)/(vPoint2.X - vXc));
        vXv := vXc + vD*cos(vLF);
        vYv := vYc - vD*sin(vLF);
        vYz := vYc + vD*sin(vLF);
        if not sgIsZero(vXv) then
        begin
          vAlfa := Degree(ArcTan((vYv/vXv)));
          if not sgIsZero(sin(vLF)) then
          begin
            vA := ((vYz - vPoint2.Y) / sin(vLF));
            vB := ((vPoint2.Y - vYv) / sin(vLF));
            ARadPoint1 := RotateFPoint(MakeFPoint(vA, 0, 0), vAlfa);
            ARadPoint2 := RotateFPoint(MakeFPoint(0, vB, 0), vAlfa);
            ARadPoint1 := AddFPoint(ARadPoint1, APCenter);
            ARadPoint2 := AddFPoint(ARadPoint2, APCenter);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure IncPoints4(const ADelta: TFPoint; var APoints: TsgPoints4);
begin
  APoints[0] := AddFPoint(APoints[0], ADelta);
  APoints[1] := AddFPoint(APoints[1], ADelta);
  APoints[2] := AddFPoint(APoints[2], ADelta);
  APoints[3] := AddFPoint(APoints[3], ADelta);
end;

procedure RotatePoints4(const AAngle: Double; var APoints: TsgPoints4);
var
  vRot: Double;
  vRotPt: TFPoint;
begin
  vRot := AAngle * fPiDividedBy180;
  vRotPt := APoints[3];
  APoints[0] := RotateAroundFPoint(APoints[0], vRotPt, False, vRot);
  APoints[1] := RotateAroundFPoint(APoints[1], vRotPt, False, vRot);
  APoints[2] := RotateAroundFPoint(APoints[2], vRotPt, False, vRot);
  //ABoxPoints[3] := RotateAroundFPoint(ABoxPoints[3], vRotPt, False, vRot);
end;

function AreaOfTriangleEx(const L12, L23, L31: Extended): Double;
var
  P: Extended;
begin
  P := (L12 + L23 + L31)* 0.5;
  Result := sgSqrt(P * (P - L12) * (P - L23) * (P - L31));
end;

function ChangeLineLength(const ALine: TsgLine; const ANewLength: Double): TsgLine;
begin
  Result := MakeLine(ALine.Point1,
    GetPointOnLine(ALine.Point1, ALine.Point2, ANewLength));
end;

function AreaOfTriangle(const AP1, AP2, AP3: TFPoint): Double;
begin
  Result := AreaOfTriangleEx(DistanceFPoint2D(AP1, AP2),
    DistanceFPoint2D(AP2, AP3), DistanceFPoint2D(AP3, AP1));
end;

function GetAreaOfList(const AList: TList; const AProc: TsgProcOfPointerGetPoint): Double;
begin
  Result := 0.0;
  if AList.Count > 0 then
    Result := GetAreaOfList(TsgListInterface.Create(AList, AProc));
end;

function GetAreaOfList(AList: IsgArrayFPoint; ACount: Integer = -1): Double;
var
  I, J: Integer;
  Pt: array [0 .. 1] of TFPoint;
begin
  Result := 0.0;
  if ACount < 0 then
    ACount := AList.FPointCount - 1;

  if ACount >= 2 then
  begin
    J := 0;
    Pt[J] := AList.FPoints[ACount];
    for I := 0 to ACount do
    begin
      J := J xor 1;
      Pt[J] := AList.FPoints[I];
      Result := Result + (Pt[J].X + Pt[J xor 1].X) * (Pt[J].Y - Pt[J xor 1].Y);
    end;
    Result := Abs(Result) * 0.5;
  end;
end;

function GetAreaOfCircle(const ARadius: Double): Double;
begin
  Result := cnstPi * Sqr(ARadius);
end;

function GetAreaOfArc(const AArc: TsgArcR): Double;
var
  vArc: TsgArcR;
  vAngle: Double;
  vSign: Double;
begin
  vArc := NormalizeArcAngels(AArc);
  if vArc.AngleS > vArc.AngleE then
    vArc.AngleE := vArc.AngleE + 360;
  vAngle := Abs(vArc.AngleE - vArc.AngleS);
  Result := vAngle * fPiDividedBy180 * Sqr(AArc.Radius) / 2;
  if vAngle > 180 then
    vSign := +1
  else
    vSign := -1;
  Result := Result + vSign * AreaOfTriangle(AArc.Center,
    GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleS),
    GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleE));
end;

function GetAreaOfRect2D(ARect: TFRect): Double;
var
  vSize: TF2DPoint;
begin
  vSize := GetSizeFRect2D(ARect);
  Result := vSize.X * vSize.Y;
end;

function GetDistanceOfNearestPointByPointList(APts: TList; AStartPt: TFPoint;
  var AMinDistPt: TFPoint): TsgFloat;
var
  I: Integer;
  vCurPt: TFPoint;
  vDist: TsgFloat;
begin
  Result := 0;
  if APts.Count = 0 then
    Exit;
  AMinDistPt := PFPoint(APts[0])^;
  Result := DistanceFPoint(AStartPt, AMinDistPt);
  for I := 1 to APts.Count - 1 do
  begin
    vCurPt := PFPoint(APts[I])^;
    vDist := DistanceFPoint(AStartPt, vCurPt);
    if vDist < Result then
    begin
      AMinDistPt := vCurPt;
      Result := vDist;
    end;
  end;
end;

function GetPerimeterOfList(const AList: TList; const Closed: Boolean; const AProc: TsgProcOfPointerGetPoint): Double;
begin
  Result := 0;
  if AList.Count > 1 then
    Result := GetPerimeterOfList(TsgListInterface.Create(AList, AProc), Closed);
end;

function GetPerimeterOfList(AList: IsgArrayFPoint; const Closed: Boolean;
  ACount: Integer = -1): Double;
var
  I: Integer;
begin
  Result := 0;
  if ACount < 0 then
    ACount := AList.FPointCount;
  for I := 0 to ACount - 2 do
    Result := Result + DistanceFPoint(AList.FPoints[I], AList.FPoints[I + 1]);
  if (ACount > 2) and Closed then
    Result := Result + DistanceFPoint(AList.FPoints[0], AList.FPoints[ACount - 1]);
end;

function GetBoxOfCircle2D(const Arcs: TsgArcR): TFRect;
begin
  Result.TopLeft := Arcs.Center;
  Result.BottomRight := Arcs.Center;
  Result.Left := Result.Left - Arcs.Radius;
  Result.Right := Result.Right + Arcs.Radius;
  Result.Top := Result.Top + Arcs.Radius;
  Result.Bottom := Result.Bottom - Arcs.Radius;
end;

function GetBoxOfArcs2D(const Arcs: TsgArcsParams): TFRect;
var
  I: Integer;
  vAngle: Double;
begin
  Result := cnstBadRect;
  ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius1, Arcs.AngleS1));
  ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius1, Arcs.AngleE1));
  ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius2, Arcs.AngleS2));
  ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius2, Arcs.AngleE2));
  for I := 0 to 3 do
  begin
    vAngle := 90 * I;
    if IsAngleInAngles(vAngle, Arcs.AngleS1, Arcs.AngleE1) then
      ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius1, vAngle));
    if IsAngleInAngles(vAngle, Arcs.AngleS2, Arcs.AngleE2) then
      ExpandFRect2D(Result, GetPointOnCircle(Arcs.Center, Arcs.Radius2, vAngle));
  end;
end;

function GetBoxOfLine2D(const ALine: TsgLine): TFRect;
begin
  if ALine.Point1.X > ALine.Point2.X then
  begin
    Result.Left := ALine.Point2.X;
    Result.Right := ALine.Point1.X;
  end
  else
  begin
    Result.Left := ALine.Point1.X;
    Result.Right := ALine.Point2.X;
  end;
  if ALine.Point1.Y > ALine.Point2.Y then
  begin
    Result.Bottom := ALine.Point2.Y;
    Result.Top := ALine.Point1.Y;
  end
  else
  begin
    Result.Bottom := ALine.Point1.Y;
    Result.Top := ALine.Point2.Y;
  end;
  Result.Z1 := 0;
  Result.Z2 := 0;
end;

function GetBoxOf2DLine(const ALine: TF2DLine): TF2DRect;
begin
  if ALine.Point1.X > ALine.Point2.X then
  begin
    Result.Left := ALine.Point2.X;
    Result.Right := ALine.Point1.X;
  end
  else
  begin
    Result.Left := ALine.Point1.X;
    Result.Right := ALine.Point2.X;
  end;
  if ALine.Point1.Y > ALine.Point2.Y then
  begin
    Result.Bottom := ALine.Point2.Y;
    Result.Top := ALine.Point1.Y;
  end
  else
  begin
    Result.Bottom := ALine.Point1.Y;
    Result.Top := ALine.Point2.Y;
  end;
end;

function GetBoxOfList(const AList: TList): TFRect;
var
  vList: TsgObjectList;
begin
  vList := TsgObjectList.Create;
  try
    vList.CopyFrom(AList);
    Result := GetBoxOfObjectList(vList);
  finally
    vList.Free;
  end;
end;

function GetBoxOfObjectList(const AList: TsgObjectList): TFRect;
var
  I: Integer;
  vPolyline: IsgArrayFPoint;
  vObj: TObject;
begin
  Result := cnstBadRect;
  for I := 0 to AList.Count - 1 do
  begin
    vObj := AList[I];
    if vObj is TF2DPointList then
      vPolyline := TF2DPointList(vObj)
    else
      vPolyline := TFPointList(vObj);
    UnionFRect(Result, GetBoxOfFPoints(vPolyline));
  end;
end;

function GetBoxOfPointI(const AP1, AP2: TPoint): TRect;
begin
  if AP1.X > AP2.X then
  begin
    Result.Left := AP2.X;
    Result.Right := AP1.X;
  end
  else
  begin
    Result.Left := AP1.X;
    Result.Right := AP2.X;
  end;
  if AP1.Y < AP2.Y then
  begin
    Result.Bottom := AP2.Y;
    Result.Top := AP1.Y;
  end
  else
  begin
    Result.Bottom := AP1.Y;
    Result.Top := AP2.Y;
  end;
end;

function GetBoxOfPts2D(const APts: TsgPoints4): TFRect;
begin
  Result.TopLeft := APts[0];
  Result.BottomRight := APts[0];
  ExpandFRect2D(Result, APts[1]);
  ExpandFRect2D(Result, APts[2]);
  ExpandFRect2D(Result, APts[3]);
  Result.Z1 := 0;
  Result.Z2 := 0;
end;

function GetRealBox(const ASourceBox: TFRect; const AMatrix: TFMatrix): TFRect;
begin
  Result := ASourceBox;
  TransRectCorners(Result, AMatrix);
end;

function GetBoxOfLine(const ALine: TsgLine): TFRect;
begin
  Result := GetBoxOfLine2D(ALine);
  if ALine.Point1.Z > ALine.Point1.Z then
  begin
    Result.Z2 := ALine.Point1.Z;
    Result.Z1 := ALine.Point2.Z;
  end
  else
  begin
    Result.Z1 := ALine.Point1.Z;
    Result.Z2 := ALine.Point2.Z;
  end;
end;

function GetBoxOfFPoints(APoints: IsgArrayFPoint): TFRect;
var
  I: Integer;
begin
  Result := cnstBadRect;
  if APoints <> nil then
  begin
    for I := 0 to APoints.FPointCount - 1 do
      ExpandFRect(Result, APoints.FPoints[I]);
  end;
end;

function GetBoxOfF2Rect(const ARect: TF2DRect): TRect;
begin
  Result.Left := Round(ARect.Left);
  Result.Right := Round(ARect.Right);
  Result.Top := Round(ARect.Top);
  Result.Bottom := Round(ARect.Bottom);
  if ARect.Left > ARect.Right then
    SwapInts(Result.Left, Result.Right);
  if ARect.Top > ARect.Bottom then
    SwapInts(Result.Top, Result.Bottom);
end;

function GetBoxOfArray(AList: IsgArrayFPoint): TFRect;
var
  I: Integer;
begin
  Result := cnstBadRect;
  for I := 0 to AList.FPointCount - 1 do
    ExpandFRect(Result, AList.FPoints[I]);
end;

function GetBoxOfArray(const APoints: array of TFPoint): TFRect;
begin
  if Length(APoints) > 0 then
    Result := GetBoxOfArray(APoints[0], Length(APoints))
  else
    Result := cnstBadRect;
end;

function GetBoxOfArray(const APoints; ACount: Integer): TFRect;
var
  I: Integer;
begin
  if ACount > 0 then
  begin
    Result.TopLeft := PFPointArray(@APoints)^[0];
    Result.BottomRight := Result.TopLeft;
    for I := 1 to ACount - 1 do
      ExpandFRect(Result, PFPointArray(@APoints)^[I]);
  end
  else
    Result := cnstBadRect;
end;

function GetBulgeOfArcR(const AArc: TsgArcR; const AVetrexPoint: TFPoint): Double;
var
  vArcStart, vArcEnd, vArcMiddle: TFPoint;
begin
  Result := 0;
  vArcStart := GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleS);
  vArcEnd := GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleE);
  if not IsEqualFPoints(vArcStart, vArcEnd) then
  begin
    if not IsEqualFPoints(AVetrexPoint, vArcStart) then
      SwapFPoints(vArcStart, vArcEnd);
    vArcMiddle := GetPointOnCircle(AArc.Center, AArc.Radius, GetMiddleAngleAP(AArc));
    GetBulgeWithCorrect(vArcStart, vArcEnd, vArcMiddle, Result);
  end;
end;

function GetBulgeWithCorrect(const AP1, AP2, APN: TFPoint;
  var ABulge: Double): Boolean;
var
  vCenter, vPt: TFPoint;
  vDistance, vRadius, vKatet, vBulge: Double;
  vActPtClassify: TsgPointClassify;
begin
  Result := False;
  ABulge := 0;
  if GetCircleParams(AP1, APN, AP2, vCenter, vRadius) then
  begin
    vDistance := DistanceFPoint2D(AP1, AP2);
    vKatet := sgSqrt(4 * Sqr(vRadius) - Sqr(vDistance));
    vActPtClassify := PointClassify(APN, AP1, AP2);
    if PointClassify(vCenter, AP1, AP2) = vActPtClassify then
      vKatet := -vKatet;
    vBulge := sgSqrt(1 - 2 * vKatet / (2 * vRadius + vKatet));
    if Abs(vBulge) > fAccuracy then
    begin
      Result := True;
      ABulge := vBulge;
      vPt := GetNormalPt(AP1, AP2, MiddleFPoint2D(AP1, AP2),
         0.5 * ABulge * vDistance);
      if PointClassify(vPt, AP1, AP2) <> vActPtClassify then
        ABulge := - ABulge;
    end;
  end;
end;

function GetCADColor(const AColor: Cardinal; const ASkipByBlock: Boolean;
   APalett: PsgCADPalett = nil): Cardinal;
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. 3] of Byte;
var
  vColors: PRGBArray;
  vDistance, vD: Integer;
  I: Integer;
  vDelta: Integer;
  vMap: PByte;
begin
  if APalett = nil then
    APalett := @arrDXFtoRGBColors;
  Result := 0;
  case AColor of
    clByBlock: Result := clDXFByBlock;
    clByLayer: Result := clDXFByLayer;
    clNone:    Result := clDXFBlackWhite;
  else
    vColors := PRGBArray(@AColor);
    vDistance := MaxInt;
    I := Low(APalett^);
    vMap := PByte(APalett);
    if ASkipByBlock and (APalett^[I] = 0) then
    begin
      Inc(I);
      vMap := PByte(@APalett^[I]);
    end;
    while I <= High(APalett^) do
    begin
      vDelta := Byte(AColor) - vMap^;
      vD := vDelta * vDelta;
      Inc(vMap);
      vDelta := vColors^[1] - vMap^;
      vD := vD + vDelta * vDelta;
      Inc(vMap);
      vDelta := vColors^[2] - vMap^;
      vD := vD + vDelta * vDelta;
      if vD < vDistance then
      begin
        Result := I;
        vDistance := vD;
      end;
      Inc(vMap,2);
      Inc(I);
    end;
  end;
end;

function GetCenterOfBulge(const AP1, AP2: TFPoint; const ABulge: Double): TFPoint;
var
  vDelta: TFPoint;
  vKoef: Double;
begin
  Result.X := (AP1.X + AP2.X) / 2;
  Result.Y := (AP1.Y + AP2.Y) / 2;
  Result.Z := AP1.Z;
  vDelta.Z := (AP2.Z - Result.Z) / 15;
  vDelta.X := AP1.X - Result.X;
  vDelta.Y := AP1.Y - Result.Y;
  vKoef := (1 - ABulge * ABulge) / 2 / ABulge;
  Result.X := Result.X + vKoef * vDelta.Y;
  Result.Y := Result.Y - vKoef * vDelta.X;
end;


function GetArcROfBulge(const AP1, AP2: TFPoint; const ABulge: Double; const ACheckAngles: Boolean = True): TsgArcR;
var
  vGenerator: TsgGeneratorShapeEdge;
begin
  vGenerator := TsgGeneratorShapeEdge.Create;
  try
    vGenerator.SetListAndProc(nil, nil, TsgCreatePPoint(@CreatePFPointProc));
    vGenerator.NumberCirclePart := GetNumberOfCircleParts;
    Result := vGenerator.CreateBulgesArc(AP1, AP2, ABulge);
  finally
    vGenerator.Free;
  end;
  if ACheckAngles then
    CheckAngles(Result.AngleS, Result.AngleE);
end;

function GetCenterOfCircle(const AR: Double; const AP1, AP2, AP3: TFPoint): TFPoint;
var
  CX, CY, D, DX, DY, L2, LX, LY: Extended;

  function PointType: Boolean;
  begin
    Result := (AP1.X - AP2.X >= AP1.X - AP3.X) and (AP1.Y - AP2.Y >= AP1.Y - AP3.Y);
  end;

begin
  CX := (AP1.X + AP2.X) * 0.5;
  CY := (AP1.Y + AP2.Y) * 0.5;
  L2 :=  DistanceFPoint2DSqr(AP1, AP2);
  D := Sqrt(AR * AR - L2 * 0.25);
  LX := AP1.X - AP2.X;
  LY := AP1.Y - AP2.Y;
  DX := D / Sqrt(1 + Sqr(LX / LY));
  DY := - DX * LX / LY;
  if PointType then
    Result := MakeFPoint(CX - DX, CY - DY, 0)
  else
    Result := MakeFPoint(CX + DX, CY + DY, 0);
end;

function GetCenterOfRect(const ARect: TFRect): TFPoint;
begin
  Result := MiddleFPoint(ARect.TopLeft, ARect.BottomRight);
end;

function GetCenterPts(const APts: TsgPoints4): TsgLine;
begin
  Result.Point1 := MiddleFPoint2D(APts[0], APts[3]);
  Result.Point2 := MiddleFPoint2D(APts[1], APts[2]);
end;

function GetCenterPPts(const APts: TsgPFPoints4): TsgLine;
begin
  Result.Point1 := MiddleFPoint(APts[0]^, APts[3]^);
  Result.Point2 := MiddleFPoint(APts[1]^, APts[2]^);
end;

function GetCenterArcs(const AArc: TsgArcsParams): TsgArcR;
var
  vLine: TsgLine;
  vP1: TFPoint;
begin
  Result.Center := AArc.Center;
  Result.Radius := (AArc.Radius1 + AArc.Radius2) * 0.5;
  vLine.Point1 := GetPointOnCircle(AArc.Center, AArc.Radius1, AArc.AngleS1);
  vLine.Point2 := GetPointOnCircle(AArc.Center, AArc.Radius2, AArc.AngleS2);
  vP1 := MiddleFPoint2D(vLine.Point1, vLine.Point2);
  Result.AngleS := GetAngleByPoints(AArc.Center, vP1, False);
  vLine.Point1 := GetPointOnCircle(AArc.Center, AArc.Radius1, AArc.AngleE1);
  vLine.Point2 := GetPointOnCircle(AArc.Center, AArc.Radius2, AArc.AngleE2);
  vP1 := MiddleFPoint2D(vLine.Point1, vLine.Point2);
  Result.AngleE := GetAngleByPoints(AArc.Center, vP1, False);
end;

function GetArcParams(const AP1, AP2, AP3: TFPoint; var Center: TFPoint;
  var Radiaus, AngleS, AngleE: Double; const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := GetCircleParams(AP1, AP2, AP3, Center, Radiaus, AAccuracy);
  if Result then
    SetAnglesByPoints(Center, AP1, AP2, AP3, False, AngleS, AngleE);
end;

function GetArcParamsAP(const AP1, AP2, AP3: TFPoint; var Arc: TsgArcR;
  const AAccuracy: Double = fDoubleResolution): Boolean;
begin
  Result := GetArcParams(AP1, AP2, AP3, Arc.Center, Arc.Radius, Arc.AngleS, Arc.AngleE, AAccuracy);
end;

function GetCircleParams(const AP1, AP2, AP3: TFPoint; var Center: TFPoint; var Radiaus: Double;
  const AAccuracy: Double = fDoubleResolution; const AIsFixDiameter: Boolean = False): Boolean;
var
  Val1, Val2, vDenom: Extended;
  vD21, vD31: TFPoint;
begin
  vD21 := SubFPoint2D(AP2, AP1);
  vD31 := SubFPoint2D(AP3, AP1);
  Val1 := vD21.X * (AP1.X + AP2.X) + vD21.Y * (AP1.Y + AP2.Y);
  Val2 := vD31.X * (AP1.X + AP3.X) + vD31.Y * (AP1.Y + AP3.Y);
  vDenom := 2 * (vD21.X * (AP3.Y - AP2.Y) - vD21.Y * (AP3.X - AP2.X));
  Result := not sgIsZero(vDenom, AAccuracy);
  if Result then
  begin
    Center.X := (vD31.Y * Val1 - vD21.Y * Val2) / vDenom;
    Center.Y := (vD21.X * Val2 - vD31.X * Val1) / vDenom;
    Center.Z := AP1.Z;
    Radiaus := DistanceFPoint(AP1, Center);
  end
  else
  begin
    if AIsFixDiameter and IsEqualFPoints2D(AP1, AP3) then//is diameter
    begin
      Center := MiddleFPoint(AP1, AP2);
      Radiaus := DistanceFPoint(AP1, Center);
      Result := True;
    end;
  end;
end;

function GetIntersectingPoint(const ALine1, ALine2: TsgLine;
  var AIntersectionMode: TIntersectionMode): TFPoint;
var
  K1, K2, B1, B2: Extended;
begin
  Result := cnstFPointZero;
  AIntersectionMode := imPoint;
  if IsEqual(ALine1.Point2.X, ALine1.Point1.X) then
  begin
    Result.X := ALine1.Point2.X;
    if not IsEqual(ALine2.Point2.X, ALine2.Point1.X) then
    begin
      K2 := (ALine2.Point2.Y - ALine2.Point1.Y) / (ALine2.Point2.X - ALine2.Point1.X);
      B2 := ALine2.Point1.Y - ALine2.Point1.X * K2;
      Result.Y := B2 + Result.X * K2;
    end
    else
      if IsEqual(ALine1.Point1.X, ALine2.Point1.X) then
        AIntersectionMode := imLine
      else
        AIntersectionMode := imNone;
  end
  else
  if IsEqual(ALine2.Point2.X, ALine2.Point1.X) then
  begin
    Result.X := ALine2.Point1.X;
    if not IsEqual(ALine1.Point2.X, ALine1.Point1.X) then
    begin
      K1 := (ALine1.Point2.Y - ALine1.Point1.Y) / (ALine1.Point2.X - ALine1.Point1.X);
      B1 := ALine1.Point1.Y - ALine1.Point1.X * K1;
      Result.Y := B1 + Result.X * K1
    end
    else
      if IsEqual(ALine1.Point1.X, ALine2.Point1.X) then
        AIntersectionMode := imLine
      else
        AIntersectionMode := imNone;
  end
  else
  begin
    K2 := (ALine2.Point2.Y - ALine2.Point1.Y) / (ALine2.Point2.X - ALine2.Point1.X);
    K1 := (ALine1.Point2.Y - ALine1.Point1.Y) / (ALine1.Point2.X - ALine1.Point1.X);
    B1 := ALine1.Point1.Y - ALine1.Point1.X * K1;
    B2 := ALine2.Point1.Y - ALine2.Point1.X * K2;
    if not IsEqual(K1, K2) then
    begin
      Result.X := (B2 - B1) / (K1 - K2);
      Result.Y := K1 * Result.X + B1;
    end
    else
      if IsEqual(B1, B2) then
        AIntersectionMode := imLine
      else
        AIntersectionMode := imNone;
  end;
end;

function GetLandScale(const AWidth, AHeight: Double): Boolean; overload;
begin
  Result := AWidth >= AHeight;
end;

function GetLandScale(const ASize: TF2DPoint): Boolean; overload;
begin
  Result := GetLandScale(ASize.X, ASize.Y);
end;

function GetIntresectingRectAndLine(const ARect: TFRect; const ALine: TsgLine;
  var AIntersectionMode: TIntersectionMode): TsgLine;
var
  vIntersectPt: array[0..3] of TFPoint;
  vRectLine: TsgLine;
  vRect: TFRect;
  I, J: Integer;
  vTempPoint: TFPoint;
  vIM: TIntersectionMode;
  A: TsgFloat;
begin
  vRect := ARect;
  if ARect.Top < ARect.Bottom then
  begin
    vRect.Top := ARect.Bottom;
    vRect.Bottom := ARect.Top;
  end;
  //Left line
  vRectLine.Point1 := vRect.TopLeft;
  vRectLine.Point2.X := vRect.Left;
  vRectLine.Point2.Y := vRect.Bottom;
  vIntersectPt[0] := GetIntersectingPoint(vRectLine, ALine, vIM);
  if (vIM <> imPoint) or
     (vRectLine.Point1.Y < vIntersectPt[0].Y)or(vIntersectPt[0].Y < vRectLine.Point2.Y) then
    vIntersectPt[0].X := MaxTsgFloat;
  //Bottom line
  vRectLine.Point1 := vRectLine.Point2;
  vRectLine.Point2 := vRect.BottomRight;
  vIntersectPt[1] := GetIntersectingPoint(vRectLine, ALine, vIM);
  if (vIM <> imPoint)or
     (vRectLine.Point1.X > vIntersectPt[1].X)or(vIntersectPt[1].X > vRectLine.Point2.X) then
    vIntersectPt[1].X := MaxTsgFloat;
  //Right line
  vRectLine.Point1 := vRectLine.Point2;
  vRectLine.Point2.X := vRect.Right;
  vRectLine.Point2.Y := vRect.Top;
  vIntersectPt[2] := GetIntersectingPoint(vRectLine, ALine, vIM);
  if (vIM <> imPoint)or
     (vRectLine.Point1.Y > vIntersectPt[2].Y)or(vIntersectPt[2].Y > vRectLine.Point2.Y) then
    vIntersectPt[2].X := MaxTsgFloat;
  //Top line
  vRectLine.Point1 := vRectLine.Point2;
  vRectLine.Point2 := vRect.TopLeft;
  vIntersectPt[3] := GetIntersectingPoint(vRectLine, ALine, vIM);
  if (vIM <> imPoint)or
     (vRectLine.Point1.X < vIntersectPt[3].X)or(vIntersectPt[3].X < vRectLine.Point2.X) then
    vIntersectPt[3].X := MaxTsgFloat;
  for I := 0 to 2 do
    for J := 0 to 2 - I do
      if vIntersectPt[J].X > vIntersectPt[J + 1].X then
      begin
        vTempPoint := vIntersectPt[J];
        vIntersectPt[J] := vIntersectPt[J + 1];
        vIntersectPt[J + 1] := vTempPoint;
      end;
  Result.Point1 := vIntersectPt[0];
  I := 1;
  while (Abs(vIntersectPt[0].X - vIntersectPt[I].X) < FAccuracy) and
    (Abs(vIntersectPt[0].Y - vIntersectPt[I].Y) < FAccuracy) and (I < 3) do
    Inc(I);
  Result.Point2 := vIntersectPt[I];
  A := MaxTsgFloat;
  if (not CompareMem(@Result.Point1.X, @A, SizeOf(Result.Point1.X))) and
    (not CompareMem(@Result.Point2.X, @A, SizeOf(Result.Point1.X))) then
    AIntersectionMode := imLine
  else
    if (not CompareMem(@Result.Point1.X, @A, SizeOf(Result.Point1.X))) and
       (CompareMem(@Result.Point1.X, @A, SizeOf(Result.Point1.X))) then
      AIntersectionMode := imPoint
    else
      AIntersectionMode := imNone;
end;

function IsRayIntersectsBox(const ARayStart, ARayVector: TFPoint;
  const ABox: TFRect; AIntersectPoint: PFPoint = nil) : Boolean;
var
  I, vPlainIdx: Integer;
  vMaxDist, vPlane, vRes: TFPoint;
  vIsMiddle: array[0 .. 2] of Boolean;
  vMinExt, vMaxExt: TFPoint;
begin
  vMinExt := MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z1);
  vMaxExt := MakeFPoint(ABox.Right, ABox.Top, ABox.Z2);
  // make vPlane
  Result := True;
  for I := 0 to 2 do
    if ARayStart.V[I] < vMinExt.V[I] then
    begin
      vPlane.V[I] := vMinExt.V[I];
      vIsMiddle[I] := False;
      Result := False;
    end
    else
      if ARayStart.V[I] > vMaxExt.V[I] then
      begin
        vPlane.V[I] := vMaxExt.V[I];
        vIsMiddle[I] := False;
        Result := False;
      end
      else
        vIsMiddle[I] := True;
  if Result then
  begin
    // ARayStart inside box.
    if AIntersectPoint <> nil then
      AIntersectPoint^ := ARayStart;
	end
  else
  begin
    // Distance to vPlane.
    vPlainIdx := 0;
    for I := 0 to 2 do
      if vIsMiddle[I] or (ARayVector.V[I] = 0) then
        vMaxDist.V[I] := -1
      else
      begin
        vMaxDist.V[I] := (vPlane.V[I] - ARayStart.V[I]) / ARayVector.V[I];
        if vMaxDist.V[I] > 0 then
        begin
          if vMaxDist.V[vPlainIdx] < vMaxDist.V[I] then
            vPlainIdx := I;
          Result := True;
        end;
      end;
    // Is inside box ?
    if Result then
    begin
      I := 0;
      while Result and (I <= 2) do
      begin
        if vPlainIdx = I then
          vRes.V[I] := vPlane.V[I]
        else
        begin
          vRes.V[I] := ARayStart.V[I] + vMaxDist.V[vPlainIdx] * ARayVector.V[I];
          Result := (vRes.V[I] >= vMinExt.V[I]) and (vRes.V[I] <= vMaxExt.V[I]);
        end;
        Inc(I);
      end;
      if Result then
        if AIntersectPoint <> nil then
          AIntersectPoint^ := vRes;
    end;
  end;
end;

function GetPointOfPFPoint(const APointer: Pointer): TFPoint;
begin
  Result := PFPoint(APointer)^;
end;

function GetPointOfItem(const APointer: Pointer): TFPoint;
begin
  Result := TsgItemLnk(APointer).Point;
end;

function AddElementsToList(AList: TList; AAddCount: Integer;
  const AValues: array of Pointer; ADisposeOthers: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AList = nil then
    Exit;
  for I := Low(AValues) to High(AValues) do
    if AAddCount > 0 then
    begin
      Result := True;
      AList.Add(AValues[I]);
      Dec(AAddCount);
    end
    else
      if ADisposeOthers then
        FreeMem(AValues[I])
      else
        Break;
end;

procedure AddFirstF2DPointUnique(const AList: TFPointList; const AP: TF2DPoint);
begin
  AddFirstFPointUnique(AList, MakeFPointFrom2D(AP));
end;

procedure AddFirstFPointUnique(const AList: TFPointList; const AP: TFPoint);
begin
  if AList.Count = 0 then
    AList.Add(AP)
  else
    if not IsEqualFPoints(AList.First, AP) then
      AList.Insert(0, AP);
end;

procedure AddLastFPointUnique(const AList: TFPointList; const AP: TFPoint);
begin
  if AList.Count = 0 then
    AList.Add(AP)
  else
    if not IsEqualFPoints(AList.Last, AP) then
      AList.Add(AP);
end;

procedure AddLastF2DPointUnique(const AList: TFPointList; const AP: TF2DPoint);
begin
  AddLastFPointUnique(AList, MakeFPointFrom2D(AP))
end;

function AddFPointInList(const AList: TList; const APoint: TFPoint): PFPoint;
begin
  New(Result);
  AList.Add(Result);
  Result^ := APoint;
end;

function AddF2DPointInList(const AList: TList; const APoint: TF2DPoint): PF2DPoint;
begin
  New(Result);
  AList.Add(Result);
  Result^ := APoint;
end;

procedure ScaleList(const APoints: TFPointList; const AScale: TFPoint);
var
  I: Integer;
begin
  for I := 0 to APoints.Count - 1 do
    APoints[I] := MultiplyFPoint(APoints[I], AScale);
end;

function AddF2DPointInList(const AList: TList; const APoint: TFPoint): PF2DPoint;
begin
  New(Result);
  AList.Add(Result);
  Result^.X := APoint.X;
  Result^.Y := APoint.Y;
end;

procedure AddFPointList(const AList: TList; const APoint: TFPoint);
begin
  AddFPointInList(AList, APoint);
end;

procedure AddFPointInFPointList(const AList: Pointer; const APoint: TFPoint);
begin
  TFPointList(AList).Add(APoint);
end;

procedure AddItemInSingleList(const AList: Pointer; const AItem: Single);
begin
  TsgSingleList(AList).Add(AItem);
end;

function GetFPointFromFPointList(const AList: Pointer; const AIndex: Integer): TFPoint;
begin
  Result := TFPointList(AList)[AIndex];
end;

function GetItemFromSingleList(const AList: Pointer; const AIndex: Integer): Single;
begin
  Result := TsgSingleList(AList)[AIndex];
end;

function GetLastFPoint(const AList: TList): TFPoint;
begin
  if AList.Count > 0 then
    Result := PFPoint(AList[AList.Count - 1])^
  else
    Result := cnstFPointZero;
end;

function GetListCount(const AList: TList; ALevel: Integer = 0): Integer;
var
  I: Integer;
begin
  if ALevel <= 0 then
    Result := AList.Count
  else
  begin
    Result := 0;
    Dec(ALevel);
    for I := 0 to AList.Count - 1 do
    begin
      if AList[I] <> nil then
        Inc(Result, GetListCount(TList(AList[I]), ALevel));
    end;
  end;
end;

procedure AddNotifyEventToList(const AList: TList; const AEvent: TNotifyEvent);
var
  vEvent: TMethod absolute AEvent;
  vMethod: PMethod;
begin
  New(vMethod);
  vMethod^ := vEvent;
  AList.Add(vMethod);
end;

function GetNotifyEventFromList(const AList: TList; const AIndex: Integer): TNotifyEvent;
begin
  Result := TNotifyEvent(PMethod(AList[AIndex])^);
end;

function RemoveNotifyEventFromList(const AList: TList; const AEvent: TNotifyEvent): Boolean;
var
  vEvent: TMethod absolute AEvent;
  I: Integer;
  vMethod: PMethod;
begin
  Result := False;
  for I := 0 to AList.Count - 1 do
  begin
    vMethod := PMethod(AList[I]);
    if (vMethod^.Code = vEvent.Code) and (vMethod^.Data = vEvent.Data) then
    begin
      Result := True;
      Dispose(vMethod);
      AList.Delete(I);
      Break;
    end;
  end;
end;

procedure ClearNotifyEventList(const AList: TList);
begin
  ClearRecordList(AList);
end;

procedure CopyForwardPsgLine(const AData, AList: Pointer; const Added: Boolean);
var
  vLine: PsgLine absolute AData;
begin
  if Added then
  begin
    AddLastFPointUnique(TFPointList(AList), vLine^.Point1);
    AddLastFPointUnique(TFPointList(AList), vLine^.Point2);
  end
  else
  begin
    AddFirstFPointUnique(TFPointList(AList), vLine^.Point1);
    AddFirstFPointUnique(TFPointList(AList), vLine^.Point2);
  end;
end;

procedure CopyBackwardPsgLine(const AData, AList: Pointer; const Added: Boolean);
var
  vLine: PsgLine absolute AData;
begin
  if Added then
  begin
    AddLastFPointUnique(TFPointList(AList), vLine^.Point2);
    AddLastFPointUnique(TFPointList(AList), vLine^.Point1);
  end
  else
  begin
    AddFirstFPointUnique(TFPointList(AList), vLine^.Point2);
    AddFirstFPointUnique(TFPointList(AList), vLine^.Point1);
  end;
end;

function GetPoint1OfPsgLine(const APointer: Pointer): TFPoint;
begin
  Result := PsgLine(APointer)^.Point1;
end;

function GetPoint2OfPsgLine(const APointer: Pointer): TFPoint;
begin
  Result := PsgLine(APointer)^.Point2;
end;

procedure CopyForwardPF2DCurveEx(const AData, AList: Pointer; const Added: Boolean);
var
  vCurve: PF2DCurveEx absolute AData;
  vAngle, vStart, vEnd, vAngleDelta: Double;
  I, NumberPart: Integer;
  vPoint: TFPoint;
  S, C: Extended;
begin
  if lpArc in vCurve^.Flags then
  begin
    vPoint.Z := 0;
    NumberPart := GetNumberOfCircleParts;
    vStart := vCurve^.Arc.AngleS;
    vEnd := vCurve^.Arc.AngleE;
    if vStart > vEnd then
      vStart := vStart - Round(vStart / 360) * 360;
    if vEnd < vStart then
      vEnd := vEnd + 360;
    vAngleDelta := (vEnd - vStart) * fPiDividedBy180 / NumberPart;
    vAngle :=  vStart * fPiDividedBy180;
    I := 0;
    while I <= NumberPart do
    begin
      SinCos(vAngle, S, C);
      vPoint.X := vCurve^.Arc.Center.X + C * vCurve^.Arc.Radius;
      vPoint.Y := vCurve^.Arc.Center.Y + S * vCurve^.Arc.Radius;
      if Added then
        AddLastFPointUnique(TFPointList(AList), vPoint)
      else
        AddFirstFPointUnique(TFPointList(AList), vPoint);
      vAngle := vAngle + vAngleDelta;
      Inc(I);
    end;
  end
  else
  begin
    if Added then
    begin
      AddLastF2DPointUnique(TFPointList(AList), vCurve^.Line.Point1);
      AddLastF2DPointUnique(TFPointList(AList), vCurve^.Line.Point2);
    end
    else
    begin
      AddFirstF2DPointUnique(TFPointList(AList), vCurve^.Line.Point1);
      AddFirstF2DPointUnique(TFPointList(AList), vCurve^.Line.Point2);
    end;
  end;
end;

procedure CopyBackwardPF2DCurveEx(const AData, AList: Pointer; const Added: Boolean);
var
  vCurve: PF2DCurveEx absolute AData;
  vAngle, vStart, vEnd, vAngleDelta: Double;
  I, NumberPart: Integer;
  vPoint: TFPoint;
  S, C: Extended;
begin
  if lpArc in vCurve^.Flags then
  begin
    vPoint.Z := 0;
    NumberPart := GetNumberOfCircleParts;
    vStart := vCurve^.Arc.AngleS;
    vEnd := vCurve^.Arc.AngleE;
    if vStart > vEnd then
      vStart := vStart - Round(vStart / 360) * 360;
    if vEnd < vStart then
      vEnd := vEnd + 360;
    vAngleDelta := (vStart - vEnd) * fPiDividedBy180 / NumberPart;
    vAngle := vEnd * fPiDividedBy180;
    I := 0;
    while I <= NumberPart do
    begin
      SinCos(vAngle, S, C);
      vPoint.X := vCurve^.Arc.Center.X + C * vCurve^.Arc.Radius;
      vPoint.Y := vCurve^.Arc.Center.Y + S * vCurve^.Arc.Radius;
      if Added then
        AddLastFPointUnique(TFPointList(AList), vPoint)
      else
        AddFirstFPointUnique(TFPointList(AList), vPoint);
      vAngle := vAngle + vAngleDelta;
      Inc(I);
    end;
  end
  else
  begin
    if Added then
    begin
      AddLastF2DPointUnique(TFPointList(AList), vCurve^.Line.Point2);
      AddLastF2DPointUnique(TFPointList(AList), vCurve^.Line.Point1);
    end
    else
    begin
      AddFirstF2DPointUnique(TFPointList(AList), vCurve^.Line.Point2);
      AddFirstF2DPointUnique(TFPointList(AList), vCurve^.Line.Point1);
    end;
  end;
end;

function GetPoint1OfPF2DCurveEx(const APointer: Pointer): TFPoint;
var
  vCurve: PF2DCurveEx absolute APointer;
begin
  if lpArc in vCurve^.Flags then
    Result := GetPointOnCircle(vCurve^.Arc.Center, vCurve^.Arc.Radius, vCurve^.Arc.AngleS)
  else
  begin
    Result.X := vCurve^.Line.Point1.X;
    Result.Y := vCurve^.Line.Point1.Y;
  end;
  Result.Z := 0;
end;

function GetPoint2OfPF2DCurveEx(const APointer: Pointer): TFPoint;
var
  vCurve: PF2DCurveEx absolute APointer;
begin
  if lpArc in vCurve^.Flags then
    Result := GetPointOnCircle(vCurve^.Arc.Center, vCurve^.Arc.Radius, vCurve^.Arc.AngleE)
  else
  begin
    Result.X := vCurve^.Line.Point2.X;
    Result.Y := vCurve^.Line.Point2.Y;
  end;
  Result.Z := 0;
end;

function GetPointOnArcAR(const AArc: TsgArcR; const AAngle: Double;
  const ApplyZ: Boolean = True): TFPoint;
begin
  Result := GetPointOnCircle(AArc.Center, AArc.Radius, AAngle, ApplyZ);
end;

function GetPointOnCircle(const ACenter: TFPoint; const ARadius, Angle: Double;
  const ApplyZ: Boolean = True): TFPoint;
var
  S, C: Extended;
begin
  SinCos(Angle * fPiDividedBy180, S, C);
  Result.X := ACenter.X + ARadius * C;
  Result.Y := ACenter.Y + ARadius * S;
  if ApplyZ then
    Result.Z := ACenter.Z
  else
    Result.Z := 0;
end;

function GetPointOnCircleI(const ACenter: TPoint; const ARadius, Angle: Double): TPoint;
var
  vRez: TFPoint;
begin
  vRez := GetPointOnCircle(MakeFPoint(ACenter.X, ACenter.Y, 0), ARadius, Angle);
  Result.X := Round(vRez.X);
  Result.Y := Round(vRez.Y);
end;

function GetPointOnEllipse(const ACenter: TFPoint; const A, B, ASin, ACos,
  AAngle: Double): TFPoint;
var
  S, C: Extended;
  X, Y: Double;
begin
  SinCos(fPiDividedBy180 * AAngle, S, C);
  X := A * C;
  Y := B * S;
  Result.X := ACenter.X + X * ACos - Y * ASin;
  Result.Y := ACenter.Y + X * ASin + Y * ACos;
  Result.Z := ACenter.Z;
end;

function GetPointOnLine(const AP1, AP2: TFPoint; const ALength: Double;
  const AEpsilon: Extended = fDoubleResolution): TFPoint;
var
  vDistance, vDX, vDY, vDZ, K: Extended;
begin
  vDX := AP2.X - AP1.X;
  vDY := AP2.Y - AP1.Y;
  vDZ := AP2.Z - AP1.Z;
  vDistance := Sqr(vDX) + Sqr(vDY) + Sqr(vDZ);
  if (vDistance > AEpsilon) and (vDistance <> 0) then
  begin
    K := ALength / Sqrt(vDistance);
    Result.X := AP1.X + vDX * K;
    Result.Y := AP1.Y + vDY * K;
    Result.Z := AP1.Z + vDZ * K;
  end
  else
    Result := AP1;
end;

function GetPointOnLine(const ALine: TsgLine; const ALength: Double;
  const AEpsilon: Extended = fDoubleResolution): TFPoint; overload;
begin
  Result := GetPointOnLine(ALine.Point1, ALine.Point2, ALength, AEpsilon);
end;

function GetPointOnLineI(const AP1, AP2: TPoint; const ALength: Double): TPoint;
var
  vRez: TFPoint;
begin
  vRez := GetPointOnLine(MakeFPoint(AP1.X, AP1.Y, 0), MakeFPoint(AP2.X, AP2.Y, 0), ALength);
  Result.X := Ceil(vRez.X);
  Result.Y := Ceil(vRez.Y);
end;

function GetPointOnLineF(const AP1, AP2: TPointF; const ALength: Double): TPointF;
var
  vRez: TFPoint;
begin
  vRez := GetPointOnLine(MakeFPoint(AP1.X, AP1.Y, 0), MakeFPoint(AP2.X, AP2.Y, 0), ALength);
  Result.X := vRez.X;
  Result.Y := vRez.Y;
end;

//optimizated
function GetPointOnLineNormal(const APt1, APt2, ANormPt: TFPoint;
  const ADelta: Extended): TFPoint;
var
  DX, DY, Len, S, C: Extended;
begin
  DX := APt2.X - APt1.X;
  DY := APt2.Y - APt1.Y;
  if (DX = 0) and (DY = 0) then
    Result := ANormPt//incorrect line
  else
  begin
    Len := DX * DX + DY * DY;
    if Len < 0 then
      Len := 0;//overflow
    Len := Sqrt(Len);
    C := DX / Len;
    S := DY / Len;
    Result.X := ANormPt.X + S * ADelta;
    Result.Y := ANormPt.Y - C * ADelta;
    Result.Z := ANormPt.Z;
  end;
end;

function GetPositionOnEntities(const APoint: TFPoint; const ALine: PsgLine;
  const AArc: PsgArcR): TsPositionOnEntities;
var
  vMode: Integer;
begin
  Result := peNone;
  vMode := 0;
  if (ALine <> nil) and IsPointOnSegment(ALine^, APoint) then
   vMode := 1;
  if (AArc <> nil) and IsPointOnArcAP(AArc^, APoint) then
   vMode := vMode or 2;
  case vMode of
    1:  Result := peOnSegment;
    2:  Result := peOnArc;
    3:  Result := peOnArcAndSegment;
  end;
end;

function GetPointOfDoubleOffset(const APB1, APB2, APC: TFPoint; const AD1, AD2: Double; const APoint: PFPoint): Boolean;
var
  vP1, vP2: TFPoint;
  vLine: TsgLine;
begin
  case IsCrossCirclesCR(APB1, AD1, APB2, AD2, @vP1, @vP2) of
    1:
      begin
        Result := True;
        if APoint <> nil then
          APoint^ := vP1;
      end;
    2:
      begin
        Result := True;
        if APoint <> nil then
        begin
          vLine.Point1 := APB1;
          vLine.Point2 := APB2;
          if PointClassifyEx(vLine, APC) = PointClassifyEx(vLine, vP1) then
            APoint^ := vP1
          else
            APoint^ := vP2;          
        end;
      end;
  else
    Result := False;
  end;
end;

function GetNormalPt(const APt1, APt2, ANormPt: TFPoint; ADelta: TsgFloat): TFPoint;
var
  A, S, C: Extended;
begin
  A := GetAngleByPoints(APt1, APt2, True);
  SinCos(A, S, C);
  Result.X := ANormPt.X + S * ADelta;
  Result.Y := ANormPt.Y - C * ADelta;
  Result.Z := ANormPt.Z;
end;

function GetUnionLine(const ALine1, ALine2: TsgLine): TsgLine;
var
  vPts: TList;
begin
  vPts := TList.Create;
  try
    vPts.Count := 4;
    vPts[0] := @ALine1.Point1;
    vPts[1] := @ALine1.Point2;
    vPts[2] := @ALine2.Point1;
    vPts[3] := @ALine2.Point2;
    QSortList(vPts, TsgProcCompare(@ProcCompareFPointByX));
    if IsEqual(PFPoint(vPts[0])^.X, PFPoint(vPts[3])^.X) then
    begin
      QSortList(vPts, TsgProcCompare(@ProcCompareFPointByY));
      Result.Point1 := PFPoint(vPts[0])^;
      if IsEqual(PFPoint(vPts[0])^.Y, PFPoint(vPts[3])^.Y) then
        Result.Point2 := Result.Point1
      else
        Result.Point2 := PFPoint(vPts[3])^
    end
    else
    begin
      Result.Point1 := PFPoint(vPts[0])^;
      Result.Point2 := PFPoint(vPts[3])^;
    end;
  finally
    vPts.Free;
  end;
end;

function GetRadiusOfCircle(const AP1, AP2, AP3: TFPoint): Double;
var
  A, B, C, P, S: Extended;
begin
  A := DistanceFPoint2D(AP1, AP2);
  B := DistanceFPoint2D(AP1, AP3);
  C := DistanceFPoint2D(AP2, AP3);
  P := (A + B + C) * 0.5;
  S := P * (P - A) * (P - B) * (P - C);
  if S > fDoubleResolution then
    Result := A * B * C / (4 * Sqrt(S))
  else
    Result := 0;
end;

procedure GetRectangle(const ABox: TFRect; var APoints: TsgPoints5);
begin
  APoints[0] := ABox.TopLeft;
  APoints[1] := MakeFPoint(ABox.Right, ABox.Top, ABox.Z2);
  APoints[2] := ABox.BottomRight;
  APoints[3] := MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z1);
  APoints[4] := APoints[0];
end;

procedure GetPts4ByBox(const ABox: TFRect; var AResult: TsgPoints4);
begin
  AResult[0] := ABox.TopLeft;
  AResult[1] := MakeFPoint(ABox.Right, ABox.Top, ABox.Z2);
  AResult[2] := ABox.BottomRight;
  AResult[3] := MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z1);
end;

procedure GetPts4ByPointAndSize(const APoint: TFPoint;
  const Angle, AWidth, AHeight: Double; var AResult: TsgPoints4);
begin
  AResult[3] := APoint;
  if Abs(Angle) > fDoubleResolution then
  begin
    AResult[0] := GetPointOnCircle(AResult[3], AHeight, Angle + 90);
    AResult[2] := GetPointOnCircle(AResult[3], AWidth, Angle);
    AResult[1] := AddFPoint2D(AResult[0], SubFPoint2D(AResult[2], AResult[3]));
  end
  else
  begin
    AResult[0] := AResult[3];
    AResult[0].Y := AResult[0].Y + AHeight;
    AResult[1] := AResult[3];
    AResult[1].X := AResult[0].X + AWidth;
    AResult[2] := AResult[1];
    AResult[2].Y := AResult[3].Y;
  end
end;

function GetAngleByChordAndRadius(const A, R: Double): Double;
begin
  Result := sgArcSin(0.5 * A / R);
end;

function GetRadiusByChordAndHeight(const A, H: Double): Double;
begin
  Result := (Sqr(A) + 4 * Sqr(H)) / (8 * H);
end;

function GetBit64(ABit: Byte; const ADigit: UInt64): Byte;
begin
  Result := (ADigit shr ABit) and 1;
end;

procedure SetBit64(ABitNum, ABit: Byte; var ADigit: UInt64);
var
  vDigit: UInt64;
begin
  if (ABit <> 1) and (ABit <> 0) then
    ABit := 1;
  vDigit := 1;
  vDigit := vDigit shl ABitNum;
  if ABit = 0 then
  begin
    vDigit := not vDigit;
    ADigit := vDigit and ADigit;
  end
  else
    ADigit := vDigit or ADigit;
end;

function GetLengthByChordAndHeight(const A, H: Double): Double;
begin
  Result := Sqr(A) + Sqr(H) * 16 / 3;
  if Result > fDoubleResolution then
    Result := Sqrt(Result)
  else
    Result := 0;
end;

function GetHeightByChordAndLength(const A, L: Double): Double;
begin
  Result := Sqr(L) - Sqr(A);
  if Result > fDoubleResolution then
    Result := Sqrt(Result) * 3 / 16
  else
    Result := 0
end;

function GetHeightByChordAndRadius(const A, R: Extended): Double;
var
  vR1: Extended;
begin
  Result := R;
  vR1 := Sqr(R) - Sqr(A) / 4;
  if vR1 > fDoubleResolution then
     Result := R - Sqrt(vR1);
end;

function GetMiddleAngle(AStart, AEnd: Double): Double;
begin
  if AStart > AEnd then
    AStart := AStart - Round(AStart / 360) * 360;
  if AEnd < AStart then
    AEnd := AEnd + 360;
  Result := (AStart + AEnd) * 0.5;
  if Result > 360 then
    Result := Result - 360;
end;

function GetMiddleAngleAP(const AP: TsgArcR): Double;
begin
  Result := GetMiddleAngle(AP.AngleS, AP.AngleE);
end;

procedure GetAnglesFromRotMatrix(const Matrix: TFMatrix;
  var Roll, Yaw, Pitch: Double);
const
  fAccuracySelf = fDoubleResolution;

  function SetVal(const Value: Double): Extended;
  begin
    Result := Value;
    if IsRange(Value,fAccuracySelf) then
      Result := 0;
  end;

  function ArcTan2Self(const Y, X: Extended): Extended;
  begin
    Result := 0;
    if IsRange(X,fAccuracySelf) then Exit;
    Result := SetVal(ArcTan2(Y, X));
  end;

begin
  if IsRotatedFMat(Matrix) then
  begin
    Roll := ArcTan2Self(SetVal(Matrix.M[2][1]), Sqrt(SetVal(Matrix.M[2][0]) * SetVal(Matrix.M[2][0]) +
      SetVal(Matrix.M[2][2]) * SetVal(Matrix.M[2][2])));
    if Abs(Roll) >  fAccuracySelf then
      Roll := -Roll * f180DividedByPi
    else
      Roll := 0;
    Yaw := ArcTan2Self(SetVal(Matrix.M[2][0]), SetVal(Matrix.M[2][2])) * f180DividedByPi;
    Pitch := ArcTan2Self(SetVal(Matrix.M[0][1]), SetVal(Matrix.M[1][1])) * f180DividedByPi;
  end
  else
  begin
    Roll := 0;
    Yaw := 0;
    Pitch := 0;
  end;
end;

function GetResYFunction(const X1, Y1, X2, Y2, X: Double): Double;
var
  vDx: Double;
begin
  vDx := X2 - X1;
  if not sgIsZero(vDx) then
    Result := (X - X1) * (Y2 - Y1) / vDx + Y1
  else
   Result := NaN;
end;

function GetResXFunction(const X1, Y1, X2, Y2, Y: Double): Double;
var
  vDy: Double;
begin
  vDy := Y2 - Y1;
  if not sgIsZero(vDY) then
    Result := (Y - Y1) * (X2 - X1) / vDY + X1
  else
    Result := NaN;
end;

function GetRezOfQuadraticEquation(const A, B, C: Double;
  var Val1, Val2: Double): Integer;
var
  D: Double;
begin
  case Integer(not sgIsZero(A)) shl 1 + Integer(not sgIsZero(B)) of
    1:
      begin
        Result := 1;
        Val1 := -C / B;
      end;
    2:
      begin
        Val1 := -C / A;
        if Val1 > 0 then
        begin
          Result := 2;
          Val1 := Sqrt(Val1);
          Val2 := -Val1;
        end
        else
          Result := 0;
      end;
    3:
      begin
        D := Sqr(B) - 4 * A * C;
        case sgSignZ(D) of
          -1:
            begin
              Result := 1;
              Val1 := (- B) / (A * 2);
            end;
          +1:
            begin
              Result := 2;
              D := Sqrt(D);
              Val1 := (- B + D) / (A * 2);
              Val2 := (- B - D) / (A * 2);
            end
        else
          Result := 0;
        end;
      end;
  else
    Result := 0;
  end;
end;

function GetScaledRect(const APictureRect: TRect; const AExtents: TFRect;
  const ABox: TFRect): TRect;
var
  vKBoxLeft, vKBoxRight, vKBoxTop, vKBoxBottom: Extended;
  vExtWidth, vExtHeight, vPicWidth, vPicHeight: Extended;
begin
  vExtWidth := AExtents.Right - AExtents.Left;
  vExtHeight := AExtents.Top - AExtents.Bottom;
  if vExtWidth <> 0 then
  begin
    vPicWidth := APictureRect.Right - APictureRect.Left;
    vKBoxLeft  := (ABox.Left - AExtents.Left) / vExtWidth;
    vKBoxRight := (ABox.Right - AExtents.Left) / vExtWidth;
    Result.Left :=  RoundToInt(APictureRect.Left + vPicWidth * vKBoxLeft);
    Result.Right := RoundToInt(APictureRect.Left + vPicWidth * vKBoxRight);
  end
  else
  begin
    Result.Left :=  APictureRect.Left;
    Result.Right := APictureRect.Right;
  end;
  if vExtHeight <> 0 then
  begin
    vPicHeight := APictureRect.Bottom - APictureRect.Top;
    vKBoxTop := (ABox.Top - AExtents.Bottom) / vExtHeight;
    vKBoxBottom := (ABox.Bottom - AExtents.Bottom) / vExtHeight;
    Result.Top := RoundToInt(APictureRect.Bottom - vPicHeight * vKBoxTop);
    Result.Bottom := RoundToInt(APictureRect.Bottom - vPicHeight * vKBoxBottom);
  end
  else
  begin
    Result.Top :=  APictureRect.Top;
    Result.Bottom := APictureRect.Bottom;
  end;
end;


function GetLineOfFRect(const ARect: TFRect; const AIndex: Integer): TsgLine;
begin
  case AIndex of
    0:
      begin
        Result.Point1 := ARect.TopLeft;
        Result.Point2 := MakeFPoint(ARect.Right, ARect.Top, ARect.Z1);
      end;
    1:
      begin
        Result.Point1 := MakeFPoint(ARect.Right, ARect.Top, ARect.Z1);
        Result.Point2 := ARect.BottomRight;
      end;
    2:
      begin
        Result.Point1 := ARect.BottomRight;
        Result.Point2 := MakeFPoint(ARect.Left, ARect.Bottom, ARect.Z2);
      end;
  else
    Result.Point1 := MakeFPoint(ARect.Left, ARect.Bottom, ARect.Z2);
    Result.Point2 := ARect.TopLeft;
  end;
end;

function GetLineParalleniar(const AP, P1, P2: TFPoint; const ALen: Double): TsgLine;
var
  DX, DY, DZ, vKoef, vLen: Extended;
begin
  Result.Point1 := AP;
  if ALen = 0 then
  begin
    Result.Point2.X := AP.X + P2.X - P1.X;
    Result.Point2.Y := AP.Y + P2.Y - P1.Y;
    Result.Point2.Z := AP.Z + P2.Z - P1.Z;
  end
  else
  begin
    DX := P2.X - P1.X;
    DY := P2.Y - P1.Y;
    DZ := P2.Z - P1.Z;
    vLen := Sqr(DX) + Sqr(DY) + Sqr(DZ);
    if vLen > fDoubleResolution then
    begin
      vKoef := ALen / Sqrt(vLen);
      Result.Point2.X := AP.X + DX * vKoef;
      Result.Point2.Y := AP.Y + DY * vKoef;
      Result.Point2.Z := AP.Z + DZ * vKoef;
    end
    else
      Result.Point2 := AP;
  end;
end;

function GetLinePerpendicular2D(const AP, P1, P2: TFPoint; const ALen: Double): TsgLine;
var
  DX, DY, vKoef, vLen: Extended;
begin
  Result.Point1 := AP;
  if ALen = 0 then
  begin
    Result.Point2.X := AP.X + P2.Y - P1.Y;
    Result.Point2.Y := AP.Y + P1.X - P2.X;
    Result.Point2.Z := AP.Z;
  end
  else
  begin
    DX := P2.X - P1.X;
    DY := P2.Y - P1.Y;
    vLen := Sqr(DX) + Sqr(DY);
    if vLen > fDoubleResolution then
    begin
      vKoef := ALen / Sqrt(vLen);
      SinCosRot90(DX, DY);
      Result.Point2.X := AP.X + DX * vKoef;
      Result.Point2.Y := AP.Y + DY * vKoef;
      Result.Point2.Z := AP.Z;
    end
    else
      Result.Point2 := AP;
  end;
end;

function GetMiddleFPointOfBulge(const AP1, AP2: TFPoint; const ABulge: Double): TFPoint;
begin
  Result := GetNormalPt(AP1, AP2, MiddleFPoint2D(AP1, AP2),
    0.5 * ABulge * DistanceFPoint2D(AP1, AP2));
end;

function DoubleToStr(const AVal: Double;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil): string;
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
begin
  Result := FloatToStr(AVal, CreateFormatSettingsInternal(AFormatSettings, Separator));
end;
{$ELSE}
var
  vDS: Char;
begin
  vDS := SetDecimalSeparator(Separator);
  try
    Result := FloatToStr(AVal);
  finally
    SetDecimalSeparator(vDS);
  end;
end;
{$ENDIF}

function DoubleToStrF(const AVal: Double; const APrecision: Byte;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil): string;
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
begin
  Result := FloatToStrF(AVal, ffFixed, 7, APrecision, CreateFormatSettingsInternal(AFormatSettings, Separator));
end;
{$ELSE}
var
  vDS: Char;
begin
  vDS := SetDecimalSeparator(Separator);
  try
    Result := FloatToStrF(AVal, ffFixed, 7, APrecision);
  finally
    SetDecimalSeparator(vDS);
  end;
end;
{$ENDIF}

function DoubleToStrFEx(const AVal: Double; const APrecision: Byte;
  const AUseSystemSeparator: Boolean = True;
  AFormatSettings: PFormatSettings = nil): string;
begin
  if AUseSystemSeparator then
    Result := DoubleToStrF(AVal, APrecision, GetDecimalSeparator{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}, AFormatSettings{$ENDIF})
  else
    Result := DoubleToStrF(AVal, APrecision, cnstDoubleSeparator, AFormatSettings);
end;

function DoubleToStrVisualRound_(const AVal: Double; const APrecision: Byte;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;
begin
  if cnstVisualRound then
    Result := DoubleToStrF(AVal, APrecision, ASeparator, AFormatSettings)
  else
    Result := FloatToStr(AVal);
end;

function AreaToStr(const AValue: Double; ADimDec: Integer;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;
begin
  if ADimDec > 0 then
    Dec(ADimDec);
  Result := DoubleToStrVisualRound_(AValue, ADimDec, ASeparator, AFormatSettings);
end;

function PerimetrToStr(const AValue: Double; ADimDec: Integer;
  const ASeparator: Char = cnstDoubleSeparator; AFormatSettings: PFormatSettings = nil): string;
begin
  Result := DoubleToStrVisualRound_(AValue, ADimDec, ASeparator, AFormatSettings);
end;

function DoubleToStringWithPrecision(const AValue: Extended;
  const APrecision: Integer): string;
var
  vRoundParam: Integer;
  vTmpValue: Extended;
begin
  vRoundParam := APrecision;
  if vRoundParam < 0 then
    vRoundParam := 0;
  vTmpValue := RoundTo(AValue, - APrecision);
  Result := FloatToStrF(vTmpValue, ffFixed, 16, vRoundParam);
end;

function StringSentenceCase(const AStr: string): string;
var
  I, vLength: Integer;
  vPerfix: Char;
begin
  Result := AStr;
  vLength := Length(Result);
  if vLength > 0 then
  begin
    I := 1;
    while (I <= vLength) and (Result[I] = ' ') do
      Inc(I);
    if I <= vLength then
    begin
      vPerfix := Result[I];
      Result := AnsiLowerCase(Result);
      Result[I] := UpCase(vPerfix);
    end;
  end;
end;

function ExceptionToString(const E: Exception): string;
begin
{$IFDEF SGDEL_2009}
  Result := E.ToString;
{$ELSE}
  Result := E.Message;
{$ENDIF}
end;

function FPointToStr(const APoint: TFPoint; ASeparator: Char = ',';
  AFormatSettings: PFormatSettings = nil): string;
begin
  Result := FPointToStrEx(APoint, ASeparator, cnstPointSeparator, AFormatSettings);
end;

function FPointsToStr(APoints: IsgArrayFPoint): string;
var
  vPoints: TFPointList;
begin
  Result := '';
  if (APoints <> nil) and (APoints.FPointCount > 0) then
  begin
    vPoints := TFPointList.Create;
    try
      vPoints.Capacity := APoints.FPointCount;
      vPoints.AppendArray(APoints);
      Result := vPoints.ToStr;
    finally
      vPoints.Free;
    end;
  end;
end;

function FPointToStrEx(const APoint: TFPoint; ASeparator: Char; ADelimiter: Char;
  AFormatSettings: PFormatSettings = nil): string;
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
var
  vFormatSettings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
  if AFormatSettings = nil then
  begin
    vFormatSettings := CreateFormatSettings(ASeparator);
    AFormatSettings := @vFormatSettings;
  end;
{$ENDIF}
  Result := DoubleToStr(APoint.X, ASeparator, AFormatSettings) + ADelimiter + ' ' +
    DoubleToStr(APoint.Y, ASeparator, AFormatSettings) + ADelimiter + ' ' +
    DoubleToStr(APoint.Z, ASeparator, AFormatSettings);
end;

function ExtractWebFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('/:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function GetLongFileName(const FileName: string): string;
var
  vSearchRec: TSearchRec;
begin
  Result := FileName;
  {$IFNDEF SG_NON_WIN_PLATFORM}
  FillChar(vSearchRec.FindData.cFileName, SizeOf(vSearchRec.FindData.cFileName), #0);
  {$ENDIF}
  if FindFirst(FileName, faAnyFile, vSearchRec) = 0 then
  {$IFNDEF SG_NON_WIN_PLATFORM}
     Result := string(vSearchRec.FindData.cFileName);
  {$ELSE}
  Result := string(vSearchRec.Name);
  {$ENDIF}
  SysUtils.FindClose(vSearchRec);
end;

{$IFDEF SGFPC}
function mbGetModuleName(Address: Pointer): string;
{$IFDEF LINUX}
const
  Dummy: Boolean = False;
var
  dlinfo: dl_info;
begin
  if Address = nil then Address := @Dummy;
  FillChar({%H-}dlinfo, SizeOf(dlinfo), #0);
  if dladdr(Address, @dlinfo) = 0 then
    Result:= EmptyStr
  else
    Result:= UTF8Encode(dlinfo.dli_fname);
{$ELSE}
begin
  Result := GetFileName;
{$ENDIF}
end;
{$ENDIF}

function GetLongModuleName(ACheckLibrary: Boolean; AModule: HMODULE): string;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vFlNm: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  if ACheckLibrary and (not IsLibrary) and (not IsConsole) then
    Result := ParamStr(0)
  else
    SetString(Result, vFlNm, GetModuleFileName(AModule, vFlNm, MAX_PATH));
  Result := GetLongFileNameIsNeeded(Result);
{$ELSE}
  if ModuleIsLib or ModuleIsPackage then
  begin
{$IFDEF SGFPC}
    Result := mbGetModuleName(get_caller_addr(get_frame));
{$ELSE}
    // This line has not been tested on Linux, yet, but should work.
    Result := ParamStr(0);
//    SetLength(Result, GetModuleFileName(0, PChar(ExecutableFilename),
//      Length(ExecutableFilename)));
{$ENDIF}
  end
  else
    Result := ParamStr(0);
{$ENDIF}
end;

function GetFileName: string;
begin
  Result := GetLongModuleName(True, HInstance);
end;

function GetValidFileName(const AFileName: string): string;
var
  I: Integer;
begin
  Result := AFileName;
  for I := Length(Result) downto 1 do
    if CharInSet(Result[I], cnstFileNameForbiddenChars) then
      Delete(Result, I, 1);
  if Length(Result) > MAX_PATH then
    Delete(Result, 1, Length(Result) - MAX_PATH - 1);
end;

function GetLongFileNameIsNeeded(const AFileName: string): string;
var
  vLongFile: string;
begin
  Result := AFileName;
  vLongFile := GetLongFileName(AFileName);
  if not SameText(vLongFile, ExtractFileName(AFileName)) then
    Result := ShortToLongPath(Result);
end;

function StrToDouble(const AStr: string;
  const Separator: Char = cnstDoubleSeparator;
  AFormatSettings: PFormatSettings = nil): Double;
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
var
  vResult: Extended;
begin
  Result := 0;
  if TextToFloat(PChar(AStr), vResult, fvExtended, CreateFormatSettingsInternal(AFormatSettings, Separator)) then
    Result := vResult;
end;
{$ELSE}
var
  vDS: Char;
  vResult: Extended;
begin
  Result := 0;
  vDS := SetDecimalSeparator(Separator);
  try
    if TextToFloat(PChar(AStr), vResult, fvExtended) then
      Result := vResult;
  finally
    SetDecimalSeparator(vDS);
  end;
end;
{$ENDIF}

function StrToFPoint(AStr: string; var APoint: TFPoint;
  const ASeparator: Char = cnstDoubleSeparator;
  const ADelimeter: Char = cnstPointSeparator;
  AFormatSettings: PFormatSettings = nil): Boolean;
var
  vPos, L: Integer;

  function StrToDble(S: string): Double;
  var
    I, vLen: Integer;
  begin
    vLen := Length(S);
    if vLen > 0 then
    begin
      if ASeparator <> '.' then
      begin
        for I := 1 to vLen do
          if S[I] = '.' then
            S[I] := ASeparator;
      end;
      Result := StrToDouble(S, ASeparator, AFormatSettings);
    end
    else
      Result := 0;
  end;

{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
var
  vFormatSettings: TFormatSettings;
{$ENDIF}
begin
  Result := False;
  try
    L := Length(AStr);
    if L < 1 then Exit;
    {$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
    if AFormatSettings = nil then
    begin
      vFormatSettings := CreateFormatSettings(ASeparator);
      AFormatSettings := @vFormatSettings;
    end;
    {$ENDIF}
    Result := True;
    APoint := cnstFPointZero;
    vPos := StringScan(ADelimeter, AStr, 1);
    if vPos > 0 then
    begin
      APoint.X := StrToDble(Copy(AStr, 1, vPos - 1));
      Delete(AStr, 1, vPos);
      vPos := StringScan(ADelimeter, AStr, 1);
      if vPos > 0 then
      begin
        APoint.Y := StrToDble(Copy(AStr, 1, vPos - 1));
        Delete(AStr, 1, vPos);
        vPos := StringScan(ADelimeter, AStr, 1);
        if vPos > 0 then
          APoint.Z := StrToDble(Copy(AStr, 1, vPos - 1))
        else
          APoint.Z := StrToDble(AStr);
      end
      else
        APoint.Y := StrToDble(AStr);
    end
    else
      APoint.X := StrToDble(AStr);
  except
    Result := False;
  end;
end;

procedure StrToStrings(const AStr: string; const ADelimiter: string;
  const AStrings: TStrings);
{$IFNDEF SGDEL_2005}
var
  P, Start, LB: PChar;
  S: string;
  LineBreakLen: Integer;
  StrPosProc: function(S1, S2: PChar): PChar;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    P := PChar(AStr);
    if P <> nil then
    begin
      if not SysLocale.FarEast then
        StrPosProc := @StrPos
      else
        StrPosProc := @AnsiStrPos;
      LineBreakLen := Length(ADelimiter);
      while P^ <> #0 do
      begin
        Start := P;
        LB := StrPosProc(P, PChar(ADelimiter));
        while (P^ <> #0) and (P <> LB) do
          Inc(P);
        SetString(S, Start, P - Start);
        AStrings.Add(S);
        if P = LB then
          Inc(P, LineBreakLen);
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
{$ELSE}
begin
  AStrings.LineBreak := ADelimiter;
  AStrings.Text := AStr;
{$ENDIF}
end;

function StrToVal(const AStr: string; var ACode: Integer): Double;
var
  vDs: Char;
begin
  Result := 0;
  if Length(AStr) > 0 then
  begin
    vDs := SetDecimalSeparator('.');
    try
      Val(AStr, Result, ACode);
    finally
      SetDecimalSeparator(vDs);
    end;
  end;
end;

function SubStrCount(const SubStr, Str: string): Integer;
var
  vPos, vLen: Integer;
begin
  Result := 0;
  vPos := 1;
  vLen := Length(Str);
  repeat
    vPos := StringPos(SubStr, Str, vPos);
    if vPos > 0 then
    begin
      Inc(vPos);
      Inc(Result);
      if vPos > vLen then
        Break;
    end;
  until vPos = 0;
end;

function StringPos(const ASubStr, AStr: string; AStart: Integer): Integer;
var
  PRes: PChar;
begin
  Result := 0;
  if (AStart = 0) or (AStart > Length(AStr)) then Exit;
  PRes := SysUtils.StrPos(PChar(@AStr[AStart]), PChar(@ASubStr[1]));
  if PRes <> nil then
    Result := PRes - @AStr[1] + 1
  else
    Result := 0;
end;

function StringPosA(const ASubStr, AStr: AnsiString; AStart: Integer): Integer;
var
  I, J, K, vLength, vSubLength: Integer;
begin
  Result := 0;
  vLength := Length(AStr);
  vSubLength := Length(ASubStr);
  if (vLength > 0) and (vSubLength > 0) and (vLength - AStart + 1 >= vSubLength) then
  begin
    for I := AStart to vLength - vSubLength + 1 do
    begin
      if AStr[I] = ASubStr[1] then
      begin
        K := I + 1;
        J := 2;
        while (J <= vSubLength) and (AStr[K] = ASubStr[J]) do
        begin
          Inc(J);
          Inc(K);
        end;
        if J > vSubLength then
          Result := I;
      end;
      if Result > 0 then
        Break;
    end;
  end;
end;

function StringPosNum(const SubStr, Str: string; SubNum, Start: Integer): Integer;
var
  vPos: Integer;
begin
  Result := 0;
  if (Str = '') or (SubStr = '') or (SubNum = 0) or (SubStrCount(SubStr, Str) < SubNum) then Exit;
  vPos := Start;
  repeat
    vPos := StringPos(SubStr, Str, vPos);
    if vPos > 0 then
    begin
      Dec(SubNum);
      if SubNum = 0 then
      begin
        Result := vPos;
        Break;
      end
      else
        Inc(vPos);
    end;
  until vPos = 0;
end;

function StringScan(Chr: Char; const AStr: string; AStart: Integer;
  const AReverse: Boolean = False): Integer;
var
  I: Integer;
  PRes: PChar;
begin
  if AReverse then
  begin
    Result := 0;
    if (Length(AStr) >= AStart) and (AStart > 1) then
    begin
      for I := AStart downto 1 do
      begin
        if AStr[I] = Chr then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end
  else
  begin
    PRes := SysUtils.StrScan(@AStr[AStart], Chr);
    if PRes <> nil then
      Result := PRes - @AStr[1] + 1
    else
      Result := 0;
  end;
end;

function ChangeSeparator(const AStr: string; const Separator: Char =
  cnstDoubleSeparator): string;
begin
  Result := AStr;
  StringReplace(Result, '.', cnstDoubleSeparator);
end;

function StringReplace(var Str: string; const Old, New: string): Boolean;
begin
  Result := ReplaceAnsi(Str, Old, New, True);
end;

function StringTrim(const AStr: string; const ABegin, AEnd: Char;
  const AInternal: Boolean = True): string;
var
  vStart, vEnd, vLength: Integer;
begin
  Result := '';
  vLength := Length(AStr);
  if vLength > 0 then
  begin
    vStart := StringScan(ABegin, AStr, 1);
    if vStart > 0 then
    begin
      vEnd := vLength;
      while (vEnd > vStart) and (AStr[vEnd] <> AEnd) do
        Dec(vEnd);
      if vEnd > vStart then
      begin
        if AInternal then
          Result := Copy(AStr, vStart + 1, vEnd - vStart - 1)
        else
        begin
          Result := Copy(AStr, 1, vStart - 1) +
            Copy(AStr, vEnd + 1, vLength - vEnd  - 1);
        end;
      end;
    end;
  end;
end;

function StringTrimLeftRightStr(const S: string; const AChr: Char): string;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := S;
  if S = '' then Exit;
  while (Length(Result) > 0) and (Result[1] = AChr) do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = AChr) do
    Delete(Result, Length(Result), 1);
end;

function StringTrimLeftRight(const S: string; const AChr: Char = ' '): string;
begin
  Result := StringTrimLeftRightStr(S, AChr);
end;

{$IFDEF SGDEL_2009}
function StringTrimLeftRight(const S: AnsiString; const AChr: AnsiChar = ' '): AnsiString; overload;
begin
  Result := AnsiString(StringTrimLeftRightStr(string(S), Char(' ')));
end;
{$ENDIF}

function StringTrimLeftRightInside(const S: string; DoAll: Boolean = True): string;
const
  cnstTrimArray: array [0..2] of String = (#13, #10,' ');
var
  I, vPos: Integer;
begin
  Result := StringTrimLeftRight(S);
  if S = '' then Exit;
  for I := Low(cnstTrimArray) to High(cnstTrimArray) - Ord(not DoAll)  do
  begin
      vPos := Pos(cnstTrimArray[I], Result);
      while vPos > 0 do
      begin
        Delete(Result, vPos, 1);
        vPos := Pos(cnstTrimArray[I], Result)
      end;
  end;
end;

function ShortToLongFileName(const ShortName:string): string;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
begin
  SearchHandle := FindFirstFile(PChar(ShortName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then begin
    Result := string(Temp.cFileName);
    if Result = '' then Result := string(Temp.cAlternateFileName);
  end
  else Result := '';
  Windows.FindClose(SearchHandle);
end;
{$ELSE}
begin
  Result := ShortName;
end;
{$ENDIF}

function ShortToLongPath(const ShortName: string): string;
var
  LastSlash: PChar;
  TempPathPtr: PChar;
  vTempStr: string;
begin
  Result := '';
  vTempStr := ShortName;//for safe changing of ShortName parameter
  UniqueString(vTempStr);
  TempPathPtr := PChar(vTempStr);
  LastSlash := StrRScan(TempPathPtr, '\');
  while LastSlash <> nil do begin
    Result := '\' + ShortToLongFileName(TempPathPtr) + Result;
    if LastSlash <> nil then begin
      LastSlash^ := Char(0);
      LastSlash := StrRScan(TempPathPtr, '\');
    end;
  end;
  Result := TempPathPtr + Result;
end;

function ReplaceToPos(var S: string; const S1, S2: string; ATo: Integer): Integer;
var
  P: Integer;
begin
  Result := 0;
  while True do
  begin
    P := Pos(LowerCase(S1), LowerCase(S));
    if (P = 0) or (P > ATo) then
      Exit;
    Inc(Result);
    Delete(S, P, Length(S1));
    if S2 <> '' then
      Insert(S2, S, P);
  end;
end;

procedure ReplaceEvenUnEven(var S: string; const AChng, AUneven, AEven: string);
var
  P, I: Integer;
begin
  I := 1;
  while True do
  begin
    P := AnsiPos(LowerCase(AChng), LowerCase(S));
    if P = 0 then
      Exit;
    Delete(S, P, Length(AChng));
    if (I mod 2 = 1) and (AUneven <> '') then
      Insert(AUneven, S, P)
    else if (I mod 2 = 0) and (AEven <> '') then
      Insert(AEven, S, P);
    Inc(I);
  end;
end;

function DeleteEmptyStrings(const AStrings: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AStrings.Count - 1 downto 0 do
  begin
    if Length(AStrings[I]) < 1 then
    begin
      AStrings.Delete(I);
      Inc(Result);
    end;
  end;
end;

function SubAngles(const Angle1, Angle2: Double): Double;
begin
  Result := Abs(Angle1 - Angle2);
end;

function UnionArcWithArc(const Arc1, Arc2: TsgArcR; var AR: TsgArcR): Boolean;
var
  I, J: Integer;
  vD: array[0..3] of Double;
  vAngles: array [0..1, 0..1] of Double;
begin
  Result := IsEqualFPoints(Arc1.Center, Arc2.Center, fAccuracy) and IsEqual(Arc1.Radius, Arc2.Radius, fAccuracy);
  if Result then
  begin
    AR := Arc1;
    if IsAngleInAnglesAP(Arc2.AngleS, Arc1) then
      AR.AngleE := Arc2.AngleE
    else
      if IsAngleInAnglesAP(Arc2.AngleE, Arc1) then
        AR.AngleS := Arc2.AngleS
      else
      begin
        vAngles[0][0] := GetAngleByPoints(AR.Center, GetPointOnCircle(AR.Center, AR.Radius, Arc1.AngleS), False);
        vAngles[0][1] := GetAngleByPoints(AR.Center, GetPointOnCircle(AR.Center, AR.Radius, Arc1.AngleE), False);
        vAngles[1][0] := GetAngleByPoints(AR.Center, GetPointOnCircle(AR.Center, AR.Radius, Arc2.AngleS), False);
        vAngles[1][1] := GetAngleByPoints(AR.Center, GetPointOnCircle(AR.Center, AR.Radius, Arc2.AngleE), False);

        vD[0] := SubAngles(vAngles[0][0], vAngles[1][0]);
        vD[1] := SubAngles(vAngles[0][0], vAngles[1][1]);
        vD[2] := SubAngles(vAngles[0][1], vAngles[1][0]);
        vD[3] := SubAngles(vAngles[0][1], vAngles[1][1]);

        J := 0;
        for I := 1 to 3 do
          if vD[J] > vD[I] then
            J := I;
        case J of
          1:  AR.AngleS := Arc2.AngleS;
          2:  AR.AngleE := Arc2.AngleE;
          3:  AR.AngleE := Arc2.AngleS;
        else
          AR.AngleS := Arc2.AngleE;
        end;
        AR.AngleS := sgMod(AR.AngleS, 360);
        AR.AngleE := sgMod(AR.AngleE, 360);
        if AR.AngleE < AR.AngleS  then
          AR.AngleE := AR.AngleE + 360;
      end;
  end;
end;

procedure UnionFRect(var Dst: TFRect; const Src: TFRect);
begin
  if IsBadRect(Src) then
    Exit;
  ExpandFRect(Dst, Src.TopLeft);
  ExpandFRect(Dst, Src.BottomRight);
end;

function DSplitLineAndLine(const Line1, Line2: TsgLine; var ALines1, ALines2: TList): Byte;
var
  vCross: TFPoint;                                                                         
begin
   Result := 0;
  if (ALines1 = nil) or (ALines2 = nil) then
    Exit;
  if IsCrossSegments(Line1, Line2, @vCross) then
  begin
    SafeSplitLinePtsToList(Line1, vCross, ALines1);
    SafeSplitLinePtsToList(Line2, vCross, ALines2);
    Result := 1;
  end;
end;

function DSplitLineAndArc(const Line: TsgLine; const Arc: TsgArcR; ALines, AArcs: TList): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  if (ALines = nil) or (AArcs = nil) then
    Exit;
  Result := IsCrossArcAndSegmentAP(Arc, Line, @vCross1, @vCross2);
  case Result of
    1:
      begin
        SafeSplitLinePtsToList(Line, [vCross1], ALines);
        SafeSplitArcPtsToList(Arc, [vCross1], AArcs);
      end;
    2:
      begin
        SafeSplitLinePtsToList(Line, [vCross1, vCross2], ALines);
        SafeSplitArcPtsToList(Arc, [vCross1, vCross2], AArcs);
      end;
  end;
end;

function DSplitArcAndArc(const Arc1, Arc2: TsgArcR; AArcs1, AArcs2: TList): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  if (AArcs1 = nil) or (AArcs2 = nil) then
    Exit;
  Result := IsCrossArcsAP(Arc1, Arc2, @vCross1, @vCross2);
  case Result of
    1:
      begin
        SafeSplitArcPtsToList(Arc1, [vCross1], AArcs1);
        SafeSplitArcPtsToList(Arc2, [vCross1], AArcs2);
      end;
    2:
      begin
        SafeSplitArcPtsToList(Arc1, [vCross1, vCross2], AArcs1);
        SafeSplitArcPtsToList(Arc2, [vCross1, vCross2], AArcs2);
      end;
  end;
end;

function SplitArcWithArc(const Arc1, Arc2: TsgArcR; var AR1, AR2, AR3: TsgArcR): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossArcsAP(Arc1, Arc2, @vCross1, @vCross2) of
    1:
      begin
        if CheckArcPnt(Arc1, vCross1) then
        begin
          Result := 1;
          SplitArcPt(Arc1, vCross1, AR1, AR2);
        end;
      end;
    2:
      begin
        case Byte(CheckArcPnt(Arc1, vCross1)) + Byte(CheckArcPnt(Arc1, vCross2)) shl 1 of
          1:
            begin
              Result := 1;
              SplitArcPt(Arc1, vCross1, AR1, AR2);
            end;
          2:
            begin
              Result := 1;
              SplitArcPt(Arc1, vCross2, AR1, AR2);
            end;
          3:
            begin
              Result := 2;
              SplitArcPt2(Arc1, vCross1, vCross2, AR1, AR2, AR3);
            end;
        end;
      end;
  end;
end;

function SplitArcWithLine(const Arc: TsgArcR; const Line: TsgLine;
  var AR1, AR2, AR3: TsgArcR; const ACross1: PFPoint = nil; const ACross2: PFPoint = nil): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossArcAndSegmentAP(Arc, Line, @vCross1, @vCross2) of
    1:
      begin
        if CheckArcPnt(Arc, vCross1) then
        begin
          Result := 1;
          if ACross1 <> nil then
            ACross1^ := vCross1;
          SplitArcPt(Arc, vCross1, AR1, AR2);
        end;
      end;
    2:
      begin
        case Byte(CheckArcPnt(Arc, vCross1)) + Byte(CheckArcPnt(Arc, vCross2)) shl 1 of
          1:
            begin
              Result := 1;
              if ACross1 <> nil then
                ACross1^ := vCross1;
              SplitArcPt(Arc, vCross1, AR1, AR2);
            end;
          2:
            begin
              Result := 1;
              if ACross1 <> nil then
                ACross1^ := vCross2;
              SplitArcPt(Arc, vCross2, AR1, AR2);
            end;
          3:
            begin
              Result := 2;
              if ACross1 <> nil then
                ACross1^ := vCross1;
              if ACross2 <> nil then
                ACross2^ := vCross2;
              SplitArcPt2(Arc, vCross1, vCross2, AR1, AR2, AR3);
            end;
        end;
      end;
  end;
end;

function SplitCircleWithLine(const ACenter: TFPoint; const ARadius: Double; const Line: TsgLine; var AL1, AL2: TsgArcR): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossCircleAndSegmentCR(ACenter, ARadius, Line, @vCross1, @vCross2) of
    1:
      begin
        if IsCrossCircleAndLineCR(ACenter, ARadius, Line, nil, nil) = 1 then
        begin
          Result := 1;
          AL1.Center := ACenter;
          AL1.Radius := ARadius;
          AL1.AngleS := GetAngleByPoints(ACenter, vCross1, False);
          AL1.AngleE := AL1.AngleS + 360;
        end;
      end;
    2:
      begin
        Result := 2;
        AL1.Center := ACenter;
        AL1.Radius := ARadius;
        AL1.AngleS := GetAngleByPoints(ACenter, vCross1, False);
        AL1.AngleE := GetAngleByPoints(ACenter, vCross2, False);;
        AL2 := AL1;
        SwapSGFloats(AL2.AngleS, AL2.AngleE);
      end;
  end;
end;

function SplitLineWithArc(const Line: TsgLine; const Arc: TsgArcR; var AL1, AL2, AL3: TsgLine): Byte;
var
  vCross1, vCross2: TFPoint;
begin
  Result := 0;
  case IsCrossArcAndSegmentAP(Arc, Line, @vCross1, @vCross2) of
    1:
      begin
        if CheckLinePnt(Line, vCross1) then
        begin
          Result := 1;
          SplitLinePt(Line, vCross1, AL1, AL2);
        end;
      end;
    2:
      begin
        case Byte(CheckLinePnt(Line, vCross1)) + Byte(CheckLinePnt(Line, vCross2)) shl 1 of
          1:
            begin
              Result := 1;
              SplitLinePt(Line, vCross1, AL1, AL2);
            end;
          2:
            begin
              Result := 1;
              SplitLinePt(Line, vCross2, AL1, AL2);
            end;
          3:
            begin
              Result := 2;
              SplitLinePt2(Line, vCross1, vCross2, AL1, AL2, AL3);
            end;
        end;
      end;
  end;
end;

function SplitLineWithLine(const Line1, Line2: TsgLine; var AL1, AL2: TsgLine): Byte;
var
  vCross: TFPoint;
begin
  Result := 0;
  if IsCrossSegments(Line1, Line2, @vCross) then
  begin
    if CheckLinePnt(Line1, vCross) then
    begin
      Result := 1;
      SplitLinePt(Line1, vCross, AL1, AL2);
    end;
  end;
end;

procedure SplitLinePt(const ALB: TsgLine; const AP: TFPoint; var AL1, AL2: TsgLine);
begin
  AL1.Point1 := ALB.Point1;
  AL1.Point2 := AP;
  AL2.Point1 := AP;
  AL2.Point2 := ALB.Point2;
end;

procedure SplitLinePt2(const ALB: TsgLine; const AP1, AP2: TFPoint; var AL1, AL2, AL3: TsgLine);
begin
  AL1.Point1 := ALB.Point1;
  AL3.Point2 := ALB.Point2;
  if DistanceFPointSqr(ALB.Point1, AP1) < DistanceFPointSqr(ALB.Point1, AP2) then
  begin
    AL1.Point2 := AP1;
    AL3.Point1 := AP2;
  end
  else
  begin
    AL1.Point2 := AP2;
    AL3.Point1 := AP1;
  end;
  AL2.Point1 := AL1.Point2;
  AL2.Point2 := AL3.Point1;
end;

procedure SplitArcPt(const Arc: TsgArcR; const AP: TFPoint; var Arc1, Arc2: TsgArcR);
var
  vAngle: Double;
begin
  vAngle := GetAngleByPoints(Arc.Center, AP, False);
  Arc1 := Arc;
  Arc2 := Arc;
  Arc1.AngleE := vAngle;
  Arc2.AngleS := vAngle;
end;

procedure SplitArcPt2(const Arc: TsgArcR; const AP1, AP2: TFPoint; var Arc1, Arc2, Arc3: TsgArcR);
var
  vAngle1, vAngle2: Double;
begin
  vAngle1 := GetAngleByPoints(Arc.Center, AP1, False);
  vAngle2 := GetAngleByPoints(Arc.Center, AP2, False);
  Arc1 := Arc;
  Arc3 := Arc;
  if IsAngleInAngles(vAngle2, Arc.AngleS, vAngle1) then
  begin
    Arc1.AngleE := vAngle2;
    Arc3.AngleS := vAngle1;
  end
  else
  begin
    Arc1.AngleE := vAngle1;
    Arc3.AngleS := vAngle2;
  end;
  Arc2.Center := Arc.Center;
  Arc2.Radius := Arc.Radius;
  Arc2.AngleS := Arc1.AngleE;
  Arc2.AngleE := Arc3.AngleS;
end;

function SafeSplitLinePt(const ALine: TsgLine; const ACutPt: TFPoint;
  var ALine1, ALine2: PsgLine; AEpsilon: Double): Boolean;
begin
  Result := False;
  if CheckLinePnt(ALine, ACutPt) and IsPointOnSegment(ALine, ACutPt, AEpsilon) then
  begin
    New(ALine1);
    New(ALine2);
    SplitLinePt(ALine, ACutPt, ALine1^, ALine2^);
    Result := True;
  end;
end;

function SafeSplitArcPt(const AArc: TsgArcR; const ACutPt: TFPoint;
  var AArc1, AArc2: PsgArcR; AEpsilon: Double = fDoubleResolution): Boolean;
begin
  Result := False;
  if CheckArcPnt(AArc, ACutPt) and IsPointOnArcAP(AArc, ACutPt, AEpsilon) then
  begin
    New(AArc1);
    New(AArc2);
    SplitArcPt(AArc, ACutPt, AArc1^, AArc2^);
    Result := True;
  end;
end;

function SafeSplitLinePtsToList(const ASourceLine: TsgLine; const ACutPts: array of TFPoint;
  const AList: TList; AEpsilon: Double = fDoubleResolution): Boolean;
var
  I, J: Integer;
  L1, L2: PsgLine;
begin
  Result := False;
  if AList = nil then
    Exit;
  New(L1);
  L1^ := ASourceLine;
  AList.Add(L1);
  I := 0;
  while I <= High(ACutPts) do
  begin
    J := 0;
    while J < AList.Count do
    begin
      if SafeSplitLinePt(PsgLine(AList[J])^, ACutPts[I], L1, L2, AEpsilon) then
      begin
        DisposeDeleteFromList(AList, J);
        AList.Insert(J, L1);
        AList.Insert(J + 1, L2);
        Result := True;
        Break;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
  if not Result then
    ClearRecordList(AList);
end;

function SafeSplitArcPtsToList(const ASourceArc: TsgArcR; const ACutPts: array of TFPoint;
  const AList: TList; AEpsilon: Double = fDoubleResolution): Boolean;
var
  I, J: Integer;
  A1, A2: PsgArcR;
begin
  Result := False;
  if AList = nil then
    Exit;
  New(A1);
  A1^ := ASourceArc;
  AList.Add(A1);
  I := 0;
  while I <= High(ACutPts) do
  begin
    J := 0;
    while J < AList.Count do
    begin
      if SafeSplitArcPt(PsgArcR(AList[J])^, ACutPts[I], A1, A2, AEpsilon) then
      begin
        DisposeDeleteFromList(AList, J);
        AList.Insert(J, A1);
        AList.Insert(J + 1, A2);
        Result := True;
        Break;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
  if not Result then
    ClearRecordList(AList);  
end;

procedure SwapArcs(var A,B: TsgArcR);
var
  vTmp: TsgArcR;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapBoolean(var A,B: Boolean);
var
  vTmp: Boolean;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapByte(var A,B: Byte);
var
  vTmp: Byte;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

function SwapBytes(AValue:Word):Word;register;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFDEF SG_FAST_CODE}
  {$IFDEF SG_CPUX64}
  asm
    xchg cl,ch
    mov eax, ecx
  end;
  {$ELSE}
  asm
    xchg al,ah
  end;
  {$ENDIF}
{$ELSE}
var
  vValues: array[0..1] of Byte absolute AValue;
  vResults: array[0..1] of Byte absolute Result;
begin
  vResults[0] := vValues[1];
  vResults[1] := vValues[0];
end;
{$ENDIF}

function SwapWords(AValue:DWORD):DWORD; register;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFDEF SG_FAST_CODE}
  {$IFDEF SG_CPUX64}
  asm
    bswap ecx
    mov eax, ecx
  end;
  {$ELSE}
  asm
    bswap eax
  end;
  {$ENDIF}
{$ELSE}
var
  vValues: array[0..4] of Byte absolute AValue;
  vResults: array[0..4] of Byte absolute Result;
begin
  vResults[0] := vValues[3];
  vResults[1] := vValues[2];
  vResults[2] := vValues[1];
  vResults[3] := vValues[0];
end;
{$ENDIF}

procedure SwapUInt64(var A,B: UInt64);{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vTmp: UInt64;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapDoubles(var A,B: Double);
var
  vTmp: Double;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapExtendeds(var A,B: Extended);
var
  vTmp: Extended;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapFPoints(var A,B: TFPoint);
var
  vTmp: TFPoint;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapF2DPoints(var A,B: TF2DPoint);
var
  vTmp: TF2DPoint;
begin
  vTmp := B;
  B := A;
  A := vTmp;
end;

procedure SwapObjects(var A,B: TObject);
var
  C: TObject;
begin
  C := A;
  A := B;
  B := C;
end;

procedure SwapPoints(var A,B: TPoint);
begin
  SwapInts(A.X, B.X);
  SwapInts(A.Y, B.Y);
end;

procedure SwapPointers(var A,B: Pointer);
var
  C: Pointer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure SwapFLines(var A,B: TsgLine);
var
  vTmp: TFPoint;
begin
  vTmp := A.Point1;
  A.Point1 := B.Point1;
  B.Point1 := vTmp;
  vTmp := A.Point2;
  A.Point2 := B.Point2;
  B.Point2 := vTmp;
end;

procedure SwapFMatrix(var A,B: TFMatrix);
begin
  SwapFPoints(A.EX, B.EX);
  SwapFPoints(A.EY, B.EY);
  SwapFPoints(A.EZ, B.EZ);
  SwapFPoints(A.E0, B.E0);
end;

procedure SwapString(var AStr1, AStr2: string);
var
  vTmp: string;
begin
  vTmp := AStr2;
  AStr2 := AStr1;
  AStr1 := vTmp;
end;

procedure SinCosRot90(var S, C: Extended);
var
  T: Extended;
begin
  T := S;
  S := -C;
  C := T;
end;

procedure FPointSinCosRot90(var ASinCos: TFPoint);
var
  T: Double;
begin
  T := ASinCos.Y;
  ASinCos.Y := -ASinCos.X;
  ASinCos.X := T;
end;

function RoundToInt(AValue: Double): Integer;
const
  cnstMaxInt = MaxInt;
  cnstMinInt = -(cnstMaxInt - 1);
begin
  if AValue > cnstMaxInt then
    Result := cnstMaxInt
  else
    if AValue < cnstMinInt then
      Result := cnstMinInt
    else
      Result := Round(AValue);
end;

{$IFNDEF SGDEL_7}
function IsNan(const AValue: Single): Boolean;
begin
  Result := ((PLongWord(@AValue)^ and $7F800000)  = $7F800000) and
            ((PLongWord(@AValue)^ and $007FFFFF) <> $00000000);
end;

function IsNan(const AValue: Extended): Boolean;
begin
{$IFDEF CPUX64}
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
{$ELSE !CPUX64}
  Result := ((PExtendedRec(@AValue)^.Exp and $7FFF)  = $7FFF) and
            ((PExtendedRec(@AValue)^.Frac and $7FFFFFFFFFFFFFFF) <> 0);
{$ENDIF}
end;

{$IFNDEF SGDEL_6}
function IsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000)
end;

function DateTimeToJulianDate(const AValue: TDateTime): Double;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := (1461 * (LYear + 4800 + (LMonth - 14) div 12)) div 4 +
            (367 * (LMonth - 2 - 12 * ((LMonth - 14) div 12))) div 12 -
            (3 * ((LYear + 4900 + (LMonth - 14) div 12) div 100)) div 4 +
            LDay - 32075.5 + Frac(AValue);
end;

function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LMilliSeconds + (LSeconds + (LMinutes + LHours * 60) * 60) * 1000;
end;

function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
var
  LFactor: Double;
begin
  LFactor := IntPower(10, ADigit);
  Result := Round(AValue / LFactor) * LFactor;
end;

function DirectoryExists(const Directory: string): Boolean;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  Attr : DWORD;
  PathZ: array [0..255] of AnsiChar;
{$ELSE}
var
  SB: TStat;
{$ENDIF}
begin
  Result := False;
  if (Pos('*', Directory) <> 0) or (Pos('?', Directory) <> 0) then
    Exit;
{$IFNDEF SG_NON_WIN_PLATFORM}
  Attr := GetFileAttributes(StrPCopy( PathZ, Directory));
  if (Attr <> DWORD(-1)) and ((Attr and faDirectory) <> 0) then
    Result := true;
{$ELSE}
  if IsFileExists(Directory) then begin
    FPstat(PAnsiChar(Directory), SB);
    Result := (SB.st_mode and AB_FMODE_DIR) = AB_FMODE_DIR;
  end;
{$ENDIF}
end;

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  Result := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMinute, ASecond, AMilliSecond);
end;

procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word);
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  DecodeTime(AValue, AHour, AMinute, ASecond, AMilliSecond);
end;

{$ENDIF}
{$ENDIF}

function IsFileExists(const AFile: string): Boolean;
begin
  Result := False;
  if FileExists(AFile) then
    Result := True;
end;

function sgGetCompressedFileSize(lpFileName: PChar; lpFileSizeHigh: PSGCardinal): Cardinal;
{$IFDEF SG_NON_WIN_PLATFORM}
var
  vMemoryStream: TMemoryStream;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  Result := Windows.GetCompressedFileSize(lpFileName, PDWORD(lpFileSizeHigh));
{$ELSE}
  vMemoryStream := TMemoryStream.Create;
  try
    vMemoryStream.LoadFromFile(string(lpFileName));
    Result := Cardinal(vMemoryStream.Size);
    if lpFileSizeHigh <> nil then
      lpFileSizeHigh^ := Cardinal(vMemoryStream.Size shr 32);
  finally
    vMemoryStream.Free;
  end;
{$ENDIF}
end;

function AsSingle(const AValue: Extended; const ADefaultMax: Single = 0): Single;
begin
  if Abs(AValue) < MinSingle then
    Result := 0
  else
    if Abs(AValue) > MaxSingle then
      Result := ADefaultMax
    else
      Result := AValue;
end;

function sgMulDiv(const ANumber, ANumerator, ADenominator: Integer): Integer;
begin
  Result := (ANumber * ANumerator) div ADenominator;
end;

function sgRoundTo(const AValue: Double; const ADigit: Integer): Double;
begin
  Result := RoundTo(AValue, ADigit);
end;

function sgRoundTo(const AValue: Double; const ADigit, AFactor: Integer): Double;
var
  vFrac: Double;
  vTrunc: Integer;
begin
  if AFactor > 1 then
  begin
//    if ADigit < 0 then
//    begin
//      vTrunc := Trunc(AValue);
//      vFrac := Frac(AValue);
//      vFrac := RoundTo(vFrac / AFactor, ADigit) * AFactor;
//      Result := vTrunc + vFrac;
//    end
//    else
      Result := RoundTo(AValue / AFactor, ADigit) * AFactor;
  end
  else
    Result := RoundTo(AValue, ADigit);
end;

function sgSimpleRoundTo(const AValue: Double;
  const ADigit: TRoundToRange = 0): Double;
const
  cnstEpsilon = 0.5 + fDoubleResolution;
var
  LFactor: Extended;
begin
  LFactor := IntPower(10.0, ADigit);
  if AValue < 0 then
    Result := Int((AValue / LFactor) - cnstEpsilon) * LFactor
  else
    Result := Int((AValue / LFactor) + cnstEpsilon) * LFactor;
end;

procedure CheckValueExtents(var AValue: Double);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  if AValue > fMaxDoubleValue then
    AValue := fMaxDoubleValue
  else
    if AValue < fMinDoubleValue then
      AValue := fMinDoubleValue;
end;

procedure CheckListOfPFPoint(const AListNew, AListOrg: TList);
var
  P: PFPoint;
begin
  while AListNew.Count < AListOrg.Count do
  begin
    New(P);
    AListNew.Add(P);
  end;
  while AListNew.Count > AListOrg.Count do
  begin
    FreeMem(AListNew[AListNew.Count - 1]);
    AListNew.Count := AListNew.Count - 1;
  end;
end;

procedure CheckListOfPF2DPoint(const AListNew, AListOrg: TList);
var
  P: PF2DPoint;
begin
  while AListNew.Count < AListOrg.Count do
  begin
    New(P);
    AListNew.Add(P);
  end;
  while AListNew.Count > AListOrg.Count do
  begin
    FreeMem(AListNew[AListNew.Count - 1]);
    AListNew.Count := AListNew.Count - 1;
  end;
end;

procedure DoFreeObject(const AObject: TObject; AListType: Integer);
begin
  if AListType < 0 then
  begin
    if AObject is TList then
      AListType := 1
    else
      if AObject is TsgList then
        AListType := 2
      else
        if AObject is TsgObjectList then
          AListType := 3
        else
          AListType := 0;
  end;
  case AListType of
    1: ClearList(TList(AObject));
    2: ClearsgList(TsgList(AObject));
    3: TsgObjectList.ClearList(TsgObjectList(AObject));
  end;
  AObject.Free;
end;

procedure CheckListOfObject(const AListNew, AListOrg: TList;
  const AClass: TClass; const AIsList: Integer = -1);
var
  vObject: TObject;
  I : Integer;
begin
  if AListNew.Count > AListOrg.Count then
  begin
    for I := AListOrg.Count to AListNew.Count - 1 do
    begin
      vObject := AListNew[I];
      if vObject <> nil then
      begin
        AListNew[I] := nil;
        DoFreeObject(vObject, AIsList);
      end;
    end;
    AListNew.Count :=  AListOrg.Count;
  end
  else
    while AListNew.Count < AListOrg.Count do
    begin
      vObject := TClass(AClass).Create;
      AListNew.Add(vObject);
    end;
end;

procedure CheckListOfObject(const AListNew, AListOrg: TsgObjectList;
  const AClass: TClass; const AIsList: Integer = -1); overload;
var
  vObject: TObject;
  I : Integer;
begin
  if AListNew.Count > AListOrg.Count then
  begin
    for I := AListOrg.Count to AListNew.Count - 1 do
    begin
      vObject := AListNew[I];
      if vObject <> nil then
      begin
        AListNew[I] := nil;
        DoFreeObject(vObject, AIsList);
      end;
    end;
    AListNew.Count :=  AListOrg.Count;
  end
  else
    while AListNew.Count < AListOrg.Count do
    begin
      vObject := TClass(AClass).Create;
      AListNew.Add(vObject);
    end;
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function GetFileVerFixed(const AFileName: string; var AFI: TVSFixedFileInfo): Boolean;
var
  vFileName: string;
  vInfoSize, vWnd: DWORD;
  vVerBuf: Pointer;
  vFI: PVSFixedFileInfo;
  vVerSize: DWORD;
begin
  Result := False;
  vFileName := AFileName;
  UniqueString(vFileName);
  vInfoSize := GetFileVersionInfoSize(PChar(vFileName), vWnd);
  if vInfoSize <> 0 then
  begin
    GetMem(vVerBuf, vInfoSize);
    try
      if GetFileVersionInfo(PChar(vFileName), vWnd, vInfoSize, vVerBuf) then
        if VerQueryValue(vVerBuf, '\', Pointer(vFI), vVerSize) then
        begin
          AFI := vFI^;
          Result := True;
        end;
    finally
      FreeMem(vVerBuf);
    end;
  end;
end;
{$ENDIF}

function GetApplicationName: string;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vFlNm: PChar;
  vLongFile: string;

  function GetLongFileName(FileName: string): string;
  var
    vSearchRec: TSearchRec;
  begin
    Result := FileName;
     FillChar(vSearchRec.FindData.cFileName, SizeOf(vSearchRec.FindData.cFileName), #0);
     if FindFirst(FileName, faAnyFile, vSearchRec) = 0 then
       Result := string(vSearchRec.FindData.cFileName);
     FindClose(vSearchRec);
  end;

  function ShortToLongFileName(const ShortName: string): string;
  var
    Temp: TWin32FindData;
    SearchHandle: THandle;
  begin
    SearchHandle := FindFirstFile(PChar(ShortName), Temp);
    if SearchHandle <> INVALID_HANDLE_VALUE then begin
      Result := string(Temp.cFileName);
      if Result = '' then Result := string(Temp.cAlternateFileName);
    end
    else Result := '';
    Windows.FindClose(SearchHandle);
  end;

  function ShortToLongPath(const ShortName: string): string;
  var
    LastSlash: PChar;
    TempPathPtr: PChar;
  begin
    Result := '';
    TempPathPtr := PChar(ShortName);
    LastSlash := StrRScan(TempPathPtr, '\');
    while LastSlash <> nil do begin
      Result := '\' + ShortToLongFileName(TempPathPtr) + Result;
      if LastSlash <> nil then
      begin
        LastSlash^ := char(0);
        LastSlash := StrRScan(TempPathPtr, '\');
      end;
    end;
    Result := TempPathPtr + Result;
  end;

begin
  if (not IsLibrary) and (not IsConsole) then
    Result := ParamStr(0)
  else
  begin
    vFlNm := StrAlloc(MAX_PATH + 1);
    try
      SetString(Result, vFlNm, GetModuleFileName(HInstance, vFlNm, MAX_PATH));
    finally
      StrDispose(vFlNm);
    end;
  end;
  vLongFile := GetLongFileName(Result);
  if AnsiUpperCase(vLongFile) <> AnsiUpperCase(ExtractFileName(Result)) then
    Result := ShortToLongPath(Result);
end;
{$ELSE}
begin
  Result := ParamStr(0);
end;

{$ENDIF}

function GetCADPixelSize(const AMatrix: TFMatrix;
  const ANumberPixels: Double): Double;
//var
//  Pdxyz, vPX, vPY, vPZ, vP0: TFPoint;
//  vSizeMax: Double;
//  vMatrix: TFMatrix;
//begin
//   vP0 := FPointXMat(cnstFPointZero, AMatrix);
//   vPX := FPointXMat(cnstXOrtAxis, AMatrix);
//   vPY := FPointXMat(cnstYOrtAxis, AMatrix);
//   vPZ := FPointXMat(cnstZOrtAxis, AMatrix);
//   Pdxyz.X := DistanceFPoint(vPX, vP0);
//   Pdxyz.Y := DistanceFPoint(vPY, vP0);
//   Pdxyz.Z := DistanceFPoint(vPZ, vP0);
//   vSizeMax := Max(Max(Pdxyz.X, Pdxyz.Y), Pdxyz.Z);
//   Result :=  Abs(ANumberPixels / IfThen(vSizeMax = 0, 1, vSizeMax));
var
  vX, vY, vZ: Double;
begin
  vX := Abs(AMatrix.EX.X + AMatrix.EY.X + AMatrix.EZ.X);
  vY := Abs(AMatrix.EX.Y + AMatrix.EY.Y + AMatrix.EZ.Y);
  vZ := Abs(AMatrix.EX.Z + AMatrix.EY.Z + AMatrix.EZ.Z);
  if vY > vX then
    vX := vY;
  if vZ > vX then
    vX := vZ;
  if vX = 0 then
    Result := Abs(ANumberPixels)
  else
    Result := Abs(ANumberPixels / vX);
end;

function GetLanguageId(const AStr: string): TsgLangugeID;
var
  I: Integer;
begin
  Result := lgAny;
  if LanguageIds = nil then
  begin
    LanguageIds := TsgStringList.Create;
    TsgStringList(LanguageIds).CaseSensitive := False;
    LanguageIds.AddObject('Russian', TsgObjectWithField.CreateInt(Ord(lgRussian)));
{$IFDEF SG_FIREMONKEY}
    LanguageIds.AddObject('English', TsgObjectWithField.CreateInt(Ord(lgEnglish)));
    LanguageIds.AddObject('Deutsch', TsgObjectWithField.CreateInt(Ord(lgDeutsch)));
    LanguageIds.AddObject('French', TsgObjectWithField.CreateInt(Ord(lgFrench)));
    LanguageIds.AddObject('Estonian', TsgObjectWithField.CreateInt(Ord(lgEstonian)));
    LanguageIds.AddObject('Italiano', TsgObjectWithField.CreateInt(Ord(lgItaliano)));
    LanguageIds.AddObject('Nederlands', TsgObjectWithField.CreateInt(Ord(lgNederlands)));
    LanguageIds.AddObject('Brazilian Portuguese', TsgObjectWithField.CreateInt(Ord(lgBrazilian)));
{$ENDIF}
    LanguageIds.AddObject('Chinese', TsgObjectWithField.CreateInt(Ord(lgChina)));
    LanguageIds.AddObject('Japanese', TsgObjectWithField.CreateInt(Ord(lgJapanise)));
    LanguageIds.AddObject('Korean', TsgObjectWithField.CreateInt(Ord(lgKorean)));
  end;
  I := LanguageIds.IndexOf(AStr);
  if I > -1 then
    Result := TsgLangugeID(TsgObjectInt64(LanguageIds.Objects[I]).FieldInt);
end;

function GetTempDir: string;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vBuf: array[0..1023] of Char;
begin
  FillChar(vBuf, SizeOf(vBuf), #0);
  SetString(Result, vBuf, GetTempPath(Length(vBuf), vBuf));
  Result := MakeRightPath(Result);
end;
{$ELSE}
begin
{$IFDEF SG_FIREMONKEY}
  Result := TPath.GetTempPath;
  TDirectory.CreateDirectory(Result);
  Result := IncludeTrailingPathDelimiter(Result);
{$ELSE}
  Result := SysUtils.GetTempDir;
{$ENDIF}
end;
{$ENDIF}

function CreateFolder(AName: string): Boolean;
{$IFDEF SGFPC}
begin
  Result := SysUtils.CreateDir(AName);
end;
{$ELSE}
  {$IFDEF SG_FIREMONKEY}
  begin
    Result := True;
    try
      System.IOUtils.TDirectory.CreateDirectory(AName);
    except
      Result := False;
    end;
  {$ELSE}
  var
    Ch: PChar;
  begin
    Ch := PChar(AName);
    Result := CreateDirectory(Ch, nil);
  {$ENDIF}
end;
{$ENDIF}

function sgCreateDir(S: string): Boolean;
begin
  Result := True;
  if not DirectoryExists(S) then
    if not CreateFolder(S) then
      Result := False;
end;

function sgDirectoryExists(const ADir: string): Boolean;
begin
  if Trim(ADir) = '' then
    Result := True
  else
    Result := DirectoryExists(ADir);
end;

function IsNetworkDir(const ADir: string): Boolean;
begin
  Result := AnsiPos('\\', ADir) = 1;
end;

function GetLineByAxis(const AP1, AP2: TFPoint; const Width: Double; const AxisType: TsgAxisType): TsgLine;
var
  vSin, vCos: Extended;
  S, C: Double;
begin
  Result.Point1 := AP1;
  Result.Point2 := AP2;
  if AxisType <> atCenter then
  begin
    SinCos(GetAngleByPoints(Result.Point1, Result.Point2, True), vSin, vCos);
    C := Width * 0.5 * vSin;
    S := Width * 0.5 * (-vCos);
    if AxisType = atLeft then
    begin
      C := -C;
      S := -S;
    end;
    Result.Point1.X := Result.Point1.X + C;
    Result.Point1.Y := Result.Point1.Y + S;
    Result.Point2.X := Result.Point2.X + C;
    Result.Point2.Y := Result.Point2.Y + S;
  end;
end;

function IsScaled(const AScale: TFPoint): Boolean;
begin
  Result := (AScale.X <> 1.0) or (AScale.Y <> 1.0) or (AScale.Z <> 1.0);
end;

function SetArcByAxis(const AP1, AP2, APM: TFPoint; const Width: Double; const AxisType: TsgAxisType;
  var APts: TsgPoints3; const AAccuracy: Double = fDoubleResolution): Boolean;
var
  vCenter: TFPoint;
  vRadius, vRadiusNew: Double;
begin
  Result := GetCircleParams(AP1, AP2, APM, vCenter, vRadius, AAccuracy);
  if Result then
  begin
    APts[0] := AP1;
    APts[1] := AP2;
    APts[2] := APM;
    if AxisType <> atCenter then
    begin
      if AxisType = atLeft then
        vRadiusNew := vRadius + Width * 0.5
      else
        vRadiusNew := vRadius - Width * 0.5;
      APts[0] := GetPointOnLine(vCenter, AP1, vRadiusNew);
      APts[1] := GetPointOnLine(vCenter, AP2, vRadiusNew);
      APts[2] := GetPointOnLine(vCenter, APM, vRadiusNew);
    end;
  end;
end;

function SetAxisByArc(const AP1, AP2, APM: TFPoint; const Width: Double; const AxisType: TsgAxisType;
  var APts: TsgPoints3; const AAccuracy: Double = fDoubleResolution): Boolean;
const
  cnstSwapAxis: array [atCenter..atRight] of TsgAxisType = (atCenter, atRight, atLeft);
begin
  Result := SetArcByAxis(AP1, AP2, APM, Width, cnstSwapAxis[AxisType], APts, AAccuracy);
end;

function GetAxisByLine(const AP1, AP2: TFPoint; const Width: Double; const AxisType: TsgAxisType): TsgLine;
const
  cnstSwapAxis: array [atCenter..atRight] of TsgAxisType = (atCenter, atRight, atLeft);
begin
  Result := GetLineByAxis(AP1, AP2, Width, cnstSwapAxis[AxisType]);
end;

function SetRatioAndMajorPoint(const ARadiusX, ARadiusY: Double;
  var ARatio: Double; var AMajorPoint: TFPoint): Boolean;
var
  vRatPoint1, vRatPoint2: TFPoint;
begin
  vRatPoint1 := MakeFPoint(ARadiusX, 0, 0);
  vRatPoint2 := MakeFPoint(0, ARadiusY, 0);
  Result := SetRatioAndMajorPointByPoints(cnstFPointZero, vRatPoint1, vRatPoint2,
    ARatio, AMajorPoint);
end;

function SetRatioAndMajorPointByPoints(const APoint, ARatPt1, ARatPt2: TFPoint;
  var ARatio: Double; var AMajorPoint: TFPoint): Boolean;
var
  vRadius1, vRadius2: Double;
begin
  vRadius1 := DistanceFPoint(ARatPt1, APoint);
  vRadius2 := DistanceFPoint(ARatPt2, APoint);
  Result := (vRadius1 > fDoubleResolution) and (vRadius2 > fDoubleResolution);
  if Result then
  begin
    if vRadius1 > vRadius2 then
    begin
      ARatio := vRadius2 / vRadius1;
      AMajorPoint := SubFPoint(ARatPt1, APoint);
    end
    else
    begin
      ARatio := vRadius1 / vRadius2;
      AMajorPoint := SubFPoint(ARatPt2, APoint);
    end;
  end;
end;  

function ProcCompareColorCAD(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpColorCAD(Value1, Value2);
end;

function ProcCompareFPointByX(const Value1, Value2: Pointer): Integer;
begin
  Result := sgSignZ(PFPoint(Value1)^.X - PFPoint(Value2)^.X);
end;

function ProcCompareFPointByY(const Value1, Value2: Pointer): Integer;
begin
  Result := sgSignZ(PFPoint(Value1)^.Y - PFPoint(Value2)^.Y);
end;

function ProcCompareFPoint(const Value1, Value2: TFPoint): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint(Value1, Value2);
end;

function ProcComparePFPoint(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpFPoint(Value1, Value2);
end;

function ProcCompareF2DPoint(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpF2DPoint(Value1, Value2);
end;

function ProcCompareF2DPointByX(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpF2DPointByX(Value1, Value2);
end;

function ProcCompareF2DPointByY(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpF2DPointByY(Value1, Value2);
end;

function ProcCompareF2DPointByYX(const Value1, Value2: Pointer): Integer;
begin
  Result := TsgPointerTypeComparer.CmpF2DPointByYX(Value1, Value2);
end;

//function ProcCompareSingle(const Value1, Value2: Pointer): Integer;
//begin
//  Result := sgSignZ(Single(Value1) - Single(Value2));
//end;

function ProcCompareDouble(const Value1, Value2: Double): Integer;
begin
  Result := TsgTypeComparer.CmpDouble(Value1, Value2);
end;

function ProcCompareSingle(const Value1, Value2: Single): Integer;
begin
  Result := TsgTypeComparer.CmpSingle(Value1, Value2);
end;

function sgCompareCardinal(const AValue1, AValue2: Cardinal): Integer;
begin
  Result := TsgTypeComparer.CmpCardinal(AValue1, AValue2);
end;

function sgCompareHandles(const AH1, AH2: UInt64): Integer;
begin
  Result := TsgTypeComparer.CmpUInt64(AH1, AH2);
end;

function sgCompareNativeUInts(const AValue1, AValue2: sgConsts.TsgNativeUInt): Integer;
begin
  Result := TsgTypeComparer.CmpNativUInt(AValue1, AValue2);
end;

function sgCompareObjects(const AValue1, AValue2: TObject): Integer;
begin
  Result := TsgTypeComparer.CmpPointer(AValue1, AValue2);
end;

function sgComparePointers(const AValue1, AValue2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpPointer(AValue1, AValue2);
end;

function sgCompareStr(const AValue1, AValue2: string): Integer;
begin
  Result := AnsiCmprStr(AValue1, AValue2, True);
end;

function sgCompareTxt(const AValue1, AValue2: string): Integer;
begin
  Result := AnsiCmprStr(AValue1, AValue2, False);
end;

function AnsiCmprStr(const AStr1, AStr2: string;
  const ACaseSensitivity: Boolean): Integer;
begin
  Result := TsgTypeComparer.CmpStr(AStr1, AStr2, ACaseSensitivity);
end;

procedure ApplyRangDouble(var AValue: Double;
  const AMin, AMax: Double);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  if AValue < AMin then
    AValue := AMin
  else
    if AValue > AMax then
       AValue := AMax;
end;

procedure QSortList(const AList: TList; const AProc: TsgProcCompare);
begin
  QSortListBounds(AList, AProc, 0, AList.Count - 1);
end;

procedure QSortArrayBounds(AArray: Pointer{PPointerArray}; const AProc: TsgProcCompare; L, R: Integer);
var
  K, N: Integer;
  PItem, PElem: Pointer;
  PList: sgLists.PPointerArray;
begin
  PList := sgLists.PPointerArray(AArray);
  repeat
    K := L;
    N := R;
    PItem := PList^[(L + R) shr 1];
    repeat
      while AProc(PList^[K], PItem) < 0 do
        Inc(K);
      while AProc(PList^[N], PItem) > 0 do
        Dec(N);
      if K <= N then
      begin
        PElem := PList^[K];
        PList^[K] := PList^[N];
        PList^[N] := PElem;
        Inc(K);
        Dec(N);
      end;
    until (K > N);
    if L < N then
      QSortArrayBounds(PList, AProc, L, N);
    L := K;
  until (K >= R);
end;

procedure QSortListBounds(const AList: TList; const AProc: TsgProcCompare; ALeftPos, ARightPos: Integer);
begin
  if (AList.Count < 1) or (ALeftPos > ARightPos) or
    not InRange(ALeftPos, 0, AList.Count - 1) or
    not InRange(ARightPos, 0, AList.Count - 1) then
    Exit;
  QSortArrayBounds(@AList.List{$IFDEF LIST_PTR}^{$ENDIF}[0], AProc, ALeftPos, ARightPos);
end;

function QFindArrayItem(AArray: Pointer{PPointerArray}; L, R: Integer;
  AItem: Pointer; const AProc: TsgProcCompare; var Index: Integer): Boolean;
var
  I, C: Integer;
begin
  Result := False;
  while L <= R do
  begin
    I := (L + R) shr 1;
    C := AProc(PPointerArray(AArray)^[I], AItem);
    if C < 0 then L := I + 1 else
    begin
      R := I - 1;
      if C = 0 then
      begin
        Result := True;
        {if Duplicates <> dupAccept then }L := I;
      end;
    end;
  end;
  Index := L;
end;

function QFindItem(const AArray; L, R: Integer; const AItem;
  const AProc: TsgCustomProcCompare; ElemSize: Integer; var Index: Integer): Boolean;
var
  I, C: Integer;
begin
  Result := False;
  while L <= R do
  begin
    I := (L + R) shr 1;
    C := AProc(PPointer(TsgNativeInt(@AArray) + I * ElemSize)^, AItem);
    if C < 0 then L := I + 1 else
    begin
      R := I - 1;
      if C = 0 then
      begin
        Result := True;
        {if Duplicates <> dupAccept then }L := I;
      end;
    end;
  end;
  Index := L;
end;

function CheckLinePnt(const AL: TsgLine; const AP: TFPoint): Boolean;
var
  vD1, vD2: Extended;
begin
  vD1 := DistanceFPoint2DSqr(AL.Point1, AP);
  vD2 := DistanceFPoint2DSqr(AL.Point2, AP);
  Result := (vD1 > cnstSqrMinDist) and (vD2 > cnstSqrMinDist);
end;

function CheckArcPnt(const AArc: TsgArcR; const AP: TFPoint): Boolean;
var
  vAngle, vD1, vD2: Double;
begin
  vAngle := GetAngleByPoints(AArc.Center, AP, False);
  vD1 := Abs(AArc.AngleS - vAngle);
  vD2 := Abs(AArc.AngleE - vAngle);
  Result := (vD1 > cnstMinDist) and (vD2 > cnstMinDist);
end;

function CorrectFPointOnZero(const AP: TFPoint; const AIs3D: Boolean = True): TFPoint;
begin
  Result := AP;
  if Result.X = 0 then
    Result.X := fExtendedResolution;
  if Result.Y = 0 then
    Result.Y := fExtendedResolution;
  if AIs3D and (Result.Z = 0) then
    Result.Z := fExtendedResolution;
end;

procedure SortBoundariesAsCurve(const ACurves: TList; const ABoundaries: TsgObjectList);

  procedure CrossingCurve(const AList: TList);
  var
    I, J, vCnt: Integer;
    vCurveBase, vCurve, vCurveNew: PsgCurveLnk;
    vCrosses: Boolean;
    vArc1, vArc2, vArc3: TsgArcR;
    vLine1, vLine2, vLine3: TsgLine;

    function AddNewCurve: PsgCurveLnk;
    begin
      Result := CreatePsgCurveLink(vCurveBase^.Flags);
      AList.Add(Result);
      Result^.Link := vCurveBase^.Link;
    end;

  begin
    repeat
      vCrosses := False;
      vCnt := AList.Count - 1;
      for I := 0 to vCnt do
      begin
        vCurveBase := AList[I];
        for J := 0 to vCnt do
        begin
          if (J <> I) then
          begin
            vCurve := AList[J];
            case (vCurveBase^.Flags and 1) + (vCurve^.Flags and 1) shl 1 of
              1://arc and line
                begin
                  case SplitArcWithLine(vCurveBase^.Arc, vCurve^.Line, vArc1, vArc2, vArc3) of
                    1:
                      begin
                        vCrosses := True;
                        vCurveBase^.Arc := vArc1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc2;
                      end;
                    2:
                      begin
                        vCrosses := True;
                        vCurveBase^.Arc := vArc1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc2;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc3;
                      end;
                  end;
                end;
              2://line and arc
                begin
                  case SplitLineWithArc(vCurveBase^.Line, vCurve^.Arc, vLine1, vLine2, vLine3) of
                    1:
                      begin
                        vCrosses := True;
                        vCurveBase^.Line := vLine1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Line := vLine2;
                      end;
                    2:
                      begin
                        vCrosses := True;
                        vCurveBase^.Line := vLine1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Line := vLine2;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Line := vLine3;
                      end;
                  end;
                end;
              3://arc and arc
                begin
                  case SplitArcWithArc(vCurveBase^.Arc, vCurve^.Arc, vArc1, vArc2, vArc3) of
                    1:
                      begin
                        vCrosses := True;
                        vCurveBase^.Arc := vArc1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc2;
                      end;
                    2:
                      begin
                        vCrosses := True;
                        vCurveBase^.Arc := vArc1;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc2;
                        vCurveNew := AddNewCurve;
                        vCurveNew^.Arc := vArc3;
                      end;                    
                  end;
                end;
            else//line ans line
              if SplitLineWithLine(vCurveBase^.Line, vCurve^.Line, vLine1, vLine2) = 1 then
              begin
                vCrosses := True;
                vCurveBase^.Line := vLine1;
                vCurveNew := AddNewCurve;
                vCurveNew^.Line := vLine2;
              end;
            end;
          end;
        end;
      end;
    until not vCrosses;
  end;

  function IndexOfPoint2D(const APoints: TsgObjectList; const AP: TFPoint): Integer;
  begin
    Result := 0;
    while (Result < APoints.Count) and (not IsEqualFPoints(TsgItemLnk(APoints[Result]).Point, AP, fAccuracy)) do
      Inc(Result);
    if Result >= APoints.Count then
      Result := -1;
  end;

  procedure InitPointsAsCurve(const AList: TList; const AItems: TsgObjectList);
  var
    I: Integer;
    vCurve: PsgCurveLnk;
    vP1, vP2: TFPoint;

    function GetPntCurve(const APoint: TFPoint; const APoints: TsgObjectList): Pointer;
    var
      vIndex: Integer;
    begin
      vIndex := IndexOfPoint2D(APoints, APoint);
      if vIndex > -1 then
        Result := TsgItemLnk(APoints[vIndex])
      else
      begin
        Result := TsgItemLnk.Create;
        APoints.Add(Result);
        TsgItemLnk(Result).Point := APoint;
      end;
    end;

  begin
    AItems.Capacity := AList.Count shl 1;
    for I := 0 to AList.Count - 1 do
    begin
      vCurve := ACurves[I];
      if vCurve^.Flags and 1 <> 0 then
      begin
        vP1 := GetPointOnCircle(vCurve^.Arc.Center, vCurve^.Arc.Radius, vCurve^.Arc.AngleS);
        vP2 := GetPointOnCircle(vCurve^.Arc.Center, vCurve^.Arc.Radius, vCurve^.Arc.AngleE);
      end
      else
      begin
        vP1 := vCurve^.Line.Point1;
        vP2 := vCurve^.Line.Point2;
      end;
      vCurve^.P1 := GetPntCurve(vP1, AItems);
      TsgItemLnk(vCurve^.P1).AddEdge(vCurve);
      if IsEqualFPoints2D(vP1, vP2) then
        vCurve^.P2 := vCurve^.P1
      else
      begin
        vCurve^.P2 := GetPntCurve(vP2, AItems);
        TsgItemLnk(vCurve^.P2).AddEdge(vCurve);
      end;
    end;
  end;

  procedure AnalizeCurves(const Curves: TList; const Items: TsgObjectList);
  var
    vItemsCur, vItemsSoed: TsgObjectList;
    vItemsJoin: TsgObjectList;
    I, vChkCnt: Integer;
  begin
    vItemsCur := TsgObjectList.Create;
    vItemsSoed := TsgObjectList.Create;
    vItemsJoin := TsgObjectList.Create;
    try
      vItemsCur.Sorted := True;
      vItemsSoed.Sorted := True;
      vItemsCur.AssignList(Items, loCopy);
      vItemsJoin.Capacity := Items.Count shl 1;
      vChkCnt := 0;
      while (vItemsCur.Count > 0) and (vItemsCur.Count <> vChkCnt) do
      begin
        vChkCnt := vItemsCur.Count;
{$IFDEF SG_CHKRECURSIVE}
        iItemsRecursive := 0;
{$ENDIF}
        vItemsSoed.Count := 0;
        AddItem(TsgItemLnk(vItemsCur[0]), vItemsCur, vItemsSoed);
{$IFDEF SG_CHKRECURSIVE}
        iItemsRecursive := 0;
{$ENDIF}
        vItemsJoin.Count := 0;
        I := 0;
        while (I < vItemsSoed.Count) and (not JoinItem(TsgItemLnk(vItemsSoed[I]), vItemsJoin, ABoundaries)) do
          Inc(I);
      end;
    finally
      vItemsJoin.Free;
      vItemsSoed.Free;
      vItemsCur.Free;
    end;
  end;

var
  vItems: TsgObjectList;
begin
  CrossingCurve(ACurves);
  vItems := TsgObjectList.Create;
  try
    vItems.Duplicates := dupIgnore;
    vItems.Sorted := True;
    InitPointsAsCurve(ACurves, vItems);
    AnalizeCurves(ACurves, vItems);
  finally
    FreePointerList(vItems, ptvObject);
  end;
end;

procedure SortBoundariesLists(const ALines, ABoundaries: TList;
  const AProc1, AProc2: TsgProcOfPointerGetPoint;
  const AProcCopyDataForward, AProcCopyDataBackward: TsgProcCopyData; const Clear: Boolean);
var
  vBoundary: TFPointList;
  I: Integer;

  procedure DelDataCur;
  begin
    if Clear then
      FreeMem(ALines[I]);
    ALines.Delete(I);
    I := 0;
  end;

begin
  while ALines.Count > 0 do
  begin
    vBoundary := TFPointList.Create;
    ABoundaries.Add(vBoundary);
    vBoundary.Capacity := ALines.Count;
    AProcCopyDataForward(ALines[0], vBoundary, True);
    I := 0;
    DelDataCur;
    while I < ALines.Count do
    begin
      if IsEqualFPoints2D(AProc1(ALines[I]), vBoundary.Last) then
      begin
        AProcCopyDataForward(ALines[I], vBoundary, True);
        DelDataCur;
      end
      else
      begin
        if IsEqualFPoints2D(AProc2(ALines[I]), vBoundary.Last) then
        begin
          AProcCopyDataBackward(ALines[I], vBoundary, True);
          DelDataCur;
        end
        else
          Inc(I);
      end;
    end;
    I := 0;
    while I < ALines.Count do
    begin
      if IsEqualFPoints2D(AProc1(ALines[I]), vBoundary.First) then
      begin
        AProcCopyDataForward(ALines[I], vBoundary, False);
        DelDataCur;
      end
      else
      begin
        if IsEqualFPoints2D(AProc2(ALines[I]), vBoundary.First) then
        begin
          AProcCopyDataBackward(ALines[I], vBoundary, False);
          DelDataCur;
        end
        else
          Inc(I);
      end;
    end;
  end;
end;

procedure SortPointsOnPoint(const APoint: TFPoint; const APoints: TFPointList);
var
  I, J: Integer;
  vDistance: TsgDoubleList;
begin
  if APoints.Count < 2 then
    Exit;
  vDistance := TsgDoubleList.Create;
  try
    vDistance.Count := APoints.Count;
    for I := 0 to APoints.Count - 1 do
      vDistance[I] := DistanceFPoint(APoint, APoints[I]);
    for I := 0 to APoints.Count - 2 do
    begin
      for J := I + 1 to APoints.Count - 1 do
      begin
        if vDistance[I] > vDistance[J] then
        begin
          vDistance.Exchange(I, J);
          APoints.Exchange(I, J);
        end;
      end;
    end;
  finally
    FreeAndNil(vDistance);
  end;
end;

procedure ClearBoundariesLists(const ABoundaries: TList);
var
  I: Integer;
  vList: TList;
begin
  I := 0;
  while I < ABoundaries.Count do
  begin
    vList := TList(ABoundaries[I]);
    if IsEqualFPoints(PFPoint(vList.First)^, PFPoint(vList.Last)^, fDoubleResolution) then
    begin
      FreeMem(vList.Last);
      vList.Count := vList.Count - 1;
      if vList.Count < 3 then
      begin
        FreeRecordList(vList);
        ABoundaries.Delete(I);
      end
      else
       Inc(I);
    end
    else
    begin
      FreeRecordList(vList);
      ABoundaries.Delete(I);
    end;
  end;
end;

function GetMinBoundary(const ABounds: TList; const APoint: PFPoint; var AAreaMin: Double): TList;
var
  I, vIndexMin: Integer;
  vCheckPoly: Boolean;
  vArea: Double;
begin
  Result := nil;
  vIndexMin := -1;
  AAreaMin := MaxDouble;
  vCheckPoly := APoint <> nil;
  for I := 0 to ABounds.Count - 1  do
  begin
    if vCheckPoly then
    begin
      if IsPointInPolyline(TList(ABounds[I]), APoint^, nil) then
        vArea := GetAreaOfList(TFPointList(ABounds[I]))
      else
        vArea := MaxDouble;
    end
    else
      vArea := GetAreaOfList(TFPointList(ABounds[I]));
    if vArea < AAreaMin then
    begin
      AAreaMin := vArea;
      vIndexMin := I;
    end;
  end;
  if vIndexMin >= 0 then
    Result := TList(ABounds[vIndexMin]);
end;

function GetMinBoundaryCustom(const ABounds: TsgObjectList; const APoint: PFPoint; var AAreaMin: Double): Integer;
var
  I: Integer;
  vCheckPoly: Boolean;
  vArea: Double;
  vObj: TObject;
begin
  Result := -1;
  AAreaMin := MaxDouble;
  vCheckPoly := APoint <> nil;
  vArea := MaxDouble;
  for I := 0 to ABounds.Count - 1  do
  begin
    vObj := TObject(ABounds[I]);
    if vCheckPoly then
    begin
      if vObj.InheritsFrom(TList) then
      begin
        if IsPointInPolyline(TList(vObj), APoint^, nil) then
          vArea := GetAreaOfList(TList(vObj), @GetPointOfPFPoint)
        else
          vArea := MaxDouble;
      end
      else
        if vObj.InheritsFrom(TFPointList) then
        begin
          if IsPointInPolyline(TFPointList(vObj), APoint^, nil) = 1 then
            vArea := GetAreaOfList(TFPointList(vObj))
          else
            vArea := MaxDouble;
        end;
    end
    else
      if vObj.InheritsFrom(TList) then
        vArea := GetAreaOfList(TList(vObj), @GetPointOfPFPoint)
      else
        if vObj.InheritsFrom(TFPointList) then
          vArea := GetAreaOfList(TFPointList(vObj));
    if vArea < AAreaMin then
    begin
      AAreaMin := vArea;
      Result := I;
    end;
  end;
end;


function GetClassEntName(AObject: TObject; PrefixLength: Integer = 6): string;
begin
  Result := UpperCase(AObject.ClassName);
  if Copy(Result, 1, 3) = 'TSG' then
    Delete(Result, 1, PrefixLength);
end;



function GetDeviceCap(const AType: Integer): Integer;
{$IFDEF SG_USE_DC}
var
  DC: HDC;
begin
  DC:= GetDC(0);
  try
    Result := GetDeviceCaps(DC, AType);
  finally
    ReleaseDC(0, DC);
  end;
end;
{$ELSE}
begin
  Result := 1;
end;
{$ENDIF}


function GetScreenMMPerPixel: Double;
{$IFDEF SG_FIREMONKEY}
begin
  Result := 25.4/System.Devices.TDeviceInfo.ThisDevice.PixelsPerInch;
end;
{$ELSE}
var
  vDC: HDC;
begin
  vDC := GetDC(0);
  try
    Result := GetDeviceCaps(vDC, HORZSIZE) / GetDeviceCaps(vDC, HORZRES);
  finally
    ReleaseDC(0, vDC);
  end;
end;
{$ENDIF}

{$IFDEF SG_FIREMONKEY}
function GetPixelsPerInch: Integer;
begin
  Result := TDeviceInfo.ThisDevice.PixelsPerInch;
end;
{$ENDIF}

function GetMMtoPixel: TF2DPoint;
{$IFDEF SG_FIREMONKEY}
begin
  Result.X := 25.4/System.Devices.TDeviceInfo.ThisDevice.PixelsPerInch;
  Result.Y := Result.X;
end;
{$ELSE}
var
  vDc: HDC;
begin
  vDc := GetDc(0);
  try
    Result.X := GetDeviceCaps(vDc, HORZSIZE) / GetDeviceCaps(vDc,HORZRES);
    Result.Y := GetDeviceCaps(vDc, VERTSIZE) / GetDeviceCaps(vDc,VERTRES);
  finally
    ReleaseDC(0, vDc);
  end;
end;
{$ENDIF}

function GetLogPixelsPerInch: TPoint;
{$IFDEF SG_FIREMONKEY}
begin
  Result.X := System.Devices.TDeviceInfo.ThisDevice.PixelsPerInch;
  Result.Y := System.Devices.TDeviceInfo.ThisDevice.PixelsPerInch;
end;
{$ELSE}
var
  vDC: HDC;
begin
  vDC := GetDC(0);
  try
    Result.X := GetDeviceCaps(vDC, LOGPIXELSX);
    Result.Y := GetDeviceCaps(vDC, LOGPIXELSY);
  finally
    ReleaseDC(0, vDC);
  end;
end;
{$ENDIF}

function GetScreenResolution: TPoint;
{$IFDEF SG_FIREMONKEY}
var
  ScreenService: IFMXScreenService;
  ScreenSize: TPoint;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
  begin
    ScreenSize := ScreenService.GetScreenSize.Round;
    Result := TPoint.Create(ScreenSize.X, ScreenSize.Y);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;
{$ELSE}
var
  vDC: HDC;
begin
  vDC := GetDC(0);
  try
    Result.X := GetDeviceCaps(vDC, HORZRES);
    Result.Y := GetDeviceCaps(vDC, VERTRES);
  finally
    ReleaseDC(0, vDC);
  end;
end;
{$ENDIF}

function GetScreenDpi: Integer;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vDC: HDC;
  vVertSize, vVertRes: Integer;
begin
  if iScreenDpi < 1 then
  begin
    vDC := GetDC(0);
    try
      try
        vVertSize := GetDeviceCaps(vDC, VERTSIZE);
        vVertRes := GetDeviceCaps(vDC, VERTRES);
        iScreenDpi := Ceil(254 * vVertSize / vVertRes);
      except
        iScreenDpi := cnstStdDPI;
      end;
    finally
      ReleaseDC(0, vDC);
    end;
  end;
  Result := iScreenDpi;
end;
{$ELSE}
begin
{$IFDEF SG_FIREMONKEY}
  Result := TDeviceInfo.ThisDevice.PixelsPerInch;
{$ELSE}
  Result := iScreenDpi;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF SG_DELPHI_VCL}
function GetMetafileScale(const AMetafile: TMetafile): Double;
var
  vMetafileDpi: Integer;
  EMFHeader: TEnhMetaHeader;
  vScreenResolution: TPoint;
begin
  Result := 1;
  vMetafileDpi := AMetafile.Inch;
  if vMetafileDpi = 0 then
  begin
{$IFDEF CLR}
    GetEnhMetaFileHeader(AMetafile.Handle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
    GetEnhMetaFileHeader(AMetafile.Handle, Sizeof(EMFHeader), @EMFHeader);
{$ENDIF}
    if EMFHeader.szlDevice.cy <> 0 then
    begin
      vScreenResolution := GetScreenResolution;
      if vScreenResolution.Y <> 0 then
        Result := EMFHeader.szlDevice.cy / vScreenResolution.Y;
    end;
  end
  else
    Result := vMetafileDpi / GetScreenDpi;
end;

function ResizeMetafileByDpi(var AMetafile: TMetafile): Boolean;
var
  vMetafileScale: Double;
begin
  Result := False;
  if Assigned(AMetafile) then
  begin
    vMetafileScale := GetMetafileScale(AMetafile);
    Result := ResizeMetafileByScale(AMetafile, vMetafileScale);
  end;
end;

function ResizeMetafileByScale(var AMetafile: TMetafile; const AScale: Double): Boolean;
var
  vMetafile: TMetafile;
  vMetafileCanvas: TMetafileCanvas;
  vWidth, vHeight: Integer;
begin
  Result := False;
  if (AScale <> 1) and (AScale <> 0) then
  begin
    vWidth := Ceil(AMetafile.Width * AScale);
    vHeight := Ceil(AMetafile.Height * AScale);
    vMetafile := TMetafile.Create;
    try
      vMetafile.MMWidth := AMetafile.MMWidth;
      vMetafile.MMHeight := AMetafile.MMHeight;
      vMetafileCanvas := TMetafileCanvas.Create(vMetafile, 0);
      try
        vMetafileCanvas.StretchDraw(Rect(0, 0, vWidth, vHeight), AMetafile);
      finally
        vMetafileCanvas.Free;
      end;
      Result := True;
    finally
      FreeAndNil(AMetafile);
      AMetafile := vMetafile;
    end;
  end;
end;
{$ENDIF}

function OpenThread(dwDesiredAccess: Cardinal; bInheritHandle: LongBool; dwThreadId: Cardinal): THandle;
begin
  Result := 0;
{$IFNDEF SGFPC}
  if @_OpenThread = nil then
    @_OpenThread := GetProcAddress(GetModuleHandle('kernel32'), 'OpenThread');
{$ENDIF}
  if @_OpenThread <> nil then
    Result := _OpenThread(dwDesiredAccess, bInheritHandle, dwThreadId);
end;

function NeedFixForWine: Boolean;
begin
  Result := sgConsts.IsWine;
end;

function GetVersionWindowsName(AVer: TsgVersionWindows): string;
const
  cnstVersionNames: array[TsgVersionWindows] of string =
  ('undefined', 'WinXP', 'WinVista', 'Windows7',
    'Windows8', 'Windows8_1', 'Windows 10', 'Windows 11',
    'Windows 2000', 'WinServer2003', 'WinServer2003R2', 'WinServer2008',
    'WinServer2008R2', 'WinServer2012', 'WinServer2012R2',
    'WinServer2016', 'WinServer2019', 'WinServer2022');
begin
  Result := cnstVersionNames[AVer];
end;

{$IFDEF MSWINDOWS}
function GetVersionWindowsInternal: TsgVersionWindows;
{$IFDEF SGDEL_XE2}
var
  VerInfo: TOSVersionInfoEx;

  function IsWindowsServer: Boolean;
  {$IFDEF SGDEL_XE8}
  var
    osvi: TOSVersionInfoEx;
    dwlConditionMask: ULONGLONG;
  {$ENDIF}
  begin
  {$IFDEF SGDEL_XE8}
    FillChar(osvi, SizeOf(TOSVersionInfoEX), 0);
    osvi.wProductType := VER_NT_WORKSTATION;
    dwlConditionMask := VerSetConditionMask(0, VER_PRODUCT_TYPE, VER_EQUAL);
    Result := VerifyVersionInfo(osvi, VER_PRODUCT_TYPE, dwlConditionMask) = FALSE;
  {$ELSE}
    Result := not VerInfo.wProductType = VER_NT_WORKSTATION;
  {$ENDIF}
  end;
var
  SysInfo: TSystemInfo;
{$IFDEF SGDEL_10_SEATTLE}
  MajorNum, MinorNum, BuildNum: DWORD;
{$ENDIF}
  vBuild: Integer;
  vMajor: Integer;
  vMinor: Integer;
begin
  Result := vrUndefined;
  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx(VerInfo);

  vMajor := VerInfo.dwMajorVersion;
  vMinor := VerInfo.dwMinorVersion;
  vBuild := VerInfo.dwBuildNumber;

  ZeroMemory(@SysInfo, SizeOf(SysInfo));
{$IFDEF SGDEL_10_SEATTLE}
  if (vMajor > 6) or ((vMajor = 6) and  (vMinor > 1)) then
  begin
    if GetProductVersion(kernelbase, MajorNum, MinorNum, BuildNum) then
    begin
      vMajor := MajorNum;
      vMinor := MinorNum;
      if not ((vMajor >= 10) and (vBuild > Integer(BuildNum))) then
        vBuild := BuildNum;
    end;
//    else
//    if GetNetWkstaMajorMinor(MajorNum, MinorNum) then
//    begin
//      FMajor := MajorNum;
//      FMinor := MinorNum;
//    end;
  end;
{$ENDIF}

  case vMajor of
    10: case vMinor of
          0: if IsWindowsServer then
             begin
               if vBuild >= 20348 then
                 Result := vrWinServer2022
               else if vBuild >= 17763 then
                 Result := vrWinServer2019
               else
                 Result := vrWinServer2016
             end
             else
             begin
               if vBuild >= 22000 then
                 Result := vrWin11
               else
                 Result := vrWin10;
             end;
        end;
    6:  case vMinor of
          0: if VerInfo.wProductType = VER_NT_WORKSTATION then
               Result := vrWinVista
             else
               Result := vrWinServer2008;
          1: if VerInfo.wProductType = VER_NT_WORKSTATION then
              Result := vrWin7
             else
               Result := vrWinServer2008R2;
          2: if VerInfo.wProductType = VER_NT_WORKSTATION then
               Result := vrWin8
             else
               Result := vrWinServer2012;
          3: if not IsWindowsServer then
               Result := vrWin8_1
             else
               Result := vrWinServer2012R2;
        end;
    5:  case vMinor of
          0: Result := vrWin2000;
          1: Result := vrWinXP;
          2:
            begin
              if (VerInfo.wProductType = VER_NT_WORKSTATION) and
                 (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                Result := vrWinXP
              else
              begin
                if GetSystemMetrics(SM_SERVERR2) = 0 then
                  Result := vrWinServer2003
                else
                  Result := vrWinServer2003R2
              end;
            end;
        end;
  end;
{$ELSE}
begin
  Result := vrUndefined;
{$ENDIF}
end;
{$ENDIF}

function GetVersionWindows: TsgVersionWindows;
const
  cnstVersionWindows: TsgVersionWindows = vrUndefined;
begin
{$IFDEF MSWINDOWS}
  if cnstVersionWindows = vrUndefined then
    cnstVersionWindows := GetVersionWindowsInternal;
{$ENDIF}
  Result := cnstVersionWindows;
end;
{$IFNDEF SGDEL_2009}
{ system functions }
function MonitorEnter(const AObject: TObject; Timeout: Cardinal = INFINITE): Boolean;
begin
  Result := True;
{$IFDEF SG_OPENING_IN_THEADS}
  GlobalCriticalSection.Enter;
{$ENDIF}
end;

procedure MonitorExit(const AObject: TObject);
begin
{$IFDEF SG_OPENING_IN_THEADS}
  GlobalCriticalSection.Leave;
{$ENDIF}
end;

function MonitorTryEnter(AObject: TObject): Boolean;
begin
{$IFDEF SG_OPENING_IN_THEADS}
  Result := GlobalCriticalSection.TryEnter;
{$ELSE}
  Result := False;
{$ENDIF}
end;
{$ENDIF}

function IndexOfStrings(const AValue: string; const AStrings: array of string;
  ProcCompare: TsgProcCompareStrings = nil): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not Assigned(ProcCompare) then
    ProcCompare := @sgCompareStr;
  for I := Low(AStrings) to High(AStrings) do
  begin
    if ProcCompare(AValue, AStrings[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function IsHasSubstring(const AValue, AData: string): Integer;
begin
  Result := Integer(not (AnsiPos(AData, AValue) > 0));
end;

function IsMatch(const AFilter, AIgnoreFilter, AName: string): Boolean;
{$IFDEF SGDEL_XE}
var
  vOptions: TRegExOptions;
begin
  vOptions := [roIgnoreCase];
  if AIgnoreFilter = '' then
    Result := True
  else
    Result := (not TRegEx.IsMatch(AName, AIgnoreFilter, vOptions));
  if Result then
    Result := TRegEx.IsMatch(AName, AFilter, vOptions);
{$ELSE}
  {$IFDEF SG_RegExpr}
var
  vRegEx: TRegExpr;
begin
  vRegEx := TRegExpr.Create;
  try
    if AIgnoreFilter = '' then
      Result := True
    else
    begin
      vRegEx.Expression := AIgnoreFilter;
      Result := not vRegEx.Exec(AName);
    end;
    if Result then
    begin
      vRegEx.Expression := AFilter;
      Result := vRegEx.Exec(AName);
    end;
  finally
    vRegEx.Free;
  end;
  {$ELSE}
  begin
    Result := False;
  {$ENDIF}
{$ENDIF}
end;


{TdwgSortHandleList}

type
  PsgInlineMapElement = ^TsgInlineMapElement;
  TsgInlineMapElement = packed record
    Size: SmallInt;
    Loaded: SmallInt;
    Element: TdwgMapElementList;
    Name: string;
  end;

constructor TsgObjectsMap.Create;
begin
  inherited Create;
  FMap := TsgMapList.Create;
  FMap.Duplicates := dupAccept;
//  FMap.Sorted := True;
  FMap.Capacity := 1024;
  FObjectsLoaded := 0;
end;

destructor TsgObjectsMap.Destroy;
begin
  Clear;
  FMap.Free;
  inherited Destroy;
end;

procedure TsgObjectsMap.Clear;
var
  P: PsgInlineMapElement;
  I: Integer;
begin
  for I := FMap.Count - 1 downto 0 do
  begin
    P := PsgInlineMapElement(TsgNativeInt(FMap[I]) - 2 * SizeOf(SmallInt));
{$IFDEF AUTOREFCOUNT}
    P^.Element.Obj := nil;
{$ENDIF}
    if P^.Size > SizeOf(TdwgMapElementList) then
      P^.Name := '';
    FreeMem(P);
  end;
  FMap.Clear;
  FObjectsLoaded := 0;
end;

function TsgObjectsMap.GetCount : integer;
begin
  Result := FMap.Count - 1;
end;

function TsgObjectsMap.GetHasName(Index: Integer): Boolean;
begin
  Result := PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Size > SizeOf(TdwgMapElementList);
end;

function TsgObjectsMap.GetObjectsLoaded: Integer;
begin
  Result := FObjectsLoaded;
end;

function TsgObjectsMap.IndexOfHandle(const AHandle: UInt64): Integer;
var
  vElem: TdwgMapElementList;
begin
  vElem.Handle := AHandle;
  vElem.Location := 0;
  vElem.Obj := nil;
  Result := FMap.IndexOf(@vElem);
end;

procedure TsgObjectsMap.Sort;
begin
  if not FMap.Sorted then
    FMap.Sorted := True;
end;

procedure TsgObjectsMap.SetName(Index: Integer; const Value: string);
var
  vElem, vSrc: PsgInlineMapElement;
begin
  if not HasName[Index] then
  begin
    GetMem(vElem, SizeOf(TsgInlineMapElement));
    vSrc := PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt));
    System.Move(vSrc^.Loaded, vElem^.Loaded, SizeOf(TsgInlineMapElement) - SizeOf(Pointer) - SizeOf(SmallInt));
    vElem^.Size := SizeOf(TdwgMapElementList) + SizeOf(Pointer);
    Pointer(vElem^.Name) := nil;
    PPointer(FMap.GetItemBase(Index))^ := @vElem^.Element;
    FreeMem(vSrc);
  end;
  PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Name := Value;
end;

procedure TsgObjectsMap.SetObject(AIndex: Integer;
  const Value: TObject);
var
  vElem: PdwgMapElementList;
begin
  vElem := FMap[AIndex];
  if not ObjectLoaded[AIndex] then
  begin
    ObjectLoaded[AIndex] := True;
    Inc(FObjectsLoaded);
  end
  else
    ObjectLoaded[AIndex] := True; //increases ObjectLoadedCount
  vElem^.Obj := Value;
end;

procedure TsgObjectsMap.SetObjectLoaded(const Index: Integer; const Value: Boolean);
begin
  if Value then
    Inc(PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Loaded)
  else
    PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Loaded := -1;
end;

procedure TsgObjectsMap.SetSorted(const Value: Boolean);
begin
  if Value then
    FMap.Flags := FMap.Flags or cnstSortedBit
  else
    FMap.Flags := FMap.Flags and not cnstSortedBit
end;

function TsgObjectsMap.GetMapElement(AIndex: Integer): TdwgMapElementList;
begin
  Result := GetPMapElement(AIndex)^;
end;

function TsgObjectsMap.GetName(Index: Integer): string;
begin
  if HasName[Index] then
    Result := PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Name
  else
    Result := '';
end;

function TsgObjectsMap.GetPMapElement(Index: Integer): PdwgMapElementList;
begin
  Result := @PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Element;
end;

function TsgObjectsMap.GetSorted: Boolean;
begin
  Result := FMap.Sorted;
end;

function TsgObjectsMap.GetObjectLoaded(const Index: Integer): Boolean;
begin
  Result := PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Loaded <> -1;
end;

function TsgObjectsMap.GetObjectLoadedCount(const Index: Integer): Integer;
begin
  Result := PsgInlineMapElement(TsgNativeInt(FMap[Index]) - 2 * SizeOf(SmallInt))^.Loaded;
  if Result < 0 then
    Result := 0;
end;

procedure TsgObjectsMap.AddDwgElement(const AHandle: UInt64; const ALocation: Integer;
  const AObj: Tobject = nil);
var
  vElem: PsgInlineMapElement;
begin
  GetMem(vElem, SizeOf(TsgInlineMapElement) - SizeOf(Pointer));
  FillChar(vElem^, SizeOf(TsgInlineMapElement) - SizeOf(Pointer), 0);
  vElem^.Size := SizeOf(TdwgMapElementList);
  vElem^.Loaded := -1;
  vElem^.Element.Handle := AHandle;
  vElem^.Element.Location := ALocation;
  vElem^.Element.Obj := AObj;
  FMap.Add(@vElem^.Element);
end;

 { TsgGeneratorShapeEdge }

 (* TsgGeneratorShapeEdge.CalculateBSplineBasis

   Basis functions evaluated are returned in increasing order
        N - Curve degree
        ALeftKnotIndex - Index of left knot closest to UU
	      UU - The parameter value to be calculated at                      *)
procedure TsgGeneratorShapeEdge.CalculateBSplineBasis(const AKnots: TsgDoubleList;
  N, ALeftKnotIndex: Integer; const UU: Double; var ABasisValues: array of Double);
const
  cnstMaximimDegree = 9;
var
	I, J: Integer;
	U: array [0..cnstMaximimDegree, 0..cnstMaximimDegree] of Double;
	T: array [0..2*cnstMaximimDegree] of Double;

  function GetU(const A, B, Index, Sgn: Integer): Double;
  begin
    if T[N-Index+B] - T[N+B] <> 0 then
      Result := U[Index-1][B] * Sgn * (UU - T[N-A+B]) / (T[N-Index+B] - T[N+B])
    else
      Result := 0;
  end;
begin
  J := ALeftKnotIndex - N + 1;
  for I := 0 to 2*N-1 do
  begin
    T[I] := AKnots[J];
    Inc(J);
  end;
  FillChar(U, SizeOf(U), 0);
	U[0][0] := 1.0;
  for I := 1 to N do
  begin
    //    U[I][0] := U[I-1][0] * ((UU-T[N]) / (T[N-I] - T[N]));
    //    for J := 1 to I - 1 do
    //      U[I][J] := U[I-1][J-1] * ((T[N-1-I+J] - UU) / (T[N-1-I+J] - T[N-1+J])) + U[I-1][J] * ((UU - T[N+J]) / (T[N-I+J] - T[N+J]));
    //    U[I][I] := U[I-1][I-1] * ((T[N-1] - UU) / (T[N-1] - T[N-1+I]));
    U[I][0] := GetU(0, 0, I, 1);
    for J := 1 to I - 1 do
      U[I][J] := GetU(I, J-1, I, -1) + GetU(0, J, I, 1);
    U[I][I] := GetU(I, I-1, I, -1);
  end;
  for I := 0 to N do
    ABasisValues[I] := U[N][I];// other U-values for future versions
end;

function  TsgGeneratorShapeEdge.CalculateBSplinePoints(const AControls: IsgArrayFPoint;
  const AKnots, AWeights: TsgDoubleList; const ADegree: Integer; const Param: Double;
  var AValue: TFPoint): Boolean;
const
  cnstCoords = 3;// X, Y and Z - points dimension (for SPLINE)

  function GetLeftKnotIndex(const AKnot: Double): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AKnots.Count - 1 downto 1 do
      if (AKnots[I] < AKnots[I + 1]) and (AKnots[I] <= AKnot)
        and (AKnot <= AKnots[I + 1]) then
      begin
        Result := I;// Knot[I] <= AKnot <= Knot[I+1]
        Exit;
      end;
  end;

  function CheckWeights(const AIndex: Integer): Boolean;
  begin
    Result := (AWeights <> nil) and (AIndex >= 0) and (AIndex < AWeights.Count);
  end;

var
  J, K, vIndex, vLeftIndex: Integer;
  vAlignedControls: array of TFPoint;
  Values: array of Double;
  Sum: Double;
begin
  Result := True;
  vLeftIndex := GetLeftKnotIndex(Param);
  if vLeftIndex < ADegree then
    vLeftIndex := ADegree;

  SetLength(vAlignedControls, ADegree + 1);
  for J := 0 to ADegree do
  begin
    vIndex := vLeftIndex - ADegree + J;
    if vIndex >= AControls.FPointCount then
    begin
      Result := False;
      Break;
    end;
    vAlignedControls[J] := AControls.FPoints[vIndex];
    if CheckWeights(vIndex) then
      for K := 0 to cnstCoords-1 do
        vAlignedControls[J].V[K] := vAlignedControls[J].V[K] * AWeights[vIndex];
  end;

  SetLength(Values, ADegree + 1);
  CalculateBSplineBasis(AKnots, ADegree, vLeftIndex, Param, Values);

  for K := 0 to cnstCoords-1 do
    AValue.V[K] := Values[0]* vAlignedControls[0].V[K];

  for J := 1 to ADegree do
    for K := 0 to cnstCoords-1 do
      AValue.V[K] := AValue.V[K]+ Values[J]* vAlignedControls[J].V[K];

  if Assigned(AWeights) then//and (vLeftIndex <= 2)
  begin
    K := vLeftIndex - ADegree;
    Sum := 0;
    for J := 0 to ADegree do
    begin
      if CheckWeights(K) then
        Sum := Sum + (Values[J] * AWeights[K]);
      Inc(K);
    end;
    if not sgIsZero(Sum) then
    begin
      for J := 0 to cnstCoords-1 do
        AValue.V[J] := AValue.V[J] / Sum;
    end;
  end;
end;

function TsgGeneratorShapeEdge.Normalize(const AOffsetIndex, AKnotIndex: Integer;
  const AParam: Double): Double;
var
  vDelta1, vDelta2, vValue1, vValue2: Double;
begin
  if AOffsetIndex = 0 then
  begin
    if (cnstKnotsCubicSpline[AKnotIndex] <= AParam) and
     (AParam < cnstKnotsCubicSpline[AKnotIndex + 1]) then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    vDelta1 := cnstKnotsCubicSpline[AKnotIndex + AOffsetIndex] -
      cnstKnotsCubicSpline[AKnotIndex];
    vValue1 := (AParam - cnstKnotsCubicSpline[AKnotIndex]) *
      Normalize(AOffsetIndex - 1, AKnotIndex, AParam);
    if vDelta1 = 0 then
      vValue1 := 0
    else
      vValue1 := vValue1 / vDelta1;
    vDelta2 := cnstKnotsCubicSpline[AKnotIndex + AOffsetIndex + 1] -
      cnstKnotsCubicSpline[AKnotIndex + 1];
    vValue2 := (cnstKnotsCubicSpline[AKnotIndex + AOffsetIndex + 1] - AParam) *
      Normalize(AOffsetIndex - 1, AKnotIndex + 1, AParam);
    if vDelta2 = 0 then
      vValue2 := 0
    else
      vValue2 := vValue2 / vDelta2;
    Result := vValue1 + vValue2;
  end;
end;

procedure TsgGeneratorShapeEdge.SetIsImportMode(AValue: Boolean);
begin
  FIsImportMode := AValue;
end;

procedure TsgGeneratorShapeEdge.SetNumberCirclePart(const AValue: Cardinal);
begin
  FNumberCirclePart := GetNumberPart(AValue);
end;

procedure TsgGeneratorShapeEdge.SetNumberSplinePart(const AValue: Cardinal);
begin
  FNumberSplinePart := GetNumberPart(AValue);
end;

procedure TsgGeneratorShapeEdge.SetQualityApproximation(const AValue: Cardinal);
begin
  FQualityApproximation := AValue;
  if FQualityApproximation > 6 then
    FQualityApproximation := 6;
  SetQualityParams;
end;

procedure TsgGeneratorShapeEdge.AddPoint(const APoint: TFPoint);
begin
  Lock;
  try
    case FListType of
      ltList:
        begin
          if Assigned(FCreatePoint) then
            TList(FList).Add(FCreatePoint(APoint.X, APoint.Y, APoint.Z))
          else
            FAddPoint(TList(FList), APoint);
        end;
      ltObjectList:
        begin
          if Assigned(FCreatePoint) then
            TsgObjectList(FList).Add(FCreatePoint(APoint.X, APoint.Y, APoint.Z))
          else
            FAddPoint(TsgObjectList(FList), APoint);
        end;
      ltFPoint:    TFPointList(FList).Add(APoint);
      ltF2DPoint:  TF2DPointList(FList).Add(APoint.Point2D);
      ltDouble:    TsgDoubleList(FList).AppendArray(APoint.V);
      ltPointF:    TsgPointFList(FList).Add(MakePointF(APoint.X, APoint.Y));
    else
      FAddPoint(FList, APoint);
    end;
  finally
    Unlock;
  end;
end;

procedure TsgGeneratorShapeEdge.CreateSplineByKnots(
  AControls: IsgArrayFPoint; const AKnots: TsgDoubleList;
  const AIncT: Double);

  function IsOverflow(AValue: Double): Boolean;
  begin
    AValue := Abs(AValue);
    Result := (AValue > 0) and (Log10(AValue) > 7);
  end;

  function _Normalize(const N, I: Integer; Param: Double; var AError: Boolean): Double;
  var
    V1, D1, V2, D2: Double;
  begin
    if N = 0 then
    begin
      if (AKnots[I] <= Param) and (Param < AKnots[I + 1]) then
        Result := 1
      else
        Result := 0;
    end
    else
    begin
      D1 := AKnots[I + N] - AKnots[I];
      V1 := (Param - AKnots[I]) * _Normalize(N - 1, I, Param, AError);
      if D1 = 0 then
        V1 := 0
      else
      begin
        V1 := V1 / D1;
        AError := IsOverflow(V1);
      end;
      D2 := AKnots[I + N + 1] - AKnots[I + 1];
      V2 := (AKnots[I + N + 1] - Param) * _Normalize(N - 1, I + 1, Param, AError);
      if D2 = 0 then
        V2 := 0
      else
      begin
        V2 := V2 / D2;
        AError := IsOverflow(V2);
      end;
      Result := V1 + V2;
    end;
  end;

  function GetNURBS(Index: Integer; Param: Double): TFPoint;
  var
    I: Integer;
    Ni: Double;
    vError: Boolean;
  begin
    Result := cnstFPointZero;
    I := Index - 3;
    while I <= Index do
    begin
      vError := False;
      Ni := _Normalize(3, I, Param, vError);
      if not vError then
        Result := AddFPoint(Result, PtXScalar(AControls.FPoints[I], Ni))
      else
      begin
        Result := AControls.FPoints[I];
        I := Index;
      end;
      Inc(I);
    end;
  end;

var
  IncTNorm: Double;
  T: Double;
  I, vControlCount: Integer;
begin
  Lock;
  try
    vControlCount := AControls.FPointCount;
    while AKnots.Count - vControlCount < 4 do
      AKnots.Add(AKnots.Last);
    AddPoint(AControls.FPoints[0]);
    for I := 3 to vControlCount - 1 do
    begin
      T := AKnots[I];
      IncTNorm := AIncT * Abs(AKnots[I + 1] - AKnots[I]);
      if IncTNorm > fDoubleResolution then//fix overflow
      begin
        while T < AKnots[I + 1] do
        begin
          AddPoint(GetNURBS(I, T));
          T := T + IncTNorm;
        end;
      end;
    end;
    AddPoint(AControls.FPoints[AControls.FPointCount - 1]);
  finally
    Unlock;
  end;
end;

function TsgGeneratorShapeEdge.CheckParams: Boolean;
begin
  Result := (FList <> nil);
  if Result then
  begin
    case FListType of
      ltFPoint, ltF2DPoint, ltDouble, ltPointF:  begin end;
      ltList, ltObjectList:  Result := Assigned(FAddPoint) or Assigned(FCreatePoint);
    else
      Result := False;
    end;
  end;
end;

procedure TsgGeneratorShapeEdge.GenerateBSpline(AControls: IsgArrayFPoint;
  const AKnots, AWeights: TsgDoubleList; const ADegree: Integer);
var
  I, J, Cnt: Integer;
  vStartKnot, vStep: Double;
  vPoint: TFPoint;
  vAddLastControl: Boolean;
begin
  Lock;
  try
    vAddLastControl := False;
    Cnt := AKnots.Count - 2;
    for I := 0 to Cnt do
    begin
      vStartKnot := AKnots[I];
      vStep := (AKnots[I+1] - vStartKnot) / NumberSplinePart;
      if vStep = 0 then
        Continue;
      for J := 0 to NumberSplinePart do
      begin
        if CalculateBSplinePoints(AControls, AKnots, AWeights, ADegree, vStartKnot + J*vStep, vPoint) then
          AddPoint(vPoint)
        else
          vAddLastControl := True;
      end;
    end;
    if vAddLastControl and (AControls.FPointCount > 1) then
      AddPoint(AControls.FPoints[AControls.FPointCount - 1]);
  finally
    Unlock;
  end;
end;

procedure TsgGeneratorShapeEdge.GenerateEllipticCurve(const AX, AY, AZ, ARadiusX,
  ARadiusY, AStart, AEnd, AAxisSin, AAxisCos: Extended; AReverse: Boolean = False);
var
  I,vCnt: Integer;
  vAngleStart, vAngleEnd: Extended;
  vDelta: Double;

  procedure CalcAndAddPoint(AAngle: Extended);
  var
    vPoint: TFPoint;
    vSin, vCos, vX, vY: Extended;
  begin
    SinCos(AAngle, vSin, vCos);
    vX := ARadiusX * vCos;
    vY := ARadiusY * vSin;
    vPoint.Z := AZ;
    vPoint.X := AX + vX * AAxisCos - vY * AAxisSin;
    vPoint.Y := AY + vX * AAxisSin + vY * AAxisCos;
    AddPoint(vPoint);
  end;

begin
  Lock;
  try
    SetStartEndAngles(AStart, AEnd, vAngleStart, vAngleEnd);
    vCnt := GetNumberArcPart(FNumberCirclePart, AStart, AEnd, IsImportMode, vDelta);
    if AReverse then
    begin
      SwapExtendeds(vAngleStart, vAngleEnd);
      vDelta := -vDelta;
    end;
    CalcAndAddPoint(vAngleStart);
    for I := 1 to vCnt - 1 do
      CalcAndAddPoint(vAngleStart + I * vDelta);
    CalcAndAddPoint(vAngleEnd);
  finally
    Unlock;
  end;
end;

procedure TsgGeneratorShapeEdge.GenerateQuadraticSpline(const APoint1, APoint2,
  APoint3: TFPoint; const ALevel: Byte);
var
  vPoint12, vPoint23, vPoint123, vSubPoint31, vSubPoint23: TFPoint;
  vTmpPoint: TFPoint;
  vTmpValue: Extended;
  vCanAddPoint: Boolean;
begin
  Lock;
  try
    vPoint12 := MiddleFPoint2D(APoint1, APoint2);
    vPoint23 := MiddleFPoint2D(APoint2, APoint3);
    vPoint123 := MiddleFPoint2D(vPoint12, vPoint23);
    vSubPoint31 := SubFPoint2D(APoint3, APoint1);
    vSubPoint23 := SubFPoint2D(APoint2, APoint3);
    vTmpValue := Abs(DenomFPoint2D(vSubPoint23, vSubPoint31));
    if vTmpValue > 1e-30 then
      begin
        vCanAddPoint := Sqr(vTmpValue) <= (FDistanceToleranceSquare *
          (Sqr(vSubPoint31.X) + Sqr(vSubPoint31.Y)));
      end
    else
      begin
        vTmpPoint := AddFPoint2D(SubFPoint2D(APoint2, APoint1), vSubPoint23);
        vCanAddPoint := Abs(vTmpPoint.X) + Abs(vTmpPoint.Y) <=
          FDistanceToleranceManhattan;
      end;
    if vCanAddPoint then
      AddPoint(vPoint123)
    else
      begin
        if ALevel + 1 <= FRecursionLimit then
          begin
            GenerateQuadraticSpline(APoint1, vPoint12, vPoint123, ALevel + 1);
            GenerateQuadraticSpline(vPoint123, vPoint23, APoint3, ALevel + 1);
          end;
      end;
  finally
    Unlock;
  end;
end;

function TsgGeneratorShapeEdge.GetNumberPart(const AValue: Cardinal): Byte;
begin
  if AValue > MAXBYTE then
    Result := MAXBYTE
  else
    if AValue < 3 then
      Result := 3
    else
      Result := AValue;
end;

procedure TsgGeneratorShapeEdge.SetQualityParams;
const
  cnstApproximationScales: array [0..6] of Double = (1,
    0.000001, 0.005, 0.1, 1, 5, 10);
var
  vApproximationScale: Double;
begin
  FRecursionLimit := 16;
  vApproximationScale := cnstApproximationScales[FQualityApproximation];
  FDistanceToleranceSquare := 0.5 / Sqr(vApproximationScale);
  FDistanceToleranceManhattan := 4.0 / vApproximationScale;
end;

class procedure TsgGeneratorShapeEdge.SetStartEndAngles(const AStartAngle,
  AEndAngle: Double; var AStart, AEnd: Extended);
begin
  AStart := AStartAngle;
  AEnd := AEndAngle;
  if AStartAngle > AEndAngle then
    AStart := AStart - Round(AStartAngle / 360) * 360;
  if AEnd < AStart then
    AEnd := AEnd + 360;
  AStart := AStart * fPiDividedBy180;
  AEnd := AEnd * fPiDividedBy180;
end;

constructor TsgGeneratorShapeEdge.Create;
begin
  inherited Create;
  InitializeCriticalSectionInt;
  NumberCirclePart := iDefaultNumberOfCircleParts;
  NumberSplinePart := iDefaultNumberOfSplineParts;
  QualityApproximation := 0;
end;

procedure TsgGeneratorShapeEdge.DeleteCriticalSectionInt;
begin
{$IFDEF SG_FIREMONKEY}
  FreeAndNil(FLock);
{$ELSE}
  {$IFNDEF SGFPC}
  DeleteCriticalSection(FLock);
  {$ELSE}
  System.DoneCriticalsection(FLock);
  {$ENDIF}
{$ENDIF}
end;

destructor TsgGeneratorShapeEdge.Destroy;
begin
  DeleteCriticalSectionInt;
  inherited Destroy;
end;

function TsgGeneratorShapeEdge.Lock: Integer;
begin
{$IFDEF SG_FIREMONKEY}
   Result := Ord(FLock.TryEnter);
{$ELSE}
  {$IFDEF SGFPC}System.{$ENDIF}EnterCriticalSection(FLock);
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Result := FLock.LockCount;
  {$ELSE}
  Result := FLock.__m_count;
  {$ENDIF};
{$ENDIF}
end;

procedure TsgGeneratorShapeEdge.Unlock;
begin
{$IFDEF SG_FIREMONKEY}
  FLock.Leave;
{$ELSE}
  {$IFDEF SGFPC}System.{$ENDIF}LeaveCriticalSection(FLock);
{$ENDIF}
end;

function TsgGeneratorShapeEdge.CreateBulgesArc(const P1, P2: TFPoint;
  const Bulge: Double): TsgArcR;
var
  J,vCnt: Integer;
  X0, Y0, Z0, X1, Y1, DX, DY, DZ, K, A1, A2, vDelta: Double;
  vOldAngSt, vOldAngEnd: Double;
  SinA, CosA: Extended;

  function Angle(X, Y: Extended): Extended;
  begin
    Result := ArcTan2(Y - Y0, X - X0);
    if Result < 0 then
      Result := Result + 2 * Pi;
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  X0 := (P1.X + P2.X) / 2;
  Y0 := (P1.Y + P2.Y) / 2;
  Z0 := P1.Z;
  DZ := (P2.Z - Z0) / 15;
  K := Bulge;
  DX := P1.X - X0;
  DY := P1.Y - Y0;
  X1 := X0 - K * DY;
  Y1 := Y0 + K * DX;
  K := (1 - K * K) / 2 / K;
  X0 := X0 + K * DY;
  Y0 := Y0 - K * DX;
  DX := X1 - X0;
  DY := Y1 - Y0;
  K := Sqrt(DX * DX + DY * DY);
  A1 := Angle(P1.X, P1.Y);
  A2 := Angle(P2.X, P2.Y);
  if (Bulge > 0) and (A1 > A2) then
    A1 := A1 - 2 * Pi;
  if (Bulge < 0) and (A1 < A2) then
    A2 := A2 - 2 * Pi;
  Result.Center := MakeFPoint(X0, Y0, Z0);
  Result.Radius := K;
  Result.AngleS := Degree(A2);
  Result.AngleE := Degree(A1);
  Lock;
  vOldAngSt := Result.AngleS;
  vOldAngEnd := Result.AngleE;
  try
    if Result.AngleS > Result.AngleE then
      SwapSGFloats(Result.AngleS, Result.AngleE);
    if FListType <> ltNil then
    begin
      if vOldAngSt > vOldAngEnd then
  begin
        vCnt := GetNumberArcPart(FNumberCirclePart, vOldAngEnd, vOldAngSt, IsImportMode, vDelta);
    vDelta := -vDelta;
  end
  else
        vCnt := GetNumberArcPart(FNumberCirclePart, vOldAngSt, vOldAngEnd, IsImportMode, vDelta);
      AddPoint(P1);
      for J := 1 to vCnt - 1 do
      begin
        A1 := A1 - vDelta;
        SinCos(A1, SinA, CosA);
        AddPoint(MakeFPoint(X0 + K*CosA, Y0 + K*SinA, Z0));
        Z0 := Z0 + DZ;
      end;
      AddPoint(P2);
    end;
  finally
    Unlock;
  end;
end;

procedure TsgGeneratorShapeEdge.CreateCircularArc(const ACenter: TFPoint;
  const ARadius, AStartAngle, AEndAngle: Double);
begin
  if not CheckParams then Exit;
  GenerateEllipticCurve(ACenter.X, ACenter.Y, ACenter.Z, ARadius, ARadius,
    AStartAngle, AEndAngle, 0, 1);
end;

procedure TsgGeneratorShapeEdge.CreateBSpline(AControls: IsgArrayFPoint;
  const AKnots, AWeights: TsgDoubleList; const ADegree: Integer);
begin
 if (ADegree <> 3) and (ADegree < 10) then
   GenerateBSpline(AControls, AKnots, AWeights, ADegree);
end;

procedure TsgGeneratorShapeEdge.CreateCubicSpline(const APoint1, APoint2,
  APoint3, APoint4: TFPoint);
var
  T: Double;
  IncT, IncTNorm, Ni: Double;
  I, J, L: Integer;
  vPoint: TFPoint;
  vPoints: array [0..7] of TFPoint;
begin
  if not CheckParams then Exit;
  Lock;
  try
    AddPoint(APoint1);
    vPoints[0] := APoint1;
    vPoints[1] := APoint2;
    vPoints[2] := APoint3;
    vPoints[3] := APoint4;
    CopyMemory(@vPoints[4], @vPoints[0], SizeOf(TFPoint) * 4);
    IncT := 1 / FNumberSplinePart;
    for I := 3 to 8 do
    begin
      T := cnstKnotsCubicSpline[I];
      IncTNorm := IncT * Abs(cnstKnotsCubicSpline[I + 1] - cnstKnotsCubicSpline[I]);
      while T < cnstKnotsCubicSpline[I + 1] do
      begin
        vPoint := cnstFPointZero;
        for J := I - 3 to I do
        begin
          Ni := Normalize(3, J, T);
          for L := 0 to 2 do
            vPoint.V[L] := vPoint.V[L] + vPoints[J].V[L] * Ni;
        end;
        AddPoint(vPoint);
        T := T + IncTNorm;
      end;
    end;
    AddPoint(APoint4);
  finally
    Unlock;
  end;
end;

procedure TsgGeneratorShapeEdge.CreateEllipticArc(const ACenter,
  ARatPoint: TFPoint; const ARation, AStartAngle, AEndAngle: Double);
var
  vRadiusX, vRadiusY, vStartAngle, vEndAngle, vAxisSin, vAxisCos: Extended;
begin
  if not CheckParams then Exit;
  vRadiusX := DistanceVector(ARatPoint.X, ARatPoint.Y, 0);
  vRadiusY := vRadiusX * ARation;
  if not sgIsZero(vRadiusX) then
  begin
    vAxisSin := ARatPoint.Y / vRadiusX;
    vAxisCos := ARatPoint.X / vRadiusX;
  end
  else
  begin
    vAxisSin := 1.0;
    vAxisCos := 0.0;
  end;
  vStartAngle := AStartAngle;
  vEndAngle := AEndAngle;
  GenerateEllipticCurve(ACenter.X, ACenter.Y, ACenter.Z, vRadiusX, vRadiusY,
    vStartAngle, vEndAngle, vAxisSin, vAxisCos);
end;

procedure TsgGeneratorShapeEdge.CreateEllipticArcByRadiuses(const ACenter: TFPoint;
  const ARadiusX, ARadiusY, AStartAngle, AEndAngle, AAxisSin, AAxisCos: Double;
  AReverse: Boolean = False);
begin
  GenerateEllipticCurve(ACenter.X, ACenter.Y, ACenter.Z, ARadiusX, ARadiusY,
    AStartAngle, AEndAngle, AAxisSin, AAxisCos, AReverse);
end;

procedure TsgGeneratorShapeEdge.CreateEllipticArcByPoints(const ACenter, ARatPoint1,
  ARatPoint2: TFPoint; const AStartPoint, AEndPoint: PFPoint);
var
  vMajorPoint: TFPoint;
  vStartAngle, vEndAngle, vRatio: Double;
begin
 if SetRatioAndMajorPointByPoints(ACenter, ARatPoint1, ARatPoint2, vRatio,
    vMajorPoint) then
  begin
    vStartAngle := 0;
    vEndAngle := 360;
    if AStartPoint <> nil then
      vStartAngle := GetAngleParam(ACenter, vMajorPoint, vRatio, AStartPoint^, False);
    if AEndPoint <> nil then
      vEndAngle := GetAngleParam(ACenter, vMajorPoint, vRatio, AEndPoint^, False);
    CreateEllipticArc(ACenter, vMajorPoint, vRatio, vStartAngle, vEndAngle);
  end;
end;

procedure TsgGeneratorShapeEdge.CreateQuadraticSpline(const APoint1,
  APoint2, APoint3: TFPoint);
begin
  if not CheckParams then Exit;
  GenerateQuadraticSpline(APoint1, APoint2, APoint3, 0);
end;

procedure TsgGeneratorShapeEdge.SetBaseList(const AList: TsgBaseList);
begin
  FList := AList;
  FAddPoint := nil;
  FListType := Alist.ListType;
end;

procedure TsgGeneratorShapeEdge.SetListAndProc(const AList: TObject;
  const AProc: TsgProcAddPointInList;
  const ACreatePoint: TsgCreatePPoint = nil);
begin
  FListType := ltNil;
  FList := AList;
  FAddPoint := AProc;
  FCreatePoint := ACreatePoint;
  if AList is TsgObjectList then
    FListType := ltObjectList
  else
    if AList is TList then
      FListType := ltList;
end;

procedure TsgGeneratorShapeEdge.InitializeCriticalSectionInt;
begin
{$IFDEF SG_FIREMONKEY}
  FLock := TCriticalSection.Create;
{$ELSE}
  {$IFNDEF SGFPC}
  InitializeCriticalSection(FLock);
  {$ELSE}
  System.InitCriticalSection(FLock);
  {$ENDIF}
{$ENDIF}
end;

class function TsgGeneratorShapeEdge.IsBSpline(const ADegree: Integer): Boolean;
begin
  Result := (ADegree > 0) and (ADegree < 10) and (ADegree <> 3);
end;

class function TsgGeneratorShapeEdge.GetNumberArcPart(const ANumberOfCirclePart: Integer;
  const AStartAngle,AEndAngle: Double; const AOldMode: Boolean; var ADelta: Double): Integer;
var
  vAngleStart, vAngleEnd: Extended;
begin
  SetStartEndAngles(AStartAngle, AEndAngle, vAngleStart, vAngleEnd);
  if AOldMode then
    Result := ANumberOfCirclePart
  else
  begin
    Result := Round((vAngleEnd - vAngleStart) * ANumberOfCirclePart / cnstPiMul2);
    if Result <= 0 then
      Result := 1;
  end;
  ADelta := (vAngleEnd - vAngleStart) / Result;
end;

 { TsgStackRegions }

procedure TsgStackRegions.Clear;
{$IFNDEF SG_FIREMONKEY}
var
  I: Integer;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  for I := 0 to Count - 1 do
    if List{$IFDEF LIST_PTR}^{$ENDIF}[I] <> nil then
      DeleteObject(HGDIOBJ(List{$IFDEF LIST_PTR}^{$ENDIF}[I]));
  inherited Clear;
{$ENDIF}
end;

function TsgStackRegions.Pop: HRGN;
begin
  Result := 0;
  if Count > 0 then
  begin
    Result := HRGN(List{$IFDEF LIST_PTR}^{$ENDIF}[Count - 1]);
    Count := Count - 1;
  end;
end;

procedure TsgStackRegions.Push(const RGN: HRGN);
begin
  Add(Pointer(RGN));
end;


{$IFNDEF SGDEL_6}
 { TsgStrigListV6 }

procedure TsgStringListV6.Assign(Source: TPersistent);
begin
  if Source is TsgStringListV6 then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TsgStringListV6(Source).FDefined;
      FQuoteChar := TsgStringListV6(Source).FQuoteChar;
      FDelimiter := TsgStringListV6(Source).FDelimiter;
      FCaseSensitive := TsgStringListV6(Source).FCaseSensitive;
      AddStrings(TsgStringListV6(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

function TsgStringListV6.GetDelimiter: Char;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TsgStringListV6.GetQuoteChar: Char;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TsgStringListV6.SetDelimiter(const Value: Char);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TsgStringListV6.SetQuoteChar(const Value: Char);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

function TsgStringListV6.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;

procedure TsgStringListV6.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;
{$ENDIF}

{$IFNDEF SGDEL_7}
 { TsgStringListV7 }
constructor TsgStringListV7.Create;
begin
  inherited Create;
  FNameValueSeparator := NameValueSeparator;
end;

procedure TsgStringListV7.Assign(Source: TPersistent);
begin
  if Source is TsgStringListV7 then
  begin
    BeginUpdate;
    try
      Clear;
      FNameValueSeparator := TsgStringListV7(Source).FNameValueSeparator;
      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else inherited Assign(Source);
end;

function TsgStringListV7.GetNameValueSeparator: Char;
begin
  if not (sdNameValueSeparator in FDefined) then
    Self.NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TsgStringListV7.SetNameValueSeparator(const Value: Char);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TsgStringListV7.GetValueFromIndex(Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TsgStringListV7.SetValueFromIndex(Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;
{$ENDIF}

{$IFNDEF SGDEL_2005}

{ TStringsEnumerator }

constructor TStringsEnumerator.Create(AStrings: TStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TStringsEnumerator.GetCurrent: string;
begin
  Result := FStrings[FIndex];
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TsgStringListV2005 }

function TsgStringListV2005.GetLineBreak: string;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

procedure TsgStringListV2005.SetLineBreak(const Value: string);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

function TsgStringListV2005.GetTextStr: string;
var
  I, L, Size, vCount: Integer;
  P: PChar;
  S, LB: string;
begin
  vCount := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to vCount - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to vCount - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

procedure TsgStringListV2005.SetTextStr(const Value: string);
var
  P, Start, LB: PChar;
  S: string;
  LineBreakLen: Integer;
  StrPosProc: function(S1, S2: PChar): PChar;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if CompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        // Use StrPos to find line breaks. If running on a FarEast
        // locale use AnsiStrPos, which handles MBCS characters
        if not SysLocale.FarEast then
          StrPosProc := @StrPos
        else
          StrPosProc := @AnsiStrPos;
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := StrPosProc(P, PChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

function TsgStringListV2005.GetEnumerator: TStringsEnumerator;
begin
  Result := TStringsEnumerator.Create(Self);
end;
{$ENDIF}

{ TsgStringListNemed }

function TsgStringListNamed.AddNameValue(const AName, AValue: string; const AType: Cardinal): Integer;
begin
  Result := AddObject(AName + NameValueSeparator + AValue, TObject(Pointer(AType)));
end;

constructor TsgStringListNamed.Create;
begin
  inherited Create;
  Sorted := True;
  CaseSensitive := True;
  Duplicates := dupIgnore;
  NameValueSeparator := '=';
end;

function TsgStringListNamed.FindName(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(Names[I], S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TsgStringListNamed.IndexOfName(const Name: string): Integer;
begin
  if Sorted then
  begin
    if not FindName(Name, Result) then
      Result := -1;
  end
  else
    Result := inherited IndexOfName(Name);
end;

{ TsgVertex }

procedure TsgVertex.AddEdge(AEdge: TsgEdge; APointIndex: Integer);
begin
  AEdge.Vertexes[APointIndex] := Self;
  if FEdges.IndexOf(AEdge) < 0 then
    FEdges.Add(AEdge);
end;

constructor TsgVertex.Create;
begin
  inherited Create;
  FEdges := TList.Create;
end;

procedure TsgVertex.DeleteEdge(AEdge: TsgEdge);
begin
  FEdges.Remove(AEdge);
  if AEdge.Vertexes[1] = Self then
    AEdge.Vertexes[1] := nil;
  if AEdge.Vertexes[2] = Self then
    AEdge.Vertexes[2] := nil;
end;

{$IFDEF SGFPC}
function TsgVertex.IsEqual(AVertex: TsgVertex): Boolean;
begin
  Result := False;
end;
{$ENDIF}

destructor TsgVertex.Destroy;
begin
  FreeAndNil(FEdges);
  inherited Destroy;
end;

function TsgVertex.GetEdge(AIndex: Integer): TsgEdge;
begin
  if InRange(AIndex, 0, FEdges.Count - 1) then
    Result := TsgEdge(FEdges[AIndex])
  else
    Result := nil;
end;

function TsgVertex.GetEdgesCount: Integer;
begin
  Result := FEdges.Count;
end;

{ TsgEdge }

constructor TsgEdge.Create;
begin
  inherited Create;
  FVertexes := TList.Create;
  FVertexes.Add(nil);
  FVertexes.Add(nil);
end;

destructor TsgEdge.Destroy;
begin
  FreeAndNil(FVertexes);
  inherited Destroy;
end;

function TsgEdge.GetVertexes(AIndex: Integer): TsgVertex;
begin
  if InRange(AIndex, 1, FVertexes.Count) then
    Result := TsgVertex(FVertexes[AIndex - 1])
  else
    raise Exception.Create('');
end;

function TsgEdge.HasVertex(AVertex: TsgVertex): Integer;
begin
  Result := FVertexes.IndexOf(AVertex);
end;

function TsgEdge.IsIdentical(AEdge: TsgEdge): Boolean;
begin
  Result := True;
end;

function TsgEdge.IsLoop: Boolean;
begin
  Result := Vertexes[1] = Vertexes[2];
end;

procedure TsgEdge.SetVertexes(AIndex: Integer; const Value: TsgVertex);
begin
  if InRange(AIndex, 1, FVertexes.Count) then
    FVertexes[AIndex - 1] := Value
  else
    raise Exception.Create('');
end;



{ TsgEdgeLine }

constructor TsgEdgeLine.Create;
begin
  inherited Create;

end;

destructor TsgEdgeLine.Destroy;
begin

  inherited Destroy;
end;

function TsgEdgeLine.EdgeType: TsgEdgeType;
begin
  Result := eetLine;
end;

procedure TsgEdgeLine.GetAngleLineAndNormal(AVertex: TsgVertexPoint;
  const ARadius: Double; var ALine, ANormal: TsgLine);
var
  vPt2: TFPoint;
begin
  if AVertex = Vertexes[1] then
    vPt2 := TsgVertexPoint(Vertexes[2]).Point
  else
    vPt2 := TsgVertexPoint(Vertexes[1]).Point;
  ALine := MakeLine(AVertex.Point, vPt2);
  ANormal := BypassNormal;
end;

function TsgEdgeLine.GetVetrexPoints(AIndex: Integer): TFPoint;
begin
  case AIndex of
    1: Result := Line.Point1;
    2: Result := Line.Point2;
  else
    raise Exception.Create('');
  end;
end;

function TsgEdgeLine.FastLineCrossingCheck(ALine: TsgLine): Boolean;

  function Check(const A, B, C1, C2: Double; AIsMax: Boolean): Boolean;
  var
    Z: Double;
  begin
    if AIsMax then
      Z := Max(A, B)
    else
      Z := Min(A, B);
    if AIsMax then
      Result := (Z < C1) and (Z < C2)
    else
      Result := (Z > C1) and (Z > C2);
  end;

var
  vExactlyNotCross: Boolean;
begin   // fast cross check
  vExactlyNotCross :=
    Check(Line.Point1.X, Line.Point2.X, ALine.Point1.X, ALine.Point2.X, True) or
    Check(Line.Point1.X, Line.Point2.X, ALine.Point1.X, ALine.Point2.X, False) or
    Check(Line.Point1.Y, Line.Point2.Y, ALine.Point1.Y, ALine.Point2.Y, True) or
    Check(Line.Point1.Y, Line.Point2.Y, ALine.Point1.Y, ALine.Point2.Y, False);
  Result := not vExactlyNotCross;
end;

function TsgEdgeLine.IsCrossArc(const AArc: TsgArcR; ACrossPts: TList): Boolean;
var
  vPPt1, vPPt2: PFPoint;
  vCnt: Integer;
begin
  New(vPPt1);
  New(vPPt2);
  vCnt := IsCrossArcAndSegmentAP(AArc, Line, vPPt1, vPPt2);
  Result := AddElementsToList(ACrossPts, vCnt, [vPPt1, vPPt2], True);
end;

function TsgEdgeLine.IsCrossLine(const ALine: TsgLine; ACrossPts: TList): Boolean;
var
  vPt: PFPoint;
begin
  Result := FastLineCrossingCheck(ALine);
  if not Result then
    Exit;
  New(vPt);
  Result := IsCrossSegments(Line, ALine, vPt);
  if Result then
    ACrossPts.Add(vPt)
  else
    Dispose(vPt);
end;

function TsgEdgeLine.IsIdentical(AEdge: TsgEdge): Boolean;
var
  vEdge: TsgEdgeLine absolute AEdge;
begin
  Result := inherited IsIdentical(AEdge);
  if Result then
    Result := IsEqualsgLines(Line, vEdge.Line);
end;

function TsgEdgeLine.SplitEdge(const ACutPts, AResult: TList; AEpsilon: Double): Boolean;
var
  I: Integer;
  vArr: array of TFPoint;
begin
  SetLength(vArr, ACutPts.Count);
  for I := 0 to ACutPts.Count - 1 do
    vArr[I] := PFPoint(ACutPts[I])^;
  Result := SafeSplitLinePtsToList(Line, vArr, AResult, AEpsilon);
end;

{ TsgEdgeArc }

constructor TsgEdgeArc.Create;
begin
  inherited Create;

end;

destructor TsgEdgeArc.Destroy;
begin

  inherited Destroy;
end;

function TsgEdgeArc.EdgeType: TsgEdgeType;
begin
  Result := eetArc;
end;

procedure TsgEdgeArc.GetAngleLineAndNormal(AVertex: TsgVertexPoint;
  const ARadius: Double; var ALine, ANormal: TsgLine);
var
  RotAngle: TsgFloat;
  vLine: TsgLine;
  vCircle: TsgArcR;
  vCrossPt1, vCrossPt2: TFPoint;
  vVertex1, vVertex2: TsgVertexPoint;
begin
  vVertex1 := AVertex;
  if vVertex1 = Vertexes[1] then // vVertex1 = AVertex, another vertex - vVertex2
    vVertex2 := TsgVertexPoint(Vertexes[2])
  else
    vVertex2 := TsgVertexPoint(Vertexes[1]);

  vLine := MakeLine(vVertex1.Point, vVertex2.Point);
  vCircle.Center := vVertex1.Point;
  vCircle.Radius := ARadius;
  vCircle.AngleS := 0;
  vCircle.AngleE := 360;
  case IsCrossArcsAP(Arc, vCircle, @vCrossPt1, @vCrossPt2) of
    1: ALine := MakeLine(vVertex1.Point, vCrossPt1);
//    0, 2: sgNop;
  end;

  RotAngle :=  GetAngleOfVectorsDirect(SubFPoint2D(vLine.Point2, vLine.Point1),
    SubFPoint2D(ALine.Point2, ALine.Point1), True);
  ANormal.Point1 := RotateAroundFPoint(BypassNormal.Point1, ALine.Point1, False, RotAngle);
  ANormal.Point2 := RotateAroundFPoint(BypassNormal.Point2, ALine.Point1, False, RotAngle);
end;

function TsgEdgeArc.GetVetrexPoints(AIndex: Integer): TFPoint;
var
  v2dPt: TF2DPoint;
begin
  case AIndex of
    1: v2dPt := GetStartF2DPoint(Arc);
    2: v2dPt := GetEndF2DPoint(Arc);
  else
    raise Exception.Create('');
  end;
  Result := MakeFPoint(v2dPt.X, v2dPt.Y, 0);
end;

function TsgEdgeArc.IsCrossArc(const AArc: TsgArcR; ACrossPts: TList): Boolean;
var
  vPPt1, vPPt2: PFPoint;
  vCnt: Integer;
begin
  New(vPPt1);
  New(vPPt2);
  vCnt := IsCrossArcsAP(AArc, Arc, vPPt1, vPPt2);
  Result := AddElementsToList(ACrossPts, vCnt, [vPPt1, vPPt2], True);
end;

function TsgEdgeArc.IsCrossLine(const ALine: TsgLine;
  ACrossPts: TList): Boolean;
var
  vPPt1, vPPt2: PFPoint;
  vCnt: Integer;
begin
  New(vPPt1);
  New(vPPt2);
  vCnt := IsCrossArcAndSegmentAP(Arc, ALine, vPPt1, vPPt2);
  Result := AddElementsToList(ACrossPts, vCnt, [vPPt1, vPPt2], True);
end;

function TsgEdgeArc.IsIdentical(AEdge: TsgEdge): Boolean;
var
  vEdge: TsgEdgeArc absolute AEdge;
begin
  Result := inherited IsIdentical(AEdge);
  if Result then
    Result := IsEqualArcR(Arc, vEdge.Arc);
end;

function TsgEdgeArc.SplitEdge(const ACutPts, AResult: TList; AEpsilon: Double): Boolean;
var
  I: Integer;
  vArr: array of TFPoint;
begin
  SetLength(vArr, ACutPts.Count);
  for I := 0 to ACutPts.Count - 1 do
    vArr[I] := PFPoint(ACutPts[I])^;
  Result := SafeSplitArcPtsToList(Arc, vArr, AResult, AEpsilon);
end;

{ TsgVertexPoint }

function TsgVertexPoint.IsEqual(AVertex: TsgVertex): Boolean;
begin
  if AVertex.ClassType = Self.ClassType then
    Result := IsEqualFPoints(Point, TsgVertexPoint(AVertex).Point)
  else
    Result := inherited IsEqual(AVertex);
end;

function TsgVertexPoint.SelectEdgeBySecondVertex(ASecondVertex: TsgVertexPoint): TsgEdgeEntity;
var
  I: Integer;
begin
  Result := nil;
  if ASecondVertex = Self then
    Exit;  
  for I := 0 to EdgesCount - 1 do
  begin
    Result := TsgEdgeEntity(Edges[I]);
    if Result.HasVertex(ASecondVertex) >= 0 then
      Exit;
  end;
end;


{ TsgEdgeEntity }

function TsgEdgeEntity.BypassJoin(AEdges: TsgObjectList): Boolean;
var
  I: Integer;
  vEdge: TsgEdgeEntity;
  vEdgesList: TList;
begin
  if AEdges.Count > cnstMaxJoinEdgesCnt then
    raise EJoinInfLoop.Create('');
  Result := (AEdges.Count > 0) and (AEdges[0] = Pointer(Self));
  if (Vertex2.EdgesCount = 0) {or (AEdges.IndexOf(Self) >= 0)} or Result then
    Exit;
  Inc(FBypassCount);
  vEdgesList := TList.Create;
  try
    GetBypassPriorityList(vEdgesList);
    I := 0;
    AEdges.Add(Self);
    while I < vEdgesList.Count do
    begin
      vEdge := TsgEdgeEntity(vEdgesList[I]);
      if vEdge.BypassPossible then
      begin
        Result := vEdge.BypassJoin(AEdges);
        if Result then
          Break;
      end;
      Inc(I);
    end;
    if not Result then
      AEdges.Delete(AEdges.Count - 1);
  finally
    Dec(FBypassCount);
    vEdgesList.Free;
  end;
end;

function TsgEdgeEntity.BypassPossible: Boolean;
var
  vCount,I: Integer;
begin
  vCount := 0;
  for I := 0 to FVertexes.Count - 1 do
    Inc(vCount, TsgVertex(FVertexes[I]).EdgesCount);
  Result := FBypassCount <= vCount;
end;

procedure TsgEdgeEntity.CalcBypass(APrevBypassSign: TValueSign;
  APrevVertex: TsgVertexPoint);
var
  vSimpleEdge: TsgLine;
  vMiddle: TFPoint;
begin
  FVertex1Num := 1;
  FVertex2Num := 2;
  if Vertexes[2] = APrevVertex then
    SwapInts(FVertex1Num, FVertex2Num);
  vSimpleEdge := MakeLine(Vertex1.Point, Vertex2.Point);

  vMiddle := MiddleFPoint(vSimpleEdge.Point1, vSimpleEdge.Point2);
  BypassSign := APrevBypassSign;
  BypassNormal := CalcBypassNormal(vSimpleEdge, vMiddle, BypassSign);
end;

function TsgEdgeEntity.CalcBypassNormal(ALine: TsgLine; APt: TFPoint;
  ABypassSign: TsgFloat): TsgLine;
var
  vLength, vp1: TsgFloat;
  vN1, vN2: TsgLine;
begin
  vLength := DistanceFPoint(ALine.Point1, ALine.Point2);
  vN1 := MakeLine(APt, GetNormalPt(ALine.Point1, ALine.Point2, APt, vLength));
  vN2 := MakeLine(APt, GetNormalPt(ALine.Point1, ALine.Point2, APt, -vLength));
  vp1 := VectorProduct2d(ALine, vN1);

  if Sign(vp1) = ABypassSign then
    Result := vN1
  else
    Result := vN2;
end;

procedure TsgEdgeEntity.ClearBypass;
begin
  FVertex1Num := -1;
  FVertex2Num := -1;
  FBypassAngle := -1;
  FBypassNext := nil;
  FBypassCount := 0;
  FBypassNormal := MakeLine(cnstFPointIllegal, cnstFPointIllegal);
  FBypassSign := 0;
end;

constructor TsgEdgeEntity.Create;
begin
  inherited Create;
  ClearBypass;
end;

destructor TsgEdgeEntity.Destroy;
begin                                 
  inherited Destroy;
end;

function TsgEdgeEntity.CalcBypassAngle(ALine1, ALine2, ANormal1,
  ANormal2: TsgLine): TsgFloat;

  procedure CorrectCommonPoint(var ALine1, ALine2: TsgLine);
  begin
    if IsEqualFPoints(ALine1.Point1, ALine2.Point1) then
      SwapFPoints(ALine1.Point1, ALine1.Point2)
    else
    if IsEqualFPoints(ALine1.Point1, ALine2.Point2) then
    begin
      SwapFPoints(ALine1.Point1, ALine1.Point2);
      SwapFPoints(ALine2.Point1, ALine2.Point2);
    end
    else
    if not IsEqualFPoints(ALine1.Point2, ALine2.Point1) then
      if IsEqualFPoints(ALine1.Point2, ALine2.Point2) then
        SwapFPoints(ALine2.Point1, ALine2.Point2);
  end;

var
  vCrossPt, vMiddlePt, v1, v2: TFPoint;
begin
  CorrectCommonPoint(ALine1, ALine2); 
  v1 := SubFPoint(ALine1.Point1, ALine1.Point2);
  v2 := SubFPoint(ALine2.Point2, ALine2.Point1);
  Result := GetAngleOfVectors(v1, v2, False);
  if IsCrossLines(ANormal1, ANormal2, @vCrossPt) then
  begin
    vMiddlePt := MiddleFPoint(ALine1.Point1, ALine2.Point2);
    if PointClassifyEx(ALine1, vMiddlePt) <>
      PointClassifyEx(ALine1, ANormal1.Point2) then
      Result := 360 - Result;
  end;
  FBypassAngle := Result;
end;

{$IFDEF SG_FIREMONKEY}
function CompareEdgeAngles(const Value1, Value2: Pointer): Integer;
{$ELSE}
function CompareEdgeAngles(Value1, Value2: Pointer): Integer;
{$ENDIF}
var
  vAngle: TsgFloat;
begin
  vAngle := TsgEdgeEntity(Value1).FBypassAngle -
    TsgEdgeEntity(Value2).FBypassAngle;
  if SameValue(vAngle, 0) then
    Result := 0
  else
    if vAngle > 0 then
      Result := 1
    else
      Result := -1;
end;

procedure TsgEdgeEntity.GetBypassPriorityList(AList: TList);
var
  I: Integer;
  vLength, vRadius: Double;
  vEdge: TsgEdgeEntity;
begin
  vRadius := GetLengthLine(GetSimpleEdge);
  for I := 0 to Vertex2.EdgesCount - 1 do
  begin
    vEdge := TsgEdgeEntity(Vertex2.Edges[I]);
    if (vEdge = Self) or (vEdge = nil) or vEdge.IsLoop then
      Continue;
    AList.Add(vEdge);
    vEdge.CalcBypass(BypassSign, Vertex2);
    vLength := GetLengthLine(vEdge.GetSimpleEdge);
    if (not IsEqual(vLength, 0)) and (vRadius > vLength) then
      vRadius := vLength;
  end;
  vRadius := vRadius / 2;
  for I := 0 to AList.Count - 1 do
  begin
    vEdge := TsgEdgeEntity(AList[I]);
    vEdge.GetEdgeAngle(Self, vRadius);
  end;
{$IFDEF SG_FIREMONKEY}
  if AList.Count > 0 then
    QSortArrayBounds(@AList.List[0], CompareEdgeAngles, 0, AList.Count - 1);
{$ELSE}
  AList.Sort(@CompareEdgeAngles);
{$ENDIF}
end;

function TsgEdgeEntity.GetEdgeAngle(APrevEdge: TsgEdgeEntity; ARadius: Double): TsgFloat;
var
  vLine1, vLine2: TsgLine;
  vNormal1, vNormal2: TsgLine;
begin
  Result := 0;
  if APrevEdge = nil then
    Exit;
  APrevEdge.GetAngleLineAndNormal(APrevEdge.Vertex2, ARadius, vLine1, vNormal1);
  GetAngleLineAndNormal(Vertex1, ARadius, vLine2, vNormal2);
  Result := CalcBypassAngle(vLine1, vLine2, vNormal1, vNormal2);
end;

function TsgEdgeEntity.GetSimpleEdge: TsgLine;
begin
  Result := MakeLine(Vertex1.Point, Vertex2.Point);
end;

function TsgEdgeEntity.GetVertex1: TsgVertexPoint;
begin
  if FVertex1Num = -1 then
    Result := TsgVertexPoint(Vertexes[1])
  else
    Result := TsgVertexPoint(Vertexes[FVertex1Num]);
end;

function TsgEdgeEntity.GetVertex2: TsgVertexPoint;
begin
  if FVertex2Num = -1 then
    Result := TsgVertexPoint(Vertexes[2])
  else
    Result := TsgVertexPoint(Vertexes[FVertex2Num]);
end;

function TsgEdgeEntity.IsIdentical(AEdge: TsgEdge): Boolean;
var
  vEdge: TsgEdgeEntity absolute AEdge;
begin
  Result := inherited IsIdentical(AEdge);
  if Result then
    Result := (EdgeType = vEdge.EdgeType);
end;

function TsgEdgeEntity.VectorProduct2d(AL1, AL2: TsgLine): Extended;
var
  v1, v2: TFPoint;
begin
// vector product in 2d: [a,b] = (0, 0, x1*y2 - y1*x2)
  v1 := SubFPoint2D(AL1.Point2, AL1.Point1);
  v2 := SubFPoint2D(AL2.Point2, AL2.Point1);
  Result := DenomVectors(v1.X, v1.Y, v2.X, v2.Y);
end;

////////////////
function CmdSymReplaceU(const AText: sgUnicodeStr): sgUnicodeStr;
begin
  Result := '';
  if Length(AText) < 4 then
    Exit;
  SetLength(Result, 1);
  PWord(Result)^ := StringHexToIntDef(Copy(AText, 1, 4), 0);
end;

function CmdSymReplaceM(const AText: sgUnicodeStr): sgUnicodeStr;
var
  vMBytes: array[0 .. 1] of Word;
begin
  Result := '';
  if Length(AText) < 5 then
    Exit;
  Integer(vMBytes) := Swap(StringHexToIntDef(Copy(string(AText), 1, 5), 0));
  SetLength(Result, 1);
  UnicodeFromLocaleChars(GetCodePageByIndex(vMBytes[1]), 0,
    PAnsiChar(@vMBytes[0]), 2, PWideChar(Result), 1);
end;

//function CmdSymReplaceDPercent(const AText: sgUnicodeStr): sgUnicodeStr;
//var
//  vAnsi: AnsiString;
//  vCode: Integer;
//begin
//  vAnsi := '';
//  SetCodePage(RawByteString(vAnsi), AConverter.CodePage, False);
//  vCode := StrToIntDef(AText, -1);
//  if vCode = -1 then
//    Result := ''
//  else
//    Result := sgUnicodeStr(AnsiChar(vCode));
//end;

function ParseCommandSym(const AText, ACmd: sgUnicodeStr; var AOutText: sgUnicodeStr;
  ACharCountDef: Integer; AProc: TCmdSymReplaceProc): Boolean;

  function GetCharCnt(AIndex: Integer): Integer;
  begin
    Result := 0;
    if ACharCountDef > 0 then
      Result := Math.Min(ACharCountDef, Length(AOutText) - AIndex + 1)
    else
      while (Length(AOutText) >= AIndex + Result) and
        IsDigit(AOutText[AIndex + Result]) do
        Inc(Result);
  end;

var
  vParseChars: sgUnicodeStr;
  I, vParseCharCount: Integer;
begin
  Result := False;
  AOutText := AText;
  I := Pos(ACmd, AOutText);
  while I > 0 do
  begin
    Delete(AOutText, I, Length(ACmd));
    vParseCharCount := GetCharCnt(I);
    if vParseCharCount > 0 then
    begin
      vParseChars := Copy(AOutText, I, vParseCharCount);
      Delete(AOutText, I, vParseCharCount);
      Insert(AProc(vParseChars), AOutText, I);
      Result := True;
    end;
    I := Pos(ACmd, AOutText);
  end;
end;

function BinSearch(AList: TList; ACompareFunc: TListSortCompare;
  var AIndex: Integer; var AParam): Boolean;
var
  vCompareResult, vLeft, vRight: Integer;
begin
  Result := False;
  AIndex := 0;
  vLeft := 0;
  vRight := AList.Count;
  while True do
  begin
    AIndex := (vLeft + vRight) shr 1;
    if vLeft = vRight then
      Exit;
    vCompareResult := ACompareFunc(AList[AIndex], @AParam);
    Result := vCompareResult = 0;
    if Result then
      Exit;
    if vCompareResult > 0 then
      vRight := AIndex
    else
      vLeft := AIndex + 1;
  end;
end;

{$IFDEF SG_FAST_CODE}
const
  MxInt: Integer = MaxInt;
{$ENDIF}

// fastest more then 2.5 times
function FPointXMat2DLongint(const APoint: TFPoint; const AMatrix: TFMatrix): TPoint; {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
{$IFDEF SG_FAST_CODE}
{$IFDEF SG_CPUX64}
  // RCX: APoint
  // RDX: AMatrix
asm
  fld qword ptr [rcx]     // P.x
  fld ST(0)               // P.x
  fmul qword ptr [rdx]    // P.x * M[0,0]
  fld qword ptr [rcx+$08] // P.Y
  fmul qword ptr [rdx+$18]// P.Y * M[1,0]
  faddp                   // ST(0) = ST(1) + ST(0)
  fld qword ptr [rcx+$10] // P.Z
  fmul qword ptr [rdx+$30]// P.Z * M[2,0]
  faddp                   // ST(0) = ST(1) + ST(0)
  fadd qword ptr [rdx+$48]// ST(0) = ST(0) + M[3,0]
  fld ST(0)               // load FPU stack by ST(0)
  fabs                    // ST(0) = Absolute ST(0)
  fild dword ptr [MxInt]  // load $7FFFFFFF (Max Integer)
  fcompp                  // Compare Absolute ST(1) and $7FFFFFFF
  fstsw ax                // FPU state to ax
  sahf                    // load ah to flag's register
  jbe @BigValueX          // if Absolute ST(1) more then $7FFFFFFF jump to set Result.X := $7FFFFFFF
  fistp dword ptr [Result]   // else Result.X := Round(ST(0))

@calcy:
  fmul qword ptr [rdx+$08]// P.X * M[0, 1]
  fld qword ptr [rcx+$08] // Load P.Y
  fmul qword ptr [rdx+$20]// P.Y * M[1, 1]
  faddp                   // ST(0) = ST(1) + ST(0)
  fld qword ptr [rcx+$10] // Load P.Z
  fmul qword ptr [rdx+$38]// P.Z * M[2, 1]
  faddp                   // ST(0) = ST(1) + ST(0)
  fadd qword ptr [rdx+$50]// ST(0) = M[3, 1] + ST(0)
  fld ST(0)               // Load ST(1) = ST(0)
  fabs                    // Absolute ST(0)
  fild dword ptr [MxInt]  // load $7FFFFFFF (Max Integer)
  fcompp                  // Compare Absolute ST(1) and $7FFFFFFF
  fstsw ax                // FPU state to ax
  sahf                    // load ah to flag's register
  jbe @BigValueY          // if Absolute ST(1) more then $7FFFFFFF jump to set Result.Y := $7FFFFFFF
  fistp dword ptr [Result+$04]// else Result.Y := Round(ST(0))
  wait                    // wait FPU memory operation
  jmp @exitf
@BigValueX:
  fstp ST(0)              // pop FPU stack
  wait                    // wait FPU memory operation
  mov eax,MxInt
  mov dword ptr [Result],eax           // set Result.X := $7FFFFFFF (Max Integer)
  jmp @calcy              // jump to calc Result.Y
@BigValueY:
  fstp ST(0)              // pop FPU stack
  wait                    // wait FPU memory operation
  mov eax,MxInt
  mov dword ptr [Result+$04],eax    // set Result.Y := $7FFFFFFF (Max Integer)
@exitf:
end;
{$ELSE}
    // EAX: APoint
    // EDX: AMatrix
    // ECX: Result
asm
  push esi
  mov esi,eax
  fld qword ptr [esi]     // P.x
  fld ST(0)               // P.x
  fmul qword ptr [edx]    // P.x * M[0,0]
  fld qword ptr [esi+$08] // P.Y
  fmul qword ptr [edx+$18]// P.Y * M[1,0]
  faddp                   // ST(0) = ST(1) + ST(0)
  fld qword ptr [esi+$10] // P.Z
  fmul qword ptr [edx+$30]// P.Z * M[2,0]
  faddp                   // ST(0) = ST(1) + ST(0)
  fadd qword ptr [edx+$48]// ST(0) = ST(0) + M[3,0]
  fld ST(0)               // load FPU stack by ST(0)
  fabs                    // ST(0) = Absolute ST(0)
  fild dword ptr [MxInt]  // load $7FFFFFFF (Max Integer)
  fcompp                  // Compare Absolute ST(1) and $7FFFFFFF
  fstsw ax                // FPU state to ax
  sahf                    // load ah to flag's register
  jbe @BigValueX          // if Absolute ST(1) more then $7FFFFFFF jump to set Result.X := $7FFFFFFF
  fistp dword ptr [ecx]   // else Result.X := Round(ST(0))

@calcy:
  fmul qword ptr [edx+$08]// P.X * M[0, 1]
  fld qword ptr [esi+$08] // Load P.Y
  fmul qword ptr [edx+$20]// P.Y * M[1, 1]
  faddp                   // ST(0) = ST(1) + ST(0)
  fld qword ptr [esi+$10] // Load P.Z
  fmul qword ptr [edx+$38]// P.Z * M[2, 1]
  faddp                   // ST(0) = ST(1) + ST(0)
  fadd qword ptr [edx+$50]// ST(0) = M[3, 1] + ST(0)
  fld ST(0)               // Load ST(1) = ST(0)
  fabs                    // Absolute ST(0)
  fild dword ptr [MxInt]  // load $7FFFFFFF (Max Integer)
  fcompp                  // Compare Absolute ST(1) and $7FFFFFFF
  fstsw ax                // FPU state to ax
  sahf                    // load ah to flag's register
  jbe @BigValueY          // if Absolute ST(1) more then $7FFFFFFF jump to set Result.Y := $7FFFFFFF
  fistp dword ptr [ecx+$04]// else Result.Y := Round(ST(0))
  wait                    // wait FPU memory operation
  pop esi                 // restore esi
  ret
@BigValueX:
  fstp ST(0)              // pop FPU stack
  wait                    // wait FPU memory operation
  mov eax,MxInt
  mov [ecx],eax           // set Result.X := $7FFFFFFF (Max Integer)
  jmp @calcy              // jump to calc Result.Y
@BigValueY:
  fstp ST(0)              // pop FPU stack
  wait                    // wait FPU memory operation
  mov eax,MxInt
  mov [ecx+$04],eax       // set Result.Y := $7FFFFFFF (Max Integer)
@exitf:
  pop esi                 // restore esi
end;
{$ENDIF}
{$ELSE}
var
  vResult: TF2DPoint;
begin
  vResult := FPointXMat2D(APoint, AMatrix);
  if Abs(vResult.X) < MaxInt then
    Result.X := Round(vResult.X)
  else
    Result.X := MaxInt;
  if Abs(vResult.Y) < MaxInt then
    Result.Y := Round(vResult.Y)
  else
    Result.Y := MaxInt;
end;
{$ENDIF}

{$IFNDEF SGDEL_2009}
function CharInSet(AChar: AnsiChar; const ACharSet: TSysCharSet): Boolean;
begin
  Result := AChar in ACharSet;
end;

function CharInSet(AChar: WideChar; const ACharSet: TSysCharSet): Boolean;
begin
  Result := (AChar < #$0100) and (AnsiChar(AChar) in ACharSet);
end;
{$ENDIF}

function ApplyScale(const ARealImgMatrix: TFMatrix; const AOffset: TFPoint;
  const AExtents: TFRect; const ARectLeft, ARectTop, ARectRight, ARectBottom: Double;
  const AStretch, ANotYCrossover: Boolean): TFMatrix;{$IFDEF USE_INLINE}inline;{$ENDIF}
// for drawing in given sizes
var
  vScale, vCenter, vOffs: TFPoint;
  vWidth, vHeight, vAbsWidth, vAbsHeight, kX, kY: Double;
begin
  vWidth := ARectRight - ARectLeft;
  vHeight := ARectBottom - ARectTop;
  vAbsWidth := AExtents.Right - AExtents.Left;
  vAbsHeight := AExtents.Top - AExtents.Bottom;
  case Ord(vWidth = 0) or (Ord(vHeight = 0) shl 1) of
    1: vWidth := vHeight;
    2: vHeight := vWidth;
    3:
      begin
        vWidth := 1;
        vHeight := 1;
      end
  end;
  if vAbsWidth = 0 then
    vAbsWidth := vWidth;
  if vAbsHeight = 0 then
    vAbsHeight := vHeight;
  kX := vWidth / vAbsWidth;
  kY := vHeight / vAbsHeight;
  if not AStretch then
    if Abs(kX) > Abs(kY) then
      kX := kY
    else
      if Abs(kY) > Abs(kX) then
        kY := kX;
  vScale.X := kX;
  vScale.Y := kY;
  if Abs(kX) > Abs(kY) then
    vScale.Z := kY
  else
    vScale.Z := kX;
  if ANotYCrossover then
    vScale.Y := -vScale.Y;
  Result := FMatScale(ARealImgMatrix, vScale);
  vOffs := FPointXMat(AOffset, Result);
  vCenter.X := 0.5 * (ARectRight + ARectLeft);
  vCenter.Y := 0.5 * (ARectTop + ARectBottom);
  vCenter.Z := 0;
  vOffs := SubFPoint(vCenter, vOffs);
  FMatOffset(Result, vOffs);
end;

function BGRToRGB(ABGR: Integer): Integer; register;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFDEF SG_FAST_CODE}
  asm
  {$IFDEF SG_CPUX64}
    MOV EAX,ECX
  {$ENDIF}
    BSWAP EAX
    SHR EAX,8
  end;
{$ELSE}
begin
  PByteArray(@Result)^[0] := PByteArray(@ABGR)^[2];
  PByteArray(@Result)^[1] := PByteArray(@ABGR)^[1];
  PByteArray(@Result)^[2] := PByteArray(@ABGR)^[0];
  PByteArray(@Result)^[3] := 0;
end;
{$ENDIF}

procedure CheckHeadVar(const APHeadVarStruct: PsgHeadVarStruct);
begin
  if vnDIMENBL in cnstIniHeadVar.InitFlags then
//  if cnstIniHeadVar.InitFlags and 1 <> 0 then
  begin
    if vnDIMDEC in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 2 <> 0 then
      APHeadVarStruct^.DimProps.Dec := cnstIniHeadVar.DimDec;
    if vnDIMLFAC in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 4 <> 0 then
      APHeadVarStruct^.DimProps.LFac := cnstIniHeadVar.DimLFac;
    if vnDIMTXT in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 8 <> 0 then
      APHeadVarStruct^.DimProps.Txt := cnstIniHeadVar.DimTxt;
    if vnDIMTXTALIGN in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 16 <> 0 then
    begin
      case cnstIniHeadVar.DimTextAlign of
        1://align
          begin
            APHeadVarStruct^.DimProps.TIH := False;
            APHeadVarStruct^.DimProps.TOH := False;
          end;
        2://ISO
          begin
            APHeadVarStruct^.DimProps.TIH := False;
            APHeadVarStruct^.DimProps.TOH := True;
          end;
      else//gorizontal - default
        APHeadVarStruct^.DimProps.TIH := True;
        APHeadVarStruct^.DimProps.TOH := True;
      end;
    end;
    if vnDIMCLRT in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 32 <> 0 then
      APHeadVarStruct^.DimProps.ClrT := cnstIniHeadVar.DimClrT;
    if vnDIMGAP in cnstIniHeadVar.InitFlags then
//    if cnstIniHeadVar.InitFlags and 64 <> 0 then
      APHeadVarStruct^.DimProps.Gap := cnstIniHeadVar.DimGap;
  end;
end;

function IsNameCorrect(const AName: string;
  const AExcludeCharSet: TSysCharSet): Boolean; overload;
{var
  I: Integer;
begin
  Result := True;
  if Length(AName) > 0 then
    for I := Low(cnstCheckSymbolAutoCadName) to High(cnstCheckSymbolAutoCadName) do
    begin
      if Pos(cnstCheckSymbolAutoCadName[I], AName) > 0 then
      begin
        Result := False;
        Break;
      end;
    end
  else
    Result := False;
end;}
var
  Len: Integer;
  vCharSet: TSysCharSet;
begin
  vCharSet := cnstInvalidSymbolAutoCadName - AExcludeCharSet;
  Len := Length(AName);
  Result := (Len <= 255) and (Len > 0);
  while Result and (Len >= 1) do
  begin
    Result := not CharInSet(AName[Len], vCharSet);
    Dec(Len);
  end;
end;

function IsNameCorrect(const AName: string): Boolean; overload;
begin
  Result := IsNameCorrect(AName, []);
end;

function GetNameCorrect(const AName: string;
  const AExcludeCharSet: TSysCharSet): string;
var
  I: Integer;
  vCharSet: TSysCharSet;
begin
  Result := AName;
  SetLength(Result, MinI(Length(Result), 255));
  vCharSet := cnstInvalidSymbolAutoCadName - AExcludeCharSet;
  for I := 1 to Length(Result) do
  begin
    if CharInSet(Result[I], vCharSet) then
      Result[I] := cnstLowLine;
  end;
end;

function GetNameCorrect(const AName: string): string;
begin
  Result := GetNameCorrect(AName, []);
end;

function ColorToDXF(const Value: TsgColorCAD): Cardinal;
begin
  Result := ConvertColorCADToIndexColor(Value, False, @arrDXFtoRGBColors);
end;

function IsEqualMethod(AMethod1, AMethod2: TMethod): Boolean;
begin
  Result := (AMethod1.Code = AMethod2.Code) and (AMethod1.Data = AMethod2.Data);
end;

function IsEqualEvent(AEvent1, AEvent2: TNotifyEvent): Boolean;
begin
  Result := IsEqualMethod(TMethod(AEvent1), TMethod(AEvent2));
end;

function CompareDXFPointByXCoord(AItem1, AItem2: Pointer): Integer;
begin
  if(PdxfPoint(AItem1)^.X> PdxfPoint(AItem2)^.X) then
    Result := +1
  else if(PdxfPoint(AItem1)^.X = PdxfPoint(AItem2)^.X) then
    Result := 0
  else
    Result := -1;
end;

function CompareDXFPointByYCoord(AItem1, AItem2: Pointer): Integer;
begin
  if(PdxfPoint(AItem1)^.Y> PdxfPoint(AItem2)^.Y) then
    Result := +1
  else if(PdxfPoint(AItem1)^.Y = PdxfPoint(AItem2)^.Y) then
    Result := 0
  else
    Result := -1;
end;

function CopyWideChars(var AStr1, AStr2: WideString;
  const AIsEqual: Boolean): WideString;
var
  I, vLen: Integer;
begin
  Result := '';
  for I := 1 to Length(AStr1) do
  begin
    if AIsEqual xor (AStr1[I] = AStr2[I]) then
      Break
    else
      Result := Result + AStr1[I];
  end;
  vLen := Length(Result);
  if vLen > 0 then
  begin
    Delete(AStr1, 1, vLen);
    Delete(AStr2, 2, vLen);
  end;
end;


function ConvertToAnsiString(const AText: WideString;
  const ACodePage: Integer): sgRawByteString;
{$IFNDEF SGFPC}
var
  vSymbolCount: Integer;
begin
  Result := '';
  vSymbolCount := LocaleCharsFromUnicode(ACodePage, 0, PWideChar(AText),
    Length(AText), nil, 0, nil, nil);
  if vSymbolCount > 0 then
  begin
    SetLength(Result, vSymbolCount);
    LocaleCharsFromUnicode(ACodePage, 0, PWideChar(AText), Length(AText),
      PAnsiChar(Result), vSymbolCount, nil, nil);
  end;
end;
{$ELSE}
begin
  Result := AText;
end;
{$ENDIF}

function AddLastSlash(const S: string; Slash: string = ''): string;
begin
  Result := S;
  if Result <> '' then
  begin
    if Length(Slash) = 0 then
      Slash := cnstDirectorySeparator;
    if (Slash <> '') and (Copy(Result, Length(Result) - Length(Slash) + 1, Length(Slash)) <> Slash) then
      Result := Result + Slash;
  end;
end;

procedure CheckLastSlash(var AStr: string);
begin
  AStr := AddLastSlash(AStr);
end;

function CreateFolders(const AFilePath: string): Integer;
var
  I: Integer;
  vNames: TsgStringList;
  vDir: string;
begin
  if DirectoryExists(AFilePath) then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
  vNames := TsgStringList.Create;
  try
    vNames.LineBreak := cnstDirectorySeparator;
    vNames.Text := AFilePath;
    if vNames.Count > 0 then
    begin
      vDir := vNames[0];
      for I := 1 to vNames.Count - 1 do
      begin
        vDir := vDir + cnstDirectorySeparator + vNames[I];
        if not DirectoryExists(vDir) then
        begin
          try
            if not CreateDir(vDir) then
            begin
              Result := -1;
              Break;
            end;
          except
            Result := -2;
         end;
        end;
      end;
    end;
  finally
    vNames.Free;
    if (Result = 1) and DirectoryExists(AFilePath) then
      Result := 0;
  end;
end;

procedure CalcDistanceParameters(AP1,AP2: TFPoint; ADimScale: Double;
  var ADistance,AXYAngle,AZAngle: Double; var ADelta: TFPoint);
var
  vDen: Double;
begin
  ADistance := DistanceFPoint(AP1, AP2) * ADimScale;
  ADelta := PtXScalar(SubFPoint(AP2, AP1), ADimScale);

  if ADelta.X <> 0 then
  begin
    AXYAngle := 180 / Pi * ArcTan2(ADelta.Y, ADelta.X);
    if AXYAngle < 0 then
      AXYAngle := 360 + AXYAngle;
  end
  else
    if AP2.Y > AP1.Y then
      AXYAngle := 90
    else
      AXYAngle := 270;

  vDen := Sqrt(Sqr(ADelta.X) + Sqr(ADelta.Y) + Sqr(ADelta.Z));
  AZAngle := 0;

  if Abs(vDen) > fDimAccuracy then
  begin
    AZAngle := 180 / Pi * ArcSin(Abs(ADelta.Z) / vDen);
    if ADelta.Z < 0 then
      AZAngle := 360 - AZAngle;
  end;
end;

function CaptionToCommand(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S)do
    if not CharInSet(S[I], [' ', '.', '&', ':', '(', ')', '+', '-', '''', '"']) then
      Result := Result + S[I];
end;

function CompareCommand(const ACmd,AEnCaption,ACaption: string): Boolean;
var
  vEnCmdName,vCmdName: string;
begin
  vEnCmdName := CaptionToCommand(AEnCaption);
  vCmdName := CaptionToCommand(ACaption);
  Result := sgSameText(ACmd, vEnCmdName) or sgSameText(ACmd, vCmdName);
end;

function ConvertIntegerToPlotLayoutFlags(const AValue: Integer): TsgPlotLayoutFlags;
var
  I: TsgPlotLayoutFlag;
  vBitTest: Integer;
begin
  vBitTest := $1;
  Result := [];
  for I := Low(TsgPlotLayoutFlag) to High(TsgPlotLayoutFlag) do
  begin
    if AValue and vBitTest <> 0 then
      Result := Result + [I];
    vBitTest := vBitTest shl 1;
  end;
end;

function ConvertPlotLayoutFlagsToInteger(const AValue: TsgPlotLayoutFlags): Integer;
var
  I: TsgPlotLayoutFlag;
  vBitTest: Integer;
begin
  vBitTest := $1;
  Result := 0;
  for I := Low(TsgPlotLayoutFlag) to High(TsgPlotLayoutFlag) do
  begin
    if I in AValue then
      Result := Result or vBitTest;
    vBitTest := vBitTest shl 1;
  end;
end;

function ConvertToDXFUnicode(const AText: WideString): sgRawByteString;
begin
  Result := sgRawByteString(sgUnicodeToDXFUnicode(AText, True));
end;

function ConvertToRawString(const AText: WideString;
  const ACurrentCodePage: Integer): sgRawByteString;
var
  vStr: sgRawByteString;
  vUnicode, vUnicodeCopy, vUnicodeTest: WideString;
begin
  Result := ConvertToAnsiString(AText, ACurrentCodePage);
  vUnicodeTest := ConvertToWideString(Result, ACurrentCodePage);
  if (Length(AText) = Length(vUnicodeTest)) and (AText <> vUnicodeTest) then
  begin
    vStr := '';
    vUnicode := AText;
    while Length(vUnicode) > 0 do
    begin
      vUnicodeCopy := CopyWideChars(vUnicode, vUnicodeTest, True);
      if Length(vUnicodeCopy) > 0 then
        vStr := vStr + ConvertToAnsiString(vUnicodeCopy, ACurrentCodePage);
      vUnicodeCopy := CopyWideChars(vUnicode, vUnicodeTest, False);
      if Length(vUnicodeCopy) > 0 then
        vStr := vStr + ConvertToDXFUnicode(vUnicodeCopy);
    end;
    if Length(vStr) > 0 then
      Result := vStr;
  end;
end;

function ConvertToWideChar(const AHi, ALow: AnsiChar): WideChar; overload;
type
  TMBytePair = packed record
    L, H: AnsiChar;
  end;
begin
  TMBytePair(Result).L := ALow;
  TMBytePair(Result).H := AHi;
end;

function ConvertToWideChar(const AHi, ALow: WideChar): WideChar; overload;
begin
  Result := ConvertToWideChar(AnsiChar(AHi), AnsiChar(ALow));
end;

function ConvertToWideChar(const AChars: sgRawByteString): WideChar; overload;
begin
  case Length(AChars) of
    0: Result := WideChar(0);
    1: Result := ConvertToWideChar(Char(0), AChars[1])
  else
    Result := ConvertToWideChar(AChars[1], AChars[2]);
  end;
end;

{$IFNDEF SGDEL_XE}

function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: {$IFNDEF SG_NON_WIN_PLATFORM}PBOOL{$ELSE}PInteger{$ENDIF}): Integer;
{$IFDEF HAS_FEATURE_ENCODING}
var
  Encoding: TEncoding;
  Chars: {$IFDEF UNICODE}TCharArray{$ELSE}TUnicodeCharArray{$ENDIF};
  Bytes: TBytes;
begin
  Encoding := TEncoding.GetEncoding(CodePage);
  try
    if UnicodeStrLen = -1 then UnicodeStrLen := StrLen(UnicodeStr);
    SetLength(Chars, UnicodeStrLen);
    System.Move(UnicodeStr^, Chars[0], UnicodeStrLen * SizeOf(Chars[0]));
    Bytes := Encoding.GetBytes(Chars);
    Result := MinI(Length(Bytes), LocaleStrLen);
    System.Move(Bytes[0], LocaleStr^, Result);
    if Assigned(UsedDefaultChar) then
      UsedDefaultChar^ := {$IFNDEF SG_NON_WIN_PLATFORM}False{$ELSE}0{$ENDIF};
  finally
    if not TEncoding.IsStandardEncoding(Encoding) then
      Encoding.Free;
  end;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
begin
  Result := WideCharToMultiByte(CodePage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr,
    LocaleStrLen, DefaultChar, PBOOL(UsedDefaultChar));
end;
{$ENDIF}
{$ENDIF}

function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer;
{$IFDEF HAS_FEATURE_ENCODING}
var
  Encoding: TEncoding;
  Chars: {$IFDEF UNICODE}TCharArray{$ELSE}TUnicodeCharArray{$ENDIF};
  Bytes: TBytes;
begin
  Encoding := TEncoding.GetEncoding(CodePage);
  try
    if LocaleStrLen = -1 then LocaleStrLen := StrLen(LocaleStr);
    SetLength(Bytes, LocaleStrLen);
    System.Move(LocaleStr^, Bytes[0], LocaleStrLen);
    Chars := Encoding.GetChars(Bytes, 0, LocaleStrLen);
    Result := MinI(Length(Chars), UnicodeStrLen);
    System.Move(Chars[0], UnicodeStr^, Result * SizeOf(Chars[0]));
  finally
    if not TEncoding.IsStandardEncoding(Encoding) then
      Encoding.Free;
  end;
end;
{$ELSE}
begin
  Result := MultiByteToWideChar(CodePage, Flags, LocaleStr, LocaleStrLen,
    UnicodeStr, UnicodeStrLen);
end;
{$ENDIF}

{$ENDIF}

function ConvertToWideString(const AText: AnsiString;
  const ACodePage: Integer): WideString;
{$IFNDEF SGFPC}
var
  vSymbolCount: Integer;
begin
  Result := '';
  vSymbolCount := UnicodeFromLocaleChars(ACodePage, 0, PAnsiChar(AText),
    Length(AText), nil, 0);
  if vSymbolCount <> 0 then
  begin
    SetLength(Result, vSymbolCount);
    if UnicodeFromLocaleChars(ACodePage, 0, PAnsiChar(AText), Length(AText),
      PWideChar(Result), vSymbolCount) = 0 then
      Result := '';
  end;
end;
{$ELSE}
begin
  Result := AText;
end;
{$ENDIF}

function ConvertToUtf8(const AText: WideString): sgRawByteString;
{$IFNDEF SGFPC}
var
  vSymbolCount: Integer;
begin
  Result := '';
  vSymbolCount := LocaleCharsFromUnicode(CP_UTF8, 0, PWideChar(AText), Length(AText), nil, 0, nil, nil);
  if vSymbolCount <> 0 then
  begin
    SetLength(Result, vSymbolCount);
    if LocaleCharsFromUnicode(CP_UTF8, 0, PWideChar(AText), Length(AText), PAnsiChar(Result), vSymbolCount, nil, nil) = 0 then
      Result := '';
  end;
end;
{$ELSE}
begin
  Result := AText;
end;
{$ENDIF}

{
sgRawStringToDXFUnicode - ANSI conversion function in Unicode DXF (\ U + XXXX)
AStr - raw string
ACodePage - code page which presents the raw string
AMode:
    0 - to convert to DXF Unicode (\ U + XXXX)
    1 - to convert to the current code page
    2 - Convert the current code page or DXF Unicode (\ U + XXXX)
ACurrentCodePage - current code page
}
function sgRawStringToDXFUnicode(const AStr: sgRawByteString;
  const ACodePage: Integer; const AMode: Byte;
  const ACurrentCodePage: Integer): sgRawByteString;
var
  vStr: sgRawByteString;
  vUnicode: WideString;
begin
  Result := AStr;
  if Length(AStr) > 0 then
  begin
    vUnicode := ConvertToWideString(AStr, ACodePage);
    if Length(vUnicode) > 0 then
    begin
      case AMode of
        0:   Result := ConvertToDXFUnicode(vUnicode);
        1:   Result := ConvertToAnsiString(vUnicode, ACurrentCodePage);
      else
        vStr := ConvertToRawString(vUnicode, ACurrentCodePage);
        if Length(vStr) > 0 then
          Result := vStr;
      end;
    end;
  end;
end;

function sgUnicodeToDXFUnicode(const AStr: WideString; const Check: Boolean = False): string;
var
  I, vLen: Integer;
  vChar: string;
begin
  Result := '';
  vLen := Length(AStr);
  for I := 1 to vLen do
  begin
    if Check and (Cardinal(AStr[I]) < 128) then
      vChar := AStr[I]
    else
      vChar := cnstSymbolUnicode + IntToHex(Integer(AStr[I]), 4);
    Result := Result + vChar;
  end;
end;

function Utf8ToUni(ADst: PWideChar; AMaxDstLen: DWORD; ASrc: PChar;
  ASrcLen: DWORD): DWORD;
var
  I, Cnt: DWORD;
  vChar: Byte;
  vUnicode: DWORD;
begin
  if ASrc = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := $FFFFFFFF;
  Cnt := 0;
  I := 0;
  if ADst <> nil then
  begin
    while (I < ASrcLen) and (Cnt < AMaxDstLen) do
    begin
      vUnicode := DWORD(ASrc[I]);
      Inc(I);
      if (vUnicode and $80) <> 0 then
      begin
        vUnicode := vUnicode and $3F;
        if I > ASrcLen then
          Exit;
        if (vUnicode and $20) <> 0 then
        begin
          vChar := Byte(ASrc[I]);
          Inc(I);
          if ((vChar and $C0) <> $80) or (I > ASrcLen) then
            Exit;
          vUnicode := (vUnicode shl 6) or (vChar and $3F);
        end;
        vChar := Byte(ASrc[I]);
        Inc(I);
        if (vChar and $C0) <> $80 then
          Exit;
        ADst[Cnt] := WideChar((vUnicode shl 6) or (vChar and $3F));
      end
      else
        ADst[Cnt] := WideChar(vUnicode);
      Inc(Cnt);
    end;
    if Cnt >= AMaxDstLen then
      Cnt := AMaxDstLen - 1;
    ADst[Cnt] := #0;
  end
  else
  begin
    while (I <= ASrcLen) do
    begin
      vChar := Byte(ASrc[I]);
      Inc(I);
      if (vChar and $80) <> 0 then
      begin
        if ((vChar and $F0) = $F0) or ((vChar and $40) = 0) or (I > ASrcLen) or
          ((Byte(ASrc[I]) and $C0) <> $80) then
          Exit;
        Inc(I);
        if (I > ASrcLen) or (((vChar and $20) <> 0) and
          ((Byte(ASrc[I]) and $C0) <> $80)) then
          Exit;
        Inc(I);
      end;
      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;
end;

function sgUTF8ToUnicode(const S: TsgUTF8String): WideString;
var
  Len: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));
  Len := Utf8ToUni(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if Len > 0 then
    SetLength(Temp, Len - 1)
  else
    Temp := '';
  Result := Temp;
end;

function ConvToFloatDef(const AStr: string; const AValue: Extended): Extended;
var
  E: Integer;
begin
  Val(AStr, Result, E);
  if E <> 0 then
  begin
    Dec(E);
    Val(Copy(AStr, 1, E), Result, E);
    if (E <> 0) and (Result = 0) then
      Result := AValue;
  end;
end;

function CalcScaleFactor(const AInitial, ADisplayed: TsgInsUnits): Double;
var
  vDispKoef: Double;
  vInitialKoef: Double;
begin
  Result := 1;
  vDispKoef := cnstMeasureUnitsKoef[ADisplayed];
  vInitialKoef := cnstMeasureUnitsKoef[AInitial];
  if (vInitialKoef > 0) and (vDispKoef > 0) and (not sgIsZero(vInitialKoef)) then
  begin
    Result := vDispKoef / vInitialKoef;
  end;
end;

procedure CheckAngles(var AStart, AEnd: Double);
begin
  if AStart < 0 then
    AStart := 360 + AStart;
  if AEnd < 0 then
    AEnd := 360 + AEnd;
  AStart := sgMod(AStart, 360);
  AEnd := sgMod(AEnd, 360);
  if AStart = AEnd then
    AEnd := AStart + 360;
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function CheckWin32Version(AMajor: Integer; AMinor: Integer = 0): Boolean;
begin
  Result := (Win32MajorVersion > AMajor) or
            ((Win32MajorVersion = AMajor) and
             (Win32MinorVersion >= AMinor));
end;
{$ENDIF}

function Degree(ARadian: Extended): Extended;
begin
  Result := ARadian * f180DividedByPi;
end;

function FindAutoCADSHXPaths(APaths: TStringList): Boolean;
{$IFNDEF SG_NON_WIN_PLATFORM}
const
  ACADRegPath = 'Software\Autodesk\AutoCAD';
var
  S, vRegPath, vDir: string;
  vStrLst: array[0..3] of TStringList;
  I, J, K, vPos: Integer;

  function GetKeyNames(ANameLst: TStrings; const AValName: string;
    var AValue: string): Boolean;
  var
    vReg: TRegistry;
  begin
    Result := False;
    ANameLst.Clear;
    vReg := TRegistry.Create;
    try
      vReg.RootKey := HKEY_CURRENT_USER;
      if vReg.OpenKey(vRegPath,False) then
      begin
        Result := True;
        vReg.GetKeyNames(ANameLst);
        if (AValName <> '') and vReg.ValueExists(AValName) then
          AValue := vReg.ReadString(AValName)
        else
          AValue := '';
      end;
    finally
      vReg.Free;
    end;
  end;

  function IsSHXInFolder(const AFolder: string): Boolean;
  var
    vSr: TSearchRec;
    S: string;
    vLen: Integer;
  begin
    S := AFolder;
    vLen := Length(S);
    if vLen > 0 then
      if S[vLen] <> '\' then S := S + '\';
    Result := FindFirst(S + '*.shx', faAnyFile, vSr) = 0;
    FindClose(vSr);
  end;

begin
  Result := False;
  if not Assigned(APaths) then Exit;
  for I := 0 to 3 do
    vStrLst[I] := TStringList.Create;
  try
    vRegPath := ACADRegPath;
    GetKeyNames(vStrLst[0], '', S);
    for I := 0 to vStrLst[0].Count - 1 do
    begin
      vRegPath := ACADRegPath + '\' + vStrLst[0][I];
      GetKeyNames(vStrLst[1], '', S);
      for J := 0 to vStrLst[1].Count - 1 do
      begin
        vRegPath := vRegPath + '\' + vStrLst[1][J] + '\Profiles';
        GetKeyNames(vStrLst[2], '', S);
        for K := 0 to vStrLst[2].Count - 1 do
        begin
          vRegPath := vRegPath + '\' + vStrLst[2][K] + '\General';
          GetKeyNames(vStrLst[3], 'ACAD', S);
          while S <> '' do
          begin
            vPos := AnsiPos(';', S);
            if vPos = 0 then Break;
            vDir := Copy(S, 1, vPos - 1);
            if IsSHXInFolder(vDir) then APaths.Add(vDir);
            Delete(S, 1, vPos);
          end;
        end;
      end;
    end;
  finally
    for I := 0 to 3 do
      vStrLst[I].Free;
  end;
  Result := APaths.Count > 0;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function FindDWGTrueViewSHXPaths(APaths: TStringList): Boolean; overload;
begin
 {$IFNDEF SG_NON_WIN_PLATFORM}
  if CheckWin32Version(5, 1) then // Windows XP or higher
  begin
    Result := FindDWGTrueViewSHXPaths(APaths, rax86);
    Result := FindDWGTrueViewSHXPaths(APaths, rax64) or Result;
  end
  else
    Result := FindDWGTrueViewSHXPaths(APaths, raNone);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function FindDWGTrueViewSHXPaths(APaths: TStringList; ARegistryAccess: TsgRegistryAccess): Boolean;
{$IFNDEF SG_NON_WIN_PLATFORM}
const
  DWGTrueViewPath = 'Software\Autodesk\DWG TrueView';
  sRegAppDataSupportValueName = 'RoamableRootFolder';
  sLocation = 'Location';
  sSupport = 'Support';
  sFonts = 'Fonts';
var
  I, J: Integer;
  vReg: TRegistryIniFile;
  vSections: array[0 .. 1] of TStringList;
  S, vPath: string;

  function CorrectPath(const APath: string): string;
  var
    Len: Integer;
  begin
    Result := APath;
    Len := Length(Result);
    if Len > 0 then
      if Result[Len] <> '\' then Result := Result + '\';
  end;

  function IsSHXInFolder(const AFolder: string): Boolean;
  var
    vSr: TSearchRec;
  begin
    Result := FindFirst(CorrectPath(AFolder) + '*.shx', faAnyFile, vSr) = 0;
    SysUtils.FindClose(vSr);
  end;

{$IFNDEF SGDEL_2005}
  procedure ReadSectionValues(ARegIniFile: TRegIniFile; const Section: string; Strings: TStrings);
  var
    KeyList: TStringList;
    I: Integer;
  begin
    KeyList := TStringList.Create;
    try
      ARegIniFile.ReadSection(Section, KeyList);
      Strings.BeginUpdate;
      try
        for I := 0 to KeyList.Count - 1 do
          Strings.Values[KeyList[I]] := ARegIniFile.ReadString(Section, KeyList[I], '');
      finally
        Strings.EndUpdate;
      end;
    finally
      KeyList.Free;
    end;
  end;
{$ENDIF}

begin
  Result := False;
  if APaths <> nil then
  begin
    vReg := TRegistryIniFile.Create('');
    try
      if CheckWin32Version(5, 1) then // Windows XP or higher
      begin
{$IFDEF SGDEL_6}
        if ARegistryAccess = rax86 then
          vReg.RegIniFile.Access := vReg.RegIniFile.Access or KEY_WOW64_32KEY;
        if ARegistryAccess = rax64 then
          vReg.RegIniFile.Access := vReg.RegIniFile.Access or KEY_WOW64_64KEY;
{$ENDIF}
      end;
      for I := Low(vSections) to High(vSections) do
        vSections[I] := TStringList.Create;
      try
{$IFDEF SGDEL_2005}
        vReg.ReadSections(DWGTrueViewPath, vSections[0]);
{$ELSE}
        ReadSectionValues(vReg.RegIniFile, DWGTrueViewPath, vSections[0]);
{$ENDIF}
        for I := 0 to vSections[0].Count - 1 do
        begin
{$IFDEF SGDEL_2005}
          vReg.ReadSections(DWGTrueViewPath + '\' + vSections[0][I], vSections[1]);
{$ELSE}
          ReadSectionValues(vReg.RegIniFile, DWGTrueViewPath + '\' + vSections[0][I], vSections[1]);
{$ENDIF}
          J := vSections[1].Count;
          while (J > 0) and (vSections[1].Count > 0) do
          begin
            if Pos('DWGVIEWR', AnsiUpperCase(vSections[1][J-1])) = 1 then
              Dec(J)
            else vSections[1].Delete(J-1);
          end;
          for J := 0 to vSections[1].Count - 1 do
          begin
            S := DWGTrueViewPath + '\' + vSections[0][I] + '\' + vSections[1][J];
            vPath := vReg.ReadString(S, sRegAppDataSupportValueName, '');
            if vPath <> '' then
            begin
              vPath := CorrectPath(vPath) + sSupport;
              if IsSHXInFolder(vPath) and (APaths.IndexOf(vPath) = -1) then
                APaths.Add(vPath);
            end;
            vReg.RegIniFile.RootKey := HKEY_LOCAL_MACHINE;
            try
              vPath := vReg.ReadString(S, sLocation, '');
              if vPath <> '' then
              begin
                vPath := CorrectPath(vPath) + sFonts;
                if IsSHXInFolder(vPath) and (APaths.IndexOf(vPath) = -1) then
                  APaths.Add(vPath);
              end;
            finally
              vReg.RegIniFile.RootKey := HKEY_CURRENT_USER;
            end;
          end;
        end;
        Result := APaths.Count > 0;
      finally
        for I := Low(vSections) to High(vSections) do
          vSections[I].Free;
      end;
    finally
      vReg.Free;
    end;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function FindSGSHXPaths(const APaths: TStringList): Boolean;
{$IFNDEF SG_NON_WIN_PLATFORM}
const
  sRegSHXPaths = cnstResources + '\' + cnstSHXPath;
var
  I: Integer;
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  S: string;
{$ENDIF}
  vPaths: TsgStringList;
  vReg: TRegistry;
  vSr: TSearchRec;
begin
  Result := False;
  vPaths := TsgStringList.Create;
  try
    vPaths.Sorted := True;
    vPaths.LineBreak := ';';
    vReg := TRegistry.Create;
    try
      if vReg.OpenKey(cnstSoftwareSoftGold + sRegSHXPaths, False) then
      try
        vPaths.Text := vReg.ReadString('');
      finally
        vReg.CloseKey;
      end;
    finally
      vReg.Free;
    end;
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF SG_CADIMPORTERDLLDEMO}
    if sgShellAPI.GetSpecialFolderPath(CSIDL_APPDATA, S) then
    begin
      S := S + cnstCompanyCST + '\' + sRegSHXPaths;
      vPaths.Add(S);
    end;
{$ENDIF}
{$ENDIF}
    for I := 0 to vPaths.Count - 1 do
      if APaths.IndexOf(vPaths[I]) < 0 then
      begin
        Result := FindFirst(vPaths[I] + '\*.shx', faAnyFile, vSr) = 0;
        FindClose(vSr);
        if Result then
          APaths.Add(vPaths[I]);
      end;
  finally
    vPaths.Free;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function FindSHXPaths(const APaths: TStringList): Boolean;
begin
  Result := FindSGSHXPaths(APaths);
  Result := FindAutoCADSHXPaths(APaths) or Result;
  Result := FindDWGTrueViewSHXPaths(APaths) or Result;
end;

function GetAccuracy(const ALength: Double; const AResolution: Double = fDoubleResolution): Double;
begin
  Result := Max(ALength * AResolution, AResolution);
end;

function GetAccuracyByPoint(const APoint: TFPoint; const AResolution: Double = fDoubleResolution): Double;
var
  vMax: Double;
begin
  vMax := Max(Abs(APoint.X * AResolution), AResolution);
  Result := Max(Abs(APoint.Y * AResolution), vMax);
end;


function GetAccuracyByFPoints(const APoint1, APoint2: TFPoint;
  const AResolution: Double = fDoubleResolution): Double;
begin
  Result := GetAccuracy(DistanceFPoint(APoint1, APoint2), AResolution);
end;

function GetAccuracyByLine(const ALine: TsgLine;
  const AResolution: Double = fDoubleResolution): Double;
begin
  Result := GetAccuracyByFPoints(ALine.Point1, ALine.Point2, AResolution);
end;

function GetAccuracyByBox(const ABox: TFRect;
  const AResolution: Double = fDoubleResolution): Double;
begin
  Result := GetAccuracyByFPoints(ABox.TopLeft, ABox.BottomRight, AResolution)
end;

function GetAccuracyByBox(const ABox: PFRect;
  const AResolution: Double = fDoubleResolution): Double;
begin
  if Assigned(ABox) then
    Result := GetAccuracyByBox(ABox^, AResolution)
  else
    Result := AResolution;
end;

function GetArrowTypeByName(AName: string; const DefValue: TsgDimArrowType): TsgDimArrowType;
const
  cnstBlockPrefix = '_';
var
  I: TsgDimArrowType;
begin
  Result := DefValue;
  if Length(AName) > 0 then
  begin
    Result := datUserarrow;
    AName := AnsiUpperCase(AName);
    if AName[1] <> cnstBlockPrefix then
      AName := cnstBlockPrefix + AName;
    for I := Low(sgDimensionArrowTypeNames) to High(sgDimensionArrowTypeNames) do
      if AName = sgDimensionArrowTypeNames[I] then
      begin
        Result := I;
        Break;
      end;
  end;
end;


//bti functions begin
function GetBTITypeByEntTypeEx(const ATypeEx: Integer): TbtiEntity;
begin
  case ATypeEx of
    cnstDimConstruction: Result := btiDimConstruction;
    cnstConstruction:    Result := btiConstruction;
    cnstComplex:         Result := btiComplex;
    cnstElement:         Result := btiElement;
    cnstComplexBroad:    Result := btiComplexBroad;
    cnstComplexLinear:   Result := btiComplexLinear;
    cnstElementBroad:    Result := btiElementBroad;
    cnstElementLinear:   Result := btiElementLinear;
    cnstElementCarved:   Result := btiElementCarved;
    cnstElementModifier: Result := btiElementModifier;
    cnstArea:            Result := btiArea;
    cnstComplexArea:     Result := btiComplexArea;
    cnstLabel:           Result := btiLabel;
    cnstDimLabel:        Result := btiDimLabel;
    cnstBlockPattern:    Result := btiBlockPattern;
  else
    Result := btiUndefined;
  end;
end;

function GetBTITypeByCADEntType(const AType: TsgCADEntityType): TbtiEntity;
begin
  case AType of
    etConstruction:     Result := btiConstruction;
    etDimConstruction:  Result := btiDimConstruction;
    etComplex:          Result := btiComplex;
    etElement:          Result := btiElement;
    etComplexBroad:     Result := btiComplexBroad;
    etComplexLinear:    Result := btiComplexLinear;
    etElementBroad:     Result := btiElementBroad;
    etElementLinear:    Result := btiElementLinear;
    etElementCarved:    Result := btiElementCarved;
    etElementModifier:  Result := btiElementModifier;
    etArea:             Result := btiArea;
    etComplexArea:      Result := btiComplexArea;
  else
    Result := btiUndefined;
  end;
end;

function GetEntTypeExByBTIType(const AType: TbtiEntity): Integer;
begin
  case AType of
    btiConstruction:     Result := cnstConstruction;
    btiDimConstruction:  Result := cnstDimConstruction;
    btiComplex:          Result := cnstComplex;
    btiElement:          Result := cnstElement;
    btiComplexBroad:     Result := cnstComplexBroad;
    btiComplexLinear:    Result := cnstComplexLinear;
    btiElementBroad:     Result := cnstElementBroad;
    btiElementLinear:    Result := cnstElementLinear;
    btiElementCarved:    Result := cnstElementCarved;
    btiElementModifier:  Result := cnstElementModifier;
    btiArea:             Result := cnstArea;
    btiComplexArea:      Result := cnstComplexArea;
    btiLabel:            Result := cnstLabel;
    btiDimLabel:         Result := cnstDimLabel;
    btiBlockPattern:     Result := cnstBlockPattern;
  else
    Result := cnstUndefined;
  end;
end;

function GetTypeExByCADEntType(const AType: TsgCADEntityType): Integer;
begin
  Result := GetEntTypeExByBTIType(GetBTITypeByCADEntType(AType));
end;

function GetCADEntTypeByBTIType(const AType: TbtiEntity): TsgCADEntityType;
begin
  case AType of
    btiConstruction:     Result := etConstruction;
    btiDimConstruction:  Result := etDimConstruction;
    btiComplex:          Result := etComplex;
    btiElement:          Result := etElement;
    btiComplexBroad:     Result := etComplexBroad;
    btiComplexLinear:    Result := etComplexLinear;
    btiElementBroad:     Result := etElementBroad;
    btiElementLinear:    Result := etElementLinear;
    btiElementCarved:    Result := etElementCarved;
    btiElementModifier:  Result := etElementModifier;
    btiArea:             Result := etArea;
    btiComplexArea:      Result := etComplexArea;
  else
    Result := etNone;
  end;
end;

function GetCADEntTypeByTypeEx(const ATypeEx: Integer): TsgCADEntityType;
begin
  Result := GetCADEntTypeByBTIType(GetBTITypeByEntTypeEx(ATypeEx));
end;

function GetBlockShortNameByBTIType(const AType: TbtiEntity): string;
begin
  Result := GetBlockShortNameByEntTypeEx(GetEntTypeExByBTIType(AType));
end;

function GetBlockShortNameByEntTypeEx(const AType: Integer): string;
begin
  case AType of
    cnstConstruction:     Result := 'B01';
    cnstDimConstruction:  Result := 'B02';
    cnstBlockPattern:     Result := 'B03';
    cnstComplex:          Result := 'K01';
    cnstComplexBroad:     Result := 'K02';
    cnstComplexLinear:    Result := 'K03';
    cnstElement:          Result := 'H01';
    cnstElementBroad:     Result := 'H02';
    cnstElementCarved:    Result := 'H03';
    cnstElementLinear:    Result := 'H04';
    cnstElementModifier:  Result := 'H05';
    cnstArea:             Result := 'F01';
    cnstComplexArea:      Result := 'F02';
    cnstLabel:            Result := 'L01';
    cnstDimLabel:         Result := 'L02';
  else
    Result := cnstDefBlockNameSymbol;
  end;
  Result := cnstAsterisk + Result;
end;

function GetNameAct(const AName: string;
  const AHasAsterisk: Boolean = True): string;
begin
  if AHasAsterisk then
    Result := Copy(AName, 1, cnstBlockShortName)
  else
    Result := cnstAsterisk + Copy(AName, 1, cnstBlockShortName - 1);
end;

function GetEntTypeExByBlockShortName(const AName: string;
  const AHasAsterisk: Boolean = True): Integer;
var
  I: Integer;
  vName, vShortName: string;
begin
  Result := cnstUndefined;
  if Length(AName) > cnstBlockShortName - Integer(not AHasAsterisk) then
  begin
    vName := GetNameAct(AName, AHasAsterisk);
    for I := Low(cnstBTIEntTypeEx) to High(cnstBTIEntTypeEx) do
    begin
      vShortName := GetBlockShortNameByEntTypeEx(cnstBTIEntTypeEx[I]);
      if vName = vShortName then
      begin
        Result := cnstBTIEntTypeEx[I];
        Break;
      end;
    end;
  end;
end;

function IsBTIBlockNameOld(const AName: string): Boolean;
const
  cnstBTIBlockName: array [0..5] of string = ('*BCB', '*BEA', 'BEB', '*BCA', 'BCL', '*L_');
var
  I, vFind: Integer;
  vName: string;
begin
  vFind := 0;
  Result := False;
  vName := AName;
  if Length(vName) > 4 then
    SetLength(vName, 4);
  vName := UpperCase(vName);
  for I := Low(cnstBTIBlockName) to High(cnstBTIBlockName) do
    if Pos(cnstBTIBlockName[I], vName) > 0 then
      Inc(vFind);
  if vFind > 0 then
    Result := True;
end;

function IsBTIBlockName(const AName: string;
  const AHasAsterisk: Boolean = True): Boolean;
var
  vEntTypeEx: Integer;
begin
  Result := False;
  vEntTypeEx := GetEntTypeExByBlockShortName(AName, AHasAsterisk);
  if vEntTypeEx <> cnstUndefined then
    Result := True;
end;

function IsBTIBlockInternal(const AName: string): Boolean;
var
  vName: string;
begin
  Result := False;
  if (AName = 'ABViewer_RedLine') then
    Result := True
  else
  begin
    if Length(AName) >= Length(cnstBTIReservedBlock) then
    begin
      vName := AName;
      if vName[1] <> cnstAsterisk then
        vName := cnstAsterisk + vName;
      SetLength(vName, Length(cnstBTIReservedBlock));
      if vName = cnstBTIReservedBlock then
        Result := True;
    end;
  end;
end;

function CreateStringsValues(const AValues: string = '';
  const AValuesDelimiter: Char = cnstEnumValuesDelimiter): TStrings;
begin
  Result := TsgStringList.Create;
  TsgStringList(Result).LineBreak := AValuesDelimiter;
  TsgStringList(Result).Text := AValues;
end;

function BTITypesToStr(const ATypes: TbtiEntities): string;
var
  vBTIType: TbtiEntity;
   vValues: TStrings;
begin
  Result := '';
  vValues := CreateStringsValues;
  try
    for vBTIType := btiConstruction to High(TbtiEntity) do
    begin
      if vBTIType in ATypes then
        vValues.Add(IntToStr(GetEntTypeExByBTIType(vBTIType)));
    end;
    if vValues.Count > 0 then
      Result := vValues.Text;
  finally
    vValues.Free;
  end;
end;

function StrToBTITypes(const AStr: string): TbtiEntities;
var
  I, vTypeEx: Integer;
  vValues: TStrings;
  vBTIType: TbtiEntity;
begin
  Result := [];
  vValues := CreateStringsValues(AStr);
  try
    for I := 0 to vValues.Count - 1 do
    begin
      vTypeEx := StrToIntDef(vValues[I], cnstUndefined);
      vBTIType := GetBTITypeByEntTypeEx(vTypeEx);
      if vBTIType <> btiUndefined then
        Include(Result, vBTIType);
    end;
  finally
    vValues.Free;
  end;
end;

//bti functions end

function GetBoxType(const ABox: TFRect; const Resolution: Double = fDoubleResolution): TsgBoxType;
var
  vSize: TFPoint;
begin
  vSize := SubFPoint(ABox.BottomRight, ABox.TopLeft);
  Result := GetBoxType(vSize, Resolution);
end;

function GetBoxType(const ASize: TFPoint; const Resolution: Double = fDoubleResolution): TsgBoxType; overload;
var
  vSpace: Integer;
begin
  vSpace := 0;
  if not IsZero(ASize.X, Resolution) then
    vSpace := 1;
  if not IsZero(ASize.Y, Resolution) then
    Inc(vSpace, 2);
  if not IsZero(ASize.Z, Resolution) then
    Inc(vSpace, 4);
  case vSpace of
    1:  Result := bxX;
    2:  Result := bxY;
    4:  Result := bxZ;
    3:  Result := bxXY;
    5:  Result := bxXZ;
    6:  Result := bxYZ;
    7:  Result := bxXYZ;
  else
    Result := bxEmpty;
  end;
end;

function GetMatrixDirection(const AMatrix: TFMatrix): TPoint;

  procedure SetDirection(const AZero, AAxis: TFPoint;
    const AMat: TFMatrix; var ADirection: Integer);
  var
    vPoint, vAxis: TFPoint;
  begin
    vPoint := FPointXMat(AAxis, AMat);
    vAxis := SubFPoint2D(vPoint, AZero);
    if not IsEqualDirection(AAxis, vAxis, cnstFPointZero) then
      ADirection := -1;
  end;

var
  vPointZero: TFPoint;
  vMatrix: TFMatrix;
begin
  Result := cnstPointSingle;
  vMatrix := AMatrix;
  vMatrix.E0 := cnstFPointZero;
  vPointZero := FPointXMat(cnstFPointZero, vMatrix);
  SetDirection(vPointZero, cnstXOrtAxis, vMatrix, Result.X);
  SetDirection(vPointZero, cnstYOrtAxis, vMatrix, Result.Y);
end;

{$IFNDEF SG_USE_SYNAPSE}
{$IFDEF SG_USE_INDY}
type
  TsgVerifyPeer = class
  public
    procedure DoOnAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
    function DoOnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
  end;

function TsgVerifyPeer.DoOnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
begin
  Result := AOk;
  if cnstSSLUseAnyCertificate then
    Result := True;
end;

procedure TsgVerifyPeer.DoOnAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
begin

end;
{$ENDIF}
{$ENDIF}

{$IFDEF SG_USE_SYNAPSE}

function HeaderByName(const AHTTP: THTTPSend; const AHeaderName: string): string;
var
  I, vHeadLength: Integer;
  S, vHeadLineCopy: string;
begin
  Result := EmptyStr;
  vHeadLength := Length(AHeaderName);
  for I := 0 to AHTTP.Headers.Count - 1 do
  begin
    S := AHTTP.Headers[I];
    vHeadLineCopy := Copy(S, 1, vHeadLength);
    if sgSameText(AHeaderName, vHeadLineCopy) then
    begin
      Result := Copy(S, vHeadLength + 3, Length(S) - 1);
      Break;
    end;
  end;
end;

function GetTargetURL(const AHTTP: THTTPSend; const AStartURL: string;
  const ARedirectMaximum: Integer = cnstMaxHttpRedirects): string;
var
  vRedirectCount: integer;
  vLocationURL: string;
begin
  vRedirectCount := 0;
  Result := AStartURL;
  repeat
    if AHTTP.HTTPMethod('HEAD', Result) then
    begin
      vLocationURL := HeaderByName(AHTTP, 'Location');
      if Length(vLocationURL) > 0 then
      begin
        Result := vLocationURL;
        Inc(vRedirectCount);
      end;
    end
    else
      Exit;
  until ((AHTTP.ResultCode <> 301) and (AHTTP.ResultCode <> 302)) or (vRedirectCount < ARedirectMaximum);
end;

function HTTPMethodExt(const AHTTP: THTTPSend; const AMehtod, AURL: string;
  const AHandleRedirects: Boolean; const ARedirectMaximum: Integer): Boolean;
var
  vTargetURL: string;
begin
  if not AHandleRedirects then
    Result := AHTTP.HTTPMethod(AMehtod, AURL)
  else
  begin
    vTargetURL := GetTargetURL(AHTTP, AURL, ARedirectMaximum);
    AHTTP.Headers.Clear;
    Result := AHTTP.HTTPMethod(AMehtod, vTargetURL);
  end;
end;
{$ENDIF}

function GetDataFromHttp(const AHost: string;
  AStream: TStream; ATimeOut: Integer; var AFileName: string;
  var AErrorMessage: string; const AProxyInfo: TsgHTTPProxyInfo;
  AUseProxy: Boolean = False; AHandleRedirects: Boolean = True;
  AGetMethod: Boolean = True;
  const AEvents: PsgDataHttpParams = nil): Boolean;//PsgDataFromHttpEvents

  procedure Prepare(var Host: string);
  begin
    AStream.Size := 0;
    AStream.Position := 0;
    Host := AHost;
    Result := False;
    if Copy(UpperCase(Host), 1, 4) <> UpperCase(sHttp) then
      Host := sHttp + sSlashes + Host;
  end;

  procedure FinalizeSuccess;
  var
    vPos: Integer;
  begin
    AStream.Position := 0;
    AFileName := AHost;
    vPos := Pos(sSlashes, AFileName);
    if vPos > 0 then
      AFileName := Copy(AFileName, vPos + Length(sSlashes), MaxInt);
    ReplaceAnsi(AFileName, sSlash, sBackSlash);
    AFileName := GetValidFileName(ExtractFileName(AFileName));
    Result := True;
  end;

{$IFDEF SGDEL_XE8}
var
  vClient: TNetHTTPClient;
  vLogUser, vLogPass: string;
  vHost, vCurrPath: string;
begin
  Prepare(vHost);
  AErrorMessage := '';
  vClient := TNetHTTPClient.Create(nil);
  try
    if Assigned(HTTPOwner) then HTTPOwner.InsertComponent(vClient);
    if AUseProxy then
    begin
      if AProxyInfo.ProxyNeedPass then
      begin
        vLogUser := AProxyInfo.ProxyUser;
        vLogPass := AProxyInfo.ProxyPass;
      end
      else
      begin
        vLogUser := '';
        vLogPass := '';
      end;
      vClient.ProxySettings := TProxySettings.Create(AProxyInfo.ProxyHost, AProxyInfo.ProxyPort, vLogUser, vLogPass);
    end;
    {$IFDEF SGDEL_10_SYDNEY}
    vClient.ConnectionTimeout := ATimeOut;
    vClient.ResponseTimeout := ATimeOut;
    {$ENDIF}
    vClient.HandleRedirects := AHandleRedirects;
    vClient.MaxRedirects := cnstMaxHttpRedirects;
    {$IFDEF SGDEL_XE8}
    if AEvents <> nil then
    begin
       vClient.OnReceiveData := AEvents.OnReceiveData;
       {$IFDEF SGDEL_10_SYDNEY}
       vClient.OnSendData := AEvents.OnSendData;
       {$ENDIF}
    end;
    {$ENDIF}
    vCurrPath := GetCurrentDir;
    SetCurrentDir(ExtractFilePath(GetFileName));
    try
      if AGetMethod then
        vClient.Get(vHost, AStream)
      else
        vClient.Post(vHost, AStream);
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        Exit;
      end;
    end;
    FinalizeSuccess;
  finally
    vClient.Free;
    if vCurrPath <> '' then
      SetCurrentDir(vCurrPath)
  end;
end;
{$ELSE}
{$IFDEF SG_USE_SYNAPSE}
var
  HTTPClient: THTTPSend;
  vHost: string;
begin
  AErrorMessage := '';
  HTTPClient := THTTPSend.Create;
  try
    Prepare(vHost);
    HTTPClient.Timeout := ATimeOut;
    HTTPClient.KeepAlive := False;
    if AUseProxy then
    begin
      HTTPClient.ProxyHost := AProxyInfo.ProxyHost;
      HTTPClient.ProxyPort := IntToStr(AProxyInfo.ProxyPort);
      if AProxyInfo.ProxyNeedPass then
      begin
        HTTPClient.ProxyUser := AProxyInfo.ProxyUser;
        HTTPClient.ProxyPass := AProxyInfo.ProxyPass;
      end;
    end;
    try
      if HTTPMethodExt(HTTPClient, 'GET', vHost, AHandleRedirects, cnstMaxHttpRedirects) then
      begin
        HTTPClient.Document.Position := 0;
        HTTPClient.Document.SaveToStream(AStream);
        FinalizeSuccess;
      end
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        Exit;
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;
{$ELSE}
{$IFDEF SG_USE_INDY}
  procedure FreeAll(var AIdHTTP: TIdHTTP; {$IFDEF SG_INDY_HAS_ANTIFREEZE} var AIdAntiFreeze: TIdAntiFreezeBase;{$ENDIF}
    var AIdOpenSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    var AVerifyPeer: TsgVerifyPeer);
  begin
    FreeAndNil(AIdHTTP);
{$IFDEF SG_INDY_HAS_ANTIFREEZE}
    Dec(GlobalAntiFreezeRefcount);
    if GlobalAntiFreezeRefcount <= 0 then
    begin
      GlobalAntiFreezeRefcount := 0;
      FreeAndNil(AIdAntiFreeze);
    end;
{$ENDIF}
    FreeAndNil(AIdOpenSSLIOHandler);
    FreeAndNil(AVerifyPeer);
  end;
var
  vHost, vCurrPath: string;
  IdHTTP: TIdHTTP;
{$IFDEF SG_INDY_HAS_ANTIFREEZE}
  IdAntiFreeze: TIdAntiFreezeBase;
{$ENDIF}
  IdOpenSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  vVerifyPeer: TsgVerifyPeer;
begin
  AErrorMessage := '';
  Prepare(vHost);
  IdOpenSSLIOHandler := nil;
  vVerifyPeer := nil;
  IdHTTP := TIdHTTP.Create(nil);
{$IFDEF SG_INDY_HAS_ANTIFREEZE}
  if GAntiFreeze <> nil then
    IdAntiFreeze := GAntiFreeze
  else
    IdAntiFreeze := TIdAntiFreeze.Create(nil);
  Inc(GlobalAntiFreezeRefcount);
  IdAntiFreeze.Active := True;
{$ENDIF}
  try
    if Assigned(HTTPOwner) then HTTPOwner.InsertComponent(IdHTTP);
   //IdHTTP.OnStatus := AOnStatus;
   IdHTTP.ReadTimeout := ATimeOut;
   if AUseProxy then
   begin
      IdHTTP.ProxyParams.ProxyServer := AProxyInfo.ProxyHost;
      IdHTTP.ProxyParams.ProxyPort := AProxyInfo.ProxyPort;
      if AProxyInfo.ProxyNeedPass then
      begin
        IdHTTP.ProxyParams.ProxyUsername := AProxyInfo.ProxyUser;
        IdHTTP.ProxyParams.ProxyPassword := AProxyInfo.ProxyPass;
      end;
    end;
    IdHTTP.HandleRedirects := AHandleRedirects;
    IdHTTP.RedirectMaximum := cnstMaxHttpRedirects;
    //always tries to use HTTPS protocol because can be automatic redirect from http to https
    IdOpenSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    IdOpenSSLIOHandler.SSLOptions.SSLVersions := [Low(TIdSSLVersion)..High(TIdSSLVersion)];
    //IdOpenSSLIOHandler.SSLOptions.Method
    vVerifyPeer := TsgVerifyPeer.Create;
    IdOpenSSLIOHandler.OnVerifyPeer := vVerifyPeer.DoOnVerifyPeer;
    IdHTTP.OnAuthorization := vVerifyPeer.DoOnAuthorization;
    IdHTTP.IOHandler := IdOpenSSLIOHandler;
    IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoInProcessAuth];
    //sslvTLSv1_2
    vCurrPath := GetCurrentDir;
    SetCurrentDir(ExtractFilePath(GetFileName));
    try
      if AGetMethod then
        IdHTTP.Get(vHost, AStream)
      else
        IdHTTP.Post(vHost, AStream)
    except
      on E: Exception do
      begin
        AErrorMessage := E.Message;
        FreeAll(IdHTTP, {$IFDEF SG_INDY_HAS_ANTIFREEZE}IdAntiFreeze,{$ENDIF} IdOpenSSLIOHandler, vVerifyPeer);
        Exit;
      end;
    end;
    FinalizeSuccess;
  finally
    FreeAll(IdHTTP, {$IFDEF SG_INDY_HAS_ANTIFREEZE}IdAntiFreeze,{$ENDIF} IdOpenSSLIOHandler, vVerifyPeer);
    if vCurrPath <> '' then
      SetCurrentDir(vCurrPath)
  end;
end;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
const
  UrlMonLib = 'URLMON.DLL';
type
  TCreateURLMoniker = function(MkCtx: IMoniker; szURL: LPCWSTR; out mk: IMoniker): HResult; stdcall;
var
  vHost: WideString;
  vBind: IBindCtx;
  vURLMoniker: IMoniker;
  vStm: IStream;
  vStat: TStatStg;
  vBuff: PByte;
  vRead: LongInt;
  UrlMonLibHandle: THandle;
  CreateURLMoniker: TCreateURLMoniker;
begin
  AErrorMessage := '';
  vRead := 0;
  UrlMonLibHandle := SafeLoadLibrary(UrlMonLib);
  if UrlMonLibHandle <> 0 then
  try
    Pointer({$IFNDEF FPC_OBJFPC}@{$ENDIF}CreateURLMoniker) := GetProcAddress(UrlMonLibHandle, 'CreateURLMoniker');
    if @CreateURLMoniker <> nil then
    begin
      if Copy(UpperCase(AHost), 1, 4) <> UpperCase(sHttp) then
        vHost := WideString(sHttp + sSlashes + AHost)
      else
        vHost := WideString(AHost);
      if Succeeded(CreateBindCtx(0, vBind)) then
      begin
        if Succeeded(CreateURLMoniker(nil, PWideChar(vHost), vURLMoniker)) then
        begin
          if Succeeded(vURLMoniker.BindToStorage(vBind, nil, IStream, vStm)) then
          begin
            if Succeeded(vStm.Stat(vStat, STATFLAG_DEFAULT)) then
            begin
              vRead := 0;
              if AStream is TCustomMemoryStream then
              begin
                AStream.Size := AStream.Size + vStat.cbSize;
                vBuff := TCustomMemoryStream(AStream).Memory;
                vStm.Read(vBuff, vStat.cbSize, @vRead);
              end
              else
              begin
                GetMem(vBuff, vStat.cbSize);
                try
                  if Succeeded(vStm.Read(vBuff, vStat.cbSize, @vRead)) then
                  begin
                    AStream.Write(vBuff^, vRead);
                    AStream.Position := 0;
                  end;
                finally
                  FreeMem(vBuff, vStat.cbSize);
                end;
              end;
              FinalizeSuccess;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeLibrary(UrlMonLibHandle);
  end;
  Result := vRead > 0;
end;
{$ELSE}
{$IFDEF SG_USE_WEB_LAZ}
var
  vHTPClient: TFPHTTPClient;
  vHost: string;
begin
  AErrorMessage := '';
  vHTPClient := TFPHTTPClient.Create(nil);
  try
    if Assigned(HTTPOwner) then HTTPOwner.InsertComponent(vHTPClient);
    vHTPClient.IOTimeout := ATimeOut;
    vHTPClient.AllowRedirect := AHandleRedirects;
    //vHTPClient.OnGetSocketHandler := vSocketHandler.DoGetSocketHandlerEvent;
    if AUseProxy then
    begin
      vHTPClient.Proxy.Host := AProxyInfo.ProxyHost;
      vHTPClient.Proxy.Port := AProxyInfo.ProxyPort;
      if AProxyInfo.ProxyNeedPass then
      begin
        vHTPClient.Proxy.UserName := AProxyInfo.ProxyUser;
        vHTPClient.Proxy.Password := AProxyInfo.ProxyPass;
      end;
    end;
    Result := True;
  finally
    vHTPClient.Free;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF} //SG_USE_WEB_LAZ
{$ENDIF} //SG_NON_WIN_PLATFORM
{$ENDIF} //SG_USE_INDY
{$ENDIF} //SG_USE_SYNAPSE
{$ENDIF} //SGDEL_XE8

function CodepageToCharset(ACodePage: Cardinal): TFontCharset;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vcsi: TCharsetInfo;
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  if not TranslateCharsetInfo(ACodepage, vcsi, TCI_SRCCODEPAGE) then
  begin
{$ENDIF}
    case ACodePage of
      874:  Result := THAI_CHARSET;
      932:  Result := SHIFTJIS_CHARSET;
      936:  Result := GB2312_CHARSET;
      949:  Result := HANGEUL_CHARSET;
      950:  Result := CHINESEBIG5_CHARSET;
      1250: Result := EASTEUROPE_CHARSET;
      1251: Result := RUSSIAN_CHARSET;
      1252: Result := ANSI_CHARSET;
      1253: Result := GREEK_CHARSET;
      1254: Result := TURKISH_CHARSET;
      1255: Result := HEBREW_CHARSET;
      1256: Result := ARABIC_CHARSET;
      1257: Result := BALTIC_CHARSET;
      1258: Result := 163;//VIETNAMESE_CHARSET;
      1361: Result := JOHAB_CHARSET;
    else
      Result := DEFAULT_CHARSET;
    end;
{$IFNDEF SG_NON_WIN_PLATFORM}
  end
  else
    Result := vcsi.ciCharset;
{$ENDIF}
end;

function GetCodePageByIndex(const AIndex: Integer): Integer;
begin
  case AIndex of
    1: Result := 932;
    2: Result := 950;
    3: Result := 949;
    4: Result := 1361;
    5: Result := 936;
  else
    Result := CP_ACP;// current system Windows ANSI code page
  end;
end;

function CodepageToMCodeIndex(ACodePage: Cardinal): Integer;
begin
  case CodepageToCharset(ACodePage) of
    SHIFTJIS_CHARSET: Result := 1;
    CHINESEBIG5_CHARSET: Result := 2;
    HANGEUL_CHARSET: Result := 3;
    JOHAB_CHARSET: Result := 4;
    GB2312_CHARSET: Result := 5;
  else
    Result := 0;
  end;
end;

function GetMultibyteSymbol(const ALeft, ARight: Byte; const ANumber: Integer): WideChar;
var
  vCodePage: Integer;
  vWideStr: WideString;
begin
  vCodePage := GetCodePageByIndex(ANumber);
  vWideStr := ConvertToWideString(AnsiChar(ALeft) + AnsiChar(ARight), vCodePage);
  if vWideStr <> '' then
    Result := vWideStr[1]
  else
    Result := WideChar(0);
end;

function GetNameByArrowType(const AType: TsgDimArrowType; const ADefValue: string): string;
begin
  Result := sgDimensionArrowTypeNames[AType]
end;

function GetNameByEntTypeEx(const AType: Integer): string;
begin
  case AType of
    cnstDimConstruction:  Result := sEntNamesEx[0];
    cnstConstruction:     Result := sEntNamesEx[1];
    cnstComplex:          Result := sEntNamesEx[2];
    cnstElement:          Result := sEntNamesEx[3];
    cnstComplexBroad:     Result := sEntNamesEx[4];
    cnstComplexLinear:    Result := sEntNamesEx[5];
    cnstElementBroad:     Result := sEntNamesEx[6];
    cnstElementLinear:    Result := sEntNamesEx[7];
    cnstElementCarved:    Result := sEntNamesEx[8];
    cnstElementModifier:  Result := sEntNamesEx[14];
    cnstArea:             Result := sEntNamesEx[9];
    cnstComplexArea:      Result := sEntNamesEx[10];
    cnstLabel:            Result := sEntNamesEx[11];
    cnstDimLabel:         Result := sEntNamesEx[12];
    cnstBlockPattern:     Result := sEntNamesEx[13];
  else
    Result := sEntNamesEx[-1];
  end;
end;

function IntToColor(AValue: Integer; APalett: PsgCADPalett = nil): TColor;
begin
  Result := AValue;
  if Result < 0 then
    Exit;
  if APalett = nil then
    APalett := @arrDXFtoRGBColors;
  Result := clByLayer;
  if AValue = 0 then
    Result := clByBlock
  else
    if (AValue > 0) and (AValue < 256) then
    begin
      if (AValue <> 7) then
        Result := TColor(APalett^[AValue])
      else
        Result := clNone;
    end;
end;

function IsBadRect(ARect: TFRect): Boolean;
var
  vIsNan: Boolean;
  I: Integer;
begin
  I := 0;
  vIsNan := False;
  while (I < 3) and not vIsNan do
    if IsNan(ARect.TopLeft.V[I]) then
      Inc(vIsNan)
    else
      Inc(I);
  if not vIsNan then
  begin
    I := 0;
    while (I < 3) and not vIsNan do
      if IsNan(ARect.BottomRight.V[I]) then
        Inc(vIsNan)
      else
        Inc(I);
  end;
  if not vIsNan then
  begin
    if (ARect.Right < ARect.Left) or (ARect.Top < ARect.Bottom)
      or (ARect.Z2 < ARect.Z1) then
        vIsNan := True;
  end;
  Result := vIsNan;
end;

function IsBadMatrix(const AMatrix: TFMatrix): Boolean;
var
  I, J: Integer;
  vValue: Double;
begin
  Result := False;
  I := 0;
  while (I <= 3) and not Result do
  begin
    J := 0;
    while (J <= 2) and not Result do
    begin
      vValue := AMatrix.M[I, J];
      Result := IsNan(vValue) or IsInfinite(vValue);
      Inc(J);
    end;
    Inc(I);
  end;
end;

function IsHexDigit(AChar: Char): Boolean;
begin
  Result := IsDigit(AChar);
  if not Result then
  begin
    AChar := UpCase(AChar);
    Result := (AChar >= 'A') and (AChar <= 'F');
  end;
end;

function LCIDToCodePage(const ALCID:{$IFDEF SG_SYSTEM_PROCS}DWORD{$ELSE}SysUtils.TLocaleID{$ENDIF}): Integer;
{$IFDEF MSWINDOWS}
var
  Buffer: array [0..6] of Char;
  Err: Integer;
begin
  if GetLocaleInfo(ALCID, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer)) <> 0 then
  begin
    Val(Buffer, Result, Err);
    if Err > 0 then
      Result := GetACP;
  end
  else
    Result := GetACP;
end;
{$ELSE}
begin
  Result := CP_ACP;
end;
{$ENDIF}


procedure FMatOffset(var AMatrix: TFMatrix; const APoint: TFPoint);
begin
  AMatrix.E0 := APoint;
end;

function AddInt32(AValue: Int64; const ADelta: Integer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CheckInt32Range(AValue + ADelta);
end;

function SubInt32(AValue: Int64; const ADelta: Integer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CheckInt32Range(AValue - ADelta);
end;

function CheckInt32Range(const AValue: Int64): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValue < cnstMaxInt32 then
  begin
    if AValue > cnstMinInt32 then
      Result := AValue
    else
      Result := cnstMinInt32;
  end
  else
    Result := cnstMaxInt32;
end;

function LispDoubleToStr(AValue: Double): string;
begin
  Result := DoubleToStr(AValue, cnstPoint);
end;

function LispStrToDouble(const AStr: string): Double;
var
  vSeparator: Char;
begin
  vSeparator := GetDecimalSeparator;
  if Pos(vSeparator, AStr) > 0 then
    Result := StrToDouble(AStr, vSeparator)
  else
    Result := StrToDouble(AStr, cnstPoint);
end;

function Max(const AValue1, AValue2: Double): Double;
begin
  if AValue1 > AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function MaxI(const AValue1, AValue2: Integer): Integer;
begin
  if AValue1 > AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function Min(const AValue1, AValue2: Double): Double;
begin
  if AValue1 < AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function MinI(const AValue1, AValue2: Integer): Integer;
begin
  if AValue1 < AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function IsMaxRangeInt32(const AValue: Int64): Boolean;
begin
  if AValue > 0 then
    Result := AValue >= cnstMaxInt32
  else
    Result := AValue <= cnstMinInt32;
end;

function ParseUnicode(AStr: sgUnicodeStr; AModes: TParseUnicodeModes): sgUnicodeStr;
begin
  Result := AStr;
  if puU in AModes then
  begin
    ParseCommandSym(Result, UpperCase(cnstTypeU), Result, 4, @CmdSymReplaceU);
    ParseCommandSym(Result, LowerCase(cnstTypeU), Result, 4, @CmdSymReplaceU);
  end;
  if puM in AModes then
  begin
    ParseCommandSym(Result, UpperCase(cnstTypeM), Result, 5, @CmdSymReplaceM);
    ParseCommandSym(Result, LowerCase(cnstTypeM), Result, 5, @CmdSymReplaceM);
  end;
//  if puDP in AModes then
//    ParseCommandSym(AStr, cnstTypeDP, AStr, -1, @CmdSymReplaceDPercent);
end;

{  PointClassify
   p0, p1 - line (segment from p0 to p1)
   Classifies a point concerning a line. }
function PointClassify(const APoint, AStartPoint, AEndPoint: TFPoint;
  const AAccuracy: Double = 0): TsgPointClassify;
begin
  Result := PointClassifyCoords(APoint.X, APoint.Y, AStartPoint.X, AStartPoint.Y,
    AEndPoint.X, AEndPoint.Y, AAccuracy);
end;

function PointClassifyCoords(const APX, APY, AP1X, AP1Y, AP2X, AP2Y: Extended;
  const AAccuracy: Extended = 0): TsgPointClassify;
var
  aX, aY, bX, bY, sa, vAccuracy, vSegmentLength, vXY: Extended;
begin
  aX := AP2X - AP1X;
  aY := AP2Y - AP1Y;
  bX := APX - AP1X;
  bY := APY - AP1Y;
  vSegmentLength := DistanceVector2D(aX, aY);
  vAccuracy := AAccuracy;
  if vAccuracy = 0 then
    vAccuracy := GetAccuracy(vSegmentLength);
  sa := aX * bY - bX * aY;
  Result := pcBETWEEN;
  if sa > vAccuracy then
    Result := pcLEFT
  else
    if sa < -vAccuracy then
      Result := pcRIGHT
    else
    begin
      if AAccuracy = 0 then
      begin
        if (aX * bX < 0.0) or (aY * bY < 0.0) then
          Result := pcBEHIND
        else
          if vSegmentLength < DistanceVector2D(bX, bY) then
            Result := pcBEYOND
          else
          begin
            if (AP1X = APX) and (AP1Y = APY) then
              Result := pcORIGIN
            else
              if (AP2X = APX) and (AP2Y = APY) then
                Result := pcDESTINATION;
        end;
      end
      else
      begin
        if (aX * bX <= -vAccuracy) or (aY * bY <= -vAccuracy) then
          Result := pcBEHIND
        else
        begin
          vXY := vSegmentLength - DistanceVector2D(bX, bY);
          if (vXY > 0) or IsRange(vXY, vAccuracy)  then
          begin
            if IsRange(bX, vAccuracy) and IsRange(bY, vAccuracy) then
              Result := pcORIGIN
            else
              if IsRange(AP2X - APX, vAccuracy) and IsRange(AP2Y - APY, vAccuracy)then
                Result := pcDESTINATION;
          end
          else
            Result := pcBEYOND;
        end;
      end;
    end;
end;

function TestValues(const AP, AP12, AV, AV1, AV2: Double;
 const AAccuracy: Double): TsgPointClassify;
begin
  case sgCompareDoubleAcc(AP, AV, AAccuracy) of
    -1:  Result := pcLEFT;
    +1:  Result := pcRIGHT;
  else
    case sgCompareDoubleAcc(AP12, AV1, AAccuracy) of
      -1:  Result := pcBEYOND;
      +1:
        begin
          case sgCompareDoubleAcc(AP12, AV2, AAccuracy) of
            -1:  Result := pcBETWEEN;
            +1:  Result := pcBEHIND;
          else
            Result := pcDESTINATION;
          end;
        end;
    else
      Result := pcORIGIN;
    end;
  end;
end;

function PointClassifyTest(const APX, APY, AP1X, AP1Y, AP2X, AP2Y: Double;
  const AAccuracy: Extended = -1): TsgPointClassify;
var
  dX, dY, Y, X, vAccuracy: Double;
begin
  dX := AP2X - AP1X;
  dY := AP2Y - AP1Y;
  vAccuracy := AAccuracy;
  if vAccuracy < 0 then
    vAccuracy := GetAccuracy(DistanceVector2D(dX, dY));
  if dX = 0 then
    Result := TestValues(APX, APY, AP1X, AP1Y, AP2Y, vAccuracy)
  else
  begin
    if dY = 0 then
      Result := TestValues(APY, APX, AP1Y, AP1X, AP2X, vAccuracy)
    else
    begin
      if Abs(dX) >= Abs(dY) then
      begin
        Y := (dY / dX) * (APX - AP1X) + AP1Y;
        Result := TestValues(Y, APX, APY, AP1X, AP2X, vAccuracy)
      end
      else
      begin
        X := (dX / dY) * (APY - AP1Y) + AP1X;
        Result := TestValues(X, APY, APX, AP1Y, AP2Y, vAccuracy);
      end;
    end;
  end;
end;

function PointClassify2DPts(const APoint, AStartPoint, AEndPoint: TF2DPoint;
  const AAccuracy: Double = 0): TsgPointClassify;
begin
  Result := PointClassifyCoords(APoint.X, APoint.Y, AStartPoint.X, AStartPoint.Y,
    AEndPoint.X, AEndPoint.Y, AAccuracy);
end;

function Proportional(const AXInput, AYInput, AOutput: TsgFloat;
  AOutputIsX : Boolean): TsgFloat;
begin
  if (AXInput = 0) or (AYInput = 0) then
  begin
    Result := AOutput;
    Exit;
  end;
  if AOutputIsX then
    Result := AYInput * AOutput / AXInput
  else
    Result := AXInput * AOutput / AYInput;
end;

function CADUnProject(const AClientPos: TFPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
var
  vPlanePt: TFPoint;
  M: TFMatrix;
begin
  M := FMatInverse(ADrawMatrix);

  vPlanePt := cnstFPointZero;
//  if M.EZ.X <> 0 then
//    vPlanePt.X := -(M.E0.X + AClientPos.X * M.EX.X + AClientPos.Y * M.EY.X) / M.EZ.X;
//  if M.EZ.Y <> 0 then
//    vPlanePt.Y := -(M.E0.Y + AClientPos.X * M.EX.Y + AClientPos.Y * M.EY.Y) / M.EZ.Y;
  if M.EZ.Z <> 0 then
    vPlanePt.Z := -(M.E0.Z + AClientPos.X * M.EX.Z + AClientPos.Y * M.EY.Z) / M.EZ.Z;

  Result := FPointXMat(MakeFPoint(AClientPos.X, AClientPos.Y, vPlanePt.Z), M);
end;

function CADUnProject(const AClientPos: TF2DPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
begin
  Result := CADUnProject(MakeFPointFrom2D(AClientPos), ADrawMatrix);
end;

function CADUnProject(const X, Y, Z: Double; const ADrawMatrix: TFMatrix): TFPoint; overload;
begin
  Result := CADUnProject(MakeFPoint(X, Y, Z), ADrawMatrix);
end;

function CADUnProject(const X, Y: Integer; const ADrawMatrix: TFMatrix): TFPoint; overload;
begin
  Result := CADUnProject(MakeFPoint(X, Y), ADrawMatrix);
end;

function CADUnProject(const AClientPos: TPoint; const ADrawMatrix: TFMatrix): TFPoint; overload;
begin
  Result := CADUnProject(MakeFPointFromPoint(AClientPos), ADrawMatrix);
end;

function AffineTransformPoint(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint;
begin
  Result := AffineTransformPoint(APoint, AMatrix.Affine);
end;

function AffineTransformPoint(const APoint: TFPoint; const AMatrix: TsgAffineMatrix): TFPoint;
{$IFDEF SG_FAST_CODE}
{$IFDEF SG_CPUX64}
asm
  fld qword ptr [APoint]
  fld ST(0)
  fmul qword ptr [AMatrix]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$18]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$30]
  faddp
//  fadd qword ptr [AMatrix+$48]
  fstp qword ptr [Result]
  wait

  fld ST(0)
  fmul qword ptr [AMatrix+$08]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$20]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$38]
  faddp
//  fadd qword ptr [AMatrix+$50]
  fstp qword ptr [Result+$08]
  wait

  fmul qword ptr [AMatrix+$10]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$28]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$40]
  faddp
//  fadd qword ptr [AMatrix+$58]
  fstp qword ptr [Result+$10]
  wait
end;
{$ELSE}
asm
  fld qword ptr [eax]
  fld ST(0)
  fmul qword ptr [edx]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$18]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$30]
  faddp
//  fadd qword ptr [edx+$48]
  fstp qword ptr [ecx]
  wait

  fld ST(0)
  fmul qword ptr [edx+$08]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$20]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$38]
  faddp
//  fadd qword ptr [edx+$50]
  fstp qword ptr [ecx+$08]
  wait

  fmul qword ptr [edx+$10]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$28]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$40]
  faddp
//  fadd qword ptr [edx+$58]
  fstp qword ptr [ecx+$10]
  wait
end;
{$ENDIF}
{$ELSE}
begin
  Result.X := APoint.X * AMatrix.M[0,0] + APoint.Y * AMatrix.M[1,0] +
    APoint.Z * AMatrix.M[2,0];
  Result.Y := APoint.X * AMatrix.M[0,1] + APoint.Y * AMatrix.M[1,1] +
    APoint.Z * AMatrix.M[2,1];
  Result.Z := APoint.X * AMatrix.M[0,2] + APoint.Y * AMatrix.M[1,2] +
    APoint.Z * AMatrix.M[2,2];
end;
{$ENDIF}

// fastest more then 10 times
function FPointXMat(const APoint: TFPoint; const AMatrix: TFMatrix): TFPoint; {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
{$IFDEF SG_FAST_CODE}
{$IFDEF SG_CPUX64}
asm
  fld qword ptr [APoint]
  fld ST(0)
  fmul qword ptr [AMatrix]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$18]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$30]
  faddp
  fadd qword ptr [AMatrix+$48]
  fstp qword ptr [Result]
  wait

  fld ST(0)
  fmul qword ptr [AMatrix+$08]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$20]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$38]
  faddp
  fadd qword ptr [AMatrix+$50]
  fstp qword ptr [Result+$08]
  wait

  fmul qword ptr [AMatrix+$10]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$28]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$40]
  faddp
  fadd qword ptr [AMatrix+$58]
  fstp qword ptr [Result+$10]
  wait
end;
{$ELSE}
asm
  fld qword ptr [eax]
  fld ST(0)
  fmul qword ptr [edx]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$18]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$30]
  faddp
  fadd qword ptr [edx+$48]
  fstp qword ptr [ecx]
  wait

  fld ST(0)
  fmul qword ptr [edx+$08]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$20]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$38]
  faddp
  fadd qword ptr [edx+$50]
  fstp qword ptr [ecx+$08]
  wait

  fmul qword ptr [edx+$10]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$28]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$40]
  faddp
  fadd qword ptr [edx+$58]
  fstp qword ptr [ecx+$10]
  wait
end;
{$ENDIF}
{$ELSE}
begin
  Result.X := APoint.X * AMatrix.M[0,0] + APoint.Y * AMatrix.M[1,0] +
    APoint.Z * AMatrix.M[2,0] + AMatrix.M[3,0];
  Result.Y := APoint.X * AMatrix.M[0,1] + APoint.Y * AMatrix.M[1,1] +
    APoint.Z * AMatrix.M[2,1] + AMatrix.M[3,1];
  Result.Z := APoint.X * AMatrix.M[0,2] + APoint.Y * AMatrix.M[1,2] +
    APoint.Z * AMatrix.M[2,2] + AMatrix.M[3,2];
end;
{$ENDIF}

// fastest more then 10 times
procedure TransformPointTo3f(const APoint: TFPoint; const AMatrix: TFMatrix; AAffineVector: PSingle); {$IFDEF SG_FAST_CODE} assembler; register; {$ENDIF}
{$IFDEF SG_FAST_CODE}
{$IFDEF SG_CPUX64}
asm
  fld qword ptr [APoint]
  fld ST(0)
  fmul qword ptr [AMatrix]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$18]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$30]
  faddp
  fadd qword ptr [AMatrix+$48]
  fstp dword ptr [AAffineVector]
  wait

  fld ST(0)
  fmul qword ptr [AMatrix+$08]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$20]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$38]
  faddp
  fadd qword ptr [AMatrix+$50]
  fstp dword ptr [AAffineVector+$04]
  wait

  fmul qword ptr [AMatrix+$10]
  fld qword ptr [APoint+$08]
  fmul qword ptr [AMatrix+$28]
  faddp
  fld qword ptr [APoint+$10]
  fmul qword ptr [AMatrix+$40]
  faddp
  fadd qword ptr [AMatrix+$58]
  fstp dword ptr [AAffineVector+$08]
  wait
end;
{$ELSE}
asm
  fld qword ptr [eax]
  fld ST(0)
  fmul qword ptr [edx]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$18]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$30]
  faddp
  fadd qword ptr [edx+$48]
  fstp dword ptr [ecx]
  wait

  fld ST(0)
  fmul qword ptr [edx+$08]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$20]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$38]
  faddp
  fadd qword ptr [edx+$50]
  fstp dword ptr [ecx+$04]
  wait

  fmul qword ptr [edx+$10]
  fld qword ptr [eax+$08]
  fmul qword ptr [edx+$28]
  faddp
  fld qword ptr [eax+$10]
  fmul qword ptr [edx+$40]
  faddp
  fadd qword ptr [edx+$58]
  fstp dword ptr [ecx+$08]
  wait
end;
{$ENDIF}
{$ELSE}
begin
  AAffineVector^ := APoint.X * AMatrix.M[0,0] + APoint.Y * AMatrix.M[1,0] +
    APoint.Z * AMatrix.M[2,0] + AMatrix.M[3,0];
  Inc(AAffineVector);
  AAffineVector^ := APoint.X * AMatrix.M[0,1] + APoint.Y * AMatrix.M[1,1] +
    APoint.Z * AMatrix.M[2,1] + AMatrix.M[3,1];
  Inc(AAffineVector);
  AAffineVector^ := APoint.X * AMatrix.M[0,2] + APoint.Y * AMatrix.M[1,2] +
    APoint.Z * AMatrix.M[2,2] + AMatrix.M[3,2];
end;
{$ENDIF}

function AffineMatrixAdjoint(const AAfineMatrix: TsgAffineMatrix): TsgAffineMatrix;
var
  a1, a2, a3, b1, b2, b3, c1, c2, c3: Double;
begin
  a1 := AAfineMatrix.M[0, 0];
  a2 := AAfineMatrix.M[0, 1];
  a3 := AAfineMatrix.M[0, 2];
  b1 := AAfineMatrix.M[1, 0];
  b2 := AAfineMatrix.M[1, 1];
  b3 := AAfineMatrix.M[1, 2];
  c1 := AAfineMatrix.M[2, 0];
  c2 := AAfineMatrix.M[2, 1];
  c3 := AAfineMatrix.M[2, 2];

  Result.M[0, 0] := (b2 * c3 - c2 * b3);
  Result.M[1, 0] := -(b1 * c3 - c1 * b3);
  Result.M[2, 0] := (b1 * c2 - c1 * b2);

  Result.M[0, 1] := -(a2 * c3 - c2 * a3);
  Result.M[1, 1] := (a1 * c3 - c1 * a3);
  Result.M[2, 1] := -(a1 * c2 - c1 * a2);

  Result.M[0, 2] := (a2 * b3 - b2 * a3);
  Result.M[1, 2] := -(a1 * b3 - b1 * a3);
  Result.M[2, 2] := (a1 * b2 - b1 * a2);
end;

function AffineMatrixDeterminant(const AAfineMatrix: TsgAffineMatrix): Extended;
begin
  Result := GetDeterminant3x3(
    AAfineMatrix.EX.X, AAfineMatrix.EY.X, AAfineMatrix.EZ.X,
    AAfineMatrix.EX.Y, AAfineMatrix.EY.Y, AAfineMatrix.EZ.Y,
    AAfineMatrix.EX.Z, AAfineMatrix.EY.Z, AAfineMatrix.EZ.Z);
end;

function Radian(AAngle: Extended): Extended;
begin
  Result := AAngle * fPiDividedBy180;
end;

function ReplaceAnsi(var AText: string; const AFromText, AToText: string;
  const ACaseSensitive: Boolean = False): Boolean;
var
  vFromTextPos, vFromTextLen, vToTextLen: Integer;
  vText, vFromText: string;
begin
  Result := False;
  vFromTextLen := Length(AFromText);
  vToTextLen := Length(AToText);
  if ACaseSensitive then
  begin
    vText := AText;
    vFromText := AFromText;
  end
  else
  begin
    vText := LowerCase(AText);
    vFromText := LowerCase(AFromText);
  end;
  vFromTextPos := StringPos(vFromText, vText, 1);
  while vFromTextPos > 0 do
  begin
    Result := True;
    Delete(vText, vFromTextPos, vFromTextLen);
    Delete(AText, vFromTextPos, vFromTextLen);
    if vToTextLen > 0 then
    begin
      Insert(AToText, vText, vFromTextPos);
      Insert(AToText, AText, vFromTextPos);
      Inc(vFromTextPos, vToTextLen);
    end;
    vFromTextPos := StringPos(vFromText, vText, vFromTextPos);
  end;
end;

procedure PluginsReplaceAnsis(var AStr: string);
begin
  ReplaceAnsi(AStr, #1, '\L');
  ReplaceAnsi(AStr, #2, '\l');
  ReplaceAnsi(AStr, #3, '\O');
  ReplaceAnsi(AStr, #4, '\o');
end;

function ReplaceChar(var AString: string;
  const ACharOld, ACharNew: Char): string;
var
  I, vLen: Integer;
begin
  vLen := Length(AString);
  for I := 1 to vLen do
    if AString[I] = ACharOld then
      AString[I] := ACharNew;
  Result := AString;
end;

function ScalarXMat(const AValue: TsgFloat; AType: Byte;
  const AMatrix: TFMatrix; const ASing: Boolean = False): TsgFloat;
var
  vMatrix: TFMatrix;
  vPoint: TFPoint;
begin
  vMatrix := AMatrix;
  FMatOffset(vMatrix, cnstFPointZero);
  vPoint := cnstFPointZero;
  if AType > 2 then
    AType := 2;
  vPoint.V[AType] := AValue;
  Result := DistanceFPoint(cnstFPointZero, FPointXMat(vPoint, vMatrix));
  if ASing and (AValue < 0) then
    Result := -Result;
end;

procedure SetSHXSearchPaths(const APaths: TStringList);
var
  I: Integer;
  vSearchPath: string;
  vLength: Integer;
begin
  vSearchPath := AnsiLowerCase(sSHXSearchPaths);
  vLength := Length(vSearchPath);
  if (vLength > 0) and (vSearchPath[vLength] <> ';') then
    vSearchPath := vSearchPath + ';';
  for I := 0 to APaths.Count - 1 do
    if Pos(AnsiLowerCase(APaths[I]) + ';', vSearchPath) <= 0 then
      sSHXSearchPaths := sSHXSearchPaths + APaths[I] + ';';
end;

// only for debug
procedure sgNOP;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFDEF SG_FAST_CODE}
asm
  NOP
end;
{$ELSE}
begin
  Sleep(1);//for debug
end;
{$ENDIF}

{$IFDEF SGDEL_XE2}
function PointerAdd(APointer: Pointer; Add: Integer): Pointer;
begin
  Result := Pointer(TsgNativePointer(APointer) + TsgNativePointer(Add));
end;

function PointerDec(APointer: Pointer; Dec: Integer): Pointer;
begin
  Result := Pointer(TsgNativePointer(APointer) - TsgNativePointer(Dec));
end;
{$ENDIF}

function HasAccessByWrite(ADir: string): Boolean;
const
  cnstTestFile = 'testfile.tmp';
  cnstEndDir = '\';
var
  vCreate: Integer;
  vDelete: Boolean;
  vFile: string;
begin
  Result := False;
  if (Length(ADir) > 0) and (ADir[Length(ADir)] <> cnstEndDir) then
    ADir := ADir + cnstEndDir;
  vFile := ADir + cnstTestFile;
  try
    try
      vCreate := SysUtils.FileCreate(vFile);
      Result := vCreate > 0;
      if Result then
        SysUtils.FileClose(vCreate);
    except
{$IFDEF _FIXINSIGHT_}
      on E: Exception do EmptyExceptionHandler(E);
{$ENDIF}
    end;
  finally
    try
      vDelete := SysUtils.DeleteFile(vFile);
      Result := Result and vDelete;
    except
      Result := False;
    end;
  end;
end;

procedure BuildRotMatrix(const AAxis: TsgAxes; ARadAngle: Double;
  var AAffineMatrix: TsgAffineMatrix;
  const AResolution: Double = fExtendedResolution);

  procedure DoMat(A, B: Integer);
  var
    S, C: Extended;
    vOct, vFrac: Double;
    vInt: Integer;
    vBadRot: Boolean;
  begin
    vOct := ARadAngle / cnstPiDiv2;
    vInt := Round(Int(vOct));
    vFrac := Frac(vOct);
    vBadRot := False;
    if Abs(1.0 - Abs(vFrac)) < AResolution then
    begin
      Inc(vInt, Sign(vFrac));
      vBadRot := True;
    end
    else
      if Abs(vFrac) < AResolution then
        vBadRot := True;
    if vBadRot then
      case vInt mod 4 of
        0, 4:   // 0, 360
          begin
            S := 0;
            C := 1;
          end;
        -3, 1:  // -270, 90
          begin
            S := 1;
            C := 0;
          end;
        -2, 2: // -180, 180
          begin
            S := 0;
            C := -1;
          end;
        -1, 3: // -90, 270
          begin
            S := -1;
            C := 0;
          end;
      end { case }
    else
      SinCos(ARadAngle, S, C);
    if AAxis = axisY then
      S := -S;
    AAffineMatrix.M[A, A] := C;
    AAffineMatrix.M[B, B] := C;
    AAffineMatrix.M[A, B] := S;
    AAffineMatrix.M[B, A] := -S;
  end;

begin
  AAffineMatrix := cnstIdentityMat.Affine;
  case AAxis of
    axisX: DoMat(1, 2);
    axisY: DoMat(0, 2);
    axisZ: DoMat(0, 1);
  end;
end;

function BuildRotMatrix(const AAxis: TsgAxes; ARadAngle: Double;
  const AResolution: Double): TFMatrix; overload;
begin
  BuildRotMatrix(AAxis, ARadAngle, Result.Affine, AResolution);
  Result.E0 := cnstFPointZero;
end;

procedure BuildRotMatrix(const AAxisVector: TFPoint; AAngle: Double;
  var AResult: TFMatrix); overload;
var
   vAxis: TFPoint;
   C, S, NCos: Extended;
begin
   SinCos(AAngle, S, C);
   NCos := 1 - C;
   vAxis := Ort(AAxisVector);

   AResult.M[0, 0] := (NCos * vAxis.V[0] * vAxis.V[0]) + C;
   AResult.M[0, 1] := (NCos * vAxis.V[0] * vAxis.V[1]) - (vAxis.V[2] * S);
   AResult.M[0, 2] := (NCos * vAxis.V[2] * vAxis.V[0]) + (vAxis.V[1] * S);

   AResult.M[1, 0] := (NCos * vAxis.V[0] * vAxis.V[1]) + (vAxis.V[2] * S);
   AResult.M[1, 1] := (NCos * vAxis.V[1] * vAxis.V[1]) + C;
   AResult.M[1, 2] := (NCos * vAxis.V[1] * vAxis.V[2]) - (vAxis.V[0] * S);

   AResult.M[2, 0] := (NCos * vAxis.V[2] * vAxis.V[0]) - (vAxis.V[1] * S);
   AResult.M[2, 1] := (NCos * vAxis.V[1] * vAxis.V[2]) + (vAxis.V[0] * S);
   AResult.M[2, 2] := (NCos * vAxis.V[2] * vAxis.V[2]) + C;

   AResult.M[3, 0] := 0;
   AResult.M[3, 1] := 0;
   AResult.M[3, 2] := 0;
end;

function StdMat(const AScale, AOffset: TFPoint): TFMatrix;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.M[0,0] := AScale.X;
  Result.M[1,1] := AScale.Y;
  Result.M[2,2] := AScale.Z;
  FMatOffset(Result, AOffset);
end;

function StringHexToIntDef(const AStr: string; ADefault: Integer): UInt64;
var
  vCode: Integer;
begin
  Val(HexDisplayPrefix + AStr, Result, vCode);
  if vCode <> 0 then
    Result := ADefault;
end;

function StringToACADPresentation(const AString: sgUnicodeStr;
  const ACodePage: Integer): sgRawByteString;
const
  Mask = $400; // WC_NO_BEST_FIT_CHARS
{$IFNDEF SG_NON_WIN_PLATFORM}
  cnstBuffSize = 2;
{$ENDIF}
var
  I{$IFNDEF SG_NON_WIN_PLATFORM}, vOutLength{$ENDIF}: Integer;
  vInChar: sgUnicodeChar;
{$IFNDEF SG_NON_WIN_PLATFORM}
  vOutBuff: array[0 .. cnstBuffSize - 1] of AnsiChar;
{$ENDIF}
  vOutChar: sgRawByteString; // type "string" for 2-byte characters (double-byte character set)
  {$IFNDEF SG_NON_WIN_PLATFORM}
  vNotConvert: LongBool;
  {$ENDIF}
begin
  Result := '';
  if AString = '' then
    Exit;
  for I := 1 to Length(AString) do
  begin
    vInChar := AString[I];
    vOutChar := '';
    if Word(vInChar) <= 127 then // don't need converting
      vOutChar := AnsiChar(vInChar)
    else
    begin
{$IFNDEF SG_NON_WIN_PLATFORM}
      vOutLength := LocaleCharsFromUnicode(ACodePage, Mask, @vInChar, 1,
        @vOutBuff[0], cnstBuffSize, nil, @vNotConvert);
      if (vOutLength > 0) and not vNotConvert then
        SetString(vOutChar, vOutBuff, vOutLength)
      else
{$ENDIF}
        vOutChar := '\U+' + sgRawByteString(IntToHex(Word(vInChar), 4));
    end;
    Result := Result + vOutChar;
  end;
end;

procedure SwapSGFloats(var AValue1, AValue2: TsgFloat);
var
  vTempValue: TsgFloat;
begin
  vTempValue := AValue1;
  AValue1 := AValue2;
  AValue2 := vTempValue;
end;

procedure SwapSingles(var A,B: Single);
var
  vTempValue: Single;
begin
  vTempValue := A;
  A := B;
  B := vTempValue;
end;

function FMatTranspose(const AMatrix: TFMatrix): TFMatrix;
begin
  Result.M[0, 0] := AMatrix.M[0,0];
  Result.M[1, 0] := AMatrix.M[0,1];
  Result.M[2, 0] := AMatrix.M[0,2];
  Result.M[0, 1] := AMatrix.M[1,0];
  Result.M[1, 1] := AMatrix.M[1,1];
  Result.M[2, 1] := AMatrix.M[1,2];
  Result.M[0, 2] := AMatrix.M[2,0];
  Result.M[1, 2] := AMatrix.M[2,1];
  Result.M[2, 2] := AMatrix.M[2,2];
  Result.M[3, 0] := -AMatrix.M[3,0];
  Result.M[3, 1] := -AMatrix.M[3,1];
  Result.M[3, 2] := -AMatrix.M[3,2];
end;

function FMatTranspose(const AMatrix: TsgAffineMatrix): TsgAffineMatrix;
begin
  Result.M[0, 0] := AMatrix.M[0,0];
  Result.M[1, 0] := AMatrix.M[0,1];
  Result.M[2, 0] := AMatrix.M[0,2];
  Result.M[0, 1] := AMatrix.M[1,0];
  Result.M[1, 1] := AMatrix.M[1,1];
  Result.M[2, 1] := AMatrix.M[1,2];
  Result.M[0, 2] := AMatrix.M[2,0];
  Result.M[1, 2] := AMatrix.M[2,1];
  Result.M[2, 2] := AMatrix.M[2,2];
end;


function FMatTranslate(const AMatrix: TFMatrix; const AX, AY, AZ: Double): TFMatrix; overload;
begin
  Result := FMatXMat(AMatrix, FMatByTranslate(AX, AY, AZ));
end;

function FMatTranslate(const AMatrix: TFMatrix; const ADelta: TFPoint): TFMatrix; overload;
begin
  Result := FMatXMat(AMatrix, FMatByTranslate(ADelta));
end;

{
  TransRectCorners

  Optimaized more then 50%
}
procedure AbsDouble(var AValue);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  PByte(TsgNativeUInt(@AValue) + 7)^ := PByte(TsgNativeUInt(@AValue) + 7)^ and $7F;
end;
procedure NegDouble(var AValue);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  PByte(TsgNativeUInt(@AValue) + 7)^ := PByte(TsgNativeUInt(@AValue) + 7)^ or $80;
end;
procedure ReverseDouble(var AValue);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  PByte(TsgNativeUInt(@AValue) + 7)^ := PByte(TsgNativeUInt(@AValue) + 7)^ xor $80;
end;
procedure TransRectCorners(var ARect: TFRect; const AMatrix: TFMatrix);
var
  vSize, vCenter, vTL, vBR: TFPoint;
  vDir: array[1 .. 3] of TFPoint;
  vWidth, vHeight: Double;
  I, J: Integer;
begin
  vWidth := ARect.Right - ARect.Left;
  vHeight := ARect.Top - ARect.Bottom;
  vSize.X := 0.5 * vWidth;
  vSize.Y := 0.5 * vHeight;
  vSize.Z := 0.5 * (ARect.Z2 - ARect.Z1);

  vTL := AffineTransformPoint(vSize, AMatrix);
  vBR := vTL;
  ReverseDouble(vSize.Y);
  vCenter := FPointXMat(AddFPoint(ARect.TopLeft, vSize), AMatrix);

  NegDouble(vWidth);
  NegDouble(vHeight);
  vDir[1] := PtXScalar(AMatrix.EX, vWidth);
  vDir[2] := PtXScalar(AMatrix.EY, vHeight);
  vDir[3] := AddFPoint(vDir[1], vDir[2]);

  vDir[1] := AddFPoint(vDir[1], vTL);
  vDir[2] := AddFPoint(vDir[2], vTL);
  vDir[3] := AddFPoint(vDir[3], vTL);

  for I := 0 to 2 do
  begin
    NegDouble(vTL.V[I]);
    AbsDouble(vBR.V[I]);
    for J := 1 to 3 do
      with vDir[J] do // need for optimize
      begin
        NegDouble(V[I]);
        if V[I] < vTL.V[I] then
          vTL.V[I] := V[I];
        AbsDouble(V[I]);
        if V[I] > vBR.V[I] then
          vBR.V[I] := V[I];
      end;
  end;
  SwapDoubles(vTL.Y, vBR.Y);
  ARect.TopLeft := AddFPoint(vTL, vCenter);
  ARect.BottomRight := AddFPoint(vBR, vCenter);
end;
{procedure TransRectCorners(var ARect: TFRect; const AMatrix: TFMatrix);

  procedure ExpR(var ARect: TFRect; const AX, AY, AZ: TsgFloat);
  begin
    ExpandFRect(ARect, AffineTransformPoint(MakeFPoint(AX, AY, AZ), AMatrix));
  end;

var
  vRect: TFRect;
begin
  vRect.TopLeft := AffineTransformPoint(ARect.TopLeft, AMatrix);
  vRect.BottomRight := vRect.TopLeft;
  ExpR(vRect, ARect.Left, ARect.Top, ARect.Z2);
  ExpR(vRect, ARect.Left, ARect.Bottom, ARect.Z1);
  ExpR(vRect, ARect.Left, ARect.Bottom, ARect.Z2);
  ExpR(vRect, ARect.Right, ARect.Top, ARect.Z1);
  ExpR(vRect, ARect.Right, ARect.Top, ARect.Z2);
  ExpR(vRect, ARect.Right, ARect.Bottom, ARect.Z1);
  ExpR(vRect, ARect.Right, ARect.Bottom, ARect.Z2);
  ARect.TopLeft := AddFPoint(vRect.TopLeft, AMatrix.E0);
  ARect.BottomRight := AddFPoint(vRect.BottomRight, AMatrix.E0);
end;}

///
///
{$IFNDEF SG_FIREMONKEY}
function GetsgKeyBoardState(const AMsg: TMessage): TShiftState;
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  KeyState: TKeyboardState;
begin
  case AMsg.Msg of
    WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK: Result := [ssDouble];
  else
    Result := [];
  end;
  if GetKeyboardState(KeyState) then
  begin
    if KeyState[VK_SHIFT]   and $80 <> 0 then Include(Result, ssShift);
    if KeyState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
    if KeyState[VK_MENU]    and $80 <> 0 then Include(Result, ssAlt);
    if KeyState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
    if KeyState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
    if KeyState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
  end;
end;
{$ELSE}
begin
  Result := [];
end;
{$ENDIF}
{$ENDIF}

function IsInsUnits(const AValue: Integer): Boolean;
begin
  Result := (AValue >= Integer(Low(TsgInsUnits))) and
    (AValue <= Integer(High(TsgInsUnits)));
end;

function GetPixelFormat(const ASize: Integer): {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
begin
  Result := pfCustom;
  case ASize of
    1: Result := pf1bit;
//    2: Result := pf4bit;
    4: Result := pf4bit;
    8: Result := pf8bit;
    15: Result := pf15bit;
    16: Result := pf16bit;
    24: Result := pf24bit;
    32: Result := pf32bit;
  end;
end;
{$IFNDEF SGFPC}
  {$IFNDEF SG_FIREMONKEY}
function GetMetafileSize(AMF: TMetafile;
  ARatio: Double = cnstMetafileDefRatio): TF2DPoint;
var
  vMetaHdr: TEnhMetaHeader;
  vRatio: Double;
  vMillimeters: TF2DPoint;
begin
  vMetaHdr.nSize := SizeOf(vMetaHdr);
  GetEnhMetaFileHeader(AMF.Handle, vMetaHdr.nSize, @vMetaHdr);
  Result := MakeF2DPoint(
    Abs(vMetaHdr.rclBounds.Right - vMetaHdr.rclBounds.Left),
    Abs(vMetaHdr.rclBounds.Bottom - vMetaHdr.rclBounds.Top));
  vMillimeters := MakeF2DPoint(
    Abs(vMetaHdr.rclFrame.Right - vMetaHdr.rclFrame.Left) / 100,
    Abs(vMetaHdr.rclFrame.Bottom - vMetaHdr.rclFrame.Top) / 100);
  vRatio := 1;
  if (Result.X > Result.Y) and (vMillimeters.X < vMillimeters.Y) or
    (Result.X < Result.Y) and (vMillimeters.X > vMillimeters.Y) then
    SwapSGFloats(Result.X, Result.Y);
  if IsZero(Result.X) and IsZero(Result.Y) then
  begin
    Result.X := AMF.MMWidth / 100;
    Result.Y := AMF.MMHeight / 100;
  end
  else
    if IsZero(Result.X) then
      Result.X := Result.Y
    else
      if IsZero(Result.Y) then
        Result.Y := Result.X
      else
        vRatio := AMF.Width / AMF.Height;
  if IsZero(vMillimeters.X) then
  begin
    Result.Y := vMillimeters.Y / Result.Y * ARatio;
    Result.X := Result.Y * vRatio;
  end
  else
  begin
    Result.X := vMillimeters.X / Result.X * ARatio;
    Result.Y := Result.X / vRatio;
  end;
end;
  {$ENDIF}
{$ENDIF}

function GetPixelSize(const AFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat): Integer;
const
  cnstPixelSize: array [{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat] of Integer = (1,1,4,8,16,16,24,32,32);
begin
  Result := cnstPixelSize[AFormat];
end;

function GetSizeProportionForExport(const AGraphic: TGraphic;
  const AExportWidth, AExportHeight: Integer): TPoint;
var
  vRatio: Double;
begin
  vRatio := AGraphic.Width / AGraphic.Height;
  if vRatio <> 0 then
  begin
    Result.Y := AExportHeight;
    Result.X := Trunc(Result.Y * vRatio);
    if (Result.X > AExportWidth) and (AExportWidth <> 0) then
    begin
      Result.X := AExportWidth;
      Result.Y := Trunc(Result.X / vRatio);
    end;
  end
  else
  begin
    Result.X := AExportWidth;
    Result.Y := AExportHeight;
  end;
end;

function SetBitmapInCenterForExport(const ABitmap: TBitmap;
  const AExportWidth, AExportHeight: Integer;
  const AQuality: Double = 1): Boolean;
{$IFDEF SG_FIREMONKEY}
begin
  Result := False;
{$ELSE}
  {$IFDEF SGFPC}
begin
  Result := False;
  {$ELSE}
var
  vBitmapCentr: TBitmap;
  dX, dY: Integer;
begin
  Result := (ABitmap.Width <> AExportWidth) or (ABitmap.Height <> AExportHeight);
  if Result then
  begin
    vBitmapCentr := TBitmap.Create;
    try
      vBitmapCentr.Assign(ABitmap);
      ABitmap.Width := Ceil(AExportWidth * AQuality);
      ABitmap.Height := Ceil(AExportHeight * AQuality);
      ApplyBitmapTransparent(ABitmap, ABitmap.TransparentColor);
      dX := (ABitmap.Width - vBitmapCentr.Width) div 2;
      dY := (ABitmap.Height - vBitmapCentr.Height) div 2;
      BitBlt(ABitmap.Canvas.Handle, dX, dY, vBitmapCentr.Width, vBitmapCentr.Height, vBitmapCentr.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      vBitmapCentr.Free;
    end;
  end;
  {$ENDIF}
{$ENDIF}
end;

function ColorToRGBString(const AColor: TColor): string;
var
  vRGB: array[0 .. 3] of Byte absolute AColor;
begin
  Result := Format('%d,%d,%d', [vRGB[0], vRGB[1], vRGB[2]]);
end;

function ColorCADToCADString(const AColor: TsgColorCAD; const AByBlock,
  AByLayer, ARed, AYellow, AGreen, AAqua, ABlue, AFuchsia, ABlackWhite,
  APrefix: string): string;
begin
  case AColor.Active of
    acIndexColor:
      begin
        case AColor.Color of
          clDXFByBlock:    Result := AByBlock;
          clDXFByLayer:    Result := AByLayer;
          clDXFRed:        Result := ARed;
          clDXFYellow:     Result := AYellow;
          clDXFGreen:      Result := AGreen;
          clDXFAqua:       Result := AAqua;
          clDXFBlue:       Result := ABlue;
          clDXFFuchsia:    Result := AFuchsia;
          clDXFBlackWhite: Result := ABlackWhite;
        else
          Result := '';
        end;
        if Length(Result) = 0 then
          Result := APrefix + ' ' + IntToStr(AColor.Color);
      end;
    acRGBColor:
      Result := ColorToRGBString(AColor.Color);
  else
    Result := '';
  end;
end;

function ConvertARGBToColorCAD(const AColor: TColor;
  const ACadNone: PsgColorCAD = nil): TsgColorCAD;
begin
  case AColor of
    clWhite:  Result := cnstColorCADByWhite;
    clNone:
      begin
        if ACadNone = nil then
          Result := cnstColorCADNone
        else
          Result := ACadNone^;
      end;
  else
    Result := ConvertColortoColorCAD(AColor);
  end;
end;

function ConvertColorCADToIndexColor(const AColor: TsgColorCAD;
  const ASkipByBlock: Boolean; APalett: PsgCADPalett = nil): Cardinal;
begin
  case AColor.Active of
    acIndexColor: Result := AColor.Color;
  else
    Result := GetCADColor(AColor.Color, ASkipByBlock, APalett);
  end;
end;

function ConvertColorCADToRGB(const AColor: TsgColorCAD; APalett: PsgCADPalett = nil): Cardinal;
begin
  case AColor.Active of
    acIndexColor: Result := IntToColor(AColor.Color, APalett);
  else
    Result := AColor.Color;
  end;
end;

function ConvertColortoColorCAD(const AColor: Cardinal; APalett: PsgCADPalett = nil): TsgColorCAD;
var
  vColorIndex: Integer;
begin
  if APalett = nil then
    APalett := @arrDXFtoRGBColors;
  Result.Active := acIndexColor;
  case AColor of
    clByLayer:  Result.Color := clDXFByLayer;
    clByBlock:  Result.Color := clDXFByBlock;
    clNone:     Result.Color := clDXFBlackWhite;
  else
    vColorIndex := GetCADColor(AColor, False);
    if AColor = APalett^[vColorIndex] then
    begin
      if vColorIndex = 0 then
        vColorIndex := clDXFBlackWhite;
      Result := MakeColorCAD(acIndexColor, vColorIndex);
    end
    else
      Result := MakeColorCAD(acRGBColor, AColor);
  end;
end;

procedure ConvertColorToRGB(const AColor: Cardinal; var R, G, B: Byte);
var
  vColors: array [0..3] of Byte absolute AColor;
begin
  R := vColors[0];//Red
  G := vColors[1];//Green
  B := vColors[2];//Blue
end;

function ConvertRGBtoColor(const R, G, B: Byte; const C: Byte = 0): Integer;
begin
  PByteArray(@Result)^[0] := R;//Red
  PByteArray(@Result)^[1] := G;//Green
  PByteArray(@Result)^[2] := B;//Blue
  PByteArray(@Result)^[3] := C;//Clarity
end;

procedure ConvertRGBtoCMYK(const R, G, B: Byte; var C, M, Y, K: Byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
  K := MinI(C, M);
  if Y < K then
    K := Y;
  if K > 0 then
  begin
    Dec(C, K);
    Dec(M, K);
    Dec(Y, K);
  end;
end;

procedure ConvertRGBtoCMYKFloat(const R, G, B: Byte; var C, M, Y, K: Double);
begin
  C := 1 - R / 255;
  M := 1 - G / 255;
  Y := 1 - B / 255;
  K := Min(C, M);
  if Y < K then
    K := Y;
  if K <> 1.0 then
  begin
    C := (C - K) / (1 - K);
    M := (M - K) / (1 - K);
    Y := (Y - K) / (1 - K);
  end;
end;

function ConvertCMYKtoColor(const C, M, Y, K: Integer): Integer;
begin
  PByteArray(@Result)^[3] := 0;
  ConvertCMYKtoRGB(C, M, Y, K,
    PByteArray(@Result)^[0],
    PByteArray(@Result)^[1],
    PByteArray(@Result)^[2]);
end;

procedure ConvertCMYKtoRGB(const C, M, Y, K: Integer; var R, G, B: Byte);
begin
  R := MaxI(255 - (C + K), 0);
  G := MaxI(255 - (M + K), 0);
  B := MaxI(255 - (Y + K), 0);
end;

function ConvertRGBtoGray(const R, G, B: Byte; const AMode: Byte = 3): Integer;
var
  vColor: Double;
  vGray, vMin, vMax: Byte;
begin
  case AMode of
    1:
    begin
      vMax := MaxI(MaxI(R, G), B);
      vMin := MinI(MinI(R, G), B);
      vColor := (vMax + vMin) * 0.5;
    end;
    2:  vColor := (0.21 * R) + (0.72 * G) + (0.07 * B);
    3:  vColor := (0.299 * R) + (0.587 * G) + (0.114 * B);
  else
    vColor := (R + G + B) / 3;
  end;
  if vColor >= 255 then
    vGray := 255
  else
    if vColor <= 0 then
      vGray := 0
    else
      vGray := Round(vColor);
  Result := ConvertRGBtoColor(vGray, vGray, vGray);
end;

function ConvertColortoGray(const AColor: Cardinal; const AMode: Byte = 3): Integer;
var
  R, G, B: Byte;
begin
  ConvertColorToRGB(AColor, R, G, B);
  Result := ConvertRGBtoGray(R, G, B, AMode);
end;

procedure NormalizeCMYK(var C, M, Y, K: Byte);
var
  vTmp: Byte;
begin
  vTmp := MinI(C, M);
  if Y < vTmp  then
    vTmp := Y;
  if vTmp + K > 255 then
    vTmp := 255 - K;
  Dec(C, vTmp);
  Dec(M, vTmp);
  Dec(Y, vTmp);
  Dec(K, vTmp);
end;

function IsColorCADByBlock(const AColor: TsgColorCAD): Boolean;
begin
  if AColor.Active = acIndexColor then
    Result := AColor.Color = clDXFByBlock
  else
    Result := AColor.Color = clByBlock;
end;

function IsColorCADByLayer(const AColor: TsgColorCAD): Boolean;
begin
  if AColor.Active = acIndexColor then
    Result := AColor.Color = clDXFByLayer
  else
    Result := AColor.Color = clByLayer;
end;

function IsColorCADEqual(const AColor1, AColor2: TsgColorCAD;
  APalett: PsgCADPalett): Boolean;
begin
  if AColor1.Active = AColor2.Active then
    Result := AColor1.Color = AColor2.Color
  else
  begin
    if APalett = nil then
      APalett := @arrDXFtoRGBColors;
    if AColor1.Active = acRGBColor then
      Result := AColor1.Color = APalett^[AColor2.Color]
    else
      Result := AColor2.Color = APalett^[AColor1.Color]
  end;
end;

function CreateColorCAD(const AColor: TsgColorCAD): PsgColorCAD;
begin
  New(Result);
  Result^ := AColor;
end;

function MakeColorCAD(const AType: TsgActiveColor;
  const AValue: Cardinal; const AAlbumString: string = ''): TsgColorCAD;
begin
  Result.Active := AType;
  Result.Color := AValue;
  Result.AlbumString := AAlbumString;
  if (Result.Active = acIndexColor) and (Result.Color > 256) then
    Result.Active := acRGBColor;
end;

function CmEntityColorToColorCAD(ACmEntityColor: Cardinal): TsgColorCAD;
begin
  case Byte(ACmEntityColor shr 24) of
    cmcByBlock:  Result := MakeColorCAD(acIndexColor, clDXFByBlock);
    cmcBGR:  Result := MakeColorCAD(acRGBColor, BGRToRGB(ACmEntityColor));
    cmcIndexed: Result := MakeColorCAD(acIndexColor, ACmEntityColor and $1FF);
    else  Result := MakeColorCAD(acIndexColor, clDXFByLayer);
  end;
end;

function ColorCADToCmEntityColor(const AColor: TsgColorCAD): Cardinal;
begin
  case AColor.Active of
    acIndexColor:
      begin
        case AColor.Color of
          clDXFByBlock: Result := Cardinal(cmcByBlock shl 24) or AColor.Color;
          clDXFByLayer: Result := Cardinal(cmcByLayer) shl 24;
        else
          Result := Cardinal(cmcIndexed shl 24) or (AColor.Color and $FF);
        end;
      end;
  else
    Result := Cardinal(cmcBGR shl 24) or Cardinal(BGRToRGB(AColor.Color));
  end;
end;

function MakeMessageParams(ACode: Integer; AMessage: string): TsgMessageParams;
begin
  Result.Code := ACode;
  Result.Msg := AMessage;
end;

function NormalizeColor(const AColor, AcolorMin, AColorMax: Cardinal): Integer;
var
  I : Integer;
  vColors: array [0..3] of Byte;
  vMin: array [0..3] of Byte;
  vMax: array [0..3] of Byte;
begin
  ConvertColorToRGB(AcolorMin,vMin[0], vMin[1], vMin[2]);
  ConvertColorToRGB(AColorMax,vMax[0], vMax[1], vMax[2]);
  ConvertColorToRGB(AColor,vColors[0], vColors[1], vColors[2]);
  for I := 0 to 2 do
  begin
    vColors[I] := EnsureRange(vColors[I],vMin[I], vMax[I]);
    if vMax[I] <> vMin[I] then
      vColors[I] := 255 * (vColors[I] - vMin[I]) div (vMax[I] - vMin[I]);
  end;
  Result := ConvertRGBtoColor(vColors[0], vColors[1], vColors[2]);
end;

function ConvertDXFToLineWeight(const ADXFWeight: Integer; var ALineWeight: Double): Boolean;
var
  I: Integer;
begin
  if (ADXFWeight >= -3) and (ADXFWeight < 0) then
  begin
    Result := True;
    ALineWeight := ADXFWeight;
  end
  else
  begin
    Result := False;
    for I := Low(sgDXFLineWeights) to High(sgDXFLineWeights) do
      if sgDXFLineWeights[I] = ADXFWeight then
      begin
        ALineWeight := sgDXFLineWeights[I]/100;
        Result := True;
        Exit;
      end;
  end;
end;

function ConvertLineWeightToDXF(const ALineWeight: Double): Integer;
var
  I: Integer;
begin
  if ALineWeight > 0 then
  begin
    Result := Round(ALineWeight * 100);
    if Result >= sgDXFLineWeights[High(sgDXFLineWeights)] then
      Result := sgDXFLineWeights[High(sgDXFLineWeights)]
    else
    begin
      for I := Low(sgDXFLineWeights) to High(sgDXFLineWeights) do
      begin
        if Result < sgDXFLineWeights[I] then
        begin
          Result := sgDXFLineWeights[I - 1];
          Break;
        end;
      end;
    end;
  end
  else
    Result := Round(ALineWeight);
end;

function ConvertLineWeightToDWG(const ALineWeight: Double): Integer;
const
 cnstLineWeightDefault: array[0..3] of Integer =
  (29, 29, 30, 31);
var
  I, vRes: Integer;
begin
  Result := 29;
  if ALineWeight > 0 then
  begin
    vRes := ConvertLineWeightToDXF(ALineWeight);
    for I := Low(sgDXFLineWeights) to High(sgDXFLineWeights) do
    if vRes = sgDXFLineWeights[I] then
    begin
      Result := I;
      Break;
    end;
  end
  else
  begin
    I := Abs(Round(ALineWeight));
    if I <= 3 then
      Result := cnstLineWeightDefault[I];
  end;
end;

function DimSubclassNum(Flags: Byte): Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 6 downto 1 do
  begin
    if (Flags and I) = I then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function GetCrc32FromStream(const AStream: TStream): Cardinal;
var
  vMemoryStream: TMemoryStream;

  function CalcCRC: Cardinal;
  begin
    vMemoryStream.Position := 0;
    Result := CRC32(vMemoryStream.Memory, 0, vMemoryStream.Size);
  end;

begin
  if AStream is TMemoryStream then
  begin
    vMemoryStream := TMemoryStream(AStream);
    Result := CalcCRC;
  end
  else
  begin
    vMemoryStream := TMemoryStream.Create;
    try
      AStream.Position := 0;
      vMemoryStream.CopyFrom(AStream, AStream.Size);
      Result := CalcCRC;
    finally
      vMemoryStream.Free;
    end;
  end;
end;

function GetHash64(var P: PByte; Count: TsgNativeUInt; ASeed: UInt64): UInt64;
const
  LHashShift = 5;
  RHashShift = SizeOf(ASeed) shl 3 - LHashShift;
begin
  Result := ASeed;
  while Count > 0 do
  begin
    Result := Result xor P^;
    Result := (Result shr RHashShift) or (Result shl LHashShift); // ROL Result,LHashShift
    Inc(P);
    Dec(Count);
  end;
end;

function IsXRefInName(const AName: string): Boolean;
begin
  Result := AnsiPos(cnstXRefNameDelimiter, AName) <> 0;
end;


function GetHexTextValue(const AStr: string): string;
var
  I, vLength, vSymbol: Integer;
begin
  Result := '';
  vLength := Length(AStr);
  if vLength > 0 then
  begin
    I := 1;
    while I < vLength do
    begin
      vSymbol := StrToInt('$' + Copy(AStr, I, cnstCharHexSize));
      Result := Result + Char(vSymbol);
      Inc(I, cnstCharHexSize);
    end;
  end;
end;

function SetHexTextValue(const AStr: string): string;
var
  I: Integer;
  vSymbol: string;
begin
  Result := '';
  if Length(AStr) > 0 then
  begin
    for I := 1 to Length(AStr) do
    begin
      vSymbol := IntToHex(Integer(AStr[I]), cnstCharHexSize);
      Result := Result + vSymbol;
    end;
  end;
end;

{$IFNDEF SG_CADIMPORTERDLLDEMO}
function GetCompressTextValue(const AStr: string; const ACompress: Boolean): string;
const
  cnstByteHexSize = 2;
var
  I, vLength: Integer;
  vBuffer: Byte;
  vSymbol: string;
  vStream: TMemoryStream;
begin
  Result := '';
  vStream := TMemoryStream.Create;
  try
    if ACompress then
    begin
      vStream.Write(AStr[1], Length(AStr) * SizeOf(Char));
      vStream.Position := 0;
      if PackStream(vStream) then
      begin
        vStream.Position := 0;
        for I := 1 to vStream.Size do
        begin
          vStream.Read(vBuffer, 1);
          vSymbol := IntToHex(Integer(vBuffer), cnstByteHexSize);
          Result := Result + vSymbol;
        end;
      end;
    end
    else
    begin
      I := 1;
      vLength := Length(AStr);
      while I < vLength do
      begin
        vBuffer := StrToInt('$' + Copy(AStr, I, cnstByteHexSize));
        vStream.Write(vBuffer, 1);
        Inc(I, cnstByteHexSize);
      end;
      vStream.Position := 0;
      if UnPackStream(vStream) then
      begin
        vStream.Position := 0;
        SetLength(Result, vStream.Size div SizeOf(Char));
        vStream.Read(Result[1], vStream.Size);
      end;
    end;
  finally
   vStream.Free;
  end;
end;

function CodingTextValue(const AStr: string; const AEncode: Boolean): string;
begin
  try
    if Length(AStr) > 0 then
      Result := GetCompressTextValue(AStr, AEncode)
    else
      Result := '';
  except
    Result := '';
  end;
end;
{$ENDIF}

{ Function converts table indexes }

{ Function TableIndexToStringCol

  convert column index to alpha symbols, for example:

  1 -> 'A'
  2 -> 'B'
  3 -> 'C'
  ...
  26 -> 'Z'
  27 -> 'AA'
  28 -> 'AB'
  29 -> 'AC'
  ... }

function TableIndexToStringCol(AIndex: Integer): string;
begin
  Result := '';
  repeat
    Dec(AIndex);
    Result := Char(AIndex mod cnstAlphaCount + Byte('A')) + Result;
    AIndex := AIndex div cnstAlphaCount;
  until AIndex = 0;
end;

function TableCellStringToCol(const ASym: string; var ARow: string): Integer;
var
  I, Len, D: Integer;
  S: string;
  FillTail: Boolean;
begin
  Result := 0;
  Len := Length(ASym);
  I := 1;
  S := '';
  ARow := '';
  FillTail := False;
  while I <= Len do
  begin
    if not FillTail and CharInSet(ASym[I], ['A' .. 'Z']) then
      S := ASym[I] + S
    else
    begin
      ARow := ARow + ASym[I];
      FillTail := True;
    end;
    Inc(I);
  end;
  Len := Length(S);
  I := 1;
  if I <= Len then
  begin
    Result := Result + Byte(S[I]) - Byte('A');
    D := cnstAlphaCount;
    Inc(I);
    while I <= Len do
    begin
      Result := Result + D * (Byte(S[I]) - Byte('A') + 1);
      Inc(I);
      D := D shl 1;
    end;
  end;
  Inc(Result);
end;

{ Procedure ParseTableCellString

  parse cell string index to column and row indexes, for example:

  'B3' -> comunm:2, row:3
  'AB7' -> column:27, row:7.

  Return:
    - Lo word: error code position of row index convertion;
    - Hi word: length of string column index. }

function ParseTableCellString(const AStr: string; var ACol, ARow: Integer): Integer;
var
  vRow: string;
begin
  ACol := TableCellStringToCol(AStr, vRow);
  ARow := 0;
  Val(vRow, ARow, Result);
  PWord(Integer(@Result) + 2)^ := Length(AStr) - Length(vRow);
end;

function ClearBrackets(const AStr: string): string;
begin
  Result := AStr;
  if (Length(Result) > 1) and (Result[1] = '(') then
    if (Result[Length(Result)] = ')') then
    begin
      Result := Copy(Result, 2, Length(Result) - 2);
      Result := Trim(Result);
    end;
end;

{ Parser functions for property TOpenDialog.Filter }

function BuildDialogFilter(const AFilterList: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(AFilterList) then
  begin
    for I := 0 to AFilterList.Count - 1 do
      Result := Result + GetFilterFromList(AFilterList, I + 1);
    Delete(Result, 1, 1);
  end;
end;

procedure ClearFilterList(const AFilterList: TStrings);
var
  I: Integer;
begin
  if Assigned(AFilterList) then
  begin
    for I := AFilterList.Count - 1 downto 0 do
      AFilterList.Objects[I].Free;
    AFilterList.Clear;
  end;
end;

function GetFilterFromList(const AFilterList: TStrings;
  const AFilterIndex: Integer): string;
var
  S: string;
  I: Integer;
  vExtList: TStrings;
begin
  Result := cnstVertLine + AFilterList[AFilterIndex - 1] + cnstVertLine;
  vExtList := TStrings(AFilterList.Objects[AFilterIndex - 1]);
  S := cnstAsterisk;
  if vExtList.Count > 0 then
  begin
    I := 0;
    while I < vExtList.Count - 1 do
    begin
      S := S + vExtList[Integer(vExtList.Objects[I])] + cnstFilterSeparator;
      Inc(I);
    end;
    S := S + vExtList[Integer(vExtList.Objects[I])];
  end;
  Result := Result + S;
end;

function GetFilter(const AFilter: string; const AFilterIndex: Integer): string;
var
  vFilter: TsgStringList;
begin
  vFilter := TsgStringList.Create;
  try
    ParseDialogFilter(AFilter, vFilter);
    try
      Result := GetFilterFromList(vFilter, AFilterIndex);
    finally
      ClearFilterList(vFilter);
    end;
  finally
    vFilter.Free;
  end;
end;

function GetExtsByFilterIndex(const AFilter: string; AFilterIndex: Integer): string;
var
  vFilters: TStringList;
  vStrings: TStrings;
begin
  Result := '';
  if AFilterIndex {$IFDEF SG_FM_LINUX}>={$ELSE}>{$ENDIF} 0 then
  begin
    vFilters := TStringList.Create;
    try
      ParseDialogFilter(AFilter, vFilters);
      try
        TStrings(vFilters.Objects[AFilterIndex {$IFNDEF SG_FM_LINUX}- 1{$ENDIF}]).IndexOfObject(nil);
        vStrings := TStrings(vFilters.Objects[AFilterIndex {$IFNDEF SG_FM_LINUX}- 1{$ENDIF}]);
        Result := vStrings.Text;
      finally
        ClearFilterList(vFilters);
      end;
    finally
      vFilters.Free;
    end;
  end;
end;

function GetExtByFilterIndex(const AFilter: string; AFilterIndex: Integer): string;
var
  vFilters: TStringList;
  vDefIndex: Integer;
  vStrings: TStrings;
begin
  Result := '';
  if AFilterIndex > 0 then
  begin
    vFilters := TStringList.Create;
    try
      ParseDialogFilter(AFilter, vFilters);
      try
        vDefIndex := TStrings(vFilters.Objects[AFilterIndex - 1]).IndexOfObject(nil);
        vStrings := TStrings(vFilters.Objects[AFilterIndex - 1]);
        Result := vStrings[vDefIndex];
      finally
        ClearFilterList(vFilters);
      end;
    finally
      vFilters.Free;
    end;
  end;
end;

function GetFilterIndexFromList(const AFilterList: TStrings;
  const AExt: string): Integer;
var
  I: Integer;
begin
  if Assigned(AFilterList) then
  begin
    I := AFilterList.Count - 1;
    while (I >= 0) and (TStrings(AFilterList.Objects[I]).IndexOf(AExt) < 0) do
      Dec(I);
    Result := I {$IFNDEF SG_FM_LINUX}+ 1{$ENDIF};
  end
  else
    Result := 0;
end;

function GetFilterIndexFromList(const AFilterList: TStrings; const ADWGVer: TsgDWGVersion): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(AFilterList) then
    for I := 0 to AFilterList.Count - 1 do
      if StringPos(cnstDWGVersionStr[ADWGVer].Name, AFilterList[I], 1) >= 1 then
      begin
        Result := I {$IFNDEF SG_FM_LINUX}+ 1{$ENDIF};
        Break;
      end;
end;

function GetFilterIndexByExt(const AFilter: string; const AExt: string): Integer;
var
  vFilter: TsgStringList;
begin
  vFilter := TsgStringList.Create;
  try
    ParseDialogFilter(AFilter, vFilter);
    try
      Result := GetFilterIndexFromList(vFilter, AExt);
    finally
      ClearFilterList(vFilter);
    end;
  finally
    vFilter.Free;
  end;
end;

procedure ParseDialogFilter(const AFilter: string; const AFilterList: TStrings);
var
  I, J: Integer;
  vExts: string;
  vFilter, vExtList: TsgStringList;
begin
  if Assigned(AFilterList) then
  begin
    vFilter := TsgStringList.Create;
    try
      vFilter.LineBreak := cnstVertLine;
      vFilter.Text := AFilter;
      AFilterList.Clear;
      I := 0;
      while I < vFilter.Count do
      begin
        vExtList := TsgStringList.Create;
        vExtList.LineBreak := cnstFilterSeparator;
        vExtList.CaseSensitive := False;
        vExtList.Duplicates := dupIgnore;
        AFilterList.AddObject(vFilter[I], vExtList);
        Inc(I);
        vExts := ';' + vFilter[I];
        vExtList.Text := vExts;
        vExtList.Delete(0);
        for J := 0 to vExtList.Count - 1 do
          vExtList.Objects[J] := TObject(J);
        vExtList.Sort;
        Inc(I);
      end;
    finally
      vFilter.Free;
    end;
  end;
end;

{ Graphics function }

function CheckMaxSize(const AMaxSize: Double; var AWidth, AHeight: Double): Boolean; overload;
begin
  if AWidth > AHeight then
    Result := AWidth > AMaxSize
  else
    Result := AHeight > AMaxSize;
  if Result then
    SetMaxSize(AMaxSize, AWidth, AHeight);
end;

function CheckMaxSize(const AMaxSize: Double; var AWidth, AHeight: Integer): Boolean; overload;
var
  vW, vH: Double;
begin
  vW := AWidth;
  vH := AHeight;
  Result := CheckMaxSize(AMaxSize, vW, vH);
  AWidth := Round(vW);
  AHeight := Round(vH);
end;

procedure SetMaxSize(const AMaxSize: Double; var AWidth, AHeight: Double); overload;
begin
  if AWidth > AHeight then
  begin
    AHeight := AMaxSize * AHeight / AWidth;
    AWidth := AMaxSize;
  end
  else
  begin
    AWidth := AMaxSize * AWidth / AHeight;
    AHeight := AMaxSize;
  end;
end;

procedure SetMaxSize(const AMaxSize: Double; var AWidth, AHeight: Integer); overload;
var
  vW, vH: Double;
begin
  vW := AWidth;
  vH := AHeight;
  SetMaxSize(AMaxSize, vW, vH);
  AWidth := Round(vW);
  AHeight := Round(vH);
end;

function CreateCopyGraphic(const AGraphic: TPersistent): TPersistent;
begin
  if AGraphic is TGraphic then
    Result := TPersistent(CreateCopyGraphic(TGraphic(AGraphic)))
  else
  begin
    try
      Result := TPersistent(TPersistentClass(AGraphic.ClassType).Create);
      Result.Assign(AGraphic);
    except
      FreeAndNil(Result);
    end;
  end;
end;

function CreateCopyGraphic(const AGraphic: TGraphic): TGraphic;
begin
  try
    Result := TGraphic(TGraphicClass(AGraphic.ClassType).Create);
    Result.Assign(AGraphic);
  except
    FreeAndNil(Result);
    Result := CreateCopyGraphicAsBitmap(AGraphic);
  end;
end;

function OpenPictureByMemmoryStreamSafe(const AMemStream: TMemoryStream;
  const AFileName: string; const AFileExt: string;
  const AResult: PInteger = nil): TPersistent;
var
  vFullFileName: string;
begin
  Result := nil;
  vFullFileName := GetTempDir;
  if (Length(vFullFileName) > 0) and HasAccessByWrite(vFullFileName)  then
  begin
    vFullFileName := vFullFileName + AFileName +  IntToHex(TsgNativeInt(AMemStream), 0) + '.' + AFileExt;
    Result := OpenPictureByMemmoryStream(AMemStream, vFullFileName, AResult);
  end;
end;

function OpenPictureByMemmoryStream(const AMemStream: TMemoryStream;
  const AFullFileName: string; const AResult: PInteger = nil): TPersistent;
var
  vPicture: TPicture;
begin
  Result := nil;
  if AResult <> nil then
    AResult^ := 0;
  vPicture := TPicture.Create;
  try
    try
      AMemStream.SaveToFile(AFullFileName);
      try
        vPicture.LoadFromFile(AFullFileName);
        if vPicture.Graphic <> nil then
        begin
          Result := CreateCopyGraphic(vPicture.Graphic);
          if AResult <> nil then
            AResult^ := 1;
        end;
      finally
        try
          DeleteFile(AFullFileName);
        except
{$IFDEF _FIXINSIGHT_}
      on E: Exception do EmptyExceptionHandler(E);
{$ENDIF}
        end;
      end;
    except
      if AResult <> nil then
        AResult^ := -1;
      try
        DeleteFile(AFullFileName);
      except
{$IFDEF _FIXINSIGHT_}
      on E: Exception do EmptyExceptionHandler(E);
{$ENDIF}
      end;
    end;
  finally
    vPicture.Free;
  end;
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function CreateCopyIconAsBitmap(const AIcon: TIcon): TGraphic;
var
  vii: _ICONINFO;
begin
  Result := TBitmap.Create;
  Windows.GetIconInfo(AIcon.Handle, vii);
  SetSizeGraphic(Result, AIcon.Width, AIcon.Height);
  TBitmap(Result).MaskHandle := vii.hbmMask;
  TBitmap(Result).Palette := AIcon.Palette;
  TBitmap(Result).Canvas.Draw(0, 0, AIcon);
end;
{$ENDIF}

function CreateCopyBitmapAsBitmap(const ABitmap: TBitmap): TGraphic;
{$IFDEF SG_FIREMONKEY}
begin
  TBitmap(Result) := TBitmap.Create;//change in future version
  TBitmap(Result).Assign(ABitmap);
{$ELSE}
begin
  TBitmap(Result) := TBitmap.Create;
  if TBitmap(Result).PixelFormat in [pfDevice, pfCustom] then
  begin
    TBitmap(Result).PixelFormat := pf24bit;
    SetSizeBmp(TBitmap(Result), ABitmap.Width, ABitmap.Height);
    TBitmap(Result).Canvas.StretchDraw(TBitmap(Result).Canvas.ClipRect, ABitmap);
  end
  else
    TBitmap(Result).Assign(ABitmap);
{$ENDIF}
end;

function CreateCopyAsBitmap(const AGraphic: TGraphic): TGraphic;
{$IFDEF SG_FIREMONKEY}
begin
  Result := nil;
{$ELSE}
var
  vBitmap: TBitmap absolute Result;
  vWidth, vHeight, vMax: Integer;
begin
  Result := TBitmap.Create;
  vWidth := AGraphic.Width;
  vHeight := AGraphic.Height;
  vMax := MaxI(vWidth, vHeight);
  if vMax > cnstMaxImgSize then
  begin
    vWidth := cnstMaxImgSize * vWidth div vMax;
    vHeight := cnstMaxImgSize * vHeight div vMax;
  end;
  if vWidth < 1 then  vWidth := 1;
  if vHeight < 1 then vHeight := 1;
  TBitmap(Result).PixelFormat := pf24bit;
  SetSizeGraphic(Result, vWidth, vHeight);
  TBitmap(Result).Canvas.StretchDraw(Rect(0, 0, vWidth, vHeight), AGraphic);
{$ENDIF}
end;

function CreateCopyGraphicAsBitmap(const AGraphic: TGraphic;
  const AIsGdiObject: Boolean = true): TGraphic;
{$IFDEF SG_FIREMONKEY}
begin
  Result := nil;
{$ELSE}
var
  vBitmap: TGraphic;
begin
  Result := nil;
  try
    if AGraphic is TIcon then
    {$IFNDEF SG_NON_WIN_PLATFORM}
      vBitmap := CreateCopyIconAsBitmap(TIcon(AGraphic))
     {$ELSE}
      vBitmap := nil
     {$ENDIF}
    else
    begin
      if AGraphic is TBitMap then
      begin
        try
          vBitmap := CreateCopyBitmapAsBitmap(TBitMap(AGraphic))
        except
          FreeAndNil(vBitmap);
          vBitmap := CreateCopyAsBitmap(AGraphic);
        end;
      end
      else
        vBitmap := CreateCopyAsBitmap(AGraphic);
    end;
    if AIsGdiObject then
      Result := vBitmap
    else
    begin
{$IFNDEF SG_CADIMPORTERDLLDEMO}
      try
        Result := sgBitmap.TsgBitmap.Create;
        Result.Assign(vBitmap);
      finally
        FreeAndNil(vBitmap);
      end;
{$ELSE}
      Result := vBitmap;
{$ENDIF}
    end
  except
    if Result <> vBitmap then
      FreeAndNil(vBitmap);
    FreeAndNil(Result);
  end;
{$ENDIF}
end;

function GetGraphicPixelFormat(const AGraphic: TGraphic; AIsBitmap: PBoolean = nil):  {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
var
  vIsBitmap: Boolean;
begin
  if AIsBitmap = nil then AIsBitmap := @vIsBitmap;
  Result := pfCustom;
{$IFDEF SG_FIREMONKEY}
  AIsBitmap^ := False;
  if Assigned(AGraphic) then
  begin
    AIsBitmap^ := TObject(AGraphic) is FMX.Graphics.TBitmap;
    if AIsBitmap^ then
      Result := TBitmap(AGraphic).PixelFormat
    else
      Result := AGraphic.GetBitmapPixelFormat;
  end
{$ELSE}
  AIsBitmap^ := TObject(AGraphic) is Graphics.TBitmap;
  if AIsBitmap^ then
    Result := Graphics.TBitmap(AGraphic).PixelFormat
{$ENDIF}
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  {$IFNDEF SG_FIREMONKEY}
  else
  begin
    AIsBitmap^ := AGraphic is sgBitmap.TsgBitmap;
    if AIsBitmap^ then
      Result := sgBitmap.TsgBitmap(AGraphic).PixelFormat;
  end;
  {$ENDIF}
{$ELSE}
  ;
{$ENDIF}
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function GetMaxMetafileSize(const AIsEnhaned: Boolean): Integer;
const
  iMaxMetafileSizeWinNT = 64000000;
  iMaxMetafileSizeWin98 = 28000;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if AIsEnhaned then
      Result := iMaxMetafileSizeWinNT
    else     // wmf
      Result := 28000;
  end
  else
  begin
    if AIsEnhaned then
      Result := iMaxMetafileSizeWin98
    else     // wmf
      Result := 16000;//26000
  end;
end;

function GetDefaultMetafileSize(const AIsEnhaned: Boolean): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if AIsEnhaned then
      Result := 128000
    else     // wmf
      Result := 28000;
  end
  else
  begin
    if AIsEnhaned then
      Result := 28000
    else     // wmf
      Result := 16000;//26000
  end;
end;
{$ENDIF}

procedure SetMaxMetafileExtents(AWidth, AHeight: Double;
  var AMetWidth, AMetHeight: Integer; AMaxExtents: Integer = -1);
const
  cnstMaxMetafileSize = 100000;
begin
  if AMaxExtents <= 0 then
    AMaxExtents := cnstMaxMetafileSize;
  if AWidth = 0 then
    AWidth := 1;
  if AHeight = 0 then
    AHeight := 1;
  SetMaxSize(AMaxExtents, AWidth, AHeight);
  AMetWidth := Round(AWidth);
  AMetHeight := Round(AHeight);
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
type
{$IFDEF CLR}
  TFileFormat = class(TObject)
  public
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: string;
  end;

  TFileFormatType = TFileFormat;
{$ELSE}
  PFileFormat = ^TFileFormat;
  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  TFileFormatType = PFileFormat;
{$ENDIF}

var
  VclGraphicsFileFormatsPtr: Pointer = nil;

procedure GetGraphicsFileFormatsPtr(var APFileFormats: Pointer);
const
  cmpOp = $3D83;
var
  I: Integer;
  code: array[Byte] of Byte;
begin
  APFileFormats := nil;
  CodeMove(@TPicture.UnregisterGraphicClass, code, SizeOf(code));
  I := 1;
  while (I < Length(code) - 2) and (PWord(@code[I])^ <> cmpOp) do Inc(I);
  if I < Length(code) - 2 then
  begin
{$IFDEF SG_CPUX64}
    APFileFormats := Pointer(NativeInt(@TPicture.UnregisterGraphicClass) + I + 7);
{$ENDIF}
    Inc(PByte(APFileFormats), PInteger(@code[I+2])^);
  end;
end;

function GraphicClass(AExt: string): TGraphicClass;
type
  PFileFormatTypeArray = ^TFileFormatTypeArray;
  TFileFormatTypeArray = array[Byte] of TFileFormatType;
var
  I: Integer;
  P: PFileFormatTypeArray;
begin
  Result := nil;
  try
    if VclGraphicsFileFormatsPtr = nil then
      GetGraphicsFileFormatsPtr(VclGraphicsFileFormatsPtr);
    if VclGraphicsFileFormatsPtr = nil then Exit;
    AExt := AnsiLowerCase(AExt);
    if (AExt <> '') and (AExt[1] = '.') then Delete(AExt, 1, 1);
    I := TList(VclGraphicsFileFormatsPtr^).Count - 1;
    if I > 0 then
    begin
      P := PFileFormatTypeArray(@TList(VclGraphicsFileFormatsPtr^).List[0]);
      while (I >= 0) and not Assigned(Result) do
        if P^[I]^.Extension = AExt then
          Result := P^[I]^.GraphicClass
        else
          Dec(I);
    end;
  except
{$IFDEF _FIXINSIGHT_}
    on E: Exception do EmptyExceptionHandler(E);
{$ENDIF}
  end;
end;
{$ENDIF}

procedure ApplyBitmapTransparent(const ABitmap: TBitmap; const ATransparentColor: Integer);
{$IFDEF SG_FIREMONKEY}
begin
end;
{$ELSE}
var
  vRect: TRect;
begin
  ABitmap.TransparentColor := ATransparentColor;
  ABitmap.Canvas.Pen.Color := ATransparentColor;
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := ATransparentColor;
  vRect := ABitmap.Canvas.ClipRect;
  Inc(vRect.Right);
  Inc(vRect.Bottom);
  ABitmap.Canvas.FillRect(vRect);
end;
{$ENDIF}

procedure SetBitmapTransparent(const ABitmap: TBitmap;
  const ATransparent: Boolean; const ATransparentColor: Integer = clEmpty);
{$IFDEF SG_FIREMONKEY}
begin
end;
{$ELSE}
begin
  ABitmap.Transparent := ATransparent;
  if ATransparent then
  begin
    ABitmap.TransparentMode := tmAuto;
    if ATransparentColor <> clEmpty then
    begin
      ABitmap.TransparentMode := tmFixed;
      ApplyBitmapTransparent(ABitmap, ATransparentColor);
    end;
  end;
end;
{$ENDIF}

function GetSizeGraphic(const AGraphic: TPersistent): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  if AGraphic <> nil then
  begin
    if AGraphic is TBitmap then
    begin
      Result.cx := TBitmap(AGraphic).Width;
      Result.cy := TBitmap(AGraphic).Height;
    end
    else
    begin
      Result.cx := TGraphic(AGraphic).Width;
      Result.cy := TGraphic(AGraphic).Height;
    end;
  end;
end;

procedure SetSizeGraphic(const AGraphic: TPersistent; const Width, Height: Integer); overload;
begin
{$IFDEF SG_FIREMONKEY}
  if AGraphic is TBitmap then
    TBitmap(AGraphic).Size := TSize.Create(Width, Height)
  else
  begin
    TGraphic(AGraphic).Width := Width;
    TGraphic(AGraphic).Height := Height;
  end;
{$ELSE}
  {$IFDEF SGDEL_2006}
    TGraphic(AGraphic).SetSize(Width, Height);
  {$ELSE}
    TGraphic(AGraphic).Width := Width;
    TGraphic(AGraphic).Height := Height;
  {$ENDIF}
{$ENDIF}
end;

procedure SetSizeGraphic(ADst, ASrc: TPersistent); overload;
begin
{$IFDEF SG_FIREMONKEY}
  if ASrc is TBitmap then
    SetSizeGraphic(ADst, TBitmap(ASrc).Width, TBitmap(ASrc).Height)
  else
    SetSizeGraphic(ADst, TGraphic(ASrc).Width, TGraphic(ASrc).Height);
{$ELSE}
  SetSizeGraphic(ADst, TGraphic(ASrc).Width, TGraphic(ASrc).Height);
{$ENDIF}
end;

procedure SetSizeBmp(const ABmp: TBitmap; Width, Height: Integer;
  const AMode: Byte = 0; const AMaxImgSize: Integer = cnstMaxImgSize);
var
  vMax: Integer;
  vKoef: Double;
begin
  if AMode and 1 = 1 then//check min size
  begin
    if Width < 1 then
      Width := 1;
    if Height < 1 then
      Height := 1;
  end;
  if AMode and 4 <> 0 then//set max size
  begin
    vMax := MaxI(Width, Height);
    if vMax = 0 then
      vMax := 1;
    vKoef := AMaxImgSize / vMax;
    Width := Ceil(vKoef * Width);
    Height := Ceil(vKoef * Height);
  end
  else
  begin
    if AMode and 2 <> 0 then//check max size
    begin
      if Width > Height then
      begin
        if Width > AMaxImgSize then
        begin
          vKoef := Height / Width;
          Width := AMaxImgSize;
          Height := Ceil(vKoef * AMaxImgSize);
          if Height = 0 then
            Height := 1;
        end;
      end
      else
      begin
        if Height > AMaxImgSize then
        begin
          vKoef := Width / Height;
          Height := AMaxImgSize;
          Width := Ceil(vKoef * AMaxImgSize);
          if Width = 0 then
            Width := 1;
        end;
      end;
    end;
  end;
  SetSizeGraphic(TGraphic(ABmp), Width, Height);
end;

procedure DoRotateBitmap(ABitmap: TBitmap; AAngle: Extended; ARotToSource: PFMatrix = nil);
var
  vSrcRect: TRect;
  vRotBitmap: TBitmap;
  vTmpMatrix: TFMatrix;
  vRotBounds: TFRect;

  function DoRot8_15_16_24_32(S, D: TBitmap): Integer;
  var
    X, Y: Integer;
    vSrcLines, vDstLines: Pointer;
    vSrcBytesPerLine, vDstBytesPerLine: Integer;
{$IFDEF SGFPC}
    vImage: TLazIntfImage;
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
    vSrcMap, vDstMap: TBitmapData;
{$ENDIF}
    vRotLine, vSrcLine: Pointer;
    vSrcPoint: TPoint;
  begin
    case D.PixelFormat of
      pf24bit: Result := 3;
      pf8bit: Result := 1;
      pf15bit, pf16bit: Result := 2;
    else
      Result := 4;
    end;
{$IFDEF SG_FIREMONKEY}
    S.Map(TMapAccess.Read, vSrcMap);
    D.Map(TMapAccess.ReadWrite, vDstMap);
    try
    vSrcLines := vSrcMap.Data;
    vSrcBytesPerLine := vSrcMap.BytesPerLine;
    vDstLines := vDstMap.Data;
    vDstBytesPerLine := vDstMap.BytesPerLine;
{$ELSE}
    vSrcLines := S.ScanLine[{$IFDEF SGFPC}0{$ELSE}S.Height - 1{$ENDIF}];
    vSrcBytesPerLine := BytesPerScanline(S.Width, Result shl 3, 32);
{$IFDEF SGFPC}
    vImage := D.CreateIntfImage;
    try
      vDstLines := vImage.GetDataLineStart(0);
{$ELSE}
    vDstLines := D.ScanLine[D.Height - 1];
{$ENDIF}
    vDstBytesPerLine := BytesPerScanline(D.Width, Result shl 3, 32);
{$ENDIF}
    for Y := 0 to D.Height - 1 do
    begin
      vRotLine := Pointer(TsgNativeInt(vDstLines) + (D.Height - Y - 1) * vDstBytesPerLine);
      for X := 0 to D.Width - 1 do
      begin
        vSrcPoint := FPointXMat2DLongint(MakeFPoint(X, Y, 0), ARotToSource^);
        if PtInRect(vSrcRect, vSrcPoint) then
        begin
          vSrcLine := Pointer(TsgNativeInt(vSrcLines) + vSrcPoint.Y * vSrcBytesPerLine);
          System.Move(PByte(TsgNativeInt(vSrcLine) + vSrcPoint.X * Result)^, vRotLine^, Result);
        end;
        Inc(PByte(vRotLine), Result);
      end;
    end;
{$IFDEF SGFPC}
      D.LoadFromIntfImage(vImage);
    finally
      vImage.Free;
    end;
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
    finally
      S.Unmap(vSrcMap);
      D.Unmap(vDstMap);
    end;
{$ENDIF}
  end;

  procedure DoRotCustom(S, D: TBitmap);
  var
    X, Y: Integer;
    vSrcPoint: TPoint;
  begin
    for Y := 0 to D.Height - 1 do
      for X := 0 to D.Width - 1 do
      begin
        vSrcPoint := FPointXMat2DLongint(MakeFPoint(X, Y, 0), ARotToSource^);
        if PtInRect(vSrcRect, vSrcPoint) then
          D.Canvas.Pixels[X, Y] := S.Canvas.Pixels[vSrcPoint.X, S.Height - vSrcPoint.Y - 1];
      end;
  end;

begin
  if ARotToSource = nil then ARotToSource := @vTmpMatrix;
  ARotToSource^ := BuildRotMatrix(axisZ, DegToRad(AAngle));
  ARotToSource^ := FMatScale(ARotToSource^, MakeFPoint(1, -1, 1));
  vSrcRect := Rect(0, 0, ABitmap.Width, -ABitmap.Height);
  vRotBounds := GetRealBox(MakeFRectFromRect(vSrcRect), ARotToSource^);
  vRotBounds.BottomRight := SubFPoint(vRotBounds.BottomRight, vRotBounds.TopLeft);
  vRotBounds.TopLeft := cnstFPointZero;
  ARotToSource^.E0 := AffineTransformPoint(MakeFPoint(0.5 * ABitmap.Width, 0.5 * ABitmap.Height, 0), ARotToSource^);
  ARotToSource^.E0.X := 0.5 * (vRotBounds.Left + vRotBounds.Right) - ARotToSource^.E0.X;
  ARotToSource^.E0.Y := 0.5 * (vRotBounds.Top + vRotBounds.Bottom) - ARotToSource^.E0.Y + (vRotBounds.Top - vRotBounds.Bottom);
  ARotToSource^.E0.Z := 0.5 * (vRotBounds.Z1 + vRotBounds.Z2) - ARotToSource^.E0.Z;
  InvertMatrix(ARotToSource^);
  vRotBitmap := TBitmap.Create;
  try
    vRotBitmap.Assign(ABitmap);
    SetSizeGraphic(vRotBitmap, Round(vRotBounds.Right - vRotBounds.Left),
      Round(vRotBounds.Top - vRotBounds.Bottom));
    vRotBitmap.Canvas.Brush := ABitmap.Canvas.Brush;
    vRotBitmap.Canvas.FillRect(vRotBitmap.Canvas.ClipRect);
    vSrcRect.Bottom := -vSrcRect.Bottom;
    if vRotBitmap.PixelFormat in [pf8bit, pf15bit, pf16bit, pf24bit, pf32bit] then
      DoRot8_15_16_24_32(ABitmap, vRotBitmap)
    else
      DoRotCustom(ABitmap, vRotBitmap);
    ABitmap.Assign(vRotBitmap);
    //ABitmap.Canvas.Draw(0, 0, vRotBitmap);
  finally
    vRotBitmap.Free;
  end;
end;

procedure IniScreenParams;
begin
{$IFNDEF SG_FIREMONKEY}
  sgConsts.KoefDeviceCaps := 96 / GetDeviceCap(LOGPIXELSY);
{$ELSE}
  sgConsts.KoefDeviceCaps := 96 / GetPixelsPerInch;
{$ENDIF}
  sgConsts.ScreenResolution := GetScreenResolution;
end;

procedure SetPFPointsListCount(var APoints: TList; const ACount: Integer);
var
  I, vDelta: Integer;
begin
  if ACount <= 0 then
    FreeRecordList(APoints)
  else
  begin
    if APoints = nil then
      APoints := TList.Create;
    vDelta := ACount - APoints.Count;
    I := 0;
    while I < Abs(vDelta) do
    begin
      if vDelta > 0 then
        AddFPointInList(APoints, cnstFPointZero)
      else
      begin
        Dispose(PFPoint(APoints[APoints.Count - 1]));
        APoints.Count := APoints.Count - 1;
      end;
      Inc(I);
    end;
  end;
end;

function sgCodePageFromDWG(const DWGCodePage: Byte;
  const ADefaultCodePage: Word = CP_ACP): Integer;
begin
  Result := ADefaultCodePage;
  if DWGCodePage <= High(arDWGCodePages) then
    Result := arDWGCodePages[DWGCodePage];
end;

function sgDWGCodePageFromCP(const CodePage: Integer; ConverToRealCP: Boolean = False): Word;
var
  vCodePage, I: Integer;
begin
  Result := 0;
  vCodePage := CodePage;
  if ConverToRealCP and (vCodePage = CP_ACP) then
    vCodePage := LCIDToCodePage(SysLocale.DefaultLCID);
  for I := Low(arDWGCodePages) to High(arDWGCodePages) do
    if arDWGCodePages[I] = vCodePage then
    begin
      Result := I;
      Break;
    end;
  if (Result = 0) and (CodePage <> CP_ACP) and ConverToRealCP then
    Result := sgDWGCodePageFromCP(CP_ACP, ConverToRealCP);
end;

function sgDWGVerToString(const Version: TsgDWGVersion;
  const AName: Boolean = False; const AUseAutoCADName: Boolean = True): string;
begin
  if AName then
  begin
    Result := cnstDWGVersionStr[Version].Name;
    if AUseAutoCADName then
      Result := cnstAutocadName + ' ' + Result;
  end
  else
    Result := cnstAutocadValue + cnstDWGVersionStr[Version].Value;
end;

function sgStringToDWGVersion(const AString: string;
  const ADefault: TsgDWGVersion = acR2000): TsgDWGVersion;
var
  V: TsgDWGVersion;
  vVer: string;
  vFind: Boolean;
  vIndex: Integer;
begin
  Result := ADefault;
  vFind := False;
  vVer := AnsiUpperCase(AString);
  vIndex := AnsiPos(cnstAutocadValue, vVer);
  if vIndex > 0 then
  begin
    Delete(vVer, 1, vIndex + Length(cnstAutocadValue) - 1);
    for V := Low(cnstDWGVersionStr) to High(cnstDWGVersionStr) do
    begin
      if cnstDWGVersionStr[V].Value = vVer then
      begin
        Result := V;
        vFind := True;
        Break;
      end;
    end;
  end
  else
  begin
    for V := Low(cnstDWGVersionStr) to High(cnstDWGVersionStr) do
    begin
      if AnsiPos(cnstDWGVersionStr[V].Name, vVer) > 0 then
      begin
        Result := V;
        vFind := True;
        Break;
      end;
    end;
  end;
  if (not vFind) then
  begin
    Result := ADefault;
{$IFDEF SG_BTI_MOLDOVA}
    if AnsiPos('DXF', vVer) > 0 then
      Result := cnstSaveCADVersionMoldova;
{$ENDIF}
  end;
end;

function sgDXFCodePageName(const CodePage: Integer): string;
var
  vCodePage: Integer;
begin
  vCodePage := CodePage;
  if vCodePage = CP_ACP then
    vCodePage := LCIDToCodePage(SysLocale.DefaultLCID);
{$IFNDEF SG_CADIMPORTERDLLDEMO}
  Result := GetDWGCPNameByCodePage(vCodePage);
{$ENDIF}
  if Result = '' then
    Result := 'ANSI_' + IntToStr(vCodePage);
end;

function sgDWG2004Coding(Data: Pointer; Size: Cardinal; ASeed: Cardinal = 1): Cardinal;
var
  I: Cardinal;
begin
  Result := ASeed;
  I := 0;
  while I < Size do
  begin
    Result := Result * cntDWG2004RSeedMul;
    Result := Result + cntDWG2004RSeedPlus;
    PByte(Data)^ := PByte(Data)^ xor (Result shr $10);
    Inc(PByte(Data));
    Inc(I);
  end;
end;

function sgDWG2004Decode(Data: Pointer; Size: Cardinal): Cardinal;
begin
  Result := sgDWG2004Coding(Data, Size);
end;

function sgDWG2004Encode(Data: Pointer; Size: Cardinal): Cardinal;
begin
  Result := sgDWG2004Coding(Data, Size);
end;

procedure sgDWG2004PageHeaderEncode(AData: Pointer; AOffset: Cardinal);
begin
  sgDWG2004PageHeaderDecode(AData, AOffset);
end;

procedure sgDWG2004PageHeaderDecode(AData: Pointer; AOffset: Cardinal);
type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0 .. 7] of Cardinal;
var
  I: Integer;
  vSecMask: Cardinal;
begin
  vSecMask := AOffset xor $4164536B;
  for I := 0 to 7 do
    PCardinalArray(AData)^[I] := PCardinalArray(AData)^[I] xor vSecMask;
end;

function sgDWGAlignSize(const Length: Cardinal; const Step: Cardinal = $20): Integer;
begin
  if (Length = 0) or (Step = 0) then
    raise Exception.Create(sDWGFileError);
  Result := (Step - 1) - (Length - 1) mod Step;
end;

function CRC8(P: PByte; Init: Word; N: Cardinal): Word;
var
  vIndex: Byte;
begin
  Result := Init;
  while N > 0 do
  begin
    vIndex := P^ xor (Result and $FF);
    Result := (Result shr 8) and $FF;
    Result := Result xor cntCRC8Table[vIndex];
    Inc(P);
    Dec(N);
  end;
end;

function sgDWG2004SectionCheckSum(const Data: Pointer; Seed, Size: Cardinal): Cardinal;
var
  vSum1, vSum2, vChunkSize: Cardinal;
  P: PByte;
  I: Cardinal;
begin
  vSum1 := Seed and $FFFF;
  vSum2 := Seed shr $10;
  P := PByte(Data);
  while Size <> 0 do
  begin
    vChunkSize := Math.Min($15B0, Size);
    Size := Size - vChunkSize;
    I := 0;
    while I < vChunkSize do
    begin
      vSum1 := vSum1 * P^;
      vSum2 := vSum2 + vSum1;
      Inc(P);
      Inc(I);
    end;
    vSum1 := vSum1 mod $FFF1;
    vSum2 := vSum2 mod $FFF1;
  end;
  Result := (vSum2 shl $10) or (vSum1 and $FFFF);
end;

function CRC32(const P: PByte; Seed, N: Cardinal): Cardinal;
var
  vByte: Byte;
  P1: PByte;
begin
  Result := not Seed;
  P1 := P;
  while N > 0 do
  begin
    vByte := P1^;
    Result := (Result shr 8) xor cntCRC32Table[(Result xor vByte) and $FF];
    Inc(P1);
    Dec(N);
  end;
  Result := not Result;
end;

//OdUint32 invertedCrc = ~seed
//while (n--) {
//OdUInt8 byte = *p++; invertedCrc = (invertedCrc >> 8) ^ crc32Table[(invertedCrc ^ byte) & 0xff];
//} return ~invertedCrc;


function Checksum(const P: PByte; Seed, N: Cardinal): Cardinal;
var
  vSum1, vSum2, vChunkSize: Cardinal;
  I: Cardinal;
  P1: PByte;
begin
  vSum1 := Seed and $FFFF;
  vSum2 := Seed shr $10;
  P1 := P;
  while N > 0 do //while N <> 0 do
  begin
    vChunkSize := Math.Min($15B0, N);
    Dec(N, vChunkSize);
    I := 0;
    while I < vChunkSize do
    begin
     Inc(vSum1, P1^);
     Inc(P1);
     Inc(vSum2, vSum1);
     Inc(I);
    end;
    vSum1 := vSum1 mod $FFF1;
    vSum2 := vSum2 mod $FFF1;
  end;
  Result := (vSum2 shl $10) or (vSum1 and $FFFF);
end;

function CreateDWGHandle(const Handle: UInt64; const Code: Byte): TsgDWGHandle;
begin
  Result.Handle := Handle;
  Result.Code := Code;
end;

function CreateDWGHandleRef(const Handle, RefHandle: UInt64): TsgDWGHandle;
var
  vHandle: UInt64;
  vCode: Byte;
  vValue: Int64;
begin
  vValue := RefHandle - Handle;
  case vValue of
    -1:
      begin
        vCode := cntDWGObjHandleType6;
        vHandle := 0;
      end;
    1:
      begin
        vCode := cntDWGObjHandleType8;
        vHandle := 0;
      end;
  else
    begin
      if RefHandle > Handle then
      begin
        vCode := cntDWGObjHandleTypeC;
        vHandle := RefHandle - Handle;
      end
      else
      begin
        vCode := cntDWGObjHandleTypeA;
        vHandle := Handle - RefHandle;
      end;
    end;
  end;

  Result.Handle := vHandle;
  Result.Code := vCode;
end;

function StrPosLen(const Str1, Str2: PByte; Str1Len, Str2Len: TsgPrNativeUInt;
  var AFoundAdr: sgConsts.TsgNativeUInt): PByte;
var
  I: TsgPrNativeUInt;
  Find: Boolean;
  P1, P2: PByte;
begin
  Result := nil;
  if Str1Len < Str2Len then
    Exit;
  I := 0;
  AFoundAdr := 0;
  Find := False;
  P1 := Str1;
  P2 := Str2;
  while I < Str1Len do
  begin
    if P1^ = P2^ then
    begin
      if I + Str2Len > Str1Len then
        Exit;
      Find := CompareMem(P1, P2, Str2Len);
      if Find then
        Break;
    end;
    Inc(P1);
    Inc(I);
  end;
  if Find then
  begin
    Result := Pointer(TsgNativeUInt(Str1) + I);
    AFoundAdr := I;
  end;
end;

procedure FindAddresses(SourceAdd, FindAdd: PByte; SourceLen, FindLen: TsgNativeUInt;
  AFoundAddresses, AIgnoredAddresses: TList);
var
  Find, Base: PByte;
  MaxAddr: TsgPrNativeUInt;
  vFoundAdr: TsgNativeUInt;
  vNeedToBeAdded: Boolean;
begin
  Base := SourceAdd;
  MaxAddr := TsgNativeUInt(Base) + SourceLen - 1;
  AFoundAddresses.Clear;

  while True do
  begin
    Find := StrPosLen(Base, FindAdd, SourceLen, FindLen, vFoundAdr);
    if Find = nil then
      Break;
    vNeedToBeAdded := True;
    if AIgnoredAddresses <> nil then
      vNeedToBeAdded := AIgnoredAddresses.IndexOf(Find) < 0;
    if vNeedToBeAdded then
      AFoundAddresses.Add(Find);
    Base := Find;
    Inc(Base, 1);
    if TsgNativeUInt(Base) > MaxAddr then
      Exit;
    SourceLen := MaxAddr - TsgNativeUInt(Find);
  end;
end;

procedure sgZeroMemory(Destination: Pointer; Length: TsgNativeUInt);
begin
  FillChar(Destination^, Length, 0);
end;

function BoolToBit(const Value: Boolean): Byte;
begin
  if Value then
    Result := 1
  else
    Result := 0;
end;

{$IFNDEF SGDEL_7}
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  SecsPerHour   = SecsPerMin * MinsPerHour;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;

function IncMilliSecond(const AValue: TDateTime;
  const ANumberOfMilliSeconds: Int64 = 1): TDateTime;
var
  TS: TTimeStamp;
  TempTime: Comp;
begin
  TS := DateTimeToTimeStamp(AValue);
  TempTime := TimeStampToMSecs(TS);
  TempTime := TempTime + ANumberOfMilliSeconds;
  TS := MSecsToTimeStamp(TempTime);
  Result := TimeStampToDateTime(TS);
end;

function IncSecond(const AValue: TDateTime;
  const ANumberOfSeconds: Int64 = 1): TDateTime;
begin
  Result := IncMilliSecond(Avalue, ANumberOfSeconds * MSecsPerSec);
end;

function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64 = 1): TDateTime;
begin
  Result := IncSecond(AValue, ANumberOfMinutes * MinsPerHour);
end;

function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime;
begin
  Result := IncMinute(AValue, ANumberOfHours * MinsPerHour);
end;

function IsLeapYear(Year: Word): Boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function TryJulianDateToDateTime(const AValue: Double;
  out ADateTime: TDateTime): Boolean;
var
  L, N, LYear, LMonth, LDay: Integer;
  ExtraMilliseconds: Integer;
begin
  L := Trunc(AValue) + 68570;
  N := 4 * L div 146097;
  L := L - (146097 * N + 3) div 4;
  LYear := 4000 * (L + 1) div 1461001;
  L := L - 1461 * LYear div 4 + 31;
  LMonth := 80 * L div 2447;
  LDay := L - 2447 * LMonth div 80;
  L := LMonth div 11;
  LMonth := LMonth + 2 - 12 * L;
  LYear := 100 * (N - 49) + LYear + L;
  Result := TryEncodeDate(Word(LYear), Word(LMonth), Word(LDay), ADateTime);
  if Result then
  begin
    ExtraMilliseconds := MilliSecondOfTheDay(Abs(Frac(aValue)));
    ADateTime := IncHour(ADateTime, -12); // Julian days start at noon
    ADateTime := IncMilliSecond(ADateTime, ExtraMilliseconds);
  end;
end;

{$ENDIF}

function sgGetTickCount: Cardinal;
begin
  Result := {$IFDEF SG_FIREMONKEY}TThread.{$ENDIF}GetTickCount;
end;

function sgGetTickCount64: UInt64;
{$IFDEF MSWINDOWS}
type
  TsgGetTickCount64Func = function: UInt64; stdcall;
var
  vGetTickCount64Func: TsgGetTickCount64Func;
  vKernel32Handle: HMODULE;
{$ENDIF}
begin
  Result := sgGetTickCount;
{$IFDEF MSWINDOWS}
  vKernel32Handle := GetModuleHandle('Kernel32.dll');
  if vKernel32Handle <> 0 then
  begin
    vGetTickCount64Func := TsgGetTickCount64Func(GetProcAddress(vKernel32Handle, 'GetTickCount64'));
    if @vGetTickCount64Func <> nil then
      Result := vGetTickCount64Func;
  end;
{$ENDIF}
{$IFDEF LINUX}
{$ENDIF}
{$IFDEF MACOS}
{$ENDIF}
end;

procedure DateTimeToJulianDateInt(const Date: TDateTime; var JDDay,
  JDMilSecInDay: Cardinal);
var
  vMJDate: Double;
  vDate: TDateTime;
begin
  if IsZero(Date) then
  begin
    JDDay := 0;
    JDMilSecInDay := 0;
  end
  else
  begin
    vDate := IncHour(Date, -cntJulianDateHourDif);
    vMJDate := DateTimeToJulianDate(vDate);
    JDDay := Trunc(vMJDate);
    JDMilSecInDay := MilliSecondOfTheDay(vDate);
  end;
end;

function JulianDateIntToDateTime(const JDDay, JDMilSecInDay: Cardinal;
  DefDate: TDateTime = 0): TDateTime;
begin
  if TryJulianDateToDateTime(JDDay, Result) then
  begin
    Result := IncMilliSecond(Trunc(Result), JDMilSecInDay);
    Result := IncHour(Result, cntJulianDateHourDif);
  end
  else
    Result := DefDate;
end;

function DateTimeToJulianTimeStamp(const ADateTime: TDateTime;
  const AHourOffs: Integer = cntDwgJulianDateHourDif): TTimeStamp;
var
  vJDDate: Double;
begin
  vJDDate := DateTimeToJulianDate(IncHour(ADateTime, AHourOffs));
  if IsZero(vJDDate) then
  begin
    Result.Date := 0;
    Result.Time := 0;
  end
  else
  begin
    Result.Date := Trunc(vJDDate);
    Result.Time := MilliSecondOfTheDay(Abs(vJDDate - Result.Date));
  end;
end;

function JulianTimeStampToDateTime(const AJDTimeStamp: TTimeStamp;
  const ADefaultDateTime: TDateTime = 0;
  const AHourOffs: Integer = cntDwgJulianDateHourDif): TDateTime;
var
  vJDDateTime: Double;
begin
  vJDDateTime := AJDTimeStamp.Date + AJDTimeStamp.Time / MSecsPerDay - AHourOffs / HoursPerDay;
  if not TryJulianDateToDateTime(vJDDateTime, Result) then
    Result := ADefaultDateTime;
end;

function CreateTimeStamp(const Day: Integer = 0; const Milliseconds: Integer = 0): TTimeStamp;
begin
  Result.Date := Day;
  Result.Time := Milliseconds;
end;

function AlignByBox(const AAlign: Integer; const ABox: TFRect;
  const AHeight: Double = 0): TFPoint;
begin
  Result := ABox.TopLeft;
  case AAlign of
    2, 5, 8:  Result.X := 0.5 * (ABox.Left + ABox.Right);
    3, 6, 9:  Result.X := ABox.Right;
    10:       Result.X := 0;
  end;
  case AAlign of
    4, 5, 6:  Result.Y := 0.5 * (ABox.Top + ABox.Bottom);
    7, 8, 9:  Result.Y := ABox.Bottom;
    10:       Result.Y := 0.5 * (ABox.Top + ABox.Bottom - AHeight);
  end;
end;

function GetTextWidthByCanvas(ACanvas: TCanvas; AText: string): Integer;
const
  cnstTextExtentPointInaccuracy = 5;
{$IFNDEF SG_FIREMONKEY}
var
  vSize: TSize;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  Result := Round(ACanvas.TextWidth(AText) + cnstTextExtentPointInaccuracy);
{$ELSE}
{$IFDEF SGFPC}
  ACanvas.GetTextSize(AText, vSize.cx, vSize.cy);
{$ELSE}
  GetTextExtentPoint32(ACanvas.Handle, PChar(AText), Length(AText), vSize);
{$ENDIF}
  Result := vSize.cx + cnstTextExtentPointInaccuracy;
{$ENDIF}
end;

function StrToDoubleEx(const AStr: string;
  const AUseSystemSeparator: Boolean = True): Double;
begin
  if AUseSystemSeparator then
    Result := StrToDouble(AStr, GetDecimalSeparator)
  else
    Result := StrToDouble(AStr);
end;

function sgFloatToStr(const AValue: Double): string;
begin
  try
    Result := FloatToStr(AValue);
  except
    Result := '0';
  end
end;

function sgStrToFloat(const AValue: string): Double;
begin
  try
{$IFNDEF SGDEL_6}
  Result := StrToFloat(AValue);
{$ELSE}
    Result := StrToFloatDef(AValue, 0);
{$ENDIF}
  except
    Result := 0;
  end;
end;

function sgTryStrToFloat(const AValue: string; var AResult: Double;
  const ASeparator: Char = cnstPoint;
  AFormatSettings: PFormatSettings = nil): Boolean;
{$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
begin
  Result := TryStrToFloat(AValue, AResult, CreateFormatSettingsInternal(AFormatSettings, ASeparator));
end;
{$ELSE}
var
  DS: Char;
begin
  DS := SetDecimalSeparator(ASeparator);
  try
{$IFNDEF SGDEL_6}
    try
      Result := True;
      AResult := StrToFloat(AValue);
    except
      Result := False;
    end;
{$ELSE}
    Result := TryStrToFloat(AValue, AResult);
{$ENDIF}
  finally
    SetDecimalSeparator(DS);
  end;
end;
{$ENDIF}

function sgTryStrToInt(const AValue: string; var AResult: Integer): Boolean;
begin
{$IFNDEF SGDEL_6}
  try
    Result := True;
    AResult := StrToInt(AValue);
  except
    Result := False;
  end;
{$ELSE}
  Result := TryStrToInt(AValue, AResult);
{$ENDIF}
end;

function ExtendedToInt(const AValue: Extended;
  const AMode: Integer = 0;
  const ARange: Integer = MaxInt): Integer;
begin
  if AValue > ARange then
    Result := ARange
  else
    if AValue < -ARange then
      Result := -ARange
    else
    begin
      case AMode of
        1:  Result := Floor(AValue);
        2:  Result := Round(AValue);
        3:  Result := Ceil(AValue);
        4:  Result := Trunc(AValue);
      else
        Result := Round(AValue);
      end;
    end;
end;

function Ord2(const A: TsgInt64): TsgNativeInt; register;
{$IFNDEF SG_ASSEMBLER}
{$IFNDEF SG_FM_WINDOWS}
var
  V1, V2, V3: TsgNativeInt;
begin
  V1 := TsgNativeInt(A.Lo shr 3);
  V2 := SizeOf(A.Lo) shl 3 - 3;
  V3 := A.Hi shl (V2);
  Result := TsgNativeInt(V1 or V3);
end;
{$ELSE}
begin
  Result := TsgNativeInt((A.Lo shr 3) or (A.Hi shl ((SizeOf(A.Lo) shl 3) - 3)));
end;
{$ENDIF}
{$ELSE}
asm
{$IFDEF SG_CPUX64}
  mov rdx,[rcx+8]
  shl rdx,61
  mov rax,[rcx]
  shr rax,3
  or rax,rdx
{$ELSE}
  mov edx,[eax+4]
  shl edx,29
  mov eax,[eax]
  shr eax,3
  or eax,edx
{$ENDIF}
end;
{$ENDIF}

function CreateStringListSorted(const ADuplicates: TDuplicates;
  const ACaseSensitive: Boolean = False): TStringList;
begin
  Result := TsgStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := ADuplicates;
  TsgStringList(Result).CaseSensitive := ACaseSensitive;
end;

function ColorRGBToStr(AColor: TColor;
  const AHexPrefix: Char = '$'): string;
var
  vARGB: array[0..3] of Byte absolute AColor;
begin
  SwapByte(vARGB[0], vARGB[2]);
  Result := AHexPrefix + IntToHex(AColor, 0);
end;

function ColorCADToStr(const AColor: TsgColorCAD): string;
begin
  Result := IntToStr(Integer(AColor.Active)) + cnstPointSeparator;
  case AColor.Active of
    acIndexColor:
      Result := Result + IntToStr(AColor.Color);
  else
    Result := Result + ColorRGBToStr(AColor.Color);
  end;
  Result := Result + cnstPointSeparator + AColor.AlbumString;
end;

function StrToColorRGB(AString: string;
  const AHexPrefix: Char = '#'): TColor;
var
  I: Integer;
  vARGB: array[0..3] of Byte absolute Result;
  vRGB: TsgStringList;
begin
  Result := 0;
  if Length(AString) > 0 then
  begin
    if AString[1] = '(' then
    begin
      Delete(AString, 1, 1);
      I := StringScan(')', AString, 1);
      if I > 0 then
        SetLength(AString, I - 1);
      vRGB := TsgStringList.Create;
      try
        vRGB.LineBreak := ',';
        vRGB.Text := AString;
        for I := 0 to MinI(vRGB.Count, 2) do
          vARGB[I] := StrToIntDef(vRGB[I], 0);
      finally
        vRGB.Free;
      end;
    end
    else
    begin
      if AString[1] = AHexPrefix then
        AString[1] := '$';
      Result := StrToIntDef(AString, Result);
      SwapByte(vARGB[0], vARGB[2]);
    end;
  end;
end;

function StrToColorCAD(const AStr: string): TsgColorCAD;
var
  vActive: Integer;
  vValues: TsgStringList;
begin
  Result := cnstColorCADByLayer;
  vValues := TsgStringList.Create;
  try
    vValues.LineBreak := cnstPointSeparator;
    vValues.Text := AStr;
    if vValues.Count > 1 then
    begin
      vActive := StrToInt(vValues[0]);
      if vActive > Integer(High(TsgActiveColor)) then
        vActive := Integer(High(TsgActiveColor));
      Result.Active := TsgActiveColor(vActive);
      case Result.Active of
        acIndexColor:
          begin
            Result.Color := StrToIntDef(vValues[1], clDXFByLayer);
            if Result.Color > clDXFByLayer  then
              Result.Active := acRGBColor;
          end;
      else
        Result.Color := Cardinal(StrToColorRGB(vValues[1]));
      end;
      if vValues.Count > 2 then
        Result.AlbumString := vValues[2];
    end;
  finally
    vValues.Free;
  end;
end;

function SnapModeToInt(const AMode: TObjectSnapState): DWORD;
var
  I: TObjectSnapMode;
begin
  Result := 0;
  for I := Low(TObjectSnapMode) to High(TObjectSnapMode) do
    if I in AMode then
      Inc(Result, 1 shl Ord(I));
end;

function IntToSnapMode(const AFlags: DWORD): TObjectSnapState;
var
  I: TObjectSnapMode;
begin
  Result := [];
  for I := Low(TObjectSnapMode) to High(TObjectSnapMode) do
    if ((1 shl Ord(I)) and AFlags) <> 0 then
      Include(Result, I);
end;

function IsXMLHelp(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := xmCallHelp in AMode;
end;

function IsFullProps(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := xmlGetDefaultValue in AMode;
end;

function IsFullPropsOrXMLHelp(const AMode: TsgXMLModes): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IsFullProps(AMode) or IsXMLHelp(AMode);
end;

function sgIntToXMLModes(const AMode: Integer): TsgXMLModes; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // AMode is bit coded
  // 0 bit xmAddSubEntities
  // 1 bit xmAddSectionEntities
  // 2 bit xmNoSubEntitiesNode
  // 3 bit xmCallHelp
  // 4 bit xmOnlyChildNodes
  // 5 bit xmlGetDefaultValue
  // 6 bit xmlForViewing
  // 7 bit xmlModMeshes
  // 8 bit xmlModPhysical
  // 9 bit xmlModAliases
  // 10 bit xmFullXRefNames
  // 11 bit xmlModGeometry
  // 12 bit xmlModReduced
  // 13 bit xmPathGenSup

  Result := cnstDefaultXMLMode;
  if AMode <> 0 then
  begin
    Result := [];
    if GetBit64(0, AMode) = 1 then
      Include(Result, xmAddSubEntities);
    if GetBit64(1, AMode) = 1 then
      Include(Result, xmAddSectionEntities);
    if GetBit64(2, AMode) = 1 then
      Include(Result, xmNoSubEntitiesNode);
    if GetBit64(3, AMode) = 1 then
      Include(Result, xmCallHelp);
    if GetBit64(4, AMode) = 1 then
      Include(Result, xmOnlyChildNodes);
    if GetBit64(5, AMode) = 1 then
      Include(Result, xmlGetDefaultValue);
    if GetBit64(6, AMode) = 1 then
      Include(Result, xmlForViewing);
    if GetBit64(7, AMode) = 1 then
      Include(Result, xmlModMeshes);
    if GetBit64(8, AMode) = 1 then
      Include(Result, xmlModPhysical);
    if GetBit64(9, AMode) = 1 then
      Include(Result, xmlModAliases);
    if GetBit64(10, AMode) = 1 then
      Include(Result, xmFullXRefNames);
    if GetBit64(11, AMode) = 1 then
      Include(Result, xmlModGeometry);
    if GetBit64(12, AMode) = 1 then
      Include(Result, xmlModReduced);
    if GetBit64(13, AMode) = 1 then
      Include(Result, xmPathGenSup);
  end;
end;

function sgXMLModesToInt(const AMode: TsgXMLModes): Integer;
var
  M: TsgXMLMode;
begin
  Result := 0;
  for M := Low(TsgXMLMode) to High(TsgXMLMode) do
  begin
    if M in AMode then
      Result := Result + (1 shl Ord(M));
  end;
end;

function sgGetXMLFromCommand(const ACommand: string;
  const ARootAttrib: string = ''): string;
const
  cnstXMLText ='<?xml version="1.0" encoding="UTF-16"?>';
  cnstXMLText2Begin ='<cadsofttools version="';
  cnstXMLText2End = '">';
  cnstXMLText3 = '</cadsofttools>';
begin
  Result := cnstXMLText + cnstXMLText2Begin;
  Result := Result + IntToStr(cnstXMLIntfaceVersion.Major) + '.' +
  IntToStr(cnstXMLIntfaceVersion.Minor);
  Result := Result + '"';

  Result := Result + ' ' +  ARootAttrib;
  Result := Result + '>';

  Result := Result + ACommand;
  Result := Result + cnstXMLText3;
end;

function GetCountBlocksForOLEExport(const AWidth, AHeight: Integer;
    const APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat; var Chunk: TPoint): TPoint;
const
  cnstBPPs: array[{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat] of Byte = (24, 1, 4, 8, 16, 16, 24, 32, 0);
var
  vCount, vIMax, vJMax: Integer;
begin
  vCount := Ceil(((AWidth * AHeight * cnstBPPs[APixelFormat]) div 8 ) /
    cnstMaxOLEStream);
  if vCount = 0 then
    vCount := 1;
  vIMax := Ceil(Sqrt(vCount));
  vJMax := vIMax;
  Result := Point(vIMax - 1, vJMax - 1);
  Chunk.X := 1;
  Chunk.Y := 1;
  if vIMax > 0 then
    Chunk.X := AWidth div vIMax;
  if vJMax > 0 then
    Chunk.Y := AHeight div vJMax;
  if (Chunk.X > 0) and ((AWidth mod Chunk.X) > 0) then
    Inc(Result.X);
  if (Chunk.Y > 0) and ((AHeight mod Chunk.Y) > 0) then
    Inc(Result.Y);
end;


function CreatePointsWithBoundary(const APoints: IsgArrayFPoint; const ABoundary: Double): TFPointList;
const
  cnstMinSegmentSize: Double = 1E-09;
var
  I, J, vIsPolyline: Integer;
  vPolygon: TFPointList;
  vPolygonBox: TFRect;
  vVertex1, vVertex2, vDelta, vPoint1: TFPoint;
  vTestBoundary: DOuble;
begin
  Result := TFPointList.Create;
  if APoints.FPointCount > 2 then
  begin
    vPolygon := Result;
    vPolygonBox := cnstBadRect;
    for I := 0 to APoints.FPointCount - 1 do
    begin
      vPoint1 := APoints.FPoints[I];
      if (I = 0) or (DistanceFPointSqr(vPolygon.Last, vPoint1) >= cnstMinSegmentSize) then
      begin
        vPolygon.Add(vPoint1);
        vPolygon.Add(vPoint1);
        ExpandFRect(vPolygonBox, vPoint1);
      end;
    end;
    if GetBoxType(vPolygonBox) <> bxXY then
    begin
      vPolygon.Clear;
      Exit;
    end;
    vTestBoundary := Min(Abs(vPolygonBox.Right - vPolygonBox.Left), Abs(vPolygonBox.Bottom - vPolygonBox.Top)) / 100;
    vPolygon.Add(vPolygon.First);
    vPolygon.Delete(0);
    I := 0;
    while I < vPolygon.Count do
    begin
      vVertex1 := vPolygon[I];
      vVertex2 := vPolygon[I + 1];
      vPoint1 := GetNormalPt(vVertex1, vVertex2, vVertex1, ABoundary);
      vDelta := SubFPoint(vPoint1, vVertex1);
      if vTestBoundary < ABoundary then
        vIsPolyline := IsPointInPolyline(APoints, AddFPoint(MiddleFPoint(vVertex1, vVertex2), SubFPoint(vPoint1, GetNormalPt(vVertex1, vVertex2, vVertex1, vTestBoundary))), @vPolygonBox)
      else
        vIsPolyline := IsPointInPolyline(APoints, AddFPoint(MiddleFPoint(vVertex1, vVertex2), vDelta), @vPolygonBox);
      if (vIsPolyline <> 0) then
      begin
        vPoint1 := GetNormalPt(vVertex1, vVertex2, vVertex1, -ABoundary);
        vDelta := SubFPoint(vPoint1, vVertex1);
      end;
      vPolygon[I] := vPoint1;
      vPolygon[I + 1] := AddFPoint(vVertex2, vDelta);
      Inc(I, 2);
    end;
    I := 0;
    while I < vPolygon.Count do
    begin
      vVertex1 := vPolygon[I];
      vVertex2 := vPolygon[I + 1];
      Inc(I, 2);
      if I < vPolygon.Count then
        J := I
      else
        J := 0;
      if IsCrossLinesPts(vVertex1, vVertex2, vPolygon[J], vPolygon[J + 1], @vPoint1) then
      begin
        vPolygon[I - 1] := vPoint1;
        vPolygon[J] := vPoint1;
      end
      else
      begin
//        sgNOP;
      end;
    end;
  end;
end;

function MMToText(Value: Extended; Digits: Integer; AUnitsKf: TsgFloat;
  const AMethod: Byte = 0): string;
begin
  Value := Value * AUnitsKf;
  case AMethod of
    1:  Result := DoubleToStrF(Value, 2, cnstColorLWeightValSeparator);
  else
    Result := FloatToStrF(Value, ffFixed, 15, Digits);
  end;
end;

function DtoS(D: Double; Digits: Integer = 4): string;
var
  DS: Char;
begin
  DS := SetDecimalSeparator('.');
  try
    Result := FloatToStrF(D, ffFixed, 15, Digits);
  finally
    SetDecimalSeparator(DS);
  end;
end;

function StrToD(var S: string; var D: Double): LongBool;
var
  DS: Char;
begin
  DS := SetDecimalSeparator('.');
  try
    Integer(Result) := Pos(DS, S);
    if Integer(Result) > 0 then
      S[Integer(Result)] := '.';
    Val(S, D, Integer(Result));
    Result := not Result;
  finally
    SetDecimalSeparator(DS);
  end;
end;

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
    StrToD(vS, W);
    vS := '';
    repeat
      vS := vS + S[I];
      Inc(I);
    until S[I] = ' ';
    StrToD(vS, H);
    Inc(I);
    if S[I] <> '"' then
      Result := True;
  end;
end;

function CheckPathDelim(const APath: string): string;
{$IFNDEF MSWINDOWS}
var
  vPath: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := APath;
{$ELSE}
  vPath := APath;
  StringReplace(vPath, cnstBackSlash, PathDelim);
  Result := vPath;
{$ENDIF}
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
function CheckExportOLEAsMetafile(Handle: HENHMETAFILE): Boolean;
var
  vEnhMFHeader: TEnhMetaHeader;
begin
  GetEnhMetaFileHeader(Handle, SizeOf(vEnhMFHeader), @vEnhMFHeader);
  Result := vEnhMFHeader.nBytes < cnstMaxOLEStream;
end;
{$ENDIF}

procedure sgBinToHex(Buffer, Text: Pointer; BufSize: Integer);
var
  I: Integer;
begin
  InitByteBinToHex;
  for I := 0 to BufSize - 1 do
    PWordArray(Text)^[I] := ByteBinToHex^[PByteArray(Buffer)^[I]];
end;

function sgHexToBin(Text: PAnsiChar; Buffer: PAnsiChar; BufSize: Integer): Integer;
const
  h2bcvt: array[Byte] of Byte = (
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $0,  $1,  $2,  $3,  $4,  $5,  $6,  $7,  $8,  $9, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF,  $A,  $B,  $C,  $D,  $E,  $F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF,  $A,  $B,  $C,  $D,  $E,  $F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
var
  I: Integer;
  b1, b2: Byte;
begin
  I := BufSize;
  while I > 0 do
  begin
    b1 := h2bcvt[Ord(Text[0])];
    b2 := h2bcvt[Ord(Text[1])];
    if (b1 = $FF) or (b2 = $FF) then
      Break;
    Buffer[0] := AnsiChar((b1 shl 4) + b2);
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

procedure InitByteBinToHex;
var
  I: Integer;
begin
  if not Assigned(ByteBinToHex) then
  begin
    New(ByteBinToHex);
    for I := Low(TByteBinToHex) to High(TByteBinToHex) do
      ByteBinToHex^[I] := (Byte(sBinToHex[I and $F + 1]) shl 8) or Byte(sBinToHex[I shr 4 + 1]);
  end;
end;

procedure FreeByteBinToHex;
begin
  DisposeAndNil(ByteBinToHex);
end;

function GetFileNameByParam(const ADelimetrString: string;
  const AFullName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(ADelimetrString, AFullName);
  Result := Copy(AFullName, I + 1, MaxInt);
end;

function GetFileExtByParam(const ADelimetrString: string;
  const AFullName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + ADelimetrString, AFullName);
  if (I > 0) and (AFullName[I] = '.') then
    Result := Copy(AFullName, I, MaxInt) else
    Result := '';
end;

function NormalDir(const DirName: string): string;
var
  vChr: Char;
begin
  Result := DirName;
  if (Result <> '') then
  begin
    vChr := AnsiLastChar(Result)^;
    if not CharInSet(vChr, [':', '\']) then
    begin
      vChr := UpCase(Result[1]);
      if (Length(Result) = 1) and CharInSet(vChr, ['A'..'Z']) then
        Result := Result + ':\'
      else
        Result := Result + '\';
    end;
  end;
end;

{$IFDEF SG_SYSTEM_PROCS}
function GetFileSize(const FileName: string): Int64;
  {$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := -1;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(Result).Lo := FindData.nFileSizeLow;
      Int64Rec(Result).Hi := FindData.nFileSizeHigh;
    end;
  end;
end;
  {$ELSE}
var
  vSt: stat;
begin
  if FileExists(FileName) then
    if FpStat(FileName, vSt) = 0 then
      Result := vSt.st_size;
end;
  {$ENDIF}
{$ENDIF}

procedure sgShellExecute(Operation, FileName, Parameters,
  Directory: string; ShowCmd: Integer; AHandle: Cardinal = 0);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(AHandle, PChar(Operation), PChar(FileName),
   PChar(Parameters), PChar(Directory), ShowCmd);
{$ENDIF}
end;

procedure sgOpenURL(const AUrl: string);
var
  myURL: string;
{$IFDEF SG_FM_ANDROID}
  Intent: JIntent;
{$ENDIF}

{$IFDEF SG_FM_IOS}
  NSU: NSUrl;
{$ENDIF}
begin
  myURL := AUrl;
{$IFDEF MSWINDOWS}
   sgShellExecute('OPEN', AUrl, '', '', SW_SHOWNORMAL);
{$ENDIF}

{$IFDEF SG_FM_IOS}
  NSU := StrToNSUrl(TIdURI.URLEncode(myURL));
  if SharedApplication.canOpenURL(NSU) then
  begin
    SharedApplication.openUrl(NSU);
  end;
{$ENDIF}

{$IFDEF SG_FM_MACOS}
  _system(PAnsiChar(AnsiString('open ' + myURL)));
{$ENDIF}

{$IFDEF SG_FM_LINUX}
  FmuxOpenUrl(PChar(myURL));
{$ENDIF}

{$IFDEF SG_FM_ANDROID}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(myURL));
  TAndroidHelper.Activity.startActivity(Intent);
{$ENDIF}
end;

procedure sgOpenEMail(const AMail: string);
begin
  sgShellExecute('OPEN', 'mailto:' + AMail, '', '', SW_SHOWNORMAL);
end;

{ Print function}

procedure AddPrinterPaperSize(const APaper: TPaperFormat);
var
  vCount: Integer;
  vPaper: PPaperFormat;
  vPrinterPapers: TStringList;
begin
  vPrinterPapers := GetDefaultPrinterPapers;
  New(vPaper);
  vCount := vPrinterPapers.Count;
  PrinterPapers.AddObject(APaper.Name, TObject(vPaper));
  if PrinterPapers.Count = vCount then
    Dispose(vPaper)
  else
    vPaper^ := APaper;
end;

procedure FreePrinterPapers;
var
  I: Integer;
  vPP: PPaperFormat;
begin
  if Assigned(PrinterPapers) then
  begin
    for I := 0 to PrinterPapers.Count - 1 do
    begin
      vPP := PPaperFormat(PrinterPapers.Objects[I]);
      PrinterPapers.Objects[I] := nil;
      Dispose(vPP);
    end;
    FreeAndNil(PrinterPapers);
  end;
end;

function FindPaperIndexBySizes(const APaperFormats: TStrings;
  const APaperSize: TF2DPoint; const AFindRot: Boolean): Integer;
var
  I: Integer;
  vFormatSize: TF2DPoint;
  vMiscalculation: Integer;

  function EqualWithMiscalculation(const ASourceValue, ACompareValue: Double): Boolean;
  var
    vRoundSource, vRoundCompare: Integer;
  begin
    vRoundCompare := Round(ACompareValue);
    vRoundSource := Round(ASourceValue * 10);
    Result := Abs(vRoundCompare - vRoundSource) < vMiscalculation;
  end;

begin
  Result := -1;
  vMiscalculation := 10;
  for I := 0 to APaperFormats.Count - 1 do
    if Assigned(APaperFormats.Objects[I]) then
    begin
      vFormatSize := PPaperFormat(APaperFormats.Objects[I])^.PaperSize;
      if ((EqualWithMiscalculation(APaperSize.X, vFormatSize.X) and EqualWithMiscalculation(APaperSize.Y, vFormatSize.Y))) or
         (AFindRot and (EqualWithMiscalculation(APaperSize.X, vFormatSize.Y) and EqualWithMiscalculation(APaperSize.Y, vFormatSize.X))) then
        begin
          Result := I;
          Break;
        end;
    end;
end;

function FindPaperIndex(const APaperFormats: TStrings;
  const APaperName: string; const APaperSize: TF2DPoint;
  const AFindRot: Boolean): Integer;
begin
  Result := APaperFormats.IndexOf(APaperName);
  if Result = -1 then
    Result := FindPaperIndexBySizes(APaperFormats, APaperSize, AFindRot);
end;

function GetDefaultPrinterPapers: TStringList;
begin
  if PrinterPapers = nil then
  begin
    PrinterPapers := TsgStringList.Create;
    PrinterPapers.Sorted := True;
    PrinterPapers.Duplicates := dupIgnore;
    TsgStringList(PrinterPapers).CaseSensitive := False;
  end;
  Result := PrinterPapers;
end;

function GetPrinterActualPaperSize(const ABox: TFRect; var AName:string;
  const AMargins: PF2DRect = nil): TF2DPoint;
var
  vPaperSize: TF2DPoint;
  I: Integer;
  vPaper: PPaperFormat;
begin
  Result := MakeF2DPoint(210, 297);
  AName := 'A4';
  if Assigned(AMargins) then
    AMargins^ := cnstDefaultMargins;
  if Assigned(PrinterPapers) and (not IsBadRect(ABox)) then
  begin
    vPaperSize := GetSizeFRect2D(ABox);
    I := FindPaperIndexBySizes(PrinterPapers, vPaperSize, False);
    if I < 0 then
      I := FindPaperIndexBySizes(PrinterPapers, Result, False);
    if I > -1 then
    begin
      vPaper := PPaperFormat(PrinterPapers.Objects[I]);
      Result.X := (vPaper^.PaperSize.X) / 10;
      Result.Y := (vPaper^.PaperSize.Y) / 10;
      AName :=  vPaper^.Name;
      if Assigned(AMargins) then
        AMargins^ := vPaper^.Margins;
    end;
  end;
end;

function CalcFitScale(AInputRect: TFRect; AMargins: TF2DRect;
  APlotSize: TF2DPoint; AIsLandscape: Boolean; AIsMM: Boolean): TsgPlotScale;
var
  vSourceHeight, vSourceWidth, vWidht, vHeight: Double;
  vHorMarginsWidth, vHorMarginsHeight: Double;
  vScale: Double;
  vWidthK, vHeightK: Double;
begin
  vHorMarginsWidth := AMargins.Left + AMargins.Right;
  vHorMarginsHeight := AMargins.Top + AMargins.Bottom;
  vSourceWidth := APlotSize.X - vHorMarginsWidth;
  vSourceHeight := APlotSize.Y - vHorMarginsHeight;
  if AIsLandscape then
    SwapDoubles(vSourceHeight, vSourceWidth);
  vWidht := Abs(AInputRect.Left - AInputRect.Right);
  vHeight := Abs(AInputRect.Top - AInputRect.Bottom);
  vWidthK := IfThen(vWidht <> 0, vSourceWidth / vWidht, 1);
  vHeightK := IfThen(vHeight <> 0, vSourceHeight / vHeight, 1);
  if vWidthK < vHeightK then
    vScale := vWidthK
  else
    vScale := vHeightK;
  Result.Numerator := 1;
  if not AIsMM then
    Result.Denomenator := 1 / vScale * cnstMMPerInch
  else
    Result.Denomenator := 1 / vScale;
end;

function GetPaperImageOriginPoint(const AIsMM: Boolean;
  const APlotType: TsgPlotType; const APrintRect: TFRect): TF2DPoint;
var
  K: Double;
begin
  Result := cnstF2DPointZero;
  if APlotType <> ptLayoutInformation then
  begin
    if AIsMM then
      K := 1
    else
      K := cnstMMPerInch;
    Result.X := -Min(APrintRect.Left, APrintRect.Right) * K;
    Result.Y := -Min(APrintRect.Top, APrintRect.Bottom) * K;
  end;
end;

function GetPaperSizesFromBox(const AIndexFormat: Integer;
  const AFormats: TStrings): TF2DPoint;
var
  vFormat: TPaperFormat;
begin
  Result := MakeF2dPoint(0, 0);
  if (AIndexFormat < AFormats.Count) and  Assigned(AFormats.Objects[AIndexFormat])then
  begin
    vFormat := PPaperFormat(AFormats.Objects[AIndexFormat])^;
    if (vFormat.PaperSize.X <> 0) and (vFormat.PaperSize.Y <> 0) then
    begin
      Result.X := vFormat.PaperSize.X{$IFNDEF SG_FIREMONKEY} / 10{$ENDIF};
      Result.Y := vFormat.PaperSize.Y{$IFNDEF SG_FIREMONKEY} / 10{$ENDIF};
    end;
  end;
end;

function RoundToPowerOf2(Value: Integer): Integer;{$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFDEF SG_FAST_CODE}
asm
	MOV	EDX,EAX
	MOV	EAX,1
@@1:	CMP	EAX,EDX
	JAE	@@2
	SHL	EAX,1
	JNC	@@1
@@2:
end;
{$ELSE}
var
  vStop: Boolean;
begin
  Result := 1;
  vStop := False;
  while (Cardinal(Result) < Cardinal(Value)) and not vStop do
  begin
    vStop := Result and $80000000 <> 0;
    Result := Result shl 1;
  end;
end;
{$ENDIF}
procedure NormRect(var Rect: TRect);
begin
  if Rect.Right < Rect.Left then
    SwapInts(Rect.Left, Rect.Right);
  if Rect.Bottom < Rect.Top then
    SwapInts(Rect.Top, Rect.Bottom);
end;

procedure Angle(var Value: Smallint);
begin
  if Abs(Value) >= 3600 then Value := Value mod 3600;
end;

function PreAssign(Src,Dst: TGraphic): Boolean;
var
  vW, vH: Integer;
begin
  Result := Src.Empty or (Src.Width = 0) or (Src.Height = 0);
  if Result then
  begin
    Dst.Assign(nil);
    Exit;
  end;
  vW := {$IFDEF SG_FIREMONKEY}TBitmap(Dst){$ELSE}Dst{$ENDIF}.Width;
  vH := {$IFDEF SG_FIREMONKEY}TBitmap(Dst){$ELSE}Dst{$ENDIF}.Height;
  if vW or vH = 0 then
  begin
    vW := Src.Width;
    vH := Src.Height;
  end;
  if vW = 0 then
    vW := sgMulDiv(vH, Src.Width, Src.Height);
  if vH = 0 then
    vH := sgMulDiv(vW, Src.Height, Src.Width);
  if Src.Palette <> 0 then
  {$IFNDEF SG_FIREMONKEY}
    Dst.Palette := CopyPalette(Src.Palette);
  {$ENDIF}
  {$IFDEF SG_FIREMONKEY}TBitmap(Dst){$ELSE}Dst{$ENDIF}.Width := vW;
  {$IFDEF SG_FIREMONKEY}TBitmap(Dst){$ELSE}Dst{$ENDIF}.Height := vH;
end;

function ImgRect(G: TGraphic): TRect;
begin
  Result := Rect(0, 0, G.Width, G.Height);
end;

procedure AssignToBitmap(Src: TGraphic; BM: TBitmap);
{$IFDEF SG_FIREMONKEY}
var
  vSrcBitmap: TBitmap;
{$ENDIF}
begin
  if (not PreAssign(Src, TGraphic(BM))) then
  begin
    {$IFDEF SG_FIREMONKEY}
    if TObject(Src) is TBitmap then
    begin
      vSrcBitmap := TBitmap(Src);
      BM.Canvas.DrawBitmap(TBitmap(Src), RectF(0, 0, vSrcBitmap.Width, vSrcBitmap.Height), RectF(0, 0, BM.Width, BM.Height), 1, False);
    end;
    {$ELSE}
    BM.Canvas.StretchDraw(ImgRect(BM), Src);
    {$ENDIF}
  end;
end;

{$IFDEF SGFPC}
function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then
    Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then
    Exit;
  if PaletteSize = 0 then
    Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  //Result := CreatePalette(LogPal);
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;
{$ENDIF}

{$IFDEF SG_DELPHI_VCL}
procedure AssignToMetafile(Src: TGraphic; MF: TMetafile);
var
  vMC: TMetafileCanvas;
begin
  if PreAssign(Src, MF) then
    Exit;
  vMC := TMetafileCanvas.Create(MF, 0);
  try
    vMC.StretchDraw(ImgRect(MF), Src)
  finally
    vMC.Free
  end;
end;
{$ENDIF}


function GetGlobalConstant(const AName: string): string;
begin
  Result := '';
  if sgSameText(AName, cnstHttpTimeOutName) then
    Result := IntToStr(cnstHttpTimeOut);
  if sgSameText(AName, cnstHPGapTolOpt) then
    Result := LispDoubleToStr(cnstCADVariables.HPGapTol);
end;

procedure SetGlobalConstant(const AName, AValue: string);
var
  vTmpDouble: Double;
begin
  if sgSameText(AName, cnstHttpTimeOutName) then
{$IFNDEF SGDEL_6}
    cnstHttpTimeOut := StrToInt(AValue);
{$ELSE}
    TryStrToInt(AValue, cnstHttpTimeOut);
{$ENDIF}
  if sgSameText(AName, cnstHPGapTolOpt) then
  begin
    vTmpDouble := LispStrToDouble(AValue);
    if vTmpDouble < 0 then
      vTmpDouble := cnstDefHPGapTol;
    cnstCADVariables.HPGapTol := vTmpDouble;
  end;
end;

{ LineWeight for Color}

function ColorCADtoColorLW(const AColor: TsgColorCAD): string;
begin
  if AColor.Active = acIndexColor then
    Result := IntToStr(AColor.Color)
  else
    Result := '$' + IntToHex(AColor.Color, 8);
end;

function ColorLWtoColorCAD(const AStr: string): TsgColorCAD;
begin
  if Length(AStr) > 0 then
  begin
    Result.AlbumString := '';
    Result.Color := StrToInt(AStr);
    if (Result.Color > 255) or (Pos('$', AStr) > 0) then
      Result.Active := acRGBColor
    else
      Result.Active := acIndexColor;
  end
  else
    Result := cnstColorCADLWeightBadKey;
end;

function ColorLineWeightToStr(const AColor, ALineWeght: string;
  const AColorNew: string): string;
begin
  Result := AColor + cnstColorLWeightDelimiter + ALineWeght;
  if Length(AColorNew) > 0 then
    Result := Result + cnstColorLWeightValuesDelimiter + AColorNew;
end;

function ColorLineWeightToString(const AColor: TsgColorCAD; const ALineWeght: Double;
  const AColorNew: Integer): string;
var
  vColorNew, vKey: string;
begin
  vColorNew := '';
  if AColorNew <> cnstColorLWeightColorNewEmpty then
    vColorNew := '$' + IntToHex(AColorNew, 8);
  vKey := ColorCADtoColorLW(AColor);
  Result := ColorLineWeightToStr(vKey, DoubleToStr(ALineWeght, cnstColorLWeightValSeparator), vColorNew);
end;

function StringToColorLineWeight(const Source: string; var AColor: TsgColorCAD;
  var ALineWeght: TsgFloat; var AColorNew: Integer): Boolean;
var
  vColor, vWeight, vColorNew: string;
begin
  Result := False;
  if StringToColorLineWeightStr(Source, vColor, vWeight, vColorNew) then
  begin
    try
      AColor := ColorLWtoColorCAD(vColor);
      ALineWeght := StrToDouble(vWeight, cnstColorLWeightValSeparator);
      AColorNew := StrToIntDef(vColorNew, cnstColorLWeightColorNewEmpty);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function StringToColorLineWeightStr(const Source: string; var AColor: string;
  var ALineWeght: string; var AColorNew: string): Boolean;
var
  vColorWeight: TsgStringList;
  vPos: Integer;
  vStr: string;
begin
  Result := False;
  AColor := '';
  ALineWeght := '';
  AColorNew := '';
  vColorWeight := TsgStringList.Create;
  try
    vColorWeight.LineBreak := cnstColorLWeightDelimiter;
    vPos := Pos(cnstColorLWeightValuesDelimiter, Source);
    if vPos > 0 then
    begin
      AColorNew := Copy(Source, vPos + 1, Length(Source) - vPos);
      vStr := Copy(Source, 1, vPos - 1);
      vPos := Pos(cnstColorLWeightValuesDelimiter, AColorNew);
      if vPos > 0 then
        SetLength(AColorNew, vPos - 1);
    end
    else
      vStr := Source;
    vColorWeight.Text := Trim(vStr);
    DeleteEmptyStrings(vColorWeight);
    if vColorWeight.Count > 1 then
    begin
      AColor := vColorWeight[0];
      ALineWeght := vColorWeight[1];
      Result := True;
    end;
  finally
    vColorWeight.Free;
  end;
end;

function GetKeyWeight(const vColorWeight: TsgStringList; var AKey: TsgColorCAD;
  var AWeightStr: string): Integer;
var
  vColor: string;
  J: Integer;
  vWeightDouble: Double;
begin
  Result := 0;
  AKey := cnstColorCADLWeightBadKey;
  AWeightStr := '';
  vColor := Trim(vColorWeight[0]);
  if Length(vColor) > 0 then
  begin
    try
      AKey := ColorLWtoColorCAD(vColor);
    except
      AKey := cnstColorCADLWeightBadKey;
    end;
    if (AKey.Active = acIndexColor) and (AKey.Color >= 1) and (AKey.Color <= 255) then
    begin
      Result := 1;
      AWeightStr := Trim(vColorWeight[1]);
      if Length(AWeightStr) > 0 then
      begin
        if AWeightStr[1] = '-' then
        begin
          Result := 3;
          Exit;
        end;
        for J := Length(AWeightStr) downto 1 do
        begin
          if (not IsDigit(AWeightStr[J])) and (AWeightStr[J] <> cnstColorLWeightValSeparator) then
          begin
            Result := 2;
            AWeightStr[J] := cnstColorLWeightValSeparator;
          end;
        end;
        if Result = 2 then
        begin
          try
            vWeightDouble := StrToDouble(AWeightStr, cnstColorLWeightValSeparator);
            AWeightStr := DoubleToStr(vWeightDouble, cnstColorLWeightValSeparator);
          except
            AWeightStr := cnstColorLWeightZero;
          end;
        end;
      end
      else
      begin
        Result := 2;
        AWeightStr := cnstColorLWeightZero;
      end;
    end;
  end;
end;

procedure CorrectFileCws(const AColors: TStringList);
var
  I, vColorColor: Integer;
  vColorKey: TsgColorCAD;
  vColorWeight, vColorValues: TsgStringList;
  vWeight: string;
  vWeightDbl: Double;
  vDel: Boolean;
begin
  vColorWeight := TsgStringList.Create;
  vColorValues := TsgStringList.Create;
  try
    vColorWeight.LineBreak := cnstColorLWeightDelimiter;
    vColorValues.LineBreak := cnstColorLWeightValuesDelimiter;
    for I := AColors.Count - 1 downto 0 do
    begin
      if Pos(cnstColorLWeightValuesDelimiter, AColors[I]) > 0 then
      begin
        vColorValues.Text := AColors[I];
        DeleteEmptyStrings(vColorWeight);
        if vColorValues.Count > 0 then
        begin
          vColorWeight.Text := vColorValues[0];
          DeleteEmptyStrings(vColorWeight);
          if vColorWeight.Count > 1 then
          begin
            if GetKeyWeight(vColorWeight, vColorKey, vWeight) > 0 then
            begin
              vDel := False;
              vColorValues[0] := ColorLineWeightToStr(ColorCADtoColorLW(vColorKey), vWeight, '');
              if vColorValues.Count > 1 then
              begin
                try
                  vColorColor := StrToIntDef(vColorValues[1], cnstColorLWeightColorNewEmpty);
                  try
                    vWeightDbl := StrToDouble(vWeight, cnstColorLWeightValSeparator);
                  except
                    vWeightDbl := fLineWeightDefault;
                  end;
                  if (vColorColor = cnstColorLWeightColorNewEmpty) and (vWeightDbl < 0) then
                     vDel := True
                  else
                     vColorValues[1] := '$' + IntToHex(vColorColor, 8);
                except
                  vDel := True;
                end;
              end;
              if vDel then
                AColors.Delete(I)
              else
                AColors[I] := vColorValues.Text;
            end
            else
              AColors.Delete(I);
          end
          else
            AColors.Delete(I);
        end
        else
          AColors.Delete(I);
      end
      else
      begin
        vColorWeight.Text := AColors[I];
        DeleteEmptyStrings(vColorWeight);
        if vColorWeight.Count > 1 then
        begin
          case GetKeyWeight(vColorWeight, vColorKey, vWeight) of
            0:  AColors.Delete(I);
            2:  AColors[I] := ColorLineWeightToStr(ColorCADtoColorLW(vColorKey), vWeight, '');
          end;
        end
        else
          AColors.Delete(I);
      end;
    end;
  finally
    vColorValues.Free;
    vColorWeight.Free;
  end;
end;

function LoadFromFileCws(const AFileName: string; const AColors: TStringList): Boolean;
begin
  Result := False;
  try
    AColors.LoadFromFile(AFileName);
    if AColors.Count > 0 then
    begin
      CorrectFileCws(AColors);
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function SaveToFileCws(const AColors: TStringList; const AFileName: string): Boolean;
var
  vColors: TStringList;
begin
  try
    Result := True;
    if AColors.Count > 0 then
    begin
      vColors := TStringList.Create;
      try
        vColors.Assign(AColors);
        CorrectFileCws(vColors);
        vColors.SaveToFile(AFileName);
      finally
        vColors.Free;
      end;
    end
    else
      AColors.SaveToFile(AFileName);//create empty file
  except
    Result := False;
  end;
end;

procedure AssignColorToLineWeights(const ASource, ADest: TStringList);
var
  I: Integer;
  vStr: string;
  vKey: TsgColorCAD;
  vColorNew: Integer;
  vLineWeight: TsgFloat;
begin
  ADest.BeginUpdate;
  ADest.Clear;
  try
    for I := 0 to ASource.Count - 1 do
    begin
      vStr := ASource[I];
      StringToColorLineWeight(vStr, vKey, vLineWeight, vColorNew);
      vStr := ColorLineWeightToString(vKey, vLineWeight{ * Resolution / MMPerInch}, vColorNew);
      ADest.Add(vStr);
    end;
  finally
    ADest.EndUpdate;
  end;
end;

{$IFDEF _FIXINSIGHT_}
function EmptyExceptionHandler(E: Exception): Boolean;
begin
  Result := False;
end;
{$ENDIF}

{$IFNDEF SGDEL_6}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;
const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
var
  LResult: Extended;
begin
  try
    Result := True;
    LResult := StrToFloat(S);
  except
    Result := False;
  end;
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function ReverseString(const AText: string): string;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, Length(AText));
  P := PChar(Result);
  for I := Length(AText) downto 1 do
  begin
    P^ := AText[I];
    Inc(P);
  end;
end;
{$ENDIF}

{ TsgPolyPolygon }

function TsgPolyPolygon.AddPoints(const APoints: IsgArrayFPoint): TsgPolyPolygonItem;
begin
  Result := TsgPolyPolygonItem.Create(APoints);
  FObjectItems.Add(Result);
end;

constructor TsgPolyPolygon.Create(const APolyPolylines: TsgObjectList = nil);
var
  I: Integer;
  vPoints: IsgArrayFPoint;
  vObjPoints: TInterfacedObject;
begin
  inherited Create;
  FObjectItems := TsgObjectList.Create;
  if Assigned(APolyPolylines) then
  begin
    for I := 0 to APolyPolylines.Count - 1 do
    begin
      vObjPoints := TInterfacedObject(APolyPolylines[I]);
      if vObjPoints.GetInterface(GUID_ArrayFPoints, vPoints) then
        AddPoints(vPoints);
    end;
  end;
end;

destructor TsgPolyPolygon.Destroy;
begin
  TsgObjectList.FreeList(FObjectItems);
  inherited Destroy;
end;

procedure TsgPolyPolygon.FindHoles;
var
  I, J: Integer;
  vItem, vItemJ: TsgPolyPolygonItem;
begin
  for I := 0 to FObjectItems.Count - 2 do//sort
  begin
    vItem := TsgPolyPolygonItem(FObjectItems[I]);
    for J := I + 1 to FObjectItems.Count - 1 do
    begin
      vItemJ := TsgPolyPolygonItem(FObjectItems[J]);
      if vItem.Area < vItemJ.Area then
      begin
        FObjectItems.Exchange(I, J);
        vItem := vItemJ;
      end;
    end;
  end;
  TsgPolyPolygonItem.FindHoles(Self.ObjectItems);
end;

{ TsgPolyPolygonItem }

constructor TsgPolyPolygonItem.Create(const APoints: IsgArrayFPoint);
begin
  inherited Create;
  FPoints := APoints;
  FArea := GetAreaOfList(FPoints);
  FBox := GetBoxOfArray(FPoints);
  FChilds := TsgPolyPolygon.Create;
end;

destructor TsgPolyPolygonItem.Destroy;
begin
  FPoints := nil;
  FArea := 0;
  FreeAndNil(FChilds);
  inherited Destroy;
end;

class procedure TsgPolyPolygonItem.FindHoles(const APoly: TsgObjectList);
var
  I, J, K, vCnt: Integer;
  vItem, vItemJ: TsgPolyPolygonItem;
begin
  I := 0;
  while I < APoly.Count do
  begin
    vItem := TsgPolyPolygonItem(APoly[I]);
    J := I + 1;
    while J < APoly.Count do
    begin
      vItemJ := TsgPolyPolygonItem(APoly[J]);
      if vItem.Area > vItemJ.Area then
      begin
        if IntersectFRect(vItem.Box, vItemJ.Box) <> -1 then
        begin
          vCnt := 0;
          for K := 0 to vItemJ.Points.FPointCount - 1 do
          begin
            if IsPointInPolyline(vItem.FPoints, vItemJ.Points.FPoints[K], @vItem.FBox) <> 0 then
              Inc(vCnt)
            else
              Break;
          end;
          if vCnt = vItemJ.Points.FPointCount then
          begin
            vItem.Childs.ObjectItems.Add(vItemJ);
            APoly.Delete(J);
            FindHoles(vItem.Childs.ObjectItems);
            Dec(J);
          end;
        end;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
end;

function TsgPolyPolygonItem.ToStr: string;
var
  I: Integer;
  vItem: string;
begin
  Result := '';
  if FPoints.FPointCount > 0 then
  begin
    Result := sgLists.FPointToStrInternal(FPoints.FPoints[0]);
    for I := 1 to FPoints.FPointCount - 1 do
    begin
      vItem := cnstXMLValuesSeparator + FPointToStrInternal(FPoints.FPoints[I]);
      Result := Result + vItem;
    end;
  end;
end;

function CreatePolyPolygon(const APolylines: TsgObjectList): TsgPolyPolygon;
begin
  Result := TsgPolyPolygon.Create(APolylines);
  Result.FindHoles;
end;


{$IFNDEF SG_NON_WIN_PLATFORM}
{ RTL functions }

function CodeMove(CodeSource: Pointer; var Dest; Count: Integer): Cardinal;
begin
  VirtualProtect(CodeSource, Count, PAGE_EXECUTE_READ, Result);
  try
    System.Move(CodeSource^, Dest, Count);
  finally
    VirtualProtect(CodeSource, Count, Result, nil);
  end;
end;
{$ENDIF}

{ TsgConstantsCustom }

constructor TsgConstantsCustom.Create;
begin
  inherited Create;
  FCodes := TsgCollection.Create;
  FNames := CreateStringListSorted(dupIgnore);
  SetDefaults;
end;

destructor TsgConstantsCustom.Destroy;
begin
  FreeAndNil(FNames);
  FreeAndNil(FCodes);
  inherited Destroy;
end;

function TsgConstantsCustom.GetCodeByName(const AName: string): Integer;
begin
  Result := FNames.IndexOf(AName);
  if Result > -1 then
    Result := Integer(FNames.Objects[Result]);
end;

function TsgConstantsCustom.GetCodeType(const ACode: Integer): Integer;
var
  vValue: Pointer;
begin
  Result := 0;
  vValue := GetValueByCode(ACode);
  if vValue <> nil then
    Result := ACode and $FFFF0000;
end;

function TsgConstantsCustom.GetBoolValue(const ACode: Integer): Boolean;
var
  vValue: Pointer;
begin
  Result := False;
  vValue := GetValueByCode(ACode, cnstCodeType_Bool);
  if vValue <> nil then
    Result := PBoolean(vValue)^;
end;

function TsgConstantsCustom.GetIntValue(const ACode: Integer): Integer;
var
  vValue: Pointer;
begin
  Result := 0;
  vValue := GetValueByCode(ACode, cnstCodeType_Int);
  if vValue <> nil then
    Result := PInteger(vValue)^;
end;

function TsgConstantsCustom.GetStrValue(const ACode: Integer): string;
var
  vValue: Pointer;
begin
  Result := '';
  vValue := GetValueByCode(ACode, cnstCodeType_Str);
  if vValue <> nil then
    Result := PString(vValue)^;
end;

function TsgConstantsCustom.GetValue(const ACode: Integer;
  var AValue: string): Boolean;
begin
  Result := True;
  case ACode and $FFFF0000 of
    cnstCodeType_Bool:  AValue := BoolToStr(BoolValue[ACode]);
    cnstCodeType_Int:   AValue := IntToStr(IntValue[ACode]);
    cnstCodeType_Str:   AValue := StrValue[ACode];
  else
    Result := False;
  end;
end;

function TsgConstantsCustom.GetValueByCode(const ACode: Integer;
  const AType: Integer = 0): Pointer;
var
  vIndex: Integer;
begin
  if (ACode = 0) or (ACode and AType <> 0) then
  begin
    vIndex := FCodes.IndexOf(ACode);
    if vIndex > -1 then
      Result := FCodes.Items[vIndex].Data
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TsgConstantsCustom.GetValue(const ASection, APropName: string;
  var AValue: string): Boolean;
var
  vCode: Integer;
begin
  Result := False;
  AValue := '';
  vCode := GetCodeByName(ASection + APropName);
  if vCode > -1 then
    Result := GetValue(vCode, AValue);
end;

function TsgConstantsCustom.Load(const AOptions: TObject): Boolean;
begin
  Result := False;
  if AOptions is TCustomIniFile then
  begin
    Result := LoadFromIni(TCustomIniFile(AOptions));
    FChangeValues := False;
  end;
end;

procedure TsgConstantsCustom.RegsitredCode(const ACode: Integer;
  const AName: string; const AValue: Pointer);
begin
  FNames.AddObject(AName, TObject(Pointer(ACode)));
  FCodes.Add(ACode, AValue);
end;

function TsgConstantsCustom.Save(const AOptions: TObject): Boolean;
begin
  Result := False;
  if AOptions is TCustomIniFile then
    Result := SaveToIni(TCustomIniFile(AOptions));
end;

procedure TsgConstantsCustom.SetDefaults;
begin
  FChangeValues := False;
end;

procedure TsgConstantsCustom.SetBoolValue(const ACode: Integer; const AValue: Boolean);
var
  vValue: Pointer;
begin
  vValue := GetValueByCode(ACode, cnstCodeType_Bool);
  if vValue <> nil then
     PBoolean(vValue)^ := AValue;
end;

procedure TsgConstantsCustom.SetChangeValues(const AValue: Boolean);
begin
  FChangeValues := AValue;
end;

procedure TsgConstantsCustom.SetIntValue(const ACode: Integer; const AValue: Integer);
var
  vValue: Pointer;
begin
  vValue := GetValueByCode(ACode, cnstCodeType_Int);
  if vValue <> nil then
     PInteger(vValue)^ := AValue;
end;

procedure TsgConstantsCustom.SetStrValue(const ACode: Integer; const AValue: string);
var
  vValue: Pointer;
begin
  vValue := GetValueByCode(ACode, cnstCodeType_Int);
  if vValue <> nil then
     PString(vValue)^ := AValue;
end;

function TsgConstantsCustom.SetValue(const ACode: Integer;
  AValue: string): Boolean;
begin
  Result := True;
  case ACode and $FFFF0000 of
    cnstCodeType_Bool:  BoolValue[ACode] := StrToBoolDef(AValue, false);
    cnstCodeType_Int:   IntValue[ACode] := StrToIntDef(AValue, 0);
    cnstCodeType_Str:   StrValue[ACode] := AValue;
  else
    Result := False;
  end;
  if Result then
    FChangeValues := True;
end;

function TsgConstantsCustom.SetValue(const ASection, APropName,
  AValue: string): Boolean;
var
  vCode: Integer;
begin
  Result := False;
  vCode := GetCodeByName(ASection + APropName);
  if vCode > -1 then
  begin
    Result := SetValue(vCode, AValue);
    FChangeValues := True;
  end;
end;

{ TsgProgress }

constructor TsgProgress.Create(const AOnProgress: TProgressEvent);
begin
  inherited Create;
  FOnProgress := AOnProgress;
end;

destructor TsgProgress.Destroy;
begin
  FOnProgress := nil;
  inherited Destroy;
end;

procedure TsgProgress.DoProgress(Sender: TObject; Stage: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string{$IFDEF SGFPC}; var Continue: Boolean{$ENDIF});
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg{$IFDEF SGFPC}, Continue{$ENDIF});
end;

function TsgProgress.GetBreak: Boolean;
begin
  Result := FProgressBreak;
end;

procedure TsgProgress.SetBreak(const AValue: Boolean);
begin
  FProgressBreak := AValue;
end;

{ TsgCustomPolyItemsCollection }

procedure TsgCustomPolyItemsCollection.ChangeItemsCount(ADelta: Integer);
begin
  FPoly.Count := FPoly.Count + ADelta;
end;

constructor TsgCustomPolyItemsCollection.Create(APoly: TsgBaseList; AOwnsPoly: Boolean);
begin
  FOwnsPoly := AOwnsPoly;
  FPoly := APoly;
  FCounts := TsgIntegerList.Create;
  SetLength(FTransformation, SizeOf(TFMatrix) div SizeOf(TsgFloat));
  System.Move(cnstIdentityMat, FTransformation[0], SizeOf(TFMatrix));
  FIsIdentityTransformation := True;
end;

destructor TsgCustomPolyItemsCollection.Destroy;
begin
  if FOwnsPoly then
    FreeAndNil(FPoly);
  FCounts.Free;
  inherited Destroy;
end;

function TsgCustomPolyItemsCollection.DoAddPt(const P): Integer;
var
  Pt: TFPoint;
begin
  Pt := FPointXMat(TFPoint(P), PFMatrix(FTransformation)^);
  Result := DoAddPtDirrect(Pt);
end;

function TsgCustomPolyItemsCollection.DoAddPtDirrect(const P): Integer;
begin
  Inc(FCounts.List^[FCounts.Count - 1]);
  Result := FCounts.List^[FCounts.Count - 1];
end;

function TsgCustomPolyItemsCollection.GetAsPolyPolyline: Boolean;
begin
  Result := False;
end;

function TsgCustomPolyItemsCollection.NewCollection: TAddPt;
begin
  if FCounts.Count > 0 then
  begin
    case FCounts.List^[FCounts.Count - 1] of
      0:;
      1:
        begin
          FCounts.List^[FCounts.Count - 1] := 0;
          ChangeItemsCount(-1);
        end;
    else
      FCounts.Add(0);
    end;
  end
  else
    FCounts.Add(0);
  if IsIdentityTransformation then
    Result := DoAddPtDirrect
  else
    Result := DoAddPt;
end;

procedure TsgCustomPolyItemsCollection.Normalize;
begin
  if FCounts.Count > 0 then
  begin
    case FCounts.List^[FCounts.Count - 1] of
      0: FCounts.Count := FCounts.Count - 1;
      1:
        begin
          FCounts.Count := FCounts.Count - 1;
          ChangeItemsCount(-1);
        end;
    end;
  end;
end;

function TsgCustomPolyItemsCollection.ReleaseItems: Pointer;
begin
  Result := TsgBaseListAccess(FPoly).GetItemBase(0);
  TsgBaseListAccess(FPoly).Attach(nil, 0);
  FPoly.Capacity := 0;
end;

function TsgCustomPolyItemsCollection.UpdateTransformation(const AValue;
  {$IFDEF HAS_UNMANAGED_TYPEINFO}AType: Pointer{$ELSE}ASize: Integer{$ENDIF}): Boolean;
begin
  Result := False;
  if {$IFDEF HAS_UNMANAGED_TYPEINFO}AType = TypeInfo(TFMatrix){$ELSE}ASize = SizeOf(TFMatrix){$ENDIF} then
  begin
    SetLength(FTransformation, SizeOf(TFMatrix) div SizeOf(TsgFloat));
    System.Move(AValue, FTransformation[0], SizeOf(TFMatrix));
    FIsIdentityTransformation := CompareMem(FTransformation, @cnstIdentityMat.V[0].V[0], SizeOf(TFMatrix));
    Result := True;
  end;
end;

{ TsgProxyBase }

function TsgProxyBase.BeginScene: Boolean;
begin
  Result := False;
end;

procedure TsgProxyBase.DoPixelList(const APoints: TsgBaseList; AColor: TColor);
begin
  case APoints.ListType of
    ltInteger: DoPixel(TsgPointsListHelper(APoints).Points[0], AColor);
  end;
end;

procedure TsgProxyBase.DoPolygonList(const APoints: TsgBaseList);
begin
  case APoints.ListType of
    ltInteger: DoPolygon(PPoint(TsgIntegerList(APoints).List), APoints.Count div 2);
  end;
end;

procedure TsgProxyBase.DoPolylineList(const APoints: TsgBaseList);
begin
  case APoints.ListType of
    ltInteger: DoPolyline(PPoint(TsgIntegerList(APoints).List), APoints.Count div 2);
  end;
end;

procedure TsgProxyBase.DoPolyPolygonList(const APoints: TsgIntegerList;
  const ACounts: TsgIntegerList);
begin
  DoPolyPolygon(APoints.List^, PInteger(ACounts.List), ACounts.Count);
end;

procedure TsgProxyBase.DoPolyPolylineList(const APoints: TsgIntegerList;
  const ACounts: TsgIntegerList);
begin
  DoPolyPolyline(APoints.List^, PInteger(ACounts.List), ACounts.Count);
end;

procedure TsgProxyBase.DoPolyPoly(const Points: array of TPointF;
  const Counts: array of Integer; AFillMode: Integer = 2; AUseFont: Boolean = False);
var
  I, C: Integer;
  vPoints: PPoints;
begin
  if AFillMode <> 0 then
  begin
    C := 0;
    for I := Low(Counts) to High(Counts) do Inc(C, Counts[I]);
    GetMem(Pointer(vPoints), C * SizeOf(TPoint));
    try
      for I := 0 to C - 1 do
      begin
        vPoints^[I].X := Round(Points[I].X);
        vPoints^[I].Y := Round(Points[I].Y);
      end;
      if AFillMode and 2 <> 0 then
        DoPolyPolygon(vPoints^, @Counts[0], Length(Counts));
      if AFillMode and 1 <> 0 then
        DoPolyPolyline(vPoints^, @Counts[0], Length(Counts));
    finally
      FreeMem(vPoints);
    end;
  end;
end;

procedure TsgProxyBase.DoTextOut(X, Y: Integer; const AText: string;
  const ACount: Longint = 0);
begin
  DoTextOutAW(X, Y, {$IFDEF UNICODE}'', AText{$ELSE}AText, ''{$ENDIF}, nil, ACount);
end;

procedure TsgProxyBase.DoTextOutW(X, Y: Integer; const AText: WideString;
  const ACount: Longint = 0);
begin
  DoTextOutAW(X, Y, '', AText, nil, ACount);
end;

procedure TsgProxyBase.EndScene;
begin
end;

function TsgProxyBase.GetHeight: Integer;
begin
  Result := Size.Y;
end;

function TsgProxyBase.GetSize: TPoint;
begin
  Result := Point(0, 0);
end;

function TsgProxyBase.GetWidth: Integer;
begin
  Result := Size.X;
end;

function TsgProxyBase.DoSetPolyFillMode(PolyFillMode: Integer): Integer;
begin
  Result := 0;
end;

{ TsgAliasList }

{ TsgAliasList }

procedure TsgAliasList.AddAlias(const ASource, AAlias: string; AType: Integer);
var
  vAliasIndex: Integer;
  vSource: string;
begin
  if AType = 0 then
    vSource := ASource
  else
    vSource := ASource + '*' + IntToStr(AType);
  vAliasIndex := FAliasies.Add(AAlias);
  FSourceName.AddObject(vSource, TObject(vAliasIndex));
end;

procedure TsgAliasList.Clear;
begin
  FAliasies.Clear;
  FSourceName.Clear;
end;

constructor TsgAliasList.Create;
begin
  FAliasies := TStringList.Create;
  FSourceName := TStringList.Create;
  FSourceName.Sorted := True;
end;

destructor TsgAliasList.Destroy;
begin
  FreeAndNil(FAliasies);
  FreeAndNil(FSourceName);
  inherited;
end;

function TsgAliasList.GetAliasByOriginalName(
  const AOriginalName: string; AType: Integer): string;
var
  vSourceIndex: Integer;
  vAliasIndex: Integer;
  vOriginalName: string;
begin
  Result := '';
  if AType = 0 then
    vOriginalName := AOriginalName
  else
    vOriginalName := AOriginalName + '*' + IntToStr(AType);
  vSourceIndex := FSourceName.IndexOf(vOriginalName);
  if vSourceIndex > -1 then
  begin
    vAliasIndex := Integer(FSourceName.Objects[vSourceIndex]);
    Result := FAliasies[vAliasIndex];
  end;
end;

{ TsgMap }

function TsgMap.GetPixel(X, Y: Integer): TColor;
var
  P: PByte;
begin
  P := ScanLine[Y];
  Result := clBlack;
  case PixelFormat of
    pf32bit:
      begin
        Inc(P, X shl 2);
        PByteArray(@Result)^[0] := PByteArray(P)^[2];
        PByteArray(@Result)^[1] := PByteArray(P)^[1];
        PByteArray(@Result)^[2] := PByteArray(P)^[0];
        PByteArray(@Result)^[3] := {$IFDEF SG_FIREMONKEY}255-{$ENDIF}PByteArray(P)^[3];
      end;
    pf24bit:
      begin
        Inc(P, 3 * X);
        PByteArray(@Result)^[0] := PByteArray(P)^[2];
        PByteArray(@Result)^[1] := PByteArray(P)^[1];
        PByteArray(@Result)^[2] := PByteArray(P)^[0];
      end;
    pf16bit:
      begin
        Inc(P, X shl 1);
        PByteArray(@Result)^[0] := (PWord(P)^ shr 11 and $1F) * 8;
        PByteArray(@Result)^[1] := (PWord(P)^ shr 5  and $3F) * 4;
        PByteArray(@Result)^[2] := (PWord(P)^        and $1F) * 8;
      end;
    pf15bit:
      begin
        Inc(P, X shl 1);
        PByteArray(@Result)^[0] := (PWord(P)^ shr 10 and $1F) * 8;
        PByteArray(@Result)^[1] := (PWord(P)^ shr 5  and $1F) * 8;
        PByteArray(@Result)^[2] := (PWord(P)^        and $1F) * 8;
      end;
    pf8bit:
      begin
        Inc(P, X);
        if Length(Pal) = 0 then
        begin
          {$IFDEF SG_FIREMONKEY}
          PByteArray(@Result)^[3] := P^;
          {$ELSE}
          Result := 0;
          {$ENDIF}
          PByteArray(@Result)^[0] := P^;
          PByteArray(@Result)^[1] := P^;
          PByteArray(@Result)^[2] := P^;
        end
        else
        begin
          Result := Pal[P^];
          BSwap(PByteArray(@Result)^[0], PByteArray(@Result)^[2]);
        end;
      end;
    pf4bit:
      begin
        Inc(P, X shr 1);
        Result := Pal[P^ shr (Ord(X and 1 = 0) * 4) and $F];
        BSwap(PByteArray(@Result)^[0], PByteArray(@Result)^[2]);
      end;
    pf1bit:
      begin
        Inc(P, X shr 3);
        Result := Pal[Ord(P^ shl (X and $7) and $80 <> 0)];
        BSwap(PByteArray(@Result)^[0], PByteArray(@Result)^[2]);
      end;
  end;
end;

function TsgMap.GetPixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
begin
  Result := pf24bit;
  if FDescription.IsComponent = 0 then
  begin
    case FDescription.BitsPerPixel of
      1: Result := pf1bit;
      2..4: Result := pf4bit;
      5..8: Result := pf8bit;
    end;
  end
  else
    if FDescription.A = 0 then
    begin
      case FDescription.R + FDescription.G + FDescription.B of
        17..24: Result := pf24bit;
        9..15: Result := pf15bit;
        16: Result := pf16bit;
      end;
    end
    else
      Result := pf32bit;
end;

procedure TsgMap.SetPixel(X, Y: Integer; AValue: TColor);
var
  P: PByte;
  Index: Integer;
  Mask: Byte;
  Shift: Byte;
begin
  P := PByte(Scanline[Y]);
  case PixelFormat of
    pf32bit:
      begin
        Inc(P, X shl 2);
        PByteArray(P)^[0] := PByteArray(@AValue)^[2];
        PByteArray(P)^[1] := PByteArray(@AValue)^[1];
        PByteArray(P)^[2] := PByteArray(@AValue)^[0];
        PByteArray(P)^[3] := {$IFDEF SG_FIREMONKEY}255-{$ENDIF}PByteArray(@AValue)^[3];
      end;
    pf24bit:
      begin
        Inc(P, 3 * X);
        PByteArray(P)^[0] := PByteArray(@AValue)^[2];
        PByteArray(P)^[1] := PByteArray(@AValue)^[1];
        PByteArray(P)^[2] := PByteArray(@AValue)^[0];
      end;
    pf16bit:
      begin
        Inc(P, X shl 1);
        PWord(P)^ := ((AValue and $FF) div 8) shl 11;
        PWord(P)^ := PWord(P)^ or (((AValue shr 8) and $FF) div 4) shl 5;
        PWord(P)^ := PWord(P)^ or (((AValue shr 16) and $FF) div 8);
      end;
    pf15bit:
      begin
        Inc(P, X shl 1);
        PWord(P)^ := ((AValue and $FF) div 8) shl 10;
        PWord(P)^ := PWord(P)^ or (((AValue shr 8) and $FF) div 8) shl 5;
        PWord(P)^ := PWord(P)^ or (((AValue shr 16) and $FF) div 8);
      end;
    pf8bit:
      begin
        Inc(P, X);
        BSwap(PByteArray(@AValue)^[0], PByteArray(@AValue)^[2]);
        P^ := PalIndex[AValue];
      end;
    pf4bit:
      begin
        Inc(P, X shr 1);
        Shift := (X and 1) shl 2;
        Mask := not ($F0 shr Shift);
        BSwap(PByteArray(@AValue)^[0], PByteArray(@AValue)^[2]);
        Index := (PalIndex[AValue] shl 4) shr Shift;
        P^ := (P^ and Mask) or Index;
      end;
    pf1bit:
      begin
        Inc(P, X shr 3);
        Shift := X and $7;
        Mask := not ($80 shr Shift);
        BSwap(PByteArray(@AValue)^[0], PByteArray(@AValue)^[2]);
        Index := (Ord(PalIndex[AValue] <> 0) shl 7) shr Shift;
        P^ := (P^ and Mask) or Index;
      end;
  end;
end;

procedure TsgMap.Assign(Source: TPersistent);
begin
  if Source is TBitmap then
    CopyFromBitmap(TBitmap(Source))
  else
    if Source is TsgMap then
      TsgMap(Source).CopyTo(Self)
    else
      inherited Assign(Source);
end;

procedure TsgMap.AssignTo(Dest: TPersistent);
begin
  if Dest is TsgMap then
    CopyTo(TsgMap(Dest))
  else
    if Dest is TBitmap then
      CopyToBitmap(TBitmap(Dest))
    else
      inherited AssignTo(Dest);
end;

function TsgMap.BSwap(var A, B: Byte): Byte;
begin
  Result := A;
  A := B;
  B := Result;
end;

procedure TsgMap.CopyTo(AMap: TsgMap);
var
  P: Pointer;
begin
  P := AMap.ReleaseData;
  FreeMemAndNil(P);
  AMap.FWidth := FWidth;
  AMap.FHeight := FHeight;
  AMap.FBytesPerScanline := FBytesPerScanline;
  AMap.FDescription := FDescription;
  if FData <> nil then
  begin
    GetMem(AMap.FData, FHeight * FBytesPerScanline);
    System.Move(FData^, AMap.FData^, FBytesPerScanline * FHeight);
  end;
  AMap.Pal := Pal;
end;

procedure TsgMap.CopyToBitmap(ABitmap: TBitmap);
{$IFNDEF SG_FIREMONKEY}
var
  Y: Integer;
begin
  ABitmap.PixelFormat := PixelFormat;
  SetSizeGraphic(TGraphic(ABitmap), FWidth, FHeight);
  if (PixelFormat in [pf1bit .. pf8bit]) and (Length(Pal) > 0) then
    ABitmap.Palette := CreateHPalette;
  for Y := 0 to FHeight - 1 do
    System.Move(ScanLine[Y]^, ABitmap.ScanLine[Y]^, FBytesPerScanline);
end;
{$ELSE}
var
  Y, X: Integer;
  vBitmapData: TBitmapData;
begin
  ABitmap.SetSize(FWidth, FHeight);
  if ABitmap.Map(TMapAccess.Write, vBitmapData) then
  try
    for Y := 0 to FHeight - 1 do
      for X := 0 to FWidth - 1 do
        vBitmapData.SetPixel(X, FHeight - Y - 1, Pixels[X, Y].AsBGRA);
  finally
    ABitmap.Unmap(vBitmapData);
  end;
end;
{$ENDIF}

procedure TsgMap.CopyFromBitmap(ABitmap: TBitmap);
var
  Y: Integer;
{$IFDEF SG_FIREMONKEY}
  vBitmapData: TBitmapData;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  FreeMemAndNil(FData);
  InitDescription(ABitmap.PixelFormat, FDescription);
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  if (ABitmap.PixelFormat in [pf1bit .. pf8bit]) then
    InitPal(ABitmap.Palette);
  AllocMap;
  for Y := 0 to FHeight - 1 do
    System.Move(ABitmap.ScanLine[Y]^, Scanline[Y]^, FBytesPerScanline);
{$ELSE}
  FreeMemAndNil(FData);
  SetLength(Pal, 0);
  InitDescription(pf32bit, FDescription);
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  AllocMap;
  ABitmap.Map(TMapAccess.Read, vBitmapData);
  try
    for Y := 0 to FHeight - 1 do
      System.Move(vBitmapData.GetScanline(FHeight - Y - 1)^, ScanLine[Y]^, BytesPerScanline);
  finally
    ABitmap.Unmap(vBitmapData);
  end;
{$ENDIF}
end;
{$IFDEF SGFPC}
procedure TsgMap.InitPal(APalette: TFPPalette);
var
  I: Integer;
begin
  if Assigned(APalette) then
  begin
    SetLength(Pal, APalette.Count);
    for I := 0 to APalette.Count - 1 do
      Pal[I] := FPColorToTColor(APalette.Color[I]);
  end
  else
    SetLength(Pal, 0);
end;
{$ENDIF}
procedure TsgMap.InitPal(Colors: Pointer; Count: Integer; AElementTypeInfo: Pointer = nil);
var
  I: Integer;
begin
  SetLength(Pal, Count);
  if Count > 0 then
  begin
    SetLength(Pal, Count);
    System.Move(Colors^, Pal[0], Length(Pal) * SizeOf(Pal[0]));
    for I := 0 to Count - 1 do SwapByte(PByteArray(@Pal[I])^[0], PByteArray(@Pal[I])^[2]);
  end;
end;

class function TsgMap.InitDescription(APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
  var ADescription: TsgMapDescription): Boolean;
begin
  Result := True;
  FillChar(ADescription, SizeOf(TsgMapDescription), 0);
  case APixelFormat of
    pf1bit: ADescription.BitsPerPixel := 1;
    pf4bit: ADescription.BitsPerPixel := 4;
    pf8bit: ADescription.BitsPerPixel := 8;
    pf15bit:
      begin
        ADescription.BitsPerPixel := 15;
        ADescription.ShiftA := 0;
        ADescription.ShiftR := 0;
        ADescription.ShiftG := 5;
        ADescription.ShiftB := 10;
        ADescription.A := 0;
        ADescription.R := 5;
        ADescription.G := 5;
        ADescription.B := 5;
      end;
    pf16bit:
      begin
        ADescription.BitsPerPixel := 16;
        ADescription.ShiftA := 0;
        ADescription.ShiftR := 0;
        ADescription.ShiftG := 5;
        ADescription.ShiftB := 11;
        ADescription.A := 0;
        ADescription.R := 5;
        ADescription.G := 6;
        ADescription.B := 5;
      end;
    pf24bit:
      begin
        ADescription.BitsPerPixel := 24;
        ADescription.ShiftA := 0;
        ADescription.ShiftR := 0;
        ADescription.ShiftG := 8;
        ADescription.ShiftB := 16;
        ADescription.A := 0;
        ADescription.R := 8;
        ADescription.G := 8;
        ADescription.B := 8;
      end;
    pf32bit:
      begin
        ADescription.BitsPerPixel := 32;
        ADescription.ShiftA := 24;
        ADescription.ShiftR := 0;
        ADescription.ShiftG := 8;
        ADescription.ShiftB := 16;
        ADescription.A := 8;
        ADescription.R := 8;
        ADescription.G := 8;
        ADescription.B := 8;
      end;
  else
    Result := False;
  end;
end;

{$IFNDEF SG_FIREMONKEY}
procedure TsgMap.InitPal(HPal: HPALETTE);
const
  cnstMaxColors = 256;
var
  Colors: Pointer;
begin
  GetMem(Colors, cnstMaxColors * SizeOf(Integer));
  try
    InitPal(Colors, GetPaletteEntries(HPal, 0, cnstMaxColors, Colors^));
  finally
    FreeMem(Colors, cnstMaxColors * SizeOf(Integer));
  end;
end;
{$ENDIF}

procedure TsgMap.UpdatePal;
var
  X, Y: Integer;
begin
  SetLength(Pal, 0);
  if PixelFormat in [pf1bit .. pf8bit] then
    for Y := 0 to FHeight - 1 do
      for X := 0 to FWidth - 1 do
        PalIndex[Pixels[X, Y]];
end;

class function TsgMap.Wrap(AWidth, AHeight: Integer;
  const ADescription: TsgMapDescription; Data: Pointer; AColors: Pointer;
  ACount: Integer; AAlignment: Integer): TsgMap;
begin
  Result := TsgMap(NewInstance);
  Result.FData := Data;
  Result.Create(AWidth, AHeight, ADescription, AAlignment);
  Result.InitPal(AColors, ACount);
end;

constructor TsgMap.Create(AWidth, AHeight: Integer; APixelFormat:
  {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat = pf32bit;
  AAlignment: Integer = 32);
const
  cnstBPPs: array[{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat] of Byte = (24{?},1,4,8,16,16,24,32,32{?});
begin
  FWidth := AWidth;
  FHeight := AHeight;
  InitDescription(APixelFormat, FDescription);
  Dec(AAlignment);
  FBytesPerScanline := (FWidth * cnstBPPs[APixelFormat] + AAlignment) and not AAlignment shr 3;
  if FData = nil then
    AllocMap;
end;

constructor TsgMap.Create(AWidth, AHeight: Integer;
  const ADescription: TsgMapDescription; AAlignment: Integer = 32);
const
  cnstBPPs: array[{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat] of Byte = (24{?},1,4,8,16,16,24,32,32{?});
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FDescription := ADescription;
  Dec(AAlignment);
  FBytesPerScanline := (FWidth * cnstBPPs[PixelFormat] + AAlignment) and not AAlignment shr 3;
  if FData = nil then
    AllocMap;
end;

destructor TsgMap.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

class function TsgMap.Wrap(AWidth, AHeight: Integer;
  APixelFormat: {$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TPixelFormat;
  Data: Pointer; AColors: Pointer; ACount: Integer;
  AAlignment: Integer = 32): TsgMap;
begin
  Result := TsgMap(NewInstance);
  Result.FData := Data;
  Result.Create(AWidth, AHeight, APixelFormat, AAlignment);
  Result.InitPal(AColors, ACount);
end;

class function TsgMap.Wrap(ABitmap: TBitmap; AReadOnly: Boolean = False): TsgMap;
{$IFDEF SG_FIREMONKEY}
var
  BitmapData: TBitmapData;
{$ENDIF}
{$IFDEF SGFPC}
var
  vLazIntfImage: TLazIntfImage;
{$ENDIF}
begin
  Result := TsgMap(NewInstance);
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGFPC}
  Result.FData := ABitmap.RawImage.Data;
{$ELSE}
  Result.FData := ABitmap.ScanLine[ABitmap.Height - 1];
{$ENDIF}
{$ELSE}
  if AReadOnly then
    ABitmap.Map(TMapAccess.Read, BitmapData)
  else
    ABitmap.Map(TMapAccess.ReadWrite, BitmapData);
  Result.FData := BitmapData.Data;
{$ENDIF}
  Result.Create(ABitmap.Width, ABitmap.Height, ABitmap.PixelFormat);
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGFPC}
  vLazIntfImage := ABitmap.CreateIntfImage;
  try
    Result.InitPal(vLazIntfImage.Palette);
  finally
    vLazIntfImage.Free;
  end;
{$ELSE}
  if ABitmap.Palette <> 0 then
    Result.InitPal(ABitmap.Palette);
{$ENDIF}
{$ELSE}
  Result.FOwner := ABitmap;
{$ENDIF}
end;

class function TsgMap.Unwrap(var AMap: TsgMap): Pointer;
{$IFDEF SG_FIREMONKEY}
var
  BitmapData: TBitmapData;
{$ENDIF}
begin
  Result := AMap.ReleaseData;
{$IFDEF SG_FIREMONKEY}
  if AMap.GetOwner is TBitmap then
  begin
    BitmapData := TBitmapData.Create(AMap.Width, AMap.Height, TPixelFormat.RGBA);
    BitmapData.Data := Result;
    BitmapData.Pitch := AMap.BytesPerScanline;
    TBitmap(AMap.GetOwner).Unmap(BitmapData);
  end;
{$ENDIF}
  FreeAndNil(AMap);
end;

procedure TsgMap.AllocMap;
begin
  GetMem(FData, FBytesPerScanline * FHeight);
  FillChar(FData^, FBytesPerScanline * FHeight, 0);
end;

procedure TsgMap.ReallocMap;
begin
  if FData <> nil then
  begin
    Freemem(FData);
    FData := nil;
  end;
  AllocMap;
end;

function TsgMap.ReleaseData: Pointer;
begin
  Result := FData;
  FData := nil;
end;

class function TsgMap.GetNearestPalIndex(AColor: TColor;
  const APal: array of Integer; var Index: Integer): Boolean;
const
  cnsMaxPalDistance = 4 * 512;
var
  I, H: Integer;
  vDelta, vMinDelta: Integer;
  P, C: PByteArray;
begin
  Result := False;
  I := Low(APal);
  H := High(APal);
  Index := I;
  vMinDelta := cnsMaxPalDistance;
  if H - I >= 0 then
  begin
    C := PByteArray(@AColor);
    P := PByteArray(@APal[I]);
    repeat
      vDelta := Abs(P^[0] - C^[0]) shl 1 + Abs(P^[1] - C^[1]) shl 1 +
        Abs(P^[2] - C^[2]) shl 1 + Abs(P^[3] - C^[3]) shl 1;
      if vDelta < vMinDelta then
      begin
        vMinDelta := vDelta;
        Result := vMinDelta = 0;
        Index := I;
      end;
      Inc(PByte(P), SizeOf(APal[I]));
      Inc(I);
    until (I > H) or Result;
  end;
end;

function TsgMap.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TsgMap.GetPalIndex(AColor: TColor): Integer;
var
  vFind: Boolean;
begin
  vFind := False;
  Result := 0;
  while (Result < Length(Pal)) and not vFind do
    if Pal[Result] = AColor then
      Inc(vFind)
    else
      Inc(Result);
  if not vFind then
  begin
    Result := Length(Pal);
    SetLength(Pal, Result + 1);
    Pal[Result] := AColor;
  end;
end;

function TsgMap.GetScanline(Y: Integer): Pointer;
begin
  Result := Pointer(IntPtr(FData) + (FHeight - Y - 1) * FBytesPerScanline);
end;

{$IFNDEF SG_FIREMONKEY}
function TsgMap.CreateHPalette: HPALETTE;
var
  I: Integer;
  vlp: PLogPalette;
begin
  Result := 0;
  if Length(Pal) > 0 then
  begin
    GetMem(vlp, SizeOf(TLogPalette) + (Length(Pal) - 1) * SizeOf(TPaletteEntry));
    try
      vlp^.palVersion := $300;
      vlp^.palNumEntries := Length(Pal);
      for I := 0 to vlp^.palNumEntries - 1 do
      begin
        vlp^.palPalEntry[I].peRed := PByteArray(@Pal[I])^[2];
        vlp^.palPalEntry[I].peGreen := PByteArray(@Pal[I])^[1];
        vlp^.palPalEntry[I].peBlue := PByteArray(@Pal[I])^[0];
        vlp^.palPalEntry[I].peFlags := 0;
      end;
      Result := CreatePalette(vlp^);
    finally
      FreeMem(vlp);
    end;
  end;
end;
{$ENDIF}

function IsFileOLEStorage(const AFileName: string): Boolean;
var
  vStream: TFileStream;
begin
  try
    vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := IsStreamOLEStorage(vStream);
    finally
      vStream.Free;
    end;
  except
    Result := False;
  end;
end;

function IsStreamOLEStorage(const AStream: TStream): Boolean;
type
  TsgOLEStorageSignature = packed array[0..7] of Byte;
const
  cnstOLEStorageSignature: TsgOLEStorageSignature = ($D0, $CF, $11, $E0, $A1, $B1, $1A, $E1);
var
  vSignature: TsgOLEStorageSignature;
begin
  Result := False;
  try
    if AStream.Size <= SizeOf(TsgOLEStorageSignature) then
      Exit;
    AStream.Position := 0;
    AStream.Read(vSignature, SizeOf(TsgOLEStorageSignature));
    Result := CompareMem(@vSignature, @cnstOLEStorageSignature, SizeOf(TsgOLEStorageSignature));
  except
    Result := False;
  end;
end;

function IsSpace(const AChar: WideChar): Boolean;
begin
{$IFDEF SGDEL_10_SEATTLE}
  Result := AChar.IsWhiteSpace;
{$ELSE}
{$IFDEF SGDEL_2009}
  Result := TCharacter.IsWhiteSpace(AChar);
{$ELSE}
  case AChar of
    #9, #10, #13, #32: Result := True;
  else
    Result := False;
  end;
{$ENDIF}
{$ENDIF}
end;

function IsSpace(const AChar: AnsiChar): Boolean;
begin
  case AChar of
    #9, #10, #13, #32: Result := True;
  else
    Result := False;
  end;
end;

{$IFDEF SG_HAS_WINAPI_INTERFACE}

function MKBIH32(var ABitmapInfoHeader: {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}TBitmapInfoHeader; AWidth, AHeight: Integer): PBitmapInfoHeader;
begin
  FillChar(ABitmapInfoHeader, SizeOf(ABitmapInfoHeader), 0);
  ABitmapInfoHeader.biSize := SizeOf(ABitmapInfoHeader);
  ABitmapInfoHeader.biWidth := AWidth;
  if ABitmapInfoHeader.biWidth = 0 then
    ABitmapInfoHeader.biWidth := 1;
  ABitmapInfoHeader.biHeight := -AHeight;
  if ABitmapInfoHeader.biHeight = 0 then
    ABitmapInfoHeader.biHeight := -1;
  ABitmapInfoHeader.biPlanes := 1;
  ABitmapInfoHeader.biBitCount := 32;
  ABitmapInfoHeader.biCompression := BI_RGB;
  ABitmapInfoHeader.biSizeImage := Abs(ABitmapInfoHeader.biHeight) * ABitmapInfoHeader.biWidth * 4;
  Result := @ABitmapInfoHeader;
end;

constructor TsgDIBBitmap32.Create(AWidth, AHeight: Integer);
begin
  FBufferHandle := CreateCompatibleDC(0);
  if FBufferHandle = 0 then
   {$IFNDEF SGDEL_6}RaiseLastWin32Error{$ELSE}RaiseLastOSError{$ENDIF};
  SetSize(AWidth, AHeight);
end;

destructor TsgDIBBitmap32.Destroy;
begin
  SelectObject(FBufferHandle, FStockHandle);
  DeleteDC(FBufferHandle);
  TsgMap.Unwrap(FMap);
  DeleteObject(FDIBHandle);
  inherited Destroy;
end;

procedure TsgDIBBitmap32.SetSize(AWidth, AHeight: Integer);
var
  vBits: Pointer;
{$IFDEF LCLGTK2}
  vRawImage: TRawImage;
{$ELSE}
  vBitmapInfo: {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}TBitmapInfo;
{$ENDIF}
begin
  if (FMap = nil) or ((FMap.Width <> AWidth) or (FMap.Height <> AHeight)) then
  begin
    if FStockHandle <> 0 then
    begin
      SelectObject(FBufferHandle, FStockHandle);
      FStockHandle := 0;
    end;
    if FDIBHandle <> 0 then
    begin
      DeleteObject(FDIBHandle);
      FDIBHandle := 0;
    end;
{$IFNDEF LCLGTK2}
    MKBIH32(vBitmapInfo.bmiHeader, AWidth, AHeight);
    FDIBHandle := {$IFDEF SG_WIN_FPC}Windows.{$ENDIF}CreateDIBSection(0, vBitmapInfo, DIB_RGB_COLORS, Pointer(vBits), 0, 0);
    if FDIBHandle = 0 then
     {$IFNDEF SGDEL_6}RaiseLastWin32Error{$ELSE}RaiseLastOSError{$ENDIF};
{$ELSE}
    FDIBHandle := CreateBitmap(AWidth, AHeight, 1, 32, nil);
    if FDIBHandle = 0 then
      {$IFNDEF SGDEL_6}RaiseLastWin32Error{$ELSE}RaiseLastOSError{$ENDIF};
    if RawImage_FromBitmap(vRawImage, FDIBHandle, 0) then
      vBits := Pointer(vRawImage.Data);
{$ENDIF}
    FStockHandle := SelectObject(FBufferHandle, FDIBHandle);
    if FMap = nil then
      FMap := TsgMap.Wrap(AWidth, AHeight, pf32bit, vBits, nil, 0)
    else
    begin
      FMap.FData := vBits;
      FMap.Create(AWidth, AHeight);
    end;
  end;
end;

{$ENDIF}

function SAR(AValue: Integer; const ACount: Byte): Integer;
{$IFDEF SG_ASSEMBLER}
asm
{$IFDEF SG_CPUX64}
  MOV EAX,ECX               // value to result
{$ENDIF}
  MOV CL,DL                 // cl - shift count
  SAR EAX,CL                // do shift
end;
{$ELSE}
begin
  if AValue >= 0 then
    Result := AValue shr ACount
  else
    if ACount mod (SizeOf(Result) shl 3) = 0 then
      Result := AValue
    else
      Result := (AValue shr ACount) or ($FFFFFFFF shl ((SizeOf(Result) shl 3) - ACount));
end;
{$ENDIF}

function SAR16(AValue: SmallInt; const ACount: Byte): SmallInt;
{$IFDEF SG_ASSEMBLER}
asm
{$IFDEF SG_CPUX64}
  MOV AX,CX               // value to result
{$ENDIF}
  MOV CL,DL                 // cl - shift count
  SAR AX,CL                // do shift
end;
{$ELSE}
begin
  if AValue >= 0 then
    Result := AValue shr ACount
  else
    if ACount mod (SizeOf(Result) shl 3) = 0 then
      Result := AValue
    else
      Result := (AValue shr ACount) or ($FFFF shl ((SizeOf(Result) shl 3) - ACount));
end;
{$ENDIF}

function SAR8(AValue: Byte; const ACount: Byte): Byte;
{$IFDEF SG_ASSEMBLER}
asm
{$IFDEF SG_CPUX64}
  MOV AX,CX               // value to result
{$ENDIF}
  MOV CL,DL                 // cl - shift count
  SAR AL,CL                // do shift
end;
{$ELSE}
begin
  if ShortInt(AValue) >= 0 then
    Result := AValue shr ACount
  else
    if ACount mod (SizeOf(Result) shl 3) = 0 then
      Result := AValue
    else
      Result := (AValue shr ACount) or ($FF shl ((SizeOf(Result) shl 3) - ACount));
end;
{$ENDIF}

//procedure Test;
//var
//  vP1,vP2, vP3, vP4: TFPoint;
//begin
//  vP1 := MakeFpoint(5, 5, 1);
//  vP2 := MakeFpoint(-5, -5, 1);
//  vP3 := MakeFpoint(-5, 5, 1);
//  vP4 := MakeFpoint(5, -5, 1);
//  IsCrossSegmentsPts3D(vP1, vP2, vP3, vP4, nil);
//end;

{ TsgMapList }

function TsgMapList.ComparePointers(const A, B: Pointer): Integer;
var
  H1, H2: UInt64;
begin
  Result := 0;
  H1 := PPdwgMapElementList(A)^^.Handle;
  H2 := PPdwgMapElementList(B)^^.Handle;
  if H1 > H2 then
    Inc(Result)
  else
    if H1 < H2 then
      Dec(Result);
end;

function ACIS_Cipher(const AChar: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(ACIS_Cipher(Byte(AChar)));
end;

function ACIS_Cipher(const AByte: Byte): Byte;
begin
  if AByte in [33..126] then
    Result := 159 - Byte(AByte)
  else
    Result := AByte;
end;

procedure InitProc;
begin
  {$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}
  FormatSettings_DoubleSeparator := {$IFDEF SGFPC}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};
  FormatSettings_DoubleSeparator.DecimalSeparator := cnstDoubleSeparator;
  FormatSettings_Point := {$IFDEF SGFPC}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};
  FormatSettings_Point.DecimalSeparator := cnstDecimalSeparatorPoint;
  {$ENDIF}
  {$IFDEF SG_FIREMONKEY}
  cnstDirectorySeparator := System.IOUtils.TPath.DirectorySeparatorChar;
  cnstExtensionSeparator := System.IOUtils.TPath.ExtensionSeparatorChar;
  {$ELSE}
  cnstDirectorySeparator := '\';
  cnstExtensionSeparator := '.';
  {$ENDIF}
end;

function _GetGlobalPropProvider(AInstance: TObject; AOwnsProps: Boolean;
  out AGlobalPropProvider: IsgGlobalPropProvider): Boolean;
begin
  Result := Assigned(AInstance);
  if Result then
    AGlobalPropProvider := TsgGlobalPropProvider.Create(AInstance, AOwnsProps);
end;

{ TsgGlobalPropProvider }

function CompareProp(const Value1, Value2): Integer;
begin
  Result := 0;
  if TsgProp(Value1).Hash > TsgProp(Value2).Hash then
    Inc(Result)
  else
    if TsgProp(Value1).Hash < TsgProp(Value2).Hash then
      Dec(Result)
end;

procedure TsgGlobalPropProvider.Clear;
var
  I: Integer;
  Item: TsgProp;
begin
  Item.Hash := MakeHash(PChar($FFFF));
  if not Find(Item, I) then Dec(I);
  while (I >= 0) and (PsgIntegerArray(@Props[I].Hash)^[1] = FHash) do
  begin
    DeleteItem(I);
    Dec(I);
  end;
end;

constructor TsgGlobalPropProvider.Create(const AInstance: TObject; AOwnsProps: Boolean);
begin
  Create(
{$IFDEF HAS_OBJECT_GETHASHCODE}
    AInstance.GetHashCode
{$ELSE}
    Integer(AInstance)
{$ENDIF},
    AOwnsProps);
end;

constructor TsgGlobalPropProvider.Create(const AHash: Integer;
  AOwnsProps: Boolean);
begin
  FOwnsProps := AOwnsProps;
  FHash := AHash;
end;

procedure TsgGlobalPropProvider.DeleteItem(Index: Integer);
begin
  VarClear(Props[Index].Value);
  Dec(PropsCount);
  if Index < PropsCount then
  begin
    System.Move(Props[Index + 1], Props[Index], (PropsCount - Index) * SizeOf(TsgProp));
    FillChar(Props[PropsCount].Value, SizeOf(Props[PropsCount].Value), 0);
  end;
  Props[PropsCount].Hash := Int64($FFFFFFFFFFFFFFFF);
end;

destructor TsgGlobalPropProvider.Destroy;
begin
  if FOwnsProps then
    Clear;
  inherited Destroy;
end;

function TsgGlobalPropProvider.Find(const Item: TsgProp;
  var Index: Integer): Boolean;
begin
  Result := QFindItem(Props[0], 0, PropsCount - 1, Item, CompareProp, SizeOf(TsgProp), Index);
end;

function TsgGlobalPropProvider.Find(Name: PChar;
  var Index: Integer): Boolean;
var
  Item: TsgProp;
begin
  Item.Hash := MakeHash(Name);
  Result := Find(Item, Index);
end;

function TsgGlobalPropProvider.GetItem(Name: PChar): Variant;
var
  I: Integer;
begin
  if Find(Name, I) then
    Result := Props[I].Value
  else
    VarClear(Result);
end;

class function TsgGlobalPropProvider.hash_name(name: PChar): Integer;
var
  P: PChar;
begin
  P := name;
  if TsgNativeUInt(P) > $FFFF then
  begin
    Result := 0;
    while P^ <> #0 do
    begin
      if (P^ >= 'A') and (P^ <= 'Z') then
        Result := Result xor (Ord(P^) or $20)
      else
        Result := Result xor Ord(P^);
      Result := (Result shr 27) or (Result shl 5);
      Inc(P);
    end;
  end
  else
    Result := Integer($FFFF0000) or Integer(P);
end;

function TsgGlobalPropProvider.MakeHash(name: PChar): Int64;
begin
  PsgIntegerArray(@Result)^[1] := FHash;
  PsgIntegerArray(@Result)^[0] := hash_name(name);
end;

function TsgGlobalPropProvider.RemoveItem(Name: PChar): Integer;
begin
  if Find(Name, Result) then
    DeleteItem(Result)
  else
    Result := -1;
end;

procedure TsgGlobalPropProvider.SetItem(Name: PChar; const Value: Variant);
var
  I: Integer;
  Item: TsgProp;
begin
  Item.Hash := MakeHash(Name);
  if Find(Item, I) then
  begin
    if TVarData(Value).VType = varEmpty then
      DeleteItem(I)
    else
      Props[I].Value := Value;
  end
  else
  begin
    if PropsCount = Length(Props) then
      SetLength(Props, ListGrow(Length(Props)));
    if I < PropsCount then
      System.Move(Props[I], Props[I + 1], (PropsCount - I) * SizeOf(TsgProp));
    FillChar(Props[I].Value, SizeOf(Props[I].Value), 0);
    Props[I].Hash := Item.Hash;
    Props[I].Value := Value;
    Inc(PropsCount);
  end;
end;

function TsgGlobalPropProvider.TryGetValue(Name: PChar;
  out Value: Variant): Boolean;
var
  I: Integer;
begin
  Result := Find(Name, I);
  if Result then
    Value := Props[I].Value
  else
    VarClear(Value);
end;

procedure FinalizeProps;
begin
  while PropsCount > 0 do
  try
    VarClear(Props[PropsCount - 1].Value);
  finally
    Dec(PropsCount);
  end;
  Finalize(Props);
end;

{ TsgColorToLineWeightMap }

procedure TsgColorToLineWeightMap.Assign(const AMap: TsgColorToLineWeightMap);
begin
  Strings.Assign(AMap.Strings);
  Update(3);
end;

procedure TsgColorToLineWeightMap.CorrectStrings;
begin
  Update(3);
  CorrectFileCws(Strings);
end;

constructor TsgColorToLineWeightMap.Create;
begin
  inherited Create;
  Strings := TStringList.Create;
  Keys := nil;
  Weights := nil;
  Colors := nil;
end;

destructor TsgColorToLineWeightMap.Destroy;
begin
  FreeAndNil(Strings);
  FreeAndNil(Keys);
  FreeAndNil(Weights);
  FreeAndNil(Colors);
  inherited Destroy;
end;

function TsgColorToLineWeightMap.GetCount: Integer;
begin
  Result := Strings.Count;
end;

function TsgColorToLineWeightMap.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := LoadFromFileCws(AFileName, Strings);
  Update(Ord(Result) * 3);
end;


procedure TsgColorToLineWeightMap.LoadFromStrings(AColors: TStrings);
begin
  Strings.Assign(AColors);
  Update(0);
end;

procedure TsgColorToLineWeightMap.MakeStrings;
var
  I, vColorNew: Integer;
begin
  Strings.Clear;
  if Assigned(Keys) then
  begin
    Strings.Capacity := Keys.Count;
    for I := 0 to Keys.Count - 1 do
    begin
      vColorNew := cnstColorLWeightColorNewEmpty;
      if Assigned(Colors) then
        vColorNew := Colors[I];
      Strings.Add(ColorLineWeightToString(Keys[I], Weights[I], vColorNew));
    end;
  end;
end;

function TsgColorToLineWeightMap.SaveToFile(const AFileName: string): Boolean;
begin
  Result := SaveToFileCws(Strings, AFileName);
end;

function SGStringToColor(const S: string): TsgColorCAD;
var
  vColor: Integer;
begin
  if IdentToColor(S, vColor) then
  begin
    Result.Active := acRGBColor;
    Result.Color := vColor;
    Result.AlbumString := '';
  end
  else
    Result := ColorLWtoColorCAD(S);
end;

function TsgColorToLineWeightMap.GetColorKey(const AStrKeyValue: string;
  var AKey: TsgColorCAD; var AWeight: Double): Boolean;
var
  vPos: Integer;
  vStr: string;
begin
  Result := False;
  AKey := cnstColorCADLWeightBadKey;
  vPos := AnsiPos(cnstColorLWeightDelimiter, AStrKeyValue);
  if vPos = 0 then
  begin
    AWeight := cnstPenWidthNull;
  end
  else
  begin
    Result := True;
    vStr := Trim(Copy(AStrKeyValue, 1, vPos - 1));
    AKey := SGStringToColor(vStr);
    vStr := Trim(Copy(AStrKeyValue, vPos + 1, Length(AStrKeyValue) - vPos));
    try
      AWeight := sgFunction.StrToDouble(vStr, cnstColorLWeightValSeparator);
    except
      AWeight := cnstPenWidthNull;
    end;
  end;
end;

procedure TsgColorToLineWeightMap.Update(const AClear: Integer);
var
  I, J, vPos: Integer;
  vColorKey: TsgColorCAD;
  vColorToLineWeight: string;
  vColorValue: Double;
  vColorColor: Integer;
  vStrValues: TsgStringList;
begin
  if (AClear and 3 > 0) and (Strings.Count > 0) then
  begin
    if not Assigned(Keys) then
      Keys := TsgColorCADList.Create;
    if not Assigned(Weights) then
      Weights := TsgDoubleList.Create;
    FreeAndNil(Colors);
    Keys.Count := Strings.Count;
    Weights.Count := Strings.Count;
    vStrValues := TsgStringList.Create;
    try
      vStrValues.LineBreak := cnstColorLWeightValuesDelimiter;
      for I := 0 to Strings.Count - 1 do
      begin
        vColorToLineWeight := Strings[I];
        vPos := AnsiPos(cnstColorLWeightValuesDelimiter, vColorToLineWeight);
        if vPos > 0 then
        begin
          vColorKey := cnstColorCADLWeightBadKey;
          vColorValue := cnstPenWidthNull;
          vColorColor := cnstColorLWeightColorNewEmpty;
          vStrValues.Text := vColorToLineWeight;
          if vStrValues.Count > 0 then
          begin
            if not Assigned(Colors) then
            begin
              Colors := TsgIntegerList.Create;
              Colors.Count := Keys.Count;
              for J := 0 to I - 1 do
                Colors[J] := cnstColorLWeightColorNewEmpty;
            end;
            if GetColorKey(vStrValues[0], vColorKey, vColorValue) then
            begin
              if vStrValues.Count > 1 then//Color
                vColorColor := StrToIntDef(vStrValues[1], vColorColor);
            end;
          end;
        end
        else
        begin
          vColorColor := cnstColorLWeightColorNewEmpty;
          GetColorKey(vColorToLineWeight, vColorKey, vColorValue);
        end;
        Keys[I] := vColorKey;
        Weights[I] := vColorValue;
        if Assigned(Colors) then
          Colors[I] := vColorColor;
      end;
    finally
      FreeAndNil(vStrValues);
    end;
    if AClear and 1 = 0 then
      FreeAndNil(Weights);
    if AClear and  2 = 0 then
      FreeAndNil(Colors);
    if not (Assigned(Weights) or Assigned(Colors)) then
      FreeAndNil(Keys);
  end
  else
  begin
    FreeAndNil(Keys);
    FreeAndNil(Weights);
    FreeAndNil(Colors);
  end;
end;

{$IFNDEF SGDEL_6}
function sgVarTypeIsStr(const AVarType: TVarType): Boolean;
begin
  case AVarType of
    varOleStr, varString, varUString:
      Result := True;
  else
    Result := False;
  end;
end;

function sgVarTypeIsOrdinal(const AVarType: TVarType): Boolean;
begin
  case AVarType of
    varSmallInt, varInteger, varBoolean, varShortInt,
      varByte, varWord, varLongWord, varInt64, varUInt64:
      Result := True;
  else
    Result := False;
  end;
end;

function sgVarIsStr(const V: Variant): Boolean;
begin
  Result := sgVarTypeIsStr(TVarData(V).VType);
end;

function sgVarIsOrdinal(const V: Variant): Boolean;
begin
  Result := sgVarTypeIsOrdinal(TVarData(V).VType);
end;

function sgVarAsInt64(const V: Variant): Int64;
begin
  Result := PInt64(@TVarData(V).VPointer)^;
end;

function sgVarAsUInt64(const V: Variant): UInt64;
begin
  Result := PUInt64(@TVarData(V).VPointer)^;
end;

function sgUInt64AsVar(const V: UInt64): Variant;
begin
  VarClear(Result);
  TVarData(Result).VType := varUInt64;
  PUInt64(@TVarData(Result).VPointer)^ := V;
end;
{$ENDIF}

{ TsgConverterObj }

function TsgConverterObj.Convert: Integer;
begin
  Result := 0;
end;

constructor TsgConverterObj.Create(const ADest: TObject);
begin
  inherited Create;
  FDestination := ADest;
end;

function TsgConverterObj.Execute(const ASource: TObject): Integer;
begin
  FSource := ASource;
  Result := Convert;
end;

{ TsgView }

procedure TsgView.Assign(Source: TPersistent);
var
  Src: TsgView;
begin
  if Source is TsgView then
  begin
    Src := TsgView(Source);
    SetLength(FStack, Src.Capacity);
    if Src.Capacity > 0 then
      System.Move(Src.FStack[0], FStack[0], Src.Capacity * SizeOf(FStack[0]));
    FPos := Src.FPos;
    FCount := Src.FCount;
  end
  else
    inherited Assign(Source);
end;

procedure TsgView.Clear;
begin
  FPos := -1;
  FCount := 0;
end;

constructor TsgView.Create(const ACapacity: Integer);
begin
  inherited Create;
  FPos := -1;
  if ACapacity > 0 then
    SetLength(FStack, ACapacity);
end;

function TsgView.GetCapacity: Integer;
begin
  Result := Length(FStack);
end;

function TsgView.HasView(AView: PFMatrix = nil): Boolean;
begin
  Result := (FCount > 0) and ((FPos >= 0) and (FPos < Capacity));
  if Assigned(AView) then
    if Result then
      AView^ := {$IFDEF SG_USE_VIEW_RECORD}ViewRecToMatrix{$ENDIF}(FStack[FPos])
    else
      AView^ := cnstCrossYMat;
end;

function TsgView.PopView(AView: PFMatrix = nil): Boolean;
begin
  Result := False;
  if FCount > 0 then
  begin
    if Assigned(AView) then
      AView^ := {$IFDEF SG_USE_VIEW_RECORD}ViewRecToMatrix{$ENDIF}(FStack[FPos]);
    Result := True;
    Dec(FPos);
    if FPos < 0 then
      FPos := High(FStack);
    Dec(FCount);
  end;
end;

procedure TsgView.PushView(const AView: TFMatrix);
begin
  if Capacity > 0 then
  begin
    Inc(FPos);
    FPos := FPos mod Capacity;
    Inc(FCount);
    if FCount > Capacity then
      FCount := Capacity;
    FStack[FPos] := {$IFDEF SG_USE_VIEW_RECORD}MatrixToViewRec{$ENDIF}(AView);
  end;
end;

procedure TsgView.SetCapacity(const Value: Integer);
begin
  if Value <= 0 then
    SetLength(FStack, 0)
  else
    SetLength(FStack, Value);
  Clear;
end;

{$IFDEF SG_USE_VIEW_RECORD}
class function TsgView.ViewRecToMatrix(const AViewRec: TsgViewRec): TFMatrix;
begin
  Result.EX := MakeFPoint(AViewRec.Right[0], AViewRec.Right[1], AViewRec.Right[2]);
  Result.EY := MakeFPoint(AViewRec.Up[0], AViewRec.Up[1], AViewRec.Up[2]);
  Result.EZ := PtXScalar(Ort(Vector(Result.EY, Result.EX)), AViewRec.Scale);
  Result.EX := PtXScalar(Result.EX, AViewRec.Scale);
  Result.EY := PtXScalar(Result.EY, AViewRec.Scale);
  Result.E0.X := AViewRec.Offset[0];
  Result.E0.Y := AViewRec.Offset[1];
  Result.E0.Z := AViewRec.Offset[2];
end;

class function TsgView.MatrixToViewRec(const AMatrix: TFMatrix): TsgViewRec;
var
  vScale: Extended;

  procedure SetPoint(const APoint: TFPoint; var P: array of Single);
  var
    I: Integer;
  begin
    for I := Low(APoint.V) to High(APoint.V) do
      P[I] := APoint.V[I]
  end;

begin
  vScale := Abs(AffineMatrixDeterminant(AMatrix.Affine));
  if vScale > fMaxResolution then
    vScale := Power(vScale, 1.0/3.0)
  else
    vScale := vScale * 10;
  Result.Scale := vScale;
  vScale := 1/vScale;
  SetPoint(Ort(PtXScalar(AMatrix.EY, vScale)), Result.Up);
  SetPoint(Ort(PtXScalar(AMatrix.EX, vScale)), Result.Right);
  Result.Offset[0] := AMatrix.E0.V[0];
  Result.Offset[1] := AMatrix.E0.V[1];
  Result.Offset[2] := AMatrix.E0.V[2];
end;
{$ENDIF}

procedure TsgMeasureUnitsProps.ApplyParamsDefault;
begin
  FScaleFactor := FDefaults.ScaleFactor;
  FDisplayedUnits := FDefaults.DisplayedUnits;
  FInitialUnits := FDefaults.InitialUnits;
  FPrecisionFactor := FDefaults.PrecisionFactor;
end;

procedure TsgMeasureUnitsProps.Assign(ASource: TsgMeasureUnitsProps);
begin
  FDefaults := ASource.FDefaults;
  FScaleFactor := ASource.FScaleFactor;
  FDisplayedUnits := ASource.FDisplayedUnits;
  FInitialUnits := ASource.FInitialUnits;
  FPrecisionFactor := ASource.FPrecisionFactor;
  FFlags := ASource.FFlags;
end;

constructor TsgMeasureUnitsProps.Create;
begin
  FIsUserSet := False;
  FDefaults.DisplayedUnits := iuUnitless;
  FDefaults.InitialUnits := iuUnitless;
  FDefaults.ScaleFactor := 1;
  FDefaults.PrecisionFactor := cnstPrecisionFactorGlobal;
  ApplyParamsDefault;
end;

constructor TsgMeasureUnitsProps.CreateWithParams(ADisplayedUnits,
  AInitialUnits: TsgInsUnits; AScaleFactor: Double; APrecision: Integer);
begin
  Create;
  FDisplayedUnits := ADisplayedUnits;
  FInitialUnits := AInitialUnits;
  FScaleFactor := AScaleFactor;
  FPrecisionFactor := APrecision;
end;

destructor TsgMeasureUnitsProps.Destroy;
begin
  inherited;
end;

function TsgMeasureUnitsProps.IsEqual(
  ACompareObj: TsgMeasureUnitsProps): Boolean;
begin
  Result := False;
  if Assigned(ACompareObj) then
  begin
    Result :=
      (FPrecisionFactor = ACompareObj.FPrecisionFactor) and
      (FDisplayedUnits = ACompareObj.FDisplayedUnits) and
      (FInitialUnits = ACompareObj.FInitialUnits) and
      (FScaleFactor = ACompareObj.FScaleFactor);
  end;
end;

procedure TsgMeasureUnitsProps.SetProps(const ADisplayedUnits, AInitialUnits: TsgInsUnits;
  const AScaleFactor: Double; const APrecisionFactor: Integer);
begin
  FDefaults.DisplayedUnits := ADisplayedUnits;
  FDefaults.InitialUnits := AInitialUnits;
  FDefaults.ScaleFactor := AScaleFactor;
  FDefaults.PrecisionFactor := APrecisionFactor;
  ApplyParamsDefault;
end;

procedure TsgMeasureUnitsProps.SetDisplayedUnits(const Value: TsgInsUnits);
begin
  FDisplayedUnits := Value;
end;

procedure TsgMeasureUnitsProps.SetFlags(const Value: Integer);
begin
  FFlags := Value;
end;

procedure TsgMeasureUnitsProps.SetInitialUnits(const Value: TsgInsUnits);
begin
  FInitialUnits := Value;
end;

procedure TsgMeasureUnitsProps.SetIsUserSet(const Value: Boolean);
begin
  FIsUserSet := Value;
end;

procedure TsgMeasureUnitsProps.SetPrecisionFactor(const Value: Integer);
begin
  FPrecisionFactor := Value;
end;

procedure TsgMeasureUnitsProps.SetScaleFactor(const Value: Double);
begin
  FScaleFactor := Value;
end;

function getosid: string;
{$IFDEF LINUX}
var
  vLinuxOSInfoFile: RawByteString;
  vLinuxOSInfo: TStringList;
  S: THandleStream;
  vOSId: string;
{$ENDIF}
begin
  if sgOSId = '' then
{$IFDEF LINUX}
    //if FileGetSymLinkTarget('/etc/os_release', vLinuxOSInfoFile) then
    begin
      vLinuxOSInfoFile := '/etc/lsb-release';
      S := THandleStream.Create(FileOpen(vLinuxOSInfoFile, fmOpenRead or fmShareDenyWrite));
      try
        if S.Handle <> System.THandle(INVALID_HANDLE_VALUE) then
        try
          vLinuxOSInfo := TStringList.Create;
          try
            vLinuxOSInfo.LoadFromStream(S);
            vOSId := vLinuxOSInfo.Values['DISTRIB_ID'];
            if vOSId <> '' then
              sgOSId := vOSId;
          finally
            vLinuxOSInfo.Free;
          end;
        finally
          FileClose(S.Handle);
        end;
      finally
        S.Free;
      end;
    end;
  if sgOSId = '' then
    sgOSId := 'linux';
{$ELSE}
    sgOSId :=
      {$IFDEF ANDROID}'android'{$ELSE}
      {$IFDEF MACOS}'macos'{$ELSE}
      {$IFDEF IOS}'ios'{$ELSE}
      {$IFDEF MSWINDOWS}'windows'{$ELSE}
      {$IFDEF LINUX}'linux'{$ELSE}''{$ENDIF}
      {$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF};
{$ENDIF}
  Result := sgOSId;
end;

initialization
{$IFDEF MSWINDOWS}
  {$IFDEF DEBUG}
  //GetVersionWindows;
  {$ENDIF}
{$ENDIF}
  SetLength(Props, 4);
  GetGlobalPropProvider := {$IFNDEF FPC_OBJFPC}@{$ENDIF}_GetGlobalPropProvider;
{$IFDEF SG_USE_WEB_LAZ}
{$IFNDEF MSWINDOWS}
  DLLVersions[1] := '.1.1';
{$ENDIF}
{$ENDIF}
{$IFDEF SG_OPENING_IN_THEADS}
  GlobalCriticalSection := TCriticalSection.Create;
{$ENDIF}
  PrinterPapers := nil;
  InitProc;
//  PointClassifyTest(10, 0, 5, 0, 20, 0, -1);
//  PointClassifyTest(0, 10, 0, 5, 0, 20, -1);
//  PointClassifyTest(5, 5, 1, 1, 21, 21, -1);

finalization
  FreeObjectStringList(LanguageIds);
  FreeByteBinToHex;
  FreePrinterPapers;
{$IFDEF SG_OPENING_IN_THEADS}
  FreeAndNil(GlobalCriticalSection);
{$ENDIF}
  FinalizeProps;

end.

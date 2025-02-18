{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{             TsgParser class for XML parsing                }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgXMLParser;
{$INCLUDE SGDXF.inc}

//{$DEFINE SG_PARSER_USE_RTTI}
{$DEFINE SG_USE_DICTIONARY_BY_NAME}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$DEFINE SG_XML_DATA_TYPE}

{$IFDEF SGDEL_7}
  {$DEFINE SG_XML_USE_TYPE_VARIANT}
{$ENDIF}

//{$DEFINE SG_XML_DEBUG}

//{$IFDEF SG_CPUX64}
  {$DEFINE SG_DATA64}
//{$ENDIF}
//  {$DEFINE SG_EXPORT_WITH_STRINGS}

interface

uses
{$IFDEF SG_FIREMONKEY}
  sgFMXTypes, FMX.Graphics, System.NetEncoding, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
  {$ENDIF}
  {$IFDEF SGFPC}
  LCLIntf, LCLType,
  {$ENDIF}
  SysUtils, Classes, sgFunction, sgConsts, sgUnicode, sgLists, SyncObjs
  {$IFDEF SGFPC},FPimage {$ENDIF}
  {$IFDEF SGDEL_2009}, Character, Generics.Collections{$ENDIF}
  {$IFDEF SG_PARSER_DEBUG}, ComCtrls{$ENDIF}
  {$IFDEF SG_PARSER_USE_RTTI}, TypInfo{$ENDIF}
  {$IFDEF SGDEL_XE2}
  ,System.Types, System.UITypes
  {$ELSE}
  {$IFDEF SGDEL_6}
  , Types
  {$ENDIF}
  {$ENDIF}
  {$IFDEF SG_XML_USE_TYPE_VARIANT}
{$IFDEF SGDEL_6}
  ,Variants
{$ENDIF}
{$IFNDEF SGDEL_6}
  ,ActiveX
{$ENDIF}
  {$ENDIF}
  ;

const
  cnstDefCodePage = CP_UTF8;
  cnstEncoding = 'encoding';
  cnstVersion = 'version';
  cnstStandalone = 'standalone';
  cnstLineBreak = ',';
  cnstLineBreakDef = ' ';
  cnstLineBreakStyle = ';';
  cnstLineBreakText = #1;
  cnstPercentUpdateProgress: Integer = 5;
  cnstSymbolByHandle = #2;
  cnstXML = 'xml';
  cnstTrue = 'true';
  cnstFalse = 'false';
  cnstNodeValue = 'NodeValue';
  cnstASCIPrefix = '&#';
  cnstXMLPathDelimiter = '/';

  cnstBoolValues: array[Boolean] of string = ('False', 'True');

var
  cnstExportXmlWeb: Boolean = True;

type
  PString = ^string;
  TsgPointer = {$IFDEF SG_CPUX64}UInt64{$ELSE}Cardinal{$ENDIF};

  TsgProgressEvent = procedure(Stage: TProgressStage; Done,
    Count: Integer) of object;

  TsgNodeType = (ntUndefined, ntElement, ntAttribute, ntText, ntCData,
    ntEntityRef, ntEntity, ntProcessingInstr, ntComment, ntDocument,
    ntDocType, ntDocFragment, ntNotation);

  TsgNodeTextMode = (tmValue, tmText, tmFullText);

  TsgTagID = (idUndefined, idCX, idCY, idR, idRX, idRY, idStyle, idX1, idY1,
    idX2, idY2, idX, idY, idDX, idDY, idPoints, idWidth, idHeight, idD,
    idPathLength, idID, idHref, idTransform, idViewBox, idPreserveAspectRatio,
    idClass, idClipPath, idStroke, idStrokeWidth, idStrokeLineCap,
    idStrokeLineJoin, idStrokeMiterlimit, idStrokeDashArray, idStrokeDashOffset,
    idStrokeOpacity, idVectorEffect, idFill, idFillOpacity, idFillRule,
    idGradientUnits, idGradientTransform, idSpreadMethod, idFX, idFY,
    idStopColor, idStopOpacity, idOffset, idPatternUnits,idPatternContentUnits,
    idFont, idFontFamily, idFontStyle, idFontWeight, idFontSize, idFontVariant,
    idFontStretch, idTextAnchor, idTextDecoration, idDisplay, idVisibility,
    idOpacity);

  TsgTagIDs = set of TsgTagID;

  TsgUnitsOfMeasurements = (umUndefined, umPR, umPX, umMM, umCM, umM, umIN,
    umPT, umPC, umEM);

{$IFDEF SG_XML_DATA_TYPE}
  TsgDataValue = packed record
    DType: TsgDataType;
    case TsgDataType of
      dtUndefined: (Value: Pointer);
      dtBool:      (AsBool: Boolean);
      dtByte:      (AsByte: Byte);
      dtWord:      (AsWord: Word);
      dtInteger:   (AsInteger: Integer);
      dtColor:     (AsColor: {$IFDEF SGDEL_XE2}System.UITypes.{$ENDIF}TColor);
      dtSingle:    (AsSingle: Single);
      dtPointer:   (AsPointer: Pointer);
      dtString:    (AsString: PString);
      dtTF2DPoint: (AsF2DPoint: PF2DPoint);
      dtTFPoint:   (AsFPoint: PFPoint);
      dtRect:      (AsRect: PRect);
      dtTF2DRect:  (AsF2DRect: PF2DRect);
      dtTFRect:    (AsFRect: PFRect);
      dtVersion:   (AsVersion: PsgVersion);
      dtColorCAD:  (AsColorCAD: PsgColorCAD);
{$IFDEF SG_DATA64}
      dtHandle:    (AsHandle: UInt64);
      dtInt64:     (AsInt64: Int64);
      dtDouble:    (AsDouble: Double);
      dtPoint:     (AsPoint: TPoint);
{$ELSE}
      dtHandle:    (AsHandle: PUInt64);
      dtInt64:     (AsInt64: PInt64);
      dtDouble:    (AsDouble: PDouble);
      dtPoint:     (AsPoint: PPoint);
{$ENDIF}
  end;
  {$NODEFINE TsgDataValue}

(*$HPPEMIT 'namespace Sgxmlparser'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '  #pragma pack(push,1)'*)
(*$HPPEMIT '  struct TsgDataValue '*)
(*$HPPEMIT '  {'*)
(*$HPPEMIT ' '*)
(*$HPPEMIT '  public:'*)
(*$HPPEMIT '  	Sgconsts::TsgDataType DType;'*)
(*$HPPEMIT '  	union'*)
(*$HPPEMIT '  	{'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			double AsDouble;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			__int64 AsInt64;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			__int64 AsHandle;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TsgColorCAD *AsColorCAD;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TsgVersion *AsVersion;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TFRect *AsFRect;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TF2DRect *AsF2DRect;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '#ifdef __BORLANDC__'*)
(*$HPPEMIT '	#if (__BORLANDC__ >= 0x0640)'*)
(*$HPPEMIT '  			System::Types::TRect *AsRect;'*)
(*$HPPEMIT '	#else'*)
(*$HPPEMIT '  			Types::TRect *AsRect;'*)
(*$HPPEMIT '	#endif'*)
(*$HPPEMIT '#else'*)
(*$HPPEMIT '  			TRect *AsRect;'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TFPoint *AsFPoint;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Sgconsts::TF2DPoint *AsF2DPoint;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			AnsiString *AsString;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			void *AsPointer;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			float AsSingle;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '#ifdef __BORLANDC__'*)
(*$HPPEMIT '	#if (__BORLANDC__ >= 0x0640)'*)
(*$HPPEMIT '  			System::Uitypes::TColor AsColor;'*)
(*$HPPEMIT '	#else'*)
(*$HPPEMIT '  			Graphics::TColor AsColor;'*)
(*$HPPEMIT '	#endif'*)
(*$HPPEMIT '#else'*)
(*$HPPEMIT '  			TColor AsColor;'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			int AsInteger;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Word AsWord;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			Byte AsByte;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			bool AsBool;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '  			void *Value;'*)
(*$HPPEMIT '  		};'*)
(*$HPPEMIT '  		struct'*)
(*$HPPEMIT '  		{'*)
(*$HPPEMIT '#ifdef __BORLANDC__'*)
(*$HPPEMIT '	#if (__BORLANDC__ >= 0x0640)'*)
(*$HPPEMIT '  			System::Types::TPoint *AsPoint;'*)
(*$HPPEMIT '	#else'*)
(*$HPPEMIT '  			Types::TPoint *AsPoint;'*)
(*$HPPEMIT '	#endif'*)
(*$HPPEMIT '#else'*)
(*$HPPEMIT '  			TPoint *AsPoint;'*)
(*$HPPEMIT '#endif'*)
(*$HPPEMIT '  		}*SGDATAVALUE;'*)
(*$HPPEMIT '  	}; '*)
(*$HPPEMIT '  } ;'*)
(*$HPPEMIT '  #pragma pack(pop)'*)
(*$HPPEMIT '}'*)
{$ENDIF}

  TsgNodeSample = class;
  TsgParser = class;

  TsgData = class
  private
{$IFDEF SG_XML_DATA_TYPE}
    FValue: TsgDataValue;
{$ELSE}
    FValue: string;
{$ENDIF}
    procedure ClearValue;
    function GetMeasurements: TsgUnitsOfMeasurements;
    function GetValueAsBool: Boolean;
    function GetValueAsByte: Byte;
    function GetValueAsColor: TColor;
    function GetValueAsColorCAD: TsgColorCAD;
    function GetValueAsDouble: Double;
    function GetValueAsFPoint: TFPoint;
    function GetValueAsF2DPoint: TF2DPoint;
    function GetValueAsFRect: TFRect;
    function GetValueAsF2DRect: TF2DRect;
    function GetValueAsHandle: UInt64;
    function GetValueAsInt: Integer;
    function GetValueAsInt64: Int64;
    function GetValueAsPoint: TPoint;
    function GetValueAsPointer: Pointer;
    function GetValueAsRect: TRect;
    function GetValueAsVersion: TsgVersion;
    function GetValueAsWord: Word;
    function GetValueAsSingle: Single;
    function GetValueAsStr: string;
    function GetValueAsText: string;
    function GetValueAsGuid: TGUID;
    function GetValueAsMatrix: TFMatrix;
    procedure SetValueAsBool(const AValue: Boolean);
    procedure SetValueAsByte(const AValue: Byte);
    procedure SetValueAsDouble(const AValue: Double);
    procedure SetValueAsFPoint(const AValue: TFPoint);
    procedure SetValueAsF2DPoint(const AValue: TF2DPoint);
    procedure SetValueAsFRect(const AValue: TFRect);
    procedure SetValueAsF2DRect(const AValue: TF2DRect);
    procedure SetValueAsHandle(const AValue: UInt64);
    procedure SetValueAsInt(const AValue: Integer);
    procedure SetValueAsInt64(const AValue: Int64);
    procedure SetValueAsPoint(const AValue: TPoint);
    procedure SetValueAsPointer(const AValue: Pointer);
    procedure SetValueAsColor(const AValue: TColor);
    procedure SetValueAsColorCAD(const AValue: TsgColorCAD);
    procedure SetValueAsVersion(const AValue: TsgVersion);
    procedure SetValueAsSingle(const AValue: Single);
    procedure SetValueAsStr(const AValue: string);
    procedure SetValueAsText(const AValue: string);
    procedure SetValueAsGuid(const AValue: TGUID);
    procedure SetValueAsRect(const AValue: TRect);
    procedure SetValueAsWord(const AValue: Word);
    procedure SetValueAsMatrix(const Value: TFMatrix);
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    function  GetValueAsVariant: Variant;
    procedure SetValueAsVariant(const AValue: Variant);
{$ENDIF}
{$IFDEF SG_XML_DATA_TYPE}
    function GetValueType: TsgDataType;
    procedure NewValueWithType(const ADataType: TsgDataType);
    procedure SetValueWithType(const ADataType: TsgDataType; const AValue: Pointer);
{$ENDIF}
  protected
    class function GetAsBool(const AData: TsgData): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsByte(const AData: TsgData): Byte;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsDouble(const AData: TsgData): Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsF2DPoint(const AData: TsgData): TF2DPoint;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsFPoint(const AData: TsgData): TFPoint;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsFRect(const AData: TsgData): TFRect;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsF2DRect(const AData: TsgData): TF2DRect;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsHandle(const AData: TsgData): UInt64;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsInt(const AData: TsgData): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsInt64(const AData: TsgData): Int64;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsPointer(const AData: TsgData): Pointer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsStr(const AData: TsgData): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsText(const AData: TsgData): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetAsGuid(const AData: TsgData): TGUID;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    class function GetAsVariant(const AData: TsgData): Variant;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
    class procedure SetAsBool(const AData: TsgData; const AValue: Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsByte(const AData: TsgData; const AValue: Byte);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsDouble(const AData: TsgData; const AValue: Double);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsF2DPoint(const AData: TsgData; const AValue: TF2DPoint);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsFPoint(const AData: TsgData; const AValue: TFPoint);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsFRect(const AData: TsgData; const AValue: TFRect);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsF2DRect(const AData: TsgData; const AValue: TF2DRect);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsHandle(const AData: TsgData; const AValue: UInt64);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsInt(const AData: TsgData; const AValue: Integer);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsInt64(const AData: TsgData; const AValue: Int64);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsPointer(const AData: TsgData; const AValue: Pointer);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsStr(const AData: TsgData; const AValue: string);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsText(const AData: TsgData; const AValue: string);{$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure SetAsGuid(const AData: TsgData; const AValue: TGUID);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF SG_XML_USE_TYPE_VARIANT}
   class procedure SetAsVariant(const AData: TsgData; const AValue: Variant);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
    class procedure DoError(const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AData: TsgData);
    function Compare(const AData: TsgData; const ACaseSensitivity: Boolean = True): Integer;
    function IsEqual(const AData: TsgData; const ACaseSensitivity: Boolean = True): Boolean;
    function HasValue:Boolean;
    function SetValueTyped(const AType: TsgDataType; const AValue: Pointer): Boolean;
    property Measurements: TsgUnitsOfMeasurements read GetMeasurements;
{$IFDEF SG_XML_DATA_TYPE}
    property Value: TsgDataValue read FValue;
    property ValueType: TsgDataType read GetValueType;
{$ELSE}
    property Value: string read FValue write FValue;
{$ENDIF}
    property ValueAsBool: Boolean read GetValueAsBool write SetValueAsBool;
    property ValueAsByte: Byte read GetValueAsByte write SetValueAsByte;
    property ValueAsColor: TColor read GetValueAsColor write SetValueAsColor;
    property ValueAsColorCAD: TsgColorCAD read GetValueAsColorCAD write SetValueAsColorCAD;
    property ValueAsDouble: Double read GetValueAsDouble write SetValueAsDouble;
    property ValueAsInt: Integer read GetValueAsInt write SetValueAsInt;
    property ValueAsInt64: Int64 read GetValueAsInt64 write SetValueAsInt64;
    property ValueAsHandle: UInt64 read GetValueAsHandle write SetValueAsHandle;
    property ValueAsFPoint: TFPoint read GetValueAsFPoint write SetValueAsFPoint;
    property ValueAsF2DPoint: TF2DPoint read GetValueAsF2DPoint write SetValueAsF2DPoint;
    property ValueAsFRect: TFRect read GetValueAsFRect write SetValueAsFRect;
    property ValueAsF2DRect: TF2DRect read GetValueAsF2DRect write SetValueAsF2DRect;
    property ValueAsPoint: TPoint read GetValueAsPoint write SetValueAsPoint;
    property ValueAsPointer: Pointer read GetValueAsPointer write SetValueAsPointer;
    property ValueAsRect: TRect read GetValueAsRect write SetValueAsRect;
    property ValueAsVersion: TsgVersion read GetValueAsVersion write SetValueAsVersion;
    property ValueAsSingle: Single read GetValueAsSingle write SetValueAsSingle;
    property ValueAsStr: string read GetValueAsStr write SetValueAsStr;
    property ValueAsText: string read GetValueAsText write SetValueAsText;
    property ValueAsWord: Word read GetValueAsWord write SetValueAsWord;
    property ValueAsGuid: TGUID read GetValueAsGuid write SetValueAsGuid;
    property ValueAsMatrix: TFMatrix read GetValueAsMatrix write SetValueAsMatrix;
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    property ValueAsVariant: Variant read GetValueAsVariant write SetValueAsVariant;
{$ENDIF}
  end;

  TsgNodeList = class(TsgObjectList)//not use freelist and clearlist
  private
    FFlags: Byte;
    FSortedList: TStringList;
    procedure CreateSortedList;
    function GetCaseSensitivity: Boolean;
    function GetNode(const AIndex: Integer): TsgNodeSample;
    function GetNodeByName(const AName: string): TsgNodeSample;
    function GetQwickFind: Boolean;
    procedure SetCaseSensitivity(const AValue: Boolean);
    procedure SetQwickFind(const AValue: Boolean);
  protected
    procedure Notify(const Obj: TObject; Action: TListNotification); override;
    procedure UpdateNode(const ANode: TsgNodeSample);
  public
    class function CreateNodeList(const AQwickFind: Boolean = False): TsgNodeList;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TsgBaseList); override;
    procedure ClearNodes;
    procedure SortNodes(const AProc: TsgObjProcCompare);
    property CaseSensitivity: Boolean read GetCaseSensitivity
      write SetCaseSensitivity;
    property NodeByName[const AName: string]: TsgNodeSample read GetNodeByName; default;
    property Nodes[const AIndex: Integer]: TsgNodeSample read GetNode;
    property QwickFind: Boolean read GetQwickFind write SetQwickFind;
  end;

  TsgNodeCompareFunc = function (const ANode: TsgNodeSample;
    const ACompareChilds: Boolean = False): Integer of object;
  TsgNodeSampleClass = class of TsgNodeSample;

  TsgNodeSamplePosition = record
    X, Y: Integer;
  end;

  TsgNodeSample = class(TObject)
  private
    FPosition: TsgNodeSamplePosition;
    FOwner: TsgNodeSample;
    FEntity: TObject;
{$IFDEF SG_XML_DEBUG}
    FEntityClassName: string;
{$ENDIF}
    FUserData: Pointer;
    function GetCountOwner: Integer;
    function GetSpacesBySave: string;
    function GetText: string;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure SetText(const AText: string);{$IFDEF USE_INLINE} inline;{$ENDIF}
    function GetValue: string;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure SetValue(const AValue: string);{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure SetEntity(const Value: TObject);
{$IFNDEF SG_EXPORT_WITH_STRINGS}
    procedure SaveString(const AStream: TStream; const AString: string);
{$ENDIF}
  protected
    function CompareAttributes(const ANode: TsgNodeSample;
      const AIgnoreAttribs: TStrings): Integer;
    function CompareChilds(const ANode: TsgNodeSample;
      const AIgnoreAttribs: TStrings): Integer;
    function CheckText(const ANode: TsgNodeSample): Boolean; virtual;
    procedure ChangeName(const AValue: string); virtual;
    function GetAttributeNode(const AIndex: Integer): TsgNodeSample; virtual;
    function GetAttributeNodesCount: Integer; virtual;
    function GetChildNode(const AIndex: Integer): TsgNodeSample; virtual;
    function GetChildNodesCount: Integer; virtual;
    function GetClassID: Cardinal; virtual;
    function GetClosed: Boolean; virtual;
    function GetDataInternal(const AOwner: TObject): TObject; virtual;
    function GetPosition: TsgNodeSamplePosition;
    function GetFullName: string;
    function GetName: string; virtual;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
    function GetIndexName: Integer; virtual;
{$ENDIF}
    function GetNodeType: TsgNodeType; virtual;
    function GetOwnerList(const AType: TsgNodeType): TsgNodeList; virtual;
    function GetPostamble: Boolean; virtual;
    function GetPrefix: string; virtual;
    function GetSaveData(var AName, AValue, AText: string): Integer; virtual;
    function GetTextAsBool: Boolean;
    function GetTextAsDouble: Double;
    function GetTextAsFPoint: TFPoint;
    function GetTextAsFRect: TFRect;    
    function GetTextAsF2DRect: TF2DRect;
    function GetTextAsHandle: UInt64;
    function GetTextAsInt: Integer;
    function GetTextAsInt64: Int64;
    function GetTextAsPointer: Pointer;
    function GetTextAsStr: string;
    function GetTextAsText: string;
    function GetTextData: TsgData; virtual;
    function GetUnitsOfMeasurements: TsgUnitsOfMeasurements;
    function GetValueAsBool: Boolean;
    function GetValueAsDouble: Double;
    function GetValueAsF2DPoint: TF2DPoint;
    function GetValueAsFPoint: TFPoint;
    function GetValueAsFRect: TFRect;
    function GetValueAsF2DRect: TF2DRect;
    function GetValueAsHandle: UInt64;
    function GetValueAsInt: Integer;
    function GetValueAsInt64: Int64;
    function GetValueAsPointer: Pointer;
    function GetValueAsStr: string;
    function GetValueAsText: string;
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    function GetValueAsVariant: Variant;
    function GetTextAsVariant: Variant; virtual;
{$ENDIF}
    function GetValueData: TsgData; virtual;
    procedure Read(const AParser: TsgParser); virtual;
{$IFDEF SG_EXPORT_WITH_STRINGS}
    procedure SaveToStrings(const AStrings: TStrings); virtual;
{$ELSE}
    procedure SaveToStream(const AStream: TStream); virtual;
{$ENDIF}
    procedure SetClosed(const AValue: Boolean); virtual;
    procedure SetFullName(const AValue: string);
    procedure SetPosition(const AValue: TsgNodeSamplePosition);
    procedure SetName(const AValue: string); virtual;
    procedure SetNodeType(const AValue: TsgNodeType); virtual;
    procedure SetPostamble(const AValue: Boolean); virtual;
    procedure SetPrefix(const AValue: string); virtual;
    procedure SetReader(const AReader: Pointer); virtual;
    procedure SetTextAsBool(const AValue: Boolean);
    procedure SetTextAsDouble(const AValue: Double);
    procedure SetTextAsFPoint(const AValue: TFPoint);
    procedure SetTextAsFRect(const AValue: TFRect);
    procedure SetTextAsF2DRect(const AValue: TF2DRect);
    procedure SetTextAsHandle(const AValue: UInt64);
    procedure SetTextAsInt(const AValue: Integer);
    procedure SetTextAsInt64(const AValue: Int64);
    procedure SetTextAsPointer(const AValue: Pointer);
    procedure SetTextAsStr(const AValue: string);
    procedure SetTextAsText(const AValue: string);
    procedure SetValueAsBool(const AValue: Boolean);
    procedure SetValueAsDouble(const AValue: Double);
    procedure SetValueAsF2DPoint(const AValue: TF2DPoint);
    procedure SetValueAsFPoint(const AValue: TFPoint);
    procedure SetValueAsFRect(const AValue: TFRect);
    procedure SetValueAsF2DRect(const AValue: TF2DRect);
    procedure SetValueAsHandle(const AValue: UInt64);
    procedure SetValueAsInt(const AValue: Integer);
    procedure SetValueAsInt64(const AValue: Int64);
    procedure SetValueAsPointer(const AValue: Pointer);
    procedure SetValueAsStr(const AValue: string);
    procedure SetValueAsText(const AValue: string);
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    procedure SetValueAsVariant(const AValue: Variant);
    procedure SetTextAsVariant(const AValue: Variant); virtual;
{$ENDIF}
    property Closed: Boolean read GetClosed write SetClosed;
    property Entity: TObject read FEntity write SetEntity;
    property Postamble: Boolean read GetPostamble write SetPostamble;
    property TextAsText: string read GetTextAsText write SetTextAsText;
    property ValueAsText: string read GetValueAsText write SetValueAsText;
    property UserData: Pointer read FUserData write FUserData;
  public
    constructor Create; virtual;
    procedure Assign(const Obj: TObject); virtual;
    procedure Clear; virtual;
    function Compare(const ANode: TsgNodeSample;
      const ACompareChilds: Boolean = False): Integer; virtual;
    function CompareNodes(const ANode: TsgNodeSample;
      const ACompareChilds: Boolean; const AIgnoreAttribs: TStrings): Integer;
    class function SameNodeNames(const A, B: Pointer): Integer;
{$IFDEF SGDEL_2009}
    function GetAttributesEnumerable: IEnumerable<TsgNodeSample>; virtual;
    function GetChildsEnumerable: IEnumerable<TsgNodeSample>; virtual;
{$ENDIF}
    function GetAttributeByTagID(const ATag: TsgTagID): TsgNodeSample;
    function GetAttributeByName(const AName: string): TsgNodeSample; virtual;
    function GetChildByName(const AName: string): TsgNodeSample; virtual;
    function GetChildByPathName(const APathName: string;
      const ADelimiter: string = cnstXMLPathDelimiter): TsgNodeSample;
    function GetData(const AReader, AOwner: TObject): TObject;
    function HasAttribute(const AName: string): Boolean;
    function HasAttributeNodes: Boolean;
    function HasChildNodes: Boolean;
    function HasPosition: Boolean;
    function IsEqual(const ANode: TsgNodeSample;
      const ACompareChilds: Boolean = False): Boolean;
    property AttributeNodes[const AIndex: Integer]: TsgNodeSample
      read GetAttributeNode;
    property AttributeNodesCount: Integer read GetAttributeNodesCount;
    property ChildNodes[const AIndex: Integer]: TsgNodeSample read GetChildNode;
    property ChildNodesCount: Integer read GetChildNodesCount;
    property ClassID: Cardinal read GetClassID;
    property FullName: string read GetFullName write SetFullName;
    property Name: string read GetName write SetName;
    property NodeType: TsgNodeType read GetNodeType;
    property Owner: TsgNodeSample read FOwner;
    property Position: TsgNodeSamplePosition read GetPosition;
    property Prefix: string read GetPrefix write SetPrefix;
    property Text: string read GetText write SetText;
    property TextData: TsgData read GetTextData;
    property TextAsBool: Boolean read GetTextAsBool write SetTextAsBool;
    property TextAsDouble: Double read GetTextAsDouble write SetTextAsDouble;
    property TextAsInt: Integer read GetTextAsInt write SetTextAsInt;
    property TextAsInt64: Int64 read GetTextAsInt64 write SetTextAsInt64;
    property TextAsHandle: UInt64 read GetTextAsHandle write SetTextAsHandle;
    property TextAsFPoint: TFPoint read GetTextAsFPoint write SetTextAsFPoint;
    property TextAsFRect: TFRect read GetTextAsFRect write SetTextAsFRect;
    property TextAsF2DRect: TF2DRect read GetTextAsF2DRect write SetTextAsF2DRect;        
    property TextAsStr: string read GetTextAsStr write SetTextAsStr;
    property TextAsPointer: Pointer read GetTextAsPointer write SetTextAsPointer;
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    property TextAsVariant: Variant read GetTextAsVariant write SetTextAsVariant;
{$ENDIF}
    property UnitsOfMeasurements: TsgUnitsOfMeasurements
      read GetUnitsOfMeasurements;
    property Value: string read GetValue write SetValue;
    property ValueData: TsgData read GetValueData;
    property ValueAsBool: Boolean read GetValueAsBool write SetValueAsBool;
    property ValueAsDouble: Double read GetValueAsDouble write SetValueAsDouble;
    property ValueAsInt: Integer read GetValueAsInt write SetValueAsInt;
    property ValueAsInt64: Int64 read GetValueAsInt64 write SetValueAsInt64;
    property ValueAsHandle: UInt64 read GetValueAsHandle write SetValueAsHandle;
    property ValueAsF2DPoint: TF2DPoint read GetValueAsF2DPoint write SetValueAsF2DPoint;
    property ValueAsFPoint: TFPoint read GetValueAsFPoint write SetValueAsFPoint;
    property ValueAsFRect: TFRect read GetValueAsFRect write SetValueAsFRect;
    property ValueAsF2DRect: TF2DRect read GetValueAsF2DRect write SetValueAsF2DRect;            
    property ValueAsPointer: Pointer read GetValueAsPointer write SetValueAsPointer;
    property ValueAsStr: string read GetValueAsStr write SetValueAsStr;
{$IFDEF SG_XML_USE_TYPE_VARIANT}
    property ValueAsVariant: Variant read GetValueAsVariant write SetValueAsVariant;
{$ENDIF}
  end;

  TsgNodeComment = class(TsgNodeSample)
  private
    FValue: TsgData;
  protected
    function GetNodeType: TsgNodeType; override;
    function GetValueData: TsgData; override;
    procedure Read(const AParser: TsgParser); override;
{$IFDEF SG_EXPORT_WITH_STRINGS}
    procedure SaveToStrings(const AStrings: Tstrings); override;
{$ELSE}
    procedure SaveToStream(const AStream: TStream); override;
{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(const Obj: TObject); override;
  end;

  TsgNodeAttrib = class(TsgNodeComment)
  private
    FPrefix: string;
{$IFNDEF SG_USE_DICTIONARY_BY_NAME}
    FName: string;
{$ELSE}    
    FNameIndex: Integer;
    procedure SetFName(const AValue: string);
    function GetFName: string;
    property FName: string read GetFName write SetFName;
{$ENDIF}
  protected
    function GetName: string; override;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
    function GetIndexName: Integer; override;
{$ENDIF}
    function GetNodeType: TsgNodeType; override;
    function GetPrefix: string; override;
    procedure Read(const AParser: TsgParser); override;
    procedure ChangeName(const AValue: string); override;
    procedure SetPrefix(const AValue: string); override;
  public
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
    constructor Create; override;
{$ENDIF}
    procedure Assign(const Obj: TObject); override;
  end;

  TsgNode = class(TsgNodeAttrib)
  private
    FAttributes: TsgNodeList;
    FChilds: TsgNodeList;
    FFlags: Byte;
    FText: TsgData;
    FType: TsgNodeType;
  protected
    function GetAttributeNode(const AIndex: Integer): TsgNodeSample; override;
    function GetAttributeNodesCount: Integer; override;
    function GetChildNode(const AIndex: Integer): TsgNodeSample; override;
    function GetChildNodesCount: Integer; override;
    function GetClosed: Boolean; override;
    function GetNodeType: TsgNodeType; override;
    function GetOwnerList(const AType: TsgNodeType): TsgNodeList; override;
    function GetPostamble: Boolean; override;
    function GetSaveData(var AName, AValue, AText: string): Integer; override;
    function GetTextData: TsgData; override;
    function RemoveByName(const ANodes: TsgNodeList;
      const AName: string; const ADoFree: Boolean): Boolean;
    function RemoveNode(const ANodes: TsgNodeList;
      const ANode: TsgNodeSample): Boolean;
    procedure Read(const AParser: TsgParser); override;
    procedure ReadTextAndChildNodes(const AParser: TsgParser);
    procedure SetClosed(const AValue: Boolean); override;
    procedure SetNodeType(const AValue: TsgNodeType); override;
    procedure SetPostamble(const AValue: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
{$IFDEF SG_PARSER_USE_RTTI}
    procedure LoadFromObject(const AObj: TObject; AClass: TClass = nil);
{$ENDIF}
{$IFDEF SGDEL_2009}
    function GetAttributesEnumerable: IEnumerable<TsgNodeSample>; override;
    function GetChildsEnumerable: IEnumerable<TsgNodeSample>; override;
{$ENDIF}
    procedure AddAttrib(const ANode: TsgNodeSample);
    function AddAttribNV(const AFullName: string;
      const AValue: string = ''): TsgNodeAttrib;
    procedure AddChild(const ANode: TsgNodeSample;
      const AIndex: Integer = -1);
    function AddChildNV(const AFullName: string;
      const AValue: string = ''): TsgNode;
    function AddChildNVInt(const AFullName: string;
      const AValue: Integer): TsgNode;
    function AddChildNVDouble(const AFullName: string;
      const AValue: Double): TsgNode;
    function AddChildNVBool(const AFullName: string;
      const AValue: Boolean): TsgNode;
    function AddAttribs(const AAttribs: TStrings): Integer;
    procedure Assign(const Obj: TObject); override;
    procedure Clear; override;
    function GetAttributeByName(const AName: string): TsgNodeSample; override;
    function GetChildByName(const AName: string): TsgNodeSample; override;
    function GetNodeByAttribValue(const ANodeName, AName,
      AValue: string; const ACaseSensitivity: Boolean = True): TsgNodeSample;
    function RemoveAttrib(const ANode: TsgNodeSample): Boolean;
    function RemoveChild(const ANode: TsgNodeSample): Boolean;
    function RemoveAttributeByName(const AName: string;
      const ADoFree: Boolean = True): Boolean;
    function RemoveChildByName(const AName: string;
      const ADoFree: Boolean = True): Boolean;
    property Attributes: TsgNodeList read FAttributes;
    property Childs: TsgNodeList read FChilds;
    property NodeType: TsgNodeType read GetNodeType;
  end;  
  
  TsgNodeCDATA = class(TsgNodeComment)
  protected
    function GetNodeType: TsgNodeType; override;
    procedure Read(const AParser: TsgParser); override;
  end;

  TsgNodeDocument = class(TsgNode)
  protected
    function GetClosed: Boolean; override;
    function GetNodeType: TsgNodeType; override;
{$IFDEF SG_EXPORT_WITH_STRINGS}
    procedure SaveToStrings(const AStrings: TStrings); override;
{$ELSE}
    procedure SaveToStream(const AStream: TStream); override;
{$ENDIF}
  end;

  TsgNodeDocType = class(TsgNodeDocument)
  private
    FValues: TsgNodeList;
    procedure FreeValues;
  protected
    function GetName: string; override;
    function GetNodeType: TsgNodeType; override;
    procedure Read(const AParser: TsgParser); override;
    procedure ReadValues(const AParser: TsgParser);
    procedure ChangeName(const AValue: string); override;
  public
    destructor Destroy; override;
    procedure Assign(const Obj: TObject); override;
    property Values: TsgNodeList read FValues;
  end;

  TsgNodeEntity = class(TsgNodeAttrib)
  protected
    function GetName: string; override;
    function GetNodeType: TsgNodeType; override;
    procedure Read(const AParser: TsgParser); override;
    function ReadNodeName(const AParser: TsgParser): string;
    procedure ChangeName(const AValue: string); override;
  end;

  TsgNodeHTML = class(TsgNode)
  protected
    function GetClassID: Cardinal; override;
  end;

  TsgParser = class
  private
    FBuffer: string;
    FBufferIndex: Integer;
    FBufferReaded: Integer;
    FBufferSize: Integer;
    FBufferStep: Integer;
    FDebugNodePosition: TsgIntegerList;
    FDebugInfoMode: Integer;
    FOnProgress: TsgProgressEvent;
    FRegisterNodes: TsgStringList;
    FROOT: TsgNodeList;
    FSource: TStream;
    FSourceString: string;
    FStopped: TsgObjProcBool;
    FNodeTextMode: TsgNodeTextMode;
    procedure BeginUpdateProgress;
    procedure EndUpdateProgress;
    procedure CheckReadBufferError;
    procedure LoadDebugInfo;
    procedure UpdateProgress;
    function GetDebugInfo: Boolean;
    function GetXMLString: string;
    function InitFromStream: Boolean;
    function InitFromString: Boolean;
    procedure SetXMLString(const AValue: string);
    procedure SetDebugInfo(const AValue: Boolean);
  protected
    procedure Analize;
    procedure AfterLoadFromStream; virtual;
    procedure BeginAnalize;
    procedure BeforeLoadFromStream; virtual;
    function BufferCheckPos: Boolean;
    procedure BufferNext;
    function BufferValue: Char;
    function BufferValueToChar(const AChar: Char): string;
    function BufferValueToStr(const AStr: string): string;
    function CheckEncoding(var ADocument: TsgNodeDocument): Boolean;
    function CheckEntities(var ADocType: TsgNodeDocType): Boolean;
    function CheckState: Boolean;
    procedure ClearBuffer;
    function CreateNodeSample(const AFullName: string;
      AClass: TClass): TsgNodeSample;    
    function FindNodeByAttribValue(const ANode: TsgNodeSample;
      const AAttribName, AAttribValue: string; const AFoundNodes: TsgObjectList = nil;
      const AIgnoreValue: Boolean = False): TsgNodeSample;
    function LoadFromBuffer(const AInitBuffer: TsgObjProcBool): Boolean;
    function ReadAttrib: TsgNodeSample;
    function ReadCData: string;
    function ReadComment: string;
    function ReadDocType: string;
    function ReadNode: TsgNodeSample;
    function ReadNodeName(var ANodeType: TsgNodeType;
      var APostamble: Boolean): string;
    function ReadText: string;
    function ReadValue: string;
    procedure SetNodePosition(const ANode: TsgNodeSample);
  public
    constructor Create; virtual;
    destructor Destroy; override;
{$IFDEF SGDEL_2009}
    class function SaveEncoding: TEncoding; static;
{$ENDIF}
    procedure Assign(const AParser: TsgParser);
    function AddDeclaration(const AVersion: string = '1.0';
      AEncoding: string = ''): Boolean;
    procedure Clear;
    function CreateRootNode(const AFullName: string = cnstXML;
      const ACreateDocument: Boolean = False): TsgNode;
    function FindNode(const AID: string;
      const AList: TsgNodeList): TsgNodeSample;
    class function FindNodeByAttrib(const AAttribName, AAttribValue: string;
      const AList: TsgNodeList; AFoundNodes: TsgObjectList = nil;
      const AIgnoreValue: Boolean = False;
      const AFindCount: Integer = -1): TsgNodeSample;
    class function FindNodeByName(const ANodeName: string;
      const AList: TsgNodeList; AFoundNodes: TsgObjectList = nil;
      const AFindCount: Integer = -1): TsgNodeSample;
    function FindEqualNode(const ABase: TsgNodeSample; const ANode: TsgNodeSample;
      ANodeCompareFunc: TsgNodeCompareFunc = nil;
      const ATree: TsgObjectList = nil): TsgNodeSample;
    procedure ImportNodes(const AParser: TsgParser);
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(const S: TStream): Boolean;
    function LoadFromString(const AString: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function SaveToStream(const S: TStream): Boolean;
    function SaveToStrings(const AStrings: TStrings): Boolean;
    function SaveToString: string;
    procedure RegisterClass(const AFullName: string; const AClass: TClass);
    function Stopped: Boolean;
    procedure UnRegisterClass(const AFullName: string);
    property DebugInfo: Boolean read GetDebugInfo write SetDebugInfo;
    property DebugInfoMode: Integer read FDebugInfoMode write FDebugInfoMode;
    property OnProgress: TsgProgressEvent read FOnProgress write FOnProgress;
    property ROOT: TsgNodeList read FROOT;
    property StopRead: TsgObjProcBool read FStopped write FStopped;
    property NodeTextMode: TsgNodeTextMode read FNodeTextMode write FNodeTextMode;
    property XMLString: string read GetXMLString write SetXMLString;
  end;

  TsgFindNodeCustom = class
    function Compare(const ANode: TsgNodeSample): Boolean; virtual; abstract;
  end;

  TsgFindByAttribute = class(TsgFindNodeCustom)
    Name: string;
    Value: string;
    IgnoreValue: Boolean;
    CaseSensitivity: Boolean;
    constructor Create(const AName, AValue: string;
      const AIgnore: Boolean = False; const ACaseSensitivity: Boolean = True);
    function Compare(const ANode: TsgNodeSample): Boolean; override;
  end;

  TsgFindByNode = class(TsgFindNodeCustom)
    Name: string;
    CaseSensitivity: Boolean;
    constructor Create(const AName: string; const ACaseSensitivity: Boolean = True);
    function Compare(const ANode: TsgNodeSample): Boolean; override;
  end;

  TsgNodeIterator = class
  private
    FComparer: TsgFindNodeCustom;
    FRoot: TsgNodeSample;
    FRootNotes: TsgNodeList;
    FFindCount: Integer;
    FFindNodes: TsgObjectList;
    FFindTree: TsgObjectList;
    function BreakFind: Boolean;
    function GetFindNotAll: Boolean;
    procedure SetFindNotAll(const Value: Boolean);
  protected
    function Compare(const ANode: TsgNodeSample): Boolean;
    function FindChildNode(const ANode: TsgNodeSample): Boolean;
  public
    constructor Create(const ANode: TsgNodeSample;
      const AComparer: TsgFindNodeCustom); overload;
    constructor Create(const ANodes: TsgNodeList;
      const AComparer: TsgFindNodeCustom); overload;
    destructor Destroy; override;
    function Find(const AFindCount: Integer = -1): Integer;
    property FindNotAll: Boolean read GetFindNotAll write SetFindNotAll;
    property FindResult: TsgObjectList read FFindNodes;
    property FindTree: TsgObjectList read FFindTree;
  end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
  TsgNodeSampleFrame = class(TInterfacedObject, IsgNodeFrame)
  private
    FNode: TsgNode;
  public
    class function CreateFrame(const ANode: TsgNode): IsgNodeFrame;
    function AddChild(const NodeName: string): IsgNodeFrame;
    procedure ClearChildNodes;
    function ChildNodeCount: Integer;
    function FindChildNode(const NodeName: string): IsgNodeFrame;
    function GetAttribute(const AttrName: string): OleVariant;
    function GetChildNode(const AIndex: Integer): IsgNodeFrame;
    function GetChildValue(const IndexOrName: string): OleVariant;
    function GetName: string;
    procedure SetAttribute(const AttrName: string; const Value: OleVariant);
    procedure SetChildValue(const IndexOrName: string; const Value: OleVariant);
  end;
{$ENDIF}

  IsgResultNode = interface(IInterface)
    [cnstGUID_ResultNode]
    function GetResult: TsgNode;
    function GetOutput: TsgNode;
    function GetErrors: TsgNode;
    property Result: TsgNode read GetResult;
    property Output: TsgNode read GetOutput;
    property Errors: TsgNode read GetErrors;
  end;

  TsgEventNode = procedure(const ANode: TsgNodeSample) of object;

function IsNodeSample(const AValue: Pointer): Boolean;
//  Get and convert function
procedure CopyChildTextAsStr(const AElem: TsgNode;
  const ASourceNodeName, ADestNdoeName: string);

function GetXMLNodeTextAsInt(const ANode: TsgNodeSample;
  const ADefaultValue: Integer = 0): Integer;
function GetXMLNodeTextAsIntWithRang(const ANode: TsgNodeSample;
  const ALow, AHigh: Integer; const ADefaultValue: Integer = 0): Integer;
function GetXMLNodeTextAsBool(const ANode: TsgNodeSample;
  const ADefaultValue: Boolean = False): Boolean;
function GetXMLNodeTextAsDouble(const ANode: TsgNodeSample;
  const ADefaultValue: Double = 0): Double;
function GetXMLNodeTextAsString(const ANode: TsgNodeSample;
  const ADefaultValue: string = ''): string;
function GetXMLNodeTextAsFRect(const ANode: TsgNodeSample;
  const ADefaultValue: TFRect): TFRect;
function GetXMLNodeTextAsTFPoint(const ANode: TsgNodeSample;
  const ADefaultValue: TFPoint): TFPoint;
function GetXMLNodeTextAsTF2DPoint(const ANode: TsgNodeSample;
  const ADefaultValue: TF2DPoint): TF2DPoint;
function GetXMLNodeTextAsHanle(const ANode: TsgNodeSample;
  const ADefaultValue: Integer = 0): UInt64;

function SaveNodeToXMLString(const ANode: TsgNodeSample): string; overload;
function SaveNodeToXMLString(const ANode: TsgNodeSample;
  const ADefaultValue: string): string; overload;
//
function GetAttributeBool(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Boolean = False): Boolean;
function GetAttributeFloat(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Double = 0): Double;
function GetAttributeFPoint(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: TFPoint): TFPoint;
function GetAttributeInt(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Integer = 0): Integer;
function GetAttributeInt64(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Int64 = 0): Int64;
function GetAttributeInt64List(const AElem: TsgNodeSample;
  const AName: string; const AList: TsgInt64List;
  const ADecimalSeparatorNames: Char = ';'): Integer;
function GetAttributeStr(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: string = ''): string;
function GetAttributeHandle(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Uint64 = cnstBadHandle): Uint64;
function GetAttributeTxt(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: string = ''): string;
function GetAttributeColor(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Integer = 0): TColor;

function GetActAttrib(const AElem: TsgNode; const AName: string): TsgNodeSample;
function GetActChild(const AElem: TsgNode; const AName: string): TsgNodeSample;

function SetAttributeStr(const AElem: TsgNode; const AName: string;
  const AValue: string; const ACreateAttrib: Boolean = False): Boolean;
function SetAttributeInt(const AElem: TsgNode; const AName: string;
  const AValue: Integer; const ACreateAttrib: Boolean = False): Boolean;
function SetAttributeBool(const AElem: TsgNode; const AName: string;
  const AValue: Boolean; const ACreateAttrib: Boolean = False): Boolean;
function SetChildBool(const AElem: TsgNode; const AName: string;
  const AValue: Boolean): Boolean;
function SetChildFloat(const AElem: TsgNode; const AName: string;
  const AValue: Double): Boolean;
function SetChildInt(const AElem: TsgNode; const AName: string;
  const AValue: Integer): Boolean;
function SetChildStr(const AElem: TsgNode; const AName: string;
  const AValue: string): Boolean;
function SetAttributeTxt(const AElem: TsgNode; const AName: string;
  const AValue: string; const ACreateAttrib: Boolean = False): Boolean;
function SetAttributeColor(const AElem: TsgNode; const AName: string;
  const AValue: TColor; const ACreateAttrib: Boolean = False): Boolean;

procedure CheckStringParams(const AParams: TStrings);
procedure DecodeBase64(const AString: string; AStrings: TStrings;
  var AStream: TStream);
function StrDecodeBase64(const AString: string): string;
procedure EncodeBase64(const AInStream: TStream;
  var AOutStream: TStream);
function StrEncodeBase64(const AString: string {$IFDEF SGDEL_2009}; AEncoding: TEncoding = nil{$ENDIF}): string;
function GetCodePageByName(const AStr: string; var ACodePage: Integer): Boolean;
function GetCodePageName(const ACodePage: Integer): string;
{$IFDEF SGDEL_2009}
function GetEncodingCodePageName(const AEncoding: TEncoding): string;
{$ENDIF}
function GetFullText(const AStrings: TStringList; const APoint1: PFPoint = nil): string;
function GetHandleByNode(const ANode: TsgNodeSample): string;
function GetNameByTagId(const AId: TsgTagID): string;
function GetNameByCodePage(const ACodePage: Integer; var AName: string): Boolean;
function GetNodeByHandle(const AStr: string): TsgNodeSample;
function GetTagIdByName(const AName: string): TsgTagID;
function GetUnitsMeasurements(const AValue: string): TsgUnitsOfMeasurements;
function IsApostrophe(const AChar: Char): Boolean;
function IsHandleOfNode(const AStr: string): Boolean;
function HasHandleOfNode(const AStr: string): Boolean;
function IsLetter(const AChar: Char): Boolean;
function IsName(const AChar: Char): Boolean;
function IsNumber(const AChar: Char): Boolean;
function ReplaceAnsiCodes(const AString: string): string;
procedure ReplaceWebOrAnsiCode(const AToAnsi: Boolean; var AString: string; AMax: Integer = -1);
function ReplaceASCICode(const AStr: string; AUnicode: Boolean): string;
function ReplaceWebCodes(const AString: string): string;
function ScanDigitsInString(const AString: string; const ALength: Integer;
  const AIsReadExponent: Boolean; var AIndex: Integer): Boolean;
procedure SetStringsLineBreak(const AStrings: TsgStringList;
  const AText: string; ALineBreak: Char = cnstLineBreak);
procedure SetStringsText(const AStrings: TsgStringList; const AText: string);

function CheckStringDouble(const AString: string): string;
function CheckStringInteger(const AString: string): string;
function CheckStringFPoint(const AString: string): string;
function BoolToStr(const AValue: Boolean): string;
function FloatToString(const AValue: Double): string;
function FloatRoundToStr(const AValue: Double; const ADigits: Integer): string;
function FPointToStr(const AValue: TFPoint): string;
function FPointRoundToStr(const AValue: TFPoint; const ADigits: Integer): string;
function F2DPointRoundToStr(const AValue: TF2DPoint; const ADigits: Integer): string;
function HandleToStr(const AValue: UInt64): string;
function ValToStr(const ADouble: Double): string;
function sgColorHexToStr(const AColor: TColor): string;
function FRectToStr(const ARect: TFRect; const AIs3D: Boolean = True): string;
function FRectRoundToStr(const ARect: TFRect; const ADigits: Integer;
  const AIs3D: Boolean = True): string;
function F2DRectToStr(const ARect: TF2DRect): string;
function F2DRectRoundToStr(const ARect: TF2DRect; const ADigits: Integer): string;
function FMatrixToStr(const AM: TFMatrix; const AIs3D: Boolean = True): string;
function StrHexToInt(const AString: string): UInt64;
function StrToColor(const AString: string): Integer;
function sgStrToColorHex(const AString: string): TColor;
function StrToBool(const AString: string; var ABool: Boolean): Boolean;
function StringToFloat(const AString: string): Double;
function StrToFPoint(const AString: string): TFPoint;
function StrToFRect(const AString: string; const AIs3D: Boolean = True): TFRect;
function StrToF2DRect(const AString: string): TF2DRect;
function StrToHandle(const AString: string): UInt64;
function StrToVal(const AString: string): Double;
function TryStrToFPoint(const AString: string; P: PFPoint = nil): Boolean;

function StrToPointsList(const AString: string; const AIs3D: Boolean;
  const AList: Pointer; const AAddProc: TsgProcOfListAddPoint;
  const ASeparator: Char = cnstLineBreak): Integer;
function PointsListToStr(const AIs3D: Boolean; const AList: TObject;
  const ACount: Integer; const GetProc: TsgProcOfListGetPoint;
  const ASeparator: Char = cnstLineBreak): string; overload;
function PointsListToStr(const AIs3D: Boolean; AList: IsgArrayFPoint;
  const ASeparator: Char = cnstLineBreak): string; overload;
function StrToSingleList(const AString: string; const AList: Pointer;
  const AAddProc: TsgProcOfListAddSingle; const ASeparator: Char = cnstLineBreak): Integer;
function SingleListToStr(const AList: TObject; const ACount: Integer;
  const GetProc: TsgProcOfListGetSingle; const ASeparator: Char = cnstLineBreak): string;

function IsValidXML(const AString: string; var AMessage: string;
  const APoint: PPoint = nil): Boolean;

function DeleteAttributes(const ANode: TsgNode;
  const AAttribNames: TStrings; const AUseChild: Boolean): Integer;

{$IFDEF SG_PARSER_DEBUG}
function TestParser(const AFileName: string; const Tree: TTreeView): DWORD;
{$ENDIF}

function GetValueRound(const AData: TsgData;
  const ADigits: Integer = iRoundDigits): string;

procedure SetDataValueAsDouble(const AData: TsgData; const AValue: Double; const AViewMode: Boolean);
procedure SetDataValueAsFPoint(const AData: TsgData; const AValue: TFPoint; const AViewMode: Boolean);
procedure SetDataValueAsF2DPoint(const AData: TsgData; const AValue: TF2DPoint; const AViewMode: Boolean);
procedure SetDataValueAsFRect(const AData: TsgData; const AValue: TFRect; const AViewMode: Boolean);
procedure SetDataValueAsF2DRect(const AData: TsgData; const AValue: TF2DRect; const AViewMode: Boolean);

function IntToStrWithSymbolCount(const AValue: Integer;
  const ADigits: Integer = 0): string;
function StrToDayMonthYear(const AStr: string; const AError: PInteger = nil): TDateTime;
function DayMonthYearToStr(const AValue: TDateTime): string;

function GuidToStr(const AGuid: TGUID): string;
function StrToGUID(const AGuid: string): TGUID;
function IsGuidNull(const AGuid: TGUID): Boolean;

{$IFDEF SGDEL_7}
function IsNoInitVariant(const V: OleVariant): Boolean;
function VariantToDouble(const V: Variant; const ASeparator: Char = cnstPoint;
  const ADefault: Double = 0): Double;
{$ENDIF}

function sgCryptStr(AStr, AKey: String; const AsHex: Boolean): String;
function sgDeCryptStr(AStr, AKey: String; const AsHex: Boolean): String;

implementation

{$IFNDEF SG_NON_WIN_PLATFORM}
uses ActiveX;
{$ENDIF}

const
  cnstIncorrectType: string = 'Incorrect type value!';
  cnstDayMonthYearSeparator = '.';
  cnstColorHexPrefix = '#';

{$IFDEF SG_USE_DICTIONARY_BY_NAME}
const
  cnstDictionaryByNamesCaseSensitive = True;

type
 PsgSystemColor = ^{$IFDEF SGDEL_XE2}System.UITypes.{$ENDIF}TColor;

var
  DictionaryByNames: TsgStringList = nil;
  DictionaryByIndexes: TsgStringList = nil;
{$IFDEF SG_OPENING_IN_THEADS}
  DictionaryLock: SyncObjs.TCriticalSection;
{$ENDIF}

function IsValidXML(const AString: string; var AMessage: string;
  const APoint: PPoint = nil): Boolean;
{$IFDEF SGFPC}
begin
  Result := True;
end;
{$ELSE}
  {$IFDEF SG_WINDOWS}
const
  CLS_DOMDoc: TGUID = '{F6D90F11-9C73-11D3-B32E-00C04F990BB4}';
  CLS_DOMDoc26: TGUID = '{F5078F1B-C551-11D3-89B9-0000F81FE221}';
  CLS_DOMDoc30: TGUID = '{F5078F32-C551-11D3-89B9-0000F81FE221}';
  CLS_DOMDoc40: TGUID = '{88D969C0-F192-11D4-A65F-0040963251E5}';
  CLS_DOMDoc60: TGUID = '{88D96A05-F192-11D4-A65F-0040963251E5}';
var
  vDom: Variant;//IXMLDOMDocument;
  vError: Variant;//IXMLDOMParseError;

  function TryObjCreate(const ACLSIDs: array of TGuid; out Obj): HRESULT;
  var
    I: Integer;
  begin
    Pointer(Obj) := nil;
    Result := REGDB_E_CLASSNOTREG;
    I := Low(ACLSIDs);
    while (I <= High(ACLSIDs)) and not Succeeded(Result) do
    begin
      Result := CoCreateInstance(ACLSIDs[I], nil, CLSCTX_INPROC_SERVER or
        CLSCTX_LOCAL_SERVER, IDispatch, Obj);
      Inc(I);
    end;
  end;

begin
  AMessage := '';
  Result := True;
  TVarData(vDom).VType := varEmpty;
  if Succeeded(TryObjCreate([CLS_DOMDoc60, CLS_DOMDoc40,
    CLS_DOMDoc30, CLS_DOMDoc26, CLS_DOMDoc], TVarData(vDom).VDispatch)) then
  try
    if TVarData(vDom).VDispatch <> nil then
    begin
      TVarData(vDom).VType := varDispatch;
      vDOM.loadXML(AString);
      try
        vError := vDom.parseError;
        if vError.errorCode <> 0 then
        begin
          Result := False;
          AMessage := vError.reason;
          if APoint <> nil then
          begin
            APoint^.X := vError.linepos;
            APoint^.Y := vError.line;
          end;
        end;
      finally
        VarClear(vError);
      end;
    end;
  finally
    VarClear(vDOM);
  end;
  {$ELSE}
  begin
    Result := True;
  {$ENDIF}
end;
{$ENDIF}

function sgCryptStr(AStr, AKey: String; const AsHex: Boolean): String;
var
  I, V: Integer;
  vStr: string;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    V := (Ord(AStr[I]) + (Ord(AKey[(Pred(I) mod Length(AKey)) + 1]) - Ord('0')));
    if V >= 256 then
      Dec(V, 256);
    AStr[I] := Chr(V);
    Result := AStr;
  end;
  if AsHex and (Length(Result) > 0) then
  begin
    vStr := '';
    V := SizeOf(Char) * 2;
    for I := 1 to Length(Result) do
      vStr := vStr + IntToHex(Integer(Result[I]), V);
    Result := vStr;
  end;
end;

function sgDeCryptStr(AStr, AKey: String; const AsHex: Boolean): String;
var
  I, L, V: Integer;
  vStr, vChar: string;
begin
  Result := '';
  if AsHex and (Length(AStr) > 0) then
  begin
    V := SizeOf(Char) * 2;
    L := (Length(AStr) div V) * V;
    if L > 0 then
    begin
      vStr := '';
      I := 1;
      while I < L do
      begin
        vChar := '$' + Copy(AStr, I, V);
        Inc(I, V);
        vStr := vStr + Char(StrToInt(vChar));
      end;
      AStr := vStr;
    end
    else
      AStr := '';
  end;
  for I := 1 to Length(AStr) do
  begin
    V := (Ord(AStr[I]) - (Ord(AKey[(Pred(I) mod Length(AKey)) + 1]) - Ord('0')));
    if V < 0 then
      Inc(V, 256);
    AStr[I] := Chr(V);
    Result := AStr;
  end;
end;

function sgDeleteEmptyStrings(const AStrings: TStrings): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.DeleteEmptyStrings(AStrings);
end;

function sgStrToDouble(const AStr: string;
  const Separator: Char = cnstDoubleSeparator):  Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StrToDouble(AStr, Separator);
end;

function sgStringScan(Chr: Char; const AStr: string; AStart: Integer;
  const AReverse: Boolean = False): Integer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StringScan(Chr, AStr, AStart, AReverse);
end;

function sgStringPos(const ASubStr, AStr: string;
  const AStart: Integer): Integer; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StringPos(ASubStr, AStr, AStart);
end;

function sgDoubleToStr(const AVal: Double;
  const Separator: Char = cnstDoubleSeparator): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.DoubleToStr(AVal, Separator);
end;

function sgRoundToDouble(const AValue: Double;
  const ADigit: Integer): Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.sgRoundTo(AValue, ADigit);
end;

procedure sgStrToStrings(const AStr: string; const ADelimiter: string;
  const AStrings: TStrings);{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  sgFunction.StrToStrings(AStr, ADelimiter, AStrings);
end;

function sgReplaceAnsi(var AText: string; const AFromText, AToText: string;
  const ACaseSensitive: Boolean = False): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.ReplaceAnsi(AText, AFromText, AToText, ACaseSensitive);
end;

function sgAnsiCmprStr(const AStr1, AStr2: string;
  const ACaseSensitivity: Boolean): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.AnsiCmprStr(AStr1, AStr2, ACaseSensitivity);
end;

function sgStringReplace(var Str: string; const Old, New: string): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StringReplace(Str, Old, New);
end;

function sgSwapWords(const AValue: DWORD): DWORD;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.SwapWords(AValue);
end;

function sgStrToColorCAD(const AStr: string): TsgColorCAD;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StrToColorCAD(AStr);
end;

function sgColorCADToStr(const AColor: TsgColorCAD): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.ColorCADToStr(AColor);
end;

function sgStrToColorHex(const AString: string): TColor;//{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.StrToColorRGB(AString, cnstColorHexPrefix);
end;

function sgColorHexToStr(const AColor: TColor): string;//{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := sgFunction.ColorRGBToStr(AColor, cnstColorHexPrefix);
end;

procedure CreateDictionaryNames;
begin
  DictionaryByNames := TsgStringList.Create;
  DictionaryByNames.Sorted := True;
  DictionaryByNames.Duplicates := dupIgnore;
  DictionaryByNames.CaseSensitive := cnstDictionaryByNamesCaseSensitive;
  DictionaryByIndexes := TsgStringList.Create;
  DictionaryByIndexes.Sorted := False;
end;

function GetNameIndex(const AName: string; const AddNew: Boolean): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vIndex: Integer;
begin
{$IFNDEF SG_OPENING_IN_THEADS}
  if DictionaryByNames = nil then
    CreateDictionaryNames;
{$ENDIF}
  vIndex := DictionaryByNames.IndexOf(AName);
  if vIndex > -1 then
    Result := TsgObjectInt64(DictionaryByNames.Objects[vIndex]).FieldInt
  else
  begin
    if AddNew then
    begin
{$IFDEF SG_OPENING_IN_THEADS}
      DictionaryLock.Enter;
      try
{$ENDIF}
      DictionaryByIndexes.Add(AName);
      Result := DictionaryByIndexes.Count;
      DictionaryByNames.AddObject(AName, TsgObjectInt64.CreateInt(Result));
{$IFDEF SG_OPENING_IN_THEADS}
      finally
        DictionaryLock.Leave;
      end;
{$ENDIF}
    end
    else
      Result := -1;
  end;
end;

function GetNameByIndex(const AIndex: Integer): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := '';
  if (DictionaryByIndexes <> nil) and (AIndex  > 0) then
    Result := DictionaryByIndexes[AIndex - 1];
end;

function GetNameIndexesCaseSensitivity: Boolean;
begin
  Result := cnstDictionaryByNamesCaseSensitive;
  if Assigned(DictionaryByNames) then
    Result := DictionaryByNames.CaseSensitive;
end;
{$ENDIF}

{$IFDEF SG_PARSER_USE_RTTI}
type
  TsgRecordType = (tkrUndefined, tkrTFPoint);

function IsField(P: Pointer): Boolean; inline;
{$IFDEF CPUX86}
begin
  Result := (IntPtr(P) and $FF000000) = $FF000000;
end;
{$ENDIF}
{$IFDEF CPUX64}
begin
  Result := (IntPtr(P) and $FF00000000000000) = $FF00000000000000;
end;
{$ENDIF}

function GetField(Instance: TObject; P: Pointer): Pointer; inline;
begin
  Result := Pointer(PByte(Instance) + (IntPtr(P) and $00FFFFFF));
end;

function GetCodePointer(Instance: TObject; P: Pointer): Pointer; inline;
{$IFDEF CPUX86}
begin
  if IntPtr(P) and $FF000000 = $FE000000 then // Virtual Method
    Result := PPointer(PNativeUInt(Instance)^ + (UIntPtr(P) and $FFFF))^
  else // Static method
    Result := P;
end;
{$ENDIF}
{$IFDEF CPUX64}
begin
  if IntPtr(P) and $FF00000000000000 = $FE00000000000000 then // Virtual Method
    Result := PPointer(PNativeUInt(Instance)^ + (UIntPtr(P) and $FFFF))^
  else // Static method
    Result := P;
end;
{$ENDIF}

function GetPropertyAsFPoint(Instance: TObject; const Info: PPropInfo): TFPoint;
var
  vMetod: TMethod;
begin
  if IsField(Info^.GetProc) then
    Result := PFPoint(GetField(Instance, Info^.GetProc))^
  else
  begin
    vMetod.Data := Instance;
    vMetod.Code := @GetCodePointer(Instance, Info^.GetProc)^;
    Result := sgFunction.TsgObjProcFPoint(vMetod);
  end;
end;

function GetRecordType(PropInfo: PPropInfo): TsgRecordType;
begin
  Result := tkrUndefined;
  if PropInfo^.PropType^^.Name = 'TFPoint' then
    Result := tkrTFPoint;
end;
{$ENDIF}

function DeleteAttributes(const ANode: TsgNode;
  const AAttribNames: TStrings; const AUseChild: Boolean): Integer;
var
  I, vRez: Integer;
  vAttrib: TsgNodeSample;
begin
  Result := 0;
  if ANode.HasAttributeNodes then
    for I := ANode.AttributeNodesCount - 1 downto 0 do
    begin
      vAttrib := ANode.AttributeNodes[I];
      if AAttribNames.IndexOf(vAttrib.Name) > 0 then
      begin
        Inc(Result);
        ANode.Attributes.Delete(I);
        vAttrib.Free;
      end;
    end;
  if AUseChild then
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vRez := DeleteAttributes(TsgNode(ANode.ChildNodes[I]), AAttribNames, AUseChild);
      Inc(Result, vRez);
    end;
end;


function GetValueRound(const AData: TsgData;
  const ADigits: Integer = iRoundDigits): string;
begin
  case AData.ValueType of
    dtDouble: Result := FloatRoundToStr(AData.ValueAsDouble, - ADigits);
    dtTFPoint: Result := FPointRoundToStr(AData.ValueAsFPoint, - ADigits);
    dtTF2DPoint: Result := F2DPointRoundToStr(AData.ValueAsF2DPoint, - ADigits);
    dtTFRect: Result := FRectRoundToStr(AData.ValueAsFRect, - ADigits);
    dtTF2DRect: Result := F2DRectRoundToStr(AData.ValueAsF2DRect, - ADigits);
  else
    Result := AData.ValueAsStr;
  end;
end;

procedure SetDataValueAsDouble(const AData: TsgData; const AValue: Double;
  const AViewMode: Boolean);
begin
  if AViewMode then
    AData.ValueAsStr := FloatRoundToStr(AValue, - iRoundDigits)
  else
    AData.ValueAsDouble :=  AValue;
end;

procedure SetDataValueAsFPoint(const AData: TsgData; const AValue: TFPoint;
  const AViewMode: Boolean);
begin
  if AViewMode then
    AData.ValueAsStr := FPointRoundToStr(AValue, - iRoundDigits)
  else
    AData.ValueAsFPoint :=  AValue;
end;

procedure SetDataValueAsF2DPoint(const AData: TsgData; const AValue: TF2DPoint;
  const AViewMode: Boolean);
begin
  if AViewMode then
    AData.ValueAsStr := F2DPointRoundToStr(AValue, - iRoundDigits)
  else
    AData.ValueAsF2DPoint :=  AValue;
end;

procedure SetDataValueAsFRect(const AData: TsgData; const AValue: TFRect;
  const AViewMode: Boolean);
begin
  if AViewMode then
    AData.ValueAsStr := FRectRoundToStr(AValue, - iRoundDigits)
  else
    AData.ValueAsFRect:=  AValue;
end;

procedure SetDataValueAsF2DRect(const AData: TsgData; const AValue: TF2DRect;
  const AViewMode: Boolean);
begin
  if AViewMode then
    AData.ValueAsStr := F2DRectRoundToStr(AValue, - iRoundDigits)
  else
    AData.ValueAsF2DRect :=  AValue;
end;

function StrToDayMonthYear(const AStr: string;
  const AError: PInteger = nil): TDateTime;
var
  vStr: TStringList;
  vDay, vMonth, vYear: Word;
  vError: Integer;
begin
  Result := SysUtils.Date;//  FillChar(Result, SizeOf(Result), 0);
  vError := 0;
  vStr := TsgStringList.Create;
  try
    TsgStringList(vStr).LineBreak := cnstDayMonthYearSeparator;
    vStr.Text := AStr;
    sgDeleteEmptyStrings(vStr);
    try
      DecodeDate(Result, vYear, vMonth, vDay);
      if vStr.Count > 0 then
      begin
        Inc(vError);
        vDay := StrToIntDef(vStr[0], vDay);
        if vStr.Count > 1 then
        begin
          Inc(vError);
          vMonth := StrToIntDef(vStr[1], vMonth);
          if vStr.Count > 2 then
          begin
            Inc(vError);
            vYear := StrToIntDef(vStr[2], vYear);
         end;
        end;
        Result := EncodeDate(vYear, vMonth, vDay);
      end;
    except
      vError := -vError;
    end;
  finally
    if AError <> nil then
      AError^ := vError;
    vStr.Free;
  end;
end;

function IntToStrWithSymbolCount(const AValue: Integer;
  const ADigits: Integer = 0): string;
begin
  Result := IntToStr(AValue);
  while Length(Result) < ADigits do
    Result := '0' + Result;
end;

{$IFDEF SGDEL_7}
function IsNoInitVariant(const V: OleVariant): Boolean;
begin
  Result := FindVarData(V)^.VType in [varEmpty, varNull];
end;

function VariantToAddr(const V: Variant; const AVarType: Integer;
  const Addr: Pointer; const Separator: Char = cnstPoint): Boolean;
var
  vVarType: TVarType;
begin
  Result := True;
  vVarType := VarType(V) and varTypeMask;
  if vVarType = AVarType then
  begin
    case AVarType of
      varBoolean:  PBoolean(Addr)^ := V;
      varByte:     PByte(Addr)^ := V;
      varWord:     PWord(Addr)^ := V;
      varInteger:  PInteger(Addr)^ := V;
      varUInt64:   PUInt64(Addr)^ := V;
      varSingle:   PSingle(Addr)^ := V;
      varDouble:   PDouble(Addr)^ := V;
    else
      Result := False;
    end;
  end
  else
  begin
    case AVarType of
      varBoolean:  PBoolean(Addr)^ := StrToBoolDef(V, False);
      varByte:     PByte(Addr)^ := StrToIntDef(V, 0);
      varWord:     PWord(Addr)^ := StrToIntDef(V, 0);
      varInteger:  PInteger(Addr)^ := StrToIntDef(V, 0);
      varUInt64:   PUInt64(Addr)^ := StrToInt64Def(V, 0);
      varSingle:   PSingle(Addr)^ := sgStrToDouble(V, Separator);
      varDouble:   PDouble(Addr)^ := sgStrToDouble(V, Separator);
    else
      Result := False;
    end;
  end;
end;

function VariantToDouble(const V: Variant; const ASeparator: Char = cnstPoint;
  const ADefault: Double = 0): Double;
begin
  Result := ADefault;
  VariantToAddr(V, varDouble, @Result);
end;
{$ENDIF}

function DayMonthYearToStr(const AValue: TDateTime): string;
var
  vDay, vMonth, vYear: Word;
begin
  Result := '';
  DecodeDate(AValue, vYear, vMonth, vDay);
  Result := IntToStrWithSymbolCount(vDay, 2) + cnstDayMonthYearSeparator +
    IntToStrWithSymbolCount(vMonth, 2) + cnstDayMonthYearSeparator +
    IntToStrWithSymbolCount(vYear, 4);
end;

function GuidToStr(const AGuid: TGUID): string;
begin
{$IFNDEF SGDEL_6}
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
    [AGuid.D1, AGuid.D2, AGuid.D3, AGuid.D4[0], AGuid.D4[1], AGuid.D4[2], AGuid.D4[3],
    AGuid.D4[4], AGuid.D4[5], AGuid.D4[6], AGuid.D4[7]]);
{$ELSE}
  Result := GUIDToString(AGuid);
{$ENDIF}
end;

function StrToGUID(const AGuid: string): TGUID;
begin
{$IFNDEF SGDEL_6}
  ActiveX.IIDFromString(PWideChar(WideString(AGuid)), Result);
{$ELSE}
  Result := StringToGUID(AGuid);
{$ENDIF}
end;

function IsGuidNull(const AGuid: TGUID): Boolean;
begin
  Result := sgIsEqualIID(AGuid, GUID_NULL);
end;

const
  cnstCDATABegin = '[';
  cnstCDATAEnd = ']';
  cnstCommentBegin = '<!--';
  cnstCommentEnd = '-->';
  cnstDecimalSeparatorFloat = '.';
  cnstDecimalSeparatorValues = ',';
  cnstDecimalSeparatorVersion = '.';
  cnstDOCTYPE = 'DOCTYPE';
  cnstENTITY = 'ENTITY';
  cnstKeyWordsStr: array [TsgTagID] of string = ('', 'cx', 'cy', 'r', 'rx',
    'ry', 'style', 'x1', 'y1', 'x2', 'y2', 'x', 'y', 'dx', 'dy', 'points',
    'width', 'height', 'd', 'pathlength', 'id', 'href', 'transform', 'viewbox',
    'preserveaspectratio', 'class', 'clip-path', 'stroke', 'stroke-width',
    'stroke-linecap', 'stroke-linejoin', 'stroke-miterlimit',
    'stroke-dasharray', 'stroke-dashoffset', 'stroke-opacity', 'vector-effect',
    'fill', 'fill-opacity', 'fill-rule', 'gradientunits', 'gradienttransform',
    'spreadmethod', 'fx', 'fy', 'stop-color', 'stop-opacity', 'offset',
    'patternunits', 'patterncontentunits', 'font', 'font-family', 'font-style',
    'font-weight', 'font-size', 'font-variant', 'font-stretch',
    'text-anchor', 'text-decoration', 'display', 'visibility', 'opacity');
{$IFDEF SG_PARSER_DEBUG}
  cnstNodeTypeNames: array[TsgNodeType] of string =
    ('', 'Element','Attribute','Text','CDATASection', 'EntityRef','Entity',
     'ProcessingInstr', 'Comment','Document','DocumentType',
     'DocumentFragment','Notation');
{$ENDIF}
  cnstRGB = 'rgb';
//  cnstColorHexPrefix = '#';
  cnstSeparatorFullName = ':';
  cnstStringColors: array [0 .. 136] of record
    Name: string;
    Value: TColor;
  end = ((Name: 'aliceblue'; Value: $F0F8FF),
    (Name: 'antiquewhite'; Value: $FAEBD7), (Name: 'aqua'; Value: $00FFFF),
    (Name: 'aquamarine'; Value: $7FFFD4), (Name: 'azure'; Value: $F0FFFF),
    (Name: 'beige'; Value: $F5F5DC), (Name: 'black'; Value: $000000),
    (Name: 'blue'; Value: $0000FF), (Name: 'blueviolet'; Value: $8A2BE2),
    (Name: 'brown'; Value: $A52A2A), (Name: 'burlywood'; Value: $DEB887),
    (Name: 'cadetblue'; Value: $5F9EA0), (Name: 'chartreuse'; Value: $7FFF00),
    (Name: 'chocolate'; Value: $D2691E), (Name: 'coral'; Value: $FF7F50),
    (Name: 'cornflowerblue'; Value: $6495ED), (Name: 'cornsilk'; Value: $FFF8DC),
    (Name: 'crimson'; Value: $DC143C), (Name: 'darkblue'; Value: $00008B),
    (Name: 'darkcyan'; Value: $008B8B), (Name: 'darkgoldenrod'; Value: $B8860B),
    (Name: 'darkgray'; Value: $A9A9A9), (Name: 'darkgreen'; Value: $006400),
    (Name: 'darkkhaki'; Value: $BDB76B), (Name: 'darkmagenta'; Value: $8B008B),
    (Name: 'darkolivegreen'; Value: $556B2F),
    (Name: 'darkorange'; Value: $FF8C00), (Name: 'darkorchid'; Value: $9932CC),
    (Name: 'darkred'; Value: $8B0000), (Name: 'darksalmon'; Value: $E9967A),
    (Name: 'darkseagreen'; Value: $8FBC8F),
    (Name: 'darkslateblue'; Value: $483D8B),
    (Name: 'darkslategray'; Value: $2F4F4F),
    (Name: 'darkturquoise'; Value: $00CED1),
    (Name: 'darkviolet'; Value: $9400D3), (Name: 'deeppink'; Value: $FF1493),
    (Name: 'deepskyblue'; Value: $00BFFF), (Name: 'dimgray'; Value: $696969),
    (Name: 'dodgerblue'; Value: $1E90FF), (Name: 'firebrick'; Value: $B22222),
    (Name: 'floralwhite'; Value: $FFFAF0), (Name: 'forestgreen'; Value: $228B22),
    (Name: 'fuchsia'; Value: $FF00FF), (Name: 'gainsboro'; Value: $DCDCDC),
    (Name: 'ghostwhite'; Value: $F8F8FF), (Name: 'gold'; Value: $FFD700),
    (Name: 'goldenrod'; Value: $DAA520), (Name: 'gray'; Value: $808080),
    (Name: 'green'; Value: $008000), (Name: 'greenyellow'; Value: $ADFF2F),
    (Name: 'honeydew'; Value: $F0FFF0), (Name: 'hotpink'; Value: $FF69B4),
    (Name: 'indianred'; Value: $CD5C5C), (Name: 'indigo'; Value: $4B0082),
    (Name: 'ivory'; Value: $FFFFF0), (Name: 'khaki'; Value: $F0E68C),
    (Name: 'lavender'; Value: $E6E6FA), (Name: 'lavenderblush'; Value: $FFF0F5),
    (Name: 'lawngreen'; Value: $7CFC00), (Name: 'lemonchiffon'; Value: $FFFACD),
    (Name: 'lightblue'; Value: $ADD8E6), (Name: 'lightcoral'; Value: $F08080),
    (Name: 'lightcyan'; Value: $E0FFFF),
    (Name: 'lightgoldenrodyellow'; Value: $FAFAD2),
    (Name: 'lightgreen'; Value: $90EE90), (Name: 'lightgrey'; Value: $D3D3D3),
    (Name: 'lightpink'; Value: $FFB6C1), (Name: 'lightsalmon'; Value: $FFA07A),
    (Name: 'lightseagreen'; Value: $20B2AA),
    (Name: 'lightskyblue'; Value: $87CEFA),
    (Name: 'lightslategray'; Value: $778899),
    (Name: 'lightsteelblue'; Value: $B0C4DE),
    (Name: 'lightyellow'; Value: $FFFFE0),
    (Name: 'lime'; Value: $00FF00), (Name: 'limegreen'; Value: $32CD32),
    (Name: 'linen'; Value: $FAF0E6), (Name: 'maroon'; Value: $800000),
    (Name: 'mediumaquamarine'; Value: $66CDAA),
    (Name: 'mediumblue'; Value: $0000CD), (Name: 'mediumorchid'; Value: $BA55D3),
    (Name: 'mediumpurple'; Value: $9370DB),
    (Name: 'mediumseagreen'; Value: $3CB371),
    (Name: 'mediumslateblue'; Value: $7B68EE),
    (Name: 'mediumspringgreen'; Value: $00FA9A),
    (Name: 'mediumturquoise'; Value: $48D1CC),
    (Name: 'mediumvioletred'; Value: $C71585),
    (Name: 'midnightblue'; Value: $191970), (Name: 'mintcream'; Value: $F5FFFA),
    (Name: 'mistyrose'; Value: $FFE4E1), (Name: 'moccasin'; Value: $FFE4B5),
    (Name: 'navajowhite'; Value: $FFDEAD), (Name: 'navy'; Value: $000080),
    (Name: 'none'; Value: clNone), (Name: 'oldlace'; Value: $FDF5E6),
    (Name: 'olive'; Value: $808000), (Name: 'olivedrab'; Value: $6B8E23),
    (Name: 'orange'; Value: $FFA500), (Name: 'orangered'; Value: $FF4500),
    (Name: 'orchid'; Value: $DA70D6), (Name: 'palegoldenrod'; Value: $EEE8AA),
    (Name: 'palegreen'; Value: $98FB98), (Name: 'paleturquoise'; Value: $AFEEEE),
    (Name: 'palevioletred'; Value: $DB7093), (Name: 'papayawhip'; Value: $FFEFD5),
    (Name: 'peachpuff'; Value: $FFDAB9), (Name: 'peru'; Value: $CD853F),
    (Name: 'pink'; Value: $FFC0CB), (Name: 'plum'; Value: $DDA0DD),
    (Name: 'powderblue'; Value: $B0E0E6), (Name: 'purple'; Value: $800080),
    (Name: 'red'; Value: $FF0000), (Name: 'rosybrown'; Value: $BC8F8F),
    (Name: 'royalblue'; Value: $4169E1),
    (Name: 'saddlebrown'; Value: $8B4513), (Name: 'salmon'; Value: $FA8072),
    (Name: 'sandybrown'; Value: $F4A460), (Name: 'seagreen'; Value: $2E8B57),
    (Name: 'seashell'; Value: $FFF5EE), (Name: 'sienna'; Value: $A0522D),
    (Name: 'silver'; Value: $C0C0C0), (Name: 'skyblue'; Value: $87CEEB),
    (Name: 'slateblue'; Value: $6A5ACD), (Name: 'slategray'; Value: $708090),
    (Name: 'snow'; Value: $FFFAFA), (Name: 'springgreen'; Value: $00FF7F),
    (Name: 'steelblue'; Value: $4682B4), (Name: 'tan'; Value: $D2B48C),
    (Name: 'teal'; Value: $008080), (Name: 'thistle'; Value: $D8BFD8),
    (Name: 'tomato'; Value: $FF6347), (Name: 'turquoise'; Value: $40E0D0),
    (Name: 'violet'; Value: $EE82EE), (Name: 'wheat'; Value: $F5DEB3),
    (Name: 'white'; Value: $FFFFFF), (Name: 'whitesmoke'; Value: $F5F5F5),
    (Name: 'yellow'; Value: $FFFF00), (Name: 'yellowgreen'; Value: $9ACD32));

  cnstStringWebKeysLast = 4;
  cnstStringWebKeys: array [0..8] of record
    Str: string;
    Key: string;
  end = (
    (Str: '&amp;'; Key: cnstAmpersant),
    (Str: '&lt;'; Key: cnstLess),
    (Str: '&gt;'; Key: cnstGreater),
    (Str: '&apos;'; Key: cnstApos),
    (Str: '&quot;'; Key: cnstQuotationMark),

    (Str: '&deg;'; Key: cnstSymbolDegree),
    (Str: '&copy;'; Key: cnstCopyRight),
    (Str: '&reg;'; Key: cnstRegisteredSign),
    (Str: '&nbsp;'; Key: cnstSpace));

  cnstUnitsOfMeasurements: array [0..9] of record
    Name: string;
    Value: TsgUnitsOfMeasurements;
  end = ((Name: '%'; Value: umPR), (Name: 'PR'; Value: umPR),
    (Name: 'PX'; Value: umPX), (Name: 'MM'; Value: umMM),
    (Name: 'CM'; Value: umCM), (Name: 'IN'; Value: umIN),
    (Name: 'EM'; Value: umEM), (Name: 'PT'; Value: umPT),
    (Name: 'PC'; Value: umPC), (Name: 'M'; Value: umM));

{$IFNDEF SG_FIREMONKEY}
  EncodeBase64Table: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
{$ENDIF}

const
  cnstCountValue: array [Boolean] of Integer = (1, 2);

{$IFDEF SGDEL_2009}
type
  TsgNodeEnumeratorBase = class(TInterfacedObject, IEnumerable, IEnumerator)
  private
    FIndex: Integer;
    FList: TsgNodeList;
  public
    constructor Create(const ANodeList: TsgNodeList);
    function GetCurrent: TObject; overload;
    function GetEnumerator: IEnumerator; overload;
    function MoveNext: Boolean;
    procedure Reset;
  end;

  TsgNodeEnumerator = class(TsgNodeEnumeratorBase, IEnumerable<TsgNodeSample>, IEnumerator<TsgNodeSample>)
  public
    function GetCurrent: TsgNodeSample; overload;
    function GetEnumerator: IEnumerator<TsgNodeSample>; overload;
  end;

{$ENDIF}

type
  TsgMemoryStream = class(TMemoryStream);
  TsgClassOfNode = class of TsgNodeSample;

  TsgBase64 = class
  private
{$IFNDEF SG_FIREMONKEY}
    FBuffer: string;
    FCol: TsgNativeInt;
    FRMax: TsgNativeInt;
    FRow: TsgNativeInt;
    FStrs: TStrings;
  protected
    function Next: Char;
{$ENDIF}
  public
    procedure Decode(const AStr: string; AStrs: TStrings; const S: TStream);
    procedure EnCode(const AInStream: TStream; const AOutStream: TStream);
  end;

  TsgParserStage = (psUndifined, psBeginValue, psValue, psEnd, psText, psAttrib);

var
  KeyWords: TsgStringList = nil;
  TableColors: TsgStringList = nil;
{$IFNDEF SG_OPENING_IN_THEADS}
  Spliter: TsgStringList = nil;
{$ENDIF}

function IsNodeSample(const AValue: Pointer): Boolean;
begin
  try
    Result := False;
    if TObject(AValue) is TsgNodeSample then
      Result := True;
  except
    Result := False;
  end;
end;


function GetXMLNodeTextAsInt(const ANode: TsgNodeSample;
  const ADefaultValue: Integer = 0): Integer;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsInt;
end;

function GetXMLNodeTextAsIntWithRang(const ANode: TsgNodeSample;
  const ALow, AHigh: Integer; const ADefaultValue: Integer = 0): Integer;
var
  vValue: Integer;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
  begin
    vValue := ANode.TextAsInt;
    if (vValue >= ALow) and (vValue <= AHigh) then
      Result := vValue;
  end;
end;

function GetXMLNodeTextAsHanle(const ANode: TsgNodeSample;
  const ADefaultValue: Integer = 0): UInt64;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsHandle;
end;

function GetXMLNodeTextAsBool(const ANode: TsgNodeSample;
  const ADefaultValue: Boolean = False): Boolean;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsBool;
end;

function GetXMLNodeTextAsDouble(const ANode: TsgNodeSample;
  const ADefaultValue: Double = 0): Double;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsDouble;
end;

function GetXMLNodeTextAsString(const ANode: TsgNodeSample;
  const ADefaultValue: string = ''): string;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsStr;
end;

function GetXMLNodeTextAsFRect(const ANode: TsgNodeSample;
  const ADefaultValue: TFRect): TFRect;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextAsFRect;
end;

function GetXMLNodeTextAsTFPoint(const ANode: TsgNodeSample;
  const ADefaultValue: TFPoint): TFPoint;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextData.ValueAsFPoint;
end;

function GetXMLNodeTextAsTF2DPoint(const ANode: TsgNodeSample;
  const ADefaultValue: TF2DPoint): TF2DPoint;
begin
  Result := ADefaultValue;
  if Assigned(ANode) then
    Result := ANode.TextData.ValueAsF2DPoint;
end;

procedure CopyChildTextAsStr(const AElem: TsgNode;
  const ASourceNodeName, ADestNdoeName: string);
var
  vItem: TsgNodeSample;
begin
  vItem := AElem.GetChildByName(ASourceNodeName);
  if Assigned(vItem) then
    SetChildStr(AElem, ADestNdoeName, vItem.TextAsStr);
end;

function GetAttributeBool(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsBool;
end;

function GetAttributeFloat(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Double = 0): Double;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsDouble;
end;

function GetAttributeFPoint(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: TFPoint): TFPoint;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsFPoint;
end;

function GetAttributeInt(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Integer = 0): Integer;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsInt;
end;

function GetAttributeInt64(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Int64 = 0): Int64;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsInt64;
end;

function GetAttributeInt64List(const AElem: TsgNodeSample;
  const AName: string; const AList: TsgInt64List;
  const ADecimalSeparatorNames: Char = ';'): Integer;
var
  I: Integer;
  vItem: TsgNodeSample;
  vStrValues: TsgStringList;
  vValue: Int64;
begin
  Result := -1;
  vItem := AElem.GetAttributeByName(AName);
  if Assigned(vItem) then
  begin
    Result := AList.Count;
    vStrValues := TsgStringList.Create;
    try
      vStrValues.LineBreak := ADecimalSeparatorNames;
      vStrValues.Text := vItem.ValueAsStr;
      AList.Capacity := AList.Capacity + vStrValues.Count;
      for I := 0 to vStrValues.Count - 1 do
      begin
        if Length(vStrValues[I]) > 0 then
        begin
          if TryStrToInt64(vStrValues[I], vValue) then
            AList.Add(vValue);
        end;
      end;
    finally
      vStrValues.Free;
      Result := AList.Count - Result;
    end;
  end;
end;

function GetAttributeStr(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: string = ''): string;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.Value;
end;

function GetAttributeHandle(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Uint64 = cnstBadHandle): Uint64;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsHandle;
end;

function GetAttributeTxt(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: string = ''): string;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueAsText;
end;

function GetAttributeColor(const AElem: TsgNodeSample; const AName: string;
  const ADefaultValue: Integer = 0): TColor;
var
  vItem: TsgNodeSample;
begin
  Result := ADefaultValue;
  vItem := AElem.GetAttributeByName(AName);
  if vItem <> nil then
    Result := vItem.ValueData.ValueAsColor;
end;

function GetActAttrib(const AElem: TsgNode; const AName: string): TsgNodeSample;
begin
  Result := AElem.GetAttributeByName(AName);
  if not Assigned(Result) then
    Result := AElem.AddAttribNV(AName);
end;

function GetActChild(const AElem: TsgNode; const AName: string): TsgNodeSample;
begin
  Result := AElem.GetChildByName(AName);
  if not Assigned(Result) then
    Result := AElem.AddChildNV(AName);
end;

function SetAttributeStr(const AElem: TsgNode; const AName: string;
  const AValue: string; const ACreateAttrib: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActAttrib(AElem, AName);
  if (not Assigned(vItem)) and ACreateAttrib then
    vItem := AElem.AddAttribNV(AName);
  if Assigned(vItem) then
  begin
    vItem.ValueAsStr := AValue;
    Result := True;
  end;
end;

function SetAttributeInt(const AElem: TsgNode; const AName: string;
  const AValue: Integer; const ACreateAttrib: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActAttrib(AElem, AName);
  if (not Assigned(vItem)) and ACreateAttrib then
    vItem := AElem.AddAttribNV(AName);
  if Assigned(vItem) then
  begin
    Result := True;
    vItem.ValueAsInt := AValue;
  end;
end;

function SetAttributeColor(const AElem: TsgNode; const AName: string;
  const AValue: TColor; const ACreateAttrib: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActAttrib(AElem, AName);
  if (not Assigned(vItem)) and ACreateAttrib then
    vItem := AElem.AddAttribNV(AName);
  if Assigned(vItem) then
  begin
    Result := True;
    vItem.ValueData.ValueAsColor := AValue;
  end;
end;

function SetAttributeBool(const AElem: TsgNode; const AName: string;
  const AValue: Boolean; const ACreateAttrib: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActAttrib(AElem, AName);
  if (not Assigned(vItem)) and ACreateAttrib then
    vItem := AElem.AddAttribNV(AName);
  if Assigned(vItem) then
  begin
    vItem.ValueAsBool := AValue;
    Result := True;
  end;
end;

function SetAttributeTxt(const AElem: TsgNode; const AName: string;
  const AValue: string; const ACreateAttrib: Boolean = False): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActAttrib(AElem, AName);
  if (not Assigned(vItem)) and ACreateAttrib then
    vItem := AElem.AddAttribNV(AName);
  if Assigned(vItem) then
  begin
    vItem.ValueAsText := AValue;
    Result := True;
  end;
end;

function SetChildBool(const AElem: TsgNode; const AName: string;
  const AValue: Boolean): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActChild(AElem, AName);
  if Assigned(vItem) then
  begin
    vItem.TextAsBool := AValue;
    Result := True;
  end;
end;

function SetChildFloat(const AElem: TsgNode; const AName: string;
  const AValue: Double): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActChild(AElem, AName);
  if Assigned(vItem) then
  begin
    vItem.TextAsDouble := AValue;
    Result := True;
  end;
end;

function SetChildInt(const AElem: TsgNode; const AName: string;
  const AValue: Integer): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActChild(AElem, AName);
  if Assigned(vItem) then
  begin
    vItem.TextAsInt := AValue;
    Result := True;
  end;
end;

function SetChildStr(const AElem: TsgNode; const AName: string;
  const AValue: string): Boolean;
var
  vItem: TsgNodeSample;
begin
  Result := False;
  vItem := GetActChild(AElem, AName);
  if Assigned(vItem) then
  begin
    vItem.TextAsStr := AValue;
    Result := True;
  end;
end;

function SaveNodeToXMLString(const ANode: TsgNodeSample): string;
var
  vParser: TsgParser;
begin
  vParser := TsgParser.Create;
  try
    try
      vParser.FROOT.Add(ANode);
      Result := vParser.SaveToString;
    finally
      vParser.FROOT.Count := 0;
    end;
  finally
    vParser.Free;
  end;
end;

function SaveNodeToXMLString(const ANode: TsgNodeSample;
  const ADefaultValue: string): string;
begin
  if Assigned(ANode) then
    Result := SaveNodeToXMLString(ANode)
  else
    Result := ADefaultValue;
end;

procedure AssignObjLists(const AOwner: TsgNodeSample;
  const ASource: TsgNodeList; var ADest: TsgNodeList);
var
  I: Integer;
  vNodeBase, vNodeNew: TsgNodeSample;
begin
  if ASource = nil then
  begin
    if ADest <> nil then
      FreeAndNil(ADest);
  end
  else
  begin
    if ADest <> nil then
      ADest.ClearNodes
    else
      ADest := TsgNodeList.CreateNodeList;
    ADest.QwickFind := False;
    ADest.Capacity := ASource.Count;
    for I := 0 to ASource.Count - 1 do
    begin
       vNodeBase := TsgNodeSample(ASource.Nodes[I]);
       vNodeNew := TsgClassOfNode(vNodeBase.ClassType).Create;
       ADest.Add(vNodeNew);
       vNodeNew.Assign(vNodeBase);
       vNodeNew.FOwner := AOwner;
    end;
    ADest.QwickFind := ASource.QwickFind;
  end;
end;

procedure CheckStringParams(const AParams: TStrings);
var
  I: Integer;
begin
  I := 0;
  while I < AParams.Count do
  begin
    if Length(AParams[I]) = 0 then
      AParams.Delete(I)
    else
      Inc(I);
  end;
end;

procedure CreateKeyWords;
var
  I: TsgTagID;
begin
  KeyWords := TsgStringList.Create;
  KeyWords.Sorted := True;
  KeyWords.Duplicates := dupIgnore;
  KeyWords.CaseSensitive := False;
  KeyWords.Capacity := Integer(High(TsgTagID)) - Integer(Low(TsgTagID)) + 1;
  for I := Low(TsgTagID) to High(TsgTagID) do
    KeyWords.AddObject(cnstKeyWordsStr[I], TsgObjectWithField.CreateInt(Ord(I)));
end;

procedure DecodeBase64(const AString: string; AStrings: TStrings;
  var AStream: TStream);
var
  vCoderBase64: TsgBase64;
begin
  vCoderBase64 := TsgBase64.Create;
  try
    vCoderBase64.Decode(AString, AStrings, AStream);
  finally
    vCoderBase64.Free;
  end;
end;

procedure EncodeBase64(const AInStream: TStream;
  var AOutStream: TStream);
var
  vCoderBase64: TsgBase64;
begin
  vCoderBase64 := TsgBase64.Create;
  try
    vCoderBase64.Encode(AInStream, AOutStream);
  finally
    vCoderBase64.Free;
  end;
end;

function StrDecodeBase64(const AString: string): string;
var
  vMemBase64: TMemoryStream;
  vStrings: TStringList;
begin
  Result := '';
  vMemBase64 := TMemoryStream.Create;
  vStrings := TStringList.Create;
  try
    DecodeBase64(AString, nil, TStream(vMemBase64));
    vMemBase64.Position := 0;
    vStrings.LoadFromStream(vMemBase64);
    Result := vStrings.Text;
  finally
    vStrings.Free;
    vMemBase64.Free;
  end;
end;

function StrEncodeBase64(const AString: string {$IFDEF SGDEL_2009}; AEncoding: TEncoding = nil{$ENDIF}): string;
var
  vMem: TMemoryStream;
  vMemBase64: TMemoryStream;
  vStrings: TStringList;
begin
  Result := '';
  vMem := TMemoryStream.Create;
  vMemBase64 := TMemoryStream.Create;
  vStrings := TStringList.Create;
  try
    vStrings.Text := AString;
    vStrings.SaveToStream(vMem{$IFDEF SGDEL_2009}, AEncoding {$ENDIF});
    vMem.Position := 0;
    EncodeBase64(vMem, TStream(vMemBase64));
    vMemBase64.Position := 0;
    vStrings.LoadFromStream(vMemBase64);
    Result := vStrings.Text;
  finally
    vStrings.Free;
    vMemBase64.Free;
    vMem.Free;
  end;
end;

procedure CreateTableColor;
var
  I: Integer;
begin
  TableColors := TsgStringList.Create;
  TableColors.Capacity := High(cnstStringColors) - Low(cnstStringColors) + 1;
  TableColors.Sorted := True;
  for I := Low(cnstStringColors) to High(cnstStringColors) do
    TableColors.AddObject(cnstStringColors[I].Name,
      TsgObjectInt64.CreateInt(cnstStringColors[I].Value));
end;

function GetColorByName(const AString: string): TsgColor;
var
  I: Integer;
  vStringColor: string;
begin
  Result.Color := clBlack;
  if Length(AString) > 0 then
  begin
    if TableColors = nil then
      CreateTableColor;
    if (AString[1] = 'c') and (AString[2] = 'l') then
      vStringColor := Copy(AString, 2, Length(AString) - 2)
    else
      vStringColor := AString;
    I := TableColors.IndexOf(vStringColor);
    if I > -1 then
      Result.Color := TsgObjectInt64(TableColors.Objects[I]).FieldInt;
  end;
end;

function GetNameFromFullName(const AFillName: string): string;
var
  vLength, vIndex: Integer;
begin
  vLength := Length(AFillName);
  if vLength > 0 then
  begin
    vIndex := sgStringScan(cnstSeparatorFullName, AFillName, 1);
    if vIndex > 0 then
      Result := Copy(AFillName, vIndex + 1, vLength - vIndex)
    else
      Result := AFillName;
  end
  else
    Result := '';
end;

function GetCodePageByName(const AStr: string; var ACodePage: Integer): Boolean;
var
  vStr: string;
begin
  vStr := AnsiLowerCase(AStr);
  ACodePage := CodePageIdTable.GetCodePage(vStr);
  Result := ACodePage > -1;
end;

function GetCodePageName(const ACodePage: Integer): string;
begin
  Result := CodePageIdTable.GetName(ACodePage);
end;

{$IFDEF SGDEL_2009}
function GetEncodingCodePageName(const AEncoding: TEncoding): string;
begin
{$IFDEF SGDEL_XE2}
  Result := GetCodePageName(AEncoding.CodePage);
{$ELSE}
  if AEncoding is TUTF8Encoding then
    Result := CodePageIdTable.GetName(65001)
  else
    Result := '';
{$ENDIF}
end;
{$ENDIF}

function GetSimpleText(const AStr: string; const APoint1: PFPoint): string;
var
  vStrings: TsgStringList;
  vNode: TsgNodeSample;
  vX, vY: TsgNodeSample;
  vPoint1: TFPoint;
begin
  Result := AStr;
  if Length(Result) > 0 then
  begin
    if IsHandleOfNode(Result) then
    begin
      vNode := GetNodeByHandle(Result);
      if vNode <> nil then
      begin
        if (APoint1 <> nil) and (APoint1^.Z = 0) then
        begin
          vX := vNode.GetAttributeByTagID(idX);
          vY := vNode.GetAttributeByTagID(idY);
          if Assigned(vX) or Assigned(vY) then
          begin
            vPoint1 := cnstFPointZero;
          if Assigned(vX) then
           vPoint1.X := vX.ValueAsDouble;
          if Assigned(vY) then
           vPoint1.Y := vY.ValueAsDouble;
          vPoint1.Z := 1;
          APoint1^ := vPoint1;
          end;
        end;
        vStrings := TsgStringList.Create;
        try
          vStrings.LineBreak := cnstLineBreakText;
          vStrings.Text := vNode.TextAsText;
          Result := GetFullText(vStrings);
        finally
          FreeAndNil(vStrings);
        end;
      end;
    end;
  end;
end;

function GetFullText(const AStrings: TStringList; const APoint1: PFPoint = nil): string;
var
  I: Integer;
  vStr: string;
begin
  Result := '';
  for I := 0 to AStrings.Count - 1 do
  begin
    vStr := GetSimpleText(AStrings[I], APoint1);
    Result := Result + vStr;
  end;
end;

function GetHandleByNode(const ANode: TsgNodeSample): string;
begin
  Result := cnstLineBreakText + cnstSymbolByHandle +
    IntToHex(TsgNativeInt(ANode), 0) + cnstLineBreakText;
end;

function GetHexValue(const AValue: string): string;
begin
  Result := '';
  if Length(AValue) > 1 then
  begin
    Result := AValue;
    case Result[1] of
      '#':  Result[1] := '$';
      '$':  begin end;
    else
      Result := '$' + Result;
    end;
  end;
end;

function GetNameByCodePage(const ACodePage: Integer; var AName: string): Boolean;
begin
  AName := CodePageIdTable.GetName(ACodePage);
  Result := Length(AName) > 0;
end;

function GetNodeByHandle(const AStr: string): TsgNodeSample;
var
  vNode: TsgNodeSample;
begin
  Result := nil;
  try
    vNode := TsgNodeSample(Pointer(StrHexToInt(Trim(AStr))));
    if vNode is TsgNodeSample then
      Result := vNode;
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
end;

function GetNameByTagId(const AId: TsgTagID): string;
begin
  Result := cnstKeyWordsStr[AId];
end;

function GetTagIdByName(const AName: string): TsgTagID;
var
  vIndex: Integer;
  vName: string;
begin
  vName := AnsiLowerCase(AName);
  if KeyWords = nil then
    CreateKeyWords;
  if KeyWords.Find(vName, vIndex) then
    Result := TsgTagID(TsgObjectInt64(KeyWords.Objects[vIndex]).FieldInt)
  else
    Result := idUndefined;
end;

function GetUnitsMeasurements(const AValue: string): TsgUnitsOfMeasurements;
var
  vIndex, vHigh: Integer;
  vValue: string;
begin
  Result := umUndefined;
  vValue := Trim(AValue);
  if Length(vValue) > 0 then
  begin
    vValue := UpperCase(vValue);
    vIndex := Low(cnstUnitsOfMeasurements);
    vHigh := High(cnstUnitsOfMeasurements);
    while (vIndex <= vHigh) and (sgStringPos(cnstUnitsOfMeasurements[vIndex].Name,
      vValue, 1) < 1) do
      Inc(vIndex);
    if vIndex <= vHigh then
      Result := cnstUnitsOfMeasurements[vIndex].Value;
  end;
end;

function IsApostrophe(const AChar: Char): Boolean;
begin
  case AChar of
    #34, #39: Result := True;
  else
    Result := False;
  end;
end;

function IsHandleOfNode(const AStr: string): Boolean;
begin
  Result := (Length(AStr) > 0) and (AStr[1] = cnstSymbolByHandle);
end;

function HasHandleOfNode(const AStr: string): Boolean;
begin
  Result := (Length(AStr) > 0) and (Pos(cnstSymbolByHandle,  AStr) > 0);
end;

function IsLetter(const AChar: Char): Boolean;
begin
{$IFDEF SGDEL_10_SEATTLE}
  Result := AChar.IsLetter;
{$ELSE}
{$IFDEF SGDEL_2009}
  Result := TCharacter.IsLetter(AChar);
{$ELSE}
  case AChar of
    'a'..'z', 'A'..'Z': Result := True;
  else
    Result := False;
  end;
{$ENDIF}
{$ENDIF}
end;

function IsName(const AChar: Char): Boolean;
begin
{$IFDEF SGDEL_2009}
  case AChar of
    '-', '_', ':':  Result := True;
  else
    Result := {$IFDEF SGDEL_XE4}AChar.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(AChar){$ENDIF};
  end;
{$ELSE}
  case AChar of
    'a'..'z', 'A'..'Z', '0'..'9', '-', '_', ':':  Result := True;
  else
    Result := False;
  end;
{$ENDIF}
//  Result := IsLetter(AChar) or IsNumber(c) or (AChar = '-') or (AChar = '_');
end;

function IsNumber(const AChar: Char): Boolean;
begin
{$IFDEF SGDEL_XE4}
  Result := AChar.IsNumber;
{$ELSE}
{$IFDEF SGDEL_2009}
  Result := TCharacter.IsNumber(AChar);
{$ELSE}
  case AChar of
    '0'..'9': Result := True;
  else
    Result := False;
  end;
{$ENDIF}
{$ENDIF}
end;

function ReplaceAnsiCodes(const AString: string): string;
begin
  Result :=  AString;
  ReplaceWebOrAnsiCode(False, Result);
end;

function ReplaceWebCodes(const AString: string): string;
begin
  Result := AString;
  ReplaceWebOrAnsiCode(True, Result);
end;

function ReplaceAnsiCodesEx(const AString: string): string;
begin
  Result :=  AString;
  if Length(Result) > 0 then
    ReplaceWebOrAnsiCode(False, Result, cnstStringWebKeysLast);
end;

function ReplaceWebCodeEx(const AString: string): string;
begin
  Result :=  AString;
  if Length(Result) > 0 then
    ReplaceWebOrAnsiCode(True, Result, cnstStringWebKeysLast);
end;

function ReplaceASCICode(const AStr: string; AUnicode: Boolean): string;
var
  vASCIKeyPos, J: Integer;
  vKey: string;
begin
  Result := AStr;
  if Length(Result) > 0 then
  begin
    if SizeOf(Char) = 1 then
      AUnicode := False;
    vASCIKeyPos := 1;
    vASCIKeyPos := sgStringPos(cnstASCIPrefix, Result, vASCIKeyPos);
    while vASCIKeyPos > 0 do
    begin
      J := vASCIKeyPos + 2;
      vKey := '';
      while (J <= Length(Result)) and (Result[J] <> ';') do
      begin
        vKey := vKey + Result[J];
        Inc(J);
      end;
      Delete(Result, vASCIKeyPos, Length(vKey) + 3);
      if Length(vKey) > 0 then
      begin
        case vKey[1] of
          'x', 'X':
            begin
              Delete(vKey, 1, 1);
              while Length(vKey) < 4 do
                vKey := '0' + vKey;
            end;
        else
          vKey := IntToHex(StrToInt(vKey), 4);
        end;
        if AUnicode then
          vKey := Char(StrToInt('$' + vKey))
        else
        begin
          if Length(vKey) > 4 then//need test
            Delete(vKey, 1, Length(vKey) - 4);
          vKey := cnstSymbolUnicode + vKey;
        end;
        Insert(vKey, Result, vASCIKeyPos);
        Inc(vASCIKeyPos, Length(vKey));
      end;
      vASCIKeyPos := sgStringPos(cnstASCIPrefix, Result, vASCIKeyPos);
    end;
  end;
end;

procedure ReplaceWebOrAnsiCode(const AToAnsi: Boolean; var AString: string;
  AMax: Integer = -1);
var
  I, vWebKeyPos, vLengthStrKey: Integer;
  vKey, vValue: string;
begin
  if Length(AString) > 0 then
  begin
    if (AMax < Low(cnstStringWebKeys)) or (AMax > High(cnstStringWebKeys)) then
      AMax := High(cnstStringWebKeys);
    for I := Low(cnstStringWebKeys) to AMax do
    begin
      if AToAnsi then
      begin
        vKey := cnstStringWebKeys[I].Str;
        vValue := cnstStringWebKeys[I].Key;
      end
      else
      begin
        vKey := cnstStringWebKeys[I].Key;
        vValue := cnstStringWebKeys[I].Str;
      end;
      vLengthStrKey := Length(vKey);
      vWebKeyPos := sgStringPos(vKey, AString, 1);
      while vWebKeyPos > 0 do
      begin
        Delete(AString, vWebKeyPos, vLengthStrKey);
        Insert(vValue, AString, vWebKeyPos);
        Inc(vWebKeyPos, Length(vValue));
        vWebKeyPos := sgStringPos(vKey, AString, vWebKeyPos);
      end;
    end;
  end;
end;

function ScanDigitsInString(const AString: string; const ALength: Integer;
  const AIsReadExponent: Boolean; var AIndex: Integer): Boolean;
var
  vHasPoint, vHasMinus, vHasDigits, vHasPlus: Boolean;
begin
  Result := False;
  vHasMinus := False;
  vHasPoint := False;
  vHasDigits := False;
  vHasPlus := False;
  while AIndex <= ALength do
  begin
    case AString[AIndex] of
      '0'..'9':  vHasDigits := True;
      'e', 'E':
        begin
          if not AIsReadExponent then
          begin
            if AIndex < ALength then
            begin
              Inc(AIndex);
              Result := ScanDigitsInString(AString, ALength, True, AIndex)
            end
            else
              Result := True;
          end
          else
            Result := True;
          Break;
        end;
      '-':
        begin
          if vHasMinus or vHasDigits then
          begin
            Result := True;
            Break;
          end;
          vHasMinus := True;
        end;
      '+':
        begin
          if vHasPlus or vHasDigits then
          begin
            Result := True;
            Break;
          end;
          vHasPlus := True;
        end;
      '.':
        begin
          if vHasPoint or AIsReadExponent then
          begin
            Result := True;
            Break;
          end;
          vHasPoint := True;
        end;
      ' ':
        begin
          if vHasDigits then
          begin
            Result := True;
            Break;
          end;
        end;
    else
      Result := True;
      Break;
    end;
    Inc(AIndex);
  end;
end;

procedure SetStringsLineBreak(const AStrings: TsgStringList;
  const AText: string; ALineBreak: Char = cnstLineBreak);
begin
  if (Length(AText) > 0) and (sgStringScan(ALineBreak, AText, 1) < 1) then
  begin
    if (ALineBreak <> cnstLineBreak) and
     (sgStringScan(cnstLineBreak, AText, 1) > 0) then
      ALineBreak := cnstLineBreak
    else
      ALineBreak := cnstLineBreakDef;
  end;
  AStrings.LineBreak := ALineBreak;
end;

procedure SetStringsText(const AStrings: TsgStringList; const AText: string);
begin
  AStrings.Text := AText;
  if AStrings.LineBreak = cnstLineBreakDef then
    CheckStringParams(AStrings);
end;

function sgConvToFloat(const AString: string; const ARes: PInteger = nil): Double;
var
  I, vLenght: Integer;
  vString: string;
  vValue: Extended;
  vConvert: Boolean;
begin
  Result := 0;
  if ARes <> nil then
    ARes^ := -1;
  vLenght := Length(AString);
  vString := Copy(AString, 1, vLenght);
  if vLenght > 0 then
  begin
    I := 1;
    while ScanDigitsInString(vString, vLenght, False, I) do
    begin
      vString := Copy(vString, 1, I - 1);
      I := 1;
      vLenght := Length(vString);
    end;
    if Length(vString) > 0 then
    begin
      vConvert := SysUtils.TextToFloat(PChar(vString), vValue, fvExtended);
      if vConvert then
      begin
//        if vValue > fMaxDouble then
//          vValue := fMaxDouble
//        else
//          if vValue < fMinDouble then
//            vValue := fMinDouble;
        Result := vValue;
      end;
      if ARes <> nil then
        ARes^ := Ord(vConvert);
    end;
  end
end;

function BoolToStr(const AValue: Boolean): string;
begin
  if AValue then
    Result := cnstTrue
  else
    Result := cnstFalse;
end;

function FloatToString(const AValue: Double): string;
begin
  Result := sgDoubleToStr(AValue, cnstDecimalSeparatorFloat);
end;

function FloatRoundToStr(const AValue: Double; const ADigits: Integer): string;
var
  vValue: Double;
begin;
  vValue := sgRoundToDouble(AValue, ADigits);
  Result := FloatToString(vValue);
end;

function PointToStr(const APoint: TFPoint; const AIs3D: Boolean;
  const ASeparator: Char = cnstLineBreak): string;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := FloatToString(APoint.X) + ASeparator + FloatToString(APoint.Y);
  if AIs3D then
    Result := Result + ASeparator + FloatToString(APoint.Z);
end;

function PointRoundToStr(const APoint: TFPoint; const AIs3D: Boolean;
  const ADigits: Byte; const ASeparator: Char =  cnstLineBreak): string;
var
  vPoint: TFPoint;
begin
  vPoint.X := sgRoundToDouble(APoint.X, ADigits);
  vPoint.Y := sgRoundToDouble(APoint.Y, ADigits);
  if AIs3D then
    vPoint.Z := sgRoundToDouble(APoint.Z, ADigits)
  else
    vPoint.Z := 0;
  Result := PointToStr(vPoint, AIs3D, ASeparator);
end;

function FPointToStr(const AValue: TFPoint): string;
begin
  Result := PointToStr(AValue, AValue.Z <> 0, cnstDecimalSeparatorValues);
end;

function FPointRoundToStr(const AValue: TFPoint; const ADigits: Integer): string;
begin;
  Result := PointRoundToStr(AValue, AValue.Z <> 0, ADigits, cnstDecimalSeparatorValues);
end;

function F2DPointRoundToStr(const AValue: TF2DPoint; const ADigits: Integer): string;
begin
  Result := PointRoundToStr(MakeFPointFrom2D(AValue), False, ADigits, cnstDecimalSeparatorValues);
end;

function TPointToStr(const AValue: TPoint): string;
begin
  Result := IntToStr(AValue.X) + cnstDecimalSeparatorValues +
    IntToStr(AValue.Y);
end;

function TRectToStr(const ARect: TRect): string;
begin
  Result := TPointToStr(ARect.TopLeft) + cnstDecimalSeparatorValues +
    TPointToStr(ARect.BottomRight);
end;

function CheckStringFPoint(const AString: string): string;
var
  vPoint: TFPoint;
begin
  Result := '';
  if Length(AString) > 0 then
  begin
    try
      vPoint := StrToFPoint(AString);
      Result := FPointToStr(vPoint);
    except
      Result := '';
    end;
  end;
end;

function CheckStringInteger(const AString: string): string;
var
  vInt: Integer;
begin
  Result := '';
  if Length(AString) > 0 then
  begin
    try
      vInt := StrToInt(AString);
      Result := IntToStr(vInt);
    except
      Result := '';
    end;
  end;
end;

function CheckStringDouble(const AString: string): string;
var
  vDbl: Double;
begin
  Result := '';
  if Length(AString) > 0 then
  begin
    try
      vDbl := StrToFloat(AString);
      Result := sgDoubleToStr(vDbl, '.');
    except
      Result := '';
    end;
  end;
end;

function HandleToStr(const AValue: UInt64): string;
begin
  Result := '$' + IntToHex(AValue, 0);
end;

function PointerToStr(const AValue: Pointer): string;
begin
  Result := '$' + IntToHex({$IFDEF SG_CPUX64}UInt64(AValue), 16{$ELSE}Cardinal(AValue), 8{$ENDIF});
end;

function VersionToStr(const AValue: TsgVersion): string;
begin
  Result := IntToStr(AValue.Major) + cnstDecimalSeparatorVersion +
    IntToStr(AValue.Minor);
end;

function StrHexToColor(const AString: string): TsgColor;
var
  vStringColor: string;
begin
  if (Length(AString) = 4) and (AString[1] = '#') then
    vStringColor := AString[1] + AString[2] + AString[2] +
      AString[3] + AString[3] + AString[4] + AString[4]
  else
    vStringColor := AString;
  Result.Color := StrHexToInt(vStringColor);
end;

function StrHexToInt(const AString: string): UInt64;
var
  vHexValue: string;
begin
  Result := 0;
  vHexValue := GetHexValue(AString);
  if Length(vHexValue) > 0 then
    Result := StrToInt64Def(vHexValue, 0);
end;

function StrRGBToColor(const AString: string): TsgColor;
var
  I, J: Integer;
  vStringColor: string;
  IsPercent: Boolean;
  C: Char;
begin
  Result.Color := 0;
  J := 3;
  IsPercent := sgStringPos('%', AString, 1) > 0;
  for I := 0 to 2 do
  begin
    vStringColor := '';
    Inc(J);
    while J <= Length(AString) do
    begin
      C := AString[J];
      case C of
        cnstDecimalSeparatorFloat:
          begin
            vStringColor := vStringColor + C;
          end;
        ',':
          begin
            Break;
          end;
      else
        if {$IFDEF SGDEL_XE4}C.IsDigit{$ELSE}IsDigit(C){$ENDIF}then
          vStringColor := vStringColor + C;
      end;
      Inc(J);
    end;
    if IsPercent then
      Result.V[2 - I] := Round(255 * StrToVal(vStringColor) / 100.0)
    else
      Result.V[2 - I] := Round(StrToVal(vStringColor));
  end;
end;

function StrToColor(const AString: string): Integer;
var
  vColor: TsgColor;
  I: Integer;
  vBuffer: Byte;
  vStringColor, vValue: string;
begin
  vValue := AnsiLowerCase(AString);
  if sgStringPos(cnstRGB, vValue, 1) > 0 then
    vColor := StrRGBToColor(AString)
  else
  begin
    vStringColor := '';
    for I := 1 to Length(vValue) do
      case vValue[I] of
        '0'..'9', 'a'..'z', '#': vStringColor := vStringColor + vValue[I]
      else
        if Length(vStringColor) > 0 then
          Break;
      end;
    if Length(vStringColor) > 0 then
    begin
      if vStringColor[1] <> cnstColorHexPrefix then
        vColor := GetColorByName(vStringColor)
      else
        vColor := StrHexToColor(vStringColor);
    end
    else
      vColor.Color := 0;
  end;
  vBuffer := vColor.R;
  vColor.R := vColor.B;
  vColor.B := vBuffer;

  Result := vColor.Color;
end;

function StrToBool(const AString: string; var ABool: Boolean): Boolean;
var
  vValue: string;
begin
  ABool := False;
  Result := True;
  vValue := AnsiLowerCase(AString);
  if vValue = cnstTrue then
    ABool := True
  else
    if vValue <> cnstFalse then
      Result := False;
end;

function StringToFloat(const AString: string): Double;
var
  vDs: Char;
begin
  vDs := SetDecimalSeparator(cnstDecimalSeparatorFloat);
  try
    Result := sgConvToFloat(AString);
  finally
    SetDecimalSeparator(vDs);
  end;
end;

function StrToFPoint(const AString: string): TFPoint;
var
  vDs: Char;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter: TsgStringList;
{$ENDIF}
begin
  Result := cnstFPointZero;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter := TsgStringList.Create;
  try
{$ENDIF}
  sgStrToStrings(AString, cnstDecimalSeparatorValues, Spliter);
  if Spliter.Count > 0 then
  begin
    vDs := SetDecimalSeparator(cnstDecimalSeparatorFloat);
    try
      Result.X := sgConvToFloat(Spliter[0]);
      if Spliter.Count > 1 then
      begin
        Result.Y := sgConvToFloat(Spliter[1]);
        if Spliter.Count > 2 then
          Result.Z := sgConvToFloat(Spliter[2]);
      end;
    finally
      SetDecimalSeparator(vDs);
    end;
  end;
{$IFDEF SG_OPENING_IN_THEADS}
  finally
    Spliter.Free;
  end;
{$ENDIF}
end;

function StrToFRect(const AString: string; const AIs3D: Boolean = True): TFRect;
var
  I, J: Integer;
  vDs: Char;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter: TsgStringList;
{$ENDIF}
begin
  Result := cnstFRectZero;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter := TsgStringList.Create;
  try
{$ENDIF}
  sgStrToStrings(AString, cnstDecimalSeparatorValues, Spliter);
  vDs := SetDecimalSeparator(cnstDecimalSeparatorFloat);
  try
    I := 0;
    J := 0;
    while (I < Spliter.Count) and (J <= 5)  do
    begin
      Result.D[J] := sgConvToFloat(Spliter[I]);
      if not AIs3D then
      begin
        case J of
          1, 4:  Inc(J);
        end;
      end;
      Inc(J);
      Inc(I);
    end;
  finally
    SetDecimalSeparator(vDs);
  end;
{$IFDEF SG_OPENING_IN_THEADS}
  finally
    Spliter.Free;
  end;
{$ENDIF}
end;

function StrToF2DRect(const AString: string): TF2DRect;
begin
  Result := MakeF2dRectFromFRect(StrToFRect(AString, False));
end;

function StrToHandle(const AString: string): UInt64;
var
  vHexValue: string;
begin
  Result := 0;
  vHexValue := GetHexValue(AString);
  if Length(vHexValue) > 0 then
    Result := StrToInt64(vHexValue);
end;

function StrToPointer(const AString: string): Pointer;
var
  vHexValue: string;
  vValue: {$IFDEF SG_CPUX64}UInt64{$ELSE}Cardinal{$ENDIF};
begin
  Result := nil;
  vHexValue := GetHexValue(AString);
  if Length(vHexValue) > 0 then
  begin
    vValue := StrToInt(vHexValue);
    Result := Pointer(vValue);
  end;
end;

function StrToTPoint(const AString: string): TPoint;
{$IFDEF SG_OPENING_IN_THEADS}
var
  Spliter: TsgStringList;
{$ENDIF}
begin
  Result := cnstPointZero;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter := TsgStringList.Create;
  try
{$ENDIF}
  sgStrToStrings(AString, cnstDecimalSeparatorValues, Spliter);
  if Spliter.Count > 0 then
  begin
    Result.X := StrToIntDef(Spliter[0], 0);
    if Spliter.Count > 1 then
      Result.Y := StrToIntDef(Spliter[1], 0);
  end;
{$IFDEF SG_OPENING_IN_THEADS}
  finally
    Spliter.Free;
  end;
{$ENDIF}
end;

function StrToTRect(const AString: string): TRect;
var
  vValues: array[0..3] of Integer absolute Result;
  I: Integer;
  vDs: Char;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter: TsgStringList;
{$ENDIF}
begin
  Result := cnstRectZero;
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter := TsgStringList.Create;
  try
{$ENDIF}
  sgStrToStrings(AString, cnstDecimalSeparatorValues, Spliter);
  vDs := SetDecimalSeparator(cnstDecimalSeparatorFloat);
  try
    I := 0;
    while (I < Spliter.Count) and (I <= 3)  do
    begin
      if Length(Spliter[I]) > 0 then
        vValues[I] := StrToIntDef(Spliter[I], 0);
      Inc(I);
    end;
  finally
    SetDecimalSeparator(vDs);
  end;
{$IFDEF SG_OPENING_IN_THEADS}
  finally
    Spliter.Free;
  end;
{$ENDIF}
end;

function StrToVal(const AString: string): Double;
var
  vDs: Char;
begin
  vDs := SetDecimalSeparator(cnstDecimalSeparatorFloat);
  try
    Result := sgConvToFloat(AString);
  finally
    SetDecimalSeparator(vDs);
  end;
end;

function StrToVersion(const AString: string): TsgVersion;
{$IFDEF SG_OPENING_IN_THEADS}
var
  Spliter: TsgStringList;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
{$IFDEF SG_OPENING_IN_THEADS}
  Spliter := TsgStringList.Create;
  try
{$ENDIF}
  sgStrToStrings(AString, cnstDecimalSeparatorVersion, Spliter);
  if Spliter.Count > 0 then
  begin
    Result.Major := StrToIntDef(Spliter[0], 0);
    if Spliter.Count > 1 then
      Result.Minor := StrToIntDef(Spliter[1], 0);
  end;
{$IFDEF SG_OPENING_IN_THEADS}
  finally
    Spliter.Free;
  end;
{$ENDIF}
end;

function StrToPointsList(const AString: string; const AIs3D: Boolean;
  const AList: Pointer; const AAddProc: TsgProcOfListAddPoint;
  const ASeparator: Char = cnstLineBreak): Integer;
var
  I, J: Integer;
  vStrings: TsgStringList;
  vPoint: TFPoint;
begin
  Result := 0;
  if Length(AString) > 0 then
  begin
    vStrings := TsgStringList.Create;
    try
      sgStrToStrings(AString, ASeparator, vStrings);
      I := 0;
      while I < vStrings.Count do
      begin
        J := 0;
        vPoint := cnstFPointZero;
        while J < cnstCountValue[AIs3D] do
        begin
          if (I < vStrings.Count) and (Length(vStrings[I]) > 0) then
          begin
            vPoint.V[J] := sgConvToFloat(vStrings[I]);
            Inc(I);
          end
          else
            Break;
          Inc(J);
        end;
        if J > 0 then
        begin
          AAddProc(AList, vPoint);
          Inc(Result);
        end
        else
          Break;
      end;
    finally
      vStrings.Free;
    end;
  end;
end;

procedure CheckSepatator(var AStr: string;
  const ASeparator: Char =  cnstLineBreak);
var
  vLength: Integer;
begin
  vLength := Length(AStr);
  if (vLength > 0) and (AStr[vLength] = ASeparator) then
    SetLength(AStr, vLength - 1);
end;

function PointsListToStr(const AIs3D: Boolean; const AList: TObject;
  const ACount: Integer; const GetProc: TsgProcOfListGetPoint;
  const ASeparator: Char = cnstLineBreak): string; overload;
var
  I: Integer;
  vStr: string;
begin
  Result := '';
  for I := 0 to ACount - 1 do
  begin
    vStr := PointToStr(GetProc(AList, I), AIs3D, ASeparator);
    Result := Result + ASeparator;
  end;
  CheckSepatator(Result, ASeparator);
end;

function PointsListToStr(const AIs3D: Boolean; AList: IsgArrayFPoint;
  const ASeparator: Char = cnstLineBreak): string; overload;
var
  I: Integer;
  vStr: string;
begin
  Result := '';
  for I := 0 to AList.FPointCount - 1 do
  begin
    vStr := PointToStr(AList.FPoints[I], AIs3D, ASeparator);
    Result := Result + ASeparator;
  end;
  CheckSepatator(Result, ASeparator);
end;


function FRectToStr(const ARect: TFRect; const AIs3D: Boolean = True): string;
var
  vTopLeft, vBottomRight: string;
begin
  vTopLeft := PointToStr(ARect.TopLeft, AIs3D, cnstDecimalSeparatorValues);
  vBottomRight := PointToStr(ARect.BottomRight, AIs3D, cnstDecimalSeparatorValues);
  Result := vTopLeft + cnstDecimalSeparatorValues + vBottomRight;
end;

function FRectRoundToStr(const ARect: TFRect; const ADigits: Integer;
  const AIs3D: Boolean = True): string;
var
  vTopLeft, vBottomRight: string;
begin
  vTopLeft := PointRoundToStr(ARect.TopLeft, AIs3D, ADigits, cnstDecimalSeparatorValues);
  vBottomRight := PointRoundToStr(ARect.BottomRight, AIs3D, ADigits, cnstDecimalSeparatorValues);
  Result := vTopLeft + cnstDecimalSeparatorValues + vBottomRight;
end;

function F2DRectToStr(const ARect: TF2DRect): string;
begin
  Result := FRectToStr(MakeFRectFromF2dRect(ARect), False);
end;

function F2DRectRoundToStr(const ARect: TF2DRect; const ADigits: Integer): string;
begin
  Result := FRectRoundToStr(MakeFRectFromF2dRect(ARect), ADigits, False);
end;

function FMatrixToStr(const AM: TFMatrix; const AIs3D: Boolean = True): string;
var
  EX, EY, EZ, EO: string;
begin
  if AIs3D then
  begin
    EX := PointToStr(AM.EX, True, cnstDecimalSeparatorValues);
    EY := PointToStr(AM.EY, True, cnstDecimalSeparatorValues);
    EZ := PointToStr(AM.EZ, True, cnstDecimalSeparatorValues);
    EO := PointToStr(AM.E0, True, cnstDecimalSeparatorValues);
    Result := EX + cnstDecimalSeparatorValues +
      EY +  cnstDecimalSeparatorValues + EZ +  cnstDecimalSeparatorValues + EO;
  end
  else
  begin
    EX := PointToStr(AM.EX, False, cnstDecimalSeparatorValues);
    EY := PointToStr(AM.EY, False, cnstDecimalSeparatorValues);
    EO := PointToStr(AM.E0, False, cnstDecimalSeparatorValues);
    Result := EX + cnstDecimalSeparatorValues +
      EY +  cnstDecimalSeparatorValues + EO;
  end;
end;

function StrToFMatrix(const AValue: string): TFMatrix;
var
  vStrings: TsgStringList;
  I, V, P: Integer;
  vValue: Double;
  vIs3d: Boolean;
begin
  Result := cnstIdentityMat;
  if Length(AValue) > 0 then
  begin
    vStrings := TsgStringList.Create;
    try
      sgStrToStrings(AValue, cnstDecimalSeparatorValues, vStrings);
      if vStrings.Count in [6, 12] then
      begin
        vIs3d := vStrings.Count = 12;
        P := 0;
        V := 0;
        for I := 0 to vStrings.Count - 1 do
        begin
          vValue := sgConvToFloat(vStrings[I]);
          Result.V[P].V[V] := vValue;
          Inc(V);
          if vIs3d then
          begin
            if V > 2 then
            begin
              V := 0;
              Inc(P);
            end;
          end
          else
          begin
            if V > 1 then
            begin
              V := 0;
              Inc(P);
              if P > 1 then
                P := 3;
            end;
          end;
        end;
      end;
    finally
      vStrings.Free;
    end;
  end;
end;

function StrToSingleList(const AString: string; const AList: Pointer;
  const AAddProc: TsgProcOfListAddSingle; const ASeparator: Char = cnstLineBreak): Integer;
var
  I: Integer;
  vStrings: TsgStringList;
  vValue: Single;
begin
  Result := 0;
  if Length(AString) > 0 then
  begin
    vStrings := TsgStringList.Create;
    try
      sgStrToStrings(AString, ASeparator + cnstLineBreakDef, vStrings);
      I := 0;
      while I < vStrings.Count do
      begin
        vValue := sgConvToFloat(vStrings[I]);
        AAddProc(AList, vValue);
        Inc(Result);
        Inc(I);
      end;
    finally
      vStrings.Free;
    end;
  end;
end;

function SingleListToStr(const AList: TObject; const ACount: Integer;
  const GetProc: TsgProcOfListGetSingle; const ASeparator: Char = cnstLineBreak): string;
var
  I: Integer;
  vItem: Single;
  vSeparator: string;
begin
  Result := '';
  for I := 0 to ACount - 1 do
  begin
    vSeparator := ASeparator + cnstLineBreakDef;
    vItem := GetProc(AList, I);
    Result := Result + ValToStr(vItem) + vSeparator;
  end;
end;

function TryStrToFPoint(const AString: string; P: PFPoint = nil): Boolean;
var
  S, V: string;
  Pt: TFPoint;
  Err: Integer;
begin
  Result := False;
  S := AString;
  sgReplaceAnsi(S, cnstDecimalSeparatorValues, #0);
  if P = nil then P := @Pt;
  V := string(PChar(S));
  Val(V, P^.X, Err);
  if Err = 0 then
  begin
    S := Copy(S, Length(V) + 2, MaxInt);
    V := string(PChar(S));
    Val(V, P^.Y, Err);
    if Err = 0 then
    begin
      S := Copy(S, Length(V) + 2, MaxInt);
      Val(S, P^.Z, Err);
      if Err = 0 then
        Result := True
      else
        Result := S = '';
    end;
  end;
end;

function ValToStr(const ADouble: Double): string;
begin
  Result := FloatToString(ADouble);
end;

{$IFDEF SG_PARSER_DEBUG}
procedure AddNode(const ANode: TsgNode; const ATreeNode: TTreeNode;
  const ATree: TTreeView);
var
  I: Integer;
  vNewTreeNode: TTreeNode;
  vNodeText, vNodeName, vNodeValue: string;
  vAttrNode: TsgNode;
begin
  vNodeText := ANode.Name;
  if Length(ANode.Value) > 0 then
    vNodeText := vNodeText + ' = ' + ANode.Value;
  if Length(ANode.Text) > 0 then
    vNodeText := vNodeText + ' : ' + ANode.Text;
  vNodeText := vNodeText + ' :: ' +  cnstNodeTypeNames[ANode.NodeType];
  vNewTreeNode := ATree.Items.AddChild(ATreeNode, vNodeText);
  if ANode.HasAttributeNodes then
    for I := 0 to ANode.AttributeNodesCount - 1 do
    begin
      vAttrNode := TsgNode(ANode.AttributeNodes[I]);
      vNodeName := vAttrNode.Name;
      vNodeValue := vAttrNode.Value;
      ATree.Items.AddChild(vNewTreeNode, '[' + vNodeName +
        ' = ' + vNodeValue + ']');
    end;
  if ANode.HasChildNodes then
    for I := 0 to ANode.ChildNodesCount - 1 do
      AddNode(TsgNode(ANode.ChildNodes[I]), vNewTreeNode, ATree);
end;

function TestParser(const AFileName: string; const Tree: TTreeView): DWORD;
var
  I: Integer;
  vParser: TsgParser;
begin
  vParser := TsgParser.Create;
  try
    Result := GetTickCount;
    vParser.LoadFromFile(AFileName);
    Result := GetTickCount - Result;
    for I := 0 to vParser.ROOT.Count - 1 do
      AddNode(TsgNode(vParser.ROOT[I]), nil, Tree);
  finally
    vParser.Free;
  end;
end;
{$ENDIF}

{TsgBase64}

procedure TsgBase64.EnCode(const AInStream: TStream;
  const AOutStream: TStream);
{$IFDEF SG_FIREMONKEY}
begin
  System.NetEncoding.TNetEncoding.Base64.Encode(AInStream, AOutStream);
{$ELSE}
const
  cnstColumnWidth = 75;
type
  PInteger = ^Integer;
var
  vInBuffer: array[0..509] of Byte;
  vOutBuffer: array[0..1023] of AnsiChar;
  vBuffer: PAnsiChar;
  I, J, K, BytesRead: Integer;
  vBlock: TsgParametrs;
begin
  K := 0;
  repeat
    BytesRead := AInStream.Read(vInBuffer, SizeOf(vInBuffer));
    I := 0;
    vBuffer := vOutBuffer;
    while I < BytesRead do
    begin
      if BytesRead - I < 3 then
        J := BytesRead - I
      else J := 3;
      vBlock.Parametrs := 0;
      vBlock.Param1 := vInBuffer[I];
      if J > 1 then
        vBlock.Param2 := vInBuffer[I + 1];
      if J > 2 then
        vBlock.Param3 := vInBuffer[I + 2];
      vBuffer[0] := EncodeBase64Table[vBlock.Params[0] shr 2];
      vBuffer[1] := EncodeBase64Table[((vBlock.Params[0] shl 4) or (vBlock.Params[1] shr 4)) and $0000003f];
      if J < 2 then
        vBuffer[2] := '='
      else vBuffer[2] := EncodeBase64Table[((vBlock.Params[1] shl 2) or (vBlock.Params[2] shr 6)) and $0000003f];
      if J < 3 then
        vBuffer[3] := '='
      else vBuffer[3] := EncodeBase64Table[vBlock.Params[2] and $0000003f];
      (*FillChar(vBuffer[2],2,'=');
      if J >= 2 then
      begin
        vBuffer[2] := EncodeBase64Table[((vBlock.Params[1] shl 2) or (vBlock.Params[2] shr 6)) and $0000003f];
        if J = 3 then
          vBuffer[3] := EncodeBase64Table[vBlock.Params[2] and $0000003f];
      end;
      *)
      Inc(I, 3);
      Inc(vBuffer, 4);
      Inc(K, 4);
      if K > cnstColumnWidth then
      begin
        vBuffer[0] := #$0D;
        vBuffer[1] := #$0A;
        Inc(vBuffer, 2);
        K := 0;
      end;
    end;
    AOutStream.Write(vOutBuffer, vBuffer - PAnsiChar(@vOutBuffer));
  until BytesRead = 0;
{$ENDIF}
end;

{$IFNDEF SG_FIREMONKEY}
function TsgBase64.Next: Char;
begin
  repeat
    Result := #0;
    if FCol > Length(FBuffer) then begin
      Inc(FRow);
      if FRow >= FRMax then
        Exit;
      FCol := 1;
      FBuffer := FStrs[FRow];
      if FBuffer = '' then
        Continue;
    end;
    Result := FBuffer[FCol];
    Inc(FCol);
  until CharInSet(Result, ['+', '/', '0'..'9', 'A'..'Z', 'a'..'z']);
end;
{$ENDIF}

procedure TsgBase64.Decode(const AStr: string; AStrs: TStrings; const S: TStream);
{$IFDEF SG_FIREMONKEY}
var
  vStream: TMemoryStream;
//  vBuffer: TStrings;
begin
  vStream := TMemoryStream.Create;
  try
    if (AStrs <> nil) and (AStrs.Count > 0) then
      AStrs.SaveToStream(vStream)
    else
    begin
//      vBuffer := TStrings.Create;
//      try
//        vBuffer.Add(AStr);
//        vBuffer.SaveToStream(vStream);
//      finally
//        vBuffer.Free;
//      end;
      if AStr.Length > 0 then
        vStream.Write(AStr[1], AStr.Length * SizeOf(Char));
    end;
    vStream.Position := 0;
    if vStream.Size > 0 then
      System.NetEncoding.TNetEncoding.Base64.Decode(vStream, S);
  finally
    vStream.Free;
  end;
{$ELSE}
{
Initial data - or single line Str, or the list of lines Strs
The result is written in stream S or (at S=nil) is refunded as a line
}
var
  I, Count, Index, Read: Integer;
  Ch: Char;
  Buf: array[0..3076] of AnsiChar;
  pBuf: PInteger;
begin
  Count := 0;
  FRow := 0;
  FCol := 1;
  FRMax := 0;
  FBuffer := AStr;
  FStrs := AStrs;
  if (FStrs <> nil) and (FStrs.Count > 0) then
  begin
    FRMax := FStrs.Count;
    FBuffer := FStrs[0];
  end;
  repeat
    if Count >= 3072 then
    begin
      if S <> nil then
        S.WriteBuffer(Buf[0], Count);
      Count := 0;
    end;
    pBuf := @Buf[Count];
    pBuf^ := 0;
    Read := 0;
    I := 0;
    while I <= 3 do
    begin
      Ch := Next;
      case Ch of
      'A'..'Z':  Index := Ord(Ch) - Ord('A');
      'a'..'z':  Index := Ord(Ch) - Ord('a') + 26;
      '0'..'9':  Index := Ord(Ch) - Ord('0') + 52;
      '+':	   Index := 62;
      '/':	   Index := 63;
      else	   Index := 0;
      end;
      if Ch <> #0 then
        Inc(Read);
      pBuf^ := pBuf^ shl 6 or Index;
      Inc(I);
    end;
    pBuf^ := sgSwapWords(pBuf^) shr 8;
    Inc(Count, 3);
  until Read < 4;
  case Read of
    0:	   Dec(Count, 3);
    1, 2:  Dec(Count, 2);
    3:     Dec(Count);
  end;
  if Count < 0 then
    Exit;
  if S <> nil then
    S.WriteBuffer(Buf[0], Count);
{$ENDIF}
end;

{ TsgNodeList }

function TsgNodeList.GetCaseSensitivity: Boolean;
begin
  Result := (FFlags and 2) <> 0;
end;

function TsgNodeList.GetNode(const AIndex: Integer): TsgNodeSample;
begin
  Result := TsgNodeSample(Self.Items[AIndex]);
end;

function TsgNodeList.GetNodeByName(const AName: string): TsgNodeSample;
var
  I: Integer;
  vFind: Boolean;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
  vSourceNameIndex, vItemNameIndex: Integer;
{$ENDIF}
  vNode: TsgNodeSample;
  vCaseSensitivity: Boolean;
begin
  Result := nil;
  if QwickFind then
  begin
    I := FSortedList.IndexOf(AName);
    if I > -1 then
      Result := TsgNodeSample(FSortedList.Objects[I]);
  end
  else
  begin
    vCaseSensitivity := CaseSensitivity;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
    vSourceNameIndex := -1;
    if Count > 8 then
    begin
      if GetNameIndexesCaseSensitivity = vCaseSensitivity then
        vSourceNameIndex := GetNameIndex(AName, False);
    end;
{$ENDIF}
    for I := 0 to Count - 1 do
    begin
      vNode := TsgNodeSample(Self.Items[I]);
      if (vNode.NodeType in [ntElement, ntAttribute]) then
      begin
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
        if (vSourceNameIndex > -1) then
        begin
          vItemNameIndex := vNode.GetIndexName;
          if vItemNameIndex > -1 then
            vFind := vItemNameIndex = vSourceNameIndex
          else
            vFind := sgAnsiCmprStr(vNode.Name, AName, vCaseSensitivity) = 0;
        end
        else
{$ENDIF}
          vFind := sgAnsiCmprStr(vNode.Name, AName, vCaseSensitivity) = 0;
        if vFind then
        begin
          Result := vNode;
          Break;
        end;
      end;
    end;
  end;
end;

function TsgNodeList.GetQwickFind: Boolean;
begin
  Result := FSortedList <> nil;
end;

procedure TsgNodeList.Notify(const Obj: TObject; Action: TListNotification);
var
  I: Integer;
begin
  inherited Notify(Obj, Action);
  if (Obj <> nil) and QwickFind then
  begin
    case Action of
      lnAdded:
        begin
          FSortedList.AddObject(TsgNodeSample(Obj).Name, TObject(Obj));
        end;
      lnDeleted:
        begin
          if FSortedList.Count > 0 then
          begin
            I := FSortedList.IndexOf(TsgNodeSample(Obj).Name);
            if (I > -1) and (FSortedList.Objects[I] <> TObject(Obj)) then
              I := -1;
            if I < 0 then
              I := FSortedList.IndexOfObject(TObject(Obj));
            if I > -1 then
              FSortedList.Delete(I);
          end;
        end;
    end;
  end;
end;

procedure TsgNodeList.UpdateNode(const ANode: TsgNodeSample);
var
  I: Integer;
begin
  if QwickFind then
  begin
    I := FSortedList.IndexOfObject(ANode);
    if I > -1 then
    begin
      FSortedList.Delete(I);
      FSortedList.AddObject(ANode.Name, ANode);
    end;
  end;
end;

procedure TsgNodeList.SetCaseSensitivity(const AValue: Boolean);
begin
  FFlags := (FFlags and 253) or (Byte(AValue) shl 1);
  if QwickFind then
    TsgStringList(FSortedList).CaseSensitive := AValue;
end;

procedure TsgNodeList.SetQwickFind(const AValue: Boolean);
begin
  if AValue then
  begin
    if FSortedList = nil then
      CreateSortedList;
  end
  else
    FreeAndNil(FSortedList);
end;

procedure TsgNodeList.SortNodes(const AProc: TsgObjProcCompare);
var
  vProcCompare: TsgObjProcCompare;
begin
  if Count > 1 then
  begin
    vProcCompare := ProcCompare;
    try
      ProcCompare := AProc;
      Sort(0, Count - 1);
    finally
      ProcCompare := vProcCompare;
    end;
  end;
end;

constructor TsgNodeList.Create;
begin
  inherited Create;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
//  SetCaseSensitivity(GetNameCaseSensitivity);
{$ENDIF}
end;

class function TsgNodeList.CreateNodeList(const AQwickFind: Boolean = False): TsgNodeList;
begin
  Result := TsgNodeList.Create;
  Result.QwickFind := AQwickFind;
end;

procedure TsgNodeList.CreateSortedList;
var
  I: Integer;
  vNode: TsgNodeSample;
begin
  FSortedList := TsgStringList.Create;
  TsgStringList(FSortedList).CaseSensitive := CaseSensitivity;
  FSortedList.Capacity := Count;
  FSortedList.Duplicates := dupAccept;
  for I := 0 to Count - 1 do
  begin
    vNode := TsgNodeSample(Self.Items[I]);
    FSortedList.AddObject(vNode.Name, vNode);
  end;
end;

destructor TsgNodeList.Destroy;
begin
  ClearNodes;
  FreeAndNil(FSortedList);
  inherited Destroy;
end;

procedure TsgNodeList.Assign(Source: TsgBaseList);
begin
  QwickFind := False;
  FreeAndNil(FSortedList);//free list whene sQwickFind = False
  if Source is TsgNodeList then
    FFlags := TsgNodeList(Source).FFlags;
  try
    inherited Assign(Source);
  finally
    if QwickFind then
      CreateSortedList;
  end;
end;

procedure TsgNodeList.ClearNodes;
begin
  if QwickFind then
    FSortedList.Clear;
  TsgObjectList.ClearList(Self);
  Count := 0;
end;

{ TsgNodeSample }

function TsgNodeSample.CheckText(const ANode: TsgNodeSample): Boolean;
begin
  Result := False;
end;

procedure TsgNodeSample.Clear;
begin
end;

function TsgNodeSample.Compare(const ANode: TsgNodeSample;
  const ACompareChilds: Boolean): Integer;
begin
  Result := CompareNodes(ANode, ACompareChilds, nil);
end;

function TsgNodeSample.CompareNodes(const ANode: TsgNodeSample;
  const ACompareChilds: Boolean; const AIgnoreAttribs: TStrings): Integer;
begin
  Result := 1;
  if ClassType = ANode.ClassType then
  begin
    Result := CompareText(Name, ANode.Name);
    if Result = 0 then
      Result := ValueData.Compare(ANode.ValueData, False);
    if Result = 0 then
      Result := CompareAttributes(ANode, AIgnoreAttribs);
    if (Result = 0) and ACompareChilds then
      Result := CompareChilds(ANode, AIgnoreAttribs);
  end;
end;

function TsgNodeSample.CompareAttributes(const ANode: TsgNodeSample;
  const AIgnoreAttribs: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if HasAttributeNodes then
  begin
    if ANode.HasAttributeNodes then
    begin
      if AttributeNodesCount = ANode.AttributeNodesCount then
      begin
        for I := 0 to AttributeNodesCount - 1 do
        begin
          if Assigned(AIgnoreAttribs) then
          begin
            if AIgnoreAttribs.IndexOf(AttributeNodes[I].Name) > -1 then
              Continue;
          end;
          Result := AttributeNodes[I].Compare(ANode.AttributeNodes[I], False);
          if Result <> 0 then
            Break;
        end;
      end
      else
      begin
        if AttributeNodesCount > ANode.AttributeNodesCount then
          Result := 1
        else
          Result := -1;
      end;
    end
    else
      Result := 1;
  end
  else
  begin
    if ANode.HasAttributeNodes then
      Result := -1;
  end;
end;

function TsgNodeSample.CompareChilds(const ANode: TsgNodeSample;
  const AIgnoreAttribs: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if HasChildNodes then
  begin
    if ANode.HasChildNodes then
    begin
      if ChildNodesCount = ANode.ChildNodesCount then
      begin
        for I := 0 to ChildNodesCount - 1 do
        begin
          Result := ChildNodes[I].CompareNodes(ANode.ChildNodes[I],
            True, AIgnoreAttribs);
          if Result <> 0 then
            Break;
        end;
      end
      else
      begin
        if ChildNodesCount > ANode.ChildNodesCount then
          Result := 1
        else
          Result := -1;
      end;
    end
    else
      Result := 1;
  end
  else
  begin
    if ANode.HasChildNodes then
      Result := -1;
  end;
end;

{$IFDEF SGDEL_2009}
function TsgNodeSample.GetAttributesEnumerable: IEnumerable<TsgNodeSample>;
begin
  Result := TsgNodeEnumerator.Create(nil);
end;

function TsgNodeSample.GetChildsEnumerable: IEnumerable<TsgNodeSample>;
begin
  Result := TsgNodeEnumerator.Create(nil);
end;
{$ENDIF}

function TsgNodeSample.GetAttributeNode(const AIndex: Integer): TsgNodeSample;
begin
  Result := nil;
end;

function TsgNodeSample.GetAttributeNodesCount: Integer;
begin
  Result := 0;
end;

function TsgNodeSample.GetChildByName(const AName: string): TsgNodeSample;
begin
  Result := nil;
end;

function TsgNodeSample.GetChildByPathName(const APathName: string;
  const ADelimiter: string = cnstXMLPathDelimiter): TsgNodeSample;
var
  vNode, vNodeItem: TsgNodeSample;
  I, vCnt: Integer;
  vNames: TsgStringList;
begin
  Result := nil;
  vNames := TsgStringList.Create;
  try
    vNode := Self;
    vNames.LineBreak := ADelimiter;
    vNames.Text := APathName;
    sgDeleteEmptyStrings(vNames);
    vCnt := vNames.Count - 1;
    for I := 0 to vCnt do
    begin
      vNodeItem := vNode.GetChildByName(vNames[I]);
      if vNodeItem <> nil then
      begin
        if I = vCnt then
          Result := vNodeItem
        else
          vNode := vNodeItem;
      end;
    end;
  finally
    vNames.Free;
  end;
end;

function TsgNodeSample.GetChildNode(const AIndex: Integer): TsgNodeSample;
begin
  Result := nil;
end;

function TsgNodeSample.GetChildNodesCount: Integer;
begin
  Result := 0;
end;

function TsgNodeSample.GetClassID: Cardinal;
begin
  Result := cnstClassID_XML;
end;

function TsgNodeSample.GetClosed: Boolean;
begin
  Result := True;
end;


function TsgNodeSample.GetCountOwner: Integer;
var
  vOwner: TsgNodeSample;
begin
  Result := 0;
  vOwner := FOwner;
  while Assigned(vOwner) do
  begin
    vOwner := vOwner.FOwner;
    Inc(Result);
  end;
end;

function TsgNodeSample.GetDataInternal(const AOwner: TObject): TObject;
begin
  Result := nil;
end;

function TsgNodeSample.GetFullName: string;
var
  vPrefix: string;
begin
  vPrefix := Prefix;
  if Length(vPrefix) = 0 then
    Result := Name
  else
    Result := vPrefix + cnstSeparatorFullName + Name;
end;

function TsgNodeSample.GetPosition: TsgNodeSamplePosition;
begin
  Result := FPosition;
end;

function TsgNodeSample.GetName: string;
begin
  Result := ClassName;
  Delete(Result, 1, 7);
end;

{$IFDEF SG_USE_DICTIONARY_BY_NAME}
function TsgNodeSample.GetIndexName: Integer;
begin
  Result := -1;
end;
{$ENDIF}

function TsgNodeSample.GetNodeType: TsgNodeType;
begin
  Result := ntUndefined;
end;

function TsgNodeSample.GetOwnerList(const AType: TsgNodeType): TsgNodeList;
begin
  Result := nil;
end;

function TsgNodeSample.GetPostamble: Boolean;
begin
  Result := False;
end;

function TsgNodeSample.GetPrefix: string;
begin
  Result := '';
end;

function TsgNodeSample.GetSaveData(var AName, AValue, AText: string): Integer;
begin
  Result := 0;
  AName := Name;
  AValue := ValueAsStr;
  AText := TextAsStr;
  if cnstExportXmlWeb then
  begin
    AName := ReplaceAnsiCodesEx(AName);
    if Length(AValue) > 0 then//ValueAsText
    begin
      AValue := ReplaceASCICode(AValue, True);
      AValue := ReplaceAnsiCodesEx(AValue);
    end;
    if Length(AText) > 0 then//TextAsText
    begin
      AText := ReplaceASCICode(AText, True);
      AText := ReplaceAnsiCodesEx(AText);
    end;
  end;
  if Length(AValue) > 0 then
  begin
    Result := 1;
    AValue := '="' + AValue + '"';
  end
  else
    AValue := '=""';
end;

function TsgNodeSample.GetSpacesBySave: string;
var
  I: Integer;
begin
  I := GetCountOwner;
  Result := StringOfChar(' ', I * 2);
end;

function TsgNodeSample.GetText: string;
begin
  Result := GetTextAsStr;
end;

function TsgNodeSample.GetTextAsBool: Boolean;
begin
  Result := TsgData.GetAsBool(TextData);
end;

function TsgNodeSample.GetTextAsDouble: Double;
begin
  Result := TsgData.GetAsDouble(TextData);
end;

function TsgNodeSample.GetTextAsF2DRect: TF2DRect;
begin
  Result := TsgData.GetAsF2DRect(TextData);
end;

function TsgNodeSample.GetTextAsFPoint: TFPoint;
begin
  Result := TsgData.GetAsFPoint(TextData);
end;

function TsgNodeSample.GetTextAsFRect: TFRect;
begin
  Result := TsgData.GetAsFRect(TextData);
end;

function TsgNodeSample.GetTextAsInt: Integer;
begin
  Result := TsgData.GetAsInt(TextData);
end;

function TsgNodeSample.GetTextAsInt64: Int64;
begin
  Result := TsgData.GetAsInt64(TextData);
end;

function TsgNodeSample.GetTextAsPointer: Pointer;
begin
  Result := TsgData.GetAsPointer(TextData);
end;

function TsgNodeSample.GetTextAsStr: string;
begin
  Result := TsgData.GetAsStr(TextData);
end;

function TsgNodeSample.GetTextAsText: string;
begin
  Result := TsgData.GetAsText(TextData);
end;

function TsgNodeSample.GetTextData: TsgData;
begin
  Result := nil;
end;

function TsgNodeSample.GetTextAsHandle: UInt64;
begin
  Result := TsgData.GetAsHandle(TextData);
end;

function TsgNodeSample.GetUnitsOfMeasurements: TsgUnitsOfMeasurements;
var
  vValueData: TsgData;
begin
  Result := umUndefined;
  vValueData := ValueData;
  if Assigned(vValueData) then
    Result := vValueData.Measurements;
end;

function TsgNodeSample.GetValue: string;
begin
  Result := GetValueAsStr;
end;

function TsgNodeSample.GetValueAsBool: Boolean;
begin
  Result := TsgData.GetAsBool(ValueData);
end;

function TsgNodeSample.GetValueAsDouble: Double;
begin
  Result := TsgData.GetAsDouble(ValueData);
end;

function TsgNodeSample.GetValueAsF2DRect: TF2DRect;
begin
  Result := TsgData.GetAsF2DRect(ValueData);
end;

function TsgNodeSample.GetValueAsF2DPoint: TF2DPoint;
begin
  Result := TsgData.GetAsF2DPoint(ValueData);
end;

function TsgNodeSample.GetValueAsFPoint: TFPoint;
begin
  Result := TsgData.GetAsFPoint(ValueData);
end;

function TsgNodeSample.GetValueAsFRect: TFRect;
begin
  Result := TsgData.GetAsFRect(ValueData);
end;

function TsgNodeSample.GetValueAsInt: Integer;
begin
  Result := TsgData.GetAsInt(ValueData);
end;

function TsgNodeSample.GetValueAsInt64: Int64;
begin
  Result := TsgData.GetAsInt64(ValueData);
end;

function TsgNodeSample.GetValueAsPointer: Pointer;
begin
  Result := TsgData.GetAsPointer(ValueData);
end;

function TsgNodeSample.GetValueAsStr: string;
begin
  Result := TsgData.GetAsStr(ValueData);
end;

function TsgNodeSample.GetValueAsText: string;
begin
  Result := TsgData.GetAsText(ValueData);
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
function TsgNodeSample.GetValueAsVariant: Variant;
begin
  Result := TsgData.GetAsVariant(ValueData);
end;

function TsgNodeSample.GetTextAsVariant: Variant;
begin
  Result := TsgData.GetAsVariant(TextData);
end;
{$ENDIF}

function TsgNodeSample.GetValueData: TsgData;
begin
  Result := nil;
end;

function TsgNodeSample.GetValueAsHandle: UInt64;
begin
  Result := TsgData.GetAsHandle(ValueData);
end;

procedure TsgNodeSample.Read(const AParser: TsgParser);
begin
end;

procedure TsgNodeSample.SetName(const AValue: string);
var
  vNodes: TsgNodeList;
begin
  vNodes := nil;
  if Length(Name) > 0 then
    vNodes := GetOwnerList(NodeType);
  ChangeName(AValue);
  if Assigned(vNodes) then
    vNodes.UpdateNode(Self);
end;

{$IFDEF SG_EXPORT_WITH_STRINGS}
procedure TsgNodeSample.SaveToStrings(const AStrings: TStrings);
var
  vAttribute: TsgNodeSample;
  I: Integer;
  vTmp, vSpaces, vName, vValue, vText, vAttrName, vAttrValue, vAttrText: string;
begin
  vSpaces := GetSpacesBySave;
  GetSaveData(vName, vValue, vText);
  vTmp := vSpaces + '<' + vName + vValue;
  for I := 0 to GetAttributeNodesCount - 1 do
  begin
    vAttribute := AttributeNodes[I];
    vAttribute.GetSaveData(vAttrName, vAttrValue, vAttrText);
    vTmp := vTmp + ' ' + vAttrName + '="' + vAttrValue + '"';
  end;
  if (GetChildNodesCount > 0) or (Length(TextAsStr) > 0) then
  begin
    vTmp := vTmp + '>' + vText;
    if GetChildNodesCount > 0 then
    begin
      AStrings.AddObject(vTmp, Self);
      try
        vTmp := '';
        for I := 0 to GetChildNodesCount - 1 do
          ChildNodes[I].SaveToStrings(AStrings);
      finally
        AStrings.AddObject(vSpaces + '</' + vName + '>', Self);
      end;
    end
    else
      AStrings.AddObject(vTmp + '</' + vName + '>', Self);
  end
  else
    AStrings.AddObject(vTmp + '/>', Self);
end;
{$ELSE}

class function TsgNodeSample.SameNodeNames(const A, B: Pointer): Integer;
var
  vName1, vName2: string;
begin
  vName1 := '';
  vName2 := '';
  if Assigned(A) then
    vName1 := TsgNodeSample(PPointer(A)^).Name;
  if Assigned(B) then
    vName2 := TsgNodeSample(PPointer(B)^).Name;
  Result := CompareText(vName1, vName2);
end;

procedure TsgNodeSample.SaveString(const AStream: TStream; const AString: string);
{$IFDEF SGDEL_2009}
var
  vStringStream: TStringStream;
begin
  if TsgParser.SaveEncoding <> TEncoding.Unicode then
  begin
    vStringStream := TStringStream.Create(AString, TsgParser.SaveEncoding, False);
    try
      vStringStream.SaveToStream(AStream);
    finally
      vStringStream.Free;
    end;
  end
  else
    AStream.WriteBuffer(Pointer(AString)^, Length(AString) * SizeOf(Char));
{$ELSE}
begin
  AStream.WriteBuffer(Pointer(AString)^, Length(AString) * SizeOf(Char));
{$ENDIF}
end;

procedure TsgNodeSample.SaveToStream(const AStream: TStream);
var
  vAttribute: TsgNodeSample;
  I: Integer;
  vTmp, vSpaces, vName, vValue, vText, vAtrName, vAtrValue, vAtrText: string;
begin
  vSpaces := GetSpacesBySave;
  GetSaveData(vName, vValue, vText);

  vTmp := vSpaces + '<' + vName + vValue;

  for I := 0 to GetAttributeNodesCount - 1 do
  begin
    vAttribute := AttributeNodes[I];
    vAttribute.GetSaveData(vAtrName, vAtrValue, vAtrText);
    vTmp := vTmp + ' ' + vAtrName + vAtrValue;
  end;

  if (GetChildNodesCount > 0) then
  begin
    vTmp := vTmp + '>' + sLineBreak;
    SaveString(AStream, vTmp);
    try
      vTmp := '';
      for I := 0 to GetChildNodesCount - 1 do
        ChildNodes[I].SaveToStream(AStream);
    finally
      vTmp := vSpaces + '</' + vName + '>' + sLineBreak;
    end;
  end
  else
  begin
    if Length(TextAsStr) > 0 then
    begin
      vTmp := vTmp + '>' + vText + '</' + vName + '>' + sLineBreak;
    end
    else
    begin
      vTmp := vTmp + '/>' + sLineBreak;
    end;
  end;
  SaveString(AStream, vTmp);
end;
{$ENDIF}

procedure TsgNodeSample.SetClosed(const AValue: Boolean);
begin
end;

procedure TsgNodeSample.SetEntity(const Value: TObject);
begin
  FEntity := Value;
{$IFDEF SG_XML_DEBUG}
  if Assigned(FEntity) then
  begin
    try
      FEntityClassName := FEntity.ClassName;
    except
      FEntityClassName := 'EXP';
    end;
  end
  else
    FEntityClassName := 'BAD';
{$ENDIF}
end;

procedure TsgNodeSample.SetFullName(const AValue: string);
var
  vIndex, vLength: Integer;
begin
  vLength := Length(AValue);
  if vLength > 0 then
  begin
    vIndex := sgStringScan(cnstSeparatorFullName, AValue, 1);
    if vIndex > 0 then
    begin
      Prefix := Copy(AValue, 1, vIndex - 1);
      Name := Copy(AValue, vIndex + 1, vLength - vIndex);
    end
    else
    begin
      Name := AValue;
      Prefix := '';
    end;
  end
  else
  begin
    Name := '';
    Prefix := '';
  end;
end;

procedure TsgNodeSample.SetPosition(const AValue: TsgNodeSamplePosition);
begin
  FPosition := AValue;
end;

procedure TsgNodeSample.ChangeName(const AValue: string);
begin
end;

procedure TsgNodeSample.SetNodeType(const AValue: TsgNodeType);
begin
end;

procedure TsgNodeSample.SetPostamble(const AValue: Boolean);
begin
end;

procedure TsgNodeSample.SetPrefix(const AValue: string);
begin
end;

procedure TsgNodeSample.SetReader(const AReader: Pointer);
begin
end;

procedure TsgNodeSample.SetText(const AText: string);
begin
  SetTextAsStr(AText);
end;

procedure TsgNodeSample.SetTextAsBool(const AValue: Boolean);
begin
  TsgData.SetAsBool(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsDouble(const AValue: Double);
begin
  TsgData.SetAsDouble(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsF2DRect(const AValue: TF2DRect);
begin
  TsgData.SetAsF2DRect(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsFPoint(const AValue: TFPoint);
begin
  TsgData.SetAsFPoint(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsFRect(const AValue: TFRect);
begin
  TsgData.SetAsFRect(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsInt(const AValue: Integer);
begin
  TsgData.SetAsInt(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsInt64(const AValue: Int64);
begin
  TsgData.SetAsInt64(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsPointer(const AValue: Pointer);
begin
  TsgData.SetAsPointer(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsStr(const AValue: string);
begin
  TsgData.SetAsStr(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsText(const AValue: string);
begin
  TsgData.SetAsText(TextData, AValue);
end;

procedure TsgNodeSample.SetTextAsHandle(const AValue: UInt64);
begin
  TsgData.SetAsHandle(TextData, AValue);
end;

procedure TsgNodeSample.SetValue(const AValue: string);
begin
  SetValueAsStr(AValue);
end;

procedure TsgNodeSample.SetValueAsBool(const AValue: Boolean);
begin
  TsgData.SetAsBool(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsDouble(const AValue: Double);
begin
  TsgData.SetAsDouble(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsF2DRect(const AValue: TF2DRect);
begin
  TsgData.SetAsF2DRect(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsF2DPoint(const AValue: TF2DPoint);
begin
  TsgData.SetAsF2DPoint(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsFPoint(const AValue: TFPoint);
begin
  TsgData.SetAsFPoint(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsFRect(const AValue: TFRect);
begin
  TsgData.SetAsFRect(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsInt(const AValue: Integer);
begin
  TsgData.SetAsInt(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsInt64(const AValue: Int64);
begin
  TsgData.SetAsInt64(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsPointer(const AValue: Pointer);
begin
  TsgData.SetAsPointer(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsStr(const AValue: string);
begin
  TsgData.SetAsStr(ValueData, AValue);
end;

procedure TsgNodeSample.SetValueAsText(const AValue: string);
begin
  TsgData.SetAsText(ValueData, AValue);
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
procedure TsgNodeSample.SetValueAsVariant(const AValue: Variant);
begin
  TsgData.SetAsVariant(ValueData, AValue);
end;

procedure TsgNodeSample.SetTextAsVariant(const AValue: Variant);
begin
  TsgData.SetAsVariant(TextData, AValue);
end;
{$ENDIF}

procedure TsgNodeSample.SetValueAsHandle(const AValue: UInt64);
begin
  TsgData.SetAsHandle(ValueData, AValue);
end;

constructor TsgNodeSample.Create;
begin
  inherited Create;
  FPosition.X := -1;
  FPosition.Y := -1;
  FEntity := nil;
  FUserData := nil;
end;

procedure TsgNodeSample.Assign(const Obj: TObject);
begin
  if Obj is TsgNodeSample then
    FPosition := TsgNodeSample(Obj).FPosition;
end;

function TsgNodeSample.GetAttributeByName(const AName: string): TsgNodeSample;
begin
  Result := nil;
end;

function TsgNodeSample.GetAttributeByTagID(const ATag: TsgTagID): TsgNodeSample;
begin
  Result := GetAttributeByName(cnstKeyWordsStr[ATag]);
end;

function TsgNodeSample.GetData(const AReader, AOwner: TObject): TObject;
begin
  SetReader(AReader);
  Result := GetDataInternal(AOwner);
end;


function TsgNodeSample.HasAttribute(const AName: string): Boolean;
begin
  Result := GetAttributeByName(AName) <> nil;
end;

function TsgNodeSample.HasAttributeNodes: Boolean;
begin
  Result := GetAttributeNodesCount > 0;
end;

function TsgNodeSample.HasChildNodes: Boolean;
begin
  Result := GetChildNodesCount > 0;
end;

function TsgNodeSample.HasPosition: Boolean;
begin
  Result := (FPosition.X > -1) and (FPosition.Y > -1);
end;

function TsgNodeSample.IsEqual(const ANode: TsgNodeSample;
  const ACompareChilds: Boolean): Boolean;
begin
  Result := Compare(ANode, ACompareChilds) = 0;
end;

{ TsgNodeComment }

constructor TsgNodeComment.Create;
begin
  inherited Create;
  FValue := TsgData.Create;
end;

destructor TsgNodeComment.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

function TsgNodeComment.GetNodeType: TsgNodeType;
begin
  Result := ntComment;
end;

function TsgNodeComment.GetValueData: TsgData;
begin
  Result := FValue;
end;

procedure TsgNodeComment.Read(const AParser: TsgParser);
begin
  AParser.BufferNext;
  FValue.ValueAsText := AParser.BufferValueToStr(cnstCommentEnd);
  if Length(FValue.ValueAsStr) = 0 then
    FValue.ValueAsText := AParser.BufferValueToChar('>');
end;

{$IFDEF SG_EXPORT_WITH_STRINGS}
procedure TsgNodeComment.SaveToStrings(const AStrings: TStrings);
var
  vComment: string;
begin
  if NodeType <> ntComment then
    inherited SaveToStrings(AStrings)
  else
  begin
    vComment := cnstCommentBegin + ValueAsText + cnstCommentEnd + sLineBreak;
    AStrings.AddObject(vComment, Self);
  end;
end;
{$ELSE}
procedure TsgNodeComment.SaveToStream(const AStream: TStream);
var
  vComment: string;
begin
  if NodeType <> ntComment then
    inherited SaveToStream(AStream)
  else
  begin
    vComment := cnstCommentBegin + ValueAsText + cnstCommentEnd + sLineBreak;
    SaveString(AStream, vComment);
  end;
end;
{$ENDIF}

procedure TsgNodeComment.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsgNodeComment then
    FValue.Assign(TsgNodeComment(Obj).FValue);
end;

{ TsgNodeAttrib }

{$IFDEF SG_USE_DICTIONARY_BY_NAME}
constructor TsgNodeAttrib.Create;
begin
  inherited Create;
  FNameIndex := -1;
end;

function TsgNodeAttrib.GetFName: string;
begin
  Result := GetNameByIndex(FNameIndex);
end;

function TsgNodeAttrib.GetIndexName: Integer;
begin
  Result := FNameIndex;
end;
{$ENDIF}

function TsgNodeAttrib.GetName: string;
begin
  Result := FName;
end;

function TsgNodeAttrib.GetNodeType: TsgNodeType;
begin
  Result := ntAttribute;
end;

function TsgNodeAttrib.GetPrefix: string;
begin
  Result := FPrefix;
end;

procedure TsgNodeAttrib.Read(const AParser: TsgParser);
var
  vFindEqual: Boolean;
begin
  vFindEqual := False;
  while AParser.CheckState do
  begin
    if not IsSpace(AParser.BufferValue) then
    begin
      if AParser.BufferValue = '=' then
        vFindEqual := True;
      Break;
    end;
    AParser.BufferNext;
  end;
  if vFindEqual then
  begin
    AParser.BufferNext;
    while AParser.CheckState do
    begin
      if not IsSpace(AParser.BufferValue) then
      begin
        if IsApostrophe(AParser.BufferValue) then
        begin
          FValue.ValueAsText := AParser.ReadValue;
          AParser.BufferNext;
        end;
        Break;
      end;
      AParser.BufferNext;
    end;
  end;
end;

{$IFDEF SG_USE_DICTIONARY_BY_NAME}
procedure TsgNodeAttrib.SetFName(const AValue: string);
begin
  FNameIndex := GetNameIndex(AValue, True);
end;
{$ENDIF}

procedure TsgNodeAttrib.ChangeName(const AValue: string);
begin
  FName := AValue;
end;

procedure TsgNodeAttrib.SetPrefix(const AValue: string);
begin
  FPrefix := AValue;
end;

procedure TsgNodeAttrib.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsgNodeAttrib then
  begin
{$IFNDEF SG_USE_DICTIONARY_BY_NAME}
    FName := TsgNodeAttrib(Obj).FName;
{$ELSE}
    FNameIndex := TsgNodeAttrib(Obj).FNameIndex;
{$ENDIF}
    FPrefix := TsgNodeAttrib(Obj).FPrefix;
  end;
end;

{ TsgNode }

function TsgNode.GetAttributeNode(const AIndex: Integer): TsgNodeSample;
begin
  Result := TsgNodeSample(FAttributes.Items[AIndex]);
end;

function TsgNode.GetAttributeNodesCount: Integer;
begin
  if FAttributes <> nil then
    Result := FAttributes.Count
  else
    Result := 0;
end;

function TsgNode.GetChildByName(const AName: string): TsgNodeSample;
begin
  Result := nil;
  if FChilds <> nil then
     Result := FChilds.NodeByName[AName];
end;

function TsgNode.GetChildNode(const AIndex: Integer): TsgNodeSample;
begin
  Result := FChilds.Nodes[AIndex];
end;

function TsgNode.GetChildNodesCount: Integer;
begin
  if FChilds <> nil then
    Result := FChilds.Count
  else
    Result := 0;
end;

function TsgNode.GetClosed: Boolean;
begin
  Result := FFlags and 1 <> 0;
end;

function TsgNode.GetNodeType: TsgNodeType;
begin
  Result := FType;
end;

function TsgNode.GetOwnerList(const AType: TsgNodeType): TsgNodeList;
begin
  Result := nil;
  case AType of
    ntAttribute:  Result := FAttributes;
    ntElement:    Result := FChilds;
  end;
end;

function TsgNode.GetPostamble: Boolean;
begin
  Result := FFlags and 2 <> 0;
end;

function TsgNode.GetSaveData(var AName, AValue, AText: string): Integer;
begin
  Result := inherited GetSaveData(AName, AValue, AText);
  if Result and 1 <> 0 then
    AValue := ' ' + cnstNodeValue + AValue
  else
    AValue := '';
end;

function TsgNode.GetTextData: TsgData;
begin
  if not Assigned(FText) then
    FText := TsgData.Create;
  Result := FText;
end;

procedure TsgNode.Read(const AParser: TsgParser);
var
  vStage: TsgParserStage;
  vNode: TsgNodeSample;
begin
  inherited Read(AParser);
  vStage := psEnd;
  while AParser.CheckState do
  begin
    case vStage of
      psEnd:
        begin
          case AParser.BufferValue of
            '/':
              begin
                if Length(Name) > 0 then
                  Closed := True
                else
                  Postamble := True;
                AParser.BufferNext;
              end;
            '>':
              begin
                AParser.BufferNext;
                if Closed or Postamble then
                  Break
                else
                  vStage := psText;
              end;
          else
            if IsLetter(AParser.BufferValue) then
              vStage := psAttrib
            else
              AParser.BufferNext;
          end;
        end;
      psText:
        begin
          if not (Closed or Postamble) then
            ReadTextAndChildNodes(AParser);
          Break;
        end;
      psAttrib:
        begin
          if IsLetter(AParser.BufferValue) then
          begin
            vNode := AParser.ReadAttrib;
            try
              AddAttrib(vNode);
              vNode.Read(AParser);
              vStage := psEnd;
            finally
              if (vNode.FullName = cnstNodeValue) and RemoveAttrib(vNode) then
              begin
                ValueData.Assign(vNode.ValueData);
                vNode.Free;
              end;
            end;
          end;
        end;
{$IFDEF SG_DEBUG}
    else
      ShowMessage(sNodeStageUndefined)
{$ENDIF}
    end;
  end;
end;

procedure TsgNode.ReadTextAndChildNodes(const AParser: TsgParser);
var
  vNode: TsgNodeSample;
  vReadTextCnt: Integer;
  vStr, vTextValue: string;
begin
  vTextValue := '';
  try
    vReadTextCnt := 0;
    while AParser.CheckState do
    begin
      vStr := AParser.ReadText;
      Inc(vReadTextCnt);
      case AParser.FNodeTextMode of
        tmFullText:
          begin
            vTextValue := ReplaceWebCodeEx(vStr);
            if Length(vTextValue) > 0 then
              Text := Text + vTextValue;
          end;
      else
        if vReadTextCnt = 1 then
          vTextValue := ReplaceWebCodeEx(vStr);
      end;
      if AParser.CheckState then
      begin
        vNode := AParser.ReadNode;
        if vNode <> nil then
        begin
          vNode.Read(AParser);
          if vNode.Postamble then
          begin
            Closed := True;
            FreeAndNil(vNode);
            Break;
          end
          else
          begin
            AddChild(vNode);
            CheckText(vNode);
          end;
        end
        else
          Break;
      end
      else
        Break;
    end;
  finally
    case AParser.FNodeTextMode of
      tmValue:
        begin
          if GetChildNodesCount < 1 then
            Text := vTextValue;
        end;
      tmText:  Text := vTextValue;
    end;
  end;
end;

function TsgNode.RemoveAttrib(const ANode: TsgNodeSample): Boolean;
begin
  Result := RemoveNode(FAttributes, ANode);
end;

function TsgNode.RemoveChild(const ANode: TsgNodeSample): Boolean;
begin
  Result := RemoveNode(FChilds, ANode);
end;

function TsgNode.RemoveAttributeByName(const AName: string;
  const ADoFree: Boolean): Boolean;
begin
  Result := RemoveByName(FAttributes, AName, ADoFree); 
end;

function TsgNode.RemoveByName(const ANodes: TsgNodeList; const AName: string;
  const ADoFree: Boolean): Boolean;
var
  vNode: TsgNodeSample;
begin
  Result := False;
  if Assigned(ANodes) then
  begin
    vNode := ANodes.NodeByName[AName];
    if Assigned(vNode) then
    begin
      Result := RemoveNode(ANodes, vNode);
      if Result and ADoFree then
        vNode.Free;
    end; 
  end;
end;

function TsgNode.RemoveNode(const ANodes: TsgNodeList;
  const ANode: TsgNodeSample): Boolean;
begin
  Result := False;
  if ANode.FOwner = Self then
  begin
    ANode.FOwner := nil;
    if Assigned(ANodes) then
    begin
      if ANodes.Remove(ANode) > -1 then
        Result := True;
    end;
  end;
end;

function TsgNode.RemoveChildByName(const AName: string;
  const ADoFree: Boolean): Boolean;
begin
  Result := RemoveByName(FChilds, AName, ADoFree);
end;

procedure TsgNode.SetClosed(const AValue: Boolean);
begin
  FFlags := (FFlags and 254) or Byte(AValue);
end;

procedure TsgNode.SetNodeType(const AValue: TsgNodeType);
begin
  FType := AValue;
end;

procedure TsgNode.SetPostamble(const AValue: Boolean);
begin
  FFlags := (FFlags and 253) or (Byte(AValue) shl 1);
end;

procedure TsgNode.Clear;
begin
  inherited Clear;
  if FAttributes <> nil then
    FAttributes.ClearNodes;
  if FChilds <> nil then
    FChilds.ClearNodes;
end;

constructor TsgNode.Create;
begin
  inherited Create;
//  FText := TsgData.Create;//create in get metod
  FType := ntElement;
end;


destructor TsgNode.Destroy;
begin
  FreeAndNil(FText);
  if FAttributes <> nil then
    FAttributes.Free;
  if FChilds <> nil then
    FChilds.Free;
  inherited Destroy;
end;

{$IFDEF SG_PARSER_USE_RTTI}
procedure TsgNode.LoadFromObject(const AObj: TObject; AClass: TClass = nil);
var
  I, J: Integer;
  vTypeData: PTypeData;
  vPropList: TPropList;
  vPPropInfo: PPropInfo;
  vFloatValue: Double;
  vIntValue: Integer;
  vFPointValue: TFPoint;
  vRecordName: string;
begin
  if AClass = nil then
    AClass := AObj.ClassType;
  vTypeData := GetTypeData(AClass.ClassInfo);
  GetPropInfos(AClass.ClassInfo, @vPropList);
  for I := 0 to vTypeData.PropCount - 1 do
  begin
    vPPropInfo := vPropList[I];
    if not Assigned(vPPropInfo) then
      Continue;
    case vPPropInfo^.PropType^.Kind of
      tkFloat:
        begin
          vFloatValue := GetPropValue(Self, vPPropInfo);
          AddAttribNV(vPPropInfo^.Name).ValueAsDouble := vFloatValue;
        end;
      tkInteger:
        begin
          vIntValue := GetPropValue(Self, vPPropInfo);
          AddAttribNV(vPPropInfo^.Name).ValueAsInt := vIntValue;
        end;
      tkRecord:
        begin
          case GetRecordType(vPPropInfo) of
            tkrTFPoint:
              begin
                vFPointValue := GetPropertyAsFPoint(Self, vPPropInfo);
                AddAttribNV(vPPropInfo^.Name).ValueAsFPoint := vFPointValue;
              end;
          end;
        end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF SGDEL_2009}
function TsgNode.GetAttributesEnumerable: IEnumerable<TsgNodeSample>;
begin
  Result := TsgNodeEnumerator.Create(FAttributes);
end;

function TsgNode.GetChildsEnumerable: IEnumerable<TsgNodeSample>;
begin
  Result := TsgNodeEnumerator.Create(FChilds);
end;
{$ENDIF}

procedure TsgNode.AddAttrib(const ANode: TsgNodeSample);
begin
  if FAttributes = nil then
    FAttributes := TsgNodeList.CreateNodeList;
  FAttributes.Add(ANode);
  ANode.FOwner := Self;
end;

function TsgNode.AddAttribNV(const AFullName: string;
  const AValue: string = ''): TsgNodeAttrib;
begin
  Result := nil;
  if Length(AFullName) > 0 then
  begin
    Result := TsgNodeAttrib.Create;
    Result.FullName := AFullName;
    Result.ValueAsStr := AValue;
    AddAttrib(TsgNode(Result));
  end;
end;

procedure TsgNode.AddChild(const ANode: TsgNodeSample;
  const AIndex: Integer = -1);
begin
  if FChilds = nil then
    FChilds := TsgNodeList.CreateNodeList;
  if AIndex < 0 then
    FChilds.Add(ANode)
  else
    FChilds.Insert(AIndex, ANode);
  ANode.FOwner := Self;
end;

function TsgNode.AddChildNV(const AFullName: string;
  const AValue: string = ''): TsgNode;
begin
  Result := nil;
  if Length(AFullName) > 0 then
  begin
    Result := TsgNode.Create;
    Result.FullName := AFullName;
    if Length(AValue) > 0 then
      Result.ValueAsStr := AValue;
    AddChild(Result);
  end;
end;

function TsgNode.AddChildNVBool(const AFullName: string;
  const AValue: Boolean): TsgNode;
begin
  Result := AddChildNV(AFullName);
  Result.ValueAsBool := AValue;
end;

function TsgNode.AddChildNVDouble(const AFullName: string;
  const AValue: Double): TsgNode;
var
  S: string;
begin
  S := DoubleToStr(AValue);
  Result := AddChildNV(AFullName, S);
end;

function TsgNode.AddChildNVInt(const AFullName: string;
  const AValue: Integer): TsgNode;
var
  S: string;
begin
  S := IntToStr(AValue);
  Result := AddChildNV(AFullName, S);
end;

function TsgNode.AddAttribs(const AAttribs: TStrings): Integer;
var
  I: Integer;
{$IFNDEF SGDEL_7}
  J: Integer;
  vName, vValue: string;
{$ENDIF}
begin
  if Assigned(AAttribs) and (AAttribs.Count > 0) then
  begin
    Result := AttributeNodesCount;
    try
{$IFDEF SGDEL_7}
      for I := 0 to AAttribs.Count - 1 do
        AddAttribNV(AAttribs.Names[I], AAttribs.ValueFromIndex[I]);
{$ELSE}
      for I := 0 to AAttribs.Count - 1 do
      begin
        J := AnsiPos('=', AAttribs[I]);
        if J > 0 then
        begin
          vName := Copy(AAttribs[I], 1, J - 1);
          vValue := Copy(AAttribs[I], J + 1, MaxInt);
        end
        else
        begin
          vName := AAttribs[I];
          vValue := '';
        end;
        AddAttribNV(vName, vValue);
      end;
{$ENDIF}
    finally
      Result := AttributeNodesCount - Result;
    end;
  end
  else
    Result := 0;
end;

procedure TsgNode.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsgNode then
  begin
    FType := TsgNode(Obj).FType;
    FreeAndNil(FText);
    if Assigned(TsgNode(Obj).FText) then
      TextData.Assign(TsgNode(Obj).FText);
    AssignObjLists(Self, TsgNode(Obj).FAttributes, FAttributes);
    AssignObjLists(Self, TsgNode(Obj).FChilds, FChilds);
  end;
end;

function TsgNode.GetAttributeByName(const AName: string): TsgNodeSample;
begin
  if FAttributes = nil then
    Result := nil
  else
    Result := FAttributes.NodeByName[AName];
end;

function TsgNode.GetNodeByAttribValue(const ANodeName, AName,
  AValue: string; const ACaseSensitivity: Boolean = True): TsgNodeSample;
var
  I: Integer;
  vNode: TsgNodeSample;
  vAttribute: TsgNodeSample;
begin
  Result := nil;
  for I := 0 to ChildNodesCount - 1 do
  begin
    vNode := ChildNodes[I];
    if (Length(ANodeName) = 0) or
      (sgAnsiCmprStr(ANodeName, vNode.Name, FChilds.CaseSensitivity) = 0) then
    begin
      if vNode.HasAttributeNodes then
      begin
        vAttribute := vNode.GetAttributeByName(AName);
        if Assigned(vAttribute) and (sgAnsiCmprStr(vAttribute.ValueAsStr, AValue, ACaseSensitivity) = 0) then
        begin
          Result := vNode;
          Break;
        end;
      end;
    end;
  end;
end;

{ TsgNodeCDATA }

function TsgNodeCDATA.GetNodeType: TsgNodeType;
begin
  Result := ntCData;
end;

procedure TsgNodeCDATA.Read(const AParser: TsgParser);
var
  vValue: string;
begin
  vValue := '';
  while AParser.BufferCheckPos and (AParser.BufferValue <> cnstCDATABegin) do
    AParser.BufferNext;
  AParser.BufferNext;
  while AParser.BufferCheckPos do
  begin
    if AParser.BufferValue = cnstCDATAEnd then
    begin
      if (AParser.FBufferIndex + 1 < AParser.FBufferSize - 1) or
         (AParser.FBuffer[AParser.FBufferIndex + 1]  = cnstCDATAEnd) then
        Break;
    end;
    vValue := vValue + AParser.BufferValue;
    AParser.BufferNext;
  end;
  FValue.ValueAsText := vValue;
end;

{ TsgNodeDocument }

function TsgNodeDocument.GetClosed: Boolean;
begin
  Result := True;
end;

function TsgNodeDocument.GetNodeType: TsgNodeType;
begin
  Result := ntDocument;
end;

{$IFDEF SG_EXPORT_WITH_STRINGS}
procedure TsgNodeDocument.SaveToStrings(const AStrings: TStrings);
var
  I: Integer;
  vName, vTmp: string;
  vAttribute: TsgNodeSample;
begin
  vName := Name;
  if Length(vName) > 0 then
  begin
    if vName[1] <> '?' then
      vName := '?' + vName;
    vTmp := GetSpacesBySave + '<' + vName;
    for I := 0 to GetAttributeNodesCount - 1 do
    begin
      vAttribute := AttributeNodes[I];
      vTmp := vTmp + ' ' + vAttribute.Name + '="' + vAttribute.ValueAsStr + '"';
    end;
    vTmp := vTmp + '?>' + sLineBreak;
    AStrings.AddObject(vTmp, Self);
  end;
end;
{$ELSE}
procedure TsgNodeDocument.SaveToStream(const AStream: TStream);
var
  I: Integer;
  vName, vTmp: string;
  vAttribute: TsgNodeSample;
begin
  vName := Name;
  if Length(vName) > 0 then
  begin
    if vName[1] <> '?' then
      vName := '?' + vName;
    vTmp := GetSpacesBySave + '<' + vName;
    for I := 0 to GetAttributeNodesCount - 1 do
    begin
      vAttribute := AttributeNodes[I];
      vTmp := vTmp + ' ' + vAttribute.Name + '="' + vAttribute.ValueAsStr + '"';
    end;
    vTmp := vTmp + '?>' + sLineBreak;
    SaveString(AStream, vTmp);
  end;
end;
{$ENDIF}

{ TsgNodeDocType }

function TsgNodeDocType.GetName: string;
begin
  Result := cnstDOCTYPE;
end;

function TsgNodeDocType.GetNodeType: TsgNodeType;
begin
  Result := ntDocType;
end;

procedure TsgNodeDocType.Read(const AParser: TsgParser);
var
  vValue: string;
  vReadValue: Integer;
  vChar: Char;
begin
  vValue := '';
  vReadValue := 0;
  while AParser.FBufferIndex < AParser.FBufferSize do
  begin
    vChar := AParser.BufferValue;
    AParser.BufferNext;
    case vChar of
      '>':
        begin
          if vReadValue = 0 then
            Break
          else
            vValue := vValue + vChar;
        end;
      '[': Inc(vReadValue);
      ']': Dec(vReadValue);
    else
      if vReadValue > 0 then
        vValue := vValue + vChar;
    end;
  end;
  FreeValues;
  if Length(vValue) > 0 then
  begin
    FValue.ValueAsText := vValue;
    ReadValues(AParser);
  end;
end;

procedure TsgNodeDocType.ReadValues(const AParser: TsgParser);
var
  vParser: TsgParser;
  vStream: TMemoryStream;
  vValueStr: string;
{$IFDEF SGDEL_2009}
  vBom: TBytes;
{$ENDIF}
begin
  vParser := TsgParser.Create;
  try
    vParser.Assign(AParser);
    vStream := TMemoryStream.Create;
    try
      vValueStr := FValue.ValueAsStr;
      TsgMemoryStream(vStream).Capacity := Length(vValueStr) * SizeOf(Char) + 2;
{$IFDEF SGDEL_2009}
      vBom := TEncoding.Unicode.GetPreamble;
      vStream.Write(vBom[0], Length(vBom));
{$ENDIF}
      vStream.Write(vValueStr[1], Length(vValueStr) * SizeOf(Char));
      vValueStr := '';
      vStream.Position := 0;
      try
        vParser.LoadFromStream(vStream);
      except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
      end;
    finally
      vStream.Free;
    end;
    if vParser.ROOT.Count > 0 then
    begin
      FValues := TsgNodeList.CreateNodeList;
      FValues.QwickFind := False;
      FValues.AssignList(vParser.ROOT, loCopy);
      vParser.ROOT.Count := 0;
    end;
  finally
    vParser.Free;
  end;
end;

procedure TsgNodeDocType.ChangeName(const AValue: string);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

destructor TsgNodeDocType.Destroy;
begin
  FreeValues;
  inherited Destroy;
end;

procedure TsgNodeDocType.FreeValues;
begin
  if FValues <> nil then
  begin
    FValues.ClearNodes;
    FreeAndNil(FValues);
  end;
end;

procedure TsgNodeDocType.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsgNodeDocType then
    AssignObjLists(Self, TsgNodeDocType(Obj).FValues, FValues);
end;

{ TsgNodeEntity }

function TsgNodeEntity.GetName: string;
begin
  Result := FName;
  if Length(Result) = 0 then
    Result := cnstENTITY;
end;

function TsgNodeEntity.GetNodeType: TsgNodeType;
begin
  Result := ntEntity;
end;

procedure TsgNodeEntity.Read(const AParser: TsgParser);
var
  vName, vValue: string;
  vValueKey: Char;
begin
  vName := ReadNodeName(AParser);
  while AParser.CheckState and (IsSpace(AParser.BufferValue) or
   (AParser.BufferValue = '>')) do
    AParser.BufferNext;
  if (Length(vName) > 0) and AParser.CheckState and
    IsApostrophe(AParser.BufferValue) then
  begin
    vValueKey := AParser.BufferValue;//this value from IsValueKey
    vValue := '';
    AParser.BufferNext;
    while AParser.CheckState and (not (AParser.BufferValue = vValueKey) or
      (AParser.BufferValue = '>')) do
    begin
      vValue := vValue + AParser.BufferValue;
      AParser.BufferNext;
    end;
    if Length(vValue) > 0 then
    begin
      Name := vName;
      FValue.ValueAsText := vValue;
    end;
  end;
end;

function TsgNodeEntity.ReadNodeName(const AParser: TsgParser): string;
begin
  Result := '';
  while AParser.CheckState do
  begin
    if IsLetter(AParser.BufferValue) then
      Result := Result + AParser.BufferValue
    else
    begin
      if Length(Result) > 0 then
      begin
        if IsName(AParser.BufferValue) then
          Result := Result + AParser.BufferValue
        else
          Break;
      end;
    end;
    AParser.BufferNext;
  end;
end;

procedure TsgNodeEntity.ChangeName(const AValue: string);
begin
  if UpperCase(AValue) = cnstENTITY then
    FName := ''
  else
    FName := AValue;
end;

{ TsgNodeHTML }

function TsgNodeHTML.GetClassID: Cardinal;
begin
  Result := cnstClassID_HTML;
end;

{ TsgParser }

procedure TsgParser.BeginUpdateProgress;
begin
  FBufferReaded := 0;
  FBufferStep := cnstPercentUpdateProgress * FBufferSize div 100;
  if Assigned(FOnProgress) then
    FOnProgress(psStarting, 0, 100);
end;

procedure TsgParser.EndUpdateProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(psEnding, 100, 100);
end;

procedure TsgParser.CheckReadBufferError;
var
  I, vSymbol: Integer;
begin
  for I := 1 to FBufferSize - 1 do
  begin
    vSymbol := Integer(FBuffer[I]);
    if vSymbol = 0 then// < 9 then
    begin
      FBufferSize := I - 1;
      SetLength(FBuffer, FBufferSize);;
      Break;
    end;
  end;
end;

procedure TsgParser.UpdateProgress;
var
  vBufferReaded: Integer;
begin
  vBufferReaded := FBufferIndex - FBufferReaded;
  if vBufferReaded > FBufferStep then
  begin
    FBufferReaded := FBufferIndex;
    if Assigned(FOnProgress) then
      FOnProgress(psRunning, FBufferReaded, FBufferSize);
  end;
end;

function TsgParser.AddDeclaration(const AVersion: string = '1.0';
  AEncoding: string = ''): Boolean;
var
  vNode: TsgNodeDocument;
begin
  Result := False;
  if (FROOT.Count = 0) or (not (FROOT.Nodes[0] is TsgNodeDocument)) or
     (FROOT.Nodes[0].Name <> 'xml') then
  begin
    vNode := TsgNodeDocument.Create;
    vNode.Name := 'xml';
    FROOT.Insert(0, vNode);
    vNode.AddAttribNV(cnstVersion, AVersion);
    if Length(AEncoding) = 0 then
      AEncoding := 'UTF-8';
    vNode.AddAttribNV(cnstEncoding, AEncoding);
    Result := True;
  end;
end;

procedure TsgParser.AfterLoadFromStream;
begin
end;

procedure TsgParser.Analize;
var
  vReload, vCheckedEntities, vCheckedEncoding: Boolean;
  vNode: TsgNodeSample;
begin
  LoadDebugInfo;
  vCheckedEntities := False;
  vCheckedEncoding := False;
  repeat
    vReload := False;
    while CheckState do
    begin
      vNode := ReadNode;
      if vNode <> nil then
      begin
        vNode.Read(Self);
        case vNode.NodeType of
          ntDocType:
            begin
              if (not vCheckedEntities) and (UpperCase(vNode.Name) =
                cnstDOCTYPE) then
              begin
                vCheckedEntities := True;
                vReload := CheckEntities(TsgNodeDocType(vNode));
                if vReload then
                  Break;
              end
            end;
          ntDocument:
            begin
              if (not vCheckedEncoding) and (AnsiLowerCase(vNode.Name) =
                cnstXML) then
              begin
                vCheckedEncoding := True;
                vReload := CheckEncoding(TsgNodeDocument(vNode));
                if vReload then
                  Break;
              end;
            end;
        end;
        FROOT.Add(vNode);
      end
      else
        Break;
    end;
  until not vReload;
end;

procedure TsgParser.BeforeLoadFromStream;
begin
end;

procedure TsgParser.BeginAnalize;
begin
  FBufferIndex := 1;
  FBufferSize := Length(FBuffer);
  FROOT.ClearNodes;
  BeginUpdateProgress;
end;

function TsgParser.BufferCheckPos: Boolean;
begin
  Result := FBufferIndex <= FBufferSize;
end;

procedure TsgParser.BufferNext;
begin
  Inc(FBufferIndex);
end;

function TsgParser.BufferValue: Char;
begin
  Result := FBuffer[FBufferIndex];
end;

function TsgParser.BufferValueToChar(const AChar: Char): string;
var
  vIndex: Integer;
begin
  Result := '';
  vIndex := sgStringScan(AChar, FBuffer, FBufferIndex);
  if vIndex > -1 then
  begin
    Result := Copy(FBuffer, FBufferIndex, vIndex - FBufferIndex);
    if vIndex > FBufferIndex then // vIndex == 0 if #0 find on StringScan!!!
      FBufferIndex := vIndex;
  end;
end;

function TsgParser.BufferValueToStr(const AStr: string): string;
var
  vIndex: Integer;
begin
  Result := '';
  vIndex := sgStringPos(AStr, FBuffer, FBufferIndex);
  if vIndex > -1 then
  begin
    Result := Copy(FBuffer, FBufferIndex, vIndex - FBufferIndex);
    FBufferIndex := vIndex;
  end;
end;
function TsgParser.CheckEncoding(var ADocument: TsgNodeDocument): Boolean;
{$IFDEF SGDEL_2009}
var
  vAttrib: TsgNodeSample;
  vCodePage: Integer;
  vStrStream: TStringStream;
  vEnc: TEncoding;
{$ENDIF}
begin
  Result := False;
{$IFDEF SGDEL_2009}
  if (FSource = nil) or (not ADocument.HasAttributeNodes) then Exit;
  vCodePage := cnstDefCodePage;
  vAttrib := ADocument.GetAttributeByName(cnstEncoding);
  if (vAttrib <> nil) and GetCodePageByName(vAttrib.ValueAsStr, vCodePage) then
  begin
    Result := vCodePage <> cnstDefCodePage;
    if Result then
    begin
      vEnc := TEncoding.GetEncoding(vCodePage);
      vStrStream := TStringStream.Create('', vEnc, False);
      try
        vStrStream.LoadFromStream(FSource);
        FBuffer := vStrStream.DataString;
        FBufferSize := Length(FBuffer);
        FreeAndNil(ADocument);
        BeginAnalize;
      finally
        vStrStream.Free;
        if not TEncoding.IsStandardEncoding(vEnc) then
          vEnc.Free;
      end;
    end;
  end;
{$ENDIF}
end;

function TsgParser.CheckEntities(var ADocType: TsgNodeDocType): Boolean;
var
  I: Integer;
  vNode: TsgNodeSample;
begin
  Result := False;
  if ADocType.Values <> nil then
  begin
    for I := 0 to ADocType.Values.Count - 1 do
    begin
      vNode := ADocType.Values.Nodes[I];
      if vNode.NodeType = ntEntity then
      begin
        if sgStringReplace(FBuffer, '&' + vNode.Name + ';', vNode.ValueAsStr) then
          Result := True;
      end;
    end;
    if Result then
    begin
      FreeAndNil(ADocType);
      BeginAnalize;
    end;
  end;
end;

function TsgParser.CheckState: Boolean;
begin
  Result := (FBufferIndex <= FBufferSize) and (not Stopped);
end;

procedure TsgParser.Clear;
begin
  FROOT.ClearNodes;
  FROOT.Clear;
end;

procedure TsgParser.ClearBuffer;
begin
  FBuffer := '';
  FBufferIndex := 1;
  FBufferSize := 0;
end;

function TsgParser.CreateNodeSample(const AFullName: string;
  AClass: TClass): TsgNodeSample;
var
  vIndex: Integer;
  vName: string;
begin
  if FRegisterNodes.Count > 0 then
  begin
    vName := GetNameFromFullName(AFullName);
    vIndex := FRegisterNodes.IndexOf(vName);
    if vIndex > -1 then
      AClass := TsgObjectTClass(FRegisterNodes.Objects[vIndex]).FieldClass;
  end;
  Result := TsgClassOfNode(AClass).Create;
  Result.FullName := AFullName;
end;

class function TsgParser.FindNodeByAttrib(const AAttribName, AAttribValue: string;
  const AList: TsgNodeList; AFoundNodes: TsgObjectList = nil;
  const AIgnoreValue: Boolean = False;
  const AFindCount: Integer = -1): TsgNodeSample;
var
  vIterator: TsgNodeIterator;
begin
  Result := nil;
  if AList <> nil then
  begin
    vIterator := TsgNodeIterator.Create(AList,
      TsgFindByAttribute.Create(AAttribName, AAttribValue, AIgnoreValue));
    try
      vIterator.FindNotAll := not Assigned(AFoundNodes);
      if vIterator.Find(AFindCount) > 0 then
      begin
        Result := TsgNodeSample(vIterator.FindResult.First);
        if Assigned(AFoundNodes) then
          AFoundNodes.AssignList(vIterator.FindResult, loCopy);
      end;
    finally
      vIterator.Free;
    end;
  end;
end;

function TsgParser.FindEqualNode(const ABase: TsgNodeSample; const ANode: TsgNodeSample;
  ANodeCompareFunc: TsgNodeCompareFunc = nil; const ATree: TsgObjectList = nil): TsgNodeSample;
var
  I: Integer;
  vNodeCompareFunc: TsgNodeCompareFunc;
begin
  Result := nil;
  if (ANode = nil) and (not Assigned(ANodeCompareFunc)) then
     Exit;
  if ANode <> nil then
    vNodeCompareFunc := {$IFDEF FPC_OBJFPC}@{$ENDIF}ANode.Compare
  else
    vNodeCompareFunc := ANodeCompareFunc;
  if Assigned(ABase) then
  begin
    if vNodeCompareFunc(ABase) = 0 then
      Result := ABase
    else
    begin
      if Assigned(ATree) then
        ATree.Add(ABase);
      for I := 0 to ABase.GetChildNodesCount - 1 do
      begin
        Result := FindEqualNode(ABase.GetChildNode(I), ANode, ANodeCompareFunc,
          ATree);
        if Assigned(Result) then
          Break;
      end;
      if Assigned(ATree) and (not Assigned(Result)) then
        ATree.Count := ATree.Count - 1;
    end;
  end
  else
  begin
    for I := 0 to ROOT.Count - 1 do
    begin
      Result := FindEqualNode(ROOT.Nodes[I], ANode, ANodeCompareFunc, ATree);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

function TsgParser.FindNodeByAttribValue(const ANode: TsgNodeSample;
  const AAttribName, AAttribValue: string; const AFoundNodes: TsgObjectList = nil;
  const AIgnoreValue: Boolean = False): TsgNodeSample;
var
  I: Integer;
  vAttrib: TsgNodeSample;
begin
  Result := nil;
  if ANode.HasAttributeNodes then
  begin
    vAttrib := ANode.GetAttributeByName(AAttribName);
    if (vAttrib <> nil) then
    begin
      if AIgnoreValue then
      begin
        if Assigned(AFoundNodes) then
          AFoundNodes.Add(ANode)
        else
          Result := ANode;
      end
      else
      begin
        if vAttrib.ValueAsStr = AAttribValue then
          Result := ANode;
      end;
    end;
  end;
  if (Result = nil) and ANode.HasChildNodes then
  begin
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      Result := FindNodeByAttribValue(ANode.ChildNodes[I], AAttribName,
        AAttribValue, AFoundNodes, AIgnoreValue);
      if Result <> nil then
        if Assigned(AFoundNodes) then
          AFoundNodes.Add(Result)
        else
          Break;
    end;
  end;
end;

class function TsgParser.FindNodeByName(const ANodeName: string;
  const AList: TsgNodeList; AFoundNodes: TsgObjectList = nil;
  const AFindCount: Integer = -1): TsgNodeSample;
var
  vIterator: TsgNodeIterator;
begin
  Result := nil;
  if AList <> nil then
  begin
    vIterator := TsgNodeIterator.Create(AList,
      TsgFindByNode.Create(ANodeName));
    try
      vIterator.FindNotAll := not Assigned(AFoundNodes);
      if vIterator.Find(AFindCount) > 0 then
      begin
        Result := TsgNodeSample(vIterator.FindResult.First);
        if Assigned(AFoundNodes) then
          AFoundNodes.AssignList(vIterator.FindResult, loCopy);
      end;
    finally
      vIterator.Free;
    end;
  end;
end;

function TsgParser.GetDebugInfo: Boolean;
begin
  Result := Assigned(FDebugNodePosition);
end;

function TsgParser.GetXMLString: string;
begin
  Result := SaveToString;
end;

procedure TsgParser.ImportNodes(const AParser: TsgParser);
var
  vNode: TsgNodeSample;
begin
  FreeAndNil(FDebugNodePosition);
  ClearBuffer;
  ROOT.ClearNodes;
  while AParser.ROOT.Count > 0 do
  begin
    vNode := TsgNodeSample(AParser.ROOT.Items[0]);
    AParser.ROOT.Delete(0);
    ROOT.Add(vNode);
  end;
end;

function TsgParser.InitFromStream: Boolean;
{$IFDEF SGDEL_2009}
var
  vBytes: TBytes;
  vBytesSize: Integer;
  vEncoding: TEncoding;
{$ENDIF}
begin
  Result := False;
  if FSource <> nil then
  begin
    FSource.Position := 0;
{$IFDEF SGDEL_2009}
    vEncoding := nil;
    try
      vBytesSize := FSource.Size;
      FSource.Position := 0;
      SetLength(vBytes, vBytesSize);
      FSource.Read(vBytes[0], vBytesSize);
      if TEncoding.GetBufferEncoding(vBytes, vEncoding) = 0 then
        vEncoding := TMBCSEncoding.Create(cnstDefCodePage)
      //  vEncoding := TEncoding.GetEncoding(cnstDefCodePage)
      else
        FSource := nil;
      FBuffer := vEncoding.GetString(vBytes);
      FBufferSize := Length(FBuffer);
      CheckReadBufferError;
    finally
      SetLength(vBytes, 0);
      if (vEncoding <> nil) and
        (not TEncoding.IsStandardEncoding(vEncoding)) then
        vEncoding.Free;
    end;
{$ELSE}
    FBufferSize := FSource.Seek(0, soFromEnd);
    FSource.Seek(0, soFromBeginning);
    SetLength(FBuffer, FBufferSize);
    FSource.Read(FBuffer[1], FBufferSize);
    CheckReadBufferError;
{$ENDIF}
    Result := FBufferSize > 0;
  end;
end;

function TsgParser.InitFromString: Boolean;
begin
  FBuffer := FSourceString;
  FBufferSize := Length(FBuffer);
  FSourceString := '';
  Result := FBufferSize > 0;
end;

{$IFDEF SGDEL_2009}
class function TsgParser.SaveEncoding;
begin
  Result := TEncoding.UTF8;
end;
{$ENDIF}

function TsgParser.ReadAttrib: TsgNodeSample;//for optimization
var
  vName: string;
begin
  Result := TsgNodeSample(TsgNodeAttrib.Create);
  vName := '';
  while (FBufferIndex <= FBufferSize) and IsName(FBuffer[FBufferIndex]) do
  begin
    vName := vName + FBuffer[FBufferIndex];
    BufferNext;
  end;
  Result.FullName := vName;
  SetNodePosition(Result);
end;

function TsgParser.ReadCData: string;//for optimization
begin
  Result := '';
  while (FBufferIndex <= FBufferSize) and
    (FBuffer[FBufferIndex] <> cnstCDATABegin) do
    BufferNext;
  BufferNext;
  while (FBufferIndex <= FBufferSize) and
    (FBuffer[FBufferIndex] <> cnstCDATAEnd) do
  begin
    Result := Result + FBuffer[FBufferIndex];
    BufferNext;
  end;
  BufferNext;
end;

function TsgParser.ReadComment: string;//for optimization
begin
  BufferNext;
  Result := BufferValueToChar('>');
end;

function TsgParser.ReadDocType: string;
begin
  Result := ReadComment;
end;

function TsgParser.ReadNode: TsgNodeSample;
var
  vName: string;
  vNodeType: TsgNodeType;
  vPostamble: Boolean;
begin
  Result := nil;
  while CheckState and (FBuffer[FBufferIndex] <> '<') do
    BufferNext;
  vName := ReadNodeName(vNodeType, vPostamble);
  case vNodeType of
    ntUndefined:  begin end;
    ntComment:
      begin
        Result := TsgNodeSample(TsgNodeComment.Create);
      end;
    ntDocType:
      begin
        if UpperCase(vName) = cnstDOCTYPE then
          Result := TsgNodeSample(TsgNodeDocType.Create)
        else
        begin
          if UpperCase(vName) = cnstENTITY then
            Result := TsgNodeSample(TsgNodeEntity.Create)
          else
          begin
            Result := TsgNodeSample.Create;
            Result.SetNodeType(ntDocType);
            Result.FullName := vName;
          end;
        end;
      end;
    ntCData:
      begin
        Result := TsgNodeSample(TsgNodeCDATA.Create);
      end;
    ntDocument:
      begin
        Result := TsgNodeSample(TsgNodeDocument.Create);
        Result.FullName := vName;
      end;
  else
    if Length(vName) > 0 then
    begin
      Result := CreateNodeSample(vName, TsgNode);
      Result.SetNodeType(vNodeType);
      Result.Postamble := vPostamble;
    end;
  end;
  if Assigned(Result) then
    SetNodePosition(Result);
  UpdateProgress;
end;

function TsgParser.ReadNodeName(var ANodeType: TsgNodeType;
  var APostamble: Boolean): string;
begin
  Result := '';
  ANodeType := ntElement;
  APostamble := False;  
  while CheckState do
  begin
    case FBuffer[FBufferIndex] of
      '/':  APostamble := True;
      '?':  ANodeType := ntDocument;
      '!':  ANodeType := ntDocType;
      '[':  ANodeType := ntCData;
      '-':
      begin
        if ANodeType = ntDocType then
        begin
          BufferNext;
          if CheckState and (FBuffer[FBufferIndex] = '-') then
          begin
            ANodeType := ntComment;
            Break;
          end;
          Dec(FBufferIndex);
        end;
        ANodeType := ntElement;
        Result := FBuffer[FBufferIndex];
        BufferNext;
        while CheckState and IsName(FBuffer[FBufferIndex]) do
        begin
          Result := Result + FBuffer[FBufferIndex];
          BufferNext;
        end;
        Break;
      end;
    else
      if IsLetter(FBuffer[FBufferIndex]) then
      begin
        Result := FBuffer[FBufferIndex];
        BufferNext;
        while CheckState and IsName(FBuffer[FBufferIndex]) do
        begin
          Result := Result + FBuffer[FBufferIndex];
          BufferNext;
        end;
        Break;
      end
    end;
    BufferNext;
  end;
end;

function TsgParser.ReadText: string;//for optimization
var
  vTmp: string;
begin
  vTmp := '';
  while (FBufferIndex <= FBufferSize) and (FBuffer[FBufferIndex] <> '<') do
  begin
    if FBuffer[FBufferIndex] > Char($D) then
      vTmp := vTmp + FBuffer[FBufferIndex];
    BufferNext;
  end;
  Result := vTmp;
end;

function TsgParser.ReadValue: string;//for optimization
var
  vValueKey: Char;
begin
  vValueKey := FBuffer[FBufferIndex];
  BufferNext;
  Result := BufferValueToChar(vValueKey);
end;

constructor TsgParser.Create;
begin
  inherited Create;
  FNodeTextMode := tmText;
  FROOT := TsgNodeList.Create;
  FRegisterNodes := TsgStringList.Create;
  FRegisterNodes.Sorted := True;
  FRegisterNodes.CaseSensitive := False;
  FRegisterNodes.Duplicates := dupIgnore;
end;

function TsgParser.CreateRootNode(const AFullName: string = cnstXML;
  const ACreateDocument: Boolean = False): TsgNode;
var
  vDocument: TsgNodeDocument;
begin
  if ACreateDocument then
  begin
    vDocument := TsgNodeDocument.Create;
    vDocument.Name := cnstXML;
    ROOT.Add(vDocument);
    vDocument.AddAttribNV(cnstVersion).ValueAsStr := '1.0';
    vDocument.AddAttribNV(cnstEncoding).ValueAsStr := 'UTF-8';
    vDocument.AddAttribNV(cnstStandalone).ValueAsStr := 'no';
  end;
  Result := TsgNode.Create;
  Result.Name := AFullName;
  ROOT.Add(Result);
end;

destructor TsgParser.Destroy;
begin
  FreeAndNil(FDebugNodePosition);
  ClearObjectObjects(FRegisterNodes);
  FreeAndNil(FRegisterNodes);
  FROOT.Free;
  inherited Destroy;
end;

procedure TsgParser.Assign(const AParser: TsgParser);
var
  I: Integer;
begin
  ClearObjectObjects(FRegisterNodes);
  for I := 0 to AParser.FRegisterNodes.Count - 1 do
    RegisterClass(AParser.FRegisterNodes[I], TsgObjectTClass(AParser.FRegisterNodes[I]).FieldClass);
end;

function TsgParser.FindNode(const AID: string;
  const AList: TsgNodeList): TsgNodeSample;
begin
  Result := FindNodeByAttrib(cnstKeyWordsStr[idID], AID, AList);
end;

procedure TsgParser.LoadDebugInfo;
var
  I, vIndex, vLenLineBreak: Integer;
  vStrings: TsgStringList;
begin
  if not Assigned(FDebugNodePosition) then
    Exit;
  FDebugNodePosition.Count := 0;
  if FDebugInfoMode = 0 then
  begin
    vStrings := TsgStringList.Create;
    try
      try
        vLenLineBreak := Length(vStrings.LineBreak);
        vStrings.Text := FBuffer;
        vIndex := 0;
        for I := 0 to vStrings.Count - 1 do
        begin
          Inc(vIndex, Length(vStrings[I]) + vLenLineBreak);
          FDebugNodePosition.Add(vIndex);
        end;
      except
        FreeAndNil(FDebugNodePosition);
      end;
    finally
      vStrings.Free;
    end;
  end;
end;

function TsgParser.LoadFromBuffer(const AInitBuffer: TsgObjProcBool): Boolean;
begin
  Result := True;
  BeforeLoadFromStream;
  FROOT.ClearNodes;
  ClearBuffer;
  try
    if AInitBuffer{$IFDEF SGFPC}(){$ENDIF} then
    begin
      BeginUpdateProgress;
      try
        Analize;
      finally
        EndUpdateProgress;
      end;
    end;
    ClearBuffer;
    AfterLoadFromStream;
  except
    Result := False;
    ClearBuffer;
  end
end;

function TsgParser.LoadFromFile(const AFileName: string): Boolean;
var
  S: TFileStream;
  Dir: string;
begin
  Dir := GetCurrentDir;
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetCurrentDir(ExtractFilePath(AFileName));
    try
      try
        Result := LoadFromStream(S);
      except
        Result := False;
      end;
    finally
      S.Free;
      try
        SetCurrentDir(Dir);
      except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
      end;
    end;
  except
    raise;
  end;
end;

function TsgParser.LoadFromStream(const S: TStream): Boolean;
begin
  FSource := S;
  try
    Result := LoadFromBuffer({$IFDEF FPC_OBJFPC}@{$ENDIF}InitFromStream);
  finally
    FSource := nil;
  end;
end;

function TsgParser.LoadFromString(const AString: string): Boolean;
begin
  FSourceString := AString;
  try
    Result := LoadFromBuffer({$IFDEF FPC_OBJFPC}@{$ENDIF}InitFromString);
  finally
    FSourceString := '';
  end;
end;

procedure TsgParser.RegisterClass(const AFullName: string;
  const AClass: TClass);
begin
  FRegisterNodes.AddObject(AFullName, TsgObjectTClass.Create(AClass));
end;

function TsgParser.SaveToFile(const AFileName: string): Boolean;
var
  vFileStream: TFileStream;
begin
  vFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := SaveToStream(vFileStream);
  finally
    vFileStream.Free;
  end;
end;

function TsgParser.SaveToStream(const S: TStream): Boolean;
{$IFDEF SG_EXPORT_WITH_STRINGS}
var
  vBufferString: TStringList;
begin
  Result := False;
  vBufferString := TStringList.Create;
  try
    if SaveToStrings(vBufferString) then
    begin
      vBufferString.SaveToStream(S, SaveEncoding);
      Result := True;
    end;
  finally
    vBufferString.Free;
  end;
{$ELSE}
var
  I: Integer;
{$IFDEF SGDEL_2009}
  vBom: TBytes;
{$ENDIF}
begin
  Result := True;
  try
{$IFDEF SGDEL_2009}
    vBom := SaveEncoding.GetPreamble;
    S.Write(vBom[0], Length(vBom));
{$ENDIF}
    for I := 0 to FROOT.Count - 1 do
      FROOT.Nodes[I].SaveToStream(S);
  except
    Result := False;
  end;
{$ENDIF}
end;

function TsgParser.SaveToString: string;
var
  vBufferString: TStringList;
begin
  Result := '';
  vBufferString := TStringList.Create;
  try
    if SaveToStrings(vBufferString) then
      Result := vBufferString.Text;
  finally
    vBufferString.Free;
  end;
end;

function TsgParser.SaveToStrings(const AStrings: TStrings): Boolean;
{$IFDEF SG_EXPORT_WITH_STRINGS}
var
  I: Integer;
begin
  Result := True;
  try
    for I := 0 to FROOT.Count - 1 do
      FROOT.Nodes[I].SaveToStrings(AStrings);
  except
    Result := False;
  end;
{$ELSE}
var
  vStream: TMemoryStream;
begin
  Result := False;
  AStrings.Clear;
  vStream := TMemoryStream.Create;
  try
    if SaveToStream(vStream) then
    begin
      vStream.Position := 0;
      AStrings.LoadFromStream(vStream{$IFDEF SGDEL_2009}, SaveEncoding{$ENDIF});
      Result := True;
    end;
  finally
    vStream.Free;
  end;
{$ENDIF}
end;

procedure TsgParser.SetDebugInfo(const AValue: Boolean);
begin
  FreeAndNil(FDebugNodePosition);
  if AValue then
    FDebugNodePosition := TsgIntegerList.Create;
end;

procedure TsgParser.SetNodePosition(const ANode: TsgNodeSample);
var
  I, vPos1, vPos2: Integer;
  vPosition: TsgNodeSamplePosition;
begin
  if Assigned(FDebugNodePosition) then
  begin
    FillChar(vPosition, SizeOf(vPosition), 0);
    case FDebugInfoMode of
      0:
        begin
          if FDebugNodePosition.Count > 0 then
          begin
            vPos1 := 0;
            for I := 0 to FDebugNodePosition.Count - 1 do
            begin
              vPos2 := FDebugNodePosition[I];
              if vPos2 > FBufferIndex then
              begin
                vPosition.Y := I + 1;
                vPosition.X := FBufferIndex - vPos1;
                Dec(vPosition.X , Length(ANode.FullName));
                Break;
              end;
              vPos1 := vPos2;
            end;
          end;
        end;
      1:
        begin
          vPosition.X := FBufferIndex;
        end;
    end;
    ANode.SetPosition(vPosition);
  end;
end;

procedure TsgParser.SetXMLString(const AValue: string);
begin
  LoadFromString(AValue);
end;

function TsgParser.Stopped: Boolean;
begin
  Result := Assigned(FStopped) and FStopped{$IFDEF SGFPC}(){$ENDIF};
end;

procedure TsgParser.UnRegisterClass(const AFullName: string);
var
  vIndex: Integer;
begin
  vIndex := FRegisterNodes.IndexOf(AFullName);
  if vIndex > -1 then
    FRegisterNodes.Delete(vIndex);
end;

{$IFDEF SGDEL_2009}

{ TsgNodeEnumeratorBase }

constructor TsgNodeEnumeratorBase.Create(const ANodeList: TsgNodeList);
begin
  inherited Create;
  FList := ANodeList;
  FIndex := -1;
end;

function TsgNodeEnumeratorBase.GetCurrent: TObject;
begin
  Result := FList.List[FIndex];
end;

function TsgNodeEnumeratorBase.GetEnumerator: IEnumerator;
begin
  Result := Self;
end;

function TsgNodeEnumeratorBase.MoveNext: Boolean;
begin
  Result := (FList <> nil) and (FIndex < FList.Count - 1);
  if Result then
    Inc(FIndex);
end;

procedure TsgNodeEnumeratorBase.Reset;
begin
  FIndex := -1;
end;

{ TsgNodeEnumerator }

function TsgNodeEnumerator.GetCurrent: TsgNodeSample;
begin
  Result := FList.GetNode(FIndex);
end;

function TsgNodeEnumerator.GetEnumerator: IEnumerator<TsgNodeSample>;
begin
  Result := Self;
end;

{$ENDIF}

{ TsgData }

procedure TsgData.ClearValue;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case FValue.DType of
    dtString:
      begin
        FValue.AsString^ := '';
        Dispose(FValue.AsString);
      end;
    dtTF2DPoint: Dispose(FValue.AsF2DPoint);
    dtTFPoint:   Dispose(FValue.AsFPoint);
    dtRect:      Dispose(FValue.AsRect);
    dtTF2DRect:  Dispose(FValue.AsF2DRect);
    dtTFRect:    Dispose(FValue.AsFRect);
    dtVersion:   Dispose(FValue.AsVersion);
    dtColorCAD:
      begin
        if FValue.AsColorCAD <> nil then
        begin
          FValue.AsColorCAD^.AlbumString := '';
          Dispose(FValue.AsColorCAD);
        end;
      end;
{$IFNDEF SG_DATA64}
    dtHandle:    Dispose(FValue.AsHandle);
    dtInt64:     Dispose(FValue.AsInt64);
    dtDouble:    Dispose(FValue.AsDouble);
    dtPoint:     Dispose(FValue.AsPoint);
{$ENDIF}
  end;
  FillChar(FValue, SizeOf(FValue), 0);
{$ELSE}
   FValue := '';
{$ENDIF}
end;

procedure TsgData.Assign(const AData: TsgData);
{$IFDEF SG_XML_DATA_TYPE}
var
  vValue: Pointer;
begin
  ClearValue;
  case AData.FValue.DType of
{$IFNDEF SG_DATA64}
    dtHandle, dtInt64, dtDouble, dtPoint,
{$ENDIF}
    dtString, dtTF2DPoint, dtTFPoint,
    dtRect, dtTF2DRect, dtTFRect, dtVersion, dtColorCAD:
      vValue := AData.FValue.Value;
  else
    vValue := @AData.FValue.Value;
  end;
  SetValueWithType(AData.FValue.DType, vValue);
{$ELSE}
begin
  FValue := AData.FValue;
{$ENDIF}
end;

function TsgData.Compare(const AData: TsgData;
  const ACaseSensitivity: Boolean = True): Integer;
begin
  if ACaseSensitivity then
    Result := CompareStr(ValueAsStr, AData.ValueAsStr)
  else
    Result := CompareText(ValueAsStr, AData.ValueAsStr);
end;

constructor TsgData.Create;
begin
  inherited Create;
end;

destructor TsgData.Destroy;
begin
  ClearValue;
  inherited Destroy;
end;

class procedure TsgData.DoError(const AMessage: string);
begin
  raise EConvertError.Create(AMessage);
end;

class function TsgData.GetAsBool(const AData: TsgData): Boolean;
begin
  if AData <> nil then
    Result := AData.ValueAsBool
  else
  begin
    Result := False;
    DoError('');
  end;
end;

class function TsgData.GetAsByte(const AData: TsgData): Byte;
begin
  if AData <> nil then
    Result := AData.ValueAsByte
  else
    Result := 0;
end;

class function TsgData.GetAsDouble(const AData: TsgData): Double;
begin
  if AData <> nil then
    Result := AData.ValueAsDouble
  else
    Result := 0;
end;

class function TsgData.GetAsF2DRect(const AData: TsgData): TF2DRect;
begin
  if AData <> nil then
    Result := AData.ValueAsF2DRect
  else
    Result := cnstF2DRectZero;
end;

class function TsgData.GetAsF2DPoint(const AData: TsgData): TF2DPoint;
begin
  if AData <> nil then
    Result := AData.ValueAsF2DPoint
  else
    Result := cnstF2DPointZero;
end;

class function TsgData.GetAsFPoint(const AData: TsgData): TFPoint;
begin
  if AData <> nil then
    Result := AData.ValueAsFPoint
  else
    Result := cnstFPointZero;
end;

class function TsgData.GetAsFRect(const AData: TsgData): TFRect;
begin
  if AData <> nil then
    Result := AData.ValueAsFRect
  else
    Result := cnstFRectZero;
end;

class function TsgData.GetAsHandle(const AData: TsgData): UInt64;
begin
  if AData <> nil then
    Result := AData.ValueAsHandle
  else
    Result := 0;
end;

class function TsgData.GetAsInt(const AData: TsgData): Integer;
begin
  if AData <> nil then
    Result := AData.ValueAsInt
  else
    Result := 0;
end;

class function TsgData.GetAsInt64(const AData: TsgData): Int64;
begin
  if AData <> nil then
    Result := AData.ValueAsInt64
  else
    Result := 0;
end;

class function TsgData.GetAsPointer(const AData: TsgData): Pointer;
begin
  if AData <> nil then
    Result := AData.ValueAsPointer
  else
    Result := nil;
end;

class function TsgData.GetAsStr(const AData: TsgData): string;
begin
  if AData <> nil then
    Result := AData.ValueAsStr
  else
    Result := '';
end;

class function TsgData.GetAsText(const AData: TsgData): string;
begin
  if AData <> nil then
    Result := AData.ValueAsText
  else
    Result := '';
end;

class function TsgData.GetAsGuid(const AData: TsgData): TGUID;
begin
  if AData <> nil then
    Result := AData.ValueAsGuid
  else
    Result := GUID_NULL;
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
class function TsgData.GetAsVariant(const AData: TsgData): Variant;
begin
  if AData <> nil then
    Result := AData.ValueAsVariant
  else
  begin
    Result := 0;
  end;
end;
{$ENDIF}

function TsgData.GetMeasurements: TsgUnitsOfMeasurements;
begin
  Result := umUndefined;
{$IFDEF SG_XML_DATA_TYPE}
  if FValue.DType = dtString then
    Result := GetUnitsMeasurements(GetValueAsStr);
{$ELSE}
  if Length(FValue) > 0 then
    Result := GetUnitsMeasurements(FValue);
{$ENDIF}
end;

function TsgData.GetValueAsBool: Boolean;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtBool:   Result := Value.AsBool;
    dtString:
      begin
{$ENDIF}
        if not StrToBool(ValueAsStr, Result) then
          DoError(cnstIncorrectType);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsBool(Result);
      end;
  else
    Result := False;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsByte: Byte;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtByte:   Result := Value.AsByte;
    dtString:
      begin
{$ENDIF}
        Result := GetValueAsInt;
{$IFDEF SG_XML_DATA_TYPE}
        if Measurements = umUndefined  then
          SetValueAsByte(Result);
      end;
  else
   Result := 0;
   DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsColor: TColor;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtColor:   Result := Value.AsColor;
    dtString:
      begin
{$ENDIF}
        Result := sgStrToColorHex(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsColor(Result);
      end;
  else
   Result := 0;
   DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsColorCAD: TsgColorCAD;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtColorCAD:   Result := Value.AsColorCAD^;
    dtString:
      begin
{$ENDIF}
        Result := sgStrToColorCAD(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsColorCAD(Result);
      end;
  else
    Result := cnstColorCADByLayer;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsDouble: Double;
var
  vStr: string;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtDouble:   Result := Value.AsDouble{$IFNDEF SG_DATA64}^{$ENDIF};
    dtString:
      begin
{$ENDIF}
        vStr := ValueAsStr;
        if Length(vStr) > 0 then
          Result := StrToVal(vStr)
        else
          Result := 0;
{$IFDEF SG_XML_DATA_TYPE}
        if Measurements = umUndefined  then
          SetValueAsDouble(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsF2DRect: TF2DRect;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtTF2DRect:   Result := Value.AsF2DRect^;
    dtString:
      begin
{$ENDIF}
        Result := StrToF2DRect(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsF2DRect(Result);
      end;
  else
    Result := cnstF2DRectZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsFPoint: TFPoint;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtTFPoint:   Result := Value.AsFPoint^;
    dtString:
      begin
{$ENDIF}
        Result := StrToFPoint(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsFPoint(Result);
      end;
  else
    Result := cnstFPointZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsF2DPoint: TF2DPoint;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtTF2DPoint:   Result := Value.AsF2DPoint^;
    dtString:
      begin
{$ENDIF}
        Result := MakeF2DPointFrom3D(StrToFPoint(ValueAsStr));
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsF2DPoint(Result);
      end;
  else
    Result := cnstF2DPointZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsFRect: TFRect;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtTFRect:   Result := Value.AsFRect^;
    dtString:
      begin
{$ENDIF}
        Result := StrToFRect(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsFRect(Result);
      end;
  else
    Result := cnstFRectZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsHandle: UInt64;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtHandle:   Result := Value.AsHandle{$IFNDEF SG_DATA64}^{$ENDIF};
    dtString:
      begin
{$ENDIF}
        Result := StrToHandle(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsHandle(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsInt: Integer;
var
  vStr: string;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtInteger:   Result := Value.AsInteger;
    dtString:
      begin
{$ENDIF}
        vStr := ValueAsStr;
        if Length(vStr) > 0 then
          Result := StrToIntDef(vStr, 0)
        else
          Result := 0;
{$IFDEF SG_XML_DATA_TYPE}
        if Measurements = umUndefined  then
          SetValueAsInt(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsInt64: Int64;
var
  vStr: string;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtInt64:   Result := Value.AsInt64{$IFNDEF SG_DATA64}^{$ENDIF};
    dtString:
      begin
{$ENDIF}
        vStr := ValueAsStr;
        if Length(vStr) > 0 then
          Result := StrToInt64Def(vStr, 0)
        else
          Result := 0;
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsInt64(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsMatrix: TFMatrix;
begin
  Result := StrToFMatrix(ValueAsStr);
end;

function TsgData.GetValueAsPoint: TPoint;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtPoint:   Result := Value.AsPoint{$IFNDEF SG_DATA64}^{$ENDIF};
    dtString:
      begin
{$ENDIF}
        Result := StrToTPoint(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsPoint(Result);
      end;
  else
    Result := cnstPointZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsPointer: Pointer;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtPointer:   Result := Value.AsPointer;
    dtString:
      begin
{$ENDIF}
        Result := StrToPointer(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsPointer(Result);
      end;
  else
    Result := nil;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsRect: TRect;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtRect:   Result := Value.AsRect^;
    dtString:
      begin
{$ENDIF}
        Result := StrToTRect(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsRect(Result);
      end;
  else
    Result := cnstRectZero;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsVersion: TsgVersion;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtVersion:   Result := Value.AsVersion^;
    dtString:
      begin
{$ENDIF}
        Result := StrToVersion(ValueAsStr);
{$IFDEF SG_XML_DATA_TYPE}
        SetValueAsVersion(Result);
      end;
  else
    Result := cnstXMLIntfaceVersion;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsWord: Word;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtWord:   Result := Value.AsWord;
    dtString:
      begin
{$ENDIF}
        Result := GetValueAsInt;
{$IFDEF SG_XML_DATA_TYPE}
        if Measurements = umUndefined  then
          SetValueAsWord(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsSingle: Single;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtSingle:   Result := Value.AsSingle;
    dtDouble:   Result := Value.AsDouble;
    dtString:
      begin
{$ENDIF}
        Result := GetValueAsDouble;
{$IFDEF SG_XML_DATA_TYPE}
        if Measurements = umUndefined  then
          SetValueAsSingle(Result);
      end;
  else
    Result := 0;
    DoError(cnstIncorrectType);
  end
{$ENDIF}
end;

function TsgData.GetValueAsStr: string;
begin
{$IFDEF SG_XML_DATA_TYPE}
  Result := '';
  case Value.DType of
    dtBool:       Result := BoolToStr(Value.AsBool);
    dtByte:       Result := IntToStr(Value.AsByte);
    dtWord:       Result := IntToStr(Value.AsWord);
    dtInteger:    Result := IntToStr(Value.AsInteger);
    dtColor:      Result := sgColorHexToStr(Value.AsColor);
    dtSingle:     Result := ValToStr(Value.AsSingle);
    dtPointer:    Result := PointerToStr(Value.AsPointer);
    dtString:     Result := Value.AsString^;
    dtTF2DPoint:  Result := FPointToStr(MakeFPointFrom2D(Value.AsF2DPoint^));
    dtTFPoint:    Result := FPointToStr(Value.AsFPoint^);
    dtRect:       Result := TRectToStr(Value.AsRect^);
    dtTF2DRect:   Result := F2DRectToStr(Value.AsF2DRect^);
    dtTFRect:     Result := FRectToStr(Value.AsFRect^);
    dtVersion:    Result := VersionToStr(Value.AsVersion^);
    dtColorCAD:   Result := sgColorCADToStr(Value.AsColorCAD^);

    dtHandle:     Result := HandleToStr(Value.AsHandle{$IFNDEF SG_DATA64}^{$ENDIF});
    dtInt64:      Result := IntToStr(Value.AsInt64{$IFNDEF SG_DATA64}^{$ENDIF});
    dtDouble:     Result := ValToStr(Value.AsDouble{$IFNDEF SG_DATA64}^{$ENDIF});
    dtPoint:      Result := TPointToStr(Value.AsPoint{$IFNDEF SG_DATA64}^{$ENDIF});
  end;
{$ELSE}
  Result := Value;
{$ENDIF}
end;

//ValueAsStr with Ansi Codes!!!
function TsgData.GetValueAsText: string;
var
  vStr: string;
begin
  vStr := ValueAsStr;
  if Length(vStr) > 0 then
  begin
    Result := ReplaceASCICode(vStr, True);
    Result := ReplaceAnsiCodesEx(Result);
  end
  else
    Result := '';
end;

function TsgData.GetValueAsGuid: TGUID;
begin
  Result := StrToGUID(Trim(ValueAsStr));
end;

function TsgData.IsEqual(const AData: TsgData;
  const ACaseSensitivity: Boolean = True): Boolean;
begin
  Result := Compare(AData, ACaseSensitivity) = 0;
end;

class procedure TsgData.SetAsBool(const AData: TsgData; const AValue: Boolean);
begin
  if AData <> nil then
    AData.ValueAsBool := AValue;
end;

class procedure TsgData.SetAsByte(const AData: TsgData; const AValue: Byte);
begin
  if AData <> nil then
    AData.ValueAsByte := AValue;
end;

class procedure TsgData.SetAsDouble(const AData: TsgData; const AValue: Double);
begin
  if AData <> nil then
    AData.ValueAsDouble := AValue;
end;

class procedure TsgData.SetAsF2DRect(const AData: TsgData; const AValue: TF2DRect);
begin
  if AData <> nil then
    AData.ValueAsF2DRect := AValue;
end;

class procedure TsgData.SetAsF2DPoint(const AData: TsgData; const AValue: TF2DPoint);
begin
  if AData <> nil then
    AData.ValueAsF2DPoint := AValue;
end;

class procedure TsgData.SetAsFPoint(const AData: TsgData; const AValue: TFPoint);
begin
  if AData <> nil then
    AData.ValueAsFPoint := AValue;
end;

class procedure TsgData.SetAsFRect(const AData: TsgData; const AValue: TFRect);
begin
  if AData <> nil then
    AData.ValueAsFRect := AValue;
end;

class procedure TsgData.SetAsHandle(const AData: TsgData; const AValue: UInt64);
begin
  if AData <> nil then
    AData.ValueAsHandle:= AValue;
end;

class procedure TsgData.SetAsInt(const AData: TsgData; const AValue: Integer);
begin
  if AData <> nil then
    AData.ValueAsInt := AValue;
end;

class procedure TsgData.SetAsInt64(const AData: TsgData; const AValue: Int64);
begin
  if AData <> nil then
    AData.ValueAsInt64 := AValue;
end;

class procedure TsgData.SetAsPointer(const AData: TsgData;
  const AValue: Pointer);
begin
  if AData <> nil then
    AData.ValueAsPointer := AValue;
end;

class procedure TsgData.SetAsStr(const AData: TsgData; const AValue: string);
begin
  if AData <> nil then
    AData.ValueAsStr := AValue;
end;

class procedure TsgData.SetAsText(const AData: TsgData; const AValue: string);
begin
  if AData <> nil then
    AData.ValueAsText := AValue;
end;

class procedure TsgData.SetAsGuid(const AData: TsgData; const AValue: TGUID);
begin
  if AData <> nil then
    AData.ValueAsGuid := AValue;
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
class procedure TsgData.SetAsVariant(const AData: TsgData; const AValue: Variant);
begin
  if AData <> nil then
    AData.ValueAsVariant := AValue;
end;
{$ENDIF}

procedure TsgData.SetValueAsBool(const AValue: Boolean);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtBool, @AValue);
{$ELSE}
  Value := BoolToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsByte(const AValue: Byte);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtByte, @AValue);
{$ELSE}
  SetValueAsInt(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsColor(const AValue: TColor);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtColor, @AValue);
{$ELSE}
  Value := ColorHexToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsColorCAD(const AValue: TsgColorCAD);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtColorCAD, @AValue);
{$ELSE}
  Value := ColorCADToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsDouble(const AValue: Double);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtDouble, @AValue);
{$ELSE}
  Value := ValToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsF2DRect(const AValue: TF2DRect);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtTF2DRect, @AValue);
{$ELSE}
  Value := F2DRectToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsFPoint(const AValue: TFPoint);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtTFPoint, @AValue);
{$ELSE}
  Value := FPointToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsF2DPoint(const AValue: TF2DPoint);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtTF2DPoint, @AValue);
{$ELSE}
  Value := FPointToStr(MakeFPointFrom2D(AValue));
{$ENDIF}
end;

procedure TsgData.SetValueAsFRect(const AValue: TFRect);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtTFRect, @AValue);
{$ELSE}
  Value := FRectToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsHandle(const AValue: UInt64);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtHandle, @AValue);
{$ELSE}
  Value := HandleToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsInt(const AValue: Integer);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtInteger, @AValue);
{$ELSE}
  Value := IntToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsInt64(const AValue: Int64);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtInt64, @AValue);
{$ELSE}
  Value := IntToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsMatrix(const Value: TFMatrix);
begin
  ValueAsStr := FMatrixToStr(Value, True);
end;

procedure TsgData.SetValueAsPoint(const AValue: TPoint);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtPoint, @AValue);
{$ELSE}
  Value := TPointToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsPointer(const AValue: Pointer);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtPointer, @AValue);
{$ELSE}
  Value := PointerToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsVersion(const AValue: TsgVersion);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtVersion, @AValue);
{$ELSE}
  Value := VersionToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsSingle(const AValue: Single);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtSingle, @AValue);
{$ELSE}
  SetValueAsDouble(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsStr(const AValue: string);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtString, @AValue);
{$ELSE}
  Value := AValue;
{$ENDIF}
end;

//AValue with WEB Codes!!!
procedure TsgData.SetValueAsText(const AValue: string);
var
  vStr: string;
begin
  vStr := AValue;
  if Length(vStr) > 0 then
  begin
    vStr := ReplaceWebCodeEx(vStr);
//    vStr := ReplaceASCICode(vStr, False);
  end;
  ValueAsStr := vStr;
end;

procedure TsgData.SetValueAsGuid(const AValue: TGUID);
begin
  ValueAsStr := GuidToStr(AValue);
end;

procedure TsgData.SetValueAsRect(const AValue: TRect);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtRect, @AValue);
{$ELSE}
  Value := TRectToStr(AValue);
{$ENDIF}
end;

procedure TsgData.SetValueAsWord(const AValue: Word);
begin
{$IFDEF SG_XML_DATA_TYPE}
  SetValueWithType(dtWord, @AValue);
{$ELSE}
  SetValueAsInt(AValue);
{$ENDIF}
end;

function TsgData.SetValueTyped(const AType: TsgDataType;
  const AValue: Pointer): Boolean;
begin
  Result := True;
  case AType of
    dtBool:       ValueAsBool := PBoolean(AValue)^;
    dtByte:       ValueAsByte := PByte(AValue)^;
    dtWord:       ValueAsWord := PWord(AValue)^;
    dtInteger:    ValueAsInt := PInteger(AValue)^;
    dtColor:      ValueAsColor := PsgSystemColor(AValue)^;
    dtSingle:     ValueAsSingle := PSingle(AValue)^;
    dtPointer:    ValueAsPointer := PPointer(AValue)^;
    //dtString:     ValueAsStr := PChar(AValue)^;
    dtTF2DPoint:  ValueAsF2DPoint := PF2DPoint(AValue)^;
    dtTFPoint:    ValueAsFPoint := PFPoint(AValue)^;
    dtRect:       ValueAsRect := PRect(AValue)^;
    dtTF2DRect:   ValueAsF2DRect := PF2DRect(AValue)^;
    dtTFRect:     ValueAsFRect := PFRect(AValue)^;
    dtVersion:    ValueAsVersion := PsgVersion(AValue)^;
    dtColorCAD:   ValueAsColorCAD := PsgColorCAD(AValue)^;
    dtHandle:     ValueAsHandle := PUInt64(AValue)^;
    dtInt64:      ValueAsInt64 := PInt64(AValue)^;
    dtDouble:     ValueAsDouble := PDouble(AValue)^;
    dtPoint:      ValueAsPoint := PPoint(AValue)^;
  else
    Result := False;
  end;
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}
function TsgData.GetValueAsVariant: Variant;
begin
{$IFDEF SG_XML_DATA_TYPE}
  case Value.DType of
    dtBool:      Result := Value.AsBool;
    dtByte:      Result := Value.AsByte;
    dtWord:      Result := Value.AsWord;
    dtInteger:   Result := Value.AsInteger;
    dtColor:     Result := Value.AsColor;
    dtSingle:    Result := Value.AsSingle;
//    dtPointer:   Result := TsgPointer(Value.AsPointer);
    dtDouble:    Result := Value.AsDouble{$IFNDEF SG_DATA64}^{$ENDIF};
//    dtHandle:    Result := Value.AsHandle{$IFNDEF SG_DATA64}^{$ENDIF};
    dtInt64:     Result := Value.AsInt64{$IFNDEF SG_DATA64}^{$ENDIF};
  else
{$ENDIF}
    Result := ValueAsStr;
{$IFDEF SG_XML_DATA_TYPE}
  end;
{$ENDIF}
end;

procedure TsgData.SetValueAsVariant(const AValue: Variant);
var
  vVarType: TVarType;
  vStr: string;
begin
  vVarType := VarType(AValue) and varTypeMask;
{$IFDEF SG_XML_DATA_TYPE}
  case vVarType of
    varEmpty:    ValueAsStr := '';
    varBoolean:  ValueAsBool := AValue;
    varByte:     ValueAsByte := AValue;
    varWord:     ValueAsWord := AValue;
    varInteger:  ValueAsInt := AValue;
    varSingle:   ValueAsSingle := AValue;
    varDouble:   ValueAsDouble := AValue;
    varUInt64:   ValueAsInt64 := AValue;
  else
    vStr := AValue;
    ValueAsStr := vStr;
  end;
{$ELSE}
  case vVarType of
    varEmpty: vStr := '';
  else
    vStr := AValue;
  end;
  ValueAsStr := vStr;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF SG_XML_DATA_TYPE}
function TsgData.GetValueType: TsgDataType;
begin
  Result := FValue.DType;
end;

function TsgData.HasValue: Boolean;
begin
  case FValue.DType of
    dtString:  Result := Length(ValueAsStr) > 0;
  else
    Result := True;
  end;
end;

procedure TsgData.NewValueWithType(const ADataType: TsgDataType);
begin
  ClearValue;
  FValue.DType := ADataType;
  case FValue.DType of
    dtString:    New(FValue.AsString);
    dtTF2DPoint: New(FValue.AsF2DPoint);
    dtTFPoint:   New(FValue.AsFPoint);
    dtRect:      New(FValue.AsRect);
    dtTF2DRect:  New(FValue.AsF2DRect);
    dtTFRect:    New(FValue.AsFRect);
    dtVersion:   New(FValue.AsVersion);
    dtColorCAD:  New(FValue.AsColorCAD);
{$IFNDEF SG_DATA64}
    dtHandle:    New(FValue.AsHandle);
    dtInt64:     New(FValue.AsInt64);
    dtDouble:    New(FValue.AsDouble);
    dtPoint:     New(FValue.AsPoint);
{$ENDIF}
  end;
end;

procedure TsgData.SetValueWithType(const ADataType: TsgDataType; const AValue: Pointer);
begin
  if Value.DType = ADataType then
  begin
    case Value.DType of
      dtBool:      FValue.AsBool := {$IFNDEF SGFPC} PBoolean(AValue)^ {$ELSE} Boolean(PBoolean(AValue)^) {$ENDIF};
      dtByte:      FValue.AsByte := PByte(AValue)^;
      dtWord:      FValue.AsWord := PWord(AValue)^;
      dtInteger:   FValue.AsInteger := PInteger(AValue)^;
      dtColor:     FValue.AsColor := PColor(AValue)^;
      dtSingle:    FValue.AsSingle := PSingle(AValue)^;
      dtPointer:   FValue.AsPointer := PPointer(AValue)^;
      dtString:    FValue.AsString^ := PString(AValue)^;
      dtTF2DPoint: FValue.AsF2DPoint^ := PF2DPoint(AValue)^;
      dtTFPoint:   FValue.AsFPoint^ := PFPoint(AValue)^;
      dtRect:      FValue.AsRect^ := PRect(AValue)^;
      dtTF2DRect:  FValue.AsF2DRect^ := PF2DRect(AValue)^;
      dtTFRect:    FValue.AsFRect^ := PFRect(AValue)^;
      dtVersion:   FValue.AsVersion^ := PsgVersion(AValue)^;
      dtColorCAD:  FValue.AsColorCAD^ := PsgColorCAD(AValue)^;

      dtHandle:    FValue.AsHandle{$IFNDEF SG_DATA64}^{$ENDIF} := PUInt64(AValue)^;
      dtInt64:     FValue.AsInt64{$IFNDEF SG_DATA64}^{$ENDIF} := PInt64(AValue)^;
      dtDouble:    FValue.AsDouble{$IFNDEF SG_DATA64}^{$ENDIF} := PDouble(AValue)^;
      dtPoint:     FValue.AsPoint{$IFNDEF SG_DATA64}^{$ENDIF} := PPoint(AValue)^;
    else
      FValue.Value := AValue;
    end;
  end
  else
  begin
    if ((ADataType = dtString) or (Value.DType in [dtUndefined, dtString])) or
       ((ADataType in [dtSingle, dtDouble]) and (Value.DType in [dtSingle, dtDouble])) then
    begin
      NewValueWithType(ADataType);
      SetValueWithType(ADataType, AValue);
    end
    else
      DoError(cnstIncorrectType);
  end;
end;
{$ENDIF}


{ TsgNodeIterator }

constructor TsgNodeIterator.Create(const ANode: TsgNodeSample;
  const AComparer: TsgFindNodeCustom);
var
  vNodes: TsgNodeList;
begin
  FRoot:= ANode;
  vNodes := nil;
  if FRoot is TsgNode then
    vNodes := TsgNode(FRoot).FChilds;
  Create(vNodes, AComparer);
end;

function TsgNodeIterator.Compare(const ANode: TsgNodeSample): Boolean;
begin
  Result := FComparer.Compare(ANode);
end;

constructor TsgNodeIterator.Create(const ANodes: TsgNodeList;
  const AComparer: TsgFindNodeCustom);
begin
  FComparer := AComparer;
  FRootNotes := ANodes;
  FFindNodes := TsgObjectList.Create;
  FFindTree := TsgObjectList.Create;
end;

destructor TsgNodeIterator.Destroy;
begin
  FreeAndNil(FComparer);
  FreeAndNil(FFindNodes);
  FreeAndNil(FFindTree);
  inherited;
end;

function TsgNodeIterator.Find(const AFindCount: Integer = -1): Integer;
var
  I: Integer;
begin
  FFindCount := AFindCount;
  FFindNodes.Clear;
  if Assigned(FFindTree) then
  begin
    FFindCount := 1;
    FFindTree.Clear;
  end;
  try
    if Assigned(FRootNotes) then
    begin
      for I := 0 to FRootNotes.Count - 1 do
      begin
        if FindChildNode(FRootNotes.Nodes[I]) then
        begin
          if BreakFind then
            Break;
        end;
      end;
    end
    else
    begin
      if Assigned(FRoot) then
        FindChildNode(FRoot);
    end;
  finally
    Result := FFindNodes.Count;
  end;
end;

function TsgNodeIterator.FindChildNode(const ANode: TsgNodeSample): Boolean;
var
  I: Integer;
  vNode: TsgNodeSample;
begin
  Result := False;
  if Assigned(FFindTree) then
    FFindTree.Add(ANode);
  for I := 0 to ANode.ChildNodesCount - 1 do
  begin
    vNode := ANode.ChildNodes[I];
    if Compare(vNode) then
    begin
      FFindNodes.Add(vNode);
      Result := True;
      if BreakFind then
        Break;
    end;
    if FindChildNode(vNode) then
    begin
      Result := True;
      if BreakFind then
        Break;
    end;
  end;
  if (not Result) and Assigned(FFindTree) then
   FFindTree.Count := FFindTree.Count - 1;
end;

function TsgNodeIterator.BreakFind: Boolean;
begin
  Result := (FFindCount > -1) and (FFindCount <= FFindNodes.Count);
end;

function TsgNodeIterator.GetFindNotAll: Boolean;
begin
  Result := Assigned(FFindTree);
end;

procedure TsgNodeIterator.SetFindNotAll(const Value: Boolean);
begin
  FreeAndNil(FFindTree);
  if Value then
    FFindTree := TsgObjectList.Create;
end;

{ TsgFindByAttribute }

function TsgFindByAttribute.Compare(const ANode: TsgNodeSample): Boolean;
var
  vAttrib: TsgNodeSample;
begin
  Result := False;
  vAttrib := ANode.GetAttributeByName(Name);
  if Assigned(vAttrib) then
  begin
    if IgnoreValue then
      Result := True
    else
    begin
      if CaseSensitivity then
        Result := vAttrib.ValueAsStr = Value
      else
        Result := CompareText(vAttrib.ValueAsStr, Value) = 0;
    end;
  end;
end;

constructor TsgFindByAttribute.Create(const AName, AValue: string;
  const AIgnore: Boolean = False; const ACaseSensitivity: Boolean = True);
begin
  inherited Create;
  Name := AName;
  Value := AValue;
  IgnoreValue := AIgnore;
  CaseSensitivity := ACaseSensitivity;
end;

{ TsgFindByNode }

function TsgFindByNode.Compare(const ANode: TsgNodeSample): Boolean;
begin
  if CaseSensitivity then
    Result := ANode.Name = Name
  else
    Result := CompareText(ANode.Name, Name) = 0;
end;

constructor TsgFindByNode.Create(const AName: string;
  const ACaseSensitivity: Boolean = True);
begin
  inherited Create;
  Name := AName;
  CaseSensitivity := ACaseSensitivity;
end;

{$IFDEF SG_XML_USE_TYPE_VARIANT}

{ TsgNodeSampleFrame }

function TsgNodeSampleFrame.AddChild(const NodeName: string): IsgNodeFrame;
begin
  Result := TsgNodeSampleFrame.CreateFrame(FNode.AddChildNV(NodeName));
end;

procedure TsgNodeSampleFrame.ClearChildNodes;
begin
  FNode.Childs.ClearNodes;
end;

class function TsgNodeSampleFrame.CreateFrame(const ANode: TsgNode): IsgNodeFrame;
var
  vFrame: TsgNodeSampleFrame;
begin
  vFrame := TsgNodeSampleFrame.Create;
  vFrame.FNode := ANode;
  Result := vFrame;
end;

function TsgNodeSampleFrame.ChildNodeCount: Integer;
begin
  Result := FNode.ChildNodesCount;
end;

function TsgNodeSampleFrame.FindChildNode(const NodeName: string): IsgNodeFrame;
var
  vNode: TsgNodeSample;
begin
  vNode := FNode.GetChildByName(NodeName);
  if Assigned(vNode) then
    Result := CreateFrame(TsgNode(vNode))
  else
    Result := nil;
end;

function TsgNodeSampleFrame.GetAttribute(const AttrName: string): OleVariant;
var
  vNode: TsgNodeSample;
begin
  {$IFNDEF SGFPC}System.{$ENDIF}VarClear(Result);
  vNode := FNode.GetAttributeByName(AttrName);
  if Assigned(vNode) then
    Result := vNode.ValueAsVariant;
end;

function TsgNodeSampleFrame.GetChildNode(const AIndex: Integer): IsgNodeFrame;
begin
  Result := TsgNodeSampleFrame.CreateFrame(TsgNode(FNode.ChildNodes[AIndex]));
end;

function TsgNodeSampleFrame.GetChildValue(const IndexOrName: string): OleVariant;
var
  vNode: TsgNodeSample;
begin
  vNode := FNode.GetChildByName(IndexOrName);
  if Assigned(vNode) then
    Result := vNode.TextAsVariant
  else
    Result := 0;
end;

function TsgNodeSampleFrame.GetName: string;
begin
  Result := FNode.Name;
end;

procedure TsgNodeSampleFrame.SetAttribute(const AttrName: string; const Value: OleVariant);
var
  vNode: TsgNodeSample;
begin
  vNode := FNode.GetAttributeByName(AttrName);
  if not Assigned(vNode) then
    vNode := FNode.AddAttribNV(AttrName);
  vNode.ValueAsVariant := Value;
end;

procedure TsgNodeSampleFrame.SetChildValue(const IndexOrName: string;
  const Value: OleVariant);
var
  vNode: TsgNodeSample;
begin
  vNode := FNode.GetChildByName(IndexOrName);
  if not Assigned(vNode) then
    vNode := FNode.AddChildNV(IndexOrName);
  vNode.TextAsVariant := Value;
end;
{$ENDIF}

procedure FreeStringsObjects(var Strings: TStrings);
begin
  ClearObjects(Strings);
  FreeAndNil(Strings);
end;

initialization
  KeyWords := nil;
  TableColors := nil;
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
  DictionaryByNames := nil;
  DictionaryByIndexes := nil;
  {$IFDEF SG_OPENING_IN_THEADS}
  DictionaryLock := TCriticalSection.Create;
  CreateDictionaryNames;
  {$ENDIF}
{$ENDIF}

{$IFDEF SG_OPENING_IN_THEADS}
  CreateKeyWords;
  CreateTableColor;
{$ELSE}
  Spliter := TsgStringList.Create;
{$ENDIF}

finalization
  FreeStringsObjects(TStrings(KeyWords));
  FreeStringsObjects(TStrings(TableColors));
{$IFDEF SG_USE_DICTIONARY_BY_NAME}
  FreeStringsObjects(TStrings(DictionaryByNames));
  FreeAndNil(DictionaryByIndexes);
  {$IFDEF SG_OPENING_IN_THEADS}
  FreeAndNil(DictionaryLock);
  {$ENDIF}
{$ENDIF}

{$IFDEF SG_OPENING_IN_THEADS}
{$ELSE}
  FreeAndNil(Spliter);
{$ENDIF}

end.

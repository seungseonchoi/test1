{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                  CGM files TGraphic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit CGM;
{$INCLUDE SGDXF.inc}
//{$DEFINE SG_CGM_DEBUG}

//{$DEFINE SG_CORTONA_VIEWER}

interface

uses

{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  {$UNDEF SG_CGM_DEBUG}
  SyncObjs, FMX.Graphics, sgFMXTypes, sgProxyGraphics,// sgFMCommonFunctions,
{$ELSE}
  Graphics, MVFont,
{$ENDIF}
  SysUtils, Classes, DXFConv, CADImage, sgConsts, Math,
  TTF, sgFunction, sgDecompress, sgXMLParser, sgLists,
  sgComparer//, sgDefinitions
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  ;

const
  SCGMImage = 'CGM image';

var
  CF_CGM: Word;


type
  TsgCgmBackGroundMode = (bgIgnore, bgFrame, bgFill);

  TsgCgmTypeEntity = (cgmNone, cgmArc3Point, cgmArc3PointClose, cgmCircle,
    cgmCircularArcCenter, cgmCircularArcCenterReversed,
    cgmCircularArcCenterClose, cgmEllipse, cgmEllipticalArc,
    cgmEllipticalArcClose, cgmPolygone, cgmPolyline, cgmRectangle, cgmText,
    cgmRestrictedText, cgmAppendText, cgmPolyBezier, cgmDisjtPolyline,
    cgmMarker, cgmConnectingEdge, cgmHyperbolicArc, cgmParabolicArc,
    cgmNonUniformBSpline, cgmNonUniformRationalBSpline, cgmPolyMarker,
    cgmPolygonSet);
  TsgCgmTypeEntities = set of TsgCgmTypeEntity;
  TsgCgmProgressEvent = procedure(Stage: TProgressStage;
    Done,Count: Integer) of object;
  TsgCgmGroupElements = (geNone, geLine, geMarker, geEdge, geText);
  TsgTextPrecision = (tpString, tpCharacter, tpStroke);
  TsgTextPath = (tpaRight, tpaLeft, tpaUp, tpaDown);
  TsgCgmCharacterCodingAnnouncer = (ccaBasic7, ccaBasic8, ccaExtended7,
    ccaExtended8);
  TsgCgmCharacterSetType = (cst94, cst96, cst94Multibyte, cst96Multibyte,
    cstCompleteCode, cstNone);
  TsgCgmVDCType = (vdcInteger, vdcReal);
  TsgCgmIntPrecision = (ip0, ipShortInt, ipByte, ipSmallInt, ipWord, ipInt24,
    ipInteger, ipCardinal);
  TsgCgmScalingMode = (smAbstract, smMetric);
  TsgCgmRealPrecision = (rp0, rpSingle, rpDouble, rpFixed, rpFixed64);
  TsgCgmColorMode = (cmIndexed, cmDirect);
  TsgCgmColorModel = (cmlRGB, cmlCIELAB, cmlCIELUV, cmlCMYK, cmlRGBRelated,
    cmlReserved, cmlIndexes);
  TsgCgmLineType = (ltNone, ltSolid, ltDash, ltDot, ltDashDot, ltDashDotDot,
    ltSingleArrow, ltSingleDot, ltDoubleArrow, ltStitch, ltChain, ltCenter,
    ltHidden, ltPhantom, ltBreakStyle1, ltBreakStyle2, ltReserved);
  TsgCgmWidthSpecMode = (wsmAbsolute, wsmScaled, wsmFractional, wsmMM);
  TsgCgmInteriorStyle = (isHollow, isSolid, isPattern, isHatch, isEmpty,
     isPeometricPattern, isInterpolated);
  TsgCgmElementsList = (elDrawing, elDrawingPlusControl, elVersion2,
    elExtendedPrimitives, elVersion2gksm, elVersion3, elVersion4);
  TsgCgmMarkerType = (mtNone, mtDot, mtPlus, mtStar, mtCircle, mtCross);
  TsgCgmTextHorizontalAlignment = (thaNormal, thaLeft, thaCentre, thaRight,
    thaContinuous);
  TsgCgmTextVerticalAlignment = (tvaNormal, tvaTop, tvaCap, tvaHalf,
    tvaBase, tvaBottom, tvaContinuous);
  TsgCGMReaderType = (crtUnknown, crtTxt, crtBin);

  TsgCgmColor = record
    Model:  TsgCgmColorModel;
    case TsgCgmColorModel of
      cmlIndexes:      (Index: Integer);
      cmlRGB, cmlCMYK: (Components:  array [0..3] of Cardinal);
  end;
  TsgCgmColorArray = array[0..MaxInt div SizeOf(TsgCgmColor) - 1] of TsgCgmColor;
  PsgCgmColorArray = ^TsgCgmColorArray;


  TsgCgmColorScaledComponent = record
    Scale, Offset: Double;
  end;

  PsgCgmColourParametrs = ^TsgCgmColourParametrs;
  TsgCgmColourParametrs = record
    Components: array [0..2] of TsgCgmColorScaledComponent;
    IndexPrecision: Integer;
    MaximumIndex: Integer;
    Model: TsgCgmColorModel;
    Precision: Integer;
    SelectionMode: TsgCgmColorMode;
    ValueMax: Cardinal;
    ValueMin: Cardinal;
  end;

  PsgRegionParametrs = ^TsgRegionParametrs;
  TsgRegionParametrs = record
    ConnectionPoint: TFPoint;
    FirstPointElement: TFPoint;
    IsCloseRegion: Boolean;
    IsConnectionEdge: Boolean;
    IsInitStartPoint: Boolean;
    LastPointElement: TFPoint;
    StartPointRegion: TFPoint; // Start point region
  end;

type
{  |   Word   |
   | p1|p2|p3 |
   |  p1 | p2 | }
  TsgCGMParameters = class
  protected
    FParameters: TsgParametrs;
  public
    constructor Create;
    procedure TwoToThree(AValue1, AValue2: Byte);
    property Parameter1: Byte read FParameters.Param1;
    property Parameter2: Byte read FParameters.Param2;
    property Parameter3: Byte read FParameters.Param3;
  end;

  TsgContainer = class;

  TsgPropertyValue = (pvColour, pvBundleIndex);
  TsgPropertyValues = set of TsgPropertyValue;

  TsgProperty = class
  private
    FColour: TsgCgmColor;
    FBundleIndex: Integer;
    FFlags: TsgPropertyValues;
    function GetGroupElement: TsgCgmGroupElements; virtual;
    procedure SetColour(const Value: TsgCgmColor);
    procedure SetBundleIndex(const Value: Integer);
  public
    procedure Assign(const AProperty: TsgProperty); virtual;
    procedure Reset(AReaderType: TsgCGMReaderType); virtual;
    function IsInitValue(const AProperty: TsgPropertyValue): Boolean;
    property Colour: TsgCgmColor read FColour write SetColour;
    property BundleIndex: Integer read FBundleIndex write SetBundleIndex;
    property GroupElement: TsgCgmGroupElements read GetGroupElement;
  end;

  TsgPropertySpecMode = class(TsgProperty)
  private
    FSpecMode: TsgCgmWidthSpecMode;
  public
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
    property SpecMode: TsgCgmWidthSpecMode read FSpecMode;
  end;

  TsgMarkerPropery = class(TsgPropertySpecMode)
  protected
    function GetDefaultScaleFactor: Double; virtual;
    function GetDefaultWidthMM: Double; virtual;
    function GetGroupElement: TsgCgmGroupElements; override;
  public
    ClippingMode: Integer;
    EType: Integer;
    Width: Double;
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
    property DefaultScaleFactor: Double read GetDefaultScaleFactor;
    property DefaultWidthMM: Double read GetDefaultWidthMM;
  end;

  TsgLineProperty = class(TsgMarkerPropery)
  protected
    function GetDefaultScaleFactor: Double; override;
    function GetDefaultWidthMM: Double; override;
    function GetGroupElement: TsgCgmGroupElements; override;
  public
    PenStyle: Word; // CAP and JOIN
    TypeContinuation: Integer;
    TypeInitialOffset: Integer;
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
  end;

  TsgEdgeProperty = class(TsgLineProperty)
  protected
    function GetGroupElement: TsgCgmGroupElements; override;
  public
    Visibility: Boolean;
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
  end;

  TsgTextProperty = class(TsgProperty)
  protected
    function GetGroupElement: TsgCgmGroupElements; override;
  public
    AlternateCharacterSetIndex: Integer;
    ContinuousHorizontalAlignment: Double;
    ContinuousVerticalAlignment: Double;
    CharacterExpansionFactor: Double;
    CharacterHeight: Double;
    CharacterOrientationBase: TFPoint;
    CharacterOrientationUp: TFPoint;
    CharacterSetIndex: Integer;
    CharacterSpacing: Double;
    HorizontalAlignment: TsgCgmTextHorizontalAlignment;
    RestrictedTextType: Integer;
    SymbolColour: TsgCgmColor;
    SymbolOrientationBase: TFPoint;
    SymbolOrientationUp: TFPoint;
    SymbolSize: Double;
    TextFontIndex: Integer;
    TextPath: Integer;
    TextPrecision: TsgTextPrecision;
    TextScoreType: Integer;
    VerticalAlignment: TsgCgmTextVerticalAlignment;
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
  end;

  TsgHatchProperty = class(TsgPropertySpecMode)
  public
    FillReferencePoint: TFPoint;
    HatchIndex: Integer;
    InteriorStyle: TsgCgmInteriorStyle;
    InterpolatedInterior: Integer;
    PatternIndex: Integer;
    PatternSize: Integer;
    PickIdentifier: Integer;
    procedure Assign(const AProperty: TsgProperty); override;
    procedure Reset(AReaderType: TsgCGMReaderType); override;
  end;

  TsgPrecisions = class
  private
    FCharacterCodingAnnouncer: TsgCgmCharacterCodingAnnouncer;
    FCharacterSetList: TStringList;
    FColourComponents: array [0..2] of TsgCgmColorScaledComponent;
    FColourIndexPrecision: Integer;
    FColourMaximumIndex: Integer;
    FColourModel: TsgCgmColorModel;
    FColourPrecision: Integer;
    FColourSelectionMode: TsgCgmColorMode;
    FColourValueMax: TsgCgmColor;
    FColourValueMin: TsgCgmColor;
    FIndexPrecision: TsgCgmIntPrecision;
    FIntegerPrecision: TsgCgmIntPrecision;
    FNamePrecision: TsgCgmIntPrecision;
    FOwner: TsgContainer;
    FRealPrecision: TsgCgmRealPrecision;
    FScalingMode: TsgCgmScalingMode;
    FScalingModeFactor: Double;
    FVDCExtents: TFRect;
    FVDCIntPrecision: TsgCgmIntPrecision;
    FVDCRealPrecision: TsgCgmRealPrecision;
    FVDCType: TsgCgmVDCType;
    function GetColourComponents(const AIndex: Integer):
      TsgCgmColorScaledComponent;
    function GetEdgeWidthSpecMode: TsgCgmWidthSpecMode;
    function GetInteriorStyleSpecMode: TsgCgmWidthSpecMode;
    function GetLineWidthSpecMode: TsgCgmWidthSpecMode;
    function GetMarkerWidthSpecMode: TsgCgmWidthSpecMode;
    function GetCharacterCodingAnnouncer: TsgCgmCharacterCodingAnnouncer;
    function GetCharacterSetList: TStringList;
    procedure SetColourComponents(const AIndex: Integer;
      const Value: TsgCgmColorScaledComponent);
    procedure SetColourIndexPrecision(const Value: Integer);
    procedure SetColourMaximumIndex(const Value: Integer);
    procedure SetColourModel(const Value: TsgCgmColorModel);
    procedure SetColourPrecision(const Value: Integer);
    procedure SetColourSelectionMode(const Value: TsgCgmColorMode);
    procedure SetColourValueMax(const Value: TsgCgmColor);
    procedure SetColourValueMin(const Value: TsgCgmColor);
    procedure SetEdgeWidthSpecMode(const Value: TsgCgmWidthSpecMode);
    procedure SetIndexPrecision(const Value: TsgCgmIntPrecision);
    procedure SetIntegerPrecision(const Value: TsgCgmIntPrecision);
    procedure SetInteriorStyleSpecMode(const Value: TsgCgmWidthSpecMode);
    procedure SetLineWidthSpecMode(const Value: TsgCgmWidthSpecMode);
    procedure SetMarkerWidthSpecMode(const Value: TsgCgmWidthSpecMode);
    procedure SetNamePrecision(const Value: TsgCgmIntPrecision);
    procedure SetRealPrecision(const Value: TsgCgmRealPrecision);
    procedure SetScalingMode(const Value: TsgCgmScalingMode);
    procedure SetScalingModeFactor(const Value: Double);
    procedure SetVDCExtents(const Value: TFRect);
    procedure SetVDCIntPrecision(const Value: TsgCgmIntPrecision);
    procedure SetVDCRealPrecision(const Value: TsgCgmRealPrecision);
    procedure SetVDCType(const Value: TsgCgmVDCType);
    procedure SetCharacterCodingAnnouncer(
      const Value: TsgCgmCharacterCodingAnnouncer);
  protected
    function GetDefaultVDCExtents(const AType: TsgCgmVDCType): TFRect;
    function GetDefaultColor: TsgCgmColor;
    function GetMaxComponentValue: Cardinal;
    function GetWidth(const AProperty: TsgMarkerPropery;
      const AVDCExtents: TFRect): Double;
  public
    constructor Create(const AOwner: TsgContainer);
    destructor  Destroy; override;
    procedure Assign(const APrecicsion: TsgPrecisions);
    function GetDefaultWidth(AProperty: TsgMarkerPropery = nil): Double;
    procedure Reset(AReaderType: TsgCGMReaderType); virtual;
    property CharacterCodingAnnouncer: TsgCgmCharacterCodingAnnouncer
      read GetCharacterCodingAnnouncer write SetCharacterCodingAnnouncer;
    property CharacterSetList: TStringList read GetCharacterSetList;
    property ColourComponents[const AIndex: Integer]: TsgCgmColorScaledComponent
      read GetColourComponents write SetColourComponents;
    property ColourIndexPrecision: Integer read FColourIndexPrecision
      write SetColourIndexPrecision;
    property ColourMaximumIndex: Integer read FColourMaximumIndex
      write SetColourMaximumIndex;
    property ColourModel: TsgCgmColorModel read FColourModel
      write SetColourModel;
    property ColourPrecision: Integer read FColourPrecision
      write SetColourPrecision;
    property ColourSelectionMode: TsgCgmColorMode read FColourSelectionMode
      write SetColourSelectionMode;
    property ColourValueMax: TsgCgmColor read FColourValueMax
      write SetColourValueMax;
    property ColourValueMin: TsgCgmColor read FColourValueMin
      write SetColourValueMin;
    property EdgeWidthSpecMode: TsgCgmWidthSpecMode read GetEdgeWidthSpecMode
      write SetEdgeWidthSpecMode;
    property IndexPrecision: TsgCgmIntPrecision read FIndexPrecision
      write SetIndexPrecision;
    property IntegerPrecision: TsgCgmIntPrecision read FIntegerPrecision
      write SetIntegerPrecision;
    property InteriorStyleSpecMode: TsgCgmWidthSpecMode
      read GetInteriorStyleSpecMode write SetInteriorStyleSpecMode;
    property LineWidthSpecMode: TsgCgmWidthSpecMode read GetLineWidthSpecMode
      write SetLineWidthSpecMode;
    property MarkerWidthSpecMode: TsgCgmWidthSpecMode
      read GetMarkerWidthSpecMode write SetMarkerWidthSpecMode;
    property NamePrecision: TsgCgmIntPrecision read FNamePrecision
      write SetNamePrecision;
    property RealPrecision: TsgCgmRealPrecision read FRealPrecision
      write SetRealPrecision;
    property ScalingMode: TsgCgmScalingMode read FScalingMode
      write SetScalingMode;
    property ScalingModeFactor: Double read FScalingModeFactor
      write SetScalingModeFactor;
    property VDCExtents: TFRect read FVDCExtents write SetVDCExtents;
    property VDCIntPrecision: TsgCgmIntPrecision read FVDCIntPrecision
      write SetVDCIntPrecision;
    property VDCRealPrecision: TsgCgmRealPrecision read FVDCRealPrecision
      write SetVDCRealPrecision;
    property VDCType: TsgCgmVDCType read FVDCType write SetVDCType;
  end;

  TsgCGMContext = class
  private
    FAspectSourceFlags: Integer;
    FAuxiliaryColour: TsgCgmColor;
    FClipIndicator: Boolean;
    FClipRectangle: TFRect;
    FEdge: TsgEdgeProperty;
    FHatch: TsgHatchProperty;
    FLine: TsgLineProperty;
    FMarker: TsgMarkerPropery;
    FMitreLimit: Integer;
    FProtectionRegionIndicator: Integer;
    FSymbolLibraryIndex: Integer;
    FText: TsgTextProperty;
    FTransparency: Boolean;
    procedure SetAspectSourceFlags(const Value: Integer);
    procedure SetAuxiliaryColour(const Value: TsgCgmColor);
    procedure SetClipIndicator(const Value: Boolean);
    procedure SetClipRectangle(const Value: TFRect);
    procedure SetMitreLimit(const Value: Integer);
    procedure SetProtectionRegionIndicator(const Value: Integer);
    procedure SetSymbolLibraryIndex(const Value: Integer);
    procedure SetTransparency(const Value: Boolean);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(const AContext: TsgCGMContext);
    procedure Reset(AReaderType: TsgCGMReaderType); virtual;
    property AspectSourceFlags: Integer read FSymbolLibraryIndex
      write SetAspectSourceFlags;
    property AuxiliaryColour: TsgCgmColor read FAuxiliaryColour
      write SetAuxiliaryColour;
    property ClipIndicator: Boolean read FClipIndicator write SetClipIndicator;
    property ClipRectangle: TFRect read FClipRectangle write SetClipRectangle;
    property Edge: TsgEdgeProperty read FEdge ;
    property Hatch: TsgHatchProperty read FHatch;
    property Line: TsgLineProperty read FLine;
    property Marker: TsgMarkerPropery read FMarker;
    property MitreLimit: Integer read FAspectSourceFlags write SetMitreLimit;
    property ProtectionRegionIndicator: Integer read FProtectionRegionIndicator
      write SetProtectionRegionIndicator;
    property SymbolLibraryIndex: Integer read FSymbolLibraryIndex
      write SetSymbolLibraryIndex;
    property Text: TsgTextProperty read FText;
    property Transparency: Boolean read FTransparency write SetTransparency;
  end;

  TsgContainer = class
  private
    FContext: TsgCGMContext;
    FPrecision: TsgPrecisions;
    FStackContext: TStringList;
    FTableColor: TsgIntegerList;
    function GetBackgroundColour: TsgColorCAD;
    function GetForegroundColour: TsgColorCAD;
    procedure SetBackgroundColour(const Value: TsgColorCAD);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const AContext: TsgContainer);
    function GetColorByIndex(const AIndex: Integer): Integer;
    procedure Reset(AReaderType: TsgCGMReaderType); virtual;
    property BackgroundColour: TsgColorCAD read GetBackgroundColour
     write SetBackgroundColour;
    property Context: TsgCGMContext read FContext;
    property ForegroundColour: TsgColorCAD read GetForegroundColour;
    property Precision: TsgPrecisions read FPrecision;
    property TableColor: TsgIntegerList read FTableColor;
  end;

  TsgCGMReader = class(TsgCustomMemoryStream)
  private
    FCodePage: Cardinal;
    FMetafileVersion: Integer;
    FContainerCGMData: TsgContainer;
    FContainerSave: TsgContainer;
    FOnReadError: TNotifyEvent;
    FParameters: TsgCGMParameters;

    function GetByte(const AIndex: Integer): Byte;

    class function CompareKeyWordsBin(const AValue1, AValue2: Pointer): Integer;
  protected
    procedure DoReadError;
    function NormalizeColorCgm(const AColor: TsgCgmColor): TsgCgmColor;
    function ConvertColorCGMToColor(const AColor: TsgCgmColor): TsgColorCAD;
    function GetComponentsCount: Integer;
    function GetIntValue(APrecision: TsgCgmIntPrecision = ip0):
      Integer; virtual; abstract;
    function GetNextTagPosition: Integer; virtual; abstract;
    function GetRealValue(APrecision: TsgCgmRealPrecision = rp0):
      Double; virtual; abstract;
    function GetReaderType: TsgCGMReaderType; virtual;
    function GetIsUTF16String : Boolean; virtual;
    procedure SetIsUTF16String(const Value: Boolean); virtual;
    function ReadIntVal(const APrecision: TsgCgmIntPrecision): Integer;
    function ReadRealVal(const APrecision: TsgCgmRealPrecision): Double;
    procedure SpecReadBuffer(const ASize: Integer = 0); virtual; abstract;
    function SpecReadCharacterCodingAnnouncer: Integer; virtual; abstract;
    function SpecReadCharacterSetType: Integer; virtual; abstract;
    function SpecReadColourModel: Integer; virtual; abstract;
    function SpecReadColourSelectionMode: Integer; virtual; abstract;
    function SpecReadInteriorStyle: Integer; virtual; abstract;
    function SpecReadMetafileElementList: Integer; virtual; abstract;
    function SpecReadMetafileElementListCount: Integer; virtual; abstract;
    function SpecReadPrecision_I: TsgCgmIntPrecision; virtual; abstract;
    function SpecReadPrecision_I_Size: Integer; virtual; abstract;
    function SpecReadPrecision_R: TsgCgmRealPrecision; virtual; abstract;
    function SpecReadScalingMode: Integer; virtual; abstract;
    function SpecReadText_Piece: Integer; virtual; abstract;
    function SpecReadTextHorizontalAlignment: Integer; virtual; abstract;
    function SpecReadTextPath: Integer; virtual; abstract;
    function SpecReadTextVerticalAlignment: Integer; virtual; abstract;
    function SpecReadVDCType: Integer; virtual; abstract;
    function SpecReadWidthSpecificationMode: Integer; virtual; abstract;
    property IsUTF16String: Boolean read GetIsUTF16String write SetIsUTF16String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AlignBytes; virtual;
    procedure AssignedParams(const AReader: TsgCGMReader); virtual;
    procedure DoNextTag;
    function GetVDCExtKoef: Double;
    procedure InitInternalParams(const AStream: TStream);
    function IsEndCommand: Boolean; override;
    function IsTagEOF: Boolean; virtual; abstract;
    function IsWeReadNBytes(const ACountByte: Byte = 1): Boolean; virtual; abstract;
    function MetafileElementList: TsgCgmElementsList;
    function Read_nSF: TStringList;
    function ReadByteCount(const ASize: Integer): Integer; override;
    function ReadCCO: Integer;
    procedure ReadCCO_Precision;
    function ReadCD: TsgCgmColor;
    function ReadCI: TsgCgmColor;
    procedure ReadCI_Precision;
    function ReadCloseSpec: Integer; virtual;
    function ReadCO: Integer;
    function ReadCO_Precision: Integer;
    function ReadColor: Cardinal;
    function ReadCgmColor: TsgCgmColor;
    function ReadColorLength: Integer;
    procedure ReadColorModel;
    procedure ReadColorSelectionMode;
    procedure ReadColorValuesExtent;
    procedure ReadCharacterCodingAnnouncer;
    procedure ReadCharacterSetList(const ASetListTable: TObject);
    function ReadDashCap: Integer; virtual;
    function ReadDirection: TPoint; virtual; abstract;
    function ReadE: Integer;
    function ReadI: Integer;
    procedure ReadI_Precision; virtual; abstract;
    function ReadInteriorStyle: TsgCgmInteriorStyle;
    function ReadIX: Integer;
    procedure ReadIX_Precision; virtual; abstract;
    function ReadLineCap: Integer; virtual;
    function ReadLineJoin: Integer; virtual;
    function ReadLineType: Integer;
    function ReadN: Integer;
    procedure ReadN_Precision; virtual; abstract;
    function ReadP: TFPoint;
    function ReadR: Double;
    function ReadFloatingR: Double;
    function ReadFixedR: Double;
    procedure ReadR_Precision; virtual; abstract;
    function ReadRepresentationMode: Integer; virtual; abstract;
    procedure ReadScalingMode;
    function ReadSF(const AIsCgmSF: Boolean = True): string; virtual; abstract;
    function ReadSF_A: AnsiString; virtual; abstract;
    procedure ReadSF_Precision; virtual; abstract;
    function ReadSS(const AMode: TsgCgmWidthSpecMode): Double;
    function ReadTagHeader: Integer; virtual; abstract;
    function ReadTextHorizontalAlignment: TsgCgmTextHorizontalAlignment;
    function ReadTextPath: Integer;
    function ReadTextPiece: Integer;
    function ReadTextVerticalAlignment: TsgCgmTextVerticalAlignment;
    function ReadVDC: Double;
    procedure ReadVDC_I_Precision; virtual; abstract;
    procedure ReadVDC_R_Precision; virtual; abstract;
    procedure ReadVDC_Type;
    function ReadVisibility: Integer; virtual; abstract;
    function ReadWidthSpecificationMode: TsgCgmWidthSpecMode;
    procedure ResetDefault; virtual;
    procedure RestoreColorParams;
    procedure SaveColorParams;
    procedure SeekUndefined; virtual;
    procedure SetRangeColourByPrecision;
    property Bytes[const AIndex: Integer]: Byte read GetByte;
    property Container: TsgContainer read FContainerCGMData;
    property OnReadError: TNotifyEvent read FOnReadError write FOnReadError;
  end;

  TsgCGMReaderBin = class(TsgCGMReader)
  private
    FBuffer: array [1..64] of Byte;
    FBufferSize: Byte;
    FIsUTF16String: Boolean;
    FLocation: Int64;
    FLocations: TsgInt64List;
    FSaveLocation: Int64;
    FTagNextPos: Int64;
    function AddLocation(const ALength: Integer): Boolean;
  protected
    function GetIntValue(APrecision: TsgCgmIntPrecision = ip0):
      Integer; override;
    function GetNextLocation: Integer;
    function GetNextTagPosition: Integer; override;
    function GetRealValue(APrecision: TsgCgmRealPrecision = rp0):
      Double; override;
    function GetReaderType: TsgCGMReaderType; override;
    function GetIsUTF16String : Boolean; override;
    procedure SetIsUTF16String(const Value: Boolean); override;
    function NextLocation: Boolean;
    procedure SpecReadBuffer(const ASize: Integer = 0); override;
    function SpecReadCharacterCodingAnnouncer: Integer; override;
    function SpecReadCharacterSetType: Integer; override;
    function SpecReadColourModel: Integer; override;
    function SpecReadColourSelectionMode: Integer; override;
    function SpecReadInteriorStyle: Integer; override;
    function SpecReadMetafileElementList: Integer; override;
    function SpecReadMetafileElementListCount: Integer; override;
    function SpecReadPrecision_I: TsgCgmIntPrecision; override;
    function SpecReadPrecision_I_Size: Integer; override;
    function SpecReadPrecision_R: TsgCgmRealPrecision; override;
    function SpecReadScalingMode: Integer; override;
    function SpecReadText_Piece: Integer; override;
    function SpecReadTextHorizontalAlignment: Integer; override;
    function SpecReadTextPath: Integer; override;
    function SpecReadTextVerticalAlignment: Integer; override;
    function SpecReadVDCType: Integer; override;
    function SpecReadWidthSpecificationMode: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AlignBytes; override;
    function IsTagEOF: Boolean; override;
    function IsWeReadNBytes(const ACountByte: Byte = 1): Boolean; override;
    function ReadSDR: Integer; override;
    function ReadDirection: TPoint; override;
    procedure ReadI_Precision; override;
    procedure ReadIX_Precision; override;
    procedure ReadN_Precision; override;
    procedure ReadR_Precision; override;
    function ReadRepresentationMode: Integer; override;
    function ReadSF(const AIsCgmSF: Boolean = True): string; override;
    function ReadSF_A: AnsiString; override;
    procedure ReadSF_Precision; override;
    function ReadTagHeader: Integer; override;
    procedure ReadVDC_I_Precision; override;
    procedure ReadVDC_R_Precision; override;
    function ReadVisibility: Integer; override;
    procedure RestorePosition; override;
    procedure SavePosition; override;
    procedure SeekUndefined; override;
  end;

  TsgCGMReaderStr = class(TsgCGMReader)
  private
    FBuffer: TStringList;
    FBufferIndex: Integer;
    FBufferIndexSave: Integer;
    function ReadChar(var ABuffer: Char): Boolean;
    function ReadSpace: Integer;
  protected
    function GetBufferValue: string;
    function GetBufferIntValue: string;
    function GetIntValue(APrecision: TsgCgmIntPrecision = ip0):
      Integer; override;
    function GetNextTagPosition: Integer; override;
    function GetRealValue(APrecision: TsgCgmRealPrecision = rp0):
      Double; override;
    function GetReaderType: TsgCGMReaderType; override;
    function ReadIndexValue(const AValues: array of string): Integer;
    procedure ReadTag;
    procedure SpecReadBuffer(const ASize: Integer = 0); override;
    function SpecReadCharacterCodingAnnouncer: Integer; override;
    function SpecReadCharacterSetType: Integer; override;
    function SpecReadColourModel: Integer; override;
    function SpecReadColourSelectionMode: Integer; override;
    function SpecReadInteriorStyle: Integer; override;
    function SpecReadMetafileElementList: Integer; override;
    function SpecReadMetafileElementListCount: Integer; override;
    function SpecReadPrecision_I: TsgCgmIntPrecision; override;
    function SpecReadPrecision_I_Size: Integer; override;
    function SpecReadPrecision_R: TsgCgmRealPrecision; override;
    function SpecReadScalingMode: Integer; override;
    function SpecReadText_Piece: Integer; override;
    function SpecReadTextHorizontalAlignment: Integer; override;
    function SpecReadTextPath: Integer; override;
    function SpecReadTextVerticalAlignment: Integer; override;
    function SpecReadVDCType: Integer; override;
    function SpecReadWidthSpecificationMode: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginReadBits; override;
    procedure EndReadBits; override;
    function IsTagEOF: Boolean; override;
    function IsWeReadNBytes(const ACountByte: Byte = 1): Boolean; override;
    function ReadByteCount(const ASize: Integer): Integer; override;
    function ReadSDR: Integer; override;
    function ReadCloseSpec: Integer; override;
    function ReadDashCap: Integer; override;
    function ReadDirection: TPoint; override;
    procedure ReadI_Precision; override;
    procedure ReadIX_Precision; override;
    function ReadLineCap: Integer; override;
    function ReadLineJoin: Integer; override;
    procedure ReadN_Precision; override;
    procedure ReadR_Precision; override;
    function ReadRepresentationMode: Integer; override;
    function ReadSF(const AIsCgmSF: Boolean = True): string; override;
    function ReadSF_A: AnsiString; override;
    procedure ReadSF_Precision; override;
    function ReadTagHeader: Integer; override;
    procedure ReadVDC_I_Precision; override;
    procedure ReadVDC_R_Precision; override;
    function ReadVisibility: Integer; override;
    procedure RestorePosition; override;
    procedure SavePosition; override;
  end;

  TsgCGMClassOfReader = class of TsgCGMReader;

type
  TsgCGMImage = class(TsgVectorImageWithAltWhite)
  private
{$IFDEF SG_CGM_DEBUG}
    FFileName: string;
    FStackCommands: TList;
    FAPS: TStringList;
{$ENDIF}
    FSism: TObject;
    FBoxByCurvePolygon: TFRect;
    FClearHatches: TsgObjectList;
    FCADCurvePolygon: TsgCADCurvePolygon;
    FError: Boolean;
    FFontStyles: TsgObjectList;
    FHatchStyles: TsgObjectList;
    FIndexFunction: Integer;
    FInsertClip: TsgDXFInsert;
    FInsertVDC: TsgDXFInsert;
    FMetafileElementListSet: TsgCgmElementsList;
    FMetaFileName: string;
    FOnProgress: TsgCgmProgressEvent;
    FParameters: TsgCGMParameters;
    FProfileId: string;
    FReader: TsgCGMReader;
    FRegionParametrs: PsgRegionParametrs;
    FRestText: TsgDXFEntity;
    FTileArray: TObject;
    FTypeEntity: TsgCgmTypeEntity;
    FUpdateProgress: Integer;
    FUpdateProgressCount: Integer;

    FStackInsert: TsgStackObject;
    FCurrentInsert: TsgDXFInsert;

    FStackAPS: TsgStackObject;
    FCurrentLayerIfGiven: TsgDXFLayer;

    FDeltaWidth: Double;
    FDeltaHeight: Double;

    FFontAlias: TObject;
    FSetListTable: TObject;
{$IFDEF SG_CGM_DEBUG}
    procedure SaveStackCommand;
{$ENDIF}
    procedure AddConnectionEdgeEntity(const AFirstPoint, ALastPoint: TFPoint);
    procedure AddConnectionEdgeInHatch(const AFirstPoint, ALastPoint: TFPoint);
    function AddInsert(const AVDC: Boolean; const AScale: TFPoint;
      const AClip: PFRect): TsgDXFInsert;
    procedure AppendToMultyText(const AText: string);
    procedure ConvertToMultyText;
    procedure FixClearHatches;
    function CreateCurvePolygon(const ABasePoint: PFPoint = nil): Boolean;
    function GenerateMTextStyle: string;
    function GetActiveSection: TsgDXFBlock;
    function GetCharacterHeight: Double;
    function GetTextFontStyle(ATextFontIndex: Integer): TsgDXFStyle;
    function GetScaleVDC(const AVDC: TFREct): Double;
    function GetPixelFormat(const APrecision: Integer): TPixelFormat;
    procedure InitInternalParams;
    procedure InitParamsByReader;
    procedure LoadCurvePolygon(const ABasePoint: PFPoint = nil);
    procedure LoadInsert(const AVDC: Boolean);
    procedure NewRegion(const ACreateNewBoundary: Boolean = True);
    procedure SetCharacterHeight(const Value: Double);
    procedure SetRegionParam(const AFirstPoint, ALastPoint: TFPoint);
    procedure SetTypeEntity(const Value: TsgCgmTypeEntity);
    procedure PushInsert(const AInsert: TsgDXFInsert);
    function PopInsert: TsgDXFInsert;

  protected
    procedure InitGlobalParam;
    procedure ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD); override;
    procedure AddEntities(const AEntity: TsgDXFEntity; const AList: TsgObjectList);
    procedure AddEntityInSection(const AEntity: TsgDXFEntity);
//    procedure AddHatchAsPoly(AHatch: TList; APoly: TsgCADBasePolyline);//for future version
    procedure AddHatchBoundary(const ABoundary: Tsg2DBoundaryList;
      const AEntity: TsgDXFEntity);
    procedure AddHatchByEntities(const AList: TsgObjectList);
    procedure AddHatchOfArc(const ABoundary: Tsg2DBoundaryList;
      const AArc: TsgDXFArc);
    procedure AddHatchOfCircle(const ABoundary: Tsg2DBoundaryList;
      const ACircle: TsgDXFCircle);
    procedure AddHatchOfEllipse(const ABoundary: Tsg2DBoundaryList;
      const AEllipse: TsgDXFEllipse);
    procedure AddHatchOfLine(const ABoundary: Tsg2DBoundaryList;
      const ALine: TsgDXFLine);
    procedure AddHatchOfLines(const ABoundary: Tsg2DBoundaryList;
      const APoly: TsgDXFPolyline);
    procedure AddHatchOfPointsList(const ABoundary: Tsg2DBoundaryList;
      const APoly: TFPointList; const AsPolyline: Boolean);
    procedure AddHatchOfPolyBezier(const ABoundary: Tsg2DBoundaryList;
      const ASpline: TsgDXFSpline);
    procedure AddHatchOfPolyline(const ABoundary: Tsg2DBoundaryList;
      const APoly: TsgDXFPolyline);
    procedure AddHathPatternData(const AHatch: TsgCADHatch;
      const ABasePoint: TFPoint; const Angle: Double; const ASign: Integer);
    procedure AddPatternInHatch(const AHatch: TsgCADHatch;
      const ABasePoint: TFPoint);
    procedure AddRectangle(const ATopLeft, ABottomRight: TFPoint);
    procedure AddStyleByText(const AFontName: string);
    procedure AddRestText(const AType: TsgCgmTypeEntity); 
    procedure AnalizePictureBody;
    procedure ApplyCharOrientation(const AText: TsgDXFText);
    function CreateCircle(const APoints: array of TFPoint;
      const APointCalc: TFPoint; const ARadius: Double): TsgDXFCircle;
    function CreateEllipse(const APoints: array of TFPoint): TsgDXFEllipse;
    function CreateReader(const AStream: TStream): TsgCGMReader;
    procedure InitStandartStyleByText;
    function IsDrawHatch: Boolean;
    function GetImageType: TsgExportFormat; override;
    function GetCADColour(const AColor: TsgCgmColor): TsgColorCAD;
    function GetGroupsElements(
      const ATypeEntity: TsgCgmTypeEntity): TsgCgmGroupElements;
    function GetLineTypeScale: Double;
    function GetHatchStyle(const AIndex: Integer): TObject;
    function GetTextPoint(const ADeltaWidth, ADeltaHeight: Double;
      const ATextPosition: TFPoint): TFPoint;
    function MakeCloseLineByArc(const AArc: TsgDXFCircle;
      const AType: Integer): TsgDXFLWPolyline;
    function GetMetafileVersion: Integer;
    procedure MakePolyBezier(const ASpline: TsgDXFSpline;
      const AControlPointsList: TFPointList);
    procedure ReadAltCharSetIndex;
    procedure ReadApndText;
    procedure ReadBackGroundColor;
    procedure ReadBeginPicture;
    procedure ReadBeginPictureBody;
    procedure ReadBeginFigure;
    procedure ReadCellArray;//Reading BidMap of CGM File;
    procedure ReadCharacterHeight;
    procedure ReadCharacterOrientation;
    procedure ReadCharSetIndex;
    procedure ReadCharSpace;
    procedure ReadCircle;
    procedure ReadCircularArc3Point(const AClose: Boolean);
    procedure ReadCircularArcCenter(const AClose: Boolean);
    procedure ReadClipIndicator;
    procedure ReadClipRectandgle;
    procedure ReadConnectingEdge;
    procedure ReadGeneralizeDrawingPrimitive;// GENERALIZE DRAWING PRIMITIVE
    procedure ReadEdgeVisibility;
    procedure ReadEllipse;
    procedure ReadEllipticalArc(const AClose: Boolean);
    procedure ReadEndFigure;
    procedure ReadFillColor;
    procedure ReadLenMetadata;
    procedure ReadLineToMoveTo;
    procedure ReadHatchStyleDefinition;
    procedure ReadLineAndEdgeTypeDefinition;
    procedure ReadMarkerSize;
    procedure ReadMetafileDefaultsReplacement;
    procedure ReadMetafileDescription;
    procedure ReadNewRegion;
    procedure ReadFontList;
    procedure ReadBeginMetafile;
    procedure ReadPolyBezier;
    procedure ReadPolyline(const ATypeEntity: TsgCgmTypeEntity);
    procedure ReadPolygonSet;
    procedure ReadPolyMarker;
    procedure ReadLineCap(const AProperty: TsgLineProperty);
    procedure ReadLineJoin(const AProperty: TsgLineProperty);
    function ReadCompressiontType: TsgCgmCompressionType;
    procedure ReadBeginTileArray;
    procedure ReadBitonalTile;
    procedure ReadEndTileArray;
    procedure ReadTile;
    function CreateTile: TObject;
    procedure DisposeTile;
    procedure ReadRectangle;
    procedure ReadRestrTextType;
    procedure ReadTableColor;
    procedure ReadText(const AReadExtent: Boolean);
    procedure ReadRestrictedText;
    procedure ReadTextAlign;
    procedure ReadTextPath;
    procedure ReadTextScaleX;
    procedure ReadTextSpacing;
    procedure ReadUnknowBlock;
    procedure ReadVDCExtent;
    procedure ReadWidth(const AProperty: TsgMarkerPropery);
    // CGM Version 4 ( support APS - Application structures)
    procedure ReadBeginApplicationStructure;
    procedure ReadEndApplicationStructure;
    procedure ReadApplicationStrucrureAttribute;
    procedure ReadBeginApplicationStructureBody;
    procedure SetMetafileVersion(const AVersion: Integer);
    procedure SetCharOrientations(const AParams: array of Double);
    function SetupPropertyAndLoadEntity(const AEntity: TsgDXFEntity;
      const ATypeEntity: TsgCgmTypeEntity):
      TsgCgmGroupElements;
    procedure UpdateProgress;
    procedure ReadError(Sender: TObject);
    property CharacterHeight: Double read GetCharacterHeight
      write SetCharacterHeight;
    property TypeEntity: TsgCgmTypeEntity read FTypeEntity write SetTypeEntity;
  public
    constructor Create; override;
    destructor Destroy; override;
{$IFDEF SG_CGM_DEBUG}
    procedure LoadFromFile(const FileName: string); override;
{$ENDIF}
    procedure LoadFromStream(AStream: TStream); override;
    procedure StopLoading; override;
    property Error: Boolean read FError;
    property FileName: string read FMetaFileName;
  end;

var
  cgmBackGroundMode: TsgCgmBackGroundMode = bgFrame;

implementation


const
  cnstBT = ' BT ';
  cnstBTLight = ' BT';
  cnstClearStyle = 'ClearStyle';
  cnstCloseVis = 'CLOSEVIS';
  cnstDefaultFontName = 'Arial';
  cnstDefaultPrimaryFont = 'Arial.ttf';
  cnstIntegerRect: TFRect =  (Left: 0; Top: 0; Z1: 0; Right: 32767;
    Bottom: 32767; Z2: 0);
  cnstRealRect: TFRect =  (Left: 0; Top: 0; Z1: 0; Right: 1.0;
    Bottom: 1.0; Z2: 0);
  cnstDigitDiv = 0.00006103515625;//1 / 16384
  cnstDefMiterlimit = 10;
  cnstPercentUpdateProgress = 10.0;
  cnstKoefByDefaultImageSize = 297;
  cnstHatch = 'HATCH';
  cnstHershey = 'HERSHEY';
  cnstKeyWordsParamsUnknow = 'Unknow';
  cnstKeyWordsParamsLow = -1;
  cnstKeyWordsParamsHigh = 171;
  cnstMaxNumberTiles = 64;
  cnstMaxNumberCellTile = 4096;
  cnstMaxNumberCellArray = 32768;
  cnstKeyWordsParams: array [cnstKeyWordsParamsLow .. cnstKeyWordsParamsHigh]
    of record
    EClass: Byte;
    ECode: Byte;
    EID: string;
    EName: string;
  end = (
//Delemiter Elements
    (EClass: 255; ECode: 255; EID: 'unknow'; EName: 'unknow'),
    (EClass: 0; ECode: 0; EID: ''; EName: 'no-op'),//0
    (EClass: 0; ECode: 1; EID: 'BEGMF'; EName: 'BEGIN METAFILE'),
    (EClass: 0; ECode: 2; EID: 'ENDMF'; EName: 'END METAFILE'),
    (EClass: 0; ECode: 3; EID: 'BEGPIC'; EName: 'BEGIN PICTURE'),
    (EClass: 0; ECode: 4; EID: 'BEGPICBODY'; EName: 'BEGIN PICTURE BODY'),
    (EClass: 0; ECode: 5; EID: 'ENDPIC'; EName: 'END PICTURE'),
    (EClass: 0; ECode: 6; EID: 'BEGSEG'; EName: 'BEGIN SEGMENT'),
    (EClass: 0; ECode: 7; EID: 'ENDSEG'; EName: 'END SEGMENT'),
    (EClass: 0; ECode: 8; EID: 'BEGFIGURE'; EName: 'BEGIN FIGURE'),
    (EClass: 0; ECode: 9; EID: 'ENDFIGURE'; EName: 'END FIGURE'),
    (EClass: 0; ECode: 13; EID: 'BEGPROTREGION'; EName: 'BEGIN PROTECTION REGION'),
    (EClass: 0; ECode: 14; EID: 'ENDPROTREGION'; EName: 'END PROTECTION REGION'),
    (EClass: 0; ECode: 15; EID: 'BEGCOMPOLINE'; EName: 'BEGIN COMPOUND LINE'),
    (EClass: 0; ECode: 16; EID: 'ENDCOMPOLINE'; EName: 'END COMPOUND LINE'),
    (EClass: 0; ECode: 17; EID: 'BEGCOMPOTEXTPATH'; EName: 'BEGIN COMPOUND TEXT PATH'),
    (EClass: 0; ECode: 18; EID: 'ENDCOMPOTEXTPATH'; EName: 'END COMPOUND TEXT PATH'),
    (EClass: 0; ECode: 19; EID: 'BEGTILEARRAY'; EName: 'BEGIN TILE ARRAY'),
    (EClass: 0; ECode: 20; EID: 'ENDTILEARRAY'; EName: 'END TILE ARRAY'),
    (EClass: 0; ECode: 21; EID: 'BEGAPS'; EName: 'BEGIN APPLICATION STRUCTURE'),
    (EClass: 0; ECode: 22; EID: 'BEGAPSBODY'; EName: 'BEGIN APPLICATION STRUCTURE BODY'),
    (EClass: 0; ECode: 23; EID: 'ENDAPS'; EName: 'END APPLICATION STRUCTURE'),
// Metafile Descriptor Elements
    (EClass: 1; ECode: 1; EID: 'MFVERSION'; EName: 'METAFILE VERSION'),
    (EClass: 1; ECode: 2; EID: 'MFDESC'; EName: 'METAFILE DESCRIPTION'),
    (EClass: 1; ECode: 3; EID: 'VDCTYPE'; EName: 'VDC TYPE'),
    (EClass: 1; ECode: 4; EID: 'INTEGERPREC'; EName: 'INTEGER PRECISION'),
    (EClass: 1; ECode: 5; EID: 'REALPREC'; EName: 'REAL PRECISION'),
    (EClass: 1; ECode: 6; EID: 'INDEXPREC'; EName: 'INDEX PRECISION'),
    (EClass: 1; ECode: 7; EID: 'COLRPREC'; EName: 'COLOUR PRECISION'),
    (EClass: 1; ECode: 8; EID: 'COLRINDEXPREC'; EName: 'COLOUR INDEX PRECISION'),
    (EClass: 1; ECode: 9; EID: 'MAXCOLRINDEX'; EName: 'MAXIMUM COLOUR INDEX'),
    (EClass: 1; ECode: 10; EID: 'COLRVALUEEXT'; EName: 'COLOUR VALUE EXTENT'),
    (EClass: 1; ECode: 11; EID: 'MFELEMLIST'; EName: 'METAFILE ELEMENT LIST'),
    (EClass: 1; ECode: 12; EID: 'BEGMFDEFAULTS'; EName: 'METAFILE DEFAULTS REPLACEMENT'),//ENDMFDEFAULTS
    (EClass: 1; ECode: 13; EID: 'FONTLIST'; EName: 'FONT LIST'),
    (EClass: 1; ECode: 14; EID: 'CHARSETLIST'; EName: 'CHARACTER SET LIST'),
    (EClass: 1; ECode: 15; EID: 'CHARCODING'; EName: 'CHARACTER CODING ANNOUNCER'),
    (EClass: 1; ECode: 16; EID: 'NAMEPREC'; EName: 'NAME PRECISION'),
    (EClass: 1; ECode: 17; EID: 'MAXVDCEXT'; EName: 'MAXIMUM VDC EXTENT'),
    (EClass: 1; ECode: 18; EID: 'SEGPRIEXT'; EName: 'SEGMENT PROPIRY EXTENT'),
    (EClass: 1; ECode: 19; EID: 'COLRMODEL'; EName: 'COLOUR MODEL'),
    (EClass: 1; ECode: 20; EID: 'COLRCALIB'; EName: 'COLOUR CALIBRATION'),
    (EClass: 1; ECode: 21; EID: 'FONTPROP'; EName: 'FONT PROPERTIES'),
    (EClass: 1; ECode: 22; EID: 'GLYPHMAP'; EName: 'GLYPH MAPPING'),
    (EClass: 1; ECode: 23; EID: 'SYMBOLLIBLIST'; EName: 'SYMBOL, LIBRARY LIST'),
    (EClass: 1; ECode: 24; EID: 'PICDIR'; EName: 'PICTURE DIRECTORY'),
//Picture Descriptor Elements
    (EClass: 2; ECode: 1; EID: 'SCALEMODE'; EName: 'SCALING MODE'),
    (EClass: 2; ECode: 2; EID: 'COLRMODE'; EName: 'COLOUR SELECTION MODE'),
    (EClass: 2; ECode: 3; EID: 'LINEWIDTHMODE'; EName: 'LINE WIDTH SPECIFICATION MODE'),
    (EClass: 2; ECode: 4; EID: 'MARKERSIZEMODE'; EName: 'MARKER SIZE SPECIFICATION MODE'),
    (EClass: 2; ECode: 5; EID: 'EDGEWIDTHMODE'; EName: 'EDGE WIDTH SPECIFICATION MODE'),
    (EClass: 2; ECode: 6; EID: 'VDCEXT'; EName: 'VDC EXTENT'),
    (EClass: 2; ECode: 7; EID: 'BACKCOLR'; EName: 'BACKGROUND COLOUR'),
    (EClass: 2; ECode: 8; EID: 'DEVVP'; EName: 'DEVICE VIEWPORT'),
    (EClass: 2; ECode: 9; EID: 'DEVVPMODE'; EName: 'DEVICE VIEWPORT SPECIFICATION MODE'),
    (EClass: 2; ECode: 10; EID: 'DEVVPMAP'; EName: 'DEVICE VIEWPORT MAPPING'),
    (EClass: 2; ECode: 11; EID: 'LINEREP'; EName: 'LINE REPRESENTATION'),
    (EClass: 2; ECode: 12; EID: 'MARKERREP'; EName: 'MARKER REPRESENTATION'),
    (EClass: 2; ECode: 13; EID: 'TEXTREP'; EName: 'TEXT REPRESENTATION'),
    (EClass: 2; ECode: 14; EID: 'FILLREP'; EName: 'FILL REPRESENTATION'),
    (EClass: 2; ECode: 15; EID: 'EDGEREP'; EName: 'EDGE REPRESENTATION'),
    (EClass: 2; ECode: 16; EID: 'INTSTYLEMODE'; EName: 'INTERIOR STYLE SPECIFICATION MODE'),
    (EClass: 2; ECode: 17; EID: 'LINEEDGETYPEDEF'; EName: 'LINE AND EDGE TYPE DEFINITION'),
    (EClass: 2; ECode: 18; EID: 'HATCHSTYLEDEF'; EName: 'HATCH STYLE DEFINITION'),
    (EClass: 2; ECode: 19; EID: 'GEOPATDEF'; EName: 'GEOMETRIC PATTERN DEFINITION'),
    (EClass: 2; ECode: 20; EID: 'APSDIR'; EName: 'APPLICATION STRUCTURE DIRECTORY'),
//Control Elements
    (EClass: 3; ECode: 1; EID: 'VDCINTEGERPREC'; EName: 'VDC INTEGER PRECISION'),
    (EClass: 3; ECode: 2; EID: 'VDCREALPREC'; EName: 'VDC REAL PRECISION'),
    (EClass: 3; ECode: 3; EID: 'AUXCOLR'; EName: 'AUXILIARY COLOUR'),
    (EClass: 3; ECode: 4; EID: 'TRANSPARENCY'; EName: 'TRANSPARENTCY'),
    (EClass: 3; ECode: 5; EID: 'CLIPRECT'; EName: 'CLIP RECTANGLE'),
    (EClass: 3; ECode: 6; EID: 'CLIP'; EName: 'CLIP INDICATOR'),
    (EClass: 3; ECode: 7; EID: 'LINECLIPMODE'; EName: 'LINE CLIPPING MODE'),
    (EClass: 3; ECode: 8; EID: 'MARKERCLIPMODE'; EName: 'MARKER CLIPPING MODE'),
    (EClass: 3; ECode: 9; EID: 'EDGECLIPMODE'; EName: 'EDGE CLIPPING MODE'),
    (EClass: 3; ECode: 10; EID: 'NEWREGION'; EName: 'NEW REGION'),
    (EClass: 3; ECode: 11; EID: 'SAVEPRIMCONT'; EName: 'SAVE PRIMITIVE CONTEXT'),
    (EClass: 3; ECode: 12; EID: 'RESPRIMCONT'; EName: 'RESTORE PRIMITIVE CONTEXT'),
    (EClass: 3; ECode: 17; EID: 'PROTREGION'; EName: 'PROTECTION REGION INDICATOR'),
    (EClass: 3; ECode: 18; EID: 'GENTEXTPATHMODE'; EName: 'GENERALIZED TEXT PATH MODE'),
    (EClass: 3; ECode: 19; EID: 'MITRELIMIT'; EName: 'MITRE LIMIT'),
    (EClass: 3; ECode: 20; EID: 'TRANSPCELLCOLR'; EName: 'TRANSPARENT CELL COLOUR'),
//Graphical Primitive Elements
    (EClass: 4; ECode: 1; EID: 'LINE'; EName: 'POLYLINE'),//INCRLINE
    (EClass: 4; ECode: 2; EID: 'DISJTLINE'; EName: 'DISJOINT POLYLINE'),//INCRDISJTLINE
    (EClass: 4; ECode: 3; EID: 'MARKER'; EName: 'POLYMARKER'),//INCRMARKER
    (EClass: 4; ECode: 4; EID: 'TEXT'; EName: 'TEXT'),
    (EClass: 4; ECode: 5; EID: 'RESTRTEXT'; EName: 'RESTRICTED TEXT'),
    (EClass: 4; ECode: 6; EID: 'APNDTEXT'; EName: 'APPEND TEXT'),
    (EClass: 4; ECode: 7; EID: 'POLYGON'; EName: 'POLYGON'),//INCRPOLYGON
    (EClass: 4; ECode: 8; EID: 'POLYGONSET'; EName: 'POLYGON SET'),//INCRPOLYGONSET
    (EClass: 4; ECode: 9; EID: 'CELLARRAY'; EName: 'CELL ARRAY'),
    (EClass: 4; ECode: 10; EID: 'GDP'; EName: 'GENERALIZE DRAWING PRIMITIVE'),
    (EClass: 4; ECode: 11; EID: 'RECT'; EName: 'RECTANGLE'),
    (EClass: 4; ECode: 12; EID: 'CIRCLE'; EName: 'CIRCLE'),
    (EClass: 4; ECode: 13; EID: 'ARC3PT'; EName: 'CIRCLE ARC 3 POINT'),
    (EClass: 4; ECode: 14; EID: 'ARC3PTCLOSE'; EName: 'CIRCLE ARC 3 POINT CLOSE'),
    (EClass: 4; ECode: 15; EID: 'ARCCTR'; EName: 'CIRCLE ARC 3 CENTRE'),
    (EClass: 4; ECode: 16; EID: 'ARCCTRCLOSE'; EName: 'CIRCLE ARC 3 CENTRE CLOSE'),
    (EClass: 4; ECode: 17; EID: 'ELLIPSE'; EName: 'ELLIPSE'),
    (EClass: 4; ECode: 18; EID: 'ELLIPARC'; EName: 'ELLIPSE ARC'),
    (EClass: 4; ECode: 19; EID: 'ELLIPARCCLOSE'; EName: 'ELLIPSE ARC CLOSE'),
    (EClass: 4; ECode: 20; EID: 'ARCCTRREV'; EName: 'CIRCULAR ARC CENTRE REVERSED'),
    (EClass: 4; ECode: 21; EID: 'CONNEDGE'; EName: 'CONNECTING EDGE'),
    (EClass: 4; ECode: 22; EID: 'HYPERBARC'; EName: 'HYPERBOLIC ARC'),
    (EClass: 4; ECode: 23; EID: 'PARABARC'; EName: 'PARABOLIC ARC'),
    (EClass: 4; ECode: 24; EID: 'NUB'; EName: 'NON-UNIFORM B-SPLINE'),
    (EClass: 4; ECode: 25; EID: 'NURB'; EName: 'NON-UNIFORM RATIONAL B-SPLINE'),
    (EClass: 4; ECode: 26; EID: 'POLYBEZIER'; EName: 'POLYBEZIER'),
    (EClass: 4; ECode: 27; EID: 'SYMBOL'; EName: 'POLYSYMBOL'),//INCRSYMBOL
    (EClass: 4; ECode: 28; EID: 'BITONALTILE'; EName: 'BITONAL TILE'),
    (EClass: 4; ECode: 29; EID: 'TILE'; EName: 'TILE'),
//Attribute Elements
    (EClass: 5; ECode: 1; EID: 'LINEINDEX'; EName: 'LINE BUNDLE INDEX'),
    (EClass: 5; ECode: 2; EID: 'LINETYPE'; EName: 'LINE TYPE'),
    (EClass: 5; ECode: 3; EID: 'LINEWIDTH'; EName: 'LINE WIDTH'),
    (EClass: 5; ECode: 4; EID: 'LINECOLR'; EName: 'LINE COLOUR '),
    (EClass: 5; ECode: 5; EID: 'MARKERINDEX'; EName: 'MARKER BUNDLE INDEX'),
    (EClass: 5; ECode: 6; EID: 'MARKERTYPE'; EName: 'MARKER TYPE'),
    (EClass: 5; ECode: 7; EID: 'MARKERSIZE'; EName: 'MARKER SIZE'),
    (EClass: 5; ECode: 8; EID: 'MARKERCOLR'; EName: 'MARKER COLOUR'),
    (EClass: 5; ECode: 9; EID: 'TEXTINDEX'; EName: 'TEXT BUNDLE INDEX'),
    (EClass: 5; ECode: 10; EID: 'TEXTFONTINDEX'; EName: 'TEXT FONT INDEX'),
    (EClass: 5; ECode: 11; EID: 'TEXTPREC'; EName: 'TEXT PRECISION'),
    (EClass: 5; ECode: 12; EID: 'CHAREXPAN'; EName: 'CHARACTER EXPANSION FRACTION'),
    (EClass: 5; ECode: 13; EID: 'CHARSPACE'; EName: 'CHARACTER SPACING'),
    (EClass: 5; ECode: 14; EID: 'TEXTCOLR'; EName: 'TEXT COLOUR'),
    (EClass: 5; ECode: 15; EID: 'CHARHEIGHT'; EName: 'CHARACTER HEIGHT'),
    (EClass: 5; ECode: 16; EID: 'CHARORI'; EName: 'CHARACTER ORIENTATION'),
    (EClass: 5; ECode: 17; EID: 'TEXTPATH'; EName: 'TEXT PATH'),
    (EClass: 5; ECode: 18; EID: 'TEXTALIGN'; EName: 'TEXT ALIGNMENT'),
    (EClass: 5; ECode: 19; EID: 'CHARSETINDEX'; EName: 'CHARACTER SET INDEX'),
    (EClass: 5; ECode: 20; EID: 'ALTCHARSETINDEX'; EName: 'ALTERNATE CHARACTER SET INDEX'),
    (EClass: 5; ECode: 21; EID: 'FILLINDEX'; EName: 'FILL BUNDLE INDEX'),
    (EClass: 5; ECode: 22; EID: 'INTSTYLE'; EName: 'INTERIOR STYLE'),
    (EClass: 5; ECode: 23; EID: 'FILLCOLR'; EName: 'FILL COLOUR'),
    (EClass: 5; ECode: 24; EID: 'HATCHINDEX'; EName: 'HATCH INDEX'),
    (EClass: 5; ECode: 25; EID: 'PATINDEX'; EName: 'PATTERN INDEX'),
    (EClass: 5; ECode: 26; EID: 'EDGEINDEX'; EName: 'EDGE BUNDLE INDEX'),
    (EClass: 5; ECode: 27; EID: 'EDGETYPE'; EName: 'EDGE TYPE'),
    (EClass: 5; ECode: 28; EID: 'EDGEWIDTH'; EName: 'EDGE WIDTH'),
    (EClass: 5; ECode: 29; EID: 'EDGECOLR'; EName: 'EDGE COLOUR'),
    (EClass: 5; ECode: 30; EID: 'EDGEVIS'; EName: 'EDGE VISIBILITY'),
    (EClass: 5; ECode: 31; EID: 'FILLREFPT'; EName: 'FILL PEDFERENCE POINT'),
    (EClass: 5; ECode: 32; EID: 'PATTABLE'; EName: 'PATTERN TABLE'),
    (EClass: 5; ECode: 33; EID: 'PATSIZE'; EName: 'PATTERN SIZE'),
    (EClass: 5; ECode: 34; EID: 'COLRTABLE'; EName: 'COLOUR TABLE'),
    (EClass: 5; ECode: 35; EID: 'ASF'; EName: 'ACPECT SOURCE FLAGS'),
    (EClass: 5; ECode: 36; EID: 'PICKID'; EName: 'PICK INDENTIFIER'),
    (EClass: 5; ECode: 37; EID: 'LINECAP'; EName: 'LINE CAP'),
    (EClass: 5; ECode: 38; EID: 'LINEJOIN'; EName: 'LINE JOIN'),
    (EClass: 5; ECode: 39; EID: 'LINETYPECONT'; EName: 'LINE TYPE CONTINUATION'),
    (EClass: 5; ECode: 40; EID: 'LINETYPEINITOFFSET'; EName: 'LINE TYPE INITAL OFFSET'),
    (EClass: 5; ECode: 41; EID: 'TEXTSCORETYPE'; EName: 'TEXT SOURCE TYPE'),
    (EClass: 5; ECode: 42; EID: 'RESTRTEXTTYPE'; EName: 'RESTRICTED TEXT TYPE'),
    (EClass: 5; ECode: 43; EID: 'INTERPINT'; EName: 'INTERPOLATED INTERIOR'),
    (EClass: 5; ECode: 44; EID: 'EDGECAP'; EName: 'EDGE CAP'),
    (EClass: 5; ECode: 45; EID: 'EDGEJOIN'; EName: 'EDGE JOIN'),
    (EClass: 5; ECode: 46; EID: 'EDGETYPECONT'; EName: 'EDGE TYPE CONTINUATION'),
    (EClass: 5; ECode: 47; EID: 'EDGETYPEINITOFFSET'; EName: 'EDGE TYPE INITAL OFFSET'),
    (EClass: 5; ECode: 48; EID: 'SYMBOLLIBINDEX'; EName: 'SYMBOL LIBRARY INDEX'),
    (EClass: 5; ECode: 49; EID: 'SYMBOLCOLR'; EName: 'SYMBOL COLOUR'),
    (EClass: 5; ECode: 50; EID: 'SYMBOLSIZE'; EName: 'SYMBOL SIZE'),
    (EClass: 5; ECode: 51; EID: 'SYMBOLORI'; EName: 'SYMBOL ORIENTARION'),
//Escape Element
    (EClass: 6; ECode: 1; EID: 'ESCAPE'; EName: 'ESCAPE'),
//External Elements
    (EClass: 7; ECode: 1; EID: 'MESSAGE'; EName: 'MESSAGE'),
    (EClass: 7; ECode: 2; EID: 'APPLDATA'; EName: 'APPLICATION DATA'),
//Segment Elements
    (EClass: 8; ECode: 1; EID: 'COPYSEG'; EName: 'COPY SEGMENT'),
    (EClass: 8; ECode: 2; EID: 'INHFILTER'; EName: 'INHERITANCE FILTER'),
    (EClass: 8; ECode: 3; EID: 'CLIPINH'; EName: 'CLIP INHERITANCE'),
    (EClass: 8; ECode: 4; EID: 'SEGTRAN'; EName: 'SEGMENT TRANSFORMATION'),
    (EClass: 8; ECode: 5; EID: 'SEGHIGHL'; EName: 'SEGMENT HIGHLIGHTING'),
    (EClass: 8; ECode: 6; EID: 'SEGDISPPRI'; EName: 'SEGMENT DISPLAY PRIORITY'),
    (EClass: 8; ECode: 7; EID: 'SEGPICKPRI'; EName: 'SEGMENT PICK PRIORITY'),
//Applicatyion Structere Descriptor Elements
    (EClass: 9; ECode: 1; EID: 'APSATTR'; EName: 'APPLICATION STRUCTURE ATTRIBUTE')
   );
  cnstLayer = 'Layer';
  cnstLeftBracket = ' (';
  cnstLTPrefix      = 'CGM ';
  cnstLTName        = 'Line Type ';
  cnstLTSolid       = sSOLID;
  cnstLTypeDash     = 'ISO dash';
  cnstLTypeDot      = 'ISO dot';
  cnstLTypeDashDot  = 'ISO dash dot';
  cnstLTypeDash2Dot = 'ISO dash double-dot';
  cnstLTypeStitch   = 'ATA stitch';
  cnstLTypeChain    = 'ATA chain';
  cnstLTypeCenter   = 'ATA center';
  cnstLTypeHidden   = 'ATA hidden';
  cnstLTypePhantom   = 'ATA phantom';
  cnstLTypeBreakStyle2   = 'ATA break style 2';
  cnstHSName = 'Hatch Style ';
  cnstMaxWidth: array [TsgCgmWidthSpecMode] of Double = (512, 16, 512, 512);
  cnstMaxWord = $7FC0;
  cnstReadAsParts = $FFC0;
  cnstMaskTagLength = $FFF;
  cnstMask15bit = $7FFF;
  cnstModeCreateText = 'NOTFINAL';
  cnstModeDirect = 'DIRECT';
  cnstModeScaled = 'SCALED';
  cnstON = 'ON';
  cnstOneDivOnSin45: Extended = 1.4142135623730950488016887242097;
  cnstPie = 'PIE';
  cnstPS = ' PS ';
  cnstProfileId = 'PROFILEID:';
  cnstWebCgm = 'WEBCGM';
  cnstMetafileVersionWebCGM = 40;
  cnstLayerByClearHatches = 'clearhatches';
  cnstLayerNoName = 'apsBegin';
//  cnstFormat1 = 'MIL-D-28003';
//  cnstFormat2 = 'UG180';
//  cnstFormat4 = 'ISODRAW';
//  cnstFormat5 = 'MIL-D-28003A/BASIC-1.2';
  cnstSingle1: Single = 1;
  cnstWidthSingleKoef: Double = 40.109 / 32768;

  cnstDefaultColourParams: TsgCgmColourParametrs =
    (Components:((Scale:0;Offset:0),(Scale:0;Offset:0),(Scale:0;Offset:0));
     IndexPrecision: 8;
     MaximumIndex: 63;
     Model: cmlRGB;
     Precision: 8;
     SelectionMode: cmIndexed;
     ValueMax: $FFFFFF;
     ValueMin: 0;
    );

  cnstVdcTypes: array [0..1] of string = ('INTEGER', 'REAL');
  cnstSF_Precision: array[0..2] of string = ('STRING', 'CHAR', 'STROKE');
  cnstColourModel: array[0..4] of string = ('RGB', 'CIELAB', 'CIELUV', 'CMYK',
    'RGB-RELATED');
  cnstColourSelectionMode: array[0..1] of string = ('INDEXED', 'DIRECT');
  cnstScaleMode: array [0..1] of string = ('ABSTRACT', 'METRIC');
  cnstWidthSpecificationMode: array [0..3] of string = ('ABS', 'SCALED', 'FRACTIONAL', 'MM');
  cnstVisibility: array [0..5] of string = ('OFF', 'ON', 'INVIS', 'VIS', 'CLOSEINVIS', 'CLOSEVIS');
  cnstInteriorStyle: array [0..6] of string = ('HOLLOW', 'SOLID', 'PAT', 'HATCH', 'EMPTY', 'GEOPAT', 'INTERP');
  cnstElementsList: array [0..6] of string = ('DRAWINGPLUS', 'DRAWINGSET',
    'VERSION2', 'EXTDPRIM', 'VERSION2GKSM', 'VERSION3', 'VERSION4');
  cnstCloseSpec: array [0..1] of string = ('PIE', 'CHORD');
  cnstLineCap: array[1..5] of string = ('UNSPECIFIED','BUTT','ROUND',
    'PROJECTING SQUARE', 'TRIANGLE');
  cnstDashCap: array[1..3] of string = ('UNSPECIFIED', 'BUTT', 'MATCH');
  cnstLineJoin: array[1..4] of string = ('UNSPECIFIED','MITRE', 'ROUND', 'BEVEL');
  cnstHatchName: array[1..6] of string = ('HORIZONTAL', 'VERTICAL',
    'POSITIVE SLOPE', 'NEGATIVE SLOPE', 'HORIZONTAL/VERTICAL CROSS',
    'HORIZONTAL/VERTICAL SLOPE CROSS');
  cnstTextPiece: array [0..1] of string = ('NOTFINAL', 'FINAL');  
  cnstTextHorizontalAlignment: array [0..4] of string = ('NORMHORIZ', 'LEFT',
    'CTR', 'RIGHT', 'CONTHORIZ');
  cnstTextVerticalAlignment: array [0..6] of string = ('NORMVERT', 'TOP', 'CAP',
    'HALF', 'BASE', 'BOTTOM',  'CONTVERT');  
  cnstTextPath: array [0..3] of string = ('RIGHT', 'LEFT', 'UP', 'DOWN');
  cnstCharacterCodingAnnouncer: array [0..3] of string = ('BASIC7BIT',
    'BASIC8BIT', 'EXTD7BIT', 'EXTD8BIT');
  cnstCharacterSetList: array [0..4] of string = ('STD94', 'STD96',
    'STD94MULTIBYTE', 'STD96MULTIBYTE', 'COMPLETECODE');

  cnstMaxMinIntExtents: array [1 .. 8] of record
      Min: string;
      Max: string;
      Precision: TsgCgmIntPrecision;
      Size: Byte;//in bits
    end =
    ((Min: '0'; Max: '0'; Precision: ip0; Size: 0),
     (Min: '-128'; Max: '127'; Precision: ipShortInt; Size: 8),
     (Min: '0'; Max: '255'; Precision: ipByte; Size: 8),
     (Min: '-32766'; Max: '32767'; Precision: ipSmallInt; Size: 16),
     (Min: '0'; Max: '65535'; Precision: ipWord; Size: 16),
     (Min: '16777215'; Max: '16777215'; Precision: ipInt24; Size: 24),
     (Min: '-2147483648'; Max: '2147483647'; Precision: ipInteger; Size: 32),
     (Min: '0'; Max: '4294967295'; Precision: ipCardinal; Size: 32));
  cnstIntPrecisionSize: array [TsgCgmIntPrecision] of Integer = (2, 1, 1, 2, 2,
    3, 4, 4);
  cnstRealPrecisionSize: array [TsgCgmRealPrecision] of Integer = (4, 4, 8, 4,
   8);
  cnstLineGroups = [cgmPolyline, cgmDisjtPolyline, cgmArc3Point,
    cgmCircularArcCenter, cgmEllipticalArc, cgmCircularArcCenterReversed,
    cgmHyperbolicArc, cgmParabolicArc, cgmNonUniformBSpline,
    cgmNonUniformRationalBSpline, cgmPolyBezier];
  cnstMarkerGroups = [cgmMarker, cgmPolyMarker];
  cnstEdgeGroups = [cgmPolygone, cgmPolygonSet, cgmConnectingEdge, cgmRectangle,
    cgmCircle, cgmArc3PointClose, cgmCircularArcCenterClose, cgmEllipse,
    cgmEllipticalArcClose];
  cnstFillGroups = cnstEdgeGroups - [cgmConnectingEdge];
  cnstTextGroups = [cgmText, cgmRestrictedText, cgmAppendText];
  cnstGroupElements: array [TsgCgmGroupElements] of TsgCgmTypeEntities =
    ([cgmNone], cnstLineGroups, cnstMarkerGroups, cnstEdgeGroups,
    cnstTextGroups);

type
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFTextAccess = class(TsgDXFText);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TsgDXFPenEntityAccess = class(TsgDXFPenEntity);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgCADHatchAccess = class(TsgCADHatch);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgFlatPolyAccess = class(TsgFlatPoly);

  TcgmHatchStyle = class
  private
    FGapWidths: TsgIntegerList;
    FLineTypes: TsgIntegerList;
  public
    Index: Integer;
    Indicator: Integer;//parallel, crosshatch
    BasePoint: TFPoint;
    OffsetPoint: TFPoint;
    Length: Double;
    constructor Create;
    destructor Destroy; override;
    procedure Apply(const AHatch: TsgCADhatch; const AConverter:
      TsgDXFConverter);
    property GapWidths: TsgIntegerList read FGapWidths;
    property LineTypes: TsgIntegerList read FLineTypes;
  end;

  TsgSism = class
  private
    FImage: TsgCGMImage;
    FActiveSection: TsgDXFBlock;

    FTraceStartPoint: TFPoint;
    FBaseDirection: TFPoint;
    FAmpDirection: TFPoint;
    FTraceDirection: TFPoint;

    FBaseScaleFactor: Double;
    FAmplitudeScaleFactor: Double;
    FTraceStepFactor: Double;
    FVAOffset: Double;
    FPositiveClippingLimit: Double;
    FNegativeClippingLimit: Double;
    FTraceDisplayMode: Integer;
    FSampleType: Integer;
    FNumberOfSamples: Integer;
    FPositiveBackFillBoundary: Double;
    FNegativeBackFillBoundary: Double;
    FWiggleTraceModulus: Integer;
    FNullColorIndex: Integer;
    FNumberTracesPerLocation: Integer;

//    FSampleI: TsgIntegerList;
    FSampleD: TsgDoubleList;
    FCoind: PsgCgmColorArray;

    FFlatPoly: TsgFlatPoly;
    FFlatHatch: TsgFlatHatch;
    FFlatHatchBox: TFRect;
    FFlatHatchCount: Integer;
    FFlatHatchSumCounts: Integer;

    FTrace: Integer;
    procedure AddFlatHatchPoint(const X, Y: Double);
    function CreatePolygon(const AColor: TsgColorCAD):  TsgFlatHatch;
    procedure ResetFlatHatchBox;
    procedure SetFlatHatchBox;
  protected
    procedure Bgclfl(const AMode: Integer);
    procedure Wiggle(APosclp, ANegclp: Double);
    function Interpl(const I: Integer; const AY1, AY2, X: Double): Double;
    procedure Vasamp(const AMode: Integer; AMax, AMin: Double;
      const ACordep: Integer);
    procedure Dirvet(const ADx: Double);
    procedure Ampscf(const ADx: Double);
    procedure ReadSample(const AIndex: Integer);
    procedure TraceInitialize;
    procedure TraceData;
  public
    constructor Create(const AImage: TsgCGMImage);
    destructor Destroy; override;
    procedure Clear;
    property ActiveSection: TsgDXFBlock read FActiveSection;
  end;

  TcgmTileArray = class
    Position: TFPoint;//(P)
    Direction: TPoint;//(one of: 0, 90, 180, 270) (E)
    NumberOfTiles: TPoint;//(I)
    NumberOfCells: TPoint;//(I)
    CellSize: TF2DPoint;//(R)
    ImageOffset: TPoint;//(I)
    ImageNumberOfCells: TPoint;//(I)
    RowPadding: Integer;
    Decompressor: TsgDecompressor;
    CurrentNumberTile: Integer; // i = (CurrentNumberTile mod NumberOfTiles.X)
                                // j = (CurrentNumberTile div NumberOfTiles.X)
  end;

  TsgStructureDataType = (sdtSDR{$IFDEF SGDEL_6}=1{$ENDIF}, sdtCI, sdtCD, sdtN, sdtE, sdtI, sdtReserved,
    sdtIF8, sdtIF16, sdtIF32, sdtIX, sdtR, sdtS, sdtSF, sdtVC, sdtVDC, sdtCCO,
    sdtUI8, sdtUI32, sdtBS, sdtCL, sdtUI16);

  TsgCGM_SDR = class
    FListData: TsgCollection;
  end;

  // picbody ::= layer+ |
  //          (grobject | para | grnode | gdata)*

  TsgDXFEntityClass = class of TsgDXFEntity;

  TsgCustomAps = class(TsgDXFEntity);
  TsgApsGrobject = class(TsgCustomAps);
  TsgApsPara = class(TsgCustomAps);
  TsgApsGrnode = class(TsgCustomAps);
  TsgApsGdata = class(TsgCustomAps);

  TsgCgmApsToken = (catNone, catLayers{$IFDEF SGDEL_6} = 1{$ENDIF}, catGrobject, catPara, catGrnode, catGdata);

  TsgAPSStackObject = class(TsgStackObject)
  public
    function GetTopStack: TsgDXFEntity;
    function GetUpperByType(const AClassName: string): TsgDXFEntity;
  end;

  TsgISO2375 = class
    FinalByte: string;
    CodePage: Integer;
    Content: string;
  end;

  TsgFontAlias = class
    FontName: string;
    AliasName: string;
  end;

  TsgFontAliasTable = class
  private
    {$IFDEF SG_FIREMONKEY}
    FCS: TCriticalSection;
    {$ENDIF}
    FFontAlias: TsgObjectList;
    FFindItem: TObject;
    function CompareAlias(const A, B: Pointer): Integer;
    procedure AddItem(const AFontName, AAliasName: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetFontAlias(const AFont: string): string;
  end;

  TsgSetList = class
  private
    {$IFDEF SG_FIREMONKEY}
    FCS: TCriticalSection;
    {$ENDIF}
    F94CharacterGSetOneByte: TsgObjectList;//array[0..61] of TsgISO2375;
    F94CharacterGSetSecondByte: TsgObjectList;//array[0..6] of TsgISO2375;
    F96CharacterGSet: TsgObjectList;//array[0..42] of TsgISO2375;
    F94MultipleByteCharacterGSet: TsgObjectList;//array[0..17] of TsgISO2375;
    FCompleteCodeEscapeSeq: TsgObjectList;//array[0..21] of TsgISO2375;
    procedure AddItem(const AList: TsgObjectList; const AFinalByte: string;
      const ACodePage: Integer; const AContent: string);
    procedure Init;
    procedure GenerateISO4873ListForCGM;
    procedure FreeISO4873ListForCGM;
  protected
    ItemsList: array[cst94..cstNone] of TsgObjectCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function GetISO2375Item(const AGSet: TsgCgmCharacterSetType;
      const ACode: String): TsgISO2375;
  end;

const
  cnstAPSClasses: array[catGrobject..catGdata] of TsgDXFEntityClass =
    (TsgApsGrobject, TsgApsPara, TsgApsGrnode, TsgApsGdata);
  cnstAPSToken: array[TsgCgmApsToken] of string = ('', 'layer', 'grobject', 'para',
    'grnode', 'gdata');

  cnstSDT_string: array[TsgStructureDataType] of string =
    ('SDR','CI','CD','N','E','I','Reserved','IF8','IF16','IF32', 'IX',
    'R','S','SF','VC', 'VDC', 'CCO','UI8','UI32', 'BS', 'CL', 'UI16');

var
{$IFDEF SG_CGM_DEBUG}
  iEntitiesIndex: Integer = 0;
{$ENDIF}
  KeyWordsBin: array [cnstKeyWordsParamsLow .. cnstKeyWordsParamsHigh] of Word;
  KeyWordsBinSort: TsgIntegerList = nil;
  KeyWordsStr: TStringList = nil;
  FontAliasTable: TsgFontAliasTable = nil;
  SetListTable: TsgSetList = nil;

// forward block
procedure ReadSDRData(const AReader: TsgCGMReader; const AXMLSDR: TsgNode); forward;

function GetCodeFromString(const AString: String): String;
var
  I: Integer;
  vByte: Byte;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    if AString[I] = ' ' then
      Continue;
    vByte := Byte(AString[I]);
    if Length(Result) > 0 then
      Result := Result + ' ';
    Result := Result + IntToStr((vByte and $F0) shr 4) + '/' + IntToStr(vByte and $0F);
  end;
end;

procedure CheckString(var AString: string);
var
  I: Integer;
begin
  for I := 1 to Length(AString) do
    if AString[I] < #$20 then
      AString[I] := ' ';
end;

function FindInString(const AValue1, AValue2: string): Boolean;
var
  I, J: Integer;
  vLen1, vLen2, vCount: Integer;
begin
  vLen1 := Length(AValue1);
  vLen2 := Length(AValue2);
  J := 1;
  Result := False;
  for I := 1 to vLen1 do
  begin
    if AValue1[I] = AValue2[J] then
    begin
      vCount := 0;
      for J := 1 to vLen2 do
      begin
        if (I + J - 1) <= vLen1 then
          if AValue1[I + J - 1] = AValue2[J] then
            Inc(vCount)
          else
            Break
        else
          Break;
      end;
      J := 1;
      if vCount = vLen2 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function GetCGMFileNameByFontName(const AFontName: string): string;
var
  I, vLength, vTypeLast: Integer;
begin
  Result := '';
  vTypeLast := 2;
  vLength := Length(AFontName);
  for I := 1 to vLength do
  begin
    case AFontName[I] of
      'a'..'z':
         begin
           vTypeLast := 1;
           Result := Result + AFontName[I];
         end;
      'A'..'Z':
         begin
           if (vTypeLast <> 2) and (I <> vLength) then
             Result := Result + ' ';
           vTypeLast := 2;
           Result := Result + AFontName[I];
         end;
      '0'..'9':
         begin
          if vTypeLast <> 3 then
             Result := Result + ' ';
           vTypeLast := 3;
           Result := Result + AFontName[I];
         end;
    else
      vTypeLast := 0;
    end;
  end;
  Result := Result + cnstTrueType;
end;

function GetBoundaryType(const AEntity: TsgDXFEntity): Byte;
begin
  case AEntity.EntType of
    ceLWPolyline, cePolyline: Result := 7;
  else
    Result := 1;
  end;
end;

function GetFontNameCorrect(const AFontAliasTable: TsgFontAliasTable; const AFontName: string): string;
var
  I: Integer;
  vFontAlias: string;
begin
  Result := '';
  if Length(AFontName) > 0 then
  begin
    Result := UpperCase(AFontName);
    for I := Length(Result) downto 1 do
    begin
      case Result[I] of
        ':', '/':  Result[I] := ' ';
        '_':       Result[I] := '-';
        #0: Delete(Result, I, 1);
      end;
    end;
    vFontAlias := AFontAliasTable.GetFontAlias(Result);
    if vFontAlias <> '' then
      Result := vFontAlias;
  end;
end;

function GetIntPrecisionBySize(const ASize: Integer):  TsgCgmIntPrecision;
begin
  case ASize of
    1:  Result := ipByte;
    2:  Result := ipWord;
    3:  Result := ipInt24;
    4:  Result := ipCardinal;
  else
    Result := ip0;
  end;
end;

function GetIndexByMinMax(const AMin, AMax: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(cnstMaxMinIntExtents) to High(cnstMaxMinIntExtents) do
    if (AMax = cnstMaxMinIntExtents[I].Max) then
    begin
      if (Length(AMin) = 0) or (AMin = cnstMaxMinIntExtents[I].Min) then
      begin
        Result := I;
        Break;
      end;
    end;  
end;

function GetIntSizeByMinMax(const AMin, AMax: string): Integer;
var
  vIndex: Integer;
begin
  Result := 0;
  vIndex := GetIndexByMinMax(AMin, AMax);
  if vIndex > -1 then
    Result := cnstMaxMinIntExtents[vIndex].Size;
end;

function GetIntPrecisionByMinMax(const AMin, AMax: string): TsgCgmIntPrecision;
var
  vIndex: Integer;
begin
  Result := ip0;
  vIndex := GetIndexByMinMax(AMin, AMax);
  if vIndex > -1 then
    Result := cnstMaxMinIntExtents[vIndex].Precision;
end;


function GetSizeByIntPrecision(const AValue: TsgCgmIntPrecision): Integer;
begin
  Result := cnstIntPrecisionSize[AValue];
end;

function GetRealPrecisionByMaxMin(const AMin, AMax, ADigit: string):
  TsgCgmRealPrecision;
begin
  Result := rp0;
end;

function GetHatchName(const AHatchIndex: Integer): string;
begin
  case AHatchIndex of
    1..6:  Result := cnstHatchName[AHatchIndex];
  else
    if AHatchIndex < 0 then
      Result := cnstHSName + IntToStr(Abs(AHatchIndex))
    else
      Result := cnstHatch;
  end;
end;

function GetLineTypeName(const AConverter: TsgDXFConverter;
  const ALineType: Integer): string;

  function AddCGMLineType(const AConverter: TsgDXFConverter;
    const ALineTypeName: string; const ADashes: array of Double): string;
  begin
    Result := '';
    if Assigned(TsgDXFConverterAccess(AConverter).AddLineType(
      cnstLTPrefix + ALineTypeName, 2, ADashes)) then
      Result := ALineTypeName;
  end;

begin
  case ALineType of
    Integer(ltNone):        Result := '';
    Integer(ltSolid):       Result := cnstLTSolid;
    Integer(ltDash):        Result := cnstLTypeDash;
    Integer(ltDot):         Result := cnstLTypeDot;
    Integer(ltDashDot):     Result := cnstLTypeDashDot;
    Integer(ltDashDotDot):  Result := cnstLTypeDash2Dot;
    Integer(ltSingleArrow),Integer(ltSingleDot),
      Integer(ltDoubleArrow): Result := '';
    Integer(ltStitch):   Result := AddCGMLineType(AConverter, cnstLTypeStitch,
      [0.25, -0.25]);
    Integer(ltChain):  Result := AddCGMLineType(AConverter, cnstLTypeChain,
      [1.25, -0.25, 0.25, -0.25]);
    Integer(ltCenter): Result := AddCGMLineType(AConverter, cnstLTypeCenter,
      [2.25, -0.25, 0.25, -0.25]);
    Integer(ltHidden): Result := AddCGMLineType(AConverter, cnstLTypeHidden,
      [1.50, -0.50]);
    Integer(ltPhantom): Result := AddCGMLineType(AConverter, cnstLTypePhantom,
      [1.25, -0.25, 0.25, -0.25, 0.25, -0.25]);
    Integer(ltBreakStyle2): Result := AddCGMLineType(AConverter, cnstLTypePhantom,
      [2.25, -0.25, 0.25, -0.25]);
    Integer(ltBreakStyle1): Result := '';
  else
    if ALineType < 0 then
      Result := cnstLTName + IntToStr(Abs(ALineType))
    else
      Result := '';
  end;
  if Length(Result) > 0  then
    Result := cnstLTPrefix + Result
  else
    Result := sContinuous;
end;

function GetMStyle(const AFontName: string; const AColor: TsgColorCAD;
  const AHeight: Double): string;
begin
  Result := '\f' + AFontName + ';';
  Result := Result + '\h' + DoubleToStr(AHeight, '.') + ';';
  Result := Result + '\c' + IntToStr(AColor.Color) + ';';
end;

procedure InitKeyWordsBin;
var
  I: Integer;
  vParametrs: TsgParametrs;
begin
  if KeyWordsBinSort = nil then
  begin
    FillChar(vParametrs, SizeOf(vParametrs), 0);
    KeyWordsBinSort := TsgIntegerList.Create;
    KeyWordsBinSort.Capacity := cnstKeyWordsParamsHigh -
      cnstKeyWordsParamsLow + 1;
    for I := cnstKeyWordsParamsLow to cnstKeyWordsParamsHigh do
    begin
      vParametrs.Param1 := cnstKeyWordsParams[I].EClass;
      vParametrs.Param2 := cnstKeyWordsParams[I].ECode;
      KeyWordsBin[I] := vParametrs.FirstTwoParams;
      KeyWordsBinSort.Add(I);
    end;
    KeyWordsBinSort.ProcCompare := TsgCGMReader.CompareKeyWordsBin;
    KeyWordsBinSort.Sort;
    //QSortList(KeyWordsBinSort, CompareKeyWordsBin);
  end;
end;

function CreateEllipseEx(ACenter, ARatPoint1, ARatPoint2,
  AStartPoint, AEndPoint: PFPoint): TsgDXFEllipse;
var
  vAngle1, vAngle2, vR, vR1, vR2 :Double;
begin
  Result := nil;
  vR1 := DistanceFPoint(ARatPoint1^, ACenter^);
  vR2 := DistanceFPoint(ARatPoint2^, ACenter^);
  if not (IsZero(vR1) and IsZero(vR2)) then
  begin
    if IsZero(vR1) then
      vR1 := vR2
    else
      if IsZero(vR2) then
        vR2 := vR1;
    Result := TsgDXFEllipse.Create;
    Result.Point := ACenter^;
    if vR1 > vR2 then
    begin
      vR := vR2 / vR1;
      Result.EndPoint := SubFPoint(ARatPoint1^, ACenter^);
    end
    else
    begin
      vR := vR1 / vR2;
      Result.EndPoint := SubFPoint(ARatPoint2^, ACenter^);
    end;
    Result.Ratio := vR;
    Result.StartAngle := 0;
    Result.EndAngle := 360;
    if (AStartPoint <> nil) and (AEndPoint <> nil)  then
    begin
      vAngle1 := sgFunction.GetAngleParam(Result.Point, Result.RadPt,
        Result.Ratio, AStartPoint^, False);
      vAngle2 := sgFunction.GetAngleParam(Result.Point, Result.RadPt,
        Result.Ratio, AEndPoint^, False);

      if (vAngle1 > vAngle2) then
      begin
        if vAngle1 - vAngle2 > 180 then
          SwapDoubles(vAngle1, vAngle2);
      end
      else
        if vAngle2 - vAngle1 < 180 then
          SwapDoubles(vAngle1, vAngle2);

      Result.StartAngle := vAngle2;
      Result.EndAngle := vAngle1;
    end;
  end;
end;

procedure InitKeyWordsStr;
var
  I: Integer;
begin
  if KeyWordsStr <> nil then
    Exit;
  KeyWordsStr := TStringList.Create;
  KeyWordsStr.Capacity := cnstKeyWordsParamsHigh - cnstKeyWordsParamsLow + 1;
  KeyWordsStr.Sorted := True;
  KeyWordsStr.Duplicates := dupIgnore;
{$IFDEF SG_DEL6}
  KeyWordsStr.CaseSensitive := True;
{$ENDIF}
  for I := cnstKeyWordsParamsLow to cnstKeyWordsParamsHigh do
    KeyWordsStr.AddObject(cnstKeyWordsParams[I].EID, TsgObjectWithField.CreateInt(I));
end;

function FindIndexOfKeyWordsBin(const AKey: Integer): Integer;
var
  vKey, vIndex, vLeftIndex, vRightIndex, vResultCompare, vKeyIndex: Integer;
begin
  if KeyWordsBinSort = nil then
    InitKeyWordsBin;
  Result := -1;
  vKey := AKey;
  vLeftIndex := 0;
  vRightIndex := KeyWordsBinSort.Count - 1;
  while vLeftIndex <= vRightIndex do
  begin
    vIndex := (vLeftIndex + vRightIndex) shr 1;
    vKeyIndex := Integer(KeyWordsBinSort.List[vIndex]);
    vResultCompare := Integer(KeyWordsBin[vKeyIndex]) - vKey;
    if vResultCompare < 0 then
     vLeftIndex := vIndex + 1
    else
    begin
      vRightIndex := vIndex - 1;
      if vResultCompare = 0 then
      begin
        Result := vKeyIndex;
        Break;
      end;
    end;
  end;
end;

function IndexOfKeyWordsBin(const AClass, ACode: Byte): Integer;
var
  vParametrs: TsgParametrs;
begin
  vParametrs.Param1 := AClass;
  vParametrs.Param2 := ACode;
  Result := FindIndexOfKeyWordsBin(vParametrs.FirstTwoParams);
end;

function IndexOfKeyWordsString(const AKey: string): Integer;
begin
  if KeyWordsStr = nil then
    InitKeyWordsStr;
  Result := KeyWordsStr.IndexOf(UpperCase(AKey));
  if Result > -1 then
    Result := TsgObjectInt64(KeyWordsStr.Objects[Result]).FieldInt;
end;

function IsPolylineClosed(const APoly: TsgCADBasePolyline): Boolean;
var
  vPoint1, vPoint2: TFPoint;
begin
  Result := APoly.Closed;
  if (not Result) and (APoly.Count > 1) then
  begin
    vPoint1 := TsgDXFVertex(APoly.Entities[0]).Point;
    vPoint2 := TsgDXFVertex(APoly.Entities[APoly.Count - 1]).Point;
    Result := IsEqualFPoints(vPoint1, vPoint2);
  end;
end;

function IsEmptyBlock(const ABlock: TsgDXFBlock): Boolean;
var
  I: Integer;
begin
  Result := True;
  I := 0;
  while Result and (I < ABlock.Count) do
  begin
    if ABlock.Entities[I] is TsgDXFInsert then
    begin
      if not IsEmptyBlock(TsgDXFInsert(ABlock.Entities[I]).Block) then
        Result := False;
    end
    else
      Result := False;
    Inc(I);
  end;
end;

function MakeCgmRgbColor(const AComponent1, AComponent2,
  AComponent3: Cardinal): TsgCgmColor;
begin
  Result.Model := cmlRGB;
  Result.Components[0] := AComponent1;
  Result.Components[1] := AComponent2;
  Result.Components[2] := AComponent3;
end;

  {TCGMParameters}

constructor TsgCGMParameters.Create;
begin
  inherited Create;
  FillChar(FParameters, SizeOf(FParameters), 0);
end;

procedure TsgCGMParameters.TwoToThree(AValue1, AValue2: Byte);
var
  vValue: Cardinal;
begin
  vValue := (AValue1 shl 8) or AValue2;
  FParameters.Param1 := (vValue shr 12) and $F;
  FParameters.Param2 := (vValue shr 5) and $7F;
  FParameters.Param3 := vValue and $1F;
  FParameters.Param4 := 1;
end;

  {TsgCGMImage}

procedure TsgCGMImage.InitGlobalParam;
begin
  if not Assigned(FontAliasTable) then
    FontAliasTable := TsgFontAliasTable.Create;
  FFontAlias := FontAliasTable;
  if not Assigned(SetListTable) then
    SetListTable := TsgSetList.Create;
  FSetListTable := SetListTable;
end;

procedure TsgCGMImage.InitInternalParams;
begin
  FError := False;
  TypeEntity:= cgmNone;
  FMetafileElementListSet := elDrawing;
  if FParameters = nil then
    FParameters := TsgCGMParameters.Create;
  FRestText := nil;
  TsgDXFConverterAccess(Converter).LoadStopped := False;
  FProfileId := '';
  FUpdateProgress := Round(FReader.Size * cnstPercentUpdateProgress / 100);
{$IFDEF SG_FIREMONKEY}
  {$IFDEF SG_FM_WINDOWS}
  InitGlobalParam;
  {$ELSE}
  FFontAlias := TsgFontAliasTable.Create;
  FSetListTable := TsgSetList.Create;
  {$ENDIF}
{$ELSE}
  InitGlobalParam;
{$ENDIF}
{$IFDEF SG_CGM_DEBUG}
  iEntitiesIndex := 0;
  FStackCommands.Count := 0;
  FAPS.Clear;
{$ENDIF}
end;

procedure TsgCGMImage.InitParamsByReader;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

{$IFDEF SG_CGM_DEBUG}
procedure TsgCGMImage.SaveStackCommand;
var
  I, vIndexFunction, vIndexBuffer: Integer;
  vStrings: TStringList;
  S: string;
  vCGMParams: TsgCGMParameters;
begin
  vCGMParams := TsgCGMParameters.Create;
  vStrings := TStringList.Create;
  try
    try
      I := 0;
      while I < FStackCommands.Count do
      begin
        vIndexBuffer := Integer(FStackCommands.List[I]);
        vIndexFunction := Integer(FStackCommands.List[I + 1]);
        Inc(I, 2);

        case vIndexFunction of
          2,5,6,7,10,11,12,13,14,15,37,38,40,41,42,43,
          44,52,53,54,55,56,57,58,59,60,63,64,67,68,71,
          72,73,75,76,77,78,79,80,100,102,103,104,105,
          107,110,116,118,130,134,135,140,141,142,144,
          145,148,149,150,152,153,154,155,156,157,158,
          160,161,162,163,164,165,166,167,168,169,170: S := '*>';
        else
          S:= '';
        end;



        S := S + IntToStr(vIndexBuffer)+ ' {' + IntToHex(vIndexBuffer, 8)  + '}: ';
        if vIndexFunction > -1 then
        begin
          S := S + cnstKeyWordsParams[vIndexFunction].EName +
            '(' + IntToStr(cnstKeyWordsParams[vIndexFunction].EClass) + '.' +
                  IntToStr(cnstKeyWordsParams[vIndexFunction].ECode) + ')';
        end
        else
          S := S + cnstKeyWordsParamsUnknow;
        vStrings.Add(S);
      end;
      if Length(FFileName) = 0 then
        FFileName := 'stackcommands_' + IntToHex(Integer(Self), 8);
      vStrings.SaveToFile('d:\LogCGM\' + FFileName + '.txt');

      FAPS.SaveToFile('d:\LogCGM\' + FFileName + '_APS' + '.txt');
    except
    end;
  finally
    vStrings.Free;
    vCGMParams.Free;
  end;
end;
{$ENDIF}

procedure TsgCGMImage.AddConnectionEdgeEntity(const AFirstPoint,
  ALastPoint: TFPoint);
var
  vEdge: TsgDXFLine;
  vList: TsgObjectList;
begin
  vList := TsgObjectList.Create;
  try
    vEdge := TsgDXFLine.Create;
    vList.Add(vEdge);
    vEdge.Point := AFirstPoint;
    vEdge.Point1 := ALastPoint;
    TypeEntity := cgmConnectingEdge;
    AddEntities(nil, vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.AddConnectionEdgeInHatch(const AFirstPoint,
  ALastPoint: TFPoint);
var
  vEdge: TsgDXFLine;
  vList: TsgObjectList;
begin
  vList := TsgObjectList.Create;
  try
    vEdge := TsgDXFLine.Create;
    try
      vList.Add(vEdge);
      vEdge.Point := AFirstPoint;
      vEdge.Point1 := ALastPoint;
      AddHatchByEntities(vList);
    finally
      vEdge.Free;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.AddEntities(const AEntity: TsgDXFEntity;
  const AList: TsgObjectList);
var
  I: Integer;
  vTypeEntity: TsgCgmTypeEntity;
  vGroupElements: TsgCGMGroupElements;
  vList: TsgObjectList;
begin
  if (AEntity = nil) and (AList = nil) then Exit;

  vTypeEntity := TypeEntity; // !!! called iteratively

  if (FRegionParametrs <> nil) and
    (FRegionParametrs^.IsConnectionEdge)  then
  begin
    FRegionParametrs^.IsConnectionEdge := False;
    AddConnectionEdgeEntity(FRegionParametrs^.ConnectionPoint,
      FRegionParametrs^.FirstPointElement);
  end;
  if AList = nil then
  begin
    vList := TsgObjectList.Create;
    vList.Add(AEntity);
  end
  else
    vList := AList;
  try
    vGroupElements := GetGroupsElements(vTypeEntity);
    for I := 0 to vList.Count - 1 do
    begin
      SetupPropertyAndLoadEntity(TsgDXFEntity(vList[I]), vTypeEntity);
    end;
    if IsDrawHatch and ((vGroupElements = geEdge) or
      (FCADCurvePolygon <> nil)) then
      AddHatchByEntities(vList);
    for I := 0 to vList.Count - 1 do
      AddEntityInSection(TsgDXFEntity(vList[I]));
  finally
    TypeEntity := cgmNone;
    if vList <> AList then
      FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.AddEntityInSection(const AEntity: TsgDXFEntity);
var
  vBlock: TsgDXFBlock;
begin
  if Assigned(FCurrentLayerIfGiven) then
    AEntity.Layer := FCurrentLayerIfGiven;

  vBlock := GetActiveSection;
  vBlock.AddEntity(AEntity);
{$IFDEF SG_CGM_DEBUG}
  Inc(iEntitiesIndex);
{$ENDIF}
  if Assigned(Converter.OnCreate) then
    Converter.OnCreate(AEntity);
end;

procedure TsgCGMImage.AddHatchBoundary(const ABoundary: Tsg2DBoundaryList;
  const AEntity: TsgDXFEntity);
begin
  UnionFRect(FBoxByCurvePolygon, AEntity.Box);
  case AEntity.EntType of
    ceLine:
       AddHatchOfLine(ABoundary, TsgDXFLine(AEntity));
    ceLWPolyline, cePolyline:
      begin
        if ABoundary.BoundaryType = 1 then
          AddHatchOfLines(ABoundary, TsgDXFPolyline(AEntity))
        else
          AddHatchOfPolyline(ABoundary, TsgDXFPolyline(AEntity));
      end;
    ceArc:
      AddHatchOfArc(ABoundary, TsgDXFArc(AEntity));
    ceCircle:
      AddHatchOfCircle(ABoundary, TsgDXFCircle(AEntity));
    ceEllipse:
      AddHatchOfEllipse(ABoundary, TsgDXFEllipse(AEntity));
    ceSpline:
      begin
        if TsgDXFSpline(AEntity).IsByAngles then
          AddHatchOfPointsList(ABoundary, TsgDXFSpline(AEntity).Fit, False)
        else
          AddHatchOfPolyBezier(ABoundary, TsgDXFSpline(AEntity));
      end;
  end;
end;

procedure TsgCGMImage.AddHatchByEntities(const AList: TsgObjectList);
var
  I: Integer;
  vCADCurvePolygon: TsgCADCurvePolygon;
  vBoundary: Tsg2DBoundaryList;
  vBasePoint: TFPoint;
begin
  if AList.Count < 1 then Exit;
  vBoundary := nil;
  vCADCurvePolygon := FCADCurvePolygon;
  try
    if FCADCurvePolygon = nil then
    begin
      vBasePoint := TsgDXFEntity(AList.First).Box.TopLeft;
      if CreateCurvePolygon(@vBasePoint) then
      begin
        NewRegion;
        vBoundary := FCADCurvePolygon.BoundaryDataLast;
        if AList.Count = 1 then
          vBoundary.BoundaryType := GetBoundaryType(TsgDXFEntity(AList[0]));
      end;
    end
    else//in figure
      vBoundary := FCADCurvePolygon.BoundaryDataLast;

    if Assigned(vBoundary) then
    begin
      for I := 0 to AList.Count - 1 do
        AddHatchBoundary(vBoundary, TsgDXFEntity(AList[I]));

      if (FRegionParametrs <> nil) and (FRegionParametrs^.IsCloseRegion) then
        NewRegion;
    end;
  finally
    if vCADCurvePolygon = nil then
      LoadCurvePolygon;
  end;
end;

procedure TsgCGMImage.AddHatchOfArc(const ABoundary: Tsg2DBoundaryList;
  const AArc: TsgDXFArc);
var
  vArc: Tsg2DArc;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  vArc := Tsg2DArc.Create;
  ABoundary.Add(vArc);
  vArc.CenterPoint := MakeF2DPoint(AArc.Point.X, AArc.Point.Y);
  vArc.Radius := AArc.Radius;
  vArc.StartParam := AArc.StartAngle;
  vArc.EndParam := AArc.EndAngle;
  vArc.CounterClockWise := True;
end;

procedure TsgCGMImage.AddHatchOfCircle(const ABoundary: Tsg2DBoundaryList;
  const ACircle: TsgDXFCircle);
var
  vCircle: Tsg2DArc;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  vCircle := Tsg2DArc.Create;
  ABoundary.Add(vCircle);
  vCircle.CenterPoint := MakeF2DPoint(ACircle.Point.X, ACircle.Point.Y);
  vCircle.Radius := ACircle.Radius;
  vCircle.StartParam := 0;
  vCircle.EndParam := 360;
  vCircle.CounterClockWise := True;
end;

procedure TsgCGMImage.AddHatchOfEllipse(const ABoundary: Tsg2DBoundaryList;
  const AEllipse: TsgDXFEllipse);
var
  vEllipse: Tsg2DEllipse;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  vEllipse := Tsg2DEllipse.Create;
  ABoundary.Add(vEllipse);
  vEllipse.CenterPoint := MakeF2DPoint(AEllipse.Point.X, AEllipse.Point.Y);
  vEllipse.MajorPoint := MakeF2DPoint(AEllipse.EndPoint.X, AEllipse.EndPoint.Y);
  vEllipse.Radius := AEllipse.Ratio;
  vEllipse.StartParam := AEllipse.StartAngle;
  vEllipse.EndParam := AEllipse.EndAngle;
  vEllipse.IsAngleInParam := False;
  vEllipse.CounterClockWise := True;
end;

procedure TsgCGMImage.AddHatchOfLine(const ABoundary: Tsg2DBoundaryList;
  const ALine: TsgDXFLine);
var
  vLine: Tsg2DLine;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  if not IsEqualFPoints2D(ALine.Point, ALine.Point1) then
  begin
    vLine := Tsg2DLine.Create;
    ABoundary.Add(vLine);
    vLine.SetStartPoint(MakeF2DPointFrom3D(ALine.Point));
    vLine.SetEndPoint(MakeF2DPointFrom3D(ALine.Point1));
  end;
end;

procedure TsgCGMImage.AddHatchOfLines(const ABoundary: Tsg2DBoundaryList;
  const APoly: TsgDXFPolyline);
var
  I: Integer;
  vLine: Tsg2DLine;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  if APoly.PolyPoints.Count > 0 then
  begin
    for I := 0 to APoly.PolyPoints.Count - 2 do
    begin
      vLine := Tsg2DLine.Create;
      ABoundary.Add(vLine);
      if not IsEqualFPoints2D(APoly.Points[I], APoly.Points[I + 1]) then
      begin
        vLine.SetStartPoint(MakeF2DPointFrom3D(APoly.Points[I]));
        vLine.SetEndPoint(MakeF2DPointFrom3D(APoly.Points[I + 1]));
      end;
    end;
  end
  else
  begin
    for I := 0 to APoly.Count - 2 do
    begin
      vLine := Tsg2DLine.Create;
      ABoundary.Add(vLine);
      if not IsEqualFPoints2D(TsgDXFVertex(APoly.Entities[I]).Point,
               TsgDXFVertex(APoly.Entities[I+1]).Point) then
      begin
        vLine.SetStartPoint(MakeF2DPointFrom3D(
          TsgDXFVertex(APoly.Entities[I]).Point));
        vLine.SetEndPoint(MakeF2DPointFrom3D(
          TsgDXFVertex(APoly.Entities[I+1]).Point));
      end;
    end;
  end;
end;

procedure TsgCGMImage.AddHatchOfPointsList(const ABoundary: Tsg2DBoundaryList;
  const APoly: TFPointList; const AsPolyline: Boolean);
var
  I: Integer;
  vPolyline: TsgDXFPolyline;
begin
  vPolyline := TsgDXFPolyline.Create;
  try
    for I := 0 to APoly.Count - 1 do
      AddVertexInPolyline(vPolyline, APoly.List[I]);
    if AsPolyline then
      AddHatchOfPolyline(ABoundary, vPolyline)
    else
      AddHatchOfLines(ABoundary, vPolyline);
  finally
    vPolyline.Free;
  end;
end;

procedure TsgCGMImage.AddHatchOfPolyBezier(const ABoundary: Tsg2DBoundaryList;
  const ASpline: TsgDXFSpline);
var
  I: Integer;
  vSpline: Tsg2DSpline;
  vPoint: TFPoint;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 1);
{$ENDIF}
  vSpline := Tsg2DSpline.Create;
  ABoundary.Add(vSpline);
  for I := 0 to ASpline.Controls.Count - 1 do
  begin
    vPoint := ASpline.Controls[I];
    vSpline.AddControl(MakeF2DPoint(vPoint.X, vPoint.Y));
  end;
  for I := 0 to ASpline.Knots.Count - 1 do
    vSpline.AddKnot(ASpline.Knots[I]);
end;

procedure TsgCGMImage.AddHatchOfPolyline(const ABoundary: Tsg2DBoundaryList;
  const APoly: TsgDXFPolyline);
var
  I: Integer;
  vPoly: Tsg2DPolyline;
  vVertex: TsgDXFVertex;
  vPoint: TF2DPoint;
begin
{$IFDEF SG_CGM_DEBUG}
  Assert(ABoundary.BoundaryType = 7);
{$ENDIF}
  vPoly := Tsg2DPolyline.Create;
  ABoundary.Add(vPoly);
  for I := 0 to APoly.Count - 1 do
  begin
    vVertex := TsgDXFVertex(APoly.Entities[I]);
    vPoint.X := vVertex.Point.X;
    vPoint.Y := vVertex.Point.Y;
    if (I > 0) and IsEqualF2DPoints(vPoint, vPoly.EndPoint) then
      Continue;
    vPoly.AddVertex(vPoint);
  end;
  vPoly.Closed := not IsEqualF2DPoints(vPoly.StartPoint, vPoly.EndPoint);
end;

procedure TsgCGMImage.AddHathPatternData(const AHatch: TsgCADHatch;
  const ABasePoint: TFPoint; const Angle: Double; const ASign: Integer);
var
  vPHatchPatternData: PsgHatchPatternData;
begin
  New(vPHatchPatternData);
  AHatch.HatchPatternData.Add(vPHatchPatternData);
  vPHatchPatternData.BaseP := ABasePoint;
  vPHatchPatternData.Offset := MakeFPoint(200, ASign * 200, 0);
  vPHatchPatternData.LineAngle := Angle;
  vPHatchPatternData.IsDash := False;
  vPHatchPatternData.Lines := nil;
  vPHatchPatternData.DashNum := 0;
end;

function TsgCGMImage.AddInsert(const AVDC: Boolean; const AScale: TFPoint;
  const AClip: PFRect): TsgDXFInsert;
var
  vClipBox: TFRect;
begin
  if Assigned(FCurrentInsert) then
  begin
    PushInsert(FCurrentInsert);
  end;
  if AClip = nil then
    Result := TsgDXFInsert.Create
  else
  begin
    Result := TsgCADClipInsert.Create;
    vClipBox := cnstBadRect;
    ExpandFRect(vClipBox, AClip^.TopLeft);
    ExpandFRect(vClipBox, AClip^.BottomRight);
    FReader.Container.Context.ClipRectangle := AClip^;
    TransRectCorners(vClipBox, StdMat(AScale, cnstFPointZero));
    TsgCADClipInsert(Result).ClipBox := vClipBox;
  end;
  AddEntityInSection(Result);
  if AVDC then
    FInsertVDC := Result
  else
    FInsertClip := Result;
  Result.Block := TsgDXFBlock.Create;
  Result.Block.Name := 'CGM' + IntToHex(Integer(Result.Block), 0);
  Converter.Sections[csBlocks].AddEntity(Result.Block);
  Result.Scale := AScale;
  FCurrentInsert := Result;
end;

procedure TsgCGMImage.AddPatternInHatch(const AHatch: TsgCADHatch;
  const ABasePoint: TFPoint);
var
  vHatchIndex: Integer;
  vHatchStyle: TcgmHatchStyle;
  vFillStyle: TsgFillStyle;
begin
  vHatchIndex := FReader.Container.Context.Hatch.HatchIndex;
  case vHatchIndex of
    1:  AHatch.FillStyle := fsHorizontal;//horizontal equally spaced parallel lines
    2:  AHatch.FillStyle := fsVertical;//vertical equally spaced parallel lines
    3:  AHatch.FillStyle := fsBDiagonal;//positive slope equally spaced parallel lines is +45°
    4:  AHatch.FillStyle := fsFDiagonal;//negative slope equally spaced parallel lines is +135°
    5:  AHatch.FillStyle := fsCross;//horizontal/vertical crosshatch
    6:  AHatch.FillStyle := fsDiagCross;//positive slope/negative slope crosshatch
  else
    if vHatchIndex < 0 then
    begin
      vHatchStyle := TcgmHatchStyle(GetHatchStyle(vHatchIndex));
      if vHatchStyle <> nil then
        vHatchStyle.Apply(AHatch, Converter)
      else
        AHatch.FillStyle := fsClear;
    end
    else
      AHatch.FillStyle := fsClear;
  end;
  vFillStyle := AHatch.FillStyle;
  try
    AHatch.HatchName := GetHatchName(vHatchIndex);
    AHatch.PatternScale := GetLineTypeScale;
    AHatch.Offset := ABasePoint;
  finally
    TsgCADHatchAccess(AHatch).SetFillStyle(vFillStyle);  
  end;
end;

procedure TsgCGMImage.AddRectangle(const ATopLeft, ABottomRight: TFPoint);
var
  vPolyline: TsgDXFLWPolyline;
begin
  vPolyline := TsgDXFLWPolyline.Create;
  vPolyline.Closed := True;
  AddVertexInPolyline(vPolyline, ATopLeft);
  AddVertexInPolyline(vPolyline, MakeFPoint(ABottomRight.X, ATopLeft.Y, 0));
  AddVertexInPolyline(vPolyline, ABottomRight);
  AddVertexInPolyline(vPolyline, MakeFPoint(ATopLeft.X, ABottomRight.Y, 0));
  // Set type BEFORE call SetRegionParam
  TypeEntity := cgmRectangle;
  SetRegionParam(vPolyline.Vertexes[0].Point,
        vPolyline.Vertexes[vPolyline.Count - 1].Point);
  AddEntities(vPolyline, nil);
end;

procedure TsgCGMImage.AddRestText(const AType: TsgCgmTypeEntity);
begin
  TypeEntity := AType;
  AddEntities(FRestText, nil);
  FRestText := nil;
end;

procedure TsgCGMImage.AddStyleByText(const AFontName: string);
var
  vIndex: Integer;
  vFontName: string;
  vFontStyle: TFontStyles;
  vStyle: TsgDXFStyle;
  vStyles: TsgDXFTable;
begin
  vStyle := nil;
  vStyles := TsgDXFTable(Converter.Sections[csStyles]);
  if vStyles <> nil then
  begin
    vStyle := TsgDXFStyle(vStyles.FindEntByName(AFontName));
    if (vStyle = nil) then
    begin
      vStyle := TsgDXFStyle.Create;
      vStyle.Name := AFontName;
      vStyles.AddEntity(vStyle);
      vFontName := AFontName;//GetCGMFileNameByFontName(vStyle.Name);
      vIndex := AnsiPos(cnstBT,vFontName);
      if vIndex > 0 then
      begin
        Delete(vFontName, vIndex, 3);
        vIndex := AnsiPos(cnstLeftBracket, vFontName);
        Insert(cnstBTLight, vFontName, vIndex);
      end;
      vIndex := AnsiPos(cnstPS,vFontName);
      if vIndex > 0 then
        Delete(vFontName, vIndex, 3);
      SplitFullName(vFontName, vFontStyle);
      vStyle.FontName := vFontName;
      vStyle.FontStyle := FontStylesToMVFontStyles(vFontStyle);
      vStyle.PrimaryFont := GetFileNameByFontName(vFontName, vFontStyle);
      if Length(vStyle.PrimaryFont) = 0 then
        vStyle.PrimaryFont := cnstDefaultPrimaryFont;
      Converter.Loads(vStyle);
    end;
  end;
  FFontStyles.Add(vStyle);
end;

procedure TsgCGMImage.AnalizePictureBody;
begin
  while (FReader.Position < FReader.Size - 1) and (not IsStopLoading) do
  begin
    UpdateProgress;
    FIndexFunction := FReader.ReadTagHeader;
{$IFDEF SG_CGM_DEBUG}
    FStackCommands.Add(Pointer(FReader.Position - 2));
    FStackCommands.Add(Pointer(FIndexFunction));
    //SaveStackCommand;
{$ENDIF}
    case FIndexFunction of
//Delemiter Elements
      0:
        begin
          FReader.SeekUndefined;
        end;
      1:  ReadBeginMetafile;// 'BEGIN METAFILE'
      2:  ReadUnknowBlock;//'END METAFILE'
      3:  ReadBeginPicture;//'BEGIN PICTURE'
      4:  ReadBeginPictureBody;//'BEGIN PICTURE BODY'
      5:  ReadUnknowBlock;//'END PICTURE'
      6:  ReadUnknowBlock;//'BEGIN SEGMENT'
      7:  ReadUnknowBlock;//'END SEGMENT'
      8:  ReadBeginFigure;//'BEGIN FIGURE'
      9:  ReadEndFigure;//'END FIGURE'
      10:  ReadUnknowBlock;//'BEGIN PROTECTION REGION'
      11:  ReadUnknowBlock;//'END PROTECTION REGION'
      12:  ReadUnknowBlock;//'BEGIN COMPOUND LINE'
      13:  ReadUnknowBlock;//'END COMPOUND LINE'
      14:  ReadUnknowBlock;//'BEGIN COMPOUND TEXT PATH'
      15:  ReadUnknowBlock;//'END COMPOUND TEXT PATH'
      16:  ReadBeginTileArray;//'BEGIN TILE ARRAY'
      17:  ReadEndTileArray;//'END TILE ARRAY'
      18:  ReadBeginApplicationStructure;//'BEGIN APPLICATION STRUCTURE'
      19:  ReadBeginApplicationStructureBody;//'BEGIN APPLICATION STRUCTURE BODY'
      20:  ReadEndApplicationStructure;//'END APPLICATION STRUCTURE'
// Metafile Descriptor Elements
      21:  SetMetafileVersion(FReader.ReadI);//'METAFILE VERSION'
      22:  ReadMetafileDescription;//'METAFILE DESCRIPTION'
      23:  FReader.ReadVDC_Type;//'VDC TYPE'
      24:  FReader.ReadI_Precision;//'INTEGER PRECISION'
      25:  FReader.ReadR_Precision;//'REAL PRECISION'
      26:  FReader.ReadIX_Precision;//'INDEX PRECISION'
      27:  FReader.ReadCCO_Precision;//'COLOUR PRECISION'
      28:  FReader.ReadCI_Precision;//'COLOUR INDEX PRECISION'
      29:  FReader.Container.FPrecision.ColourMaximumIndex := FReader.ReadCI.Index;//'MAXIMUM COLOUR INDEX'
      30:  FReader.ReadColorValuesExtent;//'COLOUR VALUE EXTENT'
      31:  FMetafileElementListSet := FReader.MetafileElementList;//'METAFILE ELEMENT LIST'
      32:  ReadMetafileDefaultsReplacement;//'METAFILE DEFAULTS REPLACEMENT'
      33:  ReadFontList;//'FONT LIST'
      34:  FReader.ReadCharacterSetList(FSetListTable);//'CHARACTER SET LIST'
      35:  FReader.ReadCharacterCodingAnnouncer;//'CHARACTER CODING ANNOUNCER'
      36:  FReader.ReadN_Precision;//'NAME PRECISION'
      37:  ReadUnknowBlock;//'MAXIMUM VDC EXTENT'
      38:  ReadUnknowBlock;//'SEGMENT PROPIRY EXTENT'
      39:  FReader.ReadColorModel;//'COLOUR MODEL'
      40:  ReadUnknowBlock;//'COLOUR CALIBRATION'
      41:  ReadUnknowBlock;//'FONT PROPERTIES'
      42:  ReadUnknowBlock;//'GLYPH MAPPING'
      43:  ReadUnknowBlock;//'SYMBOL, LIBRARY LIST'
      44:  ReadUnknowBlock;//'PICTURE DERECTORY'
//Picture Descriptor Elements
      45:  FReader.ReadScalingMode;//'SCALING MODE'
      46:  FReader.ReadColorSelectionMode;//COLOUR SELECTION MODE
      47:  FReader.Container.Precision.LineWidthSpecMode := FReader.ReadWidthSpecificationMode;//'LINE WIDTH SPECIFICATION MODE'
      48:  FReader.Container.Precision.MarkerWidthSpecMode := FReader.ReadWidthSpecificationMode;//'MARKER SIZE SPECIFICATION MODE'
      49:  FReader.Container.Precision.EdgeWidthSpecMode := FReader.ReadWidthSpecificationMode;//'EDGE WIDTH SPECIFICATION MODE'
      50:  ReadVDCExtent;//'VDC EXTENT'
      51:  ReadBackGroundColor;//'BACKGROUND COLOUR'
      52:  ReadUnknowBlock;//'DEVICE VIEWPORT'
      53:  ReadUnknowBlock;//'DEVICE VIEWPORT SPECIFICATION MODE'
      54:  ReadUnknowBlock;//'DEVICE VIEWPORT MAPPING'
      55:  ReadUnknowBlock;//'LINE REPRESENTATION'
      56:  ReadUnknowBlock;//'MARKER REPRESENTATION'
      57:  ReadUnknowBlock;//'TEXT REPRESENTATION'
      58:  ReadUnknowBlock;//'FILL REPRESENTATION'
      59:  ReadUnknowBlock;//'EDGE REPRESENTATION'
      60:  ReadUnknowBlock;//'INTERIOR STYLE SPECIFICATION MODE'
      61:  ReadLineAndEdgeTypeDefinition;//'LINE AND EDGE TYPE DEFINITION'
      62:  ReadHatchStyleDefinition;//'HATCH STYLE DEFINITION'
      63:  ReadUnknowBlock;//'GEOMETRIC PATTERN DEFINITION'
      64:  ReadUnknowBlock;//'APPLICATION STRUCTURE DIRECTORY'
//Control Elements
      65:  FReader.ReadVDC_I_Precision;//'VDC INTEGER PRECISION'
      66:  FReader.ReadVDC_R_Precision;//'VDC REAL PRECISION'
      67:  ReadUnknowBlock;//'AUXILIARY COLOUR'
      68:  ReadUnknowBlock;//'TRANSPARENTCY'
      69:  ReadClipRectandgle;//'CLIP RECTANGLE'
      70:  ReadClipIndicator;//'CLIP INDICATOR'
      71:  ReadUnknowBlock;//'LINE CLIPPING MODE'
      72:  ReadUnknowBlock;//'MARKER CLIPPING MODE'
      73:  ReadUnknowBlock;//'EDGE CLIPPING MODE'
      74:  ReadNewRegion;//'NEW REGION'
      75:  ReadUnknowBlock;//'SAVE PRIMITIVE CONTEXT'
      76:  ReadUnknowBlock;//'RESTORE PRIMITIVE CONTEXT'
      77:  ReadUnknowBlock;//'PROTECTION REGION INDICATOR'
      78:  ReadUnknowBlock;//'GENERALIZED TEXT PATH MODE'
      79:  ReadUnknowBlock;//'MITRE LIMIT'
      80:  ReadUnknowBlock;//'TRANSPARENT CELL COLOUR'
//Graphical Primitive Elements
      81:  ReadPolyline(cgmPolyline);//'POLYLINE'
      82:  ReadLineToMoveTo;//'DISJOINT POLYLINE'
      83:  ReadPolyMarker;//'POLYMARKER'
      84:  ReadText(False);//'TEXT'
      85:  ReadRestrictedText;//'RESTRICTED TEXT'
      86:  ReadApndText;//'APPEND TEXT'
      87:  ReadPolyline(cgmPolygone);//'POLYGON'
      88:  ReadPolygonSet;//'POLYGON SET'
      89:  ReadCellArray;//'CELL ARRAY'
      90:  ReadGeneralizeDrawingPrimitive;//'GENERALIZE DRAWING PRIMITIVE'
      91:  ReadRectangle;//'RECTANGLE'
      92:  ReadCircle;//'CIRCLE'
      93:  ReadCircularArc3Point(False);//'CIRCLE ARC 3 POINT'
      94:  ReadCircularArc3Point(True);//'CIRCLE ARC 3 POINT CLOSE'
      95:  ReadCircularArcCenter(False);//'CIRCLE ARC CENTRE'
      96:  ReadCircularArcCenter(True);//'CIRCLE ARC CENTRE CLOSE'
      97:  ReadEllipse;//'ELLIPSE'
      98:  ReadEllipticalArc(False);//'ELLIPSE ARC'
      99:  ReadEllipticalArc(True);//'ELLIPSE ARC CLOSE'
      100:  ReadUnknowBlock;//'CIRCULAR ARC CENTRE REVERSED'
      101:  ReadConnectingEdge;//'CONNECTING EDGE'
      102:  ReadUnknowBlock;//'HYPERBOLIC ARC'
      103:  ReadUnknowBlock;//'PARABOLIC ARC'
      104:  ReadUnknowBlock;//'NON-UNIFORM B-SPLINE'
      105:  ReadUnknowBlock;//'NON-UNIFORM RATIONAL B-SPLINE'
      106:  ReadPolyBezier;//'POLYBEZIER'
      107:  ReadUnknowBlock;//'POLYSYMBOL'
      108:  ReadBitonalTile;//'BITONAL TILE'
      109:  ReadTile;//'TILE'
//Attribute Elements
      110:  ReadUnknowBlock;//'LINE BUNDLE INDEX'
      111:  FReader.Container.Context.Line.EType := FReader.ReadLineType;//'LINE TYPE'
      112:  ReadWidth(FReader.Container.Context.Line);//'LINE WIDTH'
      113:  FReader.Container.Context.Line.Colour := FReader.ReadCgmColor;//'LINE COLOUR
      114:  ReadUnknowBlock;//'MARKER BUNDLE INDEX'
      115:  FReader.Container.Context.Marker.EType := Integer(TsgCgmMarkerType(FReader.ReadIX));//'MARKER TYPE'
      116:  ReadUnknowBlock; //'MARKER SIZE'
      117:  FReader.Container.Context.Marker.Colour := FReader.ReadCgmColor;//'MARKER COLOUR'
      118:  ReadUnknowBlock;//'TEXT BUNDLE INDEX'
      119:  FReader.Container.Context.Text.TextFontIndex := FReader.ReadIX;//'TEXT FONT INDEX'
      120:  FReader.ReadSF_Precision;//'TEXT PRECISION'
      121:  ReadTextScaleX;//'CHARACTER EXPANSION FRACTION'
      122:  ReadTextSpacing;//'CHARACTER SPACING'
      123:  FReader.Container.Context.Text.Colour := FReader.ReadCgmColor;//'TEXT COLOUR'
      124:  ReadCharacterHeight;//'CHARACTER HEIGHT'
      125:  ReadCharacterOrientation;//'CHARACTER ORIENTATION'
      126:  ReadTextPath;//'TEXT PATH'
      127:  ReadTextAlign;//'TEXT ALIGNMENT'
      128:  ReadCharSetIndex;//'CHARACTER SET INDEX'
      129:  ReadAltCharSetIndex;//'ALTERNATE CHARACTER SET INDEX'
      130:  ReadUnknowBlock;//'FILL BUNDLE INDEX'
      131:  FReader.Container.Context.Hatch.InteriorStyle := FReader.ReadInteriorStyle;//'INTERIOR STYLE'
      132:  ReadFillColor;//'FILL COLOUR'
      133:  FReader.Container.Context.Hatch.HatchIndex := FReader.ReadIX;//'HATCH INDEX'
      134:  ReadUnknowBlock;//'PATTERN INDEX'
      135:  ReadUnknowBlock;//'EDGE BUNDLE INDEX'
      136:  FReader.Container.Context.Edge.EType := FReader.ReadLineType;//'EDGE TYPE'
      137:  ReadWidth(FReader.Container.Context.Edge);//'EDGE WIDTH'
      138:  FReader.Container.Context.Edge.Colour := FReader.ReadCgmColor;//'EDGE COLOUR'
      139:  ReadEdgeVisibility;//'EDGE VISIBILITY'
      140:  ReadUnknowBlock;//'FILL PEDFERENCE POINT'
      141:  ReadUnknowBlock;//'PATTERN TABLE'
      142:  ReadUnknowBlock;//'PATTERN SIZE'
      143:  ReadTableColor;//'COLOUR TABLE'
      144:  ReadUnknowBlock;//'ASPECT SOURCE FLAGS'
      145:  ReadUnknowBlock;//'PICK INDENTIFIER'
      146:  ReadLineCap(FReader.Container.Context.Line);//'LINE CAP'
      147:  ReadLineJoin(FReader.Container.Context.Line);//'LINE JOIN'
      148:  ReadUnknowBlock;//'LINE TYPE CONTINUATION'
      149:  ReadUnknowBlock;//'LINE TYPE INITAL OFFSET'
      150:  ReadUnknowBlock;//'TEXT SOURCE TYPE'
      151:  ReadRestrTextType;//'RESTRICTED TEXT TYPE'
      152:  ReadUnknowBlock;//'INTERPOLATED INTERIOR'
      153:  ReadUnknowBlock;//'EDGE CAP'
      154:  ReadUnknowBlock;//'EDGE JOIN'
      155:  ReadUnknowBlock;//'EDGE TYPE CONTINUATION'
      156:  ReadUnknowBlock;//'EDGE TYPE INITAL OFFSET'
      157:  ReadUnknowBlock;//'SYMBOL LIBRARY INDEX'
      158:  ReadUnknowBlock;//'SYMBOL COLOUR'
      159:  ReadUnknowBlock;//'SYMBOL SIZE'
      160:  ReadUnknowBlock;//'SYMBOL ORIENTARION'
//Escape Element
      161:  ReadUnknowBlock;//'ESCAPE'
//External Elements
      162:  ReadUnknowBlock;//'MESSAGE'
      163:  ReadUnknowBlock;//'APPLICATION DATA'
//Segment Elements
      164:  ReadUnknowBlock;//'COPY SEGMENT'
      165:  ReadUnknowBlock;//'INHERITANCE FILTER'
      166:  ReadUnknowBlock;//'CLIP INHERITANCE'
      167:  ReadUnknowBlock;//'SEGMENT TRANSFORMATION'
      168:  ReadUnknowBlock;//'SEGMENT HIGHLIGHTING'
      169:  ReadUnknowBlock;//'SEGMENT DISPLAY PRIORITY'
      170:  ReadUnknowBlock;//'SEGMENT PICK PRIORITY'
//Applicatyion Structere Descriptor Elements
      171:  ReadApplicationStrucrureAttribute;//'APPLICATION STRUCTURE ATTRIBUTE'
    else
      ReadUnknowBlock;
    end;
    FReader.DoNextTag;
  end;
  if FRestText <> nil then
    AddRestText(cgmRestrictedText);
end;

procedure TsgCGMImage.AppendToMultyText(const AText: string);
var
  vStyle: string;
begin
  if FRestText.EntType = ceText then
    ConvertToMultyText;
  vStyle := GenerateMTextStyle;
  TsgDXFMText(FRestText).Text := TsgDXFMText(FRestText).Text + vStyle + AText;
end;

procedure TsgCGMImage.ApplyCharOrientation(const AText: TsgDXFText);
var
  vRotarions: TF2DPoint;
  vText: TsgTextProperty;
  vTempStr: string;
  I,J: Integer;
begin
  vText := FReader.Container.Context.Text;
  vRotarions.X := GetAngleByPoints(cnstFPointZero,
    vText.CharacterOrientationBase, False);
  vRotarions.Y := GetAngleByPoints(cnstFPointZero,
    vText.CharacterOrientationUp, False);
  if (vRotarions.X > 90) and (vRotarions.X < 270) then
  begin
    if (vRotarions.Y >= 0) and (vRotarions.Y <= 180) then
      TsgDXFTextAccess(AText).SetUpsideDown(True);
  end
  else
  begin
    if (vRotarions.Y > 180) and (vRotarions.Y < 360) then
      TsgDXFTextAccess(AText).SetUpsideDown(True);
  end;

  case vText.TextPath of
    1: begin
        J := Length(AText.Text);
        for I := J downto 1 do
          vTempStr := vTempStr + AText.Text[I];
        CheckString(vTempStr);
        AText.Text := vTempStr;
        //vRotarions.X := vRotarions.X + 180;
       end;
    2: vRotarions.X := vRotarions.Y;
    3: vRotarions.X := vRotarions.Y + 180;
  end;

  AText.Rotation := vRotarions.X;
end;

procedure TsgCGMImage.ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD);
var
  vPenWidth: Integer;
  vEntity: TsgDXFPenEntityAccess;
begin
  inherited ApplyPen(Sender, AEntColor);
  vPenWidth := SetLineWeigth(TsgDXFEntity(Sender), True);
  if vPenWidth > 1 then
  begin
    vEntity := TsgDXFPenEntityAccess(Sender);
    SetPenStyle(vEntity.LineCap or vEntity.LineJoin, cnstDefMiterlimit);
  end;
end;

procedure TsgCGMImage.ConvertToMultyText;
var
  vText: TsgDXFText;
  vMultyText: TsgDXFMText;
  vStyleStr: string;
  vTextString: string;
begin
  vText := TsgDXFText(FRestText);
  try
    vTextString := vText.Text;
    //for empty text is not call LoadedInternal
    if (vTextString = '') or vText.IsEmpty then
      vText.Text := '*';
    Converter.Loads(vText);
    vMultyText := TsgDXFMText.Create;
    FRestText := vMultyText;
    vMultyText.AssignEntity(vText);
    vMultyText.Point := AddFPoint(vText.StartPoint,
      RotateFPoint(MakeFPoint(0, vText.Height, 0), vText.Rotation));
    vMultyText.Style := vText.Style;
    vMultyText.Height := vText.Height;
    vStyleStr := GetMStyle(vText.Style.FontName, vText.ColorCAD, vText.Height);
    vMultyText.Text := vStyleStr + vTextString;
    vMultyText.Align := 1;
    vMultyText.Angle := vText.Rotation;
    vMultyText.Scale := MakeFPoint(vText.Scale, vText.Scale, 1);
  finally
    vText.Free;
  end;
end;

function TsgCGMImage.CreateCircle(const APoints: array of TFPoint;
  const APointCalc: TFPoint; const ARadius: Double): TsgDXFCircle;
var
  vPointsCount, vLowIndex: Integer;
  vArc: TsgDXFArc;
  vAngleMiddle, vAngleSwap, vStartAngle, vEndAngle: Double;
begin
  Result := nil;
  vLowIndex := Low(APoints);
  vPointsCount := High(APoints) - vLowIndex + 1;
  if (vPointsCount > 0) and (ARadius > 0) then
  begin
    if vPointsCount > 1 then
      Result := TsgDXFArc.Create
    else
      Result := TsgDXFCircle.Create;
    Result.Point := APoints[vLowIndex];
    Result.Radius := ARadius;
    if vPointsCount >= 3 then
    begin
      vArc := TsgDXFArc(Result);
      vStartAngle := GetAngleByPoints(APointCalc, APoints[vLowIndex + 1], False);
      vEndAngle := GetAngleByPoints(APointCalc, APoints[vLowIndex + 2], False);

      vArc.StartAngle := vStartAngle;
      vArc.EndAngle := vEndAngle;
      if vPointsCount > 3 then
      begin
        vAngleMiddle := GetAngleByPoints(APointCalc, APoints[vLowIndex + 3], False);
        if not IsAngleInAngles(vAngleMiddle, vArc.StartAngle, vArc.EndAngle) then
        begin
          vAngleSwap := vArc.StartAngle;
          vArc.StartAngle := vArc.EndAngle;
          vArc.EndAngle := vAngleSwap;
        end;
      end;

      SetRegionParam(GetPointOnCircle(Result.Point,  Result.Radius, vStartAngle),
        GetPointOnCircle(Result.Point,  Result.Radius, vEndAngle));

      if IsEqual(vArc.StartAngle, vArc.EndAngle) then
        vArc.EndAngle := vArc.EndAngle + 360;
    end;
  end;
end;

function TsgCGMImage.CreateCurvePolygon(const ABasePoint: PFPoint = nil): Boolean;
begin
  Result := False;
  FBoxByCurvePolygon := cnstBadRect;
  if FReader.Container.Context.Hatch.InteriorStyle = isHatch then
  begin
    FCADCurvePolygon := TsgCADCurvePolygon(TsgCADHatch.Create);
    if ABasePoint <> nil then
      AddPatternInHatch(TsgCADHatch(FCADCurvePolygon), ABasePoint^);
    if TsgCADHatch(FCADCurvePolygon).HatchPatternData.Count = 0 then
    begin
      if FClearHatches = nil then
        FClearHatches := TsgObjectList.Create;
      FClearHatches.Add(FCADCurvePolygon);
    end;
  end
  else
    FCADCurvePolygon := TsgCADCurvePolygon.Create;
  if Assigned(FCADCurvePolygon) then
  begin
    Result := True;
    FCADCurvePolygon.ColorCAD := GetCADColour(FReader.Container.Context.Hatch.Colour);
    AddEntityInSection(FCADCurvePolygon);
  end;
end;


function TsgCGMImage.CreateEllipse(const APoints: array of TFPoint):
  TsgDXFEllipse;
var
  vPointsCount, vIndexLow, vIndexHigh: Integer;
  vCenterPoint, vConjugatePoint1, vConjugatePoint2,
    vPointStart, vPointEnd: PFPoint;
  vPoint: TFPoint;
  vAngle: Double;
  vDistA, vTemp: Double;
  vRatPoint1,vRatPoint2 : TFPoint;
  vUseMetod: Boolean;
begin
  Result := nil;
  vUseMetod := False;
  vIndexLow := Low(APoints);
  vIndexHigh := High(APoints);
  vPointsCount := vIndexHigh - vIndexLow + 1;
  if vPointsCount > 2 then
  begin
    vCenterPoint := @APoints[vIndexLow];
    vConjugatePoint1 := @APoints[vIndexLow + 1];
    vConjugatePoint2 := @APoints[vIndexLow + 2];
    vPointStart := nil;
    vPointEnd := nil;
    if vPointsCount > 3 then
    begin
      vPointStart := @APoints[vIndexLow + 3];
      vPointEnd := @APoints[vIndexLow + 4];
    end;
    vAngle := GetAngleOfLinesEx(vCenterPoint, vConjugatePoint1,
      vConjugatePoint2, @GetPointOfPFPoint);
    vRatPoint1 := vConjugatePoint1^;
    vRatPoint2 := vConjugatePoint2^;
    if sgMod(Abs(vAngle), 90) <> 0 then
    begin
      vUseMetod := not GetEllipseParams(vCenterPoint^, vConjugatePoint1^,
        vConjugatePoint2^, vRatPoint1, vRatPoint2);
    end;
    if vUseMetod then
      Result := CreateEllipseEx(vCenterPoint, vConjugatePoint1, vConjugatePoint2,
        vPointStart, vPointEnd)
    else
    begin
      Result := DXFConv.CreateCADEllipticArc(vCenterPoint^, vRatPoint1,
          vRatPoint2, vPointStart, vPointEnd);
      // Determining the direction of drawing
      if Assigned(Result) then
      begin
        vAngle := GetAngleByPoints(vCenterPoint^, vConjugatePoint1^, False);
        vDistA := DistanceFPoint2D(vConjugatePoint1^, vCenterPoint^);
        vPoint := GetPointOnCircle(vCenterPoint^, vDistA, vAngle + 90);

        Converter.Loads(Result);
        SetRegionParam(Result.Points[0],Result.Points[Result.PointCount - 1]);

        if (vPointsCount > 3) and (not IsEqualDirection(vPoint,
          vConjugatePoint2^, vCenterPoint^)) then
        begin
          vTemp := Result.EndAngle;
          Result.EndAngle := Result.StartAngle;
          Result.StartAngle := vTemp;
        end;

        if IsEqual(Result.StartAngle, Result.EndAngle) then
          Result.EndAngle := Result.EndAngle + 360;
      end;

    end;
  end;
end;

function TsgCGMImage.CreateReader(const AStream: TStream): TsgCGMReader;
var
  vIndex: Integer;
begin
  Result := TsgCGMReaderBin.Create;
  try
    Result.InitInternalParams(AStream);
    repeat
      vIndex := Result.ReadTagHeader;
      if vIndex = 0 then
        Result.DoNextTag
      else
        Break;
    until Result.Position >= Result.Size;//read no-ops
    if not ((Result.Position < Result.Size) and (vIndex = 1)) then
    begin
      Result.Free;
      Result := TsgCGMReaderStr.Create;
      AStream.Position := 0;
      Result.InitInternalParams(AStream);
    end;
    Result.Position := 0;
    Result.OnReadError := ReadError;
  except
    Result := nil;
  end;
end;

function TsgCGMImage.CreateTile: TObject;
begin
  if FTileArray = nil then
    FTileArray := TcgmTileArray.Create
  else
  begin
    if Assigned(TcgmTileArray(FTileArray).Decompressor) then
      FreeAndNil(TcgmTileArray(FTileArray).Decompressor);
  end;
  TcgmTileArray(FTileArray).Decompressor := TsgDecompressor.Create;
  Result := FTileArray;
end;

function TsgCGMImage.GenerateMTextStyle: string;
var
  vTextProperty: TsgTextProperty;
  vStyle: TsgDXFStyle;
begin
  vTextProperty := FReader.Container.Context.Text;
  vStyle := GetTextFontStyle(vTextProperty.TextFontIndex);
  Result := GetMStyle(vStyle.FontName,
    GetCADColour(vTextProperty.Colour),
    vTextProperty.CharacterHeight)
end;

function TsgCGMImage.GetActiveSection: TsgDXFBlock;
begin
  if Assigned(FCurrentInsert)  then
    Result := FCurrentInsert.Block
  else
    Result := CurrentLayout.PaperSpaceBlock;
end;

function TsgCGMImage.GetCharacterHeight: Double;
var
  vVDC: TFRect;
begin
  Result := FReader.Container.Context.Text.CharacterHeight;
  if Result = 0 then
  begin
    vVDC := FReader.Container.Precision.VDCExtents;
    Result :=  Max(Abs(vVDC.Right - vVDC.Left),
      Abs(vVDC.Bottom - vVDC.Top)) * 0.01;
  end;
end;

function TsgCGMImage.GetCADColour(const AColor: TsgCgmColor): TsgColorCAD;
begin
  Result := FReader.ConvertColorCGMToColor(AColor);
  if bAlternateBlack then//equal DefExportParams.IsParseWhite
  begin
    if (Result.Active = acRGBColor) and (Result.Color = 0) then//is black
      Result := cnstColorCADByBlackWhite;
  end;
end;

function TsgCGMImage.GetGroupsElements(
  const ATypeEntity: TsgCgmTypeEntity): TsgCgmGroupElements;
var
  I: TsgCgmGroupElements;
begin
  Result := geNone;
  for I := Low(TsgCgmGroupElements) to High(TsgCgmGroupElements) do
  begin
    if ATypeEntity in cnstGroupElements[I] then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TsgCGMImage.GetHatchStyle(const AIndex: Integer): TObject;
var
  I: Integer;
begin
  Result := nil;
  if FHatchStyles <> nil then
  begin
    I := 0;
    while (I < FHatchStyles.Count) and
      (TcgmHatchStyle(FHatchStyles[I]).Index <> AIndex) do
      Inc(I);
    if I < FHatchStyles.Count then
      Result := FHatchStyles[I];
  end;
end;

function TsgCGMImage.GetImageType: TsgExportFormat;
begin
  Result := efCgm;
end;

function TsgCGMImage.GetPixelFormat(const APrecision: Integer): TPixelFormat;
begin
  if  FReader.Container.Precision.ColourSelectionMode = cmIndexed then
  begin
    if APrecision > 8 then
      Result := pf24bit
    else
      Result := sgFunction.GetPixelFormat(APrecision);
  end
  else
  begin
    Result := sgFunction.GetPixelFormat(APrecision * FReader.GetComponentsCount);
    if FReader.Container.Precision.ColourModel <> cmlRGB then
      Result := pf24bit;
  end;
end;

function TsgCGMImage.GetScaleVDC(const AVDC: TFRect): Double;
var
  vDeltaVDC, vDeltaDefaultVDC: TFPoint;
  vDefaultVDC: TFRect;
  vIndex: Integer;
begin
  vDefaultVDC := cnstIntegerRect;//FReader.GetDefaultVDCExtents;
  vDeltaDefaultVDC := SubFPoint2D(vDefaultVDC.TopLeft, vDefaultVDC.BottomRight);
  vDeltaVDC := SubFPoint2D(AVDC.TopLeft, AVDC.BottomRight);
  vIndex := Integer(Abs(vDeltaVDC.X) < Abs(vDeltaVDC.Y));
  if sgIsZero(vDeltaDefaultVDC.V[vIndex]) then
    vDeltaDefaultVDC.V[vIndex] := 1;
  Result := vDeltaVDC.V[vIndex] / vDeltaDefaultVDC.V[vIndex];
end;

function TsgCGMImage.GetTextFontStyle(ATextFontIndex: Integer): TsgDXFStyle;
begin
  Result := nil;
  if FFontStyles.Count > 0 then
  begin
    if ATextFontIndex >= FFontStyles.Count then
      ATextFontIndex := FFontStyles.Count - 1;
    if ATextFontIndex < 0 then
      ATextFontIndex := 0;
    Result := TsgDXFStyle(FFontStyles[ATextFontIndex]);
  end;
end;

function TsgCGMImage.GetTextPoint(const ADeltaWidth,
  ADeltaHeight: Double; const ATextPosition: TFPoint): TFPoint;
var
  vTextPath: Integer;
  vHorizontalAlignment: TsgCgmTextHorizontalAlignment;
  vVerticalAlignment: TsgCgmTextVerticalAlignment;
  vContinuousHorizontalAlignment: Double;
  vContinuousVerticalAlignment: Double;
  vPosX, vPosY: Double;
begin
  vPosX := 0;
  vPosY := 0;
  vTextPath := FReader.Container.FContext.Text.TextPath;
  vHorizontalAlignment := FReader.Container.FContext.Text.HorizontalAlignment;
  vVerticalAlignment := FReader.Container.FContext.Text.VerticalAlignment;
  vContinuousHorizontalAlignment :=
    FReader.Container.FContext.Text.ContinuousHorizontalAlignment;
  vContinuousVerticalAlignment :=
    FReader.Container.FContext.Text.ContinuousVerticalAlignment;
  case vTextPath of
    0: {RIGHT}
    begin
      case vHorizontalAlignment of
        thaNormal, thaLeft: vPosX := 0;
        thaRight: vPosX := -ADeltaWidth;
        thaCentre: vPosX := -ADeltaWidth * 0.5;
        thaContinuous: vPosX := -ADeltaWidth * vContinuousHorizontalAlignment;
      end;
      case vVerticalAlignment of
        tvaNormal,tvaBase: vPosY := 0;
        tvaTop: vPosY := ADeltaHeight;
        tvaCap: vPosY := 0; {TODO}
        tvaHalf: vPosY := ADeltaHeight * 0.5;
        tvaBottom: vPosY := 0; {TODO}
        tvaContinuous: vPosY := ADeltaHeight * vContinuousVerticalAlignment;
      end;
    end;
    1: {LEFT}
    begin
      case vHorizontalAlignment of
        thaNormal: vPosX := -ADeltaWidth;
        thaLeft: vPosX := 0;
        thaRight: vPosX := -ADeltaWidth;
        thaCentre: vPosX := -ADeltaWidth * 0.5;
        thaContinuous: vPosX := -ADeltaWidth * vContinuousHorizontalAlignment;
      end;
      case vVerticalAlignment of
        tvaNormal,tvaBase: vPosY := 0;
        tvaTop: vPosY := ADeltaHeight;
        tvaCap: vPosY := 0; {TODO}
        tvaHalf: vPosY := ADeltaHeight * 0.5;
        tvaBottom: vPosY := 0; {TODO}
        tvaContinuous: vPosY := ADeltaHeight * vContinuousVerticalAlignment;
      end;
    end;
    2: {UP}
    begin
      case vHorizontalAlignment of
        thaNormal,thaCentre: vPosX := -ADeltaWidth * 0.5;
        thaLeft: vPosX := 0;
        thaRight: vPosX := -ADeltaWidth;
        thaContinuous: vPosX := -ADeltaWidth * vContinuousHorizontalAlignment;
      end;
      case vVerticalAlignment of
        tvaNormal,tvaBase: vPosY := 0;
        tvaTop: vPosY := ADeltaHeight;
        tvaCap: vPosY := ADeltaHeight;
        tvaHalf: vPosY := ADeltaHeight * 0.5;
        tvaBottom: vPosY := 0; {TODO}
        tvaContinuous: vPosY := ADeltaHeight * vContinuousVerticalAlignment;
      end;
    end;
    3: {DOWN}
    begin
      case vHorizontalAlignment of
        thaNormal,thaCentre: vPosX := -ADeltaWidth * 0.5;
        thaLeft: vPosX := 0;
        thaRight: vPosX := -ADeltaWidth;
        thaContinuous: vPosX := -ADeltaWidth * vContinuousHorizontalAlignment;
      end;
      case vVerticalAlignment of
        tvaNormal: vPosY := ADeltaHeight;
        tvaBase: vPosY := 0;
        tvaTop: vPosY := ADeltaHeight;
        tvaCap: vPosY := ADeltaHeight;
        tvaHalf: vPosY := ADeltaHeight * 0.5;
        tvaBottom: vPosY := 0; {TODO}
        tvaContinuous: vPosY := ADeltaHeight * vContinuousVerticalAlignment;
      end;
    end;
    4:
  end;
  Result := MakeFPoint(vPosX,vPosY, 0);
end;

function TsgCGMImage.GetLineTypeScale: Double;
begin
  Result := GetScaleVDC(FReader.Container.Precision.VDCExtents) *
    cnstKoefByDefaultImageSize;
  if Result <= 0 then
    Result := 1;
end;

function TsgCGMImage.GetMetafileVersion: Integer;
begin
  Result := 0;
  if Assigned(FReader) then
    Result :=  FReader.FMetafileVersion;
end;

procedure TsgCGMImage.InitStandartStyleByText;
var
  vStyle: TsgDXFStyle;
begin
  vStyle := Converter.StyleByName(sStandardName);
  vStyle.FontName := cnstDefaultFontName;
  vStyle.PrimaryFont := cnstDefaultPrimaryFont;
end;

function TsgCGMImage.IsDrawHatch: Boolean;
begin
  Result := FReader.Container.Context.Hatch.InteriorStyle in [isSolid, isHatch];
end;

function TsgCGMImage.MakeCloseLineByArc(const AArc: TsgDXFCircle;
  const AType: Integer): TsgDXFLWPolyline;
begin
  Result := nil;
  if Atype < 0 then Exit;
  Converter.Loads(AArc);
  Result := TsgDXFLWPolyline.Create;
  AddVertexInPolyline(Result, AArc.PolyPoints.First);
  if Atype = 0 then//this pie
    AddVertexInPolyline(Result, AArc.Point);
  AddVertexInPolyline(Result, AArc.PolyPoints.Last);
end;

procedure TsgCGMImage.MakePolyBezier(const ASpline: TsgDXFSpline;
  const AControlPointsList: TFPointList);
var
  I, J, vRepeat: Integer;
begin
  for I := 0 to Floor((AControlPointsList.Count / 4) - 1) do
  begin
    vRepeat := 0;
    repeat
      ASpline.Knots.AppendConst(4, 0);
      ASpline.Knots.AppendConst(4, cnstSingle1);
      for J := 0 to 3 do
        ASpline.Controls.Add(AControlPointsList[I * 4 + J]);
      Inc(vRepeat);
    until vRepeat = 2;
  end;
end;

procedure TsgCGMImage.NewRegion(const ACreateNewBoundary: Boolean);
var
  vFirstPoint, vLastPoint: TFPoint;
begin
  if (FRegionParametrs <> nil) then
  begin
    if (FRegionParametrs^.IsInitStartPoint) then
    begin
      vFirstPoint := FRegionParametrs^.LastPointElement;
      vLastPoint := FRegionParametrs^.StartPointRegion;
      if FRegionParametrs^.IsConnectionEdge then
      begin
        FRegionParametrs^.IsConnectionEdge := False;
        AddConnectionEdgeEntity(vFirstPoint, vLastPoint)
      end
      else
      begin
        if not FRegionParametrs^.IsCloseRegion then
          AddConnectionEdgeInHatch(vFirstPoint, vLastPoint)
        else
          FRegionParametrs^.IsCloseRegion := False;
      end;
      FRegionParametrs^.IsInitStartPoint := False;
    end;
  end;
  if ACreateNewBoundary and Assigned(FCADCurvePolygon) then
    FCADCurvePolygon.AddBoundaryList(1);
end;

procedure TsgCGMImage.ReadAltCharSetIndex;
begin
//  for future version
//  vAltCharSetIndex := FParameters.TwoToOne(GetDataByte(FIndexBuffer + 2]),
//      GetDataByte(FIndexBuffer + 3]));
  FReader.Container.Context.Text.AlternateCharacterSetIndex := FReader.ReadIX;
end;

procedure TsgCGMImage.ReadApndText;
var
  vFlags: Word;
  vAppendText: string;
begin
  vFlags := FReader.ReadE;
  vAppendText := FReader.ReadSF(False);
  if FRestText <> nil then
  begin
    AppendToMultyText(vAppendText);
    if vFlags > 0 then
      AddRestText(cgmAppendText);
  end;
end;


{$IFDEF SG_CGM_DEBUG}
var
  gTab: Integer;
{$ENDIF}

procedure TsgCGMImage.ReadBeginApplicationStructure;
var
  vApplicationStructureIdentifier: string;
  vApplicationStructureType: TsgCgmApsToken;
{$IFDEF SG_CGM_DEBUG}
  vInheritanceFlag: Integer;
{$ENDIF}
  vAPSObject: TsgDXFEntity;

  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;

  function GetApsToken(ApplicationStructureType: string): TsgCgmApsToken;
  var
    vToken: TsgCgmApsToken;
  begin
    Result := catNone;
    for vToken := catLayers to High(cnstAPSToken) do
      if CompareText(cnstAPSToken[vToken], LowerCase(ApplicationStructureType)) = 0 then
      begin
        Result := vToken;
        Break;
      end;
  end;

begin
  vApplicationStructureIdentifier := FReader.ReadSF;
  vApplicationStructureType := GetApsToken(FReader.ReadSF);  //vApplicationStructureType := FReader.ReadSF;
{$IFDEF SG_CGM_DEBUG}
  vInheritanceFlag := FReader.ReadE;
{$ELSE}
  (*vInheritanceFlag := *)FReader.ReadE;
{$ENDIF}

  case vApplicationStructureType of
    catLayers:
      begin
        FCurrentLayerIfGiven := Converter.LayerByName(cnstLayerNoName);
        FStackAPS.Push(FCurrentLayerIfGiven);
      end;
    catGrobject:
      begin
        vBlock := TsgDXFBlock.Create;
        vBlock.Name := GetNameCorrect(vApplicationStructureIdentifier);
        Converter.Sections[csBlocks].AddEntity(vBlock);

        vInsert := TsgDXFInsert.Create;
        vInsert.ColorCAD := cnstColorCADByLayer;
        vInsert.LineWeight := fLineWeightByLayer;
        vInsert.Block := vBlock;
        FStackAPS.Push(vInsert);
        if Assigned(FCurrentInsert) then
          PushInsert(FCurrentInsert);
        AddEntityInSection(vInsert);
        FCurrentInsert := vInsert;
      end;
    catPara, catGrnode, catGdata:
      begin
        vAPSObject := cnstAPSClasses[vApplicationStructureType].Create;
        FStackAPS.Push(vAPSObject);
      end;
    catNone:
      begin
        vAPSObject := nil;
        FStackAPS.Push(vAPSObject);
       //sgNop;
     end;
  end;


{$IFDEF SG_CGM_DEBUG}
  Inc(gTab,2);
  FAPS.Add(Format('%sBegin Application Structure',[StringOfChar(' ', gTab)]));
  FAPS.Add(Format('%sASI = %s AST = %s Flag = %d',
    [StringOfChar(' ', gTab+2),vApplicationStructureIdentifier, cnstAPSToken[vApplicationStructureType], vInheritanceFlag]));
{$ENDIF}
end;

procedure TsgCGMImage.ReadEndApplicationStructure;
var
  vAPSObject: TsgDXFEntity;
  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;
  vEntity: TsgDXFEntity;
  I: Integer;
begin
  vAPSObject := FStackAPS.Pop;
  if Assigned(vAPSObject) then
  begin
    if vAPSObject.ClassNameIs('TsgDXFLayer') then
      FCurrentLayerIfGiven := nil
    else if vAPSObject.ClassNameIs('TsgDXFInsert') then
    begin
      FCurrentInsert := PopInsert;
      vInsert := TsgDXFInsert(vAPSObject);
      vBlock := vInsert.Block;
      I := 0;
      while I < vBlock.Count do
      begin
        vEntity := vBlock[I];
        Converter.Loads(vEntity);
        Inc(I);
      end;
      Converter.Loads(vBlock);
      Converter.Loads(vInsert);
    end
    else
      if vAPSObject is TsgCustomAps then
        vAPSObject.Free;
  end;

{$IFDEF SG_CGM_DEBUG}
  FAPS.Add(Format('%sEnd Application Structure',[StringOfChar(' ', gTab)]));
  Dec(gTab,2);
  ReadUnknowBlock;
{$ENDIF}
end;

procedure EnumXML(const AXML: TsgNode; AList: TStringList; APreAmbula: string);
var
  I, J: Integer;
  vNode, vNodeAttrib: TsgNodeSample;
begin
  if AXML.ChildNodesCount > 0 then
  begin
    for I := 0 to AXML.ChildNodesCount - 1 do
    begin
      vNode := AXML.ChildNodes[I];
      EnumXML(TsgNode(vNode), AList, APreAmbula + '_' + vNode.Name);
      if vNode.AttributeNodesCount > 0 then
      begin
        for J := 0 to vNode.AttributeNodesCount - 1 do
        begin
          vNodeAttrib := vNode.AttributeNodes[J];
          AList.Add(APreAmbula);
          AList.Add(vNodeAttrib.Value);
        end;
      end
    end;
  end;
end;

procedure TsgCGMImage.ReadApplicationStrucrureAttribute;
var
  I: Integer;
  vApplicationStructureAttributeType: string;
  vXMLSDR: TsgParser;
  vRoot: TsgNode;
  vAPSObject: TsgDXFEntity;
  vNode: TsgNodeSample;
  vList: TStringList;

  procedure AddAPSAttrib(AInsert: TsgDXFInsert; ATag, AText: string);
  var
    vAttrib: TsgDXFAttrib;
  begin
    vAttrib := TsgDXFAttrib.Create;
    AInsert.AddEntity(vAttrib);
    vAttrib.NotAppear := True;
    vAttrib.Tag := ATag;
    vAttrib.Text := AText;
    vAttrib.Value := vAttrib.Text;
    vAttrib.Point := FPointXMat(cnstFPointZero, AInsert.GetMatrix);
    vAttrib.Height := 1;
    Converter.Loads(vAttrib);
  end;

begin
  vApplicationStructureAttributeType := FReader.ReadSF;
  vXMLSDR := TsgParser.Create;
  try
    vRoot := vXMLSDR.CreateRootNode('SDR_ROOT', True);
    ReadSDRData(FReader, vRoot);
    vAPSObject := FStackAPS.List[FStackAPS.Count - 1];
    if Assigned(vAPSObject) then
    begin
      if vAPSObject.ClassNameIs('TsgDXFLayer') then
      begin
        if vApplicationStructureAttributeType = 'layername' then
        begin
          vNode := vRoot.GetChildByPathName('SF/Value');
          if Assigned(vNode) then
            FCurrentLayerIfGiven.Name := GetNameCorrect(vNode.GetAttributeByName('String').Value);
        end;
      end;
    end;
    //
    // Add atribb
    //
    if Assigned(FCurrentInsert) then
    begin
      vList := TStringList.Create;
      try
        EnumXML(vRoot, vList, vApplicationStructureAttributeType);
        for I := 0 to vList.Count div 2 - 1  do
          AddAPSAttrib(FCurrentInsert, vList.Strings[I*2], vList.Strings[I*2+1]);
      finally
        vList.Free;
      end;
    end;

  finally
    vXMLSDR.Free;
  end;

{$IFDEF SG_CGM_DEBUG}
  FAPS.Add(Format('%sApplication Strucrure Attribute',[StringOfChar(' ', gTab+2)]));
  FAPS.Add(Format('%sASAT = %s SDR = %s',
    [StringOfChar(' ', gTab+4), vApplicationStructureAttributeType, vRoot.ToString]));
{$ENDIF}
end;

procedure TsgCGMImage.ReadBeginApplicationStructureBody;
begin
{$IFDEF SG_CGM_DEBUG}
  FAPS.Add(Format('%sBegin Application Structure Body',[StringOfChar(' ', gTab+2)]));
{$ENDIF}
  ReadUnknowBlock;
end;

procedure TsgCGMImage.ReadBackGroundColor;// 0x$20 | 0xE3
var
  vBackground: TsgColorCAD;
begin
//  vBackground := FReader.ConvertColorCGMToColor(FReader.ReadCD);
  vBackground := GetCADColour(FReader.ReadCD);
  FReader.Container.BackgroundColour := vBackground;
end;

procedure TsgCGMImage.ReadBeginPicture;
var
  vName: string;
  vLayout: TsgDXFLayout;
begin
  vName := FReader.ReadSF;
  if not IsEmptyBlock(CurrentLayout.PaperSpaceBlock) then
  begin
    if Length(vName) = 0 then
      vName := sLayout;
    vLayout := TsgDXFLayout.Create;
    vLayout.Name := vName;
    if Converter.LayoutByName(vLayout.Name) <> nil then
      vLayout.Name := vLayout.Name + '(' + IntToStr(Converter.LayoutsCount) + ')';
    Converter.AddLayout(vLayout);
    CurrentLayout := vLayout;
  end
  else
  begin
    if FInsertClip <> nil then
      Converter.DeleteBlock(FInsertClip.Block, True);
    FInsertClip := nil;
    if FInsertVDC <> nil then
      Converter.DeleteBlock(FInsertVDC.Block, True);
    FInsertVDC := nil;
    FCurrentInsert := nil;
    CurrentLayout.Clear(True);
  end;
end;

procedure TsgCGMImage.ReadBeginPictureBody;
var
  vBackGround: TsgDXFSolid;
  vExtents: TFRect;
  vScale: TFPoint;
  vInsert: TsgDXFInsert;
begin
  ReadUnknowBlock;
  GenerateLineTypesEx(Converter, cnstLTPrefix, False, 2);
  LoadInsert(True);
  InitParamsByReader;
  vExtents := FReader.Container.Precision.VDCExtents;
  vScale := cnstFPointSingle;
  if FReader.Container.Precision.ScalingMode = smMetric then
    vScale := PtXScalar(vScale, FReader.Container.Precision.ScalingModeFactor);
  if vExtents.Left > vExtents.Right then
    vScale.X := -1*vScale.X;
  if vExtents.Top > vExtents.Bottom then
    vScale.Y := -1*vScale.Y;
//  if (FReader.Container.Precision.ScalingMode = smMetric) and
//    (FMetafileVersion = 1) and
//    (FProfileId = '') then
//  begin
//    // special fix by N4_Generic1.CGM, 1593b-7022.cgm, 1593b-7027.cgm,
//    //                sample1.cgm
//  end
//  else
  begin
    vInsert := AddInsert(True, vScale, @vExtents);
    vInsert.ColorCAD := FReader.Container.BackgroundColour;
    case cgmBackGroundMode of
      bgFrame:
        begin
          {$IFDEF SG_FIREMONKEY}
          vExtents.Z1 := -0.5;
          vExtents.Z2 := vExtents.Z1;
          {$ENDIF}
          vBackGround := TsgDXFSolid.Create;
          vInsert.Block.AddEntity(vBackGround);
          vBackGround.ColorCAD := MakeColorCAD(acIndexColor, clDXFByBlock);
          vBackGround.Point := vExtents.TopLeft;
          vBackGround.Point1 := MakeFPoint(vExtents.Right, vExtents.Top, vExtents.Z1);
          vBackGround.Point3 := vExtents.BottomRight;
          vBackGround.Point2 := MakeFPoint(vExtents.Left, vExtents.Bottom, vExtents.Z2);
          vBackGround.Layer := TsgDXFConverterAccess(Converter).GetBackGroundLayer;
          Converter.Loads(vBackGround);
        end;
      bgFill:
        begin
          BackgroundColor := ConvertColorCADToRGB(vInsert.ColorCAD);
        end;
    end;
  end;
end;

procedure TsgCGMImage.ReadBeginTileArray;
var
  vTileArray: TcgmTileArray;
begin
  vTileArray := TcgmTileArray(CreateTile);
  vTileArray.Position := FReader.ReadP;

{ Value X - Angle (0 0, 1 90, 2 180, 3 270)}
{ Value Y - Angle (0 90, 1 270) }
  vTileArray.Direction := FReader.ReadDirection;

  vTileArray.NumberOfTiles.X := FReader.ReadI;
  vTileArray.NumberOfTiles.Y := FReader.ReadI;

  vTileArray.NumberOfCells.X := FReader.ReadI;
  vTileArray.NumberOfCells.Y := FReader.ReadI;

  vTileArray.CellSize.X := FReader.ReadR;
  vTileArray.CellSize.Y := FReader.ReadR;

  vTileArray.ImageOffset.X := FReader.ReadI;
  vTileArray.ImageOffset.Y := FReader.ReadI;

  vTileArray.ImageNumberOfCells.X := FReader.ReadI;
  vTileArray.ImageNumberOfCells.Y := FReader.ReadI;
  //
  if vTileArray.Decompressor <> nil then
  begin
    vTileArray.Decompressor.NumberOfCells := vTileArray.NumberOfCells;
    vTileArray.Decompressor.NumberOfTiles := vTileArray.NumberOfTiles;
    if (*(vTileArray.NumberOfCells.X > cnstMaxNumberCellTile) or
       (vTileArray.NumberOfCells.Y > cnstMaxNumberCellTile) or
       (vTileArray.NumberOfTiles.X > cnstMaxNumberTiles) or
       (vTileArray.NumberOfTiles.Y > cnstMaxNumberTiles) or *)
       (not vTileArray.Decompressor.BeforeDecompress(pf1bit)) then
      DisposeTile;
  end;
end;

procedure TsgCGMImage.ReadCellArray;//Reading BidMap of CGM File;
var
  P, Q, R, vH, vW, vG: TFPoint;
  N: TPoint;
  vImageEnt: TsgDXFImageEnt;
  vRepresentationMode: Integer;
  vPixelFormat: TPixelFormat;
  vPrecision: Integer;
  vWidth, vheight: Integer;

  vDecompressor: TsgDecompressor;
begin
  FReader.SaveColorParams;
  try
    P := FReader.ReadP;
    Q := FReader.ReadP;
    R := FReader.ReadP;
    N.X := FReader.ReadI;
    N.Y := FReader.ReadI;

    if ((N.X = 0) and (N.Y = 0)) or (N.X > cnstMaxNumberCellArray) or
       (N.Y > cnstMaxNumberCellArray) then
      Exit;

    vW := SubFPoint2D(P, R);
    vH := SubFPoint2D(Q, R);
    vG := AddFPoint2D(vW, vH);

    vPrecision := FReader.ReadCO_Precision;
    FReader.SetRangeColourByPrecision;

    vRepresentationMode := FReader.ReadRepresentationMode;

    vPixelFormat := GetPixelFormat(vPrecision);

    if vPixelFormat <> pfCustom then
    begin
      vDecompressor := TsgDecompressor.Create;
      try
        vDecompressor.ProcReadColor := FReader.ReadColor;
        vDecompressor.ProcReadAlignBytes := FReader.AlignBytes;
        vDecompressor.NumberOfCells := N;
        if vRepresentationMode = 0 then
          vDecompressor.Compression := ctRLE
        else
          vDecompressor.Compression := ctUncompressed;
        vDecompressor.BeforeDecompress(vPixelFormat);
        if vDecompressor.Execute(cnstPointZero, FReader) then
        begin;
          vDecompressor.AfterDecompress;
          if Assigned(vDecompressor.Bitmap) then
          begin
            vWidth := vDecompressor.Bitmap.Width;
            vHeight := vDecompressor.Bitmap.Height;
            vImageEnt := TsgDXFImageEnt.Create;
            try
              vImageEnt.SetImage(TGraphic(vDecompressor.Bitmap));
              TsgDXFImageEntAccess(vImageEnt).ChangeGraphic;
              vImageEnt.Point := AddFPoint2D(vG, R);
              vImageEnt.UVector := SubFPoint2D(Q, vImageEnt.Point);
              vImageEnt.VVector := SubFPoint2D(P, vImageEnt.Point);
              vImageEnt.Size := MakeFPoint(vWidth, vHeight);
              vImageEnt.UVector := PtXScalar(vImageEnt.UVector,1/vImageEnt.Size.X);
              vImageEnt.VVector := PtXScalar(vImageEnt.VVector,1/vImageEnt.Size.Y);
              Converter.Loads(vImageEnt);
            finally
              AddEntityInSection(vImageEnt);
            end;
          end;
        end;
      finally
        vDecompressor.Free;
      end;
    end;
  finally
    FReader.RestoreColorParams;
  end;
end;

procedure TsgCGMImage.ReadCharacterHeight;//0x51E2
begin
  CharacterHeight := FReader.ReadVDC;
end;

procedure TsgCGMImage.ReadCharacterOrientation;//0x5208
var
  I: Integer;
  vParams: array[0..3] of Double;
begin
  for I := 0 to 3 do
    vParams[I] := FReader.ReadVDC;
  SetCharOrientations(vParams);
end;

procedure TsgCGMImage.ReadCharSetIndex;//0x5261
begin
  FReader.Container.Context.Text.CharacterSetIndex := FReader.ReadIX;
  //ReadUnknowBlock;//for future version
end;

procedure TsgCGMImage.ReadCharSpace;//0x51A4
//var
//  vCharSpace: TRect;
begin
//  Inc(FIndexBuffer, 2);
//  vCharSpace.Left := GetDataByte(FIndexBuffer);
//  Inc(FIndexBuffer);
//  vCharSpace.Top := GetDataByte(FIndexBuffer);
//  Inc(FIndexBuffer);
//  vCharSpace.Right := GetDataByte(FIndexBuffer);
//  Inc(FIndexBuffer);
//  vCharSpace.Bottom := GetDataByte(FIndexBuffer);
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCGMImage.ReadCircle;//0x4186
var
  vCircle: TsgDXFCircle;
  vRadius: Double;
  vPoints: array[0..0] of TFPoint;
begin
  vPoints[0] := FReader.ReadP;
  vRadius := FReader.ReadVDC;
  TypeEntity := cgmCircle;
  vCircle := CreateCircle(vPoints, cnstFPointZero, vRadius);
  if vCircle <> nil then
  begin
    AddEntities(vCircle, nil);
  end;
end;

procedure TsgCGMImage.ReadCircularArc3Point(const AClose: Boolean);
var
  I: Integer;
  vRadius: Double;
  vPointCenter: TFPoint;
  vPoints: array[0..2] of TFPoint;
  vArc: TsgDXFCircle;
  vType: Integer;
  vTypeEntity: TsgCgmTypeEntity;
  vList: TsgObjectList;
begin
  vType := -1;
  vTypeEntity := cgmArc3Point;
  for I := 0 to 2 do
    vPoints[I] := FReader.ReadP;
  if AClose then
  begin
    vTypeEntity := cgmArc3PointClose;
    vType := FReader.ReadCloseSpec;
  end;
  if GetCircleParams(vPoints[0], vPoints[1], vPoints[2],
    vPointCenter, vRadius, fDoubleResolution, True) then
  begin
    vList := TsgObjectList.Create;
    try
      TypeEntity := vTypeEntity;
      vArc := CreateCircle([vPointCenter, vPoints[0], vPoints[2], vPoints[1]],
        vPointCenter, vRadius);
      if vArc <> nil then
      begin
        vList.Add(vArc);
        if AClose then
          vList.Add(MakeCloseLineByArc(vArc, vType));
      end;
      AddEntities(nil, vList);
    finally
      FreeAndNil(vList);
    end;
  end;
end;

procedure TsgCGMImage.ReadCircularArcCenter(const AClose: Boolean);//0x41EE
var
  I: Integer;
  vRadius: Double;
  vPoints: array[0..2] of TFPoint;
  vCircularArcCenter: TsgDXFCircle;
  vTypeEntity: TsgCgmTypeEntity;
  vType: Integer;
  vList: TsgObjectList;
begin
  // Centre point DX_start, DY_start, DX_end, DY_end
  vType := -1;
  vTypeEntity := cgmCircularArcCenter;
  for I := 0 to 2 do
    vPoints[I] := FReader.ReadP;
  vRadius := FReader.ReadVDC;
  if AClose then
  begin
    vTypeEntity := cgmCircularArcCenterClose;
    vType := FReader.ReadCloseSpec;
  end;
  vList := TsgObjectList.Create;
  try
    TypeEntity := vTypeEntity;
    vCircularArcCenter := CreateCircle(vPoints, cnstFPointZero, vRadius);
    if vCircularArcCenter <> nil then
    begin
      vList.Add(vCircularArcCenter);
      if AClose then
        vList.Add(MakeCloseLineByArc(vCircularArcCenter, vType));
    end;
    AddEntities(nil, vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.ReadClipIndicator;
begin
  if FReader.Container.Context.ClipIndicator then
    LoadInsert(False);
  FReader.Container.Context.ClipIndicator := FReader.ReadE <> 0;
  if FReader.Container.Context.ClipIndicator then
    AddInsert(False, cnstFPointSingle, @FReader.Container.Context.ClipRectangle);
end;

procedure TsgCGMImage.ReadClipRectandgle;
begin
  if FReader.Container.Context.ClipIndicator then
  begin
    LoadInsert(False);
    FReader.Container.Context.ClipIndicator := False;
  end;
  FReader.Container.Context.ClipRectangle :=
    MakeFRectByPoints(FReader.ReadP, FReader.ReadP);
  if FReader.Container.Context.ClipIndicator then
    AddInsert(False, cnstFPointSingle, @FReader.Container.Context.ClipRectangle);
end;

function TsgCGMImage.ReadCompressiontType: TsgCgmCompressionType;
var
  vIndex: Integer;
begin
  Result := ctReserved;
  vIndex := FReader.ReadIX;
  if (vIndex >= Integer(Low(TsgCgmCompressionType))) and
     (vIndex <= Integer(High(TsgCgmCompressionType))) then
    Result := TsgCgmCompressionType(vIndex);
end;

procedure TsgCGMImage.ReadConnectingEdge;
begin
  if FRegionParametrs = nil then
    Exit;
  FRegionParametrs^.IsConnectionEdge := True;
  FRegionParametrs^.ConnectionPoint := FRegionParametrs^.LastPointElement;
end;

procedure TsgCGMImage.ReadGeneralizeDrawingPrimitive;
var
  vIdentifier: Integer;
begin
  vIdentifier := FReader.ReadI;
  case vIdentifier of
    -4:
      begin // CGM+ Trace Initialize
        if not ((FSism  <> nil) and (TsgSism(FSism).ActiveSection = GetActiveSection)) then
        begin
          FreeAndNil(FSism);
          FSism := TsgSism.Create(Self);
        end;
        TsgSism(FSism).TraceInitialize;
      end;
    -5:
      begin
        if FSism <> nil then
          TsgSism(FSism).TraceData;
      end;
  end;
end;

procedure TsgCGMImage.ReadEdgeVisibility;//0x53C2
var
  vVisibility: Integer;
begin
  //0 off , 1 on
  vVisibility := FReader.ReadVisibility;
  FReader.Container.Context.Edge.Visibility := (vVisibility and 1 <> 0);
end;

procedure TsgCGMImage.ReadEllipse;//0x422C
var
  I: Integer;
  vEllipse: TsgDXFEllipse;
  vPoints: array [0..2] of TFPoint;
begin
  for I := 0 to 2 do
    vPoints[I] := FReader.ReadP;
  TypeEntity := cgmEllipse;
  try
    vEllipse := CreateEllipse(vPoints);
    if vEllipse <> nil then
      AddEntities(vEllipse, nil);
  except
    FReader.DoReadError;
  end;
end;

procedure TsgCGMImage.ReadEllipticalArc(const AClose: Boolean);//0x4254
var
  I, vType: Integer;
  vEllipticalArc: TsgDXFEllipse;
  vPoints: array [0..4] of TFPoint;
  vList: TsgObjectList;
  vTypeEntity: TsgCgmTypeEntity;
begin
  vTypeEntity := cgmEllipticalArc;
  vType := -1;
  for I := 0 to 4 do
    vPoints[I] := FReader.ReadP;
  if AClose then
  begin
    vTypeEntity := cgmEllipticalArcClose;
    vType := FReader.ReadCloseSpec;
  end;
  vList := TsgObjectList.Create;
  try
    TypeEntity := vTypeEntity;
    vEllipticalArc := CreateEllipse(vPoints);
    if vEllipticalArc <> nil then
    begin
      vList.Add(vEllipticalArc);
      if AClose then
        vList.Add(MakeCloseLineByArc(vEllipticalArc, vType));
    end;
    AddEntities(nil, vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.ReadEndFigure;
var
  vBase: TFPoint;
begin
  NewRegion(False);
  DisposeAndNil(PsgRegionParametrs(FRegionParametrs));
  if IsBadRect(FBoxByCurvePolygon) then
    vBase := cnstFPointZero
  else
    vBase := FBoxByCurvePolygon.TopLeft;
  LoadCurvePolygon(@vBase);
end;

procedure TsgCGMImage.ReadEndTileArray;
var
  vTileArray: TcgmTileArray;
  vImageEnt: TsgDXFImageEnt;
  vScale, vSize: TFPoint;
  vAngle: TFPoint;
  vSin, vCos: Extended;
begin
  vTileArray := TcgmTileArray(FTileArray);
  if (vTileArray <> nil) and (vTileArray.Decompressor <> nil) then
  begin
    vTileArray.Decompressor.AfterDecompress;
    vScale := cnstFPointSingle;
    if vTileArray.CellSize.X <> 0 then
      vScale.X := 1 / vTileArray.CellSize.X;
    if vTileArray.CellSize.Y <> 0 then
      vScale.Y := 1 / vTileArray.CellSize.Y;
    vImageEnt := TsgDXFImageEnt.Create;
    vImageEnt.SetImage(TGraphic(vTileArray.Decompressor.Bitmap));
    {$IFNDEF SG_FIREMONKEY}
    TsgDXFImageEntAccess(vImageEnt).ChangeGraphic;
    {$ENDIF}
    vImageEnt.Point := vTileArray.Position;

    vAngle.X := vTileArray.Direction.X * 90;
    vAngle.Y := 90 + vTileArray.Direction.Y * 180;

    vSize := MultiplyFPoint(vScale,
      MakeFPoint(vTileArray.Decompressor.Bitmap.Width,
                 vTileArray.Decompressor.Bitmap.Height, 0));
    vImageEnt.Point := SubFPoint(vImageEnt.Point, MakeFpoint(0, vSize.Y, 0));

    SinCos(Radian(vAngle.X), vSin, vCos);
     vImageEnt.UVector := MakeFPoint(vCos, vSin, 0);

    SinCos(Radian(vAngle.Y + 180), vSin, vCos);
    vImageEnt.VVector := MakeFPoint(vCos, vSin, 0);

    vImageEnt.Size := MakeFPoint(vTileArray.Decompressor.Bitmap.Width,
      vTileArray.Decompressor.Bitmap.Height);
    vImageEnt.UVector := PtXScalar(vImageEnt.UVector, vScale.X);
    vImageEnt.VVector := PtXScalar(vImageEnt.VVector, vScale.Y);

    Converter.Loads(vImageEnt);
    AddEntityInSection(vImageEnt);
  end;
  DisposeTile;
  ReadUnknowBlock;
end;

procedure TsgCGMImage.ReadError(Sender: TObject);
begin
  FError := True;
  TsgDXFConverterAccess(Converter).LoadStopped := True;
end;

procedure TsgCGMImage.ReadFillColor;// //0x5083 Mask +19,+21,+22
begin
  FReader.Container.Context.Hatch.Colour := FReader.ReadCgmColor;
end;

procedure TsgCGMImage.ReadLenMetadata;
begin
  ReadUnknowBlock;
end;

procedure TsgCGMImage.ReadLineAndEdgeTypeDefinition;
var
  vIndex: Integer;
  vLineType: TsgDXFLineType;
  vName: string;
  {vLength, }vTick, vKoef: Double;
begin
  vIndex := FReader.ReadIX;
  vName := GetLineTypeName(Converter, vIndex);
  if (vIndex < 0) and (Converter.LTypeByName(vName) = nil) then
  begin
    vKoef := 0.25;// by Image Size = cnstImageSizeM
    {vLength := }FReader.ReadSS(FReader.Container.Context.Line.SpecMode);
    vLineType := TsgDXFLineType.Create;
    try
      vLineType.Name := vName;
      while not FReader.IsTagEOF do
      begin
        vTick := FReader.ReadI;
        vLineType.Lines.AddTick(vKoef * vTick);
        vLineType.Lines.Uniform := 2;
        vKoef := -vKoef;
      end;
      Converter.Loads(vLineType);
    finally
      Converter.Sections[csLTypes].AddEntity(vLineType);
    end;
  end;
end;

procedure TsgCGMImage.ReadLineCap(const AProperty: TsgLineProperty);
var
  vLineCap: Integer;
begin
  case FReader.ReadLineCap of
    1,3: vLineCap := PS_ENDCAP_ROUND;
    4: vLineCap := PS_ENDCAP_SQUARE;
    5: vLineCap := PS_ENDCAP_FLAT; //triangle
  else
    vLineCap := PS_ENDCAP_FLAT;
  end;
  AProperty.PenStyle := (AProperty.PenStyle and (not  PS_ENDCAP_MASK))
    or vLineCap;
  FReader.ReadDashCap;
end;

procedure TsgCGMImage.ReadLineJoin(const AProperty: TsgLineProperty);
var
  vLineJoin: Integer;
begin
  case FReader.ReadLineJoin of
    1,3: vLineJoin := PS_JOIN_ROUND;
    4: vLineJoin := PS_JOIN_BEVEL;
  else
    vLineJoin := PS_JOIN_MITER;
  end;
  AProperty.PenStyle := (AProperty.PenStyle and (not  PS_JOIN_MASK))
    or vLineJoin;
end;

procedure TsgCGMImage.ReadLineToMoveTo;
var
  vLine: TsgDXFLine;
  vList: TsgObjectList;
begin
  vList := TsgObjectList.Create;
  try
    while not FReader.IsTagEOF do
    begin
      vLine := TsgDXFLine.Create;
      vList.Add(vLine);
      vLine.Point := FReader.ReadP;
      vLine.Point1 := FReader.ReadP;
    end;
    TypeEntity := cgmPolyline;
    AddEntities(nil, vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.ReadMetafileDescription;
var
  I, vIndex: Integer;
  vParameterList: string;
begin
  vParameterList := UpperCase(FReader.ReadSF);
  CheckString(vParameterList);
  I := AnsiPos(cnstProfileId, vParameterList);
  if I > 0 then
  begin
    Inc(I, Length(cnstProfileId));
    vIndex := StringScan('"', vParameterList, I);
    if vIndex > 0 then
    begin
      FProfileId := Copy(vParameterList, I, vIndex - I);
//      if FProfileId = cnstWebCgm then
//        FReader.Container.Context.Edge.Visibility := True;
    end;
    if (GetMetafileVersion = 4) and SameText(FProfileId, cnstWebCgm) then//web cgm
      SetMetafileVersion(cnstMetafileVersionWebCGM);
  end;
end;

procedure TsgCGMImage.ReadNewRegion;
begin
  NewRegion;
end;

procedure CloseRegion;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCGMImage.ReadFontList;//1 13 x
var
  I: Integer;
  vStrings: TStringList;
  vName: string;
  vFontAlias: TsgFontAliasTable;
begin
  vStrings := FReader.Read_nSF;
  try
    if Assigned(vStrings) then
    begin
      vFontAlias := TsgFontAliasTable(FFontAlias);
      for I := 0 to vStrings.Count - 1 do
      begin
        vName := GetFontNameCorrect(vFontAlias, vStrings[I]);
        AddStyleByText(vName);
      end;
    end;
  finally
    FreeAndNil(vStrings);
  end;
end;

procedure TsgCGMImage.ReadHatchStyleDefinition;
var
  vHatchStyle: TcgmHatchStyle;
  I, vIndex, vCount: Integer;
  vPoint: TFPoint;
  vInteriorStyleSpecMode: TsgCgmWidthSpecMode;
begin
  vIndex := FReader.ReadIX;
  vHatchStyle := TcgmHatchStyle(GetHatchStyle(vIndex));
  if vHatchStyle = nil then
  begin
    if FHatchStyles = nil then
      FHatchStyles := TsgObjectList.Create;
    vHatchStyle := TcgmHatchStyle.Create;
    vHatchStyle.Index := vIndex;
    FHatchStyles.Add(vHatchStyle);
  end;
  vHatchStyle.Indicator := FReader.ReadE;
  vInteriorStyleSpecMode := FReader.Container.Precision.InteriorStyleSpecMode;
  vPoint.X := FReader.ReadSS(vInteriorStyleSpecMode);
  vPoint.Y := FReader.ReadSS(vInteriorStyleSpecMode);
  vPoint.Z := 0;
  vHatchStyle.BasePoint := vPoint;
  vPoint.X := FReader.ReadSS(vInteriorStyleSpecMode);
  vPoint.Y := FReader.ReadSS(vInteriorStyleSpecMode);
  vHatchStyle.OffsetPoint := vPoint;
  vHatchStyle.Length := FReader.ReadSS(vInteriorStyleSpecMode);
  vCount :=  FReader.ReadI;
  I := 0;
  while I < vCount do
  begin
    vHatchStyle.GapWidths.Add(FReader.ReadI);
    Inc(I);
  end;
  I := 0;
  while I < vCount do
  begin
    vHatchStyle.LineTypes.Add(FReader.ReadIX);
    Inc(I);
  end;
end;

procedure TsgCGMImage.ReadBeginFigure;
begin
  LoadCurvePolygon;
  CreateCurvePolygon;
  if FRegionParametrs = nil then
    New(FRegionParametrs);
  FillChar(FRegionParametrs^,sizeof(TsgRegionParametrs), 0);
  NewRegion;
end;

procedure TsgCGMImage.ReadBeginMetafile;
const
  cnstESCTable: array[0..15,0..16] of Byte =
  (     {  |   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2
           |   /   /   /   /   /   /   /   /   /   /   /   /   /   /   /   /
           |   0   1   2   3   4   5   6   7   8   9   10  11  12  13  14 15 }
{2/0}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/1}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/2}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/3}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/4}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(2),(2),(2),(2),(5),(3),(3),(3)),
{2/5}    ((4),(5),(4),(4),(4),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(4)),
{2/6}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/7}    ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/8}    ((0),(5),(0),(0),(0),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/9}    ((0),(5),(0),(0),(0),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/10}   ((0),(5),(0),(0),(0),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/11}   ((0),(5),(0),(0),(0),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/12}   ((5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/13}   ((1),(5),(1),(1),(1),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/14}   ((1),(5),(1),(1),(1),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5)),
{2/15}   ((1),(5),(1),(1),(1),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5),(5))
  );
var
  vString: AnsiString;
  I, vLen, vSkipChar: Integer;
  vFinalByte: Integer;
  vCharacterSetType: TsgCgmCharacterSetType;
  vCharacterSetDesignation: String;
  vItem: TsgISO2375;
{$IFDEF SGDEL_2009}
  vBytes: TBytes;
{$ENDIF}
begin
  if FReader.GetReaderType = crtBin then
  begin
    vString := FReader.ReadSF_A;
    vLen := Length(vString);
    if vLen < 4 then
    begin
      FMetaFileName := String(vString);
      Exit;
    end;
    vFinalByte := 0;
    vSkipChar := 0;
    if Byte(vString[1]) = $1B then
    begin
      for I := 2 to 4 do
      begin
        case Byte(vString[I]) of
          $20..$2F: Inc(vFinalByte);
          else
            Break;
        end;
      end;
    end;
    vCharacterSetDesignation := '';
    vCharacterSetType := cstNone;
    case vFinalByte of
      1, 2: begin
        vCharacterSetType := TsgCgmCharacterSetType(cnstESCTable[Byte(vString[2]) - $20, (Byte(vString[3]) - $20 + 1)*(vFinalByte - 1)]);
        vSkipChar := vFinalByte + 2;
      end;
    end;

    case vCharacterSetType of
      cst94, cstCompleteCode: begin
        for I := 3 to 2 + vFinalByte do
          vCharacterSetDesignation := vCharacterSetDesignation + String(vString[I]);
      end;
      cst96: vCharacterSetDesignation := String(vString[3]);
      cst94Multibyte, cst96Multibyte: vCharacterSetDesignation := String(vString[4]);
    end;
    vItem := nil;
    if Assigned(FSetListTable) then
      vItem := TsgSetList(FSetListTable).GetISO2375Item(vCharacterSetType,
        GetCodeFromString(vCharacterSetDesignation));
    if Assigned(vItem) then
    begin
      FReader.FCodePage := vItem.CodePage;
{$IFDEF SGDEL_2009}
      case FReader.FCodePage of
        1200: begin
          FReader.IsUTF16String := True;
          vBytes := BytesOf(sgRawByteString(vString));
          FMetaFileName := TEncoding.BigEndianUnicode.GetString(vBytes, vSkipChar, Length(vBytes) - vSkipChar);
        end;
        65001: begin
          try
            vBytes := BytesOf(sgRawByteString(vString));
            FMetaFileName := TEncoding.UTF8.GetString(vBytes, vSkipChar, Length(vBytes) - vSkipChar);
          except
          end;
        end
        else
{$ENDIF}
          vString := Copy(vString, vSkipChar + 1, MaxInt);
          FMetaFileName := ConvertToWideString(vString, vItem.CodePage);
{$IFDEF SGDEL_2009}
      end;
{$ENDIF}
    end
    else
      FMetaFileName := String(vString);
  end
  else
    FMetaFileName := FReader.ReadSF;
end;

procedure TsgCGMImage.ReadPolyBezier;
var
  vPolyBezier: TsgDXFLWPolyline;
  vContinuityIndicator, I: Integer;
  vControlPointsList: TFPointList;
  vIsShortForm: Boolean;
  vGenerator: TsgGeneratorShapeEdge;
begin
  vControlPointsList := TFPointList.Create;
  try
    vControlPointsList.Capacity := 32;
    vContinuityIndicator := FReader.ReadIX;
    repeat
      vIsShortForm := (vContinuityIndicator = 2) and
        (vControlPointsList.Count > 0);
      if vIsShortForm then
        vControlPointsList.Add(vControlPointsList.Last);
      I := Integer(vIsShortForm);
      while not FReader.IsTagEOF and (I <= 3) do
      begin
        vControlPointsList.Add(FReader.ReadP);
        Inc(I);
      end;
    until FReader.IsTagEOF;

    if vControlPointsList.Count > 0 then
    begin
      vPolyBezier := TsgDXFLWPolyline.Create;
      try
        vGenerator := TsgGeneratorShapeEdge.Create;
        try
          vGenerator.NumberSplinePart := Converter.NumberOfPartsInSpline;
          vGenerator.QualityApproximation := 4;
          TsgDXFPolylineAccess(vPolyBezier).List.Capacity :=
            (vControlPointsList.Count div 4) * Integer(vGenerator.NumberSplinePart);
          vGenerator.SetListAndProc(TsgDXFPolylineAccess(vPolyBezier).List,
            @AddVertexList);
          for I := 0 to (vControlPointsList.Count div 4) - 1 do
          begin
            vGenerator.CreateCubicSpline(vControlPointsList[I * 4],
              vControlPointsList[I * 4 + 1], vControlPointsList[I * 4 + 2],
              vControlPointsList[I * 4 +3]);
          end;
        finally
          vGenerator.Free;
        end;
      finally
        vPolyBezier.Closed := False;
        TypeEntity := cgmPolyBezier;

        if vPolyBezier.Count > 1 then
          SetRegionParam(vPolyBezier.Vertexes[0].Point,
            vPolyBezier.Vertexes[vPolyBezier.Count - 1].Point);

        AddEntities(vPolyBezier, nil);
      end;
    end;

    {
    if vControlPointsList.Count > 0 then
    begin
      vPolyBezier := TsgDXFSpline.Create;
      try
        MakePolyBezier(vPolyBezier, vControlPointsList);
      finally
        TypeEntity := cgmPolyBezier;
        AddEntities(vPolyBezier, nil);
      end;
    end;
    }
  finally
    FreeAndNil(vControlPointsList);
  end;
end;

procedure TsgCGMImage.ReadPolyline(const ATypeEntity: TsgCgmTypeEntity);
var
  vPoints: TFPointList;
  vEntity: TsgDXFEntity;
begin
  vPoints := TFPointList.Create;
  try
    while not FReader.IsTagEOF do
      vPoints.Add(FReader.ReadP);
    if vPoints.Count > 0 then
    begin
      if (vPoints.Count = 2) and (ATypeEntity <> cgmPolygone) then
      begin
        vEntity := TsgDXFLine.Create;
        TsgDXFLine(vEntity).Point := vPoints[0];
        TsgDXFLine(vEntity).Point1 := vPoints[1];
      end
      else
      begin
        vEntity := TsgDXFLWPolyline.Create;
        AddVertexesInPolyline(TsgDXFLWPolyline(vEntity), vPoints);
        TsgDXFLWPolyline(vEntity).Closed := ATypeEntity = cgmPolygone;
      end;
      TypeEntity := ATypeEntity;
      SetRegionParam(vPoints.First, vPoints.Last);
      AddEntities(vEntity, nil);
    end;
  finally
    vPoints.Free;
  end;
end;

procedure TsgCGMImage.ReadPolyMarker;
var
  vPoint: TFPoint;
  vList: TsgObjectList;
  vSize, vMax: Double;
  vMarker: TsgMarkerPropery;

  function AddLine(const AAngle, ASize: Double): TsgDXFLine;
  begin
    Result := TsgDXFLine.Create;
    vList.Add(Result);
    Result.Point := AddFPoint(vPoint, GetPointOnCircle(cnstFPointZero,
      ASize, AAngle));
    Result.Point1 := AddFPoint(vPoint, GetPointOnCircle(cnstFPointZero,
      -ASize, AAngle));
  end;

  procedure MakePlus;
  begin
    AddLine(0, vSize);
    AddLine(90, vSize);
  end;

  procedure MakeCircle;
  var
    vCircle: TsgDXFCircle;
  begin
    vCircle := TsgDXFCircle.Create;
    vList.Add(vCircle);
    vCircle.Point := vPoint;
    vCircle.Radius := vSize;
  end;

  procedure MakeCross;
  begin
    AddLine(45, vSize);
    AddLine(135, vSize);
  end;

  procedure MakeStart;//asterisk
  begin
    AddLine(30, vSize);
    AddLine(-30, vSize);
    AddLine(90, vSize);
  end;

begin
  vMarker := FReader.Container.Context.Marker;
  vSize := vMarker.Width * 0.5;
  if FReader.Container.Precision.MarkerWidthSpecMode = wsmScaled then
  begin
    vMax := Max(FReader.Container.Precision.VDCExtents.Right -
      FReader.Container.Precision.VDCExtents.Left,
      FReader.Container.Precision.VDCExtents.Bottom -
      FReader.Container.Precision.VDCExtents.Top);
    if not sgIsZero(vMax) then
      vSize := vSize /  vMax;
  end;

  vList := TsgObjectList.Create;
  try
    while not FReader.IsTagEOF do
    begin
      vPoint := FReader.ReadP;
      case TsgCgmMarkerType(vMarker.EType) of
        mtDot:     MakeCircle;
        mtPlus:    MakePlus;
        mtStar:    MakeStart;
        mtCircle:  MakeCircle;
        mtCross:   MakeCross;
      end;
    end;
  finally
    TypeEntity := cgmMarker;
    AddEntities(nil, vList);
    FreeAndNil(vList);
  end;
end;

procedure TsgCGMImage.ReadPolygonSet;
var
  vPolyline: TsgDXFLWPolyline;
  vPolylines, vInvisibles: TsgObjectList;
  I, vVertexFlags: Integer;
  vVertex: TFPoint;
  vFirstVertex: TsgDXFVertex;
begin
  vPolylines := TsgObjectList.Create;
  vInvisibles := TsgObjectList.Create;
  try
    vPolyline := nil;
    vFirstVertex := nil;
    while not FReader.IsTagEOF do
    begin
      vVertex := FReader.ReadP;
      vVertexFlags := FReader.ReadVisibility;
      if vPolyline = nil then
      begin
        vPolyline := TsgDXFLWPolyline.Create;
        vPolylines.Add(vPolyline);
        vPolyline.Visibility := Boolean(vVertexFlags and 1);
        vFirstVertex := AddVertexInPolyline(vPolyline, vVertex);
      end
      else
      begin
        AddVertexInPolyline(vPolyline, vVertex);
        if (vVertexFlags and 1 <> 0) xor vPolyline.Visibility then
        begin
          vPolyline := TsgDXFLWPolyline.Create;
          vPolylines.Add(vPolyline);
          vPolyline.Visibility := Boolean(vVertexFlags and 1);
          AddVertexInPolyline(vPolyline, vVertex);
        end;
        if vVertexFlags and 2 <> 0 then
        begin
          if vFirstVertex <> nil then
            AddVertexInPolyline(vPolyline, vFirstVertex.Point);
          vPolyline := nil;
          vFirstVertex := nil;
        end;
      end;
    end;

    vInvisibles.Capacity := vPolylines.Count;
    for I := 0 to vPolylines.Count - 1 do
      if not TsgDXFEntity(vPolylines[I]).Visibility then
        vInvisibles.Add(vPolylines[I]);

    if vPolylines.Count > 0 then
    begin
      TypeEntity := cgmPolygone;
      AddEntities(nil, vPolylines);
    end;

    for I := 0 to vInvisibles.Count - 1 do
      TsgDXFEntity(vInvisibles[I]).Visibility := False;
  finally
    FreeAndNil(vPolylines);
    FreeAndNil(vInvisibles);
  end;
end;

procedure TsgCGMImage.ReadBitonalTile;
var
  vTileArray: TcgmTileArray;
  vOffset: TPoint;
   I, J: Integer;
begin
    vTileArray := TcgmTileArray(FTileArray);
    if (vTileArray <> nil) and (vTileArray.Decompressor <> nil) then
    begin
      vTileArray.Decompressor.Compression := ReadCompressiontType;
      vTileArray.RowPadding := Cardinal(FReader.ReadI);
      vTileArray.Decompressor.Background := FReader.ReadCO;
      vTileArray.Decompressor.Foreground := FReader.ReadCO;

      J := vTileArray.CurrentNumberTile div vTileArray.NumberOfTiles.X;
      I := vTileArray.CurrentNumberTile mod vTileArray.NumberOfTiles.X;
      Inc(vTileArray.CurrentNumberTile);
      vOffset := Point(I * (vTileArray.NumberOfCells.X), J *
        vTileArray.NumberOfCells.Y);
      (*  By 1805411c2.cgm
      if (vTileArray.NumberOfCells.X * vTileArray.NumberOfTiles.X) <> vTileArray.ImageNumberOfCells.X then
        vTileArray.Decompressor.ApplyPadding(vTileArray.RowPadding)
      else
        vTileArray.Decompressor.ApplyPadding(0);
      *)
      if ((vTileArray.NumberOfTiles.X * vTileArray.NumberOfTiles.Y) = 1) and
        (vTileArray.NumberOfCells.X = vTileArray.ImageNumberOfCells.X) and
        (vTileArray.NumberOfCells.Y = vTileArray.ImageNumberOfCells.Y) then
        vTileArray.RowPadding := 0;
      vTileArray.Decompressor.ApplyPadding(vTileArray.RowPadding);
      vTileArray.Decompressor.Execute(vOffset, FReader, True);
    end;
end;

procedure TsgCGMImage.ReadRectangle;
begin
  if (FIndexFunction = 88) and (FMetafileElementListSet = elDrawing) then
    ReadPolygonSet
  else
    AddRectangle(FReader.ReadP, FReader.ReadP);
end;

procedure TsgCGMImage.ReadRestrTextType;
begin
  ReadUnknowBlock;//for future version
end;

procedure TsgCGMImage.ReadTableColor;
var
  vStartIndex, I: Integer;
  vColor, vForeGround: Integer;
  vContainer: TsgContainer;
begin
  vContainer := FReader.Container;
  vStartIndex := FReader.ReadCI.Index;
  if vStartIndex >= vContainer.TableColor.Count then
  begin
    vForeGround := FReader.Container.ForegroundColour.Color;
    vContainer.TableColor.Capacity := vStartIndex + 1;
    I := vContainer.TableColor.Count;
    while I <= vStartIndex do
    begin
      vContainer.TableColor.Add(vForeGround);
      Inc(I);
    end;
  end;
  while not FReader.IsTagEOF do
  begin
    vColor := FReader.ConvertColorCGMToColor(FReader.ReadCD).Color;
    if vStartIndex < vContainer.TableColor.Count then
      vContainer.TableColor[vStartIndex] := vColor
    else
      vContainer.TableColor.Add(vColor);
    Inc(vStartIndex);
  end;
end;

procedure TsgCGMImage.ReadText(const AReadExtent: Boolean);
var
  vReadText : string;
  vText: TsgDXFText;
  vPoint: TFPoint;
  vFlags: Word;
  //vExtent: TF2DPoint;
  vTextProperty: TsgTextProperty;
begin
  vTextProperty := FReader.Container.Context.Text;
  FDeltaWidth := 0;
  FDeltaHeight := 0;
  if AReadExtent then
  begin
    //vExtent.X := FReader.ReadVDC;
    //vExtent.Y := FReader.ReadVDC;
    FDeltaWidth :=  FReader.ReadVDC;
    FDeltaHeight := FReader.ReadVDC;
  end;
  vPoint := FReader.ReadP;
  vFlags := FReader.ReadTextPiece;
  vReadText := FReader.ReadSF(False);

  CheckString(vReadText);
  vText := TsgDXFText.Create;
  vText.Point := vPoint;
  vText.Style := GetTextFontStyle(vTextProperty.TextFontIndex);
  vText.Height := CharacterHeight;

  if vTextProperty.CharacterExpansionFactor > 0 then
  begin
{$IFDEF SG_CORTONA_VIEWER}
    if SameText(FProfileId, cnstWebCgm) then
    begin
      vText.Scale := vTextProperty.CharacterExpansionFactor*
        (DistanceFVector(FReader.Container.Context.Text.CharacterOrientationBase)/
        DistanceFVector(FReader.Container.Context.Text.CharacterOrientationUp));
    end
    else
      vText.Scale := vTextProperty.CharacterExpansionFactor;
{$ELSE}
    vText.Scale := vTextProperty.CharacterExpansionFactor;
{$ENDIF}
    if vText.Scale < 0 then
      vText.Scale := vText.Scale + 1;//change in future version
  end;

  if vText.VAlign = 2 then
    vText.Point := MakeFPoint(vText.Point.X, vText.Point.Y - vText.Height, 0);

  vText.Point1 := vText.Point;
  vText.ColorCAD := GetCADColour(vTextProperty.Colour);
  vText.Text := vReadText;
  //ApplyCharOrientation(vText);

  case vTextProperty.HorizontalAlignment of
    thaNormal, thaLeft: vText.HAlign := 0;
    thaCentre:          vText.HAlign := 1;
  else
    vText.HAlign := 2;
  end;
  case vTextProperty.VerticalAlignment of
    tvaCap: vText.VAlign := 3;
    tvaTop: vText.VAlign := 3;
    tvaBottom:      vText.VAlign := 1;
    tvaHalf:        vText.VAlign := 2;
  else
    vText.VAlign := 0;
  end;

  if vFlags > 0 then
  begin
    if FRestText <> nil then
      AddRestText(cgmRestrictedText);
    if AReadExtent then
      TypeEntity := cgmRestrictedText
    else
      TypeEntity := cgmText;
    ApplyCharOrientation(vText);
    AddEntities(vText, nil);
  end
  else
  begin
    ApplyCharOrientation(vText);
    if FRestText = nil then
      FRestText := vText
    else
    begin
      AppendToMultyText(vText.Text);
      vText.Free;
    end;
  end;
end;

procedure TsgCGMImage.ReadRestrictedText;
begin
  ReadText(True);
end;

procedure TsgCGMImage.ReadTextAlign;
begin
  FReader.Container.Context.Text.HorizontalAlignment :=
    FReader.ReadTextHorizontalAlignment;
  FReader.Container.Context.Text.VerticalAlignment :=
    FReader.ReadTextVerticalAlignment;
  FReader.Container.Context.Text.ContinuousHorizontalAlignment := FReader.ReadR;
  FReader.Container.Context.Text.ContinuousVerticalAlignment := FReader.ReadR;
end;

procedure TsgCGMImage.ReadTextPath;
begin
  FReader.Container.Context.Text.TextPath := FReader.ReadTextPath;
end;

procedure TsgCGMImage.ReadTextScaleX;
begin
  FReader.Container.Context.Text.CharacterExpansionFactor := FReader.ReadR;
end;

procedure TsgCGMImage.ReadTextSpacing;
begin
  FReader.Container.Context.Text.CharacterSpacing := FReader.ReadR;
end;

procedure TsgCGMImage.ReadTile;
var
  vTileArray: TcgmTileArray;
  I, J: Integer;
  vOffset: TPoint;
  vPrecision: Integer;
  vPixelFormat: TPixelFormat;
begin
  vTileArray := TcgmTileArray(FTileArray);
  if (vTileArray <> nil) and (vTileArray.Decompressor <> nil) then
  begin
    vTileArray.Decompressor.Compression := ReadCompressiontType;
    vTileArray.RowPadding := Cardinal(FReader.ReadI);
    FReader.SaveColorParams;
    try
      // cell colour precision <=> local colour precision

      vPrecision := FReader.ReadCO_Precision;

      if vTileArray.Decompressor.Compression = ctRLE then
      begin
        FReader.SetRangeColourByPrecision;
        vPixelFormat := GetPixelFormat(vPrecision);
        if vPixelFormat <> pfCustom then
        begin
          vTileArray.Decompressor.ProcReadColor := FReader.ReadColor;
          vTileArray.Decompressor.SetPixelFormat(vPixelFormat);
        end;
      end;

      J := vTileArray.CurrentNumberTile div vTileArray.NumberOfTiles.X;
      I := vTileArray.CurrentNumberTile mod vTileArray.NumberOfTiles.X;
      Inc(vTileArray.CurrentNumberTile);
      vOffset := Point(I * (vTileArray.NumberOfCells.X), J *
        vTileArray.NumberOfCells.Y);
      (*  By 1805411c2.cgm
      if (vTileArray.NumberOfCells.X * vTileArray.NumberOfTiles.X) <> vTileArray.ImageNumberOfCells.X then
        vTileArray.Decompressor.ApplyPadding(vTileArray.RowPadding)
      else
        vTileArray.Decompressor.ApplyPadding(0);
      *)
      vTileArray.Decompressor.Execute(vOffset, FReader, True);
    finally
      FReader.RestoreColorParams;
    end;
  end;
end;

procedure TsgCGMImage.ReadUnknowBlock;
//var
//  vIdentifier: Integer;
begin
//  vIdentifier := FReader.__Read_I;
// For future version
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCGMImage.ReadVDCExtent;
var
  vVDCExtent: TFRect;
begin
  vVDCExtent.TopLeft := FReader.ReadP;
  vVDCExtent.BottomRight := FReader.ReadP;
  FReader.Container.Precision.VDCExtents := vVDCExtent;
end;

procedure TsgCGMImage.ReadWidth(const AProperty: TsgMarkerPropery);
begin
  AProperty.Width := FReader.ReadSS(AProperty.SpecMode);
end;

procedure TsgCGMImage.ReadMarkerSize;
begin
  FReader.Container.Context.Marker.Width :=
    FReader.ReadSS(FReader.Container.Context.Marker.SpecMode);
end;

procedure TsgCGMImage.ReadMetafileDefaultsReplacement;
var
  vBuffer: Word;
  vReader, vReaderTmp: TsgCGMReader;
begin
  vReader := TsgCGMClassOfReader(FReader.ClassType).Create;
  try
    vReader.AssignedParams(FReader);
    while not FReader.IsTagEOF do
    begin
      FReader.Read(vBuffer, SizeOf(vBuffer));
      vReader.Write(vBuffer, SizeOf(vBuffer));
    end;
    vReader.Position := 0;
    vReaderTmp := FReader;
    try
      FReader := vReader;
      AnalizePictureBody;
    finally
      FReader := vReaderTmp;
    end;
    FReader.AssignedParams(vReader);
  finally
    vReader.Free;
  end;
end;

procedure TsgCGMImage.SetCharacterHeight(const Value: Double);
begin
  FReader.Container.Context.Text.CharacterHeight := Value;
end;

procedure TsgCGMImage.SetCharOrientations(const AParams: array of Double);
begin
  FReader.Container.Context.Text.CharacterOrientationUp :=
    MakeFPoint(AParams[0], AParams[1], 0);
  FReader.Container.Context.Text.CharacterOrientationBase :=
    MakeFPoint(AParams[2], AParams[3], 0);
end;

procedure TsgCGMImage.SetMetafileVersion(const AVersion: Integer);
begin
  if Assigned(FReader) then
    FReader.FMetafileVersion := AVersion;
end;

procedure TsgCGMImage.SetRegionParam(const AFirstPoint, ALastPoint: TFPoint);
begin
  if FRegionParametrs <> nil then
  begin
    FRegionParametrs^.FirstPointElement := AFirstPoint;
    FRegionParametrs^.LastPointElement := ALastPoint;
    if not FRegionParametrs^.IsInitStartPoint then
    begin
      FRegionParametrs^.StartPointRegion := AFirstPoint;
      FRegionParametrs^.IsInitStartPoint := True;
    end;
  end;
end;

procedure TsgCGMImage.SetTypeEntity(const Value: TsgCgmTypeEntity);
begin
  FTypeEntity := Value;
  if (FCADCurvePolygon <> nil) and (FRegionParametrs <> nil) then
  begin
    if FTypeEntity in cnstFillGroups then
    begin
      NewRegion;
      FRegionParametrs^.IsCloseRegion := True;
    end;
  end;
end;

function TsgCGMImage.SetupPropertyAndLoadEntity(const AEntity: TsgDXFEntity;
  const ATypeEntity: TsgCgmTypeEntity): TsgCgmGroupElements;
var
  vEntProperty: TsgProperty;
  vMarkerProperty: TsgMarkerPropery;
  vLineType: TsgDXFLineType;
  vVisibility: Boolean;
  vHollow: Boolean;
begin
  vVisibility := True;
  vHollow := False;
  Result := GetGroupsElements(ATypeEntity);
  case Result of
    geLine:
      begin
          vEntProperty := FReader.Container.Context.Line;
          if FRegionParametrs <> nil then
          begin
             vEntProperty := FReader.Container.Context.Edge;
             vVisibility := TsgEdgeProperty(vEntProperty).Visibility;
             if not vVisibility then
               vHollow := FReader.Container.Context.Hatch.InteriorStyle in [isHollow];
          end;
      end;
    geMarker:  vEntProperty := FReader.Container.Context.Marker;
    geEdge:
      begin
        vEntProperty := FReader.Container.Context.Edge;
        vVisibility := TsgEdgeProperty(vEntProperty).Visibility;
        if not vVisibility then
          vHollow := FReader.Container.Context.Hatch.InteriorStyle in [isHollow];
      end;
    geText:    vEntProperty := FReader.Container.Context.Text;
  else
    Exit;
  end;
  AEntity.Visible := True;
  if not vHollow then
    AEntity.ColorCAD := GetCADColour(vEntProperty.Colour)
  else
    AEntity.ColorCAD := GetCADColour(FReader.Container.Context.Hatch.Colour);

  case vEntProperty.GroupElement of
    geLine, geEdge:
      begin
        vMarkerProperty := TsgMarkerPropery(vEntProperty);
        if (vMarkerProperty.Width > cnstMaxWidth[vMarkerProperty.SpecMode]) or vHollow then
          AEntity.LineWeight := FReader.Container.Precision.GetDefaultWidth(
            vMarkerProperty)
        else
          begin
            if vMarkerProperty.SpecMode = wsmScaled then
            begin
              if FReader.Container.Precision.ScalingModeFactor <> 0 then
                AEntity.LineWeight := vMarkerProperty.Width / (
                (FReader.Container.Precision.VDCExtents.Right -
                FReader.Container.Precision.VDCExtents.Left)
                 *FReader.Container.Precision.ScalingModeFactor * 2)
              else
                AEntity.LineWeight := vMarkerProperty.Width;
            end
            else
              AEntity.LineWeight := vMarkerProperty.Width;
          end;
        AEntity.Visibility := vVisibility or vHollow;
        TsgDXFPenEntityAccess(AEntity).LineCap :=
          TsgLineProperty(vEntProperty).PenStyle;
        TsgDXFPenEntityAccess(AEntity).LineJoin :=
          TsgLineProperty(vEntProperty).PenStyle;
        if (TsgMarkerPropery(vEntProperty).EType <> 0) and (not vHollow) then
        begin
          vLineType := Converter.LTypeByName(GetLineTypeName(
            Converter, vMarkerProperty.EType));
          AEntity.SetLType(vLineType);
          AEntity.LineTypeScale := GetLineTypeScale;
        end;
      end;
    geText:
      begin
        Converter.Loads(AEntity);
        if AEntity is TsgDXFText then
          TsgDXFText(AEntity).Point := AddFPoint(TsgDXFText(AEntity).Point,
            GetTextPoint(FDeltaWidth,FDeltaHeight,TsgDXFText(AEntity).Point));
      end;
  end;
  Converter.Loads(AEntity);
end;

procedure TsgCGMImage.UpdateProgress;
begin
  if (FReader.Position - FUpdateProgressCount) >= FUpdateProgress then
  begin
    FUpdateProgressCount := FReader.Position;
    DoOnProgress(psRunning, FUpdateProgressCount, FReader.Size);
  end;
end;


procedure TsgCGMImage.PushInsert(const AInsert: TsgDXFInsert);
begin
  if AInsert.Block.IsLoaded then
    AInsert.Block.IsLoaded := False;
  FStackInsert.Push(AInsert);
end;

function TsgCGMImage.PopInsert: TsgDXFInsert;
begin
  Result := TsgDXFInsert(FStackInsert.Pop);
end;

constructor TsgCGMImage.Create;
begin
  inherited Create;
  Converter.InitializeSectionsBegin;
  Converter.UseSHXFonts := False;
  Stretch := False;
  FOnProgress := DoOnProgress;
  InitStandartStyleByText;
{$IFDEF SG_CGM_DEBUG}
  FStackCommands := TList.Create;
  FAPS := TStringList.Create;
{$ENDIF}
  FSism := nil;
  FStackAPS := TsgAPSStackObject.Create;
  FStackInsert := TsgStackObject.Create;
  FCurrentLayerIfGiven := nil;
end;

procedure TsgCGMImage.FixClearHatches;
var
  I: Integer;
  vHatch: TsgCADHatchAccess;
  vLayer: TsgDXFLayer;
  vScale: Double;
  vHPD: PsgHatchPatternData;
begin
  if FClearHatches = nil then Exit;
  begin
    try
      vLayer := nil;
      for I := 0 to FClearHatches.Count - 1 do
      begin
        vHatch := TsgCADHatchAccess(FClearHatches[I]);
        if vHatch.HatchPatternData.Count = 0 then
        begin
          if vLayer = nil then
          begin
            vLayer := Converter.LayerByName(cnstLayerByClearHatches);
            vLayer.Visible := False;
            vLayer.Frozen := True;
          end;
          vHatch.Layer := vLayer;
          vHPD := vHatch.AddPatternData(cnstFPointZero, 0, 1);
          if not IsBadRect(vHatch.Box) then
          begin
            vScale := Max(Abs(vHatch.Box.Right - vHatch.Box.Left),  Abs(vHatch.Box.Top - vHatch.Box.Bottom)) * 2;
            if vScale > 0 then
              vHPD^.Offset := MakeFPoint(vScale, vScale);
          end;
          vHatch.FillStyle := fsCAD;
          Converter.Loads(vHatch);
        end;
      end;
    finally
      FreeAndNil(FClearHatches);
    end;
  end;
end;

destructor TsgCGMImage.Destroy;
begin
  if FStackAPS.Count > 0 then
    FStackAPS.Clear; // The file did not open correctly
  FStackAPS.Free;
  if FStackInsert.Count > 0 then
    FStackInsert.Count := 0; // Removal is not required
  FStackInsert.Free;
  if Assigned(FCurrentLayerIfGiven) then
    FCurrentLayerIfGiven := nil; // The file did not open correctly
  FreeAndNil(FReader);
  FParameters.Free;
{$IFDEF SG_CGM_DEBUG}
  FStackCommands.Free;
  FAPS.Free;
{$ENDIF}
  FreeAndNil(FSism);
  inherited Destroy;
end;

procedure TsgCGMImage.DisposeTile;
begin
  if Assigned(FTileArray) then
    FreeAndNil(TcgmTileArray(FTileArray).Decompressor);
  FreeAndNil(FTileArray);
end;

{$IFDEF SG_CGM_DEBUG}
procedure TsgCGMImage.LoadFromFile(const FileName: string);
begin
  FFileName := ExtractFileName(FileName);
  inherited LoadFromFile(FileName);
end;
{$ENDIF}

procedure TsgCGMImage.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  FError := False;
  {$IFNDEF SG_OPENING_IN_THEADS}
  Loading := Self;
  {$ENDIF}
{$IFDEF SG_CGM_DEBUG}
  gTab := 1;
{$ENDIF}
  CurrentLayout := Layouts[0];
  TsgDXFConverterAccess(Converter).SetLoading(True, nil, DoUpdateEvent);
  Converter.ClearDrawingProp;
  TsgDXFConverterAccess(Converter).LoadStopped := False;
  FCADCurvePolygon := nil;
  FReader := CreateReader(AStream);
  try
    FFontStyles := TsgObjectList.Create;
    try
      FFontStyles.Add(nil);
      if Assigned(FReader) and (FReader.Size > 0) then
      begin
        InitInternalParams;
        DoOnProgress(psStarting, 1, FReader.Size);
        try
          try
            try
              AnalizePictureBody;
            except
              FError := True;
            end;
          finally
            DisposeTile;
            LoadCurvePolygon;
            DisposeAndNil(PsgRegionParametrs(FRegionParametrs));
            {$IFNDEF SG_OPENING_IN_THEADS}
            Loading := nil;
            {$ENDIF}
            LoadInsert(True);
            FixClearHatches;
            TsgObjectList.FreeList(FHatchStyles);
{$IFDEF SG_CGM_DEBUG}
            SaveStackCommand;
{$ENDIF}
          end;
        finally
          DoOnProgress(psEnding, FReader.Position, FReader.Size);
        end;
      end;
    finally
      FFontStyles.Clear(True);
      FreeAndNil(FFontStyles);
      if FFontAlias <> FontAliasTable then
        FreeAndNil(FFontAlias);
      if FSetListTable <> SetListTable then
        FreeAndNil(FSetListTable);
    end;
  finally
    FreeAndNil(FReader);
  end;
  TsgDXFConverterAccess(Converter).SetLoading(False, nil, nil);
  SetDefaultViewPort(Converter);
  GetExtents;
  SetDefaultPlotSettings;
end;

procedure TsgCGMImage.LoadCurvePolygon(const ABasePoint: PFPoint = nil);
begin
  if FCADCurvePolygon <> nil then
  begin
    if (FCADCurvePolygon.EntType = ceHatch) and (ABasePoint <> nil) then
    begin
      if TsgCADHatch(FCADCurvePolygon).HatchPatternData.Count = 0 then
        AddPatternInHatch(TsgCADHatch(FCADCurvePolygon), ABasePoint^);
    end;
    Converter.Loads(FCADCurvePolygon);
    FCADCurvePolygon := nil;
    FBoxByCurvePolygon := cnstBadRect;
  end;
end;

procedure TsgCGMImage.LoadInsert(const AVDC: Boolean);
begin
  if FInsertClip <> nil then
  begin
    Converter.Loads(FInsertClip.Block);
    Converter.Loads(FInsertClip);
    FInsertClip := nil;
    FCurrentInsert := PopInsert;
  end;
  if AVDC and (FInsertVDC <> nil) then
  begin
    Converter.Loads(FInsertVDC.Block);
    Converter.Loads(FInsertVDC);
    FInsertVDC := nil;
    FCurrentInsert := PopInsert;
  end;
end;

procedure TsgCGMImage.StopLoading;
begin
  TsgDXFConverterAccess(Converter).LoadStopped := True;
end;

{ TsgCGMReader }

procedure TsgCGMReader.AlignBytes;
begin
end;

procedure TsgCGMReader.AssignedParams(const AReader: TsgCGMReader);
begin
  FContainerCGMData.Assign(AReader.FContainerCGMData);
  FreeAndNil(FContainerSave);
  if AReader.FContainerSave <> nil then
  begin
    FContainerSave := TsgContainer.Create;
    FContainerSave.Assign(AReader.FContainerSave);
  end;
end;

class function TsgCGMReader.CompareKeyWordsBin(const AValue1,
  AValue2: Pointer): Integer;
var
  vKey1, vKey2: Word;
begin
  vKey1 := KeyWordsBin[PInteger(AValue1)^];
  vKey2 := KeyWordsBin[PInteger(AValue2)^];
  if vKey1 > vKey2 then
    Result := 1
  else
    if vKey1 < vKey2 then
      Result:= -1
    else
      Result := 0;
end;

function TsgCGMReader.ConvertColorCGMToColor(const AColor: TsgCgmColor): TsgColorCAD;
var
  vColor: TsgCgmColor;
begin
  Result.Active := acRGBColor;
  Result.AlbumString := '';  
  case AColor.Model of
    cmlIndexes:
      Result.Color := Container.GetColorByIndex(AColor.Index);
    cmlRGB:
      begin
        vColor := NormalizeColorCgm(AColor);
        Result.Color := ConvertRGBtoColor(vColor.Components[0], vColor.Components[1],
          vColor.Components[2]);
      end;
    cmlCMYK:
      begin
        vColor := NormalizeColorCgm(AColor);
        Result.Color := ConvertCMYKtoColor(vColor.Components[0], vColor.Components[1],
          vColor.Components[2], vColor.Components[3]);
      end;
  else
    Result := cnstColorCADByLayer;
  end;
end;

constructor TsgCGMReader.Create;
begin
  inherited Create;
  FCodePage := CP_ACP;
  FParameters := TsgCGMParameters.Create;
  FContainerCGMData:= TsgContainer.Create;
  InitInternalParams(nil);
end;

destructor TsgCGMReader.Destroy;
begin
  FreeAndNil(FParameters);
  FreeAndNil(FContainerCGMData);
  FreeAndNil(FContainerSave);
  inherited Destroy;
end;

procedure TsgCGMReader.DoNextTag;
begin
  Position := GetNextTagPosition;
end;

procedure TsgCGMReader.DoReadError;
begin
  if Assigned(FOnReadError) then
    FOnReadError(Self);
end;

function TsgCGMReader.GetByte(const AIndex: Integer): Byte;
begin
  Result := PByte(TsgNativeInt(Memory) + AIndex)^;
end;

function TsgCGMReader.GetComponentsCount: Integer;
begin
  if Container.Precision.ColourModel = cmlCMYK then
    Result := 4
  else
    Result := 3;
end;

function TsgCGMReader.GetIsUTF16String: Boolean;
begin
  Result := False;
end;

function TsgCGMReader.GetReaderType: TsgCGMReaderType;
begin
  Result := crtUnknown;
end;

function TsgCGMReader.GetVDCExtKoef: Double;
var
  vWidth, vHeight: Double;
  vVDCExtent: TFRect;
begin
  if Container.Precision.ScalingMode = smMetric then
    Result := Container.Precision.ScalingModeFactor
  else
  begin
    vVDCExtent := Container.Precision.VDCExtents;
    vWidth := (vVDCExtent.Right - vVDCExtent.Left);
    vHeight := (vVDCExtent.Bottom - vVDCExtent.Top);
    if vHeight <> 0 then
      Result := vWidth / vHeight
    else
      Result := 1;
  end;
end;

procedure TsgCGMReader.InitInternalParams(const AStream: TStream);
var
  vPos: Int64;
begin
  if AStream <> nil then
  begin
    vPos := AStream.Position;
    try
      CopyFrom(AStream, AStream.Size - AStream.Position);
    except
      Clear;
    end;
    AStream.Position := vPos;
  end;
  Position := 0;
  ResetDefault;
end;

function TsgCGMReader.IsEndCommand: Boolean;
begin
  Result := IsTagEOF;
end;

function TsgCGMReader.ReadIntVal(const APrecision: TsgCgmIntPrecision): Integer;
begin
  SpecReadBuffer(cnstIntPrecisionSize[APrecision]);
  Result := GetIntValue(APrecision);
end;

function TsgCGMReader.ReadByteCount(const ASize: Integer): Integer;
begin
  Result := ReadIntVal(GetIntPrecisionBySize(ASize));
end;

function TsgCGMReader.ReadColor: Cardinal;
begin
  Result := ReadCO;
end;

function TsgCGMReader.ReadColorLength: Integer;
begin
  Result := ReadBits(GetSizeByIntPrecision(ipWord) * 8);//__Read_E
end;

function TsgCGMReader.ReadRealVal(const APrecision: TsgCgmRealPrecision):
  Double;
begin
  SpecReadBuffer(cnstRealPrecisionSize[APrecision]);
  Result := GetRealValue(APrecision);
end;

procedure TsgCGMReader.ResetDefault;
begin
  Container.Reset(GetReaderType);
end;

procedure TsgCGMReader.SeekUndefined;
begin
end;

procedure TsgCGMReader.SetIsUTF16String(const Value: Boolean);
begin
end;

procedure TsgCGMReader.SetRangeColourByPrecision;
var
  vValue: Cardinal;
begin
  if (Container.Precision.ColourSelectionMode = cmIndexed) then
    Container.Precision.ColourMaximumIndex := (1 shl Container.Precision.ColourIndexPrecision) - 1
  else
  begin
    vValue := Container.Precision.GetMaxComponentValue;
    Container.Precision.ColourValueMin := MakeCgmRgbColor(0, 0, 0);
    Container.Precision.ColourValueMax := MakeCgmRgbColor(vValue, vValue, vValue);
  end;
end;

function TsgCGMReader.MetafileElementList: TsgCgmElementsList;
var
  vPairCount: Integer;
  vMetafileElementList: Integer;
begin
  Result := elDrawing;
  vPairCount := SpecReadMetafileElementListCount;
  while (vPairCount > 0) and (not IsTagEOF) do
  begin
    vMetafileElementList := SpecReadMetafileElementList;
    Dec(vPairCount);
    if (vMetafileElementList >= Integer(Low(TsgCgmElementsList))) and
       (vMetafileElementList <= Integer(High(TsgCgmElementsList))) then
      Result := TsgCgmElementsList(vMetafileElementList);
  end;
end;

function TsgCGMReader.NormalizeColorCgm(const AColor: TsgCgmColor): TsgCgmColor;
const
  cnstRangNormalize = 255;
var
  I: Integer;
  vMin, vMax: TsgCgmColor;
begin
  Result := AColor;
  vMax := Container.Precision.ColourValueMax;
  vMin := Container.Precision.ColourValueMin;
  if vMin.Model = AColor.Model then
  begin
    case AColor.Model of
      cmlRGB, cmlCMYK:
        begin
          for I := 0 to 2 + Integer(AColor.Model = cmlCMYK) do
          begin
            Result.Components[I] := EnsureRange(AColor.Components[I],
              vMin.Components[I], vMax.Components[I]);
            if vMax.Components[I] <> vMin.Components[I] then
            begin
              Result.Components[I] := cnstRangNormalize *
                (Result.Components[I] - vMin.Components[I]) div
                (vMax.Components[I] - vMin.Components[I]);
            end;
          end;
        end;
    end;
  end;
end;

procedure TsgCGMReader.ReadColorModel;
begin
  case SpecReadColourModel of
    1: Container.Precision.ColourModel := cmlRGB;
    2: Container.Precision.ColourModel := cmlCIELAB;
    3: Container.Precision.ColourModel := cmlCIELUV;
    4: Container.Precision.ColourModel := cmlCMYK;
    5: Container.Precision.ColourModel := cmlRGBRelated;
  else
    Container.Precision.ColourModel := cmlReserved;
  end;
end;

function TsgCGMReader.ReadCO_Precision: Integer;
begin
  if Container.Precision.ColourSelectionMode = cmDirect then
  begin
    ReadCCO_Precision;
    Result := Container.Precision.ColourPrecision;
  end
  else
  begin
    ReadCI_Precision;
    Result := Container.Precision.ColourIndexPrecision;
  end;
end;

function TsgCGMReader.ReadDashCap: Integer;
begin
  Result := ReadIX;
end;

procedure TsgCGMReader.ReadColorSelectionMode;
begin
  case SpecReadColourSelectionMode of
    0:  Container.Precision.ColourSelectionMode := cmIndexed;
    1:  Container.Precision.ColourSelectionMode := cmDirect;
  end;
end;

procedure TsgCGMReader.ReadColorValuesExtent;
var
  I: Integer;
  vScaledComponent: TsgCgmColorScaledComponent;
  vColourValueMin, vColourValueMax: TsgCgmColor;
begin
  if Container.Precision.ColourModel in [cmlRGB, cmlCMYK] then
  begin
    vColourValueMin := ReadCD;
    vColourValueMax := ReadCD;
    Container.Precision.ColourValueMin := vColourValueMin;
    Container.Precision.ColourValueMax := vColourValueMax;
  end
  else
  begin
    for I := 0 to 3 do
    begin
      {vScaledComponent.Scale := }ReadR;
      vScaledComponent.Scale := ReadR;
      Container.Precision.ColourComponents[I] := vScaledComponent;
    end;
  end;
end;

procedure TsgCGMReader.ReadScalingMode;
var
  vMode: Integer;
  vFloatTemp: Double;
begin
  vMode := SpecReadScalingMode;
  if (vMode = 0) or (vMode = 1) then
  begin
    Container.Precision.ScalingMode := TsgCgmScalingMode(vMode);
    vFloatTemp := ReadFloatingR;
    if vFloatTemp < 1E-15 then
      vFloatTemp := 1;
    Container.Precision.ScalingModeFactor := vFloatTemp;
  end;
end;

function TsgCGMReader.ReadCCO: Integer;
begin
  case Container.Precision.ColourPrecision of
    1, 2, 4:
      Result := ReadBits(Container.Precision.ColourPrecision);
  else
    Result := ReadIntVal(GetIntPrecisionBySize(
      Container.Precision.ColourPrecision div 8));
  end;
end;

procedure TsgCGMReader.ReadCCO_Precision;
var
  vTempPrecision: Integer;
begin
  vTempPrecision := SpecReadPrecision_I_Size;
  if vTempPrecision <> 0 then
    Container.Precision.ColourPrecision := vTempPrecision;
end;

function TsgCGMReader.ReadCD: TsgCgmColor;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(TsgCgmColor), 0);
  Result.Model := Container.Precision.ColourModel;
  case Result.Model of
    cmlRGB, cmlCMYK:
      begin
        for I := 0 to 2 + Integer(Result.Model = cmlCMYK) do
          Result.Components[I] := ReadCCO;
      end;
  end;
end;

function TsgCGMReader.ReadCI: TsgCgmColor;
begin
  FillChar(Result, SizeOf(TsgCgmColor), 0);
  Result.Model := cmlIndexes;
  case Container.Precision.ColourIndexPrecision of
    1, 2, 4:
      Result.Index := ReadBits(Container.Precision.ColourIndexPrecision);
  else
    Result.Index := ReadIntVal(GetIntPrecisionBySize(
      Container.Precision.ColourIndexPrecision div 8));
  end;
end;

procedure TsgCGMReader.ReadCI_Precision;
var
  vTempPrecision: Integer;
begin
  vTempPrecision := SpecReadPrecision_I_Size;
  if vTempPrecision <> 0 then
    Container.Precision.ColourIndexPrecision := vTempPrecision;
end;

function TsgCGMReader.ReadCloseSpec: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReader.ReadCO: Integer;
begin
  Result := ConvertColorCGMToColor(ReadCgmColor).Color;
end;

function TsgCGMReader.ReadCgmColor: TsgCgmColor;
begin
  if Container.Precision.ColourSelectionMode = cmDirect  then
    Result := ReadCD
  else
    Result := ReadCI;
end;

procedure TsgCGMReader.ReadCharacterCodingAnnouncer;
var
  vIndex: Integer;
begin
  vIndex := SpecReadCharacterCodingAnnouncer;
  if (vIndex >= Integer(Low(TsgCgmCharacterCodingAnnouncer))) and
     (vIndex <= Integer(High(TsgCgmCharacterCodingAnnouncer))) then
    Container.Precision.CharacterCodingAnnouncer :=
      TsgCgmCharacterCodingAnnouncer(vIndex);
end;

procedure TsgCGMReader.ReadCharacterSetList(const ASetListTable: TObject);
var
  vIndex: Integer;
  vCharacterSetDesignation: String;
  vCharacterSetType: TsgCgmCharacterSetType;
  vItem: TsgISO2375;
  vCode: Integer;
begin
  vIndex := 1;
  while not IsTagEOF do
  begin
    if IsWeReadNBytes then
     Break;
    vCode := CP_ACP;
    vCharacterSetType := TsgCgmCharacterSetType(SpecReadCharacterSetType);
    vCharacterSetDesignation := ReadSF;
    vItem := nil;
    if Assigned(ASetListTable) then
     vItem := TsgSetList(ASetListTable).GetISO2375Item(vCharacterSetType,
     GetCodeFromString(vCharacterSetDesignation));
    if Assigned(vItem) then
      vCode := vItem.CodePage;
    Container.Precision.CharacterSetList.AddObject(IntToStr(vIndex), TsgObjectWithField.CreateInt(vCode));
    Inc(vIndex);
  end;
end;

function TsgCGMReader.ReadE: Integer;
begin
  Result := ReadIntVal(ipWord);
end;

function TsgCGMReader.ReadI: Integer;
begin
  Result := ReadIntVal(Container.Precision.IntegerPrecision);
end;

function TsgCGMReader.ReadN: Integer;
begin
  Result := ReadIntVal(Container.Precision.NamePrecision);
end;

function TsgCGMReader.ReadInteriorStyle: TsgCgmInteriorStyle;
begin
  case SpecReadInteriorStyle of
    0: Result := isHollow;
    1: Result := isSolid;
    2: Result := isPattern;
    3: Result := isHatch;
    4: Result := isEmpty;
    5: Result := isPeometricPattern;
    6: Result := isInterpolated;
  else
    Result := isEmpty;
  end;
end;

function TsgCGMReader.ReadIX: Integer;
begin
  Result := ReadIntVal(Container.Precision.IndexPrecision);
end;

function TsgCGMReader.ReadLineCap: Integer;
begin
  Result := ReadIX;
end;

function TsgCGMReader.ReadLineJoin: Integer;
begin
  Result := ReadIX;
end;

function TsgCGMReader.ReadLineType: Integer;
begin
  Result := ReadIX;
end;

function TsgCGMReader.Read_nSF: TStringList;
var
  vStr: string;
begin
  Result := nil;
  while not IsTagEOF do
  begin
    vStr := ReadSF;
    if Length(vStr) > 0 then
    begin
      if Result = nil then
        Result := TStringList.Create;
      Result.Add(vStr);
    end
    else
      Break;
  end;
end;

function TsgCGMReader.ReadP: TFPoint;
begin
  Result.Z := 0;
  Result.X := ReadVDC;
  Result.Y := ReadVDC;
end;

function TsgCGMReader.ReadR: Double;
begin
  Result := ReadRealVal(Container.Precision.RealPrecision);
end;

//
// Add by ISO/IEC 8632-3 (Table 1 Node 10)
//
function TsgCGMReader.ReadFloatingR: Double;
begin
  case Container.Precision.RealPrecision of
    rpSingle: Result := ReadRealVal(rpSingle);
    rpDouble: Result := ReadRealVal(rpDouble);
    else
      Result := ReadRealVal(rpSingle);
  end;
end;

function TsgCGMReader.ReadFixedR: Double;
begin
  case Container.Precision.RealPrecision of
    rpFixed: Result := ReadRealVal(rpFixed);
    rpFixed64: Result := ReadRealVal(rpFixed64);
    else
      Result := ReadRealVal(rpFixed);
  end;
end;

function TsgCGMReader.ReadSS(const AMode: TsgCgmWidthSpecMode): Double;
var
  vScale: Double;
begin
  if AMode = wsmAbsolute then
  begin
    Result := ReadVDC;
    if AMode = wsmScaled then
    begin
      vScale := GetVDCExtKoef;
      if vScale <> 0 then
        Result := Result * vScale;
    end;
  end
  else
    Result := ReadR;
end;

function TsgCGMReader.ReadTextHorizontalAlignment: TsgCgmTextHorizontalAlignment;
begin
  case SpecReadTextHorizontalAlignment of
    0: Result := thaNormal;
    1: Result := thaleft;
    2: Result := thaCentre;
    3: Result := thaRight;
    4: Result := thaContinuous;
  else
    Result := thaNormal;
  end;
end;

function TsgCGMReader.ReadTextPath: Integer;
begin
  Result := SpecReadTextPath;
  if Result > 4 then
    Result := 0;
end;

function TsgCGMReader.ReadTextPiece: Integer;
begin
  Result := SpecReadText_Piece;
  if Result > 1 then
    Result := 1;  
end;

function TsgCGMReader.ReadTextVerticalAlignment: TsgCgmTextVerticalAlignment;
begin
  case SpecReadTextVerticalAlignment of
    0: Result := tvaNormal;
    1: Result := tvaTop;
    2: Result := tvaCap;
    3: Result := tvaHalf;
    4: Result := tvaBase;
    5: Result := tvaBottom;
    6: Result := tvaContinuous;
  else
    Result := tvaNormal    
  end;
end;

function TsgCGMReader.ReadVDC: Double;
begin
  if Container.Precision.VDCType = vdcInteger then
    Result := ReadIntVal(Container.Precision.VDCIntPrecision)
  else
    Result := ReadRealVal(Container.Precision.VDCRealPrecision);
end;

procedure TsgCGMReader.ReadVDC_Type;
begin
  case SpecReadVDCType of
    0:  Container.Precision.VDCType := vdcInteger;
    1:  Container.Precision.VDCType := vdcReal;
  end;
end;

function TsgCGMReader.ReadWidthSpecificationMode: TsgCgmWidthSpecMode;
begin
  case SpecReadWidthSpecificationMode of
    0:  Result := wsmAbsolute;
    1:  Result := wsmScaled;
    2:  Result := wsmFractional;
    3:  Result := wsmMM;
  else
    Result := wsmAbsolute
  end;
end;

procedure TsgCGMReader.RestoreColorParams;
begin
  if FContainerSave <> nil then
  begin
    FContainerCGMData.Assign(FContainerSave);
    FreeAndNil(FContainerSave);
  end;
end;

procedure TsgCGMReader.SaveColorParams;
begin
  if FContainerSave = nil then
    FContainerSave := TsgContainer.Create;
  FContainerSave.Assign(FContainerCGMData);
end;

{ TsgReaderBin }

function TsgCGMReaderBin.AddLocation(const ALength: Integer): Boolean;
begin
  Result := ALength and $8000 <> 0;
  FTagNextPos := Position + (ALength and $7FFF);
  if not (Result or IsEvent(FTagNextPos)) then
    Inc(FTagNextPos);
  FLocations.Add(FTagNextPos);
end;

procedure TsgCGMReaderBin.AlignBytes;
begin
  if not IsEvent(Position) then
    Position := Position + 1;
end;

constructor TsgCGMReaderBin.Create;
begin
  inherited Create;
  FLocations := TsgInt64List.Create;
  FIsUTF16String := False;
end;

destructor TsgCGMReaderBin.Destroy;
begin
  FreeAndNil(FLocations);
  inherited;
end;

function TsgCGMReaderBin.GetIntValue(APrecision: TsgCgmIntPrecision): Integer;
begin
  Result := 0;
  if APrecision = ip0 then
    APrecision := ipWord;
  case APrecision of
    ipByte:     Result := FBuffer[1];
    ipShortInt: Result := ShortInt(FBuffer[1]);
    ipWord:     Result := SwapBytes(PWord(@FBuffer)^);
    ipSmallInt: Result := SmallInt(SwapBytes(PWord(@FBuffer)^));
    ipInt24:    Result := FBuffer[1] shl 16 + FBuffer[2] shl 8 + FBuffer[3]; //ConvertRGBtoColor(FBuffer[1], FBuffer[2], FBuffer[3]);
    ipCardinal: Result := Cardinal(SwapWords(PInteger(@FBuffer)^));
    ipInteger:  Result := SwapWords(PInteger(@FBuffer)^);
  end;
end;

function TsgCGMReaderBin.GetIsUTF16String: Boolean;
begin
  Result := FIsUTF16String;
end;

function TsgCGMReaderBin.GetNextLocation: Integer;
begin
  Result := FLocations[FLocation];
end;

function TsgCGMReaderBin.GetNextTagPosition: Integer;
begin
  Result := FTagNextPos;
end;

function TsgCGMReaderBin.GetReaderType: TsgCGMReaderType;
begin
  Result := crtBin;
end;

function TsgCGMReaderBin.GetRealValue(APrecision: TsgCgmRealPrecision): Double;
var
  vSI: Integer;
  vUI: Cardinal;
  vBuffer: Int64Rec;
  vTempSingle: DWord;
begin
  Result := 0;
  if APrecision = rp0 then
    APrecision := rpSingle;
  case APrecision of
    rpSingle:
      begin
        vTempSingle := SwapWords(PInteger(@FBuffer)^);
        Result := PSingle(@vTempSingle)^;
      end;
    rpDouble:
      begin
        vBuffer.Lo := SwapWords(PInteger(@FBuffer[1])^);
        vBuffer.Hi := SwapWords(PInteger(@FBuffer[5])^);
        Result := Double(vBuffer);
      end;
    rpFixed:
      begin
        vSI := SmallInt(SwapBytes(PWord(@FBuffer[1])^));
        vUI := SwapBytes(PWord(@FBuffer[3])^);
        Result := vSI + vUI * cnstFract16;
      end;
    rpFixed64:
      begin
        vSI := SwapWords(PInteger(@FBuffer[1])^);
        vUI := Cardinal(SwapWords(PInteger(@FBuffer[5])^));
        Result := vSI + vUI * cnstFract32;
      end;
  end;
end;

function TsgCGMReaderBin.IsTagEOF: Boolean;
begin
  Result := Position >= FTagNextPos;
end;

function TsgCGMReaderBin.IsWeReadNBytes(const ACountByte: Byte = 1): Boolean;
begin
  Result := (Position+ACountByte) >= FTagNextPos;
end;


function TsgCGMReaderBin.NextLocation: Boolean;
begin
  Position := Position + 2;//size of taglength = 2 byte
  Inc(FLocation);
  Result := FLocation < FLocations.Count;
end;

function TsgCGMReaderBin.SpecReadCharacterCodingAnnouncer: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadCharacterSetType: Integer;
begin
  Result := ReadE;
end;

procedure TsgCGMReaderBin.SpecReadBuffer(const ASize: Integer = 0);
var
  I: Integer;
begin
  if Position + ASize <= GetNextLocation then
    FBufferSize := Read(FBuffer, ASize)
  else
  begin
    FBufferSize := 0;
    for I := 1 to ASize do
    begin
      if Position >= GetNextLocation then
      begin
        if not NextLocation then
          Break;
      end;
      Inc(FBufferSize, Read(FBuffer[I], 1));
    end;
  end;  
end;

procedure TsgCGMReaderBin.RestorePosition;
begin
  inherited RestorePosition;
  FLocation := FSaveLocation;
end;

procedure TsgCGMReaderBin.SavePosition;
begin
  inherited SavePosition;
  FSaveLocation := FLocation;
end;

procedure TsgCGMReaderBin.SeekUndefined;
begin
  Position := Position + 2;
end;

procedure TsgCGMReaderBin.SetIsUTF16String(const Value: Boolean);
begin
  if Value <> FIsUTF16String then
  begin
    FIsUTF16String := Value;
  end;
end;

function TsgCGMReaderBin.ReadSDR: Integer;
begin
  Result := ReadIntVal(GetIntPrecisionBySize(1));

  //New version

end;

function TsgCGMReaderBin.ReadDirection: TPoint;
begin
  Result.X := ReadE;
  Result.Y := ReadE;
end;

function TsgCGMReaderBin.ReadTagHeader: Integer;
var
  vTagLength: Word;
  vPosition: UInt64;
begin
  FLocation := 0;
  FLocations.Count := 0;
  FBufferSize := Read(FBuffer, 2);
  FParameters.TwoToThree(FBuffer[1], FBuffer[2]);
  Result := FindIndexOfKeyWordsBin(FParameters.FParameters.FirstTwoParams);
  vTagLength := FParameters.Parameter3;
  if vTagLength >= 31 then
  begin
    vPosition := Position + 2;
    try
      FTagNextPos := Position;
      repeat
        Position := FTagNextPos;
        Read(vTagLength, 2);
      until not AddLocation(SwapBytes(vTagLength));
    finally
      Position := vPosition;
    end;
  end
  else
    AddLocation(vTagLength);
end;

function TsgCGMReaderBin.SpecReadColourModel: Integer;
begin
  Result := ReadIX;
end;

function TsgCGMReaderBin.SpecReadColourSelectionMode: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadInteriorStyle: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadMetafileElementList: Integer;
var
  vValue1, vValue2: Integer;
begin
  Result := -1;
  vValue1 := ReadIX;
  vValue2 := ReadIX;
  if vValue1 = -1 then
    Result := vValue2;
end;

function TsgCGMReaderBin.SpecReadMetafileElementListCount: Integer;
begin
  Result := ReadI;
end;

function TsgCGMReaderBin.SpecReadPrecision_I: TsgCgmIntPrecision;
begin
  case ReadI of
    8:   Result := ipByte;
    16:  Result := ipSmallInt;
    24:  Result := ipInt24;
    32:  Result := ipInteger;
  else
    Result := ip0;
  end;
end;

function TsgCGMReaderBin.SpecReadPrecision_I_Size: Integer;
begin
  Result := ReadI;
end;

function TsgCGMReaderBin.SpecReadPrecision_R: TsgCgmRealPrecision;
var
  vType, vExponent, vFraction: Integer;
begin
  Result := rp0;
  vType := ReadE;
  vExponent := ReadI;
  vFraction := ReadI;
  if vType = 0 then
  begin
    if (vExponent = 9) and (vFraction = 23) then
      Result := rpSingle;
    if (vExponent = 12) and (vFraction = 52) then
      Result := rpDouble;
  end
  else
  begin
    if (vExponent = 16) and (vFraction = 16) then
      Result := rpFixed;
    if (vExponent = 32) and (vFraction = 32) then
      Result := rpFixed64;
  end;
end;

function TsgCGMReaderBin.SpecReadScalingMode: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadTextHorizontalAlignment: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadTextPath: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadText_Piece: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadTextVerticalAlignment: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadVDCType: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.SpecReadWidthSpecificationMode: Integer;
begin
  Result := ReadE;
end;

procedure TsgCGMReaderBin.ReadIX_Precision;
begin
  Container.Precision.IndexPrecision := SpecReadPrecision_I;
end;

procedure TsgCGMReaderBin.ReadI_Precision;
begin
  Container.Precision.IntegerPrecision := SpecReadPrecision_I;
end;

procedure TsgCGMReaderBin.ReadN_Precision;
begin
  Container.Precision.NamePrecision := SpecReadPrecision_I;
end;

function TsgCGMReaderBin.ReadRepresentationMode: Integer;
begin
  Result := ReadE;
end;

procedure TsgCGMReaderBin.ReadR_Precision;
begin
  Container.Precision.RealPrecision := SpecReadPrecision_R;
end;

procedure TsgCGMReaderBin.ReadSF_Precision;
begin
  Container.Context.Text.TextPrecision := TsgTextPrecision(SpecReadPrecision_I);
end;

procedure TsgCGMReaderBin.ReadVDC_I_Precision;
begin
  Container.Precision.VDCIntPrecision := SpecReadPrecision_I;
end;

procedure TsgCGMReaderBin.ReadVDC_R_Precision;
begin
  Container.Precision.VDCRealPrecision := SpecReadPrecision_R;
end;

function TsgCGMReaderBin.ReadVisibility: Integer;
begin
  Result := ReadE;
end;

function TsgCGMReaderBin.ReadSF(const AIsCgmSF: Boolean = True): string;
var
  vRawString: sgRawByteString;
{$IFDEF SGDEL_2009}
  vCodePage: Integer;
  vBytes: TBytes;
  vIndex: Integer;
{$ENDIF}
begin
  vRawString := sgRawByteString(ReadSF_A);
{$IFDEF SGDEL_2009}
  if AIsCgmSF then
  begin
    case FCodePage of
      1200: begin
        vBytes := BytesOf(vRawString);
        Result := TEncoding.BigEndianUnicode.GetString(vBytes, 0, Length(vBytes));
      end;
      65001: begin
        try
          vBytes := BytesOf(vRawString);
          Result := TEncoding.UTF8.GetString(vBytes, 0, Length(vBytes));
        except
        end;
      end
      else  begin
        Result := ConvertToWideString(vRawString, FCodePage);
      end;
    end;
  end
  else
  begin
    vIndex := Container.Context.Text.CharacterSetIndex - 1;
    if (vIndex > -1) and (vIndex < Container.Precision.CharacterSetList.Count) then
    begin
      vCodePage := TsgObjectInt64(Container.Precision.CharacterSetList.Objects[vIndex]).FieldInt;
    end
    else
      vCodePage := CP_ACP;
    case vCodePage of
      1200: begin
       vBytes := BytesOf(vRawString);
       Result := TEncoding.BigEndianUnicode.GetString(vBytes, 0, Length(vBytes));
      end;
      65001: begin
        try
          vBytes := BytesOf(vRawString);
          Result := TEncoding.UTF8.GetString(vBytes, 0, Length(vBytes));
        except
        end;
      end
      else
      begin
        Result := string(vRawString);//ConvertToWideString(vRawString, vCodePage);
      end;
    end;
  end;
{$ELSE}
  Result := vRawString;
{$ENDIF}
end;

function TsgCGMReaderBin.ReadSF_A: AnsiString;
var
  vLength, vLengthCheck, vFlags, I: Integer;
  vSymbol: AnsiChar;
  vContinue: Boolean;
begin
  Result := '';
  vContinue := False;
  repeat
    vLength := ReadIntVal(ipByte);
    if vLength >= 255 then
    begin
      vFlags := ReadIntVal(ipWord);
      vLength := cnstMask15bit and vFlags;
      vContinue := vFlags <> vLength;
    end;
    vLengthCheck := Size - Position;
    if vLength > vLengthCheck then
    begin
      vLength := vLengthCheck;
      vContinue := False;
    end;
    Result := '';
    I := 0;
    while I < vLength do
    begin
      vSymbol := AnsiChar(ReadIntVal(ipByte));
      Result := Result + vSymbol;
      Inc(I);
    end;
  until not vContinue;
end;

{ TsgCGMReaderStr }

procedure TsgCGMReaderStr.BeginReadBits;
var
  I, J, vCountByteInItem: Integer;
  vBuffer: string;
  vBytes: TStringList;
begin
  vCountByteInItem := Length(FBuffer[FBufferIndex + 1]) div 2;
  if vCountByteInItem >= 1 then
  begin
    vBytes := TStringList.Create;
    try
      vBytes.Capacity := FBuffer.Count * vCountByteInItem;
      for I := 0 to FBufferIndex do
        vBytes.Add(FBuffer[I]);
      for I := FBufferIndex + 1 to FBuffer.Count - 1 do
      begin
        vBuffer := FBuffer[I];
        for J := 1 to vCountByteInItem do
          vBytes.Add(Copy(vBuffer, J * 2 - 1, 2));
      end;
      SwapObjects(TObject(vBytes), TObject(FBuffer));
    finally
      vBytes.Free;
    end;
  end
  else
    Assert(vCountByteInItem < 1);
  inherited BeginReadBits;
end;

constructor TsgCGMReaderStr.Create;
begin
  inherited Create;
  FBuffer := TStringList.Create;
end;

destructor TsgCGMReaderStr.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TsgCGMReaderStr.EndReadBits;
begin
  inherited EndReadBits;
end;

function TsgCGMReaderStr.GetBufferIntValue: string;
var
  vBuffer: string;
  vPosEnd, vPosStart: Integer;
begin
  Result := '';
  vBuffer := GetBufferValue;
  vPosStart := 1;
  vPosEnd := Length(vBuffer);
  while (vPosStart <= vPosEnd) and not (IsDigit(vBuffer[vPosStart]) or (vBuffer[vPosStart] = '-'))do
    Inc(vPosStart);
  while (vPosEnd >= vPosStart) and (not IsDigit(vBuffer[vPosEnd])) do
    Dec(vPosEnd);
  if vPosStart <= vPosEnd then
    Result := Copy(vBuffer, vPosStart, vPosEnd - vPosStart + 1);
end;

function TsgCGMReaderStr.GetBufferValue: string;
begin
 if FBufferIndex < FBuffer.Count then
   Result := FBuffer[FBufferIndex]
 else
   Result := '';
end;

function TsgCGMReaderStr.GetIntValue(APrecision: TsgCgmIntPrecision): Integer;
begin
  Result := StrToIntDef(GetBufferIntValue, 0);
end;

function TsgCGMReaderStr.GetNextTagPosition: Integer;
begin
  Result := Position;
end;

function TsgCGMReaderStr.GetReaderType: TsgCGMReaderType;
begin
  Result := crtTxt;
end;

function TsgCGMReaderStr.GetRealValue(APrecision: TsgCgmRealPrecision): Double;
begin
  Result := StrToDouble(GetBufferIntValue, '.');
end;

function TsgCGMReaderStr.IsTagEOF: Boolean;
begin
  Result := (FBufferIndex + 1) >= FBuffer.Count;
end;

function TsgCGMReaderStr.IsWeReadNBytes(const ACountByte: Byte = 1): Boolean;
begin
  Result := (FBufferIndex + 1 + ACountByte)  >= FBuffer.Count;
end;

procedure TsgCGMReaderStr.SavePosition;
begin
  inherited SavePosition;
  FBufferIndexSave := FBufferIndex;
end;

procedure TsgCGMReaderStr.SpecReadBuffer(const ASize: Integer = 0);
begin
  Inc(FBufferIndex);
end;

function TsgCGMReaderStr.ReadIndexValue(const AValues: array of string): Integer;
begin
  SpecReadBuffer;
  Result := IndexOfStrings(UpperCase(GetBufferValue), AValues);
end;

function TsgCGMReaderStr.ReadSpace: Integer;
var
  vBuffer: AnsiChar;
begin
  Result := 0;
  while IsSpace(Char(Bytes[Position])) and
    (Read(vBuffer, SizeOf(vBuffer)) = SizeOf(vBuffer)) do
    Inc(Result);
end;

procedure TsgCGMReaderStr.ReadTag;
var
  vBuffer: Char;
  vBufferValue: string;

  function ReadStringInChar(const ASeporator: Char): string;
  var
    vReadBuffer: Char;
  begin
    Result := '';
    while (ReadChar(vReadBuffer) and (vReadBuffer <> ASeporator)) do
      Result := Result + vReadBuffer;
  end;

  procedure AddInBuffer;
  var
    I, vLength: Integer;
    vValue: string;
  begin
    if Length(vBufferValue) > 0 then
    begin
      repeat
        vValue := '';
        I := 1;
        vLength := Length(vBufferValue);
        while I <= vLength do
        begin
          case vBufferValue[I] of
            '(':
              begin
                Delete(vBufferValue, 1, I);
                vLength := Length(vBufferValue);
                I := 1;
              end;
            ')':
              begin
                vValue := Copy(vBufferValue, 1, I - 1);
                Delete(vBufferValue, 1, I);
                Break;
              end;
          else
            Inc(I);
          end;
        end;
        if Length(vValue) = 0 then
        begin
          vValue := vBufferValue;
          vBufferValue := '';
        end;
        FBuffer.Add(vValue);
      until Length(vBufferValue) = 0;
    end;
  end;

begin
  FBufferIndex := 0;
  FBuffer.Clear;
  repeat
    vBuffer := ';';
    vBufferValue := '';
    if Position < Size then
    begin
      ReadSpace;
      while ReadChar(vBuffer) do
      begin
        if IsApostrophe(vBuffer) then//read text
        begin
          AddInBuffer;
          FBuffer.Add(ReadStringInChar(vBuffer));
        end
        else
        begin
          if vBuffer = '%' then//read comment
            ReadStringInChar(vBuffer)
          else
          begin
            if not CharInSet(vBuffer, ['_', '$']) then
            begin
              if CharInSet(vBuffer, [',', ';']) or IsSpace(vBuffer) then
                Break;
              vBufferValue := vBufferValue + vBuffer;
            end;
          end;
        end;
      end;
      AddInBuffer;
    end;
  until (vBuffer = ';');
end;

function TsgCGMReaderStr.SpecReadCharacterCodingAnnouncer: Integer;
begin
  Result := ReadIndexValue(cnstCharacterCodingAnnouncer);
end;

function TsgCGMReaderStr.SpecReadCharacterSetType: Integer;
begin
  Result := ReadIndexValue(cnstCharacterSetList);
end;

function TsgCGMReaderStr.SpecReadColourModel: Integer;
begin
  Result := ReadIndexValue(cnstColourModel);
end;

function TsgCGMReaderStr.SpecReadColourSelectionMode: Integer;
begin
  Result := ReadIndexValue(cnstColourSelectionMode);
end;

function TsgCGMReaderStr.SpecReadInteriorStyle: Integer;
begin
  Result := ReadIndexValue(cnstInteriorStyle);
end;

function TsgCGMReaderStr.SpecReadMetafileElementList: Integer;
begin
  SpecReadBuffer;
  Result := ReadIndexValue(cnstElementsList);
end;

function TsgCGMReaderStr.SpecReadMetafileElementListCount: Integer;
begin
  Result := Integer(High(TsgCgmElementsList)) -
    Integer(Low(TsgCgmElementsList)) + 1;
end;

function TsgCGMReaderStr.SpecReadPrecision_I: TsgCgmIntPrecision;
var
  vMax, vMin: string;
begin
  SpecReadBuffer;
  vMin := GetBufferValue;
  SpecReadBuffer;
  vMax := GetBufferValue;
  Result := GetIntPrecisionByMinMax(vMin, vMax);
end;

function TsgCGMReaderStr.SpecReadPrecision_I_Size: Integer;
begin
  SpecReadBuffer;
  Result := GetIntSizeByMinMax('', GetBufferValue);
end;

function TsgCGMReaderStr.SpecReadPrecision_R: TsgCgmRealPrecision;
var
  vMax, vMin, vDigit: string;
begin
  SpecReadBuffer;
  vMin := GetBufferValue;
  SpecReadBuffer;
  vMax := GetBufferValue;
  SpecReadBuffer;
  vDigit := GetBufferValue;
  Result := GetRealPrecisionByMaxMin(vMin, vMax, vDigit);
end;

function TsgCGMReaderStr.SpecReadScalingMode: Integer;
begin
  Result := ReadIndexValue(cnstScaleMode);
end;

function TsgCGMReaderStr.SpecReadTextHorizontalAlignment: Integer;
begin
  Result := ReadIndexValue(cnstTextHorizontalAlignment);
end;

function TsgCGMReaderStr.SpecReadTextPath: Integer;
begin
  Result := ReadIndexValue(cnstTextPath);
end;

function TsgCGMReaderStr.SpecReadText_Piece: Integer;
begin
  Result := ReadIndexValue(cnstTextPiece);
end;

function TsgCGMReaderStr.SpecReadTextVerticalAlignment: Integer;
begin
  Result := ReadIndexValue(cnstTextVerticalAlignment);
end;

function TsgCGMReaderStr.SpecReadVDCType: Integer;
begin
  Result := ReadIndexValue(cnstVdcTypes);
end;

function TsgCGMReaderStr.SpecReadWidthSpecificationMode: Integer;
begin
  Result := ReadIndexValue(cnstWidthSpecificationMode);
end;

function TsgCGMReaderStr.ReadSDR: Integer;
begin
  Result := 0;
  Inc(FBufferIndex);
end;

function TsgCGMReaderStr.ReadByteCount(const ASize: Integer): Integer;
var
  I: Integer;
  vHexValue: string;
begin
  Result := 0;
  I := 0;
  while I < ASize do
  begin
    // !!! pointer is on the previous item
    Inc(FBufferIndex);
    vHexValue := GetBufferValue;
    if Length(vHexValue) > 0 then
    begin
      Result := (Result shl 8) or StrToIntDef('$' + vHexValue, 0);
    end
    else
      Break;
    Inc(I)
  end;
end;

function TsgCGMReaderStr.ReadTagHeader: Integer;
begin
  ReadTag;
  if FBuffer.Count > 0 then
    Result := IndexOfKeyWordsString(UpperCase(GetBufferValue))
  else
    Result := -1;
end;

function TsgCGMReaderStr.ReadChar(var ABuffer: Char): Boolean;
{$IFDEF UNICODE}
var
  vReadBuffer: AnsiChar;
begin
  ABuffer := #0;
  Result := Read(vReadBuffer, 1) = 1;
  PByte(@ABuffer)^ := Byte(vReadBuffer);
{$ELSE}
begin
  Result := Read(ABuffer, 1) = 1;
{$ENDIF}
end;

function TsgCGMReaderStr.ReadCloseSpec: Integer;
begin
  Result := ReadIndexValue(cnstCloseSpec);
end;

function TsgCGMReaderStr.ReadDashCap: Integer;
begin
  Result := ReadIndexValue(cnstDashCap);
end;

function TsgCGMReaderStr.ReadDirection: TPoint;
begin
  Result.X := ReadE;
  case Result.X of
    0, 90, 180, 270: Result.X := Result.X div 90;
  else
    Result.X := 0;
  end;
  Result.Y := ReadE;
  case Result.Y of
    270: Result.Y := 1;
  else
    Result.Y := 0;
  end;
end;

procedure TsgCGMReaderStr.ReadIX_Precision;
begin
  Container.Precision.IndexPrecision := SpecReadPrecision_I;
end;

procedure TsgCGMReaderStr.ReadI_Precision;
begin
  Container.Precision.FIntegerPrecision := SpecReadPrecision_I;
end;

function TsgCGMReaderStr.ReadLineCap: Integer;
begin
  Result := ReadIndexValue(cnstLineCap);
end;

function TsgCGMReaderStr.ReadLineJoin: Integer;
begin
  Result := ReadIndexValue(cnstLineJoin);
end;

procedure TsgCGMReaderStr.ReadN_Precision;
begin
  Container.Precision.NamePrecision := SpecReadPrecision_I;
end;

function TsgCGMReaderStr.ReadRepresentationMode: Integer;
begin
  Result := 1;//packed representation
end;

procedure TsgCGMReaderStr.ReadR_Precision;
begin
  Container.Precision.RealPrecision := SpecReadPrecision_R;
end;

function TsgCGMReaderStr.ReadSF(const AIsCgmSF: Boolean = True): string;
begin
  Result := '';
  SpecReadBuffer;
  if FBufferIndex < FBuffer.Count then
    Result := FBuffer[FBufferIndex];
end;

function TsgCGMReaderStr.ReadSF_A: AnsiString;
begin
  Result := '';
end;

procedure TsgCGMReaderStr.ReadSF_Precision;
begin
  ReadIndexValue(cnstSF_Precision);
end;

procedure TsgCGMReaderStr.ReadVDC_I_Precision;
begin
  Container.Precision.VDCIntPrecision := SpecReadPrecision_I;
end;

procedure TsgCGMReaderStr.ReadVDC_R_Precision;
begin
  Container.Precision.VDCRealPrecision := SpecReadPrecision_R;
end;

function TsgCGMReaderStr.ReadVisibility: Integer;
begin
  Result := ReadIndexValue(cnstVisibility);
  if Result > 1  then
    Dec(Result, 2);
end;

procedure TsgCGMReaderStr.RestorePosition;
begin
  inherited RestorePosition;
  FBufferIndex := FBufferIndexSave;
end;

{ TsgPrecisions }

procedure TsgPrecisions.Assign(const APrecicsion: TsgPrecisions);
var
  I: Integer;
  vCode: Int64;
  vObInt: TsgObjectInt64;
  vCharacterSetList: TStringList;
begin
  System.Move(APrecicsion.FColourComponents, FColourComponents,
    SizeOf(FColourComponents));
  ClearObjects(FCharacterSetList);
  FreeAndNil(FCharacterSetList);
  if APrecicsion.FCharacterSetList <> nil then
  begin
    vCharacterSetList := CharacterSetList;
    for I := 0 to APrecicsion.FCharacterSetList.Count - 1 do
    begin
      vObInt := TsgObjectInt64(APrecicsion.FCharacterSetList.Objects[I]);
      if Assigned(vObInt) then
        vCode := vObInt.FieldInt64
      else
        vCode := CP_ACP;
      vCharacterSetList.AddObject(APrecicsion.FCharacterSetList[I], TsgObjectInt64.Create(vCode));
    end;
  end;
  FCharacterCodingAnnouncer := APrecicsion.FCharacterCodingAnnouncer;
  FIntegerPrecision         := APrecicsion.FIntegerPrecision;
  FIndexPrecision           := APrecicsion.FIndexPrecision;
  FNamePrecision            := APrecicsion.FNamePrecision;
  FRealPrecision            := APrecicsion.FRealPrecision;
  FColourModel              := APrecicsion.FColourModel;
  FColourSelectionMode      := APrecicsion.FColourSelectionMode;
  FColourPrecision          := APrecicsion.FColourPrecision;
  FColourIndexPrecision     := APrecicsion.FColourIndexPrecision;
  FColourMaximumIndex       := APrecicsion.FColourMaximumIndex;
  FColourValueMax           := APrecicsion.FColourValueMax;
  FColourValueMin           := APrecicsion.FColourValueMin;
  FScalingMode              := APrecicsion.FScalingMode;
  FVDCType                  := APrecicsion.FVDCType;
  FVDCRealPrecision         := APrecicsion.FVDCRealPrecision;
  FVDCExtents               := APrecicsion.FVDCExtents;
  FScalingModeFactor        := APrecicsion.FScalingModeFactor;
  FVDCIntPrecision          := APrecicsion.FVDCIntPrecision;
end;

constructor TsgPrecisions.Create(const AOwner: TsgContainer);
begin
  inherited Create;
  FOwner := AOwner;
  Reset(crtUnknown);
end;

destructor TsgPrecisions.Destroy;
begin
  ClearObjects(FCharacterSetList);
  FreeAndNil(FCharacterSetList);
  inherited Destroy;
end;

function TsgPrecisions.GetCharacterCodingAnnouncer: TsgCgmCharacterCodingAnnouncer;
begin
  Result := FCharacterCodingAnnouncer;
end;

function TsgPrecisions.GetCharacterSetList: TStringList;
begin
  if FCharacterSetList = nil then
    FCharacterSetList := TStringList.Create;
  Result := FCharacterSetList;
end;

function TsgPrecisions.GetColourComponents(
  const AIndex: Integer): TsgCgmColorScaledComponent;
begin
  Result := FColourComponents[AIndex];
end;

function TsgPrecisions.GetDefaultColor: TsgCgmColor;
var
  vComponent: Cardinal;
  vColourSelectionModeSave: TsgCgmColorMode;
begin
  FillChar(Result, SizeOf(TsgCgmColor), 0);
  vColourSelectionModeSave := FColourSelectionMode;
  FColourSelectionMode :=  cmIndexed;
  if FColourSelectionMode = cmIndexed then
  begin
    Result.Model := cmlIndexes;
    Result.Index := 1;
  end
  else
  begin
    vComponent := GetMaxComponentValue;
    Result.Model := cmlRGB;
    Result.Components[0] := vComponent;
    Result.Components[1] := vComponent;
    Result.Components[2] := vComponent;
  end;
  FColourSelectionMode := vColourSelectionModeSave;
end;

function TsgPrecisions.GetDefaultVDCExtents(const AType: TsgCgmVDCType): TFRect;
begin
  if FVDCType = vdcInteger then
    Result := cnstIntegerRect
  else
    Result := cnstRealRect;
end;

function TsgPrecisions.GetDefaultWidth(AProperty: TsgMarkerPropery = nil): Double;
begin
  if AProperty = nil then
    AProperty := FOwner.Context.Line;
  Result := GetWidth(AProperty, FVDCExtents);
end;

function TsgPrecisions.GetEdgeWidthSpecMode: TsgCgmWidthSpecMode;
begin
  Result := FOwner.Context.Edge.SpecMode;
end;

function TsgPrecisions.GetInteriorStyleSpecMode: TsgCgmWidthSpecMode;
begin
  Result := FOwner.Context.Hatch.SpecMode;
end;

function TsgPrecisions.GetLineWidthSpecMode: TsgCgmWidthSpecMode;
begin
  Result := FOwner.Context.Line.SpecMode;
end;

function TsgPrecisions.GetMarkerWidthSpecMode: TsgCgmWidthSpecMode;
begin
  Result := FOwner.Context.Marker.SpecMode;
end;

function TsgPrecisions.GetMaxComponentValue: Cardinal;
begin
  Result := (1 shl FColourPrecision) - 1;
end;

procedure TsgPrecisions.Reset(AReaderType: TsgCGMReaderType);
var
  vComponent: Cardinal;
begin
  CharacterCodingAnnouncer := ccaBasic7;
  IntegerPrecision := ipSmallInt;
  IndexPrecision := ipSmallInt;
  NamePrecision := ipWord;
  ScalingMode := smAbstract;
  VDCType := vdcInteger;
  VDCIntPrecision := ipSmallInt;
  VDCRealPrecision := rpFixed;
  ColourModel := cmlRGB;
  ColourSelectionMode := cmIndexed;
  ColourPrecision := 8;
  ColourIndexPrecision := 8;
  ColourMaximumIndex := 63;
  ColourValueMin := MakeCgmRgbColor(0, 0, 0);
  vComponent := GetMaxComponentValue;
  ColourValueMax := MakeCgmRgbColor(vComponent, vComponent, vComponent);
  // processing encoding dependent
  case AReaderType of
    crtBin: RealPrecision := rpFixed
  end;
end;

procedure TsgPrecisions.SetCharacterCodingAnnouncer(
  const Value: TsgCgmCharacterCodingAnnouncer);
begin
  FCharacterCodingAnnouncer := Value;
end;

procedure TsgPrecisions.SetColourComponents(const AIndex: Integer;
  const Value: TsgCgmColorScaledComponent);
begin
  FColourComponents[AIndex] := Value;
end;

procedure TsgPrecisions.SetColourIndexPrecision(const Value: Integer);
begin
  FColourIndexPrecision := Value;
end;

procedure TsgPrecisions.SetColourMaximumIndex(const Value: Integer);
begin
  FColourMaximumIndex := Value;
end;

procedure TsgPrecisions.SetColourModel(const Value: TsgCgmColorModel);
begin
  FColourModel := Value;
  case FColourModel of
    cmlRGB, cmlCMYK:
      begin
        FColourValueMin := MakeCgmRgbColor(0, 0, 0);
        FColourValueMax := MakeCgmRgbColor(MAXDWORD, MAXDWORD, MAXDWORD);
      end;
  else
    FillChar(FColourComponents, SizeOf(FColourComponents), 0);
  end;
end;

procedure TsgPrecisions.SetColourPrecision(const Value: Integer);
begin
  FColourPrecision := Value;
end;

procedure TsgPrecisions.SetColourSelectionMode(const Value: TsgCgmColorMode);
var
  vDefaultColor: TsgCgmColor;
begin
  FColourSelectionMode := Value;
  vDefaultColor := GetDefaultColor;
  if not FOwner.Context.Line.IsInitValue(pvColour) then
    FOwner.Context.Line.FColour := vDefaultColor;

  if not FOwner.Context.Edge.IsInitValue(pvColour) then
    FOwner.Context.Edge.FColour := vDefaultColor;

  if not FOwner.Context.Marker.IsInitValue(pvColour) then
    FOwner.Context.Marker.FColour := vDefaultColor;

  if not FOwner.Context.Hatch.IsInitValue(pvColour) then
    FOwner.Context.Hatch.FColour := vDefaultColor;

  if not FOwner.Context.Text.IsInitValue(pvColour) then
    FOwner.Context.Text.FColour := vDefaultColor;
end;

procedure TsgPrecisions.SetColourValueMax(const Value: TsgCgmColor);
begin
  FColourValueMax := Value;
end;

procedure TsgPrecisions.SetColourValueMin(const Value: TsgCgmColor);
begin
  FColourValueMin := Value;
end;

procedure TsgPrecisions.SetEdgeWidthSpecMode(const Value: TsgCgmWidthSpecMode);
begin
  FOwner.Context.Edge.FSpecMode := Value;
  FOwner.Context.Edge.Width := GetWidth(FOwner.Context.Edge, FVDCExtents);
end;

procedure TsgPrecisions.SetIndexPrecision(const Value: TsgCgmIntPrecision);
begin
  FIndexPrecision := Value;
end;

procedure TsgPrecisions.SetIntegerPrecision(const Value: TsgCgmIntPrecision);
begin
  FIntegerPrecision := Value;
end;

procedure TsgPrecisions.SetInteriorStyleSpecMode(
  const Value: TsgCgmWidthSpecMode);
begin
  FOwner.Context.Hatch.FSpecMode := Value;
end;

procedure TsgPrecisions.SetLineWidthSpecMode(const Value: TsgCgmWidthSpecMode);
begin
  FOwner.Context.Line.FSpecMode := Value;
  FOwner.Context.Line.Width := GetWidth(FOwner.Context.Line, FVDCExtents);
end;

procedure TsgPrecisions.SetMarkerWidthSpecMode(
  const Value: TsgCgmWidthSpecMode);
begin
  FOwner.Context.Marker.FSpecMode := Value;
  FOwner.Context.Marker.Width := GetWidth(FOwner.Context.Marker, FVDCExtents);
end;

procedure TsgPrecisions.SetNamePrecision(const Value: TsgCgmIntPrecision);
begin
  FNamePrecision := Value;
end;

function TsgPrecisions.GetWidth(const AProperty: TsgMarkerPropery;
  const AVDCExtents: TFRect): Double;
var
  vVDCRect: TFRect;
begin
  Result := 1;
  case AProperty.SpecMode of
    wsmAbsolute:
      begin
       vVDCRect := GetDefaultVDCExtents(VDCType);
       Result := AProperty.DefaultScaleFactor *
         Max(vVDCRect.Right - vVDCRect.Left, vVDCRect.Bottom - vVDCRect.Top);
      end;
    wsmScaled:   Result := 1;
    wsmFractional:  Result := AProperty.DefaultScaleFactor;
    wsmMM:          Result := AProperty.DefaultWidthMM;
  end;
end;

procedure TsgPrecisions.SetRealPrecision(const Value: TsgCgmRealPrecision);
begin
  FRealPrecision := Value;
end;

procedure TsgPrecisions.SetScalingMode(const Value: TsgCgmScalingMode);
begin
  FScalingMode := Value;
end;

procedure TsgPrecisions.SetScalingModeFactor(const Value: Double);
begin
  FScalingModeFactor := Value;
end;

procedure TsgPrecisions.SetVDCExtents(const Value: TFRect);
begin
  FVDCExtents := Value;
  if FOwner.Context.Line.SpecMode = wsmAbsolute then
    SetLineWidthSpecMode(wsmAbsolute);
  if FOwner.Context.Marker.SpecMode = wsmAbsolute then
    SetMarkerWidthSpecMode(wsmAbsolute);
  if FOwner.Context.Edge.SpecMode = wsmAbsolute then
    SetEdgeWidthSpecMode(wsmAbsolute);
  FOwner.Context.Hatch.FillReferencePoint := FVDCExtents.TopLeft;
end;

procedure TsgPrecisions.SetVDCIntPrecision(const Value: TsgCgmIntPrecision);
begin
  FVDCIntPrecision := Value;
end;

procedure TsgPrecisions.SetVDCRealPrecision(const Value: TsgCgmRealPrecision);
begin
  FVDCRealPrecision := Value;
end;

procedure TsgPrecisions.SetVDCType(const Value: TsgCgmVDCType);
begin
  FVDCType := Value;
  VDCExtents := GetDefaultVDCExtents(FVDCType);
end;

{ TsgContext }

procedure TsgCGMContext.Assign(const AContext: TsgCGMContext);
begin
  FEdge.Assign(AContext.FEdge);
  FLine.Assign(AContext.FLine);
  FMarker.Assign(AContext.FMarker);
  FText.Assign(AContext.FText);
  FHatch.Assign(AContext.FHatch);
  FClipIndicator:= AContext.FClipIndicator;
  FClipRectangle:= AContext.FClipRectangle;
  FProtectionRegionIndicator:= AContext.FProtectionRegionIndicator;
  FAuxiliaryColour:= AContext.FAuxiliaryColour;
  FTransparency:= AContext.FTransparency;
  FMitreLimit:= AContext.FMitreLimit;
  FAspectSourceFlags:= AContext.FAspectSourceFlags;
  FSymbolLibraryIndex:= AContext.FSymbolLibraryIndex;
end;

constructor TsgCGMContext.Create;
begin
  inherited Create;
  FEdge:= TsgEdgeProperty.Create;
  FLine:= TsgLineProperty.Create;
  FMarker:= TsgMarkerPropery.Create;
  FText:= TsgTextProperty.Create;
  FHatch:= TsgHatchProperty.Create;
  Reset(crtUnknown);
end;

destructor TsgCGMContext.Destroy;
begin
  FEdge.Free;
  FLine.Free;
  FMarker.Free;
  FText.Free;
  FHatch.Free;
  inherited Destroy;
end;

procedure TsgCGMContext.Reset(AReaderType: TsgCGMReaderType);
var
  vColourDefault: TsgCgmColor;
begin
  FillChar(vColourDefault, SizeOf(TsgCgmColor), 0);
  vColourDefault.Model := cmlIndexes;
  vColourDefault.Index := 1;

  FMitreLimit := 32767;
  FClipIndicator :=  True;
  FClipRectangle := MakeFRectByPoints(cnstFPointZero,
    MakeFPoint(FMitreLimit,FMitreLimit,0));
  FProtectionRegionIndicator := 1;
  FAuxiliaryColour := vColourDefault;
  FTransparency :=  True;
  FAspectSourceFlags := 0;
  FSymbolLibraryIndex := -1;
  //Line
  FLine.Reset(AReaderType);
  //Edge
  FEdge.Reset(AReaderType);
  //Marker
  FMarker.Reset(AReaderType);
  //Text
  FText.Reset(AReaderType);
  FText.CharacterHeight := FMitreLimit / 1000;
  //Hatch
  FHatch.Reset(AReaderType);
end;

procedure TsgCGMContext.SetAspectSourceFlags(const Value: Integer);
begin
  FSymbolLibraryIndex := Value;
end;

procedure TsgCGMContext.SetAuxiliaryColour(const Value: TsgCgmColor);
begin
  FAuxiliaryColour := Value;
end;

procedure TsgCGMContext.SetClipIndicator(const Value: Boolean);
begin
  FClipIndicator := Value;
end;

procedure TsgCGMContext.SetClipRectangle(const Value: TFRect);
begin
  FClipRectangle := Value;
end;

procedure TsgCGMContext.SetMitreLimit(const Value: Integer);
begin
  FAspectSourceFlags := Value;
end;

procedure TsgCGMContext.SetProtectionRegionIndicator(const Value: Integer);
begin
  FProtectionRegionIndicator := Value;
end;

procedure TsgCGMContext.SetSymbolLibraryIndex(const Value: Integer);
begin
  FSymbolLibraryIndex := Value;
end;

procedure TsgCGMContext.SetTransparency(const Value: Boolean);
begin
  FTransparency := Value;
end;

{ TsgContainer }

procedure TsgContainer.Assign(const AContext: TsgContainer);
begin
  FPrecision.Assign(AContext.FPrecision);
  FContext.Assign(AContext.FContext);
  if FTableColor = nil then
    FTableColor := TsgIntegerList.Create;
  FTableColor.Assign(AContext.FTableColor);
  ClearObjects(FStackContext, True);
end;

constructor TsgContainer.Create;
begin
  inherited Create;
  FTableColor := TsgIntegerList.Create;
  FContext := TsgCGMContext.Create;
  FPrecision:= TsgPrecisions.Create(Self);
end;

destructor TsgContainer.Destroy;
begin
  FreeAndNil(FTableColor);
  FPrecision.Free;
  FContext.Free;
  inherited Destroy;
end;

function TsgContainer.GetBackgroundColour: TsgColorCAD;
begin
  Result := MakeColorCAD(acRGBColor, Cardinal(FTableColor[0]));
end;

function TsgContainer.GetColorByIndex(const AIndex: Integer): Integer;
begin
  if (FTableColor <> nil) and (AIndex < FTableColor.Count) and (AIndex > -1) then
    Result := Integer(FTableColor[AIndex])
  else
    Result := ForeGroundColour.Color;
end;

function TsgContainer.GetForegroundColour: TsgColorCAD;
begin
  Result := BackGroundColour;
  Result.Color := $00FFFFFF and (not Result.Color);
end;

procedure TsgContainer.Reset(AReaderType: TsgCGMReaderType);
begin
  Context.Reset(AReaderType);
  Precision.Reset(AReaderType);
  ClearObjects(FStackContext, True);
  if FTableColor <> nil then
    FTableColor.Count := 1;
  BackgroundColour := MakeColorCAD(acRGBColor, clWhite);
end;

procedure TsgContainer.SetBackgroundColour(const Value: TsgColorCAD);
begin
//  if FTableColor = nil then
//    FTableColor := TList.Create;
  if FTableColor.Count = 0 then
    FTableColor.Add(clWhite);
  if Value.Active = acRGBColor then
    FTableColor[0] := Value.Color
  else
    FTableColor[0] := ConvertColorCADToRGB(Value);
end;

{ TsgProperty }

procedure TsgProperty.Assign(const AProperty: TsgProperty);
begin
  FFlags := TsgProperty(AProperty).FFlags;
  FColour := TsgProperty(AProperty).FColour;
  FBundleIndex := TsgProperty(AProperty).FBundleIndex;
end;

function TsgProperty.GetGroupElement: TsgCgmGroupElements;
begin
  Result := geNone;
end;

function TsgProperty.IsInitValue(const AProperty: TsgPropertyValue): Boolean;
begin
  Result := AProperty in FFlags;
end;

procedure TsgProperty.Reset;
begin
  FFlags := [];
  FillChar(FColour, SizeOf(TsgCgmColor), 0);
  FColour.Model := cmlIndexes;
  FColour.Index := 1;
end;

procedure TsgProperty.SetBundleIndex(const Value: Integer);
begin
  FBundleIndex := Value;
  Include(FFlags, pvBundleIndex);
end;

procedure TsgProperty.SetColour(const Value: TsgCgmColor);
begin
  FColour := Value;
  Include(FFlags, pvColour);
end;

{ TsgMarkerPropery }

procedure TsgMarkerPropery.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgMarkerPropery then
  begin
    EType := TsgMarkerPropery(AProperty).EType;
    Width := TsgMarkerPropery(AProperty).Width;
    ClippingMode := TsgMarkerPropery(AProperty).ClippingMode;
    FSpecMode := TsgMarkerPropery(AProperty).FSpecMode;
  end;
end;

function TsgMarkerPropery.GetDefaultScaleFactor: Double;
begin
  Result := 0.001;
end;

function TsgMarkerPropery.GetDefaultWidthMM: Double;
begin
  Result := 2.5;
end;

function TsgMarkerPropery.GetGroupElement: TsgCgmGroupElements;
begin
  Result := geMarker;
end;

procedure TsgMarkerPropery.Reset(AReaderType: TsgCGMReaderType);
begin
  inherited Reset(AReaderType);
  BundleIndex := 1;
  EType := Integer(mtStar);
  Width := 1;
  ClippingMode := 0
end;

{ TsgLineProperty }

procedure TsgLineProperty.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgLineProperty then
  begin
    PenStyle := TsgLineProperty(AProperty).PenStyle;
    TypeContinuation := TsgLineProperty(AProperty).TypeContinuation;
    TypeInitialOffset := TsgLineProperty(AProperty).TypeInitialOffset;
  end;
end;

function TsgLineProperty.GetDefaultScaleFactor: Double;
begin
  Result := 0.0001;
end;

function TsgLineProperty.GetDefaultWidthMM: Double;
begin
  Result := 0.35;
end;

function TsgLineProperty.GetGroupElement: TsgCgmGroupElements;
begin
  Result := geLine;
end;

procedure TsgLineProperty.Reset;
begin
  inherited Reset(AReaderType);
  EType := Integer(ltSolid);
  PenStyle := PS_ENDCAP_ROUND or PS_JOIN_ROUND;
  TypeContinuation := 0;
  TypeInitialOffset := 0;
end;

{ TsgEdgeProperty }

procedure TsgEdgeProperty.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgEdgeProperty then
  begin
    Visibility := TsgEdgeProperty(AProperty).Visibility;
  end;
end;

function TsgEdgeProperty.GetGroupElement: TsgCgmGroupElements;
begin
  Result := geEdge;
end;

procedure TsgEdgeProperty.Reset(AReaderType: TsgCGMReaderType);
begin
  inherited Reset(AReaderType);
  Visibility := False;
end;

{ TsgTextProperty }

procedure TsgTextProperty.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgTextProperty then
  begin
    TextFontIndex := TsgTextProperty(AProperty).TextFontIndex;
    TextPrecision := TsgTextProperty(AProperty).TextPrecision;
    ContinuousHorizontalAlignment :=
      TsgTextProperty(AProperty).ContinuousHorizontalAlignment;
    ContinuousVerticalAlignment :=
      TsgTextProperty(AProperty).ContinuousVerticalAlignment;
    CharacterExpansionFactor :=
      TsgTextProperty(AProperty).CharacterExpansionFactor;
    CharacterSpacing := TsgTextProperty(AProperty).CharacterSpacing;
    CharacterHeight := TsgTextProperty(AProperty).CharacterHeight;
    CharacterOrientationBase :=
      TsgTextProperty(AProperty).CharacterOrientationBase;
    CharacterOrientationUp := TsgTextProperty(AProperty).CharacterOrientationUp;
    TextPath := TsgTextProperty(AProperty).TextPath;
    HorizontalAlignment := TsgTextProperty(AProperty).HorizontalAlignment;
    VerticalAlignment := TsgTextProperty(AProperty).VerticalAlignment;
    CharacterSetIndex := TsgTextProperty(AProperty).CharacterSetIndex;
    AlternateCharacterSetIndex :=
      TsgTextProperty(AProperty).AlternateCharacterSetIndex;
    TextScoreType := TsgTextProperty(AProperty).TextScoreType;
    RestrictedTextType := TsgTextProperty(AProperty).RestrictedTextType;
    SymbolColour := TsgTextProperty(AProperty).SymbolColour;
    SymbolSize := TsgTextProperty(AProperty).SymbolSize;
    SymbolOrientationBase := TsgTextProperty(AProperty).SymbolOrientationBase;
    SymbolOrientationUp := TsgTextProperty(AProperty).SymbolOrientationUp;
  end;
end;

function TsgTextProperty.GetGroupElement: TsgCgmGroupElements;
begin
  Result := geText;
end;

procedure TsgTextProperty.Reset(AReaderType: TsgCGMReaderType);
begin
  inherited Reset(AReaderType);
  TextFontIndex := 1;
  TextPrecision := tpString;
  CharacterExpansionFactor := 1;
  CharacterSpacing := 0;
  CharacterHeight := 1 / 1000;
  CharacterOrientationBase := cnstXOrtAxis;
  CharacterOrientationUp := cnstYOrtAxis;
  TextPath := Integer(tpaRight);
  HorizontalAlignment := thaNormal;
  VerticalAlignment := tvaNormal;
  CharacterSetIndex := 1;
  AlternateCharacterSetIndex := 1;
  TextScoreType := 1;
  RestrictedTextType := 1;
  SymbolColour := FColour;
  SymbolSize := 0.01;
  SymbolOrientationBase := CharacterOrientationBase;
  SymbolOrientationUp := CharacterOrientationUp;
end;

{ TsgHatchProperty }

procedure TsgHatchProperty.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgHatchProperty then
  begin
    InteriorStyle := TsgHatchProperty(AProperty).InteriorStyle;
    HatchIndex := TsgHatchProperty(AProperty).HatchIndex;
    PatternIndex := TsgHatchProperty(AProperty).PatternIndex;
    InterpolatedInterior := TsgHatchProperty(AProperty).InterpolatedInterior;
    FillReferencePoint := TsgHatchProperty(AProperty).FillReferencePoint;
    PatternSize := TsgHatchProperty(AProperty).PatternSize;
    PickIdentifier := TsgHatchProperty(AProperty).PickIdentifier;
  end;
end;

{ TcgmHatshStyle }

procedure TcgmHatchStyle.Apply(const AHatch: TsgCADhatch;
  const AConverter: TsgDXFConverter);
var
  vHatch: TsgCADHatchAccess absolute AHatch;
//  vPattern: PsgHatchPatternData;
  vAngle: Double;
begin
  vHatch.ClearHatchPatternData;
  vAngle := GetAngleByPoints(cnstFPointZero, OffsetPoint, False);
{  vPattern := }vHatch.AddPatternData(cnstFPointZero, vAngle, -1);
end;

constructor TcgmHatchStyle.Create;
begin
  inherited Create;
  FGapWidths := TsgIntegerList.Create;
  FLineTypes := TsgIntegerList.Create;
end;

destructor TcgmHatchStyle.Destroy;
begin
  FLineTypes.Free;
  FGapWidths.Free;
  inherited Destroy;
end;

procedure TsgHatchProperty.Reset(AReaderType: TsgCGMReaderType);
begin
  inherited Reset(AReaderType);
  InteriorStyle := isHollow;
  HatchIndex := 1;
  PatternIndex := 1;
  InterpolatedInterior := 0;
  FillReferencePoint := cnstFPointZero;
  PatternSize := 0;
  PickIdentifier := 0;
end;

{ TsgPropertySpecMode }

procedure TsgPropertySpecMode.Assign(const AProperty: TsgProperty);
begin
  inherited Assign(AProperty);
  if AProperty is TsgPropertySpecMode then
    FSpecMode := TsgPropertySpecMode(AProperty).FSpecMode;
end;

procedure TsgPropertySpecMode.Reset(AReaderType: TsgCGMReaderType);
begin
  inherited Reset(AReaderType);
  FSpecMode := wsmAbsolute;
end;

{ TsgSism }

//
// adust the x offset from the baseline to maximum amplitude
//
procedure TsgSism.AddFlatHatchPoint(const X, Y: Double);
begin
  FFlatHatch.Points.Add(X);
  FFlatHatch.Points.Add(Y);
  Inc(FFlatHatchCount);
  ExpandFRect2D(FFlatHatchBox, MakeFPoint(X, Y));
end;

procedure TsgSism.Ampscf(const ADx: Double);
var
  vMax: Double;
  I: Integer;
begin
  vMax := 1;
  case FSampleType of
    0, 4: vMax := MAXSHORT;
    1: vMax := MaxInt;
    2: vMax := MaxDouble;
    3, 5: vMax := MAXCHAR;
  end;
  for I := 0 to FNumberOfSamples - 1 do
    FSampleD[I] := (FSampleD[I]/vMax)*FAmplitudeScaleFactor*ADx;
end;

function TsgSism.Interpl(const I: Integer; const AY1, AY2, X: Double): Double;
begin
  Result := (AY1*(X - FSampleD[I+1]) + AY2*(FSampleD[I] - X))/
    (FSampleD[I] - FSampleD[I+1]);
end;

procedure TsgSism.Vasamp(const AMode: Integer; AMax, AMin: Double;
  const ACordep: Integer);
var
  vFactx, vFacty: Double;
  x: array[0..5] of Double;
  y: array[0..5] of Double;
  vSamp1, vSamp2, vMn, vMx: Double;
  I, J, N, vLineCount: Integer;
  y1, y2, dx: Double;
  vColor: TsgColorCAD;
begin
  vFacty := FBaseDirection.Y * FBaseScaleFactor;
  vFactx := FTraceDirection.X * FTraceStepFactor;

  AMax := AMax * FAmplitudeScaleFactor;
  AMin := AMIn * FAmplitudeScaleFactor;
  vMn := AMin;
  vMx := AMax;
  if AMode = 64 then
  begin
    SwapDoubles(AMax, AMin);
    vMn := AMin * -1.0;
    vMx := AMax * -1.0;
  end;

  vColor := cnstColorCADByLayer;
  for I := 0 to FNumberOfSamples - 2 do
  begin
    y1 := FTraceStartPoint.Y + vFacty*I;
    y2 := FTraceStartPoint.Y + vFacty*(I+1);
    dx := FTraceStartPoint.X + vFactx*(FTrace-1);

    vSamp1 := FSampleD[I];
    vSamp2 := FSampleD[I+1];
    if AMode = 64 then
    begin
      vSamp1 := vSamp1 * -1.0;
      vSamp2 := vSamp2 * -1.0;
    end;
    N := 0;
    if (vSamp1 < vMn) and (vSamp2 > vMn) and (vSamp2 < vMx) then
    begin
      x[0] := AMin + dx;
      y[0] := Interpl(I, y1, y2, AMin);
      x[1] := FSampleD[I+1] + dx;
      y[1] := y2;
      x[2] := Amin + dx;
      y[2] := y2;
      N := 3;
    end
    else if (vSamp1 < vMn) and (vSamp2 > vMx) then
    begin
      x[0] := AMin + dx;
      y[0] := Interpl(I, y1, y2, AMin);
      x[1] := AMAx + dx;
      y[1] := Interpl(I, y1, y2, AMax);
      x[2] := AMax + dx;
      y[2] := y2;
      x[3] := AMin + dx;
      y[3] := y2;
      N := 4;
    end
    else if (((vSamp1 > vMn) and (vSamp1 < vMx)) and
            ((vSamp2 > vMn) and (vSamp2 < vMx))) then
    begin
      x[0] := AMin + dx;
      y[0] := y1;
      x[1] := FSampleD[I] + dx;
      y[1] := y1;
      x[2] := FSampleD[I+1] + dx;
      y[2] := y2;
      x[3] := AMin + dx;
      y[3] := y2;
      N := 4;
    end
    else if (vSamp1 > vMn) and (vSamp1 < vMx) and (vSamp2 > vMx) then
    begin
      x[0] := AMin + dx;
      y[0] := y1;
      x[1] := FSampleD[I] + dx;
      y[1] := y1;
      x[2] := AMax + dx;
      y[2] := Interpl(I, y1, y2, AMax);
      x[3] := AMax + dx;
      y[3] := y2;
      x[4] := AMin + dx;
      y[4] := y2;
      N := 5;
    end
    else if (vSamp1 > vMx) and (vSamp2 > vMx) then
    begin
      x[0] := AMin + dx;
      y[0] := y1;
      x[1] := AMax + dx;
      y[1] := y1;
      x[2] := AMax + dx;
      y[2] := y2;
      x[3] := AMin + dx;
      y[3] := y2;
      N := 4;
    end
    else if (vSamp1 > vMx) and (vSamp2 < vMx) and (vSamp2 > vMn) then
    begin
      x[0] := AMin + dx;
      y[0] := y1;
      x[1] := AMax + dx;
      y[1] := y1;
      x[2] := AMax + dx;
      y[2] := Interpl(I, y1, y2, AMax);
      x[3] := FSampleD[I+1] + dx;
      y[3] := y2;
      x[4] := AMin + dx;
      y[4] := y2;
      N := 5;
    end
    else if (vSamp1 > vMn) and (vSamp1 < vMx) and (vSamp2 < vMn) then
    begin
      x[0] := AMin + dx;
      y[0] := y1;
      x[1] := FSampleD[I] + dx;
      y[1] := y1;
      x[2] := AMin + dx;
      y[2] := Interpl(I, y1, y2, AMin);
      N := 3;
    end;

    if N > 0 then
    begin
      if ACordep <> 0 then
        vColor := FImage.GetCADColour(PsgCgmColorArray(FCoind)[I]);
      FFlatHatch := CreatePolygon(vColor);
      ResetFlatHatchBox;
      J := 0;
      vLineCount := N - 2;
      while J <= vLineCount do
      begin
        AddFlatHatchPoint(x[J], y[J]);
        Inc(J);
        AddFlatHatchPoint(x[J], y[J]);
      end;
      SetFlatHatchBox;
    FImage.Converter.Loads(FFlatHatch);
    end;
  end;
end;

procedure TsgSism.Bgclfl(const AMode: Integer);
var
  vFactx, vFacty: Double;
  posdx, negdx: Double;
  I: Integer;
  vColor: TsgColorCAD;
begin
  vFacty := FBaseDirection.Y * FBaseScaleFactor;
  vFactx := FTraceDirection.X * FTraceStepFactor;

  posdx := FPositiveBackFillBoundary * FAmplitudeScaleFactor * FAmpDirection.X;
  negdx := FNegativeBackFillBoundary * FAmplitudeScaleFactor * FAmpDirection.X;
  for I := 0 to FNumberOfSamples-1 do
  begin
    vColor := FImage.GetCADColour(PsgCgmColorArray(FCoind)[I]);
    FFlatHatch := CreatePolygon(vColor);
    ResetFlatHatchBox;
    AddFlatHatchPoint(posdx + FTraceStartPoint.X + vFactx*(FTrace-1),
       FTraceStartPoint.Y + vFacty*(I));
    AddFlatHatchPoint(posdx + FTraceStartPoint.X + vFactx*(FTrace-1),
       FTraceStartPoint.Y + vFacty*(I+1));
    AddFlatHatchPoint(negdx + FTraceStartPoint.X + vFactx*(FTrace-1),
       FTraceStartPoint.Y + vFacty*(I+1));
    AddFlatHatchPoint(negdx + FTraceStartPoint.X + vFactx*(FTrace-1),
       FTraceStartPoint.Y + vFacty*(I));
    SetFlatHatchBox;
    FImage.Converter.Loads(FFlatHatch);
  end;
end;

procedure TsgSism.Wiggle(APosclp, ANegclp: Double);
var
  vFactx, vFacty: Double;
  dx: Double;
  I: Integer;
  y1, y2: Double;
  vBox: TFRect;
  vPointCount: Integer;

  procedure AddPoint(const X, Y: Double);
  begin
    FFlatPoly.Points.Add(X);
    FFlatPoly.Points.Add(Y);
    Inc(vPointCount);
    ExpandFRect2D(vBox, MakeFPoint(X, Y));
  end;

begin
  vFacty := FBaseDirection.Y * FBaseScaleFactor;
  vFactx := FTraceDirection.X * FTraceStepFactor;
  dx := FTraceStartPoint.X + vFactx*(FTrace-1);

  APosclp := APosclp * FAmplitudeScaleFactor;
  ANegclp := ANegclp * FAmplitudeScaleFactor;

  vBox := cnstBadRect;
  vBox.Z1 := 0;
  vBox.Z2 := 0;
  vPointCount := 0;

  if FFlatPoly = nil then
  begin
    FFlatPoly := TsgFlatPoly.Create;
    FImage.AddEntityInSection(FFlatPoly);
  end;
  try
    for I := 0 to FNumberOfSamples - 1 do
    begin
      y1 := FTraceStartPoint.Y + vFacty*I;
      y2 := FTraceStartPoint.Y + vFacty*(I+1);

      if (FSampleD[i]> ANegclp) and (FSampleD[i]< APosclp) and
          (FSampleD[i+1] > ANegclp) and (FSampleD[i+1] < APosclp)  then
      begin
        AddPoint(FSampleD[I]+ dx, y1);
      end
      else if (FSampleD[i]<ANegclp) and (FSampleD[i+1]>ANegclp) and (FSampleD[i+1]<APosclp) then
      begin
        AddPoint(ANegclp+ dx, Interpl(i, y1, y2, ANegclp));
      end
      else if (FSampleD[i]<ANegclp) and (FSampleD[i+1]>APosclp) then
      begin
        AddPoint(ANegclp+ dx, Interpl(i, y1, y2, ANegclp));
        AddPoint(APosclp+ dx, Interpl(i, y1, y2, APosclp));
      end
      else if (FSampleD[i]>ANegclp) and (FSampleD[i]<APosclp) and (FSampleD[i+1]>APosclp) then
      begin
        AddPoint(FSampleD[I]+ dx, y1);
        AddPoint(APosclp+ dx, Interpl(i, y1, y2, APosclp));
      end
      else if (FSampleD[i]>APosclp) and (FSampleD[i+1]<APosclp) and (FSampleD[i+1]>ANegclp) then
      begin
        AddPoint(APosclp+ dx, Interpl(i, y1, y2, APosclp));
      end
      else if (FSampleD[i]>APosclp) and (FSampleD[i+1]<ANegclp) then
      begin
        AddPoint(APosclp + dx, Interpl(i, y1, y2, APosclp));
        AddPoint(ANegclp + dx, Interpl(i, y1, y2, ANegclp));
      end
      else if (FSampleD[i]>ANegclp) and (FSampleD[i]<APosclp) and (FSampleD[i+1]<ANegclp) then
      begin
        AddPoint(FSampleD[I]+ dx, y1);
        AddPoint(ANegclp+ dx, Interpl(i, y1, y2, ANegclp));
      end;
    end;
  finally
    if vPointCount > 0 then
    begin
      FFlatPoly.Counts.Add(vPointCount);
      UnionFRect(vBox, FFlatPoly.Box);
      TsgFlatPolyAccess(FFlatPoly).SetBox(vBox);
      FImage.Converter.Loads(FFlatPoly);
    end;
  end;
end;

procedure TsgSism.Clear;
begin
  FTrace := 0;
  FSampleD.Count := 0;
  if FCoind <> nil then
  begin
    FreeMem(FCoind);
    FCoind := nil;
  end;
end;

constructor TsgSism.Create(const AImage: TsgCGMImage);
begin
  FImage := AImage;
  FActiveSection := FImage.GetActiveSection;

  FSampleD := TsgDoubleList.Create;
  FCoind := nil;
end;

function TsgSism.CreatePolygon(const AColor: TsgColorCAD): TsgFlatHatch;
begin
  Result := TsgFlatHatch.Create;
  Result.ColorCAD := AColor;
  Result.Layer := FImage.Converter.LayerByName('0');
  Result.Counts.Add(0);//boundaries count
  Result.Counts.Add(0);//sum counts points in boundaries
  Result.Counts.Add(0);//flush
  FImage.AddEntityInSection(Result);
end;

destructor TsgSism.Destroy;
begin
  Clear;
  FreeAndNil(FSampleD);
  FFlatPoly := nil;
  FFlatHatch := nil;
  inherited Destroy;
end;
//
// adjust the samples to the amplitude direction vector
//
procedure TsgSism.Dirvet(const ADx: Double);
var
  I: Integer;
begin
  for I := 0 to FNumberOfSamples - 1 do
    FSampleD[I] := FSampleD[I] * (ADx / Abs(ADx));
end;

procedure TsgSism.ReadSample(const AIndex: Integer);
begin
  case FSampleType of
    0: FSampleD[AIndex] := FImage.FReader.ReadIntVal(ipSmallInt);
    1: FSampleD[AIndex] := FImage.FReader.ReadIntVal(ipInteger);
    2: FSampleD[AIndex] := FImage.FReader.ReadR;
    3: FSampleD[AIndex] := FImage.FReader.ReadByteCount(1);
    4: FSampleD[AIndex] := FImage.FReader.ReadIntVal(ipWord);
    5: FSampleD[AIndex] := FImage.FReader.ReadByteCount(1);
  end;
  case FSampleType of
    4,5: PsgCgmColorArray(FCoind)[AIndex] := FImage.FReader.ReadCI;
  end;
end;

procedure TsgSism.ResetFlatHatchBox;
begin
  FFlatHatchBox := FFlatHatch.Box;
  FFlatHatchBox.Z1 := 0;
  FFlatHatchBox.Z2 := 0;
  FFlatHatchCount := 0;
  FFlatHatchSumCounts := FFlatHatch.Counts[FFlatHatch.Count - 2];
  FFlatHatch.Counts.Count := FFlatHatch.Counts.Count - 2;
end;

procedure TsgSism.SetFlatHatchBox;
begin
  if FFlatHatchCount > 0 then
  begin
    FFlatHatch.Counts.First := FFlatHatch.Counts.First + 1;
    FFlatHatch.Counts.Add(FFlatHatchCount);
    Inc(FFlatHatchSumCounts, FFlatHatchCount);
    FFlatHatch.Counts.Add(FFlatHatchSumCounts);
    FFlatHatch.Counts.Add(0);//flush
    FFlatHatchCount := 0;
    TsgFlatPolyAccess(FFlatHatch).SetBox(FFlatHatchBox);
  end;
end;

procedure TsgSism.TraceData;
var
  I: Integer;
  vMode, vModeTemp: Integer;
  vPosClp, vNegClp: Double;
begin
  FSampleD.Count := FNumberOfSamples;
  if (FSampleType = 4) or (FSampleType = 5) then
    FCoind := PsgCgmColorArray(AllocMem(FNumberOfSamples * SizeOf(TsgCgmColor)));

  for I := 0 to FNumberOfSamples - 1 do
  begin
    ReadSample(I);
  end;

  Inc(FTrace);
  Dirvet(FAmpDirection.X);
  Ampscf(FAmpDirection.X);
  vMode := FTraceDisplayMode;

  while vMode <> 0 do
  begin
    case vMode of
      1: vModeTemp := 1;
      2..63: vModeTemp := (vMode shr 1) * 2;
      else
        vModeTemp := 64;
    end;
    case vModeTemp of
      64: begin
        vNegClp := FNegativeClippingLimit;
        vPosClp := IfThen(FPositiveClippingLimit < 0, FPositiveClippingLimit, 0);
        Vasamp(vModeTemp, vPosClp, vNegClp, 1);
        vMode := vMode - 64;
      end;
      32: begin
        vNegClp := IfThen(FNegativeClippingLimit > 0, FNegativeClippingLimit, 0);
        vPosClp := FPositiveClippingLimit;
        Vasamp(vModeTemp, vPosClp, vNegClp, 1);
        vMode := vMode - 32;
      end;
      16: begin
        Bgclfl(vModeTemp);
        vMode := vMode - 16;
      end;
      8: begin
        Bgclfl(vModeTemp);
        vMode := vMode - 8;
      end;
      4: begin
        vNegClp := IfThen(FNegativeClippingLimit > 0, FNegativeClippingLimit, 0);
        vPosClp := FPositiveClippingLimit;
        Vasamp(vModeTemp, vPosClp, vNegClp, 0);
        vMode := vMode - 4;
      end;
      2: begin
        vNegClp := IfThen(FNegativeClippingLimit > 0, FNegativeClippingLimit, 0);
        vPosClp := FPositiveClippingLimit;
        Vasamp(vModeTemp, vPosClp, vNegClp, 0);
        vMode := vMode - 2;
      end;
      1: begin
        vNegClp := FNegativeClippingLimit;
        vPosClp := FPositiveClippingLimit;
        Wiggle (vPosClp, vNegClp );
        vMode := vMode - 1;
      end;
    end;
  end;

  if FCoind <> nil then
  begin
    FreeMem(FCoind);
    FCoind := nil;
  end;
end;

procedure TsgSism.TraceInitialize;
begin
  Clear;
  // CGM+ Trace Initialize
  FTraceStartPoint := FImage.FReader.ReadP;
  FBaseDirection   := FImage.FReader.ReadP;
  FAmpDirection    := FImage.FReader.ReadP;
  FTraceDirection  := FImage.FReader.ReadP;

  FImage.FReader.ReadByteCount(2);

  FBaseScaleFactor       := FImage.FReader.ReadR;
  FAmplitudeScaleFactor  := FImage.FReader.ReadR;
  FTraceStepFactor       := FImage.FReader.ReadR;
  FVAOffset              := FImage.FReader.ReadR;
  FPositiveClippingLimit := FImage.FReader.ReadR;
  FNegativeClippingLimit := FImage.FReader.ReadR;
  FTraceDisplayMode         := FImage.FReader.ReadI;
  FSampleType               := FImage.FReader.ReadI;
  FNumberOfSamples          := FImage.FReader.ReadI;
  FPositiveBackFillBoundary := FImage.FReader.ReadR;
  FNegativeBackFillBoundary := FImage.FReader.ReadR;
  FWiggleTraceModulus       := FImage.FReader.ReadI;
  FNullColorIndex           := FImage.FReader.ReadI;
  FNumberTracesPerLocation  := FImage.FReader.ReadI;
end;

{ TsgCGM_SDR}

procedure ReadSDRData(const AReader: TsgCGMReader; const AXMLSDR: TsgNode);
var
  I: Integer;
  vType: Integer;
  vNode, vNodeChild: TsgNode;
  vColor: TsgCgmColor;
  vInteger: Integer;
  vString, vNodeChildName: string;
  vLength, vFlags, vCount: Integer;
  vStartPos: Int64;
  vDouble: Double;

  vReader: TsgCGMReader;
  vIsBin: Boolean;
  vStreamSDR: TStream;
  vSDRReader: TsgCGMReaderStr;
  vTemp: string;
begin
  vReader := AReader;
  vIsBin := False;
  vLength := 0;
  vStartPos := 0;
  vSDRReader := nil;
  vStreamSDR := nil;
  if vReader is TsgCGMReaderBin then
  begin
    vIsBin := True;
    vLength := vReader.ReadByteCount(1);
    if vLength >= 255 then
    begin
      vFlags := vReader.ReadByteCount(2);
      vLength := cnstMask15bit and vFlags;
    end;
    vStartPos := AReader.Position;
  end
  else
  begin

    vStreamSDR := TMemoryStream.Create;
    // Add shadow tag
    vTemp := '2021 ' + vReader.ReadSF;
    vStreamSDR.WriteBuffer(Pointer(AnsiString(vTemp))^, Length(vTemp));
    vStreamSDR.Position := 0;

    vSDRReader := TsgCGMReaderStr.Create;
    vSDRReader.InitInternalParams(vStreamSDR);
    vSDRReader.Position := 0;

    vSDRReader.ReadTag;
    vReader := vSDRReader;
  end;
  try
    while not vReader.IsTagEOF do
    begin
      vType := vReader.ReadIX;
      if (vType >= Ord(Low(TsgStructureDataType))) and (vType <= Ord(High(TsgStructureDataType)))  then
        vNodeChildName := cnstSDT_string[TsgStructureDataType(vType)]
      else
        vNodeChildName := 'UnknownStructureDataType_' + IntToStr(vType);
      vNodeChild := AXMLSDR.AddChildNV(vNodeChildName);
      vCount := vReader.ReadI;
      for I := 0 to vCount - 1 do
      case TsgStructureDataType(vType) of
        sdtSDR: begin
          vNode := vNodeChild.AddChildNV('Value');
          ReadSDRData(vReader, vNode);
        end;
        sdtCI: begin
          vColor := vReader.ReadCI;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Model').ValueData.ValueAsStr := IntToStr(Integer(vColor.Model));
          case vColor.Model of
            cmlIndexes:
              vNode.AddAttribNV('Indexes').ValueData.ValueAsStr := IntToStr(vColor.Index);
            cmlRGB:
              vNode.AddAttribNV('RGB').ValueData.ValueAsStr :=
                Format('%d %d %d %d', [IntToStr(vColor.Components[0]), IntToStr(vColor.Components[1]),
                 IntToStr(vColor.Components[2]), IntToStr(vColor.Components[3])]);
            cmlCMYK:
              vNode.AddAttribNV('CMYK').ValueData.ValueAsStr :=
                Format('%d %d %d %d', [IntToStr(vColor.Components[0]), IntToStr(vColor.Components[1]),
                 IntToStr(vColor.Components[2]), IntToStr(vColor.Components[3])]);
          end;
        end;
        sdtCD: begin
          vColor := vReader.ReadCD;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Model').ValueData.ValueAsStr := IntToStr(Integer(vColor.Model));
          case vColor.Model of
            cmlIndexes:
              vNode.AddAttribNV('Indexes').ValueData.ValueAsStr := IntToStr(vColor.Index);
            cmlRGB:
              vNode.AddAttribNV('RGB').ValueData.ValueAsStr :=
                Format('%d %d %d %d', [IntToStr(vColor.Components[0]), IntToStr(vColor.Components[1]),
                 IntToStr(vColor.Components[2]), IntToStr(vColor.Components[3])]);
            cmlCMYK:
              vNode.AddAttribNV('CMYK').ValueData.ValueAsStr :=
                Format('%d %d %d %d', [IntToStr(vColor.Components[0]), IntToStr(vColor.Components[1]),
                 IntToStr(vColor.Components[2]), IntToStr(vColor.Components[3])]);
          end;
        end;
        sdtN: begin
          vInteger := vReader.ReadN;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Name').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtE: begin
          vInteger := vReader.ReadE;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Enumerated').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtI: begin
          vInteger := vReader.ReadI;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Integer').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtIF8: begin
          vInteger := vReader.ReadByteCount(1);
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Integer 8 bit').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtIF16: begin
          vInteger := vReader.ReadByteCount(2);
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Integer 16 bit').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtIF32: begin
          vInteger := vReader.ReadByteCount(4);
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Integer 32 bit').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtIX: begin
          vInteger := vReader.ReadIX;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Index').ValueData.ValueAsStr := IntToStr(vInteger);
        end;
        sdtR: begin
          vDouble := vReader.ReadR;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('String').ValueData.ValueAsStr := FloatToString(vDouble);;
        end;
        sdtS: begin
        end;
        sdtSF: begin
          vString := vReader.ReadSF(True);
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('String').ValueData.ValueAsStr := vString;
        end;
        sdtVC: begin
        end;
        sdtVDC: begin
          vDouble := vReader.ReadVDC;
          vNode := vNodeChild.AddChildNV('Value');
          vNode.AddAttribNV('Double').ValueData.ValueAsStr := FloatToString(vDouble);
        end;
        sdtCCO:  begin
        end;
        sdtUI8: begin
        end;
        sdtUI32: begin
        end;
        sdtBS: begin
        end;
        sdtCL: begin
        end;
        sdtUI16: begin
        end;

      end;

      if vIsBin then
        if (AReader.Position - vStartPos >= vLength) then
          Break;
    end;
  finally
    if not vIsBin then
    begin
      vSDRReader.Free;
      vStreamSDR.Free;
    end;
  end;
end;

{ TsgAPSStackObject }

function TsgAPSStackObject.GetTopStack: TsgDXFEntity;
begin
  Result := nil;
  if Count > 0 then
    Result := List[Count - 1];
end;

function TsgAPSStackObject.GetUpperByType(
  const AClassName: string): TsgDXFEntity;
var
  I: Integer;
  vEntFind: TsgDXFEntity;
begin
  Result := nil;
  for I:= Count-1 downto 0 do
  begin
    vEntFind := List[I];
    if vEntFind.ClassNameIs(AClassName) then
    begin
      Result := vEntFind;
      Break;
    end;
  end;
end;

{ TsgFontAliasTable }

function TsgFontAliasTable.CompareAlias(const A, B: Pointer): Integer;
begin
  Result := CompareText(TsgFontAlias(A^).FontName, TsgFontAlias(B^).FontName);
end;

constructor TsgFontAliasTable.Create;
begin
  inherited Create;
  {$IFDEF SG_FIREMONKEY}
  FCS := TCriticalSection.Create;
  {$ENDIF}
  FFontAlias := TsgObjectList.Create;
  FFontAlias.Capacity := 32;
  FFontAlias.Duplicates := dupAccept;
  FFontAlias.ProcCompare := CompareAlias;

  AddItem('times-roman', 'SansSerif');
  AddItem('times-bold', 'SansSerif Bold');
  AddItem('times-italic', 'SansSerif Oblique');
  AddItem('times-bolditalic', 'SansSerif BoldOblique');
  AddItem('times-bold-italic', 'SansSerif BoldOblique');
  AddItem('helvetica', 'Arial');
  AddItem('helvetica-bold', 'Arial Bold');
  AddItem('helvetica-oblique', 'Arial Italic');
  AddItem('helvetica-boldoblique', 'Arial Bold Italic');
  AddItem('helvetica-bold-oblique', 'Arial Bold Italic');
//  //http://192.168.0.102/bugzilla/show_bug.cgi?id=4945
//  AddItem('courier', 'SansSerif'(*'Courier New'*)),
//  AddItem('courier-bold', 'SansSerif Bold'(*'Courier New Bold'*)),
//  AddItem('courier-italic', 'SansSerif Oblique'(*'Courier New Italic'*)),
//  AddItem('courier-oblique', 'SansSerif Oblique'(*'Courier New Italic'*)),
//  AddItem('courier-bolditalic', 'SansSerif BoldOblique'(*'Courier New Bold Italic'*)),
//  AddItem('courier-boldoblique', 'SansSerif BoldOblique'(*'Courier New Bold Italic'*)),
//  AddItem('courier-bold-italic', 'SansSerif BoldOblique'(*'Courier New Bold Italic'*)),
//  AddItem('courier-bold-oblique', 'SansSerif BoldOblique'(*'Courier New Bold Italic'*)),
  AddItem('courier', 'Courier New');
  AddItem('courier-bold', 'Courier New Bold');
  AddItem('courier-italic', 'Courier New Italic');
  AddItem('courier-oblique', 'Courier New Italic');
  AddItem('courier-bolditalic', 'Courier New Bold Italic');
  AddItem('courier-boldoblique', 'Courier New Bold Italic');
  AddItem('courier-bold-italic', 'Courier New Bold Italic');
  AddItem('courier-bold-oblique', 'Courier New Bold Italic');
  AddItem('symbol', 'Symbol');

  FFontAlias.Sorted := True;

  FFindItem := TsgFontAlias.Create;
end;

procedure TsgFontAliasTable.AddItem(const AFontName, AAliasName: string);
var
  vItem: TsgFontAlias;
begin
  vItem := TsgFontAlias.Create;
  vItem.FontName := AFontName;
  vItem.AliasName := AAliasName;
  FFontAlias.Add(vItem);
end;

destructor TsgFontAliasTable.Destroy;
begin
  FreeAndNil(FFindItem);
  TsgObjectList.FreeList(FFontAlias);
  {$IFDEF SG_FIREMONKEY}
  FreeAndNil(FCS);
  {$ENDIF}
  inherited Destroy;
end;

function TsgFontAliasTable.GetFontAlias(const AFont: string): string;
var
  vIndex: Integer;
  vObj: TObject;
begin
  Result := '';
  {$IFDEF SG_FIREMONKEY}
  FCS.Enter;
  try
  {$ENDIF}
    TsgFontAlias(FFindItem).FontName := LowerCase(AFont);
    vIndex := FFontAlias.IndexOf(FFindItem);
    if vIndex > -1 then
    begin
      vObj := FFontAlias[vIndex];
      if Assigned(vObj) then
        Result := TsgFontAlias(vObj).AliasName;
    end;
  {$IFDEF SG_FIREMONKEY}
  finally
    FCS.Leave;
  end;
  {$ENDIF}
end;

{ TsgSetList }

constructor TsgSetList.Create;
begin
  inherited Create;
  {$IFDEF SG_FIREMONKEY}
  FCS := TCriticalSection.Create;
  {$ENDIF}
  F94CharacterGSetOneByte := TsgObjectList.Create;//array[0..61] of TsgISO2375;
  F94CharacterGSetOneByte.Capacity := 64;
  F94CharacterGSetSecondByte := TsgObjectList.Create;//array[0..6] of TsgISO2375;
  F94CharacterGSetSecondByte.Capacity := 8;
  F96CharacterGSet := TsgObjectList.Create;//array[0..42] of TsgISO2375;
  F96CharacterGSet.Capacity := 48;
  F94MultipleByteCharacterGSet := TsgObjectList.Create;//array[0..17] of TsgISO2375;
  F94MultipleByteCharacterGSet.Capacity := 24;
  FCompleteCodeEscapeSeq := TsgObjectList.Create;//array[0..21] of TsgISO2375;
  FCompleteCodeEscapeSeq.Capacity := 24;
  Init;
  GenerateISO4873ListForCGM;
end;

procedure TsgSetList.AddItem(const AList: TsgObjectList;
  const AFinalByte: string; const ACodePage: Integer; const AContent: string);
var
  vItem: TsgISO2375;
begin
  vItem := TsgISO2375.Create;
  AList.Add(vItem);
  vItem.FinalByte := AFinalByte;
  vItem.CodePage := ACodePage;
  vItem.Content := AContent;
end;

destructor TsgSetList.Destroy;
begin
  FreeISO4873ListForCGM;
  TsgObjectList.FreeList(F94CharacterGSetOneByte);
  TsgObjectList.FreeList(F94CharacterGSetSecondByte);
  TsgObjectList.FreeList(F96CharacterGSet);
  TsgObjectList.FreeList(F94MultipleByteCharacterGSet);
  TsgObjectList.FreeList(FCompleteCodeEscapeSeq);
  {$IFDEF SG_FIREMONKEY}
  FreeAndNil(FCS);
  {$ENDIF}
  inherited Destroy;
end;

procedure TsgSetList.GenerateISO4873ListForCGM;
var
  vGSetType: TsgCgmCharacterSetType;

  procedure AddGSetToTable(const ACollection: TsgObjectCollection;
    const AGSetArray: TsgObjectList; const AConstPart: String = '');
  var
    I: Integer;
    vItem: TsgHashItemObject;
  begin
    for I := 0 to AGSetArray.Count - 1 do
    begin
      vItem.HashCode := ACollection.CalcHash(AConstPart + TsgISO2375(AGSetArray[I]).FinalByte);
      vItem.Data := AGSetArray[I];
      ACollection.Add(vItem);
    end;
  end;

begin
  for vGSetType := Low(TsgCgmCharacterSetType) to High(TsgCgmCharacterSetType) do
  begin
    ItemsList[vGSetType] := TsgObjectCollection.Create;
    ItemsList[vGSetType].Sorted := True;
  end;

  AddGSetToTable(ItemsList[cst94], F94CharacterGSetOneByte);
  AddGSetToTable(ItemsList[cst94], F94CharacterGSetSecondByte, '2/1' );
  AddGSetToTable(ItemsList[cst96], F96CharacterGSet);
  AddGSetToTable(ItemsList[cst94Multibyte], F94MultipleByteCharacterGSet);
  //AddGSetToTable(CGMGSetList[cst96Multibyte], cnst94CharacterGSetOneByte);
  AddGSetToTable(ItemsList[cstCompleteCode], FCompleteCodeEscapeSeq);
end;

procedure TsgSetList.FreeISO4873ListForCGM;
var
  vGSetType: TsgCgmCharacterSetType;
begin
  for vGSetType := Low(TsgCgmCharacterSetType) to High(TsgCgmCharacterSetType) do
    FreeAndNil(ItemsList[vGSetType]);
end;

function TsgSetList.GetISO2375Item(const AGSet: TsgCgmCharacterSetType;
  const ACode: String): TsgISO2375;
var
  I: Integer;
  vHash: UInt64;
  vCollection: TsgObjectCollection;
begin
  Result := nil;
  {$IFDEF SG_FIREMONKEY}
  FCS.Enter;
  try
  {$ENDIF}
    if Length(ACode) > 0 then
    begin
      vCollection := ItemsList[AGSet];
      if Assigned(vCollection) then
      begin
        vHash := vCollection.CalcHash(ACode);
        I := vCollection.IndexOf(vHash);
        if I > -1 then
          Result := TsgISO2375(vCollection.Items[I].Data);
      end;
    end;
  {$IFDEF SG_FIREMONKEY}
  finally
    FCS.Leave;
  end;
  {$ENDIF}
end;


procedure TsgSetList.Init;
begin
//94-Character graphic character sets with one Intermediate byte
// G0: ESC 2/8 F
// G1: ESC 2/9 F
// G2: ESC 2/10 F
// G3: ESC 2/11 F
  AddItem(F94CharacterGSetOneByte, '4/0', 20105, 'IRV of ISO 646 : 1983');
  AddItem(F94CharacterGSetOneByte, '4/1', 1013, 'ISO 646, British Version BSI 4730');
  AddItem(F94CharacterGSetOneByte, '4/2', 1251, 'ISO 646, USA Version X3.4 - 1968');
  AddItem(F94CharacterGSetOneByte, '4/3', 0, '8-1 NATS, Primary Set for Finland and Sweden');
  AddItem(F94CharacterGSetOneByte, '4/4', 0, 'NATS, Secondary Set for Finland and Sweden');
  AddItem(F94CharacterGSetOneByte, '4/5', 0, 'NATS, Primary Set for Denmark and Norway');
  AddItem(F94CharacterGSetOneByte, '4/6', 0, 'NATS, Secondary Set for Denmark and Norway');
  AddItem(F94CharacterGSetOneByte, '4/7', 20107, 'ISO 646, Swedish Version SEN 850200 (Ann. B)');
  AddItem(F94CharacterGSetOneByte, '4/8', 20107, 'ISO 646, Swedish Version for Names, (SEN 850200 Ann. C)');
  AddItem(F94CharacterGSetOneByte, '4/9', 50222, 'Katakana Character Set JIS C6220-1969');
  AddItem(F94CharacterGSetOneByte, '4/10', 50222, 'ISO 646, Japanese Version for Roman Characters JIS C6220-1969');
  AddItem(F94CharacterGSetOneByte, '5/9', 0, 'ISO 646, Version for Italian, ECMA (Olivetti)');
  AddItem(F94CharacterGSetOneByte, '4/12', 0, 'ISO 646, Version for Portuguese, ECMA (Olivetti)');
  AddItem(F94CharacterGSetOneByte, '5/10', 0, 'ISO 646, Version for Spanish, ECMA (Olivetti)');
  AddItem(F94CharacterGSetOneByte, '5/11', 0, 'Character Set for Greek, ECMA (Olivetti)');
  AddItem(F94CharacterGSetOneByte, '5/12', 0, 'Latin-Greek Character Set, ECMA (Olivetti)');
  AddItem(F94CharacterGSetOneByte, '4/11', 0, '646, German Version DIN 66083');
  AddItem(F94CharacterGSetOneByte, '5/2', 0, 'ISO 646, French Version, NF Z 62010-1973 (Withdrawn in April1985)');
  AddItem(F94CharacterGSetOneByte, '5/5', 0, 'Latin-Greek Character Set, ECMA (Honeywell-Bull)');
  AddItem(F94CharacterGSetOneByte, '5/8', 0, 'Greek Character Set for Bibliography, ISO 5428');
  AddItem(F94CharacterGSetOneByte, '4/14', 0, 'Basic Cyrillic Character Set, ECMA (Cii Honeywell-Bull) and ISO 5427');
  AddItem(F94CharacterGSetOneByte, '4/15', 0, 'Extended Graphic Character Set for Bibliography, DIN 31624');
  AddItem(F94CharacterGSetOneByte, '4/13', 0, 'Character Set for African Languages, DIN 31625 and ISO 6438');
  AddItem(F94CharacterGSetOneByte, '5/6', 0, 'Character Set for Viewdata and Teletext (UK)');
  AddItem(F94CharacterGSetOneByte, '5/7', 0, 'INIS, Sub-set of the IRV');
  AddItem(F94CharacterGSetOneByte, '5/13', 0, 'INIS, Non-standard Extension of Reg. 49');
  AddItem(F94CharacterGSetOneByte, '5/14', 0, 'INIS, Cyrillic Extension of Reg. 49');
  AddItem(F94CharacterGSetOneByte, '5/0', 0, 'Extended Graphic Character Set for Bibliography ISO 5426-1980');
  AddItem(F94CharacterGSetOneByte, '5/1', 0, 'Extension of the Cyrillic Character Set of Reg. 37, ISO 5427-1981');
  AddItem(F94CharacterGSetOneByte, '5/3', 0, 'Greek Character Set for Bibliography, ISO 5428-1980');
  AddItem(F94CharacterGSetOneByte, '5/4', 0, 'Coded Character Set for Information Interchange, Standard Chinese GB 1988-80');
  AddItem(F94CharacterGSetOneByte, '5/15', 0, 'Arabic Character Set CODAR-U IERA (Morocco)');
  AddItem(F94CharacterGSetOneByte, '6/0', 0, 'ISO 646, Norwegian Version NS 4551');
  AddItem(F94CharacterGSetOneByte, '6/1', 0, 'Norwegian Character Set, Version 2, NS 4551 (Withdrawn in June 1987)');
  AddItem(F94CharacterGSetOneByte, '6/5', 0, 'APL Character Set, Canadian APL Working Group');
  AddItem(F94CharacterGSetOneByte, '6/6', 0, 'ISO 646, French Version NF Z 62010-1982');
  AddItem(F94CharacterGSetOneByte, '6/2', 0, '70 Supplementary Set for Videotex, CCITT');
  AddItem(F94CharacterGSetOneByte, '6/3', 0, 'Second Supplementary Set for Videotex (Mosaic), CCITT');
  AddItem(F94CharacterGSetOneByte, '6/4', 0, 'Third Supplementary Set for Videotex, (Mosaic), CCITT (superseded by Reg. 173)');
  AddItem(F94CharacterGSetOneByte, '6/7', 0, 'ISO 646, Version for Portuguese, ECMA (IBM)');
  AddItem(F94CharacterGSetOneByte, '6/8', 0, 'ISO 646, Version for the Spanish languages, ECMA (IBM)');
  AddItem(F94CharacterGSetOneByte, '6/9', 0, 'ISO 646, Hungarian Version Hungarian standard 7795/3');
  AddItem(F94CharacterGSetOneByte, '6/10', 0, 'Greek Character Set ELOT, Hellenic Organization for Standardization (Withdrawn in November 1986)');
  AddItem(F94CharacterGSetOneByte, '6/11', 0, '7-bit Arabic Code for Information Interchange, Arab standard ASMO-449, ISO 9036');
  AddItem(F94CharacterGSetOneByte, '6/12', 0, 'Supplementary Set for Use with Registration No.2');
  AddItem(F94CharacterGSetOneByte, '6/13', 0, 'Japanese OCR-A graphic set JIS C6229-1984');
  AddItem(F94CharacterGSetOneByte, '6/14', 0, 'Japanese OCR-B Graphic Set JIS C6229-1984');
  AddItem(F94CharacterGSetOneByte, '6/15', 0, 'Japanese OCR-B, Additional Graphic Set, JIS C6229-1984');
  AddItem(F94CharacterGSetOneByte, '7/0', 0, 'Japanese Basic Hand-printed Graphic Set for OCR JIS C6229-1984');
  AddItem(F94CharacterGSetOneByte, '7/1', 0, 'Japanese Additional Handprinted Graphic Character Set for OCR JIS C6229- 1984');
  AddItem(F94CharacterGSetOneByte, '7/2', 0, 'Katakana hand-printed Graphic Character Set for OCR JIS C6229-1984');
  AddItem(F94CharacterGSetOneByte, '7/3', 0, 'E13B Graphic Character Set Japanese National Committee for ISO/TC97/SC2');
  AddItem(F94CharacterGSetOneByte, '7/4', 0, 'Supplementary Set of Graphic Characters for Videotex and Teletext ANSI and Teletext ANSI and CSA (Withdrawn in November 1986)');
  AddItem(F94CharacterGSetOneByte, '7/5', 0, 'Teletex Primary Set of Graphic Characters CCITT Rec. T.61');
  AddItem(F94CharacterGSetOneByte, '7/6', 0, 'Teletex Supplementary Set of Graphic characters CCITT Rec. T.61');
  AddItem(F94CharacterGSetOneByte, '7/7', 0, 'Alternate Primary Graphic Set No. 1 CSA Standard Z 243.4-1985');
  AddItem(F94CharacterGSetOneByte, '7/8', 0, 'Alternate Primary Graphic Set No.2 CSA Standard Z 243.4-1985');
  AddItem(F94CharacterGSetOneByte, '7/12', 0, 'Supplementary Set of graphic Characters for CCITT Rec. T.101, Data Syntax III');
  AddItem(F94CharacterGSetOneByte, '7/9', 0, 'Mosaic-1 Set of Data Syntax I of CCITT Rec. T.101');
  AddItem(F94CharacterGSetOneByte, '7/10', 0, 'Serbocroatian and Slovenian Latin Alphabet');
  AddItem(F94CharacterGSetOneByte, '7/11', 0, 'Serbocroatian Cyrillic Alphabet');
  AddItem(F94CharacterGSetOneByte, '7/13', 0, 'Macedonian Cyrillic Alphabet');
//94-Character graphic character set with second Intermediate byte
// G0: ESC 2/8 2/1 F
// G1: ESC 2/9 2/1 F
// G2: ESC 2/10 2/1 F
// G3: ESC 2/11 2/1 F
  AddItem(F94CharacterGSetSecondByte, '4/0', 0, 'Greek Primary Set of CCITT');
  AddItem(F94CharacterGSetSecondByte, '4/1', 0, 'Character Set of Cuba');
  AddItem(F94CharacterGSetSecondByte, '4/2', 0, 'Invariant characters (82) of ISO/IEC 646');
  AddItem(F94CharacterGSetSecondByte, '4/3', 0, 'Variant of the ISO 7-bit coded character set for the Irish Gaelic language');
  AddItem(F94CharacterGSetSecondByte, '4/4', 0, 'Turkmen Alphabet');
  AddItem(F94CharacterGSetSecondByte, '4/5', 0, 'American National Standard Extended Latin Alphabet Coded Character Set for Bibliographic Use (ANSEL)');
  AddItem(F94CharacterGSetSecondByte, '4/6', 0, 'Turkmen character set for 8-bit codes');
//96-Character graphic character set
// G0: -
// G1: ESC 2/13 F
// G2: ESC 2/14 F
// G3: ESC 2/15 F
  AddItem(F96CharacterGSet, '4/1', 28591, 'Right-hand Part of Latin Alphabet No.1 ISO 8859/1, ECMA-94');
  AddItem(F96CharacterGSet, '4/2', 28592, 'Right-hand Part of Latin Alphabet No.2 ISO 8859/2, ECMA-94');
  AddItem(F96CharacterGSet, '4/3', 28593, 'Right-hand Part of Latin Alphabet No.3 ISO DIS 8859/3, ECMA-94');
  AddItem(F96CharacterGSet, '4/4', 28594, 'Right-hand Part of Latin Alphabet No.4 ISO DIS 8859/4, ECMA-94');
  AddItem(F96CharacterGSet, '4/0', 28595, 'Right-hand Part of the Latin/Cyrillic Alphabet ECMA-113 (Version of June 1986)');
  AddItem(F96CharacterGSet, '4/5', 0, 'General Purpose Supplementary Graphic Set CSA Standard Z 243.4-1985');
  AddItem(F96CharacterGSet, '4/6', 28597, 'Right-hand Part of the Latin/Greek Alphabet Standard ELOT 928, ECMA-118, ISO DIS 8859/7');
  AddItem(F96CharacterGSet, '4/7', 28596, 'Right-hand Part of the Latin/Arabic Alphabet ECMA-114, ISO 8859/6');
  AddItem(F96CharacterGSet, '7/13', 0, 'Supplementary Set of Mosaic Characters for CCITT Rec. 101, Data Syntax III');
  AddItem(F96CharacterGSet, '4/8', 0, 'Latin/Hebrew Alphabet Standard ECMA-121');
  AddItem(F96CharacterGSet, '4/9', 0, 'Right-hand Part for Czechoslovak Standard CSN 369103');
  AddItem(F96CharacterGSet, '4/10', 0, 'Supplementary Set of Latin Alphabetic and non-Alphabetic Graphic Characters');
  AddItem(F96CharacterGSet, '4/11', 0, 'Technical Set');
  AddItem(F96CharacterGSet, '4/12', 0, 'Cyrillic part of the Latin/Cyrillic Alphabet');
  AddItem(F96CharacterGSet, '4/13', 0, 'Right-hand part of Latin Alphabet No.5');
  AddItem(F96CharacterGSet, '4/14', 0, 'Residual Characters from ISO 6937-2 : 1983');
  AddItem(F96CharacterGSet, '4/15', 0, 'Basic Cyrillic Character Set for 8-bit Codes');
  AddItem(F96CharacterGSet, '5/0', 0, 'Supplementary set for Latin Alphabets No.1, No.2 and No.5');
  AddItem(F96CharacterGSet, '5/1', 0, 'Basic Box Drawings Set');
  AddItem(F96CharacterGSet, '5/2', 0, 'Supplementary Set ISO/IEC 6937 : 1992');
  AddItem(F96CharacterGSet, '5/6', 0, 'Supplementary Set for Latin Alphabet No. 6');
  AddItem(F96CharacterGSet, '5/8', 0, 'Sami (Lappish) Supplementary Set');
  AddItem(F96CharacterGSet, '5/3', 0, 'CCITT Hebrew Supplementary Set');
  AddItem(F96CharacterGSet, '5/4', 0, 'Thai Set');
  AddItem(F96CharacterGSet, '5/5', 0, 'Arabic/French/German Set');
  AddItem(F96CharacterGSet, '5/9', 0, 'Baltic Rim Supplementary Set');
  AddItem(F96CharacterGSet, '5/10', 0, 'Vietnamese Standard Code for Information Interchange');
  AddItem(F96CharacterGSet, '5/11', 0, 'Technical Character Set No.1: IEC Publication 1289');
  AddItem(F96CharacterGSet, '5/12', 0, 'Welsh variant of Latin Alphabet No. 1');
  AddItem(F96CharacterGSet, '5/13', 0, 'Sami supplementary Latin set');
  AddItem(F96CharacterGSet, '5/14', 0, 'Latin/Hebrew Alphabet');
  AddItem(F96CharacterGSet, '5/15', 0, 'Celtic Supplementary Latin Set');
  AddItem(F96CharacterGSet, '6/0', 0, 'Uralic Supplementary Cyrllic Set');
  AddItem(F96CharacterGSet, '6/1', 0, 'Volgaic Supplementary Cyrllic Set');
  AddItem(F96CharacterGSet, '6/2', 0, 'European Supplementary Latin Set ("Latin 9")');
  AddItem(F96CharacterGSet, '6/3', 0, 'Supplementary set for Latin-1 alternative with EURO SIGN');
  AddItem(F96CharacterGSet, '6/4', 0, 'Supplementary set for Latin-4 alternative with EURO SIGN');
  AddItem(F96CharacterGSet, '6/5', 0, 'Supplementary set for Latin-7 alternative with EURO SIGN');
  AddItem(F96CharacterGSet, '6/6', 0, 'Romanian Character Set for Information Interchange');
  AddItem(F96CharacterGSet, '6/7', 0, 'Ogham coded character set for information interchange');
  AddItem(F96CharacterGSet, '6/8', 0, 'Sami supplementary Latin set no 2');
  AddItem(F96CharacterGSet, '6/9', 0, 'Right-hand part of Latin/Greek alphabet');
  AddItem(F96CharacterGSet, '6/10', 0, 'Right-hand part of Latin/Greek alphabet');
// 94 Multiple byte graphic character sets
//G0: <ESC> 2/4 2/8 F
//G1: <ESC> 2/4 2/9 F
//G2: <ESC> 2/4 2/10 F
//G3: <ESC> 2/4 2/11 F
  AddItem(F94MultipleByteCharacterGSet, '4/0', 0, 'Japanese Character Set JISC C 6226-1978');
  AddItem(F94MultipleByteCharacterGSet, '4/1', 0, 'Chinese Character Set Chinese Standard GB 2312-80');
  AddItem(F94MultipleByteCharacterGSet, '4/2', 0, 'Japanese Character Set JIS C 6226-1983');
  AddItem(F94MultipleByteCharacterGSet, '4/3', 0, 'Korean Graphic Character Set, Korean Standard KSC 5601-1987');
  AddItem(F94MultipleByteCharacterGSet, '4/4', 0, 'Supplementary Japanese Graphic Character Set for Information Interchange');
  AddItem(F94MultipleByteCharacterGSet, '4/5', 0, 'CCITT Chinese Set');
  AddItem(F94MultipleByteCharacterGSet, '4/6', 0, 'Blissymbol Graphic Character Set');
  AddItem(F94MultipleByteCharacterGSet, '4/7', 0, 'Chinese Standard Interchange Code - Set 1, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/8', 0, 'Chinese Standard Interchange Code - Set 2, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/9', 0, 'Chinese Standard Interchange Code - Set 3, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/10', 0, 'Chinese Standard Interchange Code - Set 4, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/11', 0, 'Chinese Standard Interchange Code - Set 5, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/12', 0, 'Chinese Standard Interchange Code - Set 6, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/13', 0, 'Chinese Standard Interchange Code - Set 7, CNS 11643-1992');
  AddItem(F94MultipleByteCharacterGSet, '4/14', 0, 'DPRK Standard Korean Graphic Character Set for Information Interchange');
  AddItem(F94MultipleByteCharacterGSet, '4/15', 0, 'Japanese Graphic Character Set for Information Interchange --- Plane 1');
  AddItem(F94MultipleByteCharacterGSet, '5/0', 0, 'Japanese Graphic Character Set for Information Interchange --- Plane 2');
  AddItem(F94MultipleByteCharacterGSet, '5/1', 0, 'Japanese Graphic Character Set for Information Interchange, Plane 1 (Update of ISO-IR 228)');
// 96 Multiple byte graphic character sets
// G0: -
// G1:    <ESC> 2/4 2/13 <F>
// G2:    <ESC> 2/4 2/14 <F>
// G3:     <ESC> 2/4 2/15 <F>
// CompleteCode
//
// Coding systems with Standard return      ESC 2/5 F
// Coding Systems without Standard return   ESC 2/5 2/15 F
  AddItem(FCompleteCodeEscapeSeq, '4/1', 0, 'Syntax of the North American Videotex/Teletex Presentation Level Protocol (NAPLPS), CSA T 500-1983');
  AddItem(FCompleteCodeEscapeSeq, '4/3', 0, 'Data Syntax I of CCITT Rec.T.101');
  AddItem(FCompleteCodeEscapeSeq, '4/4', 0, 'Data Syntax II of CCITT Rec. T.101');
  AddItem(FCompleteCodeEscapeSeq, '4/5', 0, 'Photo-Videotex Data Syntax of CCITT Rec. T.101');
  AddItem(FCompleteCodeEscapeSeq, '4/6', 0, 'Audio Data Syntax of CCITT Rec. T.101');
  AddItem(FCompleteCodeEscapeSeq, '4/2', 0, 'UCS Transformation Format One (UTF-1)');
  AddItem(FCompleteCodeEscapeSeq, '4/8', 0, 'Videotex Enhanced Man Machine Interface (VEMMI) Data Syntax of ITU-T Rec. T.107');
  AddItem(FCompleteCodeEscapeSeq, '4/7', 65001, 'UTF-8 without implementation level');
  AddItem(FCompleteCodeEscapeSeq, '4/9', 65001, 'UTF-8');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/2', 0, 'Virtual Terminal service Transparent Set');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/0', 1200, 'ISO/IEC 10646:1993, UCS-2, Level 1');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/1', 1200, 'ISO/IEC 10646:1993, UCS-4, Level 1');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/3', 1200, 'ISO/IEC 10646:1993, UCS-2, Level 2');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/4', 1200, 'ISO/IEC 10646:1993, UCS-4, Level 2');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/5', 1200, 'ISO/IEC 10646:1993, UCS-2, Level 3');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/6', 1200, 'ISO/IEC 10646:1993, UCS-4, Level 3');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/7', 65001, 'UTF-8 Level 1');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/8', 65001, 'UTF-8 Level 2');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/9', 65001, 'UTF-8 Level 3'); // 1251????
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/10', 1200, 'UTF-16 Level 1');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/11', 1200, 'UTF-16 Level 2');
  AddItem(FCompleteCodeEscapeSeq, '2/15 4/12', 1200, 'UTF-16 Level 3');
end;

procedure CGM_Initialization;
begin
  FontAliasTable := nil;
  SetListTable := nil;
{$IFDEF SG_FIREMONKEY}
  InitKeyWordsBin;
  InitKeyWordsStr;
{$ELSE}
  KeyWordsBinSort := nil;
  KeyWordsStr := nil;
{$ENDIF}
end;

procedure CGM_Finalization;
begin
  FreeAndNil(SetListTable);
  FreeAndNil(FontAliasTable);
  FreeAndNil(KeyWordsBinSort);
  FreeObjectStringList(TStrings(KeyWordsStr));
end;


initialization
  CGM_Initialization;
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_CGM := RegisterClipboardFormat('SoftGold CGM Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_CGM, TsgCGMImage);
  TPicture.RegisterFileFormat('cgm', SCGMImage, TsgCGMImage);
{$IFDEF SG_XREF_EXTENDED}
  TsgDXFConverterAccess.RegisterXRefGraphicClass('cgm', TsgCGMImage);
{$ENDIF}

finalization
  CGM_Finalization;
  TPicture.UnRegisterGraphicClass(TsgCGMImage);
end.

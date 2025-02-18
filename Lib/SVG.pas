{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                  SVG files TGraphic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit SVG;
{$INCLUDE SGDXF.inc}

{$IFDEF SGFPC}
  {$DEFINE SG_INCLUDE_PNG}
{$ELSE}
  {$IFDEF SGDEL_2009}
      {$DEFINE SG_INCLUDE_PNG}
  {$ENDIF}
{$ENDIF}

//{$DEFINE TESTPARSER}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage, LazUtils,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  {$UNDEF SG_INCLUDE_PNG}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  {$IFDEF SG_OPENING_IN_THEADS}
  SyncObjs,
  {$ENDIF}
  DXFConv, CADImage, sgContext, sgConsts, sgLists,
  Math, sgFunction, TTF, sgUnicode, sgDefinitions, ExtData, sgXMLParser
  {$IFDEF SG_SVG_DEBUG}, ComCtrls {$ENDIF}{$IFDEF SG_DEBUG}, Dialogs {$ENDIF}
  {$IFDEF SGDEL_2009},  Character{$ENDIF}
  {$IFDEF SG_INCLUDE_PNG}{$IFNDEF SGFPC}, pngimage{$ENDIF}{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  ;

const
  SSVGImage = 'SVG image';
  cnstBlockDef = '*BLOCK_DEF';

var
  CF_SVG: Word;

type
  EsvgLoadError = class(Exception);
  EsvgParsedError = class(Exception);

  TsgSVGConverter = class(TsgDXFConverter)
  private
    FParser: TsgParser;
    FClipPolyPolylines: TsgStringList;
    FCodePage: Integer;
    FGradientStyles: TsgStringList;
    FGraphicsNames: TStringList;
    procedure InitStandartStyleByText;
  protected
    FMsg: string;
    procedure ClearBegin; override;
    procedure ClearEnd; override;
    function CreateDefs: TsgSVGContainer;
    function AddGraphicName(const AFileName: string): Boolean;
    procedure DelGraphicName(const AFileName: string);
    property ClipPolyPolylines: TsgStringList read FClipPolyPolylines;
    property GradientStyles: TsgStringList read FGradientStyles;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TsgSVGImage = class(TsgVectorImageWithSave)
  protected
    procedure ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD); override;
    function CreateConverter: TsgDXFConverter; override;
    function DoDraw(Entity: TsgDXFEntity): Integer; override;
    function DoFinish(Entity: TsgDXFEntity): Integer; override;
    procedure DrawBox(const ABox: TFRect; const AColor: TColor);
    function DrawGroup(Sender: TObject): Integer;
    procedure DoOnProgress(AStage: TProgressStage; ADone, ACount: Integer); override;
    procedure FinishGroup(Sender: TObject);
    function GetPolygonFillingMode: Integer; override;
    function GetEntityColors(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TsgEntColors; override;
    function IsSVGGroup(Entity: TsgDXFEntity): Boolean;
    function GetParserInternal: TsgParser; virtual;
    function IsRotatedText(const AText: TsgDXFText; AHeight: Single;
      const ADrawMatrix: TFMatrix): Boolean; override;
    function HasClip(AInsert: TsgDXFInsert; out AFilter: TObject): Boolean; override;
    procedure SetClip(AInsert: TsgDXFInsert; AFilter: TObject); override;
  public
    constructor Create; override;
    procedure LoadFromStream(S: TStream); override;
    function CanSaveToStream: Boolean; override;
    function SaveToStream(const AStream: TStream): Integer; override;
  end;

  TsgSVGMaterialImage = class(TsgSVGImage)
  private
    function GetParser: TsgParser;
    procedure SetParser(const AParser: TsgParser);
  protected
    function GetParserInternal: TsgParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Parser: TsgParser read GetParser;
  end;

  TsgSVGEntity = class(TsgSVGContainer)
  private
  protected
    procedure ClearBlock; override;
    function GetSVGType: TsvgEntity; override;
  public
    function AddEntity(const AEntity: TsgDXFEntity): Integer; override;
  end;

  TsgSVGGroup = class(TsgSVGContainer)
  private
    FClipBox: PFRect;
    FClipPoints: TsgObjectList;
    FViewBox: PsgViewBox;
    FFlag: Byte;
    procedure CreateViewBox;
    procedure CopyClipPoints(const AClipPoint: TsgObjectList);
  protected
    procedure Generate; override;
    function GetBox: TFRect; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetPoint: TFPoint; override;
    function GetSVGType: TsvgEntity; override;
    function GetUsedMatrix: TFMatrix; override;
    function GetViewBox: TsgViewBox;
    function HasViewBox: Boolean;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetViewBox(const AValue: TsgViewBox);
  public
    destructor Destroy; override;
    procedure AddClipCoords(const AX, AY: Double);
    procedure AddClipPoint(const APoint: TF2DPoint);
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure ClearClipPoints;
    procedure CreateClipPoints(const ALink: TsgObjectList);
    property ClipPoints: TsgObjectList read FClipPoints;
    property ViewBox: TsgViewBox read GetViewBox write SetViewBox;
  end;

  TsgSVGGroupDim = class(TsgSVGGroup)
  private
    FPoint1: PF2DPoint;
    FSize: PF2DPoint;
  protected
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetHeight: Double;
    function GetPoint1: TFPoint;
    function GetSize: TFPoint;
    function GetSVGType: TsvgEntity; override;
    function GetUsedMatrix: TFMatrix; override;
    function GetWidth: Double;
    function GetX1: Double;
    function GetY1: Double;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetHeight(const AValue: Double);
    procedure SetPoint1(const AValue: TFPoint);
    procedure SetSize(const AValue: TFPoint);
    procedure SetWidth(const AValue: Double);
    procedure SetX1(const AValue: Double);
    procedure SetY1(const AValue: Double);
  public
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Height: Double read GetHeight write SetHeight;
    property Point1: TFPoint read GetPoint1 write SetPoint1;
    property Size: TFPoint read GetSize write SetSize;
    property Width: Double read GetWidth write SetWidth;
    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
  end;

  TsgSVGLink = class(TsgSVGEntity)
  private
    FEref: TsgSVGContainer;
    FHref: string;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetHref: string;
    function GetSVGType: TsvgEntity; override;
    function GetUsingClass: TClass; virtual;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetHref(const AValue: string);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure FindEref(const AConverter: TsgDXFConverter);
    property Eref: TsgSVGContainer read FEref write FEref;
    property Href: string read GetHref write SetHref;
  end;

  TsgSVGPath = class(TsgSVGEntity)
  private
    FData: TList;
    FDataList: TsgSingleList;
    procedure AddPathSegment(const APathSegTypeByte: Byte);
    function GetPathSegType(const AKey: Char): Byte;
    function DataCopyToValues(const AValuesCount: Integer; var AIndex: Integer;
      var AValues: array of Single): Boolean;
    procedure ExportByte(const AData: TsgCADExtendedData;
      const ACode: SmallInt; var AIndex: Integer);
    procedure ExportDouble(const AData: TsgCADExtendedData;
      const ACode: SmallInt; var AIndex: Integer);
    procedure ExportPoint2D(const AData: TsgCADExtendedData;
      const ACode: SmallInt; var AIndex: Integer);
  protected
    procedure AddPathData(const AType: Byte; const AValues: array of Single);
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetPropertiesType: Byte; override;
    function GetSVGType: TsvgEntity; override;
    procedure ParseData(AList: TsgObjectList);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetData(const APathsSeg: string);
    procedure GetPolyPoints(const AList: TsgObjectList; AConverter: TsgDXFConverter);
    procedure AddArc(const AArcFlags: Byte; const AAxisAnlge, AX, AY,
      ARX, ARY: Single; const AAbsCoords: Boolean);
    procedure AddCloseFigure;
    procedure AddCubicBezier(const AX, AY, AX1, AY1, AX2, AY2: Single;
      const AAbsCoords: Boolean);
    procedure AddCubicBezierShort(const AX, AY, AX2, AY2: Single;
      const AAbsCoords: Boolean);
    procedure AddHorizLine(const AX: Single; const AAbsCoords: Boolean);
    procedure AddLine(const AX, AY: Single; const AAbsCoords: Boolean);
    procedure AddMove(const AX, AY: Single; const AAbsCoords: Boolean);
    procedure AddQuadraticBezier(const AX, AY, AX1, AY1: Single;
      const AAbsCoords: Boolean);
    procedure AddQuadraticBezierShort(const AX, AY: Single;
      const AAbsCoords: Boolean);
    procedure AddVertLine(const AY: Single; const AAbsCoords: Boolean);
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure ClearData;
    procedure CopyData(const APath: TsgSVGPath);
    function GetArcFlags(const ALargeArcFlag, ASweepFlag: Boolean): Byte;
    procedure SetExtData(const AData: TsgCADExtendedData); override;
  end;

  TsgSVGPoint = class(TsgSVGEntity)
  private
    FPoint1: TF2DPoint;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetPoint1: TFPoint;
    function GetPropertiesType: Byte; override;
    function GetSVGType: TsvgEntity; override;
    function GetX1: Double;
    function GetY1: Double;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetPoint1(const AValue: TFPoint);
    procedure SetX1(const AValue: Double);
    procedure SetY1(const AValue: Double);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Point1: TFPoint read GetPoint1 write SetPoint1;
    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
  end;

  TsgSVGPoly = class(TsgSVGEntity)
  private
    FPoints: TF2DPointList;
    FSVGFlags: Byte;
  protected
    procedure Generate; override;
    function GetClosed: Boolean;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetSVGType: TsvgEntity; override;
    procedure SetClosed(const AValue: Boolean);
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddPoint(const APoint: TFPoint);
    procedure AddPoint2D(const APoint: TF2DPoint);
    procedure AddPointCoords(const AX, AY: Double);
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure ClearPoints;
    property Closed: Boolean read GetClosed write SetClosed;
  end;

  TsgSVGCircle = class(TsgSVGPoint)
  private
    FRadius: Double;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetRadius: Double; virtual;
    function GetSVGType: TsvgEntity; override;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetRadius(const AValue: Double); virtual;
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Radius: Double read GetRadius write SetRadius;
  end;


  TsgSVGLine = class(TsgSVGPoint)
  private
    FPoint2: TF2DPoint;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetPoint2: TFPoint;
    function GetSVGType: TsvgEntity; override;
    function GetX2: Double;
    function GetY2: Double;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetPoint2(const AValue: TFPoint);
    procedure SetX2(const AValue: Double);
    procedure SetY2(const AValue: Double);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Point2: TFPoint read GetPoint2 write SetPoint2;
    property X2: Double read GetX2 write SetX2;
    property Y2: Double read GetY2 write SetY2;
  end;

  TsgSVGRect = class(TsgSVGPoint)
  private
    FRX: PDouble;
    FRY: PDouble;
    FSize: TF2DPoint;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetHeight: Double;
    function GetRX: Double;
    function GetRY: Double;
    function GetSize: TFPoint;
    function GetSVGType: TsvgEntity; override;
    function GetWidth: Double;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetHeight(const AValue: Double);
    procedure SetRX(const AValue: Double);
    procedure SetRY(const AValue: Double);
    procedure SetSize(const AValue: TFPoint);
    procedure SetWidth(const AValue: Double);
  public
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Height: Double read GetHeight write SetHeight;
    property RX: Double read GetRX write SetRX;
    property RY: Double read GetRY write SetRY;
    property Size: TFPoint read GetSize write SetSize;
    property Width: Double read GetWidth write SetWidth;
  end;

  TsgSVGText = class(TsgSVGPoint)
  private
    FDPoint: PF2DPoint;
    FText: string;
    FPoints1: TFPointList;
    FPointsD: TFPointList;
    procedure AddText(const AStr: string; const AP: TFPoint; const AEnts: TsgObjectList;
      var ABox: TFRect);
    function GetStyleByText: TsgDXFStyle;
    procedure SetList(var D: TFPointList; const S: TFPointList);
    procedure SetPointD2D(const AValue: TF2DPoint);
  protected
    procedure AddFill(const AEntity: TsgDXFEntity); override;
    procedure AddFills(AEnts: TsgObjectList); override;
    procedure Generate; override;
    function GetDX: Double;
    function GetDY: Double;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetFontHeight: Double;
    function GetPointD: TFPoint;
    function GetPropertiesType: Byte; override;
    function GetSVGType: TsvgEntity; override;
    function GetText: string;
    procedure SetDX(const AValue: Double);
    procedure SetDY(const AValue: Double);
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetFontHeight(const AValue: Double);
    procedure SetPointD(const AValue: TFPoint);
    procedure SetPoints1(const AValue: TFPointList);
    procedure SetPointsD(const AValue: TFPointList);
    procedure SetText(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property DX: Double read GetDX write SetDX;
    property DY: Double read GetDY write SetDY;
    property FontHeight: Double read GetFontHeight write SetFontHeight;
    property PointD: TFPoint read GetPointD write SetPointD;
    property Points1: TFPointList read FPoints1 write SetPoints1;
    property PointsD: TFPointList read FPointsD write SetPointsD;
    property Text: string read GetText Write SetText;
  end;

  TsgSVGTextPath = class(TsgSVGLink)
  private
    FText: string;
    procedure AddGlyphAsPoly(const APoints: array of TFPoint;
      const ACounts: array of Integer; const AP1, AP2: PFPoint);
    function GetStyleByText: TsgDXFStyle;
    procedure LoadGlyphs(const AGlyphs: TsgObjectList; AGlyphsBox: TList);
    procedure LoadPath(const APath: TList);
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetFontHeight: Double;
    function GetPropertiesType: Byte; override;
    function GetSVGType: TsvgEntity; override;
    function GetText: string;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetFontHeight(const AValue: Double);
    procedure SetText(const AValue: string);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property FontHeight: Double read GetFontHeight write SetFontHeight;
    property Text: string read GetText Write SetText;
  end;

  TsgSVGUse = class(TsgSVGLink)
  private
    FPoint1: TF2DPoint;
    FSize: TF2DPoint;
    procedure AddCopyAndSetProps(const ABlock: TsgDXFBlock;
      const AEntity, AOwner: TsgSVGContainer);
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetHeight: Double;
    function GetPoint1: TFPoint;
    function GetPropertiesType: Byte; override;
    function GetSize: TFPoint;
    function GetSVGType: TsvgEntity; override;
    function GetUsedMatrix: TFMatrix; override;
    function GetWidth: Double;
    function GetX1: Double;
    function GetY1: Double;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetHeight(const AValue: Double);
    procedure SetPoint1(const AValue: TFPoint);
    procedure SetSize(const AValue: TFPoint);
    procedure SetWidth(const AValue: Double);
    procedure SetX1(const AValue: Double);
    procedure SetY1(const AValue: Double);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property Height: Double read GetHeight write SetHeight;
    property Point1: TFPoint read GetPoint1 write SetPoint1;
    property Size: TFPoint read GetSize write SetSize;
    property Width: Double read GetWidth write SetWidth;
    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
  end;  

  TsgSVGEllipse = class(TsgSVGCircle)
  private
    FRY: Double;
  protected
    procedure Generate; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; override;
    function GetRadius: Double; override;
    function GetRX: Double;
    function GetRY: Double;
    function GetSVGType: TsvgEntity; override;
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); override;
    procedure SetRadius(const AValue: Double); override;
    procedure SetRX(const AValue: Double);
    procedure SetRY(const AValue: Double);
  public
    procedure AssignEntity(Source: TsgDXFEntity); override;
    property RX: Double read GetRX write SetRX;
    property RY: Double read GetRY write SetRY;
  end;

  TsgSVGImageEnt = class(TsgSVGRect)
  private
    FPicture: TsgDXFImageEnt;
    FPictureBlock: TsgDXFBlock;
  protected
    procedure AddFill(const AEntity: TsgDXFEntity); override;
    procedure AddFills(AEnts: TsgObjectList); override;
    procedure Generate; override;
    function GetSVGType: TsvgEntity; override;
    function GetImageEnt: TsgDXFImageEnt;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignEntity(Source: TsgDXFEntity); override;
    procedure ConvertToBitmap;
    procedure SetImage(const AImg: TObject);
  end;

function GetMatrixByParams(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
function GetMatrixByRotate(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
function GetMatrixByScale(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
function GetMatrixBySkew(const AParams: TsgStringList;
  const XY: Byte; var AMatrix: TFMatrix): Boolean;
function GetMatrixByTranslate(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
function LoadSVGEntity(const AEntity: TsgDXFEntity;
  const AConverter: TsgDXFConverter): Boolean;

//for CAD.DLL
function GetTextImportMode: Integer;
procedure SetTextImportMode(const AValue: Integer);
//

implementation

uses
{$IFNDEF SGFPC}
 {$IFNDEF SG_FIREMONKEY}
  {$IFDEF SGDEL_XE2}
  Vcl.Imaging.jpeg,
   {$ELSE}
  jpeg,
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
  sgZip, sgLines;

type
  TsgCADGradientPolygonAccess = class(TsgCADGradientPolygon);
  TsgClassOfContainer= class of TsgSVGContainer;
  TsgClassOfEntity = class of TsgDXFEntity;
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgDXFObjectEntityAccess = class(TsgDXFObjectEntity);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TsgDXFTextAccess = class(TsgDXFText);
  TsgSVGContainerAccess = class(TsgSVGContainer);
  TsgSVGInsertAccess = class(TsgSVGInsert);
  TsgSVGMTextAccess = class(TsgSVGInsert);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFStyleAccess = class(TsgDXFStyle);
  TsgTextGlyphAccess = class(TsgTextGlyph);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);

  TsgSVGPictureEnt = class(TsgDXFImageEnt)
  protected
    procedure ChangeGraphic; override;
  end;


  TsgClipPathPoints = TF2DPointList;

  TsgSVGNodeID = (niDefault, niNode, niA, niSVG, niSTYLE, niDEFS, niG, niSYMBOL,
    niCIRCLE, niELLIPSE, niLINE, niRECT, nIPOLYLINE, niPATH, niPOLYGON, niTEXT,
    niTSPAN, niTREF, niTEXTPATH, niUSE, niLGRADIENT, niRGRADIENT, niPATTERN,
    niIMAGE, niSWITCH, niClipPath, niSet, niStop);

  TsgPathSegType = (psUNKNOWN, psCLOSEPATH, psMOVETO_ABS, psMOVETO_REL,
    psLINETO_ABS, psLINETO_REL, psCURVETO_CUBIC_ABS, psCURVETO_CUBIC_REL,
    psCURVETO_QUADRATIC_ABS, psCURVETO_QUADRATIC_REL, psARC_ABS, psARC_REL,
    psLINETO_HORIZONTAL_ABS, psLINETO_HORIZONTAL_REL, psLINETO_VERTICAL_ABS,
    psLINETO_VERTICAL_REL, psCURVETO_CUBIC_SMOOTH_ABS,
    psCURVETO_CUBIC_SMOOTH_REL, psCURVETO_QUADRATIC_SMOOTH_ABS,
    psCURVETO_QUADRATIC_SMOOTH_REL);
  TsgPathSegTypes = set of TsgPathSegType;

  TsvgTextLinesCollection = class(TsgTextLinesCollection)
  end;

  TsvgPositions = class
  public
    Points1: TFPointList;
    States1: TsgIntegerList;
    PointsD: TFPointList;
    StatesD: TsgIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure AddPoint1(const AP: TFPoint; const AState: Integer = 3);
    procedure Clear;
    class procedure SetPointWithState(const AP: TFPoint; const ASt: Integer;
      var APoint: TFPoint; var AState: Integer);
    class procedure Union(const APointsIn: TFPointList;
      const AStateIn: TsgIntegerList; const APointsOut: TFPointList;
      const AStateOut: TsgIntegerList);
  end;

{$IFDEF SGFPC}
  TPngImage = TPortableNetworkGraphic;
{$ENDIF}

const
  cnst1cm:   Double = 960 / 25.4;
  cnst1inch: Double = 96;
  cnst1m:    Double = 96000 / 25.4;
  cnst1mm:   Double = 96 / 25.4;
  cnst1pc:   Double = 96 / 6;// 1 INCH / 6
  cnst1pt:   Double = 96 / 72;// 1 INCH / 72
  cnstHalfpt:Double = 0.5 * 96 / 72;// 0.5 INCH / 72
  cnst1px:   Double = 1;
  cnstAlignParameters: array [0..8] of string = ('xminymin', 'xmidymin',
    'xmaxymin', 'xminymid', 'xmidymid', 'xmaxymid', 'xminymax', 'xmidymax',
    'xmaxymax');
  cnstAlignParametersTypes: array [0..8] of TsgSVGPreserveAspectRatio =
    (altxMinyMin, altxMidyMin, altxMaxyMin, altxMinyMid, altxMidyMid,
     altxMaxyMid, altxMinyMax, altXmidyMax,altxMaxyMax);
  cnstBaseClassName: array [TsvgEntity] of string = ('', 'container', 'group',
    'groupdim',   'entity', 'point', 'circle', 'ellipse', 'line', 'link',
    'polyline', 'rect', 'path', 'text', 'textpath', 'use', 'image');
  cnstBevel = 'bevel';
  cnstBlockSeparator = '_';
  cnstStyleSeparator = '.';
  cnstUseStyleSeparator = False;
  cnstCollapse = 'collapse';
  cnstDefCodePage = CP_UTF8;
  cnstDefs = '*DEFS';
  cnstDefStyleGradient: Pointer = Pointer(1);
  cnstDisplayModes: array [0..16] of string = ('inline', 'block', 'list-item',
    'run-in', 'compact', 'marker', 'table', 'inline-table', 'table-row-group',
    'table-header-group', 'table-footer-group', 'table-row',
    'table-column-group', 'table-column', 'table-cell', 'table-caption', 'none');
  cnstED_ClipPointsFlag = 73;
  cnstED_ClipPolyPointsCount = 448;
  cnstED_HREF = 473;
  cnstED_Point2D1 = 16;
  cnstED_Point2D2 = 17;
  cnstED_Point2D3 = 18;
  cnstED_Point2D4 = 19;
  cnstED_Point3D1 = 14;
  cnstED_Point3D2 = 15;
  cnstED_PointFlags = 74;
  cnstED_PointParam = 130;
  cnstED_PointsCount1 = 444;
  cnstED_PointsCount2 = 445;
  cnstED_PointsCount3 = 446;
  cnstED_PointsCount4 = 447;
  cnstED_PolyFlags = 73;
  cnstED_R1 = 130;
  cnstED_R2 = 131;
  cnstED_SegType = 73;
  cnstED_TEXT = 474;
  cnstED_ViewBoxMode = 72;
  cnstEnd = 'end';
  cnstEvenodd = 'evenodd';
  cnstFontStretchs: array [0..10] of string =   ('normal', 'wider', 'narrower',
    'ultra-condensed', 'extra-condensed',    'condensed', 'semi-condensed',
     'semi-expanded', 'expanded',    'extra-expanded', 'ultra-expanded');
  cnstFontStyle: array [TmvFontStyle] of  string = ('bold', 'italic',
    'underline', 'strikeout', 'condensed', 'upward', 'downward');
  cnstFontStylePostFix: array [TmvFontStyle] of string = ('B', 'I', 'U', 'S',
    'C', 'U', 'D');
  cnstFontWeights: array [0..3] of record
      Name: string;
      Value: Double;
    end = ((Name: 'normal'; Value: 400),(Name: 'bold'; Value: 700),
      (Name: 'bolder'; Value: 900), (Name: 'lighter'; Value: 1000));
  cnstFRectZero: TFRect = ();
  cnstHiden = 'hidden';
  cnstHref = 'href';
  cnstLineCaps: array [0..3] of string = ('butt', 'flat',  'round', 'square');
  cnstMeetOrSliceParameters: array [0..1] of string = ('meet', 'slice');
  cnstMeetOrSliceParametersTypes: array [0..2] of TsgSVGPreserveAspectRatio =
   (altMeet, altSlice, altNone);
  cnstMiddle = 'middle';
  cnstNone = 'none';
  cnstNonScalingStroke = 'non-scaling-stroke';
  cnstObjectBoundingBox = 'objectboundingbox';
  cnstPathValuesCount: array [TsgPathSegType] of Byte =
   (0, 0, 2, 2, 2, 2, 6, 6, 4, 4, 6, 6, 1, 1, 1, 1, 4, 4, 2, 2);
  cnstQualityApproximation = 4;
  cnstRastrFormat: array [0..2] of record
    Ext: string;
    ClassType: TClass;
   end = ((Ext: 'BMP';  ClassType: TBitmap),
     (Ext: 'JPG';  ClassType: {$IFNDEF SG_FIREMONKEY}TJPEGImage{$ELSE}TBitmap{$ENDIF}),
     (Ext: 'JPEG';  ClassType: {$IFNDEF SG_FIREMONKEY}TJPEGImage{$ELSE}TBitmap{$ENDIF}));
  cnstRound = 'round';
  cnstSmallCaps = 'small-caps';
  cnstSquare = 'square';
  cnstSVGBlock = 'SVGBlock' + cnstBlockSeparator;
  cnstSVGEntityClasses: array [0..15] of record
    ID: TsvgEntity;
    ClassType: TClass;
  end = ((ID: svgContainer; ClassType: TsgSVGContainer),
    (ID: svgGroup; ClassType: TsgSVGGroup),
    (ID: svgGroupDim; ClassType: TsgSVGGroupDim),
    (ID: svgEntity; ClassType: TsgSVGEntity),
    (ID: svgPoint; ClassType: TsgSVGPoint),
    (ID: svgCircle; ClassType: TsgSVGCircle),
    (ID: svgEllipse; ClassType: TsgSVGEllipse),
    (ID: svgLine; ClassType: TsgSVGLine),
    (ID: svgLink; ClassType: TsgSVGLink),
    (ID: svgPolyline; ClassType: TsgSVGPoly),
    (ID: svgRect; ClassType: TsgSVGRect),
    (ID: svgPath; ClassType: TsgSVGPath),
    (ID: svgText; ClassType: TsgSVGText),
    (ID: svgTextPath; ClassType: TsgSVGTextPath),
    (ID: svgUse; ClassType: TsgSVGUse),
    (ID: svgImage; ClassType: TsgSVGImageEnt));
  cnstTextDecorations: array [0..3] of string = ('underline', 'overline',
    'line-through', 'blink');
  cnstTransformName: array [0..5] of string = ('matrix', 'rotate',
    'scale', 'skewx', 'skewy', 'translate');
  cnstURL = 'url';
  cnstXlink = 'xlink';

  cnstSVGNodeName: array [TsgSVGNodeID] of string = ('default', 'node', 'a',
    'svg', 'style', 'defs', 'g', 'symbol', 'circle', 'ellipse', 'line',
    'rect', 'polyline', 'path', 'polygon', 'text', 'tspan', 'tref',
    'textpath', 'use', 'lineargradient', 'radialgradient', 'pattern', 'image',
    'switch', 'clippath', 'set', 'stop');

  LocaleNameFamily: string = 'en';
  LocaleName: string = 'en-US';

var
  DefaultImageSize: TF2DPoint = (X: -1; Y: -1);
  KoefDeviceCaps: Double = 1;
{$IFDEF SG_FIREMONKEY}
  PictureInternal: TPicture = nil;
{$ENDIF}
  TextImportMode: Integer = 0;

{$IFDEF SG_OPENING_IN_THEADS}
  //StackCriticalSection: SyncObjs.TCriticalSection = nil;
{$ENDIF}
  svgStacks: TsgObjectList = nil;

type
  TsgStackItem = class
    Id: Int64;
    Objects: TStrings;
  end;

function sgGetCurrentThreadId: Int64;
begin
  Result := {$IFDEF SG_FIREMONKEY}TThread.CurrentThread.ThreadID{$ELSE}GetCurrentThreadId{$ENDIF};
end;

function GetStackItemById(const AId: Int64;
  const AIndex: PInteger = nil): TsgStackItem;
var
  I: Integer;
  vItem: TsgStackItem;
begin
  Result := nil;
  for I := 0 to svgStacks.Count - 1 do
  begin
    vItem := TsgStackItem(svgStacks[I]);
    if vItem.Id = AId then
    begin
      Result := vItem;
      if AIndex <> nil then
        AIndex^ := I;
      Break;
    end;
  end;
end;

function GetStack(const AId: Int64): TStrings;
var
  vItem: TsgStackItem;
begin
  vItem := GetStackItemById(AId);
  if not Assigned(vItem) then
  begin
    vItem := TsgStackItem.Create;
    vItem.Id := AId;
    vItem.Objects := CreateStringListSorted(dupIgnore);
    {$IFDEF SG_OPENING_IN_THEADS}
    MonitorEnter(svgStacks);
    {$ENDIF}
    try
      svgStacks.Add(vItem);
    finally
    {$IFDEF SG_OPENING_IN_THEADS}
    MonitorExit(svgStacks);
    {$ENDIF}
    end;
  end;
  Result := vItem.Objects;
end;

procedure FreeStackItem(const AItem: TsgStackItem);
begin
  FreeObjectStringList(AItem.Objects);
  AItem.Free;
end;

procedure FreeStack(const AId: Int64);
var
  I: Integer;
  vItem: TsgStackItem;
begin
  vItem := GetStackItemById(AId, @I);
  if Assigned(vItem) then
  begin
    {$IFDEF SG_OPENING_IN_THEADS}
    MonitorEnter(svgStacks);
    {$ENDIF}
    try
      svgStacks.Delete(I);
    finally
    {$IFDEF SG_OPENING_IN_THEADS}
    MonitorExit(svgStacks);
    {$ENDIF}
    end;
    FreeStackItem(vItem);
  end;
end;

procedure FreeFullStack;
var
  I: Integer;
  vItem: TsgStackItem;
begin
  for I := svgStacks.Count - 1 downto 0 do
  begin
    vItem := TsgStackItem(svgStacks[I]);
    svgStacks[I] := nil;
    FreeStackItem(vItem);
  end;
  FreeAndNil(svgStacks);
end;

// 0 - auto
// 1 - only text
// 2 - only poly
function GetTextImportMode: Integer;
begin
  Result := TextImportMode;
end;

procedure SetTextImportMode(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 2) then
    TextImportMode := AValue;
end;

procedure AddListItems(const ADestination, ASource: TsgEntitiesList;
  const ABeginToEnd: Boolean);
var
  I, vIndex: Integer;
begin
  vIndex := ADestination.Count;
  if ABeginToEnd then
    ADestination.AppendDynArray(ASource)
  else
  begin
    ADestination.Count := ADestination.Count + ASource.Count;
    for I := ASource.Count - 1 downto 0 do
    begin
      ADestination.List[vIndex] := ASource.List[I];
      Inc(vIndex);
    end;
  end;
end;

function BuildFullText(const AText: string): string;
var
  vStrings: TsgStringList;
begin
  Result := AText;
  vStrings := TsgStringList.Create;
  try
    SetStringsLineBreak(vStrings, Result, cnstLineBreakText);
    vStrings.Text := Result;
    if vStrings.Count > 1 then
      Result := GetFullText(vStrings);
  finally
    vStrings.Free;
  end;
end;

//this calculations should occur only to such type
function CalcArcCenter(const AStartX, AStartY, AEndX, AEndY, ARadiusX, ARadiusY,
  AXAxisRotation: Extended; const AArcFlags: Byte; var ACenter: TFPoint): Boolean;
var
  vSin, vCos, vMiddleX, vMiddleY, vSubDiv2X, vSubDiv2Y, vX, vY: Extended;
  vRadiusXY, vRadiusXY1, vRadiusXY2, vXTmp, vYTmp: Extended;
  vSign: Integer;
begin
  Result := False;
  vSubDiv2X := (AStartX - AEndX) * 0.5;
  vSubDiv2Y := (AStartY - AEndY) * 0.5;
  SinCos(AXAxisRotation * fPiDividedBy180, vSin, vCos);
  vX := vCos * vSubDiv2X + vSin * vSubDiv2Y;
  vY :=-vSin * vSubDiv2X + vCos * vSubDiv2Y;
  vSign := IfThen((AArcFlags = 1) or (AArcFlags = 2), +1, -1);
  vRadiusXY1 := Sqr(ARadiusX * ARadiusY) - Sqr(ARadiusX * vY) -
    Sqr(ARadiusY * vX);
  vRadiusXY2 := Sqr(ARadiusX * vY) + Sqr(ARadiusY * vX);
  if not sgIsZero(vRadiusXY2) then
  begin
    vRadiusXY := vSign * sgSqrt(vRadiusXY1 / vRadiusXY2);
    vXTmp := vRadiusXY * ( ARadiusX * vY / ARadiusY);
    vYTmp := vRadiusXY * (-ARadiusY * vX / ARadiusX);
    vMiddleX := (AStartX + AEndX) * 0.5;
    vMiddleY := (AStartY + AEndY) * 0.5;
    ACenter.X := vCos * vXTmp - vSin * vYTmp + vMiddleX;
    ACenter.Y := vSin * vXTmp + vCos * vYTmp + vMiddleY;
    ACenter.Z := 0;
    Result := True;
  end;
end;

function CalcMatrix(const AName:  string; const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
var
  vIndex: Integer;
begin
  vIndex := 0;
  while (vIndex < 6) and (AName <> cnstTransformName[vIndex]) do
    Inc(vIndex);
  case vIndex of
    0:  Result := GetMatrixByParams(AParams, AMatrix);
    1:  Result := GetMatrixByRotate(AParams, AMatrix);
    2:  Result := GetMatrixByScale(AParams, AMatrix);
    3:  Result := GetMatrixBySkew(AParams, 0, AMatrix);
    4:  Result := GetMatrixBySkew(AParams, 1, AMatrix);
    5:  Result := GetMatrixByTranslate(AParams, AMatrix);
  else
    Result := False;
  end;
end;


function CutMarks(const AStr: string; const AMark: Char): string;
begin
  Result := AStr;
  while (Length(Result) > 0) and (Result[1] = cnstQuotationMark) do
  begin
    Delete(Result, 1, 1);
    Result := Trim(Result);
  end;
  while (Length(Result) > 0) and (Result[Length(Result)] = cnstQuotationMark) do
  begin
    Delete(Result, Length(Result), 1);
    Result := Trim(Result);
  end;
end;

function IsBadPathType(const AType: Integer): Boolean;
begin
  Result := False;
  if (AType <= Integer(Low(TsgPathSegType))) or (AType > Integer(High(TsgPathSegType))) then
    Result := True;
end;

function GetBlockName(const AName: string): string;
var
  vIndex, vLength: Integer;
begin
  Result := '';
  vLength := Length(AName);
  if vLength > 0 then
  begin
    if AName[1] = '#' then
      vIndex := 2
    else
      vIndex := 1;
    Result := Copy(AName, vIndex, vLength);
  end;
end;

function GetBulgeByPoints(const AP1, AP2: TFPoint; const AR: Double): Double;
var
  vD, vR: Double;
begin
  vD := DistanceFPoint(AP1, AP2) * 0.5;
  vR := Abs(AR);
  if (Abs(vD) > fAccuracy) and (vD < vR - fAccuracy) then
    Result := (vR - Sqrt(Sqr(AR) - Sqr(vD))) / vD
  else
    Result := 0;
end;

function GetClassByFileExt(const AStr: string): TClass;
{$IFDEF  SG_FIREMONKEY}
begin
  if not Assigned(PictureInternal) then
    PictureInternal := TPicture.Create;
  Result := PictureInternal.GetClassByExt(AStr);
{$ELSE}
var
  I: Integer;
  vFileEx: string;
begin
  Result := nil;
  if Length(AStr) > 0 then
  begin
    vFileEx := AnsiUpperCase(AStr);
    if vFileEx[1] = '.' then
      Delete(vFileEx, 1, 1);
    for I := Low(cnstRastrFormat) to High(cnstRastrFormat) do
      if vFileEx = cnstRastrFormat[I].Ext then
      begin
        Result := cnstRastrFormat[I].ClassType;
        Break;
      end;
  end;
{$ENDIF}
end;

function GetClassByStream(AFileExt: string;
  const AStream: TStream): TClass;
var
  vPicture: TPicture;
  vTempDir, vFileName: string;
  vStreamMem: TMemoryStream;
begin
  Result := nil;
  vTempDir := GetTempDir;
  if (AStream = nil) or (AFileExt = '') or (vTempDir = '') then Exit;
  if AFileExt[1] = '.' then
    Delete(AFileExt, 1, 1);
  vFileName := vTempDir + sTempFileName + AFileExt;
  vPicture := TPicture.Create;
  try
    vStreamMem := TMemoryStream.Create;
    try
      AStream.Position := 0;
      try
        vStreamMem.CopyFrom(AStream, 0);
        vStreamMem.SaveToFile(vFileName);
        vPicture.LoadFromFile(vFileName);
      except
        vStreamMem.Free;
        vPicture.Free;
        vStreamMem := nil;
        vPicture := nil;
        DeleteFile(vFileName);
        Exit;
      end;
      DeleteFile(vFileName);
    finally
      vStreamMem.Free;
    end;
    Result := vPicture.Graphic.ClassType;
  finally
    vPicture.Free;
  end;
end;

function GetDisplayMode(const AMade: string): Byte;
begin
  Result := IndexOfStrings(AnsiLowerCase(Trim(AMade)), cnstDisplayModes);
end;

function GetDivKoef(const AValue1, AValue2: Double): Double;
begin
  if not sgIsZero(AValue2) then
    Result := Abs(AValue1 / AValue2)
  else
    Result := 1;
end;

function GetEntityId(const AEntity: TsgDXFEntity): string;
begin
  Result := IntToHex(TsgNativeInt(AEntity), 0);
end;

function GetEntityInBlock(const AID: string;
  const ABlock: TsgDXFBlock): TsgDXFEntity;
var
  I: Integer;
  vEntity:  TsgDXFEntity;
begin
  Result := nil;
  if ABlock <> nil then
  begin
    for I := 0 to ABlock.Count - 1 do
    begin
      vEntity := ABlock[I];
      if vEntity is TsgSVGGroup then
      begin
        if TsgSVGGroup(vEntity).ID = AID then
          Result := vEntity
        else
          Result := GetEntityInBlock(AID, TsgSVGGroup(vEntity).Block)
      end
      else
      begin
        if (vEntity is TsgSVGEntity) and (TsgSVGEntity(vEntity).ID = AID) then
          Result := vEntity;
      end;
      if Result <> nil then
        Break;
    end;
  end;
end;

function GetEntityLineWeightScaled(const AEntity: TsgDXFEntity;
  const AIns: TsgDXFInsert): Boolean;
var
  vEntLineWeight: TsgFloat;
begin
  Result := TsgDXFEntityAccess(AEntity).LineWeightScaled;
  vEntLineWeight := AEntity.LineWeight;
  if vEntLineWeight < -fAccuracy then
  begin
    if vEntLineWeight = fLineWeightByBlock then
    begin
      if AIns <> nil then
        Result := GetEntityLineWeightScaled(AIns, AIns.OwnerInsert)
      else
        Result := False;
    end
    else
      Result := False;
  end;
end;

function GetEntityLineParams(const AEntity: TsgDXFEntity;
  const AIns: TsgDXFInsert; var ALineWeightScaled: Boolean;
  var ALineCap, ALineJoin: Byte; var AMiterLimit: Single): Boolean;
begin
  if (AEntity.EntType = ceInsert) and (AEntity is TsgSVGContainer) then
  begin
    Result := True;
    ALineCap := TsgSVGContainer(AEntity).StrokeLineCap;
    ALineJoin := TsgSVGContainer(AEntity).StrokeLineJoin;
    AMiterLimit := TsgSVGContainer(AEntity).StrokeMiterlimit;
    ALineWeightScaled := TsgSVGContainer(AEntity).VectorEffect = 0;
  end
  else
  begin
    if AIns <> nil then
      Result := GetEntityLineParams(AIns, AIns.OwnerInsert, ALineWeightScaled,
        ALineCap, ALineJoin, AMiterLimit)
    else
    begin
      Result := False;
      ALineCap := cnstDefStrokeLinecap;
      ALineJoin := cnstDefStrokeLinejoin;
      AMiterLimit := cnstDefStrokeMiterlimit;
      ALineWeightScaled := True;
    end;
  end;
end;

function GetEntityVisible(const AVisible: string): Byte;
var
  vVisible: string;
begin
  Result := 0;
  vVisible := AnsiLowerCase(Trim(AVisible));
  if vVisible = cnstHiden then
    Result := 1
  else
    if vVisible = cnstCollapse then
      Result := 2;
end;

function GetF2DPointCoord(const APPoint: PF2DPoint; const AType: Byte): Double;
begin
  if APPoint <> nil then
    Result := APPoint^.V[AType]
  else
    Result := 0;
end;

function GetFillColorEx(const AColor: TColor): TsgFillColor;
begin
  Result.IsStyle := False;
  Result.Color := AColor;
end;

function GetFillRule(const ARule: string): Byte;
begin
  Result := Byte(AnsiLowerCase(Trim(ARule)) = cnstEvenodd);
end;

function GetFontNameFromFamily(const AFamily: string): string;
var
  vFamilies: TsgStringList;
begin
  Result := AFamily;
  vFamilies := TsgStringList.Create;
  try
    vFamilies.LineBreak := cnstComma;
    vFamilies.Text := AFamily;
    DeleteEmptyStrings(vFamilies);
    if vFamilies.Count > 0 then
    begin
      Result := CutMarks(Trim(vFamilies[0]), cnstQuotationMark);
      sgXMLParser.ReplaceWebOrAnsiCode(True, Result);
    end;
  finally
    vFamilies.Free;
  end;
end;

function GetFontStyles(const AStyles: string): TmvFontStyles;
var
  I: TmvFontStyle;
  vStyles: string;
begin
  Result := [];
  vStyles := AnsiLowerCase(AStyles);
  for I := Low(TmvFontStyle) to High(TmvFontStyle) do
  begin
    if AnsiPos(cnstFontStyle[I], vStyles) > 0 then
      Result := Result + [I];
  end;
end;

function GetFontStylesDecoration(const ADecor: Byte): TmvFontStyles;
begin
  Result := [];
  if ADecor and 1 <> 0 then
    Result := Result + [fmUnderline];
  if ADecor and 2 <> 0 then
    Result := Result + [fmStrikeOut];
  if ADecor and 4 <> 0 then
    Result := Result + [fmStrikeOut];
end;

function GetFontStretch(const AFontStretch: string): Byte;
begin
  Result := IndexOfStrings(AnsiLowerCase(Trim(AFontStretch)), cnstFontStretchs);
end;

function GetFontVariant(const ARule: string): Byte;
begin
  Result := Byte(AnsiLowerCase(Trim(ARule)) = cnstSmallCaps);
end;

function GetFontWeight(const AWeight: string): Double;
var
  I, vHigh: Integer;
  vWeight: string;
begin
  vWeight := AnsiLowerCase(Trim(AWeight));
  I := Low(cnstFontWeights);
  vHigh := High(cnstFontWeights);
  while (I <= vHigh) and (vWeight <> cnstFontWeights[I].Name) do
    Inc(I);
  if I <= vHigh then
    Result := cnstFontWeights[I].Value
  else
    Result := StrToVal(vWeight)
end;

function GetFPointFromP2D(const APPoint: PF2DPoint): TFPoint;
begin
  if APPoint = nil then
    Result := cnstFPointZero
  else
    Result := MakeFPointFrom2D(APPoint^);
end;

function GetGraphic(const AFileExt: string; const AStream: TStream): TGraphic;
var
  vClassType: TClass;
begin
  Result := nil;
  if AStream <> nil then
  begin
    vClassType := GetClassByFileExt(AFileExt);
    if vClassType = nil then
      vClassType := GetClassByStream(AFileExt, AStream);
    if vClassType <> nil then
    begin
      Result := TGraphicClass(vClassType).Create;
      AStream.Position := 0;
      Result.LoadFromStream(AStream);
    end;
  end;
end;

function GetLineCap(const ALineCap: string): Byte;
var
  vLineCap: string;
begin
  Result := 0;
  vLineCap := AnsiLowerCase(Trim(ALineCap));
  if vLineCap = cnstLineCaps[2] then
    Result := 1
  else
    if vLineCap = cnstLineCaps[3] then
      Result := 2;
end;

function GetLineJoin(const ALineJoin: string): Byte;
var
  vLineJoin: string;
begin
  Result := 0;
  vLineJoin := AnsiLowerCase(Trim(ALineJoin));
  if vLineJoin = cnstRound then
    Result := 1
  else
    if vLineJoin = cnstBevel then
      Result := 2;
end;

function GetMatrixByParams(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
begin
  Result := AParams.Count = 6;
  if Result then
    AMatrix := FMat2DByParams(StrToVal(AParams[0]),  StrToVal(AParams[1]),
      StrToVal(AParams[2]), StrToVal(AParams[3]), StrToVal(AParams[4]),
      StrToVal(AParams[5]));
end;

function GetMatrixByRotate(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
begin
  Result := False;
  case AParams.Count of
    1:
      begin
        AMatrix := FMatByAngle(StrToVal(AParams[0]));
        Result := True;
      end;
    3:
      begin
        AMatrix := FMatByTranslate(StrToVal(AParams[1]), StrToVal(AParams[2]), 0);
        AMatrix := FMatXMat2D(AMatrix, FMatByAngle(StrToVal(AParams[0])));
        AMatrix := FMatXMat2D(AMatrix, FMatByTranslate(-1 * StrToVal(AParams[1]),
          -1 * StrToVal(AParams[2]), 0));
        Result := True;
      end;
  end;
end;

function GetMatrixByScale(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
var
  vScale: Double;
begin
  Result := False;
  case AParams.Count of
    1:
      begin
        vScale := StrToVal(AParams[0]);
        AMatrix := FMatByScale(vScale);
        Result := True;
      end;
    2:
      begin
        AMatrix := FMatByScales(StrToVal(AParams[0]),  StrToVal(AParams[1]), 1);
        Result := True;
      end;
  end;
end;

function GetMatrixBySkew(const AParams: TsgStringList;
  const XY: Byte; var AMatrix: TFMatrix): Boolean;
var
  vSkew: TF2DPoint;
begin
  Result := AParams.Count = 1;
  begin
    vSkew := cnstF2DPointZero;
    vSkew.V[XY] := StrToVal(AParams[0]);
    AMatrix := FMatBySkew(vSkew.X, vSkew.Y);
  end;
end;

function GetMatrixByTranslate(const AParams: TsgStringList;
  var AMatrix: TFMatrix): Boolean;
begin
  Result := False;
  case AParams.Count of
    1:
      begin
        AMatrix := FMatByTranslate(StrToVal(AParams[0]), 0, 0);
        Result := True;
      end;
    2:
      begin
        AMatrix := FMatByTranslate(StrToVal(AParams[0]),
          StrToVal(AParams[1]), 0);
        Result := True;
      end;
  end;
end;

function GetMatrixByViewBox(const AViewBox: PsgViewBox; const APoint,
  ASize: PF2DPoint; const ABox: TFRect): TFMatrix;
var
  vPoint, vScale: TFPoint;
  vScaleX, vScaleY: Double;
  vMode: TsgSVGPreserveAspectRatios;
begin
  if not IsBadRect(ABox) then
  begin
    if APoint <> nil then
      vPoint := MakeFPoint(APoint^.X, APoint^.Y, 0)
    else
      vPoint := cnstFPointZero;
    if ASize <> nil then
    begin
      vMode := [];
      if AViewBox <> nil then
      begin
        vScaleX := GetDivKoef(ASize.X, AViewBox^.Width);
        vScaleY := GetDivKoef(ASize.Y, AViewBox^.Height);
        vMode := AViewBox^.Mode;
      end
      else
      begin
        vScaleX := GetDivKoef(ASize.X, ABox.Right - ABox.Left);
        vScaleY := GetDivKoef(ASize.Y, ABox.Top - ABox.Bottom);
      end;
      if vMode <> [altNone] then//none
      begin
        if Abs(vScaleX) < Abs(vScaleY) then
          vScaleY := vScaleX
        else
          vScaleX := vScaleY;
      end;
      vScale := MakeFPoint(vScaleX, vScaleY, 1);
    end
    else
      vScale := cnstFPointSingle;
    Result := StdMat(vScale, vPoint);
  end
  else
    Result := cnstIdentityMat;
end;

function GetPolylineVertexes(const APoly: TsgDXFPolyline): TsgEntitiesList;
begin
  Result := TsgDXFPolylineAccess(APoly).List;
end;

function GetSegType(const AType: TsgPathSegType;
  const AAbsCoords: Boolean): Byte;
begin
  Result := Byte(AType) + Byte(not AAbsCoords);
end;

function GetPreserveAspectRatio(const AValue: string): TsgSVGPreserveAspectRatios;
var
  vValue: string;
  vAlignIndex, vMeetIndex: Integer;
begin
  vValue := AnsiLowerCase(Trim(AValue));
  if Length(vValue) > 0 then
  begin
    Result := [];
    if vValue = cnstNone then
      Result := [altNone]
    else
    begin
      vAlignIndex := IndexOfStrings(vValue, cnstAlignParameters, @IsHasSubstring);
      if vAlignIndex < 0 then
        vAlignIndex := cnstDefAlingIndex;
      Include(Result, cnstAlignParametersTypes[vAlignIndex]);
      vMeetIndex := IndexOfStrings(vValue, cnstMeetOrSliceParameters,
        @IsHasSubstring);
      if vMeetIndex < 0 then
        vMeetIndex := cnstDefMeetIndex;
      Include(Result, cnstMeetOrSliceParametersTypes[vMeetIndex]);
    end;
  end
  else
    Result := cnstDefViewBox.Mode;
end;

function GetTextAnchor(const ATextAnchor: string): Byte;
var
  vTextAnchor: string;
begin
  Result := 0;
  vTextAnchor := AnsiLowerCase(Trim(ATextAnchor));
  if vTextAnchor = cnstMiddle then
    Result := 1
  else
    if vTextAnchor = cnstEnd then
      Result := 2;
end;

function GetTextDecoration(const ATextDecoration: string): Byte;
var
  I: Integer;
  vTextDecoration: string;
begin
  Result := 0;
  vTextDecoration := AnsiLowerCase(Trim(ATextDecoration));
  if vTextDecoration <> cnstNone then
  begin
    for I := 0 to 3 do
      if AnsiPos(cnstTextDecorations[I], vTextDecoration) > 0 then
        Result := Result or (1 shl I);
  end;
end;

function GetTextHeigthBySize(const AFontName: string;
  const ASize: Double): Double;
//Font.Height = -Font.Size * Font.PixelsPerInch / 72
{$IFDEF SG_FIREMONKEY}
begin
  Result := Abs(ASize * cnstHalfpt);
end;
{$ELSE}
var
  vFont: TFont;
  vScale: Double;
begin
  vScale := 1;
  if ASize < 10 then
    vScale := 20;
  vFont := TFont.Create;
  try
    vFont.Name := AFontName;
    vFont.Size := Round(ASize * vScale);
    Result := Abs(vFont.Height * KoefDeviceCaps * 0.5 / vScale);
  finally
    vFont.Free;
  end;
end;
{$ENDIF}

function GetTextSizeByHeigth(const AFontName: string;
  const AHeigth: Double): Double;
{$IFDEF SG_FIREMONKEY}
begin
  Result := Abs(AHeigth / cnstHalfpt);
end;
{$ELSE}
var
  vFont: TFont;
begin
  vFont := TFont.Create;
  try
    vFont.Name := AFontName;
    vFont.Height := Round(2 * AHeigth / KoefDeviceCaps);
    Result := Abs(vFont.Size);
  finally
    vFont.Free;
  end;
end;
{$ENDIF}

function GetTramsformMatrix(const AStr: string): TFMatrix;
var
  vPos1, vPos2, vPos3, vLen: Integer;
  vStr, vName, vValue: string;
  vParams: TsgStringList;
  vMatrix: TFMatrix;
begin
  Result := cnstIdentityMat;
  if Length(AStr) > 0 then
  begin
    vParams := TsgStringList.Create;
    try
      vParams.LineBreak := cnstLineBreak;
      vPos1 := 1;
      vPos2 := StringScan(')', AStr, vPos1);
      vLen := Length(AStr);
      while vPos2 >= 0 do
      begin
        vStr := Copy(AStr, vPos1, vPos2 - vPos1 + 1);
        if Length(vStr) > 0 then
        begin
          vPos3 := StringScan('(', vStr, 1);
          if vPos3 >= 0 then
          begin
            vName := AnsiLowerCase(Trim(Copy(vStr, 1, vPos3 - 1)));
            vValue := Trim(Copy(vStr, vPos3 + 1, Length(vStr) - vPos3 - 1));
            SetStringsLineBreak(vParams, vValue);
            SetStringsText(vParams, Trim(vValue));
            if CalcMatrix(vName, vParams, vMatrix) then
              Result := FMatXMat2D(Result, vMatrix);
          end;
          vPos1 := vPos2 + 1;
          if vPos1 < vLen then
            vPos2 := StringScan(')', AStr, vPos1)
          else
            Break;
        end
        else
          Break;
      end;
    finally
      vParams.Free;
    end;
  end;
end;

function GetURIName(const AFill: string): string;
var
  vIndex, vPos1, vPos2: Integer;
begin
  Result := '';
  if Length(AFill) > 0 then
  begin
    vIndex := AnsiPos(cnstURL, AFill);
    if vIndex > 0 then
    begin
      vPos1 := StringScan('(', AFill, vIndex + 2);
      if vPos1 > 0 then
      begin
        vPos2 := StringScan(')', AFill, vPos1 + 1);
        if vPos2 > 0 then
        begin
          Result := Trim(Copy(AFill, vPos1 + 1, vPos2 - vPos1 - 1));
          if (Length(Result) > 0) and (Result[1] = '#') then
            Delete(Result, 1, 1);
        end;
      end;
    end;
  end;
end;

function GetVectorEffect(const AValue: string): Byte;
begin
  Result := Byte(AnsiLowerCase(Trim(AValue)) = cnstNonScalingStroke);
end;

function GetViewBox(const AStr: string): TsgViewBox;
var
  vParams: TsgStringList;
begin
  Result := cnstViewBoxZero;
  if Length(AStr) > 0 then
  begin
    vParams := TsgStringList.Create;
    try
      vParams.LineBreak := cnstLineBreakDef;
      SetStringsLineBreak(vParams, AStr, cnstLineBreakDef);
      SetStringsText(vParams, Trim(AStr));
      case vParams.Count of
        2:  Result := MakeViewBox(0, 0, StrToVal(vParams[0]),
              StrToVal(vParams[1]));
        4:  Result := MakeViewBox(StrToVal(vParams[0]), StrToVal(vParams[1]),
              StrToVal(vParams[2]), StrToVal(vParams[3]));
      end;
    finally
      vParams.Free;
    end;
  end;
end;

procedure InitParams;
var
  vScreen: TPoint;
begin
  if DefaultImageSize.X >= 0 then
    Exit;
  DefaultImageSize.X := 1024;
  DefaultImageSize.Y := 768;
  try
{$IFNDEF SG_FIREMONKEY}
    KoefDeviceCaps := 96 / GetDeviceCap(LOGPIXELSY);
{$ELSE}
    KoefDeviceCaps := sgConsts.KoefDeviceCaps;
    if KoefDeviceCaps < 0 then
      KoefDeviceCaps := 96 / GetPixelsPerInch;
{$ENDIF}
  except
    KoefDeviceCaps := 1;
  end;
  try
    vScreen := sgConsts.ScreenResolution;
    if vScreen.X < 0 then
      vScreen := GetScreenResolution;
    DefaultImageSize.X := vScreen.X;
    DefaultImageSize.Y := vScreen.Y;
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
  DefaultImageSize.X := DefaultImageSize.X * cnst1px;
  DefaultImageSize.Y := DefaultImageSize.Y * cnst1px;
end;

function InitLocales: Integer;
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF SGDEL_XE}
  function GetLocaleDataW(ID: LCID; Flag: DWORD): string;
  var
    Buffer: array[0..1023] of WideChar;
  begin
    Buffer[0] := #0;
    GetLocaleInfoW(ID, Flag, Buffer, Length(Buffer));
    Result := Buffer;
  end;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF SG_DELPHI_VCL}
  Result := Languages.IndexOf(SysLocale.DefaultLCID);
{$ELSE}
  Result := 0;
{$ENDIF}
  if Result >= 0 then
  begin
{$IFNDEF SGDEL_XE}
    LocaleName := Format('%s-%s', [GetLocaleDataW(SysLocale.DefaultLCID, LOCALE_SISO639LANGNAME),
      GetLocaleDataW(SysLocale.DefaultLCID, LOCALE_SISO3166CTRYNAME)]);
{$ELSE}
    LocaleName := Languages.LocaleName[Result];
{$ENDIF}
    LocaleNameFamily := Copy(LocaleName, 1, Pos('-', LocaleName) - 1);
  end;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

procedure LoadContainer(const AContainer: TsgSVGContainer;
  const AConverter: TsgDXFConverter);
begin
  AConverter.Loads(AContainer);
  AContainer.IsLoaded := True;
end;

function LoadSVGEntity(const AEntity: TsgDXFEntity;
  const AConverter: TsgDXFConverter): Boolean;
var
  I: Integer;
  vContainer: TsgSVGContainer;
  vEntLink: TsgSVGLink;
begin
  if AEntity.EntClass.EG = gtSVG then
  begin
    vContainer := TsgSVGContainer(AEntity);
    Result := vContainer.IsLoaded;
    if not Result then
    begin
      if (vContainer.Block = nil) and (Length(vContainer.BlockName) > 0) then
        vContainer.Block := AConverter.BlockByName(vContainer.BlockName);
      case vContainer.SVGType of
        svgContainer, svgGroup, svgGroupDim:
          begin
            if vContainer.Block <> nil then
            begin
              Result := True;
              for I := 0 to vContainer.Block.Count - 1 do
                if not LoadSVGEntity(vContainer.Block[I], AConverter) then
                  Result := False;
              LoadContainer(vContainer, AConverter);
            end;
          end;
        svgEntity, svgPoint, svgCircle, svgEllipse, svgLine, svgPolyline,
          svgRect, svgPath, svgText, svgImage:
          begin
            LoadContainer(vContainer, AConverter);
            Result := True;
          end;
        svgLink, svgTextPath, svgUse:
          begin
            vEntLink := TsgSVGLink(vContainer);
            if vEntLink.Eref = nil then
              vEntLink.FindEref(AConverter);
            if (vEntLink.Eref <> nil) and LoadSVGEntity(vEntLink.Eref, AConverter) then
            begin
              LoadContainer(vContainer, AConverter);
              Result := True;
            end;
          end;
      end;
    end;
  end
  else
  begin
    AConverter.Loads(AEntity);
    Result := True;
  end;
end;

procedure RegisterClasses;
var
  I: Integer;
  vGTN: string;
begin
  vGTN := cnstGroupTypeName[gtSVG];
  for I := Low(cnstSVGEntityClasses) to High(cnstSVGEntityClasses) do
    RegisterEntityClass(vGTN, Integer(cnstSVGEntityClasses[I].ID),
      cnstSVGEntityClasses[I].ClassType);
end;

procedure ReplaceASCICode(var AStr: string);
begin
  AStr := sgXMLParser.ReplaceASCICode(AStr, False);
end;

procedure SetDoubleValue(var AVariable: PDouble; const AValue: Double);
begin
  if AValue = 0 then
  begin
    if AVariable <> nil then
    begin
      Dispose(AVariable);
      AVariable := nil;
    end;
  end
  else
  begin
    if AVariable = nil then
      New(AVariable);
    AVariable^ := AValue;
  end;
end;

procedure SetEntityVisibilityAndLoaded(const AEntity: TsgDXFEntity;
  const AConverter: TsgDXFConverter; const AVis: Boolean);
var
  I: Integer;
begin
  if AEntity = nil then Exit;
  AEntity.Visibility := AVis;
  if AEntity is TsgDXFInsert then
    SetEntityVisibilityAndLoaded(TsgDXFInsert(AEntity).Block, AConverter, AVis)
  else
    if AEntity is TsgDXFBlock then
    begin
      TsgDXFBlock(AEntity).IsLoaded := False;
      for I := TsgDXFBlock(AEntity).Count - 1 downto 0 do
        SetEntityVisibilityAndLoaded(TsgDXFBlock(AEntity).Entities[I],
          AConverter, AVis);
    end;
  TsgDXFEntityAccess(AEntity).Loaded(AConverter);
end;

procedure SetF2DPointCoord(var APPoint: PF2DPoint; const AType: Byte;
  const AValue: Double);
begin
  if AValue = 0 then
  begin
    if (APPoint <> nil) and (APPoint^.V[Byte(not Boolean(AType))] = 0) then
    begin
      Dispose(APPoint);
      APPoint := nil;
    end;
  end
  else
  begin
    if APPoint = nil then
    begin
      New(APPoint);
      APPoint^ := cnstF2DPointZero;
    end;
    APPoint.V[AType] := AValue;
  end;
end;

procedure SetF2DPointValue(var APPoint: PF2DPoint; const AValue: TF2DPoint);
begin
  if IsEqualF2DPoints(AValue, cnstF2DPointZero) then
  begin
    if APPoint <> nil then
    begin
      Dispose(APPoint);
      APPoint := nil;
    end;
  end
  else
  begin
    if APPoint = nil then
      New(APPoint);
    APPoint^ := AValue;
  end;
end;

procedure SetSpliteNumber(var ANumber: Integer; const ACurrent,
  ADefault: Integer);
begin
  ANumber := ACurrent;
  if ANumber < 3 then
  begin
    ANumber := ADefault;
    if ANumber < 3 then
      ANumber := 3;
  end;
end;

procedure SwapBlocks(const AIns1, AIns2: TsgDXFInsert);
var
  vBlock: TsgDXFBlock;
begin
  vBlock := AIns2.Block;
  AIns2.Block := AIns1.Block;                                      
  AIns1.Block := vBlock;
end;
////////////////////////////////////////////////////////////////////////////////

type
  TsvgNode = class(TsgNode)
  private
    procedure LoadFPointsArray(const AList: TFPointList;
      const AArrayValues: string;
      const AType: Byte; const AEntity: TsgDXFEntity);
    procedure SetArrayValues(var AList: TFPointList;
      const AStates: TsgIntegerList; const AType: Byte;
      const AValue: string; const AEntity: TsgDXFEntity);
    procedure SetClass(const AEntity: TsgDXFEntity; const AValue: string;
      const Base: Boolean; const ASVGNodeID: TsgSVGNodeID = niDefault);
    procedure SetDashArray(const AEntity: TsgDXFEntity; const AValue: string);
    procedure SetFill(const AEntity: TsgDXFEntity; const AValue: string);
    procedure SetFont(const AEntity: TsgDXFEntity; const AValue: string);
    procedure SetStyle(const AEntity: TsgDXFEntity; const AValue: string);
    procedure SetSvgEntExtendedInfo(const AEntity: TsgDXFEntity);
    procedure SetViewBox(var AViewBox: PsgViewBox; const AValue: string);
  protected
    FReader: Pointer;
    procedure ApplyAttribs(const AEntity: TsgDXFEntity;
      const ABaseIgnores: TsgTagIDs = []);
    function CheckText(const ANode: TsgNodeSample): Boolean; override;
    function GetClassID: Cardinal; override;
    function GetSVGNodeID: TsgSVGNodeID; virtual;
    procedure SetReader(const AReader: Pointer); override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); virtual;
    function SetSvgEntPropsBase(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample): Boolean; virtual;
  public
    destructor Destroy; override;
    procedure Assign(const Obj: TObject); override;
    function ValueAsDoubleDim(const Attrib: TsgNodeSample; const AType: Byte;
      const AEntity: Pointer): Double;
    property SVGNodeID: TsgSVGNodeID read GetSVGNodeID;
  end;
  
  TsvgNode_A = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
  end;

  TsvgNode_SVG = class(TsvgNode)
  private
    FPoint1: PF2DPoint;
    FSize: PF2DPoint;
    FViewBox: PsgViewBox;
  protected
    procedure ApplyReadParams(const AEntity: TsgDXFEntity); virtual;
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  public
    destructor Destroy; override;
    procedure Assign(const Obj: TObject); override;
  end;

  TsvgNode_STYLE = class(TsvgNode)
  private
    procedure ClearComments(var AStr: string);
  protected
    procedure AddStyles(const AStr: string);
    procedure Apply;
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
  end;

  TsvgNode_DEFS = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_G = class(TsvgNode_SVG)
  private
    FClipPath: string;
  protected
    procedure ApplyReadParams(const AEntity: TsgDXFEntity); override;
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    function SetSvgEntPropsBase(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample): Boolean; override;
   public
    procedure Assign(const Obj: TObject); override;
  end;

  TsvgNode_SYMBOL = class(TsvgNode_G)
  protected
    procedure ApplyReadParams(const AEntity: TsgDXFEntity); override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_SWITCH = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
  end;

  TsvgNode_CLIPPATH = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;    
  end;

  TsvgNode_SET = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;    
  end;

  TsvgNode_CIRCLE = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_ELLIPSE = class(TsvgNode_CIRCLE)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_LINE = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_RECT = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
     const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_POLYLINE = class(TsvgNode)
  protected
    procedure AddPoints(const AEntity: TsgDXFEntity; const APoints: string);
    function Closed: Boolean; virtual;
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
     const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_PATH = class(TsvgNode)
  private
    FPathData: string;
    FPathLength: Double;
  protected
    procedure AddPathSeg(const AEntity: TsgDXFEntity; const APathsSeg: string);
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  public
    procedure Assign(const Obj: TObject); override;
  end;

  TsvgNode_POLYGON = class(TsvgNode_POLYLINE)
  protected
    function GetSVGNodeID: TsgSVGNodeID; override;
    function Closed: Boolean; override;
  end;

  TsvgNode_TEXT = class(TsvgNode)
  private
    FEnt: TsgSVGContainer;
    FPositions: TsvgPositions;
    FRootNode: TsvgNode_TEXT;
    FTextPrev: TsgSVGText;
    FFontSize: Double;
    FIgnoreAttribPositions: Boolean;
    function GetFontSize: Double;
    procedure GetTextPosition(const APositions: TsvgPositions; const AUseOwners: Boolean = True);
    procedure LoadTextPosition(const APositions: TsvgPositions;
      const AEntity: TsgDXFEntity);
    procedure LoadOwners(const AOwners: TsgObjectList);
    procedure SetTextPosition(const AText, ATextPrev: TsgSVGText);
    procedure ApplyFullAttribs(const AEnt: TsgDXFEntity);
    procedure SetSvgEntPropsPositions(const AEntity: TsgDXFEntity;
      const APositions: TsvgPositions; const AID: TsgTagID; const Attrib: TsgNodeSample);
  protected
    procedure ClearTextPositions;
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetOwnerBySetProps(const AOwner: Pointer): Pointer; virtual;
    function GetSVGNodeID: TsgSVGNodeID; override;
    function GetUseText: string; virtual;
    procedure SetEntMultiText(const AOwner: Pointer;
      const AStrings: TsgStringList);
    procedure SetEntText(const AOwner: Pointer; const AText: string);
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
    procedure SetAlignContainer(const AValue: Byte);
  public
    destructor Destroy; override;
  end;

  TsvgNode_TEXTPATH = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;  

  TsvgNode_TSPAN = class(TsvgNode_TEXT)
  protected
    function GetSVGNodeID: TsgSVGNodeID; override;
    function GetOwnerBySetProps(const AOwner: Pointer): Pointer; override;
  end;

  TsvgNode_TREF = class(TsvgNode_TSPAN)
  private
    FRef: string;
    function GetAllTexts(const AEntity: TsgDXFEntity): string;
  protected
    function GetSVGNodeID: TsgSVGNodeID; override;
    function GetUseText: string; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_USE = class(TsvgNode)
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  end;

  TsvgNode_IMAGE = class(TsvgNode_RECT)
  private
    FHRef: string;
    function GetImageByHref: TGraphic;
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    procedure SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample); override;
  public
    procedure Assign(const Obj: TObject); override;
  end;

  TsvgNode_LGRADIENT = class(TsvgNode)
  private
    FStopColor: Integer;
    FStopOffset: Double;
    FStopOpasity: Double;
    FSubNodeId: TsgSVGNodeID;
  protected
    function GetDataInternal(const AOwner: TObject): TObject; override;
    function GetSVGNodeID: TsgSVGNodeID; override;
    function IsLinear: Boolean; virtual;
    function SetSvgEntPropsBase(const AEntity: TsgDXFEntity; const AID: TsgTagID;
      const Attrib: TsgNodeSample): Boolean; override;
  public
    procedure Assign(const Obj: TObject); override;
  end;

  TsvgNode_RGRADIENT = class(TsvgNode_LGRADIENT)
  protected
    function GetSVGNodeID: TsgSVGNodeID; override;
    function IsLinear: Boolean; override;
  end;

  TsvgNode_PATTERN = class(TsvgNode)
  protected
    function GetSVGNodeID: TsgSVGNodeID; override;
    function GetDataInternal(const AOwner: TObject): TObject; override;
  end;

  TsvgReader = class
  private
    FBufferList: TsgObjectList;
    FBufferIntegerList: TsgIntegerList;
    FConv: TsgSVGConverter;
    FDefs: TsgSVGGroup;
    FDebugInfo: Boolean;
    FDebugInfoMode: Integer;
    FHeader: PsgHeadVarStruct;
    FImages: TList;
    FMaxImages: Integer;
    FNodesAnalize: Integer;
    FNodesAnalizing: Integer;
    FNodesCacl: Boolean;
    FNodesCounter: Integer;
    FNodesStep: Integer;
    FOnProgress: TsgProgressEvent;
    FParser: TsgParser;
    FParserExtended: TsgParser;
    FStream: TStream;
    FStyles: TsgStringListNamed;
    FSVGReload: Boolean;
    FSVGSize: PF2DPoint;
    procedure BeginUpdateProgress;
    procedure EndUpdateProgress;
    function FindEntity(const AOwner: TObject; const AID: string): TsgDXFEntity;
    function FindHrefs(const ABlock: TsgDXFBlock): Boolean;
    function GetSVGClipPaths: TsgStringList;
    function GetSVGDefs: TsgSVGGroup;
    function GetSVGStyleFills: TsgStringList;
    function GetMsg: string;
    procedure LoadStyles(const ANode: TsgNodeSample);
    procedure SetMsg(const AValue: string);
    procedure UpdateProgress;
    procedure ClearEmptyBlocks;
    function AddImageName(const AFileName: string): Boolean;
    procedure DelImageName(const AFileName: string);
  protected
    procedure AddSVGClipPath(const AName: string; const AList: TsgObjectList);
    procedure AddSVGStyle(const AName, AValue: string);
    function AddSVGStyleFill(const AName: string; const AStyle: Pointer): Pointer;
    procedure AnalizeChildNodes(const AContainer: TsgSVGContainer;
      const ANode: TsgNodeSample);
    procedure AnaliseNode(const AContainer: TsgSVGContainer;
      const ANode: TsgNodeSample);
    procedure CheckImageCounts(const AContainer: TsgSVGContainer);
    procedure GetBufferList(var ABufferObject: TsgObjectList;
      var ABufferInt: TsgIntegerList; Capacity: Integer = 0);
    function GetCurrentLayout: TsgDXFLayout;
    function GetKoefValue(const ADimValue: TsgUnitsOfMeasurements;
      const AType: Byte; const AFontSize: Double): Double;
    function GetNodeAllTexts(const ANode: TsgNodeSample): string;
    function GetNodeByID(const ID: string): TsgNodeSample;
    function GetStrWithCodePage(const AStr: string): string;
    function GetStyleValue(const AIndex: Integer): string;
    procedure DeleteStyleByIndex(const AIndex: Integer);
    function IndexStyleOfName(const AName: string): Integer;
    function IndexSVGStyleFillOfName(const AName: string): Integer;
    procedure LoadEntities;
    procedure LoadNodes;
    function NeedExtendedInfo: Boolean;
    function Parsing: Boolean;
    procedure RegisterClasses;
    procedure ReLoadEntities;
    procedure SetSVGSize(const ASize: TF2DPoint);
    function Stopped: Boolean;
    function CreateGroup(AEntClass: TsgClassOfContainer;
      AOwner: TsgSVGContainer): TObject;
    procedure FreeGroup(var AGroup);
    procedure SetParserExtened(const AParser: TsgParser);
    property Msg: string read GetMsg write SetMsg;
    property SVGClipPaths: TsgStringList read GetSVGClipPaths;
    property SVGDefs: TsgSVGGroup read GetSVGDefs;
    property SVGReload: Boolean read FSVGReload write FSVGReload;
    property SVGStyleFills: TsgStringList read GetSVGStyleFills;
  public
    constructor Create(const Converter: TsgDXFConverter;const S: TStream);
    destructor Destroy; override;
    procedure ReadFile; virtual;
    property Conv: TsgSVGConverter read FConv;
    property DebugInfo: Boolean read FDebugInfo write FDebugInfo;
    property DebugInfoMode: Integer read FDebugInfoMode write FDebugInfoMode;
    property OnProgress: TsgProgressEvent read FOnProgress write FOnProgress;
  end;

  TsvgReaderZip = class(TsvgReader)
  public
    procedure ReadFile; override;
  end;

  TsgPathLoader = class
  private
    FBuffer: TFPointList;
    FConverter: TsgDXFConverter;
    FData: TList;
    FEntities: TsgObjectList;
    FGenerator: TsgGeneratorShapeEdge;
    FIndex: Integer;
    FPolyline: TsgDXFLWPolyline;
    FPrevPoint: TFPoint;
    procedure AddFPointsInBuffer(const APoints: array of TFPoint);
    procedure SetFPoints(var APoints: array of TFPoint;
      const ARelative: Boolean);
  protected
    function AddPointsArc(const ARelative: Boolean): Boolean;
    procedure AddPointsCoord(const ARelative: Boolean; const AType: Byte);
    procedure AddPointsCurveCubic(const ARelative: Boolean);
    procedure AddPointsCurveCubicSmooth(const ARelative: Boolean);
    procedure AddPointsCurveQuadratic(const ARelative: Boolean);
    procedure AddPointsCurveQuadraticSmooth(const ARelative: Boolean);
    procedure AddPointsLine(const ARelative: Boolean);
    function AddPointsMove(const ARelative: Boolean): Boolean;
    function AddPoly(const AClose: Boolean): Boolean;
    function ApplyRelative(const APoint: TFPoint;
      const ARelative: Boolean): TFPoint;
    function GetDataAsByte: Byte;
    function GetDataAsDouble: Double;
    function GetDataAsFPoint: TFPoint;
    function GetDataAsSegmentType: TsgPathSegType;
    function GetLastPoint: TFPoint;
    function GetNumberPartsByCircle: Integer;
    function GetNumberPartsBySpline: Integer;
    function SetFirstPointsAndPoly(const APoint: TFPoint;
      var APoint1, APoint2: TFPoint): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadEntities(const AConverter: TsgDXFConverter; const AData: TList; const AEntities: TsgObjectList): Boolean;
  end;


{ TsgSVGImage }

procedure TsgSVGImage.ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD);
var
  vEnt: TsgDXFEntityAccess absolute Sender;
  vLineWeightScaled, vHasParams: Boolean;
  vLineCap, vLineJoin: Byte;
  vMiterLimit: Single;
  vPenWidth, vLC, vLJ: Integer;
begin
  inherited ApplyPen(Sender, AEntColor);
  vHasParams := GetEntityLineParams(TsgDXFEntity(Sender), FDraw.Insert,
    vLineWeightScaled, vLineCap, vLineJoin, vMiterLimit);
  if vHasParams then
  begin
    vPenWidth := SetLineWeigth(TsgDXFEntity(Sender),
      vLineWeightScaled{$IFDEF SGSPECIALUSER_OTHERUNITS}, FmmToPixelX{$ENDIF});
    if (vPenWidth > 1){ and ((vLineCap or vLineJoin) <> 0 ) }then
    begin
      case vLineCap of
        0: vLC := PS_ENDCAP_FLAT;
        1: vLC := PS_ENDCAP_ROUND;
        2: vLC := PS_ENDCAP_SQUARE;
      else
        vLC := PS_ENDCAP_FLAT;
      end;
      case vLineJoin of
        0: vLJ := PS_JOIN_MITER;
        1: vLJ := PS_JOIN_ROUND;
        2: vLJ := PS_JOIN_BEVEL;
      else
        vLJ := PS_JOIN_MITER;
      end;
      SetPenStyle(vLC or vLJ, vMiterLimit);
    end;
  end;
end;

constructor TsgSVGImage.Create;
begin
  inherited Create;
  IsShowLineWeight := True;
{$IFDEF SG_FIREMONKEY}
  InitParams;
{$ENDIF}
end;

function TsgSVGImage.CreateConverter: TsgDXFConverter;
begin
  CBFormat := CF_SVG;
  Result := TsgSVGConverter.CreateFromFile('');
end;

function TsgSVGImage.DoDraw(Entity: TsgDXFEntity): Integer;
begin
  case TsgDXFEntity(Entity).EntType of
    ceInsert:
      begin
        if IsSVGGroup(Entity) then
          Result := DrawGroup(Entity)
        else
          Result := inherited DoDraw(Entity);
      end;
  else
    Result := inherited DoDraw(Entity);
  end;
end;

function TsgSVGImage.DoFinish(Entity: TsgDXFEntity): Integer;
begin
  case TsgDXFEntity(Entity).EntType of
    ceInsert:
      begin
        if IsSVGGroup(Entity) then
        begin
          Result := 0;
          FinishGroup(Entity);
        end
        else
          Result := inherited DoFinish(Entity);
      end;
  else
    Result := inherited DoFinish(Entity);
  end;
end;

procedure TsgSVGImage.DoOnProgress(AStage: TProgressStage; ADone,
  ACount: Integer);
var
  vProgress: Byte;
begin
  vProgress := Byte(sgMulDiv(ADone, 100, ACount));
  Progress(Self, AStage, vProgress, False, cnstRectZero, TsgSVGConverter(Converter).FMsg);
end;

procedure TsgSVGImage.FinishGroup(Sender: TObject);
begin
  EnterInsert(Sender);
end;

function TsgSVGImage.IsSVGGroup(Entity: TsgDXFEntity): Boolean;
begin
  Result := False;
  if (Entity is TsgSVGContainer) and
    (Entity.EntClass.ET.SVG in [svgGroup, svgGroupDim]) then
    Result := True;
end;

function TsgSVGImage.GetParserInternal: TsgParser;
begin
  Result := nil;
end;

function TsgSVGImage.IsRotatedText(const AText: TsgDXFText; AHeight: Single;
  const ADrawMatrix: TFMatrix): Boolean;
begin
  Result := (ADrawMatrix.M[0, 0] < 0) or (ADrawMatrix.M[1, 1] > 0)
    or IsRotatedTextInternal(AText, AHeight, ADrawMatrix);
end;

function TsgSVGImage.DrawGroup(Sender: TObject): Integer;
begin
  Result := DrawInsert(Sender);
end;

procedure TsgSVGImage.DrawBox(const ABox: TFRect; const AColor: TColor);
var
  vRect: TRect;
begin
  vRect := cnstBad2DRect;
  ExpandRect(vRect, GetPoint(ABox.TopLeft));
  ExpandRect(vRect, GetPoint(ABox.BottomRight));
  Context.PenColor := AColor;
  Context.PenWidth := 1;
  Context.BrushStyle := bsClear;
  Context.Rectangle(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
end;

function TsgSVGImage.GetEntityColors(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TsgEntColors;
var
  vInsert: TsgDXFInsert;

  function IsClearColor(AEnt: TsgDXFEntity): Boolean;
  var
    vColor: TColor;
  begin
    vColor := AEnt.Color;
    Result := (vColor = clNone) or (vColor = clByBlock) or
      (vColor = clByLayer);
  end;

begin
  case (Byte(AInsert = nil) shl 1) + Byte(IsClearColor(AEntity)) of
    0:  Result.EntColorCAD := AEntity.ColorCAD;
    1:
      begin
        Result.EntColorCAD := cnstColorCADNone;
        vInsert := AInsert;
        while vInsert <> nil do
        begin
          if not IsClearColor(vInsert) then
          begin
            Result.EntColorCAD := vInsert.ColorCAD;
            Break;
          end;
          vInsert := vInsert.OwnerInsert;
        end;
      end;
    2:  Result.EntColorCAD := AEntity.ColorCAD;
    3:  Result.EntColorCAD := cnstColorCADNone;
  else
    Result.EntColorCAD := AEntity.ColorCAD;
  end;
  if IsEqualColorCAD(Result.EntColorCAD, cnstColorCADNone) then
    Result.EntColorCAD :=  cnstColorCA_RGB_Black;
  Result.EntColor := ConvertColorCADToRGB(Result.EntColorCAD);
  Result.DrawColor := Result.EntColor;
  if Result.DrawColor = clBlack then //evg
    Result.DrawColor := GetDrawPenColor
  else
   if Result.DrawColor = clWhite then
      Result.DrawColor := GetDrawBackgroundColor
   else
     if DrawMode = dmBlack then
     begin
       Result.DrawColor := GetDrawPenColor;
     end;
  ApplyAlternateWhite(Result.DrawColor);
end;

function TsgSVGImage.GetPolygonFillingMode: Integer;
begin
  Result := WINDING;
end;

function TsgSVGImage.HasClip(AInsert: TsgDXFInsert;
  out AFilter: TObject): Boolean;
var
  I: Integer;
  vPoints: TsgClipPathPoints;
  vGroup: TsgSVGGroup absolute AInsert;
begin
  Result := False;
  AFilter := nil;
  if AInsert is TsgSVGGroup then
  begin
    if vGroup.HasViewBox then
      Result := True
    else
    begin
      if (vGroup.ClipPoints <> nil) and (vGroup.ClipPoints.Count > 0) then
      begin
        for I := 0 to vGroup.ClipPoints.Count - 1 do
        begin
          vPoints := TsgClipPathPoints(vGroup.ClipPoints[I]);
          if vPoints.Count > 2 then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TsgSVGImage.CanSaveToStream: Boolean;
begin
  Result := Assigned(GetParserInternal());
end;

procedure TsgSVGImage.LoadFromStream(S: TStream);
var
  vReader: TsvgReader;
  vIsGZPacked: Boolean;
  vFileName: string;
begin
  inherited LoadFromStream(S);
  {$IFNDEF SG_OPENING_IN_THEADS}
  Loading := Self;
  {$ENDIF}
  vFileName := FileName;
  TsgSVGConverter(Converter).SetLoading(True, nil, DoUpdateEvent);
  TsgSVGConverter(Converter).AddGraphicName(vFileName);
  try
    if S.Size = 0 then
    begin
      {$IFNDEF SG_OPENING_IN_THEADS}
      Loading := nil;
      {$ENDIF}
      GetExtents;
      Exit;
    end;
    vIsGZPacked := IsGZPacked(S);
    if vIsGZPacked then
      vReader := TsvgReaderZip.Create(Converter, S)
    else
      vReader := TsvgReader.Create(Converter, S);
    try
      vReader.OnProgress := DoOnProgress;
      if not vIsGZPacked then
        vReader.SetParserExtened(GetParserInternal);
      Converter.ClearDrawingProp;
      vReader.ReadFile;
      SetDefaultViewPort(Converter);
      GetExtents;
      SetDefaultPlotSettings;
    finally
      vReader.OnProgress := nil;
      vReader.Free;
    end;
  finally
    TsgSVGConverter(Converter).DelGraphicName(vFileName);
    TsgDXFConverterAccess(Converter).SetLoading(False, nil, nil);
    FreeStack(sgGetCurrentThreadId);
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := nil;
    {$ENDIF}
  end;
end;

function TsgSVGImage.SaveToStream(const AStream: TStream): Integer;
var
  vParser: TsgParser;
begin
  Result := inherited SaveToStream(AStream);
  vParser := GetParserInternal;
  if Assigned(vParser) then
  begin
    try
      vParser.SaveToStream(AStream);
      Result := 1;
    except
      Result := -1;
    end;
  end;
end;

procedure TsgSVGImage.SetClip(AInsert: TsgDXFInsert; AFilter: TObject);
var
  vGroup: TsgSVGGroup absolute AInsert;
  vPoints: TsgIntegerList;
  vRegion, vCurRegion: TsgRegion;
  vDrawMatrix: TFMatrix;
  L: Integer;

  procedure AddPoint(const AP: TF2DPoint);
  var
    vPoint: TPoint;
  begin
    vPoint := GetPoint(MakeFPointFrom2D(AP));
    vPoints.Add(vPoint.X);
    vPoints.Add(vPoint.Y);
  end;

  procedure GetRegionPointsList(const AList: TsgClipPathPoints; ARegion: TsgRegion);
  var
    I: Integer;
  begin
    vPoints.Count := 0;
    vPoints.Capacity := AList.Count shl 1;
    for I := 0 to AList.Count - 1 do
      AddPoint(AList.List[I]);
    ARegion.SetPolygon(vPoints.List^[0], vPoints.Count shr 1, WINDING);
  end;

  procedure GetRegionViewBox(const ASize: TF2DPoint; ARegion: TsgRegion);
  begin
    vPoints.Count := 0;
    vPoints.Capacity := 8;
    AddPoint(MakeF2DPoint(0, 0));
    AddPoint(MakeF2DPoint(ASize.X, 0));
    AddPoint(MakeF2DPoint(ASize.X, ASize.Y));
    AddPoint(MakeF2DPoint(0, ASize.Y));
    ARegion.SetPolygon(vPoints.List^[0], vPoints.Count shr 1, WINDING);
  end;

begin
  inherited SetClip(AInsert, AFilter);
  if AFilter <> nil then Exit;
//    if (FDraw.XScale = 0) or (FDraw.YScale = 0) then Exit;
  vDrawMatrix := FDraw.Matrix;
  vPoints := TsgIntegerList.Create;
  try
    vRegion := TsgRegion.Create;
    try
      if vGroup.HasViewBox then
        GetRegionViewBox(vGroup.ViewBox.Size, vRegion)
      else
      begin
        FDraw.Matrix := FMatXMat(vGroup.GetMatrix, FDraw.Matrix);
        if Assigned(vGroup.ClipPoints) and (vGroup.ClipPoints.Count > 0) then
        begin
          if vGroup.ClipPoints.Count > 1 then
          begin
            vCurRegion := TsgRegion.Create;
            try
              for L := 0 to vGroup.ClipPoints.Count - 1 do
              begin
                GetRegionPointsList(TF2DPointList(vGroup.ClipPoints[L]), vCurRegion);
                vRegion.Exclude(vCurRegion);
              end;
            finally
              vCurRegion.Free;
            end;
          end
          else
            GetRegionPointsList(TF2DPointList(vGroup.ClipPoints[0]), vRegion);
        end;
      end;
      Context.RegionOffset(vRegion);
      ExpClipRegionInternal(AInsert, vRegion);
    finally
      vRegion.Free;
    end;
  finally
    FDraw.Matrix := vDrawMatrix;
    vPoints.Free;
  end;
end;

{ TsgSVGConverter }


function TsgSVGConverter.AddGraphicName(const AFileName: string): Boolean;
const
  cnstName: string = 'Graphics';
var
  vCnt, I: Integer;
  vStack: TStrings;
  vFileName: string;
begin
  Result := False;
  if not Assigned(FGraphicsNames) then
  begin
    vStack := GetStack(sgGetCurrentThreadId);
    I := vStack.IndexOf(cnstName);
    if I > -1 then
      FGraphicsNames := TStringList(vStack.Objects[I])
    else
    begin
      FGraphicsNames := CreateStringListSorted(dupIgnore);
      vStack.AddObject(cnstName, FGraphicsNames);
    end;
  end;
  vCnt := FGraphicsNames.Count;
  vFileName := ExtractFileName(AFileName);
  FGraphicsNames.Add(vFileName);
  if vCnt < FGraphicsNames.Count then
    Result := True;
end;

procedure TsgSVGConverter.DelGraphicName(const AFileName: string);
var
  I: Integer;
  vFileName: string;
begin
  if Assigned(FGraphicsNames) and (FGraphicsNames.Count > 0) then
  begin
    vFileName := ExtractFileName(AFileName);
    I := FGraphicsNames.IndexOf(vFileName);
    if I > -1 then
      FGraphicsNames.Delete(I);
  end;
end;

procedure TsgSVGConverter.ClearBegin;
var
  I, J: Integer;
  vBlock: TsgDXFBlockAccess;
  vBlocks: TsgDXFEntity;
begin
  inherited ClearBegin;
  vBlocks := Sections[csBlocks];
  for I := 0 to vBlocks.Count - 1 do
  begin
    vBlock := TsgDXFBlockAccess(vBlocks[I]);
    for J := vBlock.References.Count - 1 downto 0 do
      TsgDXFEntityAccess(vBlock.References[J]).RemoveReferenceNotification(vBlock);
  end;
end;

procedure TsgSVGConverter.ClearEnd;
var
  I: Integer;
  vList: TsgObjectList;
begin
  inherited ClearEnd;
  if Assigned(FGradientStyles) then
  begin
    for I := 0 to FGradientStyles.Count - 1 do
      FGradientStyles.Objects[I].Free;
    FreeAndNil(FGradientStyles);
  end;
  if Assigned(FClipPolyPolylines) then
  begin
    for I := 0 to FClipPolyPolylines.Count - 1 do
    begin
      vList := TsgObjectList(FClipPolyPolylines.Objects[I]);
      TsgObjectList.FreeList(vList);
    end;
    FreeAndNil(FClipPolyPolylines);
  end;
//  FDefs.Free;
end;

constructor TsgSVGConverter.Create;
begin
  inherited Create;
  FCodePage := CP_ACP;
  //IsCrossoverMatrix := False;
  UseSHXFonts := False;
  InitializeSectionsBegin;
  InitStandartStyleByText;

  FGradientStyles := TsgStringList.Create;
  FGradientStyles.Sorted := True;
  FGradientStyles.Duplicates := dupIgnore;

  FClipPolyPolylines := TsgStringList.Create;
  FClipPolyPolylines.Sorted := True;
  FClipPolyPolylines.Duplicates := dupAccept;  
end;

destructor TsgSVGConverter.Destroy;
begin
  FGraphicsNames := nil;
  FreeAndNil(FParser);
  inherited Destroy;
end;

function TsgSVGConverter.CreateDefs: TsgSVGContainer;
var
  vBlock: TsgDXFBlock;
begin
  vBlock := TsgDXFBlock.Create;
  vBlock.Name := cnstBlockDef;

  Result := TsgSVGGroup.Create;
  TsgDXFEntityAccess(Result).SetConverter(Self);
  TsgDXFBlockAccess(TsgSVGGroup(Result).Block).SetFlags(1);
  TsgSVGGroup(Result).Block.Name := cnstDefs;
  TsgSVGGroup(Result).ID := cnstDefs;
  Sections[csBlocks].AddEntity(TsgSVGGroup(Result).Block);
  Loads(Result);
  TsgSVGGroup(Result).Block.IsLoaded := False;

  vBlock.AddEntity(TsgDXFEntity(Result));
  TsgDXFBlockAccess(vBlock).SetFlags(1);
  Loads(vBlock);
  vBlock.IsLoaded := False;
  Sections[csBlocks].AddEntity(vBlock);
end;

procedure TsgSVGConverter.InitStandartStyleByText;
var
  vText: TsgDXFTextAccess;
  vStyle: TsgDXFStyle;
begin
  vText := TsgDXFTextAccess(TsgDXFText.Create);
  Self.Loads(vText);
  vText.Free;
  vStyle := Self.Styles[0];
  vStyle.FontName := cnstDefFontFamily;
  vStyle.PrimaryFont := cnstDefFileName;
end;

{ TsvgReader }

function TsvgReader.AddSVGStyleFill(const AName: string; const AStyle: Pointer): Pointer;
var
  vIndex: Integer;
begin
  Result := AStyle;
  vIndex := Conv.GradientStyles.IndexOf(AName);
  if vIndex = -1 then
    Conv.GradientStyles.AddObject(AName, AStyle)
  else
  begin
    if Conv.GradientStyles.Objects[vIndex] = nil then
      Conv.GradientStyles.Objects[vIndex] := AStyle
    else
    begin
      Result := Conv.GradientStyles.Objects[vIndex];
      TObject(AStyle).Free;
    end;
  end;
end;

procedure TsvgReader.AddSVGStyle(const AName, AValue: string);
begin
  FStyles.AddNameValue(AName, AValue, 0);
end;

function TsvgReader.AddImageName(const AFileName: string): Boolean;
begin
  Result := False;
  if Assigned(FConv) then
    Result := FConv.AddGraphicName(AFileName);
end;


procedure TsvgReader.DelImageName(const AFileName: string);
begin
  if Assigned(FConv) then
    FConv.DelGraphicName(AFileName);
end;

procedure TsvgReader.AddSVGClipPath(const AName: string; const AList: TsgObjectList);
begin
  Conv.ClipPolyPolylines.AddObject(AName, AList);
end;

procedure TsvgReader.AnalizeChildNodes(const AContainer: TsgSVGContainer; const ANode: TsgNodeSample);
var
  I: Integer;
begin
  if ANode.HasChildNodes then
    for I := 0 to ANode.ChildNodesCount - 1 do
       AnaliseNode(AContainer, ANode.ChildNodes[I]);
end;

procedure TsvgReader.AnaliseNode(const AContainer: TsgSVGContainer; const ANode: TsgNodeSample);
var                                    
  vAttrib: TsgNodeSample;
  vEntity: TsgSVGContainer;
  vCodePage: Integer;
begin
  if Stopped then Exit;
  case ANode.NodeType of
    ntElement:
      begin
        case ANode.ClassID of
          cnstClassID_SVG:
            begin
              if FNodesCacl then
              begin
                Inc(FNodesCounter);
                AnalizeChildNodes(nil, ANode);
              end
              else
              begin
                vEntity := TsgSVGContainer(ANode.GetData(Self, AContainer));
                if vEntity <> nil then
                begin
                  if AContainer <> nil then
                    AContainer.AddEntity(vEntity)
                  else
                    GetCurrentLayout.AddEntity(vEntity);
                  TsgDXFEntityAccess(vEntity).SetConverter(FConv);
                  if Assigned(vEntity.ExtendedInfo) then
                    vEntity.ExtendedInfo.Node := ANode;
                end;
                UpdateProgress;
              end;
            end;
          cnstClassID_HTML:  AnalizeChildNodes(nil, ANode);
        end;
      end;
    ntDocType:
      begin
      end;
    ntDocument:
      begin
        if AnsiLowerCase(ANode.Name) = cnstXML then
        begin
          FHeader^.CodePage := cnstDefCodePage;
          if ANode.HasAttributeNodes then
          begin
            vAttrib := ANode.GetAttributeByName(cnstEncoding);
            if (vAttrib <> nil) and GetCodePageByName(vAttrib.Value, vCodePage) then
              FHeader^.CodePage := vCodePage;
          end;
        end;
      end;
  end;
end;

procedure TsvgReader.BeginUpdateProgress;
begin
  FNodesAnalize := 0;
  FNodesAnalizing := 0;
  FNodesCounter := 0;
  FNodesStep := 0;
  Msg := sLoadEntities;
  if Assigned(FOnProgress) then
    FOnProgress(psStarting, 0, 100);
end;

procedure TsvgReader.EndUpdateProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(psEnding, 100, 100);
end;

procedure TsvgReader.UpdateProgress;
begin
  Inc(FNodesAnalizing);
  if (FNodesAnalizing - FNodesAnalize) > FNodesStep then
  begin
    FNodesAnalize := FNodesAnalizing;
    if Assigned(FOnProgress) then
      FOnProgress(psRunning, FNodesAnalize, FNodesCounter);
  end;
end;

procedure TsvgReader.CheckImageCounts(const AContainer: TsgSVGContainer);
var
  I: Integer;
begin
  if AContainer.SVGType = svgImage then
  begin
    if FImages = nil then
       TsgSVGImageEnt(AContainer).ConvertToBitmap
    else
    begin
      FImages.Add(AContainer);
      if FImages.Count >= FMaxImages then
      begin
        try
          for I := 0 to FImages.Count - 1 do
            TsgSVGImageEnt(FImages.List[I]).ConvertToBitmap;
        finally
          FreeAndNil(FImages);
        end;
      end;
    end;
  end;
end;

procedure TsvgReader.ClearEmptyBlocks;
var
  I: Integer;
  vList: TList;
  vBlock: TsgDXFBlock;
  vBlocks: TsgDXFEntity;
begin
  vList := TList.Create;
  try
    vBlocks := FConv.Sections[csBlocks];
    for I := vBlocks.Count - 1 downto 0 do
    begin
      vBlock := TsgDXFBlock(vBlocks[I]);
      if (vBlock.Layout = nil) and (vBlock.References.Count = 0) and (vBlock.Count = 0) then
      begin
        vList.Add(vBlock);
        vBlocks.DeleteEntity(I);
      end;
    end;
    for I := 0 to vList.Count - 1 do
      TObject(vList[I]).Free;
  finally
    vList.Free;
  end;
end;

constructor TsvgReader.Create(const Converter: TsgDXFConverter; const S: TStream);
begin
  FImages := TList.Create;
  FMaxImages := 1024;
  FConv := TsgSVGConverter(Converter);
  FDefs := TsgSVGGroup(FConv.CreateDefs);
  FStream := S;
  FHeader := @FConv.HeadVarStruct;
  FStyles := TsgStringListNamed.Create;
  FBufferList := TsgObjectList.Create;
  FBufferIntegerList := TsgIntegerList.Create;
end;

function TsvgReader.CreateGroup(AEntClass: TsgClassOfContainer;
  AOwner: TsgSVGContainer): TObject;
begin
  Result := AEntClass.Create;
  TsgDXFEntityAccess(Result).SetConverter(FConv);
  TsgSVGContainerAccess(Result).Owner := AOwner;
end;

procedure TsvgReader.DeleteStyleByIndex(const AIndex: Integer);
begin
  FStyles.Delete(AIndex);
end;

destructor TsvgReader.Destroy;
begin
  FreeAndNil(FImages);
  if FSVGSize <> nil then
    Dispose(FSVGSize);
  Msg := '';
  FreeAndNil(FStyles);
  FreeAndNil(FBufferList);
  FreeAndNil(FBufferIntegerList);
  inherited Destroy;
end;

function TsvgReader.GetCurrentLayout: TsgDXFLayout;
begin
  Result := FConv.Layouts[0];
end;

procedure TsvgReader.GetBufferList(var ABufferObject: TsgObjectList;
  var ABufferInt: TsgIntegerList; Capacity: Integer = 0);
begin
  ABufferObject := FBufferList;
  ABufferObject.Count := 0;
  if ABufferObject.Capacity < Capacity then
    ABufferObject.Capacity := Capacity;

  ABufferInt := FBufferIntegerList;
  ABufferInt.Count := 0;
  if ABufferInt.Capacity < Capacity then
    ABufferInt.Capacity := Capacity;
end;

function TsvgReader.GetSVGClipPaths: TsgStringList;
begin
  Result := Conv.ClipPolyPolylines;
end;

function TsvgReader.GetSVGDefs: TsgSVGGroup;
begin
  Result := FDefs;
end;

function TsvgReader.GetSVGStyleFills: TsgStringList;
begin
  Result := Conv.GradientStyles;
end;

function TsvgReader.GetMsg: string;
begin
  if FConv <> nil then
    Result := FConv.FMsg
  else
    Result := ''; 
end;

function TsvgReader.GetKoefValue(const ADimValue: TsgUnitsOfMeasurements; const AType: Byte; const AFontSize: Double): Double;
//REC-SVG11-20030114/coords.html#UnitIdentifiers
var
  vSize: TF2DPoint;
begin
  Result := 1;
  case ADimValue of
    umPR:
      begin
        if FSVGSize <> nil then
          vSize := FSVGSize^
        else
          vSize := DefaultImageSize;
        case AType of
          0:  Result := vSize.X * 0.01;
          1:  Result := vSize.Y * 0.01;
        else
          Result := Sqr(vSize.X) + Sqr(vSize.Y);
          if Result > fDoubleResolution then
            Result := Sqrt(Result) * 0.01 / cnstSqrt2
          else
            Result := 0;
        end;
      end;
    umPX:  Result := cnst1px;
    umMM:  Result := cnst1mm;
    umCM:  Result := cnst1cm;
    umM :  Result := cnst1m;
    umIN:  Result := cnst1inch;
    umPT:  Result := cnst1pt;
    umPC:  Result := cnst1pc;
    umEM:  Result := cnst1px * AFontSize;//As in CSS, the em and ex unit identifiers are relative to the current font's font-size and x-height, respectively.
  end;
end;

function TsvgReader.GetNodeByID(const ID: string): TsgNodeSample;
begin
  Result := nil;
  if FParser <> nil then
    Result := FParser.FindNode(ID, FParser.ROOT);
end;

function TsvgReader.GetNodeAllTexts(const ANode: TsgNodeSample): string;
var
  I: Integer;
begin
  Result := '';
  if ANode.NodeType = ntElement then
  begin
    Result := ANode.Text;
    if ANode.HasChildNodes then
      for I := 0 to ANode.ChildNodesCount - 1 do
        Result := Result + GetNodeAllTexts(ANode.ChildNodes[I]);
  end;
end;

function TsvgReader.GetStrWithCodePage(const AStr: string): string;
{$IFNDEF SGDEL_2009}
var
  vWStr: WideString;
begin
  vWStr := '';
  case FHeader^.CodePage of
    1://CP_UTF8
      vWStr := sgUTF8ToUnicode(AStr);
  end;
  if Length(vWStr) > 0 then
    Result :=  sgUnicodeToDXFUnicode(vWStr)
  else
    Result := AStr;
{$ELSE}
begin
  Result := AStr;
{$ENDIF}
end;

function TsvgReader.IndexStyleOfName(const AName: string): Integer;
var
  vSorted: Boolean;
begin
  vSorted := FStyles.Sorted;
  try
    FStyles.Sorted := False;
    Result := FStyles.IndexOfName(AName);
  finally
    FStyles.Sorted := vSorted;
  end;
end;

function TsvgReader.IndexSVGStyleFillOfName(const AName: string): Integer;
var
  vNode: TsgNodeSample;
begin
  Result := SVGStyleFills.IndexOf(AName);
  if Result > -1 then
  begin
    if SVGStyleFills.Objects[Result] = nil then
      Result := -1;
  end
  else
  begin
    vNode := GetNodeByID(AName);
    if vNode is TsvgNode_LGRADIENT then
    begin
      TsvgNode_LGRADIENT(vNode).GetData(Self, nil);
      Result := SVGStyleFills.IndexOf(AName);
    end
    else
      AddSVGStyleFill(AName, nil);
  end;
end;

function TsvgReader.GetStyleValue(const AIndex: Integer): string;
begin
  Result := FStyles.ValueFromIndex[AIndex];
end;

procedure LoadEntitiesInBlock(const AConv: TsgDXFConverter; const ABlock: TsgDXFBlock);
var
  I: Integer;
  vContainer: TsgSVGContainerAccess;
  vEntity: TsgDXFEntity;
begin
  for I := 0 to ABlock.Count - 1 do
  begin
    vEntity := ABlock[I];
    if (vEntity.EntType = ceInsert) and (vEntity is TsgSVGContainer) then
    begin
      vContainer := TsgSVGContainerAccess(vEntity);
      if vContainer.GetSVGType in [svgGroup, svgGroupDim] then
        LoadEntitiesInBlock(AConv, vContainer.Block);
      AConv.Loads(vContainer);
    end;
  end;
end;

function TsvgReader.FindEntity(const AOwner: TObject; const AID: string): TsgDXFEntity;
begin
  Result := nil;
  if SVGDefs <> nil then
    Result := GetEntityInBlock(AID, SVGDefs.Block);
  if (Result = nil) and (AOwner is TsgSVGGroup) then
    Result := GetEntityInBlock(AID, TsgSVGGroup(AOwner).Block);
  if Result = nil then
    Result := GetEntityInBlock(AID, GetCurrentLayout.PaperSpaceBlock);
  if Result = nil then
    FSVGReload := True;
end;

function TsvgReader.FindHrefs(const ABlock: TsgDXFBlock): Boolean;
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vContainer: TsgSVGContainerAccess;
  vSVGLink: TsgSVGLink;
begin
  Result := False;
  for I := 0 to ABlock.Count - 1 do
  begin
    vEntity := ABlock[I];
    if (vEntity.EntType = ceInsert) and (vEntity is TsgSVGContainer) then
    begin
      vContainer := TsgSVGContainerAccess(vEntity);
      case vContainer.GetSVGType of
        svgUse, svgLink, svgTextPath:
          begin
            vSVGLink := TsgSVGLink(vContainer);
            if vSVGLink.Eref = nil then
            begin
              vSVGLink.Eref := TsgSVGContainer(FindEntity(vSVGLink.Owner, vSVGLink.Href));
              if vSVGLink.Eref is vSVGLink.GetUsingClass then
                Result := True
              else
                vSVGLink.Eref := nil;
            end;
          end;
        svgGroup, svgGroupDim:
          begin
            if FindHrefs(vContainer.Block) then
              Result := True;
          end;
      end;
    end;
  end;
end;

procedure TsvgReader.FreeGroup(var AGroup);
var
  vBlock: TsgDXFBlock;
  I: Integer;
begin
  vBlock := TsgSVGContainer(AGroup).Block;
  I := FConv.Sections[csBlocks].IndexOfEntity(vBlock);
  if I >= 0 then
    FConv.Sections[csBlocks].DeleteEntity(I);
  TsgSVGContainer(AGroup).BlockRecord := nil;
  vBlock.Free;
  FreeAndNil(TsgSVGContainer(AGroup));
end;

procedure TsvgReader.LoadEntities;
begin
  FSVGReload := False;
  FNodesCacl := True;
  try
    if FSVGSize <> nil then
      Dispose(FSVGSize);
    LoadNodes;
  finally
    FNodesCacl := False;
    FNodesStep := cnstPercentUpdateProgress * FNodesCounter div 100;
  end;
  LoadNodes;
  ClearEmptyBlocks;
  if FSVGReload then
    ReLoadEntities;
end;

procedure TsvgReader.LoadNodes;
var
  I: Integer;
begin
  for I := 0 to FParser.ROOT.Count - 1 do
    LoadStyles(FParser.ROOT.Nodes[I]);
  for I := 0 to FParser.ROOT.Count - 1 do
    AnaliseNode(nil, FParser.ROOT.Nodes[I]);
end;

function TsvgReader.NeedExtendedInfo: Boolean;
begin
  Result := Assigned(FParserExtended);
end;

function TsvgReader.Parsing: Boolean;
begin
  Result := False;
  FParser.OnProgress := FOnProgress;
  FParser.StopRead := Stopped;
  if FDebugInfo then
  begin
    FParser.DebugInfo := True;
    FParser.DebugInfoMode := DebugInfoMode;
  end;
  RegisterClasses;
  try
    if FParser.LoadFromStream(FStream) and (not Stopped) then
      Result := True;
  except
    raise EsvgParsedError.Create(cnstParsingError);
  end;
end;

procedure TsvgReader.ReLoadEntities;
var
  vReCalckDefs, vReCalckPaper: Boolean;
begin
  vReCalckDefs := False;
  vReCalckPaper := False;
  if (FConv.LayoutsCount > 0) then
  begin
    if SVGDefs <> nil then
      if FindHrefs(SVGDefs.Block) then
      begin
        LoadEntitiesInBlock(FConv, SVGDefs.Block);
        FConv.Loads(SVGDefs.Block);
        FConv.Loads(SVGDefs);
        SVGDefs.Block.IsLoaded := False;
        vReCalckDefs := True;
      end;
    if FindHrefs(FConv.Layouts[0].PaperSpaceBlock) then
       vReCalckPaper := True;
    if vReCalckPaper or vReCalckDefs then
    begin
      LoadEntitiesInBlock(FConv, FConv.Layouts[0].PaperSpaceBlock);
      FConv.DoExtents;
    end;
  end;
end;

procedure TsvgReader.LoadStyles(const ANode: TsgNodeSample);
var
  I: Integer;
begin
  if (ANode.ClassID = cnstClassID_SVG) and (TsvgNode(ANode).SVGNodeID = niSTYLE) then
  begin
     if FNodesCacl then
       Inc(FNodesCounter)
     else
     begin
       TsvgNode_STYLE(ANode).SetReader(Self);     
       TsvgNode_STYLE(ANode).Apply;
       UpdateProgress;
     end;
  end
  else
    if ANode.HasChildNodes then
      for I := 0 to ANode.ChildNodesCount - 1 do
        LoadStyles(ANode.ChildNodes[I]);
end;

procedure TsvgReader.ReadFile;
var
  vParsed: Boolean;
begin
  Msg := sLoadFile;
  FParser := FParserExtended;
  if not Assigned(FParser) then
    FParser := TsgParser.Create;
  try
    FParser.NodeTextMode := tmFullText;
    vParsed := Parsing;
    if vParsed then
    begin
      BeginUpdateProgress;
      try
        try
          LoadEntities;
        except
          raise EsvgLoadError.Create(cnstLoadError);
        end;
      finally
        EndUpdateProgress;
      end;
    end;
  finally
    FParser.OnProgress := nil;
    FParser.StopRead := nil;
    if FParserExtended <> FParser then
      FreeAndNil(FParser);
  end;
end;

procedure TsvgReader.RegisterClasses;
begin
  FParser.RegisterClass(cnstSVGNodeName[niA], TsvgNode_A);
  FParser.RegisterClass(cnstSVGNodeName[niSvg], TsvgNode_SVG);
  FParser.RegisterClass(cnstSVGNodeName[niG], TsvgNode_G);
  FParser.RegisterClass(cnstSVGNodeName[niCircle], TsvgNode_CIRCLE);
  FParser.RegisterClass(cnstSVGNodeName[niEllipse], TsvgNode_ELLIPSE);
  FParser.RegisterClass(cnstSVGNodeName[niLine], TsvgNode_LINE);
  FParser.RegisterClass(cnstSVGNodeName[niRect], TsvgNode_RECT);
  FParser.RegisterClass(cnstSVGNodeName[niPath], TsvgNode_PATH);
  FParser.RegisterClass(cnstSVGNodeName[niPolyline], TsvgNode_POLYLINE);
  FParser.RegisterClass(cnstSVGNodeName[niPolygon], TsvgNode_POLYGON);

  FParser.RegisterClass(cnstSVGNodeName[niText], TsvgNode_TEXT);
  FParser.RegisterClass(cnstSVGNodeName[niTspan], TsvgNode_TSPAN);
  FParser.RegisterClass(cnstSVGNodeName[niTref], TsvgNode_TREF);
  FParser.RegisterClass(cnstSVGNodeName[niTextpath], TsvgNode_TEXTPATH);

  FParser.RegisterClass(cnstSVGNodeName[niUse], TsvgNode_USE);
  FParser.RegisterClass(cnstSVGNodeName[niDefs], TsvgNode_DEFS);
  FParser.RegisterClass(cnstSVGNodeName[niSymbol], TsvgNode_SYMBOL);
  FParser.RegisterClass(cnstSVGNodeName[niStyle], TsvgNode_STYLE);
  FParser.RegisterClass(cnstSVGNodeName[niLGRADIENT], TsvgNode_LGRADIENT);
  FParser.RegisterClass(cnstSVGNodeName[niRGRADIENT], TsvgNode_RGRADIENT);
  FParser.RegisterClass(cnstSVGNodeName[niPattern], TsvgNode_PATTERN);
  FParser.RegisterClass(cnstSVGNodeName[niImage], TsvgNode_IMAGE);
  FParser.RegisterClass(cnstSVGNodeName[niSwitch], TsvgNode_SWITCH);
  FParser.RegisterClass(cnstSVGNodeName[niClippath], TsvgNode_CLIPPATH);
//  FParser.RegisterClass('set', TsvgNode_SET);//for future version   

  FParser.RegisterClass('html', TsgNodeHTML);
  FParser.RegisterClass('head', TsgNodeHTML);
  FParser.RegisterClass('body', TsgNodeHTML);
end;

procedure TsvgReader.SetSVGSize(const ASize: TF2DPoint);
begin
  SetF2DPointValue(FSVGSize, ASize);
end;

procedure TsvgReader.SetMsg(const AValue: string);
begin
  if FConv <> nil then
    FConv.FMsg := AValue;
end;

function TsvgReader.Stopped: Boolean;
begin
  Result := FConv.LoadStopped;
end;

procedure TsvgReader.SetParserExtened(const AParser: TsgParser);
begin
  FParserExtended := AParser;
end;

{ TsvgReaderZip }

procedure TsvgReaderZip.ReadFile;
var
  vStream: TStream;
  vDecomposeFile: TMemoryStream;
  vDecomposeFileName: string;
begin
  vStream := FStream;
  vDecomposeFile := TMemoryStream.Create;
  try
    if UnPackGZStream(FStream, vDecomposeFile, vDecomposeFileName) then
    begin
      FStream := vDecomposeFile;
      inherited ReadFile;
    end;
  finally
    FStream := vStream;
    vDecomposeFile.Free;
  end;
end;

procedure MoveToLast(const AID: TsgTagID; vIdList: TsgIntegerList; vBufferList: TsgObjectList);
var
  I, vCnt: Integer;
  vID: TsgTagID;
  vElement: TObject;
begin
  I := 0;
  vCnt := vIdList.Count;
  while I < vCnt do
  begin
    vID := TsgTagID(vIdList[I]);
    if vID = AID then
    begin
      vIdList.Delete(I);
      vElement := vBufferList[I];
      vBufferList.Delete(I);
      Dec(vCnt);
      vIdList.Add(Integer(vID));
      vBufferList.Add(vElement);
    end
    else
      Inc(I);
  end;
end;

procedure TsvgNode.ApplyAttribs(const AEntity: TsgDXFEntity;
  const ABaseIgnores: TsgTagIDs = []);
var
  vAttrib: TsgNodeSample;
  vID: TsgTagID;
  vStyleAttribs, I: Integer;
  vBufferList: TsgObjectList;
  vIdList: TsgIntegerList;
begin
  SetClass(AEntity, cnstBaseClassName[TsgSVGContainerAccess(AEntity).GetSVGType], True);
  if Assigned(FReader) and  TsvgReader(FReader).NeedExtendedInfo then
    SetSvgEntExtendedInfo(AEntity);
  if HasAttributeNodes then
  begin
    TsvgReader(FReader).GetBufferList(vBufferList, vIdList, AttributeNodesCount * 2);
    vStyleAttribs := 0;
    for I := 0 to AttributeNodesCount - 1 do
    begin
      vAttrib := AttributeNodes[I];
      vID := GetTagIdByName(vAttrib.Name);
      if vID > idUndefined then
      begin
        case vID of
          idClass: vStyleAttribs := vStyleAttribs or 1;
          idStyle: vStyleAttribs := vStyleAttribs or 2;
        end;
        vBufferList.Add(vAttrib);
        vIdList.Add(Integer(vID));
      end;
    end;
    if vStyleAttribs and 1 <> 0 then
      MoveToLast(idClass, vIdList, vBufferList);
    if vStyleAttribs and 2 <> 0 then
      MoveToLast(idStyle, vIdList, vBufferList);
    for I := 0 to vBufferList.Count - 1 do
    begin
      vID :=  TsgTagID(vIdList[I]);
      if not (vID in ABaseIgnores) then
      begin
        if SetSvgEntPropsBase(AEntity, vID, TsgNodeSample(vBufferList[I])) then
          vIdList[I] := Integer(idUndefined);
      end;
    end;
    for I := 0 to vBufferList.Count - 1 do
    begin
      vID := TsgTagID(vIdList[I]);
      if vID <> idUndefined then
        SetSvgEntProps(AEntity, vID, TsgNodeSample(vBufferList[I]));
    end;
  end;
end;

procedure TsvgNode.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsvgNode then
    FReader := TsvgNode(Obj).FReader;
end;

function TsvgNode.CheckText(const ANode: TsgNodeSample): Boolean;
begin
  Result := False;
  if (ANode.ClassID = cnstClassID_SVG) and (TsvgNode(ANode).SVGNodeID in [niTEXT, niTSPAN, niTREF, niTEXTPATH]) then
  begin
    Result := True;
    Text := Text + GetHandleByNode(ANode);
  end;
end;

function TsvgNode.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niNode;
end;

destructor TsvgNode.Destroy;
begin
  inherited Destroy;
end;

function TsvgNode.GetClassID: Cardinal;
begin
  Result := cnstClassID_SVG;
end;

procedure TsvgNode.SetClass(const AEntity: TsgDXFEntity; const AValue: string;
  const Base: Boolean; const ASVGNodeID: TsgSVGNodeID = niDefault);
var
  I, vIndex: Integer;
  vStyles: TsgStringList;
  S: string;
  vSVGNodeID: TsgSVGNodeID;
begin
  if Base then
  begin
    vIndex := TsvgReader(FReader).IndexStyleOfName(AValue);
    if vIndex > -1 then
      SetStyle(AEntity, TsvgReader(FReader).GetStyleValue(vIndex));
  end
  else
  begin
    vStyles := TsgStringList.Create;
    try
      SetStringsLineBreak(vStyles, AValue);
      SetStringsText(vStyles, AValue);
//see REC-SVG11-20030114/types.html
      for I := 0 to vStyles.Count - 1 do
      begin
        S := Trim(vStyles[I]);
        if Length(S) > 0 then
        begin
          if not CharInSet(S[1], [cnstStyleSeparator, '&']) then
            S := cnstStyleSeparator + S;
          vIndex := TsvgReader(FReader).IndexStyleOfName(S);
          if vIndex > -1 then
            SetStyle(AEntity, TsvgReader(FReader).GetStyleValue(vIndex))
          else
          begin
            if not cnstUseStyleSeparator then
            begin
              if S[1] = cnstStyleSeparator then
                Delete(S, 1, 1);
            end;
            vSVGNodeID := SVGNodeID;
            if ASVGNodeID <> niDefault then
              vSVGNodeID := ASVGNodeID;
            S := cnstSVGNodeName[vSVGNodeID] + S;
            vIndex := TsvgReader(FReader).IndexStyleOfName(S);
            if vIndex > -1 then
              SetStyle(AEntity, TsvgReader(FReader).GetStyleValue(vIndex))
          end;
        end;
      end;
    finally
      vStyles.Free;
    end;
  end;
end;

procedure AddValueInList(const AList: TFPointList; const AType: Byte; const AValue: Double);
var
  vPoint: TFPoint;
begin
  vPoint := cnstFPointZero;
  vPoint.V[AType] := AValue;
  AList.Add(vPoint);
end;

procedure TsvgNode.LoadFPointsArray(const AList: TFPointList;
  const AArrayValues: string; const AType: Byte; const AEntity: TsgDXFEntity);
var
  I: Integer;
  vValue: string;
  vIsMinus: Boolean;
  vParamsValue: array [Boolean] of Boolean;
  vKoef, vFontSize: Double;
begin
  FillChar(vParamsValue, Sizeof(vParamsValue), 0);
  vKoef := 1;
  if Assigned(AEntity) then
  begin
    vFontSize := TsgSVGContainer(AEntity).FontSize;
    vKoef := TsvgReader(FReader).GetKoefValue(GetUnitsMeasurements(AArrayValues), AType, vFontSize);
  end;
  for I := 1 to Length(AArrayValues) do
  begin
    case AArrayValues[I] of
      '0'..'9':  vValue := vValue + AArrayValues[I];
      '.', '-':  
         begin
           vIsMinus := AArrayValues[I] = '-';
           vParamsValue[vIsMinus] := not vParamsValue[vIsMinus];
           if vParamsValue[vIsMinus] then
             vValue := vValue + AArrayValues[I]
           else
           begin
             vParamsValue[not vIsMinus] := False;
             AddValueInList(AList, AType, vKoef * StrToVal(vValue));
             vValue := '';
           end;  
         end;  
    else
      if Length(vValue) > 0 then
      begin
        FillChar(vParamsValue, Sizeof(vParamsValue), 0);
        AddValueInList(AList, AType, vKoef * StrToVal(vValue));
        vValue := '';
      end;  
    end;
  end;
  if Length(vValue) > 0 then
    AddValueInList(AList, AType, vKoef * StrToVal(vValue));
end;

procedure TsvgNode.SetArrayValues(var AList: TFPointList; const AStates: TsgIntegerList;
  const AType: Byte; const AValue: string; const AEntity: TsgDXFEntity);
var
  I: Integer;
  vValues: TFPointList;
  vValue: TFPoint;
  vState: Integer;
begin
  vValues := TFPointList.Create;
  try
    vState := AType + 1;
    if AList <> nil then
      vValues.Capacity := AList.Count;
    LoadFPointsArray(vValues, AValue, AType, AEntity);
    if vValues.Count > 0 then
    begin
      if (AList = nil) or (AList.Count = 0) then
      begin
        if AList <> nil then
          AList.Free;
        AList := vValues;
        vValues := nil;
        if Assigned(AStates) then
        begin
          AStates.Count := 0;
          AStates.AppendConst(AList.Count, vState);
        end;
      end
      else
      begin
        I := 0;
        while I < vValues.Count - AList.Count do
        begin
          AList.Add(AList.Last);
          if Assigned(AStates) then
            AStates.Add(AType + 1);
          Inc(I);
        end;
        I := 0;
        while I < AList.Count do
        begin
          if I < vValues.Count then
            vValue := vValues[I]
          else
            vValue := vValues.Last;
          AList[I] := AddFPoint2D(AList[I], vValue);
          AStates[I] := AStates[I] or vState;
          Inc(I);
        end;
      end;
    end;
  finally
    FreeAndNil(vValues);
  end;
end;

procedure TsvgNode.SetDashArray(const AEntity: TsgDXFEntity; const AValue: string);
var
  vSVGEntity: TsgSVGEntity;
  vParams: TsgStringList;
  vLType: TsgDXFLineType;
  I, Cnt: Integer;
  vElement: TsgLTypeElement;
  vDash: Double;
begin
  vSVGEntity := TsgSVGEntity(AEntity);
  vParams := TsgStringList.Create;
  try
    SetStringsLineBreak(vParams, AValue);
    SetStringsText(vParams, AValue);
    Cnt := (vParams.Count shr 1) shl 1 - 1;
    if Cnt > 0 then
    begin
      vLType := TsgDXFLineType.Create;
      try
        vSVGEntity.StrokeDashArray := vLType;
        vLType.Name := 'SVGType' + IntToStr(TsvgReader(FReader).Conv.Counts[csLTypes] + 1);
        vLType.Lines.Uniform := 1;
        FillChar(vElement, SizeOf(vElement), 0);
        for I := 0 to Cnt do
        begin
          vDash := StrToVal(vParams[I]);
          vElement.Dash := Abs(vDash);
          if I mod 2 <> 0 then
            vElement.Dash := -vElement.Dash;
          vLType.Lines.Add(vElement);
        end;
        TsvgReader(FReader).Conv.Loads(vLType);
      finally
        TsvgReader(FReader).Conv.Sections[csLTypes].AddEntity(vLType);
      end;
    end;
  finally
    vParams.Free;
  end;
end;

procedure TsvgNode.SetStyle(const AEntity: TsgDXFEntity; const AValue: string);
var
  vAttrib: TsgNodeSample;
  vParams: TsgStringList;
  vValue, vSubString: string;
  I, J, vLength: Integer;
  vID: TsgTagID;  
begin
  vValue := Trim(AValue);
  vLength := Length(vValue);
  if vLength > 0 then
  begin
    if vValue[1] <> '&' then
    begin
      if vValue[vLength] <> cnstLineBreakStyle then
        vValue := vValue + cnstLineBreakStyle;
      vAttrib := TsgNodeAttrib.Create;
      vParams := TsgStringList.Create;
      try
        SetStringsLineBreak(vParams, vValue, cnstLineBreakStyle);
        SetStringsText(vParams, vValue);
        for I := 0 to vParams.Count - 1 do
        begin
          vSubString := vParams[I];
          vLength := Length(vSubString);
          if vLength > 0 then
          begin
            J := StringScan(':', vSubString, 1);
            if J > 0 then
            begin
              vAttrib.Name := AnsiLowerCase(Trim(Copy(vSubString, 1, J - 1)));
              vID := GetTagIdByName(vAttrib.Name);
              if vID > idUndefined then
              begin
                vAttrib.Value := Copy(vSubString, J + 1, vLength - J);
                SetSvgEntPropsBase(AEntity, vID, vAttrib);
              end;
            end;
          end;
        end;
      finally
        vParams.Free;
        vAttrib.Free;
      end;
    end
    else
    begin
      if vValue[vLength] = ';' then
        SetLength(vValue, vLength - 1);
      if Length(vValue) > 0 then
        SetClass(AEntity, vValue, False)
    end;
  end;
end;

procedure TsvgNode.SetSvgEntExtendedInfo(const AEntity: TsgDXFEntity);
begin
  if Assigned(AEntity) then
    TsgSVGEntity(AEntity).ExtendedInfo := TsgSVGExtendedInfo.Create;
end;

procedure TsvgNode.SetViewBox(var AViewBox: PsgViewBox; const AValue: string);
var
  vViewBox: TsgViewBox;
begin
  vViewBox := GetViewBox(AValue);
  if (vViewBox.Width > 0) and (vViewBox.Height > 0) then
  begin
    if AViewBox = nil then
    begin
      New(AViewBox);
      AViewBox^ := cnstDefViewBox;
    end;
    vViewBox.Mode := AViewBox^.Mode;
    AViewBox^ := vViewBox;
  end
  else
  begin
    if AViewBox <> nil then
    begin
      Dispose(AViewBox);
      AViewBox := nil;
    end;
  end;
end;

procedure TsvgNode.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
end;

function TsvgNode.SetSvgEntPropsBase(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample): Boolean;
var
  vEntity: TsgSVGEntity;
begin
  Result := True;
  vEntity := TsgSVGEntity(AEntity);
  case AID of
    idID:                vEntity.ID := Attrib.Value;
    idClass:             SetClass(vEntity, Attrib.Value, False);
    idStyle:             SetStyle(vEntity, Attrib.Value);
    idTransform:         vEntity.TransformMatrix := FMatXMat2D(vEntity.TransformMatrix,
                           GetTramsformMatrix(Attrib.Value));

    idStroke:            vEntity.StrokeColor := StrToColor(Attrib.Value);
    idStrokeWidth:       vEntity.StrokeWidth := ValueAsDoubleDim(Attrib, 0, AEntity);
    idStrokeLineCap:     vEntity.StrokeLineCap := GetLineCap(Attrib.Value);
    idStrokeLineJoin:    vEntity.StrokeLineJoin := GetLineJoin(Attrib.Value);
    idStrokeMiterlimit:  vEntity.StrokeMiterlimit := Attrib.ValueAsDouble;
    idStrokeDashArray:   SetDashArray(vEntity, Attrib.Value);
    idStrokeDashOffset:  vEntity.StrokeDashOffset := Attrib.ValueAsDouble;
    idStrokeOpacity:     vEntity.StrokeOpacity := Attrib.ValueAsDouble;
    idVectorEffect:      vEntity.VectorEffect := GetVectorEffect(Attrib.Value);

    idFill:              SetFill(vEntity, Attrib.Value);
    idFillOpacity:       vEntity.FillOpacity := Attrib.ValueAsDouble;
    idFillRule:          vEntity.FillRule := GetFillRule(Attrib.Value);

    idFont:              SetFont(vEntity, Attrib.Value);
    idFontFamily:        vEntity.FontName := GetFontNameFromFamily(Attrib.Value);
    idFontStyle:         vEntity.FontStyles := GetFontStyles(Attrib.Value);
    idFontWeight:        vEntity.FontWeight := GetFontWeight(Attrib.Value);
    idFontSize:          vEntity.FontSize :=  ValueAsDoubleDim(Attrib, 0, AEntity);
    idFontVariant:       vEntity.FontVariant := GetFontVariant(Attrib.Value);
    idFontStretch:       vEntity.FontStretch := GetFontStretch(Attrib.Value);
    idTextAnchor:        vEntity.TextAnchor := GetTextAnchor(Attrib.Value);
    idTextDecoration:    vEntity.TextDecoration := GetTextDecoration(Attrib.Value);

    idDisplay:           vEntity.Display := GetDisplayMode(Attrib.Value);
    idVisibility:        vEntity.EntVis := GetEntityVisible(Attrib.Value);
    idOpacity:           vEntity.Opacity := Attrib.ValueAsDouble;
  else
    Result := False;
  end;
end;

procedure TsvgNode.SetFill(const AEntity: TsgDXFEntity; const AValue: string);
var
  vURIName: string;
  vIndex: Integer;
  vFillColor: TsgFillColor;
begin
  vFillColor := cnstDefFillColor;
  vURIName := GetURIName(AValue);
  if Length(vURIName) > 0 then
  begin
    vIndex := TsvgReader(FReader).IndexSVGStyleFillOfName(vURIName);
    if vIndex > -1 then
    begin
      vFillColor.IsStyle := True;
      vFillColor.Style := TsgCADStyleFill(TsvgReader(FReader).SVGStyleFills.Objects[vIndex]);
    end;
  end
  else
    vFillColor.Color := StrToColor(AValue);
  TsgSVGEntity(AEntity).FillColor := vFillColor;
end;

procedure TsvgNode.SetFont(const AEntity: TsgDXFEntity; const AValue: string);
begin
//for future version
end;

procedure TsvgNode.SetReader(const AReader: Pointer);
begin
  FReader := AReader;
end;

 function TsvgNode.ValueAsDoubleDim(const Attrib: TsgNodeSample;
   const AType: Byte; const AEntity: Pointer): Double;
 begin
   Result := Attrib.ValueAsDouble;
   if Result <> 0 then
     Result := Result * TsvgReader(FReader).GetKoefValue(
       Attrib.UnitsOfMeasurements, AType, TsgSVGContainer(AEntity).FontSize);
 end;

{ TsvgNode_A }

function TsvgNode_A.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niA;
end;

function TsvgNode_A.GetDataInternal(const AOwner: TObject): TObject;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGGroup, TsgSVGContainer(AOwner));
  ApplyAttribs(TsgDXFEntity(Result));
  TsvgReader(FReader).AnalizeChildNodes(TsgSVGContainer(Result), Self);
  TsvgReader(FReader).Conv.Loads(TsgDXFEntity(Result));
end;

{ TsvgNode_SVG }

procedure TsvgNode_SVG.ApplyReadParams(const AEntity: TsgDXFEntity);
var
  vGroup: TsgSVGContainerAccess;
//  vGroupDim: TsgSVGGroupDim;
  vMatrix: TFMatrix;
begin
  vGroup := TsgSVGContainerAccess(AEntity);
  TsvgReader(FReader).Conv.Loads(vGroup.Block);
  vGroup.Block.IsLoaded := False;
//  if vGroup.GetSVGType = svgGroupDim then
//  begin
//    vGroupDim := TsgSVGGroupDim(vGroup);
//    if FViewBox <> nil then
//      vGroupDim.ViewBox := FViewBox^;
//    if FPoint1 <> nil then
//      vGroupDim.Point1 := MakeFPointFrom2D(FPoint1^);
//    if FSize <> nil then
//      vGroupDim.Size := MakeFPointFrom2D(FSize^);
//  end
//  else
  begin
    vMatrix := GetMatrixByViewBox(FViewBox, FPoint1, FSize, vGroup.Block.Box);
    vGroup.TransformMatrix := FMatXMat(vGroup.TransformMatrix, vMatrix);
  end;
end;

procedure TsvgNode_SVG.Assign(const Obj: TObject);
var
  vSVG: TsvgNode_SVG absolute Obj;
begin
  inherited Assign(Obj);
  if Obj is TsvgNode_SVG then
  begin
    if vSVG.FViewBox = nil then
      DisposeAndNil(FViewBox)
    else
    begin
      if FViewBox = nil then
        New(FViewBox);
      FViewBox^ := vSVG.FViewBox^;
    end;
    DisposeAndNil(FPoint1);
    DisposeAndNil(FSize);
    if vSVG.FPoint1 <> nil then
    begin
      New(FPoint1);
      FPoint1^ := vSVG.FPoint1^;
    end;
    if vSVG.FSize <> nil then
    begin
      New(FSize);
      FSize^ := vSVG.FSize^;
    end;
  end;
end;

destructor TsvgNode_SVG.Destroy;
begin
  if FViewBox <> nil then
    Dispose(FViewBox);
  if FPoint1 <> nil then
    Dispose(FPoint1);
  if FSize <> nil then
    Dispose(FSize);
  inherited Destroy;
end;

function TsvgNode_SVG.GetDataInternal(const AOwner: TObject): TObject;
var
  vGroup: TsgSVGGroup absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGGroupDim, TsgSVGContainer(AOwner));
  if AOwner = nil then
  begin
    vGroup.TransformMatrix := cnstCrossYMat;

    vGroup.FillColor := cnstDefFillColor;;
    vGroup.FillRule := cnstDefFillRule;
    vGroup.FillOpacity := cnstDefFillOpacity;

    vGroup.FontVariant := cnstDefFontVariant;
    vGroup.FontWeight := cnstDefFontWeight;
    vGroup.FontStretch := cnstDefFontStretch;
    vGroup.FontName := cnstDefFontFamily;
    vGroup.FontSize := cnstDefFontSize;
    vGroup.FontStyles := cnstDefFontStyles;
    vGroup.TextAnchor := cnstDefTextAnchor;
    vGroup.TextDecoration := cnstDefTextDecoration;    

    vGroup.StrokeColor := cnstDefStroke;
    vGroup.StrokeWidth := cnstDefStrokeWidth;
    vGroup.StrokeLineCap := cnstDefStrokeLineCap;
    vGroup.StrokeLineJoin := cnstDefStrokeLineJoin;
    vGroup.StrokeMiterlimit := cnstDefStrokeMiterlimit;
    vGroup.StrokeDashArray := cnstDefStrokeDashArray;
    vGroup.StrokeDashOffset := cnstDefStrokeDashOffset;
    vGroup.StrokeOpacity := cnstDefStrokeOpacity;

    vGroup.Display := cnstDefDisplay;
    vGroup.EntVis := cnstDefVisibility;
    vGroup.Opacity := cnstDefOpacity;
  end;
  ApplyAttribs(vGroup);

  if (AOwner = nil) and (FSize <> nil) then
    TsvgReader(FReader).SetSVGSize(FSize^);

  TsvgReader(FReader).AnalizeChildNodes(vGroup, Self);
  ApplyReadParams(vGroup);

//  TsvgReader(FReader).Conv.Loads(vGroup.Block);
  TsvgReader(FReader).Conv.Loads(vGroup);
end;

function TsvgNode_SVG.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niSVG;
end;

{ TsvgNode_STYLE }

procedure TsvgNode_STYLE.AddStyles(const AStr: string);

  procedure AddSvgStyle(AFullName: string; const AValue: string);
  var
    vValue: string;
    vIndex, vPos4: Integer;
  begin
    if not cnstUseStyleSeparator then
    begin
      vPos4 := StringScan(cnstStyleSeparator, AFullName, 1);
      if vPos4 > 1 then
        Delete(AFullName, vPos4, 1);
    end;
    if Length(AFullName) > 0 then
    begin
      vIndex := TsvgReader(FReader).IndexStyleOfName(AFullName);
      if vIndex < 0 then
        TsvgReader(FReader).AddSVGStyle(AFullName, AValue)
      else
      begin
        vValue := TsvgReader(FReader).GetStyleValue(vIndex);
        if (Length(vValue) > 0) and (vValue[Length(vValue)] <> ';')then
          vValue := vValue + ';';
        TsvgReader(FReader).DeleteStyleByIndex(vIndex);
        TsvgReader(FReader).AddSVGStyle(AFullName, vValue + AValue);
      end;
    end;
  end;

var
  I, vPos1, vPos2, vPos3: Integer;
  vFullName, vValue: string;
  vNames: TsgStringList;
begin
  vNames := TsgStringList.Create;
  try
    vPos1 := 1;
    vPos2 := StringScan('{', AStr, 1);
    vPos3 := StringScan('}', AStr, vPos2 + 1);
    while (vPos2 > 0) and (vPos3 > 0) do
    begin
      vFullName := Trim(Copy(AStr, vPos1, vPos2 - vPos1));
      vValue := Copy(AStr, vPos2 + 1, vPos3 - vPos2 - 1);
      vPos1 := vPos3 + 1;
      vPos2 := StringScan('{', AStr, vPos1);
      vPos3 := StringScan('}', AStr, vPos2 + 1);
      if (Length(vFullName) > 0) and (Length(vValue) > 0) then
      begin
        if StringScan(',', vFullName, 1) > 0 then
        begin
          SetStringsLineBreak(vNames, vFullName);
          SetStringsText(vNames, vFullName);
          for I := 0 to vNames.Count - 1 do
          begin
            vFullName := Trim(vNames[I]);
            AddSvgStyle(vFullName, vValue);
          end;
        end
        else
          AddSvgStyle(vFullName, vValue);
      end;
    end;
  finally
    vNames.Free;
  end;
end;

procedure TsvgNode_STYLE.Apply;
var
  I: Integer;
  vNode: TsgNodeSample;
  vStyles: string;
begin
  if HasChildNodes then
  begin
    for I := 0 to ChildNodesCount - 1 do
    begin
      vNode := ChildNodes[I];
      if vNode.NodeType = ntCData then
      begin
        vStyles := Trim(vNode.Value);
        ClearComments(vStyles);
        if Length(vStyles) > 0 then
          AddStyles(vStyles);
      end;
    end;
  end
  else
  begin
    vStyles := Text;
    ClearComments(vStyles);
    if Length(vStyles) > 0 then
      AddStyles(vStyles);
  end;  
end;

procedure TsvgNode_STYLE.ClearComments(var AStr: string);
var
  vIndex1, vIndex2: Integer;

  procedure DelComment(const ALen: Integer);
  begin
    Delete(AStr, vIndex1, vIndex2 - vIndex1 + ALen);
    vIndex2 := vIndex1;
  end;

begin
  vIndex2 := 1;
  while Length(AStr) > 1 do
  begin
    vIndex1 := StringPos('<!--', AStr, vIndex2);
    if vIndex1 > 0 then
    begin
      vIndex2 := StringPos('-->', AStr, vIndex1 + 4);
      if vIndex2 > 0 then
        DelComment(3)
      else
      begin
        vIndex2 := StringPos('>', AStr, vIndex1 + 4);
        if vIndex2 > 0 then
          DelComment(1)
        else
          AStr := '';
      end;
    end
    else
    begin
      vIndex1 := StringPos('/*', AStr, vIndex2);
      if vIndex1 > 0 then
      begin
        vIndex2 := StringPos('*/', AStr, vIndex1 + 2);
        if vIndex2 > 0 then
          DelComment(2)
        else
          AStr := '';      
      end
      else
        Break;
    end;
  end;
end;

function TsvgNode_STYLE.GetDataInternal(const AOwner: TObject): TObject;
begin
  Result := nil;
end;

function TsvgNode_STYLE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niSTYLE;
end;

{ TsvgNode_DEFS }

function TsvgNode_DEFS.GetDataInternal(const AOwner: TObject): TObject;
begin
  Result := nil;
  TsvgReader(FReader).AnalizeChildNodes(TsvgReader(FReader).SVGDefs, Self);
  TsvgReader(FReader).Conv.Loads(TsvgReader(FReader).SVGDefs);
end;

function TsvgNode_DEFS.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niDEFS;
end;

procedure TsvgNode_DEFS.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin

end;

procedure TsvgNode_SVG.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idX:       SetF2DPointCoord(FPoint1, 0, ValueAsDoubleDim(Attrib, 0, AEntity));
    idY:       SetF2DPointCoord(FPoint1, 1, ValueAsDoubleDim(Attrib, 1, AEntity));
    idWidth:   SetF2DPointCoord(FSize, 0, ValueAsDoubleDim(Attrib, 0, AEntity));
    idHeight:  SetF2DPointCoord(FSize, 1, ValueAsDoubleDim(Attrib, 1, AEntity));
    idViewBox: SetViewBox(FViewBox, Attrib.Value);
    idPreserveAspectRatio:
      begin
        if FViewBox = nil then
        begin
          New(FViewBox);
          FViewBox^ := cnstDefViewBox;
        end;
        FViewBox^.Mode := GetPreserveAspectRatio(Attrib.Value);
      end;
  end;
end;

{ TsvgNode_G }

procedure TsvgNode_G.ApplyReadParams(const AEntity: TsgDXFEntity);
begin
end;

procedure TsvgNode_G.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsvgNode_G then
    FClipPath := TsvgNode_G(Obj).FClipPath;
end;

function TsvgNode_G.GetDataInternal(const AOwner: TObject): TObject;
var
  vGroup: TsgSVGGroup absolute Result;
  vIndex: Integer;
  vList: TsgObjectList;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGGroup, TsgSVGContainer(AOwner));
  ApplyAttribs(vGroup);
  TsvgReader(FReader).AnalizeChildNodes(vGroup, Self);
  ApplyReadParams(vGroup);
  if Length(FClipPath) > 0 then
  begin
    vIndex := TsvgReader(FReader).SVGClipPaths.IndexOf(FClipPath);
    if vIndex > -1 then
    begin
      vList := TsgObjectList(TsvgReader(FReader).SVGClipPaths.Objects[vIndex]);
      vGroup.CreateClipPoints(vList);
    end;
  end;
  TsvgReader(FReader).Conv.Loads(vGroup);
end;

function TsvgNode_G.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niG;
end;

function TsvgNode_G.SetSvgEntPropsBase(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample): Boolean;
begin
  Result := inherited SetSvgEntPropsBase(AEntity, AID, Attrib);
  if not Result then
  begin
    case AID of
      idClipPath:
        begin
          FClipPath := GetURIName(Attrib.Value);
          Result := True;
        end;
    end;
  end;
end;

{ TsvgNode_SYMBOL }

procedure TsvgNode_SYMBOL.ApplyReadParams(const AEntity: TsgDXFEntity);
var
  vGroup: TsgSVGGroup;
begin
  vGroup := TsgSVGGroup(AEntity);
  vGroup.Visibility := False;
  if FViewBox <> nil then
    vGroup.ViewBox := FViewBox^;
end;

function TsvgNode_SYMBOL.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niSYMBOL;
end;

procedure TsvgNode_SYMBOL.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idViewBox:  SetViewBox(FViewBox, Attrib.Value);
    idPreserveAspectRatio:
      begin
        if FViewBox = nil then
        begin
          New(FViewBox);
          FViewBox^ := cnstDefViewBox;
        end;
        FViewBox^.Mode := GetPreserveAspectRatio(Attrib.Value);
      end;
  else
    inherited SetSvgEntProps(AEntity, AID, Attrib);
  end;
end;

{  TsvgNode_SWITCH  }

function TsvgNode_SWITCH.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niSWITCH;
end;

(*
function TsvgNode_SWITCH.GetDataInternal(const AOwner: TObject): Pointer;
var
  I: Integer;
  vSwitch: TsgSVGGroup absolute Result;
begin
  vSwitch := TsgSVGGroup.Create;
  vSwitch.Owner := AOwner;
  ApplyChilds(vSwitch);
  for I := 1 to vSwitch.Block.Count - 1 do
    vSwitch.Block.Entities[I].Visibility := False;
  TsvgReader(FReader).Conv.Loads(vSwitch);
end;
*)
function TsvgNode_SWITCH.GetDataInternal(const AOwner: TObject): TObject;
var
  I, J: Integer;
  vSwitch: TsgSVGContainer;
  vLangNode: TsgNodeAttrib;
  vNode, vSwitchedNode: TsgNodeSample;
  vNodes, vNodeByName: TStringList;

  procedure DoInvisible(AIns: TsgDXFInsert; const ADeep: Integer = MaxInt);
  var
    L: Integer;
  begin
    AIns.Visibility := False;
    if ADeep > 0 then
      for L := 0 to AIns.Block.Count - 1 do
        if (AIns.Block[L].EntClass.EG = gtSVG) and (AIns.Block[L].EntType = ceInsert) then
          DoInvisible(TsgDXFInsert(AIns.Block[L]), ADeep - 1)
        else
          AIns.Block[L].Visibility := False;
  end;

begin
  Result := nil;
  if HasChildNodes then
  begin
    vSwitch := TsgSVGContainer(AOwner);
    // or place to new group
    // TsvgReader(FReader).CreateGroup(TsgSVGGroup, TsgSVGContainer(AOwner), vSwitch);
    vNodes := TStringList.Create;
    try
      for I := 0 to ChildNodesCount - 1 do
        if not ChildNodes[I].HasAttribute('requiredExtensions') then// skip downloading extensions
        begin
          J := vNodes.IndexOf(ChildNodes[I].Name);
          if J < 0 then
            J := vNodes.AddObject(ChildNodes[I].Name, TStringList.Create);
          vNodeByName := TStringList(vNodes.Objects[J]);
          vNodeByName.AddObject(HexDisplayPrefix + IntToHex(vNodeByName.Count, 0), ChildNodes[I]);
        end;
      for I := 0 to vNodes.Count - 1 do
      begin
        vNodeByName := TStringList(vNodes.Objects[I]);
        vSwitchedNode := nil;
        J := 0;
        while (J < vNodeByName.Count) and not Assigned(vSwitchedNode) do
        begin
          vNode := TsgNodeSample(vNodeByName.Objects[J]);
          vLangNode := TsgNodeAttrib(vNode.GetAttributeByName('systemLanguage'));
          if Assigned(vLangNode) then
          begin
            if (vLangNode.Value = LocaleName) or (vLangNode.Value = LocaleNameFamily) then
              vSwitchedNode := vNode;
          end
          else
            vSwitchedNode := vNode;
          TsvgReader(FReader).AnaliseNode(vSwitch, vNode);
          if not Assigned(vSwitchedNode) then
            if vSwitch.Block[vSwitch.Block.Count - 1] is TsgSVGContainer then
              DoInvisible(TsgSVGContainer(vSwitch.Block[vSwitch.Block.Count - 1]), MaxInt);
          Inc(J);
        end;
      end;
      // if placed into new group then
      // Result := vSwitch;
    finally
      ClearObjects(vNodes);
      vNodes.Free;
    end;
  end;
end;

{  TsvgNode_CLIPPATH  }

function TsvgNode_CLIPPATH.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niClipPath;
end;

function TsvgNode_CLIPPATH.GetDataInternal(const AOwner: TObject): TObject;
var
  vGroup: TsgSVGGroup;
  vList: TsgObjectList;
  vPoints: TsgClipPathPoints;
  vPolyPoints: TFPointList;
  vSVGEnt: TsgSVGContainerAccess;
  vEnt: TsgDXFEntity;
  I, J, K, Cnt: Integer;
  vMatrix: TFMatrix;
begin
  Result := nil;
  vGroup := TsgSVGGroup(TsvgReader(FReader).CreateGroup(TsgSVGGroup, TsgSVGContainer(AOwner)));
  try
    // realy needed for free block on vGroup destroy!!!!!
    TsgDXFBlockAccess(vGroup.Block).SetConverter(TsvgReader(FReader).Conv);
    // realy needed for free block on vGroup destroy!!!!!

    ApplyAttribs(vGroup);
//by create edge
    vGroup.FillColor := GetFillColorEx(clNone);
    vGroup.StrokeColor := clBlack;
//
    if Length(vGroup.ID) > 0 then
    begin
      Cnt := 0;
      TsvgReader(FReader).AnalizeChildNodes(vGroup, Self);
      vList := TsgObjectList.Create;
      try
        for I := 0 to vGroup.Block.Count - 1 do
        begin
          vSVGEnt := TsgSVGContainerAccess(vGroup.Block.Entities[I]);
          case vSVGEnt.GetSVGType of
            svgPath, svgRect, svgPolyline, svgCircle, svgEllipse:
               begin
                 vMatrix := FMatXMat2D(vGroup.TransformMatrix, vSVGEnt.TransformMatrix);
                 for J := 0 to vSVGEnt.Block.Count - 1 do
                 begin
                   vEnt := vSVGEnt.Block.Entities[J];
                   if vEnt is TsgCADBasePolyline then
                   begin
                     vPolyPoints := TsgCADBasePolyline(vEnt).PolyPoints;
                     vPoints := TsgClipPathPoints.Create;
                     vList.Add(vPoints);
                     vPoints.Capacity := vPolyPoints.Count;
                     for K := 0 to vPolyPoints.Count - 1 do
                       vPoints.Add(FPointXMat2D(vPolyPoints[K], vMatrix));
                     TsgCADBasePolyline(vEnt).PolyPoints.Count := 0;
                     Inc(Cnt, vPoints.Count);
                   end;
                 end;
               end;
          end;
        end;
      finally
        if Cnt > 0 then
          TsvgReader(FReader).AddSVGClipPath(vGroup.ID, vList)
        else
          TsgObjectList.FreeList(vList);
      end;
    end;  
  finally
    TsvgReader(FReader).FreeGroup(vGroup);
  end;
end;

{ TsvgNode_SET }

function TsvgNode_SET.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niSet;
end;

function TsvgNode_SET.GetDataInternal(const AOwner: TObject): TObject;
const
  cnstAttribCnt = 3;
  cnstAttribName: array [0..cnstAttribCnt] of string = ('attributename', 'attributetype', 'to' ,'begin');
var
  I, J: Integer;
  vAttribs: array[0..cnstAttribCnt] of TsgNodeSample;
  vAttrib: TsgNodeSample;
  vName: string;
  vID: TsgTagID;
begin
  Result := nil;
  if not HasAttributeNodes then Exit;
  FillChar(vAttribs, Sizeof(vAttribs), 0);
  for I := 0 to AttributeNodesCount - 1 do
  begin
    vName := AnsiLowerCase(Trim(AttributeNodes[I].Name));
    for J := 0 to cnstAttribCnt do
    begin
      if vName = cnstAttribName[J] then
      begin
        vAttribs[J] := AttributeNodes[I];
        Break;
      end;
    end;
  end;
  if (vAttribs[0] <> nil) and (vAttribs[2] <> nil) and (vAttribs[3] <> nil) then
  begin
    if AnsiLowerCase(Trim(vAttribs[3].Value)) = 'mouseout' then
    begin
      vAttrib := TsgNodeAttrib.Create;
      try
        vAttrib.FullName := vAttribs[0].Value;
        vAttrib.Value := vAttribs[2].Value;
        vID := GetTagIdByName(vAttrib.Name);
        if vID > idUndefined then
        begin
          if not SetSvgEntPropsBase(TsgDXFEntity(AOwner), vID, vAttrib) then
            SetSvgEntProps(TsgDXFEntity(AOwner), vID, vAttrib);
        end;
      finally
        vAttrib.Free
      end;
    end;  
  end;
end;

{ TsvgNode_Circle }

function TsvgNode_CIRCLE.GetDataInternal(const AOwner: TObject): TObject;
var
  vCircle: TsgSVGCircle absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGCircle, TsgSVGContainer(AOwner));
  ApplyAttribs(vCircle);
  TsvgReader(FReader).Conv.Loads(vCircle);
end;

function TsvgNode_CIRCLE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niCIRCLE;
end;

procedure TsvgNode_CIRCLE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idCX:  TsgSVGCircle(AEntity).X1 := ValueAsDoubleDim(Attrib, 0, AEntity);
    idCY:  TsgSVGCircle(AEntity).Y1 := ValueAsDoubleDim(Attrib, 1, AEntity);
    idR:   TsgSVGCircle(AEntity).Radius := ValueAsDoubleDim(Attrib, 2, AEntity);
  end;
end;

{ TsvgNode_ELLIPSE }

function TsvgNode_ELLIPSE.GetDataInternal(const AOwner: TObject): TObject;
var
  vEllipse: TsgSVGEllipse absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGEllipse, TsgSVGContainer(AOwner));
  ApplyAttribs(vEllipse);
  TsvgReader(FReader).Conv.Loads(vEllipse);
end;

function TsvgNode_ELLIPSE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niELLIPSE;
end;

procedure TsvgNode_ELLIPSE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  inherited SetSvgEntProps(AEntity, AID, Attrib);
  case AID of
    idRX: TsgSVGEllipse(AEntity).RX := ValueAsDoubleDim(Attrib, 0, AEntity);
    idRY: TsgSVGEllipse(AEntity).RY := ValueAsDoubleDim(Attrib, 1, AEntity);
  end;
end;

{ TsvgNode_LINE }

function TsvgNode_LINE.GetDataInternal(const AOwner: TObject): TObject;
var
  vLine: TsgSVGLine absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGLine, TsgSVGContainer(AOwner));
  ApplyAttribs(vLine);
  TsvgReader(FReader).Conv.Loads(vLine);
end;

function TsvgNode_LINE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niLINE;
end;

procedure TsvgNode_LINE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idX1:  TsgSVGLine(AEntity).X1 := ValueAsDoubleDim(Attrib, 0, AEntity);
    idY1:  TsgSVGLine(AEntity).Y1 := ValueAsDoubleDim(Attrib, 1, AEntity);
    idX2:  TsgSVGLine(AEntity).X2 := ValueAsDoubleDim(Attrib, 0, AEntity);
    idY2:  TsgSVGLine(AEntity).Y2 := ValueAsDoubleDim(Attrib, 1, AEntity);
  end;
end;

{ TsvgNode_RECT }

function TsvgNode_RECT.GetDataInternal(const AOwner: TObject): TObject;
var
  vRect: TsgSVGRect absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGRect, TsgSVGContainer(AOwner));
  ApplyAttribs(vRect);
  TsvgReader(FReader).Conv.Loads(vRect);
end;

function TsvgNode_RECT.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niRECT;
end;

procedure TsvgNode_RECT.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idX:       TsgSVGRect(AEntity).X1 := ValueAsDoubleDim(Attrib, 0, AEntity);
    idY:       TsgSVGRect(AEntity).Y1 := ValueAsDoubleDim(Attrib, 1, AEntity);
    idWidth:   TsgSVGRect(AEntity).Width := ValueAsDoubleDim(Attrib, 0, AEntity);
    idHeight:  TsgSVGRect(AEntity).Height := ValueAsDoubleDim(Attrib, 1, AEntity);
    idRX:      TsgSVGRect(AEntity).RX := ValueAsDoubleDim(Attrib, 2, AEntity);
    idRY:      TsgSVGRect(AEntity).RY := ValueAsDoubleDim(Attrib, 2, AEntity);
  end;
end;

{ TsvgNode_POLYLINE }

procedure TsvgNode_POLYLINE.AddPoints(const AEntity: TsgDXFEntity; const APoints: string);
var
  I: Integer;
  vValue: string;
  vType: Boolean;
  vPoint: TFPoint;
begin
  vPoint := cnstFPointZero;
  vValue := '';
  vType := False;
  for I := 1 to Length(APoints) do
  begin
    case APoints[I] of
      '0'..'9':       vValue := vValue + APoints[I];
      '-', 'e', '.':  vValue := vValue + APoints[I];
      #32, #13, #10, #9, ',':
        begin
          if (Length(vValue) > 0) then
          begin
            vPoint.V[Integer(vType)] := StrToVal(vValue);
            vValue := '';
            if vType then
            begin
              TsgSVGPoly(AEntity).AddPoint(vPoint);
              vPoint := cnstFPointZero;
            end;
            vType := not vType;
          end;
        end;
    end;
  end;
  if vType and (Length(vValue) > 0) then
  begin
    vPoint.V[Integer(vType)] := StrToVal(vValue);
    TsgSVGPoly(AEntity).AddPoint(vPoint);
  end;
end;

function TsvgNode_POLYLINE.Closed: Boolean;
begin
  Result := False;
end;

function TsvgNode_POLYLINE.GetDataInternal(const AOwner: TObject): TObject;
var
  vPLine: TsgSVGPoly absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGPoly, TsgSVGContainer(AOwner));
  ApplyAttribs(vPLine);
  vPLine.Closed := Closed;
  TsvgReader(FReader).Conv.Loads(vPLine);
end;

function TsvgNode_POLYLINE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := nIPOLYLINE;
end;

procedure TsvgNode_POLYLINE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idPoints:  AddPoints(AEntity, Trim(Attrib.Value));
  end;
end;

{ TsvgNode_PATH }

procedure TsvgNode_PATH.AddPathSeg(const AEntity: TsgDXFEntity; const APathsSeg: string);
var
  vPath: TsgSVGPath;
begin
  vPath := TsgSVGPath(AEntity);
  vPath.SetData(APathsSeg);
end;

procedure TsvgNode_PATH.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
  if Obj is TsvgNode_PATH then
  begin
    FPathData := TsvgNode_PATH(Obj).FPathData;
    FPathLength := TsvgNode_PATH(Obj).FPathLength;
  end;
end;

function TsvgNode_PATH.GetDataInternal(const AOwner: TObject): TObject;
var
  vPath: SVG.TsgSVGPath absolute Result;
begin
  Result := TsvgReader(FReader).CreateGroup(SVG.TsgSVGPath, TsgSVGContainer(AOwner));
  ApplyAttribs(vPath);
  AddPathSeg(vPath, Trim(FPathData));
  TsvgReader(FReader).Conv.Loads(vPath);
end;

function TsvgNode_PATH.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niPATH;
end;

procedure TsvgNode_PATH.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idD:           FPathData := Attrib.Value;
    idPathLength:  FPathLength := Attrib.ValueAsDouble;
  end;
end;

{ TsvgNode_POLYGON }

function TsvgNode_POLYGON.Closed: Boolean;
begin
  Result := True;
end;

function TsvgNode_POLYGON.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niPOLYGON;
end;

{ TsvgNode_USE }

function TsvgNode_USE.GetDataInternal(const AOwner: TObject): TObject;
var
  vUse: TsgSVGUse absolute Result;
  vEnt: TsgDXFEntity;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGUse, TsgSVGContainer(AOwner));
  ApplyAttribs(vUse);
  if (Length(vUse.Href) > 0) and (not SameText(vUse.Href, vUse.ID)) then
  begin
    vUse.Eref := nil;
    vEnt := TsvgReader(FReader).FindEntity(AOwner, vUse.Href);
    if vEnt is vUse.GetUsingClass then
      vUse.Eref := TsgSVGContainer(vEnt);
    TsvgReader(FReader).Conv.Loads(vUse);
  end
  else
    TsvgReader(FReader).FreeGroup(vUse);
end;

function TsvgNode_USE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niUSE;
end;

procedure TsvgNode_USE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idX:       TsgSVGUse(AEntity).X1 := ValueAsDoubleDim(Attrib, 0, AEntity);
    idY:       TsgSVGUse(AEntity).Y1 := ValueAsDoubleDim(Attrib, 1, AEntity);
    idWidth:   TsgSVGUse(AEntity).Width := ValueAsDoubleDim(Attrib, 0, AEntity);
    idHeight:  TsgSVGUse(AEntity).Height := ValueAsDoubleDim(Attrib, 1, AEntity);
    idHref:    TsgSVGUse(AEntity).Href := GetBlockName(Attrib.Value);    
  end;
end;

{ TsvgNode_TEXT }

procedure TsvgNode_TEXT.SetTextPosition(const AText, ATextPrev: TsgSVGText);

  function GetPrevPoint(const APrev: TsgDXFEntity): TFPoint;
  var
    I: Integer;
    vDY: Double;
    vTextPrev: TsgSVGText absolute APrev;
  begin
    if APrev is TsgSVGText then
    begin
      if Assigned(vTextPrev.PointsD) then
      begin
        vDY := 0;
        for I := 0 to vTextPrev.PointsD.Count - 1 do
          vDY := vDY + vTextPrev.PointsD[I].Y;
      end
      else
        vDY := vTextPrev.PointD.Y;
      Result := MakeFPoint(APrev.Box.Right, vTextPrev.Point1.Y + vDY);
    end
    else
      Result := MakeFPoint(APrev.Box.Right, vTextPrev.Box.Bottom);
  end;

var
  vPositions: TsvgPositions;
  vS1: Integer;
  vP1: TFPoint;
  vTextPrev: TsgSVGText;
begin
  vPositions := TsvgPositions.Create;
  try
    if Assigned(ATextPrev) and not IsBadRect(ATextPrev.Box) then
    begin
      vP1 := GetPrevPoint(ATextPrev);
      GetTextPosition(vPositions, False);
      vPositions.Points1.Count := 0;
      vPositions.States1.Count := 0;
    end
    else
    begin
      vP1 := cnstFPointZero;
      if Assigned(FRootNode) and Assigned(FRootNode.FEnt) and (FRootNode.FEnt.Block.Count > 0) then
      begin
        vTextPrev := TsgSVGText(FRootNode.FEnt.Block[FRootNode.FEnt.Block.Count - 1]);
        if Assigned(vTextPrev) and not IsBadRect(vTextPrev.Box) then
          vP1 := GetPrevPoint(vTextPrev);
      end;
      GetTextPosition(vPositions);
      vS1 := 0;
      case vPositions.Points1.Count of
        0:  begin end;
        1:  TsvgPositions.SetPointWithState(vPositions.Points1.First, vPositions.States1.First, vP1, vS1);
      else
      end;
    end;
//    if (vPositions.Points1.Count = 0) and (Assigned(ATextPrev)) then
//      AText.TextAnchor := 0;
    case vPositions.Points1.Count of
      0, 1:  AText.Point1 := vP1;
    else
      AText.Points1 := vPositions.Points1;
    end;
    case vPositions.PointsD.Count of
      0:  AText.PointD :=  cnstFPointZero;    
      1:  AText.PointD := vPositions.PointsD[0];
    else
      AText.PointsD := vPositions.PointsD;
    end;
  finally
    vPositions.Free;
  end;
end;

procedure TsvgNode_TEXT.ApplyFullAttribs(const AEnt: TsgDXFEntity);
const
  cnstBaseIgnores: TsgTagIDs = [idTransform, idX, idY, idDX, idDY];
var
  I: Integer;
  vOwners: TsgObjectList;
  vOwner: TsvgNode_TEXT;
  vIgnores: TsgTagIDs;
begin
  vOwners := TsgObjectList.Create;
  try
    LoadOwners(vOwners);
    for I := vOwners.Count - 1 downto 0 do
    begin
      vOwner := TsvgNode_TEXT(vOwners[I]);
      vIgnores := cnstBaseIgnores;
      if not Assigned(vOwner.FRootNode) then
        Include(vIgnores, idTextAnchor);
      vOwner.ApplyAttribs(AEnt, vIgnores);
    end;
  finally
    vOwners.Free;
  end;
end;

procedure TsvgNode_TEXT.ClearTextPositions;
begin
  FreeAndNil(FPositions);
  FPositions := TsvgPositions.Create;
end;

destructor TsvgNode_TEXT.Destroy;
begin
  FreeAndNil(FPositions);
  inherited Destroy;
end;

function TsvgNode_TEXT.GetDataInternal(const AOwner: TObject): TObject;
var
  vText: string;
  vStrings: TsgStringList;
begin
  Result := nil;
  FreeAndNil(FPositions);
  FPositions := TsvgPositions.Create;
  vStrings := TsgStringList.Create;
  try
    vText := GetUseText;
    if Length(vText) > 0 then
    begin
      vStrings.LineBreak := cnstLineBreakText;
      vStrings.Text := vText;
      if Assigned(FRootNode) or (HasChildNodes and (vStrings.Count > 1)) then
        SetEntMultiText(AOwner, vStrings)
      else
      begin
        SetEntText(AOwner, vText);
        FreeAndNil(FPositions);
      end;
      Result := FEnt;
      if Assigned(Result) then
        TsvgReader(FReader).Conv.Loads(TsgDXFEntity(Result));
    end;
  finally
    vStrings.Free;
  end;
end;

function TsvgNode_TEXT.GetFontSize: Double;
var
  I: Integer;
  vOwners: TsgObjectList;
  vAttrib, vFontSizeAttrib: TsgNodeSample;
  vText: TsgSVGText;
begin
  Result := cnstDefFontSize;
  if FFontSize = 0 then
  begin
    vFontSizeAttrib := nil;
    vOwners := TsgObjectList.Create;
    try
      LoadOwners(vOwners);
      for I := vOwners.Count - 1 downto 0 do
      begin
        vAttrib := TsvgNode_TEXT(vOwners[I]).GetAttributeByTagID(idFontSize);
        if Assigned(vAttrib) then
          vFontSizeAttrib := vAttrib;
      end;
    finally
      vOwners.Free;
    end;
    if Assigned(vFontSizeAttrib) then
    begin
      vText := TsgSVGText.Create;
      try
        FFontSize := ValueAsDoubleDim(vFontSizeAttrib, 0, vText);
        if FFontSize = 0 then
          FFontSize := cnstDefFontSize;
      finally
        vText.Free;
      end;
    end
    else
      FFontSize := cnstDefFontSize;
  end;
end;

procedure TsvgNode_TEXT.SetEntText(const AOwner: Pointer; const AText: string);
begin
  FEnt := TsgSVGContainer(TsvgReader(FReader).CreateGroup(TsgSVGText, AOwner));
  TsgSVGText(FEnt).Text := Trim(AText);
  ApplyAttribs(FEnt);
  SetTextPosition(TsgSVGText(FEnt), nil);
end;

procedure TsvgNode_TEXT.SetAlignContainer(const AValue: Byte);
var
  vP1, vP2: TFPoint;
begin
  if AValue <> 0 then
  begin
    TsvgReader(FReader).Conv.Loads(FEnt.Block);
    if AValue = 1 then
      vP1 := MiddleFPoint2D(FEnt.Block.Box.BottomRight, FEnt.Block.Box.TopLeft)
    else
      vP1 := FEnt.Block.Box.BottomRight;
    vP2 := cnstFPointZero;
    if FPositions.Points1.Count > 0 then
      vP2 := FPositions.Points1.First;
    FEnt.TransformMatrix := FMatXMat2D(FEnt.TransformMatrix, StdMat(cnstFPointSingle, MakeFPoint(vP2.X - vP1.X, 0)));
  end;
end;

procedure TsvgNode_TEXT.SetEntMultiText(const AOwner: Pointer; const AStrings: TsgStringList);
var
  I, vEntCnt: Integer;
  vStr, vStrTrim: string;
  vText: TsgSVGText;
  vNode: TsgNodeSample;
  vRootNode: TsvgNode_TEXT;
  vRootEntity: TsgSVGGroup;
  vTextAnchor: Byte;
begin
  vTextAnchor := 0;
  FIgnoreAttribPositions := True;
  LoadTextPosition(FPositions, FEnt);
  if Assigned(FRootNode) then
  begin
    vRootNode := FRootNode;
    vRootEntity := TsgSVGGroup(FRootNode.FEnt);
    if FPositions.Points1.Count = 0 then
      FTextPrev := TsvgNode_TEXT(Owner).FTextPrev;
  end
  else
  begin
    vRootNode := Self;
    FEnt := TsgSVGContainer(TsvgReader(FReader).CreateGroup(TsgSVGGroup, AOwner));
    FEnt.TextAnchor := 0;
    vRootEntity := TsgSVGGroup(FEnt);
    ApplyAttribs(FEnt);
    if (FPositions.Points1.Count > 0) then
    begin
      vTextAnchor := FEnt.TextAnchor;
      FEnt.TextAnchor := 0;
    end;
  end;
  for I := 0 to AStrings.Count - 1 do
  begin
    vStr := AStrings[I];
    if IsHandleOfNode(vStr) then
    begin
      vNode := GetNodeByHandle(vStr);
      if vNode <> nil then
      begin
        if vNode is TsvgNode_TEXT then
          TsvgNode_TEXT(vNode).FRootNode := vRootNode;
        vEntCnt := vRootEntity.Block.Count;
        TsvgReader(FReader).AnaliseNode(vRootEntity, vNode);
        if vRootEntity.Block.Count > vEntCnt then
        begin
          FTextPrev := TsgSVGText(vRootEntity.Block[vRootEntity.Block.Count - 1]);
        end;
      end;
    end
    else
    begin
      vStrTrim := Trim(vStr);
      if Length(vStrTrim) > 0 then
      begin
        vText := TsgSVGText(TsvgReader(FReader).CreateGroup(TsgSVGText, nil));
        vText.Text := vStr;
        ApplyFullAttribs(vText);
        LoadTextPosition(FPositions, vText);
        SetTextPosition(vText, FTextPrev);
        vRootEntity.AddEntity(vText);
        TsvgReader(FReader).Conv.Loads(vText);
        FTextPrev := vText;
      end;
    end;
  end;
  if not Assigned(FRootNode) then
   SetAlignContainer(vTextAnchor);
end;

function TsvgNode_TEXT.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niTEXT;
end;

procedure TsvgNode_TEXT.GetTextPosition(const APositions: TsvgPositions;
 const AUseOwners: Boolean = True);
begin
  if (APositions.States1.Count = 0) or (APositions.States1.First = 0)  then
    TsvgPositions.Union(FPositions.Points1, FPositions.States1, APositions.Points1, APositions.States1);
  if (APositions.StatesD.Count = 0) or (APositions.StatesD.First = 0)  then
    TsvgPositions.Union(FPositions.PointsD, FPositions.StatesD,  APositions.PointsD, APositions.StatesD);
  if AUseOwners and Assigned(FRootNode) and ((APositions.States1.Count = 0) or (APositions.States1.First = 0)) then
    TsvgNode_TEXT(Owner).GetTextPosition(APositions);
end;

function TsvgNode_TEXT.GetOwnerBySetProps(const AOwner: Pointer): Pointer;
begin
  Result := nil;
end;

function TsvgNode_TEXT.GetUseText: string;
begin
  Result := Text;
end;

procedure TsvgNode_TEXT.LoadOwners(const AOwners: TsgObjectList);
var
  vOwner: TsvgNode_TEXT;
begin
  vOwner := Self;
  while Assigned(vOwner) do
  begin
    AOwners.Add(vOwner);
    if not Assigned(vOwner.FRootNode) then
      Break;
    vOwner := vOwner.Owner as TsvgNode_TEXT;
  end;
end;

procedure TsvgNode_TEXT.LoadTextPosition(const APositions: TsvgPositions;
  const AEntity: TsgDXFEntity);
var
  I: Integer;
  vId: TsgTagID;
  vAttrib: TsgNodeSample;
  vText: TsgDXFEntity;
begin
  APositions.Clear;
  vText := AEntity;
  try
    if not Assigned(vText) then
    begin
      vText := TsgSVGText.Create;
      TsgSVGText(vText).FontSize := GetFontSize;
    end;
    for I := 0 to AttributeNodesCount - 1 do
    begin
      vAttrib := AttributeNodes[I];
      vId := GetTagIdByName(vAttrib.Name);
      SetSvgEntPropsPositions(vText, APositions, vId, vAttrib);
    end;
  finally
    if vText <> AEntity then
      vText.Free;
  end;
end;

procedure TsvgNode_TEXT.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  if not FIgnoreAttribPositions then
    SetSvgEntPropsPositions(AEntity, FPositions, AID, Attrib);
end;

procedure TsvgNode_TEXT.SetSvgEntPropsPositions(const AEntity: TsgDXFEntity;
  const APositions: TsvgPositions; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idX:  SetArrayValues(APositions.Points1, APositions.States1, 0, Attrib.Value, AEntity);
    idY:  SetArrayValues(APositions.Points1, APositions.States1, 1, Attrib.Value, AEntity);
    idDX: SetArrayValues(APositions.PointsD, APositions.StatesD, 0, Attrib.Value, AEntity);
    idDY: SetArrayValues(APositions.PointsD, APositions.StatesD, 1, Attrib.Value, AEntity);
  end;
end;

{  TsvgNode_TEXTPATH  }

function TsvgNode_TEXTPATH.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niTEXTPATH;
end;

function TsvgNode_TEXTPATH.GetDataInternal(const AOwner: TObject): TObject;
var
  vText: TsgSVGTextPath absolute Result;
  vEnt: TsgDXFEntity;
begin
  Result := TsvgReader(FReader).CreateGroup(TsgSVGTextPath, TsgSVGContainer(AOwner));
  vText.Text := BuildFullText(Trim(Text));
  ApplyAttribs(vText);

  if (vText.StrokeColor = clNone) and (not vText.FillColor.IsStyle) and (vText.FillColor.Color <> clNone) then
    vText.StrokeColor := vText.FillColor.Color;//by better show

  if Length(vText.Href) > 0 then
  begin
    vText.Eref := nil;
    vEnt := TsvgReader(FReader).FindEntity(AOwner, vText.Href);
    if vEnt is vText.GetUsingClass then
    begin
      vText.Eref := TsgSVGContainer(vEnt);
      TsvgReader(FReader).Conv.Loads(vText);
    end;
  end
  else
    TsvgReader(FReader).FreeGroup(vText);
end;

procedure TsvgNode_TEXTPATH.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idHref:  TsgSVGTextPath(AEntity).Href := GetBlockName(Attrib.Value);
  end;
end;

{  TsvgNode_TSPAN  }

function TsvgNode_TSPAN.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niTSPAN;
end;

function TsvgNode_TSPAN.GetOwnerBySetProps(const AOwner: Pointer): Pointer;
begin
  Result := AOwner;
end;

{  TsvgNode_TREF  }

function TsvgNode_TREF.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niTREF;
end;

function TsvgNode_TREF.GetAllTexts(const AEntity: TsgDXFEntity): string;
var
  I: Integer;
begin
  Result := '';
  if AEntity is TsgSVGContainer then
  begin
    case TsgSVGContainerAccess(AEntity).GetSVGType of
      svgText:  Result := TsgSVGText(AEntity).Text;
      svgGroup, svgGroupDim, svgContainer:
        begin
          for I := 0 to TsgSVGGroup(AEntity).Block.Count - 1 do
            Result := Result + GetAllTexts(TsgSVGGroup(AEntity).Block.Entities[I])
        end;
      svgUse:  Result := GetAllTexts(TsgSVGUse(AEntity).Eref);
    end;
  end;  
end;

function TsvgNode_TREF.GetUseText: string;
var
  vEnt: TsgDXFEntity;
  vNode: TsgNodeSample;
  vFind: Boolean;
begin
  Result := '';
  if Length(FRef) > 0then
  begin
    vFind := False;
    if TsvgReader(FReader).SVGDefs <> nil then
    begin
      vEnt := GetEntityInBlock(FRef, TsvgReader(FReader).SVGDefs.Block);
      if vEnt <> nil then
      begin
        vFind := True;
        Result := GetAllTexts(vEnt)
      end;
    end;
    if not vFind then
    begin
      vNode := TsvgReader(FReader).GetNodeByID(FRef);
      if vNode <> nil then
        Result := TsvgReader(FReader).GetNodeAllTexts(vNode);
    end;
  end;
end;

procedure TsvgNode_TREF.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  if AID = idHref then
    FRef := GetBlockName(Attrib.Value)
  else
    inherited SetSvgEntProps(AEntity, AID, Attrib);
end;

{  TsvgNode_IMAGE  }

procedure TsvgNode_IMAGE.Assign(const Obj: TObject);
begin
  inherited Assign(Obj);
end;

function TsvgNode_IMAGE.GetImageByHref: TGraphic;
const
  cnstDataType = 'data:image';
  cnstFileNam = 'svgimage';
var
  I, J: Integer;
  vHead, vData, vFileExt, vFileName: string;
  vMemStream: TMemoryStream;
  vGraphic: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TInterfacedPersistent{$ENDIF};
  vClass: TClass;
  vPicture: TPicture;
begin
  Result := nil;
  I := AnsiPos(cnstDataType, FHRef);
  if I < 1 then
  begin
    vFileName := Trim(FHRef);
    if TsvgReader(FReader).AddImageName(vFileName) then
    begin
      try
      vPicture := TPicture.Create;
        try
          if not IsFileOnWeb(vFileName) then
            LoadPictureFromDisk(vPicture, vFileName)
    {$IFDEF SG_USE_INDY}
          else
            LoadPictureFromWEB(vPicture, vFileName)  // request INDY components
    {$ENDIF};
          if vPicture.Graphic <> nil then
          begin
            {$IFDEF SG_FIREMONKEY}
            vGraphic := vPicture.ExtractGraphic;
            {$ELSE}
            vGraphic := TGraphicClass(vPicture.Graphic.ClassType).Create;
            vGraphic.Assign(vPicture.Graphic);
            {$ENDIF}
            Result := TGraphic(vGraphic);
          end;
        finally
          vPicture.Free;
        end;
      finally
        TsvgReader(FReader).DelImageName(vFileName);
      end;
    end;
  end
  else
  begin
    J := StringScan(',', FHRef, I + 1);
    if J > 0 then
    begin
      vFileExt := '';
      vHead := Copy(FHRef, I, J - I);
      vData := Copy(FHRef, J + 1, Length(FHRef) - J);
      I := StringScan('/',  vHead, 1);
      if I > 0 then
      begin
        J := StringScan(';',  vHead, I + 1);
        if J > 0 then
          vFileExt := Trim(Copy(vHead, I + 1, J - I - 1));
      end;
      vMemStream := TMemoryStream.Create;
      try
        DecodeBase64(vData, nil, TStream(vMemStream));
        vMemStream.Position := 0;
        if vMemStream.Size > 0 then
        begin
          vClass := GetClassByFileExt(vFileExt);
          if vClass <> nil then
          begin
           {$IFDEF SG_FIREMONKEY}
            vGraphic := nil;
            if vClass.InheritsFrom(TGraphic) then
              vGraphic := TGraphicClass(vClass).Create
            else
              if vClass.InheritsFrom(TBitmap) then
                vGraphic := TBitmapClass(vClass).Create;
            {$ELSE}
            vGraphic := TGraphicClass(vClass).Create;
            {$ENDIF}
            try
              if Assigned(vGraphic) then
              begin
                try
                  {$IFDEF SG_FIREMONKEY}
                  if vGraphic is TBitmap then
                    TBitmap(vGraphic).LoadFromStream(vMemStream)
                  else
                    TGraphic(vGraphic).LoadFromStream(vMemStream);
                  {$ELSE}
                   vGraphic.LoadFromStream(vMemStream);
                  {$ENDIF}
                except
                  FreeAndNil(vGraphic);
                end;
                Result := TGraphic(vGraphic);
                vGraphic := nil;
              end;
            finally
              FreeAndNil(vGraphic);
            end;
          end
          else
          begin
            Result := TGraphic(OpenPictureByMemmoryStreamSafe(vMemStream, cnstFileNam, vFileExt));
          end;
        end;
      finally
        if vMemStream <> nil then
          vMemStream.Free;
      end;
    end;
  end;
end;

function TsvgNode_IMAGE.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niIMAGE;
end;

function TsvgNode_IMAGE.GetDataInternal(const AOwner: TObject): TObject;
var
  vImgEnt: TsgSVGImageEnt absolute Result;
  vImg: TGraphic;
begin
  FHRef := '';
  Result := TsvgReader(FReader).CreateGroup(TsgSVGImageEnt, TsgSVGContainer(AOwner));
  ApplyAttribs(vImgEnt);
  if Length(FHRef) > 0 then
  begin
    vImg := GetImageByHref;
    try
      vImgEnt.SetImage(vImg);
    finally
      vImg.Free;
    end;
    TsvgReader(FReader).CheckImageCounts(vImgEnt);
    TsvgReader(FReader).Conv.Loads(vImgEnt);
  end
  else
    TsvgReader(FReader).FreeGroup(vImgEnt);
end;

procedure TsvgNode_IMAGE.SetSvgEntProps(const AEntity: TsgDXFEntity; const AID: TsgTagID; const Attrib: TsgNodeSample);
begin
  case AID of
    idHref: FHref := Attrib.Value;
  else
    inherited SetSvgEntProps(AEntity, AID, Attrib);
  end;
end;


{ TsgSVGEntity }

{  TsgSVGGroup  }

procedure TsgSVGGroup.AssignEntity(Source: TsgDXFEntity);
var
  vGroup: TsgSVGGroup;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGGroup then
  begin
    vGroup := TsgSVGGroup(Source);
    if vGroup.FViewBox <> nil then
      ViewBox := vGroup.FViewBox^
    else
      DisposeAndNil(FViewBox);
    ClearClipPoints;
    FFlag := vGroup.FFlag;
    if Assigned(vGroup.FClipPoints) then
      CopyClipPoints(vGroup.FClipPoints);
  end;
end;

function TsgSVGGroup.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
var
  I, J: Integer;
  vMode: Word;
  vRation: TsgSVGPreserveAspectRatio;
begin
  inherited GetExtDataBody(AData);
  if FViewBox <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_ViewBoxMode);
    vMode := 0;
    J := 0;
    for vRation := Low(TsgSVGPreserveAspectRatio) to High(TsgSVGPreserveAspectRatio) do
    begin
      if vRation in FViewBox^.Mode then
        Inc(vMode, 1 shl J);
      Inc(J);
    end;
    AData.AddInt16(Integer_1070, vMode);
    AData.AddInt16(Integer_1070, cnstED_Point2D3);
    AData.AddPoint2D(Float_1010, FViewBox^.Point);
    AData.AddInt16(Integer_1070, cnstED_Point2D4);
    AData.AddPoint2D(Float_1010, FViewBox^.Size);
  end;
  AData.AddInt16(Integer_1070, cnstED_ClipPointsFlag);
  AData.AddByte(Integer_1070, FFlag and 253);//poinst no link
  AData.AddInt16(Integer_1070, cnstED_ClipPolyPointsCount);
  AData.AddInt(Integer_1071, FClipPoints.Count);
  for I := 0 to FClipPoints.Count - 1 do
    AData.AddListPoints(cnstED_PointsCount1, cnstED_Point2D1, TFPointList(FClipPoints[I]));
  Result := AData.DataCount > 0;
end;

procedure TsgSVGGroup.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);
var
  vList: TF2DPointList;
  J: Integer;
  vMode: Word;
  vRation: TsgSVGPreserveAspectRatio;
begin
  case ACode of
    cnstED_ViewBoxMode:
      begin
        CreateViewBox;
        vMode := AData.DataByte[AIndex];
        FViewBox^.Mode := [];
        J := 0;
        for vRation := Low(TsgSVGPreserveAspectRatio) to High(TsgSVGPreserveAspectRatio) do
        begin
          if vMode and (1 shl J) <> 0 then
            Include(FViewBox^.Mode, vRation);
          Inc(J);
        end;
      end;
    cnstED_Point2D3:
      begin
        CreateViewBox;
        FViewBox^.Point := AData.DataPoint2D[AIndex];
      end;
    cnstED_Point2D4:
      begin
        CreateViewBox;
        FViewBox^.Size := AData.DataPoint2D[AIndex];
      end;
    cnstED_ClipPointsFlag:  FFlag := AData.DataByte[AIndex];
    cnstED_PointsCount1:
      begin
        vList := TF2DPointList.Create;
        if FClipPoints = nil then
          FClipPoints := TsgObjectList.Create;
        FClipPoints.Add(vList);
        vList.Capacity := AData.DataInt[AIndex];
      end;
    cnstED_Point2D1:
      begin
        AddClipPoint(AData.DataPoint2D[AIndex]);
      end;
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

procedure TsgSVGGroup.AddClipCoords(const AX, AY: Double);
begin
  AddClipPoint(MakeF2DPoint(AX, AY));
end;

procedure TsgSVGGroup.AddClipPoint(const APoint: TF2DPoint);
begin
  if FClipPoints.Count = 0 then
    FClipPoints.Add(TsgClipPathPoints.Create);
  TsgClipPathPoints(FClipPoints.Last).Add(APoint);
end;

destructor TsgSVGGroup.Destroy;
begin
  ClearClipPoints;
  if FClipBox <> nil then
    Dispose(FClipBox);
  if FViewBox <> nil then
    Dispose(FViewBox);
  inherited Destroy;
end;

procedure TsgSVGGroup.CreateViewBox;
begin
  if FViewBox = nil then
  begin
    New(FViewBox);
    FViewBox^ := cnstViewBoxZero;
  end;
end;

procedure TsgSVGGroup.CreateClipPoints(const ALink: TsgObjectList);
begin
  ClearClipPoints;
  if FClipPoints = nil then
    FClipPoints := TsgObjectList.Create;
  if Assigned(ALink) then
    CopyClipPoints(ALink);
end;

procedure TsgSVGGroup.ClearClipPoints;
begin
  TsgObjectList.FreeList(FClipPoints);
  FFlag := 0;
end;

procedure TsgSVGGroup.CopyClipPoints(const AClipPoint: TsgObjectList);
var
  I: Integer;
  vListDest, vListSource: TsgClipPathPoints;
begin
  if FClipPoints = nil then
    FClipPoints := TsgObjectList.Create;
  FClipPoints.Capacity := AClipPoint.Count;
  for I := 0 to AClipPoint.Count - 1 do
  begin
    vListSource := TsgClipPathPoints(AClipPoint[I]);
    vListDest := TsgClipPathPoints.Create;
    FClipPoints.Add(vListDest);
    vListDest.Assign(vListSource);
  end;
end;

procedure TsgSVGGroup.Generate;
var
  I: Integer;
  vClipPathPoints: TsgClipPathPoints;
  vClipBox: TFRect;
begin
  Block.IsLoaded := False;
  Visibility := IsEntVisible;//and (Opacity > 0)
  if FClipPoints = nil then
    DisposeAndNil(FClipBox)
  else
  begin
    vClipBox := cnstBadRect;
    for I := 0 to FClipPoints.Count - 1 do
    begin
      vClipPathPoints := TsgClipPathPoints(FClipPoints[I]);
      UnionFRect(vClipBox, vClipPathPoints.GetBox);
    end;
    if IsBadRect(vClipBox) then
    begin
      if FClipBox <> nil then
        DisposeAndNil(FClipBox);
    end
    else
    begin
      if FClipBox = nil then
        New(FClipBox);
      FClipBox^ := vClipBox;
    end;
  end;
end;

function TsgSVGGroup.GetUsedMatrix: TFMatrix;
begin
  Result := inherited GetUsedMatrix;
  if (Block <> nil) and HasViewBox then
    Result := FMatXMat(StdMat(cnstFPointSingle, MakeFPoint(-FViewBox^.X, -FViewBox^.Y, 0)), Result);
end;

function TsgSVGGroup.GetBox: TFRect;
begin
  if FViewBox <> nil then
  begin
    Result := cnstFRectZero;
    ExpandFRect(Result, MakeFPointFrom2D(FViewBox^.Size));
  end
  else
    if FClipBox = nil then
      Result := inherited GetBox
    else
    begin
      Result := FClipBox^;
      TransRectCorners(Result, FMatrix);
    end;
end;

function TsgSVGGroup.GetPoint: TFPoint;
begin
  Result := cnstFPointZero;
end;

function TsgSVGGroup.GetSVGType: TsvgEntity;
begin
  Result := svgGroup;
end;

function TsgSVGGroup.GetViewBox: TsgViewBox;
begin
  if FViewBox = nil then
    Result := cnstViewBoxZero
  else
    Result := FViewBox^;
end;

function TsgSVGGroup.HasViewBox: Boolean;
begin
  Result := FViewBox <> nil;
end;

procedure TsgSVGGroup.SetViewBox(const AValue: TsgViewBox);
begin
  if (AValue.Width > 0) and (AValue.Height > 0) then
  begin
    CreateViewBox;
    FViewBox^ := AValue;
  end
  else
  begin
    if FViewBox <> nil then
    begin
      Dispose(FViewBox);
      FViewBox := nil;
    end;
  end;
end;

{  TsgSVGGroupDim  }

destructor TsgSVGGroupDim.Destroy;
begin
  if FPoint1 <> nil then
    Dispose(FPoint1);
  if FSize <> nil then
    Dispose(FSize);
  inherited Destroy;
end;

procedure TsgSVGGroupDim.AssignEntity(Source: TsgDXFEntity);
var
  vGroupDim: TsgSVGGroupDim absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGGroupDim then
  begin
    if vGroupDim.FPoint1 <> nil then
      SetF2DPointValue(FPoint1, vGroupDim.FPoint1^)
    else
      DisposeAndNil(FPoint1);
    if vGroupDim.FSize <> nil then
      SetF2DPointValue(FSize, vGroupDim.FSize^)
    else
      DisposeAndNil(FSize);
  end;
end;

function TsgSVGGroupDim.GetExtDataBody(
  const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  if FPoint1 <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_Point2D1);
    AData.AddPoint2D(Float_1010, FPoint1^);
  end;
  if FSize <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_Point2D2);
    AData.AddPoint2D(Float_1010, FSize^);
  end;
  Result := AData.DataCount > 0;
end;

procedure TsgSVGGroupDim.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_Point2D1:  SetF2DPointValue(FPoint1, AData.DataPoint2D[AIndex]);
    cnstED_Point2D2:  SetF2DPointValue(FSize, AData.DataPoint2D[AIndex]);
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

function TsgSVGGroupDim.GetSVGType: TsvgEntity;
begin
  Result := svgGroupDim;
end;

function TsgSVGGroupDim.GetX1: Double;
begin
  Result := GetF2DPointCoord(FPoint1, 0);
end;

function TsgSVGGroupDim.GetY1: Double;
begin
  Result := GetF2DPointCoord(FPoint1, 1);
end;

function TsgSVGGroupDim.GetWidth: Double;
begin
  Result := GetF2DPointCoord(FSize, 0);
end;

function TsgSVGGroupDim.GetHeight: Double;
begin
  Result := GetF2DPointCoord(FSize, 1);
end;

function TsgSVGGroupDim.GetPoint1: TFPoint;
begin
  Result := GetFPointFromP2D(FPoint1);
end;

function TsgSVGGroupDim.GetSize: TFPoint;
begin
  Result := GetFPointFromP2D(FSize);
end;

function TsgSVGGroupDim.GetUsedMatrix: TFMatrix;
var
  M: TFMatrix;
begin
  M := GetMatrixByViewBox(FViewBox, FPoint1, FSize, Block.Box);
  Result := FMatXMat(M, inherited GetUsedMatrix);
end;

procedure TsgSVGGroupDim.SetX1(const AValue: Double);
begin
  SetF2DPointCoord(FPoint1, 0, AValue);
end;

procedure TsgSVGGroupDim.SetY1(const AValue: Double);
begin
  SetF2DPointCoord(FPoint1, 1, AValue);
end;

procedure TsgSVGGroupDim.SetWidth(const AValue: Double);
begin
  SetF2DPointCoord(FSize, 0, AValue);
end;

procedure TsgSVGGroupDim.SetHeight(const AValue: Double);
begin
  SetF2DPointCoord(FSize, 1, AValue);
end;

procedure TsgSVGGroupDim.SetPoint1(const AValue: TFPoint);
begin
  SetF2DPointValue(FPoint1, MakeF2DPointFrom3D(AValue));
end;

procedure TsgSVGGroupDim.SetSize(const AValue: TFPoint);
begin
  SetF2DPointValue(FSize, MakeF2DPointFrom3D(AValue));
end;
                              
{ TsgSVGEntity }

function TsgSVGEntity.AddEntity(const AEntity: TsgDXFEntity): Integer;
begin
  Result := -1;
end;

procedure TsgSVGEntity.ClearBlock;
begin
  Block.Clear(True);
  Block.IsLoaded := False;
end;

function TsgSVGEntity.GetSVGType: TsvgEntity;
begin
  Result := svgEntity;
end;

{ TsgSVGRect }

procedure TsgSVGRect.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGRect then
  begin
    FSize := TsgSVGRect(Source).FSize;
    DisposeAndNil(FRX);
    DisposeAndNil(FRY);
    if TsgSVGRect(Source).FRX <> nil then
    begin
      New(FRX);
      FRX^ := TsgSVGRect(Source).FRX^;
    end;
    if TsgSVGRect(Source).FRY <> nil then
    begin
      New(FRY);
      FRY^ := TsgSVGRect(Source).FRY^;
    end;
  end;
end;

function TsgSVGRect.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_Point2D2);
  AData.AddPoint2D(Float_1010, FSize);
  if FRX <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_R1);
    AData.AddDouble(Float_1040, FRX^);
  end;
  if FRY <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_R2);
    AData.AddDouble(Float_1040, FRY^);
  end;
  Result := AData.DataCount > 0;
end;

procedure TsgSVGRect.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_Point2D2:  FSize := AData.DataPoint2D[AIndex];
    cnstED_R1:        RX := AData.DataDouble[AIndex];
    cnstED_R2:        RY := AData.DataDouble[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

destructor TsgSVGRect.Destroy;
begin
  if FRX <> nil then
    Dispose(FRX);
  if FRY <> nil then
    Dispose(FRY);
  inherited;
end;

procedure TsgSVGRect.Generate;
var
  vPoly: TsgDXFLWPolyline;
  P1, P2: TFPoint;
  I: Integer;
  vPoints: array[0..7] of TFPoint;
  vBulges: array[0..7] of Double;
  vRX, vRY: Double;
begin
  vPoly := TsgDXFLWPolyline.Create;
  vPoly.Closed := True;
  P1 := MakeFPoint(FPoint1.X, FPoint1.Y, 0);
  P2 := MakeFPoint(FPoint1.X + FSize.X, FPoint1.Y + FSize.Y, 0);
  if (FRX = nil) and (FRY = nil) then
  begin
    AddVertexInPolyline(vPoly, P1, 0);
    AddVertexInPolyline(vPoly, MakeFPoint(P1.X, P2.Y, 0), 0);
    AddVertexInPolyline(vPoly, P2, 0);
    AddVertexInPolyline(vPoly, MakeFPoint(P2.X, P1.Y, 0), 0);
  end
  else
  begin
    vRX := RX;
    vRY := RY;
    if vRX = 0 then
      vRX := vRY;
    if vRY = 0 then
      vRY := vRX;
    FillChar(vBulges, SizeOf(vBulges), 0);
    vPoints[0] := MakeFPoint(P1.X + vRX, P1.Y, 0);
    vPoints[1] := MakeFPoint(P2.X - vRX, P1.Y, 0);
    vPoints[2] := MakeFPoint(P2.X, P1.Y + vRY, 0);
    vPoints[3] := MakeFPoint(P2.X, P2.Y - vRY, 0);
    vPoints[4] := MakeFPoint(P2.X - vRX, P2.Y, 0);
    vPoints[5] := MakeFPoint(P1.X + vRX, P2.Y, 0);
    vPoints[6] := MakeFPoint(P1.X, P2.Y - vRY, 0);
    vPoints[7] := MakeFPoint(P1.X, P1.Y + vRY, 0);
    vBulges[1] := GetBulgeByPoints(vPoints[1], vPoints[2], vRX);
    vBulges[3] := GetBulgeByPoints(vPoints[3], vPoints[4], vRX);
    vBulges[5] := GetBulgeByPoints(vPoints[5], vPoints[6], vRX);
    vBulges[7] := GetBulgeByPoints(vPoints[7], vPoints[0], vRX);
    for I := 0 to 7 do
      AddVertexInPolyline(vPoly, vPoints[I], vBulges[I]);
  end;
  AddEnt(vPoly);
end;

function TsgSVGRect.GetSize: TFPoint;
begin
  Result.X := FSize.X;
  Result.Y := FSize.Y;
  Result.Z := 0;
end;

procedure TsgSVGRect.SetSize(const AValue: TFPoint);
begin
  FSize.X := AValue.X;
  FSize.Y := AValue.Y;
end;

function TsgSVGRect.GetHeight: Double;
begin
  Result := FSize.Y;
end;

function TsgSVGRect.GetRX: Double;
begin
  if FRX = nil then
    Result := 0
  else
    Result := FRX^;
end;

function TsgSVGRect.GetRY: Double;
begin
  if FRY = nil then
    Result := 0
  else
    Result := FRY^;
end;

function TsgSVGRect.GetSVGType: TsvgEntity;
begin
  Result := svgRect;
end;

function TsgSVGRect.GetWidth: Double;
begin
  Result := FSize.X;
end;

procedure TsgSVGRect.SetHeight(const AValue: Double);
begin
  FSize.Y := AValue;
end;

procedure TsgSVGRect.SetRX(const AValue: Double);
begin
  SetDoubleValue(FRX, AValue);
end;

procedure TsgSVGRect.SetRY(const AValue: Double);
begin
  SetDoubleValue(FRY, AValue);
end;

procedure TsgSVGRect.SetWidth(const AValue: Double);
begin
  FSize.X := AValue;
end;

{  TsgSVGImageEnt  }

procedure TsgSVGImageEnt.ConvertToBitmap;
var
  vNewraphic: TGraphic;
  vImageEnt: TsgDXFImageEnt;
begin
  vImageEnt := GetImageEnt;
  try
    if vImageEnt.Picture.Graphic <> nil then
    begin
{$IFNDEF SG_FIREMONKEY}
      vNewraphic := sgFunction.CreateCopyGraphicAsBitmap(vImageEnt.Picture.Graphic, false);
{$ELSE}
      vNewraphic := TGraphic(sgFunction.CreateCopyGraphic(vImageEnt.Picture.Graphic));
{$ENDIF}
      try
        if vNewraphic <> nil then
          vImageEnt.SetImage(vNewraphic);
      finally
        vNewraphic.Free;
      end;
    end;
  except
    vImageEnt.SetImage(nil);
  end;
end;

constructor TsgSVGImageEnt.Create;
begin
  inherited Create;
  Properties.StrokeColor := clBlack;
  FPicture := TsgSVGPictureEnt.Create;
  FPictureBlock := nil;
end;

destructor TsgSVGImageEnt.Destroy;
begin
  FPictureBlock := nil;
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TsgSVGImageEnt.AddFill(const AEntity: TsgDXFEntity);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgSVGImageEnt.AddFills(AEnts: TsgObjectList);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgSVGImageEnt.Generate;
var
  vInsert: TsgDXFInsert;
  vGraphic: TObject;
  vBitmapSize: TPoint;
begin
  if Assigned(FPicture) then
  begin
    vGraphic := nil;
    Converter.Loads(FPicture);
    if Assigned(FPicture.Picture) then
      vGraphic := FPicture.Picture.Graphic;
  end
  else
    vGraphic := GetImageEnt.Picture.Graphic;
  if Assigned(vGraphic) then
  begin
    if not Assigned(FPictureBlock) then
    begin
      FPictureBlock := TsgDXFBlock.Create;
      FPictureBlock.Flags := 1;
      FPictureBlock.Name := GetUniqBlockName(Converter,
        cnstSVGBImagelockNameBase, UInt64(FPictureBlock));
      FPictureBlock.AddEntity(FPicture);
      {$IFDEF SG_INCLUDE_PNG}
      if vGraphic is TPngImage then
      begin
{$IFDEF SGFPC}
        FPicture.Transparency := TPngImage(vGraphic).PixelFormat = pf8bit;
{$ELSE}
        FPicture.Transparency := TPngImage(vGraphic).PixelInformation.Header.BitDepth = 8;
{$ENDIF}
        FPicture.TransparentColor := TPngImage(vGraphic).TransparentColor;
      end
      else
      {$ENDIF}
        if vGraphic is TBitmap then
        begin
          {$IFDEF SG_FIREMONKEY}
          FPicture.Transparency := True;
          {$ELSE}
          FPicture.Transparency := TBitmap(vGraphic).Transparent;
          FPicture.TransparentColor := TBitmap(vGraphic).TransparentColor;
          {$ENDIF}
        end;
      FPicture := nil;
      Converter.Loads(FPictureBlock);
      Converter.Sections[csBlocks].AddEntity(FPictureBlock);
    end;
    vBitmapSize := cnstPointZero;
    if vGraphic is TGraphic then
    begin
      vBitmapSize.X := TGraphic(vGraphic).Width;
      vBitmapSize.Y := TGraphic(vGraphic).Height;
    end;
    {$IFDEF SG_FIREMONKEY}
    if vGraphic is TBitmap then
      vBitmapSize := TPoint.Create(TBitmap(vGraphic).Width, TBitmap(vGraphic).Height);
    {$ENDIF}
    vInsert := TsgDXFInsert.Create;
    vInsert.Point := MakeFPoint(Point1.X, Point1.Y + Height);
    if (vBitmapSize.X <> 0) and (vBitmapSize.Y <> 0) then
      vInsert.Scale := MakeFPoint(Width / vBitmapSize.X, -Height / vBitmapSize.Y, 1)
    else
      vInsert.Scale := cnstFPointSingleYMinus;
    vInsert.Block := FPictureBlock;
    Converter.Loads(vInsert);
    Block.AddEntity(vInsert);
  end
  else
    inherited Generate;
end;

function TsgSVGImageEnt.GetSVGType: TsvgEntity;
begin
  Result := svgImage;
end;

procedure TsgSVGImageEnt.AssignEntity(Source: TsgDXFEntity);
var
  vSVGImageEnt: TsgSVGImageEnt absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGImageEnt then
  begin
    if Assigned(vSVGImageEnt.FPicture) then
      SetImage(TGraphic(vSVGImageEnt.GetImageEnt.Picture.Graphic))
    else
    begin
      FreeAndNil(FPicture);
      FPictureBlock := vSVGImageEnt.FPictureBlock;
    end;
  end;
end;

function TsgSVGImageEnt.GetImageEnt: TsgDXFImageEnt;
begin
  Result := FPicture;
  if not Assigned(Result) then
    Result := TsgDXFImageEnt(FPictureBlock.Entities[0]);
end;

procedure TsgSVGImageEnt.SetImage(const AImg: TObject);
begin
  if not Assigned(FPicture) then
  begin
    FPictureBlock := nil;
    FPicture := TsgSVGPictureEnt.Create;
  end;
  FPicture.SetImage(TGraphic(AImg));
  if Assigned(AImg) then
  begin
    if AImg is TGraphic then
      FPicture.Size := MakeFPoint(TGraphic(AImg).Width, TGraphic(AImg).Height);
    {$IFDEF SG_FIREMONKEY}
    if AImg is TBitmap then
      FPicture.Size := MakeFPoint(TBitmap(AImg).Width, TBitmap(AImg).Height);
    {$ENDIF}
  end;
end;


{ TsgSVGPoint }

procedure TsgSVGPoint.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGPoint then
    FPoint1 := TsgSVGPoint(Source).FPoint1;
end;

function TsgSVGPoint.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_Point2D1);
  AData.AddPoint2D(Float_1010, FPoint1);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGPoint.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_Point2D1: FPoint1 := AData.DataPoint2D[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);    
  end;
end;


function TsgSVGPoint.GetPropertiesType: Byte;
begin
  Result := 1;
end;

procedure TsgSVGPoint.Generate;
var
  vPoint: TsgDXFPoint;
begin
  inherited Generate;
  vPoint := TsgDXFPoint.Create;
  vPoint.Point := Point1;
  AddEnt(vPoint);
end;

function TsgSVGPoint.GetPoint1: TFPoint;
begin
  Result.X := FPoint1.X;
  Result.Y := FPoint1.Y;
  Result.Z := 0;
end;

function TsgSVGPoint.GetSVGType: TsvgEntity;
begin
  Result := svgPoint;
end;

function TsgSVGPoint.GetX1: Double;
begin
  Result := FPoint1.X;
end;

function TsgSVGPoint.GetY1: Double;
begin
  Result := FPoint1.Y;
end;

procedure TsgSVGPoint.SetPoint1(const AValue: TFPoint);
begin
  FPoint1.X := AValue.X;
  FPoint1.Y := AValue.Y;
end;

procedure TsgSVGPoint.SetX1(const AValue: Double);
begin
  FPoint1.X := AValue;
end;

procedure TsgSVGPoint.SetY1(const AValue: Double);
begin
  FPoint1.Y := AValue;
end;

{ TsgSVGLine }

procedure TsgSVGLine.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGLine then
    FPoint2 := TsgSVGLine(Source).FPoint2;
end;

function TsgSVGLine.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_Point2D2);
  AData.AddPoint2D(Float_1010, FPoint2);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGLine.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_Point2D2: FPoint2 := AData.DataPoint2D[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);    
  end;
end;

procedure TsgSVGLine.Generate;
var
  vLine: TsgDXFLWPolyline;
  vFillColor: TsgFillColor;
begin
  vLine := TsgDXFLWPolyline.Create;
  SetEntProps(vLine);
  AddVertexInPolyline(vLine, MakeFPoint(FPoint1.X, FPoint1.Y, 0), 0);
  AddVertexInPolyline(vLine, MakeFPoint(FPoint2.X, FPoint2.Y, 0), 0);
  if StrokeColor = clNone then
  begin
    vFillColor := FillColor;
    if (not vFillColor.IsStyle) and (vFillColor.Color <> clNone) then
    begin
      vLine.ColorCAD := svgConvertARGBToColorCAD(vFillColor.Color);
      vLine.LineWeight := 1;
      Converter.Loads(vLine);
      Block.AddEntity(vLine);
    end
    else
      vLine.Free;
  end
  else
  begin
    Converter.Loads(vLine);
    Block.AddEntity(vLine);
  end;
end;

function TsgSVGLine.GetPoint2: TFPoint;
begin
  Result := MakeFPointFrom2D(FPoint2);
end;

function TsgSVGLine.GetSVGType: TsvgEntity;
begin
  Result := svgLine;
end;

function TsgSVGLine.GetX2: Double;
begin
  Result := FPoint2.X;
end;

function TsgSVGLine.GetY2: Double;
begin
  Result := FPoint2.Y;
end;

procedure TsgSVGLine.SetPoint2(const AValue: TFPoint);
begin
  FPoint2 := MakeF2DPointFrom3D(AValue);
end;

procedure TsgSVGLine.SetX2(const AValue: Double);
begin
  FPoint2.X := AValue;
end;

procedure TsgSVGLine.SetY2(const AValue: Double);
begin
  FPoint2.Y := AValue;
end;

 { TsgSVGLink }

procedure TsgSVGLink.AssignEntity(Source: TsgDXFEntity);
var
  vLink: TsgSVGLink absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGLink then
  begin
    if Block <> nil then
      Block.Clear(True);
    FHref := vLink.FHref;
    FEref := nil;
    if Converter <> nil then
      FEref := vLink.FEref;
  end;
end;

function TsgSVGLink.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_HREF);
  AData.AddString(String_1000, Href);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGLink.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_HREF:  Href := AData.DataString[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

function TsgSVGLink.GetHref: string;
begin
  Result := FHref;
end;

function TsgSVGLink.GetSVGType: TsvgEntity;
begin
  Result := svgLink;
end;

function TsgSVGLink.GetUsingClass: TClass;
begin
  Result := TsgSVGContainer;
end;

procedure TsgSVGLink.FindEref(const AConverter: TsgDXFConverter);
var
  I: Integer;
  vBlock: TsgDXFBlock;
  vEnt: TsgSVGContainer;
  vClass: TClass;
begin
  vClass := GetUsingClass;
  vBlock := AConverter.BlockByName(cnstDefs);
  if vBlock <> nil then
  begin
    vEnt := TsgSVGContainer(GetEntityInBlock(FHref, vBlock));
    if vEnt is vClass then
      FEref := vEnt;
  end;
  if FEref = nil then
  begin
    for I := 0 to AConverter.LayoutsCount - 1 do
    begin
      vEnt := TsgSVGContainer(GetEntityInBlock(FHref, AConverter.Layouts[I].PaperSpaceBlock));
      if vEnt is vClass then
      begin
        FEref := TsgSVGPath(vEnt);
        Break;
      end;
    end;
  end;
end;

procedure TsgSVGLink.Generate;
begin
  if FEref = nil then
    FindEref(Converter);
end;

procedure TsgSVGLink.SetHref(const AValue: string);
begin
  FHref := AValue;
  FEref := nil;
end;

{ TsgSVGCircle }

procedure TsgSVGCircle.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGCircle then
    FRadius := TsgSVGCircle(Source).FRadius;
end;

function TsgSVGCircle.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_R1);
  AData.AddDouble(Float_1040, FRadius);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGCircle.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_R1:  FRadius := AData.DataDouble[Aindex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);    
  end;
end;

procedure TsgSVGCircle.Generate;
var
  vCircle: TsgDXFCircle;
begin
  vCircle := TsgDXFCircle.Create;
  vCircle.Point := Point1;
  vCircle.Radius := Radius;
  AddEnt(vCircle);  
end;

function TsgSVGCircle.GetRadius: Double;
begin
  Result := FRadius;
end;

function TsgSVGCircle.GetSVGType: TsvgEntity;
begin
  Result := svgCircle;
end;

procedure TsgSVGCircle.SetRadius(const AValue: Double);
begin
  FRadius := AValue;
end;

{ TsgSVGEllipse }

procedure TsgSVGEllipse.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGEllipse then
    FRY := TsgSVGEllipse(Source).FRY;
end;

function TsgSVGEllipse.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_R2);
  AData.AddDouble(Float_1040, RY);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGEllipse.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_R2:  RY := AData.DataDouble[Aindex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

procedure TsgSVGEllipse.Generate;
var
  vEllipse: TsgDXFEllipse;
  vRatX, vRatY: TFPoint;
begin
  vRatX := MakeFpoint(Point1.X + RX, Point1.Y, Point1.Z);
  vRatY := MakeFpoint(Point1.X, Point1.Y + RY, Point1.Z);
  vEllipse := CreateCADEllipticArc(Point1, vRatX, vRatY, nil, nil);
  if vEllipse <> nil then
    AddEnt(vEllipse);
end;

function TsgSVGEllipse.GetRadius: Double;
begin
  Result := 0;
end;

function TsgSVGEllipse.GetRX: Double;
begin
  Result := FRadius;
end;

function TsgSVGEllipse.GetRY: Double;
begin
  Result := FRY;
end;

function TsgSVGEllipse.GetSVGType: TsvgEntity;
begin
  Result := svgEllipse;
end;

procedure TsgSVGEllipse.SetRadius(const AValue: Double);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgSVGEllipse.SetRX(const AValue: Double);
begin
  FRadius := AValue;
end;

procedure TsgSVGEllipse.SetRY(const AValue: Double);
begin
  FRY := AValue;
end;

{  TsgPathLoader }

procedure TsgPathLoader.AddFPointsInBuffer(const APoints: array of TFPoint);
var
  I: Integer;
begin
  FBuffer.Capacity := FBuffer.Capacity + High(APoints) - Low(APoints) + 1;
  for I := Low(APoints) to High(APoints) do
    FBuffer.Add(APoints[I]);
end;

procedure TsgPathLoader.SetFPoints(var APoints: array of TFPoint;
  const ARelative: Boolean);
var
  I: Integer;
begin
  for I := Low(APoints) to High(APoints) do
    APoints[I] := ApplyRelative(GetDataAsFPoint, ARelative);
end;

function TsgPathLoader.AddPointsArc(const ARelative: Boolean): Boolean;
var
  vPoints: array[0..0] of TFPoint;
  vStartAngle, vEndAngle, vAxisAngle, vRatio: Double;
  vArcFlags: Byte;
  vLastPoint, vCenterPoint, vRadius, vPointFirst, vPointLast, vRadPt: TFPoint;
  vIsBeginToEnd: Boolean;
  vVertexes: TsgEntitiesList;
begin
  Result := False;
  vArcFlags := Floor(GetDataAsDouble);
  vAxisAngle := GetDataAsDouble;
  SetFPoints(vPoints, ARelative);
  vRadius := GetDataAsFPoint;
  vLastPoint := GetLastPoint;
  if SetRatioAndMajorPoint(vRadius.X, vRadius.Y, vRatio, vRadPt) then
  begin
    if CalcArcCenter(vLastPoint.X, vLastPoint.Y, vPoints[0].X, vPoints[0].Y,
      vRadius.X, vRadius.Y, vAxisAngle, vArcFlags, vCenterPoint) then
    begin
      vRadPt := sgFunction.RotateFPoint(vRadPt, vAxisAngle);
      vStartAngle := sgMod(GetAngleParam(vCenterPoint, vRadPt, vRatio, vLastPoint), 360);
      vEndAngle := sgMod(GetAngleParam(vCenterPoint, vRadPt, vRatio, vPoints[0]), 360);
      if vArcFlags and 2 = 0 then
        SwapSGFloats(vStartAngle, vEndAngle);
      if vEndAngle <= vStartAngle then
         vEndAngle := vEndAngle + 360;
      vVertexes := TsgEntitiesList.Create;
      try
        FGenerator.SetListAndProc(vVertexes, @AddVertexList);
        FGenerator.CreateEllipticArc(vCenterPoint, vRadPt, vRatio,
          vStartAngle, vEndAngle);
        Result := vVertexes.Count > 0;
        if Result then
        begin
          vPointFirst := TsgDXFVertex(vVertexes.First).Point;
          vPointLast:= TsgDXFVertex(vVertexes.Last).Point;
          vIsBeginToEnd := DistanceFPointSqr(vLastPoint, vPointFirst) <
            DistanceFPointSqr(vLastPoint, vPointLast);
          AddListItems(GetPolylineVertexes(FPolyline), vVertexes,
            vIsBeginToEnd);
        end;
      finally
        vVertexes.Free;
      end;
    end;  
  end;
  if not Result then
  begin
    AddVertexInPolyline(FPolyline, vLastPoint);
    AddVertexInPolyline(FPolyline, vPoints[0]);
  end;
  AddFPointsInBuffer(vPoints);
end;

procedure TsgPathLoader.AddPointsCoord(const ARelative: Boolean;
  const AType: Byte);
var
  vPoint: TFPoint;
  vCoord: Double;
begin
  vCoord := GetDataAsDouble;
  vPoint := GetLastPoint;
  AddVertexInPolyline(FPolyline, vPoint);
  if ARelative then
    vPoint.V[AType] := vPoint.V[AType] + vCoord
  else
    vPoint.V[AType] :=  vCoord;
  AddVertexInPolyline(FPolyline, vPoint);
  AddFPointsInBuffer([vPoint]);
end;

procedure TsgPathLoader.AddPointsCurveCubic(const ARelative: Boolean);
var
  vPoints: array [0 .. 2] of TFPoint;
begin
  SetFPoints(vPoints, ARelative);
  FGenerator.SetListAndProc(GetPolylineVertexes(FPolyline), @AddVertexList);
  FGenerator.CreateCubicSpline(GetLastPoint, vPoints[0], vPoints[1], vPoints[2]);
  AddFPointsInBuffer(vPoints);
end;

procedure TsgPathLoader.AddPointsCurveCubicSmooth(const ARelative: Boolean);
var
  vPoint1, vPoint2: TFPoint;
  vPoints: array [0 .. 1] of TFPoint;
begin
  SetFPoints(vPoints, ARelative);
  SetFirstPointsAndPoly(vPoints[0], vPoint1, vPoint2);
  FGenerator.SetListAndProc(GetPolylineVertexes(FPolyline), @AddVertexList);
  FGenerator.CreateCubicSpline(vPoint1, vPoint2, vPoints[0], vPoints[1]);
  AddFPointsInBuffer(vPoints);
end;

procedure TsgPathLoader.AddPointsCurveQuadratic(const ARelative: Boolean);
var
  vPoints: array [0 .. 1] of TFPoint;
begin
  SetFPoints(vPoints, ARelative);
  FGenerator.SetListAndProc(GetPolylineVertexes(FPolyline), @AddVertexList);
  FGenerator.CreateQuadraticSpline(GetLastPoint, vPoints[0], vPoints[1]);
  AddFPointsInBuffer(vPoints);
end;

procedure TsgPathLoader.AddPointsCurveQuadraticSmooth(const ARelative: Boolean);
var
  vPoint1, vPoint2: TFPoint;
  vPoints: array [0 .. 0] of TFPoint;
begin
  SetFPoints(vPoints, ARelative);
  if SetFirstPointsAndPoly(vPoints[0], vPoint1, vPoint2) then
  begin
    FGenerator.SetListAndProc(GetPolylineVertexes(FPolyline), @AddVertexList);
    FGenerator.CreateQuadraticSpline(vPoint1, vPoint2, vPoints[0]);
    AddFPointsInBuffer([vPoint2, vPoints[0]]);
  end
  else
  begin
    if FBuffer.Count > 0 then
      AddVertexInPolyline(FPolyline, GetLastPoint);
    AddVertexInPolyline(FPolyline, vPoints[0]);
    AddFPointsInBuffer(vPoints);
  end;
end;

procedure TsgPathLoader.AddPointsLine(const ARelative: Boolean);
var
  vPoints: array [0 .. 0] of TFPoint;
begin
  AddVertexInPolyline(FPolyline, GetLastPoint);
  SetFPoints(vPoints, ARelative);
  AddVertexInPolyline(FPolyline, vPoints[0]);
  AddFPointsInBuffer(vPoints);
end;

function TsgPathLoader.AddPointsMove(const ARelative: Boolean): Boolean;
var
  vPoint: TFPoint;
begin
  Result := AddPoly(False);
  vPoint := ApplyRelative(GetDataAsFPoint, ARelative);
  FBuffer.Add(vPoint);
end;

function TsgPathLoader.AddPoly(const AClose: Boolean): Boolean;
begin
  Result := False;
  if FPolyline <> nil then
  begin
    if FPolyline.Count > 0 then
    begin
      FPolyline.Closed := AClose;
      FEntities.Add(FPolyline);
      Result := True;
    end
    else
      FPolyline.Free;
    FPolyline := nil;
  end;
end;

function TsgPathLoader.GetDataAsByte: Byte;
begin
  Result := PByte(@FData.List[FIndex])^;
  Inc(FIndex);
end;

function TsgPathLoader.GetDataAsDouble: Double;
begin
  Result := PSingle(@FData.List[FIndex])^;
  Inc(FIndex);
end;

function TsgPathLoader.GetDataAsFPoint: TFPoint;
begin
  Result.X := PSingle(@FData.List[FIndex])^;
  Result.Y := PSingle(@FData.List[FIndex + 1])^;
  Result.Z := 0;
  Inc(FIndex, 2);
end;

function TsgPathLoader.GetDataAsSegmentType: TsgPathSegType;
var
  vPathType: Integer;
begin
  Result := psUNKNOWN;
  vPathType := PInteger(@FData.List[FIndex])^;
  Inc(FIndex);
  if not IsBadPathType(vPathType) then
    Result := TsgPathSegType(vPathType);
end;

function TsgPathLoader.ApplyRelative(const APoint: TFPoint;
  const ARelative: Boolean): TFPoint;
begin
  if ARelative then
    Result := AddFPoint2D(APoint, GetLastPoint)
  else
    Result := APoint;
end;

function TsgPathLoader.GetLastPoint: TFPoint;
begin
  if FBuffer.Count > 0  then
    Result := FBuffer.Last
  else
    Result := FPrevPoint;
end;

function TsgPathLoader.GetNumberPartsByCircle: Integer;
begin
  SetSpliteNumber(Result, FConverter.NumberOfPartsInCircle,
    GetNumberOfCircleParts);
end;

function TsgPathLoader.GetNumberPartsBySpline: Integer;
begin
   SetSpliteNumber(Result, FConverter.NumberOfPartsInSpline,
     GetNumberOfSplineParts);
end;

function TsgPathLoader.SetFirstPointsAndPoly(const APoint: TFPoint;
  var APoint1, APoint2: TFPoint): Boolean;
begin
  Result := False;
  if FBuffer.Count > 0 then
  begin
    APoint1 := GetLastPoint;
    if FBuffer.Count > 1 then
    begin
      APoint2 := MirrorFPoint(FBuffer[FBuffer.Count - 2], APoint1);
      Result := True;
    end
    else
      APoint2 := APoint;
  end
  else
  begin
    APoint1 := APoint;
    APoint2 := APoint;
  end;
end;

constructor TsgPathLoader.Create;
begin
  inherited Create;
  FGenerator := TsgGeneratorShapeEdge.Create;
end;

destructor TsgPathLoader.Destroy;
begin
  FGenerator.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TsgPathLoader.Clear;
begin
  FIndex := 0;
  FData := nil;
  FPolyline := nil;
  FEntities := nil;
  FConverter := nil;
  FGenerator.SetListAndProc(nil, nil);
  if Assigned(FBuffer) then
    FBuffer.Clear(False);
  FPrevPoint := cnstFPointZero;
end;

function TsgPathLoader.LoadEntities(const AConverter: TsgDXFConverter;
  const AData: TList; const AEntities: TsgObjectList): Boolean;
var
  vRel: Boolean;
  vPathSegType: TsgPathSegType;
  vPathSegTypes: TsgPathSegTypes;
begin
  Result := False;
  Clear;
  FData := AData;
  FEntities := AEntities;
  FConverter := AConverter;
  if (FConverter <> nil) and (FData <> nil) and (AEntities <> nil) then
  begin
    FPrevPoint := cnstFPointZero;
    FGenerator.NumberCirclePart := GetNumberPartsByCircle;
    FGenerator.NumberSplinePart := GetNumberPartsBySpline;
    FGenerator.QualityApproximation := cnstQualityApproximation;
    if FBuffer = nil then
      FBuffer := TFPointList.Create;
    FPolyline := nil;
    try
      FIndex := 0;
      while FIndex < FData.Count do
      begin
        vPathSegType := GetDataAsSegmentType;
        vRel := Integer(vPathSegType) mod 2 <> 0;
        if not (vPathSegType in [psMOVETO_ABS, psMOVETO_REL, psCLOSEPATH]) then
        begin
          if FPolyline = nil then
            FPolyline := TsgDXFLWPolyline.Create;
          Include(vPathSegTypes, vPathSegType);
        end;
        case vPathSegType of
          psMOVETO_ABS, psMOVETO_REL:
            begin
              if AddPointsMove(vRel) then
                vPathSegTypes := [];
            end;
          psLINETO_ABS, psLINETO_REL:
            AddPointsLine(vRel);
          psCURVETO_CUBIC_ABS, psCURVETO_CUBIC_REL:
            AddPointsCurveCubic(vRel);
          psCURVETO_QUADRATIC_ABS, psCURVETO_QUADRATIC_REL:
            AddPointsCurveQuadratic(vRel);
          psARC_ABS, psARC_REL:
            AddPointsArc(vRel);
          psLINETO_HORIZONTAL_ABS, psLINETO_HORIZONTAL_REL:
            AddPointsCoord(vRel, 0);
          psLINETO_VERTICAL_ABS, psLINETO_VERTICAL_REL:
            AddPointsCoord(vRel, 1);
          psCURVETO_CUBIC_SMOOTH_ABS, psCURVETO_CUBIC_SMOOTH_REL:
            AddPointsCurveCubicSmooth(vRel);
          psCURVETO_QUADRATIC_SMOOTH_ABS, psCURVETO_QUADRATIC_SMOOTH_REL:
            AddPointsCurveQuadraticSmooth(vRel);
          psCLOSEPATH:
            begin
              if (vPathSegTypes <> []) then
              begin
                if AddPoly(True) then
                begin
                  if FBuffer.Count > 0 then
                    FPrevPoint := FBuffer.First;
                end
                else
                  FPrevPoint := GetLastPoint;
                FBuffer.Clear(False);
                vPathSegTypes := [];
              end;
            end;
        end;
      end;
    finally
      FPrevPoint := GetLastPoint;
      FBuffer.Clear(False);
      if FPolyline <> nil then
      begin
        if FPolyline.Count > 0 then
          AddPoly(False)
        else
          FPolyline.Free;
      end;
    end;
    Result := True;
  end;
end;

{ TsgSVGPoly }

procedure TsgSVGPoly.AddPoint(const APoint: TFPoint);
begin
  AddPoint2D(MakeF2DPointFrom3D(APoint));
end;

procedure TsgSVGPoly.AddPoint2D(const APoint: TF2DPoint);
begin
  FPoints.Add(APoint);
end;

procedure TsgSVGPoly.AddPointCoords(const AX, AY: Double);
begin
  AddPoint2D(MakeF2DPoint(AX, AY));
end;

procedure TsgSVGPoly.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGPoly then
  begin
    FSVGFlags := TsgSVGPoly(Source).FSVGFlags;
    FPoints.Assign(TsgSVGPoly(Source).FPoints);
  end;
end;

function TsgSVGPoly.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_PolyFlags);
  AData.AddByte(Integer_1070, FSVGFlags);
  AData.AddArrayPoints2D(cnstED_PointsCount1, cnstED_Point2D1, FPoints);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGPoly.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
var
  vCapasity: Integer;
begin
  case ACode of
    cnstED_PolyFlags:      FSVGFlags := AData.DataByte[AIndex];
    cnstED_PointsCount1:
      begin
        vCapasity := AData.DataInt[AIndex];
        if FPoints.Capacity < vCapasity then
          FPoints.Capacity := vCapasity;
      end;
    cnstED_Point2D1:  AddPoint(AData.DataPoint[AIndex]);
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);    
  end;
end;

procedure TsgSVGPoly.ClearPoints;
begin
  FPoints.Count := 0;
end;

constructor TsgSVGPoly.Create;
begin
  inherited Create;
  FPoints := TF2DPointList.Create;
end;

destructor TsgSVGPoly.Destroy;
begin
  ClearPoints;
  FPoints.Free;
  inherited Destroy
end;

procedure TsgSVGPoly.Generate;
var
  I: Integer;
  vPoly: TsgDXFLWPolyline;
begin
  vPoly := TsgDXFLWPolyline.Create;
  vPoly.Closed := Closed;
  for I := 0 to FPoints.Count - 1 do
    AddVertexInPolyline(vPoly, MakeFPointFrom2D(FPoints[I]));
  AddEnt(vPoly);
end;

function TsgSVGPoly.GetClosed: Boolean;
begin
  Result := FSVGFlags and 1 <> 0;
end;

function TsgSVGPoly.GetSVGType: TsvgEntity;
begin
  Result := svgPolyline;
end;

procedure TsgSVGPoly.SetClosed(const AValue: Boolean);
begin
  FSVGFlags := (FSVGFlags and 254) or Byte(AValue);
end;

{ TsgSVGPath }

procedure TsgSVGPath.ExportByte(const AData: TsgCADExtendedData;
  const ACode: SmallInt; var AIndex: Integer);
begin
  AData.AddByte(ACode, PByte(@FData.List[AIndex])^);
  Inc(AIndex);
end;

procedure TsgSVGPath.ExportDouble(const AData: TsgCADExtendedData;
  const ACode: SmallInt; var AIndex: Integer);
begin
  AData.AddDouble(ACode, PSingle(@FData.List[AIndex])^);
  Inc(AIndex);
end;

procedure TsgSVGPath.ExportPoint2D(const AData: TsgCADExtendedData;
  const ACode: SmallInt; var AIndex: Integer);
var
  vPoint: TF2DPoint;
begin
  vPoint.X := PSingle(@FData.List[AIndex])^;
  vPoint.Y := PSingle(@FData.List[AIndex + 1])^;
  AData.AddPoint2D(ACode, vPoint);
  Inc(AIndex, 2);
end;

procedure TsgSVGPath.AddPathData(const AType: Byte;
  const AValues: array of Single);
var
  vIndex, vValuesCount: Integer;
{$IFDEF SG_CPUX64}
  I, J: Integer;
{$ENDIF}
begin
  if IsBadPathType(AType) then Exit;
  vIndex := FData.Count;
  vValuesCount := cnstpathValuesCount[TsgPathSegType(AType)];
  FData.Count := FData.Count + vValuesCount + 1;
  FData.List[vIndex] := Pointer(AType);
{$IFDEF SG_CPUX64}
  for I := 0 to vValuesCount - 1 do
  begin
    J := vIndex + 1 + I;
    FData.List[J] := nil;
    PSingle(@FData.List[J])^ := AValues[I];
  end;
{$ELSE}
  CopyMemory(@FData.List[vIndex + 1], @AValues, vValuesCount * SizeOf(Pointer));
{$ENDIF}
end;

function TsgSVGPath.DataCopyToValues(const AValuesCount: Integer;
  var AIndex: Integer; var AValues: array of Single): Boolean;
var
  I: Integer;
begin
  Result := AIndex + AValuesCount <= FDataList.Count;
  if Result then
  begin
    for I := 0 to AValuesCount - 1 do
      AValues[I] := FDataList.List[I + AIndex];
    Inc(AIndex, AValuesCount);
  end;
end;

procedure TsgSVGPath.AddPathSegment(const APathSegTypeByte: Byte);
var
  APathSegType: TsgPathSegType;
  vValues, vValuesCopy: array [0..6] of Single;
  I, vValueCount: Integer;
  vIsArcSegment: Boolean;
  vPathSegType: Byte;
begin
  APathSegType := TsgPathSegType(APathSegTypeByte);
  if APathSegType <> psUNKNOWN then
  begin
    if APathSegType <> psCLOSEPATH then
    begin
      vPathSegType := Byte(APathSegType);
      vIsArcSegment := APathSegType in [psARC_ABS, psARC_REL];
      vValueCount := cnstPathValuesCount[APathSegType] + Integer(vIsArcSegment);
      I := 0;
      while DataCopyToValues(vValueCount, I, vValues) do
      begin
        if vIsArcSegment then
        begin
          CopyMemory(@vValuesCopy, @vValues, SizeOf(vValues));
          vValues[0] := GetArcFlags(vValues[3] <> 0, vValues[4] <> 0);
          vValues[1] := vValuesCopy[2];
          vValues[2] := vValuesCopy[5];
          vValues[3] := vValuesCopy[6];
          vValues[4] := vValuesCopy[0];
          vValues[5] := vValuesCopy[1];
        end;
        AddPathData(vPathSegType, vValues);
        case APathSegType of
          psMOVETO_ABS:  vPathSegType := Byte(psLINETO_ABS);
          psMOVETO_REL:  vPathSegType := Byte(psLINETO_REL);
        end;
      end;
    end
    else
      AddCloseFigure;
    FDataList.Count := 0;
  end;
end;

procedure TsgSVGPath.SetData(const APathsSeg: string);
var
  vPathSegType, vKeyPathSegType: TsgPathSegType;
  I, J, vLenPathsSeg: Integer;
  vValue: Single;
begin
  vLenPathsSeg := Length(APathsSeg);
  if vLenPathsSeg > 0 then
  begin
    FDataList := TsgSingleList.Create;
    try
      I := 1;
      vKeyPathSegType := psUNKNOWN;
      while (I <= vLenPathsSeg) do
      begin
        while (I <= vLenPathsSeg) and ((APathsSeg[I] = ',') or IsSpace(APathsSeg[I])) do
          Inc(I);
        vPathSegType := TsgPathSegType(GetPathSegType(APathsSeg[I]));
        if vPathSegType <> psUNKNOWN then
        begin
          Inc(I);
          AddPathSegment(Byte(vKeyPathSegType));
          vKeyPathSegType := vPathSegType;
        end
        else
        begin
          J := I;
          ScanDigitsInString(APathsSeg, vLenPathsSeg, False, I);
          if I > J then
          begin
            vValue := StrToVal(Copy(APathsSeg, J, I - J));
            FDataList.Add(vValue);
          end
          else
            Inc(I);
        end;
      end;
      AddPathSegment(Byte(vKeyPathSegType));
    finally
      FreeAndNil(FDataList);
    end;
  end;
end;

function TsgSVGPath.GetPathSegType(const AKey: Char): Byte;
var
  vResult: TsgPathSegType absolute Result;
begin
  case AKey of
    'A':  vResult := psARC_ABS;
    'a':  vResult := psARC_REL;
    'C':  vResult := psCURVETO_CUBIC_ABS;
    'c':  vResult := psCURVETO_CUBIC_REL;
    'H':  vResult := psLINETO_HORIZONTAL_ABS;
    'h':  vResult := psLINETO_HORIZONTAL_REL;
    'L':  vResult := psLINETO_ABS;
    'l':  vResult := psLINETO_REL;
    'M':  vResult := psMOVETO_ABS;
    'm':  vResult := psMOVETO_REL;
    'S':  vResult := psCURVETO_CUBIC_SMOOTH_ABS;
    's':  vResult := psCURVETO_CUBIC_SMOOTH_REL;
    'T':  vResult := psCURVETO_QUADRATIC_SMOOTH_ABS;
    't':  vResult := psCURVETO_QUADRATIC_SMOOTH_REL;
    'Q':  vResult := psCURVETO_QUADRATIC_ABS;
    'q':  vResult := psCURVETO_QUADRATIC_REL;
    'V':  vResult := psLINETO_VERTICAL_ABS;
    'v':  vResult := psLINETO_VERTICAL_REL;
    'Z', 'z':  vResult := psCLOSEPATH;
  else
    vResult := psUNKNOWN;
  end;
end;

procedure TsgSVGPath.GetPolyPoints(const AList: TsgObjectList; AConverter: TsgDXFConverter);
var
  I, J: Integer;
  vPoints: TFPointList;
  vEnt: TsgDXFEntity;
  vEntities: TsgObjectList;
begin
  if not Assigned(AConverter) then
    AConverter := Converter;
  SetConverter(AConverter);
  vEntities := TsgObjectList.Create;
  try
    ParseData(vEntities);
    for I := 0 to vEntities.Count - 1 do
    begin
      vEnt := TsgDXFEntity(vEntities[I]);
      if vEnt is TsgCADBasePolyline then
      begin
        if Assigned(AConverter) then
        begin
          AConverter.Loads(vEnt);
          if TsgCADBasePolyline(vEnt).PolyPoints.Count > 0 then
          begin
            vPoints := TFPointList.Create;
            AList.Add(vPoints);
            vPoints.Assign(TsgCADBasePolyline(vEnt).PolyPoints);
            if TsgCADBasePolyline(vEnt).Closed and (not IsEqualFPoints(vPoints.First, vPoints.Last)) then
              vPoints.Add(vPoints.First);
          end;
        end
        else
        begin
          if (vEnt.EntType in [cePolyline, ceLWPolyline]) and (TsgDXFPolyline(vEnt).Count > 0) then
          begin
            vPoints := TFPointList.Create;
            AList.Add(vPoints);
            vPoints.Capacity := TsgDXFPolyline(vEnt).Count;
            for J := 0 to TsgDXFPolyline(vEnt).Count - 1 do
              vPoints.Add(TsgDXFPolyline(vEnt).Vertexes[J].Point);
            if TsgCADBasePolyline(vEnt).Closed and (not IsEqualFPoints(vPoints.First, vPoints.Last)) then
              vPoints.Add(vPoints.First);
          end;
        end;
      end;
    end;
  finally
    TsgObjectList.FreeList(vEntities);
  end;
end;

procedure TsgSVGPath.AddArc(const AArcFlags: Byte; const AAxisAnlge, AX, AY,
  ARX, ARY: Single; const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psARC_ABS, AAbsCoords),
    [AArcFlags, AAxisAnlge, AX, AY, ARX, ARY]);
end;

procedure TsgSVGPath.AddCloseFigure;
begin
  AddPathData(Byte(psCLOSEPATH), []);
end;

procedure TsgSVGPath.AddHorizLine(const AX: Single; const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psLINETO_HORIZONTAL_ABS, AAbsCoords), [AX]);
end;

procedure TsgSVGPath.AddLine(const AX, AY: Single; const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psLINETO_ABS, AAbsCoords), [AX, AY]);
end;

procedure TsgSVGPath.AddMove(const AX, AY: Single; const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psMOVETO_ABS, AAbsCoords), [AX, AY]);
end;

procedure TsgSVGPath.AddQuadraticBezier(const AX, AY, AX1, AY1: Single;
  const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psCURVETO_QUADRATIC_ABS, AAbsCoords),
    [AX, AY, AX1, AY1]);
end;

procedure TsgSVGPath.AddQuadraticBezierShort(const AX, AY: Single;
  const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psCURVETO_QUADRATIC_SMOOTH_ABS, AAbsCoords), [AX, AY]);
end;

procedure TsgSVGPath.AddCubicBezier(const AX, AY, AX1, AY1, AX2, AY2: Single;
  const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psCURVETO_CUBIC_ABS, AAbsCoords), [AX, AY, AX1, AY1, AX2, AY2]);
end;

procedure TsgSVGPath.AddCubicBezierShort(const AX, AY, AX2, AY2: Single;
  const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psCURVETO_CUBIC_SMOOTH_ABS, AAbsCoords),
    [AX, AY, AX2, AY2]);
end;

procedure TsgSVGPath.AddVertLine(const AY: Single; const AAbsCoords: Boolean);
begin
  AddPathData(GetSegType(psLINETO_VERTICAL_ABS, AAbsCoords), [AY]);
end;

procedure TsgSVGPath.AssignEntity(Source: TsgDXFEntity);
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGPath then
    CopyLists(FData, TsgSVGPath(Source).FData);
end;

function TsgSVGPath.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
var
  I: Integer;
  vSegmentType: TsgPathSegType;
begin
  inherited GetExtDataBody(AData);
  I := 0;
  while I < FData.Count do
  begin
    vSegmentType := TsgPathSegType(FData.List[I]);
{ TODO: TsgSVGPath.GetExtDataBody }
    ExportByte(AData, cnstED_SegType, I);
    case vSegmentType of
      psARC_ABS, psARC_REL:
        begin
          ExportByte(AData, cnstED_PointFlags, I);
          ExportDouble(AData, cnstED_PointParam, I);
          ExportPoint2D(AData, cnstED_Point2D1, I);
          ExportPoint2D(AData, cnstED_Point2D2, I);
        end;
      psMOVETO_ABS, psMOVETO_REL, psLINETO_ABS, psLINETO_REL,
        psCURVETO_QUADRATIC_SMOOTH_ABS, psCURVETO_QUADRATIC_SMOOTH_REL:
        begin
          ExportPoint2D(AData, cnstED_Point2D1, I);
        end;
      psLINETO_HORIZONTAL_ABS, psLINETO_HORIZONTAL_REL, psLINETO_VERTICAL_ABS,
        psLINETO_VERTICAL_REL:
        begin
          ExportDouble(AData, cnstED_PointParam, I);
        end;
      psCURVETO_CUBIC_ABS,  psCURVETO_CUBIC_REL:
        begin
          ExportPoint2D(AData, cnstED_Point2D1, I);
          ExportPoint2D(AData, cnstED_Point2D2, I);
          ExportPoint2D(AData, cnstED_Point2D3, I);
        end;
      psCURVETO_QUADRATIC_ABS, psCURVETO_QUADRATIC_REL,
        psCURVETO_CUBIC_SMOOTH_ABS, psCURVETO_CUBIC_SMOOTH_REL:
        begin
          ExportPoint2D(AData, cnstED_Point2D1, I);
          ExportPoint2D(AData, cnstED_Point2D2, I);
        end;
    end;
  end;
  Result := AData.DataCount > 0;
end;

procedure TsgSVGPath.SetExtData(const AData: TsgCADExtendedData);
var
  I, J: Integer;
  vSegmentType: TsgPathSegType;
  vSegmentValues: array [0..7] of Single;
  vPoint: TF2DPoint;
begin
  vSegmentType := psUNKNOWN;
  FillChar(vSegmentValues, Sizeof(vSegmentValues), 0);
  J := 0;
  AData.UseCode := False;
  for I := 0 to AData.DataCount - 1 do
  begin
    case AData.DataCode[I] of
      cnstED_SegType:
        begin
          if vSegmentType <> psUNKNOWN then
             AddPathData(Byte(vSegmentType), vSegmentValues);
          vSegmentType := TsgPathSegType(AData.DataByte[I]);
          J := 0;
          FillChar(vSegmentValues, Sizeof(vSegmentValues), 0);
        end;
      cnstED_PointFlags:
        begin
          vSegmentValues[J] := AData.DataByte[I];
          Inc(J);
        end;
      cnstED_PointParam:
        begin
          vSegmentValues[J] := AData.DataDouble[I];
          Inc(J);
        end;
      cnstED_Point2D1, cnstED_Point2D2, cnstED_Point2D3:
        begin
          vPoint := AData.DataPoint2D[I];
          vSegmentValues[J] := vPoint.X;
          vSegmentValues[J + 1] := vPoint.Y;
          Inc(J, 2);
        end;
    end;
  end;
  if vSegmentType <> psUNKNOWN then
    AddPathData(Byte(vSegmentType), vSegmentValues);
  inherited SetExtData(AData);
end;

procedure TsgSVGPath.ClearData;
begin
  FData.Count := 0;
end;

function TsgSVGPath.GetArcFlags(const ALargeArcFlag, ASweepFlag: Boolean): Byte;
begin
  Result := Byte(ALargeArcFlag) + Byte(ASweepFlag) shl 1;
end;

procedure TsgSVGPath.CopyData(const APath: TsgSVGPath);
begin
  CopyLists(FData, APath.FData, cmAppend);
end;

constructor TsgSVGPath.Create;
begin
  inherited Create;
  FData := TList.Create;
end;

function TsgSVGPath.GetPropertiesType: Byte;
begin
  Result := 1;
end;

destructor TsgSVGPath.Destroy;
begin
  FreeAndNil(FDataList);
  ClearData;
  FData.Free;
  inherited Destroy;
end;

procedure TsgSVGPath.Generate;
var
  vEntities: TsgObjectList;
begin
  vEntities := TsgObjectList.Create;
  try
    ParseData(vEntities);
    AddEnts(vEntities);
  finally
    vEntities.Free;
  end;
end;

function TsgSVGPath.GetSVGType: TsvgEntity;
begin
  Result := svgPath;
end;

procedure TsgSVGPath.ParseData(AList: TsgObjectList);
var
  vPathLoader: TsgPathLoader;
begin
  vPathLoader := TsgPathLoader.Create;
  try
    vPathLoader.LoadEntities(Converter, FData, AList);
  finally
    vPathLoader.Free;
  end;
end;

{ TsgSVGText }

procedure TsgSVGText.AddFill(const AEntity: TsgDXFEntity);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgSVGText.AddFills(AEnts: TsgObjectList);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgSVGText.AssignEntity(Source: TsgDXFEntity);
var
  vText: TsgSVGText absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGText then
  begin
    FText := vText.FText;
    if vText.FDPoint <> nil then
    begin
      if FDPoint = nil then
        New(FDPoint);
      FDPoint^ := vText.FDPoint^;
    end
    else
      DisposeAndNil(FDPoint);
    Points1 := vText.Points1;
    PointsD := vText.PointsD;
  end;
end;

function TsgSVGText.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_TEXT);
  AData.AddString(String_1000, Text);
  if FDPoint <> nil then
  begin
    AData.AddInt16(Integer_1070, cnstED_Point2D2);
    AData.AddPoint2D(Float_1010, FDPoint^);
  end;
{ TODO: TsgSVGText.GetExtDataBody (AData.AddListPoints) }
  AData.AddFPoints(cnstED_PointsCount2, cnstED_Point3D1, FPoints1);
  AData.AddFPoints(cnstED_PointsCount3, cnstED_Point3D2, FPointsD);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGText.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);

  procedure SetCapacity(var AList: TFPointList; const ACount: Integer);
  begin
    if AList = nil then
    begin
      AList := TFPointList.Create;
      AList.Capacity := ACount;
    end;
  end;

begin
  case ACode of
    cnstED_TEXT:          Text := AData.DataString[AIndex];
    cnstED_Point2D2:      SetPointD2D(AData.DataPoint2D[AIndex]);
    cnstED_Point3D1:
      begin
        if FPoints1 = nil then
          FPoints1 := TFPointList.Create;
        FPoints1.Add(AData.DataPoint[AIndex]);
      end;
    cnstED_Point3D2:
      begin
        if FPointsD = nil then
          FPointsD := TFPointList.Create;
        FPointsD.Add(AData.DataPoint[AIndex]);
      end;
    cnstED_PointsCount2:  SetCapacity(FPoints1, AData.DataInt[AIndex]);
    cnstED_PointsCount3:  SetCapacity(FPointsD, AData.DataInt[AIndex]);
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

procedure TsgSVGText.AddText(const AStr: string; const AP: TFPoint; const AEnts: TsgObjectList; var ABox: TFRect);
var
  I, J, K: Integer;
  vText: TsgDXFTextAccess;
  vPoly: TsgDXFLWPolyline;
  vIndex: Integer;
  vTextGlyph: TsgTextGlyph;
  vPolylines: TsgObjectList;
  vMatrix: TFMatrix;
  vFillColor: TsgFillColor;
  vSColor: TColor;
  vPoints: TFPointList;
  TextLinesCollection: TsgTextLinesCollection;
  vTextImportMode: Integer;
begin
  vTextImportMode := -1;
  if Assigned(Converter) and (Converter.ImportMode = imImport) then
     vTextImportMode := TextImportMode;

  vFillColor := FillColor;
  vSColor := StrokeColor;

  vPolylines := nil;
  vText := TsgDXFTextAccess(TsgDXFtext.Create);
  try
    vText.Style := GetStyleByText;
    vText.Point := AP;
    vText.Text := AStr;
    vText.Height := FontHeight;
    SetEntProps(vText);
    TsgDXFTextAccess(vText).SetBackward(True);
    vText.Rotation := -180;
    if vFillColor.IsStyle then
      vText.ColorCAD := cnstColorCADByBlock
    else
      vText.ColorCAD := svgConvertARGBToColorCAD(vFillColor.Color);
    Converter.Loads(vText);
    UnionFRect(ABox, vText.Box);
    if (vTextImportMode = 2) or
      ((vTextImportMode <= 0) and (not vFillColor.IsStyle) and (vFillColor.Color <> vSColor) and (vSColor <> clByBlock) and (vSColor <> clNone)) then
    begin
      vIndex := ContainerOfTextGlyphs.IndexOf(vText.Properties.FontName, MVFontStylesToFontStyles(vText.Properties.FontStyle), vText.Charset);
      if vIndex > -1 then
      begin
        vPoints := TFPointList.Create;
        TextLinesCollection := TsgTextLinesCollection.Create(vPoints, True);
        try
          vTextGlyph := ContainerOfTextGlyphs.TextGlyph[vIndex];
          vMatrix := vText.GetMatrix;
          vTextGlyph.GetPolyPolyline(vText.Text, vText.UnicodeText, vMatrix, TextLinesCollection, vText.Tracking);
          K := 0;
          for I := 0 to TextLinesCollection.Counts.Count - 1 do
          begin
            vPoly := TsgDXFLWPolyline.Create;
            if vPolylines = nil then
            begin
              vPolylines := TsgObjectList.Create;
              vPolylines.Capacity := TextLinesCollection.Counts[I];
            end;
            vPolylines.Add(vPoly);
            for J := 0 to TextLinesCollection.Counts[I] - 1 do
            begin
              AddVertexInPolyline(vPoly, vPoints[K + J]);
              ExpandFRect(ABox, vPoints[K + J]);
            end;
            Inc(K, TextLinesCollection.Counts[I]);
          end;
        finally
          TextLinesCollection.Free;
        end;
      end;
    end;
  finally
    case vTextImportMode of
      0:
        begin
          AEnts.Add(vText);
          if Assigned(vPolylines) then
            AEnts.Add(vPolylines);
        end;
      1:
        begin
          AEnts.Add(vText);
          TsgObjectList.FreeList(vPolylines);
        end;
      2:
        begin
          FreeAndNil(vText);
          if Assigned(vPolylines) then
            AEnts.Add(vPolylines);
        end
    else
      if IsEqualColorCAD(vText.ColorCAD, cnstColorCADNone) then
        FreeAndNil(vText)
      else
        AEnts.Add(vText);
      if Assigned(vPolylines) then
        AEnts.Add(vPolylines);
    end;
  end;
end;

constructor TsgSVGText.Create;
begin
  inherited Create;
  Properties.FillColor := cnstByBlockFillColor;
  Properties.FontFamily := cnstDefFontFamily;
  Properties.FontSize := cnstDefFontSize;
  Properties.FontStyles := cnstDefFontStyles;
end;

destructor TsgSVGText.Destroy;
begin
  FreeAndNil(FPoints1);
  FreeAndNil(FPointsD);
  DisposeAndNil(FDPoint);
  inherited Destroy;
end;

function TsgSVGText.GetPropertiesType: Byte;
begin
  Result := 2;
end;

procedure ApplyTextAnchor(const Anchor: Byte; const AEnts: TsgObjectList; const ABox: TFRect; const AConv: TsgDXFConverter);
var
  I, J, K: Integer;
  vP1, vP2, vOffset: TFPoint;
  vObj: TObject;
  vText: TsgDXFText absolute vObj;
  vList: TsgObjectList absolute  vObj;
  vPoly: TsgDXFLWPolyline;
  vVertex: TsgDXFVertex;
begin
  if Anchor = 0 then Exit;
  vP1 := ABox.TopLeft;
  vP2 := vP1;
  if Anchor = 1 then
    vP2.X := (ABox.Left + ABox.Right) * 0.5
  else
    vP2.X := ABox.Right;
  vOffset := SubFPoint(vP2, vP1);
  for I := 0 to AEnts.Count - 1 do
  begin
   vObj := AEnts.List[I];
   if vObj is TsgDXFText then
   begin
     vText.Point := SubFPoint(vText.Point, vOffset);
     AConv.Loads(vText);
   end
   else
   begin
     if vObj is TsgObjectList then
     begin
       for J := 0 to vList.Count - 1 do
       begin
         vPoly := TsgDXFLWPolyline(vList[J]);
         for K := 0 to vPoly.Count - 1 do
         begin
           vVertex := TsgDXFVertex(vPoly.Entities[K]);
           vVertex.Point := SubFPoint(vVertex.Point, vOffset);
         end;
       end;
     end;
   end;
  end;
end;

procedure TsgSVGText.Generate;

  procedure CreatePointList(var AList: TFPointList; const AValue: TFPoint; const ACount: Integer);
//  var
//    I: Integer;
  begin
    AList := TFPointList.Create;
    AList.Capacity := ACount;
    //for I := 1 to ACount do
      AList.Add(AValue);
  end;

  function TrimEx(const S: string): string;
  var
    I, L: Integer;
  begin
    Result := '';
    L := Length(S);
    I := 1;
    if (L > 0) and (S[I] >= ' ') and (S[L] > ' ') then
    begin
      Result := S;
      Exit;
    end;
    while (I <= L) and (S[I] <= ' ') do
      Inc(I);
    if I > L then
      Exit;
    while (S[L] < ' ') or ((S[L] = ' ') and (S[L - 1] = ' ')) do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;

var
  vEntities: TsgObjectList;
  vNoStrLast: Boolean;
  I, L, vLen, C, vIndexPoint: Integer;
  vStr, S: string;
  vPoints1, vPointsD: TFPointList;
  vBox: TFRect;
  vObj: TObject;
  vP, vD: TFPoint;
begin
  Block.Offset := cnstFPointZero;
  vStr := TrimEx(Text);
  ReplaceASCICode(vStr);
  ReplaceWebOrAnsiCode(True, vStr);

  vBox := cnstBadRect;
  vEntities := TsgObjectList.Create;
  try
    if (FPoints1 = nil) and (FPointsD = nil) then
      AddText(vStr, AddFPoint2D(Point1, PointD), vEntities, vBox)
    else
    begin
      vPoints1 := FPoints1;
      vPointsD := FPointsD;
      try
        if vPoints1 = nil then
          CreatePointList(vPoints1, Point1, vPointsD.Count)
        else
        begin
          if vPointsD = nil then
            CreatePointList(vPointsD, PointD, vPoints1.Count);
        end;
        L := 1;
        vLen := Length(vStr);
        C := MaxI(vPoints1.Count, vPointsD.Count);
        vNoStrLast := True;
        vBox := cnstBadRect;
        while (L <= vLen) and vNoStrLast do
        begin
          vNoStrLast := L < C;
          if vNoStrLast then
            S := vStr[L]
          else
            S := Copy(vStr, L, vLen - L + 1);
          vIndexPoint := L - 1;
          if vIndexPoint < vPoints1.Count then
            vP := vPoints1[vIndexPoint]
          else
          begin
            vP := cnstFPointZero;
            if not IsBadRect(vBox) then
            begin
              vP := vPoints1.Last;
              for I := vPoints1.Count - 1 to vIndexPoint - 1 do
              begin
                if I < vPointsD.Count then
                  vP := AddFPoint2D(vP, vPointsD[I]);
              end;
              vP.X := vBox.Right;
            end;
          end;
          if vIndexPoint < vPointsD.Count then
            vD := vPointsD[vIndexPoint]
          else
            vD := cnstFPointZero;
          vP := AddFPoint2D(vP, vD);
          AddText(S, vP, vEntities, vBox);
          Inc(L);
        end;
      finally
        if vPoints1 <> FPoints1 then
          FreeAndNil(vPoints1);
        if vPointsD <> FPointsD then
          FreeAndNil(vPointsD);
      end;
    end;
    ApplyTextAnchor(TextAnchor, vEntities, vBox, Converter);
    for I := 0 to vEntities.Count - 1 do
    begin
      vObj := TObject(vEntities[I]);
      if vObj is TsgDXFText then
        Block.AddEntity(TsgDXFEntity(vEntities[I]))
      else
      begin
        if vObj is TsgObjectList then
        begin
          AddEnts(TsgObjectList(vObj));
          vEntities[I] := nil;
          TsgObjectList(vObj).Free;
        end;
      end;
    end;
    vEntities.Count := 0;
  finally
    TsgObjectList.FreeList(vEntities);
  end;
end;

function TsgSVGText.GetDX: Double;
begin
  if FDPoint = nil then
    Result := 0
  else
    Result := FDPoint^.X;
end;

function TsgSVGText.GetDY: Double;
begin
  if FDPoint = nil then
    Result := 0
  else
    Result := FDPoint^.Y;
end;

function TsgSVGText.GetFontHeight: Double;
begin
  Result := GetTextHeigthBySize(FontName, FontSize);
end;

function TsgSVGText.GetPointD: TFPoint;
begin
  if FDPoint = nil then
    Result := cnstFPointZero
  else
    Result := MakeFPointFrom2D(FDPoint^);
end;

function TsgSVGText.GetStyleByText: TsgDXFStyle;
var
  I: TmvFontStyle;
  vStyleName: string;
  vFontStyles: TmvFontStyles;
begin
  vFontStyles := FontStyles + GetFontStylesDecoration(TextDecoration);
  vStyleName := FontName;
  if vFontStyles <> [] then
  begin
    vStyleName := vStyleName + '_';
    for I := Low(TmvFontStyle) to High(TmvFontStyle) do
      if I in vFontStyles then
        vStyleName := vStyleName + cnstFontStylePostFix[I];
  end;
  Result := TsgDXFStyle(TsgDXFTable(Converter.Sections[csStyles]).FindEntByName(vStyleName));
  if Result = nil then
  begin
    Result := Converter.StyleByName(vStyleName);
    Result.PrimaryFont := TTF.GetFileNameByFontName(FontName, MVFontStylesToFontStyles(vFontStyles));
    Result.FontName := FontName;    
    Result.FontStyle := vFontStyles;
    Converter.Loads(Result);
  end;
end;

function TsgSVGText.GetSVGType: TsvgEntity;
begin
  Result := svgText;
end;

function TsgSVGText.GetText: string;
begin
  Result := FText;
end;

procedure TsgSVGText.SetDX(const AValue: Double);
begin
  SetF2DPointCoord(FDPoint, 0, AValue);
end;

procedure TsgSVGText.SetDY(const AValue: Double);
begin
  SetF2DPointCoord(FDPoint, 1, AValue);
end;

procedure TsgSVGText.SetFontHeight(const AValue: Double);
begin
  FontSize := GetTextSizeByHeigth(FontName, AValue);
end;

procedure TsgSVGText.SetPointD(const AValue: TFPoint);
begin
  SetPointD2D(MakeF2DPoint(AValue.X, AValue.Y));
end;

procedure TsgSVGText.SetPointD2D(const AValue: TF2DPoint);
begin
  SetF2DPointValue(FDPoint, AValue);
end;

procedure TsgSVGText.SetPoints1(const AValue: TFPointList);
begin
  SetList(FPoints1, AValue);
end;

procedure TsgSVGText.SetPointsD(const AValue: TFPointList);
begin
  SetList(FPointsD, AValue);
end;

procedure TsgSVGText.SetList(var D: TFPointList; const S: TFPointList);
begin
  if S <> nil then
  begin
    if D = nil then
      D := TFPointList.Create;
    D.Assign(S);
  end
  else
    FreeAndNil(D);
end;

procedure TsgSVGText.SetText(const AValue: string);
begin
  FText := AValue;
end;

{  TsgSVGTextPath  }

procedure TsgSVGTextPath.AssignEntity(Source: TsgDXFEntity);
var
  vText: TsgSVGTextPath absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGTextPath then
    FText := vText.FText;
end;

function TsgSVGTextPath.GetExtDataBody(
  const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_TEXT);
  AData.AddString(String_1000, Text);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGTextPath.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_TEXT:  Text := AData.DataString[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

function TsgSVGTextPath.GetPropertiesType: Byte;
begin
  Result := 2;
end;

procedure TsgSVGTextPath.Generate;
var
  vGlyphsBox, vPath: TList;
  vGlyphs: TsgObjectList;
  vPathLengs, vGlyphLengs: TsgDoubleList;
  I, vStartI, vEntI, vGlypI: Integer;
  vGlypLeng, vPathLeng, vDeltaLeng, vFullPathLeng, vCurFullLeng: Double;
  vBox: PFRect;
  vCollection: TsvgTextLinesCollection;

  procedure FreeGliphs;
  begin
    TsgObjectList.ClearList(vGlyphs);
    vGlyphs.Free;
  end;

begin
  inherited Generate;
  if (FEref <> nil) and (Length(FText) > 0) then
  begin
    vGlyphs := TsgObjectList.Create;
    vGlyphsBox := TList.Create;
    vPath := TList.Create;
    try
      LoadGlyphs(vGlyphs, vGlyphsBox);
      LoadPath(vPath);
      if (vGlyphs.Count > 0) and (vPath.Count > 0) then
      begin
        vPathLengs := TsgDoubleList.Create(0);
        vGlyphLengs := TsgDoubleList.Create(0);
        try
          vPathLengs.Capacity := vPath.Count;
          vGlyphLengs.Capacity := vGlyphs.Count;

          vFullPathLeng := 0;
          for I := 1 to vPath.Count - 1 do
          begin
            vPathLeng := DistanceFPoint(PFPoint(vPath.List[I - 1])^, PFPoint(vPath.List[I])^);
            vPathLengs.Add(vPathLeng);
            vFullPathLeng := vFullPathLeng + vPathLeng;
          end;
          for I := 0 to vGlyphsBox.Count - 1 do
          begin
            vBox := vGlyphsBox.List[I];
            vGlyphLengs.Add(vBox^.Right - vBox^.Left);
            Dispose(vBox);
          end;
          FreeAndNil(vGlyphsBox);

          vStartI := 0;
          vEntI := 0;
          vGlypI := 0;
          vGlypLeng := vGlyphLengs[vGlypI];

          vCurFullLeng := 0;
          while (MaxI(vEntI, vStartI) < vPathLengs.Count) and (vGlypI < vGlyphLengs.Count) do
          begin
            vDeltaLeng := vPathLengs[vEntI] - vGlypLeng;
            if vDeltaLeng >= 0 then
            begin
              vCurFullLeng := vCurFullLeng + vGlypLeng;
              vCollection := TsvgTextLinesCollection(vGlyphs[vGlypI]);
              AddGlyphAsPoly(Slice(TFPointList(vCollection.Poly).List^, TFPointList(vCollection.Poly).Count),
                Slice(vCollection.Counts.List^, vCollection.Counts.Count),
                vPath.List[vStartI], vPath.List[vStartI + 1]);
              vPathLengs[vEntI] := vDeltaLeng;
              PFPoint(vPath.List[vEntI])^ := GetPointOnLine(PFPoint(vPath.List[vEntI])^, PFPoint(vPath.List[vEntI + 1])^, vGlypLeng);
              vStartI := vEntI;
              Inc(vGlypI);
              vGlypLeng := vGlyphLengs[vGlypI];
            end
            else
            begin
              vCurFullLeng := vCurFullLeng + vPathLengs[vEntI];
              vGlypLeng := -vDeltaLeng;
              Inc(vEntI);
            end;
            if vCurFullLeng >= vFullPathLeng then
              Break;
          end;
        finally
          vGlyphLengs.Free;
          vPathLengs.Free;
        end;
      end;
    finally
      FreeRecordList(vPath);
      FreeRecordList(vGlyphsBox);
      FreeGliphs;
    end;
  end;
end;

procedure TsgSVGTextPath.AddGlyphAsPoly(const APoints: array of TFPoint;
  const ACounts: array of Integer; const AP1, AP2: PFPoint);
var
  vLWPoly: TsgDXFLWPolyline;
  I, J, K: Integer;
  vMat: TFMatrix;
  vGlyph: TsgObjectList;
begin
  vMat := FMatXMat2D(FMatByTranslate(AP1^.X, AP1^.Y, 0),
    FMatByAngle(GetAngleByPoints(AP1^, AP2^, False)));
  vGlyph := TsgObjectList.Create;
  try
    K := 0;
    for I := Low(ACounts) to High(ACounts) do
    begin
      if ACounts[I] > 0 then
      begin
        vLWPoly := TsgDXFLWPolyline.Create;
        vGlyph.Add(vLWPoly);
        for J := 0 to ACounts[I] - 1 do
          AddVertexInPolyline(vLWPoly, FPointXMat(APoints[J + K], vMat));
        Inc(K, ACounts[I]);
      end;
    end;
    AddEnts(vGlyph);
  finally
    vGlyph.Free;
  end;
end;

function TsgSVGTextPath.GetStyleByText: TsgDXFStyle;
var
  I: TmvFontStyle;
  vStyleName: string;
  vFontStyles: TmvFontStyles;
begin
  vFontStyles := FontStyles;
  vStyleName := FontName;
  if vFontStyles <> [] then
  begin
    vStyleName := vStyleName + ' ';
    for I := Low(TmvFontStyle) to High(TmvFontStyle) do
      if I in vFontStyles then
        vStyleName := vStyleName + cnstFontStylePostFix[I];
  end;
  Result := TsgDXFStyle(TsgDXFTable(Converter.Sections[csStyles]).FindEntByName(vStyleName));
  if Result = nil then
  begin
    Result := Converter.StyleByName(vStyleName);
    Result.FontName := FontName;
    Result.FontStyle := vFontStyles;
    Result.PrimaryFont := '';
    Converter.Loads(Result);
  end;
end;

procedure TsgSVGTextPath.LoadGlyphs(const AGlyphs: TsgObjectList;
  AGlyphsBox: TList);
var
  vText: TsgDXFTextAccess;
  I: Integer;
  vTextGlyph: TsgTextGlyphAccess;
  vMatrix: TFMatrix;
  vBox: PFRect;
  vStr: string;
  vUnderline, vStrikeout: Boolean;
  vCollection: TsvgTextLinesCollection;
  vTextItems: TStringBuilder;
begin
  vStr := Trim(Text);
  ReplaceASCICode(vStr);
  ReplaceWebOrAnsiCode(True, vStr);
  vText := TsgDXFTextAccess(TsgDXFText.Create);
  try
    vText.Style := GetStyleByText;
    vText.Point := cnstFPointZero;
    vText.Text := vStr;
    vText.Height := FontHeight;
    SetEntProps(vText);
    TsgDXFTextAccess(vText).SetBackward(True);
    vText.Rotation := -180;
    Converter.Loads(vText);
    vTextGlyph := TsgTextGlyphAccess(TsgDXFStyleAccess(vText.Properties).FFontGlyphRef);

    AGlyphsBox.Capacity := AGlyphsBox.Capacity + Length(vStr);
    vMatrix := vText.GetMatrix;
    vUnderline := fmUnderline in vText.Properties.FontStyle;
    vStrikeout := fmStrikeOut in vText.Properties.FontStyle;
    for I := 1 to Length(vStr) do
    begin
      vTextItems := TStringBuilder.{$IFNDEF SGDEL_7}CreateFromWideString{$ELSE}Create{$ENDIF}(vStr[I]);
      try
        New(vBox);
        AGlyphsBox.Add(vBox);
        vCollection := TsvgTextLinesCollection.Create(TFPointList.Create, True);
        AGlyphs.Add(vCollection);
        vTextGlyph.GetPolyPolyline(vTextItems, vMatrix, vCollection, vText.Tracking, nil, vUnderline, vStrikeout);
        vBox^ := GetRealBox(vTextGlyph.GetTextBox(vTextItems, vText.Tracking), vMatrix);
      finally
        vTextItems.Free;
      end;
    end;
  finally
    vText.Free;
  end;
end;

procedure TsgSVGTextPath.LoadPath(const APath: TList);
var
  vPoly: TsgDXFLWPolyline;
  vMat: TFMatrix;
  I, J: Integer;
  P: PFPoint;
  vPoint: TFPoint;
  vPath: TsgSVGPath;
begin
  vPath := TsgSVGPath.Create;
  try
    vPath.StrokeColor := clBlack;
    vPath.FillColor := GetFillColorEx(clNone);
    vPath.TransformMatrix := TsgSVGPath(FEref).TransformMatrix;
    vPath.CopyData(TsgSVGPath(FEref));
    Converter.Loads(vPath);
    vMat := vPath.TransformMatrix;
    for I := 0 to vPath.Block.Count - 1 do
    begin
      vPoly := TsgDXFLWPolyline(vPath.Block.Entities[I]);
      if vPoly.EntType = ceLWPolyline then
      begin
        for J := 0 to vPoly.Count - 1 do
        begin
          vPoint := FPointXMat(TsgDXFVertex(vPoly.Entities[J]).Point, vMat);
          if (APath.Count = 0) or (not IsEqualFPoints2D(vPoint, PFPoint(APath.List[APath.Count - 1])^)) then
          begin
            New(P);
            APath.Add(P);
            P^ := vPoint;
          end;
        end;
//      APath.Add(nil);
      end;
    end;
  finally
    vPath.Free;
  end;
end;

function TsgSVGTextPath.GetSVGType: TsvgEntity;
begin
  Result := svgTextPath;
end;

function TsgSVGTextPath.GetFontHeight: Double;
begin
  Result := GetTextHeigthBySize(FontName, FontSize);
end;

function TsgSVGTextPath.GetText: string;
begin
  Result := FText;
end;

procedure TsgSVGTextPath.SetFontHeight(const AValue: Double);
begin
  FontSize := GetTextSizeByHeigth(FontName, AValue);
end;

procedure TsgSVGTextPath.SetText(const AValue: string);
begin
  FText := AValue;
end;

{ TsgSVGUse }

procedure TsgSVGUse.AssignEntity(Source: TsgDXFEntity);
var
  vUse: TsgSVGUse absolute Source;
begin
  inherited AssignEntity(Source);
  if Source is TsgSVGUse then
  begin
    FPoint1 := vUse.FPoint1;
    FSize := vUse.FSize;
  end;
end;

function TsgSVGUse.GetExtDataBody(const AData: TsgCADExtendedData): Boolean;
begin
  inherited GetExtDataBody(AData);
  AData.AddInt16(Integer_1070, cnstED_Point2D1);
  AData.AddPoint2D(Float_1010, FPoint1);
  AData.AddInt16(Integer_1070, cnstED_Point2D1);
  AData.AddPoint2D(Float_1010, FSize);
  Result := AData.DataCount > 0;
end;

procedure TsgSVGUse.SetExtDataProperty(const ACode: SmallInt; const AData: TsgCADExtendedData; const AIndex: Integer);
begin
  case ACode of
    cnstED_Point2D1:  FPoint1 := AData.DataPoint2D[AIndex];
    cnstED_Point2D2:  FSize := AData.DataPoint2D[AIndex];
  else
    inherited SetExtDataProperty(ACode, AData, AIndex);
  end;
end;

procedure TsgSVGUse.AddCopyAndSetProps(const ABlock: TsgDXFBlock; const AEntity, AOwner: TsgSVGContainer);
var
  vConatinerNew: TsgSVGContainer;
  I: Integer;
  vEnt: TsgDXFEntity;
  vBlock: TsgDXFBlock;
begin
  vConatinerNew := TsgClassOfContainer(AEntity.ClassType).Create;
  ABlock.IsLoaded := False;
  vBlock := vConatinerNew.Block;
  try
    vConatinerNew.Block := nil;
    ABlock.AddEntity(vConatinerNew);
    vConatinerNew.AssignEntity(AEntity);
  finally
    vConatinerNew.Block := vBlock;
  end;
  vConatinerNew.Owner := AOwner;
  vConatinerNew.Visibility := True;
  if (TsgSVGContainerAccess(AEntity).GetSVGType in [svgContainer, svgGroup]) and (AEntity.Block <> nil) then
  begin
    for I := vConatinerNew.Block.Count - 1 downto 0 do
    begin
      vEnt := vConatinerNew.Block.Entities[I];
      vConatinerNew.Block.DeleteEntity(I);
      if AEntity.Block.IndexOfEntity(vEnt) < 0 then
        FreeAndNil(vEnt);
    end;
    for I := 0 to AEntity.Block.Count - 1 do
      AddCopyAndSetProps(vConatinerNew.Block, TsgSVGContainer(AEntity.Block.Entities[I]), vConatinerNew);
  end;
  Converter.Loads(vConatinerNew);
end;

function TsgSVGUse.GetPropertiesType: Byte;
begin
  Result := 1;
end;

procedure TsgSVGUse.Generate;
begin
  inherited Generate;
  if FEref <> nil then
    AddCopyAndSetProps(Block, FEref, Self);
end;

//function TsgSVGUse.GetBox: TFRect;
//begin
//  if Width * Height > 0 then
//  begin
//    Result.TopLeft := Point1;
//    Result.BottomRight := Result.TopLeft;
//    ExpandFRect(Result, AddFPoint2D(Point1, Size));
//    if HasTransformMatrix then
//      TransRectCorners(Result, TransformMatrix);
//  end
//  else
//    Result := inherited GetBox;
//end;

function TsgSVGUse.GetSVGType: TsvgEntity;
begin
  Result := svgUse;
end;

function TsgSVGUse.GetUsedMatrix: TFMatrix;
var
  S: TFPoint;
  vBox: TFRect;
begin
  Result := inherited GetUsedMatrix;
  S := cnstFPointSingle;
  if (Eref <> nil) and (Block.Count > 0) and (Width * Height > 0) and (TsgSVGContainerAccess(Eref).GetSVGType in [svgGroup, svgGroupDim]) then
  begin
    if TsgSVGGroup(Eref).HasViewBox or ((TsgSVGContainerAccess(Eref).GetSVGType = svgGroupDim) and (TsgSVGGroupDim(Eref).Width *  TsgSVGGroupDim(Eref).Height > 0)) then
    begin
      vBox := Block.Box;
      S.X := GetDivKoef(Width, vBox.Right - vBox.Left);
      S.Y := GetDivKoef(Height, vBox.Top - vBox.Bottom);
      if S.X >= S.Y then
        S.X := S.Y
      else
        S.Y := S.X;
    end;
  end;
  Result := FMatXMat(StdMat(S, Point1), Result);
end;

function TsgSVGUse.GetX1: Double;
begin
  Result := FPoint1.X;
end;

function TsgSVGUse.GetY1: Double;
begin
  Result := FPoint1.Y;
end;

function TsgSVGUse.GetHeight: Double;
begin
  Result := FSize.Y;
end;

function TsgSVGUse.GetWidth: Double;
begin
  Result := FSize.X;
end;

function TsgSVGUse.GetPoint1: TFPoint;
begin
  Result.X := FPoint1.X;
  Result.Y := FPoint1.Y;
  Result.Z := 0;
end;

function TsgSVGUse.GetSize: TFPoint;
begin
  Result.X := FSize.X;
  Result.Y := FSize.Y;
  Result.Z := 0;
end;

procedure TsgSVGUse.SetX1(const AValue: Double);
begin
  FPoint1.X := AValue;
end;

procedure TsgSVGUse.SetY1(const AValue: Double);
begin
  FPoint1.Y := AValue;
end;

procedure TsgSVGUse.SetHeight(const AValue: Double);
begin
  FSize.Y := AValue;
end;

procedure TsgSVGUse.SetWidth(const AValue: Double);
begin
  FSize.X := AValue;
end;

procedure TsgSVGUse.SetPoint1(const AValue: TFPoint);
begin
  FPoint1.X := AValue.X;
  FPoint1.Y := AValue.Y;
end;

procedure TsgSVGUse.SetSize(const AValue: TFPoint);
begin
  FSize.X := AValue.X;
  FSize.Y := AValue.Y;
end;

{ TsvgNode_LGRADIENT }

procedure TsvgNode_LGRADIENT.Assign(const Obj: TObject);
var
  G: TsvgNode_LGRADIENT absolute Obj;
begin
  inherited Assign(Obj);
  if Obj is TsvgNode_LGRADIENT then
  begin
    FStopColor := G.FStopColor;
    FStopOffset := G.FStopOffset;
    FStopOpasity := G.FStopOpasity;
  end;
end;

  function GetDoubleValue(const Units: TsgStyleFillUnits; const Attrib: TsgNodeSample): Double;
  begin
    Result := Attrib.ValueAsDouble;
    if (Units = sfObjectBoundingBox) and (Length(Attrib.Value) > 0) then
    begin
      if StringScan('%', Attrib.Value, 1) < 1 then
        Result := Result * 100;
    end;
  end;

function TsvgNode_LGRADIENT.GetDataInternal(const AOwner: TObject): TObject;

  function GetSpreadMethod(const AStr: string): Integer;
  const
    cnstSpreadMethods: array[0..2] of string = ('pad','reflect','repeat');
  var
    S: string;
  begin
    Result := 0;
    S := AnsiLowerCase(AStr);
    if S = cnstSpreadMethods[1] then
      Result := 1
    else
      if S = cnstSpreadMethods[2] then
        Result := 2;
  end;

var
  vAttrib, vStops: TsgNodeSample;
  vGradient: TsgCADStyleGradient;
  vName, vHRef: string;
  I, J: Integer;
  vID: TsgTagID;
  vAttribsID: set of TsgTagID;
begin
  Result := nil;
  if HasAttributeNodes then
  begin
    vGradient := TsgCADStyleGradient.Create;
    try
      vGradient.Units := sfObjectBoundingBox;
      vGradient.Linear := IsLinear;
      if vGradient.Linear then //REC-SVG11-20030114/pservers.html#Gradients
        vGradient.X2 := 100
      else
      begin
        vGradient.CX := 50;
        vGradient.CY := 50;
        vGradient.R := 50;
        vGradient.FX := cnstNan;
        vGradient.FY := cnstNan;
      end;  
      vAttrib := GetAttributeByTagID(idGradientUnits);
      if vAttrib <> nil then
      begin
        if AnsiLowerCase(vAttrib.Value) = cnstObjectBoundingBox then
          vGradient.Units := sfObjectBoundingBox
        else
          vGradient.Units := sfUserSpaceOnUse;
      end;
      vAttribsID := [];
      for I := 0 to AttributeNodesCount - 1 do
      begin
        vAttrib := AttributeNodes[I];
        vID := GetTagIdByName(vAttrib.Name);
        vAttribsID := vAttribsID + [vID];
        case vID of
          idID:                 vName := vAttrib.Value;
          idGradientTransform:  vGradient.Transform := GetTramsformMatrix(vAttrib.Value);
          idSpreadMethod:       begin end;
          idHref:               vHref := vAttrib.Value;

          idX1: vGradient.X1 := GetDoubleValue(vGradient.Units, vAttrib);
          idY1: vGradient.Y1 := GetDoubleValue(vGradient.Units, vAttrib);
          idX2: vGradient.X2 := GetDoubleValue(vGradient.Units, vAttrib);
          idY2: vGradient.Y2 := GetDoubleValue(vGradient.Units, vAttrib);

          idCX: vGradient.CX := GetDoubleValue(vGradient.Units, vAttrib);
          idCY: vGradient.CY := GetDoubleValue(vGradient.Units, vAttrib);
          idR:  vGradient.R  := GetDoubleValue(vGradient.Units, vAttrib);
          idFX: vGradient.FX := GetDoubleValue(vGradient.Units, vAttrib);
          idFY: vGradient.FY := GetDoubleValue(vGradient.Units, vAttrib);
        end;
      end;
      if vAttribsID * [idX1, idY1, idX2, idY2, idCX, idCY, idR, idFX, idFY] = [] then
        vGradient.Units := sfObjectBoundingBox;
      if not vGradient.Linear then
      begin
        if sgIsNan(vGradient.FX) then
          vGradient.FX := vGradient.CX;
        if sgIsNan(vGradient.FY) then
          vGradient.FY := vGradient.CY;
      end;
      if HasChildNodes then
      begin
        FSubNodeId := niStop;
        try
          for I := 0 to ChildNodesCount - 1 do
          begin
            vStops := ChildNodes[I];
            FStopOpasity := 1;
            FStopColor := clBlack;
            if AnsiLowerCase(vStops.Name) = 'stop' then
            begin
              if vStops.HasAttributeNodes then
              begin
                for J := 0 to vStops.AttributeNodesCount - 1 do
                begin
                  vAttrib := vStops.AttributeNodes[J];
                  SetSvgEntPropsBase(nil, GetTagIdByName(vAttrib.Name), vAttrib)
                end;
              end;
              FStopColor := GetColorWithOpacity(FStopColor, FStopOpasity);
              vGradient.AddStops(FStopColor, FStopOffset);
            end;
          end;
        finally
          FSubNodeId := niDefault;
        end;
        vGradient.CheckParams;
      end;
      if (Length(vName) > 0){ and vGradient.HasStops } then
      begin
        vGradient := TsvgReader(FReader).AddSVGStyleFill(vName, vGradient);
        if Length(vHref) > 0 then
        begin
          if vHref[1] = '#' then
            Delete(vHref, 1, 1);
          I := TsvgReader(FReader).IndexSVGStyleFillOfName(vHref);
          if I > -1 then
            vGradient.HRef := TsgCADStyleGradient(TsvgReader(FReader).SVGStyleFills.Objects[I]);
        end;
        vGradient := nil;
      end;
    finally
      if vGradient <> nil then
        vGradient.Free;
    end;
  end;
end;

function TsvgNode_LGRADIENT.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niLGRADIENT;
end;

function TsvgNode_LGRADIENT.IsLinear: Boolean;
begin
  Result := True;
end;

function TsvgNode_LGRADIENT.SetSvgEntPropsBase(const AEntity: TsgDXFEntity;
  const AID: TsgTagID; const Attrib: TsgNodeSample): Boolean;
var
  vNodeId: TsgSVGNodeID;
begin
  Result := True;
  case AID of
    idOffset:       FStopOffset := Attrib.ValueAsDouble;
    idStopColor:    FStopColor := StrToColor(Attrib.Value);
    idStopOpacity:  FStopOpasity := Attrib.ValueAsDouble;
    idStyle:        SetStyle(AEntity, Attrib.Value);
    idClass:
      begin
        vNodeId := SVGNodeID;
        if FSubNodeId <> niDefault then
          vNodeId := FSubNodeId;
        SetClass(AEntity, Attrib.Value, False, vNodeId);
      end;
  end;
end;

{ TsvgNode_RGRADIENT }

function TsvgNode_RGRADIENT.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niRGRADIENT;
end;

function TsvgNode_RGRADIENT.IsLinear: Boolean;
begin
  Result := False;
end;

{  TsvgNode_PATTERN  }

function TsvgNode_PATTERN.GetSVGNodeID: TsgSVGNodeID;
begin
  Result := niPATTERN;
end;

function TsvgNode_PATTERN.GetDataInternal(const AOwner: TObject): TObject;
var
  vPattern: TsgCADStylePattern;
  vAttrib: TsgNodeSample;
  vID: TsgTagID;
  vAttribsID: set of TsgTagID;
  I: Integer;
  vName, vHref: string;
  vContainer: TsgSVGGroup;
  vViewBox: PsgViewBox;
begin
  Result := nil;
  vViewBox := nil;
  vPattern := TsgCADStylePattern.Create;
  try
    vPattern.Units := sfObjectBoundingBox;
    vPattern.ContentUnits := sfUserSpaceOnUse;
    if HasAttributeNodes then
    begin
      vAttribsID := [];
      vAttrib := GetAttributeByTagID(idPatternUnits);
      if vAttrib <> nil then
      begin
        if AnsiLowerCase(vAttrib.Value) = cnstObjectBoundingBox then
          vPattern.Units := sfObjectBoundingBox
        else
          vPattern.Units := sfUserSpaceOnUse;
      end;
      for I := 0 to AttributeNodesCount - 1 do
      begin
        vAttrib := AttributeNodes[I];
        vID := GetTagIdByName(vAttrib.Name);
        vAttribsID := vAttribsID + [vID];
        case vID of
          idID:                  vName := vAttrib.Value;
          idGradientTransform:   vPattern.Transform := GetTramsformMatrix(vAttrib.Value);
          idPatternContentUnits:
            begin
              if AnsiLowerCase(vAttrib.Value) = cnstObjectBoundingBox then
                vPattern.ContentUnits := sfObjectBoundingBox
              else
                vPattern.ContentUnits := sfUserSpaceOnUse;
            end;
          idHref:                vHref := vAttrib.Value;
          idX:                   vPattern.X1 := GetDoubleValue(vPattern.Units, vAttrib);
          idY:                   vPattern.Y1 := GetDoubleValue(vPattern.Units, vAttrib);
          idWidth:               vPattern.Width := GetDoubleValue(vPattern.Units, vAttrib);
          idHeight:              vPattern.Height := GetDoubleValue(vPattern.Units, vAttrib);
          idViewBox:             SetViewBox(vViewBox, vAttrib.Value);
          idPreserveAspectRatio:
            begin
              if vViewBox = nil then
              begin
                New(vViewBox);
                vViewBox^ := cnstDefViewBox;
              end;
              vViewBox^.Mode := GetPreserveAspectRatio(vAttrib.Value);
            end;
        end;
      end;
      if vAttribsID * [idX, idY, idWidth, idHeight] = [] then
        vPattern.Units := sfObjectBoundingBox;
    end;

    if  vPattern.Units = sfObjectBoundingBox then
    begin
      vContainer := TsgSVGGroup(TsvgReader(FReader).CreateGroup(TsgSVGGroup, nil));
      try
        TsvgReader(FReader).AnalizeChildNodes(vContainer, Self);
        while vContainer.Block.Count > 0 do
        begin
          vPattern.AddEntity(vContainer.Block[0]);
          vContainer.Block.DeleteEntity(0);
        end;
        vPattern.Loaded(TsvgReader(FReader).Conv);
      finally
        TsvgReader(FReader).FreeGroup(vContainer);
      end;  
    end    
    else
    begin
      vContainer := TsgSVGGroup(TsvgReader(FReader).CreateGroup(TsgSVGGroup, nil));
      vPattern.AddEntity(vContainer);
      TsvgReader(FReader).AnalizeChildNodes(vContainer, Self);

      vContainer.CreateClipPoints(nil);

      vContainer.AddClipCoords(0, 0);
      vContainer.AddClipCoords(vPattern.Width, 0);
      vContainer.AddClipCoords(vPattern.Width, vPattern.Height);
      vContainer.AddClipCoords(0, vPattern.Height);

      TsvgReader(FReader).Conv.Loads(vContainer);
      vPattern.Loaded(TsvgReader(FReader).Conv);
    end;


    if vViewBox <> nil then
      vPattern.ViewBox := vViewBox^;
    for I := 0 to vPattern.Count - 1 do
      TsgSVGContainer(vPattern.Entities[I]).Owner := nil;
    vPattern.CheckParams;
    if Length(vHref) > 0 then
    begin
      if vHref[1] = '#' then
        Delete(vHref, 1, 1);
      I := TsvgReader(FReader).IndexSVGStyleFillOfName(vHref);
      if I > -1 then
        vPattern.HRef := TsgCADStylePattern(TsvgReader(FReader).SVGStyleFills.Objects[I]);
    end;
    if (Length(vName) > 0) and (not IsBadRect(vPattern.Box)) then
    begin
      TsvgReader(FReader).AddSVGStyleFill(vName, vPattern);
      vPattern := nil;
    end;
  finally
    if vViewBox <> nil then
      Dispose(vViewBox);
    if vPattern <> nil then
      vPattern.Free;
  end;
end;

{ TsgSVGMaterialImage }

constructor TsgSVGMaterialImage.Create;
begin
  inherited Create;
end;

destructor TsgSVGMaterialImage.Destroy;
begin
  inherited Destroy;
end;

function TsgSVGMaterialImage.GetParser: TsgParser;
begin
  Result := TsgSVGConverter(Converter).FParser;
end;

function TsgSVGMaterialImage.GetParserInternal: TsgParser;
begin
  Result := GetParser;
  if not Assigned(Result) then
  begin
    SetParser(TsgParser.Create);
    Result := GetParser;
  end;
end;

procedure TsgSVGMaterialImage.SetParser(const AParser: TsgParser);
begin
  TsgSVGConverter(Converter).FParser := AParser;
end;

{ TsvgTextPositions }

procedure TsvgPositions.AddPoint1(const AP: TFPoint; const AState: Integer = 3);
begin
  Points1.Add(AP);
  States1.Add(AState);
end;

procedure TsvgPositions.Clear;
begin
  Points1.Clear;
  States1.Clear;
  PointsD.Clear;
  StatesD.Clear;
end;

constructor TsvgPositions.Create;
begin
  Points1 := TFPointList.Create;
  States1 := TsgIntegerList.Create;
  PointsD := TFPointList.Create;
  StatesD := TsgIntegerList.Create;
end;

destructor TsvgPositions.Destroy;
begin
  Points1.Free;
  States1.Free;
  PointsD.Free;
  StatesD.Free;
  inherited Destroy;
end;

class procedure TsvgPositions.SetPointWithState(const AP: TFPoint;
  const ASt: Integer; var APoint: TFPoint; var AState: Integer);
begin
  if (AState and 1 = 0) and (ASt and 1 <> 0) then
  begin
    APoint.X := AP.X;
    AState := AState or 1;
  end;
  if (AState and 2 = 0) and (ASt and 2 <> 0) then
  begin
    APoint.Y := AP.Y;
    AState := AState or 2;
  end;
end;

class procedure TsvgPositions.Union(const APointsIn: TFPointList;
  const AStateIn: TsgIntegerList; const APointsOut: TFPointList;
  const AStateOut: TsgIntegerList);
var
  I: Integer;
  vS: Integer;
  vP: TFPoint;
begin
  case APointsIn.Count of
    0:  begin end;
    1:
      begin
        vS := 0;
        vP := cnstFPointZero;
        if APointsOut.Count > 0 then
        begin
          vP := APointsOut.First;
          vS := AStateOut.First;
        end;
        SetPointWithState(APointsIn.First, AStateIn.First, vP, vS);
        if APointsOut.Count > 0 then
        begin
          APointsOut.First := vP;
          AStateOut.First := vS;
        end
        else
        begin
          APointsOut.Add(vP);
          AStateOut.Add(vS);
        end;
      end;
  else
    while APointsOut.Count < APointsIn.Count do
    begin
      APointsOut.Add(cnstFPointZero);
      AStateOut.Add(0);
    end;
    for I := 0 to APointsIn.Count - 1 do
    begin
      vP := APointsOut[I];
      vS := AStateOut[I];
      SetPointWithState(APointsIn[I], AStateIn[I], vP, vS);
      APointsOut[I] := vP;
      AStateOut[I] := vS;
    end;
  end;
end;

{ TsgSVGPictureEnd }

procedure TsgSVGPictureEnt.ChangeGraphic;
var
  vTransp: Boolean;
  vTranspColor: Cardinal;
begin
  vTransp := Transparency;
  vTranspColor := TransparentColor;
  try
    inherited ChangeGraphic;
  finally
    if vTransp then
    begin
      Transparency := True;
      TransparentColor := vTranspColor;
    end;
  end;
end;

initialization
{$IFDEF SG_FIREMONKEY}
  PictureInternal := nil;
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_SVG := RegisterClipboardFormat('SoftGold SVG Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_SVG, TsgSVGImage);
  TPicture.RegisterFileFormat('SVG', SSVGImage, TsgSVGImage);
  TPicture.RegisterFileFormat('SVGZ', SSVGImage, TsgSVGImage);
  TPicture.RegisterFileFormat('SVGM', SSVGImage, TsgSVGMaterialImage);
{$IFDEF SG_XREF_EXTENDED}
  TsgDXFConverterAccess.RegisterXRefGraphicClass('svg', TsgSVGImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('svgz', TsgSVGImage);
{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
  InitParams;
{$ENDIF}
  RegisterClasses;
  // If init default locales names will be worked like in inkscape, chrome, yandex
  // else like in firefox, edge, IE
  InitLocales;
//  GlobalsgTypes.AddClass(TsgSVGContainer, 57);
  svgStacks := TsgObjectList.Create;

finalization
  FreeFullStack;
  TPicture.UnRegisterGraphicClass(TsgSVGImage);
  TPicture.UnRegisterGraphicClass(TsgSVGMaterialImage);
{$IFDEF SG_FIREMONKEY}
  FreeAndNil(PictureInternal);
{$ENDIF}

end.

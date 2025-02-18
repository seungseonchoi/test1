{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                  Denitions Entities                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDefinitions;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, System.UITypes,
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, Math, SysUtils, DXFConv, sgFunction, sgConsts,
  ExtData, sgLists, sgXMLParser;

type

  PsgFillColor = ^TsgFillColor;

//  TsgFillColor = class
//  private
//    FColor: TColor;
//    FIsStyle: Boolean;
//    FStyle: TsgCADStyleFill;
//  public
//    constructor Create;
//    property Color: TColor read FColor write FColor;
//    property IsStyle: Boolean read FIsStyle write FIsStyle;
//    property Style: TsgCADStyleFill read FStyle write FStyle;
//  end;

  TsgFillColor = record
    IsStyle: Boolean;
    case Byte of
      0: (Color: TColor);
//      1: (Style: TsgCADStyleFill);
      1: (Style: Pointer);
  end;

var
  SVGLoadEntity: TsgCADLoadEntitiy = nil;

const
  cnstDefFillColor: TsgFillColor = ();

  cnstDefFillRule: Byte = 0; //nonzero - Winding| evenodd - Alternate
  cnstDefFillOpacity = 1;
  cnstDefStroke: TColor = clNone;
  cnstDefStrokeWidth: Double = 1;//A zero value causes no stroke to be painted.
  cnstDefStrokeLinecap: Byte = 0;  //butt | round | square
  cnstDefStrokeLinejoin: Byte = 0; //	miter | round | bevel
  cnstDefStrokeMiterlimit = 4;
  cnstDefStrokeDashArray: Pointer = nil;
  cnstDefStrokeDashOffset = 0;
  cnstDefStrokeOpacity = 1;
  cnstDefDisplay: Byte = 0; // inline | block | list-item | run-in | compact |
                            // marker | table | inline-table | table-row-group |
                            // table-header-group | table-footer-group |
                            // table-row | table-column-group | table-column |
                            // table-cell | table-caption | none
  cnstDefVisibility: Byte = 0; // visible | hidden | collapse
  cnstDefOpacity: Single = 1;
  cnstByBlockFillColor: TsgFillColor = (IsStyle: False; Color: clByBlock;);
  cnstDefTextAnchor: Byte = 0;           // start | middle | end
  cnstDefTextDecoration: Byte = 0;       // none | [ underline || overline ||
                                         // line-through || blink ]
  cnstDefFontFamily: string = 'Arial';   // depends on user agent
  cnstDefFileName: string = 'arial.ttf';
  cnstDefFontStyles: TmvFontStyles = []; // normal | italic | oblique
  cnstDefFontVariant: Byte = 0;  //	normal | small-caps
  cnstDefFontWeight: Double = 0; // normal | bold | bolder | lighter | 100 |
                                 // 200 | 300 | 400 | 500 | 600 | 700 | 800 |
                                 // 900
  cnstDefFontStretch: Byte = 0;  // normal | wider | narrower | ultra-condensed
                                 // | extra-condensed | condensed |
                                 // semi-condensed | semi-expanded | expanded |
                                 // extra-expanded | ultra-expanded
  cnstDefFontSize: Double = 12;  // 12pt
  cnstDefFontHeight: Double = 8;
  cnstDefAlingIndex = 5;
  cnstDefMeetIndex = 0;
  cnstDefViewBox: TsgViewBox = (Mode: [altxMidyMin, altMeet]; X: 0; Y: 0; Width: 0; Height: 0);

type

  TsgPropertiesMask = (pmDisplay, pmFillColor, pmFillRule, pmFillOpacity,
    pmTextAnchor, pmFontFamily, pmFontStyles, pmFontVariant, pmFontWeight,
    pmFontStretch, pmFontSize, pmStrokeColor, pmStrokeWidth, pmStrokeLineCap,
    pmStrokeLineJoin, pmVectorEffect, pmStrokeMiterlimit, pmStrokeDashArray,
    pmStrokeDashOffset, pmStrokeOpacity, pmVisibility, pmOpacity,
    pmTextDecoration);
  TsgPropertiesMasks = set of TsgPropertiesMask;

  TsvgPropertiesBase = class;//for internal use

  TsgSVGExtendedInfo = class(TObject)
  public
    Node: TsgNodeSample;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const ASource: TsgSVGExtendedInfo); virtual;
  end;

  TClassExtendedInfo = class of TsgSVGExtendedInfo;

  TsgSVGContainer = class(TsgDXFInsert)
  private
    FContainer: TsgSVGContainer;
    FFlags: Byte;
    FID: string;
    FProperties: TsvgPropertiesBase;
    FTMatrix: PFMatrix;
    FExtendedInfo: TsgSVGExtendedInfo;
    procedure AddAndLoads(const AEntity: TsgDXFEntity; const AColor: TColor;
      const AIndexToInsert: Integer = -1);
    function CanLoading(AConverter: TsgDXFConverter): Boolean;
    procedure CreateProperties;
    function GetBlockName: string;
    function GetDisplay: Byte;
    function GetEntVis: Byte;
    function GetFillOpacity: Single;
    function GetFillRule: Byte;
    function GetFontName: string;
    function GetFontSize: Single;
    function GetFontStretch: Byte;
    function GetFontStyles: TmvFontStyles;
    function GetFontVariant: Byte;
    function GetFontWeight: Single;
    function GetID: string;
    function GetIsLoaded: Boolean;
    function GetOpacity: Single;
    function GetStrokeDashArray: TsgDXFLineType;
    function GetStrokeDashOffset: Single;
    function GetStrokeLineCap: Byte;
    function GetStrokeLineJoin: Byte;
    function GetStrokeMiterlimit: Single;
    function GetStrokeOpacity: Single;
    function GetTextAnchor: Byte;
    function GetTextDecoration: Byte;
    function GetVectorEffect: Byte;
    function GetMatPropId: TsgNodeSample;
    procedure InsertBlockInConvert;
    procedure SetDisplay(const AValue: Byte);
    procedure SetEntVis(const AValue: Byte);
    procedure SetFillOpacity(const AValue: Single);
    procedure SetFillRule(const AValue: Byte);
    procedure SetFontName(const AValue: string);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStretch(const AValue: Byte);
    procedure SetFontStyles(const AValue: TmvFontStyles);
    procedure SetFontVariant(const AValue: Byte);
    procedure SetFontWeight(const AValue: Single);
    procedure SetID(const AValue: string);
    procedure SetIsLoaded(const AValue: Boolean);
    procedure SetOpacity(const AValue: Single);
    procedure SetStrokeDashArray(const AValue: TsgDXFLineType);
    procedure SetStrokeDashOffset(const AValue: Single);
    procedure SetStrokeLineCap(const AValue: Byte);
    procedure SetStrokeLineJoin(const AValue: Byte);
    procedure SetStrokeMiterlimit(const AValue: Single);
    procedure SetStrokeOpacity(const AValue: Single);
    procedure SetTextAnchor(const AValue: Byte);
    procedure SetTextDecoration(const AValue: Byte);
    procedure SetTransformMatrix(const AValue: TFMatrix);
    procedure SetVectorEffect(const AValue: Byte);
  protected
    procedure AddEnt(const AEntity: TsgDXFEntity);
    procedure AddEnts(AEntities: TsgObjectList);
    procedure AddFill(const AEntity: TsgDXFEntity); virtual;
    procedure AddFills(AEntities: TsgObjectList); virtual;
    function BeginExtData(const AData: TsgCADExtendedData; const AAppID: string): Boolean; override;
    procedure ClearBlock; virtual;
    procedure ClearReferences; override;
    procedure EndExtData(const AData: TsgCADExtendedData); override;
    procedure FreeReferences; override;
    procedure Generate; virtual;
    function GetActualBlockRecord: TsgDXFBlockRecord; override;
    function GetConverter: TsgDXFConverter; override;
    function GetEntsCount: Integer;
    function GetEntClassEx: TsgEntClass; override;
    function GetExtDataBody(const AData: TsgCADExtendedData): Boolean; virtual;
    function GetFillColor: TsgFillColor;
    function GetPropertiesType: Byte; virtual;
    function GetStrokeColor: TColor;
    function GetStrokeWidth: Single;
    function GetSVGType: TsvgEntity; virtual;
    function GetTransformMatrix: TFMatrix;
    function GetUsedMatrix: TFMatrix; virtual;
    procedure InitializeMatrix; override;
    function IsEntVisible: Boolean;
    procedure LoadedInternal(AConverter: TsgDXFConverter); override;
    procedure LoadedAsInsert(AConverter: TsgDXFConverter);
    procedure ReferenceBlockRecord(ABlockRecord: TsgDXFBlockRecord); override;
    procedure SetEntProps(const AEntity: TsgDXFEntity);
    procedure SetExtDataProperty(const ACode: SmallInt;
      const AData: TsgCADExtendedData; const AIndex: Integer); virtual;
    procedure SetFillColor(const AValue: TsgFillColor);
    procedure SetStrokeColor(const AValue: TColor);
    procedure SetStrokeWidth(const AValue: Single);

    function ToXMLNode(const ANode: TsgNode;
      const AParams: TsgXMLParams): Integer; override;
    function ToXMLGeoData(const ANode: TsgNode;
      const AParams: TsgXMLParams): Integer; override;
    function FromXMLNode(const AType: TsgXMLType;
      const ANode: TsgNodeSample; const AIsChild: Boolean;
      const AResult: IsgResultNode = nil): Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddEntity(const AEntity: TsgDXFEntity): Integer; override;
    procedure AssignEntity(ASource: TsgDXFEntity); override;
    procedure CopyProps(ASource: TsgSVGContainer);
    function GetExtData(const AData: TsgCADExtendedData; const AAppID: string): Boolean; override;
    function HasTransformMatrix: Boolean;

    function GetMatProp: string;
    procedure SetMatProp(const AStr: string);

    procedure SetExtData(const AData: TsgCADExtendedData); override;
    class function svgConvertARGBToColorCAD(const AColor: TColor): TsgColorCAD;
    property BlockName: string read GetBlockName;
    property Display: Byte read GetDisplay write SetDisplay;
    property EntVis: Byte read GetEntVis write SetEntVis;
    property FillColor: TsgFillColor read GetFillColor write SetFillColor;
    property FillOpacity: Single read GetFillOpacity write SetFillOpacity;
    property FillRule: Byte read GetFillRule write SetFillRule;
    property FontName: string read GetFontName write SetFontName;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStretch: Byte read GetFontStretch write SetFontStretch;
    property FontStyles: TmvFontStyles read GetFontStyles write SetFontStyles;
    property FontVariant: Byte read GetFontVariant write SetFontVariant;
    property FontWeight: Single read GetFontWeight write SetFontWeight;
    property ID: string read GetID write SetID;
    property IsLoaded: Boolean read GetIsLoaded write SetIsLoaded;
    property Opacity: Single read GetOpacity write SetOpacity;
    property Owner: TsgSVGContainer read FContainer write FContainer;
    property Properties: TsvgPropertiesBase read FProperties;
    property StrokeColor: TColor read GetStrokeColor write SetStrokeColor;
    property StrokeDashArray: TsgDXFLineType read GetStrokeDashArray
      write SetStrokeDashArray;
    property StrokeDashOffset: Single read GetStrokeDashOffset
      write SetStrokeDashOffset;
    property StrokeLineCap: Byte read GetStrokeLineCap
      write SetStrokeLineCap;
    property StrokeLineJoin: Byte read GetStrokeLineJoin
      write SetStrokeLineJoin;
    property StrokeMiterlimit: Single read GetStrokeMiterlimit
      write SetStrokeMiterlimit;
    property StrokeOpacity: Single read GetStrokeOpacity write SetStrokeOpacity;
    property StrokeWidth: Single read GetStrokeWidth write SetStrokeWidth;
    property SVGType: TsvgEntity read GetSVGType;
    property TextAnchor: Byte read GetTextAnchor write SetTextAnchor;
    property TextDecoration: Byte read GetTextDecoration
      write SetTextDecoration;
    property TransformMatrix: TFMatrix read GetTransformMatrix
      write SetTransformMatrix;
    property VectorEffect: Byte read GetVectorEffect write SetVectorEffect;
    property ExtendedInfo: TsgSVGExtendedInfo read FExtendedInfo write FExtendedInfo;
  end;

  TsvgPropertiesBase = class
  private
    FDisplay: Byte;
    FFontSize: Single;
    FMask: TsgPropertiesMasks;
    FOpacity: Single;
    FVectorEffect: Byte;
    FVisibility: Byte;
    procedure SetMask(const AValue: TsgPropertiesMasks);
  protected
    function GetDisplay: Byte; virtual;
    function GetFillColor: TsgFillColor; virtual;
    function GetFillOpacity: Single; virtual;
    function GetFillRule: Byte; virtual;
    function GetFontFamily: string; virtual;
    function GetFontSize: Single;
    function GetFontStretch: Byte; virtual;
    function GetFontStyles: TmvFontStyles; virtual;
    function GetFontVariant: Byte; virtual;
    function GetFontWeight: Single; virtual;
    function GetInternalMask: TsgPropertiesMasks; virtual;
    function GetOpacity: Single;
    function GetStrokeColor: TColor; virtual;
    function GetStrokeDashArray: Pointer; virtual;
    function GetStrokeDashOffset: Single; virtual;
    function GetStrokeLineCap: Byte; virtual;
    function GetStrokeLineJoin: Byte; virtual;
    function GetStrokeMiterlimit: Single; virtual;
    function GetStrokeOpacity: Single; virtual;
    function GetStrokeWidth: Double; virtual;
    function GetTextAnchor: Byte; virtual;
    function GetTextDecoration: Byte; virtual;
    function GetVectorEffect: Byte;
    function GetVisibility: Byte;
    procedure SetDisplay(const AValue: Byte); virtual;
    procedure SetFillColor(const AValue: TsgFillColor); virtual;
    procedure SetFillOpacity(const AValue: Single); virtual;
    procedure SetFillRule(const AValue: Byte); virtual;
    procedure SetFontFamily(const AValue: string); virtual;
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStretch(const AValue: Byte); virtual;
    procedure SetFontStyles(const AValue: TmvFontStyles); virtual;
    procedure SetFontVariant(const AValue: Byte); virtual;
    procedure SetFontWeight(const AValue: Single); virtual;
    procedure SetOpacity(const AValue: Single);
    procedure SetStrokeColor(const AValue: TColor); virtual;
    procedure SetStrokeDashArray(const AValue: Pointer); virtual;
    procedure SetStrokeDashOffset(const AValue: Single); virtual;
    procedure SetStrokeLineCap(const AValue: Byte); virtual;
    procedure SetStrokeLineJoin(const AValue: Byte); virtual;
    procedure SetStrokeMiterlimit(const AValue: Single); virtual;
    procedure SetStrokeOpacity(const AValue: Single); virtual;
    procedure SetStrokeWidth(const AValue: Double); virtual;
    procedure SetTextAnchor(const AValue: Byte); virtual;
    procedure SetTextDecoration(const AValue: Byte); virtual;
    procedure SetVectorEffect(const AValue: Byte);
    procedure SetVisibility(const AValue: Byte);
  public
    constructor Create; virtual;
    procedure Assign(const AProps: TsvgPropertiesBase); virtual;
    property Display: Byte read GetDisplay write SetDisplay;
    property FillColor: TsgFillColor read GetFillColor write SetFillColor;
    property FillOpacity: Single read GetFillOpacity write SetFillOpacity;
    property FillRule: Byte read GetFillRule write SetFillRule;
    property FontFamily: string read GetFontFamily write SetFontFamily;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStretch: Byte read GetFontStretch write SetFontStretch;
    property FontStyles: TmvFontStyles read GetFontStyles write SetFontStyles;
    property FontVariant: Byte read GetFontVariant write SetFontVariant;
    property FontWeight: Single read GetFontWeight write SetFontWeight;
    property Mask: TsgPropertiesMasks read FMask write SetMask;
    property Opacity: Single read GetOpacity write SetOpacity;
    property StrokeColor: TColor read GetStrokeColor write SetStrokeColor;
    property StrokeDashArray: Pointer read GetStrokeDashArray
      write SetStrokeDashArray;
    property StrokeDashOffset: Single read GetStrokeDashOffset
      write SetStrokeDashOffset;
    property StrokeLineCap: Byte read GetStrokeLineCap write SetStrokeLineCap;
    property StrokeLineJoin: Byte read GetStrokeLineJoin
      write SetStrokeLineJoin;
    property StrokeMiterlimit: Single read GetStrokeMiterlimit
      write SetStrokeMiterlimit;
    property StrokeOpacity: Single read GetStrokeOpacity write SetStrokeOpacity;
    property StrokeWidth: Double read GetStrokeWidth write SetStrokeWidth;
    property TextAnchor: Byte read GetTextAnchor write SetTextAnchor;
    property TextDecoration: Byte read GetTextDecoration
      write SetTextDecoration;
    property VectorEffect: Byte read GetVectorEffect write SetVectorEffect;
    property Visibility: Byte read GetVisibility write SetVisibility;
  end;

procedure AddCurvePolygonBoundary(const ACurve: TsgCADCurvePolygon;
  const AEnt: TsgDXFEntity; const AAdded: Boolean);
procedure AddPolyPolygonBoundary(const AFill: TsgCADPolyPolygon;
  const AEnt: TsgDXFEntity);
function GetColorWithOpacity(const AColor: TColor; AOpacity: Double): TColor;
function GetUniqBlockName(const AConv: TsgDXFConverter; AName: string;
  const AID: UInt64): string;

function SetStyleValue(const AId: string; const AValue: string; var AStyleStr: string): Boolean;

// change future versions
function IsSVGEntity(const AContainer: TsgSVGContainer): Boolean;

implementation

const
  cnstBlockNameBase = cnstSVGBlockNameBase;
  cnstED_ID = 472;

  cnstED_MatrixEX = 10;
  cnstED_MatrixEY = 11;
  cnstED_MatrixEZ = 12;
  cnstED_MatrixEO = 13;

  cnstED_Display = 60;
  cnstED_HasMatrix = 61;
  cnstED_FillHasStyle = 62;
  cnstED_FillRule = 63;
  cnstED_FontVariant = 64;
  cnstED_FontStretch = 65;
  cnstED_TextAnchor = 66;
  cnstED_TextDecoration = 67;
  cnstED_StrokeLineCap = 68;
  cnstED_StrokeLineJoin = 69;

  cnstED_EntVis = 71; //code = 70 use by external data

  cnstED_FillOpacity = 120;
  cnstED_FontWeight = 121;
  cnstED_FontSize = 122;
  cnstED_StrokeWidth = 123;
  cnstED_StrokeMiterlimit = 124;
  cnstED_StrokeOpacity = 125;
  cnstED_Opacity = 126;
  cnstED_StrokeDashOffset = 127;

  cnstED_FillColor = 440;
  cnstED_StrokeColor = 441;
  cnstED_FontStyles = 442;
  cnstED_FillStyle = 443;

  cnstED_FontName = 470;
  cnstED_StrokeDashArray = 471;

type
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);

function CreateBoundaryList(const ACurve: TsgCADCurvePolygon;
  const AType: Byte; const AAdded: Boolean): Tsg2DBoundaryList;
begin
  if AAdded and (ACurve.BoundaryDataCount > 0) then
    if ACurve.BoundaryDataLast.BoundaryType = AType then
      Result := ACurve.BoundaryDataLast
    else
      Result := ACurve.AddBoundaryList(AType)
  else
    Result := ACurve.AddBoundaryList(AType);
end;

procedure AddCurvePolygonBoundary(const ACurve: TsgCADCurvePolygon;
  const AEnt: TsgDXFEntity; const AAdded: Boolean);
var
  vArc: TsgDXFArc;
  vCircle: TsgDXFCircle;
  vEllipse: TsgDXFEllipse;
  vPoly: TsgDXFPolyline;
  vBoundary: Tsg2DBoundaryList;
  I: Integer;
  vVertex: TsgDXFVertex;
  vPoint2D: TF2DPoint;
  vArc2D: Tsg2DArc;
  vEllipse2D: Tsg2DEllipse;
  vSpline2D: Tsg2DSpline;
  vPoly2D: Tsg2DPolyline;
begin
  case AEnt.EntType of
    ceArc:
      begin
        vBoundary := CreateBoundaryList(ACurve, 1, AAdded);
        vArc2D := Tsg2DArc.Create;
        vBoundary.Add(vArc2D);
        vArc := TsgDXFArc(AEnt);
        vArc2D.CenterPoint := MakeF2DPointFrom3D(vArc.Point);
        vArc2D.Radius := vArc.Radius;
        vArc2D.StartParam := vArc.StartAngle;
        vArc2D.EndParam := vArc.EndAngle;
      end;
    ceCircle:
      begin
        vBoundary := CreateBoundaryList(ACurve, 1, AAdded);
        vArc2D := Tsg2DArc.Create;
        vBoundary.Add(vArc2D);
        vCircle := TsgDXFCircle(AEnt);
        vArc2D.CenterPoint := MakeF2DPointFrom3D(vCircle.Point);
        vArc2D.Radius := vCircle.Radius;
        vArc2D.StartParam := 0;
        vArc2D.EndParam := 360;
        vArc2D.CounterClockWise := True;
      end;
    ceEllipse:
      begin
        vBoundary := CreateBoundaryList(ACurve, 1, AAdded);
        vEllipse2D:= Tsg2DEllipse.Create;
        vBoundary.Add(vEllipse2D);
        vEllipse := TsgDXFEllipse(AEnt);
        vEllipse2D.CenterPoint := MakeF2DPointFrom3D(vEllipse.Point);
        vEllipse2D.MajorPoint := MakeF2DPointFrom3D(vEllipse.EndPoint);
        vEllipse2D.Radius := vEllipse.Ratio;
        vEllipse2D.StartParam := vEllipse.StartAngle;
        vEllipse2D.EndParam := vEllipse.EndAngle;
        vEllipse2D.IsAngleInParam := False;
      end;
    ceLWPolyline, cePolyline:
      begin
        vBoundary := CreateBoundaryList(ACurve, 7, AAdded);
        vPoly2D := Tsg2DPolyline.Create;
        vBoundary.Add(vPoly2D);
        vPoly := TsgDXFPolyline(AEnt);
        if (vPoly.PolyPoints <> nil) and (vPoly.PolyPoints.Count > 0) then
          for I := 0 to vPoly.PolyPoints.Count - 1 do
          begin
            vPoint2D := MakeF2DPointFrom3D(vPoly.PolyPoints.List[I]);
            if (I <= 0) or not IsEqualF2DPoints(vPoint2D, vPoly2D.EndPoint) then
              vPoly2D.AddVertex(vPoint2D);
          end
        else
          for I := 0 to vPoly.Count - 1 do
          begin
            vVertex := TsgDXFVertex(vPoly.Entities[I]);
            vPoint2D := MakeF2DPointFrom3D(vVertex.Point);
            if (I <= 0) or not IsEqualF2DPoints(vPoint2D, vPoly2D.EndPoint) then
              vPoly2D.AddVertex(vPoint2D);
          end;
        vPoly2D.Closed := not IsEqualF2DPoints(vPoly2D.StartPoint,
          vPoly2D.EndPoint);
      end;
    ceSpline:
      begin
        vBoundary := CreateBoundaryList(ACurve, 1, AAdded);
        vSpline2D := Tsg2DSpline.Create;
        vBoundary.Add(vSpline2D);
        for I := 0 to TsgDXFSpline(AEnt).Controls.Count - 1 do
        begin
          vSpline2D.AddControl(
            MakeF2DPointFrom3D(TsgDXFSpline(AEnt).Controls[I]));
        end;
        for I := 0 to TsgDXFSpline(AEnt).Knots.Count - 1 do
          vSpline2D.AddKnot(TsgDXFSpline(AEnt).Knots[I]);
      end;
  end;
end;

procedure AddPolyPolygonBoundary(const AFill: TsgCADPolyPolygon;
  const AEnt: TsgDXFEntity);
var
  I: Integer;
  vPoints: TF2DPointList;
  vPPoint2D: PF2DPoint;
  vTmpPoint: TF2DPoint;
  vPoint: TFPoint;
begin
  if AEnt is TsgCADBasePolyline then
  begin
    vPPoint2D := nil;
    vPoints := TF2DPointList.Create;
    AFill.Boundaries.Add(vPoints);
    vPoints.Capacity := TsgCADBasePolyline(AEnt).PolyPoints.Count;
    for I := 0 to TsgCADBasePolyline(AEnt).PolyPoints.Count - 1 do
    begin
      vPoint := TsgCADBasePolyline(AEnt).PolyPoints[I];
      vTmpPoint := MakeF2DPoint(vPoint.X, vPoint.Y);
      if not Assigned(vPPoint2D)
         or not IsEqualF2DPoints(vTmpPoint, vPPoint2D^) then
      begin
        vPoints.Add(vTmpPoint);
        vPPoint2D := @vPoints.List^[vPoints.Count - 1];
      end;
    end;
    if (vPoints.Count > 0) and
       (not IsEqualF2DPoints(vPoints.First, vPoints.Last)) then
      vPoints.Add(vPoints.First);
  end;
end;

function FontStylesToInt(const AFontStyles: TmvFontStyles): Integer;
var
  I: TmvFontStyle;
begin
  Result := 0;
  for I := Low(TmvFontStyle) to High(TmvFontStyle) do
    if I in AFontStyles then
      Result := Result or (1 shl Ord(I));
end;

{ GetColorWithOpacity

  values outside the range 0.0 (fully transparent) to 1.0 (fully opaque) }
function GetColorWithOpacity(const AColor: TColor; AOpacity: Double): TColor;
begin
  Result := AColor;
  if AOpacity <= 0 then
    PsgColor(@Result)^.A := 255
  else
    if AOpacity >= 1 then
      PsgColor(@Result)^.A := 0
    else
      PsgColor(@Result)^.A := Floor(255 * (1 - AOpacity));
end;

function GetUniqBlockName(const AConv: TsgDXFConverter; AName: string;
  const AID: UInt64): string;
var
  vID: UInt64;
begin
  vID := AID;
  repeat
    Result := AName + '-' + IntToHex(vID, 0);
    Inc(vID);
    if vID = AID then
      AName := AName + '-';
  until AConv.BlockByName(Result) = nil;
end;

function IntToFontStyles(const AValue: Integer): TmvFontStyles;
var
  I: TmvFontStyle;
begin
  Result := [];
  for I := Low(TmvFontStyle) to High(TmvFontStyle) do
    if AValue and (1 shl Ord(I)) <> 0  then
      Include(Result, I);
end;

function SetStyleValue(const AId: string; const AValue: string;
  var AStyleStr: string): Boolean;
var
  vParams: TsgStringList;
  vValue, vSubString, vAttribName, vId: string;
  I, J, vLength: Integer;
begin
  Result := False;
  vValue := Trim(AStyleStr);
  vLength := Length(vValue);
  if (vLength > 0) and (vValue[1] <> '&') then
  begin
    vId := AnsiLowerCase(AId);
    if vValue[vLength] <> cnstLineBreakStyle then
      vValue := vValue + cnstLineBreakStyle;
    vParams := TsgStringList.Create;
    try
      SetStringsLineBreak(vParams, vValue, cnstLineBreakStyle);
      SetStringsText(vParams, vValue);
      I := 0;
      while I < vParams.Count do
      begin
        vSubString := vParams[I];
        vLength := Length(vSubString);
        if vLength > 0 then
        begin
          J := StringScan(':', vSubString, 1);
          if J > 0 then
          begin
            vAttribName := AnsiLowerCase(Trim(Copy(vSubString, 1, J - 1)));
            if vID = vAttribName then
            begin
              Delete(vSubString, J + 1, vLength - J);
              Insert(AValue, vSubString, J + 1);
              vParams[I] := vSubString;
              Result := True;
              Break;
            end;
          end;
          Inc(I);
        end
        else
          vParams.Delete(I);
      end;
      if Result then
        AStyleStr := vParams.Text
      else
      begin
        AStyleStr := AStyleStr + cnstLineBreakStyle + vId + ':' + AValue;
        Result := True;
      end;
    finally
      vParams.Free;
    end;
  end;
end;

function IsSVGEntity(const AContainer: TsgSVGContainer): Boolean;
begin
  Result := not (AContainer.SVGType in [svgContainer, svgGroup, svgGroupDim
   {, svgLink}]);
end;

type
  TsvgPropertiesPenBrush = class(TsvgPropertiesBase)
  private
    FFillColor: TsgFillColor;
    FFillOpacity: Single;
    FStrokeColor: TColor;
    FStrokeDashArray: Pointer;
    FStrokeDashOffset: Single;
    FStrokeLineCap: Byte;
    FStrokeLineJoin: Byte;
    FStrokeWidth: Double;
  protected
    function GetFillColor: TsgFillColor; override;
    function GetFillOpacity: Single; override;
    function GetInternalMask: TsgPropertiesMasks; override;
    function GetStrokeColor: TColor; override;
    function GetStrokeDashArray: Pointer; override;
    function GetStrokeDashOffset: Single; override;
    function GetStrokeLineCap: Byte; override;
    function GetStrokeLineJoin: Byte; override;
    function GetStrokeWidth: Double; override;
    procedure SetFillColor(const AValue: TsgFillColor); override;
    procedure SetFillOpacity(const AValue: Single); override;
    procedure SetStrokeColor(const AValue: TColor); override;
    procedure SetStrokeDashArray(const AValue: Pointer); override;
    procedure SetStrokeDashOffset(const AValue: Single); override;
    procedure SetStrokeLineCap(const AValue: Byte); override;
    procedure SetStrokeLineJoin(const AValue: Byte); override;
    procedure SetStrokeWidth(const AValue: Double); override;
  public
    constructor Create; override;
    procedure Assign(const AProps: TsvgPropertiesBase); override;
  end;

  TsvgProperties = class(TsvgPropertiesPenBrush)
  private
    FFontFamily: string;
    FFontStretch: Byte;
    FFontStyles: TmvFontStyles;
    FFontVariant: Byte;
    FFontWeight: Single;
    FTextAnchor: Byte;
    FTextDecoration: Byte;
  protected
    function GetFontFamily: string; override;
    function GetFontStretch: Byte; override;
    function GetFontStyles: TmvFontStyles; override;
    function GetFontVariant: Byte; override;
    function GetFontWeight: Single; override;
    function GetInternalMask: TsgPropertiesMasks; override;
    function GetTextAnchor: Byte; override;
    function GetTextDecoration: Byte; override;
    procedure SetFontFamily(const AValue: string); override;
    procedure SetFontStretch(const AValue: Byte); override;
    procedure SetFontStyles(const AValue: TmvFontStyles); override;
    procedure SetFontVariant(const AValue: Byte); override;
    procedure SetFontWeight(const AValue: Single); override;
    procedure SetTextAnchor(const AValue: Byte); override;
    procedure SetTextDecoration(const AValue: Byte); override;
  public
    constructor Create; override;
    procedure Assign(const AProps: TsvgPropertiesBase); override;
  end;

  TsvgPropertiesFont = class(TsvgPropertiesBase)
  private
    FFillColor: TsgFillColor;
    FFontFamily: string;
    FFontStretch: Byte;
    FFontStyles: TmvFontStyles;
    FFontVariant: Byte;
    FFontWeight: Single;
    FStrokeColor: TColor;
    FStrokeWidth: Double;
    FTextAnchor: Byte;
    FTextDecoration: Byte;
  protected
    function GetFillColor: TsgFillColor; override;
    function GetFontFamily: string; override;
    function GetFontStretch: Byte; override;
    function GetFontStyles: TmvFontStyles; override;
    function GetFontVariant: Byte; override;
    function GetFontWeight: Single; override;
    function GetInternalMask: TsgPropertiesMasks; override;
    function GetStrokeColor: TColor; override;
    function GetStrokeWidth: Double; override;
    function GetTextAnchor: Byte; override;
    function GetTextDecoration: Byte; override;
    procedure SetFillColor(const AValue: TsgFillColor); override;
    procedure SetFontFamily(const AValue: string); override;
    procedure SetFontStretch(const AValue: Byte); override;
    procedure SetFontStyles(const AValue: TmvFontStyles); override;
    procedure SetFontVariant(const AValue: Byte); override;
    procedure SetFontWeight(const AValue: Single); override;
    procedure SetStrokeColor(const AValue: TColor); override;
    procedure SetStrokeWidth(const AValue: Double); override;
    procedure SetTextAnchor(const AValue: Byte); override;
    procedure SetTextDecoration(const AValue: Byte); override;
  public
    constructor Create; override;
    procedure Assign(const AProps: TsvgPropertiesBase); override;
  end;


{  TsgSVGContainer  }

procedure TsgSVGContainer.AddAndLoads(const AEntity: TsgDXFEntity;
  const AColor: TColor; const AIndexToInsert: Integer);
begin
  if Assigned(AEntity) then
  begin
    AEntity.Layer := Layer;
    if AColor <> AEntity.Color then
      AEntity.ColorCAD := svgConvertARGBToColorCAD(AColor);
    Converter.Loads(AEntity);
    if AIndexToInsert = -1 then
      Block.AddEntity(AEntity)
    else
      Block.InsertEntity(AIndexToInsert, AEntity);
  end;
end;

function TsgSVGContainer.CanLoading(AConverter: TsgDXFConverter): Boolean;
begin
  Result := (Block <> nil) and (AConverter <> nil)
    and (TsgDXFConverterAccess(AConverter).Status <> stClearing);
    // and (TsgDXFConverterAccess(AConverter).Status = stDefault);
end;

procedure TsgSVGContainer.CreateProperties;
begin
  case GetPropertiesType of
    1:
      FProperties := TsvgPropertiesPenBrush.Create;
    2:
      FProperties := TsvgPropertiesFont.Create;
    3:
      FProperties := TsvgProperties.Create;
  else
    FProperties := TsvgProperties.Create;
  end;
end;

procedure TsgSVGContainer.FreeReferences;
begin
  inherited FreeReferences;
  FreeAndNil(FProperties);
  DisposeAndNil(FTMatrix);
end;

function TsgSVGContainer.GetActualBlockRecord: TsgDXFBlockRecord;
begin
  Result := BlockRecord;
end;

function TsgSVGContainer.GetBlockName: string;
begin
  Result := '';
  if Assigned(BlockRecord) then
    Result := BlockRecord.Name;
end;

function TsgSVGContainer.GetConverter: TsgDXFConverter;
begin
  Result := inherited GetConverter;
  if not Assigned(Result) and Assigned(Block) then
    Result := Block.Converter;
end;

function TsgSVGContainer.GetDisplay: Byte;
begin
  if (FContainer = nil) or (pmDisplay in FProperties.Mask)  then
    Result := FProperties.Display
  else
    Result := FContainer.Display;
end;

function TsgSVGContainer.GetEntVis: Byte;
begin
  if (FContainer = nil) or (pmVisibility in FProperties.Mask)  then
    Result := FProperties.Visibility
  else
    Result := FContainer.EntVis;
end;

function TsgSVGContainer.GetFillOpacity: Single;
begin
  if (FContainer = nil) or (pmFillOpacity in FProperties.Mask)  then
    Result := FProperties.FillOpacity
  else
    Result := FContainer.FillOpacity;
end;

function TsgSVGContainer.GetFillRule: Byte;
begin
  if (FContainer = nil) or (pmFillRule in FProperties.Mask)  then
    Result := FProperties.FillRule
  else
    Result := FContainer.FillRule;
end;

function TsgSVGContainer.GetFontName: string;
begin
  if (FContainer = nil) or (pmFontFamily in FProperties.Mask)  then
    Result := FProperties.FontFamily
  else
    Result := FContainer.FontName;
end;

function TsgSVGContainer.GetFontSize: Single;
begin
  if (FContainer = nil) or (pmFontSize in FProperties.Mask)  then
    Result := FProperties.FontSize
  else
    Result := FContainer.FontSize;
end;

function TsgSVGContainer.GetFontStretch: Byte;
begin
  if (FContainer = nil) or (pmFontStretch in FProperties.Mask)  then
    Result := FProperties.FontStretch
  else
    Result := FContainer.FontStretch;
end;

function TsgSVGContainer.GetFontStyles: TmvFontStyles;
begin
  if (FContainer = nil) or (pmFontStyles in FProperties.Mask)  then
    Result := FProperties.FontStyles
  else
    Result := FContainer.FontStyles;
end;

function TsgSVGContainer.GetFontVariant: Byte;
begin
  if (FContainer = nil) or (pmFontVariant in FProperties.Mask)  then
    Result := FProperties.FontVariant
  else
    Result := FContainer.FontVariant;
end;

function TsgSVGContainer.GetFontWeight: Single;
begin
  if (FContainer = nil) or (pmFontWeight in FProperties.Mask)  then
    Result := FProperties.FontWeight
  else
    Result := FContainer.FontWeight;
end;

function TsgSVGContainer.GetID: string;
begin
  Result := FID;
end;

function TsgSVGContainer.GetIsLoaded: Boolean;
begin
  Result := FFlags and 1 <> 0;
end;

function TsgSVGContainer.GetOpacity: Single;
begin
  if (FContainer = nil) or (pmOpacity in FProperties.Mask)  then
    Result := FProperties.Opacity
  else
    Result := FContainer.Opacity;
end;

function TsgSVGContainer.GetStrokeDashArray: TsgDXFLineType;
begin
  if (FContainer = nil) or (pmStrokeDashArray in FProperties.Mask)  then
    Result := FProperties.StrokeDashArray
  else
    Result := FContainer.StrokeDashArray;
end;

function TsgSVGContainer.GetStrokeDashOffset: Single;
begin
  if (FContainer = nil) or (pmStrokeDashOffset in FProperties.Mask)  then
    Result := FProperties.StrokeDashOffset
  else
    Result := FContainer.StrokeDashOffset;
end;

function TsgSVGContainer.GetStrokeLineCap: Byte;
begin
  if (FContainer = nil) or (pmStrokeLineCap in FProperties.Mask)  then
    Result := FProperties.StrokeLineCap
  else
    Result := FContainer.StrokeLineCap;
end;

function TsgSVGContainer.GetStrokeLineJoin: Byte;
begin
  if (FContainer = nil) or (pmStrokeLineJoin in FProperties.Mask)  then
    Result := FProperties.StrokeLineJoin
  else
    Result := FContainer.StrokeLineJoin;
end;

function TsgSVGContainer.GetStrokeMiterlimit: Single;
begin
  if (FContainer = nil) or (pmStrokeMiterlimit in FProperties.Mask)  then
    Result := FProperties.StrokeMiterlimit
  else
    Result := FContainer.StrokeMiterlimit;
end;

function TsgSVGContainer.GetStrokeOpacity: Single;
begin
  if (FContainer = nil) or (pmStrokeOpacity in FProperties.Mask)  then
    Result := FProperties.StrokeOpacity
  else
    Result := FContainer.StrokeOpacity;
end;

function TsgSVGContainer.GetTextAnchor: Byte;
begin
  if (FContainer = nil) or (pmTextAnchor in FProperties.Mask)  then
    Result := FProperties.TextAnchor
  else
    Result := FContainer.TextAnchor;
end;

function TsgSVGContainer.GetTextDecoration: Byte;
begin
  if (FContainer = nil) or (pmTextDecoration in FProperties.Mask)  then
    Result := FProperties.TextDecoration
  else
    Result := FContainer.TextDecoration;
end;

function TsgSVGContainer.GetVectorEffect: Byte;
begin
  if (FContainer = nil) or (pmVectorEffect in FProperties.Mask)  then
    Result := FProperties.GetVectorEffect
  else
    Result := FContainer.VectorEffect;
end;

procedure TsgSVGContainer.InsertBlockInConvert;
begin
  if (Block <> nil) and (Length(Block.Name) = 0) then
  begin
    Block.Name := GetUniqBlockName(Converter, cnstBlockNameBase, Integer(Block));
    TsgDXFBlockAccess(Block).SetFlags(1);
    Converter.Loads(Block);
    Converter.Sections[csBlocks].AddEntity(Block);
    Block.IsLoaded := False;
  end;
end;

procedure TsgSVGContainer.SetDisplay(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmDisplay];
  FProperties.Display := AValue;
end;

procedure TsgSVGContainer.SetEntVis(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmVisibility];
  FProperties.Visibility := AValue;
end;

procedure TsgSVGContainer.SetFillOpacity(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmFillOpacity];
  FProperties.FillOpacity := AValue;
end;

procedure TsgSVGContainer.SetFillRule(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmFillRule];
  FProperties.FillRule := AValue;
end;

procedure TsgSVGContainer.SetFontName(const AValue: string);
begin
  FProperties.Mask := FProperties.Mask + [pmFontFamily];
  FProperties.FontFamily := Trim(AValue);
end;

procedure TsgSVGContainer.SetFontSize(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmFontSize];
  FProperties.FontSize := AValue;
end;

procedure TsgSVGContainer.SetFontStretch(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmFontStretch];
  FProperties.FontStretch := AValue;
end;

procedure TsgSVGContainer.SetFontStyles(const AValue: TmvFontStyles);
begin
  FProperties.Mask := FProperties.Mask + [pmFontStyles];
  FProperties.FontStyles := AValue;
end;

procedure TsgSVGContainer.SetFontVariant(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmFontVariant];
  FProperties.FontVariant := AValue;
end;

procedure TsgSVGContainer.SetFontWeight(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmFontWeight];
  FProperties.FontWeight := AValue;
end;

procedure TsgSVGContainer.SetID(const AValue: string);
begin
  FID := AValue;
end;

procedure TsgSVGContainer.SetIsLoaded(const AValue: Boolean);
begin
  FFlags := (FFlags and 254) or Byte(AValue);
end;

procedure TsgSVGContainer.SetOpacity(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmOpacity];
  FProperties.Opacity := AValue;
end;

procedure TsgSVGContainer.SetStrokeDashArray(const AValue: TsgDXFLineType);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeDashArray];
  FProperties.StrokeDashArray := AValue;
end;

procedure TsgSVGContainer.SetStrokeDashOffset(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeDashOffset];
  FProperties.StrokeDashOffset := AValue;
end;

procedure TsgSVGContainer.SetStrokeLineCap(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeLineCap];
  FProperties.StrokeLineCap := AValue;
end;

procedure TsgSVGContainer.SetStrokeLineJoin(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeLineJoin];
  FProperties.StrokeLineJoin := AValue;
end;

procedure TsgSVGContainer.SetStrokeMiterlimit(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeMiterlimit];
  FProperties.StrokeMiterlimit := AValue;
end;

procedure TsgSVGContainer.SetStrokeOpacity(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeOpacity];
  FProperties.StrokeOpacity := AValue;
end;

procedure TsgSVGContainer.SetTextAnchor(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmTextAnchor];
  FProperties.TextAnchor := AValue;
end;

procedure TsgSVGContainer.SetTextDecoration(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmTextDecoration];
  FProperties.TextDecoration := AValue;
end;

procedure TsgSVGContainer.SetTransformMatrix(const AValue: TFMatrix);
begin
  if CompareMem(@AValue, @cnstIdentityMat, Sizeof(AValue)) then
  begin
    if FTMatrix <> nil then
    begin
      Dispose(FTMatrix);
      FTMatrix := nil;
    end;
  end
  else
  begin
    if FTMatrix = nil then
      New(FTMatrix);
    FTMatrix^ := AValue;
  end;
end;

procedure TsgSVGContainer.SetVectorEffect(const AValue: Byte);
begin
  FProperties.Mask := FProperties.Mask + [pmVectorEffect];
  FProperties.VectorEffect := AValue;
end;

procedure TsgSVGContainer.AddEnt(const AEntity: TsgDXFEntity);
begin
  SetEntProps(AEntity);
  Converter.Loads(AEntity);
  AddFill(AEntity);
  if (StrokeColor <> clNone) and (StrokeWidth <> 0) then
    Block.AddEntity(AEntity)
  else
    AEntity.Free;
end;

procedure TsgSVGContainer.AddEnts(AEntities: TsgObjectList);
var
  I: Integer;
  vEntity: TsgDXFEntity;
begin
  for I := 0 to AEntities.Count - 1 do
  begin
    vEntity := TsgDXFEntity(AEntities[I]);
    SetEntProps(vEntity);
    Converter.Loads(vEntity);
  end;
  AddFills(AEntities);
  if StrokeColor <> clNone then
    for I := 0 to AEntities.Count - 1 do
      Block.AddEntity(TsgDXFEntity(AEntities.List[I]))
  else
    TsgObjectList.ClearList(AEntities);
end;

procedure TsgSVGContainer.AddFill(const AEntity: TsgDXFEntity);
var
  vFill: TsgCADPolyPolygon;
  vCADfill: TsgCADFill;
  vFillColor: TsgFillColor;
begin
  if IsEntVisible and (Opacity <> 0) then
  begin
    vFillColor := FillColor;
    if vFillColor.IsStyle then
    begin
      vCADfill := TsgCADFill.Create;
      vCADfill.StyleFill := vFillColor.Style;
      AddCurvePolygonBoundary(vCADfill, AEntity, False);
      if vCADfill.BoundaryDataCount > 0 then
        AddAndLoads(vCADfill, vCADfill.Color)
      else
        vCADfill.Free;
    end else
      if vFillColor.Color <> clNone then
        if not (AEntity.EntType in [cePoint, ceLine]) then
        begin
          vFill := TsgCADPolyPolygon.Create;
          AddPolyPolygonBoundary(vFill, AEntity);
          if vFill.Boundaries.Count > 0 then
            AddAndLoads(vFill, GetColorWithOpacity(vFillColor.Color,
              FillOpacity * Opacity))
          else
            vFill.Free;
        end else
          if StrokeColor = clByBlock then
            AEntity.ColorCAD := svgConvertARGBToColorCAD(vFillColor.Color);
  end;
end;

procedure TsgSVGContainer.AddFills(AEntities: TsgObjectList);
var
  I: Integer;
  vFill: TsgCADPolyPolygon;
  vCADFill: TsgCADFill;
  vFillColor: TsgFillColor;
begin
  if IsEntVisible and (Opacity <> 0) then
  begin
    vFillColor := FillColor;
    if vFillColor.IsStyle then
    begin
      vCADFill := TsgCADFill.Create;
      vCADFill.StyleFill := vFillColor.Style;
      for I := 0 to AEntities.Count - 1 do
        AddCurvePolygonBoundary(vCADFill, TsgDXFEntity(AEntities[I]), False);
      if vCADFill.BoundaryDataCount > 0 then
        AddAndLoads(vCADFill, vCADFill.Color, 0)
      else
        vCADFill.Free;
    end
    else
      if vFillColor.Color <> clNone then
      begin
        vFill := TsgCADPolyPolygon.Create;
        for I := 0 to AEntities.Count - 1 do
          AddPolyPolygonBoundary(vFill, TsgDXFEntity(AEntities[I]));
        if vFill.Boundaries.Count > 0 then
          AddAndLoads(vFill, GetColorWithOpacity(vFillColor.Color,
            FillOpacity * Opacity), 0)
        else
          vFill.Free;
      end;
  end;
end;

procedure TsgSVGContainer.ClearBlock;
begin
end;

procedure TsgSVGContainer.ClearReferences;
var
  vBlock: TsgDXFBlockAccess;
  vConv: TsgDXFConverterAccess;
begin
  vConv := TsgDXFConverterAccess(Converter);
  vBlock := TsgDXFBlockAccess(Block);
  if Assigned(vConv) and not (vConv.Status in [stClearing, stDestroying]) and
     Assigned(vBlock) then
    vConv.DeleteBlock(vBlock, False); // delete block from block`s section
  BlockRecord := nil; // delete self from TsgDXFBlock.References list
  if Assigned(vBlock) and (TsgOwneredItem(vBlock.GetNamedItem).RefCount = 0) then // if no any refs
    vBlock.Free;
  inherited ClearReferences;
  FContainer := nil;
end;

procedure TsgSVGContainer.Generate;
begin
end;

function TsgSVGContainer.GetEntClassEx: TsgEntClass;
begin
  Result.EG := gtSVG;
  Result.ET.SVG := GetSVGType;
end;

function TsgSVGContainer.GetEntsCount: Integer;
begin
  Result := 0;
  if Assigned(Block) then
    Result := Block.Count;
end;

function TsgSVGContainer.GetExtData(const AData: TsgCADExtendedData; const AAppID: string): Boolean;
begin
  if BeginExtData(AData, AAppID) then
  begin
    GetExtDataBody(AData);
    EndExtData(AData);
    Result := True;
  end
  else
    Result := inherited GetExtData(AData, AAppID);
end;

function TsgSVGContainer.GetExtDataBody(
  const AData: TsgCADExtendedData): Boolean;
var
  vTM: TFMatrix;
  vFillColor: TsgFillColor;
begin
  vFillColor := FillColor;
  if Length(FID) > 0 then
    AData.AddComplexString(cnstED_ID, FID);
  AData.AddComplexInt16(cnstED_HasMatrix, Byte(HasTransformMatrix));
  if HasTransformMatrix then
  begin
    vTM := TransformMatrix;
    AData.AddComplexPoint(cnstED_MatrixEX, vTM.EX);
    AData.AddComplexPoint(cnstED_MatrixEY, vTM.EY);
    AData.AddComplexPoint(cnstED_MatrixEZ, vTM.EZ);
    AData.AddComplexPoint(cnstED_MatrixEO, vTM.E0);
  end;
  AData.AddComplexInt16(cnstED_FillHasStyle, Byte(vFillColor.IsStyle));
  if vFillColor.IsStyle then
  begin
    //AData.AddComplexInt32(cnstED_FillStyle, Integer(vFillColor.Style));
  end
  else
    AData.AddComplexInt32(cnstED_FillColor, vFillColor.Color);
  AData.AddComplexInt16(cnstED_FillRule, FillRule);
  AData.AddComplexInt16(cnstED_FontVariant, FontVariant);
  AData.AddComplexInt16(cnstED_FontStretch, FontStretch);
  AData.AddComplexInt16(cnstED_TextAnchor, TextAnchor);
  AData.AddComplexInt16(cnstED_TextDecoration, TextDecoration);
  AData.AddComplexInt16(cnstED_StrokeLineCap, StrokeLineCap);
  AData.AddComplexInt16(cnstED_StrokeLineJoin, StrokeLineJoin);
  AData.AddComplexInt16(cnstED_Display, Display);
  AData.AddComplexInt16(cnstED_EntVis, EntVis);
  AData.AddComplexInt32(cnstED_StrokeColor, StrokeColor);
  AData.AddComplexInt32(cnstED_FontStyles, FontStylesToInt(FontStyles));
  AData.AddComplexDouble(cnstED_FillOpacity, FillOpacity);
  AData.AddComplexDouble(cnstED_FontWeight, FontWeight);
  AData.AddComplexDouble(cnstED_FontSize, FontSize);
  AData.AddComplexDouble(cnstED_StrokeWidth, StrokeWidth);
  AData.AddComplexDouble(cnstED_StrokeMiterlimit, StrokeMiterlimit);
  AData.AddComplexDouble(cnstED_StrokeOpacity, StrokeOpacity);
  AData.AddComplexDouble(cnstED_Opacity, Opacity);
  AData.AddComplexDouble(cnstED_StrokeDashOffset, StrokeDashOffset);
  AData.AddComplexString(cnstED_FontName, FontName);
  if StrokeDashArray <> nil then
    AData.AddComplexString(cnstED_StrokeDashArray, StrokeDashArray.EntName);
  Result := AData.DataCount > 0;
end;

function TsgSVGContainer.GetFillColor: TsgFillColor;
begin
  if (FContainer = nil) or (pmFillColor in FProperties.Mask)  then
    Result := FProperties.FillColor
  else
    Result := FContainer.FillColor;
end;

function TsgSVGContainer.GetPropertiesType: Byte;
begin
  Result := 3;
end;

function TsgSVGContainer.GetStrokeColor: TColor;
begin
  if (FContainer = nil) or (pmStrokeColor in FProperties.Mask)  then
    Result := FProperties.StrokeColor
  else
    Result := FContainer.StrokeColor;
end;

function TsgSVGContainer.GetStrokeWidth: Single;
begin
  if (FContainer = nil) or (pmStrokeWidth in FProperties.Mask)  then
    Result := FProperties.StrokeWidth
  else
    Result := FContainer.StrokeWidth;
end;

function TsgSVGContainer.GetSVGType: TsvgEntity;
begin
  Result := svgContainer;
end;

function TsgSVGContainer.GetTransformMatrix: TFMatrix;
begin
  if FTMatrix = nil then
    Result := cnstIdentityMat
  else
    Result := FTMatrix^;
end;

function TsgSVGContainer.GetUsedMatrix: TFMatrix;
begin
  if FTMatrix <> nil then
    Result := FTMatrix^
  else
    Result := cnstIdentityMat;
end;

procedure TsgSVGContainer.InitializeMatrix;
begin
  FMatrix := GetUsedMatrix;
end;

function TsgSVGContainer.IsEntVisible: Boolean;
begin
  Result := (EntVis = 0 ) and (Display <> 16);
end;

procedure TsgSVGContainer.LoadedInternal(AConverter: TsgDXFConverter);
begin
  SetConverter(AConverter);
  FMatrix := cnstIdentityMat;
  Point := cnstFPointZero;
  Scale := cnstFPointSingle;
  Angle := 0;
  if CanLoading(Converter) then
  begin
    InsertBlockInConvert;
    ClearBlock;
    Generate;
    Converter.Loads(Block);
  end;
  LoadedAsInsert(AConverter);
end;

procedure CreateCopyEntitiesList(const P: Pointer; const ABase, ACopy: TsgDXFEntity;
  const AConverter: TsgDXFConverter);
var
  I: Integer;
  vEntityBase, vEntityCopy: TsgDXFEntity;
begin
  for I := 0 to ABase.Count - 1 do
  begin
    vEntityBase := ABase[I];
    vEntityCopy := TsgDXFEntityClass(vEntityBase.ClassType).Create;
    ACopy.AddEntity(vEntityCopy);
    vEntityCopy.AssignEntity(vEntityBase);
    if vEntityCopy.EntClass.EG = gtSVG then
      TsgSVGContainer(vEntityCopy).Owner := P;
    AConverter.Loads(vEntityCopy);
  end;
end;

procedure TsgSVGContainer.ReferenceBlockRecord(ABlockRecord: TsgDXFBlockRecord);
var
  vSourceBlock: TsgDXFBlock;
begin
  // do nothing, SVGContainer must use only self created block
  if Assigned(Block) then
  begin
    vSourceBlock := nil;
    if Assigned(ABlockRecord) then
      vSourceBlock := TsgDXFBlockRecordAccess(ABlockRecord).Block;
    if vSourceBlock <> Block then
    begin
      if vSourceBlock.Count > 0 then
        Block.Clear(True);
      if Converter <> nil then
      begin
        if Assigned(vSourceBlock) then
          CreateCopyEntitiesList(Self, vSourceBlock, Block, Converter);
        Converter.Loads(Block);
      end;
    end;
  end;
end;

procedure TsgSVGContainer.LoadedAsInsert(AConverter: TsgDXFConverter);
begin
  inherited LoadedInternal(AConverter);
end;

class function TsgSVGContainer.svgConvertARGBToColorCAD(const AColor: TColor): TsgColorCAD;
begin
  Result := sgFunction.ConvertARGBToColorCAD(AColor);
  if bAlternateBlack then
  begin
    if (Result.Active = acRGBColor) and (Result.Color = 0) then//is black
      Result := cnstColorCADByBlackWhite;
  end;
end;

procedure TsgSVGContainer.SetEntProps(const AEntity: TsgDXFEntity);
begin
  AEntity.ColorCAD := svgConvertARGBToColorCAD(StrokeColor);
  AEntity.Layer := Layer;
  AEntity.LineWeight := StrokeWidth;
  AEntity.LineType := StrokeDashArray;
  AEntity.LineTypeScale := LineTypeScale;
  AEntity.Visibility := IsEntVisible and (Opacity > 0);
  if AEntity.Visibility and (not (AEntity is TsgDXFText)) then
    AEntity.Visibility := StrokeWidth <> 0;
end;

procedure TsgSVGContainer.SetExtData(const AData: TsgCADExtendedData);
var
  I: Integer;
  vTM: TFMatrix;
  vFillColor: TsgFillColor;
  vHasTM: Boolean;
begin
  ID := '';
  vTM := cnstIdentityMat;
  vFillColor := cnstDefFillColor;
  vHasTM := False;
  AData.UseCode := False;
  for I := 0 to AData.DataCount - 1 do
    case AData.DataCode[I] of
      cnstED_ID:               ID := AData.DataString[I];

      cnstED_HasMatrix:        vHasTM := AData.DataInt16[I] = 1;
      cnstED_MatrixEX:         vTM.EX := AData.DataPoint[I];
      cnstED_MatrixEY:         vTM.EY := AData.DataPoint[I];
      cnstED_MatrixEZ:         vTM.EZ := AData.DataPoint[I];
      cnstED_MatrixEO:         vTM.E0 := AData.DataPoint[I];

      cnstED_FillHasStyle:     vFillColor.IsStyle := AData.DataInt16[I] = 1;
      cnstED_FillRule:         FillRule := AData.DataInt16[I];
      cnstED_FontVariant:      FontVariant := AData.DataInt16[I];
      cnstED_FontStretch:      FontStretch := AData.DataInt16[I];
      cnstED_TextAnchor:       TextAnchor := AData.DataInt16[I];
      cnstED_TextDecoration:   TextDecoration := AData.DataInt16[I];
      cnstED_StrokeLineCap:    StrokeLineCap := AData.DataInt16[I];
      cnstED_StrokeLineJoin:   StrokeLineJoin := AData.DataInt16[I];
      cnstED_Display:          Display := AData.DataInt16[I];
      cnstED_EntVis:           EntVis := AData.DataInt16[I];

      cnstED_FillOpacity:      FillOpacity := AData.DataSingle[I];
      cnstED_FontWeight:       FontWeight := AData.DataSingle[I];
      cnstED_FontSize:         FontSize:= AData.DataSingle[I];
      cnstED_StrokeWidth:      StrokeWidth := AData.DataSingle[I];
      cnstED_StrokeMiterlimit: StrokeMiterlimit := AData.DataSingle[I];
      cnstED_StrokeOpacity:    StrokeOpacity := AData.DataSingle[I];
      cnstED_Opacity:          Opacity := AData.DataSingle[I];
      cnstED_StrokeDashOffset: StrokeDashOffset := AData.DataSingle[I];

      cnstED_FillColor:        vFillColor.Color := AData.DataInt[I];
      cnstED_StrokeColor:      StrokeColor := AData.DataInt[I];
      cnstED_FontStyles:       FontStyles := IntToFontStyles(AData.DataInt[I]);
      cnstED_FillStyle:
        begin
          vFillColor.Style := TsgCADStyleFill(AData.DataInt[I]);
        end;

      cnstED_FontName:         FontName :=  AData.DataString[I];
      cnstED_StrokeDashArray:
        begin
          if Converter <> nil then
            StrokeDashArray := Converter.LTypeByName(AData.DataString[I]);
        end
    else
      SetExtDataProperty(AData.DataCode[I], AData, I);
    end;
  FillColor := vFillColor;
  if vHasTM then
    TransformMatrix := vTM
  else
    TransformMatrix := cnstIdentityMat;
end;

procedure TsgSVGContainer.SetExtDataProperty(const ACode: SmallInt;
  const AData: TsgCADExtendedData; const AIndex: Integer);
begin
end;

procedure TsgSVGContainer.SetFillColor(const AValue: TsgFillColor);
begin
  FProperties.Mask := FProperties.Mask + [pmFillColor];
  FProperties.FillColor := AValue;
end;

procedure TsgSVGContainer.SetStrokeColor(const AValue: TColor);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeColor];
  FProperties.StrokeColor := AValue;
end;

procedure TsgSVGContainer.SetStrokeWidth(const AValue: Single);
begin
  FProperties.Mask := FProperties.Mask + [pmStrokeWidth];
  FProperties.StrokeWidth := AValue;
end;

function TsgSVGContainer.ToXMLGeoData(const ANode: TsgNode;
  const AParams: TsgXMLParams): Integer;
var
  vColorAttr: TsgNodeSample;
  vColor: TsgColorCAD;
begin
  Result := cnstXML_OK;
  ANode.RemoveAttributeByName(cnstXMLNames[xmlLineWeight].Name, True);
  ANode.RemoveAttributeByName(cnstXMLNames[xmlPoint].Name, True);
  vColor := MakeColorCAD(acRGBColor, StrokeColor);
  vColorAttr := ANode.GetAttributeByName(cnstXMLNames[xmlColor].Name);
  if Assigned(vColorAttr) then
    vColorAttr.ValueData.ValueAsColorCAD := vColor
  else
    ANode.AddAttribNV(cnstXMLNames[xmlColor].Name).ValueData.ValueAsColorCAD := vColor;
end;

function TsgSVGContainer.ToXMLNode(const ANode: TsgNode;
  const AParams: TsgXMLParams): Integer;
var
  vMatProps: string;
begin
  Result := inherited ToXMLNode(ANode, AParams);
  vMatProps := GetMatProp;
  if Length(vMatProps) > 0 then
    ANode.AddAttribNV(cnstXMLNames[xmlMatProp].Name).ValueAsStr := vMatProps;
end;

function TsgSVGContainer.FromXMLNode(const AType: TsgXMLType;
  const ANode: TsgNodeSample; const AIsChild: Boolean;
  const AResult: IsgResultNode = nil): Integer;
begin
  Result := cnstXML_OK;
  case AType.Id of
    xmlColor:    StrokeColor := ConvertColorCADToRGB(ANode.ValueData.ValueAsColorCAD);
    xmlMatProp:  SetMatProp(ANode.ValueAsStr);
    xmlExtrusion, xmlAngle, xmlFPointScale, xmlPoint, xmlLineWeight:
      Result := cnstXML_UNSUPPORTED;
  else
    Result := inherited FromXMLNode(AType, ANode, AIsChild, AResult);
  end;
end;

constructor TsgSVGContainer.Create;
begin
  inherited Create;
  LineWeight := fLineWeightByBlock;
  Color := clByBlock;
  CreateProperties;
  Block := TsgDXFBlock.Create;
  TsgDXFBlockAccess(Block).SetFlags(1);
  FProperties.StrokeColor := clByBlock;
  FProperties.StrokeWidth := fLineWeightByBlock;
  FProperties.FillColor := cnstByBlockFillColor;
end;

destructor TsgSVGContainer.Destroy;
begin
  FreeAndNil(FExtendedInfo);
  inherited Destroy;
end;

procedure TsgSVGContainer.EndExtData(const AData: TsgCADExtendedData);
begin
  AData.AddString(String_1002, '}');
end;

function TsgSVGContainer.AddEntity(const AEntity: TsgDXFEntity): Integer;
begin
  Result := -1;
  if (AEntity.EntType = ceInsert) and (AEntity is TsgSVGContainer) then
  begin
    Block.IsLoaded := False;
    Result := Block.AddEntity(AEntity);
    TsgSVGContainer(AEntity).FContainer := Self;
  end;
end;

procedure TsgSVGContainer.AssignEntity(ASource: TsgDXFEntity);
var
  vSource: TsgSVGContainer;
begin
  FFlags := 0;
  if (not Assigned(Converter)) and Assigned(ASource.Converter) then
    SetConverter(ASource.Converter);
  inherited AssignEntity(ASource);
  if ASource is TsgSVGContainer then
  begin
    vSource := TsgSVGContainer(ASource);
    FID := vSource.FID;
    FContainer := vSource.FContainer;
    FProperties.Assign(vSource.FProperties);
    if vSource.FTMatrix <> nil then
    begin
      if not Assigned(FTMatrix) then
        New(FTMatrix);
      FTMatrix^ := vSource.FTMatrix^;
    end
    else
      DisposeAndNil(FTMatrix);
    FreeAndNil(FExtendedInfo);
    if Assigned(vSource.FExtendedInfo) then
    begin
      FreeAndNil(FExtendedInfo);
      FExtendedInfo := TClassExtendedInfo(vSource.FExtendedInfo.ClassType).Create;
      FExtendedInfo.Assign(vSource.FExtendedInfo);
    end;
  end;
end;

function TsgSVGContainer.BeginExtData(const AData: TsgCADExtendedData; const AAppID: string): Boolean;
begin
  Result := False;
  if Assigned(DXFConv.LoadEntityProc) and (@LoadEntityProc = @SVGLoadEntity) and (AAppID = GetAppID) then
  begin
    AData.AddString(String_1001, AAppID);
    AData.AddString(String_1000, cnstGroupTypeName[EntClass.EG]);
    AData.AddString(String_1002, '{');
    AData.AddInt16(Integer_1070, Integer_70);
    AData.AddInt(Integer_1071, EntClass.ET.Val);
    Result := True;
  end;
end;

procedure TsgSVGContainer.CopyProps(ASource: TsgSVGContainer);
begin
  FillColor := ASource.FillColor;
  FillRule := ASource.FillRule;
  FillOpacity := ASource.FillOpacity;
  FontVariant := ASource.FontVariant;
  FontWeight := ASource.FontWeight;
  FontStretch := ASource.FontStretch;
  FontName := ASource.FontName;
  FontSize := ASource.FontSize;
  FontStyles := ASource.FontStyles;
  TextAnchor := ASource.TextAnchor;
  TextDecoration := ASource.TextDecoration;
  StrokeColor := ASource.StrokeColor;
  StrokeWidth := ASource.StrokeWidth;
  StrokeLineCap := ASource.StrokeLineCap;
  StrokeLineJoin := ASource.StrokeLineJoin;
  StrokeMiterlimit := ASource.StrokeMiterlimit;
  StrokeDashArray := ASource.StrokeDashArray;
  StrokeDashOffset := ASource.StrokeDashOffset;
  StrokeOpacity := ASource.StrokeOpacity;
end;

function TsgSVGContainer.HasTransformMatrix: Boolean;
begin
  Result := FTMatrix <> nil;
end;

function TsgSVGContainer.GetMatPropId: TsgNodeSample;
var
  vMatProp: TsgNodeSample;
begin
  Result := nil;
  if Assigned(ExtendedInfo) and Assigned(ExtendedInfo.Node) then
  begin
    vMatProp := ExtendedInfo.Node.GetChildByName('matProp');
    if Assigned(vMatProp) then
      Result := vMatProp.GetAttributeByName('id');
  end;
end;

function TsgSVGContainer.GetMatProp: string;
var
  vMatPropId: TsgNodeSample;
begin
  Result := '';
  vMatPropId := GetMatPropId;
  if Assigned(vMatPropId) then
    Result := vMatPropId.ValueAsStr;
end;

procedure TsgSVGContainer.SetMatProp(const AStr: string);
var
  vColor: Integer;
  vFillColor: TsgFillColor;
  vMatPropIdAttr, vFillColorAttr, vStyleAttr: TsgNodeSample;
  vFillStr, vStyleStr, vMaterialID: string;
begin
  vMatPropIdAttr := GetMatPropId;
  if Assigned(vMatPropIdAttr) then
  begin
    if GetIDByMaterialName(AStr, vMaterialID) then
      vMatPropIdAttr.ValueAsStr := vMaterialID
    else
      vMatPropIdAttr.ValueAsStr := AStr;
    vFillColor.IsStyle := False;
    if GetColorByMaterialName(AStr, vColor) then
    begin
      vFillColor.Color := vColor;
      vFillStr := '#' + IntToHex(vColor, 0);
    end
    else
    begin
      vFillColor.Color := clNone;
      vFillStr := 'none';
    end;
    FillColor := vFillColor;
    vFillColorAttr := ExtendedInfo.Node.GetAttributeByName(GetNameByTagId(idFill));
    if Assigned(vFillColorAttr) then
      vFillColorAttr.ValueAsStr := vFillStr;
    vStyleAttr := ExtendedInfo.Node.GetAttributeByName(GetNameByTagId(idStyle));
    if Assigned(vStyleAttr) then
    begin
      vStyleStr := vStyleAttr.ValueAsStr;
      if SetStyleValue(GetNameByTagId(idFill), vFillStr, vStyleStr) then
        vStyleAttr.ValueAsStr := vStyleStr
    end;
  end;
end;

{ TsvgProperties }

function TsvgProperties.GetFontFamily: string;
begin
  Result := FFontFamily;
end;

function TsvgProperties.GetFontStretch: Byte;
begin
  Result := FFontStretch;
end;

function TsvgProperties.GetFontStyles: TmvFontStyles;
begin
  Result := FFontStyles;
end;

function TsvgProperties.GetFontVariant: Byte;
begin
  Result := FFontVariant;
end;

function TsvgProperties.GetFontWeight: Single;
begin
  Result := FFontWeight;
end;

function TsvgProperties.GetInternalMask: TsgPropertiesMasks;
begin
  Result := inherited GetInternalMask +
    [pmTextAnchor, pmFontFamily, pmFontStyles, pmFontVariant, pmFontWeight,
     pmFontStretch, pmFontSize, pmTextDecoration];
end;

function TsvgProperties.GetTextAnchor: Byte;
begin
  Result := FTextAnchor;
end;

function TsvgProperties.GetTextDecoration: Byte;
begin
  Result := FTextDecoration;
end;

procedure TsvgProperties.SetFontFamily(const AValue: string);
begin
  FFontFamily := AValue;
end;

procedure TsvgProperties.SetFontStretch(const AValue: Byte);
begin
  FFontStretch := AValue;
end;

procedure TsvgProperties.SetFontStyles(const AValue: TmvFontStyles);
begin
  FFontStyles := AValue;
end;

procedure TsvgProperties.SetFontVariant(const AValue: Byte);
begin
  FFontVariant := AValue;
end;

procedure TsvgProperties.SetFontWeight(const AValue: Single);
begin
  FFontWeight:= AValue;
end;

procedure TsvgProperties.SetTextAnchor(const AValue: Byte);
begin
  FTextAnchor := AValue;
end;

procedure TsvgProperties.SetTextDecoration(const AValue: Byte);
begin
  FTextDecoration := AValue;
end;

constructor TsvgProperties.Create;
begin
  inherited Create;
  FTextAnchor := cnstDefTextAnchor;
  FFontFamily := cnstDefFontFamily;
  FFontStyles := cnstDefFontStyles;
  FFontVariant := cnstDefFontVariant;
  FFontWeight := cnstDefFontWeight;
  FFontStretch := cnstDefFontStretch;
  FFontSize := cnstDefFontSize;
  FTextDecoration := cnstDefTextDecoration;
end;

procedure TsvgProperties.Assign(const AProps: TsvgPropertiesBase);
begin
  inherited Assign(AProps);
  if AProps is TsvgProperties then
  begin
    FTextAnchor := TsvgProperties(AProps).FTextAnchor;
    FFontFamily := TsvgProperties(AProps).FFontFamily;
    FFontStyles := TsvgProperties(AProps).FFontStyles;
    FFontVariant := TsvgProperties(AProps).FFontVariant;
    FFontWeight := TsvgProperties(AProps).FFontWeight;
    FFontStretch := TsvgProperties(AProps).FFontStretch;
    FTextDecoration := TsvgProperties(AProps).FTextDecoration;
  end;
end;

{ TsvgPropertiesBase }

procedure TsvgPropertiesBase.SetMask(const AValue: TsgPropertiesMasks);
begin
  FMask := GetInternalMask * AValue;
end;

function TsvgPropertiesBase.GetDisplay: Byte;
begin
  Result := FDisplay;
end;

function TsvgPropertiesBase.GetVisibility: Byte;
begin
  Result := FVisibility;
end;

function TsvgPropertiesBase.GetFillColor: TsgFillColor;
begin
  Result := cnstDefFillColor;
end;

function TsvgPropertiesBase.GetFillOpacity: Single;
begin
  Result := cnstDefFillOpacity;
end;

function TsvgPropertiesBase.GetFillRule: Byte;
begin
  Result := cnstDefFillRule;
end;

function TsvgPropertiesBase.GetFontFamily: string;
begin
  Result := cnstDefFontFamily;
end;

function TsvgPropertiesBase.GetFontSize: Single;
begin
  Result := FFontSize;
end;

function TsvgPropertiesBase.GetFontStretch: Byte;
begin
  Result := cnstDefFontStretch;
end;

function TsvgPropertiesBase.GetFontStyles: TmvFontStyles;
begin
  Result := cnstDefFontStyles;
end;

function TsvgPropertiesBase.GetFontVariant: Byte;
begin
  Result := cnstDefFontVariant;
end;

function TsvgPropertiesBase.GetFontWeight: Single;
begin
  Result := cnstDefFontWeight;
end;

function TsvgPropertiesBase.GetInternalMask: TsgPropertiesMasks;
begin
  Result := [pmDisplay, pmVisibility, pmOpacity, pmFontSize, pmVectorEffect];
end;

function TsvgPropertiesBase.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TsvgPropertiesBase.GetStrokeColor: TColor;
begin
  Result := cnstDefStroke;
end;

function TsvgPropertiesBase.GetStrokeDashArray: Pointer;
begin
  Result := cnstDefStrokeDashArray;
end;

function TsvgPropertiesBase.GetStrokeDashOffset: Single;
begin
  Result := cnstDefStrokeDashOffset;
end;

function TsvgPropertiesBase.GetStrokeLineCap: Byte;
begin
  Result := cnstDefStrokeLinecap;
end;

function TsvgPropertiesBase.GetStrokeLineJoin: Byte;
begin
  Result := cnstDefStrokeLinejoin;
end;

function TsvgPropertiesBase.GetStrokeMiterlimit: Single;
begin
  Result := cnstDefStrokeMiterlimit;
end;

function TsvgPropertiesBase.GetStrokeOpacity: Single;
begin
  Result := cnstDefStrokeOpacity;
end;

function TsvgPropertiesBase.GetStrokeWidth: Double;
begin
  Result := cnstDefStrokeWidth;
end;

function TsvgPropertiesBase.GetTextAnchor: Byte;
begin
  Result := cnstDefTextAnchor;
end;

function TsvgPropertiesBase.GetTextDecoration: Byte;
begin
  Result := cnstDefTextDecoration;
end;

function TsvgPropertiesBase.GetVectorEffect: Byte;
begin
  Result := FVectorEffect;
end;

procedure TsvgPropertiesBase.SetDisplay(const AValue: Byte);
begin
  FDisplay := AValue and 31;
end;

procedure TsvgPropertiesBase.SetVisibility(const AValue: Byte);
begin
  FVisibility := AValue;
end;

procedure TsvgPropertiesBase.SetFillColor(const AValue: TsgFillColor);
begin
end;

procedure TsvgPropertiesBase.SetFillOpacity(const AValue: Single);
begin
end;

procedure TsvgPropertiesBase.SetFillRule(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetFontFamily(const AValue: string);
begin
end;

procedure TsvgPropertiesBase.SetFontSize(const AValue: Single);
begin
  FFontSize := AValue;
end;

procedure TsvgPropertiesBase.SetFontStretch(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetFontStyles(const AValue: TmvFontStyles);
begin
end;

procedure TsvgPropertiesBase.SetFontVariant(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetFontWeight(const AValue: Single);
begin
end;

procedure TsvgPropertiesBase.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
end;

procedure TsvgPropertiesBase.SetStrokeColor(const AValue: TColor);
begin
end;

procedure TsvgPropertiesBase.SetStrokeDashArray(const AValue: Pointer);
begin
end;

procedure TsvgPropertiesBase.SetStrokeDashOffset(const AValue: Single);
begin
end;

procedure TsvgPropertiesBase.SetStrokeLineCap(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetStrokeLineJoin(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetStrokeMiterlimit(const AValue: Single);
begin
end;

procedure TsvgPropertiesBase.SetStrokeOpacity(const AValue: Single);
begin
end;

procedure TsvgPropertiesBase.SetStrokeWidth(const AValue: Double);
begin
end;

procedure TsvgPropertiesBase.SetTextAnchor(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetTextDecoration(const AValue: Byte);
begin
end;

procedure TsvgPropertiesBase.SetVectorEffect(const AValue: Byte);
begin
  FVectorEffect := AValue and 1;
end;

constructor TsvgPropertiesBase.Create;
begin
  FDisplay := cnstDefDisplay;
  FOpacity := cnstDefOpacity;
  FVisibility := cnstDefVisibility;
  FFontSize := cnstDefFontSize;
end;

procedure TsvgPropertiesBase.Assign(const AProps: TsvgPropertiesBase);
begin
  FMask := AProps.FMask;
  FDisplay := AProps.FDisplay;
  FVisibility := AProps.FVisibility;
  FFontSize := AProps.FFontSize;
  FVectorEffect := AProps.FVectorEffect;
end;

{ TsvgPropertiesFont }

function TsvgPropertiesFont.GetFillColor: TsgFillColor;
begin
  Result := FFillColor;
end;

function TsvgPropertiesFont.GetFontFamily: string;
begin
  Result := FFontFamily;
end;

function TsvgPropertiesFont.GetFontStretch: Byte;
begin
  Result := FFontStretch;
end;

function TsvgPropertiesFont.GetFontStyles: TmvFontStyles;
begin
  Result := FFontStyles;
end;

function TsvgPropertiesFont.GetFontVariant: Byte;
begin
  Result := FFontVariant;
end;

function TsvgPropertiesFont.GetFontWeight: Single;
begin
  Result := FFontWeight;
end;

function TsvgPropertiesFont.GetInternalMask: TsgPropertiesMasks;
begin
  Result := inherited GetInternalMask +
   [pmFillColor, pmStrokeColor, pmStrokeWidth, pmTextAnchor, pmFontFamily,
    pmFontStyles, pmFontVariant, pmFontWeight, pmFontStretch, pmFontSize,
    pmTextDecoration];
end;

function TsvgPropertiesFont.GetStrokeColor: TColor;
begin
  Result := FStrokeColor;
end;

function TsvgPropertiesFont.GetStrokeWidth: Double;
begin
  Result := FStrokeWidth;
end;

function TsvgPropertiesFont.GetTextAnchor: Byte;
begin
  Result := FTextAnchor;
end;

function TsvgPropertiesFont.GetTextDecoration: Byte;
begin
  Result := FTextDecoration;
end;

procedure TsvgPropertiesFont.SetFillColor(const AValue: TsgFillColor);
begin
  FFillColor := AValue;
end;

procedure TsvgPropertiesFont.SetFontFamily(const AValue: string);
begin
  FFontFamily := AValue;
end;

procedure TsvgPropertiesFont.SetFontStretch(const AValue: Byte);
begin
  FFontStretch := AValue;
end;

procedure TsvgPropertiesFont.SetFontStyles(const AValue: TmvFontStyles);
begin
  FFontStyles := AValue;
end;

procedure TsvgPropertiesFont.SetFontVariant(const AValue: Byte);
begin
  FFontVariant := AValue;
end;

procedure TsvgPropertiesFont.SetFontWeight(const AValue: Single);
begin
  FFontWeight:= AValue;
end;

procedure TsvgPropertiesFont.SetStrokeColor(const AValue: TColor);
begin
  FStrokeColor := AValue;
end;

procedure TsvgPropertiesFont.SetStrokeWidth(const AValue: Double);
begin
  FStrokeWidth := AValue;
end;

procedure TsvgPropertiesFont.SetTextAnchor(const AValue: Byte);
begin
  FTextAnchor := AValue;
end;

procedure TsvgPropertiesFont.SetTextDecoration(const AValue: Byte);
begin
  FTextDecoration := AValue;
end;

constructor TsvgPropertiesFont.Create;
begin
  inherited Create;
  FFillColor := cnstDefFillColor;
  FStrokeColor := cnstDefStroke;
  FStrokeWidth := cnstDefStrokeWidth;
  FTextAnchor := cnstDefTextAnchor;
  FFontFamily := cnstDefFontFamily;
  FFontStyles := cnstDefFontStyles;
  FFontVariant := cnstDefFontVariant;
  FFontWeight := cnstDefFontWeight;
  FFontStretch := cnstDefFontStretch;
  FTextDecoration := cnstDefTextDecoration;
end;

procedure TsvgPropertiesFont.Assign(const AProps: TsvgPropertiesBase);
begin
  inherited Assign(AProps);
  if AProps is TsvgPropertiesFont then
  begin
    FFillColor := TsvgPropertiesFont(AProps).FFillColor;
    FStrokeColor := TsvgPropertiesFont(AProps).FStrokeColor;
    FStrokeWidth := TsvgPropertiesFont(AProps).FStrokeWidth;
    FTextAnchor := TsvgPropertiesFont(AProps).FTextAnchor;
    FFontFamily := TsvgPropertiesFont(AProps).FFontFamily;
    FFontStyles := TsvgPropertiesFont(AProps).FFontStyles;
    FFontVariant := TsvgPropertiesFont(AProps).FFontVariant;
    FFontWeight := TsvgPropertiesFont(AProps).FFontWeight;
    FFontStretch := TsvgPropertiesFont(AProps).FFontStretch;
    FTextDecoration := TsvgPropertiesFont(AProps).FTextDecoration;
  end;
end;

{ TsvgPropertiesPenBrush }

function TsvgPropertiesPenBrush.GetFillColor: TsgFillColor;
begin
  Result := FFillColor;
end;

function TsvgPropertiesPenBrush.GetFillOpacity: Single;
begin
  Result := FFillOpacity;
end;

function TsvgPropertiesPenBrush.GetInternalMask: TsgPropertiesMasks;
begin
  Result := inherited GetInternalMask +
    [pmFillColor, pmFillOpacity, pmStrokeColor, pmStrokeDashArray,
     pmStrokeDashOffset, pmStrokeLineCap, pmStrokeLineJoin, pmStrokeWidth];
end;

function TsvgPropertiesPenBrush.GetStrokeColor: TColor;
begin
  Result := FStrokeColor;
end;

function TsvgPropertiesPenBrush.GetStrokeDashArray: Pointer;
begin
  Result := FStrokeDashArray;
end;

function TsvgPropertiesPenBrush.GetStrokeDashOffset: Single;
begin
  Result := FStrokeDashOffset;
end;

function TsvgPropertiesPenBrush.GetStrokeLineCap: Byte;
begin
  Result := FStrokeLineCap;
end;

function TsvgPropertiesPenBrush.GetStrokeLineJoin: Byte;
begin
  Result := FStrokeLineJoin;
end;

function TsvgPropertiesPenBrush.GetStrokeWidth: Double;
begin
  Result := FStrokeWidth;
end;

procedure TsvgPropertiesPenBrush.SetFillColor(const AValue: TsgFillColor);
begin
  FFillColor := AValue;
end;

procedure TsvgPropertiesPenBrush.SetFillOpacity(const AValue: Single);
begin
  FFillOpacity := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeColor(const AValue: TColor);
begin
  FStrokeColor := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeDashArray(const AValue: Pointer);
begin
  FStrokeDashArray := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeDashOffset(const AValue: Single);
begin
  FStrokeDashOffset := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeLineCap(const AValue: Byte);
begin
  FStrokeLineCap := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeLineJoin(const AValue: Byte);
begin
  FStrokeLineJoin := AValue;
end;

procedure TsvgPropertiesPenBrush.SetStrokeWidth(const AValue: Double);
begin
  FStrokeWidth := AValue;
end;

constructor TsvgPropertiesPenBrush.Create;
begin
  inherited Create;
  FFillColor := cnstDefFillColor;
  FFillOpacity := cnstDefFillOpacity;
  FStrokeColor := cnstDefStroke;
  FStrokeDashArray := cnstDefStrokeDashArray;
  FStrokeDashOffset := cnstDefStrokeDashOffset;
  FStrokeLineCap := cnstDefStrokeLineCap;
  FStrokeLineJoin := cnstDefStrokeLineJoin;
  FStrokeWidth := cnstDefStrokeWidth;
end;

procedure TsvgPropertiesPenBrush.Assign(const AProps: TsvgPropertiesBase);
begin
  inherited Assign(AProps);
  if AProps is TsvgPropertiesPenBrush then
  begin
    FFillColor := TsvgPropertiesPenBrush(AProps).FFillColor;
    FFillOpacity := TsvgPropertiesPenBrush(AProps).FFillOpacity;
    FStrokeColor := TsvgPropertiesPenBrush(AProps).FStrokeColor;
    FStrokeDashArray := TsvgPropertiesPenBrush(AProps).FStrokeDashArray;
    FStrokeDashOffset := TsvgPropertiesPenBrush(AProps).FStrokeDashOffset;
    FStrokeLineCap := TsvgPropertiesPenBrush(AProps).FStrokeLineCap;
    FStrokeLineJoin := TsvgPropertiesPenBrush(AProps).FStrokeLineJoin;
    FStrokeWidth := TsvgPropertiesPenBrush(AProps).FStrokeWidth;
  end;
end;

{  TsgSVGExtendedInfo  }

constructor TsgSVGExtendedInfo.Create;
begin
  inherited Create;
  Node := nil;
end;

destructor TsgSVGExtendedInfo.Destroy;
begin
  inherited Destroy;
end;

procedure TsgSVGExtendedInfo.Assign(const ASource: TsgSVGExtendedInfo);
begin
//  inherited Assign(ASource);
  Node := ASource.Node;
end;

end.

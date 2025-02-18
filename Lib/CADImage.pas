{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{               CAD files TGraphic base class                }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools?                       }
{************************************************************}

unit CADImage;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_AGG2D}
  {$IFNDEF SG_USE_AGG2D_AS_GDI}
    {$DEFINE USE_SG_AGG2D}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF SG_HAS_ACTIVEX}ActiveX, {$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  ComObj,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$IFDEF USE_SG_AGG2D}
  Agg_LCL, FPCanvas,
{$ENDIF}
{$ENDIF}
  Classes, SysUtils,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, System.Devices,
  System.Math.Vectors, System.UIConsts,
{$ELSE}
  Graphics, MVFont,
  {$IFNDEF SG_CAD_DLL}{$IFNDEF CS_PLUGINS}
  Forms, Dialogs,
  {$ENDIF}{$ENDIF}
{$ENDIF}
  TTF, sgLists, sgLines, sgFunction, sgContext,
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ,// Winapi.GDIPUTIL,
{$ELSE}
  GDIPlus,
{$ENDIF}
{$ENDIF}
  DXFConv, sgConsts, SHX, sgSelection, sgXMLParser, Properties
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
{$IFNDEF SG_NO_USE_KERNEL3D}
  ,sgModeller,sgMDefinitions, sgMFunctions
{$ENDIF}
  ;

// For working with SaveToStream and Assign should be set sgConsts.KeepOriginal := True
// This is necessary for Fast Report Add-in.

type

{$IFDEF MSWINDOWS}
  {$IFDEF SGFPC}
  PInteger = ObjPas.PInteger;
  {$ENDIF}
{$ENDIF}

  TsgDXFDrawMode = (dmNormal, dmBlack, dmGray);

  TsgSHXFontsProc = function: TsgSHXFontList of object;
  TsgGetSelectionMatrixModeProc = function: TsgSelectionMode of object;
  TsgSetSelectionMatrixModeProc = procedure (const AMode: TsgSelectionMode) of object;

  // Obsolete type
  TScaling = record
    Offset: TFPoint;
    Scale: TFPoint;
    DX: Integer;
    DY: Integer;
  end;

  PsgCanvasParams = ^TsgCanvasParams;
  TsgCanvasParams = record
    Brush: TBrush;
    Font: TFont;
    Pen: TPen;
    Region: TRegion;
  end;

  PsgViewPortCanvasParams = ^TsgViewPortCanvasParams;
  TsgViewPortCanvasParams = record
    Index: Integer;
    CanvasParams: TsgCanvasParams;
  end;

  TsgCalcCoordsStruct = record
    XScaled: Extended;
    YScaled: Extended;
    X: TsgFloat;
    Y: TsgFloat;
    Rot: TFMatrix;
    RealLayoutCenter: TFPoint;
    NullCoord: Integer;
  end;

  TsgDistanceUnits = (duNone, duInch, duMM);
  TsgAngularUnits = (auNone);
  TsgMeasurement = record
    DistanceUnits: TsgDistanceUnits;
    AngularUnits: TsgAngularUnits;
  end;

  TsgCADImage = class;
  TsgCADImageClass = class of TsgCADImage;

  { TsgCADImage }

  TsgCADImage = class(TGraphic, IsgXMLObject)
  private
    FBackgroundColor: TColor;
    FChangeBackgroundColor: TColor;
    FPlotWindowAreaColor: TColor;
    FBorderSize: TsgBorderSize;
    FBorderType: TsgBorderType;
    FCachingRasterMode: TsgCachingRasterMode;
    FCanvasParams: TsgCanvasParams;
    FContext: TsgContext;
    FContextSelectionMatrix: TsgContextSelection;
    FCBFormat: Word;
    FCenter: TFPoint;
    FClippingRect: PFRect;
    FColorToLineWeightMap: TsgColorToLineWeightMap;
    FConverter: TsgDXFConverter;
    FConverting: Boolean;
    FCurrentLayout: TsgDXFLayout;
    FPixel: TPoint;
    FDottedSingPts: TFPointList;
    FSnapLastPoint: TFPoint;
    FDxyz: TFPoint;
    FColorBrush: TColor;
    FColorFont: TColor;
    FPenWidth : Single;
    FPixelsPerInch: Integer;
    FEntityHandle: Int64;
    FEntityLines: sgLines.TsgLines;
    FPolyPoints: TFPointList;

    FDefaultColor: TColor;
    FDrawingBox: TFRect;
    FDrawMode: TsgDXFDrawMode;
    FExportToDXFMode: Boolean;
    FExtents: TFRect;
    FFileInfo: string;
    FFileName: string;
    FIsDraw3DAxes: Boolean;
    FIsPlotting: Boolean;
    FIsProcessMessages: Boolean;
{$IFDEF SGFPC}
    FIsShowBackground: Boolean;
{$ENDIF}
    FIsWithoutBorder: Boolean;
    FLast: Integer;
    FLibraryGDIPlusExists: Boolean;
    FLockLayoutChangeEvent: Boolean;
    FMetafileRegionScale: Double;
    FMillimetres: Integer;
    FBadRotation: Boolean;
    FNullWidth: Double;
    FOnBeforeRotate: TNotifyEvent;
    FOnAfterRotate: TNotifyEvent;
    FOnLayoutChange: TNotifyEvent;
    FOnLayoutBeforeChange: TNotifyEvent;

    //If set in the main CADImage these fields will be set in all internal CADImages.
    FOnInsert: TsgDXFInsert;// -  an insert for which the block entities will be drawn separately on the bitmap.
    FOnInsertMode: Boolean;// - a mode of separate drawing the entities on the bitmap for the diven insert.
    FOnComplexEntity: TsgDXFEntity;

    FPureExtents: TFRect;
    FReady: Boolean;
    FRect: TRect;
    FRegenEntities: TsgObjectCollection;
    FRegenScales: TsgObjectList;
    FRegenDelta: Double;
    FZRotate: Double;
    FSelectionMatrixMode: TsgSelectionMode;
    FObjectSnapMask: TObjectSnapState;
    FOnGetBox: TsgGetBox;
    FSnapEntity: TsgDXFEntity;
    FSnapEntityDefault: TsgDXFEntity;
    FSnapMatrix: TsgSnapMatrix;
    FSnapOnlyIterate: Boolean;
    FStoreSnapOnlyIterate: Boolean;
    FStoreInsert: TsgDXFInsert;
    FStream: TMemoryStream;
    FStretch: Boolean;
    FUseWinEllipse: Boolean;
    FShowImages: Boolean;
    FViewPortCanvasParams: PsgViewPortCanvasParams;
    FCanvasRegion: TRegion;
    FXMax: Double;
    FXMin: Double;
    FYMax: Double;
    FYMin: Double;
    FZMax: Double;
    FZMin: Double;
    FZoom: Double;
    FViewRectangle: TFRect;
    FLineWeightScale: TsgFloat;
    FResizeDisproportionateDrawing: Boolean;
    FXrefStack: TsgObjectList;
    FXDisproportionateShift: TsgFloat;
    FYDisproportionateShift: TsgFloat;
    FSHXFontsProc: TsgSHXFontsProc;
    FGetSelectionMatrixModeProc: TsgGetSelectionMatrixModeProc;
    FSetSelectionMatrixModeProc: TsgSetSelectionMatrixModeProc;
    FSelectionMatrix: TsgSelectionMatrix;
    FTTFMode: TsgTTFMode;
    FMessage: string;

    FExpProcs: TsgProxyExport;
    FRegionStack: TsgRegionStack;

    FOnAfterDraw: TNotifyEvent;
    FOnBeforeDraw: TNotifyEvent;
    FAfterDrawNotifications: TsgNotifications;
    FBeforeDrawNotifications: TsgNotifications;
    FAfterIterateNotifications: TsgNotifications;
    FBeforeIterateNotifications: TsgNotifications;

    FCustomDraw: Boolean;
    FOffset: TFPoint;
    FOnMatrixChanged: TNotifyEvent;
    FRotCount: Integer;
    FRotToView: Boolean;

    FOnMetafileExport: TsgObjMetafileExport;
    FUsePlotSetting: Boolean;
    FShowPlotSetting: Boolean;
    FShowPlotMargins: Boolean;
    FShowPlotFrame: Boolean;
    FShowPlotForModel: Boolean;
    FPrinting: TsgPrintMode;
    FDeviceDPI: TPoint;
    FGuid: TGUID;
    FXrefTransparency: Single;
// saved drawing params
    FDrawSaved: TsgCADIterate;
    FTmpPlotWindow: TsgDXFPlotSettings;
    FExternalRegion: TsgDXFEntity;
    FConstants: TsgConstantsCustom;
{$IFDEF SG_BTI}
    FUseFillStyleIndex: Boolean;
{$ENDIF}
    FAnnotativeBlockRecordStack: TsgList;
    FOnDrawCustomEntity: TNotifyEvent;
    FViewportRect: TRect;

    FPercentDone: Integer;
    FUseColorRGBInColorMap: Boolean;
{$IFDEF SG_FM_LINUX}
    FIsPrinterCanvas: Boolean;
{$ENDIF}
    FOnDrawBackground: TsgCADEntityProc;
//  the following functions are "end"-functions for GDI or for export to formats
    procedure ExpPixelInteger(const Sender: TObject; APoint: TPoint; AColor: TColor);
    procedure ExpArcInteger(const Sender: TObject; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
      const Arc: TsgArc);
    procedure ExpPolylineInteger(const Sender: TObject; const APoints: TsgIntegerList);
    procedure ExpPolygonInteger(const Sender: TObject; const APoints: TsgIntegerList);
    procedure ExpPolyPolylineLists(const Sender: TObject; APoints : TsgIntegerList;
      ACounts: Pointer; ACount: Integer; const ACheckDrawOnBitMap: Boolean = True);
    procedure ExpPolyPolygonInteger(const Sender: TObject; Points: PPoint; Counts:
      PInteger; Count: Integer; const ACheckDrawOnBitMap: Boolean = True);
    {$IFDEF SG_BTI}
    procedure ExpPolyPolygonLists(const Sender: TObject; APoints : TsgIntegerList;
      ACounts: Pointer; ACount: Integer; const ACheckDrawOnBitMap: Boolean = True);
    {$ENDIF}
    procedure ExpTextInteger(ATextType: TsgTextType; X, Y: Integer; Options: Longint;
      Rect: PRect; StrWideChar: PWideChar; StrChar: PAnsiChar; Count: Longint;
      Dx:{$IFDEF SGFPC}Pointer{$ELSE}PInteger{$ENDIF};
      const ATextParams: PsgExpTextParam);
    procedure DoSetColor(AColor: TColor; AColorType: TsgColorType;
      AUseExternalColor: Integer = 1);
    function SetColorByExternalPen(const AUseExternalColor: Integer;
      var AColor: TColor; const AEntColor: PsgColorCAD): Boolean;
    procedure DoSetFont(AFont: TFont);
    procedure DoSetStyle(AStyle: Integer; AColorType: TsgColorType);
//    procedure DoExpPolyline(const Points: array of TPoint);
//    procedure DoExpPolygon(const Points: array of TPoint);
    procedure ExpFillRegionInternal(const ASender: TObject; ARegion: TRegion;
      const ADrawOnBitMap: Boolean = True);
    procedure ExpArrowsInternal(const ASender: TObject;
      const APoly: TsgIntegerList; const AHasZTick: Boolean);
    procedure ExpSolidInternal(const ASender: TObject;
      var ASolid, APts: array of TPoint; AHasZTick: Boolean);
    procedure ExpOle2FrameInternal(const ASender: TObject; const ARect: TF2DRect);
    procedure ExpTextInternal(const ASender: TObject; ATextGlyph: TsgTextGlyph;
      ATextItems: TStringBuilder; const AMatrix: TFMatrix; const ATextMode: Byte);
    procedure ExpImageGDIInternal(const ASender: TObject; const R: TRect;
      AGraphic: TPersistent);
    procedure AfterChangeEntitiesVisible;
    procedure ApplyText;
    procedure AddLastPixelToPoly(const IsPoly: Boolean; ACounts: TsgIntegerList);
    procedure AddRectAsPoly(const ABounds: TRect; const APoly: TsgIntegerList;
     const AClosed: Boolean = True);
    procedure BeginRead(Sender: TObject);
    procedure BoxPoint(const P: TFPoint);
    function CalcCADCoordsParameters({AXScaled, AYScaled: Extended;
               var AX, AY: TsgFloat; var ARot: TFMatrix;
               var ARealLayoutCenter: TFPoint; var ANullCoord: Integer}var AStruct: TsgCalcCoordsStruct): Boolean;
{$IFDEF SG_FIREMONKEY}
    procedure ClearImagesCache;
{$ENDIF}
    function CoordinateBorderCorrect(var AXScaled, AYScaled: Extended): Boolean;
    procedure DoProgress(Stage: TProgressStage);
    procedure DoRead(Sender: TObject);
    procedure DrawArc(Sender: TObject);
    procedure DrawCircle(Sender: TObject);
    procedure DrawFace(Sender: TObject);
    procedure DrawCADFill(Sender: TObject);
    procedure DrawHatch(Sender: TObject);
    procedure DrawMPolygon(Sender: TObject);
    function DrawDimension(Sender: TObject): Integer;
    procedure DrawLeader(Sender: TObject);
    procedure DrawLine(Sender: TObject);
    procedure DrawPoint(Sender: TObject);
    procedure DrawPoly(Sender: TObject);
    procedure DrawModEntity(const AEnt: TsgModEntity; var AParams: TsgBrepModIterateParams);
    procedure DrawACISEntity(Sender: TObject);
    procedure DrawSpline(Sender: TObject);
    procedure DrawSolid(Sender: TObject);
    procedure DrawShape(Sender: TObject);
    procedure DrawText(Sender: TObject);
    procedure DrawViewPort(Sender: TObject);
    procedure DrawImage(Sender: TObject);
{$IFNDEF SG_FIREMONKEY}
    procedure DrawImageGDIPlus(const AImgEnt: TsgDXFImageEnt;
      const APoints: TsgPointsListHelper);
{$ENDIF}
    procedure DrawImageEnt(Sender: TObject);
{$IFDEF SG_USEGDIPLUS}
    procedure DrawTilesImage(const AGraphics: TGPGraphics;
      const AImgEnt: TsgDXFImageEnt;
      const APoints: TsgPointsListHelper;
      const ImageAttributes: TGPImageAttributes = nil);
{$ENDIF}
    procedure DrawFlatPoly(Sender: TObject);
    procedure DrawFlatHatch(Sender: TObject);
    procedure DrawOle2Frame(Sender: TObject);
    procedure DrawPointsListByPolyline(const AObj: TObject; const DottedSingPts: TFPointList; AClosed: Boolean);
    procedure DrawPointsListByPolyPolyline(const AObj: TObject;
      const DottedSingPts: TFPointList; AClosed: Boolean = False); overload;
    procedure DrawPointsListByPolyPolyline(const AObj: TObject;
      const DottedSingPts: IsgArrayFPoint; AClosed: Boolean = False); overload;
    procedure DrawWipeout(Sender: TObject);
    procedure DrawXRef(Sender: TObject);
{$IFDEF SG_BTI}
    procedure DrawPolyPolyLine2D(Sender: TObject);
{$ENDIF}
    procedure DoSetClipInsert(AInsert: TsgCADClipInsert);
    procedure DoSetSpatialClip(AInsert: TsgDXFInsert; AFilter: TsgDXFEntity);
    procedure EndRead(Sender: TObject);
    procedure EnterViewport(Sender: TObject);
    procedure EnterXRef(Sender: TObject);
    procedure DoAppendPolyFromFlat(FP: TsgFlatEntity);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure FillFromFlat(FP: TsgFlatEntity);
    function NeedToAddOnePixel: Boolean;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function GetAlternateWhite: Boolean; virtual;
    function GetAlternateWhiteColor: TColor; virtual;
    function GetBackgroundColor: TColor;
    function GetDefaultColor: TColor;
    function GetBlock(Index: Integer): TsgDXFBlock;
    function GetClipping: TsgClipping;
    function GetEntities(Index: Integer): TsgDXFEntity;
    function GetIsLoading: Boolean;
    function GetIsDrawingRotated: Boolean;
    function GetShowLineWeight: Boolean;
    function GetLayout(Index: Integer): TsgDXFLayout;
    function GetLayoutsCount: Integer;
    function GetLineWeightScale: TsgFloat;
    function GetScale: TFPoint;
    function GetOwnSource: Boolean;
    function GetSHXFontsProc: TsgSHXFontList;
    function GetSelectionMatrixProc: TsgSelectionMode;
    function GetRealClip: PFRect;
    function GetRegenerateArcs: Boolean;
    function GetExpAnnotation: TsgExpAnnotation;
    function GetExpArc: TsgExpArc;
    function GetExpClipRgn: TsgExpClipRgn;
    function GetExpCloseFigure: TsgExpCloseFigure;
    function GetExpFillRgn: TsgExpFillRgn;
    function GetExpImage: TsgExpImage;
    function GetExpImageUV: TsgExpImageUV;
    function GetExpPixel: TsgExpPixel;
    function GetExpPolygon: TsgExpPolygon;
    function GetExpPolyline: TsgExpPolyline;
    function GetExpPolyPolygon: TsgExpPolyPolygon;
    function GetExpPolyPolyline: TsgExpPolyPolyline;
    function GetExpProgress: TsgExpProgress;
    function GetExpRestoreDC: TsgExpRestoreDC;
    function GetExpSaveDC: TsgExpSaveDC;
    function GetExpSetColor: TsgExpSetColor;
    function GetExpSetFont: TsgExpSetFont;
    function GetExpSetPenGeometric: TsgExpSetPenGeometric;
    function GetExpSetPenMiterLimit: TsgExpSetPenMiterLimit;
    function GetExpSetPenWidth: TsgExpSetPenWidth;
    function GetExpSetStyle: TsgExpSetStyle;
    function GetExpText: TsgExpText;
    function GetExpTextOut: TsgExpTextOut;
    function GetColorToLineWeight(var AContextPenColor: TsgColorCAD;
      const AEntColor: PsgColorCAD): Boolean;
    function GetIterator: Boolean;
    function InitializeLineType(AEntity: TsgDXFPenEntity): Boolean;
    procedure MakeAndDrawDottedSingPts(AEntity: TsgDXFEntity;
      const APolyPoints: TFPointList; const AClosed: Boolean);
    function PolyRegion(PL: TsgCADBasePolyline): TGraphicsObject;
    procedure DoAppendPoly(Points: TFPointList);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure FillPoly(PL: TsgCADBasePolyline);
    procedure ResetExtents;
    procedure SetAlternateWhite(const AValue: Boolean); virtual;
    procedure SetAlternateWhiteColor(const AValue: TColor); virtual;
    procedure SetBackgroundColor(Value: TColor);
    procedure SetPlotWindowAreaColor(Value: TColor);
    procedure SetDefaultColor(Value: TColor);

    procedure SetDrawingBox(const ABox: TFRect);
    procedure SetDrawMode(Value: TsgDXFDrawMode);
    procedure SetIsShowBackground(const AValue: Boolean);
    procedure SetIsShowLineWeight(Value: Boolean);
    procedure SetIsWithoutBorder(Value: Boolean);
    procedure SetRegenerateArcs(const AValue: Boolean);
    procedure SetSelectionMatrixProc(const AMode: TsgSelectionMode);
    procedure SetStretch(Value: Boolean);
    procedure SetUseWinEllipse(Value: Boolean);
    procedure SetShowImages(Value: Boolean);
    procedure SetDimensionsVisible(AVisible: Boolean);
    procedure SetTextVisible(AVisible: Boolean);
    procedure SetOwnSource(Value: Boolean);
    procedure DrawEntityOnSnapMatrix(Entity: TsgDXFEntity);
    procedure SetupSnapMatrix;
    procedure SetPixelSnapEx(const APt: TFPoint; Pt: TPoint;
      const ASnapState: TObjectSnapState);
    procedure SetPixelSnap(const APt: TFPoint;
      const ASnapState: TObjectSnapState);
    function GetNullWidth: Integer;
    procedure SetNullWidth(const Value: Integer);
{$IFNDEF SG_FIREMONKEY}
    function CheckRastrByFastMode(Sender: TObject;
      var AGraphic: TGraphic; const APoints: TsgPointsListHelper): Boolean;
{$ENDIF}
    function CheckOrthoRaster(const AAngle: Double): Boolean;
    procedure SetOnAfterDraw(const Value: TNotifyEvent);
    procedure SetOnBeforeDraw(const AValue: TNotifyEvent);
    procedure SetListsCapacity(const ASetDefault: Boolean);
    procedure SetOffset(const AValue: TFPoint);
    procedure SetUsePlotSetting(const Value: Boolean);
    procedure SetShowPlotSetting(const Value: Boolean);
    procedure SetShowPlotFrame(const Value: Boolean);
    procedure SetShowPlotMargins(const Value: Boolean);
    procedure SetShowPlotForModel(const Value: Boolean);
    function GetIsCurLayoutShowPlot: Boolean;
    procedure SetPrinting(const AValue: TsgPrintMode);
    procedure SetClipping(const AClip: TsgClipping);
    procedure SetTmpPlotWindow(const Value: TsgDXFPlotSettings);
    procedure SetExternalRegion(const Value: TsgDXFEntity);
    procedure SetExtents(const Value: TFRect);
    procedure SetBorderSize(const Value: TsgBorderSize);
    procedure SetBorderType(const Value: TsgBorderType);
    procedure SetTTFMode(const Value: TsgTTFMode);
    procedure SetSelectionMatrix(const Value: TsgSelectionMatrix);
    procedure SetExpAnnotation(const Value: TsgExpAnnotation);
    procedure SetExpArc(const Value: TsgExpArc);
    procedure SetExpClipRgn(const Value: TsgExpClipRgn);
    procedure SetExpCloseFigure(const Value: TsgExpCloseFigure);
    procedure SetExpFillRgn(const Value: TsgExpFillRgn);
    procedure SetExpImage(const Value: TsgExpImage);
    procedure SetExpImageUV(const Value: TsgExpImageUV);
    procedure SetExpPixel(const Value: TsgExpPixel);
    procedure SetExpPolygon(const Value: TsgExpPolygon);
    procedure SetExpPolyline(const Value: TsgExpPolyline);
    procedure SetExpPolyPolygon(const Value: TsgExpPolyPolygon);
    procedure SetExpPolyPolyline(const Value: TsgExpPolyPolyline);
    procedure SetExpProgress(const Value: TsgExpProgress);
    procedure SetExpRestoreDC(const Value: TsgExpRestoreDC);
    procedure SetExpSaveDC(const Value: TsgExpSaveDC);
    procedure SetExpSetColor(const Value: TsgExpSetColor);
    procedure SetExpSetFont(const Value: TsgExpSetFont);
    procedure SetExpSetPenGeometric(const Value: TsgExpSetPenGeometric);
    procedure SetExpSetPenMiterLimit(const Value: TsgExpSetPenMiterLimit);
    procedure SetExpSetPenWidth(const Value: TsgExpSetPenWidth);
    procedure SetExpSetStyle(const Value: TsgExpSetStyle);
    procedure SetExpText(const Value: TsgExpText);
    procedure SetExpTextOut(const Value: TsgExpTextOut);
{$IFDEF SG_BTI}
    procedure SetUseFillStyleIndex(const AValue: Boolean);
{$ENDIF}
    procedure SetViewportRect(const Value: TRect);
    procedure UpdateContextSelectionMatrix;
    procedure SetIterator(const Value: Boolean);
    procedure SetLibraryGDIPlusExists(const Value: Boolean);
    function GetColorToLineWeightList: TStringList;
    function GetModeller: TsgModeller;
    procedure SetXrefTransparency(const Value: Single);
    procedure SetNullWidthExt(const Value: Double);
    function UseKeyColor(const AKey: TsgColorCAD;
      const AContextPenColor: TsgColorCAD; const ADrawPenColor: Integer): Boolean;
    procedure SetUseColorRGBInColorMap(const Value: Boolean);
    procedure SetRegenDelta(const Value: Double);
    procedure SetObjectSnapMaskEx(const Value: TObjectSnapState);
    procedure UpdateClientClipRect(const ARect: TRect);
  protected
    FAppInfo: TsgDWGAppInfo;
    FAddOnePixel: Boolean;
    FDraw: TsgCADIterate;
    FDrawExternlProps: TsgExternalProps;
    FDimensionsVisible: Boolean;
    FTextVisible: Boolean;
    FmmToPixelX: Double;
    FMsg: string;
    FClientClipRect: TRect;
    FColorPen: TColor;
    FPixelColor: TColor;
    FPrecision: Double;
    FUsePrecision: Boolean;
    FNormalsInternal: TPoint;
    FExportUseLayerPlotting: Boolean;
    FShowLineWeightExport: Boolean;
    FTmpClipRect: TRect;
    FTmpEntColor: TsgEntColors;
{$IFDEF SG_FIREMONKEY}
    FImagesCache: TObject;//TDictionary<TsgImageCacheItem,TBitmap>;
{$ENDIF}
{$IFDEF SG_OPENING_IN_THEADS}
    FThreadId: TThreadID;
{$ENDIF}
    class procedure RegisterDestroyNotification(const ANotify: TNotifyEvent);
    class procedure UnRegisterDestroyNotification(const ANotify: TNotifyEvent);

    //this function implements IInterface
    function _AddRef: Integer; {$IFNDEF SG_LINUX}stdcall;{$ELSE}cdecl;{$ENDIF}
    function _Release: Integer; {$IFNDEF SG_LINUX}stdcall;{$ELSE}cdecl;{$ENDIF}

    function GetPalColorMode(const AC1, AC2: Integer): Integer;
    function CloseRegionSegment(Entity: TsgDXFEntity; AViewPortRegion: TRegion): Integer;
    procedure DrawModPolyline(const APoly: TsgModMeshPolyline; const AParams: TsgBrepModIterateParams);
    procedure DrawModEdge(const AEdge: TsgModTopoEdge; const AParams: TsgBrepModIterateParams);
    procedure DrawModIsoLines(const AFace: TsgModTopoFace; const AParams: TsgBrepModIterateParams);

    procedure ApplyLineWeightFactor(var ALineWeight: Double); virtual;
    procedure ApplyLineWeightFactorBase(var ALineWeight: Double);
    procedure ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD); virtual;
    procedure AddSelectionMatrix(const APointer: Pointer);
    procedure ApplyScale(const ARect: TRect); overload;
    procedure ApplyScale(const ARectLeft, ARectTop, ARectRight, ARectBottom: Double); overload;
    function ApplyAlternateWhite(var AColor: TColor): Boolean;
    procedure ApplyMMToPixelX(var AWeight: Double);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    procedure DrawPaperSpace;
    function CreateDefaultViewPort(const AHandle: UInt64): TsgDXFViewport;
    function CreateDefaultVPort(const Handle: UInt64): TsgDXFVPort;
    procedure CalcDottedSingPts(AEntity: TsgDXFEntity;
      AEntityLines: sgLines.TsgLines;
      ConvertMatrixCurrentInsertToWCS: TFMatrix;
      ADottedPoints: TFPointList);
    function CheckActiveVPort: Boolean;
    procedure CheckColorToLineWeight;
    procedure ClearMatrices(const ARect: TRect);
    function CreateConverter: TsgDXFConverter; virtual;
    procedure CreateRegionStack;
    function CurrXRef: TsgDXFXref;
    procedure GetPixelSize(ANumberOfPixels: Double; var Adx, Ady: Double;
      const AScaled: Boolean);
    procedure DoAfterRotate; dynamic;
    procedure DoBeforeRotate; dynamic;
    procedure DoCreateExternalRegion(const ACombineRegion: TObject);
    function DoDraw(Entity: TsgDXFEntity): Integer; virtual;
    function DoFinish(Entity: TsgDXFEntity): Integer; virtual;
    procedure DoDrawAfter;
    procedure DoDrawBefore({const ACanvas: TCanvas; const ARect: TRect});
    procedure DoIterateAfter;
    procedure DoIterateBefore;
    procedure DrawOnSelMatrixLine(const Sender: TObject);
    {$IFDEF SG_DELPHI_VCL}
    procedure DoMetafileExport(const MC: TMetafileCanvas; const ARect: TRect);
    {$ENDIF}
    procedure DoMatrixChanged; dynamic;
    procedure DoOnProgress(Stage: TProgressStage; Done, Count: Integer); virtual;
    procedure DoUpdateEvent(Sender: TObject);
    procedure Draw3DAxes;  //cadimage
    procedure Draw(Canvas: TCanvas; const Rect: TRect); override;
    {$IFDEF SG_DELPHI_VCL}
    procedure DrawOnMetafile(const AMetafile: TMetafile;
      const AUseLogicMetrix: Boolean; const ADrawProc: TsgObjProcDraw = nil;
      const AMetafileRegionScale: Double = 1);
    {$ENDIF}
    function DoDrawBackground(const ARect: TRect): Integer; virtual;
    function DrawEntity(Entity: TsgDXFEntity): Integer;
    function DrawSnapPoints(const APoints: TFPointList;
      const AMode: TObjectSnapState; const ASnapEntitry: TsgDXFEntity = nil): Integer;
    function DrawComplexEntity(Sender: TObject): Integer;
    function DrawMesh(Sender: TObject): Integer;
    procedure DrawImageGDI(Sender: TObject; const AGraphic: TGraphic;
      const APoints: TsgPointsListHelper); virtual;
    function DrawInsert(Sender: TObject): Integer;
    procedure EnterComplexEntity(Sender: TObject);
    procedure EnterInsert(Sender: TObject);
    procedure EntityCreated(Sender: TObject);
    function EntVisible(const AEnt: TsgDXFEntity): Boolean;
    function EntVisibleByLayer(E: TsgDXFEntity; Ins: TsgDXFInsert;
      ARealVis: Boolean = False): Boolean;
    procedure ExpEntityProcBegin(const AEntity: TsgDXFEntity);
    procedure ExpEntityProcEnd(const AEntity: TsgDXFEntity);
    procedure ExpBitBlt(const ASender: TObject; const X, Y, Width,
      Height: Integer; const ABitMap: TBitmap; const SX: Integer = 0;
      const SY: Integer = 0; const ARop: Cardinal = SRCCOPY);
    function ExpClipRegionInternal(const ASender: TObject; const ARegion: TRegion): Boolean;
    procedure ExpFillRectInternal(const Sender: TObject; const ARect: TRect);
    function ExpSaveDCInternal(const ASender: TObject): Integer;
    procedure ExpRestoreDCInternal(const ASender: TObject; const AIndex: Integer);
    function GetPlotSettingsRect: TRect;
    function GetEntityColors(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TsgEntColors; virtual;
    function IsDisableSnapMatrix: Boolean;
    function IsImageWithColorsRGB: Boolean; virtual;
    function IsNotScaledLType: Boolean;
    function IsNoWidth: Boolean;
    procedure FillBoxForCoordsCalc(ACoord: Integer; var ABox: TFRect); virtual;
    procedure Fill2Counts(AMax: Integer);
    procedure InitializeVersionData(const AIsTrial: Boolean; const AMessage: string);
    function IsDrawOnBitMap(P: PsgCADIterate): Boolean; overload;{$IFDEF  SG_INLINE} inline;{$ENDIF}
    function IsDrawOnBitMap: Boolean; overload;
    function IsDrawOnCanvas: Boolean;
    function IsAutocadLType: Boolean; virtual;
    function IsAutocadFormat: Boolean; virtual;
    function IsRotatedTextInternal(const AText: TsgDXFText; AHeight: Single;
      const ADrawMatrix: TFMatrix): Boolean;
    function IsRotatedText(const AText: TsgDXFText; AHeight: Single; const ADrawMatrix: TFMatrix): Boolean; virtual;
    function IsRectInRegion(ARect: TRect): Boolean;
    function IsStopLoading: Boolean;
    procedure GetAngles(const AImageEnt: TsgDXFImageEnt; var Angle1, Angle2: Double;
      const Abs: Boolean = False);
    function GetCanvas: TCanvas;
    function GetConverterParams: TsgConverterParams; virtual;
    function GetDefaultView: TsgDefaultView; virtual;
    function GetDefaultImageSize: TFRect;
    function GetEmpty: Boolean; override;
    {$IFDEF SGFPC}
    function GetTransparent: Boolean; override;
    {$ENDIF}
    function GetFPoint(const P: TFPoint): TFPoint;
    function GetHeight: Integer; override;
    function GetImageType: TsgExportFormat; virtual;
    function GetLineTypeScale: Double;
    function GetMillimetres: Boolean; virtual;
    function GetGDIPlusColor(const AColor: TColor): Cardinal;
    function GetOnInsertMode: TsgDXFInsert;
    function GetPalette: HPalette; override;
    function GetPolygonFillingMode: Integer; virtual;
    function GetRect(const ARect: TFRect): TRect;
    function GetRealImageMatrix: TFMatrix;
    function GetEntLineWeight(const AEntity, AInsert: TsgDXFEntity): Double;
    function GetLineWeightFactor: Double; virtual;
    function GetLineWeightFactorBase: Double;
    function GetIsShowBackground: Boolean;
    function GetIsShowLineWeight: Boolean;// virtual;
    function GetStretchBltModeInt: Integer;
    function GetWidth: Integer; override;
    function GetDefSolidRotView: TsgDXFViewDirection; virtual;
    function GetDrawPenColor: TColor;
    function GetDrawBackgroundColor: TColor;
    function HasClip(AInsert: TsgDXFInsert; out AFilter: TObject): Boolean; virtual;
    function Load2DPointToPoints(const AList: TF2DPointList): Integer;
    procedure Progress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string); override;
    procedure ClearRegenData;
    procedure Regenerate(const AEntities: TsgObjectList = nil);
    procedure ResetRegenData;
    procedure RegenerateInternal(ABlock: TsgDXFBlock;
      const AEntities: TsgObjectList; const ADelta: Double);
    function RegenerateFinishProc(AEntity: TsgDXFEntity): Integer;
    function RegenerateProc(AEntity: TsgDXFEntity): Integer;
    procedure RegenerateWithParts(AParts: Integer);
    procedure RestoreDrawParams;
    procedure RotateImageEnt();
    procedure SaveDrawParams;
    procedure SetSelectClipRegion(ARegion: TRegion);
    procedure SetClip(AInsert: TsgDXFInsert; AFilter: TObject); virtual;
    procedure SetCurrentLayout(const ALayout: TsgDXFLayout); virtual;
    procedure SetDefaultViewPort(AConverter: TsgDXFCOnverter);
    procedure SetDefaultPlotSettings(const ACheckSize: Boolean = False;
      const ANeedSaveSize: Boolean = False); virtual;
    procedure SetDrawingMatrixEx(AValue: TFMatrix);
    procedure SetEntityVisibility(ACADFile: TObject);
    procedure SetExtentsParameters(const ARect: TFRect; const AIs3DExtents: Boolean); virtual;
    function SetLineWeigth(const AEntity: TsgDXFEntity;
      const ALineWeightScaled: Boolean; AMetric: Double = 1.0): Integer;
    procedure SetSHXFontsProc(const AValue: TsgSHXFontsProc);//for internal use
    procedure SetLineWeightScale(const Value: TsgFloat);
    procedure SetOnInsertMode(const AIns: TsgDXFInsert);
    procedure SetHeight(Value: Integer); override;
    procedure SetMillimetres(Value: Boolean); virtual;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetPenWidth(AWidth: Single);
    procedure SetPenStyle(const AStylePen: DWORD; const AMiterLimit: Single);
    procedure SetPenGeometric(AStyle: DWORD);
    procedure SetRegion(const ASender: TObject; const ARegion: TRegion); virtual;
    procedure SetStretchBltModeInt(const AMode: Integer);
    procedure SetWidth(Value: Integer); override;
    {$IFDEF SGFPC}
    procedure SetTransparent(Value: Boolean); override;
    {$ENDIF}
    procedure Set3DRotDef(const ABox: TFRect; var ANeed2DViewByDefault: Boolean); virtual;
    procedure SetCanvas(const ACanvas: TCanvas);
    procedure TransformToUCS(var APoint: TFPoint);
    procedure TransformToWorldCS(var APoint: TFPoint);
    procedure SetGuid(const AGuid: TGUID);
    //* for B-Rep formats (STEP, IGES, SldAsm, IAM) only
    function GetUnresolvedExternalLinkNames(const AFileNames: TStrings): Integer; virtual;
    property DefaultView: TsgDefaultView read GetDefaultView;
    property FColor: TColor read FDraw.Color write FDraw.Color;
    property FHCoef: Single read FDraw.YScale write FDraw.YScale;
    property IsRotToView: Boolean read FRotToView;
    property FAngle: Single read FDraw.Angle write FDraw.Angle;
    property FileInfo: string read FFileInfo write FFileInfo;
    property ObjectSnapMask: TObjectSnapState read FObjectSnapMask write SetObjectSnapMaskEx;
    property OnLayoutBeforeChange: TNotifyEvent read FOnLayoutBeforeChange write FOnLayoutBeforeChange;
    property OnMatrixChanged: TNotifyEvent read FOnMatrixChanged write FOnMatrixChanged;
    property OnMetafileExport: TsgObjMetafileExport read FOnMetafileExport
      write FOnMetafileExport;//set before export
    property RotCount: Integer read FRotCount;
    property LibraryGDIPlusExists: Boolean read FLibraryGDIPlusExists write SetLibraryGDIPlusExists;
    property RegionStack: TsgRegionStack read FRegionStack;
    property Context: TsgContext read FContext;
    property CanvasRegion: TRegion read FCanvasRegion;
    property Stream: TMemoryStream read FStream write FStream;
    property SHXFontsProc: TsgSHXFontsProc read FSHXFontsProc{ write FSHXFontsProc};
    property ExternalRegion: TsgDXFEntity read FExternalRegion write SetExternalRegion;//for internal using
    property MainConstants: TsgConstantsCustom read FConstants write FConstants;
{$IFDEF SG_BTI}
    property UseFillStyleIndex: Boolean read FUseFillStyleIndex write SetUseFillStyleIndex;
{$ENDIF}
    property AfterDrawNotifications: TsgNotifications read FAfterDrawNotifications;
    property BeforeDrawNotifications: TsgNotifications read FBeforeDrawNotifications;
    property AfterIterateNotifications: TsgNotifications read FAfterIterateNotifications;
    property BeforeIterateNotifications: TsgNotifications read FBeforeIterateNotifications;

    property ViewportRect: TRect read FViewportRect write SetViewportRect;
    property XrefTransparency: Single read FXrefTransparency write SetXrefTransparency;

    property RegenDelta: Double read FRegenDelta write SetRegenDelta;

    property FCanvas: TCanvas read GetCanvas write SetCanvas;
    property OnDrawBackground: TsgCADEntityProc read FOnDrawBackground write FOnDrawBackground;
    property SnapMatrix: TsgSnapMatrix read FSnapMatrix write FSnapMatrix;
  public
    constructor Create; override;
    destructor Destroy; override;

    {$IFDEF SGFPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFDEF SG_NON_WIN_PLATFORM}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): HResult; {$IFDEF SGDEL_6}override;{$ENDIF} stdcall;
    {$ENDIF}

    function AbsSize: TF2DPoint;
    function AbsHeight: Extended;
    function AbsWidth: Extended;
    procedure AddScaledDXF(ACADFile: TObject; const AName: string; APos, AScale: TFPoint; Rotation: Single);
    function AddScaledDXFEx(ACADFile: TObject; const AName: string;
      APos, AScale: TFPoint; Rotation: Single;
      const AddXrefLayers: Boolean = True; const ADoExtends: Boolean = True): TsgDXFInsert;
    procedure Assign(Source: TPersistent); override; //cadimage
    function CommonToInternalUnits(Pt: TFPoint): TFPoint; virtual;
{$IFDEF SGDEL_4}
{$IFDEF SG_DELPHI_VCL}
    function ExportToMetafile(AWidth,AHeight: Integer): TMetafile; overload;
    function ExportToMetafile(MaxDim: Integer): TMetafile; overload;
    function ExportToMetafile(var OffsetX, OffsetY, UnitSize: TsgFloat): TMetafile; overload;
{$ENDIF}
{$ELSE}
    procedure ExportToMetafile(const FileName: string; AWidth,AHeight: Integer);
{$ENDIF}
    procedure DrawRect(DC: HDC; SourceRect: TFRect; DestRect: TRect);
    procedure DrawRectInt(Rect: TRect; APrecision: Double = -1);
    procedure GetExtents;
{$IFDEF SGDEL_4}
    function GetCADCoords(const AXScaled, AYScaled: Extended): TFPoint; overload;
    function GetCADCoords(const AXScaled, AYScaled: Extended; var CoordsInUCS: TFPoint): TFPoint; overload;
{$ELSE}
    function GetCADCoords(const AXScaled, AYScaled: Extended): TFPoint;
{$ENDIF}
    function GetEntityColor(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TColor;
    function GetDrawingExtents: TFRect;
    function GetPoint(const P: TFPoint): TPoint;
    function GetPointUCS(P: TFPoint): TPoint;
    function GetRealImagePoint(const P: TFPoint): TFPoint;
    function GetSnapData(const P: TPoint): PSnapData;
    function GetObjectSnapMask: TObjectSnapState;
    function GetExternalFiles(const AList: TStringList;
      const AMode: TsgExternalFilesModes = [efAddXref, efAddImageDef
      {$IFDEF DATA_LINK}, efDataLink{$ENDIF}]): Boolean;
    procedure SetObjectSnapMask(const AValue: TObjectSnapState;
      const AUpdate: Boolean = True);
    function InternalToCommonUnits(Pt: TFPoint): TFPoint; virtual;
    procedure LoadFromClipboardFormat({$IFNDEF SGFPC}Fmt: Word; Data: THandle; Pal: HPALETTE{$ELSE}FormatID: TClipboardFormat{$ENDIF}); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(S: TStream); override;
    function Measurement: TsgMeasurement; virtual;
    procedure RefreshCurrentLayout;
    procedure ResetDrawingBox;

    function Rotate(const APitch, ATurn, ARoll: Extended): Integer; overload;
    procedure Rotate(Axis: TsgAxes; Angle: Extended); overload;
    procedure SetDrawMatrix(const AMatrix: TFMatrix);
    procedure ScaleDrawMatrix(ASx, ASy, ASz: Double);
    procedure TranslateDrawMatrix(ADx, ADy, ADz: Double);

    procedure RotDefault;
    procedure RotToView(const A3DView: TsgDXFViewDirection);
    procedure SaveToClipboardFormat({$IFNDEF SGFPC}var Fmt: Word; var Data: THandle; var Pal: HPALETTE{$ELSE}FormatID: TClipboardFormat{$ENDIF}); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(S: TStream); override;
    procedure SetClippingRect(Value: PFRect);// not documented, only for internal using (!)
    procedure SetClippingRectExt(const AValue: TFRect);// not documented, only for internal using (!)
    procedure SetDefaultViewModePlotSettings; virtual;
    procedure StopLoading; virtual;

//IsgXMLObject
    function GetNodeName: string;
    function ToNode(const AParentNode: TsgNode;
      const AParams: TsgXMLParams): TsgNode;
    procedure FromNode(const ANode: TsgNodeSample);
    procedure GetXMLIdsChangingHiding(const AChangeList, AHideList: TsgIntegerList;
      AProgID: TsgProgID);
//

    property CustomDraw: Boolean read FCustomDraw write FCustomDraw;

    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property PlotWindowAreaColor: TColor read FPlotWindowAreaColor write SetPlotWindowAreaColor;
    property Blocks [Index: Integer]: TsgDXFBlock read GetBlock;
    property BorderSize: TsgBorderSize read FBorderSize write SetBorderSize;
    property BorderType: TsgBorderType read FBorderType write SetBorderType;
    property CBFormat: Word read FCBFormat write FCBFormat;
    property Center: TFPoint read FCenter;
    property Clipping: TsgClipping read GetClipping write SetClipping;// not documented, only for internal using (!)
    property ClippingRect: PFRect read FClippingRect write SetClippingRect;// not documented, only for internal using (!)
    property ColorToLineWeight: TStringList read GetColorToLineWeightList;
    property Converter: TsgDXFConverter read FConverter;
    property CurrentLayout: TsgDXFLayout read FCurrentLayout write SetCurrentLayout;
    property DefaultColor: TColor read GetDefaultColor write SetDefaultColor;
    property DrawingBox: TFRect read FDrawingBox write SetDrawingBox;
    property DrawMode: TsgDXFDrawMode read FDrawMode write SetDrawMode;
    property Entities [Index: Integer]: TsgDXFEntity read GetEntities;
    property Extents: TFRect read FExtents write SetExtents;
    property FileName: string read FFileName write FFileName;
    property IsDraw3DAxes: Boolean read FIsDraw3DAxes write FIsDraw3DAxes;
    property IsLoading: Boolean read GetIsLoading;
    property IsPlotting: Boolean read FIsPlotting write FIsPlotting;
    property IsProcessMessages: Boolean read FIsProcessMessages write FIsProcessMessages;
    property IsDrawingRotated: Boolean read GetIsDrawingRotated;
    property IsShowBackground: Boolean read GetIsShowBackground write SetIsShowBackground;
    property IsShowLineWeight: Boolean read GetIsShowLineWeight write SetIsShowLineWeight;
    property IsCurLayoutShowPlot: Boolean read GetIsCurLayoutShowPlot;
    property IsWithoutBorder: Boolean read FIsWithoutBorder write SetIsWithoutBorder;
    property Layouts[Index: Integer]: TsgDXFLayout read GetLayout;
    property LayoutsCount: Integer read GetLayoutsCount;
    property LineScaled: Boolean read GetIsShowLineWeight write SetIsShowLineWeight;//FI:C110 ignore //old version
    property LineWeightScale: TsgFloat read GetLineWeightScale write SetLineWeightScale;
    property LockLayoutChangeEvent: Boolean read FLockLayoutChangeEvent;
    property Modeller: TsgModeller read GetModeller;
    property TTFMode: TsgTTFMode read FTTFMode write SetTTFMode;
    property Guid: TGUID read FGuid;
    property Millimetres: Boolean read GetMillimetres write SetMillimetres;
    property NullWidth: Integer read GetNullWidth write SetNullWidth;
    property NullWidthExt: Double read FNullWidth write SetNullWidthExt;
    property AppInfo: TsgDWGAppInfo read FAppInfo;
    property UseColorRGBInColorMap: Boolean read FUseColorRGBInColorMap write SetUseColorRGBInColorMap;

    property OnBeforeRotate: TNotifyEvent read FOnBeforeRotate write FOnBeforeRotate;
    property OnAfterRotate: TNotifyEvent read FOnAfterRotate write FOnAfterRotate;
    property OnAfterDraw: TNotifyEvent read FOnAfterDraw write SetOnAfterDraw;
    property OnBeforeDraw: TNotifyEvent read FOnBeforeDraw write SetOnBeforeDraw;
    property OnLayoutChange: TNotifyEvent read FOnLayoutChange write FOnLayoutChange;
    property OnDrawCustomEntity: TNotifyEvent read FOnDrawCustomEntity write FOnDrawCustomEntity;
    property PureExtents: TFRect read FPureExtents;
    property RegenerateArcs: Boolean read GetRegenerateArcs write SetRegenerateArcs;
    property ResizeDisproportionateDrawing: Boolean read FResizeDisproportionateDrawing
               write FResizeDisproportionateDrawing;
    property Scale: TFPoint read GetScale;
{Transferred from the protected visibility scope to the public visibility scope for internal use in "Editor" demo.
start}
    property SelectionMatrix: TsgSelectionMatrix read FSelectionMatrix write SetSelectionMatrix;
    property GetMatrixMode: TsgGetSelectionMatrixModeProc read FGetSelectionMatrixModeProc;
    property SetMatrixMode: TsgSetSelectionMatrixModeProc read FSetSelectionMatrixModeProc;
{end}
    property ShowImages: Boolean read FShowImages write SetShowImages;
    property Stretch: Boolean read FStretch write SetStretch;
    property DeviceDPI: TPoint read FDeviceDPI write FDeviceDPI;
    property UseWinEllipse: Boolean read FUseWinEllipse write SetUseWinEllipse;
    property ViewRectangle: TFRect read FViewRectangle write FViewRectangle;
    property DimensionsVisible: Boolean read FDimensionsVisible write SetDimensionsVisible;
    property DrawMatrix: TFMatrix read FDraw.Matrix;
    property TextVisible: Boolean read FTextVisible write SetTextVisible;
    property OwnSource: Boolean read GetOwnSource write SetOwnSource;
    property Offset: TFPoint read FOffset write SetOffset;
    property CachingRasterMode: TsgCachingRasterMode read FCachingRasterMode write FCachingRasterMode;
    property UsePlotSetting: Boolean read FUsePlotSetting write SetUsePlotSetting;
    property ShowPlotSetting: Boolean read FShowPlotSetting write SetShowPlotSetting;
    property ShowPlotForModel: Boolean read FShowPlotForModel write SetShowPlotForModel;
    property ShowPlotFrame: Boolean read FShowPlotFrame write SetShowPlotFrame;
    property ShowPlotMargins: Boolean read FShowPlotMargins write SetShowPlotMargins;
    property Printing: TsgPrintMode read FPrinting write SetPrinting;

    //Events for export in an old version. Property ExpProcs is needed
    property ExpPixel: TsgExpPixel read GetExpPixel write SetExpPixel;
    property ExpArc: TsgExpArc read GetExpArc write SetExpArc;
    property ExpPolyline: TsgExpPolyline read GetExpPolyline write SetExpPolyline;
    property ExpPolygon: TsgExpPolygon read GetExpPolygon write SetExpPolygon;
    property ExpPolyPolyline: TsgExpPolyPolyline read GetExpPolyPolyline write SetExpPolyPolyline;
    property ExpPolyPolygon: TsgExpPolyPolygon read GetExpPolyPolygon write SetExpPolyPolygon;
    property ExpText: TsgExpText read GetExpText write SetExpText;
    property ExpTextOut: TsgExpTextOut read GetExpTextOut write SetExpTextOut;
    property ExpSetFont: TsgExpSetFont read GetExpSetFont write SetExpSetFont;
    property ExpSetStyle: TsgExpSetStyle read GetExpSetStyle write SetExpSetStyle;
    property ExpFillRgn: TsgExpFillRgn read GetExpFillRgn write SetExpFillRgn;
    property ExpClipRgn: TsgExpClipRgn read GetExpClipRgn write SetExpClipRgn;
    property ExpCloseFigure: TsgExpCloseFigure read GetExpCloseFigure write SetExpCloseFigure;
    property ExpImage: TsgExpImage read GetExpImage write SetExpImage;
    property ExpImageUV: TsgExpImageUV read GetExpImageUV write SetExpImageUV;
    property ExpSaveDC: TsgExpSaveDC read GetExpSaveDC write SetExpSaveDC;
    property ExpRestoreDC: TsgExpRestoreDC read GetExpRestoreDC write SetExpRestoreDC;
    property ExpProgress: TsgExpProgress read GetExpProgress write SetExpProgress;
    property ExpSetColor: TsgExpSetColor read GetExpSetColor write SetExpSetColor;
    property ExpSetPenWidth: TsgExpSetPenWidth read GetExpSetPenWidth write SetExpSetPenWidth;
    property ExpSetPenMiterLimit: TsgExpSetPenMiterLimit read GetExpSetPenMiterLimit write SetExpSetPenMiterLimit;
    property ExpSetPenGeometric: TsgExpSetPenGeometric read GetExpSetPenGeometric write SetExpSetPenGeometric;
    property ExpAnnotation: TsgExpAnnotation read GetExpAnnotation write SetExpAnnotation;

    property ExpProcs: TsgProxyExport read FExpProcs;

    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch default 1;
    property mmToPixelX: Double read FmmToPixelX write FmmToPixelX;
    property IsIterator: Boolean read GetIterator write SetIterator default False;
    property OnGetBox: TsgGetBox read FOnGetBox write FOnGetBox;
    property TmpPlotWindow: TsgDXFPlotSettings read FTmpPlotWindow
      write SetTmpPlotWindow;
    //for vectors format(CGM, SVG, PLT etc)
    property AlternateWhite: Boolean read GetAlternateWhite
      write SetAlternateWhite;
    property AlternateWhiteColor: TColor read GetAlternateWhiteColor
      write SetAlternateWhiteColor;
  end;

  TsgCadImageDyDimension = class(TsgCADImage)
  private
    FDimByDrawing: TsgDXFDimension;
    FTextByDrawing: TsgDXFText;
    FScale: Double;
  public
    constructor Create; override;
    procedure StringToPolylinesList(const AText: string;
      const APolylines: TsgObjectList);
    property DimByDrawing: TsgDXFDimension read FDimByDrawing;
    property TetxByDrawing: TsgDXFText read FTextByDrawing;
  end;

  TsgVectorImageCustom = class(TsgCADImage)
  protected
    procedure ApplyLineWeightFactor(var ALineWeight: Double); override;
    function GetLineWeightFactor: Double; override;
    procedure SetDefaultPlotSettings(const ACheckSize: Boolean = False;
      const ANeedSaveSize: Boolean = False); override;
    function IsImageWithColorsRGB: Boolean; override;
  public
    constructor Create; override;
    procedure SetDefaultViewModePlotSettings; override;
  end;

  TsgVectorImage = class(TsgVectorImageCustom)
  public
    procedure ConvertToBitmap(const ABitmap: TBitmap); virtual; //for internal using
  end;

  TsgVectorImage3D = class(TsgVectorImageCustom)
  end;

  TsgVectorImageWithModeller = class(TsgVectorImage3D)
  protected
    function GetBrepGroupClass: TClass; virtual;
    function GetUnNamedPart(ACompound: TsgModPartCompound;
      const AIsRoot: Boolean = False): string;
    procedure AddAttrib(AInsert: TsgDXFInsert; ATag, AText: string);
    procedure AddInfo(const AInsert: TsgDXFInsert;
      const ATag, AText: string; const AMode: Integer = 3);
    procedure ExtractPartCompound(ACompound: TsgModPartCompound;
      ACollection: TsgCollection; ABlock: TsgDXFBlock; const ALayer: TsgDXFLayer);
    procedure UpdateFromModeller(const ALayer: TsgDXFLayer);
  public
    constructor Create; override;
    class function CreateFromModeller(const AModeller: TsgModeller): TsgCADImage;
  end;

  TsgVectorImageWithAltWhite = class(TsgVectorImage)
  protected
    FAlternateWhite: Boolean;
    FAlternateWhiteColor: TColor;
    function GetAlternateWhite: Boolean; override;
    function GetAlternateWhiteColor: TColor; override;
    procedure SetAlternateWhite(const AValue: Boolean); override;
    procedure SetAlternateWhiteColor(const AValue: TColor); override;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TsgVectorImageWithSave = class(TsgVectorImageWithAltWhite)
  public
    function CanSaveToStream: Boolean; virtual;
    function SaveToStream(const AStream: TStream): Integer; reintroduce; virtual;
  end;

  TsgImageWithHandles = class(TsgCADImage)   // dwg, dxf
  protected
    function GetConverterParams: TsgConverterParams; override;
    function IsAutocadFormat: Boolean; override;
  public
    constructor Create; override;
    procedure SetDefaultViewModePlotSettings; override;
  end;

  IsgChange = interface
    [cnstGUID_Change]
    procedure _BeginBlock(const AEnt: TsgDXFEntity; const AInternal: Boolean);
    procedure _BeginEnt(const AEnt: TsgDXFEntity; const AInternal: Boolean);
    procedure _BeginList(const AList: TList; const ACount: Integer; const AInternal: Boolean);
    procedure _Changed;
    procedure _EndBlock(const AEnt: TsgDXFEntity);
    procedure _EndEnt(const AEnt: TsgDXFEntity);
    procedure _EndList(const AList: TList; const ACount: Integer);
    procedure _ClearStack;
    function _IsStackEmpty: Boolean;
    function _IsEntUsed(const AEnt: TsgDXFEntity): Boolean;
  end;

  function GetCADFile(const AFileName: string): TsgCADImage;
{$IFNDEF SG_FIREMONKEY}
  procedure RotateBitmap(Bitmap: TBitmap; Angle: Double; BackColor: TColor);
  procedure RotatePicture(Picture: TPicture; Angle: Double; BackColor: TColor);
{$ENDIF}
  function IsDrawGDI(var AAngleX, AAngleY: Double): Boolean;

{$IFNDEF BCB}
threadvar
{$ELSE}
var
{$ENDIF}
  Drawing: TsgCADImage;
var
  MinDXFSize: Integer;
{$IFNDEF SG_OPENING_IN_THEADS}
{$IFNDEF BCB}
threadvar
  Loading: TsgCADImage;
{$ELSE}
var
  Loading: TsgCADImage;
{$ENDIF}
{$ENDIF}


function PLibraryGDIPlusExists: Boolean;
function ApplyBitmapSmooth(const ABitmap: TBitmap): Boolean;

{$IFDEF SG_USEGDIPLUS}
type
  TsgGDIPlusOpimization = (opDefault, opSpeed, opQuality);
const
  cnstImageQuality: TsgGDIPlusOpimization = opSpeed;
{$ENDIF}

implementation

uses
  Math, sgBitmap, sgIterator//, sgEncMgr, sgEncoding
{$IFDEF SG_FIREMONKEY}
  , System.Generics.Collections
{$ENDIF}
  ;

const
  cnstMaxTextHeight = 8192;// Solution of the big fonts GDI-problem
  cnstMaxCapacity  = 1000000;
{$IFNDEF SG_NON_WIN_PLATFORM}
  cnstDefaultTextAlign = (TA_LEFT or TA_TOP) and TA_NOUPDATECP;
{$ENDIF}
  cnstDefaultCapacity = 100000;
  cnstDefaultLineWeightFactor = 1.0;
  cnstCADLineWeightMax =  8.875;
  cnstHordOnScreenByArc = 5;
  cnstRegenDeltaRatio = 1.6;
{$IFNDEF SG_NON_WIN_PLATFORM}
  cnstPenStyle_ENDCAP_FLAT = PS_ENDCAP_FLAT or PS_JOIN_BEVEL;
{$ENDIF}
  cnstPenStyle_ENDCAP_JOIN_ROUND = PS_ENDCAP_ROUND or PS_JOIN_ROUND;
{$IFDEF SG_LINUX}
  RGN_ERROR = {$IFDEF SGFPC}Region_Error{$ELSE}0{$ENDIF};
{$ENDIF}

type
  PTriVertex = ^TTriVertex;
  TTriVertex = record
    X,Y: Integer;
    Red,Green,Blue,Alpha: Word;
  end;
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFDimensionStyleAccess = class(TsgDXFDimensionStyle);
  TsgDXFDimensionAccess = class(TsgDXFDimension);
  TsgCADGradientPolygonAccess = class(TsgCADGradientPolygon);
  TsgCADWipeoutAccess = class(TsgCADWipeout);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgDXFPointAccess = class(TsgDXFPoint);
  TsgDXFCircleAccess = class(TsgDXFCircle);
  TsgCADBasePolyLineAccess = class(TsgCADBasePolyline);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgDXFViewportAccess = class(TsgDXFViewport);
  TsgDXFOle2FrameAccess = class(TsgDXFOle2Frame);
{$IFNDEF SG_NON_WIN_PLATFORM}
  TsgBitmapAccess = class(TsgBitMap);
{$ENDIF}
  TsgDXFTextAccess = class(TsgDXFText);
  TsgDXF3dFaceAccess = class(TsgDXF3dFace);
  TsgDXFSolidAccess = class(TsgDXFSolid);
  TsgDXFRayAccess = class(TsgDXFRay);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  TsgDXFStyleAccess = class(TsgDXFStyle);
  TsgCADSpatialFilterAccess = class(TsgCADSpatialFilter);
  TsgTextGlyphAccess = class(TsgTextGlyph);
{$IFDEF USE_SG_AGG2D}
  TFPCustomFontAccess = class(TFPCustomFont);
{$ENDIF}
  TsgContextAccess = class(TsgContext);
  TsgDXFMeshAccess = class(TsgDXFMesh);
  TBitmap = {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap;

  TsgXrefStackItem = class
    XRef: TObject;
    LwDisplay: Byte;
  end;

  TCustomDrawPoly = function(DC: HDC; const Points; Count: Integer): BOOL;{$IFNDEF SG_NON_WIN_PLATFORM} stdcall;{$ENDIF}

  TsgRegenBlockItem = class
  public
    PixelSize: Double;
  end;

  TsgRegenStackItem = class
  private
    FEntity: TsgDXFEntity;
    FMatrix: TFMatrix;
  public
    constructor Create(AEntity: TsgDXFEntity; AMatrix: TFMatrix);
    property Entity: TsgDXFEntity read FEntity;
    property Matrix: TFMatrix read FMatrix;
  end;

{$IFDEF SG_FIREMONKEY}
  TsgImageCacheItem = record
    BitmapHashCode: Integer;
    TransparentColor: Cardinal;
    DefaultColor: Cardinal;
    Mode: Integer;// flags: 1 - monochrome; 2 - transparent
  end;
{$ENDIF}

var
  AcadPal: HPalette;
  StdLines: TsgLines = nil;
  CADImageDestroyNotifications: TsgNotifyEventsList;

 { TsgRegenStackItem }
constructor TsgRegenStackItem.Create(AEntity: TsgDXFEntity; AMatrix: TFMatrix);
begin
  FEntity := AEntity;
  FMatrix := AMatrix;
end;

function Point(AX, AY: Integer): TPoint;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF SGDEL_XE2}
  Result := System.Types.Point(AX, AY);
  {$ELSE}
  {$IFDEF SGDEL_6}
  Result := Types.Point(AX, AY);
  {$ELSE}
  Result := Classes.Point(AX, AY);
  {$ENDIF}
{$ENDIF}
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF SGDEL_XE2}
  Result := System.Types.Rect(Left, Top, Right, Bottom);
  {$ELSE}
  {$IFDEF SGDEL_6}
  Result := Types.Rect(Left, Top, Right, Bottom);
  {$ELSE}
  Result := Classes.Rect(Left, Top, Right, Bottom);
  {$ENDIF}
{$ENDIF}
end;

function PLibraryGDIPlusExists: Boolean;
{$IFDEF SG_USEGDIPLUS}
{$IFNDEF SG_WINAPI_GDIPLUS}
// Use GDI +
var
  GdiPlus: THandle;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF SG_USEGDIPLUS}
{$IFNDEF SG_WINAPI_GDIPLUS}
  GdiPlus := 0;
  try
    GdiPlus := LoadLibrary('GdiPlus.dll');
  finally
    Result := GdiPlus <> 0;
    FreeLibrary(GdiPlus);
  end;
{$ELSE}
  Result := True;
{$ENDIF}
{$ELSE}
  Result := False;
{$ENDIF}
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
procedure CreateAcadPal;
var
  I: Integer;
  P: TMaxLogPalette;
  LP: TLogPalette absolute P;
begin
  P.palVersion := $300;
  P.palNumEntries := 216;
  for I:=0 to 215 do
  begin
    P.palPalEntry[I].peRed := I mod 6 * 51;
    P.palPalEntry[I].peGreen := I div 6 mod 6 * 51;
    P.palPalEntry[I].peBlue := I div 36 * 51;
    P.palPalEntry[I].peFlags := 0;
  end;
  AcadPal := CreatePalette(LP);
end;
{$ENDIF}

procedure InflateRegion(ARegion: TsgRegion; APoly: TsgIntegerList;
  const AFillingMode: Integer; const APixels: Integer);
{$IFNDEF SG_NON_WIN_PLATFORM}
var
  vRGN: TsgRegion;
  vData: PRgnData;
  vCount: Integer;
  I: Integer;
  vPRect: PRect;
  vTempReg: TsgRegion;
  vRectPoints: array[0..3] of TPoint;
begin
  vRGN := TsgRegion.Create;
  try
    vRGN.SetPolygon(APoly.List^, APoly.Count div 2, AFillingMode);
    vCount := GetRegionData(vRGN.Handle, 0, nil);
    vData := AllocMem(vCount);
    try
      GetRegionData(vRGN.Handle, vCount, vData);
      for I := 0 to vData.rdh.nCount - 1 do
      begin
        vPRect := PRect(@vData.Buffer[I * SizeOf(TRect)]);
        InflateRect(vPRect^, APixels, APixels);
        vPRect.Left := Max(vPRect.Left, 0);
        vPRect.Top := Max(vPRect.Top, 0);
        vTempReg := TsgRegion.Create;
        try
          vRectPoints[0] := vPRect^.TopLeft;
          vRectPoints[1] := Point(vPRect^.Right, vPRect^.Top);
          vRectPoints[2] := vPRect^.BottomRight;
          vRectPoints[3] := Point(vPRect^.Left, vPRect^.Bottom);
          vTempReg.SetPolygon(vRectPoints, 4, AFillingMode);
          ARegion.Union(vTempReg);
        finally
          vTempReg.Free;
        end;
      end;
    finally
      FreeMem(vData);
    end;
  finally
    vRGN.Free;
  end;
end;
{$ELSE}
begin
  //TODO: procedure InflateRegion for Firemonkey
end;
{$ENDIF}

function GetCADFile(const AFileName: string): TsgCADImage;
var
  vPicture: TPicture;
begin
  Result := nil;
  vPicture := TPicture.Create;
  try
    vPicture.LoadFromFile(AFileName);
    if vPicture.Graphic is TsgCADImage then
    begin
      Result := TsgCADImageClass(vPicture.Graphic.ClassType).Create;
      Result.Assign(vPicture.Graphic);
      Result.IsWithoutBorder := True;
    end;
  finally
    vPicture.Free;
  end;
end;

function GetPointOfRotateByCenter(const APoint, ACenter: TPoint; const Angle: Double): TPoint;
var
  vAngle, vDistance, vDX, vDY: Double;
  S, C: Extended;
begin
  vDX := APoint.X - ACenter.X;
  vDY := APoint.Y - ACenter.Y;
  vDistance := vDX * vDX + vDY * vDY;
  if vDistance > fAccuracy then
    vDistance := Sqrt(vDistance)
  else
    vDistance := 0;
  vAngle := (GetAngleByPoints(MakeFPoint(ACenter.X,ACenter.Y),
    MakeFPoint(APoint.X,APoint .Y), False, 0) + Angle) * fPiDividedBy180;
  SinCos(vAngle, S, C);
  Result.X := ACenter.X + Round(vDistance * C);
  Result.Y := ACenter.Y + Round(vDistance * S);
end;

procedure SetListCapacity(const AList: TsgBaseList; const ASetDefault: Boolean);
begin
  AList.Count := 0;
  if ASetDefault then
    AList.Capacity := cnstDefaultCapacity
  else
  begin
    if AList.Capacity > cnstMaxCapacity then
      AList.Capacity := cnstMaxCapacity;
  end;
end;

{$IFNDEF SG_FIREMONKEY}
procedure RotatePicture(Picture: TPicture; Angle: Double; BackColor: TColor);
var
  SG: TsgBitmap;
  vBitmap: TGraphic;
begin
  vBitmap := Picture.Graphic;
  if vBitmap is TsgBitmap then begin
    TsgBitmap(vBitmap).Rotate(Round(Angle));
    Exit;
  end;
  if not (vBitmap is Graphics.TBitmap) then Exit;
  SG := TsgBitmap.Create;
  try
    SG.Assign(vBitmap);
    SG.Rotate(Round(Angle));
    Picture.Graphic := SG;
  finally
    SG.Free;
  end;
end;
{$ENDIF}

function IsDrawGDI(var AAngleX, AAngleY: Double): Boolean;
begin
  if sgIsZero(AAngleX - 0) then
    AAngleX := 0;
  if sgIsZero(AAngleY - 90) then
    AAngleY := 90;
  Result := (AAngleX = 0) and (AAngleY = 90);
end;

{$IFNDEF SG_FIREMONKEY}
procedure RotateBitmap(Bitmap: Graphics.TBitmap; Angle: Double; BackColor: TColor);
const
  cnstMaxBmpHeight = 8191;
type
  TRGB = record
    B, G, R: Byte;
  end;
  pRGB = ^TRGB;
var
  x, y, W, H, v1, v2: Integer;
  Dest, Src: pRGB;
  VertArray: array[0..cnstMaxBmpHeight] of pByteArray;
  Bmp: Graphics.TBitmap;
begin
  Angle := Angle - Floor(Angle / 360) * 360;
  try
    while Angle < 0 do
    Angle := Angle + 360;
    Bitmap.PixelFormat := pf24Bit;
    Bmp := Graphics.TBitmap.Create;
    try
      Bmp.Assign(Bitmap);
      Bmp.FreeImage;
      W := Bitmap.Width - 1;
      H := Bitmap.Height - 1;
      if H > cnstMaxBmpHeight then
        H := cnstMaxBmpHeight;
      case Trunc(Angle) of
        90, 270:
        begin
          Bitmap.Width := H + 1;
          Bitmap.Height := W + 1;
          v1 := 0;
          v2 := 0;
          if Trunc(Angle) = 90 then
            v1 := H
          else
            v2 := W;
          for y := 0 to H do
            VertArray[y] := Bmp.ScanLine[Abs(v1 - y)];
          for x := 0 to W do
          begin
            Dest := Bitmap.ScanLine[x];
            for y := 0 to H do
            begin
              v1 := Abs(v2 - x)*3;
              with Dest^ do
              begin
                B := VertArray[y, v1];
                G := VertArray[y, v1+1];
                R := VertArray[y, v1+2];
              end;
              Inc(Dest);
            end;
          end
        end;
        180:
        begin
          for y := 0 to H do
          begin
            Dest := Bitmap.ScanLine[y];
	          Src := Bmp.ScanLine[H - y];
            Inc(Src, W);
            x := 0;
            while x <= W do
            begin
              Dest^ := Src^;
              Dec(Src);
              Inc(Dest);
              Inc(x);
            end;
          end;
        end;
      end;
    finally
      Bmp.Free;
    end;
    if not Bitmap.Modified then Bitmap.Modified := True;
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
end;
{$ENDIF}

procedure SetNewBox(const P1, P2, P3, P4: TPoint; var NewBox: TRect); overload;
begin
  ExpandRect(NewBox, P1);
  ExpandRect(NewBox, P2);
  ExpandRect(NewBox, P3);
  ExpandRect(NewBox, P4);
end;

procedure SetNewBox(const P1, P2, P3, P4: TPointF; var NewBox: TRectF); overload;
begin
  ExpandRect(NewBox, P1);
  ExpandRect(NewBox, P2);
  ExpandRect(NewBox, P3);
  ExpandRect(NewBox, P4);
end;

function ApplyBitmapSmooth(const ABitmap: TBitmap): Boolean;
{$IFDEF SG_USEGDIPLUS}
var
  vPts: array [0..3] of TGPPoint;
  vBitmap: TBitmap;
  vGraphics: TGPGraphics;
  vAttr: TGPImageAttributes;
  vTransparentColor: Cardinal;
  vW, vH: Integer;
  vGDIPImage: TGPImage;
{$ENDIF}
begin
{$IFDEF SG_USEGDIPLUS}
  vBitmap := TBitmap.Create;
  try
    vBitmap.PixelFormat := ABitmap.PixelFormat;
    vW := ABitmap.Width;
    vH := ABitmap.Height;
    SetSizeGraphic(vBitmap, vW, vH);
    vBitmap.TransparentMode := ABitmap.TransparentMode;
    ApplyBitmapTransparent(vBitmap, ABitmap.TransparentColor);
    FillChar(vPts, SizeOf(vPts), 0);
    vPts[0] := MakePoint(0, 0);
    vPts[1] := MakePoint(vW-1, 0);
    vPts[2] := MakePoint(0, vH-1);
    vGDIPImage := TGPBitmap.Create(ABitmap.Handle, ABitmap.Palette);
    try
      vGraphics := TGPGraphics.Create(vBitmap.Canvas.Handle);
      try
        vGraphics.SetCompositingMode(CompositingModeSourceOver);
        vGraphics.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        vGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);
        vGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
        vAttr := nil;
        try
          if ABitmap.Transparent then
          begin
            vAttr := TGPImageAttributes.Create;
            vTransparentColor := ColorRefToARGB(ABitmap.TransparentColor);
            vAttr.SetColorKey(vTransparentColor, vTransparentColor, ColorAdjustTypeBitmap);
          end;
          Result := vGraphics.DrawImage(vGDIPImage, @vPts[0], 3, 0, 0, vW, vH, {$IFDEF SG_WINAPI_GDIPLUS}UnitPixel, {$ENDIF}vAttr) <> OutOfMemory;
          if Result then
            ABitmap.Assign(vBitmap);
        finally
          vAttr.Free;
        end;
      finally
        vGraphics.Free;
      end;
    finally
      vGDIPImage.Free;
    end;
  finally
    vBitmap.Free;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
end;

type
  TsgText2dLinesCollection = class(TsgCustomPolyItemsCollection)
  private
    FAsPolyPolyline: Boolean;
  protected
    function DoAddPtDirrect(const P): Integer; override;
    function DoAddPt(const P): Integer; override;
    function GetAsPolyPolyline: Boolean; override;
  public
    procedure ChangeItemsCount(ADelta: Integer); override;
    property AsPolyPolyline: Boolean read FAsPolyPolyline write FAsPolyPolyline;
  end;

  TsgText2dLinesCollectionGDI = class(TsgText2dLinesCollection)
  protected
    FPoint: TPoint;
    function DoAddPtDirrect(const P): Integer; override;
  end;

  TsgModPolyItemsCollection = class(TsgText2dLinesCollectionGDI)
  protected
    function DoAddPt(const P): Integer; override;
    function UpdateTransformation(const AValue; {$IFDEF HAS_UNMANAGED_TYPEINFO}AType: Pointer{$ELSE}ASize: Integer{$ENDIF}): Boolean; override;
  end;

{ TsgText2dLinesCollection }

procedure TsgText2dLinesCollection.ChangeItemsCount(ADelta: Integer);
begin
  Poly.Count := Poly.Count + 2 * ADelta;
end;

function TsgText2dLinesCollection.DoAddPt(const P): Integer;
var
  Pt: TFPoint;
  M: PFMatrix;
begin
  M := PFMatrix(Transformation);
  Pt.X := TFPoint(P).X * M^.M[0, 0] + TFPoint(P).Y * M^.M[1, 0] + M^.M[3, 0];
  Pt.Y := TFPoint(P).X * M^.M[0, 1] + TFPoint(P).Y * M^.M[1, 1] + M^.M[3, 1];
  //Pt.Z := TFPoint(P).X * M^.M[0, 2] + TFPoint(P).Y * M^.M[1, 2] + M^.M[3, 2];
  Result := DoAddPtDirrect(Pt);
end;

function TsgText2dLinesCollection.DoAddPtDirrect(const P): Integer;
begin
  TsgPointsListHelper(Poly).AddPoint(Point(Round(TFPoint(P).X), Round(TFPoint(P).Y)));
  Result := inherited DoAddPtDirrect(P);
end;

function TsgText2dLinesCollection.GetAsPolyPolyline: Boolean;
begin
  Result := FAsPolyPolyline;
end;

{ TsgText2dLinesCollectionGDI }

function TsgText2dLinesCollectionGDI.DoAddPtDirrect(const P): Integer;
var
  Pt: TPoint;
begin
  Pt := Point(Round(TFPoint(P).X), Round(TFPoint(P).Y));
  if (Counts[Counts.Count - 1] <= 1) or ((Pt.X <> FPoint.X) or (Pt.Y <> FPoint.Y)) then
  begin
    TsgPointsListHelper(Poly).AddPoint(Pt);
    Inc(Counts.List^[Counts.Count - 1]);
    Result := Counts.List^[Counts.Count - 1];
    FPoint := Pt;
  end
  else
    Result := Counts[Counts.Count - 1];
end;

{ TsgModPolyItemsCollection }

function TsgModPolyItemsCollection.DoAddPt(const P): Integer;
var
  Pt: TFPoint;
  M: PsgMatrix4d;
begin
  M := PsgMatrix4d(Transformation);
  Pt := {$IFNDEF SG_NO_USE_KERNEL3D}PointTransform{$ELSE}FPointXMat{$ENDIF}(TFPoint(P), M^);
  Result := DoAddPtDirrect(Pt);
end;

function TsgModPolyItemsCollection.UpdateTransformation(const AValue;
  {$IFDEF HAS_UNMANAGED_TYPEINFO}AType: Pointer{$ELSE}ASize: Integer{$ENDIF}): Boolean;
begin
  if {$IFDEF HAS_UNMANAGED_TYPEINFO}AType = TypeInfo(TsgMatrix4d){$ELSE}ASize = SizeOf(TsgMatrix4d){$ENDIF} then
  begin
    SetLength(FTransformation, SizeOf(TsgMatrix4d) div SizeOf(TsgFloat));
    System.Move(AValue, FTransformation[0], SizeOf(TsgMatrix4d));
    FIsIdentityTransformation := {$IFNDEF SG_NO_USE_KERNEL3D}CompareMem(FTransformation, @IdentityMatrix4d.V[0].V[0], SizeOf(TFMatrix)){$ELSE}True{$ENDIF};
    Result := True;
  end
  else
    Result := inherited UpdateTransformation(AValue, {$IFDEF HAS_UNMANAGED_TYPEINFO}AType{$ELSE}ASize{$ENDIF});
end;

{ TsgCADImage }

constructor TsgCADImage.Create;
var
  vGuid: TGUID;
begin
  inherited Create;
  FRegenEntities := TsgObjectCollection.Create;
  FRegenScales := TsgObjectList.Create;
  FExpProcs := TsgProxyExport.Create;
  TsgContextAccess(FExpProcs).FIRect := @FRect;
  FAfterDrawNotifications := TsgNotifications.Create(Self);
  FBeforeDrawNotifications := TsgNotifications.Create(Self);
  FAfterIterateNotifications := TsgNotifications.Create(Self);
  FBeforeIterateNotifications := TsgNotifications.Create(Self);
{$IFDEF SG_THREAD_DRAW}
  FEntityLines := TsgLines.Create;
{$ENDIF}
{$IFDEF SG_MODULE_LOG}
  Log.Add('$' + IntToHex(TsgNativeUInt(Self), 0) + ' := '+ ClassName + '.Create;');
{$ENDIF}
{$IFNDEF SGDEL_6}
  if CoCreateGuid(vGuid) <> 0 then
{$ELSE}
  if CreateGUID(vGuid) <> 0 then
{$ENDIF}
    vGuid := cnstFailCreateGuid;
  SetGuid(vGuid);
//  SetDefaultViewPlotSettingsMode;
  FMetafileRegionScale := 1;
  FAddOnePixel := True;
  FCustomDraw := True;
  LibraryGDIPlusExists := PLibraryGDIPlusExists;
  FDraw.DrawMatrix := cnstCrossYMat;
  FDraw.Matrix := cnstIdentityMat;
  FDraw.ConvertMatrixCurrentInsertToWCS := FDraw.Matrix;
  FDraw.Matrix.EY.Y := -1;
  FFileName := '';
  FLineWeightScale := 1.0;
  FViewRectangle := BadRect;
  FConverter := CreateConverter;
  FCanvasParams.Brush := TBrush.Create{$IFDEF SG_FIREMONKEY}(TBrushKind.Solid, clBlack){$ENDIF};
  FCanvasParams.Font := TFont.Create;
  FCanvasParams.Pen := TPen.Create{$IFDEF SG_FIREMONKEY}(TBrushKind.Solid, clBlack){$ENDIF};
  FCanvasParams.Region := TsgRegion.Create;
  FConverter.OnCreate := EntityCreated;
  FConverter.BeforeRead := BeginRead;
  FConverter.AfterRead := EndRead;
  FConverter.OnRead := DoRead;
  CreateRegionStack;
  FStretch := True;
{$IFDEF SGFPC}
  FIsShowBackground := True;
{$ENDIF}
  IsShowLineWeight := cnstShowLineWeightDefault;
{$IFDEF SG_FIREMONKEY}
  FIsWithoutBorder := True;
{$ENDIF}
  FShowImages := True;
  FDrawMode := dmNormal;
  FBackgroundColor := clWhite;
  FChangeBackgroundColor := GetColorByBackgroud(FBackgroundColor);
  FDefaultColor := clBlack;
  FPlotWindowAreaColor := cnstPlotWindowAreaColor;
  FMillimetres := -1;
  FDottedSingPts := TFPointList.Create; //evg
  FColorToLineWeightMap := TsgColorToLineWeightMap.Create;
  FUseColorRGBInColorMap := IsImageWithColorsRGB;
  FIsProcessMessages := True; // process the messages
  FDrawingBox := BadRect;
  FStream := TMemoryStream.Create;
  FDimensionsVisible := True;
  FTextVisible := True;
  FBorderSize := cnstBorderSize;
  FBorderType := cnstBorderType;
  ContainerOfTextGlyphs.Reference;
  FSHXFontsProc := GetSHXFontsProc;
  FSelectionMatrixMode := smDisabled;
  FGetSelectionMatrixModeProc := GetSelectionMatrixProc;
  FSetSelectionMatrixModeProc := SetSelectionMatrixProc;
  FCachingRasterMode := bCachingRasterMode;
  FXrefStack := TsgObjectList.Create;
  FSnapMatrix := TsgSnapMatrix.Create;
  //FIsDraw3DAxes := True;
  SetListsCapacity(True);
  FUsePrecision := False;
  FPrecision := -1;
  FCanvasRegion := TsgRegion.Create;
  FRegenDelta := 0;
  FAnnotativeBlockRecordStack := TsgList.Create;
  FAnnotativeBlockRecordStack.Duplicates := dupIgnore;
  FAnnotativeBlockRecordStack.Sorted := True;
  FContext := TsgContext.Create;
  TsgContextAccess(FContext).FIRect := @FRect;
  FContextSelectionMatrix := TsgContextSelection.Create;
  FTTFMode := cnstTTFModeDefault;
  FXrefTransparency := -1;
end;

destructor TsgCADImage.Destroy;
var
  I: Integer;
begin
  TsgObjectCollection.FreeList(FRegenEntities);
  TsgObjectList.FreeList(FRegenScales);
  if Assigned(CADImageDestroyNotifications) then
    for I := 0 to CADImageDestroyNotifications.Count - 1 do
      CADImageDestroyNotifications{$IFNDEF SGDEL_2009}.List^{$ENDIF}[I](Self);
  FReeAndNil(FSnapEntityDefault);
  FreeAndNil(FExpProcs);
  FreeAndNil(FAfterDrawNotifications);
  FreeAndNil(FBeforeDrawNotifications);
  FreeAndNil(FAfterIterateNotifications);
  FreeAndNil(FBeforeIterateNotifications);
  FreeAndNil(FColorToLineWeightMap);
  FSelectionMatrix.Free;
  FDottedSingPts.Free;
  FPolyPoints.Free;
  FCanvasParams.Brush.Free;
  FCanvasParams.Font.Free;
  FCanvasParams.Pen.Free;
  FCanvasParams.Region.Free;
  FConverter.Release;
  if Assigned(ContainerOfTextGlyphs) then//for cadeditorX: call finalization before this
    ContainerOfTextGlyphs.Release;
  DisposeAndNil(FClippingRect);
  FStream.Free;
  FSnapMatrix.Free;
  TsgObjectList.FreeList(FXrefStack);
  FCanvasRegion.Free;
  FRegionStack.Free;
  FAnnotativeBlockRecordStack.Free;
  FContext.Free;
  FContextSelectionMatrix.Free;
{$IFDEF SG_FIREMONKEY}
  ClearImagesCache;
  FreeAndNil(FImagesCache);
{$ENDIF}
{$IFDEF SG_THREAD_DRAW}
  FreeAndNil(FEntityLines);
{$ENDIF}
  inherited Destroy;
end;

function GetDrawRect(const ADrawRect: TRect; const AExtents: TFRect;
  const AClippingRect: PFRect): TRect;

  procedure SetSizeExtents(const R1, R2: Integer;
    const E1, E2, C1, C2: Double; var Rez1, Rez2: Integer);
  var
    K, vdR, vdC: Double;
  begin
    vdC := C2 - C1;
    if vdC <> 0 then
    begin
      vdR := R2 - R1;
      K := vdR / vdC;
      Rez1 := R1 - Round(C1 * K);
      K := (E2 - E1) / vdC;
      Rez2 := Rez1 + Round(vdR * K);
    end;
  end;

begin
  Result := ADrawRect;
  if Assigned(AClippingRect) then
  begin
    SetSizeExtents(ADrawRect.Left, ADrawRect.Right,
      AExtents.Left, AExtents.Right, AClippingRect^.Left, AClippingRect^.Right,
      Result.Left, Result.Right);
    SetSizeExtents(ADrawRect.Top, ADrawRect.Bottom,
      AExtents.Bottom, AExtents.Top, AClippingRect^.Top, AClippingRect^.Bottom,
      Result.Top, Result.Bottom);
  end;
end;
//evg
procedure TsgCADImage.DrawRectInt(Rect: TRect; APrecision: Double = -1);
var
  R: TRect;
  vClippingRect: PFRect;
  vRegion: TsgRegion;
begin
  if FCurrentLayout = nil then Exit;
  vClippingRect := GetRealClip;
  Drawing := Self;
  try
  try
    if FExpProcs.Active and Assigned(FExpProcs.ExpInitView) then
      FExpProcs.ExpInitView(Self, Rect)
    else
      if CustomDraw then
        ApplyScale(GetDrawRect(Rect, GetDrawingExtents, vClippingRect));
  except
    Exit
  end;
  UpdateClientClipRect(Rect);
  if APrecision <> -1 then
  begin
    FPrecision := APrecision;
    SetDrawMatrix(cnstIdentityMat);
    UpdateClientClipRect(Classes.Rect(-maxint, -maxint, maxint, maxint));
  end;
  SetPenGeometric(cnstPenStyle_ENDCAP_JOIN_ROUND);
  // Set converter initial parameters
  Converter.AutoInsert := False;
  Converter.Params := @FDraw;
  if FExpProcs.Active then
    FContext.SetMatrix(@FDraw.Matrix);
  FSnapLastPoint.X := MaxDouble;     //evg
  FSnapLastPoint.Y := MaxDouble;//evg
  GetPixelSize(1, FDxyz.X, FDxyz.Y, True);
  //evg - to DrawRect!!!
  FColorPen := clWhite; //evg - temp
  FColorBrush := clWhite; //evg - temp
  DoSetColor(clBlack, ctPen);

  DoSetColor(clBlack, ctBrush);
  CreateRegionStack;
  try
    if Assigned(vClippingRect) or not CustomDraw then
      R := Rect
    else
      R := GetPlotSettingsRect;
    vRegion := TsgRegion.Create;
    try
      if not IsRectEmpty(R) then
        vRegion.ClipRect := R;
      if Assigned(FExternalRegion) then
        DoCreateExternalRegion(nil);
      ExpClipRegionInternal(nil, vRegion);
      CurrentLayout.Iterate(Converter, DrawEntity, DoFinish);
    finally
      vRegion.Free;
    end;
    if FIsDraw3DAxes then
      Draw3DAxes;
    FZRotate := 0;
  finally
    FRegionStack.Clear;
  end;
  finally
    Drawing := nil;
  end;
end;

function TsgCADImage.AbsHeight: Extended;
begin
  Result := AbsSize.Y;
end;

function TsgCADImage.AbsSize: TF2DPoint;
var
  vRect: PFRect;
  vExtents: TFRect;
begin
  Result.X := FXMax - FXMin;
  Result.Y := FYMax - FYMin;
  if UsePlotSetting then
  begin
    vExtents := GetDrawingExtents;
    Result.X := Abs(vExtents.Right - vExtents.Left);
    Result.Y := Abs(vExtents.Bottom - vExtents.Top);
  end
  else
  begin
    vRect := GetRealClip;
    if vRect <> nil then
    begin
      Result.X := vRect.Right - vRect.Left;
      Result.Y := vRect.Bottom - vRect.Top;
    end;
  end;
end;

function TsgCADImage.AbsWidth: Extended;
begin
  Result := AbsSize.X;
end;

procedure TsgCADImage.SetEntityVisibility(ACADFile: TObject);
begin
  if ACADFile is TsgCADImage then
  begin
    TsgCADImage(ACADFile).TextVisible := TextVisible;
    TsgCADImage(ACADFile).DimensionsVisible := DimensionsVisible;
  end;
end;

procedure TsgCADImage.AddScaledDXF(ACADFile: TObject; const AName: string; APos, AScale: TFPoint;
  Rotation: Single);
begin
  SetEntityVisibility(ACADFile);
  Converter.AddXRef(ACADFile, AName, APos, AScale, Rotation);
  GetExtents;
end;

function TsgCADImage.AddScaledDXFEx(ACADFile: TObject; const AName: string; APos, AScale: TFPoint;
  Rotation: Single; const AddXrefLayers: Boolean = True;
  const ADoExtends: Boolean = True): TsgDXFInsert;
begin
  SetEntityVisibility(ACADFile);
  Result := Converter.AddXRefEx(ACADFile, AName, APos, AScale, Rotation,
    CurrentLayout, AddXrefLayers);
  if ADoExtends then
    GetExtents;
end;

procedure TsgCADImage.Assign(Source: TPersistent);
var
  vPos: Integer;
begin
  if Source is TsgCADImage then
  begin
//{$IFDEF SG_OPENING_IN_THEADS}
//    FThreadId := TsgCADImage(Source).FThreadId;
//{$ENDIF}
    FAppInfo := TsgCADImage(Source).AppInfo;
    FConverter.Release;
    FConverter := TsgCADImage(Source).Converter;
    FConverter.Reference;
    Self.FXMin := TsgCADImage(Source).FXMin;
    Self.FXMax := TsgCADImage(Source).FXMax;
    Self.FYMin := TsgCADImage(Source).FYMin;
    Self.FYMax := TsgCADImage(Source).FYMax;
    Self.FZMin := TsgCADImage(Source).FZMin;
    Self.FZMax := TsgCADImage(Source).FZMax;
    Self.FCenter := TsgCADImage(Source).FCenter;
    Self.FColorToLineWeightMap.Assign(TsgCADImage(Source).FColorToLineWeightMap);
    Self.UseColorRGBInColorMap := TsgCADImage(Source).UseColorRGBInColorMap;
    Self.FCurrentLayout := TsgCADImage(Source).FCurrentLayout;
    Self.FBackgroundColor := TsgCADImage(Source).FBackgroundColor;
    Self.FChangeBackgroundColor := TsgCADImage(Source).FChangeBackgroundColor;
    Self.FDefaultColor := TsgCADImage(Source).FDefaultColor;
    Self.DimensionsVisible := TsgCADImage(Source).DimensionsVisible;
    Self.DrawingBox := TsgCADImage(Source).DrawingBox;
    Self.DrawMode := TsgCADImage(Source).DrawMode;
//    Self.SetClippingRect(TsgCADImage(Source).FClippingRect);
    Self.Clipping := TsgCADImage(Source).Clipping;
    Self.UsePlotSetting := TsgCADImage(Source).UsePlotSetting;
//    Self.ShowPlotSetting := TsgCADImage(Source).ShowPlotSetting; // this use to actual cadImage ONLY
    Self.PlotWindowAreaColor := TsgCADImage(Source).PlotWindowAreaColor;
    Self.TextVisible := TsgCADImage(Source).TextVisible;
    Self.UseWinEllipse := TsgCADImage(Source).UseWinEllipse;
    Self.BorderSize := TsgCADImage(Source).BorderSize;
    Self.BorderType := TsgCADImage(Source).BorderType;
    Self.IsWithoutBorder := TsgCADImage(Source).IsWithoutBorder;
    Self.FileInfo := TsgCADImage(Source).FileInfo;
    Self.FExternalRegion := TsgCADImage(Source).FExternalRegion;
{$IFDEF SG_BTI}
    Self.FUseFillStyleIndex := TsgCADImage(Source).FUseFillStyleIndex;
{$ENDIF}
    Self.XrefTransparency := TsgCADImage(Source).XrefTransparency;
    if KeepOriginal then
    begin
      vPos := TsgCADImage(Source).Stream.Position;
      TsgCADImage(Source).Stream.Position := 0;
      Self.FStream.Clear;
      Self.FStream.CopyFrom(TsgCADImage(Source).Stream, TsgCADImage(Source).Stream.Size);
      TsgCADImage(Source).Stream.Position := vPos;
      Self.FStream.Position := 0;
    end;
    ResetExtents;
    Changed(Self);
  end
  else
    inherited Assign(Source);
end;

function TsgCADImage.GetImageType: TsgExportFormat;
begin
  Result := efAuto;
end;

function TsgCADImage.GetIsCurLayoutShowPlot: Boolean;
begin
  Result := ShowPlotSetting and ((CurrentLayout.IsModel and ShowPlotForModel) or
    (not CurrentLayout.IsModel));
end;

function TsgCADImage.GetIsDrawingRotated: Boolean;
  function IsMatrixRotated(const M: TFMatrix): Boolean;
  var
    I, J: Integer;
  begin
    I := 0;
    Result := False;
    while not Result and (I <= 2) do
    begin
      J := 0;
      while not Result and (J <= 2) do
      begin
        Result := Abs(M.M[I,J]-Ord(I=J)) > fAccuracy;// if I=J, then M[I,J]<>1
        Inc(J);
      end;
      Inc(I);
    end;
  end;
begin
  Result := IsMatrixRotated(FCurrentLayout.RotMatrix);
end;

procedure TsgCADImage.ExpPolylineInteger(const Sender: TObject;
  const APoints: TsgIntegerList);
begin
  if FExpProcs.Active then
    FExpProcs.DoPolylineList(APoints)//PPoint(APoints.List), APoints.Count div 2)
  else
  begin
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolylineList(APoints);
    end;
    if IsDrawOnCanvas then
      FContext.DoPolylineList(APoints);
  end;
end;

procedure TsgCADImage.ExpPolyPolylineLists(const Sender: TObject; APoints : TsgIntegerList;
  ACounts: Pointer; ACount: Integer; const ACheckDrawOnBitMap: Boolean = True);
var
  I: Integer;
  vPPoint: PPoint;
  C: PInteger;
  vDrawOnBitMap: Boolean;
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
   if FExpProcs.Active then
    FExpProcs.DoPolyPolyline(APoints.List^, ACounts, ACount)
  else
  begin
    vDrawOnBitMap := IsDrawOnBitMap and ACheckDrawOnBitMap;
    if vDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
    end;
    if APoints.Count < 4000 then   // polypolyline works fast only for limited nubmer of points
    begin
      if vDrawOnBitMap then
        FContextSelectionMatrix.DoPolyPolyline(APoints.List^, ACounts, ACount);
      if IsDrawOnCanvas then
        FContext.DoPolyPolyline(APoints.List^, ACounts, ACount)
    end
    else
    begin
      vPPoint := PPoint(APoints.List);
      C := PInteger(ACounts);
      I := 0;
      while I < ACount do
      begin
        if vDrawOnBitMap then
          FContextSelectionMatrix.DoPolyline(vPPoint, C^);
        if IsDrawOnCanvas then
          FContext.DoPolyline(vPPoint, C^);
        Inc(vPPoint, C^);
        Inc(C);
        Inc(I);
      end;
    end;
  end;
end;

{$IFDEF SG_BTI}
procedure TsgCADImage.ExpPolyPolygonLists(const Sender: TObject; APoints : TsgIntegerList;
  ACounts: Pointer; ACount: Integer; const ACheckDrawOnBitMap: Boolean = True);
begin
  ExpPolyPolygonInteger(Sender, PPoint(APoints.List), PInteger(ACounts), ACount, ACheckDrawOnBitMap);
end;
{$ENDIF}

procedure TsgCADImage.ExpFillRectInternal(const Sender: TObject; const ARect: TRect);
begin
  FContext.IntPoints.Count := 0;
  FContext.AddIntPointDirect(MakeFPointFromPoint(ARect.TopLeft));
  FContext.AddIntPointDirect(MakeFPoint(ARect.Right, ARect.Top));
  FContext.AddIntPointDirect(MakeFPointFromPoint(ARect.BottomRight));
  FContext.AddIntPointDirect(MakeFPoint(ARect.Left, ARect.Bottom));
  FContext.AddIntFirstPoint(0);
  ExpPolygonInteger(Sender, FContext.IntPoints);
  FContext.IntPoints.Count := 0;
end;

procedure TsgCADImage.ExpFillRegionInternal(const ASender: TObject;
  ARegion: TRegion; const ADrawOnBitMap: Boolean = True);
{$IFNDEF SG_FIREMONKEY}
var
  R: PRect;
  vCount: Integer;
{$ENDIF}
begin
//TODO:  procedure TsgCADImage.ExpFillRegionInternal for FExpProcs.Active (Firemonkey)
  if FExpProcs.Active then
  begin
    if TsgRegion(ARegion).HandleAllocated  then
    begin
{$IFDEF SG_FIREMONKEY}
      if Assigned(FExpProcs.ExpPathData)then
        FExpProcs.ExpPathData(ARegion.Handle, False, True);
{$ELSE}
      if Assigned(FExpProcs.ExpFillRgn)then
      begin
        vCount := FRegionStack.GetData(ARegion, R);
        FExpProcs.DoFillRgn(R, vCount);
        FRegionStack.FinalizeData;
      end;
{$ENDIF}
    end;
  end
  else
  begin
    if ADrawOnBitMap and IsDrawOnBitMap then
    begin
      AddSelectionMatrix(ASender);
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.FillRegion(ARegion);
      FContextSelectionMatrix.BrushStyle := bsClear;
    end;
    if IsDrawOnCanvas then
      FContext.FillRegion(ARegion);
  end;
end;

procedure TsgCADImage.ExpRestoreDCInternal(const ASender: TObject;
  const AIndex: Integer);
begin
  FRegionStack.Pop;
  if FExpProcs.Active then
  begin
    if Assigned(FExpProcs.ExpRestoreDC) and Assigned(FExpProcs.ExpSaveDC) then
    begin
      FExpProcs.DoRestoreDC;
    end;
  end
  else
  begin
    if ASender <> nil then
    begin
      FContext.DoRestoreDC(AIndex);
      UpdateClientClipRect(FContext.ClipRect);
    end;
    if FRegionStack.Top <> nil then
      FContext.RegionSelect(TsgRegion(FRegionStack.Top))
    else
    begin
      TsgRegion(FCanvasRegion).FreeReference;
      FContext.RegionSelect(TsgRegion(FCanvasRegion));
    end;
  end;
end;

function TsgCADImage.ExpSaveDCInternal(const ASender: TObject): Integer;
begin
//  if not FExpProcs.Active then
//    TsgRegion(FCanvasRegion).Update(FCanvas);
//  FRegionStack.Push(FCanvasRegion);
  FRegionStack.Push(FRegionStack.Top);
  if FExpProcs.Active then
  begin
    if Assigned(FExpProcs.ExpSaveDC) and Assigned(FExpProcs.ExpRestoreDC) then
    begin
      FExpProcs.DoSaveDC;
    end;
    Result := 0;
  end
  else
  begin
    Result := 0;
    if ASender <> nil then
    begin
      Result := FContext.DoSaveDC;
      FContext.RegionUpdate(TsgRegion(FCanvasRegion));
    end;
  end;
end;

procedure TsgCADImage.ExpSolidInternal(const ASender: TObject; var ASolid,
  APts: array of TPoint; AHasZTick: Boolean);

  procedure DoCustomDrawSolid(const ASolids, APoints: array of TPoint;
    const AContext: TsgProxyBase);
  var
    I: Integer;
    P: array[0 .. 1] of TPoint;
  begin
    if AHasZTick then
    begin
      AContext.DoPolyline(@ASolids[0], Length(ASolids));
      if Length(APoints) > 0 then
      begin
        AContext.DoPolyline(@APoints[0], Length(APoints));
        for I := Low(APoints) to High(APoints) do
        begin
          P[0] := ASolids[I];
          P[1] := APoints[I];
          AContext.DoPolyline(@P[0], Length(P));
        end;
      end;
    end
    else
      if TsgDXFConverterAccess(Converter).PHeadVarStruct^.FillMode then
        AContext.DoPolygon(@ASolids[0], Length(ASolids))
      else
        AContext.DoPolyline(@ASolids[0], Length(ASolids));
  end;

begin
  if FExpProcs.Active then
    DoCustomDrawSolid(ASolid, APts, FExpProcs)
  else
  begin
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(ASender);
      FContextSelectionMatrix.PenWidth := cnstPenWidthNull;
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
      DoCustomDrawSolid(ASolid, APts, FContextSelectionMatrix);
      FContextSelectionMatrix.BrushStyle := bsClear;
    end;
    if IsDrawOnCanvas then
      DoCustomDrawSolid(ASolid, APts, FContext);
  end;
end;

procedure TsgCADImage.ExpPixelInteger(const Sender: TObject; APoint: TPoint; AColor: TColor);
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
 if AColor = clNone then
    AColor := GetDrawPenColor;
  if Assigned(FExpProcs.ExpPixel) then //evg
  begin
{$IFNDEF SG_NON_WIN_PLATFORM}
    SetPenGeometric(cnstPenStyle_ENDCAP_FLAT);
    try
      FExpProcs.DoPixel(APoint, AColor);
    finally
      SetPenGeometric(cnstPenStyle_ENDCAP_JOIN_ROUND);//pixel has width!!!
    end;
{$ELSE}
    FExpProcs.DoPixel(APoint, AColor);
{$ENDIF}
  end
  else
  begin
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPixel(APoint, TColor(SelectionMatrix.CurrColor - 1));
    end;
    if IsDrawOnCanvas then
      if IsPlotting then
      begin //printer
        DoSetColor(AColor, ctPen, 3);
        FContext.Line(APoint.X, APoint.Y, APoint.X, APoint.Y + 1);
      end
      else
        FContext.DoPixel(APoint, AColor);
  end;
end;

procedure TsgCADImage.ExpArcInteger(const Sender: TObject; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  const Arc: TsgArc);
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
  if FExpProcs.Active then
    FExpProcs.DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4, @Arc)
  else
  begin
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4, @Arc);
    end;
    if IsDrawOnCanvas then
      FContext.DoArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4, @Arc);
  end;
end;

procedure TsgCADImage.ExpArrowsInternal(const ASender: TObject;
  const APoly: TsgIntegerList; const AHasZTick: Boolean);
type
  PsgArrow = ^TsgArrow;
  TsgArrow = array [0..3] of TPoint;
  TsgArrowClosed = array [0..4] of TPoint;

  procedure DrawArrowInternal(const vArrowClosed: TsgIntegerList;
    const AArrow: PsgArrow; const AClosed: Boolean = True);
  begin
    if AArrow <> nil then
    begin
      vArrowClosed.Count := 0;
      vArrowClosed.AppendArray([AArrow^[0].X, AArrow^[0].Y,
        AArrow^[1].X, AArrow^[1].Y, AArrow^[2].X, AArrow^[2].Y,
        AArrow^[3].X, AArrow^[3].Y]);
      if AClosed then
      begin
        vArrowClosed.Add(AArrow^[0].X);
        vArrowClosed.Add(AArrow^[0].Y);
      end;
      ExpPolygonInteger(ASender, vArrowClosed);
    end;
  end;

var
  I, J, vCnt, vCntSub8: Integer;
  A1, A2: PsgArrow;
  vLines, vArrowClosed: TsgIntegerList;
begin
  vLines := TsgIntegerList.Create;
  vArrowClosed := TsgIntegerList.Create;
  try
    if AHasZTick then
    begin
      Fill2Counts(5);
      vCnt := APoly.Count div 2;
    end
    else
      vCnt := APoly.Count;
    vCntSub8 := APoly.Count - 8;
    I := 0;
    while I < vCnt do
    begin
      A1 := @APoly.List[I];
      if AHasZTick then
      begin
        J := I + vCnt;
        if J <= vCntSub8 then
          A2 := @APoly.List[J]
        else
          A2 := nil;
        Inc(I, 8);
        DrawArrowInternal(vArrowClosed, A1);
        if A2 <> nil then
        begin
          DrawArrowInternal(vArrowClosed, A2);
          vLines.Count := 0;
          for J := 0 to 3 do
            vLines.AppendArray([A1^[J].X, A1^[J].Y, A2^[J].X, A2^[J].Y]);
          vLines.AppendArray([A1^[0].X, A1^[0].Y, A2^[0].X, A2^[0].Y]);
          ExpPolyPolylineLists(ASender, vLines, FContext.Counts2.List, 5);
        end;
      end
      else
      begin
        Inc(I, 8);
        if TsgDXFConverterAccess(Converter).PHeadVarStruct^.FillMode then
        begin
          //ExpPolygonInteger(ASender, PPoint(A1), 4)
          DrawArrowInternal(vArrowClosed, A1, False);
        end
        else
          DrawArrowInternal(vArrowClosed, A1);
      end;
    end;
  finally
    FreeAndNil(vArrowClosed);
    vLines.Free;
  end;
end;

procedure TsgCADImage.ExpBitBlt(const ASender: TObject; const X, Y, Width,
  Height: Integer; const ABitMap: TBitmap; const SX: Integer = 0;
  const SY: Integer = 0; const ARop: Cardinal = SRCCOPY);
var
  vRect: TRect;
  vIsDrawOnBitMap: Boolean;
begin
  if FExpProcs.Active then
  begin
    vRect := Rect(X, Y, X + Width, Y + Height);
    ExpImageGDIInternal(ASender, vRect, ABitMap);
  end
  else
  begin
    vIsDrawOnBitMap := IsDrawOnBitMap;
    if (ASender is TsgDXFImageEnt) and (TsgDXFImageEnt(ASender).Flags and 1 <> 0) then
      vIsDrawOnBitMap := False;
    if vIsDrawOnBitMap then
    begin
      vRect := Rect(X, Y, X + Width, Y + Height);
      AddSelectionMatrix(ASender);
      FContextSelectionMatrix.IntPoints.Count := 0;
      try
        AddRectAsPoly(vRect, FContextSelectionMatrix.IntPoints);
        FContextSelectionMatrix.BrushStyle := bsSolid;
        FContextSelectionMatrix.Changed;
        FContextSelectionMatrix.DoPolygonList(FContextSelectionMatrix.IntPoints);
        FContextSelectionMatrix.BrushStyle := bsClear;
      finally
        FContextSelectionMatrix.IntPoints.Count := 0;
      end;
    end;
    if IsDrawOnCanvas then
      FContext.BitBlt(X, Y, Width, Height, SX, SY, ARop, ABitMap);
  end;
end;

function TsgCADImage.ExpClipRegionInternal(const ASender: TObject;
  const ARegion: TRegion): Boolean;
var
  R: PRect;
  vCount: Integer;
  vRegion: TsgRegion;
begin
  Result := TsgRegion(ARegion).HandleAllocated;
  if Result then
  begin
    vRegion := TsgRegion(FRegionStack.Top);
    if vRegion <> nil then
      vRegion.Intersect(ARegion)
    else
    begin
      FRegionStack.Push(nil);
      FRegionStack.Top.Assign(ARegion);
      vRegion := TsgRegion(FRegionStack.Top);
    end;
    if vRegion.HandleAllocated then
      if FExpProcs.Active then
      begin
        if Assigned(FExpProcs.ExpFillRgn) and vRegion.HandleAllocated then
        begin
          vCount := FRegionStack.GetData(vRegion, R);
          FExpProcs.DoClipRgn(R, vCount);
          FRegionStack.FinalizeData;
        end;
      end
      else
        if ASender <> nil then
          SetRegion(ASender, vRegion);
  end;
end;

procedure TsgCADImage.ExpImageGDIInternal(const ASender: TObject;
  const R: TRect; AGraphic: TPersistent);
begin
  if FExpProcs.Active then
  begin
    if Assigned(FExpProcs.ExpImage) then
  {$IFNDEF SG_FIREMONKEY}
      FExpProcs.DoImage(FExpProcs.Rect, TGraphic(AGraphic));  //evg
  {$ELSE}
      FExpProcs.DoImage(FExpProcs.Rect, TBitmap(AGraphic));  //evg
  {$ENDIF}
  end
  else
    FContext.DoImage(FContext.Rect, {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TBitmap{$ENDIF}(AGraphic));
end;

procedure TsgCADImage.ExpOle2FrameInternal(const ASender: TObject;
  const ARect: TF2DRect);
var
  vOleFrame: TsgDXFOle2FrameAccess;
  vBounds: TRect;
{$IFDEF SG_HAS_ACTIVEX}
{$IFDEF SG_FM_WINDOWS}
  vOleObject: IOleObject;
  vStroke: TStrokeBrush;
  vBitmap: TBitmap;
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
  vDC: HDC;
  vPrevTextAlign: Integer;
  vOleObject: IOleObject;

  procedure DoDrawFrame(ADC: HDC; const ABounds: TRect; ABrush: HBRUSH);
  var
    vPrevBrush: HBRUSH;
  begin
    if GetObjectType(ADC) <> 0 then
    begin
      vPrevBrush := SelectObject(ADC, ABrush);
      Rectangle(ADC, ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Bottom);
      SelectObject(ADC, vPrevBrush);
    end;
  end;
{$ENDIF}
{$ENDIF}

  function GetActualOleObejct(out obj): Boolean;
  var
    stg: IStorage;
    vBindCtx: IBindCtx;
  begin
    Pointer(obj) := nil;
    if (vOleFrame.TID <> GetCurrentThreadId) and (vOleFrame.Moniker <> nil) and
       Succeeded(CreateBindCtx(0, vBindCtx)) and Succeeded(vOleFrame.Moniker.BindToStorage(vBindCtx, nil, IStorage, stg)) then
      Result := OleLoad(stg, IOleObject, nil, obj) = S_OK
    else
      Result := vOleFrame.OleObject.QueryInterface(IOleObject, obj) = S_OK;
  end;
{$ENDIF}

  function NeedDrawFrame: Boolean;
  begin
    Result := (Converter.OleFrame = 1) or ((Converter.OleFrame = 2) and not IsPlotting);
  end;

  procedure DrawOleAsBitmap(AActiveContext: TsgProxyBase2D);
{$IFDEF SGFPC}
  var
    X, Y: Integer;
    vGrayBitmap: TBitmap;
{$ENDIF}
  begin
    vBounds := cnstBad2DRectExport;
    ExpandRect(vBounds, Point(Round(ARect.Left), Round(ARect.Top)));
    ExpandRect(vBounds, Point(Round(ARect.Right), Round(ARect.Bottom)));
{$IFDEF SG_FM_WINDOWS}
   {$IFDEF SG_HAS_ACTIVEX}
    if not GetActualOleObejct(vOleObject) then
      vOleObject := vOleFrame.OleObject;
    {$ENDIF}
    vBitmap := TBitmap.Create;
    try
      vOleFrame.ConvertOleToBitmap(vOleObject, ARect.Right - ARect.Left,
        ARect.Bottom - ARect.Top, vBounds, vBitmap);
      AActiveContext.DoImage(vBounds, vBitmap);
    finally
      vBitmap.Free;
    end;
{$ELSE}
    if vOleFrame.OleBitmap <> nil then
    begin
    {$IFDEF SGFPC}
      if DrawMode in [dmGray] then
      begin
        vGrayBitmap := TBitmap.Create;
        try
          vGrayBitmap.PixelFormat := pf32bit;
          vGrayBitmap.Width := vOleFrame.OleBitmap.Width;
          vGrayBitmap.Height := vOleFrame.OleBitmap.Height;
          for Y := 0 to vGrayBitmap.Height - 1 do
            for X := 0 to vGrayBitmap.Height - 1 do
              vGrayBitmap.Canvas.Pixels[X, Y] := ConvertColortoGray(vOleFrame.OleBitmap.Canvas.Pixels[X, Y]);
          AActiveContext.DoImage(vBounds, vGrayBitmap);
        finally
          vGrayBitmap.Free;
        end;
      end
      else
      {$ENDIF}
        AActiveContext.DoImage(vBounds, vOleFrame.OleBitmap);
    end;
{$ENDIF}
    if NeedDrawFrame then
    begin
      AActiveContext.IntPoints.Count := 0;
      AddRectAsPoly(vBounds, AActiveContext.IntPoints);
      AActiveContext.DoPolylineList(AActiveContext.IntPoints);
      AActiveContext.IntPoints.Count := 0;
    end;
  end;

begin
  vOleFrame := TsgDXFOle2FrameAccess(ASender);
  if FExpProcs.Active then
  begin
    if Assigned(FExpProcs.ExpImage) then
      DrawOleAsBitmap(FExpProcs);
  end
  else
  begin
    vBounds := GetBoxOfF2Rect(ARect);
    if IsDrawOnBitMap then
    begin
      FContextSelectionMatrix.IntPoints.Count := 0;
      try
        AddRectAsPoly(vBounds, FContextSelectionMatrix.IntPoints);
        AddSelectionMatrix(ASender);
        if cnstSelectRasterMode = 1 then
        begin
          FContextSelectionMatrix.Changed;
          FContextSelectionMatrix.DoPolylineList(FContextSelectionMatrix.IntPoints);
        end
        else
        begin
          FContextSelectionMatrix.BrushStyle := bsSolid;
          FContextSelectionMatrix.Changed;
          FContextSelectionMatrix.DoPolygonList(FContextSelectionMatrix.IntPoints);
          FContextSelectionMatrix.BrushStyle := bsClear;
        end;
      finally
        FContextSelectionMatrix.IntPoints.Count := 0;
      end;
    end;
    if IsDrawOnCanvas then
    begin
{$IFDEF SG_HAS_ACTIVEX}
      if GetActualOleObejct(vOleObject) then
      begin
  {$IFDEF SG_FM_WINDOWS}
        DrawOleAsBitmap(FContext);
        if (Converter.OleFrame = 1) or ((Converter.OleFrame = 2) and not IsPlotting) then
        begin
          vStroke := TStrokeBrush.Create(TBrushKind.Solid, GetDefaultColor.AsBGRA);
          try
            FContext.Canvas.DrawRect(TRectF.Create(vBounds), 0, 0, [], 1, vStroke);
          finally
            vStroke.Free;
          end;
        end;
  {$ELSE}
  {$IFNDEF SG_NON_WIN_PLATFORM}
        try
          vDC := FContext.GetDC;
          vPrevTextAlign := SetTextAlign(vDC, cnstDefaultTextAlign);
          try
            FillRect(vDC, vBounds, GetStockObject(WHITE_BRUSH));
            try
              try
                OleCheck(OleDraw(vOleObject, TsgDXFOle2Frame(ASender).Aspect, vDC, vBounds));
              finally
                vOleObject := nil;
              end;
            finally
              if NeedDrawFrame then
                DoDrawFrame(vDC, vBounds, GetStockObject(NULL_BRUSH));
            end;
          finally
            SetTextAlign(vDC, vPrevTextAlign);
          end;
        except
  {$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
        end;
  {$ENDIF}
  {$ENDIF}
      end;
{$ELSE}
      DrawOleAsBitmap(FContext);
{$ENDIF}
    end;
  end;
end;

procedure TsgCADImage.ExpPolygonInteger(const Sender: TObject;
  const APoints: TsgIntegerList);
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
  if FExpProcs.Active then
    FExpProcs.DoPolygonList(APoints)
  else
  begin
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolygonList(APoints);
      FContextSelectionMatrix.BrushStyle := bsClear;
    end;
    if IsDrawOnCanvas then
      FContext.DoPolygonList(APoints);
  end
end;

procedure TsgCADImage.ExpPolyPolygonInteger(const Sender: TObject; Points: PPoint;
  Counts: {$IFDEF SGFPC}ObjPas.{$ENDIF}PInteger; Count: Integer;
  const ACheckDrawOnBitMap: Boolean = True);
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
  if FExpProcs.Active then
    FExpProcs.DoPolyPolygon(Points^, Counts, Count)
  else
  begin
    if ACheckDrawOnBitMap and IsDrawOnBitMap then
    begin
      FContextSelectionMatrix.PenWidth := cnstPenWidthNull;
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolyPolygon(Points^, Counts, Count);
    end;
    if IsDrawOnCanvas then
    begin
      SetPenWidth(1);
      FContext.DoPolyPolygon(Points^, Counts, Count);
    end;
  end;
end;

procedure TsgCADImage.ExpTextInteger(ATextType: TsgTextType; X, Y: Integer;
  Options: Longint; Rect: PRect; StrWideChar: PWideChar;
  StrChar: PAnsiChar; Count: Longint;
  Dx:{$IFDEF SGFPC}Pointer{$ELSE}PInteger{$ENDIF};
  const ATextParams: PsgExpTextParam);
var
  vTextA: AnsiString;
  vTextW: WideString;
{$IFDEF SG_NON_WIN_PLATFORM}
  S: string;
{$ENDIF}
begin
 {$IFDEF NOGDI} Exit; {$ENDIF}
  if FExpProcs.Active then
  begin
    if Assigned(FExpProcs.ExpText) then
      FExpProcs.DoText(ATextType, X, Y, Options, Rect, StrWideChar, StrChar, Count, Dx)
    else
    begin
      if Assigned(FExpProcs.ExpTextOut) then
      begin
        vTextA := '';
        vTextW := '';
        if StrChar <> nil then
          vTextA := AnsiString(StrChar);
        if StrWideChar <> nil then
          vTextW := WideString(StrWideChar);
        FExpProcs.DoTextOutAW(X, Y, vTextA, vTextW, ATextParams);
      end;
    end
  end
  else
  begin
      //
      // Bugfix Delphi XE2
      //
{$IFNDEF SG_NON_WIN_PLATFORM}
    if not FExportToDXFMode then
      TmvExtFont(FContext.Canvas.Font).Escapement := 0;
    case ATextType of
{$IFDEF UNICODE}
      ttExtTextOutA: FContext.DoExtTextOutA(X, Y, Options, Rect, StrChar, Count, Dx);
      ttTextOutW:    FContext.DoTextOutW(X, Y, StrWideChar, Count);
      ttExtTextOutW: FContext.DoExtTextOutW(X, Y, Options, Rect, StrWideChar, Count, Dx);
{$ELSE}
      ttTextOut:     FContext.DoTextOut(X, Y, StrChar, Count);
      ttTextOutW:    FContext.DoTextOutW(X, Y, StrWideChar, Count);
      ttExTextOut, ttExtTextOutA:   FContext.DoExtTextOutA(X, Y, Options, Rect, StrChar, Count, Dx);
      ttExtTextOutW: FContext.DoExtTextOutW(X, Y, Options, Rect, StrWideChar, Count, Dx);
{$ENDIF}
      end;
{$ELSE}
    S := string(WideString(StrWideChar));
    case ATextType of
      ttTextOut:  FContext.DoTextOut(X, Y, string(AnsiString(StrChar)), Count);
      ttTextOutW: FContext.DoTextOut(X, Y, PChar(S), Count);
      ttExTextOut, ttExtTextOutA: FContext.DoExtTextOutA(X, Y, Options, Rect, StrChar, Count, Dx);
      ttExtTextOutW: FContext.DoExtTextOut(X, Y, Options, Rect, StrWideChar, Count, Dx);
    end;
{$ENDIF}
  end;
end;

procedure TsgCADImage.ExpTextInternal(const ASender: TObject;
  ATextGlyph: TsgTextGlyph; ATextItems: TStringBuilder;
  const AMatrix: TFMatrix; const ATextMode: Byte);
var
  vPointSize: TFPoint;
  TextLinesCollection: TsgText2dLinesCollection;
  vUnderline, vStrikeOut: Boolean;
  vFill, vEdge, vStyle: Boolean;
begin
  vUnderline := fmUnderline in TsgDXFTextAccess(ASender).Properties.FontStyle;
  vStrikeOut := fmStrikeOut in TsgDXFTextAccess(ASender).Properties.FontStyle;
  if FExpProcs.Active then
  begin
    if not (Assigned(FExpProcs.ExpPolyPolygon) or Assigned(FExpProcs.ExpPolyPolyline)) then
      Exit;
    vEdge := ATextMode and 1 <> 0;
    vFill := ATextMode and 2 <> 0;
    vStyle := ATextMode and 4 <> 0;
    GetPixelSize(1.5, vPointSize.X, vPointSize.Y, True);
    vPointSize.Z := vPointSize.X;
    FContext.Poly.Count := 0;
    TextLinesCollection := TsgText2dLinesCollection.Create(FContext.Poly, False);
    try
      ATextGlyph.GetPolyPolyline(ATextItems, AMatrix, TextLinesCollection, TsgDXFText(ASender).Tracking, @vPointSize, vUnderline, vStrikeOut, vStyle);
      if vFill then
        FExpProcs.DoPolyPolygon(FContext.Poly.List^, PInteger(TextLinesCollection.Counts.List), TextLinesCollection.Counts.Count);
      if vEdge then
        FExpProcs.DoPolyPolyline(FContext.Poly.List^, PInteger(TextLinesCollection.Counts.List), TextLinesCollection.Counts.Count) ;
      if vStyle then
        FExpProcs.DoPolyPolyline(FContext.Poly.List^, PInteger(TextLinesCollection.Counts.List), TextLinesCollection.Counts.Count);
    finally
      TextLinesCollection.Free;
    end;
  end
  else
  begin
    FContext.PenWidth := cnstPenWidthNull;
    DoSetColor(FColorFont, ctPen, 3);//FCanvas.Pen.Color := FCanvas.Font.Color
    DoSetColor(FColorFont, ctBrush, 3);//FCanvas.Brush.Color := FCanvas.Pen.Color;
    ATextGlyph.DrawText(FContext, ATextItems, vUnderline, vStrikeOut, AMatrix,
      TsgDXFText(ASender).Tracking, ATextMode);
{$IFNDEF SG_FIREMONKEY}
    DoSetColor(FColorBrush, ctBrushClear);
{$ENDIF}
  end;
end;

procedure TsgCADImage.ExpEntityProcBegin(const AEntity: TsgDXFEntity);
begin
  if FExpProcs.Active and Assigned(FExpProcs.ExpEntProc) then
     FExpProcs.ExpEntProc(AEntity, 1);
end;

procedure TsgCADImage.ExpEntityProcEnd(const AEntity: TsgDXFEntity);
begin
  if FExpProcs.Active and Assigned(FExpProcs.ExpEntProc) then
     FExpProcs.ExpEntProc(AEntity, 2);
end;

procedure TsgCADImage.DoSetColor(AColor: TColor; AColorType: TsgColorType;
  AUseExternalColor: Integer = 1);
var
  vChanged: Boolean;
begin
  if AColorType = ctBrushClear then
    AUseExternalColor := AUseExternalColor and $FFFFFFFD;
  if not SetColorByExternalPen(AUseExternalColor, AColor, @FTmpEntColor.EntColorCAD) then
  begin
    if AColor = clNone then
      AColor := GetDrawPenColor;
  end;
  if FExpProcs.Active then
    FExpProcs.DoColor(AColor, AColorType)
  else
  begin
    case AColorType of
      ctPen:
        begin
          vChanged := AColor <> FColorPen;
          if vChanged then
            FColorPen := AColor;
        end;
      ctBrush:
        begin
          vChanged := (AColor <> FColorBrush) or (FContext.BrushStyle = bsClear) or
            (AColor <> FContext.BrushColor);  // FCanvas.Brush.Color something changes
              // without calling DoSetColor
          if vChanged then
            FColorBrush := AColor;
        end;
      ctFont:
        begin
          vChanged := AColor <> FColorFont;
          if vChanged then
            FColorFont := AColor;
        end;
      ctBrushClear: vChanged := True;
    else
      vChanged := False;
    end;
    if vChanged then
    begin
      case AColorType of
        ctPen:   FContext.PenColor := AColor;
        ctBrush: FContext.BrushColor := AColor;
        ctFont:  FContext.FontColor := AColor;
        ctBrushClear:
         begin
           if FColorBrush <> GetDrawBackgroundColor then
           begin
             FColorBrush := GetDrawBackgroundColor;
             FContext.BrushColor := FColorBrush;
           end;
           if not Assigned(FDrawExternlProps) then
             FContext.BrushStyle := bsClear;
         end;
      end;
    end;
  end;
end;

function TsgCADImage.SetColorByExternalPen(const AUseExternalColor: Integer;
  var AColor: TColor; const AEntColor: PsgColorCAD): Boolean;
var
  I, vDrawPenColor, vColorRGB: Integer;
  vContextPenColor: TsgColorCAD;
  vColorKey: TsgColorCAD;
begin
  Result := False;
  if AUseExternalColor and 2 <> 0 then
  begin
    if Assigned(FColorToLineWeightMap.Colors) and
      GetColorToLineWeight(vContextPenColor, AEntColor) then
    begin
      vDrawPenColor := GetDrawPenColor;
      for I := 0 to FColorToLineWeightMap.Count - 1 do
      begin
        vColorKey := FColorToLineWeightMap.Keys[I];
        vColorRGB := FColorToLineWeightMap.Colors[I];
        if (vColorRGB = cnstColorLWeightColorNewEmpty) or IsEqualColorCAD(vColorKey, cnstColorCADLWeightBadKey) then
          Continue;
        if UseKeyColor(vColorKey, vContextPenColor, vDrawPenColor) then
        begin
          AColor := vColorRGB;
          Break;
        end;
      end;
    end;
  end;
  if AUseExternalColor and 1 <> 0 then
  begin
    if Assigned(FDrawExternlProps) then
    begin
      case FDrawExternlProps.UseExternalPen of
        epmInternal:
          begin
            if FDrawExternlProps.EntityHandle = FEntityHandle then
            begin
              AColor := FDrawExternlProps.Mode.Color;
              Result := True;
            end;
          end;
        epmExternal:
          begin
            AColor := FDrawExternlProps.Mode.Color;
            Result := True;
          end;
      end;
    end;
  end;
end;

procedure TsgCADImage.DoSetFont(AFont: TFont);
begin
  if FExpProcs.Active then
  begin
    FExpProcs.DoFont(AFont);
  end
  else
  begin
    FContext.DoFont(AFont);
{$IFNDEF SG_FIREMONKEY}
    FColorFont := AFont.Color;
{$ENDIF}
  end;
end;

procedure TsgCADImage.DoSetStyle(AStyle: Integer; AColorType: TsgColorType);
begin
  if FExpProcs.Active then
    FExpProcs.DoStyle(AStyle, AColorType)
  else
    case AColorType of
      ctPen:         FContext.PenStyle := TPenStyle(AStyle);
      ctBrush:       FContext.BrushStyle := TBrushStyle(AStyle);
      ctFont:        FContext.FontStyle := TFontStyles({$IFDEF SGFPC}Integer{$ELSE}Byte{$ENDIF}(AStyle));
      ctBrushClear:  DoSetColor(FColorBrush, ctBrushClear);
    end;
end;

procedure TsgCADImage.DoUpdateEvent(Sender: TObject);
const
  R: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  vPercentDone: Integer;
  vStage: TProgressStage;
begin
  if Sender is sgConsts.TsgObjectInt64 then
  begin
    if Assigned(OnProgress) then
    begin
      vPercentDone := sgConsts.TsgObjectInt64(Sender).FieldInt;
      if vPercentDone < 1 then
        vStage := psStarting
      else
        if vPercentDone >= 100 then
          vStage := psRunning//psEnding
        else
          vStage := psRunning;
      Progress(Self, vStage, vPercentDone, False, R, sLoadEntities);
      if vPercentDone >= 100 then
        Progress(Self, psRunning, FPercentDone, False, R, FMsg);
    end;
  end
  else
    DoProgress(psRunning);
end;

procedure TsgCADImage.SetPalette(Value: HPALETTE);
begin

end;

procedure TsgCADImage.SetPenGeometric(AStyle: DWORD);
var
  vStyle: DWORD;
begin
  vStyle := {$IFNDEF SG_FIREMONKEY}PS_GEOMETRIC or PS_SOLID or{$ENDIF} AStyle;
  if FExpProcs.Active then
    FExpProcs.DoPenGeometric(vStyle, FColorPen, Ceil(FPenWidth))
  else
    FContext.DoPenGeometric(vStyle, FColorPen, Ceil(FPenWidth));
end;

procedure TsgCADImage.SetPenStyle(const AStylePen: DWORD; const AMiterLimit: Single);
begin
  SetPenGeometric(AStylePen);
{$IFNDEF SG_FIREMONKEY}
  if (AStylePen and PS_JOIN_MITER) <> 0 then
  begin
    if FExpProcs.Active then
      FExpProcs.DoPenMiterLimit(AMiterLimit)
    else
      FContext.DoPenMiterLimit(AMiterLimit);
  end;
{$ENDIF}
end;

procedure TsgCADImage.SetPenWidth(AWidth: Single);
begin
  if FExpProcs.Active then
    FExpProcs.DoPenWidth(AWidth);
  if AWidth = FPenWidth then
    Exit;
  FPenWidth := AWidth;
  if not FExpProcs.Active then
    FContext.PenWidth := AWidth;
end;

// evg: the function GetPixelSize should be completely changed
procedure TsgCADImage.GetPixelSize(ANumberOfPixels: Double; var Adx, Ady: Double;
  const AScaled: Boolean);
begin
  if AScaled then
    Adx := GetCADPixelSize(FDraw.Matrix, ANumberOfPixels)
  else
    Adx := GetCADPixelSize(FDraw.DrawMatrix, ANumberOfPixels);
  Ady :=  Adx;
end;

procedure TsgCADImage.SetClipping(const AClip: TsgClipping);
begin
  case AClip.ClipMode of
    1:  SetClippingRect(@AClip.ClipRect);
    2:  SetClippingRectExt(AClip.ClipRect);
    3:  SetClippingRectExt(AClip.ClipRect);//clip of view rect
    4:  SetClippingRect(nil);//clip is draw rect
  else
    SetClippingRect(nil);
  end;
end;

procedure TsgCADImage.SetClippingRect(Value: PFRect);
begin
  if FClippingRect = nil then
    New(FClippingRect);
  if Value = nil then
  begin
    Dispose(FClippingRect);
    FClippingRect := nil;
  end
  else
    FClippingRect^ := Value^;
  Changed(Self);
end;

procedure TsgCADImage.SetClippingRectExt(const AValue: TFRect);
var
  vPicRect: TF2DRect;
  vClippingRect, vNewExts, vViewBox: TFRect;
begin
  vNewExts := Extents;
  vPicRect := MakeF2DRect(vNewExts.Left, vNewExts.Bottom, vNewExts.Right, vNewExts.Top);
  ApplyScale(vPicRect.Left, vPicRect.Top, vPicRect.Right, vPicRect.Bottom);
  vViewBox := AValue;
  TransRectCorners(vViewBox, DrawMatrix);
  SwapDoubles(vViewBox.Top, vViewBox.Bottom);
  // Clipping rectangle (ignoring Z-coordinate)
  vClippingRect.Left := (vViewBox.Left - vPicRect.Left);
  vClippingRect.Top := (vViewBox.Top - vPicRect.Top);
  vClippingRect.Right := vClippingRect.Left + (vViewBox.Right - vViewBox.Left);
  vClippingRect.Bottom := vClippingRect.Top + (vViewBox.Bottom - vViewBox.Top);
  vClippingRect.Z1 := 0;
  vClippingRect.Z2 := 0;
  SetClippingRect(@vClippingRect);
end;

{$IFDEF SGDEL_4}   //cadimage
{$IFDEF SG_DELPHI_VCL}
function TsgCADImage.ExportToMetafile(AWidth, AHeight: Integer): TMetafile;
var
  MF: TMetafile;
  MC: TMetafileCanvas;
  vmmToPixelX, vmmToPixelY: Double;
  Ratio: Double;
  vMax: Integer;
  vDc: HDC;
  R: TRect;
begin
   vMax := 3276500;
   vDc := GetDc(0);
   try
     vmmToPixelX := GetDeviceCaps(vDc, HORZSIZE) /
       GetDeviceCaps(vDc,HORZRES);
     vmmToPixelY := GetDeviceCaps(vDc, VERTSIZE) /
       GetDeviceCaps(vDc,VERTRES);
   finally
     ReleaseDC(0, vDc);
   end;
   if AHeight <> 0 then
     Ratio := AWidth / AHeight
   else
     Ratio := 1;
   if vmmToPixelX <= vmmToPixelY then
     vMax := Round(vMax * vmmToPixelX)
   else
     vMax := Round(vMax * vmmToPixelY);
   if (AWidth > vMax) or (AHeight > vMax) then
     if Ratio <= 1 then
     begin
       AHeight := vMax;
       AWidth := Round(vMax * Ratio);
     end
     else
     begin
       AWidth := vMax;
       AHeight := Round(vMax / Ratio);
     end;

   MF := TMetafile.Create;
   MF.MMWidth := AWidth;
   MF.MMHeight := AHeight;
   MC := TMetafileCanvas.Create(MF, 0);
   try
     FConverting := True;
     R := Rect(0, 0, Round(AWidth / (100 * vmmToPixelX)),
       Round(AHeight / (100 * vmmToPixelY)));
     Draw(MC, R);
     DoMetafileExport(MC, R);
   finally
     OnMetafileExport := nil;
     FConverting := False;
     MC.Free;
   end;
   result := MF;
end;

function TsgCADImage.ExportToMetafile(MaxDim: Integer): TMetafile;
var
  AWidth, AHeight: Integer;
  Ratio: Double;
begin
   if MaxDim > 3276700 then
     MaxDim := 3276700;
   Ratio := Width / Height;
   if Ratio <= 1 then
   begin
     AHeight := MaxDim;
     AWidth := round(MaxDim * Ratio);
   end
   else
   begin
     AWidth := MaxDim;
     AHeight := round(MaxDim / Ratio);
   end;
   Result := ExportToMetafile(AWidth, AHeight);
end;

function TsgCADImage.ExportToMetafile(var OffsetX, OffsetY, UnitSize: TsgFloat): TMetafile;
const
  iMaxMetafileSizeWin98 = 32765;
  iMaxMetafileSizeWinNT = 64000000;
var
  MF: TMetafile;
  MC: TMetafileCanvas;
  vMaxDim, vRatio, vHeight, vWidth, RatioKoef: Double;
  OSVersionInfo: TOSVersionInfo;
  vFlag1, vFlag2, vFlag3: Boolean;
  vPrevRots: TFMatrix;
  S, S1: string;
  I: Integer;
  vRealExtents: TFRect;
  vDs: Char;
  R: TRect;
begin
  FExportToDXFMode := True;
  try
    if IsBadRect(FDrawingBox) then
      vRealExtents := PureExtents
    else
      vRealExtents := FDrawingBox;
    vFlag1 := Transparent;
    vFlag2 := IsWithoutBorder;
    vFlag3 := Converter.UseSHXFonts;
    vPrevRots := CurrentLayout.RotMatrix;
    Transparent :=  True;
    IsWithoutBorder := True;
    Converter.UseSHXFonts := False;
    OSVersionInfo.dwOSVersionInfoSize := Sizeof(TOSVersionInfo);
    GetVersionEx(OSVersionInfo);
    (* // For future versions
    if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
      vMaxDim := iMaxMetafileSizeWinNT
    else
      vMaxDim := iMaxMetafileSizeWin98;*)
    vMaxDim := iMaxMetafileSizeWin98;
    vRatio := AbsWidth / AbsHeight;
    if vRatio <= 1 then
      UnitSize := AbsHeight
    else
      UnitSize := AbsWidth;
    UnitSize := UnitSize / vMaxDim;
    vDs := SetDecimalSeparator('.');
    try
      try
        S := FloatToStr(UnitSize);
        for I := 1 to Length(S) do
          if not CharInSet(S[I], ['0', GetDecimalSeparator]) then
          begin
            S1 := S1 + '1';
            Break;
          end
          else
            S1 := S1 + S[I];
      except
        S1 := '';
      end;
      UnitSize := ConvToFloatDef(S1, UnitSize) * 10;
      I := Pos('E', AnsiUpperCase(S));
      if I > 0 then
        UnitSize := UnitSize * Power(10, StrToIntDef(Copy(S, I+1, MaxInt), 1));
    finally
      SetDecimalSeparator(vDs);
    end;
    RatioKoef := 1 / UnitSize;
    if vRatio <= 1 then
    begin
      vHeight := RatioKoef * AbsHeight;
      vWidth := vHeight * vRatio;
    end
    else
    begin
      vWidth := RatioKoef * AbsWidth;
      vHeight := vWidth / vRatio;
    end;
    MF := TMetafile.Create;
    OffsetX := Round(RatioKoef * vRealExtents.Left);
    OffsetY := Round(RatioKoef * vRealExtents.Top);
    MC := TMetafileCanvas.Create(MF, 0);
    try
      FConverting := True;
      R := Rect(0, 0, Round(vWidth), Round(vHeight));
      Draw(MC, R);
    finally
      FConverting := False;
      MC.Free;
      Transparent := vFlag1;
      IsWithoutBorder := vFlag2;
      Converter.UseSHXFonts := vFlag3;
      CurrentLayout.SetRotMatrix(vPrevRots);
      GetExtents;
    end;
    Result := MF;
  finally
    FExportToDXFMode := False;
  end;
end;
{$ENDIF}
{$ELSE}
procedure TsgCADImage.ExportToMetafile(const FileName: string;
  AWidth, AHeight: Integer);
var
  MF: TMetafile;
begin
  MF := ExportToMetafile(AWidth, AHeight);
  try
    MF.SaveToFile(FileName);
  finally
    MF.Free;
  end;
end;
{$ENDIF}

function TsgCADImage.GetPoint(const P: TFPoint): TPoint;
var
  vFPoint: TFPoint;
begin
  if FUsePrecision then
  begin
    vFPoint := GetFPoint(P);
    Result.X := Round(vFPoint.X / FPrecision);
    Result.Y := Round(vFPoint.Y / FPrecision);
  end
  else
    Result := FPointXMat2DLongint(P, FDraw.Matrix);
end;

function TsgCADImage.GetFPoint(const P: TFPoint): TFPoint;
begin
  Result := FPointXMat(P, FDraw.Matrix);
end;

function TsgCADImage.GetGDIPlusColor(const AColor: TColor): Cardinal;
var
  vIn: array[0..3] of Byte absolute AColor;
  vOut: array[0..3] of Byte absolute Result;
begin
 if (AColor = clByBlock) or (AColor = clByLayer) or (AColor = clNone) then //evg
    Result := GetDrawPenColor
 else
  case FDrawMode of
    dmBlack:
      case GetBackgroundColor of
        clWhite:  Result := IfThen(AColor <> clWhite, clBlack, clWhite);
        clBlack:  Result := IfThen(AColor <> clBlack, clWhite, clBlack);
      else
        Result := GetDrawPenColor;
      end;
    dmGray:
      begin
        Result := ConvertColortoGray(AColor);
      end;
  else
    Result := BGRToRGB(AColor);
  end;
  vOut[3] := 255 - vIn[3];
end;

function TsgCADImage.GetPointUCS(P: TFPoint): TPoint;
begin
  TransformToWorldCS(P);
  Result := GetPoint(P);
end;

function TsgCADImage.GetPolygonFillingMode: Integer;
begin
  Result := ALTERNATE;
end;

function TsgCADImage.GetRealClip: PFRect;
begin
  Result := nil;
  if FClippingRect <> nil then
    Result := FClippingRect;
end;

function TsgCADImage.GetRegenerateArcs: Boolean;
begin
  Result := FConverter.RegenerateArcs;
end;

procedure TsgCADImage.CalcDottedSingPts(AEntity: TsgDXFEntity;
      AEntityLines: sgLines.TsgLines;
      ConvertMatrixCurrentInsertToWCS: TFMatrix;
      ADottedPoints: TFPointList);

begin
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(AEntityLines);
  try
{$ENDIF}
    DXFConv.CalcDottedSingPts(AEntity, AEntityLines,
      ConvertMatrixCurrentInsertToWCS, ADottedPoints, IsAutocadLType,
      IsNotScaledLType);
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(AEntityLines);
  end;
{$ENDIF}
end;

function TsgCADImage.GetPlotSettingsRect: TRect;
begin
  if UsePlotSetting then
  begin
    if CurrentLayout.PlotSettings.PlotType = ptWindowSpecified then
      Result := GetRect(CurrentLayout.PlotSettings.PlotPrintableArea)
    else
      Result := GetRect(CurrentLayout.PlotSettings.PlotMarginsArea);
    InflateRect(Result, 1, 1);
  end
  else
    Result := cnstRectZero;
end;

function TsgCADImage.GetRealImageMatrix: TFMatrix;
begin
  Result := FCurrentLayout.RotMatrix;
  // TransPoseMat(Result);// only for future versions
end;

function TsgCADImage.GetRealImagePoint(const P: TFPoint): TFPoint;
begin
  Result := FPointXMat(P, GetRealImageMatrix);
end;

procedure TsgCADImage.LoadFromFile(const FileName: string);
var
  S: TFileStream;
  Dir: string;
 // Ticks: Cardinal;      //evg
begin
 // Ticks := GetTickCount; //evg
  Dir := GetCurrentDir ;
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
{$IFDEF SG_MODULE_LOG}
    Log.Add('TFileStream.Create(FileName: ' + FileName + ', Mode: $' + IntToHex(fmOpenRead or fmShareDenyNone, 0) + ')');
{$ENDIF}
    SetCurrentDir(ExtractFilePath(FileName));
    Self.FFileName := FileName;
    Converter.FileName := ExtractFileName(FileName);
    Converter.SHXFonts.SearchPath.Insert(0, ExtractFilePath(FileName));
    try
      {// For future versions
      OwnSource := True;
      Converter.SetSourceStream(S); }
      LoadFromStream(S);
    finally
      S.Free;
      SetCurrentDir(Dir);
    end;
  except
    OwnSource := False;
    raise;
  end;
   // Ticks := GetTickCount - Ticks; //evg
  //  ShowMessage('file loading time is ' + FloatToStr(Ticks) + 'mlsek'); //evg
end;

procedure TsgCADImage.SetDefaultViewModePlotSettings;
begin
  ShowPlotSetting := True;
  ShowPlotMargins := True;
  ShowPlotForModel := False;
end;

procedure TsgCADImage.SetDefaultViewPort(AConverter: TsgDXFCOnverter);
begin
  FRotToView := True;
  try
    DoBeforeRotate;
    try
      TsgDXFConverterAccess(AConverter).GetModelLayout.SetRotMatrix(AConverter.ViewTwistMatrix);
    finally
      DoAfterRotate;
    end;
  finally
    FRotToView := False;
  end;
end;

procedure TsgCADImage.LoadFromStream(S: TStream);
var
  vPos: Integer;
begin
  ClearAppInfo(FAppInfo);
{$IFDEF SG_MODULE_LOG}
  Log.Add(ClassName + '($' + IntToHex(TsgNativeUInt(Self), 0) + ').LoadFromStream(S);');
{$ENDIF}
  if KeepOriginal then
  begin
    vPos := S.Position;
    Stream.Clear;
    Stream.CopyFrom(S, S.Size - S.Position);
    S.Position := vPos;
    Stream.Position := 0;
  end;
end;

procedure TsgCADImage.RefreshCurrentLayout;
var
  vBox: TFRect;
begin
  if FCurrentLayout <> nil then
  begin
    vBox := FCurrentLayout.Box;
    if UsePlotSetting then
      vBox := GetDrawingExtents;
    SetExtentsParameters(vBox, FCurrentLayout.CADSpace <> csUndefined);
  end;
end;

procedure TsgCADImage.ResetDrawingBox;
begin
  SetDrawingBox(BadRect);
end;

procedure TsgCADImage.ResetExtents;
var
  W, H: TsgFloat;
begin
  ResetRegenData;
  Extents := MakeFRect(FXMin, FYMax, FZMin, FXMax, FYMin, FZMax);
  W := AbsWidth;
  H := AbsHeight;
  if W < H then
    SwapSGFloats(W,H);
  if H = 0 then
    H := 1E-20;
  if W = 0 then
    W := 1E-20;
  if W < 10 then
    FZoom := 10
  else
  begin
    W := $7FFF / W;
    FZoom := MinDXFSize / H;
    if FZoom < 1 then
      FZoom := 1;
    if FZoom > W then
      FZoom := W;
  end;
end;

procedure TsgCADImage.ResetRegenData;
begin
  FRegenDelta := 0;
end;

procedure TsgCADImage.RestoreDrawParams;
begin
  FDraw := FDrawSaved;
end;

procedure TsgCADImage.Rotate(Axis: TsgAxes; Angle: Extended);
var
  vAngle: Extended;
begin
  vAngle := Angle;
  Angle := Angle - Int(Angle / 360.0) * 360.0;
  if (not (Axis=axisZ) and (Angle <> 0))
    or ((Axis=axisZ) and ((Round(Abs(Angle)) mod 90) <> 0)) then
    FBadRotation := True;
  DoBeforeRotate;
  try
    FCurrentLayout.SetRotMatrix(FMatXMat(FCurrentLayout.RotMatrix,
      BuildRotMatrix(Axis, DegToRad(vAngle))));
    if Axis = axisZ then
      FZRotate := FZRotate + Angle;
  finally
    DoAfterRotate;
  end;
end;

function TsgCADImage.Rotate(const APitch, ATurn, ARoll: Extended): Integer;
var
  vM: TFMatrix;
begin
  Result := Ord(APitch<>0) or (Ord(ATurn<>0) shl 1) or (Ord(ARoll<>0) shl 2);
  if Result <> 0 then
  begin
    DoBeforeRotate;
    try
      case Result of
        1: vM := BuildRotMatrix(axisX, DegToRad(APitch));
        2: vM := BuildRotMatrix(axisY, DegToRad(ATurn));
        3: vM := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), BuildRotMatrix(axisY, DegToRad(ATurn)));
        4: vM := BuildRotMatrix(axisZ, DegToRad(ARoll));
        5: vM := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), BuildRotMatrix(axisZ, DegToRad(ARoll)));
        6: vM := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), FMatXMat(BuildRotMatrix(axisY, DegToRad(ATurn)), BuildRotMatrix(axisZ, DegToRad(ARoll))));
      end;
      FCurrentLayout.SetRotMatrix(FMatXMat(FCurrentLayout.RotMatrix, vM));
      if Result and 2 <> 0 then
        FZRotate := FZRotate + ARoll;
    finally
      DoAfterRotate;
    end;
  end;
end;

procedure TsgCADImage.ClearRegenData;
var
  I: Integer;
  vBlocks: TsgDXFGroup;
begin
  FRegenEntities.Clear;
  vBlocks := Converter.Sections[csBlocks];
  if Assigned(vBlocks) then
  begin
    for I := 0 to vBlocks.Count - 1 do
      TsgDXFBlockAccess(vBlocks.Entities[I]).FRegenFlags := 0;
  end;
end;

procedure TsgCADImage.Regenerate(const AEntities: TsgObjectList = nil);
var
  vDeltaMax,vDX,vDY, vMin, vMax: Double;
  vRegenDeltaRatio: Double;
begin
  GetPixelSize(cnstHordOnScreenByArc, vDX, vDY, True);
  vDeltaMax := Max(vDX, vDY);
  if vDeltaMax < FRegenDelta then
  begin
    vMin := vDeltaMax;
    vMax := FRegenDelta;
  end
  else
  begin
    vMin := FRegenDelta;
    vMax := vDeltaMax;
  end;
  vRegenDeltaRatio := cnstRegenDeltaRatio;
//  if Converter.HighQualityRegenerateArcs then
    //vRegenDeltaRatio := vRegenDeltaRatio * 2;
  if RegenerateArcs and
    (IsZero(FRegenDelta) or IsZero(vMin) or (vMax / vMin >= vRegenDeltaRatio)) then
  begin
    FDraw.RegenScale := cnstIdentityMat;
    RegenerateInternal(CurrentLayout.PaperSpaceBlock, AEntities, vDeltaMax);
  end;
end;

procedure TsgCADImage.RegenerateInternal(ABlock: TsgDXFBlock;
  const AEntities: TsgObjectList; const ADelta: Double);
var
  I: Integer;
  vParams: PsgCADIterate;
  vStatus: TConvStatus;
  vEntity: TsgDXFEntityAccess;
begin
  if Assigned(ABlock) then
  begin
    vStatus := TsgDXFConverterAccess(Converter).Status;
    try
      TsgDXFConverterAccess(Converter).InitRegeneration(ABlock, ADelta);
      ClearRegenData;
      FRegenDelta := ADelta;
      vParams := TsgDXFConverterAccess(Converter).ExchangeParams(@FDraw);
      try
        if Assigned(AEntities) and (AEntities.Count > 0) then
        begin
          for I := 0 to AEntities.Count - 1 do
          begin
            vEntity := TsgDXFEntityAccess(AEntities[I]);
            vEntity.Invoke(Converter, RegenerateProc, RegenerateFinishProc);
          end;
        end
        else
          ABlock.Iterate(Converter, RegenerateProc, RegenerateFinishProc);
      finally
        TsgDXFConverterAccess(Converter).ExchangeParams(vParams);
        ClearRegenData;
      end;
    finally
      TsgDXFConverterAccess(Converter).Status := vStatus;
    end;
  end;
end;

function TsgCADImage.RegenerateFinishProc(AEntity: TsgDXFEntity): Integer;
var
  vItem: TsgRegenStackItem;
begin
  Result := 0;
  case AEntity.EntType of
    ceViewport, ceInsert:
      if FRegenScales.Count > 0 then
      begin
        vItem := TsgRegenStackItem(FRegenScales.Last);
        if vItem.Entity = AEntity then
        begin
          FRegenScales.Delete(FRegenScales.Count - 1);
          FDraw.RegenScale := vItem.Matrix;
          vItem.Free;
        end;
      end;
  end;
end;

function TsgCADImage.RegenerateProc(AEntity: TsgDXFEntity): Integer;

  procedure SaveScale;
  var
    vItem: TsgRegenStackItem;
  begin
    vItem := TsgRegenStackItem.Create(AEntity, FDraw.RegenScale);
    FRegenScales.Add(vItem);
  end;

var
  vInsert: TsgDXFInsert absolute AEntity;
  vViewport: TsgDXFViewportAccess absolute AEntity;
  vXRef: TsgDXFXRef absolute AEntity;
  vXImage: TsgCADImage;
  vConv: TsgDXFConverterAccess;
  vParams: PsgCADIterate;
  I: Integer;
  vIsNeedRegen: Boolean;
  vBlock: TsgDXFBlockAccess;
  vBlockItem: TsgRegenBlockItem;
  vInsertRegenScale: TFMatrix;
  vInsertPixelSize: Double;
begin
  Result := 0;
  case AEntity.EntType of
    ceViewport:
      begin
        SaveScale;
        FDraw.RegenScale := FMatXMat(FDraw.RegenScale, vViewport.CalcMatrix);
        vBlock := TsgDXFBlockAccess(Layouts[0].PaperSpaceBlock);
        if vBlock.FRegenFlags = 0 then
          vBlock.FRegenFlags := 1;
      end;
    ceCircle, ceEllipse, ceArc, ceLWPolyline, cePolyline, ceCurvePolygon, ceHatch:
      begin
        vConv := TsgDXFConverterAccess(AEntity.Converter);
        if Assigned(vConv) then
        begin
          vParams := vConv.ExchangeParams(@FDraw);
          try
            AEntity.Converter.Loads(AEntity);
          finally
            if vParams <> @FDraw then
              vConv.ExchangeParams(vParams);
          end;
        end;
      end;
    ceXRef:
      if Assigned(vXRef.CADImage) and (vXRef.CADImage is TsgCADImage) then
      begin
        vXImage := TsgCADImage(vXRef.CADImage);
        if vXImage.RegenerateArcs <> RegenerateArcs then
          vXImage.RegenerateArcs := RegenerateArcs;
      end;
    ceInsert:
      begin
        Result := 1;
        vInsertRegenScale := FMatXMat(FDraw.RegenScale, vInsert.GetMatrix);
        vInsertPixelSize := GetCADPixelSize(vInsertRegenScale, iMaxNumberOfPart);
        vBlockItem := nil;
        vIsNeedRegen := False;
        I := -1;
        vBlock := TsgDXFBlockAccess(vInsert.Block);
        if Assigned(vBlock) and (vBlock.FRegenFlags > 0) then
          I := FRegenEntities.IndexOf(UInt64(vBlock));
        if I < 0 then
          vIsNeedRegen := Assigned(vBlock)
        else
        begin
          vBlockItem := TsgRegenBlockItem(FRegenEntities.Items[I].Data);
          if vBlockItem.PixelSize > vInsertPixelSize then
            vIsNeedRegen := True;
        end;
        if vIsNeedRegen then
        begin
          if not Assigned(vBlockItem) then
          begin
            vBlockItem := TsgRegenBlockItem.Create;
            FRegenEntities.Add(UInt64(vBlock), vBlockItem);
          end;
          vBlockItem.PixelSize := vInsertPixelSize;
          SaveScale;
          FDraw.RegenScale := vInsertRegenScale;
          vBlock.FRegenFlags := 1;
        end
        else
        begin
          Result := 0;
          if Assigned(vBlock) then
            vBlock.FRegenFlags := 2;
        end;
      end;
{$IFDEF SG_BTI}
    cePolyPolyline2D: AEntity.Converter.Loads(AEntity);
{$ENDIF}
  end;
end;

procedure TsgCADImage.RegenerateWithParts(AParts: Integer);
var
  vNumberParts: Integer;
begin
  vNumberParts := FConverter.NumberOfPartsInCircle;
  try
    FConverter.NumberOfPartsInCircle := AParts;
    RegenerateInternal(CurrentLayout.PaperSpaceBlock, nil, 0);
  finally
    FConverter.NumberOfPartsInCircle := vNumberParts;
  end;
end;

class procedure TsgCADImage.RegisterDestroyNotification(
  const ANotify: TNotifyEvent);
begin
  if not Assigned(CADImageDestroyNotifications) then
    CADImageDestroyNotifications := TsgNotifyEventsList.Create;
  CADImageDestroyNotifications.Add(ANotify);
end;

procedure TsgCADImage.RotateImageEnt;
var
  vProc: TsgIteratorProc;
  vIterator: TsgIteratorBase;
begin
  vProc := TsgRotateImage.Create;
  vIterator := TsgIteratorBase.Create(vProc);
  try
    vIterator.Iterate(Self);
  finally
    vIterator.Free;
    vProc.Free;
  end;
end;

procedure TsgCADImage.RotDefault;
begin
  RotToView(vdDefault);
end;

procedure TsgCADImage.RotToView(const A3DView: TsgDXFViewDirection);
var
  vM: TFMatrix;
begin
  FRotToView := True;
  try
    if A3DView = vdDefault then
    begin
      FBadRotation := False;
      vM := Converter.ViewTwistMatrix;
    end
    else
      vM := cnstIdentityMat;
    DoBeforeRotate;
    try
      FConverter.DoExtents;
      FCurrentLayout.SetRotMatrix(vM);
      case A3DView of
        vdDefault, vdTop:
          begin
            FZRotate := -FCurrentLayout.RotZAngle;
          end;
        vdFront:
          begin
            Rotate(AxisX, -90);
          end;
        vdBottom:
          begin
            Rotate(AxisY, 180);
          end;
        vdLeft:
          begin
            Rotate(AxisY, 90);
            Rotate(AxisZ, 90);
          end;
        vdRight:
          begin
            Rotate(AxisY, -90);
            Rotate(AxisZ, -90);
          end;
        vdBack:
          begin
            Rotate(AxisX, 90);
            Rotate(AxisZ, 180);
          end;
        vdSWIsometric:
          begin
            Rotate(AxisZ, 45);
            Rotate(AxisX, -fIsometricRotation);
          end;
        vdSEIsometric:
          begin
            Rotate(AxisZ, -45);
            Rotate(AxisX, -fIsometricRotation);
          end;
        vdNWIsometric:
          begin
            Rotate(AxisZ, 135);
            Rotate(AxisX, -fIsometricRotation);
          end;
        vdNEIsometric:
          begin
            Rotate(AxisZ, -135);
            Rotate(AxisX, -fIsometricRotation);
          end;
        vdGeodezik:
          begin
            Rotate(AxisX, 0);
            Rotate(axisY, -180);
            Rotate(AxisZ, -90);
          end;
        vdDecard:
          begin
            Rotate(AxisX, 0);
            Rotate(AxisY, 0);
            Rotate(AxisZ, 0);
          end;
      end;
    finally
      DoAfterRotate;
    end;
  finally
    FRotToView := False;
  end;
end;

procedure TsgCADImage.SaveToFile(const FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S)
  finally
    S.Free
  end;
end;

procedure TsgCADImage.SaveToStream(S: TStream);
var
  P: Pointer;
begin
  if KeepOriginal then
  begin
    Stream.Position := 0;
    GetMem(P, Stream.Size);
    try
      Stream.Read(P^, Stream.Size);
      S.Write(P^, Stream.Size);
      Stream.Position := 0;
    finally
      FreeMem(P);
    end;
  end
  else
    if FConverter.Source <> nil then FConverter.Source.SaveToStream(S);
end;

procedure TsgCADImage.ScaleDrawMatrix(ASx, ASy, ASz: Double);
begin
  SetDrawMatrix(FMatScale(DrawMatrix, MakeFPoint(ASx, ASy, ASz)));
end;

procedure TsgCADImage.SetSelectClipRegion(ARegion: TRegion);
var
  vResult: Integer;
  vRegion: TsgRegion;
begin
  vRegion := TsgRegion(ARegion);
  vResult := FContext.RegionSelect(vRegion);
  if vResult <= ERROR then
  begin
    vRegion.FreeReference;
    FContext.RegionSelect(vRegion);
  end;
  UpdateClientClipRect(FContext.ClipRect);
end;

procedure TsgCADImage.SetCurrentLayout(const ALayout: TsgDXFLayout);
var
  vNewLayout: Boolean;
  vBox: TFRect;
begin
  if Assigned(OnLayoutBeforeChange) then
    OnLayoutBeforeChange(Self);
  vNewLayout := (FCurrentLayout <> ALayout) and Assigned(ALayout);
  FCurrentLayout := ALayout;
  if Assigned(FCurrentLayout) then
  begin
    if not FCustomDraw and vNewLayout then
      FDraw.Matrix := FMatScale(GetRealImageMatrix, cnstFPointSingleYMinus);
    vBox := FCurrentLayout.Box;
    if UsePlotSetting then
      vBox := GetDrawingExtents;
    SetExtentsParameters(vBox, FCurrentLayout.CADSpace <> csUndefined);
  end
  else
  begin
    FDraw.Matrix := cnstCrossYMat;
    SetExtentsParameters(cnstBadRect, False);
  end;
  if Assigned(OnLayoutChange) then
    OnLayoutChange(Self);
end;

procedure TsgCADImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
    AssignToBitmap(Self, TBitmap(Dest))
  else
  {$IFDEF SG_DELPHI_VCL}
    if Dest is TMetafile then
      AssignToMetafile(Self, TMetafile(Dest))
    else
  {$ENDIF}
    inherited AssignTo(Dest);
end;

procedure TsgCADImage.AddSelectionMatrix(const APointer: Pointer);
begin
  if (FOnComplexEntity = nil) and
    ((FDraw.Insert = nil) or (FOnInsertMode and (FDraw.Insert = FOnInsert))) then
    SelectionMatrix.AddElement(APointer)
  else
    SelectionMatrix.AddElement(nil);
end;

{$IFDEF SG_FIREMONKEY}
procedure TsgCADImage.ClearImagesCache;
var
  vImageCacheItem: TPair<TsgImageCacheItem,TBitmap>;
begin
  if Assigned(FImagesCache) then
  begin
    for vImageCacheItem in TDictionary<TsgImageCacheItem,TBitmap>(FImagesCache) do
      vImageCacheItem.Value.Free;
    TDictionary<TsgImageCacheItem,TBitmap>(FImagesCache).Clear;
  end;
end;
{$ENDIF}

procedure TsgCADImage.ClearMatrices(const ARect: TRect);
begin
  if SelectionMatrix <> nil then
    SelectionMatrix.ClearRect(ARect);
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(FSnapMatrix);
  try
{$ENDIF}
    FSnapMatrix.ClearRect(ARect);
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(FSnapMatrix);
  end;
{$ENDIF}
end;

function TsgCADImage.CreateConverter: TsgDXFConverter;
begin
  Result := TsgDXFConverter.CreateEx(GetConverterParams);
end;

function TsgCADImage.CreateDefaultViewPort(
  const AHandle: UInt64): TsgDXFViewport;
var
  vBox, vExtents: TFRect;
  vDrawRect: TFRect;
  vDrawW, vDrawH, {vScaleX, vScaleY,} vViewTwistAngle: Double;
  vViewCenter: TFPoint;
  vM, vtm0: TFMatrix;
  vViewSize: TPoint;
begin
  Result := TsgDXFViewPort.Create;
  Result.Handle := AHandle;
  vM := Layouts[0].RotMatrix;
  vBox := Layouts[0].Box;
  vExtents := GetRealBox(vBox, vM);
  Result.PSpaceWidth := vExtents.Right - vExtents.Left;
  Result.PSpaceHeight := vExtents.Top - vExtents.Bottom;
  vViewCenter := MiddleFPoint(vBox.TopLeft, vBox.BottomRight);
  if CurrentLayout.IsModel then
  begin
    vViewSize := FViewportRect.BottomRight;
    if (vViewSize.X <> 0) and (vViewSize.Y <> 0) then
    begin
      vDrawRect := GetRealBox(vBox, DrawMatrix);
      vDrawW := vDrawRect.Right - vDrawRect.Left;
      vDrawH := vDrawRect.Top - vDrawRect.Bottom;
      if  (vDrawW <> 0) and (vDrawH <> 0) then
      begin
        Result.PSpaceWidth := Result.PSpaceWidth * vViewSize.X / vDrawW;
        Result.PSpaceHeight := Result.PSpaceHeight * vViewSize.Y / vDrawH;
        vViewCenter := MakeFPoint(vViewSize.X / 2, vViewSize.Y / 2);
        //vScaleX := (vViewCenter.X - vDrawRect.Left) / vDrawW;
        //vScaleY := 1.0 - (vViewCenter.Y - vDrawRect.Bottom) / vDrawH;
        //vViewCenter := GetCADCoords(vScaleX, vScaleY);
        // or
        //vViewCenter := FPointXMat(vViewCenter, FMatInverse(vCADImage.DrawMatrix));
        // or may be optimal calculation!
        vViewCenter := CADUnProject(0.5 * (FViewportRect.Right + FViewportRect.Left),
          0.5 * (FViewportRect.Top + FViewportRect.Bottom), 0, DrawMatrix);
      end;
    end;
  end;
  if Result.PSpaceHeight = 0 then
    Result.PSpaceHeight := 1;
  if Result.PSpaceWidth = 0 then
    Result.PSpaceWidth := 1;
  Result.PSpaceCenter := FPointXMat(vViewCenter, vM);
  Result.MSpaceCenter := Result.PSpaceCenter;
  Result.ViewTarget := cnstFPointZero;
  // calc ViewDirection and ViewTwistAngle
  Result.ViewDirection := MakeFPoint(vM.EX.Z, vM.EY.Z, vM.EZ.Z);
  vtm0.E0 := cnstFPointZero;
  DoNewAutoCADAxes(Result.ViewDirection, vtm0.EX, vtm0.EY, vtm0.EZ);
  vtm0 := FMatXMat(vtm0, vM);
  if Abs(vtm0.M[0, 0]) >= 1.0 then
  begin
    if vtm0.M[0, 0] < 0 then
      vViewTwistAngle := cnstPi
    else
      vViewTwistAngle := 0
  end
  else
  begin
    vViewTwistAngle := ArcCos(vtm0.M[0, 0]);
    if vtm0.M[0, 1] < 0 then
      vViewTwistAngle := -vViewTwistAngle;
  end;
  Result.ViewTwistAngle := RadToDeg(vViewTwistAngle);
  Result.FrontClipPlane := 0;
  Result.BackClipPlane := 0;
  Result.MSpaceHeight := Result.PSpaceHeight;
end;

function TsgCADImage.CreateDefaultVPort(const Handle: UInt64): TsgDXFVPort;
var
  vViewPort: TsgDXFViewPort;
begin
  Result := TsgDXFVport.Create;
  Result.Name := sActiveVPort;
  Result.Handle := Handle;
  vViewPort := CreateDefaultViewPort(cnstBadHandle);
  try
    Result.ViewHeight := vViewPort.PSpaceHeight;
    if not IsZero(vViewPort.PSpaceHeight) then
      Result.ViewAspectRatio := vViewPort.PSpaceWidth / vViewPort.PSpaceHeight
    else
      Result.ViewAspectRatio := 1;
    Result.ViewCenterPoint := vViewPort.MSpaceCenter;
    Result.ViewTarget := vViewPort.ViewTarget;
    Result.ViewDirection := vViewPort.ViewDirection;
    Result.ViewTwistAngle := vViewPort.ViewTwistAngle;
    Result.CircleZoomPercent := cntViewPortCircleZoomPercent;
    Result.UCSOrigin := cnstFPointZero;
    Result.UCSXDir := cnstXOrtAxis;
    Result.UCSYDir := cnstYOrtAxis;
    Result.UCSVP := False;
  finally
    vViewPort.Free;
  end;
end;

procedure TsgCADImage.CreateRegionStack;
begin
  FreeAndNil(FRegionStack);
  FRegionStack := TsgRegionStack.Create;
end;

function TsgCADImage.CurrXRef: TsgDXFXref;
begin
  if FXrefStack.Count > 0 then
    Result := TsgDXFXref(TsgXrefStackItem(FXrefStack.Last).XRef)
  else
    Result := nil;
end;

//cadimage
procedure TsgCADImage.Draw3DAxes;
var
  PtCntr: TPoint;
  vSize: Single;

  procedure DrawAxis(const AName: string; AAxis: TFPoint; AColor: TColor);
  var
    vPtAxis: TPoint;
  begin
      FContext.PenColor := AColor;
      DoSetColor(AColor, ctPen);
      DoSetColor(AColor, ctFont);
      FContext.FontColor := FContext.PenColor;
      vPtAxis := GetPoint(MakeFPoint(FCenter.X + AAxis.X,
        FCenter.Y + AAxis.Y, FCenter.Z + AAxis.Z));
      FContext.Line(PtCntr.X, PtCntr.Y, vPtAxis.X, vPtAxis.Y);
      FContext.DoTextOut(vPtAxis.X, vPtAxis.Y, AName);
  end;

begin
  if not IsIterator then
  begin
    vSize := AbsWidth;
    if vSize > AbsHeight then
      vSize := AbsHeight;
    vSize := vSize / 4;
    if vSize <= 0 then
      vSize := 1;
    PtCntr := GetPoint(FCenter);
    if not FExpProcs.Active then
      FContext.PenWidth := 2;
    DrawAxis('X', MakeFPoint(vSize,0,0), clRed);// FNew0X
    DrawAxis('Y', MakeFPoint(0,vSize,0), TColor(ConvertRGBtoColor(68, 235, 140)));//FNew0Y
    DrawAxis('Z', MakeFPoint(0,0,vSize), clBlue);// FNew0Z
    if not FExpProcs.Active then
    begin
      FContext.FontHeight := 12;
      FContext.PenColor := clAqua;
      FContext.Ellipse(PtCntr.X - 1, PtCntr.Y - 1, PtCntr.X + 1, PtCntr.Y + 1);
    end;
  end;
end;

function TsgCADImage.CoordinateBorderCorrect(var AXScaled, AYScaled: Extended): Boolean;
var
  vTemp: TsgFloat;
  vBorderRatio, vCorrectedBorderRatio: TsgFloat;
  vExtents: TFRect;
begin
  Result := True;
  vExtents := GetDrawingExtents;
  if not IsWithoutBorder then
    if BorderType = btRatio then
    begin
      vCorrectedBorderRatio := BorderSize + FXDisproportionateShift;
      AXScaled := AXScaled * (1 + 2 * vCorrectedBorderRatio) - vCorrectedBorderRatio;
      vCorrectedBorderRatio := BorderSize + FYDisproportionateShift;
      AYScaled := AYScaled * (1 + 2 * vCorrectedBorderRatio) - vCorrectedBorderRatio;
    end
    else
      if BorderType = btGlobal then
      begin
        vTemp := vExtents.Right - vExtents.Left - 2 * BorderSize;
        if vTemp <> 0 then
          vBorderRatio := BorderSize / vTemp
        else
        begin
          Result := False;
          Exit;
        end;
        vCorrectedBorderRatio := vBorderRatio + FXDisproportionateShift;
        AXScaled := AXScaled * (1 + 2 * vCorrectedBorderRatio) - vCorrectedBorderRatio;
        vTemp := vExtents.Top - vExtents.Bottom - 2 * BorderSize;
        if vTemp <> 0 then
          vBorderRatio := BorderSize / vTemp
        else
        begin
          Result := False;
          Exit;
        end;
        vCorrectedBorderRatio := vBorderRatio + FYDisproportionateShift;
        AYScaled := AYScaled * (1 + 2 * vCorrectedBorderRatio) - vCorrectedBorderRatio;
      end
      else
        Result := False;
end;

procedure TsgCADImage.FillBoxForCoordsCalc(ACoord: Integer; var ABox: TFRect);
begin
  case ACoord of
    1:
    begin
      ABox.Left := FCurrentLayout.Box.Z1;
      ABox.Right := FCurrentLayout.Box.Z2;
      ABox.Bottom := FCurrentLayout.Box.Bottom;
      ABox.Top := FCurrentLayout.Box.Top;
      ABox.Z1 := FCurrentLayout.Box.Left;
      ABox.Z2 := FCurrentLayout.Box.Right;
    end;
    2:
    begin
      ABox.Left := FCurrentLayout.Box.Left;
      ABox.Right := FCurrentLayout.Box.Right;
      ABox.Bottom := FCurrentLayout.Box.Z1;
      ABox.Top := FCurrentLayout.Box.Z2;
      ABox.Z1 := FCurrentLayout.Box.Bottom;
      ABox.Z2 := FCurrentLayout.Box.Top;
    end;
  else
    ABox.Left := FCurrentLayout.Box.Left;
    ABox.Right := FCurrentLayout.Box.Right;
    ABox.Bottom := FCurrentLayout.Box.Bottom;
    ABox.Top := FCurrentLayout.Box.Top;
    ABox.Z1 := FCurrentLayout.Box.Z1;
    ABox.Z2 := FCurrentLayout.Box.Z2;
  end;
end;

function TsgCADImage.IsAutocadFormat: Boolean;
begin
  Result := False;
end;

function TsgCADImage.IsAutocadLType: Boolean;
begin
  Result := False;
end;

function TsgCADImage.IsImageWithColorsRGB: Boolean;
begin
  Result := False;
end;

function TsgCADImage.IsDisableSnapMatrix: Boolean;
begin
  Result := (osDisabled in FObjectSnapMask) or (FObjectSnapMask = []);
end;

function TsgCADImage.Load2DPointToPoints(const AList: TF2DPointList): Integer;
var
  P: TFPoint;
begin
  Result := 0;
  P.Z := 0;
  while (Result < AList.Count){$IFDEF SG_THREAD_DRAW} and not FDraw.Stopped{$ENDIF} do// prepares TPoint array for GDI
  begin
    P.Point2D := AList.List[Result];
    FContext.AddPolyPoint(P);
    Inc(Result);
  end;
end;

function TsgCADImage.IsDrawOnBitMap(P: PsgCADIterate): Boolean;
begin
  Result := (FSelectionMatrix <> nil) and (not FSelectionMatrix.IsOverflow) and
    (Assigned(P) and not Assigned(P^.Viewport)) and
    ((GetMatrixMode <> smDisabled) or FOnInsertMode);
end;

function TsgCADImage.IsDrawOnBitMap: Boolean;
begin
  Result := IsDrawOnBitMap(Converter.Params);
end;

function TsgCADImage.IsDrawOnCanvas: Boolean;
begin
  Result := GetMatrixMode <> smMatrixOnly;
end;

function TsgCADImage.IsRectInRegion(ARect: TRect): Boolean;
begin
  Result:= True;
  if Assigned(FRegionStack) and (FRegionStack.Count > 0) and TsgRegion(FRegionStack.Top).HandleAllocated then
  begin
{$IFDEF MSWINDOWS}
    if not FExpProcs.Active then
      LPtoDP(FContext.GetDC, ARect, 2);
{$ENDIF}
    Result := FRegionStack.RectInRegion(FContext, ARect);
  end;
end;

function TsgCADImage.IsRotatedText(const AText: TsgDXFText; AHeight: Single;
  const ADrawMatrix: TFMatrix): Boolean;
begin
  Result := AText.UpsideDown or AText.Backward
    or IsRotatedTextInternal(AText, AHeight, ADrawMatrix);
end;

function TsgCADImage.IsRotatedTextInternal(const AText: TsgDXFText; AHeight: Single;
      const ADrawMatrix: TFMatrix): Boolean;
var
  vMaxTextHeight: Double;
  vPoint: PPoint;
begin
  vMaxTextHeight := cnstMaxTextHeight;
  if FExpProcs.Active then
    vMaxTextHeight := cnstMaxTextHeight * 100;
  Result := (AHeight > vMaxTextHeight) or
   (Abs(AText.Scale - 1) > fAccuracy) or
   ((AText.Point.Z + Integer(TsgDXFTextAccess(AText).HasSecond) * AText.Point1.Z) <> 0) or
   (not IsEqual(AText.Tracking, 1));
  if not Result then
  begin
    vPoint := nil;
    if FExpProcs.Active then
      vPoint := @FNormalsInternal;
    Result := IsRotatedFMatEx(ADrawMatrix, vPoint, True);
  end;
end;

function TsgCADImage.IsStopLoading: Boolean;
begin
  Result := TsgDXFConverterAccess(FConverter).LoadStopped;
end;

function TsgCADImage.CalcCADCoordsParameters({AXScaled, AYScaled: Extended;
         var AX, AY: TsgFloat; var ARot: TFMatrix;
         var ARealLayoutCenter: TFPoint; var ANullCoord: Integer}var AStruct: TsgCalcCoordsStruct): Boolean;
var
  vWidth, vHeight, vZWidth, vZHeight, vZXShift, vZYShift: TsgFloat;
  vRot: TFMatrix;
  vBox: TFRect;
  vXScaledCorrect, vYScaledCorrect: Extended;

  procedure AssignCoords(I: Integer);
  var
    M: TFMatrix;
  begin
    AStruct.NullCoord := I;
    M := GetRealImageMatrix;
    FillBoxForCoordsCalc(I, vBox);
    case I of
      1:
      begin
        vRot.M[0, 0] := M.M[2, 0];
        vRot.M[1, 1] := M.M[1, 1];
        vRot.M[2, 2] := 0;
        vRot.M[0, 1] := M.M[2, 1];
        vRot.M[0, 2] := 0;
        vRot.M[1, 0] := M.M[1, 0];
        vRot.M[1, 2] := 0;
        vRot.M[2, 0] := 0;
        vRot.M[2, 1] := 0;
      end;
      2:
      begin
        vRot.M[0, 0] := M.M[0, 0];
        vRot.M[1, 1] := M.M[2, 1];
        vRot.M[2, 2] := 0;
        vRot.M[0, 1] := M.M[0, 1];
        vRot.M[0, 2] := 0;
        vRot.M[1, 0] := M.M[2, 0];
        vRot.M[1, 2] := 0;
        vRot.M[2, 0] := 0;
        vRot.M[2, 1] := 0;
      end;
    else
      vRot := M;
    end;
    vWidth := Abs((vBox.Right - vBox.Left) * vRot.M[0, 0]) + Abs((vBox.Top - vBox.Bottom) * vRot.M[1, 0]);
    vHeight := Abs((vBox.Top - vBox.Bottom) * vRot.M[1, 1]) + Abs((vBox.Right - vBox.Left) * vRot.M[0, 1]);
  end;

begin
  Result := True;
  if IsBadRect(FCurrentLayout.Box) then
  begin
    Result := False;
    Exit;
  end;

  vXScaledCorrect := AStruct.XScaled;
  vYScaledCorrect := AStruct.YScaled;
  if not CoordinateBorderCorrect(vXScaledCorrect, vYScaledCorrect) then
  begin
    Result := False;
    Exit;
  end;

  AssignCoords(0);
  if Abs(Abs(vRot.M[1, 2]) - 1) < fAccuracy then
    AssignCoords(2)
  else
    if Abs(Abs(vRot.M[0, 2]) - 1) < fAccuracy then
    AssignCoords(1);
  if (vWidth = 0) or (vHeight = 0) then
  begin
    Result := False;
    Exit;
  end;

  vZWidth := Abs((vBox.Z2 - vBox.Z1) * vRot.M[2, 0]);
  vZHeight := Abs((vBox.Z2 - vBox.Z1) * vRot.M[2, 1]);
  vZXShift := vBox.Z1 * vRot.M[2, 0] / (vZWidth + vWidth);
  vZYShift := vBox.Z1 * vRot.M[2, 1] / (vZHeight + vHeight);
  AStruct.RealLayoutCenter.X := 0.5 * (vBox.Right + vBox.Left);
  AStruct.RealLayoutCenter.Y := 0.5 * (vBox.Top + vBox.Bottom);
  AStruct.RealLayoutCenter.Z := 0.0;

  if vRot.M[2, 0] > 0 then
    AStruct.X := vWidth * ((vXScaledCorrect + vZXShift) * (vZWidth + vWidth) / vWidth - 0.5)
  else
    AStruct.X := vWidth * ((vXScaledCorrect - vZWidth / (vZWidth + vWidth) + vZXShift) * (vZWidth + vWidth) / vWidth - 0.5);
  if vRot.M[2, 1] > 0 then
    AStruct.Y := vHeight * ((vYScaledCorrect + vZYShift) * (vZHeight + vHeight) / vHeight - 0.5)
  else
    AStruct.Y := vHeight * ((vYScaledCorrect - vZHeight / (vZHeight + vHeight) + vZYShift) * (vZHeight + vHeight) / vHeight - 0.5);
  AStruct.Rot := vRot;
end;

function TsgCADImage.CheckActiveVPort: Boolean;
begin
  Result := TsgDXFConverterAccess(Converter).CheckActiveVPort(AbsHeight);
end;

procedure TsgCADImage.CheckColorToLineWeight;
begin
  FColorToLineWeightMap.Update(2 or Ord(GetShowLineWeight));
end;

function TsgCADImage.CheckOrthoRaster(const AAngle: Double): Boolean;
begin
  Result := not FBadRotation and IsEqual((Round(AAngle) div 90) * 90, AAngle);
end;

{$IFNDEF SG_FIREMONKEY}
function TsgCADImage.CheckRastrByFastMode(Sender: TObject;
  var AGraphic: TGraphic; const APoints: TsgPointsListHelper): Boolean;
var
  vImageEnt: TsgDXFImageEntAccess absolute Sender;
  vHS, vWS, vHI, vWI: Double;
  vScaleMode: Integer;
{$IFDEF SG_THREAD_DRAW}
  vReference: TObject;
{$ENDIF}
begin
  Result := not (vImageEnt.GetAnimate {$IFDEF SG_DELPHI_VCL}or (vImageEnt.Picture.Graphic is TMetafile){$ENDIF});
  if Result then
  begin
    if FCachingRasterMode <> crmNone then
    begin
      vImageEnt.ChangeGraphic;
      vWS := DistancePoint(APoints.Points[0], APoints.Points[1]);
      vHS := DistancePoint(APoints.Points[0], APoints.Points[3]);
      if FCachingRasterMode = crmDoubleCache then
      begin
        vHI := vImageEnt.Picture.Graphic.Width / 8;
        vWI := vImageEnt.Picture.Graphic.Height / 8;
        if (vWI >= vWS) and (vHI >= vHS) then
          vScaleMode := 8
        else
        begin
          vHI := vImageEnt.Picture.Graphic.Width / 4;
          vWI := vImageEnt.Picture.Graphic.Height / 4;
          if (vWI >= vWS) and (vHI >= vHS) then
            vScaleMode := 4
          else
            vScaleMode := 0;
        end;
      end
      else
      begin
        vHI := vImageEnt.Picture.Graphic.Width / 4;
        vWI := vImageEnt.Picture.Graphic.Height / 4;
        if (vWI >= vWS) and (vHI >= vHS) then
          vScaleMode := 4
        else
          vScaleMode := 0;
      end;
      if vScaleMode > 0 then
      begin
        if (vImageEnt.FScaleMode <> vScaleMode) and ((vImageEnt.FBitMap = nil) or (TsgBitMap(vImageEnt.Picture.Graphic).PixelFormat = pf1bit)) then
        begin
{$IFNDEF SGFPC}
{$IFDEF SG_THREAD_DRAW}
          if GetCurrentThreadId <> MainThreadID then
          begin
            vReference := vImageEnt.FBitMap;
            vImageEnt.FBitMap := nil;
            TThread.Queue(nil, procedure
              begin
                vImageEnt.CreateImgLittle(vScaleMode);
                vReference.Free;
              end);
          end
          else
{$ELSE}
          vImageEnt.CreateImgLittle(vScaleMode);
{$ENDIF}
{$ELSE}
          vImageEnt.CreateImgLittle(vScaleMode);
{$ENDIF}
        end;
        if vImageEnt.FBitMap <> nil then
          AGraphic := vImageEnt.FBitMap
        else
          AGraphic := vImageEnt.Picture.Graphic;
      end
      else
        AGraphic := vImageEnt.Picture.Graphic;
    end
    else
      AGraphic := vImageEnt.Picture.Graphic;
  end
  else
    AGraphic := vImageEnt.Picture.Graphic;
end;
{$ENDIF}

function TsgCADImage.GetCADCoords(const AXScaled, AYScaled: Extended): TFPoint;
var
  C, N, A, B, ASubB, Intersect: TFPoint;
  T, NMulC: TsgFloat;
  SubResult: TFPoint;
  vStruct: TsgCalcCoordsStruct;

  procedure Transpose(var A: TFMatrix);
  begin
    SwapSGFloats(A.M[0,1], A.M[1,0]);
    SwapSGFloats(A.M[0,2], A.M[2,0]);
    SwapSGFloats(A.M[2,1], A.M[1,2]);
  end;

begin
  vStruct.XScaled := AXScaled;
  vStruct.YScaled := AYScaled;
  if not CalcCADCoordsParameters(vStruct) then
  begin
    Result := MakeFPoint(0, 0, 0);
    Exit;
  end;

  C := MakeFPoint(0.0, 0.0, -1.0);
  N := FPointXMat(MakeFPoint(0.0, 0.0, 1.0), vStruct.Rot);
  A := MakeFPoint(vStruct.X, vStruct.Y, 0.0);
  B := MakeFPoint(0.0, 0.0, 0.0);
  ASubB := MakeFPoint(B.X - A.X, B.Y - A.Y, B.Z - A.Z);
  NMulC := ScalarMultiplyVectors(N, C);
  if NMulC <> 0 then //It is always not null
    T := ScalarMultiplyVectors(N, ASubB) / NMulC
  else
    T := 0;
  Intersect := MakeFPoint(A.X + C.X * T, A.Y + C.Y * T, A.Z + C.Z * T);
  Transpose(vStruct.Rot);
  SubResult := FPointXMat(Intersect, vStruct.Rot);
  case vStruct.NullCoord of
    1:
    begin
      Result.X := 0;
      Result.Y := SubResult.Y + vStruct.RealLayoutCenter.Y;
      Result.Z := SubResult.X + vStruct.RealLayoutCenter.X;
    end;
    2:
    begin
      Result.X := SubResult.X + vStruct.RealLayoutCenter.X;
      Result.Y := 0;
      Result.Z := SubResult.Y + vStruct.RealLayoutCenter.Y;
    end;
  else
    Result.X := SubResult.X + vStruct.RealLayoutCenter.X;
    Result.Y := SubResult.Y + vStruct.RealLayoutCenter.Y;
    Result.Z := 0;
  end;
end;

function TsgCADImage.GetCADCoords(const AXScaled, AYScaled: Extended;
  var CoordsInUCS: TFPoint): TFPoint;
begin
  Result := GetCADCoords(AXScaled, AYScaled);
  CoordsInUCS := Result;
  TransformToUCS(CoordsInUCS);
end;

function TsgCADImage.GetCanvas: TCanvas;
begin
  Result := TCanvas(FContext.Canvas);
end;

function TsgCADImage.GetClipping: TsgClipping;
begin
  if Assigned(FClippingRect) then
  begin
    Result.ClipMode := 1;
    Result.ClipRect := FClippingRect^;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TsgCADImage.GetColorToLineWeight(var AContextPenColor: TsgColorCAD;
  const AEntColor: PsgColorCAD): Boolean;
begin
  AContextPenColor := cnstColorCADNone;
  if FExpProcs.Active then
  begin
    Result := False;
    if (AEntColor <> nil) and FExpProcs.SupportColorToLineWeight then
    begin
      Result := True;
      AContextPenColor := AEntColor^;
    end;
  end
  else
  begin
    Result := True;
    if AEntColor <> nil then
      AContextPenColor := AEntColor^
    else
    begin
      AContextPenColor.Active := acRGBColor;
      AContextPenColor.Color := FContext.PenColor;
    end;
  end;
end;

function TsgCADImage.GetColorToLineWeightList: TStringList;
begin
  Result := FColorToLineWeightMap.Strings;
end;

function TsgCADImage.GetConverterParams: TsgConverterParams;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TsgCADImage.GetDefaultColor: TColor;
begin
  Result := FDefaultColor;
end;

function TsgCADImage.GetDefaultImageSize: TFRect;
begin
  Result := SGDefaultImageSize(TsgInsUnits(Converter.HeadVarStruct.InsUnits) <> iuMeters);
end;

function TsgCADImage.GetDefaultView: TsgDefaultView;
begin
  Result := df2D;
end;

function TsgCADImage.GetDefSolidRotView: TsgDXFViewDirection;
begin
  Result := vdSEIsometric;
end;

function TsgCADImage.GetDrawBackgroundColor: TColor;
begin
  if FPrinting = pntPrint then
    Result := cnstPrintBackground
  else
    Result := BackgroundColor;
end;

function TsgCADImage.GetDrawingExtents: TFRect;
begin
  if UsePlotSetting then
  begin
    Result := CurrentLayout.PlotSettings.PlotPaperArea;
    SwapDoubles(Result.Top, Result.Bottom);
  end
  else
    Result := Extents;
end;

function TsgCADImage.GetDrawPenColor: TColor;
begin
  if FPrinting = pntPrint then
    Result := cnstPrintDefaultColor
  else
    Result := DefaultColor;
end;

function TsgCADImage.GetSnapData(const P: TPoint): PSnapData;
begin
  Result := FSnapMatrix.Cells[P.X, P.Y];
end;

function TsgCADImage.GetObjectSnapMask: TObjectSnapState;
begin
  Result := FObjectSnapMask;
end;

procedure TsgCADImage.SetObjectSnapMask(const AValue: TObjectSnapState;
  const AUpdate: Boolean = True);
var
  B: Boolean;
begin
  SetObjectSnapMaskEx(AValue);
  SetupSnapMatrix;
  if AUpdate and (not IsDisableSnapMatrix) then
  begin
    if Converter.Params <> nil then
    begin
      FSnapOnlyIterate := True;
      B := Converter.AutoInsert;
  {$IFDEF SG_FIREMONKEY}
      try
        FContextSelectionMatrix.BeginScene;
  {$ENDIF}
        try
          Converter.AutoInsert := (osAll * FObjectSnapMask) = osAll;
          FCurrentLayout.Iterate(Converter, DrawEntity, DoFinish);
        finally
          Converter.AutoInsert := B;
          FSnapOnlyIterate := False;
        end;
  {$IFDEF SG_FIREMONKEY}
      finally
        FContextSelectionMatrix.EndScene;
      end;
  {$ENDIF}
    end;
  end;
end;

procedure TsgCADImage.SetObjectSnapMaskEx(const Value: TObjectSnapState);
begin
  FObjectSnapMask := Value;
end;

procedure TsgCADImage.SetOffset(const AValue: TFPoint);
begin
  FOffset := AValue;
end;

procedure TsgCADImage.SetupSnapMatrix;
begin
  if not Assigned(FSnapMatrix) then Exit;
  // Clear All snap matrix
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(FSnapMatrix);
  try
{$ENDIF}
    FSnapMatrix.Clear;
    FSnapMatrix.Resize(FViewportRect.Right, FViewportRect.Bottom);
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(FSnapMatrix);
  end;
{$ENDIF}
end;

 //evg remove double here!!!!
procedure TsgCADImage.SetPixelSnapEx(const APt: TFPoint; Pt: TPoint;
  const ASnapState: TObjectSnapState);
var
  vSnapMatrixElement: TSnapData;
begin
  if (ASnapState * FObjectSnapMask <> []) { and
     ((Abs(FSnapLastPoint.X - APt.X) > FDxyz.X) or
      (Abs(FSnapLastPoint.Y - APt.Y) > FDxyz.Y)) - this check is bad for "two lines in one point" tracers} then
  begin
    FSnapLastPoint := APt;
    if sgClientRectForSnap.Left <> MaxInt then
      Pt.X := Pt.X - sgClientRectForSnap.Left;
    if sgClientRectForSnap.Top <> MaxInt then
      Pt.Y := Pt.Y - sgClientRectForSnap.Top;
    if FSnapMatrix.IsCellValid(Pt.X, Pt.Y) then
    begin
      vSnapMatrixElement.CADPoint := FPointXMat(APt, Converter.GetTransform3D);
      vSnapMatrixElement.Entity := FSnapEntity;
      vSnapMatrixElement.EntityIntersected := nil;
      vSnapMatrixElement.SnapMode := ASnapState;
      vSnapMatrixElement.SnapType := osDisabled;
      vSnapMatrixElement.Insert := FDraw.Insert;
      vSnapMatrixElement.CADPointInUCS := vSnapMatrixElement.CADPoint;
{$IFDEF SG_THREAD_DRAW}
      MonitorEnter(FSnapMatrix);
      try
{$ENDIF}
        FSnapMatrix.UpdateCell(Pt.X, Pt.Y, vSnapMatrixElement);
{$IFDEF SG_THREAD_DRAW}
      finally
        MonitorExit(FSnapMatrix);
      end;
{$ENDIF}
    end;
  end;
end;

procedure TsgCADImage.SetRegion(const ASender: TObject;
  const ARegion: TRegion);
var
  vRegion: TsgRegion;
  vEntType: TsgCADEntities;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
  P: TPoint;
{$ENDIF}
begin
  vRegion := TsgRegion(ARegion);
  vEntType := ceEntity;
  if Assigned(ASender) then
    vEntType := TsgDXFEntity(ASender).EntType;
  if not Assigned(ASender) or (not (vEntType in [ceInsert, ceViewport])) then
    FContext.RegionOffset(vRegion);
  if FContext.RegionUpdate(TsgRegion(FCanvasRegion)) > 0 then
    vRegion.Intersect(FCanvasRegion);
  if IsDrawOnCanvas then
    SetSelectClipRegion(vRegion);
  if IsDrawOnBitMap then
  begin
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    if vEntType <> ceViewport then
    begin
      P := Point(0, 0);
      LPtoDP(FContext.GetDC, P, 1);
      vRegion.Offset(Point(-P.X, -P.Y));
    end;
{$ENDIF}
    FContextSelectionMatrix.RegionSelect(vRegion);
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    if vEntType <> ceViewport then
      vRegion.Offset(P);
{$ENDIF}
    if not IsDrawOnCanvas then
      UpdateClientClipRect(FContextSelectionMatrix.ClipRect);
  end;
end;

procedure TsgCADImage.SetPixelSnap(const APt: TFPoint;
  const ASnapState: TObjectSnapState);
var
  Pt: TPoint;
begin
  Pt := GetPoint(APt);
  SetPixelSnapEx(APt, Pt, ASnapState);
end;

procedure TsgCADImage.SetCanvas(const ACanvas: TCanvas);
begin
  FContext.Canvas := {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ENDIF}(ACanvas);
end;

procedure TsgCADImage.SetClip(AInsert: TsgDXFInsert; AFilter: TObject);
begin
  ExpSaveDCInternal(nil);
  DoScale2D(FDraw);
  if (FDraw.XScale = 0) or (FDraw.YScale = 0) then Exit;
  if not (IsDrawOnBitMap or IsDrawOnCanvas) then Exit;
  if AInsert = AFilter then // see HasClip method
    DoSetClipInsert(TsgCADClipInsert(AInsert))
  else
    if AFilter is TsgCADSpatialFilter then
      DoSetSpatialClip(AInsert, TsgDXFEntity(AFilter));
end;

procedure TsgCADImage.DoSetClipInsert(AInsert: TsgCADClipInsert);
var
  vRegion: TsgRegion;
  F: TFRect;
  R: array[0..3] of TPoint;

  function GetRgnPoint(X, Y, DX, DY: TsgFloat): TPoint;
  var
    P: TPoint;
  begin
    Result := GetPoint(MakeFPoint(X, Y, 0));
    while True do
    begin
      P := GetPoint(MakeFPoint(X+DX,Y+DY,0));
      if (P.X <> Result.X) and (P.Y <> Result.Y) then
        Break;
      DX := DX * 2;
      DY := DY * 2;
    end;
    if P.X > Result.X then
      Inc(Result.X)
    else
      Dec(Result.X);
    if P.Y > Result.Y then
      Inc(Result.Y)
    else
      Dec(Result.Y);
  end;
begin
  F := AInsert.Box;
  if IsBadRect(F) then Exit;
  R[0] := GetRgnPoint(F.Left, F.Top, -1, 1);
  R[1] := GetRgnPoint(F.Right, F.Top, 1, 1);
  R[2] := GetRgnPoint(F.Right, F.Bottom, 1, -1);
  R[3] := GetRgnPoint(F.Left, F.Bottom, -1, -1);
  vRegion := TsgRegion.Create;
  try
    vRegion.SetPolygon(R, 4, WINDING);
    if not FExpProcs.Active then
      FContext.RegionOffset(vRegion);
    ExpClipRegionInternal(AInsert, vRegion);
  finally
    vRegion.Free;
  end;
end;

procedure TsgCADImage.DoSetSpatialClip(AInsert: TsgDXFInsert; AFilter: TsgDXFEntity);
var
  vRegion: TsgRegion;
  I: Integer;
  vSpatialFilter: TsgCADSpatialFilterAccess;
  vBounds: array of TFPoint;
  vBoundsBox: TFRect;
  vT: TFMatrix;
begin
  FContext.IntPoints.Count := 0;
  vSpatialFilter := TsgCADSpatialFilterAccess(AFilter);
  if vSpatialFilter.Bounds.Count = 2 then
  begin
    SetLength(vBounds, 4);
    vBoundsBox := PFRect(vSpatialFilter.Bounds.List)^;
    SwapDoubles(vBoundsBox.Top, vBoundsBox.Bottom);
    GetPts4ByBox(vBoundsBox, PsgPoints4(vBounds)^);
  end
  else
  begin
    SetLength(vBounds, vSpatialFilter.Bounds.Count);
    System.Move(vSpatialFilter.Bounds.List^, vBounds[0], Length(vBounds) * SizeOf(vBounds[0]));
  end;
  try
    if Length(vBounds) > 0 then
    begin
      vT := vSpatialFilter.GetBoundTransformation(TsgDXFInsertAccess(AInsert).GetMatrix);
      for I := 0 to High(vBounds) do
        vBounds[I] := FPointXMat(vBounds[I], vT);
      for I := 0 to High(vBounds) do
        FContext.AddIntPoint(vBounds[I]);
    end;
    if FContext.IntPoints.Count >= 3 * 2 then
    begin
      vRegion := TsgRegion.Create;
      try
        vRegion.SetPolygonList(FContext.IntPoints, WINDING);
        if vSpatialFilter.DisplayBounds and (Converter.HeadVarStruct.XClipFrame <> 0) then
        begin
          FContext.AddIntFirstPoint(0);
          ExpPolylineInteger(AInsert, FContext.IntPoints);
        end;
        if not FExpProcs.Active then
          FContext.RegionOffset(vRegion);
        ExpClipRegionInternal(AInsert, vRegion);
      finally
        vRegion.Free;
      end;
    end;
  finally
    Finalize(vBounds);
  end;
  FContext.IntPoints.Count := 0;
end;

procedure TsgCADImage.Draw(Canvas: TCanvas; const Rect: TRect);
var
{$IFNDEF SG_NON_WIN_PLATFORM}
  vPrevMode, vTextAlign: Integer;
{$ENDIF}
  OldPal: HPalette;
  vRect, R: TRect;
  vSelectionMatrixMode: TsgSelectionMode;

  vSelectionMatrix: TsgSelectionMatrix;
  vClearfFull: Boolean;
  vSnapMask: TObjectSnapState;
  vOldConverting: Boolean;
  vClippingRect: PFRect;
  vDPIScale, vLineWeightScale: Double;
{$IFNDEF SG_FM_UNIX}
  vRegion: TsgRegion;
 {$ELSE}
 vCanvasState: TCanvasSaveState;
 {$ENDIF}
 {$IFDEF SHOWTICKS}   Ticks: Cardinal;    {$ENDIF}  //evg
  vDX,vDY: Double;
{$IFDEF SG_THREAD_DRAW}
  vSelectionMatrixLocked: Boolean;
{$ENDIF}

  function ClipPtToIntPt(AClipPt: TFPoint): TPoint;
  var
    vCADPt: TFPoint;
  begin
    vCADPt := MakeFPoint(Extents.Left + AClipPt.X, Extents.Top - AClipPt.Y);
    Result := GetPoint(vCADPt);
  end;

begin
  Drawing := Self;
  try
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_LINUX}
  FIsPrinterCanvas := IsPrinterCanvas(Canvas);
{$ENDIF}
  SetViewportRect(TRect.Create(0 , 0, Canvas.Width, Canvas.Height));
  Canvas.Stroke.Cap := TStrokeCap.Round;
{$ENDIF}
  if FCurrentLayout = nil then Exit;
  {$IFDEF SHOWTICKS}  Ticks := GetTickCount; {$endif}//evg
  //SetupSnapMatrix;
  //Regenerate;
  vLineWeightScale := LineWeightScale;
  vSelectionMatrixMode := GetMatrixMode;
  vSelectionMatrix := nil;
  if Assigned(SelectionMatrix) then
  begin
{$IFNDEF USE_SG_AGG2D}
    FContextSelectionMatrix.Canvas := nil;
{$ENDIF}
    vSelectionMatrix := SelectionMatrix;
{$IFDEF SG_THREAD_DRAW}
    vSelectionMatrixLocked := MonitorTryEnter(vSelectionMatrix);
    try
{$ENDIF}
      if (vSelectionMatrixMode <> smDisabled) or (FOnInsert <> nil) then
        vSelectionMatrix.Size := FViewportRect.BottomRight
      else
        vSelectionMatrix.Size := Point(1, 1);
{$IFDEF SG_THREAD_DRAW}
    finally
      if vSelectionMatrixLocked then
        MonitorExit(vSelectionMatrix);
    end;
{$ENDIF}
  end;
  vClippingRect := GetRealClip;

  vRect := Rect;
  if (BorderType = btPixel) and (BorderSize <> 0) then
    InflateRect(vRect, -Round(BorderSize), -Round(BorderSize));
  vRect := GetDrawRect(vRect, GetDrawingExtents, vClippingRect);

  FContext.Canvas := {$IFDEF USE_SG_AGG2D}TAggLCLCanvas{$ENDIF}(Canvas);
  FContext.SetMatrix(@FDraw.Matrix);
  if FExpProcs.Active then
    FExpProcs.SetMatrix(@FDraw.Matrix);
  UpdateContextSelectionMatrix;
  try
    if FCustomDraw then
      ApplyScale(vRect);
  except
    Exit
  end;

  Regenerate;

{$IFNDEF SG_NON_WIN_PLATFORM}
  vPrevMode := FContext.SetGraphicsMode(GM_COMPATIBLE);// GM_ADVANCED
{$ENDIF}
  OldPal := FContext.SelectPalette(AcadPal, True);
{$IFNDEF SG_FIREMONKEY}
{$IFDEF USE_SG_AGG2D}
  FmmToPixelX := cnstMMPerInch / FContext.PixelsPerInch;
{$ELSE}
  FmmToPixelX := FContext.GetDeviceCaps(HORZSIZE) / FContext.GetDeviceCaps(HORZRES);
{$ENDIF}
{$ELSE}
  FmmToPixelX := FContext.GetMMToPixel;
{$ENDIF}
  if FDeviceDPI.X > 0 then
  begin
    vDPIScale :=  FDeviceDPI.X * FmmToPixelX / 25.4;
    LineWeightScale := LineWeightScale * vDPIScale;
  end;
{$IFNDEF USE_SG_AGG2D}
  FCanvasParams.Brush.Assign(Canvas.Brush);
  FCanvasParams.Font.Assign(Canvas.Font);
  FCanvasParams.Pen.Assign(Canvas.Pen);
  FCanvasParams.Region.Assign(Canvas);
{$ELSE}
  { TODO: sava agg2d canvas style }
{$ENDIF}
  vOldConverting := FConverting;
{$IFNDEF SG_FM_UNIX}
  vRegion := TsgRegion.Create;
{$ELSE}
  vCanvasState := FContext.Canvas.SaveState;
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  vTextAlign := FContext.SetTextAlign(TA_BASELINE);
{$ENDIF}
  try
  {$IFNDEF SG_FM_UNIX}
    vRegion.Assign(FCanvasParams.Region);
  {$ENDIF}
    {$IFDEF SG_DELPHI_VCL}
    if (Canvas is TMetafileCanvas) then
      FConverting := True;
    {$ENDIF}

{$IFNDEF SG_NON_WIN_PLATFORM}
    if not FConverting then
      FConverting := GetObjectType(Canvas.Handle) in [OBJ_METADC, OBJ_ENHMETADC];
{$ENDIF}
    FContext.RealizePalette;
    //evg - to DrawRect!!!
    FPixelsPerInch := FContext.PixelsPerInch; //evg
    FColor := GetDrawPenColor;
    FColorPen := GetDrawPenColor;
    FColorBrush := GetDrawPenColor;
    FColorFont := GetDrawPenColor;
    FContext.PenColor := FColorPen;
    FContext.BrushColor := FColorBrush;
    FContext.FontColor := FColorFont;
    FPenWidth := cnstPenWidthNull;
    FContext.PenWidth := FPenWidth;
    FContext.PenStyle := psSolid;
    if not Transparent then
    begin
      if IsCurLayoutShowPlot then
        DoSetColor(cnstPlotBackground, ctBrush)
      else
        DoSetColor(GetBackgroundColor, ctBrush);
      if vSelectionMatrixMode <> smMatrixOnly then
        DoDrawBackground(FContext.ClipRect);
      if ShowPlotSetting and (not FExpProcs.Active) then
        DrawPaperSpace;
    end;
    Converter.AutoInsert := False;
    Converter.Params := @FDraw;
    FContextSelectionMatrix.BeginScene;
    try
      CheckColorToLineWeight;
      DoDrawBefore({Canvas, Rect});
      if Assigned(FExternalRegion) then
        DoCreateExternalRegion(nil);
      R := cnstRectZero;
      if not IsBadRect(FDrawingBox) then
      begin
        R := GetRect(FDrawingBox);
        InflateRect(R, 1, 1);
      end;
      if Assigned(vClippingRect) then
      begin
        vDX := (FMetafileRegionScale - 1) * Abs(Rect.Right - Rect.Left);
        vDY := (FMetafileRegionScale - 1) * Abs(Rect.Bottom - Rect.Top);
        FContext.IntersectClipRect(Floor(Rect.Left - vDX), Floor(Rect.Top - vDY), Ceil(Rect.Right + vDX), Ceil(Rect.Bottom + vDY));
      {$IFNDEF SG_FM_UNIX}
        FContext.RegionUpdate(vRegion);
      {$ENDIF}
      end
      else
        if IsRectEmpty(R) then // FDrawingBox not initialized
          R := GetPlotSettingsRect;
      if not IsRectEmpty(R) then
    {$IFDEF SG_FIREMONKEY}
        FContext.Canvas.IntersectClipRect(R);
    {$ELSE}
        vRegion.ClipRect := R;
      if vRegion.HandleAllocated then
        FContext.RegionSelect(vRegion);
    {$ENDIF}
      if FConverting then
      begin
        UpdateClientClipRect(vRect);
        if vClippingRect <> nil then
        begin
          FTmpClipRect.TopLeft := ClipPtToIntPt(vClippingRect^.TopLeft);
          FTmpClipRect.BottomRight := ClipPtToIntPt(vClippingRect^.BottomRight);
        end
        else
          FTmpClipRect := cnstBad2DRect;
      end
      else
        UpdateClientClipRect(FContext.ClipRect); // evg
      FContext.SetStretchBltMode(HALFTONE);
  // clear matrices
      vClearfFull := False;
      if (PtInRect(FClientClipRect, cnstRectZero.TopLeft) and
         PtInRect(FClientClipRect, Point(FViewportRect.Right - 1, FViewportRect.Bottom - 1))) then
      begin
        if Assigned(vSelectionMatrix) then
          vSelectionMatrix.ClearFull;
        vClearfFull := True;
      end
      else
        if Assigned(vSelectionMatrix) then
          if vSelectionMatrix.IsOverflow then
          begin
            vSelectionMatrix.ClearFull;
            vClearfFull := True;
          end
          else
            vSelectionMatrix.ClearRect(FClientClipRect);
      if (FSnapMatrix.ColsCount <> FViewportRect.Right) or
         (FSnapMatrix.RowsCount <> FViewportRect.Bottom) then
      begin
      {$IFDEF SG_FM_LINUX}
        if not FIsPrinterCanvas then        
      {$ENDIF}
        FSnapMatrix.Resize(FViewportRect.Right, FViewportRect.Bottom);
        vClearfFull := False;
      end;
      if vClearfFull then
        FSnapMatrix.Clear
      else
        FSnapMatrix.ClearRect(FClientClipRect);
      FSnapLastPoint.X := MaxDouble;     //evg
      FSnapLastPoint.Y := MaxDouble;//evg
      GetPixelSize(5, FDxyz.X, FDxyz.Y, True);
      FOnComplexEntity := nil;

      CreateRegionStack;
      DoIterateBefore;
      try
      {$IFNDEF SG_FM_UNIX}
        ExpClipRegionInternal(nil, vRegion);
      {$ENDIF}
        CurrentLayout.Iterate(Converter, DrawEntity, DoFinish);
      finally
        FRegionStack.Clear;
        DoIterateAfter;
      end;

      if Assigned(vSelectionMatrix) and (vSelectionMatrix.IsOverflow) then
      begin
        SetSelectionMatrixProc(smMatrixOnly);
        try
          vSnapMask := FObjectSnapMask;
          try
            Include(FObjectSnapMask, osDisabled); // do not draw to FSnapMatrix
            vSelectionMatrix.ClearFull;
            UpdateClientClipRect(FViewportRect);
            CurrentLayout.Iterate(Converter, DrawEntity, DoFinish);
          finally
             FObjectSnapMask := vSnapMask;
          end;
        finally
          SetSelectionMatrixProc(vSelectionMatrixMode);
        end;
      end;
   {$IFDEF SHOWTICKS}   Ticks := GetTickCount - Ticks; //evg
      ShowMessage(FloatToStr(Ticks));  {$ENDIF} //evg

      if FIsDraw3DAxes then
        Draw3DAxes;
    finally
      FContextSelectionMatrix.EndScene;
    end;
  finally
    LineWeightScale := vLineWeightScale;
    FConverting := vOldConverting;
    FOnComplexEntity := nil;
    FCurrentLayout.RotZAngle := FCurrentLayout.RotZAngle+FZRotate;
    FZRotate := 0;
    FContext.SelectPalette(OldPal, True);
{$IFNDEF USE_SG_AGG2D}
    Canvas.Brush.Assign(FCanvasParams.Brush);
    Canvas.Font.Assign(FCanvasParams.Font);
    Canvas.Pen.Assign(FCanvasParams.Pen);
    TsgRegion(FCanvasParams.Region).Select(Canvas);
{$ELSE}
    { TODO: restore agg2d canvas style }
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
    FContext.SetTextAlign(vTextAlign);
    FContext.SetGraphicsMode(vPrevMode);
{$ENDIF}
    TsgObjectList.ClearList(FXrefStack);
    DoDrawAfter;
    SetListsCapacity(False);
{$IFNDEF SG_FM_UNIX}
    vRegion.Free;
{$ELSE}
    FContext.Canvas.RestoreState(vCanvasState);
{$ENDIF}
{.$IFDEF SG_FIREMONKEY}
    FContext.Canvas := nil;
{.$ENDIF}
  end;
  finally
    Drawing := nil;
  end;
end;

{$IFDEF SG_DELPHI_VCL}
procedure TsgCADImage.DrawOnMetafile(const AMetafile: TMetafile;
  const AUseLogicMetrix: Boolean; const ADrawProc: TsgObjProcDraw = nil;
  const AMetafileRegionScale: Double = 1);
var
  R: TRect;
  vMetafileCanvas: TMetafileCanvas;
  vOutMMWidth, vOutMMHeight, vKoef: Double;
  vUseGDIPlus, vAddOnePixel: Boolean;
  vReg: HRGN;
  vDC: HDC;
  vDpi: Double;
  vDX,vDY: Double;
  vMetafileRegionScaleOld: Double;
  virtWidth, physWidth, vKoefDPI: Double;
begin
  vDC := GetDC(0);
  try

    virtWidth := GetDeviceCaps(vDC, HORZRES);
    physWidth := GetDeviceCaps(vDC, DESKTOPHORZRES);
    vKoefDPI := physWidth / virtWidth;

    vDpi := GetDeviceCaps(vDC, HORZSIZE) / cnstMMPerInch;
    vDpi := GetDeviceCaps(vDC, HORZRES) / vDpi;

    vDpi := vDpi * vKoefDPI;
  finally
    ReleaseDC(0, vDC);
  end;
  vKoef := vDpi / AMetafile.Inch;
  if AUseLogicMetrix then
  begin
    vOutMMWidth := AMetafile.Width;
    vOutMMHeight := AMetafile.Height;
    SetMaxSize(31000, vOutMMWidth, vOutMMHeight);
    R := Rect(0, 0, Round(vOutMMWidth), Round(vOutMMHeight));
  end;
  vAddOnePixel := FAddOnePixel;
  vUseGDIPlus := LibraryGDIPlusExists;
  vReg := 0;
  vMetafileRegionScaleOld := FMetafileRegionScale;
  vMetafileCanvas := TMetafileCanvas.Create(AMetafile, 0);
  try
    FMetafileRegionScale := AMetafileRegionScale;
    vDX := (AMetafileRegionScale - 1) * AMetafile.Width * vKoef;
    vDY := (AMetafileRegionScale - 1) * AMetafile.Height * vKoef;
    vReg := CreateRectRgn(-Floor(vDX), -Floor(vDY), Ceil(AMetafile.Width * vKoef + vDX), Ceil(AMetafile.Height * vKoef + vDY));
    SelectClipRgn(vMetafileCanvas.Handle, vReg);
    FAddOnePixel := False;
    if AUseLogicMetrix then
    begin
      LibraryGDIPlusExists := False;
      SetMapMode(vMetafileCanvas.Handle, MM_ANISOTROPIC or MM_MAX);
      SetViewportExtEx(vMetafileCanvas.Handle, Round(AMetafile.Width * vKoef), Round(AMetafile.Height * vKoef), nil);
      SetWindowExtEx(vMetafileCanvas.Handle, AMetafile.Width, AMetafile.Height, nil);
      SetWindowOrgEx(vMetafileCanvas.Handle, 0, 0, nil);
      SetWindowExtEx(vMetafileCanvas.Handle, R.Right, R.Bottom, nil);
    end
    else
      R := Rect(0, 0, Round(AMetafile.Width * vKoef), Round(AMetafile.Height * vKoef));
    vMetafileCanvas.StretchDraw(R, Self);
    if Assigned(ADrawProc) then
      ADrawProc(vMetafileCanvas, R);
  finally
    FMetafileRegionScale := vMetafileRegionScaleOld;
    DeleteObject(vReg);
    FAddOnePixel := vAddOnePixel;
    LibraryGDIPlusExists := vUseGDIPlus;
    vMetafileCanvas.Free;
  end;
end;
{$ENDIF}

function TsgCADImage.DrawComplexEntity(Sender: TObject): Integer;
begin
  Result := Integer(FOnComplexEntity = nil);
  //Exit;
  if Result = 1 then
  begin
    if (SelectionMatrix <> nil) and (not FExpProcs.Active) then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
    end;
    FOnComplexEntity := TsgDXFEntity(Sender);
  end;
end;


//evg reg
function sgRectVisible(ABaseRect, AEntityRect: TRect): Boolean;
begin
  Result := False;
  if AEntityRect.Right < ABaseRect.Left then
    Exit;
  if AEntityRect.Left > ABaseRect.Right then
   Exit;
  if AEntityRect.Bottom < ABaseRect.Top then
    Exit;
  if AEntityRect.Top > ABaseRect.Bottom then
   Exit;
  Result := True;
end;

function TsgCADImage.DrawEntity(Entity: TsgDXFEntity): Integer;
var
  vRectVisible, vAttdef, vIsNoClipping: Boolean;
  vIntersectRect, vTmpIRect: TRect;
  vMatrixMode: TsgSelectionMode;
  vLayerLines{$IFDEF SG_THREAD_DRAW}, vEntityLines{$ENDIF}: TsgLines;
  vColor: TColor;
  vRegionStack: TsgObjectList;
  Temp : Boolean;
  BlockNameStr: String; // 최승선 수정
  IsBlockEntity: Boolean; // 최승선 수정

  function IsBadGDIRect(const R: TRect): Boolean;
  const
    cnstGDIMaxBound: UInt64 =        $0438C3FFBE400001; // empirical
    cnstGDIMaxPolygonBound: UInt64 = $0000800000000000; // empirical
  var
    vDx, vDy: Int64;
    vLen: UInt64;
  begin
    vDx := R.Right - R.Left;
    vDy := R.Bottom - R.Top;
    vLen := vDx * vDx + vDy * vDy;
    case Entity.EntType of
      ceCurvePolygon, ceGradient, ceGradientPolygon, ceHatch:
        Result := vLen > cnstGDIMaxPolygonBound; // do not create huge region objects
    else
      Result := vLen > cnstGDIMaxBound;
    end;
  end;

  function IsEntityInRectVisible(ARect: TRect): Boolean;
  begin
    if vMatrixMode = smMatrixOnly then
      Result := (SelectionMatrix <> nil) and sgRectVisible(FClientClipRect, ARect) and not IsBadGDIRect(ARect)
    else
    begin
      Result := sgRectVisible(FClientClipRect, ARect) and not IsBadGDIRect(ARect);
      if Result then
        Result := IsRectInRegion(ARect);
    end;
  end;

  function CheckDraw(AEntity: TsgDXFEntity): Boolean;
  begin
//    Result := EntVisible(AEntity);
    case AEntity.EntType of
      ceRay, ceXline:
        begin
          TsgDXFRayAccess(AEntity).UpdatePoints(TsgDXFRayAccess(AEntity).Direction, FClientClipRect, FDraw.Matrix);
          Result := True;
        end;
      ceAttdef, ceAttrib:
       Result := not (Converter.HeadVarStruct.AttMode = attDisable);
      ce3dFace:
       Result :=  not TsgDXF3dFaceAccess(AEntity).IsHidden;
    else
      Result := True;
    end;
  end;

begin
  Result := 0;
  FEntityHandle := Entity.Handle;
  IsBlockEntity := Entity is TsgDXFInsert; // 최승선 수정
{$IFDEF SG_THREAD_DRAW}
  vEntityLines := EntLines(Entity, FDraw, vLayerLines);
  FEntityLines.Assign(vEntityLines);
{$ELSE}
  FEntityLines := EntLines(Entity, FDraw, vLayerLines);
{$ENDIF}
  if FExpProcs.Active then
    FExpProcs.DoProgress;

  vRegionStack := FRegionStack;
  if vRegionStack = nil then
    FRegionStack := TsgRegionStack.Create;

  // 최승선 수정
  if Entity is TsgDXFInsert then
  begin
    if Assigned(TsgDXFInsert(Entity).BlockRecord) then
      BlockNameStr := TsgDXFInsert(Entity).BlockRecord.Name
    else
      BlockNameStr := 'UnknownBlock';
    if Assigned(FExpProcs) then
      FExpProcs.DoExportBlockAW('<G ID="' + BlockNameStr + '">');
  end;

  if FExpProcs.Active then
    FExpProcs.DoEntityProcBegin(Entity);
  vMatrixMode := GetMatrixMode;
  try
    if Entity.EntType = ceViewport then
    begin
      TsgDXFViewPort(Entity).VisibleBoundary := EntVisibleByLayer(Entity, FDraw.Insert);
      FTmpEntColor := GetEntityColors(Entity, FDraw.Insert);
      FColor := FTmpEntColor.DrawColor;
      DoSetColor(FColor, ctPen, 3);
      ApplyPen(Entity, @FTmpEntColor.EntColorCAD);//SetPenWidth(1);
      DrawEntityOnSnapMatrix(Entity);
      Result := DoDraw(Entity);// allways call draw function for TsgDXFVewport
    end
    else
    begin
      vAttdef := Entity.EntType = ceAttdef;
      if not CheckDraw(Entity) then
        Exit;
      FReady := False;
      begin
        if not EntVisibleByLayer(Entity, FDraw.Insert) then
          Exit;
        FContext.Rect := cnstBad2DRectExport;
        FContext.IndexPoints := 0;
        FReady := Entity.GetBoxPoints(BoxPoint, FDraw.Additional <> 0);
        if FReady and Assigned(vLayerLines) and (not vLayerLines.IsSolid) then
          FReady := False;
        vTmpIRect := FContext.Rect;
        Inc(vTmpIRect.Right);
        Inc(vTmpIRect.Bottom);
        vRectVisible := IsEntityInRectVisible(vTmpIRect);
        if IsBadRectI(FTmpClipRect) then
          vIsNoClipping := True
        else
          if FConverting then
            vIsNoClipping := Assigned(FClippingRect) and sgRectVisible(FTmpClipRect, vTmpIRect)
          else
            vIsNoClipping := False;
        if not vRectVisible and not vIsNoClipping then
          Exit;
//        if not EntVisibleByLayer(Entity, FDraw.Insert) then
//          Exit;
        Temp := True;
        if not FExpProcs.Active then
           Temp := (FContext.PenMode <> pmXor) and (FContext.PenMode <> pmNotXor);
        if ((FContext.Rect.Right - FContext.Rect.Left)< 1.5) and ((FContext.Rect.Bottom - FContext.Rect.Top)< 1.5) and
              Entity.IsOnePixelOptimisationValid and Temp then
        begin
          FTmpEntColor := GetEntityColors(Entity, FDraw.Insert);
          FColor := FTmpEntColor.DrawColor;
          if (FColor = clNone) or (DrawMode = dmBlack) then
            FColor := GetDrawPenColor;
          if ((FPixel.X = FContext.Rect.Left) and (FPixel.Y = FContext.Rect.Top)) and
            (FPixelColor = FColor) then
            Exit;
          FPixelColor := FColor;
          ExpPixelInteger(Entity, Point(FContext.Rect.Left, FContext.Rect.Top), FPixelColor);
          FPixel := Point(FContext.Rect.Left, FContext.Rect.Top);
          if Entity.EntType = cePoint then
            DrawEntityOnSnapMatrix(Entity);
          // SetPixelEx - evg!!! it is needed to add E.DefaultPoint: TFPOint property here
          Exit;
        end;
        DrawEntityOnSnapMatrix(Entity);
       // ApplyPen(Entity); //evg ??? - is any sence to show lineweight for less than 1 pixel elements?
        {evg}
        if not (osDisabled in FObjectSnapMask) and (FObjectSnapMask <> []) and
           Entity.IsInsert then
        begin
          IntersectRect(vIntersectRect, sgClientRectForSnap, vTmpIRect);
          if (not IsRectEmpty(vIntersectRect)) and  vRectVisible then //evg - the same code twice
          begin
            if FStoreInsert = nil then
            begin
              FStoreSnapOnlyIterate := FSnapOnlyIterate;
              FSnapOnlyIterate := True;
              FStoreInsert := FDraw.Insert;
            end;
            DoDraw(Entity);
            if FStoreInsert = FDraw.Insert then
            begin
              FSnapOnlyIterate := FStoreSnapOnlyIterate;
              FStoreInsert := nil;
            end;
          end;
        end;
      end;
      if vAttdef or vRectVisible or FConverting then// evg - ????? ??? metafilecanvas
      begin
        if SetColorByExternalPen(1, vColor, nil) then
        begin
          FColor := vColor;
          DoSetColor(vColor, ctPen, 3);
          SetPenWidth(FDrawExternlProps.Mode.LineWidth);
        end
        else
        begin
          FTmpEntColor := GetEntityColors(Entity, FDraw.Insert);
          FColor := FTmpEntColor.DrawColor;
          DoSetColor(FColor, ctPen, 3);
          if Assigned(FDrawExternlProps) and Assigned(FDraw.Insert) then
            SetPenWidth(FDrawExternlProps.Mode.LineWidth)
          else
            ApplyPen(Entity, @FTmpEntColor.EntColorCAD);
        end;
        Result := DoDraw(Entity);
      end;
    end;
  finally
    SetMatrixMode(vMatrixMode);
    if vRegionStack <> FRegionStack then
      FreeAndNil(FRegionStack);
    if FExpProcs.Active then
      FExpProcs.DoEntityProcEnd(Entity);
    // 최승선 수정
    if Entity is TsgDXFInsert then
    begin
      if Assigned(FExpProcs) then
        FExpProcs.DoExportBlockAW('</G>');
    end;
  end;
end;

type
  TPointsPolylineVertices = class(TInterfacedObject, IsgArrayFPoint)
  private
    FEntList: TsgEntitiesList;
  protected
    function GetFPoint(const AIndex: Integer): TFPoint;
    function GetFPointCount: Integer;
  public
    constructor Create(const AEntList: TsgEntitiesList);
  end;

  TPointsTransformed = class(TInterfacedObject, IsgArrayFPoint)
  private
    FOwner: IsgArrayFPoint;
    FTransformation: TFMatrix;
  protected
    function GetFPoint(const AIndex: Integer): TFPoint; virtual;
    function GetFPointCount: Integer;
  public
    constructor Create(const AOwner: IsgArrayFPoint; const ATransformation: TFMatrix);
  end;

  TPointsMoved = class(TPointsTransformed)
  protected
    function GetFPoint(const AIndex: Integer): TFPoint; override;
  end;

  TPointsAffineTransformed = class(TPointsTransformed)
  protected
    function GetFPoint(const AIndex: Integer): TFPoint; override;
  end;

{ TPointsPolylineVertices }

function TPointsPolylineVertices.GetFPoint(const AIndex: Integer): TFPoint;
begin
  Result := TsgDXFVertex(FEntList[AIndex]).Point;
end;

function TPointsPolylineVertices.GetFPointCount: Integer;
begin
  Result := FEntList.Count;
end;

constructor TPointsPolylineVertices.Create(const AEntList: TsgEntitiesList);
begin
  inherited Create;
  FEntList := AEntList;
end;

{ TPointsMoved }

function TPointsMoved.GetFPoint(const AIndex: Integer): TFPoint;
begin
  Result := AddFPoint(FOwner.FPoints[AIndex], FTransformation.E0);
end;

{ TPointsTransformed }

function TPointsTransformed.GetFPoint(const AIndex: Integer): TFPoint;
begin
  Result := FPointXMat(FOwner.FPoints[AIndex], FTransformation);
end;

function TPointsTransformed.GetFPointCount: Integer;
begin
  Result := FOwner.FPointCount;
end;

constructor TPointsTransformed.Create(const AOwner: IsgArrayFPoint; const ATransformation: TFMatrix);
begin
  inherited Create;
  FOwner := AOwner;
  FTransformation := ATransformation;
end;

{ TPointsAffineTransformed }

function TPointsAffineTransformed.GetFPoint(const AIndex: Integer): TFPoint;
begin
  Result := AffineTransformPoint(FOwner.FPoints[AIndex], FTransformation);
end;

function TsgCADImage.DrawSnapPoints(const APoints: TFPointList;
  const AMode: TObjectSnapState; const ASnapEntitry: TsgDXFEntity = nil): Integer;
var
  I: Integer;
begin
  Result := 0;
  if not IsDisableSnapMatrix then
  begin
    if Assigned(ASnapEntitry) then
      FSnapEntity := ASnapEntitry
    else
    begin
      if not Assigned(FSnapEntityDefault) then
        FSnapEntityDefault := TsgDXFEntity.Create;
      FSnapEntity := FSnapEntityDefault;
    end;
    for I := 0 to APoints.Count - 1 do
      SetPixelSnap(APoints[I], AMode);
    Result := 1;
  end;
end;

//evg new
procedure TsgCADImage.DrawEntityOnSnapMatrix(Entity: TsgDXFEntity);
var
  vPt: TFPoint;
  vPoly: TsgCADBasePolyline absolute Entity;
  vEllipse: TsgDXFEllipse;
  vRect: TFRect;

  procedure PolyPoints(const APoints: TFPointList);
  var
    I: Integer;
  begin
    for I := 0 to APoints.Count - 1 do
      SetPixelSnap(APoints[I], [osEndPt]);
  end;

  procedure PolylinePoints(APolyLine: TsgCADBasePolyline);
  begin
    if APolyLine.PolyPoints.Count > 0 then
      PolyPoints(APolyLine.PolyPoints);
  end;

  procedure FlatEntityPoints(APoly: TsgFlatEntity);
  var
    I: Integer;
  begin
    if APoly.PCount > 0 then
    begin
      for I := 0 to APoly.PCount - 1 do
        SetPixelSnap(APoly.XY[I], [osEndPt]);
    end;
  end;

  procedure DoQuardrantSnap(const ACircle: TsgDXFCircle);
  var
    vAngle, I: Integer;
    vA, vB, vStart, vEnd, vSin, vCos: Double;
    vPoint: TFPoint;
  begin
    if ([osQuadrant] * FObjectSnapMask = []) or
       TsgDXFCircleAccess(ACircle).IsExtruded then Exit;
    TsgDXFCircleAccess(ACircle).Params(vA, vB, vStart, vEnd, vSin, vCos);
    CheckAngles(vStart, vEnd);
    for I := 0 to 3 do
    begin
      vAngle := 90 * I;
      if IsAngleInAngles(vAngle, vStart, vEnd) then
      begin
        if ACircle.EntType = ceEllipse then
          vPoint := GetPointOnEllipse(ACircle.Point, vA, vB, vSin, vCos, vAngle)
        else//this function has a more precise calculations
          vPoint := GetPointOnCircle(ACircle.Point, ACircle.Radius, vAngle);
        SetPixelSnap(vPoint, [osQuadrant]);
      end;
    end;
  end;

  procedure DoCircle(const ACircle: TsgDXFCircle);
  begin
    SetPixelSnap(ACircle.Point, [osCenter]);
    DoQuardrantSnap(ACircle);
  end;

  procedure DoArc(const AArc: TsgDXFArc);
  begin
    DoCircle(AArc);
    SetPixelSnap(AArc.StartPoint, [osEndPt]);
    SetPixelSnap(AArc.EndPoint, [osEndPt]);
  end;

  procedure Vertexes(APolyLine: TsgDXFPolyline);
  var
    I: Integer;
    vArc: TsgDXFArc;
    vBulges: TsgDoubleList;
    vPoly, vPoly1, vPoly2: IsgArrayFPoint;
    vExtrusion: TFMatrix;
    vTransformFlags: Integer;

    procedure DoPolySnap(const AArc: TsgDXFArc; const ABulges: TsgDoubleList;
      APoints: IsgArrayFPoint);
    var
      J, C: Integer;
      vPoint1, vPoint2: TFPoint;
    begin
      C := APoints.FPointCount - 1;
      if C >= 0 then
      begin
        vPoint1 := APoints.FPoints[0];
        for J := 0 to C do
        begin
          SetPixelSnap(vPoint1, [osEndPt]);
          if J >= C then
            vPoint2 := APoints.FPoints[0]
          else
            vPoint2 := APoints.FPoints[J + 1];
          if Assigned(AArc) and (ABulges[J] <> 0) then
          begin
            if not IsEqualFPoints(vPoint1, vPoint2) then
            begin
              SetArcParams(AArc, vPoint1, vPoint2, ABulges[J]);
              DoCircle(AArc);
            end;
          end;
          vPoint1 := vPoint2;
        end;
      end;
    end;

  begin
    if APolyLine.PolyPoints.Count > 0 then
    begin
      vArc := nil;
      vBulges := nil;
      try
        if (APolyLine.EntType = ceLWPolyline) and
           ([osCenter, osQuadrant] * FObjectSnapMask <> []) and
           (not TsgDXFPolylineAccess(APolyLine).IsExtruded) then  //??
        begin
          vArc := TsgDXFArc.Create;
          vBulges := TsgDoubleList.Create(APolyLine.Count);
          for I := 0 to APolyLine.Count - 1 do
            vBulges.List^[I] := APolyLine.Vertexes[I].Bulge;
        end;
        vPoly := TPointsPolylineVertices.Create(TsgDXFPolylineAccess(APolyLine).List);
        vTransformFlags := Ord(TsgDXFPolylineAccess(APolyLine).GetElevation <> 0) or
          Ord(TsgDXFPolylineAccess(APolyLine).IsExtruded) shl 1 or
          Ord(TsgDXFPolylineAccess(APolyLine).GetThickness <> 0) shl 2;

        vExtrusion := cnstIdentityMat;
        case vTransformFlags and $3 of
          0: vPoly1 := vPoly;
          1:
            begin
              vExtrusion.E0 := MakeFPoint(0, 0, APolyLine.Elevation);
              vPoly1 := TPointsMoved.Create(vPoly, vExtrusion);
            end;
          2:
            begin
              vExtrusion := ExtrusionToMatrix(APolyLine.Extrusion);
              vPoly1 := TPointsAffineTransformed.Create(vPoly, vExtrusion);
            end;
          3:
            begin
              vExtrusion := ExtrusionToMatrix(APolyLine.Extrusion);
              vExtrusion.E0 := AffineTransformPoint(MakeFPoint(0, 0, APolyLine.Elevation), vExtrusion);
              vPoly1 := TPointsTransformed.Create(vPoly, vExtrusion);
            end;
        end;
        DoPolySnap(vArc, vBulges, vPoly1);
        if vTransformFlags and $4 <> 0 then
        begin
          case vTransformFlags and $3 of
            0:
              begin
                vExtrusion.E0 := MakeFPoint(0, 0, APolyLine.ZThick);
                vPoly2 := TPointsMoved.Create(vPoly, vExtrusion);
              end;
            1:
              begin
                vExtrusion.E0 := AddFPoint(vExtrusion.E0, MakeFPoint(0, 0, APolyLine.ZThick));
                vPoly2 := TPointsMoved.Create(vPoly, vExtrusion);
              end;
            2:
              begin
                vExtrusion.E0 := AffineTransformPoint(MakeFPoint(0, 0, APolyLine.ZThick), vExtrusion);
                vPoly2 := TPointsAffineTransformed.Create(vPoly, vExtrusion);
              end;
            3:
              begin
                vExtrusion.E0 := AddFPoint(vExtrusion.E0, AffineTransformPoint(MakeFPoint(0, 0, APolyLine.ZThick), vExtrusion));
                vPoly2 := TPointsTransformed.Create(vPoly, vExtrusion);
              end;
          end;
          DoPolySnap(vArc, vBulges, vPoly2);
        end;
      finally
        vBulges.Free;
        FreeAndNil(vArc);
      end;
    end;
  end;

{$IFDEF SG_BTI}
  procedure PolyPolyline2DPoints(APoly: TsgCADPolyPolyline2D);
  var
    I: Integer;
    vLine: TsgLine;
    vArcR: TsgArcR;
    vArc: TsgDXFArc;
  begin
    if APoly.CurvesCount > 0 then
    begin
      vArc := TsgDXFArc.Create;;
      try
        for I := APoly.CurvesCount - 1 downto 0 do
        begin
          if APoly.CurveVisible[I] then
            if APoly.IsCurveArc(I) and
              ([osEndPt, osCenter, osQuadrant] * FObjectSnapMask <> []) then
            begin
              vArcR := APoly.Arcs[I];
              vArc.Point := vArcR.Center;
              vArc.Radius := vArcR.Radius;
              vArc.StartAngle := vArcR.AngleS;
              vArc.EndAngle := vArcR.AngleE;
              DoArc(vArc);
            end
            else
            begin
              vLine := APoly.Lines[I];
              SetPixelSnap(vLine.Point1, [osEndPt]);
              SetPixelSnap(vLine.Point2, [osEndPt]);
            end;
        end;
      finally
        FreeAndNil(vArc);
      end;
    end;
  end;

  procedure InsertPts(const AInsert: TsgDXFInsertAccess; const AMode: Integer);
  var
    I: Integer;
    vPts: TsgPoints4;
    vPoints: TFPointList;
  begin
    if AMode > 1 then
    begin
      vPoints := TFPointList.Create;
      try
        AInsert.GetPtsEx(vPoints);
        for I := 0 to MinI(4, vPoints.Count - 1) do
          SetPixelSnap(vPoints[I], [osEndPt]);
        for I := 5 to MinI(8, vPoints.Count - 1) do
          SetPixelSnap(vPoints[I], [osMiddle]);
      finally
        vPoints.Free;
      end;
    end
    else
    begin
      AInsert.GetPts(vPts);
      for I := Low(vPts) to High(vPts) do
        SetPixelSnap(vPts[I], [osEndPt]);
      if AMode > 0 then
      begin
        SetPixelSnap(MiddleFPoint(vPts[0], vPts[3]), [osMiddle]);
        SetPixelSnap(MiddleFPoint(vPts[1], vPts[2]), [osMiddle]);
      end;
    end;
  end;
{$ENDIF}

begin
  if IsDisableSnapMatrix then
    Exit;
  FSnapEntity := Entity;
  case Entity.EntType of
    cePoint:
      begin
        SetPixelSnap(TsgDXFPoint(Entity).Point, [osEndPt]);
      end;
    ceLine, ceSolid, ceTrace, ce3dFace:
      begin
        SetPixelSnapEx(TsgDXFSolid(Entity).Point, FContext.ReadyPoints.Points[0], [osEndPt]);
        SetPixelSnapEx(TsgDXFSolid(Entity).Point1, FContext.ReadyPoints.Points[1], [osEndPt]);
        case Entity.EntType of
          ceSolid, ceTrace, ce3dFace:
            begin
              SetPixelSnapEx(TsgDXFSolid(Entity).Point2, FContext.ReadyPoints.Points[2], [osEndPt]);
              SetPixelSnapEx(TsgDXFSolid(Entity).Point3, FContext.ReadyPoints.Points[3], [osEndPt]);
            end;
        end;
      end;
    ceRay, ceXline:
      begin
        SetPixelSnapEx(TsgDXFRay(Entity).StartPoint, FContext.ReadyPoints.Points[0], [osEndPt]);
      end;
    ceCircle:
      begin
        DoCircle(TsgDXFCircle(Entity));
      end;
    ceArc:
      begin
        DoArc(TsgDXFArc(Entity));
      end;
    ceEllipse:
      begin
        vEllipse := TsgDXFEllipse(Entity);
        vPt := vEllipse.Point;
        DoExtrusion(vPt, vEllipse.Extrusion);
        SetPixelSnap(vPt, [osCenter]);
        if not(Abs(vEllipse.StartAngle - vEllipse.EndAngle + 360) < FAccuracy) and
          (vEllipse.PolyPoints <> nil) then
        begin
          vPt := vEllipse.Points[0];
          SetPixelSnap(vPt, [osEndPt]);
          vPt := vEllipse.Points[vEllipse.PolyPoints.Count - 1];
          SetPixelSnap(vPt, [osEndPt]);
        end;
        DoQuardrantSnap(TsgDXFCircle(Entity));
      end;
    ceLWPolyline,
      cePolyline:
        Vertexes(TsgDXFPolyline(Entity));
      cePath,
      ceLeader:
        PolylinePoints(TsgCADBasePolyLine(Entity));
    ceWipeout,
    ceImageEnt:
        begin
          if TsgCADBasePolyLine(Entity).PolyPoints.Count > 0 then
            PolylinePoints(TsgCADBasePolyLine(Entity))
          else
          begin
            SetPixelSnap(TsgCADWipeoutAccess(Entity).Point, [osEndPt]);
            SetPixelSnap(TsgCADWipeoutAccess(Entity).Point1, [osEndPt]);
            SetPixelSnap(TsgCADWipeoutAccess(Entity).Point2, [osEndPt]);
            SetPixelSnap(TsgCADWipeoutAccess(Entity).Point3, [osEndPt]);
          end;
        end;
    ceSpline, ceHelix:
      begin
        SetPixelSnap(vPoly.Points[0], [osEndPt]);
        SetPixelSnap(vPoly.Points[vPoly.PolyPoints.Count - 1], [osEndPt]);
        if TsgDXFSpline(vPoly).FitCount > 0 then
          PolyPoints(TsgDXFSpline(vPoly).Fit);
      end;
    ceDimension:
      begin
        if osInsert in FObjectSnapMask then
        begin
          if FDraw.Insert = nil then
            SetPixelSnap(TsgDXFDimension(FSnapEntity).DefPoint, [osInsert])
        end;
      end;
    ceACADTable,
      ceInsert,
      ceMText,
      ceMLine,
      ceTolerance:
        begin
          if osInsert in FObjectSnapMask then
          begin
            if FDraw.Insert = nil then
            begin
{$IFDEF SG_BTI}
              if TsgDXFEntityAccess(FSnapEntity).GetEntTypeEx <= cnstUndefined then
{$ENDIF}
                SetPixelSnap(TsgDXFInsert(FSnapEntity).Point, [osInsert]);
            end;
          end;
{$IFDEF SG_BTI}
          if [osEndPt, osMiddle] * FObjectSnapMask <> [] then
          begin
            case TsgDXFEntityAccess(FSnapEntity).GetEntTypeEx of
              cnstElementModifier, cnstElementBroad, cnstElementCarved:
                InsertPts(TsgDXFInsertAccess(FSnapEntity), 2);
              cnstComplexBroad,  cnstComplexLinear:
                InsertPts(TsgDXFInsertAccess(FSnapEntity), 1);
            end;
          end;
{$ENDIF}
        end;

    ceText,
      ceAttdef,
      ceAttrib:
        begin
          if (osInsert in FObjectSnapMask) and (FDraw.Insert = nil) then
            SetPixelSnap(TsgDXFText(FSnapEntity).Point, [osInsert]);
        end;
{  //evg: now there is no snap here
    ceShape:
      DrawShape(Entity);
    ce3dFace:
      DrawFace(Entity);
    cePolyPolygon,
      ceGradient,
      ceGradientPolygon,
      ceCurvePolygon,
      ceHatch:           DrawHatch(Entity);
    ceImageEnt:          DrawImageEnt(Entity);
    ceViewport:          DrawViewPort(Entity);
    ceRegion, ceSurface, ce3dSolid, ceBody,
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor:
                         DrawACISEntity(Entity);
    ceOle2Frame:         DrawOle2Frame(Entity);  }
  //evg - removed to drawflatpoly      ceFlatPoly:          DrawFlatPoly(Entity);
    ceFlatHatch,
     ceFlatPoly3D:         FlatEntityPoints(TsgFlatEntity(Entity));
  //  ceXRef:              DrawXRef(Entity);
 //   ceWipeout:           DrawWipeout(Entity);
{$IFDEF SG_BTI}
    cePolyPolyline2D:   PolyPolyline2DPoints(TsgCADPolyPolyline2D(Entity));
{$ENDIF}
    ceOle2Frame:
      begin
{$IFNDEF SG_NON_WIN_PLATFORM}
        SetPixelSnap(TsgDXFOle2Frame(Entity).Point, [osEndPt]);
        SetPixelSnap(TsgDXFOle2Frame(Entity).Point1, [osEndPt]);
{$ENDIF}
      end;
    ceViewPort:
      begin
        if TsgDXFViewportAccess(Entity).GetClipBoundary(FConverter) = nil then
        begin
          vRect :=  TsgDXFViewPort(Entity).Rect;
          SetPixelSnap(vRect.TopLeft, [osEndPt]);
          SetPixelSnap(vRect.BottomRight, [osEndPt]);
          SetPixelSnap(MakeFPoint(vRect.Right, vRect.Top), [osEndPt]);
          SetPixelSnap(MakeFPoint(vRect.Left, vRect.Bottom), [osEndPt]);
        end;
      end;
  end;
end;

procedure TsgCADImage.DrawRect(DC: HDC; SourceRect: TFRect; DestRect: TRect);
var
  vCanvas: TCanvas;
  vBox, vExtents, vClip: TFRect;
begin
  if SourceRect.Left > SourceRect.Right then
    SwapSGFloats(SourceRect.Left, SourceRect.Right);
  if SourceRect.Bottom > SourceRect.Top then
    SwapSGFloats(SourceRect.Top, SourceRect.Bottom);
  if SourceRect.Z1 > SourceRect.Z2 then
    SwapSGFloats(SourceRect.Z1, SourceRect.Z2);
{$IFDEF SG_FIREMONKEY}
  HDC(vCanvas) := DC;
{$ELSE}
  vCanvas := TCanvas.Create;
{$ENDIF}
  try
{$IFNDEF SG_FIREMONKEY}
    vCanvas.Handle := DC;
{$ENDIF}
    vBox := FDrawingBox;
    try
      vExtents := GetDrawingExtents;
      vClip.Left := - vExtents.Left + SourceRect.Left;
      vClip.Top := vExtents.Top - SourceRect.Top;
      vClip.Right := - vExtents.Left + SourceRect.Right;
      vClip.Bottom := vExtents.Top - SourceRect.Bottom;
      SetClippingRect(@vClip);
      Draw(vCanvas, DestRect);
    finally
      FDrawingBox := vBox;
      SetClippingRect(nil);
    end;
  finally
{$IFDEF SG_FIREMONKEY}
  HDC(vCanvas) := 0;
{$ELSE}
    vCanvas.Free;
{$ENDIF}
  end;
end;

function TsgCADImage.GetEntityColor(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TColor;
var
  vColors: TsgEntColors;
begin
  vColors := GetEntityColors(AEntity, AInsert);
  Result := vColors.DrawColor;
end;

function TsgCADImage.GetEntityColors(AEntity: TsgDXFEntity;
  AInsert: TsgDXFInsert): TsgEntColors;
var
  vColorCAD: TsgColorCAD;
begin
  Result.EntColorCAD := cnstColorCADNone;
  Result.EntColor := GetEntColorCAD(AEntity, AInsert, @Result.EntColorCAD);
  if (Result.EntColor = clByLayer) or (Result.EntColor = clByBlock) then
    Result.DrawColor := GetDrawPenColor
  else
    Result.DrawColor := Result.EntColor;
  case FDrawMode of
    dmBlack:
      begin
       if Result.DrawColor <> clWhite then
         Result.DrawColor := GetDrawPenColor;
      end;
    dmGray:
      begin
        if FDrawMode = dmGray then
          Result.DrawColor := ConvertColortoGray(Result.EntColor);
      end;
  end;
  if FPrinting = pntNone then
  begin
    if Result.DrawColor = GetBackgroundColor then
    begin
      vColorCAD := Result.EntColorCAD;
      if (vColorCAD.Active = acIndexColor) and
        (vColorCAD.Color > 249) and (vColorCAD.Color < 256)  then
        Result.DrawColor := FChangeBackgroundColor;
    end;
  end;
  ApplyAlternateWhite(Result.DrawColor);
end;

function TsgCADImage.GetEntLineWeight(const AEntity, AInsert: TsgDXFEntity): Double;
begin
  Result := EntLineWeight(AEntity, TsgDXFInsert(AInsert));
  if TsgDXFEntityAccess(AEntity).HasLineWeight then
  begin
    ApplyLineWeightFactor(Result);
    ApplyMMToPixelX(Result);
    if (Result < 0) or sgIsZero(Result) then
      Result := FNullWidth;
  end;
{$IFDEF USE_SG_AGG2D}
  if Result = 0 then
    Result := FmmToPixelX;
{$ENDIF}
end;

function TsgCADImage.GetEmpty: Boolean;
begin
  Result := False;//(FConverter.Counts[csEntities] = 0);
end;

{$IFDEF SGFPC}
function TsgCADImage.GetTransparent: Boolean;
begin
  Result := not FIsShowBackground;
end;
{$ENDIF}

function TsgCADImage.GetExpAnnotation: TsgExpAnnotation;
begin
  Result := FExpProcs.ExpAnnotation;
end;

function TsgCADImage.GetExpArc: TsgExpArc;
begin
  Result := FExpProcs.ExpArc;
end;

function TsgCADImage.GetExpClipRgn: TsgExpClipRgn;
begin
  Result := FExpProcs.ExpClipRgn;
end;

function TsgCADImage.GetExpCloseFigure: TsgExpCloseFigure;
begin
  Result := FExpProcs.ExpCloseFigure;
end;

function TsgCADImage.GetExpFillRgn: TsgExpFillRgn;
begin
  Result := FExpProcs.ExpFillRgn;
end;

function TsgCADImage.GetExpImage: TsgExpImage;
begin
  Result := FExpProcs.ExpImage;
end;

function TsgCADImage.GetExpImageUV: TsgExpImageUV;
begin
  Result := FExpProcs.ExpImageUV;
end;

function TsgCADImage.GetExpPixel: TsgExpPixel;
begin
  Result := FExpProcs.ExpPixel;
end;

function TsgCADImage.GetExpPolygon: TsgExpPolygon;
begin
  Result := FExpProcs.ExpPolygon;
end;

function TsgCADImage.GetExpPolyline: TsgExpPolyline;
begin
  Result := FExpProcs.ExpPolyline;
end;

function TsgCADImage.GetExpPolyPolygon: TsgExpPolyPolygon;
begin
  Result := FExpProcs.ExpPolyPolygon;
end;

function TsgCADImage.GetExpPolyPolyline: TsgExpPolyPolyline;
begin
  Result := FExpProcs.ExpPolyPolyline;
end;

function TsgCADImage.GetExpProgress: TsgExpProgress;
begin
  Result := FExpProcs.ExpProgress;
end;

function TsgCADImage.GetExpRestoreDC: TsgExpRestoreDC;
begin
  Result := FExpProcs.ExpRestoreDC;
end;

function TsgCADImage.GetExpSaveDC: TsgExpSaveDC;
begin
  Result := FExpProcs.ExpSaveDC;
end;

function TsgCADImage.GetExpSetColor: TsgExpSetColor;
begin
  Result := FExpProcs.ExpSetColor;
end;

function TsgCADImage.GetExpSetFont: TsgExpSetFont;
begin
  Result := FExpProcs.ExpSetFont;
end;

function TsgCADImage.GetExpSetPenGeometric: TsgExpSetPenGeometric;
begin
  Result := FExpProcs.ExpSetPenGeometric;
end;

function TsgCADImage.GetExpSetPenMiterLimit: TsgExpSetPenMiterLimit;
begin
  Result := FExpProcs.ExpSetPenMiterLimit;
end;

function TsgCADImage.GetExpSetPenWidth: TsgExpSetPenWidth;
begin
  Result := FExpProcs.ExpSetPenWidth;
end;

function TsgCADImage.GetExpSetStyle: TsgExpSetStyle;
begin
  Result := FExpProcs.ExpSetStyle;
end;

function TsgCADImage.GetExpText: TsgExpText;
begin
  Result := FExpProcs.ExpText;
end;

function TsgCADImage.GetExpTextOut: TsgExpTextOut;
begin
  Result := FExpProcs.ExpTextOut;
end;

procedure TsgCADImage.GetExtents;
begin
  FRegenDelta := 0;
  FConverter.DoExtents;
  if CurrentLayout = nil then
    SetCurrentLayout(FConverter.Layouts[0])
  else
  begin
    try
      FLockLayoutChangeEvent := True;
      SetCurrentLayout(CurrentLayout);
    finally
      FLockLayoutChangeEvent := False;
    end;
  end;
end;

function TsgCADImage.GetExternalFiles(const AList: TStringList;
  const AMode: TsgExternalFilesModes = [efAddXRef, efAddImageDef
  {$IFDEF DATA_LINK}, efDataLink{$ENDIF}]): Boolean;
var
  I: Integer;
  vStackConverters: TsgList;
  vList: TsgStringList;

  procedure GetXRefListBlock(const AConverter: TsgDXFConverter;
    const AList: TStringList; const AStackConverters: TsgList;
    const AMode: TsgExternalFilesModes);
  var
    I: Integer;
    vBlock: TsgDXFBlock;
    vImageDef: TsgDXFImageDef;
{$IFDEF DATA_LINK}
    J, vIndex: Integer;
    vFileName: string;
    vDataLink: TsgDXFEntity;
{$ENDIF}

  begin
    if AStackConverters.IndexOf(AConverter) < 0 then
    begin
      AStackConverters.Add(AConverter);
      try
        for I := 0 to AConverter.Counts[csBlocks] - 1 do
        begin
          vBlock := TsgDXFBlock(AConverter.Sections[csBlocks][I]);
          if Assigned(vBlock.Xref) and (efAddXRef in AMode) then
          begin
            if (efConvertXrefInFullPath in AMode) then
            begin
              if TsgCADImage(vBlock.Xref.CADImage).FileName <> '' then
                AList.Add(TsgCADImage(vBlock.Xref.CADImage).FileName);
            end
            else
              if vBlock.Xref.Path = '' then
                AList.Add(vBlock.XrefPath)
              else
                AList.Add(vBlock.Xref.Path);
            if not (efNoIterateChildren in AMode) and Assigned(vBlock.Xref.CADImage) then
              GetXRefListBlock(TsgCADImage(vBlock.Xref.CADImage).Converter,
                AList, AStackConverters, AMode);
          end;
        end;
        for I := 0 to AConverter.Counts[csImageDefs] - 1 do
        begin
          vImageDef := TsgDXFImageDef(AConverter.Sections[csImageDefs][I]);
          if Assigned(vImageDef) and (efAddImageDef in AMode) then
            AList.Add(vImageDef.FileName);
        end;

{$IFDEF DATA_LINK}
        // Data Link
        vDataLink := AConverter.Sections[csObjects].FindEntByName(sDictionaryAcadDataLink);
        if Assigned(vDataLink) and (efDataLink in AMode) then
        begin
          for I := 0 to vDataLink.Count - 1 do
          begin
            vFileName := TsgCADDataLink(vDataLink[I]).FileName;
            if Length(vFileName) > 0 then//trim excel paper link
            begin
              J := StringScan(cnstExclamationPoint, vFileName, Length(vFileName), True);
              if J > 1 then
              begin
                vIndex := StringScan(cnstExclamationPoint, vFileName, J - 1, True);
                if vIndex > 1 then
                  SetLength(vFileName, vIndex - 1);
              end;
              AList.Add(vFileName);
            end;
          end;
        end;
{$ENDIF}

      finally
        AStackConverters.Remove(AConverter);
      end;
    end;
  end;

begin
  vList := TsgStringList.Create;
  vStackConverters := TsgList.Create;
  try
    vList.CaseSensitive := False;
    vList.Duplicates := dupIgnore;
    vList.Sorted := True;
    GetXRefListBlock(Converter, vList, vStackConverters, AMode);
  finally
    if Assigned(AList) then
      for I := 0 to vList.Count - 1 do
        AList.Add(vList[I]);
    Result := vList.Count > 0;
    vList.Free;
    vStackConverters.Free;
  end;
end;

function TsgCADImage.GetHeight: Integer;
begin
  Result := ExtendedToInt(FZoom * AbsHeight + 0.5, 0, MaxInt shr 1);
  if Result = 0 then
    Result := 1;
end;

function TsgCADImage.GetMillimetres: Boolean;
begin
  if (FMillimetres < 0) and (FConverter <> nil) then
    FMillimetres := Integer(FConverter.HeadVarStruct.Measurement);
  Result := FMillimetres <> 0;
end;

function TsgCADImage.GetModeller: TsgModeller;
begin
  Result := TsgDXFConverterAccess(FConverter).GetModeller;
end;

function TsgCADImage.GetNullWidth: Integer;
begin
  Result := Round(FNullWidth);
end;

function TsgCADImage.GetPalColorMode(const AC1, AC2: Integer): Integer;
begin
  if IsAutocadFormat then
    Result := 4
  else
    Result := Ord((AC1 = $0) and (AC2 = $FFFFFF)) +
              Ord((AC2 = $0) and (AC1 = $FFFFFF)) shl 1;
end;

function TsgCADImage.GetPalette: HPalette;
begin
  Result := AcadPal;
end;

function TsgCADImage.GetWidth: Integer;
begin
  Result := ExtendedToInt(FZoom * AbsWidth + 0.5, 0,  MaxInt shr 1);
  if Result = 0 then
    Result := 1;
end;

procedure TsgCADImage.GetXMLIdsChangingHiding(const AChangeList,
  AHideList: TsgIntegerList; AProgID: TsgProgID);
begin

end;

{
  TsgCADImage.HasClip

  - AFilter = AInsert, if AInsert is TsgCADClipInsert
  - AFilter = TsgCADSpatialFilter object, if AInsert has spatial dictionary }

function TsgCADImage.HasClip(AInsert: TsgDXFInsert; out AFilter: TObject): Boolean;
begin
  AFilter := nil;
  Result := AInsert is TsgCADClipInsert;
  if not Result then
    Result := GetSpatialFilter(AInsert, TsgDXFEntity(AFilter))
  else
    AFilter := AInsert;
end;

procedure TsgCADImage.LoadFromClipboardFormat({$IFNDEF SGFPC}Fmt: Word; Data: THandle; Pal: HPALETTE{$ELSE}FormatID: TClipboardFormat{$ENDIF});
begin
  // do nothing
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADImage.DoAppendPoly(Points: TFPointList);
var
  I: Integer;
begin
  for I := 0 to Points.Count - 1 do
    FContext.AddPolyPoint(Points[I]);
end;

procedure TsgCADImage.DoAppendPolyFromFlat(FP: TsgFlatEntity);
var
  I: Integer;
begin
  for I := 0 to FP.PCount - 1 do
   FContext.AddPolyPoint(FP.XY[I]);
end;

procedure TsgCADImage.FillPoly(PL: TsgCADBasePolyline);
begin
  FContext.Poly.Count := 0;
  DoAppendPoly(PL.PolyPoints);
end;

function TsgCADImage.PolyRegion(PL: TsgCADBasePolyline): TGraphicsObject;
begin
  FillPoly(PL);
  Result := TsgRegion.Create;
  TsgRegion(Result).SetPolygonList(FContext.Poly, WINDING);
end;

procedure TsgCADImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
var
  T: Integer;
  {$IFDEF SG_CAD_DLL}
  Mesg: TMsg;
  {$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  T := {$IFDEF SGFPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF} and not $FF;
{$ELSE}
  T := TThread.GetTickCount;
{$ENDIF}
  if (Stage=psRunning) and (T=FLast) then
    Exit;
  FLast := T;
{$IFNDEF SG_FIREMONKEY}
  if FIsProcessMessages then
  begin
  {$IFNDEF SG_CAD_DLL}
     {$IFNDEF SG_OPENING_IN_THEADS}
     {$IFNDEF CS_PLUGINS}
     Application.ProcessMessages;
     {$ENDIF}
     {$ENDIF}
  {$ELSE}
    {$IFNDEF SG_NON_WIN_PLATFORM}
  if Stage <> psStarting then
    if PeekMessage(Mesg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage({$IFDEF SGFPC}@{$ENDIF}Mesg);
      DispatchMessage({$IFDEF SGFPC}@{$ENDIF}Mesg);
    end;
    {$ENDIF}
  {$ENDIF}
  end;
{$ENDIF}
  inherited Progress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TsgCADImage.GetLineWeightFactor: Double;
begin
  Result := GetLineWeightFactorBase;
end;

function TsgCADImage.GetLineWeightFactorBase: Double;
begin
  if IsPlotting then
    Result := cnstDefaultLineWeightFactor
  else
    Result := fLineWeightFactor;
end;

function TsgCADImage.GetLineTypeScale: Double;
//var
//  vCurrXref: TsgDXFXref;
begin
  Result := FConverter.GetLTScale;
//  vCurrXref := CurrXRef;
//  if (vCurrXref <> nil) and (vCurrXRef.CADImage <> nil) then
//    Result := Result * TsgCADImage(vCurrXRef.CADImage).Converter.GetLScale
end;

procedure TsgCADImage.SaveDrawParams;
begin
  FDrawSaved := FDraw;
end;

procedure TsgCADImage.SaveToClipboardFormat({$IFNDEF SGFPC}var Fmt: Word; var Data: THandle; var Pal: HPALETTE{$ELSE}FormatID: TClipboardFormat{$ENDIF});
begin
  // do nothing
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADImage.SetExpAnnotation(const Value: TsgExpAnnotation);
begin
  FExpProcs.ExpAnnotation := Value;
end;

procedure TsgCADImage.SetExpArc(const Value: TsgExpArc);
begin
  FExpProcs.ExpArc := Value;
end;

procedure TsgCADImage.SetExpClipRgn(const Value: TsgExpClipRgn);
begin
  FExpProcs.ExpClipRgn := Value;
end;

procedure TsgCADImage.SetExpCloseFigure(const Value: TsgExpCloseFigure);
begin
  FExpProcs.ExpCloseFigure := Value;
end;

procedure TsgCADImage.SetExpFillRgn(const Value: TsgExpFillRgn);
begin
  FExpProcs.ExpFillRgn := Value;
end;

procedure TsgCADImage.SetExpImage(const Value: TsgExpImage);
begin
  FExpProcs.ExpImage := Value;
end;

procedure TsgCADImage.SetExpImageUV(const Value: TsgExpImageUV);
begin
  FExpProcs.ExpImageUV := Value;
end;

procedure TsgCADImage.SetExpPixel(const Value: TsgExpPixel);
begin
  FExpProcs.ExpPixel := Value;
end;

procedure TsgCADImage.SetExpPolygon(const Value: TsgExpPolygon);
begin
  FExpProcs.ExpPolygon := Value;
end;

procedure TsgCADImage.SetExpPolyline(const Value: TsgExpPolyline);
begin
  FExpProcs.ExpPolyline := Value;
end;

procedure TsgCADImage.SetExpPolyPolygon(const Value: TsgExpPolyPolygon);
begin
  FExpProcs.ExpPolyPolygon := Value;
end;

procedure TsgCADImage.SetExpPolyPolyline(const Value: TsgExpPolyPolyline);
begin
  FExpProcs.ExpPolyPolyline := Value;
end;

procedure TsgCADImage.SetExpProgress(const Value: TsgExpProgress);
begin
  FExpProcs.ExpProgress := Value;
end;

procedure TsgCADImage.SetExpRestoreDC(const Value: TsgExpRestoreDC);
begin
  FExpProcs.ExpRestoreDC := Value;
end;

procedure TsgCADImage.SetExpSaveDC(const Value: TsgExpSaveDC);
begin
  FExpProcs.ExpSaveDC := Value;
end;

procedure TsgCADImage.SetExpSetColor(const Value: TsgExpSetColor);
begin
  FExpProcs.ExpSetColor := Value;
end;

procedure TsgCADImage.SetExpSetFont(const Value: TsgExpSetFont);
begin
  FExpProcs.ExpSetFont := Value;
end;

procedure TsgCADImage.SetExpSetPenGeometric(const Value: TsgExpSetPenGeometric);
begin
  FExpProcs.ExpSetPenGeometric := Value;
end;

procedure TsgCADImage.SetExpSetPenMiterLimit(
  const Value: TsgExpSetPenMiterLimit);
begin
  FExpProcs.ExpSetPenMiterLimit := Value;
end;

procedure TsgCADImage.SetExpSetPenWidth(const Value: TsgExpSetPenWidth);
begin
  FExpProcs.ExpSetPenWidth := Value;
end;

procedure TsgCADImage.SetExpSetStyle(const Value: TsgExpSetStyle);
begin
  FExpProcs.ExpSetStyle := Value;
end;

procedure TsgCADImage.SetExpText(const Value: TsgExpText);
begin
  FExpProcs.ExpText := Value;
end;

procedure TsgCADImage.SetExpTextOut(const Value: TsgExpTextOut);
begin
  FExpProcs.ExpTextOut := Value;
end;

procedure TsgCADImage.SetExtents(const Value: TFRect);
begin
  FExtents := Value;
end;

procedure TsgCADImage.SetExtentsParameters(const ARect: TFRect; const AIs3DExtents: Boolean);
const
  flRatio = 16;
var
  vWidth, vHeight: TsgFloat;
  vRect: TFRect;
  vRectRatio: Double;
begin
  if IsBadRect(ARect) then
  begin
    if Assigned(FOnGetBox) then
    begin
      vRect := ARect;
      if FOnGetBox(Self, vRect) then
        FPureExtents := vRect
      else
        FPureExtents := UnitRect;
    end
    else
      FPureExtents := UnitRect;
    FXMax := FPureExtents.Right;
    FYMax := FPureExtents.Top;
    FZMax := FPureExtents.Z2;
    FXMin := FPureExtents.Left;
    FYMin := FPureExtents.Bottom;
    FZMin := FPureExtents.Z1;
    ResetExtents;
    Changed(Self);
    Exit;
  end;
  if IsBadRect(FDrawingBox) then
    vRect := ARect
  else
    vRect := FDrawingBox;

  if Assigned(FOnGetBox) then
    FOnGetBox(Self, vRect);

  FCenter.X := 0.5 * (vRect.Right + vRect.Left);
  FCenter.Y := 0.5 * (vRect.Top + vRect.Bottom);
  FCenter.Z := 0.5 * (vRect.Z1 + vRect.Z2);
  Offset := FCenter;
//  if AIs3DExtents then//It is not actual for 2D layot because it has a rotate 3D matrix
    TransRectCorners(vRect, GetRealImageMatrix);
  FXMin := 0;
  FYMin := 0;
  FPureExtents := vRect;
  if not FIsWithoutBorder then
  begin
    case FBorderType of
      btRatio:
        begin
          FXMin := (vRect.Right - vRect.Left) * FBorderSize;
          FYMin := (vRect.Top - vRect.Bottom) * FBorderSize;
        end;
      btGlobal:
        begin
          vWidth := vRect.Right - vRect.Left;
          vHeight := vRect.Top - vRect.Bottom;
          FXMin := Abs(FBorderSize);
          FYMin := Abs(FBorderSize);
          if vHeight <> 0 then
          begin
            vRectRatio := vWidth / vHeight;
            if vRectRatio > 1 then
              FXMin := FYMin / vRectRatio
            else
              FYMin := FXMin / vRectRatio;
          end;
        end;
    end;
    if FXMin < 0 then
      FXMin := 1;
    if FYMin < 0  then
      FYMin := 1;
  end;
  if IsBadRect(vRect) then// for unsized model or layouts
  begin
    FPureExtents := UnitRect;
    FXMax := FPureExtents.Right;
    FYMax := FPureExtents.Top;
    FZMax := FPureExtents.Z2;
    FXMin := FPureExtents.Left;
    FYMin := FPureExtents.Bottom;
    FZMin := FPureExtents.Z1;
  end
  else
  begin
    FXMax := vRect.Right + FXMin;
    FYMax := vRect.Top + FYMin;
    FZMax := vRect.Z2;
    FXMin := vRect.Left - FXMin;
    FYMin := vRect.Bottom - FYMin;
    FZMin := vRect.Z1;
  end;
  FXDisproportionateShift := 0;
  FYDisproportionateShift := 0;
  if FResizeDisproportionateDrawing then
  begin
    vWidth := Abs(FXMax - FXMin);
    vHeight := Abs(FYMax - FYMin);
    if (vWidth <> 0)and(vHeight / vWidth > flRatio) or (vWidth = 0) then
    begin
      FXDisproportionateShift := (vHeight - vWidth) / 2;
      FXMin := FXMin - FXDisproportionateShift;
      FXMax := FXMax + FXDisproportionateShift;
    end
    else
      if (vHeight <> 0)and(vWidth / vHeight > flRatio) or (vHeight = 0) then
    begin
      FYDisproportionateShift := (vWidth - vHeight) / 2;
      FYMax := FYMax + FYDisproportionateShift;
      FYMin := FYMin - FYDisproportionateShift;
    end;
  end;
  ResetExtents;
  Changed(Self);
end;

procedure TsgCADImage.SetExternalRegion(const Value: TsgDXFEntity);
begin
  FExternalRegion := Value;
end;

procedure TsgCADImage.SetGuid(const AGuid: TGUID);
begin
  FGuid := AGuid;
end;

function TsgCADImage.GetOnInsertMode: TsgDXFInsert;
begin
  Result := FOnInsert;
end;

procedure TsgCADImage.SetOnAfterDraw(const Value: TNotifyEvent);
begin
  FOnAfterDraw := Value;
end;

procedure TsgCADImage.SetOnBeforeDraw(const AValue: TNotifyEvent);
begin
  FOnBeforeDraw := AValue;
end;

procedure TsgCADImage.SetOnInsertMode(const AIns: TsgDXFInsert);
begin
  FOnInsert := AIns;
  FOnInsertMode := False;
end;

procedure TsgCADImage.SetHeight(Value: Integer);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADImage.SetMillimetres(Value: Boolean);
begin
  FMillimetres := Ord(Value);
end;

procedure TsgCADImage.SetNullWidth(const Value: Integer);
begin
  FNullWidth := Value;
end;

procedure TsgCADImage.SetNullWidthExt(const Value: Double);
begin
  FNullWidth := Value;
end;

procedure TsgCADImage.SetSelectionMatrix(const Value: TsgSelectionMatrix);
begin
  if FSelectionMatrix <> Value then
  begin
    FSelectionMatrix := Value;
    UpdateContextSelectionMatrix;
  end;
end;

procedure TsgCADImage.SetSelectionMatrixProc(const AMode: TsgSelectionMode);
begin
  FSelectionMatrixMode := AMode;
end;

procedure TsgCADImage.SetWidth(Value: Integer);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

// -1 disable
// range in 0...1
procedure TsgCADImage.SetXrefTransparency(const Value: Single);
begin
  FXrefTransparency := Value;
end;

{$IFDEF SGFPC}
procedure TsgCADImage.SetTransparent(Value: Boolean);
begin
  if not FIsShowBackground <> Value then
  begin
    FIsShowBackground := not Value;
    Changed(Self);
  end;
end;
{$ENDIF}

function TsgCADImage.GetSHXFontsProc: TsgSHXFontList;
begin
  Result := Converter.SHXFonts;
end;

function TsgCADImage.GetScale: TFPoint;
begin
  Result := ExtractMatrixAbsoluteScale(FDraw.DrawMatrix);// need current view of whole drawing
end;

function TsgCADImage.GetSelectionMatrixProc: TsgSelectionMode;
begin
  Result := FSelectionMatrixMode;
end;

function TsgCADImage.GetShowLineWeight: Boolean;
begin
  Result := IsShowLineWeight;
  if Assigned(FExpProcs) and FExpProcs.Active then
    Result := FShowLineWeightExport;
end;

function TsgCADImage.InitializeLineType(AEntity: TsgDXFPenEntity): Boolean;
var
  vDx, vDY, vPatternSize: Double;
begin
  FEntityLines.Scale := FEntityLines.Scale * GetLineTypeScale * AEntity.LineTypeScale;
  if Assigned(FExpProcs) and FExpProcs.Active and (FExpProcs.LineTypeMode > 0) then
  begin
    Result := False;
  end
  else
  begin
    GetPixelSize(1.5, vdX, vdY, not IsAutocadLType);
    vPatternSize := FEntityLines.MaxSizeOfPatterns *  FEntityLines.Scale;
    Result := (vPatternSize > vDx) or  (vPatternSize > vDy);
  end;
  FDottedSingPts.Count := 0;
end;

procedure TsgCADImage.MakeAndDrawDottedSingPts(AEntity: TsgDXFEntity;
  const APolyPoints: TFPointList; const AClosed: Boolean);
var
  vMatrixModePrev: TsgSelectionMode;
begin
  vMatrixModePrev := GetMatrixMode;
  if cnstSolidSelection and Assigned(APolyPoints) and (vMatrixModePrev <> smDisabled) then
  begin
    try
      if vMatrixModePrev <> smMatrixOnly then
      begin
        //SetMatrixMode(smDisabled);
        CalcDottedSingPts(AEntity, FEntityLines, FDraw.ConvertMatrixCurrentInsertToWCS, FDottedSingPts);
        DrawPointsListByPolyPolyline(AEntity, FDottedSingPts, AClosed);
      end;
      SetMatrixMode(smMatrixOnly);
      DrawPointsListByPolyline(AEntity, APolyPoints, AClosed);
    finally
      SetMatrixMode(vMatrixModePrev);
    end;
  end
  else
  begin
    CalcDottedSingPts(AEntity, FEntityLines, FDraw.ConvertMatrixCurrentInsertToWCS, FDottedSingPts);
    DrawPointsListByPolyPolyline(AEntity, FDottedSingPts, AClosed);
  end;
end;

function TsgCADImage._AddRef: Integer;{$IFNDEF SG_LINUX}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Result := -1;
end;

function TsgCADImage._Release: Integer;{$IFNDEF SG_LINUX}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Result := -1;
end;

{$IFDEF SGFPC}
function TsgCADImage.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TsgCADImage.QueryInterface(const IID: TGUID; out Obj): HResult;
{$ENDIF}
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TsgCADImage.ApplyLineWeightFactor(var ALineWeight: Double);
begin
  ApplyLineWeightFactorBase(ALineWeight);
end;

procedure TsgCADImage.ApplyLineWeightFactorBase(var ALineWeight: Double);
begin
  //if CurrentLayout = Layouts[0] then//is model space for future version
  begin
    ALineWeight := GetLineWeightFactor * ALineWeight;
    if ALineWeight > cnstCADLineWeightMax then
      ALineWeight := cnstCADLineWeightMax;
  end;
end;

function TsgCADImage.UseKeyColor(const AKey: TsgColorCAD;
  const AContextPenColor: TsgColorCAD; const ADrawPenColor: Integer) : Boolean;
var
  vContextPenColorRGB: Integer;
  vKeyRGB: Integer;
begin
  if IsEqualColorCAD(AContextPenColor, AKey) then
    Result := True
  else
  begin
    vContextPenColorRGB := ConvertColorCADToRGB(AContextPenColor);
    vKeyRGB := ConvertColorCADToRGB(AKey);
    if UseColorRGBInColorMap and (vContextPenColorRGB = vKeyRGB)  then
      Result := True
    else
      Result := ((vContextPenColorRGB = clNone) and (vKeyRGB = clBlack)) or
                ((vContextPenColorRGB = ADrawPenColor) and (vKeyRGB = clNone));
  end;
end;

procedure TsgCADImage.ApplyPen(Sender: TObject; const AEntColor: PsgColorCAD);
var
  I, vDefNullWidth: Integer;
  vDrawPenColor: TColor;
  W, vLineWeight: Double;
  vUseColorMap: Boolean;
  vColorKey, vContextPenColor: TsgColorCAD;
begin
  W := cnstPenWidthNull;
  if IsPlotting then
    W := FNullWidth;
  if GetShowLineWeight then
  begin
    vLineWeight := GetEntLineWeight(TsgDXFEntity(Sender), FDraw.Insert);
    W := FLineWeightScale * vLineWeight;
    vUseColorMap := (FColorToLineWeightMap.Count > 0) and Assigned(FColorToLineWeightMap.Weights) and
      GetColorToLineWeight(vContextPenColor, AEntColor);
    if vUseColorMap then
    begin
      vDrawPenColor := GetDrawPenColor;
      for I := 0 to FColorToLineWeightMap.Count - 1 do
      begin
        vColorKey := FColorToLineWeightMap.Keys[I];
        vLineWeight := FColorToLineWeightMap.Weights[I];
        if (vLineWeight = fLineWeightDefault) or IsEqualColorCAD(vColorKey, cnstColorCADLWeightBadKey) then
          Continue;
        if UseKeyColor(vColorKey, vContextPenColor, vDrawPenColor) then
        begin
          if vLineWeight >= 0 then
          begin
            ApplyMMToPixelX(vLineWeight);
            W := FLineWeightScale * vLineWeight;
            if (W = 0) and IsPlotting then
              W := FNullWidth;
          end;
          Break;
        end;
      end;
    end;
  end;
{$IFDEF SG_FIREMONKEY}
  if FExpProcs.Active then
    vDefNullWidth := 0
  else
{$ENDIF}
    vDefNullWidth := cnstPenWidthNull;
  if W < vDefNullWidth then
    W := vDefNullWidth;
  SetPenWidth(W);
end;

procedure TsgCADImage.ApplyScale(const ARect: TRect);
begin
   ApplyScale(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TsgCADImage.ApplyScale(const ARectLeft, ARectTop, ARectRight, ARectBottom: Double);
var
  vDrawMatrix: TFMatrix;
begin
  vDrawMatrix := sgFunction.ApplyScale(GetRealImageMatrix, FOffset, GetDrawingExtents,
    ARectLeft, ARectTop, ARectRight, ARectBottom, Stretch,
    not IsIterator or not Converter.IsCrossoverMatrix);
  SetDrawMatrix(vDrawMatrix);
end;

procedure TsgCADImage.ApplyText;
var
  vColor: Integer;
begin
  case FDrawMode of
    dmBlack, dmGray:
      begin
        vColor := GetDrawPenColor;
        if FDrawMode = dmGray then
          vColor := ConvertColortoGray(vColor);
        DoSetColor(vColor, ctFont, 3);
        DoSetColor(vColor, ctPen, 3);
      end;
  end;
end;

function TsgCADImage.ApplyAlternateWhite(var AColor: TColor): Boolean;

  function IsWhite: Boolean;
  begin
    Result := ((FBackgroundColor and $FFFFFF) <> clBlack) and
              ((AColor and $FFFFFF) = clWhite);
  end;

  function IsBlack: Boolean;
  begin
    Result := ((FBackgroundColor and $FFFFFF) = clBlack) and
              ((AColor and $FFFFFF) = clBlack);
  end;

begin
  Result := False;
  if FDrawMode = dmGray then
  begin
    if IsWhite then
    begin
      Result := True;
      AColor := clBlack;
    end
    else
    begin
      if IsBlack then
      begin
        Result := True;
        AColor := clWhite;
      end;
    end;
  end
  else
  begin
    if AlternateWhite and IsWhite then
    begin
      Result := True;
      AColor := AlternateWhiteColor;
    end;
  end;
end;

procedure TsgCADImage.ApplyMMToPixelX(var AWeight: Double);
begin
  if FmmToPixelX <> 0 then
    AWeight := AWeight / FmmToPixelX;
end;

procedure TsgCADImage.AddLastPixelToPoly(const IsPoly: Boolean; ACounts: TsgIntegerList);
begin
  if IsPoly then
  begin
    if FContext.Poly.Count = 0  then
      Exit;
    if NeedToAddOnePixel then
      FContext.AddPolyLastPoint;
  end
  else
  begin
    if FContext.IntPoints.Count = 0 then
      Exit;
    if NeedToAddOnePixel then
      FContext.AddIntLastPoint;
  end;
  if NeedToAddOnePixel then
    ACounts.List[ACounts.Count - 1] := ACounts[ACounts.Count - 1] + 1;
end;

procedure TsgCADImage.AddRectAsPoly(const ABounds: TRect;
  const APoly: TsgIntegerList; const AClosed: Boolean = True);
begin
  APoly.Add(ABounds.Left);
  APoly.Add(ABounds.Top);

  APoly.Add(ABounds.Right);
  APoly.Add(ABounds.Top);

  APoly.Add(ABounds.Right);
  APoly.Add(ABounds.Bottom);

  APoly.Add(ABounds.Left);
  APoly.Add(ABounds.Bottom);
  if AClosed then
  begin
    APoly.Add(ABounds.Left);
    APoly.Add(ABounds.Top);
  end;
end;

procedure TsgCADImage.BeginRead(Sender: TObject);
begin
  DoProgress(psStarting);
end;

// evg reg - see this funciton to improve speed
procedure TsgCADImage.BoxPoint(const P: TFPoint);
var
  IP: TPoint;
begin
  IP := GetPoint(P);
  ExpandRect(FRect, IP);
  FContext.BoxPoint(IP);
  if FExpProcs.Active then
    FExpProcs.BoxPoint(IP);
end;

procedure TsgCADImage.DoProgress(Stage: TProgressStage);        // OnProgress event
const
  R: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  vPercentDoneE: Extended;
begin
  if not Assigned(OnProgress) then
    Exit;
  vPercentDoneE := FConverter.Pos / FConverter.Count;
  if vPercentDoneE > 1 then vPercentDoneE := 1;
  FPercentDone := Round(vPercentDoneE * 100);
  Progress(Self, Stage, FPercentDone, False, R, FMsg);
end;

procedure TsgCADImage.DoRead(Sender: TObject);
begin
  DoProgress(psRunning);
end;

procedure TsgCADImage.DrawArc(Sender: TObject);
var
  R: TRect;
  P1, P2: TPoint;
  A: TsgArc;
begin
  if FUseWinEllipse and FEntityLines.IsSolid and (Converter.VPort.X = 0)
      and (Converter.VPort.Y = 0) and (Converter.VPort.Z > 0) and (not IsDrawingRotated) then
  begin
    A := EntArc(TsgDXFArc(Sender), FDraw);
    if not A.Valid then
    begin
      DrawPoly(Sender);
      Exit;
    end;
    R.Left := Round(A.Rect.Left);
    R.Top := Round(A.Rect.Top);
    R.Right := Round(A.Rect.Right);
    R.Bottom := Round(A.Rect.Bottom);
    P1.X := Round(A.Point1.X);
    P1.Y := Round(A.Point1.Y);
    P2.X := Round(A.Point2.X);
    P2.Y := Round(A.Point2.Y);
    if (P1.X = P2.X) and (P1.Y = P2.Y) then
      ExpPixelInteger(Sender, P1, FColor)
    else
      ExpArcInteger(Sender, R.Left, R.Top, R.Right, R.Bottom, P1.X, P1.Y, P2.X, P2.Y, A);
  end
  else
    DrawPoly(Sender);
end;

{$IFDEF SG_USEGDIPLUS}

procedure SetGPGraphicsParams(const AGPGraphics: TGPGraphics;
  const AOptimization: TsgGDIPlusOpimization);
begin
  case AOptimization of
    opSpeed:
      begin
        AGPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
        AGPGraphics.SetInterpolationMode(InterpolationModeNearestNeighbor);
      end;
    opQuality:
      begin
        AGPGraphics.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        AGPGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);
      end;
  end;
  AGPGraphics.SetPageUnit(UnitPixel);
end;
{$ENDIF}

procedure TsgCADImage.DrawCADFill(Sender: TObject);
type
  TGPRectD = record
    X, Y, Width, Height: Double;
  end;
var
  vCadFill: TsgCADFill absolute Sender;
  vStyle: TsgCADStyleFill;
  vGradient: TsgCADStyleGradient absolute vStyle;
  vPattern: TsgCADStylePattern absolute vStyle;
  vColors: TsgIntegerList;
  vPositions: TsgSingleList;
  vScreenRect: TGPRectD;
  GP1, GP2: TPointF;
  vBoxNew: TRectF;
{$IFDEF SG_USEGDIPLUS}
  graphics: TGPGraphics;
  GradP: TGPPathGradientBrush;
  GradL: TGPLinearGradientBrush;
  GBrushSolid: TGPSolidBrush;
  GPath: TGPGraphicsPath;
  GPen: TGPPen;
  GEllipse: TGPRectF;
  I, J: Integer;
  vPenWidth: Double;
  vBmp: TGPBitmap;
  vAttr: TGPImageAttributes;
  vPts: array[0..2] of TGPPoint;
{$ENDIF}

  function IsCADFillUsedBuffer(const ACadFill: TsgCADFill): Boolean;
  begin
    Result := {$IFDEF SG_THREAD_DRAW}False{$ELSE}ACadFill.UseBuffer{$ENDIF};
  end;

  function GetScreenRect(const ABox: TFRect): TGPRectD;
  var
    vRect: TFRect;
  begin
    vRect := GetRealBox(ABox, FDraw.Matrix);
    Result.X := vRect.Left;
    Result.Y := vRect.Bottom;
    Result.Width := vRect.Right - vRect.Left;
    Result.Height := vRect.Top - vRect.Bottom;
  end;

  function GetGradPoint(const APoint: TFPoint): TPointF;
  var
    V: array[0..2] of Single;
  begin
    TransformPointTo3f(APoint, FDraw.Matrix, @V[0]);
    Result.X := V[0];
    Result.Y := V[1];
  end;

  procedure SetGP12Rel(const AP1, AP2: TFPoint; out P1, P2: TPointF);
  var
    vKX, vKY: Double;
  begin
    vKX := vScreenRect.Width / 100;
    vKY := vScreenRect.Height / 100;
    P1.X := vKX * AP1.X + vScreenRect.X;
    P1.Y := vKY * AP1.Y + vScreenRect.Y;
    P2.X := vKX * AP2.X + vScreenRect.X;
    P2.Y := vKY * AP2.Y + vScreenRect.Y;
  end;

{$IFDEF SG_USEGDIPLUS}
  function GPMakeRect(const AR: TGPRectD): TGPRectF; overload;
  begin
    Result.X := AR.X;
    Result.Y := AR.Y;
    Result.Width := AR.Width;
    Result.Height := AR.Height;
  end;

  function GPMakeRect(const AR: TRectF): TGPRectF; overload;
  begin
    Result.X := AR.Left;
    Result.Y := AR.Top;
    Result.Width := AR.Right - AR.Left;
    Result.Height := AR.Bottom - AR.Top;
  end;
{$ENDIF}

  procedure ExpandGradBox(const ACenter: TFPoint; const ARadius: Double;
    const ATransform: TFMatrix; var ARect: TRectF);
  begin
    SetNewBox(
      GetGradPoint(FPointXMat(MakeFPoint(ACenter.X - ARadius, ACenter.Y, 0), ATransform)),
      GetGradPoint(FPointXMat(MakeFPoint(ACenter.X, ACenter.Y - ARadius, 0), ATransform)),
      GetGradPoint(FPointXMat(MakeFPoint(ACenter.X + ARadius, ACenter.Y, 0), ATransform)),
      GetGradPoint(FPointXMat(MakeFPoint(ACenter.X, ACenter.Y + ARadius, 0), ATransform)), ARect);
  end;

{$IFDEF SG_USEGDIPLUS}
  procedure DrawLinearGradient;
  begin
    if vGradient.Units = sfUserSpaceOnUse then
    begin
      GP1 := GetGradPoint(FPointXMat(vGradient.Point1, vGradient.Transform));
      GP2 := GetGradPoint(FPointXMat(vGradient.Point2, vGradient.Transform));
    end
    else
      SetGP12Rel(vGradient.Point1, vGradient.Point2, GP1, GP2);
    if IsEqualPoints(TPointF(GP1), TPointF(GP2)) then//REC-SVG11-20030114/pservers.html#LinearGradients
    begin
      GBrushSolid := TGPSolidBrush.Create(vColors.First);
      try
        graphics.FillRectangle(GBrushSolid, GPMakeRect(vScreenRect));
      finally
        GBrushSolid.Free;
      end;
    end
    else
    begin
      vBoxNew := RectF(vScreenRect.X, vScreenRect.Y,
        vScreenRect.X + vScreenRect.Width, vScreenRect.Y + vScreenRect.Height);
      ExpandRect(vBoxNew, TPointF(GP1));
      ExpandRect(vBoxNew, TPointF(GP2));
      vPenWidth := Sqr(vBoxNew.Bottom - vBoxNew.Top) + Sqr(vBoxNew.Right - vBoxNew.Left);
      if vPenWidth > fDoubleResolution then
        vPenWidth := Sqrt(vPenWidth) * 2
      else
        vPenWidth := 1;
      GradL := TGPLinearGradientBrush.Create(TGPPointF(GP1), TGPPointF(GP2), Cardinal(TsgColor(vColors.First)), Cardinal(TsgColor(vColors.Last)));
      try
        GradL.SetInterpolationColors(@vColors.List[0], @vPositions.List[0], vColors.Count);
        GPen := TGPPen.Create(GradL, vPenWidth);
        try
          graphics.DrawLine(GPen, TGPPointF(GP1), TGPPointF(GP2));
        finally
          GPen.Free;
        end;
        GPen := TGPPen.Create(Cardinal(TsgColor(vColors.First)), vPenWidth);
        try
          graphics.DrawLine(GPen, TGPPointF(GetPointOnLineF(GP1, GP2, 2)), TGPPointF(GetPointOnLineF(GP1, GP2, -vPenWidth)));
        finally
          GPen.Free;
        end;
        GPen := TGPPen.Create(Cardinal(TsgColor(vColors.Last)), vPenWidth);
        try
          graphics.DrawLine(GPen, TGPPointF(GetPointOnLineF(GP2, GP1, 2)), TGPPointF(GetPointOnLineF(GP1, GP2, vPenWidth)));
        finally
          GPen.Free;
        end;
      finally
        GradL.Free;
      end;
    end;
  end;

  procedure DrawLinearCircular;
  begin
    if vGradient.Units = sfUserSpaceOnUse then
    begin
      GP1 := GetGradPoint(FPointXMat(vGradient.PointC, vGradient.Transform));
      GP2 := GetGradPoint(FPointXMat(vGradient.PointF, vGradient.Transform));
      ExpandGradBox(vGradient.PointC, vGradient.R, vGradient.Transform, vBoxNew);
    end
    else
    begin
      SetGP12Rel(vGradient.PointC, vGradient.PointF, GP1, GP2);
      I := Ceil(vGradient.R * vScreenRect.Width / 100);
      if not IsEqual(vScreenRect.Width, vScreenRect.Height) then
        J := Ceil(vGradient.R * vScreenRect.Height / 100)
      else
        J := I;
      SetNewBox(sgConsts.MakePointF(GP1.X - I, GP1.Y), sgConsts.MakePointF(GP1.X, GP1.Y - J),
                sgConsts.MakePointF(GP1.X + I, GP1.Y), sgConsts.MakePointF(GP1.X, GP1.Y + J), vBoxNew)
    end;
    GEllipse := GPMakeRect(vBoxNew);
    GBrushSolid := TGPSolidBrush.Create(vColors.First);
    try
      graphics.FillRectangle(GBrushSolid, GPMakeRect(vScreenRect));
    finally
      GBrushSolid.Free;
    end;
    GPath := TGPGraphicsPath.Create;
    try
      GPath.StartFigure;
      GPath.AddEllipse(GEllipse);
      GPath.SetFillMode(FillModeWinding);
      GradP := TGPPathGradientBrush.Create(GPath);
      try
        GradP.SetCenterPoint(TGPPointF(GP2));
        GradP.SetInterpolationColors(@vColors.List[0], @vPositions.List[0], vColors.Count);
        graphics.FillPath(GradP, GPath);
      finally
        GradP.Free;
      end;
    finally
      GPath.Free;
    end;
  end;
{$ENDIF}

  procedure DrawGDIGradient;
{$IFDEF SG_FIREMONKEY}
  var
    I, J: Integer;
    vColors: TsgIntegerList;
    vPositions: TsgSingleList;
    vPathData: TPathData;
    vGradientBrush: TBrush;
    V, vScale: TPointF;
    P, E, S: TFPoint;
    A: Double;
    vTransform: FMX.Types.TTransform;

    function RGBAToAlphaColor(const ARGBA: Integer): TAlphaColor;
    begin
      PByteArray(@Result)^[0] := PByteArray(@ARGBA)^[2];//Blue
      PByteArray(@Result)^[1] := PByteArray(@ARGBA)^[1];//Green
      PByteArray(@Result)^[2] := PByteArray(@ARGBA)^[0];//Red
      PByteArray(@Result)^[3] := 255 - PByteArray(@ARGBA)^[3];//Clarity
    end;

  begin
    vScreenRect := GetScreenRect(vCadFill.Box);
    vColors := TsgIntegerList.Create;
    vPositions := TsgSingleList.Create;
    try
      vGradient.GetPositionColors(vPositions, vColors);
      vGradientBrush := TBrush.Create(TBrushKind.Gradient, FColorBrush.AsBGRA);
      try
        if vGradient.Linear then
        begin
          vGradientBrush.Gradient.Style := TGradientStyle.Linear;
          GP1 := GetGradPoint(FPointXMat(vGradient.Point1, vGradient.Transform));
          GP2 := GetGradPoint(FPointXMat(vGradient.Point2, vGradient.Transform));
          V := GP2 - GP1;
          if V.Y < 0 then
          begin
            vGradientBrush.Gradient.StartPosition.Point := V.Normalize;
            vGradientBrush.Gradient.StopPosition.Point := TPointF.Create(0, 0);
          end
          else
          begin
            vGradientBrush.Gradient.StartPosition.Point := TPointF.Create(0, 0);
            vGradientBrush.Gradient.StopPosition.Point := V.Normalize;
          end;
        end
        else
        begin
          vTransform := vGradientBrush.Gradient.RadialTransform;
          vTransform.RotationCenter.Point := TPointF.Create(FContext.Rect.CenterPoint);
          vGradientBrush.Gradient.Style := TGradientStyle.Radial;
          ExtractMatrixParams(vGradient.Transform, P, S, E, A);
          vBoxNew := TRectF.Create(cnstBad2DRect);
          if vGradient.Units = sfUserSpaceOnUse then
          begin
            GP1 := GetGradPoint(FPointXMat(vGradient.PointC, vGradient.Transform));
            GP2 := GetGradPoint(FPointXMat(vGradient.PointF, vGradient.Transform));
            ExpandGradBox(vGradient.PointC, vGradient.R, vGradient.Transform, vBoxNew);
            vScale := TPointF.Create(vBoxNew.Width / vScreenRect.Width, vBoxNew.Height / vScreenRect.Height);
            vTransform.Position.Point := GP2 - vTransform.RotationCenter.Point * vScale;
            vTransform.Scale.Point := vScale;
            vTransform.RotationAngle := A;
          end
          else
          begin
            SetGP12Rel(vGradient.PointC, vGradient.PointF, GP1, GP2);
            I := Ceil(vGradient.R * vScreenRect.Width / 100);
            if not IsEqual(vScreenRect.Width, vScreenRect.Height) then
              J := Ceil(vGradient.R * vScreenRect.Height / 100)
            else
              J := I;
            SetNewBox(MakePointF(GP1.X - I, GP1.Y), MakePointF(GP1.X, GP1.Y - J),
                      MakePointF(GP1.X + I, GP1.Y), MakePointF(GP1.X, GP1.Y + J), vBoxNew);
            vTransform.Position.Point := GP2 - GP1;
            vTransform.Scale.Point := TPointF.Create(1, 1);
            vTransform.RotationAngle := A;
          end;
        end;
        vGradientBrush.Gradient.Points.Clear;
        for I := 0 to vColors.Count - 1 do
          with TGradientPoint(vGradientBrush.Gradient.Points.Add) do
          begin
            Offset := vPositions[I];
            Color := RGBAToAlphaColor(vColors[I]);
          end;
        vPathData := TPathData.Create;
        try
          FContext.Canvas.AppendPathData(vPathData, PPoint(FContext.Poly.List), [FContext.Poly.Count shr 1], TsgPathDataType.pdtPolygone);
          FContext.Canvas.FillPath(vPathData, 1.0, vGradientBrush);
        finally
          vPathData.Free;
        end;
      finally
        vGradientBrush.Free;
      end;
    finally
      vPositions.Free;
      vColors.Free;
    end;
  end;
{$ELSE}
  var
    I, J: Integer;
    vRadius: Extended;
    R: TRect;
    vP1, vP2: TFPoint;

    function RoundRect(const AR: TRectF): TRect;
    begin
      Result.Left := Round(AR.Left);
      Result.Top := Round(AR.Top);
      Result.Right := Round(AR.Right);
      Result.Bottom := Round(AR.Bottom);
    end;

    function GPRectToRectF(const AR: TGPRectD): TRectF;
    begin
      Result.Left := AR.X;
      Result.Top := AR.Y;
      Result.Right := AR.X + AR.Width;
      Result.Bottom := AR.Y + AR.Height;
    end;

  begin
    vColors := TsgIntegerList.Create;
    vPositions := TsgSingleList.Create;
    try
      vGradient.GetPositionColors(vPositions, vColors);
      if vColors.Count <= 0 then
      begin
        vColors.Add(vCadFill.GradientColor[0]);
        vColors.Add(vCadFill.GradientColor[1]);
        vPositions.Add(0);
        vPositions.Add(1);
      end;
      for I := 0 to vColors.Count - 1 do
        vColors[I] := BGRToRGB(vColors[I]);
      vBoxNew := GPRectToRectF(vScreenRect);
      if vGradient.Units = sfUserSpaceOnUse then
      begin
        GP1 := GetGradPoint(FPointXMat(vGradient.PointC, vGradient.Transform));
        GP2 := GetGradPoint(FPointXMat(vGradient.PointF, vGradient.Transform));
        ExpandGradBox(vGradient.PointC, vGradient.R, vGradient.Transform, vBoxNew);
        vP1 := MakeFPoint(vGradient.PointC.X - vGradient.R, vGradient.PointC.Y - vGradient.R);
        vP2 := MakeFPoint(vGradient.PointC.X + vGradient.R, vGradient.PointC.Y + vGradient.R);
        vP1 := FPointXMat(FPointXMat(vP1, vGradient.Transform), FDraw.Matrix);
        vP2 := FPointXMat(FPointXMat(vP2, vGradient.Transform), FDraw.Matrix);
        vRadius := 0.5*DistanceFPoint(vP1, vP2);
      end
      else
      begin
        SetGP12Rel(vGradient.PointC, vGradient.PointF, GP1, GP2);
        I := Ceil(vGradient.R * vScreenRect.Width / 100);
        if not IsEqual(vScreenRect.Width, vScreenRect.Height) then
          J := Ceil(vGradient.R * vScreenRect.Height / 100)
        else
          J := I;
        SetNewBox(sgConsts.MakePointF(GP1.X - I, GP1.Y), sgConsts.MakePointF(GP1.X, GP1.Y - J),
                  sgConsts.MakePointF(GP1.X + I, GP1.Y), sgConsts.MakePointF(GP1.X, GP1.Y + J), vBoxNew);
        vRadius := 0.5*DistanceFPoint(MakeFPointFromPoint(vBoxNew.TopLeft), MakeFPointFromPoint(vBoxNew.BottomRight));
      end;
      R := RoundRect(vBoxNew);
      if vGradient.Linear then
        FContext.LinearGradFill(nil^, R, vPositions.List^, vColors.List^, vColors.Count, GP2.Y <> GP1.Y)
      else
        FContext.RadialGradFill(nil^, GP2.X, GP2.Y, vRadius, R,
            vPositions.List^, vColors.List^, vColors.Count);
    finally
      vColors.Free;
      vPositions.Free;
    end;
  end;
{$ENDIF}

  function GetStart(const VImg, VPat, VSize: Double): Double;
  var
    vDelta, vShift: Double;
  begin
    vDelta := VImg - VPat;
    if vDelta = 0 then
      Result := 0
    else
    begin
       vShift := Floor(vDelta / VSize) * VSize;
       Result :=  VPat + vShift;
    end;
  end;

  function GetPatMatrix(const ADest: TFRect; ASource: TFRect): TFMatrix;
  var
    vSWidth, vSHeight, vDWidth, vDHeight: Double;
  begin
    Result := cnstIdentityMat;
    if ASource.Left > ASource.Right then
      SwapSGFloats(ASource.Left, ASource.Right);
    if ASource.Top < ASource.Bottom then
      SwapSGFloats(ASource.Top, ASource.Bottom);

    vSWidth := ASource.Right - ASource.Left;
    vSHeight := ASource.Bottom - ASource.Top;
    vDWidth := ADest.Right - ADest.Left;
    vDHeight := ADest.Bottom - ADest.Top;

    if vSWidth = 0 then
      vSWidth := 1;
    if vSHeight = 0 then
      vSHeight := 1;

    if vDWidth = 0 then
      vDWidth := vSWidth;
    if vDHeight = 0 then
      vDHeight := vSHeight;

    Result.M[0, 0] := vDWidth / vSWidth;
    Result.M[1, 1] := vDHeight / vSHeight;
  end;

  procedure DrawPattern;
  var
    vPatSize, vPatStart: TFPoint;
    X, Y: Integer;
    K: TPoint;
    vPatMatrix, vMatrix: TFMatrix;
    vSaved: TsgCADIterate;
    vMat: TFMatrix;
    vBoxImg: TFRect;
    vIsDraw: Boolean;
{$IFDEF SG_USEGDIPLUS}
    vPatScreenSize: TPoint;
    vPatBmp: TBitmap;
    vPatPts: array[0..3] of TFPoint;
    vPts: array[0..3] of TPoint;
{$ENDIF}
  begin
    vIsDraw := False;
    vBoxImg := vCadFill.Box;
    vPatStart := vPattern.Point1;
    vPatSize := vPattern.Size;
    if vPattern.Units = sfObjectBoundingBox then
    begin
      vMat := StdMat(MakeFPoint((vBoxImg.Right - vBoxImg.Left) / 100, (vBoxImg.Top - vBoxImg.Bottom) / 100, 1), cnstFPointZero);
      vPatStart := FPointXMat(vPatStart, vMat);
      vPatSize := FPointXMat(vPatSize, vMat);
    end;
    vPatStart.X := GetStart(vBoxImg.Left, vPatStart.X, vPatSize.X);
    vPatStart.Y := GetStart(vBoxImg.Top, vPatStart.Y, vPatSize.Y);
    vSaved := FDraw;
    try
      ExpandFRect(vBoxImg, vPatStart);
      K.X := Ceil((vBoxImg.Right - vBoxImg.Left) / vPatSize.X);
      K.Y := Ceil((vBoxImg.Top - vBoxImg.Bottom) / vPatSize.Y);
{$IFDEF SG_USEGDIPLUS}
      if FLibraryGDIPlusExists and (not FExpProcs.Active) and vCadFill.UseBuffer then
      begin
        FillChar(vPatPts, SizeOf(vPatPts), 0);
        vPatPts[1].X := vPatSize.X;
        vPatPts[2].Y := vPatSize.Y;
        vPatPts[3] := vPatSize;
        vPts[0] := GetPoint(vPatPts[0]);
        vPts[1] := GetPoint(vPatPts[1]);
        vPts[2] := GetPoint(vPatPts[2]);
        vPatScreenSize.X := Ceil(DistancePoint(vPts[0], vPts[1]));
        vPatScreenSize.Y := Ceil(DistancePoint(vPts[0], vPts[2]));
        vPatBmp := vCadFill.CreateBMPPattern(vPatScreenSize.X, vPatScreenSize.Y, ClassType);
        try
          vPatScreenSize := Point(vPatBmp.Width, vPatBmp.Height);
          graphics := FContext.CreateGDIPGraphic;
          try
            SetGPGraphicsParams(graphics, cnstImageQuality);
            vBmp := TGPBitmap.Create(vPatBmp.Handle, vPatBmp.Palette);
            try
              vAttr := TGPImageAttributes.Create;
              try
                vAttr.SetColorKey(aclWhite, aclWhite, ColorAdjustTypeBitmap);
                vMatrix := cnstIdentityMat;
                vMatrix.M[3, 1] := vPatStart.Y;
                Y := 0;
                while Y <= K.Y do
                begin
                  vMatrix.M[3, 0] := vPatStart.X;
                  X := 0;
                  while X <= K.X do
                  begin
                    FDraw.Matrix := FMatXMat(vMatrix, vSaved.Matrix);
                    vPts[0] := GetPoint(vPatPts[0]);
                    vPts[1] := GetPoint(vPatPts[1]);
                    vPts[2] := GetPoint(vPatPts[2]);
                    vPts[3] := GetPoint(vPatPts[3]);
                    graphics.DrawImage(vBmp, PGPPoint(@vPts[0]), 3, 0, 0,
                      vPatScreenSize.X, vPatScreenSize.Y, {$IFDEF SG_WINAPI_GDIPLUS}UnitPixel, {$ENDIF}vAttr);
                    vMatrix.M[3, 0] := vMatrix.M[3, 0] + vPatSize.X;
                    Inc(X);
                  end;
                  vMatrix.M[3, 1] := vMatrix.M[3, 1] - vPatSize.Y;
                  Inc(Y);
                end;
                vIsDraw := True;
              finally
                vAttr.Free;
              end;
            finally
              vBMP.Free;
            end;
          finally
            graphics.Free;
          end;
        finally
          vPatBmp.Free;
        end;
      end;
{$ENDIF}
      if not vIsDraw then
      begin
        FillChar(vBoxImg, Sizeof(vBoxImg), 0);
        ExpandFRect(vBoxImg, vPatSize);
        vPatMatrix := GetPatMatrix(vBoxImg, vPattern.ContainerBox);
        vMatrix := cnstIdentityMat;
        vMatrix.M[3, 1] := vPatStart.Y;
        Y := 0;
        while Y <= K.Y do
        begin
          vMatrix.M[3, 0] := vPatStart.X;
          X := 0;
          while X <= K.X do
          begin
            FDraw.Matrix := FMatXMat(vMatrix, vSaved.Matrix);
            FDraw.Matrix := FMatXMat(vPatMatrix, FDraw.Matrix);
            vPattern.Iterate(Converter, DrawEntity, DoFinish);
            vMatrix.M[3, 0] := vMatrix.M[3, 0] + vPatSize.X;
            Inc(X);
          end;
          vMatrix.M[3, 1] := vMatrix.M[3, 1] - vPatSize.Y;
          Inc(Y);
        end;
      end;
    finally
      FDraw := vSaved;
    end;
  end;

  procedure DrawBMPPattern(const AImg: TBitmap);
  begin
{$IFDEF SG_USEGDIPLUS}
    if Converter.IsCrossoverMatrix then
    begin
      vPts[0] := TGPPoint(GetPoint(vCadFill.Box.TopLeft));
      vPts[1] := TGPPoint(GetPoint(MakeFPoint(vCadFill.Box.Right, vCadFill.Box.Top, 0)));
      vPts[2] := TGPPoint(GetPoint(MakeFPoint(vCadFill.Box.Left, vCadFill.Box.Bottom, 0)));
    end
    else
    begin
      vPts[0] := TGPPoint(GetPoint(MakeFPoint(vCadFill.Box.Left, vCadFill.Box.Bottom, 0)));
      vPts[1] := TGPPoint(GetPoint(MakeFPoint(vCadFill.Box.Right, vCadFill.Box.Bottom, 0)));
      vPts[2] := TGPPoint(GetPoint(vCadFill.Box.TopLeft));
    end;
    graphics := FContext.CreateGDIPGraphic;
    try
      SetGPGraphicsParams(graphics, cnstImageQuality);
      vBmp := TGPBitmap.Create(AImg.Handle, AImg.Palette);
      try
        vAttr := TGPImageAttributes.Create;
        try
          vAttr.SetColorKey(aclWhite, aclWhite, ColorAdjustTypeBitmap);
          graphics.DrawImage(vBmp, PGPPoint(@vPts[0]), 3, 0, 0,
            AImg.Width, AImg.Height, {$IFDEF SG_WINAPI_GDIPLUS}UnitPixel, {$ENDIF}vAttr);
        finally
          vAttr.Free;
        end;
      finally
        vBMP.Free;
      end;
    finally
      graphics.Free;
    end;
{$ENDIF}
  end;

begin
//this DrawHatch(Sender)
  vScreenRect := GetScreenRect(vCadFill.Box);
  vStyle := vCadFill.StyleFill;
  if vStyle is TsgCADStyleGradient then
  begin
    if FExpProcs.Active then
    begin
      vColors := TsgIntegerList.Create;
      vPositions := TsgSingleList.Create;
      try
        vGradient.GetPositionColors(vPositions, vColors);
        if vColors.Count > 0 then
        begin
          FColor := vColors.First;
          DoSetColor(FColor, ctBrush, 3);
          if FContext.Poly.Count > 0 then
          begin
            if FContext.Counts.Count = 1 then
              ExpPolygonInteger(Sender, FContext.Poly)
            else
              ExpPolyPolygonInteger(nil, PPoint(FContext.Poly.List), PInteger(FContext.Counts.List), FContext.Counts.Count);
          end;
        end;
      finally
        vPositions.Free;
        vColors.Free;
      end;
    end
    else
    begin
      {$IFDEF SG_USEGDIPLUS}
      if FLibraryGDIPlusExists then
      begin
        vBoxNew := RectF(cnstBad2DRect.Left, cnstBad2DRect.Top, cnstBad2DRect.Right, cnstBad2DRect.Bottom);
        vColors := TsgIntegerList.Create;
        vPositions := TsgSingleList.Create;
        try
          vGradient.GetPositionColors(vPositions, vColors);
          for I := 0 to vColors.Count - 1 do
            vColors.List[I] := GetGDIPlusColor(vColors.List[I]);
          if vColors.Count > 0 then
          begin
            graphics := FContext.CreateGDIPGraphic;
            try
              SetGPGraphicsParams(graphics, cnstImageQuality);
              if vGradient.Linear then
                DrawLinearGradient
              else
                DrawLinearCircular;
            finally
              graphics.Free;
            end;
          end;
        finally
          vPositions.Free;
          vColors.Free;
        end;
      end
      else
        {$ENDIF}
        DrawGDIGradient;
    end;
  end
  else
  begin
{$IFDEF SG_USEGDIPLUS}
    if FLibraryGDIPlusExists and (not FExpProcs.Active) and IsCADFillUsedBuffer(vCadFill) then
    begin
      if vCadFill.CreateBMPFill(FDraw.Matrix, Self.ClassType) then
        DrawBMPPattern(vCadFill.BMPImage)
    end
    else
{$ENDIF}
      DrawPattern;
  end;
end;

procedure TsgCADImage.DrawCircle(Sender: TObject);
var
  C: TsgDXFCircle absolute Sender;
  R: TRect;

  function CBounds: TRect;
  var
    OffsetPt: TPoint;
  begin
    Result.TopLeft := GetPoint(C.Point);
    Result.BottomRight := Result.TopLeft;
    DoScale2D(FDraw);
    OffsetPt.X := Abs(Round(C.Radius * FDraw.XScale {* Scale.X}));
    OffsetPt.Y := Abs(Round(C.Radius * FDraw.YScale {* Scale.Y}));
    InflateRect(Result, OffsetPt.X, OffsetPt.Y);
  end;
begin
  if FUseWinEllipse and (not Converter.Is3D)
    and FEntityLines.IsSolid and (Converter.VPort.X = 0)
      and (Converter.VPort.Y = 0) and (Converter.VPort.Z > 0) and (not IsDrawingRotated) then
  begin
    R := CBounds;
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.BrushStyle := bsClear;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
    end;
    if IsDrawOnCanvas then
    begin
      FContext.BrushStyle := bsClear;
      FContext.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end
  else
    DrawPoly(Sender);
end;

procedure TsgCADImage.DrawFace(Sender: TObject);
  procedure Edge(Lines: TsgLines; const F1, F2: TFPoint);
  var
    AList: TFPointList;
  begin
    if Lines.IsSolid then
    begin
      FContext.Poly.Count := 0;
      FContext.AddPolySegment(F1, F2);
      ExpPolylineInteger(Sender, FContext.Poly);
    end
    else
    begin
      AList := TFPointList.Create;
      try
        Lines.Line(F1, F2, AList);
        DrawPointsListByPolyPolyline(Sender, AList);
      finally
        AList.Free;
      end;
    end;
  end;
var
  F: TsgDXF3dFace absolute Sender;
begin
  if F.Flags and 1 = 0 then
    Edge(FEntityLines, F.Point, F.Point1);
  if F.Flags and 2 = 0 then
    Edge(FEntityLines, F.Point1, F.Point2);
  if F.Flags and 4 = 0 then
    Edge(FEntityLines, F.Point2, F.Point3);
  if F.Flags and 8 = 0 then
    Edge(FEntityLines,F.Point3, F.Point);
end;

{$IFDEF SG_USEGDIPLUS}
function MakeRectF(x, y, width, height: Single): TGPRectF;
begin
  Result.X      := x;
  Result.Y      := y;
  Result.Width  := width;
  Result.Height := height;
end;
{$ENDIF}

procedure TsgCADImage.DrawHatch(Sender: TObject);
{$IFDEF SG_FIREMONKEY}
const
  cnstUseFMXClipRegion =
  {$IFDEF SG_FM_WINDOWS}
     True
  {$ELSE}
    {$IFDEF SG_FM_ANDOID}
     True
    {$ELSE}
     False
    {$ENDIF}
  {$ENDIF};
{$ENDIF}

var
  vPolygon: TsgCADPolyPolygon;
  vMainRegion: TsgRegion;
  I, SaveIndex: Integer;
  vRect: TRect;
  vIsDrawOnCanvas, vIsDrawOnBitMap: Boolean;

  function GetMainRegion: TsgRegion;
  var
    J: Integer;
    vRegion: TsgRegion;
    P: PPoint;
  begin
    if vMainRegion = nil then
    begin
      vMainRegion := TsgRegion.Create;
      J := 0;
      if J < FContext.Counts.Count then
      begin
        P := PPoint(FContext.Poly.List);
        vMainRegion.SetPolygon(P^, FContext.Counts[J], GetPolygonFillingMode);
        Inc(P, FContext.Counts[J]);
        Inc(J);
        while J < FContext.Counts.Count do
        begin
          vRegion := TsgRegion.Create;
          try
            vRegion.SetPolygon(P^, FContext.Counts[J], GetPolygonFillingMode);
            vMainRegion.Exclude(vRegion);
          finally
            vRegion.Free;
          end;
          Inc(PByte(P), FContext.Counts[J] * SizeOf(TPoint));
          Inc(J);
        end;
      end;
    end;
    Result := vMainRegion;
  end;

  procedure SetClipRGN;
  begin
    if SaveIndex = -1 then
      SaveIndex := ExpSaveDCInternal(Sender);
    vRect := GetMainRegion.ClipRect;
    FContext.RegionOffset(GetMainRegion);
    if FContext.RegionUpdate(TsgRegion(FCanvasRegion)) > 0 then
      GetMainRegion.Intersect(FCanvasRegion);
    FContext.RegionSelect(GetMainRegion);
  end;

{$IFDEF SG_USEGDIPLUS}
  (*
  procedure DrawGDIPlusGradientEx(const DC: HDC; const ABox: TRect; const AGrad: TsgDXFGradient);
  var
    graphics: TsgGDIPlusGraphics;
    GradP: TsgGDIPlusPathGradientBrush;
    GradL: TsgGDIPlusLinearGradientBrush;
    GBrushSolid: TsgGDIPlusSolidBrush;
    GPath: TsgGDIPlusGraphicsPath;
    GP1, GP2: TsgGPoint;
    GPen: TsgGDIPlusPen;
    GPRect, GEllipse: TsgGRect;
    vPC: TFPoint;
    I, J: Integer;
    vColors, vPositions: TList;
    vPenWidth: Double;
    vBoxNew: TRect;

    procedure SetGP12Rel(const AP1, AP2: TFPoint);
    var
      vKX, vKY: Double;
    begin
      vKX := GPRect.Width / 100;
      vKY := GPRect.Height / 100;
      GP1.X := Round(vKX * AP1.X + ABox.Left);
      GP1.Y := Round(vKY * AP1.Y + ABox.Top);
      GP2.X := Round(vKX * AP2.X + ABox.Left);
      GP2.Y := Round(vKY * AP2.Y + ABox.Top);
    end;

  begin
    GPRect := MakeRect(ABox.Left, ABox.Top, ABox.Right - ABox.Left, ABox.Bottom - ABox.Top);
    vBoxNew := cnstBad2DRect;
    vColors := TList.Create;
    vPositions := TList.Create;
    try
      vColors.Count := AGrad.Colors.Count;
      vPositions.Count := vColors.Count;
      for I := 0 to vColors.Count - 1 do
      begin
        if AGrad.IsSpheric then
          J := vColors.Count - 1 - I
        else
          J := I;
        vColors.List^[J] := Pointer(GetGDIPlusColor(TColor(AGrad.Colors.List^[I])));
        Single(vPositions.List^[I]) := Single(AGrad.Positions.List^[I]) * 0.01;
      end;
      graphics := TsgGDIPlusGraphics.Create(DC);
      try
        if AGrad.IsSpheric then
        begin
          if AGrad.AbsParams then
          begin
            vPC := AGrad.PointC;
            GP1 := TsgGPoint(GetPoint(FPointXMat(vPC, AGrad.Transform)));
            GP2 := TsgGPoint(GetPoint(FPointXMat(AGrad.PointF, AGrad.Transform)));
            SetNewBox(GetPoint(FPointXMat(MakeFPoint(vPC.X - AGrad.Radius, vPC.Y, 0), AGrad.Transform)),
                      GetPoint(FPointXMat(MakeFPoint(vPC.X, vPC.Y - AGrad.Radius, 0), AGrad.Transform)),
                      GetPoint(FPointXMat(MakeFPoint(vPC.X + AGrad.Radius, vPC.Y, 0), AGrad.Transform)),
                      GetPoint(FPointXMat(MakeFPoint(vPC.X, vPC.Y + AGrad.Radius, 0), AGrad.Transform)), vBoxNew);
          end
          else
          begin
            SetGP12Rel(AGrad.PointC, AGrad.PointF);
            I := Ceil(AGrad.Radius * GPRect.Width / 100);
            if GPRect.Width <> GPRect.Height then
              J := Ceil(AGrad.Radius * GPRect.Height / 100)
            else
              J := I;
            SetNewBox(Point(GP1.X - I, GP1.Y), Point(GP1.X, GP1.Y - J),
                      Point(GP1.X + I, GP1.Y), Point(GP1.X, GP1.Y + J), vBoxNew)
          end;
          GEllipse := MakeRect(vBoxNew.Left, vBoxNew.Top, vBoxNew.Right - vBoxNew.Left, vBoxNew.Bottom - vBoxNew.Top);
          GBrushSolid := TsgGDIPlusSolidBrush.Create(Cardinal(vColors.First));
          try
            graphics.FillRectangle(GBrushSolid, GPRect);
          finally
            GBrushSolid.Free;
          end;
          GPath := TsgGDIPlusGraphicsPath.Create;
          try
            GPath.StartFigure;
            GPath.AddEllipse(GEllipse);
            GPath.SetFillMode(FillModeWinding);
            GradP := TsgGDIPlusPathGradientBrush.Create(GPath);
            try
              GradP.SetCenterPoint(GP2);
              GradP.SetInterpolationColors(@vColors.List^[0], @vPositions.List^[0], vColors.Count);
              graphics.FillPath(GradP, GPath);
            finally
              GradP.Free;
            end;
          finally
            GPath.Free;
          end;
        end
        else
        begin
          if AGrad.AbsParams then
          begin
            GP1 := TsgGPoint(GetPoint(FPointXMat(AGrad.Point1, AGrad.Transform)));
            GP2 := TsgGPoint(GetPoint(FPointXMat(AGrad.Point2, AGrad.Transform)));
          end
          else
            SetGP12Rel(AGrad.Point1, AGrad.Point2);
          if IsEqualPoints(TPoint(GP1), TPoint(GP2)) then//REC-SVG11-20030114/pservers.html#LinearGradients
          begin
            GBrushSolid := TsgGDIPlusSolidBrush.Create(Cardinal(vColors.First));
            try
              graphics.FillRectangle(GBrushSolid, GPRect);
            finally
              GBrushSolid.Free;
            end;
          end
          else
          begin
            vBoxNew := ABox;
            ExpandRect(vBoxNew, TPoint(GP1));
            ExpandRect(vBoxNew, TPoint(GP2));
            vPenWidth := Sqr(vBoxNew.Bottom - vBoxNew.Top) + Sqr(vBoxNew.Right - vBoxNew.Left);
            if vPenWidth > fDoubleResolution then
              vPenWidth := Sqrt(vPenWidth) * 2
            else
              vPenWidth := 1;
            GradL := TsgGDIPlusLinearGradientBrush.Create(GP1, GP2, TsgColor(vColors.First), TsgColor(vColors.Last));
            try
              GradL.SetInterpolationColors(@vColors.List^[0], @vPositions.List^[0], vColors.Count);
  //            graphics.FillRectangle(GradL, GPRect);
              GPen := TsgGDIPlusPen.Create(GradL, vPenWidth);
              try
                graphics.DrawLine(GPen, GP1, GP2);
              finally
                GPen.Free;
              end;
              GPen := TsgGDIPlusPen.Create(TsgColor(vColors.First), vPenWidth);
              try
                graphics.DrawLine(GPen, TsgGPoint(GetPointOnLineI(TPoint(GP1), TPoint(GP2), 2)), TsgGPoint(GetPointOnLineI(TPoint(GP1), TPoint(GP2), -vPenWidth)));
              finally
                GPen.Free;
              end;
              GPen := TsgGDIPlusPen.Create(TsgColor(vColors.Last), vPenWidth);
              try
                graphics.DrawLine(GPen, TsgGPoint(GetPointOnLineI(TPoint(GP2), TPoint(GP1), 2)), TsgGPoint(GetPointOnLineI(TPoint(GP1), TPoint(GP2), vPenWidth)));
              finally
                GPen.Free;
              end;
            finally
              GradL.Free;
            end;
          end;
        end;
      finally
        graphics.Free;
      end;
    finally
      vPositions.Free;
      vColors.Free;
    end;
  end;
  *)
  procedure DrawGDIPlusHatch(AColor: TColor);
  var
    graphics: TGPGraphics;
    Solid: TGPSolidBrush;
    GPRect: TGPRect;
    vColor: TColor;
    vRGBA: array[0..3] of Byte absolute vColor;

    procedure ApplyTransparency(const ATransparency: Single);
    var
      vTransparency: Integer;
    begin
      vTransparency := Ceil(255 * ATransparency);
      if vTransparency < 0 then
        vTransparency := 0;
      if vTransparency > 255 then
        vTransparency := 255;
      vRGBA[3] := vTransparency;
    end;

  begin
    SetClipRGN;
    GPRect := MakeRect(vRect.Left, vRect.Top, vRect.Right - vRect.Left, vRect.Bottom - vRect.Top);
    graphics := FContext.CreateGDIPGraphic;
    try
      SetGPGraphicsParams(graphics, cnstImageQuality);
      case AColor of
        clByBlock, clByLayer, clNone: vColor := GetDrawPenColor;
      else
        vColor := AColor;
      end;
      if (FXrefTransparency >= 0) and Assigned(FXrefStack) and (FXrefStack.Count > 0) then
        ApplyTransparency(FXrefTransparency)
      else
      begin
        if vPolygon.EntType = ceCurvePolygon then
        begin
          if TsgCADCurvePolygon(vPolygon).Transparency > 0 then
            ApplyTransparency(TsgCADCurvePolygon(vPolygon).Transparency / 100);
        end;
      end;
      Solid := TGPSolidBrush.Create(GetGDIPlusColor(vColor));
      try
        graphics.FillRectangle(Solid, GPRect);
      finally
        Solid.Free;
      end;
    finally
      graphics.Free;
    end;
  end;

  function GetRectMaxSizeF(const ARect: TGPRectF): TGPRectF;
  begin
    if ARect.Width < ARect.Height then
      Result.Width := ARect.Height
    else
      Result.Width := ARect.Width;
    Result.Height := ARect.Width;
    Result.X := ARect.X - (Result.Width - ARect.Width) / 2;
    Result.Y := ARect.Y - (Result.Height - ARect.Height) / 2;
  end;

  function GetRectRotated(const ARect: TRect; const Angle: Double): TRect;
  var
    vC: TPoint;
  begin
    Result := ARect;
    vC.X := (ARect.Left + ARect.Right) div 2;
    vC.Y := (ARect.Top + ARect.Bottom) div 2;
    ExpandRect(Result, GetPointOfRotateByCenter(Point(ARect.Left, ARect.Top), vC, Angle));
    ExpandRect(Result, GetPointOfRotateByCenter(Point(ARect.Right, ARect.Top), vC, Angle));
    ExpandRect(Result, GetPointOfRotateByCenter(Point(ARect.Right, ARect.Bottom), vC, Angle));
    ExpandRect(Result, GetPointOfRotateByCenter(Point(ARect.Left, ARect.Bottom), vC, Angle));
  end;

  procedure DrawGDIPlusCADGradient(const ACADGradinet: TsgCADGradientPolygonAccess);
  const
    cnstCounts: Integer = 3;
    cnstBlendFactorsByCylinder: array [0..2] of Single = (0, 1, 0);
    cnstBlendPositionsByCylinder: array [0..2] of Single = (0, 0.5, 1);
    cnstBlendFactorsByCylinderNotCenter: array [0..2] of Single = (0, 1, 0);
    cnstBlendPositionsByCylinderNotCenter: array [0..2] of Single = (0, 0.2, 1);
    cnstBlendFactorsByLiner: array [0..2] of Single = (0, 0.5, 1);
    cnstBlendPositionsByLinear: array [0..2] of Single = (0, 0.2, 1);
  var
    vColors: array [0..1] of TsgColor;
    graphics: TGPGraphics;
    GRect: TGPRectF;
    GCenter: TGPPointF;
    GR: TGPRectF;
    GMatrix: TGPMatrix;
    GGradL: TGPLinearGradientBrush;

    procedure DrawSolid(const AColor: TsgColor);
    var
      GSolid: TGPSolidBrush;
    begin
      GSolid := TGPSolidBrush.Create(Cardinal(AColor));
      try
        graphics.FillRectangle(GSolid, GRect);
      finally
        GSolid.Free;
      end;
    end;

    procedure DrawSpheric(const ARect: TGPRectF;
      const ACenter: TGPPointF);
    var
      GPath: TGPGraphicsPath;
      GGrad: TGPPathGradientBrush;
      vCount: Integer;
    begin
      GPath := TGPGraphicsPath.Create;
      try
        GPath.StartFigure;
        GPath.AddEllipse(ARect);
        GGrad := TGPPathGradientBrush.Create(GPath);
        try
          vCount := 1;
          //GGrad.MultiplyTransform(GMatrix);
          GGrad.SetCenterColor(Cardinal(vColors[1]));
          GGrad.SetCenterPoint(ACenter);
          GGrad.SetSurroundColors(@vColors[0], vCount);
          graphics.FillPath(GGrad, GPath);
        finally
          GGrad.Free;
        end;
      finally
        GPath.Free;
      end;
    end;

    function GetCenterF(const ARect: TGPRect):
      TGPPointF; overload;
    begin
      Result.X := ARect.X + ARect.Width / 2;
      Result.Y := ARect.Y + ARect.Height / 2;
    end;

    function GetCenterF(const ARect: TGPRectF):
      TGPPointF; overload;
    begin
      Result.X := ARect.X + ARect.Width / 2;
      Result.Y := ARect.Y + ARect.Height / 2;
    end;

  begin
    Cardinal(vColors[0]) := GetGDIPlusColor(ACADGradinet.GradientColor[0]);
    Cardinal(vColors[1]) := GetGDIPlusColor(ACADGradinet.GradientColor[1]);
    if ACADGradinet.FType in [gtInvCurved, gtInvCylinder, gtInvHemiSpherical, gtInvSpherical] then
      SwapInts(Cardinal(vColors[0]), Cardinal(vColors[1]));
    vRect := GetRectRotated(vRect, 360 - ACADGradinet.GradientAngle);
    GRect := MakeRectF(vRect.Left, vRect.Top, vRect.Right - vRect.Left, vRect.Bottom - vRect.Top);
    graphics := FContext.CreateGDIPGraphic;
    try
      SetGPGraphicsParams(graphics, cnstImageQuality);
      case ACADGradinet.FType of
        gtLinear:
          begin
            GMatrix := TGPMatrix.Create(1, 0, 0, 1, 0, 0);
            try
              if Abs(ACADGradinet.GradientAngle) > fAccuracy then
                GMatrix.RotateAt(360 - ACADGradinet.GradientAngle, GetCenterF(GRect));
              GGradL := TGPLinearGradientBrush.Create(GRect, Cardinal(vColors[0]), Cardinal(vColors[1]), LinearGradientModeHorizontal);
              try
                GGradL.MultiplyTransform(GMatrix);
                if not ACADGradinet.GradientUseCenter then
                  GGradL.SetBlend(@cnstBlendFactorsByLiner, @cnstBlendPositionsByLinear, cnstCounts);
                graphics.FillRectangle(GGradL, GRect);
              finally
                GGradL.Free;
              end;
            finally
              GMatrix.Free;
            end;
          end;
        gtCylinder, gtInvCylinder:
          begin
            GMatrix := TGPMatrix.Create(1, 0, 0, 1, 0, 0);
            try
              if Abs(ACADGradinet.GradientAngle) > fAccuracy then
                GMatrix.RotateAt(360 - ACADGradinet.GradientAngle, GetCenterF(GRect));
              GGradL := TGPLinearGradientBrush.Create(GRect, Cardinal(vColors[0]), Cardinal(vColors[1]), LinearGradientModeHorizontal);
              try
                GGradL.MultiplyTransform(GMatrix);
                if ACADGradinet.GradientUseCenter then
                  GGradL.SetBlend(@cnstBlendFactorsByCylinder, @cnstBlendPositionsByCylinder, cnstCounts)
                else
                  GGradL.SetBlend(@cnstBlendFactorsByCylinderNotCenter, @cnstBlendPositionsByCylinderNotCenter, cnstCounts);
                graphics.FillRectangle(GGradL, GRect);
              finally
                GGradL.Free;
              end;
            finally
              GMatrix.Free;
            end;
          end;
        gtSpherical, gtInvSpherical:
          begin
            DrawSolid(vColors[0]);
            GR := GetRectMaxSizeF(GRect);
            if ACADGradinet.GradientUseCenter then
              GCenter := GetCenterF(GR)
            else
            begin
              GCenter.X := GR.X + 0.2 * GR.Width;
              GCenter.Y := GR.Y + 0.2 * GR.Height;
              GR.X := GCenter.X - GR.Width / 2;
              GR.Y := GCenter.Y - GR.Height / 2;
            end;
            DrawSpheric(GR, GCenter);
          end;
        gtHemiSpherical, gtInvHemiSpherical:
          begin
            DrawSolid(vColors[0]);
            if ACADGradinet.GradientUseCenter then
            begin
              GCenter.X := GRect.X + GRect.Width / 2;
              GCenter.Y := GRect.Y + GRect.Height;
            end
            else
            begin
              GCenter.X := GRect.X;
              GCenter.Y := GRect.Y + GRect.Height;
            end;
            if GRect.Width > GRect.Height then
              GR.Width := GRect.Width * 2
            else
              GR.Width := GRect.Height * 2;
            GR.Height := GR.Width;
            GR.X := GCenter.X - GR.Width / 2;
            GR.Y := GCenter.Y - GR.Height / 2;
            DrawSpheric(GR, GCenter);
          end;
        gtCurved, gtInvCurved:
          begin
            //GGradL := TsgGDIPlusLinearGradientBrush.Create(GRect, vColors[0], vColors[1], LinearGradientModeVertical);
            GGradL := TGPLinearGradientBrush.Create(GRect, Cardinal(vColors[0]), Cardinal(vColors[1]), ACADGradinet.GradientAngle+90, False);
            try
              graphics.FillRectangle(GGradL, GRect);
            finally
              GGradL.Free;
            end;
          end;
      end;
    finally
      graphics.Free;
    end;
  end;


{$ENDIF}

  procedure DrawGradient;

    procedure GetCrossLineByAngle(ARect: TFRect; AAngle: Double; var ACross1, ACross2: TFPoint);
    var
      vLine, vRotLine: TsgLine;
      vMiddle: TFPoint;
    begin
      vLine := MakeLine(MakeFPoint(0, 0.5), MakeFPoint(1, 0.5));
      vMiddle := MiddleFPoint(vLine.Point1, vLine.Point2);
      vRotLine.Point1 := RotateAroundFPoint(vLine.Point1, vMiddle, False,
        DegToRad(AAngle + 180));
      vRotLine.Point2 := RotateAroundFPoint(vLine.Point2, vMiddle, False,
        DegToRad(AAngle + 180));
      IsCrossLineAndBox(vRotLine.Point1, vRotLine.Point2, ARect, ACross1, ACross2);
    end;

  var
    vGRPolygon: TsgCADGradientPolygonAccess absolute Sender;
  {$IFDEF SG_FIREMONKEY}
    vStyle: Byte;
    vCol, vCol1: Integer;
    vFRect: TFRect;
    vPosRect: TRectF;
    vCross1, vCross2: TFPoint;
  {$ENDIF}
  begin
    //if Sender.ClassType <> TsgDXFGradient then
    begin
{$IFDEF SG_USEGDIPLUS}
      if FLibraryGDIPlusExists then
        DrawGDIPlusCADGradient(TsgCADGradientPolygonAccess(vPolygon))
      else
{$ENDIF}
      begin
      {$IFDEF SG_FIREMONKEY}
        vCol := vGRPolygon.GradientColor[0];
        vCol1 := vGRPolygon.GradientColor[1];
        if vGRPolygon.FType in [gtInvCurved, gtInvCylinder, gtInvHemiSpherical, gtInvSpherical] then
          SwapInts(vCol, vCol1);
        vPosRect := TRectF.Create(GetMainRegion.ClipRect);
        vStyle := 0;
        case vGRPolygon.FType of
          gtLinear:
            begin
              vStyle := 0;
              vFRect := MakeFRect2D(0, 0, 1, 1);
              GetCrossLineByAngle(vFRect, vGRPolygon.GradientAngle, vCross1, vCross2);
              vFRect := MakeFRectByPoints(vCross2, vCross1);
            end;
          gtSpherical, gtInvSpherical:
            begin
              vStyle := 1;
              if vGRPolygon.GradientUseCenter then
                FCanvas.Fill.Gradient.RadialTransform.Position.Point := TPoint.Zero
              else
              begin
                FCanvas.Fill.Gradient.RadialTransform.Position.X := - 0.3 * vPosRect.Width;
                FCanvas.Fill.Gradient.RadialTransform.Position.Y := - 0.3 * vPosRect.Height;
              end;
            end;
          gtCylinder, gtInvCylinder:
            begin
              vStyle := 0;
              vFRect := MakeFRect2D(0, 0, 0.5, 0.5);
              GetCrossLineByAngle(vFRect, vGRPolygon.GradientAngle, vCross1, vCross2);
              vFRect := MakeFRectByPoints(vCross2, vCross1);
            end;
          gtHemiSpherical, gtInvHemiSpherical:
            begin
              vStyle := 1;
              if vGRPolygon.GradientUseCenter then
              begin
                FCanvas.Fill.Gradient.RadialTransform.Position.X := 0;
                FCanvas.Fill.Gradient.RadialTransform.Position.Y := 0.5 * vPosRect.Height;
              end
              else
              begin
                FCanvas.Fill.Gradient.RadialTransform.Position.X := -0.5 * vPosRect.Width;
                FCanvas.Fill.Gradient.RadialTransform.Position.Y := 0.5 * vPosRect.Height;
              end;
            end;
          gtCurved, gtInvCurved:
            begin
              vStyle := 0;
              vFRect := MakeFRect2D(0, 1, 1, 0);
              GetCrossLineByAngle(vFRect, vGRPolygon.GradientAngle + 90, vCross1, vCross2);
              vFRect := MakeFRectByPoints(vCross2, vCross1);
            end;
        end;
        FContext.GradientFill(vRect, vCol, vCol1, FContext.Poly, @vFRect, vStyle, FContext.Counts);
      {$ELSE}
        case DrawMode of
          dmGray: FContext.GradientFill(vRect, ConvertColortoGray(vGRPolygon.GradientColor[0]),
            ConvertColortoGray(vGRPolygon.GradientColor[1]), FContext.Poly);
          dmBlack:
            begin
              FContext.BrushColor:=DefaultColor;
              FContext.DoPolygonList(FContext.Poly);
            end
        else
          FContext.GradientFill(vRect, vGRPolygon.GradientColor[0], vGRPolygon.GradientColor[1], FContext.Poly);
        end;
      {$ENDIF}
      end;
    end
    (*
    else
    begin

      vGradient := TsgDXFGradient(Sender);
      vR := vGradient.Box;
      TransRectCorners(vR, FDraw.Matrix);
      vRectEnt.TopLeft := Point(Round(vR.TopLeft.X), Round(vR.TopLeft.Y));
      vRectEnt.BottomRight := Point(Round(vR.BottomRight.X), Round(vR.BottomRight.Y));
      SwapInts(vRectEnt.TopLeft.Y, vRectEnt.BottomRight.Y);
{$IFDEF SG_USEGDIPLUS}
      if FLibraryGDIPlusExists then
        DrawGDIPlusGradientEx(FCanvas.Handle, vRectEnt, vGradient)
      else
{$ENDIF}
      begin
        if vGradient.IsVertical then
          Per := (vRectEnt.Bottom - vRectEnt.Top) / 100
        else
          Per := (vRectEnt.Right - vRectEnt.Left) / 100;
        for L := 0 to vGradient.Colors.Count - 2 do
        begin
          if vGradient.IsVertical then
          begin
            vP1.Y := vRectEnt.TopLeft.Y + Round(Single(vGradient.Positions[L]) * Per);
            vP2.Y := vRectEnt.TopLeft.Y + Round(Single(vGradient.Positions[L + 1]) * Per);
          end
          else
          begin
            vP1.X := vRectEnt.TopLeft.X + Round(Single(vGradient.Positions[L]) * Per);
            vP2.X := vRectEnt.TopLeft.X + Round(Single(vGradient.Positions[L + 1]) * Per);
          end;
          SetVert(Vert[0], vP1, TColor(vGradient.Colors[L]));
          SetVert(Vert[1], vP2, TColor(vGradient.Colors[L + 1]));
          GradientFillProc(FCanvas.Handle, @Vert, 2, @P1, 1, Integer(vGradient.IsVertical));
        end;
      end;

    end;  *)
  end;

  procedure DoDrawOnBitmapAsFillRgn;
  begin
    AddSelectionMatrix(Sender);
    FContextSelectionMatrix.BrushStyle := bsSolid;
    FContextSelectionMatrix.PenWidth := cnstPenWidthNull;
    FContextSelectionMatrix.Changed;
    if vPolygon.Boundaries.Count = 1 then
      FContextSelectionMatrix.DoPolygonList(FContext.Poly)
    else
    begin
      //FContextSelectionMatrix.FillRegion(GetMainRegion);
      FContextSelectionMatrix.DoPolyPolygon(FContext.Poly.List^, PInteger(FContext.Counts.List), FContext.Counts.Count);
    end;
    FContextSelectionMatrix.BrushStyle := bsClear;
  end;
var
  vConv: TsgDXFConverter;
  {$IFDEF SG_FIREMONKEY}
  vUseFMXClipRegion: Boolean;
  {$ENDIF}
  vLTScale: Double;
  vDrawDottedParsedLines: Boolean;
begin
  if not TsgDXFConverterAccess(Converter).PHeadVarStruct^.FillMode then Exit;
{$IFDEF SG_BTI}
  if Assigned(FDraw.Insert) and (TsgDXFInsertAccess(FDraw.Insert).GetSelectedIndex > -1) then
    if IsDrawOnBitMap or FUseFillStyleIndex then
      Exit;
{$ENDIF}
  vIsDrawOnCanvas := IsDrawOnCanvas;
  vIsDrawOnBitMap := IsDrawOnBitMap;
  if not (vIsDrawOnCanvas or vIsDrawOnBitMap) then
    Exit;
  vPolygon := TsgCADPolyPolygon(Sender);
  if (vPolygon.EntType = ceHatch) and (TsgCADHatch(vPolygon).FillStyle = fsCAD) then
  begin
    if TsgCADHatch(vPolygon).ParseLineOnDraw then
    begin
      TsgCADHatch(vPolygon).ParseLineOnDraw := False;
      vConv := vPolygon.Converter;
      if not Assigned(vConv) then
        vConv := Converter;
      TsgDXFEntityAccess(vPolygon).Loaded(vConv);
    end;
    if (not TsgCADHatch(vPolygon).DrawAsSolid) and
      ((TsgCADHatch(vPolygon).ParsedLines = nil) or (TsgCADHatch(vPolygon).ParsedLines.Count = 0)) then
      Exit;
  end;
  vMainRegion := nil;
  SaveIndex := -1;
  try
    FContext.Poly.Count := 0;
    FContext.Counts.Count := 0;
    I := 0;
    while (I < vPolygon.Boundaries.Count){$IFDEF SG_THREAD_DRAW} and not FDraw.Stopped{$ENDIF} do
    begin
      FContext.Counts.Add(Load2DPointToPoints(TF2DPointList(vPolygon.Boundaries[I])));
      if not FExpProcs.Active then
        FContext.FixLastPointsCount;//for DoPolyPolygon
      Inc(I);
    end;
{$IFDEF SG_THREAD_DRAW}
    if FDraw.Stopped then Exit;
{$ENDIF}
{$IFDEF SG_DELPHI_VCL}
    if FContext.IsMetafileCanvas then
    begin
      FContext.RegionUpdate(TsgRegion(FCanvasRegion));
      vRect := FContext.ClipRect;
      if (vRect.Right <> vRect.Left) and FCanvasRegion.HandleAllocated then
        GetMainRegion.Intersect(FCanvasRegion);
    end;
{$ENDIF}
    if vPolygon.SolidFill or ((vPolygon.EntType = ceHatch) and
      (TsgCADHatch(vPolygon).DrawAsSolid or (TsgCADHatch(vPolygon).FillStyle = fsSolid))) then
    begin
      if IsCADGradient(vPolygon) and (not FExpProcs.Active) then
      begin
        if vIsDrawOnCanvas then
        begin
        {$IFNDEF SG_FIREMONKEY}
          SetClipRGN;
        {$ELSE}
         vUseFMXClipRegion := False;
        {$ENDIF}
          if vPolygon is TsgCADFill then
          begin
            {$IFDEF SG_FIREMONKEY}
             if TsgCADFill(vPolygon).StyleFill is TsgCADStylePattern then
               vUseFMXClipRegion := cnstUseFMXClipRegion;
             if vUseFMXClipRegion then
             begin
               if SaveIndex = -1 then
                 SaveIndex := ExpSaveDCInternal(Sender);
               ExpClipRegionInternal(Sender, GetMainRegion);
             end;
            {$ENDIF}
            try
              DrawCADFill(Sender);
            finally
              {$IFDEF SG_FIREMONKEY}
              if vUseFMXClipRegion then
              begin
                ExpRestoreDCInternal(Sender, SaveIndex);
                SaveIndex := -1;
              end;
              {$ENDIF}
            end;
          end
          else
            DrawGradient;
        end;
        if vIsDrawOnBitMap then
        begin
          DoDrawOnBitmapAsFillRgn;
        end;
      end
      else
      begin
        if not FLibraryGDIPlusExists or FExpProcs.Active or FConverting then
        begin
          if vPolygon is TsgCADFill then//for future version
          begin
            if SaveIndex = -1 then
              SaveIndex := ExpSaveDCInternal(Sender);
            ExpClipRegionInternal(Sender, GetMainRegion);
            DrawCADFill(Sender);
          end
          else
          begin
            if IsCADGradient(vPolygon) then
              FColor := TsgCADGradientPolygon(Sender).GradientColor[0];
            DoSetColor(FColor, ctBrush, 3);
            if vPolygon.Boundaries.Count = 1 then
              ExpPolygonInteger(Sender, FContext.Poly)
            else
{$IFNDEF SG_FIREMONKEY}{$IFNDEF SG_USE_AGG2D_AS_GDI}
              if not({$IFDEF SG_DELPHI_VCL}FContext.IsMetafileCanvas or {$ENDIF}(FExpProcs.Active and not Assigned(FExpProcs.ExpFillRgn))) then
                ExpFillRegionInternal(Sender, GetMainRegion)
              else
{$ENDIF}{$ENDIF}
                ExpPolyPolygonInteger(nil, PPoint(FContext.Poly.List), PInteger(FContext.Counts.List), FContext.Counts.Count);
          end;
        end
{$IFDEF SG_USEGDIPLUS}
        else
        begin
          if vIsDrawOnBitMap then
          begin
            DoDrawOnBitmapAsFillRgn;
          end;
          if vIsDrawOnCanvas then
          begin
            DoSetColor(FColor, ctBrush, 3);
            DrawGDIPlusHatch(FColorBrush);
          end;
        end;
{$ENDIF}
      end;
      Exit;
    end;
    case TsgCADHatch(vPolygon).FillStyle of
      fsClear:  begin end;
      fsCAD:
        begin
          vLTScale := FEntityLines.Scale;
          try
            if FEntityLines.IsSolid then
              vDrawDottedParsedLines := False
            else
              vDrawDottedParsedLines := (vPolygon.EntType = ceMPolygon) and InitializeLineType(TsgCADHatch(vPolygon));
            if not vDrawDottedParsedLines then
              DrawPointsListByPolyPolyline(Sender, TsgCADHatch(vPolygon).ParsedLines)
            else
            begin
              for I := 0 to TsgCADHatch(vPolygon).ParsedLines.Count div 2 - 1 do
              begin
                FEntityLines.Line(MakeFPointFrom2D(TsgCADHatch(vPolygon).ParsedLines[I*2]),
                  MakeFPointFrom2D(TsgCADHatch(vPolygon).ParsedLines[I*2+1]), FDottedSingPts);
              end;
              DrawPointsListByPolyPolyline(Sender, FDottedSingPts);
            end;
          finally
            FEntityLines.Scale := vLTScale;
          end;
        end;
    else
      DoSetColor(FColor, ctBrush, 3);
      DoSetStyle(Integer(TsgCADHatch(vPolygon).FillStyle), ctBrush);
      ExpFillRegionInternal(Sender, GetMainRegion, False);
      if vIsDrawOnBitMap then
       DoDrawOnBitmapAsFillRgn;
    end;
  finally
    if SaveIndex <> -1 then
      ExpRestoreDCInternal(Sender, SaveIndex);
    vMainRegion.Free;
{$IFNDEF SG_FIREMONKEY}
    DoSetColor(FColor, ctBrushClear);
{$ENDIF}
    FContext.Poly.Count := 0;
    FContext.Counts.Count := 0;
  end;
end;

function TsgCADImage.DrawMesh(Sender: TObject): Integer;
var
  I, J, C: Integer;
  vPoints: TsgPointsListHelper;
  vVertices: PFPointArray;
  vFaces: TsgFaceIndices;
begin
  Result := 0;
  DrawComplexEntity(Sender);
  if TsgDXFMesh(Sender).Count = 0 then
  begin
    Result := 2;
    C := TsgDXFMeshAccess(Sender).GetFaceIndices(TsgInlineFaceIndices(vFaces), nil);
    try
      if C > 0 then
      begin
        for J := 0 to C - 1 do
          Dec(PInteger(vFaces[J])); // convert PsgVertexIndices to PsgInlineFace

        FContext.Counts.Count := 0;
        FContext.IntPoints.Count := 0;
        vVertices := TsgDXFMeshAccess(Sender).Vertices.List;
        vPoints := TsgPointsListHelper(TsgPointsListHelper(FContext.IntPoints));
        for I := 0 to C - 1 do
        begin
          FContext.Counts.Add(vFaces[I]^.Count + 1);
          for J := 0 to vFaces[I]^.Count - 1 do
            FContext.AddIntPoint(vVertices^[vFaces[I]^.Indices[J]]);
          vPoints.AddPoint(vPoints.Points[vPoints.PointsCount - vFaces[I]^.Count]);
        end;

        // TODO: DrawMesh if assigned FExpProcs

        if IsDrawOnBitMap then
        begin
          AddSelectionMatrix(Sender);
          FContextSelectionMatrix.Changed;
          FContextSelectionMatrix.DoPolyPolygonList(FContext.IntPoints, FContext.Counts);
        end;
        if IsDrawOnCanvas then
          FContext.DoPolyPolygonList(FContext.IntPoints, FContext.Counts);

        FContext.IntPoints.Count := 0;
        FContext.Counts.Count := 0;
      end;
    finally
      Finalize(vFaces);
    end;
  end;
end;

procedure TsgCADImage.DrawMPolygon(Sender: TObject);
var
  I: Integer;
  vMPolygon: TsgCADMPolygon;
  vColor: TsgColorCAD;
  vIsDrawOnCanvas, vIsDrawOnBitMap, vIsDrawBoundaryAsSolid: Boolean;
  vPoints: TFPointList;
  vLTScale: Double;
begin
  vMPolygon := TsgCADMPolygon(Sender);
  vColor := vMPolygon.ColorCAD;
  try
    vMPolygon.ColorCAD := vMPolygon.FillColor;
    FTmpEntColor := GetEntityColors(vMPolygon, FDraw.Insert);
    FColor := FTmpEntColor.DrawColor;
    DoSetColor(FColor, ctPen, 3);
    DrawHatch(Sender);
  finally
    vMPolygon.ColorCAD := vColor;
  end;
  if not IsEqualColorCAD(vMPolygon.ColorCAD,vMPolygon.FillColor) then
  begin
    FTmpEntColor := GetEntityColors(vMPolygon, FDraw.Insert);
    FColor := FTmpEntColor.DrawColor;
    DoSetColor(FColor, ctPen, 3);
    ApplyPen(Sender, @FTmpEntColor.EntColorCAD);
  end;
  vIsDrawOnCanvas := IsDrawOnCanvas;
  vIsDrawOnBitMap := IsDrawOnBitMap;
  if vIsDrawOnBitMap or vIsDrawOnCanvas then
  begin
    if FExpProcs.Active then
    begin
      FContext.Poly.Count := 0;
      for I := 0 to vMPolygon.Boundaries.Count - 1 do
      begin
        Load2DPointToPoints(TF2DPointList(vMPolygon.Boundaries[I]));
        FExpProcs.DoPolylineList(FContext.Poly);
        FContext.Poly.Count := 0;
      end;
    end
    else
    begin
      if vIsDrawOnBitMap then
      begin
        AddSelectionMatrix(Sender);
        FContextSelectionMatrix.BrushStyle := bsClear;
        FContextSelectionMatrix.Changed;
      end;
      FContext.Poly.Count := 0;
      vLTScale := FEntityLines.Scale;
      try
        if not FEntityLines.IsSolid then
          vIsDrawBoundaryAsSolid := not InitializeLineType(vMPolygon)
        else
          vIsDrawBoundaryAsSolid := True;
        if vIsDrawBoundaryAsSolid then
        begin
          for I := 0 to vMPolygon.Boundaries.Count - 1 do
          begin
            Load2DPointToPoints(TF2DPointList(vMPolygon.Boundaries[I]));
            if vIsDrawOnCanvas then
              FContext.DoPolylineList(FContext.Poly);
            if vIsDrawOnBitMap then
              FContextSelectionMatrix.DoPolylineList(FContext.Poly);
            FContext.Poly.Count := 0;
          end;
        end
        else
        begin
          vPoints := TFPointList.Create;
          try
            for I := 0 to vMPolygon.Boundaries.Count - 1 do
            begin
              vPoints.Clear;
              vPoints.AppendArray(TF2DPointList(vMPolygon.Boundaries[I]));
              FEntityLines.Curve(vPoints, FDottedSingPts, False);
            end;
            DrawPointsListByPolyPolyline(Sender, FDottedSingPts);
          finally
            vPoints.Free;
          end;
        end;
      finally
        FEntityLines.Scale := vLTScale;
      end;
    end;
  end;
end;

function TsgCADImage.DrawInsert(Sender: TObject): Integer;
var
  Ins: TsgDXFInsert absolute Sender;
  vFilter: TsgDXFEntity;
{$IFDEF SG_BTI}
  vSelectByInsideArea, vMarkControlPoints: Integer;

  procedure CreateFillPts(const ABoundaryPoints: TFPointLIst; const AIntList: TsgIntegerList);
  var
    J: Integer;
    vPoint: TPoint;
  begin
    AIntList.Capacity := MaxI(AIntList.Capacity, ABoundaryPoints.Count * 2);
    for J := 0 to ABoundaryPoints.Count - 1 do
    begin
      vPoint := GetPoint(ABoundaryPoints[J]);
      AIntList.Add(vPoint.X);
      AIntList.Add(vPoint.Y);
    end;
    AIntList.Add(AIntList[0]);//closed
    AIntList.Add(AIntList[1]);
  end;

  function IsDrawConstruction(const AInsert: TsgDXFInsert): Boolean;
//  var
//    vLine: TsgDXFLine;
  begin
//    Result := False;
//    vLine := TsgDXFLine.Create;
//    try
//      vLine.Point := cnstFPointZero;
//      vLine.Point1 := cnstFPointSingle;
//      vLine.Handle := AInsert.Handle;//for no increments handles
//      Converter.Loads(vLine);
//      if EntVisibleByLayer(vLine, AInsert) then
//        Result := True;
//    finally
//      vLine.Free;
//    end;
    Result := True;
    if Assigned(AInsert.Layer) and (not AInsert.Layer.Visible) then
      Result := False;
  end;

  procedure DrawConstructionMarkControlPoints(const AInsAccess: TsgDXFInsertAccess;
    const AMarkControlPoints: Integer);
  var
    I, J: Integer;
    vBoundaries: TsgObjectList;
    vBoundaryPoints: TFPointList;
    vIntList, vIntCnt: TsgIntegerList;
    vPoint: TPoint;
    vSize: Integer;
    vDoSetColor: Integer;
    vColor: TColor;
  begin
    if (AMarkControlPoints <= 0) or (not IsDrawConstruction(AInsAccess)) or (not IsDrawOnCanvas) then
      Exit;
    vDoSetColor := 0;
    vSize := cnstSnapMarkerSize div 2;
    vBoundaries := TsgObjectList.Create;
    vIntList := TsgIntegerList.Create;
    vIntCnt := TsgIntegerList.Create;
    try
      AInsAccess.GetBoundariesPoints(vBoundaries);
      for I := 0 to vBoundaries.Count - 1 do
      begin
        vBoundaryPoints := TFPointList(vBoundaries[I]);
        vIntList.Count := 0;
        vIntCnt.Count := 0;
        if vBoundaryPoints.Count > 0 then
        begin
          vIntCnt.Capacity := MaxI(vIntCnt.Capacity, vBoundaryPoints.Count);
          for J := 0 to vBoundaryPoints.Count - 1 do
            vIntCnt.Add(5);
          vIntList.Capacity := MaxI(vIntList.Capacity, vBoundaryPoints.Count * 2 * 5);
          for J := 0 to vBoundaryPoints.Count - 1 do
          begin
            vPoint := GetPoint(vBoundaryPoints[J]);
            vIntList.AppendArray([vPoint.X - vSize, vPoint.Y - vSize,
                                  vPoint.X + vSize, vPoint.Y - vSize,
                                  vPoint.X + vSize, vPoint.Y + vSize,
                                  vPoint.X - vSize, vPoint.Y + vSize,
                                  vPoint.X - vSize, vPoint.Y - vSize]);
          end;
          if vDoSetColor = 0 then
          begin
            vDoSetColor := 1 + Ord(cnstMakrControlPointsFill);
            vColor := cnstMakrControlPointsColor;
            DoSetColor(vColor, ctPen);
            if cnstMakrControlPointsFill then
              DoSetColor(vColor, ctBrush);
          end;
          if vDoSetColor = 1 then
            ExpPolyPolylineLists(Sender, vIntList, vIntCnt.List, vIntCnt.Count, False)
          else
            ExpPolyPolygonLists(Sender, vIntList, vIntCnt.List, vIntCnt.Count, False)
        end;
      end;
    finally
      vIntList.Free;
      vIntCnt.Free;
      TsgObjectList.FreeList(vBoundaries, True);
    end;
  end;

  function DrawConstruction(const AInsAccess: TsgDXFInsertAccess;
    const AMode: Integer): Integer;
  var
    vInsAccess: TsgDXFInsertAccess absolute Sender;
    I: Integer;
    vColor: TColor;
    vBoundaries: TsgObjectList;
    vBoundaryPoints: TFPointList;
    vIntList: TsgIntegerList;
    vIsDrawOnCanvas, vIsDrawOnBitMap: Boolean;
    vColorCAD: TsgColorCAD;
  begin
    Result := 2;//Draw only attribs
    vIsDrawOnCanvas := IsDrawOnCanvas;
    vIsDrawOnBitMap := IsDrawOnBitMap;
    if not IsDrawConstruction(AInsAccess) then
      Exit;
    if vIsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
    end;
    if vIsDrawOnCanvas then
    begin
      if (AInsAccess.GetSelectedIndex > -1) and vIsDrawOnCanvas then
      begin
        vColorCAD := vInsAccess.ColorCAD;
        try
          if AInsAccess.GetSelectedIndex < 256 then
            vInsAccess.ColorCAD := MakeColorCAD(acIndexColor, AInsAccess.GetSelectedIndex)
          else
            vInsAccess.ColorCAD := cnstColorCADByLayer;
           FTmpEntColor := GetEntityColors(vInsAccess, FDraw.Insert);
           vColor := FTmpEntColor.DrawColor;
           DoSetColor(vColor, ctBrush, 3);
        finally
          vInsAccess.ColorCAD := vColorCAD;
        end;
      end;
    end;
    try
      vBoundaries := TsgObjectList.Create;
      vIntList := TsgIntegerList.Create;
      try
        vInsAccess.GetBoundariesPoints(vBoundaries);
        for I := 0 to vBoundaries.Count - 1 do
        begin
          vBoundaryPoints := TFPointList(vBoundaries[I]);
          vIntList.Count := 0;
          if vBoundaryPoints.Count > 0 then
          begin
            if vIsDrawOnCanvas then
            begin
              if AInsAccess.GetSelectedIndex > -1 then
              begin
                CreateFillPts(vBoundaryPoints, vIntList);
                FContext.DoPolygonList(vIntList);
              end;
              if vInsAccess.IsDraw and (AMode and 1 <> 0) then
                DrawPointsListByPolyline(vInsAccess, vBoundaryPoints, True);
            end;
            if vIsDrawOnBitMap and (AMode and 2 <> 0) then
            begin
              if vIntList.Count = 0 then
                CreateFillPts(vBoundaryPoints, vIntList);
              FContextSelectionMatrix.DoPolygonList(vIntList);
              FContextSelectionMatrix.BrushStyle := bsClear;
            end;
          end;
        end;
      finally
        vIntList.Free;
        TsgObjectList.FreeList(vBoundaries, True);
      end;
    finally
      if vIsDrawOnBitMap then
        FContextSelectionMatrix.BrushStyle := bsClear;
    end;
  end;

  function DrawElementModifier(const Sender: TObject): Integer;
  var
    vInsAccess: TsgDXFInsertAccess absolute Sender;
  begin
    Result := 2;//Draw only attribs
    DrawConstruction(vInsAccess, 3);
  end;

  procedure DrawConstructionFillOnMatrix(const Sender: TObject;
    const ASelectInArea: Boolean; const AMarkControlPoints: Integer);
  var
    vInsAccess: TsgDXFInsertAccess absolute Sender;
  begin
    if not FUseFillStyleIndex then
    begin
      if (not IsDrawOnBitMap) or (not Assigned(vInsAccess.Block)) then
      begin
        DrawConstructionMarkControlPoints(vInsAccess, AMarkControlPoints);
        Exit;
      end;
    end;
    if ASelectInArea or (vInsAccess.GetSelectedIndex > -1) then
      DrawConstruction(vInsAccess, IfThen(ASelectInArea, 2, 0));
    DrawConstructionMarkControlPoints(vInsAccess, AMarkControlPoints);
  end;


{$ENDIF}
begin
//최승선 수정
{
  if Assigned(Converter) and (Converter is TsgCADtoSVG) then
    TsgCADtoSVG(Converter).ExpData('<G ID="Insert">');  // SVG 데이터 추가 테스트
    }
{$IFDEF SG_BTI}
  if TsgDXFEntityAccess(Ins).GetEntTypeEx = cnstElementModifier then
    Result := DrawElementModifier(Sender)
  else
{$ENDIF}
  begin
    Result := Integer(Ins.Block <> nil);
    // 최승선 수정 2025-01-31
    //블록이면 <G> 태그 시작 추가
    {
    if Ins.Block <> nil then
    begin
      ExpData('<G ID="' + Ins.Name + '">');  //블록 시작
    end;
     }
{$IFDEF SG_BTI}
    vSelectByInsideArea := 0;
    vMarkControlPoints := TsgDXFInsertAccess(Ins).GetMarkControlPoints;
    if Assigned(FConstants) then
      vSelectByInsideArea := FConstants.IntValue[cnstCode_SelectByInsideArea];
    case TsgDXFEntityAccess(Ins).GetEntTypeEx of
      cnstLabel:
        begin
          if not FDimensionsVisible then
            Result := 0;
        end;
      cnstArea:
        begin
          DrawConstructionFillOnMatrix(Sender, vSelectByInsideArea and 1 <> 0, vMarkControlPoints);
        end;
      cnstComplexArea:
        begin
          DrawConstructionFillOnMatrix(Sender, vSelectByInsideArea and 2 <> 0, vMarkControlPoints);
        end;
    end;
{$ENDIF}
    if Result = 1 then
    begin
      if FOnInsert = Sender then
        FOnInsertMode := True;
      if (SelectionMatrix <> nil) and (not FExpProcs.Active) then
      begin
        AddSelectionMatrix(Sender);
        FContextSelectionMatrix.Changed;
      end;
    end;
    if HasClip(Ins, TObject(vFilter)) then
      SetClip(Ins, vFilter);
    // 최승선 수정 2025-01-31
    // 블록이면 </G> 태그 종료 추가
    {
    if Ins.Block <> nil then
    begin
      ExpData('</G>');  //블록 종료
    end;
     }
  end;
end;

function TsgCADImage.DrawDimension(Sender: TObject): Integer;
var
  vDim: TsgDXFDimension absolute Sender;
begin
  Result := Integer(FDimensionsVisible and (vDim.Block <> nil));
  if (Result = 1) and (SelectionMatrix <> nil) then
  begin
    AddSelectionMatrix(Sender);
    FContextSelectionMatrix.Changed;
  end;
end;

function TsgCADImage.CloseRegionSegment(Entity: TsgDXFEntity; AViewPortRegion: TRegion): Integer;
var
  vRegion: TsgRegion;
begin
  Result := cnstContinueIterate;
  if FContext.Poly.Count > 0 then
  begin
    vRegion := TsgRegion.Create;
    try
      vRegion.SetPolygon(FContext.Poly.List^, FContext.Poly.Count shr 1, ALTERNATE);
      TsgRegion(AViewPortRegion).Exclude(vRegion);
      if Assigned(FConverter.Params^.Viewport) and FConverter.Params^.Viewport.VisibleBoundary then
      begin
        FContext.BrushStyle := bsClear;
        if IsDrawOnBitMap then
        begin
          FContextSelectionMatrix.Changed;
          FContextSelectionMatrix.DoPolyline(PPoint(FContext.Poly.List), FContext.Poly.PointsCount);
        end;
        if IsDrawOnCanvas then
          FContext.DoPolyline(PPoint(FContext.Poly.List), FContext.Poly.PointsCount);
      end;
      FContext.Poly.Clear(False);
    finally
      vRegion.Free;
    end;
  end;
end;

procedure TsgCADImage.DrawViewPort(Sender: TObject);
var
  vIsDrawOnBitMap: Boolean;
  vIsDrawOnCanvas: Boolean;
  ClipBoundary: TsgDXFEntity;
  ViewPort: TsgDXFViewPort;
  vViewPortRegion: TsgRegion;

  procedure CreateHRGNByRegion(ARegionEntity: TsgDXFRegion);
  var
    I, K: Integer;
    vRegion: TsgRegion;
    vCollection: TsgModPolyItemsCollection;
    vParams: TsgBrepModIterateParams;
    P: PPoints;
  begin
    FContext.Poly.Clear(False);
    vCollection := TsgModPolyItemsCollection.Create(FContext.Poly, False);
    try
      FillChar(vParams, SizeOf(TsgBrepModIterateParams), 0);
      vParams.Matrix := {$IFNDEF SG_NO_USE_KERNEL3D}Matrix34To44{$ENDIF}(FDraw.Matrix);
      vCollection.UpdateTransformation(vParams.Matrix, {$IFDEF HAS_UNMANAGED_TYPEINFO}TypeInfo(TsgMatrix4d){$ELSE}SizeOf(TsgMatrix4d){$ENDIF});
      Converter.GetModPolylines(ARegionEntity.Compound, vCollection, vParams);
      vCollection.Normalize;
      if (vCollection.Counts.Count > 0) and (FContext.Poly.Count > 1) then
      begin
        P := PPoints(FContext.Poly.List);
        K := vCollection.Counts[0];
        vViewPortRegion.SetPolygon(P^, K, GetPolygonFillingMode);
        for I := 1 to vCollection.Counts.Count - 1 do
        begin
          vRegion := TsgRegion.Create;
          try
            vRegion.SetPolygon(P^[K], vCollection.Counts[I], GetPolygonFillingMode);
            vViewPortRegion.Exclude(vRegion);
          finally
            vRegion.Free;
          end;
          Inc(K, vCollection.Counts[I]);
        end;
        FContext.RegionOffset(vViewPortRegion);
      end;
    finally
      vCollection.Free;
    end;
  end;

begin
  ViewPort := TsgDXFViewPort(Sender);
  if not ViewPort.IsDraw  then
    Exit;
  vIsDrawOnBitMap := IsDrawOnBitMap;
  vIsDrawOnCanvas := IsDrawOnCanvas;
  if vIsDrawOnBitMap then
    SelectionMatrix.SaveClipRGN;
  New(FViewPortCanvasParams);
  FViewPortCanvasParams^.Index := ExpSaveDCInternal(Sender); //SaveDC(FCanvas.Handle);
  FViewPortCanvasParams^.CanvasParams.Brush := TBrush.Create{$IFDEF SG_FIREMONKEY}(TBrushKind.Solid, TColorRec.Black){$ENDIF};
  FViewPortCanvasParams^.CanvasParams.Font := TFont.Create;
  FViewPortCanvasParams^.CanvasParams.Pen := TPen.Create{$IFDEF SG_FIREMONKEY}(TBrushKind.Solid, TColorRec.Black){$ENDIF};
  FViewPortCanvasParams^.CanvasParams.Region := TsgRegion.Create;
  vViewPortRegion := TsgRegion.Create;
  try
  if not FExpProcs.Active then
  begin
    FViewPortCanvasParams^.CanvasParams.Brush.Assign(FContext.Canvas.Brush);
    FViewPortCanvasParams^.CanvasParams.Font.Assign(FContext.Canvas.Font);
    FViewPortCanvasParams^.CanvasParams.Pen.Assign(FContext.Canvas.Pen);
{$IFNDEF USE_SG_AGG2D}
    TsgRegion(FViewPortCanvasParams^.CanvasParams.Region).Update(FContext.Canvas);
    vViewPortRegion.Update(FContext.Canvas);
{$ENDIF}
  end
  else
    vViewPortRegion.FreeReference;
  ViewPort := TsgDXFViewPort(Sender);
  ClipBoundary := TsgDXFViewportAccess(ViewPort).GetClipBoundary(FConverter);
  FContext.Poly.Count := 0;
  if vIsDrawOnBitMap and not FExpProcs.Active then
    AddSelectionMatrix(Sender);
  if ClipBoundary = nil then
  begin
    FContext.AddPolyRect2D(ViewPort.Rect);
    vViewPortRegion.SetPolygon(FContext.Poly.List^, FContext.Poly.Count shr 1, ALTERNATE);
    if not FExpProcs.Active then
      FContext.RegionOffset(vViewPortRegion);
    if ViewPort.VisibleBoundary then
    begin
      if FExpProcs.Active then
      begin
        FExpProcs.DoPolylineList(FContext.Poly);
      end
      else
      begin
        if vIsDrawOnCanvas then
        begin
          FContext.BrushStyle := bsClear;
          FContext.DoPolylineList(FContext.Poly);
{$IFDEF SG_FIREMONKEY}
          FContext.Canvas.Brush.Kind := FViewPortCanvasParams^.CanvasParams.Brush.Kind;
{$ENDIF}
        end;
      end;
    end;
  end
  else
  begin
    if ClipBoundary is TsgDXFRegion then
      CreateHRGNByRegion(TsgDXFRegion(ClipBoundary))
    else//only for classes for which TsgDXFPolyLine is parent
    begin
      DoAppendPoly(TsgDXFPolyLine(ClipBoundary).PolyPoints);
      if FExpProcs.Active then
        FExpProcs.DoPolylineList(FContext.Poly);
      vViewPortRegion.SetPolygon(FContext.Poly.List^, FContext.Poly.Count shr 1, ALTERNATE);
      if not FExpProcs.Active then
        FContext.RegionOffset(vViewPortRegion);
    end;
  end;
  if (FContext.Poly.Count > 0) and (not FExpProcs.Active) and vIsDrawOnBitMap then
  begin
    if SelectionMatrix.ViewPortMode = vmFill then
    begin
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolygonList(FContext.Poly);
    end
    else
    begin
      FContextSelectionMatrix.BrushStyle := bsClear;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolylineList(FContext.Poly);
    end;
  end;
  ExpClipRegionInternal(Sender, vViewPortRegion);
  finally
    vViewPortRegion.Free;
  end;
end;

procedure TsgCADImage.DrawWipeout(Sender: TObject);
var
  W: TsgCADWipeout absolute Sender;
  vRegion: TsgRegion;
  R: PRect;
  vCount: Integer;
begin
  if IsDrawOnCanvas then
  begin
    vRegion := TsgRegion(PolyRegion(W));
    try
      DoSetColor(GetBackgroundColor, ctBrush, 0);
      if FExpProcs.Active then
      begin
        if Assigned(FExpProcs.ExpFillRgn) and vRegion.HandleAllocated then
        begin
          vCount := FRegionStack.GetData(vRegion, R);
          FExpProcs.DoFillRgn(R, vCount);
          FRegionStack.FinalizeData;
        end;
      end
      else
        FContext.RegionPaint(vRegion);
    finally
      vRegion.Free;
    end;
  end;
  if not FConverter.HideWipeouts then
    DrawPoly(W);// draws only WIPEOUT boundary (TsgCADBasePolyline) with pen from W
end;

procedure TsgCADImage.DrawImageEnt(Sender: TObject);
var
  Img: TsgDXFImageEnt absolute Sender;
  vRegion: TsgRegion;
  SaveIndex: Integer;
begin
  if Img.ClipPointsCount < 2 then
    DrawImage(Sender)
  else
  begin
    vRegion := TsgRegion(PolyRegion(Img));
    try
      if FExpProcs.Active then
      begin
        SaveIndex := ExpSaveDCInternal(Sender);
        ExpClipRegionInternal(Sender, vRegion);
        DrawImage(Sender);
        ExpRestoreDCInternal(Sender, SaveIndex);
      end
      else
      begin
        FContext.RegionOffset(vRegion);
        if FContext.RegionUpdate(TsgRegion(FCanvasRegion)) > 0 then
          vRegion.Intersect(FCanvasRegion);
        FContext.RegionSelect(vRegion);
        DrawImage(Sender);
        FContext.RegionSelect(TsgRegion(FCanvasRegion));
      end;
      if Converter.DisplayImageFrame then
        DrawPoly(Img);// draws only IMAGE boundary (TsgCADBasePolyline) with pen from Img
    finally
      vRegion.Free;
    end;
  end;
end;

procedure TsgCADImage.DrawImageGDI(Sender: TObject;
  const AGraphic: TGraphic; const APoints: TsgPointsListHelper);
const
  cnstMinSize = 32;
var
  vImageEnt: TsgDXFImageEntAccess absolute Sender;
  vStretchBltModeChanged: Boolean;
  vSize: TSize;
  vStretchBltMode: Integer;
{$IFNDEF SG_FIREMONKEY}
  vColors, vSaveColors: array[0 .. 1] of Cardinal;
  vPalChanged: Boolean;
  vPalType, vPenType: Integer;
{$ENDIF}

  procedure SelectRgnByMetafile;
  var
    vRegion: TsgRegion;
  begin
    vRegion := TsgRegion.Create;
    try
      vRegion.ClipRect := FContext.Rect;
      FContext.RegionOffset(vRegion);
      if FContext.RegionUpdate(TsgRegion(FCanvasRegion)) > 0 then
        vRegion.Intersect(FCanvasRegion);
      FContext.RegionSelect(vRegion);
    finally
      vRegion.Free;
    end;
  end;

begin
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(AGraphic);
  try
{$ENDIF}
  vStretchBltModeChanged := False;
  vStretchBltMode := GetStretchBltModeInt;
  if not FExpProcs.Active then
  begin
    vSize := GetSizeGraphic(AGraphic);
    if (vSize.cx <= cnstMinSize) and (vSize.cy <= cnstMinSize) then
    begin
      vStretchBltModeChanged := vStretchBltMode <> COLORONCOLOR;
      SetStretchBltModeInt(COLORONCOLOR);
    end;
  end;
{$IFNDEF SG_FIREMONKEY}
  vPalChanged := False;
  if vImageEnt.IsMonoChrome and (AGraphic is TsgBitmap) then
  begin
    vPenType := GetPalColorMode(DefaultColor, BackgroundColor);
    if vPenType > 0 then
    begin
      TsgBitmap(AGraphic).GetColorsByIndexAndRange(@vSaveColors[0], 0, 2);
      vPalType := GetPalColorMode(vSaveColors[0], vSaveColors[1]);
      if vPalType > 0 then
      begin
        vPalChanged := True;
        vColors[1] := BGRToRGB(BackgroundColor);
        if vPenType > 3 then
        begin
          vColors[0] := BGRToRGB(FContext.PenColor);
          if TsgBitmap(AGraphic).PixelFormat <> pf1bit then
            SwapInts(vColors[0], vColors[1]);
        end
        else
        begin
          if BackgroundColor = clBlack then
            vColors[0] := BGRToRGB(clWhite)
          else
            vColors[0] := BGRToRGB(clBlack);
        end;
        if (vPalType and 2 <> 0) then
          SwapInts(vColors[0], vColors[1]);
        TsgBitmap(AGraphic).SetColorsByIndexAndRange(@vColors[0], 0, 2);
      end;
    end;
  end;
{$ENDIF}
 {$IFDEF SG_DELPHI_VCL}
  if AGraphic is TMetafile then
  begin
    try
      SelectRgnByMetafile;
      {$IFNDEF NOGDI}
      ExpImageGDIInternal(Sender, FContext.Rect, AGraphic);
      {$ENDIF}
    finally
      FContext.RegionSelect(TsgRegion(FCanvasRegion));
    end;
  end
  {$IFNDEF NOGDI}
  else
    ExpImageGDIInternal(Sender, FContext.Rect, AGraphic)
 {$ENDIF};
 {$ELSE}
 {$IFNDEF NOGDI}
   ExpImageGDIInternal(Sender, FContext.Rect, AGraphic)
  {$ENDIF};
 {$ENDIF}
{$IFNDEF SG_FIREMONKEY}
  if vPalChanged then
    TsgBitmap(AGraphic).SetColorsByIndexAndRange(@vSaveColors[0], 0, 2);
{$ENDIF}
  if vStretchBltModeChanged then
    SetStretchBltModeInt(vStretchBltMode);
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(AGraphic);
  end;
{$ENDIF}
end;

procedure TsgCADImage.DrawLeader(Sender: TObject);
begin
  if not TsgDXFLeader(Sender).IsSpline then
    DrawPoly(Sender)
  else
    DrawSpline(Sender);
end;

procedure TsgCADImage.DrawLine(Sender: TObject);
var
  vLine: TsgDXFLine absolute Sender;
  vIsDrawOnBitMap: Boolean;
  vLTScale: Single;
  vDir: TFPoint;

  procedure DrawLineAndZTick(const APoint1, APoint2, AZDir: TFPoint);
  begin
    FContext.AddIntPoint(APoint1);
    FContext.AddIntPoint(APoint2, True);
    FContext.AddIntPoint(AddFPoint(APoint2, AZDir), True);
    if FContext.IntPoints.Count < 2 * 2 then
      ExpPixelInteger(Sender, TsgPointsListHelper(FContext.IntPoints).Points[0], FColor)
    else
    begin
      FContext.AddIntPoint(AddFPoint(APoint1, AZDir));
      FContext.AddIntFirstPoint(0);
      ExpPolylineInteger(Sender, FContext.IntPoints);
    end;
    FContext.IntPoints.Count := 0;
  end;

  procedure DrawDottedSingPtsWithZTick(const ADir: TFPoint);
  var
    vMatrixModePrev: TsgSelectionMode;
    I, Cnt: Integer;
  begin
    vMatrixModePrev := GetMatrixMode;
    try
      if (not cnstSolidSelection) or (vMatrixModePrev <> smMatrixOnly) then
      begin
        if cnstSolidSelection then
          SetMatrixMode(smDisabled);
        I := 0;
        Cnt := (FDottedSingPts.Count shr 1) shl 1;
        while I < Cnt do
        begin
          DrawLineAndZTick(FDottedSingPts[I], FDottedSingPts[I + 1], ADir);
          Inc(I, 2);
        end;
      end;
      if cnstSolidSelection and (vMatrixModePrev <> smDisabled) then
      begin
        SetMatrixMode(smMatrixOnly);
        DrawLineAndZTick(vLine.Point, vLine.Point1, ADir);
      end;
    finally
      SetMatrixMode(vMatrixModePrev);
    end;
  end;

begin
  vIsDrawOnBitMap := IsDrawOnBitMap;
  FContext.IntPoints.Count := 0;
  if vLine.ZThick = 0 then
  begin
    if FReady and FEntityLines.IsSolid then
    begin
      if IsEqualPoints(FContext.ReadyPoints.Points[0], FContext.ReadyPoints.Points[1]) then
      begin
        ExpPixelInteger(Sender, FContext.ReadyPoints.Points[0], FColor);
      end
      else
      begin
        FContext.AddIntPointDirect(FContext.ReadyPoints.Points[0]);
        FContext.AddIntPointDirect(FContext.ReadyPoints.Points[1]);
        if NeedToAddOnePixel then
          FContext.AddIntLastPoint;
        ExpPolylineInteger(Sender, FContext.IntPoints);
      end;
    end
    else
    begin
      FContext.AddIntPointDirect(FContext.ReadyPoints.Points[0]);
      FContext.AddIntPointDirect(FContext.ReadyPoints.Points[1]);
      if (FEntityLines.IsSolid) or (vLine.LineTypeScale = 0) then
        ExpPolylineInteger(Sender, FContext.IntPoints)
      else
      begin
        vLTScale := FEntityLines.Scale;
        if InitializeLineType(vLine) then
        begin
          if Assigned(FPolyPoints) then
            FPolyPoints.Count := 0
          else
            FPolyPoints := TFPointList.Create;
          FPolyPoints.Add(vLine.Point);
          FPolyPoints.Add(vLine.Point1);
          MakeAndDrawDottedSingPts(vLine, FPolyPoints, False);
        end
        else
        begin
          if NeedToAddOnePixel then
            FContext.AddIntLastPoint;
          ExpPolylineInteger(Sender, FContext.IntPoints);
        end;
        FEntityLines.Scale := vLTScale;
      end;
    end;
    FContext.IntPoints.Count := 0;;
  end
  else
  begin
    vDir := MakeFPoint(0, 0, vLine.ZThick);
    if Extruded(vLine.Extrusion) then
      DoExtrusion(vDir, vLine.Extrusion);
    if vIsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
    end;
    if FEntityLines.IsSolid then
      DrawLineAndZTick(vLine.Point, vLine.Point1, vDir)
    else
    begin
      begin
        vLTScale := FEntityLines.Scale;
        try
          if InitializeLineType(vLine) then
          begin
            CalcDottedSingPts(vLine, FEntityLines, FDraw.ConvertMatrixCurrentInsertToWCS, FDottedSingPts);
            DrawDottedSingPtsWithZTick(vDir);
          end;
        finally
          FEntityLines.Scale := vLTScale;
        end;
      end;
    end;
  end;
end;

procedure TsgCADImage.DrawPaperSpace;

  procedure DrawRect(const ABox: TFRect; const AIsFill: Boolean;
    const AColor: TColor; const APenStyle: TPenStyle = psSolid);
  const
    cnstColorType: array[Boolean] of TsgColorType = (ctPen, ctBrush);
  var
    vPlotWindowsArea: array [0..4] of TPoint;
  begin
    DoSetColor(AColor, cnstColorType[AIsFill]);
    DoSetStyle(Integer(APenStyle), ctPen);
    vPlotWindowsArea[0] := GetPoint(ABox.TopLeft);
    vPlotWindowsArea[1] := GetPoint(MakeFPoint(ABox.Right, ABox.Top, ABox.Z1));
    vPlotWindowsArea[2] := GetPoint(ABox.BottomRight);
    vPlotWindowsArea[3] := GetPoint(MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z2));
    vPlotWindowsArea[4] := vPlotWindowsArea[0];
    if AIsFill then
      FContext.DoPolygon(@vPlotWindowsArea[0], Length(vPlotWindowsArea))
    else
      FContext.DoPolyline(@vPlotWindowsArea[0], Length(vPlotWindowsArea));
  end;

  function RectInRect(const ASourceRect, ATargetRect: TFRect): Boolean;
  begin
    Result :=
      (ASourceRect.Left > ATargetRect.Left) and
      (ASourceRect.Top > ATargetRect.Top) and
      (ASourceRect.Right < ATargetRect.Right) and
      (ASourceRect.Bottom < ATargetRect.Bottom);
  end;

var
  vPSettings: TsgDXFPlotSettings;
  vShowRect, vPaperArea, vMarginArea: TFRect;
  vMarginColor: TColor;
  vDrawPaperAfterFrame: Boolean;
begin
  if (CurrentLayout.IsModel and FShowPlotForModel) or
     (not CurrentLayout.IsModel) then
  try
    vPSettings := CurrentLayout.PlotSettings;
    vShowRect := vPSettings.PlotFrameArea;
    vPaperArea := vPSettings.PlotPaperArea;
    vMarginArea := vPSettings.PlotMarginsArea;
    vDrawPaperAfterFrame := (vPSettings.PlotType = ptWindowSpecified) and
      RectInRect(vPaperArea, vShowRect);
    DrawRect(vPaperArea, True, GetBackgroundColor, psSolid);

// draw plotsettings window
    if Assigned(FTmpPlotWindow) and (FTmpPlotWindow.PlotType = ptWindowSpecified) then
    begin
      DrawRect(FTmpPlotWindow.PlotFrameArea, True, PlotWindowAreaColor, psClear);
      DrawRect(FTmpPlotWindow.PlotFrameArea, False, $A0A0A0, psDot);
    end
    else
    if (vPSettings.PlotType = ptWindowSpecified) and ShowPlotFrame then
    begin
      DrawRect(vShowRect, True, PlotWindowAreaColor, psClear);
      DrawRect(vShowRect, False, $A0A0A0, psDot);
    end;
    if vDrawPaperAfterFrame then
      DrawRect(vPaperArea, True, GetBackgroundColor, psSolid);
    if ShowPlotMargins then
    begin
      if GetBackgroundColor = clBlack then
        vMarginColor := clWhite
      else
        vMarginColor := clBlack;
      DoSetStyle(Integer(bsClear), ctBrush);
      DrawRect(vMarginArea, False, vMarginColor, psDash);
    end;
  finally
    DoSetStyle(Integer(psSolid), ctPen);
  end;
end;

procedure TsgCADImage.DrawPoint(Sender: TObject);
var
  vPoint: TsgDXFPointAccess absolute Sender;
  I, IE, J, Cnt, vCount: Integer;
  vPt: TFPoint;
  vPointer: Pointer;
begin
  // Draw only null width
  SetPenWidth(FNullWidth);
{$IFDEF SG_FIREMONKEY}
  if IsDrawOnCanvas and (not FExpProcs.Active) and (FContext.PenWidth = 0) then
    FContext.PenWidth := 1;
{$ENDIF}
  FContext.IntPoints.Count := 0;
  if vPoint.Points = nil then
  begin
    if Abs(vPoint.ZThick) > fAccuracy then
    begin
      vPt := MakeFPoint(0, 0, vPoint.ZThick);
      if Extruded(vPoint.Extrusion) then
        DoExtrusion(vPt, vPoint.Extrusion);
      vPt.X := vPoint.Point.X + vPt.X;
      vPt.Y := vPoint.Point.Y + vPt.Y;
      vPt.Z := vPoint.Point.Z + vPt.Z;
      FContext.AddIntSegment(vPoint.Point, vPt);
    end
    else
      FContext.AddIntPoint2(vPoint.Point);
    ExpPolylineInteger(Sender, FContext.IntPoints);
    FContext.IntPoints.Count := 0;
  end
  else
  begin
    I := 2;
    vCount := vPoint.Points.Count;
    while I < vCount do
    begin
      Cnt := Integer(vPoint.Points[I]);
      Inc(I);
      J := I;
      Inc(I, Cnt);
      while J < I do
      begin
        FContext.AddIntPoint(PFPoint(vPoint.Points[J])^);
        Inc(J);
      end;
      if FContext.IntPoints.Count shr 1 = 1 then // point only
        FContext.AddIntFirstPoint;
      ExpPolylineInteger(Sender, FContext.IntPoints);
      FContext.IntPoints.Count := 0;
    end;
    if Abs(vPoint.ZThick) > fAccuracy then
    begin
      vCount := (vPoint.Points.Count - 2) shr 1;
      I := 2;
      IE := I + vCount;
      while I < IE do
      begin
        Cnt := Integer(vPoint.Points[I]);
        Inc(I);
        J := I;
        Inc(I, Cnt);
        while J < I do
        begin
          FContext.AddIntPoint(PFPoint(vPoint.Points[J])^);
          FContext.AddIntPoint(PFPoint(vPoint.Points[J + vCount])^);
          Inc(J);
        end;
      end;
      vCount := FContext.IntPoints.Count shr 2;
      if vCount <> 0 then
      begin
        Cnt := FContext.IntPoints.Count;
        FContext.IntPoints.AppendConst(vCount, 2);
        vPointer := FContext.IntPoints.List;
        Inc(PInteger(vPointer), Cnt);
        ExpPolyPolylineLists(Sender, FContext.IntPoints, vPointer, vCount);
      end;
      FContext.IntPoints.Count := 0;
    end;
  end;
end;

//최승선 수정 DrawBlock 추가
{
procedure TsgCADImage.DrawBlock(Sender: TObject);
var
  BlockBox: TFRect;
  BlockName: string;
  vGTag: string;
  IsBlock: Boolean;
  InsertEntity: TsgDXFInsert;
begin
    InsertEntity := TsgDXFInsert(Sender as TsgDXFInsert);
    BlockBox := InsertEntity.Box;  // 블록의 3D 경계 박스 가져오기
    BlockName := InsertEntity.Block.Name;  // 블록 이름 가져오기
    // 블록명이 없는 경우 기본 ID 할당
    if BlockName = '' then
      BlockName := 'UnnamedBlock_' + IntToStr(InsertEntity.Handle);
    // <g> 태그 생성 (블록명 + Box 좌표 추가)
    vGTag := Format('<g id="%s" data-top="%f,%f" data-bottom="%f,%f">',
                    [BlockName, BlockBox.Left, BlockBox.Top, BlockBox.Right, BlockBox.Bottom]);
    IsBlock := True;  // 블록 처리됨 표시
end;
}

procedure TsgCADImage.DrawPoly(Sender: TObject);
var
  PLine: TsgCADBasePolyLineAccess absolute Sender;
  PL: TsgDXFPolyLine;
  vDx, vDy: Double;

  procedure DrawPolygonMesh(const M, N: Integer; const APointsList: TFPointList;
    const AIsSolid: Boolean);
  var
    I, J, K, C: Integer;
    vMClosed, vNClosed: Boolean;
    P: PInteger;
  begin
    vMClosed := PLine.IsMeshMClosed;
    vNClosed := PLine.IsMeshNClosed or (PLine.Closed and PLine.IsPolyZThickness);
    for I := 0 to APointsList.Count - 1 do
    begin
      FContext.AddPolyPoint(APointsList[I]);
      if AIsSolid and ((I + 1) mod N = 0) and vNClosed then
        FContext.Poly.AddPoint(FContext.Poly.Points[FContext.Poly.PointsCount - N]);
    end;
    if AIsSolid or (PLine.IsPolygonMesh and not PLine.IsPolyZThickness)then
    begin
      K := FContext.Poly.Count;
      for I := 0 to N - 1 do
      begin
        J := I shl 1;
        while J < K do
        begin
          FContext.Poly.AddPoint(FContext.Poly.Points[J shr 1]);
          Inc(J, N shl 1 + 2 * Ord(vNClosed));
        end;
        if vMClosed then
          FContext.Poly.AddPoint(FContext.Poly.Points[I]);
      end;
      J := FContext.Poly.Count;
      FContext.Poly.AppendConst(M, N + Ord(vNClosed));
      FContext.Poly.AppendConst(N, M + Ord(vMClosed));
      K := M + N;
    end
    else
    begin
      if vNClosed or PLine.IsPolyZThickness then
      begin
        C := Round(APointsList.Count / M);
        for J := 1 to M - 1 do
          for I := 0 to C - 1 do
          begin
            K := C * J + I;
            FContext.AddPolyPoint(APointsList[K]);
            Dec(K, C);
            FContext.AddPolyPoint(APointsList[K]);
          end;
      end;
      J := FContext.Poly.Count;
      K := Round(J / 4);
      FContext.Poly.AppendConst(K, 2);
    end;
    P := PInteger(FContext.Poly.List);
    Inc(P, J);
    ExpPolyPolylineLists(Sender, FContext.Poly, P, K);
    (*
    if IsDrawOnBitMap then
    begin
      AddSelectionMatrix(Sender);
      FContextSelectionMatrix.Changed;
      PolyPolyline(SelectionMatrix.Handle, FPoly.List^, P^, K);
    end;
    if IsDrawOnCanvas then
      PolyPolyline(FCanvas.Handle, FPoly.List^, P^, K);
    *)
  end;

  procedure DrawPolyFaceMesh;
  var
    I: Integer;
    vPolyPoints: TsgIntegerList;

    procedure CreateWireframeIndexes(vertexNumber: Integer; APLine: TsgDXFPolyline);
    var
      Index, vMaxIndex, vOldIndex, J: Integer;

      function GetVertexIndex(AItemIndex: Integer): Integer;
      begin
        Result := Integer(PL.PolyFaceVertexIndexes.Items[AItemIndex]);
      end;

      procedure AddEdge(AFirst, ALast, AMaxIndex: Integer; APolyLine: TsgDXFPolyline);
      begin
        if ((APolyLine <> nil) and (AFirst > 0) and (Alast <> 0)
          and (AFirst <= AMaxIndex) and (Abs(ALast) <= AMaxIndex)) then
        begin
          FContext.AddPolyPoint(APolyLine.Points[AFirst - 1]);
          FContext.AddPolyPoint(APolyLine.Points[Abs(ALast) - 1]);
          vPolyPoints.Add(2);
        end;
      end;
    begin
      vMaxIndex := PL.PolyPoints.Count;
      vOldIndex := GetVertexIndex(vertexNumber);
      if vOldIndex = 0 then
        Exit;
      for J := 1 to 3 do
      begin
        Index := GetVertexIndex(vertexNumber + j);
        if Index = 0 then
          Break
        else
          AddEdge(vOldIndex, Index, vMaxIndex, APLine);
        vOldIndex := Index;
      end;
      AddEdge(vOldIndex, GetVertexIndex(vertexNumber), vMaxIndex, APLine);
    end;
  begin
    vPolyPoints := TsgIntegerList.Create;
    try
      I := 0;
      while I < PL.PolyFaceVertexIndexes.Count do
      begin
        CreateWireframeIndexes(I, PL);
        Inc(I, 4);
      end;

      ExpPolyPolylineLists(Sender, FContext.Poly, PInteger(vPolyPoints.List), vPolyPoints.Count);
      (*
      if IsDrawOnBitMap then
      begin
        AddSelectionMatrix(Sender);
        FContextSelectionMatrix.Changed;
        PolyPolyline(SelectionMatrix.Handle, FPoly.List^, PInteger(vPolyPoints.List)^, vPolyPoints.Count);
      end;
      if IsDrawOnCanvas then
        PolyPolyline(FCanvas.Handle, FPoly.List^, PInteger(vPolyPoints.List)^, vPolyPoints.Count);
      *)
    finally
      vPolyPoints.Free;
    end;
  end;

  function DrawArrows(const AApplyLineType: Boolean = False): Boolean;
  var
    Arrows: TFPointList;
//  vLTScale: Double;
  begin
    Result := False;
    FContext.Poly.Count := 0;
    Arrows := nil;
    if PL <> nil then
    begin
      Arrows := PL.Arrows;
      if AApplyLineType then
      begin
//        vLTScale := FEntityLines.Scale;
//        try
//          if InitializeLineType(PLine) then
//          begin
//            CalcDottedSingPts(TsgDXFEntity(Sender), FEntityLines, FDraw.ConvertMatrixCurrentInsertToWCS, FDottedSingPts)
//          end
//          else
//            DoPolylineArrow(PL, nil, nil);
//        finally
//          Arrows := PL.Arrows;
//          FEntityLines.Scale := vLTScale;
//        end;
      end;
    end;
    if (Arrows = nil) or (Arrows.Count <= 3) then
      Exit;
    try
      if FContext.Poly.Capacity < Arrows.Capacity * 2 then
      begin
        try
          FContext.Poly.Capacity := Arrows.Capacity * 2;
        except
          SetListsCapacity(True);
          FContext.Poly.Capacity := Arrows.Capacity * 2;
        end;
      end;
      DoAppendPoly(Arrows);
      DoSetColor(FColor, ctBrush, 3);
      ExpArrowsInternal(Sender, FContext.Poly, PL.IsPolyZThickness);
      Result := True;
    finally
      SetListCapacity(FContext.Poly, False);
    end;
  end;

  function IsPolyPathClosed: Boolean;
  begin
    Result := PLine.Closed and (PLine.PolyPoints.Count > 2);
  end;

  procedure BeginPolyPath;
  begin
{$IFNDEF SG_NON_WIN_PLATFORM}
    if not FExpProcs.Active then
    begin
      if IsDrawOnBitMap then
        FContextSelectionMatrix.BeginPath;
      if IsDrawOnCanvas then
        FContext.BeginPath;
    end;
{$ENDIF}
  end;

  procedure EndPolyPath;
  begin
    if FExpProcs.Active then
    begin
      //  After calling CloseFuger pdf ceases to show the objects.
      //if Assigned(FExpProcs.ExpCloseFigure) then
      //  FExpProcs.DoCloseFigure;
    end
    else
    begin
      if IsDrawOnBitMap then
      begin
{$IFNDEF SG_NON_WIN_PLATFORM}
        FContextSelectionMatrix.CloseFigure;
        FContextSelectionMatrix.EndPath;
        FContextSelectionMatrix.StrokePath;
{$ENDIF}
      end;
      if IsDrawOnCanvas then
      begin
{$IFNDEF SG_NON_WIN_PLATFORM}
        FContext.CloseFigure;
        FContext.EndPath;
        FContext.StrokePath;
{$ENDIF}
      end;
    end;
  end;

  procedure DrawPolyLineReady;
  var
    vClosePath: Boolean;
    vCnt: Integer;
  begin
    vClosePath := IsPolyPathClosed;
    if vClosePath then
      BeginPolyPath
    else
    begin
      if (FContext.IndexPoints > 1) and NeedToAddOnePixel then
        FContext.AddOnePixel;
    end;
    vCnt := FContext.ReadyPoints.Count;
    try
      FContext.ReadyPoints.Count := FContext.IndexPoints * 2;
      ExpPolylineInteger(Sender, FContext.ReadyPoints);
    finally
      FContext.ReadyPoints.Count := vCnt;
    end;
    if vClosePath then
      EndPolyPath;
  end;

  procedure DrawPolyLineExt;
  var
    vLTScale: Single;
    vClosePath: Boolean;
  begin
    if FReady and FEntityLines.IsSolid then
      DrawPolyLineReady
    else
      if FEntityLines.IsSolid then
      begin
        vClosePath := IsPolyPathClosed;
        if vClosePath then
          BeginPolyPath;
        DrawPointsListByPolyline(Sender, PLine.PolyPoints, PLine.Closed);
        if vClosePath then
          EndPolyPath;
        DoPolylineArrow(PL, nil, nil);
      end
      else
      begin
        vLTScale := FEntityLines.Scale;
        try
          if InitializeLineType(PLine) then
            MakeAndDrawDottedSingPts(TsgDXFEntity(Sender), PLine.PolyPoints, PLine.Closed)
          else
          begin
            DrawPointsListByPolyline(Sender, PLine.PolyPoints, PLine.Closed);
            DoPolylineArrow(PL, nil, nil);
          end;
        finally
          FEntityLines.Scale := vLTScale;
        end;
      end;
  end;

begin
  GetPixelSize(1.5, vdX, vdY, True);
  FContext.Poly.Count := 0;
  if not (Sender is TsgDXFPolyLine) then
    PL := nil
  else
    PL := TsgDXFPolyLine(Sender);
  if PLine.IsPolygonMesh and (PL <> nil) then
    DrawPolygonMesh(PL.MeshM, PL.MeshN, PL.PolyPoints, TsgCADBasePolyLineAccess(Sender).IsPolyPointsSolid)
  else
    if PLine.IsPolyZThickness then
    begin
      if not DrawArrows(True) then
        DrawPolygonMesh(2, PLine.PolyPoints.Count shr 1, PLine.PolyPoints, TsgCADBasePolyLineAccess(Sender).IsPolyPointsSolid);
    end
    else
      if (PL <> nil) and PL.IsPolyFaceMesh then
        DrawPolyFaceMesh
      else
      begin
        DrawPolyLineExt;
        DrawArrows;
      end;
end;

type
  TAddModPoint = procedure(const APoint: TFPoint; AParam: Pointer);

procedure DoConextAddPoint(const APoint: TFPoint; AParam: Pointer);
begin
  TsgContext(AParam).AddIntPoint(APoint);
end;

procedure DoListAddPoint(const APoint: TFPoint; AParam: Pointer);
begin
  TFPointList(AParam).Add(APoint);
end;

procedure TsgCADImage.DrawModPolyline(const APoly: TsgModMeshPolyline; const AParams: TsgBrepModIterateParams);
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  vSender: TsgDXFEntity;
  vLTScale: Double;
  vPoly: TsgDXFPolyline;

  function DoEnumModMeshPolylineNode(ANode: TsgModMeshPolylineNode;
    const AAddModPoint: TAddModPoint; AParam: Pointer): TFPoint;
  begin
     while Assigned(ANode) do
    begin
      Result := PointTransform(ANode.Point, AParams.Matrix);
      SetPixelSnap(Result, [osEndPt]);
      AAddModPoint(Result, AParam);
      ANode := ANode.Next;
    end;
  end;

begin
  if not Assigned(APoly) then
    Exit;
  FContext.IntPoints.Count := 0;
  FContext.DotsPoints.Count := 0;
  if not FEntityLines.IsSolid then
  begin
    vSender := TsgDXFEntity(AParams.Data);
    vPoly := TsgDXFPolyline.Create;
    try
      vPoly.LineTypeScale := vSender.LineTypeScale;
      vPoly.LineType := vSender.LineType;
      DoEnumModMeshPolylineNode(APoly.FirstNode, DoListAddPoint, vPoly.PolyPoints);
      vPoly.PointsCounts.Add(vPoly.PolyPoints.Count);
      vLTScale := FEntityLines.Scale;
      try
        if InitializeLineType(vPoly) then
        begin
          CalcDottedSingPts(vPoly, FEntityLines,
            FDraw.ConvertMatrixCurrentInsertToWCS, FDottedSingPts);
          DrawPointsListByPolyPolyline(vSender, FDottedSingPts, False);
        end
        else
          DrawPointsListByPolyline(vSender, vPoly.PolyPoints, False);
      finally
        FEntityLines.Scale := vLTScale;
      end;
    finally
      vPoly.Free;
    end;
  end
  else
  begin
    DoEnumModMeshPolylineNode(APoly.FirstNode, DoConextAddPoint, FContext);
    if FContext.IntPoints.Count > 1 * 2 then
      ExpPolylineInteger(AParams.Data, FContext.IntPoints);
  end;
{$ELSE}
begin  
{$ENDIF}  
end;

procedure TsgCADImage.DrawModEdge(const AEdge: TsgModTopoEdge; const AParams: TsgBrepModIterateParams);
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  vColorAttrib: TsgModAttribColorBase;
begin
  vColorAttrib := AEdge.QueryColorAttrib;
  DoSetColor(TsgBrepModEntity.FindColor(AParams, vColorAttrib), ctPen, 3);
  DrawModPolyline(AEdge.ExtractFirstPolyline, AParams);
{$ELSE}
begin  
{$ENDIF}
end;

procedure TsgCADImage.DrawModIsoLines(const AFace: TsgModTopoFace; const AParams: TsgBrepModIterateParams);
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  I: Integer;
  vColorAttrib: TsgModAttribColorBase;
begin
  vColorAttrib := AFace.QueryColorAttrib;
  DoSetColor(TsgBrepModEntity.FindColor(AParams, vColorAttrib), ctPen, 3);
  for I := 0 to AFace.IsoLines.Count - 1 do
    DrawModPolyline(TsgModMeshPolyline(AFace.IsoLines[I]), AParams);
{$ELSE}
begin
{$ENDIF}    
end;

procedure TsgCADImage.DrawModEntity(const AEnt: TsgModEntity; var AParams: TsgBrepModIterateParams);
begin
{$IFNDEF SG_NO_USE_KERNEL3D}
  case AEnt.GetEntType of
    mtTopology:
    begin
      case TsgModTopoEdge(AEnt).GetShapeType of
        btEdge:
        begin
          DrawModEdge(TsgModTopoEdge(AEnt), AParams);
        end;
        btFace:
        begin
          DrawModIsoLines(TsgModTopoFace(AEnt), AParams);
        end;
      end;
    end;
  end;
{$ENDIF}  
end;

procedure TsgCADImage.DrawACISEntity(Sender: TObject);
var
  vBrepModParams: TsgBrepModIterateParams;
  vEntity: TsgBrepModEntity absolute Sender;
begin
  case TsgDXFEntity(Sender).EntType of
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
    ceRegion, ceBody, ceSurface, ce3DSolid:
    begin
      vBrepModParams.CADColor := FColor;
      vBrepModParams.Matrix := {$IFNDEF SG_NO_USE_KERNEL3D}IdentityMatrix4d{$ELSE}cnstIdentityMat{$ENDIF};
      vBrepModParams.PartColor := nil;
      vBrepModParams.ShapeColor := nil;
      vBrepModParams.Data := Sender;
      vEntity.DrawIterateCompound(DrawModEntity, vBrepModParams);
    end;
  end;
end;

procedure TsgCADImage.DrawSpline(Sender: TObject);
var
  vSpline: TsgDXFSpline absolute Sender;
  vLTScale: Double;
begin
  FContext.Poly.Count := 0;
  if vSpline.FitCount > vSpline.ControlCount then
  begin
    DrawPoly(vSpline);
    Exit;
  end;
  if FEntityLines.IsSolid then
    DrawPointsListByPolyline(Sender, vSpline.PolyPoints, vSpline.Closed) //evg
  else
  begin
    vLTScale := FEntityLines.Scale;
    try
      if InitializeLineType(vSpline) then
      begin
        FEntityLines.Curve(vSpline.PolyPoints, FDottedSingPts, vSpline.Closed);
        DrawPointsListByPolyPolyline(Sender, FDottedSingPts, vSpline.Closed);
      end
      else
        DrawPointsListByPolyline(Sender, vSpline.PolyPoints, vSpline.Closed);
    finally
      FEntityLines.Scale := vLTScale;
    end;
  end;
end;

procedure TsgCADImage.DrawSolid(Sender: TObject);
var
  vSolid: TsgDXFSolidAccess absolute Sender;
  vHasZTick: Boolean;
  I, vCount, vCount1, vCount2: Integer;
  vSolids, vSolidsZ: array[0..4] of TPoint;
  vPts: TsgPoints4;
  vMText: TsgDXFMText;
//{$IFDEF MSWINDOWS}
//  vRop: Integer;
//{$ENDIF}

  function IsDrawingZ(const ACoodrZ: Double): Boolean;
  var
    vDistanceZ: Double;
    vPlaneXYVect: TFPoint;
  begin
    Result := False;
    if ACoodrZ > fDoubleResolution then
    begin
      vPlaneXYVect := PtXScalar(FDraw.Matrix.EZ, ACoodrZ);
      vDistanceZ := DistanceVector2D(vPlaneXYVect.X, vPlaneXYVect.Y);
      Result := Abs(vDistanceZ) > fAccuracy;
    end;
  end;

  function CalcNormal(const AP1, AP2, AP3: TFPoint{; var ALen: Double}): TFPoint;
  var
    v1, v2: TFPoint;
  begin
    v1 := SubFPoint(AP2, AP1);
    v2 := SubFPoint(AP3, AP1);
    Result := sgFunction.Vector(v1, v2);
    //ALen := DistanceFVector(Result);
  end;

  function IsSolidCollinearToView(const APoints4: TsgPoints4): Boolean;
  var
    //vLen: Double;
    vNormal, vView: TFPoint;
  begin
    vNormal := Ort(CalcNormal(APoints4[0], APoints4[1], APoints4[2]{, vLen}));
    vView := Ort(FDraw.Matrix.EZ);
    Result := IsEqualFPoints(vNormal, vView) or
      IsEqualFPoints(vNormal, Reverse(vView));
  end;

  function GetPoints(var APoints: array of TPoint; ACount: Integer;
    const APoints4: TsgPoints4): Integer;
  var
    P: TPoint;
    I, J: Integer;
  begin
    Result := ACount;
    I := 0;
    P := GetPoint(APoints4[I mod ACount]);
    APoints[I] := P;
    Inc(I);
    J := I;
    repeat
      APoints[J] := GetPoint(APoints4[I mod ACount]);
      if not vHasZTick and IsEqualPoints(P, APoints[J]) then
        Dec(Result)
      else
      begin
        P := APoints[J];
        Inc(J);
      end;
      Inc(I);
    until I >= ACount;
    APoints[Result] := APoints[0];
    Inc(Result);
  end;

begin
  vMText := nil;
  if Assigned(FDraw.Insert) and (FDraw.Insert.EntType = ceMText) then
    vMText := TsgDXFMText(FDraw.Insert);
  if Assigned(vMText) and (vMText.BackgroundFlags and 3 <> 0) then
  begin
    SetPenWidth(0);
    case vMText.BackgroundFlags and 3 of // do not check R2018 flag $10
      0, 2:
        begin
//          vMText := nil;
          Exit;
        end; // no background or background fill with drawing fill color
      1: // background fill
        begin
          DoSetColor(FColor, ctBrush, 3);
          DoSetColor(FColor, ctPen, 3);
        end;
      3: // background fill with drawing fill color
        begin
          DoSetColor(GetBackgroundColor, ctBrush);
          DoSetColor(GetBackgroundColor, ctPen);
        end;
    end;
  end
  else
  begin
    SetPenWidth(1);
    DoSetColor(FColor, ctBrush, 3);
    if Sender is TsgDXFImageEnt then
      DoSetColor(FColor, ctBrushClear);
//    vMText := nil;
  end;
  vCount := vSolid.GetPoints(vPts);
//  if FReady then
//    CopyMemory(@vPts, @FContext.Points, SizeOf(TPoint) * 4);
  vHasZTick := False;
  if (Abs(vSolid.ZThick) > fAccuracy) or
     IsDrawingZ(Max(Abs(vSolid.ZThick), Abs(vPts[0].Z))) then
    if not IsSolidCollinearToView(vPts) then
    //if IsRotatedFMat(FDraw.Matrix, True) then//change in future version(check only view matrix)
    begin
      vHasZTick := not FExpProcs.Active;//true;
    end;
  if vCount = 4 then
    SwapFPoints(vPts[2], vPts[3]);
  vCount1 := GetPoints(vSolids, vCount, vPts);
  vCount2 := 0;
  if vHasZTick then
  begin
    for I := 0 to vCount - 1 do
      vPts[I].Z := vPts[I].Z + vSolid.ZThick;
    vCount2 := GetPoints(vSolidsZ, vCount, vPts);
  end;
//{$IFDEF MSWINDOWS}
//  if Assigned(vMText) then
//  begin
//    FContext.BrushStyle := bsSolid;
//    vRop := SetROP2(FContext.GetDC, vMText.BackgroundTransparency);
//  end;
//{$ENDIF}
  ExpSolidInternal(Sender, Slice(vSolids, vCount1), Slice(vSolidsZ, vCount2), vHasZTick);
//{$IFDEF MSWINDOWS}
//  if Assigned(vMText) then
//    SetROP2(FContext.GetDC, vRop);
//{$ENDIF}
end;

procedure TsgCADImage.DrawImage(Sender: TObject);
var
  vImageEnt: TsgDXFImageEntAccess absolute Sender;
  vAngle, vAngleY: Double;
  R: Integer;
  vGraphic: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TInterfacedPersistent{$ENDIF};
{$IFDEF SG_FIREMONKEY}
  C: TFPoint;
  vMatrix, vTmpMatrix: TMatrix;
  vImageRect: TRectF;
  vDrawMatrix, vImageView: TFMatrix;
  vOffset: TVector;
  vRect: TRect;

{$ELSE}
{$IFDEF MSWINDOWS}
  vGraphicsMode: Integer;
  C: TFPoint;
  vMatrix, vTmpMatrix: TXForm;
  vImageRect: TRect;
  vDrawMatrix, vImageView: TFMatrix;
{$ENDIF}
  vTransparentColor: TColor;
  vTransparent: Boolean;
{$ENDIF}

  procedure GetPointAndAngle;
  begin
    FContext.SetMatrix(@FDraw.Matrix);
    FContext.AddIntPoint(vImageEnt.Point);
    FContext.AddIntPoint(vImageEnt.Point1);
    FContext.AddIntPoint(vImageEnt.Point3);
    FContext.AddIntPoint(vImageEnt.Point2);
    FContext.AddIntFirstPoint(0);
    GetAngles(vImageEnt, vAngle, vAngleY);
  end;

{$IFDEF SG_FIREMONKEY}
  function maketransparentbitmap(src: TBitmap; mode: Integer): TBitmap;
  type
    PAlphaColorRecArray = ^TAlphaColorRecArray;
    TAlphaColorRecArray = array[0 .. MaxInt div SizeOf(TAlphaColorRec) - 1] of TAlphaColorRec;
  var
    X, Y: Integer;
    vData: FMX.Graphics.TBitmapData;
    vPixel: PAlphaColorRecArray;
    vDefaultColor, vTransparentColor, vBackgroundColor: TAlphaColor;
  begin
    { TODO: use effects!!! }
    Result := TBitmap.Create;
    Result.Assign(src);
    if Result.Map(TMapAccess.ReadWrite, vData) then
    try
      vTransparentColor := TColor(vImageEnt.TransparentColor).AsBGRA;
      vBackgroundColor := BackgroundColor.AsBGRA;
      vDefaultColor := FColorPen.AsBGRA;
      for Y := 0 to vData.Height - 1 do
      begin
        vPixel := PAlphaColorRecArray(vData.GetScanline(Y));
        for X := 0 to vData.Width - 1 do
          case mode of
            1: //Monochrome only
              begin
                if TAlphaColor(vPixel^[X]) = vTransparentColor then
                  TAlphaColor(vPixel^[X]) := vBackgroundColor
                else
                  TAlphaColor(vPixel^[X]) := vDefaultColor;
              end;
            2: //transparent only
              begin
                if TAlphaColor(vPixel^[X]) = vTransparentColor then
                  vPixel^[X].A := 0;
              end;
            3: //Monochrome and transparent
              begin
                if TAlphaColor(vPixel^[X]) = vTransparentColor then
                  vPixel^[X].A := 0
                else
                  TAlphaColor(vPixel^[X]) := vDefaultColor;
              end;
          end; { case }
      end;
    finally
      Result.Unmap(vData);
    end;
  end;
{$ENDIF}

  procedure DoDrawImageGDI;
{$IFDEF SG_FIREMONKEY}
  var
    vBitmap: TBitmap;
    vImageCacheItem: TsgImageCacheItem;
{$ELSE}
{$IFDEF MSWINDOWS}
  var
    vBitmap: TBitmap;
{$ENDIF}
{$ENDIF}
  begin
{$IFDEF SG_FIREMONKEY}
    vImageCacheItem.Mode := Ord(vImageEnt.IsMonoChrome and IsAutocadLType) or (Ord(vImageEnt.Transparency) shl 1);
    if (vImageCacheItem.Mode <> 0) and (TObject(vGraphic) is TBitmap) then
    begin
      vImageCacheItem.BitmapHashCode := vGraphic.GetHashCode;
      vImageCacheItem.TransparentColor := vImageEnt.TransparentColor;
      vImageCacheItem.DefaultColor := FColorPen;
      if not Assigned(FImagesCache) then
        FImagesCache := TDictionary<TsgImageCacheItem,TBitmap>.Create;
      if not TDictionary<TsgImageCacheItem,TBitmap>(FImagesCache).TryGetValue(vImageCacheItem, vBitmap) then
      begin
        vBitmap := maketransparentbitmap(TBitmap(vGraphic), vImageCacheItem.Mode);
        TDictionary<TsgImageCacheItem,TBitmap>(FImagesCache).Add(vImageCacheItem, vBitmap);
      end;
      DrawImageGDI(vImageEnt, TGraphic(vBitmap), TsgPointsListHelper(FContext.IntPoints));
    end
    else
{$ELSE}
{$IFDEF MSWINDOWS}
    if (vGraphic.Height/vGraphic.Width >= 100) and (vGraphic is TsgBitmap) then
    begin
      vBitmap := TBitmap.Create;
      try
        vBitmap.Assign(vGraphic);
        DrawImageGDI(vImageEnt, vBitmap, TsgPointsListHelper(FContext.IntPoints));
      finally
        vBitmap.Free;
      end;
    end
    else
{$ENDIF}
{$ENDIF}
        DrawImageGDI(vImageEnt, TGraphic(vGraphic), TsgPointsListHelper(FContext.IntPoints));
  end;

{$IFDEF SG_USEGDIPLUS}
var
  vNeedReleaseGraphicRef: Boolean;
{$ENDIF}
begin
  if vImageEnt.Empty or not ShowImages then
    Exit;
  FContext.IntPoints.Count := 0;
  GetPointAndAngle;
  if IsDrawOnBitMap and (not vImageEnt.Internal) then
  begin
    AddSelectionMatrix(Sender);
    if cnstSelectRasterMode = 1 then
    begin
      FContext.IntPoints.AppendArray([FContext.IntPoints[0], FContext.IntPoints[1]]);
      try
        FContextSelectionMatrix.Changed;
        FContextSelectionMatrix.DoPolylineList(FContext.IntPoints);
      finally
        FContext.IntPoints.Count := FContext.IntPoints.Count - 2;
      end;
    end
    else
    begin
      FContextSelectionMatrix.BrushStyle := bsSolid;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolygonList(FContext.IntPoints);
      FContextSelectionMatrix.BrushStyle := bsClear;
    end;
  end;
{$IFNDEF SG_FIREMONKEY}
  CheckRastrByFastMode(vImageEnt, vGraphic, TsgPointsListHelper(FContext.IntPoints));
{$ELSE}
  vGraphic := vImageEnt.Picture.Graphic;
{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
  vTransparent := vGraphic.Transparent;
  vGraphic.Transparent := vImageEnt.Transparency;
  if vGraphic.InheritsFrom(Graphics.TBitmap) then
  begin
    vTransparentColor := Graphics.TBitmap(vGraphic).TransparentColor;
    Graphics.TBitmap(vGraphic).TransparentColor := vImageEnt.TransparentColor;
  end
  else
    vTransparentColor := vImageEnt.TransparentColor;
  try
{$ENDIF}
{$IFDEF SG_USEGDIPLUS}
  if FLibraryGDIPlusExists and bUseGDIPlusForRastImgRotate and
    ( FExpProcs.Active or not CheckOrthoRaster(vAngle) or (vGraphic is TMetafile) or
      vImageEnt.Transparency or (not IsDrawGDI(vAngle, vAngleY))) then
  begin
    vNeedReleaseGraphicRef := vGraphic = vImageEnt.Picture.Graphic;
    DrawImageGDIPlus(vImageEnt, TsgPointsListHelper(FContext.IntPoints));
    vNeedReleaseGraphicRef := vNeedReleaseGraphicRef and (vGraphic <> vImageEnt.Picture.Graphic);
    if vNeedReleaseGraphicRef then
      vGraphic := nil;
  end
  else
{$ENDIF}
  begin
    FContext.IncRectRightBottom;
    R := (Round(vAngle) div 90) * 90;
    if R <> 0 then
    begin
      // rotate must be implemented to initialize ... if you need a show GDI
    end;
    if {$IFDEF SG_FIREMONKEY} (R = 0) and (FExpProcs.Active or ((Round(vAngleY) div 90) * 90 = 90)) and{$ENDIF} CheckOrthoRaster(vAngle)then
    begin
      DoDrawImageGDI;
    end
    else
    begin
{$IFDEF SG_FIREMONKEY}
      if not FExpProcs.Active then
      begin
        vTmpMatrix := FContext.Canvas.Matrix;
        try
          vDrawMatrix := DrawMatrix;
          FillChar(vMatrix, SizeOf(vMatrix), 0);
          vImageView.EX := Ort(vImageEnt.UVector);
          vImageView.EY := Ort(vImageEnt.VVector);
          vImageView.EZ := Ort(sgFunction.Vector(vImageView.EX, vImageView.EY));
          vImageView.E0 := cnstFPointZero;
          C := FPointXMat(GetCenterOfRect(vImageEnt.Box), vDrawMatrix);
          vDrawMatrix := FMatXMat(vImageView, vDrawMatrix);
          vMatrix.m11 := vDrawMatrix.V[0].V[0];
          vMatrix.m12 := vDrawMatrix.V[0].V[1];
          vMatrix.m21 := -vDrawMatrix.V[1].V[0];
          vMatrix.m22 := -vDrawMatrix.V[1].V[1];
          vImageRect := TRectF.Create(0, vImageEnt.Height, vImageEnt.Width, 0);
          vOffset := TVector.Create(TPointF.Create(C.X, C.Y) - vImageRect.CenterPoint * vMatrix);
          vMatrix.M[2] := vOffset + TVector.Create(vTmpMatrix.m31, vTmpMatrix.m32);
          vRect := vImageRect.Round;
          vRect.NormalizeRect;
          FContext.Rect := vRect;
          FContext.Canvas.SetMatrix(vMatrix);
          DoDrawImageGDI;
        finally
          FContext.Canvas.SetMatrix(vTmpMatrix);
        end;
      end
      else
{$ELSE}
{$IFDEF MSWINDOWS}
      if not FExpProcs.Active then
      begin
        vGraphicsMode := SetGraphicsMode(FContext.GetDC, GM_ADVANCED);
        GetWorldTransform(FContext.GetDC, vTmpMatrix);
        try
          vDrawMatrix := DrawMatrix;
          FillChar(vMatrix, SizeOf(vMatrix), 0);
          vImageView.EX := Ort(vImageEnt.UVector);
          vImageView.EY := Ort(vImageEnt.VVector);
          vImageView.EZ := Ort(sgFunction.Vector(vImageView.EX, vImageView.EY));
          vImageView.E0 := cnstFPointZero;
          C := FPointXMat(GetCenterOfRect(vImageEnt.Box), vDrawMatrix);
          vDrawMatrix := FMatXMat(vImageView, vDrawMatrix);
          vMatrix.eM11 := vDrawMatrix.V[0].V[0];
          vMatrix.eM12 := vDrawMatrix.V[0].V[1];
          vMatrix.eM21 := vDrawMatrix.V[1].V[0];
          vMatrix.eM22 := vDrawMatrix.V[1].V[1];
          vMatrix.eDx := C.X - 0.5 * vImageEnt.Width * vMatrix.eM11 - 0.5 * vImageEnt.Height * vMatrix.eM21;
          vMatrix.eDy := C.Y - 0.5 * vImageEnt.Width * vMatrix.eM12 - 0.5 * vImageEnt.Height * vMatrix.eM22;
          SetWorldTransform(FContext.GetDC, vMatrix);
          vImageRect := Rect(0, Round(vImageEnt.Height), Round(vImageEnt.Width), 0);
          FContext.Rect := vImageRect;
          DoDrawImageGDI;
        finally
          SetWorldTransform(FContext.GetDC, vTmpMatrix);
          SetGraphicsMode(FContext.GetDC, vGraphicsMode);
        end;
      end
      else
{$ENDIF}
{$ENDIF}
        ExpPolylineInteger(nil, FContext.IntPoints);
    end;
  end;
  FContext.IntPoints.Count := 0;
{$IFNDEF SG_FIREMONKEY}
  finally
    if Assigned(vGraphic) then
    begin
      vGraphic.Transparent := vTransparent;
      if vGraphic.InheritsFrom(Graphics.TBitmap) then
        Graphics.TBitmap(vGraphic).TransparentColor := vTransparentColor;
    end;
  end;
{$ENDIF}
end;

{$IFDEF SG_USEGDIPLUS}
procedure TsgCADImage.DrawTilesImage(const AGraphics: TGPGraphics;
      const AImgEnt: TsgDXFImageEnt;
      const APoints: TsgPointsListHelper;
      const ImageAttributes: TGPImageAttributes = nil);
var
  vPts: array [0..3] of TPoint;
  vImgEnt: TsgDXFImageEntAccess absolute AImgEnt;
  vDpi: TFPoint;
  I,J, IMax, JMax: Integer;
  vCountParts: TPoint;
  vSmallWidth, vSmallHeight, vSmallWidthCorrect, vSmallHeightCorrect: Integer;
  vRect: TRect;
  vP: TPoint;
  ASmall, ABitmapSmall: TPoint;
  vAngle, vAngleY: Double;
  vPoint, vPoint1, vPoint2, vPoint3, vTempPoint: TPoint;
  vH, vW: Integer;
  vBitmapSmall, vBitmapBig: TBitmap;
  vTransferImage: TsgBitmap;
  vBitmap: TsgBitmapAccess;
  vFGDIPImage: TGPImage;

  function GetCountBlocks(const AWidth, AHeight: Integer;
    const APixelFormat: Graphics.TPixelFormat): Integer;
  const
    cnstBPPs: array[Graphics.TPixelFormat] of Byte = (24,1,4,8,16,16,24,32,0);
    cnstMax = 555;
  begin
    Result := Ceil(MaxI(AWidth, AHeight) / cnstMax);
    if Result = 0 then
      Result := 1;
  end;

begin
  vPts[0] := APoints.Points[0];// GetPoint(AImgEnt.Point);
  vPts[1] := APoints.Points[1];// GetPoint(AImgEnt.Point1);
  vPts[2] := APoints.Points[2];// GetPoint(AImgEnt.Point3);
  vPts[3] := APoints.Points[3];// GetPoint(AImgEnt.Point2);

  vAngle := GetAngleByPoints(GetFPoint(AImgEnt.Point), GetFPoint(AImgEnt.Point1),
    False, fDoubleResolution ,1, -1);
  vAngleY :=  GetAngleByPoints(GetFPoint(AImgEnt.Point), GetFPoint(AImgEnt.Point2),
    False, fDoubleResolution, 1, -1);

  vW := vImgEnt.Picture.Graphic.Width;
  vH := vImgEnt.Picture.Graphic.Height;

  vDpi := MakeFPoint((DistancePoint(vPts[0],vPts[1]) / vW),
                      (DistancePoint(vPts[0],vPts[3]) / vH));

  vP := vPts[0];
  IMax := Ceil(Sqrt(GetCountBlocks(vW, vH, pf8bit)));
  JMax := IMax;

  vCountParts := Point(IMax - 1, JMax - 1);
  vSmallWidth := vW div IMax;
  vSmallHeight := vH div JMax;
  if (vW mod vSmallWidth) > 0 then
    Inc(vCountParts.X);
  if (vH mod vSmallHeight) > 0 then
    Inc(vCountParts.Y);

  vBitmap := TsgBitmapAccess(TsgBitmap.Create);
  try
    vBitmap.Assign(AImgEnt.Picture.Graphic);
    vBitmapBig := TBitmap.Create;
    try
      vBitmapBig.PixelFormat := vBitmap.PixelFormat;
      vBitmapBig.Width := vBitmap.Width;
      vBitmapBig.Height := vBitmap.Height;
      vBitmap.FROP := SRCCOPY;
      vBitmapBig.Canvas.StretchDraw(vBitmapBig.Canvas.ClipRect, vBitmap);

      for I := 0 to vCountParts.X do
      begin
        for J := 0 to vCountParts.Y do
        begin
          vRect.Left := I * vSmallWidth;
          vRect.Top := J * vSmallHeight;
          vRect.Right := vRect.Left + vSmallWidth;
          vRect.Bottom := vRect.Top + vSmallHeight;
          if I = vCountParts.X then
            vRect.Right := vRect.Left + vW - I * vSmallWidth;
          if J = vCountParts.Y then
            vRect.Bottom := vRect.Top + vH - J * vSmallHeight;

          vSmallWidthCorrect := vRect.Right - vRect.Left;
          vSmallHeightCorrect := vRect.Bottom - vRect.Top;



          ASmall :=    Point(vSmallWidth,vSmallHeight);
          ABitmapSmall :=   Point(vSmallWidthCorrect, vSmallHeightCorrect);

          // Find Point2
          vTempPoint := GetPointOnCircleI(vP, I * ASmall.X * vDpi.X, 360 - vAngle);
          vPoint2 := GetPointOnCircleI(vTempPoint, vH* vDpi.Y - J * ASmall.Y * vDpi.Y, 360 - vAngleY);
          vPoint  := GetPointOnCircleI(vPoint2, ABitmapSmall.Y * vDpi.Y, 180 - vAngleY);
          vPoint1 := GetPointOnCircleI(vPoint, ABitmapSmall.X * vDpi.X, 360 - vAngle);
          // Point3
          vPoint3 := GetPointOnCircleI(vPoint2, ABitmapSmall.X * vDpi.X, 360 - vAngle);

          vPts[0] := vPoint2;
          vPts[1] := vPoint3;
          vPts[2] := vPoint;

          vBitmapSmall := TBitmap.Create;
          try
            vBitmapSmall.PixelFormat := vBitmapBig.PixelFormat;
            vBitmapSmall.Width := vSmallWidthCorrect;
            vBitmapSmall.Height := vSmallHeightCorrect;
            BitBlt(vBitmapSmall.Canvas.Handle,
                    0, 0, vBitmapSmall.Width, vBitmapSmall.Height,
                    vBitmapBig.Canvas.Handle, vRect.Left, vRect.Top, SRCCOPY);
            vTransferImage := TsgBitmap.Create;
            try
              vTransferImage.Assign(vBitmapSmall);
              vFGDIPImage := TGPImage(TGPBitmap.Create(vTransferImage.ImageData.Info^, vTransferImage.ImageData.Data));
              try
                if ImageAttributes = nil then
                begin
                  AGraphics.DrawImage(vFGDIPImage, PGPPoint(@vPts[0]), 3,
                  0, 0,
                  vSmallWidthCorrect, vSmallHeightCorrect, UnitWorld, ImageAttributes);
                end
                else
                begin
                  AGraphics.DrawImage(vFGDIPImage, PGPPoint(@vPts[0]), 3,
                  0, 0,
                  vSmallWidthCorrect, vSmallHeightCorrect, UnitPixel, ImageAttributes);
                end;
              finally
                vFGDIPImage.Free;
              end;
            finally
              vTransferImage.Free;
            end;
          finally
            vBitmapSmall.Free;
          end;
        end;
      end;
    finally
      vBitmapBig.Free;
    end;
  finally
    vBitmap.Free;
  end;
end;
{$ENDIF}

{$IFNDEF SG_FIREMONKEY}
procedure TsgCADImage.DrawImageGDIPlus(const AImgEnt: TsgDXFImageEnt;
  const APoints: TsgPointsListHelper);
{$IFDEF SG_USEGDIPLUS}
type
  PGPMonochromeColorPalette = ^TGPMonochromeColorPalette;
  TGPMonochromeColorPalette = packed record
    Pal: TColorPalette;
    Color2: ARGB;
  end;

  function GetColorModeGdip(const AC1, AC2: ARGB): Integer;
  begin
      Result := GetPalColorMode(AC1 and $00FFFFFF, AC2 and $00FFFFFF)
  end;
{$ENDIF}
var
  vImgEnt: TsgDXFImageEntAccess absolute AImgEnt;
{$IFDEF SG_USEGDIPLUS}
  vPts: array [0..3] of TPoint;
  vGraphics: TGPGraphics;
  vAttr: TGPImageAttributes;
  vH, vW: Integer;
  vTransparentColor: Cardinal;
  vColorPalette, vSaveColorPalette: TGPMonochromeColorPalette;
  vMode, vColorMode, vColorPen, vBackground: Integer;
{$ENDIF}
begin
  if FExpProcs.Active then
  begin
    FExpProcs.DoImageUV(APoints.Points[0], APoints.Points[1],
      APoints.Points[2], APoints.Points[3], TGraphic(AImgEnt.Picture.Graphic));
  end
  else
  begin
{$IFDEF SG_USEGDIPLUS}
    if vImgEnt.FGDIPImage = nil then
      vImgEnt.GDIPCreate;
    if vImgEnt.FGDIPImage <> nil then
    begin
      vPts[0] := APoints.Points[3];
      vPts[2] := APoints.Points[0];
      vPts[1] := AddPoint(APoints.Points[1], SubPoint(vPts[0], vPts[2]));
      vGraphics := FContext.CreateGDIPGraphic;
      try
        SetGPGraphicsParams(vGraphics, cnstImageQuality);
        try
          vMode := Ord(vImgEnt.IsMonoChrome or IsAutocadFormat) or (Ord(vImgEnt.Transparency) shl 1);
          if (vImgEnt.Picture.Graphic is TsgBitMap) and ((vMode <> 0) or
             (TsgBitmapAccess(vImgEnt.Picture.Graphic).FROP <> SRCCOPY)) then
          begin
            case vMode of
              1, 3:
                begin
                  vImgEnt.FGDIPImage.GetPalette(@vSaveColorPalette.Pal, SizeOf(vSaveColorPalette));
                  vColorPalette.Pal.Flags := Ord(PaletteFlagsGrayScale);
                  vColorPalette.Pal.Count := 2;
                  vColorMode := GetColorModeGdip(vSaveColorPalette.Pal.Entries[vColorPalette.Pal.Count - 2],
                    vSaveColorPalette.Pal.Entries[vColorPalette.Pal.Count - 1]);
                  if vColorMode > 0 then
                  begin
                    vBackground := BackgroundColor;
                    if vColorMode > 3 then
                      vColorPen := FColorPen
                    else
                    begin
                      if BackgroundColor = clBlack then
                        vColorPen := clWhite
                      else
                        vColorPen := clBlack;
                    end;
                    if vColorMode and 2 <> 0 then
                      SwapInts(vColorPen, vBackground);
                    vColorPalette.Pal.Entries[vColorPalette.Pal.Count - 2] := ColorRefToARGB(vColorPen);
                    vColorPalette.Pal.Entries[vColorPalette.Pal.Count - 1] := ColorRefToARGB(vBackground);
                    vImgEnt.FGDIPImage.SetPalette(@vColorPalette.Pal);
                  end;
                  vTransparentColor := vColorPalette.Pal.Entries[vColorPalette.Pal.Count - 1 - Cardinal(vColorMode and 2 <> 0)];
                end;
              2://vImgEnt.Transparency
                begin
                  vTransparentColor := ColorRefToARGB(vImgEnt.TransparentColor);
                end;
            else//TsgBitmapAccess(vImgEnt.Picture.Graphic).FROP <> SRCCOPY)
              vTransparentColor := ColorRefToARGB(vImgEnt.TransparentColor);
            end;
            try
              vAttr := TGPImageAttributes.Create;
              try
                vAttr.SetColorKey(Cardinal(vTransparentColor), Cardinal(vTransparentColor), ColorAdjustTypeBitmap);
                vW := vImgEnt.Picture.Graphic.Width;
                vH := vImgEnt.Picture.Graphic.Height;
                if vGraphics.DrawImage(vImgEnt.FGDIPImage, PGPPoint(@vPts[0]), 3, 0, 0, vW, vH,
                  {$IFDEF SG_WINAPI_GDIPLUS}UnitPixel, {$ENDIF}vAttr) = OutOfMemory then
                begin
                  DrawTilesImage(vGraphics, vImgEnt, APoints, vAttr);
                end;
              finally
                vAttr.Free;
              end;
            finally
              if vMode in [1, 3] then //dxf/dwg; change IsAutocadLType to any specific
                vImgEnt.FGDIPImage.SetPalette(@vSaveColorPalette.Pal);
            end;
          end
          else
            begin
              if vGraphics.DrawImage(vImgEnt.FGDIPImage, PGPPoint(@vPts[0]), 3) = OutOfMemory then
              begin
                DrawTilesImage(vGraphics, vImgEnt, APoints);
              end;
            end;
        except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
        end;
      finally
        vGraphics.Free;
      end;
    end
    else
{$ENDIF}
      FContext.DoPolylineList(APoints);
  end;
end;
{$ENDIF}

procedure TsgCADImage.FillFromFlat(FP: TsgFlatEntity);
begin
  if FContext.Poly.Capacity < FP.Points.Count then
   FContext.Poly.Capacity := FP.Points.Count;
  FContext.Poly.Count := 0;
  DoAppendPolyFromFlat(FP);
end;

procedure TsgCADImage.DrawFlatPoly(Sender: TObject);
var
  FP: TsgFlatPoly absolute Sender;
  vdX, vdY: Double;

  procedure DrawFlatPolyExt;
  var
    I, J, C: Integer;
  begin
    FContext.IntPoints.Count := 0;
    J := 0;
    for I := 0 to FP.Counts.Count-1 do
    begin
      for C := 1 to FP.Counts[I] do
      begin
        FContext.AddIntPoint(FP.XY[J]);
        Inc(J);
      end;
      ExpPolylineInteger(Sender, FContext.IntPoints);
      FContext.IntPoints.Count := 0;
    end;
  end;

  procedure FillFromFlatExt(FP: TsgFlatEntity);
  var
    I, vIndex, vCounterLines, vCounterInitPoints, vCounterFinalPoints: Integer;
    P, P0, vPixel: TPoint;
    PF, PFNew, PFOld: TFPoint;
    vIsMissedVertex: Boolean;


      procedure AddPoint(APoint: TPoint);
      begin
        FContext.Poly.Add(APoint.X);
        FContext.Poly.Add(APoint.Y);
        Inc(vCounterFinalPoints);
      end;

      procedure AddPointF;
      begin
        PF := PFNew;
        P := GetPoint(PF);
        AddPoint(P);
        if not IsDisableSnapMatrix then
          SetPixelSnapEx(PF, P, [osEndPt]);
      end;

      procedure AddPoly;
      begin
        FContext.Counts.Add(vCounterFinalPoints);
        AddLastPixelToPoly(True, FContext.Counts);
        vCounterFinalPoints := 0;
      end;

   begin
    if FContext.Poly.Capacity < FP.Points.Count then
      FContext.Poly.Capacity := FP.Points.Count;
    FContext.Poly.Count := 0;
    vCounterInitPoints := 0;     //evg
    vCounterFinalPoints := 0;
    vCounterLines := 0;
    vPixel.X := MaxInt;
    vPixel.Y := MaxInt;
    //evg packing
    if FP.PCount > 0 then
    begin
      PF := FP.XY[0];
      PFNew := PF;
      AddPointF;
      Inc(vCounterInitPoints);
    end;
    vIsMissedVertex := False;
    for I := 1 to FP.PCount - 1 do
    begin
     vIndex := I + I;
     PFNew.X := Single(FP.Points[vIndex]);
     PFNew.Y := Single(FP.Points[vIndex + 1]);
     PFNew.Z := 0;
     if (Abs(PFNew.X - PF.X) > vdX)
       or (Abs(PFNew.Y - PF.Y) > vdY) then
     begin
        if vIsMissedVertex then   // add last missed point if it is really should be in other pixel than the last added-pixel. the same is in DrawPointsListByPolyline()
        begin
          P0 := GetPoint(PFOld);
          if not IsEqualPoints(P, P0) then
            AddPoint(P0);
        end;
       if vCounterInitPoints > 0 then // add additional points to current poly
         AddPointF
       else // current point is from another poly, check for merging
         begin
           if (vCounterFinalPoints = 1) and (FPenWidth > 1) then // point
           begin
             AddPoint(P);
           end;
           AddPoly;
           AddPointF;
         end;
       vIsMissedVertex := False;
     end
     else
     begin
       vIsMissedVertex := True;
       PFOld := PFNew;
     end;
      Inc(vCounterInitPoints);
      if vCounterInitPoints = Integer(FP.Counts[vCounterLines]) then
      begin
      vCounterInitPoints := 0;
      Inc(vCounterLines);
      end;
    end;
    if (vCounterFinalPoints = 1) and (FPenWidth > 1) then // point
      AddPoint(P);
    AddPoly;
  end;

begin
  FContext.Counts.Count := 0;
  FContext.Poly.Count := 0;
  GetPixelSize(1.5, vdX, vdY, True);
  if Sender is TsgFlatPoly3D then
  begin
    DoAppendPolyFromFlat(FP);
    DrawFlatPolyExt;
  end
  else
  begin
    FillFromFlatExt(FP);
    ExpPolyPolylineLists(Sender, FContext.Poly, FContext.Counts.List, FContext.Counts.Count);
  end;
end;

procedure TsgCADImage.DrawFlatHatch(Sender: TObject);
var
  FH: TsgFlatHatch absolute Sender;
  procedure DrawHatchExt;
  var
    PI: PsgIntegerArray;
    PP: PPoint;
    I, vCounts, vBoundariesCount, vBoundariesPointsCount: Integer;
  begin
    PP := PPoint(FContext.Poly.List);
    PI := PsgIntegerArray(FH.Counts.List);
    I := 0;
    vCounts := 1;
    while I < FH.Counts.Count do
    begin
      vBoundariesCount := FH.Counts[I];
      Inc(I);
      if vBoundariesCount = 0 then
        Exit;
      Inc(PInteger(PI), vCounts);
      ExpPolyPolygonInteger(Sender, PP, PInteger(PI), vBoundariesCount);
      Inc(I, vBoundariesCount);
      vBoundariesPointsCount := FH.Counts[I];
      Inc(I);
      Inc(PP, vBoundariesPointsCount);
      vCounts := 2 + vBoundariesCount;
    end;
  end;
begin
  DoSetColor(FColor, ctBrush, 3);
  FillFromFlat(FH);
  DrawHatchExt;
end;

procedure TsgCADImage.DrawShape(Sender: TObject);
begin
  if not IsBadRect(TsgDXFEntity(Sender).Box) then
    DrawText(Sender);
end;

procedure TsgCADImage.DrawText(Sender: TObject);
var
  P: TFPoint;
  S: string;
  vScale: Double;
  vRect: TRect;
  vBox: TFRect;
  vP: TPoint;
  vMatrix: TFMatrix;
  vSender: TsgDXFTextAccess;
  vPrevDraw: TsgCADIterate;
  vIsDrawOnCanvas, vIsDrawOnBitMap: Boolean;
  vFontStyles: TmvFontStyles;
  vPTextParams: PsgExpTextParam;
  vTextParams: TsgExpTextParam;
  vSenderFont: {$IFNDEF SG_FIREMONKEY}{$IFDEF USE_SG_AGG2D}TAggLCLFont{$ELSE}TmvExtFont{$ENDIF}{$ELSE}TFont{$ENDIF};
  vTextGlyph: TsgTextGlyphAccess;
  vNoSHXLines: Boolean;
  vSHXText2dLinesCollection: TsgText2dLinesCollection;
{$IFNDEF SG_FIREMONKEY}
  vFontHeight: Extended;
  vFontAngle: Extended;
{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
  vFontStyle: TmvFontStyles;
{$ENDIF}

  // entropy correction
  function _VectorLen(const V: array of Double; const AScale: Extended): Extended;
  begin
    try
      Result := Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]);
      Result := Result * Sqr(AScale);
      Result := Sqrt(Result);
    except
      Result := 0;
    end;
  end;

  procedure InitTextPatams(var ATextParams: TsgExpTextParam; ATextGlyph: TsgTextGlyph);
  begin
    //Result := Round(DistanceFVector(vMatrix.EY));
    ATextParams.HKoef := ATextGlyph.HKoef;
  end;

  procedure TxtOut;
  var
{$IFNDEF SG_FIREMONKEY}
    vPts: array[0..3] of TFPoint;
    vIntPts: array[0..3] of TPoint;
{$ENDIF}
    vUnicodeText: WideString;
    vLinetoPoint: TPoint;
//{$IFNDEF SGDEL_6}
    vTextFlags: Integer;
//{$ENDIF}
    vRawStr: sgRawByteString;
    vTextItems: TStringBuilder;

    procedure DrawTextExt;
    var
      vGlyphBox: TFRect;
      vPtsList: TsgIntegerList;
      vDeltaWidth: Double;
      I: Integer;
{$IFNDEF USE_SG_AGG2D}
//      vContentEnc: IEncoding;
{$ENDIF}

      procedure InitPts(AIndex: Integer; ABox: PFRect);
      var
        vWidthCur: Integer;
        Pt1, Pt2: TFPoint;
        vWidth: Double;
      begin
        Pt1 := FPointXMat(MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z1), vMatrix);
        Pt2 := FPointXMat(ABox.BottomRight, vMatrix);
        vWidth := DistanceFPoint(Pt1, Pt2);
        vWidthCur := Floor(vWidth);
        vDeltaWidth := vDeltaWidth + vWidth - vWidthCur;
        if vDeltaWidth > 0.99 then
        begin
          Inc(vWidthCur);
          vDeltaWidth := vDeltaWidth - 1;
        end;
        vPtsList[AIndex] := vWidthCur;
      end;

    begin
//      vRect.TopLeft := GetPoint(vBox.TopLeft);
//      vRect.BottomRight := GetPoint(vBox.BottomRight);
      if (Length(vUnicodeText) > 0) and (Integer(vUnicodeText[1]) shr 8 >= $50) then
      begin
{$IFNDEF USE_SG_AGG2D}
//        vContentEnc := EncMgr.FromChar(Ord(vUnicodeText[1]));
//        if not vContentEnc.IsSingleByte then
//          vSenderFont.Charset := vContentEnc.Charset;
{$ENDIF}
        InitTextPatams(vTextParams, vTextGlyph);
        DoSetFont(TFont(vSenderFont));
      end;
      DoSetColor(FColor, ctFont, 3);
       // FCanvas.Font.Height := Round(vRect.Top - vRect.Bottom);
      vPtsList := TsgIntegerList.Create;
      try
        vDeltaWidth := 0;
        if Length(vUnicodeText) = 0 then
        begin
          vPtsList.Count := Length(S);
          for I := 1 to vPtsList.Count do
          begin
            vGlyphBox := vTextGlyph.GetBox(S[I], cnstIdentityMat, nil, TsgDXFText(Sender).Tracking);
            InitPts(I - 1, @vGlyphBox);
          end;
          vRect.TopLeft := GetPoint(vBox.TopLeft);
          vRect.BottomRight := GetPoint(vBox.BottomRight);
{$IFDEF UNICODE}
          ExpTextInteger({$IFNDEF SG_FIREMONKEY}ttTextOutW{$ELSE}ttExtTextOutW{$ENDIF}, Round(P.X), Round(P.Y), vTextFlags,
            @vRect, PWideChar(S), nil, Length(S), @vPtsList.List[0], vPTextParams);
{$ELSE}
          ExpTextInteger(ttExTextOut, Round(P.X), Round(P.Y), vTextFlags,
            @vRect, nil, PAnsiChar(S), Length(S), @vPtsList.List[0], vPTextParams);
{$ENDIF}
        end
        else
        begin
          vPtsList.Count := Length(vUnicodeText);
          for I := 1 to vPtsList.Count do
          begin
            vGlyphBox := vTextGlyph.GetBoxW('', vUnicodeText[I], cnstIdentityMat, nil, TsgDXFText(Sender).Tracking);
            InitPts(I - 1, @vGlyphBox);
          end;
          vRawStr := '';
          if vSender.UseDefaultFontName or (FContext.CharSet = SYMBOL_CHARSET) then
          begin
            I := 1;
            while (I <= Length(vUnicodeText)) and (Ord(vUnicodeText[I]) <= 255) do Inc(I);
            Dec(I);
            if I = Length(vUnicodeText) then
            begin
              SetLength(vRawStr, Length(vUnicodeText));
              for I := 1 to Length(vUnicodeText) do
                Byte(PAnsiChar(vRawStr)[I-1]) := Ord(vUnicodeText[I]);
              if FContext.CharSet <> SYMBOL_CHARSET then
                FContext.CharSet := ANSI_CHARSET;// or Converter.Charset if not default??
            end;
          end;
          vRect.TopLeft := GetPoint(vBox.TopLeft);
          vRect.BottomRight := GetPoint(vBox.BottomRight);
          if (Length(vRawStr) = Length(vUnicodeText)){$IFDEF UNICODE}and TEncoding.Default.IsSingleByte{$ENDIF} then
            ExpTextInteger(ttExtTextOutA, Round(P.X), Round(P.Y), vTextFlags,
              @vRect, nil, PAnsiChar(vRawStr), Length(vRawStr),
              @vPtsList.List[0], vPTextParams)
          else
            ExpTextInteger(ttExtTextOutW, Round(P.X), Round(P.Y), vTextFlags,
              @vRect, PWideChar(vUnicodeText), '', Length(vUnicodeText),
              @vPtsList.List[0], vPTextParams);
        end;
      finally
        vPtsList.Free;
      end;
    end;

  begin
    vTextFlags := 0;
{$IFNDEF SG_NON_WIN_PLATFORM}
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      vUnicodeText := vSender.UnicodeText;
{$ELSE}
  {$IFDEF SGFPC}
    vUnicodeText := vSender.UnicodeText;
  {$ELSE}
    vUnicodeText := '';
  {$ENDIF}
{$ENDIF}
    vTextItems := CreateTextItems(S, vUnicodeText);
    try
    {$IFNDEF SG_FIREMONKEY}
      DoSetColor(FColor, ctBrushClear);
    {$ENDIF}
      if not FExportToDXFMode and Assigned(vTextGlyph) then
      begin
  {$IFNDEF SG_FIREMONKEY}
        if not IsIterator and (Abs(FContext.FontHeight) < 2) then
        begin
          if IsBadRect(vBox) then Exit;
          vBox := vTextGlyph.GetBoxW(S, vUnicodeText, cnstIdentityMat, nil, TsgDXFText(Sender).Tracking);
          vPts[0] := FPointXMat(vBox.TopLeft, vMatrix);
          vPts[1] := FPointXMat(MakeFPoint(vBox.Right, vBox.Top, 0), vMatrix);
          vPts[2] := FPointXMat(vBox.BottomRight, vMatrix);
          vPts[3] := FPointXMat(MakeFPoint(vBox.Left, vBox.Bottom, 0), vMatrix);
          vIntPts[0] := Point(Round(vPts[0].X), Round(vPts[0].Y));
          vIntPts[1] := Point(Round(vPts[1].X), Round(vPts[1].Y));
          vIntPts[2] := Point(Round(vPts[2].X), Round(vPts[2].Y));
          vIntPts[3] := Point(Round(vPts[3].X), Round(vPts[3].Y));
          if vIsDrawOnBitMap then
            FContextSelectionMatrix.DoPolygon(@vIntPts[0], Length(vIntPts));
          if vIsDrawOnCanvas then
          begin
            DoSetColor(FColor, ctBrush, 3);
            FContext.DoPolygon(@vIntPts[0], Length(vIntPts));
          end;
        end
        else
  {$ENDIF}
        begin
          InitTextPatams(vTextParams, vTextGlyph);
  {$IFNDEF SG_NON_WIN_PLATFORM}
  {$IFNDEF USE_SG_AGG2D}
          vSenderFont.Width := 0;
          vSenderFont.HandleNeeded;
  {$ENDIF}
  {$ENDIF}
          DoSetFont(TFont(vSenderFont));
          ApplyText;
          DoSetColor(FColor, ctFont, 3);
          if vIsDrawOnBitMap then
          begin
            FContextSelectionMatrix.PenWidth := cnstPenWidthNull;
            FContextSelectionMatrix.BrushColor := FContextSelectionMatrix.PenColor;
            FContextSelectionMatrix.Changed;
            vFontStyles := vSender.Properties.FontStyle;
            vTextGlyph.DrawText(FContextSelectionMatrix, vTextItems, fmUnderline in vFontStyles, fmStrikeOut in vFontStyles, vMatrix, vSender.Tracking, 2);
            SelectionMatrix.Brush.Style := bsClear;
          end;
          if vIsDrawOnCanvas then
          begin
            case TTFMode of
              ttfGDI:
                begin
                  DrawTextExt;
                  if FExpProcs.Active then
                    ExpTextInternal(Sender,vTextGlyph, vTextItems, vMatrix, 4);
                end;
              ttfPolyPolyline:    ExpTextInternal(Sender,vTextGlyph, vTextItems, vMatrix, 1);
              ttfPolyPolygon:     ExpTextInternal(Sender, vTextGlyph, vTextItems, vMatrix, 2);
              ttfPolyFillAndEdge: ExpTextInternal(Sender, vTextGlyph, vTextItems, vMatrix, 3);
            else
              if IsRotatedText(vSender, vSenderFont.{$IFDEF USE_SG_AGG2D}AggHeight{$ELSE}Height{$ENDIF}, vMatrix) then
                ExpTextInternal(Sender, vTextGlyph, vTextItems, vMatrix, {$IFDEF SG_FIREMONKEY}3{$ELSE}2{$ENDIF})
              else
              begin
                DrawTextExt;
                if FExpProcs.Active then
                 ExpTextInternal(Sender,vTextGlyph, vTextItems, vMatrix, 4);
              end;
            end;
            if not FExpProcs.Active then
              FColorBrush := FContext.BrushColor; //evg vTextGlyph.DrawTextExtW changes FCanvas.Brush.Color
          end;
        end;
      end
      else
      begin
        if (Abs(vSenderFont.{$IFDEF USE_SG_AGG2D}AggHeight{$ELSE}Height{$ENDIF}) < 2) and not FExportToDXFMode and not IsIterator then
        begin
          if IsBadRect(vBox) then Exit;
  //        vRect.TopLeft := GetPoint(vBox.TopLeft);
  //        vRect.BottomRight := GetPoint(vBox.BottomRight);
          if vRect.Left > vRect.Right then
            SwapInts(vRect.Left, vRect.Right);
          if vRect.Top > vRect.Bottom then
            SwapInts(vRect.Top, vRect.Bottom);
          vP := Point(Round(P.X), Round(P.Y));
  {$IFNDEF SG_NON_WIN_PLATFORM}
          vLinetoPoint.X := Round(vP.X + (vRect.Right-vRect.Left)*Cos(vSenderFont.{$IFNDEF USE_SG_AGG2D}Escapement*0.1{$ELSE}AggAngle{$ENDIF}));
          vLinetoPoint.Y := Round(vP.Y - (vRect.Bottom-vRect.Top)*Sin(vSenderFont.{$IFNDEF USE_SG_AGG2D}Escapement*0.1{$ELSE}AggAngle{$ENDIF}));
  {$ELSE}
          vLinetoPoint.X := vP.X + (vRect.Right-vRect.Left);
          vLinetoPoint.Y := vP.Y + (vRect.Bottom-vRect.Top);
  {$ENDIF}
          if vIsDrawOnBitMap then
          begin
  {$IFNDEF SG_NON_WIN_PLATFORM}
            if vSenderFont.{$IFNDEF USE_SG_AGG2D}Escapement*0.1{$ELSE}AggAngle{$ENDIF} <> 0 then
              FContextSelectionMatrix.Line(vP.X, vP.Y, vLinetoPoint.X, vLinetoPoint.Y);
  {$ELSE}
            FContextSelectionMatrix.Rectangle(vP.X, vP.Y, vLinetoPoint.X, vLinetoPoint.Y);
  {$ENDIF}
          end;
          if vIsDrawOnCanvas then
          begin
  {$IFNDEF SG_NON_WIN_PLATFORM}
            if vSenderFont.{$IFNDEF USE_SG_AGG2D}Escapement*0.1{$ELSE}AggAngle{$ENDIF} <> 0 then
              FContext.Line(vP.X, vP.Y, vLinetoPoint.X, vLinetoPoint.Y);
  {$ELSE}
            FContext.Rectangle(vP.X, vP.Y, vLinetoPoint.X, vLinetoPoint.Y);
  {$ENDIF}
          end;
        end
        else
        begin
          if vIsDrawOnBitMap then
          begin
            if Length(vUnicodeText) = 0 then
              FContextSelectionMatrix.DoTextOut(Round(P.X), Round(P.Y), S)
            else
              FContextSelectionMatrix.DoTextOutW(Round(P.X), Round(P.Y), vUnicodeText);
          end;
          if vIsDrawOnCanvas then
          begin
            DoSetColor(FColor, ctFont, 3);
            if Length(vUnicodeText) = 0 then
  {$IFDEF UNICODE}
              ExpTextInteger(ttTextOutW, Round(P.X), Round(P.Y), 0, nil, PWideChar(S), nil, Length(S), nil, vPTextParams)
  {$ELSE}
              ExpTextInteger(ttTextOut, Round(P.X), Round(P.Y), 0, nil, nil, PAnsiChar(S),  Length(S), nil, vPTextParams)
  {$ENDIF}
            else
              ExpTextInteger(ttTextOutW, Round(P.X), Round(P.Y),0, nil, PWideChar(vUnicodeText), '', Length(vUnicodeText),
               nil, vPTextParams);
          end;
        end;
      end;
    finally
      vTextItems.Free;
    end;
  end;

  procedure TextDrawByPolyPolyline(const ACollection: TsgText2dLinesCollection);
  var
    vPts: {$IFDEF SGFPC}sgConsts.{$ENDIF}PPoints;
    I, Cnt, C: Integer;
  begin
    if ACollection.AsPolyPolyline then
    begin
      Cnt := ACollection.Counts.Count;
      if (Cnt > 0) and (ACollection.Counts.Last < 2) then//fix  GetSHXLinesEx
        Dec(Cnt);
      if NeedToAddOnePixel then
      begin
        vPts := {$IFDEF SGFPC}sgConsts.{$ENDIF}PPoints(FContext.Poly.List);
        for I := 0 to Cnt - 1 do
        begin
          C := ACollection.Counts[I];
          if (C = 2) and IsEqualPoints(vPts^[0], vPts^[1]) then
            Dec(vPts^[1].X);
          Inc(PPoint(vPts), C);
        end;
      end;
      ExpPolyPolylineLists(vSender, FContext.Poly, ACollection.Counts.List, Cnt);
    end
    else
    begin
      FContext.IntPoints.Count := 0;
      vPts := {$IFDEF SGFPC}sgConsts.{$ENDIF}PPoints(FContext.Poly.List);
      for I := 0 to ACollection.Counts.Count - 1 do
      begin
        Cnt := ACollection.Counts.List[I];
        if (Cnt = 2) and IsEqualPoints(vPts^[0], vPts^[1]) then
          ExpPixelInteger(Sender, vPts^[0], FColor)
        else
        begin
          FContext.IntPoints.AppendArray(PsgIntegerArray(vPts)^, Cnt * 2);
          ExpPolylineInteger(vSender, FContext.IntPoints);
          FContext.IntPoints.Count := 0;
        end;
        Inc(PPoint(vPts), Cnt);
      end;
    end;
  end;

  procedure DoAnnotation;
  var
    P: TsgExpAnnotParam;
  begin
    if Assigned(FExpProcs.ExpAnnotation) then
    begin
      P.Size := SizeOf(P);
      P.Subtype := atSquare;
      P.Open := False;
      P.Flags := 64;
      P.Title := 'AutoCAD SHX Text';
      P.Rect := vRect;
      FExpProcs.DoAnnotation(vSender, P);
    end;
  end;

begin
  if not FTextVisible then
    Exit;
  vNoSHXLines := False;
  vSender := TsgDXFTextAccess(Sender);
  vMatrix := FMatXMat(vSender.GetMatrix, FDraw.Matrix);
  vSenderFont := nil;
  try
  vTextGlyph := TsgTextGlyphAccess(TsgDXFStyleAccess(vSender.Properties).FFontGlyphRef);
  if not Assigned(vTextGlyph) then
    vTextGlyph := TsgTextGlyphAccess(ContainerOfTextGlyphs.Defaults[MVFontStylesToFontStyles(vSender.Properties.FontStyle)]);
{$IFNDEF SG_FIREMONKEY}
  vFontHeight := vTextGlyph.HKoef * _VectorLen(FDraw.Matrix.EY.V, vSender.Height);
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  vSenderFont := TsgTextGlyphAccess(vTextGlyph).CreateFont(_VectorLen(FDraw.Matrix.EY.V, vSender.Height));
  vSenderFont.Color := FColorFont;
{$ELSE}
  vSenderFont := {$IFDEF USE_SG_AGG2D}TAggLCLFont{$ELSE}TmvExtFont{$ENDIF}.Create;
  vFontStyle := FontStylesToMVFontStyles(vTextGlyph.FontStyle) + vSender.Properties.FontStyle;
{$IFDEF USE_SG_AGG2D}
  vSenderFont.AllocateResources(FCanvas);
  vSenderFont.Name := vTextGlyph.FontName;
  vSenderFont.LoadFromFile(vTextGlyph.GetFileName, vFontHeight, fmBold in vFontStyle, fmItalic in vFontStyle);
  //vSenderFont.Bold := fmBold in vFontStyle;
  //vSenderFont.Italic := fmItalic in vFontStyle;
  vSenderFont.Underline := fmUnderline in vFontStyle;
  vSenderFont.StrikeThrough := fmStrikeOut in vFontStyle;
  //vSenderFont.AggHeight := vFontHeight;
  vSenderFont.Color := FColorFont;
{$ELSE}
  vSenderFont.Name := vTextGlyph.FontName;
  vSenderFont.Style := vFontStyle;
  vSenderFont.Charset := vTextGlyph.Charset;
  vSenderFont.Height := Round(vFontHeight);
{$ENDIF}
{$ENDIF}
  vPTextParams := nil;
  if FExpProcs.Active then
  begin
    vTextParams.Height := vTextGlyph.HKoef * _VectorLen(FDraw.Matrix.EY.V, vSender.Height);
    vTextParams.HKoef := 1;
    vTextParams.Scale := vSender.Scale;
    vTextParams.Draw := vMatrix;
    vPTextParams := @vTextParams;
  end;

  vIsDrawOnBitMap := IsDrawOnBitMap;
  vIsDrawOnCanvas := IsDrawOnCanvas;
  if vIsDrawOnBitMap then
  begin
    AddSelectionMatrix(Sender);
  {$IFNDEF SG_FIREMONKEY}
    SelectionMatrix.FontColor := FContextSelectionMatrix.PenColor;
  {$ELSE}
    SelectionMatrix.FontColor := FContextSelectionMatrix.BrushColor;
  {$ENDIF}
    FContextSelectionMatrix.Changed;
  end;
  vPrevDraw := FDraw;
  try
    P := FPointXMat(vSender.StartPoint, FDraw.Matrix);
    S := vSender.Text;
    vScale := vSender.Scale;
    vBox := vSender.Box;
    if (vScale = 0) or vSender.IsEmpty then
      Exit;
    DoScale2D(FDraw);
    if FDraw.XScale = 0 then Exit;
{$IFNDEF SG_FIREMONKEY}
    if not FExpProcs.Active then
      vSenderFont.PixelsPerInch := FContext.PixelsPerInch;
{$ENDIF}
//    vSenderFont.Height :=  Abs(Round(fWinTextHeightFactor * Abs(vSender.Height) * FHCoef));
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFNDEF USE_SG_AGG2D}
    vSenderFont.Width := Abs(Round(0.64 * Abs(vSender.Height) * vScale * FHCoef));
{$ENDIF}
{$ENDIF}
    if vSenderFont.{$IFDEF USE_SG_AGG2D}AggHeight{$ELSE}Height{$ENDIF} = 0 then
      vSenderFont.{$IFDEF USE_SG_AGG2D}AggHeight{$ELSE}Height{$ENDIF} := 1;
{$IFNDEF SG_NON_WIN_PLATFORM}
    if (TTFMode = ttfGDI) or FExportToDXFMode then
    begin
      if Sender is TsgDXFAttdef then
        vFontAngle := vSender.Rotation
      else
        vFontAngle := vSender.Rotation - FAngle;
{$IFDEF USE_SG_AGG2D}
      vSenderFont.AggAngle := vFontAngle;
{$ELSE}
      vSenderFont.Escapement := Round(10 * vFontAngle);
      vSenderFont.HandleNeeded;
{$ENDIF}
    end;
{$ENDIF}
//    evg:
    if vIsDrawOnCanvas then
      DoSetFont(TFont(vSenderFont));
    ApplyText;
    vRect.TopLeft := GetPoint(vBox.TopLeft);
    vRect.BottomRight := GetPoint(vBox.BottomRight);
    if Converter.UseSHXFonts and (not vSender.WinFont) then
    begin
      //FIntPoints.Count := 0;
      FContext.Poly.Count := 0;
      if FExpProcs.Active then
        vSHXText2dLinesCollection := TsgText2dLinesCollection.Create(FContext.Poly, False)
      else
        vSHXText2dLinesCollection := TsgText2dLinesCollectionGDI.Create(FContext.Poly, False);
      try
        vSHXText2dLinesCollection.UpdateTransformation(FDraw.Matrix, {$IFDEF HAS_UNMANAGED_TYPEINFO}TypeInfo(TFMatrix){$ELSE}SizeOf(TFMatrix){$ENDIF});
        vSHXText2dLinesCollection.AsPolyPolyline := True;
        vSender.GetSHXLinesEx(SHXFontsProc, vSHXText2dLinesCollection, not FExpProcs.Active, @FDraw);
        if FContext.Poly.Count > 0 then
        begin
          if FExpProcs.Active then
            DoAnnotation;
          DoSetColor(FColor, ctPen, 3);
          if not FDraw.Stopped then
            TextDrawByPolyPolyline(vSHXText2dLinesCollection);
        end
        else
        begin
          vNoSHXLines := IsBadRect(vBox);//TxtOut;
          if not vNoSHXLines then
            TxtOut;
        end;
      finally
        //FIntPoints.Count := 0;
        FreeAndNil(vSHXText2dLinesCollection);
      end;
    end
    else
      TxtOut;
  finally
    vPrevDraw.Stopped := FDraw.Stopped;
    FDraw := vPrevDraw;
    if (not FExpProcs.Active) and vIsDrawOnCanvas then
      FPenWidth := FContext.PenWidth; // evg: text draw in ttf.pas changes pen.width
  end;
  finally
    vSenderFont.Free;
  end;
  if vNoSHXLines then
  begin
    if not vSender.WinFont then
    begin
      vSender.WinFont := True;
      vSender.LoadedInternal(FConverter);
    end;
    DrawText(Sender);
  end;
end;

procedure TsgCADImage.EndRead(Sender: TObject);
begin
  if not Assigned(OnProgress) then
    Exit;
  DoProgress(psEnding);
  FMsg := sLoadEntities;
end;

procedure TsgCADImage.EnterInsert(Sender: TObject);
{$IFDEF SG_BTI}
  procedure DrawVertex(const APoint: TFPoint; const ASize: Integer; AMode: Integer);
  var
    vPoints: TsgIntegerList;
    P: TPoint;
    R: TRect;
    vColor: TColor;
  begin
    vPoints := nil;
    try
      P := GetPoint(APoint);
      R := Rect(P.X - ASize, P.Y + ASize, P.X + ASize, P.Y - ASize);
      case AMode of
        1:
        begin
          vColor := clBlack;
          if (FBackgroundColor and $FFFFFF) = clBlack then
            vColor := clWhite;
          DoSetColor(vColor, ctPen);
          DoSetColor(vColor, ctBrush);
          FContext.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        end;
        2:
        begin
          vColor := clRed;
          if FDrawMode = dmGray then
            vColor := ConvertColortoGray(vColor);
          DoSetColor(vColor, ctPen);
          DoSetColor(vColor, ctBrush);
          FContext.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        end;
        3:
        begin
          vColor := clBlack;
          if (FBackgroundColor and $FFFFFF) = clBlack then
            vColor := clWhite;
          DoSetColor(vColor, ctPen);
          DoSetColor((not vColor) and $FFFFFF, ctBrush);
          FContext.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        end;
      end;
      if Assigned(vPoints) then
        ExpPolylineInteger(Sender, vPoints);
    finally
      FreeAndNil(vPoints);
    end;
  end;

  procedure DrawAreaVertexMode(const AInsAccess: TsgDXFInsertAccess);
  var
    I: Integer;
    vAttrib: TsgDXFAttrib;
    vPoint: TPoint;
    vSize: Integer;
  begin
    if not AInsAccess.GetVertexModeInsp then
      Exit;
    vSize := cnstSnapMarkerSize div 2;
    for I := 0 to AInsAccess.Count - 1 do
    begin
      vAttrib := TsgDXFAttrib(AInsAccess.Attribs[I]);
      if (vAttrib.TypeValue = atVertex) and (TsgAttrib(vAttrib).LinkState >= 0) then
        DrawVertex(vAttrib.Point, vSize, TsgAttrib(vAttrib).LinkState);
    end;
  end;

  procedure DrawConstruction(const AIns: TsgDXFInsertAccess);
  begin
    if Assigned(AIns.Layer) and ((not AIns.Layer.Visible) or AIns.Layer.Frozen) then
       Exit;
    case AIns.GetEntTypeEx of
      cnstArea:
        begin
          DrawAreaVertexMode(AIns);
        end;
    end;
  end;
{$ENDIF}
var
  Ins: TsgDXFInsert absolute Sender;
  vFilter: TObject;
  vRegion: TsgRegion;
begin
{$IFDEF SG_BTI}
  if (TsgDXFEntityAccess(Ins).GetEntTypeEx > 0) and IsDrawOnCanvas then
    DrawConstruction(TsgDXFInsertAccess(Ins));
{$ENDIF}
  if FOnInsert = Sender then
    FOnInsertMode := False;
  if HasClip(Ins, vFilter) then
  begin
    ExpRestoreDCInternal(nil, 0);
    if not FExpProcs.Active then
    begin
      vRegion := TsgRegion(FRegionStack.Top);
      if not Assigned(vRegion) then
        vRegion := TsgRegion(FCanvasRegion);
      if IsDrawOnBitMap then
      begin
        FContextSelectionMatrix.RegionSelect(vRegion);
        UpdateClientClipRect(FContextSelectionMatrix.ClipRect);
      end;
      if not FSnapOnlyIterate and IsDrawOnCanvas then
        SetSelectClipRegion(vRegion);
    end;
  end;
end;

procedure TsgCADImage.EnterComplexEntity(Sender: TObject);
begin
  if FOnComplexEntity = Sender then
    FOnComplexEntity := nil;
end;

procedure TsgCADImage.EnterViewport(Sender: TObject);
begin
  if not TsgDXFViewport(Sender).IsDraw  then
    Exit;
  if FViewPortCanvasParams <> nil then
  begin
    ExpRestoreDCInternal(Sender, FViewPortCanvasParams^.Index); //RestoreDC(FCanvas.Handle, FViewPortCanvasParams^.HDC);
    DoSetColor(FViewPortCanvasParams^.CanvasParams.Brush.Color, ctBrush);
    DoSetColor(FViewPortCanvasParams^.CanvasParams.Pen.Color, ctPen);
    DoSetColor(FViewPortCanvasParams^.CanvasParams.Font.Color, ctFont);
    SetPenWidth(1);
    FViewPortCanvasParams^.CanvasParams.Brush.Free;
    FViewPortCanvasParams^.CanvasParams.Font.Free;
    FViewPortCanvasParams^.CanvasParams.Pen.Free;
    if not FExpProcs.Active then
    begin
      FContext.RegionSelect(TsgRegion(FViewPortCanvasParams^.CanvasParams.Region));
      UpdateClientClipRect(FContext.ClipRect);
    end;
    FViewPortCanvasParams^.CanvasParams.Region.Free;
    Dispose(FViewPortCanvasParams);
    FViewPortCanvasParams := nil;
    if IsDrawOnBitMap then
      SelectionMatrix.RestoreClipRGN;
  end;
end;

procedure TsgCADImage.EnterXRef(Sender: TObject);
var
  vItem: TsgXrefStackItem;
  vXrefCadImage: TsgCADImage;
begin
  FSHXFontsProc := GetSHXFontsProc;
  FGetSelectionMatrixModeProc := GetSelectionMatrixProc;
  FSetSelectionMatrixModeProc := SetSelectionMatrixProc;
  if FXrefStack.Count > 0 then
  begin
    vItem := TsgXrefStackItem(FXrefStack.Last);
    try
      FXrefStack.Last := nil;
      FXrefStack.Count := FXrefStack.Count - 1;
      if Assigned(vItem.XRef) then
      begin
        if TsgDXFXRef(vItem.XRef).CADImage <> nil then//see DrawXRef
        begin
          vXrefCadImage := TsgCADImage(TsgDXFXRef(vItem.XRef).CADImage);
          TsgDXFConverterAccess(vXrefCadImage.Converter).PHeadVarStruct^.LwDisplay := vItem.LwDisplay;
        end;
      end;
    finally
      vItem.Free;
    end;
  end;
end;

procedure TsgCADImage.EntityCreated(Sender: TObject);
// Called from Converter after each entity is loaded from DXF
// In the previous versions a drawing procedure was assigned to the entity here
begin
  // SetDraw(TsgDXFEntity(Sender));
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

function TsgCADImage.EntVisible(const AEnt: TsgDXFEntity): Boolean;
begin
  Result := AEnt.Visible;
  if Result then
  begin
    case AEnt.EntType of
      cePoint:
        Result := Converter.PointDisplayMode <> 1;
      ceAttdef, ceAttrib:
        Result := not (Converter.HeadVarStruct.AttMode = attDisable);
      ce3dFace:
        Result := not (TsgDXF3dFaceAccess(AEnt).Flags = $F);
    end;
  end;
end;

function TsgCADImage.EntVisibleByLayer(E: TsgDXFEntity; Ins: TsgDXFInsert; ARealVis: Boolean): Boolean;

  function IsAnnotative(AIns: TsgDXFInsert; ABlockRecordStack: TsgList): Boolean;
  var
    C: Integer;
  begin
    Result := False;
    if Assigned(AIns) and (AIns.EntType = ceInsert) then
    begin
      Result := AIns.Annotative;
      if not Result then
      begin
        C := ABlockRecordStack.Count;
        ABlockRecordStack.Add(AIns.BlockRecord);
        if ABlockRecordStack.Count > C then
        try
          Result := IsAnnotative(AIns.OwnerInsert, ABlockRecordStack);
        finally
          ABlockRecordStack.Remove(AIns.BlockRecord);
        end;
      end;
    end;
  end;

var
  L: TsgDXFLayer;
  vIsPlotting: Boolean;
begin
  Result := E.Visibility and (not IsBadRect(E.Box));
  if not Result then Exit;
  L := EntLayer(E, Ins);
  if L = nil then Exit;
  Result := Assigned(FDraw.Viewport) or not IsAnnotative(Ins, FAnnotativeBlockRecordStack);
  if not Result then Result := not CurrentLayout.IsLayerInVisible(L,Converter,True);
  if not Result then Exit;
  if ARealVis then
    Result := L.Visible // True, if need check insert visibility
  else
    Result := L.Visible or E.IsInsert;
  vIsPlotting := IsPlotting or (Assigned(FExpProcs) and FExpProcs.Active and FExportUseLayerPlotting);
  Result := Result and (not vIsPlotting or (vIsPlotting and L.IsPlotting)); //evg: what does it mean???
  if L.Frozen or L.IsFrozenByViewPort then
    Result := False;   // why here?
  if E.EntType = cePoint then
  begin
    Result := Result and (FConverter.PointDisplayMode <> 1);
    if Ins is TsgDXFDimension then
      Result := Result and EntVisibleByLayer(Ins, Ins.OwnerInsert, True); //evg
  end;
end;

procedure TsgCADImage.GetAngles(const AImageEnt: TsgDXFImageEnt; var Angle1,
  Angle2: Double; const Abs: Boolean = False);
var
  vP1, vP2, vP3: TFPoint;
begin
  if Abs then
  begin
    vP1 := AImageEnt.Point;
    Angle1 := GetAngleByPoints(vP1, AImageEnt.Point1, False, fDoubleResolution);
    Angle2 := GetAngleByPoints(vP1, AImageEnt.Point2, False, fDoubleResolution);
  end
  else
  begin
    vP1 := GetFPoint(AImageEnt.Point);
    vP2 := GetFPoint(AImageEnt.Point1);
    vP3 := GetFPoint(AImageEnt.Point2);
    Angle1 := GetAngleByPoints(vP1, vP2, False, fDoubleResolution ,1, -1);
    Angle2 := GetAngleByPoints(vP1, vP3, False, fDoubleResolution, 1, -1);
  end;
end;

function TsgCADImage.GetAlternateWhite: Boolean;
begin
  Result := False;
end;

function TsgCADImage.GetAlternateWhiteColor: TColor;
begin
  Result := clBlack;
end;

function TsgCADImage.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

function TsgCADImage.GetBlock(Index: Integer): TsgDXFBlock;
begin
  Result := FConverter.Blocks[Index];
end;

function TsgCADImage.GetEntities(Index: Integer): TsgDXFEntity;
begin
  Result := FConverter.Entities[Index];
end;

function TsgCADImage.GetIsLoading: Boolean;
begin
  Result := {$IFDEF SG_OPENING_IN_THEADS}FThreadId <> 0{$ELSE}Loading <> nil{$ENDIF};
end;

function TsgCADImage.GetIsShowBackground: Boolean;
begin
  Result := not GetTransparent;
end;

function TsgCADImage.GetIsShowLineWeight: Boolean;
begin
  Result := Converter.HeadVarStruct.LwDisplay = 1;
end;

function TsgCADImage.GetIterator: Boolean;
begin
  Result := FExpProcs.Active;
end;

function TsgCADImage.GetLayout(Index: Integer): TsgDXFLayout;
begin
  Result := FConverter.Layouts[Index];
end;

function TsgCADImage.GetLayoutsCount: Integer;
begin
  Result := FConverter.LayoutsCount;
end;

function TsgCADImage.GetLineWeightScale: TsgFloat;
begin
  Result := FLineWeightScale;
end;

function TsgCADImage.GetOwnSource: Boolean;
begin
  Result := Converter.OwnSource;
end;

function TsgCADImage.GetRect(const ARect: TFRect): TRect;
  procedure ExpRect(const X, Y, Z: TsgFloat);
  begin
    ExpandRect(Result, GetPoint(MakeFPoint(X, Y, Z)));
  end;
begin
  Result := Classes.Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
  ExpRect(ARect.Left, ARect.Top, ARect.Z1);
  ExpRect(ARect.Left, ARect.Top, ARect.Z2);
  ExpRect(ARect.Left, ARect.Bottom, ARect.Z1);
  ExpRect(ARect.Left, ARect.Bottom, ARect.Z2);
  ExpRect(ARect.Right, ARect.Top, ARect.Z1);
  ExpRect(ARect.Right, ARect.Top, ARect.Z2);
  ExpRect(ARect.Right, ARect.Bottom, ARect.Z1);
  ExpRect(ARect.Right, ARect.Bottom, ARect.Z2);
end;

procedure TsgCADImage.SetOwnSource(Value: Boolean);
begin
  Converter.OwnSource := Value;
end;

procedure TsgCADImage.Set3DRotDef(const ABox: TFRect; var ANeed2DViewByDefault: Boolean);
begin
  ANeed2DViewByDefault := IsZero(ABox.Z1) and IsZero(ABox.Z2);
  if not ANeed2DViewByDefault then
    RotToView(GetDefSolidRotView);
end;

procedure TsgCADImage.SetAlternateWhite(const AValue: Boolean);
begin
end;

procedure TsgCADImage.SetAlternateWhiteColor(const AValue: TColor);
begin
end;

procedure TsgCADImage.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    FChangeBackgroundColor := GetColorByBackgroud(FBackgroundColor);
{$IFDEF SG_FIREMONKEY}
    ClearImagesCache;
{$ENDIF}
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetBorderSize(const Value: TsgBorderSize);
begin
  FBorderSize := Value;
end;

procedure TsgCADImage.SetBorderType(const Value: TsgBorderType);
begin
  FBorderType := Value;
end;

procedure TsgCADImage.SetPlotWindowAreaColor(Value: TColor);
begin
  if FPlotWindowAreaColor <> Value then
  begin
    FPlotWindowAreaColor := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetPrinting(const AValue: TsgPrintMode);
begin
  FPrinting := AValue;
end;

procedure TsgCADImage.SetDefaultColor(Value: TColor);
begin
  if FDefaultColor <> Value then
  begin
    FDefaultColor := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetDefaultPlotSettings(const ACheckSize: Boolean = False;
  const ANeedSaveSize: Boolean = False);
begin
end;

procedure TsgCADImage.SetDrawingMatrixEx(AValue: TFMatrix);
var
  vScale: TFPoint;
  vRotMatrix: TFMatrix;
begin
  vScale := ExtractMatrixAbsoluteScale(AValue);
  vScale.Y := -vScale.Y;
  vRotMatrix := FMatScale(AValue, ReverseScale(vScale));
  vRotMatrix.E0 := cnstFPointZero;

  CurrentLayout.SetRotMatrix(vRotMatrix);
  SetExtentsParameters(CurrentLayout.Box, CurrentLayout.CADSpace <> csUndefined);
  SetDrawMatrix(AValue);
end;

procedure TsgCADImage.DoAfterRotate;
begin
  Dec(FRotCount);
  if FRotCount < 0 then FRotCount := 0;
  if FRotCount = 0 then
  begin
    RefreshCurrentLayout;
    if Assigned(FOnAfterRotate) then
      FOnAfterRotate(Self);
  end;
end;

procedure TsgCADImage.DoBeforeRotate;
begin
  Inc(FRotCount);
  if FRotCount = 1 then
    if Assigned(FOnBeforeRotate) then
      FOnBeforeRotate(Self);
end;

procedure TsgCADImage.DoCreateExternalRegion(const ACombineRegion: TObject);
var
  vCombineRegion: TsgRegion absolute ACombineRegion;
  I, J, vRegionCount, vInflateExternalRegion: Integer;
  vGroup:  TsgDXFGroup;
  vRegion, vMainRegion: TsgRegion;
  vEnt: TsgDXFEntity;
  vPoly: TsgCADBasePolyline;
  vColor: TColor;
begin
  vInflateExternalRegion := 0;
  if BorderType = btPixel then
    vInflateExternalRegion := Round(BorderSize);
  vGroup := nil;
  if FExternalRegion is TsgDXFInsert then
    vGroup := TsgDXFInsert(FExternalRegion).Block
  else
    if FExternalRegion is TsgDXFGroup then
      vGroup := TsgDXFGroup(FExternalRegion);
  if Assigned(vGroup) then
  begin
    vRegionCount := 0;
    vMainRegion := TsgRegion.Create;
    try
//      if Assigned(vCombineRegion) then
//        vMainRegion.Combine(vCombineRegion, RGN_AND);
      for I := 0 to vGroup.Count - 1 do
      begin
        FContext.Poly.Count := 0;
        vEnt := vGroup.Entities[I];
        if vEnt is TsgCADBasePolyline then
        begin
          vPoly := TsgCADBasePolyline(vEnt);
          for J := 0 to vPoly.PointCount - 1 do
            FContext.AddPolyPoint(vPoly.Points[J]);
          if vPoly.Closed and (vPoly.PointCount > 1) then
            FContext.AddPolyFirstPoint(0);
        end;
        if FContext.Poly.Count > 0 then
        begin
          Inc(vRegionCount);
          vRegion := TsgRegion.Create;
          try
            if vInflateExternalRegion = 0 then
              vRegion.SetPolygon(FContext.Poly.List^, FContext.Poly.Count shr 1, GetPolygonFillingMode)
            else
              InflateRegion(vRegion, FContext.Poly, GetPolygonFillingMode, vInflateExternalRegion);
            vMainRegion.Exclude(vRegion);
          finally
            vRegion.Free;
          end;
        end;
      end;
      if vRegionCount > 0 then
      begin
        if FExternalRegion.Visibility then
        begin
          FTmpEntColor := GetEntityColors(FExternalRegion, nil);
          vColor := FTmpEntColor.DrawColor;
          DoSetColor(vColor, ctBrush, 3);
          FContext.BrushStyle := bsFDiagonal;
          try
            FContext.RegionPaint(vMainRegion);//?????
          finally
            FContext.BrushStyle := bsClear;
          end;
        end
        else
        begin
          if FExpProcs.Active then
             ExpClipRegionInternal(nil, vMainRegion)
          else
            FContext.RegionSelect(vMainRegion);
        end;
      end;
    finally
      vMainRegion.Free;
      FContext.Poly.Count := 0;
    end;
  end;
end;

function TsgCADImage.DoDraw(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
  //최승선 수정
  // 블록인지 확인
  case Entity.EntType of
    ceLine:              DrawLine(Entity);
    cePoint:             DrawPoint(Entity);
    ceSolid, ceTrace:    DrawSolid(Entity);
    ceCircle:            DrawCircle(Entity);
    ceArc, ceEllipse:    DrawArc(Entity);
    ceLWPolyline,
      cePolyline,
      cePath:            DrawPoly(Entity);
     ceSpline, ceHelix:   DrawSpline(Entity);
    ceLeader:            DrawLeader(Entity);
    ceDimension:         Result := DrawDimension(Entity);
    ceACADTable,
      ceInsert,
      ceMText,
      ceMLine,
      ceTolerance:       Result := DrawInsert(Entity);
    ceText,
      ceAttdef,
      ceAttrib:          DrawText(Entity);
    ceShape:             DrawShape(Entity);
    ce3dFace:            DrawFace(Entity);
    cePolyPolygon,
      ceGradient,
      ceGradientPolygon,
      ceCurvePolygon,
      ceHatch:           DrawHatch(Entity);
    ceMPolygon:          DrawMPolygon(Entity);
    ceImageEnt:          DrawImageEnt(Entity);
    ceViewport:          DrawViewPort(Entity);
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
    ceRegion, ceBody, ceSurface, ce3DSolid:
                         DrawACISEntity(Entity);
    ceOle2Frame:         DrawOle2Frame(Entity);
    ceFlatPoly,
    ceFlatPoly3D:        DrawFlatPoly(Entity);
    ceFlatHatch:         DrawFlatHatch(Entity);
    ceXRef:              DrawXRef(Entity);
    ceWipeout:           DrawWipeout(Entity);
    ceProxy, ceMLeader:  DrawComplexEntity(Entity);
    ceMesh:              Result := DrawMesh(Entity);
{$IFDEF SG_BTI}
    cePolyPolyline2D:   DrawPolyPolyLine2D(Entity);
{$ENDIF}
    ceRay, ceXline:      DrawLine(Entity);
  else
    if Assigned(FOnDrawCustomEntity) then
      FOnDrawCustomEntity(Entity);
  end;
end;

{$IFDEF SG_DELPHI_VCL}
procedure TsgCADImage.DoMetafileExport(const MC: TMetafileCanvas;
  const ARect: TRect);
begin
  if Assigned(FOnMetafileExport) then
    FOnMetafileExport(MC, ARect);
end;
{$ENDIF}

procedure TsgCADImage.DoOnProgress(Stage: TProgressStage; Done, Count: Integer);
const
  vRect: TRect = (Left:0; Top:0; Right:0; Bottom:0);
begin
  Progress(Self, Stage, Round(100.0 * Done / Count), False, vRect, '');
end;

function TsgCADImage.DoFinish(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
  case Entity.EntType of
    ceDimension, ceInsert,
      ceMText:             EnterInsert(Entity);
    ceViewport:            EnterViewport(Entity);
    ceXRef:                EnterXRef(Entity);
    ceProxy, ceMLeader, ceMesh:
                           EnterComplexEntity(Entity);
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
    ceRegion, ceBody, ceSurface, ce3DSolid:
                           EnterComplexEntity(Entity);
  end;
end;

procedure TsgCADImage.DoDrawAfter;
begin
  if Assigned(FOnAfterDraw) then
    FOnAfterDraw(Self);
  if Assigned(FAfterDrawNotifications) then
    FAfterDrawNotifications.Proc(nil);
end;

function TsgCADImage.DoDrawBackground(const ARect: TRect): Integer;
var
  vBackGroundRect: TRect;
begin
  Result := 0;
  if Assigned(FOnDrawBackground) then
    Result := FOnDrawBackground(CurrentLayout);
  if Result = 0 then
  begin
    vBackGroundRect := ARect;
{$IFDEF SG_USE_AGG2D_AS_GDI}
    Dec(vBackGroundRect.Left);
    Dec(vBackGroundRect.Top);
{$ELSE}
    Inc(vBackGroundRect.Right);
    Inc(vBackGroundRect.Bottom);
{$ENDIF}
    FContext.FillRect(vBackGroundRect);
    Result := 1;
  end;
end;

procedure TsgCADImage.DoDrawBefore({const ACanvas: TCanvas; const ARect: TRect});
begin
  if Assigned(FOnBeforeDraw) then
    FOnBeforeDraw(Self);
  if Assigned(FBeforeDrawNotifications) then
    FBeforeDrawNotifications.Proc(nil);
end;

procedure TsgCADImage.DoIterateAfter;
begin
  if Assigned(FAfterIterateNotifications) then
    FAfterIterateNotifications.Proc(nil);
end;

procedure TsgCADImage.DoIterateBefore;
begin
  if Assigned(FBeforeIterateNotifications) then
    FBeforeIterateNotifications.Proc(nil);
end;

procedure TsgCADImage.DrawOnSelMatrixLine(const Sender: TObject);
var
  vLine: TsgDXFLine absolute Sender;
begin
  if not IsDrawOnBitMap then Exit;
  FContext.AddIntPointDirect(GetPoint(vLine.Point));
  FContext.AddIntPointDirect(GetPoint(vLine.Point1));
  if NeedToAddOnePixel then
    FContext.AddIntLastPoint;

  AddSelectionMatrix(Sender);
  FContextSelectionMatrix.Changed;
  FContextSelectionMatrix.DoPolylineList(FContext.IntPoints);

  FContext.IntPoints.Count := 0;
end;

//procedure TsgCADImage.DoExpPolygon(const Points: array of TPoint);
//begin
//  FExpProcs.DoPolygonArray(Points);
//end;
//
//procedure TsgCADImage.DoExpPolyline(const Points: array of TPoint);
//begin
//  FExpProcs.DoPolylineArray(Points);
//end;

procedure TsgCADImage.DoMatrixChanged;
begin
  if Assigned(FOnMatrixChanged) then
    FOnMatrixChanged(Self);
end;

procedure TsgCADImage.SetDrawingBox(const ABox: TFRect);
begin
  FDrawingBox := ABox;
  if IsBadRect(FDrawingBox) then
  begin
    if FCurrentLayout <> nil then
      SetExtentsParameters(FCurrentLayout.Box, FCurrentLayout.CADSpace <> csUndefined)
    else
    begin
      if Converter.LayoutsCount <= 0 then
        Converter.AddLayout(TsgDXFLayout.Create);
      FCurrentLayout := Converter.Layouts[0];
      SetExtentsParameters(MakeFRect(0,0,0,0,0,0), False);
    end;
  end
  else
    SetExtentsParameters(FDrawingBox, True);
  Changed(Self);
end;

procedure TsgCADImage.SetDrawMatrix(const AMatrix: TFMatrix);
begin
  FDraw.Matrix := AMatrix;
  FDraw.DrawMatrix := AMatrix;
  FDraw.Additional := Ord(IsRotatedFMat(FDraw.Matrix));
  if not FCustomDraw then
  begin
    DoMatrixChanged;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetDrawMode(Value: TsgDXFDrawMode);
begin
  if Value <> FDrawMode then
  begin
    FDrawMode := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetIsShowBackground(const AValue: Boolean);
begin
  SetTransparent(not AValue);
end;

procedure TsgCADImage.SetIsShowLineWeight(Value: Boolean);
begin
  if IsShowLineWeight <> Value then
  begin
    TsgDXFConverterAccess(Converter).PHeadVarStruct.LwDisplay := Ord(Value);
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetIsWithoutBorder(Value: Boolean);
begin
  FIsWithoutBorder := Value;
  if FCurrentLayout <> nil then
    RefreshCurrentLayout;
end;

procedure TsgCADImage.SetIterator(const Value: Boolean);
begin
  FExpProcs.Active := Value;
end;

procedure TsgCADImage.SetRegenDelta(const Value: Double);
begin
  FRegenDelta := Value;
end;

procedure TsgCADImage.UpdateClientClipRect(const ARect: TRect);
var
  R: TRect;
begin
  if not FExpProcs.Active and not FContext.IsMetafileCanvas then
  begin
    R.Left := 0;
    R.Top := 0;
    R.BottomRight := FContext.Size;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    DPtoLP(FContext.GetDC, R, 2);
{$ENDIF}
    IntersectRect(FClientClipRect, R, ARect);
  end
  else
    FClientClipRect := ARect;
end;

procedure TsgCADImage.SetRegenerateArcs(const AValue: Boolean);
begin
  FConverter.RegenerateArcs := AValue;
  Regenerate;
end;

procedure TsgCADImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetStretchBltModeInt(const AMode: Integer);
begin
  FContext.SetStretchBltMode(AMode);
end;

function TsgCADImage.GetStretchBltModeInt: Integer;
begin
  Result := FContext.GetStretchBltMode;
end;

function TsgCADImage.GetUnresolvedExternalLinkNames(const AFileNames: TStrings): Integer;
begin
  Result := -1;
end;

procedure TsgCADImage.SetLibraryGDIPlusExists(const Value: Boolean);
begin
  FLibraryGDIPlusExists := Value;
end;

procedure TsgCADImage.SetLineWeightScale(const Value: TsgFloat);
begin
  FLineWeightScale := Value;
end;

function TsgCADImage.SetLineWeigth(const AEntity: TsgDXFEntity;
  const ALineWeightScaled: Boolean; AMetric: Double = 1.0): Integer;
var
  vLineWeight: Double;
begin
  Result := 0;
  DoScale2D(FDraw);
  if GetShowLineWeight then
  begin
    if sgIsZero(AMetric) then
      AMetric := 1;
    vLineWeight :=  GetLineWeightFactor * EntLineWeight(AEntity, FDraw.Insert) / AMetric;
    if ALineWeightScaled then
      vLineWeight := vLineWeight * FDraw.XScale;
    Result := Round(LineWeightScale * vLineWeight);
    if Result = 0 then
      Result := 1
    else
      Result := Abs(Result);
  end;
  SetPenWidth(Result);
end;

procedure TsgCADImage.SetListsCapacity(const ASetDefault: Boolean);
begin
  if Assigned(FContext) then
  begin
    SetListCapacity(FContext.IntPoints, ASetDefault);
    SetListCapacity(FContext.Poly,  ASetDefault);
  end;
  SetListCapacity(FDottedSingPts, ASetDefault);
end;

procedure TsgCADImage.SetUseColorRGBInColorMap(const Value: Boolean);
begin
  FUseColorRGBInColorMap := Value;
end;

procedure TsgCADImage.SetUsePlotSetting(const Value: Boolean);
begin
  FUsePlotSetting := Value;
end;

procedure TsgCADImage.SetShowPlotForModel(const Value: Boolean);
begin
  FShowPlotForModel := Value;
end;

procedure TsgCADImage.SetShowPlotFrame(const Value: Boolean);
begin
  FShowPlotFrame := Value;
end;

procedure TsgCADImage.SetShowPlotMargins(const Value: Boolean);
begin
  FShowPlotMargins := Value;
end;

procedure TsgCADImage.SetShowPlotSetting(const Value: Boolean);
begin
  FShowPlotSetting := Value;
end;

//for internal use
procedure TsgCADImage.SetSHXFontsProc(const AValue: TsgSHXFontsProc);
begin
  FSHXFontsProc := AValue;
end;

procedure TsgCADImage.SetUseWinEllipse(Value: Boolean);
begin
  if Value <> FUseWinEllipse then
  begin
    FUseWinEllipse := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.SetShowImages(Value: Boolean);
begin
  if Value <> FShowImages then
  begin
    FShowImages := Value;
    Changed(Self);
  end;
end;

procedure TsgCADImage.AfterChangeEntitiesVisible;
var
  vBox: TFRect;
begin
  vBox := cnstBadRect;
  if Assigned(CurrentLayout) and (CurrentLayout.Count = 0) then
    vBox := CurrentLayout.Box;
  try
    GetExtents;
  finally
    if not IsBadRect(vBox) then
      TsgDXFLayoutAccess(CurrentLayout).SetBox(vBox);
  end;
end;

procedure TsgCADImage.SetDimensionsVisible(AVisible: Boolean);
var
  I: Integer;
begin
  if (FDimensionsVisible <> AVisible) then
  begin
    FDimensionsVisible := AVisible;
    if Assigned(Converter) then
      for I := 0 to Converter.XRefs.Count - 1 do
        TsgCADImage(TsgDXFXRef(Converter.XRefs.Items[I]).CADImage).DimensionsVisible := AVisible;
    AfterChangeEntitiesVisible;
  end;
end;

procedure TsgCADImage.SetTextVisible(AVisible: Boolean);
var
  I: Integer;
begin
  if (FTextVisible <> AVisible) then
  begin
    FTextVisible := AVisible;
    if Assigned(Converter) then
      for I := 0 to Converter.XRefs.Count - 1 do
        TsgCADImage(TsgDXFXRef(Converter.XRefs.Items[I]).CADImage).TextVisible := AVisible;
    AfterChangeEntitiesVisible;
  end;
end;

procedure TsgCADImage.SetTmpPlotWindow(const Value: TsgDXFPlotSettings);
begin
  FTmpPlotWindow := Value;
end;

procedure TsgCADImage.SetTTFMode(const Value: TsgTTFMode);
begin
  FTTFMode := Value;
end;

{$IFDEF SG_BTI}
procedure TsgCADImage.SetUseFillStyleIndex(const AValue: Boolean);
begin
  FUseFillStyleIndex := AValue;
end;
{$ENDIF}

procedure TsgCADImage.StopLoading;
begin
  FConverter.StopLoading;
end;

function TsgCADImage.GetNodeName: string;
begin
  Result := GetObjectXMLName(TsgCADImage.ClassName);
end;

function TsgCADImage.ToNode(const AParentNode: TsgNode;
  const AParams: TsgXMLParams): TsgNode;
begin
  Result := AParentNode.AddChildNV(GetNodeName);
  Result.AddAttribNV(cnstXMLNames[xmlName].Name).ValueAsStr := GetObjectPropertyName(Self.ClassName);
  Result.AddAttribNV(cnstXMLNames[xmlFileName].Name).ValueAsStr := FileName;
  Converter.ToNode(Result, AParams);
end;

procedure TsgCADImage.FromNode(const ANode: TsgNodeSample);
var
  vConverterNode: TsgNodeSample;
begin
  vConverterNode := ANode.GetChildByName(IsgXMLObject(Converter).GetNodeName);
  if Assigned(vConverterNode) then
    Converter.FromNode(vConverterNode);
end;
//
// Call only Iterate functional !!!
//
function TsgCADImage.IsNotScaledLType: Boolean;
begin
  Result := Assigned(FConverter.Params.Insert) and IsAutocadLType;
end;

function TsgCADImage.IsNoWidth: Boolean;
begin
  Result := FPenWidth <= 1; //evg
end;

procedure TsgCADImage.DrawOle2Frame(Sender: TObject);
var
  vRect: TF2DRect;
begin
  if (TsgDXFOle2Frame(Sender).OleObject = nil) and (TsgDXFOle2Frame(Sender).OleBitmap = nil) then Exit;
  vRect := cnstBadRectF2D;
  ExpandF2DRect(vRect, MakeF2DPointFromPoint(GetPoint(TsgDXFOle2Frame(Sender).Point)));
  ExpandF2DRect(vRect, MakeF2DPointFromPoint(GetPoint(TsgDXFOle2Frame(Sender).Point1)));
  SwapSGFloats(vRect.Top, vRect.Bottom);
  DoSetColor(FColor, ctBrush, 3);
  ExpOle2FrameInternal(Sender, vRect);
end;

//evg completely changed
procedure TsgCADImage.DrawPointsListByPolyline(const AObj: TObject;
  const DottedSingPts: TFPointList;
  AClosed: Boolean);
var
  I: Integer;
  vIsPoint, vIsMissedVertex: Boolean;
  vdX, vdY: Double; //evg
  PF: TFPoint;
begin
  if DottedSingPts.Count <= 1 then
    Exit;
  FContext.IntPoints.Count := 0;
  FContext.DotsPoints.Count := 0;
  if DottedSingPts.Count = 2 then
  begin
    vIsPoint := FContext.AddIntSegment(DottedSingPts[0], DottedSingPts[1]);
    if vIsPoint then
      ExpPixelInteger(AObj, Point(FContext.IntPoints[0], FContext.IntPoints[1]), FColor)
    else
    begin
      if NeedToAddOnePixel then  // (!!!) points changes
        FContext.AddIntLastPoint;
      ExpPolylineInteger(AObj, FContext.IntPoints);
    end;
  end
  else
  begin
    //evg packing
    GetPixelSize(0.5, vdX, vdY, True);
    if DottedSingPts.Count >0 then
    begin
      PF := DottedSingPts[0];
      FContext.AddIntPoint(PF);
    end;
    vIsMissedVertex := False;
    for I := 1 to DottedSingPts.Count - 1 do
    begin
      if  (Abs(DottedSingPts[I].X - PF.X) > vdX)
       or (Abs(DottedSingPts[I].Y - PF.Y) > vdY)
       or (Abs(DottedSingPts[I].Z - PF.Z) > vdx) then
      begin
        if vIsMissedVertex then // add last missed point if it is really should be in other pixel than the last added-pixel. the same is in DrawFlatPoly()
          FContext.AddIntPoint(DottedSingPts[I - 1], True);
        PF := DottedSingPts[I];
        FContext.AddIntPoint(PF);
        vIsMissedVertex := False;
      end
      else
        vIsMissedVertex := True;
    end;
    if AClosed then
      FContext.AddIntPoint(DottedSingPts[0])
    else
      if NeedToAddOnePixel then  // (!!!) points changes
        FContext.AddIntLastPoint;
    if FContext.IntPoints.Count > 1 * 2 then
      ExpPolylineInteger(AObj, FContext.IntPoints);
  end;
end;

//evg completely changed
procedure TsgCADImage.DrawPointsListByPolyPolyline(const AObj: TObject;
  const DottedSingPts: TFPointList; AClosed: Boolean = False);
begin
  DrawPointsListByPolyPolyline(AObj, DottedSingPts as IsgArrayFPoint, AClosed);
end;

procedure TsgCADImage.DrawPointsListByPolyPolyline(const AObj: TObject;
  const DottedSingPts: IsgArrayFPoint; AClosed: Boolean = False);

  procedure AddPtsInt(const AP1, AP2: TPoint);
  begin
    FContext.IntPoints.Add(AP1.X);
    FContext.IntPoints.Add(AP1.Y);
    FContext.IntPoints.Add(AP2.X);
    FContext.IntPoints.Add(AP2.Y);
  end;

var
  I, vCnt: Integer;
  P1, P2: TPoint;
  vCounts: TsgIntegerList;
  vdX, vdY, vLTScale: Double;
  vIsPolyline: Boolean;
  vPolyline: TsgCADBasePolyline;
  FP, vFP1, vFP2: TFPoint;

  procedure ExtendLine(CurPt: TFPoint);
  begin
    FContext.AddIntPoint(CurPt);
    vCounts.List[vCounts.Count - 1] := vCounts[vCounts.Count - 1] + 1;
    FP := CurPt;
  end;

  procedure AddLine(const FP1, FP2: TFPoint);
  var
    vInternalPoint: TPoint;
  begin
    P1 := GetPoint(FP1);
    P2 := GetPoint(FP2);
    if vCounts.Count > 0 then
    begin
      vInternalPoint := GetPoint(FP);
      if (Abs(P1.X - vInternalPoint.X) < 2) and (Abs(P1.Y - vInternalPoint.Y) < 2) then
      begin
        ExtendLine(FP2);
        Exit;
      end;
    end;

    if IsEqualPoints(P1, P2) then
    begin
      if FPenWidth > 1 then // thin dot for thick lines
      begin
        ExpPixelInteger(AObj, P1, FColor);
        Exit;
      end;
    end;
    // here we know that previous poly is finished and we add one pixel to the end
    AddLastPixelToPoly(False, vCounts);
    AddPtsInt(P1, P2);
    vCounts.Add(2);
    FP := FP2;
  end;

  function IsInOtherPixel(CurPt: TFPoint; dX, dY: Double): Boolean;
  begin
    Result := (Abs(CurPt.X - FP.X) > dX) or
      (Abs(CurPt.Y - FP.Y) > dY) or (Abs(CurPt.Z - FP.Z) > dX);
  end;

begin
  FContext.IntPoints.Count := 0;
  FContext.DotsPoints.Count := 0;
  if AObj is TsgCADBasePolyline then
  begin
    vPolyline := TsgCADBasePolyline(AObj);
    GetPixelSize(1.5, vdX, vdY, not IsAutocadLType);
    vIsPolyline := True;

    vCnt := vPolyline.PolyPoints.Count; // Points count must be even
    I := 0;
    vLTScale := FEntityLines.MaxSizeOfPatterns * FEntityLines.Scale;
    if FEntityLines.IsSolid or (vLTScale < vdX) then
    begin
      GetPixelSize(1.5, vdX, vdY, True);
      while I < vCnt - 1 do
      begin
        if Abs(vPolyline.PolyPoints[I].X - vPolyline.PolyPoints[I + 1].X) > vdX then
        begin
          vIsPolyline := False;
          Break;
        end;
        if Abs(vPolyline.PolyPoints[I].Y - vPolyline.PolyPoints[I + 1].Y) > vdY then
        begin
          vIsPolyline := False;
          Break;
        end;
        Inc(I);
      end
    end
    else
      vIsPolyline := False; //for linetypes with big PatternLength
    if vIsPolyline then
    begin
      DrawPointsListByPolyline(AObj, vPolyline.PolyPoints, AClosed);
      Exit;
    end;
  end;
  GetPixelSize(0.5, vdX, vdY, True);
  vCnt := DottedSingPts.FPointCount shr 1 shl 1; // Points count must be even
  I := 0;
  FContext.Counts.Count := 0;
  FContext.IntPoints.Count := 0;
  FP := BadRect.TopLeft;
  vCounts := TsgIntegerList.Create; //evg??? - all lists to create/destroy!
  if I < vCnt then
  begin
    AddLine(DottedSingPts.FPoints[I], DottedSingPts.FPoints[I + 1]);
    Inc(I, 2);
    while (I < vCnt - 1){$IFDEF SG_THREAD_DRAW} and not FDraw.Stopped{$ENDIF} do
    begin
      vFP1 := DottedSingPts.FPoints[I];
      if IsInOtherPixel(vFP1, vdX, vdY) then // means that new line begins in a new pixel
        AddLine(vFP1, DottedSingPts.FPoints[I + 1])
      else // it is here: PFPoint(DottedSingPts[I])^ approx= FP
      begin
        vFP2 := DottedSingPts.FPoints[I + 1];
        if IsInOtherPixel(vFP2, vdX, vdY)  then
          ExtendLine(vFP2);
      end;
      Inc(I, 2);
    end;
  end;
{$IFDEF SG_THREAD_DRAW}
  if not FDraw.Stopped then
{$ENDIF}
  begin
    AddLastPixelToPoly(False, vCounts);
    ExpPolyPolylineLists(AObj, FContext.IntPoints, vCounts.List, vCounts.Count);
  end;
  vCounts.Free;
end;

procedure TsgCADImage.DrawXRef(Sender: TObject);
var
  vXrefCadImage: TsgCADImage;
  vItem: TsgXrefStackItem;
begin
  if SelectionMatrix <> nil then
  begin
    AddSelectionMatrix(Sender);
    FContextSelectionMatrix.Changed;
  end;
  vItem := TsgXrefStackItem.Create;
  vItem.XRef := Sender;
  vItem.LwDisplay := Converter.HeadVarStruct.LwDisplay;
  FXrefStack.Add(vItem);
  vXrefCadImage := TsgCADImage(TsgDXFXref(Sender).CADImage);
  if vXrefCadImage <> nil then
  begin
    vItem.LwDisplay := vXrefCadImage.Converter.HeadVarStruct.LwDisplay;
    TsgDXFConverterAccess(vXrefCadImage.Converter).PHeadVarStruct.LwDisplay :=
      Self.Converter.HeadVarStruct.LwDisplay;
    FSHXFontsProc := vXrefCadImage.GetSHXFontsProc;
    FGetSelectionMatrixModeProc := TsgCADImage(TsgDXFXref(Sender).CADImage).GetSelectionMatrixProc;
    FSetSelectionMatrixModeProc := TsgCADImage(TsgDXFXref(Sender).CADImage).SetSelectionMatrixProc;
  end
  else
  begin
    FSHXFontsProc := GetSHXFontsProc;
    FGetSelectionMatrixModeProc := GetSelectionMatrixProc;
    FSetSelectionMatrixModeProc := SetSelectionMatrixProc;
  end;
end;

procedure TsgCADImage.TransformToUCS(var APoint: TFPoint);
var
  UCS: TFMatrix;

  function FPointXMat(const P: TFPoint; const M: TFMatrix): TFPoint;
    function Part(I: Integer): Extended;
    begin
      Result := P.X * M.M[I,0] + P.Y * M.M[I,1] + P.Z * M.M[I,2] + M.M[3,I];
    end;
  begin
    Result.X := Part(0);
    Result.Y := Part(1);
    Result.Z := Part(2);
  end;

begin
  UCS := FConverter.GetUCS(FCurrentLayout);
  APoint := FPointXMat(APoint, UCS);
end;

procedure TsgCADImage.TransformToWorldCS(var APoint: TFPoint);
var
  UCS: TFMatrix;
begin
  UCS := FConverter.GetUCS(FCurrentLayout);
end;

procedure TsgCADImage.TranslateDrawMatrix(ADx, ADy, ADz: Double);
begin
  SetDrawMatrix(FMatTranslate(DrawMatrix, ADx, ADy, ADz));
end;

class procedure TsgCADImage.UnRegisterDestroyNotification(
  const ANotify: TNotifyEvent);
begin
  if Assigned(CADImageDestroyNotifications) then
  begin
    CADImageDestroyNotifications.Remove(ANotify);
    if CADImageDestroyNotifications.Count = 0 then
      FreeAndNil(CADImageDestroyNotifications);
  end;
end;

procedure TsgCADImage.UpdateContextSelectionMatrix;
begin
  TsgContextSelection(FContextSelectionMatrix).Selection := FSelectionMatrix;
{$IFNDEF USE_SG_AGG2D}
  if Assigned(FSelectionMatrix) then
    FContextSelectionMatrix.Canvas := FSelectionMatrix.Canvas
  else
    FContextSelectionMatrix.Canvas := nil;
{$ENDIF}
end;

function TsgCADImage.InternalToCommonUnits(Pt: TFPoint): TFPoint;
begin
  Result := Pt;
end;

function TsgCADImage.CommonToInternalUnits(Pt: TFPoint): TFPoint;
begin
  Result := Pt;
end;

function TsgCADImage.Measurement: TsgMeasurement;
begin
  case Integer(Converter <> nil) shl 1 + Integer(FConverter.HeadVarStruct.Measurement) of
    2:  Result.DistanceUnits := duInch;
    3:  Result.DistanceUnits := duMM;
  else
    Result.DistanceUnits := duNone;
  end;
  Result.AngularUnits := auNone;
end;

function TsgCADImage.NeedToAddOnePixel: Boolean;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := FAddOnePixel and (Ceil(FPenWidth) < 2) and (not FExpProcs.Active);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgCADImage.SetViewportRect(const Value: TRect);
var
  vNeedResizeSnapMatrix: Boolean;
begin
  if (FViewportRect.Left <> Value.Left) or (FViewportRect.Top <> Value.Top) or
     (FViewportRect.Right <> Value.Right) or (FViewportRect.Bottom <> Value.Bottom) then
  begin
    vNeedResizeSnapMatrix := (FViewportRect.Right <> Value.Right) or (FViewportRect.Bottom <> Value.Bottom);
    FViewportRect := Value;
{$IFDEF SG_FM_LINUX}
    if not FIsPrinterCanvas then
{$ENDIF}
      if vNeedResizeSnapMatrix then
        SetupSnapMatrix;
    if Drawing <> Self then
      Modified := True;
  end;
end;

procedure TsgCADImage.InitializeVersionData(const AIsTrial: Boolean;
  const AMessage: string);
begin
  if AIsTrial then
    FMessage := AMessage
  else
    FMessage := '';
end;

procedure TsgCADImage.Fill2Counts(AMax: Integer);
begin
  FContext.Counts2.AppendConst(AMax - FContext.Counts2.Count + 1, 2);
end;

{$IFDEF SG_BTI}
procedure TsgCADImage.DrawPolyPolyLine2D(Sender: TObject);
var
  vPolyPolyLine: TsgCADPolyPolyline2D absolute Sender;
  I: Integer;
  vLine: TsgLine;
  vList: TList;
  vCounts: TsgIntegerList;
  vDotSingPts: TFPointList;
  P: PFPoint;
  vNumberPart: Integer;
  vBoundary: TF2DPointList;
begin
  vDotSingPts := vPolyPolyLine.DotSingPts;
  if vDotSingPts = nil then
  begin
    if IsDrawOnCanvas then
    begin
      FContext.IntPoints.Count := 0;
      FContext.DotsPoints.Count := 0;
      vCounts := TsgIntegerList.Create;
      try
        Fill2Counts(vPolyPolyLine.CurvesCount);
        vCounts.Capacity := vPolyPolyLine.CurvesCount;
        for I := 0 to vPolyPolyLine.CurvesCount - 1 do
        begin
          if not vPolyPolyLine.CurveVisible[I] then
            Continue;
          if vPolyPolyLine.IsCurveArc(I) then
          begin
            vNumberPart := vPolyPolyLine.GetPointsOfArc(I, nil, FContext, Converter.NumberOfPartsInCircle);
            vCounts.Add(vNumberPart + 1);
          end
          else
          begin
            vLine := vPolyPolyLine.Lines[I];
            FContext.AddIntSegment(vLine.Point1, vLine.Point2);
            vCounts.Add(2);
          end;
        end;
        ExpPolyPolylineLists(Sender, FContext.IntPoints, vCounts.List, vCounts.Count, False);
      finally
        vCounts.Free;
        FContext.IntPoints.Count := 0;
      end;
    end;
  end
  else
    DrawPointsListByPolyPolyline(Sender, vDotSingPts);
  if (not FExpProcs.Active) and IsDrawOnBitMap then
  begin
    FContext.IntPoints.Count := 0;
    FContext.Poly.Count := 0;
    AddSelectionMatrix(Sender);
    if vPolyPolyLine.Boundaries = nil then
    begin
      vList := TList.Create;
      try
        vPolyPolyLine.GetBoundary(vList);
        for I := vList.Count - 1 downto 0 do
        begin
          P := vList.List[I];
          FContext.AddIntPoint(P^);
        end;
      finally
        vList.Free;
      end;
    end
    else
    begin
      FContext.Counts.Count := 0;
      for I := 0 to vPolyPolyLine.Boundaries.Count - 1 do
      begin
        vBoundary := TF2DPointList(vPolyPolyLine.Boundaries[I]);
        FContext.Counts.Add(Load2DPointToPoints(vBoundary));
      end;
    end;
   // FContextSelectionMatrix.BrushStyle := bsSolid;
    if FContext.IntPoints.Count > 0 then
    begin
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolygonList(FContext.IntPoints);
    end;
    if FContext.Poly.Count > 0 then
    begin
      FContextSelectionMatrix.PenWidth := cnstPenWidthNull;
      FContextSelectionMatrix.Changed;
      FContextSelectionMatrix.DoPolyPolygon(FContext.Poly.List^, PInteger(FContext.Counts.List), FContext.Counts.Count);
    end;
  // FContextSelectionMatrix.BrushStyle := bsClear;
  end;
end;
{$ENDIF}

{ TsgVectorImage }

constructor TsgVectorImageWithAltWhite.Create;
begin
  inherited Create;
  FAlternateWhite := bAlternateWhite;
  FAlternateWhiteColor := cAlternateWhiteColor;
  TsgDXFConverterAccess(Converter).FreeEntitiesWithGeneratedHandles;
end;

procedure TsgVectorImageWithAltWhite.Assign(Source: TPersistent);
begin
  if Source is TsgCADImage then
  begin
    FAlternateWhite := TsgCADImage(Source).AlternateWhite;
    FAlternateWhiteColor := TsgCADImage(Source).AlternateWhiteColor;
  end;
  inherited Assign(Source);
end;

function TsgVectorImageWithAltWhite.GetAlternateWhite: Boolean;
begin
  Result := FAlternateWhite;
end;

function TsgVectorImageWithAltWhite.GetAlternateWhiteColor: TColor;
begin
  Result := FAlternateWhiteColor;
end;

procedure TsgVectorImageWithAltWhite.SetAlternateWhite(const AValue: Boolean);
begin
  if FAlternateWhite <> AValue then
  begin
    FAlternateWhite := AValue;
    inherited SetAlternateWhite(AValue);
    Changed(Self);
  end;
end;

procedure TsgVectorImageWithAltWhite.SetAlternateWhiteColor(const AValue: TColor);
begin
  if FAlternateWhiteColor <> AValue then
  begin
    FAlternateWhiteColor := AValue;
    inherited SetAlternateWhiteColor(AValue);
    Changed(Self);
  end;
end;

{ TsgVectorImageWithParser }

function TsgVectorImageWithSave.CanSaveToStream: Boolean;
begin
  Result := False;
end;

function TsgVectorImageWithSave.SaveToStream(const AStream: TStream): Integer;
begin
  Result := 0;
end;

{ TsgHandlesImage }

constructor TsgImageWithHandles.Create;
begin
  inherited;
end;

function TsgImageWithHandles.GetConverterParams: TsgConverterParams;
begin
  Result := inherited GetConverterParams;
  Result.CheckHandles := True;
  Result.UpdateEvent := DoUpdateEvent;;
end;

function TsgImageWithHandles.IsAutocadFormat: Boolean;
begin
  Result := True;
end;

procedure TsgImageWithHandles.SetDefaultViewModePlotSettings;
begin
  ShowPlotSetting := True;
  ShowPlotMargins := True;
  ShowPlotForModel := False;
end;

{ TsgVectorImage }

procedure TsgVectorImage.ConvertToBitmap(const ABitmap: TBitmap);
begin
  ABitmap.Canvas.StretchDraw(ABitmap.Canvas.ClipRect, Self);
end;

{ TsgVectorImageCustom }

procedure TsgVectorImageCustom.ApplyLineWeightFactor(var ALineWeight: Double);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

constructor TsgVectorImageCustom.Create;
begin
  inherited Create;
  TsgDXFConverterAccess(Converter).CustomEntByName(GetAppID, csAppID, True);
end;

function TsgVectorImageCustom.GetLineWeightFactor: Double;
begin
  Result := cnstDefaultLineWeightFactor;
end;

function TsgVectorImageCustom.IsImageWithColorsRGB: Boolean;
begin
  Result := True;
end;

procedure TsgVectorImageCustom.SetDefaultPlotSettings(const ACheckSize: Boolean = False;
  const ANeedSaveSize: Boolean = False);
var
  I: Integer;
  vPlotSettings: TsgDXFPlotSettings;
begin
  for I := 0 to Converter.LayoutsCount - 1 do
  begin
    vPlotSettings := Converter.Layouts[I].PlotSettings;
    if (not ACheckSize) or (vPlotSettings.PlotPaperSize.X * vPlotSettings.PlotPaperSize.Y = 0) then
      vPlotSettings.SetDefaultValueByExtents(Converter.Layouts[I].Box, ANeedSaveSize);
  end;
end;

procedure TsgVectorImageCustom.SetDefaultViewModePlotSettings;
begin
  ShowPlotSetting := False;
  ShowPlotForModel := True;
  ShowPlotMargins := True;
end;

{ TsgVectorImageWithModeller }

procedure TsgVectorImageWithModeller.AddAttrib(AInsert: TsgDXFInsert; ATag,
  AText: string);
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

procedure TsgVectorImageWithModeller.AddInfo(const AInsert: TsgDXFInsert;
  const ATag, AText: string; const AMode: Integer = 3);
begin
  if AMode and 1 <> 0 then
    AddAttrib(AInsert, ATag, AText);
  if AMode and 2 <> 0 then
    TsgDXFConverterAccess(Converter).AddEddNameValue(AInsert, ATag, AText);//call GetEddNameValue to get values
end;

constructor TsgVectorImageWithModeller.Create;
begin
  inherited Create;
  Converter.QueryModeller(True);//create modeller
end;

procedure TsgVectorImageWithModeller.UpdateFromModeller(const ALayer: TsgDXFLayer);
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  vRootCompound: TsgModPartCompound;
  vModeller: TsgModeller;
  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;
  vCollection: TsgCollection;
  vNameAttrib: TsgModAttribName;
begin
  vModeller := Modeller;
  if not Assigned(vModeller) then
    Exit;
  vRootCompound := vModeller.Root[False];
  if not Assigned(vRootCompound) then
    Exit;

  if CurrentLayout.Count = 0 then
  begin
    vNameAttrib := TsgModAttribName(vRootCompound.QueryAttrib(TsgModAttribName, False));
    vBlock := TsgDXFBlock.Create;
    vBlock.ColorCAD := cnstColorCADByBlock;
    if Assigned(vNameAttrib) then
      vBlock.Name := Converter.QueryCorrectBlockName(string(vNameAttrib.Name), cnstBrepBlock, vBlock)
    else
      vBlock.Name := Converter.QueryCorrectBlockName('', cnstBrepBlock, vBlock);
    Converter.Sections[csBlocks].AddEntity(vBlock);
    vInsert := TsgDXFInsert.Create;
    TsgDXFInsertAccess(vInsert).SceneElement := 1;
    vInsert.Block := vBlock;
    vInsert.ColorCAD := cnstColorCADByBlock;

    if Assigned(vNameAttrib) then
      AddInfo(vInsert, cnstBrepOriginalName, vNameAttrib.Name)
    else
      AddInfo(vInsert, cnstBrepOriginalName, GetUnNamedPart(vRootCompound, True));
    vCollection := TsgCollection.Create;
    try
      ExtractPartCompound( vRootCompound, vCollection, vBlock, ALayer);
    finally
      vCollection.Free;
    end;

    Converter.Loads(vBlock);
    vInsert.Layer := ALayer;
    Converter.DoCreate(vInsert);
    Converter.Loads(vInsert);
    Converter.Layouts[0].AddEntity(vInsert);
  end;
{$ELSE}
begin
{$ENDIF}
end;

class function TsgVectorImageWithModeller.CreateFromModeller(const AModeller: TsgModeller): TsgCADImage;
var
  v3DImage: TsgVectorImageWithModeller;
  vLayer: TsgDXFLayer;
begin
  Result := nil;
  v3DImage := TsgVectorImageWithModeller.Create;
  if Assigned(v3DImage) then
  begin
{$IFNDEF SG_NO_USE_KERNEL3D}
    TsgDXFConverterAccess(v3DImage.Converter).SetModellerLinker(
      TsgModellerLinker.Create(AModeller, False));
{$ENDIF}      
    v3DImage.Converter.InitializeSectionsBegin;
    v3DImage.Converter.InitializeSectionsEnd;
    v3DImage.CurrentLayout := v3DImage.Converter.Layouts[0];
    v3DImage.Transparent := True;
    v3DImage.BorderType := btGlobal;
    v3DImage.BorderSize := 0;
    v3DImage.IsWithoutBorder := True;

    vLayer := v3DImage.Converter.LayerByName('0');
    vLayer.ColorCAD := ConvertARGBToColorCAD(clGray);

    v3DImage.UpdateFromModeller(vLayer);

    v3DImage.GetExtents;
    Result := v3DImage;
  end;
end;

procedure TsgVectorImageWithModeller.ExtractPartCompound(
  ACompound: TsgModPartCompound; ACollection: TsgCollection;
  ABlock: TsgDXFBlock; const ALayer: TsgDXFLayer);
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  I, J: Integer;
  vInsert: TsgModPartInstance;
  vInsertDXF: TsgDXFInsert;
  vColorAttrib: TsgModAttribColorBase;
  vNameAttrib: TsgModAttribName;

  vEntity:  TsgDXFEntity;
  vCompound: TsgModPartCompound;
  vHashItem: TsgHashItem;
  vBlock: TsgDXFBlock;
  vMatrix: TsgMatrix4d;
  vFMatrix: TFMatrix;
  vTransform: TsgTransformations;
  vPoint, vScale,
  vExtrusion: TFPoint;
  vAngle: Double;

begin
  //Part
  for I := 0 to ACompound.InsertCount - 1 do
  begin
    vInsert := ACompound.Insert[I];
    vCompound := vInsert.Compound;

    vNameAttrib := TsgModAttribName(vCompound.QueryAttrib(TsgModAttribName, False));
    J := ACollection.IndexOf(TsgNativeUINT(vCompound));
    if J >= 0 then
    begin
      vHashItem := ACollection[J];
      vBlock := TsgDXFBlock(vHashItem.Data);
    end
    else
    begin
      vBlock := TsgDXFBlock.Create;
      vBlock.ColorCAD := cnstColorCADByBlock;
      if Assigned(vNameAttrib) then
        vBlock.Name := Converter.QueryCorrectBlockName(string(vNameAttrib.Name), cnstBrepBlock, vBlock)
      else
        vBlock.Name := Converter.QueryCorrectBlockName('', cnstBrepBlock, vBlock);
      Converter.Sections[csBlocks].AddEntity(vBlock);
      ACollection.Add(TsgNativeUINT(vCompound), vBlock);
      ExtractPartCompound(vCompound, ACollection, vBlock, ALayer);
    end;

    vInsertDXF := TsgDXFInsert.Create;
    TsgDXFInsertAccess(vInsertDXF).SceneElement := 1;
    vInsertDXF.Block := vBlock;

    vColorAttrib := vCompound.QueryColorAttrib;
    if Assigned(vColorAttrib) then
      vInsertDXF.ColorCAD := MakeColorCAD(acRGBColor, TsgBrepModEntity.FindColor(vColorAttrib))
    else
      vInsertDXF.ColorCAD := cnstColorCADByBlock;

    if Assigned(vNameAttrib) then
      AddInfo(vInsertDXF, cnstBrepOriginalName, vNameAttrib.Name)
    else
      AddInfo(vInsertDXF, cnstBrepOriginalName, GetUnNamedPart(vCompound));

    if vInsert.HasTransformation then
    begin
      vMatrix := vInsert.FullMatrix; //!!
      vFMatrix := sgMFunctions.Matrix434To34(vMatrix);
      ExtractMatrixParams(vFMatrix, vPoint, vScale, vExtrusion, vAngle);

      sgMFunctions.MatrixDecompose(vMatrix, vTransform);
      vInsertDXF.Point := vTransform.Translate;
      vScale.X := sqrt(vTransform.Scale.X);
      vScale.Y := sqrt(vTransform.Scale.Y);
      vScale.Z := sqrt(vTransform.Scale.Z);
      //vExtrusion := sgMFunctions.DirectionTransform(ZVector3d, vMatrix);
      //sgMFunctions.NormalizeVector(vExtrusion);
      //vAngle := RadToDeg(vTransform.Rotate.Z);

      vInsertDXF.Point := vPoint;
      vInsertDXF.Scale := vScale;
      vInsertDXF.Extrusion := vExtrusion;
      vInsertDXF.Angle := vAngle;
      vInsertDXF.Scale := vScale;
    end;

    ABlock.AddEntity(vInsertDXF);

    vInsertDXF.Layer := ALayer;
    Converter.DoCreate(vInsertDXF);
    Converter.Loads(vInsertDXF);
  end;
  //Shape
  if ACompound.ShapesCount > 0 then
  begin
    vEntity := TsgDXFEntityClass(GetBrepGroupClass).Create;
    TsgDXFEntityAccess(vEntity).SetConverter(Converter);
    TsgBrepModEntity(vEntity).SetCompound(ACompound);
    Converter.Loads(vEntity);
    ABlock.AddEntity(vEntity);
    Converter.Loads(ABlock);
  end;
{$ELSE}
begin
{$ENDIF}  
end;

function TsgVectorImageWithModeller.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepBrep;
end;

function TsgVectorImageWithModeller.GetUnNamedPart(
  ACompound: TsgModPartCompound; const AIsRoot: Boolean = False): string;
begin
{$IFNDEF SG_NO_USE_KERNEL3D}
  Result := cnstModCaptionShapeCmpnd;
  if AIsRoot then
    Result := ExtractFileName(FileName)
  else
  begin
    case ACompound.PartType of
      ptEmpty:
        Result := 'Empty';
      ptAcorn, ptWire, ptSheet, ptSolid, ptGeneral, ptAssembly:
        Result := 'Instance';
    else//ptEmpty
      Result := ChangeFileExt(ExtractFileName(FileName),'');
    end;
  end;
{$ENDIF}
end;

{ TsgCadImageDyDimension }

constructor TsgCadImageDyDimension.Create;
//var
//  vPHVS: PsgHeadVarStruct;
//  vDimStyle: TsgDXFDimensionStyle;
begin
  inherited Create;
  Converter.OnSHXResolve := nil;
  Converter.OnXRefResolve := nil;
  Converter.OnImageDefResolve := nil;
  Converter.OnModellerMsg := nil;

  FScale := 1;

  Converter.InitializeSectionsBegin;
  Converter.InitializeSectionsEnd;
  CurrentLayout := Layouts[0];
  GetExtents;

//  FDimByDrawing := TsgDXFDimension.Create;
//  CurrentLayout.AddEntity(FDimByDrawing);
//  FDimByDrawing.Flags := 1;

//  vPHVS := TsgDXFConverterAccess(Converter).PHeadVarStruct;


//  vDimStyle :=  TsgDXFDimensionStyle.Create;
//  vDimStyle.AssignEntity(GetMeasureStyle3D);
//  vDimStyle.TextStyle := TsgDXFStyle.Create;
//  vDimStyle.TextStyle.AssignEntity(GetMeasureStyle3D.TextStyle);
//  Converter.Sections[csStyles].AddEntity(vDimStyle.TextStyle);
//  Converter.Loads(vDimStyle.TextStyle);
//  Converter.Sections[csDimStyles].AddEntity(vDimStyle);
//  Converter.Loads(vDimStyle);

//  vPHVS^.DimProps := TsgDXFDimensionStyleAccess(vDimStyle).FDimProps;
//  vPHVS^.DimStyle := vDimStyle.Name;
//  vPHVS^.DimTextStyle := vDimStyle.TextStyle.Name;

//  FDimByDrawing.Style := vDimStyle;
//  TsgDXFDimensionAccess(FDimByDrawing).ActualDimStyle.AssignEntity(FDimByDrawing.Style);


//  FDimByDrawing.Layer := Converter.LayerByName(sContinuous);
//  TsgDXFDimensionAccess(FDimByDrawing).SetLineType(Converter.LTypeByName(sContinuous));
//  Converter.Loads(FDimByDrawing);

  FTextByDrawing := TsgDXFText.Create;

  FTextByDrawing.Point := cnstFPointZero;
//  FTextByDrawing.Style := FDimByDrawing.Style.TextStyle;
//  FTextByDrawing.Height := FDimByDrawing.TextHeight;
  FTextByDrawing.Text := '';
  CurrentLayout.AddEntity(FTextByDrawing);
  Converter.Loads(FTextByDrawing);
end;

procedure TsgCadImageDyDimension.StringToPolylinesList(const AText: string;
  const APolylines: TsgObjectList);
begin
  FTextByDrawing.Text := AText;
  FTextByDrawing.Scale := FScale;
  Converter.Loads(FTextByDrawing.Style);
  TsgDXFTextAccess(FTextByDrawing).Loaded(Converter);

  Converter.GetTextPolylinesList(FTextByDrawing, APolylines);
end;

initialization
  StdLines := TsgLines.Create;
{$IFNDEF SG_NON_WIN_PLATFORM}
  CreateAcadPal;
{$ENDIF}

finalization
  FreeAndNil(StdLines);
{$IFNDEF SG_NON_WIN_PLATFORM}
  DeleteObject(AcadPal);
{$ENDIF}

end.

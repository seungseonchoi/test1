{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                 Selecting entity by matrix                 }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgSelection;
{$I SGDXF.inc}

{$IFDEF SG_AGG2D}
{$UNDEF SG_HAS_WINAPI_INTERFACE}
{$ENDIF}
{$IFNDEF SG_HAS_WINAPI_INTERFACE}
  {$IFDEF SG_LINUX_FPC}
    {$DEFINE SG_BITMAP_API}
  {$ENDIF}
  {$IFDEF SG_MACOS_FPC}
    {$DEFINE SG_BITMAP_API}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
  {$IFDEF SGFPC}
  JwaWinGDI,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF SGFPC}
  LCLIntf, LCLType, Types, LazFreeType,
  {$ENDIF}
  SysUtils,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Surfaces, FMX.Types,
{$IFDEF SG_FM_WINDOWS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ,// Winapi.GDIPUTIL,
{$ENDIF}
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, Math, sgConsts, sgFunction, sgLists
  {$IFDEF SGDEL_XE3}
  , System.Types, System.UITypes
  {$ENDIF}
  ;

type
{$IFNDEF SG_FIREMONKEY}
  TBitmap = Graphics.TBitmap;
{$ENDIF}
  TsgSelectionMode = (smDisabled, smEnabled, smMatrixOnly);
  TsgSearchMode = (smPoint, smSnail, smFillability);
  TsgTypeElement = (teEntity, teMarker);
  TsgTypeElements = set of TsgTypeElement;
  TsgViewPortMode = (vmEdge, vmFill);

  TsgSimpleGraphicsObject = class;
  TsgSimplePen = class;
  TsgSimpleBrush = class;

  TsgSelectionMatrix = class
  private
    FPointByEntity: TPoint;
    FPrecision: Integer;
    FCanvas: TCanvas;
{$IFDEF SG_FIREMONKEY}
  {$IFDEF SG_FM_WINDOWS}
    [Weak]FGPBitmap: TGPBitmap;
  {$ELSE}
    FSnapShoot: TBitmapSurface;
  {$ENDIF}
{$ELSE}
  {$IFDEF SG_HAS_WINAPI_INTERFACE}
    FDIBBitmap32: TsgDIBBitmap32;
  {$ENDIF}
  {$IFDEF SG_BITMAP_API}
    FBitmap: TBitmap;
  {$ENDIF}
{$ENDIF}
    FLinkPen: TsgSimplePen;
    FLinkBrush: TsgSimpleBrush;
    FRegions: TsgStackRegions;
    FSearchMode: TsgSearchMode;
    FViewPortMode: TsgViewPortMode;
    FEntities: TsgObjectList;
    FMarkers: TsgObjectList;
    function FindElementOfFillabilityMode(const X, Y: Integer): Integer;
    function FindElementOfSnailMode(const X, Y: Integer): Integer;
    function FindPointOfIndexInSnailMode(const X, Y: Integer; const AIndex: Integer; var APt: TPoint): Boolean;
    function GetCanvas: TCanvas;
    function GetCurrColor: TColor;
    function GetMatrix(const X, Y: Integer): Pointer;
    function GetFontColor: TColor;
    function GetHandle: THandle;
    function GetSize: TPoint;
    procedure SetPrecision(const AVal: Integer);
    procedure SetSize(const APoint: TPoint);
    procedure SetFontColor(const Value: TColor);
  protected
    function IsCorrectIndex(const AIndex: Integer): Boolean;
    function GetElement(const AIndex: Integer): Pointer;
    function GetList(const AType: TsgTypeElement): TsgObjectList;
    function ProcessSnailMode(const AX, AY: Integer; const AWaitIndex: Integer;
      var APoint: TPoint): Integer;
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; Value: TColor);
    procedure FreeCanvas;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddElement(const AVal: Pointer;
      const AType: TsgTypeElement = teEntity);
    procedure Assign(Source: TsgSelectionMatrix);
    procedure BeginDraw;
    procedure Changed;
    procedure ClearFull(const AResetClipping: Boolean = False);
    procedure ClearRect(const ARect: TRect);
    procedure ClearElements(const AType: TsgTypeElement);
    procedure EndDraw;
    function FindNearestPoint(const AStartPoint: TPoint;
      var AFindedPoint: TPoint): Boolean;
{$IFDEF SG_BTI_TEST}
    procedure GetBoundaryByPoint(const APoint: TPoint; const AList: TList);
{$ENDIF}
    procedure GetElements(const X, Y: Integer; const AList: TObject;
      const AUniq: Boolean);
    procedure GetElementsVis(const AList: TList; const AFull: Boolean;
      const AType: TsgTypeElement = teEntity);
    procedure GetElementsVisList(const AList: TsgObjectList; const AFull: Boolean;
      const AType: TsgTypeElement = teEntity);
    function IsOverflow: Boolean;
    procedure RemoveElement(const AElement: Pointer;
      const AType: TsgTypeElement = teEntity);
    procedure RemoveElements(const AElements: TsgObjectList;
     const AType: TsgTypeElement = teEntity);
    function RestoreClipRGN: Integer;
    function SaveClipRGN: Integer;
    property BitPixel[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Brush: TsgSimpleBrush read FLinkBrush;
    property Canvas: TCanvas read GetCanvas;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property Handle: THandle read GetHandle;
    property CurrColor: TColor read GetCurrColor;
    property Matrix[const X, Y: Integer]: Pointer read GetMatrix;
    property Pen: TsgSimplePen read FLinkPen;
    property PointByEntity: TPoint read FPointByEntity;
    property Precision: Integer read FPrecision write SetPrecision;
    property SearchMode: TsgSearchMode read FSearchMode write FSearchMode;
    property Size: TPoint read GetSize write SetSize;
    property ViewPortMode: TsgViewPortMode read FViewPortMode write FViewPortMode;
  end;

  TsgSimpleGraphicsObject = class
  private
    FHandle: THandle;
    FLinkCanvas: TCanvas;
    FValid: Boolean;
  protected
    procedure Changed; virtual;
    procedure FreeObjectHandle;
    function CanUseExStockObjects: Boolean; virtual;
    function GetCurrentStockObject: THandle; virtual;
    function HandleAllocated: Boolean;
    procedure SetHandle(const Value: THandle);
    procedure SelectDefaultObject; virtual;
    procedure SetLinkCanvas(const Value: TCanvas);
    procedure NoValid;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Handle: THandle read FHandle write SetHandle;
    property LinkCanvas: TCanvas read FLinkCanvas write SetLinkCanvas;
    property Valid: Boolean read FValid;
  end;

  TsgSimpleBrush = class(TsgSimpleGraphicsObject)
  private
    FBrushData: TBrushData;
  protected
    procedure Changed; override;
    function GetColor: TColor;
    function GetCurrentStockObject: THandle; override;
    function GetStyle: TBrushStyle;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TBrushStyle);
  public
    constructor Create; override;
    property Color: TColor read GetColor write SetColor default clWhite;
    property Style: TBrushStyle read GetStyle write SetStyle default bsSolid;
  end;

  TsgSimplePen = class(TsgSimpleGraphicsObject)
  private
    FPenData: TPenData;
  protected
    procedure Changed; override;
    function GetColor: TColor;
    function GetCurrentStockObject: THandle; override;
    function GetStyle: TPenStyle;
    function GetWidth: Single;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetWidth(const Value: Single);
  public
    constructor Create; override;
    property Color: TColor read GetColor write SetColor;
    property Style: TPenStyle read GetStyle write SetStyle;
    property Width: Single read GetWidth write SetWidth;
  end;

  PSnapDataArray = ^TSnapDataArray;
  TSnapDataArray = array[Byte] of PSnapData;

  { TODO: Left, Top properties for TsgSnapMatrix }

  TsgSnapMatrix = class
  private
    FBackColor: TColor;
    FRowsCount: Integer;
    FColsCount: Integer;
    FCells: PSnapDataArray;
    FCashe: TList;
    function GetCell(ACol, ARow: Integer): PSnapData;
    function ForceCell(ACol, ARow: Integer): PSnapData;
    function CacheSnap(var P: PSnapData): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearCell(const ACol, ARow: Integer);
    procedure ClearRect(const ARect: TRect);
    function IsCellValid(const ACol, ARow: Integer): Boolean;
    procedure Resize(const AColsCount, ARowsCount: Integer);
    procedure UpdateCell(ACol, ARow: Integer; const AValue: TSnapData);
    property BackColor: TColor read FBackColor write FBackColor;
    property Cells[ACol, ARow: Integer]: PSnapData read GetCell; default;
    property ColsCount: Integer read FColsCount;
    property RowsCount: Integer read FRowsCount;
  end;

  PContour = ^TContour;
  TContour = record
    Entities: TList;
    Pixels: TList; // list of TPixelData
    Box: TRect;
  end;

  TPixelOrient = (poLeft, poTop, poRight, poBottom);
  TPixelSide = (ps4, s1, s2, ps3, psIn, psOut, psEnt); // don't change order

  PPixelData = ^TPixelData;
  TPixelData = record
    Pt: TPoint;
    Orients: array [TPixelOrient] of set of TPixelSide;
    Contour: PContour;
    LowerPixel: PPixelData;
  end;

  TInputBitmapType = (ibtBitmap, ibtSelMatrix);
  TsgTraversalMode = (tmSnapToNearest, tmFindOneContour);
  TsgTraversalModes = set of TsgTraversalMode;

  TBmpTraversal = class
  private
    FBitmap: TBitmap;
{$IFDEF SG_FIREMONKEY}
    FBitData: FMX.Graphics.TBitmapData;
{$ENDIF}
    FDataBmp: TsgSelectionMatrix;
    FContours: TList;
    FIslandsContours: TList;
    FInputType: TInputBitmapType;
    FContour: PContour;
    FBmpRect: TRect;
    FPoint: TPoint;
    FBackColor: TColor;
    FSelMatrix: TsgSelectionMatrix;
    FIterationsLast: Cardinal;
    FStartPoint: TPoint;
    FOrient: TPixelOrient;
    FMode: TsgTraversalModes;
  protected
    procedure AddEnt(const APt: TPoint; AList: TList);
    procedure CalcExtensionPoint(const APt: TPoint; const APtData: PPixelData;
      var ExtensionPt: TPoint);
    function DeltaFromOrient(AOrient: TPixelOrient): TPoint;
    procedure DeletePixel(APixel: PPixelData);
    procedure DeleteContour(AContour: PContour);
    function GetContourEnts: TList;
    function GetData(const APt: TPoint): PPixelData;
    function GetEnt(const APt: TPoint): Pointer;
    function GetNewPixel(const APoint: TPoint; const AOrient: TPixelOrient;
      AContour: PContour): PPixelData;
    function GetNewContour: PContour;
    function GetNearestPoint(const APoint: TPoint; var AOrient: TPixelOrient): TPoint;
    function GetNextPixel(APixel: PPixelData; AEntList: TList;
      AContour: PContour): PPixelData;
    function GetPixel(X, Y: Integer): TColor;
    function HasEnt(const APt: TPoint): Boolean;
    function IsPointInContour(const APoint: TPoint; const ACountour: PContour): Boolean;
    procedure SetBitMap(const Value: TBitmap);
    procedure SetData(const APoint: TPoint; const AData: PPixelData);
    procedure SetDataSize(AWidth, AHeight: Integer);
    procedure SetPixel(X, Y: Integer; Value: TColor);
    procedure SetSelMatrix(const Value: TsgSelectionMatrix);
{$IFDEF SG_FIREMONKEY}
    procedure BitmapOnChange(Sender: TObject);
{$ENDIF}
    function OrientFromDelta(dPt: TPoint): TPixelOrient;
    function OffsetPoint(const APt: TPoint; const AOrient: TPixelOrient): TPoint;
    function ScanLine(const Pt: TPoint; var OutPt: TPoint): Boolean;
    function ScanContour(const APt: TPoint; var ExtensionPt: TPoint): PContour;
    property Orient: TPixelOrient read FOrient write FOrient;
    property StartPoint: TPoint read FStartPoint write FStartPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure CalcContours;
    property BackColor: TColor read FBackColor write FBackColor;
    property Bitmap: TBitmap read FBitmap write SetBitMap;
    property Contour: PContour read FContour;
    property ContourEnts: TList read GetContourEnts;
    property IslandsContours: TList read FIslandsContours;
    property InputType: TInputBitmapType read FInputType write FInputType;
    property UserPoint: TPoint read FPoint write FPoint;
    property SelMatrix: TsgSelectionMatrix read FSelMatrix write SetSelMatrix;
    property Mode: TsgTraversalModes read FMode write FMode;
  end;

implementation

const
  cnstPointZero: TPoint = ();
  cnstPrecisionDef: Integer = 3;
  cLeftVector: TPoint = (X: -1; Y: 0);
  cTopVector: TPoint = (X: 0; Y: -1);
  cRightVector: TPoint = (X: 1; Y: 0);
  cBottomVector: TPoint = (X: 0; Y: 1);
  cInvertVector: TPoint = (X: -1; Y: -1);
  cnstSnapMatrixCacheCount = 640 * 480;

type
  TSetDCPenColorFunc = function(DC: HDC; Color: COLORREF): COLORREF; stdcall;
  TSetDCBrushColorFunc = function(DC: HDC; Color: COLORREF): COLORREF; stdcall;
  TsgBaseListAccess = class(TsgBaseList);
{$IFDEF SGFPC}
  TRasterImageAccess = class(TRasterImage);
{$ENDIF}

var
{$IFNDEF SG_NON_WIN_PLATFORM}
  Gdi32HModule: HModule;
{$ENDIF}
  SetDCPenColorFunc: TSetDCPenColorFunc = nil;
  SetDCBrushColorFunc: TSetDCBrushColorFunc = nil;

procedure NormRect(var R: TRect);
begin
  if R.Left > R.Right then
    SwapInts(R.Left, R.Right);
  if R.Top > R.Bottom then
    SwapInts(R.Top, R.Bottom);
end;

  {TsgSelectionMatrix}

constructor TsgSelectionMatrix.Create;
var
  vSize: TPoint;
begin
  inherited Create;
  FSearchMode := smFillability;
  FEntities := TsgObjectList.Create;
  FMarkers := nil;
  vSize := GetScreenResolution;
{$IFDEF SG_FIREMONKEY}
  FCanvas := TCanvasManager.CreateFromBitmap(TBitmap.Create(vSize.X, vSize.Y), TCanvasQuality.HighPerformance);
  FCanvas.Bitmap.Clear(TColor(cnstDefColor).AsBGRA);
{$IFNDEF SG_FM_WINDOWS}
  FSnapShoot := TBitmapSurface.Create;
  FSnapShoot.Assign(FCanvas.Bitmap);
{$ENDIF}
{$ELSE}
  FCanvas := TCanvas.Create;
{$IFDEF SG_HAS_WINAPI_INTERFACE}
  FDIBBitmap32 := TsgDIBBitmap32.Create(vSize.X, vSize.Y);
  FCanvas.Handle := FDIBBitmap32.BufferHandle;
  FCanvas.Brush.Color := cnstDefColor;
  FCanvas.FillRect(FCanvas.ClipRect);
{$ENDIF}
{$IFDEF SG_BITMAP_API}
  FreeAndNil(FCanvas);
  FBitmap := TBitmap.Create;
  FCanvas := FBitmap.Canvas;
{$IFDEF SGFPC}
  FCanvas.AntialiasingMode := amOff;
{$ENDIF}
  FCanvas.Brush.Color := cnstDefColor;
  FCanvas.FillRect(FCanvas.ClipRect);
{$ENDIF}
{$ENDIF}
  FLinkPen := TsgSimplePen.Create;
  FLinkPen.LinkCanvas := Canvas;
  FLinkBrush := TsgSimpleBrush.Create;
  FLinkBrush.LinkCanvas := Canvas;
  if Assigned(Canvas) then
    Canvas.Pen.Width := {$IFDEF SG_FIREMONKEY}1{$ELSE}0{$ENDIF};
  FRegions := TsgStackRegions.Create;
  FPrecision := cnstPrecisionDef;
  FViewPortMode := vmEdge;
end;

destructor TsgSelectionMatrix.Destroy;
begin
  FEntities.Free;
  if FMarkers <> nil then
    FMarkers.Free;
  FLinkPen.Free;
  FLinkBrush.Free;
{$IFDEF SG_FIREMONKEY}
{$IFNDEF SG_FM_WINDOWS}
  FSnapShoot.Free;
{$ENDIF}
{$ELSE}
  {$IFDEF SG_HAS_WINAPI_INTERFACE}
  FCanvas.Handle := 0;
  FreeAndNil(FDIBBitmap32);
  {$ENDIF}
  {$IFDEF SG_BITMAP_API}
  if Assigned(FBitmap) and (FCanvas = FBitmap.Canvas) then
    FCanvas := nil;
  FreeAndNil(FBitmap);
  {$ENDIF}
{$ENDIF}
  FreeCanvas;
  FRegions.Free;
  inherited Destroy;
end;

procedure TsgSelectionMatrix.EndDraw;
begin
  FLinkPen.Handle := 0;
  FLinkBrush.Handle := 0;
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_WINDOWS}
  if TObject(FCanvas.Bitmap.Handle) is TGPBitmap then
    FGPBitmap := TGPBitmap(FCanvas.Bitmap.Handle)
  else
    FGPBitmap := nil;
{$ELSE}
  FSnapShoot.Assign(FCanvas.Bitmap);
{$ENDIF}
{$ENDIF}
{$IFDEF SG_BITMAP_API}
{$IFDEF SGFPC}
  TRasterImageAccess(FBitmap).SaveStreamNeeded;
{$ENDIF}
{$ENDIF}
end;

procedure TsgSelectionMatrix.AddElement(const AVal: Pointer;
  const AType: TsgTypeElement = teEntity);
var
  vColor: Integer;
begin
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(Self);
  try
{$ENDIF}
  if AVal <> nil then
  begin
    case AType of
      teMarker:
        begin
          if FMarkers = nil then
          begin
            FMarkers := TsgObjectList.Create;
            FMarkers.Count := FEntities.Count;
            FMarkers.LoadListNils;
          end;
          FMarkers.Add(AVal);
          FEntities.Add(nil);
        end;
    else//teEntity
      if FMarkers <> nil then
        FMarkers.Add(nil);
      FEntities.Add(AVal);
    end;
  end;
  vColor := FEntities.Count - 1;
  Pen.Color := vColor;
  Brush.Color := vColor;
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(Self);
  end;
{$ENDIF}
end;

procedure TsgSelectionMatrix.Assign(Source: TsgSelectionMatrix);
begin
  FPointByEntity := Source.FPointByEntity;
  FPrecision := Source.FPrecision;
{$IFDEF SG_FIREMONKEY}
  if Assigned(Source.Canvas.Bitmap) then
    FCanvas.CopyBitmap(Source.Canvas.Bitmap, FCanvas.Bitmap);
{$ELSE}
  {$IFDEF SG_HAS_WINAPI_INTERFACE}
  if Assigned(Source.FDIBBitmap32) then
  begin
{$IFDEF SG_THREAD_DRAW}
    Source.FCanvas.Lock; try
{$ENDIF}
    FDIBBitmap32.SetSize(Source.FDIBBitmap32.Map.Width, Source.FDIBBitmap32.Map.Height);
    System.Move(Source.FDIBBitmap32.Map.Data^, FDIBBitmap32.Map.Data^, FDIBBitmap32.Map.Height * FDIBBitmap32.Map.BytesPerScanline);
{$IFDEF SG_THREAD_DRAW}
    finally Source.FCanvas.Unlock end;
{$ENDIF}
  end;
  {$ENDIF}
  {$IFDEF SG_BITMAP_API}
  FBitmap.Assign(Source.FBitmap);
  {$ENDIF}
{$ENDIF}
  FSearchMode := Source.FSearchMode;
  FViewPortMode := Source.FViewPortMode;
  FEntities.AssignList(Source.FEntities, loCopy);
  FreeAndNil(FMarkers);
  Brush.Color := Source.Brush.Color;
  Pen.Color := Source.Pen.Color;
  Pen.Width := Source.Pen.Width;
end;

procedure TsgSelectionMatrix.BeginDraw;
begin
  // may be recreate FDIBBitmap32
  // for no main thread
end;

procedure TsgSelectionMatrix.Changed;
begin
  if not Pen.Valid then
    Pen.Changed;
  if not Brush.Valid then
    Brush.Changed;
end;

procedure TsgSelectionMatrix.ClearElements(const AType: TsgTypeElement);
begin
  case AType of
    teMarker: FreeAndNil(FMarkers);
  else
    if (FMarkers <> nil) and (FMarkers.Count > 0) then
      FEntities.LoadListNils
    else
      FEntities.Count := 0;
  end;
end;

procedure TsgSelectionMatrix.ClearFull(const AResetClipping: Boolean = False);
begin
  FEntities.Count := 0;
  FreeAndNil(FMarkers);
  ClearRect(Canvas.ClipRect);
  if AResetClipping then
{$IFDEF SG_FIREMONKEY}
    FMX_SelectClipRgn(Canvas.Handle, 0);
{$ELSE}
    SelectClipRgn(Canvas.Handle, 0);
{$ENDIF}
end;

procedure TsgSelectionMatrix.ClearRect(const ARect: TRect);
begin
  Pen.Color := cnstDefColor;
  Brush.Color := cnstDefColor;
{$IFDEF SG_FIREMONKEY}
  FCanvas.Bitmap.ClearRect(TRectF.Create(ARect), TColor(cnstDefColor).ToAlphaColor);
{$IFNDEF SG_FM_WINDOWS}
  FSnapShoot.Assign(FCanvas.Bitmap);
{$ENDIF}
{$ELSE}
  FCanvas.FillRect(ARect);
{$ENDIF}
end;

procedure TsgSelectionMatrix.GetElements(const X, Y: Integer; const AList: TObject; const AUniq: Boolean);
var
  vX, vY, vIndex: Integer;
  vElement: Pointer;
  vList: TList;
  vPointerList: TsgList;
  vObjList: TsgObjectList;
begin
  if SetObjLists(AList, vList, vPointerList, vObjList) > 0 then
  begin
    if vList <> nil then
      vList.Count := 0;
    if vPointerList <> nil then
      vPointerList.Count := 0;
    if Assigned(vObjList) then
      vObjList.Clear(False);
    for vX := X - FPrecision to X + FPrecision do
    begin
      for vY := Y - FPrecision to Y + FPrecision do
      begin
        vIndex := BitPixel[vX, vY];
        if IsCorrectIndex(vIndex) then
        begin
          vElement := GetElement(vIndex);
          if vPointerList <> nil then
            vPointerList.Add(vElement)
          else
          begin
            if Assigned(vObjList) then
              vObjList.Add(vElement)
            else
            begin
              if (not AUniq) or (vList.IndexOf(vElement) < 0) then
                vList.Add(vElement);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TsgSelectionMatrix.GetElementsVis(const AList: TList;
  const AFull: Boolean; const AType: TsgTypeElement = teEntity);
var
  vList: TsgObjectList;
begin
  vList := TsgObjectList.Create;
  try
    GetElementsVisList(vList, AFull, AType);
    vList.CopyTo(AList);
  finally
    vList.Free;
  end;
end;

procedure TsgSelectionMatrix.GetElementsVisList(const AList: TsgObjectList;
  const AFull: Boolean; const AType: TsgTypeElement);
var
  vWidth, vHeight: Integer;
  I, vIndex: Integer;
  vData: PInteger;
  vSortList: TsgObjectList;
  vList: TsgObjectList;
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_WINDOWS}
  vBData: FMX.Graphics.TBitmapData;
{$ENDIF}
{$ENDIF}
begin
  vList := GetList(AType);
  if vList <> nil then
  begin
    vSortList := TsgObjectList.Create;
    try
      vSortList.Capacity := FEntities.Count;
      vSortList.Duplicates := dupIgnore;
      vSortList.Sorted := True;
      if AFull then
      begin
        for I := 0 to vList.Count - 1 do
          if vList.List[I] <> nil then
            vSortList.Add(vList.List[I]);
      end
      else
      begin
{$IFDEF SG_FIREMONKEY}
{$IFNDEF SG_FM_WINDOWS}
        vWidth := FSnapShoot.Width;
        vHeight := FSnapShoot.Height;
        vData := FSnapShoot.Bits;
{$ELSE}
        FCanvas.Bitmap.Map(TMapAccess.Read, vBData);
        vWidth := vBData.Width;
        vHeight := vBData.Height;
        vData := vBData.GetScanline(FCanvas.Bitmap.Height - 1);
{$ENDIF}
{$ELSE}
{$IFDEF SG_HAS_WINAPI_INTERFACE}
        vWidth := FDIBBitmap32.Map.Width;
        vHeight := FDIBBitmap32.Map.Height;
        vData := FDIBBitmap32.Map.Data;
{$ELSE}
  {$IFDEF SG_BITMAP_API}
       vWidth := FBitmap.Width;
       vHeight := FBitmap.Height;
  {$ELSE}
        vWidth := 0;
        vHeight := 0;
  {$ENDIF}
{$ENDIF}
{$ENDIF}
        try
          I := 0;
          while I < vWidth * vHeight do
          begin
            vIndex := SwapWords(vData^) shr 8;
            Inc(vData);
            if IsCorrectIndex(vIndex) and (vList.List[vIndex] <> nil) then
              vSortList.Add(vList.List[vIndex]);
            Inc(I);
          end;
        finally
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_WINDOWS}
        FCanvas.Bitmap.Unmap(vBData);
{$ENDIF}
{$ENDIF}
        end;
      end;
      AList.AssignList(vSortList, loCopy);
    finally
      vSortList.Free;
    end;
  end;
end;

function TsgSelectionMatrix.FindElementOfFillabilityMode(const X, Y: Integer): Integer;
var
  vValues, vCounts: TsgIntegerList;
  I, J, vIndex: Integer;
  vMaxCount, vMaxIndex, vMaxIndexPrev, vValue: Integer;
begin
  Result := -1;
  vValues := TsgIntegerList.Create(0, Sqr(FPrecision shl 1));
  vCounts := TsgIntegerList.Create(0, vValues.Capacity);
  try
    for I := -FPrecision to FPrecision do
      for J := -FPrecision to FPrecision do
      begin
        vValue := BitPixel[X + I, Y + J];
        if (vValue >= 0) and (vValue < cnstDefColor) then
        begin
          vIndex := vValues.IndexOf(vValue);
          if vIndex >= 0 then
            Inc(vCounts.List^[vIndex])
          else
          begin
            vValues.Add(vValue);
            vCounts.Add(1);
          end;
        end;
      end;
    case vValues.Count of
      0:  begin end;//not found
      1:  Result := vValues[0];
    else
      vMaxCount := -1;
      vMaxIndex := -1;
      vMaxIndexPrev := -1;
      for I := vCounts.Count - 1 downto 0 do
      begin
        if vMaxCount < vCounts.List^[I] then
        begin
          vMaxCount := vCounts.List^[I];
          vMaxIndexPrev := vMaxIndex;
          vMaxIndex := I;
        end;
      end;
      if vMaxIndexPrev < 0 then
        vMaxIndexPrev := vMaxIndex;
      Result := vValues[vMaxIndexPrev];
    end;
    if (Result < 0) or (not FindPointOfIndexInSnailMode(X, Y, Result, FPointByEntity)) then
      FPointByEntity := cnstPointZero;
  finally
    vValues.Free;
    vCounts.Free;
  end;
end;

function TsgSelectionMatrix.FindElementOfSnailMode(const X, Y: Integer): Integer;
begin
  Result := ProcessSnailMode(X, Y, MaxInt, FPointByEntity);
end;

function TsgSelectionMatrix.FindNearestPoint(const AStartPoint: TPoint;
  var AFindedPoint: TPoint): Boolean;
begin
  Result := IsCorrectIndex(ProcessSnailMode(AStartPoint.X, AStartPoint.Y,
    MaxInt, AFindedPoint));
end;

function TsgSelectionMatrix.FindPointOfIndexInSnailMode(const X, Y: Integer; const AIndex: Integer; var APt: TPoint): Boolean;
begin
  Result := IsCorrectIndex(ProcessSnailMode(X, Y, AIndex, APt));
end;

procedure TsgSelectionMatrix.FreeCanvas;
{$IFDEF SG_FIREMONKEY}
var
  vBitmap: FMX.Graphics.TBitmap;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  if Assigned(FCanvas) then
  begin
    vBitmap := FCanvas.Bitmap;
    vBitmap.Free;
  end;
{$ENDIF}
  FreeAndNil(FCanvas);
end;

function TsgSelectionMatrix.ProcessSnailMode(const AX, AY: Integer; const AWaitIndex: Integer;
  var APoint: TPoint): Integer;

  function CheckPoint(const AX, AY: Integer; var AIndex: Integer): Boolean;
  begin
    AIndex := Integer(BitPixel[AX, AY]);
    if AWaitIndex = MaxInt then
      Result := IsCorrectIndex(AIndex)
    else
      Result := AIndex = AWaitIndex;
  end;

var
  I, vDeltaX, vDeltaY: Integer;
  vClosedPeriod: Boolean;
begin
  APoint := Point(AX, AY);
  Result := -1;
  vDeltaX := 1;
  vDeltaY := 1;
  while True do
  begin
    I := APoint.X + vDeltaX;
    while APoint.X <> I do
    begin
      Inc(APoint.X, Sign(vDeltaX));
      if CheckPoint(APoint.X, APoint.Y, Result) then
        Exit;
    end;
    vDeltaX := (Abs(vDeltaX) + 1) * Sign(-vDeltaX);
    vClosedPeriod := (Abs(vDeltaX) > 2) and (Abs(vDeltaX) mod 2 = 1);
    if vClosedPeriod and (FPrecision <= Abs(APoint.X - AX)) then
      Exit;
    I := APoint.Y + vDeltaY;
    while APoint.Y <> I do
    begin
      Inc(APoint.Y, Sign(vDeltaY));
      if CheckPoint(APoint.X, APoint.Y, Result) then
        Exit;
    end;
    vDeltaY := (Abs(vDeltaY) + 1) * Sign(-vDeltaY);
  end;
end;

{$IFDEF SG_BTI_TEST}
procedure TsgSelectionMatrix.GetBoundaryByPoint(const APoint: TPoint; const AList: TList);
type
  TsgDirection = (drUndefined, drRight, drTop, drLeft, drBottom);

const
  cnstNewDirection: array [drRight..drBottom] of TsgDirection = (drBottom, drRight, drTop, drLeft);
  cnstNextDirection: array [drRight..drBottom] of TsgDirection = (drTop, drLeft, drBottom, drRight);

  function GetData(const AP: TPoint): Integer;
  begin
    Result := Canvas.Pixels[AP.X, AP.Y];
  end;

  procedure ClearData(const AP: TPoint);
  begin
    Canvas.Pixels[AP.X, AP.Y] := cnstDefColor;
  end;

  function CheckData(const AData: Integer): Boolean;
  begin
    Result := (AData > -1) and (AData < cnstDefColor);
  end;

  function SetNextPos(var ACurPos: TPoint; var ACurDirection: TsgDirection; var AData: Integer): Integer;
  var
    vPos: TPoint;
//    vOldDirection: TsgDirection;
    vData: Integer;
  begin
    AData := -1;
    Result := 0;
    vPos := ACurPos;
//    vOldDirection := ACurDirection;
    ACurDirection := cnstNewDirection[ACurDirection];
    while Result < 4 do
    begin
      Inc(Result);
      case ACurDirection of
        drRight:    Inc(ACurPos.X);
        drBottom:   Inc(ACurPos.Y);
        drLeft:     Dec(ACurPos.X);
        drTop:      Dec(ACurPos.Y);
      else
        Exit;
      end;
      vData := GetData(ACurPos);
      if CheckData(vData) then
      begin
        AData := vData;
        Break;
      end
      else
        ACurPos := vPos;
      ACurDirection := cnstNextDirection[ACurDirection];
    end;
  end;

  function FindToRight(var AP: TPoint; var AIndex: Integer): Boolean;
  var
    vSize: Integer;
  begin
    Result := False;
    AP := APoint;
    vSize := Size.X;
    while AP.X < vSize do
    begin
      AIndex := GetData(AP);
      if CheckData(AIndex) then
      begin
        Result := True;
        Break;
      end
      else
        Inc(AP.X);
    end;
  end;

  function FindToLeft(var AP: TPoint; var AIndex: Integer): Boolean;
  begin
    Result := False;
    AP := APoint;
    while AP.X > -1 do
    begin
      AIndex := GetData(AP);
      if CheckData(AIndex) then
      begin
        Result := True;
        Break;
      end
      else
        Dec(AP.X);
    end;
  end;

  function FindToTop(var AP: TPoint; var AIndex: Integer): Boolean;
  begin
    Result := False;
    AP := APoint;
    while AP.Y > -1 do
    begin
      AIndex := GetData(AP);
      if CheckData(AIndex) then
      begin
        Result := True;
        Break;
      end
      else
        Dec(AP.Y);
    end;
  end;

  function FindToBottom(var AP: TPoint; var AIndex: Integer): Boolean;
  var
    vSize: Integer;
  begin
    Result := False;
    AP := APoint;
    vSize := Size.Y;
    while AP.Y < vSize do
    begin
      AIndex := GetData(AP);
      if CheckData(AIndex) then
      begin
        Result := True;
        Break;
      end
      else
        Inc(AP.Y);
    end;
  end;

var
  vIndex: Integer;
  vPoint: TPoint;
  vCurPos: TPoint;
  vCurDirection: TsgDirection;
  vList: TsgList;
begin
  vList := TsgList.Create;
  try
    vList.Sorted := True;
    vList.Duplicates := dupIgnore;
    vList.Capacity := FEntities.Count;
    if FindToTop(vPoint, vIndex) then
      vCurDirection := drTop
    else
      if FindToRight(vPoint, vIndex) then
        vCurDirection := drRight
      else
        if FindToBottom(vPoint, vIndex) then
          vCurDirection := drBottom
        else
          if FindToLeft(vPoint, vIndex) then
            vCurDirection := drLeft
          else
            vCurDirection := drUndefined;
    if vCurDirection <> drUndefined then
    begin
      //ClearData(vPoint);
      vList.Add(Pointer(vIndex));
      vCurPos := vPoint;
      repeat
        SetNextPos(vCurPos, vCurDirection, vIndex);
        if CheckData(vIndex) then
        begin
          //ClearData(vCurPos);
          vList.Add(Pointer(vIndex));
        end;
      until IsEqualPoints(vCurPos, vPoint);
    end;
    if vList.Count > 0 then
    begin
      AList.Count := vList.Count;
      for vIndex := 0 to vList.Count - 1 do
        AList.List[vIndex] := FEntities[TsgNativeUInt(vList[vIndex])];
    end;
  finally
    vList.Free;
  end;
end;
{$ENDIF}

function TsgSelectionMatrix.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TsgSelectionMatrix.GetCurrColor: TColor;
begin
  Result := FEntities.Count;
end;

function TsgSelectionMatrix.GetElement(const AIndex: Integer): Pointer;
begin
  Result := FEntities.List[AIndex];
  if (FMarkers <> nil) and (FMarkers.List[AIndex] <> nil) then
    Result := FMarkers.List[AIndex];
end;

function TsgSelectionMatrix.GetMatrix(const X, Y: Integer): Pointer;
var
  vIndex: Integer;
begin
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(Self);
  try
{$ENDIF}
  if FPrecision > 1 then
  begin
    case FSearchMode of
      smSnail:        vIndex := FindElementOfSnailMode(X, Y);
      smFillability:  vIndex := FindElementOfFillabilityMode(X, Y);
    else
      vIndex := BitPixel[X, Y];
      FPointByEntity.X := X;
      FPointByEntity.Y := Y;
    end;
  end
  else
    vIndex := BitPixel[X, Y];
  if IsCorrectIndex(vIndex) then
    Result := GetElement(vIndex)   //evg - for tracersmatrix this is good place to breakpoint
  else
  begin
    Result := nil;
    FPointByEntity := cnstPointZero;
  end;
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(Self);
  end;
{$ENDIF}
end;

function TsgSelectionMatrix.GetPixel(X, Y: Integer): TColor;
{$IFDEF SG_FM_WINDOWS}
var
  color: Cardinal;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_WINDOWS}
  if InRange(X, 0, FCanvas.Bitmap.Width - 1) and InRange(Y, 0, FCanvas.Bitmap.Height - 1) and Assigned(FGPBitmap) then
  begin
    FGPBitmap.GetPixel(X, Y, color);
    Result := TColor.Create(TAlphaColor(color));
  end
  else
{$ELSE}
  if InRange(X, 0, FSnapShoot.Width - 1) and InRange(Y, 0, FSnapShoot.Height - 1) then
    Result := TColor.Create(FSnapShoot.Pixels[X, Y])
  else
{$ENDIF}
    Result := cnstDefColor;
{$IFDEF SG_FM_ANDROID}
//  FCanvas.Bitmap.SaveToFile('/storage/emulated/0/Android/data/com.embarcadero.cstCadNavigator/files/SHXFonts/1.png');
{$ENDIF}
{$ELSE}
{$IFDEF SG_HAS_WINAPI_INTERFACE}
    if InRange(X, 0, FDIBBitmap32.Map.Width - 1) and InRange(Y, 0, FDIBBitmap32.Map.Height - 1) then
      Result := FDIBBitmap32.Map.Pixels[X, FDIBBitmap32.Map.Height - Y - 1]
    else
      Result := cnstDefColor;
{$ELSE}
    Result := FCanvas.Pixels[X, Y];
{$ENDIF}
{$ENDIF}
end;

function TsgSelectionMatrix.GetFontColor: TColor;
begin
  Result := Canvas.Font.Color;
end;

function TsgSelectionMatrix.GetHandle: THandle;
begin
  Result := Canvas.Handle;
end;

function TsgSelectionMatrix.GetList(const AType: TsgTypeElement): TsgObjectList;
begin
  case AType of
    teMarker : Result := FMarkers;
  else
    Result := FEntities;
  end;
end;

procedure TsgSelectionMatrix.SetPixel(X, Y: Integer; Value: TColor);
{$IFDEF SG_FIREMONKEY}
{$IFNDEF SG_FM_WINDOWS}
var
  vBitData: FMX.Graphics.TBitmapData;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
{$IFDEF SG_FM_WINDOWS}
  if InRange(X, 0, FCanvas.Bitmap.Width - 1) and InRange(Y, 0, FCanvas.Bitmap.Height - 1) and Assigned(FGPBitmap) then
    FGPBitmap.SetPixel(X, Y, Cardinal(Value.AsBGRA));
{$ELSE}
  if FCanvas.Bitmap.Map(TMapAccess.ReadWrite, vBitData) then
  try
    if InRange(X, 0, vBitData.Width - 1) and InRange(Y, 0, vBitData.Height - 1) then
      vBitData.SetPixel(X, Y, Value.AsBGRA);
  finally
    FCanvas.Bitmap.Unmap(vBitData);
  end;
{$ENDIF}
{$ELSE}
  FCanvas.Pixels[X, Y] := Value;
{$ENDIF}
end;

procedure TsgSelectionMatrix.SetPrecision(const AVal: Integer);
begin
  if AVal < 0 then
    FPrecision := cnstPrecisionDef
  else
    FPrecision := AVal;
end;

function TsgSelectionMatrix.GetSize: TPoint;
begin
{$IFDEF SG_FIREMONKEY}
  Result := Point(FCanvas.Width, FCanvas.Height);
{$ELSE}
  {$IFDEF SG_HAS_WINAPI_INTERFACE}
  Result := Point(FDIBBitmap32.Map.Width, FDIBBitmap32.Map.Height);
  {$ELSE}
    {$IFDEF SG_BITMAP_API}
    Result := Point(FCanvas.Width, FCanvas.Height);
    {$ELSE}
    Result := Point(1, 1);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

function TsgSelectionMatrix.IsCorrectIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex > -1) and (AIndex < FEntities.Count);
end;

function TsgSelectionMatrix.IsOverflow: Boolean;
begin
  Result := FEntities.Count >= clWhite;
end;

procedure TsgSelectionMatrix.RemoveElement(const AElement: Pointer;
  const AType: TsgTypeElement = teEntity);
var
  I: Integer;
  vList: TsgObjectList;
begin
  vList := GetList(AType);
  if vList <> nil
  then
  begin
    for I := vList.Count - 1 downto 0 do
      if vList.List[I] = AElement then
        vList.List[I] := nil;
  end;
end;

procedure TsgSelectionMatrix.RemoveElements(const AElements: TsgObjectList;
  const AType: TsgTypeElement = teEntity);
var
  I, vIndex: Integer;
  vSortList: TsgObjectList;
  vList: TsgObjectList;
  vItem: Pointer;
begin
  vList := GetList(AType);
  if vList <> nil then
  begin
    case AElements.Count of
      0:  begin end;
      1:  RemoveElement(AElements.List[0], AType);
    else
      vSortList := TsgObjectList.Create;
      try
        AElements.AssignList(vSortList, loCopy);
        if not vSortList.Sorted then
          vSortList.Sort{$IFNDEF SGDEL_5}(0, vSortList.Count - 1){$ENDIF};
        for I := vList.Count - 1 downto 0 do
        begin
          vItem := vList[I];
          if vItem <> nil then
          begin
            vIndex := vSortList.IndexOf(vItem);
            if vIndex > -1 then
              vList[I] := nil;
          end;
        end;
      finally
        vSortList.Free;
      end;
    end;
  end;
end;

function TsgSelectionMatrix.RestoreClipRGN: Integer;
{$IFNDEF SG_FIREMONKEY}
var
  vRGN: HRGN;
{$ENDIF}
begin
  Result := -1;
{$IFNDEF SG_FIREMONKEY}
  if FRegions.Count > 0 then
  begin
    vRGN := FRegions.Pop;
    SelectClipRgn(Handle, vRGN);
    if vRGN <> 0 then
      DeleteObject(vRGN);
    Result := FRegions.Count - 1;
  end;
{$ENDIF}
end;

procedure TsgSelectionMatrix.SetSize(const APoint: TPoint);
var
  P, vCurSize: TPoint;
{$IFDEF SG_FIREMONKEY}
  vBitmap: TBitmap;
{$ENDIF}
begin
  P := APoint;
  vCurSize := GetSize;
  if P.X <= 0 then P.X := vCurSize.X;
  if P.Y <= 0 then P.Y := vCurSize.Y;
  if (P.X <> vCurSize.X) or (P.Y <> vCurSize.Y) then
  begin
    FEntities.Clear;
    FreeAndNil(FMarkers);
{$IFDEF SG_FIREMONKEY}
    FLinkPen.LinkCanvas := nil;
    FLinkBrush.LinkCanvas := nil;
    FreeCanvas;
    vBitmap := TBitmap.Create;
    SetSizeBmp(vBitmap, P.X, P.Y);
    FCanvas := TCanvasManager.CreateFromBitmap(vBitmap, TCanvasQuality.HighPerformance);
{$IFDEF SG_FM_WINDOWS}
    if TObject(FCanvas.Bitmap.Handle) is TGPBitmap then
      FGPBitmap := TGPBitmap(FCanvas.Bitmap.Handle)
    else
      FGPBitmap := nil;
{$ENDIF}
    FLinkPen.LinkCanvas := FCanvas;
    FLinkBrush.LinkCanvas := FCanvas;
{$ELSE}
{$IFDEF SG_HAS_WINAPI_INTERFACE}
{$IFDEF SG_THREAD_DRAW}
    if GetCurrentThreadId <> MainThreadID then
      while FCanvas.LockCount > 0 do;
    FCanvas.Lock; try
{$ENDIF}
    FDIBBitmap32.SetSize(P.X, P.Y);
{$IFDEF SG_THREAD_DRAW}
    finally FCanvas.Unlock; end;
{$ENDIF}
{$ELSE}
{$IFDEF SG_BITMAP_API}
   FBitmap.SetSize(P.X, P.Y);
{$ENDIF}
{$ENDIF}
{$ENDIF}
    ClearFull;
  end;
//  ClearFull;
end;

function TsgSelectionMatrix.SaveClipRGN: Integer;
{$IFNDEF SG_FIREMONKEY}
var
  vRGN: HRGN;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  vRGN := CreateRectRgn(0, 0, 0, 0);
  if GetClipRgn(Handle, vRGN) = 0 then
  begin
    DeleteObject(vRGN);
    vRGN := 0;
  end;
  FRegions.Push(vRGN);
{$ENDIF}
  Result := FRegions.Count;
end;

procedure TsgSelectionMatrix.SetFontColor(const Value: TColor);
begin
  Canvas.Font.Color := Value;
end;

{ TsgSimpleGraphicsObject }

function TsgSimpleGraphicsObject.CanUseExStockObjects: Boolean;
begin
{$IFNDEF SG_NON_WIN_PLATFORM}
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgSimpleGraphicsObject.Changed;
begin
  FValid := True;
end;

procedure TsgSimpleGraphicsObject.FreeObjectHandle;
begin
  Handle := 0;
end;

function TsgSimpleGraphicsObject.GetCurrentStockObject: THandle;
begin
  Result := 0;
end;

function TsgSimpleGraphicsObject.HandleAllocated: Boolean;
begin
  Result := Handle <> 0;
end;

procedure TsgSimpleGraphicsObject.NoValid;
begin
  FValid := False;
end;

procedure TsgSimpleGraphicsObject.SelectDefaultObject;
begin
{$IFNDEF SG_FIREMONKEY}
  if LinkCanvas <> nil then
    SelectObject(LinkCanvas.Handle, GetCurrentStockObject)
{$ENDIF}
end;

procedure TsgSimpleGraphicsObject.SetLinkCanvas(const Value: TCanvas);
begin
  FLinkCanvas := Value;
  NoValid;
end;

procedure TsgSimpleGraphicsObject.SetHandle(const Value: THandle);
begin
{$IFNDEF SG_FIREMONKEY}
  if (LinkCanvas = nil) {or (Value = FHandle)} then
    Exit;
  if HandleAllocated then
    DeleteObject(FHandle);
  FHandle := Value;
  if FHandle <> 0 then
    SelectObject(LinkCanvas.Handle, FHandle)
  else
    SelectDefaultObject;
{$ENDIF}
end;

constructor TsgSimpleGraphicsObject.Create;
begin
  inherited Create;
  FHandle := 0;
  FLinkCanvas := nil;
  FValid := False;
end;

destructor TsgSimpleGraphicsObject.Destroy;
begin
  if HandleAllocated then
    FreeObjectHandle;
  inherited Destroy;
end;

{ TsgSimpleBrush }

procedure TsgSimpleBrush.Changed;
{$IFNDEF SG_FIREMONKEY}
var
  vLogBrush: TLogBrush;
begin
  inherited Changed;
  if (FBrushData.Style = bsSolid) and (FBrushData.Bitmap = nil) and
    CanUseExStockObjects then
  begin
    Handle := GetCurrentStockObject;
    if LinkCanvas <> nil then
      SetDCBrushColorFunc(LinkCanvas.Handle, ColorToRGB(FBrushData.Color));
  end
  else
  begin
    vLogBrush.lbHatch := 0;
    vLogBrush.lbColor := ColorToRGB(FBrushData.Color);
    case FBrushData.Style of
      bsClear: vLogBrush.lbStyle := BS_HOLLOW;
    else
      vLogBrush.lbStyle := BS_SOLID;
    end;
    Handle := CreateBrushIndirect(vLogBrush);
  end;
{$ElSE}
begin
  inherited Changed;
  if Assigned(FLinkCanvas) then
  begin
    FLinkCanvas.Fill.Color := FBrushData.Color.AsBGRA;
    if FBrushData.Style = bsClear then
      FLinkCanvas.Fill.Kind := TBrushKind.None
    else
      FLinkCanvas.Fill.Kind := TBrushKind.Solid;
  end;
{$ENDIF}
end;

constructor TsgSimpleBrush.Create;
begin
  inherited Create;
  FBrushData.Color := clWhite;
  FBrushData.Style := bsClear;
end;

function TsgSimpleBrush.GetColor: TColor;
begin
  Result := FBrushData.Color;
end;

function TsgSimpleBrush.GetCurrentStockObject: THandle;
begin
{$IFDEF SG_FIREMONKEY}
  Result := 0;
{$ELSE}
  if CanUseExStockObjects then
    Result := GetStockObject(DC_BRUSH)
  else
    Result := GetStockObject(BLACK_BRUSH)
{$ENDIF}
end;

function TsgSimpleBrush.GetStyle: TBrushStyle;
begin
  Result := FBrushData.Style;
end;

procedure TsgSimpleBrush.SetColor(const Value: TColor);
begin
  if Value = FBrushData.Color then
    Exit;
  FBrushData.Color := Value;
  if FBrushData.Style = bsClear then // as VCL
    FBrushData.Style := bsSolid;
  NoValid;
end;

procedure TsgSimpleBrush.SetStyle(const Value: TBrushStyle);
begin
  if Value = FBrushData.Style then
    Exit;
  FBrushData.Style := Value;
  NoValid;
end;

{ TsgSimplePen }

procedure TsgSimplePen.Changed;
{$IFNDEF SG_FIREMONKEY}
const
  PenStyles: array[TPenStyle] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME
{$IFDEF SGDEL_2006}
    ,PS_USERSTYLE, PS_ALTERNATE
{$ENDIF}
{$IFDEF SGFPC}
   ,PS_USERSTYLE
{$ENDIF}
    );
var
  vLogPen : TLogPen;
begin
  inherited Changed;
  if (FPenData.Style = psSolid) and (FPenData.Width = 1) and
    CanUseExStockObjects then
  begin
    Handle := GetCurrentStockObject;
    if LinkCanvas <> nil then
      SetDCPenColorFunc(LinkCanvas.Handle, ColorToRGB(FPenData.Color));
  end
  else
  begin
    vLogPen.lopnStyle := PenStyles[FPenData.Style];
    vLogPen.lopnWidth.X := FPenData.Width;
    vLogPen.lopnColor := ColorToRGB(FPenData.Color);
    Handle := CreatePenIndirect(vLogPen);
  end;
{$ELSE}
begin
  inherited Changed;
  if Assigned(FLinkCanvas) then
  begin
    FLinkCanvas.Stroke.Color := FPenData.Color.AsBGRA;
    FLinkCanvas.Stroke.Thickness := FPenData.Width;
  end;
{$ENDIF}
end;

constructor TsgSimplePen.Create;
begin
  inherited Create;
  FPenData.Color := clWhite;
  FPenData.Width := 1;
  FPenData.Style := psSolid;
end;

function TsgSimplePen.GetColor: TColor;
begin
  Result := FPenData.Color;
end;

function TsgSimplePen.GetCurrentStockObject: THandle;
begin
{$IFDEF SG_FIREMONKEY}
  Result := 0;
{$ELSE}
  if CanUseExStockObjects then
    Result := GetStockObject(DC_PEN)
  else
    Result := GetStockObject(BLACK_PEN);
{$ENDIF}
end;

function TsgSimplePen.GetStyle: TPenStyle;
begin
  Result := FPenData.Style;
end;

function TsgSimplePen.GetWidth: Single;
begin
  Result := FPenData.Width;
end;

procedure TsgSimplePen.SetColor(const Value: TColor);
begin
  if Value = FPenData.Color then
    Exit;
  FPenData.Color := Value;
  NoValid;
end;

procedure TsgSimplePen.SetStyle(const Value: TPenStyle);
begin
  if Value = FPenData.Style then
    Exit;
  FPenData.Style := Value;
  NoValid;
end;

procedure TsgSimplePen.SetWidth(const Value: Single);
begin
  if Value = FPenData.Width then
    Exit;
  FPenData.Width := Ceil(Value);
  NoValid;
end;

//------------------------------------------------------------------------------

{ TsgSnapMatrix }

constructor TsgSnapMatrix.Create;
begin
  Inc(FColsCount);
  Inc(FRowsCount);
  GetMem(FCells, SizeOf(Pointer));
  FCells^[0] := nil;
  FCashe := TList.Create;
end;

destructor TsgSnapMatrix.Destroy;
var
  I: Integer;
  P: PSnapData;
begin
  for I := 0 to FColsCount * FRowsCount - 1 do
    if FCells^[I] <> nil then
      Dispose(FCells^[I]);
  for I := 0 to FCashe.Count - 1 do
  begin
    P := PSnapData(FCashe.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
    Dispose(P);
  end;
  FCashe.Free;
  FreeMem(FCells);
  inherited Destroy;
end;

function TsgSnapMatrix.CacheSnap(var P: PSnapData): Boolean;
begin
  Result := False;
  if FCashe.Count < cnstSnapMatrixCacheCount then
  begin
    if Assigned(P) then
    begin
{$IFDEF SG_THREAD_DRAW}
      MonitorEnter(Self);
      try
{$ENDIF}
        FCashe.Add(P);
{$IFDEF AUTOREFCOUNT}
        Finalize(P^);
{$ENDIF}
        FillChar(P^, SizeOf(TSnapData), 0);
        P := nil;
        Result := True;
{$IFDEF SG_THREAD_DRAW}
      finally
        MonitorExit(Self);
      end;
{$ENDIF}
    end;
  end
  else
  begin
    if Assigned(P) then
    begin
{$IFDEF SG_THREAD_DRAW}
      MonitorEnter(Self);
      try
{$ENDIF}
        Dispose(P);
        P := nil;
{$IFDEF SG_THREAD_DRAW}
      finally
        MonitorExit(Self);
      end;
{$ENDIF}
    end;
  end;
end;

procedure TsgSnapMatrix.Clear;
var
  I: Integer;
begin
  for I := 0 to FRowsCount * FColsCount - 1 do
    if Assigned(FCells^[I]) then
      CacheSnap(FCells^[I]);
end;

procedure TsgSnapMatrix.ClearRect(const ARect: TRect);
var
  Y, X: Integer;
  R: TRect;
begin
  R := ARect;
  NormRect(R);
  IntersectRect(R, R, Rect(0, 0, FColsCount - 1, FRowsCount - 1));
{$IFDEF SG_THREAD_DRAW}
  MonitorEnter(Self);
  try
{$ENDIF}
    for X := R.Left to R.Right do
      for Y := R.Top to R.Bottom do
        ClearCell(X, Y);
{$IFDEF SG_THREAD_DRAW}
  finally
    MonitorExit(Self);
  end;
{$ENDIF}
end;

function TsgSnapMatrix.ForceCell(ACol, ARow: Integer): PSnapData;
begin
  Result := FCells^[ARow * FColsCount + ACol];
  if not Assigned(Result) then
  begin
{$IFDEF SG_THREAD_DRAW}
    MonitorEnter(Self);
    try
{$ENDIF}
      if FCashe.Count > 0 then
      begin
        Result := PSnapData(FCashe.Last);
        FCashe.Delete(FCashe.Count - 1);
      end
      else
      begin
        New(Result);
        FillChar(Result^, SizeOf(TSnapData), 0);
      end;
      FCells^[ARow * FColsCount + ACol] := Result;
{$IFDEF SG_THREAD_DRAW}
    finally
      MonitorExit(Self);
    end;
{$ENDIF}
  end;
end;

function TsgSnapMatrix.GetCell(ACol, ARow: Integer): PSnapData;
begin
  if IsCellValid(ACol, ARow) then
    Result := ForceCell(ACol, ARow)
  else
    Result := nil;
end;

function TsgSnapMatrix.IsCellValid(const ACol, ARow: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < FColsCount) and (ARow >= 0)
    and (ARow < FRowsCount);
end;

procedure TsgSnapMatrix.Resize(const AColsCount, ARowsCount: Integer);
var
  I, vNewColCount, vNewRowCount: Integer;
begin
  if (ARowsCount <> FRowsCount) or (AColsCount <> FColsCount) then
  begin
    Clear;
    vNewRowCount := ARowsCount;
    if vNewRowCount <= 0 then vNewRowCount := 1;
    vNewColCount := AColsCount;
    if vNewColCount <= 0 then vNewColCount := 1;
    if vNewColCount * vNewRowCount > FColsCount * FRowsCount then
    begin
      for I := 0 to FColsCount * FRowsCount - 1 do
        if FCells^[I] <> nil then
          Dispose(FCells^[I]);
      FreeMem(FCells);
      FRowsCount := vNewRowCount;
      FColsCount := vNewColCount;
      GetMem(FCells, FRowsCount * FColsCount * SizeOf(Pointer));
      FillChar(FCells^, FRowsCount * FColsCount * SizeOf(Pointer), 0);
    end
    else
    begin
      for I := vNewColCount * vNewRowCount to FColsCount * FRowsCount - 1 do
        if FCells^[I] <> nil then
        begin
          CacheSnap(FCells^[I]);
          FCells^[I] := nil;
        end;
      FRowsCount := vNewRowCount;
      FColsCount := vNewColCount;
    end;
  end;
end;

procedure TsgSnapMatrix.UpdateCell(ACol, ARow: Integer; const AValue: TSnapData);
var
  vObject: TObject;
  P: PSnapData;
begin
  if IsCellValid(ACol, ARow) then
  begin
    P := ForceCell(ACol, ARow);
    if AValue.Entity <> P^.Entity then
      vObject := P^.Entity
    else
      vObject := P^.EntityIntersected;
    System.Move(AValue, P^, SizeOf(TSnapData));
    if (AValue.EntityIntersected = nil) and (P^.Entity <> vObject) then
      P^.EntityIntersected := vObject;
  end;
end;

procedure TsgSnapMatrix.ClearCell(const ACol, ARow: Integer);
begin
  if FCells^[ARow * FColsCount + ACol] <> nil then
  begin
{$IFDEF AUTOREFCOUNT}
    Finalize(FCells^[ARow * FColsCount + ACol]^);
{$ENDIF}
    FillChar(FCells^[ARow * FColsCount + ACol]^, SizeOf(TSnapData), 0);
  end;
end;
//------------------------------------------------------------------------------

function SuccOrient(AOrient: TPixelOrient): TPixelOrient;
begin
  if AOrient = High(TPixelOrient) then
    Result := Low(TPixelOrient)
  else
    Result := Succ(AOrient);
end;

function PredOrient(AOrient: TPixelOrient): TPixelOrient;
begin
  if AOrient = Low(TPixelOrient) then
    Result := High(TPixelOrient)
  else
    Result := Pred(AOrient);
end;

function SuccOrient2(AOrient: TPixelOrient): TPixelOrient;
begin
  Result := SuccOrient(SuccOrient(AOrient));
end;

procedure SetOrient(APixel: PPixelData; const AOrient: TPixelOrient);
  // (AOrient = poRight => Orients = {[s3],[s4],[s1],[s2]} )
  // (AOrient = poTop => Orients = {[s4],[s1],[s2],[s3]} )
var
  I: TPixelOrient;
  vFlag: TPixelSide;
begin
  vFlag := ps4;
  I := AOrient;
  repeat
    APixel^.Orients[I] := APixel^.Orients[I] + [vFlag];
    I := PredOrient(I);
    vFlag := Succ(vFlag);
  until I = AOrient;
end;

 { TBmpTraversal }

procedure TBmpTraversal.Clear;
begin
  while FContours.Count > 0 do
    DeleteContour(PContour(FContours.Last));
end;

constructor TBmpTraversal.Create;
begin
  inherited Create;
  FPoint := Point(0, 0);
  FIslandsContours := TList.Create;
  FContours := TList.Create;
  FContour := nil;
  FBitmap := nil;
  FSelMatrix := nil;
  FDataBmp := nil;
  FInputType := ibtBitmap;
  FOrient := poLeft;
  FMode := [];
end;

destructor TBmpTraversal.Destroy;
begin
  Clear;
  FreeAndNil(FContours);
  FreeAndNil(FDataBmp);
  FreeAndNil(FIslandsContours);
  inherited Destroy;
end;

function TBmpTraversal.GetEnt(const APt: TPoint): Pointer;
var
  vColor: TColor absolute Result;
begin
  if InputType = ibtBitmap then
  begin
    Result := Pointer(GetPixel(APt.X, APt.Y));
    if vColor = BackColor then
      Result := nil;
  end
  else // InputType = ibtSelMatrix
  begin
    Result := SelMatrix.Matrix[APt.X, APt.Y];
  end;
end;

function TBmpTraversal.GetData(const APt: TPoint): PPixelData;
begin
  Result := FDataBmp.Matrix[APt.X, APt.Y];
end;

procedure TBmpTraversal.SetBitMap(const Value: TBitmap);
begin
  FBitmap := Value;
{$IFDEF SG_FIREMONKEY}
  FBitmap.OnChange := BitmapOnChange;
{$ENDIF}
  InputType := ibtBitmap;
  SetDataSize(FBitmap.Width, FBitmap.Height);
end;

procedure TBmpTraversal.SetData(const APoint: TPoint; const AData: PPixelData);
var
  vPrevData: PPixelData;
begin
  vPrevData := GetData(APoint);
  if (vPrevData <> nil) and (AData <> nil) then
    AData^.LowerPixel := vPrevData;
  FDataBmp.AddElement(AData);
  SetPixel(APoint.X, APoint.Y, FDataBmp.CurrColor);
end;

procedure TBmpTraversal.SetDataSize(AWidth, AHeight: Integer);
begin
  FreeAndNil(FDataBmp);
  FDataBmp := TsgSelectionMatrix.Create;
  FDataBmp.SearchMode := smPoint;
  FDataBmp.Size := Point(AWidth, AHeight);
  FBmpRect := Rect(0, 0, AWidth - 1, AHeight - 1);
  FDataBmp.ClearFull;
end;

procedure TBmpTraversal.SetPixel(X, Y: Integer; Value: TColor);
begin
{$IFDEF SG_FIREMONKEY}
  FBitData.SetPixel(X, Y, Value);
{$ELSE}
  FDataBmp.Canvas.Pixels[X, Y] := Value;
{$ENDIF}
end;

procedure TBmpTraversal.SetSelMatrix(const Value: TsgSelectionMatrix);
begin
  FSelMatrix := Value;
  InputType := ibtSelMatrix;
  SetDataSize(FSelMatrix.Size.X, FSelMatrix.Size.Y);
end;

function TBmpTraversal.DeltaFromOrient(AOrient: TPixelOrient): TPoint;
begin
  case AOrient of
    poLeft: Result := cLeftVector;
    poTop: Result := cTopVector;
    poRight: Result := cRightVector;
    poBottom: Result := cBottomVector;
  end;
end;

function TBmpTraversal.OrientFromDelta(dPt: TPoint): TPixelOrient;
begin
  if IsEqualPoints(dPt, cLeftVector) then
    Result := poLeft
  else if IsEqualPoints(dPt, cTopVector) then
    Result := poTop
  else if IsEqualPoints(dPt, cRightVector) then
    Result := poRight
  else if IsEqualPoints(dPt, cBottomVector) then
    Result := poBottom
  else
    Result := poLeft;
end;

function TBmpTraversal.OffsetPoint(const APt: TPoint; const AOrient: TPixelOrient): TPoint;
var
  dPt: TPoint;
begin
  dPt := DeltaFromOrient(AOrient);
  Result := AddPoint(APt, dPt);
end;

function TBmpTraversal.GetNewPixel(const APoint: TPoint;
  const AOrient: TPixelOrient; AContour: PContour): PPixelData;
begin
  New(Result);
  AContour.Pixels.Add(Result);
  Result^.Pt := APoint;
  Result^.Orients[poLeft] := [];
  Result^.Orients[poTop] := [];
  Result^.Orients[poRight] := [];
  Result^.Orients[poBottom] := [];
  SetOrient(Result, AOrient);
  Result^.Contour := nil;
end;

function TBmpTraversal.GetNearestPoint(const APoint: TPoint; var AOrient: TPixelOrient): TPoint;
var
  vSearchMode: TsgSearchMode;
  vPrecision: Integer;
  vDelta, vNearest: TPoint;
begin
  Result := APoint;
  vSearchMode := SelMatrix.SearchMode;
  vPrecision := SelMatrix.Precision;
  try
    SelMatrix.SearchMode := smFillability;
    SelMatrix.Precision := 150;
    if not SelMatrix.FindNearestPoint(APoint, vNearest) then
      Exit;
  finally
    SelMatrix.SearchMode := vSearchMode;
    SelMatrix.Precision := vPrecision;
  end;
  vDelta := Point(Sign(APoint.X - vNearest.X), Sign(APoint.Y - vNearest.Y));
  if not HasEnt(Point(vNearest.X + vDelta.X, vNearest.Y)) then
    vDelta.Y := 0
  else
    if not HasEnt(Point(vNearest.X, vNearest.Y + vDelta.Y)) then
      vDelta.X := 0
    else
      Exit; // FindNearestPoint not work correct
   AOrient := OrientFromDelta(Point(-vDelta.X, -vDelta.Y));
   Result := AddPoint(vNearest, vDelta);
end;

function TBmpTraversal.GetNewContour: PContour;
begin
  New(Result);
  FContours.Add(Result);
  Result.Entities := TList.Create;
  Result.Pixels := TList.Create;
  Result.Box := cnstBad2DRect;
end;

procedure TBmpTraversal.DeletePixel(APixel: PPixelData);
begin
  if APixel = nil then
    Exit;
  Dispose(APixel);
end;

procedure TBmpTraversal.DeleteContour(AContour: PContour);
var
  I: Integer;
begin
  if AContour = nil then
    Exit;
  for I := 0 to AContour.Pixels.Count - 1 do
    DeletePixel(AContour.Pixels[I]);
  AContour.Pixels.Free;
  AContour.Entities.Free;
  FContours.Delete(FContours.IndexOf(AContour));
  Dispose(AContour);
end;

function TBmpTraversal.ScanLine(const Pt: TPoint; var OutPt: TPoint): Boolean;
var
  vNextPt: TPoint;
begin
  Result := False;
  if (not PtInRect(FBmpRect, Pt)) or HasEnt(Pt) then
    Exit;
  vNextPt := Pt;
  while (not Result) and PtInRect(FBmpRect, vNextPt) do
  begin
    OutPt := vNextPt;
    vNextPt := OffsetPoint(vNextPt, Orient);
    Result := HasEnt(vNextPt);
  end;
end;

function TBmpTraversal.GetNextPixel(APixel: PPixelData; AEntList: TList;
  AContour: PContour): PPixelData;
var
  I, vStartI: TPixelOrient;
  vNextPt: TPoint;
begin
  Result := nil;
  for vStartI := Low(TPixelOrient) to High(TPixelOrient) do
    if s1 in APixel.Orients[vStartI] then
      Break;
  I := vStartI;
  repeat
    vNextPt := OffsetPoint(APixel^.Pt, I);
    if not PtInRect(FBmpRect, vNextPt) then
      Exit;
    if HasEnt(vNextPt) then
    begin
      APixel.Orients[I] := APixel.Orients[I] + [psEnt];
      AddEnt(vNextPt, AEntList);
    end
    else
    begin
      Result := GetNewPixel(vNextPt, SuccOrient2(I), AContour);
      APixel.Orients[I] := APixel.Orients[I] + [psOut];
      Break;
    end;
    I := PredOrient(I)
  until I = vStartI;
end;

function TBmpTraversal.GetPixel(X, Y: Integer): TColor;
begin
{$IFDEF SG_FIREMONKEY}
  Result := FBitData.GetPixel(X, Y);
{$ELSE}
  Result := FBitmap.Canvas.Pixels[X, Y];
{$ENDIF}
end;

function TBmpTraversal.HasEnt(const APt: TPoint): Boolean;
var
  vBackColor: Pointer;
begin
  if InputType = ibtBitmap then
    vBackColor := Pointer(BackColor)
  else
    vBackColor := nil;
  Result := GetEnt(APt) <> Pointer(vBackColor);
end;

function TBmpTraversal.ScanContour(const APt: TPoint; var ExtensionPt: TPoint): PContour;
var
  vPtData, vPtDataFirst: PPixelData;
  vNextEntPt: TPoint;
begin
  Result := nil;
  ExtensionPt := APt;
  vNextEntPt := OffsetPoint(APt, Orient);
  if not HasEnt(vNextEntPt) then
    Exit;
  Result := GetNewContour;
  try
    Result^.Box := Rect(APt.X, APt.Y, APt.X, APt.Y);
    AddEnt(vNextEntPt, Result^.Entities);
    vPtDataFirst := GetNewPixel(APt, SuccOrient2(PredOrient(Orient)), Result);
    vPtData := vPtDataFirst;
    repeat
      Dec(FIterationsLast);
      if FIterationsLast = 0 then // infinite cycle protection
      begin
        Result := nil;
        Exit;
      end;
      SetData(vPtData^.Pt, vPtData);
      vPtData^.Contour := Result;
      ExpandRect(Result^.Box, vPtData^.Pt);
      CalcExtensionPoint(APt, vPtData, ExtensionPt);

      vPtData := GetNextPixel(vPtData, Result^.Entities, Result);
      if vPtData = nil then // contour can't be calculated
      begin
        DeleteContour(Result);
        Result := nil;
        Exit;
      end;
    until IsEqualPoints(vPtDataFirst^.Pt, vPtData^.Pt);
  except
    DeleteContour(Result);
  end;
end;

procedure TBmpTraversal.AddEnt(const APt: TPoint; AList: TList);
var
  vEnt: Pointer;
begin
  if (AList = nil) or not HasEnt(APt) then
    Exit;
  vEnt := GetEnt(APt);
  if AList.IndexOf(vEnt) < 0 then
    AList.Add(vEnt)
end;

{$IFDEF SG_FIREMONKEY}
procedure TBmpTraversal.BitmapOnChange(Sender: TObject);
begin
  if Assigned(FBitmap) then
    FBitmap.Map(TMapAccess.Read, FBitData);
end;
{$ENDIF}

procedure TBmpTraversal.CalcContours;
var
  vOutPt, vExtPt: TPoint;
  vCont: PContour;
begin
  if tmSnapToNearest in Mode then
    StartPoint := GetNearestPoint(UserPoint, FOrient)
  else
    StartPoint := UserPoint;
  FIterationsLast := 100000;
  vOutPt := StartPoint;
  while True do
  begin
    if not ScanLine(vOutPt, vOutPt) then
      Break;
    vCont := ScanContour(vOutPt, vExtPt);
    if vCont = nil then
      Break;
    vOutPt := vExtPt;
    if (tmFindOneContour in Mode) or IsPointInContour(StartPoint, vCont) then
    begin
      FContour := vCont;
      Break;
    end
    else
      IslandsContours.Add(vCont);
  end;
end;

procedure TBmpTraversal.CalcExtensionPoint(const APt: TPoint; const APtData: PPixelData;
  var ExtensionPt: TPoint);
begin
  // calc box
  case Orient of
    poLeft:
      if APtData^.Pt.Y = APt.Y then
        ExtensionPt.X := Math.Min(APtData^.Pt.X, ExtensionPt.X);
    poTop:
      if APtData^.Pt.X = APt.X then
        ExtensionPt.Y := Math.Min(APtData^.Pt.Y, ExtensionPt.Y);
    poRight:
      if APtData^.Pt.Y = APt.Y then
        ExtensionPt.X := Math.Max(APtData^.Pt.X, ExtensionPt.X);
    poBottom:
      if APtData^.Pt.X = APt.X then
        ExtensionPt.Y := Math.Max(APtData^.Pt.Y, ExtensionPt.Y);
  end;
end;

function TBmpTraversal.IsPointInContour(const APoint: TPoint; const ACountour: PContour): Boolean;
var
  X: Integer;
  vPtData: PPixelData;

  function CheckPt(APt: TPoint): Boolean;
  var
    vEnt: Pointer;
  begin
    Result := HasEnt(APt);
    if not Result then
      Exit;
    vEnt := GetEnt(APt);
    Result := ACountour.Entities.IndexOf(vEnt) > -1;
  end;

begin
  Result := False;
  for X := ACountour^.Box.Left to ACountour^.Box.Right do
  begin
    vPtData := GetData(Point(X, StartPoint.Y));
    if vPtData <> nil then
    begin
      Result := CheckPt(Point(X - 1, APoint.Y));
      if Result then
        Break
      else
      begin
        if CheckPt(Point(X + 1, APoint.Y)) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

function TBmpTraversal.GetContourEnts: TList;
begin
  if Contour <> nil then
    Result := Contour.Entities
  else
    Result := nil;
end;

initialization
{$IFNDEF SG_NON_WIN_PLATFORM}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    try
      Gdi32HModule := LoadLibrary(PChar(gdi32));
      if Gdi32HModule <> 0 then
      begin
        SetDCPenColorFunc := GetProcAddress(Gdi32HModule, 'SetDCPenColor');
        SetDCBrushColorFunc := GetProcAddress(Gdi32HModule, 'SetDCBrushColor');
      end;
    except
    end;
{$ENDIF}

finalization
{$IFNDEF SG_NON_WIN_PLATFORM}
  if Gdi32HModule <> 0 then
    FreeLibrary(Gdi32HModule);
{$ENDIF}

end.

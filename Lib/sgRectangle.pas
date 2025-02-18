{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{   Implements the functionality of the clipping rectangle   }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgRectangle;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, Types,
{$ENDIF}
  SysUtils, Classes, Controls, Forms, Messages, Graphics, sgConsts
  {$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
  {$ENDIF}
  ;

type
  TsgRectangleMarker = (rmTL, rmTM, rmTR, rmMR, rmBR, rmBM, rmBL, rmML, rmMM);

  TsgRectangle = class;

  TsgCustomPaintItem = class
  protected
    procedure Draw(const ARect: TRect; const ACanvas: TCanvas); virtual; abstract;
    function GetCanvas: TCanvas; virtual; abstract;
    procedure Paint; virtual;
    procedure PaintWindow(ADC: HDC); virtual;
    property Canvas: TCanvas read GetCanvas;
  end;

  TsgStyledItem = class(TsgCustomPaintItem)
  private
    FBrush: TBrush;
    FBrushStore: TBrush;
    FPen: TPen;
    FPenStore: TPen;
    procedure SetBrush(const AValue: TBrush);
    procedure SetPen(const AValue: TPen);
  protected
    procedure Draw(const ARect: TRect; const ACanvas: TCanvas); override;
    procedure StyleChange(Sender: TObject); virtual;
    procedure Initialize; virtual;
    procedure RestoreStyle(const ACanvas: TCanvas); virtual;
    procedure ApplyStyle(const ACanvas: TCanvas); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
  end;

  TsgMarker = class(TsgStyledItem)
  private
    FID: TsgRectangleMarker;
    FPosition: TFPoint;
    FRectangle: TsgRectangle;
    FVisibility: Boolean;
    FVisible: Boolean;
    function GetBoundsRect: TRect;
    function GetCursor: TCursor;
    function GetPoint: TPoint;
    function GetPoint3D: TFPoint;
    function GetVisibility: Boolean;
    procedure SetID(const AValue: TsgRectangleMarker);
    procedure SetPoint(const AValue: TPoint);
    procedure SetPoint3D(const AValue: TFPoint);
    procedure SetPosition(const AValue: TFPoint);
    procedure SetVisibility(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure ApplyPos(const AX, AY, AZ: Double; const AMatrix: TFMatrix);
    procedure ApplyPosition(const APoint: TFPoint; const AMatrix: TFMatrix);
    procedure Draw(const ARect: TRect; const ACanvas: TCanvas); override;
    function GetBoundsRectByRect: TRect;
    function GetCanvas: TCanvas; override;
    procedure Initialize; override;
    procedure Invalidate;
    procedure MouseHandler(var Message: TWMMouse);
    procedure Paint; override;
    procedure PaintHandler(var Message: TWMPaint);
    procedure ApplyStyle(const ACanvas: TCanvas); override;
    procedure StyleChange(Sender: TObject); override;
    property Rectangle: TsgRectangle read FRectangle;
    property Visibility: Boolean read GetVisibility write SetVisibility;
  public
    constructor Create(const AOwner: TsgCustomPaintItem); virtual;
{$IFNDEF SGDEL_4}
    procedure Dispatch(var Message); virtual;
{$ELSE}
    procedure Dispatch(var Message); override;
{$ENDIF}
    property BoundsRect: TRect read GetBoundsRect;
    property Canvas: TCanvas read GetCanvas;
    property Cursor: TCursor read GetCursor;
    property ID: TsgRectangleMarker read FID write SetID;
    property Point: TPoint read GetPoint write SetPoint;
    property Point3D: TFPoint read GetPoint3D write SetPoint3D;
    property Position: TFPoint read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TsgRectangle = class(TsgStyledItem)
  private
    FActive: Boolean;
    FCanvas: TCanvas;
    FCapture: Boolean;
    FCaptureStarted: Boolean;
    FCaptureStop: Boolean;
    FControl: TControl;
    FDownPosition: TFPoint;
    FDownPositionNoResize: TFPoint;
    FEnterRectEvent: TNotifyEvent;
    FHint: THintWindow;
    FHintHiden: Boolean;
    FMarker: TsgMarker;
    FMarkerCaptured: Boolean;
    FMarkers: array[TsgRectangleMarker] of TsgMarker;
    FMarkerSize: Integer;
    FMarkersVisible: Boolean;
    FMatrix: TFMatrix;
    FNoResize: Boolean;
    FRect: TRect;
    FRotated: Boolean;
    FShowHint: Boolean;
    procedure DoMarkerCapture(const AValue: Boolean);
    function DoRectChanging(Sender: TObject; const APoint: TPoint): Integer;
    function GetBox: TFRect;
    function GetCursor: TCursor;
    function GetDrawMatrix: TFMatrix;
    function GetInvertDrawMatrix: TFMatrix;
    function GetMarkerAtPos(const APoint: TPoint): TsgMarker;
    function GetMarkers(Index: TsgRectangleMarker): TsgMarker;
    function GetMatrix: TFMatrix;
    function GetRect: TRect;
    function LoadMarkersPos(const APoint1, APoint2: TFPoint): TFRect;
    procedure PerformCapture(const ACanvas: TCanvas);
    procedure PerformCaptureStop(const ACanvas: TCanvas);
    procedure SetActive(const AValue: Boolean);
    procedure SetBox(const AValue: TFRect);
    procedure SetControl(const AValue: TControl);
    procedure SetCursor(const AValue: TCursor);
    procedure SetMarker(const AValue: TsgMarker);
    procedure SetMarkerSize(const Value: Integer);
    procedure SetMarkersVisible(const AValue: Boolean);
    procedure SetNoResize(const AValue: Boolean);
    procedure SetRect(const AValue: TRect);
    procedure SetShowHint(const AValue: Boolean);
  protected
    procedure ActivateHint;
    procedure ConnectTo(const AControl: TControl);
    procedure Disconnect;
    function DoMouseDown(Sender: TObject;
      const APoint: TPoint): Integer; dynamic;
    function DoMouseMove(Sender: TObject;
      const APoint: TPoint): Integer; dynamic;
    function DoMouseUp(Sender: TObject;
      const APoint: TPoint): Integer;  dynamic;
    procedure DrawMarkers(const ACanvas: TCanvas);
    function GetCanvas: TCanvas; override;
    function GetWindowHandle: HWND;
    procedure HideHint;
    procedure Initialize; override;
    procedure MouseHandler(var Message: TWMMouse);
    procedure Paint; override;
    procedure PaintHandler(var Message: TWMPaint);
    procedure PictureMove; virtual;
    procedure ScaleChanging; virtual;
    property Capture: Boolean read FCapture write FCapture;
    property Rotated: Boolean read FRotated write FRotated;
  public
    constructor Create(const AControl: TWinControl); virtual;
    destructor Destroy; override;
{$IFNDEF SGDEL_4}
    procedure Dispatch(var Message); virtual;
{$ELSE}
    procedure Dispatch(var Message); override;
{$ENDIF}
    function GetCoord(const APoint: TPoint): TFPoint;
    function GetPoint(const ACoord: TFPoint): TPoint;
    function GetSnapedCoord(const APoint: TPoint): TFPoint;
    function IsPointInMarker(const APoint: TPoint): Boolean;
    procedure InvalidateClipRect;
    property Active: Boolean read FActive write SetActive;
    property Box: TFRect read GetBox write SetBox;
    property Canvas: TCanvas read GetCanvas;
    property Control: TControl read FControl write SetControl;
    property Cursor: TCursor read GetCursor write SetCursor;
    property Marker: TsgMarker read FMarker write SetMarker;
    property Markers[Index: TsgRectangleMarker]: TsgMarker read GetMarkers;
    property MarkerSize: Integer read FMarkerSize write SetMarkerSize;
    property MarkersVisible: Boolean read FMarkersVisible write SetMarkersVisible;
    property NoResize: Boolean read FNoResize write SetNoResize;
    property OnEnterRect: TNotifyEvent read FEnterRectEvent write FEnterRectEvent;
    property Rect: TRect read GetRect write SetRect;
    property ShowHint: Boolean read FShowHint write SetShowHint;
  end;

implementation

uses
  Math, sgFunction, sgDrawingNavigator, sgTools;

const
  cnstMarkerSize: TPoint = (X: 10; Y: 10);

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWincontrol);
  TsgDNAccess = class(TsgDrawingNavigator);

function CalcPos(const ACoordX, ACoordY, ACoordZ: Double;
  const AMatrix: TFMatrix): TFPoint;
begin
  Result := FPointXMat(MakeFPoint(ACoordX, ACoordY, ACoordZ), AMatrix);
end;

function GetMarkerCursor(const AMarkerID: TsgRectangleMarker): TCursor;
begin
  case AMarkerID of
    rmTL, rmBR: Result := TCursor(-8); //crSizeNWSE
    rmTR, rmBL: Result := TCursor(-6); //crSizeNESW;
    rmTM, rmBM: Result := TCursor(-7); //crSizeNS;
    rmML, rmMR: Result := TCursor(-9); //crSizeWE;
  else
    Result := TCursor(-22);            //crSizeAll;
  end;
end;

{ TsgCustomPaintItem }

procedure TsgCustomPaintItem.Paint;
begin
end;

procedure TsgCustomPaintItem.PaintWindow(ADC: HDC);
var
  vLocked: Boolean;
begin
  vLocked := Canvas.TryLock;
  try
    Canvas.Handle := ADC;
    try
      Paint;
    finally
      Canvas.Handle := 0;
    end;
  finally
    if vLocked then
      Canvas.UnLock;
  end;
end;

{ TsgStyledItem }

procedure TsgStyledItem.StyleChange(Sender: TObject);
begin
end;

constructor TsgStyledItem.Create;
begin
  inherited Create;
  FBrush := TBrush.Create;
  FBrushStore := TBrush.Create;
  FPen := TPen.Create;
  FPenStore := TPen.Create;
  Initialize;
  FBrush.OnChange := StyleChange;
  FPen.OnChange := StyleChange;
end;

destructor TsgStyledItem.Destroy;
begin
  FBrush.Free;
  FBrushStore.Free;
  FPen.Free;
  FPenStore.Free;
  inherited Destroy;
end;

procedure TsgStyledItem.Draw(const ARect: TRect; const ACanvas: TCanvas);
begin
  ApplyStyle(ACanvas);
  try
    ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  finally
    RestoreStyle(ACanvas);
  end;
end;

procedure TsgStyledItem.Initialize;
begin
end;

procedure TsgStyledItem.RestoreStyle(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Assign(FBrushStore);
  ACanvas.Pen.Assign(FPenStore);
end;

procedure TsgStyledItem.ApplyStyle(const ACanvas: TCanvas);
begin
  FBrushStore.Assign(ACanvas.Brush);
  FPenStore.Assign(ACanvas.Pen);
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Pen.Assign(FPen);
end;

procedure TsgStyledItem.SetBrush(const AValue: TBrush);
begin
  FBrush.Assign(AValue);
end;

procedure TsgStyledItem.SetPen(const AValue: TPen);
begin
  FPen.Assign(AValue);
end;

{ TsgRectangle }

procedure TsgRectangle.DoMarkerCapture(const AValue: Boolean);
begin
  FMarkerCaptured := AValue;
  FCapture := True;
  if FNoResize and FMarkerCaptured then
  begin
    Cursor := crDrag;
    if FControl is TWinControl then
      FControl.Perform(WM_SETCURSOR, TWinControl(FControl).Handle, HTCLIENT);
  end;
end;

function TsgRectangle.DoRectChanging(Sender: TObject;
  const APoint: TPoint): Integer;
var
  vID: TsgRectangleMarker;
  vCoord, vPoint3D, vOffset: TFPoint;
  vDrawMatrix, vInvertDrawMatrix: TFMatrix;
begin
  Result := 0;
  if Sender is TsgMarker then
  begin
    vDrawMatrix := GetDrawMatrix;
    vInvertDrawMatrix := FMatInverse(vDrawMatrix);
    vCoord := GetSnapedCoord(APoint);
    if FControl is TsgDrawingNavigator then
      if TsgDNAccess(FControl).SnapControl.SnappedEntity = nil then
        vCoord := FPointXMat(MakeFPoint(APoint.X, APoint.Y, 0), vInvertDrawMatrix);
    if NoResize and Capture and FMarkerCaptured then
    begin
      vID := rmMM;
      vOffset := SubFPoint(vCoord, FDownPositionNoResize);
      FDownPositionNoResize := vCoord;
    end
    else
    begin
      vID := FMarker.ID;
      vOffset := SubFPoint(vCoord, FMarkers[rmMM].Position);
    end;
    case vID of
      rmTL:
        begin
          if (APoint.X > FMarkers[rmBR].Point3D.X)
             and (APoint.Y > FMarkers[rmBR].Point3D.Y) then
            Marker := FMarkers[rmBR]
          else
            if APoint.X > FMarkers[rmTR].Point3D.X then
              Marker := FMarkers[rmTR]
            else
              if APoint.Y > FMarkers[rmBL].Point3D.Y then
                Marker := FMarkers[rmBL];
          LoadMarkersPos(vCoord, FMarkers[rmBR].Position);
        end;
      rmTM:
        begin
          if APoint.Y > FMarkers[rmBM].Point3D.Y then
            Marker := FMarkers[rmBM];
          vPoint3D := FMarkers[rmTL].Point3D;
          vPoint3D.Y := APoint.Y;
          vCoord := FPointXMat(vPoint3D, vInvertDrawMatrix);
          LoadMarkersPos(vCoord, FMarkers[rmBR].Position);
        end;
      rmTR:
        begin
          if (APoint.X < FMarkers[rmBL].Point3D.X)
             and (APoint.Y > FMarkers[rmBL].Point3D.Y) then
            Marker := FMarkers[rmBL]
          else
            if APoint.X < FMarkers[rmTL].Point3D.X then
              Marker := FMarkers[rmTL]
            else
              if APoint.Y > FMarkers[rmBR].Point3D.Y then
                Marker := FMarkers[rmBR];
          LoadMarkersPos(vCoord, FMarkers[rmBL].Position);
        end;
      rmML:
        begin
          if APoint.X > FMarkers[rmMR].Point3D.X then
            Marker := FMarkers[rmMR];
          vPoint3D := FMarkers[rmBL].Point3D;
          vPoint3D.X := APoint.X;
          vCoord := FPointXMat(vPoint3D, vInvertDrawMatrix);
          LoadMarkersPos(FMarkers[rmTR].Position, vCoord);
        end;
      rmMM:
        begin
          LoadMarkersPos(AddFPoint(FMarkers[rmTL].Position, vOffset),
            AddFPoint(FMarkers[rmBR].Position, vOffset));
        end;
      rmMR:
        begin
          if APoint.X < FMarkers[rmML].Point3D.X then
            Marker := FMarkers[rmML];
          vPoint3D := FMarkers[rmBR].Point3D;
          vPoint3D.X := APoint.X;
          vCoord := FPointXMat(vPoint3D, vInvertDrawMatrix);
          LoadMarkersPos(FMarkers[rmTL].Position, vCoord);
        end;
      rmBL:
        begin
          if (APoint.X > FMarkers[rmTR].Point3D.X)
             and (APoint.Y < FMarkers[rmTR].Point3D.Y) then
            Marker := FMarkers[rmTR]
          else
            if APoint.X > FMarkers[rmBR].Point3D.X then
              Marker := FMarkers[rmBR]
            else
              if APoint.Y < FMarkers[rmTL].Point3D.Y then
                Marker := FMarkers[rmTL];
          LoadMarkersPos(FMarkers[rmTR].Position, vCoord);
        end;
      rmBM:
        begin
          if APoint.Y < FMarkers[rmTM].Point3D.Y then
            Marker := FMarkers[rmTM];
          vPoint3D := FMarkers[rmBR].Point3D;
          vPoint3D.Y := APoint.Y;
          vCoord := FPointXMat(vPoint3D, vInvertDrawMatrix);
          LoadMarkersPos(FMarkers[rmTL].Position, vCoord);
        end;
      rmBR:
        begin
          if (APoint.X < FMarkers[rmTL].Point3D.X)
             and (APoint.Y < FMarkers[rmTL].Point3D.Y) then
            Marker := FMarkers[rmTL]
          else
            if APoint.X < FMarkers[rmBL].Point3D.X then
              Marker := FMarkers[rmBL]
            else
              if APoint.Y < FMarkers[rmTR].Point3D.Y then
                Marker := FMarkers[rmTR];
          LoadMarkersPos(FMarkers[rmTL].Position, vCoord);
        end;
    end; { case TsgMarker(Sender).ID }
  end;
end;

function TsgRectangle.GetBox: TFRect;
begin
  Result.TopLeft := FMArkers[rmTL].Position;
  Result.BottomRight := Result.TopLeft;
  ExpandFRect(Result, FMArkers[rmBR].Position);
end;

function TsgRectangle.GetCursor: TCursor;
begin
  if Assigned(FControl) then
    Result := FControl.Cursor
  else
    Result := TCursor(6);
end;

function TsgRectangle.GetDrawMatrix: TFMatrix;
begin
  if FControl is TsgDrawingNavigator then
    Result := TsgDNAccess(FControl).ControlTool.GetDrawMatrix
  else
    Result := cnstCrossYMat;
end;

function TsgRectangle.GetInvertDrawMatrix: TFMatrix;
begin
  if FControl is TsgDrawingNavigator then
    Result := TsgDNAccess(FControl).ControlTool.GetInvertDrawMatrix
  else
    Result := cnstCrossYMat;
end;

function TsgRectangle.GetWindowHandle: HWND;
var
  vDC: HDC;
begin
  Result := 0;
  if (FControl is TWinControl) and TWinControl(FControl).HandleAllocated then
    Result := TWinControl(FControl).Handle
  else
  begin
    vDC := TControlAccess(FControl).GetDeviceContext(Result);
    if (vDC <> 0) and (Result <> 0) then
      ReleaseDC(Result, vDC);
  end;
end;

function TsgRectangle.GetMarkerAtPos(const APoint: TPoint): TsgMarker;
var
  I: TsgRectangleMarker;
begin
  Result := nil;
  I := Low(FMarkers);
  while (Ord(I) < Ord(High(FMarkers))) and not Assigned(Result) do
    if PtInRect(FMarkers[I].BoundsRect, APoint) then
      Result := FMarkers[I]
    else
      I := Succ(I);
end;

function TsgRectangle.GetMarkers(Index: TsgRectangleMarker): TsgMarker;
begin
  Result := FMarkers[Index];
end;

function TsgRectangle.GetMatrix: TFMatrix;
begin
  if FControl is TsgDrawingNavigator then
    Result := TsgDNAccess(FControl).ControlTool.GetRealImageMatrix
  else
    Result := cnstIdentityMat;
end;

function TsgRectangle.GetRect: TRect;
begin
  if not FRotated then
  begin
    Result.TopLeft := FMarkers[rmTL].Point;
    Result.BottomRight := Result.TopLeft;
    ExpandRect(Result, FMarkers[rmBR].Point);
    FRect := Result;
  end
  else
    Result := FRect;
end;

function TsgRectangle.LoadMarkersPos(const APoint1, APoint2: TFPoint): TFRect;
var
  vMidPoint: TFPoint;
  vMatrix: TFMatrix;
begin
  vMatrix := GetMatrix;
  Result.TopLeft := FPointXMat(APoint1, vMatrix);
  Result.BottomRight := Result.TopLeft;
  ExpandFRect(Result, FPointXMat(APoint2, vMatrix));
  vMidPoint := MiddleFPoint(Result.TopLeft, Result.BottomRight);
  vMatrix := FMatInverse(vMatrix);
  FMarkers[rmTL].ApplyPos(Result.Left, Result.Top, vMidPoint.Z, vMatrix);
  FMarkers[rmBR].ApplyPos(Result.Right, Result.Bottom, vMidPoint.Z, vMatrix);
  FMarkers[rmTM].ApplyPos(vMidPoint.X, Result.Top, vMidPoint.Z, vMatrix);
  FMarkers[rmBM].ApplyPos(vMidPoint.X, Result.Bottom, vMidPoint.Z, vMatrix);
  FMarkers[rmTR].ApplyPos(Result.Right, Result.Top, vMidPoint.Z, vMatrix);
  FMarkers[rmBL].ApplyPos(Result.Left, Result.Bottom, vMidPoint.Z, vMatrix);
  FMarkers[rmML].ApplyPos(Result.Left, vMidPoint.Y, vMidPoint.Z, vMatrix);
  FMarkers[rmMR].ApplyPos(Result.Right, vMidPoint.Y, vMidPoint.Z, vMatrix);
  FMarkers[rmMM].ApplyPos(vMidPoint.X, vMidPoint.Y, vMidPoint.Z, vMatrix);
end;

procedure TsgRectangle.PerformCapture(const ACanvas: TCanvas);
var
  vMarkerPoint: TPoint;
  vDownPointNoResize: TPoint;
  vIsPointOnRect: Boolean;
  vIsPointOnMarker: Boolean;
  vMarker: TsgMarker;
  vCursorPos: TPoint;
begin
  GetCursorPos(vCursorPos);
  vCursorPos := FControl.ScreenToClient(vCursorPos);
  // reset flag
  FCaptureStarted := False;
  // point on marker
  vMarkerPoint := GetPoint(FDownPosition);
  // point on not marker
  vDownPointNoResize := GetPoint(FDownPositionNoResize);
  // if down on rectangle
  vIsPointOnRect := PtInRect(GetRect, vDownPointNoResize);
  // if point on rectangle
  if Assigned(FMarker) then
    vIsPointOnMarker := PtInRect(FMarker.BoundsRect, vMarkerPoint)
  else
    vIsPointOnMarker := False;
  // marker under down point
  vMarker := GetMarkerAtPos(vDownPointNoResize);
  if FNoResize then
  begin
    if vIsPointOnRect and (not Assigned(vMarker) or vMarker.Visibility) then
    begin
      DoMarkerCapture(True);
      vMarkerPoint := vDownPointNoResize;
    end else
      if not vIsPointOnRect and not vIsPointOnMarker
         and not FMarkers[rmTL].Visibility then
      begin
        LoadMarkersPos(FDownPosition, FDownPosition);
        FMarker := FMarkers[rmBR];
        DoMarkerCapture(False);
      end else
        if vIsPointOnMarker or vIsPointOnRect then
        begin
          DoMarkerCapture(True);
          if vIsPointOnMarker then
          begin
            FDownPositionNoResize := FMarker.Position;
            vMarkerPoint := FMarker.Point
          end;
        end else
          if Assigned(FMarker)
             and PtInRect(FMarker.GetBoundsRectByRect, vCursorPos) then
            DoMarkerCapture(True);
  end else
    if not vIsPointOnMarker then
    begin
      if Assigned(FMarker)
         and PtInRect(FMarker.GetBoundsRectByRect, vCursorPos) then
        DoMarkerCapture(True)
      else
      begin
        LoadMarkersPos(FDownPosition, FDownPosition);
        FMarker := FMarkers[rmBR];
        DoMarkerCapture(False);
      end;
    end else
      DoMarkerCapture(True);
  if FCapture then
  begin
    DoRectChanging(FMarker, vMarkerPoint);
    Draw(GetRect, Canvas);
  end;
end;

procedure TsgRectangle.PerformCaptureStop(const ACanvas: TCanvas);
begin
  FCaptureStop := False;
  FCapture := False;
  FMarkerCaptured := False;
  if Assigned(FMarker) then
    DoRectChanging(FMarker, FMarker.Point);
  Draw(GetRect, Canvas);
  DrawMarkers(Canvas);
end;

procedure TsgRectangle.SetActive(const AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    if FActive then
    begin
      FCapture := False;
      InvalidateClipRect;
    end;
    FMatrix := TsgDNAccess(FControl).ControlTool.GetRealImageMatrix;
    FDownPosition := cnstBadRect.BottomRight;
    FDownPositionNoResize := FDownPosition;
    FRect := Classes.Rect(MaxInt, MaxInt, MaxInt, MaxInt);
    LoadMarkersPos(FDownPosition, FDownPosition);
    FActive := AValue;
    if not FActive then
    begin
      Disconnect;
      HideHint;
      FHint.Free;
      FHint := THintWindowClass.Create(nil);
      FHint.Color := clInfoBk;
    end
    else
    begin
      ConnectTo(FControl);
      FControl.Invalidate;
    end;
  end;
end;

procedure TsgRectangle.SetBox(const AValue: TFRect);
begin
  LoadMarkersPos(AValue.TopLeft, AValue.BottomRight);
  InvalidateClipRect;
end;

procedure TsgRectangle.SetControl(const AValue: TControl);
begin
  if FControl <> AValue then
  begin
    Disconnect;
    FControl := AValue;
    if Active then
      ConnectTo(FControl);
  end;
end;

procedure TsgRectangle.SetCursor(const AValue: TCursor);
begin
  if Assigned(FControl) then
    FControl.Cursor := AValue;
end;

procedure TsgRectangle.SetMarker(const AValue: TsgMarker);
begin
  if FMarker <> AValue then
  begin
    FMarker := AValue;
    if Assigned(FMarker) then
      FMarker.Pen.Mode := pmNotCopy;
  end;
  if Assigned(FMarker) then
    Cursor := FMarker.Cursor
  else
    Cursor := crDefault;
end;

procedure TsgRectangle.SetMarkerSize(const Value: Integer);
begin
  FMarkerSize := Value;
  InvalidateClipRect;
end;

procedure TsgRectangle.SetMarkersVisible(const AValue: Boolean);
var
  I: TsgRectangleMarker;
begin
  if FMarkersVisible <> AValue then
  begin
    FMarkersVisible := AValue;
    for I := Low(FMarkers) to High(FMarkers) do
      FMarkers[I].Visible := FMarkersVisible;
    InvalidateClipRect;
  end;
end;

procedure TsgRectangle.SetNoResize(const AValue: Boolean);
begin
  FNoResize := AValue;
end;

procedure TsgRectangle.SetRect(const AValue: TRect);
var
  vRect: TFRect;
begin
  vRect.TopLeft := GetCoord(AValue.TopLeft);
  vRect.BottomRight := GetCoord(AValue.BottomRight);
  Box := vRect;
end;

procedure TsgRectangle.SetShowHint(const AValue: Boolean);
begin
  FShowHint := AValue;
  if not FShowHint then
    HideHint
  else ActivateHint;
end;

procedure TsgRectangle.ActivateHint;
var
  vPoint: TPoint;
  vRect: TRect;
  S: string;
begin
  if FActive then
  begin
    if Assigned(FMarker) then
    begin
      vPoint := FMarker.BoundsRect.BottomRight;
      S := Format('X: %.3f'#13#10'Y: %.3f'#13#10'Z: %.3f',
        [FMarker.Position.X, FMarker.Position.Y, FMarker.Position.Z]);
      vRect := FHint.CalcHintRect(200, S, nil);
      vPoint := FControl.ClientToScreen(vPoint);
      OffsetRect(vRect, vPoint.X + cnstMarkerSize.X div 2,
        vPoint.Y + cnstMarkerSize.Y div 2);
      FHint.ActivateHint(vRect, S);
    end;
  end;
end;

procedure TsgRectangle.ConnectTo(const AControl: TControl);
begin
  if AControl is TsgDrawingNavigator then
  begin
    TsgDNAccess(AControl).AddClient(Self);
    TsgDNAccess(AControl).AddClient(FMarkers[rmBR]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmBM]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmMR]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmBL]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmTR]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmML]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmTM]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmTL]);
    TsgDNAccess(AControl).AddClient(FMarkers[rmMM]);
  end;
  TControlCanvas(FCanvas).Control := AControl;
end;

procedure TsgRectangle.Disconnect;
var
  I: TsgRectangleMarker;
begin
  TControlCanvas(FCanvas).Control := nil;
  if FControl is TsgDrawingNavigator then
  begin
    for I := Low(FMarkers) to High(FMarkers) do
      TsgDNAccess(FControl).RemoveClient(FMarkers[I]);
    TsgDNAccess(FControl).RemoveClient(Self);
  end;
end;

function TsgRectangle.DoMouseDown(Sender: TObject;
  const APoint: TPoint): Integer;
var
  vCanInvalidate: Boolean;
  vMarker: TsgMarker;
  vEnterRectCall: Boolean;
begin
  vEnterRectCall := False;
  Result := 0;
  vCanInvalidate := True;
  if FNoResize and FMarkers[rmTL].Visibility then
  begin
    vMarker := GetMarkerAtPos(APoint);
    if not (PtInRect(GetRect, APoint) or Assigned(vMarker)) then
      vCanInvalidate := not FMarkers[rmTL].Visibility;
  end;
  if not FCapture then
  begin
    FDownPosition := GetSnapedCoord(APoint);
    FDownPositionNoResize := GetCoord(APoint);
    FCaptureStarted := True;
  end
  else
  begin
    FCaptureStop := True;
    FCaptureStarted := False;
    vEnterRectCall := True;
  end;
  if vCanInvalidate then
    InvalidateClipRect;
  if vEnterRectCall and Assigned(FEnterRectEvent) then
    FEnterRectEvent(Self);
end;

function TsgRectangle.DoMouseMove(Sender: TObject;
  const APoint: TPoint): Integer;
begin
  Result := 0;
  if FCapture then
  begin
    Draw(GetRect, Canvas);
    Result := DoRectChanging(Sender, APoint);
    Draw(GetRect, Canvas);
  end
  else
    if FCaptureStarted then
      Paint;
end;

function TsgRectangle.DoMouseUp(Sender: TObject; const APoint: TPoint): Integer;
begin
  Result := 0;
  if FMarkerCaptured then
  begin
    FCaptureStop := True;
    //InvalidateClipRect;
    Draw(GetRect, Canvas);
    Paint;
  end;
end;

procedure TsgRectangle.DrawMarkers(const ACanvas: TCanvas);
var
  I: TsgRectangleMarker;
  vDC: HDC;
begin
  vDC := ACanvas.Handle;
  try
    for I := Low(FMarkers) to High(FMarkers) do
      FMarkers[I].PaintWindow(vDC);
  finally
    ACanvas.Handle := vDC;
  end;
end;

procedure TsgRectangle.HideHint;
begin
  FHint.ActivateHint(Classes.Rect(-MaxInt, -MaxInt, -MaxInt, -MaxInt), '');
  if FHintHiden then
  begin
    FHint.Free;
    FHint := THintWindowClass.Create(nil);
    FHint.Color := clInfoBk;
    FHintHiden := False;
  end;
end;

procedure TsgRectangle.MouseHandler(var Message: TWMMouse);
var
  vPoint: TPoint;
  I: TsgRectangleMarker;
  vIsMarker, vCrossMarkers: Boolean;
  vShift: TShiftState;
begin
  vPoint := SmallPointToPoint(Message.Pos);
  case Message.Msg of
    WM_LBUTTONDOWN:
      Message.Result := DoMouseDown(FMarker, vPoint);
    WM_LBUTTONUP:
      Message.Result := DoMouseUp(FMarker, vPoint);
    WM_MOUSEMOVE:
      begin
        vIsMarker := False;
        vCrossMarkers := False;
        for I := Low(FMarkers) to High(FMarkers) do
          if not PtInRect(FMarkers[I].BoundsRect, vPoint) then
            FMarkers[I].Pen.Mode := pmCopy
          else
          begin
            FMarkers[I].Pen.Mode := pmNotCopy;
            vCrossMarkers := vIsMarker;
            vIsMarker := True;
          end;
          vShift := KeysToShiftState(Message.Keys);
          if FCapture or vIsMarker then
          begin
            if FShowHint then
              ActivateHint;
            if not ((ssMiddle in vShift) or (ssRight in vShift)) then
              if Assigned(FMarker) then
                Cursor := FMarker.Cursor;
          end
          else
            if not vCrossMarkers and FShowHint then
              HideHint;
          if not FCapture then
            if (ssMiddle in vShift) or (ssRight in vShift) then
              Cursor := TCursor(5)
            else
              if not vIsMarker then
                Cursor := TCursor(6);
        Message.Result := DoMouseMove(FMarker, vPoint);
      end;
  end;
end;

procedure TsgRectangle.Paint;
begin
  if not (FCaptureStarted or FCaptureStop) then
    Draw(GetRect, Canvas);
  if FCaptureStarted then
    PerformCapture(Canvas);
  if FCaptureStop then
    PerformCaptureStop(Canvas);
end;

procedure TsgRectangle.PaintHandler(var Message: TWMPaint);
begin
  PaintWindow(Message.DC);
end;

procedure TsgRectangle.PictureMove;
begin
end;

procedure TsgRectangle.ScaleChanging;
begin
end;

constructor TsgRectangle.Create(const AControl: TWinControl);
var
  I: TsgRectangleMarker;
begin
  inherited Create;
  FCanvas := TControlCanvas.Create;
  for I := Low(FMarkers) to High(FMarkers) do
  begin
    FMarkers[I] := TsgMarker.Create(Self);
    FMarkers[I].ID := I;
  end;
  FMatrix := cnstIdentityMat;
  FMarker := FMarkers[rmBR];
  FMarkerSize := cnstMarkerSize.X;
  SetControl(AControl);
  FHint := THintWindowClass.Create(nil);
  FHint.Color := clInfoBk;
  FHintHiden := True;
  FShowHint := False;
  FMarkersVisible := True;
end;

destructor TsgRectangle.Destroy;
var
  I: TsgRectangleMarker;
begin
  SetControl(nil);
  for I := Low(FMarkers) to High(FMarkers) do
    FMarkers[I].Free;
  FHint.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TsgRectangle.Dispatch(var Message);
var
  vMatrix: TFMatrix;
begin
  if FActive then
    case TMessage(Message).Msg of
      WM_MOUSEFIRST .. WM_MOUSELAST:
        MouseHandler(TWMMouse(Message));
      WM_PAINT:
        // WM_PAINT notify after TsgDrawingNavigator custom paint
        // before .OnPaint
        PaintHandler(TWMPaint(Message));
      WM_SCALECHANGING:
        ScaleChanging;
      WM_PICTUREMOVE:
        PictureMove;
      WM_PICTURECHANGE:
        begin
          if Active then
          begin
            vMatrix := TsgDNAccess(FControl).ControlTool.GetRealImageMatrix;
            if not IsEqualFMatrix(vMatrix, FMatrix, False) then
            begin
              FMatrix := vMatrix;
              FRotated := True;
            end;
          end;
        end;
    end;
  inherited Dispatch(Message);
end;

function TsgRectangle.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TsgRectangle.GetCoord(const APoint: TPoint): TFPoint;
begin
  if Control is TsgDrawingNavigator then
  begin
    Result := TsgDNAccess(Control).GetDrawingInternalCoordsWithoutSnap(APoint.X,
      APoint.Y)
  end
  else
    Result := cnstBadRect.BottomRight;
end;

function TsgRectangle.GetPoint(const ACoord: TFPoint): TPoint;
begin
  if Control is TsgDrawingNavigator then
    Result := TsgDNAccess(Control).GetPoint(ACoord)
  else
    Result := Point(MaxInt, MaxInt);
end;

function TsgRectangle.GetSnapedCoord(const APoint: TPoint): TFPoint;
begin
  if Control is TsgDrawingNavigator then
    Result := TsgDNAccess(FControl).GetDrawingInternalCoords(APoint.X,
      APoint.Y)
  else
    Result := cnstBadRect.BottomRight;
end;

procedure TsgRectangle.Initialize;
begin
  Brush.Style := bsClear;
  Brush.Color := clBlack;
  Pen.Style := psDot;
  Pen.Color := clWhite;
  Pen.Mode := pmXor;
  Pen.Width := 1;
end;

procedure TsgRectangle.InvalidateClipRect;
var
  vHandle: HWND;
  vRect: TRect;
begin
  vHandle := GetWindowHandle;
  if vHandle <> 0 then
  begin
    vRect := Rect;
    InflateRect(vRect, cnstMarkerSize.X div 2 + 1,
      cnstMarkerSize.Y div 2 + 1);
    InvalidateRect(vHandle, @vRect, True);
  end
  else
    FControl.Invalidate;
end;

function TsgRectangle.IsPointInMarker(const APoint: TPoint): Boolean;
var
  i: TsgRectangleMarker;
begin
  Result := False;
  for i := Low(TsgRectangleMarker) to High(TsgRectangleMarker) do
    if PtInRect(Markers[i].BoundsRect, APoint) then
      Result := True;
end;

{ TsgMarker }

function TsgMarker.GetBoundsRect: TRect;
begin
  if not FRectangle.Rotated and not IsBadFPoint(FPosition) then
  begin
    Result.TopLeft := FRectangle.GetPoint(FPosition);
    Result.BottomRight := Result.TopLeft;
    InflateRect(Result, Rectangle.MarkerSize div 2, Rectangle.MarkerSize div 2);
  end
  else
    Result := GetBoundsRectByRect;
end;

function TsgMarker.GetCanvas: TCanvas;
begin
  Result := FRectangle.Canvas;
end;

function TsgMarker.GetCursor: TCursor;
begin
  if FRectangle.NoResize and FRectangle.Capture then
    Result := crDrag
  else
    if FVisible then
      Result := GetMarkerCursor(FID)
    else
      Result := FRectangle.Cursor;
end;

function TsgMarker.GetPoint: TPoint;
begin
  Result := FRectangle.GetPoint(FPosition);
end;

function TsgMarker.GetPoint3D: TFPoint;
begin
  Result := FPointXMat(FPosition, Rectangle.GetDrawMatrix);
end;

function TsgMarker.GetVisibility: Boolean;
begin
  Result := FVisibility and ((FPosition.X <> cnstBadRect.Right)
    and (FPosition.Y <> cnstBadRect.Bottom));
end;

procedure TsgMarker.StyleChange(Sender: TObject);
begin
  if FVisibility then
    Paint; //Invalidate
end;

procedure TsgMarker.SetID(const AValue: TsgRectangleMarker);
begin
  FID := AValue;
end;

procedure TsgMarker.SetPoint(const AValue: TPoint);
begin
  FRectangle.DoRectChanging(Self, AValue);
end;

procedure TsgMarker.SetPoint3D(const AValue: TFPoint);
begin
  SetPosition(FPointXMat(AValue, Rectangle.GetInvertDrawMatrix));
end;

procedure TsgMarker.SetPosition(const AValue: TFPoint);
begin
  FPosition := AValue;
end;

procedure TsgMarker.ApplyPos(const AX, AY, AZ: Double; const AMatrix: TFMatrix);
begin
  ApplyPosition(MakeFPoint(AX, AY, AZ), AMatrix);
end;

procedure TsgMarker.Draw(const ARect: TRect; const ACanvas: TCanvas);
begin
  if not IsBadRectI(ARect) and FVisible then
    inherited Draw(ARect, ACanvas);
  FVisibility := (FPosition.X <> cnstBadRect.Right) and
    (FPosition.Y <> cnstBadRect.Bottom);
end;

function TsgMarker.GetBoundsRectByRect: TRect;
var
  vRect: TRect;
  vMiddlePoint: TPoint;
begin
  vRect := Rectangle.Rect;
  vMiddlePoint := MiddlePoint(vRect.TopLeft, vRect.BottomRight);
  case FID of
    rmTL: vRect.BottomRight := vRect.TopLeft;
    rmTM:
      begin
        vRect.Left := vMiddlePoint.X;
        vRect.Right := vMiddlePoint.X;
      end;
    rmTR:
      begin
        vRect.Left := vRect.Right;
        vRect.Bottom := vRect.Top;
      end;
    rmMR:
      begin
        vRect.Left := vRect.Right;
        vRect.Top := vMiddlePoint.Y;
        vRect.BottomRight := vRect.TopLeft;
      end;
    rmBR: vRect.TopLeft := vRect.BottomRight;
    rmBM:
      begin
        vRect.Top := vRect.Bottom;
        vRect.Left := vMiddlePoint.X;
        vRect.Right := vMiddlePoint.X;
      end;
    rmBL:
      begin
        vRect.Top := vRect.Bottom;
        vRect.Right := vRect.Left;
      end;
    rmML:
      begin
        vRect.Top := vMiddlePoint.Y;
        vRect.Bottom := vMiddlePoint.Y;
        vRect.Right := vRect.Left;
      end;
    rmMM: vRect := Classes.Rect(vMiddlePoint.X, vMiddlePoint.X, vMiddlePoint.Y, vMiddlePoint.Y);
  end;
  InflateRect(vRect, cnstMarkerSize.X div 2, cnstMarkerSize.Y div 2);
  Result := vRect;
end;

procedure TsgMarker.Initialize;
begin
  Brush.Style := bsSolid;
  Brush.Color := clBlue;
  Pen.Color := clYellow;
  Pen.Width := 1;
end;

procedure TsgMarker.Invalidate;
var
  vHandle: HWND;
  vRect: TRect;
begin
  vHandle := FRectangle.GetWindowHandle;
  if vHandle <> 0 then
  begin
    vRect := BoundsRect;
    InvalidateRect(vHandle, @vRect, False);
  end;
end;

procedure TsgMarker.MouseHandler(var Message: TWMMouse);
begin
  if PtInRect(BoundsRect, SmallPointToPoint(Message.Pos))
     and not Rectangle.Capture then
    case Message.Msg of
      WM_LBUTTONDOWN:
        begin
          Rectangle.Marker := Self;
          Rectangle.DoMouseDown(Self, SmallPointToPoint(Message.Pos));
        end;
      WM_LBUTTONUP: Rectangle.Marker := nil;
      WM_MOUSEMOVE:
        begin
          Rectangle.Marker := Self;
          Rectangle.Cursor := Cursor;
          Rectangle.DoMouseMove(Self, SmallPointToPoint(Message.Pos));
        end;
    end; { case Message.Msg }
end;

procedure TsgMarker.Paint;
begin
  if not FRectangle.Capture then
    Draw(BoundsRect, Canvas);
end;

procedure TsgMarker.PaintHandler(var Message: TWMPaint);
begin
  FVisibility := False;
  PaintWindow(Message.DC);
  if FRectangle.Rotated then
  begin
    FRectangle.SetRect(FRectangle.Rect);
    FRectangle.Rotated := False;
  end;
end;

procedure TsgMarker.ApplyStyle(const ACanvas: TCanvas);
begin
  inherited ApplyStyle(ACanvas);
  FVisibility := True;
end;

procedure TsgMarker.SetVisibility(const AValue: Boolean);
begin
  FVisibility := AValue;
end;

procedure TsgMarker.SetVisible(const AValue: Boolean);
begin
  FVisible := AValue;
end;

procedure TsgMarker.ApplyPosition(const APoint: TFPoint;
  const AMatrix: TFMatrix);
begin
  SetPosition(FPointXMat(APoint, AMatrix));
end;

constructor TsgMarker.Create(const AOwner: TsgCustomPaintItem);
begin
  inherited Create;
  FRectangle := TsgRectangle(AOwner);
  FPosition := cnstBadRect.BottomRight;
  FVisible := True;
end;

procedure TsgMarker.Dispatch(var Message);
begin
  if FRectangle.Active then
    case TMessage(Message).Msg of
      WM_MOUSEFIRST .. WM_MOUSELAST:
        MouseHandler(TWMMouse(Message));
      WM_PAINT:
        // WM_PAINT notify after TsgDrawingNavigator custom paint
        // before .OnPaint
        PaintHandler(TWMPaint(Message));
    end;
  inherited Dispatch(Message);
end;

end.
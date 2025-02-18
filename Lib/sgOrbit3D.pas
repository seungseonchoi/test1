{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{       Custom control implements 3D orbit drawing mode      }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgOrbit3D;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, Types, LMessages,
{$ENDIF}
  SysUtils, Classes, sgConsts,
{$IFDEF SG_FIREMONKEY}
  System.Messaging, FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, Messages,
{$ENDIF}
{$IFDEF SGDEL_XE2}
  System.Types, System.UITypes
{$ELSE}
  Controls
{$ENDIF}
  ;

type
  Tsg3DStates = (stateX, stateY, stateZ, stateXY);

{$IFDEF SG_FIREMONKEY}
  TOrbitCoordType = Single;
  TOrbitPoint = TPointF;
  TOrbitRect = TRectF;
{$ELSE}
  TOrbitCoordType = Integer;
  TOrbitPoint = TPoint;
  TOrbitRect = TRect;
{$ENDIF}

{ TsgOrbit3D

  Control is designed to implement the rotation of the drawing. }

  TsgOrbit3D = class(TPersistent)
  private
    FAlignToClientRect: Boolean;
    FAutoTarget: Boolean;
    FBigRadius: TOrbitCoordType;
    FBoundsRect: TOrbitRect;
    FCenter: TOrbitPoint;
    FColor: TColor;
    FCurrentState: Tsg3DStates;
    FDegreesPerPixel: Double;
    FMouseCapture: Boolean;
    FMouseCoord: TOrbitPoint;
    FOnChange: TNotifyEvent;
    FOnMouseCaptureChanged: TNotifyEvent;
    FOnRotate: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FOnVisibleChanging: TNotifyEvent;
    FOwner: TPersistent;
    FPitch: Single;
    FRoll: Single;
    FSmallRadius: Integer;
    FTurn: Single;
    FVisible: Boolean;
{$IFDEF SG_FIREMONKEY}
    FButtonDownID: Integer;
    FButtonUpID: Integer;
    FMouseMoveID: Integer;
    FPaintID: Integer;
{$ENDIF}
    FControlShift: TShiftState;
    FAlternateControlShift: TShiftState;
    FVisualMode: Boolean;
    FOrbitColor: {$IFDEF SG_FIREMONKEY}TAlphaColor{$ELSE}TColor{$ENDIF};
    function GetSmallCenters(Index: Integer): TOrbitPoint;
    procedure SetAlignToClientRect(const AValue: Boolean);
    procedure SetAutoTarget(const AValue: Boolean);
    procedure SetBigRadius(const AValue: TOrbitCoordType);
    procedure SetColor(const AValue: TColor);
    procedure SetCurrentState(const AValue: Tsg3DStates);
    procedure SetCursorState(const AX, AY: TOrbitCoordType);
    procedure SetMouseCapture(const Value: Boolean);
    procedure SetSmallRadius(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    function UpdateAutoParams: TOrbitCoordType;
    procedure SetVisualMode(const Value: Boolean);
    procedure SetOrbitColorByState(const AValue: Tsg3DStates);
  protected
    procedure Change; dynamic;
    procedure DoMouseCaptureChanged; dynamic;
    procedure DoRotate; virtual;
    procedure DoStateChanged; dynamic;
    function GetOwner: TPersistent; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: TOrbitCoordType); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType); virtual;
    procedure PaintWindow(AContextInfo: sgConsts.THandle); virtual;
    procedure RotateDirectly(const ADeltaX, ADeltaY: TOrbitCoordType); virtual;
    procedure SetBoundsRect(const AValue: TOrbitRect); virtual;
    procedure SetCenter(const AValue: TOrbitPoint);
    procedure VisibleChanging; virtual;
    function DoButtonDown(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType): Boolean; virtual;
    function DoButtonUp(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType): Boolean; virtual;
    function DoMouseMove(Shift: TShiftState; X, Y: TOrbitCoordType): Boolean; virtual;
    function DoMouseMouseCapture(Shift: TShiftState; X, Y: TOrbitCoordType): Boolean;
{$IFNDEF SG_FIREMONKEY}
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Message: TWMMouse); message {$IFDEF FPC}LM_RBUTTONDOWN{$ELSE}WM_RBUTTONDOWN{$ENDIF};
    procedure WMRButtonUp(var Message: TWMMouse); message {$IFDEF FPC}LM_RBUTTONUP{$ELSE}WM_RBUTTONUP{$ENDIF};
    procedure WMMButtonDown(var Message: TWMMouse); message {$IFDEF FPC}LM_MBUTTONDOWN{$ELSE}WM_MBUTTONDOWN{$ENDIF};
    procedure WMMButtonUp(var Message: TWMMouse); message {$IFDEF FPC}LM_MBUTTONUP{$ELSE}WM_MBUTTONUP{$ENDIF};
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{$ENDIF}
    property SmallCenters[Index: Integer]: TOrbitPoint read GetSmallCenters;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AlignToClientRect: Boolean read FAlignToClientRect write SetAlignToClientRect default True;
    property AutoTarget: Boolean read FAutoTarget write SetAutoTarget;
    property BigRadius: TOrbitCoordType read FBigRadius write SetBigRadius;
    property BoundsRect: TOrbitRect read FBoundsRect write SetBoundsRect;
    property Center: TOrbitPoint read FCenter write SetCenter;
    property Color: TColor read FColor write SetColor;
    property CurrentState: Tsg3DStates read FCurrentState write SetCurrentState;
    property DegreesPerPixel: Double read FDegreesPerPixel write FDegreesPerPixel;
    property MouseCapture: Boolean read FMouseCapture write SetMouseCapture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseCaptureChanged: TNotifyEvent read FOnMouseCaptureChanged write FOnMouseCaptureChanged;
    property OnRotate: TNotifyEvent read FOnRotate write FOnRotate;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnVisibleChanging: TNotifyEvent read FOnVisibleChanging write FOnVisibleChanging;
    property Owner: TPersistent read FOwner;
    property Pitch: Single read FPitch;
    property Roll: Single read FRoll;
    property SmallRadius: Integer read FSmallRadius write SetSmallRadius;
    property Turn: Single read FTurn;
    property Visible: Boolean read FVisible write SetVisible;
    property VisualMode: Boolean read FVisualMode write SetVisualMode;
    property ControlShift: TShiftState read FControlShift write FControlShift;
    property AlternateControlShift: TShiftState read FAlternateControlShift write FAlternateControlShift;
  end;

  TsgOrbit3DClass = class of TsgOrbit3D;

implementation

// optimized for integer values
function IsPointInCircle(const ACenter: TOrbitPoint;
  const ARadius: TOrbitCoordType; AX, AY: TOrbitCoordType): Boolean;
begin
  AX := AX - ACenter.X;
  AY := AY - ACenter.Y;
  Result := AX * AX + (AY - ARadius) * (AY + ARadius) < 0;
end;

function RectCenter(const R: TOrbitRect): TOrbitPoint;
begin
{$IFDEF SG_FIREMONKEY}
  Result := R.CenterPoint;
{$ELSE}
  Result.X := Round((R.Right + R.Left)/2);
  Result.Y := Round((R.Bottom + R.Top)/2);
{$ENDIF}
end;

{$IFNDEF SG_FIREMONKEY}
function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;
{$ENDIF}

{ TsgOrbit3D }

{ function TsgOrbit3D.GetSmallCenters

  The method calculates the center of the small circles:
    Index = 0 - left
    Index = 1 - top
    Index = 2 - right
    Index = 3 - bottom. }
function TsgOrbit3D.GetSmallCenters(Index: Integer): TOrbitPoint;

  function GetOffset(const AIndex: Integer; const AOffs: TOrbitCoordType): TOrbitCoordType;
  begin
    Result := (AIndex and 1) * (((AIndex and 2) shr 1) shl 1 - 1) * AOffs;
  end;

begin
  Result := Center;
  Result.X := Result.X + GetOffset(Index - 1, BigRadius);
  Result.Y := Result.Y + GetOffset(Index, BigRadius);
end;

procedure TsgOrbit3D.RotateDirectly(const ADeltaX, ADeltaY: TOrbitCoordType);

  procedure AddRotate(const AAxis: Integer; const AAngle: Double);
  begin
    if AAngle <> 0 then
      case AAxis of
        0: FPitch := FPitch + AAngle * FDegreesPerPixel;
        1: FTurn := FTurn + AAngle * FDegreesPerPixel;
        2: FRoll := FRoll + AAngle * FDegreesPerPixel;
      end;
  end;

begin
  case CurrentState of
    stateX: AddRotate(0, ADeltaY);                                       // axisX
    stateY: AddRotate(1, ADeltaX);                                       // axisY
    stateZ:
      if Abs(ADeltaX) > Abs(ADeltaY) then
        AddRotate(2, (2 * Ord(FMouseCoord.Y >= Center.Y) - 1) * ADeltaX) // axisZ
      else
        AddRotate(2, (2 * Ord(FMouseCoord.X < Center.X) - 1) * ADeltaY); // axisZ
    stateXY:
      begin
        AddRotate(0, ADeltaY);                      // axisX
        AddRotate(1, ADeltaX);                      // axisY
      end;
  end;
  DoRotate;
  FPitch := 0;
  FTurn := 0;
  FRoll := 0;
end;

{ TsgOrbit3D.SetAlignToClientRect

  Chane property AlignToClientRect. If AlignToClientRect = True then
  orbit will recalc size of radiuses on resize and draw to ClienTOrbitRect
  of control, otherwise it draw using settings of Center and BigRadius. }

procedure TsgOrbit3D.SetAlignToClientRect(const AValue: Boolean);
begin
  if FAlignToClientRect <> AValue then
  begin
    FAlignToClientRect := AValue;
    if FAlignToClientRect then
      UpdateAutoParams;
    Change;
  end;
end;

procedure TsgOrbit3D.SetAutoTarget(const AValue: Boolean);
begin
  FAutoTarget := AValue;
end;

procedure TsgOrbit3D.SetBigRadius(const AValue: TOrbitCoordType);
begin
  if FBigRadius <> AValue then
  begin
    FBigRadius := AValue;
    if not FAlignToClientRect then
      Change;
  end;
end;

procedure TsgOrbit3D.SetCurrentState(const AValue: Tsg3DStates);
begin
  SetOrbitColorByState(AValue);
  if AValue <> FCurrentState then
  begin
    FCurrentState := AValue;
    DoStateChanged;
    if FVisualMode {$IFDEF SG_FIREMONKEY} and Assigned(FOnStateChanged){$ENDIF} then
      Change;
  end;
end;

procedure TsgOrbit3D.SetOrbitColorByState(const AValue: Tsg3DStates);
begin
  FOrbitColor := Color{$IFDEF SG_FIREMONKEY}.AsBGRA{$ENDIF};
  if FVisualMode {$IFDEF SG_FIREMONKEY} and Assigned(FOnStateChanged){$ENDIF} then
  begin
    case AValue of
      stateX: FOrbitColor := {$IFDEF SG_FIREMONKEY}TAlphaColorRec.Red{$ELSE}clRed{$ENDIF};
      stateY: FOrbitColor := {$IFDEF SG_FIREMONKEY}TAlphaColorRec.Green{$ELSE}clGreen{$ENDIF};
      stateZ: FOrbitColor := {$IFDEF SG_FIREMONKEY}TAlphaColorRec.Blue{$ELSE}clBlue{$ENDIF};
    end;
  end;
end;

procedure TsgOrbit3D.SetCursorState(const AX, AY: TOrbitCoordType);
var
  I: Integer;
  vCurrentState: Tsg3DStates;
begin
  I := 0;
  while (I <= 3)
     and not IsPointInCircle(SmallCenters[I], SmallRadius, AX, AY) do
    Inc(I);
  if I <= 3 then
    vCurrentState := Tsg3DStates((I and 1) xor 1)
  else
    if IsPointInCircle(Center, BigRadius, AX, AY) then
      vCurrentState := stateXY
    else
      vCurrentState := stateZ;
  SetCurrentState(vCurrentState);
end;

procedure TsgOrbit3D.SetMouseCapture(const Value: Boolean);
begin
  if FMouseCapture <> Value then
  begin
    FMouseCapture := Value;
    DoMouseCaptureChanged;
  end;
end;

procedure TsgOrbit3D.SetSmallRadius(const AValue: Integer);
begin
  if FSmallRadius <> AValue then
  begin
    FSmallRadius := AValue;
    Change;
  end;
end;

procedure TsgOrbit3D.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    VisibleChanging;
    FVisible := AValue;
  end;
end;

procedure TsgOrbit3D.SetVisualMode(const Value: Boolean);
begin
  FVisualMode := Value;
end;

procedure TsgOrbit3D.VisibleChanging;
begin
  if Assigned(FOnVisibleChanging) then
    FOnVisibleChanging(Self);
end;

procedure TsgOrbit3D.DoStateChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TsgOrbit3D.SetBoundsRect(const AValue: TOrbitRect);
begin
  if not CompareMem(@AValue, @FBoundsRect, SizeOf(TOrbitRect)) then
  begin
    FBoundsRect := AValue;
    if FAlignToClientRect then
      UpdateAutoParams;
    Change;
  end;
end;

procedure TsgOrbit3D.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType);
begin
  if (Shift * ControlShift = ControlShift) or
     ((AlternateControlShift <> []) and (Shift * AlternateControlShift = AlternateControlShift)) then
  begin
    DoMouseMouseCapture(Shift, X, Y);
  end;
end;

procedure TsgOrbit3D.MouseMove(Shift: TShiftState; X, Y: TOrbitCoordType);
begin
  if MouseCapture and ((Shift * ControlShift = ControlShift) or
     ((AlternateControlShift <> []) and
      (Shift * AlternateControlShift = AlternateControlShift))) then
  begin
    if (FMouseCoord.X <> X) or (FMouseCoord.Y <> Y) then
    begin
      RotateDirectly(X - FMouseCoord.X, Y - FMouseCoord.Y);
      FMouseCoord.X := X;
      FMouseCoord.Y := Y;
    end;
  end
  else
    SetCursorState(X, Y);
end;

procedure TsgOrbit3D.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: TOrbitCoordType);
begin
  if Shift = [] then
    case Button of
      {$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbLeft: Shift := [ssLeft];
      {$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbRight: Shift := [ssRight];
      {$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbMiddle: Shift := [ssMiddle];
    end;
  if (Shift * ControlShift = ControlShift) or
     ((AlternateControlShift <> []) and
      (Shift * AlternateControlShift = AlternateControlShift)) then
  begin
    MouseCapture := False;
    SetCursorState(X, Y);
  end;
end;

procedure TsgOrbit3D.SetCenter(const AValue: TOrbitPoint);
begin
  if (FCenter.X <> AValue.X) or (FCenter.Y <> AValue.Y) then
  begin
    FCenter := AValue;
    if not FAlignToClientRect then
      Change;
  end;
end;

procedure TsgOrbit3D.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    Change;
  end;
end;

constructor TsgOrbit3D.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create;
  FControlShift := [ssLeft];
  FAlternateControlShift := [];
{$IFDEF SG_FIREMONKEY}
  FButtonDownID := TMessageManager.DefaultManager.SubscribeToMessage(THandledMessage<TMouseDownEventArgs>,
    procedure (const Sender: TObject; const M: TMessage)
    begin
      if Sender = Owner then
        with THandledMessage<TMouseDownEventArgs>(M) do
          Handled := DoButtonDown(Value.Button, Value.Shift, Value.X, Value.Y);
    end
  );

  FButtonUpID := TMessageManager.DefaultManager.SubscribeToMessage(THandledMessage<TMouseUpEventArgs>,
    procedure (const Sender: TObject; const M: TMessage)
    begin
      if Sender = Owner then
        with THandledMessage<TMouseUpEventArgs>(M) do
          Handled := DoButtonUp(Value.Button, Value.Shift, Value.X, Value.Y);
    end
  );

  FMouseMoveID := TMessageManager.DefaultManager.SubscribeToMessage(THandledMessage<TMouseMoveEventArgs>,
    procedure (const Sender: TObject; const M: TMessage)
    begin
      if Sender = Owner then
        with THandledMessage<TMouseMoveEventArgs>(M) do
          Handled := DoMouseMove(Value.Shift, Value.X, Value.Y);
    end
  );

  FPaintID := TMessageManager.DefaultManager.SubscribeToMessage(THandledMessage<TPaintEventArgs>,
    procedure (const Sender: TObject; const M: TMessage)
    begin
      if (Sender = Owner) and Visible then
        PaintWindow(THandle(THandledMessage<TPaintEventArgs>(M).Value.Canvas));
    end
  );
{$ENDIF}
  FAlignToClientRect := True;
  FAutoTarget := True;
  FColor := $008CEB44; //TColor(RGB(68, 235, 140));
  SetOrbitColorByState(stateXY);
  FBoundsRect := Rect(0, 0, 267, 267);
  FDegreesPerPixel := 1.;
  FSmallRadius := 10;
  UpdateAutoParams;
end;

procedure TsgOrbit3D.PaintWindow(AContextInfo: sgConsts.THandle);
var
  I: Integer;
{$IFNDEF SG_FIREMONKEY}
  vOldPen, vOldvBrush: THandle;
{$ELSE}
  [Weak]Canvas: TCanvas;
  Brush: TStrokeBrush;
{$ENDIF}
  P: TOrbitPoint;
begin
{$IFNDEF SG_FIREMONKEY}
  if (AContextInfo <> 0) {$IFNDEF SG_NON_WIN_PLATFORM}and (GetObjectType(AContextInfo) in [OBJ_DC, OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC]){$ENDIF} then
  begin
    vOldPen := SelectObject(AContextInfo, CreatePen(PS_SOLID, 1, ColorToRGB(FOrbitColor)));
    vOldvBrush := SelectObject(AContextInfo, GetStockObject(NULL_BRUSH));
    Ellipse(AContextInfo, Center.X - BigRadius, Center.Y - BigRadius, Center.X + BigRadius, Center.Y + BigRadius);
    for I := 0 to 3 do
    begin
      P := SmallCenters[I];
      Ellipse(AContextInfo, P.X - SmallRadius, P.Y - SmallRadius, P.X + SmallRadius, P.Y + SmallRadius);
    end;
    SelectObject(AContextInfo, vOldvBrush);
    DeleteObject(SelectObject(AContextInfo, vOldPen));
  end;
{$ELSE}
  Canvas := TCanvas(AContextInfo);
  Brush := TStrokeBrush.Create(TBrushKind.Solid, FOrbitColor);
  try
    Brush.Thickness := 1;
    Canvas.DrawEllipse(TRectF.Create(Center.X - BigRadius, Center.Y - BigRadius, Center.X + BigRadius, Center.Y + BigRadius), 1, Brush);
    for I := 0 to 3 do
    begin
      P := SmallCenters[I];
      Canvas.DrawEllipse(TRectF.Create(P.X - SmallRadius, P.Y - SmallRadius, P.X + SmallRadius, P.Y + SmallRadius), 1, Brush);
    end;
  finally
    Brush.Free;
  end;
{$ENDIF}
end;

procedure TsgOrbit3D.Assign(Source: TPersistent);
begin
  if Source is TsgOrbit3D then
  begin
    FAlignToClientRect := TsgOrbit3D(Source).FAlignToClientRect;
    FAutoTarget := TsgOrbit3D(Source).FAutoTarget;
    FBigRadius := TsgOrbit3D(Source).FBigRadius;
    FBoundsRect := TsgOrbit3D(Source).FBoundsRect;
    FCenter := TsgOrbit3D(Source).FCenter;
    FColor := TsgOrbit3D(Source).FColor;
    FCurrentState := TsgOrbit3D(Source).FCurrentState;
    FDegreesPerPixel := TsgOrbit3D(Source).FDegreesPerPixel;
    FMouseCapture := TsgOrbit3D(Source).FMouseCapture;
    FMouseCoord := TsgOrbit3D(Source).FMouseCoord;
    FSmallRadius := TsgOrbit3D(Source).FSmallRadius;
    FVisible := TsgOrbit3D(Source).FVisible;
    FPitch := TsgOrbit3D(Source).FPitch;
    FTurn := TsgOrbit3D(Source).FTurn;
    FRoll := TsgOrbit3D(Source).FRoll;
    FVisualMode := TsgOrbit3D(Source).FVisualMode;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TsgOrbit3D.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TsgOrbit3D.UpdateAutoParams: TOrbitCoordType;
begin
  FCenter := RectCenter(FBoundsRect);
  Result := {$IFNDEF SG_FIREMONKEY}Round(Int({$ENDIF}(FBoundsRect.Right - FBoundsRect.Left) / 2{$IFNDEF SG_FIREMONKEY})){$ENDIF};
  FBigRadius := {$IFNDEF SG_FIREMONKEY}Round(Int({$ENDIF}(FBoundsRect.Bottom - FBoundsRect.Top) / 2{$IFNDEF SG_FIREMONKEY})){$ENDIF};
  if FBigRadius > Result then FBigRadius := Result;
  FBigRadius := {$IFNDEF SG_FIREMONKEY}Round(Int({$ENDIF}FBigRadius - FBigRadius / 4{$IFNDEF SG_FIREMONKEY})){$ENDIF};
end;

procedure TsgOrbit3D.DoRotate;
begin
  if Assigned(FOnRotate) then
    FOnRotate(Self);
end;

destructor TsgOrbit3D.Destroy;
begin
{$IFDEF SG_FIREMONKEY}
  if FButtonDownID <> 0 then
    TMessageManager.DefaultManager.Unsubscribe(TMessage<TMouseEventArgs>, FButtonDownID);
  if FButtonUpID <> 0 then
    TMessageManager.DefaultManager.Unsubscribe(TMessage<TMouseEventArgs>, FButtonUpID);
  if FMouseMoveID <> 0 then
    TMessageManager.DefaultManager.Unsubscribe(TMessage<TMouseEventArgs>, FMouseMoveID);
  if FPaintID <> 0 then
    TMessageManager.DefaultManager.Unsubscribe(TMessage<TPaintEventArgs>, FPaintID);
{$ENDIF}
  inherited Destroy;
end;

function TsgOrbit3D.DoButtonDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: TOrbitCoordType): Boolean;
var
  vMouseCaptured: Boolean;
begin
  Result := False;
  if Visible then
  begin
    vMouseCaptured := MouseCapture;
    MouseDown(Button, Shift, X, Y);
    Result := not vMouseCaptured and MouseCapture;
  end;
end;

function TsgOrbit3D.DoButtonUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: TOrbitCoordType): Boolean;
var
  vMouseCaptured: Boolean;
begin
  Result := False;
  if Visible then
  begin
    vMouseCaptured := MouseCapture;
    MouseUp(Button, Shift, X, Y);
    Result := vMouseCaptured and not MouseCapture;
  end;
end;

procedure TsgOrbit3D.DoMouseCaptureChanged;
begin
  if Assigned(FOnMouseCaptureChanged) then
    FOnMouseCaptureChanged(Self);
end;

function TsgOrbit3D.DoMouseMove(Shift: TShiftState; X,
  Y: TOrbitCoordType): Boolean;
begin
  Result := False;
  if Visible then
  begin
    MouseMove(Shift, X, Y);
    Result := MouseCapture;
  end;
end;

function TsgOrbit3D.DoMouseMouseCapture(Shift: TShiftState; X, Y: TOrbitCoordType): Boolean;
begin
  Result := MouseCapture;
  MouseCapture := True;
  FMouseCoord.X := X;
  FMouseCoord.Y := Y;
  SetCursorState(X, Y);
end;

function TsgOrbit3D.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{$IFNDEF SG_FIREMONKEY}
procedure TsgOrbit3D.WMLButtonDown(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonDown({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbLeft, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMLButtonUp(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonUp({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbLeft, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMMButtonDown(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonDown({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbMiddle, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMMButtonUp(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonUp({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbMiddle, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMMouseMove(var Message: TWMMouse);
begin
  Message.Result := Ord(DoMouseMove(KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMRButtonDown(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonDown({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbRight, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMRButtonUp(var Message: TWMMouse);
begin
  Message.Result := Ord(DoButtonUp({$IFDEF SGDEL_2009}TMouseButton.{$ENDIF}mbRight, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos));
end;

procedure TsgOrbit3D.WMPaint(var Message: TWMPaint);
begin
  if Visible then
    PaintWindow(Message.DC);
end;
{$ENDIF}

end.

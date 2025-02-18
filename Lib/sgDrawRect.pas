{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{             Drawing matrix navigation control              }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDrawRect;
{$INCLUDE SGDXF.inc}
interface

uses
  Classes, sgConsts, Math{$IFDEF SGDEL_6}, Types{$ELSE}, Windows{$ENDIF};

type
  TsgDrawRect = class(TPersistent)
  private
    FBox: TFRect;
    FOnChanged: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FMatrix: PFMatrix;
    function GetRoundRect: TRect;
    function GetRectangleF: TRectF;
    procedure SetBox(const Value: TFRect);
  protected
    FInternalMatrix: TFMatrix;
    function GetFRect: TFRect;
    function GetRotMatrix(const APitch, ATurn, ARoll: Double; AMatrix: PFMatrix): Integer; overload;
    function GetRotMatrix(AAxis: TsgAxes; AAngle: Double; AMatrix: PFMatrix): Integer; overload;
    procedure DoRotate(const AMatrix: TFMatrix; const ACenter: TFPoint); overload;
    procedure DoRotate(const AMatrix: TFMatrix; const ALocalCenter: TPointF); overload;
    function Determinant3D: Extended;
    function RotMatrix: TFMatrix;
    function OpenVPort(const ADir: TFPoint; const AAngle: Double;
      const ATarget, ACenter: TFPoint;
      const AHeight, AAspect: Double; const AClientRect: TRectF): Boolean;
    function GetBoxParams(var ASize: TFPoint): TFPoint;
    procedure DoChanging; dynamic;
    procedure DoChanged; dynamic;
  public
    constructor Create; virtual;
    function AttachMatrix(AMatrix: PFMatrix): PFMatrix;
    function DettachMatrix(AInitMatrix: PFMatrix = nil): PFMatrix;
    function Scale(const AScale: Double; const APosX, APosY: Double): Boolean; overload;
    function Scale(const AScale: Double; const APos: TPointF): Boolean; overload;
    function Scale(const AScale: Double; const APos: TPoint): Boolean; overload;
    function FitTo(const X, Y, Width, Height: Double): Boolean; overload;
    function FitTo(const AClientRect: TRectF): Boolean; overload;
    function FitTo(const AClientRect: TRect): Boolean; overload;
    function Load(const ARot: TFMatrix; const AScale: Double; const ARotOffs, AOffs: TFPoint): Boolean; overload;
    function Load(const ARot: TFMatrix; const AScale, ARotOffs, AOffs: TFPoint): Boolean; overload;
    function Offset(ADX, ADY: Double; ADZ: Double = 0.0): Boolean; overload;
    function Offset(const AOffs: TPointF): Boolean; overload;
    function Zoom(AViewport, ARect: TRectF): Boolean; overload;
    function Zoom(AViewport, ARect: TRect): Boolean; overload;
    function Zoom(AViewport: TRect; ACenter, ASize: TF2DPoint): Boolean; overload;
    function Zoom(AViewport: TRectF; ACenter: TF2DPoint; ASize: TF2DPoint): Boolean; overload;
    function Rotate(AAxis: TsgAxes; AAngle: Double; ACenter: PFPoint = nil): Boolean; overload;
    function Rotate(AAxis: TsgAxes; AAngle: Double; const ALocalCenter: TPointF): Boolean; overload;
    function Rotate(const APitch, ATurn, ARoll: Double; ACenter: PFPoint = nil): Boolean; overload;
    function Rotate(const APitch, ATurn, ARoll: Double; const ALocalCenter: TPointF): Boolean; overload;
    property RoundRect: TRect read GetRoundRect;
    property RectangleF: TRectF read GetRectangleF;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Box: TFRect read FBox write SetBox;
    property Matrix: PFMatrix read FMatrix;
    property FRect: TFRect read GetFRect;
  end;

procedure MakeDrawMatrix(const ARotMatrix: TFMatrix;
  const AScale, ARotOffs, AOffs: TFPoint; var AMatrix: TFMatrix); overload;
procedure MakeDrawMatrix(const ARotMatrix: TFMatrix; const AScale: Double;
  const ARotOffs, AOffs: TFPoint; var AMatrix: TFMatrix); overload;

function SplitRect(const R: TRectF; var AWidth, AHeight: Single): TPointF; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
function SplitRect(const R: TRectF; var ASize: TFPoint): TFPoint; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
function SplitRect(const R: TFRect; var ASize: TFPoint): TFPoint; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}

implementation

uses
  sgFunction;

function SwapFloats(var A, B: Single): Single;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := A;
  A := B;
  B := Result;
end;

procedure NormalRect(var R: TRectF);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  if R.Left > R.Right then SwapFloats(R.Left, R.Right);
  if R.Top > R.Bottom then SwapFloats(R.Top, R.Bottom);
end;

function SplitRect(const R: TRectF; var AWidth, AHeight: Single): TPointF; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  AWidth := R.Right - R.Left;
  AHeight := R.Bottom - R.Top;
  Result.X := 0.5 * (R.Left + R.Right);
  Result.Y := 0.5 * (R.Top + R.Bottom);
end;

function SplitRect(const R: TRectF; var ASize: TFPoint): TFPoint; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  ASize.X := R.Right - R.Left;
  ASize.Y := R.Bottom - R.Top;
  ASize.Z := 0;
  Result.X := 0.5 * (R.Left + R.Right);
  Result.Y := 0.5 * (R.Top + R.Bottom);
  Result.Z := 0;
end;

function SplitRect(const R: TFRect; var ASize: TFPoint): TFPoint; overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  ASize.X := R.Right - R.Left;
  ASize.Y := R.Top - R.Bottom;
  ASize.Z := R.Z2 - R.Z1;
  Result.X := 0.5 * (R.Left + R.Right);
  Result.Y := 0.5 * (R.Top + R.Bottom);
  Result.Z := 0.5 * (R.Z1 + R.Z2);
end;

procedure FMatAffineScale(const A: TFMatrix; const AScale: TFPoint; var AMatrix: TFMatrix); overload;
begin
  AMatrix.M[0,0] := A.M[0,0] * AScale.X;
  AMatrix.M[1,0] := A.M[1,0] * AScale.X;
  AMatrix.M[2,0] := A.M[2,0] * AScale.X;

  AMatrix.M[0,1] := A.M[0,1] * AScale.Y;
  AMatrix.M[1,1] := A.M[1,1] * AScale.Y;
  AMatrix.M[2,1] := A.M[2,1] * AScale.Y;

  AMatrix.M[0,2] := A.M[0,2] * AScale.Z;
  AMatrix.M[1,2] := A.M[1,2] * AScale.Z;
  AMatrix.M[2,2] := A.M[2,2] * AScale.Z;
end;

procedure FMatAffineScale(const A: TFMatrix; const AScale: Double; var AMatrix: TFMatrix); overload;
begin
  FMatAffineScale(A, MakeFPoint(AScale, AScale, AScale), AMatrix);
end;

procedure MakeDrawMatrix(const ARotMatrix: TFMatrix; const AScale, ARotOffs, AOffs: TFPoint; var AMatrix: TFMatrix);
begin
  FMatAffineScale(ARotMatrix, AScale, AMatrix);
  AMatrix.E0 := SubFPoint(ARotOffs, AffineTransformPoint(AOffs, AMatrix));
end;

procedure MakeDrawMatrix(const ARotMatrix: TFMatrix; const AScale: Double; const ARotOffs, AOffs: TFPoint; var AMatrix: TFMatrix);
begin
  MakeDrawMatrix(ARotMatrix, MakeFPoint(AScale, -AScale, AScale), ARotOffs, AOffs, AMatrix);
end;

{$IFDEF SGFPC}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;
{$ENDIF}

{ TsgDrawRect }

function TsgDrawRect.AttachMatrix(AMatrix: PFMatrix): PFMatrix;
begin
  Result := FMatrix;
  FMatrix := AMatrix;
end;

constructor TsgDrawRect.Create;
begin
  inherited Create;
  FInternalMatrix := cnstCrossYMat;
  FMatrix := @FInternalMatrix;
  FBox := MakeFRect(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5);
end;

function TsgDrawRect.Determinant3D: Extended;
begin
  Result := AffineMatrixDeterminant(FMatrix^.Affine);
end;

function TsgDrawRect.DettachMatrix(AInitMatrix: PFMatrix = nil): PFMatrix;
begin
  Result := FMatrix;
  FMatrix := @FInternalMatrix;
  if AInitMatrix <> nil then
    FMatrix^ := AInitMatrix^;
end;

procedure TsgDrawRect.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TsgDrawRect.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TsgDrawRect.DoRotate(const AMatrix: TFMatrix;
  const ALocalCenter: TPointF);
var
  vBoxCenter: TFPoint;
begin
  vBoxCenter := CADUnProject(ALocalCenter.X, ALocalCenter.Y, 0, FMatrix^);
  DoChanging;
  FMatrix^ := FMatXMat(FMatrix^, AMatrix);
  FMatrix^.E0 := SubFPoint(MakeFPointFromPoint(ALocalCenter), AffineTransformPoint(vBoxCenter, FMatrix^));
  DoChanged;
end;

function TsgDrawRect.FitTo(const AClientRect: TRect): Boolean;
begin
  Result := FitTo(RectF(AClientRect.Left, AClientRect.Top,
    AClientRect.Right, AClientRect.Bottom));
end;

function TsgDrawRect.FitTo(const X, Y, Width, Height: Double): Boolean;
var
  vClientSize: TFPoint;
  vRect: TFRect;
  vScale: Extended;

  function DoScale(VX: Integer; const AClientSize: TFPoint; var ARect: TFRect): Extended;
  var
    VY: Integer;
    vNewSize: TFPoint;
    vSize: Extended;
  begin
    VY := VX xor 1;
    vSize := ARect.V[VY].V[VX] - ARect.V[VX].V[VX];
    if IsZero(vSize) then
      Result := -1
    else
    begin
      Result := AClientSize.V[VX]/vSize;
      vNewSize.V[VY] := Round(ARect.V[VX].V[VY] * Result) - Round(ARect.V[VY].V[VY] * Result);// rounding for 2D algorithm
      if vNewSize.V[VY] > AClientSize.V[VY] then
        Result := Result * AClientSize.V[VY]/vNewSize.V[VY];
    end;
  end;

begin
  Result := False;
  // one pixel correct
  vClientSize.X := Width - 1;
  vClientSize.Y := Height - 1;
  vRect := GetRealBox(FBox, FMatrix^);
  case Ord(IsZero(vRect.Right - vRect.Left)) or (Ord(IsZero(vRect.Top - vRect.Bottom)) shl 1) of
    1: vScale := DoScale(1, vClientSize, vRect);
  else
    vScale := DoScale(0, vClientSize, vRect);
  end;
  if vScale > 0 then
  begin
    DoChanging;
    MakeDrawMatrix(FMatrix^, MakeFPoint(vScale, vScale, vScale),
      MakeFPoint(X + 0.5 * vClientSize.X, Y + 0.5 * vClientSize.Y),
      GetCenterOfRect(FBox), FMatrix^);
    DoChanged;
    Result := True;
  end;
end;

procedure TsgDrawRect.DoRotate(const AMatrix: TFMatrix; const ACenter: TFPoint);
var
  vLocalBoxCenter: TFPoint;
begin
  vLocalBoxCenter := FPointXMat(ACenter, FMatrix^);
  DoChanging;
  FMatrix^ := FMatXMat(FMatrix^, AMatrix);
  FMatrix^.E0 := SubFPoint(MakeFPoint(vLocalBoxCenter.X, vLocalBoxCenter.Y), AffineTransformPoint(ACenter, FMatrix^));
  DoChanged;
end;

function TsgDrawRect.FitTo(const AClientRect: TRectF): Boolean;
var
  R: TRectF;
begin
  R := AClientRect;
  NormalRect(R);
  Result := FitTo(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
end;

function TsgDrawRect.GetBoxParams(var ASize: TFPoint): TFPoint;
begin
  Result := SplitRect(FBox, ASize);
end;

function TsgDrawRect.GetFRect: TFRect;
begin
  Result := GetRealBox(FBox, FMatrix^);
end;

function TsgDrawRect.GetRectangleF: TRectF;

  function ToSingle(const AValue: Double): Single;
  const
    cnstMaxSignle = MaxSingle / 2;
    cnstMinSingle = -cnstMaxSignle;
  begin
    if AValue >= cnstMaxSignle then
      Result := cnstMaxSignle
    else
      if AValue <= cnstMinSingle then
        Result := cnstMinSingle
      else
        Result := AValue;
  end;

var
  R: TFRect;
begin
  R := GetFRect;
  if R.Right - R.Left < 1 then
  begin
    R.Left := R.Left - 0.5;
    R.Right := R.Right + 0.5;
  end;
  if R.Top - R.Bottom < 1 then
  begin
    R.Top := R.Top + 0.5;
    R.Bottom := R.Bottom - 0.5;
  end;

  Result.Left := ToSingle(R.Left);
  Result.Top := ToSingle(R.Bottom);
  Result.Right := ToSingle(R.Right);
  Result.Bottom := ToSingle(R.Top);
end;

function TsgDrawRect.GetRotMatrix(AAxis: TsgAxes; AAngle: Double;
  AMatrix: PFMatrix): Integer;
begin
  Result := 0;
  if AAngle <> 0 then
  begin
    Result := 1 shl Ord(AAxis);
    if AMatrix <> nil then
      AMatrix^ := BuildRotMatrix(AAxis, DegToRad(AAngle));
  end;
end;

function TsgDrawRect.GetRotMatrix(const APitch, ATurn, ARoll: Double;
  AMatrix: PFMatrix): Integer;
begin
  Result := Ord(APitch<>0) or (Ord(ATurn<>0) shl 1) or (Ord(ARoll<>0) shl 2);
  if (Result <> 0) and (AMatrix <> nil) then
  begin
    case Result of
      1: AMatrix^ := BuildRotMatrix(axisX, DegToRad(APitch));
      2: AMatrix^ := BuildRotMatrix(axisY, DegToRad(ATurn));
      3: AMatrix^ := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), BuildRotMatrix(axisY, DegToRad(ATurn)));
      4: AMatrix^ := BuildRotMatrix(axisZ, DegToRad(ARoll));
      5: AMatrix^ := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), BuildRotMatrix(axisZ, DegToRad(ARoll)));
      6: AMatrix^ := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), BuildRotMatrix(axisY, DegToRad(ATurn)));
      7: AMatrix^ := FMatXMat(BuildRotMatrix(axisX, DegToRad(APitch)), FMatXMat(BuildRotMatrix(axisY, DegToRad(ATurn)), BuildRotMatrix(axisZ, DegToRad(ARoll))));
    end;
  end;
end;

function TsgDrawRect.GetRoundRect: TRect;

  function ToInteger(const AValue: Single): Integer;
  begin
    if AValue >= cnstMaxInt32 then
      Result := cnstMaxInt32
    else
      if AValue <= cnstMinInt32 then
        Result := cnstMinInt32
      else
        Result := Round(AValue);
  end;

var
  R: TRectF;
begin
  R := GetRectangleF;
  Result.Left := ToInteger(R.Left);
  Result.Top := ToInteger(R.Top);
  Result.Right := ToInteger(R.Right);
  Result.Bottom := ToInteger(R.Bottom);
end;

function TsgDrawRect.Load(const ARot: TFMatrix; const AScale: Double;
  const ARotOffs, AOffs: TFPoint): Boolean;
begin
  Result := Load(ARot, MakeFPoint(AScale, -AScale, AScale), ARotOffs, AOffs);
end;

function TsgDrawRect.Load(const ARot: TFMatrix; const AScale, ARotOffs,
  AOffs: TFPoint): Boolean;
begin
  DoChanging;
  MakeDrawMatrix(ARot, AScale, ARotOffs, AOffs, FMatrix^);
  Result := True;
  DoChanged;
end;

function TsgDrawRect.Offset(const AOffs: TPointF): Boolean;
begin
  Result := Offset(AOffs.X, AOffs.Y);
end;

function TsgDrawRect.OpenVPort(const ADir: TFPoint; const AAngle: Double;
  const ATarget, ACenter: TFPoint; const AHeight, AAspect: Double; const AClientRect: TRectF): Boolean;
var
  vDrawMatrix: TFMatrix;
  vScale, vOffset: TFPoint;
  vClientHeight: Double;
begin
  vDrawMatrix := GetViewTwistMatrix(Ort(ADir), AAngle);
  FMatOffset(vDrawMatrix, Reverse(ACenter));
  vClientHeight := AClientRect.Bottom - AClientRect.Top;
  vScale.X := vClientHeight / AHeight;
  vScale.Y := -vScale.X;
  vScale.Z := vScale.X;
  vDrawMatrix := FMatScale(vDrawMatrix, vScale);
  vOffset := SubFPoint(FPointXMat(ADir, vDrawMatrix),
    FPointXMat(ATarget, vDrawMatrix));
  vDrawMatrix := FMatTranslate(vDrawMatrix,
    MakeFPoint(vOffset.X + 0.5 * AAspect * vClientHeight,
      vOffset.Y + 0.5 * vClientHeight));
  DoChanging;
  FMatrix^ := vDrawMatrix;
  DoChanged;
  Result := True;
end;

function TsgDrawRect.Rotate(const APitch, ATurn, ARoll: Double;
  const ALocalCenter: TPointF): Boolean;
var
  vMatrix: TFMatrix;
begin
  Result := GetRotMatrix(APitch, ATurn, ARoll, @vMatrix) > 0;
  if Result then
    DoRotate(vMatrix, ALocalCenter);
end;

function TsgDrawRect.RotMatrix: TFMatrix;
var
  vDet: Extended;
begin
  vDet := Power(Abs(Determinant3D), - 1.0 / 3.0);
  Result.EX.X := FMatrix^.EX.X * vDet;
  Result.EX.Y := -FMatrix^.EX.Y * vDet;
  Result.EX.Z := FMatrix^.EX.Z * vDet;
  Result.EY.X := FMatrix^.EY.X * vDet;
  Result.EY.Y := -FMatrix^.EY.Y * vDet;
  Result.EY.Z := FMatrix^.EY.Z * vDet;
  Result.EZ.X := FMatrix^.EZ.X * vDet;
  Result.EZ.Y := -FMatrix^.EZ.Y * vDet;
  Result.EZ.Z := FMatrix^.EZ.Z * vDet;
  Result.E0.X := 0;
  Result.E0.Y := 0;
  Result.E0.Z := 0;
end;

function TsgDrawRect.Rotate(AAxis: TsgAxes; AAngle: Double;
  const ALocalCenter: TPointF): Boolean;
var
  vMatrix: TFMatrix;
begin
  Result := GetRotMatrix(AAxis, AAngle, @vMatrix) > 0;
  if Result then
    DoRotate(vMatrix, ALocalCenter);
end;

function TsgDrawRect.Rotate(AAxis: TsgAxes; AAngle: Double;
  ACenter: PFPoint = nil): Boolean;
var
  vMatrix: TFMatrix;
begin
  Result := GetRotMatrix(AAxis, AAngle, @vMatrix) > 0;
  if Result then
    if ACenter <> nil then
      DoRotate(vMatrix, ACenter^)
    else
      DoRotate(vMatrix, GetCenterOfRect(FBox));
end;

function TsgDrawRect.Rotate(const APitch, ATurn, ARoll: Double; ACenter: PFPoint): Boolean;
var
  vMatrix: TFMatrix;
begin
  Result := GetRotMatrix(APitch, ATurn, ARoll, @vMatrix) > 0;
  if Result then
    if ACenter <> nil then
      DoRotate(vMatrix, ACenter^)
    else
      DoRotate(vMatrix, GetCenterOfRect(FBox));
end;

function TsgDrawRect.Offset(ADX, ADY: Double; ADZ: Double = 0.0): Boolean;
begin
  DoChanging;
  FMatrix^.E0 := AddFPoint(FMatrix^.E0, MakeFPoint(ADX, ADY, ADZ));
  DoChanged;
  Result := True;
end;

function TsgDrawRect.Scale(const AScale, APosX, APosY: Double): Boolean;
begin
  DoChanging;
  MakeDrawMatrix(FMatrix^, MakeFPoint(AScale, AScale, AScale),
    MakeFPoint(APosX, APosY), CADUnProject(APosX, APosY, 0, FMatrix^), FMatrix^);
  DoChanged;
  Result := True;
end;

function TsgDrawRect.Scale(const AScale: Double; const APos: TPointF): Boolean;
begin
  Result := Scale(AScale, APos.X, APos.Y);
end;

procedure TsgDrawRect.SetBox(const Value: TFRect);
begin
  FBox := Value;
end;

function TsgDrawRect.Zoom(AViewport: TRect; ACenter, ASize: TF2DPoint): Boolean;
begin
  Result := Zoom(
    RectF(AViewport.Left, AViewport.Top, AViewport.Right, AViewport.Bottom),
      ACenter, ASize);
end;

function TsgDrawRect.Zoom(AViewport: TRectF; ACenter, ASize: TF2DPoint): Boolean;
var
  S, vVC, vVSZ: TFPoint;
begin
  Result := False;
  if (ASize.X > 0) and (ASize.Y > 0) then
  begin
    NormalRect(AViewport);
    vVC := SplitRect(AViewport, vVSZ);
    S.V[0] := vVSZ.V[0] / ASize.V[0];
    S.V[1] := vVSZ.V[1] / ASize.V[1];
    Result := S.V[0] * ASize.V[1] > vVSZ.V[1];
    S.V[Ord(not Result)] := S.V[Ord(Result)];
    S.V[2] := S.V[0];
    DoChanging;
    MakeDrawMatrix(FMatrix^, S, vVC, CADUnProject(ACenter, FMatrix^), FMatrix^);
    DoChanged;
    Result := True;
  end;
end;

function TsgDrawRect.Zoom(AViewport, ARect: TRect): Boolean;
begin
  Result := Zoom(RectF(AViewport.Left, AViewport.Top, AViewport.Right, AViewport.Bottom),
    RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom))
end;

function TsgDrawRect.Zoom(AViewport, ARect: TRectF): Boolean;
var
  vRC, vRSZ: TFPoint;
begin
  NormalRect(ARect);
  vRC := SplitRect(ARect, vRSZ);
  Result := Zoom(AViewport, vRC.Point2D, vRSZ.Point2D);
end;

function TsgDrawRect.Scale(const AScale: Double; const APos: TPoint): Boolean;
begin
  Result := Scale(AScale, APos.X, APos.Y);
end;

end.

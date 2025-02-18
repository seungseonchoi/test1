{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                Functions by compare values                 }
{                                                            }
{       Copyright (c) 2012-2014 SoftGold software company    }
{                                                            }
{************************************************************}
unit sgComparer;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

interface

uses
  {$IFNDEF SG_NON_WIN_PLATFORM}
    Windows,
  {$ENDIF}
  SysUtils,
{$IFDEF SGDEL_7}
    Types, {$IFDEF SGDEL_XE2} System.UITypes, {$ENDIF}
{$ENDIF}
  sgConsts;

const
  cnstAngleConvert:array[Boolean] of Double = (57.295779513082320876798154814105, 1);
  cnstAnglePi:     array[Boolean] of Double = (180, cnstPi);
  cnstAnglePiDiv2: array[Boolean] of Double = (90, cnstPiDiv2);
  cnstAnglePiMul2: array[Boolean] of Double = (360, cnstPiMul2);

type
  TsgPointerConverter = class
    class function GetAsPFPoint(const AItem: Pointer): TFPoint;
    class function GetAsPF2DPoint(const AItem: Pointer): TFPoint;
    class function GetAsPPoint(const AItem: Pointer): TFPoint;
    class procedure SetAsPFPoint(const AItem: Pointer; const AValue: TFPoint);
    class procedure SetAsPF2DPoint(const AItem: Pointer; const AValue: TFPoint);
    class procedure SetAsPPoint(const AItem: Pointer; const AValue: TFPoint);
  end;

  TsgTypeComparer = class
    class function CmpCardinal(const A, B: Cardinal): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpColorCAD(const A, B: TsgColorCAD): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpDouble(const A, B: Double): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpF2DPoint(const A, B: TF2DPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpF2DPointByX(const A, B: TF2DPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpF2DPointByY(const A, B: TF2DPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpF2DPointByYX(const A, B: TF2DPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFloat(const A, B: TsgFloat): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPoint(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPoint2D(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPoint2DXnegY(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPoint2DXYneg(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPoint2DXnegYneg(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPointByX(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPointByY(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpFPointByZ(const A, B: TFPoint): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpHashItem(const A, B: TsgHashItem): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpHashItemObject(const A, B: TsgHashItemObject): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpInt64(const A, B: Int64): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpInteger(const A, B: Integer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpNativInt(const A, B: TsgNativeInt): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpNativUInt(const A, B: TsgNativeUInt): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpPointer(const A, B: Pointer): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpPointF(const A, B: TPointF): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpSingle(const A, B: Single): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpStr(const A, B: string; ACaseSensitivity: Boolean): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpStrAsNumber(const A, B: string): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpUInt64(const A, B: UInt64): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CmpMethod(const A, B: TMethod): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}

    class function NearestDouble(const A, B: Double): Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function NearestFPoint(const A, B: TFPoint): Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  TsgPointerTypeComparer = class
    class function CmpCardinal(const A, B: Pointer): Integer;
    class function CmpColorCAD(const A, B: Pointer): Integer;
    class function CmpDouble(const A, B: Pointer): Integer;
    class function CmpF2DPoint(const A, B: Pointer): Integer;
    class function CmpF2DPointByX(const A, B: Pointer): Integer;
    class function CmpF2DPointByY(const A, B: Pointer): Integer;
    class function CmpF2DPointByYX(const A, B: Pointer): Integer;
    class function CmpFloat(const A, B: Pointer): Integer;
    class function CmpFPoint(const A, B: Pointer): Integer;
    class function CmpFPoint2D(const A, B: Pointer): Integer;
    class function CmpFPoint2DXnegY(const A, B: Pointer): Integer;
    class function CmpFPoint2DXYneg(const A, B: Pointer): Integer;
    class function CmpFPoint2DXnegYneg(const A, B: Pointer): Integer;
    class function CmpFPointByX(const A, B: Pointer): Integer;
    class function CmpFPointByY(const A, B: Pointer): Integer;
    class function CmpHashItem(const A, B: Pointer): Integer;
    class function CmpHashItemObject(const A, B: Pointer): Integer;
    class function CmpInt64(const A, B: Pointer): Integer;
    class function CmpInteger(const A, B: Pointer): Integer;
    class function CmpNativInt(const A, B: Pointer): Integer;
    class function CmpNativUInt(const A, B: Pointer): Integer;
    class function CmpPointer(const A, B: Pointer): Integer;
    class function CmpPointF(const A, B: Pointer): Integer;
    class function CmpSingle(const A, B: Pointer): Integer;
    class function CmpUInt64(const A, B: Pointer): Integer;
    class function CmpMethod(const A, B: Pointer): Integer;

    class function NearestDouble(const A, B: Pointer): Double;
    class function NearestFPoint(const A, B: Pointer): Double;
  end;

function sgAngleByPoints(const ACenter, APoint: TFPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution; const AOrtoX: Integer = 1;
  const AOrtoY :Integer = 1): Double; {$IFDEF USE_INLINE}inline;{$ENDIF}
function sgSignZ(const AVal: Double; const AAccuracy: Double = fDoubleResolution): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
function GetHashCodeStr(const AStr: string): LongInt;
function GetHashCodeInt64(const AValue: Int64): LongInt;
function GetHashCodeUInt64(const AValue: UInt64): LongInt;
function GetHashCodePtr96(const AValue: Pointer): LongInt;
function GetHashCodePtr192(const AValue: Pointer): LongInt;
function GetHashCodeObj(const AValue: TObject): LongInt;

implementation

type
  PMethod = ^TMethod;

function sgAngleByPoints(const ACenter, APoint: TFPoint; const AIsRadian: Boolean;
  const Epsilon: Double = fDoubleResolution; const AOrtoX: Integer = 1;
  const AOrtoY :Integer = 1): Double; {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  vKatetX, vKatetY: Double;
  vCmpX, vCmpY: Integer;
begin
  vKatetX := AOrtoX*(APoint.X - ACenter.X);
  vKatetY := AOrtoY*(APoint.Y - ACenter.Y);
  vCmpX := Integer(vKatetX < 0);
  vCmpY := Integer(vKatetY < 0);
  vKatetX := Abs(vKatetX);
  vKatetY := Abs(vKatetY);
  if vKatetX <= Epsilon  then
    Result := cnstAnglePiDiv2[AIsRadian] + cnstAnglePi[AIsRadian] * vCmpY
  else
  begin
    Result := cnstAngleConvert[AIsRadian] * ArcTan(vKatetY / vKatetX);
    case vCmpX shl 1 + vCmpY of
      1:
        begin
          Result := cnstAnglePiMul2[AIsRadian] - Result;
          if (Result > cnstAnglePiMul2[AIsRadian]) or IsEqual(Result, cnstAnglePiMul2[AIsRadian]) then
            Result := 0;
        end;
      2:  Result := cnstAnglePi[AIsRadian] - Result;
      3:  Result := cnstAnglePi[AIsRadian] + Result;
    end;
  end;
end;

function sgSignZ(const AVal: Double; const AAccuracy: Double = fDoubleResolution): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AVal > AAccuracy then
    Result := 1
  else
  begin
    if AVal < -AAccuracy then
      Result := -1
    else
      Result := 0
  end;
end;

{ BobJenkinsHash }

function Rot(x, k: Cardinal): Cardinal;{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Result := (x shl k) or (x shr (32 - k));
end;

procedure Mix(var a, b, c: Cardinal);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  Dec(a, c); a := a xor Rot(c, 4); Inc(c, b);
  Dec(b, a); b := b xor Rot(a, 6); Inc(a, c);
  Dec(c, b); c := c xor Rot(b, 8); Inc(b, a);
  Dec(a, c); a := a xor Rot(c,16); Inc(c, b);
  Dec(b, a); b := b xor Rot(a,19); Inc(a, c);
  Dec(c, b); c := c xor Rot(b, 4); Inc(b, a);
end;

procedure Final(var a, b, c: Cardinal);{$IFDEF USE_INLINE} inline;{$ENDIF}
begin
  c := c xor b; Dec(c, Rot(b,14));
  a := a xor c; Dec(a, Rot(c,11));
  b := b xor a; Dec(b, Rot(a,25));
  c := c xor b; Dec(c, Rot(b,16));
  a := a xor c; Dec(a, Rot(c, 4));
  b := b xor a; Dec(b, Rot(a,14));
  c := c xor b; Dec(c, Rot(b,24));
end;

{$IFDEF SGDEL_2009}
{$POINTERMATH ON}
{$ELSE}
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..4095] of Byte;
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..4095] of Cardinal;
{$ENDIF}
// http://burtleburtle.net/bob/c/lookup3.c
function HashLittle(const Data; Len, InitVal: Integer): Integer;
type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[Byte] of Cardinal;
var
  pb: PByteArray;
  pd: PCardinalArray absolute pb;
  a, b, c: Cardinal;
label
  case_1, case_2, case_3, case_4, case_5, case_6,
  case_7, case_8, case_9, case_10, case_11, case_12;
begin
  a := Cardinal($DEADBEEF) + Cardinal(Len shl 2) + Cardinal(InitVal);
  b := a;
  c := a;

  pb := @Data;

  // 4-byte aligned data
  if (Cardinal(pb) and 3) = 0 then
  begin
    while Len > 12 do
    begin
      Inc(a, pd^[0]);
      Inc(b, pd^[1]);
      Inc(c, pd^[2]);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(PByte(pd), 3 * SizeOf(Cardinal));
    end;

    case Len of
      0: {$IFDEF SGDEL_2009}Exit(Integer(c)){$ELSE}begin Result := Integer(c); Exit end{$ENDIF};
      1: Inc(a, pd^[0] and $FF);
      2: Inc(a, pd^[0] and $FFFF);
      3: Inc(a, pd^[0] and $FFFFFF);
      4: Inc(a, pd^[0]);
      5:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1] and $FF);
      end;
      6:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1] and $FFFF);
      end;
      7:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1] and $FFFFFF);
      end;
      8:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1]);
      end;
      9:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1]);
        Inc(c, pd^[2] and $FF);
      end;
      10:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1]);
        Inc(c, pd^[2] and $FFFF);
      end;
      11:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1]);
        Inc(c, pd^[2] and $FFFFFF);
      end;
      12:
      begin
        Inc(a, pd^[0]);
        Inc(b, pd^[1]);
        Inc(c, pd^[2]);
      end;
    end;
  end
  else
  begin
    // Ignoring rare case of 2-byte aligned data. This handles all other cases.
    while Len > 12 do
    begin
      Inc(a, pb^[0] + pb^[1] shl 8 + pb^[2] shl 16 + pb^[3] shl 24);
      Inc(b, pb^[4] + pb^[5] shl 8 + pb^[6] shl 16 + pb^[7] shl 24);
      Inc(c, pb^[8] + pb^[9] shl 8 + pb^[10] shl 16 + pb^[11] shl 24);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(PByte(pb), 12);
    end;

    case Len of
      0: {$IFDEF SGDEL_2009}Exit(Integer(c)){$ELSE}begin Result := Integer(c); Exit end{$ENDIF};
      1: goto case_1;
      2: goto case_2;
      3: goto case_3;
      4: goto case_4;
      5: goto case_5;
      6: goto case_6;
      7: goto case_7;
      8: goto case_8;
      9: goto case_9;
      10: goto case_10;
      11: goto case_11;
      12: goto case_12;
    end;

case_12:
    Inc(c, pb^[11] shl 24);
case_11:
    Inc(c, pb^[10] shl 16);
case_10:
    Inc(c, pb^[9] shl 8);
case_9:
    Inc(c, pb^[8]);
case_8:
    Inc(b, pb^[7] shl 24);
case_7:
    Inc(b, pb^[6] shl 16);
case_6:
    Inc(b, pb^[5] shl 8);
case_5:
    Inc(b, pb^[4]);
case_4:
    Inc(a, pb^[3] shl 24);
case_3:
    Inc(a, pb^[2] shl 16);
case_2:
    Inc(a, pb^[1] shl 8);
case_1:
    Inc(a, pb^[0]);
  end;

  Final(a, b, c);
  Result := Integer(c);
end;

function GetHashCodeStr(const AStr: string): LongInt;
begin
  Result := 0;
  if Length(AStr) > 0 then
    Result := HashLittle(PByte(@AStr[1])^, Length(AStr) * SizeOf(Char), 0);
end;

function GetHashCodeInt64(const AValue: Int64): LongInt;
begin
  Result := HashLittle(AValue, SizeOf(AValue), 0);
end;

function GetHashCodeUInt64(const AValue: UInt64): LongInt;
begin
  Result := HashLittle(AValue, SizeOf(AValue), 0);
end;

function GetHashCodePtr96(const AValue: Pointer): LongInt;
begin
  Result := HashLittle(PSingle(AValue)^, 12, 0);
end;

function GetHashCodePtr192(const AValue: Pointer): LongInt;
begin
  Result := HashLittle(PDouble(AValue)^, 24, 0);
end;

function GetHashCodeObj(const AValue: TObject): LongInt;
begin
  Result := HashLittle(AValue, SizeOf(TObject), 0);
end;

{ TsgPointerTypeComparer }

class function TsgPointerTypeComparer.CmpCardinal(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpCardinal(PCardinal(A)^, PCardinal(B)^);
end;

class function TsgPointerTypeComparer.CmpColorCAD(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpColorCAD(PsgColorCAD(A)^, PsgColorCAD(B)^);
end;

class function TsgPointerTypeComparer.CmpDouble(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpDouble(PDouble(A)^, PDouble(B)^);
end;

class function TsgPointerTypeComparer.CmpF2DPoint(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpF2DPoint(PF2DPoint(A)^, PF2DPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpF2DPointByX(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpF2DPointByX(PF2DPoint(A)^, PF2DPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpF2DPointByY(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpF2DPointByY(PF2DPoint(A)^, PF2DPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpF2DPointByYX(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpF2DPointByYX(PF2DPoint(A)^, PF2DPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFloat(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFloat(PsgFloat(A)^, PsgFloat(B)^);
end;

class function TsgPointerTypeComparer.CmpFPoint(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPoint2D(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint2D(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPoint2DXnegY(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint2DXnegY(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPoint2DXYneg(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint2DXYneg(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPoint2DXnegYneg(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPoint2DXnegYneg(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPointByX(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPointByX(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpFPointByY(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpFPointByY(PFPoint(A)^, PFPoint(B)^);
end;

class function TsgPointerTypeComparer.CmpHashItem(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpHashItem(PsgHashItem(A)^, PsgHashItem(B)^);
end;

class function TsgPointerTypeComparer.CmpHashItemObject(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpHashItemObject(PsgHashItemObject(A)^, PsgHashItemObject(B)^);
end;

class function TsgPointerTypeComparer.CmpInt64(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpInt64(PInt64(A)^, PInt64(B)^);
end;

class function TsgPointerTypeComparer.CmpInteger(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpInteger(PInteger(A)^, PInteger(B)^);
end;

class function TsgPointerTypeComparer.CmpMethod(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpMethod(PMethod(A)^, PMethod(B)^);
end;

class function TsgPointerTypeComparer.CmpNativInt(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpNativInt(PsgNativeInt(A)^, PsgNativeInt(B)^);
end;

class function TsgPointerTypeComparer.CmpNativUInt(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpNativUInt(PsgNativeUInt(A)^, PsgNativeUInt(B)^);
end;

class function TsgPointerTypeComparer.CmpPointer(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpPointer(PPointer(A)^, PPointer(B)^);
end;

class function TsgPointerTypeComparer.CmpPointF(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpPointF(PPointF(A)^, PPointF(B)^);
end;

class function TsgPointerTypeComparer.CmpSingle(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpSingle(PSingle(A)^, PSingle(B)^);
end;

class function TsgPointerTypeComparer.CmpUInt64(const A, B: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpUInt64(PUInt64(A)^, PUInt64(B)^);
end;

class function TsgPointerTypeComparer.NearestDouble(const A, B: Pointer): Double;
begin
  Result := TsgTypeComparer.NearestDouble(PDouble(A)^, PDouble(B)^);
end;

class function TsgPointerTypeComparer.NearestFPoint(const A, B: Pointer): Double;
begin
  Result := TsgTypeComparer.NearestFPoint(PFPoint(A)^, PFPoint(B)^);
end;

{ TsgTypeComparer }

class function TsgTypeComparer.CmpCardinal(const A, B: Cardinal): Integer;
begin
  if A > B then
    Result := 1
  else
    if A < B then
      Result:= -1
    else
      Result := 0;
end;

class function TsgTypeComparer.CmpDouble(const A, B: Double): Integer;
begin
  Result := sgSignZ(A - B);
end;

class function TsgTypeComparer.CmpF2DPoint(const A, B: TF2DPoint): Integer;
begin
  Result := sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpF2DPointByX(const A, B: TF2DPoint): Integer;
begin
  if A.X > B.X then
    Result := +1
  else if A.X = B.X then
    Result := 0
  else
    Result := -1;
end;

class function TsgTypeComparer.CmpF2DPointByY(const A, B: TF2DPoint): Integer;
begin
  if A.Y > B.Y then
    Result := +1
  else if A.Y = B.Y then
    Result := 0
  else
    Result := -1;
end;

class function TsgTypeComparer.CmpF2DPointByYX(const A, B: TF2DPoint): Integer;
begin
  Result := CmpF2DPointByY(A, B);
  if Result = 0 then
    Result := CmpF2DPointByX(A, B);
end;

class function TsgTypeComparer.CmpFloat(const A, B: TsgFloat): Integer;
begin
  Result := sgSignZ(A - B);
end;

class function TsgTypeComparer.CmpFPoint(const A, B: TFPoint): Integer;
begin
  Result := CmpFPoint2D(A, B);
  if Result = 0 then
    Result := sgSignZ(A.Z - B.Z);
end;

class function TsgTypeComparer.CmpFPoint2DXnegY(const A, B: TFPoint): Integer;
begin
  Result := -sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpFPoint2DXYneg(const A, B: TFPoint): Integer;
begin
  Result := sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := -sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpFPoint2DXnegYneg(const A, B: TFPoint): Integer;
begin
  Result := -sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := -sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpFPoint2D(const A, B: TFPoint): Integer;
begin
  Result := sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpFPointByX(const A, B: TFPoint): Integer;
begin
  Result := sgSignZ(A.X - B.X);
end;

class function TsgTypeComparer.CmpFPointByY(const A, B: TFPoint): Integer;
begin
  Result := sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpFPointByZ(const A, B: TFPoint): Integer;
begin
  Result := sgSignZ(A.Z - B.Z);
end;

class function TsgTypeComparer.CmpHashItem(const A, B: TsgHashItem): Integer;
begin
  Result := CmpUInt64(A.HashCode, B.HashCode);
end;

class function TsgTypeComparer.CmpHashItemObject(const A, B: TsgHashItemObject): Integer;
begin
  Result := CmpUInt64(A.HashCode, B.HashCode);
end;

class function TsgTypeComparer.CmpInt64(const A, B: Int64): Integer;
begin
  if A = B then
    Result := 0
  else
    if A > B then
      Result := 1
    else
      Result := -1;
end;

class function TsgTypeComparer.CmpInteger(const A, B: Integer): Integer;
begin
  Result := A - B;
end;

class function TsgTypeComparer.CmpMethod(const A, B: TMethod): Integer;
begin
  Result := CmpPointer(A.Code, B.Code);
  if Result = 0 then
    Result := CmpPointer(A.Data, B.Data)
end;

class function TsgTypeComparer.CmpNativInt(const A, B: TsgNativeInt): Integer;
begin
{$IFDEF SG_CPUX64}
  Result := CmpInt64(A, B);
{$ELSE}
  Result := CmpInteger(A, B);
{$ENDIF}
end;

class function TsgTypeComparer.CmpNativUInt(const A, B: TsgNativeUInt): Integer;
begin
{$IFDEF SG_CPUX64}
  Result := CmpUInt64(A, B);
{$ELSE}
  Result := CmpCardinal(A, B);
{$ENDIF}
end;

class function TsgTypeComparer.CmpPointer(const A, B: Pointer): Integer;
begin
  Result := CmpNativUInt(TsgNativeUInt(A), TsgNativeUInt(B));
end;

class function TsgTypeComparer.CmpPointF(const A, B: TPointF): Integer;
begin
  Result := sgSignZ(A.X - B.X);
  if Result = 0 then
    Result := sgSignZ(A.Y - B.Y);
end;

class function TsgTypeComparer.CmpSingle(const A, B: Single): Integer;
begin
  Result := sgSignZ(A - B);
end;

class function TsgTypeComparer.CmpUInt64(const A, B: UInt64): Integer;
begin
  if A = B then
    Result := 0
  else
    if A > B then
      Result := 1
    else
      Result := -1;
end;

class function TsgTypeComparer.NearestDouble(const A, B: Double): Double;
begin
  Result := A - B;
  if Result < 0 then
    Result := -Result;
end;

class function TsgTypeComparer.NearestFPoint(const A, B: TFPoint): Double;
begin
  Result := DistanceFPoint(A, B);
end;

class function TsgTypeComparer.CmpColorCAD(const A, B: TsgColorCAD): Integer;
begin
  if A.Active = B.Active then
    Result := CmpCardinal(Cardinal(A.Color), Cardinal(B.Color))
  else
    if A.Active = acIndexColor then
      Result := -1
    else
      Result := +1;
end;

class function TsgTypeComparer.CmpStr(const A, B: string;
  ACaseSensitivity: Boolean): Integer;
begin
  if ACaseSensitivity then
    Result := AnsiCompareStr(A, B)
  else
    Result := AnsiCompareText(A, B);
end;

class function TsgTypeComparer.CmpStrAsNumber(const A, B: string): Integer;
var
  vLen1, vLen2, vNum1, vNum2: Integer;
  S1, S2: string;
begin
  Result := 0;
  vLen1 := Length(A);
  vLen2 := Length(B);
  if vLen1 > 0 then
  begin
    if vLen2 > 0 then
    begin
      S1 := A;
      S2 := B;
      SetNumberFromStr(vNum1, S1);
      SetNumberFromStr(vNum2, S2);
      Result :=  TsgTypeComparer.CmpInteger(vNum1, vNum2);
      if Result = 0 then
      begin
        Result := TsgTypeComparer.CmpStr(S1, S2, True);
      end;
    end
    else
      Result := 1;
  end
  else
   if vLen2 > 0 then
     Result := -1;
end;

{ TsgPointerConverter }

class function TsgPointerConverter.GetAsPF2DPoint(const AItem: Pointer): TFPoint;
begin
  Result.X := PF2DPoint(AItem)^.X;
  Result.Y := PF2DPoint(AItem)^.Y;
  Result.Z := 0;
end;

class function TsgPointerConverter.GetAsPFPoint(const AItem: Pointer): TFPoint;
begin
  Result := PFPoint(AItem)^;
end;

class function TsgPointerConverter.GetAsPPoint(const AItem: Pointer): TFPoint;
begin
  Result.X := PPoint(AItem)^.X;
  Result.Y := PPoint(AItem)^.Y;
  Result.Z := 0;
end;

class procedure TsgPointerConverter.SetAsPF2DPoint(const AItem: Pointer;
  const AValue: TFPoint);
begin
  PF2DPoint(AItem)^.X := AValue.X;
  PF2DPoint(AItem)^.Y := AValue.Y;
end;

class procedure TsgPointerConverter.SetAsPFPoint(const AItem: Pointer;
  const AValue: TFPoint);
begin
  PFPoint(AItem)^ := AValue;
end;

class procedure TsgPointerConverter.SetAsPPoint(const AItem: Pointer;
  const AValue: TFPoint);
begin
  PPoint(AItem)^.X := Round(AValue.X);
  PPoint(AItem)^.Y := Round(AValue.Y);
end;

end.

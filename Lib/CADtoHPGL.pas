{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   Export CAD to HPGL                       }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoHPGL;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, MVFont,
{$ENDIF}
  CADExport, CADImage, sgBitmap,
  sgConsts, sgFunction{$IFDEF SGDEL_2009}{$IFNDEF SG_FIREMONKEY}, AnsiStrings{$ENDIF}{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
  ;

const
  cntRTLEsc = AnsiChar(#27);
  cntRTLPersonalityModeCmd = AnsiChar('%');
  cntRTLDeviceFeatureControlCmd = AnsiChar('&');
  cntRTLGraphicsControlCmd = AnsiChar('*');

type

  THpglStrings = class(TPersistent)
  private
    FList: TList;
    FStream: TStream;
    FBuffPos: PAnsiChar;
    FBuffBase: PAnsiChar;
    function GetCount: Integer;
    function GetStrings(Index: Integer): AnsiString;
    procedure SetStrings(Index: Integer; const Value: AnsiString);
    procedure DoWriteBuffer;{$IFDEF SG_INLINE} inline;{$ENDIF}
  protected
    function Extract(Index: Integer): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(const S: AnsiString): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Flip;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: AnsiString read GetStrings
      write SetStrings; default;
  end;

  TsgCADtoHPGL = class(TsgSimpleExport)
  private
    FData: THpglStrings;
    FClips: THpglStrings;
    FClip: AnsiString;
    FCurPen: TColor;
    FStrokePen: TColor;
    FFillPen: TColor;
    FPt0: TPoint;
    FHd: Double;
    FWd: Double;
    FIsOleExport: Boolean;
    FPCLBitmap: TsgPCLBitmap;
    FPack: PPoints;
    FPackSize: Integer;
    FWritePJLHeader: Boolean;
    FUsePECommand: Boolean;
    procedure PolyPoints(AHPGLCommandStack: TObject; Points: PPoint; Count: Integer);
    procedure SetPen(Pen: TColor);
    procedure ClearPackBuffer;
    function Pack(APoints: PPoints; var ACount: Integer): Pointer;
  protected
//    function GetExportRect: TRect; override;
    procedure ExpPolyline(Points: PPoint; Count: Integer); override;
    procedure ExpPolygon(Points: PPoint; Count: Integer); override;
    procedure ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
//    procedure ExpFillRgn(P: PRect; Count: Integer); override;
    procedure ExpClipRgn(P: PRect; Count: Integer); override;
    procedure ExpSaveDC; override;
    procedure ExpRestoreDC; override;
    procedure ExpImage(const R: TRect; AImage: TPersistent); override;
    procedure ExpImageUV(const APoint1, APoint2, APoint3,  APoint4: TPoint;
      AImage: TPersistent); override;
    procedure ExpSetPenWidth(AWidth: Double); override;
    procedure PageStart(N: Integer); override;
//    procedure PageEnd(N: Integer); override;
    procedure SetStrokeColor(AColor: TColor); override;
    procedure SetFillColor(AColor: TColor); override;
    procedure ExpCloseFigure; override;
    function Flipping: Boolean; override;
    procedure SaveToStreamCustom(S: TStream); override;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    property WritePJLHeader: Boolean read FWritePJLHeader write FWritePJLHeader;
    property UsePECommand: Boolean read FUsePECommand write FUsePECommand;
  end;

  TsgHPGLExport = TsgCADtoHPGL;

implementation

type
  TsgHPGLCmd = Word;

const
  // 1 plu = 0,025 mm
  // 40 plu = 1 mm
  // 1016 plu = 1 inch

  MMPerInch = 25.4;
  cnstDefaultPageWidth = 210.0; //mm
  cnstDefaultPageHeight = 297.0; //mm
  cnstDefaultPageHeightIn = (cnstDefaultPageHeight * 40) / 1016;
  cnstDefaultPageWidthIn = (cnstDefaultPageWidth * 40) / 1016;
  cnstDPI = 300;

  cnstMaxIntValueLenWithTail = 12;

  HPGLCmdFP = TsgHPGLCmd($5046);
  HPGLCmdPA = TsgHPGLCmd($4150);
  HPGLCmdPC = TsgHPGLCmd($4350);
  HPGLCmdPD = TsgHPGLCmd($4450);
  HPGLCmdPM = TsgHPGLCmd($4D50);
  HPGLCmdPU = TsgHPGLCmd($5550);
  HPGLCmdPR = TsgHPGLCmd($5250);
  HPGLCmdPE = TsgHPGLCmd($4550);
  HPGLCmdEP = TsgHPGLCmd($5045);

  HPGLCmdIW = TsgHPGLCmd($5749);

  HPGLCmdEA = TsgHPGLCmd($4145);
  HPGLCmdER = TsgHPGLCmd($5245);

  HPGLCmdRA = TsgHPGLCmd($4152);
  HPGLCmdRR = TsgHPGLCmd($5252);

  HPGLCmdPW = TsgHPGLCmd(Byte('P') or Byte('W') shl 8 );

type
  TsgBitmapAccess = class(TsgBitmap);

  TsgHPGLParamType = (ptNoParam, ptInts, ptValue, ptSym, ptPoint, ptPolyPointsIndex, ptBase64);

  TsgHPGLCommandParamItem = record
    case TsgHPGLParamType of
      ptNoParam: (V: Pointer);
      ptInts: (Ints: PInteger);
      ptValue: (Value: Integer);
      ptSym: (Sym: AnsiChar);
      ptPoint: (Point: TPoint);
      ptPolyPointsIndex, ptBase64: (Pen, Index: Integer);
  end;

  TsgHPGLCommand = record
    Cmd: TsgHPGLCmd;
    ParamType: TsgHPGLParamType;
    Count: Integer;
    Param: TsgHPGLCommandParamItem;
  end;

  TsgHPGLCommandArray = array of TsgHPGLCommand;

  PPointArray = ^TPointArray; 
  TPointArray = array of TPoint;

  TsgHPGLCommandStack = class
  private
    FItems: TsgHPGLCommandArray;
    FLine: AnsiString;
    FBuff: PAnsiChar;
    FPolyPoints: array of TPointArray;
    FBase64: array of AnsiString;
    procedure PutPts(AInts: PInteger; ACount: Integer); {$IFDEF SG_INLINE} inline;{$ENDIF}
    function Grow: Integer; {$IFDEF SG_INLINE} inline;{$ENDIF}
    function AddPoints(const APoints: array of TPoint): Integer;
    function AddBase64(const APoints: array of TPoint; var ALen: Integer): Integer;
  protected
    function CalcBuffLen: Integer;
    function BuildLine: AnsiString;

    function Call(ACmd: TsgHPGLCmd): Integer; overload;
    function Call(ACmd: TsgHPGLCmd; ASym: AnsiChar): Integer; overload;
    function Call(ACmd: TsgHPGLCmd; AInts: PInteger; ACount: Integer): Integer; overload;
    function CallPoints(ACmd: TsgHPGLCmd; const APoints: array of TPoint): Integer;
    function Call(ACmd: TsgHPGLCmd; const APoint: TPoint): Integer; overload;
    function Call(ACmd: TsgHPGLCmd; const AValue: Integer): Integer; overload;
    property Items: TsgHPGLCommandArray read FItems;
    property Line: AnsiString read FLine;
  end;

  TsgRTLDataType = (rtldtNone, rtldtSym, rtldtValue, rtldtBin);

  TsgRTLData = record
    case TsgRTLDataType of
      rtldtNone: (V: Pointer);
      rtldtSym: (Sym: AnsiChar);
      rtldtValue: (Value: Integer);
      rtldtBin: (Bin: Pointer);
  end;

  TsgRTLCommand = record
    Parameterized: AnsiChar;
    Param: AnsiChar;
    Cmd: AnsiChar;
    Count: Integer;
    DataType: TsgRTLDataType;
    Data: TsgRTLData;
  end;

  TsgRTLCommandArray = array of TsgRTLCommand;

  TsgRTLCommandStack = class
  private
    FItems: TsgRTLCommandArray;
    FPreamble: AnsiString;
    FLine: AnsiString;
    FBuff: PAnsiChar;
    function Grow: Integer; {$IFDEF SG_INLINE} inline;{$ENDIF}
    procedure FillBuff(const S: AnsiString); overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
    procedure FillBuff(AData: Pointer; ASize: Integer); overload;{$IFDEF SG_INLINE} inline;{$ENDIF}
  protected
    function CalcBuffLen: Integer;
    function BuildLine: AnsiString;
    function Call(AParam: AnsiChar; AValue: Integer; ACmd: AnsiChar;
      const AParameterized: AnsiChar = cntRTLGraphicsControlCmd): Integer; overload;
    function Call(AParam: AnsiChar; ASym: AnsiChar; ACmd: AnsiChar;
      const AParameterized: AnsiChar = cntRTLGraphicsControlCmd): Integer; overload;
    function Call(AParam: AnsiChar; AData: Pointer; ACount: Integer; ACmd: AnsiChar;
      const AParameterized: AnsiChar = cntRTLGraphicsControlCmd): Integer; overload;
  public
    constructor Create(const APreamble: AnsiString);
  end;

{$IFDEF SG_CPUX86}
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
function _itosA(AValue: Integer; var ABuff): PAnsiChar; register; assembler;
asm
  push ebx // buffer
  push edi // divider 10
  push esi // temp register

  push edx // save buffer address

  mov ebx,[edx] // save data begining
  mov ecx,ebx   //

  mov esi,eax   // check sign
  or eax,eax
  jns @positive // if negative
  neg eax       // eax = Abs(AValue)

@positive:
  mov edi,10    // init divider

@divide:
  xor edx,edx   // clear :EDX
  div edi       // divide
  add edx,'0'   // do symbol
  mov byte ptr [ebx],dl // fill data
  inc ebx       // next data position
  test eax,eax  // test tail on zero
  jnz @divide   // loop

  or esi,esi    // test fill sign
  jns @positive1
  mov byte ptr [ebx],'-'
  inc ebx

@positive1:

  mov eax,ebx   // save end of buffer
  mov edi,eax

// swap      '54321-' => '-12345'
  dec ebx
@swap:
  mov dl,byte ptr[ebx]
  xchg byte ptr[ecx],dl
  mov byte ptr[ebx],dl
  dec ebx
  inc ecx
  cmp ebx,ecx
  jae @swap

  pop edx         // restore buffer address
  mov [edx],edi   // set new buffer position

  pop esi
  pop edi
  pop ebx
end;
{$ENDIF}
{$ENDIF}

function itoa(AValue: Integer): AnsiString;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := AnsiString(IntToStr(AValue));
end;

function CharsByUInt(AValue: Cardinal): Integer;
begin
  case AValue of
    0..9:                   Result := 1;
    10..99:                 Result := 2;
    100..999:               Result := 3;
    1000..9999:             Result := 4;
    10000..99999:           Result := 5;
    100000..999999:         Result := 6;
    1000000..9999999:       Result := 7;
    10000000..99999999:     Result := 8;
    100000000..999999999:   Result := 9;
  else
    Result := 10;
  end;
end;

function CharsByInt(AValue: Integer): Integer;
begin
  if AValue < 0 then
    Result := CharsByUInt(-AValue) + 1
  else
    Result := CharsByUInt(AValue)
end;

function CalcLen(const AInts: array of Integer): Integer; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(AInts) to High(AInts) do
    Inc(Result, CharsByInt(AInts[I]));
end;

function CalcLen(AInts: PInteger; ACount: Integer): Integer; overload;
begin
  Result := 0;
  while ACount > 0 do
  begin
    Inc(Result, CharsByInt(AInts^));
    Dec(ACount);
    Inc(AInts);
  end;
end;

function IsRectangle(APoints: PPoints; ACount: Integer): Boolean;
var
  Pts: array[0 .. 3] of TPoint;
begin
  Result := False;
  if (ACount = 5) then
    if (APoints^[0].X = APoints^[4].X) and (APoints^[0].Y = APoints^[4].Y) then
    begin
      Pts[0] := SubPoint(APoints^[0], APoints^[1]);
      Pts[1] := SubPoint(APoints^[1], APoints^[2]);
      Pts[2] := SubPoint(APoints^[3], APoints^[2]);
      Pts[3] := SubPoint(APoints^[0], APoints^[3]);
      Result := IsEqualPoints(Pts[0], Pts[2]) and IsEqualPoints(Pts[1], Pts[3]) and
        ((Pts[0].X = 0) xor (Pts[0].Y = 0));
    end;
end;

{ TsgHPGLExport }

procedure TsgCADtoHPGL.ClearPackBuffer;
begin
  if FPack <> nil then
  begin
    FreeMem(FPack, FPackSize);
    FPackSize := 0;
    FPack := nil;
  end;
end;

constructor TsgCADtoHPGL.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FData := THpglStrings.Create;
  FClips := THpglStrings.Create;
  FClip := 'IW;';
  FCurPen := clNone; FStrokePen := clNone; FFillPen := clNone;
  FIsOleExport := False;
  LayoutExportMode := lemAllPaperSpaces;
  FPCLBitmap.ColorSpace := 0;
  FPCLBitmap.Encode := 1;
  FPCLBitmap.BitIndex := 1;
  FPCLBitmap.BitRed := 8;
  FPCLBitmap.BitGrn := 8;
  FPCLBitmap.BitBlue := 8;
  //For check in CadImage.DrawHatch
  if Assigned(CADImage) and Assigned(CADImage.ExpProcs) then
    CADImage.ExpProcs.ExpFillRgn := nil;
  FWritePJLHeader := True;
  FUsePECommand := True;
end;

destructor TsgCADtoHPGL.Destroy;
begin
  ClearPackBuffer;
  FData.Free;
  FClips.Free;
  inherited Destroy;
end;

procedure _FillIntA(const AValue: Integer; var ABuff: PAnsiChar; AEoln: AnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
var
  vStr: AnsiString;
  Len: Integer;
begin
  vStr := itoa(AValue);
  Len := Length(vStr);
  Move(PPointer(vStr)^, ABuff^, Len);
  ABuff[Len] := AEoln;
  Inc(Len);
  Inc(ABuff, Len);
end;

procedure FillIntA(const AValue: Integer; var ABuff: PAnsiChar; AEoln: AnsiChar);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
{$IFDEF SG_CPUX86}
  _itosA(AValue, ABuff);
  ABuff^ := AEoln;
  Inc(ABuff);
{$ELSE}
  _FillIntA(AValue, ABuff, AEoln);
{$ENDIF}
{$ELSE}
  _FillIntA(AValue, ABuff, AEoln);
{$ENDIF}
end;

procedure TsgCADtoHPGL.SetPen(Pen: TColor);
var
  vRGBA: array[0 .. 3] of Byte absolute Pen;
  vInts: array[0 .. 3] of Integer;
  vStack: TsgHPGLCommandStack;
begin
  if Pen <> FCurPen then
  begin
    FCurPen := Pen;
    vInts[0] := 1;
    vInts[1] := vRGBA[0];
    vInts[2] := vRGBA[1];
    vInts[3] := vRGBA[2];
    vStack := TsgHPGLCommandStack.Create;
    try
      vStack.Call(HPGLCmdPC, @vInts[0], 4);
      FData.Add(vStack.BuildLine);
    finally
      vStack.Free;
    end;
  end;
end;

procedure TsgCADtoHPGL.PolyPoints(AHPGLCommandStack: TObject; Points: PPoint; Count: Integer);
var
  PDst: PPoints;
  vStack: TsgHPGLCommandStack;
begin
  PDst := PPoints(Pack(PPoints(Points), Count));
  vStack := TsgHPGLCommandStack(AHPGLCommandStack);
  if not UsePECommand then
  begin
    if Count > 1 then
    begin
      FPt0 := PDst^[0];
      vStack.Call(HPGLCmdPU, FPt0);
      vStack.CallPoints(HPGLCmdPD, Slice(PPoints(@PDst^[1])^, Count - 1));
    end;
  end
  else
    if Count > 1 then
    begin
      FPt0 := PDst^[0];
      if IsRectangle(PDst, Count) then
      begin
        vStack.Call(HPGLCmdPU, FPt0);
        vStack.Call(HPGLCmdER, SubPoint(PDst^[2], PDst^[0]))
      end
      else
        vStack.CallPoints(HPGLCmdPE, Slice(PDst^, Count));
    end
    else
      if Count = 1 then
      begin
        FPt0 := PDst^[0];
        vStack.CallPoints(HPGLCmdPE, [FPt0, FPt0]);
      end;
end;

procedure TsgCADtoHPGL.ExpPolyline(Points: PPoint; Count: Integer);
var
  vStack: TsgHPGLCommandStack;
begin
  vStack := TsgHPGLCommandStack.Create;
  try
    SetPen(FStrokePen);
    PolyPoints(vStack, Points, Count);
    FData.Add(vStack.BuildLine);
  finally
    vStack.Free;
  end;
end;

procedure TsgCADtoHPGL.ExpPolygon(Points: PPoint; Count: Integer);
var
  vStack: TsgHPGLCommandStack;
  PDst: PPoints;
begin
  PDst := PPoints(Pack(PPoints(Points), Count));
  if Count > 1 then
  begin
    vStack := TsgHPGLCommandStack.Create;
    try
      SetPen(FFillPen);
      FPt0 := PDst^[0];
      if UsePECommand and IsRectangle(PDst, Count) then
        vStack.CallPoints(HPGLCmdPE, Slice(PDst^, Count))
      else
      begin
        vStack.Call(HPGLCmdPU, FPt0);
        vStack.Call(HPGLCmdPM);
        Dec(Count);
        Inc(PPoint(PDst));
        vStack.CallPoints(HPGLCmdPD, Slice(PDst^, Count));
        vStack.Call(HPGLCmdPM, '2');
        vStack.Call(HPGLCmdFP);
      end;
      FData.Add(vStack.BuildLine);
    finally
      vStack.Free;
    end;
  end;
end;

procedure TsgCADtoHPGL.ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer);
var
  PC: PInteger;
  PP: PPoint;
  vStack: TsgHPGLCommandStack;
begin
  vStack := TsgHPGLCommandStack.Create;
  try
    PP := PPoint(@Points);
    PC := Counts;
    SetPen(FStrokePen);
    while Count > 0 do
    begin
      PolyPoints(vStack, PP, PC^);
      Inc(PP, PC^);
      Inc(PC);
      Dec(Count);
    end;
    FData.Add(vStack.BuildLine);
  finally
    vStack.Free;
  end;
end;

procedure TsgCADtoHPGL.ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer);
var
  C: Integer;
  vStack: TsgHPGLCommandStack;
  PP, PDst: PPoints;
  PC: PInteger;

  procedure CloseSubPolygon(AStack: TsgHPGLCommandStack;
    const APoints: array of TPoint);
  begin
    AStack.CallPoints(HPGLCmdPD, APoints);
    AStack.Call(HPGLCmdPM, '1');
  end;

begin
  if Count > 0 then
  begin
    if Count = 1 then
      ExpPolygon(PPoint(@Points), Counts^)
    else
    begin
      vStack := TsgHPGLCommandStack.Create;
      try
        PP := PPoints(@Points);
        FPt0 := PP^[0];
        PC := Counts;
        SetPen(FFillPen);
        vStack.Call(HPGLCmdPU, FPt0);
        vStack.Call(HPGLCmdPM, '0');

        while Count > 0 do
        begin
          C := PC^;
          PDst := PPoints(Pack(PP, C));
          if PPoint(PP) = @Points then
          begin
            Dec(C);
            Inc(PPoint(PDst));
          end;
          CloseSubPolygon(vStack, Slice(PDst^, C));
          Inc(PPoint(PP), PC^);
          Inc(PC);
          Dec(Count);
        end;

        vStack.Call(HPGLCmdPM, '2');
        vStack.Call(HPGLCmdFP);
        FData.Add(vStack.BuildLine);
      finally
        vStack.Free;
      end;
    end;
  end;
end;

procedure TsgCADtoHPGL.ExpClipRgn(P: PRect; Count: Integer);
var
  vStack: TsgHPGLCommandStack;
begin
  if Count > 0 then
  begin
    vStack := TsgHPGLCommandStack.Create;
    try
      Dec(P);
      vStack.Call(HPGLCmdIW, @P^.Left, 4);
      FClip := vStack.BuildLine;
      FData.Add(FClip);
    finally
      vStack.Free;
    end;
  end;
end;

procedure TsgCADtoHPGL.ExpSaveDC;
begin
  FClips.Add(FClip);
end;

procedure TsgCADtoHPGL.ExpSetPenWidth(AWidth: Double);
var
  vWillChanged: Boolean;
begin
  vWillChanged := IsEqual(PenWidthInt, AWidth, fAccuracy);
  inherited ExpSetPenWidth(AWidth);
  if vWillChanged then
    FData.Add(AnsiString('PW '+ DoubleToStr(PenWidth*0.025, '.')+';'))
end;

function TsgCADtoHPGL.Flipping: Boolean;
begin
  Result := True;
end;

procedure TsgCADtoHPGL.ExpRestoreDC;
begin
  if FClips.Count > 0 then
  begin
    FClip := FClips[FClips.Count-1];
    FClips.Delete(FClips.Count-1);
    FData.Add(FClip);
  end;
end;

procedure TsgCADtoHPGL.ExpCloseFigure;
var
  vStack: TsgHPGLCommandStack;
begin
  vStack := TsgHPGLCommandStack.Create;
  try
    vStack.Call(HPGLCmdPD, FPt0);
    FData.Add(vStack.BuildLine);
  finally
    vStack.Free;
  end;
end;

procedure TsgCADtoHPGL.SetStrokeColor(AColor: TColor);
begin
  FStrokePen := AColor;
end;

procedure TsgCADtoHPGL.SetFillColor(AColor: TColor);
begin
  FFillPen := AColor;
end;

function TsgCADtoHPGL.Pack(APoints: PPoints; var ACount: Integer): Pointer;
var
  I, J: Integer;
begin
  if FPackSize < ACount * SizeOf(TPoint) then
  begin
    if FPack <> nil then
      FreeMem(FPack);
    FPackSize := ACount * SizeOf(TPoint);
    GetMem(FPack, FPackSize);
  end;
  I := 0;
  J := 0;
  FPack^[J] := APoints^[I];
  Inc(I);
  while I < ACount do
  begin
    if not IsEqualPoints(APoints^[I], FPack^[J]) then
    begin
      Inc(J);
      FPack^[J] := APoints^[I];
    end;
    Inc(I);
  end;
  ACount := J + 1;
  Result := FPack;
end;

procedure TsgCADtoHPGL.PageStart(N: Integer);
var
  vRect: TRect;
  W, H: Integer;
begin
  ClearPackBuffer;
  if N > 0 then FData.Add('PG;');
  vRect := GetExportRect;
  W := vRect.Right-vRect.Left;
  H := vRect.Bottom-vRect.Top;
  FHd := H / cnstDefaultPageHeightIn; // points in one in.
  FWd := W / cnstDefaultPageWidthIn;
end;

procedure TsgCADtoHPGL.ExpImage(const R: TRect; AImage: TPersistent);
begin
  FIsOleExport := True;
  ExpImageUV(R.TopLeft, Point(R.Right,R.Top),R.BottomRight,
    Point(R.Left,R.Bottom), AImage);
  FIsOleExport := False;
end;

// 0 1 3 2
procedure TsgCADtoHPGL.ExpImageUV(const APoint1, APoint2, APoint3, APoint4: TPoint;
  AImage: TPersistent);
type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[Byte] of TRGBQuad;
  PRGB = ^TRGB;
  TRGB = packed record
    Red, Green, Blue: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[Byte] of TRGB;

const
  BitCounts: array[pf1bit .. pf32Bit] of Byte = (1, 4, 8, 16, 16, 24, 32);
var
  vAngle, vAngleY: Double;
  R: Integer;
  vBitmap: TsgBitmapAccess;
  vBmp: TBitmap;
//  vROP: Integer;
  vR: TRect;
  vPoint, vPoint1, vPoint2: TFPoint;
  vScale: Integer;
  I, J: Integer;
  P: PInteger;
  vDstW, vDstH: Integer;
  vBytesPerLine: Integer;
  vLines: TList;
  vPixelFormat: TPixelFormat;
  vHeight, vWidth, vTransparentOff: Integer;
  vCompressionMethod: Integer;
  vPalette: AnsiString;
  vColors: array[Byte] of TRGBQuad;
  vColorsCount: Integer;
  vBGRColors: PRGBArray;
  vHPGLStack: TsgHPGLCommandStack;
  vPreamble: AnsiString;
  vRTLStack: TsgRTLCommandStack;

  function PackBits(ABits: Pointer; W, H: Integer; APixelFormat: TPixelFormat): TList;
  var
    Row: Integer;
    BytesPerLn: Integer;
    OutputBuffer, LiteralPos, ControlPos: PByte;
    PosLn, EOLn: PByte;

    function DoCopy(AOutBuff, ALastPos: PByte): Pointer;
    var
      LineSize: Integer;
    begin
      LineSize := Integer(TsgNativeUInt(ALastPos) - TsgNativeUInt(AOutBuff) + 1);
      GetMem(Result, LineSize + SizeOf(Integer));
      PInteger(Result)^ := LineSize;
      Move(AOutBuff^, PByte(TsgNativeUInt(Result) + SizeOf(Integer))^, LineSize);
    end;

  begin
    Result := TList.Create;
    BytesPerLn := BytesPerScanline(W, BitCounts[APixelFormat], 32);
    GetMem(OutputBuffer, BytesPerLn shl 1 + BytesPerLn shr 2);
    try
      for Row := 0 to H - 1 do
      begin
        PosLn := PByte(TsgNativeInt(ABits) + (H - Row - 1) * BytesPerLn);
        EOLn := PosLn;
        Inc(EOLn, BytesPerLn);
        LiteralPos := OutputBuffer;
        Dec(LiteralPos);
        while TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn) do
        begin
          repeat
            ControlPos := LiteralPos;
            Inc(ControlPos);
            ControlPos^ := 0;
            LiteralPos := ControlPos;
            Inc(LiteralPos);
            LiteralPos^ := PosLn^;
            Inc(PosLn);
            while (TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn)) and
              (PosLn^ = LiteralPos^) and (ShortInt(ControlPos^) > -127) do
            begin
              Dec(ControlPos^);
              Inc(PosLn);
            end;
            if (ControlPos^ = 0) and (TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn)) then
            begin
              Dec(LiteralPos, 2);
              Dec(PosLn);
            end;
          until ShortInt(ControlPos^) <> -127;
          if TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn) then
          begin
            repeat
              ControlPos := LiteralPos;
              Inc(ControlPos);
              ControlPos^ := 0;
              LiteralPos := ControlPos;
              Inc(LiteralPos);
              LiteralPos^ := PosLn^;
              Inc(PosLn);
              while (TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn)) and
                (PosLn^ <> LiteralPos^) and (ControlPos^ < 127) do
              begin
                Inc(ControlPos^);
                Inc(LiteralPos);
                LiteralPos^ := PosLn^;
                Inc(PosLn);
              end;
              if (ControlPos^ = 0) and (TsgNativeUInt(PosLn) < TsgNativeUInt(EOLn)) then
              begin
                Dec(LiteralPos, 2);
                Dec(PosLn);
              end;
            until ControlPos^ <> 127;
          end;
          if TsgNativeUInt(PosLn) >= TsgNativeUInt(EOLn) then
            Result.Add(DoCopy(OutputBuffer, LiteralPos));
        end;
      end;
    finally
      FreeMem(OutputBuffer);
    end;
  end;

  function MakePalEntryStr(const AColor: TRGBQuad; AIndex: Integer): AnsiString;
  begin
    Result := itoa(AColor.rgbRed) + 'a' + itoa(AColor.rgbGreen) + 'b' +
      itoa(AColor.rgbBlue) + 'c' + itoa(AIndex);
  end;

  function PaletteToStr(const AColors: array of TRGBQuad): AnsiString;
  var
    L: Integer;
  begin
    if Length(AColors) > 0 then
    begin
      L := Low(AColors);
      Result := MakePalEntryStr(AColors[L], L);
      Inc(L);
      while L <= High(AColors) do
      begin
        Result := Result + 'i' + MakePalEntryStr(AColors[L], L);
        Inc(L);
      end;
    end
    else
      Result := '';
  end;

  function CreateLines(ABitmap: TPersistent): TList; overload;
  var
    vRow: Integer;
  {$IFDEF SG_FIREMONKEY}
    I: Integer;
    vBitData: TBitmapData;
    vBGRCol: PRGBArray;
    vBGRACol: PAlphaColorRec;
  {$ENDIF}
  begin
    Result := TList.Create;
    if ABitmap is TsgBitmap then
    begin
      for vRow := 0 to TsgBitmap(ABitmap).Height - 1 do
        Result.Add(TsgBitmap(ABitmap).ScanLine[vRow]);
    end
    else
    begin
    {$IFDEF SG_FIREMONKEY}
      TBitmap(ABitmap).Map(TMapAccess.Read, vBitData);
    {$ENDIF}
      for vRow := 0 to TBitmap(ABitmap).Height - 1 do
    {$IFNDEF SG_FIREMONKEY}
        Result.Add(TBitmap(ABitmap).ScanLine[vRow]);
    {$ELSE}
      begin
        GetMem(vBGRCol, BytesPerScanline(TBitmap(ABitmap).Width, 24, 32));
        vBGRACol := PAlphaColorRec(vBitData.GetScanline(vRow));
        for I := 0 to TBitmap(ABitmap).Width - 1 do
        begin
        {$IFDEF SG_FM_WINDOWS}
          vBGRCol^[I].Red := vBGRACol^.B;
          vBGRCol^[I].Blue := vBGRACol^.R;
        {$ELSE}
          vBGRCol^[I].Red := vBGRACol^.R;
          vBGRCol^[I].Blue := vBGRACol^.B;
        {$ENDIF}
          vBGRCol^[I].Green := vBGRACol^.G;
          Inc(vBGRACol);
        end;
        Result.Add(vBGRCol);
      end;
      TBitmap(ABitmap).Unmap(vBitData);
    {$ENDIF}
    end;
  end;

begin
  if (AImage = nil) or IsGreatImage(AImage) then
    Exit;
  ClearPackBuffer;
  vPoint := MakeFPointFromPoint(APoint1);
  vPoint1 := MakeFPointFromPoint(APoint2);
  vPoint2 := MakeFPointFromPoint(APoint4);

  vAngle := GetAngleByPoints(vPoint, vPoint1, False, fDoubleResolution ,1, 1);
  vAngleY :=  GetAngleByPoints(vPoint, vPoint2, False, fDoubleResolution, 1, 1);

  vR.TopLeft := APoint1;
  vR.BottomRight := APoint1;
  ExpandRect(vR, APoint2);
  ExpandRect(vR, APoint3);
  ExpandRect(vR, APoint4);

  vDstW := Abs(Round((vR.Bottom-vR.Top) - (vR.Bottom-vR.Top)/(3.39)));
  vDstH := Abs(Round((vR.Right-vR.Left) - (vR.Right-vR.Left)/(3.39)));

  vScale := 3;
  vLines := nil;
  vPalette := '';
  vColorsCount := 0;
  vBytesPerLine := 0;
  vCompressionMethod := 0;
  vBmp := nil;
  vRTLStack := nil;
  try
    vBitmap := TsgBitmapAccess(TsgBitmap.Create);
    try
      vBitmap.Assign(AImage);
      if IsEqual((Round(vAngle) div 90) * 90, vAngle) then
      begin
        if (vAngle = 0) and (vAngleY = 90) then
        begin
          vBitmap.Rotate(90);
        end
        else
        begin
          R := (Round(vAngle) div 90) * 90;
          if R <> 0 then
          begin
            if FIsOleExport then
            begin
              vBitmap.Rotate(R+90);
            end
            else
            begin
              vBitmap.Rotate(360-R+90);
            end;
          end;
        end;
      end
      else
      begin
        Exit;
      end;
      vHeight := vBitmap.Height;
      vWidth := vBitmap.Width;
      vPixelFormat := vBitmap.PixelFormat;
      vTransparentOff := Ord(not vBitmap.Transparent);
      case vPixelFormat of
        pf1bit:
          begin
            vColorsCount := vBitmap.GetColors(@vColors);
            vLines := PackBits(vBitmap.ScanLine[vHeight - 1], vWidth, vHeight, vPixelFormat);
            FreeAndNil(vBitmap);
            vCompressionMethod := 2;
          end;
        pf4bit, pf8bit:
          begin
            vColorsCount := vBitmap.GetColors(@vColors);
            vLines := CreateLines(vBitmap);
          end;
//        pf16bit:
//          begin
//            vLines := CreateLines(vBitmap);
//          end;
//        pf24bit:
//          begin
//            vCompressionMethod := 2;
//            vLines := CreateLines(vBitmap);
//            for I := 0 to vLines.Count - 1 do
//            begin
//              vBGRColors := PRGBArray(vLines[I]);
//              for J := 0 to vWidth - 1 do
//                SwapByte(vBGRColors^[J].Red, vBGRColors^[J].Blue);
//            end;
//            if vCompressionMethod = 2 then
//            begin
//              vLines.Free;
//              vLines := PackBits(vBitmap.ScanLine[vHeight - 1], vWidth, vHeight, vPixelFormat);
//              FreeAndNil(vBitmap);
//            end;
//          end;
      else
        if vPixelFormat <> pf24bit then
        begin
          vPixelFormat := pf24bit;
          vBmp := TBitmap.Create;
          vBmp.PixelFormat := vPixelFormat;
          SetSizeBmp(vBmp, vWidth, vHeight);
          vBitmap.FROP := SRCCOPY;
        {$IFDEF SG_FIREMONKEY}
          vBmp.Canvas.BeginScene;
        {$ENDIF}
          vBmp.Canvas.StretchDraw(vBmp.Canvas.ClipRect, vBitmap);
        {$IFDEF SG_FIREMONKEY}
          vBmp.Canvas.EndScene;
        {$ENDIF}
          FreeAndNil(vBitmap);
          vLines := CreateLines(vBmp);
        end
        else
          vLines := CreateLines(vBitmap);
        for I := 0 to vLines.Count - 1 do
        begin
          vBGRColors := PRGBArray(vLines[I]);
          for J := 0 to vWidth - 1 do
            SwapByte(vBGRColors^[J].Red, vBGRColors^[J].Blue);
        end;
      end; { case }
    finally
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
    end;
    case vPixelFormat of
      pf1bit:
        begin
          vPalette := PaletteToStr(Slice(PRGBQuadArray(@vColors)^, vColorsCount));
          FPCLBitmap.Encode := 1;
          FPCLBitmap.BitIndex := 1;
        end;
      pf4bit:
        begin
          vPalette := PaletteToStr(Slice(PRGBQuadArray(@vColors)^, vColorsCount));
          vBytesPerLine := BytesPerScanline(vWidth, BitCounts[vPixelFormat], 32);
          FPCLBitmap.Encode := 1;
          FPCLBitmap.BitIndex := 2;
        end;
      pf8bit:
        begin
          vPalette := PaletteToStr(Slice(PRGBQuadArray(@vColors)^, vColorsCount));
          vBytesPerLine := vWidth;
          FPCLBitmap.Encode := 1;
          FPCLBitmap.BitIndex := 8;
        end;
//      pf16bit:
//        begin
//          vBytesPerLine := BytesPerScanline(vWidth, BitCounts[vPixelFormat], 32);
//          FPCLBitmap.Encode := 2;
//          FPCLBitmap.BitIndex := 1;
//        end;
      pf24bit:
        begin
          vBytesPerLine := vWidth * 3;//BytesPerScanline(vWidth, BitCounts[vPixelFormat], 32);
          FPCLBitmap.Encode := 3;
          FPCLBitmap.BitIndex := 8;
        end;
    else
      vBytesPerLine := BytesPerScanline(vWidth, BitCounts[vPixelFormat], 32);
      FPCLBitmap.Encode := 3;
      FPCLBitmap.BitIndex := 8;
    end;

    vPreamble := '';
    vHPGLStack := TsgHPGLCommandStack.Create;
    try
      vHPGLStack.Call(HPGLCmdPU);
      vHPGLStack.Call(HPGLCmdPA, PPoint(@vR.Left)^);
      vPreamble := vHPGLStack.BuildLine;
    finally
      vHPGLStack.Free;
    end;
    vRTLStack := TsgRTLCommandStack.Create(vPreamble);
    //HP RTL CAP is set to the HP-GL/2 pen position
    vRTLStack.Call('1', nil, 0, 'A', cntRTLPersonalityModeCmd);
    // Source Transparency Mode (OFF)
    vRTLStack.Call('v', vTransparentOff, 'N');
    // Pattern Transparency Mode (OFF)
    vRTLStack.Call('v', '1', 'O');
    // Negative Motion
    vRTLStack.Call('a', '1', 'N', cntRTLDeviceFeatureControlCmd);
    //Configure Image Data
    //                                   Bit Number
    //Byte 15                                8 7                           0   Byte
    //     0 Color Space                        Pixel Encoding Mode             1
    //     2 Number of Bits per Index           Number of Bits per Primary—Red  3
    //     4 Number of Bits per Primary—Green   Number of Bits per Primary—Blue 5
    //     6                   White Reference for Red                          7
    //     8                   White Reference for Green                        9
    //     10                  White Reference for Blue                         11
    //     12                  Black Reference for Red                          13
    //     14                  Black Reference for Green                        15
    //     16                  Black Reference for Blue                         17
    vRTLStack.Call('v', @FPCLBitmap, 6, 'W');
    if vPalette <> '' then
      vRTLStack.Call('v', PAnsiChar(vPalette), Length(vPalette), 'I');
    //Move CAP Horizontal + Move CAP Vertical
    //S := S + Format(AnsiString(#27'*p%dX'#27'*p%dY'), [vR.Left, vR.Top]);
    //Source Raster Width + Source Raster Height
    vRTLStack.Call('r', vWidth, 'S');
    vRTLStack.Call('r', vHeight, 'T');
    // Destination Raster Width + Destination Raster Height
    vRTLStack.Call('t', vDstW, 'H');
    vRTLStack.Call('t', vDstH, 'V');
    //Start Raster Graphics
    vRTLStack.Call('r', vScale, 'A');
    // Compression Method:
    // 2 - Tagged Image File Format (TIFF) revision 4.0 "Packbits" encoding (row-based)
    // 10 - near lossless RGB/KCMY replacement delta row encoding
    vRTLStack.Call('b', vCompressionMethod, 'M');
    case vCompressionMethod of
      2:
        for I := 0 to vLines.Count - 1 do
        begin
          P := PInteger(vLines[I]);
          vRTLStack.Call('b', Pointer(TsgNativeInt(P) + SizeOf(Integer)), P^, 'W');
        end;
    else
      for I := 0 to vLines.Count - 1 do
        vRTLStack.Call('b', vLines[I], vBytesPerLine, 'W');
    end; { case }
    vRTLStack.Call('r', nil, 0, 'C');
    vRTLStack.Call('1', nil, 0, 'B', cntRTLPersonalityModeCmd);
    FData.Add(vRTLStack.BuildLine);
  finally
    vRTLStack.Free;
    vBmp.Free;
    if vCompressionMethod = 2 then
      ClearRecordList(vLines);
  {$IFDEF SG_FIREMONKEY}
    ClearRecordList(vLines);
  {$ENDIF}
    vLines.Free;
    FreeAndNil(vBitmap);
  end;
end;

procedure TsgCADtoHPGL.SaveToStreamCustom(S: TStream);
begin
  FData.Clear;
  if WritePJLHeader then
  begin
    FData.Add(#27'%-12345X@PJL SET RESOLUTION = ' + itoa(cnstDPI));
    FData.Add('@PJL SET ORIENTATION = LANDSCAPE');
    FData.Add('@PJL ENTER LANGUAGE = HPGL2');
    FData.Add(#27'%1B');
    FData.Add('BP5,1;IN;SP1;IW;PS9600,7100;TR0;IP0,0,7100,9600;SC;');
    //SC 0,1,0,1,2; Moves the origin to P1 and establishes a 1:1 ratio of user-units to plotter-units.
  end
  else
    FData.Add('IN;');
  inherited SaveToStreamCustom(S);
  ClearPackBuffer;
  FData.SaveToStream(S);
end;

{ THpglStrings }

function THpglStrings.Add(const S: AnsiString): Integer;
var
  vRef: AnsiString;
begin
  vRef := S;
  Result := FList.Add(PPointer(@vRef)^);
  PPointer(@vRef)^ := nil;
end;

procedure THpglStrings.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is THpglStrings then
  begin
    Clear;
    for I := 0 to THpglStrings(Source).Count - 1 do
      Add(THpglStrings(Source)[I]);
  end
  else
    if Source is TStrings then
    begin
      Clear;
      for I := 0 to TStrings(Source).Count - 1 do
        Add(AnsiString(TStrings(Source)[I]));
    end
    else
      inherited Assign(Source);
end;

procedure THpglStrings.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
end;

constructor THpglStrings.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure THpglStrings.Delete(Index: Integer);
var
  S: AnsiString;
begin
  Pointer(S) := FList[Index];
  FList.Delete(Index);
end;

destructor THpglStrings.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function THpglStrings.Extract(Index: Integer): AnsiString;
begin
  Result := Strings[Index];
  Delete(Index);
end;

function THpglStrings.GetCount: Integer;
begin
  Result := FList.Count;
end;

function THpglStrings.GetStrings(Index: Integer): AnsiString;
begin
  Result := AnsiString(FList[Index]);
end;

procedure THpglStrings.LoadFromStream(Stream: TStream);
var
  I: Integer;
  S: TStrings;
  vTmp: AnsiString;
begin
  S := TStringList.Create;
  try
    S.LoadFromStream(Stream);
    Clear;
    I := S.Count - 1;
    while I > 0 do
    begin
      vTmp := AnsiString(S[I]);
      Add(vTmp);
      S.Delete(I);
      Dec(I);
    end;
    Flip;
  finally
    S.Free;
  end;
end;

procedure THpglStrings.SetStrings(Index: Integer; const Value: AnsiString);
begin
  AnsiString(FList.List[Index]) := Value;
end;

procedure THpglStrings.Flip;
var
  J, C, K: Integer;
{$IFOPT O+}
  vTmp: Pointer;
{$ENDIF}
begin
  C := FList.Count;
  K := C shr 1 - 1;
{$IFOPT O+}
  Dec(C);
{$ENDIF}
  for J := 0 to K do
{$IFOPT O+}
    with FList do
    begin
      vTmp := List[J];
      List[J] := List[C];
      List[C] := vTmp;
      Dec(C);
    end;
{$ELSE}
    FList.Exchange(J, FList.Count - J - 1);
{$ENDIF}
end;

procedure THpglStrings.DoWriteBuffer;
begin
  FStream.WriteBuffer(FBuffBase^, FBuffPos - FBuffBase);
  FBuffPos := FBuffBase;
end;

procedure THpglStrings.SaveToStream(Stream: TStream);
const
  cnstEoln = $0A0D;
  cnstBufferLength = $F000;
var
  vSrc, vSrcEnd: PAnsiChar;
  vBytesWrite: Integer;
  vBuffEnd: PAnsiChar;
  S: AnsiString;

  procedure NextString(var S: AnsiString; AIndex: Integer);
  begin
    S := Extract(AIndex);
    vSrc := Pointer(S);
    vSrcEnd := vSrc;
    Inc(vSrcEnd, Length(S));
  end;

begin
  if Count > 0 then
  begin
    FStream := Stream;
    GetMem(FBuffBase, cnstBufferLength);
    try
      FBuffPos := FBuffBase;
      vBuffEnd := FBuffPos;
      Inc(vBuffEnd, cnstBufferLength);
      Flip;
      NextString(S, Count - 1);
      repeat
        while ((Count > 0) or (vSrc < vSrcEnd)) and (FBuffPos < vBuffEnd) do
        begin
          if vSrcEnd - vSrc <= vBuffEnd - FBuffPos then
            vBytesWrite := vSrcEnd - vSrc
          else
            vBytesWrite := vBuffEnd - FBuffPos;
          Move(vSrc^, FBuffPos^, vBytesWrite);
          Inc(FBuffPos, vBytesWrite);
          Inc(vSrc, vBytesWrite);
          if vSrc = vSrcEnd then
          begin
            case vBuffEnd - FBuffPos of
              0:
                begin
                  DoWriteBuffer;
                  PWord(FBuffPos)^ := cnstEoln;
                  Inc(FBuffPos, 2);
                end;
              1:
                begin
                  FBuffPos^ := #13;
                  Inc(FBuffPos);
                  DoWriteBuffer;
                  FBuffPos^ := #10;
                  Inc(FBuffPos);
                end;
              2:
                begin
                  PWord(FBuffPos)^ := cnstEoln;
                  Inc(FBuffPos, 2);
                  DoWriteBuffer;
                end;
            else
              PWord(FBuffPos)^ := cnstEoln;
              Inc(FBuffPos, 2);
            end;
            if Count > 0 then
              NextString(S, Count - 1);
          end
          else
            if FBuffPos = vBuffEnd then
              DoWriteBuffer;
        end;
      until Count <= 0;
      if FBuffPos > FBuffBase then
        DoWriteBuffer;
    finally
      FreeMem(FBuffBase);
    end;
  end;
end;

{ TsgHPGLCommandStack }


procedure TsgHPGLCommandStack.PutPts(AInts: PInteger; ACount: Integer);
var
  I: Integer;
begin
  if ACount > 0 then
  begin
    I := 0;
    while I < ACount - 1 do
    begin
      FillIntA(AInts^, FBuff, ',');
      Inc(AInts);
      Inc(I);
    end;
    FillIntA(AInts^, FBuff, ';');
  end;
end;

function TsgHPGLCommandStack.AddBase64(const APoints: array of TPoint;
  var ALen: Integer): Integer;
var
  I: Integer;
  S: AnsiString;
  P: PAnsiChar;
  vDelta: TPoint;
  vBytes: Integer;

  procedure IntToBase64(AValue: TsgNativeInt; var ABase64: PAnsiChar);
  const
    cnstTerm = 191;//$BF
    cnstBase64 = 63;//3F
  begin
    if AValue < 0 then
    begin
      AValue := -AValue;
      Inc(AValue);
    end;
    while AValue > cnstBase64 do
    begin
      ABase64^ := AnsiChar(AValue and cnstBase64 + cnstBase64);
      Inc(ABase64);
      AValue := AValue shr 6;
    end;
    ABase64^ := AnsiChar(AValue + cnstTerm);
    Inc(ABase64);
  end;

begin
  Result := Length(FBase64);
  SetLength(FBase64, Result + 1);

  vBytes := (Length(APoints) + 1) * SizeOf(TPoint);
  ALen := (3 * vBytes) div 4 + (2 * Length(APoints)) + 3;

  SetLength(S, ALen);
  P := PAnsiChar(S);

  I := 0;
  vDelta.X := APoints[I].X;
  vDelta.Y := APoints[I].Y;
  if IsRectangle(PPoints(@APoints[I]), Length(APoints)) then
  begin
    P^ := '9';
    Inc(P);
    P^ := '=';
    Inc(P);
    IntToBase64(vDelta.X * 2, P);
    IntToBase64(vDelta.Y * 2, P);
    vDelta.X := APoints[I+2].X - vDelta.X;
    vDelta.Y := APoints[I+2].Y - vDelta.Y;
  end
  else
  begin
    P^ := '<';
    Inc(P);
    P^ := '=';
    Inc(P);
    Inc(I);
    while I < Length(APoints) do
    begin
      IntToBase64(vDelta.X * 2, P);
      IntToBase64(vDelta.Y * 2, P);
      vDelta.X := APoints[I].X - APoints[I-1].X;
      vDelta.Y := APoints[I].Y - APoints[I-1].Y;
      Inc(I);
    end;
  end;

  IntToBase64(vDelta.X * 2, P);
  IntToBase64(vDelta.Y * 2, P);

  ALen := P - PAnsiChar(S);
  FBase64[Result] := S;
end;

function TsgHPGLCommandStack.AddPoints(const APoints: array of TPoint): Integer;
begin
  Result := Length(FPolyPoints);
  SetLength(FPolyPoints, Result + 1);
  SetLength(FPolyPoints[Result], Length(APoints));
  System.Move(APoints[0], FPolyPoints[Result][0], Length(APoints) * SizeOf(APoints[0]));
end;

function TsgHPGLCommandStack.BuildLine: AnsiString;
var
  Len: Integer;
  I: Integer;
  Pts: TPointArray;
begin
  Len := CalcBuffLen;
  SetLength(FLine, Len);
  FBuff := PAnsiChar(FLine);
  for I := Low(FItems) to High(FItems) do
  begin
    PWord(FBuff)^ := FItems[I].Cmd;
    Inc(FBuff, SizeOf(FItems[I].Cmd));
    case FItems[I].ParamType of
      ptNoParam:
        begin
          FBuff^ := ';';
          Inc(FBuff);
        end;
      ptInts:
        begin
          PutPts(FItems[I].Param.Ints, FItems[I].Count);
        end;
      ptValue:
        begin
          FillIntA(FItems[I].Param.Value, FBuff, ';');
        end;
      ptSym:
        begin
          FBuff^ := FItems[I].Param.Sym;
          Inc(FBuff);
          FBuff^ := ';';
          Inc(FBuff);
        end;
      ptPoint:
        begin
          PutPts(@FItems[I].Param.Point.X, 2);
        end;
      ptPolyPointsIndex:
        begin
          Pts := FPolyPoints[FItems[I].Param.Index];
          PutPts(@Pts[0], Length(Pts) shl 1);
        end;
      ptBase64:
        begin
          Move(FBase64[FItems[I].Param.Index][1], FBuff^, FItems[I].Count);
          Inc(FBuff, FItems[I].Count);
          FBuff^ := ';';
          Inc(FBuff);
        end;
    end;
  end;
  Result := FLine;
end;

function TsgHPGLCommandStack.CalcBuffLen: Integer;
var
  I: Integer;
  Pts: TPointArray;
begin
  Result := 0;
  for I := Low(FItems) to High(FItems) do
  begin
    Inc(Result, SizeOf(FItems[I].Cmd));
    case FItems[I].ParamType of
      ptNoParam: Inc(Result);
      ptInts: Inc(Result, CalcLen(FItems[I].Param.Ints, FItems[I].Count) + FItems[I].Count);
      ptValue: Inc(Result, CharsByInt(FItems[I].Param.Value) + 1);
      ptSym: Inc(Result, SizeOf(FItems[I].Param.Sym) + 1);
      ptPoint: Inc(Result, CalcLen(@FItems[I].Param.Point.X, 2) + 2);
      ptPolyPointsIndex:
        begin
          Pts := FPolyPoints[FItems[I].Param.Index];
          Inc(Result, CalcLen(@Pts[0], Length(Pts) shl 1) + Length(Pts) shl 1);
        end;
      ptBase64: Inc(Result, FItems[I].Count + 1);
    end;
  end;
end;

function TsgHPGLCommandStack.Call(ACmd: TsgHPGLCmd; ASym: AnsiChar): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  FItems[Result].ParamType := ptSym;
  FItems[Result].Param.Sym := ASym;
end;

function TsgHPGLCommandStack.Call(ACmd: TsgHPGLCmd): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  FItems[Result].ParamType := ptNoParam;
end;

function TsgHPGLCommandStack.Call(ACmd: TsgHPGLCmd;
  const APoint: TPoint): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  FItems[Result].ParamType := ptPoint;
  FItems[Result].Param.Point := APoint;
end;

function TsgHPGLCommandStack.Call(ACmd: TsgHPGLCmd; AInts: PInteger;
  ACount: Integer): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  FItems[Result].ParamType := ptInts;
  FItems[Result].Param.Ints := AInts;
  FItems[Result].Count := ACount;
end;

function TsgHPGLCommandStack.Call(ACmd: TsgHPGLCmd;
  const AValue: Integer): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  FItems[Result].ParamType := ptValue;
  FItems[Result].Param.Value := AValue;
end;

function TsgHPGLCommandStack.CallPoints(ACmd: TsgHPGLCmd;
  const APoints: array of TPoint): Integer;
begin
  Result := Grow;
  FItems[Result].Cmd := ACmd;
  if ACmd = HPGLCmdPE then
  begin
    FItems[Result].ParamType := ptBase64;
    FItems[Result].Param.Index := AddBase64(APoints, FItems[Result].Count);
  end
  else
  begin
    FItems[Result].ParamType := ptPolyPointsIndex;
    FItems[Result].Param.Index := AddPoints(APoints);
    FItems[Result].Count := Length(APoints);
  end;
end;

function TsgHPGLCommandStack.Grow: Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);
end;

{ TsgRTLCommandStack }

function TsgRTLCommandStack.BuildLine: AnsiString;
var
  I: Integer;
  Len: Integer;
  S: AnsiString;
begin
  Len := CalcBuffLen;
  SetLength(FLine, Len);
  FBuff := PAnsiChar(FLine);
  System.Move(Pointer(FPreamble)^, FBuff^, Length(FPreamble));
  Inc(FBuff, Length(FPreamble));
  for I := Low(FItems) to High(FItems) do
  begin
    FBuff^ := cntRTLEsc;
    Inc(FBuff);
    FBuff^ := FItems[I].Parameterized;
    Inc(FBuff);
    case FItems[I].Parameterized of
      cntRTLPersonalityModeCmd:
        begin
          case FItems[I].DataType of
            rtldtNone:
              begin
                FBuff^ := FItems[I].Param;
                Inc(FBuff);
              end;
            rtldtSym:
              begin
                FBuff^ := FItems[I].Data.Sym;
                Inc(FBuff);
              end;
            rtldtValue:
              begin
                S := itoa(FItems[I].Data.Value);
                FillBuff(S);
              end;
            //rtldtBin: Inc(Result, FItems[I].Count + CharsByInt(FItems[I].Count));
          end;
          FBuff^ := FItems[I].Cmd;
          Inc(FBuff);
        end;
      cntRTLDeviceFeatureControlCmd:
        begin
          FBuff^ := FItems[I].Param;
          Inc(FBuff);
          case FItems[I].DataType of
            rtldtNone:;
            rtldtSym:
              begin
                FBuff^ := FItems[I].Data.Sym;
                Inc(FBuff);
              end;
            rtldtValue:
              begin
                S := itoa(FItems[I].Data.Value);
                FillBuff(S);
              end;
            //rtldtBin: Inc(Result, FItems[I].Count + CharsByInt(FItems[I].Count));
          end;
          FBuff^ := FItems[I].Cmd;
          Inc(FBuff);
        end;
      cntRTLGraphicsControlCmd:
        begin
          FBuff^ := FItems[I].Param;
          Inc(FBuff);
          case FItems[I].DataType of
            rtldtNone:
              begin
                FBuff^ := FItems[I].Cmd;
                Inc(FBuff);
              end;
            rtldtSym:
              begin
                FBuff^ := FItems[I].Data.Sym;
                Inc(FBuff);
                FBuff^ := FItems[I].Cmd;
                Inc(FBuff);
              end;
            rtldtValue:
              begin
                S := itoa(FItems[I].Data.Value);
                FillBuff(S);
                FBuff^ := FItems[I].Cmd;
                Inc(FBuff);
              end;
            rtldtBin:
              begin
                if FItems[I].Cmd = 'I' then // palette
                begin
                  FillBuff(FItems[I].Data.Bin, FItems[I].Count);
                  FBuff^ := FItems[I].Cmd;
                  Inc(FBuff);
                end
                else
                begin
                  S := itoa(FItems[I].Count);
                  FillBuff(S);
                  FBuff^ := FItems[I].Cmd;
                  Inc(FBuff);
                  FillBuff(FItems[I].Data.Bin, FItems[I].Count);
                end;
              end;
          end;
        end;
    end;
  end;
  Result := FLine;
end;

function TsgRTLCommandStack.CalcBuffLen: Integer;
var
  I: Integer;
begin
  Result := Length(FPreamble);
  for I := Low(FItems) to High(FItems) do
  begin
    Inc(Result, SizeOf(cntRTLEsc) + SizeOf(FItems[I].Parameterized));
    case FItems[I].Parameterized of
      cntRTLPersonalityModeCmd: //('%')
        begin
          Inc(Result, SizeOf(FItems[I].Param));
        end;
      cntRTLDeviceFeatureControlCmd: //('&')
        begin
          Inc(Result, SizeOf(FItems[I].Param));
          case FItems[I].DataType of
            rtldtNone:;
            rtldtSym: Inc(Result);
            rtldtValue: Inc(Result, CharsByInt(FItems[I].Data.Value));
            rtldtBin: Inc(Result, FItems[I].Count + CharsByInt(FItems[I].Count));
          end;
        end;
      cntRTLGraphicsControlCmd: //('*');
        begin
          Inc(Result, SizeOf(FItems[I].Param));
          case FItems[I].DataType of
            rtldtNone:;
            rtldtSym: Inc(Result);
            rtldtValue: Inc(Result, CharsByInt(FItems[I].Data.Value));
            rtldtBin:
              begin
                Inc(Result, FItems[I].Count);
                if FItems[I].Cmd <> 'I' then // palette
                  Inc(Result, CharsByInt(FItems[I].Count));
              end;
          end;
        end;
    end;
    Inc(Result, SizeOf(FItems[I].Cmd));
  end;
end;

function TsgRTLCommandStack.Call(AParam, ASym, ACmd: AnsiChar;
  const AParameterized: AnsiChar): Integer;
begin
  Result := Grow;
  FItems[Result].Parameterized := AParameterized;
  FItems[Result].Param := AParam;
  FItems[Result].Cmd := ACmd;
  FItems[Result].DataType := rtldtSym;
  FItems[Result].Data.Sym := ASym;
end;

function TsgRTLCommandStack.Call(AParam: AnsiChar; AValue: Integer;
  ACmd: AnsiChar; const AParameterized: AnsiChar): Integer;
begin
  Result := Grow;
  FItems[Result].Parameterized := AParameterized;
  FItems[Result].Param := AParam;
  FItems[Result].Cmd := ACmd;
  FItems[Result].DataType := rtldtValue;
  FItems[Result].Data.Value := AValue;
end;

function TsgRTLCommandStack.Call(AParam: AnsiChar; AData: Pointer;
  ACount: Integer; ACmd: AnsiChar; const AParameterized: AnsiChar): Integer;
begin
  Result := Grow;
  FItems[Result].Parameterized := AParameterized;
  FItems[Result].Param := AParam;
  FItems[Result].Cmd := ACmd;
  if AData = nil then
    FItems[Result].DataType := rtldtNone
  else
    FItems[Result].DataType := rtldtBin;
  FItems[Result].Data.Bin := AData;
  FItems[Result].Count := ACount;
end;

constructor TsgRTLCommandStack.Create(const APreamble: AnsiString);
begin
  inherited Create;
  FPreamble := APreamble;
end;

procedure TsgRTLCommandStack.FillBuff(AData: Pointer; ASize: Integer);
begin
  Move(AData^, FBuff^, ASize);
  Inc(FBuff, ASize);
end;

procedure TsgRTLCommandStack.FillBuff(const S: AnsiString);
var
  Len: Integer;
begin
  Len := Length(S);
  System.Move(Pointer(S)^, FBuff^, Len);
  Inc(FBuff, Len);
end;

function TsgRTLCommandStack.Grow: Integer;
begin
  Result := Length(FItems);
  SetLength(FItems, Result + 1);
end;

end.

{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   Export CAD to CGM                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoCGM;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  Math, CADExport, CADImage, sgConsts, sgBitmap,
  sgLists
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
  ;

//{$DEFINE SG_DEBUG}

type

  TsgCADtoCGM = class(TsgSimpleExport)
  private
    FBuffer: TObject;
    FStream: TStream;
    FVersion: string;
    FHasLine: Boolean;
    FHasFill: Boolean;
    FLineColor: TColor;
    FFillColor: TColor;
    FIsModel: Boolean;
    FWeight: Integer;
    FOldColourSelectionMode: Word;
    procedure AddEdgeArttribetes;
    procedure AddLineArttribetes;
    procedure AddPoints(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure ExportPolyG(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure ExportPolyL(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure ExportRect(const R: PRect);
    procedure ExpBuffer;
    procedure SetColourSelectionMode(AMode: Word);
  protected
    procedure ExportHeaderStart;
    procedure ExportHeaderEnd;
    procedure ExpFillRgn(P: PRect; Count: Integer); override;
    procedure ExpPolyline(Points: PPoint; Count: Integer); override;
    procedure ExpPolygon(Points: PPoint; Count: Integer); override;
    procedure ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpImage(const R: TRect; AImage: TPersistent); override;
    procedure ExpImageUV(const APoint1, APoint2, APoint3,  APoint4: TPoint;
      AImage: TPersistent); override;
    procedure ExpSaveDC; override;
    procedure ExpRestoreDC; override;
    function Flipping: Boolean; override;
    function IsFormatMultiPages: Boolean; override;
    procedure PageEnd(N: Integer); override;
    procedure PageStart(N: Integer); override;
    procedure SaveToStreamCustom(S: TStream); override;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    property Version: string read FVersion write FVersion;
  end;

implementation

uses sgFunction;

const
  cnstMaxWord = $7FC0;
  cnstMaxCnt = $FFC0 shr 2;
  cnstMaxCntReal = cnstMaxWord shr 2;
  cnstMaxSize = cnstMaxWord div 2;
  cnstDefWidth = 0;

  //((Class and $0F) shl 12) + ((ID and $7F) shl 5) + (Lenght and $31)
  cnstBeginMetafile = $0021;//0.1.1
  cnstMetafileVersion = $1022;//1.1.2
  cnstMetafileDescription = $105F;//1.2.31
  cnstBeginPicture = $0060;//0.3.0
  cnstBeginPictureBody = $0080;//1.2.31
  cnstVDCType = $1062; //1.3.2
  cnstEndPicture = $00A0;
  cnstEndMetafile = $0040;
  cnstDescription = 'CADtoCGM';
  cnstDescriptionLen = Length(cnstDescription);
  cnstColorSelectionMode = $2042;//2.2.2
  cnstVDCExt = $20C8;//2.6.8
  cnstBackGround = $20E3;//2.7.3
  cnstFillColor = $52E3;//5.23.3
  cnstEdgeVisibility = $53C2;//5.30.2
  cnstEdgeWidth = $5382;//5.28.2
  cnstEdgeColor = $53A3;//5.29.3
  cnstInteriorStyle = $52C2;//5.22.2
  cnstHatchIndex = $5302;//5.24.2
  cnstLineWidth = $5062;//5.27.2
  cnstLineType = $5042;//5.2.2
  cnstLineColor = $5083;//5.4.3
  cnstPolyLine = $403F;//4.1.31
  cnstPolygon = $40FF;//4.7.31
  cnstRectangle = $4168;//4.11.8
  cnstCellArray = $413F; //4.9.31
  cnstColourTable = $545F; //5.34.31;

type
  TsgInteger = {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF};

  TsgList = class(TsgIntegerList)
  private
    FLength: LongWord;
  protected
    function IsEven: Boolean;
  public
    procedure AddAnsiString(Value: AnsiString; AUsedBlockMem: Integer = 0);
    procedure AddString(Value: string);
    procedure AddWord(Value: Word);
    procedure AddLongWord(Value: LongWord);
    procedure Clear(ClearCapacity: Boolean{ = False}); override;
    procedure FastClear;
    procedure SaveToStream(S: TStream);
    property Length: LongWord read FLength;
  end;

function GetColor(AColor: TColor): LongWord;
var
  vColors: array [0..3] of Byte absolute AColor;
  vBuf: Byte;
begin
  vBuf := vColors[0];
  vColors[0] := vColors[2];
  vColors[2] := vBuf;
  Result := AColor shl 8;
end;

{ TsgCADtoCGM }

function GetCode(const AVal1, AVal2, AVa3: Byte): Word;
begin
  Result := (AVal1 shl 12) or (AVal2 shl 5) or AVa3;
end;

constructor TsgCADtoCGM.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FVersion := '1.0';
  FBuffer := TsgList.Create;
  TsgList(FBuffer).Sorted := False;
  TTFMode := ttfPolyPolyline;
  FOldColourSelectionMode := 3;
end;

destructor TsgCADtoCGM.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TsgCADtoCGM.ExpBuffer;
begin
  TsgList(FBuffer).SaveToStream(FStream);
  TsgList(FBuffer).FastClear;
end;

procedure TsgCADtoCGM.AddEdgeArttribetes;
begin
  TsgList(FBuffer).AddWord(cnstFillColor);
  TsgList(FBuffer).AddLongWord(GetColor(FFillColor));
  TsgList(FBuffer).AddWord(cnstInteriorStyle);
  TsgList(FBuffer).AddWord(Word(FHasFill));
  TsgList(FBuffer).AddWord(cnstEdgeVisibility);
  TsgList(FBuffer).AddWord(Word(FHasLine));
//  TsgList(FBuffer).AddWord(cnstEdgeWidth);
//  TsgList(FBuffer).AddWord(FWeight);
  TsgList(FBuffer).AddWord(cnstEdgeColor);
  TsgList(FBuffer).AddLongWord(GetColor(FLineColor));
end;

procedure TsgCADtoCGM.ExpFillRgn(P: PRect; Count: Integer);
begin
  FHasLine := False;
  FHasFill := True;
  FFillColor := GetFillColor;
  FLineColor := FFillColor;
  while Count > 0 do
  begin
    ExportRect(P);
    Inc(P);
    Dec(Count);
  end;
end;

procedure TsgCADtoCGM.ExpImage(const R: TRect; AImage: TPersistent);
begin
  ExpImageUV(R.TopLeft, Point(R.Right,R.Top),R.BottomRight,
    Point(R.Left,R.Bottom), AImage);
end;

procedure TsgCADtoCGM.ExpImageUV(const APoint1, APoint2, APoint3,
  APoint4: TPoint; AImage: TPersistent);
const
  BPCs: array[TPixelFormat] of Byte = (0,1,4,8,4,4,8,8,0);
var
  I, W, vLen: Integer;
  P: Pointer;
  BM: TsgBMAdapter;
  Pal: array[Byte] of Integer;
  vByteBuffer: AnsiString;
  vBytePalett: AnsiString;

  function PutRGB(var vString: AnsiString; SrcPtr: Pointer; SrcBytesPerPixel, AWidth: Integer): Integer;
  var
    Index: Integer;
    InpData: PByte;
    OutData: Pointer;
  begin
    Result := Length(vString);
    SetLength(vString, Result + AWidth * 3 + Ord(Odd(AWidth)));
    OutData := Pointer(IntPtr(Pointer(vString)) + Result);
    InpData := PByte(SrcPtr);
    for Index := 0 to AWidth - 1 do
    begin
      {$IFDEF MSWINDOWS}
        PByteArray(OutData)^[0] := PByteArray(InpData)^[2];
        PByteArray(OutData)^[2] := PByteArray(InpData)^[0];
      {$ELSE}
        PByteArray(OutData)^[0] := PByteArray(InpData)^[0];
        PByteArray(OutData)^[2] := PByteArray(InpData)^[2];
      {$ENDIF}
      PByteArray(OutData)^[1] := PByteArray(InpData)^[1];
      Inc(InpData, SrcBytesPerPixel);
      Inc(PByte(OutData), 3);
    end;
    if Odd(AWidth) then
      PByte(OutData)^ := $F1;
  end;

  procedure PutBytes(Ptr: Pointer; Count: Integer);
  var
    P: PByte absolute Ptr;
    vCount: Integer;
  begin
    vCount := Count;
    while Count > 0 do begin
      vByteBuffer := vByteBuffer + AnsiChar(P^);
      Inc(P);
      Dec(Count);
    end;
    if ((vCount shr 1) shl 1) <> vCount then
      vByteBuffer := vByteBuffer + AnsiChar($F1);
  end;

  procedure PutWords(Ptr: Pointer; Count,Sh: Integer);
  var
    P: PWord absolute Ptr;
    vCount: Integer;
  begin
    vCount := Count;
    while Count > 0 do begin
      vByteBuffer := vByteBuffer + AnsiChar(P^ shr (Sh+5));
      vByteBuffer := vByteBuffer + AnsiChar(P^ shr Sh);
      vByteBuffer := vByteBuffer + AnsiChar(P^ shr 1);
      Inc(P);
      Dec(Count);
    end;
    if ((vCount shr 1) shl 1) <> vCount then
      vByteBuffer := vByteBuffer + AnsiChar($F1);
  end;

  function AdjustLen(var vString: AnsiString): Integer;
  begin
    Result := System.Length(vString);
    if (Result shr 1) shl 1 <> Result then
    begin
      Inc(Result);
      vString := vString + AnsiChar(0);
    end;
  end;

begin
  if (GetSizeGraphic(AImage).cx < 3) or (GetSizeGraphic(AImage).cy < 3) then Exit;  //?
  if IsGreatImage(AImage) then
    Exit;

  BM := TsgBMAdapter.Create;
  try
    BM.Assign(AImage);
    BM.HandleType := bmDIB;
    if BPCs[BM.PixelFormat] = 0 then
      Exit;

    //
    // Does not work correctly  ScanLine for pf32bit.
    //
    if BM.PixelFormat = pf32bit then
       BM.PixelFormat := pf24bit;

    if (BM.Width < 1000) and (BM.Height < 1000) then
      BM.ROP := SRCCOPY;	// if ???
    vByteBuffer := '';
    vBytePalett := '';
    W := BM.Width;
    for I:= 0 to BM.Height - 1 do begin
      P := BM.ScanLine[I];
      case BM.PixelFormat of
        pf1bit:         PutBytes(P, (W + 7) shr 3);
        pf4bit:         PutBytes(P, (W + 1) shr 1);
        pf8bit:         PutBytes(P, W);
        pf15bit:        PutWords(P, W, 6);
        pf16bit:        PutWords(P, W, 7);
        pf24bit:        PutRGB(vByteBuffer, P, 3, W);
        pf32bit:        PutRGB(vByteBuffer, P, 4, W);
      end;
    end;

    // Write metadata to file
    case BM.PixelFormat of
       pf1bit, pf4bit, pf8bit:
       begin
         SetColourSelectionMode($0000);
         I := BM.GetColorTable(@Pal);
         if I > 0 then
         begin
           vBytePalett := vBytePalett + AnsiChar(0);
           PutRGB(vBytePalett, @Pal, 4, I);
           TsgList(FBuffer).AddWord(cnstColourTable);
           vLen := AdjustLen(vBytePalett);
           if (vLen) < 31 then
           begin
             vBytePalett := vBytePalett +
               '123456789123456789123456789123456789123456789';
             vLen := AdjustLen(vBytePalett);
           end;
           TsgList(FBuffer).AddWord(vLen);
           TsgList(FBuffer).AddAnsiString(vBytePalett);
         end;
       end;
       pf15bit, pf16bit, pf24bit, pf32bit: SetColourSelectionMode($0001);
    end;

    TsgList(FBuffer).AddWord(cnstCellArray);
    vLen := AdjustLen(vByteBuffer);
    if (vLen  + 20) > cnstMaxWord then
      TsgList(FBuffer).AddWord($8000 + cnstMaxWord)
    else
      TsgList(FBuffer).AddWord(vLen + 20);
    TsgList(FBuffer).AddWord(APoint4.X);
    TsgList(FBuffer).AddWord(APoint4.Y);
    TsgList(FBuffer).AddWord(APoint2.X);
    TsgList(FBuffer).AddWord(APoint2.Y);
    TsgList(FBuffer).AddWord(APoint3.X);
    TsgList(FBuffer).AddWord(APoint3.Y);

    //nx,ny
    TsgList(FBuffer).AddWord(BM.Width);
    TsgList(FBuffer).AddWord(BM.Height);
    case BM.PixelFormat of
       pf1bit: TsgList(FBuffer).AddWord(1);
       pf4bit: TsgList(FBuffer).AddWord(4);
       pf8bit: TsgList(FBuffer).AddWord(8);
       pf15bit, pf16bit, pf24bit, pf32bit: TsgList(FBuffer).AddWord(8);
    end;
    TsgList(FBuffer).AddWord($0001);
    TsgList(FBuffer).AddAnsiString(vByteBuffer, 20);
    SetColourSelectionMode($0001);
  finally
    BM.Free;
  end;
end;

procedure TsgCADtoCGM.AddLineArttribetes;
begin
// TsgList(FBuffer).AddWord(cnstLineWidth);
//  TsgList(FBuffer).AddWord(FWeight);
  TsgList(FBuffer).AddWord(cnstLineType);
  TsgList(FBuffer).AddWord($0001);
  TsgList(FBuffer).AddWord(cnstLineColor);
  TsgList(FBuffer).AddLongWord(GetColor(FLineColor));
end;

procedure TsgCADtoCGM.AddPoints(Points: PPoint; Count: Integer; const AClosed: Boolean);

var
  vCnt: Integer;
  Pts: PInteger;

  procedure AddPts(APts: Pointer; ACnt: Integer; const APart: Integer = 0);
  var
    P: PInteger;
  begin
    if ACnt > (cnstMaxCntReal - Integer(AClosed)) then
      ACnt := (cnstMaxCntReal - Integer(AClosed));
    TsgList(FBuffer).AddWord(((ACnt + Integer(AClosed)) shl 2) or APart);
    P := PInteger(APts);
    while ACnt > 0 do
    begin
      TsgList(FBuffer).AddWord(P^); Inc(P);
      TsgList(FBuffer).AddWord(P^); Inc(P);
      Dec(ACnt);
    end;
    if AClosed then
    begin
      P := PInteger(APts);
      TsgList(FBuffer).AddWord(P^); Inc(P);
      TsgList(FBuffer).AddWord(P^);
    end;
  end;

begin
  vCnt := Count;
  if vCnt <= cnstMaxCntReal then
    AddPts(Points, vCnt)
  else
  begin
    Pts := PInteger(Points);
    while vCnt > 0 do
    begin
      if vCnt > cnstMaxCntReal then
      begin
        AddPts(Pts, cnstMaxCnt, $8000);
        Inc(Pts, cnstMaxCntReal shl 1);
        Dec(vCnt, cnstMaxCntReal);
      end
      else
      begin
        AddPts(Pts, vCnt);
        Break;
      end;
    end;
  end;
end;

procedure TsgCADtoCGM.ExportHeaderEnd;
begin
  TsgList(FBuffer).AddWord(cnstEndMetafile);
  ExpBuffer;
end;

procedure TsgCADtoCGM.ExportHeaderStart;
begin
  TsgList(FBuffer).AddWord(cnstBeginMetafile);
  TsgList(FBuffer).AddWord($0000);
  TsgList(FBuffer).AddWord(cnstMetafileVersion);
  TsgList(FBuffer).AddWord($0001);
  TsgList(FBuffer).AddWord(cnstMetafileDescription);
  TsgList(FBuffer).AddWord(cnstDescriptionLen);
  TsgList(FBuffer).AddString(cnstDescription);
  TsgList(FBuffer).AddWord(cnstVDCType);
  TsgList(FBuffer).AddWord($0000);
  TsgList(FBuffer).AddWord(cnstBeginPicture);
  SetColourSelectionMode($0001);
  TsgList(FBuffer).AddWord(cnstBackGround);
  TsgList(FBuffer).AddLongWord(GetColor(BackgroundColor));
  ExpBuffer;
end;

procedure TsgCADtoCGM.ExpPolyline(Points: PPoint; Count: Integer);
begin
  if not FIsModel then Exit;
  FHasLine := True;
  FHasFill := False;
  FLineColor := GetStrokeColor;
  FWeight := cnstDefWidth;
  ExportPolyL(Points, Count, False);
end;

procedure TsgCADtoCGM.ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer);
var
  PP: PPoint;
  PC: PInteger;
  C: Integer;
begin
  if not FIsModel then Exit;
  FHasLine := False;
  FHasFill := True;
  FFillColor := GetFillColor;
  FLineColor := FFillColor;
  FWeight := cnstDefWidth;
  PP := PPoint(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    ExportPolyG(PP, C, False);
    Inc(PP, C);
    Dec(Count);
  end;
end;

procedure TsgCADtoCGM.ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer);
var
  PP: PPoint;
  PC: PInteger;
  C: Integer;
begin
  if not FIsModel then Exit;
  FHasLine := True;
  FHasFill := False;
  FLineColor := GetStrokeColor;
  FFillColor := FLineColor;
  FWeight := cnstDefWidth;
  PP := PPoint(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    ExportPolyL(PP, C, False);
    Inc(PP, C);
    Dec(Count);
  end;
end;

procedure TsgCADtoCGM.ExpPolygon(Points: PPoint; Count: Integer);
begin
  if not FIsModel then Exit;
  FHasLine := False;
  FHasFill := True;
  FFillColor := GetFillColor;
  FLineColor := FFillColor;
  FWeight := cnstDefWidth;
  ExportPolyG(Points, Count, False);
end;

procedure TsgCADtoCGM.ExpSaveDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

function TsgCADtoCGM.Flipping: Boolean;
begin
  Result := True;
end;

function TsgCADtoCGM.IsFormatMultiPages: Boolean;
begin
  Result := False;
end;

procedure TsgCADtoCGM.ExpRestoreDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADtoCGM.PageEnd(N: Integer);
begin
  FIsModel := False;
  TsgList(FBuffer).AddWord(cnstEndPicture);
  ExpBuffer;
end;

procedure TsgCADtoCGM.PageStart(N: Integer);
var
  vBoundsRect: TRect;
  xMax: Integer;
begin
  FIsModel := N = 0;
  if FIsModel then
  begin
    vBoundsRect := GetExportRect;
    xMax := MaxI(vBoundsRect.Right - vBoundsRect.Left, Abs(vBoundsRect.Top - vBoundsRect.Bottom));
    if (xMax > 0) then
      XScale := cnstMaxSize/xMax;
  end;
  TsgList(FBuffer).AddWord(cnstVDCExt);
  vBoundsRect := GetExportRect;
  TsgList(FBuffer).AddWord(vBoundsRect.Left);
  TsgList(FBuffer).AddWord(vBoundsRect.Top);
  TsgList(FBuffer).AddWord(vBoundsRect.Right);
  TsgList(FBuffer).AddWord(vBoundsRect.Bottom);
  TsgList(FBuffer).AddWord(cnstBeginPictureBody);
  ExpBuffer;
end;

procedure TsgCADtoCGM.ExportPolyG(Points: PPoint; Count: Integer; const AClosed: Boolean);
begin
  AddEdgeArttribetes;
  TsgList(FBuffer).AddWord(cnstPolygon);
  AddPoints(Points, Count, AClosed);
  ExpBuffer;
end;

procedure TsgCADtoCGM.ExportPolyL(Points: PPoint; Count: Integer; const AClosed: Boolean);
begin
  AddLineArttribetes;
  TsgList(FBuffer).AddWord(cnstPolyLine);
  AddPoints(Points, Count, AClosed);
  ExpBuffer;
end;

procedure TsgCADtoCGM.ExportRect(const R: PRect);
begin
  AddEdgeArttribetes;
  TsgList(FBuffer).AddWord(cnstRectangle);
  TsgList(FBuffer).AddWord(R^.Left);
  TsgList(FBuffer).AddWord(R^.Top);
  TsgList(FBuffer).AddWord(R^.Right);
  TsgList(FBuffer).AddWord(R^.Bottom);
  ExpBuffer;
end;

procedure TsgCADtoCGM.SaveToStreamCustom(S: TStream);
{$IFDEF SG_DEBUG}
var
  vTickCount: Cardinal;
{$ENDIF}
begin
{$IFDEF SG_DEBUG}
  vTickCount := GetTickCount;
{$ENDIF}
  TsgList(FBuffer).Clear(False);
  FStream := S;
  try
    ExportHeaderStart;
    try
      inherited SaveToStreamCustom(FStream);
    finally
      ExportHeaderEnd;
    end;
  finally
    FStream := nil;
    TsgList(FBuffer).Clear(False);
  end;
{$IFDEF SG_DEBUG}
  MessageBox(0, PChar('TickCount= '+IntToStr(GetTickCount - vTickCount)), 'CGM EXPORT DEBUG MODE', MB_OK or MB_ICONINFORMATION);
{$ENDIF}
end;

procedure TsgCADtoCGM.SetColourSelectionMode(AMode: Word);
begin
  if (FOldColourSelectionMode <> AMode) and ((AMode and $8000) < 2) then
  begin
    TsgList(FBuffer).AddWord(cnstColorSelectionMode);
    TsgList(FBuffer).AddWord(AMode);
    FOldColourSelectionMode := AMode;
  end;
end;

{ TsgList }

procedure TsgList.AddLongWord(Value: LongWord);
var
  vWords: array[0..1] of Word absolute Value;
begin
  AddWord(vWords[1]);
  AddWord(vWords[0]);
end;

procedure TsgList.AddString(Value: string);
var
  vAnsi: AnsiString;
  I, vLen: Integer;
begin
  vAnsi := AnsiString(Value);
  vLen := System.Length(vAnsi);
  if (vLen shr 1) shl 1 <> vLen then
  begin
    vAnsi := vAnsi + AnsiChar(0);
    Inc(vLen);
  end;
  I := 1;
  while I < vLen do
  begin
    AddWord(Byte(vAnsi[I]) shl 8 or Byte(vAnsi[I + 1]));
    Inc(I, 2);
  end;
end;

procedure TsgList.AddAnsiString(Value: AnsiString; AUsedBlockMem: Integer = 0);
var
  vAnsi: AnsiString;
  I, vLen, vWriteWord: Integer;
begin
  vAnsi := AnsiString(Value);
  vLen := System.Length(vAnsi);
  if (vLen shr 1) shl 1 <> vLen then
  begin
    vAnsi := vAnsi + AnsiChar(0);
    Inc(vLen);
  end;
  vWriteWord := AUsedBlockMem;
  I := 1;
  while I < vLen do
  begin
    if (vWriteWord) = cnstMaxWord then
    begin
      if (vLen - I - 2) > cnstMaxWord  then
        AddWord($8000 + cnstMaxWord)
      else
        AddWord($0000 + vLen - I);
      vWriteWord := 0;
    end;
    AddWord(Byte(vAnsi[I]) shl 8 or Byte(vAnsi[I + 1]));
    Inc(I, 2);
    Inc(vWriteWord, 2);
  end;
end;

procedure TsgList.AddWord(Value: Word);
var
  vBytes: array [0..1] of Byte absolute Value;
  vByte: Byte;
begin
  vByte := vBytes[0];
  vBytes[0] := vBytes[1];
  vBytes[1] := vByte;
  if Count = 0 then
  begin
    Add(0);
    List[0] := Value;
  end
  else
  begin
    if IsEven then
    begin
      Add(0);
      List[Count - 1] := Value;
    end
    else
      List[Count - 1] := (LongWord(List[Count - 1])) or (Value  shl 16);
  end;
  Inc(FLength);
end;

procedure TsgList.Clear(ClearCapacity: Boolean{ = False});
begin
  FLength := 0;
  inherited Clear(ClearCapacity);
end;

procedure TsgList.FastClear;
begin
  Count := 0;
  FLength := 0;
end;

function TsgList.IsEven: Boolean;
begin
  Result := (FLength shr 1) shl 1 = FLength;
end;

procedure TsgList.SaveToStream(S: TStream);
begin
  S.Write(List[0], Length shl 1);
end;

end.

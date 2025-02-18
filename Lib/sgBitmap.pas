{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   Bitmap files TGrapic class               }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgBitmap;

interface

{$INCLUDE SGDXF.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF SGFPC}
  LCLIntf, LCLType, Types, IntfGraphics, GraphType, FPCanvas,
  {$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Types, FMX.Objects, FMX.Graphics, System.Types, sgFMXTypes, sgProxyGraphics,
  FMX.Surfaces, System.UITypes,
{$ELSE}
  Graphics,
{$ENDIF}
  sgConsts, Math;

type
  EsgBitmapMaxSizeException = class(Exception);

  TsgBitmapImage = class;

  { TsgBitmap }

  TsgBitmap = class(TGraphic)
  private
    FHeight: Word;
    FImg: TsgBitmapImage;
    FOnRotate: TNotifyEvent;
    FPixelFormat: TPixelFormat;
    FWidth: Word;
    function GetROP: Byte;
    function GetScanLine(ARow: Integer): Pointer;
    function InfoSize: Integer;
    procedure NewImage;
    procedure SetPixelFormat(AValue: TPixelFormat);
    procedure SetROP(AValue: Byte);
    procedure Update;
    function GetRowData: Pointer;
    procedure ColorsByIndexAndRange(AColor: Pointer;
     const AIndex: Integer; const ARange: Integer; const IsGet: Boolean);
  protected
    FROP: Integer;
    //procedure AssignBM(ABitmap: TBitmap);
    procedure AssignTo(ADest: TPersistent); override;
    procedure CopyBitmapImage;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    procedure DoExceptionNotImplemented;
    procedure NewBitmapImage;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    {$IFDEF SGFPC}
    function GetTransparent: Boolean; override;
    {$ENDIF}
    procedure SetHeight(AValue: Integer); override;
    procedure SetWidth(AValue: Integer); override;
    function GetPalette: HPALETTE; override;
    procedure SetPalette(Value: HPALETTE); override;
    {$IFDEF SGFPC}
    procedure SetTransparent(Value: Boolean); override;
    {$ENDIF}
{$IFDEF SG_BLOCK_PREVIEW}
    procedure LoadFromBitmapInfo(AInfo: PBitmapInfo);
{$ENDIF}
    procedure WriteImage(Stream: TStream);
{$IFDEF SG_FIREMONKEY}
    procedure FillMap(const AMap: TBitmapData);
{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure AssignFromBMP(ABitmap: TBitmap);
    procedure AssignToBMP(ABitmap: TBitmap);
    procedure Flip;
    function GetColors(AColors: Pointer): Integer;
    procedure GetColorsByIndexAndRange(AColor: Pointer; const AIndex: Integer; const ARange: Integer);
    procedure LoadFromClipboardFormat(AFmt: Word; AData: THandle; APal: HPALETTE); {$IFNDEF SGFPC} override;{$ENDIF}
    procedure LoadFromStream(AStream: TStream); override;
    procedure Rotate(const AAngle: Integer);
    function RowSize: Integer;
    procedure SaveToClipboardFormat(var AFmt: Word; var AData: THandle; var APal: HPALETTE); {$IFNDEF SGFPC} override;{$ENDIF}
    procedure SaveToFile(const FileName: String); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SetColors(AColors: Pointer);
    procedure SetColorsByIndexAndRange(AColor: Pointer; const AIndex: Integer; const ARange: Integer);
    procedure SetSize(AWidth, AHeight: Integer); {$IFDEF SGDEL_2006}override{$ELSE}virtual{$ENDIF};
    property ImageData: TsgBitmapImage read FImg;
    property OnRotate: TNotifyEvent read FOnRotate write FOnRotate;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat;
    property ROP: Byte read GetROP write SetROP;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property RowData: Pointer read GetRowData;
  end;

  TsgBitmapImage = class(TSharedImage)
  protected
    FData: Pointer;
    FInfo: PBitmapInfo;
    procedure FreeHandle; override;
  public
    procedure Assign(ASource: TsgBitmapImage);
    property Data: Pointer read FData;
    property Info: PBitmapInfo read FInfo;
  end;

  { TsgBMAdapter }

  TsgBMAdapter = class(TGraphic)
  private
    FOwned: TPersistent;
    FSource: TPersistent;
    FBitmap: TBitmap;
    function BM: TBitmap;
    procedure ForceBM;
    procedure ForceSG;
    function GetCanvas: TCanvas;
    function GetHandleType: TBitmapHandleType;
    function GetPixelFormat: TPixelFormat;
    function GetROP: Integer;
    function GetScanLine(ARow: Integer): Pointer;
    function GetSource: TPersistent;
    function GetBitmap: TBitmap;
    procedure SetHandleType(AValue: TBitmapHandleType);
    procedure SetOwned(AValue: TPersistent);
    procedure SetPixelFormat(AValue: TPixelFormat);
    procedure SetROP(AValue: Integer);
    procedure SetSource(AValue: TPersistent);
    function SG: TsgBitmap;
  protected
    procedure AssignTo(ADest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    {$IFDEF SGFPC}
    function GetTransparent: Boolean; override;
    {$ENDIF}
    procedure SetHeight(AValue: Integer); override;
    procedure SetWidth(AValue: Integer); override;
    function GetPalette: HPALETTE; override;
    procedure SetPalette(Value: HPALETTE); override;
    {$IFDEF SGFPC}
    procedure SetTransparent(Value: Boolean); override;
    {$ENDIF}
    function IsBM: Boolean;
    function IsSG: Boolean;
    property Owned: TPersistent read FOwned write SetOwned;
    property Source: TPersistent read GetSource write SetSource;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASrc: TPersistent); override;
    function GetColorTable(APoint: Pointer): Integer;
    procedure LoadFromClipboardFormat(AFmt:{$IFDEF SGFPC}TClipboardFormat{$ELSE}Word; AData: THandle;
      APal: HPALETTE{$ENDIF}); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Rotate(AAngle: Integer);
    function IsRGB: Boolean;
//{$IFNDEF SG_FIREMONKEY}
    procedure SaveToClipboardFormat({$IFNDEF SGFPC}var {$ENDIF} AFmt:{$IFDEF SGFPC}TClipboardFormat{$ELSE}Word;
      var AData: THandle; var APal: HPALETTE{$ENDIF}); override;
//{$ENDIF}
    procedure SaveToStream(AStream: TStream); override;
    property Bitmap: TBitmap read GetBitmap;
    property Canvas: TCanvas read GetCanvas;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property ROP: Integer read GetROP write SetROP;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
  end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;

implementation

uses
  sgFunction
{$IFNDEF SG_NON_WIN_PLATFORM}
  , ActiveX
{$ENDIF}
  ;

{$IFDEF SGFPC}
  {$DEFINE USE_STD_MM}
{$ELSE}
  {$IFDEF SGDEL_2006}
    {$DEFINE USE_STD_MM}
  {$ENDIF}
{$ENDIF}

type
  TsgMapAccess = class(TsgMap);
{$IFDEF SGFPC}
  TRasterImageAccess = class(TRasterImage);
{$ENDIF}

const

  ROPs: array[Byte] of Word =
  ($0042, $0289, $0C89, $00AA, $0C88, $00A9, $0865, $02C5,
   $0F08, $0245, $0329, $0B2A, $0324, $0B25, $08A5, $0001,
   $0C85, $00A6, $0868, $02C8, $0869, $02C9, $5CCA, $1D54,
   $0D59, $1CC8, $06C5, $0768, $06CA, $0766, $01A5, $0385,
   $0F09, $0248, $0326, $0B24, $0D55, $1CC5, $06C8, $1868,
   $0369, $16CA, $0CC9, $1D58, $0784, $060A, $064A, $0E2A,
   $032A, $0B28, $0688, $0008, $06C4, $1864, $01A8, $0388,
   $078A, $0604, $0644, $0E24, $004A, $18A4, $1B24, $00EA,
   $0F0A, $0249, $0D5D, $1CC4, $0328, $0B29, $06C6, $076A,
   $0368, $16C5, $0789, $0605, $0CC8, $1954, $0645, $0E25,
   $0325, $0B26, $06C9, $0764, $08A9, $0009, $01A9, $0389,
   $0785, $0609, $0049, $18A9, $0649, $0E29, $1B29, $00E9,
   $0365, $16C6, $0786, $0608, $0788, $0606, $0046, $18A8,
   $58A6, $0145, $01E9, $178A, $01E8, $1785, $1E28, $0C65,
   $0CC5, $1D5C, $0648, $0E28, $0646, $0E26, $1B28, $00E6,
   $01E5, $1786, $1E29, $0C68, $1E24, $0C69, $0955, $03C9,
   $03E9, $0975, $0C49, $1E04, $0C48, $1E05, $17A6, $01C5,
   $00C6, $1B08, $0E06, $0666, $0E08, $0668, $1D7C, $0CE5,
   $0C45, $1E08, $17A9, $01C4, $17AA, $01C9, $0169, $588A,
   $1888, $0066, $0709, $07A8, $0704, $07A6, $16E6, $0345,
   $00C9, $1B05, $0E09, $0669, $1885, $0065, $0706, $07A5,
   $03A9, $0189, $0029, $0889, $0744, $06E9, $0B06, $0229,
   $0E05, $0665, $1974, $0CE8, $070A, $07A9, $16E9, $0348,
   $074A, $06E6, $0B09, $0226, $1CE4, $0D7D, $0269, $08C9,
   $00CA, $1B04, $1884, $006A, $0E04, $0664, $0708, $07AA,
   $03A8, $0184, $0749, $06E4, $0020, $0888, $0B08, $0224,
   $0E0A, $066A, $0705, $07A4, $1D78, $0CE9, $16EA, $0349,
   $0745, $06E8, $1CE9, $0D75, $0B04, $0228, $0268, $08C8,
   $03A5, $0185, $0746, $06EA, $0748, $06E5, $1CE8, $0D79,
   $1D74, $5CE6, $02E9, $0849, $02E8, $0848, $0086, $0A08,
   $0021, $0885, $0B05, $022A, $0B0A, $0225, $0265, $08C5,
   $02E5, $0845, $0089, $0A09, $008A, $0A0A, $02A9, $0062);

  cnstBPPs: array[TPixelFormat] of Byte = (0,1,4,8,16,16,24,32,32);

type
  TBitmapAccess = class(TBitmap);

{$IFNDEF SG_FIREMONKEY}
  TRotateData = record
    Dst: PByte;
    Src: PByte;
    ISrc: Integer;
    DSrc: Integer;
    Count: Integer;
  end;

  TRowProc = procedure(var Data: TRotateData; PrepProc: Pointer);

{assembler inserts}

procedure PrepareRow;
{$IFDEF SG_CPUX64}
asm
  MOV RAX, RCX
	MOVSXD	RCX,[RAX].TRotateData.Count
	MOVSXD  RDX,[RAX].TRotateData.DSrc
	MOVSXD	RSI,[RAX].TRotateData.ISrc
	MOV	RDI,[RAX].TRotateData.Dst
	MOV	RAX,[RAX].TRotateData.Src
end;
{$ELSE}
asm
	MOV	ECX,[EAX].TRotateData.Count;
	MOV	EDX,[EAX].TRotateData.DSrc;
	MOV	ESI,[EAX].TRotateData.ISrc;
	MOV	EDI,[EAX].TRotateData.Dst;
	MOV	EAX,[EAX].TRotateData.Src;
end;
{$ENDIF}

procedure Row01(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RBX
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	MOV	RBX,$8080
	PUSH	RCX
	MOV	RCX,RSI
	AND	CL,7
	SHR	BL,CL
	POP	RCX
	SHR	RSI,3
	ADD	RSI,RAX
@@1:	TEST	[RSI],BL
	JZ	@@2
	OR	[RDI],BH
@@2:	ROR	BH,1
	ADC	RDI,0
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
	POP	RBX
{$ELSE}
	PUSH	EBX
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	MOV	EBX,$8080
	PUSH	ECX
	MOV	ECX,ESI
	AND	CL,7
	SHR	BL,CL
	POP	ECX
	SHR	ESI,3
	ADD	ESI,EAX
@@1:	TEST	[ESI],BL
	JZ	@@2
	OR	[EDI],BH
@@2:	ROR	BH,1
	ADC	EDI,0
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
	POP	EBX
{$ENDIF}
end;

procedure Row01H(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RBX
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	MOV	RBX,$8080
	PUSH	RCX
	MOV	RCX,RSI
	AND	CL,7
	SHR	BL,CL
	POP	RCX
	SHR	RSI,3
	ADD	RSI,RAX
@@1:	TEST	[RSI],BL
	JZ	@@2
	OR	[RDI],BH
@@2:	ROR	BH,1
	ADC	RDI,0
	ROL	BL,1
	SBB	RSI,0
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
	POP	RBX
{$ELSE}
	PUSH	EBX
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	MOV	EBX,$8080
	PUSH	ECX
	MOV	ECX,ESI
	AND	CL,7
	SHR	BL,CL
	POP	ECX
	SHR	ESI,3
	ADD	ESI,EAX
@@1:	TEST	[ESI],BL
	JZ	@@2
	OR	[EDI],BH
@@2:	ROR	BH,1
	ADC	EDI,0
	ROL	BL,1
	SBB	ESI,0
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
	POP	EBX
{$ENDIF}
end;

procedure Row04(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RBX
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	MOV	RBX,$F0F0
	SHR	RSI,1
	JNC	@@1
	NOT	BL
@@1:	ADD	RSI,RAX
@@2:	MOV	AL,[RSI]
	AND	AL,BL
	MOV	AH,AL
	ROR	AH,4
	OR	AL,AH
	AND	AL,BH
	OR	[RDI],AL
	ROR	BH,4
	ADC	RDI,0
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@2
	POP	RDI
	POP	RSI
	POP	RBX
{$ELSE}
	PUSH	EBX
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	MOV	EBX,$F0F0
	SHR	ESI,1
	JNC	@@1
	NOT	BL
@@1:	ADD	ESI,EAX
@@2:	MOV	AL,[ESI]
	AND	AL,BL
	MOV	AH,AL
	ROR	AH,4
	OR	AL,AH
	AND	AL,BH
	OR	[EDI],AL
	ROR	BH,4
	ADC	EDI,0
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@2
	POP	EDI
	POP	ESI
	POP	EBX
{$ENDIF}
end;

procedure Row04H(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RBX
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	MOV	RBX,$F0F0
	SHR	RSI,1
	JNC	@@1
	NOT	BL
@@1:	ADD	RSI,RAX
@@2:	MOV	AL,[RSI]
	AND	AL,BL
	MOV	AH,AL
	ROR	AH,4
	OR	AL,AH
	AND	AL,BH
	OR	[RDI],AL
	ROR	BH,4
	ADC	RDI,0
	ROL	BL,4
	SBB	RSI,0
	DEC	RCX
	JNZ	@@2
	POP	RDI
	POP	RSI
	POP	RBX
{$ELSE}
	PUSH	EBX
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	MOV	EBX,$F0F0
	SHR	ESI,1
	JNC	@@1
	NOT	BL
@@1:	ADD	ESI,EAX
@@2:	MOV	AL,[ESI]
	AND	AL,BL
	MOV	AH,AL
	ROR	AH,4
	OR	AL,AH
	AND	AL,BH
	OR	[EDI],AL
	ROR	BH,4
	ADC	EDI,0
	ROL	BL,4
	SBB	ESI,0
	DEC	ECX
	JNZ	@@2
	POP	EDI
	POP	ESI
	POP	EBX
{$ENDIF}
end;

procedure Row08(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	ADD	RSI,RAX
@@1:	MOV	AL,[RSI]
	MOV	[RDI],AL
	INC	RDI
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
{$ELSE}
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	ADD	ESI,EAX
@@1:	MOV	AL,[ESI]
	MOV	[EDI],AL
	INC	EDI
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
{$ENDIF}
end;

procedure Row16(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	LEA	RSI,[RAX][RSI*2]
@@1:	MOV	AX,[RSI]
	MOV	[RDI],AX
	ADD	RDI,2
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
{$ELSE}
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	LEA	ESI,[EAX][ESI*2]
@@1:	MOV	AX,[ESI]
	MOV	[EDI],AX
	ADD	EDI,2
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
{$ENDIF}
end;

procedure Row24(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	LEA	RSI,[ESI][ESI*2]
  ADD RSI,RAX
@@1:	MOV	AX,[RSI]
	MOV	[RDI],AX
	MOV	AL,[RSI+2]
	MOV	[RDI+2],AL
	ADD	RDI,3
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
{$ELSE}
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	LEA	ESI,[ESI][ESI * 2]
	ADD	ESI,EAX
@@1:	MOV	AX,[ESI]
	MOV	[EDI],AX
        MOV	AL,[ESI + 2]
	MOV	[EDI + 2],AL
	ADD	EDI,3
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
{$ENDIF}
end;

procedure Row32(var Data: TRotateData; PrepProc: Pointer);
asm
{$IFDEF SG_CPUX64}
	PUSH	RSI
	PUSH	RDI
	CALL	RDX
	LEA	RSI,[RAX][RSI*4]
@@1:	MOV	EAX,[RSI]
	MOV	[RDI],EAX
	ADD	RDI,4
	ADD	RSI,RDX
	DEC	RCX
	JNZ	@@1
	POP	RDI
	POP	RSI
{$ELSE}
	PUSH	ESI
	PUSH	EDI
	CALL	EDX
	LEA	ESI,[EAX][ESI*4]
@@1:	MOV	EAX,[ESI]
	MOV	[EDI],EAX
	ADD	EDI,4
	ADD	ESI,EDX
	DEC	ECX
	JNZ	@@1
	POP	EDI
	POP	ESI
{$ENDIF}
end;
{$ENDIF}

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

procedure SGGetMem(var P; Size: TsgNativeInt);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF USE_STD_MM}
  GetMem(Pointer(P), Size);
{$ELSE}
  Pointer(P) := GlobalAllocPtr(HeapAllocFlags, Size);
{$ENDIF}
end;

procedure SGReallocMem(var P; Size: TsgNativeInt);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF USE_STD_MM}
  ReallocMem(Pointer(P), Size);
{$ELSE}
  Pointer(P) := GlobalReAllocPtr(Pointer(P), Size, HeapAllocFlags);
{$ENDIF}
end;

procedure SGFreeMem(P: Pointer);{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF USE_STD_MM}
  FreeMem(P);
{$ELSE}
  GlobalFreePtr(P);
{$ENDIF}
end;

function SGAllocMem(Size: TsgNativeInt): Pointer;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
{$IFDEF USE_STD_MM}
  Result := AllocMem(Size);
{$ELSE}
  Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size);
{$ENDIF}
end;


function GetPixelFormat(AInfo: PBitmapInfo): TPixelFormat;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := pfCustom;
  case AInfo^.bmiHeader.biBitCount of
    1:  Result := pf1Bit;
    4:  Result := pf4Bit;
    8:  Result := pf8Bit;
    16:
      case AInfo^.bmiHeader.biCompression of
        BI_RGB : Result := pf15Bit;
        BI_BITFIELDS: if PWord(@AInfo^.bmiColors[0].rgbRed)^ = $7E0 then Result := pf16Bit;
      end;
    24: Result := pf24Bit;
    32: if AInfo^.bmiHeader.biCompression = BI_RGB then Result := pf32Bit;
  end;
{$ELSE}
  Result := TPixelFormat.pf32bit;
{$ENDIF}
end;


{procedure ReadBytes(var PBit: TsgInt64; var Dest; Count: Integer); register;
var
  Src: PByte;
  Next: WordRec;
  Shift: Byte;
  P: PByte;
begin
  Integer(Src) := PBit.Lo shr 3 or PBit.Hi shl 29;
  Shift := PBit.Lo and 7;
  Inc2(PBit, Count shl 3);
  P := @Dest;
  while Count > 0 do
  begin
    Next.Hi := Src^;
    Inc(Src);
    Next.Lo := Src^;
    Word(Next) := Word(Next) shl Shift;
    P^ := Next.Hi;
    Inc(P);
    Dec(Count);
  end;
end;}
//procedure ReadBytes(var PBit: TsgInt64; var Dest; Count: Integer); register;
//asm
//{$IFDEF SG_CPUX64}
//{$ELSE}
//    PUSH    EBX
//    PUSH    ESI
//    PUSH    EDI
//    MOV     EBX,ECX
//    MOV     EDI,EDX
//    MOV     EDX,EAX
//    MOV     EAX,[EDX]
//    MOV	    ECX,4[EDX]
//    MOV     ESI,EAX
//    SHRD    ESI,ECX,3
//    AND     AL,7
//    MOV     CL,AL
//    MOV     EAX,EBX
//    SHL     EAX,3
//    ADD     [EDX],EAX
//    ADC	    DWORD PTR 4[EDX],0
//@@1:        MOV     AX,[ESI]
//    XCHG    AL,AH
//    SHL     AX,CL
//    MOV     [EDI],AH
//    INC     ESI
//    INC     EDI
//    DEC     EBX
//    JNZ     @@1
//    POP     EDI
//    POP     ESI
//    POP     EBX
//{$ENDIF}
//end;
//
//procedure Inc2(var PBit: TsgInt64; Value: Integer);
//asm
//	ADD	[EAX],EDX
//	ADC	DWORD PTR 4[EAX],0
//end;

{ TsgBitmap }

{private}

procedure TsgBitmap.AssignFromBMP(ABitmap: TBitmap);
{$IFDEF SGFPC}
var
  I,J: Integer;
  vImage: TLazIntfImage;
  vDesc: TRawImageDescription;
  vHeaderSize, vImageSize: Cardinal;
  vMap: TsgMap;
  vColors: array[Byte] of Integer;
begin
  vImage := ABitmap.CreateIntfImage;
  try
    FWidth := vImage.Width;
    FHeight := vImage.Height;
    FPixelFormat := pf32bit;// ABitmap.PixelFormat
    FROP := SRCCOPY;
    vDesc := vImage.DataDescription;
    vDesc.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    vHeaderSize := SizeOf(TBitmapInfoHeader);
    if vImage.UsePalette then
      Inc(vHeaderSize, vImage.Palette.Count * SizeOf(TRGBQuad));
    vImageSize := vImage.Height * vDesc.BytesPerLine;
    if FImg.RefCount > 1 then
    begin
      FImg.Release;
      NewImage;
    end;
    FImg.FreeHandle;
    if not Empty then
    begin
      FImg.FInfo := SGAllocMem(vHeaderSize);
      FImg.FInfo^.bmiHeader.biSize := SizeOf(FImg.FInfo^.bmiHeader);
      FImg.FInfo^.bmiHeader.biWidth := FWidth;
      FImg.FInfo^.bmiHeader.biHeight := FHeight;
      FImg.FInfo^.bmiHeader.biPlanes := 1;
      FImg.FInfo^.bmiHeader.biBitCount := cnstBPPs[FPixelFormat];
      SGGetMem(FImg.FData, vImageSize);
    end;
    if vImage.UsePalette then
      for I := 0 to vImage.Palette.Count - 1 do
        TColor(FImg.FInfo^.bmiColors[I]) := FPColorToTColor(vImage.Palette.Color[I]);
    if vDesc.IsEqual(vImage.DataDescription) then
    begin
      for I := 0 to FHeight - 1 do
        System.Move(vImage.GetDataLineStart(I)^, ScanLine[I]^, RowSize);
    end
    else
    begin
      vMap := TsgMap.Wrap(FWidth, FHeight, FPixelFormat, FImg.Data, @vColors[0], GetColors(@vColors[0]));
      try
        for J := 0 to FHeight - 1 do
          for I := 0 to FWidth - 1 do
            vMap.Pixels[I, J] := vImage.TColors[I, J];
      finally
        TsgMap.Unwrap(vMap);
      end;
    end;
    Changed(Self);
  finally
    vImage.Free;
  end;
end;
{$ELSE}
{$IFNDEF SG_FIREMONKEY}
var
  I: Integer;
  vHeaderSize, vImageSize: Cardinal;
  DC: HDC;
  vBitmapHandle: THandle;
begin
  ABitmap.HandleType := bmDIB;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FPixelFormat := ABitmap.PixelFormat;
  FROP := SRCCOPY;
  GetDIBSizes(ABitmap.Handle, vHeaderSize, vImageSize);
  if FImg.RefCount > 1 then
  begin
    FImg.Release;
    NewImage;
  end;
  FImg.FreeHandle;
  if not Empty then
  begin
    FImg.FInfo := SGAllocMem(vHeaderSize);
    FImg.FInfo^.bmiHeader.biSize := SizeOf(FImg.FInfo^.bmiHeader);
    FImg.FInfo^.bmiHeader.biWidth := FWidth;
    FImg.FInfo^.bmiHeader.biHeight := FHeight;
    FImg.FInfo^.bmiHeader.biPlanes := 1;
    FImg.FInfo^.bmiHeader.biBitCount := cnstBPPs[FPixelFormat];
    SGGetMem(FImg.FData, vImageSize);
  end;
  if PixelFormat <= pf8bit then
  begin
    DC := CreateCompatibleDC(0);
    vBitmapHandle := SelectObject(DC, ABitmap.Handle);
    I := GetDIBColorTable(DC, 0, 1 shl cnstBPPs[FPixelFormat], FImg.Info^.bmiColors[0]);
    SelectObject(DC, vBitmapHandle);
    DeleteDC(DC);
    if (I = 0) and (ABitmap.Palette <> 0) then
    begin
      I := GetPaletteEntries(ABitmap.Palette, 0, 1 shl cnstBPPs[FPixelFormat], FImg.Info^.bmiColors[0]);
      if I > 0 then
        repeat
          Dec(I);
          SwapByte(FImg.Info^.bmiColors[I].rgbRed, FImg.Info^.bmiColors[I].rgbBlue);
        until I <= 0;
    end;
  end;
  for I := 0 to FHeight - 1 do
    System.Move(ABitmap.ScanLine[I]^, Scanline[I]^, RowSize);
  Changed(Self);
end;
{$ELSE}
var
  I: Integer;
  vMap: TBitmapData;
begin
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FPixelFormat := pf32bit;
  FROP := SRCCOPY;
  Update;
  if ABitmap.Map(TMapAccess.Read, vMap) then
  begin
    try
      for I := 0 to FHeight - 1 do
        System.Move(vMap.GetScanline(I)^, ScanLine[I]^, RowSize);
    finally
      ABitmap.Unmap(vMap);
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TsgBitmap.AssignToBMP(ABitmap: TBitmap);
{$IFDEF SG_FIREMONKEY}
var
  vBitData: TBitmapData;
  vMaxBitmapSize: Integer;
  vBitmapSurface: TBitmapSurface;
  Y: Integer;
begin
  vMaxBitmapSize := Round(ABitmap.Canvas.GetAttribute(TCanvasAttribute.MaxBitmapSize));
  if (FWidth < vMaxBitmapSize) and (FHeight < vMaxBitmapSize) then
  begin
    ABitmap.SetSize(FWidth, FHeight);
    ABitmap.Map(TMapAccess.ReadWrite, vBitData);
    try
      FillMap(vBitData);
    finally
      ABitmap.Unmap(vBitData);
    end;
  end
  else
  begin
    vBitmapSurface := TBitmapSurface.Create;
    try
      vBitmapSurface.SetSize(FWidth, FHeight);
      for Y := 0 to FHeight - 1 do
        System.Move(GetScanLine(Y)^, vBitmapSurface.Scanline[Y]^, vBitmapSurface.Pitch);
      ABitmap.Assign(vBitmapSurface);
    finally
      vBitmapSurface.Free;
    end;
//  raise EsgBitmapMaxSizeException.Create('Set bitmap max size');
  end;
end;
{$ELSE}
{$IFDEF SGFPC}
var
  X, Y: Integer;
  vImage: TLazIntfImage;
  vMap: TsgMap;
  vColors: array[Byte] of Integer;
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.SetSize(FWidth, FHeight);
  vImage := ABitmap.CreateIntfImage;
  try
    vMap := TsgMap.Wrap(FWidth, FHeight, FPixelFormat, FImg.Data, @vColors[0], GetColors(@vColors[0]));
    try
      for Y := 0 to FHeight - 1 do
        for X := 0 to FWidth - 1 do
          vImage.TColors[X, Y] := vMap.Pixels[X, Y];
    finally
      TsgMap.Unwrap(vMap);
    end;
    ABitmap.LoadFromIntfImage(vImage);
  finally
    vImage.Free;
  end;
end;
{$ELSE}
var
  X, Y: Integer;
  vMap, vMapDest: TsgMap;
  vColors: array[Byte] of Integer;
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.Width := FWidth;
  ABitmap.Height := FHeight;
  vMapDest := TsgMap.Wrap(ABitmap);
  try
    vMap := TsgMap.Wrap(FWidth, FHeight, FPixelFormat, FImg.Data, @vColors[0], GetColors(@vColors[0]));
    try
      for Y := 0 to FHeight - 1 do
        for X := 0 to FWidth - 1 do
          vMapDest.Pixels[X, Y] := vMap.Pixels[X, Y];
    finally
      TsgMap.Unwrap(vMap);
    end;
  finally
    TsgMap.Unwrap(vMapDest);
  end;
end;
{$ENDIF}
{$ENDIF}


function TsgBitmap.GetROP: Byte;
begin
  Result := LongRec(FROP).Hi;
end;

function TsgBitmap.GetRowData: Pointer;
begin
  Result := FImg.FData;
end;

procedure TsgBitmap.ColorsByIndexAndRange(AColor: Pointer;
     const AIndex: Integer; const ARange: Integer; const IsGet: Boolean);
var
  vPByte: PByte;
  vLenPal: Integer;
begin
  if (FImg.FInfo = nil) or not (FPixelFormat in [pf1bit..pf8bit]) then
    Exit;
  vLenPal := 1 shl FImg.FInfo.bmiHeader.biBitCount;
  if (AIndex > vLenPal) or (AIndex + ARange > vLenPal) then
    Exit;
  vPByte := PByte(FImg.FInfo);
  Inc(vPByte, SizeOf(TBitmapInfoHeader));
  Inc(vPByte, AIndex shl 2);
  if IsGet then
    Move(vPByte^, AColor^, ARange shl 2)
  else
    Move(AColor^, vPByte^, ARange shl 2)
end;

function TsgBitmap.GetScanLine(ARow: Integer): Pointer;
begin
  Result := FImg.FData;
  if Result <> nil then
  begin
    ARow := (FHeight - 1) - ARow;
    Inc(PByte(Result), RowSize * ARow);
  end;
end;

function TsgBitmap.InfoSize: Integer;
begin
  if PixelFormat <= pf8bit then
    if Assigned(FImg.FInfo) then
      Result := 1 shl (FImg.FInfo.bmiHeader.biBitCount + 2)
    else
      Result := 1 shl (cnstBPPs[FPixelFormat] + 2)
  else
  begin
    if (Assigned(FImg.FInfo) and (FImg.FInfo.bmiHeader.biCompression = BI_BITFIELDS)) or (FPixelFormat = pfCustom) then
      Result := 12
    else
      Result := 0;
  end;
  Inc(Result, SizeOf(TBitmapInfoHeader));
end;

procedure TsgBitmap.CopyBitmapImage;
var
  vImgOrg: TsgBitmapImage;
begin
  vImgOrg := FImg;
  try
    NewImage;
    NewBitmapImage;
    CopyMemory(FImg.Info, vImgOrg.FInfo, InfoSize);
    CopyMemory(FImg.FData, vImgOrg.FData, FHeight * RowSize);
  finally
    vImgOrg.Release;
  end;
end;

procedure TsgBitmap.NewBitmapImage;
begin
  FImg.FInfo := SGAllocMem(InfoSize);
  with FImg.FInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := FWidth;
    biHeight := FHeight;
    biPlanes := 1;
    biBitCount := cnstBPPs[FPixelFormat];
  end;
  SGGetMem(FImg.FData, FHeight*RowSize);
end;

procedure TsgBitmap.NewImage;
begin
  FImg := TsgBitmapImage.Create;
  FImg.Reference;
end;

{$IFDEF SG_FIREMONKEY}
procedure TsgBitmap.FillMap(const AMap: TBitmapData);
var
  X, Y, vColors: Integer;
  vPal: array[Byte] of TColor;
  vSGMap: TsgMapAccess;
begin
  vSGMap := TsgMapAccess(TsgMap.Create(FWidth, FHeight, FPixelFormat));
  try
    vColors := GetColors(@vPal);
    System.Move(FImg.FData^, vSGMap.Data^, FHeight * RowSize);
    if vColors > 0 then
    begin
      SetLength(vSGMap.Pal, vColors);
      System.Move(vPal[0], vSGMap.Pal[0], vColors * SizeOf(Integer));
    end;
    for Y := 0 to FHeight - 1 do
      for X := 0 to FWidth - 1 do
        AMap.SetPixel(X, Y, vSGMap.Pixels[X, Y].AsBGRA);
  finally
    vSGMap.Free;
  end;
end;
{$ENDIF}

procedure TsgBitmap.SetPalette(Value: HPALETTE);
begin

end;

procedure TsgBitmap.SetPixelFormat(AValue: TPixelFormat);
begin
  if AValue <> FPixelFormat then
  begin
    FPixelFormat := AValue;
    Update;
  end;
end;

procedure TsgBitmap.SetROP(AValue: Byte);
begin
  FROP := AValue shl 16 + ROPs[AValue];
end;

procedure TsgBitmap.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    Update;
  end;
end;

procedure TsgBitmap.Update;
begin
  if FImg.RefCount > 1 then
  begin
    FImg.Release;
    NewImage;
  end;
  FImg.FreeHandle;
  if not Empty then
    NewBitmapImage;
  Changed(Self);
end;

{protected}

procedure TsgBitmap.AssignTo(ADest: TPersistent);
begin
  if ADest is TBitmap then
  begin
    {$IFNDEF SG_FIREMONKEY}
    TBitmap(ADest).PixelFormat := PixelFormat;
    {$ENDIF}
    AssignToBitmap(Self, TBitmap(ADest))
  end
  else
    inherited AssignTo(ADest);
end;

procedure TsgBitmap.Draw(ACanvas: TCanvas; const ARect: TRect);
{$IFNDEF SG_FIREMONKEY}
var
  vOldGdiObject: HGDIOBJ;
  vRect: TRect;
{$IFNDEF SGFPC}
  vX1, vY1, vX2, vY2: Integer;
{$ENDIF}
  vWidth, vHeigth, vROP: Integer;
//  vStretchBltMode: Integer;
  vDC: HDC;
{$IFDEF SGFPC}
  vBitmap: TBitmap;
{$ENDIF}

  function GetNormalRect(const R: TRect): TRect;
  begin
    Result := R;
    NormRect(Result);
  end;

begin
  if not Empty then
  begin
    vDC := ACanvas.Handle;
    {$IFNDEF SG_NON_WIN_PLATFORM}
    if {$IFNDEF SGFPC} (ACanvas is TMetaFileCanvas) or {$ENDIF} (GetObjectType(vDC) in [OBJ_METADC, OBJ_ENHMETADC]) then
      vRect := ARect
    else
    {$ENDIF}
      if {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}GetClipBox(vDC, {$IFNDEF SG_NON_WIN_PLATFORM}vRect{$ELSE}@vRect{$ENDIF}) <> NULLREGION then
        IntersectRect(vRect, vRect, GetNormalRect(ARect));
    if not IsRectEmpty(vRect) then
    begin
      vROP := FROP;
      if Transparent then
        FROP := SRCAND;
      try
        vWidth := ARect.Right - ARect.Left;
        vHeigth := ARect.Bottom - ARect.Top;
{$IFNDEF SGFPC}
        vX1 := MulDiv(vRect.Left - ARect.Left, FWidth, vWidth);
        vY1 := MulDiv(vRect.Top - ARect.Top, FHeight, vHeigth);
        vX2 := MulDiv(vRect.Right - ARect.Left, FWidth, vWidth);
        vY2 := MulDiv(vRect.Bottom - ARect.Top, FHeight, vHeigth);
        if vX1 > 0 then
          Dec(vX1);
        if vY1 > 0 then
          Dec(vY1);
        if vX2 < FWidth then
          Inc(vX2);
        if vY2 < FHeight then
          Inc(vY2);
        vRect.Left := ARect.Left + MulDiv(vX1, vWidth, FWidth);
        vRect.Top := ARect.Top + MulDiv(vY1, vHeigth, FHeight);
        vRect.Right := ARect.Left + MulDiv(vX2, vWidth, FWidth);
        vRect.Bottom := ARect.Top + MulDiv(vY2, vHeigth, FHeight);
        if not(ACanvas is TMetafileCanvas) then
        begin
          if vRect.Left < -$3000 then
            vRect.Left := -$3000;
          if vRect.Top < -$3000 then
            vRect.Top := -$3000;
          if vRect.Right - vRect.Left > $7FFF then
            vRect.Right := vRect.Left + $6000;
          if vRect.Bottom - vRect.Top > $7FFF then
            vRect.Bottom := vRect.Top + $6000;
        end;
{$ENDIF}
        vOldGdiObject := 0;
        if FROP <> SRCCOPY then
          vOldGdiObject := SelectObject(vDC, GetStockObject(BLACK_BRUSH)); // !!!!!?????
        try
{$IFDEF SGFPC}
           vBitmap := TBitmap.Create;
           try
             AssignToBMP(vBitmap);
             StretchBlt(vDC, ARect.Left, ARect.Top, vWidth, vHeigth,
               vBitmap.Canvas.Handle, 0, 0, FWidth, FHeight, FROP);
           finally
             vBitmap.Free;
           end;
{$ELSE}
//          vStretchBltMode := GetStretchBltMode(vDC);
//          try
//            if FPixelFormat <> pf1bit then
//              SetStretchBltMode(vDC, COLORONCOLOR);
{$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}StretchDIBits(vDC, vRect.Left,
              vRect.Top, vRect.Right - vRect.Left,
              vRect.Bottom - vRect.Top, vX1, FHeight - vY2, vX2 - vX1, vY2 - vY1,
              FImg.FData, FImg.FInfo^, DIB_RGB_COLORS, FROP);
//          finally
//            SetStretchBltMode(vDC, vStretchBltMode);
//          end;
{$ENDIF}
        finally
          if vOldGdiObject <> 0 then
            SelectObject(vDC, vOldGdiObject);
        end;
      finally
        FROP := vROP;
      end;
    end;
  end;
end;
{$ELSE}
var
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    Self.AssignToBMP(vBitmap);
  {$IFDEF SG_FIREMONKEY}
    try
      ACanvas.BeginScene;
  {$ENDIF}
      ACanvas.DrawBitmap(vBitmap, TRectF.Create(0, 0, FWidth, FHeight), TRectF.Create(ARect), 1, True);
  {$IFDEF SG_FIREMONKEY}
    finally
      ACanvas.EndScene;
    end;
  {$ENDIF}
  finally
    vBitmap.Free;
  end;
end;
{$ENDIF}

procedure TsgBitmap.DoExceptionNotImplemented;
begin
  Exception.Create('Not implemented');
end;

function TsgBitmap.GetEmpty: Boolean;
begin
  Result := (FWidth = 0) or (FHeight = 0) or (FPixelFormat = pfDevice);
end;

function TsgBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TsgBitmap.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TsgBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

{$IFDEF SG_BLOCK_PREVIEW}
procedure TsgBitmap.LoadFromBitmapInfo(AInfo: PBitmapInfo);
begin
  if AInfo <> nil then
  begin
    FPixelFormat := GetPixelFormat(AInfo);
    FWidth := Abs(AInfo^.bmiHeader.biWidth);
    FHeight := Abs(AInfo^.bmiHeader.biHeight);
    Update;
    Move(AInfo^, FImg.FInfo^, InfoSize);
    Move(PByte(TsgNativeInt(AInfo) + InfoSize)^, FImg.FData^, FHeight * RowSize);
    Changed(Self);
  end;
end;
{$ENDIF}

procedure TsgBitmap.WriteImage(Stream: TStream);
begin
  Stream.WriteBuffer(FImg.FInfo^, InfoSize);
  Stream.WriteBuffer(FImg.FData^, FHeight*RowSize);
end;

{$IFDEF SGFPC}
function TsgBitmap.GetTransparent: Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TsgBitmap.SetHeight(AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    Update;
  end;
end;

procedure TsgBitmap.SetWidth(AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    Update;
  end;
end;

{$IFDEF SGFPC}
procedure TsgBitmap.SetTransparent(Value: Boolean);
begin

end;
{$ENDIF}

{public}

constructor TsgBitmap.Create;
begin
  inherited Create;
  NewImage;
  FROP := SRCCOPY;
end;

destructor TsgBitmap.Destroy;
begin
  FImg.Release;
  inherited Destroy;
end;

procedure TsgBitmap.Assign(ASource: TPersistent);
var
  vsgBitmap: TsgBitmap absolute ASource;
  vGraphic: TGraphic;
begin
  if ASource is TsgBitmap then
  begin
    Self.Transparent := vsgBitmap.Transparent;
    Self.FWidth := vsgBitmap.FWidth;
    Self.FHeight := vsgBitmap.FHeight;
    Self.FROP := vsgBitmap.FROP;
    Self.FPixelFormat := vsgBitmap.FPixelFormat;
    vsgBitmap.FImg.Reference;
    Self.FImg.Release;
    Self.FImg := vsgBitmap.FImg;
    Self.Changed(Self);
  end
  else
    if ASource is TBitmap then
      AssignFromBMP(TBitmap(ASource))
    else
    begin
      if ASource is TGraphic then
      begin
        vGraphic := CreateCopyGraphicAsBitmap(TGraphic(ASource));
        try
          if vGraphic <> nil then
            AssignFromBMP(TBitmap(vGraphic))
          else
            inherited Assign(ASource);
        finally
          vGraphic.Free;
        end;
      end
      else
        inherited Assign(ASource);
    end;
end;

procedure TsgBitmap.Flip;
var
  vPByte0,vPByte1,vPByte2: PAnsiChar;
  vRowSize: Integer;
begin
  vRowSize := RowSize;
  SGGetMem(vPByte2,vRowSize);
  try
    vPByte0 := ScanLine[Height - 1];
    vPByte1 := ScanLine[0];
    while vPByte0 < vPByte1 do
    begin
      Move(vPByte0^, vPByte2^, vRowSize);
      Move(vPByte1^, vPByte0^, vRowSize);
      Move(vPByte2^, vPByte1^, vRowSize);
      Inc(vPByte0,vRowSize);
      Dec(vPByte1,vRowSize);
    end;
  finally
    SGFreeMem(vPByte2);
  end;
end;

function TsgBitmap.GetColors(AColors: Pointer): Integer;
var
  vPByte: PByte;
begin
  Result := 0;
  if (FImg.FInfo = nil) or not (FPixelFormat in [pf1bit..pf8bit]) then
    Exit;
  Result := 1 shl FImg.FInfo.bmiHeader.biBitCount;
  vPByte := PByte(FImg.FInfo);
  Inc(vPByte, SizeOf(TBitmapInfoHeader));
  Move(vPByte^, AColors^, Result shl 2);
end;

procedure TsgBitmap.GetColorsByIndexAndRange(AColor: Pointer;
  const AIndex: Integer; const ARange: Integer);
begin
  ColorsByIndexAndRange(AColor, AIndex, ARange, True)
end;

procedure TsgBitmap.LoadFromClipboardFormat(AFmt: Word; AData: THandle; APal: HPALETTE);
begin
  DoExceptionNotImplemented;
end;

procedure TsgBitmap.LoadFromStream(AStream: TStream);
var
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    vBitmap.LoadFromStream(AStream);
    AssignFromBMP(vBitmap);
  finally
    vBitmap.Free;
  end;
end;

procedure TsgBitmap.Rotate(const AAngle: Integer);
{$IFDEF SGFPC}
var
  vBitmap: TBitmap;
  vROP: Integer;
begin
  vROP := FROP;
  vBitmap := TBitmap.Create;
  try
    AssignToBMP(vBitmap);
    DoRotateBitmap(vBitmap, AAngle);
    AssignFromBMP(vBitmap);
  finally
    FROP := vROP;
    vBitmap.Free;
  end;
end;
{$ELSE}
{$IFNDEF SG_FIREMONKEY}
const
  cnstProcs: array[TPixelFormat] of TRowProc =
    (nil, Row01, Row04, Row08, Row16, Row16, Row24, Row32, nil);
var
  I,vRowSrc,vRowDst: Integer;
  vRowProc: TRowProc;
  vNewData: PByte;
  vPoint: TPoint;
  vRotateData: TRotateData;
  vOctant: Integer;
begin
  vOctant := AAngle div 90 and 3;
  if Empty or (vOctant = 0) then
    Exit;
  if FImg.RefCount > 1 then
    CopyBitmapImage;
  if vOctant=2 then
  begin
    vPoint.X := FWidth;
    vPoint.Y := FHeight;
  end
  else
  begin
    vPoint.X := FHeight;
    vPoint.Y := FWidth;
  end;
  vRowSrc := (FWidth * FImg.FInfo.bmiHeader.biBitCount + 31) and -32 shr 3;
  vRowDst := (vPoint.X * FImg.FInfo.bmiHeader.biBitCount + 31) and -32 shr 3;
  I := vRowDst * vPoint.Y;
  SGGetMem(vNewData, I);
  if PixelFormat < pf8bit then
    FillChar(vNewData^, I, 0);
  vRotateData.Dst := vNewData;
  vRotateData.Src := FImg.FData;
  vRotateData.Count := vPoint.X;
  if vOctant=3 then
    vRotateData.ISrc := 0
  else
    vRotateData.ISrc := FWidth-1;
  if vOctant > 1 then
    Inc(vRotateData.Src, vRowSrc*(FHeight-1));
  if vOctant = 1 then
    vRotateData.DSrc := vRowSrc
  else
    vRotateData.DSrc := -vRowSrc;
  case vOctant of
    1: vRowSrc := -1;
    3: vRowSrc := 1;
  end;
  vRowProc := cnstProcs[PixelFormat];
  if vOctant=2 then
  case PixelFormat of
    pf1bit:	vRowProc := Row01H;
    pf4bit: 	vRowProc := Row04H;
    else	vRotateData.DSrc := -(FImg.FInfo.bmiHeader.biBitCount shr 3);
  end;
  for I:=0 to vPoint.Y -1 do
  begin
    if Assigned(OnRotate) then
      OnRotate(Self);
    vRowProc(vRotateData, @PrepareRow);
    Inc(vRotateData.Dst, vRowDst);
    if vOctant=2 then
      Dec(vRotateData.Src, vRowSrc)
    else
      Inc(vRotateData.ISrc, vRowSrc);
  end;
  SGFreeMem(FImg.FData);
  FImg.FData := vNewData;
  FWidth := vPoint.X;
  FHeight := vPoint.Y;
  FImg.FInfo.bmiHeader.biWidth := FWidth;
  FImg.FInfo.bmiHeader.biHeight := FHeight;
  Changed(Self);
end;
{$ELSE}
var
  vBitmap: TBitmap;
begin
  vBitmap := TBitmap.Create;
  try
    AssignToBMP(vBitmap);
    vBitmap.Rotate(AAngle);
    AssignFromBMP(vBitmap);
  finally
    vBitmap.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

function TsgBitmap.RowSize: Integer;
begin
  Result := (FWidth * FImg.FInfo.bmiHeader.biBitCount + 31) and -32 shr 3;
end;

procedure TsgBitmap.SaveToClipboardFormat(var AFmt: Word; var AData: THandle;
  var APal: HPALETTE);
begin
  DoExceptionNotImplemented;
end;

procedure TsgBitmap.SaveToFile(const FileName: String);
var S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TsgBitmap.SaveToStream(AStream: TStream);
var
  vBitmapFileHeader: TBitmapFileHeader;
begin
  vBitmapFileHeader.bfType := $4D42;
  vBitmapFileHeader.bfOffBits := SizeOf(vBitmapFileHeader);
  if not Empty then
    Inc(vBitmapFileHeader.bfOffBits, InfoSize);
  vBitmapFileHeader.bfSize := vBitmapFileHeader.bfOffBits + FHeight*Cardinal(RowSize);
  vBitmapFileHeader.bfReserved1 := 0;
  vBitmapFileHeader.bfReserved2 := 0;
  AStream.WriteBuffer(vBitmapFileHeader, SizeOf(vBitmapFileHeader));
  WriteImage(AStream);
end;

procedure TsgBitmap.SetColors(AColors: Pointer);
var
  vPByte: PByte;
begin
  if (FImg.FInfo = nil) or not (FPixelFormat in [pf1bit..pf8bit]) then
    Exit;
  vPByte := PByte(FImg.FInfo);
  Inc(vPByte, SizeOf(TBitmapInfoHeader));
  Move(AColors^, vPByte^, 1 shl (FImg.FInfo.bmiHeader.biBitCount + 2));
end;

procedure TsgBitmap.SetColorsByIndexAndRange(AColor: Pointer;
  const AIndex: Integer; const ARange: Integer);
begin
  ColorsByIndexAndRange(AColor, AIndex, ARange, False)
end;

{TsgBitmapImage}

procedure TsgBitmapImage.Assign(ASource: TsgBitmapImage);
begin
  if ASource.Info <> nil then
  begin
    if FInfo = nil then
      New(FInfo);
    FInfo^ := ASource.Info^;
  end
  else
    if FInfo <> nil then
    begin
      SGFreeMem(FInfo);
      FInfo := nil;
    end;
  if ASource.FData <> nil then
  begin

  end
  else
  begin
    if FData <> nil then
    begin
      SGFreeMem(FData);
      FData := nil;
    end;
  end;
end;

procedure TsgBitmapImage.FreeHandle;
begin
  SGReallocMem(FInfo, 0);
  if FData <> nil then
  begin
    SGFreeMem(FData);
    FData := nil;
  end;
end;

{ TsgBMAdapter }

{private}

function TsgBMAdapter.BM: TBitmap;
begin
  Result := TBitmap(Source);
end;

procedure TsgBMAdapter.ForceBM;
var
  vBitmap: TBitmap;
begin
  if IsBM then
    Exit;
  vBitmap := TBitmap.Create;
  try
    if FSource <> nil then
      vBitmap.Assign(FSource);
{$IFNDEF SG_FIREMONKEY}
    if IsSG then
      vBitmap.PixelFormat := SG.PixelFormat;
{$ENDIF}
    Owned := vBitmap;
  except
    vBitmap.Free;
    raise;
  end;
end;

procedure TsgBMAdapter.ForceSG;
var
  vsgBitmap: TsgBitmap;
begin
  if IsSG then
    Exit;
  vsgBitmap := TsgBitmap.Create;
  try
    if FSource <> nil then
      vsgBitmap.Assign(FSource);
    Owned := vsgBitmap;
  except
    vsgBitmap.Free;
    raise;
  end;
end;

function TsgBMAdapter.GetBitmap: TBitmap;
begin
  if BM.ClassName = TsgBitmap.ClassName then
    TsgBitmap(TObject(BM)).AssignTo(FBitmap)
  else
    if BM is TBitmap then
      FBitmap.Assign(BM);
   Result := FBitmap;
end;

function TsgBMAdapter.GetCanvas: TCanvas;
begin
  ForceBM;
  Result := BM.Canvas;
end;

function TsgBMAdapter.GetHandleType: TBitmapHandleType;
begin
{$IFNDEF SG_FIREMONKEY}
  if not IsBM then
    Result := bmDIB
  else
    Result := BM.HandleType;
{$ELSE}
  Result := bmDIB;
{$ENDIF}
end;

function TsgBMAdapter.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TsgBMAdapter.GetPixelFormat: TPixelFormat;
{$IFNDEF SG_FIREMONKEY}
var
  vDIBSection: TDIBSection;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  if IsBM then
    Result := BM.PixelFormat
  else Result := SG.PixelFormat;
  if (Result=pfCustom) and IsBM then
  begin
    GetObject(BM.Handle, SizeOf(vDIBSection), @vDIBSection);
    if (vDIBSection.dsBmih.biBitCount = 16) and
       (vDIBSection.dsBmih.biCompression = BI_BITFIELDS) then
      Result := pf15bit;
  end;
{$ELSE}
  Result := TPixelFormat.pf32bit;
{$ENDIF}
end;

function TsgBMAdapter.GetROP: Integer;
begin
  if IsSG then
    Result := SG.FROP
  else
    Result := SRCCOPY;
end;

function TsgBMAdapter.GetScanLine(ARow: Integer): Pointer;
{$IFDEF SG_FIREMONKEY}
var
  vBitData: TBitmapData;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  if IsBM then
    Result := BM.ScanLine[ARow]
  else
    Result := SG.ScanLine[ARow];
{$ELSE}
  BM.Map(TMapAccess.Read, vBitData);
  Result := vBitData.GetScanline(ARow);
  BM.Unmap(vBitData);
{$ENDIF}
end;

function TsgBMAdapter.GetSource: TPersistent;
begin
  if FSource = nil then
    ForceSG;
  Result := FSource;
end;

function TsgBMAdapter.IsBM: Boolean;
begin
  Result := FSource is TBitmap;
end;

function TsgBMAdapter.IsRGB: Boolean;
{$IFDEF SGFPC}
var
  vDesc: PRawImageDescription;
{$ENDIF}
begin
  Result := True;
{$IFDEF SGFPC}
  if IsBM and (Bitmap.PixelFormat in [pf24bit, pf32bit]) then
  begin
    vDesc := TRasterImageAccess(FSource).GetRawImageDescriptionPtr;
    if Assigned(vDesc) then
      Result := (vDesc^.RedShift = 0) and (vDesc^.BlueShift = 16)
    else
      Result := False;
  end;
{$ELSE}
  {$IFDEF SG_FIREMONKEY}
  if IsBM then
  begin
    Result := TBitmap(FSource).ImagePixelFormat in [FMX.Types.TPixelFormat.RGB,
      FMX.Types.TPixelFormat.RGBA, FMX.Types.TPixelFormat.RGBA16,
      FMX.Types.TPixelFormat.RGB10_A2, FMX.Types.TPixelFormat.RGBA16F,
      FMX.Types.TPixelFormat.RGBA32F];
  end;
  {$ENDIF}
{$ENDIF}
end;

function TsgBMAdapter.IsSG: Boolean;
begin
  Result := FSource is TsgBitmap;
end;

procedure TsgBMAdapter.SetHandleType(AValue: TBitmapHandleType);
begin
{$IFNDEF SG_FIREMONKEY}
  if IsBM then
    BM.HandleType := AValue;
{$ENDIF}
end;

procedure TsgBMAdapter.SetOwned(AValue: TPersistent);
begin
  if AValue <> FOwned then
  begin
    FOwned.Free;
    FOwned := AValue;
    FSource := AValue;
  end;
end;

procedure TsgBMAdapter.SetPalette(Value: HPALETTE);
begin

end;

procedure TsgBMAdapter.SetPixelFormat(AValue: TPixelFormat);
begin
{$IFNDEF SG_FIREMONKEY}
  if AValue = PixelFormat then
    Exit;
  if IsSG and Empty then
    SG.PixelFormat := AValue
  else
  begin
    ForceBM;
    BM.PixelFormat := AValue;
  end;
{$ENDIF}
end;

procedure TsgBMAdapter.SetROP(AValue: Integer);
begin
  if AValue = GetROP then
    Exit;
  if AValue = SRCCOPY then
    ForceBM
  else
  begin
    ForceSG;
    SG.FROP := AValue;
  end;
end;

procedure TsgBMAdapter.SetSource(AValue: TPersistent);
begin
  if AValue <> FSource then
  begin
    FOwned.Free;
    FOwned := nil;
    FSource := AValue;
  end;
end;

function TsgBMAdapter.SG: TsgBitmap;
begin
  Result := TsgBitmap(Source);
end;

{protected}

procedure TsgBMAdapter.AssignTo(ADest: TPersistent);
begin
  ADest.Assign(Source);
end;

procedure TsgBMAdapter.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
{$IFNDEF SG_FIREMONKEY}
  ACanvas.StretchDraw(ARect, TGraphic(Source));
{$ELSE}
  ACanvas.DrawBitmap(TBitmap(Source), TBitmap(Source).Bounds, TRectF.Create(ARect), 1);
{$ENDIF}
end;

function TsgBMAdapter.GetEmpty: Boolean;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := TGraphic(Source).Empty;
{$ELSE}
  Result := TBitmap(Source).IsEmpty;
{$ENDIF}
end;

function TsgBMAdapter.GetHeight: Integer;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := TGraphic(Source).Height;
{$ELSE}
  Result := TBitmap(Source).Height;
{$ENDIF}
end;

function TsgBMAdapter.GetWidth: Integer;
begin
{$IFNDEF SG_FIREMONKEY}
  Result := TGraphic(Source).Width;
{$ELSE}
  Result := TBitmap(Source).Width;
{$ENDIF}
end;

{$IFDEF SGFPC}
function TsgBMAdapter.GetTransparent: Boolean;
begin
  Result := False;
end;
{$ENDIF}

procedure TsgBMAdapter.SetHeight(AValue: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
  TGraphic(Source).Height := AValue;
{$ELSE}
  TBitmap(Source).Height := AValue;
{$ENDIF}
end;

procedure TsgBMAdapter.SetWidth(AValue: Integer);
begin
{$IFNDEF SG_FIREMONKEY}
  TGraphic(Source).Width := AValue;
{$ELSE}
  TBitmap(Source).Width := AValue;
{$ENDIF}
end;

{$IFDEF SGFPC}
procedure TsgBMAdapter.SetTransparent(Value: Boolean);
begin
  //
end;
{$ENDIF}

{public}

constructor TsgBMAdapter.Create;
begin
  FBitmap := TBitmap.Create;
  inherited Create;
end;

destructor TsgBMAdapter.Destroy;
begin
  FBitmap.Free;
  FOwned.Free;
  inherited Destroy;
end;

procedure TsgBMAdapter.Assign(ASrc: TPersistent);
  function TryAssign: Boolean;
  begin
    Result := True;
    try
      BM.Assign(ASrc)
    except
      Result := False
    end;
  end;
begin
  if ASrc is TsgBitmap then
    Source := TsgBitmap(ASrc)
  else
    if ASrc is TBitmap then
      Source := TBitmap(ASrc)
    else
      {$IFDEF SG_DELPHI_VCL}
      if ASrc is TMetafile then
      begin
        ForceBM;
        AssignToBitmap(TGraphic(ASrc), BM);
      end
      else
      {$ENDIF}
        if ASrc is TGraphic then
        begin
          ForceBM;
          if not TryAssign then
            AssignToBitmap(TGraphic(ASrc), BM);
        end
        else
          inherited Assign(ASrc);
end;

function TsgBMAdapter.GetColorTable(APoint: Pointer): Integer;
begin
  if IsSG then
    Result := SG.GetColors(APoint)
  else
    {$IFNDEF SG_NON_WIN_PLATFORM}
    Result := GetDIBColorTable(BM.Canvas.Handle, 0, 256, APoint^);
    {$ELSE}
    Result := 0;
    {$ENDIF}
end;


procedure TsgBMAdapter.LoadFromClipboardFormat(AFmt:{$IFDEF SGFPC}TClipboardFormat{$ELSE}Word;
 AData: THandle; APal: HPALETTE{$ENDIF});
begin
{$IFNDEF SG_FIREMONKEY}
  ForceBM;
  BM.LoadFromClipboardFormat(AFmt{$IFNDEF SGFPC}, AData, APal{$ENDIF});
{$ENDIF}
end;

procedure TsgBMAdapter.LoadFromStream(AStream: TStream);
begin
{$IFNDEF SG_FIREMONKEY}
  TGraphic(Source).LoadFromStream(AStream);
{$ELSE}
  TBitmap(Source).LoadFromStream(AStream);
{$ENDIF}
end;

procedure TsgBMAdapter.Rotate(AAngle: Integer);
begin
  ForceSG;
  SG.Rotate(AAngle);
end;

procedure TsgBMAdapter.SaveToClipboardFormat({$IFNDEF SGFPC}var {$ENDIF} AFmt:{$IFDEF SGFPC}TClipboardFormat{$ELSE}Word;
 var AData: THandle; var APal: HPALETTE{$ENDIF});
begin
{$IFNDEF SG_FIREMONKEY}
  ForceBM;
  BM.SaveToClipboardFormat(AFmt{$IFNDEF SGFPC}, AData, APal{$ENDIF});
{$ENDIF}
end;

procedure TsgBMAdapter.SaveToStream(AStream: TStream);
begin
{$IFNDEF SG_FIREMONKEY}
  TGraphic(Source).SaveToStream(AStream);
{$ELSE}
  TBitmap(Source).SaveToStream(AStream);
{$ENDIF}
end;

(*// For future versions
initialization
  TPicture.RegisterFileFormat('bmp', 'Bitmap image', TsgBitmap);

finalization
  TPicture.UnRegisterGraphicClass(TsgBitmap);
*)
end.

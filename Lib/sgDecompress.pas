{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{              TsgDecompressor class                         }
{   Decompresses the images from the built-in CGM-drawing    }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDecompress;
{$INCLUDE SGDXF.inc}
{$IFDEF SGFPC}      {$DEFINE SG_INCLUDE_PNG} {$ELSE}
{$IFDEF SGDEL_2009} {$DEFINE SG_INCLUDE_PNG} {$ENDIF}{$ENDIF}
//{$DEFINE SG_DECOMPRESS_DEBUG}
{$IFDEF SG_INCLUDE_PNG}
//{$DEFINE SG_IMPORT_INCORRECT_PNG}
{$ENDIF}

interface

uses 
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  FPImage, IntfGraphics,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  {$UNDEF SG_INCLUDE_PNG}
  FMX.Graphics, FMX.Types, FMX.Surfaces,  System.IOUtils,
  sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, MVFont,
  {$IFNDEF SGFPC}
    {$IFDEF SGDEL_XE2}Vcl.Imaging.jpeg{$ELSE}jpeg{$ENDIF},
  {$ENDIF}
{$ENDIF}
  sgFunction, Math, sgConsts
{$IFDEF SG_INCLUDE_PNG}
  {$IFNDEF SGFPC},pngimage{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.UITypes
  , System.Types
{$ENDIF}
  ;

type

{
1	Null Foreground
2	T6
3	T4 1-dimension
4	T4 2-dimension
5	Bitmap (uncompressed)
6	Run length
7	Baseline JPEG
8	LZW
9	PNG Compression Method 0
}
  TsgCgmCompressionType = (ctNull_Background, ctNull_Foreground, ctT6,
    ctT4_1Dimensional, ctT4_2Dimensional, ctUncompressed, ctRLE, ctBaselineJPEG,
    ctLZW, ctPNGCopmM0, ctReserved);
  TsgCgmT6_CodeWord = (cwUncode, cwPass, cwHorizontal, cwV0, cwVR1, cwVR2, cwVR3,
    cwVL1, cwVL2, cwVL3, cwExtension, cwEOF, cwEOL);
  TsgCgmT6_UncompressedCode = (uc1, uc01, uc001, uc0001, uc00001, uc00000,
    ucEmpty, uc0, uc00, uc000, uc0000, ucError);

  TsgTerminalCode = record
    RunLength: Word;
    Code: Word;
    Length: Byte;
  end;

  TsgModeCodesTable = array [0..11] of TsgTerminalCode;
  TsgTerminalCodesTable = array [0..104] of TsgTerminalCode;

  TsgProcReadAlignBytes = procedure of object;
  TsgProcReadColor = function: Cardinal of object;

{$IFDEF SGFPC}
  TPngImage = TPortableNetworkGraphic;
{$ENDIF}

  TsgCustomMemoryStream = class(TMemoryStream)
  private
    FReadBitsCount: Integer;
    FReadBitsBuffer: Cardinal;
    FSavePosition: Int64;
  protected
    function ReadBit: Integer;
    procedure RestorePosition; virtual;
    procedure SavePosition; virtual;
  public
    function GetByteCount(const ASize: Integer): Integer;
    function ReadByteCount(const ASize: Integer): Integer; virtual; abstract;
    function IsEndCommand: Boolean; virtual; abstract;

    procedure BeginReadBits; virtual;
    procedure EndReadBits; virtual;
    function ReadSDR: Integer; virtual;

    function GetBits(const ASize: Integer): Integer;
    function ReadBits(const ASize: Integer): Integer;
    procedure SeekBits(const ASeek: Integer);
  end;

  { TsgDecompressor }

  TsgDecompressor = class
  private
    FBitmap__: {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap;
    {$IFNDEF SG_DELPHI_VCL}
    FBitmapData: {$IFDEF SG_FIREMONKEY}FMX.Graphics.TBitmapData{$ELSE}TLazIntfImage{$ENDIF};
    {$ELSE}
    FBitmapData: TsgMap;
    {$ENDIF}
    FPixelFormat: {$IFDEF SG_FIREMONKEY}FMX.Types.{$ENDIF}TPixelFormat;
    FRowIndex: Integer;
    FDrawRow: array of Integer;
    FScanRow: array of Integer;
    FUseBuffer: Boolean;
    FPenColor: TColor;
    FColors: array of Integer;
    FCompression: TsgCgmCompressionType;
    FNumberOfCells: TPoint;
    FNumberOfTiles: TPoint;
    FOneLen: TPoint;
    FOffset: TPoint;
    FReader: TsgCustomMemoryStream;
    FProcReadColor: TsgProcReadColor;
    FProcReadAlignBytes: TsgProcReadAlignBytes;
    FSDR: TMemoryStream;
    FRunCountPrecision: Integer;
    procedure DoNextDrawLine;
    procedure DoNextScanLine;

    function ReadBit: Integer;
    procedure ReadAlignBytes;
    function ReadColor: Cardinal;
    function ReadColorLength: Integer;
    function ReadCodeWord: TsgCgmT6_CodeWord;
    function ReadBitmap(const AIsRLE: Boolean): Boolean;
    function ReadRunLength(const ATable: TsgTerminalCodesTable): Integer;
    function GetA1(const B1: Integer; const ACodeWord: TsgCgmT6_CodeWord): Integer;
    function GetB1(const A0: Integer; const ActiveColor: Boolean): Integer;
    function GetB2(const A0: Integer; const ActiveColor: Boolean): Integer;    
    function GetBackground: TColor;
    function GetForeground: TColor;
    function GetNumberOfCells: TPoint;
    function GetNumberOfTiles: TPoint;

    function GetUncompressedMode: Integer;
    function GetUncompressedCode: TsgCgmT6_UncompressedCode;
    procedure SetBackground(const Value: TColor);
    procedure SetCompression(const Value: TsgCgmCompressionType);
    procedure SetForeground(const Value: TColor);
    procedure SetNumberOfCells(const Value: TPoint);
    procedure SetNumberOfTiles(const Value: TPoint);
    procedure SetPenColor(const AColor: TColor);

    procedure FillRow(var ARow: array of Integer; const AColor: TColor);
    procedure DrawRowLine(X1, X2: Integer; const AColor: TColor);
    procedure DrawRowPoint(const X: Integer; AColor: TColor);
    function GetScanRowPixels(const X: Integer): TColor;
    procedure ReadSDR;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    procedure BitmapChanged(Sender: TObject);
    procedure DrawLineWO(const X1, X2: Integer; const AColor: TColor);
    procedure DrawPointWO(const X: Integer; const AColor: TColor);
    function GetScanPointWO(const X: Integer): TColor;
    procedure DrawPixels(var X, Y: Integer; ALength, AColor: Integer; const AModeRC: Boolean);

    function Decompress_Uncompress: Boolean;
    function Decompress_BaselineJPEG: Boolean;
    function Decompress_BaselinePNG: Boolean;
    function Decompress_RLE: Boolean;
    function Decompress_T4_1D: Boolean;
    function Decompress_T4_2D: Boolean;
    function Decompress_T6: Boolean;
    {$IFDEF SG_FIREMONKEY}
    function GetAlphaColor(AColor: TColor): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ApplyPadding(const APadding: Integer);
    function Execute(const AOffset: TPoint;
      const AReader: TsgCustomMemoryStream; const AReadSDR: Boolean = False): Boolean;
    function BeforeDecompress(const AFormat: TPixelFormat): Boolean;
    function SetPixelFormat(const AFormat: TPixelFormat): Boolean;
    procedure AfterDecompress;
    property Background: TColor read GetBackground write SetBackground;
    property Bitmap: {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap read FBitmap__;
    property Compression: TsgCgmCompressionType read FCompression write SetCompression;
    property Foreground: TColor read GetForeground write SetForeground;
    property NumberOfCells: TPoint read GetNumberOfCells write SetNumberOfCells;
    property NumberOfTiles: TPoint read GetNumberOfTiles write SetNumberOfTiles;
    property ProcReadColor: TsgProcReadColor read FProcReadColor write FProcReadColor;
    property ProcReadAlignBytes: TsgProcReadAlignBytes read FProcReadAlignBytes write FProcReadAlignBytes;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

type
  PsgColor = ^TsgColor;
  TsgColor = array [0..3] of Byte;

{$IFDEF SG_IMPORT_INCORRECT_PNG}
  TsgPngImage = class(TPngImage)
  private
    procedure DoCorrectPngData;
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;
{$ENDIF}

  TsgCGMMap = class(TsgMap)
  protected
    function GetPalIndex(AColor: TColor): Integer; override;
  end;

const
  cnstMaxByte = High(Byte);
  cnstMaxWord = High(Word);
  cnstRawWidth = 50;

  cnstEOF: TsgTerminalCode = (RunLength: $1001; Code: $1001; Length: 13);  //1000 0000 0000 1
  cnstEOL: TsgTerminalCode = (RunLength: cnstMaxWord; Code: 1; Length: 12);//0000 0000 0001

  cnstTerminalCodesBlack: TsgTerminalCodesTable = (
    (RunLength: 0000; Code: $37; Length: 10), // 0000 1101 11           0   0000110111
    (RunLength: 0001; Code: $02; Length: 03), // 010                    1   010
    (RunLength: 0002; Code: $03; Length: 02), // 11                     2   11
    (RunLength: 0003; Code: $02; Length: 02), // 10                     3   10
    (RunLength: 0004; Code: $03; Length: 03), // 011                    4   011
    (RunLength: 0005; Code: $03; Length: 04), // 0011                   5   0011
    (RunLength: 0006; Code: $02; Length: 04), // 0010                   6   0010
    (RunLength: 0007; Code: $03; Length: 05), // 0001 1                 7   00011
    (RunLength: 0008; Code: $05; Length: 06), // 0001 01                8   000101
    (RunLength: 0009; Code: $04; Length: 06), // 0001 00                9   000100
    (RunLength: 0010; Code: $04; Length: 07), // 0000 100               10  0000100
    (RunLength: 0011; Code: $05; Length: 07), // 0000 101               11  0000101
    (RunLength: 0012; Code: $07; Length: 07), // 0000 111               12  0000111
    (RunLength: 0013; Code: $04; Length: 08), // 0000 0100              13  00000100
    (RunLength: 0014; Code: $07; Length: 08), // 0000 0111              14  00000111
    (RunLength: 0015; Code: $18; Length: 09), // 0000 1100 0            15  000011000
    (RunLength: 0016; Code: $17; Length: 10), // 0000 0101 11           16  0000010111
    (RunLength: 0017; Code: $18; Length: 10), // 0000 0110 00           17  0000011000
    (RunLength: 0018; Code: $08; Length: 10), // 0000 0010 00           18  0000001000
    (RunLength: 0019; Code: $67; Length: 11), // 0000 1100 111          19  00001100111
    (RunLength: 0020; Code: $68; Length: 11), // 0000 1101 000          20  00001101000
    (RunLength: 0021; Code: $6C; Length: 11), // 0000 1101 100          21  00001101100
    (RunLength: 0022; Code: $37; Length: 11), // 0000 0110 111          22  00000110111
    (RunLength: 0023; Code: $28; Length: 11), // 0000 0101 000          23  00000101000
    (RunLength: 0024; Code: $17; Length: 11), // 0000 0010 111          24  00000010111
    (RunLength: 0025; Code: $18; Length: 11), // 0000 0011 000          25  00000011000
    (RunLength: 0026; Code: $CA; Length: 12), // 0000 1100 1010         26  000011001010
    (RunLength: 0027; Code: $CB; Length: 12), // 0000 1100 1011         27  000011001011
    (RunLength: 0028; Code: $CC; Length: 12), // 0000 1100 1100         28  000011001100
    (RunLength: 0029; Code: $CD; Length: 12), // 0000 1100 1101         29  000011001101
    (RunLength: 0030; Code: $68; Length: 12), // 0000 0110 1000         30  000001101000
    (RunLength: 0031; Code: $69; Length: 12), // 0000 0110 1001         31  000001101001
    (RunLength: 0032; Code: $6A; Length: 12), // 0000 0110 1010         32  000001101010
    (RunLength: 0033; Code: $6B; Length: 12), // 0000 0110 1011         33  000001101011
    (RunLength: 0034; Code: $D2; Length: 12), // 0000 1101 0010         34  000011010010
    (RunLength: 0035; Code: $D3; Length: 12), // 0000 1101 0011         35  000011010011
    (RunLength: 0036; Code: $D4; Length: 12), // 0000 1101 0100         36  000011010100
    (RunLength: 0037; Code: $D5; Length: 12), // 0000 1101 0101         37  000011010101
    (RunLength: 0038; Code: $D6; Length: 12), // 0000 1101 0110         38  000011010110
    (RunLength: 0039; Code: $D7; Length: 12), // 0000 1101 0111         39  000011010111
    (RunLength: 0040; Code: $6C; Length: 12), // 0000 0110 1100         40  000001101100
    (RunLength: 0041; Code: $6D; Length: 12), // 0000 0110 1101         41  000001101101
    (RunLength: 0042; Code: $DA; Length: 12), // 0000 1101 1010         42  000011011010
    (RunLength: 0043; Code: $DB; Length: 12), // 0000 1101 1011         43  000011011011
    (RunLength: 0044; Code: $54; Length: 12), // 0000 0101 0100         44  000001010100
    (RunLength: 0045; Code: $55; Length: 12), // 0000 0101 0101         45  000001010101
    (RunLength: 0046; Code: $56; Length: 12), // 0000 0101 0110         46  000001010110
    (RunLength: 0047; Code: $57; Length: 12), // 0000 0101 0111         47  000001010111
    (RunLength: 0048; Code: $64; Length: 12), // 0000 0110 0100         48  000001100100
    (RunLength: 0049; Code: $65; Length: 12), // 0000 0110 0101         49  000001100101
    (RunLength: 0050; Code: $52; Length: 12), // 0000 0101 0010         50  000001010010
    (RunLength: 0051; Code: $53; Length: 12), // 0000 0101 0011         51  000001010011
    (RunLength: 0052; Code: $24; Length: 12), // 0000 0010 0100         52  000000100100
    (RunLength: 0053; Code: $37; Length: 12), // 0000 0011 0111         53  000000110111
    (RunLength: 0054; Code: $38; Length: 12), // 0000 0011 1000         54  000000111000
    (RunLength: 0055; Code: $27; Length: 12), // 0000 0010 0111         55  000000100111
    (RunLength: 0056; Code: $28; Length: 12), // 0000 0010 1000         56  000000101000
    (RunLength: 0057; Code: $58; Length: 12), // 0000 0101 1000         57  000001011000
    (RunLength: 0058; Code: $59; Length: 12), // 0000 0101 1001         58  000001011001
    (RunLength: 0059; Code: $2B; Length: 12), // 0000 0010 1011         59  000000101011
    (RunLength: 0060; Code: $2C; Length: 12), // 0000 0010 1100         60  000000101100
    (RunLength: 0061; Code: $5A; Length: 12), // 0000 0101 1010         61  000001011010
    (RunLength: 0062; Code: $66; Length: 12), // 0000 0110 0110         62  000001100110
    (RunLength: 0063; Code: $67; Length: 12), // 0000 0110 0111         63  000001100111
    (RunLength: 0064; Code: $0F; Length: 10), // 0000 0011 11           64      0000001111
    (RunLength: 0128; Code: $C8; Length: 12), // 0000 1100 1000         128     000011001000
    (RunLength: 0192; Code: $C9; Length: 12), // 0000 1100 1001         192     000011001001
    (RunLength: 0256; Code: $5B; Length: 12), // 0000 0101 1011         256     000001011011
    (RunLength: 0320; Code: $33; Length: 12), // 0000 0011 0011         320     000000110011
    (RunLength: 0384; Code: $34; Length: 12), // 0000 0011 0100         384     000000110100
    (RunLength: 0448; Code: $35; Length: 12), // 0000 0011 0101         448     000000110101
    (RunLength: 0512; Code: $6C; Length: 13), // 0000 0011 0110 0       512     0000001101100
    (RunLength: 0576; Code: $6D; Length: 13), // 0000 0011 0110 1       576     0000001101101
    (RunLength: 0640; Code: $4A; Length: 13), // 0000 0010 0101 0       640     0000001001010
    (RunLength: 0704; Code: $4B; Length: 13), // 0000 0010 0101 1       704     0000001001011
    (RunLength: 0768; Code: $4C; Length: 13), // 0000 0010 0110 0       768     0000001001100
    (RunLength: 0832; Code: $4D; Length: 13), // 0000 0010 0110 1       832     0000001001101
    (RunLength: 0896; Code: $72; Length: 13), // 0000 0011 1001 0       896     0000001110010
    (RunLength: 0960; Code: $73; Length: 13), // 0000 0011 1001 1       960     0000001110011
    (RunLength: 1024; Code: $74; Length: 13), // 0000 0011 1010 0       1024    0000001110100
    (RunLength: 1088; Code: $75; Length: 13), // 0000 0011 1010 1       1088    0000001110101
    (RunLength: 1152; Code: $76; Length: 13), // 0000 0011 1011 0       1152    0000001110110
    (RunLength: 1216; Code: $77; Length: 13), // 0000 0011 1011 1       1216    0000001110111
    (RunLength: 1280; Code: $52; Length: 13), // 0000 0010 1001 0       1280    0000001010010
    (RunLength: 1344; Code: $53; Length: 13), // 0000 0010 1001 1       1344    0000001010011
    (RunLength: 1408; Code: $54; Length: 13), // 0000 0010 1010 0       1408    0000001010100
    (RunLength: 1472; Code: $55; Length: 13), // 0000 0010 1010 1       1472    0000001010101
    (RunLength: 1536; Code: $5A; Length: 13), // 0000 0010 1101 0       1536    0000001011010
    (RunLength: 1600; Code: $5B; Length: 13), // 0000 0010 1101 1       1600    0000001011011
    (RunLength: 1664; Code: $64; Length: 13), // 0000 0011 0010 0       1664    0000001100100
    (RunLength: 1728; Code: $65; Length: 13), // 0000 0011 0010 1       1728    0000001100101
    (RunLength: 1792; Code: $08; Length: 11), // 0000 0001 000          1792  00000001000
    (RunLength: 1856; Code: $0C; Length: 11), // 0000 0001 100          1856  00000001100
    (RunLength: 1920; Code: $0D; Length: 11), // 0000 0001 101          1920  00000001101
    (RunLength: 1984; Code: $12; Length: 12), // 0000 0001 0010         1984  000000010010
    (RunLength: 2048; Code: $13; Length: 12), // 0000 0001 0011         2048  000000010011
    (RunLength: 2112; Code: $14; Length: 12), // 0000 0001 0100         2112  000000010100
    (RunLength: 2176; Code: $15; Length: 12), // 0000 0001 0101         2176  000000010101
    (RunLength: 2240; Code: $16; Length: 12), // 0000 0001 0110         2240  000000010110
    (RunLength: 2304; Code: $17; Length: 12), // 0000 0001 0111         2304  000000010111
    (RunLength: 2368; Code: $1C; Length: 12), // 0000 0001 1100         2368  000000011100
    (RunLength: 2432; Code: $1D; Length: 12), // 0000 0001 1101         2432  000000011101
    (RunLength: 2496; Code: $1E; Length: 12), // 0000 0001 1110         2496  000000011110
    (RunLength: 2560; Code: $1F; Length: 12), // 0000 0001 1111         2560  000000011111
    (RunLength: cnstMaxWord; Code: 1; Length: 12)//cnstEOL
    );

  cnstTerminalCodesWhite: TsgTerminalCodesTable = (
    (RunLength: 0000; Code: $35; Length: 08), // 0011 0101         0    00110101
    (RunLength: 0001; Code: $07; Length: 06), // 0001 11           1    000111
    (RunLength: 0002; Code: $07; Length: 04), // 0111              2    0111
    (RunLength: 0003; Code: $08; Length: 04), // 1000              3    1000
    (RunLength: 0004; Code: $0B; Length: 04), // 1011              4    1011
    (RunLength: 0005; Code: $0C; Length: 04), // 1100              5    1100
    (RunLength: 0006; Code: $0E; Length: 04), // 1110              6    1110
    (RunLength: 0007; Code: $0F; Length: 04), // 1111              7    1111
    (RunLength: 0008; Code: $13; Length: 05), // 1001 1            8    10011
    (RunLength: 0009; Code: $14; Length: 05), // 1010 0            9    10100
    (RunLength: 0010; Code: $07; Length: 05), // 0011 1            10   00111
    (RunLength: 0011; Code: $08; Length: 05), // 0100 0            11   01000
    (RunLength: 0012; Code: $08; Length: 06), // 0010 00           12   001000
    (RunLength: 0013; Code: $03; Length: 06), // 0000 11           13   000011
    (RunLength: 0014; Code: $34; Length: 06), // 1101 00           14   110100
    (RunLength: 0015; Code: $35; Length: 06), // 1101 01           15   110101
    (RunLength: 0016; Code: $2A; Length: 06), // 1010 10           16   101010
    (RunLength: 0017; Code: $2B; Length: 06), // 1010 11           17   101011
    (RunLength: 0018; Code: $27; Length: 07), // 0100 111          18   0100111
    (RunLength: 0019; Code: $0C; Length: 07), // 0001 100          19   0001100
    (RunLength: 0020; Code: $08; Length: 07), // 0001 000          20   0001000
    (RunLength: 0021; Code: $17; Length: 07), // 0010 111          21   0010111
    (RunLength: 0022; Code: $03; Length: 07), // 0000 011          22   0000011
    (RunLength: 0023; Code: $04; Length: 07), // 0000 100          23   0000100
    (RunLength: 0024; Code: $28; Length: 07), // 0101 000          24   0101000
    (RunLength: 0025; Code: $2B; Length: 07), // 0101 011          25   0101011
    (RunLength: 0026; Code: $13; Length: 07), // 0010 011          26   0010011
    (RunLength: 0027; Code: $24; Length: 07), // 0100 100          27   0100100
    (RunLength: 0028; Code: $18; Length: 07), // 0011 000          28   0011000
    (RunLength: 0029; Code: $02; Length: 08), // 0000 0010         29   00000010
    (RunLength: 0030; Code: $03; Length: 08), // 0000 0011         30   00000011
    (RunLength: 0031; Code: $1A; Length: 08), // 0001 1010         31   00011010
    (RunLength: 0032; Code: $1B; Length: 08), // 0001 1011         32   00011011
    (RunLength: 0033; Code: $12; Length: 08), // 0001 0010         33   00010010
    (RunLength: 0034; Code: $13; Length: 08), // 0001 0011         34   00010011
    (RunLength: 0035; Code: $14; Length: 08), // 0001 0100         35   00010100
    (RunLength: 0036; Code: $15; Length: 08), // 0001 0101         36   00010101
    (RunLength: 0037; Code: $16; Length: 08), // 0001 0110         37   00010110
    (RunLength: 0038; Code: $17; Length: 08), // 0001 0111         38   00010111
    (RunLength: 0039; Code: $28; Length: 08), // 0010 1000         39   00101000
    (RunLength: 0040; Code: $29; Length: 08), // 0010 1001         40   00101001
    (RunLength: 0041; Code: $2A; Length: 08), // 0010 1010         41   00101010
    (RunLength: 0042; Code: $2B; Length: 08), // 0010 1011         42   00101011
    (RunLength: 0043; Code: $2C; Length: 08), // 0010 1100         43   00101100
    (RunLength: 0044; Code: $2D; Length: 08), // 0010 1101         44   00101101
    (RunLength: 0045; Code: $04; Length: 08), // 0000 0100         45   00000100
    (RunLength: 0046; Code: $05; Length: 08), // 0000 0101         46   00000101
    (RunLength: 0047; Code: $0A; Length: 08), // 0000 1010         47   00001010
    (RunLength: 0048; Code: $0B; Length: 08), // 0000 1011         48   00001011
    (RunLength: 0049; Code: $52; Length: 08), // 0101 0010         49   01010010
    (RunLength: 0050; Code: $53; Length: 08), // 0101 0011         50   01010011
    (RunLength: 0051; Code: $54; Length: 08), // 0101 0100         51   01010100
    (RunLength: 0052; Code: $55; Length: 08), // 0101 0101         52   01010101
    (RunLength: 0053; Code: $24; Length: 08), // 0010 0100         53   00100100
    (RunLength: 0054; Code: $25; Length: 08), // 0010 0101         54   00100101
    (RunLength: 0055; Code: $58; Length: 08), // 0101 1000         55   01011000
    (RunLength: 0056; Code: $59; Length: 08), // 0101 1001         56   01011001
    (RunLength: 0057; Code: $5A; Length: 08), // 0101 1010         57   01011010
    (RunLength: 0058; Code: $5B; Length: 08), // 0101 1011         58   01011011
    (RunLength: 0059; Code: $4A; Length: 08), // 0100 1010         59   01001010
    (RunLength: 0060; Code: $4B; Length: 08), // 0100 1011         60   01001011
    (RunLength: 0061; Code: $32; Length: 08), // 0011 0010         61   00110010
    (RunLength: 0062; Code: $33; Length: 08), // 0011 0011         62   00110011
    (RunLength: 0063; Code: $34; Length: 08), // 0011 0100         63   00110100
    (RunLength: 0064; Code: $1B; Length: 05), // 1101 1            64     11011
    (RunLength: 0128; Code: $12; Length: 05), // 1001 0            128    10010
    (RunLength: 0192; Code: $17; Length: 06), // 0101 11           192    010111
    (RunLength: 0256; Code: $37; Length: 07), // 0110 111          256    0110111
    (RunLength: 0320; Code: $36; Length: 08), // 0011 0110         320    00110110
    (RunLength: 0384; Code: $37; Length: 08), // 0011 0111         384    00110111
    (RunLength: 0448; Code: $64; Length: 08), // 0110 0100         448    01100100
    (RunLength: 0512; Code: $65; Length: 08), // 0110 0101         512    01100101
    (RunLength: 0576; Code: $68; Length: 08), // 0110 1000         576    01101000
    (RunLength: 0640; Code: $67; Length: 08), // 0110 0111         640    01100111
    (RunLength: 0704; Code: $CC; Length: 09), // 0110 0110 0       704    011001100
    (RunLength: 0768; Code: $CD; Length: 09), // 0110 0110 1       768    011001101
    (RunLength: 0832; Code: $D2; Length: 09), // 0110 1001 0       832    011010010
    (RunLength: 0896; Code: $D3; Length: 09), // 0110 1001 1       896    011010011
    (RunLength: 0960; Code: $D4; Length: 09), // 0110 1010 0       960    011010100
    (RunLength: 1024; Code: $D5; Length: 09), // 0110 1010 1       1024   011010101
    (RunLength: 1088; Code: $D6; Length: 09), // 0110 1011 0       1088   011010110
    (RunLength: 1152; Code: $D7; Length: 09), // 0110 1011 1       1152   011010111
    (RunLength: 1216; Code: $D8; Length: 09), // 0110 1100 0       1216   011011000
    (RunLength: 1280; Code: $D9; Length: 09), // 0110 1100 1       1280   011011001
    (RunLength: 1344; Code: $DA; Length: 09), // 0110 1101 0       1344   011011010
    (RunLength: 1408; Code: $DB; Length: 09), // 0110 1101 1       1408   011011011
    (RunLength: 1472; Code: $98; Length: 09), // 0100 1100 0       1472   010011000
    (RunLength: 1536; Code: $99; Length: 09), // 0100 1100 1       1536   010011001
    (RunLength: 1600; Code: $9A; Length: 09), // 0100 1101 0       1600   010011010
    (RunLength: 1664; Code: $18; Length: 06), // 0110 00           1664   011000
    (RunLength: 1728; Code: $9B; Length: 09), // 0100 1101 1       1728   010011011
    (RunLength: 1792; Code: $08; Length: 11), // 0000 0001 000     1792  00000001000
    (RunLength: 1856; Code: $0C; Length: 11), // 0000 0001 100     1856  00000001100
    (RunLength: 1920; Code: $0D; Length: 11), // 0000 0001 101     1920  00000001101
    (RunLength: 1984; Code: $12; Length: 12), // 0000 0001 0010    1984  000000010010
    (RunLength: 2048; Code: $13; Length: 12), // 0000 0001 0011    2048  000000010011
    (RunLength: 2112; Code: $14; Length: 12), // 0000 0001 0100    2112  000000010100
    (RunLength: 2176; Code: $15; Length: 12), // 0000 0001 0101    2176  000000010101
    (RunLength: 2240; Code: $16; Length: 12), // 0000 0001 0110    2240  000000010110
    (RunLength: 2304; Code: $17; Length: 12), // 0000 0001 0111    2304  000000010111
    (RunLength: 2368; Code: $1C; Length: 12), // 0000 0001 1100    2368  000000011100
    (RunLength: 2432; Code: $1D; Length: 12), // 0000 0001 1101    2432  000000011101
    (RunLength: 2496; Code: $1E; Length: 12), // 0000 0001 1110    2496  000000011110
    (RunLength: 2560; Code: $1F; Length: 12), // 0000 0001 1111    2560  000000011111
    (RunLength: cnstMaxWord; Code: 1; Length: 12)
    );

  cnstModeCodes: TsgModeCodesTable = (
    (RunLength: Word(cwPass);       Code: $01; Length: 04), // 0001
    (RunLength: Word(cwHorizontal); Code: $01; Length: 03), // 001
    (RunLength: Word(cwV0);         Code: $01; Length: 01), // 1
    (RunLength: Word(cwVR1);        Code: $03; Length: 03), // 011
    (RunLength: Word(cwVR2);        Code: $03; Length: 06), // 0000 11
    (RunLength: Word(cwVR3);        Code: $03; Length: 07), // 0000 011
    (RunLength: Word(cwVL1);        Code: $02; Length: 03), // 010
    (RunLength: Word(cwVL2);        Code: $02; Length: 06), // 0000 10
    (RunLength: Word(cwVL3);        Code: $02; Length: 07), // 0000 010
    (RunLength: Word(cwExtension);  Code: $01; Length: 07), // 0000 001xxx
    (RunLength: Word(cwEOF);  Code: $1001; Length: 13),  //cnstEOF
    (RunLength: Word(cwEOL);  Code: $1;    Length: 12));//cnstEOL

  cnstExtensionMode: array [0..0] of TsgTerminalCode = (
    (RunLength: 1;       Code: $07; Length: 03)); // 111

  //  T color next run (black = 1, white = 0)
  cnstUncompressedMode: array [0..10] of TsgTerminalCode = (
    (RunLength: Word(uc1); Code: $01; Length: 01),      //  1
    (RunLength: Word(uc01); Code: $01; Length: 02),     //  01
    (RunLength: Word(uc001); Code: $01; Length: 03),    //  001
    (RunLength: Word(uc0001); Code: $01; Length: 04),   //  0001
    (RunLength: Word(uc00001); Code: $01; Length: 05),  //  00001
    (RunLength: Word(uc00000); Code: $01; Length: 06),  //  000001
    (RunLength: Word(ucEmpty); Code: $01; Length: 07),  //  0000001T
    (RunLength: Word(uc0); Code: $01; Length: 08),      //  00000001T
    (RunLength: Word(uc00); Code: $01; Length: 09),     //  000000001T
    (RunLength: Word(uc000); Code: $01; Length: 10),    //  0000000001T
    (RunLength: Word(uc0000); Code: $01; Length: 11));  //  00000000001T

{$IFDEF SG_DECOMPRESS_DEBUG}
var
  LogIndex: Integer;
{$ENDIF}

function IndexOfCodesTable(const ASize: Byte; const ACode: Word;
  const ATable: array of TsgTerminalCode): Integer;
begin
  Result := Low(ATable);
  while (Result <= High(ATable)) and
    (ACode shr (ASize - ATable[Result].Length) <> ATable[Result].Code) do
    Inc(Result);
  if Result > High(ATable) then
    Result := -1;
end;

function GetTerminalCodesTable(const ABlack: Boolean): TsgTerminalCodesTable;
begin
  if ABlack then
    Result := cnstTerminalCodesBlack
  else
    Result := cnstTerminalCodesWhite;
end;

function GetColor(const ABlack: Boolean): TColor;
begin
  if ABlack then
    Result := clBlack
  else
    Result := clWhite;
end;


{ TsgDecompressor }

procedure TsgDecompressor.ApplyPadding(const APadding: Integer);
var
  vLen: Integer;
begin
  FOneLen := FNumberOfCells;
  if APadding <> 0 then
  begin
    vLen := ((FOneLen.X div APadding) + Ord((FOneLen.X mod APadding)> 0)) * APadding;
    if vLen > FOneLen.X then
      FOneLen.X := vLen;
  end;
end;

constructor TsgDecompressor.Create;
begin
  inherited Create;
  FBitmap__ := {$IFDEF SG_FIREMONKEY}FMX.{$ENDIF}Graphics.TBitmap.Create;
{$IFDEF SG_DELPHI_VCL}
  FBitmap__.OnChange := BitmapChanged;
{$ENDIF}

  FRowIndex := -1;

  SetLength(FColors, 2);
  FColors[0] := clWhite;
  FColors[1] := clBlack;
  FNumberOfCells := Point(0, 0);
  FNumberOfTiles := Point(1, 1);

  FSDR := TMemoryStream.Create;
  FRunCountPrecision := 16;
end;

destructor TsgDecompressor.Destroy;
begin
  FRowIndex := -1;
  Finalize(FColors);
  Finalize(FScanRow);
  Finalize(FDrawRow);
  FBitmap__.Free;
  FSDR.Free;
  inherited Destroy;
end;

procedure TsgDecompressor.DoNextDrawLine;
begin
  DoNextScanLine;
  if FUseBuffer then
  begin
    SwapPointers(Pointer(FScanRow), Pointer(FDrawRow));
    if Length(FDrawRow) > 0 then
      FillChar(FDrawRow[0], Length(FDrawRow) * SizeOf(FDrawRow[0]), $FF);
  end;
end;

procedure TsgDecompressor.DoNextScanLine;
begin
  Inc(FRowIndex);
  if FRowIndex >= FBitmapData.Height then
    FRowIndex := -1;
end;

procedure TsgDecompressor.DrawLineWO(const X1, X2: Integer; const AColor: TColor);
begin
  DrawRowLine(FOffset.X + X1, FOffset.X + X2, AColor);
end;

procedure TsgDecompressor.DrawPointWO(const X: Integer; const AColor: TColor);
begin
  if (X >= 0) and (X < FOneLen.X) then
    DrawRowPoint(FOffset.X + X, AColor);
end;

procedure TsgDecompressor.DrawPixels(var X, Y: Integer; ALength,
  AColor: Integer; const AModeRC: Boolean);
var
  vStartX, vEndX: Integer;
begin
  case ALength of
    0: begin end;
    1:
      begin
        DrawPointWO(X, AColor);
        Inc(X);
        if AModeRC and (X = FOneLen.X) then
        begin
          X := 0;
          DoNextDrawLine;
          Inc(Y);
        end;
      end;
  else
    repeat
      vStartX := MaxI(X, 0);
      vEndX := MaxI(X + ALength, 0);;
      if AModeRC and (vEndX >= FOneLen.X) then
      begin
        vEndX := FOneLen.X - 1;
        DoNextDrawLine;
        Inc(Y);
      end;
      Dec(ALength, vEndX);
      DrawLineWO(vStartX, vEndX, AColor);
    until (ALength <= 0);
    X := vEndX;
  end;
end;

procedure TsgDecompressor.DrawRowLine(X1, X2: Integer; const AColor: TColor);
var
  I: Integer;
begin
  if FRowIndex < 0 then Exit;
  if FUseBuffer then
  begin
    X2 := Min(X2, High(FDrawRow));
    for I := X1 to X2 do
      FDrawRow[I] := AColor;
  end;
  SetPenColor(AColor);
  X2 := Min(X2, FBitmapData.Width - 1);
  for I := X1 to X2 do
{$IFDEF SG_DELPHI_VCL}
    FBitmapData.Pixels[I, FRowIndex] := FPenColor;
{$ELSE}
{$IFDEF SG_FIREMONKEY}
    FBitmapData.SetPixel(I, FRowIndex, GetAlphaColor(FPenColor));
{$ELSE}
    FBitmapData.TColors[I, FRowIndex] := FPenColor;
{$ENDIF}
{$ENDIF}
end;

procedure TsgDecompressor.DrawRowPoint(const X: Integer; AColor: TColor);
begin
  if FRowIndex < 0 then Exit;
  if FUseBuffer and (X <= High(FDrawRow)) then
    FDrawRow[X] := AColor;
  if X < FBitmapData.Width then
  begin
{$IFDEF SG_FIREMONKEY}
    FBitmapData.SetPixel(X, FRowIndex, GetAlphaColor(AColor));
{$ELSE}
{$IFNDEF SGFPC}
   FBitmapData.Pixels[X, FRowIndex] := AColor;
{$ELSE}
   FBitmapData.TColors[X, FRowIndex] := AColor;
{$ENDIF}
{$ENDIF}
  end;
end;

function TsgDecompressor.Decompress_BaselineJPEG: Boolean;
var
  vTemp: TMemoryStream;
  vJpeg: {$IFDEF SG_FIREMONKEY}TBitmap{$ELSE}TJPEGImage{$ENDIF};
  vBuffer: Byte;
  vRect: TRect;
begin
  try
    vTemp := TMemoryStream.Create;
    try
      while not FReader.IsEndCommand do
      begin
        vBuffer := FReader.ReadByteCount(1);
        vTemp.Write(vBuffer,1);
      end;
      vTemp.Position := 0;
      vJpeg := {$IFDEF SG_FIREMONKEY}TBitmap{$ELSE}TJPEGImage{$ENDIF}.Create;
      try
        vJpeg.LoadFromStream(vTemp);
        vRect.TopLeft := FOffset;
        vRect.BottomRight.X := FOffset.X+vJpeg.Width;
        vRect.BottomRight.Y := FOffset.Y+vJpeg.Height;
        FBitmap__.PixelFormat := pf24bit;
        FBitmap__.Canvas.StretchDraw(vRect,vJpeg);
        //FBitmap__.Assign(vJpeg);
        Result := True;
      finally
        vJpeg.Free;
      end;
    finally
      vTemp.Free;
    end;
  except
    Result := False;
  end;
end;

function OpenPngByMemmoryStream(const AStream: TStream): TGraphic;
{$IFDEF SG_IMPORT_INCORRECT_PNG}
var
  vPng: TsgPngImage;
begin
  Result := nil;
  try
    AStream.Position := 0;
    vPng := TsgPngImage.Create;
    try
      vPng.LoadFromStream(AStream);
      Result := TPngImage.Create;
      Result.Assign(vPng);
    finally
      vPng.Free;
    end;
  except
  end;
{$ELSE}
begin
  Result := nil;
{$ENDIF}
end;

function TsgDecompressor.Decompress_BaselinePNG: Boolean;
const
  cnstFileName = 'decompressimage';
var
  vImage: TPersistent;
  vPng: {$IFDEF SG_FIREMONKEY}TBitmap{$ELSE}TGraphic{$ENDIF};
  vBuffer: Byte;
  vRect: TRect;
  vRez: Integer;
begin
  Result := False;
  vPng := nil;
  try
    while not FReader.IsEndCommand do
    begin
      vBuffer := FReader.ReadByteCount(1);
      FSDR.Write(vBuffer,1);
    end;
    FSDR.Position := 0;
    vRez := 0;
    vImage := TPersistent(OpenPictureByMemmoryStreamSafe(FSDR, cnstFileName, 'png', @vRez));
    if (not Assigned(vImage)) and (vRez = -1) then
      vImage := OpenPngByMemmoryStream(FSDR);
    if vImage <> nil then
    begin
      try
        if vImage is {$IFDEF SG_FIREMONKEY}TBitmap{$ELSE}TGraphic{$ENDIF} then
          TObject(vPng) := vImage;
        if Assigned(vPng) then
        begin
          vRect.TopLeft := FOffset;
          vRect.BottomRight.X := FOffset.X+vPng.Width;
          vRect.BottomRight.Y := FOffset.Y+vPng.Height;

          {$IFNDEF SG_FIREMONKEY}
          FBitmap__.PixelFormat := pf24bit;
          FBitmap__.Canvas.StretchDraw(vRect,vPng);
          {$ELSE}
          if FBitmap__.Canvas.BeginScene then
          begin
            try
              FBitmap__.Canvas.StretchDraw(vRect,vPng);
            finally
              FBitmap__.Canvas.EndScene;
            end;
          end;
         {$ENDIF}
          Result := True;
        end;
      finally
        vPng.Free;
      end;
    end;
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
end;

function TsgDecompressor.Decompress_Uncompress: Boolean;
begin
  Result := ReadBitmap(False);
end;

function TsgDecompressor.Decompress_RLE: Boolean;
begin
  Result := ReadBitmap(True);
end;

function TsgDecompressor.Decompress_T4_1D: Boolean;
begin
  Result := False;
end;

function TsgDecompressor.Decompress_T4_2D: Boolean;
begin
  Result := False;
end;

function TsgDecompressor.Decompress_T6: Boolean;
{$IFDEF SG_DECOMPRESS_DEBUG}
const
  cnstSCodeWord: array [TsgCgmT6_CodeWord] of string = ('U', 'P', 'H', 'V0',
    'VR1', 'VR2', 'VR3', 'VL1', 'VL2', 'VL3', 'E', 'EOF', 'EOL');
{$ENDIF}
var
  Y: Integer;
  a: array [0..2] of Integer;
  b: array [1..2] of Integer;
  vCodeWord: TsgCgmT6_CodeWord;
  vUncompressCode: TsgCgmT6_UncompressedCode;
  vRunLengthOne, vRunLengthTwo: Integer;
  vExtensionMode: Integer;
  vActiveColor, vActiveColorSave: Boolean;//false - white, true  - black;
{$IFDEF SG_DECOMPRESS_DEBUG}
  vLog: TStringList;
  S: string;
{$ENDIF}

  function Draw(const ARunLength: Word; const AColor: TColor; const AModeRC: Boolean): Boolean;
  begin
    if a[0] < 0 then
      Inc(a[0]);
    DrawPixels(a[0], Y, ARunLength, AColor, AModeRC);
    Result := a[0] >= FOneLen.X;
    if Result then
    begin
      a[0] := -1;
      vActiveColor := False;
      DoNextDrawLine;
      Inc(Y);
{$IFDEF SG_DECOMPRESS_DEBUG}
      vLog.Add('---('+ IntToStr(Y) +')---');
{$ENDIF}
    end;
  end;

begin
  Result := True;
  vActiveColor := False;
  a[0] := -1;
  a[1] := -1;
  a[2] := -1;
  b[1] := -1;
  b[2] := -1;
  Y := 0;

{$IFDEF SG_DECOMPRESS_DEBUG}
  vLog := TStringList.Create;
{$ENDIF}
  FReader.BeginReadBits;
  try
    while not FReader.IsEndCommand do
    begin
      vCodeWord := ReadCodeWord;
{$IFDEF SG_DECOMPRESS_DEBUG}
      vLog.Add(cnstSCodeWord[vCodeWord]);
{$ENDIF}

      if a[0] < 0 then
        vActiveColor := False;
      case vCodeWord of
        cwExtension:
          begin
            vExtensionMode := GetUncompressedMode;
            if vExtensionMode = 1 then
            begin
              {$IFNDEF SGDEL_10_SEATTLE}
              vUncompressCode := ucError;
              {$ENDIF}
              while True do
              begin
                vUncompressCode := GetUncompressedCode;
                case vUncompressCode of
                  uc1: Draw(1,clBlack,True);
                  uc01:
                    begin
                      Draw(1,clWhite,True); Draw(1,clBlack,True);
                    end;
                  uc001:
                    begin
                      Draw(2,clWhite,True); Draw(1,clBlack,True);
                    end;
                  uc0001:
                    begin
                      Draw(3,clWhite,True); Draw(1,clBlack,True);
                    end;
                  uc00001:
                    begin
                      Draw(4,clWhite,True); Draw(1,clBlack,True);
                    end;
                  uc00000: Draw(5,clWhite,True);
                  ucEmpty..ucError: Break;
                end;
              end;

              if vUncompressCode = ucError then
                Break;

              case vUncompressCode of
                ucEmpty: ;
                uc0: Draw(1,clWhite,True);
                uc00: Draw(2,clWhite,True);
                uc000: Draw(3,clWhite,True);
                uc0000: Draw(4,clWhite,True);
              end;

              vActiveColor := Boolean(ReadBit);
            end
            else
              Break;
              
          end;
        cwPass:
          begin
            b[2] := GetB2(a[0], vActiveColor);
            if b[2] > -1 then
            begin
              if a[0] >= 0 then
                Draw(b[2] - a[0], GetColor(vActiveColor), False)
              else
                begin
                  if b[2] >= 1 then
                    Draw(b[2], GetColor(vActiveColor), False)
                  else
                    Draw(0, GetColor(vActiveColor), False);
                end;
            end;
          end;
        cwHorizontal:
          begin
{$IFDEF SG_DECOMPRESS_DEBUG}
            S := '   ';
{$ENDIF}
            vActiveColorSave := vActiveColor;

            vRunLengthOne := ReadRunLength(GetTerminalCodesTable(vActiveColorSave));
{$IFDEF SG_DECOMPRESS_DEBUG}
              if vActiveColorSave then
                S := S + ' b=' + IntToStr(vRunLengthOne)
              else
                S := S + ' w=' + IntToStr(vRunLengthOne);
{$ENDIF}
            vRunLengthTwo := ReadRunLength(GetTerminalCodesTable(not vActiveColorSave));
{$IFDEF SG_DECOMPRESS_DEBUG}
              if not vActiveColorSave then
                S := S + ' b=' + IntToStr(vRunLengthTwo)
              else
                S := S + ' w=' + IntToStr(vRunLengthTwo);
{$ENDIF}
            if not Draw(vRunLengthOne, GetColor(vActiveColorSave), False) then
            begin
              if not Draw(vRunLengthTwo, GetColor(not vActiveColorSave), False) then
                vActiveColor := vActiveColorSave;
            end;

{$IFDEF SG_DECOMPRESS_DEBUG}
            vLog.Add(S);
{$ENDIF}
          end;
        cwV0..cwVL3:
          begin
            b[1] := GetB1(a[0], vActiveColor);
            if b[1] > -1 then
            begin
              a[1] := GetA1(b[1], vCodeWord);
              if a[0] >= 0 then
                Draw(a[1] - a[0], GetColor(vActiveColor), False)
              else
                begin
                  if a[1] >= 1 then
                    Draw(a[1], GetColor(vActiveColor), False)
                  else
                    Draw(0, GetColor(vActiveColor), False);
                end;

              vActiveColor := not vActiveColor;
              (*
              case vCodeWord of
                cwV0, cwVL1..cwVL3: vActiveColor := not vActiveColor;
              end;
              *)
            end;
          end;
       cwEOF, cwEOL:
           Break;
      else//cwUncode
        Break;
      end;
    end;
{$IFDEF SG_DECOMPRESS_DEBUG}
    vLog.SaveToFile('D:\LogCGM\Log'+IntToStr(LogIndex)+'.txt');
    Inc(LogIndex);
{$ENDIF}
  finally
    FReader.EndReadBits;
{$IFDEF SG_DECOMPRESS_DEBUG}
    vLog.Free;
{$ENDIF}
  end;
end;

procedure TsgDecompressor.ReadSDR;
var
  I, vCount: Integer;
  vBuffer: Byte;
const
  cnstPNGHeader: array[0..7] of Byte = ($89,$50,$4E,$47,$0D,$0A,$1A,$0A);
  cnstMagicPNG = 30;
begin
  FSDR.Clear;
  vCount := FReader.ReadSDR;
  if vCount = 0 then
    Exit;
  case FCompression of
    ctPNGCopmM0:
      begin
        // test file
        // SDR 56 byte
        //ICN-1BAAJ2045C0117B09461A011.CGM - 56 byte
        //ICN-1BAAJ2045C0117B09462A011.CGM
        // SDR 30 byte
        //ICN-1BAZZ4210DS314700366A021.CGM
        //ICN-1BAZZ4210DS314700367A021.CGM
        // PNG header
        if vCount >= cnstMagicPNG then
        begin
          for I := Low(cnstPNGHeader) to High(cnstPNGHeader) do
          begin
            vBuffer := cnstPNGHeader[I];
            FSDR.Write(vBuffer,1);
          end;
          // ???
          FReader.ReadByteCount(4);
          // IHDR
          for I := 0 to cnstMagicPNG - 4 - 1 do
          begin
            vBuffer := FReader.ReadByteCount(1);
            FSDR.Write(vBuffer,1);
          end;
          // ???
          FReader.ReadByteCount(1);
          for I := 0 to vCount - cnstMagicPNG -1 do
          begin
            vBuffer := FReader.ReadByteCount(1);
          end;
        end
        else
          for I := 0 to vCount - 1 do
          begin
           vBuffer := FReader.ReadByteCount(1);
          end;
      end;
    ctRLE:
      begin
        FReader.ReadBits(16);
        FReader.ReadBits(16);
        FRunCountPrecision := FReader.ReadBits(16);
        if FRunCountPrecision > 32 then
          FRunCountPrecision := 16;
      end
    else
      begin
        for I := 0 to vCount - 1 do
        begin
         vBuffer := FReader.ReadByteCount(1);
         FSDR.Write(vBuffer,1);
        end;
      end;
  end;
end;

function TsgDecompressor.Execute(const AOffset: TPoint;
  const AReader: TsgCustomMemoryStream; const AReadSDR: Boolean = False): Boolean;
{$IFDEF SG_FIREMONKEY}
var
  vMap: Boolean;
{$ENDIF}
begin
  Result := False;
  FOffset := AOffset;
  FReader := AReader;
{$IFDEF SG_FIREMONKEY}
  FBitmap__.Canvas.Stroke.Thickness := 1;
  FBitmap__.Canvas.Stroke.Dash := TStrokeDash.Solid;
  //FBitmap__.Clear(clWhite);
  vMap := False;
  if not (FCompression in [ctBaselineJPEG, ctPNGCopmM0]) then
    vMap := FBitmap__.Map(TMapAccess.ReadWrite, FBitmapData);
{$ELSE}
  FBitmap__.Canvas.Pen.Width := 1;
  FBitmap__.Canvas.Pen.Style := psSolid;
  FBitmap__.Canvas.Pen.Mode := pmCopy;
{$IFDEF SGFPC}
  FBitmapData := FBitmap__.CreateIntfImage;
{$ELSE}
  FBitmapData := TsgCGMMap.Wrap(FBitmap__);
{$ENDIF}
{$ENDIF}
  try
    FPenColor := FBitmap__.Canvas.Pen.Color;
{$IFDEF SG_FIREMONKEY}
    //FBitmapData := FMX.Graphics.TBitmapData.Create(FBitmap__.Width, FBitmap__.Height, FMX.Types.TPixelFormat.RGB);
    FPixelFormat := FBitmapData.PixelFormat;
{$ELSE}
    FPixelFormat := FBitmap__.PixelFormat;
{$ENDIF}
    // SDR
    if AReadSDR then
      ReadSDR;

    FRowIndex := FOffset.Y - 1;
    DoNextScanLine;
    FUseBuffer := FCompression in [ctT6];
    if FCompression in [ctT6] then
    begin
      SetLength(FDrawRow, FBitmap__.Width + FOneLen.X);
      FillRow(FDrawRow, clWhite);
      SetLength(FScanRow, Length(FDrawRow) * SizeOf(FDrawRow[0]));
      System.Move(FDrawRow[0], FScanRow[0], Length(FDrawRow) * SizeOf(FDrawRow[0]));
    end;
    case FCompression of
      ctUncompressed:    Result := Decompress_Uncompress;
      ctRLE:             Result := Decompress_RLE;
      ctT4_1Dimensional: Result := Decompress_T4_1D;
      ctT4_2Dimensional: Result := Decompress_T4_2D;
      ctT6:              Result := Decompress_T6;
      ctBaselineJPEG:    Result := Decompress_BaselineJPEG;
      ctPNGCopmM0:       Result := Decompress_BaselinePNG;
    end;
  finally
    FReader := nil;
{$IFDEF SG_FIREMONKEY}
    if vMap then
      FBitmap__.Unmap(FBitmapData);
{$ELSE}
    {$IFDEF SGFPC}
    if not (FCompression in [ctBaselineJPEG, ctPNGCopmM0]) then
      FBitmap__.LoadFromIntfImage(FBitmapData);
   {$ENDIF}
{$ENDIF}
  end;
{$IFDEF SGFPC}
  FreeAndNil(FBitmapData);
{$ELSE}
{$IFNDEF SG_FIREMONKEY}
  TsgCGMMap.Unwrap(FBitmapData);
{$ENDIF}
{$ENDIF}
end;

procedure TsgDecompressor.FillRow(var ARow: array of Integer; const AColor: TColor);
var
  I: Integer;
begin
  for I := Low(ARow) to High(ARow) do
    ARow[I] := AColor;
end;

{$IFDEF SG_FIREMONKEY}
function TsgDecompressor.GetAlphaColor(AColor: TColor): Integer;
begin
  Result := AColor.AsBGRA;
end;
{$ENDIF}

function TsgDecompressor.GetBackground: TColor;
begin
  Result := FColors[0];
end;

function TsgDecompressor.GetForeground: TColor;
begin
  Result :=  FColors[1];
end;

function TsgDecompressor.GetHeight: Integer;
begin
  Result := FNumberOfCells.Y * FNumberOfTiles.Y;
end;

function TsgDecompressor.GetA1(const B1: Integer;
  const ACodeWord: TsgCgmT6_CodeWord): Integer;
begin
  Result := B1;
  case ACodeWord of
    cwVL1..cwVL3:  Dec(Result, Integer(ACodeWord) - Integer(cwVL1) + 1);
    cwVR1..cwVR3:  Inc(Result, Integer(ACodeWord) - Integer(cwVR1) + 1);
  end;
end;

function TsgDecompressor.GetB1(const A0: Integer; const ActiveColor: Boolean): Integer;
var
  vColor: TColor;
begin
  if FRowIndex > FOffset.Y then
  begin
    vColor := GetColor(ActiveColor);
    Result := A0;
    while (Result < FOneLen.X) and (GetScanPointWO(Result) <> vColor) do
      Inc(Result);
    if Result < FOneLen.X then
    begin
      while (Result <= FOneLen.X) and (GetScanPointWO(Result) = vColor) do
        Inc(Result);
    end;
  end
  else
    Result := FOneLen.X;
end;

function TsgDecompressor.GetB2(const A0: Integer; const ActiveColor: Boolean): Integer;
begin
  Result := GetB1(A0, ActiveColor);
  if (Result >= 0) and (Result < FOneLen.X) then
    Result := GetB1(Result, not ActiveColor);
//  else
//    Result := -1;
end;

function TsgDecompressor.GetNumberOfCells: TPoint;
begin
  Result := FNumberOfCells;
end;

function TsgDecompressor.GetNumberOfTiles: TPoint;
begin
  Result := FNumberOfTiles
end;

function TsgDecompressor.GetScanPointWO(const X: Integer): TColor;
begin
  if (X >= 0) and (X < FOneLen.X) then
    Result := GetScanRowPixels(FOffset.X + X)
  else
  begin
    Result := clWhite;
    if (X > 0) and (GetScanRowPixels(FOffset.X + FOneLen.X - 1) = clWhite) then
      Result := clBlack;
  end;
end;

function TsgDecompressor.GetScanRowPixels(const X: Integer): TColor;
begin
  if (X > -1) and (X <= High(FScanRow)) then
    Result := TColor(FScanRow[X])
  else
    Result := Background;
end;

function TsgDecompressor.GetUncompressedCode: TsgCgmT6_UncompressedCode;
var
  I: Integer;
begin
  Result := ucError;
  I := IndexOfCodesTable(16, FReader.GetBits(16), cnstUncompressedMode);
  if I > -1 then
  begin
    Result := TsgCgmT6_UncompressedCode(cnstUncompressedMode[I].RunLength);
    FReader.SeekBits(cnstUncompressedMode[I].Length);
  end;
end;

function TsgDecompressor.GetUncompressedMode: Integer;
var
  I: Integer;
begin
  Result := -1;
  I := IndexOfCodesTable(3, FReader.GetBits(8), cnstExtensionMode);
  if I > -1 then
  begin
    Result := cnstExtensionMode[I].RunLength;
    FReader.SeekBits(cnstExtensionMode[I].Length);
  end
  else
  begin
    FReader.SeekBits(3);
  end;
end;

function TsgDecompressor.GetWidth: Integer;
begin
  Result := FNumberOfCells.X * FNumberOfTiles.X;
end;

procedure TsgDecompressor.AfterDecompress;
begin
//  FBitmap.Width := FBitmap.Width - cnstRawWidth;
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

function TsgDecompressor.BeforeDecompress(const AFormat: TPixelFormat): Boolean;
begin
{$IFDEF SG_DECOMPRESS_DEBUG}
  LogIndex:= 0;
{$ENDIF}
  Result := False;
  FBitmap__.PixelFormat := AFormat;
  try
    SetSizeBmp(FBitmap__, FNumberOfCells.X * FNumberOfTiles.X, FNumberOfCells.Y * FNumberOfTiles.Y);
    Result := not Result;//Result := True;
  except
    on EOutOfMemory do
    begin
      Result := False;
      SetSizeBmp(FBitmap__, 1, 1);
      FCompression := ctReserved;
    end;
  end;
end;

procedure TsgDecompressor.BitmapChanged(Sender: TObject);
begin
{$IFDEF SG_DELPHI_VCL}
  if Assigned(FBitmapData) then
  begin
    TsgMap.Unwrap(FBitmapData);
    FBitmapData := TsgMap.Wrap(FBitmap__);
  end;
{$ENDIF}
end;

procedure TsgDecompressor.ReadAlignBytes;
begin
  if Assigned(FProcReadAlignBytes) then
    FProcReadAlignBytes;
end;

function TsgDecompressor.ReadBit: Integer;
begin
  Result := FReader.ReadBit;
end;

function TsgDecompressor.ReadBitmap(const AIsRLE: Boolean): Boolean;
var
  X, Y, vLength, vColor: Integer;
begin
  Result := True;
  try
    Y := 0;
    while Y < FOneLen.Y  {FNumberOfCells.Y} do
    begin
      FReader.BeginReadBits;
      try
        X := 0;
        while X < FOneLen.X {FNumberOfCells.X} do
        begin
          vLength := 1;
          if AIsRLE then
            vLength := ReadColorLength;
          vColor := ReadColor;
          DrawPixels(X, Y, vLength, vColor, False);
        end;
      finally
        DoNextDrawLine;
        FReader.EndReadBits;
      end;
      ReadAlignBytes;
      Inc(Y);
    end;
  except
    Result := False;
  end;
end;

function TsgDecompressor.ReadCodeWord: TsgCgmT6_CodeWord;
var
  I: Integer;
begin
  Result := cwUncode;
  I := IndexOfCodesTable(16, FReader.GetBits(16), cnstModeCodes);
  if I > -1 then
  begin
    Result := TsgCgmT6_CodeWord(cnstModeCodes[I].RunLength);
    FReader.SeekBits(cnstModeCodes[I].Length);
  end;
end;

function TsgDecompressor.ReadColor: Cardinal;
var
  vIndex: Integer;
begin
  if Assigned(FProcReadColor) then
    Result := FProcReadColor
  else
  begin
    vIndex := FReader.ReadBits(1);
    if (vIndex >= 0) and (vIndex <= High(FColors)) then
      Result := Cardinal(FColors[vIndex])
    else
      Result := Background;
  end;
end;

function TsgDecompressor.ReadColorLength: Integer;
begin
  Result := FReader.ReadBits(FRunCountPrecision);
end;

function TsgDecompressor.ReadRunLength(const ATable: TsgTerminalCodesTable): Integer;
var
  I: Integer;
  vTerminalCode: TsgTerminalCode;
begin
  Result := 0;
  repeat
    vTerminalCode.RunLength := 0;
    I := IndexOfCodesTable(16, FReader.GetBits(16), ATable);
    if I > -1 then
    begin
      vTerminalCode := ATable[I];
      FReader.SeekBits(vTerminalCode.Length);
      Inc(Result, vTerminalCode.RunLength);
    end;
  until vTerminalCode.RunLength < 64;
end;

procedure TsgDecompressor.SetBackground(const Value: TColor);
begin
   FColors[0] := Value;
end;

procedure TsgDecompressor.SetCompression(const Value: TsgCgmCompressionType);
begin
  FCompression := Value;
end;

procedure TsgDecompressor.SetForeground(const Value: TColor);
begin
  FColors[1] := Value;
end;

procedure TsgDecompressor.SetNumberOfCells(const Value: TPoint);
begin
  FNumberOfCells := Value;
  FOneLen := Value;
end;

procedure TsgDecompressor.SetNumberOfTiles(const Value: TPoint);
begin
  FNumberOfTiles := Value;
end;

procedure TsgDecompressor.SetPenColor(const AColor: TColor);
begin
{$IFDEF SG_FIREMONKEY}
  FPenColor := AColor;
{$ELSE}
  if FPenColor <> AColor then
  begin
    FPenColor := AColor;
{$IFNDEF SGFPC}
    FBitmap__.Canvas.Pen.Color := FPenColor;
{$ENDIF}
  end;
{$ENDIF}
end;

function TsgDecompressor.SetPixelFormat(const AFormat: TPixelFormat): Boolean;

var
  vCurrentPixelFormat: TPixelFormat;
begin
  //Set only to increase the bits by color.
  Result := False;
  try
    vCurrentPixelFormat := FBitmap__.PixelFormat;
    if Integer(vCurrentPixelFormat) >= Integer(AFormat) then
      Exit;
    FBitmap__.PixelFormat := AFormat;
    Result := not Result;
  except
    on EOutOfMemory do
    begin
      Result := False;
      FCompression := ctReserved;
    end;
  end;
end;

{ TsgCustomMemoryStream }

procedure TsgCustomMemoryStream.BeginReadBits;
begin
   FReadBitsCount := 0;
   FReadBitsBuffer := GetByteCount(1);
end;

procedure TsgCustomMemoryStream.EndReadBits;
begin
  if FReadBitsCount > 0 then
    Position := Position + 1;
  FReadBitsCount := 0;
  FReadBitsBuffer := 0;
end;

function TsgCustomMemoryStream.GetBits(const ASize: Integer): Integer;
begin
  Result := (GetByteCount(4) shl FReadBitsCount) shr (32 - ASize);
end;

function TsgCustomMemoryStream.GetByteCount(const ASize: Integer): Integer;
begin
  SavePosition;
  try
    Result := ReadByteCount(ASize);
  finally
    RestorePosition;
  end;
end;

function TsgCustomMemoryStream.ReadBit: Integer;
begin
  Result := Integer(FReadBitsBuffer and 128 <> 0);
  FReadBitsBuffer := FReadBitsBuffer shl 1;
  Inc(FReadBitsCount);
  if FReadBitsCount >= 8 then
  begin
    ReadByteCount(1);
    FReadBitsCount := 0;
    FReadBitsBuffer := GetByteCount(1);
  end;
end;

function TsgCustomMemoryStream.ReadBits(const ASize: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (FReadBitsCount = 0) and (ASize mod 8 = 0) then
  begin
    Result := ReadByteCount(ASize div 8);
    FReadBitsBuffer := GetByteCount(1);
  end
  else
  begin
    for I := 0 to ASize - 1 do
      Result := (Result shl 1) or ReadBit;
  end;
end;

function TsgCustomMemoryStream.ReadSDR: Integer;
begin
  Result := 0;
end;

procedure TsgCustomMemoryStream.RestorePosition;
begin
  Position := FSavePosition;
end;

procedure TsgCustomMemoryStream.SavePosition;
begin
  FSavePosition := Position;
end;

procedure TsgCustomMemoryStream.SeekBits(const ASeek: Integer);
begin
  ReadBits(ASeek);
end;

{$IFDEF SG_IMPORT_INCORRECT_PNG}

{ TsgPngImage }

procedure TsgPngImage.DoCorrectPngData;
var
  vBmp: TBitmap;
begin
  vBmp := TBitmap.Create;
  try
    vBmp.Assign(self);
    Self.Assign(vBmp);
  finally
    vBmp.Free;
  end;
end;

procedure TsgPngImage.LoadFromStream(Stream: TStream);
begin
  try
    inherited LoadFromStream(Stream);
  except
    on E: EPNGUnexpectedEnd do DoCorrectPngData;
  end;
end;
{$ENDIF}

{ TsgCGMMap }

function TsgCGMMap.GetPalIndex(AColor: TColor): Integer;
begin
  GetNearestPalIndex(AColor, Pal, Result);
end;

end.

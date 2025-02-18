{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{             Convetion string to WideString tool            }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgUnicode;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFDEF SG_FIREMONKEY}
  sgFMXTypes,
{$ELSE}
{$ENDIF}
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
  {$ELSE}
  sgConsts,
  {$ENDIF}
  {$IFDEF SGFPC}
  LCLIntf, LCLType,
  {$ENDIF}
  SysUtils, Classes
{$IFDEF SGDEL_XE4}
  ,System.Character
{$ENDIF}
{$IFNDEF SGDEL_2009}
  ,sgFunction
{$ENDIF}
  ;

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

function GetDWGCodePageByName(const AName: string): Integer;
function GetDWGCPNameByCodePage(ACodePage: Cardinal): string;
function StrToUnicode(CodePage: Integer; const S: string): WideString;

type
  TCodePageIdTable = class
    private
      FPrimaryIds: TStringList;
      FSecondaryIds: TStringList;
    protected
      function IndexOfCodePage(const AValue: Integer; const AList: TStringList): Integer;
      function GetValueFromObject(const AObject: TObject): Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
      function GetObjectFromValue(const AValue: Integer): TObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
      procedure InitializeIds;
    public
      constructor Create;
      destructor Destroy; override;
      function AddId(ACodePage: Integer; const APrimaryName: string;
        const ASecondaryNames: array of string): Boolean;
      function GetCodePage(const AName: string): Integer;
      function GetName(const ACodePage: Integer): string;
      function IsExists(const AName: string): Boolean;
  end;

var
  CodePageIdTable: TCodePageIdTable;

implementation
type
  PCodeTable = ^TCodeTable;
  TCodeTable = array[Byte] of Word;

const
  cnstANSI = 'ANSI';
  cnstDOS = 'DOS';
  cnstISO = 'ISO';
  cnstBIG5 = 'BIG5';
  cnstJOHAB = 'JOHAB';
  cnstGB2312 = 'GB2312';
  cnstASCII = 'ASCII';
  cnstUTF16 = 'UTF-16';
  cnstKSC5601 = 'KSC5601';
  cnstMacintosh = 'MAC-ROMAN';
  cnstWindows = 'WINDOWS';
  CP_ISO_BASE = 28590;
  cnstISO8859 = 8859;

  CP874: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $00, $00, $00, $2026,
     $00, $00, $00, $00, $00, $00, $00, $00, $00,
     $00, $00, $2018, $2019, $201C, $201D, $2022,
     $2013, $2014, $00, $00, $00, $00, $00, $00,
     $00, $00, $A0, $E01, $E02, $E03, $E04, $E05,
     $E06, $E07, $E08, $E09, $E0A, $E0B, $E0C, $E0D,
     $E0E, $E0F, $E10, $E11, $E12, $E13, $E14, $E15,
     $E16, $E17, $E18, $E19, $E1A, $E1B, $E1C, $E1D,
     $E1E, $E1F, $E20, $E21, $E22, $E23, $E24, $E25,
     $E26, $E27, $E28, $E29, $E2A, $E2B, $E2C, $E2D,
     $E2E, $E2F, $E30, $E31, $E32, $E33, $E34, $E35,
     $E36, $E37, $E38, $E39, $E3A, $00, $00, $00,
     $00, $E3F, $E40, $E41, $E42, $E43, $E44, $E45,
     $E46, $E47, $E48, $E49, $E4A, $E4B, $E4C, $E4D,
     $E4E, $E4F, $E50, $E51, $E52, $E53, $E54, $E55,
     $E56, $E57, $E58, $E59, $E5A, $E5B, $00, $00,
     $00, $00);

  Win1250: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $00, $201E, $2026,
     $2020, $2021, $00, $2030, $160, $2039, $15A,
     $164, $17D, $179, $00, $2018, $2019, $201C,
     $201D, $2022, $2013, $2014, $00, $2122, $161,
     $203A, $15B, $165, $17E, $17A, $A0, $2C7, $2D8,
     $141, $A4, $104, $A6, $A7, $A8, $A9, $15E,
     $AB, $AC, $AD, $AE, $17B, $B0, $B1, $2DB, $142,
     $B4, $B5, $B6, $B7, $B8, $105, $15F, $BB, $13D,
     $2DD, $13E, $17C, $154, $C1, $C2, $102, $C4,
     $139, $106, $C7, $10C, $C9, $118, $CB, $11A,
     $CD, $CE, $10E, $110, $143, $147, $D3, $D4,
     $150, $D6, $D7, $158, $16E, $DA, $170, $DC,
     $DD, $162, $DF, $155, $E1, $E2, $103, $E4,
     $13A, $107, $E7, $10D, $E9, $119, $EB, $11B,
     $ED, $EE, $10F, $111, $144, $148, $F3, $F4,
     $151, $F6, $F7, $159, $16F, $FA, $171, $FC,
     $FD, $163, $2D9);
  Win1251: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $402, $403, $201A, $453, $201E, $2026,
     $2020, $2021, $20AC, $2030, $409, $2039, $40A,
     $40C, $40B, $40F, $452, $2018, $2019, $201C,
     $201D, $2022, $2013, $2014, $00, $2122, $459,
     $203A, $45A, $45C, $45B, $45F, $A0, $40E, $45E,
     $408, $A4, $490, $A6, $A7, $401, $A9, $404,
     $AB, $AC, $AD, $AE, $407, $B0, $B1, $406, $456,
     $491, $B5, $B6, $B7, $451, $2116, $454, $BB,
     $458, $405, $455, $457, $410, $411, $412, $413,
     $414, $415, $416, $417, $418, $419, $41A, $41B,
     $41C, $41D, $41E, $41F, $420, $421, $422, $423,
     $424, $425, $426, $427, $428, $429, $42A, $42B,
     $42C, $42D, $42E, $42F, $430, $431, $432, $433,
     $434, $435, $436, $437, $438, $439, $43A, $43B,
     $43C, $43D, $43E, $43F, $440, $441, $442, $443,
     $444, $445, $446, $447, $448, $449, $44A, $44B,
     $44C, $44D, $44E, $44F);
  Win1252: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $192, $201E, $2026,
     $2020, $2021, $2C6, $2030, $160, $2039, $152,
     $00, $17D, $00, $00, $2018, $2019, $201C, $201D,
     $2022, $2013, $2014, $2DC, $2122, $161, $203A,
     $153, $00, $17E, $178, $A0, $A1, $A2, $A3,
     $A4, $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC,
     $AD, $AE, $AF, $B0, $B1, $B2, $B3, $B4, $B5,
     $B6, $B7, $B8, $B9, $BA, $BB, $BC, $BD, $BE,
     $BF, $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7,
     $C8, $C9, $CA, $CB, $CC, $CD, $CE, $CF, $D0,
     $D1, $D2, $D3, $D4, $D5, $D6, $D7, $D8, $D9,
     $DA, $DB, $DC, $DD, $DE, $DF, $E0, $E1, $E2,
     $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA, $EB,
     $EC, $ED, $EE, $EF, $F0, $F1, $F2, $F3, $F4,
     $F5, $F6, $F7, $F8, $F9, $FA, $FB, $FC, $FD,
     $FE, $FF);
  Win1253: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $192, $201E, $2026,
     $2020, $2021, $00, $2030, $00, $2039, $00,
     $00, $00, $00, $00, $2018, $2019, $201C, $201D,
     $2022, $2013, $2014, $00, $2122, $00, $203A,
     $00, $00, $00, $00, $A0, $385, $386, $A3, $A4,
     $A5, $A6, $A7, $A8, $A9, $00, $AB, $AC, $AD,
     $AE, $2015, $B0, $B1, $B2, $B3, $384, $B5,
     $B6, $B7, $388, $389, $38A, $BB, $38C, $BD,
     $38E, $38F, $390, $391, $392, $393, $394, $395,
     $396, $397, $398, $399, $39A, $39B, $39C, $39D,
     $39E, $39F, $3A0, $3A1, $00, $3A3, $3A4, $3A5,
     $3A6, $3A7, $3A8, $3A9, $3AA, $3AB, $3AC, $3AD,
     $3AE, $3AF, $3B0, $3B1, $3B2, $3B3, $3B4, $3B5,
     $3B6, $3B7, $3B8, $3B9, $3BA, $3BB, $3BC, $3BD,
     $3BE, $3BF, $3C0, $3C1, $3C2, $3C3, $3C4, $3C5,
     $3C6, $3C7, $3C8, $3C9, $3CA, $3CB, $3CC, $3CD,
     $3CE, $00);
  Win1254: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $192, $201E, $2026,
     $2020, $2021, $2C6, $2030, $160, $2039, $152,
     $00, $00, $00, $00, $2018, $2019, $201C, $201D,
     $2022, $2013, $2014, $2DC, $2122, $161, $203A,
     $153, $00, $00, $178, $A0, $A1, $A2, $A3, $A4,
     $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD,
     $AE, $AF, $B0, $B1, $B2, $B3, $B4, $B5, $B6,
     $B7, $B8, $B9, $BA, $BB, $BC, $BD, $BE, $BF,
     $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8,
     $C9, $CA, $CB, $CC, $CD, $CE, $CF, $11E, $D1,
     $D2, $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA,
     $DB, $DC, $130, $15E, $DF, $E0, $E1, $E2, $E3,
     $E4, $E5, $E6, $E7, $E8, $E9, $EA, $EB, $EC,
     $ED, $EE, $EF, $11F, $F1, $F2, $F3, $F4, $F5,
     $F6, $F7, $F8, $F9, $FA, $FB, $FC, $131, $15F,
     $FF);
  Win1255: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $192, $201E, $2026,
     $2020, $2021, $2C6, $2030, $00, $2039, $00,
     $00, $00, $00, $00, $2018, $2019, $201C, $201D,
     $2022, $2013, $2014, $2DC, $2122, $00, $203A,
     $00, $00, $00, $00, $A0, $A1, $A2, $A3, $20AA,
     $A5, $A6, $A7, $A8, $A9, $D7, $AB, $AC, $AD,
     $AE, $AF, $B0, $B1, $B2, $B3, $B4, $B5, $B6,
     $B7, $B8, $B9, $F7, $BB, $BC, $BD, $BE, $BF,
     $5B0, $5B1, $5B2, $5B3, $5B4, $5B5, $5B6, $5B7,
     $5B8, $5B9, $00, $5BB, $5BC, $5BD, $5BE, $5BF,
     $5C0, $5C1, $5C2, $5C3, $5F0, $5F1, $5F2, $5F3,
     $5F4, $00, $00, $00, $00, $00, $00, $00, $5D0,
     $5D1, $5D2, $5D3, $5D4, $5D5, $5D6, $5D7, $5D8,
     $5D9, $5DA, $5DB, $5DC, $5DD, $5DE, $5DF, $5E0,
     $5E1, $5E2, $5E3, $5E4, $5E5, $5E6, $5E7, $5E8,
     $5E9, $5EA, $00, $00, $200E, $200F, $00);
  Win1256: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $67E, $201A, $192, $201E,
     $2026, $2020, $2021, $2C6, $2030, $679, $2039,
     $152, $686, $698, $688, $6AF, $2018, $2019,
     $201C, $201D, $2022, $2013, $2014, $6A9, $2122,
     $691, $203A, $153, $200C, $200D, $6BA, $A0,
     $60C, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9,
     $6BE, $AB, $AC, $AD, $AE, $AF, $B0, $B1, $B2,
     $B3, $B4, $B5, $B6, $B7, $B8, $B9, $61B, $BB,
     $BC, $BD, $BE, $61F, $6C1, $621, $622, $623,
     $624, $625, $626, $627, $628, $629, $62A, $62B,
     $62C, $62D, $62E, $62F, $630, $631, $632, $633,
     $634, $635, $636, $D7, $637, $638, $639, $63A,
     $640, $641, $642, $643, $E0, $644, $E2, $645,
     $646, $647, $648, $E7, $E8, $E9, $EA, $EB,
     $649, $64A, $EE, $EF, $64B, $64C, $64D, $64E,
     $F4, $64F, $650, $F7, $651, $F9, $652, $FB,
     $FC, $200E, $200F, $6D2);
  Win1257: TCodeTable =
    ($00, $01, $02, $03, $04, $05, $06, $07, $08,
     $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11,
     $12, $13, $14, $15, $16, $17, $18, $19, $1A,
     $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23,
     $24, $25, $26, $27, $28, $29, $2A, $2B, $2C,
     $2D, $2E, $2F, $30, $31, $32, $33, $34, $35,
     $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47,
     $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
     $51, $52, $53, $54, $55, $56, $57, $58, $59,
     $5A, $5B, $5C, $5D, $5E, $5F, $60, $61, $62,
     $63, $64, $65, $66, $67, $68, $69, $6A, $6B,
     $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74,
     $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D,
     $7E, $7F, $20AC, $00, $201A, $00, $201E, $2026,
     $2020, $2021, $00, $2030, $00, $2039, $00,
     $A8, $2C7, $B8, $00, $2018, $2019, $201C, $201D,
     $2022, $2013, $2014, $00, $2122, $00, $203A,
     $00, $AF, $2DB, $00, $A0, $00, $A2, $A3, $A4,
     $00, $A6, $A7, $D8, $A9, $156, $AB, $AC, $AD,
     $AE, $C6, $B0, $B1, $B2, $B3, $B4, $B5, $B6,
     $B7, $F8, $B9, $157, $BB, $BC, $BD, $BE, $E6,
     $104, $12E, $100, $106, $C4, $C5, $118, $112,
     $10C, $C9, $179, $116, $122, $136, $12A, $13B,
     $160, $143, $145, $D3, $14C, $D5, $D6, $D7,
     $172, $141, $15A, $16A, $DC, $17B, $17D, $DF,
     $105, $12F, $101, $107, $E4, $E5, $119, $113,
     $10D, $E9, $17A, $117, $123, $137, $12B, $13C,
     $161, $144, $146, $F3, $14D, $F5, $F6, $F7,
     $173, $142, $15B, $16B, $FC, $17C, $17E, $2D9);

function GetDWGCPNameByCodePage(ACodePage: Cardinal): string;
var
  I: Integer;
  vName: string;

  function CorrectName(const APrefix, AName: string; var AResult: string): Boolean;
  begin
    Result := False;
    if Pos(APrefix, AName) = 1 then
    begin
      if (Length(AResult) > Length(APrefix)) and
      {$IFDEF SGDEL_XE4}
        AResult[Length(APrefix) + 1].IsInArray(['-', '_']) then
      {$ELSE}
        CharInSet(AResult[Length(APrefix) + 1], ['-', '_']) then
      {$ENDIF}
      begin
        Delete(AResult, Length(APrefix) + 1, 1);
        Result := True;
      end;
    end
  end;

begin
  Result := '';
  case ACodePage of
    874, 932, 1250 .. 1259:
      Result := cnstANSI + '_' + IntToStr(ACodePage);
    437, 850, 852, 855, 857, 860, 861, 863, 864, 865, 866, 869:
      Result := cnstDOS + IntToStr(ACodePage);
    936:
      Result := cnstGB2312;
    949:
      Result := cnstKSC5601;
    950:
      Result := cnstBIG5;
    1361:
      Result := cnstJOHAB;
    20127:
      Result := cnstASCII;
    1200:
      Result := cnstUTF16;
    10000:
      Result := cnstMacintosh;
  else
    Result := CodePageIdTable.GetName(ACodePage);
    vName := UpperCase(Result);
    if not CorrectName(cnstISO, vName, Result) then
      if not CorrectName(cnstDOS, vName, Result) then
      begin
        I := System.Pos(cnstWindows, vName);
        if I = 1 then
        begin
          Delete(Result, 1, Length(cnstWindows));
          if (Result <> '') and CharInSet(Result[1], ['-', '_']) then
            Result[1] := '_';
          Result := cnstANSI + Result;
        end;
      end;
  end;
end;

function GetDWGCodePageByName(const AName: string): Integer;
var
  vName: string;
  I, vPos: Integer;

  function CorrectName(const APrefix: string; var AName: string;
    var AResult: Integer; var APos: Integer): Boolean;
  var
    J: Integer;
  begin
    Result := False;
    APos := 0;
    J := System.Pos(APrefix, AName);
    if J = 1 then
    begin
      AName := System.Copy(AName, Length(APrefix) + 1, MaxInt);
      if (AName <> '') and CharInSet(AName[1], ['-', '_']) then
        System.Delete(AName, 1, 1);
      System.Val(AName, AResult, APos);
      Result := True;
    end
  end;

begin
  Result := CP_ACP;
  vName := UpperCase(AName);
  if not CorrectName(cnstANSI, vName, Result, vPos) then
    if not CorrectName(cnstDOS, vName, Result, vPos) then
      if CorrectName(cnstISO, vName, Result, vPos) then
        if (vPos > 0) and (Result = cnstISO8859) then
        begin
          vName := System.Copy(vName, vPos + 1, MaxInt);
          Val(vName, I, vPos);
          Result := CP_ISO_BASE + I
        end
        else
          Result := CP_ACP
      else
        if vName = cnstJOHAB then
          Result := 1361
        else
        if vName = cnstGB2312 then
          Result := 936
        else
        if vName = cnstKSC5601 then
          Result := 949
        else
        if vName = cnstBIG5 then
          Result := 950
        else
        if vName = cnstASCII then
          Result := 20127
        else
        if vName = cnstUTF16 then
          Result := 1200
        else
        if vName = cnstMacintosh then
          Result := 10000;
end;

function StrToUnicode(CodePage: Integer; const S: string): WideString;
var
  I, vLen: Integer;
  vCodeTable: PCodeTable;
begin
  Result := '';
  FillChar(vCodeTable, SizeOf(vCodeTable), 0);
  case CodePage of
    874:
      vCodeTable := @CP874;
    1250:
      vCodeTable := @Win1250;
    1251:
      vCodeTable := @Win1251;
    1252:
      vCodeTable := @Win1252;
    1253:
      vCodeTable := @Win1253;
    1254:
      vCodeTable := @Win1254;
    1255:
      vCodeTable := @Win1255;
    1256:
      vCodeTable := @Win1256;
    1257:
      vCodeTable := @Win1257;
    else
      Exit;
  end;
  vLen := Length(S);
  for I := 1 to vLen do
  {$IFDEF VER100}
    Result := Result + WideString(WideChar(vCodeTable^[Byte(S[I])]));
  {$ELSE}
    Result := Result + WideChar(vCodeTable^[Byte(S[I])]);
  {$ENDIF}
end;



{ TCodePageIdTable }

function TCodePageIdTable.AddId(ACodePage: Integer; const APrimaryName: string;
  const ASecondaryNames: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (APrimaryName = '') or IsExists(APrimaryName) then
    Exit;
  FPrimaryIds.AddObject(APrimaryName, GetObjectFromValue(ACodePage));
  for I := Low(ASecondaryNames) to High(ASecondaryNames) do
    if not IsExists(ASecondaryNames[I]) then
      FSecondaryIds.AddObject(ASecondaryNames[I], GetObjectFromValue(ACodePage));
end;

constructor TCodePageIdTable.Create;
begin
  inherited Create;
  FPrimaryIds := TStringList.Create;
  FPrimaryIds.Sorted := True;
  FPrimaryIds.Duplicates := {$IFDEF SG_FIREMONKEY}dupAccept{$ELSE}dupError{$ENDIF};
{$IFDEF SGDEL_6}
  FPrimaryIds.CaseSensitive := False;
{$ENDIF}
  FSecondaryIds := TStringList.Create;
  FSecondaryIds.Sorted := True;
  FSecondaryIds.Duplicates := FPrimaryIds.Duplicates;
{$IFDEF SGDEL_6}
  FSecondaryIds.CaseSensitive := False;
{$ENDIF}
end;

destructor TCodePageIdTable.Destroy;
begin
  inherited Destroy;
{$IFDEF SG_FIREMONKEY}
  ClearObjectObjects(FPrimaryIds);
  ClearObjectObjects(FSecondaryIds);
{$ENDIF}
  FPrimaryIds.Free;
  FSecondaryIds.Free;
end;

function TCodePageIdTable.GetCodePage(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := FPrimaryIds.IndexOf(AName);
  if I >= 0 then
    Result := GetValueFromObject(FPrimaryIds.Objects[I])
  else
  begin
    I := FSecondaryIds.IndexOf(AName);
    if I >= 0 then
      Result := GetValueFromObject(FSecondaryIds.Objects[I])
  end;
end;

function TCodePageIdTable.GetName(const ACodePage: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := IndexOfCodePage(ACodePage, FPrimaryIds);
  if I >= 0 then
    Result := FPrimaryIds[I]
  else
  begin
    I := IndexOfCodePage(ACodePage, FSecondaryIds);
    if I >= 0 then
      Result := FSecondaryIds[I];
  end;
end;

function TCodePageIdTable.GetObjectFromValue(const AValue: Integer): TObject;
{$IFNDEF SG_FIREMONKEY}
var
  vCode: TObject absolute AValue;
begin
  Result := vCode;
{$ELSE}
begin
  Result := TsgObjectWithField.CreateInt(AValue);
{$ENDIF}
end;

function TCodePageIdTable.GetValueFromObject(const AObject: TObject): Integer;
{$IFNDEF SG_FIREMONKEY}
var
  vCode: Integer absolute AObject;
begin
  Result := vCode;
{$ELSE}
begin
  Result := TsgObjectInt64(AObject).FieldInt;
{$ENDIF}
end;

function TCodePageIdTable.IndexOfCodePage(const AValue: Integer;
  const AList: TStringList): Integer;
{$IFDEF SG_FIREMONKEY}
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AList.Count - 1 do
  begin
    if TsgObjectInt64(AList.Objects[I]).FieldInt = AValue then
    begin
      Result := I;
      Break;
    end;
  end;
{$ELSE}
begin
  Result := AList.IndexOfObject(GetObjectFromValue(AValue));
{$ENDIF}
end;

procedure TCodePageIdTable.InitializeIds;
begin
  AddId(437,   'IBM437',            ['DOS-437', '437', 'cp437', 'csPC8', 'CodePage437']);
  AddId(720,   'dos-720',           []);
  AddId(850,   'ibm850',            ['DOS-850']);
  AddId(852,   'ibm852',            ['cp852', 'DOS-852']);
  AddId(861,   'ibm861',            ['DOS-861']);
  AddId(862,   'dos-862',           []);
  AddId(866,   'cp866',             ['ibm866', 'DOS-866']);
  AddId(869,   'ibm869',            ['DOS-869']);
  AddId(874,   'Windows-874',       ['DOS-874', 'iso-8859-11', 'TIS-620']);
  AddId(932,   'shift_jis',         ['x-sjis', 'ms_Kanji', 'csShiftJIS', 'x-ms-cp932']);
  AddId(936,   'gb2312',            ['GB_2312-80', 'iso-ir-58', 'chinese', 'csISO58GB231280', 'csGB2312']);
  AddId(949,   'ks_c_5601',         ['ks_c_5601-1987', 'korean', 'csKSC56011987']);
  AddId(950,   'big5',              ['csbig5', 'x-x-big5']);
  AddId(1250,  'Windows-1250',      ['x-cp1250']);
  AddId(1251,  'Windows-1251',      ['x-cp1251']);
  AddId(1252,  'Windows-1252',      ['ANSI_X3.4-1968', 'ANSI_X3.4-1986', 'ascii', 'cp367', 'cp819', 'csASCII', 'IBM367', 'ibm819',
    'ISO_646.irv:1991', 'iso_8859-1', 'iso_8859-1:1987', 'ISO646-US', 'iso8859-1', 'iso-8859-1', 'iso-ir-100', 'iso-ir-6', 'latin1', 'us', 'us-ascii', 'x-ansi']);
  AddId(1253,  'Windows-1253',      []);
  AddId(1254,  'Windows-1254',      ['ISO_8859-9', 'ISO_8859-9:1989', 'iso-8859-9', 'iso-ir-148', 'latin5']);
  AddId(1255,  'Windows-1255',      ['iso-8859-8', 'iso-8859-8i', 'ISO-8859-8 Visual', 'ISO_8859-8-I', 'ISO_8859-8', 'visual']); // visual and logical
  AddId(1256,  'Windows-1256',      ['cp1256']);
  AddId(1257,  'Windows-1257',      []);
  AddId(1258,  'Windows-1258',      []);
  AddId(1361,  'Johab',             []);
  AddId(20105, 'us-ascii',          ['ascii']);
  AddId(20127, 'ascii',             ['us-ascii', 'ANSI_X3.4-1968', 'ANSI_X3.4-1986', 'cp367', 'csASCII', 'IBM367', 'ISO_646.irv:1991', 'ISO646-US', 'iso-ir-6us']);
  AddId(20866, 'koi8-r',            ['csKOI8R']);
  AddId(28591, 'iso-8859-1',        ['cp819', 'csISOLatin1', 'ibm819', 'iso_8859-1', 'iso_8859-1:1987', 'iso8859-1', 'iso-ir-100', 'l1', 'latin1']);
  AddId(28592, 'iso-8859-2',        ['iso8859-2', 'iso_8859-2', 'latin2', 'iso_8859-2:1987', 'iso-ir-101', 'l2', 'csISOLatin2']);
  AddId(28593, 'iso-8859-3',        ['csISOLatin3', 'ISO_8859-3', 'ISO_8859-3:1988', 'iso-ir-109', 'l3', 'latin3']);
  AddId(28594, 'iso-8869-4',        ['ISO_8859-4:1988', 'iso-ir-110', 'ISO_8859-4', 'latin4', 'l4', 'csISOLatin4']);
  AddId(28585, 'iso-8859-5',        ['ISO_8859-5:1988', 'iso-ir-144', 'ISO_8859-5', 'cyrillic', 'csISOLatinCyrillic', 'csISOLatin5']);
  AddId(28597, 'iso-8859-7',        ['ISO_8859-7:1987', 'iso-ir-126', 'ISO_8859-7', 'ELOT_928', 'ECMA-118', 'greek', 'greek8', 'csISOLatinGreek']);
  AddId(28598, 'iso-8859-8',        ['ISO_8859-8:1988', 'iso-ir-138', 'ISO_8859-8', 'ISO-8859-8', 'csISOLatinHebrew', 'hebrew', 'visual']);
  AddId(28599, 'iso-8859-9',        ['ISO_8859-9:1989', 'iso-ir-148', 'ISO_8859-9', 'latin5', 'l5']);
  AddId(50220, 'iso-2022-jp',       ['csISO2022JP']);
  AddId(50225, 'ISO-2022-KR',       ['csISO2022KR']);
  AddId(51932, 'x-euc-jp',          ['Extended_UNIX_Code_Packed_Format_for_Japanese', 'csEUCPkdFmtJapanese', 'x-euc']);
  AddId(52936, 'HZ-GB-2312',        []);
  AddId(65000, 'UNICODE-1-1-UTF-7', ['csUnicode11UTF7', 'utf-7']);
  AddId(65001, 'UTF-8',             ['unicode-1-1-utf-8', 'unicode-2-0-utf-8']);
end;

function TCodePageIdTable.IsExists(const AName: string): Boolean;
begin
  Result := (FPrimaryIds.IndexOf(AName) >= 0) or (FSecondaryIds.IndexOf(AName) >= 0);
end;

initialization
  CodePageIdTable := TCodePageIdTable.Create;
  CodePageIdTable.InitializeIds;

finalization
  CodePageIdTable.Free;

end.

{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   DWG bits stream read/write class         }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDWGBits;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
   Windows,
{$ENDIF}
   SysUtils, Classes, sgConsts, sgFunction, Math
{$IFDEF SGDEL_XE2}
  , System.Types
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  , sgFMXTypes
{$ENDIF}
  ;

const
  sByteArryaAccessError: string = 'Byte array access error';
  sFloatSizeError: string = 'Size of TsgFloat is incorrect';
  sBeginSentinelError: string = 'Begin sentinel cannot be empty';
  sBeginSentinelNotFoundError: string = 'Begin sentinel cannot be found';
  sEndSentinelNotFoundError: string = 'End sentinel cannot be found';
  sCRCError: string = 'CRC does not match';

type
  EDWGBitsError = class(Exception);

  TsgWriteTextLen = function(const ALength: Word): Integer of object;

  TsgDWGBits = class
  private
    FAllocBlock: Cardinal;
    FByte: Cardinal;
    FBit: Byte;
    FData: PByte;
    FAllocatedBytes: Cardinal;
    FBitSize: Int64;
    FCapacity: Cardinal;
    FBeginSentinel: PByteArray;
    FEndSentinel: PByteArray;
    FDoDispose: Boolean;
    FObjectType: Cardinal;
    procedure AllocBits(ADoMoveData: Boolean);
    procedure IncBitPos(APosition: Cardinal; ACheckSize: Boolean = False);
    procedure CheckSize;
    procedure CheckTsgFloatSize;
    function ColorToDWG(const Value: TsgColorCAD): Cardinal;
    function GetBit: Byte;
    function GetByte: Cardinal;
    function GetSize: Cardinal;
    function GetByteArray: PByteArray;
    function GetPosition: Cardinal;
    function GetObjectType: Cardinal;
    function IsLastByte: Boolean;
    procedure SetPosition(const Value: Cardinal);
    procedure SetSizes(ASize: Cardinal; ASizeInByte: Boolean);
    procedure SetAllocBlock(const Value: Cardinal);
  protected
    function GetBitSize: Cardinal;
    function GetVersion: TsgDWGVersion; virtual;
    procedure InitData; virtual;
    function InternalWriteHandle(const Handle: UInt64; const Code: Byte;
      ARecordHandle: Boolean): Integer;
    function InternalReadT(ACodePage: Integer; AParseUnicode, AXRecordString,
      XRecordStringConv: Boolean; SizeOfLen: Integer = 1;
      IsUnicode: Boolean = False): string;
    function InternalWriteT(const Value: string; WriteNullTerminator,
      XRecordString: Boolean; CodePage: Integer; SizeOfLen: Integer = 1;
      NeedStringToACADPresentation: Boolean = True; WriteTermAlways: Boolean = False): Integer;
    function InternalWriteTU(const AValue: WideString; const AWriteTextLen: TsgWriteTextLen;
      WriteNullTerminator: Boolean = False): Integer;
    function InternalUpdateValue(const Data: Pointer; Offset: Cardinal;
      ValueSize: Byte): Integer;

    function WriteENCSection(const AColorFlags: Word;
      const AValue: TsgColorCAD; ATransparency: Integer): Integer; virtual;
  public
    constructor Create(const ASize: Cardinal = 0; ASizeInByte: Boolean = True);
    constructor CreateFromDWGObjectData(const ObjData: Pointer); virtual;
    constructor CreateFromMemory(const Memory: Pointer; const ASize: Cardinal);
    constructor CreateFromMemoryAndSentinel(const Memory: Pointer; const MaxSize: Cardinal;
      const BeginSentinel, EndSentinel: array of Byte);
    constructor CreateFromTsgInt64(const AValue: TsgInt64; AOffset: Integer = 0);

    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Complete; virtual;
    function Eof: Boolean;
    procedure GotoLastBit;
    procedure GotoFirstBit;

    procedure InsertBytes(ABytePos: Cardinal; Bytes: Pointer; Count: Cardinal);
    procedure InsertMS(const ABytePos, Value: Cardinal);
    procedure InsertMCHandle(const ABytePos, Value: Cardinal);

    function Read2BD: TFPoint;
    function Read2RD: TFPoint;
    function Read3BD: TFPoint;
    function Read3DD(const DefValue: TFPoint): TFPoint;
    function Read3RD: TFPoint;
    function ReadBB: Byte;
    function ReadBE: TFPoint;
    function ReadBD: Double;
    function ReadBit: Byte;
    function ReadBL: Cardinal;
    function ReadBT: Double;
    function ReadBS: Word;
    procedure ReadBytes(const Bytes: Pointer; Count: Integer);
    function ReadBytesHandle(const ABytesCount: Byte): UInt64;
    function ReadCRC8(AReadFromEndPos: Boolean = True; ASwapByte: Boolean = False): Word;
    function ReadCMC: TsgColorCAD; virtual;
    function ReadDD(const DefValue: Double): Double;
    function ReadENCSection(AFlags: Integer): TsgColorCAD;
    function ReadHandle(var ACode: Byte; ARef: UInt64 = 0): UInt64; virtual;
    function ReadJulianDateIntAsDateTime(AReadMillSec: Boolean; AReadBL: Boolean = True): TDateTime;
    function ReadJulianDateIntAsTimeStamp(AReadMillSec: Boolean;
      AReadBL: Boolean = True): TTimeStamp;
    function ReadL: Cardinal;
    function ReadMC: Integer; overload;
    function ReadMCHandle: UInt64; overload;
    function ReadMS: Cardinal;
    function ReadOT: Word; virtual; //Object Type
    function ReadRC: Byte; overload;
    function ReadRL: Cardinal;
    function ReadRD: Double;
    function ReadRS: Word;
    function ReadT(ACodePage: Integer = 0; AParseUnicode: Boolean = True): string;
    function ReadTV(ACodePage: Integer = 0; AParseUnicode: Boolean = True): string; virtual;
    function ReadAppInfoText: string; virtual;
    function ReadSummaryInfoText: string; virtual;
    function ReadFileDepListText: string; virtual;
    function ReadXRecordText(XRecordStringConv: Boolean; AParseUnicode: Boolean = True): string; virtual;

    function TosgInt64: TsgInt64;

    function SaveToStream(Stream: TStream): Integer;
    function Seek(BitOffset: Integer; Origin: TSeekOrigin = soCurrent): Boolean;
    function SeekInBits(BitOffset: Integer; Origin: TSeekOrigin = soCurrent): Boolean;
    function SeekInBytes(ByteOffset: Integer; Origin: TSeekOrigin = soCurrent): Boolean;

    function Write2BD(const Value: TF2DPoint): Integer; overload;
    function Write2BD(const Value: TFPoint): Integer; overload;
    function Write2RD(const Value: TF2DPoint): Integer; overload;
    function Write2RD(const Value: TFPoint): Integer; overload;
    function Write3Bits(const Value: Byte): Integer;
    function Write3BD(const Value: TFPoint): Integer; overload;
    function Write3BD(const Value: TF2DPoint): Integer; overload;
    function Write3BDPreExtruded(const Value, Extrusion: TFPoint): Integer;
    function Write3RD(const Value: TFPoint): Integer;
    function Write3DD(const Value, DefValue: TFPoint): Integer;
    function WriteBB(const Value: Byte): Integer;
    function WriteBD(const Value: Double): Integer;
    function WriteBE(const Value: TFPoint): Integer;
    function WriteBit(const Value: Byte): Integer; overload;
    function WriteBit(const Value: Boolean): Integer; overload;
    function WriteBit(const Value, Mask: Byte): Integer; overload;
    function WriteBL(const Value: Cardinal): Integer;
    function WriteBS(const Value: Word): Integer;
    function WriteBT(const Value: Double): Integer;
    function WriteBytes(const Bytes: Pointer; Count: Cardinal;
      AFromBack: Boolean = False): Integer; overload;
    function WriteBytes(const Byte: Byte; Count: Cardinal): Integer; overload;
    function WriteColorFlags(const AColorFlags: Word): Integer;
    function WriteCMC(const Value: TsgColorCAD): Integer; virtual;
    function WriteCMCDirectly(const AValue: TsgColorCAD): Integer;
    function WriteCRC8(const Init: Word; ASwapBytes: Boolean = False): Integer;
    function WriteDD(const Value, DefValue: Double): Integer;
    function WriteENC(const Value: TsgColorCAD; ATransparency: Integer): Integer; virtual;
    function WriteEntHandle(const Handle: UInt64; const Code: Byte): Integer;
    function WriteHandle(const Handle: UInt64; const Code: Byte): Integer; virtual;
    function WriteHandleRecord(const Handle: UInt64; Code: Byte): Integer;
    function WriteJulianDateInt(const ADateTime: TDateTime; AWriteBL,
      AWriteMilSec: Boolean): Integer; overload;
    function WriteJulianDateInt(const ADateTime: TTimeStamp; AWriteBL,
      AWriteMilSec: Boolean): Integer; overload;
    function WriteL(const Value: Cardinal): Integer;
    function WriteMC(const Value: Integer): Integer;
    function WriteMCHandle(const Value: UInt64): Integer;
    function WriteMS(const Value: Cardinal): Integer;
    function WriteOT(const Value: Word): Integer; virtual;
    function WriteRC(const Value: Byte): Integer; overload;
    function WriteRC(const Value: AnsiChar): Integer; overload;
    function WriteSentinel(const Value: array of Byte): Integer;
    function WriteRD(const Value: Double): Integer;
    function WriteRL(const Value: Cardinal): Integer;{$IFDEF SGDEL_2005} overload;{$ENDIF}
{$IFDEF SGDEL_2005}
    function WriteRL(const Value: Integer): Integer; overload;
{$ENDIF}
    function WriteRL0: Integer;{$IFDEF SGDEL_2006} inline;{$ENDIF}
    function WriteRS(const Value: Word): Integer;
    function WriteT(const Value: string; CodePage: Integer;
      WriteNullTerminator: Boolean = False): Integer; virtual;
    function WriteTV(const Value: string; CodePage: Integer;
      WriteNullTerminator: Boolean = False): Integer; virtual;
    function WriteUInt64(const Value: UInt64): Integer;
    function WriteXDirection(HasXDirection: Boolean; const Point: TFPoint): Integer; overload;
    function WriteXDirection(const Angle: Double; const Extrusion: TFPoint): Integer; overload;
    function WriteSummaryInfoText(const Value: string): Integer; virtual;
    function WriteAppInfoText(const Value: string): Integer; virtual;
    function WriteFileDepListText(const Value: string): Integer; virtual;
    function WriteXRecordText(const Value: string; CodePage: Integer;
      XRecordStringConv: Boolean; WriteNullTerminator: Boolean = False): Integer; virtual;

    function UpdateRL(const Offset, Value: Cardinal): Integer;
    function UpdateRS(const Offset: Cardinal; const Value: Word): Integer;
    function UpdateRC(const Offset: Cardinal; const Value: Byte): Integer;

    property AllocBlock: Cardinal read FAllocBlock write SetAllocBlock;
    property BitSize: Int64 read FBitSize;
    property CurBit: Byte read GetBit;
    property CurByte: Cardinal read GetByte;
    property Data: PByteArray read GetByteArray;
    property ObjectType: Cardinal read GetObjectType;
    property Position: Cardinal read GetPosition write SetPosition;
    property Size: Cardinal read GetSize;
    property Version: TsgDWGVersion read GetVersion;
  end;

  TsgDWGBitsClass = class of TsgDWGBits;

  TsgDWG2004Bits = class(TsgDWGBits)
  private
    function WriteCMCSection(const AColorFlags: Word;
      const AValue: TsgColorCAD): Integer;
  protected
    function GetVersion: TsgDWGVersion; override;
    function WriteENCSection(const AColorFlags: Word;
      const AValue: TsgColorCAD; ATransparency: Integer): Integer; override;
  public
    function WriteCMC(const Value: TsgColorCAD): Integer; override;
    function WriteENC(const Value: TsgColorCAD; ATransparency: Integer): Integer; override;
  end;

  TsgDWG2007Bits = class(TsgDWG2004Bits)
  private
    FStrsBits: TsgDWGBits;
    FHandlesBits: TsgDWGBits;
    FUnicodeStrs: array of sgUnicodeStr;
    procedure ClearStrs;
  protected
    procedure CheckAndCreateBits(var Bits: TsgDWGBits);
    function GetVersion: TsgDWGVersion; override;
    procedure InitData; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Complete; override;
    function ReadAppInfoText: string; override;
    function ReadSummaryInfoText: string; override;
    function WriteAppInfoText(const Value: string): Integer; override;
    function WriteT(const Value: string; CodePage: Integer;
      WriteNullTerminator: Boolean = False): Integer; override;
    function WriteTV(const Value: string; CodePage: Integer;
      WriteNullTerminator: Boolean = False): Integer; override;
    function WriteSummaryInfoText(const Value: string): Integer; override;
    function WriteXRecordText(const Value: string; CodePage: Integer;
      XRecordStringConv: Boolean; WriteNullTerminator: Boolean = False): Integer; override;
  end;

  TsgDWG2010Bits = class(TsgDWG2007Bits)
  protected
    function GetMCHandleSize(const Value: Cardinal): Integer;
    function GetMSSize(const Value: Cardinal): Integer;
    function GetVersion: TsgDWGVersion; override;
  public
    function ReadOT: Word; override;
    function WriteOT(const Value: Word): Integer; override;
    procedure WriteObjectSize(const Size: Cardinal);
    procedure WriteHandlesSize(const HandlesSize: Cardinal);
  end;

  TsgDWG2013Bits = class(TsgDWG2010Bits)
  protected
    function GetVersion: TsgDWGVersion; override;
  end;

  TsgDWG2018Bits = class(TsgDWG2010Bits)
  protected
    function GetVersion: TsgDWGVersion; override;
  end;

function GetDWGBitsClassByVersion(const DWGVersion: TsgDWGVersion): TsgDWGBitsClass;
function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion; const Size: Cardinal = 0;
  SizeInByte: Boolean = True): TsgDWGBits; overload;
function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const ObjData: Pointer): TsgDWGBits; overload;
function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const Memory: Pointer; const Size: Cardinal): TsgDWGBits; overload;
function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const Memory: Pointer; const MaxSize: Cardinal;
  const BeginSentinel, EndSentinel: array of Byte): TsgDWGBits; overload;

implementation
const
  cntAllocBlock = 250;

{ TsgDWGBits }

procedure TsgDWGBits.AllocBits(ADoMoveData: Boolean);
  procedure AllocAndZero;
  begin
    GetMem(FData, FCapacity);
    FillChar(FData^, FCapacity, $0);
  end;
var
  P: PByte;
  vOldCapacity: Cardinal;
begin
  if FData = nil then
    AllocAndZero
  else
  begin
    vOldCapacity := FCapacity;
    Inc(FCapacity, FAllocBlock);
    P := FData;
    AllocAndZero;
    if ADoMoveData then
      Move(P^, FData^, vOldCapacity);
    Dispose(P);
  end;
  FDoDispose := True;
end;

procedure TsgDWGBits.IncBitPos(APosition: Cardinal;
  ACheckSize: Boolean = False);
var
  vPos: Cardinal;
begin
  if ACheckSize then CheckSize;
  vPos := FBit + APosition;
  if (FByte >= FAllocatedBytes - 1) and (vPos > cntLastBitInByte) then
  begin
    FBit := cntLastBitInByte;
    Exit;
  end;
  FByte := FByte + vPos div cntBitPerByte;
  FBit := vPos mod cntBitPerByte;
end;

procedure TsgDWGBits.InitData;
begin
  GotoFirstBit;
  ReadMS;
  ReadRL;
  FObjectType := ReadOT;
end;

procedure TsgDWGBits.InsertBytes(ABytePos: Cardinal; Bytes: Pointer;
  Count: Cardinal);
var
  P1, P2, P3: PByte;
  vSize: Cardinal;
begin
  vSize := Size;
  if ABytePos > vSize then Exit;
  GetMem(P1, vSize);
  try
    P3 := P1;
    Move(FData^, P1^, vSize);
    Clear;
    FCapacity := vSize + Count;
    AllocBits(False);
    P2 := FData;
    SetSizes(vSize + Count, True);
    Move(P1^,  P2^, ABytePos);
    Inc(P2, ABytePos);
    Move(Bytes^, P2^, Count);
    Inc(P2, Count);
    Inc(P3, ABytePos);
    Move(P3^,  P2^, vSize - ABytePos);
    GotoLastBit;
  finally
    FreeMem(P1);
  end;
end;

procedure TsgDWGBits.InsertMCHandle(const ABytePos, Value: Cardinal);
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteMCHandle(Value);
    InsertBytes(ABytePos, vBits.FData, vBits.Size);
  finally
    vBits.Free;
  end;
end;

procedure TsgDWGBits.InsertMS(const ABytePos, Value: Cardinal);
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteMS(Value);
    InsertBytes(ABytePos, vBits.FData, vBits.Size);
  finally
    vBits.Free;
  end;
end;

function TsgDWGBits.InternalReadT(ACodePage: Integer; AParseUnicode, AXRecordString,
  XRecordStringConv: Boolean; SizeOfLen: Integer = 1;
  IsUnicode: Boolean = False): string;
var
  vLen: Integer;
  vAnsi: sgRawByteString;
  vWideString: WideString;
begin
  Result := '';
  vAnsi := '';
  if AXRecordString then
  begin
    if XRecordStringConv then
    begin
      vLen := ReadRC;
{$IFDEF SGDEL_2009}
      ACodePage := sgCodePageFromDWG(ReadRS);
{$ENDIF}
    end
    else
    begin
      vLen := ReadRS;
{$IFDEF SGDEL_2009}
      ACodePage := sgCodePageFromDWG(ReadRC);
{$ENDIF}
    end;
  end
  else
    if SizeOfLen = 1 then
      vLen := ReadBS
    else
      if SizeOfLen = 2 then
        vLen := ReadRS
      else
        if SizeOfLen = 4 then
          vLen := ReadRL
        else
          vLen := 0;
  if vLen = 0 then Exit;
  if not IsUnicode then
  begin
    SetLength(vAnsi, vLen);
  {$IFDEF SGDEL_2009}
    SetCodePage(vAnsi, $FFFF, False);
  {$ENDIF}
    ReadBytes(@vAnsi[1], vLen);
  {$IFDEF SGDEL_2009}
    SetCodePage(vAnsi, ACodePage, False);
  {$ENDIF}
    Result := string(vAnsi);
  end
  else
  begin
    SetLength(vWideString, vLen);
    ReadBytes(@vWideString[1], vLen shl 1);
    Result := string(vWideString);
  end;
  vLen := Length(Result);
  if (vLen > 0) and (Result[vLen] = #0) then
    SetLength(Result, vLen - 1);
{$IFDEF SGDEL_2009}
  if AParseUnicode then
    Result := ParseUnicode(Result, [puU, puM]);
{$ENDIF}
end;

function TsgDWGBits.InternalUpdateValue(const Data: Pointer; Offset: Cardinal;
  ValueSize: Byte): Integer;
var
  vPos, vSize: Cardinal;
begin
  vPos := Position;
  vSize := Size;
  Position := Offset;
  case ValueSize of
    1: Result := WriteRC(PByte(Data)^);
    2: Result := WriteRS(PWord(Data)^);
  else
    Result := WriteRL(PCardinal(Data)^);
  end;
  if vSize < Size then
    SetSizes(vSize, True);
  Position := vPos;
end;

function TsgDWGBits.InternalWriteHandle(const Handle: UInt64; const Code: Byte;
  ARecordHandle: Boolean): Integer;
var
  vBytes: array[0..SizeOf(Handle) - 1] of Byte absolute Handle;
  I, vFirstNonZeroByteInx: Integer;
begin
  if Handle = 0 then
  begin
    if not ARecordHandle then
      Result := WriteRC(Code shl 4)
    else
      Result := WriteRC(0);
    Exit;
  end;

  vFirstNonZeroByteInx := High(vBytes);
  for I := High(vBytes) downto 0 do
    if vBytes[I] <> 0 then
    begin
      vFirstNonZeroByteInx := I;
      Break;
    end;

  if not ARecordHandle then
    Result := WriteRC((Code shl 4) or (vFirstNonZeroByteInx + 1))
  else
  begin
    Result := WriteRC(vFirstNonZeroByteInx + 1);
    Inc(Result, WriteRC(Code));
  end;

  for I := vFirstNonZeroByteInx downto 0 do
    Result := Result + WriteRC(vBytes[I]);
end;

function TsgDWGBits.InternalWriteT(const Value: string; WriteNullTerminator,
  XRecordString: Boolean; CodePage: Integer; SizeOfLen: Integer = 1;
  NeedStringToACADPresentation: Boolean = True; WriteTermAlways: Boolean = False): Integer;
var
  I, vLen: Integer;
  vAnsiString: sgRawByteString;
begin
  Result := 0;
  if NeedStringToACADPresentation then
    vAnsiString := StringToACADPresentation(Value, CodePage)
  else
    vAnsiString := sgRawByteString(Value);
  vLen := Length(vAnsiString);
  if (vLen > 0) and WriteNullTerminator then
    vLen := vLen + 1;
  if (vLen = 0) and WriteNullTerminator and WriteTermAlways then
    Inc(vLen);
  if XRecordString then
  begin
    Result := WriteRS(vLen);
    Inc(Result, WriteRC(sgDWGCodePageFromCP(CodePage, True)));
  end
  else
    if SizeOfLen = 1 then
      Result := WriteBS(vLen)
    else
      if SizeOfLen = 2 then
        Result := WriteRS(vLen)
      else
        if SizeOfLen = 4 then
          Result := WriteRL(vLen)
        else
          vLen := 0;
  if vLen > 0 then
  begin
    for I := 1 to Length(vAnsiString) do
      Result := Result + WriteRC(vAnsiString[I]);
    if WriteNullTerminator then
      Result := Result + WriteRC(#0);
  end;
end;

function TsgDWGBits.InternalWriteTU(const AValue: WideString;
  const AWriteTextLen: TsgWriteTextLen; WriteNullTerminator: Boolean = False): Integer;
var
  vStrLen: Integer;
  P: Pointer;
begin
  vStrLen := Length(AValue);
  P := Pointer(@AValue[1]);
  Result := AWriteTextLen(vStrLen + Ord(WriteNullTerminator));
  Inc(Result, WriteBytes(P, vStrLen shl 1));
  if WriteNullTerminator then
    Inc(Result, WriteBytes(0, 2));
end;

procedure TsgDWGBits.CheckSize;
begin
  if (FByte >= FAllocatedBytes - 1) or (FAllocatedBytes = 0)  then
  begin
    Inc(FAllocatedBytes);
    if FAllocatedBytes >= FCapacity then
      AllocBits(True);
  end;
end;

procedure TsgDWGBits.CheckTsgFloatSize;
begin
  if SizeOf(TsgFloat) <> SizeOf(Double) then
    raise Exception.Create(sFloatSizeError);
end;

procedure TsgDWGBits.Clear;
begin
  FAllocBlock := cntAllocBlock;
  FCapacity := FAllocBlock;
  FAllocatedBytes := 0;
  FByte := 0;
  FBit := 0;
  FBitSize := 0;
  if FDoDispose then
  begin
    DisposeAndNil(FData);
    AllocBits(False);
  end;
end;

function TsgDWGBits.ColorToDWG(const Value: TsgColorCAD): Cardinal;
begin
  Result := ConvertColorCADToIndexColor(Value, True);
end;

procedure TsgDWGBits.Complete;
begin

end;

constructor TsgDWGBits.Create(const ASize: Cardinal = 0; ASizeInByte: Boolean = True);
begin
  inherited Create;
  FAllocBlock := cntAllocBlock;
  SetSizes(ASize, ASizeInByte);
  FCapacity := ASize + FAllocBlock;
  AllocBits(False);
end;

constructor TsgDWGBits.CreateFromDWGObjectData(const ObjData: Pointer);
var
  vObjSize: Cardinal;
  vReadCRC, vCalculatedCRC: Word;
  vPosition: Cardinal;
begin
  CreateFromMemory(ObjData, 20);
  vObjSize := ReadMS;//size bytes
  SetSizes(vObjSize + Position shr cntMulDiv8 + 2, True); //MS (size) + RS (CRC) 2 bytes
  vPosition := Position;
  vReadCRC := ReadCRC8;
  Position := vPosition;
  vCalculatedCRC := CRC8(FData, cnstObjectCRCInit, Size - 2);
  if vReadCRC <> vCalculatedCRC then
    raise EDWGBitsError.Create(sCRCError);
  InitData;
end;

constructor TsgDWGBits.CreateFromMemory(const Memory: Pointer; const ASize: Cardinal);
begin
  inherited Create;
  FData := Memory;
  SetSizes(ASize, True);
  FCapacity := FAllocatedBytes;
  FDoDispose := False;
  GotoFirstBit;
end;

constructor TsgDWGBits.CreateFromMemoryAndSentinel(const Memory: Pointer;
  const MaxSize: Cardinal; const BeginSentinel, EndSentinel: array of Byte);

  function FindSentinel(const Sentinel: array of Byte; const SentinelSize: Cardinal;
    var PSentinel: PByteArray; const ErrorStr: string): Pointer;
  var
    P: PByte;
    vFoundAdr: TsgNativeUInt;
  begin
    P := PByte(@Sentinel[Low(Sentinel)]);
    Result := StrPosLen(FData, P, Size, SentinelSize, vFoundAdr);
    if Result = nil then
      raise EDWGBitsError.Create(ErrorStr);
    GetMem(PSentinel, SentinelSize);
    Move(Sentinel, PSentinel^, SentinelSize);
  end;

var
  P: PByte;
  vSize, vBeginSentinelSize, vEndSentinelSize: Cardinal;
begin
  vBeginSentinelSize := SizeOf(BeginSentinel);
  vEndSentinelSize := SizeOf(EndSentinel);
  if vBeginSentinelSize = 0 then
    raise EDWGBitsError.Create(sBeginSentinelError);
  CreateFromMemory(Memory, MaxSize);
  P := FindSentinel(BeginSentinel, vBeginSentinelSize, FBeginSentinel,
    sBeginSentinelNotFoundError);
  FData := P;
  Inc(FData, vBeginSentinelSize);
  if vEndSentinelSize <> 0 then
  begin
    P := FindSentinel(EndSentinel, vEndSentinelSize, FEndSentinel,
      sEndSentinelNotFoundError);
    vSize := TsgNativeUInt(P) - TsgNativeUInt(FData);
    SetSizes(vSize, True);
  end;
end;

constructor TsgDWGBits.CreateFromTsgInt64(const AValue: TsgInt64; AOffset: Integer = 0);
begin
  CreateFromMemory(Pointer(Ord2(AValue)), MaxInt);
  Seek(AValue.Lo and $7 + AOffset, soCurrent);
end;

destructor TsgDWGBits.Destroy;
begin
  if FDoDispose then
    FreeMem(FData);
  if Assigned(FBeginSentinel) then
    FreeMem(FBeginSentinel);
  if Assigned(FEndSentinel) then
    FreeMem(FEndSentinel);
  inherited Destroy;
end;

function TsgDWGBits.Eof: Boolean;
begin
  Result := Position = BitSize;
end;

function TsgDWGBits.GetBit: Byte;
begin
  Result := FBit;
end;

function TsgDWGBits.GetBitSize: Cardinal;
begin
  Result := FBitSize;
end;

function TsgDWGBits.GetByte: Cardinal;
begin
  Result := FByte;
end;

function TsgDWGBits.GetByteArray: PByteArray;
begin
  if FData = nil then
     raise EDWGBitsError.Create(sByteArryaAccessError);
  Result := PByteArray(FData);
end;

function TsgDWGBits.GetObjectType: Cardinal;
begin
  Result := FObjectType;
end;

function TsgDWGBits.GetPosition: Cardinal;
begin
  Result := FByte shl cntMulDiv8 + FBit;
end;

function TsgDWGBits.GetSize: Cardinal;
begin
  Result := BitSize shr cntMulDiv8;
  if BitSize mod cntBitPerByte <> 0 then
    Inc(Result);
end;

function TsgDWGBits.GetVersion: TsgDWGVersion;
begin
  Result := acR2000;
end;

procedure TsgDWGBits.GotoLastBit;
begin
  if FBitSize = 0 then
    Position := 0
  else
    Position := FBitSize;
end;

function TsgDWGBits.IsLastByte: Boolean;
begin
  Result := FByte = FAllocatedBytes - 1;
end;

function TsgDWGBits.Read2BD: TFPoint;
begin
  Result.X := ReadBD;
  Result.Y := ReadBD;
  Result.Z := 0.0;
end;

function TsgDWGBits.Read2RD: TFPoint;
begin
  Result.X := ReadRD;
  Result.Y := ReadRD;
  Result.Z := 0.0;
end;

function TsgDWGBits.Read3BD: TFPoint;
begin
  Result.X := ReadBD;
  Result.Y := ReadBD;
  Result.Z := ReadBD;
end;

function TsgDWGBits.Read3DD(const DefValue: TFPoint): TFPoint;
begin
  Result.X := ReadDD(DefValue.X);
  Result.Y := ReadDD(DefValue.Y);
  Result.Z := ReadDD(DefValue.Z);
end;

function TsgDWGBits.Read3RD: TFPoint;
begin
  Result.X := ReadRD;
  Result.Y := ReadRD;
  Result.Z := ReadRD;
end;

function TsgDWGBits.ReadAppInfoText: string;
begin
  Result := InternalReadT(0, False, False, False, 2);
end;

function TsgDWGBits.ReadBB: Byte;
begin
  if FBit < cntLastBitInByte then
    Result := (Data[FByte] and ($C0 shr FBit)) shr (6 - FBit)
  else
  begin
    Result := (Data[FByte] and 1) shl 1;
    if not IsLastByte then
      Result := Result or ((Data[FByte + 1] and $80) shr cntLastBitInByte);
  end;
  IncBitPos(2);
end;

function TsgDWGBits.ReadBD: Double;
begin
  case ReadBB of
    0:  Result := ReadRD;
    1:  Result := 1.0;
  else
    Result := 0.0;
  end;
end;

function TsgDWGBits.ReadBE: TFPoint;
begin
  if (Version >= acR2000) and (ReadBit <> 0) then
  begin
    Result.X := 0.0;
    Result.Y := 0.0;
    Result.Z := 1.0;
  end
  else
  begin
    Result.X := ReadBD;
    Result.Y := ReadBD;
    Result.Z := ReadBD;
  end;
end;

function TsgDWGBits.ReadBit: Byte;
begin
  Result := (Data[FByte] and ($80 shr FBit)) shr (cntLastBitInByte - FBit);
  IncBitPos(1);
end;

function TsgDWGBits.ReadBL: Cardinal;
begin
  case ReadBB of
    0: Result := ReadRL;
    1: Result := ReadRC and $FF;
  else
    Result := 0;
  end;
end;

function TsgDWGBits.ReadBS: Word;
begin
  case ReadBB of
    0: Result := ReadRS;
    1: Result := ReadRC;
    2: Result := 0;
  else
    Result := 256;
  end;
end;

function TsgDWGBits.ReadBT: Double;
begin
  if (Version >= acR2000) and (ReadBit <> 0) then
    Result := 0.0
  else
    Result := ReadBD;
end;

procedure TsgDWGBits.ReadBytes(const Bytes: Pointer; Count: Integer);
var
  I: Integer;
  P: PByte;
begin
  P := Bytes;
  for I := 0 to Count - 1 do
  begin
    P^ := ReadRC;
    Inc(P);
  end;
end;

function TsgDWGBits.ReadBytesHandle(const ABytesCount: Byte): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABytesCount - 1 do
    Result := Result shl 8 + ReadRC;
end;

function TsgDWGBits.ReadCMC: TsgColorCAD;
begin
  Result := MakeColorCAD(acIndexColor, ReadBS)
end;

function TsgDWGBits.ReadCRC8(AReadFromEndPos: Boolean = True; ASwapByte: Boolean = False): Word;
begin
  if AReadFromEndPos then
    Position := BitSize - cntBitPerWord
  else
    while FBit <> 0 do ReadBit;
  Result := ReadRS;
  if ASwapByte then
    Result := SwapBytes(Result);
end;

function TsgDWGBits.ReadDD(const DefValue: Double): Double;

  procedure Read6;
  var
    P: PAnsiChar;
  begin
    P := PAnsiChar(@Result) + 4;
    ReadBytes(P, 2);
    ReadBytes(@Result, 4);
  end;

begin
  Result := DefValue;
  case ReadBB of
    1: ReadBytes(@Result, 4);
    2: Read6;
    3: Result := ReadRD;
  end;
  if IsNan(Result) or (Abs(Result) > 1E+101) then
    Result := 0;
end;

function TsgDWGBits.ReadENCSection(AFlags: Integer): TsgColorCAD;
var
  vUseColorIndex: Boolean;
begin
  Result.Active := acRGBColor;
  Result.Color := clEmpty;
  vUseColorIndex := True;
  if AFlags and $4000 <> 0 then //!!!
  begin
    //vHandle := ReadHandle(APBit, vHCode, Obj.FHandle, Obj);
    // The handle to the color is written in the handle stream.
  end
  else
    if AFlags and $8000 <> 0 then
    begin
      Result := MakeColorCAD(acRGBColor, BGRToRGB(ReadBL));
      vUseColorIndex := False;
    end;
  if AFlags and $2000 <> 0 then
    ReadBL;
  if vUseColorIndex then
    Result := MakeColorCAD(acIndexColor, AFlags and $FFF);
end;

function TsgDWGBits.ReadFileDepListText: string;
begin
  Result := InternalReadT(0, False, False, False, 4);
end;

function TsgDWGBits.ReadHandle(var ACode: Byte; ARef: UInt64 = 0): UInt64;
var
  vCount: Byte;
begin
  vCount := ReadRC;
  ACode := vCount shr 4;
  Result := ReadBytesHandle(vCount and $F);
  case ACode of
    6: Result := ARef + 1;
    8: Result := ARef - 1;
    $A: Result := ARef + Result;
    $C: Result := ARef - Result;
  end;
end;

function TsgDWGBits.ReadJulianDateIntAsTimeStamp(AReadMillSec: Boolean;
  AReadBL: Boolean = True): TTimeStamp;
var
  vJDDay, vJDMilSecInDay: Cardinal;
  vFuncRead: function: Cardinal of object;
begin
  vFuncRead := ReadRL;
  if AReadBL then
    vFuncRead := ReadBL;
  vJDDay := vFuncRead;
  if AReadMillSec then
    vJDMilSecInDay := vFuncRead
  else
    vJDMilSecInDay := 0;
  Result.Date := vJDDay;
  Result.Time := vJDMilSecInDay;
end;

function TsgDWGBits.ReadJulianDateIntAsDateTime(AReadMillSec: Boolean;
  AReadBL: Boolean = True): TDateTime;
var
  vTimeStamp: TTimeStamp;
begin
  vTimeStamp := ReadJulianDateIntAsTimeStamp(AReadMillSec, AReadBL);
  Result := JulianTimeStampToDateTime(vTimeStamp, Now);
end;

function TsgDWGBits.ReadL: Cardinal;
var
  vBytes: array[0..3] of Byte absolute Result;
  I: Integer;
begin
  for I := 3 downto 0 do
    vBytes[I] := ReadRC;
end;

function TsgDWGBits.ReadMC: Integer;
var
  vNext: Integer;
  vShift: Byte;
begin
  Result := 0;
  vShift := 0;
  {$IFNDEF SGDEL_10_SEATTLE}
  vNext := 0;
  {$ENDIF}
  while True do
  begin
    vNext := ReadRC;
    if vNext and $80 = 0 then Break;
    Result := Result or (vNext and $7F) shl vShift;
    Inc(vShift, 7);
  end;
  Result := Result or (vNext and $3F) shl vShift;
  if vNext and $40 <> 0 then Result := -Result;
end;

function TsgDWGBits.ReadMCHandle: UInt64;
var
  vNext: UInt64;
  vShift: Byte;
begin
  Result := 0;
  vShift := 0;
  repeat
    vNext := ReadRC;
    if vShift <= 63 then
      Result := Result or (vNext and $7F) shl vShift;
    Inc(vShift, 7);
  until vNext and $80 = 0;
end;

function TsgDWGBits.ReadMS: Cardinal;
var
  vFirstByte, vSecondByte: Byte;
  vResult: UInt64;
  I: Integer;
begin
  I := 0;
  vResult := 0;
  repeat
    vFirstByte := ReadRC;
    vSecondByte := ReadRC;
    vResult := vResult + ((((vSecondByte shl 8) + vFirstByte) and 32767) shl (15 * I));
    Inc(I);
  until vSecondByte <= 127;
  Result := vResult;
end;

function TsgDWGBits.ReadOT: Word;
begin
  Result := ReadBS;
end;

function TsgDWGBits.ReadRC: Byte;
begin
  if FBit = 0 then
    Result := Data[FByte]
  else
  begin
    Result := Data[FByte] shl FBit;
    if not IsLastByte then
      Result := Result or (Data[FByte + 1] shr (cntBitPerByte - FBit));
  end;
  IncBitPos(8);
end;

function TsgDWGBits.ReadRD: Double;
var
  vBytes: array[0..7] of Byte absolute Result;
  I: Integer;
begin
  for I := 0 to 7 do
    vBytes[I] := ReadRC;
  if IsNan(Result) or (Abs(Result) > 1E+101) then
    Result := 0;
end;

function TsgDWGBits.ReadRL: Cardinal;
begin
  Result := ReadRS;
  Result := (Cardinal(ReadRS) shl cntBitPerWord) or Word(Result);
end;

function TsgDWGBits.ReadRS: Word;
begin
  Result := ReadRC;
  Result := (Word(ReadRC) shl cntBitPerByte) or Byte(Result);
end;

function TsgDWGBits.ReadSummaryInfoText: string;
begin
  Result := InternalReadT(0, False, False, False, 2, False);
end;

function TsgDWGBits.ReadT(ACodePage: Integer = 0; AParseUnicode: Boolean = True): string;
begin
  Result := InternalReadT(ACodePage, AParseUnicode, False, False);
end;

function TsgDWGBits.ReadTV(ACodePage: Integer = 0; AParseUnicode: Boolean = True): string;
begin
  Result := ReadT(ACodePage, AParseUnicode)
end;

function TsgDWGBits.ReadXRecordText(XRecordStringConv: Boolean; AParseUnicode: Boolean = True): string;
begin
  Result := InternalReadT(0, AParseUnicode, True, XRecordStringConv);
end;

procedure TsgDWGBits.GotoFirstBit;
begin
  FByte := 0;
  FBit := 0;
end;

function TsgDWGBits.Seek(BitOffset: Integer; Origin: TSeekOrigin = soCurrent): Boolean;
begin
  Result := SeekInBits(BitOffset, Origin);
end;

function TsgDWGBits.SeekInBits(BitOffset: Integer; Origin: TSeekOrigin): Boolean;
begin
  Result := False;
  if FBitSize = 0 then Exit;

  case Origin of
    soBeginning:
      begin
        if (BitOffset < 0) or (BitOffset > FBitSize) then Exit;
        GotoFirstBit;
      end;
    soCurrent:
      begin

        if (BitOffset < 0) then
        begin
          BitOffset := Max(Int64(Position) - Abs(BitOffset), 0);
          GotoFirstBit;
        end
        else
          if Position + Cardinal(BitOffset) > FBitSize then
            BitOffset := FBitSize - Position;

      end;
    soEnd:
      begin
        if (BitOffset > 0) or (BitOffset > FBitSize) then Exit;
        GotoFirstBit;
        BitOffset := FBitSize - Abs(BitOffset);
      end;
  end;
  IncBitPos(BitOffset);
end;

function TsgDWGBits.SeekInBytes(ByteOffset: Integer; Origin: TSeekOrigin = soCurrent): Boolean;
begin
  Result := Seek(ByteOffset shl 3, Origin);
end;

procedure TsgDWGBits.SetAllocBlock(const Value: Cardinal);
begin
  if (Value <> FAllocBlock) and (Value > cntAllocBlock) then
    FAllocBlock := Value;
end;

procedure TsgDWGBits.SetPosition(const Value: Cardinal);
begin
  SeekInBits(Value, soBeginning);
end;

procedure TsgDWGBits.SetSizes(ASize: Cardinal; ASizeInByte: Boolean);
begin
  if ASizeInByte then
  begin
    FAllocatedBytes := ASize;
    FBitSize := Int64(ASize) shl cntMulDiv8;
  end
  else
  begin
    FAllocatedBytes := ASize div cntBitPerByte;
    if ASize mod cntBitPerByte <> 0 then
      Inc(FAllocatedBytes);
    FBitSize := ASize;
  end;
end;

function TsgDWGBits.TosgInt64: TsgInt64;
begin
  InitInt64(Pointer(TsgNativeUInt(Data) + CurByte), Result);
  Result.Lo := Result.Lo or CurBit;
end;

function TsgDWGBits.UpdateRC(const Offset: Cardinal; const Value: Byte): Integer;
begin
  Result := InternalUpdateValue(@Value, Offset, SizeOf(Value));
end;

function TsgDWGBits.UpdateRL(const Offset, Value: Cardinal): Integer;
begin
  Result := InternalUpdateValue(@Value, Offset, SizeOf(Value));
end;

function TsgDWGBits.UpdateRS(const Offset: Cardinal; const Value: Word): Integer;
begin
  Result := InternalUpdateValue(@Value, Offset, SizeOf(Value));
end;

function TsgDWGBits.Write2RD(const Value: TF2DPoint): Integer;
begin
  Result := WriteRD(Value.X);
  Inc(Result, WriteRD(Value.Y));
end;

function TsgDWGBits.Write2BD(const Value: TF2DPoint): Integer;
begin
  Result := WriteBD(Value.X);
  Inc(Result, WriteBD(Value.Y));
end;

function TsgDWGBits.Write2BD(const Value: TFPoint): Integer;
begin
  Result := Write2BD(MakeF2DPointFrom3D(Value));
end;

function TsgDWGBits.Write2RD(const Value: TFPoint): Integer;
begin
  Result := Write2RD(MakeF2DPointFrom3D(Value));
end;

function TsgDWGBits.Write3BD(const Value: TFPoint): Integer;
begin
  Result := WriteBD(Value.X);
  Inc(Result, WriteBD(Value.Y));
  Inc(Result, WriteBD(Value.Z));
end;

function TsgDWGBits.Write3BD(const Value: TF2DPoint): Integer;
begin
  Result := Write2BD(Value);
  Inc(Result, WriteBD(0));
end;

function TsgDWGBits.Write3BDPreExtruded(const Value, Extrusion: TFPoint): Integer;
var
  vValue: TFPoint;
begin
  vValue := Value;
  if Extruded(Extrusion) then
    DoExtrusion(vValue, Extrusion);
  Result := Write3BD(vValue);
end;

function TsgDWGBits.Write3Bits(const Value: Byte): Integer;
var
  I, vShift, vVal: Byte;
begin
  vShift := 0;
  Result := 0;
  vVal := Min(Value, 7) shl 5;
  for I := 0 to 2 do
  begin
    vVal := vVal shl vShift;
    if vVal and $80 <> 0 then
      Result := Result + WriteBit(1)
    else
      Result := Result + WriteBit(0);
    if vShift = 0 then vShift := 1;
  end;
end;

function TsgDWGBits.Write3DD(const Value, DefValue: TFPoint): Integer;
begin
  Result := WriteDD(Value.X, DefValue.X);
  Inc(Result, WriteDD(Value.Y, DefValue.Y));
  Inc(Result, WriteDD(Value.Z, DefValue.Z));
end;

function TsgDWGBits.Write3RD(const Value: TFPoint): Integer;
begin
  Result := WriteRD(Value.X);
  Inc(Result, WriteRD(Value.Y));
  Inc(Result, WriteRD(Value.Z));
end;

function TsgDWGBits.WriteAppInfoText(const Value: string): Integer;
begin
  Result := InternalWriteT(Value, True, False, 0, 2, False);
end;

function TsgDWGBits.WriteBB(const Value: Byte): Integer;
begin
  CheckSize;
  if FBit < cntLastBitInByte then
    Data[FByte] := (Data[FByte] and (not ($C0 shr FBit))) or (Value shl (6 - FBit))
  else
  begin
    Data[FByte] := (Data[FByte] and $FE) or (Value shr 1);
    if not IsLastByte then
      Data[FByte + 1] := (Data[FByte + 1] and $7F) or ((Value and 1) shl cntLastBitInByte);
  end;
  Result := 2;
  IncBitPos(Result, True);
  if Position >= FBitSize then
    Inc(FBitSize, Result);
end;

function TsgDWGBits.WriteBD(const Value: Double): Integer;
begin
  if Value = 0.0 then
    Result := WriteBB(2)
  else
    if Value = 1.0 then
      Result := WriteBB(1)
    else
    begin
      Result := WriteBB(0);
      Inc(Result, WriteRD(Value));
    end;
end;

function TsgDWGBits.WriteBE(const Value: TFPoint): Integer;
begin
  CheckTsgFloatSize;
  if IsEqualFPoints(Value, cnstExtrusion)  then
    Result := WriteBit(1)
  else
  begin
    Result := WriteBit(0);
    Inc(Result, WriteBD(Value.X));
    Inc(Result, WriteBD(Value.Y));
    Inc(Result, WriteBD(Value.Z));
  end;
end;

function TsgDWGBits.WriteBit(const Value: Boolean): Integer;
begin
  Result := WriteBit(BoolToBit(Value));
end;

function TsgDWGBits.WriteBit(const Value: Byte): Integer;
begin
  CheckSize;
  if Value <> 0 then
    Data[FByte] := Data[FByte] or ($80 shr FBit)
  else
    Data[FByte] := Data[FByte] and (not ($80 shr FBit));
  Result := 1;
  IncBitPos(Result, True);
  if Position >= FBitSize then
    Inc(FBitSize, Result);
end;

function TsgDWGBits.WriteBL(const Value: Cardinal): Integer;
begin
  if Value > 255 then
  begin
    Result := WriteBB(0);
    Inc(Result, WriteRL(Value));
  end
  else
    if Value = 0 then
      Result := WriteBB(2)
    else
    begin
      Result := WriteBB(1);
      Inc(Result, WriteRC(Value));
    end;
end;

function TsgDWGBits.WriteBS(const Value: Word): Integer;
begin
  if Value > 256 then
  begin
    Result := WriteBB(0);
    Inc(Result, WriteRS(Value));
  end
  else
  begin
    if Value = 0 then
	    Result := WriteBB(2)
    else
      if Value = 256 then
	      Result := WriteBB(3)
      else
      begin
        Result := WriteBB(1);
        Inc(Result, WriteRC(Value));
      end;
  end;
end;

function TsgDWGBits.WriteBT(const Value: Double): Integer;
begin
  if IsZero(Value) then
    Result := WriteBit(1)
  else
  begin
    Result := WriteBit(0);
    Inc(Result, WriteBD(Value));
  end;
end;

function TsgDWGBits.WriteBytes(const Byte: Byte; Count: Cardinal): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + WriteRC(Byte);
end;

function TsgDWGBits.WriteBytes(const Bytes: Pointer; Count: Cardinal;
  AFromBack: Boolean = False): Integer;
var
  P: PByte;
  I: Integer;
begin
  P := Bytes;
  Result := 0;
  if AFromBack then
    Inc(P, Count - 1);
  for I := 0 to Count - 1 do
  begin
    Result := Result + WriteRC(P^);
    if AFromBack then
      Dec(P)
    else
      Inc(P);
  end;
end;

function TsgDWGBits.WriteCMC(const Value: TsgColorCAD): Integer;
begin
  Result := WriteENC(Value, 0);
end;

function TsgDWGBits.WriteCMCDirectly(const AValue: TsgColorCAD): Integer;
begin
  case AValue.Active of
    acIndexColor:
      begin
        case AValue.Color of
          clDXFByBlock: Result := WriteBL($C1000000);
          clDXFByLayer: Result := WriteBL($C0000000);
        else
          Result := WriteBL($C3000000 or AValue.Color and $1FF);
        end;
      end;
  else  //acRGBColor:
    Result := WriteBL($C2000000 or Cardinal(BGRToRGB(AValue.Color)));
  end;
end;

function TsgDWGBits.WriteColorFlags(const AColorFlags: Word): Integer;
begin
  Result := WriteBS(AColorFlags);
end;

function TsgDWGBits.WriteCRC8(const Init: Word; ASwapBytes: Boolean = False): Integer;
var
  vCRC: Word;
begin
  //GotoLastBit;
  while FBit <> 0 do
    WriteBit(0);
  vCRC := CRC8(FData, Init, Size);
  if ASwapBytes then
    vCRC := SwapBytes(vCRC);
  Result := WriteRS(vCRC);
end;

function TsgDWGBits.WriteDD(const Value, DefValue: Double): Integer;
type
  TsgSplitDouble = packed record
    P1: Cardinal;
    P2: Word;
    P3: Word;
  end;
var
  vValue: TsgSplitDouble absolute Value;
  vDefValue: TsgSplitDouble absolute DefValue;
begin
  if sgIsZero(Value - DefValue, fExtendedResolution) then
    Result := WriteBB(0)
  else
    if vValue.P3 = vDefValue.P3 then
    begin
      if vValue.P2 = vDefValue.P2 then
        Result := WriteBB(1)
      else
      begin
        Result := WriteBB(2);
        Inc(Result, WriteRS(vValue.P2));
      end;
      Inc(Result, WriteRL(vValue.P1));
    end
    else
    begin
      Result := WriteBB(3);
      Inc(Result, WriteRD(Value));
    end;
end;

function TsgDWGBits.WriteENC(const Value: TsgColorCAD; ATransparency: Integer): Integer;
var
  vColorFlags: Word;
begin
  Result := WriteColorFlags(ColorToDWG(Value));
  vColorFlags := $8000;
  if ATransparency and $03000000 <> 0 then
    vColorFlags := vColorFlags or $2000;
  Result := Result + WriteENCSection(vColorFlags, Value, ATransparency);
end;

function TsgDWGBits.WriteENCSection(const AColorFlags: Word;
  const AValue: TsgColorCAD; ATransparency: Integer): Integer;
begin
  Result := 0;
end;

function TsgDWGBits.WriteEntHandle(const Handle: UInt64;
  const Code: Byte): Integer;
begin
  Result := InternalWriteHandle(Handle, Code, False);
end;

function TsgDWGBits.WriteFileDepListText(const Value: string): Integer;
begin
  Result := InternalWriteT(Value, False, False, 0, 4, False);
end;

function TsgDWGBits.WriteHandle(const Handle: UInt64; const Code: Byte): Integer;
begin
  Result := InternalWriteHandle(Handle, Code, False);
end;

function TsgDWGBits.WriteHandleRecord(const Handle: UInt64; Code: Byte): Integer;
begin
  Result := InternalWriteHandle(Handle, Code, True);
end;

function TsgDWGBits.WriteJulianDateInt(const ADateTime: TTimeStamp; AWriteBL,
  AWriteMilSec: Boolean): Integer;
var
  vFuncWrite: function (const Value: Cardinal): Integer of object;
begin
  vFuncWrite := WriteRL;
  if AWriteBL then
    vFuncWrite := WriteBL;
  Result := vFuncWrite(Cardinal(ADateTime.Date));
  if AWriteMilSec then
    Inc(Result, vFuncWrite(Cardinal(ADateTime.Time)));
end;

function TsgDWGBits.WriteJulianDateInt(const ADateTime: TDateTime;
  AWriteBL, AWriteMilSec: Boolean): Integer;
var
  vTimeStamp: TTimeStamp;
begin
  vTimeStamp := DateTimeToJulianTimeStamp(ADateTime);
  Result := WriteJulianDateInt(vTimeStamp, AWriteBL,
    AWriteMilSec);
end;

function TsgDWGBits.WriteL(const Value: Cardinal): Integer;
var
  vBytes: array[0..3] of Byte absolute Value;
  I: Integer;
begin
  Result := 0;
  for I := 3 downto 0 do
    Result := Result + WriteRC(vBytes[I]);
end;

function TsgDWGBits.WriteMC(const Value: Integer): Integer;
var
  I, J, vIndex: Integer;
  vBytes: array[0..4] of Byte;
  vAbs, vMask: Cardinal;
begin
  Result := 0;
  vMask := $0000007F;
  vAbs := Abs(Value);
  J := 0;
  for I := High(vBytes) downto Low(vBytes) do
  begin
    vBytes[I] := Byte(((vAbs and vMask) shr J)) or $80;
    vMask := vMask shl cntLastBitInByte;
    Inc(J, cntLastBitInByte);
  end;
  vIndex := Low(vBytes);
  for I := Low(vBytes) to High(vBytes) do
    if (vBytes[I] and $7F) <> 0 then
    begin
      vIndex := I;
      Break;
    end;
  if (vBytes[vIndex] and $40) <> 0 then
    Dec(vIndex);
  if vIndex < Low(vBytes) then
    vIndex := Low(vBytes);
  vBytes[vIndex] := vBytes[vIndex] and $7F;
  if Value < 0 then
    vBytes[vIndex] := vBytes[vIndex] or $40;
  for J := High(vBytes) downto vIndex do
    Result := Result + WriteRC(vBytes[J]);
end;

function TsgDWGBits.WriteMCHandle(const Value: UInt64): Integer;
var
  vValue: UInt64;
  vVal: array[0..SizeOf(Value) + 2] of Byte;
  vIndex, vCount: Integer;
  vByte: Byte;
begin
  vValue := Value;
  Result := 0;
  vIndex := 0;
  vCount := 0;
  FillChar(vVal, SizeOf(vVal), 0);
  repeat
    vVal[vIndex] := vValue and $7F;
    vValue := vValue shr 7;
    if vValue <> 0 then
      vByte := vVal[vIndex] or $80
    else
      vByte := vVal[vIndex];
    if vIndex < High(vVal) then
      vVal[vIndex] := vByte;
    Inc(vIndex);
    Inc(vCount);
  until vValue = 0;
  Result := Result + WriteBytes(@vVal[0], vCount);
end;

function TsgDWGBits.WriteMS(const Value: Cardinal): Integer;
var
  vValue: UInt64;
  vVal: array[0..SizeOf(Value) div 2 + 2] of Word;
  vIndex, vCount: Integer;
  vWord: Word;
begin
  vValue := Value;
  Result := 0;
  vIndex := 0;
  vCount := 0;
  FillChar(vVal, SizeOf(vVal), 0);
  repeat
    vVal[vIndex] := vValue and $7FFF;
    vValue := vValue shr 15;
    if vValue <> 0 then
      vWord := vVal[vIndex] or $8000
    else
      vWord := vVal[vIndex];
    if vIndex < High(vVal) then
      vVal[vIndex] := vWord;
    Inc(vIndex);
    Inc(vCount);
  until vValue = 0;
  Result := Result + WriteBytes(@vVal[0], vCount * 2);
end;

function TsgDWGBits.WriteOT(const Value: Word): Integer;
begin
  Result := WriteBS(Value);
end;

function TsgDWGBits.WriteRC(const Value: Byte): Integer;
begin
  CheckSize;
  if FBit = 0 then
    Data[FByte] := Value
  else
  begin
    Data[FByte] := (Data[FByte] and (cnstMaxByte shl (cntBitPerByte - FBit))) or (Value shr FBit);
    if not IsLastByte then
      Data[FByte + 1] := (Data[FByte + 1] and (cnstMaxByte shr FBit)) or (Value shl (cntBitPerByte - FBit));
  end;
  Result := cntBitPerByte;
  IncBitPos(Result, True);
  if Position >= FBitSize then
    Inc(FBitSize, Result);
end;

function TsgDWGBits.WriteRC(const Value: AnsiChar): Integer;
begin
  Result := WriteRC(Byte(Value));
end;

function TsgDWGBits.WriteRD(const Value: Double): Integer;
var
  vVal: array[0..SizeOf(Double) - 1] of Byte absolute Value;
begin
  Result := WriteBytes(@vVal[0], SizeOf(vVal));
end;

{$IFDEF SGDEL_2005}
function TsgDWGBits.WriteRL(const Value: Integer): Integer;
begin
  Result := WriteRL(Cardinal(Value));
end;
{$ENDIF}

function TsgDWGBits.WriteRL(const Value: Cardinal): Integer;
begin
  Result := WriteRS(Value and cnstMaxWord);
  Inc(Result, WriteRS(Value shr (cntBitPerByte shl 1)));
end;

function TsgDWGBits.WriteRL0: Integer;
begin
  Result := WriteRL(Cardinal(0));
end;

function TsgDWGBits.WriteRS(const Value: Word): Integer;
begin
  Result := WriteRC(Value and cnstMaxByte);
  Inc(Result, WriteRC(Value shr cntBitPerByte));
end;

function TsgDWGBits.WriteSentinel(const Value: array of Byte): Integer;
var
  vSizeOfVal: Cardinal;
begin
  vSizeOfVal := SizeOf(Value);
  if vSizeOfVal > 0 then
    Result := WriteBytes(@Value[Low(Value)], vSizeOfVal)
  else
    Result := 0;
end;

function TsgDWGBits.WriteSummaryInfoText(const Value: string): Integer;
begin
  Result := InternalWriteT(Value, True, False, 0, 2, False, True);
end;

function TsgDWGBits.WriteT(const Value: string; CodePage: Integer;
  WriteNullTerminator: Boolean = False): Integer;
begin
  Result := InternalWriteT(Value, WriteNullTerminator, False, CodePage);
end;

function TsgDWGBits.SaveToStream(Stream: TStream): Integer;
begin
  Result := Stream.Write(FData^, Size);
end;

function TsgDWGBits.WriteTV(const Value: string; CodePage: Integer;
  WriteNullTerminator: Boolean = False): Integer;
begin
  Result := WriteT(Value, CodePage, WriteNullTerminator);
end;

function TsgDWGBits.WriteUInt64(const Value: UInt64): Integer;
var
  vBytes: array[0..SizeOf(UInt64) - 1] of Byte absolute Value;
begin
  Result := WriteBytes(@vBytes[0], SizeOf(Value));
end;

function TsgDWGBits.WriteXDirection(HasXDirection: Boolean; const Point: TFPoint): Integer;
begin
  if HasXDirection then
    Result := Write3BD(Point)
  else
    Result := Write3BD(cnstXOrtAxis);
end;

function TsgDWGBits.WriteXDirection(const Angle: Double; const Extrusion: TFPoint): Integer;
var
  vSin, vCos: Extended;
  vXDir: TFPoint;
begin
  vXDir := cnstFPointZero;
  if IsZero(Angle) then
    Result := WriteXDirection(False, vXDir)
  else
  begin
    SinCos(Radian(Angle), vSin, vCos);
    vXDir := MakeFpoint(vCos, vSin, 0);
    DoExtrusion(vXDir, Extrusion);
    Result := WriteXDirection(True, vXDir);
  end;
end;

function TsgDWGBits.WriteXRecordText(const Value: string; CodePage: Integer;
  XRecordStringConv: Boolean; WriteNullTerminator: Boolean = False): Integer;
begin
   Result := InternalWriteT(Value, WriteNullTerminator, True, CodePage);
end;

function TsgDWGBits.WriteBit(const Value, Mask: Byte): Integer;
begin
  Result := WriteBit((Value and Mask) <> 0);
end;

{ TsgDWG2004Bits }

function TsgDWG2004Bits.WriteCMCSection(const AColorFlags: Word;
  const AValue: TsgColorCAD): Integer;
begin
  Result := WriteCMCDirectly(AValue);
  // future release
  // color byte:
  //   $01 => color name follows (TV)
  //   $02 => book name follows (TV)
  Result := Result + WriteRC(0);
end;

function TsgDWG2004Bits.GetVersion: TsgDWGVersion;
begin
  Result := acR2004;
end;

function TsgDWG2004Bits.WriteCMC(const Value: TsgColorCAD): Integer;
var
  vColorFlags: Word;
begin
  vColorFlags := 0;
  Result := WriteColorFlags(vColorFlags);
  // if vColorFlags = 0 then
  Result := Result + WriteCMCSection(vColorFlags, Value);
//  else
//    Result := Result + WriteENCSection($8000, Value);
end;

function TsgDWG2004Bits.WriteENC(const Value: TsgColorCAD; ATransparency: Integer): Integer;
var
  vColorFlags: Word;
begin
  vColorFlags := $8000;
  if ATransparency and $03000000 <> 0 then
    vColorFlags := vColorFlags or $2000;
  Result := WriteColorFlags(vColorFlags);
  Inc(Result, WriteENCSection(vColorFlags, Value, ATransparency));
end;

function TsgDWG2004Bits.WriteENCSection(const AColorFlags: Word;
  const AValue: TsgColorCAD; ATransparency: Integer): Integer;
begin
  Result := 0;
  // if AColorFlags and $C000 = $C000 then handle to the color
  // is written in the handle stream.
  if AColorFlags and $C000 = $8000 then
    Result := WriteCMCDirectly(AValue);
  // The first byte represents the transparency type:
  //   0 = BYLAYER,
  //   1 = BYBLOCK,
  //   3 = the transparency value in the last byte.
  if AColorFlags and $2000 <> 0 then
    Inc(Result, WriteBL(ATransparency));
end;

function GetDWGBitsClassByVersion(const DWGVersion: TsgDWGVersion): TsgDWGBitsClass;
begin
  case DWGVersion of
    acR2000:
      Result := TsgDWGBits;
    acR2004:
      Result := TsgDWG2004Bits;
    acR2007:
      Result := TsgDWG2007Bits;
    acR2010:
      Result := TsgDWG2010Bits;
    acR2013:
      Result := TsgDWG2013Bits;
    acR2018:
      Result := TsgDWG2018Bits;
  else
    Result := nil;
  end;
end;

function CreateDWGBitsByVersionInternal(const DWGVersion: TsgDWGVersion;
  const Size: Cardinal; SizeInByte: Boolean; const ObjData: Pointer;
  const Memory: Pointer; const BeginSentinel, EndSentinel: array of Byte): TsgDWGBits; overload;
var
  vDWGBitsClass: TsgDWGBitsClass;
begin
  Result := nil;
  vDWGBitsClass := GetDWGBitsClassByVersion(DWGVersion);
  if vDWGBitsClass = nil then
    Exit;
  if SizeOf(BeginSentinel) <> 0 then
  begin
    if Memory <> nil then
      Result := vDWGBitsClass.CreateFromMemoryAndSentinel(Memory, Size, BeginSentinel, EndSentinel)
  end
  else
    if Memory <> nil then
      Result := vDWGBitsClass.CreateFromMemory(Memory, Size)
    else
      if ObjData <> nil then
        Result := vDWGBitsClass.CreateFromDWGObjectData(ObjData)
      else
        if ObjData <> nil then
          Result := vDWGBitsClass.CreateFromDWGObjectData(ObjData)
        else
          Result := vDWGBitsClass.Create(Size, SizeInByte);
end;

function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion; const Size: Cardinal = 0;
  SizeInByte: Boolean = True): TsgDWGBits;
begin
  Result := CreateDWGBitsByVersionInternal(DWGVersion, Size, SizeInByte, nil,
    nil, [], []);
end;

function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const ObjData: Pointer): TsgDWGBits;
begin
  Result := CreateDWGBitsByVersionInternal(DWGVersion, 0, True, ObjData, nil, [],
    []);
end;

function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const Memory: Pointer; const Size: Cardinal): TsgDWGBits;
begin
  Result := CreateDWGBitsByVersionInternal(DWGVersion, Size, True, nil, Memory,
    [], []);
end;

function CreateDWGBitsByVersion(const DWGVersion: TsgDWGVersion;
  const Memory: Pointer; const MaxSize: Cardinal;
  const BeginSentinel, EndSentinel: array of Byte): TsgDWGBits;
begin
  Result := CreateDWGBitsByVersionInternal(DWGVersion, MaxSize, True, nil, Memory,
    BeginSentinel, EndSentinel);
end;

{ TsgDWG2007Bits }

function TsgDWG2007Bits.GetVersion: TsgDWGVersion;
begin
  Result := acR2007;
end;

procedure TsgDWG2007Bits.InitData;
var
  vSize, vStartBit, vObjectDataBegins: Cardinal;
  vHandlesSize, vHandlesPosition: Cardinal;
  vStrsSize, vStrsStartBit: Cardinal;

  procedure CopyBytes(const PositionFrom, SizeInBit: Cardinal; const BitsDest: TsgDWGBits);
  var
    P: Pointer;
    vSizeInByte: Cardinal;
  begin
    vSizeInByte := (SizeInBit shr cntMulDiv8);
    if SizeInBit mod cntBitPerByte <> 0 then
      Inc(vSizeInByte);
    GetMem(P, vSizeInByte);
    try
      Position := PositionFrom;
      if Position + SizeInBit <= BitSize then
      begin
        ReadBytes(P, vSizeInByte);
        BitsDest.WriteBytes(P, vSizeInByte);
        BitsDest.GotoFirstBit;
      end;
    finally
      FreeMem(P);
    end;
  end;

begin
  CheckAndCreateBits(FStrsBits);
  CheckAndCreateBits(FHandlesBits);
  GotoFirstBit;
  vHandlesSize := 0;
  vSize := ReadMS;
  vStartBit := Position;
  if Version >= acR2010 then
  begin
    vHandlesSize := ReadMCHandle;
    vStartBit := Position;
  end;
  FObjectType := ReadOT;
  vObjectDataBegins := Position;

  //handles
  if Version >= acR2010 then
    vHandlesPosition := (vSize shl cntMulDiv8) - vHandlesSize
  else
    vHandlesPosition := ReadRL;

  Position := vStartBit;
  Seek(vHandlesPosition, soCurrent);
  vHandlesPosition := Position;
  CopyBytes(vHandlesPosition, vHandlesSize, FHandlesBits);

  //strings
  Position := vHandlesPosition - 1;
  if ReadBit <> 0 then //strings stream exists
  begin
    Seek(-17, soCurrent);
    vStrsStartBit := Position;
    vStrsSize := ReadRS;
    if vStrsSize  and $8000 <> 0 then
    begin
      Seek(-32, soCurrent);
      vStrsStartBit := Position;
      vStrsSize := vStrsSize and $7FFF;
      vStrsSize := (Word(ReadRS) shl $F) or vStrsSize;
    end;
    Position := vStrsStartBit;
    Seek(-vStrsSize, soCurrent);
    CopyBytes(Position, vStrsSize, FStrsBits);
    FStrsBits.GotoFirstBit;
  end;
  Position := vObjectDataBegins;
end;

function TsgDWG2007Bits.ReadAppInfoText: string;
begin
  Result := InternalReadT(0, False, False, False, 2, True);
end;

function TsgDWG2007Bits.ReadSummaryInfoText: string;
begin
  Result := InternalReadT(0, False, False, False, 2, True);
end;

procedure TsgDWG2007Bits.CheckAndCreateBits(var Bits: TsgDWGBits);
begin
  FreeAndNil(Bits);
  Bits := CreateDWGBitsByVersion(Version);
end;

procedure TsgDWG2007Bits.Clear;
begin
  ClearStrs;
  FreeAndNil(FStrsBits);
  FreeAndNil(FHandlesBits);
  inherited Clear;
end;

procedure TsgDWG2007Bits.ClearStrs;
var
  I: Integer;
begin
  for I := Low(FUnicodeStrs) to High(FUnicodeStrs) do
    FUnicodeStrs[I] := '';
  SetLength(FUnicodeStrs, 0);
end;

procedure TsgDWG2007Bits.Complete;
var
  I: Integer;
  vStrStreamBitPos: Cardinal;
  vStrStreamBitSize: Cardinal;
  vHasStringsBit: Boolean;
begin
//  GotoLastBit;
  vStrStreamBitPos := Position;
  vHasStringsBit := Length(FUnicodeStrs) > 0;
  if vHasStringsBit then
  begin
    for I := Low(FUnicodeStrs) to High(FUnicodeStrs) do
      InternalWriteTU(FUnicodeStrs[I], WriteBS);
    ClearStrs;
    vStrStreamBitSize := Position - vStrStreamBitPos;
    if vStrStreamBitSize > $7FFF then
    begin
      WriteRS((vStrStreamBitSize shr 15) and $FFFF);
      vStrStreamBitSize := (vStrStreamBitSize or $8000) and $FFFF;
    end;
    WriteRS(vStrStreamBitSize);
  end;
  WriteBit(vHasStringsBit);
end;

destructor TsgDWG2007Bits.Destroy;
begin
  FreeAndNil(FStrsBits);
  FreeAndNil(FHandlesBits);
  ClearStrs;
  inherited Destroy;
end;

function TsgDWG2007Bits.WriteAppInfoText(const Value: string): Integer;
begin
  Result := InternalWriteTU(sgUnicodeStr(Value), WriteRS, True);
end;

function TsgDWG2007Bits.WriteSummaryInfoText(const Value: string): Integer;
begin
  Result := InternalWriteTU(Value, WriteRS, True);
end;

function TsgDWG2007Bits.WriteT(const Value: string; CodePage: Integer;
  WriteNullTerminator: Boolean): Integer;
begin
  Result := WriteTV(Value, CodePage, WriteNullTerminator);
end;

function TsgDWG2007Bits.WriteTV(const Value: string; CodePage: Integer;
  WriteNullTerminator: Boolean): Integer;
begin
  Result := 0;
  SetLength(FUnicodeStrs, Length(FUnicodeStrs) + 1);
  FUnicodeStrs[High(FUnicodeStrs)] := Value;
end;

function TsgDWG2007Bits.WriteXRecordText(const Value: string; CodePage: Integer;
  XRecordStringConv, WriteNullTerminator: Boolean): Integer;
begin
  Result := InternalWriteTU(Value, WriteRS, WriteNullTerminator);
end;

{ TsgDWG2010Bits }

function TsgDWG2010Bits.GetMCHandleSize(const Value: Cardinal): Integer;
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    Result := vBits.WriteMCHandle(Value);
  finally
    vBits.Free;
  end;
end;

function TsgDWG2010Bits.GetMSSize(const Value: Cardinal): Integer;
var
  vBits: TsgDWGBits;
begin
  vBits :=  CreateDWGBitsByVersion(Version);
  try
    Result := vBits.WriteMS(Value);
  finally
    vBits.Free;
  end;
end;

function TsgDWG2010Bits.GetVersion: TsgDWGVersion;
begin
  Result := acR2010;
end;

function TsgDWG2010Bits.ReadOT: Word;
begin
  Result := 0;
  case ReadBB of
    0:
      Result := ReadRC;
    1:
      Result := ReadRC + $1F0;
    2:
      Result := ReadRS;
    3:
      Result := ReadRS;
  end;
end;

procedure TsgDWG2010Bits.WriteHandlesSize(const HandlesSize: Cardinal);
begin
  InsertMCHandle(0, HandlesSize);
end;

procedure TsgDWG2010Bits.WriteObjectSize(const Size: Cardinal);
begin
  InsertMS(0, Size);
end;

function TsgDWG2010Bits.WriteOT(const Value: Word): Integer;
begin
  if Value <= 255 then
  begin
    Result := WriteBB(0);
    Inc(Result, WriteRC(Value));
  end
  else
    if (Value > $1F0) and (Value - $1F0 <= 255) then
    begin
      Result := WriteBB(1);
      Inc(Result, WriteRC(Value - $1F0));
    end
    else
    begin
      Result := WriteBB(2);
      Inc(Result, WriteRS(Value));
    end;
end;

{ TsgDWG2013Bits }

function TsgDWG2013Bits.GetVersion: TsgDWGVersion;
begin
  Result := acR2013;
end;

{ TsgDWG2018Bits }

function TsgDWG2018Bits.GetVersion: TsgDWGVersion;
begin
  Result := acR2018;
end;

end.

{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{              Utils for DWG files supporting                }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDWGUtils;
{$INCLUDE SGDXF.inc}

interface
uses
  Classes, Math, SysUtils, sgConsts, sgLists, sgComparer, sgFunction
{$IFDEF SGDEL_XE2}
  , System.Types
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  , sgFMXTypes
{$ENDIF}
  ;

type
  TsgDWGCompressorStatus = (cdsCompress, cdsDecompress);
  TsgDWGCompressorError = class(Exception);
  TsgDWGCompressor = class
  private
    FDstCur: PAnsiChar;
    FDstEnd: PAnsiChar;
    FHashTable: TsgPointerList;
    FStream: TMemoryStream;
		FSrcStart: PAnsiChar;
    FSrcCur: PAnsiChar;
    FSrcPrev: PAnsiChar;
		FSrcEnd: PAnsiChar;
    FSrcLen: Cardinal;
    FStatus: TsgDWGCompressorStatus;
    procedure IndexError;
    function GetStream: TMemoryStream;
    procedure CheckHashTableIndex(const Index: Integer);
    function GetHashTableItem(const Index: Integer): Pointer;
    function GetPHashTableItem(const Index: Integer): Pointer;
    procedure SetHashTableItem(const Index: Integer; const Value: Pointer);
  protected
    procedure DoCompressError(IsError: Boolean; const Mes: string);
    function GetHashTableSize: Integer; virtual;
    function GetSrcCurByte(const Index: Integer = 0): Byte;
    procedure Init(AStatus: TsgDWGCompressorStatus; ASource, ADest: PByte;
      ASourceSize, ADestSize: Cardinal); virtual;
    procedure SetDest(Dest: PByte; DestSize: Cardinal);
{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF SGDEL_2005}
    function WriteByte(const Buf): Integer; overload;
{$ENDIF}
{$ENDIF}
    function WriteByte(const Value: Byte): Integer; {$IFNDEF SG_NON_WIN_PLATFORM}overload;{$ENDIF}
    function WriteBytes(const Buf; Count: Integer): Integer;
    property HashTableItem[const Index: Integer]: Pointer read GetHashTableItem
      write SetHashTableItem;
    property DstCur: PAnsiChar read FDstCur;
    property DstEnd: PAnsiChar read FDstEnd;
    property SrcStart: PAnsiChar read FSrcStart;
    property SrcCur: PAnsiChar read FSrcCur;
    property SrcPrev: PAnsiChar read FSrcPrev;
    property SrcEnd: PAnsiChar read FSrcEnd;
    property Status: TsgDWGCompressorStatus read FStatus write FStatus;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Compress(Source: PByte; SourceSize: Cardinal): Integer; virtual; abstract;
    function Decompress(Source, Dest: PByte; SourceSize,
      DestSize: Cardinal): Integer; virtual; abstract;
    property Stream: TMemoryStream read GetStream;
  end;

  TsgDWG2004Compressor = class(TsgDWGCompressor)
  private
    function FindMatch(var MatchLength, MatchOffset: TsgNativeUInt): Boolean;
    function GenHashKey(Value1, Value2, Value3, Value4: Byte): Integer;
    procedure GetCompressed(ALength, AOffset: Integer);
    procedure GetLength(var ALength: Integer; AOpCode, AMask: Integer);
    procedure GetLiteral(AOpCode: Integer);
    procedure GetLongLength(var ALength: Integer);
    procedure GetOffset(var AOpCode1, AOpCode2, AOffset: Integer);
    function WriteLength(OpCode: Byte; Length, Threshold: Integer): Integer;
    function WriteLiteral(LitLength: Cardinal): Integer;
    function WriteLongLength(Value: Integer): Integer;
    function WriteMatch(Offset, Length, LitLength: Integer): Integer;
  protected
    procedure Init(AStatus: TsgDWGCompressorStatus; ASource, ADest: PByte;
      ASourceSize, ADestSize: Cardinal); override;
  public
    function Compress(Source: PByte; SourceSize: Cardinal): Integer; override;
    function Decompress(Source, Dest: PByte; SourceSize,
      DestSize: Cardinal): Integer; override;
  end;

  TsgDWG2010Compressor = class(TsgDWG2004Compressor); //DWG 2010 compression is same as DWG 2004

implementation
const
  cntHashTableSize = $8000;

{ TsgDWGCompressor }

procedure TsgDWGCompressor.CheckHashTableIndex(const Index: Integer);
begin
  if (Index < 0) or (Index >= GetHashTableSize) then
    IndexError;
end;

procedure TsgDWGCompressor.Clear;
begin
  FStream.Clear;
  FHashTable.Clear(True);
  FHashTable.Capacity := GetHashTableSize;
	FSrcStart := nil;
  FSrcCur := nil;
  FSrcPrev := nil;
	FSrcEnd := nil;
  FDstCur := nil;
  FDstEnd := nil;
  FSrcLen := 0;
end;

constructor TsgDWGCompressor.Create;
begin
  FStream := TMemoryStream.Create;
  FHashTable := TsgPointerList.Create;
  FStatus := cdsCompress;
  Clear;
end;

destructor TsgDWGCompressor.Destroy;
begin
  FStream.Free;
  FHashTable.Free;
  inherited Destroy;
end;

procedure TsgDWGCompressor.DoCompressError(IsError: Boolean; const Mes: string);
begin
  if IsError then
    raise TsgDWGCompressorError.Create(Mes);
end;

function TsgDWGCompressor.GetSrcCurByte(const Index: Integer = 0): Byte;
begin
  Result := Byte(FSrcCur[Index]);
end;

function TsgDWGCompressor.GetHashTableItem(const Index: Integer): Pointer;
begin
  CheckHashTableIndex(Index);
  Result := FHashTable[Index];
end;

function TsgDWGCompressor.GetHashTableSize: Integer;
begin
  Result := $8000;
end;

function TsgDWGCompressor.GetPHashTableItem(const Index: Integer): Pointer;
begin
  CheckHashTableIndex(Index);
  Result := FHashTable;
  Inc(PByte(Result), Index * SizeOf(Pointer));
end;

function TsgDWGCompressor.GetStream: TMemoryStream;
begin
  Result := FStream;
end;

procedure TsgDWGCompressor.IndexError;
begin
  raise Exception.Create(sInvalidIndexError);
end;

procedure TsgDWGCompressor.Init(AStatus: TsgDWGCompressorStatus; ASource, ADest: PByte;
  ASourceSize, ADestSize: Cardinal);
begin
  Clear;
  FStatus := AStatus;
	FSrcStart := PAnsiChar(ASource);
  FSrcCur := FSrcStart;
  FSrcPrev := FSrcStart;
	FSrcEnd := FSrcStart;
  Inc(FSrcEnd, ASourceSize);
  FSrcLen := ASourceSize;
  SetDest(ADest, ADestSize);
end;

procedure TsgDWGCompressor.SetDest(Dest: PByte; DestSize: Cardinal);
begin
  FDstCur := PAnsiChar(Dest);
  FDstEnd := FDstCur;
  if FDstEnd <> nil then
    Inc(FDstEnd, DestSize);
end;

procedure TsgDWGCompressor.SetHashTableItem(const Index: Integer;
  const Value: Pointer);
begin
  CheckHashTableIndex(Index);
  FHashTable[Index] := Value;
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
{$IFDEF SGDEL_2005}
function TsgDWGCompressor.WriteByte(const Buf): Integer;
begin
  Result := FStream.Write(Buf, 1);
end;
{$ENDIF}
{$ENDIF}

function TsgDWGCompressor.WriteByte(const Value: Byte): Integer;
begin
  Result := FStream.Write(Value, 1);
end;

function TsgDWGCompressor.WriteBytes(const Buf; Count: Integer): Integer;
begin
  Result := FStream.Write(Buf, Count);
end;

{ TsgDWG2004Compressor }

function TsgDWG2004Compressor.Compress(Source: PByte;
  SourceSize: Cardinal): Integer;
var
  vLastMatchOffset, vMatchOffset, vLastMatchLen, vMatchLength: TsgNativeUInt;
  vLitLength: TsgNativeUInt;
begin
  Init(cdsCompress, Source, nil, SourceSize, 0);
  vLastMatchOffset := 0;
  vMatchOffset := 0;
  vLastMatchLen := 0;
  vMatchLength := 0;
  while TsgTypeComparer.CmpPointer(SrcCur, Pointer(TsgNativeUInt(SrcEnd) - $13)) < 0 do
  begin
     if FindMatch(vMatchLength, vMatchOffset) then
     begin
       vLitLength := TsgNativeUInt(SrcCur) - TsgNativeUInt(SrcPrev);
       if vLastMatchLen <> 0 then
         WriteMatch(vLastMatchOffset, vLastMatchLen, vLitLength);
       WriteLiteral(vLitLength);
       Inc(FSrcCur, vMatchLength);
       FSrcPrev := FSrcCur;
       vLastMatchLen := vMatchLength;
       vLastMatchOffset := vMatchOffset;
     end
     else
       Inc(FSrcCur);
  end;
  vLitLength := TsgNativeUInt(SrcEnd) - TsgNativeUInt(SrcPrev);
  if vLastMatchLen <> 0 then
    WriteMatch(vLastMatchOffset, vLastMatchLen, vLitLength);
  WriteLiteral(vLitLength);
  WriteByte(17);
  WriteByte(0);
  WriteByte(0);
  Stream.Position := 0;
  Result := Stream.Size;
end;

function TsgDWG2004Compressor.Decompress(Source, Dest: PByte; SourceSize,
  DestSize: Cardinal): Integer;
var
  vOffset, vOpCode1, vLength, vOpCode2: Integer;
  vDest: PByte;

  procedure CheckError;
  begin
    DoCompressError(GetSrcCurByte <> 0, sDecompressDWGError);
    Inc(FSrcCur);
  end;

  procedure SetResult;
  var
    vDelta: TsgNativeUInt;
  begin
    vDelta := (TsgNativeUInt(DstEnd) - TsgNativeUInt(DstCur));
    if vDest <> Dest then
      Result := {$IFDEF SGDEL_7}UInt64{$ENDIF}(Stream.Size) - vDelta
    else
      Result := WriteBytes(Dest^, DestSize - vDelta);
    if Stream.Size <> Result then
      Stream.Size := Result;
    Stream.Position := 0;
  end;

begin
  Result := 0;
  DoCompressError((Source = nil) or (SourceSize = 0) or (DestSize = 0),
    sCorruptedDWGFileHeader);

  vDest := Dest;
  Init(cdsDecompress, Source, vDest, SourceSize, DestSize);
  if vDest = nil then
  begin
    Stream.Size := DestSize;
    vDest := PByte(Stream.Memory);
    SetDest(vDest, DestSize);
  end;

  vOpCode1 := 0;
  vOpCode2 := 0;
  try
    while SrcCur < SrcEnd do
    begin
      vOffset := 0;
      vLength := 0;
      GetLiteral(vOpCode1);
      vOpCode1 := GetSrcCurByte;
      Inc(FSrcCur);
      while True do
      begin
        if vOpCode1 <> $11 then Break;
        CheckError;
        CheckError;
        Exit;
      end;
      DoCompressError(DstCur > DstEnd, sCorruptedDWGFileHeader);
      if (vOpCode1 < $10) or (vOpCode1 >= $40) then
      begin
        vOpCode2 := GetSrcCurByte;
        Inc(FSrcCur);
        vOffset := (SAR(vOpCode1, 2) and 3) or (vOpCode2 shl 2);
        vLength := SAR(vOpCode1, 4);
        Dec(vLength);
        Inc(vOffset);
      end
      else
      begin
        if vOpCode1 < $20 then
        begin
          GetLength(vLength, vOpCode1, 7);
          vOffset := (vOpCode1 and 8) shl $B;
          GetOffset(vOpCode1, vOpCode2, vOffset);
          Inc(vOffset, $4000);
        end
        else
        begin
          GetLength(vLength, vOpCode1, $1F);
          GetOffset(vOpCode1, vOpCode2, vOffset);
          Inc(vOffset);
        end;
      end;
      GetCompressed(vLength, vOffset);
    end;
  finally
    SetResult;
  end;
end;

function TsgDWG2004Compressor.FindMatch(var MatchLength,
  MatchOffset: TsgNativeUInt): Boolean;
var
  vHashKey: Integer;
  vPHashTableItem, vPSrcMatch, P: PByte;

  procedure SetResult(Index: Integer);
  begin
    FHashTable[Index] := SrcCur;
    Result := MatchLength >= 3;
  end;

  function SetPSrcMatch(Index: Integer): PByte;
  begin
    Result := PByte(FHashTable[Index]);
  end;

begin
  Result := False;
  MatchLength := 0;
  MatchOffset := 0;
  vHashKey := GenHashKey(GetSrcCurByte, GetSrcCurByte(1), GetSrcCurByte(2),
    GetSrcCurByte(3));
  vPSrcMatch := SetPSrcMatch(vHashKey);
  MatchOffset := TsgNativeUInt(SrcCur) - TsgNativeUInt(vPSrcMatch);
  if (TsgTypeComparer.CmpPointer(vPSrcMatch, SrcStart) >= 0) and (MatchOffset <= $BFFF) then
  begin
    if MatchOffset > $400 then
    begin
      if GetSrcCurByte <> Byte(PAnsiChar(vPSrcMatch)[3])
      then
        vHashKey := vHashKey and $7FF xor $401F;
        vPHashTableItem := GetPHashTableItem(vHashKey);
        vPSrcMatch := SetPSrcMatch(vHashKey);
        MatchOffset := TsgNativeUInt(SrcCur) - TsgNativeUInt(vPHashTableItem);
        if (TsgNativeUInt(vPHashTableItem) < TsgNativeUInt(SrcStart)) or
          (MatchOffset > $BFFF) or
          ((MatchOffset > $400) and (GetSrcCurByte(3) <> Byte(PAnsiChar(vPSrcMatch)[3]))) then
        begin
          SetResult(vHashKey);
          Exit;
        end;
      end;
    if CompareMem(SrcCur, vPSrcMatch, 4) then
    begin
      MatchLength := 3;
      Inc(vPSrcMatch, MatchLength);
      P := PByte(TsgNativeUInt(TsgNativeUInt(SrcCur) + 3));
      while TsgTypeComparer.CmpPointer(P, SrcEnd) < 0 do
      begin
        if P^ <> vPSrcMatch^ then
          Break;
        Inc(P);
        Inc(vPSrcMatch);
        Inc(MatchLength);
      end;
    end;
  end;
  SetResult(vHashKey);
end;

function TsgDWG2004Compressor.GenHashKey(Value1, Value2, Value3,
  Value4: Byte): Integer;
var
  vVal: Integer;
begin
  vVal := Value1 xor (Integer(Value2 xor (Integer(Value3 xor Integer(Value4
    shl 6)) shl 5)) shl 5);
  Result := (SAR(vVal, 5) + vVal) and $7FFF;
end;

procedure TsgDWG2004Compressor.GetCompressed(ALength, AOffset: Integer);
var
  vDstPrev: PAnsiChar;
begin
  vDstPrev := DstCur;
  Dec(vDstPrev, AOffset);
  if ALength < AOffset then
  begin
    Move(vDstPrev^, DstCur^, ALength);
    Inc(FDstCur, ALength);
  end
  else
  begin
    while True do
    begin
      Dec(ALength);
      if ALength < 0 then Exit;
      DstCur^ := vDstPrev^;
      Inc(FDstCur);
      Inc(vDstPrev);
    end;
  end;
end;

procedure TsgDWG2004Compressor.GetLength(var ALength: Integer; AOpCode,
  AMask: Integer);
begin
  ALength := AOpCode and AMask;
  if ALength = 0 then
  begin
    ALength := AMask;
    GetLongLength(ALength);
  end;
  Inc(ALength, 2);
end;

procedure TsgDWG2004Compressor.GetLiteral(AOpCode: Integer);
var
  vLitLength: Integer;
  vByte: Byte;
begin
  vLitLength := AOpCode and 3;
  if vLitLength = 0 then
  begin
    if Byte(SrcCur^) and $0F0 = 0 then
    begin
      vByte := GetSrcCurByte;
      Inc(FSrcCur);
      GetLength(vLitLength, vByte, $F);
      Inc(vLitLength);
    end;
  end;
  if vLitLength = 0 then Exit;
  Move(SrcCur^, DstCur^, vLitLength);
  Inc(FSrcCur, vLitLength);
  Inc(FDstCur, vLitLength);
end;

procedure TsgDWG2004Compressor.GetLongLength(var ALength: Integer);
var
  vByte: Byte;
begin
  vByte := GetSrcCurByte;
  Inc(FSrcCur);
  while vByte = 0 do
  begin
    Inc(ALength, $FF);
    vByte := GetSrcCurByte;
    Inc(FSrcCur);
  end;
  Inc(ALength, vByte);
end;

procedure TsgDWG2004Compressor.GetOffset(var AOpCode1, AOpCode2,
  AOffset: Integer);
begin
  AOpCode1 := GetSrcCurByte;;
  AOpCode2 := GetSrcCurByte(1);
  Inc(FSrcCur, 2);
  AOffset := SAR(AOpCode1, 2) or AOffset;
  AOffset := (AOpCode2 shl 6) or AOffset;
end;

procedure TsgDWG2004Compressor.Init(AStatus: TsgDWGCompressorStatus; ASource, ADest: PByte;
  ASourceSize, ADestSize: Cardinal);
begin
  inherited Init(AStatus, ASource, ADest, ASourceSize, ADestSize);
  if Status = cdsCompress then
    Inc(FSrcCur, 4);
end;

function TsgDWG2004Compressor.WriteLength(OpCode: Byte; Length,
  Threshold: Integer): Integer;
var
  vVal: Byte;
begin
  DoCompressError((Length = 0) or (ThresHold = 0), sLZ77CompressionError);
  if (Length > Threshold) then
  begin
    Result := WriteByte(OpCode);
    Inc(Result, WriteLongLength(Length - Threshold));
  end
  else
  begin
    vVal := Byte(Length - 2) or OpCode;
    Result := WriteByte(vVal);
  end;
end;

function TsgDWG2004Compressor.WriteLiteral(LitLength: Cardinal): Integer;
begin
  Result := 0;
  if LitLength > 0 then
  begin
    if LitLength > 3 then
      Inc(Result, WriteLength(0, LitLength - 1, 17));
    Inc(Result, WriteBytes(SrcPrev^, LitLength));
  end;
end;

function TsgDWG2004Compressor.WriteLongLength(Value: Integer): Integer;
begin
  DoCompressError(Value < 0, sLZ77CompressionError);
  Result := 0;
  while Value > 255 do
  begin
    Dec(Value, 255);
    Inc(Result, WriteByte(0));
  end;
  Inc(Result, WriteByte(Value));
end;

function TsgDWG2004Compressor.WriteMatch(Offset, Length, LitLength: Integer): Integer;
var
	vOpCode1, vOpCode2: Byte;
begin
  Result := 0;
  if (Offset > 1024) or (Length >= 15) then
  begin
    if Offset > 16384 then
    begin
      Dec(Offset, 16384);
      Result := WriteLength((SAR(Offset, $B) and 8) or $10, Length, 9);
    end
    else
    begin
      Dec(Offset);
      Result := WriteLength(32, Length, 33);
    end;
    vOpCode1 := (Offset and $FF) shl 2;
    vOpCode2 := SAR(Offset, 6);
  end
  else
  begin
    Dec(Offset);
    vOpCode1 := ((Length + 1) shl 4) or (Offset and 3) shl 2;
    vOpCode2 := SAR(Offset, 2);
  end;
  if LitLength  < 4 then
    vOpCode1 := LitLength or vOpCode1;
  Inc(Result, WriteByte(vOpCode1));
  Inc(Result, WriteByte(vOpCode2));
end;

end.



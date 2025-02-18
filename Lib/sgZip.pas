{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{        data compression and uncompression functions        }
{                                                            }
{          unit based on gzIO.pas from PASZLIB 1.0           }
{                 by Jacques Nomssi Nzali                    }
{        http://www.nomssi.de/paszlib/paszlib.html           }
{  Copyright (c) 1998,1999,2000 by NOMSSI NZALI Jacques H. C.}
{    Copyright (c) 1995-98 Jean-loup Gailly & Mark Adler     }
{     Copyright (c) 2010-2020 SoftGold software company      }
{************************************************************}

unit sgZip;
{$I SGDXF.inc}
//{$DEFINE SG_SGZIP_DEBUG}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
   Windows,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  Types, sgFMXTypes,
{$ELSE}
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, Types,
{$ENDIF}
  SysUtils, Classes, sgConsts, sgFunction, Math, zlibpas_sg
  {$IFDEF SGDEL_5} , zlib {$ENDIF};

const
  cnstGZPacked = $8B1F;

type
  TsgDWFFileExt = (feW2D, feW3D, fePNG, feXML, feTIFF, feUndefined);
  TsgXMLFileTag = (xmlManifest, xmlDescriptor, xmlUndefined);
  TsgDWFResTypeTag = (resThumbnail, res2dsg, resUndefined);

  PsgEPlotSection = ^TsgEPlotSection;
  TsgEPlotSection= record
    IndInZip: Integer;
    FileLength: Integer;
    Title: string;
    LayoutName: string;
    Company: string;
    Author: string;
    CreationTime: string;
    ModificationTime: string;
    Width: string;
    Height: string;
    Units: string;
    Order: string;
    Graphic: string;
    FileExt: TsgDWFFileExt;
    LayoutIndex: Integer;
  end;

  TsgGzHeader = packed record
    ID1: Byte;
    ID2: Byte;
    CompMethod: Byte;
    Flags: Byte;
    ModTime: LongInt;
    ExFlags: Byte;
    OS: Byte;
  end;

  TsgGzTail = packed record
    CRC32: uLong;
    Size: uLong;
  end;

  EsgUnzipFile = class(Exception);

  TsgUnzipFile = class
  private
    FStream: TMemoryStream;
    FContents: TList;
    FCurrentSize: Integer;
    function GetFileAttr(const AIndex: Integer): Cardinal;
  protected
    function GetContents: Boolean;
    function SearchCentralDir: Integer;
    function GetContent: TList;
    procedure Reset; virtual;
    procedure PreProcessWork(const AIndex: Integer;
      const APFileInfo: Pointer; const APFileHdr: Pointer); virtual;
    procedure PostProcessWork(const ResultUnZip: Boolean); virtual;
    function GetFileName(const AIndex: Integer): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ExtractFile(FileIndex: Integer; AStream: TStream): Boolean;
    function GetFileIndexFromFileName(const AFileName: string): Integer;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    property Contents: TList read GetContent;
    property FileNameContents[const AIndex: Integer]: string read GetFileName;
    property FileAttr[const AIndex: Integer]: Cardinal read GetFileAttr;
    property CurrentSize: Integer read FCurrentSize;
  end;


  TsgDWFUnzip = class(TsgUnzipFile)
  private
    FEPlotSections: TList;
    FLayouts: TStrings;
    FTotalSize: Integer;
    function GetFileExt(const AFileName: string): TsgDWFFileExt;
    function GetFileExtOnMime(const AMime: string): TsgDWFFileExt;
    function GetEPlotSections: TList;
    procedure AddPlotSections(const AMime, AHref: string;
      const ALayoutIndex: Integer);
  protected
    function GetXMLType(FileName: string): TsgXMLFileTag;
    procedure PreProcessWork(const AIndex: Integer;
      const APFileInfo: Pointer; const APFileHdr: Pointer); override;
    procedure PostProcessWork(const ResultUnZip: Boolean); override;
    procedure Reset; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property EPlotSections: TList read GetEPlotSections;
    property Layouts: TStrings read FLayouts;
    property TotalSize: Integer read FTotalSize;
  end;

function Compress(AStream: TStream):Boolean;
function IsGZPacked(const AStream: TStream): Boolean;
function PackStream(AStream: TStream): Boolean;
function UnPackGZStream(AGZStream, AOutStream: TStream;
  var FileName: string): Boolean;
function UnPackSTBStream(ASTBStream: TStream; ASTBFile: TStringList): Boolean;
function UnPackStream(AStream: TStream): Boolean;
function ExtractFirstFileSatisfiesCondition(ASourse: TStream; ADest: TStream;
  const AStrings: array of string): Boolean;

procedure sgZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

implementation

uses
  sgXMLParser;

type
  PsgGZStream = ^TsgGZStream;
  TsgGZStream = record
    ZStreamRec: zlibpas_sg.z_stream;
    ZErr: Integer;
    ZEof: Boolean;
    GZFile: TStream;
    InBuf: pBytef;
    OutBuf: pBytef;
    Crc: uLong;
    Transparent: Boolean;
    Mode: Char;
    StartPos: Longint;
  end;

  PsgZIPCntrDirHdr = ^TsgZIPCntrDirHdr;
  TsgZIPCntrDirHdr = packed record
    Signature: Cardinal; // $06054B50
    ThisDiskNum: Word;
    CntrlDiskDiskNum: Word;
    CntrlDirEntries: Word;
    TotalEntries: Word;
    CntrlDirSize: Cardinal;
    CntrlDirOffSet: Cardinal;
    FileCommentLen: Word;
  end;

  PsgZIPLocalFileHdr = ^TsgZIPLocalFileHdr;
  TsgZIPLocalFileHdr = packed record
    Signature: Cardinal;
    VerMadeBy0: Byte;
    VerMadeBy1: Byte;
    VerNeeded: Word;
    Flag: Word;
    ComprMethod: Word;
    ModifTime: Word;
    ModifDate: Word;
    CRC32: Cardinal;
    ComprSize: Cardinal;
    UncomprSize: Cardinal;
    FileNameLen: Word;             
    ExtraLen: Word;
    FileCommmentLen : Word;
    DiskStart: Word;
    IntFileAttr: Word;
    ExtFileAttr: Cardinal;
    RelOffLocal: Cardinal;
  end;

  PsgZIPLocalFileInfo = ^TsgZIPLocalFileInfo;
  TsgZIPLocalFileInfo= record
    FileName: string;
    Password: string;
    ComprMethod: Word;
    OffsetInZip: Integer;
    CRC32: Cardinal;
    ComprSize: Integer;
    UncomprSize: Integer;
    ExtFileAttr: Cardinal;
    case Integer of
      0: (ContentEx: array[0..10] of Integer);
      1: ( FileExt: TsgDWFFileExt;
           ResType: TsgDWFResTypeTag;)
  end;

const
  aGZMagic: array[0..1] of Byte = ($1F, $8B);

  Z_EOF = -1;
  Z_BUFSIZE = 16384;

  ASCII_FLAG  = $01;
  HEAD_CRC    = $02;
  EXTRA_FIELD = $04;
  ORIG_NAME   = $08;
  COMMENT     = $10;
  RESERVED    = $E0;

  CNTR_DIR_END_TAG = $06054B50; // 'PK'56
  DWF_PACK_FORMAT_TAG = '(DWF_V)';

procedure memcpy(Dest, Source: Pointer; Len: Integer);
begin
  Move(Source, Dest, Len);
end;

procedure GZDone(var P: PsgGZStream; DoInflateEnd: Boolean);
begin
  if P = nil then Exit;
  if DoInflateEnd then
    zlibpas_sg.inflateEnd(P^.ZStreamRec);
  P^.GZFile.Free;
  if Assigned (P^.InBuf) then FreeMem(P^.InBuf);
  if Assigned (P^.OutBuf) then FreeMem(P^.OutBuf);
  FreeMem(P);
  P := zlibpas_sg.Z_NULL;
end;

function GZInit(AGZStream: TStream; APos: Integer): PsgGZStream;
var
  vErr, vSize, vRes: Integer;
  P: Pointer;

begin
  Result := zlibpas_sg.Z_NULL;
  vSize := AGZStream.Size;
  if vSize <= 0 then Exit;
  New(Result);
  if not Assigned(Result) then
  begin
    Result := zlibpas_sg.Z_NULL;
    Exit;
  end;

  Result^.ZStreamRec.zalloc := nil;
  Result^.ZStreamRec.zfree := nil;
  Result^.ZStreamRec.state := nil;
  Result^.ZStreamRec.next_in := zlibpas_sg.Z_NULL;
  Result^.ZStreamRec.next_out := zlibpas_sg.Z_NULL;
  Result^.ZStreamRec.avail_in := 0;
  Result^.ZStreamRec.avail_out := 0;
  Result^.ZErr := Z_OK;
  Result^.ZEof := False;
  Result^.InBuf := zlibpas_sg.Z_NULL;
  Result^.OutBuf := zlibpas_sg.Z_NULL;
  Result^.Crc := zlibpas_sg.crc32(0, zlibpas_sg.Z_NULL, 0);
  Result^.Transparent := False;
  Result^.GZFile := TMemoryStream.Create;

  GetMem(P, vSize);
  if P = nil then
  begin
    GZDone(Result, False);
    Exit;
  end;
  try
    AGZStream.Position := 0;
    AGZStream.Read(P^, vSize);
    vRes := Result^.GZFile.Write(P^, vSize);
    if vRes <> vSize then
    begin
      GZDone(Result, False);
      Exit;
    end;
  finally
    FreeMem(P);
  end;

  Result^.GZFile.Position := 0;

  Result^.Mode := Chr(0);

  GetMem(Result^.InBuf, Z_BUFSIZE);
  Result^.ZStreamRec.next_in := zlibpas_sg.pBytef(Result^.InBuf);

  vErr := zlibpas_sg.inflateInit2_(zlibpas_sg.z_stream(Result^.ZStreamRec), -MAX_WBITS, AnsiString(zlibpas_sg.ZLIB_VER), SizeOf(zlibpas_sg.z_stream));

  if (vErr <> Z_OK) or (Result^.InBuf = zlibpas_sg.Z_NULL) then
  begin
    GZDone(Result, True);
    Exit;
  end;

  Result^.ZStreamRec.avail_out := Z_BUFSIZE;

  if Result^.ZStreamRec.avail_in = 0 then
  begin
    Result^.ZStreamRec.avail_in := Result^.GZFile.Read(Result^.InBuf^, Z_BUFSIZE);
    if Result^.ZStreamRec.avail_in = 0 then
    begin
      GZDone(Result, True);
      Exit;
    end;
  end;

  Dec(Result^.ZStreamRec.avail_in, APos);
  Inc(Result^.ZStreamRec.next_in, APos);

  Result^.StartPos := Result^.GZFile.Position - Result^.ZStreamRec.avail_in;
end;


function GZStreamRead(P, Buf: Pointer; BufLen: Cardinal;
  DoCheckCRC: Boolean): Integer;
var
  PStZ: PsgGZStream;
  PStart, PNextOut: pBytef;
  N, vCrcLen: uInt;
  vGzTail: TsgGzTail;
  vBytes: Integer;

begin

  PStZ := PsgGZStream(P);
  PStart := pBytef(Buf);

  if PStZ = nil then
  begin
    GZStreamRead := Z_STREAM_ERROR;
    Exit;
  end;

  case PStZ^.ZErr of
    Z_DATA_ERROR, Z_ERRNO:
      begin
        Result := -1;
        Exit;
      end;
    Z_STREAM_END:
      begin
        Result := 0;
        Exit;
      end;
  end;

  PStZ^.ZStreamRec.next_out := zlibpas_sg.pBytef(buf);
  PStZ^.ZStreamRec.avail_out := Buflen;

  while PStZ^.ZStreamRec.avail_out <> 0 do
  begin
    if PStZ^.Transparent then
    begin
      N := PStZ^.ZStreamRec.avail_in;
      if N > PStZ^.ZStreamRec.avail_out then N := PStZ^.ZStreamRec.avail_out;
      if N > 0 then
      begin
        memcpy(PStZ^.ZStreamRec.next_out, PStZ^.ZStreamRec.next_in, N);
        Inc(PStZ^.ZStreamRec.next_out, N);
        Inc(PStZ^.ZStreamRec.next_in, N);
        Dec(PStZ^.ZStreamRec.avail_out, N);
        Dec(PStZ^.ZStreamRec.avail_in, N);
      end;
      if PStZ^.ZStreamRec.avail_out > 0 then
      begin
        vBytes := PStZ^.GZFile.Read(PStZ^.ZStreamRec.next_out^, PStZ^.ZStreamRec.avail_out);
        Dec(PStZ^.ZStreamRec.avail_out, uInt(vBytes));
      end;
      Dec(BufLen, PStZ^.ZStreamRec.avail_out);
      Inc(PStZ^.ZStreamRec.total_in, uLong(BufLen));
      Inc(PStZ^.ZStreamRec.total_out, uLong(BufLen));
      Result := Integer(BufLen);
      Exit;
    end;

    if (PStZ^.ZStreamRec.avail_in = 0) and (not PStZ^.ZEof) then
    begin
      PStZ^.ZStreamRec.avail_in := PStZ^.GZFile.Read(PStZ^.inbuf^, Z_BUFSIZE);
      if PStZ^.ZStreamRec.avail_in = 0 then PStZ^.ZEof := True;
      PStZ^.ZStreamRec.next_in := zlibpas_sg.pBytef(PStZ^.InBuf);
    end;

    PStZ^.ZErr := zlibpas_sg.inflate(PStZ^.ZStreamRec, Z_NO_FLUSH);

    if (PStZ^.zerr = Z_STREAM_END) and DoCheckCRC then
    begin
      vCrcLen := 0;
      PNextOut := {$IFDEF SGFPC}zlib.pBytef({$ENDIF}PStZ^.ZStreamRec.next_out{$IFDEF SGFPC}){$ENDIF};
      while (PNextOut <> PStart) do
      begin
        Dec(PNextOut);
        Inc(vCrcLen);
      end;

      PStZ^.Crc := zlibpas_sg.crc32(PStZ^.crc, zlibpas_sg.pBytef(PStart), vCrcLen);
      PStart := {$IFDEF SGFPC}zlib.pBytef({$ENDIF}PStZ^.ZStreamRec.next_out{$IFDEF SGFPC}){$ENDIF};

      PStZ^.GZFile.Position := PStZ^.GZFile.Position - PStZ^.ZStreamRec.avail_in;

      if PStZ^.GZFile.Read(vGzTail, SizeOf(TsgGzTail)) <> SizeOf(TsgGzTail) then PStZ^.ZErr := Z_DATA_ERROR;

      if (PStZ^.Crc <> vGzTail.CRC32) or (PStZ^.ZStreamRec.total_out <> vGzTail.Size) then
        PStZ^.ZErr := Z_DATA_ERROR;
    end;
    if (PStZ^.ZErr <> Z_OK) or PStZ^.ZEof then Break;
  end;

  vCrcLen := 0;
  PNextOut := {$IFDEF SGFPC}zlib.pBytef({$ENDIF}PStZ^.ZStreamRec.next_out{$IFDEF SGFPC}){$ENDIF};
  while PNextOut <> PStart do
  begin
    Dec(PNextOut);
    Inc(vCrcLen);
  end;
  PStZ^.Crc := zlibpas_sg.crc32(PStZ^.Crc, zlibpas_sg.pBytef(PNextOut), vCrcLen);
  Result := Integer(BufLen - PStZ^.ZStreamRec.avail_out);
end;

function UnPackGZStream(AGZStream, AOutStream: TStream;
  var FileName: string): Boolean;
var
  B: Byte;
  P: Pointer;
  vLen, vCRC16: Word;
  vGzHeader: TsgGzHeader;
  S: string;
  PGZStream: PsgGZStream;
  vRes, vStrmPos: Integer;

begin
  Result := False;
  FillChar(vGzHeader, SizeOf(TsgGzHeader), 0);

  AGZStream.Position := 0;

  vRes := AGZStream.Read(vGzHeader, SizeOf(TsgGzHeader));
  if vRes <> SizeOf(TsgGzHeader) then Exit;

  if (vGzHeader.ID1 <> aGZMagic[0]) or (vGzHeader.ID2 <> aGZMagic[1]) then Exit;

  if (vGzHeader.CompMethod <> Z_DEFLATED) or ((vGzHeader.Flags and RESERVED) <> 0) then
    Exit;

  if (vGzHeader.Flags and EXTRA_FIELD) <> 0 then
  begin
    vRes := AGZStream.Read(vLen, SizeOf(Word));
    if vRes <> SizeOf(Word) then Exit;
    SetLength(S, vLen);
    vRes := AGZStream.Read(PChar(S)^, vLen);
    if vRes <> vLen then Exit;
  end;

  if (vGzHeader.Flags and ORIG_NAME) <> 0 then
  begin
    FileName := '';
    repeat
      vRes := AGZStream.Read(B, 1);
      if vRes = 0 then Exit;
      if B <> 0 then FileName := FileName + Char(B);
    until (B = 0) or (AGZStream.Position = AGZStream.Size);
  end;


  if (vGzHeader.Flags and COMMENT) <> 0 then
    repeat
      vRes := AGZStream.Read(B, 1);
      if vRes = 0 then Exit;
    until (B = 0) or (AGZStream.Position = AGZStream.Size);

  if (vGzHeader.Flags and HEAD_CRC) <> 0 then
  begin
    vRes := AGZStream.Read(vCRC16, SizeOf(Word));
    if vRes <> SizeOf(Word) then Exit;
  end;

  vStrmPos := AGZStream.Position;
  PGZStream := GZInit(AGZStream, vStrmPos);
  P := nil;
  if PGZStream = nil then Exit;
  try
    AOutStream.Size := 0;
    GetMem(P, Z_BUFSIZE);
    repeat
      vRes := GZStreamRead(PGZStream, P, Z_BUFSIZE, True);
      if vRes > 0 then AOutStream.Write(P^, vRes);
    until vRes <= 0;
  finally
    FreeMem(P);
    GZDone(PGZStream, True);
  end;
  if (vRes = 0) and (AOutStream.Size > 0) then Result := True;
end;

function StrPosLen(const Str1, Str2: PChar; Str1Len, Str2Len: Cardinal;
  var Pos: Cardinal): PChar;
var
  I, J: Cardinal;
  Find: Boolean;

begin
  Result := nil;
  Pos := Cardinal($FFFFFFFF);
  if Str1Len < Str2Len then
    Exit;
  I := 0;
  Find := False;
  while I < Str1Len do
  begin
    if Str1[I] = Str2[0] then
    begin
      Find := True;
      for J := 1 to Str2Len - 1 do
      begin
        if I + J > Str1Len then
          Exit;
        if Str1[I + J] <> Str2[J] then
        begin
          Find := False;
          Break;
        end
      end;
      if Find then
        Break;
    end;
    Inc(I);
  end;
  if Find then
  begin
    Result := Pointer(TsgNativeUInt(Str1) + I);
    Pos := I;
  end;
end;

function Compress(AStream: TStream):Boolean;
var
  P: pBytef;
  vZStream: zlibpas_sg.z_stream;
  vIn, vOut: Integer;
  S: AnsiString;

  function DoCheck(ACode: Integer): Integer;
  begin
    Result := ACode;
    if Result < 0 then
      raise Exception.Create( 'Compression error' );
  end;

begin
  Result := True;
  vIn := AStream.Size;
  GetMem(P, vIn);
  try
    AStream.Position := 0;
    AStream.Read(P^, vIn);
    vOut := ((vIn + (vIn div 10) + 12) + 255) and not 255;
    SetLength(S, vOut);
    FillChar(vZStream, SizeOf(vZStream), 0);
    vZStream.next_in := zlibpas_sg.pBytef(P);
    vZStream.avail_in := vIn;
    vZStream.next_out := @S[1];
    vZStream.avail_out := vOut;
    DoCheck(zlibpas_sg.deflateInit(vZStream, 9));
    try
      while DoCheck(zlibpas_sg.deflate(vZStream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(vOut, 256);
        SetLength(S, vOut);
        vZStream.next_out := @S[vZStream.total_out + 1];
        vZStream.avail_out := 256;
      end;
    finally
      DoCheck(zlibpas_sg.deflateEnd(vZStream));
    end;
    SetLength(S, vZStream.total_out);
  finally
    FreeMem(P);
  end;
  AStream.Size := 0;
  AStream.Write(PAnsiChar(S)^, Length(S));
end;

function sgZDecompressCheck(Code: Integer): Integer;
begin
  Result := Code;
  if Code < 0 then
    raise Exception.Create(string(zError(Code)));
end;

function sgZCompressCheck(code: Integer): Integer;
begin
  Result := Code;
  if Code < 0 then
    raise Exception.Create(string(zError(Code)));
end;


procedure sgZCompress(const inBuffer: Pointer; inSize: Integer; out outBuffer: Pointer;
  out outSize: Integer; level: Integer = Z_DEFAULT_COMPRESSION);
const
  delta = 256;
var
  zstream: zlibpas_sg.z_stream;
begin
  FillChar(zstream, SizeOf(zstream), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    sgZCompressCheck(zlibpas_sg.DeflateInit(zstream, level));

    try
      while sgZCompressCheck(zlibpas_sg.deflate(zstream, zlibpas_sg.Z_FINISH)) <> zlibpas_sg.Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := zlibpas_sg.pBytef(TsgNativeUInt(outBuffer) + Cardinal(zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      sgZCompressCheck(zlibpas_sg.deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure sgZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);
var
  zstream: zlibpas_sg.z_stream;
  delta: Integer;
begin
  FillChar(zstream, SizeOf(zstream), 0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    sgZDecompressCheck(zlibpas_sg.InflateInit(zstream));

    try
      while sgZDecompressCheck(zlibpas_sg.inflate(zstream, zlibpas_sg.Z_NO_FLUSH)) <> zlibpas_sg.Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := zlibpas_sg.pBytef(TsgNativeUInt(outBuffer) + Cardinal(zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      sgZDecompressCheck(zlibpas_sg.inflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

function CompressUncompressStream(AStream: TStream; Kind: Integer): Boolean;
{$IFDEF SGDEL_5}
var
  P1, P2: Pointer;
  vSize, vOutSize: Integer;
{$ENDIF}
begin
  Result := False;
{$IFDEF SGDEL_5}
  AStream.Position := 0;
  vSize := AStream.Size;
  GetMem(P1, AStream.Size);
  P2 := nil;
  try
    AStream.Read(P1^, vSize);
    try
      if Kind = 0 then
      {$IFDEF SGFPC}
        sgZDecompress(P1, vSize, P2, vOutSize)
      {$ELSE}
      {$IFNDEF SGDEL_2009}
        DecompressBuf(P1, vSize, 0, P2, vOutSize)
      {$ELSE}
        sgZDecompress(P1, vSize, P2, vOutSize)
      {$ENDIF}
      {$ENDIF}
      else
      {$IFDEF SGFPC}
        sgZCompress(P1, vSize, P2, vOutSize);
      {$ELSE}
      {$IFNDEF SGDEL_2009}
        CompressBuf(P1, vSize, P2, vOutSize);
      {$ELSE}
        sgZCompress(P1, vSize, P2, vOutSize);
      {$ENDIF}
      {$ENDIF}
    except
      FreeMem(P1);
      P1 := nil;
      P2 := nil;
      Exit;
    end;
    AStream.Size := 0;
    if vOutSize = 0 then Exit;
    AStream.Write(P2^, vOutSize);
    AStream.Position := 0;
  finally
    if P1 <> nil then FreeMem(P1);
    if P2 <> nil then FreeMem(P2);
  end;
  Result := True;
{$ENDIF}
end;

function IsGZPacked(const AStream: TStream): Boolean;
var
  vType: WORD;
begin
  AStream.ReadBuffer(vType, SizeOf(vType));
  AStream.Position := 0; //AStream.Seek(0, soFromBeginning);
  Result := vType = cnstGZPacked;
end;

function UnPackStream(AStream: TStream): Boolean;
begin
  Result := CompressUncompressStream(AStream, 0);
end;

function ExtractFirstFileSatisfiesCondition(ASourse: TStream; ADest: TStream;
  const AStrings: array of string): Boolean;
var
  vIsZip: Boolean;

  vUnzip: TsgUnzipFile;
  I: Integer;
  vStream: TMemoryStream;
  vExt: string;

begin
  Result := False;
  if not Assigned(ASourse) or not Assigned(ADest) then
    Exit;
  vUnzip := TsgUnzipFile.Create;
  try
    vIsZip := True;
    try
     ASourse.Position := 0;
     vUnzip.LoadFromStream(ASourse);
    except
      vIsZip := False;
    end;
    if vIsZip and (vUnzip.Contents.Count <> 0) then
    begin
      for I := 0 to vUnzip.Contents.Count - 1 do
      begin
        vStream := TMemoryStream.Create;
        try
          if (vUnzip.FileAttr[I] and $10) <> 0 then
            Continue;
          vExt := ExtractFileExt(vUnzip.FileNameContents[I]);
          Delete(vExt, 1, 1);
          if (Length(AStrings) = 0) or (IndexOfStrings(vExt, AStrings, @SysUtils.AnsiCompareText) >= 0 ) then
          begin
            if vUnzip.ExtractFile(I, vStream) then
            begin
              ADest.Size := 0;
              ADest.CopyFrom(vStream, vStream.Size);
              ADest.Position := 0;
              Result := True;
            end;
            Break;
          end;
        finally
          vStream.Free;
        end;
      end;
    end;
  finally
    vUnzip.Free;
  end;
end;

function PackStream(AStream: TStream): Boolean;
begin
  Result := CompressUncompressStream(AStream, 1);
end;

function UnPackSTBStream(ASTBStream: TStream; ASTBFile: TStringList): Boolean;
const
  SCodec = 'zlibcodec';
var
  P: PChar;
  I, vSize, vLen, vRes: Cardinal;
  vStream: TMemoryStream;
begin
  Result := False;
  P := nil;
  vStream := nil;
  try
    ASTBStream.Position := 0;
    vSize := ASTBStream.Size;
    if vSize <= 60 then Exit;
    GetMem(P, vSize);
    try
      ASTBStream.Read(P^, vSize);
      vLen := StrLen(PChar(SCodec));
      if StrPosLen(P, PChar(SCodec), vSize, vLen, vRes) = nil then Exit;
(*      Inc(vRes, vLen + 2);
      if vRes >= vSize then Exit;
      I := vRes;
      vFlag := False;
      while I <= vSize do
      begin
        if P[I] = #0 then
          if vFlag then
            Break
          else
            vFlag := True;
        Inc(I);
      end;
      if I >= vSize - 2 then Exit;*)
      I := 60;
      ASTBStream.Position := I;
      vStream := TMemoryStream.Create;
      ASTBStream.Read(P^, vSize - I);
      vStream.Write(P^, vSize - I);
      if not UnPackStream(vStream) then Exit;
      vStream.Position := 0;
      ASTBFile.LoadFromStream(vStream);
    finally
      if P <> nil then
      begin
        FreeMem(P);
        P := nil;
      end;
      vStream.Free;
      vStream := nil;
    end;
  except
    vStream.Free;
    if P <> nil then FreeMem(P);
    Exit;
  end;
  Result := True;
end;

{TsgUnzipFile}

constructor TsgUnzipFile.Create;
begin
  FStream := TMemoryStream.Create;
  FContents := TList.Create;
end;

destructor TsgUnzipFile.Destroy;
begin
  Reset;
  FStream.Free;
  FContents.Free;
  inherited Destroy;
end;

function TsgUnzipFile.ExtractFile(FileIndex: Integer; AStream: TStream): Boolean;
var
  vFileInfo: TsgZIPLocalFileInfo;
  PGZStream: PsgGZStream;
  vRes: Integer;
  P: Pointer;
  vStream: TMemoryStream;
begin
  P := nil;
  Result := False;
  if not (InRange(FileIndex, 0, FContents.Count - 1)) then
    Exit;

  vFileInfo := PsgZIPLocalFileInfo(FContents[FileIndex])^;
  if vFileInfo.ComprMethod = 0 then Exit;

  FStream.Position := vFileInfo.OffsetInZip;
  vStream := TMemoryStream.Create;
  FCurrentSize := vFileInfo.ComprSize;
  try
    GetMem(P, vFileInfo.ComprSize);
    vRes := FStream.Read(P^, vFileInfo.ComprSize);
    if vRes <> vFileInfo.ComprSize then Exit;
    vRes := vStream.Write(P^, vFileInfo.ComprSize);
    if vRes <> vFileInfo.ComprSize then Exit;
    PGZStream := GZInit(vStream, 0);
  finally
    vStream.Free;
    if Assigned(P) then
      FreeMem(P);
  end;
  P := nil;
  FStream.Position := 0;
  if PGZStream = nil then Exit;
  try
    AStream.Size := 0;
    GetMem(P, Z_BUFSIZE);
    repeat
      vRes := GZStreamRead(PGZStream, P, Z_BUFSIZE, False);
      if vRes > 0 then AStream.Write(P^, vRes);
    until vRes <= 0;
    if (vRes = 0) and (AStream.Size > 0) then
    begin
      FreeMem(P);
      GetMem(P, AStream.Size);
      AStream.Position := 0;
      if AStream.Read(P^, AStream.Size) = AStream.Size then
      begin
        if zlibpas_sg.uLong(vFileInfo.CRC32) <> zlibpas_sg.crc32(0, P, AStream.Size) then
          vRes := Z_DATA_ERROR;
      end
      else
        vRes := Z_DATA_ERROR;
    end;
  finally
    if P <> nil then
      FreeMem(P);
    GZDone(PGZStream, True);
  end;
  if (vRes = 0) and (AStream.Size > 0) then Result := True;
  if Result then
    AStream.Position := 0;
end;

type
  TsgByteArray = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PsgByteArray = ^TsgByteArray;

function TsgUnzipFile.SearchCentralDir: Integer;
const BUFREADCOMMENT = $400;
var
  I, vMaxBack, vBackRead, vReadPos, vReadSize, TempReadResult: Integer;
  vBuf: PByte;
  vTempByteArray: PsgByteArray;
begin
  vMaxBack := $ffff; // maximum size of global comment
  Result := 0;
  if vMaxBack > FStream.Size then
    vMaxBack := FStream.Size;
  vBackRead := 4;
  vBuf := AllocMem(BUFREADCOMMENT+vBackRead);
  try
    while vBackRead < vMaxBack do
    begin
      if ( vBackRead + BUFREADCOMMENT) > vMaxBack then
        vBackRead := vMaxBack
      else
        vBackRead := vBackRead + BUFREADCOMMENT;
      vReadPos := FStream.Size - vBackRead;
      vReadSize := IfThen((BUFREADCOMMENT+4) < (FStream.Size-vReadPos),
        (BUFREADCOMMENT+4), (FStream.Size-vReadPos));
      FStream.Position := vReadPos; //FStream.Seek(vReadPos, soFromBeginning);
      TempReadResult := FStream.Read(vBuf^, vReadSize);
      if TempReadResult <> vReadSize then
        Break;
      vTempByteArray := PsgByteArray(vBuf);
      I := vReadSize-3;
      while I > 0 do
      begin
        if (vTempByteArray^[I] = $50) and (vTempByteArray^[I+1] = $4b) and
           (vTempByteArray^[I+2] = $05) and (vTempByteArray^[I+3] = $06) then
        begin
          Result := vReadPos + I;
          Break;
        end;
        Dec(I);
      end;
      if Result <> 0 then
        Break;
    end;
  finally
    FreeMem(vBuf);
  end;
end;

function TsgUnzipFile.GetContent: TList;
begin
  Result := FContents;
end;

procedure TsgUnzipFile.LoadFromFile(const AFileName: string);
var
  vFileStream: TFileStream;
begin
  vFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(vFileStream);
  finally
    vFileStream.Free;
  end;
end;

procedure TsgUnzipFile.LoadFromStream(AStream: TStream);
begin
  FStream.Clear;
  FStream.CopyFrom(AStream, 0);
  if not GetContents then
  begin
    FStream.Clear;
    raise EsgUnzipFile.Create('Cannot unpack stream');
  end;
end;

function TsgUnzipFile.GetContents: Boolean;
var
  I, vCntrDirPos: Integer;
  vCntrDirHdr: TsgZIPCntrDirHdr;
  vFileHdr: TsgZIPLocalFileHdr;
  PFileInfo: PsgZIPLocalFileInfo;
  vStr, vPass: string;
  vTemp: AnsiString;

{$IFDEF SG_SGZIP_DEBUG}
  Mft: TMemoryStream;
  Ptitle, PEnd: PAnsiChar;
  Pos: Integer;
  vTemp1: string;
{$ENDIF}
begin
  FillChar(vCntrDirHdr, SizeOf(vCntrDirHdr), 0);
  Reset;
  try
    vCntrDirPos := SearchCentralDir;
    if vCntrDirPos > 0 then
    begin
      FStream.Position := vCntrDirPos;
      FStream.Read(vCntrDirHdr, SizeOf(TsgZIPCntrDirHdr));
    end;
    FStream.Position := vCntrDirHdr.CntrlDirOffSet;
    vPass := #1;

    for I := 0 to vCntrDirHdr.CntrlDirEntries - 1 do
    begin
      New(PFileInfo);
      FContents.Add(PFileInfo);
      FStream.Read(vFileHdr, SizeOf(TsgZIPLocalFileHdr));

      //Coded
      //if (vFileHdr.Flag and 1) <> 0 then raise EsgDWFUnzip.Create('Cannot unpack ciphered DWF');
      //SetLength(vStr, vFileHdr.FileNameLen);
      //FStream.Read(PChar(vStr)^, vFileHdr.FileNameLen);
      //2009 Delphi changes;
      SetLength(vTemp, vFileHdr.FileNameLen);
      FStream.Read(PAnsiChar(vTemp)^, vFileHdr.FileNameLen * SizeOf(AnsiChar));
      vStr := string(vTemp);
      PFileInfo^.FileName := vStr;
      PFileInfo^.Password := vPass;
      PFileInfo^.ComprMethod := vFileHdr.ComprMethod;
      PFileInfo^.CRC32 := vFileHdr.CRC32;
      PFileInfo^.OffsetInZip := vFileHdr.RelOffLocal + 30 + vFileHdr.ExtraLen + vFileHdr.FileNameLen;
      PFileInfo^.UncomprSize := vFileHdr.UncomprSize;
      PFileInfo^.ComprSize := vFileHdr.ComprSize;
      PFileInfo^.ExtFileAttr := vFileHdr.ExtFileAttr;

      PreProcessWork(I, Pointer(PFileInfo), Pointer(@vFileHdr));

{$IFDEF SG_SGZIP_DEBUG}
      //if not (PFileInfo^.FileExt in [feUndefined]) then
      begin
        Pos := FStream.Position;
        Mft := TMemoryStream.Create;
        try
          ExtractFile(I,Mft);
          vTemp1 := vStr;
          ReplaceAnsi(vTemp1,'\','_');
          ReplaceAnsi(vTemp1,'/','_');
          Mft.SaveToFile('D:\DFW_Test\T_'+IntToStr(I)+vTemp1);
          Mft.Size := Mft.Size + 1;
          Ptitle := PAnsiChar(Mft.Memory);
          PEnd := Ptitle;
          Inc(PEnd, Mft.Size-1);
          PEnd^ := #0;
        finally
          FStream.Position := Pos;
          Mft.Free;
        end;
      end;
{$ENDIF}
      FStream.Position := FStream.Position + vFileHdr.ExtraLen + vFileHdr.FileCommmentLen;
    end;
  except
    Result := False;
    PostProcessWork(Result);
    Exit;
  end;
  Result := True;
  PostProcessWork(Result);
end;

function TsgUnzipFile.GetFileAttr(const AIndex: Integer): Cardinal;
begin
  Result := PsgZIPLocalFileInfo(FContents[AIndex])^.ExtFileAttr;
end;

function TsgUnzipFile.GetFileIndexFromFileName(
  const AFileName: string): Integer;
var
  I: Integer;
  vPPFileInfo: PsgZIPLocalFileInfo;
begin
  Result := -1;
  for I := 0 to FContents.Count - 1 do
  begin
    vPPFileInfo := PsgZIPLocalFileInfo(FContents[I]);
    // skip dir
    if vPPFileInfo^.ExtFileAttr and $10 <> 0 then
      Continue;
    if AnsiUpperCase(vPPFileInfo^.FileName) = AnsiUpperCase(AFileName) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TsgUnzipFile.GetFileName(const AIndex: Integer): string;
begin
  Result := PsgZIPLocalFileInfo(FContents[AIndex])^.FileName;
end;

procedure TsgUnzipFile.PostProcessWork(const ResultUnZip: Boolean);
begin
end;

procedure TsgUnzipFile.PreProcessWork(const AIndex: Integer;
      const APFileInfo: Pointer; const APFileHdr: Pointer);
begin
end;

procedure TsgUnzipFile.Reset;
var
  I: Integer;
begin
  for I := 0 to FContents.Count - 1 do
  begin
    PsgZIPLocalFileInfo(FContents[I])^.FileName := '';
    PsgZIPLocalFileInfo(FContents[I])^.Password := '';
    Dispose(PsgZIPLocalFileInfo(FContents[I]));
  end;
  FContents.Clear;
end;

{ TsgDWFUnzip }

constructor TsgDWFUnzip.Create;
begin
  inherited Create;
  FEPlotSections := TList.Create;
  FLayouts := TStringList.Create;
end;

destructor TsgDWFUnzip.Destroy;
begin
  inherited Destroy;
  FEPlotSections.Free;
  FLayouts.Free;
end;

procedure TsgDWFUnzip.AddPlotSections(const AMime, AHref: string;
  const ALayoutIndex: Integer);
var
  vFileIndex: Integer;
  vPPFileInfo: PsgZIPLocalFileInfo;
  vPEPlotSection: PsgEPlotSection;
begin
  if AHref = '' then
    Exit;
  vFileIndex := GetFileIndexFromFileName(AHref);
  if vFileIndex < 0 then
    Exit;

  vPPFileInfo := PsgZIPLocalFileInfo(FContents[vFileIndex]);

  if AMime = '' then
    vPPFileInfo^.FileExt := GetFileExt(AHref)
  else
    vPPFileInfo^.FileExt := GetFileExtOnMime(AMime);

  if vPPFileInfo^.FileExt in [feW2D,feW3D,feTIFF] then
  begin
    New(vPEPlotSection);
    FEPlotSections.Add(vPEPlotSection);
    vPEPlotSection^.IndInZip := vFileIndex;
    vPEPlotSection^.FileLength := vPPFileInfo^.UncomprSize;
    vPEPlotSection^.Graphic := vPPFileInfo^.FileName;
    vPEPlotSection^.FileExt := vPPFileInfo^.FileExt;
    vPEPlotSection^.LayoutIndex := ALayoutIndex;
    Inc(FTotalSize, vPPFileInfo^.ComprSize);
  end;
end;

procedure TsgDWFUnzip.PostProcessWork(const ResultUnZip: Boolean);
var
  I, J, K, vManifestIndex, vLayoutIndex: Integer;
  vPPFileInfo: PsgZIPLocalFileInfo;
  vXMLParser: TsgParser;
  vSections, vSection, vToc, vResource: TsgNodeSample;
  vStream: TMemoryStream;
  vLayoutName, vMime, vHref: string;
  function GetAttribute(const ASource:TsgNodeSample; const AName: string): string;
  var
    vAttribute: TsgNodeSample;
  begin
    Result := '';
    if ASource.AttributeNodesCount > 0 then
    begin
      vAttribute := ASource.GetAttributeByName(AName);
      if vAttribute <> nil then
        Result :=  vAttribute.ValueData.ValueAsText;
    end;
  end;

begin
  vManifestIndex := -1;
  // Search in the root directory of manifest.xml
  for I := 0 to FContents.Count - 1 do
  begin
    vPPFileInfo := PsgZIPLocalFileInfo(FContents[I]);
    // skip dir
    if vPPFileInfo^.ExtFileAttr and $10 <> 0 then
      Continue;
    if AnsiUpperCase(GetFileNameByParam('/:\',vPPFileInfo^.FileName)) = 'MANIFEST.XML' then
    begin
      vManifestIndex := I;
      Break;
    end;
  end;
  if vManifestIndex >= 0 then
  begin
    vStream := TMemoryStream.Create;
    try
      if ExtractFile(vManifestIndex, vStream) then
      begin
        vXMLParser := TsgParser.Create;
        try
          vStream.Position := 0;
          vXMLParser.LoadFromStream(vStream);
          vSections := TsgNodeSample(vXMLParser.Root.Nodes[0].GetChildByName('Sections'));
          if (vSections <> nil) and (vSections.ChildNodesCount > 0) then
          begin
            for I := 0 to vSections.ChildNodesCount - 1 do
            begin
              vSection := vSections.ChildNodes[I];
              if  (vSection.Name = 'Section') and (vSection.ChildNodesCount > 0) then
              begin
                vLayoutIndex := -1;
                vLayoutName :=  GetAttribute(vSection, 'title');
                if vLayoutName <> '' then
                begin
                  vLayoutIndex := Layouts.Add(vLayoutName);
                end;
                for J := 0 to vSection.ChildNodesCount - 1 do
                begin
                  vToc := vSection.ChildNodes[J];
                  if (vToc.Name = 'Toc') and (vToc.ChildNodesCount > 0) then
                  begin
                    for K := 0 to vToc.ChildNodesCount - 1 do
                    begin
                      vResource := vToc.ChildNodes[K];
                      if vResource.Name = 'Resource' then
                      begin
                        vMime := GetAttribute(vResource, 'mime');
                        vHref := GetAttribute(vResource, 'href');
                        if vHref[1] = '/' then
                          Delete(vHref,1,1);
                        AddPlotSections(vMime, vHref, vLayoutIndex);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          vXMLParser.Free;
        end;
      end;
    finally
      vStream.Free;
    end;
  end;
end;

procedure TsgDWFUnzip.PreProcessWork(const AIndex: Integer;
  const APFileInfo: Pointer; const APFileHdr: Pointer);
begin
end;

function TsgDWFUnzip.GetEPlotSections: TList;
begin
  Result := FEPlotSections;
end;

function TsgDWFUnzip.GetFileExt(const AFileName: string): TsgDWFFileExt;
var
  vExt: string;
begin
  Result := feUndefined;
  vExt := AnsiUpperCase(GetFileExtByParam('\:/',AFileName));
  if vExt = '.W2D' then Result := feW2D;
  if vExt = '.W3D' then Result := feW3D;
  if vExt = '.PNG' then Result := fePNG;
  if vExt = '.XML' then Result := feXML;
//  if vExt = '.W3D_S' then Result := feW3D
end;

function TsgDWFUnzip.GetFileExtOnMime(const AMime: string): TsgDWFFileExt;
var
  vExt: string;
begin
  Result := feUndefined;
  vExt := AnsiUpperCase(AMime);
  if vExt = 'APPLICATION/X-W2D' then Result := feW2D;
  if vExt = 'APPLICATION/X-W3D' then Result := feW3D;
  if vExt = 'IMAGE/TIFF' then Result := feTIFF;
end;

function TsgDWFUnzip.GetXMLType(FileName: string): TsgXMLFileTag;
var
  S: string;
begin
  Result := xmlUndefined;
  S := AnsiUpperCase(ExtractFileName(FileName));
  if S = 'MANIFEST' then
    Result := xmlManifest;
  if S = 'DESCRIPTOR' then
    Result := xmlDescriptor;
end;

procedure TsgDWFUnzip.Reset;
var
  I: Integer;
begin
  inherited Reset;
  for I := 0 to FEPlotSections.Count - 1 do
  begin
    PsgEPlotSection(FEPlotSections[I])^.Title := '';
    PsgEPlotSection(FEPlotSections[I])^.LayoutName := '';
    PsgEPlotSection(FEPlotSections[I])^.Company := '';
    PsgEPlotSection(FEPlotSections[I])^.Author := '';
    PsgEPlotSection(FEPlotSections[I])^.CreationTime := '';
    PsgEPlotSection(FEPlotSections[I])^.CreationTime := '';
    PsgEPlotSection(FEPlotSections[I])^.ModificationTime := '';
    PsgEPlotSection(FEPlotSections[I])^.Width := '';
    PsgEPlotSection(FEPlotSections[I])^.Height := '';
    PsgEPlotSection(FEPlotSections[I])^.Units := '';
    PsgEPlotSection(FEPlotSections[I])^.Order := '';
    PsgEPlotSection(FEPlotSections[I])^.Graphic := '';
    Dispose(PsgEPlotSection(FEPlotSections[I]));
  end;
  FEPlotSections.Clear;
end;

end.

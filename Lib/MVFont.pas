{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                           Font                             }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit MVFont;

interface

{$INCLUDE SGDXF.inc}
uses
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF SGFPC}
  LMessages, LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, {$IFDEF SGFPC}FileUtil,{$ENDIF}
{$ENDIF}
  sgConsts
{$IFDEF SGDEL_XE2}
  ,System.UITypes, System.Types
{$ENDIF};
{$R-}

type

  PExtFontData = ^TExtFontData;
  TExtFontData = packed record
    D: array [0..3] of Word;
  end;

 { TmvFont }

 TmvFont = class(TFont)
  private
  {$IFDEF SGFPC}
    FOwnerLock: PRTLCriticalSection;
  {$ENDIF}
  protected
    function GetHandle: HFont;
    procedure GetLogFont(var LogFont: TLogFont); virtual;
    function GetStyle: TmvFontStyles;
    procedure SetHandle(Value: HFont);
    procedure SetStyle(Value: TmvFontStyles);
    {$IFDEF SGFPC}
    property OwnerCriticalSection: PRTLCriticalSection read FOwnerLock write FOwnerLock;
    {$ENDIF}
  public
    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    property Handle: HFont read GetHandle write SetHandle;
  published
    property Style: TmvFontStyles read GetStyle write SetStyle default [];
    property Charset default DEFAULT_CHARSET;
    property Color default clWindowText;
  end;

  TmvExtFont = class(TmvFont)
  private
    function IsOrientStored: Boolean;
  protected
    function GetEscapement: Smallint;
    procedure GetLogFont(var LogFont: TLogFont); override;
    function GetExtData: TExtFontData;
    function GetExtValue(Index: Integer): Word;
    function GetName: TFontName;
    function GetOrientation: Smallint;
    function GetWeight: Word;
    function GetWidth: Word;
    procedure SetEscapement(Value: Smallint);
    procedure SetExtData(AExtFontData: TExtFontData);
    procedure SetExtValue(Index: Integer; Value: Word);
    procedure SetName(Value: TFontName);{$IFDEF SGFPC} override;{$ENDIF}
    procedure SetOrientation(Value: Smallint);
    procedure SetWeight(Value: Word);
    procedure SetWidth(Value: Word);
  published
    property Width: Word read GetWidth write SetWidth default 0;
    property Weight: Word read GetWeight write SetWeight default 0;
    property Escapement: Smallint read GetEscapement write SetEscapement default 0;
    property Orientation: Smallint read GetOrientation write SetOrientation stored IsOrientStored;
    property Name: TFontName read GetName write SetName;
  end;

{$IFNDEF SG_NON_WIN_PLATFORM}
  TReallocCapacity = {$IFDEF SGFPC}PtrInt{$ELSE}{$IFDEF SGDEL_11_ALEXANDRIA}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF};

  TClipboardStream = class(TMemoryStream)
  protected
    FHandle: THandle;
    function Realloc(var NewCapacity: TReallocCapacity): Pointer; override;
  public
    constructor Create(AHandle: THandle);
    function Handle: THandle;
  end;

  TFileMapStream = class(TMemoryStream)
  private
    FFileHandle: Integer;
    FMapHandle: Integer;
    FFileSize: Integer;
    FDelta: Integer;
    FReadOnly: Boolean;
    procedure FreeMapping;
    procedure SetDelta(Value: Integer);
  protected
    function Realloc(var NewCapacity: TReallocCapacity): Pointer; override;
  public
    constructor Create(const FileName: String; Mode: Word);
    destructor Destroy; override;
    procedure Flush;
    property Capacity;
    property ReadOnly: Boolean read FReadOnly;
    property Delta: Integer read FDelta write SetDelta;
  end;
{$ENDIF}

implementation

uses
  sgFunction;

{$IFNDEF SGFPC}
type

  TFontPad = class(TPersistent)
  private
  {$HINTS OFF}
    FOnChange: TNotifyEvent;
  {$HINTS ON}
    FResource: PResource;
  end;
{$ENDIF}

{$DEFINE NEED_CVT_MVFONTSTYLE}
{$IFDEF SGFPC}
  {$IFDEF FPC_OBJFPC}
  {$UNDEF NEED_CVT_MVFONTSTYLE}
  {$ENDIF}
{$ELSE}
  {$UNDEF NEED_CVT_MVFONTSTYLE}
{$ENDIF}

const
  Pitches: array[TFontPitch] of Byte = (DEFAULT_PITCH, VARIABLE_PITCH,
    FIXED_PITCH);

var
  FontLock: TRTLCriticalSection;

procedure Angle(var Value: Smallint);
begin
  if Abs(Value) >= 3600 then Value := Value mod 3600;
end;

(*
{$IFNDEF SG_NON_WIN_PLATFORM}
function FontProc(const LF: TLogFont; P2,P3: Pointer; var P: TPoint): Integer; stdcall;
begin
  Inc(P.X, LF.lfWidth);
  Inc(P.Y, LF.lfHeight);
  Result := 1;
end;
{$ELSE}
function FontProc(var AEnumLogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx; AFontType: Longint; var P: TPoint):Longint; //extdecl;
begin
  Inc(P.X, AEnumLogFont.elfLogFont.lfWeight);
  Inc(P.Y, AEnumLogFont.elfLogFont.lfHeight);
  Result := 1;
end;
{$ENDIF}

function CondWidth(Font: TFont): Integer;
var
  vDC: HDC;
  vP: TPoint;
begin
  vP := Point(0, 0);
  vDC := GetDC(0);
  try
{$IFNDEF SG_NON_WIN_PLATFORM}
    EnumFonts(vDC, PChar(Font.Name), @FontProc, @vP);
{$ELSE}
    EnumFontFamilies(vDC, PChar(Font.Name), @FontProc, LPARAM(@vP));
{$ENDIF}
  finally
    ReleaseDC(0, vDC)
  end;
  if vP.Y = 0 then
    Result := 0
  else
  begin
    vP.Y := vP.Y * 3;
    Result := (vP.X * Abs(Font.Height) shl 1 + vP.Y shr 1) div vP.Y;
  end;
end;*)

function CondWidth(Font: TFont): Integer;
var
  vLogFont: tagLOGFONTW;
begin
  vLogFont.lfWidth := 0;
  GetObject(Font.Handle, SizeOf(tagLOGFONTW), @vLogFont);
  Result := vLogFont.lfWidth;
end;

{ TmvFont }

function TmvFont.HandleAllocated: Boolean;
begin
{$IFNDEF SGFPC}
  Result := TFontPad(Self).FResource^.Handle <> 0;
{$ELSE}
  Result := inherited HandleAllocated;
{$ENDIF}
end;

procedure TmvFont.HandleNeeded;
begin
  GetHandle;
end;
function TmvFont.GetStyle: TmvFontStyles;
begin
{$IFDEF NEED_CVT_MVFONTSTYLE}
  Result := FontStylesToMVFontStyles(inherited Style);
{$ELSE}
  Result := TmvFontStyles(inherited Style);
{$ENDIF}
end;

procedure TmvFont.SetStyle(Value: TmvFontStyles);
const
  NotUse = [fmUpward, fmDownward];
begin
  if Value * NotUse = NotUse then
    Value := Value - Style * NotUse;
  if Value * NotUse = NotUse then
    Value := Value - NotUse;
  if Value <> Style then
    inherited Style := {$IFDEF NEED_CVT_MVFONTSTYLE}MVFontStylesToFontStyles(Value){$ELSE}TFontStyles(Value){$ENDIF};
end;

procedure TmvFont.GetLogFont(var LogFont: TLogFont);
const
  Weights: array [Boolean] of Integer = (FW_NORMAL, FW_BOLD);
var
  vFontData: {$IFNDEF SGFPC}^{$ENDIF}TFontData;
  vFontName: string;
begin
{$IFNDEF SGFPC}
  vFontData := @TFontPad(Self).FResource^.Font;
{$ELSE}
  vFontData := FontData;
{$ENDIF}
  LogFont.lfHeight := vFontData.Height;
  LogFont.lfQuality := DEFAULT_QUALITY;
  LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  LogFont.lfPitchAndFamily := Pitches[vFontData.Pitch];
  LogFont.lfCharSet := vFontData.Charset;
  LogFont.lfItalic := Byte(fsItalic in vFontData.Style);
  LogFont.lfUnderline := Byte(fsUnderline in vFontData.Style);
  LogFont.lfStrikeOut := Byte(fsStrikeOut in vFontData.Style);
  LogFont.lfWeight := Weights[fsBold in vFontData.Style];
  if fmCondensed in {$IFDEF NEED_CVT_MVFONTSTYLE}FontStylesToMVFontStyles(vFontData.Style){$ELSE}TmvFontStyles(vFontData.Style){$ENDIF} then
    LogFont.lfWidth := CondWidth(Self)
  else
    LogFont.lfWidth := 0;
  if fmUpward in {$IFDEF NEED_CVT_MVFONTSTYLE}FontStylesToMVFontStyles(vFontData.Style){$ELSE}TmvFontStyles(vFontData.Style){$ENDIF} then
    LogFont.lfEscapement := 900
  else
    if fmDownward in {$IFDEF NEED_CVT_MVFONTSTYLE}FontStylesToMVFontStyles(vFontData.Style){$ELSE}TmvFontStyles(vFontData.Style){$ENDIF} then
      LogFont.lfEscapement := 2700
    else
      LogFont.lfEscapement := 0;
  LogFont.lfOrientation := LogFont.lfEscapement;
{$IFDEF SGDEL_2009}
  vFontName := GetName;
{$ELSE}
  vFontName := vFontData.Name;
{$ENDIF}
  StrPLCopy(LogFont.lfFaceName, vFontName, High(LogFont.lfFaceName) - 1);
end;

function TmvFont.GetHandle: HFont;
{$IFNDEF SGFPC}
var
  vLogFont: TLogFont;
  vPResource: PResource;
{$ENDIF}
begin
{$IFNDEF SGFPC}
  vPResource := TFontPad(Self).FResource;
  if vPResource^.Handle = 0 then
  begin
    if OwnerCriticalSection = nil then
      OwnerCriticalSection := @FontLock;
    Lock;
    try
      GetLogFont(vLogFont);
      vPResource^.Handle := CreateFontIndirect(vLogFont);
    finally
      Unlock;
    end;
  end;
  Result := vPResource^.Handle;
{$ELSE}
  Result := inherited Handle;
{$ENDIF}
end;

procedure TmvFont.SetHandle(Value: HFont);
begin
  {$IFDEF SGFPC}
  inherited Handle := Value;
  {$ELSE}
  inherited SetHandle(Value);
  {$ENDIF}
end;

{ TmvExtFont }

function TmvExtFont.GetExtData: TExtFontData;
{$IFNDEF SGFPC}
var
  S: string;
{$ENDIF}
begin
{$IFNDEF SGFPC}
  S := inherited GetName;
  if S <> '' then
    Result := PExtFontData(TsgNativeInt(S) + Length(S) * SizeOf(S[1]) - SizeOf(TExtFontData))^
  else
{$ENDIF}
    FillChar(Result, SizeOf(Result), 0);
end;

function TmvExtFont.GetEscapement: Smallint;
begin
{$IFNDEF SGFPC}
  Result := Smallint(GetExtValue(2));
{$ELSE}
  Result := inherited GetOrientation;
{$ENDIF}
end;

procedure TmvExtFont.GetLogFont(var LogFont: TLogFont);
var
  ExData: TExtFontData;
  vFS{$IFDEF NEED_CVT_MVFONTSTYLE}, vTmpFS{$ENDIF}: TmvFontStyles;
  vFontData: {$IFNDEF SGFPC}^{$ENDIF}TFontData;
begin
{$IFNDEF SGFPC}
  vFontData := @TFontPad(Self).FResource^.Font;
{$ELSE}
  vFontData := FontData;
{$ENDIF}
  ExData := GetExtData;
{$IFDEF NEED_CVT_MVFONTSTYLE}
  System.Move(vFontData.Style, vFS, SizeOf(vFontData.Style));
{$ELSE}
  TFontStyles(vFS) := vFontData.Style;
{$ENDIF}
  if ExData.D[0] <> 0 then
{$IFDEF NEED_CVT_MVFONTSTYLE}
  begin
    vTmpFS := vFS - [fmCondensed];
    System.Move(vTmpFS, vFontData.Style, SizeOf(vFontData.Style));
  end;
{$ELSE}
    TmvFontStyles(vFontData.Style) := vFS - [fmCondensed];
{$ENDIF}
  inherited GetLogFont(LogFont);
{$IFDEF NEED_CVT_MVFONTSTYLE}
  System.Move(vFS, vFontData.Style, SizeOf(vFontData.Style));
{$ELSE}
  vFontData.Style := TFontStyles(vFS);
{$ENDIF}
  if ExData.D[0] <> 0 then
    LogFont.lfWidth := ExData.D[0];
  if ExData.D[1] <> 0 then
    LogFont.lfWeight := ExData.D[1];
  if ExData.D[2] <> 0 then
    LogFont.lfEscapement := Smallint(ExData.D[2]);
  if ExData.D[3] <> 0 then
    LogFont.lfOrientation := Smallint(ExData.D[3]);
  if LogFont.lfEscapement < 0 then
    Inc(LogFont.lfEscapement, 3600);
  if LogFont.lfOrientation < 0 then
    Inc(LogFont.lfOrientation, 3600);
end;

function TmvExtFont.GetOrientation: Smallint;
begin
  Result := Smallint(GetExtValue(3));
end;

function TmvExtFont.GetName: TFontName;
begin
  Result := inherited GetName;
  SetLength(Result, StrLen(PChar(Result)));
end;

function TmvExtFont.GetWeight: Word;
begin
  Result := GetExtValue(1);
end;

function TmvExtFont.GetWidth: Word;
begin
  Result := GetExtValue(0);
end;

procedure TmvExtFont.SetEscapement(Value: Smallint);
begin
  Angle(Value);
{$IFNDEF SGFPC}
  SetExtValue(2, Value);
{$ELSE}
  inherited SetOrientation(Value);
{$ENDIF}
end;

procedure TmvExtFont.SetName(Value: TFontName);
{$IFNDEF SGFPC}
const
//  MaxNameLen = LF_FACESIZE - 1 - SizeOf(TExtFontData) - 1;
  MaxNameLen = High(TFontDataName) - SizeOf(TExtFontData) - 1; // one byte reserved to saving #0
  MapExtDataCharCount = SizeOf(TExtFontData) div SizeOf(Char);
{$ENDIF}
begin
{$IFNDEF SGFPC}
  if Value = '' then
    Exit;
  if Length(Value) > MaxNameLen then
    SetLength(Value, MaxNameLen);
  Value := Value +  // Value[MaxNameLen + 1] = #0 and ExData structure must be zero
    StringOfChar(Char(#0), MaxNameLen - Length(Value) + 1 + MapExtDataCharCount);
{$ENDIF}
  inherited SetName(Value);
end;

procedure TmvExtFont.SetOrientation(Value: Smallint);
begin
  Angle(Value);
{$IFNDEF SGFPC}
  SetExtValue(3, Value);
{$ELSE}
  inherited SetOrientation(Value);
{$ENDIF}
end;

procedure TmvExtFont.SetWeight(Value: Word);
begin
  SetExtValue(1, Value);
end;

procedure TmvExtFont.SetWidth(Value: Word);
begin
  SetExtValue(0, Value);
end;

function TmvExtFont.IsOrientStored: Boolean;
begin
{$IFDEF SGFPC}
  Result := inherited Orientation <> Escapement;
{$ELSE}
  {$IFDEF SGDEL_2005}
  Result := inherited Orientation <> Escapement;
  {$ELSE}
  Result := True;
  {$ENDIF}
{$ENDIF}
end;

function TmvExtFont.GetExtValue(Index: Integer): Word;
begin
  Result := GetExtData.D[Index];
end;

procedure TmvExtFont.SetExtData(AExtFontData: TExtFontData);
{$IFNDEF SGFPC}
var
  ExData: PExtFontData;
  S: string;
  vOffset: TsgNativeUInt;
{$ENDIF}
begin
{$IFNDEF SGFPC}
  S := inherited GetName;
  if S = '' then
    Exit;
  vOffset := Length(S) * SizeOf(S[1]) - SizeOf(TExtFontData);
  Pointer(ExData) := Pointer(TsgNativeUInt(Pointer(S)) + vOffset);
  ExData^ := AExtFontData;
  S := S {$IFDEF SGDEL_2009}+ #0#0 {$ENDIF}; // UTF-8 convertion trim one #0 symbol
  inherited SetName(string(S));
{$ENDIF}
end;

procedure TmvExtFont.SetExtValue(Index: Integer; Value: Word);
var
  ExData: TExtFontData;
begin
  if Value = GetExtValue(Index) then
    Exit;
  ExData := GetExtData;
  if (Index = 2) and (ExData.D[2] = ExData.D[3]) then
    ExData.D[3] := Value;
  ExData.D[Index] := Value;
  SetExtData(ExData);
end;

{$IFNDEF SG_NON_WIN_PLATFORM}
{ TClipboardStream }

constructor TClipboardStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
  if AHandle <> 0 then
    SetPointer(GlobalLock(AHandle), GlobalSize(AHandle));
end;

function TClipboardStream.Handle: THandle;
begin
  if FHandle = 0 then
  begin
    FHandle := GlobalHandle(Memory);
    GlobalUnlock(FHandle);
    SetPointer(nil, Size);
  end;
  Result := FHandle;
end;

function TClipboardStream.Realloc(var NewCapacity: TReallocCapacity): Pointer;
begin
  if FHandle = 0 then
    Result := inherited Realloc(NewCapacity)
  else
  begin
    Result := Memory;
    if NewCapacity = 0
      then GlobalUnlock(FHandle);
  end;
end;

{ TFileMapStream }

constructor TFileMapStream.Create(const FileName: string; Mode: Word);
const
  Excepts: array[Boolean] of ExceptClass = (EFOpenError, EFCreateError);
  Modes: array[Boolean] of string = ('open','creat');
var Created: Boolean;
begin
  inherited Create;
  Created := Mode=fmCreate;
  FDelta := $10000;
  FReadOnly := (Mode and 3) = fmOpenRead;
  if Created then
    FFileHandle := FileCreate(FileName)
  else
    FFileHandle := FileOpen(FileName,Mode);
  if FFileHandle < 0 then
    raise Excepts[Created].CreateFmt('"%s" file %sing failure', [FileName, Modes[Created]]);
{$IFDEF MSWINDOWS}
  FFileSize := Windows.GetFileSize(FFileHandle, nil);
{$ELSE}
  FFileSize := {$IFNDEF SG_NON_WIN_PLATFORM}Windows.{$ENDIF}FileSize(FFileHandle);
{$ENDIF}
  if not Created then Size := FFileSize;
end;

destructor TFileMapStream.Destroy;
begin
  FreeMapping;
  if FFileHandle >= 0 then
  begin
    if Size <> FFileSize then
    begin
      FileSeek(FFileHandle,Size,0);
      SetEndOfFile(FFileHandle);
    end;
    FileClose(FFileHandle);
  end;
  inherited Destroy;
end;

procedure TFileMapStream.FreeMapping;
begin
  if Memory <> nil then UnmapViewOfFile(Memory);
  if FMapHandle <> 0 then FileClose(FMapHandle);
end;

procedure TFileMapStream.Flush;
begin
  if Memory <> nil then FlushViewOfFile(Memory,0);
end;

procedure TFileMapStream.SetDelta(Value: Integer);
begin
  FDelta := RoundToPowerOf2(Value);
end;

function TFileMapStream.Realloc(var NewCapacity: TReallocCapacity): Pointer;
  procedure Error;
  begin
    raise EStreamError.Create('Failure of mapping file in memory');
  end;
const
  Protects: array[Boolean] of Integer = (PAGE_READWRITE,PAGE_READONLY);
  Accesses: array[Boolean] of Integer = (FILE_MAP_WRITE,FILE_MAP_READ);
begin
  if NewCapacity <= Capacity then begin
    NewCapacity := Capacity;
    Result := Memory;
    Exit;
  end;
  FreeMapping;
  if not FReadOnly then NewCapacity := (NewCapacity+Delta-1) and not (Delta-1);
  FMapHandle := CreateFileMapping(FFileHandle, nil, Protects[FReadOnly], 0, NewCapacity, nil);
  if FMapHandle=0 then Error;
  Result := MapViewOfFile(FMapHandle, Accesses[FReadOnly], 0, 0, 0);
  if Result=nil then Error;
end;
{$ENDIF}

initialization

{$IFNDEF SGFPC}
  InitializeCriticalSection(FontLock);
{$ELSE}
  System.InitCriticalSection(FontLock);
{$ENDIF}

finalization
{$IFNDEF SGFPC}
  DeleteCriticalSection(FontLock);
{$ELSE}
  System.DoneCriticalsection(FontLock);
{$ENDIF}

end.

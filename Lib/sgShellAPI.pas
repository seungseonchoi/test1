{************************************************************}
{                 Delphi VCL Extensions                      }
{                                                            }
{                Win32 API Shell Extensions                  }
{                                                            }
{      Copyright (c) 2002-2015 SoftGold software company     }
{                                                            }
{************************************************************}
unit sgShellAPI;
{$INCLUDE SGDXF.inc}
{$IFNDEF SG_NON_WIN_PLATFORM}
  {$DEFINE SG_WIN_PLATFORM}
{$ENDIF}

{$IFDEF SG_FM_WINDOWS}
  {$DEFINE SG_WIN_PLATFORM}
{$ENDIF}

interface
uses
  {$IFDEF SG_WIN_PLATFORM}
   Windows, ShlObj, ActiveX, Registry,
  {$ENDIF}
  {$IFDEF SGFPC}
  LMessages, LCLIntf, LCLType, Types, sgFunction,
  {$ENDIF}
  Messages, SysUtils, Classes;

{$IFNDEF SG_WIN_PLATFORM}//sgShellAPI.pas(20,3) Fatal: Cannot find shlobj used by sgShellAPI.
const
  FILE_ATTRIBUTE_DIRECTORY        = $00000010;
  BIF_RETURNONLYFSDIRS            = $0001;  // For finding a folder to start document searching
type
  PItemIDList = {$IFDEF MSWINDOWS}ShlObj.PItemIDList{$ELSE}Pointer{$ENDIF};

function GetFileAttributes(const AFileName : string) : Longint;
{$ENDIF}

function GetSpecialFolderPath(AFolder: Integer; var APath: string): Boolean;
function DirectoryExists(const Name: string): Boolean;
function SelectDirectory(const ACaption: string; const ARoot: string;
  var ADirectory: string; AOwnerWindow: HWND;
  const ABrowseFlags: Cardinal = BIF_RETURNONLYFSDIRS): Boolean; overload;
function SelectDirectory(const ACaption: string; ARootItemID: Pointer;
  var ADirectory: string; AOwnerWindow: HWND;
  const ABrowseFlags: Cardinal = BIF_RETURNONLYFSDIRS): Boolean; overload;

implementation

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

{$IFDEF SGDEL_3}
const
  CSIDL_DESKTOP = $0000;
  CSIDL_INTERNET = $0001;
  CSIDL_PROGRAMS = $0002;
  CSIDL_CONTROLS = $0003;
  CSIDL_PRINTERS = $0004;
  CSIDL_PERSONAL = $0005;
  CSIDL_FAVORITES = $0006;
  CSIDL_STARTUP = $0007;
  CSIDL_RECENT = $0008;
  CSIDL_SENDTO = $0009;
  CSIDL_BITBUCKET = $000a;
  CSIDL_STARTMENU = $000b;
  CSIDL_DESKTOPDIRECTORY = $0010;
  CSIDL_DRIVES = $0011;
  CSIDL_NETWORK = $0012;
  CSIDL_NETHOOD = $0013;
  CSIDL_FONTS = $0014;
  CSIDL_TEMPLATES = $0015;
  CSIDL_COMMON_STARTMENU = $0016;
  CSIDL_COMMON_PROGRAMS = $0017;
  CSIDL_COMMON_STARTUP = $0018;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
  CSIDL_APPDATA = $001a;
  CSIDL_PRINTHOOD = $001b;
  CSIDL_ALTSTARTUP = $001d;
  CSIDL_COMMON_ALTSTARTUP = $001e;
  CSIDL_COMMON_FAVORITES = $001f;
  CSIDL_INTERNET_CACHE = $0020;
  CSIDL_COOKIES = $0021;
  CSIDL_HISTORY = $0022;
{$ENDIF}

{$IFDEF SGFPC}
const
  REGSTR_PATH_EXPLORER = 'Software\Microsoft\Windows\CurrentVersion\Explorer';
  REGSTR_PATH_SPECIAL_FOLDERS = REGSTR_PATH_EXPLORER + '\Shell Folders';
{$ENDIF}

{$IFDEF SG_NON_WIN_PLATFORM}
function SHGetSpecialFolderLocation( hwnd:HWND; csidl:longint;out ppidl: PItemIDList):HResult;
begin
  Result := S_FALSE;
end;

function GetFileAttributes(const AFileName : string) : Longint;
begin
  Result := FileGetAttr(PChar(AFileName));
end;
{$ENDIF}

function GetPIDL(AFolder: DWORD; var PFolderPIDL: PItemIDList): HRESULT;
{$IFDEF SGDEL_2009}
  function IsWin2000or2003orME: Boolean;
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      Result := (Win32MajorVersion = 5) and (Win32MinorVersion in [0, 2])
    else
      Result := (Win32MajorVersion = 4) and (Win32MinorVersion >= 90);
  end;
{$ENDIF}
begin
{$IFDEF SGDEL_2009}
{$IFNDEF BCB}
  if IsWin2000or2003orME then //4.90.3000 Me; 5 - Windows 2000, 2003
    Result := SHGetFolderLocation(0, AFolder, 0, 0, PFolderPIDL)
  else
{$ELSE}
  Result := SHGetSpecialFolderLocation(0, AFolder, PFolderPIDL);
{$ENDIF}
  Result := SHGetSpecialFolderLocation(0, AFolder, PFolderPIDL);
{$ELSE}
  Result := SHGetSpecialFolderLocation(0, AFolder, PFolderPIDL);
{$ENDIF}
end;

function GetSpecialFolderPath(AFolder: Integer; var APath: string): Boolean;
{$IFDEF SG_WIN_PLATFORM}
var
  PFolderPIDL: PItemIDList;
  vPath: array[0..MAX_PATH + 1] of Char;
  vRes: HRESULT;
  vMAlloc: IMAlloc;

  function GetValNameByCode(ACode: Integer): string;
  begin
    Result := 'Desktop';
    case ACode of
      CSIDL_DESKTOP:
        Result := 'Desktop';
      CSIDL_INTERNET:;
      CSIDL_PROGRAMS:
        Result := 'Programs';
      CSIDL_CONTROLS:;
      CSIDL_PRINTERS:
        Result := 'PrintHood';
      CSIDL_PERSONAL:
        Result := 'Personal';
      CSIDL_FAVORITES:
        Result := 'Favorites';
      CSIDL_STARTUP:
        Result := 'Startup';
      CSIDL_RECENT:
        Result := 'Recent';
      CSIDL_SENDTO:
        Result := 'SendTo';
      CSIDL_BITBUCKET:;
      CSIDL_STARTMENU:
        Result := 'Start Menu';
      CSIDL_DESKTOPDIRECTORY:
        Result := 'Desktop';
      CSIDL_DRIVES:;
      CSIDL_NETWORK:;
      CSIDL_NETHOOD:
        Result := 'NetHood';
      CSIDL_FONTS:
        Result := 'Fonts';
      CSIDL_TEMPLATES:
        Result := 'Templates';
      CSIDL_COMMON_STARTMENU:;
      CSIDL_COMMON_PROGRAMS:;
      CSIDL_COMMON_STARTUP:;
      CSIDL_COMMON_DESKTOPDIRECTORY:;
      CSIDL_APPDATA:
        Result := 'AppData';
      CSIDL_PRINTHOOD:
        Result := 'PrintHood';
      CSIDL_ALTSTARTUP:;
      CSIDL_COMMON_ALTSTARTUP:;
      CSIDL_COMMON_FAVORITES:;
      CSIDL_INTERNET_CACHE:
        Result := 'Cache';
      CSIDL_COOKIES:
        Result := 'Cookies';
      CSIDL_HISTORY:
        Result := 'History';
    end;
  end;

  function FindInUsers(var S: string): Boolean;
  var
    vReg: TRegistry;
    vStrs: TStringList;
    I: Integer;
    vVal, vValName, vSearch: string;
    vSearchRec: TSearchRec;
  begin
    Result := False;
    vReg := TRegistry.Create;
    try
      vReg.RootKey := HKEY_USERS;
      vStrs := nil;
      try
        vStrs := TStringList.Create;
        try
          if vReg.OpenKey('', False) then
          begin
            vReg.GetKeyNames(vStrs);
            vValName := GetValNameByCode(AFolder);
            for I := 0 to vStrs.Count - 1 do
            begin
              vReg.CloseKey;
              if vReg.OpenKey(vStrs[I] + '\' + REGSTR_PATH_SPECIAL_FOLDERS, False) then
              begin
                if vReg.ValueExists(vValName) then
                begin
                  vVal := vReg.ReadString(vValName);
                  if (vVal <> '') and (DirectoryExists(vVal)) then
                  begin
                    vSearch := vVal;
                    if vSearch[Length(vSearch)] <> '\' then vSearch := vSearch + '\';
                    vSearch := vSearch + '*.ttf';
                    Result := FindFirst(vSearch, faAnyFile, vSearchRec) = 0;
                    FindClose(vSearchRec);
                    if Result then
                    begin
                      S := vVal;
                      Break;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          vStrs.Free;
          vStrs := nil;
        end;
      except
        Result := False;
        vStrs.Free;
        vReg.Free;
        vReg := nil;
        Exit;
      end;
    finally
      vReg.Free;
    end;
  end;

  function ReadVal(const ARegistry: TRegistry; const AValName: string;
    var S: string): Boolean;
  begin
    Result := False;
    if ARegistry.ValueExists(AValName) then
    begin
      try
        S := ARegistry.ReadString(AValName);
      except
        Exit;
      end;
      Result := True;
    end;
  end;

  function GetFromRegistry(var S: string): Boolean;
  var
    vReg: TRegistry;
    vValName: string;
  begin
    Result := False;
    vReg := TRegistry.Create;
    try
      try
        vReg.RootKey := HKEY_CURRENT_USER;
        if vReg.OpenKey(REGSTR_PATH_SPECIAL_FOLDERS, False) then
        begin
          vValName := GetValNameByCode(AFolder);
          if (vValName = 'Desktop') and (AFolder <> CSIDL_DESKTOP) then
            Result := False
          else
            Result := ReadVal(vReg, vValName, S);
        end;
      except
        Result := False;
      end;
    finally
      vReg.Free;
    end;
  end;

begin
  APath := '';
  FillChar(vPath, SizeOf(vPath), #0);
  PFolderPIDL := nil;
  vRes := GetPIDL(AFolder, PFolderPIDL);
  if vRes = S_OK then
  begin
    if not SHGetPathFromIDList(PFolderPIDL, vPath) then
      Result := GetFromRegistry(APath)
    else
    begin
      Result := True;
      APath := string(vPath);
    end;
  end
  else
    Result := GetFromRegistry(APath);
  if PFolderPIDL <> nil then
  begin
    vRes := SHGetMAlloc(vMAlloc);
    if vRes = S_OK then
      vMAlloc.Free(PFolderPIDL);
    vMAlloc := nil;
  end;
  if (not Result) or (APath = '') then
    Result := FindInUsers(APath);
  if Result and (APath = '') then
    Result := False;
  if Result and (APath <> '') then
  begin
    if APath[Length(APath)] <> '\' then
      APath := APath + '\'
  end;
{$ELSE}
begin
  Result := False;
  APath := '';
{$ENDIF}
end;

function SelectDirectory(const ACaption: string; ARootItemID: Pointer;
  var ADirectory: string; AOwnerWindow: HWND;
  const ABrowseFlags: Cardinal = BIF_RETURNONLYFSDIRS): Boolean;
{$IFDEF SG_WIN_PLATFORM}
var
  I: Integer;
  vBrowseInfo: TBrowseInfo;
  vBuffer, vCopyBuffer: PChar;
  vItemIDList: PItemIDList;
  vActiveWindow: HWND;
begin
  ADirectory := '';
  GetMem(vBuffer, MAX_PATH * SizeOf(Char));
  try
    vBuffer^ := #0;
    FillChar(vBrowseInfo, SizeOf(vBrowseInfo), 0);
    vBrowseInfo.pidlRoot := PItemIDList(ARootItemID);
    vBrowseInfo.hwndOwner := AOwnerWindow;
    vBrowseInfo.pszDisplayName := vBuffer;
    vBrowseInfo.lpszTitle := PChar(ACaption);
    vBrowseInfo.ulFlags := ABrowseFlags;
    vActiveWindow := GetActiveWindow;
    try
      vItemIDList := SHBrowseForFolder(vBrowseInfo);
      Result := vItemIDList <> nil;
      if Result then
      begin
        GetMem(vCopyBuffer, MAX_PATH * SizeOf(Char));
        try
          System.Move(vBuffer^, vCopyBuffer^, MAX_PATH * SizeOf(Char));
          if not SHGetPathFromIDList(vItemIDList, vBuffer) then
          begin
            FreeMem(vBuffer);
            vBuffer := vCopyBuffer;
            vCopyBuffer := nil;
          end;
        finally
          if vCopyBuffer <> nil then
            FreeMem(vCopyBuffer);
        end;
        CoTaskMemFree(vItemIDList);
        I := 0;
        while (I < MAX_PATH) and (vBuffer[I] = #0) do Inc(I);
        SetString(ADirectory, PChar(@vBuffer[I]), StrLen(PChar(@vBuffer[I])));
      end;
    finally
      SetActiveWindow(vActiveWindow);
    end;
  finally
    if vBuffer <> nil then
      FreeMem(vBuffer);
  end;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

function SelectDirectory(const ACaption: string; const ARoot: string;
  var ADirectory: string; AOwnerWindow: HWND;
  const ABrowseFlags: Cardinal = BIF_RETURNONLYFSDIRS): Boolean;
{$IFDEF SG_WIN_PLATFORM}
var
  vRootItemIDList: PItemIDList;
  vDesktopFolder: IShellFolder;
  vEaten, vFlags: {$IFDEF VER100} Integer; {$ELSE} Cardinal; {$ENDIF}
begin
  vRootItemIDList := nil;
  if ARoot <> '' then
  begin
    SHGetDesktopFolder(vDesktopFolder);
    vDesktopFolder.ParseDisplayName(0, nil, POleStr(WideString(ARoot)), vEaten,
      vRootItemIDList, vFlags);
  end;
  Result := SelectDirectory(ACaption, vRootItemIDList, ADirectory,
    AOwnerWindow, ABrowseFlags);
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

end.

unit sgEncMgr;
{$I SGDXF.inc}
{$IFNDEF SGDEL_6}
  {$L+}
{$ENDIF}

interface

uses
  sgEncoding;

function EncMgr: IEncodingManager;
function SetEncMgr(const AEncMgr: IEncodingManager): IEncodingManager;

implementation

{$DEFINE HAS_ENCODING_CODEPAGE_PROPERTY}
{$IFNDEF SGFPC}
  {$IFNDEF SGDEL_XE}
    {$UNDEF HAS_ENCODING_CODEPAGE_PROPERTY}
  {$ENDIF}
  {$IFNDEF SGDEL_XE2}
    {$DEFINE SG_FIRST_ENCODING_VERSIONS}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF SGFPC}LCLIntf, LCLType, {$ENDIF}
  {$IFDEF SG_FIREMONKEY}sgFMXTypes, {$ENDIF}
  {$IFDEF SGDEL_6}Types, {$ENDIF}
  SysUtils;

{$IFDEF MSWINDOWS}
const
  TCI_SRCLOCALE = $1000;
  {$EXTERNALSYM TCI_SRCLOCALE}
{$IFDEF SGFPC}
  {$DEFINE SG_PURE_WINDEFS}
{$ELSE}
  {$IFNDEF SGDEL_7}
    {$DEFINE SG_PURE_WINDEFS}
  {$ENDIF}
{$ENDIF}
{$IFDEF SG_PURE_WINDEFS}
  CP_THREAD_ACP = 3;
  {$EXTERNALSYM CP_THREAD_ACP}
  CP_SYMBOL = 42;
  {$EXTERNALSYM CP_SYMBOL}
{$ENDIF}
{$ENDIF}

type
  TsgLngID = type Integer;
  TsgLngIDs = array of TsgLngID;
{$IFNDEF SGDEL_6}
  PByte = ^Byte;
{$ENDIF}

  TsgLngIDHelper = class
  public
    class function FromLANGID(ALangID: Word): TsgLngID;
    class function FromCharset(ACharset: Byte): TsgLngID;
    class function FromCodepage(ACodepage: Cardinal): TsgLngID;
    class function FromChar(AChar: Cardinal): TsgLngID;
    class function ToLANGID(ALngID: TsgLngID): Word;
    class function ToCharset(ALngID: TsgLngID): Byte;
    class function ToCodepage(ALngID: TsgLngID): Cardinal;
    class function IsEqual(ALngID1, ALngID2: TsgLngID): Boolean;
    class function QueryLngIDs(ALngID: TsgLngID; var ALngIDs: TsgLngIDs): Boolean;
  end;

  TEncodingManager = class(TInterfacedObject, IEncodingManager)
  protected
    { IEncodingManager }
    function FromCP(ACodepage: Word): IEncoding;
    function FromLANGID(ALANGID: Word): IEncoding;
    function FromCharset(ACharset: Byte): IEncoding;
    function FromChar(AChar: Word): IEncoding;

    function GetEncoding(ALngID: TsgLngID): IEncoding;
  end;

  { TEncodingImpl }

  TEncodingImpl = class(TInterfacedObject, IEncoding)
  private
{$IFDEF HAS_FEATURE_ENCODING}
    FEncoding: TEncoding;
{$IFNDEF HAS_ENCODING_CODEPAGE_PROPERTY}
    FLngID: TsgLngID;
{$ENDIF}
{$ELSE}
    FLngID: TsgLngID;
    FMaxCharSize: Integer;
{$ENDIF}
{$IFNDEF HAS_FEATURE_ENCODING}
  protected
    function GetByteCount(Chars: PWideChar; CharCount: Integer): Integer; overload; virtual;{$IFDEF SGDEL_7}abstract;{$ENDIF}
    function GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; virtual;{$IFDEF SGDEL_7}abstract;{$ENDIF}
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; virtual;{$IFDEF SGDEL_7}abstract;{$ENDIF}
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer; overload; virtual;{$IFDEF SGDEL_7}abstract;{$ENDIF}
{$ENDIF}
  protected
    { IEncoding }
    function GetByteCount(const Chars: TUnicodeCharArray): Integer; overload;
    function GetByteCount(const Chars: TUnicodeCharArray; CharIndex, CharCount: Integer): Integer; overload;
    function GetByteCount(const S: UnicodeString): Integer; overload;
    function GetByteCount(const S: UnicodeString; CharIndex, CharCount: Integer): Integer; overload;
    function GetBytes(const Chars: TUnicodeCharArray): TBytes; overload;
    function GetBytes(const Chars: TUnicodeCharArray; CharIndex, CharCount: Integer): TBytes; overload;
    function GetBytes(const Chars: TUnicodeCharArray; CharIndex, CharCount: Integer;
      const Bytes: TBytes; ByteIndex: Integer): Integer; overload;
    function GetBytes(const S: UnicodeString): TBytes; overload;
    function GetBytes(const S: UnicodeString; CharIndex, CharCount: Integer;
      const Bytes: TBytes; ByteIndex: Integer): Integer; overload;
    function GetCharCount(const Bytes: TBytes): Integer; overload;
    function GetCharCount(const Bytes: TBytes; ByteIndex, ByteCount: Integer): Integer; overload;
    function GetChars(const Bytes: TBytes): TUnicodeCharArray; overload;
    function GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer): TUnicodeCharArray; overload;
    function GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer;
      const Chars: TUnicodeCharArray; CharIndex: Integer): Integer; overload;
    function GetMaxByteCount(CharCount: Integer): Integer; virtual;
    function GetMaxCharCount(ByteCount: Integer): Integer; virtual;
    function GetPreamble: TBytes; virtual;
    function GetString(const Bytes: TBytes): UnicodeString; overload;
    function GetString(const Bytes: TBytes; ByteIndex, ByteCount: Integer): UnicodeString; overload;
    function GetCodepage: Cardinal; virtual;

    function GetCharset: Byte;
    function GetLANGID: Word;
    function IsEqual(const AEncoding: IEncoding): Boolean;
    function GetIsSingleByte: Boolean;
  public
    constructor Create(const ALngID: TsgLngID); virtual;
    destructor Destroy; override;
{$IFDEF HAS_FEATURE_ENCODING}
    property Encoding: TEncoding read FEncoding;
{$ELSE}
    property LngID: TsgLngID read FLngID;
{$ENDIF}
  end;
{$IFNDEF HAS_FEATURE_ENCODING}
  TMBCSEncodingImpl = class(TEncodingImpl)
  protected
    function GetByteCount(Chars: PWideChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer; overload; override;
  end;
  TUTF7EncodingImpl = class(TMBCSEncodingImpl)
  public
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
  end;
  TUTF8EncodingImpl = class(TUTF7EncodingImpl)
  public
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;
  TUnicodeEncodingImpl = class(TEncodingImpl)
  protected
    function GetByteCount(Chars: PWideChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer; overload; override;
    //function GetCodePage: Cardinal; override;
    //function GetEncodingName: string; override;
  public
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;
  TBigEndianUnicodeEncodingImpl = class(TUnicodeEncodingImpl)
  protected
    function GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer; overload; override;
    //function GetCodePage: Cardinal; override;
    //function GetEncodingName: string; override;
  public
    function GetPreamble: TBytes; override;
  end;
{$ENDIF}

var
  _EncodingManager: IEncodingManager;

function MAKELANGID(p, s: WORD): WORD;{$IFDEF SGDEL_7}{$IFDEF SG_INLINE} inline;{$ENDIF}{$ENDIF}
begin
  Result := WORD(s shl 10) or p;
end;

function EncMgr: IEncodingManager;
begin
  if _EncodingManager = nil then
    _EncodingManager := TEncodingManager.Create;
  Result := _EncodingManager;
end;

function SetEncMgr(const AEncMgr: IEncodingManager): IEncodingManager;
begin
  Result := _EncodingManager;
  _EncodingManager := AEncMgr;
end;

function _lcid_to_cp(lcid: Cardinal): Cardinal;
{$IFDEF MSWINDOWS}
var
  vcsi: TCharsetInfo;
{$ENDIF}
begin
  Result := CP_ACP;
{$IFDEF MSWINDOWS}
  if TranslateCharsetInfo(lcid, vcsi, TCI_SRCLOCALE) then
    Result := vcsi.ciACP;
{$ENDIF}
end;

function GetLangIDByCharset(const ACharset: Byte): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := SysLocale.DefaultLCID;
  case ACharset of
    ANSI_CHARSET:
      Result := MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT);
    DEFAULT_CHARSET:
      Result := SysLocale.DefaultLCID; //(LANG_ENGLISH, SUBLANG_DEFAULT);
    SYMBOL_CHARSET:
      Result := SysLocale.DefaultLCID;
    MAC_CHARSET:
      Result := SysLocale.DefaultLCID;
    SHIFTJIS_CHARSET:
      Result := MAKELANGID(LANG_JAPANESE, SUBLANG_DEFAULT);
    HANGEUL_CHARSET:
      Result := MAKELANGID(LANG_KOREAN, SUBLANG_DEFAULT);
    JOHAB_CHARSET:
      Result := MAKELANGID(LANG_KOREAN, SUBLANG_DEFAULT);
    GB2312_CHARSET:
      Result := MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED);
    CHINESEBIG5_CHARSET:
      Result := MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_TRADITIONAL);
    GREEK_CHARSET:
      Result := MAKELANGID(LANG_GREEK, SUBLANG_DEFAULT);
    TURKISH_CHARSET:
      Result := MAKELANGID(LANG_TURKISH, SUBLANG_DEFAULT);
    HEBREW_CHARSET:
      Result := MAKELANGID(LANG_HEBREW, SUBLANG_DEFAULT);
    ARABIC_CHARSET:
      Result := MAKELANGID(LANG_ARABIC, SUBLANG_DEFAULT);
    BALTIC_CHARSET:
      Result := MAKELANGID(LANG_LATVIAN, SUBLANG_DEFAULT);
    RUSSIAN_CHARSET:
      Result := MAKELANGID(LANG_RUSSIAN, SUBLANG_DEFAULT);
    THAI_CHARSET:
      Result := MAKELANGID(LANG_THAI, SUBLANG_DEFAULT);
    EASTEUROPE_CHARSET:
      Result := MAKELANGID(LANG_CZECH, SUBLANG_DEFAULT);
    OEM_CHARSET:
      Result := SysLocale.DefaultLCID;
  end; { case ACharset }
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function CodePageToCharset(ACP: Cardinal): Byte;
{$IFDEF MSWINDOWS}
var
  vCharsetInfo: TCharsetInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if TranslateCharsetInfo(ACP, vCharsetInfo, TCI_SRCCODEPAGE) then
    Result := vCharsetInfo.ciCharset
  else
{$ENDIF}
  begin
  Result := DEFAULT_CHARSET;
  case ACP of
    {$IFDEF MSWINDOWS}CP_THREAD_ACP{$ELSE}3{$ENDIF}: Result := DEFAULT_CHARSET;
    {$IFDEF MSWINDOWS}CP_OEMCP{$ELSE}1{$ENDIF}: Result := OEM_CHARSET;
    {$IFDEF MSWINDOWS}CP_MACCP{$ELSE}2{$ENDIF}: Result := MAC_CHARSET;
    {$IFDEF MSWINDOWS}CP_SYMBOL{$ELSE}42{$ENDIF}: Result := SYMBOL_CHARSET;
    37: Result := DEFAULT_CHARSET;        // IBM EBCDIC - U.S./Canada
    437: Result := OEM_CHARSET;           // OEM - United States
    500: Result := DEFAULT_CHARSET;       // IBM EBCDIC - International
    708: Result := ARABIC_CHARSET;        // Arabic - ASMO 708
    709: Result := ARABIC_CHARSET;        // Arabic - ASMO 449+, BCON V4
    710: Result := ARABIC_CHARSET;        // Arabic - Transparent Arabic
    720: Result := ARABIC_CHARSET;        // Arabic - Transparent ASMO
    737: Result := OEM_CHARSET;           // OEM - Greek (formerly 437G)
    775: Result := OEM_CHARSET;           // OEM - Baltic
    850: Result := OEM_CHARSET;           // OEM - Multilingual Latin I
    852: Result := OEM_CHARSET;           // OEM - Latin II
    855: Result := OEM_CHARSET;           // OEM - Cyrillic (primarily Russian)
    857: Result := OEM_CHARSET;           // OEM - Turkish
    858: Result := OEM_CHARSET;           // OEM - Multlingual Latin I + Euro symbol
    860: Result := OEM_CHARSET;           // OEM - Portuguese
    861: Result := OEM_CHARSET;           // OEM - Icelandic
    862: Result := OEM_CHARSET;           // OEM - Hebrew
    863: Result := OEM_CHARSET;           // OEM - Canadian-French
    864: Result := OEM_CHARSET;           // OEM - Arabic
    865: Result := OEM_CHARSET;           // OEM - Nordic
    866: Result := OEM_CHARSET;           // OEM - Russian
    869: Result := OEM_CHARSET;           // OEM - Modern Greek
    870: Result := DEFAULT_CHARSET;       // IBM EBCDIC - Multilingual/ROECE (Latin-2)
    874: Result := THAI_CHARSET;          // ANSI/OEM - Thai (same as 28605, ISO 8859-15)
    875: Result := GREEK_CHARSET;         // IBM EBCDIC - Modern Greek
    932: Result := SHIFTJIS_CHARSET;      // ANSI/OEM - Japanese, Shift-JIS
    936: Result := GB2312_CHARSET;        // ANSI/OEM - Simplified Chinese (PRC, Singapore)
    949: Result := HANGEUL_CHARSET;       //JOHAB_CHARSET;// ANSI/OEM - Korean (Unified Hangeul Code)
    950: Result := CHINESEBIG5_CHARSET;   // ANSI/OEM - Traditional Chinese (Taiwan; Hong Kong SAR, PRC)
    1026: Result := TURKISH_CHARSET;      // IBM EBCDIC - Turkish (Latin-5)
    1047: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Latin 1/Open System
    1140: Result := DEFAULT_CHARSET;      // IBM EBCDIC - U.S./Canada (037 + Euro symbol)
    1141: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Germany (20273 + Euro symbol)
    1142: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Denmark/Norway (20277 + Euro symbol)
    1143: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Finland/Sweden (20278 + Euro symbol)
    1144: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Italy (20280 + Euro symbol)
    1145: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Latin America/Spain (20284 + Euro symbol)
    1146: Result := DEFAULT_CHARSET;      // IBM EBCDIC - United Kingdom (20285 + Euro symbol)
    1147: Result := DEFAULT_CHARSET;      // IBM EBCDIC - France (20297 + Euro symbol)
    1148: Result := DEFAULT_CHARSET;      // IBM EBCDIC - International (500 + Euro symbol)
    1149: Result := DEFAULT_CHARSET;      // IBM EBCDIC - Icelandic (20871 + Euro symbol)
    1200: Result := DEFAULT_CHARSET;      // Unicode UCS-2 Little-Endian (BMP of ISO 10646)
    1201: Result := DEFAULT_CHARSET;      // Unicode UCS-2 Big-Endian
    1250: Result := EASTEUROPE_CHARSET;   // ANSI - Central European
    1251: Result := RUSSIAN_CHARSET;      // ANSI - Cyrillic
    1252: Result := ANSI_CHARSET;         // ANSI - Latin I
    1253: Result := GREEK_CHARSET;        // ANSI - Greek
    1254: Result := TURKISH_CHARSET;      // ANSI - Turkish
    1255: Result := HEBREW_CHARSET;       // ANSI - Hebrew
    1256: Result := ARABIC_CHARSET;       // ANSI - Arabic
    1257: Result := DEFAULT_CHARSET;      // ANSI - Baltic
    1258: Result := DEFAULT_CHARSET;      // ANSI/OEM - Vietnamese
    1361: Result := JOHAB_CHARSET;        // Korean (Johab)
    10000: Result := DEFAULT_CHARSET;     // MAC - Roman
    10001: Result := SHIFTJIS_CHARSET;    // MAC - Japanese
    10002: Result := CHINESEBIG5_CHARSET; // MAC - Traditional Chinese (Big5)
    10003: Result := JOHAB_CHARSET;       // MAC - Korean
    10004: Result := ARABIC_CHARSET;      // MAC - Arabic
    10005: Result := HEBREW_CHARSET;      // MAC - Hebrew
    10006: Result := GREEK_CHARSET;       // MAC - Greek I
    10007: Result := RUSSIAN_CHARSET;     // MAC - Cyrillic
    10008: Result := GB2312_CHARSET;      // MAC - Simplified Chinese (GB 2312)
    10010: Result := DEFAULT_CHARSET;     // MAC - Romania
    10017: Result := EASTEUROPE_CHARSET;  // MAC - Ukraine
    10021: Result := THAI_CHARSET;        // MAC - Thai
    10029: Result := MAC_CHARSET;         // MAC - Latin II
    10079: Result := MAC_CHARSET;         // MAC - Icelandic
    10081: Result := TURKISH_CHARSET;     // MAC - Turkish
    10082: Result := MAC_CHARSET;         // MAC - Croatia
    12000: Result := DEFAULT_CHARSET;     // Unicode UCS-4 Little-Endian
    12001: Result := DEFAULT_CHARSET;     // Unicode UCS-4 Big-Endian
    20000: Result := DEFAULT_CHARSET;     // CNS - Taiwan
    20001: Result := DEFAULT_CHARSET;     // TCA - Taiwan
    20002: Result := DEFAULT_CHARSET;     // Eten - Taiwan
    20003: Result := DEFAULT_CHARSET;     // IBM5550 - Taiwan
    20004: Result := DEFAULT_CHARSET;     // TeleText - Taiwan
    20005: Result := DEFAULT_CHARSET;     // Wang - Taiwan
    20105: Result := DEFAULT_CHARSET;     // IA5 IRV International Alphabet No. 5 (7-bit)
    20106: Result := DEFAULT_CHARSET;     // IA5 German (7-bit)
    20107: Result := DEFAULT_CHARSET;     // IA5 Swedish (7-bit)
    20108: Result := DEFAULT_CHARSET;     // IA5 Norwegian (7-bit)
    20127: Result := DEFAULT_CHARSET;     // US-ASCII (7-bit)
    20261: Result := DEFAULT_CHARSET;     // T.61
    20269: Result := DEFAULT_CHARSET;     // ISO 6937 Non-Spacing Accent
    20273: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Germany
    20277: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Denmark/Norway
    20278: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Finland/Sweden
    20280: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Italy
    20284: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Latin America/Spain
    20285: Result := DEFAULT_CHARSET;     // IBM EBCDIC - United Kingdom
    20290: Result := SHIFTJIS_CHARSET;    // IBM EBCDIC - Japanese Katakana Extended
    20297: Result := DEFAULT_CHARSET;     // IBM EBCDIC - France
    20420: Result := ARABIC_CHARSET;      // IBM EBCDIC - Arabic
    20423: Result := GREEK_CHARSET;       // IBM EBCDIC - Greek
    20424: Result := HEBREW_CHARSET;      // IBM EBCDIC - Hebrew
    20833: Result := JOHAB_CHARSET;       // IBM EBCDIC - Korean Extended
    20838: Result := THAI_CHARSET;        // IBM EBCDIC - Thai
    20866: Result := RUSSIAN_CHARSET;     // Russian - KOI8-R
    20871: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Icelandic
    20880: Result := RUSSIAN_CHARSET;     // IBM EBCDIC - Cyrillic (Russian)
    20905: Result := TURKISH_CHARSET;     // IBM EBCDIC - Turkish
    20924: Result := DEFAULT_CHARSET;     // IBM EBCDIC - Latin-1/Open System (1047 + Euro symbol)
    20932: Result := DEFAULT_CHARSET;     // JIS X 0208-1990 & 0121-1990
    20936: Result := GB2312_CHARSET;      // Simplified Chinese (GB2312)
    21025: Result := EASTEUROPE_CHARSET;  // IBM EBCDIC - Cyrillic (Serbian, Bulgarian)
    21027: Result := DEFAULT_CHARSET;     // Extended Alpha Lowercase
    21866: Result := EASTEUROPE_CHARSET;  // Ukrainian (KOI8-U)
    28591: Result := DEFAULT_CHARSET;     // ISO 8859-1 Latin I
    28592: Result := DEFAULT_CHARSET;     // ISO 8859-2 Central Europe
    28593: Result := DEFAULT_CHARSET;     // ISO 8859-3 Latin 3
    28594: Result := DEFAULT_CHARSET;     // ISO 8859-4 Baltic
    28595: Result := RUSSIAN_CHARSET;     // ISO 8859-5 Cyrillic
    28596: Result := ARABIC_CHARSET;      // ISO 8859-6 Arabic
    28597: Result := GREEK_CHARSET;       // ISO 8859-7 Greek
    28598: Result := HEBREW_CHARSET;      // ISO 8859-8 Hebrew
    28599: Result := DEFAULT_CHARSET;     // ISO 8859-9 Latin 5
    28605: Result := DEFAULT_CHARSET;     // ISO 8859-15 Latin 9
    29001: Result := DEFAULT_CHARSET;     // Europa 3
    38598: Result := HEBREW_CHARSET;      // ISO 8859-8 Hebrew
    50220: Result := SHIFTJIS_CHARSET;    // ISO 2022 Japanese with no halfwidth Katakana
    50221: Result := SHIFTJIS_CHARSET;    // ISO 2022 Japanese with halfwidth Katakana
    50222: Result := SHIFTJIS_CHARSET;    // ISO 2022 Japanese JIS X 0201-1989
    50225: Result := JOHAB_CHARSET;       // ISO 2022 Korean
    50227: Result := GB2312_CHARSET;      // ISO 2022 Simplified Chinese
    50229: Result := CHINESEBIG5_CHARSET; // ISO 2022 Traditional Chinese
    50930: Result := SHIFTJIS_CHARSET;    // Japanese (Katakana) Extended
    50931: Result := DEFAULT_CHARSET;     // US/Canada and Japanese
    50933: Result := JOHAB_CHARSET;       // Korean Extended and Korean
    50935: Result := GB2312_CHARSET;      // Simplified Chinese Extended and Simplified Chinese
    50936: Result := GB2312_CHARSET;      // Simplified Chinese
    50937: Result := DEFAULT_CHARSET;     // US/Canada and Traditional Chinese
    50939: Result := SHIFTJIS_CHARSET;    // Japanese (Latin) Extended and Japanese
    51932: Result := SHIFTJIS_CHARSET;    // EUC - Japanese
    51936: Result := GB2312_CHARSET;      // EUC - Simplified Chinese
    51949: Result := JOHAB_CHARSET;       // EUC - Korean
    51950: Result := CHINESEBIG5_CHARSET; // EUC - Traditional Chinese
    52936: Result := GB2312_CHARSET;      // HZ-GB2312 Simplified Chinese
    54936: Result := GB2312_CHARSET;      // Windows XP: GB18030 Simplified Chinese (4 Byte)
    57002: Result := DEFAULT_CHARSET;     // ISCII Devanagari
    57003: Result := DEFAULT_CHARSET;     // ISCII Bengali
    57004: Result := DEFAULT_CHARSET;     // ISCII Tamil
    57005: Result := DEFAULT_CHARSET;     // ISCII Telugu
    57006: Result := DEFAULT_CHARSET;     // ISCII Assamese
    57007: Result := DEFAULT_CHARSET;     // ISCII Oriya
    57008: Result := DEFAULT_CHARSET;     // ISCII Kannada
    57009: Result := DEFAULT_CHARSET;     // ISCII Malayalam
    57010: Result := DEFAULT_CHARSET;     // ISCII Gujarati
    57011: Result := DEFAULT_CHARSET;     // ISCII Punjabi
    65000: Result := DEFAULT_CHARSET;     // Unicode UTF-7
    65001: Result := DEFAULT_CHARSET;     // Unicode UTF-8
  end { case }
  end;
end;

function GetCodepageFromWideChar(AChar: Cardinal): Cardinal;

  function ValInSet(const AValue: Cardinal;
    const ALoLim, AHiLim: Cardinal): Boolean;
  begin
    Result := (AValue >= ALoLim) and (AValue <= AHiLim);
  end;

begin
  Result := CP_ACP;
  if
     ValInSet(AChar, $0000, $007F) or // Basic Latin
     ValInSet(AChar, $0080, $00FF) // Latin-1 Supplement
     //ValInSet(AChar, $0100, $017F) or // Latin Extended-A
     //ValInSet(AChar, $0180, $024F) or // Latin Extended-B
     //ValInSet(AChar, $0250, $02AF) or // IPA Extensions
     //ValInSet(AChar, $1D00, $1D7F) or // Phonetic Extensions
     //ValInSet(AChar, $1D80, $1DBF) or // Phonetic Extensions Supplement
     //ValInSet(AChar, $1E00, $1EFF) or // Latin Extended Additional
     //ValInSet(AChar, $2100, $214F) or // Letterlike Symbols
     //ValInSet(AChar, $2460, $24FF) or // Enclosed Alphanumerics
     //ValInSet(AChar, $2C60, $2C7F) or // Latin Extended-C
     //ValInSet(AChar, $A720, $A7FF) or // Latin Extended-D
     //ValInSet(AChar, $FB00, $FB4F) or // Alphabetic Presentation Forms
     //ValInSet(AChar, $FF00, $FFEF) or // Halfwidth and Fullwidth Forms
     //ValInSet(AChar, $1D400, $1D7FF) // Mathematical Alphanumeric Symbols
     then
    Result := 1252
  else
  if ValInSet(AChar, $0590, $05FF) or ValInSet(AChar, $FB1D, $FB40) then
    Result := 1255 // Hebrew
  else
    if ValInSet(AChar, $0400, $04FF) or ValInSet(AChar, $0500, $052F) or
       ValInSet(AChar, $2DE0, $2DFF) or ValInSet(AChar, $A640, $A69F) then
      Result := 1251 // Cyrilic
  else
    if ValInSet(AChar, $0600, $06FF) or ValInSet(AChar, $0750, $077F) or
       ValInSet(AChar, $FB50, $FE00) or ValInSet(AChar, $FE70, $FEFF) then
      Result := 1256 // Arabic
  else
    if ValInSet(AChar, $0900, $097F) then
      Result := 8534 // Davangary
    else
    if ValInSet(AChar, $4E00, $9FA5) or ValInSet(AChar, $20000, $2A6D6) then
      Result := 936 // Chinese
    else
    if ValInSet(AChar, $EEB8, $F848) or ValInSet(AChar, $E000, $EEB7) then
      Result := 950 // big5
  else
    //Harigana (3040—309F) //Katakana (30A0—30FF)
    if ValInSet(AChar, $3040, $30FF) then
      Result := 932 // Japanese
  else
    if ValInSet(AChar, $0370, $03FF) then
      Result := 1253 // Greek
  else
    if ValInSet(AChar, $1100, $11FF) or ValInSet(AChar, $AC00, $D7AF) then
      Result := 949 // ANSI/OEM - Korean (Unified Hangeul Code) //Hang
  else
    if ValInSet(AChar, $0E00, $0E7F) then
      Result := 874 // ANSI/OEM - Thai (same as 28605, ISO 8859-15)
end;

function GetCharsetFromLang(const ALangID: Word): Byte;
{$IFDEF MSWINDOWS}
var
  vLangID: Cardinal;
  vCharsetInfo: TCharsetInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  vLangID := ALangID;
  if TranslateCharsetInfo(vLangID, vCharsetInfo, TCI_SRCLOCALE) then//win2000+
    Result := vCharsetInfo.ciCharset
  else
  begin
  Result := ANSI_CHARSET;
  case Lo(ALangID) of
    LANG_JAPANESE: Result := SHIFTJIS_CHARSET;
    LANG_KOREAN: Result := HANGEUL_CHARSET;
    LANG_CHINESE:
      case ALangID shr 10 of
        SUBLANG_CHINESE_SIMPLIFIED: Result := GB2312_CHARSET;
        SUBLANG_CHINESE_TRADITIONAL: Result := CHINESEBIG5_CHARSET;
      end; { case Lo(ALangID) }
    LANG_GREEK: Result := GREEK_CHARSET;
    LANG_TURKISH: Result := TURKISH_CHARSET;
    LANG_HEBREW: Result := HEBREW_CHARSET;
    LANG_ARABIC: Result := ARABIC_CHARSET;
    LANG_ESTONIAN, LANG_LATVIAN, LANG_LITHUANIAN: Result := BALTIC_CHARSET;
    LANG_THAI: Result := THAI_CHARSET;
    LANG_CZECH, LANG_HUNGARIAN, LANG_POLISH, LANG_CROATIAN, //LANG_MACEDONIAN,
    LANG_ROMANIAN, LANG_SLOVAK, LANG_SLOVENIAN, LANG_GERMAN,
      LANG_FRENCH: Result := EASTEUROPE_CHARSET;
    LANG_RUSSIAN, LANG_BELARUSIAN, LANG_BULGARIAN, LANG_UKRAINIAN:
      Result := RUSSIAN_CHARSET;
    LANG_ENGLISH: Result := ANSI_CHARSET;
  end; { case Lo(ALangID) }
  end;
{$ELSE}
  Result := DEFAULT_CHARSET;
{$ENDIF}
end;

{ TsgLngIDHelper }

class function TsgLngIDHelper.FromLANGID(ALangID: Word): TsgLngID;
begin
  Result := ALangID;
end;

class function TsgLngIDHelper.IsEqual(ALngID1, ALngID2: TsgLngID): Boolean;
begin
  if ALngID1 * ALngID2 < 0 then
    Result := ToCodepage(ALngID1) = ToCodepage(ALngID2)
  else
    Result := ALngID1 = ALngID2;
end;

class function TsgLngIDHelper.QueryLngIDs(ALngID: TsgLngID;
  var ALngIDs: TsgLngIDs): Boolean;
{$IFNDEF SGFPC}
var
  I: Integer;
  Langs: TLanguages;
  CurrCP, CP: Cardinal;
{$ENDIF}
begin
{$IFNDEF SGFPC}
  CurrCP := ToCodepage(ALngID);
  Langs := Languages;
  for I := 0 to Langs.Count - 1 do
  begin
    CP := {$IFDEF MSWINDOWS}_lcid_to_cp(Langs.LocaleID[I]){$ELSE}0{$ENDIF};
    if CP = CurrCP then
    begin
      SetLength(ALngIDs, Length(ALngIDs) + 1);
      ALngIDs[High(ALngIDs)] := TsgLngID(Langs.LocaleID[I]);
    end;
  end;
  Result := Length(ALngIDs) > 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

class function TsgLngIDHelper.FromChar(AChar: Cardinal): TsgLngID;
begin
  Result := FromCodepage(GetCodepageFromWideChar(AChar));
end;

class function TsgLngIDHelper.FromCharset(ACharset: Byte): TsgLngID;
var
  vCP: Word;
{$IFDEF MSWINDOWS}
  vCharset: Cardinal;
  vCharsetInfo: TCharsetInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  vCharset := ACharset;
  if TranslateCharsetInfo(vCharset, vCharsetInfo, TCI_SRCCHARSET) then
    vCP := vCharsetInfo.ciACP
  else
{$ENDIF}
  case ACharset of
    SYMBOL_CHARSET:      vCP := {$IFDEF MSWINDOWS}CP_SYMBOL{$ELSE}42{$ENDIF};
    SHIFTJIS_CHARSET:    vCP := 932;
    HANGEUL_CHARSET:     vCP := 949;
    GB2312_CHARSET:      vCP := 936;
    CHINESEBIG5_CHARSET: vCP := 950;
    OEM_CHARSET:         vCP := {$IFDEF MSWINDOWS}CP_OEMCP{$ELSE}1{$ENDIF};
    JOHAB_CHARSET:       vCP := 1361;
    HEBREW_CHARSET:      vCP := 1255;
    ARABIC_CHARSET:      vCP := 1256;
    GREEK_CHARSET:       vCP := 1253;
    TURKISH_CHARSET:     vCP := 1254;
    VIETNAMESE_CHARSET:  vCP := 1258;
    THAI_CHARSET:        vCP := 874;
    EASTEUROPE_CHARSET:  vCP := 1250;
    RUSSIAN_CHARSET:     vCP := 1251;
    MAC_CHARSET:         vCP := {$IFDEF MSWINDOWS}CP_MACCP{$ELSE}2{$ENDIF};
    BALTIC_CHARSET:      vCP := 1257;
    DEFAULT_CHARSET:     vCP := {$IFDEF MSWINDOWS}CP_THREAD_ACP{$ELSE}3{$ENDIF};
  else
    vCP := CP_ACP;
  end;
  Result := -Integer(vCP);
end;

class function TsgLngIDHelper.FromCodepage(ACodepage: Cardinal): TsgLngID;
begin
  Result := -Integer(ACodePage);
end;

class function TsgLngIDHelper.ToLANGID(ALngID: TsgLngID): Word;
begin
  if ALngID < 0 then
    Result := GetLangIDByCharset(CodePageToCharset(-ALngID))
  else
    Result := ALngID;
end;

class function TsgLngIDHelper.ToCharset(ALngID: TsgLngID): Byte;
begin
  if ALngID < 0 then
    Result := CodePageToCharset(-ALngID)
  else
    if ALngID = 0 then
      Result := ANSI_CHARSET
    else
      Result := CodePageToCharset(_lcid_to_cp(ALngID));
end;

class function TsgLngIDHelper.ToCodepage(ALngID: TsgLngID): Cardinal;
begin
  if ALngID < 0 then
    Result := -ALngID
  else
    if ALngID = 0 then
      Result := CP_ACP
    else
      Result := _lcid_to_cp(ALngID);
end;

{ TEncodingManager }

function TEncodingManager.FromChar(AChar: Word): IEncoding;
begin
  Result := GetEncoding(TsgLngIDHelper.FromChar(AChar));
end;

function TEncodingManager.FromCharset(ACharset: Byte): IEncoding;
begin
  Result := GetEncoding(TsgLngIDHelper.FromCharset(ACharset));
end;

function TEncodingManager.FromCP(ACodepage: Word): IEncoding;
begin
  Result := GetEncoding(TsgLngIDHelper.FromCodepage(ACodepage));
end;

function TEncodingManager.FromLANGID(ALANGID: Word): IEncoding;
begin
  Result := GetEncoding(TsgLngIDHelper.FromLANGID(ALANGID));
end;

function TEncodingManager.GetEncoding(ALngID: TsgLngID): IEncoding;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := TEncodingImpl.Create(ALngID);
{$ELSE}
  case TsgLngIDHelper.ToCodepage(ALngID) of
    1200: Result := TUnicodeEncodingImpl.Create(ALngID);
    1201: Result := TBigEndianUnicodeEncodingImpl.Create(ALngID);
    CP_UTF7: Result := TUTF7EncodingImpl.Create(ALngID);
    CP_UTF8: Result := TUTF8EncodingImpl.Create(ALngID);
  else
    Result := TMBCSEncodingImpl.Create(ALngID);
  end;
{$ENDIF}
end;

{ TEncodingImpl }

constructor TEncodingImpl.Create(const ALngID: TsgLngID);
var
{$IFNDEF HAS_FEATURE_ENCODING}
  CPInfo: TCPInfo;
{$ELSE}
  CP: Cardinal;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  CP := TsgLngIDHelper.ToCodepage(ALngID);
  if CP = {$IFDEF MSWINDOWS}CP_SYMBOL{$ELSE}42{$ENDIF} then
    FEncoding := TEncoding.Default
  else
    FEncoding := TEncoding.GetEncoding(CP);
{$IFNDEF HAS_ENCODING_CODEPAGE_PROPERTY}
  FLngID := ALngID;
{$ENDIF}
{$ELSE}
  FLngID := ALngID;
  FMaxCharSize := 1;
  if GetCPInfo(TsgLngIDHelper.ToCodepage(FLngID), CPInfo) then
    FMaxCharSize := CPInfo.MaxCharSize;
{$ENDIF}
end;

destructor TEncodingImpl.Destroy;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
{$ENDIF}
  inherited Destroy;
end;

function TEncodingImpl.GetCharset: Byte;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := TsgLngIDHelper.ToCharset({$IFDEF HAS_ENCODING_CODEPAGE_PROPERTY}TsgLngIDHelper.FromCodepage(FEncoding.CodePage){$ELSE}FLngID{$ENDIF});
{$ELSE}
  Result := TsgLngIDHelper.ToCharset(FLngID);
{$ENDIF}
end;

function TEncodingImpl.GetIsSingleByte: Boolean;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.IsSingleByte;
{$ELSE}
  Result := FMaxCharSize = 1;
{$ENDIF}
end;

function TEncodingImpl.GetLANGID: Word;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := TsgLngIDHelper.ToLANGID({$IFDEF HAS_ENCODING_CODEPAGE_PROPERTY}TsgLngIDHelper.FromCodepage(FEncoding.CodePage){$ELSE}FLngID{$ENDIF});
{$ELSE}
  Result := TsgLngIDHelper.ToLANGID(FLngID);
{$ENDIF}
end;

function TEncodingImpl.IsEqual(const AEncoding: IEncoding): Boolean;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := {$IFDEF HAS_ENCODING_CODEPAGE_PROPERTY}FEncoding.CodePage{$ELSE}TsgLngIDHelper.ToCodepage(FLngID){$ENDIF} = AEncoding.Codepage;
{$ELSE}
  Result := GetCodepage = AEncoding.Codepage;
{$ENDIF}
end;

function TEncodingImpl.GetByteCount(const Chars: TUnicodeCharArray): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetByteCount(Chars);
{$ELSE}
  Result := GetByteCount(PWideChar(Chars), Length(Chars));
{$ENDIF}
end;

function TEncodingImpl.GetByteCount(const Chars: TUnicodeCharArray; CharIndex,
  CharCount: Integer): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetByteCount(Chars, CharIndex, CharCount);
{$ELSE}
  Result := GetByteCount(PWideChar(@Chars[CharIndex]), CharCount);
{$ENDIF}
end;

function TEncodingImpl.GetByteCount(const S: UnicodeString): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetByteCount(S);
{$ELSE}
  Result := GetByteCount(PWideChar(S), Length(S));;
{$ENDIF}
end;

function TEncodingImpl.GetByteCount(const S: UnicodeString; CharIndex,
  CharCount: Integer): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetByteCount(S, CharIndex, CharCount);
{$ELSE}
  Result := GetByteCount(PWideChar(@S[CharIndex]), CharCount);
{$ENDIF}
end;

function TEncodingImpl.GetBytes(const Chars: TUnicodeCharArray): TBytes;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetBytes(Chars);
{$ELSE}
  Len := GetByteCount(Chars);
  SetLength(Result, Len);
  GetBytes(PWideChar(Chars), 0, Length(Chars), Result, 0);
{$ENDIF}
end;

function TEncodingImpl.GetBytes(const Chars: TUnicodeCharArray; CharIndex,
  CharCount: Integer): TBytes;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
  SetLength(Result, GetByteCount(Chars, CharIndex, CharCount));
  FEncoding.GetBytes(Chars, CharIndex, CharCount, Result, 0);
{$ELSE}
  Result := FEncoding.GetBytes(Chars, CharIndex, CharCount);
{$ENDIF}
{$ELSE}
  Len := GetByteCount(Chars, CharIndex, CharCount);
  SetLength(Result, Len);
  GetBytes(Chars, CharIndex, CharCount, Result, 0);
{$ENDIF}
end;

function TEncodingImpl.GetBytes(const Chars: TUnicodeCharArray; CharIndex,
  CharCount: Integer; const Bytes: TBytes; ByteIndex: Integer): Integer;
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
var
  Buffer: TBytes;
  Len: Integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
  Len := GetByteCount(Chars, CharIndex, CharCount);
  SetLength(Buffer, Len);
  FEncoding.GetBytes(Chars, CharIndex, CharCount, Buffer, 0);
  if Length(Bytes) - ByteIndex < Len then
    Len := Length(Bytes) - ByteIndex;
  System.Move(Buffer[0], Bytes[ByteIndex], Len);
  Result := Len;
{$ELSE}
  Result := FEncoding.GetBytes(Chars, CharIndex, CharCount, Bytes, ByteIndex);
{$ENDIF}
{$ELSE}
  Result := GetBytes(@Chars[CharIndex], CharCount, @Bytes[ByteIndex], Length(Bytes) - ByteIndex);
{$ENDIF}
end;

function TEncodingImpl.GetBytes(const S: UnicodeString): TBytes;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetBytes(S);
{$ELSE}
  Len := GetByteCount(S);
  SetLength(Result, Len);
  GetBytes(S, 1, Length(S), Result, 0);
{$ENDIF}
end;

function TEncodingImpl.GetBytes(const S: UnicodeString; CharIndex, CharCount: Integer;
  const Bytes: TBytes; ByteIndex: Integer): Integer;
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
var
  Buffer: TBytes;
  Len: Integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
  Len := GetByteCount(S, CharIndex, CharCount);
  SetLength(Buffer, Len);
  FEncoding.GetBytes(S, CharIndex, CharCount, Buffer, 0);
  if Length(Bytes) - ByteIndex < Len then
    Len := Length(Bytes) - ByteIndex;
  System.Move(Buffer[0], Bytes[ByteIndex], Len);
  Result := Len;
{$ELSE}
  Result := FEncoding.GetBytes(S, CharIndex, CharCount, Bytes, ByteIndex);
{$ENDIF}
{$ELSE}
  Result := GetBytes(@S[CharIndex], CharCount, @Bytes[ByteIndex], Length(Bytes) - ByteIndex);
{$ENDIF}
end;

function TEncodingImpl.GetCharCount(const Bytes: TBytes): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetCharCount(Bytes);
{$ELSE}
  Result := GetCharCount(Bytes, 0, Length(Bytes));
{$ENDIF}
end;

function TEncodingImpl.GetCharCount(const Bytes: TBytes; ByteIndex,
  ByteCount: Integer): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetCharCount(Bytes, ByteIndex, ByteCount);
{$ELSE}
  Result := GetCharCount(@Bytes[ByteIndex], ByteCount);
{$ENDIF}
end;

function TEncodingImpl.GetChars(const Bytes: TBytes): TUnicodeCharArray;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetChars(Bytes);
{$ELSE}
  Len := GetCharCount(Bytes, 0, Length(Bytes));
  SetLength(Result, Len);
  GetChars(@Bytes[0], Length(Bytes), PWideChar(Result), Len);
{$ENDIF}
end;

function TEncodingImpl.GetChars(const Bytes: TBytes; ByteIndex,
  ByteCount: Integer): TUnicodeCharArray;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetChars(Bytes, ByteIndex, ByteCount);
{$ELSE}
  Len := GetCharCount(Bytes, ByteIndex, ByteCount);
  SetLength(Result, Len);
  GetChars(@Bytes[ByteIndex], ByteCount, PWideChar(Result), Len);
{$ENDIF}
end;

function TEncodingImpl.GetChars(const Bytes: TBytes; ByteIndex,
  ByteCount: Integer; const Chars: TUnicodeCharArray; CharIndex: Integer): Integer;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  LCharCount: Integer;
{$ENDIF}
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
var
  CharsBuffer: TUnicodeCharArray;
  Len: Integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
{$IFDEF SG_FIRST_ENCODING_VERSIONS}
  Len := GetCharCount(Bytes, ByteIndex, ByteCount);
  SetLength(CharsBuffer, Len);
  FEncoding.GetChars(Bytes, ByteIndex, ByteCount, CharsBuffer, 0);
  if Length(Chars) - CharIndex < Len then
    Len := Length(Chars) - CharIndex;
  System.Move(CharsBuffer[0], Chars[CharIndex], Len * SizeOf(CharsBuffer[0]));
  Result := Len;
{$ELSE}
  Result := FEncoding.GetChars(Bytes, ByteIndex, ByteCount, Chars, CharIndex);
{$ENDIF}
{$ELSE}
  LCharCount := GetCharCount(Bytes, ByteIndex, ByteCount);
  Result := GetChars(@Bytes[ByteIndex], ByteCount, @Chars[CharIndex], LCharCount);
{$ENDIF}
end;

function TEncodingImpl.GetMaxByteCount(CharCount: Integer): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetMaxByteCount(CharCount);
{$ELSE}
  Result := (CharCount + 1) * FMaxCharSize;
{$ENDIF}
end;

function TEncodingImpl.GetMaxCharCount(ByteCount: Integer): Integer;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetMaxCharCount(ByteCount);
{$ELSE}
  Result := ByteCount;
{$ENDIF}
end;

function TEncodingImpl.GetPreamble: TBytes;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetPreamble;
{$ELSE}
  SetLength(Result, 0);
{$ENDIF}
end;

function TEncodingImpl.GetString(const Bytes: TBytes): UnicodeString;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetString(Bytes);
{$ELSE}
  Result := GetString(Bytes, 0, Length(Bytes));
{$ENDIF}
end;

function TEncodingImpl.GetString(const Bytes: TBytes; ByteIndex,
  ByteCount: Integer): UnicodeString;
{$IFNDEF HAS_FEATURE_ENCODING}
var
  Len: Integer;
{$ENDIF}
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := FEncoding.GetString(Bytes, ByteIndex, ByteCount);
{$ELSE}
  Result := '';
  Len := GetCharCount(Bytes, ByteIndex, ByteCount);
  if Len > 0 then
  begin
    SetLength(Result, Len);
    GetChars(@Bytes[ByteIndex], ByteCount, PWideChar(Result), Len);
  end;
{$ENDIF}
end;

function TEncodingImpl.GetCodepage: Cardinal;
begin
{$IFDEF HAS_FEATURE_ENCODING}
  Result := {$IFDEF HAS_ENCODING_CODEPAGE_PROPERTY}FEncoding.CodePage{$ELSE}TsgLngIDHelper.ToCodepage(FLngID){$ENDIF};
{$ELSE}
  Result := TsgLngIDHelper.ToCodepage(FLngID);
{$ENDIF}
end;

{$IFNDEF HAS_FEATURE_ENCODING}
{$IFNDEF SGDEL_7}
function TEncodingImpl.GetByteCount(Chars: PWideChar; CharCount: Integer): Integer;
begin
  Result := 0;
end;
function TEncodingImpl.GetBytes(Chars: PWideChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := 0;
end;
function TEncodingImpl.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := 0;
end;
function TEncodingImpl.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer;
begin
  Result := 0;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF HAS_FEATURE_ENCODING}
{ TMBCSEncodingImpl }

function TMBCSEncodingImpl.GetByteCount(Chars: PWideChar;
  CharCount: Integer): Integer;
begin
   Result := {$IFNDEF SGDEL_XE}WideCharToMultiByte{$ELSE}LocaleCharsFromUnicode{$ENDIF}(GetCodePage, 0{FWCharToMBFlags},
      PWideChar(Chars), CharCount, nil, 0, nil, nil);
end;

function TMBCSEncodingImpl.GetBytes(Chars: PWideChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := {$IFNDEF SGDEL_XE}WideCharToMultiByte{$ELSE}LocaleCharsFromUnicode{$ENDIF}(GetCodePage, 0{FWCharToMBFlags},
    PWideChar(Chars), CharCount, PAnsiChar(Bytes), ByteCount, nil, nil);
end;

function TMBCSEncodingImpl.GetCharCount(Bytes: PByte;
  ByteCount: Integer): Integer;
begin
  Result := {$IFNDEF SGDEL_XE}MultiByteToWideChar{$ELSE}UnicodeFromLocaleChars{$ENDIF}(GetCodePage, 0{FMBToWCharFlags},
    PAnsiChar(Bytes), ByteCount, nil, 0);
end;

function TMBCSEncodingImpl.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PWideChar; CharCount: Integer): Integer;
begin
  Result := {$IFNDEF SGDEL_XE}MultiByteToWideChar{$ELSE}UnicodeFromLocaleChars{$ENDIF}(GetCodePage, 0{FMBToWCharFlags},
    PAnsiChar(Bytes), ByteCount, Chars, CharCount);
end;

{ TUTF7EncodingImpl }

function TUTF7EncodingImpl.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount * 3) + 2;
end;

function TUTF7EncodingImpl.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

{ TUTF8EncodingImpl }

function TUTF8EncodingImpl.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 3;
end;

function TUTF8EncodingImpl.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount + 1;
end;

function TUTF8EncodingImpl.GetPreamble: TBytes;
begin
  SetLength(Result, 3);
  Result[0] := $EF;
  Result[1] := $BB;
  Result[1] := $BF;
end;

{ TUnicodeEncodingImpl }

function TUnicodeEncodingImpl.GetByteCount(Chars: PWideChar;
  CharCount: Integer): Integer;
begin
  Result := CharCount * SizeOf(WideChar);
end;

function TUnicodeEncodingImpl.GetBytes(Chars: PWideChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := CharCount * SizeOf(WideChar);
  Move(Chars^, Bytes^, Result);
end;

function TUnicodeEncodingImpl.GetCharCount(Bytes: PByte;
  ByteCount: Integer): Integer;
begin
  Result := ByteCount div SizeOf(WideChar);
end;

function TUnicodeEncodingImpl.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PWideChar; CharCount: Integer): Integer;
begin
  Result := CharCount;
  Move(Bytes^, Chars^, CharCount * SizeOf(WideChar));
end;

function TUnicodeEncodingImpl.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 2;
end;

function TUnicodeEncodingImpl.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := (ByteCount div 2) + (ByteCount and 1) + 1;
end;

function TUnicodeEncodingImpl.GetPreamble: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FF;
  Result[1] := $FE;
end;

{ TBigEndianUnicodeEncodingImpl }

function TBigEndianUnicodeEncodingImpl.GetBytes(Chars: PWideChar;
  CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to CharCount - 1 do
  begin
    Bytes^ := Hi(Word(Chars^));
    Inc(Bytes);
    Bytes^ := Lo(Word(Chars^));
    Inc(Bytes);
    Inc(Chars);
  end;
  Result := CharCount * SizeOf(WideChar);
end;

function TBigEndianUnicodeEncodingImpl.GetChars(Bytes: PByte;
  ByteCount: Integer; Chars: PWideChar; CharCount: Integer): Integer;
var
  P: PByte;
  I: Integer;
begin
  P := Bytes;
  Inc(P);
  for I := 0 to CharCount - 1 do
  begin
    Chars^ := WideChar(P^ or (Bytes^ shl 8));
    Inc(Bytes, 2);
    Inc(P, 2);
    Inc(Chars);
  end;
  Result := CharCount;
end;

function TBigEndianUnicodeEncodingImpl.GetPreamble: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FE;
  Result[1] := $FF;
end;
{$ENDIF}

end.



unit sgEncoding;
{$I SGDXF.inc}
interface

uses
  SysUtils;

type

  TUnicodeCharArray = {$IFDEF SGFPC}SysUtils.TUnicodeCharArray{$ELSE}
  {$IFDEF UNICODE}TCharArray{$ELSE}array of WideChar{$ENDIF}{$ENDIF};
  UnicodeString = {$IFDEF UNICODESTRING_TYPE_DEFINED}System.UnicodeString{$ELSE}WideString{$ENDIF};
{$IFNDEF SGFPC}{$IFNDEF SGDEL_2007}
  TBytes = array of Byte;
{$ENDIF}{$ENDIF}

  IEncoding = interface
    ['{24652048-CB61-4D02-8557-667773FF2068}']
    function GetCodepage: Cardinal;
    function GetCharset: Byte;
    function GetLANGID: Word;
    function GetIsSingleByte: Boolean;

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
    function GetMaxByteCount(CharCount: Integer): Integer;
    function GetMaxCharCount(ByteCount: Integer): Integer;
    function GetPreamble: TBytes; //virtual;
    function GetString(const Bytes: TBytes): UnicodeString; overload;
    function GetString(const Bytes: TBytes; ByteIndex, ByteCount: Integer): UnicodeString; overload;

    function IsEqual(const AEncoding: IEncoding): Boolean;
    property IsSingleByte: Boolean read GetIsSingleByte;

    property Codepage: Cardinal read GetCodepage;
    property Charset: Byte read GetCharset;
    property LANGID: Word read GetLANGID;
  end;

  IEncodingManager = interface
    ['{6CB5EACF-633D-4AB5-BE48-63167315E475}']
    function FromCP(ACodepage: Word): IEncoding;
    function FromLANGID(ALANGID: Word): IEncoding;
    function FromCharset(ACharset: Byte): IEncoding;
    function FromChar(AChar: Word): IEncoding;
  end;

implementation

end.

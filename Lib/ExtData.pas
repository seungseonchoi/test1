{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                Extended data for entity                    }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit ExtData;

{$INCLUDE SGDXF.inc}

{$DEFINE SORTDATA}
{$DEFINE STREXTDATA_AS_STRINGREF}

interface

uses
  {$IFDEF  SG_FIREMONKEY}
  sgFMXTypes,
  {$ENDIF}
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
  {$ENDIF}
  {$IFDEF SGFPC}
  LCLIntf, LCLType, Types, {$IFDEF SG_NON_WIN_PLATFORM}cwstring,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils, sgConsts, sgFunction, sgLists, sgXMLParser;

type
  TsgExtDataType = (edtUndefined, edtByte, edtInt16, edtInt, edtInt64,
    edtSingle, edtDouble, edtString, edtF2DPoint, edtF3DPoint, edtBinary,
    edtObject);
const

  cnstExtDataTypeString : array[TsgExtDataType] of String = (
    'Undefined', 'Byte', 'Int16', 'Int', 'Int64',
    'Single', 'Double', 'String', 'F2DPoint', 'F3DPoint', 'Binary',
    'Object'
  );

type
  PsgExtData = ^TsgExtData;
  TsgExtData = packed record
    EType: TsgExtdataType;
    ECode: SmallInt;
    ECount: Byte; // count of elements (edtF2DPoint==2;edtF3DPoint==3;edtBinary==count;edtString==length)
    case TsgExtdataType of
      edtUndefined,
      edtF2DPoint,
      edtF3DPoint:  (EData: Pointer);
      edtByte:      (EByte: Byte);
      edtInt16:     (EInt16: SmallInt);
      edtInt:       (EInt: Integer);
      edtInt64:     (EInt64: Int64);
      edtSingle:    (ESingle: Single);
      edtDouble:    (EDouble: Double);
      edtString:    (EStr: Pointer);// pointer to Chars
      edtBinary:    (EBytes: PByte);
      edtObject:    (EObject: Pointer)// TObject
  end;

  PsgExtDataPtrArray = ^TsgExtDataPtrArray;
  TsgExtDataPtrArray = array[0 .. MaxInt div SizeOf(PsgExtData) - 1] of PsgExtData;

  TsgCADExtendedData = class
  private
    FData: TsgList;
    FFlags: Byte;
    FVersion: TsgDWGVersion;
    procedure DoSetDataVariant(const AIndex: Integer; const AValue: Variant);
    function GetData(const AIndex: Integer): TsgExtData;
    function GetDataBinary(const AIndex: Integer): PByte;
    function GetDataBinaryAsHex(const AIndex: Integer): sgRawByteString;
    function GetDataByte(const AIndex: Integer): Byte;
    function GetDataCode(const AIndex: Integer): Smallint;
    function GetDataCount: Integer;
    function GetDataDouble(const AIndex: Integer): Double;
    function GetDataInt(const AIndex: Integer): Integer;
    function GetDataInt16(const AIndex: Integer): SmallInt;
    function GetDataInt64(const AIndex: Integer): Int64;
    function GetDataObject(const AIndex: Integer): TObject;
    function GetDataPoint(const AIndex: Integer): TFPoint;
    function GetDataPoint2D(const AIndex: Integer): TF2DPoint;
    function GetDataSingle(const AIndex: Integer): Single;
    function GetDataString(const AIndex: Integer): string;
    function GetDataType(const AIndex: Integer): TsgExtdataType;
    function GetDataVariant(const AIndex: Integer): Variant;
    function GetExtData(const AValue: Integer): PsgExtData;
    procedure SetDataBinary(const AIndex: Integer; const AValue: PByte);
    procedure SetDataBinaryAsHex(const AIndex: Integer; const AValue: sgRawByteString);
    procedure SetDataByte(const AIndex: Integer; const AValue: Byte);
    procedure SetDataDouble(const AIndex: Integer; const AValue: Double);
    procedure SetDataInt(const AIndex, AValue: Integer);
    procedure SetDataInt16(const AIndex: Integer; const AValue: SmallInt);
    procedure SetDataInt64(const AIndex: Integer; const AValue: Int64);
    procedure SetDataObject(const AIndex: Integer; const AValue: TObject);
    procedure SetDataPoint(const AIndex: Integer; const AValue: TFPoint);
    procedure SetDataPoint2D(const AIndex: Integer; const AValue: TF2DPoint);
    procedure SetDataSingle(const AIndex: Integer; const AValue: Single);
    procedure SetDataString(const AIndex: Integer; const AValue: string);
    procedure SetDataCode(const AIndex: Integer; const AValue: Smallint);
    procedure SetDataVariant(const AIndex: Integer; const AValue: Variant);
    function GetList: PsgExtDataPtrArray;
  protected
{$IFDEF SG_BTI}
    class function CreateByBlockPattern(const AVersion: TsgDWGVersion;
      const ABTIEntType: Integer;
      const AParentName: string = ''): TsgCADExtendedData;
{$ENDIF}
    function AddExtData(const ACode: Integer; const AType: TsgExtdataType;
      const AElementsCount: Integer = 0): Integer;
    procedure ClearData;
    function CompareExtDataByCode(const AVal1, AVal2: Pointer): Integer;
    procedure CreateData;
    class procedure DisposeData(AData: PsgExtData; const AEDataOnly: Boolean);
    function GetCodeFor(AIndex: Integer; var ACode: SmallInt): Boolean;
    function GetSorted: Boolean;
    function GetUseCode: Boolean; virtual;
    procedure SetSorted(const AValue: Boolean); virtual;
    procedure SetUseCode(const AValue: Boolean); virtual;
    function AddArrayPointsBase(const AIs3DPoint: Boolean;
      const ACodeCount, ACodePoint: Smallint; const AList: IsgArrayFPoint): Integer;
    class function GetStringFromExtData(const AData: TsgExtData): string;
    class procedure SetExtDataString(AData: PsgExtData; const S: string);
    procedure Insert(Index: Integer; const AValue: Variant);
  public
    constructor Create(const AVersion: TsgDWGVersion); virtual;
    destructor Destroy; override;
    class function GetTypeByCode(const ACode: Integer): TsgExtDataType;
    procedure Clear;
    function AddBinary(const ACode: SmallInt; const ACount: Byte;
      const AData: PByte): Integer; overload;
    function AddBinary(const ACode: SmallInt; const AHexData: AnsiString): Integer; overload;
    function AddByte(const ACode: SmallInt; const AData: Byte): Integer;
    function AddStringBig(const ACode: SmallInt; const AData: string): Integer;
    function AddDouble(const ACode: SmallInt; const AData: Double): Integer;
    function AddInt(const ACode: SmallInt; const AData: Integer): Integer;
    function AddInt16(const ACode: SmallInt; const AData: Smallint): Integer;
    function AddInt64(const ACode: SmallInt; const AData: Int64): Integer;
    function AddListPoints(const ACodeCount, ACodePoint: Smallint;
      const AList: TFPointList): Integer;
    function AddFPoints(const ACodeCount, ACodePoint: Smallint;
      const AList: TFPointList): Integer;
    function AddListPoints2D(const ACodeCount, ACodePoint: Smallint;
      const AList: TF2DPointList): Integer;
    function AddArrayPoints(const ACodeCount, ACodePoint: Smallint;
      AList: IsgArrayFPoint): Integer;
    function AddArrayPoints2D(const ACodeCount, ACodePoint: Smallint;
      AList: IsgArrayFPoint): Integer;
    function AddObject(const ACode: SmallInt; const AData: TObject): Integer;
    function AddPoint(const ACode: SmallInt; const AData: TFPoint): Integer;
    function AddPoint2D(const ACode: SmallInt; const AData: TF2DPoint): Integer;
    function AddSingle(const ACode: SmallInt; const AData: Single): Integer;
    function AddString(const ACode: SmallInt; const AData: string): Integer;

    function AddComplexInt16(const ACodeValue, AValue: SmallInt): Integer; virtual;
    function AddComplexInt32(const ACodeValue: SmallInt; const AValue: Integer): Integer; virtual;
    function AddComplexDouble(const ACodeValue: SmallInt; const AValue: Double): Integer; virtual;
    function AddComplexString(const ACodeValue: SmallInt; const AValue: string): Integer; virtual;
    function AddComplexPoint(const ACodeValue: SmallInt; const AValue: TFPoint): Integer; virtual;

    procedure AssignData(Source: TsgCADExtendedData); virtual;
    procedure ChangeType(const AIndex: Integer; const ANewType: TsgExtdataType);
    procedure Delete(const AIndex: Integer);
    function HasCode(const ACode: SmallInt; AIndex: PInteger = nil): Boolean;
    function IndexOfCode(const ACode: Integer): Integer;
    function IsEqualName(const AName :string): Boolean;
    function IsEqualSubName(const AName: string; const AIndex: Integer = 1): Boolean;

    function ExtractEEDType(var AGroupName: string;
      var AEntTypeNum: Integer; const AMaxDataIndex: Integer = 4): Integer;
{$IFDEF SG_BTI}
    function AddBTIExtData(const AData: string; const ACRC: Integer): Boolean;
    function ReadBTIExtData(const AIndex: Integer;
      var AData: string; var ACRC: Integer): Boolean;
{$ENDIF}
    function ToXMLNode(const ANodeAppId: TsgNode): Integer;
    function FromXMLNode(const AType: TsgXMLType;
      const ANode: TsgNodeSample; const AIsChild: Boolean;
      const AResult: IsgResultNode = nil): Integer;
    property Data[const AIndex: Integer]: TsgExtData read GetData;
    property DataByte[const AIndex: Integer]: Byte read GetDataByte
      write SetDataByte;
    property DataBinary[const AIndex: Integer]: PByte read GetDataBinary
      write SetDataBinary;
    property DataBinaryAsHex[const AIndex: Integer]: sgRawByteString
      read GetDataBinaryAsHex write SetDataBinaryAsHex;
    property DataCode[const AIndex: Integer]: Smallint read GetDataCode
      write SetDataCode;
    property DataCount: Integer read GetDataCount;
    property DataDouble[const AIndex: Integer]: Double read GetDataDouble
      write SetDataDouble;
    property DataInt[const AIndex: Integer]: Integer read GetDataInt
      write SetDataInt;
    property DataInt16[const AIndex: Integer]: SmallInt read GetDataInt16
      write SetDataInt16;
    property DataInt64[const AIndex: Integer]: Int64 read GetDataInt64
      write SetDataInt64;
    property DataObject[const AIndex: Integer]: TObject read GetDataObject
      write SetDataObject;
    property DataPoint[const AIndex: Integer]: TFPoint read GetDataPoint
      write SetDataPoint;
    property DataPoint2D[const AIndex: Integer]: TF2DPoint read GetDataPoint2D
      write SetDataPoint2D;
    property DataSingle[const AIndex: Integer]: Single read GetDataSingle
      write SetDataSingle;
    property DataString[const AIndex: Integer]: String read GetDataString
      write SetDataString;
    property DataVariant[const AIndex: Integer]: Variant read GetDataVariant
      write SetDataVariant;
    property DataType[const AIndex: Integer]: TsgExtdataType read GetDataType;
    property List: PsgExtDataPtrArray read GetList;
    property Sorted: Boolean read GetSorted write SetSorted;
    property UseCode: Boolean read GetUseCode write SetUseCode;
    property Version: TsgDWGVersion read FVersion;
  end;

  TsgBTIExtendedData = class(TsgCADExtendedData)
  private
    function GetxByteArray(const AIndex: Integer): Byte;
    function GetxDoubleArray(const AIndex: Integer): Double;
    function GetxIntArray(const AIndex: Integer): Integer;
    procedure SetxByteArray(const AIndex: Integer; const AValue: Byte);
    procedure SetxDoubleArray(const AIndex: Integer; const AValue: Double);
    procedure SetxIntArray(const AIndex, AValue: Integer);
    function GetxByteCount: Integer;
    function GetxDoubleCount: Integer;
    function GetxIntCount: Integer;
  protected
    function GetUseCode: Boolean; override;
    procedure SetSorted(const AValue: Boolean); override;
  public
    constructor Create(const AVersion: TsgDWGVersion); override;
    property xByteArray[const AIndex: Integer]: Byte read GetxByteArray
      write SetxByteArray;
    property xDoubleArray[const AIndex: Integer]: Double read GetxDoubleArray
      write SetxDoubleArray;
    property xIntArray[const AIndex: Integer]: Integer read GetxIntArray
      write SetxIntArray;
    property xByteCount: Integer read GetxByteCount;
    property xDoubleCount: Integer read GetxDoubleCount;
    property xIntCount: Integer read GetxIntCount;
  end;

implementation

{$IFDEF SGDEL_6}
uses
  Variants;
{$ENDIF}

const
  cnstExdTadaClear: TsgExtData = (EType: edtUndefined; ECode: 0; ECount: 0; EInt64: 0);

type
  TsgListAccess = class(TsgList);

{ TsgExtendedData }

function TsgCADExtendedData.GetCodeFor(AIndex: Integer;
  var ACode: SmallInt): Boolean;
var
  vData: PsgExtData;
begin
  Result := False;
  if (AIndex >= 1) and (AIndex < DataCount) then
  begin
    vData := GetExtData(AIndex - 1);
    if (vData^.EType = edtInt16) and (vData^.ECode = 1070) then
    begin
      ACode := vData^.EInt16;
      Result := True;
    end;
  end;
end;

function TsgCADExtendedData.GetData(const AIndex: Integer): TsgExtData;
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^
  else
    Result := cnstExdTadaClear;
end;

function TsgCADExtendedData.GetDataBinary(const AIndex: Integer): PByte;
var
  vData: PsgExtData;
begin
  Result := nil;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EBytes;
end;

function TsgCADExtendedData.GetDataBinaryAsHex(const AIndex: Integer): sgRawByteString;
var
  vData: PsgExtData;
begin
  Result := '';
  vData := GetExtData(AIndex);
  if vData <> nil then
  begin
    SetLength(Result, vData^.ECount shl 1);
    sgBinToHex(vData^.EBytes, Pointer(Result), vData^.ECount);
  end;
end;

function TsgCADExtendedData.GetDataByte(const AIndex: Integer): Byte;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EByte;
end;

function TsgCADExtendedData.GetDataCode(const AIndex: Integer): Smallint;
begin
  Result := -1;
  if FData <> nil then
    Result := PsgExtData(FData[AIndex])^.ECode;
end;

function TsgCADExtendedData.GetDataCount: Integer;
begin
  if FData <> nil then
    Result := FData.Count
  else
    Result := 0;
end;

function TsgCADExtendedData.GetDataDouble(const AIndex: Integer): Double;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EDouble;
end;

function TsgCADExtendedData.GetDataInt(const AIndex: Integer): Integer;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EInt;
end;

function TsgCADExtendedData.GetDataInt16(const AIndex: Integer): SmallInt;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EInt16;
end;

function TsgCADExtendedData.GetDataInt64(const AIndex: Integer): Int64;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EInt64;
end;

function TsgCADExtendedData.GetDataObject(const AIndex: Integer): TObject;
var
  vData: PsgExtData;
begin
  Result := nil;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := TObject(vData^.EObject);
end;

function TsgCADExtendedData.GetDataPoint(const AIndex: Integer): TFPoint;
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := PFPoint(vData^.EData)^
  else
    Result := cnstFPointZero;
end;

function TsgCADExtendedData.GetDataPoint2D(const AIndex: Integer): TF2DPoint;
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := PF2DPoint(vData^.EData)^
  else
    Result := cnstF2DPointZero;
end;

function TsgCADExtendedData.GetDataSingle(const AIndex: Integer): Single;
var
  vData: PsgExtData;
begin
  Result := 0;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.ESingle;
end;

function TsgCADExtendedData.GetDataString(const AIndex: Integer): string;
var
  vData: PsgExtData;
begin
  Result := '';
  vData := GetExtData(AIndex);
  if (vData <> nil) and (vData^.EType = edtString) then
    Result := GetStringFromExtData(vData^);
end;

function TsgCADExtendedData.GetDataType(const AIndex: Integer): TsgExtdataType;
var
  vData: PsgExtData;
begin
  Result := edtUndefined;
  vData := GetExtData(AIndex);
  if vData <> nil then
    Result := vData^.EType;
end;

function TsgCADExtendedData.GetDataVariant(const AIndex: Integer): Variant;
var
  vDst: PByte;
  vData: PsgExtData;
begin
  VarClear(Result);
  vData := GetExtData(AIndex);
  if Assigned(vData) then
  begin
    case vData^.EType of
      edtUndefined:;
      edtByte:      Result := vData^.EByte;
      edtInt16:     Result := vData^.EInt16;
      edtInt:       Result := vData^.EInt;
{$IFNDEF SGDEL_6}
      edtInt64:
        begin
          PInt64(@TVarData(Result).VDouble)^ := vData^.EInt64;
          TVarData(Result).VType := varInt64;
        end;
{$ELSE}
      edtInt64:     Result := vData^.EInt64;
{$ENDIF}
      edtSingle:    Result := vData^.ESingle;
      edtDouble:    Result := vData^.EDouble;
      edtString:    Result := GetStringFromExtData(vData^);
      edtF2DPoint:
        begin
          Result := VarArrayCreate([0, 1], varDouble);
          vDst := VarArrayLock(Result);
          Move(vData^.EData^, vDst^, vData^.ECount * SizeOf(Double));
          VarArrayUnlock(Result);
        end;
      edtF3DPoint:
        begin
          Result := VarArrayCreate([0, 2], varDouble);
          vDst := VarArrayLock(Result);
          Move(vData^.EData^, vDst^, vData^.ECount * SizeOf(Double));
          VarArrayUnlock(Result);
        end;
      edtBinary:
        begin
          Result := VarArrayCreate([0, vData^.ECount - 1], varByte);
          vDst := VarArrayLock(Result);
          Move(vData^.EBytes^, vDst^, vData^.ECount);
          VarArrayUnlock(Result);
        end;
      edtObject:
        begin
          TVarData(Result).VType := {$IFDEF SGDEL_XE2}varObject{$ELSE}varAny{$ENDIF};
          TVarData(Result).VPointer := vData^.EObject;
        end;
    end;
    TVarData(Result).{$IFDEF SGFPC}res1{$ELSE}Reserved1{$ENDIF} := Word(vData^.ECode);
  end;
end;

function TsgCADExtendedData.GetExtData(const AValue: Integer): PsgExtData;
var
  vIndex: Integer;
begin
  Result := nil;
  if FData <> nil then
  begin
    if UseCode then
    begin
      vIndex := IndexOfCode(AValue);
      if vIndex > -1 then
        Result := FData[vIndex];
    end
    else
      Result := FData[AValue];
  end;
end;

function TsgCADExtendedData.GetList: PsgExtDataPtrArray;
begin
  Result := PsgExtDataPtrArray(TsgListAccess(FData).GetItemBase(0));
end;

procedure TsgCADExtendedData.SetDataBinary(const AIndex: Integer;
  const AValue: PByte);
var
  vExtData: PsgExtData;
begin
  vExtData := GetExtData(AIndex);
  if vExtData <> nil then
  begin
    DisposeData(vExtData, True);
    vExtData^.EBytes := AValue;
  end;
end;

procedure TsgCADExtendedData.SetDataBinaryAsHex(const AIndex: Integer;
  const AValue: sgRawByteString);
var
  vData: PsgExtData;
{$IFDEF SG_FIREMONKEY}
  vBuf: TBytes;
{$ENDIF}
begin
  vData := GetExtData(AIndex);
  if Assigned(vData) then
  begin
    DisposeData(vData, True);
    vData^.ECount := Length(AValue) shr 1;
    GetMem(vData^.EBytes, vData^.ECount);
{$IFNDEF SG_FIREMONKEY}
    HexToBin(PAnsiChar(AValue), PAnsiChar(vData^.EBytes), vData^.ECount);
{$ELSE}
    SetLength(vBuf, vData^.ECount);
    HexToBin(BytesOf(AValue), 0, vBuf, 0, vData^.ECount);
    System.Move(vBuf[0], vData^.EBytes, vData^.ECount);
{$ENDIF}
  end;
end;

procedure TsgCADExtendedData.SetDataByte(const AIndex: Integer;
  const AValue: Byte);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.EByte := AValue;
end;

procedure TsgCADExtendedData.SetDataCode(const AIndex: Integer;
  const AValue: Smallint);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.ECode := AValue;
end;

procedure TsgCADExtendedData.SetDataDouble(const AIndex: Integer;
  const AValue: Double);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.EDouble := AValue;
end;

procedure TsgCADExtendedData.SetDataInt(const AIndex, AValue: Integer);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.EInt := AValue;
end;

procedure TsgCADExtendedData.SetDataInt16(const AIndex: Integer;
  const AValue: SmallInt);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.EInt16 := AValue;
end;

procedure TsgCADExtendedData.SetDataInt64(const AIndex: Integer;
  const AValue: Int64);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.EInt64 := AValue;
end;

procedure TsgCADExtendedData.SetDataObject(const AIndex: Integer;
  const AValue: TObject);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    TObject(vData^.EObject) := AValue
end;

procedure TsgCADExtendedData.SetDataPoint(const AIndex: Integer;
  const AValue: TFPoint);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    PFPoint(vData^.EData)^ := AValue;
end;

procedure TsgCADExtendedData.SetDataPoint2D(const AIndex: Integer;
  const AValue: TF2DPoint);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    PF2DPoint(vData^.EData)^ := AValue;
end;

procedure TsgCADExtendedData.SetDataSingle(const AIndex: Integer;
  const AValue: Single);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    vData^.ESingle := AValue;
end;

procedure TsgCADExtendedData.SetDataString(const AIndex: Integer;
  const AValue: string);
var
  vData: PsgExtData;
begin
  vData := GetExtData(AIndex);
  if vData <> nil then
    SetExtDataString(vData, AValue);
end;

procedure TsgCADExtendedData.DoSetDataVariant(const AIndex: Integer; const AValue: Variant);
begin
  case VarType(AValue) and varTypeMask of
    varByte, varShortInt: DataByte[AIndex] := TVarData(AValue).VByte;
    varSmallint{$IFDEF SGDEL_6}, varWord{$ENDIF}: DataInt16[AIndex] := TVarData(AValue).VSmallInt;
    {$IFDEF SGDEL_6}varLongWord, {$ENDIF}varInteger: DataInt[AIndex] := TVarData(AValue).VInteger;
    varInt64, varUInt64: DataInt64[AIndex] := PInt64(@TVarData(AValue).VDouble)^;
    varSingle: DataSingle[AIndex] := TVarData(AValue).VSingle;
    varDouble: DataDouble[AIndex] := TVarData(AValue).VDouble;
    varCurrency: DataDouble[AIndex] := TVarData(AValue).VCurrency;
{$IFDEF SGDEL_XE2}varObject{$ELSE}varAny{$ENDIF}: DataObject[AIndex] := TVarData(AValue).VPointer;
    varOleStr, varString{$IFDEF SGDEL_2009}, varUString{$ENDIF}: DataString[AIndex] := VarToStr(AValue);
  end;
end;

procedure TsgCADExtendedData.SetDataVariant(const AIndex: Integer;
  const AValue: Variant);
var
  vCount, vSize: Integer;
  vType: Word;
  vVarData, vBytes: PByte;
  vData: PsgExtData;
  vValueNoInd: Variant;
begin
  vType := VarType(AValue);
  //DataCode[AIndex] := TVarData(AValue).Reserved1;//??
  if vType and varByRef <> 0 then
  begin
    VarCopyNoInd(vValueNoInd, AValue);
    SetDataVariant(AIndex, vValueNoInd);
  end
  else
    if vType and varArray <> 0 then
    begin
      vData := GetExtData(AIndex);
      if vData <> nil then
      begin
        DisposeData(vData, True);
        vBytes := nil;
        vCount := VarArrayHighBound(AValue, 1) - VarArrayLowBound(AValue, 1) + 1;
        vSize := 0;
        case vType and varTypeMask of
          varDouble:
            begin
              vSize := vCount * SizeOf(Double);
              ReallocMem(vData^.EData, vSize);
              vBytes := vData^.EData;
              vData^.ECount := vCount;
            end;
          varByte:
            begin
              vSize := vCount;
              ReallocMem(vData^.EData, vSize);
              vBytes := vData^.EData;
              vData^.ECount := vCount;
            end;
        end;
        if vBytes <> nil then
        begin
          vVarData := VarArrayLock(AValue);
          Move(vVarData^, vBytes, vSize);
          VarArrayUnlock(AValue);
        end;
      end;
    end
    else
      DoSetDataVariant(AIndex, AValue);
end;

class procedure TsgCADExtendedData.SetExtDataString(AData: PsgExtData;
  const S: string);
var
  vS: string;
begin
  if Length(S) > High(Byte) then
    vS := Copy(S, 1, High(Byte))
  else
    vS := S;
  AData^.ECount := Length(vS);
{$IFDEF STREXTDATA_AS_STRINGREF}
  string(AData^.EStr) := vS;
{$ELSE}
  if AData^.ECount > 0 then
  begin
    ReallocMem(AData^.EStr, (AData^.ECount + 1) * SizeOf(Char));
    Move(PPointer(vS)^, AData^.EStr^, (AData^.ECount + 1) * SizeOf(Char));
  end
  else
    ReallocMem(AData^.EStr, 0);
{$ENDIF}
end;

function TsgCADExtendedData.AddExtData(const ACode: Integer;
  const AType: TsgExtdataType; const AElementsCount: Integer = 0): Integer;
var
  vData: PsgExtData;
begin
  if FData = nil then
    CreateData;
  New(vData);
  vData^.ECode := ACode;
  vData^.EType := AType;
  vData^.EInt64 := 0;
  vData^.ECount := AElementsCount;
  Result := FData.Add(vData);
end;

procedure TsgCADExtendedData.ChangeType(const AIndex: Integer;
  const ANewType: TsgExtdataType);
var
  vItem: PsgExtData;
begin
  vItem := FData.Items[AIndex];
  if (vItem <> nil) and (vItem^.EType <> ANewType) then
  begin
    DisposeData(vItem, True);
    case ANewType of
      edtF2DPoint:
        begin
          vItem^.ECount := 2;
          vItem^.EData := AllocMem(vItem^.ECount * SizeOf(Double));
        end;
      edtF3DPoint:
        begin
          vItem^.ECount := 3;
          vItem^.EData := AllocMem(vItem^.ECount * SizeOf(Double));
        end;
    end;
    vItem^.EType := ANewType;
  end;
end;

procedure TsgCADExtendedData.Clear;
begin
  ClearData;
end;

procedure TsgCADExtendedData.ClearData;
var
  I: Integer;
begin
  if FData <> nil then
  begin
    for I := 0 to FData.Count - 1 do
      DisposeData(FData[I], False);
    FData.Free;
    FData := nil;
  end;
end;

function TsgCADExtendedData.CompareExtDataByCode(
  const AVal1, AVal2: Pointer): Integer;
begin
  Result := PsgExtData(AVal1)^.ECode - PsgExtData(AVal2)^.ECode;
end;

procedure TsgCADExtendedData.CreateData;
begin
  FData := TsgList.Create;
  FData.Duplicates := dupAccept;
  FData.ProcCompare := CompareExtDataByCode;
  FData.Sorted := Sorted;
end;

class procedure TsgCADExtendedData.DisposeData(AData: PsgExtData;
  const AEDataOnly: Boolean);
begin
  AData^.ECount := 0;
  case AData^.EType of
    edtF2DPoint:  FreeMemAndNil(AData^.EData);
    edtF3DPoint:  FreeMemAndNil(AData^.EData);
{$IFDEF STREXTDATA_AS_STRINGREF}
    edtString:    string(AData^.EStr) := '';
{$ELSE}
    edtString:    FreeMemAndNil(AData^.EStr);
{$ENDIF}
    edtBinary:    FreeMemAndNil(Pointer(AData^.EBytes));
    edtObject:    TObject(AData^.EData) := nil;
  else
    AData^.EInt64 := 0;
  end;
  if not AEDataOnly then
    FreeMem(AData);
end;

function TsgCADExtendedData.GetSorted: Boolean;
begin
  Result := (FFlags and 2) <> 0;
end;

class function TsgCADExtendedData.GetStringFromExtData(
  const AData: TsgExtData): string;
begin
{$IFDEF STREXTDATA_AS_STRINGREF}
  Result := string(AData.EStr);
{$ELSE}
  if AData.ECount > 0 then
    SetString(Result, PChar(AData.EStr), AData.ECount)
  else
    Result := string(PChar(AData.EStr));
{$ENDIF}
end;

class function TsgCADExtendedData.GetTypeByCode(
  const ACode: Integer): TsgExtDataType;
begin
  Result := edtUndefined;
  case ACode of
    0..4,6..9, 300..309, 410..419, 999, 1000..1009:
      Result := edtString;//String
    10..37:
      Result := edtF3DPoint;//Double precision 3D point
    38..59, 110..139, 210..239, 1010..1059:
      Result := edtDouble;//Double precision floating point value
    60..79, 170..179, 270..289, 370..389, 400..409, 1060..1070:
      Result := edtInt16;//6-bit integer value
    90..99, 1071:
      Result := edtInt;//32-bit integer value
    100..104, 106..109:
      Result := edtString;//255-character maximum; less for Unicode strings
    5, 105, 310..369, 390..399:
      Result := edtInt64;//String representing hexadecimal (hex) handle value
    140..149:
      Result := edtDouble;//Double precision scalar floating-point value
    290..299:
      Result := edtByte;//Boolean flag value
  end;
end;

function TsgCADExtendedData.GetUseCode: Boolean;
begin
  Result := (FFlags and 1) <> 0;
end;

procedure TsgCADExtendedData.SetSorted(const AValue: Boolean);
begin
  FFlags := (FFlags and 253) or Byte(AValue);
  if FData <> nil then
    FData.Sorted := Sorted;
end;

procedure TsgCADExtendedData.SetUseCode(const AValue: Boolean);
begin
  FFlags := (FFlags and 254) or Byte(AValue);
end;

function TsgCADExtendedData.ToXMLNode(const ANodeAppId: TsgNode): Integer;
var
  I: Integer;
  vNodeEEDItem: TsgNode;
  vData: TsgExtData;
begin
  Result := cnstXML_OK;
  ANodeAppId.AddAttribNV(cnstXMLNames[xmlCount].Name).ValueAsInt := DataCount;
  for I := 0 to DataCount - 1 do
  begin
    vData := Data[I];
    vNodeEEDItem := ANodeAppId.AddChildNV(cnstXMLNames[xmlItem].Name);
    vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedCode].Name).ValueAsStr := IntToStr(vData.ECode);
    vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedType].Name).ValueAsStr := cnstExtDataTypeString[vData.EType];
    case vData.EType of
      edtString: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsStr := GetStringFromExtData(vData);
      edtF3DPoint: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsFPoint := PFPoint(vData.EData)^;
      edtF2DPoint: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsF2DPoint := PF2DPoint(vData.EData)^;
      edtDouble: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsDouble := vData.EDouble;
      edtInt16: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsInt := vData.EInt16;
      edtInt: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsInt := vData.EInt;
      edtInt64: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsInt64 := vData.EInt64;
      edtByte: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsInt := vData.EByte;
      edtBinary: vNodeEEDItem.AddAttribNV(cnstXMLNames[xmlEedData].Name).ValueAsStr := string(DataBinaryAsHex[I]);
    end;
  end;
end;

function TsgCADExtendedData.FromXMLNode(const AType: TsgXMLType;
  const ANode: TsgNodeSample; const AIsChild: Boolean;
  const AResult: IsgResultNode = nil): Integer;
var
  I: Integer;
  vNodeEEDItem: TsgNodeSample;
  vNodeCode, vNodeData(*, vNodeType*): TsgNodeSample;
  vECode: SmallInt;
  vEType: TsgExtdataType;
begin
  Result := cnstXML_OK;
  if AType.Id = xmlApplication then
  begin
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vNodeEEDItem := ANode.ChildNodes[I];

      vNodeCode := vNodeEEDItem.GetAttributeByName(cnstXMLNames[xmlEedCode].Name);
      vNodeData := vNodeEEDItem.GetAttributeByName(cnstXMLNames[xmlEedData].Name);
      //vNodeType := vNodeEEDItem.GetAttributeByName(cnstXMLNames[xmlEedType].Name);
      if Assigned(vNodeCode) and Assigned(vNodeData) then
      begin
        vECode := vNodeCode.ValueAsInt;
        vEType := GetTypeByCode(vECode);
        case vEType of
          edtString: AddString(vECode, vNodeData.ValueAsStr);
          edtF3DPoint: AddPoint(vECode, vNodeData.ValueAsFPoint);
          edtF2DPoint: AddPoint2D(vECode, vNodeData.ValueAsF2DPoint);
          edtDouble: AddDouble(vECode, vNodeData.ValueAsDouble);
          edtInt16: AddInt16(vECode, vNodeData.ValueAsInt);
          edtInt: AddInt(vECode, vNodeData.ValueAsInt);
          edtInt64: AddInt64(vECode, vNodeData.ValueAsInt64);
          edtByte: AddByte(vECode, vNodeData.ValueAsInt);
          edtBinary: AddBinary(vECode, AnsiString(vNodeData.ValueAsStr));
        end;
      end;
    end;
  end
  else
    Result := cnstXML_UNSUPPORTED;
end;

constructor TsgCADExtendedData.Create(const AVersion: TsgDWGVersion);
begin
  inherited Create;
  FVersion := AVersion;
end;

{$IFDEF SG_BTI}
class function TsgCADExtendedData.CreateByBlockPattern(const AVersion: TsgDWGVersion;
  const ABTIEntType: Integer; const AParentName: string = ''): TsgCADExtendedData;
begin
  Result := TsgCADExtendedData.Create(AVersion);
  Result.AddString(String_1001, 'Inventory');
  Result.AddString(String_1002, '{');
  Result.AddInt16(Integer_1070, Integer_70);
  Result.AddInt(Integer_1071, cnstBlockPattern);
  Result.AddInt16(Integer_1070, Integer_71);
  Result.AddInt(Integer_1071, ABTIEntType);
  Result.AddInt16(Integer_1070, String_331);
  Result.AddString(String_1000, AParentName);
  Result.AddString(String_1002, '}');
end;
{$ENDIF}

destructor TsgCADExtendedData.Destroy;
begin
  ClearData;
  inherited Destroy;
end;

function TsgCADExtendedData.AddBinary(const ACode: SmallInt;
  const ACount: Byte; const AData: PByte): Integer;
var
  vExtData: PsgExtData;
begin
  Result := AddExtData(ACode, edtBinary, ACount);
  vExtData := FData[Result];
  if ACount > 0 then
  begin
    GetMem(vExtData^.EBytes, ACount);
    if Assigned(AData) then
      Move(AData^, vExtData^.EBytes^, ACount);
  end;
end;

function TsgCADExtendedData.AddBinary(const ACode: SmallInt;
  const AHexData: AnsiString): Integer;
var
  vData: PsgExtData;
{$IFDEF SG_FIREMONKEY}
  vBuf: TBytes;
{$ENDIF}
begin
  Result := AddExtData(ACode, edtBinary, Length(AHexData) shr 1);
  vData := PsgExtData(FData[Result]);
  GetMem(vData^.EBytes, vData^.ECount);
{$IFNDEF SG_FIREMONKEY}
  HexToBin(PAnsiChar(AHexData), PAnsiChar(vData^.EBytes), vData^.ECount);
{$ELSE}
  SetLength(vBuf, vData^.ECount);
  HexToBin(BytesOf(AHexData), 0, vBuf, 0, vData^.ECount);
  System.Move(vBuf[0], vData^.EBytes^, vData^.ECount);
{$ENDIF}
end;

function TsgCADExtendedData.AddByte(const ACode: SmallInt;
  const AData: Byte): Integer;
begin
  Result := AddExtData(ACode, edtByte);
  PsgExtData(FData[Result])^.EByte := AData;
end;

function TsgCADExtendedData.AddStringBig(const ACode: SmallInt;
  const AData: string): Integer;
var
  vStr: string;
begin
  Result := 1;
  AddInt16(Integer_1070, ACode);
  if Length(AData) <= cnstMaxStringLen then
    AddString(String_1000, AData)
  else
  begin
    AddString(String_1000, Copy(AData, 1, cnstMaxStringLen));
    vStr := AData;
    repeat
      System.Delete(vStr, 1 ,cnstMaxStringLen);
      if Length(vStr) <= cnstMaxStringLen then
        AddString(String_1000, vStr)
      else
        AddString(String_1000, Copy(vStr, 1, cnstMaxStringLen));
      Inc(Result);
    until Length(vStr) <= cnstMaxStringLen;
  end;
end;

function TsgCADExtendedData.AddComplexDouble(const ACodeValue: SmallInt;
  const AValue: Double): Integer;
begin
  AddInt16(Integer_1070, ACodeValue);
  Result := AddDouble(Float_1040, AValue);
end;

function TsgCADExtendedData.AddComplexInt16(const ACodeValue,
  AValue: SmallInt): Integer;
begin
  AddInt16(Integer_1070, ACodeValue);
  Result := AddInt16(Integer_1070, AValue);
end;

function TsgCADExtendedData.AddComplexInt32(const ACodeValue: SmallInt;
  const AValue: Integer): Integer;
begin
  AddInt16(Integer_1070, ACodeValue);
  Result := AddInt(Integer_1071, AValue);
end;

function TsgCADExtendedData.AddComplexPoint(const ACodeValue: SmallInt;
  const AValue: TFPoint): Integer;
begin
  AddInt16(Integer_1070, ACodeValue);
  Result := AddPoint(Float_1010, AValue)
end;

function TsgCADExtendedData.AddComplexString(const ACodeValue: SmallInt;
  const AValue: string): Integer;
begin
  AddInt16(Integer_1070, ACodeValue);
  Result := AddString(String_1000, AValue);
end;

function TsgCADExtendedData.AddDouble(const ACode: SmallInt;
  const AData: Double): Integer;
begin
  Result := AddExtData(ACode, edtDouble);
  PsgExtData(FData[Result])^.EDouble := AData;
end;

function TsgCADExtendedData.AddInt(const ACode: SmallInt;
  const AData: Integer): Integer;
begin
  Result := AddExtData(ACode, edtInt);
  PsgExtData(FData[Result])^.EInt := AData;
end;

function TsgCADExtendedData.AddInt16(const ACode, AData: Smallint): Integer;
begin
  Result := AddExtData(ACode, edtInt16);
  PsgExtData(FData[Result])^.EInt16 := AData;
end;

function TsgCADExtendedData.AddInt64(const ACode: SmallInt;
  const AData: Int64): Integer;
begin
  Result := AddExtData(ACode, edtInt64);
  PsgExtData(FData[Result])^.EInt64 := AData;
end;

function TsgCADExtendedData.AddListPoints(const ACodeCount,
  ACodePoint: Smallint; const AList: TFPointList): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AList <> nil) and (AList.Count > 0) then
  begin
    Result := AddInt(ACodeCount, AList.Count);
    for I := 0 to AList.Count - 1 do
      AddPoint(ACodePoint, AList[I]);
  end;
end;

function TsgCADExtendedData.AddFPoints(const ACodeCount, ACodePoint: Smallint;
  const AList: TFPointList): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AList <> nil) and (AList.Count > 0) then
  begin
    Result := AddInt(ACodeCount, AList.Count);
    for I := 0 to AList.Count - 1 do
      AddPoint(ACodePoint, AList[I]);
  end;
end;

function TsgCADExtendedData.AddListPoints2D(const ACodeCount,
  ACodePoint: Smallint; const AList: TF2DPointList): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AList <> nil) and (AList.Count > 0) then
  begin
    Result := AddInt(ACodeCount, AList.Count);
    for I := 0 to AList.Count - 1 do
      AddPoint2D(ACodePoint, AList[I]);
  end;
end;

function TsgCADExtendedData.AddArrayPointsBase(const AIs3DPoint: Boolean;
  const ACodeCount, ACodePoint: Smallint; const AList: IsgArrayFPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AList <> nil) and (AList.FPointCount > 0) then
  begin
    Result := AddInt(ACodeCount, AList.FPointCount);
    if AIs3DPoint then
    begin
      for I := 0 to AList.FPointCount - 1 do
        AddPoint(ACodePoint, AList.FPoints[I]);
    end
    else
    begin
      for I := 0 to AList.FPointCount - 1 do
        AddPoint2D(ACodePoint, MakeF2DPointFrom3D(AList.FPoints[I]));
    end;
  end;
end;

function TsgCADExtendedData.AddArrayPoints(const ACodeCount,
  ACodePoint: Smallint; AList: IsgArrayFPoint): Integer;
begin
  Result := AddArrayPointsBase(True, ACodeCount, ACodePoint, AList);
end;

function TsgCADExtendedData.AddArrayPoints2D(const ACodeCount,
  ACodePoint: Smallint; AList: IsgArrayFPoint): Integer;
begin
  Result := AddArrayPointsBase(False, ACodeCount, ACodePoint, AList);
end;

function TsgCADExtendedData.AddObject(const ACode: SmallInt;
  const AData: TObject): Integer;
begin
  Result := AddExtData(ACode, edtObject);
  TObject(PsgExtData(FData[Result])^.EObject) := AData;
end;

function TsgCADExtendedData.AddPoint(const ACode: SmallInt;
  const AData: TFPoint): Integer;
begin
  Result := AddExtData(ACode, edtF3DPoint, 3);
  GetMem(PsgExtData(FData[Result])^.EData, SizeOf(TFPoint));
  PFPoint(PsgExtData(FData[Result])^.EData)^ := AData;
end;

function TsgCADExtendedData.AddPoint2D(const ACode: SmallInt;
  const AData: TF2DPoint): Integer;
begin
  Result := AddExtData(ACode, edtF2DPoint, 2);
  GetMem(PsgExtData(FData[Result])^.EData, SizeOf(TF2DPoint));
  PF2DPoint(PsgExtData(FData[Result])^.EData)^ := AData;
end;

function TsgCADExtendedData.AddSingle(const ACode: SmallInt;
  const AData: Single): Integer;
begin
  Result := AddExtData(ACode, edtSingle);
  PsgExtData(FData[Result])^.ESingle := AData;
end;

function TsgCADExtendedData.AddString(const ACode: SmallInt;
  const AData: string): Integer;
begin
  Result := AddExtData(ACode, edtString);
  SetExtDataString(PsgExtData(FData[Result]), AData);
end;

procedure TsgCADExtendedData.AssignData(Source: TsgCADExtendedData);
var
  I: Integer;
  vSrcData: TsgExtData;
begin
  ClearData;
  FFlags := Source.FFlags;
  for I := 0 to Source.DataCount - 1 do
  begin
    vSrcData := Source.Data[I];
    case vSrcData.EType of
      edtByte:     AddByte(vSrcData.ECode, vSrcData.EByte);
      edtInt:      AddInt(vSrcData.ECode, vSrcData.EInt);
      edtInt16:    AddInt16(vSrcData.ECode, vSrcData.EInt16);
      edtint64:    AddInt64(vSrcData.ECode, vSrcData.EInt64);
      edtSingle:   AddSingle(vSrcData.ECode, vSrcData.ESingle);
      edtDouble:   AddDouble(vSrcData.ECode, vSrcData.EDouble);
      edtF2DPoint: AddPoint2D(vSrcData.ECode, PF2DPoint(vSrcData.EData)^);
      edtF3DPoint: AddPoint(vSrcData.ECode, PFPoint(vSrcData.EData)^);
      edtString:   AddString(vSrcData.ECode, GetStringFromExtData(vSrcData));
      edtBinary:   AddBinary(vSrcData.ECode, vSrcData.ECount, vSrcData.EBytes);
      edtObject:   AddObject(vSrcData.ECode, TObject(vSrcData.EObject));
    end;
  end;
end;

procedure TsgCADExtendedData.Delete(const AIndex: Integer);
var
  vIndex: Integer;
begin
  if FData <> nil then
  begin
    if UseCode then
      vIndex := IndexOfCode(AIndex)
    else
      vIndex := AIndex;
    if vIndex > -1 then
    begin
      DisposeData(FData[vIndex], False);
      FData.Delete(vIndex);
      if FData.Count = 0 then
      begin
        FData.Free;
        FData := nil;
      end;
    end;
  end;
end;

function TsgCADExtendedData.HasCode(const ACode: SmallInt;
  AIndex: PInteger = nil): Boolean;
var
  vData: TsgExtData;
  vIndex: Integer;
begin
  Result := False;
  if FData <> nil then
  begin
    vData.ECode := ACode;
    vIndex := FData.IndexOf(@vData);
    Result := vIndex > -1;
    if Result and (AIndex <> nil) then
      AIndex^ := vIndex;
  end;
end;

function TsgCADExtendedData.IndexOfCode(const ACode: Integer): Integer;
var
  vData: TsgExtData;
begin
  if FData <> nil then
  begin
    vData.ECode := ACode;
    Result := FData.IndexOf(@vData);
  end
  else
    Result := -1;
end;

procedure TsgCADExtendedData.Insert(Index: Integer; const AValue: Variant);
var
  vData: PsgExtData;
  v: Variant;
begin
  if FData = nil then
    CreateData;
  New(vData);
  FillChar(vData^, SizeOf(vData^), 0);
  VarCopyNoInd(v, AValue);
  case VarType(v) and varTypeMask of
    varByte, varShortInt:
      begin
        if VarType(v) and varArray <> 0 then
          vData^.EType := edtBinary
        else
          vData^.EType := edtByte;
      end;
    varSmallint{$IFDEF SGDEL_6}, varWord{$ENDIF}: vData^.EType := edtInt16;
    {$IFDEF SGDEL_6}varLongWord, {$ENDIF}varInteger: vData^.EType := edtInt;
    varInt64, varUInt64: vData^.EType := edtInt64;
    varSingle: vData^.EType := edtSingle;
    varDouble, varCurrency:
      begin
        if VarType(v) and varArray <> 0 then
        begin
          case TVarData(v).VArray^.Bounds[0].ElementCount of
            3: vData^.EType := edtF3DPoint;
            2: vData^.EType := edtF2DPoint;
          end;
        end
        else
          vData^.EType := edtDouble;
      end;
{$IFDEF SGDEL_XE2}varObject{$ELSE}varAny{$ENDIF}: vData^.EType := edtObject;
    varOleStr, varString{$IFDEF SGDEL_2009}, varUString{$ENDIF}: vData^.EType := edtString;
  end;
  vData^.ECode := TVarData(v).{$IFDEF SGFPC}res1{$ELSE}Reserved1{$ENDIF};
  FData.Insert(Index, vData);
  SetDataVariant(Index, v);
end;

function TsgCADExtendedData.IsEqualName(const AName: string): Boolean;
begin
  Result := False;
  if DataCount > 0 then
  begin
    if (DataType[0] = edtString) and (DataCode[0] = String_1001) and
       (UpperCase(DataString[0]) = UpperCase(AName)) then
     Result := True;
  end;
end;

function TsgCADExtendedData.IsEqualSubName(const AName: string;
  const AIndex: Integer = 1): Boolean;
begin
  Result := False;
  if DataCount > AIndex then
  begin
    if (DataType[AIndex] = edtString) and (DataCode[AIndex] = String_1000) and
       (UpperCase(DataString[AIndex]) = UpperCase(AName)) then
     Result := True;
  end;
end;

function TsgCADExtendedData.ExtractEEDType(var AGroupName: string;
  var AEntTypeNum: Integer; const AMaxDataIndex: Integer = 4): Integer;
var
  I, C: Integer;
  vCode: SmallInt;
  vFind: Boolean;
  vDataStart: Boolean;
begin
  AGroupName := '';
  AEntTypeNum := cnstUndefined;
  C := AMaxDataIndex;
  if DataCount < C + 1 then
    C := DataCount - 1;
  vCode := 0;
  I := 0;
  vFind := False;
  vDataStart := False;
  while (I <= C) and not vFind do
  begin
    case DataType[I] of
      edtString:
        case DataCode[I] of
          String_1000: AGroupName := DataString[I];
          String_1002: vDataStart := DataString[I] = '{';
        end;
      edtByte:
        vDataStart := DataByte[I] = 0;
      edtInt16:
        if vDataStart then
          if vCode = 0 then
            vCode := DataInt16[I]
          else
            if vCode = Integer_70 then
            begin
              AEntTypeNum := DataInt16[I];
              vFind := True;
            end;
      edtInt:
        if vDataStart then
          if vCode = Integer_70 then
          begin
            AEntTypeNum := DataInt[I];
            vFind := True;
          end;
    end;
    Inc(I);
  end;
  Result := I;
end;

{$IFDEF SG_BTI}
function TsgCADExtendedData.AddBTIExtData(const AData: string; const ACRC: Integer): Boolean;
begin
  Result := True;
  AddInt16(Integer_1070, cnstBTExtDataCodeCRC);
  AddInt(Integer_1071, ACRC);
  AddStringBig(cnstBTExtDataCode, AData);
end;

function TsgCADExtendedData.ReadBTIExtData(const AIndex: Integer;
  var AData: string; var ACRC: Integer): Boolean;
var
  J, vCode: Integer;
begin
  Result := False;
  J := AIndex;
  AData := '';
  ACRC := 0;
  vCode := -1;
  while J < DataCount do
  begin
    case DataCode[J] of
      Integer_1070:
        begin
          if vCode = -1 then
            vCode := DataInt16[J]
          else
            vCode := -1;
        end;
      Integer_1071:
        begin
          case vCode of
            cnstBTExtDataCodeCRC:
              ACRC :=  DataInt[J];
          end;
          vCode := -1;
        end;
      String_1000:
        begin
          case vCode of
            cnstBTExtDataCode:
              AData := AData + DataString[J];
          else
            vCode := -1;
          end;
        end;
    else
      vCode := -1;
    end;
    Inc(J);
  end;
  if Length(AData) > 0 then
    Result := True;
end;
{$ENDIF}

{ TsgBTIExtendedData }

function TsgBTIExtendedData.GetxByteArray(const AIndex: Integer): Byte;
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  Result := DataByte[AIndex];
  FFlags := vFlags;
end;

function TsgBTIExtendedData.GetxByteCount: Integer;
begin
  Result := 6;
end;

function TsgBTIExtendedData.GetxDoubleArray(const AIndex: Integer): Double;
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  Result := DataDouble[AIndex + 8];
  FFlags := vFlags;
end;

function TsgBTIExtendedData.GetxDoubleCount: Integer;
begin
  Result := 11;
end;

function TsgBTIExtendedData.GetxIntArray(const AIndex: Integer): Integer;
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  Result := DataInt[AIndex + 6];
  FFlags := vFlags;
end;

function TsgBTIExtendedData.GetxIntCount: Integer;
begin
  Result := 2;
end;

procedure TsgBTIExtendedData.SetxByteArray(const AIndex: Integer;
  const AValue: Byte);
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  DataByte[AIndex] := AValue;
  FFlags := vFlags;
end;

procedure TsgBTIExtendedData.SetxDoubleArray(const AIndex: Integer;
  const AValue: Double);
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  DataDouble[AIndex + 8] := AValue;
  FFlags := vFlags;
end;

procedure TsgBTIExtendedData.SetxIntArray(const AIndex, AValue: Integer);
var
  vFlags: Byte;
begin
  vFlags := FFlags;
  UseCode := False;
  DataInt[AIndex + 6] := AValue;
  FFlags := vFlags;
end;

function TsgBTIExtendedData.GetUseCode: Boolean;
begin
  Result := False;
end;

procedure TsgBTIExtendedData.SetSorted(const AValue: Boolean);
begin
  inherited SetSorted(False)
end;

constructor TsgBTIExtendedData.Create(const AVersion: TsgDWGVersion);
var
  I: Integer;
begin
  inherited Create(AVersion);
  inherited SetSorted(False);

  AddByte(280, 0);
  AddByte(281, 0);
  AddByte(284, 0);
  AddByte(285, 0);
  AddByte(286, 0);
  AddByte(287, 0);

  AddInt(90, 0);
  AddInt(91, 0);

  I := 1;
  while I <= xDoubleCount do
  begin
    AddDouble(1040, 0);
    Inc(I);
  end;
end;

end.
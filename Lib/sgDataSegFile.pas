{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{     DWG section AcDb:AcDsPrototype_1b format parcer        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgDataSegFile;
{$I SGDXF.inc}
interface
{$DEFINE SG_TMP_CONCAT_BLOB_CHUNKS}// remove all entries if segment files will no errors
uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  sgFMXTypes,
{$ENDIF}
  SysUtils, Classes, CADImage, sgFunction, sgConsts, sgLists, DXFConv,
  sgComparer;

type
  PsgSegmentedFileHeader = ^TsgSegmentedFileHeader;
  TsgSegmentedFileHeader = packed record
		FileSignature: Cardinal;
		FileHeaderSize: Integer;
		Unknown3: Integer;
		Version: Integer;
		Unknown5: Integer;
		DsRevision: Integer;
		SegIdxOffset: Integer;
		SegIdxUnknown: Integer;
		SegIdxEntryCount: Integer;
		SchIdxIndex: Integer;
		DatIdxIndex: Integer;
		SearchIndex: Integer;
		PrvsavIndex: Integer;
		FileSize: Integer;
  end;

  PsgFileSegmentHeader = ^TsgFileSegmentHeader;
  TsgSegName = packed array[0..5] of AnsiChar;
  TsgFileSegmentHeader = packed record
		Signature: Word; //$D5AC
		Name: TsgSegName;
		SegmentIndex: Integer;
		Unknown1: Integer;
		SegmentSize: Cardinal;
		Unknown2: Integer;
		DataStorageRevision: Integer;
		Unknown3: Integer;
		SysDataAlignOffset: Integer;
		ObjDataAlignOffset: Integer;
    AlignmentBytes: packed array[0..7] of Byte;
    Data: packed record end;
  end;

  TsgSegIdxEntryType = (stUnknown, stSegInd, stDatInd, stData, stSchInd,
    stSchData, stSearch, stBlob01, stPrvsav, stFreesp);

  PsgSegIdxEntry = ^TsgSegIdxEntry;
  TsgSegIdxEntry = packed record
    Offset: UInt64;
    Size: Cardinal;
  end;

  PsgDatIdxEntry = ^TsgDatIdxEntry;
  TsgDatIdxEntry = packed record
    SegmentIndex: Cardinal;
		LocalOffset: Cardinal;
    SchemaIndex: Cardinal;
  end;

  PsgDatHeaderEntry = ^TsgDatHeaderEntry;
  TsgDatHeaderEntry = packed record
    EntrySize: Cardinal;
		Unknown: Cardinal;
    Handle: UInt64;
    LocalOffset: Cardinal;
  end;

  PsgSchIdxUnkPropHeaderEntry = ^TsgSchIdxUnkPropHeaderEntry;
  TsgSchIdxUnkPropHeaderEntry = packed record
    Index: Cardinal;
		SegmentIndex: Cardinal;
    LocalOffset: Cardinal;
  end;

  PsgSchIdxPropHeaderEntry = ^TsgSchIdxPropHeaderEntry;
  TsgSchIdxPropHeaderEntry = packed record
    SegmentIndex: Cardinal;
    LocalOffset: Cardinal;
    Index: Cardinal;
  end;

  PsgDataBlobReferencePage = ^TsgDataBlobReferencePage;
  TsgDataBlobReferencePage = packed record
    SegmentIndex: Integer;
    Size: Integer;
  end;
  TsgDataBlobReferencePages = array of TsgDataBlobReferencePage;
  PsgDataBlobReferencePageArray = ^TsgDataBlobReferencePageArray;
  TsgDataBlobReferencePageArray = array[Byte] of TsgDataBlobReferencePage;

  PsgDataBlobReference = ^TsgDataBlobReference;
  TsgDataBlobReference = packed record
	  TotalDataSize: UInt64;
		PageCount: Cardinal;
		RecordSize: Cardinal;
		PageSize: Cardinal;
		LastPageSize: Cardinal;
		Unknown1: Cardinal; //Must be 0
		Unknown2: Cardinal; //Must be 0
  end;

  TsgBlobDataEntry = array of Byte;

  TsgDSDataEntryType = (etUnknown, etACISBin, atACISTxt, etPNG);

  TsgBlobEntryReferenceHeader = packed record
    TotalDataSize: UInt64;
    PageStartOffset: UInt64;
    PageIndex: Integer;
    PageCount: Integer;
    PageDataSize: UInt64;
  end;

  PsgBlobEntryReference = ^TsgBlobEntryReference;
  TsgBlobEntryReference = record
    Header: TsgBlobEntryReferenceHeader;
    Offset: UInt64;
    Data: PByte;
  end;

  TsgSchDatUnkPropEntry = packed record
    DataSize: Cardinal;
    UnknownFlags: Cardinal;
  end;

  TsgDataSegFileError = class(Exception);

  TsgSegmentedFile = class;

  TsgCustomSegment = class
  private
    procedure FillDefautFileHeader(var AHeader: TsgFileSegmentHeader);
  protected
    FSegmentedFile: TsgSegmentedFile;
    FReaded: Boolean;
    FAlign: Cardinal;
    FOffset: UInt64;
    FSegmentIndex: Cardinal;
    FSegmentSize: Cardinal;
    class function GetSegName: TsgSegName; virtual;
    function GetAlign: Cardinal;
    function GetSegmentIndex: Cardinal;
    function GetOffset: UInt64;
    procedure SetOffset(const AValue: UInt64);
    function GetSegmentSize: Cardinal;
    procedure Read(Stream: TStream); virtual;
    procedure Write(Stream: TStream; AOffset: UInt64); virtual;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; virtual;
  public
    constructor Create(ASegmentedFile: TsgSegmentedFile; ASegmentIndex: Cardinal); virtual;
    class function CreateFromFileSegmentHeader(ASegmentedFile: TsgSegmentedFile;
      const AHeader: TsgFileSegmentHeader; AOffset: UInt64): TsgCustomSegment;
    destructor Destroy; override;
    procedure Clear; virtual;
    property SegmentedFile: TsgSegmentedFile read FSegmentedFile;
    property SegmentIndex: Cardinal read GetSegmentIndex;
    property SegmentSize: Cardinal read GetSegmentSize;
    property Offset: UInt64 read GetOffset write SetOffset;
    property Align: Cardinal read GetAlign;
  end;

  PsgSegIdxFileEntry = ^TsgSegIdxFileEntry;
  TsgSegIdxFileEntry = packed record
    Offset: UInt64;
    Size: Cardinal;
  end;
  PsgSegIdxFileEntryArray = ^TsgSegIdxFileEntryArray;
  TsgSegIdxFileEntryArray = array[Byte] of TsgSegIdxFileEntry;

  TsgSegIdx = class(TsgCustomSegment)
  protected
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffest: UInt64); override;
    class function GetSegName: TsgSegName; override;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; override;
  end;

  TsgDSItem = class
  private
    FSegmentIndex: Cardinal;
    function GetItemSize: Cardinal; virtual;
  public
    property SegmentIndex: Cardinal read FSegmentIndex write FSegmentIndex;
  end;

  TsgDataItem = class(TsgDSItem)
  protected
    FHandle: UInt64;
    FStream: TMemoryStream;
    FDataBlobReference: PsgDataBlobReference;
    function GetItemSize: Cardinal; override;
    procedure DataBlobReferenceNeeded;
    function GetEntries: PsgDataBlobReferencePageArray;
  public
    constructor Create;
    destructor Destroy; override;
    function HasBlobReference: Boolean;
    property Stream: TMemoryStream read FStream write FStream;
    property Handle: UInt64 read FHandle write FHandle;
  end;

  TsgACISItem = class(TsgDataItem)
  end;

  TsgPNGItem = class(TsgDataItem)
  end;

  TsgDatIdx = class(TsgCustomSegment)
  protected
    class function GetSegName: TsgSegName; override;
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffest: UInt64); override;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; override;
  end;

  TsgBlob01 = class(TsgCustomSegment)
  private
    function GetPageStart: PByte;
  protected
    FPageIndex: Integer;
    FPageDataSize: UInt64;
    FDataItem: TsgDataItem;
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffest: UInt64); override;
    class function GetSegName: TsgSegName; override;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; override;
  end;

	TsgDatSeg = class(TsgCustomSegment)
  protected
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffest: UInt64); override;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; override;
    class function GetSegName: TsgSegName; override;
  end;

  TsgCustomSchemaSegment = class(TsgCustomSegment)
  private
    function GetNamesSize: Integer;
  protected
    FNames: array of AnsiString;
    procedure ReadNames(Stream: TStream);
    procedure WriteNames(Stream: TStream; AOffset: UInt64);
    procedure InitializeDefault; virtual; abstract;
  public
    procedure Clear; override;
  end;

  TsgSchIdx = class(TsgCustomSchemaSegment)
  protected
    FUnkPropEntries: array of TsgSchIdxUnkPropHeaderEntry;
    FPropEntries: array of TsgSchIdxPropHeaderEntry;
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffset: UInt64); override;
    class function GetSegName: TsgSegName; override;
    procedure InitializeDefault; override;
  public
    procedure Clear; override;
  end;

  TsgSchemaPropertyEntry = record
    Flags: Cardinal; //1 = Unknown 1 (if set then all other bits are cleared).
                     //2 = Has no type.
                     //8 = Unknown 2 (if set then all other bits are cleared).
    NameIndex: Cardinal; //Name index. Index into a property names array in the schema data file segment.
                         //In a DXF file the name is directly written instead of indirectly through a table lookup.
    PropType: Cardinal;// 0-15
    TypeSize: Cardinal;
    Unknown1: Cardinal;// if Flags == 1
    Unknown2: Cardinal;// if Flags == 8
    PropValueCount: Word;
    Values: array of TBytes;
  end;

  TsgSchemaProperty = class(TsgDSItem)
  protected
    FEntries: array of TsgSchemaPropertyEntry;
    FIndices: array of UInt64;
    function AddEntry(AFlags, ANameIndex, APropType, ATypeSize,
      AUnknown1, AUnknown2: Cardinal; APropValueCount: Integer;
      const AValues): Integer;
    procedure Write(Stream: TStream; AOffset: UInt64);
    function GetItemSize: Cardinal; override;
  end;

  TsgSchDat = class(TsgCustomSchemaSegment)
  private
    procedure ReadProperties(Stream: TStream;
      const AUnkPropEntries: array of TsgSchIdxUnkPropHeaderEntry;
      const APropEntries: array of TsgSchIdxPropHeaderEntry);
  protected
    FSchDatPropEntry: array of TsgSchDatUnkPropEntry;
    FProps: array of TsgSchemaProperty;
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffset: UInt64); override;
    class function GetSegName: TsgSegName; override;
    procedure InitializeDefault; override;
  public
    procedure Clear; override;
  end;

  TsgSearch = class(TsgCustomSegment)
  private
    procedure WriteSchema(Stream: TStream);
  protected
    procedure Read(Stream: TStream); override;
    procedure Write(Stream: TStream; AOffset: UInt64); override;
    procedure ReadData(Stream: TStream);
    class function GetSegName: TsgSegName; override;
    function CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal; override;
  end;

  TsgPrvSav = class(TsgCustomSegment)
  protected
    class function GetSegName: TsgSegName; override;
  end;

  TsgFreeSp = class(TsgCustomSegment)
  protected
    class function GetSegName: TsgSegName; override;
  end;

  TsgCustomSegmentClass = class of TsgCustomSegment;

  TsgSegments = array of TsgCustomSegment;

  TsgSegmentedFile = class
  private
    FConverter: TsgDXFConverter;
    FSegIdxOffset: Integer;
		FSchIdxIndex: Integer;
		FDatIdxIndex: Integer;
		FSearchIndex: Integer;
		FPrvsavIndex: Integer;
    FSchDatIndex: Integer;// internal for default write
    FSegIdxSegment: TsgSegIdx;
    FSchIdxSegment: TsgSchIdx;
    FDatIdxSegment: TsgDatIdx;
    FSearchSegment: TsgSearch;
    FActiveDataSegmentIndex: Integer;// internal for write
    FStream: TMemoryStream;
    FSegments: TsgSegments;
    FDataItems: TsgObjectList;
    FDatIdxEntries: array of TsgDatIdxEntry;
    function CheckOffset(const Offset: UInt64): Boolean;
    function GetDataEntryType(Mem: Pointer; Size: Cardinal): TsgDSDataEntryType;
    procedure ReadHeader;
    function ReadFileSegmentHeder(var ASegmentHeader: TsgFileSegmentHeader; const AOffset: UInt64): Boolean;
    procedure LinkSegments;
    procedure ReadSegments;
    procedure ReadSegIdxSegment;
    procedure UpdateDataItemsType;
    procedure AddSegment(ASegment: TsgCustomSegment);
    function CompareDataItemHandle(const Item1, Item2: Pointer): Integer;
    procedure InitializeDefaultSegments;
    property Segments: TsgSegments read FSegments;
  public
    constructor Create(Converter: TsgDXFConverter);
    constructor CreateFromMemory(Converter: TsgDXFConverter; Mem: Pointer;
      Size: Cardinal);
    destructor Destroy; override;
    procedure Clear;
    procedure ReadFile;
    procedure WriteData(Stream: TStream);
    procedure AddData(AData: TStream; AHandle: UInt64);
    function IndexOf(const Handle: UInt64): Integer;
    property DataItems: TsgObjectList read FDataItems;
  end;

implementation

const
  cntSegIndName:    TsgSegName = ('s', 'e', 'g', 'i', 'd', 'x');
  cntDatIndName:    TsgSegName = ('d', 'a', 't', 'i', 'd', 'x');
  cntDataName:      TsgSegName = ('_', 'd', 'a', 't', 'a', '_');
  cntSchIndName:    TsgSegName = ('s', 'c', 'h', 'i', 'd', 'x');
  cntSchDataName:   TsgSegName = ('s', 'c', 'h', 'd', 'a', 't');
  cntSearchName:    TsgSegName = ('s', 'e', 'a', 'r', 'c', 'h');
  cntBlob01Name:    TsgSegName = ('b', 'l', 'o', 'b', '0', '1');
  cntPrvsavName:    TsgSegName = ('p', 'r', 'v', 's', 'a', 'v');
  cntFreespName:    TsgSegName = ('f', 'r', 'e', 'e', 's', 'p');

  cnstUInt32Zero: Cardinal = 0;
  cnstUInt32One: Cardinal = 1;
  cnstUInt64One: UInt64 = 1;
  cntFileSignature = $6472616A;
  cntFileSegmentHederSignature = Word($D5AC);
  cntSchIdxUnk: UInt64 = $0af10c;
  cntMaxDataSize = $40000;
  cntMaxPageSize = Cardinal($FFFFFFFF);// $fffb0; // controls blob01 pages count!
  cntBlobPersistMarker = Cardinal($BB106BB1);
  cnstAlignmentBytes = UInt64($5555555555555555);

  cnstSegAlign = 128;

  cnstHeaderAlignValue: Byte = $55;
  cnstDataAlignValue: Byte = $62;
  cnstSegAlignValue: Byte = $70;
  cnstSchAlignValue: Byte = $73;

  cntTypeSize: array[0 .. 15] of Byte = (0, 0, 2, 1, 2, 4, 8, 1, 2, 4, 8, 4, 8, 0, 0, 0);

  CustomSegmentClasses: array[TsgSegIdxEntryType] of TsgCustomSegmentClass = (
    TsgCustomSegment, TsgSegIdx, TsgDatIdx, TsgDatSeg, TsgSchIdx, TsgSchDat,
    TsgSearch, TsgBlob01, TsgPrvSav, TsgFreeSp);

  cntDefSchIdxNames: array[0 .. 4] of AnsiString = (cnstAcDb3DSolid_ASM_Data,
    cnstAcDbDs_TreatedAsObjectDataSchema, cnstAcDbDs_LegacySchema,
    cnstAcDbDs_IndexedPropertySchema, cnstAcDbDs_HandleAttributeSchema);
  cntDefSchDatNames: array[0 .. 5] of AnsiString = (cnstAcDbDs_ID, cnstASM_Data,
    cnstAcDbDs_TreatedAsObjectData, cnstAcDbDs_Legacy, cnstAcDs_Indexable,
    cnstAcDbDs_HandleAttribute);

  cntDatSegMaxSize = 3500000; // empirical!

type
  TSearchID = packed record
    Handle: UInt64;
    Count: UInt64;
    Index: array[0 .. 0] of UInt64;
  end;

function GetSegmentClass(const AName: TsgSegName): TsgCustomSegmentClass;
var
  I: TsgSegIdxEntryType;
begin
  Result := nil;
  I := Low(CustomSegmentClasses);
  while (I <= High(CustomSegmentClasses)) and not Assigned(Result) do
  begin
    if CustomSegmentClasses[I].GetSegName = AName then
      Result := CustomSegmentClasses[I]
    else
      Inc(I)
  end;
end;

function AlignSize(ABytes, AAlignment: Integer): Integer;
begin
  Result := ABytes div AAlignment;
  Result := Result * AAlignment + AAlignment * Ord(ABytes and (AAlignment - 1) <> 0);
end;

function GetSegIndType(const SegName: TsgSegName): TsgSegIdxEntryType;

  function CompareName(const Name: TsgSegName): Boolean;
  begin
    Result := CompareMem(@SegName, @Name, SizeOf(TsgSegName));
  end;

begin
  Result := stUnknown;
  if CompareName(cntSegIndName) then
    Result := stSegInd
  else
    if CompareName(cntDatIndName) then
      Result := stDatInd
    else
      if CompareName(cntDataName) then
        Result := stData
      else
        if CompareName(cntSchIndName) then
          Result := stSchInd
        else
          if CompareName(cntSchDataName) then
            Result := stSchData
          else
            if CompareName(cntSearchName) then
              Result := stSearch
            else
              if CompareName(cntBlob01Name) then
                Result := stBlob01
              else
                if CompareName(cntPrvsavName) then
                  Result := stPrvsav
                else
                  if CompareName(cntFreespName) then
                    Result := stFreesp;
end;

{ TsgSegmentedFile }

procedure TsgSegmentedFile.AddData(AData: TStream; AHandle: UInt64);
var
  I: Integer;
  vItem: TsgDataItem;
  vEntries: PsgDataBlobReferencePageArray;
  vBlob01: TsgBlob01;
  vDatSeg: TsgDatSeg;
begin
  vItem := TsgACISItem.Create;
  vItem.Stream.CopyFrom(AData, 0);
  vItem.Handle := AHandle;

  if FActiveDataSegmentIndex = 0 then
  begin
    if Length(FSegments) = 0 then
      InitializeDefaultSegments;
    vDatSeg := TsgDatSeg.Create(Self, Length(FSegments));
    AddSegment(vDatSeg);
    FActiveDataSegmentIndex := vDatSeg.FSegmentIndex;
  end;
  vItem.DataBlobReferenceNeeded;
  if vItem.HasBlobReference then
  begin
    vEntries := vItem.GetEntries;
    for I := 0 to Integer(vItem.FDataBlobReference^.PageCount) - 1 do
    begin
      vBlob01 := TsgBlob01.Create(Self, Length(FSegments));
      AddSegment(vBlob01);
      vEntries^[I].SegmentIndex := vBlob01.FSegmentIndex;
      vBlob01.FPageIndex := I;
      vBlob01.FPageDataSize := vItem.FDataBlobReference^.PageSize;
      vBlob01.FDataItem := vItem;
    end;
  end;
  FDataItems.Add(vItem);
  vItem.SegmentIndex := FActiveDataSegmentIndex;
  if FSegments[FActiveDataSegmentIndex].CalcSegmentNotAlignedSize > cntDatSegMaxSize then //cntMaxDataSize
  begin
    vDatSeg := TsgDatSeg.Create(Self, Length(FSegments));
    AddSegment(vDatSeg);
    FActiveDataSegmentIndex := vDatSeg.FSegmentIndex;
    vItem.SegmentIndex := FActiveDataSegmentIndex;
end;
end;

procedure TsgSegmentedFile.AddSegment(ASegment: TsgCustomSegment);
begin
  if Integer(ASegment.SegmentIndex) >= Length(FSegments) then
    SetLength(FSegments, ASegment.SegmentIndex + 1);
  FSegments[ASegment.SegmentIndex] := ASegment;
end;

function TsgSegmentedFile.CheckOffset(const Offset: UInt64): Boolean;
begin
  Result := (Offset > 0) and (Offset < FStream.Size);
end;

procedure TsgSegmentedFile.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FSegments) do
    FreeAndNil(FSegments[I]);
  SetLength(FSegments, 0);
  FSegIdxSegment := nil;
  FDatIdxSegment := nil;
  FSchIdxSegment := nil;
  FSearchSegment := nil;
  FDataItems.Clear;
  SetLength(FDatIdxEntries, 0);
  FActiveDataSegmentIndex := 0;
end;

function TsgSegmentedFile.CompareDataItemHandle(const Item1, Item2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpUInt64(TsgDataItem(Item1^).Handle, TsgDataItem(Item2^).Handle);
end;

constructor TsgSegmentedFile.Create(Converter: TsgDXFConverter);
begin
  FConverter := Converter;
  FDataItems := TsgObjectList.Create(True);
  FDataItems.ProcCompare := CompareDataItemHandle;
  FStream := TMemoryStream.Create;
end;

constructor TsgSegmentedFile.CreateFromMemory(Converter: TsgDXFConverter;
  Mem: Pointer; Size: Cardinal);
begin
  Create(Converter);
  FStream.Write(Mem^, Size);
  FStream.Position := 0;
end;

destructor TsgSegmentedFile.Destroy;
begin
  FStream.Free;
  Clear;
  FDataItems.Free;
  inherited Destroy;
end;

function TsgSegmentedFile.GetDataEntryType(Mem: Pointer; Size: Cardinal): TsgDSDataEntryType;
const
  cntPNG: AnsiString = '‰PNG';
begin
  Result := etUnknown;
  if IsSatBinaryFile(Mem, Size) then
    Result := etACISBin
  else
    if (Size >= Cardinal(Length(cntPNG))) and CompareMem(Pointer(cntPNG), Mem, Length(cntPNG)) then
      Result := etPNG;
end;

function TsgSegmentedFile.IndexOf(const Handle: UInt64): Integer;
var
  vDataItem: TsgDataItem;
begin
  vDataItem := TsgDataItem.Create;
  try
    vDataItem.Handle := Handle;
    Result := FDataItems.IndexOf(vDataItem);
  finally
    vDataItem.Free;
  end;
end;

procedure TsgSegmentedFile.InitializeDefaultSegments;
const
  cntSegClasses: array[1 .. 5] of TsgCustomSegmentClass = (TsgSegIdx,
    TsgDatIdx, TsgSchDat, TsgSchIdx, TsgSearch);
var
  I: Integer;
begin
  SetLength(FSegments, High(cntSegClasses) + 1);
  for I := Low(cntSegClasses) to High(cntSegClasses) do
    FSegments[I] := cntSegClasses[I].Create(Self, I);
  LinkSegments;
  FSchDatIndex := 3;
  // SchIdx
  // FPropEntries ans FUnkPropEntries swap in doc!
  FSchIdxSegment.InitializeDefault;  // SchDat

  TsgSchDat(FSegments[FSchDatIndex]).InitializeDefault;

  FSchIdxIndex := FSchIdxSegment.SegmentIndex;
  FDatIdxIndex := FDatIdxSegment.SegmentIndex;
  if Assigned(FSearchSegment) then
    FSearchIndex := FSearchSegment.SegmentIndex
  else
    FSearchIndex := 0;
    end;

procedure TsgSegmentedFile.LinkSegments;
var
  I: Integer;
begin
  for I := Low(FSegments) to High(FSegments) do
    if not Assigned(FSegIdxSegment) and (FSegments[I] is TsgSegIdx) then
      FSegIdxSegment := TsgSegIdx(FSegments[I]) else
    if not Assigned(FDatIdxSegment) and (FSegments[I] is TsgDatIdx) then
      FDatIdxSegment := TsgDatIdx(FSegments[I]) else
    if not Assigned(FSchIdxSegment) and (FSegments[I] is TsgSchIdx) then
      FSchIdxSegment := TsgSchIdx(FSegments[I]) else
    if not Assigned(FSearchSegment) and (FSegments[I] is TsgSearch) then
      FSearchSegment := TsgSearch(FSegments[I]);
  end;

procedure TsgSegmentedFile.ReadFile;
  begin
  Clear;
  if FStream.Size = 0 then
    raise TsgDataSegFileError.Create(sDSFileIsNotInitializedError);
  ReadHeader;
  ReadSegIdxSegment;
  LinkSegments;
  ReadSegments;
  UpdateDataItemsType;
  FDataItems.Sorted := True;
  FStream.Clear;
        end;

function TsgSegmentedFile.ReadFileSegmentHeder(
  var ASegmentHeader: TsgFileSegmentHeader; const AOffset: UInt64): Boolean;
        begin
  Result := False;
  if CheckOffset(AOffset) then
        begin
    FStream.Position := AOffset;
    if FStream.Read(ASegmentHeader, SizeOf(TsgFileSegmentHeader)) = SizeOf(TsgFileSegmentHeader) then
      Result := (ASegmentHeader.Signature = cntFileSegmentHederSignature) and
        (PUInt64(@ASegmentHeader.AlignmentBytes)^ = cnstAlignmentBytes);
            end;
          end;

procedure TsgSegmentedFile.ReadHeader;
var
  vHeader: TsgSegmentedFileHeader;
begin
  FStream.Position := 0;
  FStream.Read(vHeader, SizeOf(TsgSegmentedFileHeader));
  FSegIdxOffset := vHeader.SegIdxOffset;
  SetLength(FSegments, vHeader.SegIdxEntryCount);
  FSchIdxIndex := vHeader.SchIdxIndex;
  FDatIdxIndex := vHeader.DatIdxIndex;
  FPrvsavIndex := vHeader.PrvsavIndex;
  FSearchIndex := vHeader.SearchIndex;
  end;

procedure TsgSegmentedFile.ReadSegIdxSegment;
var
  I: Integer;
  vEntries: array of TsgSegIdxEntry;
  vHeader: TsgFileSegmentHeader;
  vSegment: TsgCustomSegment;
begin
  if not CheckOffset(FSegIdxOffset) then Exit;
  if not ReadFileSegmentHeder(vHeader, FSegIdxOffset) then Exit;
  FSegIdxSegment := TsgSegIdx(TsgCustomSegment.CreateFromFileSegmentHeader(Self, vHeader, FSegIdxOffset));
  AddSegment(FSegIdxSegment);
  SetLength(vEntries, Length(Segments));
  if FStream.Read(vEntries[0], Length(vEntries) * SizeOf(vEntries[0])) = Length(vEntries) * SizeOf(vEntries[0]) then
    for I := Low(vEntries) to High(vEntries) do
  begin
      if (vEntries[I].Offset <> 0) and (vEntries[I].Offset <> FSegIdxOffset) then
    begin
        if not ReadFileSegmentHeder(vHeader, vEntries[I].Offset) then Exit;
        vSegment := TsgCustomSegment.CreateFromFileSegmentHeader(Self, vHeader, vEntries[I].Offset);
        AddSegment(vSegment);
    end;
  end;
end;

procedure TsgSegmentedFile.ReadSegments;
var
  I: Integer;
  begin
  for I := Low(FSegments) to High(FSegments) do
    if Assigned(FSegments[I]) then
      FSegments[I].Read(FStream);
  end;

procedure TsgSegmentedFile.WriteData(Stream: TStream);
const
  cnstSaveSegmentOrder: array[1 .. 5] of Integer = (2, 3, 4, 5, 1);
var
  I: Integer;
  vHeaderStreamPosition: Int64;
  vHeader: PsgSegmentedFileHeader;
  vHeaderSize: Integer;
begin
  FDataItems.Sorted := True;
  if FDataItems.Count = 0 then Exit;
  vHeaderStreamPosition := Stream.Position;
  vHeaderSize := AlignSize(SizeOf(TsgSegmentedFileHeader), cnstSegAlign);
  GetMem(vHeader, vHeaderSize);
  FillChar(vHeader^, vHeaderSize, 0);
  vHeader^.FileSignature := cntFileSignature;
  vHeader^.FileHeaderSize := vHeaderSize;
  vHeader^.Unknown3 := 2;
  vHeader^.Version := 2;
  vHeader^.Unknown5 := 0;
  vHeader^.DsRevision := 1;
  vHeader^.SegIdxOffset := Integer($FFFFFFFF);
  vHeader^.SegIdxUnknown := 0;
  vHeader^.SegIdxEntryCount := Length(Segments);
  vHeader^.SchIdxIndex := FSchIdxIndex;
  vHeader^.DatIdxIndex := FDatIdxIndex;
  vHeader^.SearchIndex := FSearchIndex;
  vHeader^.PrvsavIndex := 0;
  vHeader^.FileSize := Integer($FFFFFFFF);

  Stream.Write(vHeader^, vHeaderSize);

  for I := 6 to High(FSegments) do
    FSegments[I].Write(Stream, 0);
  for I := Low(cnstSaveSegmentOrder) to High(cnstSaveSegmentOrder) do
    FSegments[cnstSaveSegmentOrder[I]].Write(Stream, 0);

  vHeader^.SegIdxOffset := FSegIdxSegment.Offset;
  vHeader^.SegIdxEntryCount := Length(Segments);
  vHeader^.FileSize := Stream.Position - vHeaderStreamPosition;
  Stream.Position := vHeaderStreamPosition;
  Stream.Write(vHeader^, vHeaderSize);// update SegIdxOffset and FileSize;

  FreeMem(vHeader);
  end;

procedure TsgSegmentedFile.UpdateDataItemsType;
var
  I: Integer;
  vDataItem: TsgDataItem;
begin
  for I := 0 to FDataItems.Count - 1 do
  begin
    vDataItem := TsgDataItem(FDataItems[I]);
    case GetDataEntryType(vDataItem.Stream.Memory, vDataItem.Stream.Size) of
      etACISBin: TClass(Pointer(vDataItem)^) := TsgACISItem;
      etPNG: TClass(Pointer(vDataItem)^) := TsgPNGItem;
  end;
end;
end;

{ TsgCustomSegment }

function TsgCustomSegment.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
begin
  Result := SizeOf(TsgFileSegmentHeader);
end;

procedure TsgCustomSegment.Clear;
begin
  FReaded := False;
end;

constructor TsgCustomSegment.Create(ASegmentedFile: TsgSegmentedFile; ASegmentIndex: Cardinal);
begin
  FSegmentIndex := ASegmentIndex;
  FSegmentedFile := ASegmentedFile;
  Clear;
end;

class function TsgCustomSegment.CreateFromFileSegmentHeader(ASegmentedFile: TsgSegmentedFile;
  const AHeader: TsgFileSegmentHeader; AOffset: UInt64): TsgCustomSegment;
var
  vSegmentClass: TsgCustomSegmentClass;
begin
  vSegmentClass := GetSegmentClass(AHeader.Name);
  Result := vSegmentClass.Create(ASegmentedFile, AHeader.SegmentIndex);
  if Result is TsgCustomSchemaSegment then
    Result.FAlign := AHeader.SysDataAlignOffset
  else
    Result.FAlign := AHeader.ObjDataAlignOffset;
  Result.FSegmentSize := AHeader.SegmentSize;
  Result.Offset := AOffset;
end;

destructor TsgCustomSegment.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TsgCustomSegment.FillDefautFileHeader(
  var AHeader: TsgFileSegmentHeader);
begin
  FillChar(AHeader, SizeOf(TsgFileSegmentHeader), 0);
  AHeader.Signature := cntFileSegmentHederSignature;
  AHeader.Name := GetSegName;
  AHeader.SegmentIndex := SegmentIndex;
  AHeader.DataStorageRevision := 1;
  FillChar(AHeader.AlignmentBytes, SizeOf(AHeader.AlignmentBytes), cnstHeaderAlignValue);
end;

function TsgCustomSegment.GetAlign: Cardinal;
begin
  Result := FAlign;
end;

function TsgCustomSegment.GetOffset: UInt64;
begin
  Result := FOffset;
end;

function TsgCustomSegment.GetSegmentIndex: Cardinal;
begin
  Result := FSegmentIndex;
end;

function TsgCustomSegment.GetSegmentSize: Cardinal;
begin
  Result := FSegmentSize;
end;

class function TsgCustomSegment.GetSegName: TsgSegName;
begin
  FillChar(Result, SizeOf(TsgSegName), 0);
end;

procedure TsgCustomSegment.Read(Stream: TStream);
begin
  FReaded := True;
end;

procedure TsgCustomSegment.SetOffset(const AValue: UInt64);
begin
  FOffset := AValue;
end;

procedure TsgCustomSegment.Write(Stream: TStream; AOffset: UInt64);
begin
end;

{ TsgDataSegment }

function TsgDatSeg.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
var
  I, K: Integer;
  vDataItem: TsgDataItem;
  vEntriesSizeNA: Cardinal;
begin
  if ASysEntriesSize = nil then ASysEntriesSize := PSGCardinal(@vEntriesSizeNA);
  Result := 0;
  K := 0;
  for I := 0 to SegmentedFile.DataItems.Count - 1 do
  begin
    vDataItem := TsgDataItem(SegmentedFile.DataItems[I]);
    if vDataItem.SegmentIndex = SegmentIndex then
    begin
      Inc(Result, vDataItem.GetItemSize);
      Inc(K);
end;
  end;
  ASysEntriesSize^ := SizeOf(TsgFileSegmentHeader) + K * SizeOf(TsgDatHeaderEntry);
  Inc(Result, AlignSize(ASysEntriesSize^, 16));
end;

class function TsgDatSeg.GetSegName: TsgSegName;
begin
  Result := cntDataName;
end;

procedure TsgDatSeg.Read(Stream: TStream);
var
  I, J: Integer;
  vDatHeaderEntry: TsgDatHeaderEntry;
  vMaxStreamPos, vDataStartPos: UInt64;
  vDataSize, vMaxRecordSize: Cardinal;
  vDataItem: TsgDataItem;
  vBlobReference: TsgDataBlobReference;
  vBlobReferencePages: TsgDataBlobReferencePages;
  vBlob01: TsgBlob01;
  vData: PByte;
  vDestPosition, vPageDataSize: UInt64;
begin
  if Assigned(SegmentedFile.FDatIdxSegment) then
    SegmentedFile.FDatIdxSegment.Read(Stream);
  for I := Low(SegmentedFile.FDatIdxEntries) to High(SegmentedFile.FDatIdxEntries) do
  begin
    if SegmentedFile.FDatIdxEntries[I].SegmentIndex = SegmentIndex then
    begin
      Stream.Position := Offset + SizeOf(TsgFileSegmentHeader) + SegmentedFile.FDatIdxEntries[I].LocalOffset;
      Stream.Read(vDatHeaderEntry, SizeOf(TsgDatHeaderEntry));

      vDataStartPos := Cardinal(Align shl 4) + Offset;
      vMaxStreamPos := Offset + SegmentSize;

      Stream.Position := vDataStartPos + vDatHeaderEntry.LocalOffset;
      vMaxRecordSize := vMaxStreamPos;
      Dec(vMaxRecordSize, Stream.Position);
      Stream.Read(vDataSize, 4);
      if vDataSize > 0 then
        if vDataSize + 4 <= vMaxRecordSize then
        begin
          vDataItem := TsgDataItem.Create;
          vDataItem.Stream.Size := vDataSize;
          Stream.ReadBuffer(vDataItem.Stream.Memory^, vDataSize);
          vDataItem.Stream.Position := 0;
          vDataItem.Handle := vDatHeaderEntry.Handle;
          vDataItem.SegmentIndex := SegmentIndex;
          SegmentedFile.FDataItems.Add(vDataItem);
        end
        else
        begin
          if vDataSize = cntBlobPersistMarker then
          begin
            vDataItem := TsgDataItem.Create;
            vDataItem.Handle := vDatHeaderEntry.Handle;
            vDataItem.SegmentIndex := SegmentIndex;
            SegmentedFile.FDataItems.Add(vDataItem);
            Stream.Read(vBlobReference, SizeOf(TsgDataBlobReference));
            try
              SetLength(vBlobReferencePages, vBlobReference.PageCount);
            except
              Continue;
            end;

            Stream.Read(vBlobReferencePages[0], Length(vBlobReferencePages) * SizeOf(vBlobReferencePages[0]));
            for J := Low(vBlobReferencePages) to High(vBlobReferencePages) do
            begin
              vBlob01 := TsgBlob01(SegmentedFile.Segments[vBlobReferencePages[J].SegmentIndex]);
              if Assigned(vBlob01) then
              begin
                vBlob01.Read(Stream);
                Stream.Position := vBlob01.Offset + SizeOf(TsgFileSegmentHeader) + SizeOf(TsgBlobEntryReferenceHeader);
                vDestPosition := vDataItem.Stream.Position;
                vPageDataSize := UInt64(vBlob01.FPageDataSize);
                vDataItem.Stream.Size := vDataItem.Stream.Size + vPageDataSize;
                vData := vDataItem.Stream.Memory;
                Inc(vData, vDestPosition);
                Stream.Read(vData^, vBlob01.FPageDataSize);
                vDataItem.Stream.Position := vDataItem.Stream.Size - 1;
              end;
            end;
            vDataItem.Stream.Position := 0;
          end;
      end;
    end;
  end;
end;

procedure TsgDatSeg.Write(Stream: TStream; AOffest: UInt64);
var
  vHeader: TsgFileSegmentHeader;
  I: Integer;
  vDatHeaderEntries: array of TsgDatHeaderEntry;
  vDataSize: Cardinal;
  vSegHeaderWithEntriesSizeNA: Cardinal;
  vSegSizeNA: Cardinal;
  vDataItem, vPrevDataItem: TsgDataItem;
begin
  vSegSizeNA := CalcSegmentNotAlignedSize(@vSegHeaderWithEntriesSizeNA);

  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, cnstSegAlign);
  vHeader.ObjDataAlignOffset := AlignSize(vSegHeaderWithEntriesSizeNA, 16) shr 4;
  FSegmentSize := vHeader.SegmentSize;

  vPrevDataItem := nil;
  SetLength(vDatHeaderEntries, 0);
  for I := 0 to SegmentedFile.DataItems.Count - 1 do
  begin
    vDataItem := TsgDataItem(SegmentedFile.DataItems[I]);
    if vDataItem.SegmentIndex = SegmentIndex then
    begin
      SetLength(vDatHeaderEntries, Length(vDatHeaderEntries) + 1);
      vDatHeaderEntries[High(vDatHeaderEntries)].EntrySize := SizeOf(TsgDatHeaderEntry);
      vDatHeaderEntries[High(vDatHeaderEntries)].Unknown := 1;
      vDatHeaderEntries[High(vDatHeaderEntries)].Handle := vDataItem.Handle;
      if Assigned(vPrevDataItem) then
        vDatHeaderEntries[High(vDatHeaderEntries)].LocalOffset := vDatHeaderEntries[High(vDatHeaderEntries)-1].LocalOffset + vPrevDataItem.GetItemSize;
      vPrevDataItem := vDataItem;
end;
  end;

  Offset := Stream.Position;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  Stream.Write(vDatHeaderEntries[0], Length(vDatHeaderEntries) * SizeOf(TsgDatHeaderEntry));
  for I := vSegHeaderWithEntriesSizeNA to vHeader.ObjDataAlignOffset shl 4 - 1 do
    Stream.Write(cnstDataAlignValue, SizeOf(cnstDataAlignValue));
  for I := 0 to SegmentedFile.DataItems.Count - 1 do
begin
    vDataItem := TsgDataItem(SegmentedFile.DataItems[I]);
    if vDataItem.SegmentIndex = SegmentIndex then
      if vDataItem.HasBlobReference then
      begin
        vDataSize := cntBlobPersistMarker;
    Stream.Write(vDataSize, SizeOf(Cardinal));
        Stream.Write(vDataItem.FDataBlobReference^, vDataItem.GetItemSize - SizeOf(Cardinal));
      end
      else
      begin
        vDataSize := vDataItem.Stream.Size;
        Stream.Write(vDataSize, SizeOf(Cardinal));
        Stream.Write(vDataItem.Stream.Memory^, vDataSize);
      end;
  end;
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

{ TsgSegIdx }

function TsgSegIdx.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
begin
  Result := SizeOf(TsgFileSegmentHeader) + Length(SegmentedFile.Segments) * SizeOf(TsgSegIdxFileEntry);
end;

class function TsgSegIdx.GetSegName: TsgSegName;
begin
  Result := cntSegIndName;
end;

procedure TsgSegIdx.Read(Stream: TStream);
begin
end;

procedure TsgSegIdx.Write(Stream: TStream; AOffest: UInt64);
var
  I: Integer;
  vHeader: TsgFileSegmentHeader;
  vSegSizeNA: Cardinal;
  vSegIdxEntries: array of TsgSegIdxEntry;
begin
  vSegSizeNA := CalcSegmentNotAlignedSize;
  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, $40{cnstSegAlign});
  FSegmentSize := vHeader.SegmentSize;

  Offset := Stream.Position;
  SetLength(vSegIdxEntries, Length(SegmentedFile.Segments));
  for I := Low(SegmentedFile.Segments) to High(SegmentedFile.Segments) do
    if Assigned(SegmentedFile.Segments[I]) then
    begin
      vSegIdxEntries[I].Offset := SegmentedFile.Segments[I].Offset;
      if SegmentedFile.Segments[I] is TsgBlob01 then
        vSegIdxEntries[I].Size := AlignSize(SegmentedFile.Segments[I].SegmentSize + TsgBlob01(SegmentedFile.Segments[I]).FPageDataSize, cnstSegAlign)
      else
        vSegIdxEntries[I].Size := SegmentedFile.Segments[I].SegmentSize;
end;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  Stream.Write(vSegIdxEntries[0], Length(vSegIdxEntries) * SizeOf(TsgSegIdxEntry));
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

{ TsgDatIdx }

function TsgDatIdx.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
begin
  Result := SizeOf(TsgFileSegmentHeader) + SizeOf(TsgDatIdxEntry) * SegmentedFile.DataItems.Count +
    2 * SizeOf(Integer); // count + unknown
end;

class function TsgDatIdx.GetSegName: TsgSegName;
  begin
  Result := cntDatIndName;
  end;

procedure TsgDatIdx.Read(Stream: TStream);
var
  vNumEntries, vValue: Integer;
begin
  if FReaded then Exit;
  Stream.Position := Offset + SizeOf(TsgFileSegmentHeader);
  if Stream.Read(vNumEntries, SizeOf(vNumEntries)) < SizeOf(vNumEntries) then Exit;
  if Stream.Read(vValue, SizeOf(vValue)) < SizeOf(vValue) then Exit;
  SetLength(SegmentedFile.FDatIdxEntries, vNumEntries);
  Stream.Read(SegmentedFile.FDatIdxEntries[0], vNumEntries * SizeOf(TsgDatIdxEntry));
  FReaded := True;
end;

procedure TsgDatIdx.Write(Stream: TStream; AOffest: UInt64);
var
  vHeader: TsgFileSegmentHeader;
  I, vItemsCount: Integer;
  vSegSizeNA: Cardinal;
  vDatIdxEntries: array of TsgDatIdxEntry;
begin
  vSegSizeNA := CalcSegmentNotAlignedSize;
  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, cnstSegAlign);
  FSegmentSize := vHeader.SegmentSize;

  vItemsCount := SegmentedFile.DataItems.Count;
  SetLength(vDatIdxEntries, vItemsCount);
  for I := 0 to vItemsCount - 1 do
  begin
    vDatIdxEntries[I].SegmentIndex := TsgDataItem(SegmentedFile.DataItems[I]).SegmentIndex;
    vDatIdxEntries[I].LocalOffset := I * SizeOf(TsgDatHeaderEntry);
    vDatIdxEntries[I].SchemaIndex := 0;//!!!
end;

  Offset := Stream.Position;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  Stream.Write(vItemsCount, SizeOf(Integer));
  Stream.Write(cnstUInt32Zero, SizeOf(Cardinal));
  Stream.Write(vDatIdxEntries[0], vItemsCount * SizeOf(TsgDatIdxEntry));
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

{ TsgBlobEntriesList }

function TsgBlob01.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
begin
  Result := SizeOf(TsgFileSegmentHeader) + SizeOf(TsgBlobEntryReferenceHeader);
end;

function TsgBlob01.GetPageStart: PByte;
begin
  Result := PByte(IntPtr(FDataItem.Stream.Memory) + FPageIndex * Integer(FDataItem.FDataBlobReference^.PageSize));
end;

class function TsgBlob01.GetSegName: TsgSegName;
begin
  Result := cntBlob01Name;
end;

procedure TsgBlob01.Read(Stream: TStream);
var
  vBlobHeader: TsgBlobEntryReferenceHeader;
begin
  if FReaded then Exit;
  Stream.Position := Offset + SizeOf(TsgFileSegmentHeader);
  Stream.Read(vBlobHeader, SizeOf(TsgBlobEntryReferenceHeader));
  FPageIndex := vBlobHeader.PageIndex;
  FPageDataSize := vBlobHeader.PageDataSize;
  FReaded := True;
end;

procedure TsgBlob01.Write(Stream: TStream; AOffest: UInt64);
var
  vHeader: TsgFileSegmentHeader;
  vWrites: Cardinal;
  vBlobHeader: TsgBlobEntryReferenceHeader;
begin
  FillDefautFileHeader(vHeader);
  vHeader.Unknown1 := 1;
  vHeader.SegmentSize := SizeOf(TsgFileSegmentHeader);//!
  FSegmentSize := vHeader.SegmentSize;

  vBlobHeader.TotalDataSize := FDataItem.FDataBlobReference^.TotalDataSize;
  vBlobHeader.PageStartOffset := 0;
  vBlobHeader.PageIndex := FPageIndex;
  vBlobHeader.PageCount := Integer(FDataItem.FDataBlobReference^.PageCount);
  vBlobHeader.PageDataSize := FPageDataSize;

  Offset := Stream.Position;
  vWrites := Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  Inc(vWrites, Stream.Write(vBlobHeader, SizeOf(TsgBlobEntryReferenceHeader)));
  Inc(vWrites, Stream.Write(GetPageStart^, FDataItem.GetEntries^[FPageIndex].Size));

  while vWrites mod cnstSegAlign <> 0 do
    Inc(vWrites, Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue)));
end;

{ TsgSchIdx }

procedure TsgSchIdx.Clear;
begin
  inherited Clear;
  SetLength(FUnkPropEntries, 0);
  SetLength(FPropEntries, 0);
end;

class function TsgSchIdx.GetSegName: TsgSegName;
begin
  Result := cntSchIndName;
end;

procedure TsgSchIdx.InitializeDefault;
const
  cntUnkPropOffsets: array[0 .. 4] of Integer = (32, 96, 114, 132, 150);
var
  I: Integer;
begin
  SetLength(FNames, Length(cntDefSchIdxNames));
  for I := Low(cntDefSchIdxNames) to High(cntDefSchIdxNames) do
    FNames[I] := cntDefSchIdxNames[I];
  SetLength(FUnkPropEntries, Length(FNames));
  for I := Low(FUnkPropEntries) to High(FUnkPropEntries) do
  begin
    FUnkPropEntries[I].Index := I;
    FUnkPropEntries[I].SegmentIndex := SegmentedFile.FSchDatIndex;
    FUnkPropEntries[I].LocalOffset := cntUnkPropOffsets[I];
end;
  SetLength(FPropEntries, 4);
  for I := Low(FPropEntries) to High(FPropEntries) do
  begin
    FPropEntries[I].SegmentIndex := SegmentedFile.FSchDatIndex;
    FPropEntries[I].LocalOffset := I * SizeOf(TsgSchDatUnkPropEntry);
    FPropEntries[I].Index := I;
  end;
end;

procedure TsgSchIdx.Read(Stream: TStream);
var
  vUnknownPropCount, vUnknown0, vPropCount, vUnknown1: Cardinal;
  vUnkInt64: Int64;//Unknown (0x0af10c)
begin
  if FReaded then Exit;
  Stream.Position := Offset + SizeOf(TsgFileSegmentHeader);
  Stream.Read(vUnknownPropCount, SizeOf(vUnknownPropCount));
  Stream.Read(vUnknown0, SizeOf(vUnknown0));
  SetLength(FUnkPropEntries, vUnknownPropCount);
  Stream.Read(FUnkPropEntries[0], vUnknownPropCount * SizeOf(FUnkPropEntries[0]));
  Stream.Read(vUnkInt64, SizeOf(vUnkInt64));//Unknown (0x0af10c)

  Stream.Read(vPropCount, SizeOf(vPropCount));
  Stream.Read(vUnknown1, SizeOf(vUnknown1));
  SetLength(FPropEntries, vPropCount);
  Stream.Read(FPropEntries[0], vPropCount * SizeOf(FPropEntries[0]));

  ReadNames(Stream);
  FReaded := True;
end;

procedure TsgSchIdx.Write(Stream: TStream; AOffset: UInt64);
var
  I: Integer;
  vHeader: TsgFileSegmentHeader;
  vSegHeaderWithEntriesSizeNA, vSegHeaderWithEntriesSize: Integer;
  vSegSizeNA: Cardinal;
  vUnknownPropCount, vPropCount: Cardinal;
begin
  vSegHeaderWithEntriesSizeNA := SizeOf(TsgFileSegmentHeader) + 2 * SizeOf(Cardinal) +
    Length(FUnkPropEntries) * SizeOf(TsgSchIdxUnkPropHeaderEntry) + 2 * SizeOf(Cardinal) +
    Length(FPropEntries) * SizeOf(TsgSchIdxPropHeaderEntry) +
    SizeOf(cntSchIdxUnk);

  vSegHeaderWithEntriesSize := AlignSize(vSegHeaderWithEntriesSizeNA, 16);
  vSegSizeNA := vSegHeaderWithEntriesSize + GetNamesSize;

  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, cnstSegAlign);
  vHeader.SysDataAlignOffset := vSegHeaderWithEntriesSize shr 4;
  FSegmentSize := vHeader.SegmentSize;

  Offset := Stream.Position;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  vUnknownPropCount := Length(FUnkPropEntries);
  Stream.Write(vUnknownPropCount, SizeOf(Cardinal));
  Stream.Write(cnstUInt32Zero, SizeOf(Cardinal));
  if vUnknownPropCount > 0 then
    Stream.Write(FUnkPropEntries[0], vUnknownPropCount * SizeOf(TsgSchIdxUnkPropHeaderEntry));
  Stream.Write(cntSchIdxUnk, SizeOf(UInt64));

  vPropCount := Length(FPropEntries);
  Stream.Write(vPropCount, SizeOf(Cardinal));
  Stream.Write(cnstUInt32Zero, SizeOf(Cardinal));
  if vPropCount > 0 then
    Stream.Write(FPropEntries[0], vPropCount * SizeOf(TsgSchIdxPropHeaderEntry));
  for I := vSegHeaderWithEntriesSizeNA to vSegHeaderWithEntriesSize - 1 do
    Stream.Write(cnstSchAlignValue, SizeOf(cnstSchAlignValue));
  WriteNames(Stream, 0);
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

{ TsgSchDat }

procedure TsgSchDat.Clear;
var
  I: Integer;
begin
  inherited Clear;
  for I := Low(FProps) to High(FProps) do
    FreeAndNil(FProps[I]);
  SetLength(FProps, 0);
  SetLength(FSchDatPropEntry, 0);
end;

class function TsgSchDat.GetSegName: TsgSegName;
begin
  Result := cntSchDataName;
end;

procedure TsgSchDat.InitializeDefault;
const
  cnstVal1: array[0 .. 1] of UInt64 = (2, 3);
  cnstVal2: Byte = 0;
var
  I: Integer;
begin
  SetLength(FNames, Length(cntDefSchDatNames));
  for I := Low(cntDefSchDatNames) to High(cntDefSchDatNames) do
    FNames[I] := cntDefSchDatNames[I];
  SetLength(FSchDatPropEntry, 4);
  for I := Low(FSchDatPropEntry) to High(FSchDatPropEntry) do
  begin
    FSchDatPropEntry[I].DataSize := 8;
    FSchDatPropEntry[I].UnknownFlags := Ord(I <> High(FSchDatPropEntry));
end;
  SetLength(FProps, 5);
  for I := Low(FProps) to High(FProps) do
    FProps[I] := TsgSchemaProperty.Create;
  SetLength(FProps[0].FIndices, 2);
  FProps[0].FIndices[0] := 0;
  FProps[0].FIndices[1] := 1;
  FProps[0].AddEntry(0, 0, 10, 8, 0, 0, 2, cnstVal1[0]);
  FProps[0].AddEntry(0, 1, 15, 0, 0, 0, 0, nil^);
  FProps[1].AddEntry(0, 2, 1,  0, 0, 0, 0, nil^);
  FProps[2].AddEntry(0, 3, 1,  0, 0, 0, 0, nil^);
  FProps[3].AddEntry(0, 4, 1,  0, 0, 0, 0, nil^);
  FProps[4].AddEntry(8, 5, 7,  1, 0, 1, 1, cnstVal2);
end;

procedure TsgSchDat.Read(Stream: TStream);
begin
  if FReaded then Exit;
  if Assigned(SegmentedFile.FSchIdxSegment) then
  begin
    SegmentedFile.FSchIdxSegment.Read(Stream);
    ReadProperties(Stream, SegmentedFile.FSchIdxSegment.FUnkPropEntries,
      SegmentedFile.FSchIdxSegment.FPropEntries);
  end;
  ReadNames(Stream);
  FReaded := True;
end;

procedure TsgSchDat.ReadProperties(Stream: TStream;
  const AUnkPropEntries: array of TsgSchIdxUnkPropHeaderEntry;
  const APropEntries: array of TsgSchIdxPropHeaderEntry);
var
  I, J, K: Integer;
  vSchDatPos: Int64;
  vIndexCount, vSchPropCount: Word;
  vProp: TsgSchemaProperty;
  vSchPropEntry: TsgSchemaPropertyEntry;
begin
  vSchDatPos := Offset + SizeOf(TsgFileSegmentHeader);
  for I := 0 to High(APropEntries) do
  begin
    if SegmentIndex = APropEntries[I].SegmentIndex then
    begin
      Stream.Position := vSchDatPos + APropEntries[I].LocalOffset;
      SetLength(FSchDatPropEntry, Length(FSchDatPropEntry) + 1);
      Stream.Read(FSchDatPropEntry[High(FSchDatPropEntry)], SizeOf(TsgSchDatUnkPropEntry));
end;
  end;

  for J := 0 to High(AUnkPropEntries) do
begin
    if SegmentIndex = AUnkPropEntries[J].SegmentIndex then
    begin
      Stream.Position := vSchDatPos + AUnkPropEntries[J].LocalOffset;
      Stream.Read(vIndexCount, SizeOf(Word));
      vProp := TsgSchemaProperty.Create;
      vProp.SegmentIndex := SegmentIndex;
      SetLength(FProps, Length(FProps) + 1);
      FProps[High(FProps)] := vProp;
      if vIndexCount > 0 then
      begin
        SetLength(vProp.FIndices, vIndexCount);
        Stream.Read(vProp.FIndices[0], vIndexCount * SizeOf(vProp.FIndices[0]));
end;
      Stream.Read(vSchPropCount, SizeOf(Word));
      for K := 0 to vSchPropCount - 1 do
      begin
        FillChar(vSchPropEntry, SizeOf(vSchPropEntry), 0);
        Stream.Read(vSchPropEntry.Flags, SizeOf(Cardinal));
        Stream.Read(vSchPropEntry.NameIndex, SizeOf(Cardinal));

        vSchPropEntry.TypeSize := 0;
        if vSchPropEntry.Flags and 2 = 0 then
begin
          Stream.Read(vSchPropEntry.PropType, SizeOf(Cardinal));
          case vSchPropEntry.PropType of
            $e: Stream.Read(vSchPropEntry.TypeSize, SizeOf(Cardinal));
          else
            vSchPropEntry.TypeSize := cntTypeSize[vSchPropEntry.PropType];
end;
        end;

        if vSchPropEntry.Flags = 1 then
          Stream.Read(vSchPropEntry.Unknown1, SizeOf(Cardinal))
        else
          if vSchPropEntry.Flags = 8 then
            Stream.Read(vSchPropEntry.Unknown2, SizeOf(Cardinal));

        Stream.Read(vSchPropEntry.PropValueCount, SizeOf(Word));
        SetLength(vSchPropEntry.Values, vSchPropEntry.PropValueCount);
        I := 0;
        while I < vSchPropEntry.PropValueCount do
        begin
          if vSchPropEntry.TypeSize <> 0 then
          begin
            SetLength(vSchPropEntry.Values[I], vSchPropEntry.TypeSize);
            Stream.Read(vSchPropEntry.Values[I][0], vSchPropEntry.TypeSize);
          end;
          Inc(I);
        end;
        SetLength(vProp.FEntries, Length(vProp.FEntries) + 1);
        vProp.FEntries[High(vProp.FEntries)] := vSchPropEntry;
        Finalize(vSchPropEntry.Values);
      end;
    end;
  end;
end;

procedure TsgSchDat.Write(Stream: TStream; AOffset: UInt64);
var
  vHeader: TsgFileSegmentHeader;
  vSegHeaderWithEntriesSizeNA, vSegHeaderWithEntriesSize: Integer;
  vSegSizeNA: Cardinal;
  I: Integer;
begin
  vSegHeaderWithEntriesSizeNA := SizeOf(TsgFileSegmentHeader) + Length(FSchDatPropEntry) * SizeOf(TsgSchDatUnkPropEntry);
  for I := 0 to High(FProps) do
    Inc(vSegHeaderWithEntriesSizeNA, FProps[I].GetItemSize);

  vSegHeaderWithEntriesSize := AlignSize(vSegHeaderWithEntriesSizeNA, 16);
  vSegSizeNA := vSegHeaderWithEntriesSize + GetNamesSize;

  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, cnstSegAlign);
  vHeader.SysDataAlignOffset := vSegHeaderWithEntriesSize shr 4;
  FSegmentSize := vHeader.SegmentSize;

  Offset := Stream.Position;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  if Length(FSchDatPropEntry) > 0 then
    Stream.Write(FSchDatPropEntry[0], Length(FSchDatPropEntry) * SizeOf(TsgSchDatUnkPropEntry));
  for I := Low(FProps) to High(FProps) do
    FProps[I].Write(Stream, 0);
  for I := vSegHeaderWithEntriesSizeNA to vSegHeaderWithEntriesSize - 1 do
    Stream.Write(cnstSchAlignValue, SizeOf(cnstSchAlignValue));
  WriteNames(Stream, 0);
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

{ TsgSearch }

class function TsgSearch.GetSegName: TsgSegName;
begin
  Result := cntSearchName;
end;

procedure TsgSearch.Read(Stream: TStream);
var
  I, vCount: Cardinal;
begin
  if FReaded then Exit;
  Stream.Position := Offset + SizeOf(TsgFileSegmentHeader);
  Stream.Read(vCount, SizeOf(Cardinal));
  I := 0;
  while I < vCount do
  begin
    ReadData(Stream);
    Inc(I);
  end;
  FReaded := True;
end;

function TsgSearch.CalcSegmentNotAlignedSize(ASysEntriesSize: PSGCardinal = nil): Cardinal;
begin
  Result := SizeOf(TsgFileSegmentHeader) + SizeOf(Cardinal);
  Inc(Result, SizeOf(Cardinal) + SizeOf(UInt64));// schnameindex + sorted index count
  Inc(Result, SegmentedFile.DataItems.Count * SizeOf(UInt64));//Sorted index
  Inc(Result, SizeOf(Cardinal)); // ID indexes count (write 1)
  if SegmentedFile.DataItems.Count > 0 then
  begin
    Inc(Result, SizeOf(Cardinal)); // unknown
    Inc(Result, SizeOf(Cardinal)); // IDIndexCount (write 1)
    Inc(Result, SegmentedFile.DataItems.Count * SizeOf(TSearchID));
  end;
end;

procedure TsgSearch.ReadData(Stream: TStream);
begin
  FReaded := True;
end;

procedure TsgSearch.Write(Stream: TStream; AOffset: UInt64);
var
  I: Integer;
  vHeader: TsgFileSegmentHeader;
  vItemsCount: Cardinal;
  vSegSizeNA: Cardinal;
begin
  vSegSizeNA := CalcSegmentNotAlignedSize;
  FillDefautFileHeader(vHeader);
  vHeader.SegmentSize := AlignSize(vSegSizeNA, cnstSegAlign);
  FSegmentSize := vHeader.SegmentSize;

  Offset := Stream.Position;
  vItemsCount := cnstUInt32One;
  Stream.Write(vHeader, SizeOf(TsgFileSegmentHeader));
  Stream.Write(vItemsCount, SizeOf(Cardinal));
  if vItemsCount > 0 then
    WriteSchema(Stream);
  for I := vSegSizeNA to vHeader.SegmentSize - 1 do
    Stream.Write(cnstSegAlignValue, SizeOf(cnstSegAlignValue));
end;

procedure TsgSearch.WriteSchema(Stream: TStream);
var
  vIndex: UInt64;
  I: Integer;
  vSortedIndexCount: UInt64;
  vIDIndexesCount: Cardinal;
  vIDIndexCount: Cardinal;
begin
  Stream.Write(cnstUInt32Zero, SizeOf(Cardinal));//SchemaNameIndex
  vSortedIndexCount := SegmentedFile.DataItems.Count;
  Stream.Write(vSortedIndexCount, SizeOf(UInt64)); // sorted index count
  vIndex := 0;
  while vIndex < vSortedIndexCount do
  begin
    Stream.Write(vIndex, SizeOf(UInt64)); // sorted index
    Inc(vIndex);
  end;

  vIDIndexesCount := cnstUInt32One;
  Stream.Write(vIDIndexesCount, SizeOf(Cardinal));
  if vIDIndexesCount > 0 then//!
  begin
    Stream.Write(cnstUInt32Zero, SizeOf(Cardinal));//unknown
    for I := 0 to vIDIndexesCount - 1 do
    begin
      vIDIndexCount := cnstUInt32One;
      Stream.Write(vIDIndexCount, SizeOf(Cardinal));
      //begin vIDIndexCount
      vIndex := 0;
      while vIndex < SegmentedFile.DataItems.Count do
      begin
        Stream.Write(TsgDataItem(SegmentedFile.DataItems[vIndex]).Handle, SizeOf(UInt64));
        Stream.Write(cnstUInt64One, SizeOf(UInt64));
        Stream.Write(vIndex, SizeOf(UInt64));
        Inc(vIndex);
      end;
      // end vIDIndexCount
    end;
  end;
end;

{ TsgPrvSav }

class function TsgPrvSav.GetSegName: TsgSegName;
begin
  Result := cntPrvsavName;
end;

{ TsgFreeSp }

class function TsgFreeSp.GetSegName: TsgSegName;
begin
  Result := cntFreespName;
end;

{ TsgDataItem }

constructor TsgDataItem.Create;
begin
  FStream := TMemoryStream.Create;
end;

procedure TsgDataItem.DataBlobReferenceNeeded;
var
  I: Cardinal;
  vTotalDataSize: UInt64;
  vPageCount: Cardinal;
  vRecordSize: Cardinal;
  vPageSize: Cardinal;
  vLastPageSize: Cardinal;
  vDataBlobReferencePage: PsgDataBlobReferencePage;
begin
  FreeMemAndNil(Pointer(FDataBlobReference));
  vTotalDataSize := Stream.Size;
  if vTotalDataSize > cntMaxDataSize then
  begin
    vPageCount := vTotalDataSize div cntMaxPageSize;
    vLastPageSize := vTotalDataSize mod cntMaxPageSize;
    if vPageCount = 0 then
    begin
      vPageSize := vLastPageSize;
      Inc(vPageCount);
    end
    else
    begin
      vPageSize := cntMaxPageSize;
      Inc(vPageCount, Ord(vLastPageSize <> 0));
    end;
    vRecordSize := SizeOf(TsgDataBlobReference) + SizeOf(TsgDataBlobReferencePage) * vPageCount;
    GetMem(FDataBlobReference, vRecordSize);
    FDataBlobReference^.TotalDataSize := vTotalDataSize;
		FDataBlobReference^.PageCount := vPageCount;
		FDataBlobReference^.RecordSize := vRecordSize;
		FDataBlobReference^.PageSize := vPageSize;
		FDataBlobReference^.LastPageSize := vLastPageSize;
		FDataBlobReference^.Unknown1 := 0;
		FDataBlobReference^.Unknown2 := 0;
    vDataBlobReferencePage := PsgDataBlobReferencePage(FDataBlobReference);
    Inc(PByte(vDataBlobReferencePage), SizeOf(TsgDataBlobReference));
    I := 0;
    while I < vPageCount - 1 do
    begin
      vDataBlobReferencePage^.SegmentIndex := 0;
      vDataBlobReferencePage^.Size := vPageSize;
      Inc(vDataBlobReferencePage);
      Inc(I);
    end;
    vDataBlobReferencePage^.SegmentIndex := 0;
    vDataBlobReferencePage^.Size := vLastPageSize;
  end;
end;

destructor TsgDataItem.Destroy;
begin
  FStream.Free;
  FreeMemAndNil(Pointer(FDataBlobReference));
  inherited Destroy;
end;

function TsgDataItem.GetEntries: PsgDataBlobReferencePageArray;
begin
  Result := PsgDataBlobReferencePageArray(IntPtr(FDataBlobReference) + SizeOf(TsgDataBlobReference));
end;

function TsgDataItem.GetItemSize: Cardinal;
begin
  Result := SizeOf(Cardinal); //datasize
  if Assigned(FDataBlobReference) then
    Inc(Result, SizeOf(TsgDataBlobReference) + FDataBlobReference^.PageCount * SizeOf(TsgDataBlobReferencePage))
  else
    Inc(Result, Stream.Size);
end;

function TsgDataItem.HasBlobReference: Boolean;
begin
  Result := Assigned(FDataBlobReference);
end;

{ TsgCustomSchemaSegment }

procedure TsgCustomSchemaSegment.Clear;
begin
  inherited Clear;
  SetLength(FNames, 0);
end;

function TsgCustomSchemaSegment.GetNamesSize: Integer;
var
  I: Integer;
begin
  Result := SizeOf(Integer);
  for I := Low(FNames) to High(FNames) do
    Inc(Result, Length(FNames[I]) + 1);
end;

procedure TsgCustomSchemaSegment.ReadNames(Stream: TStream);
var
  vNamesCount: Integer;
  c: AnsiChar;
begin
  Stream.Position := Offset + Align shl 4;
  Stream.Read(vNamesCount, SizeOf(vNamesCount));
  while vNamesCount > 0 do
  begin
    SetLength(FNames, Length(FNames) + 1);
    while (Stream.Read(c, 1) = 1) and (c <> #0) do
      FNames[High(FNames)] := FNames[High(FNames)] + c;
    Dec(vNamesCount);
end;
end;

procedure TsgCustomSchemaSegment.WriteNames(Stream: TStream; AOffset: UInt64);
var
  I: Integer;
  vNamesCount: Cardinal;
begin
  vNamesCount := Length(FNames);
  Stream.Write(vNamesCount, SizeOf(Cardinal));
  for I := Low(FNames) to High(FNames) do
    Stream.Write(Pointer(FNames[I])^, Length(FNames[I]) + 1);
end;

{ TsgDSItem }

function TsgDSItem.GetItemSize: Cardinal;
begin
  Result := 0;
end;

{ TsgSchemaProperty }

function TsgSchemaProperty.AddEntry(AFlags, ANameIndex, APropType, ATypeSize,
  AUnknown1, AUnknown2: Cardinal; APropValueCount: Integer; const AValues): Integer;
var
  I: Integer;
  vPValue: PByte;
  vValue: TBytes;
begin
  Result := Length(FEntries);
  SetLength(FEntries, Result + 1);
  FEntries[Result].Flags := AFlags;
  FEntries[Result].NameIndex := ANameIndex;
  FEntries[Result].PropType := APropType;
  if FEntries[Result].Flags and 2 = 0 then
  begin
    case APropType of
      $e: FEntries[Result].TypeSize := ATypeSize;
    else
      FEntries[Result].TypeSize := cntTypeSize[APropType];
    end;
  end
  else
    FEntries[Result].TypeSize := ATypeSize;
  FEntries[Result].Unknown1 := AUnknown1;
  FEntries[Result].Unknown2 := AUnknown2;
  FEntries[Result].PropValueCount := APropValueCount;
  if FEntries[Result].TypeSize > 0 then
  begin
    vPValue := PByte(@AValues);
    for I := 0 to APropValueCount - 1 do
    begin
      if Assigned(vPValue) then
      begin
        SetLength(FEntries[Result].Values, Length(FEntries[Result].Values) + 1);
        SetLength(vValue, FEntries[Result].TypeSize);
        System.Move(vPValue^, vValue[0], FEntries[Result].TypeSize);
        FEntries[Result].Values[High(FEntries[Result].Values)] := vValue;
        Inc(vPValue, FEntries[Result].TypeSize);
      end;
    end;
  end;
end;

function TsgSchemaProperty.GetItemSize: Cardinal;
var
  I, J: Integer;
begin
  Result := SizeOf(Word); // indices count
  Inc(Result, Length(FIndices) * SizeOf(UInt64));
  Inc(Result, SizeOf(Word)); //PropCount
  for I := 0 to High(FEntries) do
  begin
    Inc(Result, SizeOf(FEntries[I].Flags));
    Inc(Result, SizeOf(FEntries[I].NameIndex));
    if FEntries[I].Flags and 2 = 0 then
    begin
      Inc(Result, SizeOf(FEntries[I].PropType));
      case FEntries[I].PropType of
        $e: Inc(Result, SizeOf(FEntries[I].TypeSize));
      end;
    end;
    if FEntries[I].Flags = 1 then
      Inc(Result, SizeOf(FEntries[I].Unknown1))
    else
      if FEntries[I].Flags = 8 then
        Inc(Result, SizeOf(FEntries[I].Unknown2));
    Inc(Result, SizeOf(FEntries[I].PropValueCount)); //PropValueCount
    J := 0;
    while J < FEntries[I].PropValueCount do
    begin
      if FEntries[I].TypeSize <> 0 then
        Inc(Result, FEntries[I].TypeSize);
      Inc(J);
    end;
  end;
end;

procedure TsgSchemaProperty.Write(Stream: TStream; AOffset: UInt64);
var
  vIndicesCount, vEntriesCount: Word;
  I, J: Integer;
begin
  vIndicesCount := Length(FIndices);
  Stream.Write(vIndicesCount, SizeOf(Word));
  if vIndicesCount > 0 then
    Stream.Write(FIndices[0], vIndicesCount * SizeOf(UInt64));
  vEntriesCount := Length(FEntries);
  Stream.Write(vEntriesCount, SizeOf(Word));

  for I := 0 to High(FEntries) do
  begin
    Stream.Write(FEntries[I].Flags, SizeOf(FEntries[I].Flags));
    Stream.Write(FEntries[I].NameIndex, SizeOf(FEntries[I].NameIndex));
    if FEntries[I].Flags and 2 = 0 then
    begin
      Stream.Write(FEntries[I].PropType, SizeOf(FEntries[I].PropType));
      case FEntries[I].PropType of
        $e: Stream.Write(FEntries[I].TypeSize, SizeOf(FEntries[I].TypeSize));
      end;
    end;
    if FEntries[I].Flags = 1 then
      Stream.Write(FEntries[I].Unknown1, SizeOf(FEntries[I].Unknown1))
    else
      if FEntries[I].Flags = 8 then
        Stream.Write(FEntries[I].Unknown2, SizeOf(FEntries[I].Unknown2));
    Stream.Write(FEntries[I].PropValueCount, SizeOf(FEntries[I].PropValueCount));
    J := 0;
    while J < FEntries[I].PropValueCount do
    begin
      if FEntries[I].TypeSize <> 0 then
        Stream.Write(FEntries[I].Values[J][0], FEntries[I].TypeSize);
      Inc(J);
    end;
  end;
end;

end.

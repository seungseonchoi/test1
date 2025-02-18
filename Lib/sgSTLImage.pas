{************************************************************}
{    CAD.VCL Cross Platform Library  for Delphi / Lazarus    }
{                                                            }
{                   STL files TGrapic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgSTLImage;
{$I SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, Clipbrd,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPImage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
 FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
 Graphics,
{$ENDIF}
  SysUtils, Classes, DXFConv, CADImage, sgFunction, sgConsts, Math, sgLists,
  sgXMLParser
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  ,AnsiStrings
{$ENDIF}
{$IFDEF SGDEL_XE2}
  ,System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  ,Types
{$ENDIF}
{$ENDIF}
  ;

type
  PsgSTLVertex = ^TsgSTLVertex;
  TsgSTLVertex = packed record
    X: Single;
    Y: Single;
    Z: Single;
  end;

  TsgSTLFace = packed record
	  Normal: TsgSTLVertex;
	  Vertex1: TsgSTLVertex;
	  Vertex2: TsgSTLVertex;
	  Vertex3: TsgSTLVertex;
    Padding: array[0..1] of Byte;
  end;

  PRGBBlock = ^TRGBBlock;
  TRGBBlock = packed record
    case Integer of
     0: (MMagicsR, MMagicsG, MMagicsB: Byte);
     1: (VisCamB, VisCamG, VisCamR: Byte);
  end;

  PRGBABlock = ^TRGBABlock;
  TRGBABlock = packed record
    RGB: TRGBBlock;
    A: Byte;
  end;

  EsgSTLImageError = Exception;

  Tsg3DSolidConverter = class(TsgDXFConverter)
  protected
    procedure GetXmlSectionNames(const ANames: TStrings;
      const AParams: TsgXMLParams); override;
  public
    function FromXML(const ANode: TsgNodeSample;
      const AResult: IsgResultNode = nil): Integer; override;
  end;

  Tsg3DSolidImage = class(TsgVectorImageWithModeller)
  protected
    function CreateConverter: TsgDXFConverter; override;
  public
    procedure Assign(ASource: TPersistent); override;
  end;

  TsgSTLImage = class(Tsg3DSolidImage)
  private
    FLayer: TsgDXFLayer;
    FSTLInsert: TsgDXFInsert;
    FDefColor: TRGBABlock;
    FDiffuseColor: TRGBABlock;
    FSpecularColor: TRGBABlock;
    FAmbientColor: TRGBABlock;
    FMMagics: Boolean;
    FIsMaterial: Boolean;
    procedure AddEntity(const AEntity: TsgDXFEntity);
    procedure AddFace(const StlFace: TsgSTLFace; const IsColor: Boolean = False);
    procedure CreateBaseInsert;
    procedure LoadBaseInsert;
    function GetHandle: UInt64;
    procedure SetHandleForEntity(const Entity: TsgDXFEntity);
    function IsBinary(const Stream: TStream): Boolean;
    procedure LoadSTL(S: TStream);
    function CheckVertex(const AVertex: TsgSTLVertex): Boolean;
    function MakeFPoint(const Vertex: TsgSTLVertex): TFPoint;
  protected
    function GetDefaultView: TsgDefaultView; override;
  public
    procedure LoadFromStream(S: TStream); override;
  end;

const
  cnstSgStlNamePref = 'Sg_STL_';
  sSTLImage = 'STL image';
  sInvalidCountFaces: string = 'Invalid count Faces';
  sInvalidNormalError: string = 'Invalid Normal';
  sInvalidVertex: string = 'Invalid Vertex';
  sInvalidLoop: string = 'End of Loop Not Found';
  sInvalidFace: string = 'End of Facet Not found';
  cnstSTLImageExt = 'stl';

var
  CF_STL: Word;

implementation

const
  cntSolidLbl = 'SOLID';
  cntFacetNormalLbl = 'FACET NORMAL ';
  cntNormalLbl = ' NORMAL ';
  cntOuterLoopLbl = 'OUTER LOOP';
  cntVertexLbl = 'VERTEX';
  cntEndLoopLbl = 'ENDLOOP';
  cntEndFacetLbl = 'ENDFACET';
  cntEndSolidLbl = 'ENDSOLID';
  cntFacetLbl = 'FACET';

type
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXF3DFaceAccess = class(TsgDXF3dFace);

  TsgSTLFileHeader = packed record
    Dummy: array[0..79] of Byte;
    NumFaces: Integer;
  end;

  TStringDynArray = array of string;

  TstlTextReader = class
  private
    FImage: TsgSTLImage;
    FSource: TStream;
    FStream: TCustomMemoryStream;
    FLineIndex: Integer;
    FPosOffset: TsgNativeUInt;
    FDataOffset: TsgNativeUInt;
    FStartPos: TsgNativeUInt;
    FPos: TsgNativeUInt;
    FEnd: TsgNativeUInt;
    FDataSize: TsgNativeUInt;
    procedure PrepareStream;
    procedure Error(const AMessage: string);
  protected
    procedure DoProgress(AStage: TProgressStage);
    procedure DoReadSTL;
  public
    constructor Create(AImage: TsgSTLImage; S: TStream);
    destructor Destroy; override;
    procedure Read;
    function ReadLine: AnsiString;
  end;

function stof(const s: string; out v: single): Integer;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Val(s, v, Result);
end;

function SplitString(const S: string; AStartIndex: Integer = 1;
  const ASeparator: Char = ' '): TStringDynArray;
var
  P, PEnd, Start: PChar;
  Len: Integer;
begin
  SetLength(Result, 0);
  Len := Length(S);
  P := PChar(S);
  PEnd := P;
  Inc(P, AStartIndex - 1);
  Inc(PEnd, Len);
  while (P < PEnd) and (P^ = ' ') do Inc(P);
  while P < PEnd do
  begin
    Start := P;
    while (P < PEnd) and (P^ = ' ') do Inc(P);
    while (P < PEnd) and (P^ <> ASeparator) do Inc(P);
    SetLength(Result, Length(Result) + 1);
    SetString(Result[High(Result)], Start, (IntPtr(P) - IntPtr(Start)) div SizeOf(P^));
    if P^ = ASeparator then Inc(P);
  end;
end;

{ TstlTextReader }

constructor TstlTextReader.Create(AImage: TsgSTLImage; S: TStream);
begin
  FImage := AImage;
  FSource := S;
end;

destructor TstlTextReader.Destroy;
begin
  if FSource <> FStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TstlTextReader.DoProgress(AStage: TProgressStage);
begin
  FImage.DoOnProgress(AStage, FPosOffset + (FPos - FStartPos),
    FDataOffset + FDataSize);
end;

procedure TstlTextReader.DoReadSTL;
var
  V: PsgSTLVertex;
  vEndSolid, vBeginFacet, vBadFace: Boolean;
  vParams: TStringDynArray;
  vStlFace: TsgSTLFace;

  procedure DoVertex(AStartIndex: Integer; var AVertex: TsgSTLVertex);
  begin
    stof(vParams[AStartIndex], AVertex.X);
    stof(vParams[AStartIndex + 1], AVertex.Y);
    stof(vParams[AStartIndex + 2], AVertex.Z);
  end;

  procedure ReadSTLNormal(var Normal: TsgSTLVertex);
  begin
    if Length(vParams) <> 5 then
      Error(sInvalidNormalError)
    else
      DoVertex(2, Normal);
  end;

  procedure ReadSTLVertex(var Vertex: PsgSTLVertex);
  begin
    if Vertex = nil then
      Error(sInvalidVertex)
    else
      DoVertex(1, Vertex^);
    Inc(Vertex);
  end;

begin
  vEndSolid := False;
  vBeginFacet := False;
  vBadFace := False;
  while (FPos < FEnd) and not vEndSolid do
  begin
    vParams := SplitString(string(ReadLine), 1);
    if Length(vParams) > 0 then
    begin
      if cntEndSolidLbl = UpperCase(vParams[0]) then
        vEndSolid := True
      else
      if cntFacetLbl = UpperCase(vParams[0]) then
      begin
        if vBeginFacet then Error(sInvalidFace);
        vBeginFacet := True;
        vBadFace := False;
        FillChar(vStlFace, SizeOf(vStlFace), 0);
        ReadSTLNormal(vStlFace.Normal);
        V := @vStlFace.Vertex1;
      end
      else
      if cntVertexLbl = UpperCase(vParams[0]) then
        ReadSTLVertex(V)
      else
      if cntEndLoopLbl = UpperCase(vParams[0]) then
      begin
        vBadFace := V <> @vStlFace.Padding[0];
        V := nil;
      end
      else
      if cntEndFacetLbl = UpperCase(vParams[0]) then
      begin
        if not vBeginFacet then Error(sInvalidFace);
        if V <> nil then Error(sInvalidLoop);
        if not vBadFace then
          FImage.AddFace(vStlFace);
        vBeginFacet := False;
      end;
    end;
    DoProgress(psRunning);
  end;
end;

procedure TstlTextReader.Error(const AMessage: string);
begin
  raise EsgSTLImageError.Create(AMessage);
end;

procedure TstlTextReader.PrepareStream;
const
  cnstMaxBufSize = $F000;
var
  Buffer: Pointer;
  BufferSize, N: Integer;
begin
  if FSource is TCustomMemoryStream then
  begin
    FPosOffset := 0;
    FDataOffset := 0;
    FStream := TCustomMemoryStream(FSource);
    FDataSize := FStream.Size - FStream.Position;
    FStartPos := TsgNativeUInt(FStream.Memory) + FStream.Position;
    FPos := FStartPos;
    FEnd := FStartPos + FDataSize;
    DoProgress(psStarting);
  end
  else
  begin
    FStream := TMemoryStream.Create;
    FDataSize := FSource.Size - FSource.Position;
    FPosOffset := 0;
    FDataOffset := FDataSize;
    FStartPos := 0;
    FPos := FStartPos;
    FEnd := FStartPos + FDataSize;
    DoProgress(psStarting);
    if FDataSize < cnstMaxBufSize then BufferSize := FDataSize else BufferSize := cnstMaxBufSize;
    GetMem(Buffer, BufferSize);
    try
      while FPos < FDataSize do
      begin
        N := FDataSize - FPos;
        if N > BufferSize then N := BufferSize;
        FSource.ReadBuffer(Buffer^, N);
        FStream.WriteBuffer(Buffer^, N);
        Inc(FPos, N);
        DoProgress(psRunning);
      end;
    finally
      FreeMem(Buffer, cnstMaxBufSize);
    end;
    FStartPos := TsgNativeUInt(FStream.Memory);
    FPos := FStartPos;
    FEnd := FStartPos + FDataSize;
    FPosOffset := FDataSize;
  end;
end;

procedure TstlTextReader.Read;
begin
  PrepareStream;
  try
    DoReadSTL;
  finally
    FPos := FEnd;
    DoProgress(psEnding);
  end;
end;

function TstlTextReader.ReadLine: AnsiString;
var
  StartLn: TsgNativeUInt;
begin
  StartLn := FPos;
  while (FPos < FEnd) and (PByte(FPos)^ <> 13) and (PByte(FPos)^ <> 10) do Inc(FPos);
  SetString(Result, PAnsiChar(StartLn), FPos - StartLn);
  if (FPos < FEnd) and (PByte(FPos)^ = 13) then Inc(FPos);
  if (FPos < FEnd) and (PByte(FPos)^ = 10) then Inc(FPos);
  Inc(FLineIndex);
end;

{ TsgSTLImage }

procedure TsgSTLImage.AddEntity(const AEntity: TsgDXFEntity);
begin

  FSTLInsert.Block.AddEntity(AEntity);
end;

procedure TsgSTLImage.AddFace(const StlFace: TsgSTLFace;
  const IsColor: Boolean = False);
var
  vFace: TsgDXF3dFace;
  vWord: Word;

  function GetColor(AWord: Word): TsgColorCAD;

    function GetComponentColor(const AComponent: Cardinal): Cardinal;
    begin
      Result := (AComponent * 255) div 31;
    end;

  var
    R, G, B: Cardinal;
  begin
    R := GetComponentColor(AWord and $1F);
    G := GetComponentColor((AWord shr 5) and $1F);
    B := GetComponentColor((AWord shr 10) and $1F);
    if not FMMagics then
      SwapInts(R, B);
    Result := MakeColorCAD(acRGBColor, ConvertRGBtoColor(R, G, B));
  end;

begin
  if CheckVertex(StlFace.Vertex1) or CheckVertex(StlFace.Vertex2) or
    CheckVertex(StlFace.Vertex3) then
    Exit;
  vFace := TsgDXF3dFace.Create;
  vFace.Point := MakeFPoint(StlFace.Vertex1);
  vFace.Point1 := MakeFPoint(StlFace.Vertex2);
  vFace.Point2 := MakeFPoint(StlFace.Vertex3);
  vFace.Point3 := vFace.Point2;
  if IsColor then
  begin
    vWord := (StlFace.Padding[1] shl 8) or StlFace.Padding[0];
    if FMMagics then
    begin
      if (vWord and $8000) = $8000 then  //?
      begin
        if FIsMaterial then
          vFace.ColorCAD := MakeColorCAD(acRGBColor,
          ConvertRGBtoColor(FDiffuseColor.RGB.VisCamR, FDiffuseColor.RGB.VisCamG, FDiffuseColor.RGB.VisCamB))
        else
        vFace.ColorCAD := MakeColorCAD(acRGBColor,
          ConvertRGBtoColor(FDefColor.RGB.MMagicsR, FDefColor.RGB.MMagicsG, FDefColor.RGB.MMagicsB))
      end
      else
        vFace.ColorCAD := GetColor(vWord)
    end
    else
      if (vWord and $8000) = $8000 then
        vFace.ColorCAD := GetColor(vWord)
      else
        vFace.ColorCAD := cnstColorCADByBlock;
  end
  else
    vFace.ColorCAD := cnstColorCADByBlock;
  vFace.Layer := FLayer;
  AddEntity(vFace);
end;

function TsgSTLImage.CheckVertex(const AVertex: TsgSTLVertex): Boolean;
begin
  Result := IsNan(AVertex.X) or IsNan(AVertex.Y) or IsNan(AVertex.Z);
  Result := Result or IsInfinite(AVertex.X) or IsInfinite(AVertex.Y) or IsInfinite(AVertex.Z);
end;

procedure TsgSTLImage.CreateBaseInsert;
var
  vBlock: TsgDXFBlock;
begin
  vBlock := TsgDXFBlock.Create;
  vBlock.Name := GetNameCorrect(cnstSgStlNamePref + IntToStr(TsgNativeUINT(vBlock)));
  Converter.Sections[csBlocks].AddEntity(vBlock);
  FSTLInsert := TsgDXFInsert.Create;
  FSTLInsert.ColorCAD := FLayer.ColorCAD;
  FSTLInsert.LineWeight := fLineWeightByLayer;
  FSTLInsert.Block := vBlock;
  CurrentLayout.PaperSpaceBlock.AddEntity(FSTLInsert);
end;

function TsgSTLImage.GetDefaultView: TsgDefaultView;
begin
  Result := df3D;
end;

function TsgSTLImage.GetHandle: UInt64;
begin
  Result := cnstBadHandle;
  TsgDXFConverterAccess(Converter).CheckHandle(Result);
end;

function TsgSTLImage.IsBinary(const Stream: TStream): Boolean;
const
  cnstAnalyzedSegmentSTLFile = $300;
  cnstNonAsciiChar = $7F;
  cntSTLFileHeaderLen = SizeOf(TsgSTLFileHeader)- SizeOf(Integer);
  cnstMMagicsColor: AnsiString = 'COLOR=';
  cnstMMagicsMaterial: AnsiString = ',MATERIAL=';
var
  I, J, vPos: Integer;
  vStream: TMemoryStream;
  vSTLPointerMem: PByte;
  vHeader: TsgSTLFileHeader;
begin
  Result := False;
  if Stream.Size < cntSTLFileHeaderLen then
    Exit;
  vPos := Stream.Position;
  vStream := TMemoryStream.Create;
  try
    Stream.Position := 0;
    Stream.Read(vHeader, SizeOf(TsgSTLFileHeader));

    vSTLPointerMem := @vHeader;
    FMMagics := False;
    FIsMaterial := False;
    I := 0;
    while I <= SizeOf(TsgSTLFileHeader) do
    begin
      if CompareMem(vSTLPointerMem, PAnsiString(cnstMMagicsColor), 6) then
      begin
        FMMagics := True;
        Inc(vSTLPointerMem, 6);
        FDefColor := PRGBABlock(vSTLPointerMem)^;
        Inc(vSTLPointerMem, 4);
        J := (I+10);
        while J <= SizeOf(TsgSTLFileHeader) do
        begin
          if CompareMem(vSTLPointerMem, PAnsiString(cnstMMagicsMaterial), 10) then
          begin
            FIsMaterial := True;
            Inc(vSTLPointerMem, 10);
            FDiffuseColor := PRGBABlock(vSTLPointerMem)^;
            Inc(vSTLPointerMem, 4);
            FSpecularColor := PRGBABlock(vSTLPointerMem)^;
            Inc(vSTLPointerMem, 4);
            FAmbientColor := PRGBABlock(vSTLPointerMem)^;
            Break;
          end
          else
            Inc(vSTLPointerMem);
          Inc(J);
        end;
        Break;
      end
      else
        Inc(vSTLPointerMem);
      Inc(I);
    end;
    Stream.Position := cntSTLFileHeaderLen;
    vStream.CopyFrom(Stream, Min(Stream.Size - cntSTLFileHeaderLen,cnstAnalyzedSegmentSTLFile));
    vStream.Position := 0;
    vSTLPointerMem := vStream.Memory;
    I := 0;
    while I < vStream.Size do
    begin
      if vSTLPointerMem^ >= cnstNonAsciiChar then
      begin
        Result := True;
        Break;
      end;
      Inc(vSTLPointerMem);
      Inc(I);
    end;
  finally
    vStream.Free;
    Stream.Position := vPos;
  end;
end;

procedure TsgSTLImage.LoadBaseInsert;
var
  I: Integer;
  vBlock: TsgDXFBlock;
  vEntity: TsgDXFEntity;

  procedure AddAttrib(AInsert: TsgDXFInsert; ATag, AText: string);
  var
    vAttrib: TsgDXFAttrib;
  begin
    vAttrib := TsgDXFAttrib.Create;
    AInsert.AddEntity(vAttrib);
    vAttrib.NotAppear := True;
    vAttrib.Tag := ATag;
    vAttrib.Text := AText;
    vAttrib.Value := vAttrib.Text;
    vAttrib.Point := FPointXMat(cnstFPointZero, AInsert.GetMatrix);
    vAttrib.Height := 1;
    Converter.Loads(vAttrib);
  end;

begin
  vBlock := FSTLInsert.Block;
  I := 0;
  while I < vBlock.Count do
  begin
    vEntity := vBlock[I];
    Converter.Loads(vEntity);
    //if vEntity.EntType = ce3dFace then
      TsgDXF3DFaceAccess(vEntity).InternalFlags :=
        TsgDXF3DFaceAccess(vEntity).InternalFlags and not $10;
    Inc(I);
  end;
  Converter.Loads(vBlock);
  AddAttrib(FSTLInsert, cnstBrepOriginalName, ExtractFileName(FileName));
  Converter.Loads(FSTLInsert);
end;

procedure TsgSTLImage.LoadFromStream(S: TStream);
var
  {$IFNDEF SG_OPENING_IN_THEADS}
  vImage: TsgCADImage;
  {$ENDIF}
  vNeed2DViewByDefault: Boolean;
begin
  inherited LoadFromStream(S);
  {$IFNDEF SG_OPENING_IN_THEADS}
  vImage := Loading;
  if vImage <> nil then
    vImage.Converter.StopLoading;
  Loading := Self;
  {$ENDIF}
  try
    TsgDXFConverterAccess(Converter).SetLoading(True, nil);
    CurrentLayout := TsgDXFConverterAccess(Converter).Layouts[0];
    SetHandleForEntity(CurrentLayout);
    FLayer := Converter.LayerByName('0');
    FLayer.ColorCAD := ConvertARGBToColorCAD(clGray);
    Converter.ClearDrawingProp;
    CreateBaseInsert;
    try
      LoadSTL(S);
    finally
      LoadBaseInsert
    end;
    GetExtents;
    Set3DRotDef(Converter.Layouts[0].Box, vNeed2DViewByDefault);
    SetDefaultPlotSettings;
  finally
    TsgDXFConverterAccess(Converter).SetLoading(False, nil);
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := nil;
    {$ENDIF}
  end;
end;


procedure TsgSTLImage.LoadSTL(S: TStream);
var
  I, vCount: Integer;
  vHeader: TsgSTLFileHeader;
  vStlFace: TsgSTLFace;
  vStlTextReader: TstlTextReader;
begin
  if IsBinary(S) then
  begin
    S.Read(vHeader, SizeOf(vHeader));
    // We can not rely on it, do the calculation
    // vCount := vHeader.NumFaces - 1;
    //if vCount < 0 then
    vCount := (S.Size - S.Position) div SizeOf(TsgSTLFace);
    if vCount <> vHeader.NumFaces then
      raise EsgSTLImageError.Create(sInvalidCountFaces);
    DoOnProgress(psStarting, 1, vCount);
    try
      for I := 0 to vCount - 1 do
      begin
        S.Read(vStlFace, SizeOf(TsgSTLFace));
        AddFace(vStlFace, True);
        DoOnProgress(psRunning, I + 1, vCount);
      end;
    finally
      DoOnProgress(psEnding, vCount, vCount);
    end;
  end
  else
  begin
    vStlTextReader := TstlTextReader.Create(Self, S);
    try
      vStlTextReader.Read;
    finally
      vStlTextReader.Free;
    end;
  end;
end;

function TsgSTLImage.MakeFPoint(const Vertex: TsgSTLVertex): TFPoint;
begin
  Result.X := Vertex.X;
  Result.Y := Vertex.Y;
  Result.Z := Vertex.Z;
end;

procedure TsgSTLImage.SetHandleForEntity(const Entity: TsgDXFEntity);
begin
  if Entity.Handle = cnstBadHandle then
    Entity.Handle := GetHandle;
end;

{ Tsg3DSolidImage }

procedure Tsg3DSolidImage.Assign(ASource: TPersistent);
begin
  inherited Assign(ASource);
end;

function Tsg3DSolidImage.CreateConverter: TsgDXFConverter;
begin
  Result := Tsg3DSolidConverter.Create;
end;

{ Tsg3DSolidConverter }

function Tsg3DSolidConverter.FromXML(const ANode: TsgNodeSample;
  const AResult: IsgResultNode): Integer;
begin
  Result := inherited FromXML(ANode, AResult);
end;

procedure Tsg3DSolidConverter.GetXmlSectionNames(const ANames: TStrings;
  const AParams: TsgXMLParams);
begin
  ANames.Add(cnstXMLEntities);
end;

initialization
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_STL := RegisterClipboardFormat('CADSoftTools STL Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_STL, TsgSTLImage);
  TPicture.RegisterFileFormat(cnstSTLImageExt, sSTLImage, TsgSTLImage);
{$IFDEF SG_XREF_EXTENDED}
  TsgDXFConverterAccess.RegisterXRefGraphicClass(cnstSTLImageExt, TsgSTLImage);
{$ENDIF}

finalization
  TPicture.UnRegisterGraphicClass(TsgSTLImage);

end.

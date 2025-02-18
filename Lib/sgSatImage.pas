{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                    B-rep images clases                     }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sgSatImage;
{$I SGDXF.inc}
interface

uses
{$IFDEF SG_FIREMONKEY}
  sgFMXTypes, sgProxyGraphics, System.UITypes, FMX.Graphics,
{$ELSE}
  Graphics,
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, sgOLEStorage, ComObj, ActiveX,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPImage,
{$ENDIF}
{$IFDEF SGDEL_6}
  StrUtils, 
{$ENDIF}  
  Classes, DXFConv, CADImage, SysUtils,
  sgFunction, sgConsts, Math, sgLines, sgLists, sgSWZipStorage, sgMeasureUnits, sgModeller,
  sgModellerIO, sgModImportACIS, sgModImportBREP,
  sgModImportSTEP{sgModellerFileIO_STEP}, sgModImportIGES, sgModImportInventor,
  sgModellerFileIO_PSL, sgModImportSolidWorks, sgMetaParser,
{$IFDEF SG_USE_MCP3D}
  sgModImportMccMt,
{$ENDIF}
{$IFDEF SG_USE_IFC}
  sgModImportIFC,
{$ENDIF}
{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
  sgMeshBuilderWrapper,
{$ENDIF}
  sgMDefinitions, sgMFunctions
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  ,AnsiStrings
{$ENDIF}
  ;

type
  TsgCommonBrepImageError = class(Exception);

  TsgCommonBrepImage = class(TsgVectorImageWithModeller)
  private
    //Progress
    FTotalObjCount,
    FCurrentObjCount,
    FProgressStep,
    FDeltaCnt: Integer;
    FPercentDone: Byte;
    // MParser
    FMPReader: TsgMPReader;
    FTotalStreamSize: Int64;

    FStopLoad: Boolean;
    FNeed2DViewByDefault: Boolean;
    FExternalLinks: TStrings;
    function GetNeed2DViewByDefault: Boolean;
  protected
    procedure OnParserInit(Sender: TObject);
    procedure OnParserEntity(Sender: TObject);
    procedure OnParserError(const ASender: TObject; const AMessage: string);
    procedure OnModellerPerform(Sender: TObject);
    procedure SetModellerProgress(const AModeller: TsgModeller); virtual;
    procedure DoLoad(const AStream: TStream; const FileName: string);
    function GetDefaultView: TsgDefaultView; override;
    procedure LoadBrep(const Stream: TStream; const ALayer: TsgDXFLayer;
      const FileName: string); virtual;
    function FindExtLinkFile(const ADirName, AFileName: string): string;
    function SearchExtLinkFile(const AFileName: string): String;
    function QueryExternalLink(const AResName: WideString): TStream;
    function GetUnresolvedExternalLinkNames(const AFileNames: TStrings): Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure StopLoading; override;
    property EmptyExtLinks: TStrings read FExternalLinks;
  end;

  TsgBrepImage = class(TsgCommonBrepImage)
  protected
    function GetBrepFileIOClass: TClass; virtual; abstract;
    procedure LoadBrep(const Stream: TStream; const ALayer: TsgDXFLayer;
      const FileName: string); override;
  public
     procedure LoadFromFile(const FileName: string); override;
     procedure LoadFromStream(S: TStream); override;
  end;

{$IFDEF SG_USE_MCP3D}
  TsgMCPVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;
{$ENDIF}

  TsgACISVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgIGESVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
  public
     procedure LoadFromFile(const FileName: string); override;
{$ENDIF}
  end;


  TsgSTEPVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgSTPZVectorFile = class(TsgSTEPVectorFile)
  protected
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgBrepVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
//{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
//  public
//     procedure LoadFromFile(const FileName: string); override;
//{$ENDIF}
  end;

  TsgParasolidVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgSolidWorksBaseVectorFile = class(TsgParasolidVectorFile)
  protected
    function GetBrepGroupClass: TClass; override;
  end;

  TsgSldPrtVectorFile = class(TsgSolidWorksBaseVectorFile)
  protected
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgSldAsmVectorFile = class(TsgSolidWorksBaseVectorFile)
  protected
    function GetBrepFileIOClass: TClass; override;
  end;

  TsgInventorVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;

{$IFDEF SG_USE_IFC}
  TsgIFCVectorFile = class(TsgBrepImage)
  protected
    function GetBrepGroupClass: TClass; override;
    function GetBrepFileIOClass: TClass; override;
  end;
{$ENDIF}

const
  sSATImage = 'SAT image';

implementation

uses
  sgZip, Parasolid, ACISCommon, AllVersionParasolid;

const
  cnst3dFile = '';
  cnstProgressStep = 2;

type
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgModellerClass = class of TsgModellerImporter;


{$IFNDEF HAS_FEATURE_ANSISTRINGS}
function ReplaceStr(const AText, AFromText, AToText: string): string;{$IFDEF SG_INLINE} inline;{$ENDIF}
begin
  Result := SysUtils.StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;
{$ENDIF}

{ TsgBrepImage }

function CallBackStop(Parser: Pointer; ACurrent, AMaxCount: Integer):Integer; cdecl;
begin
  TsgCommonBrepImage(Parser).DoOnProgress(psRunning,ACurrent,
    AMaxCount);
  Result := 0;
  if (TsgCommonBrepImage(Parser).FStopLoad = True) then
    Result := 1;
end;

function QueryCADUnits(const APart: TsgModPartCompound): TsgInsUnits;
var
  vFactor: Extended;
begin
  Result := iuUnitless;
  if not Assigned(APart) then
    Exit;
  vFactor := APart.LengthFactor;
  if IsFactorEquals(vFactor, 1) then
    Result := iuMeters
  else if IsFactorEquals(vFactor, 1E-3) then
    Result := iuMillimeters
  else if IsFactorEquals(vFactor, 1E-2) then
    Result := iuCentimeters
  else if IsFactorEquals(vFactor, cnstMetersInInch) then
    Result := iuInches
  else if IsFactorEquals(vFactor, 1E-1) then
    Result := iuDecimeters
  else if IsFactorEquals(vFactor, cnstMetersInFeet) then
    Result := iuFeet
  else if IsFactorEquals(vFactor, cnstMetersInYard) then
    Result := iuYards
  else if IsFactorEquals(vFactor, cnstMetersInMile) then
    Result := iuMiles
  else if IsFactorEquals(vFactor, 1E3) then
    Result := iuKilometers
  else if IsFactorEquals(vFactor, 1E-6) then
    Result := iuMicrons
  else if IsFactorEquals(vFactor, cnstMetersInInch * 1E-3) then
    Result := iuMils
  else if IsFactorEquals(vFactor, cnstMetersInInch * 1E-6) then
    Result := iuMicroinches
  else if IsFactorEquals(vFactor, 1E-9) then
    Result := iuNanometers
  else if IsFactorEquals(vFactor, cnstMetersInAngstrom) then
    Result := iuAngstroms
  else if IsFactorEquals(vFactor, 1E1) then
    Result := iuDecameters
  else if IsFactorEquals(vFactor, 1E2) then
    Result := iuHectometers
  else if IsFactorEquals(vFactor, 1E9) then
    Result := iuGigameters
  else if IsFactorEquals(vFactor, cnstMetersInAstronomicalUnit) then
    Result := iuAstronomical
  else if IsFactorEquals(vFactor, cnstMetersInLigthYear) then
    Result := iuLightyears
  else if IsFactorEquals(vFactor, cnstMetersInParsec) then
    Result := iuParsecs
  ;
end;

procedure TsgBrepImage.LoadBrep(const Stream: TStream; const ALayer: TsgDXFLayer;
  const FileName: string);
var
  vFileIO: TsgModellerImporter;
  vRootCompound: TsgModPartCompound;
  vModeller: TsgModeller;
  vParserOptions: TsgCustomOptions;
  vHeadVarStruct: TsgHeadVarStruct;
  vRootUnits: TsgInsUnits;
begin
  vModeller := Converter.QueryModeller(True);
  vFileIO := TsgModellerClass(GetBrepFileIOClass).Create(vModeller);
  vFileIO.QueryExtLinkStream := QueryExternalLink;
  vParserOptions.OnInit := OnParserInit;
  vParserOptions.OnEntity := OnParserEntity;
  vParserOptions.OnError := OnParserError;
  vParserOptions.CustomFlags := 0;
  vFileIO.ParserOptions := @vParserOptions;
  vRootCompound := vFileIO.LoadFromStream(Stream);
  FMPReader := nil;
  if Assigned(vRootCompound) then
    vModeller.Root[False] := vRootCompound;

  vHeadVarStruct := Converter.HeadVarStruct;
  vRootUnits := QueryCADUnits(vRootCompound);
  vHeadVarStruct.InsUnits := Integer(vRootUnits);
  Converter.HeadVarStruct := vHeadVarStruct;
  SetModellerProgress(vModeller);

  UpdateFromModeller(ALayer);

{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  GlobalBrepParser.Add(vFileIO.Parser);
{$ELSE}
  vModeller.ClearExternalLinks;
  vFileIO.DropParser;
{$ENDIF}
  vFileIO.Free;
end;

procedure TsgBrepImage.LoadFromFile(const FileName: string);
begin
  inherited LoadFromFile(FileName);
  //DoLoad(nil, FileName);
end;

procedure TsgBrepImage.LoadFromStream(S: TStream);
begin
  inherited;
  DoLoad(S, '');
end;

{ TsgCommonBrepImage }

constructor TsgCommonBrepImage.Create;
begin
  inherited Create;
  //Converter.InitializeSectionsBegin;
  ShowPlotForModel := False;
  FNeed2DViewByDefault := False;
end;

function TsgCommonBrepImage.FindExtLinkFile(const ADirName, AFileName: string): string;
var
  vSRec: TSearchRec;
begin
  Result := '';
  try
  if FindFirst(ADirName + '*.*', faAnyFile, vSRec) = 0 then
    repeat
      if (vSRec.Name = '.') or (vSRec.Name = '..') then
        Continue
      else if (vSRec.Attr and faDirectory) <> 0 then
      begin
        Result := FindExtLinkFile(ADirName + vSRec.Name + '\', AFileName);
        if Result <> '' then
          Exit;
      end
      else if vSRec.Name = AFileName then
      begin
        Result := ADirName + AFileName;
        Exit;
      end;
    until FindNext(vSRec) <> 0;
  finally
    FindClose(vSRec);
  end;
end;

function TsgCommonBrepImage.SearchExtLinkFile(const AFileName: string): string;
var
  vSRec: TSearchRec;
begin
  Result := AFileName;
  try
    if FindFirst(AFileName, faAnyFile, vSRec) = 0 then
      Exit;
  finally
    FindClose(vSRec);
  end;
  Result := FindExtLinkFile(ExtractFilePath(FileName), ExtractFileName(AFileName));
end;

function TsgCommonBrepImage.QueryExternalLink(const AResName: WideString): TStream;
var
  vIndex: Integer;
  vName, vFindName: String;
  vStream: TMemoryStream;
  vObjStr: TsgObjectString;
begin
  Result := nil;
  if not Assigned(FExternalLinks) then
  begin
    FExternalLinks := TStringList.Create;
    TStringList(FExternalLinks).Sorted := True;
  end;
  vObjStr := nil;
  try
    vName := string(AResName);
    vFindName := '';
    vIndex := FExternalLinks.IndexOf(vName);
    if vIndex > -1  then
    begin
      vObjStr := TsgObjectString(FExternalLinks.Objects[vIndex]);
      if Assigned(vObjStr) then
        vFindName := vObjStr.FieldStr;
    end
    else
      vFindName := SearchExtLinkFile(vName);
    if Length(vFindName) > 0  then
    begin
      if not Assigned(vObjStr) then
        vObjStr := TsgObjectString.Create(vFindName);
      vStream := TMemoryStream.Create;
      vStream.LoadFromFile(vFindName);
      Result := vStream;
    end;
  finally
    if vIndex < 0 then
      FExternalLinks.AddObject(vName, vObjStr);
  end;
end;

destructor TsgCommonBrepImage.Destroy;
begin
  FreeObjectStringList(FExternalLinks);
  inherited Destroy;
end;

procedure TsgCommonBrepImage.Assign(Source: TPersistent);
var
  vSource: TsgCommonBrepImage absolute Source;
  I, vPos: Integer;
  vObjStr:  TsgObjectString;
begin
  FreeObjectStringList(FExternalLinks);
  inherited Assign(Source);
  if Source is TsgCommonBrepImage then
  begin
    if Assigned(vSource.FExternalLinks) then
    begin
      FExternalLinks := TStringList.Create;
      try
        for I := 0 to vSource.FExternalLinks.Count - 1 do
        begin
          vObjStr := TsgObjectString(vSource.FExternalLinks.Objects[I]);
          if vObjStr = nil then
            FExternalLinks.AddObject(vSource.FExternalLinks[I], nil)
          else
            FExternalLinks.AddObject(vSource.FExternalLinks[I], TsgObjectString.Create(vObjStr.FieldStr));
        end;
      finally
        TStringList(FExternalLinks).Sorted := True;
      end;
    end;
  end;
end;

procedure TsgCommonBrepImage.DoLoad(const AStream: TStream; const FileName: string);
var
  {$IFNDEF SG_OPENING_IN_THEADS}
  vImage: TsgCADImage;
  {$ENDIF}
  vLayer: TsgDXFLayer;
  vStream: TMemoryStream;
begin
//  if AStream is TMemoryStream then
  if AStream.InheritsFrom(TMemoryStream) then
    vStream := TMemoryStream(AStream)
  else
  begin
    vStream := TMemoryStream.Create;
    vStream.LoadFromStream(AStream);
    vStream.Position := 0;
  end;
  try
    {$IFNDEF SG_OPENING_IN_THEADS}
    vImage := Loading;
    if vImage <> nil then
      vImage.Converter.StopLoading;
    Loading := Self;
    {$ENDIF}
    try
      DoOnProgress(psStarting, 1, 100);
      TsgDXFConverterAccess(Converter).SetLoading(True, nil);
      Converter.ClearDrawingProp;
      GenerateLineTypesEx(Converter, cnst3dFile, False, 1);
      CurrentLayout := TsgDXFConverterAccess(Converter).Layouts[0];
      //SetHandleForEntity(CurrentLayout);
      vLayer := Converter.LayerByName('0');
      vLayer.ColorCAD := ConvertARGBToColorCAD(clGray);
      DoOnProgress(psRunning, 1, 100);

      LoadBrep(vStream, vLayer, FileName);
      GetExtents;
      Set3DRotDef(Converter.Layouts[0].Box, FNeed2DViewByDefault);
      SetDefaultPlotSettings;
    finally
      TsgDXFConverterAccess(Converter).SetLoading(False, nil);
      {$IFNDEF SG_OPENING_IN_THEADS}
      Loading := nil;
      {$ENDIF}
      Converter.QueryModeller(True).OnNotify[mnPerform] := nil;
      DoOnProgress(psEnding,100, 100);
    end;
  finally
    if vStream <> AStream then
     vStream.Free;
  end;
end;

function TsgCommonBrepImage.GetDefaultView: TsgDefaultView;
begin
  if GetNeed2DViewByDefault then
    Result := df2D
  else
    Result := df3D;
end;

function TsgCommonBrepImage.GetNeed2DViewByDefault: Boolean;
begin
  Result := FNeed2DViewByDefault;
end;

function TsgCommonBrepImage.GetUnresolvedExternalLinkNames(const AFileNames: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(FExternalLinks) then
  begin
    for I := 0 to FExternalLinks.Count - 1 do
    begin
      if FExternalLinks.Objects[I] = nil then
      begin
        Inc(Result);
        if Assigned(AFileNames) then
          AFileNames.Add(FExternalLinks[I]);
      end;
    end;
  end;
end;

procedure TsgCommonBrepImage.LoadBrep(const Stream: TStream;
  const ALayer: TsgDXFLayer; const FileName: string);
begin
end;

procedure TsgCommonBrepImage.OnParserInit(Sender: TObject);
begin
  FMPReader := TsgMPReader(Sender);
  if not Assigned(FMPReader) then
    Exit;
  FTotalStreamSize := FMPReader.Size;
  FPercentDone := 1;
  //additional initialization of the reader with settings,
  //depending on the format (virt. method call)
end;

procedure TsgCommonBrepImage.OnParserEntity(Sender: TObject);
var
  vProgress: Integer;
begin
  vProgress := Trunc(FMPReader.Position * 25 / FTotalStreamSize);
  if vProgress > FPercentDone then
  begin
    Inc(FPercentDone);
    if FPercentDone > 25 then
      Exit;
    DoOnProgress(psRunning, FPercentDone, 100);
  end;
end;

procedure TsgCommonBrepImage.OnParserError(const ASender: TObject; const AMessage: string);
begin
  //calling a function that adds an error message to the log
end;

procedure TsgCommonBrepImage.OnModellerPerform(Sender: TObject);
begin
  Inc(FCurrentObjCount);
  Inc(FDeltaCnt);
  if FDeltaCnt >= FProgressStep then
  begin
    FDeltaCnt := 0;
    Inc(FPercentDone, cnstProgressStep);
    if FPercentDone > 100 then
      Exit;
    DoOnProgress(psRunning, FPercentDone, 100);
  end;
end;

procedure TsgCommonBrepImage.SetModellerProgress(const AModeller: TsgModeller);
begin
  FTotalObjCount := AModeller.GetTotalShapeCount;
  FCurrentObjCount := 0;
  FDeltaCnt := 0;
  FPercentDone := 25;
  FProgressStep := Trunc(FTotalObjCount * cnstProgressStep / 75);
  if FProgressStep > 0 then
    AModeller.OnNotify[mnPerform] := OnModellerPerform
  else
    AModeller.OnNotify[mnPerform] := nil;
end;

procedure TsgCommonBrepImage.StopLoading;
begin
  inherited StopLoading;
  FStopLoad := True;
end;

{ TsgIGESVectorFile }

function TsgIGESVectorFile.GetBrepGroupClass: TClass;
begin
{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
  if cnstOpenCascadeForcedOpen then
    Result := TsgBrepStep
  else
    Result := TsgBrepIges;
{$ELSE}
  Result := TsgBrepIges;
{$ENDIF}
end;

{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
procedure TsgIGESVectorFile.LoadFromFile(const FileName: string);
var
  vFileName: string;
  vRootName: string;
begin
  if cnstOpenCascadeForcedOpen then
  begin
    vFileName := ChangeFileExt(ExtractFileName(FileName), '.step');
    vRootName := GetTempDir + sTempFileName + IntToStr(Int64(Self)) + '.step';// + vFileName;
    vRootName := String(AnsiString(vRootName));
  {$IFDEF MSWINDOWS}
    vFileName := ExtractShortPathName(FileName);
  {$ELSE}
    vFileName := FileName;
  {$ENDIF}
    sgConvertIgesStep(vFileName, vRootName,
      ExtractFileExt(FileName), ExtractFileExt(vRootName));
    try
      inherited LoadFromFile(vRootName);
      Self.FileName := FileName;
      Converter.FileName := ExtractFileName(FileName);
    finally
      DeleteFile(vRootName);
    end;
  end
  else
    inherited LoadFromFile(FileName);
end;
{$ENDIF}

function TsgIGESVectorFile.GetBrepFileIOClass: TClass;
begin
{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
  if cnstOpenCascadeForcedOpen then
    Result := TsgModImportSTEP
  else
    Result := TsgIGESFileIO;
{$ELSE}
  Result := TsgIGESFileIO;
{$ENDIF}

end;

{ TsgSTEPVectorFile }

function TsgSTEPVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepStep;
end;

function TsgSTEPVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportSTEP;//TsgSTEPFileIO;
end;

{ TsgSTPZVectorFile }

function TsgSTPZVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportSTPZ;
end;

{ TsgBrepVectorFile }

function TsgBrepVectorFile.GetBrepGroupClass: TClass;
begin
//{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
//  Result := TsgBrepStep;
//{$ELSE}
  Result := TsgBrepBrep;
//{$ENDIF}
end;

function TsgBrepVectorFile.GetBrepFileIOClass: TClass;
begin
//{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
//  Result := TsgModImportSTEP;
//{$ELSE}
  Result := TsgModImportBREP;
//{$ENDIF}

end;

//{$IFDEF SG_USE_OPENCASCADE_FORCED_OPEN}
//procedure TsgBrepVectorFile.LoadFromFile(const FileName: string);
//var
//  vFileName: string;
//  vRootName: string;
//begin
//  vFileName := ChangeFileExt(ExtractFileName(FileName), '.step');
//  vRootName := GetTempDir + sTempFileName + vFileName;
//  sgConvertIgesStep(FileName, vRootName);
//  try
//    inherited LoadFromFile(vRootName);
//  finally
//    DeleteFile(vRootName);
//  end;
//end;
//{$ENDIF}

{ TsgParasolidVectorFile }

function TsgParasolidVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepParasolid;
end;

function TsgParasolidVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgPslFileIO;
end;

{ TsgSolidWorksBaseVectorFile }

function TsgSolidWorksBaseVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepParasolid;
end;

{ TsgSldPrtVectorFile }

function TsgSldPrtVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportSolidWorks;
end;

{ TsgSldAsmVectorFile }

function TsgSldAsmVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportSolidWorks;
end;

{ TsgInventorVectorFile }

function TsgInventorVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepBrep;
end;

function TsgInventorVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportIPT;
end;

{ TsgACISVectorFile }

function TsgACISVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepBrep;
end;

function TsgACISVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportACIS;
end;

{$IFDEF SG_USE_MCP3D}

{ TsgMCPVectorFile }

function TsgMCPVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepBrep;
end;

function TsgMCPVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportMccMt;
end;

{$ENDIF}

{$IFDEF SG_USE_IFC}
function TsgIFCVectorFile.GetBrepGroupClass: TClass;
begin
  Result := TsgBrepBrep;
end;

function TsgIFCVectorFile.GetBrepFileIOClass: TClass;
begin
  Result := TsgModImportIFC;
end;
{$ENDIF}

initialization
{$IFNDEF SG_FIREMONKEY}
  TPicture.RegisterFileFormat('sat', sSATImage, TsgACISVectorFile);
  TPicture.RegisterFileFormat('sab', sSATImage, TsgACISVectorFile);
  TPicture.RegisterFileFormat('fsat', sSATImage, TsgACISVectorFile);
  TPicture.RegisterFileFormat('smt', sSATImage, TsgACISVectorFile);
  TPicture.RegisterFileFormat('iges', cnstDescrIGES, TsgIGESVectorFile);
  TPicture.RegisterFileFormat('igs', cnstDescrIGES, TsgIGESVectorFile);
  TPicture.RegisterFileFormat('step', cnstDescrSTEP, TsgSTEPVectorFile);
  TPicture.RegisterFileFormat('stp', cnstDescrSTEP, TsgSTEPVectorFile);
  TPicture.RegisterFileFormat('brep', cnstDescrBrep, TsgBrepVectorFile);
  TPicture.RegisterFileFormat('stpz', cnstDescrSTEP, TsgSTEPVectorFile);
  TPicture.RegisterFileFormat('x_t', cnstDescrParasolid, TsgParasolidVectorFile);
  TPicture.RegisterFileFormat('x_b', cnstDescrParasolid, TsgParasolidVectorFile);
  TPicture.RegisterFileFormat('xmp_bin', cnstDescrParasolid, TsgParasolidVectorFile);
  TPicture.RegisterFileFormat('sldprt', cnstDescrSldPrt, TsgSldPrtVectorFile);
  TPicture.RegisterFileFormat('sldasm', cnstDescrSldAsm, TsgSldAsmVectorFile);
  TPicture.RegisterFileFormat('ipt', cnstDescrInventor, TsgInventorVectorFile);
{$IFDEF SG_USE_MCP3D}
  TPicture.RegisterFileFormat('mcp', cnstDescrMCP3D, TsgMCPVectorFile);
{$ENDIF}
{$IFDEF SG_USE_IFC}
  TPicture.RegisterFileFormat('ifc', cnstDescrIFC, TsgIFCVectorFile);
{$ENDIF}
{$ENDIF}

finalization
{$IFNDEF SG_FIREMONKEY}
  TPicture.UnRegisterGraphicClass(TsgACISVectorFile);
  TPicture.UnRegisterGraphicClass(TsgIGESVectorFile);
  TPicture.UnRegisterGraphicClass(TsgSTEPVectorFile);
  TPicture.UnRegisterGraphicClass(TsgParasolidVectorFile);
  TPicture.UnRegisterGraphicClass(TsgSldPrtVectorFile);
  TPicture.UnRegisterGraphicClass(TsgSldAsmVectorFile);
  TPicture.UnRegisterGraphicClass(TsgInventorVectorFile);
{$IFDEF SG_USE_MCP3D}
  TPicture.UnRegisterGraphicClass(TsgMCPVectorFile);
{$ENDIF}
{$ENDIF}

end.

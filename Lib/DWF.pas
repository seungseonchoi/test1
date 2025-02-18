{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                  DWF files TGraphic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit DWF;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  sgConsts, DXFConv, CADImage, W2D, sgZip
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
;

const
  SDWFImage = 'DWF image';

var
  CF_DWF: Word;

type
{ TsgDWFImage
  Top-level class. }
  TsgDWFImage = class(TsgVectorImage)
  private
    FCoef: Double;
    FPercent: Integer;
    FSize: Integer;
    procedure ParserProgress(Sender: TObject; Percent: Double);
    procedure ParseAndLoadToStream(const AW2DParser: TwxdParser;
      const ALayout: TsgDXFLayout; const AStream: TMemoryStream);
  protected
    procedure ApplyLineWeightFactor(var ALineWeight: Double); override;
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;

implementation

uses
  sgFunction;

type
  TsgDXFGroupAccess = class(TsgDXFGroup);
  TdwfConverter = class(TsgDXFConverter);

function IsOldDWF(const AStream: TStream): Boolean;
var
  vS1, vS2: AnsiString;
  vDS: Char;
begin
  Result := False;
  SetLength(vS1, 6);
  SetLength(vS2, 5);
  AStream.Read(PAnsiChar(vS1)^, Length(vS1));
  AStream.Read(PAnsiChar(vS2)^, Length(vS2));
  AStream.Position := AStream.Position - 11;
  if vS1 = '(DWF V' then
  begin
    vDS := SetDecimalSeparator('.');
    try
      try
        Result := StrToFloat(string(vS2)) <= 0.55
      except
      end;//FI:W501 ignore
    finally
      SetDecimalSeparator(vDS);
    end;
  end
  else if vS1 = '(W2D V' then
    Result := True;
end;

{ TsgDWFImage }

procedure TsgDWFImage.ParserProgress(Sender: TObject; Percent: Double);
begin
  Progress(Self, psRunning, Round(FPercent + Percent * FCoef), False,
    sgEmptyProgressRect, '');
end;

procedure TsgDWFImage.ApplyLineWeightFactor(var ALineWeight: Double);
begin
  ALineWeight := ALineWeight * 0.25;
end;

procedure TsgDWFImage.ParseAndLoadToStream(const AW2DParser: TwxdParser;
    const ALayout: TsgDXFLayout; const AStream: TMemoryStream);
var
  I: Integer;
  vEntity : TsgDXFEntity;
  vInsert: TsgDXFInsert;
  vBox: TFRect;
  vBackGround: TsgDXFSolid;
  vIsSetBackgroundColor: Boolean;
begin
  AW2DParser.OnProgress := ParserProgress;
  Converter.Loads(ALayout);//by set converter
  AW2DParser.LoadFromStream(Converter, AStream);

  vIsSetBackgroundColor := AW2DParser.IsSetBackgroundColor;

  if vIsSetBackgroundColor then
  begin
    vInsert := TsgDXFInsert.Create;
    vInsert.Layer := Converter.LayerByName('0');
    vInsert.Block := TsgDXFBlock.Create;
    vInsert.Block.Name := 'DWF' + IntToHex(Integer(vInsert.Block), 0);
    vInsert.ColorCAD := MakeColorCAD(acRGBColor,AW2DParser.BackgroundColor);
    Converter.Sections[csBlocks].AddEntity(vInsert.Block);

    ALayout.AddEntity(vInsert);
  end
  else
    vInsert := nil;

  for I := 0 to AW2DParser.EntitiesList.Count - 1 do
  begin
    vEntity := TsgDXFEntity(AW2DParser.EntitiesList[I]);
    if vIsSetBackgroundColor then
      vInsert.Block.AddEntity(vEntity)
    else
      ALayout.AddEntity(vEntity);
    if Assigned(Converter.OnCreate) then
      Converter.OnCreate(vEntity);
    Converter.Loads(vEntity);
  end;

  if vIsSetBackgroundColor then
  begin
    Converter.Loads(vInsert.Block);
    vBox := vInsert.Block.Box;

    {$IFDEF SG_FIREMONKEY}
    vBox.Z1 := -0.5;
    vBox.Z2 := vBox.Z1;
    {$ENDIF}

    vInsert.Block.IsLoaded := False;

    vBackGround := TsgDXFSolid.Create;
    vBackGround.Layer := TdwfConverter(Converter).GetBackGroundLayer;
    vInsert.Block.InsertEntity(0, vBackGround);
    vBackGround.ColorCAD :=  MakeColorCAD(acIndexColor, clDXFByBlock);
    vBackGround.Point := vBox.TopLeft;
    vBackGround.Point1 := MakeFPoint(vBox.Right, vBox.Top, vBox.Z1);
    vBackGround.Point3 := vBox.BottomRight;
    vBackGround.Point2 := MakeFPoint(vBox.Left, vBox.Bottom, vBox.Z2);
    Converter.Loads(vBackGround);
    Converter.Loads(vInsert.Block);
    Converter.Loads(vInsert);
  end;

  Converter.Loads(ALayout);
end;

procedure TsgDWFImage.LoadFromStream(AStream: TStream);
const
  cnstNoFiles = 'There are no W2D files in this DWF package';
var
  I: Integer;
  vW2DParser: TwxdParser;
  vDWFUnzip: TsgDWFUnzip;
  vStream: TMemoryStream;
  vPlotSection: TsgEPlotSection;
  vLayoutName: string;
  vLayout: TsgDXFLayout;
  vActiveVPort: TsgDXFVport;
  vBox: TFRect;

  procedure Extract(const ADWFUnzip: TsgDWFUnzip;
    const APlotSection: TsgEPlotSection; const AStream: TStream);
  begin
    Inc(FSize, ADWFUnzip.CurrentSize);
    if not ADWFUnzip.ExtractFile(APlotSection.IndInZip, AStream) then
      raise EInvalidGraphic.Create('Invalid DWF data');
    FPercent := Round(100.0 * FSize / ADWFUnzip.TotalSize);
    FCoef := ADWFUnzip.CurrentSize / ADWFUnzip.TotalSize;
  end;

  function GetLayout(const AIndex: Integer; const ALayoutName: string): TsgDXFLayout;
  begin
    if AIndex < Converter.LayoutsCount then
      Result := Converter.Layouts[AIndex]
    else
      Result := TsgDXFLayout(Converter.NewNamedEntity(Converter.Sections[csLayouts],
        TsgDXFLayout, vLayoutName));
  end;

begin
  inherited LoadFromStream(AStream);
  vStream := nil;

  //InitW2D;

  Converter.InitializeSectionsBegin;
  TdwfConverter(Converter).SetLoading(True, nil, DoUpdateEvent);
  vW2DParser := nil;
  vDWFUnzip := TsgDWFUnzip.Create;
  try
    Progress(Self, psStarting, 0, False, sgEmptyProgressRect, '');
    vStream := TMemoryStream.Create;
    vLayoutName := '';
    FCoef := 1.0;
    Converter.ClearDrawingProp;
    if IsOldDWF(AStream) then
      vStream.LoadFromStream(AStream)
    else
    begin
      vDWFUnzip.LoadFromStream(AStream);
      if vDWFUnzip.EPlotSections.Count = 0 then
      begin
        GetExtents;
        raise EInvalidGraphic.Create(cnstNoFiles);
      end;
    end;
//    BackgroundColor := vW2DParser.BackgroundColor;
    while Converter.LayoutsCount > 1 do
    begin
      vLayout := Converter.Layouts[Converter.LayoutsCount - 1];
      Converter.DeleteLayout(vLayout);
      vLayout.Free;
    end;
    if vDWFUnzip.EPlotSections.Count = 0 then
    begin
      vW2DParser := Tw2dParser.Create;
      try
        vLayout := Converter.Layouts[0];
        vLayout.CADSpace := cs2D;
        ParseAndLoadToStream(vW2DParser, vLayout, vStream);
      finally
        vW2DParser.Free;
      end;
    end
    else
      for I := 0 to vDWFUnzip.EPlotSections.Count - 1 do
      begin
        vPlotSection := TsgEPlotSection(vDWFUnzip.EPlotSections[I]^);
        vStream.Clear;
        Extract(vDWFUnzip, vPlotSection, vStream);
        if vPlotSection.LayoutIndex >= 0 then
          vLayoutName := vDWFUnzip.Layouts[vPlotSection.LayoutIndex]
        else
          vLayoutName := ExtractFileName(vPlotSection.Graphic);
        if Converter.LayoutByName(vLayoutName) <> nil then
          vLayoutName := vLayoutName + '_' + IntToStr(I);

        if (vDWFUnzip.EPlotSections.Count = 0) then
          vW2DParser := Tw2dParser.Create
        else
        begin
          case vPlotSection.FileExt of
            feTIFF: vW2DParser := Tw2dParserOverlay.Create;
            feW2D: vW2DParser := Tw2dParser.Create;
            feW3D: vW2DParser := Tw3dParser.Create;
          end;
        end;
//
//
//        if (vDWFUnzip.EPlotSections.Count = 0) or (vPlotSection.FileExt = feW2D)
//           or (vPlotSection.FileExt = feTIFF) then
//          vW2DParser := Tw2dParser.Create
//        else
//          vW2DParser := Tw3dParser.Create;
        try
          vLayout := GetLayout(I, vLayoutName);
          if vPlotSection.FileExt = feW3D then
            vLayout.CADSpace := cs3D
          else
            vLayout.CADSpace := cs2D;
          ParseAndLoadToStream(vW2DParser, vLayout, vStream);
        finally
          vW2DParser.Free;
        end;
      end;
    TdwfConverter(Converter).SetLoading(False, nil, nil);
    vActiveVPort := TsgDXFVport(Converter.Sections[csVPorts].FindEntByName(sActiveVPort));
    if Assigned(vActiveVPort) and sgIsZero(vActiveVPort.ViewHeight) then
    begin
      vBox := TdwfConverter(Converter).GetModelLayout.Box;
      vActiveVPort.ViewHeight := vBox.Top - vBox.Bottom;
      vActiveVPort.ViewCenterPoint := MiddleFPoint(vBox.TopLeft, vBox.BottomRight);
    end;
    SetDefaultViewPort(Converter);
    GetExtents;
    SetDefaultPlotSettings;
  finally
    vStream.Free;
    vDWFUnzip.Free;
    Progress(Self, psEnding, 100, False, sgEmptyProgressRect, '');
  end;
end;

initialization
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_DWF := RegisterClipboardFormat('DWF Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_DWF, TsgDWFImage);
  TPicture.RegisterFileFormat('DWF', SDWFImage, TsgDWFImage);
  TPicture.RegisterFileFormat('DWFX', SDWFImage, TsgDWFImage);

finalization
  TPicture.UnRegisterGraphicClass(TsgDWFImage);

end.

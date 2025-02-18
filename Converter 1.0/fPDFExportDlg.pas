unit fPDFExportDlg;

{$INCLUDE SGDXF.INC}

interface

uses
  Windows, Classes, SysUtils, Controls, Forms, StdCtrls, fCustomExport, CADExport,
{$IFDEF SGMULLANG}
  sgMultilang,
{$ENDIF}
  IniFiles;

const
  cnstStaticLineWeight = 0.0254;

  function PDFExportDlgExecute(AOwner: TComponent; AOptions: TCustomIniFile;
   AExportParams: PsgExportParams): Boolean;

implementation

uses
  Consts;

type
  TfmPDFExport = class(TForm)
  private
    FbtnOk: TButton;
    FbtnCancel: TButton;
    FbtnApply: TButton;
    FFrame: TfrmCustomExport;
    FOptions: TCustomIniFile;
{$IFDEF SGMULLANG}
    FMLText: TsgMultiLang;
    procedure MLTextAfterLoad(Sender: TObject);
{$ENDIF}
    procedure ApplyClick(Sender: TObject);
    procedure FrameChanged(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure SetOptions(const Value: TCustomIniFile);
  protected
    property btnApply: TButton read FbtnApply;
    property Frame: TfrmCustomExport read FFrame;
    property Options: TCustomIniFile read FOptions write SetOptions;
  public
    constructor CreatePDFExportDlg(AOwner: TComponent; AOptions: TCustomIniFile;
      AExportParams: PsgExportParams);
{$IFDEF SGMULLANG}
    property MLText: TsgMultiLang read FMLText;
{$ENDIF}
  end;

function PDFExportDlgExecute(AOwner: TComponent;  AOptions: TCustomIniFile;
  AExportParams: PsgExportParams): Boolean;
var
  fmPDFExport: TfmPDFExport;
begin
  fmPDFExport := TfmPDFExport.CreatePDFExportDlg(Application, AOptions,
    AExportParams);
  try
    fmPDFExport.btnApply.Enabled := False;
    Result := fmPDFExport.ShowModal = mrOk;
    if Result and Assigned(AExportParams) then
      GetExportParam(fmPDFExport.Frame, AExportParams);
  finally
    fmPDFExport.Free;
  end;
end;

{ TfmPDFExport }

procedure TfmPDFExport.ApplyClick(Sender: TObject);
begin
  Frame.SaveOptions(FOptions);
  FbtnApply.Enabled := False;
end;

constructor TfmPDFExport.CreatePDFExportDlg(AOwner: TComponent;
  AOptions: TCustomIniFile; AExportParams: PsgExportParams);
const
  cnstButtonHeight = 23;
  iControlOffset = 8;

  function CreateButton(AForm: TCustomForm; const ACaption: string;
    const AModalResult: TModalResult; const ADefault, ACancel: Boolean;
    const AIndex: Integer; AOnClick: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(AForm);
    Result.Parent := AForm;
    Result.Caption := ACaption;
    Result.Default := ADefault;
    Result.Cancel := ACancel;
    Result.ModalResult := AModalResult;
    Result.SetBounds(AForm.ClientWidth - AIndex * (Result.Width + iControlOffset),
      AForm.ClientHeight - cnstButtonHeight - iControlOffset,
      Result.Width, cnstButtonHeight);
    Result.OnClick := AOnClick;
  end;

begin
  inherited CreateNew(AOwner);
  BorderStyle := bsSingle;
  BorderIcons := [biSystemMenu];
  FFrame := TfrmCustomExport.Create(Self);
  FFrame.Parent := Self;
  if Assigned(AExportParams) then
    FFrame.ExportParams := AExportParams^
  else FFrame.ExportParams := DefExportParams;
  FFrame.LoadOptions(FOptions);
  FFrame.SetBounds(iControlOffset, iControlOffset, FFrame.Width, FFrame.Height);
  FFrame.OnChanged := FrameChanged;
  ClientWidth := FFrame.Width + 2 * iControlOffset;
  ClientHeight := FFrame.Height + cnstButtonHeight + 3 * iControlOffset;
  FbtnOk := CreateButton(Self, LoadResString(@SOKButton), mrOk, True, False, 3, OkClick);
  FbtnCancel := CreateButton(Self, LoadResString(@SCancelButton), mrCancel, False, True, 2, nil);
  FbtnApply := CreateButton(Self, 'Apply', mrNone, False, False, 1, ApplyClick);
  FbtnApply.Enabled := False;
  Options := AOptions;
  if AExportParams.UseVectorLineWeight then
  begin
    Frame.lblLineWeightScale.Enabled := False;
    Frame.edtLineWeightScale.Enabled := False;
    Frame.edtLineWeightScale.Text := FloatToStr(cnstStaticLineWeight);
  end;
{$IFDEF SGMULLANG}
  FMLText := CreateMultilang(Self);
  MLTextAfterLoad(nil);
  FMLText.OnAfterLoad := MLTextAfterLoad;
{$ENDIF}
end;

procedure TfmPDFExport.FrameChanged(Sender: TObject);
begin
  FbtnApply.Enabled := True;
end;

{$IFDEF SGMULLANG}
procedure TfmPDFExport.MLTextAfterLoad(Sender: TObject);
begin
  FbtnOk.Caption := MLText.GetFromIni('', FbtnOk.Caption);
  FbtnCancel.Caption := MLText.GetFromIni('', FbtnCancel.Caption);
  FbtnApply.Caption := MLText.GetFromIni('', FbtnApply.Caption);
  Caption := MLText.GetFromIni('', 'PDF Export options');
end;
{$ENDIF}

procedure TfmPDFExport.OkClick(Sender: TObject);
begin
  FFrame.SaveOptions(FOptions);
end;

procedure TfmPDFExport.SetOptions(const Value: TCustomIniFile);
begin
  FOptions := Value;
  FFrame.LoadOptions(FOptions);
end;

end.

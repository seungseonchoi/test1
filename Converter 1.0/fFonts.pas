unit fFonts;

{$INCLUDE SGDXF.inc}

interface

uses
  {$IFDEF MSWINDOWS}
   Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, IniFiles, sgConsts, sgShellAPI;

const
  sGeneral: string = 'General';
  sSettings: string = 'Settings.ini';
  sUseSHX: string = 'Use SHX';
  sSHXPaths: string = 'SHX search paths';
  sSHXPath: string = 'SHX default path';
  sSHXFont: string = 'SHX default font';
  sUseTTF: string = 'Use TrueType fonts as curves';

type
  TfmFonts = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblSHXSearchPath: TLabel;
    edSHXSearchPath: TEdit;
    btnSHXSearchPath: TButton;
    btnSHXDefaultPath: TButton;
    edSHXDefaultPath: TEdit;
    lblDefautSHXPath: TLabel;
    lblDefaultSHXFont: TLabel;
    edSHXDefaultFont: TEdit;
    Panel1: TPanel;
    cbUseSHXFonts: TCheckBox;
    procedure cbUseSHXFontsClick(Sender: TObject);
    procedure btnSHXSearchPathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSHXDefaultPathClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FOptions: TIniFile;
  protected
    procedure LoadOptions;
    procedure SaveOptions;
  public
    { Public declarations }
  end;

var
  fmFonts: TfmFonts;

  function FontsOptionsExecute(AOptions: TIniFile; AOwner: TComponent): Boolean;

implementation

uses
  fPaths;

{$R *.dfm}

procedure TfmFonts.LoadOptions;
begin
  cbUseSHXFonts.Checked := bUseSHXFonts;
  edSHXSearchPath.Text := sSHXSearchPaths;
  edSHXDefaultPath.Text := sDefaultSHXPath;
  edSHXDefaultFont.Text := sDefaultSHXFont;
end;

procedure TfmFonts.SaveOptions;
begin
  bUseSHXFonts := cbUseSHXFonts.Checked;
  sSHXSearchPaths := edSHXSearchPath.Text;
  sDefaultSHXPath := edSHXDefaultPath.Text;
  sDefaultSHXFont := edSHXDefaultFont.Text;
  FOptions.WriteBool(sGeneral, sUseSHX, bUseSHXFonts);
  FOptions.WriteString(sGeneral, sSHXPaths, sSHXSearchPaths);
  FOptions.WriteString(sGeneral, sSHXPath, sDefaultSHXPath);
  FOptions.WriteString(sGeneral, sSHXFont, sDefaultSHXFont);
  {$IFNDEF VER100}
  FOptions.UpdateFile;
  {$ENDIF}
end;

function FontsOptionsExecute(AOptions: TIniFile; AOwner: TComponent): Boolean;
begin
  fmFonts := TfmFonts.Create(AOwner);
  fmFonts.Position := poMainFormCenter;
  with fmFonts do
  begin
    FOptions := AOptions;
    LoadOptions;
    Result := ShowModal = mrOk;
    if Result then
      SaveOptions;
    Free;
  end;
end;

procedure TfmFonts.cbUseSHXFontsClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Panel1.ControlCount - 1 do
    Panel1.Controls[I].Enabled := cbUseSHXFonts.Checked;
end;

procedure TfmFonts.btnSHXSearchPathClick(Sender: TObject);
var
  S: string;
begin
  S := edSHXSearchPath.Text;
  if GetPathsExcute(S, True) then
  begin
    edSHXSearchPath.Text := S;
  end;
end;

procedure TfmFonts.FormCreate(Sender: TObject);
begin
  LoadOptions;
  cbUseSHXFontsClick(Sender);
end;

procedure TfmFonts.btnSHXDefaultPathClick(Sender: TObject);
var
  vDir: string;
begin
  if SelectDirectory('', '', vDir, Handle) then
    edSHXDefaultPath.Text := vDir;
end;

procedure TfmFonts.btnOkClick(Sender: TObject);
begin
  {$IFNDEF VER100}
  FOptions.UpdateFile;
  {$ENDIF}
  bUseSHXFonts := cbUseSHXFonts.Checked;
end;

end.

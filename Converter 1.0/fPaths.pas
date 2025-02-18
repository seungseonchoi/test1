unit fPaths;

{$INCLUDE SGDXF.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, ShlObj,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Menus, sgConsts, sgFunction, sgShellAPI
  {$IFDEF SGDEL_XE3}
  , System.Types, System.UITypes
  {$ENDIF}
  ;

const
  sNoSHXPaths: string = 'There are no AutoCAD SHX fonts paths';

type
  TfmPaths = class(TForm)
    grPaths: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lbPaths: TListBox;
    lblPathInfo: TLabel;
    lblInvalidInfo: TLabel;
    edPath: TEdit;
    btnAddPath: TButton;
    btnReplace: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    btndeInvPaths: TButton;
    bnbtnUp: TBitBtn;
    btbtnDown: TBitBtn;
    btnAddAutoCAD: TButton;
    procedure btnAddPathClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure edPathChange(Sender: TObject);
    procedure lbPathsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure acUpdateExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btbtnDownClick(Sender: TObject);
    procedure pmiAddClick(Sender: TObject);
    procedure lbPathsClick(Sender: TObject);
    procedure btndeInvPathsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure alPathsUpdate(Sender: TObject);
    procedure btnAddAutoCADClick(Sender: TObject);
  private
    FShowAddAutoCADPath: Boolean;
    procedure UpDown(Index: Integer);
  public
    { Public declarations }
  end;

function GetPathsExcute(var APaths: string; AShowAddAutoCADPath: Boolean): Boolean;
function DirectoryExists(const Name: string): Boolean;


implementation

{$R *.dfm}

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function GetPathsExcute(var APaths: string; AShowAddAutoCADPath: Boolean): Boolean;
var
  fmPaths: TfmPaths;
  I: Integer;
  S, S1: string;
begin
  fmPaths := TfmPaths.Create(Application);
  try
    S := APaths;
    if S <> '' then
    begin
      if S[Length(S)] <> ';' then
        S := S + ';';
      while S <> '' do
      begin
        S1 := Copy(S, 1, AnsiPos(';', S) - 1);
        if Length(S1) > 0 then
          fmPaths.lbPaths.Items.Add(S1);
        Delete(S, 1, AnsiPos(';', S));
      end;
    end;
    if fmPaths.lbPaths.Items.Count > 0 then
      fmPaths.lbPaths.ItemIndex := 0;
    fmPaths.FShowAddAutoCADPath := AShowAddAutoCADPath;
    fmPaths.ShowModal;
    Result := fmPaths.ModalResult = mrOK;
    if Result then
    begin
      APaths := '';
      for I := 0 to fmPaths.lbPaths.Items.Count - 1 do
        APaths := APaths + fmPaths.lbPaths.Items[I] + ';';
    end;
  finally
    fmPaths.Free;
  end;
end;

procedure TfmPaths.btnAddPathClick(Sender: TObject);
var
  vDir: string;
begin
  if SelectDirectory('', '', vDir, Handle) then
  begin
    btnReplace.Enabled := True;
    btnAdd.Enabled := True;
    edPath.Text := vDir;
  end;
end;

procedure TfmPaths.btnAddClick(Sender: TObject);
var
  I: Integer;
  vPath: string;
begin
  vPath := Trim(edPath.Text);
  if lbPaths.Items.IndexOf(vPath) < 0 then
  begin
    I := lbPaths.Items.Add(vPath);
    lbPaths.ItemIndex := I;
    btnReplace.Enabled := False;
    btnAdd.Enabled := False;
  end;
end;

procedure TfmPaths.btnReplaceClick(Sender: TObject);
begin
  if lbPaths.ItemIndex < 0 then Exit;
  lbPaths.Items[lbPaths.ItemIndex] := edPath.Text;
  btnReplace.Enabled := False;
  btnAdd.Enabled := False;
end;

procedure TfmPaths.edPathChange(Sender: TObject);
begin
  btnReplace.Enabled := True;
  btnAdd.Enabled := True;
end;

procedure TfmPaths.lbPathsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  lbPaths.Canvas.Brush.Color := lbPaths.Color;
  lbPaths.Canvas.FillRect(Rect);
  lbPaths.Canvas.Brush.Color := lbPaths.Color;
  {$IFNDEF SGFPC}
  if odSelected in State then
  begin
    lbPaths.Canvas.Brush.Color := clHighlight;
    lbPaths.Canvas.Font.Color := clHighlightText;
  end
  else
  {$ENDIF}
  begin
    lbPaths.Canvas.Brush.Color := lbPaths.Color;
    if not DirectoryExists(lbPaths.Items[Index]) then
      lbPaths.Canvas.Font.Color := clGray
    else
      lbPaths.Canvas.Font.Color := clBlack;
  end;
  lbPaths.Canvas.FillRect(Rect);
  lbPaths.Canvas.TextOut(Rect.Left, Rect.Top, lbPaths.Items[Index]);
end;

procedure TfmPaths.acUpdateExecute(Sender: TObject);
begin
  if lbPaths.ItemIndex > 0 then UpDown(-1);
end;

procedure TfmPaths.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;

procedure TfmPaths.btbtnDownClick(Sender: TObject);
begin
  if (lbPaths.ItemIndex >= 0) and (lbPaths.ItemIndex < lbPaths.Items.Count - 1)
    and (lbPaths.Items.Count > 0) then
    UpDown(1);
end;

procedure TfmPaths.UpDown(Index: Integer);
var
  S: string;
begin
  S := lbPaths.Items[lbPaths.ItemIndex + Index];
  lbPaths.Items[lbPaths.ItemIndex + Index] := lbPaths.Items[lbPaths.ItemIndex];
  lbPaths.Items[lbPaths.ItemIndex] := S;
  lbPaths.ItemIndex := lbPaths.ItemIndex + Index;
end;

procedure TfmPaths.pmiAddClick(Sender: TObject);
var
  I: Integer;
begin
  I := lbPaths.Items.Add(edPath.Text);
  lbPaths.ItemIndex := I;
  btnReplace.Enabled := False;
  btnAdd.Enabled := False;
end;

procedure TfmPaths.lbPathsClick(Sender: TObject);
begin
  if lbPaths.ItemIndex < 0 then Exit;
  edPath.Text := lbPaths.Items[lbPaths.ItemIndex];
  btnReplace.Enabled := False;
  btnAdd.Enabled := False;
end;

procedure TfmPaths.btndeInvPathsClick(Sender: TObject);
var
  vStr: TStringList;
  I, Index: Integer;
begin
  vStr := TStringList.Create;
  try
    vStr.Assign(lbPaths.Items);
    for I := 0 to vStr.Count - 1 do
      if not DirectoryExists(vStr[I]) then
      begin
        Index := lbPaths.Items.IndexOf(vStr[I]);
        if Index >= 0 then lbPaths.Items.Delete(Index);
      end;
  finally
    vStr.Free;
  end;
end;

procedure TfmPaths.btnDeleteClick(Sender: TObject);
begin
  if lbPaths.ItemIndex >= 0 then
    lbPaths.Items.Delete(lbPaths.ItemIndex);
end;

procedure TfmPaths.alPathsUpdate(Sender: TObject);
begin
  btnReplace.Enabled := btnReplace.Enabled and (lbPaths.ItemIndex <> -1);
end;

procedure TfmPaths.btnAddAutoCADClick(Sender: TObject);
var
  vPaths: TStringList;
  I: Integer;
begin
  vPaths := TStringList.Create;
  try
    if FindSHXPaths(vPaths) then
    begin
      for I := 0 to vPaths.Count - 1 do
        if lbPaths.Items.IndexOf(vPaths[I]) < 0 then
          lbPaths.Items.Add(vPaths[I])
    end
    else
      MessageDlg(sNoSHXPaths, mtInformation, [mbOK], 0);
  finally
    vPaths.Free;
  end;
end;

end.



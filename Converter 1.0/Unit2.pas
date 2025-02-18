unit Unit2;

{$INCLUDE SGDXF.inc}

interface

uses
  {$IFDEF MSWINDOWS}
   Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, sgConsts, sgFunction, sgDrawingNavigator;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    edtCoordX: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtScale: TEdit;
    Button1: TButton;
    Button2: TButton;
    edtCoordY: TEdit;
    Label4: TLabel;
    edtCoordZ: TEdit;
    lblPercent: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  SearchPoint: TFPoint;
  vScale: Double;
  vExt: TFRect;

  procedure ShowMsg(AText: string);
  begin
    {$IFDEF MSWINDOWS}MessageBox(Handle,{$ELSE}DefaultMessageBox({$ENDIF}PChar(AText), 'Show Point Dialog', MB_ICONWARNING);
  end;

  function TryConvert(AEdit: TEdit; var AValue: Double): Boolean;
  var
    vErr: Integer;
    DS: Char;
  begin
    Result := True;
     DS := SetDecimalSeparator('.');
    try
      Val(AEdit.Text, AValue, vErr);
    finally
      SetDecimalSeparator(DS);
    end;
    if vErr <> 0 then
    begin
      AEdit.SetFocus;
      AEdit.SelectAll;
      ShowMsg('Invalid floting point value.');
      Result := False;
    end;
  end;

begin
  if Form1.Img = nil then
    ShowMsg('This function don`t work for raster images')
  else
  begin
    vExt := Form1.Img.Extents;
    if TryConvert(edtCoordX, SearchPoint.X) then
    if TryConvert(edtCoordY, SearchPoint.Y) then
    if TryConvert(edtCoordZ, SearchPoint.Z) then
    if TryConvert(edtScale, vScale) then
    if (vExt.Left > SearchPoint.X) or (vExt.Right < SearchPoint.X) or
       (vExt.Top < SearchPoint.Y) or (vExt.Bottom > SearchPoint.Y) then
      ShowMsg('This point not in drawing extents.')
    else
      Form1.sgPaintBox.ShowPoint(SearchPoint, vScale / 100);
  end;
end;

procedure TForm2.FormShow(Sender: TObject);
var
  DS: Char;
begin
  DS := SetDecimalSeparator('.');
  try
    edtScale.Text := Format('%.3f',[Form1.sgPaintBox.Scale * 100]);
  finally
    SetDecimalSeparator(DS);
  end;
end;

procedure TForm2.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, [',', GetDecimalSeparator]) then
    Key := '.';
  if not CharInSet(Key, ['0'..'9', '.', #8, #13, '-', '+', 'E', 'e'{$IFNDEF SGFPC}, #46{$ENDIF}]) then
    Key := #0;
end;

procedure TForm2.EditChange(Sender: TObject);
var
  I: Integer;
  S: string;
  vSelStart: Integer;
begin
  try
    vSelStart := 0;
    TEdit(Sender).OnChange := nil;
    try
      vSelStart := TEdit(Sender).SelStart;
      S := TEdit(Sender).Text;
      for I := 1 to Length(S) do
        if CharInSet(S[I], [',', GetDecimalSeparator]) then
          S[I] := '.';
      TEdit(Sender).Text := S;
    finally
      TEdit(Sender).SelStart := vSelStart;
      TEdit(Sender).OnChange := EditChange;
    end;
  except
  end;
end;

end.

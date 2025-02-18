unit Unit3;

{$INCLUDE SGDXF.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    rgSelection: TRadioGroup;
    rgSearchMode: TRadioGroup;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure rgSelectionClick(Sender: TObject);
    procedure rgSearchModeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  vSelMode: TSelectionMode;
begin
  case rgSelection.ItemIndex of
    0: vSelMode := smNone;
    1: vSelMode := smRectangle;
    2: vSelMode := smEllipse;
  else
    vSelMode := smNone;
  end;
  if not Form1.FindText(Edit1.Text, vSelMode, rgSearchMode.ItemIndex = 1) then
    ShowMessage(Edit1.Text + ' not found')
  else
  begin
    Form1.sgPaintBox.Repaint;
    Close;
  end;
end;

procedure TForm3.rgSelectionClick(Sender: TObject);
begin
  if rgSelection.ItemIndex = 2 then
    rgSearchMode.ItemIndex := 0;
end;

procedure TForm3.rgSearchModeClick(Sender: TObject);
begin
  if (rgSearchMode.ItemIndex = 1) and (rgSelection.ItemIndex = 2) then
    rgSelection.ItemIndex := 1;

end;

end.

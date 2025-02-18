unit fImage2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, SGImage, StdCtrls;

type
  TfmImage2 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    sgPaintBox: TsgImage;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowClippedRect(AGr: TGraphic);

var
  fmImage2: TfmImage2;

implementation

{$R *.dfm}
procedure ShowClippedRect(AGr: TGraphic);
begin
  fmImage2.sgPaintBox.Picture.Graphic := AGr;
  fmImage2.ShowModal;
end;

procedure TfmImage2.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

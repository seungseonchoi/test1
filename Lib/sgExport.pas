{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                      DXF Export                            }
{     TsgExport - class for draw and save to                 }
{     bitmap/jpeg/metafile file formats                      }
{     heirs of the class TGraphic                            }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}
unit SGExport;
{$INCLUDE SGDXF.INC}
interface

uses SysUtils, Classes, Graphics
{$IFDEF SGDEL_XE2}
  , Vcl.Imaging.jpeg
{$ELSE}
  , jpeg
{$ENDIF};

type

  TsgExportFormat = (xpBitmap,xpJPEG,xpMetafile);

  TsgExport = class
  private
    FCanvas: TCanvas;
    FWidth: Integer;
    FHeight: Integer;
    FOnPaint: TNotifyEvent;
    FActive: Boolean;
    function ExportToBitmap(G: TGraphic): TBitmap;
    function ExportToJPEG(G: TGraphic): TJPEGImage;
    function ExportToMetafile(G: TGraphic): TMetafile;
    procedure Paint(G: TGraphic);
  public
    procedure ExportTo(G: TGraphic; const FName: String; Fmt: TsgExportFormat; W,H: Integer);
    property Canvas: TCanvas read FCanvas;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Active: Boolean read FActive;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

implementation

procedure TsgExport.ExportTo(G: TGraphic; const FName: String; Fmt: TsgExportFormat; W, H: Integer);
var XG: TGraphic;
begin
  if W <= 0 then W := G.Width;
  if H <= 0 then H := G.Height;
  FWidth := W; FHeight := H;
  XG := nil;
  try
    case Fmt of
      xpBitmap:   XG := ExportToBitmap(G);
      xpJPEG:     XG := ExportToJPEG(G);
      xpMetafile: XG := ExportToMetafile(G);
    end;
    if XG <> nil then XG.SaveToFile(FName);
  finally
    XG.Free;
  end;
end;

function TsgExport.ExportToBitmap(G: TGraphic): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf16bit;
  Result.Width := Width;
  Result.Height := Height;
  FCanvas := Result.Canvas;
  Paint(G);
end;

function TsgExport.ExportToJPEG(G: TGraphic): TJPEGImage;
var BM: TBitmap;
begin
  BM := ExportToBitmap(G);
  try
    Result := TJPEGImage.Create;
    Result.Assign(BM);
  finally
    BM.Free;
  end;
end;

function TsgExport.ExportToMetafile(G: TGraphic): TMetafile;
begin
  Result := TMetafile.Create;
  Result.Width := Width;
  Result.Height := Height;
  FCanvas := TMetafileCanvas.Create(Result,0);
  try Paint(G) finally FCanvas.Free end;
end;

procedure TsgExport.Paint(G: TGraphic);
begin
  FActive := True;
  try
    FCanvas.StretchDraw(Rect(0,0,Width,Height), G);
    if Assigned(OnPaint) then OnPaint(Self);
  finally
    FActive := False;
  end;
end;

end.

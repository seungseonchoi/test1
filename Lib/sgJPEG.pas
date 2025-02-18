{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{             TJPEGImage descendant with CMYK support        }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgJPEG;
{$I SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, ActiveX,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPImage,
{$ELSE}
{$IFDEF SGDEL_XE2}Vcl.Imaging.jpeg,{$ELSE}jpeg,{$ENDIF}
{$ENDIF}
  Graphics, Classes;

type
  TsgJPEGDotsMetric = (jdmNone, jdmInch, jdmCm);

  TsgJPEGImage = class(TJPEGImage)
  private
    FDPUX: Integer;
    FJPEGDotsMetric: TsgJPEGDotsMetric;
    FDPUY: Integer;
    function GetBmpPixelFormat: Graphics.TPixelFormat;
    procedure SetBmpPixelFormat(const Value: Graphics.TPixelFormat);
  protected
    class procedure ReadResParams(Stream: TStream;
      var AHorzRes, AVertRes: Integer; var AJPEGDotsMetric: TsgJPEGDotsMetric);
    class function SeekResPosition(Stream: TStream): Boolean;
    class procedure WriteResParams(Stream: TStream; AHorzRes, AVertRes: Integer;
      const AJPEGDotsMetric: TsgJPEGDotsMetric);
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property DPUX: Integer read FDPUX write FDPUX;
    property DPUY: Integer read FDPUY write FDPUY;
    property JPEGDotsMetric: TsgJPEGDotsMetric read FJPEGDotsMetric
      write FJPEGDotsMetric;
    property BmpPixelFormat: Graphics.TPixelFormat read GetBmpPixelFormat write SetBmpPixelFormat;
  end;

{$IFDEF SG_USEGDIPLUS}
function LoadWithGDIPlus(const AFileName: string; ABmp: TBitmap): Boolean;
{$ENDIF}

implementation

{$IFDEF SG_USEGDIPLUS}
uses
{$IFDEF SG_WINAPI_GDIPLUS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL;
{$ELSE}
  GDIPlus;
{$ENDIF}
{$ENDIF}

{$IFDEF SG_USEGDIPLUS}
function LoadWithGDIPlus(const AFileName: string; ABmp: TBitmap): Boolean;
var
  vGDIPlusImage: TGPImage;
  vGDIPlus: TGPGraphics;
  procedure LoadFromPicture;
  var
    vPicture: TPicture;
  begin
    vPicture := TPicture.Create;
    try
      try
        vPicture.LoadFromFile(AFileName);
        ABmp.PixelFormat := pf24bit;
        ABmp.Width := vPicture.Graphic.Width;
        ABmp.Height := vPicture.Graphic.Height;
        ABmp.Canvas.StretchDraw(Rect(0, 0, ABmp.Width, ABmp.Height), vPicture.Graphic);
        Result := True;
      except
        vPicture.Free;
        vPicture := nil;
        Exit;
      end;
    finally
      vPicture.Free;
    end;
  end;

begin
  Result := False;
{$IFNDEF SG_WINAPI_GDIPLUS}
  if GDIPlusLoaded = 0 then
{$ENDIF}
  begin
    vGDIPlusImage := TGPImage.Create(AFileName);
    try
      if vGDIPlusImage.{$IFDEF SG_WINAPI_GDIPLUS}GetLastStatus{$ELSE}GetStatusOfFunction{$ENDIF} = Ok then
      begin
        ABmp.Width := vGDIPlusImage.GetWidth;
        ABmp.Height := vGDIPlusImage.GetHeight;
        ABmp.PixelFormat := pf32bit;
        vGDIPlus := TGPGraphics.Create(ABmp.Canvas.Handle);
        try
          if vGDIPlus.{$IFDEF SG_WINAPI_GDIPLUS}GetLastStatus{$ELSE}GetStatusOfFunction{$ENDIF} = Ok then
            Result := vGDIPlus.DrawImage(vGDIPlusImage, 0, 0, vGDIPlusImage.GetWidth, vGDIPlusImage.GetHeight) = Ok
          else
            LoadFromPicture;
          if not Result then LoadFromPicture;
        finally
          vGDIPlus.Free;
        end;
      end;
    finally
      vGDIPlusImage.Free;
    end;
  end
{$IFNDEF SG_WINAPI_GDIPLUS}
  else
    LoadFromPicture;
{$ENDIF}
end;
{$ENDIF}
{ TsgJPEGImage }

constructor TsgJPEGImage.Create;
begin
  inherited Create;
  FJPEGDotsMetric := jdmInch;
  FDPUX := 300;
  FDPUY := 300;
end;

function TsgJPEGImage.GetBmpPixelFormat: Graphics.TPixelFormat;
{$IFNDEF SGFPC}
var
  vJPEGPixelFormat: TJPEGPixelFormat;
begin
  vJPEGPixelFormat := PixelFormat;
  case vJPEGPixelFormat of
    jf24Bit:   Result := pf24bit;
    jf8Bit:    Result := pf8bit;   
  else
    Result :=  pfCustom;
  end;
end;
{$ELSE}
begin
  Result := inherited PixelFormat;
end;
{$ENDIF}

{$IFDEF SG_USEGDIPLUS}
procedure TsgJPEGImage.LoadFromStream(Stream: TStream);
var
  vGDIPlusImage: TGPImage;
  vGDIPlus: TGPGraphics;
  vBmp: TBitmap;
  vDataHandle: HGlobal;
  vBuffer: Pointer;
  vStrm: IStream;
  vRes: Boolean;
begin
  vBmp := nil;
  vGDIPlusImage := nil;
  vGDIPlus := nil;
  vRes := False;
  vDataHandle := 0;
  vStrm := nil;
  try
{$IFNDEF SG_WINAPI_GDIPLUS}
    if GDIPlusLoaded = 0 then
{$ENDIF}
    begin
      vDataHandle := GlobalAlloc(GMEM_MOVEABLE, Stream.Size);
      try
        if vDataHandle <> 0 then
        begin
          vBuffer := GlobalLock(vDataHandle);
          if vBuffer <> nil then
          begin
            Stream.Position := 0;
            Stream.Read(vBuffer^, Stream.Size);
            GlobalUnlock(vDataHandle);
            if Succeeded(CreateStreamOnHGlobal(vDataHandle, True, vStrm)) then
            begin
              vGDIPlusImage := TGPImage.Create(vStrm);
              try
                if vGDIPlusImage.{$IFDEF SG_WINAPI_GDIPLUS}GetLastStatus{$ELSE}GetStatusOfFunction{$ENDIF} = Ok then
                begin
                  vBmp := TBitmap.Create;
                  try
                    vBmp.Transparent := False;
                    vBmp.Width := vGDIPlusImage.GetWidth;
                    vBmp.Height := vGDIPlusImage.GetHeight;
                    vBmp.PixelFormat := pf32bit;
                    vGDIPlus := TGPGraphics.Create(vBmp.Canvas.Handle);
                    try
                      if vGDIPlus.{$IFDEF SG_WINAPI_GDIPLUS}GetLastStatus{$ELSE}GetStatusOfFunction{$ENDIF} = Ok then
                      begin
                        if vGDIPlus.DrawImage(vGDIPlusImage, 0, 0, vGDIPlusImage.GetWidth, vGDIPlusImage.GetHeight) = Ok then
                        begin
                          Assign(vBmp);
                          vRes := True;
                        end;
                      end;
                    finally
                      vGDIPlus.Free;
                      vGDIPlus := nil;
                    end;
                  finally
                    vBmp.Free;
                    vBmp := nil;
                  end;
                end;
              finally
                vGDIPlusImage.Free;
                vGDIPlusImage := nil;
                vStrm := nil;
              end;
            end;
          end;
        end;
      finally
        if vDataHandle <> 0 then GlobalFree(vDataHandle);
        vDataHandle := 0;
      end;
    end;
  except
    vGDIPlus.Free;
    vBmp.Free;
    vGDIPlusImage.Free;
    if vDataHandle <> 0 then GlobalFree(vDataHandle);
    vStrm := nil;
    vRes := False;
  end;
  if not vRes then
  begin
    if Stream.Position <> 0 then
      Stream.Position := 0;
    inherited LoadFromStream(Stream);
  end;
  ReadResParams(Stream, FDPUX, FDPUY, FJPEGDotsMetric);
end;
{$ELSE}
procedure TsgJPEGImage.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
end;
{$ENDIF}

class procedure TsgJPEGImage.ReadResParams(Stream: TStream; var AHorzRes,
  AVertRes: Integer; var AJPEGDotsMetric: TsgJPEGDotsMetric);
var
  vPosition: Int64;
begin
  vPosition := Stream.Position;
  try
    if SeekResPosition(Stream) then
    begin
      AHorzRes := 0;
      AVertRes := 0;
      Stream.Read(AJPEGDotsMetric, 1);
      Stream.Read(AHorzRes, 2);
      Stream.Read(AVertRes, 2);
      AHorzRes := Swap(AHorzRes);
      AVertRes := Swap(AVertRes);
    end;
  finally
    Stream.Position := vPosition;
  end;
end;

procedure TsgJPEGImage.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteResParams(Stream, FDPUX, FDPUY, FJPEGDotsMetric);
end;

class function TsgJPEGImage.SeekResPosition(Stream: TStream): Boolean;
var
  vBufferSize, vIndex: integer;
  vStrBuf: array[1..50] of AnsiChar;
begin
  Result := False;
  Stream.Seek(0, soFromBeginning);
  vBufferSize := High(vStrBuf);
  FillChar(vStrBuf, SizeOf(vStrBuf), #0);
  //SetLength(vStrBuf, vBufferSize);
  Stream.Read(vStrBuf[1], vBufferSize);
  vIndex := Pos(AnsiString('JFIF' + #0), AnsiString(vStrBuf));
  if vIndex > 0 then
  begin
    Stream.Seek(vIndex + 6, soFromBeginning);
    Result := True;
  end;
end;

procedure TsgJPEGImage.SetBmpPixelFormat(const Value: Graphics.TPixelFormat);
{$IFNDEF SGFPC}
begin
  case Value of
    pf1bit, pf4bit, pf8bit:  PixelFormat := jf8Bit;
  else  
    PixelFormat := jf24Bit;  
  end;
end;
{$ELSE}
begin
  inherited PixelFormat := Value;
end;
{$ENDIF}

class procedure TsgJPEGImage.WriteResParams(Stream: TStream; AHorzRes,
  AVertRes: Integer; const AJPEGDotsMetric: TsgJPEGDotsMetric);
var
  vPosition: Int64;
begin
  vPosition := Stream.Position;
  try
    if SeekResPosition(Stream) then
    begin
      Stream.Write(AJPEGDotsMetric, 1);
      AHorzRes := Swap(AHorzRes);
      AVertRes := Swap(AVertRes);
      Stream.Write(AHorzRes, 2);
      Stream.Write(AVertRes, 2);
    end;
  finally
    Stream.Position := vPosition;
  end;
end;

initialization

 // TPicture.UnRegisterGraphicClass(TJPEGImage);
  TPicture.RegisterFileFormat('jpeg', 'JPEG Image File', TsgJPEGImage);
  TPicture.RegisterFileFormat('jpg', 'JPEG Image File', TsgJPEGImage);

finalization
  TPicture.UnRegisterGraphicClass(TsgJPEGImage);

end.


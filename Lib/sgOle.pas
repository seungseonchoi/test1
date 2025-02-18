{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{              Converting OLE<->TGraphic functions           }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgOle;
{$INCLUDE SGDXF.inc}

interface

uses
   Windows, Classes,  SysUtils, ActiveX, ComObj, sgConsts, sgFunction,
   sgBitmap,
{$IFDEF SG_FM_WINDOWS}
   FMX.Graphics, FMX.Types, sgFMXTypes, sgProxyGraphics, FMX.Helpers.Win
{$ELSE}
   Graphics
{$ENDIF}
   ;

const
  sgStreamSignature = $434F4442;

type
  TStreamHeader = record
  case Integer of
    0: ( { New }
      Signature: Integer;
      DrawAspect: Integer;
      DataSize: Integer);
    1: ( { Old }
      PartRect: TSmallRect);
  end;

function CreateSTGMFromImage(const AGraphic: TPersistent;
  const AFormat: TClipFormat = CF_METAFILEPICT): AnsiString;
function OleToGraphic(AOle: IOleObject; var AGraphic: TGraphic): Boolean;

implementation

const
  sg0_001MMPerInch = 2540;

type
  TsgOleDataObject = class(TInterfacedObject, IDataObject)
  private
    FGraphic: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TPersistent{$ENDIF};
  public
    constructor Create(AGraphic: TPersistent); virtual;
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium):
      HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium):
      HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
      stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcOut: TFormatEtc): HRESULT; stdcall;
    function SetData(const FormatEtc: TFormatEtc;
      {$IFDEF MSWINDOWS}var{$ELSE}const{$ENDIF} Medium: TStgMedium;
      fRelease: BOOL): HRESULT; stdcall;

    function EnumFormatEtc(dwDirection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}; out EnumFormatEtc:
      IEnumFormatEtc): HRESULT; stdcall;
    function DAdvise(const FormatEtc: TFormatEtc; advf: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF};
      const advSink: IAdviseSink; out dwConnection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}): HRESULT; stdcall;
    function DUnadvise(dwConnection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}): HRESULT; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT;
      stdcall;
  end;

{TsgOleDataObject}

constructor TsgOleDataObject.Create(AGraphic: TPersistent);
begin
  inherited Create;
  FGraphic := {$IFNDEF SG_FIREMONKEY}TGraphic(AGraphic){$ELSE}AGraphic{$ENDIF};
end;

function TsgOleDataObject.DAdvise(const FormatEtc: TFormatEtc;
  advf: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}; const advSink: IAdviseSink;
  out dwConnection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TsgOleDataObject.DUnadvise(dwConnection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF}): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TsgOleDataObject.EnumDAdvise(
  out enumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TsgOleDataObject.EnumFormatEtc(dwDirection: {$IFDEF SGFPC}DWord{$ELSE}Longint{$ENDIF};
  out EnumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  EnumFormatEtc := nil;
  Result := E_NOTIMPL;
end;

function TsgOleDataObject.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcOut: TFormatEtc): HRESULT;
begin
  FormatEtcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

function TsgOleDataObject.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
var
  SizeMetric: TPoint;
{$IFDEF SG_DELPHI_VCL}
  Buffer: Pointer;
  Length: UINT;

{$ENDIF}
  DC: HDC;
  vSrcBitmap: TBitmap;
  hMF: HMETAFILE;
  hMem: THandle;
  pMFP: PMetafilePict;
  vBitmap: TBitmap;
  vMemory: TMemoryStream;
{$IFDEF SG_FM_WINDOWS}
  vHBitmap, vStockBitmap: HBITMAP;
  vSrcDC: HDC;
{$ELSE}
  {$IFDEF MSWINDOWS}
  vCanvas: TCanvas;
  {$ENDIF}
{$ENDIF}
begin
  vSrcBitmap := nil;
  if FGraphic is TBitmap then
    vSrcBitmap := TBitmap(FGraphic);
  case FormatEtcIn.tymed of
    // Handle only Enhanced MetaFile
    TYMED_ENHMF:
      begin
{$IFDEF SG_DELPHI_VCL}
        if FGraphic is TMetafile then
        begin
          Medium.tymed := TYMED_ENHMF;
          Medium.hEnhMetaFile := CopyEnhMetaFile(TMetafile(FGraphic).Handle, nil);
          Medium.unkForRelease := nil;
          Result := S_OK;
        end
        else
{$ENDIF}
          Result := DV_E_FORMATETC; // TODO: for another graphics
      end;
    TYMED_MFPICT:
    begin
      // Handle only MetaFile
{$IFDEF SG_DELPHI_VCL}
      if FGraphic is TMetafile then
        with TMetafile(FGraphic) do
        begin
          SizeMetric.X := MMWidth;
          SizeMetric.Y := MMHeight;
          Buffer := nil;
          Length := 0;
          DC := GetDC(0);
          try
            Length := GetWinMetaFileBits(Handle, 0, nil, MM_ANISOTROPIC, DC);
            GetMem(Buffer, Length);
            if GetWinMetaFileBits(Handle, Length, Buffer,
                 MM_ANISOTROPIC, DC) = Length then
              hMF := SetMetaFileBitsEx(Length, Buffer)
            else
              hMF := 0;
          finally
            if Buffer <> nil then
              FreeMem(Buffer, Length);
            ReleaseDC(0, DC);
          end;
        end
      else
{$ENDIF}
      begin
        // convert pixels to mm
        SizeMetric := GetScreenResolution();

        if Assigned(vSrcBitmap) then
        begin
          SizeMetric.X := MulDiv(TBitmap(FGraphic).Width,
            sg0_001MMPerInch, SizeMetric.X);
          SizeMetric.Y := MulDiv(TBitmap(FGraphic).Height,
            sg0_001MMPerInch, SizeMetric.Y);
        end
        else
        begin
          SizeMetric.X := MulDiv(TGraphic(FGraphic).Width,
            sg0_001MMPerInch, SizeMetric.X);
          SizeMetric.Y := MulDiv(TGraphic(FGraphic).Height,
            sg0_001MMPerInch, SizeMetric.Y);
        end;
        // Create Metafile DC and set it up
        DC := CreateMetaFile(nil);
        SetWindowOrgEx(DC, 0, 0, nil);
        SetWindowExtEx(DC, SizeMetric.X, SizeMetric.Y, nil);
{$IFDEF SG_DELPHI_VCL}
        if FGraphic.ClassType = TIcon then
          DrawIconEx(DC, 0, 0, TIcon(FGraphic).Handle, SizeMetric.X, SizeMetric.Y,
            0, 0, DI_NORMAL)
        else
{$ENDIF}
          begin
{$IFDEF SG_FM_WINDOWS}
            if Assigned(vSrcBitmap) then
            begin
              vSrcDC := CreateCompatibleDC(0);
              try
                vHBitmap := BitmapToWinBitmap(vSrcBitmap, False);
                try
                  vStockBitmap := SelectObject(vSrcDC, vHBitmap);
                  StretchBlt(DC, 0, 0, SizeMetric.X, SizeMetric.Y, vSrcDC, 0, 0,
                    vSrcBitmap.Width, vSrcBitmap.Height, SRCCOPY);
                  SelectObject(vSrcDC, vStockBitmap);
                finally
                  DeleteObject(vHBitmap);
                end;
              finally
                DeleteDC(vSrcDC);
              end;
            end;
{$ELSE}
            vCanvas := TCanvas.Create;
            try
              vCanvas.Handle := DC;
              vCanvas.StretchDraw(Rect(0, 0, SizeMetric.X, SizeMetric.Y), FGraphic);
            finally
              vCanvas.Handle := 0;
              vCanvas.Free;
            end;
{$ENDIF}
          end;
        hMF := CloseMetaFile(DC);
      end;
      if hMF = 0 then
      begin
        Result := E_UNEXPECTED;
        Exit;
      end;

      // Get memory handle
      hMem := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE, SizeOf(METAFILEPICT));
      if hMem = 0 then
      begin
        DeleteMetaFile(hMF);
        Result := STG_E_MEDIUMFULL;
        Exit;
      end;
      pMFP := PMetafilePict(GlobalLock(hMem));
      pMFP^.hMF := hMF;
      pMFP^.mm := MM_ANISOTROPIC;
      pMFP^.xExt := SizeMetric.X;
      pMFP^.yExt := SizeMetric.Y;
      GlobalUnlock(hMem);

      Medium.tymed := TYMED_MFPICT;
      Medium.hGlobal := hMem;
      Medium.{$IFDEF SGFPC}PUnkForRelease{$ELSE}unkForRelease{$ENDIF} := nil;

      Result := S_OK;
    end;
    TYMED_GDI:
    begin
      Medium.tymed := TYMED_GDI;
      vBitmap := TBitmap.Create;
      try
        if (FGraphic.ClassType = TBitmap) or (FGraphic is TsgBitmap) then
        begin
          vMemory := TMemoryStream.Create;
          try
            if Assigned(vSrcBitmap) then
              vSrcBitmap.SaveToStream(vMemory)
            else
              TGraphic(FGraphic).SaveToStream(vMemory);
            vMemory.Position := 0;
            vBitmap.LoadFromStream(vMemory);
          finally
            vMemory.Free;
          end;
        end
        else
        begin
          vBitmap.PixelFormat := pf24bit;
          if Assigned(vSrcBitmap) then
          begin
            vBitmap.Width := vSrcBitmap.Width;
            vBitmap.Height := vSrcBitmap.Height;
          end
          else
          begin
            vBitmap.Width := TGraphic(FGraphic).Width;
            vBitmap.Height := TGraphic(FGraphic).Height;
          end;
          vBitmap.Canvas.StretchDraw(Rect(0, 0, vBitmap.Width, vBitmap.Height),
          {$IFNDEF SG_FIREMONKEY}TGraphic(FGraphic){$ELSE}FGraphic{$ENDIF});
        end;
{$IFDEF SG_FM_WINDOWS}
        Medium.hBitmap := BitmapToWinBitmap(vBitmap, False{?});
{$ELSE}
        Medium.hBitmap := vBitmap.ReleaseHandle;
{$ENDIF}
      finally
        vBitmap.Free;
      end;
      Medium.{$IFDEF SGFPC}PUnkForRelease{$ELSE}unkForRelease{$ENDIF} := nil;
      Result := S_OK;
    end;
    else
    begin
      Result := DV_E_FORMATETC;
      Exit;
    end;
  end;
end;

function TsgOleDataObject.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TsgOleDataObject.QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TsgOleDataObject.SetData(const FormatEtc: TFormatEtc;
  {$IFDEF MSWINDOWS}var{$ELSE}const{$ENDIF} Medium: TStgMedium; fRelease: BOOL): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function CreateSTGMFromImage(const AGraphic: TPersistent;
  const AFormat: TClipFormat = CF_METAFILEPICT): AnsiString;
var
  vData: IDataObject;
  vFormatEtc: TFormatEtc;
  vStorage: IStorage;
  vOleObject: IOleObject;
  vLockBytes: ILockBytes;
  vStatStg: TStatStg;
  vULOffs: {$IFDEF SGFPC}ULARGE_INTEGER{$ELSE}Int64{$ENDIF};
  vRead: {$IFDEF SGFPC}LongWord{$ELSE}Longint{$ENDIF};
begin
  vData := TsgOleDataObject.Create(AGraphic);
  OleCheck(CreateILockBytesOnHGlobal(0, True, vLockBytes));
  OleCheck(StgCreateDocfileOnILockBytes(vLockBytes,
    STGM_READWRITE or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, vStorage));
  vFormatEtc.cfFormat := AFormat;
  vFormatEtc.ptd := nil;
  vFormatEtc.dwAspect := DVASPECT_CONTENT;
  vFormatEtc.lindex := -1;
  case AFormat of
    CF_ENHMETAFILE: vFormatEtc.tymed := TYMED_ENHMF;
    CF_METAFILEPICT: vFormatEtc.tymed := TYMED_MFPICT;
    CF_BITMAP: vFormatEtc.tymed := TYMED_GDI;
  end;
  OleCheck(OleCreateStaticFromData(vData, IOleObject, OLERENDER_FORMAT,
    @vFormatEtc, nil, vStorage, vOleObject));
  OleCheck(OleSetContainedObject(vOleObject, True));
  OleCheck(vStorage.Commit(STGC_DEFAULT));
  OleCheck(vLockBytes.Stat(vStatStg, STATFLAG_DEFAULT));
  SetLength(Result, vStatStg.cbSize);
  FillChar(vULOffs, SizeOf(vULOffs), 0);
  OleCheck(vLockBytes.ReadAt(vULOffs, @Result[1], vStatStg.cbSize, {$IFNDEF SGFPC}@{$ENDIF}vRead));
end;

function StgToGraphic(const AStgMedium: TStgMedium; var AGraphic: TGraphic): Boolean;
var
  vMfPic: PMetafilePict;
  vBih: PBitmapInfo;
  vS: TMemoryStream;
  vMf: HMETAFILE;
  vDC: HDC;
//  vSrcBits, vDstBits: Pointer;
//  vInfoHeaderSize: Integer;
//  vPf: TPixelFormat;
{$IFDEF SG_FM_WINDOWS}
  Size: Integer;
  DS: TDIBSection;
  Map: TsgMap;
{$ENDIF}
begin
  Result := False;
  AGraphic := nil;
  try
    case AStgMedium.tymed of
      TYMED_GDI:
        begin
          AGraphic := TGraphic(TBitmap.Create);
{$IFDEF SG_FM_WINDOWS}
          Size := GetObject(AStgMedium.hBitmap, SizeOf(TDIBSection), @DS);
          if (Size >= (SizeOf(DS.dsbm) + SizeOf(DS.dsbmih))) and (DS.dsbmih.biSize >= DWORD(SizeOf(DS.dsbmih))) then
          begin
            Map := TsgMap.Wrap(DS.dsBm.bmWidth, DS.dsBm.bmHeight, GetPixelFormat(DS.dsBm.bmBitsPixel), DS.dsBm.bmBits, nil, 0);
            try
              AGraphic.Assign(Map);
              Result := True;
            finally
              TsgMap.Unwrap(Map);
            end;
          end;
{$ELSE}
          TBitmap(AGraphic).Handle := CopyImage(AStgMedium.hBitmap, IMAGE_BITMAP, 0, 0, 0);
          Result := True;
{$ENDIF}
        end;
      TYMED_MFPICT:
        begin
          vMfPic := GlobalLock(HGLOBAL(AStgMedium.hMetaFilePict));
          try
            vS := TMemoryStream.Create;
            try
              vS.Size := GetMetaFileBitsEx(vMfPic^.hMF, 0, nil);
              if GetMetaFileBitsEx(vMfPic^.hMF, vS.Size, vS.Memory) = vS.Size then
              begin
                vDC := GetDC(0);
                try
                  vMf := SetWinMetaFileBits(vS.Size, vS.Memory, vDC, vMfPic^);
                  try
{$IFDEF SG_DELPHI_VCL}
                    AGraphic := TMetafile.Create;
                    try
                      TMetafile(AGraphic).Handle := vMf;
                      Result := True;
                    except
                      FreeAndNil(AGraphic);
                      raise;
                    end;
{$ENDIF}
                  finally
                    if vMf <> 0 then
                      DeleteMetaFile(vMf);
                  end;
                finally
                  ReleaseDC(0, vDC);
                end;
              end;
            finally
              vS.Free;
            end;
          finally
            GlobalUnLock(HGLOBAL(AStgMedium.hMetaFilePict));
          end;
        end;
      TYMED_ENHMF:
        begin
{$IFDEF SG_DELPHI_VCL}
          AGraphic := TMetafile.Create;
          TMetafile(AGraphic).Handle := CopyEnhMetaFile(AStgMedium.hEnhMetaFile, nil);
          Result := True;
{$ENDIF}
        end;
      TYMED_HGLOBAL:
        begin
          AGraphic := TGraphic(TBitmap.Create);
          vBih := GlobalLock(AStgMedium.hGlobal);
          try
            vS := TMemoryStream.Create;
            try
              vS.Size := SizeOf(TBitmapFileHeader);
              PBitmapFileHeader(vS.Memory)^.bfType := $4D42;
              PBitmapFileHeader(vS.Memory)^.bfSize := SizeOf(TBitmapFileHeader) + vBih^.bmiHeader.biSizeImage;
              PBitmapFileHeader(vS.Memory)^.bfReserved1 := 0;
              PBitmapFileHeader(vS.Memory)^.bfReserved2 := 0;
              PBitmapFileHeader(vS.Memory)^.bfOffBits := SizeOf(TBitmapInfoHeader) + SizeOf(TBitmapFileHeader);
              vS.Position := vS.Size;
              vS.Write(vBih^, GlobalSize(AStgMedium.hGlobal));
              vS.Position := 0;
              AGraphic.LoadFromStream(vS);
              Result := True;
            finally
              vS.Free;
            end;
          finally
            GlobalUnlock(AStgMedium.hGlobal);
          end;
          {TBitmap(AGraphic).HandleType := bmDIB;
          vBih := GlobalLock(AStgMedium.hGlobal);
          vSrcBits := vBih;
          vPf := pfCustom;
          case vBih^.bmiHeader.biBitCount of
            1: vPf := pf1Bit;
            4: vPf := pf4Bit;
            8: vPf := pf8Bit;
            16:
              case vBih^.bmiHeader.biCompression of
                BI_RGB : vPf := pf15Bit;
                BI_BITFIELDS:
                  if PCardinal(TsgNativeUInt(@vBih^.bmiColors) + SizeOf(vBih^.bmiColors[0]))^ = $7E0 then // dsBitFields[1] = $7E0
                    vPf := pf16Bit;
               end;
            24: vPf := pf24Bit;
            32: if vBih^.bmiHeader.biCompression = BI_RGB then vPf := pf32Bit;
          end;
          TBitmap(AGraphic).PixelFormat := vPf;
          vInfoHeaderSize := SizeOf(TBitmapInfoHeader);
          if vBih^.bmiHeader.biBitCount > 8 then
          begin
            if (vBih^.bmiHeader.biCompression and BI_BITFIELDS) <> 0 then
              Inc(vInfoHeaderSize, 12);
          end
          else
            if vBih^.bmiHeader.biClrUsed = 0 then
              vInfoHeaderSize := SizeOf(TRGBQuad) * (1 shl vBih^.bmiHeader.biBitCount)
            else
              vInfoHeaderSize := SizeOf(TRGBQuad) * vBih^.bmiHeader.biClrUsed;
          Inc(PByte(vSrcBits), vInfoHeaderSize);
          SetSizeGraphic(AGraphic, vBih^.bmiHeader.biWidth, vBih^.bmiHeader.biHeight);
          vDstBits := TBitmap(AGraphic).ScanLine[TBitmap(AGraphic).Height - 1];
          Move(vSrcBits^, vDstBits^, vBih^.bmiHeader.biSizeImage);
          GlobalUnlock(AStgMedium.hGlobal);
          Result := True;}
        end;
    end;
  except
    FreeAndNil(AGraphic);
    raise;
  end;
end;

function OleToGraphic(AOle: IOleObject;
  var AGraphic: TGraphic): Boolean;
const
  cnstClipFormats: array[0 .. 3] of TClipFormat =
    (CF_BITMAP, CF_METAFILEPICT, CF_DIB, CF_ENHMETAFILE);
var
  I: Integer;
  vData: IDataObject;
  vEf: IEnumFORMATETC;
  vFormat: TFormatEtc;
  vStg: TStgMedium;
begin
  Result := False;
  AGraphic := nil;
  if Assigned(AOle) and (AOle.QueryInterface(IDataObject, vData) = S_OK) then
  begin
    if vData.EnumFormatEtc(DATADIR_GET, vEf) = S_OK then
      while not Result and (vEf.Next(1, vFormat, nil) = S_OK) do
        if vData.GetData(vFormat, vStg) = S_OK then
          Result := StgToGraphic(vStg, AGraphic);
    if not Result then
    begin
      vFormat.ptd := nil;
      vFormat.dwAspect := DVASPECT_CONTENT;
      vFormat.lindex := -1;
      vFormat.tymed := TYMED_NULL;
      I := Low(cnstClipFormats);
      repeat
        vFormat.cfFormat := cnstClipFormats[I];
        if vData.GetData(vFormat, vStg) = S_OK then
          Result := StgToGraphic(vStg, AGraphic);
        Inc(I);
      until (I > High(cnstClipFormats)) or Result;
    end;
  end;
end;

end.

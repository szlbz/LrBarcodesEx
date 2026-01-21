{
LazReport BarcodesEx

Copyright (C) 2026 LBZ and Ying
}

unit lr_barcodesExUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LR_Class,lr_const, LR_Utils, LR_DBRel, LResources, DelphiZXingQRCode;

type

  TBarcodeExType = (btQR_Auto, btQR_Numeric, btQR_Alphanumeric, btQR_ISO88591, btQR_UTF8NoBOM, btQR_UTF8BOM);

const
  qrQuietZone = 4;
  cbExefaultText = '123';
  BarcodeExName: array[btQR_Auto..btQR_UTF8BOM] of string =
    ('QR_Auto', 'QR_Numeric', 'QR_Alphanumeric', 'QR_ISO-8859-1', 'QR_UTF-8 without BOM', 'QR_UTF-8 with BOM');

type

  { TfrBarCodeEx }

  TfrBarCodeEx = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TfrCustomBarCodeExView }



  TfrCustomBarCodeExView = class(TfrView)
  private
    FBarType: TBarcodeExType;
    procedure SetBarType(AValue: TBarcodeExType);
  public
    constructor Create(AOwnerPage:TfrPage);override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    property BarType: TBarcodeExType read FBarType write SetBarType;
  end;

  TfrBarCodeExView = class(TfrCustomBarCodeExView)
  published
    property BarType;
    property Memo;
    property Script;
  end;

  { TBarCodeExForm }

  TBarCodeExForm = class(TfrObjEditorForm)
    Button_Ok: TButton;
    Button_Cancel: TButton;
    Button_DBData: TButton;
    ComboBox_BarType: TComboBox;
    Image_logo: TImage;
    Memo_BarCodeEx: TMemo;
    procedure Button_DBDataClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure ShowEditor(t: TfrView); override;
  end;

var
  BarCodeExForm: TBarCodeExForm;

implementation

uses LR_Flds;

{$R *.lfm}

procedure InitializeBarcExAddin;
begin
  if not assigned(BarCodeExForm) {and not (csDesigning in ComponentState)} then
  begin
    BarCodeExForm := TBarCodeExForm.Create(nil);
    frSetAddinEditor(TfrBarCodeExView, BarCodeExForm);
    frSetAddinIcon(TfrBarCodeExView, BarCodeExForm.Image_logo.Picture.Bitmap);
    frSetAddinHint(TfrBarCodeExView, 'Add BarCodeEx');
  end;
end;

{ TBarCodeExForm }

procedure TBarCodeExForm.FormCreate(Sender: TObject);
var
  I: TBarcodeExType;
begin
  for I := btQR_Auto to btQR_UTF8BOM do
    ComboBox_BarType.Items.Add(BarcodeExName[I]);
end;

procedure TBarCodeExForm.Button_DBDataClick(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    if DBField <> '' then
      Memo_BarCodeEx.Lines.Text := '[' + DBField + ']';
  frFieldsForm.Free;
end;

procedure TBarCodeExForm.ShowEditor(t: TfrView);
begin
  ComboBox_BarType.ItemIndex := ord(TfrCustomBarCodeExView(t).BarType);
  Memo_BarCodeEx.Lines.Text := TfrCustomBarCodeExView(t).Memo.Text;
  if ShowModal = mrOK then
  begin
    TfrCustomBarCodeExView(t).BarType := TBarcodeExType(ComboBox_BarType.ItemIndex);
    TfrCustomBarCodeExView(t).Memo.Text := Memo_BarCodeEx.Lines.Text;
  end;
end;

{ TfrBarCodeEx }

constructor TfrBarCodeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeBarcExAddin;
end;

{ TfrCustomBarCodeExView }

procedure TfrCustomBarCodeExView.SetBarType(AValue: TBarcodeExType);
begin
  if FBarType <> AValue then
  begin
    BeforeChange;
    FBarType := AValue;
    AfterChange;
  end;
end;

constructor TfrCustomBarCodeExView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  Typ := gtAddIn;
  BaseName := 'Bar';
  FBarType := btQR_Auto;
  Memo.Add(cbExefaultText);
end;

destructor TfrCustomBarCodeExView.Destroy;
begin
  inherited Destroy;
end;

procedure TfrCustomBarCodeExView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TfrCustomBarCodeExView then
  begin
    FBarType := TfrCustomBarCodeExView(Source).BarType;
  end;
end;

procedure TfrCustomBarCodeExView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FBarType, SizeOf(FBarType));
end;

procedure TfrCustomBarCodeExView.LoadFromStream(Stream: TStream);
var
  bt: TBarcodeExType;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(bt, SizeOf(bt));
  BarType := bt;
end;

procedure TfrCustomBarCodeExView.SaveToXML(XML: TLrXMLConfig; const Path: String
  );
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'BarCodeEx/BarType', GetSaveProperty('BarType'));
end;

procedure TfrCustomBarCodeExView.LoadFromXML(XML: TLrXMLConfig;
  const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('BarType',XML.GetValue(Path+'BarCodeEx/BarType',''));
end;

procedure TfrCustomBarCodeExView.Draw(aCanvas: TCanvas);
var
  tmpQRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
  tmpBitmap: TBitmap;
  tmpEncoding: Integer;
begin    
  BeginDraw(aCanvas);
  CalcGaps;
  tmpQRCode := TDelphiZXingQRCode.Create;
  tmpBitmap := TBitmap.Create;
  try
    Memo1.Assign(Memo);
    tmpQRCode.Data := Memo.Text; 
    tmpEncoding:=0;
    case FBarType of
      btQR_Auto: tmpEncoding:=0;
      btQR_Numeric: tmpEncoding:=1;
      btQR_Alphanumeric: tmpEncoding:=2;
      btQR_ISO88591: tmpEncoding:=3;
      btQR_UTF8NoBOM: tmpEncoding:=4;
      btQR_UTF8BOM: tmpEncoding:=5;
    end;
    tmpQRCode.Encoding := TQRCodeEncoding(tmpEncoding);
    tmpQRCode.QuietZone := qrQuietZone;
    tmpBitmap.Width := tmpQRCode.Rows;
    tmpBitmap.Height := tmpQRCode.Columns;
    tmpBitmap.Canvas.Brush.Color := clWhite;
    tmpBitmap.Canvas.FillRect(Rect(0, 0, tmpBitmap.Width, tmpBitmap.Height));
    for Row := 0 to tmpQRCode.Rows - 1 do
    begin
      for Column := 0 to tmpQRCode.Columns - 1 do
      begin
        if (tmpQRCode.IsBlack[Row, Column]) then
        begin
          tmpBitmap.Canvas.DrawPixel(Column, Row,TColorToFPColor(clBlack));
        end;
      end;
    end;
    aCanvas.StretchDraw(DRect, tmpBitmap);
    ShowFrame;
  finally
    tmpQRCode.Free;
    tmpBitmap.Free;
    RestoreCoord;
  end;
end;

procedure TfrCustomBarCodeExView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1,Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    if (Length(Memo1[0]) > 0) and (Pos('[',Memo1[0])<>0) then
      Memo1[0] := frParser.Calc(Memo1[0]);
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

initialization
  {$I lr_barcodesexuniticon.lrs}
  BarCodeExForm := nil;
  frRegisterObject(TfrBarCodeExView, nil, '', nil, @InitializeBarcExAddin);

end.


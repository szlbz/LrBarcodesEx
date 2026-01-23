unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, ExtCtrls, LR_Class, LR_DBSet, LR_Desgn, lr_barcodesExUnit, LR_DSet;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    frBarCodeEx1: TfrBarCodeEx;
    frDBDataSet1: TfrDBDataSet;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    MemDataset1: TMemDataset;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  I:integer;
begin
  MemDataset1.Open;
  For I := 1 To 5 Do
  Begin
       MemDataset1.Append;
       MemDataset1.Fields[0].Value := I;
       MemDataset1.Fields[1].Value := 100000+I;
  End;
  MemDataset1.Post;
  MemDataset1.First;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin  
  frReport1.LoadFromFile('ok.lrf');
  frReport1.ShowReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin   
    frReport1.LoadFromFile('ok.lrf');
    frReport1.DesignReport;
end;

end.


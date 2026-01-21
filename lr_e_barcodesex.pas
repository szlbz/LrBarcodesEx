unit lr_e_BarcodesEx;

{$mode objfpc}{$H+}

interface

uses
  Classes,lr_barcodesExUnit,SysUtils,  LResources;

procedure Register;

implementation   

procedure Register;
begin
  RegisterComponents('LazReport', [TfrBarCodeEx]);
end;

end.


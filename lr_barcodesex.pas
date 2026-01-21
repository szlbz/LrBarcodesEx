{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_BarcodesEx;

{$warn 5023 off : no warning about unused units}
interface

uses
  lr_e_BarcodesEx, lr_barcodesExUnit, DelphiZXingQRCode, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lr_e_BarcodesEx', @lr_e_BarcodesEx.Register);
end;

initialization
  RegisterPackage('lr_BarcodesEx', @Register);
end.

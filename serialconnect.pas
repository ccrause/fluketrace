unit serialconnect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    ConnectButton: TButton;
    CancelButton: TButton;
    ComboBox1: TComboBox;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

uses
  serialutils;

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin
  ComboBox1.Items.Delimiter := ',';
  ComboBox1.Items.DelimitedText := GetSerialPortNames;
  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := 0;
end;

end.


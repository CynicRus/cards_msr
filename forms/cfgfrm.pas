unit cfgfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, uconfig;

type

  { TfrmCfg }

  TfrmCfg = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  frmCfg: TfrmCfg;

implementation

{$R *.lfm}

{ TfrmCfg }

procedure TfrmCfg.FormActivate(Sender: TObject);
begin
  SpinEdit1.Value:=config.CurrentCardValue;
end;

procedure TfrmCfg.Button1Click(Sender: TObject);
begin
  config.CurrentCardValue:=spinedit1.Value;
  config.Save;
  close;
end;

end.


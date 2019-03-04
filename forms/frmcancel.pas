unit frmcancel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TcnclFrm }

  TcnclFrm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  cnclFrm: TcnclFrm;

implementation
uses Main;

{$R *.lfm}

{ TcnclFrm }

procedure TcnclFrm.Button1Click(Sender: TObject);
begin
  MainForm.Cancel();
  self.Close;
end;

end.


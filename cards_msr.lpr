program cards_msr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, uconfig, ucardworker, cfgfrm,frmcancel, ucardthread;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmCfg, frmCfg);
  Application.CreateForm(TcnclFrm, cnclFrm);
  Application.Run;
end.


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus,Windows, LCLIntf,Lmessages, ExtCtrls, ucardworker, uconfig, uother,cfgfrm, ucardthread, frmcancel;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnImgList: TImageList;
    StopBtn: TButton;
    infoGB: TGroupBox;
    MainMenu1: TMainMenu;
    infoMemo: TMemo;
    StatusBar1: TStatusBar;
    btnBar: TToolBar;
    btnExit: TToolButton;
    ToolButton2: TToolButton;
    btnReadCard: TToolButton;
    btnWriteCard: TToolButton;
    btnMultipleCard: TToolButton;
    btnCleanCard: TToolButton;
    ToolButton7: TToolButton;
    btnSettings: TToolButton;
    procedure btnCleanCardClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnMultipleCardClick(Sender: TObject);
    procedure btnReadCardClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnWriteCardClick(Sender: TObject);
    procedure clcTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure infoMemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure StopBtnClick(Sender: TObject);
  private

  public
    procedure Output(const S: string);
    procedure Status(const S: string);
    procedure Cancel();
    procedure OnThreadLog(var aMessage: TLMessage);
    message WM_THREAD_LOG;
    procedure OnThreadState(var aMessage: TLMessage);
    message WM_THREAD_STATE;

  end;

var
  MainForm: TMainForm;
  //CardWorker: TCardWorker;
  //RunFlag: boolean = False;
  //curValue: int64;
  WorkerThread: TCardThread;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.btnMultipleCardClick(Sender: TObject);
begin
  StopBtn.Visible := true;
  PostThreadMessage(WorkerThread.ThreadID,WM_TASK_WRITE_BATCH_CARD,0,0);

  //cnclFrm.ShowModal;
end;

procedure TMainForm.btnCleanCardClick(Sender: TObject);
begin
  StopBtn.Visible := true;
  PostThreadMessage(WorkerThread.ThreadID,WM_TASK_CLEAN_CARD,0,0);
  //cnclFrm.Show;
end;

//читаем вторую дорожку с карты
procedure TMainForm.btnReadCardClick(Sender: TObject);
begin
  PostThreadMessage(WorkerThread.ThreadID,WM_TASK_READ_CARD,0,0);
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  frmcfg.Show;
end;

procedure TMainForm.btnWriteCardClick(Sender: TObject);
var
  Data: string;
begin
  Data := config.CardPrefix;
  if InputQuery( 'ввод карты:', 'введите номер:', Data) then
    begin
      PostThreadMessage(WorkerThread.ThreadID,WM_TASK_WRITE_SINGLE_CARD,0,Integer(PChar(Data)));
    end;
end;

procedure TMainForm.clcTimerTimer(Sender: TObject);
begin
 // Output(Format('### GetKeyState(VK_ESCAPE)=%d', [LCLIntf.GetKeyState(VK_ESCAPE)]));
  //if (LCLIntf.GetKeyState(VK_ESCAPE) and $8000) <> 0 then begin
   // Cancel();
 // end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WorkerThread := TCardThread.Create(false,MainForm.Handle);
  StopBtn.Visible:=false;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WorkerThread.Terminate;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if (Key = VK_RETURN) then
  //RunFlag := false;

end;

procedure TMainForm.infoMemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    Cancel();
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  self.Cancel();
  StopBtn.Visible:=false;
end;

procedure TMainForm.Output(const S: string);
begin
  infoMemo.Lines.Add(S);
end;

procedure TMainForm.Status(const S: string);
begin
  StatusBar1.Panels[0].Text:=S;
end;

procedure TMainForm.Cancel();
begin
  PostThreadMessage(WorkerThread.ThreadID,WM_TASK_FINISH,0,0);
end;

procedure TMainForm.OnThreadLog(var aMessage: TLMessage);
begin
  Output(pChar(aMessage.LParam));
end;

procedure TMainForm.OnThreadState(var aMessage: TLMessage);
var
  State: TCurrentWorkingState;
begin
  State := TCurrentWorkingState(Integer(aMessage.LParam));
  case State of
   cwsMultipleCard: Status('Запись карт');
   cwsCleanCard: Status('Очистка карт');
   cwsNoSpecialState: Status('Готов к работе');
   cwsErrorConnection: Status('Ошибка соединения');
   cwsFinished: Status('Остановлено')
  end;

end;

end.

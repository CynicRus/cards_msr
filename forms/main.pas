unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, LCLtype, LCLIntf, ExtCtrls, ucardworker, uconfig, uother;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnImgList: TImageList;
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
    procedure btnWriteCardClick(Sender: TObject);
    procedure clcTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure infoMemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private

  public
    procedure Output(const S: string);
    procedure Cancel();

  end;

var
  MainForm: TMainForm;
  CardWorker: TCardWorker;
  RunFlag: boolean = False;
  //curValue: int64;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.btnMultipleCardClick(Sender: TObject);
var
  Response: string;
begin
  if CardWorker.State = csNoConn then
    exit;
  RunFlag := True;
  try
    while RunFlag do
    begin
      Output('Пишу карту:');
      Output('Записано: '+ CardWorker.WriteCard());
      Output('Операция завершена.');
      config.CurrentCardValue := Config.CurrentCardValue + 1;
      //config.Save;
      Application.ProcessMessages;
    end;

    CardWorker.Reset();
  except
    on E: Exception do
      infoMemo.Lines.Add(E.Message);
  end;

end;

procedure TMainForm.btnCleanCardClick(Sender: TObject);
var
  Response: string;
begin
  if CardWorker.State = csNoConn then
    exit;
  RunFlag := True;
  try
    while RunFlag do
    begin
      Output('Очищаю карту:');
      CardWorker.EraseCard();
      Output('Операция завершена.');
      Application.ProcessMessages;
    end;

    CardWorker.Reset();
  except
    on E: Exception do
      infoMemo.Lines.Add(E.Message);
  end;

end;

//читаем вторую дорожку с карты
procedure TMainForm.btnReadCardClick(Sender: TObject);
var
  Response: string;
begin
  if CardWorker.State = csNoConn then
    exit;
  try
    Output('Читаю карту:');

    Response := CardWorker.ReadCard();

    Response := ExtractBetween(Response, ';', '?');
    if Length(Response) > 0 then
      Output(Response)
    else
      Output('Карта пуста.');
    Output('Операция завершена.');
  except
    on E: Exception do
      infoMemo.Lines.Add(E.Message);
  end;

end;

procedure TMainForm.btnWriteCardClick(Sender: TObject);
var
  Data: string;
begin
  Data := config.CardPrefix;
  if InputQuery( 'ввод карты:', 'введите номер:', Data) then
    begin
      Output('Запись карты:');
      Output('Записано: '+ CardWorker.WriteCard(Data));
      Output('Операция завершена.');
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
  CardWorker := TCardWorker.Create;
  CardWorker.Connect;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CardWorker.Free;
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

procedure TMainForm.Output(const S: string);
begin
  infoMemo.Lines.Add(S);
end;

procedure TMainForm.Cancel();
begin
  RunFlag := False;
  CardWorker.Reset();
  //config.CurrentCardValue := curValue;
  config.Save;
  Output('Отмена операции.');
end;

end.

unit ucardthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Messages, ucardworker, uconfig, uother;

const
  WM_TASK_READ_CARD = WM_USER + 1000;
  WM_TASK_WRITE_SINGLE_CARD = WM_USER + 1001;
  WM_TASK_WRITE_BATCH_CARD = WM_USER + 1002;
  WM_TASK_CLEAN_CARD = WM_USER + 1003;
  WM_TASK_FINISH = WM_USER + 1004;
  WM_TASK_RELOAD_CONFIG = WM_USER + 1005;
  WM_THREAD_LOG = WM_USER + 1006;
  WM_THREAD_STATE = WM_USER + 1007;

type
  TCurrentWorkingState = (cwsMultipleCard = 0, cwsCleanCard = 1,
    cwsFinished = 2, cwsNoSpecialState = 3, cwsErrorConnection = 4);

  { TCardThread }

  TCardThread = class(TThread)
  private
    CardWorker: TCardWorker;
    wnd: HWND;
    State: TCurrentWorkingState;
    procedure ReadCard();
    procedure WriteSingleCard(aMsg: TMsg);
    procedure WriteMultipleCard();
    procedure CleanCard();
    procedure LogData(const str: string);
    procedure StopTasks();
    procedure ReloadConfig();
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; FormHandle: HWND);
    destructor Destroy; override;
  end;

implementation

{ TCardThread }

procedure TCardThread.ReadCard();
var
  Response: string;
begin
  try
    LogData('Читаю карту:');
    Response := CardWorker.ReadCard();
    Response := ExtractBetween(Response, ';', '?');
    if Length(Response) > 0 then
      LogData(Response)
    else
      LogData('Карта пуста.');
    LogData('Операция завершена.');
    state := cwsFinished;
  except
    on E: Exception do
      LogData(E.Message);
  end;
end;

procedure TCardThread.WriteSingleCard(aMsg: TMsg);
var
  Data: string;
  CardData: string;
begin
  LogData('Запись карты:');
  try
    Data := PChar(aMsg.lParam);
    CardData := CardWorker.WriteCard(Data);
    LogData('Записано: ' + CardData);
    case CardWorker.StatusByte of
      $30: LogData('Операция завершена.');
      $31: LogData('Выполнено с ошибкой чтения/записи.');
      $32: LogData('Неправильный формат команды!');
      $34: LogData('Неправильная команда!');
      $39: LogData('Неправильно проведена карта!');
    end;



    state := cwsFinished;
  except
    on E: Exception do
      LogData(E.Message);
  end;
end;

procedure TCardThread.WriteMultipleCard();
var
  CardData: string;
begin
  try
    LogData('Запись карты:');
    CardData := CardWorker.WriteCard();
    LogData('Записано: ' + CardData);
    case CardWorker.StatusByte of
      $30: LogData('Операция завершена.');
      $31: LogData('Выполнено с ошибкой чтения/записи.');
      $32: LogData('Неправильный формат команды!');
      $34: LogData('Неправильная команда!');
      $39: LogData('Неправильно проведена карта!');
    end;
    config.CurrentCardValue := Config.CurrentCardValue + 1;
    config.Save;
  except
    on E: Exception do
      LogData(E.Message);
  end;
end;

procedure TCardThread.CleanCard();
begin
  try
    LogData('Очищаю карту:');
    CardWorker.EraseCard();
    LogData('Операция завершена.');
  except
    on E: Exception do
      LogData(E.Message);
  end;
end;

procedure TCardThread.LogData(const str: string);
begin
  PostMessage(wnd, WM_THREAD_LOG, 0, UIntPtr(PChar(str)));
end;

procedure TCardThread.StopTasks();
begin
  State := cwsNoSpecialState;
  CardWorker.Reset();
end;

procedure TCardThread.ReloadConfig();
begin
  if (State <> cwsErrorConnection) then
    CardWorker.Disconnect;

  CardWorker.Connect;
  if CardWorker.State = csErrorConn then
    State := cwsErrorConnection
  else
    State := cwsNoSpecialState;
end;

procedure TCardThread.Execute;
var
  aMsg: TMsg;
begin
  while not terminated do
  begin

    if PeekMessage(amsg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(aMsg);
      DispatchMessage(aMsg);
    end else
    Sleep(100);


    if aMsg.message > 0 then
    begin
      if ((state = cwsErrorConnection) and (aMsg.message <> WM_TASK_RELOAD_CONFIG)) then
      begin
        sleep(100);
        continue;
      end;

      case aMsg.message of
        WM_TASK_READ_CARD: ReadCard();
        WM_TASK_WRITE_SINGLE_CARD: WriteSingleCard(aMsg);
        WM_TASK_WRITE_BATCH_CARD: state := cwsMultipleCard;
        WM_TASK_CLEAN_CARD: state := cwsCleanCard;
        WM_TASK_FINISH: state := cwsFinished;
        WM_TASK_RELOAD_CONFIG: ;
      end;
    end;
    aMsg.message:=0;

    case state of
        cwsMultipleCard: WriteMultipleCard();
        cwsCleanCard: CleanCard();
        cwsNoSpecialState: sleep(100);
        cwsFinished: StopTasks();
    end;

    //Sleep(10);
    PostMessage(wnd, WM_THREAD_STATE, 0, integer(state));
  end;

end;



constructor TCardThread.Create(CreateSuspended: boolean; FormHandle: HWND);
begin
  CardWorker := TCardWorker.Create;
  CardWorker.Connect;
  wnd := FormHandle;
  State := cwsNoSpecialState;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TCardThread.Destroy;
begin
  CardWorker.Free;
  inherited Destroy;
end;


end.

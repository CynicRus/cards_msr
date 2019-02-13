unit ucardworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uconfig, umsrworker;

type

  { TCardWorker }
  TConnectState = (csNoConn = 0,csConnected = 1);

  TCardWorker = class
    private
      FMsrWorker: TMsrWorker;
      FState: TConnectState;
    public
      procedure Connect;
      constructor Create;
      destructor Destroy;override;
      function GetFirmware(): string;
      function ReadCard(): string;
      function WriteCard(): string;overload;
      function WriteCard(Data: string): string;overload;
      procedure EraseCard();
      procedure Reset();

      property State: TConnectState read FState write FState;

  end;

implementation

{ TCardWorker }

procedure TCardWorker.Connect;
begin
  FMsrWorker.ComName := Config.ComPortName;
  FMsrWorker.Connect;
  State := csConnected;
end;

constructor TCardWorker.Create;
begin
  FMsrWorker := TMsrWorker.Create();
end;

destructor TCardWorker.Destroy;
begin
  if FmsrWorker <> nil then
   FMsrWorker.Free;
  inherited Destroy;
end;

function TCardWorker.GetFirmware(): string;
var
  Str: string;
begin
  str := '';
  str := FMsrWorker.GetFirmware();

  result := str;

end;

function TCardWorker.ReadCard(): string;
begin
  result := FMsrWorker.ReadCard();
end;

function TCardWorker.WriteCard(): string;
var
  Data: string;
begin
  Data := '';
  Data := config.CardPrefix + IntToStr(config.CurrentCardValue);
  result := WriteCard(Data);
end;

function TCardWorker.WriteCard(Data: string): string;
begin
 FMsrWorker.WriteCard('',Data,'');
 result := Data;
end;

procedure TCardWorker.EraseCard();
begin
  FMsrWorker.EraseCard();
end;

procedure TCardWorker.Reset();
begin
  FMsrWorker.Reset();
end;

end.


unit utransport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,{Forms,} Synaser, uother;
type

  { TMSRTransport }

  TMSRTransport = class
  private
    FAnswerTimeout: integer;
    FCancel: boolean;
    FConfigured: boolean;
    FConnected: boolean;
    FConnector: TBlockSerial;
    FLastError: integer;
    FLastErrorDesc: string;
    FPortNo: string;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Connect;
    procedure Disconnect;
    procedure DoCancel;
    function Configure(const ConnectionString: string): boolean;
    procedure Send(const Data: Pointer; DataLength: integer);
    function SendData(const Data: Pointer; DataLength: integer): string;
    property Connector: TBlockSerial read FConnector;
    property Connected: boolean read FConnected write FConnected;
    property LastError: integer read FLastError write FLastError;
    property Configured: boolean read FConfigured write FConfigured;
    property LastErrorDesc: string read FLastErrorDesc write FLastErrorDesc;
    property AnswerTimeout: integer read FAnswerTimeout write FAnswerTimeout;
  end;

implementation

{ TRS232Transport }

constructor TMSRTransport.Create;
begin
  FConnector := TBlockSerial.Create;
  FConnected := False;
  FConfigured := False;
  FLastError := 0;
  LastErrorDesc := '';
  FAnswerTimeout := 1000;
  inherited;
end;

destructor TMSRTransport.Destroy;
begin

  FConnector.Free;
  inherited Destroy;
end;

procedure TMSRTransport.Connect;
begin
  if Configured then
  begin
    with FConnector do
    begin
      Connect(FPortNo);
      LinuxLock := False;
      sleep(500);
      Config(9600, 8, 'N', SB1, True, False);
      Sleep(300);
      self.LastError := LastError;
      Self.LastErrorDesc := LastErrorDesc;
      if self.LastError = 0 then
        Connected := True;
    end;
  end;

end;

procedure TMSRTransport.Disconnect;
begin
  if Connected then
  begin
    FConnector.Flush;
    FConnector.CloseSocket;
    Sleep(500);
  end;
end;

procedure TMSRTransport.DoCancel;
begin
  if Connected then
   Disconnect;
  Connect;
end;

// формат типа PortNo = COM1
function TMSRTransport.Configure(const ConnectionString: string): boolean;
var
  St: TStringList;
begin
  Result := False;
  St := TStringList.Create;
  try
    Split('=', ConnectionString, St);
    if St.Count > 0 then
    begin
      FPortNo := UpperCase(St[1]);
      Configured := True;
      Result := True;
    end;
  finally
    St.Free;
  end;
end;

procedure TMSRTransport.Send(const Data: Pointer; DataLength: integer);
var
  Answer: string;
  Str: string;
  WrittenBytes: integer;
begin
  Answer := '';
  if Connected then
  begin
    WrittenBytes := 0;
    WrittenBytes := FConnector.SendBuffer(Data,DataLength);
    //FConnector.SendString(StrData);
    if FConnector.LastError <> 0 then
    begin
      Self.LastError := FConnector.LastError;
      Self.LastErrorDesc := FConnector.LastErrorDesc;
    end;
    Sleep(50);
  end;
end;

function TMSRTransport.SendData(const Data: Pointer; DataLength: integer): string;
var
  Answer: string;
  Str: string;
  WrittenBytes: integer;
begin
  Result := '';
  Answer := '';
  if Connected then
  begin
    WrittenBytes := 0;
    WrittenBytes := FConnector.SendBuffer(Data,DataLength);
    //FConnector.SendString(StrData);
    if FConnector.LastError <> 0 then
    begin
      Self.LastError := FConnector.LastError;
      Self.LastErrorDesc := FConnector.LastErrorDesc;
      exit;
    end;

    str := '';
    str := FConnector.RecvPacket(100);
    if str = '' then
      repeat
       // application.ProcessMessages;
       // Sleep(50);
        str := FConnector.RecvPacket(100);
      until (str <> '') {or (Cancel)};
    answer := str;
    while (str <> '') {or (not Cancel)} do
    begin
      //application.ProcessMessages;
     // Sleep(50);
      str := FConnector.RecvPacket(100);
      answer := answer + str;
    end;
    Result := Answer;
    if FConnector.LastError <> 0 then
    begin
      Self.LastError := FConnector.LastError;
      Self.LastErrorDesc := FConnector.LastErrorDesc;
    end;
  end;
end;


end.


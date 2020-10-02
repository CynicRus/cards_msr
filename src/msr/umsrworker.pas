unit umsrworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, udatabuilder, uother, utransport, umsr_defines;

type

  { TMSRWorker }

  TMSRWorker = class
  private
    FComName: string;
    FEncoderMode: integer;
    FTransport: TMSRTransport;
    FBuilder: TMSRDataBuilder;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function GetConnectionStatus(): boolean;
    //function WriteCard(Data: Integer):string;
    function WriteCard(Track1, Track2, Track3: string): string;
    function ReadCard(): string;
    function EraseCard(): string;
    function GetFirmware(): string;
    procedure Reset();
    property ComName: string read FComName write FComName;
    property EncoderMode: integer read FEncoderMode write FEncoderMode;

  end;

implementation

{ TMSRWorker }

constructor TMSRWorker.Create();
begin
  FTransport := TMSRTransport.Create;
  FBuilder := TMSRDataBuilder.Create;
end;

destructor TMSRWorker.Destroy;
begin
  {if FTransport.Connected then
   FTransport.;}
  FTransport.Free;
  FBuilder.Free;
  inherited Destroy;
end;

procedure TMSRWorker.Connect;
begin
  FTransport.Configure(Format('PortNo=%s', [ComName]));
  FTransport.Connect;
end;

procedure TMSRWorker.Disconnect;
begin
  FTransport.Disconnect;
end;

function TMSRWorker.GetConnectionStatus(): boolean;
begin
  Result := FTransport.Connected;
end;

function TMSRWorker.WriteCard(Track1, Track2, Track3: string): string;
var
  Tr1, Tr2, Tr3, EncData: TByteArray;
  Cmd, Buff: array of byte;
begin
  //SetLength(CMD, 2);
 // SetLength(Buff, 2);
  Tr1 := StrToByte(Track1);
  Tr2 := StrToByte(Track2);
  Tr3 := StrToByte(Track3);
  FBuilder.MakeReset(CMD);
  FTransport.Send(Pointer(@CMD[0]), Length(CMD));
  //CMD := FBuilder.MakeSetHiCo;
  case EncoderMode of
    0:  FBuilder.MakeSetHiCo(CMD);
    1:  FBuilder.MakeSetLowCo(CMD);
  end;
  FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
  FBuilder.MakeSetBPC(CMD);
  DynArrayAppend(Cmd, $07);
  DynArrayAppend(Cmd, $05);
  DynArrayAppend(Cmd, $05);
  FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
  FBuilder.MakeMSRWriteISO(CMD);
  FBuilder.MakeRWStart(Buff);
  DynArrayAppendByteArray(CMD, Buff);
  DynArrayAppend(CMD, $1B);
  DynArrayAppend(CMD, $01);
  SetLength(EncData, Length(Tr1));
  //EncodeTrack(7,@Tr1[0],Length(tr1),@EncData[0]);
  //DynArrayAppend(CMD,Tr1);
  DynArrayAppendByteArray(CMD, tr1);
  DynArrayAppend(CMD, $1B);
  DynArrayAppend(CMD, $02);
  SetLength(EncData, Length(Tr2));
  // EncodeTrack(5,@Tr1[0],Length(tr2),@EncData[0]);
  DynArrayAppendByteArray(CMD, tr2);
  DynArrayAppend(CMD, $1B);
  DynArrayAppend(CMD, $03);
  SetLength(EncData, Length(Tr3));
  // EncodeTrack(5,@Tr3[0],Length(tr3),@EncData[0]);
  DynArrayAppendByteArray(CMD, tr3);
  DynArrayAppend(CMD, $3F);
  DynArrayAppend(CMD, $1C);
  Result :={ByteToString(CMD) }FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
end;

function TMSRWorker.ReadCard(): string;
var
  CMD: array of byte;

begin
  //SetLength(CMD, 2);
  FBuilder.MakeReset(CMD);
  FTransport.Send(Pointer(@CMD[0]), Length(CMD));
  FBuilder.MakeMSRReadIso(CMD);
  Result := FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
end;

function TMSRWorker.EraseCard(): string;
var
  CMD: array of byte;

begin
  //SetLength(CMD, 2);
  FBuilder.MakeErase(CMD);
  FTransport.Send(Pointer(@CMD[0]), Length(CMD));
  FBuilder.MakeEraseAll(CMD);
  Result := FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
end;

function TMSRWorker.GetFirmware(): string;
var
  CMD: array of byte;

begin
  //SetLength(CMD, 2);
  FBuilder.MakeReset(CMD);
  FTransport.Send(Pointer(@CMD[0]), Length(CMD));
  FBuilder.MakeCheckFirmware(CMD);
  Result := FTransport.SendData(Pointer(@CMD[0]), Length(CMD));
end;

procedure TMSRWorker.Reset();
var
  CMD: array of byte;
begin
 // SetLength(CMD, 2);
  FBuilder.MakeReset(CMD);
  FTransport.Send(Pointer(@CMD[0]), Length(CMD));
  FTransport.DoCancel();
end;

end.

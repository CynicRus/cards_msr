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
      FTransport: TMSRTransport;
      FBuilder: TMSRDataBuilder;
    public
      constructor Create();
      destructor Destroy;override;
      procedure Connect;
      //function WriteCard(Data: Integer):string;
      function WriteCard(Track1,Track2,Track3: string): string;
      function ReadCard():string;
      function EraseCard(): string;
      function GetFirmware(): string;
      procedure Reset();
      property ComName: string read FComName write FComName;

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
  FTransport.Configure(Format('PortNo=%s',[ComName]));
  FTransport.Connect;
end;

function TMSRWorker.WriteCard(Track1, Track2, Track3: string): string;
var
  Tr1,Tr2,Tr3,EncData: TByteArray;
  Cmd: array of byte;
begin
  Tr1 := StrToByte(Track1);
  Tr2 := StrToByte(Track2);
  Tr3 := StrToByte(Track3);
  CMD := FBuilder.MakeReset;
  FTransport.Send(Pointer(@CMD[0]),Length(CMD));
  CMD := FBuilder.MakeSetHiCo;
  FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
  Cmd := FBuilder.MakeSetBPC;
  DynArrayAppend(Cmd,$07);
  DynArrayAppend(Cmd,$05);
  DynArrayAppend(Cmd,$05);
  FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
  CMD := FBuilder.MakeMSRWriteISO;
  DynArrayAppendByteArray(CMD,FBuilder.MakeRWStart);
  DynArrayAppend(CMD,$1B);
  DynArrayAppend(CMD,$01);
  SetLength(EncData,Length(Tr1));
  //EncodeTrack(7,@Tr1[0],Length(tr1),@EncData[0]);
  //DynArrayAppend(CMD,Tr1);
  DynArrayAppendByteArray(CMD,tr1);
  DynArrayAppend(CMD,$1B);
  DynArrayAppend(CMD,$02);
  SetLength(EncData,Length(Tr2));
 // EncodeTrack(5,@Tr1[0],Length(tr2),@EncData[0]);
  DynArrayAppendByteArray(CMD,tr2);
  DynArrayAppend(CMD,$1B);
  DynArrayAppend(CMD,$03);
  SetLength(EncData,Length(Tr3));
 // EncodeTrack(5,@Tr3[0],Length(tr3),@EncData[0]);
  DynArrayAppendByteArray(CMD,tr3);
  DynArrayAppend(CMD,$3F);
  DynArrayAppend(CMD,$1C);
  result :={ByteToString(CMD) }FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
end;

function TMSRWorker.ReadCard(): string;
var
  CMD: array of byte;

begin
  CMD := FBuilder.MakeReset;
  FTransport.Send(Pointer(@CMD[0]),Length(CMD));
  CMD := FBuilder.MakeMSRReadIso;
  result := FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
end;

function TMSRWorker.EraseCard(): string;
var
  CMD: array of byte;

begin
  CMD := FBuilder.MakeErase;
  FTransport.Send(Pointer(@CMD[0]),Length(CMD));
  CMD := FBuilder.MakeEraseAll;
  result := FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
end;

function TMSRWorker.GetFirmware(): string;
var
  CMD: array of byte;

begin
  CMD := FBuilder.MakeReset;
  FTransport.Send(Pointer(@CMD[0]),Length(CMD));
  CMD := FBuilder.MakeCheckFirmware;
  result := FTransport.SendData(Pointer(@CMD[0]),Length(CMD));
end;

procedure TMSRWorker.Reset();
var
  CMD: array of byte;
begin
   CMD := FBuilder.MakeReset;
   FTransport.Send(Pointer(@CMD[0]),Length(CMD));
   FTransport.Cancel();
end;

end.

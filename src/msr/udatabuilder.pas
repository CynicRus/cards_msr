unit udatabuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umsr_defines,UOther;
type
  { TMSRDataBuilder }

  TMSRDataBuilder = class
    private
      function MakeCommand(Command: byte):TMSRCMD;
    public
      Constructor Create;
      Destructor Destroy;override;
      procedure MakeCommTest(out Arr: TByteArray);
      procedure MakeCheckFirmware(out Arr: TByteArray);
      procedure MakeCheckDeviceModel(out Arr: TByteArray);
      procedure MakeSensorTest(out Arr: TByteArray);
      procedure MakeRAMTest(out Arr: TByteArray);
      procedure MakeSetHiCo(out Arr: TByteArray);
      procedure MakeSetLowCo(out Arr: TByteArray);
      procedure MakeReset(out Arr: TByteArray);
      procedure MakeMSRWriteISO(out Arr: TByteArray);
      procedure MakeMSRWriteRaw(out Arr: TByteArray);
      procedure MakeRWStart(out Arr: TByteArray);
      procedure MakeMSRReadISO(out Arr: TByteArray);
      procedure MakeMSRReadRaw(out Arr: TByteArray);
      procedure MakeErase(out Arr: TByteArray);
      procedure MakeEraseAll(out Arr: TByteArray);
      procedure MakeSetBPI(out Arr: TByteArray);
      procedure MakeSetBPC(out Arr: TByteArray);
      procedure MakeLeadingZeroes(out Arr: TByteArray);
  end;

implementation

{ TMSRDataBuilder }

function TMSRDataBuilder.MakeCommand(Command: byte): TMSRCMD;
begin
  Result.ESC := MSR_ESC;
  Result.CMD:=Command;
end;

constructor TMSRDataBuilder.Create;
begin
  inherited;
end;

destructor TMSRDataBuilder.Destroy;
begin
  inherited Destroy;
end;

procedure TMSRDataBuilder.MakeCommTest(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_DIAG_COMM);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
  //Result := MyArr;
end;

procedure TMSRDataBuilder.MakeCheckFirmware(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_FWREV);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeCheckDeviceModel(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_MODEL);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeSensorTest(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_DIAG_SENSOR);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeRAMTest(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_DIAG_RAM);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeSetHiCo(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_SETCO_HI);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeSetLowCo(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_SETCO_LO);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeReset(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_RESET);
  Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeMSRWriteISO(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
  SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_WRITE);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeMSRWriteRaw(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_RAW_WRITE);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeRWStart(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_RW_START);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeMSRReadISO(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_READ);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeMSRReadRaw(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_RAW_READ);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeErase(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_ERASE);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeEraseAll(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_ERASE_ALL);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeSetBPI(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_SETBPI);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeSetBPC(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_SETBPC);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;

procedure TMSRDataBuilder.MakeLeadingZeroes(out Arr: TByteArray);
var
  Cmd: TMSRCmd;
begin
   SetLength(Arr,SizeOf(TMSRCmd));
  Cmd := MakeCommand(MSR_CMD_SLZ);
   Move(Cmd,Arr[0],SizeOf(TMSRCmd));
end;





end.



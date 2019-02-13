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
      function MakeCommTest: TByteArray;
      function MakeCheckFirmware: TByteArray;
      function MakeCheckDeviceModel: TByteArray;
      function MakeSensorTest: TByteArray;
      function MakeRAMTest: TByteArray;
      function MakeSetHiCo:TByteArray;
      function MakeSetLowCo:TByteArray;
      function MakeReset: TByteArray;
      function MakeMSRWriteISO: TByteArray;
      function MakeMSRWriteRaw: TByteArray;
      function MakeRWStart: TByteArray;
      function MakeMSRReadISO: TByteArray;
      function MakeMSRReadRaw: TByteArray;
      function MakeErase: TByteArray;
      function MakeEraseAll: TByteArray;
      function MakeSetBPI: TByteArray;
      function MakeSetBPC: TByteArray;
      function MakeLeadingZeroes:TByteArray;
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

function TMSRDataBuilder.MakeCommTest: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_DIAG_COMM);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeCheckFirmware: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_FWREV);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeCheckDeviceModel: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_MODEL);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeSensorTest: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_DIAG_SENSOR);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeRAMTest: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_DIAG_RAM);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeSetHiCo: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_SETCO_HI);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeSetLowCo: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_SETCO_LO);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeReset: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_RESET);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeMSRWriteISO: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_WRITE);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeMSRWriteRaw: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_RAW_WRITE);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeRWStart: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_RW_START);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeMSRReadISO: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_READ);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeMSRReadRaw: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_RAW_READ);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeErase: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_ERASE);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeEraseAll: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_ERASE_ALL);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeSetBPI: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_SETBPI);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeSetBPC: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_SETBPC);
  Result := MyArr;
end;

function TMSRDataBuilder.MakeLeadingZeroes: TByteArray;
var
  Cmd: TMSRCmd;
  MyArr : array[0..1] of byte absolute Cmd;
begin
  Cmd := MakeCommand(MSR_CMD_SLZ);
  Result := MyArr;
end;





end.


unit uconfig;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles;

const
  ConfigFile = 'config.cfg';

type

  { TConfig }

  TConfig = class
  private
    FComPortName: string;
    FCurrentCardValue: int64;
  public
    procedure Load;
    procedure Save;
    property CurrentCardValue: int64 read FCurrentCardValue write FCurrentCardValue;
    property ComPortName: string read FComPortName write FComPortName;
  end;

var
  Config: TConfig;

implementation

{ TConfig }

procedure TConfig.Load;
var
  Ini: TIniFile;
begin
  Ini := nil;
  Ini := TIniFile.Create(Configfile);
  try
    CurrentCardValue := Ini.ReadInt64('card','current_value', 71242);
    ComPortName := Ini.ReadString('msr','portname','COM4');
  finally
    Ini.Free;
  end;
end;

procedure TConfig.Save;
var
  Ini: TIniFile;
begin
  Ini := nil;
  Ini := TIniFile.Create(Configfile);
  try
    Ini.WriteInt64('card','current_value', CurrentCardValue);
    Ini.WriteString('msr','portname',ComPortName);
  finally
    Ini.Free;
  end;
end;

initialization
  Config := TConfig.Create;
  Config.Load;

finalization
  Config.Save;
  Config.Free;
end.


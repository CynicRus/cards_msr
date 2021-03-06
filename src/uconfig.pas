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
    FCardPrefix: string;
    FComPortName: string;
    FCurrentCardValue: int64;
    FEncoderMode: integer;
  public
    procedure Load;
    procedure Save;
    property CurrentCardValue: int64 read FCurrentCardValue write FCurrentCardValue;
    property ComPortName: string read FComPortName write FComPortName;
    property CardPrefix: string read FCardPrefix write FCardPrefix;
    property EncoderMode: integer read FEncoderMode write FEncoderMode;
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
    CardPrefix := Ini.ReadString('card','prefix','778=217090001=');
    ComPortName := Ini.ReadString('msr','portname','COM4');
    EncoderMode := Ini.ReadInteger('msr','encoder_mode',0);
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
    Ini.WriteString('card','prefix',CardPrefix);
    Ini.WriteString('msr','portname',ComPortName);
    Ini.WriteInteger('msr','encoder_mode',EncoderMode);
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


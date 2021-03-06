unit uother;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  TByteArray = array of byte;

function ExtractText(const Str: string; const Delim1, Delim2: char): string;
function ExtractBetween(const Value, A, B: string): string;
procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
function CleanString(Str: string): string;
function NormalizeRequest(Str: string): string;

function StringToByteArray(S: string): TByteArray;
function HexStringToByteArray(S: string): TByteArray;
function ByteArrayToHexString(P: TByteArray): string;

function DynArrayAppend(var V: TByteArray; const R: byte): integer;
function DynArrayAppendByteArray(var V: TByteArray; const R: array of byte): integer;
function StrToByte(const Value: string): TByteArray;
function ByteToString(const Value: TByteArray): string;
function StringToHex(S: string): string;
function HexToString(H: string): string;
function NumStringToBCD(const inStr: string): string;
function BCDToNumString(const inStr: string): string;
function CheckParity(const Data: byte): byte;


implementation

procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function ExtractText(const Str: string; const Delim1, Delim2: char): string;
var
  pos1, pos2: integer;
begin
  Result := '';
  pos1 := Pos(Delim1, Str);
  pos2 := PosEx(Delim2, Str, Pos1 + 1);
  if (pos1 > 0) and (pos2 > pos1) then
    Result := Copy(Str, pos1 + 1, pos2 - pos1 - 1);
end;

function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := PosEx(B, Value, aPos);
    if bPos > 0 then begin
      result := Copy(Value, aPos, bPos - aPos);
    end;
  end;
end;

function CleanString(Str: string): string;
var
  i: integer;
  S: string;
begin
  S := Str;
  for i := Length(s) downto 1 do
    case Copy(s, i, 1) of
      #13, #10: Delete(s, i, 1);
    end;
  Result := s;
end;

function NormalizeRequest(Str: string): string;
var
  St: TStringList;
  ss: TStringStream;
  i: integer;
begin
  Result := '';
  St := TStringList.Create;
  ss := TStringStream.Create(Str);
  try
    St.LoadFromStream(ss);
    St[0] := StringReplace(St[0], 'utf-8', 'UTF-8', [rfReplaceAll]);
    for i := 0 to St.Count - 1 do
      Result := Result + Trim(CleanString(St[i]));
  finally
    ss.Free;
    st.Free;
  end;
end;

function StringToByteArray(S: string): TByteArray;
var
  SLen: int64;
  idx: integer;
begin
  SLen := Length(S);
  SetLength(Result, SLen);
  for idx := 1 to SLen do
  begin
    Result[Pred(idx)] := Ord(S[idx]);
  end;
end;


function HexStringToByteArray(S: string): TByteArray;
var
  idx: integer;
  NibbCnt: integer;  // nr of nibbles
  ByteCnt: integer;  // nr of Bytes
begin
  NibbCnt := Length(S);
  ByteCnt := NibbCnt shr 1;

  SetLength(Result, ByteCnt);

  for idx := 0 to Pred(ByteCnt) do
    Result[idx] := Hex2Dec(MidStr(S, 1 + idx * 2, 2));
end;


function ByteArrayToHexString(P: TByteArray): string;
var
  idx: integer;
begin
  Result := '';
  for idx := Low(P) to High(P) do
    Result := Result + IntToHex(P[idx], 2);
end;

function DynArrayAppend(var V: TByteArray; const R: byte): integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function DynArrayAppendByteArray(var V: TByteArray; const R: array of byte): integer;
var
  L: integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
  begin
    SetLength(V, Result + L);
    Move(R[0], V[Result], Sizeof(byte) * L);
  end;
end;

function StrToByte(const Value: string): TByteArray;
var
  I: integer;
begin
  SetLength(Result, Length(Value));
  for I := 0 to Length(Value) - 1 do
    Result[I] := Byte(Value[I + 1]);
end;

function ByteToString(const Value: TByteArray): string;
var
  I: integer;
  S: string;
  Letra: char;
begin
  S := '';
  for I := Length(Value) - 1 downto 0 do
  begin
    letra := Chr(Value[I]);
    S := letra + S;
  end;
  Result := S;
end;


function StringToHex(S: string): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to length(S) do
    Result := Result + IntToHex(Ord(S[i]), 2);
end;

function HexToString(H: string): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to length(H) div 2 do
    Result := Result + char(StrToInt('$' + Copy(H, (I - 1) * 2 + 1, 2)));
end;

function NumStringToBCD(const inStr: string): string;

  function Pack(ch1, ch2: char): char;
  begin
    Assert((ch1 >= '0') and (ch1 <= '9'));
    Assert((ch2 >= '0') and (ch2 <= '9'));
      {Ord('0') is $30, so we can just use the low nybble of the character
       as value.}
    Result := Chr((Ord(ch1) and $F) or ((Ord(ch2) and $F) shl 4));
  end;

var
  i: integer;
begin
  if Odd(Length(inStr)) then
    Result := NumStringToBCD('0' + instr)
  else
  begin
    SetLength(Result, Length(inStr) div 2);
    for i := 1 to Length(Result) do
      Result[i] := Pack(inStr[2 * i - 1], inStr[2 * i]);
  end;
end;

function BCDToNumString(const inStr: string): string;

  procedure UnPack(ch: char; var ch1, ch2: char);
  begin
    ch1 := Chr((Ord(ch) and $F) + $30);
    ch2 := Chr(((Ord(ch) shr 4) and $F) + $30);
    Assert((ch1 >= '0') and (ch1 <= '9'));
    Assert((ch2 >= '0') and (ch2 <= '9'));
  end;

var
  i: integer;
begin
  SetLength(Result, Length(inStr) * 2);
  for i := 1 to Length(inStr) do
    UnPack(inStr[i], Result[2 * i - 1], Result[2 * i]);
end;

function CheckParity(const Data: byte): byte;
var
  parity, b: byte;
begin
  b := 0;
  parity := 0;
  b := Data;
  while b <> 0 do
  begin
    parity := parity xor b;
    b := b shr 1;
  end;
  Result := (parity and $1);
end;



end.

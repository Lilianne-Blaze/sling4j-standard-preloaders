unit LBCode.StringUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function StartsWithIgnoreCase(const Str, SubStr: string): boolean;

function TruncateIfNeeded(Str: string; MaxLen: integer = 1024): string;

procedure SplitString(const Input: string; const Delimiter: char;
  constref Parts: TStringList);

function SplitStringInTwo(const Input: string; const Delimiter: char;
  var Part1, Part2: string): boolean;

function ExcludeTrailingNull(const Str: string): string;


implementation


function StartsWithIgnoreCase(const Str, SubStr: string): boolean;
var
  LowerStr, LowerSubStr: string;
begin
  LowerStr := LowerCase(Str);
  LowerSubStr := LowerCase(SubStr);
  Result := Pos(LowerSubStr, LowerStr) = 1;
end;


function TruncateIfNeeded(Str: string; MaxLen: integer = 1024): string;
const
  Ending = '...';
begin
  if Str.Length <= MaxLen then
    Result := Str
  else
    Result := Str.Substring(0, MaxLen - Ending.Length) + Ending;
end;


procedure SplitString(const Input: string; const Delimiter: char;
  constref Parts: TStringList);
var
  StartPos, EndPos: integer;
begin
  StartPos := 1;

  while startPos <= Length(input) do
  begin
    EndPos := Pos(Delimiter, Input, StartPos);

    if endPos = 0 then
      EndPos := Length(Input) + 1;

    Parts.Add(Copy(Input, StartPos, EndPos - startPos));
    StartPos := EndPos + 1;
  end;
end;


function SplitStringInTwo(const Input: string; const Delimiter: char;
  var Part1, Part2: string): boolean;
var
  DelimPos: integer;
begin
  DelimPos := Pos(Delimiter, Input);
  if DelimPos > 0 then
  begin
    Part1 := Copy(Input, 1, DelimPos - 1);
    Part2 := Copy(Input, DelimPos + 1, Length(Input) - DelimPos);
    Result := True;
  end
  else
  begin
    Part1 := Input;
    Part2 := '';
    Result := False;
  end;
end;


function ExcludeTrailingNull(const Str: string): string;
begin
  if Str[Str.Length] = #0 then
    Exit(Copy(Str, 1, Str.Length - 1));

  Result := Copy(Str, 1, Str.Length);
end;


end.


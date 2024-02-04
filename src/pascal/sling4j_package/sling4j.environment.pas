unit Sling4j.Environment;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, LBCode.Collections;

procedure WriteDumpsConditional();


type
  TStringDictionary = specialize TDictionary<string, string>;

type
  TIntStringDictionary = specialize TDictionary<integer, string>;


var
  EnvVars: TStringDictionary;

var
  RCStrings: TIntStringDictionary;

var
  JavaVars: TStringDictionary;

var
  Sling4j_dump: string;

implementation

uses
  Windows,
  LBCode.StringUtils,
  LBCode.Windows.Utils;




procedure InitRcStrings();
var
  ModuleHandle: HMODULE;
  rcStringIds: TIntList;
  i: integer;
  s: string;
begin
  ModuleHandle := GetModuleHandle(nil);

  rcStringIds := ListRCStringIDs(ModuleHandle);
  for i in rcStringIds do
  begin
    s := GetRCString(i);
    s := ExcludeTrailingNull(s);
    rcStrings.Add(i, s);
  end;

  rcStringIds.Clear;
  FreeAndNil(rcStringIds);
  FreeModule(ModuleHandle);
end;


procedure InitJavaVarsFromRc();
var
  pairs: TStringList;
  str19: string;
  pair, key, Value: string;
begin
  if not rcStrings.ContainsKey(19) then
    exit;

  str19 := rcStrings[19];
  if str19 = '' then
    exit;

  pairs := TStringList.Create;
  SplitString(str19, #9, pairs);

  for pair in pairs do
  begin
    SplitStringInTwo(pair, '=', key, Value);
    javaVars.Add(key, Value);
  end;

  pairs.Clear;
  FreeAndNil(pairs);
end;


procedure InitSling4jVars();
begin
  javaVars.TryGetValue('sling4j.dump', sling4j_dump);

end;


procedure WriteDumpsConditional();
begin
  if sling4j_dump.Contains('envVars') or sling4j_dump.Contains('all') then
  begin
    WriteDictionary(envVars, 'envVars');
    WriteLn();
  end;

  if sling4j_dump.Contains('rcStrings') or sling4j_dump.Contains('all') then
  begin
    WriteDictionary(rcStrings, 'rcStrings');
    WriteLn();
  end;

  if sling4j_dump.Contains('javaVars') or sling4j_dump.Contains('all') then
  begin
    WriteDictionary(javaVars, 'javaVars');
    WriteLn();
  end;

end;


initialization
  begin
    envVars := GetEnvironmentAsDictionary();

    rcStrings := TIntStringDictionary.Create;
    InitRcStrings();

    javaVars := TStringDictionary.Create;
    InitJavaVarsFromRc();

    InitSling4jVars();

  end;

finalization
  begin
    FreeAndNil(envVars);
    FreeAndNil(rcStrings);
    FreeAndNil(javaVars);
  end;

end.

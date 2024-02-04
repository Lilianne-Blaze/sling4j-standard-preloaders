unit LBCode.Collections;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type
  generic IPredicate<T> = interface
    function Test(constref v: T): boolean; virtual;
  end;


type
  TStringDictionary = specialize TDictionary<string, string>;

type
  TIntStringDictionary = specialize TDictionary<integer, string>;

type
  TIntList = specialize TList<integer>;


type
  generic TFilterSorter<T> = class(TInterfacedObject, specialize IPredicate<T>,
    specialize IComparer<T>)
    function Test(constref v: T): boolean; virtual;
    function Compare(constref Left, Right: T): integer; virtual;
  end;


type
  generic TList2<T> = class(specialize TList<T>)
  public
  type
    PredFunc = function(constref v: T): boolean;
    procedure WriteLns(Name: string = 'list'); virtual;
    function ToString(const v: T): string; virtual;
    procedure ClearAndFree(); virtual;
    procedure RetainIf(constref p: specialize IPredicate<T>;
      FreeRemoved: boolean = False); virtual;
    procedure RemoveIf(constref p: specialize IPredicate<T>;
      FreeRemoved: boolean = False); virtual;
    procedure RetainIf(constref p: PredFunc; FreeRemoved: boolean = False); virtual;
    procedure RemoveIf(constref p: PredFunc; FreeRemoved: boolean = False); virtual;
  private
    procedure RemoveIf0(constref p: specialize IPredicate<T>;
      RemoveOnTrue: boolean; FreeRemoved: boolean = False); virtual;
    procedure RemoveIf0(constref p: PredFunc; RemoveOnTrue: boolean;
      FreeRemoved: boolean = False); virtual;
  end;


procedure ClearFreeNil(var dict: TStringDictionary); overload;

procedure ClearFreeNil(var list: TStringList); overload;


function GetEnvironmentAsDictionary: TStringDictionary;


procedure WriteStringDictionary(const dict: TStringDictionary; Name: string = 'dict');

generic procedure WriteDictionary<K, V>(constref dict: specialize TDictionary<K, V>;
  Name: string = 'dict');

procedure WriteDictionary(constref dict: TStringDictionary; Name: string = 'dict');

procedure WriteDictionary(constref dict: TIntStringDictionary; Name: string = 'dict');


procedure WriteStringList(constref list: TStringList; Name: string = 'list');


implementation

generic procedure TList2<T>.WriteLns(Name: string = 'list');
var
  x: T;
  i: integer = 0;
  s: string;
begin
  for x in self do
  begin
    s := ToString(x);
    WriteLn(Name, '[', i, '] = "', ToString(x), '"');
    Inc(i);
  end;
end;


generic function TList2<T>.ToString(const v: T): string;
begin
  Result := PtrUInt(v).ToHexString();
end;


generic procedure TList2<T>.ClearAndFree();
var
  i: integer;
  v: T;
begin
  while Count > 0 do
  begin
    v := Last();
    Delete(Count - 1);
    v.Free;
  end;
end;


generic function TFilterSorter<T>.Test(constref v: T): boolean;
begin
  Result := True;
end;

generic function TFilterSorter<T>.Compare(constref Left, Right: T): integer;
begin
  Result := 0;
end;


generic procedure TList2<T>.RetainIf(constref p: specialize IPredicate<T>;
  FreeRemoved: boolean = False);
begin
  RemoveIf0(p, False, FreeRemoved);
end;

generic procedure TList2<T>.RemoveIf(constref p: specialize IPredicate<T>;
  FreeRemoved: boolean = False);
begin
  RemoveIf0(p, True, FreeRemoved);
end;

generic procedure TList2<T>.RetainIf(constref p: PredFunc; FreeRemoved: boolean = False);
begin
  RemoveIf0(p, False, FreeRemoved);
end;

generic procedure TList2<T>.RemoveIf(constref p: PredFunc; FreeRemoved: boolean = False);
begin
  RemoveIf0(p, True, FreeRemoved);
end;


generic procedure TList2<T>.RemoveIf0(constref p: specialize IPredicate<T>;
  RemoveOnTrue: boolean; FreeRemoved: boolean = False);
var
  v: T;
  i: integer;
begin
  i := 0;
  while i < Count do
  begin
    v := Items[i];
    if p.Test(v) = RemoveOnTrue then
    begin
      Remove(v);
      if FreeRemoved then
        v.Free;
    end
    else
    begin
      Inc(i);
    end;
  end;
end;


generic procedure TList2<T>.RemoveIf0(constref p: PredFunc;
  RemoveOnTrue: boolean; FreeRemoved: boolean = False);
var
  v: T;
  i: integer;
begin
  i := 0;
  while i < Count do
  begin
    v := Items[i];
    if p(v) = RemoveOnTrue then
    begin
      Remove(v);
      if FreeRemoved then
        v.Free;
    end
    else
    begin
      Inc(i);
    end;
  end;
end;


procedure ClearFreeNil(var dict: TStringDictionary); overload;
begin
  dict.Clear;
  dict.Free;
  dict := nil;
end;

procedure ClearFreeNil(var list: TStringList); overload;
var
  s: string;
begin
  list.Clear;
  list.Free;
  list := nil;
end;


function GetEnvironmentAsDictionary: TStringDictionary;
var
  Dict: TStringDictionary;
  Count, i, SepPos: integer;
  EnvStr, EnvName, EnvValue: string;
begin
  Dict := TStringDictionary.Create;

  Count := GetEnvironmentVariableCount;
  for i := 0 to Count do
  begin
    EnvStr := GetEnvironmentString(i);
    SepPos := EnvStr.IndexOf('=', 1);
    EnvName := EnvStr.Substring(0, SepPos);
    EnvValue := EnvStr.Substring(SepPos + 1);
    Dict.TryAdd(EnvName, EnvValue);
  end;

  Result := dict;
end;


procedure WriteStringDictionary(const Dict: TStringDictionary; Name: string = 'dict');
var
  i, Count: integer;
  EnvName, EnvValue: string;
begin
  Count := Dict.Count;
  WriteLn(Name, '.Count=', Count);

  for EnvName in Dict.Keys do
  begin
    EnvValue := Dict[EnvName];
    WriteLn(Name, '["', EnvName, '"]="', EnvValue, '"');
  end;
end;


generic procedure WriteDictionary<K, V>(constref Dict: specialize TDictionary<K, V>;
  Name: string = 'dict');
var
  Count: integer;
  Key: K;
  Value: V;
begin
  Count := Dict.Count;
  WriteLn(Name, '.Count=', Count);

  for Key in Dict.Keys do
  begin
    Value := Dict[Key];
    WriteLn(Name, '["', Key, '"]="', Value, '"');
  end;
end;

procedure WriteDictionary(constref Dict: TStringDictionary; Name: string = 'dict');
begin
  specialize WriteDictionary<string, string>(Dict, Name);
end;

procedure WriteDictionary(constref Dict: TIntStringDictionary; Name: string = 'dict');
begin
  specialize WriteDictionary<integer, string>(Dict, Name);
end;

procedure WriteStringList(constref List: TStringList; Name: string = 'list');
var
  s: string;
  i: integer = 0;
begin
  WriteLn(Name, '.Count=', List.Count);
  for s in List do
  begin
    WriteLn(Name, '[', i, ']="', s, '"');
    Inc(i);
  end;
end;

end.

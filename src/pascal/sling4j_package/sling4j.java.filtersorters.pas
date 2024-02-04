unit Sling4j.Java.FilterSorters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LBCode.Collections, Sling4j.Java.Dirs;

type
  TJavaDirFilterSorter = specialize TFilterSorter<TJavaDir>;
  TJavaDirFilterSorterList = specialize TList2<TJavaDirFilterSorter>;


type
  TBitnessFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    Bitness: string;
    constructor Create(NewBitness: string);
    function Test(constref v: TJavaDir): boolean; override;
    function Compare(constref Left, Right: TJavaDir): integer; override;
  end;


type
  TNeedsJavaFXFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    function Test(constref v: TJavaDir): boolean; override;
  end;


type
  TNeedsJdkFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    function Test(constref v: TJavaDir): boolean; override;
  end;


implementation


// accepts "64", "32", "64/32", "32/64", "64,32", "32,64"
constructor TBitnessFilterSorter.Create(NewBitness: string);
begin
  Bitness := NewBitness.Replace(',', '/');
end;

function TBitnessFilterSorter.Test(constref v: TJavaDir): boolean;
var
  Match32, Match64: boolean;
begin
  Match32 := Bitness.Contains('32') and not v.Is64bit;
  Match64 := Bitness.Contains('64') and v.Is64bit;
  Result := Match32 or Match64;
end;

function TBitnessFilterSorter.Compare(constref Left, Right: TJavaDir): integer;
var
  Direction: integer = 1;
begin
  if not Bitness.Equals('64/32') and not Bitness.Equals('32/64') then
    exit(0);

  if Bitness.Equals('32/64') then
    Direction := -1;

  if Left.Is64bit > Right.Is64bit then
    Result := -1 * Direction
  else if Left.Is64bit < Right.Is64bit then
    Result := 1 * Direction
  else
    Result := 0;
end;


function TNeedsJavaFXFilterSorter.Test(constref v: TJavaDir): boolean;
begin
  Result := v.HasJavaFX;
end;


function TNeedsJdkFilterSorter.Test(constref v: TJavaDir): boolean;
begin
  Result := v.IsJdk;
end;


end.

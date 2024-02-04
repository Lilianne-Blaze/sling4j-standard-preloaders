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


type
  TMinMaxVersionFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    MinVerInt: integer;
    MaxVerInt: integer;
    constructor Create(const AMinVerInt, AMaxVerInt: integer);
    function Test(constref v: TJavaDir): boolean; override;
    function Compare(constref Left, Right: TJavaDir): integer; override;
  end;


  // TODO: make it sort by full version and date
type
  TNewestFirstFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    NewestFirst: boolean;
    constructor Create(const ANewestFirst: boolean = True);
    function Compare(constref Left, Right: TJavaDir): integer; override;
  end;


  // see https://www.oracle.com/java/technologies/java-se-support-roadmap.html
type
  TPreferLTSFilterSorter = class(specialize TFilterSorter<TJavaDir>)
    OnlyLTS: boolean;
    TreatFutureAsLTS: boolean;
    constructor Create(const AOnlyLTS: boolean = False;
      const ATreatFutureAsLTS: boolean = False);
    function Test(constref v: TJavaDir): boolean; override;
    function Compare(constref Left, Right: TJavaDir): integer; override;
  end;


implementation

uses
  LBCode.MiscUtils;


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

constructor TMinMaxVersionFilterSorter.Create(const AMinVerInt, AMaxVerInt: integer);
begin
  MinVerInt := AMinVerInt;
  MaxVerInt := AMaxVerInt;
end;

function TMinMaxVersionFilterSorter.Test(constref v: TJavaDir): boolean;
begin
  Result := (v.FeatureVersion >= MinVerInt) and (v.FeatureVersion <= MaxVerInt);
end;

function TMinMaxVersionFilterSorter.Compare(constref Left, Right: TJavaDir): integer;
begin
  // newer first
  Result := Right.FeatureVersion - Left.FeatureVersion;
end;


function IsLTS(fv: integer; TreatFutureAsLTS: boolean = False): boolean;
begin
  if (fv = 8) or (fv = 11) or (fv = 17) or (fv = 21) then
    Exit(True);

  if (fv >= 25) and TreatFutureAsLTS then
    Exit(True);

  Result := False;
end;


constructor TPreferLTSFilterSorter.Create(const AOnlyLTS: boolean = False;
  const ATreatFutureAsLTS: boolean = False);
begin
  OnlyLTS := AOnlyLTS;
  TreatFutureAsLTS := ATreatFutureAsLTS;
end;

function TPreferLTSFilterSorter.Test(constref v: TJavaDir): boolean;
var
  fv: integer;
begin
  fv := v.FeatureVersion;

  if not OnlyLTS then
    Exit(True);

  Result := IsLTS(fv, TreatFutureAsLTS);
end;

function TPreferLTSFilterSorter.Compare(constref Left, Right: TJavaDir): integer;
begin
  // LTS first
  Result := -CompareBool(IsLTS(Left.FeatureVersion), IsLTS(Right.FeatureVersion));
end;


constructor TNewestFirstFilterSorter.Create(const ANewestFirst: boolean = True);
begin
  NewestFirst := ANewestFirst;
end;

function TNewestFirstFilterSorter.Compare(constref Left, Right: TJavaDir): integer;
var
  lv, rv: integer;
begin
  lv := Left.FeatureVersion;
  rv := Right.FeatureVersion;

  if NewestFirst then
    Result := rv - lv
  else
    Result := lv - rv;
end;


end.

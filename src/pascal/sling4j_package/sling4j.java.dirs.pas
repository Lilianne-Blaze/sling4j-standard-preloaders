unit Sling4j.Java.Dirs;

{$mode ObjFPC}{$H+}
{$unitpath ..\lbcode_package }

interface

uses
  Classes,
  SysUtils,
  LBCode.Collections;

type
  TJavaDir = class
  public
    Directory: string;
    Vendor: string;
    IsValid: boolean;
    FeatureVersion: integer;
    IsJdk: boolean;
    IsGraal: boolean;
    Is64bit: boolean;
    HasJavaFX: boolean;
    ReleaseDate: TDateTime;
    ReleaseDateStr: string;
    function InitFromDir(const InitDir: string): boolean; virtual;
    function ToString(): string; override;
    constructor Create();
    constructor CreateFromDir(const InitDir: string);
    destructor Destroy(); override;
  end;

type
  TJavaDirList = class(specialize TList2<TJavaDir>)
    function ToString(const v: TJavaDir): string; override;
  end;


function ProbablyContainsJava(const dir: string): boolean;

function ReadJavaRelease(const Dir: string;
  constref ReleaseDict: TStringDictionary): boolean;

function GetJavaFeatureVersion(const Dict: TStringDictionary): integer;

function SupportsJavaFx(const Dir: string; const Dict: TStringDictionary): boolean;

function IsJavaJDK(const Dir: string): boolean;

function IsGraalVM(const Dir: string): boolean;

function IsJava64(const Dict: TStringDictionary): boolean;

function DetectJavaDirs(constref SearchDirs: TStringList;
  constref JavaDirs: TJavaDirList): boolean;

function GetJavaReleaseDateStr(constref dict: TStringDictionary;
  var DateStr: string): boolean;

function GetJavaReleaseDate(constref dict: TStringDictionary;
  var Date: TDateTime): boolean;


implementation

uses
  StrUtils, Types;

function TJavaDir.InitFromDir(const InitDir: string): boolean;
var
  ReleaseDict: TStringDictionary;
  s: string;
begin
  if not ProbablyContainsJava(initDir) then
  begin
    IsValid := False;
    Exit(False);
  end;

  Directory := InitDir;
  IsValid := True;

  ReleaseDict := TStringDictionary.Create();

  ReadJavaRelease(InitDir, ReleaseDict);
  featureVersion := GetJavaFeatureVersion(ReleaseDict);
  IsJdk := IsJavaJDK(InitDir);
  IsGraal := IsGraalVM(InitDir);
  Is64bit := IsJava64(ReleaseDict);
  HasJavaFX := SupportsJavaFx(InitDir, ReleaseDict);

  GetJavaReleaseDateStr(ReleaseDict, ReleaseDateStr);
  GetJavaReleaseDate(ReleaseDict, ReleaseDate);

  ClearFreeNil(ReleaseDict);

  Result := True;
end;

function TJavaDir.ToString: string;
begin
  Result := '"' + Directory + '"';
  Result := Result + ',ver=' + IntToStr(FeatureVersion);
  Result := Result + IfThen(Is64bit, ',64bit', ',32bit');
  Result := Result + IfThen(IsJdk, ',JDK', ',JRE');

  if HasJavaFX then
    Result := Result + ',JavaFX';
  if IsGraal then
    Result := Result + ',GraalVM';
end;

constructor TJavaDir.Create();
begin
  inherited Create;
  //WriteLn('TJavaDir.Create() called.');
end;

constructor TJavaDir.CreateFromDir(const InitDir: string);
begin
  TJavaDir.Create();
  InitFromDir(InitDir);
end;

destructor TJavaDir.Destroy();
begin
  //WriteLn('TJavaDir.Destroy() called.');
  inherited Destroy;
end;


function ProbablyContainsJava(const Dir: string): boolean;
const
  JavaExe = 'bin\java.exe';
  Release = 'release';
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(Dir);
  Result := FileExists(s + JavaExe) and FileExists(s + Release);
end;


function ReadJavaRelease(const Dir: string;
  constref ReleaseDict: TStringDictionary): boolean;
var
  FileLines: TStringList;
  Line: string;
  EqualsPos: integer;
  FileName: string;
  s, Key, Value: string;
begin
  FileName := IncludeTrailingPathDelimiter(Dir) + '\release';
  FileLines := TStringList.Create;

  try
    FileLines.LoadFromFile(fileName);

    for Line in FileLines do
    begin
      EqualsPos := Pos('=', Line);
      if EqualsPos > 0 then
      begin
        Key := Trim(Copy(Line, 1, EqualsPos - 1));
        Value := Trim(Copy(Line, EqualsPos + 1, MaxInt));
        Value := AnsiDequotedStr(Value, '"');
        ReleaseDict.Add(Key, Value);
      end;
    end;

    Result := True;

  except
    on e: Exception do
    begin
      //Writeln('Error: ', E.Message);
      Result := False;
      //TODO: what to do? return empty?
    end;
  end;

  FileLines.Clear;
  FreeAndNil(FileLines);
end;


function GetJavaFeatureVersion(const Dict: TStringDictionary): integer;
var
  s: string;
  Parts: TStringDynArray;
  VerInt: integer;
begin
  Result := -1;
  s := Dict.Items['JAVA_VERSION'];
  Parts := SplitString(s, '.');

  VerInt := StrToIntDef(Parts[0], -1);
  if VerInt = 1 then
    VerInt := StrToIntDef(Parts[1], -1);

  Result := VerInt;
end;


function SupportsJavaFx(const Dir: string; const Dict: TStringDictionary): boolean;
const
  Key = 'MODULES';
  JavafxFxml = 'javafx.fxml';
  CheckFile = 'lib\javafx.properties';
var
  ModulesStr: string;
  s: string;
begin
  Result := False;

  // Java 8
  s := IncludeTrailingPathDelimiter(Dir) + CheckFile;
  if FileExists(s) then
  begin
    Exit(True);
  end;

  // Java 9+
  if Dict.ContainsKey(Key) then
  begin
    ModulesStr := Dict.Items['MODULES'];
    Result := Pos(JavafxFxml, ModulesStr) > 0;
  end;
end;


function IsJavaJDK(const Dir: string): boolean;
const
  JavacExe = 'bin\javac.exe';
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(Dir);
  Result := FileExists(s + JavacExe);
end;


function IsGraalVM(const Dir: string): boolean;
const
  File1 = 'bin\native-image.cmd';
  File2 = 'bin\native-image.exe'; // yes I know there's no exe for now
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(Dir);
  Result := FileExists(s + File1) or FileExists(s + File2);
end;


function IsJava64(const Dict: TStringDictionary): boolean;
const
  OsArchKey = 'OS_ARCH';
  Match1 = 'x86_64';
  Match2 = 'amd64';
var
  OsArch: string;
begin
  if Dict.ContainsKey(OsArchKey) then
  begin
    OsArch := Dict.Items[OsArchKey];
    Result := OsArch.Equals(Match1) or OsArch.Equals(Match2);
  end;
end;


function DetectJavaDirs(constref SearchDirs: TStringList;
  constref JavaDirs: TJavaDirList): boolean;
var
  JavaDir: TJavaDir;
  s: string;
begin
  for s in SearchDirs do
  begin
    if ProbablyContainsJava(s) then
    begin
      JavaDir := TJavaDir.Create;

      JavaDir.InitFromDir(s);
      JavaDirs.Add(JavaDir);

      JavaDir := nil;
    end;
  end;

  Result := True;
end;




function GetJavaReleaseDateStr(constref Dict: TStringDictionary;
  var DateStr: string): boolean;
const
  Key = 'JAVA_VERSION_DATE';
begin
  Result := False;
  if not Dict.ContainsKey(Key) then
    Exit(False);

  DateStr := Dict.Items[Key];
  Result := True;
end;


function GetJavaReleaseDate(constref Dict: TStringDictionary;
  var Date: TDateTime): boolean;
const
  Key = 'JAVA_VERSION_DATE';
var
  DateStr: string;
  Year, Month, Day: longint;
begin
  Result := False;

  if not Dict.ContainsKey(key) then
    Exit(False);

  DateStr := Dict.Items[key];

  if Length(DateStr) < 10 then
    Exit(False);

  if not TryStrToInt(Copy(DateStr, 1, 4), Year) or not
    TryStrToInt(Copy(DateStr, 6, 2), Month) or not
    TryStrToInt(Copy(DateStr, 9, 2), Day) then
    Exit(False);

  Date := EncodeDate(Year, Month, Day);
  Result := True;
end;


function TJavaDirList.ToString(const v: TJavaDir): string;
begin
  Result := v.ToString();
end;


end.

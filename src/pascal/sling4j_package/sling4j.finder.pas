unit Sling4j.Finder;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes,
  SysUtils,
  Sling4j.Java.FilterSorters,
  Sling4j.Java.Dirs;

type
  TSling4jFinder = class
  private
    SearchDirsField: TStringList;
    FiltersField: TJavaDirFilterSorterList;
    JavaDirsFoundField: TJavaDirList;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure AddDir(const dir: string); virtual;
    procedure AddJavaHome(); virtual;
    procedure AddCommonVendors(); virtual;
    procedure AddFilter(const AFilterSorter: TJavaDirFilterSorter); virtual;
    procedure Find(); virtual;
    procedure Dump(); virtual;
    procedure ResetDirs(); virtual;
    procedure ResetFilters(); virtual;
    procedure ResetFound(); virtual;
    function WereAnyFound(): boolean; virtual;
    function GetFirstFound(): TJavaDir; virtual;
  end;

implementation

uses
  LBNet.Windows.Utils,
  LBNet.Collections,
  LBNet.SystemUtils;

constructor TSling4jFinder.Create();
begin
  WriteLn('Entering TSling4jFinder.Create, ', GetMemBlockIdSize(Self));
  inherited Create();

  SearchDirsField := TStringList.Create();
  FiltersField := TJavaDirFilterSorterList.Create();
  JavaDirsFoundField := TJavaDirList.Create();

end;

destructor TSling4jFinder.Destroy();
begin
  WriteLn('Entering TSling4jFinder.Destroy');

  FreeAndNil(SearchDirsField);
  FiltersField.ClearAndFree();
  FreeAndNil(FiltersField);
  JavaDirsFoundField.ClearAndFree();
  FreeAndNil(JavaDirsFoundField);

  inherited Destroy();
end;


procedure TSling4jFinder.Find();
var
  JavaDirList: TStringList;
  JavaDir: TJavaDir;
  s: string;
  Filter: TJavaDirFilterSorter;
  JavaDirTempList: TJavaDirList;
begin
  JavaDirTempList := TJavaDirList.Create;

  for s in Self.SearchDirsField do
  begin
    JavaDir := TJavaDir.Create;
    JavaDir.InitFromDir(s);
    JavaDirTempList.Add(JavaDir);
  end;

  // filter
  for Filter in Self.FiltersField do
  begin
    JavaDirTempList.RetainIf(Filter, True);
  end;

  // sort
  for Filter in Self.FiltersField do
  begin
    JavaDirTempList.Sort(Filter);
  end;

  for JavaDir in JavaDirTempList do
  begin
    JavaDirsFoundField.Add(JavaDir);
  end;

  //JavaDirTempList.WriteLns('JavaDirTempList');

  JavaDirTempList.Clear();
  FreeAndNil(JavaDirTempList);
end;


procedure TSling4jFinder.AddDir(const Dir: string);
var
  s: string;
  i: integer;
begin
  if Dir = '' then
    Exit;

  s := GetExpandedEnvironmentStrings(Dir);
  s := ExcludeTrailingPathDelimiter(s);

  if not (s = '') then
    if SearchDirsField.IndexOf(s) = -1 then
      if ProbablyContainsJava(s) then
        SearchDirsField.Add(s);

end;

procedure TSling4jFinder.AddJavaHome();
begin
  AddDir('%JAVA_HOME%');
end;

procedure TSling4jFinder.AddCommonVendors();
var
  ProgDirList: TStringList;
  DirList: TStringList;
  s: string;
begin
  ProgDirList := TStringList.Create;
  DirList := TStringList.Create;

  AddProgramFilesDirectories(ProgDirList);

  for s in ProgDirList do
  begin
    AddSubdirectoriesMaxDepth(DirList, s + '\Bellsoft', 1);
    AddSubdirectoriesMaxDepth(DirList, s + '\Eclipse Adoptium', 1);
    AddSubdirectoriesMaxDepth(DirList, s + '\Java', 1);
    AddSubdirectoriesMaxDepth(DirList, s, 1);
  end;

  for s in DirList do
  begin
    if ProbablyContainsJava(s) then
      AddDir(s);
  end;

  ClearFreeNil(ProgDirList);
  ClearFreeNil(DirList);
end;

procedure TSling4jFinder.AddFilter(const AFilterSorter: TJavaDirFilterSorter);
begin
  FiltersField.Add(AFilterSorter);
end;

procedure TSling4jFinder.Dump();
begin
  WriteStringList(SearchDirsField, 'SearchDirs');
  JavaDirsFoundField.WriteLns('JavaDirsFound');
end;

procedure TSling4jFinder.ResetDirs();
begin
  SearchDirsField.Clear();
end;

procedure TSling4jFinder.ResetFilters();
begin
  FiltersField.ClearAndFree();
end;

procedure TSling4jFinder.ResetFound();
begin
  JavaDirsFoundField.ClearAndFree();
end;

function TSling4jFinder.WereAnyFound(): boolean;
begin
  Result := JavaDirsFoundField.Count > 0;
end;

function TSling4jFinder.GetFirstFound(): TJavaDir;
begin
  Result := JavaDirsFoundField[0];
end;


end.

unit Sling4j.Engine;

{$mode objfpc}{$H+}
{$interfaces corba }

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Sling4j.Java.FilterSorters,
  Sling4j.Java.Dirs;

type
  ISling4jEngine = interface;
  ISling4jSearch = interface;
  ISling4jFinder = interface;
  ISling4jFound = interface;

type
  TSling4jEngine = class;
  TSling4jSearch = class;
  TSling4jFinder = class;
  TSling4jFound = class;


type
  ISling4jEngine = interface
    function Search(): TSling4jSearch; virtual;

  end;


type
  ISling4jSearch = interface
    procedure Dump();
    procedure AddDir(const dir: string); virtual;
    procedure AddJavaHome(); virtual;
    procedure AddCommonVendors(); virtual;

  end;


type
  ISling4jFinder = interface
    procedure Find(); virtual;
  end;

type
  ISling4jFound = interface
    function Count(): integer; virtual;
  end;


type
  TSling4jEngine = class(ISling4jEngine)
  private
    SearchField: TSling4jSearch;
    FinderField: TSling4jFinder;
    FoundField: TSling4jFound;
  public
    constructor Create();
    destructor Destroy(); override;
    function Search(): TSling4jSearch; virtual;
    function Finder(): TSling4jFinder; virtual;
    function Found(): TSling4jFound; virtual;
  end;

type
  TSling4jSearch = class(ISling4jSearch)
  private
    OwnerField: TSling4jEngine;
    SearchDirListField: TStringList;
  public
    constructor Create(const AOwner: TSling4jEngine);
    destructor Destroy(); override;
    procedure Dump(); virtual;
    procedure AddDir(const dir: string); virtual;
    procedure AddJavaHome(); virtual;
    procedure AddCommonVendors(); virtual;
  end;



type
  TSling4jFinder = class(ISling4jFinder)
  private
    OwnerField: TSling4jEngine;
  public
    constructor Create(const AOwner: TSling4jEngine);
    destructor Destroy(); override;
    procedure Find(); virtual;
  end;

type
  TSling4jFound = class(ISling4jFound)
  private
    OwnerField: TSling4jEngine;
  public
    constructor Create(AOwner: TSling4jEngine);
    destructor Destroy(); override;
    function Count(): integer; virtual;
  end;




var
  Sling: TSling4jEngine;


implementation

uses
  LBNet.Windows.Utils,
  LBNet.Collections,
  LBNet.SystemUtils;


  // ===== TSling4jEngine =====

constructor TSling4jEngine.Create();
begin
  WriteLn('Entering ', Self.ClassName, '.Create, ', GetMemBlockIdSize(Self));
  inherited Create();

  SearchField := TSling4jSearch.Create(Self);
  FinderField := TSling4jFinder.Create(Self);
  FoundField := TSling4jFound.Create(Self);
end;

destructor TSling4jEngine.Destroy();
begin
  WriteLn('Entering TSling4jEngine.Destroy');

  FreeAndNil(SearchField);
  FreeAndNil(FinderField);
  FreeAndNil(FoundField);
  inherited Destroy();
end;

function TSling4jEngine.Search(): TSling4jSearch;
begin
  Result := Self.SearchField;
end;


function TSling4jEngine.Finder(): TSling4jFinder;
begin
  Result := Self.FinderField;
end;

function TSling4jEngine.Found(): TSling4jFound;
begin
  Result := Self.FoundField;
end;

// ===== TSling4jSearch =====

constructor TSling4jSearch.Create(const AOwner: TSling4jEngine);
begin
  WriteLn('Entering TSling4jSearch.Create, ', GetMemBlockIdSize(Self));
  inherited Create();
  Self.OwnerField := AOwner;

  Self.SearchDirListField := TStringList.Create();
end;

destructor TSling4jSearch.Destroy;
begin
  WriteLn('Entering TSling4jSearch.Destroy');

  SearchDirListField.Clear();
  FreeAndNil(SearchDirListField);

  OwnerField := nil;
  inherited Destroy();
end;

procedure TSling4jSearch.Dump();
begin
  WriteStringList(SearchDirListField, 'FSearchDirList');
end;

procedure TSling4jSearch.AddDir(const Dir: string);
begin
end;

procedure TSling4jSearch.AddCommonVendors();
begin
end;

procedure TSling4jSearch.AddJavaHome();
begin
end;

// ===== TSling4jFilters =====


// ===== TSling4jFinder =====

constructor TSling4jFinder.Create(const AOwner: TSling4jEngine);
begin
  WriteLn('Entering TSling4jFinder.Create, ', GetMemBlockIdSize(Self));
  inherited Create();
  Self.OwnerField := AOwner;

end;

destructor TSling4jFinder.Destroy;
begin
  WriteLn('Entering TSling4jFinder.Destroy');

  OwnerField := nil;
  inherited Destroy();
end;

procedure TSling4jFinder.Find();
var
  JavaDirList: TStringList;
  Filters: TJavaDirFilterSorterList;
  JavaDir: TJavaDir;
  s: string;
  Filter: TJavaDirFilterSorter;
  JavaDirTempList: TJavaDirList;
begin

  writeln('5532532');

  ////TODO
  //JavaDirList := OwnerField.SearchField.SearchDirListField;
  //Filters := OwnerField.FiltersField.FilterSortersField;
  //JavaDirTempList := TJavaDirList.Create;
  //
  //for s in JavaDirList do
  //begin
  //  JavaDir := TJavaDir.Create;
  //  JavaDir.InitFromDir(s);
  //  JavaDirTempList.Add(JavaDir);
  //end;
  //
  //JavaDirTempList.WriteLns('l');
  //
  //// filter
  //for Filter in Filters do
  //begin
  //  JavaDirTempList.RetainIf(Filter,True);
  //end;
  //
  //JavaDirTempList.WriteLns('ll');
  //
  //// sort
  //for Filter in Filters do
  //begin
  //  JavaDirTempList.Sort(Filter);
  //end;
  //
  //JavaDirTempList.WriteLns('lll');
  //
  //
  //JavaDirTempList.ClearAndFree();
  //FreeAndNil(JavaDirTempList);

end;

// ===== TSling4jFound =====

constructor TSling4jFound.Create(AOwner: TSling4jEngine);
begin
  WriteLn('Entering TSling4jFound.Create, ', GetMemBlockIdSize(Self));
  inherited Create();
  Self.OwnerField := AOwner;

end;

destructor TSling4jFound.Destroy;
begin
  WriteLn('Entering TSling4jFound.Destroy');

  OwnerField := nil;
  inherited Destroy();
end;

function TSling4jFound.Count(): integer;
begin
  Result := 0;
  //TODO
end;



initialization
  begin
    Sling := TSling4jEngine.Create();
  end;

finalization
  begin

    FreeAndNil(Sling);
  end;

end.

unit Sling4j.Engine;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Sling4j.Java.FilterSorters,
  Sling4j.Finder,
  Sling4j.Launcher,
  Sling4j.Interfaces,
  Sling4j.Java.Dirs;

type
  TSling4jEngine = class(ISling4jEngine)
  private
    FinderField: TSling4jFinder;
    LauncherField: TSling4jLauncher;
  public
    constructor Create();
    destructor Destroy(); override;
    function Finder(): ISling4jFinder; virtual;
    function Launcher(): ISling4jLauncher; virtual;
  end;




var
  Sling: TSling4jEngine;
  Finder: TSling4jFinder;
  Launcher: TSling4jLauncher;


implementation

uses
  LBCode.Windows.Utils,
  LBCode.Collections,
  LBCode.SystemUtils;

constructor TSling4jEngine.Create();
begin
  WriteLn('Entering ', Self.ClassName, '.Create, ', GetMemBlockIdSize(Self));
  inherited Create();

  FinderField := TSling4jFinder.Create(Self);
end;

destructor TSling4jEngine.Destroy();
begin
  WriteLn('Entering TSling4jEngine.Destroy');

  FreeAndNil(FinderField);

  inherited Destroy();
end;


function TSling4jEngine.Finder(): ISling4jFinder;
begin
  Result := Self.FinderField;
end;


function TSling4jEngine.Launcher(): ISling4jLauncher;
begin
  Result := Self.LauncherField;
end;


initialization
  begin
    Sling := TSling4jEngine.Create();
    Finder := Sling.FinderField;
    Launcher := Sling.LauncherField;
  end;

finalization
  begin
    Finder := nil;
    Launcher := nil;

    FreeAndNil(Sling);
  end;

end.

unit Sling4j.Launcher;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LBCode.SystemUtils,
  Sling4j.Interfaces;


type
  TSling4jLauncher = class(ISling4jLauncher)
  private
    OwnerField: ISling4jEngine;
  public
    constructor Create(const AOwner: ISling4jEngine);
    destructor Destroy(); override;
    function GetOwner(): ISling4jEngine; virtual;
  end;


implementation


constructor TSling4jLauncher.Create(const AOwner: ISling4jEngine);
begin
  WriteLn('Entering TSling4jLauncher.Create, ', GetMemBlockIdSize(Self));
  inherited Create();

  OwnerField := AOwner;

end;

destructor TSling4jLauncher.Destroy();
begin
  WriteLn('Entering TSling4jFinder.Destroy');

  OwnerField := nil;

  inherited Destroy();
end;


function TSling4jLauncher.GetOwner(): ISling4jEngine;
begin
  Result := OwnerField;
end;


end.

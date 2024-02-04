unit Sling4j.Interfaces;

{$mode ObjFPC}{$H+}
{$interfaces corba}


interface

uses
  Classes, SysUtils,
  Sling4j.Java.Dirs,
  Sling4j.Java.FilterSorters;

type
  ISling4jEngine = interface;
  ISling4jFinder = interface;
  ISling4jLauncher = interface;


type
  ISling4jEngine = interface
    function Finder(): ISling4jFinder; virtual;
  end;


type
  ISling4jFinder = interface
    function GetOwner(): ISling4jEngine; virtual;
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


type
  ISling4jLauncher = interface
    function GetOwner(): ISling4jEngine; virtual;
  end;


implementation

end.

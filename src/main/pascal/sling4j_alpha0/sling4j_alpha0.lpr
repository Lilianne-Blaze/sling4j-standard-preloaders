program sling4j_alpha0;

{$apptype console}
{$mode objfpc}{$H+}
{$interfaces corba}
{$unitpath ..\sling4j_package }
{$unitpath ..\lbcode_package }

uses
  LBCode.Collections,
  LBCode.Windows.Utils,
  LBCode.StringUtils,
  LBCode.SystemUtils,
  Sling4j.Java.Dirs,
  Sling4j.Java.FilterSorters,
  Sling4j.Java.Finder,
  Sling4j.Engine,
  Sling4j.Finder,
  Sling4j.Launcher,
  Sling4j.Environment;

  procedure Main();
  begin

    Finder.AddJavaHome();
    Finder.Dump();

    Finder.AddCommonVendors();
    Finder.Dump();



    //Finder.AddFilter(TNeedsJdkFilterSorter.Create());
    //Finder.AddFilter(TNeedsJavaFXFilterSorter.Create());
    //Finder.AddFilter(TMinMaxVersionFilterSorter.Create(17, 21));
    //Finder.AddFilter(TMinMaxVersionFilterSorter.Create(17, 21));
    //Finder.AddFilter(TPreferLTSFilterSorter.Create(False, False));
    //Finder.AddFilter(TNewestFirstFilterSorter.Create());

    Finder.AddFilter(TPreferLTSFilterSorter.Create(True, False));
    Finder.AddFilter(TNewestFirstFilterSorter.Create());


    Finder.Find();

    Finder.Dump();

  end;

begin
  Main();
  WaitForKeyPressOrTimeoutMsg(60000);
end.

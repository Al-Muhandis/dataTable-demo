unit staticfiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  ;

implementation

uses
  cmn, fpwebfile
  ;

initialization
  { It is necessary for the correct processing of server responses by your browser. }
  MimeTypesFile:=AppDir+'mime.types';
  {  The program will search for files that will be sent to requests in this folder }
  TSimpleFileModule.BaseDir:=AppDir+'html_public\';
  { All these files will be opened unless another one is specified as in the line above (tables.html ),
    that is, by default }
  TSimpleFileModule.RegisterDefaultRoute;
end.


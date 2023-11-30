unit cmn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

var
  AppDir: String;

type
  TOrderDir = (odNone, odAsc, odDesc);

function OrderToStr(aOrderDir: TOrderDir): String;

implementation

const
  _OrderDirStr: array[TOrderDir] of String = ('', 'asc', 'desc');

function OrderToStr(aOrderDir: TOrderDir): String;
begin
  Result:=_OrderDirStr[aOrderDir];
end;

initialization
  AppDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));

end.


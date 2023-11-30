unit TableProducer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, httproute
  ;

procedure RouteDataTablePage({%H-}ARequest: TRequest; AResponse: TResponse);
function BuildHTMLTable(const aTemplateFile: String; aWithBody: Boolean): String;

implementation

uses
  StrUtils, fpwebfile, getdatafromsql, Cmn
  ;

function BuildHTMLTable(const aTemplateFile: String; aWithBody: Boolean): String;
var
  aPage: TStringList;
begin
  { You can use the ARequest object to get request GET variables, string URL parameters, POST request fields, etc. }
  aPage:=TStringList.Create;
  try
    aPage.LoadFromFile(AppDir+aTemplateFile);
    { Yes, the usual replacement for the demonstration.
      More interesting and complex things are done using template engines like fpTemplate }
    Result:=ReplaceStr(aPage.Text, '%%TABLE%%', GetDataFromSQLTable(aWithBody));
  finally                                                                 
    aPage.Free;
  end;
end;

procedure RouteDataTablePage(ARequest: TRequest; AResponse: TResponse);
begin
  { HTML content of the page }
  AResponse.Content:=BuildHTMLTable('tables.html', True)
end;

initialization
  { Routing tables at http://127.0.0.1:8081/tables.html or others }
  httprouter.RegisterRoute('/tables.html', @RouteDataTablePage);

end.


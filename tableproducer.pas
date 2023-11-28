unit TableProducer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, httproute
  ;

type

  { TTableProducer }

  TTableProducer = class(TObject)
  private
    class procedure RouteDataTablePage(ARequest: TRequest; AResponse: TResponse); static;
    class function WriteTableBody: String; static;
  public

  end;

implementation

uses
  StrUtils, fpwebfile, getdatafromsql
  ;

var
  AppDir: String;

{ TTableProducer }

class procedure TTableProducer.RouteDataTablePage(ARequest: TRequest; AResponse: TResponse);
var
  aPage: TStringList;
begin
  { You can use the ARequest object to get request GET variables, string URL parameters, POST request fields, etc. }
  aPage:=TStringList.Create;
  aPage.LoadFromFile(AppDir+'tables.html');
  { Yes, the usual replacement for the demonstration.
    More interesting and complex things are done using template engines like fpTemplate}
  AResponse.Content:=ReplaceStr(aPage.Text, '%%TABLE_BODY%%', WriteTableBody);
  aPage.Free;
end;

class function TTableProducer.WriteTableBody: String;
begin
  Result:=GetDataFromSQLTable;
end;

initialization
  AppDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  { It is necessary for the correct processing of server responses by your browser. }
  MimeTypesFile:=AppDir+'mime.types';
  { Routing tables at http://127.0.0.1:8081/tables.html or others }
  httprouter.RegisterRoute('/tables.html', @TTableProducer.RouteDataTablePage);

  {  The program will search for files that will be sent to requests in this folder }
  TSimpleFileModule.BaseDir:=AppDir+'html_public\';
  { All these files will be opened unless another one is specified as in the line above (tables.html ),
    that is, by default }
  TSimpleFileModule.RegisterDefaultRoute;

end.


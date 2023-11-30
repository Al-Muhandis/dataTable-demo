unit EmptyTableProducer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  ;

implementation

uses
  getdatafromsql, httproute, HTTPDefs, TableProducer
  ;

{ TEmptyTableProducer }

procedure RouteDataTableWithAjaxPage({%H-}ARequest: TRequest; AResponse: TResponse);
begin
  { HTML template file. Create HTML page without table rows }
  AResponse.Content:=BuildHTMLTable('tpl_with-ajax.html', False); // False - without table body. Ajax will do it
end;

initialization
  { Routing tables at http://127.0.0.1:8081/tables-with-ajax.html }
  httprouter.RegisterRoute('/table-ajax.html', @RouteDataTableWithAjaxPage);

end.


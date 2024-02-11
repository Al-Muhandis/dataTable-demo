unit build_table_ajax;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, SQLDB, Cmn
  ;

type

  { TAjaxProducerTable }

  TAjaxProducerTable = class
  private
    FDraw: Integer;
    FLength: LongInt;
    FOrderColumn: LongInt;
    FOrderDir: TOrderDir;
    FSearchValue: String;
    FStart: LongInt;
  public
    class procedure RouteDataTablePage(aRequest: TRequest; aResponse: TResponse); static;
    procedure BuildResponse(aResponse: TResponse);
    procedure ParseRequest(aRequest: TRequest);
  end;

  { TAjaxProducerCellEdit }

  TAjaxProducerCellEdit = class
  private
    FColumn: LongInt;
    FID: LongInt;
    FValue: String;
  public
    class procedure RouteDataTablePage(aRequest: TRequest; aResponse: TResponse); static;
    procedure BuildResponse(aResponse: TResponse);
    procedure SaveField(aID: Integer; aCol: Integer; const aValue: String);
    procedure ParseRequest(aRequest: TRequest);
  end;

implementation

uses
  fpjson, getdatafromsql, httproute
  ;

{ TAjaxProducerTable }
{ Procedures for processing the relevant request }
class procedure TAjaxProducerTable.RouteDataTablePage(aRequest: TRequest; aResponse: TResponse);
var
  aAjaxProducer: TAjaxProducerTable;
begin
  aAjaxProducer:=TAjaxProducerTable.Create;
  with aAjaxProducer do
  begin
    ParseRequest(aRequest);
    BuildResponse(aResponse);
    Free;
  end;
end;

 { build JSON response for ajax request with certain structure
    https://datatables.net/manual/server-side }
procedure TAjaxProducerTable.BuildResponse(aResponse: TResponse);
var
  aJSON: TJSONObject;
  aData, aRecord: TJSONArray;
  aSQLQuery: TSQLQuery;
  i, l: Integer;
begin
  AResponse.ContentType:='application/json';
  aJSON:=TJSONObject.Create();
  try
    aJSON.Add('draw', FDraw);
    aSQLQuery:=CreateSQLQuery(FLength, FStart, FOrderDir, FOrderColumn, FSearchValue);
    try
      aData:=TJSONArray.Create();
      aJSON.Add('data', aData);
      l:=0;
      {  Why, in addition to aSQLQuery.EOF, do I control the loop to the maximum of the package?
        The fact is that I specifically create one more record to determine if there are more pages to request
        I guess you can come up with other mechanisms, but I did it. }
      while (not aSQLQuery.EOF) and (l<>FLength) do
      begin
        aRecord:=TJSONArray.Create;
        for i:=0 to ReadColCount-1 do
          aRecord.Add(aSQLQuery.Fields[i].AsString);
        aData.Add(aRecord);
        aSQLQuery.Next; 
        Inc(l);
      end;
      if not aSQLQuery.EOF then   // if this then record count is more than in the json data array of records
        Inc(l);
      aJSON.Add('recordsTotal', l+FStart); // You can specify total records oof the data if it is known
      aJSON.Add('recordsFiltered', l+FStart);
    finally
      aSQLQuery.Free;
    end;
    aResponse.Content:=aJSON.AsJSON;
  finally
    aJSON.Free;
  end;
end;

procedure TAjaxProducerTable.ParseRequest(aRequest: TRequest);
var
  aDir: String;
begin
  { Ajax passes the request parameters via GET parameters of the URI
    ( like sample.com/sampleuri ?getparameter1=value1&getparameter2 etc)
    http://127.0.0.1/ajax.json?draw=1&length=10&... etc }
  FDraw:=StrToIntDef(aRequest.QueryFields.Values['draw'], 0);
  FLength:=StrToIntDef(aRequest.QueryFields.Values['length'], 0);
  FSearchValue:=aRequest.QueryFields.Values['search[value]'];
  FStart:=StrToIntDef(aRequest.QueryFields.Values['start'], 0);
  FOrderColumn:=StrToIntDef(aRequest.QueryFields.Values['order[0][column]'], 0);
  aDir:=aRequest.QueryFields.Values['order[0][dir]'];
  case aDir of
    'asc':  FOrderDir:=odAsc;
    'desc': FOrderDir:=odDesc;
  else
    FOrderDir:=odNone;
  end;
end;

{ TAjaxProducerCellEdit }

class procedure TAjaxProducerCellEdit.RouteDataTablePage(aRequest: TRequest; aResponse: TResponse);
var
  aAjaxProducer: TAjaxProducerCellEdit;
begin
  aAjaxProducer:=TAjaxProducerCellEdit.Create;
  with aAjaxProducer do
  begin
    ParseRequest(aRequest);
    BuildResponse(aResponse);
    Free;
  end;
end;

procedure TAjaxProducerCellEdit.BuildResponse(aResponse: TResponse);
begin
  SaveField(FID, FColumn, FValue);
  aResponse.Content:='OK';
end;

procedure TAjaxProducerCellEdit.SaveField(aID: Integer; aCol: Integer; const aValue: String);
begin
  SaveFieldToSQLQuery(aID, aCol, aValue);
end;

procedure TAjaxProducerCellEdit.ParseRequest(aRequest: TRequest);
begin
  { Ajax CellEdit framework (see js/dataTables.cellEdit.js) via
    the myCallbackFunction function (see js/datatables-ajax.js) passes the request parameters via GET
    parameters of the URI ( like sample.com/sampleuri ?getparameter1=value1&getparameter2 etc)
    http://127.0.0.1/ajax.json/celledit?id=12&cellvalue=sample&... etc }
  FID:=StrToIntDef(aRequest.ContentFields.Values['id'], 0);
  FValue:=aRequest.ContentFields.Values['cellvalue'];
  FColumn:=StrToIntDef(aRequest.ContentFields.Values['column'], 0);
end;

initialization
  { register address for a url routing }        
  httprouter.RegisterRoute('/ajax.json/celledit/', TRouteMethod.rmPost, @TAjaxProducerCellEdit.RouteDataTablePage);
  httprouter.RegisterRoute('/ajax.json/tableread/', @TAjaxProducerTable.RouteDataTablePage);

end.


unit build_table_ajax;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, SQLDB, Cmn
  ;

type

  { TAjaxProducer }

  TAjaxProducer = class
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

implementation

uses
  fpjson, getdatafromsql, httproute
  ;

{ TAjaxProducer }

class procedure TAjaxProducer.RouteDataTablePage(aRequest: TRequest; aResponse: TResponse);
var
  aAjaxProducer: TAjaxProducer;
begin
  aAjaxProducer:=TAjaxProducer.Create;
  with aAjaxProducer do
  begin
    ParseRequest(aRequest);
    BuildResponse(aResponse);
    Free;
  end;
end;

procedure TAjaxProducer.BuildResponse(aResponse: TResponse);
var
  aJSON: TJSONObject;
  aData, aRecord: TJSONArray;
  aSQLQuery: TSQLQuery;
  i: Integer;
begin
  AResponse.ContentType:='application/json';
  aJSON:=TJSONObject.Create();
  try
    aJSON.Add('draw', FDraw);
    aSQLQuery:=CreateSQLQuery(FStart, FLength, FOrderDir, FOrderColumn, FSearchValue);
    try
      aJSON.Add('recordsTotal', 0); // You can specify total records oof the data if it is known
      aJSON.Add('recordsFiltered', aSQLQuery.RecordCount);
      aData:=TJSONArray.Create();
      aJSON.Add('data', aData);
      while not aSQLQuery.EOF do
      begin
        aRecord:=TJSONArray.Create;
        for i:=0 to ReadColCount-1 do
          aRecord.Add(aSQLQuery.Fields[i].AsString);
        aData.Add(aRecord);
        aSQLQuery.Next;
      end;
    finally
      aSQLQuery.Free;
    end;
    aResponse.Content:=aJSON.AsJSON;
  finally
    aJSON.Free;
  end;
end;

procedure TAjaxProducer.ParseRequest(aRequest: TRequest);
var
  aDir: String;
begin
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

initialization
  httprouter.RegisterRoute('/ajax.json/', @TAjaxProducer.RouteDataTablePage);

end.


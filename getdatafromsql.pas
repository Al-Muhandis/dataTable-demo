unit getdatafromsql;

{ The unit is dedicated to separate the functionality responsible for receiving data from FireBird.
    It can be another database engine, for example, a fast Sqlite, it can be just data in the program memory
    or data from a plain text file. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, cmn
  ;

function GetDataFromSQLTable(aWithBody: Boolean = True): String;
function CreateSQLQuery(aLength, aStart: Integer; aOrderDir: TOrderDir = odNone; aOrderCol: Integer = 0;
  aSearchValue: String = ''): TSQLQuery;
function ReadColCount: Integer;

implementation

uses
  IBConnection, IniFiles
  ;

var
  _Connection : TIBConnection;
  _Transaction: TSQLTransaction;
  _Ini: TMemIniFile;

function CreateConnection: TIBConnection;
begin
  result := TIBConnection.Create(nil);
                                 { This I think you understand even better than me ;) }
  result.Hostname :=      _Ini.ReadString('DB', 'Host', 'localhost');
  result.DatabaseName :=  _Ini.ReadString('DB', 'Name', EmptyStr);
  result.UserName :=      _Ini.ReadString('DB', 'User', 'sysdba');
  result.Password :=      _Ini.ReadString('DB', 'Password', 'masterkey');
end;

{ Read table params from INI file }
function ReadColName(aIndex: Integer): String;
begin
  Result:=_Ini.ReadString('Table', 'Col'+aIndex.ToString, EmptyStr);
end;
function ReadColCount: Integer;
begin
  Result:=_Ini.ReadInteger('Table', 'ColCount', 0);
end;
function ReadTableName: String;
begin
  Result:=_Ini.ReadString('Table', 'Name', EmptyStr);
end;

{ Read column inndex for searching if the search value is presented }
function ReadSearchCol: Integer;
begin
  Result:=_Ini.ReadInteger('Table', 'SearchCol', 0);
end;

function BuildQuery(aLength: Integer = 0; aStart: Integer = 0; aOrderDir: TOrderDir = odNone;
  aOrderCol: Integer = 0; aSearchValue: String = ''): String;
var
  i, aCount: Integer;
begin
  Result:='select ';
  if aLength<>0 then
    Result+='first '+(aLength+2).ToString+' ';
  if aStart<>0 then
    Result+='skip '+aStart.ToString+' ';
  aCount:=ReadColCount;
  for i:=0 to aCount-1 do
    Result+=' '+ReadColName(i)+', ';
  if aCount>0 then
    SetLength(Result, Length(Result)-Length(', '));
  Result+=' from '+ReadTableName;
  if not aSearchValue.IsEmpty then
  begin
    Result+=' where '+ReadColName(ReadSearchCol)+' like ''%'+aSearchValue+'%''';
  end;
  if aOrderDir<>odNone then
    Result+=' order by '+ReadColName(aOrderCol)+' '+OrderToStr(aOrderDir);
end;

{ Building an SQL query for the query and Open }
procedure OpenQuery(aQuery: TSQLQuery; aLength: Integer = 0; aStart:  Integer = 0; aOrderDir: TOrderDir = odNone;
  aOrderCol: Integer = 0; aSearchValue: String = '');
begin
  aQuery.Database := _Connection;
  aQuery.SQL.Text := BuildQuery(aLength, aStart, aOrderDir, aOrderCol, aSearchValue);
  aQuery.Open;
  aQuery.First;
end;

{ for ajax data retrieving we need only table header in HTML }
function GetDataFromSQLTable(aWithBody: Boolean): String;
var
  aTableBody: String;
  aIni: TMemIniFile;
  i: Integer;
  aSQLQuery: TSQLQuery;

  { Creating the contents of a specific cell }
  function GetFieldForTableCell(const aField: String): String;
  begin
    Result:='<td>'+aSQLQuery.FieldByName(aField).AsString+'</td>'+LineEnding;
  end;

begin                                        
  aIni:=TMemIniFile.Create(ChangeFileExt(ApplicationName, '.ini'));  
  aSQLQuery := TSQLQuery.Create(nil);
  try
    OpenQuery(aSQLQuery);
    { Build table header and footer (HTML syntax) }
    Result:='<thead>'+LineEnding+'<tr>'+LineEnding;
    for i:=0 to ReadColCount-1 do
      Result+='<th>'+ReadColName(i)+'</th>'+LineEnding;
    Result+='</tr>'+LineEnding+'</thead>'+LineEnding;
    Result+='<tfoot>'+LineEnding+'<tr>'+LineEnding;
    for i:=0 to ReadColCount-1 do
      Result+='<th>'+ReadColName(i)+'</th>'+LineEnding;
    Result+='</tr>'+LineEnding+'</tfoot>'+LineEnding;
    { Build table body }
    if aWithBody then
    begin
      aTableBody:='<tbody>'+LineEnding;
      { Each iteration is a separate record in the database and a row in the table }
      while not aSQLQuery.EOF do
      begin
        aTableBody+='<tr>'+LineEnding;
        for i:=0 to ReadColCount-1 do
          aTableBody+=GetFieldForTableCell(ReadColName(i));
        aTableBody+='</tr>'+LineEnding;
        aSQLQuery.Next;
      end;
      aTableBody+='</tbody>'+LineEnding;
    end;
  finally
    aSQLQuery.Close;
    aSQLQuery.Free;
    aIni.Free;
  end;
  Result+=aTableBody;
end;

{ THis function creates and open the dataset query by SQL }
function CreateSQLQuery(aLength, aStart: Integer; aOrderDir: TOrderDir; aOrderCol: Integer; aSearchValue: String
  ): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  OpenQuery(Result, aLength, aStart, aOrderDir, aOrderCol, aSearchValue);
end;

initialization
  { We can do it globally because the server is not threaded }
  _Ini:=TMemIniFile.Create(ChangeFileExt(ApplicationName, '.ini'));
  _Connection := CreateConnection;
  _Transaction:=TSQLTransaction.Create(nil);
  _Connection.Transaction:=_Transaction;
  _Connection.Open;

finalization
  _Ini.Free;
  _Connection.Close;
  _Transaction.Free;
  _Connection.Free;

end.


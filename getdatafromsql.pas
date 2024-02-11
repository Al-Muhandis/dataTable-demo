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
procedure SaveFieldToSQLQuery(aID: Integer; aCol: Integer; const aValue: String);

implementation

uses
  IBConnection, IniFiles, SQLite3Conn
  ;

var
  _Connection : TSQLConnection;
  _Transaction: TSQLTransaction;
  _Ini: TMemIniFile;

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
function ReadDBDriver: String;
begin
  Result:=_Ini.ReadString('DB', 'Driver', 'IB');
end;

function CreateConnection: TSQLConnection;
begin
  case ReadDBDriver of
    'IB':      Result := TIBConnection.Create(nil);
    'SQLite3': Result := TSQLite3Connection.Create(nil);
  else
    raise ESQLDatabaseError.Create('Unsupported database driver');
  end;
                                 { This I think you understand even better than me ;) }
  result.HostName :=      _Ini.ReadString('DB', 'Host', 'localhost');
  result.DatabaseName :=  _Ini.ReadString('DB', 'Name', EmptyStr);
  result.UserName :=      _Ini.ReadString('DB', 'User', 'sysdba');
  result.Password :=      _Ini.ReadString('DB', 'Password', 'masterkey');
end;

function BuildQuery(aLength: Integer = 0; aStart: Integer = 0; aOrderDir: TOrderDir = odNone;
  aOrderCol: Integer = 0; aSearchValue: String = ''): String;
var
  i, aCount: Integer;
  aDBDriver: String;
begin
  aDBDriver:=ReadDBDriver;
  Result:='select ';
  if aDBDriver='IB' then
  begin
    if aLength<>0 then
      Result+='first '+(aLength+2).ToString+' ';
    if aStart<>0 then
      Result+='skip '+aStart.ToString+' ';
  end;
  aCount:=ReadColCount;
  for i:=0 to aCount-1 do
    Result+=' '+ReadColName(i)+', ';
  if aCount>0 then
    SetLength(Result, Length(Result)-Length(', '));
  Result+=' from '+ReadTableName;
  if not aSearchValue.IsEmpty then
    Result+=' where '+ReadColName(ReadSearchCol)+' like ''%'+aSearchValue+'%''';
  if aOrderDir<>odNone then
    Result+=' order by '+ReadColName(aOrderCol)+' '+OrderToStr(aOrderDir);
  if aDBDriver='SQLite3' then
  begin
    if aLength<>0 then
      Result+=' limit '+(aLength+2).ToString+' ';
    if aStart<>0 then
      Result+=' offset '+aStart.ToString+' ';
  end;
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

procedure SaveFieldQuery(aQuery: TSQLQuery; aID: Integer; aCol: Integer; const aValue: String);
begin
  OpenQuery(aQuery);
  if not aQuery.Locate('id', aID{%H-}, []) then
    Exit;
  aQuery.Edit;
  aQuery.Fields[aCol].AsString:=aValue;
  aQuery.Post;
end;

procedure SaveFieldToSQLQuery(aID: Integer; aCol: Integer; const aValue: String);
var
  aQuery: TSQLQuery;
begin
  aQuery := TSQLQuery.Create(nil);
  aQuery.Options:=aQuery.Options+[sqoAutoApplyUpdates, sqoAutoCommit];
  try
    SaveFieldQuery(aQuery, aID, aCol, aValue);
  finally                                     
    aQuery.Free;
  end;
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


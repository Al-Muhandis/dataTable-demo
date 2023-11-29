unit getdatafromsql;

{ The unit is dedicated to separate the functionality responsible for receiving data from FireBird.
    It can be another database engine, for example, a fast Sqlite, it can be just data in the program memory
    or data from a plain text file. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB
  ;

function GetDataFromSQLTable: String;

implementation

uses
  IBConnection, IniFiles
  ;

var
  _Connection : TIBConnection;
  _Transaction: TSQLTransaction;

function CreateConnection: TIBConnection;
var
  aIni: TMemIniFile;
begin
  result := TIBConnection.Create(nil);
  aIni:=TMemIniFile.Create(ChangeFileExt(ApplicationName, '.ini'));
  try                                   { This I think you understand even better than me ;) }
    result.Hostname :=      aIni.ReadString('DB', 'Host', 'localhost');
    result.DatabaseName :=  aIni.ReadString('DB', 'Name', EmptyStr);
    result.UserName :=      aIni.ReadString('DB', 'User', 'sysdba');
    result.Password :=      aIni.ReadString('DB', 'Password', 'masterkey');
  finally                          
    aIni.Free;
  end;
end;


function GetDataFromSQLTable: String;
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
  function ReadColName(aIndex: Integer): String;
  begin
    Result:=aIni.ReadString('Table', 'Col'+aIndex.ToString, EmptyStr);
  end;
  function ReadColCount: Integer;
  begin
    Result:=aIni.ReadInteger('Table', 'ColCount', 0);
  end;
  function ReadTableName: String;
  begin
    Result:=aIni.ReadString('Table', 'Name', EmptyStr);
  end;
  function OpenQuery: String;
  var
    i: Integer;
  begin
    aSQLQuery.Database := _Connection;
    Result:='select ';
    for i:=0 to ReadColCount-1 do
      Result+=' '+ReadColName(i)+', ';
    SetLength(Result, Length(Result)-Length(', '));
    Result+=' from '+ReadTableName;
    aSQLQuery.SQL.Text := Result;
    aSQLQuery.Open;
    aSQLQuery.First;
  end;

begin                                        
  aIni:=TMemIniFile.Create(ChangeFileExt(ApplicationName, '.ini'));  
  aSQLQuery := TSQLQuery.Create(nil);
  try
    OpenQuery;
    Result:='<thead>'+LineEnding+'<tr>'+LineEnding;
    for i:=0 to ReadColCount-1 do
      Result+='<th>'+ReadColName(i)+'</th>'+LineEnding;
    Result+='</tr>'+LineEnding+'</thead>'+LineEnding;
    Result+='<tfoot>'+LineEnding+'<tr>'+LineEnding;
    for i:=0 to ReadColCount-1 do
      Result+='<th>'+ReadColName(i)+'</th>'+LineEnding;
    Result+='</tr>'+LineEnding+'</tfoot>'+LineEnding;
    Result+='<tbody>'+LineEnding;
    aTableBody:=EmptyStr;
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
  finally
    aSQLQuery.Close;
    aSQLQuery.Free;
    aIni.Free;
  end;
  Result+=aTableBody;
end;

initialization
  _Connection := CreateConnection;
  _Transaction:=TSQLTransaction.Create(nil);
  _Connection.Transaction:=_Transaction;
  _Connection.Open;

finalization
  _Connection.Close;
  _Transaction.Free;
  _Connection.Free;

end.


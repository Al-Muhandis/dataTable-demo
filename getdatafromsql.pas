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
  _Query: TSQLQuery;


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
  aRows: String;
                   { Creating the contents of a specific cell }
  function GetFieldForTableCell(const aField: String): String;
  begin
    Result:='<td>'+_Query.FieldByName(aField).AsString+'</td>'+LineEnding;
  end;

begin
  _Query.Open;
  aRows:=EmptyStr;
  _Query.First;
  { Each iteration is a separate record in the database and a row in the table }
  while not _Query.EOF do
  begin
    aRows+='<tr>'+LineEnding;
    aRows+=GetFieldForTableCell('ID_EMP');
    aRows+=GetFieldForTableCell('NAME');  
    aRows+=GetFieldForTableCell('SEX');
    aRows+=GetFieldForTableCell('BIRTH');  
    aRows+=GetFieldForTableCell('CITY');  
    aRows+=GetFieldForTableCell('NAME1');
    aRows+='</tr>'+LineEnding;
    _Query.Next;
  end;
  Result:=aRows;
end;


initialization
  _Connection := CreateConnection;
  _Transaction:=TSQLTransaction.Create(nil);
  _Connection.Transaction:=_Transaction;
  _Query := TSQLQuery.Create(nil);
  _Query.SQL.Text := 'select ID_EMP, NAME, SEX, BIRTH, CITY, NAME1 from TB_EMP';
  _Query.Database := _Connection;

finalization
  _Query.Close;
  _Connection.Close;
  _Query.Free;
  _Transaction.Free;
  _Connection.Free;

end.


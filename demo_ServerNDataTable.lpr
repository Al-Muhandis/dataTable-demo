program demo_ServerNDataTable;

{$mode objfpc}{$H+}

uses
  fpwebfile,
  fphttpapp, TableProducer, getdatafromsql, LCLIntf
  ;

begin

  Application.Title:='httpproject1';
  { The standard HTTP server port is 8080 and in this case it is not necessary to specify the port in the site address,
    but since this port on the computer/server may be busy, we will choose, for example, 8081 }
  Application.Port:=8081;
  Application.Threaded:=True;
  Application.Initialize;
  { This is optional. Just automatically launches by the URL you need }
  OpenURL('http://127.0.0.1:8081/tables.html');
  Application.Run;
end.


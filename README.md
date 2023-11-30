# dataTable-demo
A simple almost minimal example of how you can work with a jQuery table from the Lazarus program. DataSource from firebird database

# demo.ini
Just create `demo.ini` and write settings in it
```ini
[DB]
;; DB host, for example: localhost, 127.0.0.1, sample.com
host=localhost
;; DB name. For TSQLConnection.DatabaseName
name=C:\Users\user\Some_DB.fdb
user=sysdba
password=masterkey

[Table]
Name=TABLE_NAME
col0=COL_0
col1=COL_1
col2=COL_2
col3=COL_3
col4=COL_4
col5=COL_5
ColCount=6
SearchCol=1
```

# Frontend
The template is used as a frontend framework `sb-admin` https://github.com/startbootstrap/startbootstrap-sb-admin/ 
 It can be used for that any other

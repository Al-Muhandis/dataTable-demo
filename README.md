# dataTable-demo
A simple almost minimal example of how you can work with a jQuery table from the Lazarus program. DataSource is from firebird or SQLite3 database

# demo.ini
Just create `demo.ini` and write settings in it
```ini
[DB]
;; DB host, for example: localhost, 127.0.0.1, sample.com
host=localhost
;; DB name. For TSQLConnection.DatabaseName
name=.\sample.sqlite3
user=sysdba
password=masterkey
driver=SQLite3

[Table]
Name=employee
col0=id
col1=Empid
col2=EmpName
col3=EmpJoiningDate
col4=Salary
col5=Address
ColCount=6
SearchCol=2
```

# Frontend
The template is used as a frontend framework `sb-admin` https://github.com/startbootstrap/startbootstrap-sb-admin/ 
 It can be used for that any other

# demo video

[![Watch the video](https://img.youtube.com/vi/RD104mPfl20/hqdefault.jpg)](https://www.youtube.com/embed/RD104mPfl20)
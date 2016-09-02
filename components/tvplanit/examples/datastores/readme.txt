The folder "datastores" collects some demo projects to show usage of the
various datastores that come along with TvPlanIt:

Datastores working with the default Lazarus installation
--------------------------------------------------------

- TVpBufDsDatastore in folder "bufdataset" (for TBufDataset)

- TVpFirebirdDatastore in folder "fb" (for Firebird databases using SQLDB)

- TVpFlexDatastore in folder "flex" (two projects, one for Access, one for 
  sqlite3 database)
  
- TVpIniDatastore in folder "ini" (for ini files)

- TVpSqlite3Datastore in folder "sqlite3" (for SQLite3 database using SQLDB)


Add-on datastores requiring third-party components/code:
--------------------------------------------------------

- TVpZeosDatastore in folder "zeos" 
  requires installation of package "laz_visualplanit_zeos"
  
- TVpmORMotDatastore in folder "examples/mormot":
  Since this datastore depends on third-party source code it is not added in 
  its own package. Just look at the sample code in folder "examples/mormot".

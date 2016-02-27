Database Settings
===

The database setting  is important.
The whole website is based on the database.

## PostgreSQL

This framework, Glob is based on PostgreSQL, a great open-source database.

Wherever you host your site, you need a database.
And for some docker-ships, integrated services about database are needed.

And we need initialize the database before we launch our site, thought without database the program can still launch, because of "call by need" - lazy evaluation.

And there we uesd PostgreSQL 9.x
9.1 and 9.4.x were tested.

## Initialization

To initialize database, there are some SQL files.

* table.sql creates all table used by site
* inner.sql creates all datas.
* function.sql creates the functions needed.

## Management

To management database, psql, pgAdmin, and other tools and applications can manage database.
At the same time, Glob has the APIs to do this.

### PgAmdin
  
PgAdmin can manage database directly.
But sometime we need manage database via Glob.
 
### Glob's API for Management Database

`/management/*` can be used to management database.

* `/management/html`

We can update or insert a row via this API.
And we need 

And from 0.0.5.60, we can raw SQL file by hostname:port/management/sql via POST.

* `/management/sql`

Upload and run sql file via POST.
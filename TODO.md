The Plan To Do
===

This document is about the todo list of Glob, which is including the fixings of bug, to do list and so on.

# TODO list

## Unmarked

* Add the tags to blogs, pages, and so on.
* To support more kinds of database, including SQLite and MySQL.
* To support Microsoft SQL Server, Oracle.
* To support some NoSQL databases.
* Add the handler of searching blogs, and pages.
  tags, created date, updated date.
* Trying to make glob worked well with Nginx.
* Trying to make glob worked well with PostgreSQL-XC
* Add more launchers.

## For 0.0.8.0

* Rebuild the structure of project.

## For 0.0.7.60

* The first we need to do is change some part of the launcher of docker.
* Split Glob(Data) and main Module and launcher.

  The we used now (0.0.7.40) is not strong for a launcher. We need add
  some support of GHC's runtime, and change it's reaction of with

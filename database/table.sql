




-- database/table.sql

-- Run ' SELECT drop_all_table();' first when those table existed.

CREATE TABLE table_nav
(
  key_label TEXT NOT NULL PRIMARY KEY,
  key_order INT ,
  key_ref TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_html
(
  key_index TEXT NOT NULL PRIMARY KEY,
  key_html TEXT NOT NULL,
  key_title TEXT NOT NULL,
  key_content TEXT NOT NULL,
  key_summary TEXT,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_txt
(
  key_index TEXT NOT NULL PRIMARY KEY,
  key_text Text NOT NULL,
  key_content Text NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_bin
(
  key_index TEXT NOT NULL PRIMARY KEY,
  key_binary BYTEA NOT NULL,
  key_content TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_query
(
  key_index TEXT NOT NULL PRIMARY KEY,
  key_text TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

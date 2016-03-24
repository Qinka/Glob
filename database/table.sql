




-- database/table.sql

-- Run ' SELECT drop_all_table();' first when those table existed.

CREATE TABLE table_nav
(
  id SERIAL PRIMARY KEY,
  key_label TEXT NOT NULL,
  key_order INT ,
  key_ref TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_html
(
  id SERIAL PRIMARY KEY,
  key_index TEXT NOT NULL,
  key_html TEXT NOT NULL,
  key_title TEXT NOT NULL,
  key_content TEXT NOT NULL,
  key_summary TEXT,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_txt
(
  id SERIAL PRIMARY KEY,
  key_index TEXT NOT NULL,
  key_text Text NOT NULL,
  key_content Text NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_bin
(
  id SERIAL PRIMARY KEY,
  key_index TEXT NOT NULL,
  key_binary BYTEA NOT NULL,
  key_content TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE table_query
(
  id SERIAL PRIMARY KEY,
  key_index TEXT NOT NULL,
  key_text TEXT NOT NULL,
  key_update_time TIMESTAMP WITH TIME ZONE NOT NULL,
  key_create_time TIMESTAMP WITH TIME ZONE NOT NULL
);

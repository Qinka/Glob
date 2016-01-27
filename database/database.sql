




-- database/database.sql

CREATE OR REPLACE FUNCTION
  drop_all_table(user_name IN VARCHAR,schema_name IN VARCHAR)
  RETURNS VOID
  AS $$
  DECLARE statements CURSOR FOR
    SELECT tablename FROM pg_tables
    WHERE tableowner = user_name AND
          schemaname = schema_name;
    BEGIN
      FOR stmt IN statements LOOP
        EXECUTE 'DROP TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';
      END LOOP;
    END;
$$ LANGUAGE plpgsql;

SELECT drop_all_table('qinka','public');

CREATE TABLE table_nav
(
  texts TEXT NOT NULL PRIMARY KEY,
  ordering INT ,
  refto TEXT NOT NULL
);

CREATE TABLE table_pages
(
  indexs TEXT NOT NULL PRIMARY KEY,
  tos TEXT NOT NULL,
  times DATE NOT NULL,
  title TEXT NOT NULL
);

CREATE TABLE table_blogs
(
  indexs TEXT NOT NULL PRIMARY KEY,
  tos TEXT NOT NULL,
  times DATE NOT NULL,
  title TEXT NOT NULL
);

CREATE TABLE table_htmls
(
  indexs TEXT NOT NULL PRIMARY KEY,
  html TEXT NOT NULL,
  title TEXT NOT NULL
);

CREATE TABLE table_csss
(
  indexs TEXT NOT NULL PRIMARY KEY,
  css TEXT NOT NULL
);

CREATE TABLE table_jss
(
  indexs TEXT NOT NULL PRIMARY KEY,
  js TEXT NOT NULL
);

--

-- 添加 html 文件

INSERT INTO table_htmls VALUES
(
  '@#page.main','<h1> Hello,world </h1> Hello<p>This is Qinka''s blog. </p>','Home'
);

INSERT INTO table_htmls VALUES
(
  '@#page.frame.top','<h1>Qinka''s Blog</h1>',''
);

INSERT INTO table_htmls VALUES
(
  '@#page.frame.copyright','Copyright (C) Qinka',''
);

-- 添加 css 文件
INSERT INTO table_csss VALUES
(
  'css.frame.css','body{background:#282828;color:#ededed}'
);

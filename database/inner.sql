




-- database/inner.sql

-- At less, run table.sql first.


-- Not suppost some version.

-- Use Qinka-Blog ./upload.sh or gnu-make(Makefile) instead.

-- 添加 html 文件

INSERT INTO table_html VALUES
(
  '$page.main','<h1> Hello,world </h1> Hello<p>This is Qinka''s blog. </p>','Home','home','2016-01-01'
);

INSERT INTO table_html VALUES
(
  '$page.frame.top','<h1>Qinka''s Blog</h1>','','home','2016-01-01'
);

INSERT INTO table_html VALUES
(
  '$page.frame.bottom','<p>Copyright (C) Qinka</p><p>假设这里有广告</p>','','home','2016-01-01'
);

INSERT INTO table_html VALUES
(
  '$page.frame.nav','<nav class="navclass"><ul><li><a href="/" class="navclass"> Home</a></li><li><a href="/blog"> Blog</a></li></ul></nav>','','home','2016-01-01'
);

-- 添加 css 文件
INSERT INTO table_txt VALUES
(
  'css.frame.css','body{background:#282828;color:#ededed}','text/css'
);

-- 添加 Nav 导航
INSERT INTO table_nav VALUES
(
  'Home',0,'/'
);
INSERT INTO table_nav VALUES
(
  'Blog',0,'/blog'
);

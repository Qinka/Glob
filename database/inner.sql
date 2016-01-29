-- 先执行 database.sql


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

INSERT INTO table_htmls VALUES
(
  '@#page.frame.nav','<nav class="navclass"><ul><li><a href="/" class="navclass"> Home</a></li><li><a href="/blog"> Blog</a></li></ul></nav>',''
);

-- 添加 css 文件
INSERT INTO table_csss VALUES
(
  'css.frame.css','body{background:#282828;color:#ededed}'
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

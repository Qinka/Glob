Glob
===

# APIs

## Navigation

Request `/nav` (POST) can get some JSON datas, about navigation.

Example:
```json
[{"time":"2016-02-16T11:44:19.533015Z","order":0,"label":"Home","ref":"/"},{"time":"2016-02-16T11:44:20.254827Z","order":1,"label":"Blog","ref":"/blog"},{"time":"2016-02-16T11:44:20.418748Z","order":2,"label":"Glob","ref":"/page/glob"}]
```

## List of Blog

Request `/blog` (POST) can get some JSON datas, about blog.

Example:
```json
[{"time":"2016-02-10 06:21:41.234 UTC","title":"My First Blog","index":"myfirstBlog"},{"time":"2016-02-10 06:21:41.234 UTC","title":"My First Blog-1","index":"myfirstBlog1"},{"time":"2016-02-10 06:21:41.234 UTC","title":"My First Blog","index":"myfirstBlog2"},{"time":"2016-02-10 06:21:41.234 UTC","title":"My First Blog","index":"myfirstBlog3"},{"time":"2016-02-10 06:21:41.234 UTC","title":"My First Blog","index":"myfirstBlog4"}]
```

# Data

## [TABLE] table_html

### $page.main

`$page.main` is the main page's html, and it's type should be "home".

### $page.frame.top

`$page.frame.top` is the top part of the hole site, and it's type should be "home".

### $page.frame.bottom

`$page.frame.bottom` is the bottom part of the hole site, and it's type should be "home".
According Agasa-License, the words, just like "Power by Glob" should be there.

### $page.frame.nav

`$page.frame.nav` is the navigation of site. It's type should be "home".
And the API might will help you.

### OTHERs

* **key_index** is the index of a html.
* **key_html** is the body of a html.
* **key_title** is the title of a html (but might not display).
* **key_content** is the type of a html (home, blog, and page).
* **key_time** is the date and time that the html upload (or update).

## [TABLE] table_txt

### css.frame.css

This file is the basic css file of site.

### OTHERs

* **key_index** is the index of a textual file.
* **key_text** is the text of the file.
* **key_content** is the MIME type of a file.
* **key_time** is the date and  time of upload (or update).

## [TABLE] table_nav

### OTHERs

* **key_label** is the label of a link.
* **key_order** is the order of the link.
* **key_ref** is the url a link's href.
* **key_time** is the time and date that a link update or upload.

## [TABLE] table_bin

### OTHERs

* **key_index** is the index of a binary file.
* **key_bin** is the text of the file.
* **key_content** is the MIME type of a file.
* **key_time** is the date and  time of upload (or update).

## [TABLE] table_query

### OTHERs

* **key_index** is the index of a row.
* **key_text** is the text of a row.
* **key_time** is the time and date when it chanaged
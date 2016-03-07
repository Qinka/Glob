# The change log of Glob

## [VERSION] 0.0.6.82

增加了 博客列表翻页的功能，从提供JSON 的技术角度上。

## [VERSION] 0.0.6.75

添加了 Html 部分的摘要部分。

## [VERSION] 0.0.6.71

试图添加评论功能，目前只添加了文章的id。

## [VERSION] 0.0.6.70

1. 更新了认证的密码验证算法。消除小概率事件。

## [VERSION] 0.0.6.67

1. 添加了执行 SQL 的一些方法

## [VESION] 0.0.6.60

1. beta-2
2. 忘了

## [VERSION] 0.0.6.50

1. 添加了 直接获取静态内容的功能

## [VERSION] 0.0.6.40

1. 修正了某一个地方的小错误 -- 更新二进制文件的内容是，忽略了检查文件的 MIME 类型。
1. 变更了 url 的某些方式。

## [VERSION] 0.0.6.10

1. 修正了上传文件中会使得部分内容丢失的问题。

## [VERSION] 0.0.6.0

1. 0.1 的 alpha 测试。
1. 无重要调整。
1. 添加获取服务器时间的功能。
1. 编译通过，未测试。

## [VERSION] 0.0.5.60

1. 添加了 进行 sql 查询的管理内容，可以直接执行 sql 的查询。

## [VERSION] 0.0.5.50

1. 更换了秘钥机制，加强秘钥破解难度。

## [VERSION] 0.0.5.40

1. 改变了时间，使用 UTC time，取代 Day 与 TimeOfDay

## [VERSION] 0.0.5.23

1. 重新调整了全部的结构
1. 添加了 TemplateHaskell 以简化代码
1. 将部分页面在数据库中的前缀由 @# 转变为 $

## [VERSION] 0.0.3.56

1. 更新了秘钥认证的方法，使得秘钥传输在加密。

## [VERSION] 0.0.3.24

1. 添加了 后台删除与浏览 的接口。

## [VERSION] 0.0.3.11

1. 制作了 管理后台用的接口，可以使用这个接口上传内容，修改内容。

## [VERSION] 0.0.2.10

1. 添加了 “ 查询功能 ”， 可以查询到版本号及其他的内容。

## [VERSION] 0.0.2.1

1. 调整了 nav 返回的 json 中数据的顺序。

## [VERSION] 0.0.2.0

1. 将前后端基本分开。
1. 改变了数据库结构，使得 js 与 css 等
文本型的资源归到了一个数据库中。
同时对于二进制文件也添加了一个数据库。
1. 同时 Blog 与 Page 等也直接合并到一个数据库中。
1. 改变了 Dockerfile 与 数据库 配置文件。

## [VERSION] 0.0.1.70

1. Add Change log to Glob
1. Add 6 versions of Dockerfile of Glob
1. Used gitter
1. Used Code Climate
1. Used Travis-CI
1. Used waffle

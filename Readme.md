Glob
===
A blog backend.

---

## Snapshot

Glob is a small application, which I use to host my blog. It is wrote in Haskell with Yesod framework.
Glob store the datas with Mongodb. And I split the frontend and backend thoroughly. And Glob nearly do nothing about viewing.

Usually I host Glob on Docker, with CaaS provider - [DaoCloud](https://www.daocloud.io). 
And the resource of container is small, but free, with 128Mb memary and a common-used cpu.
And it suits for personal blog.

And to be honest, it is not a good blog backend, for many reason, including hard to maintain.
Because, now, there are not any tool that can make maintain easier. If you want to upload,
here, you need gnu-make and another things.

Glob is just the snake oil.

## Usage

Unless you build binary yourself, I just provide binary in docker image form. 
Pull from [DockerHub](https://hub.docker.com) [qinka/glob](https://hub.docker.com/r/qinka/glob/).

The tag of image include glob's infos: version, supposed-hosted plateform, git commit hash, operating system, OS release version,
compiler, compiler version, compiler arch, enabled thread of not, and using llvm or not.

### Windows Docker

Because of MS-Windows, I, now, am hard to build Windows docker with CI. I will try my best to build image manually.

### Start
You need a mongodb database, and then see the man page of glob-launch(1).

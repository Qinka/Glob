The Plan To Do
===

This document is about the to do list of Glob, which is including the fixings of bug, to do list and so on.

# TODO list

## For 0.0.7.60

* The first we need to do is change some part of the launcher of docker.

  The we used now (0.0.7.40) is not strong for a launcher. We need add
  some support of GHC's runtime, and change it's reaction of without-tls
  version.

  Sometimes when images are hosted on multi-cpu pc, when need use the following:
  ```
  glob --file=config.json +RTS -Nx -RTS
  ```
  And launcher of docker (it is same with simple-launch) does not support.

  And if you used a version without tls, you still need to set the settings of
  the files' path about tls.This is bad.

* The second we need to do is add a handler to delete to static files.

* The next thing to do is add with-tls version of glob's docker images.


# BUG FIX

About Source
===

This part is about something around source.

# GET Source

You can get GLob's sources via git.

* `https://github.com/qinka/Glob`

And at the same times, an example used this framework is holded on github.

# Get READY 

Whatever you do with these source (unless you just have a look (laughing `mix` crying)),
you need two things: GHC and Cabal.

GHC and Cabal you can visit [Haskell's Official Site](http://www.haskell.org) for more informations.

* Glob < 0.1 tested with GHC-7.10.3 (Haskell Platform)

# Configure Source

It is easy to configure these sources.

## Install Dependent Package

Install the following package via Cabal:

* base (4.8~4.9)
* bytestring 
* cmdargs
* aeson
* text
* persistent-postgresql
* persistent
* persistent-template
* shakespeare
* blaze-markup
* blaze-html
* time 
* monad-logger
* yesod-core
* warp
* warp-tls
* template-haskell
* process
* directory (> 1.2.4)
* conduit
* SHA

And we have to give those who will install these package some tips.

1. base is usually installed when you install ghc.
1. You can install each package first and last.
1. Some packages in this list are dependent another packages in this list.
   And here when build docker image, we usually run 
   ```
   cabal install directory process persistent-template yesod persistent-postgresql warp-tls cmdargs
   ```
1. When installing persistent-postgresql, we need install PostgreSQL first.
   And make sure that pgconfig's path is available in enviroment-value PATH.
1. There is a bug (befor 0.0.6.1). 
   When we do not need tls/ssl, we still have to install warp-tl.
  
## Configure

The one thing to do there is run `cabal configure`.
And if launch.* is needed, flag(-flaunch-docker or -flaunch-simple) is necessary.
And if tls/ssl is needed, flag(-fwith-tls) is necessary.

And the flag -flaunch-docker is for Docker.And the usage is [here]().
   
# Build Glob

After "configure", build Glob is very easy. Just one thing:
```
cabal build
```
And binarys are available in dist/build .
And you can copy these binarys to wherever.

# Install

To be honst, `cabal install` is available after `cabal configure`.
And for more information, [CLICK HERE](https://www.haskell.org/cabal).

Install-command will copy binarys to ~/.cabal/bin or %AppData%/cabal/bin (and so on) .

# About Sandbox

It might be easy to use sandbox. 
However you might have drunk cups of coffee, but build and install is unfinished.

Tips:

1. When use sandbox, after install dependent-package, you can run `cabal install`
   directly.
2. Installing yesod is a long run. 




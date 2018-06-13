## Resilient McMaster

[Google Drive Shared Folder](https://drive.google.com/drive/folders/0B5etXBcQqB6SZG56QjZPR2pic2c)

## Getting Started

We will assume that the Elm compiler, ghc and cabal are already installed.

Type in 
```
$ which cabal
$ cabal --version
```
on MacOS it should say /usr/local/bin/cabal and cabal-install version 2.0.0.1 (or better).
I had to delete whole bunch of .bash_* files in my home directory apparently installed by Anaconda! to the the right cabal.
```
$ cabal update
$ cabal sandbox init
$ cabal install --only-dependencies
```
If you have a conflict, try downloading a new version of Haskell Platform.  If you don't have a conflict, be prepared to wait a while.

Now build and run the server
```
$ cabal build
$ ./dist/build/
```

Now you should be able to go to
```
http://127.0.0.1:8080/x/Student1/pass1/1
```
in a browser and see question 1.  Some other questions may also work.

## Development Standards

* [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) as a prettifier.
* [hlint](https://hackage.haskell.org/package/hlint) for linting.

```
$ hlint Chat.hs
```

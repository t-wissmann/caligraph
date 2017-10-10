# Caligraph

A frontend to [remind](https://www.roaringpenguin.com/products/remind) calendar program.

## Compilation
You need Haskell's `stack` tool to be installed. In order to build, type:
```
$ stack build
```
In order to run, type:
```
$ stack exec caligraph
```

## Workaround on Arch Linux, October 2017
The above build command will install ghc-tinfo6-nopie-8.0.2 to the home instead
of using the system ghc. If you want to use the ghc from the repos you will
need wrapper libraries for libtinfo from the aur:

https://github.com/commercialhaskell/stack/issues/3448#issuecomment-332440389


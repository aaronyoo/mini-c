# Minicc

A compiler that supports a subset of the C language.

## Build Instructions

Most dependencies are handled by opam. The notable exception is LLVM which needs
to be installed by the system before it can be used. LLVM-10 needs to be
installed on the system. In order to get it on MacOS using homebrew:

```
brew install pkg-config llvm
```

# ulrvm

Ulrvm is a virtual machine modified from [nga](https://github.com/crcx/nga) with a compiler. Its goal is to provide runtime recompilation features for embedded systems.

# Install

1. Install the compiler:

```
$ cd compiler
$ cabal install
```

2. Add the vm source and header files to your embedded project.
See the example folder for integration examples.

# Usage

1. Compile the vm source code in your project 
```
$ ulrvmc source.cvm
```

2. Compile and run your project 

3. Use the compiler to dynamically replace code while the program is running.
For instance, if using a serial connection to the running system:
```
ulrvmc -s /dev/ttyACM0 9600 system.def.h recompile_source.cvm
```

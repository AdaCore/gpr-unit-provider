GPR Unit Provider
======================================

This library provides a unit provider for Libadalang based on
GPR project analysis library.

INSTALLING THIS LIBRARY
-----------------------

The GPR_Unit_Provider library depends on some other external libraries:

* [GPR](https://github.com/AdaCore/gpr)

* [Libadalang](https://github.com/AdaCore/libadalang)

Both must be installed on the system to be able to compile GPR_Unit_Provider.

BUILDING LibGPR2
----------------

To build all versions of the library (static, relocatable and
static-pic) plus the associated tools use the provided Makefile:

```sh
$ make
```

Then, to install it:

```sh
$ make install
```


BUG REPORTS
-----------

If you got this source file from GNATtracker, please send questions and bug
reports to report@gnat.com following the same procedures used to submit reports
with the GNAT toolset itself.

If you read this from the [GitHub repository](https://github.com/AdaCore/gpr-unit-provider),
please [open issues](https://github.com/AdaCore/gpr-unit-provider/issues) to send questions
and bug reports.

.. default-role:: code

* Type constraints in the `ext` constructor
* `unpack` should unpack as many object as possible (use `values` to return
  them?)
* Where to install the Guile library to?
* Properly handle EOF when unpacking
* Clean up error throwing (things like `throw` and `error`)
* Stop the tests from vomiting log files all over the repo
* A makefile argument that produces a tarball for distribution (save under
  `$(PREFIX)/dist/guile-msgpack.tar.gz` or something like that)

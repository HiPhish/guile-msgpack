.. default-role:: code

##############
 Installation
##############

The following describes all that is needed for building the MessagePack
library, as well as the accompanying documentation. The information is
sufficient to build everything manually or define a package for a package
manager. Deviations of the build process from this file are considered bugs and
shall be reported.

Use `make` to build the library and the documentation. All dependencies listed
below are build-time dependencies. The following targets are defined (`all` is
the default target):

=======  ============  ========================================================
Name     Dependencies  Description
=======  ============  ========================================================
`all`    see below     equivalent to `lib doc`
`lib`    Guile         The Guile library
`doc`    Texinfo       Documentation in Info format
`pdf`    Texinfo, TeX  Documentation in PDF format
`html`   Texinfo       Documentation in HTML format
`check`  Guile         Run tests
`clean`                Delete build results
=======  ============  ========================================================

The following variables can be passed to the makefile for customisation (in
addition to the standard makefile variables):

==========  ==========  =======================================================
Name        Default     Description
==========  ==========  =======================================================
`PREFIX`    `build`     Build destination
`GUILE`     `guile`     Guile binary for running tests
`TEXI2ANY`  `texi2any`  Texinfo binary for building documentation
==========  ==========  =======================================================

If you only want to try out the repository in-place you can also add it to the
Guile's load path:

.. code::

   $ guile -L .

   scheme@(guile-user)> ,use (msgpack)
   scheme@(guile-user)> (pack "hello" 1337 #t)
   $1 = #vu8(165 104 101 108 108 111 205 5 57 195)
   scheme@(guile-user)>

To install the library system-wide you could execute the following:

.. code:: sh

   # Build the library and Info documentation
   make all PREFIX=/usr/local


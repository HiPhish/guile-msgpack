.. default-role:: code

###########################
 MessagePack for GNU Guile
###########################

This is a GNU Guile library implementing the MessagePack data serialisation
format. It allows you to serialise (pack) and de-serialise (unpack) Scheme
objects to and from binary data according to the MessagePack format.


Installation
############

Use `make` to build the library and the documentation. The following targets
are defined (`all` is the default target):

=======  ============  ========================================================
Name     Dependencies  Description
=======  ============  ========================================================
`all`    see below     equivalent to `lib doc`
`lib`                  The Guile library
`doc`    Texinfo       Documentation in Info format
`pdf`    Texinfo, TeX  Documentation in PDF format
`html`   Texinfo       Documentation in HTML format
`check`  Guile         Run tests
`clean`                Delete build results
=======  ============  ========================================================

The following variables can be passed to the makefile for customisation (in
addition to the standard makefile variables):

================  ==========  =================================================
Name              Default     Description
================  ==========  =================================================
`PREFIX`          `build`     Build destination
`GUILE`           `guile`     Guile binary for running tests
`GUILE_VERSION`   `2.2`       Version of guile as `major.minor`
`TEXI2ANY`        `texi2any`  Texinfo binary for building documentation
================  ==========  =================================================

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


Using MessagePack
#################

First import the module, then you can pack an object to a binary input port or
unpack one from a binary output port.

.. code:: scheme

   (use-modules ((msgpack) #:select (pack unpack pack! unpack!)))

   ;; Some object to pack
   (define hodgepodge (vector 1 2 '#(3 #t) "foo"))

   (pack hodgepodge)
   ;; Returns #vu8(#x94 #x01 #x02 #x92 #x03 #xC3 #xA3 #x66 #x6F #x6F)

   (unpack #vu8(#x94 #x01 #x02 #x92 #x03 #xC3 #xA3 #x66 #x6F #x6F))
   ;; Returns '#(1 2 #(3 #t) "foo")

   ;; We can also pack and unpack directly to and from a port
   (pack! hodgepodge (current-output-port))
   (unpack! (current-input-port))

In the above code we have two variants of `pack` and `unpack`: a pure one and
an impure one with an exclamation mark suffix. The pure variant performs the
packing or unpacking without any side effects by returning the packed bytes or the
unpacked object. The impure variant on the other hand writes to or reads from a
port as a side effect, but does not return anything.


License
#######

Released under the GPLv3+ license, see the COPYING_ file for details.

.. _MessagePack: http://msgpack.org/
.. _COPYING: COPYING.txt


Self-promotion
##############

If you like this library please consider financially supporting its
development, every small amount helps. Feel free to explore my other software
projects as well.

http://hiphish.github.io/
https://liberapay.com/HiPhish/

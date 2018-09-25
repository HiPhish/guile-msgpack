.. default-role:: code

###########################
 MessagePack for GNU Guile
###########################

This is a GNU Guile library implementing the MessagePack data serialisation
format. It allows you to serialise (pack) and de-serialise (unpack) Scheme
objects to and from binary data according to the MessagePack format.


Installation
############

Use the included makefile to build the library, documentation and run tests.
Here is a quick run-down, please refer to the INSTALL_ file for full details.

.. code:: sh

   # Run tests, install to /usr/local, build PDF and HTML documentation
   make check && make lib doc PREFIX=/usr/local && make pdf html


Using MessagePack
#################

First import the module, then you can pack an object to a binary input port or
unpack one from a binary output port.

.. code:: scheme

   (use-modules ((msgpack) #:select (pack unpack pack-to unpack-from)))

   ;; Some object to pack
   (define hodgepodge (vector 1 2 '#(3 #t) "foo"))

   (pack hodgepodge)
   ;; Returns #vu8(#x94 #x01 #x02 #x92 #x03 #xC3 #xA3 #x66 #x6F #x6F)

   (unpack #vu8(#x94 #x01 #x02 #x92 #x03 #xC3 #xA3 #x66 #x6F #x6F))
   ;; Returns '#(1 2 #(3 #t) "foo")

   ;; We can also pack and unpack directly to and from a port
   (pack-to (current-output-port) hodgepodge)
   (unpack-from (current-input-port))

In the above code we have two variants of `pack` and `unpack`: a pure one and
an impure one with a `to`- or `from` suffix. The pure variant performs the
packing or unpacking without any side effects by returning the packed bytes or
the unpacked object. The impure variant on the other hand writes to or reads
from a port as a side effect, but does not return anything.


Status of the project
#####################

The project is still fresh, but it is complete and stable as far as I am
concerned. I will wait a little while for it to set before committing to a
proper release though. User experience reports are always welcome.


License
#######

Released under the GPLv3+ license, see the COPYING_ file for details.

.. _MessagePack: http://msgpack.org/
.. _INSTALL: INSTALL.rst
.. _COPYING: COPYING.txt


Self-promotion
##############

If you like this library please consider financially supporting its
development, every small amount helps. Feel free to explore my other software
projects as well.

* http://hiphish.github.io/

* https://liberapay.com/HiPhish/

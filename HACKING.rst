###############################
 Hacking MessagePack for Guile
###############################

The final authority is *always* the MessagePack specification. If the
implementation differs from the specification please add first a test case and
then fix it.


Testing
=======

We use a small DSL for specifying test cases, use it for readability, unless
the set of test cases is so small that it's not worth the effort.


Packing
=======

There are more types in Guile than in MessagePack and it is not always clear
how to map the former onto the latter. Instead of hard-coding the association,
there is an association list which uses predicates as keys and packing
procedures as values:

.. code:: scheme

   (define packing-table
     (list (cons foo? pack-foo)
           (cons bar? pack-bar))))

In reality this table is a parameter so that users can add their own entries.
If you want to add support for a new packing procedure you have to add an
entry to the packing table. The value is a procedure which takes the object to
pack and the output port as arguments:

.. code:: scheme

   (define (pack-foo foo out)
     (put-bytevector out #vu8(#x00 #x00)))


Unpacking
=========

This is fairly straight-forward. The only point of interest is the extension
type: MessagePack can specify the meaning of extensions with negative `type`
tag values. By default any extension object is unpacked to an extension object,
but we should consider unpacking them to concrete Scheme objects if the
standard gives a meaning to a particular tag.

For example, the type tag :math:`-1` is used for timestamp objects, so we could
unpack those to some appropriate Scheme object.

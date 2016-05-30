# Analytical Engine Emulator

This is an Ada translation of the Java emulator at
[Fourmilab](http://www.fourmilab.ch/babbage/).

In due course, it is hoped to create a web-based emulator (using
[Gnoga](http://www.gnoga.com)). In the mean time, there is a command
line based version.

## Copying

The Java source of the Fourmilab emulator is included in this
repository for reference (in the <tt>java/</tt> subdirectory); the
Analytical Engine code's author states _Emulator source code is
intended for experienced Java developers, and is utterly
unsupported. The program is in the public domain and you can do
anything you like with it, but you're entirely on your own._, while
<tt>BigInt.java</tt> is copyrighted by the Massachusetts Institute of
Technology.

The Ada source is licensed under the
[GNU General Public License (version 3)][] with the
[GCC Runtime Library exception][].

[GNU General Public License (version 3)]: http://www.gnu.org/licenses/gpl.html
[GCC Runtime Library exception]: http://www.gnu.org/licenses/gcc-exception-faq.html

## Building

The Ada emulator is written using [Ada2012][], and uses the
[GNU Multiple Precision Arithmetic library][], via the
[GMP binding in GNATCOLL][] (note, the actual binding used was that in
AdaCore's GPL 2015 version, for reasons).

If you have the [FSF GCC 6.1.0 binary for OS X][] a suitable library
is provided. GMP and GNATCOLL (at least the 2015 version) are
reasonably straightforward to build.

[Ada2012]: http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html

[GNU Multiple Precision Arithmetic library]: https://gmplib.org

[GMP binding in GNATCOLL]: https://github.com/AdaCore/gnatcoll/blob/master/src/gmp/gnatcoll-gmp-integers.ads

[FSF GCC 6.1.0 binary for OS X]: https://sourceforge.net/projects/gnuada/files/GNAT_GCC%20Mac%20OS%20X/6.1.0/

## Programming Cards

The implementation here is as in the
[Fourmilab Java implementation][]. To date only the card types up to
and including Comment Cards have been implemented.

As a minor change, lower case can be used: <tt>n001 42</tt> is acceptable.

Multiplication and division can only be indicated by the <tt>*</tt>
and <tt>/</tt> symbols (× and ÷ aren't supported).

[Fourmilab Java implementation]: https://www.fourmilab.ch/babbage/cards.html

# Analytical Engine Emulator

This is an Ada translation of the Java emulator at
[Fourmilab](http://www.fourmilab.ch/babbage/).

In due course, it is hoped to create a web-based emulator (using
[Gnoga](http://www.gnoga.com)). In the mean time, there is a command
line based version.

## Copying

The Java source of the Fourmilab emulator is included in this
repository for reference (in the `fourmilab/` subdirectory); the
Analytical Engine code's author states _Emulator source code is
intended for experienced Java developers, and is utterly
unsupported. The program is in the public domain and you can do
anything you like with it, but you're entirely on your own._, while
`BigInt.java` is copyrighted by the Massachusetts Institute of
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

The design here is as in the [Fourmilab Java implementation][]. To
date only the card types below have been implemented:

  * Number Cards
  * Operation Cards
  * Variable Cards
  * Stepping Up and Down Cards
  * Combinatorial Cards
  * Action Cards
  * Comment Cards

These card types have not been implemented (yet):

  * Curve Drawing Cards
  * Attendant Request Cards (but Calculation Trace Cards _are_ in)
    * Advancing and Backing Block Cards
    * Card Library Inclusion Requests
    * Decimal Place Expansion Cards
    * Numeric Output Format Cards
    * Output Annotation Cards

As a minor change, lower case can be used: `n001 42` is
acceptable (it stores 42 into column 1).

Multiplication and division can only be indicated by the `*`
and `/` symbols (× and ÷ aren't supported).

[Fourmilab Java implementation]: https://www.fourmilab.ch/babbage/cards.html

## Examples

The file `bernouilli.ae` is an implementation of the Lovelace design
in Note G of the [Sketch of the Analytical Engine][] (the last
section), with two errors corrected:

  * in operation 4, the operands are reversed (it should be V4 / V5,
    not V5 / V4).
  * in operation 24, the result placed in V24 should be **-**V13.

AAL's notes don't discuss the fact that the Engine only implemented
integer arithmetic, which means that a naive attempt at dividing 7 by
9 at operation 4 will result in 0 remainder 7, not the 0.777777777 one
might have hoped for. To deal with this, all real values are scaled by
10 decimal places;

  * the precalculated value of B1, 0.1666666666, is stored as
    1666666666, and similarly for B3, B5.
  * dividends are scaled up by 10 decimal places.
  * products are scaled down by 10 decimal places.

For more on this, see the notes on _Stepping Up and Down Cards_ in the
[Fourmilab Java implementation][].

The file `bernouilli5.ae` adds another "iteration" to the above
program; the previously computed B7 is stored on column 24, and
operations 13 to 23 are repeated once more (now using column 24 as
input at operation 21).

[Sketch of the Analytical Engine]: https://www.fourmilab.ch/babbage/sketch.html

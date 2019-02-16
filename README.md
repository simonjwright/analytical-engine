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
[GMP binding in GNATCOLL][].

If you have one of the [FSF GCC binaries for OS X][] (6.1 or later), a
suitable library is provided. Otherwise, GMP and GNATCOLL are
reasonably straightforward to build.

[Ada2012]: http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html

[GNU Multiple Precision Arithmetic library]: https://gmplib.org

[GMP binding in GNATCOLL]: https://github.com/AdaCore/gnatcoll/blob/master/src/gmp/gnatcoll-gmp-integers.ads

[FSF GCC binaries for OS X]: https://sourceforge.net/projects/gnuada/files/GNAT_GCC%20Mac%20OS%20X/

## Running

There are three switches:

  * `-h` or `--help`: outputs help.
  * `-t` or `--trace`: trace execution (like card T1)
  * `-z` or `--overwrite-nonzero`: allow storing to a non-zero column

and, as usual, run by

    ./aes [switches] [card-chain-file]

(uses standard input if no card chain file is supplied).

The "storing to a non-zero column" issue is referenced in [this paper][]
by Rainer Glaschick, section 3.5, Memory peculiarities. It would have
been necessary to ensure that a column that was to be written to was
zero beforehand; the operation of storing a value involved racks
transferring the value on the source to the destination as the source
digits were rotated back to zeros.

If `-z` is given, overwriting will be allowed; otherwise, an error
will be reported and execution will halt.

[this paper]: http://rclab.de/rclab/_media/analyticalengine/aal_noteg_glaschick_v1.2.pdf

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
  * Attendant Request Cards:
    * Calculation Trace Cards
    * Numeric Output Format Cards
    * Output Annotation Cards

These card types have not been implemented (yet):

  * Curve Drawing Cards
  * Attendant Request Cards:
    * Advancing and Backing Block Cards
    * Card Library Inclusion Requests
    * Decimal Place Expansion Cards

As in the [Fourmilab Java implementation][], multiplication can be
indicated by `*` or `×`, division by `/` or `÷`.

[Fourmilab Java implementation]: https://www.fourmilab.ch/babbage/cards.html

When counting for Combinatorial Cards (conditional and unconditional
jumps), remember that comment cards (cards starting with a period or
white space) need to be included!

### Changes

  * As a minor change, lower case can be used: `n001 42` is acceptable
  (it stores 42 into column 1).
  * Text after the required content is ignored, so comments can be included: `n001 42 the answer` is acceptable.

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

[Sketch of the Analytical Engine]: https://www.fourmilab.ch/babbage/sketch.html

The file `bernouilli5.ae` adds another "iteration" to the above
program; the previously computed B7 is stored on column 24, and
operations 13 to 23 are repeated once more (now using column 24 as
input at operation 21).

The file `check_for_prime.ae` determines whether a number is
prime. It's set to check 203 (AAL's 203rd birthday was in 2018).

## Performance

The [Deep Learning with the Analytical Engine][] project, with the
[Deep Learning with the Analytical Engine repository][], reports on an

> implementation of a convolutional neural network as a program for
> Charles Babbage's Analytical Engine, capable of recognising
> handwritten digits to a high degree of accuracy (98.5% if provided
> with a sufficient amount of training data and left running
> sufficiently long).

The program consists of over 400,000 cards! (it was generated by
Python scripts from a high-level description). It requires `-z` to
run. As a timing test, the program was run for the first 10 images
only: in Java, this took 18 seconds, of which 10 seconds was initial
processing, in Ada (where the initial processing was about a second)
with `-O0 -gnata` 44 seconds, with `-O2 -gnatp` 22 seconds. So the Ada
implementation is about 3 times slower than the Java one. More work to
be done!

[Deep Learning with the Analytical Engine]: https://cp4space.wordpress.com/2016/02/06/deep-learning-with-the-analytical-engine/
[Deep Learning with the Analytical Engine repository]: https://gitlab.com/apgoucher/DLAE

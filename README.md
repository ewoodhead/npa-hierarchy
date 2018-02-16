# npa-hierarchy

A small library that grew out of some code for generating the NPA-hierarchy relaxations for a class of problems I was working with. It can handle problems consisting of the maximisation or minimisation of the quantum expectation value of a linear combination of projection operators (i.e., a Bell operator) subject to zero or more equality constraints on the expectation values of such operators. The number of parties, inputs, and outputs is arbitrary. It works with the [SDPA family](http://sdpa.sourceforge.net/) of semidefinite programming solvers.

It is written in and for Common Lisp (currently, specifically the SBCL implementation), but you don't necessarily have to know Lisp in order to use it. It tries to use notation similar to what one would use in a research paper. For example the code you need to type to maximise CHSH at level 1 + AB of the hierarchy is just
```
(solve-problem
 (maximise A1 (B1 + B2) + A2 (B1 - B2))
 (level 1 + A B))
```
Assuming everything you need is properly installed and loaded, running this expression will
- detemine the SDP relaxation of the problem at the indicated level,
- write the corresponding input file for SDPA,
- call SDPA on it,
- extract the solution, and
- return the primal and dual solutions and an SDPA status indicator (like "PDOPT" or "PDFEAS").

It is inferred from the symbols appearing in the problem (`A1`, `A2`, `B1`, and `B2`) that there are in this case two parties each with two binary-outcome measurements.

Much of the whitespace is mandatory:
  - You generally need space around the arithmetic operators like `+` and `-`. The reason for this is that, unlike most programming languages, these are valid characters in variable names. `B1+B2` is a perfectly valid five-character variable name as far as Lisp is concerned.
  - Use a space to separate operators that are being multiplied. `A1 B1` is the product of `A1` and `B1`. `A1B1` is an unrelated variable with a four-character name that Lisp will expect to be assigned to somewhere.
  - In levels like "1 + A B", you also need spaces separating the different parties. `A B` refers to parties 0 and 1. `AB` instead refers to party 27.

Internally the library uses the Collins-Gisin projection. Dichotomic operators are converted into an equivalent operator . For example, entering `A1` is essentially treated the same as `(2 A1/1 - Id)`.

## Setup

### Install SDPA and SBCL

If you want to use this library, you will need SDPA installed in your `$PATH` (i.e., SDPA should run if you enter `sdpa` at the prompt in a terminal window). You will also need the SBCL Lisp implementation. If you are using Ubuntu Linux (or I assume Debian or any of its derivatives) then both are available via the package manager, e.g.,
```
$ sudo aptitude install sdpa sbcl rlwrap
```
On other Linux distributions and Unix-like systems you may need to [download SDPA](http://sdpa.sourceforge.net/download.html) and compile it from source yourself.

### Install Quicklisp and npa-hierarchy

Quicklisp is a package manager for Lisp that does dependency resolution (like Pip for Python). Download [Quicklisp](https://www.quicklisp.org/) and install it, following the instructions on their website. This basically means typing the following into a terminal (`$` and `*` are the shell and Lisp prompts):
```
$ wget https://beta.quicklisp.org/quicklisp.lisp
$ rlwrap sbcl --noinform --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```
Among other things, this should create a `quicklisp/` directory in your home directory. Download npa-hierarchy into the `local-projects/` subdirectory:
```
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/ewoodhead/npa-hierarchy
```
At this point you should be able to use Quicklisp to load the npa-hierarchy library and use it. If you test it (start Lisp again with `rlwrap sbcl` if you stopped it) a simple test session maximising CHSH should look something like this:
```
$ rlwrap sbcl
This is SBCL 1.3.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:quickload :npa-hierarchy)
To load "npa-hierarchy":
  Load 1 ASDF system:
    npa-hierarchy
; Loading "npa-hierarchy"

(:NPA-HIERARCHY)
* (in-package :npa-user)

* (solve-problem
   (maximise A1 (B1 + B2) + A2 (B1 - B2))
   (level 6))

2.8284264071600953
2.828427218447403
PDFEAS
*
```

### Configure Emacs (optional)

It is possible to start an interactive Lisp session from the terminal (as in the example above, `rlwrap sbcl`). However, you can get a much nicer environment by running Lisp from Emacs via the SLIME mode.

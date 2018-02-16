# npa-hierarchy

A small Lisp library that grew out of some code for generating the NPA-hierarchy relaxations for a class of problems I was looking at. I am releasing it under the MIT license because this is what GitHub says to do if you don't want to think about licenses too much.

The library can handle problems consisting of the maximisation or minimisation of the quantum expectation value of a linear combination of projection operators (i.e., a Bell operator) subject to zero or more equality constraints on the expectation values of such operators. The number of parties, inputs, and outputs is arbitrary. It works with the [SDPA family](http://sdpa.sourceforge.net/) of semidefinite programming solvers.

It is written in and for Common Lisp (currently, specifically the SBCL implementation), but you don't necessarily have to know Lisp in order to use it. It tries to use notation similar to what one would use in a research paper. For example, the code you need to type to maximise CHSH at level 1 + A B of the hierarchy is just
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

Projectors can be written like `A1\|1` or `A1/1`. (The identity can be written `Id` but it can usually be omitted: numbers are usually treated as themselves multiplied by the identity.) For example, you can maximise the CH74 form of CHSH with
```
(solve-problem
 (maximise A1/1 B1/1 + A1/1 B1/2 + A1/2 B1/1 - A1/2 B1/2
           - A1/1 - B1/1)
 (level 1))
```
Internally, the library uses the Collins-Gisin projection and you can express problems with any number of inputs, outputs, and parties. Dichotomic operators are translated to 2 times the first projector minus the identity, e.g., `A1`  is treated the same as `(2 A1/1 - Id)` (NB: not `(A1/1 - A2/1)`, since in the Collins-Gisin projection this would be taken to mean there is an implicit third output).

## Setup

### Install SDPA and SBCL

If you want to use this library, you will need SDPA installed in your `$PATH` (i.e., SDPA should run if you enter `sdpa` at the prompt in a terminal window). You will also need the SBCL Lisp implementation. If you are using Ubuntu Linux (or I assume Debian or any of its derivatives) then both are available via the package manager, e.g.
```
$ sudo aptitude install sdpa sbcl rlwrap
```
On other Linux distributions and Unix-like systems you may need to [download SDPA](http://sdpa.sourceforge.net/download.html) and compile it from source yourself.

### Install Quicklisp and npa-hierarchy

Quicklisp is a package manager for Lisp that lets you download libraries off the internet and does dependency resolution (like Pip for Python). Download [Quicklisp](https://www.quicklisp.org/) and install it, following the instructions on their website. This basically means typing the following into a terminal (`$` and `*` are the shell and Lisp prompts):
```
$ wget https://beta.quicklisp.org/quicklisp.lisp
$ rlwrap sbcl --noinform --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```
and type `y` to confirm that you want to edit the init file when it asks. Among other things, this should create a `quicklisp/` directory in your home directory. Download npa-hierarchy into the `local-projects/` subdirectory:
```
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/ewoodhead/npa-hierarchy
```
At this point you should be able to use Quicklisp to load the npa-hierarchy library and use it. A simple test session maximising CHSH should look something like this:
```
$ rlwrap sbcl --noinform
* (ql:quickload :npa-hierarchy)
;; Info about packages being loaded snipped.
* (in-package :npa-user)

#<PACKAGE "NPA-USER">
* (solve-problem
   (maximise A1 (B1 + B2) + A2 (B1 - B2))
   (level 6))

2.828426407325086
2.8284272115854705
PDFEAS
*
```
Running`(in-package :npa-user)` puts you in a working package that imports the most important symbols from other packages in the library, so you don't need to prefix them with their package names (so e.g. you can type `solve-problem` instead of `npa-hierarchy:solve-problem`).

### Configure Emacs (optional)

It is possible to start an interactive Lisp session from the terminal (as illustrated above). However, you can get a much nicer environment by running Lisp from Emacs via the SLIME mode.

## Potential pitfalls

- Much of the whitespace is mandatory:
  - You generally need space around the arithmetic operators like `+` and `-`. The reason for this is that, unlike most programming languages, these are valid characters in variable names. `B1+B2` is a perfectly valid five-character variable name as far as Lisp is concerned.
  - Use a space to separate operators that are being multiplied. `A1 B1` is the product of `A1` and `B1`. `A1B1` is an unrelated variable with a four-character name that Lisp will expect is defined somewhere.
  - In levels like "1 + A B", you also need spaces separating the different parties. `A B` refers to parties 0 and 1. `AB` instead refers to party 27.
- Lisp has single- and double-precision floats. Unlike most other languages, decimal numbers like `1.0` are read as single precision by default. If this annoys you, you can change this by evaluating
  ```
  (setf *read-default-float-format* 'double-float)
  ```
  You can add this as a line in the file `.sbclrc` in your home directory if you want this done every time you start SBCL. Note that some functions (like `sin` and `cos`) will still return a single-precision result if you call them with an integer argument. Make sure to call these functions with a double-precision argument if you want a double-precision result. 

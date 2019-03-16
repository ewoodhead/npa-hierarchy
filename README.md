# npa-hierarchy

An implementation of the [NPA
hierarchy](https://doi.org/10.1088/1367-2630/10/7/073013) in Common Lisp.

This is a small library that grew out of some code for generating the
NPA-hierarchy relaxations for a class of problems I was studying, and got
carried away with.

The library can handle problems consisting of the maximisation or
minimisation of the quantum expectation value of a linear combination of
projection operators (i.e., a Bell operator) subject to zero or more equality
and inequality constraints on the expectation values of such operators. The
number of parties, inputs, and outputs is arbitrary. It works with the [SDPA
family](http://sdpa.sourceforge.net/) of semidefinite programming solvers.

It is written in Common Lisp (currently, specifically the SBCL
implementation), but you don't necessarily have to know Lisp in order to use
it. It supports a notation similar to what you might see in a research
paper. For example, the code you need to type to maximise CHSH at level 1 + A
B of the hierarchy is
```
(solve-problem
 (maximise A1 (B1 + B2) + A2 (B1 - B2))
 (level 1 + A B))
```
Assuming everything you need is properly installed and loaded, running this
expression will
- detemine the SDP relaxation of the problem at the indicated level,
- write the corresponding input file for SDPA,
- call SDPA on it,
- extract the solution, and
- return the primal and dual solutions and an SDPA status indicator (usually
  "PDOPT" or "PDFEAS", if everything went OK).

It is inferred from the symbols appearing in the problem (`A1`, `A2`, `B1`,
and `B2`) that there are in this case two parties each with two
binary-outcome measurements.

Some more examples of its use are given below.

There are other freely-available libraries that can work with the NPA
hierarchy. You may find one of these is more convenient, depending on what
you want to do and what programming environments you are already comfortable
with:
- The Matlab toolbox [QETLAB](http://www.qetlab.com/Main_Page). The functions
  [`NPAHierarchy`](http://www.qetlab.com/NPAHierarchy) and
  [`BellInequalityMax`](http://www.qetlab.com/BellInequalityMax) can handle
  problems in the bipartite scenario.
- Peter Wittek's Python library
  [Ncpol2sdpa](https://ncpol2sdpa.readthedocs.io/en/stable/) implements the
  more general NPA hierarchy as described in [Handbook on Semidefinite, Conic
  and Polynomial Optimization,
  pp. 601-634](https://doi.org/10.1007/978-1-4614-0769-0_21).



## Setup

The following installation instructions are quite detailed since I assume
most people who might consider using this library are not already familiar
with Lisp. On Unix-like operating systems, including Linux and Mac OS, the
short version is:
- You will need SDPA, SBCL, and [Quicklisp](https://www.quicklisp.org/beta/)
  installed.
- You install npa-hierarchy by putting the project directory somewhere where
  Quicklisp can find it, like ~/common-lisp/ or ~/quicklisp/local-projects/,
  and doing `(ql:quickload :npa-hierarchy)` to load it. This will pull in
  dependencies in the process.
- It is recommended to run Lisp in an environment that supports it, such as
  [SLIME](https://common-lisp.net/project/slime/). Inputting multi-line Lisp
  expressions in a plain terminal is not a fun experience.

If you're using Microsoft Windows, the following procedure seems to work:
- Download SDPA from [here](http://sdpa.sourceforge.net/download.html) and
  arrange for the sdpa.exe executable to be located in a folder in the
  `%PATH%` environment variable. SDPA should run if you type "sdpa" into a
  DOS prompt.
- Download and install the Windows version of
  [Portacle](https://portacle.github.io/).
- Download the npa-hierarchy library. Unzip it if necessary and copy or move
  the npa-hierarchy folder and its contents into Portacle's projects/
  subfolder. The exact location depends on where you installed Portacle.
- Start Portacle and run `(ql:quickload :npa-hierarchy)` at the Lisp prompt
  to install the dependencies.


### Install SDPA and SBCL

If you want to use this library, you will need SDPA installed in your `$PATH`
(i.e., SDPA should run if you enter `sdpa` at the prompt in a terminal
window). You will also need the SBCL Lisp implementation. If you are using
Ubuntu Linux (or I assume Debian or any of its derivatives) then both are
available via the package manager, e.g.
```
$ sudo aptitude install sdpa sbcl rlwrap
```
On other Linux distributions and Unix-like systems you may need to [download
SDPA](http://sdpa.sourceforge.net/download.html) and compile it from source
yourself. Compilable versions of SDPA-DD and SDPA-QD can be found at
https://github.com/denisrosset/sdpa-dd and
https://github.com/denisrosset/sdpa-qd.


### Install Quicklisp and npa-hierarchy

Quicklisp is a package manager for Lisp that lets you download libraries off
the internet and does dependency resolution (like Pip for Python). Download
[Quicklisp](https://www.quicklisp.org/) and install it, following the
instructions on their website. This basically means typing the following into
a terminal (`$` and `*` are the shell and Lisp prompts):
```
$ wget https://beta.quicklisp.org/quicklisp.lisp
$ rlwrap sbcl --noinform --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```
and type `y` to confirm that you are OK with it editing the init file when it
asks. Among other things, this should create a quicklisp/ directory in your
home directory. Download npa-hierarchy into the local-projects/ subdirectory:
```
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/ewoodhead/npa-hierarchy
```
(Or download it wherever you like and copy it to ~/quicklisp/local-projects/
or create a symlink there.) At this point you should be able to use Quicklisp
to load the npa-hierarchy library, which will pull in a few other libraries
it depends on. A simple test session, starting SBCL in a terminal, loading
the library, and maximising CHSH should look something like this:
```
$ rlwrap sbcl --noinform
* (ql:quickload :npa-hierarchy)
;; Info about all the packages being loaded omitted.
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
(Press Ctrl-D to escape.)


### Configure Emacs (optional but recommended)

It is possible to start an interactive Lisp session from the terminal (as
illustrated above). However, you can get a much nicer environment by running
Lisp from Emacs via the SLIME mode. If you don't already use Emacs, a simple
way to get started is to copy the sample .emacs file included with the
project to your home directory:
```
$ cp ~/quicklisp/local-projects/npa-hierarchy/.emacs ~
```
If SBCL is located somewhere other than /usr/bin/ then edit the line
```
      '((sbcl ("/usr/bin/sbcl" "--dynamic-space-size" "16384"))))
```
in the .emacs file accordingly (run `which sbcl` in a terminal if you're not
sure). Then:
- Start Emacs (install it if needed, e.g. `sudo aptitude install emacs`).
- Do `M-x slime` to start SLIME. This means press whatever Emacs considers
  the "Meta" key and 'x' at the same time, then type "slime" and press enter.

In key chords like C-c and M-x, 'C' and 'M' mean you should press whatever
Emacs considers the "Control" or "Meta" key along with the key that
follows. "Control" and "Meta" are usually Ctrl and Alt (they might be bound
to the Command and/or Option keys if you are using a Mac). So on most
installations, M-x means hold down the 'Alt' key while pressing the 'x' key.

Once SLIME is running you can give it Lisp expressions to evaluate. Some
useful Emacs and SLIME commands:
- C-x 1 to make the current window take up all of the Emacs frame. C-x 2 to
  split horizontally.
- C-\<up\> and C-\<down\> (or M-p and M-n) cycle through the history of
  previously entered expressions.
- C-a and C-e go to the beginning and end of the current line.
- C-k deletes everything on the current line after the cursor.
- In a Lisp file, C-c C-c compiles the expression containing the cursor. C-x
  C-e evaluates the expression immediately preceding the cursor.
- If you find yourself in the Lisp/SLIME debugger following an error, you can
  get out of it by pressing q.
- Cut, copy, and paste are C-w, M-w, and C-y, not C-x, C-c, and C-v.
- C-g cancels a partially-entered Emacs key chord or command.
- C-h t starts the Emacs tutorial.
 
You can type ",quit<return>" to quit SLIME and C-x C-c to quit Emacs.

Emacs is probably the most used general-purpose editor for developing Lisp
code, however there are plugins that add Lisp support for other editors and
IDEs including Vim, Atom, Sublime, and Eclipse. I have no experience with
them, but if you are already very familiar with one of these editors and
would prefer to use it over Emacs then you can see if one of the plugins
listed on these websites will work for you:
- https://lispcookbook.github.io/cl-cookbook/editor-support.html
- https://www.cliki.net/Development


### Updating

If you install an updated version of this library, it's a good idea to update
Quicklisp itself before loading the new version. You can do that by running
the following in Lisp:
```
(ql:update-client)
(ql:update-all-dists)
```
You can then load the new version by running `(ql:quickload :npa-hierarchy)`.

If you're using Emacs and want to check for updates to SLIME or other Emacs
packages, do `M-x package-list-packages` and then (capital) `U` to start
installing updates if there are any.



## Using the library

### Loading

To use this library, start Lisp and run one of the following three commands
to load it:
```
(ql:quickload :npa-hierarchy)
(asdf:load-system :npa-hierarchy)
(require :npa-hierarchy)
```
You should use the first one, `(ql:quickload :npa-hierarchy)`, the first time
you load the library after installing it or after an update. The rest of the
time it doesn't matter which one you use.

After loading the library, run
```
(in-package :npa-user)
```
If you're running Lisp in Emacs/SLIME, you can alternatively do `C-c M-p
npa-user`. This puts you in a working package that imports
the most important symbols from other packages in the library, so you don't
need to prefix them with their package names (so, for example, you can type
`solve-problem` instead of `npa-hierarchy:solve-problem`).


### Examples

The following examples illustrate general interactive use of this
library. They can be typed in at the prompt if you have loaded the
npa-hierarchy library and are in the npa-user working package, as described
above.

The simplest way to use the library is to use the `solve-problem` macro. The
basic invocation is illustrated by the example for CHSH given above. The
library uses the Collins-Gisin projection. Projectors can be written like
`A1\|1` or `A1/1`. The letter(s) indicate the party, the first number is the
output, and the second number (after the '/') is the input. (The identity can
be written `Id` but it can usually be omitted: numbers are usually treated as
themselves multiplied by the identity.) For example, you can maximise the
CH74 form of CHSH with
```
(solve-problem
 (maximise A1/1 B1/1 + A1/1 B1/2 + A1/2 B1/1 - A1/2 B1/2
           - A1/1 - B1/1)
 (level 1))
```
You can also use dichotomic operators for cases where there are only two
outputs. These are translated to 2 times the first projector minus the
identity. For instance, `A1` is treated the same as `(2 A1/1 - Id)` (NB: not
`(A1/1 - A2/1)`, since in the Collins-Gisin projection this would be taken to
mean there is an implicit third output). You can use the `px` macro to find
out what polynomial is being generated for an expression, e.g. ("NPA-USER>"
is the prompt):
```
NPA-USER> (px A1 (B1 + B2) + A2 (B1 - B2))
#<POLYNOMIAL 2 Id - 4 A1|1 - 4 B1|1 + 4 A1|1 B1|1 + 4 A1|1 B1|2 + 4 A1|2 B1|1
             - 4 A1|2 B1|2>
```
The order in which commuting operators appear does not matter. `B1/1 A1/1` is
the same as `A1/1 B1/1`. The case also does not matter. The Lisp reader
converts all variable and operator names (symbols) to uppercase by default.

The general format understood by the `solve-problem` macro is
```
(solve-problem
 (*imise POLYNOMIAL)
 (subject-to CONSTRAINTS...)
 (where LOCAL-VARIABLE-BINDINGS...)
 (level LEVEL))
```
Each of the forms should be surrounded with parentheses, as shown. The
`*imise` and `level` forms are required. The other two are optional. An
example using all of them is
```
(solve-problem
 (maximise A1/1)
 (subject-to (A1 (B1 + B2) = x)
             (A2 (B1 - B2) = y))
 (where (x = 1.4)
        (y = 1.4))
 (level 1 + A B + A A B))
```
`*imise`can be either `maximise` or `minimise`. The synonyms `maximize` and
`minimize` are also accepted.

Each constraint in the `subject-to` form needs to be surrounded with
parentheses; this is so that the `solve-problem` macro can tell unambiguously
where one constraint ends and the next begins. Constraints can also be
chained, as in
```
 (subject-to (A1 (B1 + B2) = A2 (B1 - B2) = 1.4))
```
Constraints can include inequalities as well as equalities, denoted by `>=`
or `<=`. `>` and `<` are also accepted and treated the same as `>=` and
`<=`. In a chain containing multiple relations, each relation is applied to
the expressions immediately on its left and right and then rearranged to an
expression equal to or lower bounded by zero. For example, a chain of the
form
```
A >= B <= C = D >= E
```
would be translated to the following four constraints:
```
A - B >= 0
C - B >= 0
C - D = 0
D - E >= 0
```

The `where` form lets you define local variable bindings of the form
`variable = VALUE`, where `VALUE` can be any number or operator
expression. For instance, the example above could just as well have been
written
```
(solve-problem
 (maximise A1/1)
 (subject-to (r = x)
             (s = y))
 (where (r = A1 (B1 + B2))
        (s = A2 (B1 - B2))
        (x = 1.4)
        (y = 1.4))
 (level 1 + A B + A A B))
```
Variables in the `where` form can refer to later assignments (they are
processed in reverse order) as well as variables defined elsewhere, so things
like
```
 (where (y = x + 3)
        (x = 2))
```
will also work. Like in the `subject-to` form, each assignment needs to be
surrounded with parentheses. Unlike the `subject-to` form, they cannot be
chained. Things like `(where (x = y = 1.4))` will not work. `with` is
accepted as a synonym for `where`.

Problems can involve more than two parties, inputs, and outputs. For example,
the three-party Mermin expression can be maximised like this:
```
(solve-problem
 (maximise A1 B1 C1 - A1 B2 C2 - A2 B1 C2 - A2 B2 C1)
 (level 1 + A B + A C + B C))
```
Froissart (level 4 takes 1-2 minutes):
```
(solve-problem
 (maximise A1 + A2 + B1 + B2 - (A1 + A2) (B1 + B2)
           + A3 (B1 - B2) + (A1 - A2) B3)
 (level 4))
```
CGLMP with three outputs:
```
(solve-problem
 (maximise 2 - 3 (A1/1 + A2/1 + B1/2 + B2/2)
           + 3 A1/1 (B1/1 + B2/1 + B1/2)
           + 3 A2/1 (B2/1 + B1/2 + B2/2)
           - 3 A1/2 (B1/1 + B2/1 - B1/2 - B2/2)
           - 3 A2/2 (B2/1 - B2/2))
 (level 1 + A B))
```
There are a few functions included that return families of Bell operators
(currently CGLMP, Mermin, and Bell-Klyshko). Thus the maximisation of CGLMP
could have been done with
```
(solve-problem
 (maximise (cglmp 3))
 (level 1 + A B))
```
In this case an important feature of the `solve-problem` macro, namely
processing an expression written in terms of operators like `A1` or `A1/1`,
is not used, and we could just as well call the simpler `maximise` function:
```
NPA-USER> (maximise (cglmp 3) '(1 + A B))
2.9148541089409035
2.9148543012130426
PDFEAS
NPA-USER> (maximise (cglmp 4) '(1 + A B))
2.97269782791719
2.972698585623988
PDFEAS
NPA-USER> (maximise (cglmp 5) '(1 + A B))
3.01571038313946
3.0157105635326187
PDFEAS
```

Operator expressions can include variables, which may be defined elsewhere or
function parameters, as long as the variable names won't be confused for a
dichotomic operator or projector. An invocation of the `solve-problem` macro
is itself an expression that returns three values. So a convienient way to
study a family of problems could be to wrap it in a function definition, e.g.
```
(defun tilted-chsh (b a)
  (solve-problem
   (maximise b A1 + a A1 (B1 + B2) + A2 (B1 - B2))
   (level 1 + A B + A A B)))
```
A few calls:
```
NPA-USER> (tilted-chsh 0 1)
2.828427030567933
2.828427164500637
PDFEAS
NPA-USER> (tilted-chsh 1 1)
3.162277511640866
3.1622777245760862
PDOPT
NPA-USER> (tilted-chsh 1/2 2)
4.609771878272943
4.6097724260714665
PDFEAS
```
The first return value is the one used if the expression appears in a context
where one return value is expected, e.g.
```
NPA-USER> (format t "Primal = ~a.~%" (tilted-chsh 1/2 2))
Primal = 4.609771878272943.
NIL
```
If you need more than just the first return value, you can use Lisp's
`multiple-value-bind` macro to get them:
```
NPA-USER> (multiple-value-bind (primal dual) (tilted-chsh 1/2 2)
            (format t "Sol in range [~a, ~a].~%" primal dual))
Sol in range [4.609771878272943, 4.6097724260714665].
NIL
```

If you want to quickly time an evaluation, wrap it in Lisp's `time` macro:
```
NPA-USER> (time
           (solve-problem
            (maximise A1 + A2 + B1 + B2 - (A1 + A2) (B1 + B2)
                      + A3 (B1 - B2) + (A1 - A2) B3)
            (level 4)))
Evaluation took:
  80.275 seconds of real time
  0.132304 seconds of total run time (0.120906 user, 0.011398 system)
  0.16% CPU
  216,034,249,956 processor cycles
  41,014,480 bytes consed
  
5.003500949689677
5.003501993945903
PDOPT
```
This tells us we were waiting about 80 seconds for the solution, with about
0.13 of those seconds spent running Lisp code (this includes generating the
hierarchy relaxation at level 4 and writing the SPDA input file).

The name of the solver is represented as a string in the global variable
`*solver*`. You can change it if the SDPA executable is named something other
than "sdpa" or to switch between multiple versions of SDPA. For example, if
you have SDPA-GMP installed and the executable is named "sdpa_gmp",
```
NPA-USER> (setf *solver* "sdpa_gmp")
"sdpa_gmp"
NPA-USER> (maximise (cglmp 3) '(1 + A B))
2.914854215512676
2.914854215512676
PDOPT
NPA-USER> (+ 1 (sqrt (/ 11.0 3)))
2.914854215512676
```


### Running SDPA manually

You may want to export a problem without running SDPA on it, for example to
run on a more powerful computer. You can accomplish this by calling the
`export-to-file` function on a problem, for example,
```
(export-to-file
 "i3322_lvl5.dat-s"
 (problem
  (maximise A1 + A2 + B1 + B2 - (A1 + A2) (B1 + B2)
            + A3 (B1 - B2) + (A1 - A2) B3)
  (level 5)))
```
You can then run SDPA on the generated input file in a terminal, e.g. with
```
$ sdpa -ds i3322_lvl5.dat-s -o i3322_lvl5.out -pt 0
```
The output file contains the comment lines
```
*Offset = 8
*Scale = 1
*Maximise = T
*Solution = -(SDP_sol / Scale + Offset).
```
and, further down, the primal and dual solutions to the SDP,
```
objValPrimal = -1.3003501538055906e+01
objValDual   = -1.3003501538055906e+01
```
(The high precision here is due to using SDPA-DD to solve the SDP.) The
format of the SDP passed to SDPA is a minimisation problem and the constant
part (the coefficient multiplied by the identity) is ignored. The comment
lines tell us we need to divide the primal and dual solutions returned by
SDPA by the scale factor of 1, add the offset 8 to the result, and finally
flip its sign to get the solution 5.003501538055906 to the original NPA
problem. The solution can also be imported into Lisp by calling the function
`extract-solution` on the output file, which applies these operations
automatically:
```
NPA-USER> (extract-solution "i3322_lvl5.out")
5.003501538055906
5.003501538055906
PFEAS
```

The `*Scale =` comment line exists because the npa-hierarchy library by
default rescales the problem if it contains rational coefficients. In that
case, npa-hierarchy translates the entire SDP to one that is equivalent to
the original, up to a scaling factor, in such a way that it contains only
exact integer and floating point coefficients. This is done in order to avoid
a possible loss of precision when the library is used with high-precision
versions of SDPA such as SDPA-GMP. This behaviour is controlled by a global
variable `*scale-ratio*`. It can be disabled by running `(setf *scale-ratio*
nil)` and reenabled with `(setf *scale-ratio* t)`.

The `problem` macro used above processes a problem in the same format as
`solve-problem` and returns an object representing the NPA relaxation. (In
fact, `(solve-problem ...)` is just defined as a shorthand for `(solve
(problem ...))`.) The returned object does not necessarily have to be passed
immediately to the `export-to-file` function. You could, for instance, save
it to a variable first:
```
(defvar *my-problem*
  (problem
   (maximise A1 + A2 + B1 + B2 - (A1 + A2) (B1 + B2)
             + A3 (B1 - B2) + (A1 - A2) B3)
   (level 5)))

(export-to-file "i3322_lvl5.dat-s" *my-problem*)
```

The generation of the NPA relaxation is actually handled by a simpler
function, `npa->sdp`. It can be used if you don't need the `problem` macro to
process an expression with symbols like `A1` in order to generate the
objective and constraints. For example, an SDPA input file for the CGLMP
expression could be generated more simply by running
```
(export-to-file "cglmp3.dat-s"
                (npa->sdp (cglmp 3) () '(1 + A B) t))
```
using the `cglmp` function already included in the library. In the call to
`npa->sdp` above, the second parameter is an (in this case, empty) list of
constraints (polynomials whose expectation values we want to set to zero) and
the fourth, `t`, indicates we want to maximise the expectation value of the
objective polynomial.

It is possible to run SDPA from Lisp itself, if you want to do this for some
reason. The function `run-sdpa` will run SDPA with specified input and output
file names:
```
(run-sdpa "cglmp3.dat-s" "cglmp3.out")
```

This works if you have already generated the input file, such as by calling
the `export-to-file` function described above.


### Getting the expectation values

As well as the primal and dual solutions, it is possible to extract and
report the expectation values of individual monomials for the solution found
by SDPA. The simplest way to do this is to set the value of the global
variable `*return-expectation-values*` to true:
```
(setf *return-expectation-values* t)
```
This will cause the `solve-problem` macro and optimisation functions like
`maximise` to return an association list of monomials and their expectation
values. An example, maximising CHSH at level one, now looks like this:
```
NPA-USER> (solve-problem
           (maximise A1 (B1 + B2) + A2 (B1 - B2))
           (level 1))
2.828427087117473
2.8284272385370617
PDFEAS
((#<A1|1> . 0.5) (#<A1|2> . 0.5) (#<B1|1> . 0.5) (#<B1|2> . 0.5)
 (#<A1|1 A1|2> . 0.25) (#<A1|1 B1|1> . 0.4268) (#<A1|1 B1|2> . 0.4268)
 (#<A1|2 B1|1> . 0.4268) (#<A1|2 B1|2> . 0.07322) (#<B1|1 B1|2> . 0.25))
 ```

This behaviour is disabled by default because the list of expectation values
rapidly gets very long for large problems; the npa-hierarchy only returns
this information if you explicitly request it. The library includes two
helper functions, `print-expectation-values` and `write-expectation-values`,
that can print out the expectation values in a nicer format. The first one,
`print-expectation-values`, simply prints the expectation values to standard
output:
```
NPA-USER> (multiple-value-bind (primal dual status exp-values)
              (solve-problem
               (maximise A1 (B1 + B2) + A2 (B1 - B2))
               (level 1))
            (declare (ignore primal dual status))
            (print-expectation-values exp-values))
<A1|1> = 0.5
<A1|2> = 0.5
<B1|1> = 0.5
<B1|2> = 0.5
<A1|1 A1|2> = 0.25
<A1|1 B1|1> = 0.4268
<A1|1 B1|2> = 0.4268
<A1|2 B1|1> = 0.4268
<A1|2 B1|2> = 0.07322
<B1|1 B1|2> = 0.25
```
(The `(declare (ignore primal dual status))` line tells Lisp not to print a
warning about the variables `primal`, `dual`, and `status` being unused in
the code.) Note that the accuracy is limited by the number of significant
digits to which SDPA writes them in the output file. Unfortunately this is
hard-coded into the solver and can't be changed without recompiling SDPA.

The argument to `print-expectation-values` can alternatively be a string or
pathname, in which case the function will interpret it as the name of an SDPA
output file and attempt to extract the expectation values from it. For
example, if you ran the CGLMP maximisation example from the previous section
you could print the values like this:
```
NPA-USER> (print-expectation-values "cglmp3.out")
<A1|1> = 0.3333
<A2|1> = 0.3333
<A1|2> = 0.3333
<A2|2> = 0.3333
<B1|1> = 0.3333
<B2|1> = 0.3333
<B1|2> = 0.3333
<B2|2> = 0.3333
<A1|1 A1|2> = 0.1377
<A1|1 A2|2> = 0.1377
<A2|1 A1|2> = 0.05803
<A2|1 A2|2> = 0.1377
<A1|1 B1|1> = 0.2694
<A1|1 B2|1> = 0.03734
<A1|1 B1|2> = 0.2694
; 81 more lines like this printed.
```

The `write-expectation-values` function is similar but lets you explicitly
say where you want the output to go (a stream or file). We could use it to
write the previous output into a text file instead of showing it at the
prompt:
```
(write-expectation-values "cglmp3_exp_values.txt" "cglmp3.out")
```

You can extract the monomials and their expectation values from a file
without printing them by calling the `expectation-values` function. This may
be useful if you want to print them in your own custom format. As an
illustration, the following code prints the expectation values of monomials
in a format that could be copied and pasted into a LaTeX source file:
```
NPA-USER> (dolist (pair (expectation-values "cglmp3.out"))
            (write-string "  \\langle")
            (destructuring-bind (monomial . value) pair
              (do-sites (s a x monomial)
                (format t " ~a_{~d|~d}" (site->string s) a x))
              (format t " \\rangle &\approx& ~f \\,, \\\\~%" value)))
  \langle A_{1|1} \rangle &approx& 0.3333 \,, \\
  \langle A_{2|1} \rangle &approx& 0.3333 \,, \\
  \langle A_{1|2} \rangle &approx& 0.3333 \,, \\
  \langle A_{2|2} \rangle &approx& 0.3333 \,, \\
  \langle B_{1|1} \rangle &approx& 0.3333 \,, \\
  \langle B_{2|1} \rangle &approx& 0.3333 \,, \\
  \langle B_{1|2} \rangle &approx& 0.3333 \,, \\
  \langle B_{2|2} \rangle &approx& 0.3333 \,, \\
  \langle A_{1|1} A_{1|2} \rangle &approx& 0.1377 \,, \\
  \langle A_{1|1} A_{2|2} \rangle &approx& 0.1377 \,, \\
  \langle A_{2|1} A_{1|2} \rangle &approx& 0.05803 \,, \\
  \langle A_{2|1} A_{2|2} \rangle &approx& 0.1377 \,, \\
  \langle A_{1|1} B_{1|1} \rangle &approx& 0.2694 \,, \\
  \langle A_{1|1} B_{2|1} \rangle &approx& 0.03734 \,, \\
  \langle A_{1|1} B_{1|2} \rangle &approx& 0.2694 \,, \\
; 81 more lines like this printed.
```
The `do-sites` macro used here loops over all of the site (party), output,
and input numbers in a monomial. The `site->string` function used converts
site numbers to strings:
```
NPA-USER> (loop for k from 0 below 100 collect (site->string k))
("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
 "T" "U" "V" "W" "X" "Y" "Z" "AA" "AB" "AC" "AD" "AE" "AF" "AG" "AH" "AI" "AJ"
 "AK" "AL" "AM" "AN" "AO" "AP" "AQ" "AR" "AS" "AT" "AU" "AV" "AW" "AX" "AY"
 "AZ" "BA" "BB" "BC" "BD" "BE" "BF" "BG" "BH" "BI" "BJ" "BK" "BL" "BM" "BN"
 "BO" "BP" "BQ" "BR" "BS" "BT" "BU" "BV" "BW" "BX" "BY" "BZ" "CA" "CB" "CC"
 "CD" "CE" "CF" "CG" "CH" "CI" "CJ" "CK" "CL" "CM" "CN" "CO" "CP" "CQ" "CR"
 "CS" "CT" "CU" "CV")
```
There is a corresponding `string->site` function that does the opposite
conversion.


### Plotting

Lisp is not Matlab, but there are ways to get a plot done, for example using
a library to call out to gnuplot. This code snippet uses the vgplot library
(`(ql:quickload :vgplot)`) to plot the local guessing probability vs. CHSH
violation:
```
(defun pguess (s)
  (solve-problem
   (maximise A1/1)
   (subject-to (A1 (B1 + B2) + A2 (B1 - B2) = s))
   (level 1 + A B)))

(let* ((s (vgplot:range (sqrt 8.0) 2 -0.005))
       (p (map 'vector #'pguess s)))
  (vgplot:plot s p))
```
Though if you are generating a plot to include in a document, you will get a
nicer result by printing the numbers to a text file and using
[PGFPlots](http://pgfplots.sourceforge.net/) to do the actual plotting. You
could call the function `pguess` above and print its results to a file in a
loop with code like this:
```
(with-open-file (f "results.table"
                   :direction :output
                   :if-exists :supersede)
  (loop for i from 0 to 1000
        for s = (alexandria:lerp (/ i 1000) 2 (sqrt 8.0))
        do (multiple-value-bind (primal dual) (pguess s)
             (format f "~6,4f  ~8,6f  ~8,6f~%" s primal dual))))
```
(The function
[`lerp`](https://common-lisp.net/project/alexandria/draft/alexandria.html#Numbers)
in the Alexandria package does linear interpolation: `(lerp v a b)` = a +
v*(b - a). Alexandria is a dependency of the npa-hierarchy library, so you
should already have it installed.)


## Some tips and possible pitfalls

### Whitespace

A lot of the whitespace in the examples above is mandatory:
- You generally need space around the arithmetic operators like `+` and
  `-`. The reason for this is that, unlike most programming languages, these
  are valid characters in variable names. `B1+B2` is a perfectly valid
  five-character variable name as far as Lisp is concerned.
- Use a space to separate operators that are being multiplied. `A1 B1` is the
  product of `A1` and `B1`. `A1B1` is an unrelated variable with a
  four-character name that Lisp will expect is defined somewhere.
- In levels like "1 + A B", you also need spaces separating the different
  parties. `A B` means parties 0 and 1. `AB` means party 27.

Where whitespace is needed it doesn't matter what kind (spaces, tabs, or
newlines).


### Floating-point precision

Lisp has single- and double-precision floating-point numbers. Unlike most
programming languages, decimal numbers like `1.0` are read as single
precision by default. If this annoys you, you can change this by evaluating
```
(setf *read-default-float-format* 'double-float)
```
Note that some functions (like `sin` and `cos`) will still return a
single-precision result if you call them with an integer argument. Make sure
to call these functions with a double-precision argument if you want a
double-precision result.


### SBCL init file

SBCL runs code in the file .sbclrc in your home directory on startup. You can
put any code here that you want run every time you start SBCL. For example,
if you wanted to always use double-float as the default and you are *only*
planning to use Lisp for the npa-hierarchy library, you could add
```
(setf *read-default-float-format* 'double-float)
(asdf:load-system :npa-hierarchy)
(in-package :npa-user)
```


### Some Lisp resources

If you need or want to learn some Lisp, a good introduction is [Practical
Common Lisp](http://www.gigamonkeys.com/book/) by Peter Seibel. The entire
book is freely readable on the author's website. It is about K&R level: it
assumes you already know how to program and you just need to learn the
specifics of how things are done in Lisp.

These resources may also be useful:
- The [Common Lisp Cookbook](http://cl-cookbook.sourceforge.net/).
- The [Common Lisp
  HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm).
- A PDF of the ANSI Common Lisp Standard draft can be obtained from
  [here](http://cvberry.com/tech_writings/notes/common_lisp_standard_draft.html).
- [Learn X in Y minutes](https://learnxinyminutes.com/docs/common-lisp/) for
  Common Lisp.
- There's a comparison with Python and translation guide
  [here](https://norvig.com/python-lisp.html).



## Some computed cases

The tables below list upper bounds on the Froissart (a.k.a. I3322) and CGLMP
expectation values computed using SDPA-DD.


### Froissart

The following table lists upper bounds on the Froissart expectation value at
levels 1 through to 5 of the hierarchy.

| Level | I3322 bound
|:-----:| --------------------------
|   1   | 5.5
|   2   | 5.0037588865482327(71-96)
|   3   | 5.003502248092139(39-45)
|   4   | 5.00350153805590723(11-30)
|   5   | 5.0035015380559061(08-55)

At levels 2-5, the digits that are the same for the primal and dual solutions
are listed, followed by their next two digits (rounded outwards) in brackets,
e.g. 1.23(45-67) would mean the primal is greater than 1.2345 and the dual is
less than 1.2367.


### CGLMP

This table lists upper bounds on the expectation value of the family of CGLMP
expressions for different numbers of outputs at level 1 + A B of the
hierarchy. The results are accurate to about double precision.

| Outputs | CGLMP bound
|:-------:|------------------- 
|     2   | 2.8284271247461903
|     3   | 2.914854215512676
|     4   | 2.9726982671022437
|     5   | 3.0157104755226736
|     6   | 3.049700419240848
|     7   | 3.077648311434258
|     8   | 3.101280587905469
|     9   | 3.121684417680056
|    10   | 3.1395874077348047
|    11   | 3.1554996820414862

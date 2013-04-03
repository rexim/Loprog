# Loprog #

Loprog (stands for **Lo**gical **prog**ramming) is a stupid
[Prolog](https://en.wikipedia.org/wiki/Prolog) implementation.

## Usage ##

First of all, you'll need [sbt](http://www.scala-sbt.org/).

* `$ git clone git://github.com/rexim/Loprog.git Loprog && cd Loprog`
— clone the repository somewhere;
* `$ sbt test` — ensure that the last changes don't break any
functionality;
* `$ sbt 'run <source-code-file>'` — run the Loprog interpreter and
load (consult) a source code file.
* `?- halt.` — to quit the interpreter.

There are several examples of source code files in the Loprog
directory. Let's go through them.

### example-01.loprog ###

This example contains a simple database of programming languages and a
couple of general purpose predicates.

Example related queries:

* `?- oop(X), functional(X).` — what languages are object-oriented and
  functional simultaneously?
* `?- equal(X, f(X)).` — circular reference.
* `?- last(cons(x, cons(y, cons(z, nil))), Last).` — the last element
  of a list.

### example-02.loprog ###

This example was taken from
[here](http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/3_1.html)

Example related queries:

* `?- p(X).` — should act like described in the link above.

## License ##

Copyright (C) 2013 Alexey Kutepov a.k.a rexim

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

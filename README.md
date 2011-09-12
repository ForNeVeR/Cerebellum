Cerebellum
==========

About
=====
Cerebellum is simple server for sharing various kinds of data between various
clients. Main focus made on to-do lists integrated with emacs org-mode files.

License
=======
Cerebellum is distributed under terms of MIT license.

Copyright (C) 2011 by ForNeVeR

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Building
========
To build current version of source code, execute following commands:

    $ cd Cerebellum
    $ erl -make

Usage
=====
### Starting server

First, run Erlang console from the ebin directory:

    $ cd ebin && erl

Then start cerebellum application:

    1> cerebellum:start().

### Stopping server

Use the following function call:

    2> cerebellum:stop().

This will stop the whole application.

Concepts
========
Main program concept is 'adapter'. Adapter is an Erlang-written subsystem for
operating with environment. Adapters may be written for network socket, file
system, database.
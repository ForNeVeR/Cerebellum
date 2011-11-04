Cerebellum
==========

About
=====
Cerebellum is simple server for sharing various kinds of data between various
clients. Main focus made on to-do lists integrated with emacs org-mode files.

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

Configuration
=============
Server configuration is stored in a file named `config.yaml`. It is simple YAML
file storing list of module names. Every included module will be started with
its default parameters.

Concepts
========
Main program concept is 'adapter'. Adapter is an Erlang-written subsystem for
operating with environment. Adapters may be written for network socket, file
system, database.
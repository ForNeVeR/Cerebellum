Cerebellum
==========

About
=====
Cerebellum is simple server for sharing various kinds of data between various
clients. Main focus will be made on to-do lists integrated with emacs org-mode
files.

Building
========
To build current version of source code, execute following commands:

    $ cd Cerebellum
    $ erl -make

Usage
=====
### Starting server

First, start Erlang shell:

	$ cd Cerebellum
    $ erl -pa ebin

Then start cerebellum application:

    1> application:start(cerebellum).

### Stopping server

Use the standard function call:

    2> application:stop(cerebellum).

This will hopefully stop the whole application.

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
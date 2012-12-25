Simple Logger
=============

Simple Logger Facility built over disk_log.

Code Example
============

```erlang
application:start(sl),
sl:open("/path/to/log/file.log"),
sl:info("hello!"), % you can use debug, info, warn,
                     error, critical and log methods
sl:close(). % it's not necessary at all
```

Usage
=====

Project uses rebar for building purposes, so you can generate edoc by yourself
using command `rebar doc` from project root.

EzBliki - A fusion of a blog and a wiki written in Erlang
---

Intro
---
The EzBliki project provides a combination blog/wiki that
has several advanced features, including federated bliki
content, prevaylence layer for storage, OpenID authentication,
and editing via Atom Publishing Protocol.

Build Instructions (Linux/Mac OS X)
---
To build, first make sure you have the most recent version of
Erlang installed (tested on 5.6.1 R12B-1).

Now check out the sources with:
  hg clone http://hg.assembla.com/ezbliki

Next cd to ezbliki directory and type 
  ./build


Running Ezbliki
---
Ezbliki is run by typing the following at the shell while in 
the ezbliki directory:
  ./run

NOTE: EzBliki is an old project and is in the process of being 
updated to modern components and dependencies. EzBliki will not 
run currently. Depending on the updates needed this project will
either be reworked and updated or a new project will be created.

If a new project is created then this file will be updated to
point to the new project.

For questions and suggestions please email w.a.barnhill@gmail.com

Use your web browser to surf to http://localhost:8080 to verify.

Since Erlang is portable Ezbliki should run on Windows, but Windows
support and instructions have not yet arrived (coming shortly).


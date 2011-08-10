Reia
====

Welcome to Reia (pronounced RAY-uh), a Ruby-like scripting language for the 
Erlang virtual machine (BEAM).

Want to know more about Reia? Syntax examples and that sort of thing? Please
visit the home page at:

[http://reia-lang.org](http://reia-lang.org)

Compiling Reia
--------------

Reia requires Erlang version R12B-3 (5.6.3) or later. The latest version of 
Erlang is available here:

[http://www.erlang.org/download.html](http://www.erlang.org/download.html)

You'll also need Ruby and Rake installed.  To compile Reia, type:

   rake

After compilation is complete, you'll see the test suite run and if everything
went well it should hopefully pass.

Implementation
--------------

Here's some thoroughly interesting implementation trivia about Reia:

* Leex-based scanner
* Yecc-based grammar (a Neotoma-based branch is also available)
* Successive parse transforms convert Reia parse trees into the Erlang abstract
  format, then into BEAM/HiPE bytecode

Links
-----

* Home Page: [http://reia-lang.org](http://reia-lang.org)
* Reia Wiki: [http://wiki.reia-lang.org/](http://wiki.reia-lang.org/)
* Mailing List: [http://groups.google.com/group/reia](http://groups.google.com/group/reia)
* Author's Blog: [http://unlimitednovelty.com/](http://unlimitednovelty.com/)
* Author's Twitter: [http://twitter.com/bascule](http://twitter.com/bascule)
* IRC: irc.freenode.net #reia

About the Author
----------------

Reia was created by Tony Arcieri, a programmer from Denver, Colorado, USA.
Tony has a background in network services and distributed peer-to-peer 
systems. His favorite programming languages are Ruby and Erlang.

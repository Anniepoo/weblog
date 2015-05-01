weblog 
======

Web Library for [SWI-Prolog](http://www.swi-prolog.org).
Rev 0.1.0

Licensed under the Lesser General Public License Vers. 3, June 2007.

https://www.gnu.org/copyleft/lesser.html

A copy of which should accompany this library.

SWI-Prolog ships with an excellent Web framework.
This library builds on that work and provides additional predicates that ease doing common Web tasks.

Some of the code is simply snippets that are extracted from SWI-Prolog related stuff, or other sources, but we have tried to extract everything to the point that it is a reasonable library.
Currently, users should expect that almost anything can change.



Install
=======

Other than having a normal SWI-Prolog install, the only installation step is to clone this repository from [Github](https://github.com/Anniepoo/weblog) or install through [SWI-Prolog's built-in packaging system](http://www.swi-prolog.org/pack/list).

Setting keys
------------

For some parts of the demoyou will need to set the following two map providers: CloudMade and Google.
This is done by copying file [1] to location [2] and enter your own API key:

```
[1a]   weblog/keys/cloudmadekey.pl.example
[1a]   weblog/keys/googlekey.pl.example
[2a]   weblog/keys/cloudmadekey.pl
[2a]   weblog/keys/googlekey.pl
```



Starting the demo
=================

The demo is started with the following:

```bash
$ swipl debug.pl
?- weblogdemo:weblog_demo
```



Libraries
=========

Every subdirectory of this library contains a `README.md` file that explains the contents of that directory.
Here is an overview of the directories:
  - `/demo`
    Every major module (ideally) has a demo page.
    Examining the demo code is often an excellent way to learn to use a feature.
  - `/docs`
    Documentation for **weblog**.
  - `/prolog`
    - `/prolog/debug_page`
      Loading these modules causes your server to serve a couple of pages with useful debugging info.
    - `/prolog/formatting`
      Tools that assist with generating HTML elements such as tables, boxes, and other page layout elements.
    - `/prolog/html_form`
      Tools for generating HTML forms with validation.
    - `/prolog/info`
      Contains tools for displaying various sorts of information.
      This includes tools for displaying specific types of info, handling feeds (geohashing data, geocoding), and often a combination of those (e.g. twitter buttons).
    - `/prolog/keys`
      Location where API keys that are needed in order to use features by third party providers (feeds) to work.
    - `/prolog/login_prototype`
      Tools to make logging into a Web page easier (thanks Thanos!).
      For the moment this is a prototype (see `TODO.md`).
    - `/prolog/nav`
      Navigation widgets such as menus, links, etc.
    - `/prolog/resources`
      Resource definitions for other modules, such as definitions of commonly used JavaScript libraries.
    - `/prolog/static`
      Static files needed by other parts of **weblog**.
    - `/prolog/support`
      Utility bits that have generally been useful, such as utilities for HTML comments handling.

Manifesto
=========

The library should remain bits and pieces you can use as you like, with little cost of inheriting **weblog** into a custom project of your own.
For instance, if all you want is the accordion widget you should not have to change the way in which your project is coded in order to use it.

The only real 'common' pieces are `/prolog/resources`, which defines the resource inclusion names and `/prolog/static` which holds the various bits of JavaScript that are needed by some of the widgets.
For the moment a truly lean install of **weblog** might require a bit of picking through those.



A Common Pattern
================

Web widgets often need a lot of rather trivial parameters for which there are often good defaults.
A very common pattern for our widgets is to provide a closure which is called with a partially instantiated extra argument.
The closure instantiates some variables that are handed over to the widget.
This pattern gives the user flexibility.
For simple use cases Prolog facts can be used as the closure.
For more complex use cases one can describe rules.
 in 'family tree' style code.



Contributing
============

We emphatically welcome contributions.
Since this is more of a toolbox library than a unified framework, it is relatively easy to contribute bits and pieces.
**Weblog** is an ambitious project, far beyond what one person can do.
It is intended to be a kitchen sink project.
It arose from collecting existing code written for various projects.

Code should generally adhere to the following rules of guidance:
  - Follow [these style conventions](http://www.ai.uga.edu/mc/plcoding.pdf)
    with the 'comma spacing by use' convention.
  - Code comments should generate reasonable pldoc.
    See [this tutorial on using pldoc](http://www.pathwayslms.com/swipltuts/pldoc/), the SWI-Prolog documentation system.
  - Contributions should include a demo page.
  - So far the main controbutors have done a laughably bad job of following these rules themselves :-P



Contributors
============

We want to thank the following folks:
  - Anne Ogborn - HTML tables, forms, accordion, maps.
  - Thanos Tintinidis - HTML login and validated forms.
  - Jan Wielemaker - Original google maps code, underlying HTML and HTTP libraries and of course SWI-Prolog!
  - University of Houston - Contributed the original library.
  - [Wouter Beek](http://www.wouterbeek.com) - Converted to Prolog package.

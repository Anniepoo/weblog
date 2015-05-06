**WebLog**
==========

A Web library for [SWI-Prolog](http://www.swi-prolog.org).
Version 0.1.0

Licensed under the Lesser General Public License Vers. 3, June 2007,
see license.txt

[SWI-Prolog](http://www.swi-prolog.org) ships with an excellent Web framework.
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
$ swipl demo/debug.pl
?- weblog_demo.
```

Reading the documentation
-------------------------

```bash
$ swipl demo/debug.pl
?- pldoc.
```



Libraries
=========

Every subdirectory of this library contains a `README.md` file that explains the contents of that directory.
Here is an overview of the directories:
  
  $ `'/demo'` :
  Every major module (ideally) has a demo page.
  Examining the demo code is often an excellent way to learn to use a feature.
  
  $ `'/docs'` :
  Documentation for **WebLog**.
  
  $ `'/prolog/wl'` :
    $ `'/prolog/wl/format'` :
    Tools that assist with generating HTML elements such as tables, boxes, and other page layout elements.
    
    $ `'/prolog/wl/html_form'` :
    Tools for generating HTML forms with validation.
    
    $ `'/prolog/wl/identity'` :
    Tools to make logging into a Web page easier (thanks Thanos!).
    For the moment this is a prototype (see `TODO.md`).
    
    $ `'/prolog/wl/info'` :
    Contains tools for displaying various sorts of information.
    This includes tools for displaying specific types of info, handling feeds (geohashing data, geocoding), and often a combination of those (e.g. twitter buttons).
    
    $ `'/prolog/wl/nav'` :
    Navigation widgets such as menus, links, etc.
    
    $ `'/prolog/wl/page'` :
    Loading these modules causes your server to serve a couple of pages with useful debugging info.
    
    $ `'/prolog/wl/resource'` :
    Resource definitions for other modules, such as definitions of commonly used JavaScript libraries.
    
    $ `'/prolog/wl/support'` :
    Utility bits that have generally been useful, such as utilities for HTML comments handling.
    
    $ `'/prolog/wl/widget'` :
    ...



Contributors
============

Please read the **WebLog** MANIFESTO.md before contributing.

The list of current contributors:
  
  $ Anne Ogborn :
  HTML tables, forms, accordion, maps.
  
  $ Thanos Tintinidis :
  HTML login and validated forms.
  
  $ Jan Wielemaker :
  Original google maps code, underlying HTML and HTTP libraries and of course SWI-Prolog!
  
  $ University of Houston :
  Contributed the original library.
  
  $ [Wouter Beek](http://www.wouterbeek.com) :
  Converted to Prolog package.

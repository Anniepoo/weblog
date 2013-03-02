weblog
======

Web Library for SWI-Prolog
Rev 1e-9

Licensed under the Lesser General Public License Vers. 3, June 2007

https://www.gnu.org/copyleft/lesser.html

A copy of which should accompany this library

SWI-Prolog ships with an excellent web framework. This library builds on that work
and is not a 'web framework'. It is, instead, a library of predicates that ease doing
common web tasks.

Currently, users should expect that almost anything can change.

Starting
========

The demo may be started by consulting debug.pl and querying weblogdemo:weblog_demo.

Libraries
=========

debug_page/debug_page.pl  serves a page of useful debug info at /debug

formatting    tools for assisting with tables, boxes, and other page layout

html_form     validated html forms.

info          subdirectories contain tools for displaying various sorts of information.
              at the moment the only useful one is google maps
              
login_prototype   Tools to make logging in easier.

resources     resource definitions for other modules, definitions of commonly used javascript libs

Contributing
============

We welcome contributions. Because this is more a 'toolbox' library than a unified 
framework, it's particularly easy to contribute.

Code should generally follow the style conventions of 

http://www.ai.uga.edu/mc/plcoding.pdf

with the 'comma spacing by use' convention.

Comments should generate reasonable pldoc. For a tutorial on pldoc use, see

http://www.pathwayslms.com/swipltuts/pldoc/

Contributions should include a demo page.


Contributors
============

We'd like to thank the following folks:

Anne Ogborn - tables, forms, accordion
Thanos Tintinidis   - login and forms
Jan Wielemaker - google maps, and of course SWI-Prolog!
University of Houston - contributed the original library











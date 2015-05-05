page directory
==============

This directory contains modules that generate complete Web pages.

By loading these modules an HTTP handler with the same name
is added under the Web root.
For instance, `use_module(library(wl/page/debug_page))`
makes a corresponding Web page available at `http://my_site.com/debug_page`.

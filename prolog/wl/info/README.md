Info directory
==============

This directory contains libraries that are useful for displaying specialized
types of content.
Examples include:
  - Maps
  - Flash animations
  - Youtube/vimeo/etc embedded videos
  - Chart and graph packages
  - Markup languages and HTML editors
 
This directory mixes feeds and displays.
This is a deliberate decision to avoid splitting components in which
feed and display are inextricably intertwined.
 
When we have, or might reasonably have, more than one provider for
an info type (e.g., maps), we try to have an abstract library that wraps
the specific libraries.
 
Using the abstract library rather than the specific one is recommended.
 
For some components the content provider requires an API key.
The key files are in /keys
Copy the foo.pl.example file to foo.pl and modify with your key.


**WebLog** Manifesto
====================

The **WebLog** library should remain bits and pieces you can use as you like, with little cost of inheriting **WebLog** into a custom project of your own.
For instance, if all you want is the accordion widget you should not have to change the way in which your project is coded in order to use it.

The only real 'common' pieces are `/prolog/resources`, which defines the resource inclusion names and `/prolog/static` which holds the various bits of JavaScript that are needed by some of the widgets.
For the moment a truly lean install of **WebLog** might require a bit of picking through those.



A Common Pattern
----------------

Web widgets often need a lot of rather trivial parameters for which there are often good defaults.
A very common pattern for our widgets is to provide a closure which is called with a partially instantiated extra argument.
The closure instantiates some variables that are handed over to the widget.
This pattern gives the user flexibility.
For simple use cases Prolog facts can be used as the closure.
For more complex use cases one can describe rules.
 in 'family tree' style code.



Contributing
------------

We emphatically welcome contributions.
Since this is more of a toolbox library than a unified framework, it is relatively easy to contribute bits and pieces.
**WebLog** is an ambitious project, far beyond what one person can do.
It is intended to be a kitchen sink project.
It arose from collecting existing code written for various projects.

Code should generally adhere to the following rules of guidance:
  
  * Follow [these style conventions](http://www.ai.uga.edu/mc/plcoding.pdf)
    with the 'comma spacing by use' convention.
  
  * Code comments should generate reasonable pldoc.
    See [this tutorial on using pldoc](http://www.pathwayslms.com/swipltuts/pldoc/), the SWI-Prolog documentation system.
  
  * Contributions should include a demo page.

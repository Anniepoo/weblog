:- module(configurator, [
	     configurator//1
	  ]).

:- use_module(library(http/html_write)).

:- meta_predicate configurator(1, ?, ?).

/**    configurator(+Generator:callable)// is det

      Create a configurator

      A configurator is a way of selecting a set of options among a
      nonorthogonal set.

      An example would be a product selector for a car, where there are
basic models, and for each model a set of options. Suppose there's
three models, the Nogo, the Yugo, and the Bogo. Each one can be in
any of 3 colors, and any of three hubcap styles, a,b, and c,
except the Bogo's not available in blue. Lets say the default when
the page loads is a blue Nogo with b hubcaps.

      ==

      [===========]
      [           ]
      [  image    ]
      [           ]
      [===========]
      [[Nogo]] (Yugo) (Bogo)
      (red) (blk) [[blue]]
      (a) [[b]] (c)

      ==

The user picks a Bogo. Since blue's not available the color changes to
black

      ==

      [===========]
      [           ]
      [  image    ]
      [           ]
      [===========]
      (Nogo) [[Yugo]] (Bogo)
      (red) [[blk]]
      (a) [[b]] (c)

      ==

Each line is a _layer_ , with one or more choices.
The image is made up of partially transparent layers.

Generator is called with 1 extra argument which is partially
instantiated and whose arguments are bound by Generator. The extra
argument may be of the form:

      * layer(Layer)   Layer is the name of a layer

      *

      NOTE - this isn't finished, ignore for now
*/
configurator(Generator) -->
	html([
	    div([class='wl-cfg'], [
		    div([], \configurator_images(Generator)),
		    div([], \configurator_radios(Generator))
		])
	]).

configurator_layers(Generator, Layers) :-
	bagof(Layer, call(Generator, layer(Layer)), Layers).

configurator_images(Generator) -->
	{
            configurator_layers(Generator, Layers)
        },
	configurator_images(Generator, Layers).

configurator_images(_, []) --> [].
configurator_images(Generator, [H|T]) -->
	{
           call(Generator, image_for(H, ImageSrc)),
           call(Generator, image_location_for(H, point(X, Y), size(W, Ht))),
	   format(atom(Style), 'top: ~w; left: ~w;', [X, Y])
        },
	html([
	    img([src=ImageSrc, width=W, height=Ht, style=Style], [])
	]),
	configurator_images(Generator, T).

configurator_radios(Generator) -->
	{
            configurator_layers(Generator, Layers)
        },
	configurator_radios_layer(Generator, Layers).

configurator_radios_layer(_, []) --> [].
configurator_radios_layer(Generator, [Layer|T]) -->
	{
	    bagof(Choice,
		  call(Generator, choices_for(Layer, Choice)), Choices)
        },
	html([
            div([], [\configurator_choices(Generator, Layer, Choices)])
	]),
	configurator_radios_layer(Generator, T).

configurator_choices(_, _, []) --> [].
configurator_choices(Generator, Layer, [Choice|T]) -->
	{
             call(Generator, current_choice(Layer, Choice)), % we're current
	     call(Generator, radio_image_for(Layer, Choice, Image)),!
        },
	html([
             img([class=c, src=Image], [])
	]),
	configurator_choices(Generator, Layer, T).
configurator_choices(Generator, Layer, [Choice|T]) -->
	{
	     call(Generator, radio_image_for(Layer, Choice, Image)),!
        },
	html([
             img([class=x, src=Image], [])
	]),
	configurator_choices(Generator, Layer, T).

:- module(font_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/fonts/google_fonts)).

:- http_handler(root(font), font_page_handler, [id(font)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(font, 'Fonts').

font_page_handler(_):-
  findall(Font, google_font(Font), Fonts0),
  random_sublist(Fonts, 10, Fonts0),
  reply_html_page(
    weblog_demo,
    title('Fonts'),
    [
      h1('Fonts'),
      h3('(Refresh page to see new fonts.)'),
      hr([]),
      \google_font_examples(Fonts)
    ]
  ).

google_font_examples([]) --> !, html([]).
google_font_examples([H|T]) -->
  {format(atom(Style), 'font-family: "~a";', [H])},
  html([
    \google_font(H),
    p(style=Style, ['Example text using Google font "',H,'".']),
    \google_font_examples(T)
  ]).

random_sublist([], 0, _).
random_sublist([H|T], N1, L1):-
  random_select(H, L1, L2),
  N2 is N1 - 1,
  random_sublist(T, N2, L2).

google_font('ABeeZee').
google_font('Abel').
google_font('Abril Fatface').
google_font('Aclonica').
google_font('Acme').
google_font('Actor').
google_font('Adamina').
google_font('Advent Pro').
google_font('Aguafina Script').
google_font('Akronim').
google_font('Aladin').
google_font('Aldrich').
google_font('Alef').
google_font('Alegreya').
google_font('Alegreya SC').
google_font('Alegreya Sans').
google_font('Alegreya Sans SC').
google_font('Alex Brush').
google_font('Alfa Slab One').
google_font('Alice').
google_font('Alike').
google_font('Alike Angular').
google_font('Allan').
google_font('Allerta').
google_font('Allerta Stencil').
google_font('Allura').
google_font('Almendra').
google_font('Almendra Display').
google_font('Almendra SC').
google_font('Amarante').
google_font('Amaranth').
google_font('Amatic SC').
google_font('Amethysta').
google_font('Amiri').
google_font('Anaheim').
google_font('Andada').
google_font('Andika').
google_font('Angkor').
google_font('Annie Use Your Telescope').
google_font('Anonymous Pro').
google_font('Antic').
google_font('Antic Didone').
google_font('Antic Slab').
google_font('Anton').
google_font('Arapey').
google_font('Arbutus').
google_font('Arbutus Slab').
google_font('Architects Daughter').
google_font('Archivo Black').
google_font('Archivo Narrow').
google_font('Arimo').
google_font('Arizonia').
google_font('Armata').
google_font('Artifika').
google_font('Arvo').
google_font('Asap').
google_font('Asset').
google_font('Astloch').
google_font('Asul').
google_font('Atomic Age').
google_font('Aubrey').
google_font('Audiowide').
google_font('Autour One').
google_font('Average').
google_font('Average Sans').
google_font('Averia Gruesa Libre').
google_font('Averia Libre').
google_font('Averia Sans Libre').
google_font('Averia Serif Libre').
google_font('Bad Script').
google_font('Balthazar').
google_font('Bangers').
google_font('Basic').
google_font('Battambang').
google_font('Baumans').
google_font('Bayon').
google_font('Belgrano').
google_font('Belleza').
google_font('BenchNine').
google_font('Bentham').
google_font('Berkshire Swash').
google_font('Bevan').
google_font('Bigelow Rules').
google_font('Bigshot One').
google_font('Bilbo').
google_font('Bilbo Swash Caps').
google_font('Biryani').
google_font('Bitter').
google_font('Black Ops One').
google_font('Bokor').
google_font('Bonbon').
google_font('Boogaloo').
google_font('Bowlby One').
google_font('Bowlby One SC').
google_font('Brawler').
google_font('Bree Serif').
google_font('Bubblegum Sans').
google_font('Bubbler One').
google_font('Buda').
google_font('Buenard').
google_font('Butcherman').
google_font('Butterfly Kids').
google_font('Cabin').
google_font('Cabin Condensed').
google_font('Cabin Sketch').
google_font('Caesar Dressing').
google_font('Cagliostro').
google_font('Calligraffitti').
google_font('Cambay').
google_font('Cambo').
google_font('Candal').
google_font('Cantarell').
google_font('Cantata One').
google_font('Cantora One').
google_font('Capriola').
google_font('Cardo').
google_font('Carme').
google_font('Carrois Gothic').
google_font('Carrois Gothic SC').
google_font('Carter One').
google_font('Caudex').
google_font('Cedarville Cursive').
google_font('Ceviche One').
google_font('Changa One').
google_font('Chango').
google_font('Chau Philomene One').
google_font('Chela One').
google_font('Chelsea Market').
google_font('Chenla').
google_font('Cherry Cream Soda').
google_font('Cherry Swash').
google_font('Chewy').
google_font('Chicle').
google_font('Chivo').
google_font('Cinzel').
google_font('Cinzel Decorative').
google_font('Clicker Script').
google_font('Coda').
google_font('Coda Caption').
google_font('Codystar').
google_font('Combo').
google_font('Comfortaa').
google_font('Coming Soon').
google_font('Concert One').
google_font('Condiment').
google_font('Content').
google_font('Contrail One').
google_font('Convergence').
google_font('Cookie').
google_font('Copse').
google_font('Corben').
google_font('Courgette').
google_font('Cousine').
google_font('Coustard').
google_font('Covered By Your Grace').
google_font('Crafty Girls').
google_font('Creepster').
google_font('Crete Round').
google_font('Crimson Text').
google_font('Croissant One').
google_font('Crushed').
google_font('Cuprum').
google_font('Cutive').
google_font('Cutive Mono').
google_font('Damion').
google_font('Dancing Script').
google_font('Dangrek').
google_font('Dawning of a New Day').
google_font('Days One').
google_font('Dekko').
google_font('Delius').
google_font('Delius Swash Caps').
google_font('Delius Unicase').
google_font('Della Respira').
google_font('Denk One').
google_font('Devonshire').
google_font('Dhurjati').
google_font('Didact Gothic').
google_font('Diplomata').
google_font('Diplomata SC').
google_font('Domine').
google_font('Donegal One').
google_font('Doppio One').
google_font('Dorsa').
google_font('Dosis').
google_font('Dr Sugiyama').
google_font('Droid Sans').
google_font('Droid Sans Mono').
google_font('Droid Serif').
google_font('Duru Sans').
google_font('Dynalight').
google_font('EB Garamond').
google_font('Eagle Lake').
google_font('Eater').
google_font('Economica').
google_font('Ek Mukta').
google_font('Electrolize').
google_font('Elsie').
google_font('Elsie Swash Caps').
google_font('Emblema One').
google_font('Emilys Candy').
google_font('Engagement').
google_font('Englebert').
google_font('Enriqueta').
google_font('Erica One').
google_font('Esteban').
google_font('Euphoria Script').
google_font('Ewert').
google_font('Exo').
google_font('Exo 2').
google_font('Expletus Sans').
google_font('Fanwood Text').
google_font('Fascinate').
google_font('Fascinate Inline').
google_font('Faster One').
google_font('Fasthand').
google_font('Fauna One').
google_font('Federant').
google_font('Federo').
google_font('Felipa').
google_font('Fenix').
google_font('Finger Paint').
google_font('Fira Mono').
google_font('Fira Sans').
google_font('Fjalla One').
google_font('Fjord One').
google_font('Flamenco').
google_font('Flavors').
google_font('Fondamento').
google_font('Fontdiner Swanky').
google_font('Forum').
google_font('Francois One').
google_font('Freckle Face').
google_font('Fredericka the Great').
google_font('Fredoka One').
google_font('Freehand').
google_font('Fresca').
google_font('Frijole').
google_font('Fruktur').
google_font('Fugaz One').
google_font('GFS Didot').
google_font('GFS Neohellenic').
google_font('Gabriela').
google_font('Gafata').
google_font('Galdeano').
google_font('Galindo').
google_font('Gentium Basic').
google_font('Gentium Book Basic').
google_font('Geo').
google_font('Geostar').
google_font('Geostar Fill').
google_font('Germania One').
google_font('Gidugu').
google_font('Gilda Display').
google_font('Give You Glory').
google_font('Glass Antiqua').
google_font('Glegoo').
google_font('Gloria Hallelujah').
google_font('Goblin One').
google_font('Gochi Hand').
google_font('Gorditas').
google_font('Goudy Bookletter 1911').
google_font('Graduate').
google_font('Grand Hotel').
google_font('Gravitas One').
google_font('Great Vibes').
google_font('Griffy').
google_font('Gruppo').
google_font('Gudea').
google_font('Gurajada').
google_font('Habibi').
google_font('Halant').
google_font('Hammersmith One').
google_font('Hanalei').
google_font('Hanalei Fill').
google_font('Handlee').
google_font('Hanuman').
google_font('Happy Monkey').
google_font('Headland One').
google_font('Henny Penny').
google_font('Herr Von Muellerhoff').
google_font('Hind').
google_font('Holtwood One SC').
google_font('Homemade Apple').
google_font('Homenaje').
google_font('IM Fell DW Pica').
google_font('IM Fell DW Pica SC').
google_font('IM Fell Double Pica').
google_font('IM Fell Double Pica SC').
google_font('IM Fell English').
google_font('IM Fell English SC').
google_font('IM Fell French Canon').
google_font('IM Fell French Canon SC').
google_font('IM Fell Great Primer').
google_font('IM Fell Great Primer SC').
google_font('Iceberg').
google_font('Iceland').
google_font('Imprima').
google_font('Inconsolata').
google_font('Inder').
google_font('Indie Flower').
google_font('Inika').
google_font('Irish Grover').
google_font('Istok Web').
google_font('Italiana').
google_font('Italianno').
google_font('Jacques Francois').
google_font('Jacques Francois Shadow').
google_font('Jaldi').
google_font('Jim Nightshade').
google_font('Jockey One').
google_font('Jolly Lodger').
google_font('Josefin Sans').
google_font('Josefin Slab').
google_font('Joti One').
google_font('Judson').
google_font('Julee').
google_font('Julius Sans One').
google_font('Junge').
google_font('Jura').
google_font('Just Another Hand').
google_font('Just Me Again Down Here').
google_font('Kalam').
google_font('Kameron').
google_font('Kantumruy').
google_font('Karla').
google_font('Karma').
google_font('Kaushan Script').
google_font('Kavoon').
google_font('Kdam Thmor').
google_font('Keania One').
google_font('Kelly Slab').
google_font('Kenia').
google_font('Khand').
google_font('Khmer').
google_font('Khula').
google_font('Kite One').
google_font('Knewave').
google_font('Kotta One').
google_font('Koulen').
google_font('Kranky').
google_font('Kreon').
google_font('Kristi').
google_font('Krona One').
google_font('La Belle Aurore').
google_font('Laila').
google_font('Lakki Reddy').
google_font('Lancelot').
google_font('Lateef').
google_font('Lato').
google_font('League Script').
google_font('Leckerli One').
google_font('Ledger').
google_font('Lekton').
google_font('Lemon').
google_font('Libre Baskerville').
google_font('Life Savers').
google_font('Lilita One').
google_font('Lily Script One').
google_font('Limelight').
google_font('Linden Hill').
google_font('Lobster').
google_font('Lobster Two').
google_font('Londrina Outline').
google_font('Londrina Shadow').
google_font('Londrina Sketch').
google_font('Londrina Solid').
google_font('Lora').
google_font('Love Ya Like A Sister').
google_font('Loved by the King').
google_font('Lovers Quarrel').
google_font('Luckiest Guy').
google_font('Lusitana').
google_font('Lustria').
google_font('Macondo').
google_font('Macondo Swash Caps').
google_font('Magra').
google_font('Maiden Orange').
google_font('Mako').
google_font('Mallanna').
google_font('Mandali').
google_font('Marcellus').
google_font('Marcellus SC').
google_font('Marck Script').
google_font('Margarine').
google_font('Marko One').
google_font('Marmelad').
google_font('Martel').
google_font('Martel Sans').
google_font('Marvel').
google_font('Mate').
google_font('Mate SC').
google_font('Maven Pro').
google_font('McLaren').
google_font('Meddon').
google_font('MedievalSharp').
google_font('Medula One').
google_font('Megrim').
google_font('Meie Script').
google_font('Merienda').
google_font('Merienda One').
google_font('Merriweather').
google_font('Merriweather Sans').
google_font('Metal').
google_font('Metal Mania').
google_font('Metamorphous').
google_font('Metrophobic').
google_font('Michroma').
google_font('Milonga').
google_font('Miltonian').
google_font('Miltonian Tattoo').
google_font('Miniver').
google_font('Miss Fajardose').
google_font('Modak').
google_font('Modern Antiqua').
google_font('Molengo').
google_font('Molle').
google_font('Monda').
google_font('Monofett').
google_font('Monoton').
google_font('Monsieur La Doulaise').
google_font('Montaga').
google_font('Montez').
google_font('Montserrat').
google_font('Montserrat Alternates').
google_font('Montserrat Subrayada').
google_font('Moul').
google_font('Moulpali').
google_font('Mountains of Christmas').
google_font('Mouse Memoirs').
google_font('Mr Bedfort').
google_font('Mr Dafoe').
google_font('Mr De Haviland').
google_font('Mrs Saint Delafield').
google_font('Mrs Sheppards').
google_font('Muli').
google_font('Mystery Quest').
google_font('NTR').
google_font('Neucha').
google_font('Neuton').
google_font('New Rocker').
google_font('News Cycle').
google_font('Niconne').
google_font('Nixie One').
google_font('Nobile').
google_font('Nokora').
google_font('Norican').
google_font('Nosifer').
google_font('Nothing You Could Do').
google_font('Noticia Text').
google_font('Noto Sans').
google_font('Noto Serif').
google_font('Nova Cut').
google_font('Nova Flat').
google_font('Nova Mono').
google_font('Nova Oval').
google_font('Nova Round').
google_font('Nova Script').
google_font('Nova Slim').
google_font('Nova Square').
google_font('Numans').
google_font('Nunito').
google_font('Odor Mean Chey').
google_font('Offside').
google_font('Old Standard TT').
google_font('Oldenburg').
google_font('Oleo Script').
google_font('Oleo Script Swash Caps').
google_font('Open Sans').
google_font('Open Sans Condensed').
google_font('Oranienbaum').
google_font('Orbitron').
google_font('Oregano').
google_font('Orienta').
google_font('Original Surfer').
google_font('Oswald').
google_font('Over the Rainbow').
google_font('Overlock').
google_font('Overlock SC').
google_font('Ovo').
google_font('Oxygen').
google_font('Oxygen Mono').
google_font('PT Mono').
google_font('PT Sans').
google_font('PT Sans Caption').
google_font('PT Sans Narrow').
google_font('PT Serif').
google_font('PT Serif Caption').
google_font('Pacifico').
google_font('Palanquin').
google_font('Palanquin Dark').
google_font('Paprika').
google_font('Parisienne').
google_font('Passero One').
google_font('Passion One').
google_font('Pathway Gothic One').
google_font('Patrick Hand').
google_font('Patrick Hand SC').
google_font('Patua One').
google_font('Paytone One').
google_font('Peddana').
google_font('Peralta').
google_font('Permanent Marker').
google_font('Petit Formal Script').
google_font('Petrona').
google_font('Philosopher').
google_font('Piedra').
google_font('Pinyon Script').
google_font('Pirata One').
google_font('Plaster').
google_font('Play').
google_font('Playball').
google_font('Playfair Display').
google_font('Playfair Display SC').
google_font('Podkova').
google_font('Poiret One').
google_font('Poller One').
google_font('Poly').
google_font('Pompiere').
google_font('Pontano Sans').
google_font('Port Lligat Sans').
google_font('Port Lligat Slab').
google_font('Pragati Narrow').
google_font('Prata').
google_font('Preahvihear').
google_font('Press Start 2P').
google_font('Princess Sofia').
google_font('Prociono').
google_font('Prosto One').
google_font('Puritan').
google_font('Purple Purse').
google_font('Quando').
google_font('Quantico').
google_font('Quattrocento').
google_font('Quattrocento Sans').
google_font('Questrial').
google_font('Quicksand').
google_font('Quintessential').
google_font('Qwigley').
google_font('Racing Sans One').
google_font('Radley').
google_font('Rajdhani').
google_font('Raleway').
google_font('Raleway Dots').
google_font('Ramabhadra').
google_font('Ramaraja').
google_font('Rambla').
google_font('Rammetto One').
google_font('Ranchers').
google_font('Rancho').
google_font('Ranga').
google_font('Rationale').
google_font('Ravi Prakash').
google_font('Redressed').
google_font('Reenie Beanie').
google_font('Revalia').
google_font('Ribeye').
google_font('Ribeye Marrow').
google_font('Righteous').
google_font('Risque').
google_font('Roboto').
google_font('Roboto Condensed').
google_font('Roboto Slab').
google_font('Rochester').
google_font('Rock Salt').
google_font('Rokkitt').
google_font('Romanesco').
google_font('Ropa Sans').
google_font('Rosario').
google_font('Rosarivo').
google_font('Rouge Script').
google_font('Rozha One').
google_font('Rubik Mono One').
google_font('Rubik One').
google_font('Ruda').
google_font('Rufina').
google_font('Ruge Boogie').
google_font('Ruluko').
google_font('Rum Raisin').
google_font('Ruslan Display').
google_font('Russo One').
google_font('Ruthie').
google_font('Rye').
google_font('Sacramento').
google_font('Sail').
google_font('Salsa').
google_font('Sanchez').
google_font('Sancreek').
google_font('Sansita One').
google_font('Sarina').
google_font('Sarpanch').
google_font('Satisfy').
google_font('Scada').
google_font('Scheherazade').
google_font('Schoolbell').
google_font('Seaweed Script').
google_font('Sevillana').
google_font('Seymour One').
google_font('Shadows Into Light').
google_font('Shadows Into Light Two').
google_font('Shanti').
google_font('Share').
google_font('Share Tech').
google_font('Share Tech Mono').
google_font('Shojumaru').
google_font('Short Stack').
google_font('Siemreap').
google_font('Sigmar One').
google_font('Signika').
google_font('Signika Negative').
google_font('Simonetta').
google_font('Sintony').
google_font('Sirin Stencil').
google_font('Six Caps').
google_font('Skranji').
google_font('Slabo 13px').
google_font('Slabo 27px').
google_font('Slackey').
google_font('Smokum').
google_font('Smythe').
google_font('Sniglet').
google_font('Snippet').
google_font('Snowburst One').
google_font('Sofadi One').
google_font('Sofia').
google_font('Sonsie One').
google_font('Sorts Mill Goudy').
google_font('Source Code Pro').
google_font('Source Sans Pro').
google_font('Source Serif Pro').
google_font('Special Elite').
google_font('Spicy Rice').
google_font('Spinnaker').
google_font('Spirax').
google_font('Squada One').
google_font('Sree Krushnadevaraya').
google_font('Stalemate').
google_font('Stalinist One').
google_font('Stardos Stencil').
google_font('Stint Ultra Condensed').
google_font('Stint Ultra Expanded').
google_font('Stoke').
google_font('Strait').
google_font('Sue Ellen Francisco').
google_font('Sunshiney').
google_font('Supermercado One').
google_font('Suranna').
google_font('Suravaram').
google_font('Suwannaphum').
google_font('Swanky and Moo Moo').
google_font('Syncopate').
google_font('Tangerine').
google_font('Taprom').
google_font('Tauri').
google_font('Teko').
google_font('Telex').
google_font('Tenali Ramakrishna').
google_font('Tenor Sans').
google_font('Text Me One').
google_font('The Girl Next Door').
google_font('Tienne').
google_font('Timmana').
google_font('Tinos').
google_font('Titan One').
google_font('Titillium Web').
google_font('Trade Winds').
google_font('Trocchi').
google_font('Trochut').
google_font('Trykker').
google_font('Tulpen One').
google_font('Ubuntu').
google_font('Ubuntu Condensed').
google_font('Ubuntu Mono').
google_font('Ultra').
google_font('Uncial Antiqua').
google_font('Underdog').
google_font('Unica One').
google_font('UnifrakturCook').
google_font('UnifrakturMaguntia').
google_font('Unkempt').
google_font('Unlock').
google_font('Unna').
google_font('VT323').
google_font('Vampiro One').
google_font('Varela').
google_font('Varela Round').
google_font('Vast Shadow').
google_font('Vesper Libre').
google_font('Vibur').
google_font('Vidaloka').
google_font('Viga').
google_font('Voces').
google_font('Volkhov').
google_font('Vollkorn').
google_font('Voltaire').
google_font('Waiting for the Sunrise').
google_font('Wallpoet').
google_font('Walter Turncoat').
google_font('Warnes').
google_font('Wellfleet').
google_font('Wendy One').
google_font('Wire One').
google_font('Yanone Kaffeesatz').
google_font('Yellowtail').
google_font('Yeseva One').
google_font('Yesteryear').
google_font('Zeyada').

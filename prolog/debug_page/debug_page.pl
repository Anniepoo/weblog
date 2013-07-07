:- module(debug_page, []).
/**   <module>  Debugging Page

Displays generally useful debug information.

Note that this can be a security hole. If you load
this module, root(debugpage) is loaded as a priority
-10 handler. To hide it, you'll need to define a 404
handler at a higher priority.

   ==
   :- http_handler(root(debugpage) ,
		http_404([index(root(.))]),
		[priority(10)]).
   ==

 At the moment does little, eventually should include
 whatever's in

 http://www.swi-prolog.org/pldoc/doc/home/vnc/prolog/src/plweb/stats.pl
 since this code's not integrated


 * List of registered handlers and paths (done)
 * Current File Search Path
 * Current Session info
 * Info from [[stats.pl]]  done

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(weblog(formatting/wl_table)).
:- use_module(weblog(resources/resources)).
:- use_module(library(http/http_path)).
:- use_module(weblog(nav/accordion)).

:- http_handler(root(debugpage) , debug_page_handler, [priority(-10), id(debug_page)]).

debug_page_handler(_Request) :-
	reply_html_page(
	    title('Debugging Page'),
	    \debug_contents).

% dont use abox here, want to keep this separated
debug_contents -->
	html([
	      \html_requires(css('demo.css')),
	      h2('Runtime Statistics'),
	      \runtime_stats
	     ]).

runtime_stats -->
	{
	    with_output_to(atom(Settings), list_settings)
	},
	html([
	    style(
		'td   {
		     font-size: 80%;
		     font-face: sans;
		      }'),
	    \accordion([], [
	       \accordion_section('Handlers',
		  \wl_table(handler_info_cells, [
		       columns([path, absolute_path, action, is_prefix, options]),
		       header(debug_page:handler_headers)])),
	       \accordion_section('Settings',
	          pre(Settings)),
	       \accordion_section('Stats/2',
	          \wl_table(rt_stats_cells,
		      [columns([key, value, desc]),
		      header(debug_page:rt_stats_headers)])),
	       \accordion_section('Prolog Flags',
	          \wl_table(flags_cells,
		      [columns([key, value]),
		       header(debug_page:rt_stats_headers)]))
		       ])
	     ]).

handler_headers(path, 'Path').
handler_headers(absolute_path, 'Absolute Path').
handler_headers(is_prefix, 'prefix?').
handler_headers(options, 'Options').

handler_info_cells(Path, path, Path) :-
	http_dispatch:handler(Path, _, _, _).
handler_info_cells(Path, absolute_path, AbsPath) :-
	http_dispatch:handler(Path, _, _, _),
	http_absolute_location(Path, AbsPath, []).
handler_info_cells(Path, action, Action) :-
	http_dispatch:handler(Path, A, _, _),
	format(atom(Action), '~w', [A]).
handler_info_cells(Path, is_prefix, '') :-
	http_dispatch:handler(Path, _, false, _).
handler_info_cells(Path, is_prefix, b('PREFIX')) :-
	http_dispatch:handler(Path, _, true, _).
handler_info_cells(Path, options, X) :-
	http_dispatch:handler(Path, _, _, Options),
	format(atom(X), '~w', [Options]).

rt_stats_headers(key, 'Item').
rt_stats_headers(value, 'Current').
rt_stats_headers(desc, 'Description').

rt_stats_cells(Key, key, Key) :-
	stats_entries(Stats),
	member(Key-_, Stats).
rt_stats_cells(Key, value, OutValue) :-
	ground(Key),
	catch(
	    statistics(Key, Value),
	    error(domain_error(_, _), _),
	    format(atom(Value), 'n/a', [])
		    ),
	human_value(Value, OutValue).

rt_stats_cells(Key, desc, Desc) :-
	stats_entries(Stats),
	member(Key-Desc, Stats).

human_value(Value, OutValue) :-
	float(Value),!,
	format(atom(OutValue), '~4g', [Value]).
human_value(Value, OutValue) :-
	integer(Value),!,
	format(atom(OutValue), '~D', [Value]).
human_value(Value, Value).

stats_entries([
'agc'-'Number of atom garbage collections performed',
'agc_gained'-'Number of atoms removed',
'agc_time'-'Time spent in atom garbage collections',
'process_cputime'-'(User) CPU time since Prolog was started in seconds',
'cputime'-'(User) CPU time since thread was started in seconds',
'inferences'-'Total number of passes via the call and redo ports since Prolog was started',
'heapused'-'Bytes of heap in use by Prolog (0 if not maintained)',
'heap_gc'-'Number of heap garbage collections performed. Only provided if SWI-Prolog is configured with Boehm-GC. See also garbage_collect_heap/0.',
'c_stack'-'System (C-) stack limit. 0 if not known.',
'stack'-'Total memory in use for stacks in all threads',
'local'-'Allocated size of the local stack in bytes',
'localused'-'Number of bytes in use on the local stack',
'locallimit'-'Size to which the local stack is allowed to grow',
'local_shifts'-'Number of local stack expansions',
'global'-'Allocated size of the global stack in bytes',
'globalused'-'Number of bytes in use on the global stack',
'globallimit'-'Size to which the global stack is allowed to grow',
'global_shifts'-'Number of global stack expansions',
'trail'-'Allocated size of the trail stack in bytes',
'trailused'-'Number of bytes in use on the trail stack',
'traillimit'-'Size to which the trail stack is allowed to grow',
'trail_shifts'-'Number of trail stack expansions',
'shift_time'-'Time spent in stack-shifts',
'atoms'-'Total number of defined atoms',
'functors'-'Total number of defined name/arity pairs',
'clauses'-'Total number of clauses in the program',
'modules'-'Total number of defined modules',
'codes'-'Total size of (virtual) executable code in words',
'threads'-'MT-version: number of active threads',
'threads_created'-'MT-version: number of created threads',
'thread_cputime'-'MT-version: seconds CPU time used by finished threads. Supported on Windows-NT and later, Linux and possibly a few more. Verify it gives plausible results before using.'
		      ]).

flags_cells(Key, key, Key) :-
	current_prolog_flag(Key, _).
flags_cells(Key, value, DisplayValue) :-
	current_prolog_flag(Key, Value),
	format(atom(DisplayValue), '~w', [Value]).

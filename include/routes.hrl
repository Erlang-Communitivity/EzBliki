
-record(route,{
	action,		% a unique identifier atom for this route that 
				% represents the route's goal, as a verb
				
	method,		% one of HTTP methods

	pattern,	% A string the represent a path, possibly with vars between {}

	model_factory, 	% Module to create model used by renderer
					% Must export a function 
					%    model_for/2
					% which takes the following args:
					%    ParsedRoute, Options (one of which should be {yaws, Arg})
					% and outputs a proplist
					
	no_render, % set this to true to skip rendering
	
	view_name,	% the string naming a view
	
	renderer,	% Module to render view given model produced by
				% model factory. Must export a function
				%   render/4
				% which takes the following args:
				%   ProcessedRoute, Model, AppmodArgs, Options 
				% and output matches that expected of yaws appmod
				
	options 	% A proplist of options regarding how this route is handled
	}).
	
-record(route_part_static, {
	text	% A string to match a segment against
	}).
	
-record(route_part_var, {
	name,		% the variable name
	regex=".*",	% the pattern that the variable matches
	value="" 	% the value for this variable after route parsing
	}).
	
-record(parsed_route,{
	route, % the route record from which the parts were passed
	parts	% List of route parts
	}).
	

	
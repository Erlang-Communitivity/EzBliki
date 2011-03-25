-module(bliki.templates).
-export([compile/1, compile/2, render/1, render/2]).

-author("=Bill.Barnhill").
-created("2008-Feb-25").
-copyright("Communitivity,Inc.").
-license("Mozilla Public License 1.1").

-import(error_logger).
-import(filename).
-import(erlydtl_compiler).

compile(TemplatePath) ->
	compile(TemplatePath, []).
	
compile(TemplatePath, CompileVars) ->
	Name = filename:rootname(filename:basename(TemplatePath)),
	Module = "template_" ++ Name,
	Options = [
           {vars, CompileVars}, 
           {force_recompile, true}
		],
	case erlydtl_compiler:compile(TemplatePath, Module, Options) of
		ok -> {ok, list_to_atom(Module)};
		Other -> Other
	end.	

render(TemplateId) ->
	render(TemplateId, []).
	
render(TemplateId, RenderVars) ->
	error_logger:info_msg("templates:render/2: ~p", [RenderVars]),
	{ok, Val} = TemplateId:render(RenderVars),
	{html, Val}.

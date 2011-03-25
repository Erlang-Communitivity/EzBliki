{application, atom_app,
 [{description, "Atom Store backed by ETS"},
  {vsn, "1.0"},
  {modules, [bliki.app.atom.atom_app, bliki.app.atom.atom_sup, bliki.app.atom.atom]},
  {registered, [atom]},
  {applications, [kernel, stdlib]},
  {mod, {bliki.app.atom.atom_app, []}},
  {env, [
	{timeout, 3000},
	{conf_module, bliki_conf} 
	]}
 ]}.
        
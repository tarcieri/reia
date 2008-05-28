-module(reia_atom).
-export([funcall/3]).

funcall(nil,   to_s, []) -> 
  {string, <<"nil">>};
funcall(true,  to_s, []) -> 
  {string, <<"true">>};
funcall(false, to_s, []) -> 
  {string, <<"false">>};
funcall(Atom, to_s, []) when is_atom(Atom) ->
  String = atom_to_list(Atom),
  Result = case regexp:match(String, "^[A-Za-z0-9_]+$") of
    nomatch -> "~'" ++ String ++ "'";
    _       -> "~" ++ String
  end,
  reia_list:funcall(reia_erl:e2r(Result), to_string, []).
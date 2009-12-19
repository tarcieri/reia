% Records for Reia core types which are not native to Erlang
% These are hopefully temporary and will be replaced by immutable objects

-record(reia_string, {members = []}).
-record(reia_regexp,  {pattern}).
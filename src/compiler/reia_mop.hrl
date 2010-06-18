%
% reia_mop: Definitions of the Reia metaobject protocol (kinda)
% Copyright (C)2010 Tony Arcieri
%
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-define(self(Line),  (#var{line=Line, name='__reia_self'})).
-define(ivars(Line), (#var{line=Line, name='__reia_ivars'})).
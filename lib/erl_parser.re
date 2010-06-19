#
# erl_parser.re: Parser for Erlang
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# Parser for Erlang
module ErlParser
  # Parse a given string of Erlang source code, returning a parse tree
  def parse(string)
    case erl.erl_scan.string(string.to_list())
    when (:ok, tokens, _)
    when (:error, error, _)
      throw(SyntaxError, error.to_s())
    end
    
    case erl.erl_parse.parse_exprs(tokens)
    when (:ok, exprs)
      exprs
    when (:error, (_, _, error))
      throw(SyntaxError, error.flatten().to_string())
    end
  end
end
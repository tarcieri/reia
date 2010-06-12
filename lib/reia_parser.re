#
# reia_parser.re: Parser for Reia
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# Parser for Reia
module ReiaParser
  # Parse a given string of Reia source code, returning a parse tree
  def parse(string)
    # FIXME: Gee, wouldn't it be cool if I were written in Reia?
    erl.reia.parse(string.to_list())
  end
end
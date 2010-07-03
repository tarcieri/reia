#
# json_parser.re: Parser for JavaScript Object Notation (JSON)
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module JsonParser
  def parse(string)
    transform(erl.reia_json.json_to_term(string.to_list()))
  end
  
  #########
  #private#
  #########
  
  def transform(term)
    case term.class()
    when List
      [transform(element) for element in term]
    when Tuple
      [(transform(key), transform(value)) for (key, value) in term[0]].to_dict()
    when Binary
      (:reia_string, term)
    when Atom
      throw("unexpected atom from JSON parser: #{term}") unless term == :null
      nil
    else term
    end
  end
end

#
# Atom: Methods for the Atom pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Atom    
  def funcall(term, :to_s, [])
    term.to_list().to_string().sub(/^:/, '')
  end
    
  def funcall(nil, :inspect, [])
    "nil"
  end

  def funcall(true, :inspect, [])
    "true"
  end

  def funcall(false, :inspect, [])
    "false"
  end

  def funcall(term, :to_list, [])
    erlang::atom_to_list(term)
  end
    
  def funcall(term, :inspect, [])
    atom = term.to_s()
    case /^[A-Za-z0-9_]+$/.match(atom)
    when nil
      ":'#{atom}'"
    when _
      ":#{atom}"
    end
  end
end
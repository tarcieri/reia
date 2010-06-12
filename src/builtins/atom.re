#
# atom.re: Methods of the Atom builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Atom
  def class; Atom; end
  
  def to_string
    erl.atom_to_list(self).to_string()
  end
  
  def to_module
    (:reia_module, self)
  end
  
  def to_s
    to_string()
  end
  
  def inspect
    name = to_s()
    
    case erl.re.run(erl.atom_to_list(self), "^[A-Za-z0-9_]+$".to_list())
    when (match, _)
      ":#{name}"
    when nomatch
      ":'#{name}'"
    end
  end
end

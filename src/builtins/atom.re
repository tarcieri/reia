#
# atom.re: Methods of the Atom builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Atom
  def call(fake_self, :to_string, args, block)
    fake_self.to_s()
  end
  
  def call(fake_self, :to_s, args, block)
    erl.atom_to_list(fake_self).to_string()
  end
  
  def call(fake_self, :inspect, args, block)
    name = erl.atom_to_list(fake_self).to_string()
    
    case erl.re.run(erl.atom_to_list(fake_self), "^[A-Za-z0-9_]+$".to_list())
    when (match, _)
      ":#{name}"
    when nomatch
      ":'#{name}'"
    end
  end
end

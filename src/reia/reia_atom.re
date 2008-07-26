#
# reia_atom: Methods for the Atom pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module ReiaAtom
  def funcall(nil, ~to_s, [])
    "nil"
    
  def funcall(true, ~to_s, [])
    "true"
    
  def funcall(false, ~to_s, [])
    "false"
    
  def funcall(term, ~to_s, [])
    atom = erlang::atom_to_list(term).to_string()
    case /^[A-Za-z0-9_]+$/.match(atom)
      nil:
        ["~'", atom, "'"].join()
      _:
        ["~", atom].join()
#
# Atom: Methods for the Atom pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Atom    
  def funcall(term, ~to_s, [])
    erlang::atom_to_list(term).to_string()
    
  def funcall(nil, ~inspect, [])
    "nil"

  def funcall(true, ~inspect, [])
    "true"

  def funcall(false, ~inspect, [])
    "false"
    
  def funcall(term, ~inspect, [])
    atom = erlang::atom_to_list(term).to_string()
    case /^[A-Za-z0-9_]+$/.match(atom)
      nil:
        ["~'", atom, "'"].join()
      _:
        ["~", atom].join()
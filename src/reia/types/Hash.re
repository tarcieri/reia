#
# Hash: Methods for the Hash pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Hash
  # Hash#[]
  #   Retrieve an element from a Hash
  def funcall(term, ~'[]', [key])
    (~dict, hash) = term.to_internal()
    case dict::find(key, hash)
      (~ok, value):
        value
      ~error:
        nil
  
  # Hash#insert
  #   Insert an element into a hash, returning a new hash
  def funcall(term, ~insert, [key, value])
    (~dict, hash) = term.to_internal()
    dict::store(key, value, hash)

  # Hash#to_list
  #   Convert a hash to a list of its key/value pairs
  def funcall(term, ~to_list, [])
    (~dict, hash) = term.to_internal()
    dict::to_list(hash)
  
  # Hash#to_s
  #   Convert a hash to a string representation
  def funcall(hash, ~to_s, [])
    funcall(hash, ~inspect, [])
        
  # Hash#inspect
  #   Inspect the contents of a hash
  def funcall(hash, ~inspect, [])
    members = hash.to_list().map { |(k, v)| [k.inspect(), v.inspect()].join(":") }
    ["{", members.join(","), "}"].join()
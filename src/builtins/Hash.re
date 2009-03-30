#
# Hash: Methods for the Hash builtin
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Hash
  # Hash#[]
  #   Retrieve an element from a Hash
  def funcall(term, :'[]', [key])
    (:dict, hash) = term.uninternalize()
    case dict::find(key, hash)
    when (:ok, value)
      value
    when :error
      nil
    end
  end
  
  # Hash#insert
  #   Insert an element into a hash, returning a new hash
  def funcall(term, :insert, [key, value])
    (:dict, hash) = term.uninternalize()
    dict::store(key, value, hash)
  end
  
  # Hash#remove
  #   Remove an element from a hash, returning a new hash
  def funcall(term, :remove, [key])
    (:dict, hash) = term.uninternalize()
    dict::erase(key, hash)
  end
  
  # Hash#keys
  #   Returns a list of all keys from a hash
  def funcall(term, :keys, [])
    (:dict, hash) = term.uninternalize()
    dict::fetch_keys(hash)
  end
  
  # Hash#has
  #   Checks if a key is present in a hash
  def funcall(term, :has, [key])
    (:dict, hash) = term.uninternalize()
    dict::is_key(key, hash)
  end
  
  # Hash#size
  #   Returns the number of pairs in a hash
  def funcall(term, :size, [])
    (:dict, hash) = term.uninternalize()
    dict::size(hash)
  end

  # Hash#to_list
  #   Convert a hash to a list of its key/value pairs
  def funcall(term, :to_list, [])
    (:dict, hash) = term.uninternalize()
    dict::to_list(hash)
  end
  
  # Hash#to_s
  #   Convert a hash to a string representation
  def funcall(hash, :to_s, [])
    funcall(hash, :inspect, [])
  end
        
  # Hash#inspect
  #   Inspect the contents of a hash
  def funcall(hash, :inspect, [])
    members = hash.to_list().map { |(k, v)| "#{k.inspect()}:#{v.inspect()}" }.join(",")
    "{#{members}}"
  end
end
#
# Map: Methods for the Map builtin
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Map
  # Map#[]
  #   Retrieve an element from a Map
  def funcall(term, :'[]', [key])
    (:dict, map) = term.uninternalize()
    case dict::find(key, map)
    when (:ok, value)
      value
    when :error
      nil
    end
  end
  
  # Map#add
  #   Insert an element into a map, returning a new map
  def funcall(term, :add, [key, value])
    (:dict, map) = term.uninternalize()
    dict::store(key, value, map)
  end
  
  # Map#insert
  #   Alias for Map#add
  def funcall(term, :insert, [key, value])
    (:dict, map) = term.uninternalize()
    dict::store(key, value, map)
  end
  
  # Map#remove
  #   Remove an element from a map, returning a new map
  def funcall(term, :remove, [key])
    (:dict, map) = term.uninternalize()
    dict::erase(key, map)
  end
  
  # Map#keys
  #   Returns a list of all keys from a map
  def funcall(term, :keys, [])
    (:dict, map) = term.uninternalize()
    dict::fetch_keys(map)
  end
  
  # Map#has
  #   Checks if a key is present in a map
  def funcall(term, :has, [key])
    (:dict, map) = term.uninternalize()
    dict::is_key(key, map)
  end
  
  # Map#size
  #   Returns the number of pairs in a map
  def funcall(term, :size, [])
    (:dict, map) = term.uninternalize()
    dict::size(map)
  end

  # Map#to_list
  #   Convert a map to a list of its key/value pairs
  def funcall(term, :to_list, [])
    (:dict, map) = term.uninternalize()
    dict::to_list(map)
  end
  
  # Map#to_s
  #   Convert a map to a string representation
  def funcall(map, :to_s, [])
    funcall(map, :inspect, [])
  end
        
  # Map#inspect
  #   Inspect the contents of a map
  def funcall(map, :inspect, [])
    members = map.to_list().map { |(k, v)| "#{k.inspect()}=>#{v.inspect()}" }.join(",")
    "{#{members}}"
  end
end
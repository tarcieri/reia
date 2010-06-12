#
# dict.re: Methods of the Dict builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Dict
  def class; Dict; end
  
  def [](key)
    case erl.dict.find(key, self)
    when (:ok, value)
      value
    end
  end
  
  def []=(key, value)
    erl.dict.store(key, value, self)
  end
  
  def to_list
    erl.dict.to_list(self)
  end
  
  def to_s
    members = ["#{k.inspect()}=>#{v.inspect()}" for (k, v) in to_list()]
    "{#{members.join(',')}}"
  end
  
  def inspect
    to_s()
  end
  
  def size
    erl.dict.size(self)
  end
  
  def keys
    erl.dict.fetch_keys(self)
  end
end

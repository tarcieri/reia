#
# dict.re: Methods of the Dict builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Dict
  def call(self, :'[]', (key,), block)
    case erl.dict.find(key, self)
    when (:ok, value)
      value
    end
  end
  
  def call(self, :'[]=', (key, value), block)
    erl.dict.store(key, value, self)
  end
  
  def call(self, :to_list, args, block)
    erl.dict.to_list(self)
  end
  
  def call(self, :to_s, args, block)
    call(self, :inspect, args, block)
  end
  
  def call(self, :inspect, args, block)
    members = ["#{k.inspect()}=>#{v.inspect()}" for (k, v) in call(self, :to_list, nil, nil)]
    "{#{members.join(',')}}"
  end
  
  def call(self, :size, args, block)
    erl.dict.size(self)
  end
  
  def call(self, :keys, args, block)
    erl.dict.fetch_keys(self)
  end
end
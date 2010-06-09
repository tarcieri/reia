#
# dict.re: Methods of the Dict builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Dict
  def call(fake_self, :'[]', (key,), block)
    case erl.dict.find(key, fake_self)
    when (:ok, value)
      value
    end
  end
  
  def call(fake_self, :'[]=', (key, value), block)
    erl.dict.store(key, value, fake_self)
  end
  
  def call(fake_self, :to_list, args, block)
    erl.dict.to_list(fake_self)
  end
  
  def call(fake_self, :to_s, args, block)
    call(fake_self, :inspect, args, block)
  end
  
  def call(fake_self, :inspect, args, block)
    members = ["#{k.inspect()}=>#{v.inspect()}" for (k, v) in call(fake_self, :to_list, nil, nil)]
    "{#{members.join(',')}}"
  end
  
  def call(fake_self, :size, args, block)
    erl.dict.size(fake_self)
  end
  
  def call(fake_self, :keys, args, block)
    erl.dict.fetch_keys(fake_self)
  end
end

#
# time.re: Objects which represent particular points in time
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Time
  def initialize
    (megasecs, secs, @microseconds) = erl.now()
    @seconds = megasecs * 1000000 + secs
  end
  
  def seconds; @seconds; end
  def microseconds; @microseconds; end
  
  def to_i
    @seconds
  end
  
  def to_f
    @seconds + @microseconds / 1000000
  end
  
  def inspect
    secs = @seconds % 1000000
    megasecs = ((@seconds - secs) / 1000000).to_i()
    erl_time = (megasecs, secs, @microseconds)
    
    (date, time) = erl.calendar.now_to_local_time(erl_time)
    (year, month, day) = date
    (hour, minute, second) = time
    
    "#<Time #{year}/#{month}/#{day} #{hour}:#{minute}:#{second}>"
  end
end

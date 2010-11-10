#
# TestHelper: Helper functions for Reia's test framework
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module TestHelper  
  def expect(group, description, &block)
    (expected, actual) = block()
    if expected == actual
      ".".print()
      :ok
    else
      "F".print()
      (:error, group, description, expected, actual)
    end
  end
  
  def duration(started_at, finished_at)
    started_seconds  = (started_at[0]  * 1000000 + started_at[1]  + started_at[2] * 0.000001)
    finished_seconds = (finished_at[0] * 1000000 + finished_at[1] + finished_at[2] * 0.000001)
    finished_seconds - started_seconds
  end
end
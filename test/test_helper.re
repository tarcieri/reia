#
# TestHelper: Helper functions for Reia's test framework
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module TestHelper  
  def expect(group, description, lambda)
    (expected, actual) = lambda()
    if expected == actual
      ".".print()
      ~ok
    else
      "F".print()
      (~error, group, description, expected, actual)
    end
  end
end
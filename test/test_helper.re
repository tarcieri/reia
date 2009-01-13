#
# TestHelper: Helper functions for Reia's test framework
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module TestHelper  
  def expect(group, description, lambda)
    io::format("- ~s: ".to_list(), [description.to_list()])
    (expected, actual) = lambda()
    if expected == actual
      Local.puts("ok")
      true
    else
      Local.puts("FAILED")
      io::format("  expected: ~s, actual: ~s~n".to_list(), [expected.to_s().to_list(), actual.to_s().to_list()])
      false
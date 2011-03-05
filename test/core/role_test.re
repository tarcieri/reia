#
# role_test.re: tests for Reia's actor roles
# Copyright (C)2011 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

role CharlieSheen
  def initialize
    @winning = true
  end
  
  def winning?; @winning; end
  
  def star_in_two_and_a_half_men
    @winning = false
  end
  
  def do_some_hookers_and_blow
    @winning = true
  end
end
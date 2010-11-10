#
# FunrefsTest: Tests for Reia's function references
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module PlusTwoModule
  def calc(n)
    n + 2
  end
end

module FunrefsTest
  def run
    #[module_test()]
    []
  end
  
  # generate for module functions
  # FIXME: funrefs as yet unimplemented
#  def module_test
#    TestHelper.expect("Refs", "generate for module functions") do
#      ref = PlusTwoModule.calc
#      (42, ref(40))
#    end
#  end  
end
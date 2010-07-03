#
# ExceptionTest: Tests for Reia's exceptions
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class MyException < StandardError; end

module ExceptionTest
  def run
    [exception_test()]
  end

  def exception_test
    TestHelper.expect("Exceptions", "are catchable") do
      res = try
        throw(MyException, "OHAI!")
        :FAIL
      catch ex
        ex
      end
      
      ((MyException, "OHAI!"), (res.class(), res.message()))
    end
  end
end
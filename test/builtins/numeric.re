#
# NumericTest: Tests for Reia's numeric type
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module NumericTest
  def run
    [
      compare_test(), 
      int_to_list_test(), 
      float_to_list_test(), 
      int_to_string_test(), 
      float_to_string_test(), 
      str_to_float_test(), 
      str_to_int_test()
    ]
  end
  
  def compare_test
    TestHelper.expect(Numeric, "ints and floats compare") do
      (1, 1.0)
    end
  end
  
  def int_to_list_test()
    TestHelper.expect(Numeric, "converts ints to list") do
      (172.to_list(), [49,55,50])
    end
  end
  
  def float_to_list_test()
    TestHelper.expect(Numeric, "converts floats to list") do
      (3.1415.to_list(), [51,46,49,52,49,53,48,48])
    end
  end
  
  def int_to_string_test()
    TestHelper.expect(Numeric, "converts ints to string") do
      (42.to_string(), "42")
    end
  end
  
  def float_to_string_test()
    TestHelper.expect(Numeric, "converts floats to string") do
      (1.2.to_string(), "1.200000")
    end
  end
  
  def str_to_float_test()
    TestHelper.expect(Numeric, "string converts to float") do
      ("100.1234".to_float(), 100.1234)
    end
  end
  
  def str_to_int_test()
    TestHelper.expect(Numeric, "string converts to int") do
      ("19283".to_int(), 19283)
    end
  end
end
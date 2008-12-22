#
# NumericTest: Tests for Reia's numeric type
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module NumericTest
  def run
    Local.puts("Numeric")

    compare_test()
    to_list_test()
    str_to_float_test()
    str_to_int_test()

  def compare_test
    TestHelper.expect("ints and floats compare", fun do
      (1, 1.0)
    )

  def to_list_test()
    TestHelper.expect("converts to list", fun do
      (172.to_list(), [49,55,50])
    )

  def str_to_float_test()
    TestHelper.expect("string converts to float", fun do
      ("100.1234".to_float(), 100.1234)
    )

  def str_to_int_test()
    TestHelper.expect("string converts to int", fun do
      ("19283".to_int(), 19283)
    )

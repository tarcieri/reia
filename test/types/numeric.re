
module NumericTest
  def run
    Local.puts("Numeric")

    compare_test()
    to_list_test()

  def compare_test
    TestHelper.expect("ints and floats compare", fun do
      (1, 1.0)
    )

  def to_list_test()
    TestHelper.expect("converts to list", fun do
      (172.to_list(), [49,55,50])
    )

module HashTest
  def run
    Local.puts("Hash")
    index_test()
    compare_test()
    
  def index_test
    TestHelper.expect("index recieves proper value", fun do
      ({~foo: "bar"}[~foo], "bar")
    )

  def compare_test
    TestHelper.expect("comparison is equal", fun do
      ({~foo: "bar"}, {~foo: "bar"})
    )

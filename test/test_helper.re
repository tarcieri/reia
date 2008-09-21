module TestHelper
  def assert_equal(reason, expected, actual)
    io::format("- ~s: ".to_list(), [reason.to_list()])
    if expected == actual
      Local.puts("ok")
      true
    else
      Local.puts("FAILED")
      io::format("  expected: ~s, actual: ~s~n".to_list(), [expected.to_s().to_list(), actual.to_s().to_list()])
      false
  
  def expect(description, lambda)
    io::format("- ~s: ".to_list(), [description.to_list()])
    (expected, actual) = lambda()
    if expected == actual
      Local.puts("ok")
      true
    else
      Local.puts("FAILED")
      io::format("  expected: ~s, actual: ~s~n".to_list(), [expected.to_s().to_list(), actual.to_s().to_list()])
      false
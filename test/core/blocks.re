module BlockTest
  def run
    [standalone_test(), argument_test()]
  end
  
  # accepts blocks when the only argument
  def standalone_test
    TestHelper.expect("Blocks", "are accepted as the only function argument", fun do
      (42, just_a_block_alone { |n| n * 2 })
    end)
  end
  
  # accepts blocks with other arguments
  def argument_test
    TestHelper.expect("Blocks", "can be passed alongside other arguments", fun do
      (42, blocks_with_args(20, 2) { |n| n * 2 })
    end)
  end
  
  def just_a_block_alone(&block)
    block(20) + 2
  end
  
  def block_with_args(arg1, arg2, &block)
    block(arg1) + arg2
  end
end
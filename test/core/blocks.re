class BlocksClass
  def block_method_test(&block)
    block(10) + 22
  end
  
  def block_method_argument_test(arg1, arg2, &block)
    (block(arg1) + 1) * arg2
  end
end

module BlocksTest
  def run
    [
      standalone_test(), 
      argument_test(), 
      method_standalone_test(), 
      method_argument_test()
    ]
  end
  
  # accepts blocks when the only argument
  def standalone_test
    TestHelper.expect("Blocks", "are accepted as the only function argument") do
      (42, just_a_block_alone { |n| n * 2 })
    end
  end
  
  # accepts blocks with other arguments
  def argument_test
    TestHelper.expect("Blocks", "can be passed alongside other arguments") do
      (42, block_with_args(20, 2) { |n| n * 2 })
    end
  end
  
  # work with methods
  def method_standalone_test
    TestHelper.expect("Blocks", "work with methods") do
      obj = BlocksClass()
      (42, obj.block_method_test { |n| n * 2 })
    end
  end
  
  # work with methods and other arguments
  def method_argument_test
    TestHelper.expect("Blocks", "work with methods and other arguments") do
      obj = BlocksClass()
      (42, obj.block_method_argument_test(3, 6) { |n| n * 2 })
    end
  end
  
  def just_a_block_alone(&block)
    block(20) + 2
  end
  
  def block_with_args(arg1, arg2, &block)
    block(arg1) + arg2
  end
end
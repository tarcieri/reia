#
# BlocksTest: Tests for Reia's blocks type
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module PlusTwo
  def calc(n)
    n + 2
  end
end

module BlocksTest
  def run
    [
#      standalone_test(), 
      argument_test()
      #lambda_as_block_test()
    ]
  end
  
  # accepts blocks when the only argument
#  def standalone_test
#    TestHelper.expect("Blocks", "are accepted as the only function argument") do
#      (42, just_a_block_alone { |n| n * 2 })
#    end
#  end
  
  # accepts blocks with other arguments
  def argument_test
    TestHelper.expect("Blocks", "can be passed alongside other arguments") do
      (42, block_with_args(20, 2) { |n| n * 2 })
    end
  end
    
  # allows lambdas to be passed as blocks
  # FIXME: broken!!!!!
#  def lambda_as_block_test
#    TestHelper.expect("Blocks", "can utilize lambdas") do
#      lambda = PlusTwo.calc
#      (42, block_with_args(18, 22, &lambda))
#    end
#  end
 
# FIXME: naked block captures not supported!  
#  def just_a_block_alone(&block)
#    block(20) + 2
#  end
  
  def block_with_args(arg1, arg2, &block)
    block(arg1) + arg2
  end
end
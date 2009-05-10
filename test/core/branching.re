module BranchingTest
  def run
    [basic_if_test()]
  end
  
  def basic_if_test
    TestHelper.expect("The 'if' statement", "takes true branches") do
      result = if true
        1
      end
      (1, 1)
    end
  end  
end
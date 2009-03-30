module PlusTwoModule
  def calc(n)
    n + 2
  end
end

class PlusTwoClass
  def calc(n)
    n + 2
  end
end

module FunrefsTest
  def run
    [module_test(), object_test()]
  end
  
  # generate for module functions
  def module_test
    TestHelper.expect("Refs", "generate for module functions") do
      ref = PlusTwoModule.calc
      (42, ref(40))
    end
  end
  
  # generate for class methods
  def object_test
    TestHelper.expect("Refs", "generate for class methods") do
      obj = PlusTwoClass()
      ref = obj.calc
      (42, ref(40))
    end
  end
end
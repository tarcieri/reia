module ListTest
  def run
    Local.puts("List")
    
    Local.print("- retrieves the nth element correctly: ")
    Local.puts(nth_test().to_s())
    
    Local.print("- reverses the order of a list: ")
    Local.puts(nth_test().to_s())
    
  # retrieves the nth element correctly
  def nth_test
    list = [1,2,3,4,5]
    list[0] == 1 and list[1] == 2 and list[4] == 5
    
  # reverses the order of a list
  def reverse_test
    list = [1,2,3]
    list.reverse() == [3,2,1]
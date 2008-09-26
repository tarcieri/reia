load("test/test_helper.re")

# List test
load("test/types/list.re")
ListTest.run()
puts("")

# Tuple test
load("test/types/tuple.re")
TupleTest.run()

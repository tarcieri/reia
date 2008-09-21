load("test/test_helper.re")

# List test
load("test/core/list.re")
ListTest.run()
puts("")

# Tuple test
load("test/core/tuple.re")
TupleTest.run()
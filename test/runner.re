#
# runner: Runner for Reia's test suite
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

load("test/test_helper.re")

# List test
load("test/types/list.re")
ListTest.run()
puts("")

# Tuple test
load("test/types/tuple.re")
TupleTest.run()
puts("")

# Regex test
load("test/types/regex.re")
RegexTest.run()
puts("")

# Numeric test
load("test/types/numeric.re")
NumericTest.run()
puts("")

# Hash test
load("test/types/hash.re")
HashTest.run()
puts("")

# Object test
load("test/core/object.re")
ObjectTest.run()

#
# runner: Runner for Reia's test suite
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

load("test/test_helper.re")

# List test
load("test/types/list.re")
r1 = ListTest.run()
puts("")

# Tuple test
load("test/types/tuple.re")
r2 = TupleTest.run()
puts("")

# Regex test
load("test/types/regex.re")
r3 = RegexTest.run()
puts("")

# Numeric test
load("test/types/numeric.re")
r4 = NumericTest.run()
puts("")

# Hash test
load("test/types/hash.re")
r5 = HashTest.run()
puts("")

# Atom test
load("test/types/atom.re")
r6 = AtomTest.run()
puts("")

# Object test
load("test/core/object.re")
r7 = ObjectTest.run()
puts("")

# File test
load("test/lib/file.re")
r8 = FileTest.run()
puts("")

results = [r1, r2, r3, r4, r5, r6, r7, r8].flatten()
failures = [false|false in results]
puts([results.size(), " assertions, ", failures.size(), " failures"].join())
erlang::halt(1) if failures.size() > 0
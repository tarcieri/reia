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

# Tuple test
load("test/types/tuple.re")
r2 = TupleTest.run()

# Regex test
load("test/types/regex.re")
r3 = RegexTest.run()

# Numeric test
load("test/types/numeric.re")
r4 = NumericTest.run()

# Hash test
load("test/types/hash.re")
r5 = HashTest.run()

# Atom test
load("test/types/atom.re")
r6 = AtomTest.run()

# Object test
load("test/core/object.re")
r7 = ObjectTest.run()

# File test
load("test/lib/file.re")
r8 = FileTest.run()

# File test
load("test/lib/dir.re")
r9 = DirTest.run()

puts("\n")

results = [r1, r2, r3, r4, r5, r6, r7, r8, r9].flatten()
failures = [error|error = (~error, _, _, _, _) in results]
failures.each { |(~error, group, description, expected, actual)| puts(["'", group, " ", description, "' FAILED"].join()); puts(["expected ", expected, ", actual ", actual, "\n"].join()) }

puts([results.size(), " assertions, ", failures.size(), " failures"].join())
System.halt(1) if failures.size() > 0
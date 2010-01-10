#
# RegexpTest: Tests for Reia's regular expression type
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module RegexpTest
  def run
    [
      #match_all(),
      #split(),
      #sub(),
      #gsub(),
      #match_one(), 
      #to_string(), 
      #inspect()
    ]
  end

# FIXME: Regular expression literals are ambiguous with the / and /= tokens  
#  def match_one
#    TestHelper.expect(Regexp, "matches a single occurence") do
#      string = "(a,b,c,d,e)"
#      ("a,", /.,/.match(string))
#    end
#  end
#  
#  def match_all
#    TestHelper.expect(Regexp, "matches all occurrences") do
#      string = "(a,b,c,d,e)"
#      (["a,","b,","c,","d,"], /.,/.matches(string))
#    end
#  end
#  
#  def split
#    TestHelper.expect(Regexp, "splits a string") do
#      string = "a,b,c,d,e"
#      (["a","b","c","d","e"], /,/.split(string))
#    end
#  end
#  
#  def sub
#    TestHelper.expect(Regexp, "substitution") do
#      string = "a,b,c,d,e"
#      ("a12b,c,d,e", /,/.sub(string, "12"))
#    end
#  end
#  
#  def gsub
#    TestHelper.expect(Regexp, "multiple substitution") do
#      string = "a,b,c,d,e"
#      ("a12b12c12d12e", /,/.gsub(string, "12"))
#    end
#  end
#  
#  # converts to a string
#  def to_string
#    TestHelper.expect(Regexp, "converts to a string") do
#      ("/.,abc/", /.,abc/.inspect())
#    end
#  end
#  
#  def inspect
#    TestHelper.expect(Regexp, "inspect returns valid regexp syntax") do
#      ("/.,abc/", /.,abc/.inspect())
#    end
#  end
end

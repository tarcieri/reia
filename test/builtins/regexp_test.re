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
      to_string(), 
      inspect()
    ]
  end

  def match_one
    TestHelper.expect(Regexp, "matches a single occurence") do
      string = "(a,b,c,d,e)"
      ("a,", %r/.,/.match(string))
    end
  end
  
  def match_all
    TestHelper.expect(Regexp, "matches all occurrences") do
      string = "(a,b,c,d,e)"
      (["a,","b,","c,","d,"], %r/.,/.matches(string))
    end
  end
  
  def split
    TestHelper.expect(Regexp, "splits a string") do
      string = "a,b,c,d,e"
      (["a","b","c","d","e"], %r/,/.split(string))
    end
  end
  
  def sub
    TestHelper.expect(Regexp, "substitution") do
      string = "a,b,c,d,e"
      ("a12b,c,d,e", %r/,/.sub(string, "12"))
    end
  end
  
  def gsub
    TestHelper.expect(Regexp, "multiple substitution") do
      string = "a,b,c,d,e"
      ("a12b12c12d12e", %r/,/.gsub(string, "12"))
    end
  end
  
  # converts to a string
  def to_string
    TestHelper.expect(Regexp, "converts to a string") do
      ("%r/.,abc/", %r/.,abc/.inspect())
    end
  end
  
  def inspect
    TestHelper.expect(Regexp, "inspect returns valid regexp syntax") do
      ("%r/.,abc/", %r/.,abc/.inspect())
    end
  end
end

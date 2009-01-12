#
# RegexTest: Tests for Reia's regular expression type
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module RegexTest
  def run
    Local.puts("Regex")
    
    #match_all()
    #split()
    #sub()
    #gsub()
    [match_one(), to_string(), inspect()]
    
  def match_one
    TestHelper.expect("match a single occurence", fun do
      string = "(a,b,c,d,e)"
      ("a,", /.,/.match(string))
    )

  def match_all
    TestHelper.expect("match all occurencies", fun do
      string = "(a,b,c,d,e)"
      (["a,","b,","c,","d,"], /.,/.matches(string))
    )

  def split
    TestHelper.expect("splits a string", fun do
      string = "a,b,c,d,e"
      (["a","b","c","d","e"], /,/.split(string))
    )

  def sub
    TestHelper.expect("substitution", fun do
      string = "a,b,c,d,e"
      ("a12b,c,d,e", /,/.sub(string, "12"))
    )

  def gsub
    TestHelper.expect("multiple substitution", fun do
      string = "a,b,c,d,e"
      ("a12b12c12d12e", /,/.gsub(string, "12"))
    )
  
  # converts to a string
  def to_string
    TestHelper.expect("converts to a string", fun do
      ("/.,abc/", /.,abc/.inspect())
    )
  
  def inspect
    TestHelper.expect("inspect returns valid regexp syntax", fun do
      ("/.,abc/", /.,abc/.inspect())
    )

class Object
  def initialize
    nil
    
  def to_s
    ["#<", class(), ">"].join()
    
  def inspect
    to_s()
    
  def method_missing(method, args)
    throw (~error, (method, "undefined"))
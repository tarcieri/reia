class Object
  def initialize
    nil
    
  def class
    Object
    
  def to_s
    ["#<", class(), ">"].join()
    
  def inspect
    to_s()
    
  def _(method, args)
    throw (~error, (method, "undefined"))
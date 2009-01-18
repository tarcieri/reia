class Object
  def initialize
    nil
    
  def to_s
    ["#<", class(), ">"].join()
    
  def inspect
    to_s()
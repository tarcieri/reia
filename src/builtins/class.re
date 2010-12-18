# The Reia metaclass
class Class
  # Create a new instance of this class
  def new; Class(); end
  
  # Name of this class as a string
  def name; "Class"; end
  
  # String representation of this class
  def to_s; name(); end
  
  # Methods of this class
  def methods; []; end
end
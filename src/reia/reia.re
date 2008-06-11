module Reia
  def start
    reia::read_input([])
    
  def start(_args)
    reia::read_input([])

  def read_input(data)
    case io::get_line(~'')
      ~eof:
        input = data.map { |s| s.to_string() }.join()
        reia_eval::string(input)
        erlang::halt(0)
      string:
        reia::read_input(data.push(string))
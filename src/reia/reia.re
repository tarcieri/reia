module Reia
  def start
    read_input([])
    
  def start(args)
    (~ok, input) = file::read_file(args[0])
    reia_eval::string(input.to_string())
    erlang::halt(0)

  def read_input(data)
    case io::get_line(~'')
      ~eof:
        input = data.map { |s| s.to_string() }.join()
        reia_eval::string(input)
        erlang::halt(0)
      string:
        read_input(data.push(string))
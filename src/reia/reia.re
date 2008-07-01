module Reia
  def start
    read_input([])
    
  def start(args)
    (~ok, input) = file::read_file(args[0])
    eval_input(input.to_string())

  def read_input(data)
    case io::get_line(~'')
      ~eof:
        input = data.map { |s| s.to_string() }.join()
        eval_input(input)
      string:
        read_input(data.push(string))
  
  def eval_input(input)
    reia_eval::string(input)
    erlang::halt(0)
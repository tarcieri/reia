module ReiaRegexp
  def funcall(regexp, ~to_list, [])
    (~regexp, bin) = regexp.to_internal()
    erlang::binary_to_list(bin)
    
  def funcall(regexp, ~to_string, [])
    regexp.to_list().to_string()
    
  def funcall(regexp, ~to_s, [])
    ["/", regexp.to_string(), "/"].join()
  
  def funcall(regexp, ~match, [string])
    list = string.to_list()
    case regexp::match(list, regexp.to_list())
      (~match, start, len):
        lists::sublist(list, start, len).to_string()
      ~nomatch:
        nil
        
  def funcall(regexp, ~matches, [string])
    list = string.to_list()
    (~match, matches) = regexp::matches(list, regexp.to_list())
    matches.map { |(start, len)| lists::sublist(list, start, len).to_string() }

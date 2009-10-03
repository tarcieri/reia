require 'rake/clean'

task :default => %w(check_erl_version check_previous_install build)

#
# Prerequisites
#

# Returns the installed Erlang version
def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d\.\d$/)
  unless version
   STDERR.puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

# Retrieve the directory Erlang libraries are stored in
def erl_lib_dir
  `erl -noshell -eval "io:format(code:lib_dir())" -s init stop`
end

# Directory to install Reia into
def reia_install_dir
  File.join(erl_lib_dir, 'reia', '')
end

# Ensure the version of Erlang installed is recent enough
task :check_erl_version do
  print "Checking Erlang version... "
  version = erlang_version
  
  if version >= "5.6.3"
    puts "#{version} (ok)"
  else
    puts "#{version} (too old)"
    puts "Sorry, the version of Erlang you have installed is too old to run Reia"
    puts "Reia requires a minimum Erlang version of R12B-3 (5.6.3)"
    puts "Please see http://wiki.reia-lang.org/wiki/Building#Prerequisites"
    exit 1
  end
end

# Ensure Reia was not previously installed
task :check_previous_install do
  if File.exists?(reia_install_dir)
    puts "*** WARNING: Previous installation of Reia detected"
    puts "*** If you experience problems during the build process please try"
    puts "*** running 'rake uninstall' before proceeding"
  end
end

# Generate an output path for the given input file
def output_file(input_file, dir = 'ebin/')
  dir + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

ERL_SRC = FileList.new('src/{compiler,core}/**/*.erl')
ERL_SRC.each do |input|
  unless output_file(input) == "ebin/reia_parse.beam"
    file output_file(input) => input do
      sh "erlc +debug_info -o ebin #{input}"
    end
  end
end

task :build => %w(neotoma) + ERL_SRC.map { |input_file| output_file(input_file) }

#
# Neotoma (PEG-based parser generator for Erlang)
#

NEOTOMA_SRC  = FileList.new('src/neotoma/src/*.erl')
NEOTOMA_EBIN = 'src/neotoma/ebin/'

output = NEOTOMA_SRC.map { |input| output_file input, NEOTOMA_EBIN }
Rake::FileList[output].each { |file| CLEAN << file }

directory NEOTOMA_EBIN
task :neotoma => [NEOTOMA_EBIN, *output] 

# peg_meta uses a parse transform in peg_transform
task 'src/neotoma/ebin/peg_meta.beam' => 'src/neotoma/ebin/peg_transform.beam'

NEOTOMA_SRC.each do |input|
  file output_file(input, NEOTOMA_EBIN) do
    sh "erlc -W0 -I src -pa #{NEOTOMA_EBIN} -o #{NEOTOMA_EBIN} #{input}"
  end
end

#
# Parser (generated from a Neotoma grammar)
#

PARSER_GRAMMAR = 'src/compiler/reia_parse.peg'
PARSER_SRC     = PARSER_GRAMMAR.sub(/peg$/, 'erl')

CLEAN << PARSER_SRC

task PARSER_SRC => PARSER_GRAMMAR do
  sh "erl -noshell -pa #{NEOTOMA_EBIN} -eval 'peg_gen:file(\"#{PARSER_GRAMMAR}\")' -s init stop"
  src = File.read PARSER_SRC

  # Change the transform function to use the reia_tree module
  src.sub!(/transform(.+?)\s+->\s+Node\./, "transform(Type, Node, Idx) -> reia_tree:transform(Type, Node, Idx).")
  File.open(PARSER_SRC, 'w') { |parser| parser << src }
end

task 'ebin/reia_parse.beam' => PARSER_SRC do
  sh "erlc -I src -pa #{NEOTOMA_EBIN} -o ebin #{PARSER_SRC}"
end
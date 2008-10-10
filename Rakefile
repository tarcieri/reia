task :default => [:check_erl_version, :build, :test]
task :build => [:smerl, :leex, :yecc, :reia, :ebin, :clean]

def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d\.\d$/)
  unless version
   puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

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

def output_file(input_file)
  'ebin/' + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

# Reia
ERL_SRC = FileList.new('src/{compiler,core,types}/**/*.erl')
ERL_SRC.each do |input|
  file output_file(input) => input do
    sh "bin/erlc +debug_info -o artifacts/beam #{input}"
  end
end

REIA_SRC = FileList.new('src/**/*.re')
REIA_SRC.each do |input|
  output = output_file(input)
  file output => input do
    sh "bin/reiac -o artifacts/beam/#{File.basename(output, ".re")} #{input}"
  end
end

PARSER_SRC = FileList.new('src/**/*.{xrl,yrl}')

task :reia => (ERL_SRC + REIA_SRC + PARSER_SRC).map { |input_file| output_file(input_file) }

=begin
# Smart exceptions
SMEX_SRC = FileList['src/smart_exceptions/*.erl']
SMEX_SRC.each do |input|
  file output_file(input) => input do
    sh "erlc -W0 -o ebin #{input}"
  end
end

task :smart_exceptions => SMEX_SRC.map { |input_file| output_file(input_file) }
=end

# Smerl (Simple Metaprogramming for Erlang)
task :smerl => "ebin/smerl.beam"

file "ebin/smerl.beam" => "src/smerl/smerl.erl" do
  sh "erlc -W0 -o ebin src/smerl/smerl.erl"
end

# Leex (lexer generator for Erlang)
task :leex => ["ebin/leex.beam", "ebin/reia_scan.beam"]

file "ebin/leex.beam" => "src/leex/leex.erl" do
  sh "erlc -W0 -o ebin src/leex/leex.erl"
end

# Compile reia_scan using leex
file "ebin/reia_scan.beam" => %w[ebin/leex.beam src/compiler/reia_scan.xrl] do
  sh "bin/leex src/compiler/reia_scan.xrl"
  mv "src/compiler/reia_scan.erl", "artifacts/erl/reia_scan.erl"
  sh "erlc +debug_info +nowarn_unused_vars -o artifacts/beam artifacts/erl/reia_scan.erl"
end

task :yecc => "ebin/reia_parse.beam"

# Compile reia_parse using yecc
file "ebin/reia_parse.beam" => "src/compiler/reia_parse.yrl" do
  sh "bin/yecc src/compiler/reia_parse.yrl"
  mv "src/compiler/reia_parse.erl", "artifacts/erl/reia_parse.erl"
  sh "erlc +debug_info -o ebin artifacts/erl/reia_parse.erl"
end

# Copy all output BEAM files into the ebin directory
task :ebin do
  FileList["artifacts/beam/*.beam"].each { |file| cp file, "ebin" }
end

task :test do
  sh "bin/reia test/runner.re"
end

task :clean do
  FileList['artifacts/**/*.{erl,beam}'].each { |f| rm_f f }
end

task :distclean => :clean do
  FileList['ebin/**/*.beam'].each { |f| rm_f f }
end


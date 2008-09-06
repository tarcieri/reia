task :default => [:build, :test]
task :build => [:smerl, :leex, :smart_exceptions, :reia, :ebin]

def output_file(input_file)
  'ebin/' + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

# Reia
ERL_SRC = FileList.new('src/reia/**/*.erl')
ERL_SRC.each do |input|
  file output_file(input) => input do
    sh "bin/erlc +debug_info -o artifacts/beam #{input}"
  end
end

REIA_SRC = FileList.new('src/reia/**/*.re')
REIA_SRC.each do |input|
  output = output_file(input)
  file output => input do
    sh "bin/reiac -o artifacts/beam/#{File.basename(output, ".re")}.beam #{input}"
  end
end

task :reia => (ERL_SRC + REIA_SRC).map { |input_file| output_file(input_file) }

# Smart exceptions
SMEX_SRC = FileList['src/smart_exceptions/*.erl']
SMEX_SRC.each do |input|
  file output_file(input) => input do
    sh "erlc -W0 -o ebin #{input}"
  end
end

task :smart_exceptions => SMEX_SRC.map { |input_file| output_file(input_file) }

# Smerl (Simple Metaprogramming for Erlang)
task :smerl => "ebin/smerl.beam"

file "ebin/smerl.beam" => "src/smerl/smerl.erl" do
  sh "erlc -W0 -o ebin src/smerl/smerl.erl"
end

# Leex (lexer generator for Erlang)
task :leex => "ebin/leex.beam"

file "ebin/leex.beam" => "src/leex/leex.erl" do
  sh "erlc -W0 -o ebin src/leex/leex.erl"
end

# Compile reia_scan using leex
file "artifacts/erl/reia_scan.erl" => %w[ebin/leex.beam src/reia/compiler/reia_scan.xrl] do
  sh "bin/leex src/reia/compiler/reia_scan.xrl"
  mv "src/reia/compiler/reia_scan.erl", "artifacts/erl/reia_scan.erl"
end

file "ebin/reia_scan.beam" => "artifacts/erl/reia_scan.erl" do
  sh "erlc +debug_info +nowarn_unused_vars -o artifacts/beam src/reia/compiler/reia_scan.erl"
end

# Compile reia_parse using yecc
file "artifacts/erl/reia_parse.erl" => "src/reia/compiler/reia_parse.yrl" do
  sh "bin/yecc src/reia/compiler/reia_parse.yrl"
  mv "src/reia/compiler/reia_parse.erl", "artifacts/erl/reia_parse.erl"
end

file "ebin/reia_parse.beam" => "src/reia/compiler/reia_parse.erl" do
  sh "erlc +debug_info -o ebin src/reia/compiler/reia_parse.erl"
end

# Copy all output BEAM files into the ebin directory
task :ebin do
  FileList["artifacts/*.beam"].each { |file| cp file, "ebin" }
end

task :test do
  sh "bin/reia test/runner.re"
end

task :clean do
  FileList['artifacts/**/*.{erl,beam}'].each { |f| rm_f f }
end

task :default => :build
task :build => %w[compiler ire copy_ebin]

rule ".beam" => ".erl" do |t|
  sh "erlc +nowarn_unused_vars -o #{File.dirname(t.name)} #{t.source}"
end

task :compiler => %w[
  src/reia/reia_scan.beam 
  src/reia/reia_parse.beam
  src/reia/reia_compiler.beam
  src/reia/reia_operators.beam
  src/reia/reia_dispatch.beam
  src/reia/reia_erl.beam
  src/reia/reia_eval.beam
  src/reia/reia_list.beam
  src/reia/reia_numeric.beam
  src/reia/reia_atom.beam
  src/reia/reia_regexp.beam
]

task :ire => "src/reia/ire.beam"

# Compile leex
file "src/leex/leex.beam" => "src/leex/leex.erl"

# Compile reia_scan using leex
file "src/reia/reia_scan.erl" => %w[src/reia/reia_scan.xrl src/leex/leex.beam] do
  sh "erl -eval 'leex:file(\"src/reia/reia_scan.xrl\")' -pa src/leex -noshell -s init stop"
end

# Compile reia_parse using yecc
file "src/reia/reia_parse.erl" => "src/reia/reia_parse.yrl" do
  sh "erl -eval 'yecc:file(\"src/reia/reia_parse.yrl\")' -noshell -s init stop"
end

# Create the ebin directory if it doesn't exist
directory "ebin"

# Copy all output BEAM files into the ebin directory
task "copy_ebin" => "ebin" do
  FileList["src/reia/*.beam"].each do |file|
    cp file, "ebin"
  end
end

task :clean do
  rm_f "src/leex/leex.beam"
  rm_f "src/reia/reia_scan.erl"
  rm_f "src/reia/reia_scan.beam"
  rm_f "src/reia/reia_parse.erl"
  rm_f "src/reia/reia_parse.beam"
  rm_f "src/reia/reia_compiler.beam"
  rm_f "src/reia/reia_operators.beam"
  rm_f "src/reia/reia_dispatch.beam"
  rm_f "src/reia/reia_erl.beam"
  rm_f "src/reia/reia_eval.beam"
  rm_f "src/reia/reia_list.beam"
  rm_f "src/reia/reia_numeric.beam"
  rm_f "src/reia/reia_atom.beam"
  rm_f "src/reia/reia_regexp.beam"
  rm_f "src/reia/ire.beam"
end

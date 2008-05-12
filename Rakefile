task :default => :build
task :build => :compiler

rule ".beam" => ".erl" do |t|
  sh "erlc +nowarn_unused_vars -o #{File.dirname(t.name)} #{t.source}"
end

task :compiler => %w[src/reia/reia_scan.beam src/reia/reia_parse.beam]

# Compile leex
file "src/leex/leex.beam" => "src/leex/leex.erl"

# Compile reia_scan using leex
file "src/reia/reia_scan.beam" => "src/reia/reia_scan.erl"
file "src/reia/reia_scan.erl" => %w[src/reia/reia_scan.xrl src/leex/leex.beam] do
  sh "erl -eval 'leex:file(\"src/reia/reia_scan.xrl\")' -pa src/leex -noshell -s init stop"
end

# Compile reia_parse using yecc
file "src/reia/reia_parse.beam" => "src/reia/reia_parse.erl"
file "src/reia/reia_parse.erl" => "src/reia/reia_parse.yrl" do
  sh "erl -eval 'yecc:file(\"src/reia/reia_parse.yrl\")' -noshell -s init stop"
end

task :clean do
  rm_f 'src/leex/leex.beam'
  rm_f 'src/reia/reia_scan.erl'
  rm_f 'src/reia/reia_scan.beam'
  rm_f 'src/reia/reia_parse.erl'
  rm_f 'src/reia/reia_parse.beam'
end

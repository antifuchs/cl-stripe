require 'clucumber'
begin
  puts "Launching..."
  (@main_clucumber = ClucumberSubprocess.launch(File.expand_path("../", File.dirname(__FILE__)))).listen <<-LISP
      (load #p"#{File.expand_path("../../cl-stripe.asd", File.dirname(__FILE__))}")
      (ql:quickload :cl-stripe)
  LISP
  STDERR.puts(@main_clucumber.output)
rescue PTY::ChildExited => e
  STDERR.puts "Failure: #{@main_clucumber.inspect}: #{e.inspect}!"
  STDERR.puts(@main_clucumber && @main_clucumber.output)
  exit 1
end

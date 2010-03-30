#!/usr/bin/ruby -Ke

#require "getoptlong"
#require "tempfile"
#require "fold"
#require "dia/parser"

puts "¡ú¡ú¡ú¡ú orcred2ps start "
RED2PS = "red2ps"
LPR = "lpr"

if ARGV.size < 4
  usage
  exit 1
end
if ARGV.size == 10
  $printer = ARGV[7]
else
  $printer = "lp1"   
end
puts "ARG7= [" + $printer + "]"

$red_file = ARGV[0]
$in_file = ARGV[1]
x_const = ARGV[2]
$offset_x = ARGV[3]
y_const = ARGV[4]
$offset_y = ARGV[5]
p_const = ARGV[6]
$printer = ARGV[7]
o_const = ARGV[8]
$output_ps = ARGV[9]
$layer = ARGV[10..-1].join(" ")

puts "ARG1= [" + $red_file + "]"
puts "ARG2= [" + $in_file + "]"
puts "ARG3= [" + x_const + "]"
puts "ARG4= [" + $offset_x + "]"
puts "ARG5= [" + y_const + "]"
puts "ARG6= [" + $offset_y + "]"
puts "ARG7= [" + p_const + "]"
puts "ARG8= [" + $printer + "]"
puts "ARG9= [" + o_const + "]"
puts "ARG10= [" + $output_ps + "]"
puts "ARG11= [" + $layer + "]"

if $output_ps
  exec_red2ps=Array.new
  exec_red2ps << RED2PS
  exec_red2ps << $red_file
  exec_red2ps << $in_file
  exec_red2ps << "-x"
  exec_red2ps << $offset_x
  exec_red2ps << "-y"
  exec_red2ps << $offset_y 
  exec_red2ps << "-o"
  exec_red2ps << $output_ps 
  exec_red2ps << $layer    

  system(exec_red2ps.join(" "))
  if $printer != "----------"
puts "print start"
  	system(LPR, "-P", $printer, $output_ps)
#  	system(LPR, "-P", $printer, $output_ps.path)
  end
end

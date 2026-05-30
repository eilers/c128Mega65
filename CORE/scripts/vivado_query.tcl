# Query project state without launching synthesis (safe alongside GUI session).
set project_file [lindex $argv 0]
if {$project_file eq ""} {
    puts "Usage: vivado -mode batch -source vivado_query.tcl -tclargs <project.xpr>"
    exit 1
}

open_project $project_file
puts "Project: [get_property NAME [current_project]]"
puts "Part:    [get_property PART [current_project]]"
puts "Top:     [get_property top [current_fileset]]"
puts "Sources: [llength [get_files -of_objects [get_filesets sources_1]]] files in sources_1"
close_project
exit 0

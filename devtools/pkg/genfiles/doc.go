// Package genfiles reads a repository's checked-in list of generated files
// (generated_files.yaml): which files are generated, matched with
// .gitattributes glob rules, and the commands that regenerate them.
//
// Commands are argument lists run with no shell. A plain string in the list
// is always literal; a value from the matched file enters only through a
// {var: NAME} or {join: [...]} mapping, so a file name can never become
// shell syntax, a program, or more than one argument.
package genfiles

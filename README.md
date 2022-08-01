hrows
=====

A family of programs to work with text files structured as a record per line.

Introduction
============

This is a collection of programs that work with text files that are structured
as a series of records, one per line. The fields in each record are separated
by tabs, but this is just an option. We call this files *listatab files*.

The programs are:

+ hrows: a graphical front end to work with listata files. It allows the
  editing of files and automatic update of fields usign formulas. At the
  moment, only in Spanish.

+ vrows: a TUI front end for visualizing the text fields of a listatab file.

+ cols: a command line utility to extract columns from listatab files. It can use
  formulas to create new columns.

+ listing: a command line utility to create listing of grades from a listatab file.

File Format
===========

The files processed by the programs are text files in which each line
is interpreted as a record. The fields of the record are separated by
a special character (the *separator*) that can be set using the flags:
`-s` for the input and `-S` for the output. So, you can read  a
semicolon file with `-s";"`. Or you can convert a semicolon separated
file to a comma separated one using `-s";"` together with `-S,`.

In case the separator is included in the string, this is surrounded by
quotes.

Additionally, the file can have a header to specify the names and
(maybe) the types of each field. This header can have two forms:

+ A first line that begins with `#` and has the names of each field
  surrounded by `<>` like

      #<Name><Points>

  In these case, the type of the field can be added separating it with
  a vertical bar (`|`):

      #<Name|TypeString><Points|TypeInt>

  You should not need to edit this comment, it will be created by the
  programs.

+ A first line that has the format of the rest of the lines. This line
  contains the names of the fields. Use the flag `-1` to indicate that
  the first line is to be used as header.

If in the same directory of the file there exists a file with the same
name and the extension `.conf`, it is read and used to recover the
information about the fields. This configuration holds additional
information like the expressions used to calculate derived fields.

Pending
=======

+ Finishing this `README.md`:
  + Documenting the syntax of the formulas.
  + Better explanation of the programs.

+ Translating `hrows` to English.


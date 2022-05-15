hrows
=====

A family of programs to work with text files structured as a record per line.

Introduction
============

This is a collection of progams that work with text files that are structured
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

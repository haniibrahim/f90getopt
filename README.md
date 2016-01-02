f90getopt
=========

getopt()- and getopt_long()-like functionality (similar to the C-functions) for Fortran 90. Based on sources from [Mark Gates](http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-2.2.1/src/getopt.F90).

##Purpose:

Parsing command-line options and arguments like:

   <pre>myapp -xv --longarg --arg=5.0 -p 9</pre>

##Features:

  * Short option without argument (e.g.: -x)
  * Short option with argument (e.g.: -p 9 or -p9)
  * Short options w/o arguments can be embraced (e.g.: -xv)
  * Long option w/o argument (e.g: --longarg)
  * Long option w/ arument (e.g.: --arg 5.0), Equal sign (e.g.: --arg=5,0)

##Requirements
Fortran 90 compiler which offers Fortran 2003 features *command_argument_count()* and *get_command_argument()*.

##License

GNU Public License v. 2.0

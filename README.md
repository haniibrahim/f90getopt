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
  * Long option w/ argument (e.g.: --arg 5.0), Equal sign (e.g.: --arg=5.0)

##Requirements
Fortran 90 compiler which offers Fortran 2003 features *command_argument_count()* and *get_command_argument()*.

## Example
```
program f90getopt_sample
    use f90getopt
    implicit none
    !character:: ch ! Unused: ch=getopt()

    ! START For longopts only
    type(option_s):: opts(2)
    opts(1) = option_s( "alpha", .false., 'a' )
    opts(2) = option_s( "beta",  .true.,  'b' )
    ! END longopts

      do
        select case( getopt( "ab:", opts ) ) ! opts is optional (for longopts only)
            case( char(0))
                exit
            case( 'a' )
                print *, 'option alpha/a'
            case( 'b' )
                print *, 'option beta/b=', optarg
            case( '?' )
                stop
            case default
                print *, 'unhandled option ', optopt, ' (this is a bug)'
        end select
    end do
end program f90getopt_sample
```

Build the sample program:

```gfortran f90getopt.F90 f90getopt_sample.f90 -o f90getopt_sample```

and run it, e.g.:

```./f90getopt_sample --alpha -b 23.2```

## Userfunction
| Type | Name | Description |
|--------|--------|----|
| function        | getopt(shortopt-string, [opts])  | short |
| global variable | optarg                           | argument of the option |
| global variable | optopt                           | option character, even if it isn't recognized

##License

GNU Public License v. 2.0

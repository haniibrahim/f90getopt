f90getopt
=========

getopt()- and getopt_long()-like functionality (similar to the C-functions) for Fortran 90. Based on sources from [Mark Gates](http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-2.2.1/src/getopt.F90).

## Purpose:

Parsing command-line options and arguments like:

   <pre>myapp -xv --longarg --arg=5.0 -p 9</pre>

## Features:

  * Short option without argument (e.g.: -x)
  * Short option with argument (e.g.: -p 9 or -p9)
  * Short options w/o arguments can be embraced (e.g.: -xv)
  * Long option w/o argument (e.g: --longarg)
  * Long option w/ argument (e.g.: --arg 5.0) and ***NEW***: Equal sign (e.g.: --arg=5.0)

## Requirements
Fortran 90 compiler which offers Fortran 2003 features *command_argument_count()* and *get_command_argument()*. E.g.: gfortran (GNU), g95 (g95.org), ifort (Intel), etc.

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

Put ```f90getopt.f90``` and your sample program from above (let's say ```f90getopt-sample.f90```) in the same directory and change to this directory with the ```cd```command in the terminal (or Windows command prompt). Then type:

```
gfortran f90getopt.F90 f90getopt_sample.f90 -o f90getopt_sample
```

(you can omit `.exe` in `-o f90fetopt_sample` on Windows)

to compile it and run it with, e.g.:

On Unices:

```
./f90getopt_sample --alpha -b 23.2
```

or on Windows:

```
f90getopt_sample --alpha -b 23.2
```
(you can omit `.exe`)


Output is:

```
 option alpha/a
 option beta/b=23.2
```

## Compile and link to (static) library

Change to the directory where the ```f90getopt.F90``` is located and type:

```
gfortran -c f90getopt.F90
ar cr f90getopt.a f90getopt.o
```

You will get a static libray called ```f90getopt.a``` and a module file ```f90getopt.mod```. Move the a-file on UNIX(-like) systems to ```/usr/lib```  (or ```/usr/local/lib``` if supported) and the mod-file to ```/usr/include``` (or ```/usr/local/include``` if supported). On Windows go to the appropriate directory of your compiler-system. MinGW and Cygwin are similar to UNIX. Integrated Development Environments (IDE) have their own rules. Refer the manual of your IDE.

To compile and link the sample program with the libray can be done on Unices with:

```
gfortran -o f90getopt-sample f90getopt-sample.f90 -I/usr/include /usr/lib/f90getopt.a
```

Change paths to match the right ones on Windows.

## Userfunction
| Type | Name | Description |
|--------|--------|----|
| function        | getopt(shortopt-string, [opts])  | short |
| global variable | optarg                           | argument of the option |
| global variable | optopt                           | option character, even if it isn't recognized

## License

GNU Public License v. 2.0

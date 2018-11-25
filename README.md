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
    type(option_s):: opts(3)
    opts(1) = option_s( "alpha", .false., 'a' )
    opts(2) = option_s( "beta",  .true.,  'b' )
    opts(3) = option_s( "help", .false.,  'h')
    ! END longopts

    ! If no options were committed
    if (command_argument_count() .eq. 0 ) then
      write (*,*) "Available Options: -a. --alpha -b X --beta=X --beta X"
    end if

    ! Process options one by one
    do
        select case( getopt( "ahb:", opts ) ) ! opts is optional (for longopts only)
            case( char(0) )
                exit
            case( 'a' )
                print *, 'option alpha/a'
            case( 'b' )
                print *, 'option beta/b=', optarg
            case( 'h' )
                print *, 'help-screen'
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

To avoid copying the source file `f90getopt.F90` to your working directory everytime you want to use their features, it make sense to create a static library. After, you just need to link the library to your project. To do this follow the steps below:

Change to the directory where the ```f90getopt.F90``` is located and type:

```
gfortran -c f90getopt.F90
ar cr libf90getopt.a f90getopt.o
ranlib libf90getopt.a
```

You will get a static libray called ```libf90getopt.a``` and a module file ```f90getopt.mod```. Move the a-file on UNIX(-like) systems to ```/usr/local/lib``` and the mod-file to ```/usr/local/include```: 

```
sudo cp ./libf90getopt.a /usr/local/lib/
sudo cp ./f90getopt.mod /usr/local/include/
```

On Windows go to the appropriate directory of your compiler-system. MinGW and Cygwin are similar to UNIX. Integrated Development Environments (IDE) have their own rules. Refer the manual of your IDE.

To compile and link the sample program with the libray can be done on Unices with:

```
gfortran -o f90getopt-sample f90getopt-sample.f90 -I/usr/local/include -lf90getopt
```

Change paths to match the right ones on Windows.

## Userfunction
| Type            | Name                                 | Brief Description                                                                                                  |
| --------------- | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| function        | getopt(shortopts-string, [longopts]) | Returns short option character & value (if applicable) of all arguments one by one  |
| global variable | optarg                               | value of the current option, e.g. "3" when long argument = --num=3.                                                            |
| global variable | optopt                               | Option  (single) character e.g. "b" when option = -b                                                                 |
Refer example code above for details.

## Differences ...
### ... from C version
- when options are finished, C version returns -1 instead of char(0),  and thus stupidly requires an int instead of a char.
- does not support optreset
- does not support "--" as last argument
- if no argument, optarg is blank, not NULL
- argc and argv are implicit

### ... for long options
- optional argument to getopt(), rather than separate function getopt_long()
- has_arg is logical, and does not support optional_argument
- does not support flag field (and thus always returns val)
- does not support longindex
- knows the length of longopts, so does not need an empty last record

## Changelog 

| Version | Description                                                                                                            |
| ------- | ---------------------------------------------------------------------------------------------------------------------- |
| 0.9.0   | Almost finished. Equal sign recognition in longoption (logopt=arg) is missing for v1.0                                 |
| 1.0.0   | Added support for equal sign (=) in long options, like --beta=2.0. Error messages are displayed on stderr (not stdout) |
| 1.0.1   | Longopt bug fixed and refactoring.                                      |
| 1.0.2   | Bug in README.md fixed                                                  |
|1.0.3    | Bug in README.md (sample code section) fixed |

## License

**GNU Public License v. 2.0**

Copyright (C) 2014  Hani Andreas Ibrahim

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. 

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

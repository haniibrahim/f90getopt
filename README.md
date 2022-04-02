# f90getopt [![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()

getopt()- and getopt_long()-like functionality (similar to the C-functions) for modern Fortran 90/2003 or higher. Based on sources from [Mark Gates](http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-2.2.1/src/getopt.F90).

*f90getopt* is developed as an easy to learn and compact library in just one source file. The `f90getopt.F90` file can just be added to existing source directories and deployed with them without producing dependencies. You can "learn" f90getopt in minutes and therefore it is even suitable for very small projects or "throw-away-code". It follows the GNU and POSIX command-line argument standards.

* [Purpose](#Purpose)
* [Features](#Features)
* [Requirements](#Requirements)
* [Example](#Example)
   * [Build the sample program](#Build-the-sample-program)
   * [Run the sample program](#Run-the-sample-program)
* [Changelog](#Changelog)
* [License](#License)

## Purpose

Parsing command-line options and arguments (GNU & POSIX) like:

   <pre>myapp -xv --longarg --arg=5.0 -p 9</pre>

## Features

  * Easy to learn (in minutes), only [3 user-functions/variables](https://github.com/haniibrahim/f90getopt/wiki/Table-of-user-functions-and-global-variables)
  * Easy to add to your project, just one file
  * GNU and POSIX standard compatible
  * Standard Fortran 2003

Parsing features:

  * Parse short options without argument (e.g.: -x)
  * Parse short options with argument (e.g.: -p 9 or -p9)
  * Parse short options without argumentss and they can be embraced (e.g.: -xv)
  * Parse long option without argument (e.g: --longarg)
  * Parse long option with argument (e.g.: --arg 5.0 or --arg=5.0)

## Requirements

Fortran 2003 or Fortran 90 compiler which offers Fortran 2003 features *command_argument_count()* and *get_command_argument()*. E.g.: gfortran (GNU), g95 (g95.org), ifort (Intel), etc.

## Example

This is a full working example and it make use of long and short options. It is well documented and should answer most questions without referring a manual. If you need further information, go to the [Wiki page](https://github.com/haniibrahim/f90getopt/wiki).

```f90
program f90getopt_sample
    ! Sample program for f90getopt function

    use f90getopt
    implicit none

    ! START for longopts only (optional)
    ! ----------------------------------
    ! option_s derived type:
    !   1st value = long option name (character array, max. 80)
    !   2nd value = if option has value (logical)
    !   3rd value = short option name (single character), same as in getopt()
    ! option_s is not needed if you just use short options
    type(option_s) :: opts(3)
    opts(1) = option_s("alpha", .false., "a")
    opts(2) = option_s("beta",  .true.,  "b")
    opts(3) = option_s("help",  .false., "h")
    ! END longopts


    ! If no options were committed
    ! ----------------------------
    if (command_argument_count() .eq. 0) then
      print*, "ERROR: Program has options: -a. --alpha -b x --beta=x --beta x"
    end if


    ! START Processing options
    ! ------------------------
    ! Process short options one by one and long options if specified in option_s
    !
    ! getopt(optstr, longopt):
    !  - optstr  = character of short option character without a space
    !              ":" after a character says that this option requires a value
    !  - longopt = opts, if specified in option_s (optional)
    do
        select case(getopt("ab:h", opts))
            case(char(0))
                exit
            case("a")
                print*, "option alpha/a"
            case("b")
                print*, "option beta/b=",  trim(optarg) ! "trim" is quite useful to avoid trailing spaces
            case("h")
                print*, "help-screen"
        end select
    end do
    ! END processing options

end program f90getopt_sample
```

### Build the sample program

Put `f90getopt.f90` and your sample program from above (let's say `f90getopt_sample.f90`) in the same directory and change to this directory with the `cd` command in the terminal (or Windows command prompt). Then type:

```
gfortran f90getopt.F90 f90getopt_sample.f90 -o f90getopt_sample
```

(you can omit `.exe` in `-o f90getopt_sample` on Windows)

to compile it.

### Run the sample program

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

## Changelog

| Version | Description                                                                                                            |
| ------- | ---------------------------------------------------------------------------------------------------------------------- |
| 0.9.0   | Almost finished. Equal sign recognition in longoption (logopt=arg) is missing for v1.0                                 |
| 1.0.0   | Added support for equal sign (=) in long options, like --beta=2.0. Error messages are displayed on stderr (not stdout) |
| 1.0.1   | Longopt bug fixed and refactoring.                                                                                     |
| 1.0.2   | Bug in README.md fixed                                                                                                 |
| 1.0.3   | Bug in README.md (sample code section) fixed                                                                           |
| 1.0.4   | Portable declaration of stdin/out/err fixed, minor refactoring and documentation, => GPL 3.0, Wiki page                |

## License

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()

Copyright (C) 2014  Hani Andreas Ibrahim

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

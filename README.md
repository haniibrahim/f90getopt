# f90getopt [![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]() [![Version](https://img.shields.io/badge/version-2.0.0-orange.svg)]()


getopt()- and getopt_long()-like functionality (similar to the C-functions) for modern Fortran 90/2003 or higher. Based on sources from [Mark Gates](http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-2.2.1/src/getopt.F90).

*f90getopt* is developed as an easy to learn and compact library in just one source file. The `f90getopt.F90` file can just be added to existing source directories and deployed with them. There is no hassle with library dependencies. You can "learn" f90getopt in minutes and therefore it is even suitable for very small projects or "throw-away-code". It follows the GNU and POSIX command-line argument standards.

[![](https://img.shields.io/badge/new_in-version_2-orange.svg)]() It can now process options that are **only available as long options** and have no short equivalent, and optionally check for **duplicate options**.

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

  * Easy to learn (in minutes) by just looking at the [sample program](#Example).
  * Easy to add to your project, just one module
  * GNU and POSIX standard compliant
  * Standard Fortran 2003 and cross-platform
  * [Wiki](https://github.com/haniibrahim/f90getopt/wiki)

Parsing features:

  * Parse short options without arguments (e.g.: -x)
  * Parse short options with arguments (e.g.: -p 9 or -p9)
  * Parse short options without arguments and they can be embraced (e.g.: -xv)
  * Parse long options without arguments (e.g: --longarg)
  * Parse long options with arguments (e.g.: --arg 5.0 or --arg=5.0)
  * Check function for numbers (integer, real/double) in the arguments
  * ***NEW*** Parse long-only options without short equivalents with and without arguments
  * ***NEW*** Check optional for duplicate options

## Requirements

Fortran 2003 or Fortran 90 compiler which offers Fortran 2003 features *command_argument_count()* and *get_command_argument()*. E.g.: gfortran (GNU), g95 (g95.org), ifort/ifx (Intel), etc.

## Example

This is a full working example and it make use of long and short options. It is well documented and should answer most questions without referring a manual. If you need further information, go to the [Wiki](https://github.com/haniibrahim/f90getopt/wiki).

```f90
program f90getopt_sample
   ! Sample program for f90getopt module
   use f90getopt
   implicit none

   ! local variables
   character(len=1)           :: c  ! getopt-character

   ! START for shortopts
   ! ----------------------------------
   ! - optshort  = character array of short option characters without a space
   !              colon ":" after a character says that this option requires an argument
   character(len=*),parameter :: optshort = "ho:"
   ! END shortopts

   ! START for longopts (optional)
   ! ----------------------------------
   ! option_s derived type:
   !   1st value = long option name (character array, max. 80)
   !   2nd value = if option has an argument (logical)
   !   3rd value = short option name (single character), same as in getopt()
   !               or "achar(0)" if longopt-only options
   !
   ! option_s is not needed if you just use short options
   type(option_s) :: optlong(4)
   ! longopts w/ short equivalents
   optlong(1) = option_s("help",    .false., "h")
   optlong(2) = option_s("output",  .true.,  "o")
   ! longopt-only w/o short equivalents
   optlong(3) = option_s("zeta",    .true.,  achar(0))
   optlong(4) = option_s("alpha",   .false., achar(0))
   ! END longopts

   ! If no options were passed
   if (command_argument_count() .eq. 0) then
      stop "Program has options: --alpha -h --help -- zeta=x --zeta x -o x --output=x --output x"
   end if


   ! START Processing options
   ! ------------------------
   ! Process short options one by one and long options if specified in option_s
   !
   ! getopt(optshort, optlong):
   !  - optshort = short options declaration, specified in optshort
   !  - optlong  = long options declaration, if specified in option_s (optional)

   ! Check for duplicates in options
   ! NEW IN VERSION 2
   call check_duplicates(optshort, optlong)

   do
      c = getopt(optshort,optlong)

      ! When all options are processed
      if (c == achar(0) .and. optlongind == 0) exit

      if (c /= achar(0)) then
         select case(c)
               ! shortopts w/ or w/o long equivalents
            case("h")
               print *,"HELP triggered"
            case("o")
               print *,"Output file: ", trim(optarg)
         end select
      else
         ! longopts w/o short equivalents
         ! NEW IN VERSION 2
         select case(optlongind)
            case(3)  ! corresponds to optlong(3)
               print *,"ZETA => ",trim(optarg)
            case(4)  ! corresponds to optlong(4)
               print *,"ALPHA triggered"
         end select
      endif
   end do
   ! END processing options
end program
```

### Build the sample program

Put `f90getopt.f90` and your sample program from above (let's say `f90getopt_sample.f90`) in the same directory and change to this directory with the `cd` command in the terminal (or Windows command prompt). Then type:

```
gfortran f90getopt.F90 f90getopt_sample.f90 -o f90getopt_sample
```

(you can omit `.exe` in `-o f90getopt_sample` on Windows) to compile it. Be advised that the capital F90 in the file extention of f90getopt is mandatory.

### Run the sample program

On Unices:

```
./f90getopt_sample -ho test.txt --zeta=15d-3 --alpha
```

or in Windows Powershell:

```
.\f90getopt_sample -h --output test.txt --zeta 15d-3 --alpha
```
(you can omit `.exe`)


Output is:

```
 HELP triggered
 Output file: test.txt
 ZETA => 15d-3
 ALPHA triggered
```

A complete sample program with all features is provided in the [Wiki](https://github.com/haniibrahim/f90getopt/wiki/Full-working-example)

## Changelog

| Version | Description                                                                                                            |
| ------- | ---------------------------------------------------------------------------------------------------------------------- |
| 0.9.0   | Almost finished. Equal sign recognition in longoption (logopt=arg) is missing for v1.0                                 |
| 1.0.0   | Added support for equal sign (=) in long options, like --beta=2.0. Error messages are displayed on stderr (not stdout) |
| 1.0.1   | Longopt bug fixed and refactoring.                                                                                     |
| 1.0.2   | Bug in README.md fixed                                                                                                 |
| 1.0.3   | Bug in README.md (sample code section) fixed                                                                           |
| 1.0.4   | Portable declaration of stdin/out/err fixed, minor refactoring and documentation, => GPL 3.0, Wiki page                |
| 1.1.0   | New utility function "`isnum()`" checks for numbers in option's arguments, More sophisticated sample program in the Wiki |
| 2.0.0   | Parses long-only options (`--longopts`). No more need to specify an short option. Can have an argument or not. Now complete GNU-compatible. New utility subroutine “`check_duplicate()`” checks for duplicate options optionally. |

## License

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()

Copyright (C) 2014  Hani Andreas Ibrahim

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

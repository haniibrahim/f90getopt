! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module f90getopt
!
! ================== Prologue =====================================================================================================
!
! Purpose:
!    getopt()- and getopt_long()-like functionality (similar to the
!    -functions) for Fortran 2003.
!
!
! History:
!    Version   Programmer         Date       Description
!    -------   ----------         ---------- -----------
!    0.8.0     Mark Gates         2014/04/27 Original code from Gates
!    0.9.0     Hani Ibrahim       2014/04/28 Removed non-standard CLI-functions and added standard F2K CLI-functions
!    1.0.0     Hani Ibrahim       2017/01/07 Parse "=" with long options, error messages to stderr not stdout
!    1.0.1     Hani Ibrahim       2017/09/10 longopt bug fixed
!    1.0.2     Hani Ibrahim       2017/09/29 Readme.md error fixed
!    1.0.3     Hani Ibrahim       2018/07/09 Several errors in Readme.md fixed
!    1.0.4     Hani Ibrahim       2022/03/31 Portable declaration of stdin/out/err fixed, refactoring, documentation
!    1.1.0     Hani Ibrahim       2022/04/10 Utility function "isnum()" added
!
! User routines:
!    getopt, isnum
!
! Global variables/types
!    option_s, optarg
!
! Special requirements:
!    Fortran 2003 compliant compiler
!
! ------------------ Use Module / Include files -----------------------------------------------------------------------------------
!
! ------------------ Implicit -----------------------------------------------------------------------------------------------------
   IMPLICIT NONE
! ------------------ Local declarations -------------------------------------------------------------------------------------------
   PUBLIC  :: getopt, option_s, optarg, isnum
   PRIVATE ! all other are private (hidden)
! ------------------ Constant declarations ----------------------------------------------------------------------------------------

    ! Portable declaration of stderr
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
#else
#define stderr 0
#endif

    character(len=80)     :: optarg        ! Option's value
    character             :: optopt        ! Option's character
    integer               :: optind=1      ! Index of the next argument to process
    logical               :: opterr=.true. ! Errors are printed by default. Set opterr=.false. to suppress them

    type option_s
        character(len=80) :: name     ! Name of the option
        logical           :: has_arg  ! Option has an argument (.true./.false.)
        character         :: short    ! Option's short character equal to optopt
    end type option_s

    ! grpind is index of next option within group; always >= 2
    integer, private:: grpind=2

contains

    ! ----------------------------------------

    character function substr( str, i, j )
    ! Return str(i:j) if 1 <= i <= j <= len(str),
    ! else return empty string.
    !
    ! This is needed because Fortran standard allows but doesn't *require* short-circuited
    ! logical AND and OR operators. So this sometimes fails:
    !     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
    ! but this works:
    !     if ( substr(str, i+1, i+1) == ':' ) then

        ! arguments
        character(len=*), intent(in) :: str
        integer, intent(in)          :: i, j

        if ( 1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        endif
    end function substr


    ! ----------------------------------------

    character function getopt( optstring, longopts )
    ! Returns short option character & value (if applicable) of all arguments one by one

        ! arguments
        character(len=*), intent(in)           :: optstring
        type(option_s),   intent(in), optional :: longopts(:)

        ! local variables
        character(len=80)                      :: arg

        optarg = ''
        if ( optind > command_argument_count()) then
            getopt = char(0)
        endif

        call get_command_argument( optind, arg )
        if ( present( longopts ) .and. arg(1:2) == '--' ) then
            getopt = process_long( longopts, arg )
        elseif ( arg(1:1) == '-' ) then
            getopt = process_short( optstring, arg )
        else
            getopt = char(0)
        endif
    end function getopt


    ! ----------------------------------------

    character function process_long( longopts, arg )
    ! Process long options

        ! arguments
        type(option_s),   intent(in) :: longopts(:)
        character(len=*), intent(in) :: arg

        ! local variables
        integer                      :: i = 0
        integer                      :: j = 0
        integer                      :: len_arg = 0             ! length of arg
        logical                      :: has_equalsign = .false. ! arg contains equal sign?

        len_arg = len_trim(arg)

        ! search for equal sign in arg and set flag "has_equalsign" and
        ! length of arg (till equal sign)
        do j=1, len_arg
            if (arg(j:j) == "=") then
                has_equalsign = .true.
                len_arg = j-1
                exit
            endif
        enddo

        ! search for matching long option

        if (.not. has_equalsign) then
            optind = optind + 1
        endif

        do i = 1, size(longopts)
            if ( arg(3:len_arg) == longopts(i)%name ) then
                optopt = longopts(i)%short
                process_long = optopt
                if ( longopts(i)%has_arg ) then
                    if (has_equalsign) then ! long option has equal sign between value and option
                        if (arg(len_arg+2:) == '') then ! no value (len_arg+2 value after "="
                            write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                            process_long=char(0) ! Option not valid
                        else
                            call get_command_argument(optind, optarg)
                            optarg = optarg(len_arg+2:)
                            optind = optind + 1
                        endif
                    else ! long option has no equal sign between value and option
                        if ( optind <= command_argument_count()) then
                            call get_command_argument( optind, optarg )
                            optind = optind + 1
                        elseif ( opterr ) then
                            write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                            process_long=char(0) ! Option not valid
                        endif
                    endif
                endif
                return
            endif
        end do
        ! else not found
        process_long = char(0)
        optopt='?'
        if ( opterr ) then
            write(stderr, '(a,a,a)') "ERROR: Unrecognized option '", arg(1:len_arg), "'"
        endif
        return
    end function process_long


    ! ----------------------------------------

    character function process_short( optstring, arg )
    ! Process short options

        ! arguments
        character(len=*), intent(in) :: optstring, arg

        ! local variables
        integer                      :: i, arglen

        arglen = len( trim( arg ))
        optopt = arg(grpind:grpind)
        process_short = optopt

        i = index( optstring, optopt )
        if ( i == 0 ) then
            ! unrecognised option
            process_short = '?'
            if ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Unrecognized option '-", optopt, "'"
            endif
        endif
        if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
            ! required argument
            optind = optind + 1
            if ( arglen > grpind ) then
                ! -xarg, return remainder of arg
                optarg = arg(grpind+1:arglen)
            elseif ( optind <= command_argument_count()) then
                ! -x arg, return next arg
                call get_command_argument( optind, optarg )
                optind = optind + 1
            elseif ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Option '-", optopt, "' requires a value"
                process_short = char(0) ! Option not valid
            endif
            grpind = 2
        elseif ( arglen > grpind ) then
            ! no argument (or unrecognised), go to next option in argument (-xyz)
            grpind = grpind + 1
        else
            ! no argument (or unrecognised), go to next argument
            grpind = 2
            optind = optind + 1
        endif
    end function process_short

    ! ----------------------------------------
    ! Utility function(s)
    ! ----------------------------------------

    integer function isnum (txtval)
    ! Verify whether a character string represents a numerical value
    !
    ! Can be used to check "optarg" for numbers. Can distinguish
    ! integer, real/double and character strings:
    !
    ! isnum = 0 => txtval is a string
    ! isnum = 1 => txtval is a integer
    ! isnum > 1 => txtval is a real/double

        character(len=*), intent(in) :: txtval

        ! Declaration local constants
        integer, parameter :: CINT  = 1 ! when txtval contains integer
        integer, parameter :: CREAL = 2 ! when txtval contains real
        integer, parameter :: CREXP = 3 ! when txtval contains real (exponential)

        ! Declaration local variables
        integer :: num       ! numerical indicator variable, if > 0 (0 >= num >= CREXP)
        logical :: isint     ! integer indicator, if .true.
        logical :: isexp     ! real with exponent indicator, if .true.
        logical :: issign    ! sign (+/-) indicator, if .true.
        logical :: issignexp ! sign (+/-) indicator for exponents, if .true.
        logical :: isblank   ! indicator for blanks between characters
        integer :: i         ! control variable (index), max. len(txtvar)

        ! Initialize local variables
        num       = 0
        isint     = .false.
        isexp     = .false.
        issign    = .false.
        issignexp = .false.
        isblank   = .false.
        i         = 0

        ! loop over characters
        do
            if (i >= len(txtval)) then
            ! last check
                if (.not. isint) exit
                if (num >= CREXP .and. (.not. isexp)) exit
                isnum = num
                return
            end if

            i = i + 1

            select case (txtval(i:i))
                ! process blanks
                case (' ')
                    if (num == 0 .and. (.not. isblank)) then ! preceding or trailing blanks
                        continue
                    else if (num /= 0) then ! blank after sign or digit
                        isblank = .true.
                    end if
                ! process digits
                case ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
                    if (num == 0) num = CINT ! first digit
                    if (num < CREXP) then ! no exponent number
                        isint = .true.
                    else ! exponent number
                        isexp = .true.
                    end if
                    if (isblank) exit ! if blanks are in the middle => string
                ! process signs
                case ('+', '-')
                    if (num == 0) then ! sign of number
                        if (issign) exit ! second sign without digit => string
                        issign = .true.
                        num = CINT
                    else ! sign of exponent
                        if (num < CREXP) exit
                        if (issignexp) exit
                        issignexp = .true.
                    end if
                ! process decimal point
                case ('.')
                    if (num /= CINT .and. i /= 1) exit
                    num = CREAL
                ! process exponent
                case ('e', 'E', 'd', 'D')
                    if (num >= CREXP) exit
                    if (.not. isint) exit
                    num = CREXP
                case default ! any other character means the string is non-numeric
                    exit
            end select
        end do

        isnum = 0 ! if this point is reached, the string is non-numeric
        return
    end function isnum

end module f90getopt

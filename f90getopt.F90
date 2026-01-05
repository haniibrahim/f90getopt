module f90getopt
implicit none
private
public :: getopt, option_s, optarg, isnum, LONG, check_duplicate

#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
#else
#define stderr 0
#endif

character(len=80) :: optarg
integer, parameter:: LONG = 128
character         :: optopt
integer           :: optind = 1
logical           :: opterr = .true.

type option_s
    character(len=80) :: name
    logical           :: has_arg
    character         :: short      ! shortopt char or char(0)
end type option_s

integer, private :: grpind = 2

contains

! =====================================================================
! getopt()
! =====================================================================
character function getopt(optstring,longopts)
    character(len=*),intent(in) :: optstring
    type(option_s),intent(in),optional :: longopts(:)

    character(len=80) :: arg
    optarg=''

    if (optind > command_argument_count()) then
        getopt = char(0)
        return
    endif

    call get_command_argument(optind,arg)

    if (present(longopts) .and. arg(1:2)=="--") then
        getopt = process_long(longopts,arg)
    elseif (arg(1:1)=="-") then
        getopt = process_short(optstring,arg)
    else
        write(stderr,'(a,a,a)') &
        "STOP Unrecognized option '", arg(1:len_trim(arg)), "'"
        stop
    endif
end function getopt

! =====================================================================
! Long options processor , like --opt
! =====================================================================
character function process_long(longopts, arg)
    type(option_s),   intent(in) :: longopts(:)
    character(len=*), intent(in) :: arg

    integer  :: i, j, len_arg
    logical  :: has_equalsign

    process_long = char(0)
    optarg = ''

    len_arg = len_trim(arg)
    has_equalsign = .false.

    ! detect '='
    do j = 1, len_arg
        if (arg(j:j) == '=') then
            has_equalsign = .true.
            len_arg = j - 1
            exit
        endif
    enddo

    ! advance index if no "=value"
    if (.not. has_equalsign) optind = optind + 1

    do i = 1, size(longopts)
        if (arg(3:len_arg) == trim(longopts(i)%name)) then

            ! return value
            if (longopts(i)%short /= "") then
                optopt = longopts(i)%short
                process_long = optopt
            else
                optopt = char(128 + i)
                process_long = optopt
            endif

            ! handle argument if required
            if (longopts(i)%has_arg) then

                if (has_equalsign) then
                    optarg = arg(len_arg+2:)
                    if (len_trim(optarg) == 0) then
                        write(stderr,'(a,a,a)') &
                            "STOP Option '", trim(arg), "' requires a value"
                        process_long = char(0)
                    endif
                    optind = optind + 1

                else
                    if (optind <= command_argument_count()) then
                        call get_command_argument(optind, optarg)

                        ! ---- FIX: reject next option as value ----
                        if (looks_like_option(optarg)) then
                            write(stderr,'(a,a,a)') &
                                "STOP Option '", trim(arg), "' requires a value"
                            process_long = char(0)
                            return
                        endif

                        optind = optind + 1
                    else
                        write(stderr,'(a,a,a)') &
                            "STOP Option '", trim(arg), "' requires a value"
                        process_long = char(0)
                    endif
                endif
            endif

            return
        endif
    enddo

    ! unknown long option
    optopt = '?'
    write(stderr,'(a,a,a)') &
        "STOP Unrecognized option '", arg(1:len_arg), "'"
        stop
end function process_long

! =====================================================================
! Short options processor , like -o
! =====================================================================
character function process_short(optstring, arg)
    character(len=*), intent(in) :: optstring, arg
    integer :: i, arglen

    arglen = len_trim(arg)
    optarg = ''

    optopt = arg(grpind:grpind)
    process_short = optopt

    i = index(optstring, optopt)

    if (i == 0) then
        process_short = '?'
        if (opterr) then
            write(stderr,'(a,a,a)') &
                "STOP Unrecognized option '-", optopt, "'"
                stop
        endif
    endif

    ! option requires argument
    if (i > 0 .and. substr(optstring, i+1, i+1) == ':') then

        optind = optind + 1

        if (arglen > grpind) then
            ! -xVALUE
            optarg = arg(grpind+1:arglen)

        elseif (optind <= command_argument_count()) then
            ! -x VALUE
            call get_command_argument(optind, optarg)

            ! ---- Reject next option as value ----
            if (looks_like_option(optarg)) then
                write(stderr,'(a,a,a)') &
                    "STOP Option '-", optopt, "' requires a value"
                process_short = char(0)
                return
            endif

            optind = optind + 1

        else
            if (opterr) then
                write(stderr,'(a,a,a)') &
                    "STOP Option '-", optopt, "' requires a value"
            endif
            process_short = char(0)
        endif

        grpind = 2

    elseif (arglen > grpind) then
        ! grouped short options: -xyz
        grpind = grpind + 1

    else
        grpind = 2
        optind = optind + 1
    endif
end function process_short

! =====================================================================
! INTERNAL HELPER ROUTINES
! =====================================================================
character function substr(str,i,j)
    ! Return str(i:j) if 1 <= i <= j <= len(str),
    ! else return empty string.
    !
    ! This is needed because Fortran standard allows but doesn't *require* short-circuited
    ! logical AND and OR operators. So this sometimes fails:
    !     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
    ! but this works:
    !     if ( substr(str, i+1, i+1) == ':' ) then
    character(len=*),intent(in)::str
    integer,intent(in)::i,j
    if (1<=i .and. i<=j .and. j<=len(str)) then
        substr=str(i:j)
    else
        substr=''
    end if
end function substr

! =====================================================================

logical function looks_like_option(arg)
    ! Helper function to prevent misinterpretation of an option as a value
    ! w/o causing problems w/ negativ numbers (e.g. -3)
    character(len=*), intent(in) :: arg

    if (len_trim(arg) == 0) then
        looks_like_option = .false.
        return
    endif

    ! starts with "--" → definitely option
    if (arg(1:2) == "--") then
        looks_like_option = .true.
        return
    endif

    ! starts with "-" → could be number or option
    if (arg(1:1) == "-") then
        ! numeric values like -3, -1.2, -2e-3 are allowed
        if (isnum(arg) > 0) then
            looks_like_option = .false.
        else
            looks_like_option = .true.
        endif
        return
    endif

    looks_like_option = .false.
end function looks_like_option


! =====================================================================
! UTILLITY ROUTINES
! =====================================================================
integer function isnum(txtval)
    ! Verify whether a character string represents a numerical value
    !
    ! Can be used to check "optarg" for numbers. Can distinguish
    ! integer, real/double and character strings:
    !
    ! isnum = 0 => txtval is a string
    ! isnum = 1 => txtval is a integer
    ! isnum > 1 => txtval is a real/double
    character(len=*),intent(in)::txtval
    integer,parameter::CINT=1,CREAL=2,CREXP=3
    integer::num,i
    logical::isint,isexp,issign,issignexp,isblank
    num=0; isint=.false.; isexp=.false.; issign=.false.; issignexp=.false.; isblank=.false.; i=0

    do
        if (i>=len(txtval)) then
            if (.not. isint) exit
            if (num>=CREXP .and. (.not. isexp)) exit
            isnum=num
            return
        end if
        i=i+1

        select case(txtval(i:i))
        case(' ')
            if (num/=0) isblank=.true.
        case('0':'9')
            if (num==0) num=CINT
            if (num<CREXP) then; isint=.true.; else; isexp=.true.; end if
            if (isblank) exit
        case('+','-')
            if (num==0) then
                if (issign) exit
                issign=.true.; num=CINT
            else
                if (num<CREXP) exit
                if (issignexp) exit
                issignexp=.true.
            end if
        case('.')
            if (num/=CINT .and. i/=1) exit
            num=CREAL
        case('e','E','d','D')
            if (num>=CREXP) exit
            if (.not. isint) exit
            num=CREXP
        case default
            exit
        end select
    end do

    isnum=0
end function isnum

! =====================================================================

subroutine check_duplicate(c)
    ! Checks for duplicate options
    ! If duplicate option is present program is halted
    !
    ! 
    implicit none
    character, intent(in) :: c

    integer       :: id
    logical, save :: seen(0:255)
    logical, save :: initialized = .false.

    if (.not. initialized) then
        seen = .false.
        initialized = .true.
    endif

    id = ichar(c)

    if (seen(id)) then
        stop 'Option specified more than once'
    else
        seen(id) = .true.
    endif
end subroutine check_duplicate

end module f90getopt

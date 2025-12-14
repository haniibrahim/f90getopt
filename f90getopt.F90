module f90getopt
implicit none
public :: getopt, option_s, optarg, isnum
private

#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
#else
#define stderr 0
#endif

character(len=80) :: optarg
character         :: optopt
integer           :: optind = 1
logical           :: opterr = .true.

type option_s
    character(len=80) :: name
    logical           :: has_arg
    character         :: short
end type option_s

integer, private :: grpind = 2

contains

! -------------------------------------------------------------------------------------------------
! Utility: safe substring
! -------------------------------------------------------------------------------------------------
character function substr(str,i,j)
    character(len=*),intent(in)::str
    integer,intent(in)::i,j
    if (1<=i .and. i<=j .and. j<=len(str)) then
        substr=str(i:j)
    else
        substr=''
    end if
end function substr

! -------------------------------------------------------------------------------------------------
! New: detect "bare" longopts (zeta, zeta=3.5, zeta 3.5)
! -------------------------------------------------------------------------------------------------
logical function is_bare_long(arg)
    character(len=*),intent(in)::arg
    is_bare_long = .false.
    if (arg(1:1) == '-') return
    if (len_trim(arg) == 0) return
    ! must start with a letter
    if (arg(1:1) >= 'A' .and. arg(1:1) <= 'Z') is_bare_long = .true.
    if (arg(1:1) >= 'a' .and. arg(1:1) <= 'z') is_bare_long = .true.
end function is_bare_long

! -------------------------------------------------------------------------------------------------
! getopt dispatcher
! -------------------------------------------------------------------------------------------------
character function getopt(optstring,longopts)
    character(len=*),intent(in) :: optstring
    type(option_s),intent(in),optional :: longopts(:)
    character(len=80) :: arg

    optarg=''

    if (optind > command_argument_count()) then
        getopt = char(0)
        return
    end if

    call get_command_argument(optind,arg)

    if (present(longopts)) then
        if (arg(1:2) == "--") then
            getopt = process_long(longopts,arg)
            return
        elseif (is_bare_long(arg)) then
            getopt = process_bare_long(longopts,arg)
            return
        end if
    end if

    if (arg(1:1) == '-') then
        getopt = process_short(optstring,arg)
    else
        getopt = char(0)
    end if
end function getopt

! -------------------------------------------------------------------------------------------------
! New: process "bare" longopts
! -------------------------------------------------------------------------------------------------
character function process_bare_long(longopts,arg)
    type(option_s),intent(in)::longopts(:)
    character(len=*),intent(in)::arg
    character(len=80)::tok,value
    integer::i,pos

    process_bare_long = char(0)

    pos = index(arg,"=")
    if (pos > 0) then
        tok = arg(1:pos-1)
        value = arg(pos+1:)
    else
        tok = trim(arg)
        value = ""
    end if

    do i=1,size(longopts)
        if (trim(tok) == trim(longopts(i)%name)) then
            optopt = longopts(i)%short
            process_bare_long = optopt

            if (.not. longopts(i)%has_arg) then
                optind = optind + 1
                return
            endif

            if (pos > 0) then
                optarg = trim(value)
                optind = optind + 1
            else
                if (optind+1 <= command_argument_count()) then
                    call get_command_argument(optind+1,optarg)
                    optind = optind + 2
                else
                    if (opterr) write(stderr,'(a,a,a)')"ERROR: Option '",trim(tok),"' needs a value."
                    process_bare_long=char(0)
                endif
            endif
            return
        end if
    end do

    if (opterr) write(stderr,'(a,a,a)')"ERROR: Unknown option '",trim(arg),"'"
end function process_bare_long

! -------------------------------------------------------------------------------------------------
! Original longopt processor (unchanged except compatibility)
! -------------------------------------------------------------------------------------------------
character function process_long(longopts,arg)
    type(option_s),intent(in)::longopts(:)
    character(len=*),intent(in)::arg
    integer::i,j,len_arg
    logical::has_eq

    process_long=char(0)

    len_arg=len_trim(arg)
    has_eq=.false.

    do j=1,len_arg
        if (arg(j:j)=="=") then
            has_eq=.true.
            len_arg=j-1
            exit
        end if
    end do

    if (.not. has_eq) optind=optind+1

    do i=1,size(longopts)
        if (arg(3:len_arg) == longopts(i)%name) then
            optopt=longopts(i)%short
            process_long=optopt

            if (longopts(i)%has_arg) then
                if (has_eq) then
                    optarg = arg(len_arg+2:)
                    optind = optind + 1
                else
                    if (optind <= command_argument_count()) then
                        call get_command_argument(optind,optarg)
                        optind = optind + 1
                    else
                        write(stderr,'(a)')"ERROR: Missing value for long option."
                        process_long = char(0)
                    end if
                end if
            end if
            return
        end if
    end do

    write(stderr,'(a,a,a)')"ERROR: Unknown option '",arg(1:len_arg),"'"
end function process_long

! -------------------------------------------------------------------------------------------------
! Original shortopt processor (unchanged)
! -------------------------------------------------------------------------------------------------
character function process_short(optstring,arg)
    character(len=*),intent(in)::optstring,arg
    integer::i,arglen

    arglen=len(trim(arg))
    optopt=arg(grpind:grpind)
    process_short=optopt

    i=index(optstring,optopt)
    if (i==0) then
        process_short='?'
        if (opterr) write(stderr,'(a,a,a)')"ERROR: Unrecognized option '-",optopt,"'"
    endif

    if (i>0 .and. substr(optstring,i+1,i+1)==":") then
        optind=optind+1
        if (arglen>grpind) then
            optarg=arg(grpind+1:arglen)
        elseif (optind<=command_argument_count()) then
            call get_command_argument(optind,optarg)
            optind=optind+1
        else
            if (opterr) write(stderr,'(a,a,a)')"ERROR: Option '-",optopt,"' requires value"
            process_short=char(0)
        endif
        grpind=2
    elseif (arglen>grpind) then
        grpind=grpind+1
    else
        grpind=2
        optind=optind+1
    endif
end function process_short

! -------------------------------------------------------------------------------------------------
! Numeric type detector (unchanged)
! -------------------------------------------------------------------------------------------------
integer function isnum(txtval)
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

end module f90getopt

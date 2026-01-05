program test
use f90getopt
implicit none

character :: short
character(len=*),parameter :: optshort = "ho:"
! type(option_s),dimension(4) :: longopts

! ! longopts initialisieren
! longopts(1)%name="help";    longopts(1)%has_arg=.false.; longopts(1)%short='h'
! longopts(2)%name="output";  longopts(2)%has_arg=.true.;  longopts(2)%short='o'

! ! ---- longopt-only Optionen ----
! longopts(3)%name="zeta";    longopts(3)%has_arg=.true.;  longopts(3)%short=""
! longopts(4)%name="alpha";   longopts(4)%has_arg=.false.; longopts(4)%short=""

type(option_s) :: opts(4)
! longopts initialisieren
opts(1) = option_s("help",    .false., "h")
opts(2) = option_s("output",  .true.,  "o")
! ---- longopt-only Optionen ----
opts(3) = option_s("zeta",    .true.,  "")
opts(4) = option_s("alpha",   .false., "")

! If no options were committed
    ! ----------------------------
    if (command_argument_count() .eq. 0) then
      stop "Program has options: --alpha -h --help -- zeta=x --zeta x -o x --output=x --output x"
    end if

! optind = 1
do
    short = getopt(optshort,opts)

    call check_duplicate(short)

    select case(short)
        case(char(0)) ! When all options are processed
            exit

        case('h')
            print *,"Help selected"

        case('o')
            print *,"Output file:", trim(optarg)

        ! ------ longopts without short equivalents ------
        case(char(LONG+3))  ! corresponds to opts(3)
            print *,"ZETA => ",trim(optarg)

        case(char(LONG+4))  ! corresponds to opts(4)
            print *,"ALPHA triggered"
    end select
enddo

end program test

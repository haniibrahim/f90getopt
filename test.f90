program test
use f90getopt
implicit none

character :: c
character(len=*),parameter :: optshort = "ho:"
! type(option_s),dimension(4) :: longopts

! ! longopts initialisieren
! longopts(1)%name="help";    longopts(1)%has_arg=.false.; longopts(1)%short='h'; longopts(1)%has_short=.true.
! longopts(2)%name="output";  longopts(2)%has_arg=.true.;  longopts(2)%short='o'; longopts(2)%has_short=.true.

! ! ---- longopt-only Optionen ----
! longopts(3)%name="zeta";    longopts(3)%has_arg=.true.;  longopts(3)%short=char(0); longopts(3)%has_short=.false.
! longopts(4)%name="alpha";   longopts(4)%has_arg=.false.; longopts(4)%short=char(0); longopts(4)%has_short=.false.

! type(option_s) :: opts(4)
! ! longopts initialisieren
! opts(1) = option_s("help",    .false., "h", .true.)
! opts(2) = option_s("output",  .true.,  "o", .true.)
! ! ---- longopt-only Optionen ----
! opts(3) = option_s("zeta",    .true.,  "", .false.)
! opts(4) = option_s("alpha",   .false., "", .false.)

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
      print*, "ERROR: Program has options: -a. --alpha -b x --beta=x --beta x"
    end if

! optind = 1
do
    c = getopt(optshort,opts)
    if (c == char(0)) exit

    select case(c)
    case('h')
        print *,"Help selected"

    case('o')
        print *,"Output file:", trim(optarg)

    ! ------ longopts without short equivalents ------
    case(char(128+3))  ! corresponds to longopts(3)
        print *,"ZETA => ",trim(optarg)

    case(char(128+4))  ! corresponds to longopts(4)
        print *,"ALPHA triggered"

    ! ! case default
    !     print *,"Unknown option code",iachar(c)
    end select
enddo

end program test
program demo_getopt
use f90getopt
implicit none

type(option_s),dimension(4) :: longopts = [ &
    option_s("alpha", .true.,  'a'), &
    option_s("beta",  .false., 'b'), &
    option_s("gamma", .false.,  'g'), &
    option_s("zeta",  .true.,  'z') ]   ! zeta hat KEINE short-option, aber einen Platzhalter 'z'

character :: c

print *, "Parsing command line ..."

do
    c = getopt("a:bg", longopts)

    if (c == char(0)) exit

    select case(c)
    case('a')
        print *,"Option alpha, value=", trim(optarg)
    case('b')
        print *,"Option beta (flag)"
    case('g')
        print *,"Option gamma (flag)"
    case('z')
        print *,"Option zeta (long-only), value=", trim(optarg)
    case('?')
        print *,"Unknown option."
    end select
end do

print *, "Done."
end program demo_getopt

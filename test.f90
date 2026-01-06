program test
    ! Sample program for f90getopt function
    use f90getopt
    implicit none

    ! local variables
    character(len=1)           :: short             ! getopt-character

    ! START for shortopts
    ! ----------------------------------
    ! - optshort  = character array of short option characters without a space
    !              colon ":" after a character says that this option requires an argument
    character(len=*),parameter :: optshort = "ho:"
    ! END shortopts

    ! START for longopts (OPTIONAL)
    ! ----------------------------------
    ! option_s derived type:
    !   1st value = long option name (character array, max. 80)
    !   2nd value = if option has an argument (logical)
    !   3rd value = short option name (single character), same as in getopt()
    !               or empty if longopt-only options
    !
    ! option_s is not needed if you just use short options
    type(option_s) :: optlong(4)
      ! longopts w/ short equivalents
      optlong(1) = option_s("help",    .false., "h")
      optlong(2) = option_s("output",  .true.,  "o")
      ! longopt-only w/o short equivalents
      optlong(3) = option_s("zeta",    .true.,  "")
      optlong(4) = option_s("alpha",   .false., "")
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
    !  - optlong  = long options declaration, if specified in option_s (OPTIONAL)
    do
        short = getopt(optshort,optlong)

        ! Check for duplicates (OPTIONAL)
        ! NEW IN VERSION 2
        call check_duplicate(short)

        select case(short)
            ! When all options are processed
            case(char(0))
                exit
            ! shortopts w/ or w/o long equivalents
            case("h")
                print *,"HELP triggered"
            case("o")
                print *,"Output file: ", trim(optarg)
            ! longopts w/o short equivalents
            ! NEW IN VERSION 2
            case(char(LONG+3))  ! corresponds to optlong(3)
                print *,"ZETA => ",trim(optarg)
            case(char(LONG+4))  ! corresponds to optlong(4)
                print *,"ALPHA triggered"
        end select
    end do
    ! END processing options
end program
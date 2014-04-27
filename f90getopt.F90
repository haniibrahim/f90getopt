module f90getopt

    implicit none

    character(len=80):: optarg        ! Option's argument
    character        :: optopt        ! Option character
    integer          :: optind=1      ! Index of the next argument to process
    logical          :: opterr=.true. ! Errors are printed by default. Set opterr=.false. to suppress them

    type option_s
        character(len=80) :: name     ! Name of the option
        logical           :: has_arg  ! Option has an argument (.true./.false.)
        character         :: arg      ! When has_arg=.true. then arg contains the argument of option
    end type option_s

    ! grpind is index of next option within group; always >= 2
    integer, private:: grpind=2

contains

    ! ----------------------------------------
    ! Return str(i:j) if 1 <= i <= j <= len(str),
    ! else return empty string.
    ! This is needed because Fortran standard allows but doesn't *require* short-circuited
    ! logical AND and OR operators. So this sometimes fails:
    !     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
    ! but this works:
    !     if ( substr(str, i+1, i+1) == ':' ) then

    character function substr( str, i, j )
        ! arguments
        character(len=*), intent(in):: str
        integer, intent(in):: i, j

        if ( 1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        endif
    end function substr


    ! ----------------------------------------
    character function getopt( optstring, longopts )
        ! arguments
        character(len=*), intent(in):: optstring
        type(option_s),   intent(in), optional:: longopts(:)

        ! local variables
        character(len=80):: arg

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
        ! arguments
        type(option_s),   intent(in):: longopts(:)
        character(len=*), intent(in):: arg

        ! local variables
        integer:: i

        ! search for matching long option
        optind = optind + 1
        do i = 1, size(longopts)
            if ( arg(3:) == longopts(i)%name ) then
                optopt = longopts(i)%arg
                process_long = optopt
                if ( longopts(i)%has_arg ) then
                    if ( optind <= command_argument_count()) then
                        call get_command_argument( optind, optarg )
                        optind = optind + 1
                    elseif ( opterr ) then
                        print '(a,a,a)', "Error: option '", trim(arg), "' requires an argument"
                        process_long=char(0) ! Option not valid
                    endif
                endif
                return
            endif
        end do
        ! else not found
        process_long = '?'
        if ( opterr ) then
            print '(a,a,a)', "Error: unrecognized option '", trim(arg), "'"
        endif
    end function process_long


    ! ----------------------------------------
    character function process_short( optstring, arg )
        ! arguments
        character(len=*), intent(in):: optstring, arg

        ! local variables
        integer:: i, arglen

        arglen = len( trim( arg ))
        optopt = arg(grpind:grpind)
        process_short = optopt

        i = index( optstring, optopt )
        if ( i == 0 ) then
            ! unrecognized option
            process_short = '?'
            if ( opterr ) then
                print '(a,a,a)', "Error: unrecognized option '-", optopt, "'"
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
                print '(a,a,a)', "Error: option '-", optopt, "' requires an argument"
                process_short = char(0) ! Option not valid
            endif
            grpind = 2
        elseif ( arglen > grpind ) then
            ! no argument (or unrecognized), go to next option in argument (-xyz)
            grpind = grpind + 1
        else
            ! no argument (or unrecognized), go to next argument
            grpind = 2
            optind = optind + 1
        endif
    end function process_short

end module f90getopt

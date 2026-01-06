program test
   ! Sample program for f90getopt function
   use f90getopt
   implicit none

   ! local variables
   character(len = 1) :: c             ! getopt-character

   ! START for shortopts
   ! ----------------------------------
   ! - optshort  = character array of short option characters without a space
   !              colon ":" after a character says that this option requires an argument
   character(len = *), parameter :: optshort = "ho:"
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
   optlong(1) = option_s("help", .false., "h")
   optlong(2) = option_s("output", .true., "o")
   ! longopt-only w/o short equivalents
   optlong(3) = option_s("zeta", .true., achar(0))
   optlong(4) = option_s("alpha", .false., achar(0))
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
   
   call check_duplicates(optshort, optlong)

   do
      c = getopt(optshort, optlong)
      if (c == achar(0) .and. optlongind == 0) exit

      if (c /= achar(0)) then
         select case (c)
            case ('h')
               print *, "HELP"
            case ('o')
               print *, "Out = ", trim(optarg)
         end select
      else
         select case (optlongind)
            case (3)   ! zeta
               print *, "zeta = ", trim(optarg)
            case (4)   ! alpha
               print *, "alpha"
         end select
      endif
   end do
   ! END processing options
end program
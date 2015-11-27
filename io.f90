module io
    use errors
    use system
    implicit none

    integer :: unit_run, unit_geom, unit_force

contains

    subroutine open_file(filestream, filename, readwrite)
        ! Opens a file to a given filestream
        integer, intent(out) :: filestream
        integer, intent(in) :: readwrite ! 0 for read, 1 for write, 2 for both
        character(9) :: mode
        character(*), intent(in) :: filename
        integer :: ioerr ! Check status of opening file

        if (readwrite == 1) then
            mode = "write"
        else if (readwrite == 2) then
            mode = "readwrite"
        else
            mode = "read"
        end if
        
        open(newunit=filestream, file=trim(filename), action=mode, iostat=ioerr)
            
        ! Check that opening was successful
        if (ioerr > 0) call throw_error('IO', 'Failed to open file ' // filename)
    end subroutine open_file

    subroutine system_init(filename)
        character(*), intent(in) :: filename
        ! Initialise variables in the system

        ! get params from runfile
        ! get N from geom file
        ! put geom file data into array
        ! open force params file
        ! put force params into array
        
        call open_file(unit_run, trim(filename) // ".run", 0)

        call parse_runfile()

        N = 100

        close(unit_run)
        allocate(state(7,N), params(3,N), forces(3,N))
        
!        call exit(0)
    end subroutine

    subroutine parse_runfile()
        integer :: ios
        real(dp) :: total_time
        character(20) :: keyword, str1, str2
        do
            read(unit_run, *, iostat = ios) keyword
            if (ios < 0) then
                exit ! EOF
            else if (ios > 0) then
                call throw_error("io", "could not read run file")
            end if
            backspace(unit_run)
            select case (keyword)
                case ("verbosity")
                    read(unit_run, *) keyword, verbosity
                case ("force")
                    read(unit_run, *) keyword, force_name
                case ("integrator")
                    read(unit_run, *) keyword, integrator_name
                case ("thermostat")
                    read(unit_run, *) keyword, thermostat_name
                case ("output")
                    read(unit_run, *) keyword, str1, str2
                    ! convert to logicals
                case ("T")
                    read(unit_run, *) keyword, T
                case ("P")
                    read(unit_run, *) keyword, P
                case ("step")
                    read(unit_run, *) keyword, dt
                case ("time")
                    read(unit_run, *) keyword, total_time
                    iter_tot = ceiling(total_time / dt)
                case ("box")
                    read(unit_run, *) keyword, box
                case ("rcut")
                    read(unit_run, *) keyword, r_cut
                    r_cut = r_cut * r_cut
                case default
                    read(unit_run, '(A)') str1
                    call throw_log("Could not handle runfile line " // str1)
            end select
        end do
    end subroutine

    character(20) function inttostr(num)
        ! converts an integer into a left-aligned string
        integer, intent(in) :: num
        write(inttostr, *) num
        inttostr = adjustl(inttostr)
    end function
end module io

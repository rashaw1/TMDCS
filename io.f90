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

        ! get params from runfile - done
        ! get N from geom file - done
        ! put geom file data into array
        ! open force params file
        ! put force params into array
        
        call open_file(unit_run, trim(filename) // ".run", 0)
        call parse_runfile()
        close(unit_run)

        call open_file(unit_geom, trim(filename) // ".geom", 0)
        call parse_geomfile_N()
        close(unit_geom)

        allocate(state(7,N), params(3,N), forces(3,N))
        
        call exit(0)
    end subroutine

    subroutine parse_runfile()
        integer :: ios
        real(dp) :: total_time
        character(20) :: keyword, str1, str2
        str1 = ""
        str2 = ""
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
                    if ((str1 == "geom") .or. (str2 == "geom")) then
                        output_geom = .true.
                    end if
                    if ((str1 == "jmol") .or. (str2 == "jmol")) then
                        output_jmol = .true.
                    end if
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
                    call throw_warning("Could not handle runfile line " // str1)
            end select
        end do
    end subroutine

    subroutine parse_geomfile_N()
        character(10) file_format
        character(20) line
        integer :: ioerr

        N = 0

        read(unit_geom, *) file_format
        select case (file_format)
            case("XYZ")
                call throw_log("Geom file found in XYZ format", 4)
                XYZ: do
                    read(unit_geom, *, iostat=ioerr) line
                    line = adjustl(line)

                    if (ioerr < 0) then
                        exit XYZ
                    else if (ioerr > 0) then
                        call throw_error("io", "Error reading geom file")
                    end if

                    if (.not. charisalpha(line(1:1))) then
                        N = N + 1
                    end if
                end do XYZ
            case("GRID")
                call throw_error("io", "GRID format not yet implemented")
                ! TODO: support GRID format
            case default
                call throw_error("io", "Geom file in unsupported format " // &
                    file_format)
        end select
        
    end subroutine

    character(20) function inttostr(num)
        ! converts an integer into a left-aligned string
        integer, intent(in) :: num
        write(inttostr, *) num
        inttostr = adjustl(inttostr)
    end function

    logical function charisalpha(letter)
        ! returns true if char is a letter
        character, intent(in) :: letter
        integer :: au, zu, al, zl, l
        au = iachar("A")
        zu = iachar("Z")
        al = iachar("a")
        zl = iachar("z")
        l = iachar(letter)
        charisalpha = .false.
        if ( ( (l >= au) .and. (l <= zu) ) .or. ( (l >= al) .and. &
            (l <= zl) ) ) then
            charisalpha = .true.
        end if
    end function
end module io

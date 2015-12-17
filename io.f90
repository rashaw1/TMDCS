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

        call open_file(unit_run, trim(filename) // ".run", 0)
        call parse_runfile()
        close(unit_run)

        call open_file(unit_geom, trim(filename) // ".geom", 0)
        call parse_geomfile_N()

        allocate(positions(3,N), velocities(3,N), params(3,N), forces(3,N), &
            atom_names(N))

        rewind(unit_geom)
        call parse_geomfile()
        close(unit_geom)
        
        call open_file(unit_force, trim(force_name) // ".dat", 0)
        call parse_forcefile()
        close(unit_force)
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
                    e_cut = 4d0/(r_cut**12 - r_cut**6) 
                 case default
                    read(unit_run, '(A)') str1
                    call throw_warning("Could not handle runfile line " // str1)
            end select
        end do
    end subroutine

    subroutine parse_geomfile_N()
        character(10) :: file_format
        character(255) :: line
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
                    trim(file_format))
        end select
    end subroutine

    subroutine parse_geomfile()
        character(10) :: file_format
        character(8) :: atom_name
        character(255) :: line
        integer :: ioerr, i

        i = 1

        read(unit_geom, *) file_format
        select case(file_format)
            case("XYZ")
                XYZ: do
                    read(unit_geom, *, iostat=ioerr) line
                    line = adjustl(line)

                    if (ioerr < 0) then
                        exit XYZ
                    else if (ioerr > 0) then
                        call throw_error("io", "Error reading geom file")
                    end if
                    
                    if (charisalpha(line(1:1))) then
                        atom_name = trim(line)
                    else
                        backspace(unit_geom)
                        read(unit_geom, *) positions(:, i), velocities(:, i)
                        atom_names(i) = atom_name
                        i = i+1
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

    subroutine parse_forcefile()
        character(8) :: atom_name
        character(255) :: line
        integer :: i, ioerr
        real(dp) :: mass, sigma, eps

        params = -1 ! to see if any were missed later

        ! only LJ for now
        do
            read(unit_force, *, iostat = ioerr) atom_name

            if (ioerr < 0) then
                exit
            end if

            read(unit_force, *, iostat = ioerr) mass
            read(unit_force, *, iostat = ioerr) sigma
            read(unit_force, *, iostat = ioerr) eps

            mass = mass / ArMass

            if (ioerr < 0) then
                call throw_error("io", "force parameters end unexpectedly")
            else if (ioerr > 0) then
                call throw_error("io", "Error reading " // trim(force_name) // ".dat")
            end if

            do i = 1, N
                if (atom_names(i) == atom_name) then
                    params(1, i) = mass
                    params(2, i) = sigma
                    params(3, i) = eps
                end if
            end do
        end do
        
        ! check for missing atom types

        do i = 1, N
            if (params(i, 1) < 0) then
                call throw_error("io", "atom type " // trim(atom_names(i)) // &
                    " missing from " // trim(force_name) // ".dat")
            end if
        end do
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
    end function charisalpha

    subroutine write_jmol_xyz(jmol_unit)
        ! Writes the current xyz coordinates of the system
        ! to the jmol .xyz output file
        integer, intent(in) :: jmol_unit
        integer :: i
        write(jmol_unit, *) N
        write(jmol_unit, *) 'Iteration', iter
        do i = 1, N
           write(jmol_unit, *) atom_names(i), positions(:, i)
        end do
    end subroutine write_jmol_xyz

    subroutine write_geom(output_geom_unit)
        ! Writes the system as a .geom file ready for input
        ! back into TMDCS
        integer, intent(in) :: output_geom_unit
        integer :: i
        character(8) :: curr_atom, next_atom
        
        write(output_geom_unit, *) 'XYZ'

        curr_atom = atom_names(1)
        write(output_geom_unit, *) curr_atom
        write(output_geom_unit, '(6F12.8)') positions(:, 1), velocities(:, 1)
        do i = 2, N
           next_atom = atom_names(i)
           if (next_atom == curr_atom) then      
              write(output_geom_unit, '(6F12.8)') positions(:, i), velocities(:, i)
           else
              curr_atom = next_atom
              write(output_geom_unit, *)
              write(output_geom_unit, *) curr_atom
              write(output_geom_unit, '(6F12.8)') positions(:, 1), velocities(:, 1)
           end if
        end do
    end subroutine write_geom
    
end module io

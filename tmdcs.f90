program tmdcs
    ! The main program for the TMDCS molecular dynamics package
    use errors
    use constants
    use system
    use theforce
    use integrator
    use thermostat
    use io

    implicit none

    ! Declare variables
    real(dp), dimension(50*99) :: energies
    real(dp) :: frequency = 0.1d0 ! For Andersen thermostat
    real(dp) :: Q ! For Nose-Hoover thermostat
    real(dp) :: tau = 0 ! Relaxation time for Nose-Hoover 
    integer :: i
    integer :: jmol, geom_out
    character(255) :: filename

    ! get filename to use throughout from command line
    call get_command_argument(1, filename)
    if (trim(filename) == "") then
        write(error_unit, "(A)") "ERROR: please specify a filename as a " // &
            "command line argument"
        call exit(1)
    end if

    ! setup logging
    call errors_init(filename)
    ! Would read input here
    
    ! Initialise the SYSTEM and set particles on a grid
    call throw_log("Initialising...", 4)
    call random_init() ! seed PRNG
    call system_init(filename) ! read in variables, initialise arrays

    do i = 1, N
        write(*,*) atom_names(i), params(:, i)
    end do

    ! Calculate forces once
    call throw_log("Calculating initial forces", 3)
    call force(energies)

    ! Open xyz file
    call open_file(jmol, trim(filename) // ".xyz", 1)

    ! Initialise Nose-Hoover thermostat
    zeta = 0d0 
    Q = 100

    ! Warm up the system
    call rescale()
    
    call throw_log("Beginning main MD loop", 2)
    ! Start main loop
    ! Procedure:
    ! 1) Calculate forces
    ! 2) Update velocities
    ! 3) Rescale velocities
    ! 4) Update positions
    ! 5) Print data
    do iter = 1, iter_tot
        call throw_log("Starting iteration " // trim(inttostr(iter)), 2)
        call force(energies)
        call calc_velocities(zeta)
        ! call rescale()
        call andersen(frequency)
        !call nose_hoover(Q)
        write(*, *) calc_T()
        call calc_positions(zeta)
        !call nose_hoover(Q)
        call write_jmol_xyz(jmol)
    end do

    call open_file(geom_out, trim(filename) // ".out", 1)
    call write_geom(geom_out)
    
    call finalise()
    !  call close_files()
    call errors_final()
end program tmdcs

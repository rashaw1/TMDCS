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
    integer :: i
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
    ! initialise(N, iter_tot, T, dt, box, r_cut) 
    ! call open_files()
    call throw_log("Initialising...", 4)
    call random_init()
    call system_init(filename)

    !call initialise(filename)!100, 500, 0.5d0, 0.1d0, 77.395d0, 5d0)
    call set_positions_grid()
    write(*, *) 'INITIAL POSITIONS:'
    call print_system()

    ! Put some test values into the arrays
    do i = 1, 100
        params(1:3, i) = 1d0
    end do

    ! Calculate forces once
    call throw_log("Calculating initial forces", 3)
    call force(energies)

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
        call calc_velocities()
        call rescale()
        call calc_positions()
        write(*, *) 100
        write(*, *) 'Iteration ', iter
        call print_system()
    end do

    call finalise()
    !  call close_files()
    call errors_final()
end program tmdcs

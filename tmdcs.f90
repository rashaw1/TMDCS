program tmdcs
  ! The main program for the TMDCS molecular dynamics package
  use constants
  use system
  use theforce
  use integrator
  use thermostat
  use input
  
  implicit none

  ! Declare variables
  real(dp), dimension(50*99) :: energies
  integer :: i
  ! Would read input here
  
  ! Initialise the SYSTEM and set particles on a grid
  ! initialise(N, iter_tot, T, dt, box, r_cut) 
  call initialise(100, 500, 0.5d0, 0.1d0, 77.395d0, 5d0)
  call set_positions_grid()
  write(*, *) 'INITIAL POSITIONS:'
  call print_system()
  
  ! Put some test values into the arrays
  do i = 1, 100
     params(1:3, i) = 1d0
  end do

  ! Calculate forces once
  call force(energies)

  ! Start main loop
  ! Procedure:
  ! 1) Calculate forces
  ! 2) Update velocities
  ! 3) Rescale velocities
  ! 4) Update positions
  ! 5) Print data
  do iter = 1, iter_tot
     call force(energies)
     call calc_velocities()
     call rescale()
     call calc_positions()
     write(*, *) 100
     write(*, *) 'Iteration ', iter
     call print_system()
  end do

  call finalise()
  
end program tmdcs

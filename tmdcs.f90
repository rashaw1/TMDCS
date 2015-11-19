program tmdcs
  ! The main program for the TMDCS molecular dynamics package
  use constants
  use system
  use theforce
  use integrator

  implicit none

  ! Declare variables
  real(dp), dimension(3) :: energies
  
  ! Would read input here
  
  ! Initialise the SYSTEM and set particles on a grid
  call initialise(3, 5, 0d0, 1d0, 10d0, 5d0)
  call set_positions_grid()
  call print_system()
  
  ! Put some test values into the arrays
  params(1, 1) = 4.0
  params(1, 2) = 1.0
  params(1, 3) = 2.0
  params(2, 1) = 6.5
  params(2, 2) = 1.3
  params(2, 3) = 4.7
  params(3, 1) = 2.0
  params(3, 2) = 0.5
  params(3, 3) = 2.0

  ! Calculate forces once
  call force(energies)

  ! Start main loop
  ! Procedure:
  ! 1) Calculate forces
  ! 2) Update velocities
  ! 3) Update positions
  ! 4) Print data
  do iter = 1, iter_tot
     call force(energies)
     call calc_velocities()
     call calc_positions()
     write(*, *) 'Iteration ', iter
     call print_system()
  end do

  call finalise()
  
end program tmdcs

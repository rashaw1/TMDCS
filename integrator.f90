module integrator
  ! Implements the velocity Verlet integrator/propagator
  ! to integrate the equations of motion for the system,
  ! and update the positions/velocities.

  ! Import necessary modules
  use constants
  use system
  
  implicit none

contains

  subroutine calc_positions()
    ! Routine to calculate the new positions at time step t + dt
    ! and half-update the velocities to t + dt/2
    integer :: i, j ! Indices
    real(dp) :: rt, vt, ft, m, dt2, dtsq2

    dt2 = dt / 2.0
    dtsq2 = dt2 * dt 
    
    ! Loop over all the particles in the system
    ploop: do i = 1, N
       
       ! Do for each cartesian coordinate
       xyzloop: do j = 1, 3

          ! Unpack variables from arrays for clarity
          rt = state(j+1, i)
          vt = state(j+4, i)
          m = params(1, i)
          ft = forces(j, i)
          
          ! Update position
          state(j+1, i) = rt + vt*dt + dtsq2*ft/m
          ! Take account of pcbs
          state(j+1, i) = state(j+1, i) - box*floor(state(j+1, i)/box)
          
          ! Half-update velocity
          state(j+4, i) = vt + dt2*ft/m
          
       end do xyzloop

    end do ploop
       
  end subroutine calc_positions

  subroutine calc_velocities()
    ! Routine to calculate the new velocities at time step t + dt

    integer :: i, j ! Indices
    real(dp) :: vt, ft, dt2, m

    dt2 = dt / 2.0

    ! Loop over particles
    ploop: do i = 1, N

       ! Do for each cartesian direction
       xyzloop: do j = 1, 3

          ! Unpack the variables from arrays
          vt = state(j+4, i)
          ft = forces(j, i)
          m = params(1, i)
          
          ! Update the velocities
          state(j+4, i) = vt + dt2*ft/m
       end do xyzloop

    end do ploop
    
  end subroutine calc_velocities

end module integrator

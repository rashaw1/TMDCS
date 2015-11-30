module integrator
  ! Implements the velocity Verlet integrator/propagator
  ! to integrate the equations of motion for the system,
  ! and update the positions/velocities.

  ! Import necessary modules
  use constants
  use system
  
  implicit none

contains

  subroutine calc_positions(zeta)
    ! Routine to calculate the new positions at time step t + dt
    ! and half-update the velocities to t + dt/2
    integer :: i, j ! Indices
    real(dp), intent(in) :: zeta
    real(dp) :: rt, vt, ft, m, dt2, dtsq2

    dt2 = dt / 2.0
    dtsq2 = dt2 * dt 
    
    ! Loop over all the particles in the system
    ploop: do i = 1, N
       
       ! Do for each cartesian coordinate
       xyzloop: do j = 1, 3

          ! Unpack variables from arrays for clarity
          rt = positions(j, i)
          vt = velocities(j, i)
          m = params(1, i)
          ft = forces(j, i)
          
          ! Update position
          positions(j, i) = rt + vt*dt + dtsq2*(ft/m - zeta*vt)
          ! Take account of pcbs
          positions(j, i) = positions(j, i) - box*floor(positions(j, i)/box)
          
          ! Half-update velocity
          velocities(j, i) = vt + dt2*(ft/m - zeta*vt)
          
       end do xyzloop

    end do ploop
       
  end subroutine calc_positions

  subroutine calc_velocities(zeta)
    ! Routine to calculate the new velocities at time step t + dt
    real(dp), intent(in) :: zeta
    integer :: i, j ! Indices
    real(dp) :: vt, ft, dt2, m, dt2zeta

    dt2 = dt / 2.0
    dt2zeta = 1d0 + dt2*zeta

    ! Loop over particles
    ploop: do i = 1, N

       ! Do for each cartesian direction
       xyzloop: do j = 1, 3

          ! Unpack the variables from arrays
          vt = velocities(j, i)
          ft = forces(j, i)
          m = params(1, i)
          
          ! Update the velocities
          velocities(j, i) = dt2zeta*(vt + dt2*ft/m)
       end do xyzloop

    end do ploop
    
  end subroutine calc_velocities

end module integrator

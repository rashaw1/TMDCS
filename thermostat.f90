module thermostat
  ! Controls the temperature of the SYSTEM
  use system
  use constants
  
contains

  subroutine rescale()
    ! Rescales the velocities to the correct temperature
    real(dp) :: v_scale
    integer :: i ! Loop index
    
    ! Get the mean kinetic energy
    v_scale = calc_avg_vel_squared()

    ! T = <v^2>/3N, so scale factor is
    ! v_new = sqrt(3NT/<v^2>)
    v_scale = sqrt(3*N*T/v_scale)

    ! Now rescale the velocities
    do i = 1, N
       state(5:7, i) = v_scale*state(5:7, i)
    end do

  end subroutine rescale
end module thermostat

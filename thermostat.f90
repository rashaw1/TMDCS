module thermostat
  ! Controls the temperature of the SYSTEM
  use system
  use constants
  implicit none
  
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
       velocities(:, i) = v_scale * velocities(:, i)
    end do

  end subroutine rescale
end module thermostat


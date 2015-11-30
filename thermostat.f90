module thermostat
  ! Controls the temperature of the SYSTEM
  use system
  use constants
  use random
  implicit none

  real(dp) :: zeta
  
contains

  real(dp) function calc_avg_kin_energy()
    integer :: i
    real(dp) :: rval = 0d0
        
    do i = 1, N
       rval = rval + 0.5*params(1, i)*&
            dot_product(velocities(:, i), velocities(:, i))
    end do
    
    calc_avg_kin_energy = rval / N
  end function calc_avg_kin_energy
    
  subroutine rescale()
    ! Rescales the velocities to the correct temperature
    real(dp) :: v_scale
    integer :: i ! Loop index
    
    ! Get the mean kinetic energy
    v_scale = calc_avg_kin_energy()

    ! T = <mv^2>/3N, so scale factor is
    ! v_new = sqrt(3NT/<mv^2>)
    v_scale = sqrt(3*T/v_scale)

    ! Now rescale the velocities
    do i = 1, N
       velocities(:, i) = v_scale * velocities(:, i)
    end do

  end subroutine rescale
  
  subroutine andersen(frequency)
    real(dp), intent(in) :: frequency
    real(dp) :: avg_ke, sigma, T_c
    real(dp) :: rand_num
    integer :: i
    
    avg_ke = calc_avg_kin_energy()
    T_c = avg_ke / 3d0
    sigma = sqrt(T_c)

    ! Work out which particles collide with the heat bath
    do i = 1, N
       call random_number(rand_num)
       if( rand_num < frequency*dt ) then
          velocities(1, i) = random_normal(0d0, sigma)
          velocities(2, i) = random_normal(0d0, sigma)
          velocities(3, i) = random_normal(0d0, sigma)
       end if
    end do

  end subroutine andersen

  subroutine nose_hoover(Q)
      ! Propagate zeta to control temperature with
      ! Nose-Hoover thermostat
      real(dp), intent(in) :: Q
      real(dp) :: avgke

      ! Update zeta by half a step
      avgke = calc_avg_kin_energy()
      zeta = zeta + (dt*(N*avgke - (3*N + 1)*T/2d0))/(Q)
  end subroutine nose_hoover
      
end module thermostat


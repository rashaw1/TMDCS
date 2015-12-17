module thermostat
  ! Controls the temperature of the SYSTEM
  use system
  use constants
  use random
  implicit none

  real(dp) :: zeta, d_zeta, d2_zeta
  
contains

  real(dp) function calc_T()
    calc_T = 2d0*calc_avg_kin_energy()/3d0
  end function calc_T
  
  real(dp) function calc_avg_kin_energy()
    integer :: i
    real(dp) :: rval

    rval = 0d0
        
    do i = 1, N
       rval = rval + 0.5*params(1, i)*&
            dot_product(velocities(:, i), velocities(:, i))
    end do
    
    calc_avg_kin_energy = rval / (real(N))
  end function calc_avg_kin_energy
    
  subroutine rescale()
    ! Rescales the velocities to the correct temperature
    real(dp) :: v_scale
    integer :: i ! Loop index
    
    ! Get the mean kinetic energy
    v_scale = calc_avg_kin_energy()

    ! T = <mv^2>/3N, so scale factor is
    ! v_new = sqrt(3NT/<mv^2>)
    v_scale = sqrt(3*T/(2d0*v_scale))

    ! Now rescale the velocities
    do i = 1, N
       velocities(:, i) = v_scale * velocities(:, i)
    end do

  end subroutine rescale
  
  subroutine andersen(frequency)
    real(dp), intent(in) :: frequency
    real(dp) :: sigma, sigma_m
    integer :: i
    
    sigma = sqrt(T)

    ! Work out which particles collide with the heat bath
    do i = 1, N
       if( random_real(0d0, 1d0) < frequency*dt ) then
          sigma_m = sigma/sqrt(params(1, i)) ! Square root of T/mass
          velocities(1, i) = random_normal(0d0, sigma_m)
          velocities(2, i) = random_normal(0d0, sigma_m)
          velocities(3, i) = random_normal(0d0, sigma_m)
       end if
    end do

  end subroutine andersen

  subroutine nose_hoover(tau)
      ! Propagate zeta to control temperature with
      ! Nose-Hoover thermostat
      real(dp), intent(in) :: tau
      real(dp) :: avgke

      avgke = 2d0*calc_avg_kin_energy()/(3d0*T)
      
      ! Update thermostat position, velocity, and acceleration
      d2_zeta = (1d0/(tau**2))*(avgke - 1d0)
      d_zeta = d_zeta + d2_zeta*dt/2d0
      zeta = zeta + d_zeta*dt
  end subroutine nose_hoover
      
end module thermostat


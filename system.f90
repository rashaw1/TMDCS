module SYSTEM
    use constants
    use random
    implicit none

    ! variables
    
    integer :: N, iter = 1, iter_tot
    real(dp) :: T, P, dt, box, r_cut, e_cut
    real(dp) :: Ek_tot = 0, v_tot = 0, Ek_tot_prev = 0, v_tot_prev = 0
    character(20) :: force_name, integrator_name, thermostat_name
    logical :: output_geom = .false., output_jmol = .false.

    ! 1st index: particle number (N)
    character(8), allocatable, dimension(:) :: atom_names
    
    ! 1st index: pos_x, pos_y, pos_z (3)
    ! 2nd index: particle number(N)
    real(dp), allocatable, dimension(:,:) :: positions

    ! 1st index: vel_x, vel_y, vel_z (3)
    ! 2nd index: particle number (N)
    real(dp), allocatable, dimension(:,:) :: velocities

    ! 1st index: mass, others (LJ: size, well depth == 3)
    ! 2nd index: particle number (N)
    real(dp), allocatable, dimension(:,:) :: params

    ! 1st index: force_x, force_y, force_z
    ! 2nd index: particle number (N)
    real(dp), allocatable, dimension(:,:) :: forces

contains

    subroutine set_positions_grid()
        integer :: i, j, k, num = 1, ppl
        real(dp) :: dist, v_scale
        write(*,*) "set pos"

        ppl = ceiling(N ** (1d0/3d0)) ! particles per length unit
        dist = box / ppl ! distance between particles

        X: do i = 0, ppl - 1
            Y: do j = 0, ppl - 1
               Z: do k = 0, ppl - 1
                    atom_names(num) = "gridtest"

                    positions(1,num) = dist * (i + 0.5)
                    positions(2,num) = dist * (j + 0.5)
                    positions(3,num) = dist * (k + 0.5)

                    ! set velocities randomly in (-1, 1)
                    ! scale up by sqrt(3NT) as <v^2> = 1 here
                    ! and <T> = <v^2>/3N

                    v_scale = sqrt(3 * N * T)
                    velocities(1,num) = random_real(-v_scale, v_scale)
                    velocities(2,num) = random_real(-v_scale, v_scale)
                    velocities(3,num) = random_real(-v_scale, v_scale)

                    if (num == N) then
                        exit X
                    end if

                    num = num + 1
                end do Z
            end do Y
        end do X
    end subroutine

    !subroutine set_postions(data_from_IO)
    !    put the positions into state()()
    !end subroutine

    function calc_avg_vel()
      real(dp), dimension(3) :: calc_avg_vel(3)
      integer :: i

      calc_avg_vel = 0

      do i = 1, N
         calc_avg_vel = calc_avg_vel + velocities(:,i)
      end do
      calc_avg_vel = calc_avg_vel / N
    end function calc_avg_vel

    real(dp) function calc_avg_vel_squared()
      integer :: i
      !real(dp) :: vel_sq

      calc_avg_vel_squared = 0

      do i = 1, N
         !vel_sq = 0
         !vel_sq = vel_sq + velocities(1,i) ** 2
         !vel_sq = vel_sq + velocities(2,i) ** 2
         !vel_sq = vel_sq + velocities(3,i) ** 2
         calc_avg_vel_squared = calc_avg_vel_squared + &
            dot_product(velocities(:,i), velocities(:,i))
      end do
      
      calc_avg_vel_squared = calc_avg_vel_squared / N
    end function calc_avg_vel_squared
          
    subroutine finalise()
        deallocate(positions, velocities, params, forces, atom_names)
    end subroutine
    
end module SYSTEM

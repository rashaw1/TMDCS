module SYSTEM
    use constants
    use random
    implicit none

    type :: particle
        integer :: a_type
        real(dp) :: x, y, z
        real(dp) :: vx, vy, vz
    end type particle
    
    type :: particle_force
        real(dp) :: fx, fy, fz
    end type particle_force

    ! variables
    
    integer :: N, iter = 1, iter_tot
    real(dp) :: T, dt, box, r_cut
    real(dp) :: Ek_tot = 0, v_tot = 0, Ek_tot_prev = 0, v_tot_prev = 0

    ! state
    ! 1st index: type, pos_x, pos_y, pos_z, vel_x, vel_y, vel_z (7)
    ! 2nd index: particle number (N)

    real(dp), allocatable, dimension(:,:) :: state

    ! params
    ! 1st index: mass, others (LJ: size, well depth == 3)
    ! 2nd index: particle number (N)
    
    real(dp), allocatable, dimension(:,:) :: params

    ! forces
    ! 1st index: force_x, force_y, force_z
    ! 2nd index: particle number (N)

    real(dp), allocatable, dimension(:,:) :: forces
    
contains
    subroutine initialise()
        call init_random()
        N = vN
        iter_tot = viter_tot
        T = vT
        dt = vdt
        box = vbox
        r_cut = vr_cut

        allocate(state(7,N), params(3,N), forces(3,N))
    end subroutine

    subroutine set_positions_grid()
        integer :: i, j, k, num = 1, ppl
        real(dp) :: dist, v_scale

        ppl = ceiling(N ** (1d0/3d0)) ! particles per length unit
        dist = box / ppl ! distance between particles

        X: do i = 0, ppl - 1
            Y: do j = 0, ppl - 1
               Z: do k = 0, ppl - 1
                    state(1,num) = 1

                    state(2,num) = dist * (i + 0.5)
                    state(3,num) = dist * (j + 0.5)
                    state(4,num) = dist * (k + 0.5)

                    ! set velocities randomly in (-1, 1)
                    ! scale up by sqrt(3NT) as <v^2> = 1 here
                    ! and <T> = <v^2>/3N

                    v_scale = sqrt(3 * N * T)
                    state(5,num) = random_real(-v_scale, v_scale)
                    state(6,num) = random_real(-v_scale, v_scale)
                    state(7,num) = random_real(-v_scale, v_scale)

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
         calc_avg_vel = calc_avg_vel + state(5:7,i)
      end do
      calc_avg_vel = calc_avg_vel / N
    end function calc_avg_vel

    real(dp) function calc_avg_vel_squared()
      integer :: i
      real(dp) :: vel_sq

      calc_avg_vel_squared = 0

      do i = 1, N
         vel_sq = 0
         vel_sq = vel_sq + state(5,i) ** 2
         vel_sq = vel_sq + state(6,i) ** 2
         vel_sq = vel_sq + state(7,i) ** 2
         calc_avg_vel_squared = calc_avg_vel_squared + vel_sq
      end do
      
      calc_avg_vel_squared = calc_avg_vel_squared / N
    end function calc_avg_vel_squared
          
    subroutine finalise()
        deallocate(state, params, forces)
    end subroutine

    subroutine print_system()
      ! Prints the SYSTEM out for debugging purposes
      integer :: i
      do i = 1, N
         write(*, *) 'AR ', state(2:4, i)
      end do
    end subroutine print_system
    
end module SYSTEM

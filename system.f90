module SYSTEM
    use constants
    implicit none

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
    subroutine initialise(vN, viter_tot, vT, vdt, vbox, vr_cut)
        integer, intent(in) :: vN, viter_tot
        real(dp), intent(in) :: vT, vdt, vbox, vr_cut

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
        real(dp) :: dist

        ppl = ceiling(N ** (1d0/3d0)) ! particles per length unit
        dist = box / ppl ! distance between particles

        X: do i = 0, ppl - 1
            Y: do j = 0, ppl - 1
                Z: do k = 0, ppl - 1
                    state(1,num) = 1
                    state(2,num) = dist * (i + 0.5)
                    state(3,num) = dist * (j + 0.5)
                    state(4,num) = dist * (k + 0.5)
                    state(5,num) = 1.0
                    state(6,num) = 0
                    state(7,num) = -1.0

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

    subroutine finalise()
        deallocate(state, params, forces)
    end subroutine

    subroutine print_system()
      ! Prints the SYSTEM out for debugging purposes
      integer :: i
      do i = 1, N
         write(*, *) state(:, i)
      end do
    end subroutine print_system
    
end module SYSTEM

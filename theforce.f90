module theforce
    use constants
    use system
    implicit none

contains
     subroutine force(energies)
       ! subroutine to calculate the new forces between particle pairs
       ! i and j from the positions

       ! Declare arguments
       real(dp), dimension(:), intent(out) :: energies
       
       ! Declare local variables
       integer :: i, j ! loop indices
       integer :: pos ! index for energies vector
       

       ! Copy old forces into forces_prev
       ! and then initialise current forces to zero
       ploop: do i = 1, N
          forces(:, i) = 0.0
       end do ploop
       
       ! Loop over all particle pairs, calculating forces and energies
       pos = 1
       outer: do i = 1, N-1
          inner: do j = i+1, N

             ! Calculate the force and energy contributions due to this pair
             call lennard_jones(i, j, energies(pos))
             pos = pos + 1 ! Increment where we are in the energies vector

          end do inner
       end do outer
       
     end subroutine
       
     subroutine lennard_jones(i,j, energy)
        ! subroutine to calculate Lennard-Jones interaction energies
        ! and forces between particle pairs i and j

        ! Declare input and output variables
        integer, intent(in) :: i, j
        real(dp), intent(out) :: energy

        ! Declare variables used only in the subroutine
        real(dp) :: sigmai, sigmaj, sigma
        real(dp) :: epsili, epsilj, epsil
        real(dp) :: dist, ratio, forcecoeff

        ! Declare counter variables
        integer :: k
        real(dp), dimension(3) :: rij

        ! Set variables from THE SYSTEM to be used within the subroutine
        sigmai = params(3,i)
        sigmaj = params(3,j)
        epsili = params(2,i)
        epsilj = params(2,j)

        ! Use Lorentz-Berthelot Rules to create a combined sigma and epsilon
        sigma = (sigmai + sigmaj) / 2
            ! Don't calculate for energies if they're the same)
        if (epsili == epsilj) THEN
            epsil = epsili
        else
            epsil = sqrt(epsili * epsilj)
        endif
        !write(*,*) " Combined epsilon", epsil

        ! Calculate interparticle displacement rij as vector
        call distance_calculator(i,j,rij)
        
        ! Now calculate magnitude of rij vector
        dist = dot_product(rij,rij)
        !write(*,*) dist

        ! Calculate Lennard-Jones Interaction Energy
        
        ! It is useful to have the ratio of sigma^6 to r^6
        ratio = (sigma ** 6) / (dist ** 3)
        
        energy = 4 * epsil * ( (ratio ** 2) - ratio )
        ! Calculate forces, only for particles which are within the cutoff
        ! distance of each other
        if (dist < r_cut) then
            forcecoeff = 48 * epsil * (1/dist) * ( ( ratio ** 2 ) - ratio / 2 )
            
            do k=1, 3, 1
               forces(k, i) = forces(k, i) + forcecoeff*rij(k)
               forces(k, j) = forces(k, j) - forcecoeff*rij(k)
            enddo
        endif
           
        !write(*,*) energy
      end subroutine lennard_jones
   
   subroutine distance_calculator(i,j,rij)
        ! This subroutine will calculate distance taking into account periodic
        ! boundary conditions

        ! Declare input and output variables
        integer, intent(in) :: i, j
        real(dp), intent(out), dimension(3) :: rij
        
        ! Declare variables to be used within the subroutine
        real(dp), dimension(3) :: posi, posj

        ! Declare counting variables
        integer :: k

        ! (Move the x, y and z components of i and j into a vector)
        posi = positions(:, i)
        posj = positions(:, j)
        !posi(1) = state(2,i)
        !posi(2) = state(3,i)
        !posi(3) = state(4,i)
        !posj(1) = state(2,j)
        !posj(2) = state(3,j)
        !posj(3) = state(4,j)

        ! Calculate rij as a vector, taking into account pbcs
        do k = 1, 3, 1
            rij(k) = (posi(k) - posj(k)) - nint((posi(k) - &
            posj(k)) / box) * box
        end do        

      end subroutine distance_calculator

      
end module theforce

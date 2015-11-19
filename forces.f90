module forces
    use constants
    implicit none

contains
    subroutine lennard_jones(i,j, energy)
        implicit none
        ! subroutine to calculate Lennard-Jones interaction energies between
        ! particle pairs i and j

        ! Declare input and output variables
        integer, intent(in) :: i, j
        real(dp), intent(out) :: energy

        ! Declare variables used only in the subroutine
        real(dp) :: sigmai, sigmaj, sigma
        real(dp) :: epsili, epsilj, epsil
        real(dp) :: dist, ratio, forcecoeff
        real(dp), dimension(3) :: posi, posj, force
        ! Declare counter variables
        integer :: k
        ! TEST VARIABLES
        real(dp), dimension(3,2) :: params
        real(dp), dimension(3) :: rij
        params(2,1) = 8
        params(2,2) = 2
        params(3,1) = 3
        params(3,2) = 1



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
        write(*,*) " Combined epsilon", epsil

        ! Calculate interparticle displacement rij as vector
        call distance_calculator(i,j,rij)

        ! Now calculate magnitude of rij vector
        dist = dot_product(rij,rij)
        write(*,*) dist

        ! Calculate Lennard-Jones Interaction Energy
            ! It is useful to have the ratio of sigma^6 to r^6
        ratio = (sigma ** 6) / (dist ** 3)
        energy = 4 * epsil * ( (ratio ** 2) - ratio )
        ! Calculate forces
        forcecoeff = 48 * epsil * (1/dist) * ( ( ratio ** 2 ) - ratio / 2 )
        do k=1, 3, 1
            force(k)=forcecoeff*rij(k)
        enddo 
        write(*,*) energy
        write(*,*) force



    end subroutine

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

        ! SET TESTING VARIABLES
        real(dp), dimension(4,2) :: state
        real(dp) :: bwidth        
        state(2,1) = 0
        state(3,1) = 0
        state(4,1) = 0
        state(2,2) = 8
        state(3,2) = 4
        state(4,2) = 15
        bwidth = 100.0

        ! (Move the x, y and z components of i and j into a vector)
        posi(1) = state(2,i)
        posi(2) = state(3,i)
        posi(3) = state(4,i)
        posj(1) = state(2,j)
        posj(2) = state(3,j)
        posj(3) = state(4,j)

        ! Calculate rij as a vector, taking into account pbcs
        do k = 1, 3, 1
            rij(k) = (posi(k) - posj(k)) - nint((posi(k) - &
            posj(k)) / bwidth) * bwidth
        end do        
       
        
    end subroutine
end module forces

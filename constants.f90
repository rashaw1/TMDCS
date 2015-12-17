module constants
    implicit none
   
    integer, parameter :: dp = selected_real_kind(15,307)

    ! Value for pi
    real(dp), parameter :: PI = 3.141592653589793238462
    
    !Define derived type variable for atoms
    type atom
        character(len = 3) :: symbol    ! Chemical Symbol
        real(dp) :: mass                ! Mass relative to that of Argon
    end type atom


    ! Definition of scaling factors for unit systems
    real(dp), parameter :: k_B = 1.38064852e-23 ! Boltzmann Constant
    real(dp), parameter :: nm = 10e-9 ! Nanometre unit of length
    real(dp), parameter :: fs = 10e-12 ! Femtosecond unit of time
    real(dp), parameter :: ArMass = 39.948 ! Mass of argon in a.m.u
    
    type(atom), dimension(24) :: atomarray = [ &
        atom("H", 1.00794 / ArMass), &
        atom("He", 4.002602 / ArMass), &
        atom("Li", 6.941 / ArMass), &
        atom("Be", 9.012182 / ArMass), &
        atom("B", 10.811 / ArMass), &
        atom("C", 12.011 / ArMass), &
        atom("N", 14.00674 / ArMass), &
        atom("O", 15.9994 / ArMass), &
        atom("F", 18.9984032 / ArMass), &
        atom("Ne", 20.1797 / ArMass), &
        atom("Na", 22.989768 / ArMass), &
        atom("Mg", 24.3050 / ArMass), &
        atom("Al", 26.981539 / ArMass), &
        atom("Si", 28.0855 / ArMass), &
        atom("P", 30.973762 / ArMass), &
        atom("S", 32.066 / ArMass), &
        atom("Cl", 35.4527 / ArMass), &
        atom("Ar", 39.948 / ArMass), &
        atom("K", 39.0983 / ArMass), &
        atom("Ca", 40.078 / ArMass), &
        atom("Br", 79.904 / ArMass), &
        atom("I", 126.90447 / ArMass), &
        atom("Kr", 83.80 / ArMass), &
        atom("Xe", 131.29 / ArMass) &
    ]

end module

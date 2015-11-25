module input
!    use constants
!    implicit none

!    type :: system_vars
!        integer :: N, iter_tot
!        real(dp) :: T, dt, box, r_cut
!    end type system_vars


    integer :: file_geom, file_run, file_dat
    
!    character(32) :: force_name, integrator_name, thermostat_name
!    logical :: output_geom, output_jmol

!    output_geom = .true.
!    output_jmol = .true.

contains
    subroutine open_files(filename)
        character(32), intent(in) :: filename

        open(newunit=file_geom, name=filename // ".geom", status='old', &
            action='read')
        open(newunit=file_run,  name=filename // ".run",  status='old', &
            action='read')
        open(newunit=file_dat,  name=filename // ".dat",  status='old', &
            action='read')
    end subroutine

!    subroutine close_files()
!        close(file_geom)
!        close(file_run)
!        close(file_dat)
!    end subroutine

!    function read_runfile()
!        type(system_vars) :: vars
!        
!    end function
    
end module

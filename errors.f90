module errors
    use iso_fortran_env
    implicit none
    integer :: logfile
    logical :: dead
  
contains
    subroutine errors_init(filename)
        character(*), intent(in) :: filename
        integer :: ioerr
        open(newunit = logfile, file = filename // ".log", action = 'write', &
            iostat = ioerr)

        if (ioerr > 0) then
            ! could not even open the logfile
            ! write to stderr and exit
            write(error_unit, "(A)") "ERROR: errors could not open logfile"
            call exit(1)
        end if

        write(logfile, "(A)") "Begin"
    end subroutine
    
    subroutine errors_final()
        integer :: ioerr
        write(logfile, "(A)") "End"
        close(logfile, iostat = ioerr)

        if (ioerr > 0) then
            ! could not close the logfile
            ! write to stderr
            write(error_unit, "(A)") "ERROR: errors could not close logfile"
        end if
    end subroutine

    subroutine throw_error(program_unit, message)
        character(*), intent(in) :: program_unit
        character(*), intent(in) :: message
        character(80) :: error_string

        error_string = "ERROR: " // program_unit // " " // message
        
        ! write errors to both stderr and logfile
        write(logfile, "(A)") error_string
        write(error_unit, "(A)") error_string

!        write(logfile, 100) 'ERROR:', program_unit, message
!100 format(1X, A10, A10, A60)

        dead = .true.
    end subroutine throw_error

    subroutine throw_warning(message)
        character(*), intent(in) :: message

        write(logfile, 200) 'WARNING:', message

200 format(1X, A10, A60)
    end subroutine throw_warning

end module errors

module errors
    use iso_fortran_env
    implicit none
    logical :: dead
    integer :: logfile
    integer :: verbosity = 4

    ! verbosity ranges from 0 upwards
    ! 0: only write out fatal errors
    ! 1: only write out errors and warnings
    ! 2 and up: write out increasingly less important messages
  
contains
    subroutine errors_init(filename)
        character(*), intent(in) :: filename
        integer :: ioerr
        open(newunit = logfile, file = trim(filename) // ".log", &
            action = "write", iostat = ioerr)

        if (ioerr > 0) then
            ! could not even open the logfile
            ! write to stderr and exit
            write(error_unit, "(A)") "ERROR: errors could not open logfile"
            call exit(1)
        end if

        call throw_log("Beginning TMDCS")
    end subroutine
    
    subroutine errors_final()
        integer :: ioerr
        call throw_log("Ending TMDCS")
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
        character(80) :: warn_string
        
        if (verbosity >= 0) then
            warn_string = "WARNING: " // message

            !write warnings to both stderr and logfile
            write(logfile, "(A)") warn_string
            write(error_unit, "(A)") warn_string
        end if

!        write(logfile, 200) 'WARNING:', message
!200 format(1X, A10, A60)
    end subroutine throw_warning

    subroutine throw_log(message, min_verbosity)
        ! writes a message to the log file
        ! min_verbosity defaults to 2, and should be 2 or higher
        character(*), intent(in) :: message
        integer, optional :: min_verbosity
        integer :: verb

        if (present(min_verbosity)) then
            verb = min_verbosity
        else
            verb = 2
        end if
        
        if (verbosity >= verb) then
            write(logfile, "(A)") message
        end if
    end subroutine

end module errors

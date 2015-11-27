module errors
  implicit none
  integer :: logfile
  logical :: dead
  
contains
  subroutine throw_error(program_unit, message)
    character(:), intent(in) :: program_unit
    character(:), intent(in) :: message

    write(logfile, 100) 'ERROR:', program_unit, message
100 format(1X, A10, A10, A60)

    dead = .true.
  end subroutine throw_error

  subroutine throw_warning(message)
    character(:), intent(in) :: message

    write(logfile, 200) 'WARNING:', message

200 format(1X, A10, A60)
  end subroutine throw_warning

end module errors

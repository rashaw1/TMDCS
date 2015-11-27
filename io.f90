module io
  use errors
  implicit none

contains
  subroutine open_file(filestream, filename, readwrite)
    ! Opens a file to a given filestream
    integer, intent(out) :: filestream
    integer, intent(in) :: readwrite ! 0 for read, 1 for write, 2 for both
    character(*), intent(in) :: filename
    integer :: ioerr ! Check status of opening file

    if (readwrite == 0) then
       open(newunit=filestream, file=filename, action='read', iostat=ioerr)
    else if (readwrite == 1) then
       open(newunit=filestream, file=filename, action='write', iostat=ioerr)
    else
       open(newunit=filestream, file=filename, action='readwrite', iostat=ioerr)
    end if

    ! Check that opening was successful
    if (ioerr > 0) call throw_error('IO', 'Failed to open file ' // filename)
  end subroutine open_file

end module io


    
    
    
    

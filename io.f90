module io
    use errors
    use system
    implicit none

contains

    subroutine open_file(filestream, filename, readwrite)
        ! Opens a file to a given filestream
        integer, intent(out) :: filestream
        integer, intent(in) :: readwrite ! 0 for read, 1 for write, 2 for both
        character(9) :: mode
        character(*), intent(in) :: filename
        integer :: ioerr ! Check status of opening file

        if (readwrite == 1) then
            mode = "write"
        else if (readwrite == 2) then
            mode = "readwrite"
        else
            mode = "read"
        end if
        
        open(newunit=filestream, file=trim(filename), action=mode, iostat=ioerr)
            
        ! Check that opening was successful
        if (ioerr > 0) call throw_error('IO', 'Failed to open file ' // filename)
    end subroutine open_file

    character(20) function inttostr(num)
        !converts an integer into a string
        integer, intent(in) :: num
        write(inttostr, *) num
        inttostr = adjustl(inttostr)
    end function
end module io

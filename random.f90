module random
    use constants
    implicit none
contains
    subroutine random_init()
        integer :: t, n, i
        integer, allocatable, dimension(:) :: seed

        call random_seed(size = n)
        allocate(seed(n))
        call system_clock(t)

        seed = t + 37 * [ (i, i = 1, n) ]
        call random_seed(put = seed)
        deallocate(seed)
    end subroutine

    real(dp) function random_real(lo_in, hi_in)
        real(dp), intent(in), optional :: lo_in, hi_in
        real(dp) :: lo, hi

        if (present(lo_in)) then
            lo = lo_in
        else
            lo = 0
        end if

        if (present(hi_in)) then
            hi = hi_in
        else
            hi = 1
        end if

        call random_number(random_real)

        random_real = random_real * (hi - lo) + lo
    end function
end module

    

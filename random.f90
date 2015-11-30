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

    real(dp) function random_normal(mean, stddev)
        ! Gives a random number sampled from a normal
        ! distribution N(mean, stddev)
        real(dp), intent(in) :: mean, stddev
        real(dp) :: r, theta, sigma
        real(dp), dimension(2) :: temp

        if (stddev == 0d0) then
           sigma = 1d0
        else if (stddev < 0d0) then
           sigma = -1*stddev
        else
           sigma = stddev
        end if

        call random_number(temp)
        r = sqrt(-2.0d0*log(temp(1)))
        theta = 2.0d0*PI*temp(2)
        random_normal = mean+sigma*r*sin(theta)
    end function random_normal  
                
end module random

    

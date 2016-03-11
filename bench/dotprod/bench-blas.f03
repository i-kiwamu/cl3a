program main
  implicit none
  integer :: n, m
  character(len=10) :: argv

  call get_command_argument(1, argv)
  read (argv, *) n
  call get_command_argument(2, argv)
  read (argv, *) m
  call perf(n, m)
end program main

subroutine perf(n, m)
  implicit none
  integer, parameter :: dp = kind(0.d0)
  integer, intent(in) :: n, m
  real(dp), external :: ddot
  real(dp) :: va(n), vb(n), x
  integer :: i, t1, t2, t_rate, t_max, res(m)

  call random_number(va)
  call random_number(vb)

  do i = 1, m
     call system_clock(t1)
     x = ddot(n, va, 1, vb, 1)
     call system_clock(t2, t_rate, t_max)
     if (t2 < t1) then
        res(i) = (t_max - t1) + t2 + 1
     else
        res(i) = t2 - t1
     end if
  end do
  print "(A, I8, A, F10.6, A)", "Total elapsed time (" &
       & , m, " repeat): ", sum(res) / dble(t_rate), " sec."
end subroutine perf

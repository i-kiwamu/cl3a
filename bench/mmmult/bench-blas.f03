program main
  implicit none
  integer :: n, m
  character(len=20) :: argv

  call openblas_set_num_threads(1)
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
  real(dp) :: ma(n, n), mb(n, n), mc(n, n)
  integer :: i, t1, t2, t_rate, t_max, res

  call random_number(ma)
  call random_number(mb)

  call system_clock(t1)
  do i = 1, m
     call dgemm('n', 'n', n, n, n, 1.d0, ma, n, mb, n, 0.d0, mc, n)
  end do
  call system_clock(t2, t_rate, t_max)

  if (t2 < t1) then
     res = (t_max - t1) + t2 + 1
  else
     res = t2 - t1
  end if

  print "(A, I8, A, F10.6, A)", "Total elapsed time (" &
       & , m, " repeat): ", res / dble(t_rate), " sec."
end subroutine perf

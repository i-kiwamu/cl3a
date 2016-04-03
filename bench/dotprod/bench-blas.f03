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
  real(dp), external :: ddot
  real(dp) :: va(n), vb(n), x
  integer :: i, t1, t2, t_rate, t_max, res

  call random_number(va)
  call random_number(vb)
  call system_clock(t1)
  do i = 1, m
     x = ddot(n, va, 1, vb, 1)
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

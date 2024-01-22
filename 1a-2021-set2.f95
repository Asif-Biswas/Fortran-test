program FixedPointIteration
  implicit none

  real(8) :: x0, x1, epsilon
  integer :: maxIterations, i

  ! Initial guess
  x0 = 1.0
  ! Tolerance level
  epsilon = 0.001
  ! Maximum number of iterations
  maxIterations = 100

  do i = 1, maxIterations
    x1 = g(x0)

    ! Check for convergence
    if (abs(x1 - x0) < epsilon) then
      write(*, *) 'Root found:', x1
      exit
    end if

    x0 = x1
  end do

  write(*, *) 'Root not found within the specified tolerance in ', maxIterations, ' iterations.'

contains

  function g(x) result(y)
    real(8), intent(in) :: x
    real(8) :: y

    ! Define the fixed-point iteration function
    y = 1.0 / (x*x + 1.0)

  end function g

end program FixedPointIteration

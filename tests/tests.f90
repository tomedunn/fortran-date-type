!=========================================================================================
program main
!=========================================================================================
  use date_type
  implicit none
  ! local variables:
  logical, allocatable :: results(:)

  write(*,'("function isLeapYear:")')
  call TEST_isLeapYear(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("function timeFrom:")')
  call TEST_timeFrom(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

contains


!=========================================================================================
! TEST_isLeapYear:
!
  subroutine TEST_isLeapYear( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 2
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = now()
    call d%setYear(2000)
    results(1) = d%isLeapYear()
    call d%setYear(2001)
    results(2) = .not.d%isLeapYear()
  end subroutine TEST_isLeapYear
!=========================================================================================


!=========================================================================================
! TEST_timeFrom:
!
  subroutine TEST_timeFrom( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 2
    ! local variables:
    type(date) :: d1, d2

    allocate(results(N_TESTS))

    d1 = now()
    call d1%setYear(2000)
    d2 = d1
    call d2%setYear(2001)
    results(1) =  d2%timeFrom(d1) == real(365*24*60*60, 8)
    results(2) = -d1%timeFrom(d2) == real(365*24*60*60, 8)
  end subroutine TEST_timeFrom
!=========================================================================================
end program main
!=========================================================================================

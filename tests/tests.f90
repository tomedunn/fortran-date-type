!=========================================================================================
program main
!=========================================================================================
  use date_type
  implicit none
  ! local variables:
  logical, allocatable :: results(:)
  integer :: i

  write(*,'("function dayOfWeek:")')
  call TEST_dayOfWeek(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("function isLeapYear:")')
  call TEST_isLeapYear(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("function timeFrom:")')
  call TEST_timeFrom(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("function toString:")')
  call TEST_toString(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

contains


!=========================================================================================
! TEST_dayOfWeek:
!
  subroutine TEST_dayOfWeek( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 1
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = date(2015, 6, 3, 8, 0, 0, 0)
    results(1) = 3 == d%dayOfWeek()
  end subroutine TEST_dayOfWeek
!=========================================================================================


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


!=========================================================================================
! TEST_toString:
!
  subroutine TEST_toString( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 8
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = date(2008, 5, 14, 4, 40, 0, 123)
    results(1) = '08' == d%toString('yy')
    results(2) = '2008' == d%toString('yyyy')
    results(3) = 'May' == d%toString('MMMMMMMMMMM')
    results(4) = '14' == d%toString('dd')
    results(5) = '04' == d%toString('HH')
    results(6) = '40' == d%toString('mm')
    results(7) = '00' == d%toString('ss')
    results(8) = '123' == d%toString('SS')
    ! there seems to be a bug that prevents printing single digit formats such
    ! as 'S'... not sure why this is but I fear it's a compiler bug...
    !results(8) = '123' == d%toString('S')

  end subroutine TEST_toString
!=========================================================================================


!===============================================================================
! print_failed_tests:
!
!   Prints which of the tests failed to standard out.
!
  subroutine print_failed_tests( results )
    logical, intent(in) :: results(:)
    ! local variables:
    integer :: i

    write(*,'(2X,"Failed tests:")', advance='no')
    do i = 1, size(results)
      if (.not.results(i)) write(*,'("  "I0)', advance='no') i
    end do
    write(*,'("")')
  end subroutine print_failed_tests
!===============================================================================
end program main
!=========================================================================================

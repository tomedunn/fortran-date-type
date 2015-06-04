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
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("function timeFrom:")')
  call TEST_timeFrom(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("function toString:")')
  call TEST_toString(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("subroutine getDayOfWeek:")')
  call TEST_getDayOfWeek(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("subroutine getMonth:")')
  call TEST_getMonth(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("subroutine setDayOfWeek:")')
  call TEST_setDayOfWeek(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

  write(*,'("subroutine setMonth:")')
  call TEST_setMonth(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)
  if (count(results) /= size(results)) call print_failed_tests(results)

contains


!=========================================================================================
! function: TEST_isLeapYear:
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
! function: TEST_timeFrom:
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
! function: TEST_toString:
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


!=========================================================================================
! subroutine: TEST_getDayOfWeek:
!
  subroutine TEST_getDayOfWeek( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 7
    ! local variables:
    type(date)   :: d
    character(9) :: day_name

    allocate(results(N_TESTS))

    d = date(2015, 6, 07, 8, 0, 0, 0)
    call d%setDayOfWeek(1)
    call d%getDayOfWeek(day_name)
    results(1) = day_name == 'Sunday'

    call d%setDayOfWeek(2)
    call d%getDayOfWeek(day_name)
    results(2) = day_name == 'Monday'

    call d%setDayOfWeek(3)
    call d%getDayOfWeek(day_name)
    results(3) = day_name == 'Tuesday'

    call d%setDayOfWeek(4)
    call d%getDayOfWeek(day_name)
    results(4) = day_name == 'Wednesday'

    call d%setDayOfWeek(5)
    call d%getDayOfWeek(day_name)
    results(5) = day_name == 'Thursday'

    call d%setDayOfWeek(6)
    call d%getDayOfWeek(day_name)
    results(6) = day_name == 'Friday'

    call d%setDayOfWeek(7)
    call d%getDayOfWeek(day_name)
    results(7) = day_name == 'Saturday'
  end subroutine TEST_getDayOfWeek
!=========================================================================================


!=========================================================================================
! subroutine: TEST_getMonth:
!
  subroutine TEST_getMonth( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 12
    ! local variables:
    type(date)   :: d
    character(9) :: month_name

    allocate(results(N_TESTS))

    d = date(2015, 1, 1, 1, 0, 0, 0)
    call d%setMonth(01)
    call d%getMonth(month_name)
    results(01) = month_name == 'January'

    call d%setMonth(02)
    call d%getMonth(month_name)
    results(02) = month_name == 'February'

    call d%setMonth(03)
    call d%getMonth(month_name)
    results(03) = month_name == 'March'

    call d%setMonth(04)
    call d%getMonth(month_name)
    results(04) = month_name == 'April'

    call d%setMonth(05)
    call d%getMonth(month_name)
    results(05) = month_name == 'May'

    call d%setMonth(06)
    call d%getMonth(month_name)
    results(06) = month_name == 'June'

    call d%setMonth(07)
    call d%getMonth(month_name)
    results(07) = month_name == 'July'

    call d%setMonth(08)
    call d%getMonth(month_name)
    results(08) = month_name == 'August'

    call d%setMonth(09)
    call d%getMonth(month_name)
    results(09) = month_name == 'September'

    call d%setMonth(10)
    call d%getMonth(month_name)
    results(10) = month_name == 'October'

    call d%setMonth(11)
    call d%getMonth(month_name)
    results(11) = month_name == 'November'

    call d%setMonth(12)
    call d%getMonth(month_name)
    results(12) = month_name == 'December'
  end subroutine TEST_getMonth
!=========================================================================================


!=========================================================================================
! subroutine: TEST_setDayOfWeek:
!
  subroutine TEST_setDayOfWeek( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 7
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = date(2015, 6, 1, 8, 0, 0, 0)
    call d%setDayOfWeek('Sunday')
    results(1) = 1 == d%dayOfWeek()

    call d%setDayOfWeek('Monday')
    results(2) = 2 == d%dayOfWeek()

    call d%setDayOfWeek('Tuesday')
    results(3) = 3 == d%dayOfWeek()

    call d%setDayOfWeek('Wednesday')
    results(4) = 4 == d%dayOfWeek()

    call d%setDayOfWeek('Thursday')
    results(5) = 5 == d%dayOfWeek()

    call d%setDayOfWeek('Friday')
    results(6) = 6 == d%dayOfWeek()

    call d%setDayOfWeek('Saturday')
    results(7) = 7 == d%dayOfWeek()
  end subroutine TEST_setDayOfWeek
!=========================================================================================


!=========================================================================================
! subroutine: TEST_setMonth:
!
  subroutine TEST_setMonth( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 12
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = date(2015, 1, 1, 1, 0, 0, 0)
    call d%setMonth('January')
    results(01) = 01 == d%month()

    call d%setMonth('February')
    results(02) = 02 == d%month()

    call d%setMonth('March')
    results(03) = 03 == d%month()

    call d%setMonth('April')
    results(04) = 04 == d%month()

    call d%setMonth('May')
    results(05) = 05 == d%month()

    call d%setMonth('June')
    results(06) = 06 == d%month()

    call d%setMonth('July')
    results(07) = 07 == d%month()

    call d%setMonth('August')
    results(08) = 08 == d%month()

    call d%setMonth('September')
    results(09) = 09 == d%month()

    call d%setMonth('October')
    results(10) = 10 == d%month()

    call d%setMonth('November')
    results(11) = 11 == d%month()

    call d%setMonth('December')
    results(12) = 12 == d%month()
  end subroutine TEST_setMonth
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

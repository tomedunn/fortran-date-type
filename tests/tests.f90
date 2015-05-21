!=========================================================================================
program main
!=========================================================================================
  use date_type
  implicit none
  ! local variables:
  logical, allocatable :: results(:)
  integer :: i

  write(*,'("function isLeapYear:")')
  call TEST_isLeapYear(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("function timeFrom:")')
  call TEST_timeFrom(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,'("function toString:")')
  call TEST_toString(results)
  write(*,'("  passed ",I0,"/",I0," tests.")') count(results), size(results)

  write(*,*) 'day'
  write(*,*) to_string_day( 1, 2)
  write(*,*) to_string_day( 1, 1)
  write(*,*) to_string_day(10, 1)
  write(*,*) ''

  write(*,*) 'hour'
  write(*,*) to_string_hour( 1, 2)
  write(*,*) to_string_hour( 1, 1)
  write(*,*) to_string_hour(10, 1)
  write(*,*) to_string_hour(10, 4)
  write(*,*) ''

  write(*,*) 'minute'
  write(*,*) to_string_minute( 1, 2)
  write(*,*) to_string_minute( 1, 1)
  write(*,*) to_string_minute(10, 1)
  write(*,*) to_string_minute(10, 4)
  write(*,*) ''

  write(*,*) 'month'
  write(*,*) to_string_month( 1, 2)
  write(*,*) to_string_month( 1, 1)
  write(*,*) to_string_month(10, 1)
  write(*,*) to_string_month(10, 4)
  write(*,*) ''

  write(*,*) 'year'
  write(*,*) to_string_year(2008, 2)
  write(*,*) to_string_year(2008, 8)
  write(*,*) ''
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


!=========================================================================================
! TEST_toString:
!
  subroutine TEST_toString( results )
    logical, allocatable, intent(out) :: results(:)
    ! local parameters:
    integer, parameter :: N_TESTS = 1
    ! local variables:
    type(date) :: d

    allocate(results(N_TESTS))

    d = date(2008, 5, 14, 4, 40, 0, 0)
    write(*,'(A)') d%toString('MMMMMMMMMMM')
  end subroutine TEST_toString
!=========================================================================================
end program main
!=========================================================================================

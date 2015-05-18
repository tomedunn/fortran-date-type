!=========================================================================================
module date_type
!=========================================================================================
  implicit none

  private

  ! public derived types:
  public :: date

  ! public functions:
  public :: now

  ! parameters:
  integer, parameter :: LK = 4
  integer, parameter :: IK = 4
  integer, parameter :: RK = 8

  ! derived types:
  type date
    private
    integer(IK) :: year_
    integer(IK) :: month_
    integer(IK) :: day_
    integer(IK) :: hour_
    integer(IK) :: minute_
    integer(IK) :: second_
    integer(IK) :: milisecond_
  contains
    procedure :: isLeapYear => date_is_leap_year
    procedure :: setYear => date_set_year
    procedure :: timeFrom => date_time_from
    procedure :: timeTo => date_time_to
  end type date

  ! interfaces:
  interface date
    module procedure :: constructor
  end interface date

  interface now
    module procedure :: date_now
  end interface now
contains


!=========================================================================================
!  assign_date_to_date:
!
!    Handles how one type(date) is assigned the values of another type(date).
!
  pure subroutine assign_date_to_date( LHS, RHS )
    type(date), intent(inout) :: LHS
    type(date), intent(in)    :: RHS

    LHS%year_       = RHS%year_
    LHS%month_      = RHS%month_
    LHS%day_        = RHS%day_
    LHS%hour_       = RHS%hour_
    LHS%minute_     = RHS%minute_
    LHS%second_     = RHS%second_
    LHS%milisecond_ = RHS%milisecond_
  end subroutine assign_date_to_date
!=========================================================================================


!=========================================================================================
!  constructor:
!
!    Returns a new type(date) set to the current date and time.
!
  function constructor( year, month, day, hour, minute, second, milisecond ) result( d )
    integer(IK), intent(in) :: year
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: day
    integer(IK), intent(in) :: hour
    integer(IK), intent(in) :: minute
    integer(IK), intent(in) :: second
    integer(IK), intent(in) :: milisecond
    type(date) :: d
    ! local variables:
    integer(IK) :: values(8)

    d%year_       = year
    d%month_      = month
    d%day_        = day
    d%hour_       = hour
    d%minute_     = minute
    d%second_     = second
    d%milisecond_ = milisecond
  end function constructor
!=========================================================================================


!=========================================================================================
!  date_is_leap_year:
!
!    Returns .TRUE. if the year for this type(date) is a leap year.
!
  pure function date_is_leap_year( self ) result( ans )
    class(date), intent(in) :: self
    logical(LK) :: ans

    ans = is_leap_year(self%year_)
  end function date_is_leap_year
!=========================================================================================


!=========================================================================================
!  date_now:
!
!    Returns a new type(date) set to the current date and time.
!
  function date_now( ) result( d )
    type(date) :: d
    ! local variables:
    integer(IK) :: values(8)

    call date_and_time(values=values)

    d%year_       = values(1)
    d%month_      = values(2)
    d%day_        = values(3)
    d%hour_       = values(5)
    d%minute_     = values(6)
    d%second_     = values(7)
    d%milisecond_ = values(8)
  end function date_now
!=========================================================================================


!=========================================================================================
!  date_set_year:
!
!    Sets the year for this type(date) to the given year.
!
  pure subroutine date_set_year( self, year )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: year

    self%year_ = year
  end subroutine date_set_year
!=========================================================================================


!=========================================================================================
!  date_time_from:
!
!    Returns .TRUE. if the year for this type(date) is a leap year.
!
  pure function date_time_from( self, date1 ) result( val )
    class(date), intent(in) :: self
    type(date) , intent(in) :: date1
    real(RK) :: val

    val = seconds_between_dates(date1, self)
  end function date_time_from
!=========================================================================================


!=========================================================================================
!  date_time_to:
!
!    Returns .TRUE. if the year for this type(date) is a leap year.
!
  pure function date_time_to( self, date2 ) result( val )
    class(date), intent(in) :: self
    type(date) , intent(in) :: date2
    real(RK) :: val

    val = seconds_between_dates(self, date2)
  end function date_time_to
!=========================================================================================


!=========================================================================================
!  days_in_month:
!
!    Returns the number of days in the given month for the given year.
!
  elemental function days_in_month( month, year ) result( days )
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: year
    integer(IK) :: days
    ! local parameters:
    integer(IK), parameter :: DAYS_REGULAR(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
    integer(IK), parameter :: DAYS_LEAP_YEAR(12) = [31,29,31,30,31,30,31,31,30,31,30,31]

    if (is_leap_year(year)) then
      days = DAYS_LEAP_YEAR(month)
    else
      days = DAYS_REGULAR(month)
    end if
  end function days_in_month
!=========================================================================================


!=========================================================================================
!  days_between_dates:
!
!    Returns the number of days from date1 to date2.
!
  elemental function days_between_dates( date1, date2 ) result( days )
    type(date), intent(in) :: date1
    type(date), intent(in) :: date2
    integer(IK) :: days

    days = days_between_years(date1%year_, date2%year_) &
         + days_to_month(date2%month_, date2%year_)     &
         - days_to_month(date1%month_, date1%year_)     &
         + date2%day_ - date1%day_
  end function days_between_dates
!=========================================================================================


!=========================================================================================
!  days_between_years:
!
!    Returns the number of days from January 1st of first_year to January 1st of
!    second_year.
!
  elemental function days_between_years( first_year, second_year ) result( days )
    integer(IK), intent(in) :: first_year
    integer(IK), intent(in) :: second_year
    integer(IK) :: days
    ! local variables:
    integer(IK) :: nly, y1, y2
    integer(IK) :: leap_years

    ! calculate days (not including leap years
    y1 = min(first_year, second_year)
    y2 = max(first_year, second_year)
    days = 365*(y2 - y1)

    ! account for leap years
    nly = next_leap_year(y1)
    if (nly < y2) days = days + (y2 - nly - 1)/4 + 1

    ! apply sign if second_year comes before first_year
    if (first_year > second_year) days = -days
  end function days_between_years
!=========================================================================================


!=========================================================================================
!  days_to_month:
!
!    Returns the number of days to the first day of the given month from January
!    1st of the given year.
!
  elemental function days_to_month( month, year ) result( days )
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: year
    integer(IK) :: days
    ! local parameters:
    integer(IK), parameter :: DAYS_REGULAR(12) = [0,31,59,90,120,151,181,212,243,273,304,334]
    integer(IK), parameter :: DAYS_LEAP_YEAR(12) = [0,31,60,91,121,152,182,213,244,274,305,335]

    if (is_leap_year(year)) then
      days = DAYS_LEAP_YEAR(month)
    else
      days = DAYS_REGULAR(month)
    end if
  end function days_to_month
!=========================================================================================


!=========================================================================================
!  is_leap_year:
!
!    Returns .TRUE. if the given year is a leap year.
!
  elemental function is_leap_year( year ) result( ans )
    integer(IK), intent(in) :: year
    logical(LK) :: ans

    ans = 0 == mod(year, 4)
  end function is_leap_year
!=========================================================================================


!=========================================================================================
!  next_leap_year:
!
!    Returns the leap year closest, either during or after the current leap
!    year.
!
  elemental function next_leap_year( year ) result( ans )
    integer(IK), intent(in) :: year
    integer(IK) :: ans

    ans = 4*(year/4)
    if (ans < year) ans = year + 4
  end function next_leap_year
!=========================================================================================


!=========================================================================================
!  seconds_between_dates:
!
!    Returns the fractional number of seconds between date1 and date2.
!
  elemental function seconds_between_dates( date1, date2 ) result( seconds )
    type(date), intent(in) :: date1
    type(date), intent(in) :: date2
    real(RK) :: seconds
    ! local variables:
    integer(IK) :: days, hours, minutes

    days    = days_between_dates(date1, date2)
    hours   = 24*days    + date2%hour_   - date1%hour_
    minutes = 60*hours   + date2%minute_ - date1%minute_
    seconds = 60*minutes + date2%second_ - date1%second_
    seconds = seconds + 0.001*(date2%milisecond_ - date1%milisecond_)
  end function seconds_between_dates
!=========================================================================================
end module date_type
!=========================================================================================

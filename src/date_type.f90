!=========================================================================================
module date_type
!=========================================================================================
  implicit none

  private

  ! public derived types:
  public :: date

  ! public functions:
  public :: now
  public :: count_character_reps

  ! parameters:
  integer, parameter :: LK = 4
  integer, parameter :: IK = 4
  integer, parameter :: RK = 8
  character(*), parameter :: MONTH_NAMES(*) = ['January  ','February ','March    ', &
           'April    ','May      ','June     ','July     ','August   ','September', &
           'October  ','Noveber  ','December ']
  integer(IK), parameter :: MONTH_NAME_LENS(*) = [7,8,5,5,3,4,4,6,9,7,7,8]
  integer(IK), parameter :: DAYS_TO_MONTH_REGULAR(12) = [0,31,59,90,120,151,181,212,243,273,304,334]
  integer(IK), parameter :: DAYS_TO_MONTH_LEAP_YEAR(12) = [0,31,60,91,121,152,182,213,244,274,305,335]

  ! derived types:
  type date
    private
    integer(IK) :: year_
    integer(IK) :: month_
    integer(IK) :: day_of_month_
    integer(IK) :: day_of_week_
    integer(IK) :: day_of_year_
    integer(IK) :: hour_
    integer(IK) :: minute_
    integer(IK) :: second_
    integer(IK) :: milisecond_
  contains
    ! get functions:
    procedure :: dayOfMonth => date_return_day_of_month
    procedure :: dayOfWeek => date_return_day_of_week
    procedure :: dayOfYear => date_return_day_of_year
    procedure :: hour => date_return_hour
    procedure :: milisecond => date_return_milisecond
    procedure :: minute => date_return_minute
    procedure :: month => date_return_month
    procedure :: year => date_return_year
    ! get subroutines:
    procedure :: getDayOfMonth => date_get_day_of_month
    procedure :: getDayOfWeek => date_get_day_of_week
    procedure :: getDayOfYear => date_get_day_of_year
    procedure :: getHour => date_get_hour
    procedure :: getMilisecond => date_get_milisecond
    procedure :: getMinute => date_get_minute
    procedure :: getMonth => date_get_month
    procedure :: getYear => date_get_year
    ! set subroutines:
    procedure :: setDayOfMonth => date_set_day_of_month
    !procedure :: setDayOfWeek => date_set_day_of_week
    procedure :: setDayOfYear => date_set_day_of_year
    procedure :: setHour => date_set_hour
    procedure :: setMilisecond => date_set_milisecond
    procedure :: setMinute => date_set_minute
    procedure :: setMonth => date_set_month
    procedure :: setYear => date_set_year
    ! other:
    procedure :: isLeapYear => date_is_leap_year
    procedure :: timeFrom => date_time_from
    procedure :: timeTo => date_time_to
    procedure :: toString => to_string_date
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

    LHS%year_         = RHS%year_
    LHS%month_        = RHS%month_
    LHS%day_of_month_ = RHS%day_of_month_
    LHS%day_of_week_  = RHS%day_of_week_
    LHS%day_of_year_  = RHS%day_of_year_
    LHS%hour_         = RHS%hour_
    LHS%minute_       = RHS%minute_
    LHS%second_       = RHS%second_
    LHS%milisecond_   = RHS%milisecond_
  end subroutine assign_date_to_date
!=========================================================================================


!=========================================================================================
!  constructor:
!
!    Returns a new type(date) set to the given YEAR, MONTH, DAY (of the month),
!    HOUR, MINUTE, SECOND, and MILISECOND.
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

    d%year_         = year
    d%month_        = month
    d%day_of_month_ = day
    d%day_of_week_  = day_of_week(year, month, day)
    d%day_of_year_  = days_to_month(month, year) + day
    d%hour_         = hour
    d%minute_       = minute
    d%second_       = second
    d%milisecond_   = milisecond
  end function constructor
!=========================================================================================


!=========================================================================================
!  date_get_day_of_month:
!
!    Gets the day of the month for this type(date).
!
  pure subroutine date_get_day_of_month( self, day )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: day

    day = self%day_of_month_
  end subroutine date_get_day_of_month
!=========================================================================================


!=========================================================================================
!  date_get_day_of_week:
!
!    Gets the day of the week for this type(date).
!
  pure subroutine date_get_day_of_week( self, day )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: day

    day = self%day_of_week_
  end subroutine date_get_day_of_week
!=========================================================================================


!=========================================================================================
!  date_get_day_of_year:
!
!    Gets the day of the year for this type(date).
!
  pure subroutine date_get_day_of_year( self, day )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: day

    day = self%day_of_year_
  end subroutine date_get_day_of_year
!=========================================================================================


!=========================================================================================
!  date_get_hour:
!
!    Gets the hour for this type(date).
!
  pure subroutine date_get_hour( self, hour )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: hour

    hour = self%hour_
  end subroutine date_get_hour
!=========================================================================================


!=========================================================================================
!  date_get_milisecond:
!
!    Gets the milisecond for this type(date).
!
  pure subroutine date_get_milisecond( self, milisecond )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: milisecond

     milisecond = self%milisecond_
  end subroutine date_get_milisecond
!=========================================================================================


!=========================================================================================
!  date_get_minute:
!
!    Gets the minute for this type(date).
!
  pure subroutine date_get_minute( self, minute )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: minute

    minute = self%minute_
  end subroutine date_get_minute
!=========================================================================================


!=========================================================================================
!  date_get_month:
!
!    Gets the month for this type(date).
!
  pure subroutine date_get_month( self, month )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: month

    month = self%month_
  end subroutine date_get_month
!=========================================================================================


!=========================================================================================
!  date_get_year:
!
!    Gets the year for this type(date).
!
  pure subroutine date_get_year( self, year )
    class(date), intent(in)  :: self
    integer(IK), intent(out) :: year

    year = self%year_
  end subroutine date_get_year
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
    d = date(values(1), values(2), values(3), values(5), values(6), values(7), values(8))
  end function date_now
!=========================================================================================


!=========================================================================================
!  date_return_day_of_month:
!
!    Returns the day of the month for this type(date).
!
  pure function date_return_day_of_month( self ) result( day )
    class(date), intent(in)  :: self
    integer(IK) :: day

    day = self%day_of_month_
  end function date_return_day_of_month
!=========================================================================================


!=========================================================================================
!  date_return_day_of_week:
!
!    Returns the day of the week for this type(date).
!
  pure function date_return_day_of_week( self ) result( day )
    class(date), intent(in)  :: self
    integer(IK) :: day

    day = self%day_of_week_
  end function date_return_day_of_week
!=========================================================================================


!=========================================================================================
!  date_return_day_of_year:
!
!    Returns the day of the year for this type(date).
!
  pure function date_return_day_of_year( self ) result( day )
    class(date), intent(in)  :: self
    integer(IK) :: day

    day = self%day_of_year_
  end function date_return_day_of_year
!=========================================================================================


!=========================================================================================
!  date_return_hour:
!
!    Returns the hour for this type(date).
!
  pure function date_return_hour( self ) result( hour )
    class(date), intent(in)  :: self
    integer(IK) :: hour

    hour = self%hour_
  end function date_return_hour
!=========================================================================================


!=========================================================================================
!  date_return_milisecond:
!
!    Returns the milisecond for this type(date).
!
  pure function date_return_milisecond( self ) result( milisecond )
    class(date), intent(in)  :: self
    integer(IK) :: milisecond

     milisecond = self%milisecond_
   end function date_return_milisecond
!=========================================================================================


!=========================================================================================
!  date_return_minute:
!
!    Returns the minute for this type(date).
!
  pure function date_return_minute( self ) result( minute )
    class(date), intent(in)  :: self
    integer(IK) :: minute

    minute = self%minute_
  end function date_return_minute
!=========================================================================================


!=========================================================================================
!  date_return_month:
!
!    Returns the month for this type(date).
!
  pure function date_return_month( self ) result( month )
    class(date), intent(in)  :: self
    integer(IK) :: month

    month = self%month_
  end function date_return_month
!=========================================================================================


!=========================================================================================
!  date_return_year:
!
!    Returns the year for this type(date).
!
  pure function date_return_year( self ) result( year )
    class(date), intent(in)  :: self
    integer(IK) :: year

    year = self%year_
  end function date_return_year
!=========================================================================================


!=========================================================================================
!  date_set_day_of_month:
!
!    Sets the day for this type(date) to the given day.
!
  pure subroutine date_set_day_of_month( self, day )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: day

    self%day_of_month_ = day
    self%day_of_year_ = days_to_month(self%month_, self%year_) + day
  end subroutine date_set_day_of_month
!=========================================================================================


!=========================================================================================
!  date_set_day_of_year:
!
!    Sets the day for this type(date) to the given day.
!
  pure subroutine date_set_day_of_year( self, day )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: day

    self%day_of_year_ = day
    self%month_ = convert_day_of_year_to_month(day, self%year_)
    self%day_of_month_ = convert_day_of_year_to_day_of_month(self%day_of_year_, &
      self%month_, self%year_)
  end subroutine date_set_day_of_year
!=========================================================================================


!=========================================================================================
!  date_set_hour:
!
!    Sets the hour for this type(date) to the given hour.
!
  pure subroutine date_set_hour( self, hour )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: hour

    self%hour_ = hour
  end subroutine date_set_hour
!=========================================================================================


!=========================================================================================
!  date_set_milisecond:
!
!    Sets the milisecond for this type(date) to the given milisecond.
!
  pure subroutine date_set_milisecond( self, milisecond )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: milisecond

    self%milisecond_ = milisecond
  end subroutine date_set_milisecond
!=========================================================================================


!=========================================================================================
!  date_set_minute:
!
!    Sets the minute for this type(date) to the given minute.
!
  pure subroutine date_set_minute( self, minute )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: minute

    self%minute_ = minute
  end subroutine date_set_minute
!=========================================================================================


!=========================================================================================
!  date_set_month:
!
!    Sets the month for this type(date) to the given month.
!
  pure subroutine date_set_month( self, month )
    class(date), intent(inout) :: self
    integer(IK), intent(in)    :: month

    self%month_ = month
    self%day_of_year_ = days_to_month(self%month_, self%year_) + self%day_of_month_
  end subroutine date_set_month
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
    self%day_of_year_ = days_to_month(self%month_, self%year_) + self%day_of_month_
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
!  count_character_reps:
!
!    Counts the number of continuous characters matching the given character
!    starting from the begining of the given string.
!
  function count_character_reps( str, c ) result( i )
    character(*), intent(in) :: str
    character(1), intent(in) :: c
    integer(IK) :: i

    do i = 1, len(str)
      if (str(i:i) /= c) exit
    end do
    i = i - 1
  end function count_character_reps
!=========================================================================================


!=========================================================================================
!  convert_day_of_year_to_day_of_month:
!
!    Returns the number of the month the given day of year belongs to.
!
  elemental function convert_day_of_year_to_day_of_month( day, month, year ) result( DOM )
    integer(IK), intent(in) :: day
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: year
    integer(IK) :: DOM

    DOM = day - days_to_month(month, year)
  end function convert_day_of_year_to_day_of_month
!=========================================================================================


!=========================================================================================
!  convert_day_of_year_to_month:
!
!    Returns the number of the month the given day of year belongs to.
!
  elemental function convert_day_of_year_to_month( day, year ) result( month )
    integer(IK), intent(in) :: day
    integer(IK), intent(in) :: year
    integer(IK) :: month

    if (is_leap_year(year)) then
      do month = 2, 12
        if (day < DAYS_TO_MONTH_LEAP_YEAR(month)) exit
      end do
    else
      do month = 2, 12
        if (day < DAYS_TO_MONTH_REGULAR(month)) exit
      end do
    end if
  end function convert_day_of_year_to_month
!=========================================================================================


!=========================================================================================
!  day_of_week:
!
!    Returns the day of the week for a given year, month, and day of the month.
!
  pure function day_of_week(year, month, day_of_month) result( day )
    integer(IK), intent(in) :: year
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: day_of_month
    integer(IK) :: day
    ! local parameters:
    integer(IK), parameter :: MONTH_NUMBER_REGULAR(12) = [0,3,3,6,1,4,6,2,5,0,3,5]
    integer(IK), parameter :: MONTH_NUMBER_LEAP_YEAR(12) = [6,2,3,6,1,4,6,2,5,0,3,5]
    ! local variables:
    integer(IK) :: century
    integer(IK) :: month_number
    integer(IK) :: year_number

    century = year/100
    year_number = mod(year, 100)
    if (is_leap_year(year)) then
      month_number = MONTH_NUMBER_LEAP_YEAR(month)
    else
      month_number = MONTH_NUMBER_REGULAR(month)
    end if
    day = mod(day_of_month + month_number + year_number + year_number/4 + century, 7)
  end function day_of_week
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
         + date2%day_of_month_ - date1%day_of_month_
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
!  days_in_month:
!
!    Returns the number of days in the given month for the given year.
!
  elemental function days_in_month( month, year ) result( days )
    integer(IK), intent(in) :: month
    integer(IK), intent(in) :: year
    integer(IK) :: days
    ! local parameters:
    integer(IK), parameter :: DAYS_IN_MONTH_REGULAR(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
    integer(IK), parameter :: DAYS_IN_MONTH_LEAP_YEAR(12) = [31,29,31,30,31,30,31,31,30,31,30,31]

    if (is_leap_year(year)) then
      days = DAYS_IN_MONTH_LEAP_YEAR(month)
    else
      days = DAYS_IN_MONTH_REGULAR(month)
    end if
  end function days_in_month
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

    if (is_leap_year(year)) then
      days = DAYS_TO_MONTH_LEAP_YEAR(month)
    else
      days = DAYS_TO_MONTH_REGULAR(month)
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


!=========================================================================================
!  to_string_date:
!
!    Returns .TRUE. if the year for this type(date) is a leap year.
!
  function to_string_date( self, fmt ) result( str )
    class(date) , intent(in) :: self
    character(*), intent(in) :: fmt
    character(:), allocatable :: str
    ! local variables:
    character :: c
    integer(IK) :: i, n

    i = 1
    str = ''
    do while(i < len(fmt))
      c = fmt(i:i)
      select case(c)
      case default
        n = 1
        str = str//c
      case('d')
        n = count_character_reps(fmt(i:), 'd')
        str = str//to_string_day_of_month(self%day_of_month_, n)
      case('D')
        n = count_character_reps(fmt(i:), 'D')
        str = str//to_string_day_of_year(self%day_of_year_, n)
      case('H')
        n = count_character_reps(fmt(i:), 'H')
        str = str//to_string_hour(self%hour_, n)
      case('M')
        n = count_character_reps(fmt(i:), 'M')
        str = str//to_string_month(self%month_, n)
      case('m')
        n = count_character_reps(fmt(i:), 'm')
        str = str//to_string_minute(self%minute_, n)
      case('S')
        n = count_character_reps(fmt(i:), 'S')
        str = str//to_string_milisecond(self%milisecond_, n)
      case('s')
        n = count_character_reps(fmt(i:), 's')
        str = str//to_string_second(self%second_, n)
      case('y')
        n = count_character_reps(fmt(i:), 'y')
        str = str//to_string_year(self%year_, n)
      end select
      i = i + n
    end do
  end function to_string_date
!=========================================================================================


!=========================================================================================
!  to_string_day_of_month:
!
!    Converts the day of month number to a string.
!
  function to_string_day_of_month( day, n ) result( str )
    integer(IK) , intent(in)  :: day
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(day, max(2, n), n)
  end function to_string_day_of_month
!=========================================================================================


!=========================================================================================
!  to_string_day_of_year:
!
!    Converts the day of the year number to a string.
!
  function to_string_day_of_year( day, n ) result( str )
    integer(IK) , intent(in)  :: day
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(day, max(3, n), n)
  end function to_string_day_of_year
!=========================================================================================


!=========================================================================================
!  to_string_hour:
!
!    Converts the hour number to a string.
!
  function to_string_hour( hour, n ) result( str )
    integer(IK) , intent(in)  :: hour
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(hour, max(2, n), n)
  end function to_string_hour
!=========================================================================================


!=========================================================================================
!  to_string_int:
!
!    Converts the given integer to a string with field width W and M digits.
!
  function to_string_int( i, w, m ) result( str )
    integer(IK) , intent(in)  :: i
    integer(IK) , intent(in)  :: w
    integer(IK) , intent(in)  :: m
    character(:), allocatable :: str
    ! local variables:
    character(len=10) :: fmt
    integer(IK)       :: str_len

    write(fmt,'("(I"I0"."I0")")') w, m
    str_len = max(w, m)
    allocate(character(len=str_len) :: str)
    write(str,fmt) i
    str = trim(adjustl(str))
  end function to_string_int
!=========================================================================================


!=========================================================================================
!  to_string_milisecond:
!
!    Converts the milisecond number to a string.
!
  function to_string_milisecond( milisecond, n ) result( str )
    integer(IK) , intent(in)  :: milisecond
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(milisecond, max(3, n), n)
  end function to_string_milisecond
!=========================================================================================


!=========================================================================================
!  to_string_minute:
!
!    Converts the minute number to a string.
!
  recursive function to_string_minute( minute, n ) result( str )
    integer(IK) , intent(in)  :: minute
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(minute, max(2, n), n)
  end function to_string_minute
!=========================================================================================


!=========================================================================================
!  to_string_month:
!
!    Converts the month number to a string, either a numeric representation if N <= 2
!    or a character representation if N > 2.
!
  function to_string_month( month, n ) result( str )
    integer(IK) , intent(in)  :: month
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str
    ! local variables:
    integer(IK) :: str_len

    if (n <= 2) then
      str = to_string_int(month, max(2, n), n)
    else
      str_len = min(n, 9)
      str = MONTH_NAMES(month)
      str = trim(str(:str_len))
    end if
  end function to_string_month
!=========================================================================================


!=========================================================================================
!  to_string_second:
!
!    Converts the second number to a string.
!
  function to_string_second( second, n ) result( str )
    integer(IK) , intent(in)  :: second
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(second, max(2, n), n)
  end function to_string_second
!=========================================================================================


!=========================================================================================
!  to_string_year:
!
!    Converts the year number to a string.
!
  function to_string_year( year, n ) result( str )
    integer(IK) , intent(in)  :: year
    integer(IK) , intent(in)  :: n
    character(:), allocatable :: str

    str = to_string_int(year, max(4, n), n)
    str = trim(str(len(str)-n+1:))
  end function to_string_year
!=========================================================================================
end module date_type
!=========================================================================================

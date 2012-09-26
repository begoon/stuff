from datetime import datetime, timedelta

# Get current date
now = datetime.today()

print "Current date:", now

# This function returns the day of the week:
# 0 - Mon, 1 - Tue, 2 - Wed, 3 - Thu, 4 - Fri, 5 - Sat, 6 - Sun
day_of_week = now.weekday()

# Offsets in days we need.
# Mon: +1, Tue: +1, Wed: +1, Thu: +1, Fri: +3, Sat: +2, Sun: +1
offsets = [1, 1, 1, 1, 3, 2, 1]

# Take a number of days we need to shift forward.
days_to_add = offsets[day_of_week]

shifted_date = now + timedelta(days = days_to_add)

print "Shifted date:", shifted_date

win32api.SetSystemTime( \
      shifted_date.year, shifted_date.month, shifted_date.day, \
      shifted_date.hour, shifted_date.month, shifted_date.second, \
      shifted_date.microsecond / 1000)

# Note: Python datetime module deals with microsecond, not milliseconds.
# But win32api.SetSystemTime function requires milliseconds, so we
# have to corrent the last argument.

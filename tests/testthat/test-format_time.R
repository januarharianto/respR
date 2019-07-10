

# Works with default arguments
x <- c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11")
expect_error(format_time(x), regexp = NA)
# Works with and outputs vector
expect_is(format_time(x), "numeric")
# Output vector is same length as input
expect_equal(length(format_time(x)), length(x))

# Correctly calculates
expect_equal(format_time(x)[3], 4441)

# Converts day-month-year hour-min
x <- c("03-02-09 01:11", "03-02-09 02:11","03-02-09 02:25")
expect_equal(format_time(x, format = "dmyHM")[3], 4441)

# Converts when AM/PM is present
x <- c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM")
expect_equal(format_time(x, format = "dmyHMSp")[3], 8041)

# Accepts dataframe
x <- data.frame(
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))
expect_error(format_time(x), regexp = NA)

# Adds new column
expect_equal(ncol(x)+1, ncol(format_time(x)))
# Adds new column as LAST column
expect_equal(as.numeric(format_time(x)[3,3]), 4441)
# Outputs data.frame
expect_is(format_time(x), 'data.frame')

# Accepts data table
x <- data.table(
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))
expect_error(format_time(x), regexp = NA)

# Outputs are same class as input
x <- data.table(
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))
expect_is(format_time(x), 'data.table')
x <- data.frame(
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))
expect_is(format_time(x), 'data.frame')

# Uses correct column if not default
x <- data.table(
  w = c("some", "random", "text"),
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))

expect_equal(as.numeric(format_time(x, time = 2)[3,4]), 4441)

# Converts dataframe with 2 separate date and time columns
x <- data.frame(
  w = c("09-02-18", "09-02-18","10-02-18"),
  x = c("22:11:11", "23:11:11","00:25:11"),
  y = c(23, 34, 45),
  z = c(56, 67, 78))
result <- format_time(x, time = c(1,2), format = "dmyHMS")
expect_equal(result[3,5], 8041)

# Converts data table with 2 separate date and time columns
x <- data.table(
  w = c("09-02-18", "09-02-18","10-02-18"),
  x = c("22:11:11", "23:11:11","00:25:11"),
  y = c(23, 34, 45),
  z = c(56, 67, 78))
result <- format_time(x, time = c(1,2), format = "dmyHMS")
expect_equal(as.numeric(result[3,4]), 8041)

# Converts dataframe with 3 separate date and time columns
x <- data.frame(
  w = c("09-02-18", "09-02-18","10-02-18"),
  x = c("22:11", "23:11","00:25"),
  y = c("11", "11", "11"),
  z = c(56, 67, 78))
# select 2 columns, different data-time format
result <- format_time(x, time = c(1,2), format = "dmyHM")
expect_equal(result[3,5], 8041)
# select 3 columns
result <- format_time(x, time = c(1,2,3), format = "dmyHMS")
expect_equal(result[3,5], 8041)

# Works with extra punctuation character
x <- data.frame(
  w = c("09-02-18", "09-02-18","10-02-18"),
  x = c("22:11", "23:11","00:25"),
  y = c(":11", ":11", ":11"),
  z = c(56, 67, 78))
# select 3 columns
result <- format_time(x, time = c(1,2,3), format = "dmyHMS")
expect_equal(result[3,5], 8041)

# Works with NO punctuation characters
x <- data.frame(
  w = c("090218", "090218","100218"),
  x = c("2211", "2311","0025"),
  y = c("11", "11", "11"),
  z = c(56, 67, 78))
# select 3 columns
result <- format_time(x, time = c(1,2,3), format = "dmyHMS")
expect_equal(result[3,5], 8041)

# Works with stupid number of columns
x <- data.frame(
  a = c("09", "09","10"),
  b = c("02", "02","02"),
  c = c("2018", "2018","2018"),
  d = c("22", "23","00"),
  e = c("11", "11","25"),
  f = c("11", "11", "11"),
  g = c(56, 67, 78))
# select 3 columns
result <- format_time(x, time = c(1:6), format = "dmyHMS")
expect_equal(result[3,8], 8041)





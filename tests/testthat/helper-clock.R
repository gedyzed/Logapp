# TRACKER_NOW is an Africa/Addis_Ababa time, for example "2026-11-06 12:59:00".

tracker_local_now <- function(now, env = parent.frame()) {
  withr::local_envvar(TRACKER_NOW = now, .local_envir = env)
}

# Friday 13:00 inclusive through Saturday 17:00 exclusive, Africa/Addis_Ababa.
weekly_window_expected <- function(time) {
  time <- as.POSIXct(time, tz = "Africa/Addis_Ababa")
  local <- as.POSIXlt(time, tz = "Africa/Addis_Ababa")
  weekday <- local$wday
  minutes <- local$hour * 60L + local$min
  (weekday == 5L && minutes >= 13L * 60L) || (weekday == 6L && minutes < 17L * 60L)
}

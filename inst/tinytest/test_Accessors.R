library(FITfileR)

garmin530_file <- system.file("extdata", "Activities", "garmin-edge530-ride.fit", 
                           package = "FITfileR")

## test we can read our example files
expect_silent(
  garmin530 <- readFitFile(garmin530_file)
)

## the 'session' messages in this file mix character and number
## this broke the unit conversion at one point (#13)
expect_silent(
  session <- getMessagesByType(garmin530, "session")
)
expect_inherits(session, "data.frame")

expect_inherits(records(garmin530), "list")
expect_inherits(laps(garmin530), "data.frame")
expect_inherits(events(garmin530), "data.frame")
expect_inherits(file_id(garmin530), "data.frame")
## hrv is not present in this file
expect_null(hrv(garmin530))
library(FITfileR)

garmin_file <- system.file("extdata", "Activities", "garmin-edge530-ride.fit", 
                           package = "FITfileR")
tomtom_file <- system.file("extdata", "Activities", "tomtom-runner3-ride.fit", 
                           package = "FITfileR")

## test we can read our two example files
expect_silent(
    garmin <- readFitFile(garmin_file)
)
## check we got a "FitFile" back
expect_inherits(garmin, "FitFile")

expect_silent(
    tomtom <- readFitFile(tomtom_file)
)
expect_inherits(tomtom, "FitFile")
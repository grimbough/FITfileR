library(FITfileR)

garmin_file <- system.file("extdata", "Activities", "garmin-edge500-ride.fit", 
                           package = "FITfileR")
tomtom_file <- system.file("extdata", "Activities", "tomtom-runner3-ride.fit", 
                           package = "FITfileR")

## test we can read our two example files
expect_silent(
    garmin <- readFitFile(garmin_file)
)

## check we got a list back
expect_true( class(garmin) == "FitFile" )

expect_silent(
    tomtom <- readFitFile(tomtom_file)
)

expect_true( class(tomtom) == "FitFile" )
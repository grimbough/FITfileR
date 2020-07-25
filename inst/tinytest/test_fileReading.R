library(fitFileR)

garmin_file <- system.file("extdata/Garmin.fit", package = "fitFileR")
tomtom_file <- system.file("extdata/TomTom.fit", package = "fitFileR")

## test we can read our two example files
expect_silent(
    garmin <- readFitFile(garmin_file)
)

expect_silent(
    tomtom <- readFitFile(tomtom_file)
)

## check we got a list back
expect_true( class(garmin) == "list" )
expect_true( class(tomtom) == "list" )
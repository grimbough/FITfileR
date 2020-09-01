library(fitFileR)

## Reading files distributed with the FIT SDK

##########################
## settings.fit
#########################
settings_file <- system.file("extdata", "FIT_SDK", "Settings.fit", package = "fitFileR")

expect_silent(
    settings <- readFitFile(settings_file)
)

expect_equal( length(settings), 3L )

user_profile <- getMessagesByType(settings, "user_profile")
expect_equivalent( user_profile$weight, 90 )
expect_equivalent( user_profile$age, 28L )


##########################
## DeveloperData.fit
#########################
devdata_file <- system.file("extdata", "FIT_SDK", "DeveloperData.fit", package = "fitFileR")
expect_silent(
    devdata <- readFitFile(devdata_file)
)

expect_true(
    "doughnuts_earned" %in% names(records(devdata))
)


library(FITfileR)


expect_equivalent(FITfileR:::.translateGlobalMessageNumber(20L), "record")
expect_equivalent(FITfileR:::.translateGlobalMessageName("record"), 20L)

## If a global message number isn't defined in the spec,
## we should get back a list with empty values
expect_equivalent(
    FITfileR:::.translateField(field_definition_number = 1, 
                               global_message_number = 500),
    list(value = '', key = '', type = '', units = NA)
)

####################################################
## Substituting Garmin product ID for their names ##
####################################################

expect_equal(
    FITfileR:::.fixGarminProducts( data.frame(manufacturer = "garmin", product = 2697L) ),
    data.frame(manufacturer = "garmin", product = "fenix5")
)


###########################
## Converting timestamps ##
###########################

## date_time fields should be relative to 1989-12-31 in the UTC timezone
expect_equal(
    FITfileR:::.adjustTimeStamp(0),
    as.POSIXct("1989-12-31", tz = "UTC")
)

########################################
## Handling left/right balance fields ##
########################################

## single value like in the session information
expect_equivalent(
    balance <- FITfileR:::.leftRightAdjustment(37797, type = "left_right_balance_100"),
    50.29
)
expect_equal(attr(balance, "side"), "right")

## vector as in multiple records
input <- c(53, 100, 35)
expect_equivalent(
    balance <- FITfileR:::.leftRightAdjustment(input, type = "left_right_balance"),
    input
)
expect_equal(attr(balance, "side"), "unknown")


library(fitFileR)


expect_equivalent(fitFileR:::.translateGlobalMessageNumber(20L), "record")
expect_equivalent(fitFileR:::.translateGlobalMessageName("record"), 20L)

expect_equivalent(
    fitFileR:::.translateField(field_definition_number = 1, 
                               global_message_number = 500),
    list(value = '', key = '', type = '', units = NA)
)


expect_equal(
    fitFileR:::.fixGarminProducts( data.frame(manufacturer = "garmin", product = 2697L) ),
    data.frame(manufacturer = "garmin", product = "fenix5")
)

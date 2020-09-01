
############################################################
## convert double representing a uint32 into a raw vector ##
############################################################

## Should fail with negative or too large values
expect_error(fitFileR:::.uintToBits(-1))
expect_error(fitFileR:::.uintToBits(2^32))

expect_true(
    is(fitFileR:::.uintToBits(10), "raw")
)

expect_equal(
    fitFileR:::.binaryToInt(fitFileR:::.uintToBits(10)), 
    10
)



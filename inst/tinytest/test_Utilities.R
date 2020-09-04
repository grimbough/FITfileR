
############################################################
## convert double representing a uint32 into a raw vector ##
############################################################

## Should fail with negative or too large values
expect_error(FITfileR:::.uintToBits(-1))
expect_error(FITfileR:::.uintToBits(2^32))

expect_true(
    is(FITfileR:::.uintToBits(10), "raw")
)

expect_equal(
    FITfileR:::.binaryToInt(FITfileR:::.uintToBits(10)), 
    10
)



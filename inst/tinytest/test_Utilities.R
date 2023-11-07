
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

## test the conversion of 8 raw values into int64

if(suppressPackageStartupMessages(require(bit64, quietly = TRUE))) {
    
    input <- list(
        as.raw(rep(0,8)),           # 0
        as.raw(c(1, rep(0,7))),     # 1
        as.raw(c(0,1,0,0,0,0,0,0)), # 256
        as.raw(c(0,0,0,1,0,0,0,0))  # 16777216
    )
    expected_values <- as.integer64(c(0, 1, 256, 16777216))

    for(i in seq_along(input)) {
        expect_equal(FITfileR:::.rawToInt64(input[[i]]),
                     expected_values[i])
    }
}
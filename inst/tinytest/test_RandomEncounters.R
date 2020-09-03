## testing files that have been sent via github issues
## this should only be run locally, and not uploaded

library(fitFileR)

files <- list.files( "~/Projects/personal/fitFileR/inst/extdata/RandomEncounters",
                     full.names = TRUE)

for(f in files) {
    expect_silent(
      fit <- readFitFile(f, mergeMessages = FALSE)
    )
}
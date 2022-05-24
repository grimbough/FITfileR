## testing files that have been sent via github issues
## this should only be run locally, and not uploaded

library(FITfileR)

files <- list.files( "~/Projects/personal/FITfileR/inst/extdata/RandomEncounters",
                     full.names = TRUE)

for(f in files) {
    expect_silent(
      fit <- readFitFile(f)
    )
}

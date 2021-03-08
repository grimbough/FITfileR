# FITfileR 0.1.3

## BUG FIXES

* Left/right balance fields are now reported correctly, rather than as `NA`.
There are many additional data types that will still be returned as `NA`.
Currently only `date_time`, `left_right_balance`, `left_right_balance_100`,
and all enum data types are dealt with correctly. Reported in (#14)

# FITfileR 0.1.2

## BUG FIXES

* Fixed bug in converting units or scaling.  This only occurred when one 
or more fields is a character, resulting in a global type conversion.
Reported in (#13).

# FITfileR 0.1.1

## PERFORMANCE IMPROVEMENTS

* Performance improvements when handling FIT files with many messages that
contain 'developer fields'.  Reported in (#7).

# FITfileR 0.1.0

## NEW FEATURES

* Transition to S4 classes to represent file and messages.
* Package down vignette created.
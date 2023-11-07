# FITfileR 0.1.8

## BUG FIXES

* Version 0.1.7 had a significant bug where the presence of developer data 
would break reading all other data fields in a message.  Reported in 
(#32, #33, #34).  Thanks @dblodgett-cycling for a diagnosis and initial patch.

# FITfileR 0.1.7

## NEW FEATURES

* It is now possible to read messages containing 64-bit integer data types.
Initially reported in (#26) and (#30).

## BUG FIXES

* Fixed an issue where string datatypes that were flagged as having endianness
would not be read correctly resulting in an error.  Reported in (#29).

# FITfileR 0.1.6

## BUG FIXES

* Improved handling of files with multiple developer data message types.
Reported in (#20) and (#23).

# FITfileR 0.1.4

## NEW FEATURES

* Added `monitoring()` accessor function to provide a direct interface to
that message type. Response to (#16).

# FITfileR 0.1.3

## BUG FIXES

* Left/right balance fields are now reported correctly, rather than as `NA`.
There are many additional data types that will still be returned as `NA`.
Currently only `date_time`, `left_right_balance`, `left_right_balance_100`,
and all enum data types are dealt with correctly. Reported in (#14).

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
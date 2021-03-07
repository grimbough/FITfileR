# FITfileR 0.1.2

* Fixed bug in converting units or scaling.  This only occurred when one 
or more fields is a character, resulting in a global type conversion.
Reported in (#13).

# FITfileR 0.1.1

* Performance improvements when handling FIT files with many messages that
contain 'developer fields'.  Reported in (#7).

# FITfileR 0.1.0

* Transition to S4 classes to represent file and messages.
* Package down vignette created.
Reading FIT files
================
Mike Smith

``` r
library(fitFileR)
```

# Reading files

## Example fit files

The files can be found in the `extdata` folder of the package, and
identified on your system using the following code.

``` r
library(fitFileR)
fenix6_file <- system.file("extdata", "Activities", "garmin-fenix6-swim.fit", 
                            package = "fitFileR")
```

## Reading

We read files using the function `readFitFile()`.

``` r
fenix6 <- readFitFile(fenix6_file)
```

The resulting object is an object of type `FitFile` containing all the
data stored in the fit file. Typing the name of the object will print
some details about the original file e.g. the time it was created, the
manufacturer and name of the device it was recorded on, and the number
of data ‘messages’ held within the file.

``` r
fenix6
```

    ## Fit File
    ## ├ File created: 2020-08-28 10:03:37
    ## ├ Device: garmin fenix6
    ## └ Number of data messages: 2141

# Working with the data

If we want to do more than just print a summary of the FIT file to
screen we need to use some accessor function to extract the data from
our FitFile object. There are several ways to achieve this depending on
the datatype you’re interested in.

## Records - GPS, speed, altitude, etc

The data most often wanted from a fit file are the values such as
location, speed, altitude, etc recorded during an activity. Such data
are classed as ‘records’ in the FIT specification and can be retrieved
from the FitFile object using using `records()`.

``` r
fenix6_records <- records(fenix6)
fenix6_records
```

    ## $record_1
    ## # A tibble: 6 x 10
    ##   timestamp           position_lat position_long distance enhanced_speed heart_rate cadence temperature cycles fractional_cadence
    ##   <dttm>                     <dbl>         <dbl>    <dbl>          <dbl>      <int>   <int>       <int>  <int>              <dbl>
    ## 1 2020-08-28 10:03:37         46.8          9.85     0             0             95       0          23      1                  0
    ## 2 2020-08-28 10:03:38         46.8          9.85     0             0             95       0          23      1                  0
    ## 3 2020-08-28 10:03:39         46.8          9.85     0             0             94       0          23      1                  0
    ## 4 2020-08-28 10:03:40         46.8          9.85     0             0             95       0          23      1                  0
    ## 5 2020-08-28 10:03:41         46.8          9.85     0.01          0.009         95       0          23      1                  0
    ## 6 2020-08-28 10:03:42         46.8          9.85     0.03          0.021         96       0          23      1                  0
    ## 
    ## $record_2
    ## # A tibble: 651 x 10
    ##    timestamp           position_lat position_long distance enhanced_speed heart_rate cadence temperature cycles fractional_cadence
    ##    <dttm>                     <dbl>         <dbl>    <dbl>          <dbl>      <int>   <int>       <int>  <int>              <dbl>
    ##  1 2020-08-28 10:03:43         46.8          9.85     0.1           0.069         96       0          23      1                  0
    ##  2 2020-08-28 10:03:44         46.8          9.85     0.19          0.087         97       0          22      1                  0
    ##  3 2020-08-28 10:03:45         46.8          9.85     0.3           0.111         97       0          22      1                  0
    ##  4 2020-08-28 10:03:46         46.8          9.85     0.44          0.143         97       0          22      1                  0
    ##  5 2020-08-28 10:03:47         46.8          9.85     0.6           0.164         99       0          22      1                  0
    ##  6 2020-08-28 10:03:48         46.8          9.85     0.8           0.199        100       0          21      1                  0
    ##  7 2020-08-28 10:03:49         46.8          9.85     1.02          0.217        100       0          21      1                  0
    ##  8 2020-08-28 10:03:50         46.8          9.85     1.3           0.28         103       0          21      2                  0
    ##  9 2020-08-28 10:03:51         46.8          9.85     1.6           0.301        104       0          21      2                  0
    ## 10 2020-08-28 10:03:52         46.8          9.85     1.92          0.313        106      34          20      3                  0
    ## # … with 641 more rows
    ## 
    ## $record_3
    ## # A tibble: 1 x 9
    ##   timestamp           position_lat position_long distance heart_rate cadence temperature cycles fractional_cadence
    ##   <dttm>                     <dbl>         <dbl>    <dbl>      <int>   <int>       <int>  <int>              <dbl>
    ## 1 2020-08-28 10:12:02         180.          180.     408.        140      36          17    238                  0

In this example we actually get a list with three sets of data. This is
because in this particular file there were three distinct definitions of
what a “record” contains. This normally happens if data recording begins
before a sensor (e.g. a heart rate monitor) has been attached to a
device, or GPS position has been acquired, although sometimes the reason
can be more opaque. In this example we are interested in the second
entry, which contains the vast majority of the data. *Note: sometimes
the bulk of your data may be spread across multiple `tibbles` in the
list rather than a single entry. See the “Plotting a route” section
below for an example of how to handle this.*

## Extracting common data types

In addition to the `records()` function, fitFileR provides an number of
other methods for accessing commonly found message types. Currently,
these include:

  - `laps()`
  - `events()`
  - `file_id()`

## Accessing all data types

A complete list of the message types stored within a file can be
accessed via the function `listMessageTypes()`.

``` r
listMessageTypes(fenix6)
```

    ##  [1] "file_id"         "device_settings" "user_profile"    "zones_target"    "sport"           "session"         "lap"             "record"          "event"          
    ## [10] "device_info"     "activity"        "file_creator"    "gps_metadata"

We can see there are 13 different message types in the file above.

If a specific accessor method doesn’t exist for the message type you’re
interested in, you can use the function `getMessagesByType()` and
provide the message type name to the `message_type` argument. The code
below will extract all “zones\_target” messages from our file.

``` r
getMessagesByType(fenix6, message_type = "zones_target")
```

    ## # A tibble: 1 x 5
    ##   functional_threshold_power max_heart_rate threshold_heart_rate hr_calc_type pwr_calc_type
    ##                        <int>          <int>                <int>        <int>         <int>
    ## 1                        275            184                    0            1             1

In this case this is a single message that reports power and heart rate
thresholds that were set on the device. These could then be used in
conjunction with the “records” to measure how well the rider performed
relative to the pre-set threshold for this particular activity.

# Common use cases

## Plotting a route

``` r
edge530_file <- system.file("extdata", "Activities", "garmin-edge530-ride.fit", 
                            package = "fitFileR")
edge530 <- readFitFile(edge530_file)
```

To plot locations we extract the longitude and latitude from our FIT
records. These data are found in ‘record’ messages, and we use
`records()` to extract them. However, unlike the previous example, there
are two different definitions for ‘record’ messages with over one
thousand data points.

``` r
edge530_records <- records(edge530)

## report the number of rows for each set of record messages
vapply(edge530_records, FUN = nrow, FUN.VALUE = integer(1))
```

    ## record_1 record_2 record_3 record_4 record_5 record_6 record_7 
    ##      197        7        9     7063     1326       22        3

We probably don’t want to discard either of these, as even the smaller
one represents over 20 minutes of data recording. We can use **dplyr**
to try and merge all the messages together into a single `tibble`
regardless of their definition. Any entries that are missing in certain
messages will be filled with `NA`. *Note: this approach of binding rows
does not always work, as sometimes the data types within a column may
change between messages, but it is more often successful.*

``` r
library(dplyr)

edge530_allrecords <- records(edge530) %>% 
  bind_rows() %>% 
  arrange(timestamp) 

edge530_allrecords
```

    ## # A tibble: 8,627 x 23
    ##    timestamp           position_lat position_long distance accumulated_pow… altitude speed power heart_rate temperature left_right_bala… left_torque_eff… right_torque_ef…
    ##    <dttm>                     <dbl>         <dbl>    <dbl>            <dbl>    <dbl> <dbl> <int>      <int>       <int>            <int>            <dbl>            <dbl>
    ##  1 2020-07-15 12:31:43         42.9      -0.00165     0                  NA     731.  1.26 65535        255          19               NA              NA               NA 
    ##  2 2020-07-15 12:31:44         42.9      -0.00164     1.35               NA     731.  1.35   142        255          19               NA              NA               NA 
    ##  3 2020-07-15 12:31:45         42.9      -0.00162     2.82               NA     731.  1.46   142        255          19               NA              NA               NA 
    ##  4 2020-07-15 12:31:46         42.9      -0.00160     4.55              425     731.  1.73   142        255          19               NA              NA               NA 
    ##  5 2020-07-15 12:31:47         42.9      -0.00158     6.65              595     731.  2.10   170        255          19              156             128.             128.
    ##  6 2020-07-15 12:31:48         42.9      -0.00154     9.81              851     731.  3.17   256        255          19              181             128.             128.
    ##  7 2020-07-15 12:31:49         42.9      -0.00149    14.0              1093     731.  4.16   242        255          19              183             128.             128.
    ##  8 2020-07-15 12:31:50         42.9      -0.00144    18.3              1093     731.  4.32     0        255          19              182             128.             128.
    ##  9 2020-07-15 12:31:51         42.9      -0.00139    22.3              1093     731.  4.02     0        255          19              182             128.             128.
    ## 10 2020-07-15 12:31:52         42.9      -0.00135    25.8              1093     731.  3.49     0        255          19               NA              NA               NA 
    ## # … with 8,617 more rows, and 10 more variables: left_pedal_smoothness <dbl>, right_pedal_smoothness <dbl>, cadence <int>, fractional_cadence <dbl>, left_pco <int>,
    ## #   right_pco <int>, left_power_phase <list>, left_power_phase_peak <list>, right_power_phase <list>, right_power_phase_peak <list>

We can then use `dplyr::select()` to extract the latitude and longitude
columns from our `tibble`, so we can pass them easily to a plotting
function.

``` r
coords <- edge530_allrecords %>% 
  select(position_long, position_lat)
```

We can now use the **leaflet** package to create an interactive map,
with our route overlayed on top.

``` r
library(leaflet)

m <- coords %>% 
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines( )
    
m
```

    ## qt5ct: using qt5ct plugin
    ## TypeError: Attempting to change the setter of an unconfigurable property.
    ## TypeError: Attempting to change the setter of an unconfigurable property.

![](figuresmapping-1.png)<!-- -->

## Comparing heart rate measurments between devices

The package comes with two example fit files, recorded during a ride in
early 2017. They are of the same ride and record the same rider, but the
data logging was carried out on two different devices: a Garmin Edge 500
and a TomTom Runner 3. These in turn used a chest strap and wrist based
heart rate monitor respectively. Here we compare the heart rates
recorded with two devices, to see how consistent they are with each
other.

First we need to locate the two files and read them into R:

``` r
garmin_file <- system.file("extdata", "Activities", "garmin-edge500-ride.fit", 
                            package = "fitFileR")
tomtom_file <- system.file("extdata", "Activities", "tomtom-runner3-ride.fit", 
                            package = "fitFileR")

garmin <- readFitFile(garmin_file)
tomtom <- readFitFile(tomtom_file)
```

We then use `records()` to extract the appropriate messages from the two
files.

``` r
garmin_records <- records(garmin)
tomtom_records <- records(tomtom)
```

``` r
library(ggplot2)
ggplot() + 
    geom_line(data = garmin_records$record_1, aes(x = timestamp, y = heart_rate), col = 1) + 
    geom_line(data = tomtom_records$record_2, aes(x = timestamp, y = heart_rate), col = 2)
```

![](figuresplot_hr-1.png)<!-- -->

That’s pretty messy as the TomTom data seem to have lots of 255 values
(these are presumably dropouts in the data recording that get assigned
the maximum value). We can filter those entries out to compare the two
traces more easily.

``` r
ggplot() + 
    geom_line(data = garmin_records$record_1, aes(x = timestamp, y = heart_rate), col = 1) + 
    geom_line(data = dplyr::filter(tomtom_records$record_2, heart_rate != 255), 
              aes(x = timestamp, y = heart_rate), col = 2)
```

![](figuresplot-hr-2-1.png)<!-- -->

We can also consider the difference between the two measurements at each
common time point.

``` r
library(zoo)
hr_differences <- inner_join(garmin_records$record_1, tomtom_records$record_2, by = c("timestamp")) %>%
  select(timestamp, heart_rate.x, heart_rate.y) %>% 
  filter(heart_rate.y != 255) %>%
  mutate(hr_diff = heart_rate.x - heart_rate.y) %>%
  mutate(hr_30 = zoo::rollmean(hr_diff, k = 30, fill = NA))

ggplot(hr_differences, aes(x = timestamp, y = hr_diff)) + 
  geom_point(col = 2) +
  geom_line(aes(y = hr_30))
```

    ## Warning: Removed 29 row(s) containing missing values (geom_path).

![](figuresunnamed-chunk-3-1.png)<!-- -->

# Session Info

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Linux Mint 19
    ## 
    ## Matrix products: default
    ## BLAS:   /home/msmith/Applications/R/R-4.0.2/lib/libRblas.so
    ## LAPACK: /home/msmith/Applications/R/R-4.0.2/lib/libRlapack.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] leaflet_2.0.3  zoo_1.8-8      ggplot2_3.3.2  dplyr_1.0.2    fitFileR_0.0.5
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] pillar_1.4.6      compiler_4.0.2    tools_4.0.2       digest_0.6.25     jsonlite_1.7.0    evaluate_0.14     lifecycle_0.2.0   tibble_3.0.3      gtable_0.3.0     
    ## [10] nlme_3.1-149      lattice_0.20-41   mgcv_1.8-32       pkgconfig_2.0.3   rlang_0.4.7       Matrix_1.2-18     cli_2.0.2         rstudioapi_0.11   crosstalk_1.1.0.1
    ## [19] yaml_2.2.1        xfun_0.16         stringr_1.4.0     withr_2.2.0       knitr_1.29        htmlwidgets_1.5.1 generics_0.0.2    vctrs_0.3.2       webshot_0.5.2    
    ## [28] grid_4.0.2        tidyselect_1.1.0  glue_1.4.1        R6_2.4.1          processx_3.4.3    fansi_0.4.1       rmarkdown_2.3     callr_3.4.3       purrr_0.3.4      
    ## [37] farver_2.0.3      magrittr_1.5      ps_1.3.4          htmltools_0.5.0   scales_1.1.1      ellipsis_0.3.1    splines_4.0.2     assertthat_0.2.1  colorspace_1.4-1 
    ## [46] labeling_0.3      utf8_1.1.4        stringi_1.4.6     munsell_0.5.0     crayon_1.3.4

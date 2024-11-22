
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datastreamr.helper

<!-- badges: start -->
<!-- badges: end -->

This package aims to support users in interfacing with the
<a href="https://github.com/datastreamapp/api-docs">DataStream API</a>
through its <a href="https://github.com/datastreamapp/api-docs">R
package</a>. See the documentation there for instructions on requesting
an API key.

## 

## Installation

You can install the development version of datastreamr.helper from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("p-schaefer/datastreamr.helper")
```

## Example

You can Set the API Key in the script, but a more secure solution is to
save the API key as an environmental variable:

``` r
library(datastreamr.helper)

# To set API Key for the current session, use:
# datastreamr::setAPIKey('xxxxxxxxxx')

# Preferably, save the API key as an environmental variable
# then there is no need to run datastreamr::setAPIKey('xxxxxxxxxx')
usethis::edit_r_environ()
#> ☐ Edit
#>   'C:/Users/PatrickSchaefer/OneDrive - Datastream Initiative/Documents/.Renviron'.
#> ☐ Restart R for changes to take effect.
# add DATASTREAM_API_KEY="xxxxxxxxxx" to the file, save, and restart R

# Setting the API key this means users never have to worry about setting the API key again
```

Get valid values for DataStream’s API:

``` r

DS_valid <- DS_valid_vals()

# Valid fields to filter
DS_valid$valid_filter
#> $metadata
#>  [1] "DOI"                    "LocationId"             "ActivityMediaName"     
#>  [4] "ActivityGroupType"      "CharacteristicName"     "MonitoringLocationType"
#>  [7] "ActivityStartYear"      "RegionId"               "LatitudeNormalized"    
#> [10] "LongitudeNormalized"    "DatasetName"            "CreateTimestamp"       
#> 
#> $locations
#>  [1] "DOI"                    "LocationId"             "ActivityMediaName"     
#>  [4] "ActivityGroupType"      "CharacteristicName"     "MonitoringLocationType"
#>  [7] "ActivityStartYear"      "RegionId"               "LatitudeNormalized"    
#> [10] "LongitudeNormalized"    "Name"                  
#> 
#> $observations
#>  [1] "DOI"                    "LocationId"             "ActivityMediaName"     
#>  [4] "ActivityGroupType"      "CharacteristicName"     "MonitoringLocationType"
#>  [7] "ActivityStartYear"      "RegionId"               "LatitudeNormalized"    
#> [10] "LongitudeNormalized"   
#> 
#> $records
#> [1] "DOI"                    "LocationId"             "ActivityMediaName"     
#> [4] "ActivityGroupType"      "CharacteristicName"     "MonitoringLocationType"
#> [7] "ActivityStartYear"      "RegionId"
```

``` r

DS_valid <- DS_valid_vals()

# valid values for lookups
DS_valid$valid_lookup$ActivityMediaName
#> [1] "Ambient Air"              "Ocean Water"             
#> [3] "Porewater"                "Rainwater"               
#> [5] "Stormwater"               "Subsurface Soil/Sediment"
#> [7] "Surface Water"            "Surface Water Sediment"
```

Use standard R operators and formats for API queries with `qs_helper()`:

``` r

qs_helper(
  select=c("ActivityStartDate",
           "ActivityStartTime",
           "CharacteristicName",
           "ResultUnit",
           "ResultValue",
           "ResultDetectionCondition",
           "ResultStatusID",
           "ResultComment",
           "CreateTimestamp"),
  filter=list(
    DOI=c("==","10.25976/0gvo-9d12"),
    CharacteristicName=c("%in%","Specific conductance","pH"),
    ActivityStartYear=c(">",2010),
    LocationId=c("==","862774"),
    ActivityStartYear=c("==","2024")
  ),
  top=5000L
)
#> $`$select`
#> [1] "ActivityStartDate, ActivityStartTime, CharacteristicName, ResultUnit, ResultValue, ResultDetectionCondition, ResultStatusID, ResultComment, CreateTimestamp"
#> 
#> $`$top`
#> [1] 5000
#> 
#> $`$filter`
#> [1] "DOI eq '10.25976/0gvo-9d12' and CharacteristicName in ('Specific conductance', 'pH') and ActivityStartYear gt '2010' and LocationId eq '862774' and ActivityStartYear eq '2024'"
```

Finally, `DS_helper()` will use the above functions to validate and
format inputs into datastreamr endpoints. It will also split filters
into separate queries if too many are specified, and iterate over values
of `$top` in instances where the resulting query is too large.

``` r

loc <- DS_helper(
  ep="locations",
  filter=list(
    DOI=c("==","10.25976/ori9-w562"),
    CharacteristicName=c("%in%","Specific conductance","pH"),
    ActivityStartYear=c(">=",2020)
  )
)

rec <- DS_helper(
  ep="records",
  select=c("MonitoringLocationID",
           "ActivityStartDate",
           "ActivityStartTime",
           "CharacteristicName",
           "ResultUnit",
           "ResultValue",
           "ResultDetectionCondition",
           "ResultStatusID",
           "ResultComment"),
  filter=list(
    DOI=c("==","10.25976/ori9-w562"),
    CharacteristicName=c("%in%","Specific conductance","pH"),
    ActivityStartYear=c(">=",2020),
    LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
  )
)

rec
#> # A tibble: 7 × 9
#>   MonitoringLocationID ActivityStartDate ActivityStartTime CharacteristicName
#>   <chr>                <chr>             <chr>             <chr>             
#> 1 11274                2023-09-26        11:32:00          pH                
#> 2 11252                2023-09-20        10:45:00          pH                
#> 3 11252                2023-11-07        10:15:00          pH                
#> 4 11252                2023-08-15        10:00:00          pH                
#> 5 11274                2023-08-30        10:30:00          pH                
#> 6 11252                2023-10-17        10:00:00          pH                
#> 7 11274                2023-11-14        13:45:00          pH                
#> # ℹ 5 more variables: ResultUnit <chr>, ResultValue <dbl>,
#> #   ResultDetectionCondition <lgl>, ResultStatusID <lgl>, ResultComment <chr>
```

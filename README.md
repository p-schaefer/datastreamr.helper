
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
# install.packages("remotes")
remotes::install_github("p-schaefer/datastreamr.helper")
```

## Example

You can set the API Key in the script, but a more secure solution is to
save the API key as an environmental variable:

``` r
library(datastreamr.helper)

# To set API Key for the current session, use:
if (F) datastreamr::setAPIKey('xxxxxxxxxx')

# Preferably, save the API key as an environmental variable
# then there is no need to run datastreamr::setAPIKey('xxxxxxxxxx')
if (F) usethis::edit_r_environ()
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

# valid values for lookups
DS_valid$valid_lookup$ActivityMediaName
#> [1] "Ambient Air"              "Ocean Water"             
#> [3] "Porewater"                "Rainwater"               
#> [5] "Stormwater"               "Subsurface Soil/Sediment"
#> [7] "Surface Water"            "Surface Water Sediment"
```

The functions in datastreamr.helper accept standard R operators and
formats (i.e., lists and vectors) for API queries with `qs_helper()`.
The `filter` argument requires a named list, with each elements name
corresponding to the field to filter by (i.e.,
`DS_valid$valid_filter[["metadata"]]`). The associated element can be a
list or vector, but the first element of that object must be a standard
R operator:

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
    CharacteristicName=c("%in%",c("Specific conductance","pH")),
    LocationId=c("==","862774"),
    ActivityStartYear=c(">=",2010),
    ActivityStartYear=c("<=","2024")
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
#> [1] "DOI eq '10.25976/0gvo-9d12' and CharacteristicName in ('Specific conductance', 'pH') and LocationId eq '862774' and ActivityStartYear gte '2010' and ActivityStartYear lte '2024'"
```

The `DS_helper()` function will use `qs_helper()` and `DS_valid_vals()`
internally to validate and format inputs for the datastreamr endpoints
(note that `qs_helper()` does not validate inputs, only formats them).
It will also split filters into separate queries if too many are
specified (maximum values specied by `.max_filt`), and iterate over
values of `$top` (via `.top_list`) in instances where the resulting
query is too large.

``` r

loc <- DS_helper(
  ep="locations",
  filter=list(
    DOI=c("==","10.25976/ori9-w562"),
    CharacteristicName=c("%in%",c("Specific conductance","pH")),
    ActivityStartYear=c(">=",2010)
  )
)

rec <- DS_helper(
  ep="records",
  select=c("MonitoringLocationID",
           "ActivityStartDate",
           "CharacteristicName",
           "ResultUnit",
           "ResultValue",
           "ResultDetectionCondition",
           "ResultStatusID",
           "ResultComment"),
  filter=list(
    DOI=c("==","10.25976/ori9-w562"),
    CharacteristicName=c("%in%",c("Specific conductance","pH")),
    ActivityStartYear=c(">=",2010),
    ActivityStartYear=c("<=","2024"),
    LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
  )
)

rec
#> # A tibble: 7 × 8
#>   MonitoringLocationID ActivityStartDate CharacteristicName ResultUnit
#>   <chr>                <chr>             <chr>              <chr>     
#> 1 11274                2023-09-26        pH                 None      
#> 2 11252                2023-09-20        pH                 None      
#> 3 11252                2023-11-07        pH                 None      
#> 4 11252                2023-08-15        pH                 None      
#> 5 11274                2023-08-30        pH                 None      
#> 6 11252                2023-10-17        pH                 None      
#> 7 11274                2023-11-14        pH                 None      
#> # ℹ 4 more variables: ResultValue <dbl>, ResultDetectionCondition <lgl>,
#> #   ResultStatusID <lgl>, ResultComment <chr>
```

Error examples:

``` r

try(
  DS_helper(
    ep="records",
    select=c("Incorrect_Column_Name",
             "MonitoringLocationID",
             "ActivityStartDate",
             "CharacteristicName",
             "ResultUnit",
             "ResultValue",
             "ResultDetectionCondition",
             "ResultStatusID",
             "ResultComment"),
    filter=list(
      DOI=c("==","10.25976/ori9-w562"),
      CharacteristicName=c("%in%",c("Specific conductance","pH")),
    ActivityStartYear=c(">=",2010),
    ActivityStartYear=c("<=","2024"),
      LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
    )
  )
)
#> Error in DS_helper(ep = "records", select = c("Incorrect_Column_Name",  : 
#>   `records` `select` cannot include: 'Incorrect_Column_Name'
```

``` r

try(
  DS_helper(
    ep="records",
    select=c("MonitoringLocationID",
             "ActivityStartDate",
             "CharacteristicName",
             "ResultUnit",
             "ResultValue",
             "ResultDetectionCondition",
             "ResultStatusID",
             "ResultComment"),
    filter=list(
      Incorrect_Column_Name=c("==","something_wrong"),
      DOI=c("==","10.25976/ori9-w562"),
      CharacteristicName=c("%in%",c("Specific conductance","pH")),
      ActivityStartYear=c(">=",2010),
      ActivityStartYear=c("<=","2024"),
      LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
    )
  )
)
#> Error in DS_helper(ep = "records", select = c("MonitoringLocationID",  : 
#>   `records` `filter` cannot include: 'Incorrect_Column_Name'
```

``` r

try(
  DS_helper(
    ep="records",
    select=c("MonitoringLocationID",
             "ActivityStartDate",
             "CharacteristicName",
             "ResultUnit",
             "ResultValue",
             "ResultDetectionCondition",
             "ResultStatusID",
             "ResultComment"),
    filter=list(
      DOI=c("==","10.25976/ori9-w562"),
      CharacteristicName=c("%in%",c("Specific conductance","pH","something_wrong")),
      ActivityStartYear=c(">=",2010),
      ActivityStartYear=c("<=","2024"),
      LocationId=c("%in%", loc$Id) # Note 'Id' used here instead of 'ID'
    )
  )
)
#> Error in purrr::map2(filter, names(filter), function(x, y) { : 
#>   ℹ In index: 2.
#> ℹ With name: CharacteristicName.
#> Caused by error in `.f()`:
#> ! `records` `filter` 'CharacteristicName' cannot include: 'something_wrong'
```

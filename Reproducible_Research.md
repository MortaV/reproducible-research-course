---
title: "Task"
output: 
  html_document:
    code_folding: "show"
    keep_md: true
    css: css_for_assignment.css
    toc: true
    number_sections: TRUE
---

# Summary

<style>
div.blue { background-color : #7FB3D5; color : #ffffff; border-radius: 15px; padding: 15px 10px 10px 10px;}
</style>
<div class = "blue">

Summary of the project

</div>

# Data Processing

First of all, I prepared an environment for myself - uploaded all needed libraries (please check if you have all of them installed before proceeding) and created a theme for *ggplot* graphs.



```r
library(tidyverse)
library(extrafont)
library(scales)
library(lubridate)
library(xtable)
library(knitr)
library(kableExtra)
library(DT)

violet = '#D98880'
blue = '#3169ra'
lightblue = '#7FB3D5'
pink = '#F2D7D5'

theme <- theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    size = 14, 
                                    family = "Kodchasan", 
                                    colour = violet),
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(family = "Montserrat"),
          legend.position = "bottom",
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = blue))
```

Secondly, I uploaded the data directly from the course website, saved it in *data* directory under my working directory and uploaded it to R as *raw_data*. If such directory didn't exist, the code would create it for me. 


```r
data_folder <- "data"

if (!file.exists(data_folder)){
    dir.create(data_folder)
}

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "data/data.csv.bz2")

raw_data <- read_csv("data/data.csv.bz2")
```

Let's check what the data looks like and if we have any unusual values.


```r
glimpse(raw_data)
```

```
## Observations: 902,297
## Variables: 37
## $ STATE__    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
## $ BGN_DATE   <chr> "4/18/1950 0:00:00", "4/18/1950 0:00:00", "2/20/195...
## $ BGN_TIME   <chr> "0130", "0145", "1600", "0900", "1500", "2000", "01...
## $ TIME_ZONE  <chr> "CST", "CST", "CST", "CST", "CST", "CST", "CST", "C...
## $ COUNTY     <dbl> 97, 3, 57, 89, 43, 77, 9, 123, 125, 57, 43, 9, 73, ...
## $ COUNTYNAME <chr> "MOBILE", "BALDWIN", "FAYETTE", "MADISON", "CULLMAN...
## $ STATE      <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL...
## $ EVTYPE     <chr> "TORNADO", "TORNADO", "TORNADO", "TORNADO", "TORNAD...
## $ BGN_RANGE  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ BGN_AZI    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ BGN_LOCATI <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_DATE   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_TIME   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ COUNTY_END <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ COUNTYENDN <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_RANGE  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ END_AZI    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_LOCATI <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ LENGTH     <dbl> 14.0, 2.0, 0.1, 0.0, 0.0, 1.5, 1.5, 0.0, 3.3, 2.3, ...
## $ WIDTH      <dbl> 100, 150, 123, 100, 150, 177, 33, 33, 100, 100, 400...
## $ F          <int> 3, 2, 2, 2, 2, 2, 2, 1, 3, 3, 1, 1, 3, 3, 3, 4, 1, ...
## $ MAG        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ FATALITIES <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 4, 0, ...
## $ INJURIES   <dbl> 15, 0, 2, 2, 2, 6, 1, 0, 14, 0, 3, 3, 26, 12, 6, 50...
## $ PROPDMG    <dbl> 25.0, 2.5, 25.0, 2.5, 2.5, 2.5, 2.5, 2.5, 25.0, 25....
## $ PROPDMGEXP <chr> "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", "...
## $ CROPDMG    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ CROPDMGEXP <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ WFO        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ STATEOFFIC <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ ZONENAMES  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ LATITUDE   <dbl> 3040, 3042, 3340, 3458, 3412, 3450, 3405, 3255, 333...
## $ LONGITUDE  <dbl> 8812, 8755, 8742, 8626, 8642, 8748, 8631, 8558, 874...
## $ LATITUDE_E <dbl> 3051, 0, 0, 0, 0, 0, 0, 0, 3336, 3337, 3402, 3404, ...
## $ LONGITUDE_ <dbl> 8806, 0, 0, 0, 0, 0, 0, 0, 8738, 8737, 8644, 8640, ...
## $ REMARKS    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ REFNUM     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
```

```r
summary(raw_data)
```

```
##     STATE__       BGN_DATE           BGN_TIME          TIME_ZONE        
##  Min.   : 1.0   Length:902297      Length:902297      Length:902297     
##  1st Qu.:19.0   Class :character   Class :character   Class :character  
##  Median :30.0   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :31.2                                                           
##  3rd Qu.:45.0                                                           
##  Max.   :95.0                                                           
##                                                                         
##      COUNTY       COUNTYNAME           STATE              EVTYPE         
##  Min.   :  0.0   Length:902297      Length:902297      Length:902297     
##  1st Qu.: 31.0   Class :character   Class :character   Class :character  
##  Median : 75.0   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :100.6                                                           
##  3rd Qu.:131.0                                                           
##  Max.   :873.0                                                           
##                                                                          
##    BGN_RANGE          BGN_AZI           BGN_LOCATI       
##  Min.   :   0.000   Length:902297      Length:902297     
##  1st Qu.:   0.000   Class :character   Class :character  
##  Median :   0.000   Mode  :character   Mode  :character  
##  Mean   :   1.484                                        
##  3rd Qu.:   1.000                                        
##  Max.   :3749.000                                        
##                                                          
##    END_DATE           END_TIME           COUNTY_END  COUNTYENDN       
##  Length:902297      Length:902297      Min.   :0    Length:902297     
##  Class :character   Class :character   1st Qu.:0    Class :character  
##  Mode  :character   Mode  :character   Median :0    Mode  :character  
##                                        Mean   :0                      
##                                        3rd Qu.:0                      
##                                        Max.   :0                      
##                                                                       
##    END_RANGE          END_AZI           END_LOCATI       
##  Min.   :  0.0000   Length:902297      Length:902297     
##  1st Qu.:  0.0000   Class :character   Class :character  
##  Median :  0.0000   Mode  :character   Mode  :character  
##  Mean   :  0.9862                                        
##  3rd Qu.:  0.0000                                        
##  Max.   :925.0000                                        
##                                                          
##      LENGTH              WIDTH                F               MAG         
##  Min.   :   0.0000   Min.   :   0.000   Min.   :0.0      Min.   :    0.0  
##  1st Qu.:   0.0000   1st Qu.:   0.000   1st Qu.:0.0      1st Qu.:    0.0  
##  Median :   0.0000   Median :   0.000   Median :1.0      Median :   50.0  
##  Mean   :   0.2301   Mean   :   7.503   Mean   :0.9      Mean   :   46.9  
##  3rd Qu.:   0.0000   3rd Qu.:   0.000   3rd Qu.:1.0      3rd Qu.:   75.0  
##  Max.   :2315.0000   Max.   :4400.000   Max.   :5.0      Max.   :22000.0  
##                                         NA's   :843563                    
##    FATALITIES          INJURIES            PROPDMG       
##  Min.   :  0.0000   Min.   :   0.0000   Min.   :   0.00  
##  1st Qu.:  0.0000   1st Qu.:   0.0000   1st Qu.:   0.00  
##  Median :  0.0000   Median :   0.0000   Median :   0.00  
##  Mean   :  0.0168   Mean   :   0.1557   Mean   :  12.06  
##  3rd Qu.:  0.0000   3rd Qu.:   0.0000   3rd Qu.:   0.50  
##  Max.   :583.0000   Max.   :1700.0000   Max.   :5000.00  
##                                                          
##   PROPDMGEXP           CROPDMG         CROPDMGEXP       
##  Length:902297      Min.   :  0.000   Length:902297     
##  Class :character   1st Qu.:  0.000   Class :character  
##  Mode  :character   Median :  0.000   Mode  :character  
##                     Mean   :  1.527                     
##                     3rd Qu.:  0.000                     
##                     Max.   :990.000                     
##                                                         
##      WFO             STATEOFFIC         ZONENAMES            LATITUDE   
##  Length:902297      Length:902297      Length:902297      Min.   :   0  
##  Class :character   Class :character   Class :character   1st Qu.:2802  
##  Mode  :character   Mode  :character   Mode  :character   Median :3540  
##                                                           Mean   :2875  
##                                                           3rd Qu.:4019  
##                                                           Max.   :9706  
##                                                           NA's   :47    
##    LONGITUDE        LATITUDE_E     LONGITUDE_       REMARKS         
##  Min.   :-14451   Min.   :   0   Min.   :-14455   Length:902297     
##  1st Qu.:  7247   1st Qu.:   0   1st Qu.:     0   Class :character  
##  Median :  8707   Median :   0   Median :     0   Mode  :character  
##  Mean   :  6940   Mean   :1452   Mean   :  3509                     
##  3rd Qu.:  9605   3rd Qu.:3549   3rd Qu.:  8735                     
##  Max.   : 17124   Max.   :9706   Max.   :106220                     
##                   NA's   :40                                        
##      REFNUM      
##  Min.   :     1  
##  1st Qu.:225575  
##  Median :451149  
##  Mean   :451149  
##  3rd Qu.:676723  
##  Max.   :902297  
## 
```

After seeing the structure of the data,  I decided to change the type of `BNG_DATE` and `BNG_TYPE` as both of them were imported as character vectors and have them as `date` and `time` (plus `date_time` as their combination). Also, I already noticed that the columns for population health (`FATALITIES` and `INJURIES`) are full so no need to worry about empty values. The last thing I did was converted column names with *tolower()* and removed all hyphens.


```r
proc_data <- raw_data %>%
    mutate(date = date(mdy_hms(BGN_DATE)),
           time = hms::as.hms(paste(substr(BGN_TIME, 1, 2), substr(BGN_TIME, 3, 2), '00', sep = ":")),
           date_time = date + time) %>%
    select(-BGN_TIME, -BGN_DATE)

names(proc_data) <- tolower(gsub("_", "", names(proc_data)))

head(proc_data)
```

```
## # A tibble: 6 x 38
##   state timezone county countyname state evtype bgnrange bgnazi bgnlocati
##   <dbl> <chr>     <dbl> <chr>      <chr> <chr>     <dbl> <chr>  <chr>    
## 1     1 CST          97 MOBILE     AL    TORNA~        0 <NA>   <NA>     
## 2     1 CST           3 BALDWIN    AL    TORNA~        0 <NA>   <NA>     
## 3     1 CST          57 FAYETTE    AL    TORNA~        0 <NA>   <NA>     
## 4     1 CST          89 MADISON    AL    TORNA~        0 <NA>   <NA>     
## 5     1 CST          43 CULLMAN    AL    TORNA~        0 <NA>   <NA>     
## 6     1 CST          77 LAUDERDALE AL    TORNA~        0 <NA>   <NA>     
## # ... with 29 more variables: enddate <chr>, endtime <chr>,
## #   countyend <dbl>, countyendn <chr>, endrange <dbl>, endazi <chr>,
## #   endlocati <chr>, length <dbl>, width <dbl>, f <int>, mag <dbl>,
## #   fatalities <dbl>, injuries <dbl>, propdmg <dbl>, propdmgexp <chr>,
## #   cropdmg <dbl>, cropdmgexp <chr>, wfo <chr>, stateoffic <chr>,
## #   zonenames <chr>, latitude <dbl>, longitude <dbl>, latitudee <dbl>,
## #   longitude <dbl>, remarks <chr>, refnum <dbl>, date <date>,
## #   time <time>, datetime <date>
```

# Results

## Results 2

Some text


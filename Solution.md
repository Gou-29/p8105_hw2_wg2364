Homework 2
================
Wenhao Gou
2020.9.24

## 0\. Overview and preparation

This is the solution of homework 2 for course P8105. To setup, we need
to import package `tidyvers` and `readxl`

``` r
#Include packages
library(tidyverse)
library(readxl)
```

## 1.Solution of Question 1

### 1.1 Read and clean the Mr. Trash Wheel sheet

  - Specify the sheet in the Excel file and to omit non-data entries
    (rows with notes / figures; columns containing notes) using
    arguments in read\_excel
  - Use reasonable variable names
  - Omit rows that do not include dumpster-specific data
  - Round the number of sports balls to the nearest integer and converts
    the result to an integer variable (using `as.integer`)

<!-- end list -->

``` r
#Read Mr. Trash Wheel Sheet
trash_wheel_df <- read_xlsx("./Datasets/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "Mr. Trash Wheel" , range = cell_cols("A:N")) %>%
  janitor::clean_names() %>% 
  drop_na(dumpster) %>%
  mutate(sports_balls = round(sports_balls, digits = 0)) %>%
  mutate(sports_balls = as.integer(sports_balls)) %>%
  relocate(dumpster,month,year,date,sports_balls)
trash_wheel_df
## # A tibble: 344 x 14
##    dumpster month  year date                sports_balls weight_tons
##       <dbl> <chr> <dbl> <dttm>                     <int>       <dbl>
##  1        1 May    2014 2014-05-16 00:00:00            7        4.31
##  2        2 May    2014 2014-05-16 00:00:00            5        2.74
##  3        3 May    2014 2014-05-16 00:00:00            6        3.45
##  4        4 May    2014 2014-05-17 00:00:00            6        3.1 
##  5        5 May    2014 2014-05-17 00:00:00            7        4.06
##  6        6 May    2014 2014-05-20 00:00:00            5        2.71
##  7        7 May    2014 2014-05-21 00:00:00            3        1.91
##  8        8 May    2014 2014-05-28 00:00:00            6        3.7 
##  9        9 June   2014 2014-06-05 00:00:00            6        2.52
## 10       10 June   2014 2014-06-11 00:00:00            7        3.76
## # ... with 334 more rows, and 8 more variables: volume_cubic_yards <dbl>,
## #   plastic_bottles <dbl>, polystyrene <dbl>, cigarette_butts <dbl>,
## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
## #   homes_powered <dbl>
```

### 1.2 Read and clean precipitation data for 2017 and 2018.

In this problem, we need to read and clean precipitation data for 2017
and 2018. For each, omit rows without precipitation data and add a
variable year. Next, combine precipitation datasets and convert month to
a character variable

``` r
#Read the year 2018 precipitation data
precipitation_2018 <- read_xlsx("./Datasets/Trash-Wheel-Collection-Totals-8-6-19.xlsx", 
                                sheet = "2018 Precipitation" , range = "A2:B14") %>% 
  janitor::clean_names() %>%
  drop_na(total) %>%
  mutate(year = 2018) 
#Read the year 2017 precipitation data
precipitation_2017 <- read_xlsx("./Datasets/Trash-Wheel-Collection-Totals-8-6-19.xlsx", 
                                sheet = "2017 Precipitation" , range = "A2:B14") %>% 
  janitor::clean_names() %>%
  drop_na(total) %>%
  mutate(year = 2017)
#Combine these two tibble and add month name
month_df <- tibble(
  month = as.double(c(1:12)),
  month_name = month.name
)
precipitation_all <- bind_rows(precipitation_2017, precipitation_2018) %>%
  left_join(., month_df, by = "month") %>%
  select(-month) %>%
  rename(month = month_name) %>%
  relocate(year, month)
precipitation_all
## # A tibble: 24 x 3
##     year month     total
##    <dbl> <chr>     <dbl>
##  1  2017 January    2.34
##  2  2017 February   1.46
##  3  2017 March      3.57
##  4  2017 April      3.99
##  5  2017 May        5.64
##  6  2017 June       1.4 
##  7  2017 July       7.09
##  8  2017 August     4.44
##  9  2017 September  1.95
## 10  2017 October    0   
## # ... with 14 more rows
```

### 1.3 Summary of the data

The first dataset is the data of Mr. Trash Wheel from May.2014 to
Jun.2019. In the filtered dataset, we have 344 observations with 14
variables, names: `dumpster, month, year, date, sports_balls,
weight_tons, volume_cubic_yards, plastic_bottles, polystyrene,
cigarette_butts, glass_bottles, grocery_bags, chip_bags, homes_powered`.
The variable `dumpster` is the column number, and the other variables
are the statistics about different type of trash collected on that day.
The variable `homes_powered` indicate that how this wheel have
contributed to the community by providing electric. On average, the
Wheel can capture , `3.262936` tons, `15.5436047`cubic yards trash per
day. For different type of trash, on average, the Wheel can capture
`11.8110465` sports balls, `1873.1540698` plastic bottles,
`2138.6831395` polystyrene, `3.0754128\times 10^{4}` cigarette butts,
`25.3604651` glass bottles, `1311.2267442` grocery bags and
`1780.2732558` chip bags. Specifically, the median number of sports
balls in a dumpster in 2017 is `8`

The second dataset is the precipitation level of the location of the
Wheel in year 2017 and 2018. The total precipitation in 2017 is `32.93`,
and `70.33` in 2018

## 2\. Solution of Quesiton 2

### 2.1 Import and clean the NYC subway data

Read and clean the data; retain line, station, name, station latitude /
longitude, routes served, entry, vending, entrance type, and ADA
compliance. Convert the entry variable from character (YES vs NO) to a
logical variable.

``` r
#Read and clean NYC subway data
NYC_subway <- read_csv("./Datasets/NYC_Transit_Subway_Entrance_And_Exit_Data.csv") %>%
  janitor::clean_names() %>% 
  select(line, station_name, station_latitude, station_longitude, route1:route11, entry, vending, entrance_type, ada) %>%
  mutate(entry = recode(entry, "YES" = T, "NO" = F))
NYC_subway
## # A tibble: 1,868 x 19
##    line  station_name station_latitude station_longitu~ route1 route2 route3
##    <chr> <chr>                   <dbl>            <dbl> <chr>  <chr>  <chr> 
##  1 4 Av~ 25th St                  40.7            -74.0 R      <NA>   <NA>  
##  2 4 Av~ 25th St                  40.7            -74.0 R      <NA>   <NA>  
##  3 4 Av~ 36th St                  40.7            -74.0 N      R      <NA>  
##  4 4 Av~ 36th St                  40.7            -74.0 N      R      <NA>  
##  5 4 Av~ 36th St                  40.7            -74.0 N      R      <NA>  
##  6 4 Av~ 45th St                  40.6            -74.0 R      <NA>   <NA>  
##  7 4 Av~ 45th St                  40.6            -74.0 R      <NA>   <NA>  
##  8 4 Av~ 45th St                  40.6            -74.0 R      <NA>   <NA>  
##  9 4 Av~ 45th St                  40.6            -74.0 R      <NA>   <NA>  
## 10 4 Av~ 53rd St                  40.6            -74.0 R      <NA>   <NA>  
## # ... with 1,858 more rows, and 12 more variables: route4 <chr>, route5 <chr>,
## #   route6 <chr>, route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>,
## #   route11 <dbl>, entry <lgl>, vending <chr>, entrance_type <chr>, ada <lgl>
```

This data contains the geographic information, transfer information and
characteristics of the station. The data cleaning process are as follow:
firstly, use `read_csv` function to read the csv file. Then, use
`clean-names` function to clean the variable and content names. Next,
use `select` function to pull the variable we are interested in, and
finally, use `mutate` and `recode` function to transfer characters into
logic variables. In the result dataset, the size of the dataset is 1868
\* 19. The variable names are: `line, station_name, station_latitude,
station_longitude, route 1 - 11, entry, vending, entrance_type, ada`.
The first two variable is the line place and name of the station, the
third and fourth variable are location of the station. Variables
`route 1 - 11` are 11 variables indicate the transfer information of
this station (e.g, if this station can transfer with 4 other routes, the
variables `route 1- 4` will be filled with transfer information, and the
remaining will be filled with `NA`. the logic variable `entry` indicate
whether this station have entry (`TRUE` or `FALSE`), the `vending`
variable indicate whether the station have vending machine(`YES` or
`NO`), the `entrance_type` variable specified the entry type (with
options: `Door, Easement, Elevator, Escalator, Ramp, Stair, Walkway`),
the `ada` variable is a logic variable describe whether this station is
ADA compliant (`TRUE` or `FALSE`). This data is not tidy enough as the
data type among variables `route 1 - 11` are not the same (mix of `chr`
and `dbl`)

### 2.2 Answers to the following questions:

  - How many distinct stations are there? Note that stations are
    identified both by name and by line (e.g. 125th St A/B/C/D; 125st 1;
    125st 4/5); the distinct function may be useful here.
  - How many stations are ADA compliant?
  - What proportion of station entrances / exits without vending allow
    entrance?
  - How many distinct stations serve the A train?
  - Of the stations that serve the A train, how many are ADA compliant?
    To answer these questions, we need to modify the dataset according
    to the number of station:

<!-- end list -->

``` r
#How many stations?
NYC_stations <- NYC_subway %>% 
  distinct(.,station_name, route1, route2, route3, route4, route5, 
           route6, route7, route8, route9, route10, route11, .keep_all = T)
NYC_stations
## # A tibble: 456 x 19
##    line  station_name station_latitude station_longitu~ route1 route2 route3
##    <chr> <chr>                   <dbl>            <dbl> <chr>  <chr>  <chr> 
##  1 4 Av~ 25th St                  40.7            -74.0 R      <NA>   <NA>  
##  2 4 Av~ 36th St                  40.7            -74.0 N      R      <NA>  
##  3 4 Av~ 45th St                  40.6            -74.0 R      <NA>   <NA>  
##  4 4 Av~ 53rd St                  40.6            -74.0 R      <NA>   <NA>  
##  5 4 Av~ 59th St                  40.6            -74.0 N      R      <NA>  
##  6 4 Av~ 77th St                  40.6            -74.0 R      <NA>   <NA>  
##  7 4 Av~ 86th St                  40.6            -74.0 R      <NA>   <NA>  
##  8 4 Av~ 95th St                  40.6            -74.0 R      <NA>   <NA>  
##  9 4 Av~ 9th St                   40.7            -74.0 F      G      R     
## 10 4 Av~ Atlantic Av~             40.7            -74.0 B      Q      D     
## # ... with 446 more rows, and 12 more variables: route4 <chr>, route5 <chr>,
## #   route6 <chr>, route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>,
## #   route11 <dbl>, entry <lgl>, vending <chr>, entrance_type <chr>, ada <lgl>
#Stations with ADA compliant
NYC_stations %>% filter(ada == TRUE) %>% dim()
## [1] 79 19
#Proportion of station entrances / exits without vending allow entrance
NYC_stations %>% filter(vending == "NO") %>% dim()
## [1]  9 19
#How many distinct stations serve the A train?
NYC_stationsA <- NYC_stations %>% 
  filter(route1 == "A"|route2 =="A"|route3 == "A"|route4 =="A"|
           route5 == "A"|route6 =="A"|route7 == "A"|route8 =="A"|
           route9 == "A"|route10 =="A"|route11 =="A")
dim(NYC_stationsA)                          
## [1] 58 19
#Of the stations that serve the A train, how many are ADA compliant?
NYC_stationsA %>% filter(ada == TRUE) %>% dim()
## [1] 17 19
```

From the result, we can see that:

  - The size of the `NYC_stations` is 456 \* 19, so there are 456
    distinct stations in this dataset.
  - 79 stations are ADA compliant
  - 9 stations have no vending allow entrance
  - 58 stations serve the A train, and among these stations, 17 stations
    are ADA compliant

## 3\. Solution of Quesiton 3

### 3.1 Clean `pols-month.csv`

Requirement: Use `separate()` to break up the variable `mon` into
integer variables `year`, `month`, and `day`; replace month number with
month name; create a `president` variable taking values `gop` and `dem`,
and remove `prez_dem` and `prez_gop`; and remove the day variable

``` r
#Read and clean pols_month.csv
pol <- read_csv("./Datasets/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon,c("year","month","day")) %>%
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>%
  mutate(month = recode(month, `1` = "Jan", `2` = "Feb", `3` = "Mar", `4` = "Apr",    #As my operation system is in
                        `5` = "May" , `6`= "Jun", `7` = "Jul", `8` = "Aug",           #Chinese, if I use function 
                        `9` = "Sep", `10` = "Oct", `11` = "Nov", `12` = "Dec")) %>%   #months, it will appear in Chinese. 
  mutate(president = ifelse(prez_dem == 1, "dem", "gop")) %>%
  select(-prez_dem,-prez_gop,-day) %>%
  arrange(desc(row_number()))
pol
## # A tibble: 822 x 9
##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
##    <int> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
##  1  2015 Jun        31      54     246      18      44     188 dem      
##  2  2015 May        31      54     245      18      44     188 dem      
##  3  2015 Apr        31      54     244      18      44     188 dem      
##  4  2015 Mar        31      54     245      18      44     188 dem      
##  5  2015 Feb        31      54     245      18      44     188 dem      
##  6  2015 Jan        31      54     245      18      44     188 dem      
##  7  2014 Dec        29      45     235      21      53     201 dem      
##  8  2014 Nov        29      45     235      21      53     201 dem      
##  9  2014 Oct        29      45     234      21      53     199 dem      
## 10  2014 Sep        29      45     234      21      53     199 dem      
## # ... with 812 more rows
```

This data contains 822 observations with 9 variable: `year, month,
gov_gop, sen_gop, rep_gop, gov_dem, sen_dem, rep_dem, president`. The
meaning and some descriptive statistics of these variables are as
follow: \* `year` and `month`: Indication of year and month. Started
from Jan. 1947 to Jun. 2015 \* `gov_gop`: the number of republican
governors on the associated month \* `sen_gop`: the number of republican
senators on the associated month \* `rep_gop`: the number of republican
representatives on the associated month \* `gov_dem`: the number of
democratic governors on the associated month \* `sen_dem`: the number of
democratic senators on the associated month \* `rep_dem`: the number of
democratic representatives on the associated month \* `president`:
indicator of whether the president was republican or democratic on the
associated month (`dem` = democratic, `gop` = republican)

### 3.2 Clean `snp.csv`

``` r
#Read and clean snp.csv
snp <- read_csv("./Datasets/snp.csv") %>%
  janitor::clean_names() %>%separate(date,c("month","day","year")) %>%
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>%
  mutate(month = recode(month, `1` = "Jan", `2` = "Feb", `3` = "Mar", `4` = "Apr",    
                        `5` = "May" , `6`= "Jun", `7` = "Jul", `8` = "Aug",           
                        `9` = "Sep", `10` = "Oct", `11` = "Nov", `12` = "Dec")) %>%   
  select(year,month,close)
snp
## # A tibble: 787 x 3
##     year month close
##    <int> <chr> <dbl>
##  1  2015 Jul   2080.
##  2  2015 Jun   2063.
##  3  2015 May   2107.
##  4  2015 Apr   2086.
##  5  2015 Mar   2068.
##  6  2015 Feb   2104.
##  7  2015 Jan   1995.
##  8  2014 Dec   2059.
##  9  2014 Nov   2068.
## 10  2014 Oct   2018.
## # ... with 777 more rows
```

This data contains 787 observations with 3 variables. Variables `year`
and `month`are descriptions of year and month, started from Jun.1950 to
Jul.2015. The variable `snp` indicate the Standard & Poor’s stock market
index (S\&P), often used as a representative measure of stock market as
a whole

### 3.3 Clean `unemployment.csv`

``` r
#Read and claen unemployment.csv
unemploy <- read_csv("./Datasets/unemployment.csv") %>%  
  pivot_longer(Jan:Dec,
               names_to = "month", 
               values_to = "unemployment") %>%
  select(year = Year,everything()) %>%
  arrange(desc(row_number()))
unemploy
## # A tibble: 816 x 3
##     year month unemployment
##    <dbl> <chr>        <dbl>
##  1  2015 Dec           NA  
##  2  2015 Nov           NA  
##  3  2015 Oct           NA  
##  4  2015 Sep           NA  
##  5  2015 Aug           NA  
##  6  2015 Jul           NA  
##  7  2015 Jun            5.3
##  8  2015 May            5.5
##  9  2015 Apr            5.4
## 10  2015 Mar            5.5
## # ... with 806 more rows
```

This data contains 816 variable with 3 variables. Variables `year` and
`month`are descriptions of year and month, started from Jun.1948 to
Dec.2015. The variable `unemployment` is the percentage of unemployment
in the relate month. 6 rows in unemployment are missing.

### 3.4 Merge into one dataset

``` r
#Combine these three tibbles
result <- left_join(pol, snp, by = c("year","month")) %>% 
  left_join(., unemploy, by = c("year","month"))
result
## # A tibble: 822 x 11
##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
##  1  2015 Jun        31      54     246      18      44     188 dem       2063.
##  2  2015 May        31      54     245      18      44     188 dem       2107.
##  3  2015 Apr        31      54     244      18      44     188 dem       2086.
##  4  2015 Mar        31      54     245      18      44     188 dem       2068.
##  5  2015 Feb        31      54     245      18      44     188 dem       2104.
##  6  2015 Jan        31      54     245      18      44     188 dem       1995.
##  7  2014 Dec        29      45     235      21      53     201 dem       2059.
##  8  2014 Nov        29      45     235      21      53     201 dem       2068.
##  9  2014 Oct        29      45     234      21      53     199 dem       2018.
## 10  2014 Sep        29      45     234      21      53     199 dem       1972.
## # ... with 812 more rows, and 1 more variable: unemployment <dbl>
```

As we set `pol` as the main dataset and all other datasets are merged
into it, the final data set have 822 observations, which is same as
`pol`.

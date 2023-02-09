Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
met <- data.table::fread("met_all.gz")
```

``` r
# lazy load, faster
met_lz <- lazy_dt(met, immutable = FALSE)
```

``` r
met <- merge(
  x = met,
  y = stations,
  by.x = "USAFID",
  by.y = "USAF",
  all.x = TRUE,
  all.y = FALSE)
# left_join(met, stations, by=c("USAFID", "USAF"))
```

``` r
str(met)
```

    ## Classes 'data.table' and 'data.frame':   2377343 obs. of  32 variables:
    ##  $ USAFID           : int  690150 690150 690150 690150 690150 690150 690150 690150 690150 690150 ...
    ##  $ WBAN             : int  93121 93121 93121 93121 93121 93121 93121 93121 93121 93121 ...
    ##  $ year             : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
    ##  $ month            : int  8 8 8 8 8 8 8 8 8 8 ...
    ##  $ day              : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ hour             : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ min              : int  56 56 56 56 56 56 56 56 56 56 ...
    ##  $ lat              : num  34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 ...
    ##  $ lon              : num  -116 -116 -116 -116 -116 ...
    ##  $ elev             : int  696 696 696 696 696 696 696 696 696 696 ...
    ##  $ wind.dir         : int  220 230 230 210 120 NA 320 10 320 350 ...
    ##  $ wind.dir.qc      : chr  "5" "5" "5" "5" ...
    ##  $ wind.type.code   : chr  "N" "N" "N" "N" ...
    ##  $ wind.sp          : num  5.7 8.2 6.7 5.1 2.1 0 1.5 2.1 2.6 1.5 ...
    ##  $ wind.sp.qc       : chr  "5" "5" "5" "5" ...
    ##  $ ceiling.ht       : int  22000 22000 22000 22000 22000 22000 22000 22000 22000 22000 ...
    ##  $ ceiling.ht.qc    : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ ceiling.ht.method: chr  "9" "9" "9" "9" ...
    ##  $ sky.cond         : chr  "N" "N" "N" "N" ...
    ##  $ vis.dist         : int  16093 16093 16093 16093 16093 16093 16093 16093 16093 16093 ...
    ##  $ vis.dist.qc      : chr  "5" "5" "5" "5" ...
    ##  $ vis.var          : chr  "N" "N" "N" "N" ...
    ##  $ vis.var.qc       : chr  "5" "5" "5" "5" ...
    ##  $ temp             : num  37.2 35.6 34.4 33.3 32.8 31.1 29.4 28.9 27.2 26.7 ...
    ##  $ temp.qc          : chr  "5" "5" "5" "5" ...
    ##  $ dew.point        : num  10.6 10.6 7.2 5 5 5.6 6.1 6.7 7.8 7.8 ...
    ##  $ dew.point.qc     : chr  "5" "5" "5" "5" ...
    ##  $ atm.press        : num  1010 1010 1011 1012 1013 ...
    ##  $ atm.press.qc     : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ rh               : num  19.9 21.8 18.5 16.9 17.4 ...
    ##  $ CTRY             : chr  "US" "US" "US" "US" ...
    ##  $ STATE            : chr  "CA" "CA" "CA" "CA" ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "sorted")= chr "USAFID"

``` r
# only works when not using lazy load
# met_avg <- met[ , .(
#   temp = mean(temp, na.rm = TRUE),
#   wind.sp = mean(wind.sp, na.rm = TRUE),
#   atm.press = mean(atm.press, na.rm = TRUE)
# ), by = "USAFID"]

met_avg_lz <- met %>% 
  group_by(USAFID) %>%
  summarise(STATE, lat, lon, across(
    c(temp, wind.sp, atm.press),
    function(x) mean(x, na.rm = TRUE)
  )) %>%
  distinct(USAFID, .keep_all = TRUE)
#group_by summarize across(c(), function(X) mean(X, na.rm =))
```

``` r
met_med_lz <- met_avg_lz %>%
  summarise(across(
    c(temp, wind.sp, atm.press),
    function(x) quantile(x, probs =0.5, na.rm =TRUE)
  ))
```

``` r
met_avg_lz %>%
  filter(
    temp == met_med_lz %>% pull(temp) |
    wind.sp == met_med_lz %>% pull(wind.sp) |
    atm.press == met_med_lz %>% pull(atm.press)
  )
```

    ## Source: local data table [1 x 7]
    ## Call:   unique(`_DT2`[, .(STATE = STATE, lat = lat, lon = lon, temp = (function (x) 
    ## mean(x, na.rm = TRUE))(temp), wind.sp = (function (x) 
    ## mean(x, na.rm = TRUE))(wind.sp), atm.press = (function (x) 
    ## mean(x, na.rm = TRUE))(atm.press)), keyby = .(USAFID)], by = "USAFID")[temp == 
    ##     met_med_lz %>% pull(temp) | wind.sp == met_med_lz %>% pull(wind.sp) | 
    ##     atm.press == met_med_lz %>% pull(atm.press)]
    ## 
    ##   USAFID STATE   lat   lon  temp wind.sp atm.press
    ##    <int> <chr> <dbl> <dbl> <dbl>   <dbl>     <dbl>
    ## 1 720929 WI     45.5 -92.0  17.4    2.46       NaN
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

``` r
temp_us_id <- met_avg_lz %>%
  mutate(d = abs(temp - met_med_lz %>% pull(temp))) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)

wind_us_id <- met_avg_lz %>%
  mutate(d = abs(wind.sp - met_med_lz %>% pull(wind.sp))) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)

atm_us_id <- met_avg_lz %>%
  mutate(d = abs(atm.press - met_med_lz %>% pull(atm.press))) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)
```

``` r
met_lz %>%
  select(USAFID, lon, lat) %>%
  distinct() %>%
  filter(USAFID %in% c(temp_us_id, wind_us_id, atm_us_id)) 
```

    ## Source: local data table [4 x 3]
    ## Call:   unique(`_DT1`[, .(USAFID, lon, lat)])[USAFID %in% c(temp_us_id, 
    ##     wind_us_id, atm_us_id)]
    ## 
    ##   USAFID   lon   lat
    ##    <int> <dbl> <dbl>
    ## 1 720458 -82.6  37.8
    ## 2 720929 -92.0  45.5
    ## 3 722238 -85.7  31.4
    ## 4 722238 -85.7  31.3
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
met_med_state <- met_avg_lz %>%
  group_by(STATE) %>%
  summarise(temp = median(temp, na.rm = TRUE), 
            wind.sp = median(wind.sp, na.rm = TRUE),
            atm.press = median(atm.press, na.rm = TRUE))
```

``` r
met_avg_med <- merge(
  x = met_avg_lz,
  y = met_med_state,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE)
```

``` r
met_d <- met_avg_med %>%
  mutate(d = sqrt((temp.x - temp.y)^2 + (wind.sp.x - wind.sp.y)^2 + (atm.press.x - atm.press.y)^2))
```

``` r
repre_state <- met_d %>%
  group_by(STATE) %>%
  slice_min(order_by = d) %>%
  summarise(STATE, USAFID, lat, lon, d)
repre_state
```

    ## # A tibble: 46 × 5
    ##    STATE USAFID   lat    lon      d
    ##    <chr>  <int> <dbl>  <dbl>  <dbl>
    ##  1 AL    722286  33.2  -87.6 0.0561
    ##  2 AR    723407  35.8  -90.6 0.461 
    ##  3 AZ    722745  32.2 -111.  0.233 
    ##  4 CA    722970  33.8 -118.  0.300 
    ##  5 CO    724767  37.3 -109.  0.944 
    ##  6 CT    725087  41.7  -72.7 0.346 
    ##  7 DE    724180  39.7  -75.6 0     
    ##  8 FL    722106  26.6  -81.9 0.0477
    ##  9 GA    723160  31.5  -82.5 0.312 
    ## 10 IA    725480  42.6  -92.4 0.199 
    ## # … with 36 more rows

``` r
as.data.table(met_avg_lz %>% filter(STATE == "WA" | STATE == "ND"))
```

    ##     USAFID STATE    lat      lon     temp  wind.sp atm.press
    ##  1: 720254    WA 46.683 -122.983 19.24684 1.268571       NaN
    ##  2: 720272    WA 48.467 -122.416 18.73372 1.881978       NaN
    ##  3: 720388    WA 47.104 -122.287 19.35326 0.558382       NaN
    ##  4: 720491    ND 46.217  -97.633 18.85637 3.872863       NaN
    ##  5: 720737    ND 47.451  -99.151 18.23984 4.246561       NaN
    ##  6: 720853    ND 48.884  -99.621 16.88894 4.107325       NaN
    ##  7: 720854    ND 46.925 -103.982 18.65313 4.236655       NaN
    ##  8: 720855    ND 48.784  -97.632 18.45570 3.307916       NaN
    ##  9: 720858    ND 48.480  -99.236 17.93556 3.972789       NaN
    ## 10: 720861    ND 48.929 -103.297 18.02843 4.136298       NaN
    ## 11: 720863    ND 48.381 -102.898 17.64007 4.329142       NaN
    ## 12: 720865    ND 48.405  -97.371 18.88400 4.113039       NaN
    ## 13: 720866    ND 47.290 -101.581 18.66758 3.467201       NaN
    ## 14: 720867    ND 48.390 -100.024 18.20367 4.203768       NaN
    ## 15: 720868    ND 47.796 -103.254 18.96057 3.594991       NaN
    ## 16: 720871    ND 46.768 -100.894 18.69658 4.525923       NaN
    ## 17: 720909    ND 48.301 -102.406 17.69519 3.682776       NaN
    ## 18: 720911    ND 46.942  -98.018 18.34248 3.940128       NaN
    ## 19: 720933    ND 46.218 -100.245 19.10517 3.545000       NaN
    ## 20: 722004    ND 46.244  -96.607 18.60128 3.431250       NaN
    ## 21: 722857    ND 48.941  -97.903 18.94941 3.178723       NaN
    ##     USAFID STATE    lat      lon     temp  wind.sp atm.press

All stations in WA and ND doesn’t have atm.press, so I cannot figure out
the representative station for these two state.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
met_avg_state <- met_avg_lz %>%
  group_by(STATE) %>%
  summarise(lat = mean(lat, na.rm = TRUE), 
            lon = mean(lon, na.rm = TRUE))
```

``` r
met_avg_avg <- merge(
  x = met_avg_lz,
  y = met_avg_state,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE)
```

``` r
met_d_avg <- met_avg_avg %>%
  mutate(d = sqrt((lat.x - lat.y)^2 + (lon.x - lon.y)^2))
```

``` r
mid_state <- met_d_avg %>%
  group_by(STATE) %>%
  slice_min(order_by = d) %>%
  summarise(STATE, USAFID, lat = lat.x, lon = lon.x, d)
mid_state
```

    ## # A tibble: 48 × 5
    ##    STATE USAFID   lat    lon      d
    ##    <chr>  <int> <dbl>  <dbl>  <dbl>
    ##  1 AL    722265  32.4  -86.4 0.459 
    ##  2 AR    720401  35.6  -92.4 0.388 
    ##  3 AZ    722783  33.5 -112.  0.383 
    ##  4 CA    723898  36.3 -120.  0.154 
    ##  5 CO    726396  39.0 -106.  0.138 
    ##  6 CT    725027  41.5  -72.8 0.0764
    ##  7 DE    724088  39.1  -75.5 0.0347
    ##  8 FL    722014  28.5  -82.5 0.197 
    ##  9 GA    722175  32.6  -83.6 0.193 
    ## 10 IA    725472  42.0  -93.6 0.241 
    ## # … with 38 more rows

``` r
library(leaflet)
leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(data = mid_state, lat=~lat, lng=~lon, popup="mid station", opacity=1, fillOpacity=1, radius=400, color="Blue") %>% 
  addCircles(data = repre_state, lat=~lat, lng=~lon, popup="representative station", opacity=1, fillOpacity=1, radius=400, color="Red")
```

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

``` r
met[, state_temperature := mean(temp, na.rm = TRUE), by=STATE]
met[, temp_level:= 
      fifelse(state_temperature <20, "low", 
              fifelse(state_temperature <25, "mid", "high"))]
```

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
table(met$temp_level, useNA="always")
```

    ## 
    ##    high     low     mid    <NA> 
    ##  811126  430794 1135423       0

``` r
tab <- met[, .(N_entries = .N,
               N_na = sum(is.na(temp_level)),
               N_stations = length(unique(USAFID)),
               N_states = length(unique(STATE)),
               mean_temp = mean(temp, na.rm = TRUE),
               mean_wind.sp = mean(wind.sp, na.rm = TRUE),
               mean_atm.press = mean(atm.press, na.rm = TRUE)),
           by=temp_level]
tab
```

    ##    temp_level N_entries N_na N_stations N_states mean_temp mean_wind.sp
    ## 1:        mid   1135423    0        781       25  22.39909     2.352712
    ## 2:       high    811126    0        555       12  27.75066     2.514644
    ## 3:        low    430794    0        259       11  18.96446     2.637410
    ##    mean_atm.press
    ## 1:       1014.383
    ## 2:       1013.738
    ## 3:       1014.366

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
library(ggplot2)
ggplot(as.data.frame(met_med_state), aes(x = wind.sp, y = temp)) +
  geom_point() +
  geom_smooth(color="red", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
gam_mod <- gam(temp ~ s(wind.sp, bs="cr", k=20), data = as.data.frame(met_med_state))
summary(gam_mod)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(wind.sp, bs = "cr", k = 20)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  22.9836     0.3986   57.66   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##             edf Ref.df     F p-value  
    ## s(wind.sp) 5.18  6.433 2.006  0.0767 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.196   Deviance explained = 28.5%
    ## GCV = 8.7549  Scale est. = 7.6277    n = 48

``` r
plot(gam_mod)
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
lm_mod <- lm(temp ~ wind.sp, data = met_med_state)
summary(lm_mod)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ wind.sp, data = met_med_state)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -5.057 -2.483 -0.506  1.808  7.414 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  23.2806     1.4929  15.594   <2e-16 ***
    ## wind.sp      -0.1207     0.5785  -0.209    0.836    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.112 on 46 degrees of freedom
    ## Multiple R-squared:  0.0009453,  Adjusted R-squared:  -0.02077 
    ## F-statistic: 0.04352 on 1 and 46 DF,  p-value: 0.8357

``` r
plot(lm_mod)
```

![](README_files/figure-gfm/last%20chunk-1.png)<!-- -->![](README_files/figure-gfm/last%20chunk-2.png)<!-- -->![](README_files/figure-gfm/last%20chunk-3.png)<!-- -->![](README_files/figure-gfm/last%20chunk-4.png)<!-- -->
From two summary table, we can see that gam model is better than linear
model. Because the p value of t test for linear model is 0.836, which is
too big, and from residual plot, we can easily find that the residuals
are not i.i.d from normal distribution. So linear model is not good
enough in this case.

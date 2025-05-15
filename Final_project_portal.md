Final Project
================
Claire Pickett
2025-05-15

## Installing Portal Data

``` r
library(portalr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.4     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.4     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
```

``` r
data_tables <- load_rodent_data("repo")
```

    ## Loading in data version 5.146.0

``` r
names(data_tables)
```

    ## [1] "rodent_data"    "species_table"  "trapping_table" "newmoons_table"
    ## [5] "plots_table"

``` r
#trying to figure out which days rodents were trapped and where so I can start analyzing images

trapping_schedule <- data_tables$trapping_table
head(trapping_schedule)
```

    ##   day month year period plot sampled effort qcflag
    ## 1  16     7 1977      1    1       1     49      1
    ## 2  16     7 1977      1    2       1     49      1
    ## 3  16     7 1977      1    3       1     49      1
    ## 4  16     7 1977      1    4       1     49      1
    ## 5  16     7 1977      1    5       1     49      1
    ## 6  16     7 1977      1    6       1     49      1

``` r
jan2024_trap <- trapping_schedule %>%
  filter(year == 2024, month == 1, sampled == TRUE)

rodents <- data_tables$rodent_data
rodents_jan_11_west<- rodents %>%
  filter(year == 2024, month == 1, plot == 11)

rodents_jan_11_west
```

    ##   recordID month day year period plot note1 stake species sex reprod  age
    ## 1    77734     1   6 2024    525   11    NA    24      DM   M      Z <NA>
    ## 2    77735     1   6 2024    525   11    NA    11      DO   M      Z <NA>
    ## 3    77736     1   6 2024    525   11    NA    72      DM   F   <NA> <NA>
    ## 4    77737     1   6 2024    525   11    NA    35      DM   M      Z <NA>
    ## 5    77738     1   6 2024    525   11    NA    33      DM   F      Z <NA>
    ## 6    77739     1   6 2024    525   11    NA    53      DM   M      Z <NA>
    ## 7    77740     1   6 2024    525   11    NA    22      DM   M      Z <NA>
    ## 8    77741     1   6 2024    525   11    NA    73      DM   M      Z <NA>
    ## 9    77742     1   6 2024    525   11    NA    74      OT   F      Z <NA>
    ##   testes vagina pregnant nipples lactation hfl wgt    tag note2 ltag note3
    ## 1   <NA>   <NA>     <NA>    <NA>      <NA>  36  44 C0F020  <NA> <NA>  <NA>
    ## 2   <NA>   <NA>     <NA>    <NA>      <NA>  34  43 C2DF9C  <NA> <NA>  <NA>
    ## 3   <NA>   <NA>        P    <NA>      <NA>  35  44 795D92  <NA> <NA>  <NA>
    ## 4   <NA>   <NA>     <NA>    <NA>      <NA>  35  42 7972C2  <NA> <NA>  <NA>
    ## 5   <NA>   <NA>     <NA>    <NA>      <NA>  35  41 B26E2A  <NA> <NA>  <NA>
    ## 6   <NA>   <NA>     <NA>    <NA>      <NA>  35  41 C2BB20  <NA> <NA>  <NA>
    ## 7   <NA>   <NA>     <NA>    <NA>      <NA>  35  40 797CBA     * <NA>  <NA>
    ## 8   <NA>   <NA>     <NA>    <NA>      <NA>  35  42 79523F  <NA> <NA>  <NA>
    ## 9   <NA>   <NA>     <NA>    <NA>      <NA>  17  19 79FBCB  <NA> <NA>  <NA>
    ##   prevrt prevlet nestdir neststk note4 note5 pit_tag   id
    ## 1   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 2   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 3   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 4   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 5   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 6   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 7   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 8   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>
    ## 9   <NA>    <NA>    <NA>      NA  <NA>  <NA>      NA <NA>

# Image analysis

I am taking the Plot 11 West for the month of January and comparing it
to rodents trapped.

``` r
#I am taking the Plot 11 West for the month of January and comparing it to rodents trapped at this time, to see if there is any correlation between which animals are there and abundance of rodents
```

``` r
file.exists("figures/graphs_2024_01_and_02_11west/activity-patterns/hour-of-day/combined.png")
```

    ## [1] TRUE

``` r
file.exists("figures/graphs_2024_01_and_02_11west/bar-charts/grouped-by-day/combined-multi-layer.png")
```

    ## [1] TRUE

![](figures/graphs_2024_01_and_02_11west/activity-patterns/hour-of-day/combined.png)

![](figures/graphs_2024_01_and_02_11west/bar-charts/grouped-by-day/combined-multi-layer.png)
\# Combining data Sets

``` r
addax_raw <- read_csv("addax_analysis/11west_excel/11west_results.csv")
```

    ## Rows: 227 Columns: 42
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): absolute_path, relative_path, data_type, label, Make, Model, Exif...
    ## dbl  (21): confidence, bbox_left, bbox_top, bbox_right, bbox_bottom, file_he...
    ## lgl   (9): human_verified, Latitude, Longitude, GPSLink, Altitude, Software,...
    ## dttm  (3): DateTimeOriginal, DateTime, DateTimeDigitized
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
addax_clean <- addax_raw %>%
  mutate(
    plot = 11,  
    species = tolower(label),  
    date = as.Date(str_extract(relative_path, "^\\d{4}-\\d{2}-\\d{2}")),
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>%
  filter(species != "unidentified animal", species != "rodent", species != "person") 
  

head(addax_clean)
```

    ## # A tibble: 6 × 48
    ##   absolute_path          relative_path data_type label confidence human_verified
    ##   <chr>                  <chr>         <chr>     <chr>      <dbl> <lgl>         
    ## 1 C:/Claire RStudio/Fin… 2023-12-31 2… img       rabb…      1.00  FALSE         
    ## 2 C:/Claire RStudio/Fin… 2024-01-02 0… img       coyo…      0.740 FALSE         
    ## 3 C:/Claire RStudio/Fin… 2024-01-02 1… img       bird       1.00  FALSE         
    ## 4 C:/Claire RStudio/Fin… 2024-01-04 0… img       rabb…      0.999 FALSE         
    ## 5 C:/Claire RStudio/Fin… 2024-01-04 0… img       rabb…      0.999 FALSE         
    ## 6 C:/Claire RStudio/Fin… 2024-01-04 0… img       racc…      1.00  FALSE         
    ## # ℹ 42 more variables: bbox_left <dbl>, bbox_top <dbl>, bbox_right <dbl>,
    ## #   bbox_bottom <dbl>, file_height <dbl>, file_width <dbl>,
    ## #   DateTimeOriginal <dttm>, DateTime <dttm>, DateTimeDigitized <dttm>,
    ## #   Latitude <lgl>, Longitude <lgl>, GPSLink <lgl>, Altitude <lgl>, Make <chr>,
    ## #   Model <chr>, Flash <dbl>, ExifOffset <dbl>, ResolutionUnit <dbl>,
    ## #   YCbCrPositioning <dbl>, XResolution <dbl>, YResolution <dbl>,
    ## #   ExifVersion <chr>, ComponentsConfiguration <chr>, FlashPixVersion <chr>, …

``` r
addax_clean <- addax_clean %>%
  select(plot, year, month, day, species, confidence)
```

``` r
rodents_clean <- rodents %>%
  filter(year == 2024, month==1, plot == 11) %>%
  group_by(plot, year, month, day) %>%
  summarise(rodent_abundance = n())
```

    ## `summarise()` has grouped output by 'plot', 'year', 'month'. You can override
    ## using the `.groups` argument.

``` r
rodents_clean
```

    ## # A tibble: 1 × 5
    ## # Groups:   plot, year, month [1]
    ##    plot  year month   day rodent_abundance
    ##   <int> <int> <int> <int>            <int>
    ## 1    11  2024     1     6                9

``` r
#I used full_join() to combine Addax camera detections with rodent census data by date and plot, to show both results from each
combined_data <- full_join(
  addax_clean,
  rodents_clean,
  by = c("plot", "year", "month", "day")
) %>%
  mutate(date = make_date(year, month, day))
combined_data
```

    ## # A tibble: 69 × 8
    ##     plot  year month   day species confidence rodent_abundance date      
    ##    <dbl> <dbl> <dbl> <int> <chr>        <dbl>            <int> <date>    
    ##  1    11  2023    12    31 rabbit       1.00                NA 2023-12-31
    ##  2    11  2024     1     2 coyote       0.740               NA 2024-01-02
    ##  3    11  2024     1     2 bird         1.00                NA 2024-01-02
    ##  4    11  2024     1     4 rabbit       0.999               NA 2024-01-04
    ##  5    11  2024     1     4 rabbit       0.999               NA 2024-01-04
    ##  6    11  2024     1     4 raccoon      1.00                NA 2024-01-04
    ##  7    11  2024     1     4 raccoon      0.998               NA 2024-01-04
    ##  8    11  2024     1     4 raccoon      1.00                NA 2024-01-04
    ##  9    11  2024     1     4 rabbit       0.545               NA 2024-01-04
    ## 10    11  2024     1     4 raccoon      1.00                NA 2024-01-04
    ## # ℹ 59 more rows

``` r
summary_data <- combined_data %>%
  group_by(species, date) %>%
  summarise(
    detections = n(),
    rodent_abundance = first(rodent_abundance),  
    .groups = "drop")

rodent_overlay <- summary_data %>%
  distinct(species) %>%
  mutate(
    date = make_date(2024, 1, 6),
    rodent_abundance = 9
  )
```

``` r
ggplot(summary_data, aes(x = date, y = detections)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_point(
    data = rodent_overlay,
    aes(x = date, y = rodent_abundance),
    color = "darkred",
    size = 3
  ) +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = "Addax Species Detections with Rodent Census Overlay (Jan 2024, Plot 11)",
    subtitle = "Red dot shows rodent abundance (n = 9) on Jan 6",
    x = "Date",
    y = "Detections"
  ) +
  theme_minimal()
```

![](Final_project_portal_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Writing a function to summarise future detections that come in from
other plots.

``` r
summarize_detections <- function(data, group_col) {
  data %>%
    group_by({{ group_col }}) %>%
    summarise(detections = n(), .groups = "drop")
}
```

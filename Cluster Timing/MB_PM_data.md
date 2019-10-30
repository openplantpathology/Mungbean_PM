Mungbean Powdery Mildew Spray Timing
================
Adam H. Sparks
30/10/2019

# Clean data

The csv file included is already reduced in columns compared with the
original file. This next step converts the date columns to `Date` class,
removes more cols, including the dates for spray applications after two
(there are up to seven in some studies) and creates two columns of time
differences.

The first column of time differences is for the time from first sign to
first spray. The second is for time from first sign to second spray.

Any rows that are missing yield data are removed.

``` r
mb <-
  mb %>%
  mutate(first_sign_disease = as.Date(first_sign_disease, format = "%d/%m/%Y")) %>%
  mutate_at(vars(starts_with("fungicide_application_")), dmy) %>%
  mutate(fungicide_timing_1 = fungicide_application_1 - first_sign_disease) %>%
  mutate(fungicide_timing_2 = fungicide_application_2 - first_sign_disease) %>%
  select(-c(fungicide_application_3:fungicide_application_7)) %>%
  filter(!is.na(grain_yield.t.ha.))
```

# Inspect the new data set

``` r
skim(mb)
```

    ## Skim summary statistics
    ##  n obs: 319 
    ##  n variables: 13 
    ## 
    ## ── Variable type:character ─────────────────────────
    ##       variable missing complete   n min max empty n_unique
    ##   fungicide_ai       0      319 319   7  34     0       15
    ##  host_genotype       0      319 319   4  13     0        5
    ##       location       0      319 319   5  12     0       13
    ##      trial_ref       0      319 319   6  11     0       23
    ## 
    ## ── Variable type:Date ──────────────────────────────
    ##                 variable missing complete   n        min        max
    ##       first_sign_disease       0      319 319 2001-03-16 2019-04-12
    ##  fungicide_application_1      52      267 319 2001-03-16 2019-04-23
    ##  fungicide_application_2     182      137 319 2001-03-26 2019-04-29
    ##      median n_unique
    ##  2016-03-08       22
    ##  2015-03-24       34
    ##  2015-04-07       27
    ## 
    ## ── Variable type:difftime ──────────────────────────
    ##            variable missing complete   n      min     max  median n_unique
    ##  fungicide_timing_1      52      267 319 -28 days 19 days  1 days       17
    ##  fungicide_timing_2     182      137 319 -20 days 34 days 17 days       18
    ## 
    ## ── Variable type:numeric ───────────────────────────
    ##           variable missing complete   n    mean   sd      p0     p25
    ##  grain_yield.t.ha.       0      319 319    1.16 0.51    0.28    0.72
    ##      row_spacing.m      11      308 319    0.6  0.28    0.25    0.25
    ##    total_fungicide       0      319 319    1.38 0.98    0       1   
    ##               year       0      319 319 2014.06 3.82 2001    2012   
    ##      p50     p75    p100     hist
    ##     1.08    1.53    2.64 ▃▇▃▅▃▂▂▁
    ##     0.75    0.75    1    ▇▁▃▁▁▇▁▅
    ##     1       2       7    ▃▇▇▁▁▁▁▁
    ##  2016    2017    2019    ▁▁▁▂▃▂▂▇

# Create Histograms of time differences to spray

## First spray

``` r
ggplot(filter(mb, !is.na(fungicide_timing_1)),
       aes(x = as.numeric(fungicide_timing_1))) +
  geom_histogram(bins = 50, fill = "grey") +
  geom_vline(linetype = 2,
             xintercept = 0,
             colour = "red") +
  xlab("Days from first sign") +
  ggtitle("First Application",
          subtitle = "Line at first sign") +
  theme_bw()
```

![](MB_PM_data_files/figure-gfm/first-spray-hist-1.png)<!-- -->

Suggest that any sprays prior to first sign are a group with another
group from Day 0 to Day 3 and third group being \>= Day 4.

## Second spray

``` r
ggplot(filter(mb, !is.na(fungicide_timing_2)),
       aes(x = as.numeric(fungicide_timing_2))) +
   geom_histogram(bins = 50, fill = "grey") +
   geom_vline(linetype = 2,
              xintercept = 14,
              colour = "red") +
   xlab("Days from first sign") +
   ggtitle("Second Application",
           subtitle = "Line at 14 days after first sign") +
   theme_bw()
```

![](MB_PM_data_files/figure-gfm/second-spray-hist-1.png)<!-- -->

Again, suggest any sprays that are prior to 14 days after first sign be
first group, the second being 14 days to 17 days after first sign and
the third being \>= 18 days after first sign.

These groups would then have preventative (before), at first sign and
then 14 days after and late applications.

``` r
mb <-
  mb %>%
  mutate(
    cluster_1 = case_when(
      fungicide_timing_1 < 0 ~ "Early",
      fungicide_timing_1 >= 0 &
        fungicide_timing_1 < 3 ~ "Recommended",
      TRUE ~ "Late"
    )
  ) %>%
  mutate(
    cluster_2 = case_when(
      fungicide_timing_2 < 13 ~ "Early",
      fungicide_timing_2 >= 13 &
        fungicide_timing_2 < 17 ~ "Recommended",
      TRUE ~ "Late"
    )
  )
```

# Histograms with Clusters

## First spray

``` r
ggplot(filter(mb, !is.na(fungicide_timing_1)),
       aes(x = as.numeric(fungicide_timing_1))) +
  geom_histogram(bins = 50, aes(fill = cluster_1)) +
  scale_fill_viridis_d("Cluster") +
   geom_vline(linetype = 2,
              xintercept = 0,
              colour = "red") +
   xlab("Days from first sign") +
   ggtitle("First Application",
           subtitle = "Line at first sign") +
   theme_bw()
```

![](MB_PM_data_files/figure-gfm/hist-clust1-1.png)<!-- -->

## Second spray

``` r
ggplot(filter(mb, !is.na(fungicide_timing_2)),
       aes(x = as.numeric(fungicide_timing_2))) +
  geom_histogram(bins = 50, aes(fill = cluster_2)) +
  scale_fill_viridis_d("Cluster") +
  geom_vline(linetype = 2,
             xintercept = 14,
             colour = "red") +
  xlab("Days from first sign") +
  ggtitle("Second Application",
          subtitle = "Line at 14 days after first sign") +
  theme_bw()
```

![](MB_PM_data_files/figure-gfm/hist-clust2-1.png)<!-- -->

Note that the cluster for the second spray “reccomended” is from Day 13,
not Day 14.

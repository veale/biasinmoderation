Automated content moderation code
================

-   [Toxicity](#toxicity)
-   [Distribution of average toxicity per worker.](#distribution-of-average-toxicity-per-worker.)
-   [Getting aggressive](#getting-aggressive)
-   [Distribution of average aggression per worker.](#distribution-of-average-aggression-per-worker.)
-   [Making text classifiers](#making-text-classifiers)

Import a load of stuff

``` r
require(irr)
require(boot)
require(tidyverse)
require(pander)
```

Toxicity
--------

``` r
tox <- read_tsv("dat/toxicity_annotated_comments.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   rev_id = col_double(),
    ##   comment = col_character(),
    ##   year = col_integer(),
    ##   logged_in = col_character(),
    ##   ns = col_character(),
    ##   sample = col_character(),
    ##   split = col_character()
    ## )

``` r
tox.dem <- read_tsv("dat/toxicity_worker_demographics.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   worker_id = col_integer(),
    ##   gender = col_character(),
    ##   english_first_language = col_integer(),
    ##   age_group = col_character(),
    ##   education = col_character()
    ## )

``` r
tox.ann <- read_tsv("dat/toxicity_annotations.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   rev_id = col_double(),
    ##   worker_id = col_integer(),
    ##   toxicity = col_integer(),
    ##   toxicity_score = col_double()
    ## )

``` r
agg <- read_tsv("dat/aggression_annotated_comments.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   rev_id = col_integer(),
    ##   comment = col_character(),
    ##   year = col_integer(),
    ##   logged_in = col_character(),
    ##   ns = col_character(),
    ##   sample = col_character(),
    ##   split = col_character()
    ## )

``` r
agg.dem <- read_tsv("dat/aggression_worker_demographics.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   worker_id = col_integer(),
    ##   gender = col_character(),
    ##   english_first_language = col_integer(),
    ##   age_group = col_character(),
    ##   education = col_character()
    ## )

``` r
agg.ann <- read_tsv("dat/aggression_annotations.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   rev_id = col_integer(),
    ##   worker_id = col_integer(),
    ##   aggression = col_double(),
    ##   aggression_score = col_double()
    ## )

``` r
# Join data frames together
tox.ann %>% left_join(tox.dem) -> tox.long
```

    ## Joining, by = "worker_id"

Distribution of average toxicity per worker.
--------------------------------------------

We might want to know what the average worker's toxicity score is.

``` r
tox.long %>% group_by(worker_id, gender, age_group, education, english_first_language) %>% filter(gender %in% c("male", "female", NA)) %>% summarise(mean.toxicityscore = mean(toxicity_score)) -> tox.workermeans


ggplot(tox.workermeans) + stat_density(aes(mean.toxicityscore)) + ggtitle("Distribution of mean toxicity score") + theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(tox.workermeans) + 
  geom_violin(aes(y = mean.toxicityscore, 
                  fill = gender, 
                  x = gender)) + 
  ggtitle("Distribution of mean toxicity score by gender") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(tox.workermeans) + 
  geom_violin(aes(mean.toxicityscore, 
                  fill = age_group, 
                  x = age_group)) + 
  ggtitle("Distribution of mean toxicity score by age") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(tox.workermeans) + 
  geom_violin(aes(mean.toxicityscore, 
                  fill = education, 
                  x = education)) + 
  ggtitle("Distribution of mean toxicity score by education") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
  ggplot(tox.workermeans) + 
  geom_violin(aes(mean.toxicityscore, 
                  fill = factor(english_first_language), 
                  x = factor(english_first_language))) + 
  ggtitle("Distribution of mean toxicity score by first language") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Can we predict who will have a higher mean toxicity score?

Do this over the data with NA's subsetted out.

``` r
lm(formula = mean.toxicityscore ~ gender + age_group + education + english_first_language,
   data = tox.workermeans %>% na.omit) %>% 
  summary %>% 
  pander(add.significance.stars = T)
```

<table>
<colgroup>
<col width="36%" />
<col width="13%" />
<col width="16%" />
<col width="12%" />
<col width="13%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">Estimate</th>
<th align="center">Std. Error</th>
<th align="center">t value</th>
<th align="center">Pr(&gt;|t|)</th>
<th></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>gendermale</strong></td>
<td align="center">0.05509</td>
<td align="center">0.01117</td>
<td align="center">4.932</td>
<td align="center">8.522e-07</td>
<td>* * *</td>
</tr>
<tr class="even">
<td align="center"><strong>age_group30-45</strong></td>
<td align="center">0.05009</td>
<td align="center">0.01156</td>
<td align="center">4.334</td>
<td align="center">1.508e-05</td>
<td>* * *</td>
</tr>
<tr class="odd">
<td align="center"><strong>age_group45-60</strong></td>
<td align="center">0.04463</td>
<td align="center">0.01972</td>
<td align="center">2.263</td>
<td align="center">0.02369</td>
<td>*</td>
</tr>
<tr class="even">
<td align="center"><strong>age_groupOver 60</strong></td>
<td align="center">-0.01468</td>
<td align="center">0.05782</td>
<td align="center">-0.2538</td>
<td align="center">0.7996</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>age_groupUnder 18</strong></td>
<td align="center">0.06791</td>
<td align="center">0.03663</td>
<td align="center">1.854</td>
<td align="center">0.0638</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationdoctorate</strong></td>
<td align="center">0.05884</td>
<td align="center">0.05026</td>
<td align="center">1.171</td>
<td align="center">0.2419</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationhs</strong></td>
<td align="center">0.02103</td>
<td align="center">0.01301</td>
<td align="center">1.616</td>
<td align="center">0.1061</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationmasters</strong></td>
<td align="center">-0.02779</td>
<td align="center">0.01601</td>
<td align="center">-1.736</td>
<td align="center">0.08263</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationnone</strong></td>
<td align="center">0.2864</td>
<td align="center">0.1813</td>
<td align="center">1.579</td>
<td align="center">0.1144</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationprofessional</strong></td>
<td align="center">0.0007496</td>
<td align="center">0.01724</td>
<td align="center">0.04349</td>
<td align="center">0.9653</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationsome</strong></td>
<td align="center">0.01837</td>
<td align="center">0.03405</td>
<td align="center">0.5394</td>
<td align="center">0.5896</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>english_first_language</strong></td>
<td align="center">0.02943</td>
<td align="center">0.01371</td>
<td align="center">2.147</td>
<td align="center">0.03185</td>
<td>*</td>
</tr>
<tr class="odd">
<td align="center"><strong>(Intercept)</strong></td>
<td align="center">0.1453</td>
<td align="center">0.0124</td>
<td align="center">11.72</td>
<td align="center">3.866e-31</td>
<td>* * *</td>
</tr>
</tbody>
</table>

<table style="width:85%;">
<caption>Fitting linear model: mean.toxicityscore ~ gender + age_group + education + english_first_language</caption>
<colgroup>
<col width="20%" />
<col width="30%" />
<col width="11%" />
<col width="22%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Observations</th>
<th align="center">Residual Std. Error</th>
<th align="center"><span class="math inline"><em>R</em><sup>2</sup></span></th>
<th align="center">Adjusted <span class="math inline"><em>R</em><sup>2</sup></span></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">3513</td>
<td align="center">0.3127</td>
<td align="center">0.01726</td>
<td align="center">0.01389</td>
</tr>
</tbody>
</table>

Getting aggressive
------------------

``` r
# Join data frames together
agg.ann %>% left_join(agg.dem) -> agg.long
```

    ## Joining, by = "worker_id"

Distribution of average aggression per worker.
----------------------------------------------

We might want to know what the average worker's aggression score is.

``` r
agg.long %>% 
  group_by(worker_id, gender, age_group, education, english_first_language) %>% 
  filter(gender %in% c("male", "female", NA)) %>% 
  summarise(mean.aggressionscore = mean(aggression_score)) -> agg.workermeans


ggplot(agg.workermeans) + 
  stat_density(aes(mean.aggressionscore)) + 
  ggtitle("Distribution of mean aggression score") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
ggplot(agg.workermeans) + 
  geom_violin(aes(y = mean.aggressionscore, 
                  fill = gender, 
                  x = gender)) + 
  ggtitle("Distribution of mean aggression score by gender") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
ggplot(agg.workermeans) + 
  geom_violin(aes(mean.aggressionscore, 
                  fill = age_group, 
                  x = age_group)) + 
  ggtitle("Distribution of mean aggression score by age") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
ggplot(agg.workermeans) +
  geom_violin(aes(mean.aggressionscore, 
                  fill = education, 
                  x = education)) + 
  ggtitle("Distribution of mean aggression score by education") +
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
  ggplot(agg.workermeans) + 
  geom_violin(aes(mean.aggressionscore, 
                  fill = factor(english_first_language), 
                  x = factor(english_first_language))) + 
  ggtitle("Distribution of mean aggression score by first language") + 
  theme_minimal()
```

![](automatedmoderationnotebook_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Can we predict who will have a higher mean aggression score?

Do this over the data with NA's subsetted out.

``` r
lm(formula = mean.aggressionscore ~ gender + age_group + education + english_first_language,
   data = agg.workermeans %>% na.omit) %>% 
  summary %>% 
  pander(add.significance.stars = T)
```

<table>
<colgroup>
<col width="36%" />
<col width="13%" />
<col width="16%" />
<col width="12%" />
<col width="13%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">Estimate</th>
<th align="center">Std. Error</th>
<th align="center">t value</th>
<th align="center">Pr(&gt;|t|)</th>
<th></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>gendermale</strong></td>
<td align="center">0.05923</td>
<td align="center">0.01608</td>
<td align="center">3.683</td>
<td align="center">0.0002364</td>
<td>* * *</td>
</tr>
<tr class="even">
<td align="center"><strong>age_group30-45</strong></td>
<td align="center">-0.01965</td>
<td align="center">0.01675</td>
<td align="center">-1.173</td>
<td align="center">0.2408</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>age_group45-60</strong></td>
<td align="center">-0.0984</td>
<td align="center">0.02688</td>
<td align="center">-3.661</td>
<td align="center">0.0002571</td>
<td>* * *</td>
</tr>
<tr class="even">
<td align="center"><strong>age_groupOver 60</strong></td>
<td align="center">-0.08616</td>
<td align="center">0.08127</td>
<td align="center">-1.06</td>
<td align="center">0.2892</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>age_groupUnder 18</strong></td>
<td align="center">0.06915</td>
<td align="center">0.0607</td>
<td align="center">1.139</td>
<td align="center">0.2548</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationdoctorate</strong></td>
<td align="center">0.0816</td>
<td align="center">0.08098</td>
<td align="center">1.008</td>
<td align="center">0.3138</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationhs</strong></td>
<td align="center">-0.02203</td>
<td align="center">0.01901</td>
<td align="center">-1.159</td>
<td align="center">0.2468</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationmasters</strong></td>
<td align="center">-0.02372</td>
<td align="center">0.02217</td>
<td align="center">-1.07</td>
<td align="center">0.2848</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationnone</strong></td>
<td align="center">0.265</td>
<td align="center">0.3574</td>
<td align="center">0.7416</td>
<td align="center">0.4584</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>educationprofessional</strong></td>
<td align="center">-0.01572</td>
<td align="center">0.02642</td>
<td align="center">-0.5951</td>
<td align="center">0.5518</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>educationsome</strong></td>
<td align="center">0.009521</td>
<td align="center">0.0548</td>
<td align="center">0.1738</td>
<td align="center">0.8621</td>
<td></td>
</tr>
<tr class="even">
<td align="center"><strong>english_first_language</strong></td>
<td align="center">0.006494</td>
<td align="center">0.02022</td>
<td align="center">0.3211</td>
<td align="center">0.7481</td>
<td></td>
</tr>
<tr class="odd">
<td align="center"><strong>(Intercept)</strong></td>
<td align="center">-0.1874</td>
<td align="center">0.01758</td>
<td align="center">-10.66</td>
<td align="center">6.942e-26</td>
<td>* * *</td>
</tr>
</tbody>
</table>

<table style="width:85%;">
<caption>Fitting linear model: mean.aggressionscore ~ gender + age_group + education + english_first_language</caption>
<colgroup>
<col width="20%" />
<col width="30%" />
<col width="11%" />
<col width="22%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Observations</th>
<th align="center">Residual Std. Error</th>
<th align="center"><span class="math inline"><em>R</em><sup>2</sup></span></th>
<th align="center">Adjusted <span class="math inline"><em>R</em><sup>2</sup></span></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2154</td>
<td align="center">0.357</td>
<td align="center">0.01707</td>
<td align="center">0.01156</td>
</tr>
</tbody>
</table>

Making text classifiers
-----------------------

``` r
library(tidytext)
agg$comment <- gsub("NEWLINE_TOKEN", " ", agg$comment)

# combine demographics + annotations
# add column for how many of each gender
# were reviewing
agg.comb <- agg.ann %>% 
  left_join(agg.dem) %>%
  group_by(rev_id) %>%
  mutate(female_workers = sum(gender == "female"),
         male_workers = sum(gender == "male"),
         NA_workers = sum(is.na(gender)),
         other_workers = sum(gender == "other"))
```

    ## Joining, by = "worker_id"

``` r
agg %>% unnest_tokens(ngram, comment, token = "ngrams", n = 2) -> agg.text
agg.text %>% left_join(agg.comb) -> agg.text
```

    ## Joining, by = "rev_id"

``` r
agg.text %>% 
  filter(gender %in% c("female", "male") & # filter NA and other
         female_workers > 0,
         male_workers > 0) %>% # filter where there are no females
  mutate(gender_male = (gender == "male"), # make a dummy variable
         weighted_agg = # normalise the scores
           (gender_male * # trigger for male
              (aggression_score/male_workers) * 
              (male_workers + female_workers) * 0.5) +
           ((1-gender_male) * # trigger for female
              (aggression_score/female_workers) * 
              (male_workers + female_workers) * 0.5)
         ) %>%
  group_by(ngram, gender) %>% 
  summarise(sum.aggression = sum(weighted_agg)) -> ngram.agg.bygender
```

Which were the most offensive ngrams for women?

``` r
# Aggression for females
ngram.agg.bygender %>% filter(gender == "female") %>% select(ngram, agg.sum.fem = sum.aggression) -> ngram.agg.fem 

# Aggression for males
ngram.agg.bygender %>% filter(gender == "male") %>% select(ngram, agg.sum.mal = sum.aggression) -> ngram.agg.male

# Combine and calculate difference, and order
ngram.agg.male %>% left_join(ngram.agg.fem) %>% mutate(agg.sum.diff = abs(agg.sum.mal - agg.sum.fem)) %>% arrange(desc(agg.sum.diff)) -> ngram.compare 
```

    ## Joining, by = "ngram"

``` r
print(ngram.compare, n = 100)
```

    ## Source: local data frame [49,330 x 4]
    ## Groups: ngram [49,330]
    ## 
    ##                          ngram   agg.sum.mal  agg.sum.fem agg.sum.diff
    ##                          <chr>         <dbl>        <dbl>        <dbl>
    ## 1                    a bastard -2038.4000000 -1213.333333    825.06667
    ## 2                  bastard pro -2038.4000000 -1213.333333    825.06667
    ## 3     assad.hanibal911you're a -2027.2000000 -1206.666667    820.53333
    ## 4   pro assad.hanibal911you're -2027.2000000 -1206.666667    820.53333
    ## 5                   a dickhead -4096.8750000 -3450.000000    646.87500
    ## 6                       your a -4096.8750000 -3450.000000    646.87500
    ## 7                dickhead your -4085.0000000 -3440.000000    645.00000
    ## 8                     you have   -80.2908730  -184.550000    104.25913
    ## 9                      you are  -198.5343254  -286.561905     88.02758
    ## 10                    that you   -62.9071429  -146.504762     83.59762
    ## 11                      if you  -103.5668651  -186.782143     83.21528
    ## 12                       to be   -84.4490079  -157.184524     72.73552
    ## 13                      to the   -70.0855159  -138.008333     67.92282
    ## 14                      i have   -90.7867063  -157.555952     66.76925
    ## 15                   and makes   -71.0333333  -134.125000     63.09167
    ## 16                      on the   -66.4825397  -128.434524     61.95198
    ## 17    djathinkimacowboy's love   -70.2000000  -131.625000     61.42500
    ## 18  drinks djathinkimacowboy's   -70.2000000  -131.625000     61.42500
    ## 19                   juice and   -70.2000000  -131.625000     61.42500
    ## 20                  love juice   -70.2000000  -131.625000     61.42500
    ## 21                makes salvio   -70.2000000  -131.625000     61.42500
    ## 22                salvio watch   -70.2000000  -131.625000     61.42500
    ## 23    jealouslyfavonian drinks   -68.4000000  -128.250000     59.85000
    ## 24     watch jealouslyfavonian   -68.4000000  -128.250000     59.85000
    ## 25                        i am   -77.2954365  -135.633333     58.33790
    ## 26                   talk page   -15.3833333   -68.208333     52.82500
    ## 27                    fair use    29.8214286   -22.500000     52.32143
    ## 28                      to use     1.3519841   -49.309524     50.66151
    ## 29                     in this     1.4992063   -47.538095     49.03730
    ## 30                      can be    29.6039683   -18.276190     47.88016
    ## 31                      do not   -36.1815476   -82.605556     46.42401
    ## 32                       he is    -6.0904762   -51.300000     45.20952
    ## 33                       not a   -52.5396825   -97.383333     44.84365
    ## 34                       to do   -93.3738095  -136.670238     43.29643
    ## 35                      of the  -102.9720238  -145.908333     42.93631
    ## 36                     and the    -4.5224206   -47.391667     42.86925
    ## 37                    there is     0.4412698   -40.275000     40.71627
    ## 38                       be in    24.5000000   -16.000000     40.50000
    ## 39                     want to     7.0176282   -33.083333     40.10096
    ## 40                     to this     7.9242063   -31.650000     39.57421
    ## 41                       to me     3.2789683   -36.240079     39.51905
    ## 42                color f5fffa    59.9142857    21.333333     38.58095
    ## 43                     are you  -119.5555556  -157.428571     37.87302
    ## 44                   color 000    53.6857143    16.000000     37.68571
    ## 45                    would be     5.1912698   -31.858333     37.04960
    ## 46                       in my    20.5170635   -15.916667     36.43373
    ## 47                on wikipedia   -33.0726190   -69.275000     36.20238
    ## 48                      is not   -83.9287698  -120.016667     36.08790
    ## 49                    they are   -24.8376984   -60.925000     36.08730
    ## 50                   thank you    89.3051587    53.328571     35.97659
    ## 51                    you will   -13.8142857   -49.190476     35.37619
    ## 52                        to a     2.4928571   -32.133333     34.62619
    ## 53   aulahehelalelalala hassan    34.2857143     0.000000     34.28571
    ## 54                  hassan you    34.2857143     0.000000     34.28571
    ## 55                    my heart    34.2857143     0.000000     34.28571
    ## 56                        i do   -28.7523810   -63.013889     34.26151
    ## 57                       as an     6.1595238   -27.875000     34.03452
    ## 58                      to say   -12.5234127   -46.333333     33.80992
    ## 59                    with the   -21.7756258   -55.500000     33.72437
    ## 60                       is in     0.3869048   -32.839286     33.22619
    ## 61                        me i    -1.5093254   -34.583333     33.07401
    ## 62                 style color    48.6857143    16.000000     32.68571
    ## 63    heart aulahehelalelalala    32.5714286     0.000000     32.57143
    ## 64                   fuck fuck  -292.5000000  -325.000000     32.50000
    ## 65                to wikipedia    30.9892857    -1.041667     32.03095
    ## 66                of wikipedia    -6.5095238   -38.458333     31.94881
    ## 67                   always be    29.6190476    -2.000000     31.61905
    ## 68                 will always    29.6190476    -2.000000     31.61905
    ## 69                     to make    -6.6785714   -38.216667     31.53810
    ## 70                      do you   -29.3896825   -60.047222     30.65754
    ## 71                 the article    -0.3265873   -30.700000     30.37341
    ## 72                  would have     4.1071429   -25.666667     29.77381
    ## 73                        as a   -31.1690476   -60.866667     29.69762
    ## 74                    you need    -9.2904762   -38.500000     29.20952
    ## 75            background color    40.8547619    11.833333     29.02143
    ## 76                      one of     0.1619048   -28.375000     28.53690
    ## 77                    the most    14.1523810   -14.208333     28.36071
    ## 78                      you to   -19.2964286   -47.125000     27.82857
    ## 79                     on this     8.7382937   -18.958333     27.69663
    ## 80                     in fact    -3.7940476   -30.791667     26.99762
    ## 81                        in a   -23.6059524   -50.582143     26.97619
    ## 82                     part of    -8.1357143   -34.858333     26.72262
    ## 83                      how to    28.2202381     1.550000     26.67024
    ## 84                    links to     1.9964286   -24.583333     26.57976
    ## 85                     as well     4.4801587   -21.833333     26.31349
    ## 86                    the edit     3.3142857   -22.791667     26.10595
    ## 87                     link to   -10.9071429   -36.750000     25.84286
    ## 88                     are the   -12.1115079   -37.930952     25.81944
    ## 89                     do with   -57.7083333   -83.516667     25.80833
    ## 90                     have to   -29.3626984   -55.000000     25.63730
    ## 91                   it should    -7.5638889    18.000000     25.56389
    ## 92                    the link     5.0198413   -20.500000     25.51984
    ## 93                  agree that    14.1936508   -11.250000     25.44365
    ## 94                  to provide     9.4126984   -16.000000     25.41270
    ## 95                    hope you     2.8928571   -22.416667     25.30952
    ## 96                    like you   -10.8940476   -36.166667     25.27262
    ## 97                    a source    -5.0642857   -30.166667     25.10238
    ## 98                      use of     8.9059524   -16.142857     25.04881
    ## 99                   there are   -13.6583333   -38.500000     24.84167
    ## 100               or something   -13.5119048   -38.000000     24.48810
    ## # ... with 4.923e+04 more rows

<!-- Make dissent metric per comment -->
<!-- ```{r, cache = TRUE} -->
<!-- tox.long %>% filter(!is.na(gender)) %>%  -->
<!--   group_by(rev_id, gender) %>%  -->
<!--   filter(n() > 1) %>% -->
<!--   mutate(mean.tox = round(mean(toxicity)),  -->
<!--          dissenter = ifelse(toxicity != mean.tox, 1, 0)) %>% -->
<!--   summarise(dissent = sum(dissenter)/n()) -> tox.dissent -->
<!-- ``` -->
<!-- Sum dissent metric per comment -->
<!-- ```{r, cache = TRUE} -->
<!-- tox.dissent %>% filter(gender == "female")  -> tox.dissent.f -->
<!-- tox.dissent %>% filter(gender == "male")  -> tox.dissent.m -->
<!-- ``` -->

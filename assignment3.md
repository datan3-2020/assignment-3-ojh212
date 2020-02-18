Statistical assignment 3
================
Olivia Hudson Candidate Number: 089369
12/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier (pidp),
sample origin(memorig), sex(sex\_dv), age (age\_dv) and political
interest(vote6). It is tedious to join all the seven waves manually, and
it makes sense to use a loop in this case. Since you don’t yet know
about iteration I’ll provide the code for you; please see the
explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

work on this

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(wave = case_when(
    wave == "a" ~ "wave 1",
    wave == "b" ~ "wave 2",
    wave == "c" ~ "wave 3",
    wave == "d" ~ "wave 4",
    wave == "e" ~ "wave 5",
    wave == "f" ~ "wave 6",
    wave == "g" ~ "wave 7"))
```

i also renamed the waves for ease

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 1, "male",
                                   ifelse(sex_dv == 2, "female", NA))) %>%
        mutate(vote6 = ifelse(
          vote6 <0, NA_real_, vote6)) 

table(Long$sex_dv) 
```

    ## 
    ## female   male 
    ## 117665 100342

``` r
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
  group_by(wave, sex_dv) %>%
  summarise(meanVote6 = mean(vote6, na.rm = TRUE))
na.omit(meanVote6)
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave   sex_dv meanVote6
    ##    <chr>  <chr>      <dbl>
    ##  1 wave 1 female      2.84
    ##  2 wave 1 male        2.53
    ##  3 wave 2 female      2.82
    ##  4 wave 2 male        2.51
    ##  5 wave 3 female      2.87
    ##  6 wave 3 male        2.54
    ##  7 wave 4 female      2.89
    ##  8 wave 4 male        2.55
    ##  9 wave 5 female      2.87
    ## 10 wave 5 male        2.51
    ## 11 wave 6 female      2.81
    ## 12 wave 6 male        2.47
    ## 13 wave 7 female      2.73
    ## 14 wave 7 male        2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

**(NOTE: i have recoded the waves earlier so instead of alphabetical
letters they are properly named by wave numbers.)**

Write a short interpretation of your findings.

For the vote6 variable; an answer of 2 means that they are ‘Fairly
interested’ in politics and an answer of 3 means that they are ‘Not very
interested’ in politics.

From these findings we can see that: - the average political interest
for females in the data set is around 2.8 - this suggests that females
in the dataset are Fairly interested in politics - the average political
interest for males in the data set is around 2.5 - this suggests that
males in the dataset are also fairly interested in politics, but more so
than women This suggest that men in the data set are, on average, more
interested in politics than the females in the data set. This is fairly
consistent across the waves, but with the lowest values being at wave 7.

``` r
na.omit(meanVote6) %>%
        pivot_wider(names_from = wave, values_from = meanVote6)
```

    ## # A tibble: 2 x 8
    ##   sex_dv `wave 1` `wave 2` `wave 3` `wave 4` `wave 5` `wave 6` `wave 7`
    ##   <chr>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 female     2.84     2.82     2.87     2.89     2.87     2.81     2.73
    ## 2 male       2.53     2.51     2.54     2.55     2.51     2.47     2.42

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following. (“wave 1”, “wave 2”, “wave 3”,
“wave 4”, “wave 5”, “wave 6”, “wave 7”)

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
LongDelta <- Long %>%
  filter(!is.na(vote6)) %>%
  group_by(pidp) %>%
  pivot_wider(names_from = wave, values_from = vote6)
```

Health Insurance Case Study
================
Jonathan Zluticky
2023-03-04

# Data Description

**Rows**

- Each row represents and individual that has filed a health insurance
  claim

**Columns**

- age: age of primary beneficiary

- sex: insurance contractor gender, female, male

- bmi: Body mass index, providing an understanding of body, weights that
  are relatively high or low relative to height, objective index of body
  weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5
  to 24.

- children: Number of children covered by health insurance / Number of
  dependents

- smoker: Whether or not the individual was a smoker

- region: the beneficiary’s residential area in the US, northeast,
  southeast, southwest, northwest.

- Diabetic: Whether or not a person is a Diabetic:

- Blood pressure: An individuals blood pressure. Whether this number
  measure is systolic or diastolic was not noted. In a real world
  context a follow up question to the team that gathered this data will
  be needed.

- claim: Individual insurance claim in USD.

# Goal

The goal of this case study is to determine which factor contribute most
to the amount of an Individuals insurance claim.

# 1.) Cleaning Data

I start this study by examining the distribution of each variable to
check for values to check for potential errors. Any errors such as
mistyped data and missing values will be address before analyzing the
data.

## Checking data types

``` r
insurance%>%glimpse()
```

    ## Rows: 1,340
    ## Columns: 11
    ## $ index         <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ PatientID     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    ## $ age           <dbl> 39, 24, NA, NA, NA, NA, NA, 19, 20, 30, 36, 37, 19, 32, …
    ## $ gender        <chr> "male", "male", "male", "male", "male", "male", "male", …
    ## $ bmi           <dbl> 23.2, 30.1, 33.3, 33.7, 34.1, 34.4, 37.3, 41.1, 43.0, 53…
    ## $ bloodpressure <int> 91, 87, 82, 80, 100, 96, 86, 100, 86, 97, 88, 90, 81, 10…
    ## $ diabetic      <chr> "Yes", "No", "Yes", "No", "No", "Yes", "Yes", "No", "No"…
    ## $ children      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ smoker        <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "N…
    ## $ region        <chr> "southeast", "southeast", "southeast", "northwest", "nor…
    ## $ claim         <dbl> 1121.87, 1131.51, 1135.94, 1136.40, 1137.01, 1137.47, 11…

Here we see several of the variables are saves as characters instead of
factors.

``` r
#Changing Data types to factors
insurance1 <- insurance%>%
  mutate(smoker = as_factor(smoker),
         gender = as_factor(gender),
         smoker = as_factor(smoker),
         diabetic = as_factor(diabetic),
         region = as_factor(region))

insurance1%>%glimpse()
```

    ## Rows: 1,340
    ## Columns: 11
    ## $ index         <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ PatientID     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    ## $ age           <dbl> 39, 24, NA, NA, NA, NA, NA, 19, 20, 30, 36, 37, 19, 32, …
    ## $ gender        <fct> male, male, male, male, male, male, male, male, male, ma…
    ## $ bmi           <dbl> 23.2, 30.1, 33.3, 33.7, 34.1, 34.4, 37.3, 41.1, 43.0, 53…
    ## $ bloodpressure <int> 91, 87, 82, 80, 100, 96, 86, 100, 86, 97, 88, 90, 81, 10…
    ## $ diabetic      <fct> Yes, No, Yes, No, No, Yes, Yes, No, No, No, Yes, Yes, No…
    ## $ children      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ smoker        <fct> No, No, No, No, No, No, No, No, No, No, No, No, No, No, …
    ## $ region        <fct> southeast, southeast, southeast, northwest, northwest, n…
    ## $ claim         <dbl> 1121.87, 1131.51, 1135.94, 1136.40, 1137.01, 1137.47, 11…

## Checking for missing values

Next I will check for missing values in this data set

``` r
insurance1 %>% summary()
```

    ##      index          PatientID           age           gender         bmi       
    ##  Min.   :   0.0   Min.   :   1.0   Min.   :18.00   male  :678   Min.   :16.00  
    ##  1st Qu.: 334.8   1st Qu.: 335.8   1st Qu.:29.00   female:662   1st Qu.:26.27  
    ##  Median : 669.5   Median : 670.5   Median :38.00                Median :30.40  
    ##  Mean   : 669.5   Mean   : 670.5   Mean   :38.08                Mean   :30.67  
    ##  3rd Qu.:1004.2   3rd Qu.:1005.2   3rd Qu.:47.00                3rd Qu.:34.70  
    ##  Max.   :1339.0   Max.   :1340.0   Max.   :60.00                Max.   :53.10  
    ##                                    NA's   :5                                   
    ##  bloodpressure    diabetic     children     smoker           region   
    ##  Min.   : 80.00   Yes:642   Min.   :0.000   No :1066   southeast:443  
    ##  1st Qu.: 86.00   No :698   1st Qu.:0.000   Yes: 274   northwest:349  
    ##  Median : 92.00             Median :1.000              southwest:314  
    ##  Mean   : 94.16             Mean   :1.093              northeast:231  
    ##  3rd Qu.: 99.00             3rd Qu.:2.000              NA's     :  3  
    ##  Max.   :140.00             Max.   :5.000                             
    ##                                                                       
    ##      claim      
    ##  Min.   : 1122  
    ##  1st Qu.: 4720  
    ##  Median : 9370  
    ##  Mean   :13253  
    ##  3rd Qu.:16604  
    ##  Max.   :63770  
    ## 

Here we see that there are 5 observation that have missing ages and 3
observation that have missing regions. Following up with the team that
gathered this data could possibly give insight into what these values
should be. Since this cannot be done, these values will be removed.

``` r
insurance2 <-
  insurance1%>% drop_na()

insurance1%>%summary()
```

    ##      index          PatientID           age           gender         bmi       
    ##  Min.   :   0.0   Min.   :   1.0   Min.   :18.00   male  :678   Min.   :16.00  
    ##  1st Qu.: 334.8   1st Qu.: 335.8   1st Qu.:29.00   female:662   1st Qu.:26.27  
    ##  Median : 669.5   Median : 670.5   Median :38.00                Median :30.40  
    ##  Mean   : 669.5   Mean   : 670.5   Mean   :38.08                Mean   :30.67  
    ##  3rd Qu.:1004.2   3rd Qu.:1005.2   3rd Qu.:47.00                3rd Qu.:34.70  
    ##  Max.   :1339.0   Max.   :1340.0   Max.   :60.00                Max.   :53.10  
    ##                                    NA's   :5                                   
    ##  bloodpressure    diabetic     children     smoker           region   
    ##  Min.   : 80.00   Yes:642   Min.   :0.000   No :1066   southeast:443  
    ##  1st Qu.: 86.00   No :698   1st Qu.:0.000   Yes: 274   northwest:349  
    ##  Median : 92.00             Median :1.000              southwest:314  
    ##  Mean   : 94.16             Mean   :1.093              northeast:231  
    ##  3rd Qu.: 99.00             3rd Qu.:2.000              NA's     :  3  
    ##  Max.   :140.00             Max.   :5.000                             
    ##                                                                       
    ##      claim      
    ##  Min.   : 1122  
    ##  1st Qu.: 4720  
    ##  Median : 9370  
    ##  Mean   :13253  
    ##  3rd Qu.:16604  
    ##  Max.   :63770  
    ## 

## Distibution of Data

I will now explore the distribution of the data and check for any
extreme values that may signify errors made when data was recorded.

### Distibution of age

``` r
# Basic summary of age
insurance2%>% 
  pull(age) %>%
  psych::describe()
```

    ##    vars    n  mean    sd median trimmed   mad min max range skew kurtosis  se
    ## X1    1 1332 38.09 11.11     38   37.89 13.34  18  60    42 0.11    -0.95 0.3

``` r
# Histogram of age
insurance2%>%
  ggplot(mapping = aes(x = age, fill = age)) +
  geom_histogram(color = "black", fill = palette_light()[3], binwidth = 5)+
  labs( x = "Age",
        y = "FREQUENCY",
        title = "Distribution of Age") +
  theme_tq()
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
Here we see that the median and mean age is around 38 years old. The
data has maximum age in this set is 60 and a minimum of 18. There does
not appear to be any extreme values as all of our data falls with in two
standard devaitons of the mean. \### Distibution of BMI

``` r
# Basic summary of variables
insurance2%>% 
  pull(bmi) %>%
  psych::describe()
```

    ##    vars    n  mean   sd median trimmed mad min  max range skew kurtosis   se
    ## X1    1 1332 30.66 6.12  30.35   30.49 6.3  16 53.1  37.1 0.29    -0.07 0.17

The mean and median BMI of this data are very similar, 30.66 and 30.35
respectively. Meaning the average person would be classified as class 1
obese. The minimum value is 16 which would classify as severely
underweight and the maximum value is 53.1 which would classify as class
3 obesity. The value 53.13 seems very high and will be investigated
later. The standard deviation of the data set is 6.1 meaning the
majority of the BMI’s are between 24.6 and 36.7.

``` r
# Histogram of BMI
insurance2%>%
  ggplot(mapping = aes(x = bmi, fill = bmi)) +
  geom_histogram(color = "black", fill = palette_light()[3], binwidth = 1)+
  labs( x = "Body Mass Index",
        y = "FREQUENCY",
        title = "Distribution of BMI") +
  theme_tq()
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
The distribution of BMI is approximately symmetric with the majority of
data occurring around 30. There are also appear to be a couple of people
with really high BMI of around 53.

#### Investigating the outliers

``` r
insurance2 %>% 
  arrange(desc(bmi))%>% head(5)
```

    ##   index PatientID age gender  bmi bloodpressure diabetic children smoker
    ## 1     9        10  30   male 53.1            97       No        0     No
    ## 2  1299      1300  50   male 52.6           110       No        1    Yes
    ## 3   141       142  46   male 50.4            89      Yes        1     No
    ## 4   802       803  42   male 49.1           109      Yes        0     No
    ## 5   675       676  49 female 48.1            81      Yes        2     No
    ##      region    claim
    ## 1 northwest  1163.46
    ## 2 southeast 44501.40
    ## 3 southeast  2438.06
    ## 4 southeast 11381.33
    ## 5 northeast  9432.93

Doing some research on BMI, I found that although only 2% of the
population has bmi greater than 50, these are not unreasonable values
and may provide insight.

#### Creating Classes of BMI

Since peoples weight fluxation from day to day their BMI may move a
point or two in either direction. Classifying people based off of BMI as
underweight, healthy, overweight, etc. could be quite useful as one is
less likely to change category. This could also be useful as severely
underweight people may also have higher risk.

``` r
# Categorizing BMI 
insurance2 <- insurance2%>%
  mutate(bmi_cat = case_when(bmi<=16 ~ 'Serverly Under Weight',
                            bmi>16 & bmi<=18.5 ~ 'Under Weight',
                            bmi>18.5 & bmi<=25 ~ 'Healthy Weight',
                            bmi> 25 & bmi <= 30 ~ 'Over Weight',
                            bmi> 30 & bmi <= 35~ 'Obese class 1',
                            bmi> 35 & bmi <= 40~ 'Obese class 2',
                            bmi>40 ~ 'Obese class 3')%>% as_factor())

 # Frequency of bmi_cat
insurance2%>%group_by(bmi_cat)%>%
  summarise(n=n())%>%
  mutate(prop = (n/sum(n))%>% scales::percent())%>%
  arrange(n)
```

    ## # A tibble: 7 × 3
    ##   bmi_cat                   n prop  
    ##   <fct>                 <int> <chr> 
    ## 1 Serverly Under Weight     1 0.08% 
    ## 2 Under Weight             20 1.50% 
    ## 3 Obese class 3            92 6.91% 
    ## 4 Obese class 2           224 16.82%
    ## 5 Healthy Weight          227 17.04%
    ## 6 Obese class 1           377 28.30%
    ## 7 Over Weight             391 29.35%

The most common body BMI categories are Obese class 1, Overweight, and
Healthy Weight. The least common categories are severely under weight,
under weight, and obese class 3.

``` r
#Bar Chart for BMI Category
insurance2%>%
  ggplot()+
  geom_bar(mapping = aes(x=bmi_cat, fill=bmi_cat))+
  labs( x = "BMI Categories",
        y = "FREQUENCY",
        title = "Distribution of Catagories of BMI",
        caption = " Obese class 1 seems to be the most common bmi") +
  
  #themes
  scale_fill_tq() +
  theme_tq()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle= 45, vjust= 0.5))
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
The largest category is overweight and the lowest categories are
underweight, severely underweight and obese class three. This is
expected as the extreme BMI ranges should be less common.

### Distribution of claims

``` r
# Basic summary of variables
insurance2%>% 
  select(claim) %>%
  summary()
```

    ##      claim      
    ##  Min.   : 1122  
    ##  1st Qu.: 4760  
    ##  Median : 9413  
    ##  Mean   :13325  
    ##  3rd Qu.:16781  
    ##  Max.   :63770

The median of the charges is \$ 9413 and the mean is \$ 13325. The
minimum amount the insurer was charged was \$ 1122 and the maximum was
\$ 63770.

``` r
# Histogram of charges
insurance2%>%
  ggplot(mapping = aes(x = claim)) +
  geom_histogram(color ="black", fill=palette_light()[3])+
  labs( x = "Charges in thousands (USD)",
        y = "FREQUENCY",
        title = "Distribution of Insurance Claims ")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The data is skewed right and with must of the charges being under \$
20,000. There were several high charges around \$ 60,000

``` r
#Violin plot for charges
insurance2%>%
  ggplot(mapping = aes(x = claim, y = 1)) +
  geom_violin(fill = palette_light()[3]) +
   labs( x = "Charges in USD",
        title = "Distribution of Charges to Insurer") +
    theme_tq()
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The violin plot shows that there is a higher frequency of charges less
than \$2000. There are several entry’s that are very high (greater than
\$6000). These will be investigated to determine whether or not they
need to be removed.

#### Checking for outliers.

``` r
insurance2%>%
  arrange(desc(claim))%>%
  select(claim,bmi_cat,smoker)%>%
  head(10)
```

    ##       claim       bmi_cat smoker
    ## 1  63770.43 Obese class 3    Yes
    ## 2  62592.87 Obese class 1    Yes
    ## 3  60021.40 Obese class 1    Yes
    ## 4  58571.07 Obese class 2    Yes
    ## 5  55135.40 Obese class 2    Yes
    ## 6  52590.83 Obese class 1    Yes
    ## 7  51194.56 Obese class 2    Yes
    ## 8  49577.66 Obese class 2    Yes
    ## 9  48970.25 Obese class 3    Yes
    ## 10 48885.14 Obese class 2    Yes

The top insurances charges all come from people who smoke and are obese.
This information could be useful so the values should not be removed

\###Distribution of Smokers

``` r
insurance2%>%
  ggplot(mapping = aes(x = smoker, fill=smoker)) +
  geom_bar()+
  labs( x = "Smoking Status",
        title = "Distribution of Smokers") +
  #themes
  scale_fill_tq() +
  theme_tq()+
  theme(legend.position = "none")
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

There are more smokers than nonsmokers.

``` r
insurance2%>%
  summarize(prop_yes= mean(smoker=='Yes')%>% scales::percent(),
            prp_no= mean(smoker=='No')%>% scales::percent())
```

    ##   prop_yes prp_no
    ## 1      21%    79%

This data contains 21% smokers and % 79non-smokers. \### Distribution of
regions

``` r
# Bar Graph for Regions
insurance2%>%
  ggplot(mapping = aes(x = region, fill=region)) +
  geom_bar()+
  #Labels
  labs(x="Region of USA",
       y= "Frequency",
       title = "Distribution of Region")+
  #themes
  scale_fill_tq() +
  theme_tq()+
  theme(legend.position = "none")
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
Individuals do not appear to be evenly distributed with the southeast
havingmore representation that the other regions and the northeast
having less.

``` r
insurance2%>%
  summarize(prop_SW= mean(region=='southwest')%>% scales::percent(),
            prp_SE= mean(region=='southeast')%>% scales::percent(),
            prp_NW= mean(region=='northwest')%>% scales::percent(),
            prp_NE= mean(region=='northeast')%>% scales::percent())
```

    ##   prop_SW prp_SE prp_NW prp_NE
    ## 1     24%    33%    26%    17%

### Distribution of blood Pressure

Blood pressure is usually measured with two numbers systolic and
diastolic. This data has only 1 value and it was not noted in the data
description. I will investigate the values to try and determine which it
is.

``` r
insurance2%>% 
  select(bloodpressure) %>%
  summary()
```

    ##  bloodpressure   
    ##  Min.   : 80.00  
    ##  1st Qu.: 86.00  
    ##  Median : 92.00  
    ##  Mean   : 94.19  
    ##  3rd Qu.: 99.00  
    ##  Max.   :140.00

``` r
# Histogram of charges
insurance2%>%
  ggplot(mapping = aes(x = bloodpressure)) +
  geom_histogram(color ="black", fill=palette_light()[3])+
  labs( x = "Blood Pressure",
        y = "FREQUENCY",
        title = "Distribution of Blood Pressure")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->
Although the the whether this measure is systolic or diastolic was not
noted, based on the fact that there are quite a few individuals with
blood pressure well over 120 I assume this is systolic. However the
lower values around 80 seem to low for systolic as most people would be
consider to have low blood pressure. Since this measure is unclear and I
am unable to find out what this measure represents, I will exclude this
variable from my analysis.

# 2.) Analyzing Data with Covariation

## Is there a relationship between Smoking and Claims

would hupothisize that there would be a smokers tend to have higher
insurance charges. I

``` r
insurance2%>%
  ggplot(mapping = aes(x= reorder(smoker, claim, median), 
                       y= claim,
                       fill = smoker))+
  geom_violin()+
  labs(x="Smoking Status",
       y="Charges in USD",
       title= "Coorilation Between Region and Charges")+
  theme_tq()+
  scale_color_tq()+
  coord_flip()+
  theme(legend.position = 'none')
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
It does appear that smoker tend to have higher claims

## Is the relationship between BMI and Claimss?

I would hypothesize that there would be a positive correlation between
BMI and Charges, meaning the higher your BMI the more charges you would
have.

``` r
# Correlation between bmi and charges
cor_BMI_Chrgs <- cor(insurance2$claim, insurance2$bmi)


cor_BMI_Chrgs<- round(cor_BMI_Chrgs, digits =2)

cor_BMI_Chrgs
```

    ## [1] 0.2

Having a correlation coefficient of .2 suggests the two variables are
not very correlated. This is surprising.

``` r
insurance2%>%
  ggplot(mapping=aes(x=bmi, y=claim))+
  geom_point( color = palette_dark()[3])+
  geom_smooth(method = 'lm')+
  labs( x = "Body Mass Index",
        y = "Charges in USD",
        title = "Relationship between BMI and Charges") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

It appears that there is a positive correlation between an individuals
BMI and the amount charges. However there appears to be two clusters
that might be occurring in this data.

## What is the correlation between BMI, Claims, and Smoking Status.

``` r
# Correlation between bmi and charges for smokers

insurance_smk <- insurance2%>%filter(smoker == 'Yes')

cor_BMI_Chrgs_smk <- 
  cor(insurance_smk$claim, insurance_smk$bmi)

cor_BMI_Chrgs_smk
```

    ## [1] 0.8067705

When looking at the subset of individuals that smoke, we see a strong
correlation between insurance charges and BMI. Meaning if you smoke, the
more your BMI effects your insurance charges.

``` r
insurance_nsmk <- insurance%>%filter(smoker == 'No')

cor_BMI_Chrgs_nsmk <- 
  cor(insurance_nsmk$claim, insurance_nsmk$bmi)

cor_BMI_Chrgs_nsmk
```

    ## [1] 0.08259916

When looking at the subset of individuals that do not smoke, we see that
there is a low correlation between BMI and Insurance charges.

``` r
insurance2%>%
  ggplot(mapping=aes(x=bmi, y=claim, color=smoker))+
  geom_point()+
  geom_smooth(method='lm')+
  labs( x = "Body Mass Index",
        y = "Charges in USD",
        title = "Relationship between BMI and Charges") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

It appears that if you are a nonsmoker then your BMI does not greatly
impact you claim, but if you do smoke, then the higher your BMI you can
expect higher charges.

## Is there a relationship between region and Claims

``` r
insurance2%>%
  ggplot(mapping = aes(x= reorder(region, claim, FUN = median), 
                       y= claim, 
                       color= region,
                       fill = region))+
  geom_boxplot()+
  labs(x="Region",
       y="Insurance Claim",
       title= "Coorilation Between Region and Insurance Claim")+
  theme_tq()+
  scale_color_tq()+
  coord_flip()+
  theme(legend.position = 'none')
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
There does not appear to be any significant difference in claims based
on region.

``` r
insurance2%>%
  group_by(region)%>%
  summarize(average__claim_region = mean(claim))
```

    ## # A tibble: 4 × 2
    ##   region    average__claim_region
    ##   <fct>                     <dbl>
    ## 1 southeast                13085.
    ## 2 northwest                11794.
    ## 3 southwest                12723.
    ## 4 northeast                16889.

The north ease has a slightly higher average claims. I will compare
smoking status and region to see if the northeast has higher amount of
smokers.

## Is there a coorilation between a person region and smoker status

``` r
#  bar graph of smokers per region
insurance2%>%
  filter(smoker == 'Yes')%>%
  ggplot(mapping = aes(x = region, fill=region)) +
  geom_bar()+
  #Labels
  labs(x="Region",
       y="Frequency of Smokers",
       title= "Coorilation Between Region and Smoker Status")+
  #themes
  scale_fill_tq() +
  theme_tq()+
  theme(legend.position =  'none')
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

The southeast has the most smokers and the northwest and southwest have
the least amount of smokers. However this can be misleading as the
highest number of observations came from the southeast and the lowest
number of observations came northeast. Instead I will look at the
proportion of smokers in each region.

``` r
#Percent of population that are smokers.
insurance2%>%
  group_by(region)%>%
  summarize(prop_smokers= mean(smoker=='Yes')%>% scales::percent())
```

    ## # A tibble: 4 × 2
    ##   region    prop_smokers
    ##   <fct>     <chr>       
    ## 1 southeast 21%         
    ## 2 northwest 17%         
    ## 3 southwest 18%         
    ## 4 northeast 29%

Here we see that the northeast has highest percent of smokers. This is
most likely why this region has the highest amount of insurance charges.

## Is there a relationship between age and claims.

``` r
insurance2%>%
  ggplot(mapping = aes(x=age, y= claim), color = palette_light(3)) +
  geom_point(color = palette_dark()[3])+
  labs(x="Age",
       y="Claims (USD)",
       title= "Coorilation Between Region and BMI")+
  theme_tq()+
  scale_color_tq()
```

![](Health-Insurance-Case-Study_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->
There does not appear to be any correlation between age and insurance
claims. \# Conclusion

According to this data the greatest factors that lead to an individual
to have high insurance claims are smoking status and Bmi. If an
indidividual smokes and has a higher bmi, they are at greater risk of
having higher insurance claims.

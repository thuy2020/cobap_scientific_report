
## Multiple linear regression model

I run a multiple regression to examine the relationship between
countries’ mortality rate and border closure policy type due to
COVID-19.

**Outcome**: Countries’ mortarity rate.

**Predictor**: Border closure policy type during the pandemic year
(complete closure vs. partial closure).

**Controlled variables**: test number, case number, critical case rate,
government effectiveness score, proportion of population aged 65 or
older, number of hospital beds, number of deaths attributable to
communicable diseases, and transport infrastructure quality score.

### Data

``` r
# Source:  https://doi.org/10.7910/DVN/U6DJAC updated May 2, 2021
cobap <- rio::import(here::here("data", "policy_list.csv")) %>%   select(ISO3, COUNTRY_NAME, POLICY_TYPE, POLICY_SUBTYPE, START_DATE, END_DATE)

control_data <- readRDS("coviddata.RDS") # control Variables - This data is prepared by Mark A. Weiss

data <- left_join(cobap, control_data ) %>% 
  drop_na() 

#convert some columns to correct types
data[, 9:ncol(data)] <- lapply(9:ncol(data), function(x) 
                               as.numeric(data[[x]]))
data[, 3:4] <- lapply(3:4, function(x) 
                               as.factor(data[[x]]))

# date variables
data <- data %>% 
  mutate(START_DATE = lubridate::mdy(data$START_DATE), 
         END_DATE = lubridate::mdy(data$END_DATE))
head(data)
```

    ##   ISO3 COUNTRY_NAME POLICY_TYPE POLICY_SUBTYPE START_DATE   END_DATE Country.x
    ## 1  MAR      Morocco    COMPLETE ESSENTIAL_ONLY 2020-03-20 2020-06-01   Morocco
    ## 2  JOR       Jordan    COMPLETE ESSENTIAL_ONLY 2020-03-17 2020-07-01    Jordan
    ## 3  LBN      Lebanon    COMPLETE ESSENTIAL_ONLY 2020-03-18 2020-03-29   Lebanon
    ## 4  JOR       Jordan    COMPLETE ESSENTIAL_ONLY 2020-03-17 2020-07-04    Jordan
    ## 5  MYS     Malaysia    COMPLETE ESSENTIAL_ONLY 2020-03-18 2020-12-31  Malaysia
    ## 6  ECU      Ecuador    COMPLETE ESSENTIAL_ONLY 2020-03-16 2020-04-03   Ecuador
    ##   cases/1000 tests/100 critical case rate mortalityrate       Score beds
    ## 1 0.23341156  1.054939       0.0008130081     1.2419196 -0.11922524 1.00
    ## 2 0.08972411  2.521581       0.0054644809     0.6849167  0.09947027 1.47
    ## 3 0.20829699  1.514474       0.0070323488     1.1568900 -0.83280247 2.73
    ## 4 0.08972411  2.521581       0.0054644809     0.6849167  0.09947027 1.47
    ## 5 0.25976951  1.911819       0.0004760771     0.8822496  0.99868566 1.88
    ## 6 2.59681161  0.727088       0.0047839574     2.2366691 -0.39858419 1.39
    ##   population_65_percent death_communicable_disease logisics_score
    ## 1              7.300455                       14.0           2.43
    ## 2              3.893416                       10.7           2.72
    ## 3              7.273292                        3.6           2.64
    ## 4              3.893416                       10.7           2.72
    ## 5              6.920771                       17.5           3.15
    ## 6              7.372199                       15.1           2.72

### Replication of Liang’s et al.

“In the multiple regression analysis, Covid-19 mortality rate was
regressed on Covid-19 test number, case number, critical case rate,
government effectiveness score, proportion of population aged 65 or
older, number of beds, deaths attributable to communicable diseases, and
transport infrastructure quality score.” (Liang, et al, p.2)

``` r
# multiple regression
model1 <- lm(mortalityrate ~ `tests/100` + `cases/1000` + `critical case rate` +
               `Score` + `population_65_percent` + beds + `death_communicable_disease`+
               logisics_score,
               data = data)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = mortalityrate ~ `tests/100` + `cases/1000` + `critical case rate` + 
    ##     Score + population_65_percent + beds + death_communicable_disease + 
    ##     logisics_score, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.91807 -0.30193  0.00226  0.19645  1.62130 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.561507   0.163084  -3.443 0.000618 ***
    ## `tests/100`                -0.028236   0.005801  -4.868 1.46e-06 ***
    ## `cases/1000`                0.007634   0.007639   0.999 0.318023    
    ## `critical case rate`       19.216809   3.414672   5.628 2.87e-08 ***
    ## Score                      -0.383603   0.043651  -8.788  < 2e-16 ***
    ## population_65_percent       0.098013   0.005051  19.406  < 2e-16 ***
    ## beds                       -0.088522   0.009393  -9.424  < 2e-16 ***
    ## death_communicable_disease  0.000622   0.003221   0.193 0.846952    
    ## logisics_score              0.385968   0.056362   6.848 1.95e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4206 on 568 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.5791, Adjusted R-squared:  0.5732 
    ## F-statistic: 97.68 on 8 and 568 DF,  p-value: < 2.2e-16

### Adding POLICY\_TYPE in COBAP data

A multiple linear regression was run to examine the relationship between
mortality rate and border closure policy type after controlling for
Covid-19 test number, case number, critical case rate, government
effectiveness score, proportion of the population aged 65 or older,
number of beds, deaths attributable to communicable diseases, and
transport infrastructure quality score. Changing POLICY\_TYPE from
COMPLETE to PARTIAl significantly lower the mortality rate by 0.087 (sd
= 0.04), t(567) = -2.1, p = 0.03.

``` r
# POLICY_TYPE 
model_policytype <- lm(mortalityrate ~ POLICY_TYPE + `tests/100` + `cases/1000` + `critical case rate` +
               `Score` + `population_65_percent` + beds + `death_communicable_disease`+
               logisics_score,
               data = data)

summary(model_policytype)
```

    ## 
    ## Call:
    ## lm(formula = mortalityrate ~ POLICY_TYPE + `tests/100` + `cases/1000` + 
    ##     `critical case rate` + Score + population_65_percent + beds + 
    ##     death_communicable_disease + logisics_score, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9902 -0.2912 -0.0076  0.2115  1.6489 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.538338   0.162949  -3.304  0.00101 ** 
    ## POLICY_TYPEPARTIAL         -0.086721   0.040839  -2.123  0.03415 *  
    ## `tests/100`                -0.026683   0.005829  -4.578 5.78e-06 ***
    ## `cases/1000`                0.006861   0.007624   0.900  0.36856    
    ## `critical case rate`       19.288450   3.404340   5.666 2.33e-08 ***
    ## Score                      -0.391017   0.043657  -8.957  < 2e-16 ***
    ## population_65_percent       0.097187   0.005050  19.245  < 2e-16 ***
    ## beds                       -0.086892   0.009396  -9.248  < 2e-16 ***
    ## death_communicable_disease  0.001104   0.003219   0.343  0.73170    
    ## logisics_score              0.398998   0.056523   7.059 4.92e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4193 on 567 degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## Multiple R-squared:  0.5824, Adjusted R-squared:  0.5758 
    ## F-statistic: 87.87 on 9 and 567 DF,  p-value: < 2.2e-16

### During the weeks of more than 50 policies issued

When considering only weeks in which more than 50 policies were issued
per week worldwide, the above effect of POLICY\_TYPE changed its
direction, but was no longer significant. b1 = 0.01, sd = 0.07, t(198) =
0.16, p =
0.87.

``` r
# during the weeks that had more than 50 policies issued/ week -> 214 policies
  week_morethan50 <- data %>% 
  add_count(week = lubridate::floor_date(START_DATE, "week")) %>% 
  arrange(desc(n)) %>% 
  filter(n > 50)

model_policytype_week_morethan50 <- lm(mortalityrate ~ POLICY_TYPE + `tests/100` + `cases/1000` + `critical case rate` + `Score` + `population_65_percent` + beds + `death_communicable_disease`+
               logisics_score,
               data = week_morethan50)
summary(model_policytype_week_morethan50)
```

    ## 
    ## Call:
    ## lm(formula = mortalityrate ~ POLICY_TYPE + `tests/100` + `cases/1000` + 
    ##     `critical case rate` + Score + population_65_percent + beds + 
    ##     death_communicable_disease + logisics_score, data = week_morethan50)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.09681 -0.26637 -0.01693  0.21981  1.58711 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                -0.444572   0.308999  -1.439 0.151801    
    ## POLICY_TYPEPARTIAL          0.011751   0.072562   0.162 0.871514    
    ## `tests/100`                -0.029184   0.010176  -2.868 0.004578 ** 
    ## `cases/1000`                0.002656   0.012571   0.211 0.832880    
    ## `critical case rate`       10.387812   4.726648   2.198 0.029129 *  
    ## Score                      -0.303241   0.085281  -3.556 0.000471 ***
    ## population_65_percent       0.099619   0.008542  11.662  < 2e-16 ***
    ## beds                       -0.095097   0.017932  -5.303 3.03e-07 ***
    ## death_communicable_disease  0.004909   0.005479   0.896 0.371351    
    ## logisics_score              0.339910   0.109482   3.105 0.002184 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.439 on 198 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.5739, Adjusted R-squared:  0.5545 
    ## F-statistic: 29.63 on 9 and 198 DF,  p-value: < 2.2e-16

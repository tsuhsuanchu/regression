
# Stroke Dataset

Read the dataset

``` r
df = read.csv("stroke.csv", na.strings = c("N/A"))
head(df)
```

Which variables has any missing data

``` r
colSums(is.na(df))
```

    ##                id            gender               age 
    ##                 0                 0                 0 
    ##      hypertension     heart_disease      ever_married 
    ##                 0                 0                 0 
    ##         work_type    Residence_type avg_glucose_level 
    ##                 0                 0                 0 
    ##               bmi    smoking_status            stroke 
    ##               201                 0                 0

Only bmi has some N/A values

``` r
### Missing Values
#Distribution for BMI
boxplot(df$bmi,data=df, main="Distribution BMI",xlab="BMI", ylab="Counts",horizontal= TRUE)
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Normality of BMI
shapiro.test(df$bmi)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$bmi
    ## W = 0.95355, p-value < 2.2e-16

``` r
### Test normality based on Stroke group

data_no_stroke<- subset(df, stroke == 0)
shapiro.test(data_no_stroke$bmi)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  data_no_stroke$bmi
    ## W = 0.95244, p-value < 2.2e-16

``` r
data_yes_stroke <- subset(df, stroke == 1)
shapiro.test(data_yes_stroke$bmi)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  data_yes_stroke$bmi
    ## W = 0.95638, p-value = 5.236e-06

## Data changes for missing values in bmi

``` r
median_bmi<- median(df$bmi,na.rm = TRUE)
df$bmi <- ifelse(is.na(df$bmi), median_bmi, df$bmi)
colSums(is.na(df))
```

    ##                id            gender               age 
    ##                 0                 0                 0 
    ##      hypertension     heart_disease      ever_married 
    ##                 0                 0                 0 
    ##         work_type    Residence_type avg_glucose_level 
    ##                 0                 0                 0 
    ##               bmi    smoking_status            stroke 
    ##                 0                 0                 0

No more missing values after that

## Remove id column from the dataset

``` r
df <- subset(df, select = -c(id))
head(df)
```

# EDA

## Histogram of each variable

``` r
library('ggplot2')

# Create a for loop to generate a histogram for each column
for (col in colnames(df)) {
  p <- ggplot(data = df) +
    geom_bar(mapping = aes(x = df[[col]])) +
    labs(title = col)
  print(p)
}
```

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->

    ## Warning: Use of `df[[col]]` is discouraged.
    ## ℹ Use `.data[[col]]` instead.

![](StrokePrediction_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->

The dependent variable (DV), stroke, predominantly includes non-stroke
patients. Among independent variables (IVs), BMI appears fairly normal
but slightly right-skewed, while glucose levels are more significantly
right-skewed. The age distribution spans 0-80 years and is relatively
normal. For categorical IVs, the gender distribution is nearly balanced,
with slightly more females. Most patients do not have hypertension or
heart disease. The marital status shows a majority being ever married at
about a 2:1 ratio. Employment is mostly private, and residence type is
evenly split with a slight preference for urban areas. Smoking status
reveals ‘never smoked’ as most common, with ‘smokes’ and ‘former smoked’
nearly equal; many records list smoking status as unknown.

## chi-square test for initial selection for categorical IVs

``` r
#DV vs. categorical IVs
stroke_gender_tb <- table(df$stroke, df$gender)
stroke_hypertension_tb <- table(df$stroke, df$hypertension)
stroke_hd_tb <- table(df$stroke, df$heart_disease)
stroke_married_tb <- table(df$stroke, df$ever_married)
stroke_work_tb <- table(df$stroke, df$work_type)
stroke_Residence_tb <- table(df$stroke, df$Residence_type)
stroke_Smoking_tb <- table(df$stroke, df$smoking_status)
```

``` r
chisq.test(stroke_gender_tb)
```

    ## Warning in chisq.test(stroke_gender_tb): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  stroke_gender_tb
    ## X-squared = 0.47259, df = 2, p-value = 0.7895

``` r
chisq.test(stroke_hypertension_tb)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity
    ##  correction
    ## 
    ## data:  stroke_hypertension_tb
    ## X-squared = 81.605, df = 1, p-value < 2.2e-16

``` r
chisq.test(stroke_hd_tb)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity
    ##  correction
    ## 
    ## data:  stroke_hd_tb
    ## X-squared = 90.26, df = 1, p-value < 2.2e-16

``` r
chisq.test(stroke_married_tb)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity
    ##  correction
    ## 
    ## data:  stroke_married_tb
    ## X-squared = 58.924, df = 1, p-value = 1.639e-14

``` r
chisq.test(stroke_work_tb)
```

    ## Warning in chisq.test(stroke_work_tb): Chi-squared approximation
    ## may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  stroke_work_tb
    ## X-squared = 49.164, df = 4, p-value = 5.398e-10

``` r
chisq.test(stroke_Residence_tb)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity
    ##  correction
    ## 
    ## data:  stroke_Residence_tb
    ## X-squared = 1.0816, df = 1, p-value = 0.2983

``` r
chisq.test(stroke_Smoking_tb)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  stroke_Smoking_tb
    ## X-squared = 29.147, df = 3, p-value = 2.085e-06

## t-test or Mann-Whitney U test for initial selection for continuous IVs

``` r
#Normality Assumption
groups <- split(df$bmi,df$stroke)
shapiro_results <- lapply(groups, shapiro.test)
p_values <- sapply(shapiro_results, function(x) x$p.value)
p_values
```

    ##            0            1 
    ## 9.811863e-38 6.427205e-10

``` r
#Mann-whiteny U test
mwu_result <- wilcox.test(df$bmi, df$stroke)
mwu_result
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$bmi and df$stroke
    ## W = 26112100, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

``` r
#Normality Assumption
groups <- split(df$age,df$stroke)
shapiro_results <- lapply(groups, shapiro.test)
p_values <- sapply(shapiro_results, function(x) x$p.value)
p_values
```

    ##            0            1 
    ## 1.633259e-30 3.173725e-13

``` r
#Mann-whiteny U test
mwu_result <- wilcox.test(df$age, df$stroke)
mwu_result
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$age and df$stroke
    ## W = 26100770, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

``` r
#Normality Assumption
groups <- split(df$avg_glucose_level,df$stroke)
shapiro_results <- lapply(groups, shapiro.test)
p_values <- sapply(shapiro_results, function(x) x$p.value)
p_values
```

    ##            0            1 
    ## 1.147529e-60 1.231952e-13

``` r
#Mann-whiteny U test
mwu_result <- wilcox.test(df$avg_glucose_level, df$stroke)
mwu_result
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$avg_glucose_level and df$stroke
    ## W = 26112100, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

# Multicollinearity

Check for multicollinearity

``` r
library(car)
```

    ## Loading required package: carData

``` r
M <- lm(stroke~.,data=df) 
vif(M) 
```

    ##                       GVIF Df GVIF^(1/(2*Df))
    ## gender            1.030611  2        1.007566
    ## age               2.869139  1        1.693853
    ## hypertension      1.118303  1        1.057499
    ## heart_disease     1.114817  1        1.055849
    ## ever_married      1.988336  1        1.410084
    ## work_type         2.557020  4        1.124519
    ## Residence_type    1.002762  1        1.001380
    ## avg_glucose_level 1.108128  1        1.052677
    ## bmi               1.298832  1        1.139663
    ## smoking_status    1.439550  3        1.062603

# Interaction plots

From all the plots, smoking_status and bmi seem to have the strongest
interaction

``` r
interaction.plot(x.factor = df$stroke,
                 trace.factor = df$smoking_status,
                 response = df$bmi,
                 fun = median,
                 ylab = "bmi",
                 xlab = "stroke",
                 col = c("red", "orange",'green','blue','gray'),
                 trace.label = "smoking_status")
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
interaction.plot(x.factor = df$stroke,
                 trace.factor = df$work_type,
                 response = df$bmi,
                 fun = median,
                 ylab = "bmi",
                 xlab = "stroke",
                 col = c("red", "orange",'green','blue','gray'),
                 trace.label = "work_type")
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
interaction.plot(x.factor = df$stroke,
                 trace.factor = df$work_type,
                 response = df$avg_glucose_level,
                 fun = median,
                 ylab = "avg_glucose_level",
                 xlab = "stroke",
                 col = c("red", "orange",'green','blue','gray'),
                 trace.label = "work_type")
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
interaction.plot(x.factor = df$stroke,
                 trace.factor = df$smoking_status,
                 response = df$age,
                 fun = median,
                 ylab = "age",
                 xlab = "stroke",
                 col = c("red", "orange",'green','blue','gray'),
                 trace.label = "smoking_status")
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
interaction.plot(x.factor = df$stroke,
                 trace.factor = df$work_type,
                 response = df$age,
                 fun = median,
                 ylab = "age",
                 xlab = "stroke",
                 col = c("red", "orange",'green','blue','gray'),
                 trace.label = "work_type")
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

# Train-Test split

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Registered S3 method overwritten by 'data.table':
    ##   method           from
    ##   print.data.table

``` r
library(rlang)
set.seed(123)  # Set a seed for reproducibility
split <- createDataPartition(df$stroke, p = 0.8, list = FALSE)  # Create the train-test split
train <- df[split, ]  # Training data
test <- df[-split, ]  # Testing data
```

``` r
dim(train)
```

    ## [1] 4088   11

``` r
dim(test)
```

    ## [1] 1022   11

# Models and variables selection

## Forward Selection

``` r
library(MASS) 
# Fit the full model  
full.model <- lm(stroke~., data = df) 
# Stepwise regression model 
step.model <- step(full.model, direction = "forward", trace = 0) 
summary(step.model) 
```

    ## 
    ## Call:
    ## lm(formula = stroke ~ gender + age + hypertension + heart_disease + 
    ##     ever_married + work_type + Residence_type + avg_glucose_level + 
    ##     bmi + smoking_status, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.29449 -0.07888 -0.02394  0.00596  1.02319 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                -3.217e-02  1.662e-02  -1.935 0.053014
    ## genderMale                 -1.589e-03  5.942e-03  -0.267 0.789173
    ## genderOther                -2.804e-02  2.066e-01  -0.136 0.892089
    ## age                         3.092e-03  2.162e-04  14.300  < 2e-16
    ## hypertension                3.858e-02  1.029e-02   3.749 0.000180
    ## heart_disease               5.009e-02  1.348e-02   3.715 0.000205
    ## ever_marriedYes            -3.447e-02  8.569e-03  -4.023 5.84e-05
    ## work_typeGovt_job          -6.210e-02  1.522e-02  -4.079 4.59e-05
    ## work_typeNever_worked      -2.533e-02  4.501e-02  -0.563 0.573570
    ## work_typePrivate           -4.746e-02  1.280e-02  -3.707 0.000212
    ## work_typeSelf-employed     -6.696e-02  1.552e-02  -4.316 1.62e-05
    ## Residence_typeUrban         5.232e-03  5.782e-03   0.905 0.365515
    ## avg_glucose_level           3.024e-04  6.711e-05   4.506 6.74e-06
    ## bmi                        -7.178e-04  4.273e-04  -1.680 0.093032
    ## smoking_statusnever smoked -8.624e-03  8.538e-03  -1.010 0.312551
    ## smoking_statussmokes       -2.424e-03  1.022e-02  -0.237 0.812504
    ## smoking_statusUnknown      -1.443e-03  9.655e-03  -0.149 0.881218
    ##                               
    ## (Intercept)                .  
    ## genderMale                    
    ## genderOther                   
    ## age                        ***
    ## hypertension               ***
    ## heart_disease              ***
    ## ever_marriedYes            ***
    ## work_typeGovt_job          ***
    ## work_typeNever_worked         
    ## work_typePrivate           ***
    ## work_typeSelf-employed     ***
    ## Residence_typeUrban           
    ## avg_glucose_level          ***
    ## bmi                        .  
    ## smoking_statusnever smoked    
    ## smoking_statussmokes          
    ## smoking_statusUnknown         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2063 on 5093 degrees of freedom
    ## Multiple R-squared:  0.08461,    Adjusted R-squared:  0.08173 
    ## F-statistic: 29.42 on 16 and 5093 DF,  p-value: < 2.2e-16

From the full model, age, hypertension, heart_disease, ever_married,
avg_glucose_level would have an effect. Gender and bmi don’t seem to
have an impact.

#### gender

``` r
model <- glm(factor(stroke) ~ factor(gender) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ factor(gender), family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.3111  -0.3111  -0.3110  -0.3110   2.4712  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -3.004981   0.096374 -31.180   <2e-16 ***
    ## factor(gender)Male    0.001157   0.149693   0.008    0.994    
    ## factor(gender)Other  -9.561082 324.743710  -0.029    0.977    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1555.1  on 4085  degrees of freedom
    ## AIC: 1561.1
    ## 
    ## Number of Fisher Scoring iterations: 11

gender is NOT significant

#### age

``` r
model <- glm(factor(stroke) ~ age , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7364  -0.3328  -0.1793  -0.0828   3.7760  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.225714   0.379753  -19.03   <2e-16 ***
    ## age          0.073893   0.005571   13.27   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1269.3  on 4086  degrees of freedom
    ## AIC: 1273.3
    ## 
    ## Number of Fisher Scoring iterations: 7

age is significant

#### hypertension

``` r
model <- glm(factor(stroke) ~ factor(hypertension) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ factor(hypertension), family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5485  -0.2752  -0.2752  -0.2752   2.5662  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -3.25472    0.08707 -37.381   <2e-16 ***
    ## factor(hypertension)1  1.43653    0.16833   8.534   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1495.3  on 4086  degrees of freedom
    ## AIC: 1499.3
    ## 
    ## Number of Fisher Scoring iterations: 6

hypertension is significant

#### age + hypertension

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension), family = "binomial", 
    ##     data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8887  -0.3247  -0.1730  -0.0823   3.7640  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -7.176716   0.381436 -18.815  < 2e-16 ***
    ## age                    0.071007   0.005661  12.543  < 2e-16 ***
    ## factor(hypertension)1  0.628996   0.176857   3.557 0.000376 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1257.5  on 4085  degrees of freedom
    ## AIC: 1263.5
    ## 
    ## Number of Fisher Scoring iterations: 7

Age + Hypertension (AIC: 1263.5) \< Age (AIC: 1273.3)  
Age + Hypertension (AIC: 1263.5) \< Hypertension (AIC: 1499.3)

``` r
pchisq(1269.3-1257.5,4086-4085,lower.tail = FALSE)
```

    ## [1] 0.0005923072

``` r
pchisq(1495.3-1257.5,4086-4085,lower.tail = FALSE)
```

    ## [1] 1.186896e-53

We move on with the full model

#### age + hypertension + heart_disease

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension)+factor(heart_disease) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + factor(heart_disease), 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0111  -0.3172  -0.1721  -0.0836   3.7448  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -7.101801   0.381984  -18.59  < 2e-16 ***
    ## age                     0.068960   0.005747   12.00  < 2e-16 ***
    ## factor(hypertension)1   0.618867   0.177328    3.49 0.000483 ***
    ## factor(heart_disease)1  0.423614   0.210797    2.01 0.044476 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1253.7  on 4084  degrees of freedom
    ## AIC: 1261.7
    ## 
    ## Number of Fisher Scoring iterations: 7

Age + Hypertension + heart_disease (AIC: 1261.7) \< Age + Hypertension
(AIC: 1263.5)

``` r
pchisq(1257.5-1253.7,4085-4084,lower.tail = FALSE)
```

    ## [1] 0.05125258

We move on with the reduced model

#### age + hypertension + ever_married

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension)+ factor(ever_married) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + factor(ever_married), 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9432  -0.3214  -0.1712  -0.0865   3.7377  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -7.078880   0.406328 -17.422  < 2e-16 ***
    ## age                      0.071593   0.005693  12.576  < 2e-16 ***
    ## factor(hypertension)1    0.628875   0.176994   3.553 0.000381 ***
    ## factor(ever_married)Yes -0.154229   0.243230  -0.634 0.526023    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1257.1  on 4084  degrees of freedom
    ## AIC: 1265.1
    ## 
    ## Number of Fisher Scoring iterations: 7

ever_married is NOT significant

#### age + hypertension + work_type

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension)+factor(work_type) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + factor(work_type), 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9926  -0.3186  -0.1658  -0.0899   3.4942  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value
    ## (Intercept)                     -6.204698   0.710545  -8.732
    ## age                              0.077333   0.006277  12.320
    ## factor(hypertension)1            0.647969   0.177913   3.642
    ## factor(work_type)Govt_job       -1.487846   0.821552  -1.811
    ## factor(work_type)Never_worked  -10.636259 333.192372  -0.032
    ## factor(work_type)Private        -1.236114   0.798678  -1.548
    ## factor(work_type)Self-employed  -1.710075   0.828648  -2.064
    ##                                Pr(>|z|)    
    ## (Intercept)                     < 2e-16 ***
    ## age                             < 2e-16 ***
    ## factor(hypertension)1           0.00027 ***
    ## factor(work_type)Govt_job       0.07014 .  
    ## factor(work_type)Never_worked   0.97453    
    ## factor(work_type)Private        0.12169    
    ## factor(work_type)Self-employed  0.03905 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1248.8  on 4081  degrees of freedom
    ## AIC: 1262.8
    ## 
    ## Number of Fisher Scoring iterations: 14

Age + Hypertension + work_type (AIC: 1262.8) \< Age + Hypertension (AIC:
1263.5)

``` r
pchisq(1257.5-1248.8,4085-4081,lower.tail = FALSE)
```

    ## [1] 0.06905145

We move on with the reduced model

#### age + hypertension + avg_glucose_level

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension) + avg_glucose_level , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + avg_glucose_level, 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0293  -0.3207  -0.1696  -0.0800   3.7959  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -7.622700   0.407637 -18.700  < 2e-16 ***
    ## age                    0.069313   0.005754  12.047  < 2e-16 ***
    ## factor(hypertension)1  0.545035   0.179047   3.044  0.00233 ** 
    ## avg_glucose_level      0.004652   0.001290   3.607  0.00031 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1245.0  on 4084  degrees of freedom
    ## AIC: 1253
    ## 
    ## Number of Fisher Scoring iterations: 7

Age + Hypertension + avg_glucose_level (AIC: 1253) \< Age + Hypertension
(AIC: 1263.5)

``` r
pchisq(1257.5-1245.0,4085-4084,lower.tail = FALSE)
```

    ## [1] 0.000406952

We move on with the full model

#### age + hypertension + avg_glucose_level + bmi

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + bmi , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + 
    ##     bmi, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0399  -0.3199  -0.1702  -0.0813   3.7870  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -7.414096   0.566711 -13.083  < 2e-16 ***
    ## age                    0.068824   0.005790  11.887  < 2e-16 ***
    ## factor(hypertension)1  0.554057   0.180064   3.077 0.002091 ** 
    ## avg_glucose_level      0.004803   0.001323   3.631 0.000282 ***
    ## bmi                   -0.006571   0.012614  -0.521 0.602393    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1244.7  on 4083  degrees of freedom
    ## AIC: 1254.7
    ## 
    ## Number of Fisher Scoring iterations: 7

bmi is NOT significant

#### age + hypertension + avg_glucose_level + smoking_status

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + factor(smoking_status) , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + 
    ##     factor(smoking_status), family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0656  -0.3211  -0.1685  -0.0777   3.8126  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value
    ## (Intercept)                        -7.572961   0.442880 -17.099
    ## age                                 0.069808   0.005848  11.937
    ## factor(hypertension)1               0.555004   0.180293   3.078
    ## avg_glucose_level                   0.004606   0.001291   3.568
    ## factor(smoking_status)never smoked -0.190077   0.194947  -0.975
    ## factor(smoking_status)smokes        0.099554   0.242638   0.410
    ## factor(smoking_status)Unknown      -0.110766   0.237235  -0.467
    ##                                    Pr(>|z|)    
    ## (Intercept)                         < 2e-16 ***
    ## age                                 < 2e-16 ***
    ## factor(hypertension)1               0.00208 ** 
    ## avg_glucose_level                   0.00036 ***
    ## factor(smoking_status)never smoked  0.32955    
    ## factor(smoking_status)smokes        0.68159    
    ## factor(smoking_status)Unknown       0.64057    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1243.0  on 4081  degrees of freedom
    ## AIC: 1257
    ## 
    ## Number of Fisher Scoring iterations: 7

Smoking status is NOT significant

### Comparing models with interaction

#### age + hypertension + avg_glucose_level

``` r
model <- glm(factor(stroke) ~ age + factor(hypertension) + avg_glucose_level , data = train, family = "binomial")
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + avg_glucose_level, 
    ##     family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0293  -0.3207  -0.1696  -0.0800   3.7959  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -7.622700   0.407637 -18.700  < 2e-16 ***
    ## age                    0.069313   0.005754  12.047  < 2e-16 ***
    ## factor(hypertension)1  0.545035   0.179047   3.044  0.00233 ** 
    ## avg_glucose_level      0.004652   0.001290   3.607  0.00031 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1245.0  on 4084  degrees of freedom
    ## AIC: 1253
    ## 
    ## Number of Fisher Scoring iterations: 7

#### age + hypertension + avg_glucose_level + interaction

All interactions:

- factor(smoking_status):bmi
- factor(work_type):bmi
- factor(work_type):avg_glucose_level
- factor(smoking_status):age
- factor(work_type):age

``` r
model_I <- glm(factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + factor(smoking_status):bmi, data = train, family = "binomial")
summary(model_I)
```

    ## 
    ## Call:
    ## glm(formula = factor(stroke) ~ age + factor(hypertension) + avg_glucose_level + 
    ##     factor(smoking_status):bmi, family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0749  -0.3196  -0.1680  -0.0791   3.8130  
    ## 
    ## Coefficients:
    ##                                             Estimate Std. Error
    ## (Intercept)                               -7.4353296  0.5746836
    ## age                                        0.0693748  0.0058954
    ## factor(hypertension)1                      0.5583706  0.1813063
    ## avg_glucose_level                          0.0047465  0.0013235
    ## factor(smoking_status)formerly smoked:bmi -0.0040521  0.0131402
    ## factor(smoking_status)never smoked:bmi    -0.0102162  0.0131398
    ## factor(smoking_status)smokes:bmi          -0.0007618  0.0138166
    ## factor(smoking_status)Unknown:bmi         -0.0092245  0.0138948
    ##                                           z value Pr(>|z|)    
    ## (Intercept)                               -12.938  < 2e-16 ***
    ## age                                        11.768  < 2e-16 ***
    ## factor(hypertension)1                       3.080 0.002072 ** 
    ## avg_glucose_level                           3.586 0.000335 ***
    ## factor(smoking_status)formerly smoked:bmi  -0.308 0.757794    
    ## factor(smoking_status)never smoked:bmi     -0.778 0.436861    
    ## factor(smoking_status)smokes:bmi           -0.055 0.956031    
    ## factor(smoking_status)Unknown:bmi          -0.664 0.506767    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1555.2  on 4087  degrees of freedom
    ## Residual deviance: 1242.6  on 4080  degrees of freedom
    ## AIC: 1258.6
    ## 
    ## Number of Fisher Scoring iterations: 7

The interaction is NOT significant  

Age + Hypertension + avg_glucose_level (AIC: 1253) \< age +
hypertension + avg_glucose_level + interaction (AIC: 1258.6)

``` r
pchisq(1245-1242.6,4084-4080,lower.tail = FALSE)
```

    ## [1] 0.6626273

Reduced model is better: age + hypertension + avg_glucose_level

### Classification Report

``` r
table(df$stroke)
```

    ## 
    ##    0    1 
    ## 4861  249

``` r
#probability of stroke = class 1
249 / (4861 + 249)
```

    ## [1] 0.04872798

``` r
model <- glm(stroke ~ age + hypertension + avg_glucose_level, data = train, family = binomial)
pred <- predict(model, newdata = test, type = "response")
pred_class <- ifelse(pred > 0.04872798, 1, 0)
classification_table <- table(actual = test$stroke, predicted = pred_class)
classification_table
```

    ##       predicted
    ## actual   0   1
    ##      0 731 235
    ##      1  17  39

``` r
TP <- classification_table[2,2]
FP <- classification_table[1,2]
TN <- classification_table[1,1]
FN <- classification_table[2,1]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN)/ (TP + FN + TN + FP)

c(sensitivity,specificity,accuracy)
```

    ## [1] 0.6964286 0.7567288 0.7534247

### ROC Curve

``` r
#Without interaction

library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
test_roc <- roc(test$stroke ~ pred, plot = TRUE, print.auc = TRUE)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](StrokePrediction_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

``` r
spec_rev <- rev(1 - test_roc$specificities)

# Plot the ROC curve with 1 - specificity on the x-axis
plot(1 - spec_rev, test_roc$sensitivities, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(a = 0, b = 1, lty = 2)
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-57-2.png)<!-- -->

``` r
#With interaction
pred <- predict(model_I, newdata = test, type = "response")
test_roc <- roc(test$stroke ~ pred, plot = TRUE, print.auc = TRUE)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](StrokePrediction_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
spec_rev <- rev(1 - test_roc$specificities)

# Plot the ROC curve with 1 - specificity on the x-axis
plot(1 - spec_rev, test_roc$sensitivities, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(a = 0, b = 1, lty = 2)
```

![](StrokePrediction_files/figure-gfm/unnamed-chunk-58-2.png)<!-- -->

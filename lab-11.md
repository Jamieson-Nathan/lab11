Lab 11 - Grading the professor, Pt. 2
================
Jamieson Nathan
04/20/2025

## Load packages and data

``` r
library(tidyverse) 
library(broom)
library(openintro)

data(evals)
```

## Exercise 1

``` r
m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

y^ =3.880 + 0.06 x ⋅bty_avg

R^2 = .035, Adjusted R^2 = .032

## Exercise 2

``` r
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

#### Q3

Intercept (3.747): Average evaluation score for a female professor with
a beauty rating of 0 (not meaningful, just a baseline).

Slope of bty_avg (0.074): For each 1-point increase in beauty,
evaluation score increases by 0.074, controlling for gender.

gendermale (0.172): Male professors score 0.172 points higher than
female professors, controlling for beauty.

#### Q4

R^2 =5.91%, meaning the model explains 5.91% of the variability in
evaluation scores.

#### Q5

Since males =1; y^male = 3.919 + 0.074 X bty_avg

#### Q6

Male professors score 0.172 points higher than female professors with
the same beauty rating.

#### Q7

No it doesn’t — the relationship between beauty and score is assumed to
be the same for both genders (no interaction term included).

The slope of bty_avg is identical regardless of gender.

#### Q8

Adding gender improves the model and it increases the explained variance
by about 2.4 percentage points, suggesting gender adds meaningful
information beyond beauty alone.

#### Q9

The slope increased slightly when gender was added. This means
controlling for gender slightly strengthened the observed relationship
between beauty and evaluation score.

``` r
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

y^ = 3.949 + 0.072 X bty_avg − 0.101 X rank(tenure track) - 0.13 X
rank(tenured)

Intercept (3.949): Average score for a teaching faculty member with a
beauty rating of 0 (not meaningful, just baseline).

bty_avg (0.072): Each 1-point increase in beauty rating is associated
with a 0.072-point increase in evaluation score, controlling for rank.

ranktenure track (-0.101): Tenure-track professors score 0.101 points
lower than teaching faculty, controlling for beauty.

ranktenured (-0.130): Tenured professors score 0.130 points lower than
teaching faculty, controlling for beauty.

## Exercise 3

``` r
m_credits <- lm(score ~ cls_credits, data = evals)
summary(m_credits)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_credits, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84702 -0.34702  0.05298  0.35298  0.85298 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.14702    0.02552 162.494  < 2e-16 ***
    ## cls_creditsone credit  0.47520    0.10568   4.496 8.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5329 on 461 degrees of freedom
    ## Multiple R-squared:  0.04202,    Adjusted R-squared:  0.03994 
    ## F-statistic: 20.22 on 1 and 461 DF,  p-value: 8.751e-06

#### 11

cls_credits refers to the number of credits the course is worth
(typically 1–4). This is a course-level administrative detail, not
something students would usually consider when rating a professor’s
teaching quality. It’s unlikely that a 3-credit course leads to higher
or lower evaluations than a 4-credit course purely because of credit
value.Maybe if more credits = more important = some sort of effect?

#### 12

After running this model, it appears that credits do have an impact of
almost 4%, more than expected for sure.

#### 13

I would not not include cls_did_eval, because it’s fully determined by
cls_perc_eval and cls_students, leading to redundancy and
multicollinearity in the model.

``` r
m_full <- lm(score ~ rank + ethnicity + gender + language + age +
               cls_perc_eval + cls_students + cls_level + cls_profs +
               cls_credits + bty_avg,
             data = evals)

summary(m_full)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
m_full <- lm(score ~ rank + ethnicity + gender + language + age +
               cls_perc_eval + cls_students + cls_level + cls_profs +
               cls_credits + bty_avg,
             data = evals)

m_final <- lm(score ~ ethnicity + gender + cls_perc_eval + cls_credits + bty_avg, data = evals)
summary(m_final)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + cls_perc_eval + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8857 -0.3294  0.1066  0.3774  1.0540 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.137381   0.146450  21.423  < 2e-16 ***
    ## ethnicitynot minority 0.233794   0.071275   3.280 0.001117 ** 
    ## gendermale            0.157832   0.048493   3.255 0.001219 ** 
    ## cls_perc_eval         0.005208   0.001443   3.608 0.000343 ***
    ## cls_creditsone credit 0.541067   0.104669   5.169 3.52e-07 ***
    ## bty_avg               0.073644   0.015773   4.669 3.98e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5053 on 457 degrees of freedom
    ## Multiple R-squared:  0.146,  Adjusted R-squared:  0.1366 
    ## F-statistic: 15.62 on 5 and 457 DF,  p-value: 3.338e-14

y^ = 3.137 + 0.234ethnicity (not minority) + 0.158gender (male) +
0.00521cls_perc_eval + 0.541cls_credits(one credit) + 0.074bty_avg

#### 16

Numerical predictor (cls_perc_eval): For each 1% increase in the
percentage of students who completed evaluations, the professor’s
average evaluation score increases by 0.005 points, holding all other
variables constant.

Categorical predictor (gender): Male professors score, on average, 0.158
points higher than female professors, after accounting for beauty,
ethnicity, course credits, and percent of evaluations completed.

#### 17

Based on the model, a professor likely to receive a high evaluation
score would have the following characteristics: Male, not a minority,
high beauty rating teaches a one-credit course, and has a high
percentage of students completing evaluations.

#### 18

No, we shouldn’t generalize too broadly, as this dataset is from one
institution and one time period, and includes subjective ratings
potentially influenced by contextual or cultural norms specific to that
university.

Legal Analytics Problem Set 3 - Part 1
-----------------------------------------------------------------------------
## Andy Ninh - 2014
-----------------------------------------------------------------------------

# Binary Classification – A Comparison of “Titanic” Proportions Between Logistic Regression, Random Forests, and Conditional Trees

The dataset I used contains records of the survival of Titanic Passengers and such information as sex, age, fare each person paid, number of parents/children aboard, number of siblings or spouses aboard, passenger class and other fields (The titanic dataset can be retrieved from a <a href="http://biostat.mc.vanderbilt.edu/twiki/bin/view/Main/DataSets">page on Vanderbilt’s website </a> replete with lots of datasets; look for “titanic3″).

I took one part of the dataset to train my models, and another part to test them.  The factors that I focused on were passenger class, sex, age, and number of siblings/spouses aboard.

First, let’s look at the GLM:

```r
titanic.train <- read.csv("titanic3.csv", header = TRUE)
titanic.survival.train = glm(survived ~ pclass + sex + pclass:sex + age + sibsp, 
    family = binomial(logit), data = titanic.train)
```


As you can see, I worked in an interaction effect between passenger class and sex, as passenger class showed a much bigger difference in survival rate amongst the women compared to the men (i.e. Higher class women were much more likely to survive than lower class women, whereas first class Men were more likely to survive than 2nd or 3rd class men, but not by the same margin as amongst the women).  Following is the model summary output, if you’re interested:

```r
summary(titanic.survival.train)
```

```
## 
## Call:
## glm(formula = survived ~ pclass + sex + pclass:sex + age + sibsp, 
##     family = binomial(logit), data = titanic.train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -3.199  -0.676  -0.486   0.486   2.371  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     7.71965    0.74310   10.39  < 2e-16 ***
## pclass         -2.21013    0.24355   -9.07  < 2e-16 ***
## sexmale        -6.01608    0.68767   -8.75  < 2e-16 ***
## age            -0.04221    0.00699   -6.04  1.6e-09 ***
## sibsp          -0.31497    0.09978   -3.16   0.0016 ** 
## pclass:sexmale  1.44918    0.26158    5.54  3.0e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1414.62  on 1045  degrees of freedom
## Residual deviance:  933.32  on 1040  degrees of freedom
##   (263 observations deleted due to missingness)
## AIC: 945.3
## 
## Number of Fisher Scoring iterations: 5
```


# Predicting Trees
-----------------------------------------------------------------------------
Andy Ninh, 2014 -
replicating Jeff Leek 
#### Original Video: https://www.youtube.com/watch?v=kdUVUINpFmI
-----------------------------------------------------------------------------

## Example: Iris Data  

```r
data(iris)
names(iris)
```

```
## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
## [5] "Species"
```



```r
table(iris$Species)
```

```
## 
##     setosa versicolor  virginica 
##         50         50         50
```


### Iris petal widths/sepal width

```r
plot(iris$Petal.Width, iris$Sepal.Width, pch = 19, col = as.numeric(iris$Species))
legend(1, 4.5, legend = unique(iris$Species), col = unique(as.numeric(iris$Species)), 
    pch = 19)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


### An alternative is library(rpart)

```r
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)
```

```
## 
## Classification tree:
## tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
## Number of terminal nodes:  5 
## Residual mean deviance:  0.204 = 29.6 / 145 
## Misclassification error rate: 0.0333 = 5 / 150
```


### Plot tree

```r
plot(tree1)
text(tree1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


### Another way of looking at a CART model

```r
plot(iris$Petal.Width, iris$Sepal.Width, pch = 19, col = as.numeric(iris$Species))
partition.tree(tree1, label = "Species", add = TRUE)
legend(1.75, 4.5, legend = unique(iris$Species), col = unique(as.numeric(iris$Species)), 
    pch = 19)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


### Predicting new values

```r
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20, 0, 2.5), Sepal.Width = runif(20, 
    2, 4.5))
pred1 <- predict(tree1, newdata)
pred1
```

```
##    setosa versicolor virginica
## 1       0    0.02174   0.97826
## 2       0    0.02174   0.97826
## 3       1    0.00000   0.00000
## 4       0    1.00000   0.00000
## 5       0    0.02174   0.97826
## 6       0    0.02174   0.97826
## 7       0    0.02174   0.97826
## 8       0    0.90476   0.09524
## 9       0    1.00000   0.00000
## 10      0    0.02174   0.97826
## 11      0    1.00000   0.00000
## 12      1    0.00000   0.00000
## 13      1    0.00000   0.00000
## 14      1    0.00000   0.00000
## 15      0    0.02174   0.97826
## 16      0    0.02174   0.97826
## 17      0    1.00000   0.00000
## 18      1    0.00000   0.00000
## 19      0    1.00000   0.00000
## 20      0    1.00000   0.00000
```


### Overlaying new values

```r
pred1 <- predict(tree1, newdata, type = "class")
plot(newdata$Petal.Width, newdata$Sepal.Width, col = as.numeric(pred1), pch = 19)
partition.tree(tree1, "Species", add = TRUE)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


## Pruning trees example: Cars

```r
data(Cars93, package = "MASS")
head(Cars93)
```

```
##   Manufacturer   Model    Type Min.Price Price Max.Price MPG.city
## 1        Acura Integra   Small      12.9  15.9      18.8       25
## 2        Acura  Legend Midsize      29.2  33.9      38.7       18
## 3         Audi      90 Compact      25.9  29.1      32.3       20
## 4         Audi     100 Midsize      30.8  37.7      44.6       19
## 5          BMW    535i Midsize      23.7  30.0      36.2       22
## 6        Buick Century Midsize      14.2  15.7      17.3       22
##   MPG.highway            AirBags DriveTrain Cylinders EngineSize
## 1          31               None      Front         4        1.8
## 2          25 Driver & Passenger      Front         6        3.2
## 3          26        Driver only      Front         6        2.8
## 4          26 Driver & Passenger      Front         6        2.8
## 5          30        Driver only       Rear         4        3.5
## 6          31        Driver only      Front         4        2.2
##   Horsepower  RPM Rev.per.mile Man.trans.avail Fuel.tank.capacity
## 1        140 6300         2890             Yes               13.2
## 2        200 5500         2335             Yes               18.0
## 3        172 5500         2280             Yes               16.9
## 4        172 5500         2535             Yes               21.1
## 5        208 5700         2545             Yes               21.1
## 6        110 5200         2565              No               16.4
##   Passengers Length Wheelbase Width Turn.circle Rear.seat.room
## 1          5    177       102    68          37           26.5
## 2          5    195       115    71          38           30.0
## 3          5    180       102    67          37           28.0
## 4          6    193       106    70          37           31.0
## 5          4    186       109    69          39           27.0
## 6          6    189       105    69          41           28.0
##   Luggage.room Weight  Origin          Make
## 1           11   2705 non-USA Acura Integra
## 2           15   3560 non-USA  Acura Legend
## 3           14   3375 non-USA       Audi 90
## 4           17   3405 non-USA      Audi 100
## 5           13   3640 non-USA      BMW 535i
## 6           16   2880     USA Buick Century
```


### Build a tree

```r
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + 
    Width + Length + Weight + Price + Cylinders + Horsepower + Wheelbase, data = Cars93)
plot(treeCars)
text(treeCars)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


### Plot errors

```r
par(mfrow = c(1, 2))
plot(cv.tree(treeCars, FUN = prune.tree, method = "misclass"))
plot(cv.tree(treeCars))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


### Prune the tree

```r
pruneTree <- prune.tree(treeCars, best = 4)
plot(pruneTree)
text(pruneTree)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


### Show resubstitution error

```r
table(Cars93$DriveTrain, predict(pruneTree, type = "class"))
```

```
##        
##         4WD Front Rear
##   4WD     5     5    0
##   Front   1    66    0
##   Rear    1    10    5
```



```r
table(Cars93$DriveTrain, predict(treeCars, type = "class"))
```

```
##        
##         4WD Front Rear
##   4WD     5     5    0
##   Front   2    61    4
##   Rear    0     3   13
```


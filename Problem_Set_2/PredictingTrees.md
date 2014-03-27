# Predicting Trees
-----------------------------------------------------------------------------
Andy Ninh, 2014 -
replicating Jeff Leek 
#### Original Video: https://www.youtube.com/watch?v=kdUVUINpFmI
-----------------------------------------------------------------------------

## Example: Iris Data  
```{r}
data(iris)
names(iris)
```

```{r}
table(iris$Species)
```

### Iris petal widths/sepal width
```{r}
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)
```

### An alternative is library(rpart)
```{r}
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)
```

### Plot tree
```{r}
plot(tree1)
text(tree1)
```

### Another way of looking at a CART model
```{r}
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species), col=unique(as.numeric(iris$Species)),pch=19)
```

### Predicting new values
```{r}
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5), Sepal.Width = runif(20,2,4.5))
pred1 <- predict(tree1,newdata)
pred1
```

### Overlaying new values
```{r}
pred1 <- predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)
```

## Pruning trees example: Cars
```{r}
data(Cars93,package="MASS")
head(Cars93)
```

### Build a tree
```{r}
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + 
                   EngineSize + Width + Length + Weight + Price + Cylinders +
                   Horsepower + Wheelbase, data=Cars93)
plot(treeCars)
text(treeCars)
```

### Plot errors
```{r}
par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))
```

### Prune the tree
```{r}
pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)
```

### Show resubstitution error
```{r}
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
```

```{r}
table(Cars93$DriveTrain,predict(treeCars,type="class"))
```

Legal Analytics Problem Set 3 Part 3
========================================================
## Andy Ninh, 2014
========================================================

# Hierarchial Clustering

For the hierarchial clustering methods, the dendogram is the main graphical tool for getting insight into a cluster solution. To illustrate interpretation of the dendogram, we'll look at a cluster analysis performed on a set of cars from 1978-1979.

```r
library(foreign)
cars = read.delim("http://www.stat.berkeley.edu/classes/s133/data/cars.tab", 
    stringsAsFactors = FALSE)
head(cars)
```

```
##   Country                       Car  MPG Weight Drive_Ratio Horsepower
## 1    U.S.        Buick Estate Wagon 16.9  4.360        2.73        155
## 2    U.S. Ford Country Squire Wagon 15.5  4.054        2.26        142
## 3    U.S.        Chevy Malibu Wagon 19.2  3.605        2.56        125
## 4    U.S.    Chrysler LeBaron Wagon 18.5  3.940        2.45        150
## 5    U.S.                  Chevette 30.0  2.155        3.70         68
## 6   Japan             Toyota Corona 27.5  2.560        3.05         95
##   Displacement Cylinders
## 1          350         8
## 2          351         8
## 3          267         8
## 4          360         8
## 5           98         4
## 6          134         4
```


It looks like the variables are measured on different scales, so we will likely want to standardize the data before proceeding. 


```r
cars.use = cars[, -c(1, 2)]
medians = apply(cars.use, 2, median)
mads = apply(cars.use, 2, mad)
cars.use = scale(cars.use, center = medians, scale = mads)
cars.use
```

```
##           MPG   Weight Drive_Ratio Horsepower Displacement Cylinders
##  [1,] -0.8403  2.09217    -0.49182    1.57860      2.69128    4.7214
##  [2,] -1.0003  1.70996    -1.15226    1.20547      2.70464    4.7214
##  [3,] -0.5773  1.14913    -0.73070    0.71754      1.58272    4.7214
##  [4,] -0.6573  1.56757    -0.88527    1.43509      2.82485    4.7214
##  [5,]  0.6573 -0.66200     0.87122   -0.91846     -0.67449   -0.6745
##  [6,]  0.3715 -0.15613    -0.04216   -0.14351     -0.19367   -0.6745
##  [7,]  0.3372 -0.48089     0.64639   -0.08611     -0.39401   -0.6745
##  [8,]  0.7602 -0.56832     0.40750   -0.71754     -0.58100   -0.6745
##  [9,] -0.4516  0.18111     1.15226    0.08611     -0.23373    0.6745
## [10,] -0.8288  0.56832     0.59018    0.71754      0.19367    2.0235
## [11,] -0.3029  0.13740     0.96958    0.43053     -0.36730   -0.6745
## [12,] -0.9203  0.90557     0.70259    0.94716      0.19367    2.0235
## [13,] -0.4173  0.86809    -0.49182    0.14351      1.10189    2.0235
## [14,] -0.3944  0.48089     0.00000   -0.43053      0.68785    2.0235
## [15,] -0.6459  1.16787    -0.51992    0.28702      1.02175    2.0235
## [16,] -0.7031  0.90557    -0.49182    0.57403      1.46251    2.0235
## [17,] -0.8288  1.44266    -0.94148    0.86105      2.09025    4.7214
## [18,] -0.7602  1.29902    -1.15226    0.83235      2.05018    4.7214
## [19,] -0.8860  1.58630    -1.15226    1.09067      2.70464    4.7214
## [20,] -0.6916  1.43017    -0.88527    1.00456      2.26388    4.7214
## [21,]  0.2572 -0.12491     0.00000   -0.34442     -0.11353   -0.6745
## [22,] -0.2687  0.28104     0.00000    0.25832      0.30052    2.0235
## [23,]  1.1261 -0.88683     0.91337   -1.00456     -0.83477   -0.6745
## [24,]  1.2404 -0.96177    -0.15457   -0.57403     -0.67449   -0.6745
## [25,]  0.3601 -0.01874     0.00000   -0.57403     -0.36730   -0.6745
## [26,]  0.8288 -0.86809     0.98363   -0.83235     -0.79470   -0.6745
## [27,]  0.6002 -0.68698    -0.04216   -0.91846     -0.67449   -0.6745
## [28,]  0.4744 -0.01874    -0.77285   -0.28702      0.03339   -0.6745
## [29,]  0.5202 -0.11242    -0.54802    0.43053      0.32723    2.0235
## [30,]  0.2915  0.01874    -0.33725    0.43053      0.32723    2.0235
## [31,]  1.0575 -0.16113    -0.54802   -0.28702      0.03339   -0.6745
## [32,]  1.1375 -0.60579     0.40750   -0.86105     -0.58100   -0.6745
## [33,]  0.8631 -0.83062     0.87122   -1.00456     -0.84812   -0.6745
## [34,]  1.4919 -0.69323     0.02810   -0.88975     -0.76798   -0.6745
## [35,]  0.7145 -0.61828     0.87122   -0.63144     -0.68785   -0.6745
## [36,] -0.2572  0.16238     0.87122   -0.08611     -0.03339    2.0235
## [37,] -0.3144 -0.10617     0.78691    0.28702     -0.36730   -0.6745
## [38,]  0.8746 -0.94928     0.98363   -0.83235     -0.79470   -0.6745
## attr(,"scaled:center")
##          MPG       Weight  Drive_Ratio   Horsepower Displacement 
##       24.250        2.685        3.080      100.000      148.500 
##    Cylinders 
##        4.500 
## attr(,"scaled:scale")
##          MPG       Weight  Drive_Ratio   Horsepower Displacement 
##       8.7473       0.8006       0.7116      34.8411      74.8713 
##    Cylinders 
##       0.7413
```


(The 2 used as the second argument to apply means to apply the function to the columns of the matrix or data frame; a value of 1 means to use the rows.) The country of origin and name of the car will not be useful in the cluster analysis, so they have been removed. Notice that the scale function doesn't change the order of the rows of the data frame, so it will be easy to identify observations using the omitted columns from the original data.

First, we'll take a look at a hierarchical method, since it will provide information about solutions with different numbers of clusters. The first step is calculating a distance matrix. For a data set with n observations, the distance matrix will have n rows and n columns; the (i,j)th element of the distance matrix will be the difference between observation i and observation j. There are two functions that can be used to calculate distance matrices in R; the dist function, which is included in every version of R, and the daisy function, which is part of the cluster library. We'll use the dist function in this example, but you should familiarize yourself with the daisy function (by reading its help page), since it offers some capabilities that dist does not. Each function provides a choice of distance metrics; in this example, we'll use the default of Euclidean distance, but you may find that using other metrics will give different insights into the structure of your data.


```r
cars.dist = dist(cars.use)
cars.dist
```

```
##          1       2       3       4       5       6       7       8       9
## 2  0.86445                                                                
## 3  1.72796 1.47239                                                        
## 4  0.70845 0.52569 1.50437                                                
## 5  7.63975 7.58380 6.65268 7.52895                                        
## 6  6.86514 6.77735 6.00759 6.74240 1.41548                                
## 7  7.12102 7.08589 6.24973 7.02299 0.97841 0.79004                        
## 8  7.45283 7.37105 6.47235 7.32068 0.53245 1.00136 0.82299                
## 9  5.80235 5.84928 4.95725 5.80246 2.24559 2.02285 1.78647 2.27397        
## 10 4.21325 4.26508 3.36954 4.23342 3.80927 3.24592 3.27627 3.71335 1.73474
## 11 6.78487 6.82750 6.08200 6.77690 1.86681 1.38722 1.07898 1.81880 1.41901
## 12 4.09352 4.21244 3.39005 4.17958 4.06052 3.45966 3.49268 3.96659 1.92108
## 13 3.67994 3.52993 2.82867 3.55211 4.11292 3.30353 3.64060 3.88730 2.60443
## 14 4.28558 4.15071 3.22673 4.16653 3.54181 3.02203 3.23206 3.40168 2.08729
## 15 3.55351 3.43132 2.79795 3.48448 4.31179 3.45804 3.80367 4.08762 2.69147
## 16 3.35036 3.22369 2.72878 3.23597 4.49879 3.60846 3.93211 4.27147 2.86812
## 17 1.22487 0.80086 0.68704 0.95776 7.06050 6.33618 6.61557 6.86531 5.33824
## 18 1.42813 0.89109 0.68216 1.05706 7.03951 6.30348 6.59858 6.83176 5.36719
## 19 0.96562 0.20382 1.36457 0.50694 7.49015 6.69994 7.00765 7.27780 5.78749
## 20 1.06175 0.69244 0.81383 0.72117 7.12252 6.38993 6.66533 6.92446 5.40636
## 21 6.85877 6.76284 5.99788 6.73805 1.36080 0.25022 0.83447 0.98663 1.98577
## 22 4.31123 4.22620 3.24295 4.19833 3.48141 2.87879 3.04063 3.30975 1.87242
## 23 7.93329 7.87777 6.90474 7.80387 0.55238 1.78008 1.37700 0.79872 2.65650
## 24 7.67122 7.54025 6.63107 7.47869 1.26517 1.35393 1.41604 0.85476 2.88556
## 25 7.01737 6.91999 6.11256 6.90490 1.21415 0.48609 0.93312 0.83334 2.07360
## 26 7.79786 7.75037 6.79379 7.68065 0.32616 1.61369 1.10578 0.69666 2.39722
## 27 7.52864 7.38815 6.48995 7.36721 0.91550 1.07973 1.16498 0.53944 2.51188
## 28 6.77674 6.61712 5.91586 6.60937 2.01247 0.79720 1.57152 1.52964 2.57561
## 29 4.57164 4.41923 3.43030 4.34152 3.52666 2.85653 3.10837 3.25583 2.48484
## 30 4.44857 4.32723 3.33543 4.25744 3.48709 2.82912 3.04719 3.24668 2.24727
## 31 6.95290 6.82492 6.07851 6.77618 1.82347 0.89363 1.50686 1.31530 2.70473
## 32 7.60099 7.51982 6.60344 7.45682 0.67880 1.28684 1.16128 0.40537 2.55664
## 33 7.84972 7.78741 6.82283 7.72703 0.32915 1.64334 1.22441 0.66944 2.49354
## 34 7.78736 7.67754 6.74365 7.61538 1.19073 1.56054 1.59570 0.87148 2.97702
## 35 7.55252 7.51365 6.59166 7.44787 0.29620 1.28365 0.77167 0.48831 2.14909
## 36 4.83694 4.84620 3.76549 4.79400 3.14624 2.93922 2.86824 3.12438 1.41656
## 37 6.84715 6.85022 6.08703 6.80728 1.67566 1.17296 0.85130 1.60223 1.45354
## 38 7.83892 7.78863 6.82792 7.71573 0.40523 1.66436 1.15726 0.74093 2.45821
##         10      11      12      13      14      15      16      17      18
## 2                                                                         
## 3                                                                         
## 4                                                                         
## 5                                                                         
## 6                                                                         
## 7                                                                         
## 8                                                                         
## 9                                                                         
## 10                                                                        
## 11 2.87789                                                                
## 12 0.43296 2.98380                                                        
## 13 1.60759 3.49322 1.77532                                                
## 14 1.45153 3.19377 1.75862 0.94514                                        
## 15 1.57997 3.55347 1.66137 0.41225 1.19661                                
## 16 1.71196 3.67881 1.79527 0.63126 1.45834 0.59119                        
## 17 3.74260 6.40104 3.72603 3.07773 3.59361 3.00614 2.87436                
## 18 3.78333 6.41735 3.78942 3.06468 3.58618 3.01068 2.87843 0.26869        
## 19 4.21887 6.77717 4.18445 3.45204 4.05608 3.37484 3.16573 0.70606 0.77032
## 20 3.81913 6.45224 3.79696 3.14907 3.68722 3.08887 2.92176 0.26996 0.41050
## 21 3.24184 1.40983 3.46173 3.26760 2.95300 3.42401 3.58223 6.32052 6.29070
## 22 0.98333 2.95238 1.33948 1.12427 0.82479 1.31153 1.50683 3.65992 3.64355
## 23 4.16483 2.31781 4.42611 4.44791 3.86569 4.67010 4.85607 7.33763 7.30863
## 24 4.10832 2.44075 4.38598 4.14238 3.73273 4.37317 4.53228 7.03572 6.97329
## 25 3.37170 1.55346 3.59097 3.40368 3.03839 3.55532 3.76455 6.45373 6.42320
## 26 3.95271 2.01734 4.20794 4.30957 3.73122 4.51798 4.69270 7.21803 7.19473
## 27 3.83756 2.10546 4.11289 3.90102 3.42461 4.10255 4.30235 6.87842 6.82879
## 28 3.49497 2.08331 3.71542 3.20416 3.05446 3.36341 3.51889 6.20704 6.15153
## 29 1.91804 3.28701 2.22709 1.58928 1.53630 1.87152 1.96092 3.86852 3.79548
## 30 1.58668 3.13634 1.90288 1.38930 1.29200 1.65397 1.76324 3.77100 3.71734
## 31 3.70129 2.21774 3.92953 3.44142 3.24782 3.64059 3.78197 6.39920 6.34331
## 32 3.95593 2.15800 4.21360 4.06431 3.57323 4.28397 4.47158 7.00711 6.96916
## 33 4.02848 2.14409 4.28915 4.33388 3.74542 4.54423 4.73658 7.24893 7.22188
## 34 4.25209 2.58871 4.51748 4.26567 3.81402 4.49457 4.69479 7.15812 7.10338
## 35 3.70738 1.68714 3.94602 4.05349 3.52261 4.24519 4.42105 6.99833 6.97860
## 36 1.12601 2.76944 1.46274 1.92953 1.23211 2.08564 2.29836 4.23529 4.25479
## 37 2.92187 0.33678 3.07039 3.47175 3.14284 3.55659 3.67687 6.41373 6.41805
## 38 4.00218 2.08449 4.26185 4.35626 3.77645 4.57015 4.73901 7.25503 7.22980
##         19      20      21      22      23      24      25      26      27
## 2                                                                         
## 3                                                                         
## 4                                                                         
## 5                                                                         
## 6                                                                         
## 7                                                                         
## 8                                                                         
## 9                                                                         
## 10                                                                        
## 11                                                                        
## 12                                                                        
## 13                                                                        
## 14                                                                        
## 15                                                                        
## 16                                                                        
## 17                                                                        
## 18                                                                        
## 19                                                                        
## 20 0.57889                                                                
## 21 6.68434 6.37888                                                        
## 22 4.14296 3.73816 2.87316                                                
## 23 7.77592 7.39336 1.76794 3.78234                                        
## 24 7.43742 7.08732 1.43466 3.57325 1.17057                                
## 25 6.83964 6.52076 0.37280 2.98379 1.60574 1.33508                        
## 26 7.65415 7.27542 1.59645 3.62775 0.35341 1.24693 1.46902                
## 27 7.29246 6.95054 1.03900 3.36276 1.12366 0.78525 0.84791 1.07665        
## 28 6.53609 6.26346 0.82501 2.98112 2.30003 1.56261 0.92374 2.21706 1.37699
## 29 4.31217 3.91419 2.90576 1.05249 3.71365 3.26902 3.01749 3.62559 3.27026
## 30 4.22787 3.82338 2.86520 0.72574 3.71070 3.34465 2.98167 3.59895 3.28368
## 31 6.73389 6.43930 0.98332 3.14677 1.98383 1.18860 1.02462 1.97007 1.28115
## 32 7.41979 7.06059 1.28749 3.49840 0.64808 0.73781 1.11493 0.73654 0.71381
## 33 7.68918 7.31101 1.61232 3.67001 0.27249 1.19468 1.44490 0.21847 0.98060
## 34 7.57238 7.21162 1.60433 3.71325 0.98623 0.52646 1.41311 1.17782 0.89982
## 35 7.42340 7.05435 1.27427 3.40322 0.63567 1.20426 1.16195 0.37402 0.96675
## 36 4.76214 4.31593 2.90833 1.00167 3.43232 3.53367 2.96673 3.26642 3.26705
## 37 6.79339 6.46938 1.18719 2.91497 2.14173 2.20714 1.35026 1.83352 1.84625
## 38 7.69091 7.31114 1.65236 3.66827 0.32149 1.22909 1.53216 0.09318 1.10371
##         28      29      30      31      32      33      34      35      36
## 2                                                                         
## 3                                                                         
## 4                                                                         
## 5                                                                         
## 6                                                                         
## 7                                                                         
## 8                                                                         
## 9                                                                         
## 10                                                                        
## 11                                                                        
## 12                                                                        
## 13                                                                        
## 14                                                                        
## 15                                                                        
## 16                                                                        
## 17                                                                        
## 18                                                                        
## 19                                                                        
## 20                                                                        
## 21                                                                        
## 22                                                                        
## 23                                                                        
## 24                                                                        
## 25                                                                        
## 26                                                                        
## 27                                                                        
## 28                                                                        
## 29 2.81809                                                                
## 30 2.84690 0.33750                                                        
## 31 0.64090 2.85855 2.92296                                                
## 32 1.69839 3.36298 3.38118 1.35061                                        
## 33 2.19206 3.65618 3.63709 1.94735 0.65788                                
## 34 1.77121 3.44022 3.50755 1.34509 0.55944 1.06989                        
## 35 1.93878 3.42711 3.38855 1.72617 0.67694 0.48175 1.18065                
## 36 3.25498 1.75813 1.47617 3.34232 3.30602 3.32078 3.60094 3.09166        
## 37 1.88488 3.20320 3.07057 2.03889 1.96602 1.95392 2.39340 1.50807 2.75902
## 38 2.25707 3.64753 3.62688 1.99593 0.75202 0.24364 1.16778 0.44676 3.30829
##         37
## 2         
## 3         
## 4         
## 5         
## 6         
## 7         
## 8         
## 9         
## 10        
## 11        
## 12        
## 13        
## 14        
## 15        
## 16        
## 17        
## 18        
## 19        
## 20        
## 21        
## 22        
## 23        
## 24        
## 25        
## 26        
## 27        
## 28        
## 29        
## 30        
## 31        
## 32        
## 33        
## 34        
## 35        
## 36        
## 37        
## 38 1.89704
```


When we display the distance matrix in R (for example, by typing its name), you'll notice that only the lower triangle of the matrix is displayed. This is to remind us that the distance matrix is symmetric, since it doesn't matter which observation we consider first when we calculate a distance. R takes advantage of this fact by only storing the lower triangle of the distance matrix. All of the clustering functions will recognize this and have no problems, but if you try to access the distance matrix in the usual way (for example, with subscripting), you'll see an error message. Thus, if you need to use the distance matrix with anything other than the clustering functions, you'll need to use as.matrix to convert it to a regular matrix.

To get started, we'll use the hclust method; the cluster library provides a similar function, called agnes to perform hierarchical cluster analysis.


```r
cars.hclust = hclust(cars.dist)
cars.hclust
```

```
## 
## Call:
## hclust(d = cars.dist)
## 
## Cluster method   : complete 
## Distance         : euclidean 
## Number of objects: 38
```


Once again, we're using the default method of hclust, which is to update the distance matrix using what R calls "complete" linkage. Using this method, when a cluster is formed, its distance to other objects is computed as the maximum distance between any object in the cluster and the other object. Other linkage methods will provide different solutions, and should not be ignored. For example, using method=ward tends to produce clusters of fairly equal size, and can be useful when other methods find clusters that contain just a few observations.

Now that we've got a cluster solution (actually a collection of cluster solutions), how can we examine the results? The main graphical tool for looking at a hierarchical cluster solution is known as a dendogram. This is a tree-like display that lists the objects which are clustered along the x-axis, and the distance at which the cluster was formed along the y-axis. (Distances along the x-axis are meaningless in a dendogram; the observations are equally spaced to make the dendogram easier to read.) To create a dendogram from a cluster solution, simply pass it to the plot function. The result is displayed below.


```r
plot(cars.hclust, labels = cars$Car, main = "Default from hclust")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


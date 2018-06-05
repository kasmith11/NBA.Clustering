NBA Clustering
================

The clustering analysis uses the ballr package to access the per game statistics of players in the 2017 season from <https://www.basketball-reference.com/>

``` r
library(ballr)
NBA <- NBAPerGameStatistics(season = 2017)
```

Data preperation for this clustering analysis include filtering out players who played less than 41 games, getting rid of all null values by imputing 0, scaling the data, calculating the distance measures, conducting pca, and finally creating a dataframe from the results of the pca.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
NBA.41 <- filter(NBA, g >= 41)
NBA.Kmeans <- NBA.41[,9:30]
NBA.Kmeans[is.na(NBA.Kmeans)] <- 0
Scaled_NBA.Kmeans <- scale(NBA.Kmeans)
dist_NBA.Kmeans <- dist(NBA.Kmeans)
pca <- prcomp(Scaled_NBA.Kmeans)
Nbacomp <- data.frame(pca$x[,1:5])
```

The following runs kmeans with a k between 1 and 30. It then saves the sum of squares for each model.

``` r
library(purrr)
tot_withinss <- map_dbl(1:30,  function(k){
  model <- kmeans(Nbacomp, centers = k)
  model$tot.withinss
})
```

Creates a dataframe for the sum of squares for each kmeans model.

``` r
elbow.NBA <- data.frame(
  k = 1:30,
  tot_withinss = tot_withinss
)
```

Plots the sum of squares for each level of k so that the optimal k can be chosen (k = 5)

``` r
library(ggplot2)
ggplot(elbow.NBA, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:30)
```

![](NBA_Clustering_files/figure-markdown_github/unnamed-chunk-5-1.png)

Runs a Kmeans model and keeps the silhouette widths for each level of k

``` r
library(cluster)
sil_width <- map_dbl(2:30,  function(k){
  model <- pam(NBA.Kmeans, k = k)
  model$silinfo$avg.width
})
```

Creates a dataframe that contains the silhouette width for each level of k

``` r
sil_NBA <- data.frame(
  k = 2:30,
  sil_width = sil_width
)
```

Plots the silhouette widths for each level of k to see how well on average each player fits in their cluster

``` r
ggplot(sil_NBA, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:30)
```

![](NBA_Clustering_files/figure-markdown_github/unnamed-chunk-8-1.png)

Creates a Kmeans model with k = 5

``` r
NBA.Kmeans5 <- kmeans(dist_NBA.Kmeans, centers = 5, nstart = 25)
```

Takes the cluster assignments from the model

``` r
cluster.NBA <- NBA.Kmeans5$cluster
```

Creates two seperate data frames one for assigning the cluster assignments to players and another data frame to analyze the means of each cluster

``` r
segment.NBA <- mutate(NBA.Kmeans, cluster = cluster.NBA)
Cluster.Assignments <- mutate(NBA.41, cluster = cluster.NBA)
```

Creates data frames of each cluster

``` r
Cluster1 <- filter(Cluster.Assignments, cluster == '1')
Cluster2 <- filter(Cluster.Assignments, cluster == '2')
Cluster3 <- filter(Cluster.Assignments, cluster == '3')
Cluster4 <- filter(Cluster.Assignments, cluster == '4')
Cluster5 <- filter(Cluster.Assignments, cluster == '5')
```

Calculates the mean for each stat by cluster

``` r
Cluster.means <- aggregate(segment.NBA, by = list(segment.NBA$cluster), FUN = "mean", na.rm = TRUE)
Cluster.means
```

    ##   Group.1       fg       fga fgpercent       x3p     x3pa x3ppercent
    ## 1       1 9.005556 18.977778 0.4766667 1.8777778 5.166667  0.3522222
    ## 2       2 7.183871 15.387097 0.4691290 1.6290323 4.306452  0.3521290
    ## 3       3 1.848760  4.173554 0.4472810 0.5107438 1.486777  0.2874959
    ## 4       4 5.007500 10.722500 0.4744250 1.1775000 3.246250  0.2928500
    ## 5       5 3.268421  7.343860 0.4489649 0.9245614 2.598246  0.3014123
    ##        x2p      x2pa x2ppercent efgpercent        ft      fta ftpercent
    ## 1 7.138889 13.822222  0.5192778  0.5260556 6.3611111 7.705556 0.8301111
    ## 2 5.548387 11.093548  0.5023871  0.5209032 3.7225806 4.590323 0.8080645
    ## 3 1.336364  2.685950  0.4902397  0.5053058 0.7264463 1.009091 0.7143967
    ## 4 3.838750  7.472500  0.5114625  0.5257750 2.1525000 2.855000 0.7574375
    ## 5 2.343860  4.737719  0.4894649  0.5099474 1.2157895 1.617544 0.7610088
    ##         orb      drb      trb      ast       stl       blk       tov
    ## 1 1.3944444 5.877778 7.272222 5.822222 1.3555556 0.8000000 3.1500000
    ## 2 1.3129032 4.706452 6.025806 3.796774 1.0387097 0.6322581 2.2258065
    ## 3 0.7082645 2.054545 2.766942 1.057851 0.4644628 0.3363636 0.7115702
    ## 4 1.3400000 4.302500 5.630000 2.548750 0.8700000 0.6062500 1.5925000
    ## 5 0.8780702 2.963158 3.845614 2.051754 0.7692982 0.4070175 1.1315789
    ##         pf      ps_g cluster
    ## 1 2.344444 26.266667       1
    ## 2 2.319355 19.712903       2
    ## 3 1.557025  4.928926       3
    ## 4 2.202500 13.335000       4
    ## 5 1.949123  8.679825       5

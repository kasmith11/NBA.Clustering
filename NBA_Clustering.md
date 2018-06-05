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
    ## 1       1 8.791667 18.704167 0.4714167 1.9250000 5.229167  0.3587917
    ## 2       2 1.848760  4.173554 0.4472810 0.5107438 1.486777  0.2874959
    ## 3       3 4.901408 10.578873 0.4684930 1.1873239 3.285915  0.3000563
    ## 4       4 3.262832  7.326549 0.4492035 0.9185841 2.579646  0.3009823
    ## 5       5 6.580000 13.877143 0.4834857 1.4171429 3.774286  0.3176857
    ##        x2p      x2pa x2ppercent efgpercent        ft      fta ftpercent
    ## 1 6.866667 13.491667  0.5107083  0.5229167 5.8416667 7.058333 0.8343333
    ## 2 1.336364  2.685950  0.4902397  0.5053058 0.7264463 1.009091 0.7143967
    ## 3 3.721127  7.288732  0.5073380  0.5214507 2.0281690 2.650704 0.7661549
    ## 4 2.344248  4.738938  0.4894248  0.5099469 1.2115044 1.614159 0.7600177
    ## 5 5.168571 10.108571  0.5158286  0.5318857 3.4171429 4.394286 0.7725429
    ##         orb      drb      trb      ast       stl       blk       tov
    ## 1 1.2708333 5.491667 6.758333 5.350000 1.2750000 0.7000000 2.9750000
    ## 2 0.7082645 2.054545 2.766942 1.057851 0.4644628 0.3363636 0.7115702
    ## 3 1.2746479 4.169014 5.428169 2.505634 0.8676056 0.5887324 1.5380282
    ## 4 0.8752212 2.941593 3.821239 2.057522 0.7690265 0.4044248 1.1345133
    ## 5 1.5200000 4.957143 6.488571 3.471429 0.9942857 0.7028571 2.0942857
    ##         pf      ps_g cluster
    ## 1 2.387500 25.366667       1
    ## 2 1.557025  4.928926       2
    ## 3 2.195775 13.004225       3
    ## 4 1.950442  8.657522       4
    ## 5 2.254286 17.994286       5

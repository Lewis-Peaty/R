---
title: "STA3300 - Assignment 2"
author: "Lewis Peaty"
date: "9 May 2016"
output: html_document
---

# Question 1 (16 marks)
> Traffic delay at different types of signalised intersections on urban streets is being studied.  Three types of trac signals are involved:  (1) pre-timed; (2) semi-actuated; and (3) fully actuated.  Five intersections of each type are available for study.  The response variable is the mean stopped time (in seconds) per vehicle negotiating the intersection.
The data is as follows:

| Pre-Timed | Semi-Actuated | Fully Actuated |
|-----------|---------------|----------------|
| 34.1      | 22.0          | 15.2           |
| 30.4      | 18.7          | 18.9           |
| 36.6      | 17.5          | 15.0           |
| 37.1      | 25.7          | 10.5           |
| 39.2      | 20.6          | 10.4           |

## a)  Express the 5x 3 data matrix in partitioned form in the same manner as demonstrated in eqn (2.7) in the Study Book. [4 marks]

```{r}


traffic <- matrix(c( 34.1      , 22.0          , 15.2           ,
 30.4      , 18.7          , 18.9           ,
 36.6      , 17.5          , 15.0           ,
 37.1      , 25.7          , 10.5           ,
 39.2      , 20.6          , 10.4           ),byrow = TRUE,nrow = 5)
traffic
traffic_mean <- traffic*0+mean(traffic)
traffic_mean

traffic_gp_means <- traffic*0
traffic_gp_means[,1] <- mean(traffic[,1])
traffic_gp_means[,2] <- mean(traffic[,2])
traffic_gp_means[,3] <- mean(traffic[,3])
traffic_gp_means
traffic_residiual <- traffic - traffic_mean - traffic_gp_means
traffic_residiual

```

## (b)  Similarly partition the degrees of freedom. [2 marks]

## (c)  Determine the total, treatment and error sums of squares directly from the matricesin part (a).[4 marks]

## (d)  Present the analysis of variance in tabular form.[2 marks]

## (e)  State the assumptions necessary for the validity of the analysis of variance F test.[2 marks]

## (f)  Estimate the p-value of this test and state your conclusions in the context of this study. [2 marks]



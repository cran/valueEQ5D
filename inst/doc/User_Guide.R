## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(valueEQ5D)

## ------------------------------------------------------------------------
## EQ-5D-3L data
 set.seed(17)
  EQ5D3Ldata <- data.frame(age=abs(rnorm(10, 60, 20)),
                           sex=factor(sample(c("M", "F"), 10, replace=T)),
                           arm=factor(sample(c("Control", "Intervention"), 10, replace=T)),
                           eq5d3L.q1=(sample(c(1,2,3), 10, replace=T)),
                           eq5d3L.q2=(sample(c(1,2,3), 10, replace=T)),
                           eq5d3L.q3=(sample(c(1,2,3), 10, replace=T)),
                           eq5d3L.q4=(sample(c(1,2,3), 10, replace=T)),
                           eq5d3L.q5=(sample(c(1,2,3), 10, replace=T)))
  
  ## EQ-5D-5L data
 set.seed(17)
  EQ5D5Ldata <- data.frame(age=abs(rnorm(10, 60, 20)),
                           sex=factor(sample(c("M", "F"), 10, replace=T)),
                           arm=factor(sample(c("Control", "Intervention"), 10, replace=T)),
                           eq5d5L.q1=(sample(c(1,2,3,4,5), 10, replace=T)),
                           eq5d5L.q2=(sample(c(1,2,3,4,5), 10, replace=T)),
                           eq5d5L.q3=(sample(c(1,2,3,4,5), 10, replace=T)),
                           eq5d5L.q4=(sample(c(1,2,3,4,5), 10, replace=T)),
                           eq5d5L.q5=(sample(c(1,2,3,4,5), 10, replace=T)))

## ------------------------------------------------------------------------
## Valuing EQ-5D-3L individual score
 valueEQ5D3LIndscores("UK","TTO",1,2,3,2,2)
 valueEQ5D3LIndscores("UK","VAS",c(1,2,3,2,2))
 valueEQ5D3LIndscores("UK","TTO",12322)

## ------------------------------------------------------------------------

result1<-valueEQ5D3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO",NULL,NULL)


## ------------------------------------------------------------------------
result1$stats
result1$frequencyTable
result1$histogram
result1$modifiedData

## ------------------------------------------------------------------------
result2<-valueEQ5D3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO","male",c(10,70))
result3<-valueEQ5D3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO","male",NULL)
result4<-valueEQ5D3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO",NULL,c(10,70))

## ------------------------------------------------------------------------
## Valuing EQ-5D-5L individual score

valueEQ5D5LIndscores("England",1,2,3,4,5)
valueEQ5D5LIndscores("England",c(1,2,3,4,5))
valueEQ5D5LIndscores("England",12345)
valueEQ5D5LIndscores("Germany",12345)
valueEQ5D5LIndscores("Spain",12345)
valueEQ5D5LIndscores("Indonesia",12345)

## ------------------------------------------------------------------------

valueEQ5D5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","England",NULL,NULL)
valueEQ5D5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","England","male",c(10,70))
valueEQ5D5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","Indonesia","male",NULL)
valueEQ5D5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","Ireland",NULL,c(10,70))

## ------------------------------------------------------------------------
## Valuing EQ-5D-5L individual score

eq5dmap5Lto3LIndscores("UK","CW",1,2,3,4,5)
eq5dmap5Lto3LIndscores("UK","CW",c(1,2,3,4,5))
eq5dmap5Lto3LIndscores("Denmark","CW",12345)

## ------------------------------------------------------------------------

 eq5dmap5Lto3L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","UK","CW",NULL,NULL)
 eq5dmap5Lto3L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","UK","CW","male",c(10,70))



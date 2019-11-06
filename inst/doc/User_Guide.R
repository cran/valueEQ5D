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
 value3LInd("UK","TTO",1,2,3,2,2)
 value3LInd("UK","VAS",c(1,2,3,2,2))
 value3LInd("UK","TTO",12322)

## ------------------------------------------------------------------------

result1<-value3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO",NULL,NULL)


## ------------------------------------------------------------------------
result1$stats
result1$frequencyTable
result1$histogram
result1$modifiedData

## ------------------------------------------------------------------------
result2<-value3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO","male",c(10,70))
result3<-value3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO","male",NULL)
result4<-value3L(EQ5D3Ldata,"eq5d3L.q1","eq5d3L.q2","eq5d3L.q3","eq5d3L.q4","eq5d3L.q5","UK","TTO",NULL,c(10,70))

## ------------------------------------------------------------------------
## Valuing EQ-5D-5L individual score

value5LInd("England",1,2,3,4,5)
value5LInd("England",c(1,2,3,4,5))
value5LInd("England",12345)
value5LInd("Germany",12345)
value5LInd("Spain",12345)
value5LInd("Indonesia",12345)

## ------------------------------------------------------------------------

value5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","England",NULL,NULL)
value5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","England","male",c(10,70))
value5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","Indonesia","male",NULL)
value5L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","Ireland",NULL,c(10,70))

## ------------------------------------------------------------------------
## Valuing EQ-5D-5L individual score

map5Lto3LInd("UK","CW",1,2,3,4,5)
map5Lto3LInd("UK","CW",c(1,2,3,4,5))
map5Lto3LInd("Denmark","CW",12345)

## ------------------------------------------------------------------------

 map5Lto3L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","UK","CW",NULL,NULL)
 map5Lto3L(EQ5D5Ldata,"eq5d5L.q1","eq5d5L.q2","eq5d5L.q3","eq5d5L.q4","eq5d5L.q5","UK","CW","male",c(10,70))



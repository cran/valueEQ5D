## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
 collapse = TRUE, 
 comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(valueEQ5D)

## ---- echo = FALSE------------------------------------------------------------
## EQ-5D-3L data
 set.seed(17)
 EQ5D3Ldata <- data.frame(age = abs(rnorm(10, 60, 20)), 
       sex = factor(sample(c("M", "F"), 10, replace = T)), 
       arm = factor(sample(c("Control", "Intervention"), 10, replace = T)), 
       eq5d3L.q1 = (sample(c(1, 2, 3), 10, replace = T)), 
       eq5d3L.q2 = (sample(c(1, 2, 3), 10, replace = T)), 
       eq5d3L.q3 = (sample(c(1, 2, 3), 10, replace = T)), 
       eq5d3L.q4 = (sample(c(1, 2, 3), 10, replace = T)), 
       eq5d3L.q5 = (sample(c(1, 2, 3), 10, replace = T)))
 
 ## EQ-5D-5L data
 set.seed(17)
 EQ5D5Ldata <- data.frame(age = abs(rnorm(10, 60, 20)), 
       sex = factor(sample(c("M", "F"), 10, replace = T)), 
       arm = factor(sample(c("Control", "Intervention"), 10, replace = T)), 
        eq5d5L.q1 = (sample(c(1, 2, 3, 4, 5), 10, replace = T)), 
              eq5d5L.q2 = (sample(c(1, 2, 3, 4, 5), 10, replace = T)), 
              eq5d5L.q3 = (sample(c(1, 2, 3, 4, 5), 10, replace = T)), 
              eq5d5L.q4 = (sample(c(1, 2, 3, 4, 5), 10, replace = T)), 
              eq5d5L.q5 = (sample(c(1, 2, 3, 4, 5), 10, replace = T)))

## ---- echo = FALSE------------------------------------------------------------
## Valuing EQ-5D-3L individual score
 value_3L_Ind("UK", "TTO", 1, 2, 3, 2, 2)
 value_3L_Ind("UK", "VAS", c(1, 2, 3, 2, 2))
 value_3L_Ind("UK", "TTO", 12322)

## ---- echo = FALSE------------------------------------------------------------

result1 <- value_3L(EQ5D3Ldata, "eq5d3L.q1", "eq5d3L.q2", "eq5d3L.q3", "eq5d3L.q4", "eq5d3L.q5", "UK", "TTO", NULL, NULL)


## ---- echo = FALSE------------------------------------------------------------
result1$stats
result1$frequencyTable
result1$histogram
result1$modifiedData

## ---- echo = FALSE------------------------------------------------------------
result2 <- value_3L(EQ5D3Ldata, "eq5d3L.q1", "eq5d3L.q2", "eq5d3L.q3", "eq5d3L.q4", "eq5d3L.q5", "UK", "TTO", "male", c(10, 70))
result3 <- value_3L(EQ5D3Ldata, "eq5d3L.q1", "eq5d3L.q2", "eq5d3L.q3", "eq5d3L.q4", "eq5d3L.q5", "UK", "TTO", "male", NULL)
result4 <- value_3L(EQ5D3Ldata, "eq5d3L.q1", "eq5d3L.q2", "eq5d3L.q3", "eq5d3L.q4", "eq5d3L.q5", "UK", "TTO", NULL, c(10, 70))

## ---- echo = FALSE------------------------------------------------------------
## Valuing EQ-5D-5L individual score

value_5L_Ind("England", 1, 2, 3, 4, 5)
value_5L_Ind("England", c(1, 2, 3, 4, 5))
value_5L_Ind("England", 12345)
value_5L_Ind("Germany", 12345)
value_5L_Ind("Spain", 12345)
value_5L_Ind("Indonesia", 12345)

## ---- echo = FALSE------------------------------------------------------------
value_5L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "England", NULL, NULL)
value_5L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "England", "male", c(10, 70))
value_5L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "Indonesia", "male", NULL)
value_5L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "Ireland", NULL, c(10, 70))

## ---- echo = FALSE------------------------------------------------------------
## Valuing EQ-5D-5L individual score

map_5Lto3L_Ind("UK", "CW", 1, 2, 3, 4, 5)
map_5Lto3L_Ind("UK", "CW", c(1, 2, 3, 4, 5))
map_5Lto3L_Ind("Denmark", "CW", 12345)

## ---- echo = FALSE------------------------------------------------------------
 map_5Lto3L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "UK", "CW", NULL, NULL)
 map_5Lto3L(EQ5D5Ldata, "eq5d5L.q1", "eq5d5L.q2", "eq5d5L.q3", "eq5d5L.q4", "eq5d5L.q5", "UK", "CW", "male", c(10, 70))


# Gender Wage Gap Econometric Analysis
### Author: Mannat Sandhu
### February 9th, 2019

This project studies the differences in wages and hours worked by the gender of respondents in Org data (included in the repository) and how these differences in employment outcomes change along the lifecycle of work, from college age to retirement.

Specifically, we estimate the following using Generalized Linear Model: 

log(rw) = as.factor(age)*female + educ + wbho + u

where, 
* rw : real wage 
* educ : education factor variable
* wbho : factor variable describing race of the respondent
* female : takes on value 1 when respondent is a woman

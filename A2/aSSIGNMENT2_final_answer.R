library(sampling)
library(tidyverse)
set.seed(1004145944)

getwd()
setwd("D:/STA304")
baseballdata<-read.csv("baseball.csv")


## a
Ni <- baseballdata %>% group_by(team) %>% summarise(Ni = n()); Ni
Ni_Vecotr <- Ni$Ni; Ni_Vecotr
N <- sum(Ni_Vecotr);N
Group_i_prop <- Ni_Vecotr/N
n <- 150
ni <- round(n * Group_i_prop);ni
strat <- strata(baseballdata, stratanames = 'team', ni, method = "srswor" )
team_strata_data <- getdata(baseballdata, strat); team_strata_data

## b
mean_for_use <- team_strata_data %>% group_by(Stratum) %>% summarise(yibar = mean(log(salary)), si = sd(log(salary)));mean_for_use
Ybarst = sum(Ni_Vecotr/N * mean_for_use$yibar);Ybarst
Variance = sum((Ni_Vecotr/N) ** 2 * ((Ni_Vecotr - ni)/Ni_Vecotr) * ((mean_for_use$si ** 2)/n));Variance
sd = Variance ** 0.5
Bound_of_error = 2 * sd
Lower_bound <-Ybarst - Bound_of_error;Lower_bound
Upper_bound <-Ybarst + Bound_of_error;Upper_bound


## c
porportion_for_use <- team_strata_data %>% group_by(Stratum) %>% summarise(pihat = mean(position == "P"));porportion_for_use
pi = porportion_for_use$pihat;pi
qi = 1 - pi; qi
variance_p = sum((Ni_Vecotr/N) ** 2 * ((Ni_Vecotr - ni)/Ni_Vecotr) * (pi * qi/ni));variance_p
sdp = variance_p ** 0.5
Bound_of_error_p = 2 * sdp
pbar = sum((Ni_Vecotr/N) * pi);pbar
Lower_bound_p <-pbar - Bound_of_error_p;Lower_bound_p
Upper_bound_p <-pbar + Bound_of_error_p;Upper_bound_p


## d
srs_baseball = sample(c(1:nrow(baseballdata)), size = 150, replace = FALSE)
pos = baseballdata$position[srs_baseball];pos
prp_p = mean(pos == "P"); prp_p
N = nrow(baseballdata);N
n = 150
q_srs = 1 - prp_p
Variance_srs_p = (1 - n/N)*(prp_p*q_srs/n)
sdp = Variance_srs_p ** 0.5
Bound_of_error_p = 2 * sdp
Lower_bound_p <-prp_p - Bound_of_error_p;Lower_bound_p
Upper_bound_p <-prp_p + Bound_of_error_p;Upper_bound_p


## e
variance_of_logsal = (mean_for_use$si) ** 2; variance_of_logsal


## f
ai = (Ni_Vecotr * variance_of_logsal)/sum((Ni_Vecotr * variance_of_logsal)); ai
ni_by_Neyman = round(n * ai);ni_by_Neyman
sum(ni_by_Neyman) ## checking, since sum != n, we need to do some change.
## In my personal opinion, minus the last one by 1. Since the last group can also be written as n - n1 - n2 - n3- n4- ... - nl-1
ni_by_Neyman[30] = ni_by_Neyman[30] - 1
sum(ni_by_Neyman)
strat_neyman <- strata(baseballdata, stratanames = 'team', ni_by_Neyman, method = "srswor" )
team_strata_data_neyman <- getdata(baseballdata, strat_neyman); team_strata_data_neyman


assign_3data<-read.csv("D:/??????/STA304/Final/hhw21.csv")
attach(assign_3data)
N=length(handspan); N
n=10
set.seed(1004145944)
srs<-sample(N,n);srs
sampx = handspan[srs]; sampx
sampy = height[srs];sampy
samid = id[srs];samid
data.frame(samid,sampx,sampy)
miu_x = mean(handspan)
miu_y = mean(height)

#b SRS
miu_srs_hat = mean(sampy);miu_srs_hat
s2=var(sampy); s2
var_miu_hat_hat = (1-(n/N))*(s2/n);var_miu_hat_hat
Bound_OF_ERROR_SRS = 2 * sqrt(var_miu_hat_hat);Bound_OF_ERROR_SRS


#c Ratio
r = mean(sampy)/mean(sampx);r
miu_r_hat = r * miu_x;miu_r_hat
s_r_2 = var(sampy - r * sampx)
var_miu_r_hat_hat = (1 - n/N) * (s_r_2/n)
Bound_OF_ERROR_RATIO = 2 * sqrt(var_miu_r_hat_hat);Bound_OF_ERROR_RATIO

#d regression
regression_model <- lm(sampy ~ sampx)
summary(regression_model)
b = 3.378
miu_regression_hat = mean(sampy) + b*(miu_x - mean(sampx));miu_regression_hat
mse = sum(residuals(regression_model)^2)/(n-2)
var_miu_regression_hat_hat = (1 - n/N) * (mse/n)
Bound_OF_ERROR_Regression = 2 * sqrt(var_miu_regression_hat_hat);Bound_OF_ERROR_Regression

#e difference
miu_difference_hat = mean(sampy) + (miu_x -mean(sampx));miu_difference_hat
d = sampy - sampx
s_d_2 = var(d)
var_miu_difference_hat_hat = (1 - n/N) * (s_d_2/n)
Bound_OF_ERROR_Difference = 2 * sqrt(var_miu_difference_hat_hat);Bound_OF_ERROR_Difference

#f
abs_srs = abs(miu_srs_hat - miu_y);abs_srs
abs_r = abs(miu_r_hat - miu_y);abs_r
abs_regression = abs(miu_regression_hat - miu_y);abs_regression
abs_difference = abs(miu_difference_hat - miu_y);abs_difference

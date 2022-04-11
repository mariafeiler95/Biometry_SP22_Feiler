# Week 9 Lab

library(MASS)
library(car)
library(boot)
library(smatr)

Temperature <- c(5.1, 5.6,  5.7, 6.6, 6.7)
Year <- c(1979, 1982, 1985, 1988, 1991)

plot(Year, Temperature, pch = 16)

ans <- cor.test(Temperature, Year)
ans
# Pearson's product-moment
# 	correlation
# 
# data:  Temperature and Year
# t = 6.4299, df = 3, p-value =
# 0.007626
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5625553 0.9978118
# sample estimates:
#      cor 
# 0.965581 

names(ans)
# [1] "statistic"   "parameter"   "p.value"     "estimate"    "null.value" 
# [6] "alternative" "method"      "data.name"   "conf.int"

r <- as.numeric(ans$estimate)
df <- as.numeric(ans$parameter)
t <- r*sqrt(df/(1-(r^2)))
# [1] 6.429911

ans$statistic
#        t 
# 6.429911 

plot(seq(-7,7,0.01),dt(seq(-7,7,0.01),df=3),typ="l",col="purple",lwd=2)
abline(v=t)

pt(6.4299,df=3,lower.tail=F)*2
# [1] 0.007625665

z<-(1/2)*log((1+r)/(1-r))
z
# [1] 2.022468

n<-5
LL.z<-z-(1/sqrt(n-3))*qnorm(0.975)
LL.r<-tanh(LL.z)
LL.r
# [1] 0.5625553

UL.z<-z+(1/sqrt(n-3))*qnorm(0.975)
UL.r<-tanh(UL.z)
UL.r
# [1] 0.9978118

cor.test(Temperature,Year, method="kendall")
# Kendall's rank correlation tau
# 
# data:  Temperature and Year
# T = 10, p-value = 0.01667
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
#   1

X<-seq(1,30) #a stand in for some covariate
value<-c()
intercept<-0.15
slope<--2.2
sigma<-10
for (i in 1:length(X))
{
        value<-c(value,rnorm(1,mean=intercept+slope*X[i],sd=sigma))
}
fit<-lm(value~X)
summary(fit)
# Call:
#         lm(formula = value ~ X)
# 
# Residuals:
#         Min       1Q   Median       3Q 
# -21.8432  -5.3113  -0.2301   6.3079 
# Max 
# 23.2362 
# 
# Coefficients:
#         Estimate Std. Error
# (Intercept)   3.3230     4.2328
# X            -2.4317     0.2384
# t value Pr(>|t|)    
# (Intercept)   0.785    0.439    
# X           -10.199 6.22e-11 ***
#         ---
#         Signif. codes:  
#         0 '***' 0.001 '**' 0.01 '*' 0.05
# '.' 0.1 ' ' 1
# 
# Residual standard error: 11.3 on 28 degrees of freedom
# Multiple R-squared:  0.7879,	Adjusted R-squared:  0.7803 
# F-statistic:   104 on 1 and 28 DF,  p-value: 6.219e-11

attach(faithful) #A rare exception to the rule of avoiding 'attach'
head(faithful)
#   eruptions waiting
# 1     3.600      79
# 2     1.800      54
# 3     3.333      74
# 4     2.283      62
# 5     4.533      85
# 6     2.883      55

plot(waiting, eruptions,pch=16)

eruption.lm<-lm(eruptions~waiting)
eruption.lm
# Call:
# lm(formula = eruptions ~ waiting)
# 
# Coefficients:
# (Intercept)      waiting  
#    -1.87402      0.07563

# Model: y~x1
# Meaning: y is explained by x1 only (intercept implicit)
# 
# Model: y~x1-1
# Meaning: y is explained by x1 only (no intercept)
# 
# Model: y~x1+x2
# Meaning: y is explained x1 and x2
# 
# Model: x1+x2+x1:x2
# Meaning: y is explained by x1,x2 and also by the interaction between them
# 
# Model: y~x1*x2
# Meaning: y is explained by x1,x2 and also by the interaction between them 
# (this is an alternative way of writing the above)

summary(eruption.lm)
# Call:
#         lm(formula = eruptions ~ waiting)
# 
# Residuals:
#         Min       1Q   Median       3Q      Max 
# -1.29917 -0.37689  0.03508  0.34909  1.19329 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.874016   0.160143  -11.70   <2e-16 ***
#         waiting      0.075628   0.002219   34.09   <2e-16 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4965 on 270 degrees of freedom
# Multiple R-squared:  0.8115,	Adjusted R-squared:  0.8108 
# F-statistic:  1162 on 1 and 270 DF,  p-value: < 2.2e-16

quantile(residuals(eruption.lm),probs=c(0.0,0.25,0.5,0.75,1.0))
#          0%         25%         50%         75%        100% 
# -1.29917268 -0.37689320  0.03508321  0.34909412  1.19329194 

residuals<-eruptions-predict(eruption.lm, data.frame(waiting))
quantile(residuals,probs=c(0.0,0.25,0.5,0.75,1.0))
#          0%         25%         50%         75%        100% 
# -1.29917268 -0.37689320  0.03508321  0.34909412  1.19329194 

x<-waiting
y<-eruptions
SSXY<-sum((x-mean(x))*(y-mean(y)))
SSX<-sum((x-mean(x))*(x-mean(x)))
slope.est<-SSXY/SSX
#Also could have used slope.est<-cov(x,y)/var(x)
n<-length(x)
residuals<-residuals(eruption.lm)
var.slope<-(1/(n-2))*sum((residuals-mean(residuals))*(residuals-mean(residuals)))/SSX
s.e.slope<-sqrt(var.slope)

slope.est
# [1] 0.07562795

s.e.slope
# [1] 0.002218541

t.value<-slope.est/s.e.slope
t.value
# [1] 34.08904

p.value<-2*(1-pt(abs(t.value),n-2))
p.value
# [1] 0

residual.se<-sqrt((1/(n-2))*sum((residuals-mean(residuals))*(residuals-mean(residuals))))
residual.se
# [1] 0.4965129

SST<-sum((y-mean(y))*(y-mean(y)))
SSR<-SST-sum(residuals*residuals)
R2<-SSR/SST
R2
# [1] 0.8114608

1-(1-R2)*((length(faithful$eruptions)-1)/(length(faithful$eruptions)-p.value-1))
# [1] 0.8114608
(cor(x,y))^2
# [1] 0.8114608

newdata<-data.frame(waiting=seq(min(waiting),max(waiting)))
confidence.bands<-predict(eruption.lm,newdata,interval="confidence")
prediction.bands<-predict(eruption.lm,newdata,interval="predict")
plot(waiting,eruptions,ylim=c(0,7))
lines(newdata[,1],confidence.bands[,1],col=1)
lines(newdata[,1],confidence.bands[,2],col=2)
lines(newdata[,1],confidence.bands[,3],col=2)
lines(newdata[,1],prediction.bands[,2],col=3)
lines(newdata[,1],prediction.bands[,3],col=3)

eruption.lm2<-lm(eruptions~waiting-1)
summary(eruption.lm2)
# Call:
#         lm(formula = eruptions ~ waiting - 1)
# 
# Residuals:
#         Min       1Q   Median       3Q      Max 
# -1.54127 -0.57533 -0.00846  0.42257  1.25718 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# waiting 0.0501292  0.0005111   98.09   <2e-16 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6084 on 271 degrees of freedom
# Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9725 
# F-statistic:  9621 on 1 and 271 DF,  p-value: < 2.2e-16

plot(waiting,eruptions,ylim=c(0,7))
lines(newdata[,1],confidence.bands[,1])
short<-(eruptions<3)
points(waiting[short],eruptions[short],pch=16)
eruption.lm<-lm(eruptions~waiting,weights=rep(1,times=272))
abline(a=eruption.lm$coef[1],b=eruption.lm$coef[2],col="black",lwd=2)
eruption.lm.wt<-lm(eruptions~waiting,weights=rep(1,times=272)+as.numeric(short))
abline(a=eruption.lm.wt$coef[1],b=eruption.lm.wt$coef[2],col="green",lwd=2)
eruption.lm.wt<-lm(eruptions~waiting,weights=rep(1,times=272)+9*as.numeric(short))
abline(a=eruption.lm.wt$coef[1],b=eruption.lm.wt$coef[2],col="purple",lwd=2)

data(Duncan)
head(Duncan)
#            type income education  prestige 
# accountant prof     62        86       82
# pilot      prof     72        76       83
# architect  prof     75        92       90
# author     prof     55        90       76
# chemist    prof     64        86       90
# minister   prof     21        84       87

plot(Duncan$education,Duncan$income,ylim=c(0,100))
temp<-c(which(rownames(Duncan)=="RR.engineer"),which(rownames(Duncan)=="conductor"))
text(x=Duncan$education[temp]-8,y=Duncan$income[temp],labels=rownames(Duncan)[temp],cex=0.5)

Duncan.model.lm<-lm(income~education, data=Duncan)
summary(Duncan.model.lm)
# Call:
#         lm(formula = income ~ education, data = Duncan)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -39.572 -11.346  -1.501   9.669  53.740 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  10.6035     5.1983   2.040   0.0475 *  
#         education     0.5949     0.0863   6.893 1.84e-08 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 17.04 on 43 degrees of freedom
# Multiple R-squared:  0.5249,	Adjusted R-squared:  0.5139 
# F-statistic: 47.51 on 1 and 43 DF,  p-value: 1.84e-08

outliers<-c(which(rownames(Duncan)=="RR.engineer"),which(rownames(Duncan)=="conductor"))
Duncan.model2<-lm(income[-outliers]~education[-outliers],data=Duncan)
# Call:
#         lm(formula = income[-outliers] ~ education[-outliers], data = Duncan)
# 
# Coefficients:
#         (Intercept)  education[-outliers]  
# 5.1206                0.6543

Duncan.model.rlm<-rlm(income~education,data=Duncan)
summary(Duncan.model.rlm)
# Call: rlm(formula = income ~ education, data = Duncan)
# Residuals:
#         Min       1Q   Median       3Q      Max 
# -40.8684  -9.8692   0.8085   7.8394  56.1770 
# 
# Coefficients:
#         Value  Std. Error t value
# (Intercept) 6.3002 4.4943     1.4018 
# education   0.6615 0.0746     8.8659 
# 
# Residual standard error: 13.06 on 43 degrees of freedom

boot.huber<-function(data,indices,maxit)
{
        data<-data[indices,] #select observations in bootstrap sample
        mod<-rlm(income~education,data=data,maxit=maxit)
        coefficients(mod) #return the coefficient vector
}
duncan.boot<-boot(Duncan,boot.huber,1999,maxit=100)
duncan.boot
# ORDINARY NONPARAMETRIC BOOTSTRAP
# 
# 
# Call:
#         boot(data = Duncan, statistic = boot.huber, R = 1999, maxit = 100)
# 
# 
# Bootstrap Statistics :
#      original       bias    std. error
# t1* 6.3002197  0.278354977  4.53698416
# t2* 0.6615263 -0.006923781  0.07450177


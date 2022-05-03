#script de la r?gression lin?aire multiple sur le dataset "World Happiness 2019"
#
#install.packages("performance")
library(performance)
library(car)
library(tidyverse)

path <- file.choose()
happiness<- read.csv(path)
score.lm=lm(formula=Score~.,data=happiness)
summary(score.lm)
extractAIC(score.lm)
check_model(score.lm)
check_normality(score.lm) 
check_heteroscedasticity(score.lm)

############ OUTLIER 

mod <- lm(formula=Score~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Perceptions.of.corruption,data=happiness)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(happiness[influential, ])

happiness2<-happiness[-c(34,76,102,148,152,155),]


############### FIN OUTLIER


score.lm<-lm(formula=Score**2~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Perceptions.of.corruption+Generosity,data=happiness2)
summary(score.lm)
extractAIC(score.lm)
check_model(score.lm)
check_normality(score.lm) 
check_heteroscedasticity(score.lm)


#########
score.lm<- update(score.lm,.~.-Generosity)
summary(score.lm)
extractAIC(score.lm)
check_model(score.lm)
check_normality(score.lm) 
check_heteroscedasticity(score.lm)


#########
score.lm<- update(score.lm,.~.-GDP.per.capita)
summary(score.lm)
extractAIC(score.lm)
check_model(score.lm)
check_normality(score.lm) 
check_heteroscedasticity(score.lm)

######## 
score.lm<-lm(formula=Score~GDP.per.capita+Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Perceptions.of.corruption,data=happiness2)
Res=residuals(score.lm)
hist(Res,freq=FALSE,nclass=6, col="yellow",main="histogramme des r?sidus")
x=seq(-20,20,by=0.1)
y=dnorm(x,0,6)
lines(x,y,type="l",col="red",lwd=2.5)





p1=predict(score.lm,data.frame(GDP.per.capita=0.2,Social.support=0.8,Healthy.life.expectancy=0.8,Freedom.to.make.life.choices=0.8,Perceptions.of.corruption=0.4),interval="prediction",level=0.99)
cat("Score pr?dit : ",sqrt(p1[1])) #Car le mod?le retourne le score au carr?
p2=predict(score.lm,data.frame(GDP.per.capita=2,Social.support=1.8,Healthy.life.expectancy=1.8,Freedom.to.make.life.choices=0.8,Perceptions.of.corruption=0.2),interval="prediction",level=0.99)
cat("Score pr?dit (2e test) : ",sqrt(p2[1]))

yn=rnorm(50,0,5)
qqplot(Res,yn,main="droite de Henri",col="blue")
z=seq(-15,15,by=0.1)
zz=z
lines(z,zz,type="l",col="red")



acf(Res,ci=0.99)

shapiro.test(residuals(score.lm))

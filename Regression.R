model1<-lm(Body.fat~Age+Height+Weight+Neck.circumferences+Chest.circumferences+Abdomen.circumferences+Hip.circumferences+Thigh.circumferences+Knee.circumferences+Ankle.circumferences+Biceps.circumferences+Forearm.circumferences+Wrist.circumferences,data=dataset)#innitial full model
summary(model1)#summary of full model
anova(model1)#anova table

dataset.sum<-summary(model1)
h<-hatvalues(model1) #leverage value
yhat<-model1$fit #prediction
e<-residuals(model1) #original residual
d<-e/dataset.sum$sigma #standardized residual
r<-e/(dataset.sum$sigma*sqrt(1-h)) #studentized residual
t<-rstudent(model1) #R-student residual
qqnorm(t) #normal probability plot
plot(yhat, t) #plot of residual vs. prediction

#plot of residual vs. regressors
plot(dataset$Age, t)
plot(dataset$Height, t) 
plot(dataset$Weight, t)
plot(dataset$Neck.circumferences, t)
plot(dataset$Chest.circumferences, t)
plot(dataset$Abdomen.circumferences, t)
plot(dataset$Hip.circumferences, t)
plot(dataset$Thigh.circumferences, t)
plot(dataset$Knee.circumferences, t)
plot(dataset$Ankle.circumferences, t)
plot(dataset$Biceps.circumferences, t)
plot(dataset$Forearm.circumferences, t)
plot(dataset$Wrist.circumferences, t)

#stepwise method for variable selection
dataset.null<-lm(Body.fat~1, data=dataset)
dataset.full<-lm(Body.fat~., data=dataset)
step(dataset.full, data=dataset, direction="backward")#stepwise variable selection

#variable selection plot 
library(leaps)
dataset.subs<-regsubsets(Body.fat~Age+Height+Weight+Neck.circumferences+Chest.circumferences+Abdomen.circumferences+Hip.circumferences+Thigh.circumferences+Knee.circumferences+Ankle.circumferences+Biceps.circumferences+Forearm.circumferences+Wrist.circumferences, data=dataset, nbest=6)
plot(dataset.subs, scale="adjr2")
plot(dataset.subs, scale="bic")
plot(dataset.subs, scale="r2")
plot(dataset.subs, scale="cp")

#model comparision
fit1<-lm( Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences + 
    Hip.circumferences, data = dataset)
fit2<-lm( Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = dataset)

#thorough analysis of reduced model
yhat1<-fit2$fit
t1<-rstudent(fit2)
qqnorm(t1)
plot(yhat1, t1) #plot of residual vs. prediction

#plot of residual vs. regressors
plot(dataset$Age, t1) 
plot(dataset$Height1, t1) 
plot(dataset$Neck.circumferences, t1)
plot(dataset$Abdomen.circumferences, t1)
plot(dataset$t1high.circumferences, t1)
plot(dataset$Forearm.circumferences, t1)
plot(dataset$Wrist1.circumferences, t1)


#comparision anova of the two best model 
reduced = lm( Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = dataset)
full = lm(Body.fat ~ 0 + as.factor(Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences), data = dataset)   
anova(reduced,full)    

outlierTest(fit2) # Bonferonni p-value for most extreme obs
qqPlot(fit2, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit2) # leverage plots

#deleted the potienial outliers
deletedDataset<-dataset[c(-39,-207,-224,-225),]
deletedFit<-lm( Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = deletedDataset)
summary(deletedFit)

#ridge regression
plot(lm.ridge(Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = dataset, lambda = seq(0,1,0.01)))
select(lm.ridge(Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = dataset, lambda = seq(0,1,0.01)))
bodyfat.ridge<-lm.ridge(Body.fat ~ Abdomen.circumferences + Height + Wrist.circumferences + 
    Forearm.circumferences + Neck.circumferences + Age + Thigh.circumferences, data = dataset, lambda=0.57)

#cross validation k-fold
library(bootstrap)
# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
# matrix of predictors
X <- as.matrix(dataset[c("Abdomen.circumferences","Height","Wrist.circumferences","Forearm.circumferences","Neck.circumferences","Height","Thigh.circumferences")])
# vector of predicted values
y <- as.matrix(dataset[c("Body.fat")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=20)
cor(y, fit$fitted.values)**2 # raw R2 
cor(y,results$cv.fit)**2 # cross-validated R2

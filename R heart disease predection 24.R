heart_data <- read.csv("heart.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
name <- c("age","sex","chest_pain","rest_bp","chol","fasting_bloodsugar","rest_ecg","max_heartrate","excercise_angina","slope","n_major_vasel","thal","target")
head(heart_data)

library(ggplot2)
library(car)
library(dplyr)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)

name <- c("age","sex","chest_pain","rest_bp","chol","fasting_bloodsugar","rest_ecg","max_heartrate","excercise_angina","ST_depression","slope","n_major_vasel","thal","target")
names(heart_data) <- name
heart_data$sex <- as.character(heart_data$sex)
heart_data$sex <- ifelse(heart_data$sex=="0",'female','male')
heart_data$chest_pain <- as.character(heart_data$chest_pain)
title.center <- theme(plot.title = element_text(hjust = 0.5))

head(heart_data)
str(heart_data)
summary(heart_data)

#EDA
ggplot(heart_data,aes(x = age)) + geom_histogram(bins =30,fill ="dodgerblue4") + theme_bw() + theme_classic() +ggtitle("age distribution") +ylab("number of people")
ggplot(heart_data,aes(x = age)) + geom_histogram(bins =30,fill ="dodgerblue4") + theme_bw() + theme_classic() +ggtitle("age distribution") +ylab("number of people")
boxplot(heart_data$age,main ="boxplot of age for normality check",col ="dodgerblue4",notch = T)
qqPlot(heart_data$age,main ="normality check for age",grid = F)

#sex
ggplot(heart_data,aes(x =sex)) + geom_bar(width = 0.2,fill ="green") + geom_text(stat = 'count',aes(label =..count..),vjust =-0.5) + theme_bw() + theme_classic() +ylab("number of count") + ggtitle("sex") + title.center
table(heart_data$chest_pain)
ggplot(heart_data,aes(x = chest_pain)) + geom_bar(width =0.2,fill ="red") + geom_text(stat = 'count',aes(label =..count..),vjust = -0.5)  + theme_bw()+theme_classic() + ggtitle("chest pain type") + title.center

#rest bp
boxplot(heart_data$rest_bp,col = "dodgerblue2",main ="boxplot of rest_bp",col.main="dodgerblue4")
ggplot(heart_data,aes(rest_bp)) + geom_histogram(bins =20,fill ="green") +theme_bw() + theme_classic() +ggtitle("resp_bp") +title.center
ggplot(heart_data,aes(rest_bp)) + geom_density(fill ="dodgerblue4") + theme_bw() + theme_classic()+ggtitle("density plot of resp_bp") +title.center

#chol
boxplot(heart_data$chol,col = "dodgerblue2",main ="boxplot of chol",col.main="dodgerblue4")
ggplot(heart_data,aes(chol)) + geom_histogram(bins =20,fill ="green") +theme_bw() + theme_classic() +ggtitle("chol") +title.center
ggplot(heart_data,aes(chol)) + geom_density(fill ="dodgerblue4") + theme_bw() + theme_classic()+ggtitle("density plot of chol") +title.center
table(heart_data$fasting_bloodsugar)
ggplot(heart_data,aes(x =factor(fasting_bloodsugar))) + geom_bar(width = 0.1,fill ="green") + geom_text(stat = 'count',aes(label =..count..),vjust =-0.5) + theme_bw() + theme_classic() +ylab("number of count") + ggtitle("blood sugar") + title.center
ggplot(heart_data,aes(factor(fasting_bloodsugar))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of fasting_bloodsugar") +title.center

ggplot(heart_data,aes(factor(rest_ecg))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of rest_ecg") +title.center

#max heart rate
ggplot(heart_data,aes(max_heartrate)) + geom_histogram(fill = "dodgerblue4",alpha =0.5) + theme_bw()+theme_classic()
ggplot(heart_data,aes(max_heartrate)) + geom_density(fill = "red",alpha =0.5) + theme_bw()+theme_classic()
boxplot(heart_data$max_heartrate,col ="lightblue",notch = T,main ="boxplot of the maximum heart rate")

ggplot(heart_data,aes(factor(excercise_angina))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of exercise angina") +title.center

ggplot(heart_data,aes(factor(slope))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of slope") +title.center

ggplot(heart_data,aes(factor(n_major_vasel))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of n_major_vassel") +title.center

ggplot(heart_data,aes(factor(thal))) + geom_bar(width = 0.2,fill ="dodgerblue4") + theme_bw() + theme_classic()+geom_text(stat ='count',aes(label =..count..),vjust =-0.2)+ggtitle("barplot of thal") +title.center

#logitic regression using train and test
set.seed(123)
train.index <- heart_data$target %>% createDataPartition(p =0.8,list =F)
train.data <- heart_data[train.index,]
test.data <- heart_data[-train.index,]

# fitting model for logitics regression
full.mod <- glm(target~.,data =train.data,family =binomial)
summary(full.mod)

# checking model accuracy
prob <- full.mod %>% predict(test.data,type ="response")
predicted.class1 <- ifelse(prob>0.5,1,0)
mean(predicted.class1==test.data$target)

#step wise logistic regression
step.model <- full.mod %>% stepAIC(trace = F)
summary(step.model)
prob.step <- step.model %>% predict(test.data,type ="response")
predicted.class2 <- ifelse(prob.step>0.5,1,0) 
mean(predicted.class2==test.data$target)

#logistic regression in r
#only quantitative variable
model_check <- glm(target~.,data =heart_data,family = binomial)
prob.check <- predict(model_check,type ="response")
my_data <- heart_data %>% select_if(is.numeric)
predictors <- colnames(my_data)
my_data <- my_data%>% mutate(logit = log(prob.check/(1-prob.check))) %>%
  gather(key = "predictors",value = "predicted.value",-logit)

#checking the linearnity 
ggplot(my_data,aes(x =logit,y =predicted.value)) + geom_point() + geom_smooth(method ="loess") + theme_classic() + theme_bw()+facet_wrap(~predictors,scale ="free_y")

#checking the infentuial point
plot(step.model,which=4,id.n =3)
model.data <- augment(step.model) %>% mutate(index =1:n())
model.data %>% top_n(3,.cooksd)
car::vif(step.model)

#checking the best cuutoff probability

res <- predict(step.model,type ="response")
ROCR_Pred <- prediction(res,train.data$target)
ROCR_perf <- performance(ROCR_Pred,"tpr","fpr")
plot(ROCR_perf,colorize=T,print.cutoffs.at =seq(0.1,by =0.1))

# final model
final.model <- glm(target~.,data =train.data,family =binomial) %>%stepAIC(trash =FALSE)
prob.final <- predict(final.model,test.data,type ="response")
predicted.class_final <- ifelse(prob.final>0.6,1,0)
mean(predicted.class_final==test.data$target)





































































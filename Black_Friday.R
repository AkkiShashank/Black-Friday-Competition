setwd("/Users/shashankgupta/Desktop/AnalyticsVidhya/Black Friday/")

library(data.table)

train<-fread("train.csv",stringsAsFactors = T)
test<-fread("test.csv",stringsAsFactors = T)

#first prediction using mean
sub_mean<-data.frame(User_ID=test$User_ID,Product_ID=test$Product_ID,
                     Purchase=mean(train$Purchase))
write.csv(sub_mean,file = "sub_mean.csv",row.names = F)

#combine data set
test[,Purchase := mean(train$Purchase)]
c<-list(train,test)
combin<-rbindlist(c)

#analyzing gender variable
combin[,prop.table(table(Gender))]

combin[,prop.table(table(Age))]
combin[,prop.table(table(City_Category))]
combin[,prop.table(table(Stay_In_Current_City_Years))]
length(unique(combin$Product_ID))
length(unique(combin$User_ID))

#missing
colSums(is.na(combin))

library(ggplot2)
ggplot(combin,aes(Age,fill=Gender)) + geom_bar()

ggplot(combin,aes(Age,fill=City_Category))+geom_bar()

library(gmodels)
CrossTable(combin$Occupation,combin$City_Category)

#create a new variable for missing values
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2,is.na)==T,1,0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3,is.na)==T,1,0)]

combin[,Product_Category_2 := ifelse(sapply(combin$Product_Category_2,is.na)==T,"-999",
                                     Product_Category_2)]
combin[,Product_Category_3 := ifelse(sapply(combin$Product_Category_3,is.na)==T,"-999",
                                     Product_Category_3)]
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years)
                                          =="4+"]<-"4"

levels(combin$Age)[levels(combin$Age)=="0-17"]<-"0"
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

combin$Age<-as.numeric(combin$Age)
combin[,Gender := as.numeric(as.factor(Gender))-1]

combin[,User_Count := .N ,by= User_ID]
combin[,Product_Count := .N,by=Product_ID]

#Mean Purchase of Product
combin[, Mean_Purchase_Product :=  mean(Purchase),by=Product_ID]
#Mean Purchase of User
combin[,Mean_Purchase_User := mean(Purchase),by=User_ID]

library(dummies)
combin<-dummy.data.frame(combin,names = c("City_Category"),sep="_")

#check classes of all variables
sapply(combin, class)

#converting Product Category 2 & 3
combin$Product_Category_2<-as.integer(combin$Product_Category_2)
combin$Product_Category_3<-as.integer(combin$Product_Category_3)

#Divide into train and test
c.train<-combin[1:nrow(train),]
c.test<-combin[-(1:nrow(train)),]

#Removing outliers
c.train<-c.train[c.train$Product_Category_1<=18,]

library(h2o)
h2o.init()

train.h2o<-as.h2o(c.train)
test.h2o<-as.h2o(c.test)

#check column index number
colnames(train.h2o)

y.dep<-14
x.dep<-c(3:13,15:20)

reg.model<-h2o.glm(y=y.dep,x=x.dep,training_frame = train.h2o,family = "gaussian")
h2o.performance(reg.model)
predict.reg<-as.data.frame(h2o.predict(reg.model,test.h2o))
sub_reg <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                      Purchase =  predict.reg$predict)
write.csv(sub_reg,file = "reg.csv",row.names = F)


###h2o.randomForest
rf.model<-h2o.randomForest(y=y.dep,x=x.dep,training_frame = train.h2o,ntrees=1000,
                           mtries = 3,max_depth = 4,seed = 1122)
h2o.performance(rf.model)
predict.rf<-as.data.frame(h2o.predict(rf.model,test.h2o))
sub_rf <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                     Purchase =  predict.rf$predict)
write.csv(sub_rf, file = "rf.csv", row.names = F)

###gbm
gbm.model<-h2o.gbm(y=y.dep,x=x.dep,training_frame = train.h2o,ntrees = 1000,
                   max_depth = 4,learn_rate = 0.02,seed=1122)
predict.gbm<-as.data.frame(h2o.predict(gbm.model,test.h2o))
sub_gbm <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                      Purchase = predict.gbm$predict)
write.csv(sub_gbm, file = "gbm.csv", row.names = F)

###deep learning
dl.model<-h2o.deeplearning(y=y.dep,x=x.dep,training_frame = train.h2o,epoch=60,
                           hidden = c(100,100),activation = "Rectifier",seed=1122)
h2o.performance(dl.model)
predict.dl=as.data.frame(h2o.predict(dl.model,test.h2o))
sub_dl <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                      Purchase = predict.dl$predict)
write.csv(sub_dl, file = "dl.csv", row.names = F)


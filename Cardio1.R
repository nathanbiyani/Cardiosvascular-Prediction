library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(ROCR)
library(caTools)
library(cowplot)
Cardio <- read.csv("Cardio_train.csv", sep = ";")
str(Cardio)
head(Cardio)

Cardio <- Cardio[, -1]

colSums(is.na(Cardio))
#no NA's 


Cardio$cholesterol <- as.factor(Cardio$cholesterol)
Cardio$gluc <- as.factor(Cardio$gluc)
Cardio$smoke <- as.factor(Cardio$smoke)
Cardio$alco <- as.factor(Cardio$alco)
Cardio$active<- as.factor(Cardio$active)
Cardio$cardio <- as.factor(Cardio$cardio)
Cardio$gender <- as.factor(Cardio$gender)

str(Cardio$cholesterol)
str(Cardio$gluc)


age <- ggplot(Cardio, aes(age)) +
  geom_histogram()

age

weight <- ggplot(Cardio, aes(weight)) +
  geom_histogram(binwidth = 5)

height <- ggplot(Cardio, aes(height)) +
  geom_histogram(binwidth = 5)

systolic_blood_pressure <- ggplot(Cardio, aes(ap_hi)) +
  geom_histogram(binwidth = 3) +
  xlim(89, 181)

diastolic_blood_pressure <- ggplot(Cardio, aes(ap_lo)) +
  geom_histogram() +
    xlim(0, 130)

ggplot(Cardio, aes(x = ap_lo, colour = cardio)) +
  geom_freqpoly(binwidth = 4) 


ggplot(Cardio, aes(x = ap_hi, colour = cardio)) +
  geom_freqpoly(binwidth = 4) 


plot_grid(age, weight, height, systolic_blood_pressure, 
          diastolic_blood_pressure, labels = "AUTO")


#checking the distribution of patients w/cardiovascular disease and
#pts w/o cardiovascular disease
CardioProportions <- Cardio %>%
  group_by(cardio) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(cardio)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

ggplot(CardioProportions, aes(x= "", y=freq, fill= cardio)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq))

#checking glucose frequencies
GlucoseProportions <- Cardio %>%
  group_by(gluc) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(gluc)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

GlucoseProportions$freq = round(GlucoseProportions$freq, 4)

glucose <- ggplot(GlucoseProportions, aes(x= "", y=freq, fill= gluc)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq)) 



#checking active frequencies
activeProportions <- Cardio %>%
  group_by(active) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(active)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

activeProportions$freq = round(activeProportions$freq, 4)


active <- ggplot(activeProportions, aes(x= "", y=freq, fill= active)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq))

#checking cholesterol frequencies
cholesterolProportions <- Cardio %>%
  group_by(cholesterol) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(cholesterol)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

cholesterolProportions$freq = round(cholesterolProportions$freq, 4)


cholesterol <- ggplot(cholesterolProportions, aes(x= "", y=freq, fill= cholesterol)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq))

#checking smoke frequencies
smokeProportions <- Cardio %>%
  group_by(smoke) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(smoke)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

smokeProportions$freq = round(smokeProportions$freq, 4)


smoke <- ggplot(smokeProportions, aes(x= "", y=freq, fill= smoke)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq))



#checking alcohol frequencies
alcoholProportions <- Cardio %>%
  group_by(alco) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  arrange(desc(alco)) %>%
  mutate(lab.ypos = cumsum(freq) - freq/2)

alcoholProportions$freq = round(alcoholProportions$freq, 4)


alcohol <- ggplot(alcoholProportions, aes(x= "", y=freq, fill= alco)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = freq))


plot_grid(glucose, active, cholesterol, smoke, alcohol, labels = "AUTO")


plot_grid(smoke, alcohol,  labels = "AUTO")




#find outliers for height
summary(Cardio$height)

Cardio <- Cardio %>%
  filter(height > 142.5) %>%
  filter(height < 186.5)


#find outliers for weight


summary(Cardio$weight)
Cardio <- Cardio %>%
  filter(weight > 39.5) %>%
  filter(weight < 107.5)

Cardio <- Cardio %>% mutate(heightInMeters = height/100)


Cardio <- Cardio %>% mutate(BMI = weight / (heightInMeters^2))

summary(Cardio$BMI)

ggplot(Cardio, aes(BMI)) +
  geom_histogram(binwidth = 2)


ggplot(Cardio, aes(x = BMI, colour = cardio)) +
  geom_freqpoly(binwidth = 10) 



#finding outliers for age -- only low outliers, not high ones

summary(Cardio$age)

Cardio <- Cardio %>%
  filter(age > 14956.5)


#making age in years instead of days for readability

summary(Cardio$age)


ggplot(Cardio, aes(x = age, colour = cardio)) +
  geom_freqpoly(binwidth = 10) 

#finding blood pressure outliers

summary(Cardio$ap_lo)

Cardio <- Cardio %>%
  filter(ap_lo > 65) %>%
  filter(ap_lo < 105)

summary(Cardio$ap_hi)


Cardio <- Cardio %>%
  filter(ap_hi > 90) %>%
  filter(ap_hi < 170)

ggplot(Cardio, aes(ap_hi, ap_lo, colour = cardio)) +
  geom_point() 




Cardio <- Cardio %>% mutate(pulsePressure = ap_hi - ap_lo)

Cardio <- Cardio %>% mutate(systolicToDiastolicRatio = ap_hi/ap_lo)


ggplot(Cardio, aes(pulsePressure)) +
  geom_histogram(binwidth = 5)


summary(Cardio$pulsePressure)

#there's still outlier for pulse pressure (negative difference) --
#this is not possible, so let's remove it

Cardio <- Cardio %>%
    filter(pulsePressure > 0)


#that must've gotten rid of the S-to-D outliers too
summary(Cardio$systolicToDiastolicRatio)

ggplot(Cardio, aes(x = pulsePressure, colour = cardio)) +
  geom_freqpoly() 


ggplot(Cardio, aes(systolicToDiastolicRatio)) +
  geom_histogram(binwidth = 0.1)

ggplot(Cardio, aes(x = systolicToDiastolicRatio, colour = cardio)) +
  geom_freqpoly(binwidth = 0.2) 






#smoking doesn't seem important to cardiovascular disease
cardioProportionsSmoking <- Cardio %>%
  group_by(smoke, cardio) %>%
  summarize(n = n()) %>%
  mutate(frequency = round(n/sum(n), 4))

cardio_smoking <- ggplot(cardioProportionsSmoking, aes(smoke, frequency,
                                     fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge")  +
   geom_text(aes(label = frequency), position=position_dodge(width=0.9), vjust = -0.25)


#glucose seems important
cardioProportionsGlucose <- Cardio %>%
  group_by(gluc, cardio) %>%
  summarize(n = n()) %>%
  mutate(frequency = round(n/sum(n), 4))

glucose_smoking <- ggplot(cardioProportionsGlucose, aes(gluc, frequency,
                                     fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = frequency), position=position_dodge(width=0.9), vjust = -0.25)


#alcohol consumption doesn't seem important 
cardioProportionsAlcohol <- Cardio %>%
  group_by(alco, cardio) %>%
  summarize(n = n()) %>%
  mutate(frequency = round(n/sum(n), 4))

alcohol_smoking <- ggplot(cardioProportionsAlcohol, aes(alco, frequency,
                                     fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = frequency), position=position_dodge(width=0.9), vjust = -0.25)

#active-ness seems mildly important, but i'm not sure -- will include anyways
cardioProportionsActive <- Cardio %>%
  group_by(active, cardio) %>%
  summarize(n = n()) %>%
  mutate(frequency = round(n/sum(n), 4))

active_smoking <- ggplot(cardioProportionsActive, aes(active, frequency, fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = frequency), position=position_dodge(width=0.9), vjust=-.25)



#cholesterol is important
cardioProportionsCholesterol <- Cardio %>%
  group_by(cholesterol, cardio) %>%
  summarize(n = n()) %>%
  mutate(frequency = round(n/sum(n), 4))

cholesterol_smoking <- ggplot(cardioProportionsCholesterol, aes(cholesterol, frequency,
                                         fill = cardio)) +
  geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = frequency), position=position_dodge(width=0.9), vjust=-.25)


plot_grid(cholesterol_smoking, active_smoking,labels = "AUTO")

plot_grid(alcohol_smoking, glucose_smoking,
          cardio_smoking,labels = "AUTO")

#split up data into test/training set

set.seed(1000)
sample = sample.split(Cardio, SplitRatio = 0.8)

train_cardio <- subset(Cardio, sample == TRUE)
test_cardio <- subset(Cardio, sample == FALSE)






#so let's include age, cholesterol, glucose, active (let's see), pulsePressure
#S-to-D ratio and BMI

model_logistic_regression <- glm(cardio ~ age + cholesterol + gluc + active + BMI + pulsePressure +
               systolicToDiastolicRatio, data = train_cardio, family = "binomial")

anova(model_logistic_regression, test='Chisq')

summary(model_logistic_regression)


exp(coef(model_logistic_regression))


cross_val_settings_logistic_regression <- trainControl(method = "cv",
                                   number = 10, 
                                   savePredictions = TRUE,
                                   classProbs = TRUE,
                                   summaryFunction = twoClassSummary)
                                   
cross_val_logistic_regression <- train(make.names(cardio) ~ age + cholesterol + gluc + active + BMI + pulsePressure +
                     systolicToDiastolicRatio, data = train_cardio,
                   family = "binomial", method = "glm", 
                   trControl = cross_val_settings_logistic_regression,
                   metric = "Sens")

cross_val_logistic_regression

pred_logistic_regression <- predict(cross_val_logistic_regression, newdata = train_cardio)
pred_logistic_regression
pred_logistic_regression <- ifelse(pred_logistic_regression == "X1", 1, 0)
pred_logistic_regression <- as.factor(pred_logistic_regression)

#F1 score -- 0.7406503




prob_model_logistic_regression <- predict(cross_val_logistic_regression, train_cardio, type = "raw")
predict_model_logistic_regression <- prediction(as.numeric(prob_model_logistic_regression), as.numeric(train_cardio$cardio))
perf_model_logistic_regression <- performance(predict_model_logistic_regression, measure = "tpr", 
                          x.measure = "fpr")

plot(perf_model_logistic_regression, col = "blue")




AUC_logistic_regression <- performance(predict_model_logistic_regression, measure = "auc")
AUC_logistic_regression@y.values



#now let's check the test set



pred_test_logistic_regression <- predict(cross_val_logistic_regression, newdata = test_cardio)
pred_test_logistic_regression <- ifelse(pred_test_logistic_regression == "X1", 1, 0)
pred_test_logistic_regression <- as.factor(pred_test_logistic_regression)

pred_test_logistic_regression
#pred_test <- as.factor(pred_test)
confusionMatrix(pred_test_logistic_regression, test_cardio$cardio)

#F1 score -- 0.7363842


prob_model_test_logistic_regression <- predict(cross_val_logistic_regression, test_cardio, type = "raw")
predict_model_test_logistic_regression <- prediction(as.numeric(prob_model_test_logistic_regression), as.numeric(test_cardio$cardio))
perf_model_test_logistic_regression <- performance(predict_model_test_logistic_regression, measure = "tpr", 
                          x.measure = "fpr")

plot(perf_model_test_logistic_regression, col = "blue")




AUC_test_logistic_regression <- performance(predict_model_test_logistic_regression, measure = "auc")
AUC_test_logistic_regression@y.values




#random forest

cross_val_settings_random_forest <- trainControl(method = "cv",
                                                 number = 5, 
                                                 savePredictions = TRUE,
                                                 classProbs = TRUE,
                                                 summaryFunction = twoClassSummary)


cross_val_random_forest <- train(make.names(cardio) ~ age + cholesterol + gluc + active + BMI + pulsePressure +
                                   systolicToDiastolicRatio, 
                                 data = train_cardio,
                                 method = "rf", 
                                 trControl = cross_val_settings_random_forest,
                                 tuneLength = 8,
                                 metric = "Sens")


cross_val_random_forest

rf <- plot(cross_val_random_forest, print.thres = 0.5, type="S")


pred_test_random_forest <- predict(cross_val_random_forest, newdata = test_cardio)
pred_test_random_forest
pred_test_random_forest <- ifelse(pred_test_random_forest == "X1", 1, 0)
pred_test_random_forest <- as.factor(pred_test_random_forest)
confusionMatrix(pred_test_random_forest, test_cardio$cardio)

prob_model_test_random_forest <- predict(cross_val_random_forest, test_cardio, type = "raw")

predict_model_test_random_forest <- prediction(as.numeric(prob_model_test_random_forest), as.numeric(test_cardio$cardio))
perf_model_test_random_forest <- performance(predict_model_test_random_forest, measure = "tpr", 
                                             x.measure = "fpr")

plot(perf_model_test_random_forest, col = "blue")


AUC_random_forest_test <- performance(predict_model_test_random_forest, measure = "auc")
AUC_random_forest_test@y.values





#knn

cross_val_knn <- train(make.names(cardio) ~ age + cholesterol + gluc + active + BMI + pulsePressure +
                         systolicToDiastolicRatio, 
                       data = train_cardio,
                       method = "knn", 
                       preProcess = c("center", "scale"),
                       metric = "Sens",
                       tuneLength = 20,
                       trControl = cross_val_settings_logistic_regression)





cross_val_knn
knn <- plot(cross_val_knn, print.thres = 0.5, type="S")




pred_test_knn <- predict(cross_val_knn, newdata = test_cardio)
pred_test_knn <- ifelse(pred_test_knn == "X1", 1, 0)
pred_test_knn <- as.factor(pred_test_knn)

confusionMatrix(pred_test_knn, test_cardio$cardio)


prob_model_test_knn <- predict(cross_val_knn, test_cardio, type = "raw")

predict_model_test_knn <- prediction(as.numeric(prob_model_test_knn), as.numeric(test_cardio$cardio))
perf_model_test_knn <- performance(predict_model_test_knn, measure = "tpr", 
                                   x.measure = "fpr")

plot(perf_model_test_knn, col = "blue")


AUC_knn_test <- performance(predict_model_test_knn, measure = "auc")
AUC_knn_test@y.values


#hard voting classifier
pred_majority<-as.factor(ifelse(pred_test_random_forest == 1 & pred_test_knn == 1, 1,
                                ifelse(pred_test_random_forest == 1 & pred_test_logistic_regression == 1, 1,
                                       ifelse(pred_test_knn == 1 & pred_test_logistic_regression == 1, 1, 0))))



confusionMatrix(pred_majority, test_cardio$cardio)



predict_model_test_majority <- prediction(as.numeric(pred_majority), as.numeric(test_cardio$cardio))
perf_model_test_majority <- performance(predict_model_test_majority, measure = "tpr", 
                                   x.measure = "fpr")

plot(perf_model_test_majority, col = "blue")


AUC_majority_test <- performance(predict_model_test_majority, measure = "auc")
AUC_majority_test@y.values



corrplot(Cardio$pulsePressure, Cardio$systolicToDiastolicRatio)


?make.names

#Introduction to caret
#source: https://lovetoken.github.io/r/machinelearning/2017/04/23/caret_package.html
install.packages("tidyverse")
install.packages("caret")
install.packages("mlbench")
library(tidyverse)
library(caret)

set.seed(1234)

data(Sonar, package = "mlbench")
str(Sonar)
dim(Sonar)
Sonar<- Sonar %>% as_tibble()
names(Sonar)
str(Sonar)

#createDataPartition(): ���������� ����� ��ȭ���� ����
table(Sonar$Class)

#sample�� �̿��� ������ ����
indexTrain <- sample(1:nrow(Sonar), round(nrow(Sonar) * .7))
training <- Sonar[ indexTrain, ]
testing  <- Sonar[-indexTrain, ]

#��ȭ����
indexTrain <-createDataPartition(Sonar$Class, p=.7, list = F)
training <-Sonar[indexTrain,]
test <-Sonar[-indexTrain,]

#k-fold cross validation
fitControl <-trainControl(method = "repeatedcv", number = 10, repeats = 5)

rf_fit <- train(Class ~ ., data = training, method = "rf", trControl = fitControl)

rf_fit

predict(rf_fit, newdata = testing) %>% confusionMatrix(testing$Class)

#Tuning parameters�� �׸��� �����ϱ�
#customgrid
customGrid <- expand.grid(mtry = 1:10)
class(customGrid)
rf_fit2 <-train(Class ~., data= training, method = "rf", trControl = fitControl, tuneGrid = customGrid)
rf_fit2
#random selection of tuning parameter combinations ���� �������� �ʰ� ȿ��������
rda_fit <-train(Class~., data = training, method = "rda", trControl = fitControl)
rda_fit

fitControl2 <-trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
rda_fit2 <-train(Class~., data = training, method = "rda", trControl = fitControl2)
rda_fit3 <-train(Class~., data = training, method = "rda", trControl = fitControl2, tuneLength = 50)
rda_fit3

#Parallel Processing
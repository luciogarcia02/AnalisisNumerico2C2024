install.packages("caret")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("sqldf")
library(sqldf)
library(caret)
library(ggplot2)
library(Hmisc)

german_credit = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

#Nombres de las columnas
colnames(german_credit) = c("chk_acct", "duration", 
                            "credit_his", "purpose", "amount", "saving_acct", 
                            "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", 
                            "housing", "n_credits", "job", "n_people", "telephone", 
                            "foreign", "response")
#Valores de clases (1 = Bueno, 2 = Malo)
german_credit$response <-
  replace(as.character(german_credit$response), 
          german_credit$response == "2", "Malo")
german_credit$response <-
  replace(as.character(german_credit$response), 
          german_credit$response == "1", "Bueno") 


table(german_credit$response)
qplot(amount, duration, color= response, data=german_credit)
prop.table(table(german_credit$foreign,german_credit$response),1)

foreigners<-sqldf("SELECT * FROM german_credit WHERE [foreign] = 'A202'")

#Valores de clases (A201 LOCAL, A202 FOREIGNER)
german_credit$foreign <- 
  replace(as.character(german_credit$foreign), 
          german_credit$foreign == "A201" , "LOCAL")
german_credit$foreign <-
  replace(as.character(german_credit$foreign), 
          german_credit$foreign == "A202", "FOREIGNER") 

# A151 : rent, A152 : own, A153 : for free
german_credit$housing <- 
  replace(as.character(german_credit$housing), 
          german_credit$housing == "A151" , "rent")
german_credit$housing <-
  replace(as.character(german_credit$housing), 
          german_credit$housing == "A152", "own") 
german_credit$housing <-
  replace(as.character(german_credit$housing), 
          german_credit$housing == "A153", "free") 

prop.table(table(german_credit$housing, 
                 german_credit$response),1)


#Proporciones de variables por cada clase
#Intervalos para las edades
hist(german_credit$age)
cutAge<-cut2(german_credit$age, g=5)
tabla <-table(cutAge, german_credit$response)
prop.table(tabla, 1)

#intervalos por duracion de los prestamos
cutDuration<-cut2(german_credit$duration, g=5)
tabla <-table(cutDuration, german_credit$response)
prop.table(tabla, 1)

#CreateDataPartition: training (80%) y test (20%)

inTrain <- createDataPartition(german_credit$response, 
                               p=0.8, list=FALSE)
training <- german_credit[ inTrain, ]
testing <- german_credit[ -inTrain, ]

#Modelo con todos los predictores
mod_fit <- train(response ~ ., data=training, 
                 method="glm", family="binomial")
mod_fit

#Predicciones con datos de testeo
pred <- predict(mod_fit, newdata=testing)
table(pred)

#Comparacion de predicciones con las clases reales
tabla <-table(pred,testing$response)
tabla
prop.table(tabla, 1)

#Modelo con menos variables
mod_fit <- train(response ~ chk_acct + duration + 
                   property+ credit_his + amount + age + job+ saving_acct
                 + purpose + sex + other_install + installment_rate, 
                 data=training, method="glm", family="binomial")
mod_fit

#Predicciones con datos de testeo
pred = predict(mod_fit, newdata=testing)
table(pred)
#Comparacion de predicciones con las clases reales
tabla <-table(pred,testing$response)
prop.table(tabla, 1)


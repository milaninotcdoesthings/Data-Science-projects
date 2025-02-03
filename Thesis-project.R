Data_visite<-Database_dipendenti_OIRM_S_Anna_anonimizzato_
#elimino i pazienti che non hanno effettuato visite
Data_visite_filtered <- Data_visite[Data_visite[, 2] != "no", ]
dim(Data_visite_filtered)
#rinomino
Data_visite<-Data_visite_filtered
#eliminazione colonne non rilevanti sfruutando l'indicizzazione
library(dplyr)
Data_visite <- Data_visite %>% select(-c(2, 4, 7, 9, 10, 15, 17, 21, 25, 26, 27, 32, 35))
Data_visite<-Data


#esempio di conversione colonna da character a boolean
Data$`Supporto psicologico` <- ifelse(is.na(Data$`Supporto psicologico`), FALSE,
                                                       Data$`Supporto psicologico`== "Sì")

#visualizzo le diverse modalità delle classi da predire
table(Data$IDONEITÁ)
#lowercasing sulla classe da predire, sfruttando il pacchetto dplyr
Data<- Data %>%
  mutate(IDONEITÁ = ifelse(IDONEITÁ == "Idoneo", "idoneo", IDONEITÁ))
#visualiziamo ora le modalità
table(Data$IDONEITÁ)

# Converto la colonna Sesso in 0 e 1

Data$Sesso<- ifelse(Data$sesso == "M", 1, 0)
table(Data$IDONEITÁ)

#correlazione tra il numero di disturbi e le tre variabili introdotte nella sezione 1.4 del progetto:

cor(Data$N.disturbi, Data$`SCORE FARMACI`)
cor(Data$N.disturbi, Data$COND.ATTUALE)
cor(Data$N.disturbi, Data$COND.PREGRESSA)

#correlazione tra le tre variabili introdotte di cui sopra
cor(Data$`SCORE FARMACI`, Data$COND.PREGRESSA)
cor(Data$`SCORE FARMACI`, Data$COND.ATTUALE)
cor(Data$COND.ATTUALE, Data$COND.PREGRESSA)

#range delle variabili
range(Data$`SCORE FARMACI`)
range(Data$COND.ATTUALE)
range(Data$COND.PREGRESSA)
range(Data$`Protocolli sanitari`)
range(Data$BMI)
range(Data$N.disturbi)
range(Data$Età)


#knn con caret
set.seed(123)
library(dplyr)
library(caTools)
library(caret)
split<-sample.split(Data$IDONEITÁ,SplitRatio = 0.7) 
train<-subset(Data,split==TRUE) 
test<-subset(Data,split==FALSE)
# Crea il modello con la funzione train
model <- train(
  IDONEITÁ ~ .,  # Formula per il modello
  data = train,  # Set di dati di addestramento
  method = "knn",  # Metodo di classificazione
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),  # Controllo dell'addestramento
  preProcess = c("scale"),  # Pre-elaborazione dei dati
  tuneLength = 20  # Numero di combinazioni di parametri da provare
)
plot(model)
model$bestTune
predicted<- model %>% predict(test)
head(predicted)
#matrice di confusione
conf_m<- table(Predicted = predicted, Actual = test$IDONEITÁ)
conf_m
sum(diag(conf_m)/sum(conf_m))



#Naive Bayes
set.seed(120)
library(tidyverse)
library(e1071)
library(caTools)
library(caret)
split<-sample.split(Data$IDONEITÁ, SplitRatio = 0.7)
train_data<-subset(Data, split == "TRUE")
test_data<-subset(Data, split == "FALSE")
nb_classifier<-naiveBayes(IDONEITÁ ~ ., data = train_data)
nb_classifier
#applichiamo il classificatore ai dati di test
nb_predictions<-predict(nb_classifier, newdata = test_data)
#Matrice di confusione
nb_conf_m<-table(test_data$IDONEITÁ, nb_predictions)
nb_conf_m
#Accuracy
confusionMatrix(nb_conf_m)
sum(diag(nb_conf_m))/sum(nb_conf_m)

#support vector machine
set.seed(123)
library(e1071)
library(caTools)
#impiego di un kernel lineare
if(!is.factor(Data$IDONEITÁ)) {
  Data$IDONEITÁ <- as.factor(Data$IDONEITÁ) #convertiamo idoneità in factor per classificare
}
split<-sample.split(Data$IDONEITÁ, SplitRatio = 0.7)
train_data_svm<-subset(Data, split =="TRUE")
test_data_svm<-subset(Data, split =="FALSE")


svm_model <- svm(IDONEITÁ ~ ., data = train_data_svm, kernel = "linear")
summary(svm_model)
pred = predict(svm_model, test_data_svm) #predictions
tab = table(Predicted=pred, Actual = test_data_svm$IDONEITÁ) #matrice di confusione
tab
#percentuale di valori corretti
sum(diag(tab)/sum(tab))

#impiego di un kernel polinomiale
svm_model_pol<-svm(IDONEITÁ ~ ., data = train_data_svm, kernel = "polynomial")
summary(svm_model_pol)
pred_pol = predict(svm_model_pol, test_data_svm)
tab_pol = table(Predicted=pred, Actual = test_data_svm$IDONEITÁ)
tab_pol
sum(diag(tab_pol))/sum(tab_pol)



#albero di classificazione
set.seed(123)
library(rpart)
library(rpart.plot)
library(caTools)
params <- rpart.control(minsplit = 20, minbucket = round(20/3), 
                        maxdepth = 20, maxcompete = 20,
                        cp = 0.01, xval = 20)
mytree<-rpart(IDONEITÁ~ ., data = Data, method = "class", 
              control = params, parms = list(split = "information"))
print(mytree)
rpart.plot(mytree)
#previsione
predicted_tree<-predict(mytree, Data, type = "class")
Data$predicted<-predicted_tree
conf_matrix_tree<-table(Data$IDONEITÁ, Data$predicted)
conf_matrix_tree
accuracy_test<-sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
accuracy_test

#random forest
set.seed(12345)
library(randomForest)
library(tidyverse)
library(caTools)

# Suddivisione del dataset
split <- sample.split(Data$IDONEITÁ, SplitRatio = 0.7)
train <- subset(Data, split == TRUE)
test <- subset(Data, split == FALSE)

# Mi assicuro che la variabile di risposta sia un fattore
train$IDONEITÁ <- factor(train$IDONEITÁ)

# Rinomino le variabili se necessario
names(train) <- make.names(names(train))

# Addestro il modello Random Forest
set.seed(123)
model <- randomForest(IDONEITÁ ~ Età + Sesso + BMI + N.disturbi + SCORE.FARMACI + COND.ATTUALE + COND.PREGRESSA,
                      data = train, importance = TRUE, ntree = 1000)
model
importance(model)
sum(is.na(Data))

#k-means
set.seed(123)
library(tidyverse)
library(cluster)
library(factoextra)
distance<-get_dist(Data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
Data_2 <- Data[sapply(Data, is.numeric)] #considero solo le variabili numeriche del dataset
Data_2<-scale(Data_2)
k2<-kmeans(Data_2, centers = 2, nstart = 25) #partiamo con due centroidi e 25 configurazioni distinte
str(k2)
k2
fviz_cluster(k2, data = Data_2) #grafico rispetto alle componenti principali
#metodo Elbow per numero ottimale di cluster
fviz_nbclust(Data_2,kmeans, method = "wss")
#metodo silhouette
fviz_nbclust(Data_2,kmeans,method = "silhouette")
#metodo gap statistics
gap_stat<-clusGap(Data_2, FUNcluster = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
#k-means con 4 cluster
final_model<-kmeans(Data_2, 4, nstart = 25)
print(final_model)
fviz_cluster(final_model, data = Data_2)
distances<-dist(Data_2)
silhouette_scores <- silhouette(k2$cluster, distances)
summary(silhouette_scores)
mean_silhouette <- mean(silhouette_scores[, 3])
print(mean_silhouette)



#clustering gerarchico
set.seed(1234)
library(tidyverse)
library(cluster)
library(factoextra)
mydata<-scale(Data_2) 
distance_h<-get_dist(mydata)
h_cluster<-hclust(distance_h, method = "ward.D2")
plot(h_cluster, cex = 0.6, hang = -1) #grafico dendrogramma
#taglio l'albero in 4 gruppi
sub_grp<-cutree(h_cluster, k = 4)
fviz_cluster(list(data = mydata, cluster = sub_grp))
print(h_cluster)
# Calcolo della matrice di distanza
dist_matrix <- dist(Data_2)
# Esecuzione di hclust
hc <- hclust(dist_matrix, method = "average")
# Calcolo del Silhouette Score
sil <- silhouette(cluster_labels, dist_matrix)
# Stampa del risultato
print(sil)
# Visualizzazione del Silhouette Plot
plot(sil, main = "Silhouette plot for hierarchical clustering")

# Calcolo del Silhouette Score medio
avg_silhouette <- mean(sil[, 3])
cat("Average Silhouette Width:", avg_silhouette, "\n")

# algoritmo Apriori
set.seed(123)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(tidyverse)
rules<-apriori(Data, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
summary(rules)
#risultati
inspect(head(sort(rules, by = "lift"),5))
inspect(sort(sort(rules, by = "support"), by= "confidence")[1:5])

#regressione logistica
set.seed(123)
library(dplyr)
library(MASS)
library(caTools)
# Riduciamo il problema a una classificazione binaria
data_log <- Data %>%
  mutate(IDONEITÁ = ifelse(IDONEITÁ %in% c("idoneo con limitazioni o prescrizioni",
                                           "idoneo con limitazioni e prescrizioni"),
                           "idoneo con", IDONEITÁ))
# Convertire la variabile di risposta in valori binari
data_log <- data_log %>%
  mutate(IDONEITÁ = ifelse(IDONEITÁ == "idoneo con", 1, 0))
split <- sample.split(data_log$IDONEITÁ, SplitRatio = 0.75)
train_log <- subset(data_log, split == TRUE)
test_log <- subset(data_log, split == FALSE)
table(train_log$IDONEITÁ)
table(test_log$IDONEITÁ)
# Eseguire la regressione logistica sul training set
model_log <- glm(IDONEITÁ ~ ., family = binomial, data = train_log)
# Visualizzare il sommario del modello
summary(model_log)
#coefficienti del modello
exp(coef(model_log))
#probabilità previste
train_log$probs = predict(model_log, type = "response")
head(train_log)
#matrice di confusione
train_log$predict = rep("idoneo", 525)
train_log$predict[train_log$probs>0.5]="idoneo con"
table<-table(train_log$predict, train_log$IDONEITÁ)
table
sum(diag(table)/sum(table))
test_log$prob = predict(model_log, newdata = test_log, type = "response")
test_log$predict = rep("idoneo" , 175)
test_log$predict[test_log$prob>0.5]="idoneo con"
table_2<-table(test_log$predict, test_log$IDONEITÁ)
table_2
sum(diag(table_2)/sum(table_2))

#ora si va su KNIME!
write_xlsx(Data, path = "/Users/macbookair/desktop/TESI/knime_data.xlsx")



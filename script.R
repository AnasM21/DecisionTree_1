#load the data
produit <- read.csv("Data Produit.csv", header = TRUE, sep = ",", dec = ".")
# describe the table
str(produit)
#every element with its occurence
table(produit$Produit)
#training data
produit_EA <- produit[1:400,]
# test data
produit_ET <- produit[401:600,]
#delete the id column
produit_EA <- produit_EA[,-1]
#2nd methode to delete it
#produit_EA <- subset(produit_EA, select = -ID)
# the summary of the dataframe 
summary(produit)
#installing and dl the rpart package for the decision tree
install.packages("rpart")
library("rpart")
#creating the tree with the default params
# .=> means that all the values or the variables of the dataframe are pedictive ones and Produit is the variable to be predicted
tree1 <- rpart(Produit ~ .,produit_EA)
#another way to do it is
#tree2 <- rpart(Produit ~ Age + Sexe + Habitat + Revenus + Marie+ Enfants + Emprunt, produit_EA)
rm(tree2)
#another way to do it
# tree2 <- rpart(Produit ~ Age + Sexe + Habitat + Revenus + Marie+ Enfants + Emprunt, produit_EA)
# by writing all the variables isntead of the dot "."
plot(tree1)
text(tree1,pretty=0)
#evaluate the perf of the tree
test_tree <- predict(tree1,produit_ET,type="class")
#vector generated
test_tree
# Nbr of Oui/Non
table(test_tree)
# add the predict column to the test table to compare
produit_ET$Prediction <- test_tree
View(test_tree)
#compare the predicitons and the class variable "Produit"
produit_ET[produit_ET$Produit==produit_ET$Prediction, ]
# number of times where the prediction is correct
nbr_succes <- nrow(produit_ET[produit_ET$Produit==produit_ET$Prediction,])
# number of failures , wrong predictions
nbr_echec <- length(produit_ET[produit_ET$Produit!=produit_ET$Prediction,"ID"])
# ratio of success
taux_succes <- nbr_succes/nrow(produit_ET)
# ratio of failures
taux_echec <- nbr_echec/nrow(produit_ET)
# using the tree for the prediction
produit_pro <- read.csv("Data Produit Prospects.csv", header = TRUE, sep = ",",
dec = ".")
str(produit_pro)
pred_tree1 <- predict(tree1, produit_pro, type="class")
table(pred_tree1)
produit_pro$Prediction <- pred_tree1
produit_pro_oui <- produit_pro[produit_pro$Prediction=="Oui",]
produit_pro_non <- produit_pro[produit_pro$Prediction=="Non",]


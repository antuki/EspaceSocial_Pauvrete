library(glmnet)
library(dplyr)

bdd_lasso <- bdd
for(col in setdiff(colnames(bdd_lasso),c("ident"))){
  if(is.character(bdd_lasso[[col]])){
      modalites <- unique(bdd_lasso[[col]])[-1]
      for(mod in modalites){
        nom_var = paste0("LASSO_",col,"-",mod)
        bdd_lasso[[nom_var]] = 1*c(bdd_lasso[[col]]==mod)
      }
  } else{
    nom_var = paste0("LASSO_",col)
    bdd_lasso[[nom_var]] = bdd_lasso[[col]]
  }
}
bdd_lasso <- bdd_lasso %>% 
  mutate(LASSO_y = factor(ifelse(subj_risque_pauvrete=="Je me considère déjà\ncomme pauvre",1,0))) %>%
  select(-LASSO_subj_risque_pauvrete) %>% 
  select(starts_with("LASSO"))


  
  
#On enlève les colonnes avec valeurs manquantes : sdstatemp sdnbenf og2_
for(col in colnames(bdd_lasso)){
  if(any(is.na(bdd_lasso[[col]]))){
    bdd_lasso[[col]] <- NULL
  }
}


select_train = sort(sample(nrow(bdd_lasso), nrow(bdd_lasso)*.7))
train<-bdd_lasso[select_train,]
test<-bdd_lasso[-select_train,]

# Modèle entier sans pénalisation (trop complexe à faire tourner : bug)
#glm <- glm(LASSO_y ~., data=train,family = "binomial")

# Find the best lambda using cross-validation : marche mais choix du lambda fait bugger la suite
#set.seed(123) 
#cv.lasso <- cv.glmnet(x=as.matrix(train[,-ncol(train)]), y=train$LASSO_y, alpha=1, family="binomial")
#plot(cv.lasso)
#cv.lasso$lambda.min
#cv.lasso$lambda.1se

# Note alpha=1 for lasso only and alpha=0 ridge only.
for(lambda in c(0.9,0.5,0.1,0.05,0.04,0.03,0.02, 0.01)){ #0.01
  print(paste0("Valeur de la pénalisation : ", lambda))
  lasso.model <- glmnet(x=as.matrix(train[-ncol(train)]), y=train$LASSO_y, alpha=1, family="binomial",lambda = lambda) #cv.lasso$lambda.min
  myCoefs <- coef(lasso.model, s=0)
  myCoefs_nonnuls <- myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ][-1]
  print(paste0("Nombre de variables retenues : ", length(myCoefs_nonnuls)))
  if(length(myCoefs_nonnuls)!=0){
    print(do.call(c,lapply(myCoefs_nonnuls,colnames_to_nomexplicite)))
    probabilities <- lasso.model %>% predict(newx = as.matrix(test[,-ncol(test)]))
    print(paste0("Pourcentages de bien prédits : ", round(100*mean(ifelse(probabilities > 0.5, 1, 0) == test$LASSO_y))," %"))
  }
  cat("\n\n")
}

myResults <- data.frame(
  features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], #intercept included
  coefs    = myCoefs              [ which(myCoefs != 0 ) ]  #intercept included
) %>% mutate(absv=abs(coefs)) %>% arrange(desc(absv)) %>% select(-absv)
myResults
#plot(lasso.model, xvar="lambda")
#probabilities <- lasso.model %>% predict(newx = as.matrix(test[,-ncol(test)]))
#mean(ifelse(probabilities > 0.5, 1, 0) == test$LASSO_y)


rm(list=ls())

library(Hmisc)
library(dplyr)
library(haven)

#indiquer le chemin du fichier
barometre2000_2019_diff<-read.csv(file="data/2019/barometre2000_2019_diff.csv", sep=";", colClasses = "character", na.strings="")
# pour attribuer des libellés aux variables et aux modalités de variables des fichiers en interne 

# Transformation de certaines variables en numérique
for(var in c("poids","sdage","sdageenf_1","sdageenf_2",
             "sdageenf_3","sdageenf_4","sdageenf_5","sdageenf_6",
             "sdageenf_7","sdageenf_8","sdageenf_9","sdageenf_10",
             "sduc","in11_1","in11_2","pe7","pe16","re4", "re5", "re16",
             "re6","sdrevcl", "sdrevcl_imput","sdniviecl_imput")){
  barometre2000_2019_diff[[var]] <- as.numeric(gsub(",",".",barometre2000_2019_diff[[var]]))
}
#chargement des dicos
# indiquer le chemin de la base des dicos 
dico <- "data/2019/"
dico_var<- read.table(paste0(dico,"dico_variable2019_diff.csv"),sep = ";", header = TRUE, stringsAsFactors = FALSE,quote="",fill=FALSE )
dico_lib<- read.table(paste0(dico,"dico_libelle2019_diff.csv"), sep = ";", header = TRUE, stringsAsFactors = FALSE,quote="",fill=FALSE)

#passage des variables au bon format
dico_lib$mod<-as.numeric(as.character(dico_lib$mod))
dico_lib$variable<-(as.character(dico_lib$variable))
dico_lib$libelle<-(as.character(dico_lib$libelle))

# Etape ajoutée pour debugger : variables supprimees dans la base diffusee

# Erreurs dans la base de diffusion avec des espaces en trop pour certaines variables 
dico_var$var[which(dico_var$var%in%c("sdrevcl_imput ","sdniviecl_imput ","sdnivie "))] <- c("sdrevcl_imput","sdniviecl_imput","sdnivie") 

# variables non diffusées qui font bugger le programme, on les enlève
var_nondiff <- setdiff(dico_var$var,colnames(barometre2000_2019_diff))
dico_var <- dico_var[which(!dico_var$var%in%var_nondiff),]

# Fin de l'étape ajoutée

# Attribuer un label aux noms de COLONNES
for (i in 1:dim(dico_var)[1])
{
  eval(parse(text = paste("attr(barometre2000_2019_diff$",dico_var$var[i],",'label')<-'",dico_var$lib[i],"'",sep="")))
}

# Attribuer un label aux noms de MODALITES
for (i in 1:length(unique(dico_lib$variable)))
{
  var<-unique(dico_lib$variable)
  eval(parse(text = paste("test<-dico_lib[which(dico_lib$variable=='",var[i],"'),]",sep="")))
  eval(parse(text = paste("attr(barometre2000_2019_diff$",var[i],",'labels')<-setNames(c(test$mod), c((test$libelle)))",sep="")))
}


# Ajout de la variable PE15
bdd2018 <- read.csv(file="data/2018/barometre2000_2018_diff.csv", sep=";", dec=",",colClasses = "character", na.strings="") %>% 
  select(ident,pe15)
attr(bdd2018$pe15,"label") <- "PE15. Actuellement, compte tenu de votre situation globale, du montant des aides publiques (RSA, allocations familiales, aides au logement), et du montant de vos impôts, vous considérez que"
attr(bdd2018$pe15,"labels") <- 1:4 %>% magrittr::set_names(c("Vous êtes suffisamment aidé(e) par les pouvoirs publics, ou n’avez pas besoin d’être aidé(e)", "Vous auriez besoin d’être aidé(e) davantage par les pouvoir publics","Non concerné(e)","NSP"))

barometre2000_2019_diff <- barometre2000_2019_diff %>%
  left_join(bdd2018,by = "ident") 


# Export en RDS
saveRDS(barometre2000_2019_diff, "data/2019/barometre2000_2019_diff.rds")


#### Création de la base de 2017 pour reproduire l'étude de Duvoux et Papuchon

barometre2000_2017_papuchon<-read.csv(file="data/2017/baro_drees_unif0017_queteletv2.csv", sep=";", colClasses = "character", na.strings="")

# Transformation de certaines variables en numérique
for(var in c("poids","sdage","sdageenf_1","sdageenf_2",
             "sdageenf_3","sdageenf_4","sdageenf_5","sdageenf_6",
             "sdageenf_7","sdageenf_8","sdageenf_9","sdageenf_10",
             "sduc","in11_1","in11_2","pe7","pe16","re4", "re5", "re16",
             "re6","sdrevcl")){ #"sdrevcl_imput","sdniviecl_imput"
  barometre2000_2017_papuchon[[var]] <- as.numeric(gsub(",",".", barometre2000_2017_papuchon[[var]]))
}

# Ajout de la variable PE15
bdd2016 <- read.csv(file="data/2016/baro_drees_unif0016_queteletv1.csv", sep=";", dec=",",colClasses = "character", na.strings="") %>% 
  select(ident,pe15)

barometre2000_2017_papuchon <-barometre2000_2017_papuchon %>%
  left_join(bdd2016,by = "ident") 


saveRDS(barometre2000_2017_papuchon, "data/2017/barometre2000_2017_papuchon.rds")

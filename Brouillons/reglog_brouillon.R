library(fastDummies)

bdd_logit <- bdd %>% 
  mutate(quantile_nivie = factor(
    cut(sdrevcl_imput_moy/sduc, include.lowest=TRUE,
        breaks=quantile(sdrevcl_imput_moy/sduc,
                        probs=seq(0,1,0.2),na.rm=TRUE),
        labels=1:5),
    levels=c(2,1,3,4,5),
    labels=paste0("-Quintile ",c(2,1,3,4,5)))
  ) %>% 
  mutate(
    statact = factor(case_when(
      sdsitua==1 & sdstatemp%in%c(1)   ~ 1,
      sdsitua%in%c(2,3) | (sdsitua==1 & sdstatemp%in%c(NA,2,3,4,5)) ~ 2,
      sdsitua==4 ~ 3,
      sdsitua==5 ~ 4,
      sdsitua==6 ~ 5,
      sdsitua==7 ~ 6,
      TRUE ~ NA_real_
    ),
    levels=1:6,
    labels=paste0("-",
                  c("CDI temps plein",
                    "Emploi précaire ou à temps partiel",
                    "Recherche d’emploi",
                    "Étudiant",
                    "Retraité",
                    "Aucune activité professionnelle"))
    )) %>% 
  mutate(prof = factor(
    #ifelse(sdpcs7!=7,sdpcs7,NA),
    sdpcs7,
    levels=c(4,1,2,3,5,6,7),
    labels=paste0("-",c("Profession intermédiaire","Agriculteur",
                        "Artisan commerçant","Cadre supérieur, profession libérale",
                        "Employé","Ouvrier","Autre inactif"))
  )) %>%
  dummy_cols(select_columns = 'prof') %>% 
  mutate(
    diplome = factor(case_when(
      sddipl%in%c(1,2,3,4) ~ 1,
      sddipl%in%c(5,6) ~ 2,
      sddipl%in%c(7) ~ 3,
      sddipl%in%c(8) ~ 4,
      TRUE ~ NA_real_
    ),
    levels=c(2,1,3,4),
    labels=paste0("-",c("Baccalauréat","CAP, BEP ou moins","Bac + 2","Bac + 3 ou plus"))
    )) %>% 
  mutate(aide_log = factor(
    ifelse(sdres_9!=3,sdres_9,NA),
    levels=c(2,1),
    labels=paste0("-",c("Pas d'aide au logement","Aide au logement reçue"))
  )) %>% 
  mutate(aide_rsa = factor(
    ifelse(sdres_3!=3,sdres_3,NA),
    levels=c(2,1),
    labels=paste0("-",c("Pas de RSA","RSA reçu"))
  )) %>% 
  mutate(aide_handi = factor(
    ifelse(sdres_10!=3,sdres_10,NA),
    levels=c(2,1),
    labels=paste0("-",c("Pas d'alloc. hand./invalid./dépend.","Alloc. hand./invalid./dépend. reçu"))
  )) %>% 
  mutate(sexe = factor(
    ifelse(sdsexe!=3,sdsexe,NA),
    levels=c(2,1),
    labels=paste0("-",c("Femme","Homme"))
  )) %>% 
  mutate(age_tranche = factor(
    cut(sdage,
        breaks=c(18,30,40,50,60,70,120),
        include.lowest=TRUE,
        labels=1:6),
    levels=c(2,1,3,4,5,6),
    labels=paste0("-",c("30 à 39 ans","18 à 29 ans","40 à 49 ans",
                        "50 à 59 ans","60 à 69 ans", "70 ans et plus")))
  ) %>% 
  mutate(
    vie_fam = factor(case_when(
      sdsitfam==1 ~ 1,
      sdsitfam==2 & sdnbenf==0 ~ 2,
      sdsitfam==2 & sdnbenf!=0 ~ 3,
      sdsitfam==3 ~ 4,
      sdsitfam==4 ~ 5,
      sdsitfam%in%c(5,6,7) ~ 6,
      TRUE ~ NA_real_
    ),
    levels=c(2,1,3,4,5,6),
    labels=paste0("-",
                  c("Membre du couple (pas d’enfants à charge)","Vit seul",
                    "Membre du couple (enfants à charge)", "Chef famille monoparentale",
                    "Enfant","Autre"))
    )) %>% 
  mutate(
    statut_occup = factor(case_when(
      lo1%in%c(2,3,4)  ~ 1,
      lo1==1 ~ 2,
      lo1==5 ~ NA_real_
   ),
    levels=c(1,2),
    labels=paste0("-",
                  c("Locataire ou hébergé","Propriétaire")) 
    )) %>% 
  mutate(annee_fac = factor(annee,labels=paste0("-",2015:2017))) %>% 
  mutate(pauvrete_mon_rel= factor(
    ifelse(sdrevcl_imput_moy/sduc<seuil_pauvrete,3,
                                  ifelse(sdrevcl_imput_moy/sduc>=1.2*seuil_pauvrete,1,
                                         ifelse(sdrevcl_imput_moy/sduc<1.2*seuil_pauvrete & sdrevcl_imput_moy/sduc>=seuil_pauvrete ,2,NA
                                         )
                                  )),
    levels=c(1,2,3),
    labels=paste0("-",c("Revenus > 20 pourcents seuil pauvreté","Entre le seuil et 20 pourcents au dessus", "Pauvreté monétaire"))
  )
  )

reg1 <- glm(subj_pauvrete ~ quantile_nivie + statact + 
             #prof +
             `prof_-Agriculteur` + `prof_-Artisan commerçant` +
             `prof_-Cadre supérieur, profession libérale` + 
             `prof_-Employé` + `prof_-Ouvrier` +
             diplome + aide_log +
             aide_rsa + aide_handi + sexe + age_tranche + vie_fam +
             statut_occup + annee_fac,
           data = bdd_logit %>% select(`subj_pauvrete`, `quantile_nivie`, `statact`, `prof`,`prof_-Agriculteur`,`prof_-Artisan commerçant`, `prof_-Cadre supérieur, profession libérale`,
                                       `prof_-Employé`, `prof_-Ouvrier`, 
                                       `diplome`, `aide_log`, `aide_rsa`,
                                       `aide_handi`, `sexe`, `age_tranche`,
                                       `vie_fam`, `statut_occup`, `annee_fac`
                                       ) %>% tidyr::drop_na(),
           family = binomial(logit))
results1 <- summary(reg1)$coefficients
modalites1 <- c("(Intercept)")
for(var in c("quantile_nivie", "statact", "prof", 
             "diplome", "aide_log", "aide_rsa",
             "aide_handi", "sexe", "age_tranche",
             "vie_fam", "statut_occup", "annee_fac")){
  modalites1 <- c(modalites1,gsub("-","",levels(bdd_logit[,var])))
  
}

df_model1 <- data.frame(noms= c("-(Intercept)",row.names(results1)[-1]),
           valeur=round(results1[,1],2),
           pvaleur=cut(results1[,4], breaks = c(0, 0.001, 0.01, 0.05, 1), include.lowest = T, labels = c('***', '**', '*', NA)),
           row.names=NULL
) %>% mutate(noms = gsub("`","",noms)) %>%  
  tidyr::separate(noms,c("variable","modalite"),sep="-") %>% 
  mutate(odds=ifelse(is.na(pvaleur),NA,round(exp(results1[,1]),2))) %>% 
  right_join(data.frame(modalite=modalites1),by="modalite") %>% 
  arrange(match(modalite, modalites1))

R2_ajuste1 <- with(summary(reg1), 1 - deviance/null.deviance)
N1 <- length(summary(reg1)$deviance.resid)


#On remplace la tranche d'âge par l'âge
#On remplace les quantiles de niveau de vie par pauvrete monétaire relative
#On enlève l'assistance
reg2 <- glm(subj_pauvrete ~ sdage + pauvrete_mon_rel + diplome + statact +
              sexe + `prof_-Agriculteur` + `prof_-Artisan commerçant` +
              `prof_-Cadre supérieur, profession libérale` + 
              `prof_-Employé` + `prof_-Ouvrier` + vie_fam +  statut_occup + annee_fac,
        data = bdd_logit %>% select(`subj_pauvrete`, `sdage`,  `pauvrete_mon_rel`, `statact`, `prof`,`prof_-Agriculteur`,`prof_-Artisan commerçant`, `prof_-Cadre supérieur, profession libérale`,
                                        `prof_-Employé`, `prof_-Ouvrier`, 
                                        `diplome`, `sexe`,
                                        `vie_fam`, `statut_occup`, `annee_fac`
            ) %>% tidyr::drop_na(),
            family = binomial(logit))

results2 <- summary(reg2)$coefficients
modalites2 <- c("(Intercept)","sdage")
for(var in c("sdage","pauvrete_mon_rel","diplome", "statact", "sexe",
             "prof","vie_fam","statut_occup", "annee_fac")){
  modalites2 <- c(modalites2,gsub("-","",levels(bdd_logit[,var])))
  
}

df_model2 <- data.frame(noms= c("-(Intercept)","-sdage",row.names(results2)[-c(1,2)]),
                        valeur=round(results2[,1],2),
                        pvaleur=cut(results2[,4], breaks = c(0, 0.001, 0.01, 0.05, 1), include.lowest = T, labels = c('***', '**', '*', NA)),
                        row.names=NULL
) %>% mutate(noms = gsub("`","",noms)) %>%  
  tidyr::separate(noms,c("variable","modalite"),sep="-") %>% 
  mutate(odds=ifelse(is.na(pvaleur),NA,round(exp(results2[,1]),2))) %>% 
  right_join(data.frame(modalite=modalites2),by="modalite") %>% 
  arrange(match(modalite, modalites2))

R2_ajuste2 <- with(summary(reg2), 1 - deviance/null.deviance)
N2 <- length(summary(reg2)$deviance.resid)








table(bdd_logit$quantile_nivie,useNA = "ifany") #412
table(bdd_logit$statact,useNA = "ifany") #0
table(bdd_logit$prof,useNA = "ifany") #0
table(bdd_logit$diplome,useNA="ifany") #90
table(bdd_logit$aide_log,useNA="ifany") #12
table(bdd_logit$aide_rsa,useNA="ifany") #0
table(bdd_logit$aide_handi,useNA="ifany") #1
table(bdd_logit$sexe,useNA="ifany") #0
table(bdd_logit$age_tranche,useNA="ifany") #0
table(bdd_logit$vie_fam,useNA="ifany") #0
table(bdd_logit$statut_occ,useNA="ifany") #1
table(bdd_logit$annee_fac,useNA="ifany") #0

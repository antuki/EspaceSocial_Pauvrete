rm(list=ls())

library(dplyr)
# 
# 
# table(barometre2000_2019_diff$sdnivie_imput)
# barometre2000_2019_diff %>% select(contains("imput")) %>% colnames
# 
# summary(barometre2000_2019_diff$sdnivie)
# 
# head(barometre2000_2019_diff$sdniviecl_imput)
# 
# attr(barometre2000_2019_diff$sdniviecl_imput,"label")

barometre2000_2019_diff<-readRDS(file="data/2019/barometre2000_2019_diff.RDS")

barometre_latent <- barometre2000_2019_diff %>% 
  filter(annee%in%2017:2019) %>% #keep only the 3 last vagues of the barometre 
  mutate(subj_diff_mini_vivre = sdrevcl_imput - pe7) %>% 
  select(ident,annee,poids,sdnivie,sdres_1,sdres_6,sdres_7, sdres_2,sdres_3,sdres_4,sdres_9,sdres_11, sdstatemp, sdsitua,lo1,sddipl,pe3,subj_diff_mini_vivre) %>% 
  setNames(c("ident","annee","poids", "mat_nivie", "mat_salaire_menage", "mat_rev_financiers",
"mat_rev_locatifs", "preca_salaire_indp_menage", "preca_RSA_menage", "preca_chom_menage",
"preca_alloc_logement", "preca_bourse_etude", "preca_type_contrat", "preca_temps_partiel",
"preca_statut_occ_log", "preca_nivdipl", "subj_risque_pauvrete", "subj_diff_mini_vivre")
)

saveRDS(barometre_latent, "data/2019/barometre_latent_2019.rds")


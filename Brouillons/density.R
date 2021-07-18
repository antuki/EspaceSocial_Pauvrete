#Un quart des Français ont un niveau de vie de moins de 1000 euros. C'est #le cas de trois quart des personnes recevant le RSA au sein de leur #ménage. Parmi les ménages au RSA, seul un couple sans enfant sur 2 et #près de 9 monoparents sur 10 ont un niveau de vie inférieur à 1000 #euros. En d'autres termes, les monoparents sont monétairement plus #pauvres que les parents de couple sans enfant parmi les ménages au RSA. 
gg_ecdf_viefam <- function(df, bdd_log=bdd_logit, sous_champ="Titre", legend_only=FALSE, legend.position="none"){
  #Empirical Cumulative Distribution Function
  p <- ggplot() + 
    stat_ecdf(data=bdd_log,aes(x=nivie, linetype="2"), size=1)+
    stat_ecdf(data=df %>%
                filter(!vie_fam%in%c("-Autre situation familiale")) %>%
                mutate(vie_fam = gsub("-","",vie_fam)) %>%         
                mutate(vie_fam = factor(vie_fam,
                                        levels=c("Vit seul",
                                                 "Membre du couple (pas d’enfants à charge)",
                                                 "Membre du couple (enfants à charge)",
                                                 "Chef famille monoparentale",
                                                 "Enfant"),
                                        labels=c("Vit seul",
                                                 "Membre du couple\n(pas d’enfants\nà charge)",
                                                 "Membre du couple\n(enfants\nà charge)",
                                                 "Chef famille\nmonoparentale",
                                                 "Enfant")
                )),
              aes(x=nivie, color=vie_fam), size=1, geom="line") +
    stat_ecdf(data=df,aes(x=nivie, linetype="1"), geom="line", size=1)+
    scale_color_brewer(palette="Set2")+
    scale_linetype_discrete(breaks=c("2","1"),labels=c("Ensemble des Français","Population spécifique (titre)"))+
    theme_minimal()+
    scale_x_continuous(name ="Niveau de vie (euros/mois)", limits=c(0,5000))+
    scale_y_continuous(name ="Part de la population concernée",
                       labels=scales::percent)+
    ggtitle(sous_champ) +
    guides(linetype = guide_legend(title="Champ",order = 1),
           color = guide_legend(title="Situation\nfamiliale", order = 2)
    )+
    theme(title=element_text(size=8, face="bold"))
  
  if(legend_only){
    p <- p +  
      theme(legend.box="vertical", legend.position = "top",
            legend.spacing.x = unit(0.2, 'cm'))
    p <- cowplot::get_legend(p)
  } else{
    p <- p+  theme(legend.position=legend.position)
    
  }
  
  return(p)
  
  
}
leg <- gg_ecdf_viefam(df=bdd_logit %>%  filter(presta_rsa=="-RSA"), sous_champ="Français qui bénéficient du RSA", legend_only = TRUE)
p1 <- gg_ecdf_viefam(df=bdd_logit %>%  filter(presta_rsa=="-RSA"), sous_champ="Français qui bénéficient du RSA")
p2 <- gg_ecdf_viefam(df=bdd_logit %>%  filter(presta_apl=="-APL"), sous_champ="Français qui bénéficient d'APL")
p3 <- gg_ecdf_viefam(df=bdd_logit %>%  filter(subj_pauvrete==TRUE), sous_champ="Français qui se sentent pauvres")
p4 <- gg_ecdf_viefam(df=bdd_logit %>%  filter(subj_inf_mini_decla==TRUE), sous_champ="Français se sentant en difficulté fin.") #"-Rev. inf. au minimum décla."
library(cowplot)

gg_ecdf <- plot_grid(NULL,NULL,p1,p2,p3,p4,nrow=3, rel_heights = c(1,3,3))+
  draw_plot(leg,x=0,y=0.45)

gg_ecdf

gg_ecdf_viefam(df=bdd_logit, sous_champ="Tous les Français")



#########
gg_taux_viefam <- function(df, legend_only=FALSE,
                           legend.position="none", sous_champ="Titre"){
  df <- df %>% 
    filter(!vie_fam%in%c("-Autre situation familiale")) %>%
    mutate(vie_fam = gsub("-","",vie_fam)) %>%         
    mutate(vie_fam = factor(vie_fam,
                            levels=c("Vit seul",
                                     "Membre du couple (pas d’enfants à charge)",
                                     "Membre du couple (enfants à charge)",
                                     "Chef famille monoparentale",
                                     "Enfant"),
                            labels=c("Vit seul",
                                     "Membre du couple\n(pas d’enfants\nà charge)",
                                     "Membre du couple\n(enfants\nà charge)",
                                     "Chef famille\nmonoparentale",
                                     "Enfant")
    )) %>% 
    group_by(vie_fam) %>%
    arrange(nivie) %>% 
    mutate(denom = cumsum(poids*ifelse(is.na(indicatrice),0,1))) %>% 
    mutate(num=cumsum(poids*ifelse(is.na(indicatrice) | !indicatrice,0,1))) %>% 
    mutate(ratio=num/denom) %>% 
    select(vie_fam,presta_rsa,poids,nivie,num,denom,ratio)
  
  p <- ggplot(df, aes(x=nivie, y=ratio, group=vie_fam, color=vie_fam)) +
    #geom_line() +
    geom_smooth(method = "loess", size=1)+ #lm, glm, loess, NULL, gam
    scale_color_brewer(palette="Set2")+
    theme_minimal()+
    scale_x_continuous(name ="Niveau de vie de moins de ... (euros/mois)", limits=c(500,3000))+
    scale_y_continuous(name ="Part de la population concernée",
                       limits=c(0,1),
                       labels=scales::percent)+
    ggtitle(sous_champ) +
    guides(linetype = guide_legend(title="Champ",order = 1),
           color = guide_legend(title="Situation\nfamiliale", order = 2)
    )+
    theme(title=element_text(size=8, face="bold"))

  if(legend_only){
    p <- p +  
      theme(legend.box="vertical", legend.position = "top",
            legend.spacing.x = unit(0.2, 'cm'))
    p <- cowplot::get_legend(p)
  } else{
    p <- p+  theme(legend.position=legend.position)
    
  }
  
  
  return(p)
  
  
}

leg <- gg_taux_viefam(df=bdd_logit %>% mutate(indicatrice = ifelse(presta_rsa=="-RSA",1,0)), legend_only = TRUE)
p1 <- gg_taux_viefam(df=bdd_logit %>% mutate(indicatrice = ifelse(presta_rsa=="-RSA",1,0)), sous_champ="Français qui bénéficient du RSA")
p2 <- gg_taux_viefam(df=bdd_logit %>% mutate(indicatrice = ifelse(presta_apl=="-APL",1,0)), sous_champ="Français qui bénéficient d'APL")
p3 <- gg_taux_viefam(df=bdd_logit %>% mutate(indicatrice = ifelse(subj_pauvrete==1,1,0)), sous_champ="Français qui se sentent pauvres")
p4 <- gg_taux_viefam(df=bdd_logit %>% mutate(indicatrice = ifelse(subj_inf_mini_decla==1,1,0)), sous_champ="Français se sentant en difficulté fin.") #"-Rev. inf. au minimum décla."
library(cowplot)

gg_taux <- plot_grid(NULL,NULL,p1,p2,p3,p4,nrow=3, rel_heights = c(1,3,3))+
  draw_plot(leg,x=0,y=0.45)

gg_taux
  



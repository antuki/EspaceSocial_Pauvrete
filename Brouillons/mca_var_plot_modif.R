library(ggplot2)
library(ggrepel)


data_acm <- explor:::MCA_var_data(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = passives,
                         var_lab_min_contrib = 0, labels_prepend_var = FALSE) %>% 
  mutate(taille=ifelse(Variable=="subj_pauvrete_et_risque","bold","plain"))

gg_acm_actives <- ggplot(data_acm %>% filter(Type=="Active")) +
  geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  stat_smooth(aes(x = Coord.x, y = Coord.y), method = "lm", formula = y ~ poly(x, 2), size = 1, se = FALSE, color = "grey") +
  geom_point(aes(x = Coord.x, y = Coord.y, color=Variable)) + 
  geom_text_repel(aes(x = Coord.x, y = Coord.y,
                      color=Variable,label=gsub("-","",Lab),
                      size=taille,fontface=taille),
                  show.legend=FALSE, max.overlaps = 30, seed=1) +
  xlim(c(-2,2)) +
  ylim(c(-1,2)) + 
  xlab("Axe 1 (19,8 %)") +
  ylab("Axe 2 (9,6 %)") + 
  scale_color_manual(name="Variables (actives)",
                     labels=c("Ménage bénéficiaire\nd'APL ?", "Ménage bénéficiaire\nd'allocation chômage ?",
                              "Ménage bénéficiaire\nde bourse d'étude ?", "Ménage bénéficiaire\nd'une allocation handicap\n/ dépendance ?",
                              "Ménage locataire\nd'un logement social ?", "Ménage bénéficiaire\ndu RSA ?", 
                              "Quintile de niveau\nde vie du ménage", "Ménage recevant\ndes revenus financiers", "Ménage recevant\ndes revenus locatifs",
                              "Difficultés financières\nperçues","Sentiment ou\nrisque de pauvreté"),
                     values=c(RColorBrewer::brewer.pal(12,"Paired")[-c(11,12)],"#000000")) +
  scale_size_discrete(range=c(4,3)) +
  theme_minimal()

gg_acm_actives

gg_acm_passives <- ggplot(data_acm %>% filter(Type=="Supplementary") %>% 
               mutate(Lab=ifelse(Lab=="-Membre du couple (pas d’enfants à charge)","-Membre du couple\n(pas d'enfants à charge)",Lab)) %>% 
              mutate(Lab=ifelse(Lab=="-Membre du couple (enfants à charge)","-Membre du couple\n(enfants à charge)",Lab)) %>% 
               filter(Variable%in%c("annee_fac", "diplome", "prof_statut_act", "vie_fam"))) +
  geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  stat_smooth(data=data_acm %>% filter(Type=="Active"),
              aes(x = Coord.x, y = Coord.y), method = "lm", formula = y ~ poly(x, 2), size = 1, se = FALSE, color = "grey") +
  geom_point(data=data_acm %>% filter(Type=="Active"), 
             aes(x = Coord.x, y = Coord.y), color="grey", shape=16) + 
  geom_point(aes(x = Coord.x, y = Coord.y, color=Variable), shape=15, size=3) +
  geom_text_repel(aes(x = Coord.x, y = Coord.y,
                      color=Variable,label=gsub("-","",Lab)),
                  size=3.5,show.legend=FALSE,
                  max.overlaps = 100, seed=100) +
  xlim(c(-2,2)) +
  ylim(c(-1,2)) + 
  xlab("Axe 1 (19,8 %)") +
  ylab("Axe 2 (9,6 %)") + 
  scale_color_brewer(name="Variables (supplémentaires)", 
                    labels=c("Année","Niveau de diplôme","Situation professionnelle","Vie familiale"),
                    palette="Set1")+
  theme_minimal()

gg_acm_passives

# c("age_tranche", "statut_occup", "conn_rsa")
# c("annee_fac", diplome", "prof_statut_act", "vie_fam")


library(FactoMineR)
library(explor)
library(dplyr)


actives <- c("quantile_nivie", "presta_rsa","presta_chomage","presta_apl")
passives_autres <- c("revenus_financiers","revenus_locatifs","presta_handi","statut_occup", "diplome", "conn_rsa","annee_fac", "prof_statut_act", "age_tranche","vie_fam")
passives_subj <- c("subj_pauvrete","subj_risque_pauvrete", "subj_inf_mini_decla")
passives <- c(passives_subj,passives_autres)



bdd_acm <- bdd_logit %>% 
  #select(c("quantile_nivie","revenus_financiers","revenus_locatifs", "presta_rsa","presta_chomage","presta_apl","presta_handi","statut_occup", "diplome", "conn_rsa","annee_fac", "prof_statut_act", "age_tranche","vie_fam","subj_pauvrete")) %>% 
  select(c(all_of(actives),all_of(passives))) %>% 
  tidyr::drop_na()
res.mca <- MCA(bdd_acm,quali.sup=(length(actives)+1):(ncol(bdd_acm))) 
explor(res.mca)


res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = NULL,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = "Type", size_var = NULL,
                     size_range = c(10, 300), labels_size = 10, point_size = 128, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.42, 3.24),
                     ylim = c(-2.26, 2.41))


res$vars %>% 
  filter(Type == "Active", Axis == 1) %>% 
  select(Variable,Level,Count,Coord,Contrib,Cos2) %>% 
  data.frame() %>% 
  afficher_tableau()

res$vars %>% 
  filter(grepl("Supplementary", Type), Axis == 1) %>%
  mutate(Level = ifelse(Class == "Quantitative", "-", Level)) %>% 
  select(Variable, Level, Count,Coord,Contrib,Cos2,V.test,P.value) %>% 
  data.frame %>% 
  afficher_tableau()

afficher_tableau <- function(tab){
  
  options <- list(lengthMenu = c(10,20,50,100),
                        pageLength = 20, orderClasses = TRUE,
                        autoWidth = FALSE, searching = TRUE)
  
  order_option <- function(table, name, order="desc") {
    index <- which(names(table) == name) - 1
    list(order = list(list(index, order)))
  }
  
  dt <- DT::datatable(tab,
                      options = c(options, order_option(tab, "Coord")),
                      rownames = FALSE)
  indices_3 <- which(names(tab) %in% c("Coord", "Cos2", "V.test", "eta2", "P.value"))
  indices_2 <- which(names(tab) %in% c("Contrib"))
  if (length(indices_3) > 0) {
    dt <- dt %>%
      DT::formatRound(indices_3, digits = 3)
  }
  if (length(indices_2) > 0) {
    dt <- dt %>%
      DT::formatRound(indices_2, digits = 2)
  }
  return(dt)
}

afficher_tableau(tab)
  
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "quantile_nivie", labels_size = 9, point_opacity = 1,
                     opacity_var = NULL, point_size = 33, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.13, 2.9), ylim = c(-1.89, 2.15))

explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "presta_chomage", labels_size = 9, point_opacity = 1,
                     opacity_var = NULL, point_size = 33, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.13, 2.9), ylim = c(-1.89, 2.15))


explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "presta_apl", labels_size = 9, point_opacity = 1,
                     opacity_var = NULL, point_size = 33, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.13, 2.9), ylim = c(-1.89, 2.15))

explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "presta_rsa", labels_size = 9, point_opacity = 1,
                     opacity_var = NULL, point_size = 33, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.13, 2.9), ylim = c(-1.89, 2.15))




explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = c("subj_pauvrete",
                                                                               "subj_risque_pauvrete", "subj_inf_mini_decla"), var_lab_min_contrib = 0,
                     col_var = "Type", symbol_var = "Variable", size_var = NULL, size_range = c(10,
                                                                                                300), labels_size = 10, point_size = 128, transitions = TRUE, labels_positions = "auto",
                     labels_prepend_var = FALSE, xlim = c(-1.42, 3.24), ylim = c(-2.26, 2.41))

explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "subj_pauvrete", labels_size = 9, point_opacity = 1,
                     opacity_var = NULL, point_size = 33, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.13, 2.9), ylim = c(-1.89, 2.15))

explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = c("revenus_financiers",
                                                                               "revenus_locatifs"), var_lab_min_contrib = 0, col_var = "Type", symbol_var = "Variable",
                     size_var = NULL, size_range = c(10, 300), labels_size = 10, point_size = 128,
                     transitions = TRUE, labels_positions = "auto", labels_prepend_var = FALSE,
                     xlim = c(-1.42, 3.24), ylim = c(-2.26, 2.41))

explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = "prof_statut_act",
                     var_lab_min_contrib = 0, col_var = "Type", symbol_var = "Variable", size_var = NULL,
                     size_range = c(10, 300), labels_size = 10, point_size = 128, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.42, 3.24),
                     ylim = c(-2.26, 2.41))
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = c("age_tranche",
                                                                               "vie_fam"), var_lab_min_contrib = 0, col_var = "Type", symbol_var = "Variable",
                     size_var = NULL, size_range = c(10, 300), labels_size = 10, point_size = 37,
                     transitions = TRUE, labels_positions = NULL, labels_prepend_var = FALSE,
                     xlim = c(-0.944, 1.47), ylim = c(-1.32, 1.1))

explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = c("presta_handi",
                                                                               "statut_occup", "diplome", "conn_rsa", "annee_fac"), var_lab_min_contrib = 0,
                     col_var = "Type", symbol_var = "Variable", size_var = NULL, size_range = c(10,
                                                                                                300), labels_size = 10, point_size = 128, transitions = TRUE, labels_positions = NULL,
                     labels_prepend_var = FALSE, xlim = c(-0.527, 0.525), ylim = c(-0.579, 0.474))


base <- bdd_logit %>% dplyr::select(`subj_pauvrete`, `quantile_nivie`,
                                    `statact`, `prof`,`diplome`, `aide_log`, `aide_rsa`,
                                    `aide_handi`, `sexe`, `age_tranche`,
                                    `vie_fam`, `statut_occup0`, `annee_fac`
) %>% tidyr::drop_na()

contr.sum.inverse <- function (n, contrasts = TRUE, sparse = FALSE){
  cont <- contr.sum(n)
  cont <- cont[c(nrow(cont),1:(nrow(cont)-1)),]
  row.names(cont)<-1:nrow(cont)
  return(cont)
}

# 
# 
#  
# contr.sum.inverse <- function (n, contrasts = TRUE, sparse = FALSE)
# {
#   if (length(n) <= 1L) {
#     if (is.numeric(n) && length(n) == 1L && n > 1L)
#       levels <- seq_len(n)
#     else stop("not enough degrees of freedom to define contrasts")
#   } else levels <- n
#   
#   levels <- as.character(levels)
#   cont <- diag(1,n,n)
#   if (contrasts) {
#     cont <- cont[, -length(levels), drop = FALSE]
#     cont[length(levels), ] <- -1
#     colnames(cont) <- NULL
#   }
#   cont
# }

####### dummy coding
contr= contr.treatment
#contrasts(base$quantile_nivie) = contr.treatment(5)

# variable	modalite	valeur	pvaleur	odds_classiques	odds_deviation
# (Intercept)	-1.21	***	0.3	0
# quantile_nivie2		0.69	***	2	0.01
# quantile_nivie3		-0.98	***	0.38	0
# quantile_nivie4		-1.94	***	0.14	0
# quantile_nivie5		-2.8	***	0.06	0

###### sum / deviation coding
contr=contr.sum.inverse
#contrasts(base$quantile_nivie) =  contr.sum(5)[c(5,1:4),]

# variable	modalite	valeur	pvaleur	odds_classiques	odds_deviation
# (Intercept)	-2.22	***	0.11	0.04
# quantile_nivie1		1.7	***	5.46	2
# quantile_nivie2		0.03		1.03	0.38
# quantile_nivie3		-0.93	***	0.39	0.14
# quantile_nivie4		-1.8	***	0.17	0.06

###### implementation
reg_test <- glm(subj_pauvrete ~ quantile_nivie,
                data = base,
                family = binomial(logit), 
                contrasts=list(quantile_nivie=contr) #NEW
                )
results_test <- summary(reg_test)$coefficients
df_test <- data.frame(noms= c("-(Intercept)",row.names(results_test)[-1]),
                      valeur=round(results_test[,1],2),
                      pvaleur=cut(results_test[,4], breaks = c(0, 0.001, 0.01, 0.05, 1),
                                  include.lowest = T,
                                  labels = c('***', '**', '*', '')),
                      row.names=NULL
) %>% mutate(noms = gsub("`","",noms)) %>%  
  tidyr::separate(noms,c("variable","modalite"),sep="-") %>% 
  mutate(odds_classiques = round(exp(results_test[,1]),2)) %>% 
  mutate(odds_deviation = round(exp(results_test[,1] + sum(results_test[-1,1])) ,2)) 


head(df_test)


write_clip(df_test)

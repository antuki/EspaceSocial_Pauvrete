correlation_ggplot <- function(mat,ordre=row.names(mat),titre_legende="Corrélation"){
  mat <- mat[ordre,ordre]
  #mat[lower.tri(mat)] <- NA
  cbind(
    data.frame(mat)  %>%
      rownames_to_column(var="mesure1") %>%
      tidyr::pivot_longer(-mesure1, names_to="mesure2") %>%
      mutate(mesure1 = factor(mesure1,levels = unique(mesure1)),
             mesure2 = factor(mesure2,levels = unique(mesure1))
      ),
    data.frame(ggcorrplot::cor_pmat(mat))  %>%
      rownames_to_column(var="mesure1") %>%
      tidyr::pivot_longer(-mesure1, names_to="mesure2") %>%
      mutate(value=ifelse(value>0.05,"0","1")) %>% 
      dplyr::select(value) %>% 
      setNames("signi")
  )  %>% 
    mutate(label = round(value,2)) %>% 
    ggplot(aes(mesure1, mesure2, col=value)) + ## to get the rect filled
    geom_tile(color="white", fill="white") +
    geom_point(size = 5, shape=16) +
    geom_point(aes(shape = signi),size = 2,color = "black")+
    labs(x = NULL, y = NULL, col = titre_legende) +
    theme_classic()+
    scale_shape_manual(name="Significativité (5 %)",
                       values=c(4,NA), breaks=c("1","0"),labels=c("Oui","Non"))+
    scale_color_gradient2(mid="#FBFEF9",low="#6D9EC1",high="#E46726",
                          limits=c(-1,1), na.value="#FFFFFF")  +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_size(range=c(4,10), guide=NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}

##### Pearson
correlation_ggplot(mat=cor_pearson, ordre = c("s_sentpauv","s_infminidecla","i_log","i_rsa","m_nivie1","i_hlm","i_bourse",
                                                   "i_handi","i_chom","m_nivie2","s_risqpauv","m_nivie3","m_nivie4","m_nivie5","m_locatif",
                                                   "m_financier"),
                   titre_legende = "Corrélation Pearson")

##### Tetra
correlation_ggplot(mat=bdd_fa_tetra$rho, ordre = c("s_sentpauv","s_infminidecla","i_log","i_rsa","m_nivie1","i_hlm","i_bourse",
                                                   "i_handi","i_chom","m_nivie2","s_risqpauv","m_nivie3","m_nivie4","m_nivie5","m_locatif",
                                                   "m_financier"),
                   titre_legende = "Corrélation Tétra")

##### Spearman
correlation_ggplot(mat=cor_spearman, ordre = rev(c("m_quantilenivie","s_infminidecla","m_locatif","m_financier",
                                                   "i_hlm","s_sentpauvrisque","i_handi","i_chom","i_rsa",
                                                   "i_log","i_bourse")),
                   titre_legende = "Corrélation Spearman")

###### Poly
correlation_ggplot(mat=bdd_poLCA_poly$rho, ordre = rev(c("m_quantilenivie","s_infminidecla","m_locatif","m_financier",
               "i_hlm","s_sentpauvrisque","i_handi","i_chom","i_rsa",
               "i_log","i_bourse")), titre_legende = "Corrélation Poly")


#### Brouillon
+ theme(
  axis.text.x = ggplot2::element_text(
    angle = tl.srt,
    vjust = 1,
    size = tl.cex,
    hjust = 1
  ),
  axis.text.y = ggplot2::element_text(size = tl.cex)
) +
  ggplot2::coord_fixed()

if(!is.null(p.mat) & insig == "blank"){
  ns <- corr$pvalue > sig.level
  if(sum(ns) > 0) label[ns] <- " "
}

if (lab) {
  p <- p +
    ggplot2::geom_text(
      mapping = ggplot2::aes_string(x = "Var1", y = "Var2"),
      label = label,
      color = lab_col,
      size = lab_size
    )
}

#https://nzilbb.github.io/Covariation_monophthongs_NZE/Covariation_monophthongs_analysis.html
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://cran.r-project.org/web/packages/ggcorrplot/readme/README.html
# https://www.khstats.com/blog/corr-plots/corr-plots/
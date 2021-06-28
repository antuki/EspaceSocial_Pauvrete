#?structure.diagram

library(lavaan)
# mod.1 <- 'A =~ A1 + A2 + A3 + A4 + A5
#          C =~ C1 + C2 + C3 + C4 + C5
#          E =~ E1 +E2 + E3 + E4 +E5'
mod.1 <- 'MR1 =~ -0.8*E2 + -0.6*E1 + 0.6*E4 + 0.5*E3 + 0.5*E5
          MR2 =~ -0.6*C4 + 0.6*C2 + 0.6*C3 + 0.6*C1 + -0.6*C5
          MR3 =~ 0.7*A3 + 0.7*A2 + 0.5*A5 + -0.5*A1 + 0.5*A4
         '
mod.1 <- 'MR1 =~ 0.6*E4 + E2 + E1 + E4 + E3 + E5
          MR2 =~ 0.6*C1 + C2 + C3 + C4 + C5
          MR3 =~ 0.7*A2 + A1 + A3 + A4 + A5
         '
#fit.1 <- sem(mod.1,psychTools::bfi[complete.cases(psychTools::bfi),],std.lv=TRUE)
fit.1 <- cfa(mod.1,psychTools::bfi[complete.cases(psychTools::bfi),],std.lv=TRUE)
summary(fit.1)
lavaan.diagram(fit.1, cut=0)

#compare with
f3 <- fa(psychTools::bfi[complete.cases(psychTools::bfi),1:15],3)
fa.diagram(f3, cut=0)


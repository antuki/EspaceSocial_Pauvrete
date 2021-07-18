data(iris)
ggplot(iris, aes(x=Petal.Width, y=Sepal.Width, z=Petal.Length, color=Species)) + 
  theme_void() +
  axes_3D() +
  stat_3D()

#devtools::install_github("AckerDWM/gg3D")
library("gg3D")
library(ggplot2)












##### BROUILL
library(lavaan)
HS.model <- 'visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
t =~ visual + textual + speed'
fit <- cfa(HS.model, data=HolzingerSwineford1939)

hist(round(scale(predictions[,1]),2), main="s",xlim=c(0,2))
hist(round(scale(predictions[,2]),2), main="m")
hist(round(scale(predictions[,3]),2), main="i")
hist(round(scale(predictions[,4]),2), main="pauvreté")

length(table(round(scale(predictions[,1]),2)))

#log_trans <- function(x){log(x+abs(min(x)) + 1)}
log_trans <- function(x){sign(x)*log(abs(x))}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
predictions_bis <- predictions
predictions_bis[,4] <- log_trans(log_trans(log_trans(log_trans(log_trans(predictions_bis[,4])))))
summary(predictions_bis[,4])
plot(predictions_bis[,4])
# q <- quantile(predictions_bis[,4], probs=c(0,0.05,0.952,1))
# predictions_bis <- predictions_bis[which(!predictions_bis[,4]<q[2]),]
# predictions_bis <- predictions_bis[which(!predictions_bis[,4]>q[3]),]
# predictions_bis[,1] <- (predictions_bis[,1]-mean(predictions_bis[,1]))/sd(predictions_bis[,1])
# predictions_bis[,2] <- (predictions_bis[,2]-mean(predictions_bis[,2]))/sd(predictions_bis[,2])
# predictions_bis[,3] <- (predictions_bis[,3]-mean(predictions_bis[,3]))/sd(predictions_bis[,3])
# predictions_bis[,4] <- (predictions_bis[,4]-mean(predictions_bis[,3]))/sd(predictions_bis[,4])

#http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
#predictions <- predict(fit_hier2)
head(predictions)
shapes = c(16, 17, 18) 
shapes <- shapes[as.numeric(typo)]
scatterplot3d(predictions_bis[,1:3],
              #angle = 55,
              main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)",
              pch = 16, #shapes,
              xlim=c(-100000000000000,100000000000000),
              ylim=c(-100000000000000,100000000000000),
              zlim=c(-100000000000000,100000000000000),
              color="steelblue")


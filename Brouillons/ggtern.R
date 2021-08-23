#http://www.ggtern.com/2014/07/26/user-request-shepards-classification-sediments/
#Build a library of points, left to right, top to bottom...
points <- data.frame(
  rbind(c(1,1.000,0.000,0.000),
        c(2,0.750,0.250,0.000),
        c(3,0.250,0.000,0.750),
        c(4,0.750,0.000,0.250),
        c(5,0.5,0.25,0.25),
        c(6,0.250,0.750,0.000),
        c(7,0.25,0.25,0.5),
        c(8,0.25,0.5,0.25),
        c(9,0.750,0.125,0.125),
        c(10,0.125,0.750,0.125),
        c(11,0.125,0.125,0.750),
        c(12,0.000,1.000,0.000),
        c(13,0.000,0.750,0.250),
        c(14,0.000,0.250,0.750),
        c(15,0.000,0,1)
        )
)
colnames(points) = c("IDPoint","s","i","m")
base <- ggtern(data=points,aes(i,s,m)) +
   theme_bw() + theme_hidetitles() + theme_hidearrows() +
  geom_point(shape=21,size=10,color="blue",fill="white") +
  geom_text(aes(label=IDPoint),color="blue",size=10)
print(base)

polygon.labels <- data.frame(
  Label=c("s",
          "i + s",
          "m",
          "s + m",
          "i + m + s", 
          "i", 
          "m+i"))
#Assign each label an index
polygon.labels$IDLabel=1:nrow(polygon.labels)
polygons <- data.frame(
  rbind(c(1,1),c(1,2),c(1,4),
        c(2,2),c(2,9),c(2,5),c(2,8),c(2,10),c(2,6),
        c(3,3),c(3,14),c(3,15),
        c(4,4),c(4,9),c(4,5),c(4,7),c(4,11),c(4,3),
        c(6,6),c(6,12),c(6,13),
        c(5,5),c(5,8),c(5,7),
        c(7,7),c(7,8),c(7,10),c(7,13),c(7,14),c(7,11)
  )
  
  
)
#IMPORTANT FOR CORRECT ORDERING.
polygons$PointOrder <- 1:nrow(polygons)

#Rename the columns
colnames(polygons) = c("IDLabel","IDPoint","PointOrder")
#Merge the three sets together to create a master set.
df <- merge(polygons,points)
df <- merge(df,polygon.labels)
df <- df[order(df$PointOrder),]
#df <- bind_rows(df,tab_covariates_inter_cfa)
#Determine the Labels Data library(plyr)
Labs = plyr::ddply(df,"Label",function(x){c(c(mean(x$s),mean(x$i),mean(x$m)))})
colnames(Labs) = c("Label","s","i","m")
#Build the final plot
library(ggtern)
base <- ggtern(data=df,aes(i,s,m)) +
  geom_polygon(data=df[1:42,],aes(fill=Label,group=Label),color="black",alpha=0.25) +
  #geom_point(data=df[43:nrow(df),],fill="red",shape=21,size=3) +
  #geom_text(data=Labs,aes(label=Label),size=4,color="black") +
  theme_bw() +
  custom_percent("Percent") +
  labs(title="Shepard Sediment Classification Diagram",
       fill = "Classification",
       s="Subjectif",
       i="Institutionnel",
       m="Monétaire")
print(base) #to console


##### TEST AUTRE

#Build a library of points, left to right, top to bottom...
points <- data.frame(
  rbind(c(1,1.000,0.000,0.000),
        c(2,0.000,0.000,1.000),
        c(3,0.000,1.000,0.000),
        c(10,0.666,0.333,0.000),
        c(5,0.333,0.666,0.000),
        c(6,0.666,0.000,0.333),
        c(7,0.333,0.000,0.666),
        c(8,0.000,0.666,0.333),
        c(9,0.000,0.333,0.666),
        c(4,0.5,0.25,0.25),
        c(11,0.25,0.5,0.25),
        c(12,0.25,0.25,0.5)
        
  )
)
colnames(points) = c("IDPoint","s","i","m")
base <- ggtern(data=points,aes(i,s,m)) +
  theme_bw() + theme_hidetitles() + theme_hidearrows() +
  geom_point(shape=21,size=10,color="blue",fill="white") +
  geom_text(aes(label=IDPoint),color="blue",size=10)
print(base)

polygon.labels <- data.frame(
  Label=c("s",
          "i",
          "m",
          "s+i+m"))
#Assign each label an index
polygon.labels$IDLabel=1:nrow(polygon.labels)
polygons <- data.frame(
  rbind(c(1,1),c(1,5),c(1,7),
        c(2,2),c(2,6),c(2,8),
        c(3,3),c(3,10),c(3,9),
        c(4,4),c(4,11),c(4,12)
  )
  
  
)
#IMPORTANT FOR CORRECT ORDERING.
polygons$PointOrder <- 1:nrow(polygons)

#Rename the columns
colnames(polygons) = c("IDLabel","IDPoint","PointOrder")
#Merge the three sets together to create a master set.
df <- merge(polygons,points)
df <- merge(df,polygon.labels)
df <- df[order(df$PointOrder),]
df$color <- ifelse(df$Label=="s+i+m","black",NA)
df <- bind_rows(df,tab_covariates_inter_cfa)
df$legende <- ifelse(df$variable%in%c("sexe_femme","diplome_bacplus3","prof_statut_act_ouv","prof_statut_act_autreinac","prof_statut_act_foyer","age_tranche_1829","age_tranche_6069","vie_fam_mono","vie_fam_coupenf"),df$variable,NA)
#Determine the Labels Data library(plyr)
Labs = plyr::ddply(df,"Label",function(x){c(c(mean(x$s),mean(x$i),mean(x$m)))})
colnames(Labs) = c("Label","s","i","m")
#Build the final plot
library(ggtern)
base <- ggtern(data=df,aes(i,s,m)) +
  geom_polygon(data=df[1:12,],aes(fill=Label,group=Label), color=NA,alpha=0.25) +
  scale_fill_manual(values=c("red","yellow","blue","white"))+
  #scale_color_manual(values=c("black"),na.value=NA)+
  geom_point(data=df[13:nrow(df),], aes(color=legende),fill="black",size=1) + #,shape=21 aes(shape=variable)
  # geom_text(data=df[13:nrow(df),],
  #           aes(label=variable),
  #           #position=position_nudge_tern(y=0.1,x=-0.1/2,z=-0.1/2),
  #           size=3,check_overlap=F,color="black") +
  theme_bw() + theme_nomask() + #Allow Labels to Spool Over Edges
  custom_percent("Percent") +
  labs(T="Subjectif",
       L="Institutionnel",
       R="Monétaire")
print(base) #to console


#####

# Example from https://github.com/ricardo-bion/ggradar
library(ggplot2)
#devtools::install_github("ricardo-bion/ggradar", 
#                        dependencies = TRUE)
library(ggradar)
library(scales)

mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% dplyr::select(1:10) -> mtcars_radar

###########


tab_radar <- sum2$PE %>% 
  filter(op=="~" & substr(rhs,1,3)=="cov") %>% 
  mutate(variable=gsub("cov_","",rhs),
         pvaleur=cut(pvalue, breaks = c(0, 0.001, 0.01, 0.05, 1),
                     include.lowest = TRUE,
                     labels = c('***', '**', '*', '')),
         estimateur=paste0(round(est,2),pvaleur) 
  ) %>%  dplyr::select(lhs,variable,est) %>%
  rbind(subset(., lhs == "i")) %>% 
  mutate(lhs=toupper(lhs))

coord_radar <- 
  function(theta='x', start=0, direction=1){
    match.arg(theta, c('x','y'))
    ggproto(
      NULL, CoordPolar, 
      theta=theta, r=ifelse(theta=='x','y','x'),
      start=start, direction=sign(direction),
      is_linear=function() TRUE)
  }

variables_gardees <- c("age_tranche_1829","diplome_bacplus3",
                      "vie_fam_mono","vie_fam_coupenf",
                      "prof_statut_act_autrinac","prof_statut_act_retr",
                      "prof_statut_act_ouv","prof_statut_act_foyer")
variable.labs <-  c("18-29 ans","Bac +3","Familles monop.","Couples avec enf.",
                    "Autres inactifs","Retraités","Ouviers","Au foyer")
names(variable.labs) <- variables_gardees
  
ggplot(data=tab_radar %>% 
         filter(variable%in%variables_gardees),
       aes(x=lhs, y=est, group=variable)) +  # colour=variable
  geom_path(data=data.frame(lhs=c("I","S","M","S","I","M"),est=rep(0,6)), group=NA,colour="black",size=1,linetype="solid")  +
  geom_point(size=1) + 
  geom_path(color="red") +
  coord_radar() +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()) + 
  facet_wrap(~ variable, nrow=2,labeller = labeller(variable = variable.labs)) 






###

library(dplyr)
library(data.table)
library(ggplot2)

scale_zero_to_one <- 
  function(x) {
    r <- range(x, na.rm = TRUE)
    min <- r[1]
    max <- r[2]
    (x - min) / (max - min)
  }

scaled.data <-
  mtcars %>%
  lapply(scale_zero_to_one) %>%
  as.data.frame %>%
  mutate(car.name=rownames(mtcars)) 

plot.data <-
  scaled.data %>%
  reshape2::melt(id.vars='car.name') %>%
  rbind(subset(., variable == names(scaled.data)[1]))

# create new coord : inherit coord_polar
coord_radar <- 
  function(theta='x', start=0, direction=1){
    # input parameter sanity check
    match.arg(theta, c('x','y'))
    
    ggproto(
      NULL, CoordPolar, 
      theta=theta, r=ifelse(theta=='x','y','x'),
      start=start, direction=sign(direction),
      is_linear=function() TRUE)
  }

plot.data %>%
  ggplot(aes(x=variable, y=value, group=car.name, colour=car.name)) + 
  geom_path() +
  geom_point(size=rel(0.9)) +
  coord_radar() + 
  facet_wrap(~ car.name, nrow=4) + 
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = 'none') +
  labs(title = "Cars' Status")

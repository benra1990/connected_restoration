##--------------
##
##Script name: Connected restoration: global challenges in meeting national-level restoration targets
##
##Purpose of script: Develop procedures and statistical analyzes of manuscript
##
##Author: Dr. ---
##Affiliation:---
##
##Copyright (c) ---
##
##--------------
##
##Notes:Input data is made available as RData file and all sources are listed in SI Table 4
##
##Description of the input data required to replicate these analyses can be found at ---
##
##

#libraries

library(rgdal)
library(raster)
library(rgeos)
library(readxl)
library(reshape2)
library(openxlsx)
library(dplyr)
library(data.table)
library(scales)
library(ggplot2)
library(gridExtra)
library(NbClust)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
library(bestNormalize)
library(ggthemes)
library(ggsci)
library(outliers)

###0. LOAD DATA####

db_pledges<-read.xlsx("Z:/Restoration_inequalities_archetypes/Data/Processed/db_pledges_all.xlsx")##change depending on where database is on your computers

glimpse(db_pledges)#glimpse list of variables
data.table(colnames(db_pledges))#column names as nice list
db_pledges<-db_pledges[-c(8,11,15,19,27,28,36)]##self sufficiency, non_productive in km2, and pa_iucn_na, donations and tenure insecurity

#number of Na in each column and in each row
colSums(is.na(db_pledges))#nr of Na within rows
rowSums(is.na(db_pledges))

#deal with na's in whole database

#db_pledges$`pledges(ha)`[is.na(db_pledges$`pledges(ha)`)]<-0 #change NAs by 0 in case it's necessary

db_pledges$`non_productive_ha`[is.na(db_pledges$`non_productive_ha`)]<-0
db_pledges$`pa_area_iucn_1[km2]`[is.na(db_pledges$`pa_area_iucn_1[km2]`)]<-0
db_pledges$`pa_area_iucn_0[km2]`[is.na(db_pledges$`pa_area_iucn_0[km2]`)]<-0
db_pledges$`ind_pop`[is.na(db_pledges$`ind_pop`)]<-0
db_pledges$`ind_land_tot`[is.na(db_pledges$`ind_land_tot`)]<-0
db_pledges$`pledges_perc`[is.na(db_pledges$`pledges_perc`)]<-0
db_pledges$`pledges(ha)`[is.na(db_pledges$`pledges(ha)`)]<-0


#check again number of Na in each column and in each row
colSums(is.na(db_pledges))#nr of Na within rows
rowSums(is.na(db_pledges))

db_pledges1<-db_pledges%>%filter(rowSums(is.na(.))< 12)#remove all rows with more than 12 NAs
data.table(colnames(db_pledges1))##column names as nice list

db_pledges1[c(4:32)]<-apply(db_pledges1[c(4:32)],2, as.numeric)#turn all relevant columns numeric, this includes the pledges : column 4

### 1 PREPARE DATA FOR Hierarchical cluster analysis (HCA)

db_pledges_s<-db_pledges1[,c(1,6:12,19:32)]#generation of object without the pledges self-sufficiency and embodied land per capita
data.table(colnames(db_pledges_s))

#Check the data and transform when needed (scale and normalize)

#Histograms for all variables

par(mfrow= c (5,7),mar=c(1,2,2,0.5))     
for (i in 2:22) {
  hist(db_pledges_s[,c(1:22)][,i],main=names(db_pledges_s[,c(1:22)])[i],xlab=names(db_pledges_s[,c(1:22)])[i])
}

dev.off()

#normalize

#biophysical
bestNormalize(db_pledges_s$npp_pot_ha)
bestNormalize(db_pledges_s$hanpp_ha)
bestNormalize(db_pledges_s$non_productive_ha)
bestNormalize(db_pledges_s$`pa_area_iucn_1[km2]`)
bestNormalize(db_pledges_s$`pa_area_iucn_0[km2]`)
bestNormalize(db_pledges_s$`forest_share[%]`)
bestNormalize(db_pledges_s$`agri_share[%]`)


#socio-economic
bestNormalize(db_pledges_s$technology_index)
bestNormalize(db_pledges_s$life_expectancy)
bestNormalize(db_pledges_s$education)
bestNormalize(db_pledges_s$income)
bestNormalize(db_pledges_s$donations_recipient)
bestNormalize(db_pledges_s$future_growth)
bestNormalize(db_pledges_s$gdp_growth)
bestNormalize(db_pledges_s$pop_density)

#governance
bestNormalize(db_pledges_s$Voice)
bestNormalize(db_pledges_s$pol_stability)
bestNormalize(db_pledges_s$reg_quality)
bestNormalize(db_pledges_s$tenure_sec)
bestNormalize(db_pledges_s$ind_pop)
bestNormalize(db_pledges_s$ind_land_tot)

# Transform indicators to try to meet normality.

db_pledges_s%>%dplyr::mutate(npp_pot_ha=predict(bestNormalize::orderNorm(npp_pot_ha)), #biophysical
                             hanpp_ha=predict(bestNormalize::center_scale(hanpp_ha)),
                             non_productive_ha=predict(bestNormalize::orderNorm(non_productive_ha)),           
                             `pa_area_iucn_1[km2]`=predict(bestNormalize::orderNorm(`pa_area_iucn_1[km2]`)),
                             `pa_area_iucn_0[km2]`=predict(bestNormalize::orderNorm(`pa_area_iucn_0[km2]`)),
                             `forest_share[%]`=predict(bestNormalize::orderNorm(`forest_share[%]`)),
                             `agri_share[%]`=predict(bestNormalize::orderNorm(`agri_share[%]`)),
                   #socio-economic
                   technology_index=predict (bestNormalize::log_x(technology_index)),   
                   life_expectancy=predict (bestNormalize::orderNorm(life_expectancy)), 
                   education=predict(bestNormalize::orderNorm(education)),
                   income=predict(bestNormalize::sqrt_x(income)),
                   donations_recipient=predict(bestNormalize::arcsinh_x(donations_recipient)),
                   future_growth=predict(bestNormalize::yeojohnson(future_growth)),
                   gdp_growth=predict(bestNormalize::yeojohnson(gdp_growth)),
                   pop_density=predict(bestNormalize::orderNorm(pop_density)),
                   #governance
                   Voice=predict(bestNormalize::orderNorm(Voice)),
                   pol_stability=predict(bestNormalize::orderNorm(pol_stability)),
                   reg_quality=predict(bestNormalize::sqrt_x(reg_quality)),
                   tenure_sec=predict(bestNormalize::orderNorm(tenure_sec)),
                   ind_pop=predict(bestNormalize::orderNorm(ind_pop)),
                   ind_land_tot=predict(bestNormalize::orderNorm(ind_land_tot)),
                   
)->db_pledges_s1#new data base with normalized variables




db_pledges_s1[2:22]<-scale(as.data.frame(db_pledges_s1[2:22], center=TRUE, scale=TRUE))#z-scale data and assign new name

db_matrix<-dist(db_pledges_s1[2:22], method="manhattan")#Matriz of distances 


##2.Hierarchical cluster
glob_cluster<-hclust(db_matrix, method="ward.D" )

##3 Determining optimal number of clusters

res<-NbClust(db_pledges_s1[2:22], diss=NULL, distance = "manhattan", min.nc=3, max.nc=10, 
             method = "ward.D", index = "kl") 

res$All.index

res$Best.nc

##3.Drawing dendogram of hierarchical cluster
plot(glob_cluster, xlab="", ylab="Height", labels=FALSE, hang=-1, lwd=2, main="")

##5.Adding groupings to generated dendogram
rect.hclust(glob_cluster, 9, border= 1:10)

##6.Cut dendogram at the wanted number of clusters and transform into dataframe
country_clusters <- cutree(glob_cluster, 9)
country_clusters<-data.frame(country_clusters)

#get dataframe ready to merge with spatial data                                        

clust_countries <- cbind(db_pledges1$ISO3, db_pledges1$pledges_perc, country_clusters)
colnames(clust_countries)<-c("ISO3", "pledges","cluster")
clust_countries<-data.frame(clust_countries)
duplicated(clust_countries$ISO3)#check if there are duplicated country codes

#calculate mean of pledges in percentage per archetype to determine order in grid

pledges_order<-clust_countries%>%
  group_by(cluster)%>%
  summarise(promedio=mean(pledges))%>%
  arrange(promedio)#sort increasing order
pledges_order
#add new column with new order from largest pledge to lowest pledge to guide all analyses later on

clust_countries<-clust_countries%>%##assign new order
                 mutate(new_order=case_when(
                   cluster == 5~"1",
                   cluster== 2~"2",
                   cluster== 9~"3",
                   cluster== 7~"4",
                   cluster== 1~"5",
                   cluster== 3~"6",
                   cluster== 4~"7",
                   cluster== 6~"8",
                   cluster== 8~"9"
  ))

##For getting new order of clusters
pledges_order1<-clust_countries%>%
  group_by(new_order)%>%
  summarise(promedio=mean(pledges))%>%
  arrange(promedio)#sort increasing order
pledges_order1
                   
               

arch1<-clust_countries%>%filter(new_order==1)
list(arch1$ISO3)
arch2<-clust_countries%>%filter(new_order==2)
list(arch2$ISO3)
arch3<-clust_countries%>%filter(new_order==3)
list(arch3$ISO3)
arch4<-clust_countries%>%filter(new_order==4)
list(arch4$ISO3)
arch5<-clust_countries%>%filter(new_order==5)
list(arch5$ISO3)
arch6<-clust_countries%>%filter(new_order==6)
list(arch6$ISO3)
arch7<-clust_countries%>%filter(new_order==7)
list(arch7$ISO3)
arch8<-clust_countries%>%filter(new_order==8)
list(arch8$ISO3)
arch9<-clust_countries%>%filter(new_order==9)
list(arch9$ISO3)

lista_ordenada<-rbind(arch1, arch2, arch3, arch4, arch5, arch6, arch7, arch8, arch9)

colnames(lista_ordenada)<-c("ISO-3 Code", "Archetype")

#write.xlsx(lista_ordenada, "Z:/Restoration_inequalities_archetypes/Data/Processed/fromR/lista_paises_ordenada.xlsx", overwrite = TRUE)# export this list if wanted


##import shapefile from countries

countries<-readOGR(dsn = "Z:/Restoration_inequalities_archetypes/Spatial_data_and_scripts", layer="World_Countries__Generalized_")

countries@data$ISO3<-countrycode(countries@data$ISO, "iso2c", "iso3c")#transform iso2 column into iso3 for country name codes to merge with db_pledges_s

####spatialize data####
spdf<-sp::merge(countries,clust_countries, by="ISO3", sort=FALSE)

data.table(colnames(spdf@data))##check if columns (variables were merged)

writeOGR(spdf,dsn = "Z:/Restoration_inequalities_archetypes/Spatial data", layer="global_archetypes_all_with_donation", driver = "ESRI Shapefile", overwrite_layer = TRUE, verbose = TRUE)##aqui se encuentran los nombre correctos en minuscula de los municipio y todas las variables actuales


#plot 

spplot(spdf,"new_order", scales=list(draw=TRUE), cuts=8)  


####Characterization of the archetypes####


#Description of the clusters with barplot figure

barplot_db <- as.data.frame(cbind(db_pledges_s1, cluster=clust_countries$new_oder))#add cluster to main database
str(barplot_db)

barplot_db<-barplot_db%>%
  group_by(cluster)%>%
  dplyr::summarise_at(vars(npp_pot_ha :ind_land_tot ), mean, na.rm=TRUE)
ncol(barplot_db)
barplot_db1<-as.data.frame(barplot_db)
str(barplot_db1)#check structure


barplot_db1<-barplot_db1[,-1]##delete cluster nr column for the moment
barplot_db1<-as.data.frame(t(barplot_db1))
colnames(barplot_db1)[1:9]<-c("archetype1","archetype2", "archetype3", "archetype4", "archetype5", "archetype6", "archetype7", "archetype8", "archetype9")


##change names for final map
barplot_db1$rest_var<-c("NPP","HANPP","Non-productive areas", "Protected areas IUCN (IV-VI)","Protected areas IUCN (I-III)","Forest share", "Agriculture share", "Technology", "Life expectancy", "Education", "Income", "Donations (recipient)","Population Growth", "GDP Growth","Population density","Voice", "Political stability", "Regulatory quality", "Tenure security", "Indigenous population", "Indigenous land" )

##Here the Barplot for the Archetypes with ggplot(Fig. 2)

Arc_barplot_function<-function(barplot_db1,rest_var,var, mytitle){
barplot_db1|>
ggplot(aes(x=factor({{rest_var}}, levels={{rest_var}}), y={{var}}, color={{rest_var}}))+ geom_bar(stat="identity",position="dodge", fill="white", lwd=1.2) +
  coord_flip()+
  scale_y_continuous(breaks=seq(-2,2,0.5), labels=seq(-2,2,0.5))+
  scale_x_discrete(limits=rev)+
  scale_color_manual(breaks=c("NPP","HANPP","Non-productive areas", "Protected areas IUCN (IV-VI)","Protected areas IUCN (I-III)","Forest share", "Agriculture share", "Technology", "Life expectancy", "Education", "Income", "Donations (recipient)","Population Growth", "GDP Growth","Population density","Voice", "Political stability", "Regulatory quality", "Tenure security", "Indigenous population", "Indigenous land"), values=c("green","green","green","green","green","green","green","purple","purple","purple","purple","purple","purple","purple","purple","red","red", "red","red", "red","red"))+#,"Donations (donor)"
  ggtitle(mytitle) +
  theme_minimal()+
  theme(legend.position="none") +
  labs(x="", y ="z-score")
}


Arc1<-Arc_barplot_function(barplot_db1,rest_var,archetype1, "(1) Western Africa, Central America, 
      Bangladesh - high pledges")+
 geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")
 
Arc1

Arc2<-Arc_barplot_function(barplot_db1,rest_var,archetype2, "(2) All geographies, high pledges")+
   geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")

 
Arc2

Arc3<-Arc_barplot_function(barplot_db1,rest_var,archetype3, "(3) Africa and Asia, high pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid") 

Arc3

Arc4<-Arc_barplot_function(barplot_db1,rest_var,archetype4, "(4) Small African and Asian states,
      moderate pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")

Arc4

Arc5<-Arc_barplot_function(barplot_db1,rest_var,archetype5, "(5) North Africa and Middle East,
      moderate pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")
  
Arc5

Arc6<-Arc_barplot_function(barplot_db1,rest_var,archetype6, "(6) Eastern Europe and Caribbean, 
      moderate pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")

  
Arc6


Arc7<-Arc_barplot_function(barplot_db1,rest_var,archetype7, "(7) Canada, Scandinavia, Australia/New Zealand
      modest pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid") 

 
Arc7

Arc8<-Arc_barplot_function(barplot_db1,rest_var,archetype8, "(8) Small European and Caribbean states, 
      modest pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")
  
Arc8


Arc9<-Arc_barplot_function(barplot_db1,rest_var,archetype9, "(9) Western Europe, South Korea, Japan, 
      very low pledges")+
  geom_hline(yintercept =c(-0.5,0.5),color = "blue", linetype = "solid")

Arc9


#we first merge raw database with cluster number, this will be used for following graphs

box_pledges<-cbind(db_pledges1, cluster=clust_countries$new_oder)#merge raw database with cluster number (because eHANPP need to be raw)

str(box_pledges)

#combine all barplots
grid.arrange(Arc1,Arc2,Arc3,Arc4, Arc5,Arc6, Arc7, Arc8, Arc9, nrow=3, ncol=3, top="") 


####boxplots contrasting embodied hanpp with each archetype####

#for boxplots of pledges we need scaled data so we get our data ready

box_pledges1<-box_pledges[,c(4:5)]#selecting pledges ha and percentage
box_pledges1<-cbind(box_pledges1, cluster=clust_countries$new_oder)#merge scaled database with cluster number
str(box_pledges1)
colnames(box_pledges1)[1]<-"pledges_ha"
box_pledges1[c(1:2)]<-sapply(box_pledges1[c(1:2)], rescale, to =c(1,100), na.rm=TRUE)#we scale again here because in the previous scaling we had not incorporated the pledges variables and I want to use them for the boxplots to have a common y-axis


box_pledges%>%dplyr::filter(is.na(pledges_perc))#use !is.na to remove the na rows


#prepare data for boxplots of total embodied land VS archetypes

scatter_data<-box_pledges %>%
  group_by(cluster) %>%
  filter(!embodied_land_total %in% c(outlier(embodied_land_total)))
colnames(scatter_data)[4]<-"pledges_ha"

#add column with archetype names
box_pledges1$names<-c("Western Africa, Central America, Bangladesh - high pledges","All geographies, high pledges","Africa and Asia, high pledges","Small African and Asian states, moderate pledges","North Africa and Middle East, moderate pledges","Eastern Europe and Caribbean, moderate pledges","Canada, Scandinavia, Australia/New Zealand modest pledges","Small European and Caribbean states, modest pledges","Western Europe, South Korea, Japan, very low pledges")


mi_palette<-c("#1BD9B5", "#F0F200" ,"#7570B3", "#F000DB", "#EE0000" ,"#E6AB02" ,"#2E1900" ,"#267300","#0000FF")# create color-blind friendly palette for following plots


##boxplot for pledges in percentage(%) of the total area of countries and in hectares (ha)

#percentage pledges (Fig. 1A)

box1<-ggboxplot(box_pledges1, x = "cluster", y = "pledges_perc", fill = "cluster", orientation= "vertical",
                ylab = "pledges (country's area percentage)", xlab = "Archetype",add = "jitter", add.params = list(color = "black", size=0.5,alpha = 0.5), size=0.4, order=c(1,2,3,4,5,6,7,8,9))+
  stat_summary(fun =mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .8, size= 1.5,linetype = "solid", color="red") +
  coord_trans(y="log10")+
  scale_y_continuous(limits = c(NA, 100), breaks = c(0.1,1, 10, 50,100))+
  theme(legend.position = "none")+
  scale_color_manual(values= mi_palette)+
  scale_fill_manual(values= mi_palette, name="Archetypes", labels=c("(1)Western Africa, Central America
Bangladesh - high pledges","(2)All geographies, high pledges","(3)Africa and Asia, high pledges","(4)Small African and Asian states, 
moderate pledges","(5)North Africa and Middle East,
moderate pledges","(6)Eastern Europe and Caribbean,
moderate pledges","(7)Canada, Scandinavia, 
Australia/New Zealand, modest pledges","(8)Small European and Caribbean states, 
modest pledges","(9)Western Europe, South Korea, 
Japan, very low pledges"))

box1


box1<-box1+ 
  annotate("text", x=1:9, y= 100, alpha=.8, fill="white", color="black",lwd=4, label=c("[16]", "[44]","[14]","[8]", "[22]", "[21]", "[34]", "[24]", "[24]"))
 


#for hectare pledges (Fig. 1B)

box2<-ggboxplot(scatter_data, x = "cluster", y = "pledges_ha", fill = "cluster", orientation= "vertical",
          ylab = "pledges (ha)", xlab = "Archetype",add = "jitter", add.params = list(color = "black", size=0.5,alpha = 0.5),size = 0.4, order=c(1,2,3,4,5,6,7,8,9))+
 stat_summary(fun=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .8, size= 1.5,linetype = "solid", color="red")+
  coord_trans(y="log10")+
  scale_y_continuous(limits = c(1, 1e8), breaks = c(1, 10, 100, 1000, 10000, 100000,1e6, 1e7))+
  theme(legend.position="none")+
  scale_color_manual(values= mi_palette)+
  scale_fill_manual(values= mi_palette, name="Archetypes", labels=c("(1)Western Africa, Central America
Bangladesh - high pledges","(2)All geographies, high pledges","(3)Africa and Asia, high pledges","(4)Small African and Asian states, 
moderate pledges","(5)North Africa and Middle East,
moderate pledges","(6)Eastern Europe and Caribbean,
moderate pledges","(7)Canada, Scandinavia, 
Australia/New Zealand, modest pledges","(8)Small European and Caribbean states, 
modest pledges","(9)Western Europe, South Korea, 
Japan, very low pledges"))
  
box2

#combine box1 and box2
ggarrange(box1, box2, nrow=1, font.label=list(size=18, color="red"), common.legend = TRUE, legend="bottom" )


#boxplot eHANPP (Fig. 3)

ehanpp_order<-scatter_data%>%
  group_by(cluster)%>%
  summarise(promedio=mean(embodied_land_total, na.rm=TRUE))%>%
  arrange(promedio)#sort increasing order


#vertical plot

ehanpp_plot<-ggboxplot(scatter_data, x = "cluster", y = "embodied_land_total", fill = "cluster",
                       ylab = "eHANPP (ha)", xlab = "Archetypes",add = "jitter", add.params = list(color = "black", size=0.5,alpha = 0.8),order=c(1,2,3,4,5,6,7,8,9), size=0.4, font.label=list(size=30))+
  stat_summary(fun=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .85, size= 1.2,linetype = "solid", color="red") +
  theme_classic(base_size = 14) +
  geom_abline(slope=0, intercept=0,  col = "black",lty=1) +
  scale_color_manual(values= mi_palette)+
  scale_fill_manual(values= mi_palette, name="Archetypes", labels=c("(1)Western Africa, Central America
Bangladesh - high pledges","(2)All geographies, high pledges","(3)Africa and Asia, high pledges","(4)Small African and Asian states, 
moderate pledges","(5)North Africa and Middle East,
moderate pledges","(6)Eastern Europe and Caribbean,
moderate pledges","(7)Canada, Scandinavia, 
Australia/New Zealand, modest pledges","(8)Small European and Caribbean states, 
modest pledges","(9)Western Europe, South Korea, 
Japan, very low pledges"))
 

ehanpp_plot+ 
  annotate("text", x=1:9, y= 135000, alpha=1, fill="white", color="black",lwd=4, label=c("28.7%", "9.55%","9.03%","6.05%", "4.65%", "3.8%", "1.61%", "1.26%", "0.37%"))+
  coord_cartesian(ylim=c(-150000, 150000))



##END##






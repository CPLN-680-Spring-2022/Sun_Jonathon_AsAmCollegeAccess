#Moran's I

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(sp,ISLR,MASS,spatstat,spatial,maptools,ppp,fossil,adehabitHR,gdata,raster,rgdal,geostatst, spdep, caret, tidyverse, sf, spdep, caret,
ckanr,
grid,
gridExtra,
knitr,
kableExtra,
tidycensus,
scales,
sp,
rgdal,
rgeos,
spdep,
spgwr,
tmap)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
# Log transform the population to even out the skew


df <- left_join(Philadelphia_tracts %>%
            dplyr::select(GEOID),
          ACS_Cluster_Group %>%
            dplyr::select(-ends_with("nn1"),
                   -ends_with("nn2"),
                   -ends_with("nn3"),
                   -ends_with("nn4"),
                   -ends_with("nn5")),
          by = "GEOID")

Variable <- df$AAPI_Race

#queen neighbors
queen <- poly2nb(df, row.names = df$GEOID)
queen

#global Moran's I
queenlist <- nb2listw(queen, style = 'W') #Spatial weights for neighors. 

#running Moran's I
MoranIAAPI <- moran(Variable, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$'I'

#Checking significance
moranMC <- moran.mc(Variable, queenlist, nsim=999)
moranMC
moranMCres <- moranMC$res
moran.plot(Variable, queenlist)

plot()
hist(moranMCres, freq = 10000000, nclass = 100)
abline(v = moran(df$AAPI_Race, queenlist, n = length(queenlist$neighbours), S0=Szero(queenlist))$'I', col='red')

MoransIFunction <- function(df,Variable){
  queen <- poly2nb(df, row.names = df$GEOID)
  queenlist <- nb2listw(queen, style = 'W') #Spatial weights for neighbors.
  MoranI <- moran(Variable, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$'I'
  
  #Checking significance
  moranMC <- moran.mc(Variable, queenlist, nsim=999)
  moranMCres <- moranMC$res
  moran.plot(Variable, queenlist)
  hist(moranMCres, freq = 10000000, nclass = 100)
  abline(v = moran(df$AAPI_Race, queenlist, n = length(queenlist$neighbours), S0=Szero(queenlist))$'I', col='red')
  }

MoransIFunction(df,Variable = df$Bachelor_more_Edu_Attainment)
MoranI
moranMC
moranMCres

MoransIFunction(df,Variable = df$University_nnAve)
MoranI
moranMC
moranMCres

#Local Moran's I (LISA)
#LISA<- localmoran(Variable, queenlist)
#moranSig.plot<-function(df,listw, title){
#  local<-localmoran(x=Variable, listw=listw, zero.policy = FALSE)
#  moran.map<-cbind(df, local)
#  tm<-tm_shape(moran.map)+
#    tm_borders(col='white')+
#    tm_fill(col='Pr.z...0.', style='fixed', breaks=c(0,0.05, 1), title= 'p-value', palette = '-BuPu') +
#    tm_layout(frame = FALSE, title = title)
#  print(tm)
#}
# moranSig.plot(df, queenlist, 'p-value')

#Moran's I statistical Significance -------------

for(i in 3:(length(colnames(df))-1)) {
  if(i == 14) next
  
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  moran.map <- cbind(df, localmoran(Variable, queenlist)) %>%
                mutate(Significant = `Pr.z....E.Ii..` < 0.05)
  ggplot() +
    geom_sf(data = moran.map, aes(fill = Significant)) +
    labs(title = paste0("P-Value Clustering:",colnames(df)[i])) +
    mapTheme()
  
  ggsave(filename = paste0("Data_Analysis/Figs/Pvalue/P-value_Clustering_Category_",colnames(df)[i],".png"), plot = last_plot())
}

# Original Moran's I Script ---------------------------
#hl.plot<-function(df, listw){
#  local<-localmoran(x=Variable, listw=listw, zero.policy = FALSE)
#  quadrant<-vector(mode='numeric', length=323)
#  m.prop<-Variable - mean(Variable)
#  m.local<-local[,1]-mean(local[,1])
#  signif<-0.05
#  quadrant[m.prop >0 & m.local>0]<-4 #high MEDHVAL, high clustering
#  quadrant[m.prop <0 & m.local<0]<-1 #low MEDHVAL, low clustering
#  quadrant[m.prop <0 & m.local>0]<-2 #low MEDHVAL, high clustering
#  quadrant[m.prop >0 & m.local<0]<-3 #high MEDHVAL, low clustering
#  quadrant[local[,5]>signif]<-0
  
#  brks <- c(0,1,2,3,4)
#  colors <- c("grey","light blue",'blue','pink',"red")
#  plot<-plot(shp,border="gray90",lwd=1.0,col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
#}
#hl.plot(df, queenlist)
#legend("bottomright",legend=c("insignificant","low-high","low-low","high-low","high-high"),
#       fill=c("grey", "light blue", "blue", "pink", "red"),bty="n", cex = 0.5)

# Clustering Uniersity NN ave -------------------
#Variable <- df$University_nnAve

#local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
#  quadrant <- vector(mode = 'numeric', length=323)
#  m.prop <- Variable - mean(Variable)
#  m.local <- local[,1] - mean(local[,1])
#  signif <- 0.05
#  quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
#  quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
#  quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
#  quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
#  quadrant[local[,5]>signif]<-0
  
#  lmoran_plot <- cbind(df,as.tibble(quadrant)) %>%
#    rename(Clustering_cat = value) %>%
#    mutate(Clustering_cat = as.factor(Clustering_cat))

#ggplot() +
#  geom_sf(data = lmoran_plot,
#          aes(fill = Clustering_cat)) +
#  labs(title = "Clustering Category") +
#  mapTheme()

#ggsave(filename = paste0("Data_Analysis/Figs/University_NNAve.png"), plot = last_plot())

# Local Moran's I Loop to print of maps ---------------------

colnames <- df %>%
  st_drop_geometry() %>%
  colnames()

for(i in 3:(length(colnames(df))-1)) {
  if(i == 14) next
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
  quadrant <- vector(mode = 'numeric', length=323)
  m.prop <- Variable - mean(Variable)
  m.local <- local[,1] - mean(local[,1])
  signif <- 0.05
  quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
  quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
  quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
  quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
  quadrant[local[,5]>signif]<-0
  
  lmoran_plot <- cbind(df,as.tibble(quadrant)) %>%
    rename(Clustering_cat = value) %>%
    mutate(Clustering_cat = as.character(Clustering_cat),
           Clustering_cat = case_when(
             Clustering_cat == "4" ~ paste0("high ", colnames[i], " high clustering"),
             Clustering_cat == "2" ~ paste0("low ", colnames[i], " high clustering"),
             Clustering_cat == "1" ~ paste0("low ", colnames[i]," low clustering"),
             Clustering_cat == "3" ~ paste0("high ", colnames[i], " low clustering")),
           Clustering_cat = as.factor(Clustering_cat))
  
  ggplot() +
    geom_sf(data = lmoran_plot,
            aes(fill = Clustering_cat)) +
    labs(title = paste0("Clustering Category:",colnames(df)[i])) +
    mapTheme()
  
  ggsave(filename = paste0("Data_Analysis/Figs/Clustering_Cat/Clustering_Category_",colnames(df)[i],".png"), plot = last_plot())
}


# Clustering with Transportation Measures

Class <- nlevels(as.factor(GTFS_Rail_route$route_id))

cc <- scales::seq_gradient_pal("#F6FF33", "#33FF52")(seq(0,1,length.out=Class))


Philly_uni_2020 <- Universities %>%
  filter(YEARS == "2020-01-01") %>%
  dplyr::select(NAME,SHORT_CARNEGIE) %>%
  drop_na(SHORT_CARNEGIE) %>%
  unique()

Class <- nlevels(as.factor(Philly_uni_2020$SHORT_CARNEGIE))

ccUni <- scales::seq_gradient_pal("red", "green")(seq(0,1,length.out=Class))

for(i in 3:(length(colnames(df))-1)) {
  if(i == 14) next
  
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
  quadrant <- vector(mode = 'numeric', length=323)
  m.prop <- Variable - mean(Variable)
  m.local <- local[,1] - mean(local[,1])
  signif <- 0.05
  quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
  quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
  quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
  quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
  quadrant[local[,5]>signif]<-0
  
  lmoran_plot <- cbind(df,as.tibble(quadrant)) %>%
    rename(Clustering_cat = value) %>%
    mutate(Clustering_cat = as.character(Clustering_cat),
           Clustering_cat = case_when(
             Clustering_cat == "4" ~ paste0("high ", colnames[i], " high clustering"),
             Clustering_cat == "2" ~ paste0("low ", colnames[i], " high clustering"),
             Clustering_cat == "1" ~ paste0("low ", colnames[i]," low clustering"),
             Clustering_cat == "3" ~ paste0("high ", colnames[i], " low clustering")),
           Clustering_cat = as.factor(Clustering_cat))
  
  ggplot() +
    geom_sf(data = lmoran_plot,
            aes(fill = Clustering_cat)) +
    geom_sf(data = GTFS_Rail_route %>%
              mutate(route_id = as.factor(route_id)),
            aes(color = route_id)) +
    scale_colour_manual(values = cc) +
    ggnewscale::new_scale_color() +
    geom_sf(data = Universities %>%
              filter(YEARS == "2020-01-01") %>%
              dplyr::select(NAME,SHORT_CARNEGIE) %>%
              unique(),
            aes(color = SHORT_CARNEGIE)) +
    scale_colour_manual(values = ccUni) +
    labs(title = paste0(colnames[i],":Local Moran's I")) +
    mapTheme()
  
  ggsave(filename = paste0("Data_Analysis/Figs/Transit/Clustering_Category_",colnames(df)[i],".png"), width = 10, height = 10, units = "in", plot = last_plot(), dpi = 400)
}



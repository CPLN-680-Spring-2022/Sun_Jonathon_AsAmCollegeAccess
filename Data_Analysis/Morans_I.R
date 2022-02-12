#Moran's I

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(sp,ISLR,MASS,spatstat,spatial,maptools,ppp,fossil,adehabitHR,gdata,raster,rgdal,geostatst, spdep)

# Log transform the population to even out the skew

ACS.Long %>%
  filter(Race_Ethnicity == "Asian_alone") %>%
  mutate(Frequency_log = log(Frequency)) %>%
  ggplot() +
  geom_histogram( aes(x = Frequency_log))

nb <- poly2nb(ACS.Long, queen = TRUE)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
inc.lag <- lag.listw(lw, ACS.Long %>%
                            filter(Race_Ethnicity == "Asian_alone") %>%
                            select(Race_Ethnicity))

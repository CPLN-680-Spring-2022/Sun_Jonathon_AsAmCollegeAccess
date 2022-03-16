library(NbClust)
library(flexclust)
library(dplyr)


#Asian Cluster ----------------------


df <- ACS_Cluster_Group %>%
        dplyr::select(GEOID,ends_with("Asian")) %>%
        select(-okinwan_Asian)

options(scipen = 999)

df <- data.frame(scale(df[-c(1:2)]))
head(df)



#Scree plot ----------------------------------
#we gotta find the wierd bend in the data
wss <- nrow(df)-1*sum(apply(df,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df,
                                     centers=i)$withinss)
plot(1:20, wss, type ="b", xlab = "number of clusters",
     ylab="Within groups sum of squares")

#Determing the appropriate amount of ----------------------------
set.seed(1234)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method="kmeans", index="all")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#Kmeans clustering ---------------------------------
set.seed(1234)
fit.km <- kmeans(df, 6, nstart = 25)

#observations per cluster
fit.km$size

#clustering results and centroid vector for each cluster
Asian_Cluster <- round(fit.km$centers, 2)
Asian_Cluster
fit.km$cluster

Cluster <- cbind(ACS_Cluster_Group[c(1:2)],fit.km$cluster)

ACS_Cluster_Group <- left_join(ACS_Cluster_Group,
                               Cluster %>%
                                 rename(Race_Cluster = `...3`),
                               by = c("GEOID","NAME"))


ACS.Long <- left_join(ACS.Long,
                      Cluster,
                      by = "GEOID") %>%
  rename(Asian_Cluster = `...3`)

Philadelphia_tracts <- left_join(Philadelphia_tracts,
                          Cluster,
                          by = "GEOID") %>%
      rename(Asian_Cluster = `...3`)


Cluster <- left_join(Philadelphia_tracts %>%
                    select(GEOID),
                  Cluster,
                  by = "GEOID")

ggplot() +
  geom_sf(data = Cluster, aes(fill = fit.km$cluster)) +
  mapTheme()

#Income Cluster ----------------------

df <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,ends_with("income"),ends_with("attainment"))

options(scipen = 999)

df <- data.frame(scale(df[-c(1:2)]))
head(df)

#Scree plot ----------------------------------
#we gotta find the wierd bend in the data
wss <- nrow(df)-1*sum(apply(df,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df,
                                     centers=i)$withinss)
plot(1:20, wss, type ="b", xlab = "number of clusters",
     ylab="Within groups sum of squares")

#Determing the appropriate amount of ----------------------------
set.seed(1234)
nc <- NbClust(df, min.nc = 3, max.nc = 15, method="kmeans", index="all")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#Kmeans clustering ---------------------------------
set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)

#observations per cluster
fit.km$size

#clustering results and centroid vector for each cluster
Income_Cluster <- round(fit.km$centers, 2)
fit.km$cluster

Cluster <- cbind(ACS_Cluster[c(1)],fit.km$cluster)

ACS_Cluster_Group <- left_join(ACS_Cluster_Group,
                               Cluster %>%
                                 rename(IncomeEduc_Cluster = `fit.km$cluster`),
                               by = c("GEOID"))

ACS.Long <- left_join(ACS.Long,
                      Cluster,
                      by = "GEOID") %>%
        rename(Edu_Inc_Cluster = `fit.km$cluster`)

Philadelphia_tracts <- left_join(Philadelphia_tracts,
                      Cluster,
                      by = "GEOID") %>%
                      rename(Edu_Inc_Cluster = `fit.km$cluster`)

Cluster <- left_join(Philadelphia_tracts %>%
                       select(GEOID),
                     Cluster,
                     by = "GEOID")

ggplot() +
  geom_sf(data = Cluster, aes(fill = fit.km$cluster)) +
  mapTheme()

#Distance Cluster ----------------------

df <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,ends_with("nnAve"))

options(scipen = 999)

df <- data.frame(scale(df[-c(1:2)]))
head(df)

#Scree plot ----------------------------------
#we gotta find the wierd bend in the data
wss <- nrow(df)-1*sum(apply(df,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df,
                                     centers=i)$withinss)
plot(1:20, wss, type ="b", xlab = "number of clusters",
     ylab="Within groups sum of squares")

#Determing the appropriate amount of ----------------------------
set.seed(1234)
nc <- NbClust(df, min.nc = 3, max.nc = 15, method="kmeans", index="all")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#Kmeans clustering ---------------------------------
set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)

#observations per cluster
fit.km$size

#clustering results and centroid vector for each cluster
Distance_Cluster <- round(fit.km$centers, 2)
Distance_Cluster
fit.km$cluster

Cluster <- cbind(ACS_Cluster[c(1)],fit.km$cluster)

ACS_Cluster_Group <- left_join(ACS_Cluster_Group,
                               Cluster %>%
                                 rename(Distance_Cluster = `fit.km$cluster`),
                               by = c("GEOID"))
ACS_Cluster_Group %>%
  st_as_sf()
ggplot() +
  geom_sf(aes(fill = Race_Cluster))

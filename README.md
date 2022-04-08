# Sun_Jonathon_AsAmCollegeAccess

## Abstract
Asian American comprises a panoply of differing and unique intersectional identities, histories, and experiences, yet Asian Americans are essentialized as a group and broadly stereotyped as the model minority, which shapes information to college access and campus resources (Museus & Truong, 2009; Palmer & Maramba, 2015; Poon & Byrd, 2013). Scholars and researchers have frequently called for the disaggregation of Asian American data to ensure that ethnic minorities are supported within the Asian American racial category (Museus & Truong, 2009). 
In the pursuit of data disaggregation, I hope to map the different experiences of Asian American ethnic groups across Philadelphia and their access to higher education utilizing K-clustering and Moran’s I. K-cluster analysis will be used to show how different Asian American groups are different distributed across Philadelphia. Moran’s I will be used to show how different resources are distributed across Philadelphia; for example, tutoring services, higher education institutions, and conditions of k-12 schools. Overall, I argue that Asian Americans have a variety of different educational resources, and as such public policy should better distribute these resources.

## Data 
* Amercain Community Survey 2010 - 2019
* IPEDS Data 
* Philadelphia Schools Metrics 

## Methods

### Moran's I
To begin this analysis, I first determined if broadly Asian populations were spatially autocorrelated in Philadelphia using Moran’s I. Moran’s I has been widely used to test for spatial autocorrelation or spatial dependencies and its value determines the strength of autocorrelation indicating how clustered values are. Values that are closer to 1 indicate strong positive autocorrelation, while values closer to -1 indicate negative autocorrelation that being how repelled values are. Conversely, if values are positively autocorrelated they are spatially clustered. If the value is close to 0, then there is no spatial autocorrelation, indicating a random pattern. Unlike Pearson's correlation coefficient, Moran's I does not always lie within -1 and 1 and can be situated beyond each of these values.

### K-Cluster Analysis
The goal of K-means is to minimize the within-cluster sum of squared errors, which is calculated for each cluster by computing the squared distance between each observation and the centroid of the cluster. There are some limitations to using K-means such as having to specify the number of clusters in advance, using numeric data, and being unable to handle noisy data and outliers. Other problems also include clusters differing in size, density, and non-globular shapes.

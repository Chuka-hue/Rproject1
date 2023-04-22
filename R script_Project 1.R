# Install libraries with install.packages
library(tidyverse) # for data wrangling and visualization
library(NbClust) # for determining the best number of clusters
library(flexclust) # for segment profile plot
library(janitor) # for tabyl

# Start by reading the data
data <- read.csv("office.csv")

# Let’s inspect our data
glimpse(data)
summary(data)

data <- data %>% 
  mutate(professional = factor(professional))

# scale the values found in every rows 
# and in columns with the rated attributes
data_std <- scale(data[, c("variety_of_choice",
                           "electronics",
                           "furniture",
                           "quality_of_service",
                           "low_prices",
                           "return_policy")]) %>% 
  as_tibble() # convert to tibble

# Normalize dataset
office_norm <- scale(data_std) %>% as_tibble()

summary(office_norm)

# compute distances
dist <- dist(office_norm, 
             method = "euclidean") # use Euclidean

# print the first 5 rows and first 5 columns of the distance matrix
as.matrix(dist)[1:5, 1:5] 

set.seed(123) # for repreducibility

# run the algorithm and store the result to an object
hc <- hclust(dist, method = "ward.D2") 

# plot the dendogram
plot(hc)

# now draw rectangles highlighting the clusters
rect.hclust(hc, k = 6, border = "red")

# create a six-cluster solution
hc6 <- cutree(hc, k = 6)

table(hc6)

# the means for each of the attitudinal variables per cluster
office_norm %>% # take the std data
  mutate(hc6 = factor(hc6)) %>% # add the cluster assignment
  group_by(hc6) %>% # group by cluster
  mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the prop per group 
  print(width = Inf) # print all columns

# Segment Profile Plot
hc6_flex <- as.kcca(hc, office_norm, k = 6)

barchart(hc6_flex)

# Check the concordance
table(hc6, clusters(hc6_flex))

# draw rectangles around the branches of a dendrogram 
# highlighting the corresponding clusters
rect.hclust(hc, k = 5, border = "red")

# create a five-cluster solution
hc5 <- cutree(hc, k = 5)
table(hc5)

office_norm %>% 
  mutate(hc5 = factor(hc5)) %>% # add the cluster assignment
  group_by(hc5) %>% # group by cluster
  mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the prop per group 
  print(width = Inf) # print all columns

# Segment Profile Plot
hc5_flex <- as.kcca(hc, office_norm, k = 5)

barchart(hc5_flex)

# Check the concordance
table(hc5, clusters(hc5_flex))

# Give “expressive” labels to the clusters
hc5 <- factor(hc5, 
              levels = c(1, 2, 3, 4, 5),
              labels = c("Furniture_Choice HC", "Price_Return HC", "Electronic_Return HC", "Electronics HC", "Service HC"))


# Number of Clusters
set.seed(123) # for reproducibility
NbClust(data = office_norm[, 1:5],  
        min.nc = 3, # min number of clusters
        max.nc = 15, # max number of clusters
        index = "all", # use all indexes
        method = "ward.D2")$Best.ncBest #  print only the number of clusters proposed

# Targeting the Segments
data <- data %>% mutate(hc5 = hc5)

# professional
data %>%
  tabyl(hc5, professional) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

# Income
# Categorize into group first
data <- data %>% mutate(incomegroup = 
                        case_when(income >= 65  & income <= 95 ~ 'Upper Class',
                                  income >= 32  & income <= 64 ~ 'Middle class',
                                  income >= 13  & income <= 31 ~ 'Lower class'))

data <- data %>% 
  mutate(incomegroup = factor(incomegroup))

data %>%
  tabyl(hc5, incomegroup) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

# Age
# Categorize into group first
data <- data %>% mutate(agegroup = 
                          case_when(age >= 58  & age <= 68 ~ 'Boomers',
                                    age >= 42  & age <= 57 ~ 'Generation X',
                                    age >= 26  & age <= 41 ~ 'Generation Y',
                                    age >= 21  & age <= 25 ~ 'Generation Z'))

data <- data %>% 
  mutate(agegroup = factor(agegroup))

data %>%
  tabyl(hc5, agegroup) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 


# K-means Clustering
set.seed(123) # for reproducibility

# run the kmeans function
km <- kmeans(office_norm, 
                       centers = 5, 
                       iter.max = 1000,
                       nstart = 100)

km
table(km$cluster)

km5 <- factor(
  km$cluster,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Price_Return KM", "Electronic_Return KM", "Electronics KM", "Service KM", "Furniture_Choice KM"))

# Results Comparison
data <- data %>% mutate(km5 = km5)

data %>%
  tabyl(km5, hc5) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

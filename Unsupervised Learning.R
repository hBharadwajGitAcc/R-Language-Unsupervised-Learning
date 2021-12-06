##################### K-means Assignment ##################### 

# 1.Building the k-means clustering algorithm: a.Start off by extracting the 'MonthlyCharges', 'tenure' & 'TotalCharges' columns from the 'customer_churn' data.frame. Store the result in 'customer_features'
library(readr)
churn = read.csv("C://Users//user//Documents//Working Directory//Working Directory_Intellipaat//R Language//Practice Session//1st  Stimulus//customer_churn.csv")

library(dplyr)
customer_features = churn %>% select("MonthlyCharges", "tenure","TotalCharges")
head(customer_features)


# b.Remove any 'NA' values from 'customer_features' if present
sum(is.na(churn))
colnames(churn)
sum(is.na(churn$customerID))
sum(is.na(churn$gender))
sum(is.na(churn$SeniorCitizen))
sum(is.na(churn$Partner))
sum(is.na(churn$Dependents))
sum(is.na(churn$tenure))
sum(is.na(churn$PhoneService))
sum(is.na(churn$MultipleLines))
sum(is.na(churn$InternetService))
sum(is.na(churn$OnlineSecurity))
sum(is.na(churn$OnlineBackup))
sum(is.na(churn$DeviceProtection))
sum(is.na(churn$TechSupport))
sum(is.na(churn$StreamingTV))
sum(is.na(churn$StreamingMovies))
sum(is.na(churn$Contract))
sum(is.na(churn$PaperlessBilling))
sum(is.na(churn$PaymentMethod))
sum(is.na(churn$MonthlyCharges))
sum(is.na(churn$TotalCharges))
sum(is.na(churn$Churn))


# While loop to get the columns with Null Values
a = churn
class(a)
b = 1
c = 0
while (b <= length(a)) {
  if(sum(is.na(a[b])) > 0) {
    c = colnames(a[b])
  }
  b = b + 1
}
print(paste(c, "is the column with the Null Values"))
sum(is.na(churn$TotalCharges))


# For loop to get the columns with Null Values
d = 0
for (e in 1:dim(a)[2]){ # 1:dim(a)[1] - to operate on rows, simply put [1] is for rows & [2] is for columns
  if(sum(is.na(a[e])) > 0) {
    d = colnames(a[e])
    }
}
print(paste(d, "is the column with the Null Values"))
sum(is.na(churn$TotalCharges))


dim(customer_features)
customer_features = na.omit(customer_features)
#customer_features[complete.cases(customer_features),]
#dim(as.data.frame(customer_features))
dim(customer_features)
sum(is.na(customer_features))


# c.Build the kmeans algorithm on top of 'customer_features'. Here, the number of clusters should be 3
clusters = kmeans(customer_features,3)


# d.Binding the clustering vector to 'customer_features'
cluster_group = cbind(customer_features, Clusters=clusters$cluster)

cluster_group = as.data.frame(cluster_group)

head(cluster_group)


# e.Extracting observations belonging to individual clusters
cluster1 = cluster_group %>% filter(Clusters==1)
head(cluster1)

cluster2 = cluster_group %>% filter(Clusters==2)
head(cluster2)

cluster3 = cluster_group %>% filter(Clusters==3)
head(cluster3)




#------------------------------------------------------------------------------------------------------------------#
model0 = kmeans(customer_features$MonthlyCharges,3)
model0

month_group = cbind(Month=customer_features$MonthlyCharges, Clusters=model0$cluster)
head(month_group)

month_group = as.data.frame(month_group)
month_group_1 = month_group %>% filter(Clusters==1) 
month_group_2 = month_group %>% filter(Clusters==2)
month_group_3 = month_group %>% filter(Clusters==3)

head(month_group_1)
head(month_group_2)
head(month_group_3)


#------------------------------------------------------------------------------------------------------------------#
model1 = kmeans(customer_features$tenure,3)
model1

tenure_group = cbind(Tenure=customer_features$tenure, Clusters=model1$cluster)
head(tenure_group)

tenure_group = as.data.frame(tenure_group)
tenure_group1 = tenure_group %>% filter(Clusters==1)
tenure_group2 = tenure_group %>% filter(Clusters==2)
tenure_group3 = tenure_group %>% filter(Clusters==3)

head(tenure_group1)
head(tenure_group2)
head(tenure_group3)


#------------------------------------------------------------------------------------------------------------------#
#na.omit(customer_features)->customer_features 
model2 = kmeans(customer_features$TotalCharges,3)
model2

total_group = cbind(Total=customer_features$TotalCharges, Clusters=model2$cluster)
head(total_group)

total_group = as.data.frame(total_group)
total_group1 = total_group %>% filter(Clusters==1)
total_group2 = total_group %>% filter(Clusters==2)
total_group3 = total_group %>% filter(Clusters==3)

head(total_group1)
head(total_group2)
head(total_group3)


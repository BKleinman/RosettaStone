library(dplyr)

setwd("~/Desktop/MGSC 410/Final/Data")

# app usage data
df = read.csv("App activity (Chapman data extract, Apr 2020).csv")
summary(df)

# pie chart showing users by platform 
app_platforms = count(df,App.Session.Platform)
pie(app_platforms$n, labels = c("Other","Android","ios","Web"), main="Pie Chart of Platforms")

# uncleaned data
df_subs = read.csv("Subscriber Information (Chapman data extract, Apr 2020).csv")
summary(df_subs)

df_renew = df_subs %>% filter(Auto.Renew=="On")
summary(df_renew)

### MODELING ###
# reading 
df_modeling = read.csv("~/Desktop/MGSC 410/RosettaStone/cleandata.csv")
str(df_modeling)
summary(df_modeling)

# train test split
set.seed = 410
smp_size = floor(0.75 * nrow(df_modeling))
train_ind = sample(seq_len(nrow(df_modeling)), size = smp_size)
train = df_modeling[train_ind,]
test = df_modeling[-train_ind,]

# removing variables for modeling renewal with logistic regresssion
df_renew = df_modeling
df_renew$ID=NULL; df_renew$Subscription.Expiration=NULL; df_renew$Subscription.Start.Date=NULL;
df_renew$Currency=NULL; df_renew$Free.Trial.Expiration=NULL; df_renew$Free.Trial.Start.Date=NULL;
df_renew$User.Type=NULL; 

# clustering using gower distance
library(ggplot2)
library(cluster)
library(Rtsne)
library(dplyr)

df_gower = df_renew[1:5000,]
gower_dist = daisy(df_gower, metric = "gower")
gower_mat = as.matrix(gower_dist)

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df_gower %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


# logistic regression on auto renewel
model = glm(Auto.Renew~., data = train,family="binomial")

# logistic regression on lifetime

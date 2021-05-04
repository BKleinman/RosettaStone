setwd("/Users/brandonkleinman/Desktop/MGSC410/FinalProject")
#valid prices  7.99   11.99   179.99   299.99
subscribers <- read.csv("Subscribers.csv")
library("tidyverse")
library("rsample")



View(subscribers)
nrow(subscribers)

subscribers2 <- subscribers %>% filter(Purchase.Amount != "NULL")
nrow(subscribers2)
View(subscribers2)
subs <- within(subscribers2, {
  Subscription.Start.Date <- as.Date(Subscription.Start.Date,format = "%m/%d/%Y")
  Subscription.Expiration <- as.Date(Subscription.Expiration,format = "%m/%d/%Y")
  Purchase.Amount <- as.numeric(Purchase.Amount)
  Purchase.Amount <- ifelse(Purchase.Amount > 1000000, Purchase.Amount/10000000000, Purchase.Amount)
  Purchase.Amount <- ifelse(Purchase.Amount > 1000, Purchase.Amount/10000000, Purchase.Amount)
  Purchase.Amount <- ifelse(Currency == "EUR", Purchase.Amount/1.2, ifelse(Currency == "GBP", Purchase.Amount/1.38, Purchase.Amount))
  Currency <- as.factor(Currency)
  Push.Notifications <- as.factor(Push.Notifications)
  Email.Subscriber <- as.factor(Email.Subscriber)
  Lead.Platform <- as.factor(Lead.Platform)
  Free.Trial.User <-  as.factor(Free.Trial.User)
  User.Type <- as.factor(User.Type)
  Country <- as.factor(Country)
  Auto.Renew <- as.factor(Auto.Renew)
  Demo.User <- as.factor(Demo.User)
  Purchase.Store <- as.factor(Purchase.Store)
  Subscription.Event.Type <- as.factor(Subscription.Event.Type)
  Subscription.Type <- as.factor(Subscription.Type)
  Language <- as.factor(Language)
  Send.Count <- as.numeric(Send.Count)
  Open.Count <- as.numeric(Open.Count)
  Click.Count <- as.numeric(Click.Count)
  Unique.Click.Count <- as.numeric(Unique.Click.Count)
  Unique.Open.Count <- as.numeric(Unique.Open.Count)
  diff_in_days = as.integer(difftime(Subscription.Expiration, Subscription.Start.Date, units = "days"))
  diff_in_months = round(diff_in_days/30)
  diff_in_years = diff_in_days/365
  Purchase.Amount <- round(ifelse(diff_in_days == 0, 0, Purchase.Amount), digits = 2)
  Purchase.Amount <- ifelse((Subscription.Type == "Limited" & diff_in_years == 1), 95.88, Purchase.Amount)
  Purchase.Amount <- ifelse(Subscription.Expiration == as.Date("2099-01-01"), 299.99, Purchase.Amount)
})
glimpse(subs)
subs %>% View()
subs <- subs %>% filter(Send.Count < 2000) 
nrow(subs)

ggplot(subs, aes(x = Send.Count, y = Unique.Open.Count, color = Auto.Renew)) + 
  geom_point()


#ggplot(subs, aes(x = Currency)) + geom_bar() +
  #theme(axis.text.x = element_text(size = 5, angle = 90),
        #legend.title = element_text(size = 10), 
        #legend.text = element_text(size = 8),
       # axis.title.x = element_text(size = 10),
        #axis.title.y = element_text(size = 10),
        #axis.ticks.x = element_blank(),
        #plot.title = element_text(size = 12, face = "bold"),
        #panel.grid.major = element_blank(),
       # panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        #panel.background = element_blank())

names(subs)
write.csv(subs, file = "/Users/brandonkleinman/Desktop/MGSC410/FinalProject/cleandata.csv")
glimpse(subs)

set.seed(410)
subSplit <- initial_split(subs, p = 0.8)
subs_train <- training(subSplit)
subs_test <- testing(subSplit)

glimpse(subs_train)
View(subs)

lmPurchase <- lm(Purchase.Amount ~ Free.Trial.User + Email.Subscriber + Auto.Renew + 
                   Lead.Platform + User.Type + Demo.User + Language, data = subs_train)
summary(lmPurchase)
superClean <- subs %>% complete.cases()
clean <- subs[superClean,]

df <- clean %>% select(-c(Free.Trial.Start.Date, Free.Trial.Expiration, Subscription.Start.Date, Subscription.Expiration))
glimpse(df)

ggplot(subs, aes(x = Language)) + geom_bar() + facet_wrap(~Subscription.Event.Type) + theme(axis.text.x = element_text(size = 5, angle = 90),
  legend.title = element_text(size = 10), 
  legend.text = element_text(size = 8),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  axis.ticks.x = element_blank(),
  plot.title = element_text(size = 12, face = "bold"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank())

glimpse(spanish)
spanish <- subs %>% filter(Language == "ESP")
ggplot(spanish, aes(x = Country)) + geom_bar()

renewals <- subs %>% filter(Subscription.Event.Type == "RENEWAL")
numerator <- subs[subs$Language == "ESP" & subs$Subscription.Event.Type == "RENEWAL",]
denominator <- subs[subs$Language == "ESP" & subs$Subscription.Event.Type == "INITIAL_PURCHASE",]
nrow(numerator)/nrow(denominator)

mobile_users <-  subs %>% filter(Lead.Platform == "App")

numeratorM <- subs[mobile_users$Language == "ESP" & mobile_users$Subscription.Event.Type == "RENEWAL",]
denominatorM <- subs[mobile_users$Language == "ESP" & mobile_users$Subscription.Event.Type == "INITIAL_PURCHASE",]
nrow(numeratorM)/nrow(denominatorM)

numeratorVector <- subs[subs$Subscription.Event.Type == "RENEWAL",]
denominatorVector <- subs[subs$Subscription.Event.Type == "INITIAL_PURCHASE",]



subs2 <- subs %>% filter(Language != "DAR" & Language != "IND" & Language != "KIS" & Language != "LAT" & Language != "PAS" & Language != "URD")

numVector <- c(subs2 %>% group_by(Language) %>% filter(Subscription.Event.Type == "RENEWAL") %>%
                 summarise(num_users = n()))
denVector <- c(subs2 %>% group_by(Language) %>% filter(Subscription.Event.Type == "INITIAL_PURCHASE") %>%
                 summarise(num_users = n()))
retentions <- c(numVector[["num_users"]]/denVector[["num_users"]])
retention <- as.data.frame(retentions, row.names = c("ALL", "ARA", "CHI", "DEU", "EBR", "ENG", "ESC", "ESP", "FAR", "FRA", "GLE", "GRK",
                                                     "HEB", "HIN", "ITA", "JPN", "KOR", "NED",
                                                     "POL", "POR", "RUS", "SVE", "TGL", "TUR", "VIE"))
View(retention)
denVector[["Language"]]

write.csv(retention, file = "/Users/brandonkleinman/Desktop/MGSC410/FinalProject/retention.csv")


ggplot(subs, aes(x = Language, y = Purchase.Amount)) + geom_boxplot() + 
  theme(axis.text.x = element_text(size = 5, angle = 90),
  legend.title = element_text(size = 10), 
  legend.text = element_text(size = 8),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  axis.ticks.x = element_blank(),
  plot.title = element_text(size = 12, face = "bold"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank())

summary(subs)
subs <- subs %>% filter(Currency == "USD" || Currency == "GBP" || Currency == "EUR")
outliers <- subs %>% filter(Purchase.Amount > 300)
subs <- subs %>% filter(Purchase.Amount < 500)
View(outliers)


ggplot(subs, aes(x = Country, y = Purchase.Amount)) + geom_boxplot() + 
  theme(axis.text.x = element_text(size = 5, angle = 90),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

nrow(subs)
write.csv(subs, file = "/Users/brandonkleinman/Desktop/MGSC410/FinalProject/cleanData.csv")

t.test(Purchase.Amount~Email.Subscriber, data = subs, na.rm = T)
#Email subscribers tend to spend more than those who do not subscribe to emails
glimpse(subs)
pairwise.t.test(subs$Purchase.Amount, subs$Language, p.adj = "bonf")
pairwise.t.test(subs$Purchase.Amount, sub$Lead.Platform, data = subs, p.adj = "bonf")



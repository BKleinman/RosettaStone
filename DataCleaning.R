setwd("/Users/brandonkleinman/Desktop/MGSC410/FinalProject")
#valid prices  7.99   11.99   179.99   299.99
subscribers <- read.csv("Subscribers.csv")
library("tidyverse")
library("e1071")

View(subscribers)
nrow(subscribers)

subscribers2 <- subscribers %>% filter(Purchase.Amount != "NULL")
nrow(subscribers2)
View(subscribers2)
subs <- within(subscribers2, {
  Subscription.Start.Date <- as.Date(Subscription.Start.Date,format = "%m/%d/%Y")
  Subscription.Expiration <- as.Date(Subscription.Expiration,format = "%m/%d/%Y")
  Purchase.Amount <- as.numeric(Purchase.Amount)
  Purchase.Amount <- ifelse(Purchase.Amount > 1000, Purchase.Amount/1000000, Purchase.Amount)
  Purchase.Amount <- ifelse(Currency == "EUR", Purchase.Amount/1.2, ifelse(Currency == "GBP", Purchase.Amount/1.38, Purchase.Amount))
  Currency <- as.factor(Currency)
  Push.Notifications <- as.factor(Push.Notifications)
  Email.Subscriber <- as.factor(Email.Subscriber)
  Lead.Platform <- as.factor(Lead.Platform)
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
glimpse(subs)


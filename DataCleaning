setwd("/Users/brandonkleinman/Desktop/MGSC410/FinalProject")
#valid prices  7.99   11.99   179.99   299.99
subscribers <- read.csv("Subscribers.csv")
library("tidyverse")
library("naivebayes")

View(subscribers)
nrow(subscribers)

subscribers2 <- subscribers %>% filter(Purchase.Amount != "NULL")
nrow(subscribers2)
View(subscribers2)
subs <- within(subscribers2, {
  Subscription.Start.Date <- as.Date(Subscription.Start.Date,format = "%m/%d/%Y")
  Subscription.Expiration <- as.Date(Subscription.Expiration,format = "%m/%d/%Y")
  Purchase.Amount <- as.numeric(Purchase.Amount)
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
subs <- subs %>% filter(Send.Count < 2000) 
nrow(subs)

ggplot(subs, aes(x = Send.Count, y = Unique.Open.Count, color = Auto.Renew)) + 
  geom_point()

glimpse(subs)
View(subs)

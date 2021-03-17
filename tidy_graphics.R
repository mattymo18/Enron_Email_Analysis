#load data and libs
library(tidyverse)
library(gridExtra)
inbox.from.to <- read.csv("derived_data/Inbox.Outbox.csv") %>% 
  select(-X)

g1 <- inbox.from.to %>% 
  na.omit() %>% 
  count(to.mailname) %>% 
  arrange(desc(n)) %>% 
  head(30) %>% 
  ggplot(aes(x = reorder(to.mailname, n), y = n)) +
  geom_bar(stat = "identity", col = "blue", fill = "light blue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 30 Receivers", 
       x = "Email", 
       y = "Emails Received")

g2 <- inbox.from.to %>% 
  na.omit() %>% 
  count(from.mailname) %>% 
  arrange(desc(n)) %>% 
  #we don't want these senders, we want real people
  filter(from.mailname != "no.address" & from.mailname != "40enron" & from.mailname != "announcements.enron") %>% 
  head(30) %>% 
  ggplot(aes(x = reorder(from.mailname, n), y = n)) +
  geom_bar(stat = "identity", col = "blue", fill = "light blue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 30 Senders", 
       x = "Email", 
       y = "Emails Sent")

Graph1 <- grid.arrange(g1, g2, ncol = 2)
ggsave("derived_graphics/Top.30.Send.Receive.Plot.png", plot = Graph1)
ggsave("README_graphics/Top.30.Send.Receive.Plot.png", plot = Graph1)
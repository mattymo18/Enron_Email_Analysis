#data nd lib load
library(tidyverse)
library(gridExtra)

DF.Isolated <- read.csv("derived_data/Isolated.Data.csv") %>% 
  select(-X)

g1 <- DF.Isolated %>% 
  count(Source) %>% 
  ggplot(aes(x = reorder(Source, n), y = n)) +
  geom_bar(stat = "identity", col = "blue", fill = "light blue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 30 Receivers Isolated Network", 
       x = "Email", 
       y = "Emails Received") +
  ylim(0, 300)

g2 <- DF.Isolated %>% 
  count(Destination) %>% 
  ggplot(aes(x = reorder(Destination, n), y = n)) +
  geom_bar(stat = "identity", col = "blue", fill = "light blue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 30 Senders Isolated Network", 
       x = "Email", 
       y = "Emails Sent") +
  ylim(0, 300)

graph1 <- grid.arrange(g1, g2, ncol = 1)
ggsave("derived_graphics/isolated.sender.receiver.Plot.png", plot = graph1)

####### Source Destination Line plot ggplot #########

DF.Coords <- DF.Isolated %>% 
  group_by(Source, Destination) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

Coords.Send <- data.frame(
  Source = sort(unique(DF.Coords$Source)), 
  x.send = rep(1, length(unique(DF.Coords$Source))),
  y.send = seq(1, length(unique(DF.Coords$Source)))
)

Coords.Recieve <- data.frame(
  Destination = sort(unique(DF.Coords$Destination)), 
  x.receive = rep(5, length(unique(DF.Coords$Destination))),
  y.receive = seq(1, length(unique(DF.Coords$Destination)))
)

DF <- left_join(DF.Coords, Coords.Send)
DF <- left_join(DF, Coords.Recieve)

#lets get send and recieve coords separate
DF$Group <- 1:nrow(DF)

DF.Send <- DF %>% 
  select(Source, Destination, x.send, y.send, Count, Group) %>% 
  rename(X = x.send, 
         Y = y.send) %>% 
  filter(X == 1) %>% 
  arrange(Source)

DF.Dest <- DF %>% 
  select(Source, Destination, x.receive, y.receive, Count, Group) %>% 
  rename(X = x.receive, 
         Y = y.receive) %>% 
  filter(X == 5) %>% 
  arrange(Destination)

DF.Whole <- rbind(DF.Send, DF.Dest)

graph2 <- DF.Whole %>% 
  ggplot(aes(x = X, y = Y, group = Group)) +
  geom_point() +
  geom_line(aes(col = Count)) +
  geom_text(aes(label = ifelse(X == 1, Source, Destination)), hjust = ifelse(DF.Whole$X == 1, 1.1, -.1)) +
  geom_text(aes(x = 1, y = 0, label = "Source"), size = 5) +
  geom_text(aes(x = 5, y = 0, label = "Destination"), size = 5) +
  xlim(-.5, 6.5) +
  scale_color_distiller(name = "Emails Sent", palette = "Spectral") +
  theme_minimal() +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank()) +
  labs(title = "Isolated Network Email Exchanges")

ggsave("derived_graphics/isolated.network.exchange.lineplot.png", plot = graph2)
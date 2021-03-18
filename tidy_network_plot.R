### load libs and data
library(igraph)
library(tidyverse)

inbox.from.to <- read.csv("derived_data/Inbox.Outbox.csv") %>% 
  select(-X)

#ok, so the biggest network we can make is a 5454 since we have 
#this many users total sending emails to our users

#what would happen if a did a groupby for both to and from and did a count?

from.any.to.any.count <- tibble(inbox.from.to) %>% 
  filter(from.mailname != "40enron" & to.mailname != "40enron") %>% 
  filter(from.mailname != "2.ews" & from.mailname !="40ect" & from.mailname != "40ees") %>% 
  group_by(from.mailname, to.mailname, .drop = FALSE) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  rename("Source" = from.mailname, 
         "Destination" = to.mailname, 
         "Weight" = Count) %>% 
  head(100)

#ok lets try to make an edges node list
nodes <- tibble(
  "id" = 1:length(unique(c(from.any.to.any.count$Source, from.any.to.any.count$Destination))),
  "label" = unique(c(from.any.to.any.count$Source, from.any.to.any.count$Destination))
)
sent.recieve <- tibble(from.any.to.any.count)

edges <- sent.recieve %>% 
  left_join(nodes, by = c("Source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Destination" = "label")) %>% 
  rename(to = id)

edges1 <- edges %>% 
  select(from, to, Weight)

Network_igraph <- graph_from_data_frame(d = edges1, vertices = nodes, directed = T)
# radian.rescale <- function(x, start=0, direction=1) {
#   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
# }
# n <- 100
# lab.locs <- radian.rescale(x=1:n, direction=-1, start=0)

png("README_graphics/top.100.network.png", width = 2000, height = 2000)
plot.igraph(Network_igraph, vertex.size = 4, layout = layout_in_circle, 
            edge.color = edges1$Weight, edge.width = 1/15*edges$Weight, edge.curved = T, 
            vertex.label = nodes$id)
title("Top 100 Employee Email Count Network", cex.main = 6, col.main="black")
dev.off()
### load libs and data
library(igraph)
library(tidyverse)
set.seed(18)

inbox.from.to <- read.csv("derived_data/Inbox.Outbox.csv") %>% 
  select(-X)

#ok, so the biggest network we can make is a 5454 since we have 
#this many users total sending emails to our users

#what would happen if a did a groupby for both to and from and did a count?
inbox.from.to$to.mailname <- str_trim(inbox.from.to$to.mailname)
inbox.from.to$from.mailname <- str_trim(inbox.from.to$from.mailname)

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

#plot using node$id as vertex labels, can look back later to see who is who

#circle Plot
png("README_graphics/top.100.network.circle.png", width = 2000, height = 2000)
plot.igraph(Network_igraph, vertex.size = 4, layout = layout_in_circle, 
            edge.color = edges1$Weight, edge.width = 1/15*edges$Weight, edge.curved = T, 
            vertex.label = nodes$id)
title("Top 100 Employee Email Count Network", cex.main = 6, col.main="black")
dev.off()

#community detection

Network_igraph.non.direc <- graph_from_data_frame(d = edges1, vertices = nodes, directed = F)
Spectral.comun <- cluster_leading_eigen(Network_igraph.non.direc)

#graphopt with colored vertex for communities
png("derived_graphics/top.100.network.spectral.comunity.png", width = 2000, height = 2000)
plot(Network_igraph.non.direc, layout = layout_with_graphopt, vertex.size = 4, 
     layout = layout_with_graphopt, edge.color = "gray", edge.curved = T, 
     edge.width = 1/50*edges$Weight, vertex.label = nodes$label, 
     vertex.color = membership(Spectral.comun))
dev.off()

############################################ Isolated Plot ###################################

#now lets try to isolate this large group in the middle, 
#the rest seem a bit detached from others, but this group 
#in the middle seemed to be talking to everyone

#plus one of them the main people there is tim belden

#what if I just look at the larger plot and check out the names of people in here

DF.isolate <- from.any.to.any.count %>% 
  filter(Destination == "tim.belden" | Destination == "holden.salisbury" | 
           Source == "grace.rodriguez" | Source == "heather.dunton" | Destination == "jeff.dasovich" |
           Source == "d..steffes" | Destination == "paul.kaufman" | Source == "j..kean" |
           Destination == "joseph.alamo" | Destination == "j..kean" | Destination == "d..steffes" |
           Destination == "richard.shapiro" | Source == "jeff.dasovich") %>% 
  arrange(Source)

nodes <- tibble(
  "id" = 1:length(unique(c(DF.isolate$Source, DF.isolate$Destination))),
  "label" = unique(c(DF.isolate$Source, DF.isolate$Destination))
)
sent.recieve <- tibble(DF.isolate)

edges <- sent.recieve %>% 
  left_join(nodes, by = c("Source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Destination" = "label")) %>% 
  rename(to = id)

edges1 <- edges %>% 
  select(from, to, Weight)

Network_igraph.iso <- graph_from_data_frame(d = edges1, vertices = nodes, directed = F)

Spectral.comun.iso <- cluster_leading_eigen(Network_igraph.iso)

png("derived_graphics/isolated.network.spectral.comunity.png", width = 2000, height = 2000)
plot(Network_igraph.iso, layout = layout_with_graphopt, vertex.size = 4, 
     layout = layout_with_graphopt, edge.color = "black", edge.curved = T, 
     edge.width = 1/25*edges1$Weight, vertex.label = nodes$label, vertex.color = membership(Spectral.comun.iso), 
     vertex.label.cex = 3, edge.label = edges1$Weight, edge.label.cex = 3)
dev.off()

png("README_graphics/isolated.network.spectral.comunity.png", width = 2000, height = 2000)
plot(Network_igraph.iso, layout = layout_with_graphopt, vertex.size = 4, 
     layout = layout_with_graphopt, edge.color = "black", edge.curved = T, 
     edge.width = 1/25*edges1$Weight, vertex.label = nodes$label, vertex.color = membership(Spectral.comun.iso), 
     vertex.label.cex = 3, edge.label = edges1$Weight, edge.label.cex = 3)
dev.off()
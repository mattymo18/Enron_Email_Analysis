#Load in libs
library(tidyverse)

################################################# From anyone at Enron to our 136 employees set ##################
#Retrieval all files in the maildir
All.Files <- list.files("source_data/maildir/", full.names = T, recursive = T)

#Filter by inbox only
Inbox <- data.frame("File" = All.Files) %>% 
  filter(grepl("/inbox", File)) 

# Create list of sender and receiver (inbox owner)

DF.From.To.raw <- data.frame( #inboxes.from.to.raw.df
  from = apply(Inbox, 1, function(x){readLines(x, warn = F)[3]}),
  to = Inbox,
  stringsAsFactors = F
)

#Let's focus on email communications inside enron.
DF.From.To.raw2 <- DF.From.To.raw %>% 
  filter(grepl("@enron.com", from)) %>% 
  mutate(from.mailname = str_sub(from, 7, nchar(from) - 10)) %>% 
  mutate(to.username = sapply(str_split(File, "/"), "[", 4)) %>% 
  select(-c(from, File))

#find the unique users that are active
users <- data.frame(user.folder = paste0("source_data/maildir/", unique(DF.From.To.raw2$to.username)))
users <- users %>%
  mutate(sent = apply(users, 1, function(x){sum(grepl("sent", dir(x)))})) %>% 
  filter(sent != 0) %>% 
  select(-sent)

# Replace user.folder name with e-mail name
# this shows the difference between their username and their email name
users$to.mailname <- NA
for (i in 1:nrow(users)){
  sentmail <- dir(paste0(users$user.folder[i], "/sent_items/"))
  name <- readLines(paste0(users$user.folder[i], "/sent_items/", sentmail[1]), warn = F)[3]
  name <- str_sub(name, 7, nchar(name)-10)
  users$to.mailname[i] <- name
}

# ok so here we are going to make a list of all the emails to and from people using their email names
# looks like we narrowed it down to about 30000 emails to our 136 people and from anyone at enron
users <- users %>% 
  mutate(to.username = str_sub(user.folder, 21)) %>% 
  select(-user.folder)

Inbox.Final <- left_join(DF.From.To.raw2, users) %>% 
  select(-to.username)

length(unique(Inbox.Final$to.mailname))

write.csv(Inbox.Final, "derived_data/Inbox.From.Any.To.User.csv")
#########################################################################################################

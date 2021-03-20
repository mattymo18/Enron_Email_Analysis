library(tidyverse)

#start from the beginning
#Retrieval all files in the maildir
All.Files <- list.files("source_data/maildir/", full.names = T, recursive = T)

#Filter by inbox only
Inbox <- data.frame("File" = All.Files) %>% 
  filter(grepl("/inbox", File)) 

# Create list of sender and receiver (inbox owner)
# in this step I think I can just make a content string similar to how i get from
# ok so I can get the whole email in the content column, this will probably take for ever to do though
# but it's ok, I'll be able to search for words in these emails now

#took 5:22
DF.From.To.raw <- data.frame( #inboxes.from.to.raw.df
  from = apply(Inbox, 1, function(x){readLines(x, warn = F)[3]}),
  to = Inbox,
  content = as.character(apply(Inbox, 1, function(x){readLines(x, warn = F)})), 
  stringsAsFactors = F
)

#ok I still need to do the rest since I need to get from an email to an email to check both

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

#Filter by sent_items only
Sent.items <- data.frame("File" = All.Files) %>% 
  filter(grepl("/sent_items", File)) 

# Create list of sender and receiver (inbox owner)
# same as before getting content

# took 5:11
DF.To.From.raw <- data.frame(
  from = Sent.items,
  to = apply(Sent.items, 1, function(x){readLines(x, warn = F)[4]}),
  content = as.character(apply(Sent.items, 1, function(x){readLines(x, warn = F)})), 
  stringsAsFactors = F
)


DF.To.From.raw2 <- DF.To.From.raw %>% 
  filter(grepl("@enron.com", to)) %>% 
  mutate(to.mailname = str_sub(to, 5, nchar(to) - 10)) %>% 
  mutate(from.username = sapply(str_split(File, "/"), "[", 4)) %>% 
  select(-c(to, File))

#find the unique users that are active
users <- data.frame(user.folder = paste0("source_data/maildir/", unique(DF.To.From.raw2$from.username)))
users <- users %>%
  mutate(sent = apply(users, 1, function(x){sum(grepl("sent", dir(x)))})) %>% 
  filter(sent != 0) %>% 
  select(-sent)

# Replace user.folder name with e-mail name
# this shows the difference between their username and their email name
users$from.mailname <- NA
for (i in 1:nrow(users)){
  sentmail <- dir(paste0(users$user.folder[i], "/sent_items/"))
  name <- readLines(paste0(users$user.folder[i], "/sent_items/", sentmail[1]), warn = F)[3]
  name <- str_sub(name, 7, nchar(name)-10)
  users$from.mailname[i] <- name
}

# ok so here we are going to make a list of all the emails to and from people using their email names
# looks like we narrowed it down to about 30000 emails from our 136 people and from anyone at enron
users <- users %>% 
  mutate(from.username = str_sub(user.folder, 21)) %>% 
  select(-user.folder)

Sent.Final <- left_join(DF.To.From.raw2, users) %>% 
  select(-from.username)

Unique.Users.From <- data.frame(
  "from.mailname" = unique(Inbox.Final$to.mailname)
)

Unique.Users.to <- data.frame(
  "to.mailname" = unique(Sent.Final$from.mailname)
)

#then we can anti join

Anti.inbox <- anti_join(Inbox.Final, Unique.Users.From)
Anti.sentbox <- anti_join(Sent.Final, Unique.Users.to)

Final.DF <- full_join(Anti.inbox, Anti.sentbox)

DF.Isolated <- Final.DF %>% 
  rename("Source" = from.mailname, 
         "Destination" = to.mailname) %>% 
  filter((Source == "tim.belden" | Source == "holden.salisbury" | Source == "grace.rodriguez" | 
            Source == "heather.dunton" | Source == "jeff.dasovich" | Source == "d..steffes" |
            Source == "paul.kaufman" | Source == "j..kean" | Source == "joseph.alamo" | 
            Source == "richard.shapiro") & 
           (Destination == "tim.belden" | Destination == "holden.salisbury" | Destination == "grace.rodriguez" |
              Destination == "heather.dunton" | Destination == "jeff.dasovich" | Destination == "d..steffes" |
              Destination == "paul.kaufman" | Destination == "j..kean" | Destination == "joseph.alamo" |
              Destination == "richard.shapiro"))

#now I'll be able to look for specific content within this group
# I should add the rest of the nodes


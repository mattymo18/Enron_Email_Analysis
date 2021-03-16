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

#########################################################################################################

#ok, lets try ot fix the ones that look like this, "houston <.ward@", and "pr <.palmer@"
# it seems like they all look the same so we should be able to remove the error with the 
# @ and <

#seems to be in both sender and receiver and sometimes both, lets fix that

#lets deal with the both case first
inbox.both.issues <- Inbox.Final %>% 
  filter(grepl("<", to.mailname) & grepl("<", from.mailname)) %>% 
  #separate in half to remove the space and <
  separate(to.mailname, into = c("to.mailname.first", "to.mailname.last"), sep = " <") %>% 
  #remove the @ sign in the lastname
  separate(to.mailname.last, into = c("to.mailname.last", "Nothing"), sep = "@") %>% 
  #remove nothing column
  select(-Nothing) %>% 
  #rejoin name and last name
  unite("to.mailname", to.mailname.first:to.mailname.last, sep = "") %>% 
  #separate in half to remove the space and <
  separate(from.mailname, into = c("from.mailname.first", "from.mailname.last"), sep = " <") %>% 
  #remove the @ sign in the lastname
  separate(from.mailname.last, into = c("from.mailname.last", "Nothing"), sep = "@") %>% 
  #remove nothing column
  select(-Nothing) %>% 
  #rejoin name and last name
  unite("from.mailname", from.mailname.first:from.mailname.last, sep = "")
#now before I do the next step I have to join this back in
# and make a temp file to the rest of it can run on the set without
# the errors on both sides
inbox.from.to.temp1 <- Inbox.Final %>% 
  filter(!(grepl("<", from.mailname) & grepl("<", to.mailname)))
inbox.clean.temp <- rbind(inbox.from.to.temp1, inbox.both.issues)

inbox.to.issues <- inbox.clean.temp %>% 
  filter(grepl("<", to.mailname)) %>% 
  #separate in half to remove the space and <
  separate(to.mailname, into = c("to.mailname.first", "to.mailname.last"), sep = " <") %>% 
  #remove the @ sign in the lastname
  separate(to.mailname.last, into = c("to.mailname.last", "Nothing"), sep = "@") %>% 
  #remove nothing column
  select(-Nothing) %>% 
  #rejoin name and last name
  unite("to.mailname", to.mailname.first:to.mailname.last, sep = "")

inbox.from.issues <- inbox.clean.temp %>% 
  #ok found a really odd outlier I am going to remove all together first
  filter(!grepl("@enron@", from.mailname)) %>% 
  filter(grepl("<", from.mailname)) %>% 
  #separate in half to remove the space and <
  separate(from.mailname, into = c("from.mailname.first", "from.mailname.last"), sep = " <") %>% 
  #remove the @ sign in the lastname
  separate(from.mailname.last, into = c("from.mailname.last", "Nothing"), sep = "@") %>% 
  #remove nothing column
  select(-Nothing) %>% 
  #rejoin name and last name
  unite("from.mailname", from.mailname.first:from.mailname.last, sep = "")

#ok, now lets remove them from the original then rbind with these new ones
inbox.from.to.temp2 <- inbox.clean.temp %>% 
  filter(!grepl("<", from.mailname) & !grepl("<", to.mailname))

inbox.clean.final <- rbind(inbox.from.to.temp2, inbox.from.issues, inbox.to.issues)

write.csv(inbox.clean.final, "derived_data/Inbox.From.Any.To.User.csv")
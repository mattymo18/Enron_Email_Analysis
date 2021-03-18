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

write.csv(Inbox.Final, "derived_data/Inbox.Of.User.csv")

#########################################################################################################

############################################ From our 136 users to anyone at Enron ################################
#Filter by sent_items only
Sent.items <- data.frame("File" = All.Files) %>% 
  filter(grepl("/sent_items", File)) 

# Create list of sender and receiver (inbox owner)

DF.From.To.raw <- data.frame(
  from = Sent.items,
  to = apply(Sent.items, 1, function(x){readLines(x, warn = F)[4]}),
  stringsAsFactors = F
)


#Let's focus on email communications inside enron.
DF.From.To.raw2 <- DF.From.To.raw %>% 
  filter(grepl("@enron.com", to)) %>% 
  mutate(to.mailname = str_sub(to, 5, nchar(to) - 10)) %>% 
  mutate(from.username = sapply(str_split(File, "/"), "[", 4)) %>% 
  select(-c(to, File))

#find the unique users that are active
users <- data.frame(user.folder = paste0("source_data/maildir/", unique(DF.From.To.raw2$from.username)))
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

Sent.Final <- left_join(DF.From.To.raw2, users) %>% 
  select(-from.username)

write.csv(Sent.Final, "derived_data/Sent.By.User.csv")

#################################################################################################################


##################################### so now we wanto to join these in a meaningful way #########################

#so we will want to take out our 136 from the from.mailname of inbox
#and also our 136 from the to.mailname of sentbox

# we want to do these since this information would be duplicated with a join
# this is difficult since some of the duplicates are emails back and forth
# so we can't just remove all duplicates. 

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
###############################################################################################################

#ok, lets try ot fix the ones that look like this, "houston <.ward@", and "pr <.palmer@"
# it seems like they all look the same so we should be able to remove the error with the 
# @ and <

#seems to be in both sender and receiver and sometimes both, lets fix that

#lets deal with the both case first
inbox.both.issues <- Final.DF %>% 
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
inbox.from.to.temp1 <- Final.DF %>% 
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

inbox.clean.final <- na.omit(rbind(inbox.from.to.temp2, inbox.from.issues, inbox.to.issues))

#Ok last few things, we have a sitauation where people emailed more than one person
#we can fix that by first removing, then fixing, then adding back in
#this will be very similar to pivoting
#it will also boost peoples numbers a bit if they emailed more than one person often
#but the relationship between the people will remain in the network
#so we are ok with it
DF.Temp <- inbox.clean.final %>% 
  filter(!grepl("@", to.mailname))

DF.Issues <- inbox.clean.final %>% 
  filter(grepl("@", to.mailname)) %>% 
  #separate into different emails
  separate(to.mailname, into = paste0("to.", seq(1,3)), sep = ",") %>% 
  #make sure all the people work at enron, lets leave out number 2 right now
  filter(grepl("@enron", to.1) & grepl("@enron", to.2)) %>% 
  #take off the @ and enron.com
  mutate(to.mailname1 = str_sub(to.1, start = 1, end = nchar(to.1) - 10)) %>% 
  select(-to.1) %>% 
  mutate(to.mailname2 = ifelse(grepl("@", to.2), str_sub(to.2, start = 1, end = nchar(to.2) - 10), to.2)) %>% 
  select(-to.2) %>% 
  mutate(to.mailname3 = ifelse(grepl("@", to.3), str_sub(to.3, start = 1, end = nchar(to.3) - 2), to.3)) %>% 
  select(-to.3)

To.1 <- DF.Issues %>% 
  select(from.mailname, to.mailname1) %>% 
  rename("to.mailname" = to.mailname1)
To.2 <- DF.Issues %>% 
  select(from.mailname, to.mailname2) %>% 
  rename("to.mailname" = to.mailname2)
To.3 <- DF.Issues %>% 
  select(from.mailname, to.mailname3) %>% 
  rename("to.mailname" = to.mailname3)
DF.Fixed <- rbind(To.1, To.2, To.3)

DF.Final <- rbind(DF.Temp, DF.Fixed) %>% 
  filter(from.mailname != "40enron" & to.mailname != "40enron") %>% 
  filter(from.mailname != "2.ews" & from.mailname !="40ect" & from.mailname != "40ees")


write.csv(DF.Final, "derived_data/Inbox.Outbox.csv")
# Inbox.Outbox is messages from anyone at enron to our users and from our users to anyone at enron
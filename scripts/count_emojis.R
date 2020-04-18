
#install.packages("rjson")
library(purrr)
library(tidyverse)
library(jsonlite)
library(rjson)

dir = "~/Documents/projects/slack_emojis/"
setwd(dir)

data_loc = paste0(dir, "data/pritchard_lab")

# Choose channel
channel = "general"

# Read in file of user ids
user_file = paste0(data_loc, "/users.json")
user_df <- fromJSON(user_file)%>% as.data.frame() %>%
  select(id,name)

# read in all message files and turn into data frame
path <- paste0(data_loc, "/", channel)
files <- dir(path, pattern = "*.json")
data <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE)) 


# Pull out reactions
list_reactions <- data %>%
  filter(!reactions == "NULL") %>% 
  pull(reactions)

# flatten reactions lists into new data frame
reaction_df <- do.call(rbind, list_reactions) %>%
  #mutate(users = unlist(users))
  mutate(initiator = map_chr(users, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  dplyr::rename("emoji" = "name") %>%
  left_join(user_df, by = c("initiator" = "id")) 


# Plot emoji initiators
reaction_df %>%
  group_by(name, emoji) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
ggplot( aes(x = name, y = emoji, fill = n)) +
  geom_tile() +
  scale_fill_viridis()+
  theme_bw() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggtitle(paste0("Emoji initiators in #", channel))



# Popular emojis
reaction_df %>%
  group_by(emoji) %>%
  dplyr::summarize(total = sum(count)) %>%
  ungroup() %>%
  ggplot( aes(x = emoji, y = total)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis()+
  theme_bw() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggtitle(paste0("Emoji usage in Pritchard Lab slack space")) +
  coord_flip()

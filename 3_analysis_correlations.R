library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
library(ggcorrplot)

## reading everything in

# bookshelf
bookshelf <- read_csv('data/bookshelf.csv') %>% 
  pivot_longer(cols = organised_TP:unorganised_TA, names_to = "condition") %>%
  separate(condition, c("organisation", "target")) %>%
  rename(rt = "value", participant = "Participant") %>%
  mutate(logrt = log(rt),
         target = if_else(target=="TP", "present", "absent"),
         target = fct_relevel(target, "present"))

# jigsaw
jigsaws <- read_csv('data/jigsaws.csv')

jigsaws$fNumberOfPieces <- as.factor(jigsaws$NumberOfPieces)
jigsaws$logRT <- log(jigsaws$RT)


# lego search
lego_search <- read_csv('data/lego_search_corrected.csv') %>%
  filter(Participant != 62) %>% # this one has a note to say problems with setup
  pivot_longer(c(colour, shape, conjunction), names_to = "condition", values_to = "rt") %>%
  rename(participant = "Participant") %>%
  mutate(logrt = log(rt),
         condition = as_factor(condition),
                                 condition = fct_relevel(condition, "conjunction"))

# lego dots
dots <- read_csv('data/lego_dots.csv') %>%
  rename(participant = "Participant") %>%
  mutate(logrt = log(rt),
         difficulty = factor(difficulty, 
                             levels = c("easy", "medium", "difficult")),
         n_dots = recode_factor(difficulty, easy = "40", medium = "80", difficult = "120"),
         n_dots100 = parse_integer(as.character(n_dots))/100)

# elephant
elephant <- read_csv('data/building.csv') 

elephant <- elephant %>%
  mutate(participant = Participant) %>%
  select(participant, rt)

## grouping stuff

bookshelf_average <- bookshelf %>%
  group_by(participant) %>%
  summarise(bookshelf = mean(rt))

jigsaws_average <- jigsaws %>%
  mutate(participant = Participant) %>%
  group_by(participant) %>%
  summarise(jigsaws = mean(RT))


lego_average <- lego_search %>%
  group_by(participant) %>%
  summarise(lego = mean(rt))

dots_average <- dots %>%
  group_by(participant) %>%
  summarise(dots = mean(rt))

correlations <- full_join(lego_average, jigsaws_average)
correlations <- full_join(correlations, bookshelf_average)
correlations <- full_join(correlations, dots_average)
correlations <- full_join(correlations, elephant)

correlations <- correlations %>%
  select(-participant) %>%
  rename(elephant = rt) %>%
  drop_na(jigsaws, bookshelf, dots, elephant)

corr <- round(cor(correlations), 2)

ggcorrplot(corr, type = "lower",
           outline.col = "white",
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = "TRUE",
           legend.title = "Correlation")

ggsave('figures/correlations.png')

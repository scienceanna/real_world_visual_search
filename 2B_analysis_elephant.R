library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
options(mc.cores = 8)

theme_set(theme_bw())

#### building (overall) ####
building <- read_csv('data/building.csv')

# P1 - total times
elephant <- read_csv('data/elephant_detail.csv')

page1_total <- elephant %>%
  filter(LookedAt == "Turned page") %>%
  group_by(Participant) %>%
  slice(n())

# P2 - total times
elephant2 <- read_csv('data/elephant_detail_p2.csv')

page2_start <- elephant2 %>%
  filter(LookedAt == "Turned Page Start") %>%
  mutate(TimeStart = Time) %>%
  select(-Time, -LookedAt)

page2_end <- elephant2 %>%
  filter(LookedAt == "Turned Page End") %>%
  mutate(TimeEnd = Time) %>%
  select(-Time, -LookedAt)

page2_total <- left_join(page2_start, page2_end)

page2_total <- page2_total %>%
  mutate(p2_time = TimeEnd - TimeStart)

# Looking at Page 1 and Page 2 together

elephant_page1 <- elephant %>%
  mutate(Page = "1")

elephant_page2 <- elephant2 %>%
  mutate(Page = "2")

# want to have the same naming conventions

elephant_page2_rename <- elephant_page2 %>%
  mutate(LookedAt = case_when(LookedAt == "Turned Page Start" ~ "Turned page",
                              LookedAt == "Turned Page Forwards" ~ "Turned page",
                              LookedAt == "Turned Page End" ~ "Turned page",
                              LookedAt == "Turned Page Backwards" ~ "Turned page",
                              LookedAt == "Turned back to First Page" ~ "Turned page",
                              LookedAt == "Removes Incorrect Pieces" ~ "Correction",
                              LookedAt == "Instruction 9" ~ "Future instruction",
                              LookedAt == "Instruction 8" ~ "Future instruction",
                              LookedAt == "Instruction 7" ~ "Future instruction",
                              LookedAt == "Instruction 6" ~ "Future instruction",
                              LookedAt == "Instruction 5" ~ "Future instruction",
                              LookedAt == "Instruction 4" ~ "Instruction 2",
                              LookedAt == "Instruction 3" ~ "Instruction 1",
                              LookedAt == "Instruction 2" ~ "Previous instruction",
                              LookedAt == "Instruction 13" ~ "Future instruction",
                              LookedAt == "Instruction 12" ~ "Future instruction",
                              LookedAt == "Instruction 10" ~ "Future instruction",
                              LookedAt == "Instruction 1" ~ "Previous instruction",
                              LookedAt == "Corrects Colour" ~ "Correction",
                              LookedAt == "Lego" ~ "Lego",
                              LookedAt == "Building / Hand" ~ "Building / Hand",
                              LookedAt == "Turned Page" ~ "Turned page") 
         )

elephant_both <- rbind(elephant_page1, elephant_page2_rename)

elephant_time <- elephant_both %>%
  group_by(Participant) %>%
  mutate(changeFromPrev = Time - lag(Time, default = Time[1]),
         changeFromPrev = ifelse(changeFromPrev == 0, 0.5, changeFromPrev))


elephant_time_summarise <- elephant_time %>%
  group_by(Participant, LookedAt, Page) %>%
  summarise(totTime = sum(changeFromPrev))

ggplot(elephant_time_summarise, aes(LookedAt, totTime, 
                                    fill = Page)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.2) + 
  geom_jitter(aes(colour = Page), alpha = 0.5) +
  theme_bw(base_size = 18) +
  xlab('Area looked at') +
  ylab('Total time (s)') + 
  coord_flip()

ggsave("figures/exp4_tottime.png", width = 10, height = 4)

# page 1 - look backs and look aheads
elephant_lookahead <- elephant_time %>%
  filter(Page == "1") %>%
  group_by(Participant, LookedAt) %>%
  summarise(temp = min(Time)) %>%
  filter(LookedAt == "Instruction 2" | LookedAt == "Building / Hand") %>%
  pivot_wider(names_from = LookedAt, values_from = temp) %>%
  mutate(LookAhead = ifelse(`Instruction 2` > `Building / Hand`, 0, 1))

# so only 3 participants don't look ahead in the instructions before starting to build (this is possibly because page 1 is pretty simple though!)

elephant_lookahead2 <- elephant_time %>%
  filter(Page == "1") %>%
  group_by(Participant, LookedAt) %>%
  summarise(temp = min(Time)) %>%
  filter(LookedAt == "Instruction 2" | LookedAt == "Lego") %>%
  pivot_wider(names_from = LookedAt, values_from = temp) %>%
  mutate(LookAhead = ifelse(`Instruction 2` > Lego, 0, 1))

sum(elephant_lookahead2$LookAhead)
# around 37% (14/38) participants look at instruction 2 before they look at the Lego.

# Q3.
# do people go and look back at instruction 1 after they've already looked at instruction 2?

elephant_trackback <- elephant_time %>%
  filter(Page == "1") %>%
  group_by(Participant, LookedAt) %>%
  summarise(min_time = min(Time), max_time = max(Time)) %>%
  filter(LookedAt == "Instruction 1" | LookedAt == "Instruction 2") %>%
  pivot_longer(min_time:max_time, names_to = "MinMax", values_to = "Time") %>%
  pivot_wider(names_from = c(LookedAt, MinMax), values_from = Time) %>%
  mutate(TrackBack = ifelse(`Instruction 1_max_time` > `Instruction 2_min_time`, 1, 0))

sum(elephant_trackback$TrackBack)
# the majority of participants 'track back' to previous instructions (31/38 - 82%)

### plotting time course

#subcluster a is participant 10
# subcluster b is participant 50
# subcluster c is participant 49

library(DiagrammeR)

a_graph <- grViz("digraph {

splines=ortho;
 
node [layout = dot, rankdir = LR]
edge [colorscheme = rdylgn11]

subgraph cluster_a {
label = 'Participant A';
color = white;

I1 [label ='I1']
L [label = 'L']
I2 [label = 'I2']
B [label = 'B']
T [label = 'T']

I1 -> L [color = 2]
L -> I2 [color = 3]
I2 -> L [color = 4]
L -> B [color = 5]
B -> L [color = 6]
L -> B [color = 7]
B -> L [color = 8]
L -> B [color = 9]
B -> T [color = 10]
}


subgraph cluster_b {
label = 'Participant B'
color = white;

I1b [label = 'I1']
Lb [label = 'L']
I2b [label = 'I2']
Bb [label = 'B']
Tb [label = 'T']

I1b -> Lb [color = 1]
Lb -> I1b [color = 2]
I1b -> Lb [color = 3]
Lb -> I2b [color = 4]
I2b -> Bb [color = 5]
Bb -> Lb [color = 6]
Lb -> Bb [color = 7]
Bb -> Lb [color = 8]
Lb -> Bb [color = 9]
Bb -> I2b [color = 10]
I2b -> Tb [color = 11]
}


subgraph cluster_c {
label = 'Participant C'
color = white;

I1c [label = 'I1']
Lc [label = 'L']
I2c [label = 'I2']
Bc [label = 'B']
Tc [label = 'T']

I1c -> Lc [color = 1]
Lc -> I1c [color = 1]
I1c -> Tc [color = 1]
Tc -> I1c [color = 2]
I1c -> I2c [color = 2]
I2c -> I1c [color = 2]
I1c -> Lc [color = 3]
Lc -> I1c [color = 3]
I1c -> Lc [color = 3]
Lc -> I1c [color = 4]
I1c -> I2c [color = 4]
I2c -> Lc [color = 5]
Lc -> I2c [color = 5]
I2c -> I1c [color = 5]
I1c -> I2c [color = 6]
I2c -> Bc [color = 6]
Bc -> I2c [color = 7]
I2c -> Bc [color = 7]
Bc -> I2c [color = 7]
I2c -> Bc [color = 8]
Bc -> Lc [color = 8]
Lc -> Bc [color = 8]
Bc -> I2c [color = 9]
I2c -> I1c [color = 9]
I1c -> Bc [color = 9]
Bc -> Lc [color = 10]
Lc -> I1c [color = 10]
I1c -> Bc [color = 10]
Bc -> I2c [color = 11]
I2c -> I1c [color = 11]
I1c -> Tc [color = 11]
}

}")


# 2. Convert to SVG, then save as png
tmp = DiagrammeRsvg::export_svg(a_graph)
tmp = charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, "figures/strategies.png") # saved graph as png in current working directory

### correlations between search time and total time

search_task_page1 <- elephant_time_summarise %>%
  filter(Page == "1", LookedAt == "Lego")

search_task_page2 <- elephant_time_summarise %>%
  filter(Page == "2", LookedAt == "Lego")

elephant_compare_page1 <- left_join(building, search_task_page1)
elephant_compare_page1 <- elephant_compare_page1 %>%
  drop_na()

elephant_compare_page2 <- left_join(building, search_task_page2)
elephant_compare_page2 <- elephant_compare_page2 %>%
  drop_na()

elephant_compare <- rbind(elephant_compare_page1, elephant_compare_page2)

ggplot(elephant_compare, aes(totTime, rt)) + geom_point() +
  geom_smooth(method = "lm") +
  xlab('Time taken on search task') +
  ylab('Time taken to complete') +
  theme_bw() + facet_wrap(~Page)

cor.test(elephant_compare_page1$rt, elephant_compare_page1$totTime, method = "spearman")
cor.test(elephant_compare_page2$rt, elephant_compare_page2$totTime, method = "spearman")


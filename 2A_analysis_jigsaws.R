library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
options(mc.cores = 8)

theme_set(theme_bw())

jigsaws <- read_csv('data/jigsaws.csv')

jigsaws$fNumberOfPieces <- as.factor(jigsaws$NumberOfPieces)
jigsaws$logRT <- log(jigsaws$RT)

jigsaws_plot <- ggplot(jigsaws, aes(fNumberOfPieces, RT, fill = Box)) + geom_boxplot() +
  theme_bw(base_size = 18) + scale_fill_brewer(palette="Set1", labels = c("Box", "No Box")) + 
  ylab("RT (s)") + xlab("Number of jigsaw pieces") + theme(legend.position="bottom")

my_priors <- c(prior(normal(0, 1), class = "b"),
               prior(exponential(1), class = "sigma"))



m_jigsaws <- brm(logRT ~ 0 + Box:fNumberOfPieces + (1|Participant), 
              data = jigsaws,
              prior = my_priors)


# plot fixed effects
jigsaws %>% modelr::data_grid(fNumberOfPieces, Box) %>% 
  add_epred_draws(m_jigsaws, re_formula = NA) %>%
  select(-.chain, -.iteration) %>%
  mutate(Box = as_factor(Box)) %>%
  ggplot(aes(exp(.epred), y = Box)) + 
  stat_histinterval(data = jigsaws, aes(x = RT, fill = Box), point_interval = NULL, breaks = 20, 
                    alpha = 0.5,
                    outline_bars = TRUE, slab_colour = "black") + 
  stat_interval(.width = c(0.53, 0.97)) +
  scale_x_continuous("rt", limits = c(0, 700)) + 
  facet_wrap(~fNumberOfPieces) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Purples",direction  = -1) + 
  scale_colour_brewer(palette = "Purples",direction  = -1) +
  ggtitle("(i) Distributions and Model Predictions for 2A") -> plt_jigsaws1

get_variables(m_jigsaws)

m_jigsaws %>% spread_draws(`b_Boxbox:fNumberOfPieces12`, `b_Boxnobox:fNumberOfPieces12`, 
                        `b_Boxbox:fNumberOfPieces16`, `b_Boxnobox:fNumberOfPieces16`,
                        `b_Boxbox:fNumberOfPieces20`, `b_Boxnobox:fNumberOfPieces20`,
                        `b_Boxbox:fNumberOfPieces24`, `b_Boxnobox:fNumberOfPieces24`) %>%
  rename(box_12 = "b_Boxbox:fNumberOfPieces12", 
         nobox_12 = "b_Boxnobox:fNumberOfPieces12",
         box_16 = "b_Boxbox:fNumberOfPieces16",
         nobox_16 = "b_Boxnobox:fNumberOfPieces16",
         box_20 = "b_Boxbox:fNumberOfPieces20",
         nobox_20 = "b_Boxnobox:fNumberOfPieces20",
         box_24 = "b_Boxbox:fNumberOfPieces24",
         nobox_24 = "b_Boxnobox:fNumberOfPieces24") %>%
  pivot_longer(-c(.chain, .iteration, .draw), names_to = "param", values_to = "b") %>%
  separate(param, c("Box", "fNumberOfPieces"), sep = "_") %>%
         mutate(Box = fct_relevel(Box, "box")) %>%
  select(-.chain, -.iteration,) -> bs

bs %>%
  ggplot(aes(exp(b), fill = Box)) + 
  geom_density(alpha = 0.5)  + 
  geom_jitter(data = jigsaws, aes(x = RT, y=  -0.1, colour = Box),
              width = 0.05, height = 0.08, alpha = 0.5) + 
  facet_wrap(~fNumberOfPieces) +
  scale_x_continuous("rt", limits = c(0,801)) +
  scale_y_continuous("density") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  ggtitle("(ii) Posterior Densities for Task 2A") -> plt_jigsaws2

bs %>% pivot_wider(names_from = "Box", values_from = "b") %>%
  mutate(difference = nobox - box) -> diff

sum(diff$difference > 0)/nrow(diff)

diff %>%
  ggplot(aes(difference, fill = fNumberOfPieces)) + 
  geom_density(alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2)   +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  ggtitle("(iii) Differences Between Conditions") -> plt_jigsaws3

(plt_jigsaws2 + plt_jigsaws3)  +  plot_layout(widths = c(2, 1))

ggsave("figures/exp3.png", width = 10, height = 4)

# Strategies

jigsaw_strat <- jigsaws %>%
  drop_na(Strategy) %>%
  mutate(Strategy = (case_when(
    Strategy  == "edges" ~ "edge",
    Strategy == "corners" ~ "edge",
    Strategy == "mixed" ~ "mixed")))

ggplot(jigsaw_strat, aes(Box, RT, fill = Strategy)) + 
  geom_boxplot(alpha = 0.5, outliers = FALSE) + 
  geom_jitter(position = position_jitterdodge(), aes(colour = Strategy)) +
  facet_wrap(~fNumberOfPieces)

ggsave("figures/exp3b.png", width = 10, height = 4)

jigsaw_strat_summary <- jigsaw_strat %>%
  mutate(Strategy_code = (case_when(
    Strategy  == "edge" ~ 1,
    Strategy == "mixed" ~ 0))) %>%
  group_by(Participant) %>%
  summarise(tot = n(),
            sum_edge = sum(Strategy_code),
            percent_edge = sum_edge/tot,
            median_RT = median(RT))

ggplot(jigsaw_strat_summary, aes(percent_edge, median_RT)) + geom_point()

       
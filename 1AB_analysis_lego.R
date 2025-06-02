library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
options(mc.cores = 8)

theme_set(theme_bw())

########################################
# Sample size justification
########################################

# 1A: lego blocks
# want to be able to detect at least a 20% increase in RT
# This sort of roughly matches Sauter et al (2020)

sim_data <- tibble(
  cond1 = rnorm(10, 10, 1),
  cond2 = rnorm(10, 12, 1),
  participant = 1:10
)

sim_data <- sim_data %>%
  pivot_longer(cols = cond1:cond2, names_to = "condition", values_to = "rt") %>%
  mutate(logrt = log(rt))

my_priors <- c(prior(normal(1.5, 1), class = "b"),
               prior(exponential(1), class = "sigma"))

m_sim <- brm(logrt ~ 0 + condition, 
             data = sim_data,
             prior = my_priors)

# compare conditions
get_variables(m_sim)

m_sim %>% spread_draws(b_conditioncond1, b_conditioncond2) %>%
  rename(cond1 = "b_conditioncond1", 
         cond2 = "b_conditioncond2") %>%
  mutate(d_comp = cond2 - cond1) %>%
  select(-.chain, -.iteration,- .draw) -> bs

sum(bs$d_comp > 0)/nrow(bs)

# 1B: lego dots

# slope from previous data looks to be about 0.18s increase per dot.

n <- 10

sim_data_dots <- tibble(
  dots = rep(seq(40, 120, by = 20), n),
  rt = 18 + (0.18 * dots) + rnorm(5*n, 0, 5),
  participant = rep(1:n, each = 5)
)

ggplot(sim_data_dots, aes(dots, rt)) + geom_jitter() + geom_smooth(method = "lm")

sim_data_dots <- sim_data_dots %>%
  mutate(logrt = log(rt), dots100 = dots/100)

m_dots <- brm(logrt ~ dots100 + (1|participant), 
              data = sim_data_dots,
              prior = my_priors,
              chains = 4)

summary(m_dots)

ggplot(sim_data_dots, aes(dots100, logrt)) + geom_jitter() + geom_smooth(method = "lm")

########################################
# 1A: lego blocks
########################################

lego_search <- read_csv('data/lego_search_corrected.csv') %>%
  filter(Participant != 62) %>% # this one has a note to say problems with setup
  pivot_longer(c(colour, shape, conjunction), names_to = "condition", values_to = "rt") %>%
  rename(participant = "Participant") %>%
  mutate(logrt = log(rt),
         condition = as_factor(condition),
                                 condition = fct_relevel(condition, "conjunction"))

lego_search_summary <- lego_search %>%
  group_by(condition) %>%
  summarise(mean_RT = mean(rt),
            sd_RT = sd(rt))

ggplot(lego_search_summary, aes(condition, mean_RT)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), position = "dodge", width = 0.25) + 
  theme_bw()

ggplot(lego_search, aes(log(rt), fill = condition)) + geom_histogram(alpha = 0.3)

# define priors

my_priors <- c(prior(normal(1.5, 1), class = "b"),
               prior(exponential(1), class = "sigma"))

validate_prior(rt ~ 0 + condition, 
          data = lego_search,
          prior = my_priors,
          family = lognormal())

m <- brm(logrt ~ 0 + condition, 
              data = lego_search,
              prior = my_priors,
              sample_prior = "only")

lego_search %>% modelr::data_grid(condition) %>% 
  add_epred_draws(m) %>%
  select(-.chain, -.iteration) %>%
  ggplot(aes(exp(.epred), y = condition, fill = condition)) + 
  stat_interval(.width = c(0.53, 0.95), alpha = 0.75, stroke = 5) +
  scale_color_brewer() + 
  scale_x_log10()

m_blks <- brm(logrt ~ 0 + condition + (1|participant), 
         data = lego_search,
         prior = my_priors,
         chains = 4)

# plot fixed effects
lego_search %>% modelr::data_grid(condition) %>% 
  add_epred_draws(m_blks, re_formula = NA) %>%
  select(-.chain, -.iteration) %>%
  mutate(condition = as_factor(condition),
         condition = fct_relevel(condition, "conjunction")) %>%
  ggplot(aes(exp(.epred), y = condition)) + 
  stat_histinterval(data = lego_search, aes(x = rt, fill = condition), 
                    point_interval = NULL, breaks = 20, 
                    alpha = 0.5,
                    outline_bars = TRUE, slab_colour = "black",
                    limits = c(0, 5)) + 
  stat_interval(.width = c(0.53, 0.97)) +
  scale_x_continuous("rt", limits = c(0, 30)) + 
  scale_color_brewer(palette = "Purples",direction  = -1) + 
  theme(legend.position = "none") + 
  ggtitle("(i) Distributions & Model Predictions for Task 1A") -> plt_blocks1

# compare conditions
get_variables(m_blks)

m_blks %>% spread_draws(b_conditioncolour, b_conditionconjunction, b_conditionshape) %>%
  rename(conjunction = "b_conditionconjunction", 
         colour = "b_conditioncolour",
         shape = "b_conditionshape") %>%
  mutate(d_colour = conjunction - colour,
         d_shape = conjunction - shape) %>%
  select(-.chain, -.iteration,- .draw) -> bs

sum(bs$d_colour > 0)/nrow(bs)
sum(bs$d_shape > 0)/nrow(bs)

bs %>% select(colour, conjunction, shape) %>%
  pivot_longer(everything(), names_to = "condition", values_to = "b") %>%
  mutate(condition = as_factor(condition),
         condition = fct_relevel(condition, "conjunction")) %>%
  ggplot(aes(b, fill = condition)) + 
  geom_density(alpha = 0.5)  + 
  theme(legend.position = "none") +
  ggtitle("(ii) Posterior Densities for Task 1A") -> plt_blocks2


bs %>% select(`conjunction - colour` = "d_colour", `conjunction - shape` = "d_shape") %>%
  pivot_longer(everything(), names_to = "condition", values_to = "b") %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(b)) + 
  geom_density(alpha = 0.75, fill = "grey") +
  geom_vline(xintercept = 0, linetype = 2)  + 
  facet_wrap(~condition) +
  ggtitle("(iii) Differences Between Conditions") -> plt_blocks3

#rm(m, bs, lego_search_summary)

########################################
# 1B: lego dots
########################################

dots <- read_csv('data/lego_dots.csv') %>%
  rename(participant = "Participant") %>%
  mutate(logrt = log(rt),
         difficulty = factor(difficulty, 
                             levels = c("easy", "medium", "difficult")),
         n_dots = recode_factor(difficulty, easy = "40", medium = "80", difficult = "120"),
         n_dots100 = parse_integer(as.character(n_dots))/100)


m_dots <- brm(logrt ~ n_dots100 + (1|participant), 
         data = dots,
         prior = my_priors,
         chains = 4)

# plot fixed effects
dots %>% modelr::data_grid(n_dots100) %>% 
  add_epred_draws(m_dots, re_formula = NA, ndraws = 100) %>%
  select(-.chain, -.iteration) %>%
  ggplot(aes(100*n_dots100, exp(.epred))) + 
  geom_jitter(data = dots, aes(y = rt), 
              height = 0, width = 2, alpha = 0.5)                            + 
  geom_path(aes(group = .draw), alpha = 0.1, colour = "purple") + 
  scale_y_continuous("rt", limits = c(10, 50)) + 
  scale_x_continuous("number of LEGO dots") + 
  ggtitle("(iv) Data and Model Predictions for Task 1B") -> plt_dots1

get_variables(m_dots)

m_dots %>% spread_draws(b_n_dots100) %>%
  rename(slope = "b_n_dots100") %>%
  select(-.chain, -.iteration,- .draw) -> bs

bs %>%  ggplot(aes(slope)) + 
  geom_density(alpha = 0.75, fill = "grey") + 
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("(v) Posterior Density for Task 1B") -> plt_dots2


plt_blocks1 + (plt_blocks2 / plt_blocks3) + plt_dots1 + plt_dots2

ggsave("figures/exp1.png", width = 10, height = 6)

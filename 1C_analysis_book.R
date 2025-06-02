library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)
options(mc.cores = 8)

theme_set(theme_bw())

#######################################
### sample size justification ###
#######################################
n <- 40

sim_data <- tibble(
  organised_absent = rnorm(n, 1.2, 0.5),
  organised_present = rnorm(n, 1, 0.5),
  unorganised_absent = rnorm(n, 1.4, 0.5),
  unorganised_present = rnorm(n, 1.2, 0.5),
  participant = 1:n
  
)

sim_data <- sim_data %>%
  pivot_longer(cols = organised_absent:unorganised_present, names_to = "condition", values_to = "rt") %>%
  separate_wider_delim(condition, delim = "_", names = c("organisation", "target")) %>%
  mutate(logrt = log(rt))

my_priors <- c(prior(normal(1.5, 1), class = "b"),
               prior(exponential(1), class = "sigma"))

m_book <- brm(logrt ~ 0 + organisation:target + (1|participant), 
              data = sim_data,
              prior = my_priors)

m_book %>% spread_draws(`b_organisationorganised:targetabsent`, `b_organisationunorganised:targetabsent`, 
                        `b_organisationorganised:targetpresent`, `b_organisationunorganised:targetpresent`) %>%
  rename(organised_TA = "b_organisationorganised:targetabsent", 
         unorganised_TA = "b_organisationunorganised:targetabsent",
         organised_TP = "b_organisationorganised:targetpresent",
         unorganised_TP = "b_organisationunorganised:targetpresent") %>%
  pivot_longer(-c(.chain, .iteration, .draw), names_to = "param", values_to = "b") %>%
  separate(param, c("organisation", "target"), sep = "_") %>%
  mutate(target = fct_recode(target, present = "TP", absent = "TA"),
         target = fct_relevel(target, "present"))%>%
  select(-.chain, -.iteration,) -> bs

bs %>% pivot_wider(names_from = "organisation", values_from = "b") %>%
  mutate(difference = unorganised - organised) -> diff

sum(diff$difference > 0)/nrow(diff)

########################################
#### bookshelf ####
########################################

bookshelf <- read_csv('data/bookshelf.csv') %>% 
  pivot_longer(cols = organised_TP:unorganised_TA, names_to = "condition") %>%
  separate(condition, c("organisation", "target")) %>%
  rename(rt = "value", participant = "Participant") %>%
  mutate(logrt = log(rt),
         target = if_else(target=="TP", "present", "absent"),
         target = fct_relevel(target, "present"))

# define priors

my_priors <- c(prior(normal(1.5, 1), class = "b"),
               prior(exponential(1), class = "sigma"))



m_book <- brm(logrt ~ 0 + organisation:target + (1|participant), 
         data = bookshelf,
         prior = my_priors)


# plot fixed effects
bookshelf %>% modelr::data_grid(organisation, target) %>% 
  add_epred_draws(m_book, re_formula = NA) %>%
  select(-.chain, -.iteration) %>%
  mutate(organisation = as_factor(organisation)) %>%
  ggplot(aes(exp(.epred), y = organisation)) + 
  stat_histinterval(data = bookshelf, aes(x = rt, fill = organisation), point_interval = NULL, breaks = 20, 
                    alpha = 0.5,
                    outline_bars = TRUE, slab_colour = "black") + 
  stat_interval(.width = c(0.53, 0.97)) +
  scale_x_continuous("rt", limits = c(0, 45)) + 
  scale_color_brewer(palette = "Purples",direction  = -1) + 
  facet_wrap(~target) + 
  theme(legend.position = "none") + 
  ggtitle("(i) Distributions and Model Predicitons for Task 2") -> plt_books1

# compare conditions
get_variables(m_book)


m_book %>% spread_draws(`b_organisationorganised:targetabsent`, `b_organisationunorganised:targetabsent`, 
                        `b_organisationorganised:targetpresent`, `b_organisationunorganised:targetpresent`) %>%
  rename(organised_TA = "b_organisationorganised:targetabsent", 
         unorganised_TA = "b_organisationunorganised:targetabsent",
         organised_TP = "b_organisationorganised:targetpresent",
         unorganised_TP = "b_organisationunorganised:targetpresent") %>%
  pivot_longer(-c(.chain, .iteration, .draw), names_to = "param", values_to = "b") %>%
  separate(param, c("organisation", "target"), sep = "_") %>%
  mutate(target = fct_recode(target, present = "TP", absent = "TA"),
         target = fct_relevel(target, "present"))%>%
  select(-.chain, -.iteration,) -> bs

bs %>%
  ggplot(aes(exp(b), fill = interaction(target, organisation) )) + 
  geom_density(alpha = 0.5)  + 
  geom_jitter(data = bookshelf, aes(x = rt, y=  -0.2, 
                                      colour =  interaction(target, organisation)),
                width = 0.05, height = 0.1, alpha = 0.5) + 
    scale_x_continuous("rt", limits = c(0,30)) +
  scale_y_continuous("density") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  ggtitle("(ii) Posterior Densities for Task 1C") -> plt_books2


bs %>% pivot_wider(names_from = "organisation", values_from = "b") %>%
  mutate(difference = unorganised - organised) -> diff

sum(diff$difference > 0)/nrow(diff)

diff %>%
  ggplot(aes(difference, fill = target)) + 
  geom_density(alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2)   +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  ggtitle("(iii) Differences Between Conditions") -> plt_books3

 (plt_books2 + plt_books3)  +  plot_layout(widths = c(2, 1))

ggsave("figures/exp2.png", width = 10, height = 4)

library(tidyverse)

set.seed(1234)

# typical psychometric function
pf <- function(threshold, stim) {
  p_seen = 1 - pnorm(stim, threshold, sd = 1)
}

# create set of typical pf
crossing(threshold = 0:40, 
         stim = seq(0, 40, 0.1)) %>% 
  mutate(p_seen = pf(threshold, stim)) -> functional_pf

# zero function
tibble(stim = seq(0, 40, 0.1)) %>% 
  mutate(threshold = -1, .before = 1) %>% 
  mutate(p_seen = 0) -> zero_pf

bind_rows(zero_pf, 
          functional_pf) -> df_pf

df_pf

# fig.1A
df_pf %>% 
  ggplot(aes(x = stim, y = p_seen, group = threshold)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Stimulus luminance (dB)",
       y = "Probability of seen") +
  scale_x_continuous(limits = c(-1, 40))


# calculate weights for each typical psychometric function
psy_weight <- function(mu, sigma, lamda) {
  tibble(threshold = 0:40, 
       weight = dnorm(0:40, mu, sigma)) %>% 
  mutate(weight = (1 - lamda) * weight) %>% 
  add_row(tibble(threshold = -1, weight = lamda), .before = 1)
}


# load parameters of empirical FOS curve
# sensitivity (full threshold) + 1 = sens (SITA equivalent)
read_csv("estimated_parameters.csv") %>% 
  transmute(sens = sensitivity + 1,
            par = parameter,
            value = mean) %>% 
  pivot_wider(names_from = "par",
              values_from = "value") -> df_par

df_par

df_par %>% 
  filter(sens == 20)

# fig.1B
psy_weight(20.6, 6.13, 0.235) %>% 
  ggplot(aes(x = threshold, y = weight)) +
  geom_col(width = 1.0) +
  scale_x_continuous(breaks = c(-1, 10, 20, 30, 40),
                     labels = c("<0", "10", "20", "30", "40")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Threshold (dB)",
       y = "Weight")


# create data for fig.2
df_par %>% 
  filter(sens %in% c(6, 14, 20, 26, 32)) %>% 
  mutate(weight = pmap(.l = list(mu, sigma, lamda), 
                       .f = psy_weight)) -> df_weight

df_weight

df_weight %>% 
  select(sens, weight) %>% 
  unnest(cols = weight) %>% 
  ggplot(aes(x = threshold, y = weight)) +
  geom_col(aes(fill = as.factor(sens)), width = 1.0) +
  scale_x_continuous(breaks = c(-1, 10, 20, 30, 40),
                     labels = c("<0", "10", "20", "30", ""),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ sens, nrow = 1) +
  labs(x = "Threshold (dB)", y = "Weight") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())


# create FOS curve
weighted_curve <- function(weight) {
  df_pf %>% 
  left_join(weight, by = "threshold") %>% 
  mutate(w_p_seen = p_seen * weight) %>% 
  group_by(stim) %>% 
  summarise(p_seen = sum(w_p_seen))
}


df_weight %>% 
  mutate(p_seen = map(.x = weight, .f = weighted_curve)) %>% 
  select(sens, p_seen) %>% 
  unnest(cols = p_seen) %>% 
  ggplot(aes(x = stim, y = p_seen)) +
  geom_line(aes(color = as.factor(sens)), linewidth = 1.0) +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Stimulus luminance (dB)",
       y = "Probability of seen") +
  theme(legend.position = "none")


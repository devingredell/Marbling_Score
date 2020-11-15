
# Calculate Parameters ----------------------------------------------------



# calculate mean and sd by group

params <- dat %>%
  filter(!is.na(juiciness)) %>%
  group_by(group) %>%
  summarize(mean = mean(juiciness, na.rm = T),
            sd = sd(juiciness, na.rm = T),
            meanlog = fitdistrplus::fitdist(juiciness, "lnorm")$estimate[[1]],
            sdlog = fitdistrplus::fitdist(juiciness, "lnorm")$estimate[[2]],
            shape = fitdistrplus::fitdist(juiciness, "weibull")$estimate[[1]],
            scale = fitdistrplus::fitdist(juiciness, "weibull")$estimate[[2]])


# Normal Distribution -----------------------------------------------------


# create norm date for density curve
norm_data <- list()
set.seed(13)
for(i in seq_along(levels(params$group))){

  norm_data[[i]] <- tibble(group = params[i, ]$group,
                                value = rnorm(1000000,
                                              params[i, ]$mean,
                                              params[i, ]$sd))
}

# remove data outside sensory scale range
norm_data <- bind_rows(norm_data)





# weib Distribution ------------------------------------------------------



weib_data <- list()
set.seed(13)
for(i in seq_along(levels(params$group))){

  weib_data[[i]] <- tibble(group = params[i, ]$group,
                                value = rweibull(1000000,
                                              shape = params[i, ]$shape,
                                              scale = params[i, ]$scale))

}

weib_data <- bind_rows(weib_data)




# Log Normal --------------------------------------------------------------



log_data <- list()
set.seed(13)
for(i in seq_along(levels(params$group))){

  log_data[[i]] <- tibble(group = params[i, ]$group,
                            value = rlnorm(1000000,
                                           meanlog = params[i, ]$meanlog,
                                           sdlog = params[i, ]$sdlog))

}

log_data <- bind_rows(log_data)






# Probabilities -----------------------------------------------------------



obs_probs <- dat %>%
  group_by(group) %>%
  filter(juiciness >= 7.5) %>%
  count() %>%
  inner_join(., params) %>%
  mutate(mean = round(mean, 1),
         obs_prob = (n / 50) * 100,
         obs_prob = paste0(obs_prob, "%")) %>%
  arrange(rev(group)) %>%
  select(-c(n, sd))

log_probs <- log_data %>%
  group_by(group) %>%
  filter(value >= 7.5) %>%
  count() %>%
  inner_join(., params) %>%
  mutate(mean = round(mean, 1),
         log_prob = round((n / 1000000) * 100, 0),
         log_prob = paste0(log_prob, "%")) %>%
  arrange(rev(group)) %>%
  select(-c(n, sd))

weib_probs <- weib_data %>%
  group_by(group) %>%
  filter(value >= 7.5) %>%
  count() %>%
  inner_join(., params) %>%
  mutate(mean = round(mean, 1),
         weib_prob = round((n / 1000000) * 100, 0),
         weib_prob = paste0(weib_prob, "%")) %>%
  arrange(rev(group)) %>%
  select(-c(n, sd))

norm_probs <- norm_data %>%
  group_by(group) %>%
  filter(value >= 7.5) %>%
  count() %>%
  inner_join(., params) %>%
  mutate(mean = round(mean, 1),
         norm_prob = round((n / 1000000) * 100, 0),
         norm_prob = paste0(norm_prob, "%")) %>%
  arrange(rev(group)) %>%
  select(-c(n, sd))


probs <- inner_join(obs_probs, log_probs) %>%
  inner_join(., weib_probs) %>%
  inner_join(., norm_probs)

probs


# Plots -------------------------------------------------------------------


# obs vs norm vs log
log_data %>%
  ggplot(aes(x = value, y = group, fill = group)) +
  geom_density_ridges(color = NA) +
  geom_density_ridges(data = dat, aes(x = juiciness, y = group),
                      size = 1, fill = NA) +
  geom_density_ridges(data = norm_data, aes(x = value, y = group),
                      size = 1, color = "grey", fill = NA) +
  geom_vline(xintercept = 7.5, size = 2) +
  theme(legend.position = "none")

# obs vs weib
weib_data %>%
  ggplot(aes(x = value, y = group, fill = group)) +
  geom_density_ridges(color = NA) +
  geom_density_ridges(data = dat, aes(x = juiciness, y = group),
                      size = 1, fill = NA) +
  geom_vline(xintercept = 7.5, size = 2) +
  theme(legend.position = "none")


# obs vs norm
norm_data %>%
  ggplot(aes(x = value, y = group, fill = group)) +
  geom_density_ridges(color = NA) +
  geom_density_ridges(data = dat, aes(x = juiciness, y = group),
                      size = 1, fill = NA) +
  geom_vline(xintercept = 7.5, size = 2) +
  theme(legend.position = "none")

# weib vs norm
weib_data %>%
  ggplot(aes(x = value, y = group, fill = group)) +
  geom_density_ridges(color = NA) +
  geom_density_ridges(data = norm_data, aes(x = value, y = group),
                      size = 1, fill = NA) +
  geom_vline(xintercept = 7.5, size = 2) +
  theme(legend.position = "none")
max(norm_data$value)


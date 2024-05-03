library(scales)


eastern_df <- filter_reg %>% filter(country == "Western")
lm_all <- lm(filter_reg, formula = log(fert_emiss_ha) ~ log(yield) + factor(country))
lm_eastern <- lm(eastern_df, formula = log(fert_emiss_ha) ~ log(yield))

lm_all$coefficients[2] # 1.353147
lm_eastern$coefficients[2] # 2.191207

pred_eastern_with_all <- predict(lm_all, newdata = eastern_df)
pred_eastern_itself <- predict(lm_eastern, newdata = eastern_df)

eastern_df <- data.frame(eastern_df, pred_with_all = exp(pred_eastern_with_all), 
                         pred_itself = exp(pred_eastern_itself))

eastern_df <- eastern_df %>%
  select(country, year, yield, fert_emiss_ha, pred_with_all, pred_itself) %>%
  pivot_longer(!c(country, year, yield), names_to = "scenario", values_to = "fert_emiss")

eastern_df %>%
  ggplot(aes(x = log(yield), y = log(fert_emiss), col = scenario)) +
  geom_line()

####
filter_reg %>%
  filter(year == 2020) %>%
  ggplot(aes(x = yield, y = fert_emiss_ha)) +
  geom_point() +
  geom_smooth(method = "gam")

lm_2020 <- lm(filter_reg %>% filter(year == 2020), formula = log(fert_emiss_ha) ~ log(yield)) 
lm_2020$coefficients[2] # 0.7722751

lm_1970 <- lm(filter_reg %>% filter(year == 1970), formula = log(fert_emiss_ha) ~ log(yield)) 
lm_1970$coefficients[2] # 0.9077005

lm_2000 <- lm(filter_reg %>% filter(year == 2000), formula = log(fert_emiss_ha) ~ log(yield)) 
lm_2000$coefficients[2] # 0.8281306

lm_all$coefficients[2]

filter_reg %>%
  #filter(year %in% 1990:2020) %>%
  filter(log(yield) > 12) %>%
  ggplot(aes(x = yield, y = fert_emiss_ha)) +
  geom_point() +
  scale_x_continuous(trans = log_trans()) +
  scale_y_continuous(trans = log_trans()) +
  geom_smooth(method = "gam")

lm_en <- lm(filter_reg %>% filter(year %in% 1990:2020), formula = log(en_emiss_ha) ~ log(yield))
summary(lm_en)
lm_en$coefficients[2]

low_yield_obs <- filter_reg %>%
  filter()

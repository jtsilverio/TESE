tuco = readRDS("../01_data/activity_processed/tuco_processed.rds")

daily_budget_id = tuco %>% 
    group_by(season, ID, date = lubridate::date(datetime)) %>% 
    summarise(Rest   = sum(state == "rest")/n() * 100, 
              Medium = sum(state == "medium")/n() * 100, 
              High   = sum(state == "high")/n() *100,
              "Total Active" = Medium + High) %>% 
    group_by(season, ID) %>% 
    summarise(Rest   = mean(Rest), 
              Medium = mean(Medium), 
              High   = mean(High),
              "Total Active" = Medium + High)

daily_budget_id = daily_budget_id %>% 
    select(1:5) %>% 
    tidyr::pivot_longer(cols = c("Rest","Medium","High"),
                        names_to = "state",
                        values_to = "perc") %>% 
    mutate(state = factor(state, c("Rest","Medium","High"),
                          labels = c("Rest","Medium","High")))

# AOV HIGH ---------------------------------------------------------------------
aov_high = aov(data = daily_budget_id %>% filter(state == "High"), formula = perc~season)
summary(aov_high)
#plot(aov_high)
TukeyHSD(aov_high)

# AOV MEDIUM -------------------------------------------------------------------
aov_medium = aov(data = daily_budget_id %>% filter(state == "Medium"), formula = perc~season)
summary(aov_medium)
#plot(aov_medium)
TukeyHSD(aov_medium)

# AOV REST ---------------------------------------------------------------------
aov_rest = aov(data = daily_budget_id %>% filter(state == "Rest"), formula = perc~season)
summary(aov_rest)
#plot(aov_rest)
TukeyHSD(aov_rest)



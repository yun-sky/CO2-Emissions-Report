library(dplyr)
library(tidyverse)
library(janitor)
library(GGally)

# 1.0 Cleaning the Data
veh <- read_csv("data/CO2 Emissions_Canada.csv")
head(veh)

# Only Select Variables we are Interested in
veh_clean <- readr::read_csv("data/CO2 Emissions_Canada.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    make                  = factor(make),
    vehicle_class         = factor(vehicle_class),
    engine_size           = engine_size_l,
    fuel_type             = factor(fuel_type),
    fuel_consumption      = fuel_consumption_comb_l_100_km,
    co2_emissions         = co2_emissions_g_km
  ) %>%
  drop_na()


# 1.1 Plotting Correlation Plot

# We can observe that there is multicolinearity between
# Engine size and fuel consumption since engine size and
# fuel consumption is highly correlated
num <- veh_clean %>%
    select(co2_emissions, 
          engine_size, 
          fuel_consumption)
ggcorr(num, label = TRUE, hjust = 0.9, size = 3)


# 1.2 FUEL TYPE

# I noticed there is only one type of car using the N fuel, pretty strange!
# So, we remove the one with fuel type equal to N
veh_clean <- veh_clean %>% filter(fuel_type != "N")

veh_clean %>%
  group_by(fuel_type) %>%
  summarise(n = n(),
            mean_co2 = mean(co2_emissions),
            sd_co2 = sd(co2_emissions)) %>%
  arrange(desc(mean_co2))

ggplot(veh_clean, aes(x = fuel_type, y = co2_emissions)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "CO2 by Fuel Type", x = "Fuel Type", y = "CO2 (g/km)")


# 
(ggplot(veh_clean, aes(x = fuel_consumption, y=co2_emissions, color=fuel_type))
  + geom_point())

# Fit the Data to the
lm_fit <- lm(co2_emissions ~ fuel_type + fuel_consumption + fuel_consumption*fuel_type, data=veh_clean)
summary(lm_fit)


(ggplot(veh_clean, aes(x = fuel_consumption, y=co2_emissions, color=fuel_type))
  + geom_point()
  + geom_abline(intercept=-0.1172, slope=26.8992))




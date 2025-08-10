#install.packages("tidyverse")
#install.packages("janitor")

veh <- read_csv("CO2 Emissions_Canada.csv")
head(veh)

library(tidyverse)
library(janitor)

veh_clean <- readr::read_csv("CO2 Emissions_Canada.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    make                  = factor(make),
    vehicle_class         = factor(vehicle_class),
    engine_size           = engine_size_l,
    fuel_type             = factor(fuel_type),
    fuel_consumption = fuel_consumption_comb_l_100_km,
    co2_emissions         = co2_emissions_g_km
  ) %>%
  drop_na()

# To sky: I drop the model since there are too many models, and some models are pretty similar, like 335i and 335i xDRIVE.
# but you can add them by typing model = factor(model),  :)
# Also drop     fuel_consumption_city = fuel_consumption_city_l_100_km,
#               fuel_consumption_hwy  = fuel_consumption_hwy_l_100_km,
#               fuel_consumption_comb_mpg     = fuel_consumption_comb_mpg,
# Because these variable are highly correlated, especially fuel_consumption_comb_l_100km and fuel_consumption_comb_mpg have 
# extremely high inverse correlation. I think it's a bed idea to put them in the same model.

glimpse(veh_clean)


# Code for create a correlation heat map (numeric-only)
library(GGally)
num <- veh_clean %>%
  dplyr::select(co2_emissions, engine_size, fuel_consumption)

GGally::ggcorr(num, label = TRUE, hjust = 0.9, size = 3)



# Code for create 3 side-by-side boxplot
# In this code, the value eta2_vc, eta2_ft and eta2_mk means 
# the proportion of total variance in CO₂ explained by each categorical variable alone
# for example, eta2_vc = 0.35 means the vehicle class explains ~35% of the variability in CO₂.
# which is higher than other two, so maybe this variable can explain response very strong.

# 1) VEHICLE CLASS
veh_clean %>%
  group_by(vehicle_class) %>%
  summarise(n = n(),
            mean_co2 = mean(co2_emissions),
            sd_co2 = sd(co2_emissions)) %>%
  arrange(desc(mean_co2))

ggplot(veh_clean, aes(x = fct_reorder(vehicle_class, co2_emissions, median),
                      y = co2_emissions)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_flip() +
  labs(title = "CO2 by Vehicle Class", x = "Vehicle Class", y = "CO2 (g/km)")

fit_vc <- aov(co2_emissions ~ vehicle_class, data = veh_clean)
summary(fit_vc)
ss_vc <- summary(fit_vc)[[1]][["Sum Sq"]]
eta2_vc <- ss_vc[1] / sum(ss_vc)  # eta-squared (effect size)
eta2_vc


# 2) FUEL TYPE
# I noticed there is only one type of car using the N fuel, pretty strange!

veh_clean %>%
  group_by(fuel_type) %>%
  summarise(n = n(),
            mean_co2 = mean(co2_emissions),
            sd_co2 = sd(co2_emissions)) %>%
  arrange(desc(mean_co2))

ggplot(veh_clean, aes(x = fuel_type, y = co2_emissions)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "CO2 by Fuel Type", x = "Fuel Type", y = "CO2 (g/km)")

fit_ft <- aov(co2_emissions ~ fuel_type, data = veh_clean)
summary(fit_ft)
ss_ft <- summary(fit_ft)[[1]][["Sum Sq"]]
eta2_ft <- ss_ft[1] / sum(ss_ft)
eta2_ft


# 3) MAKE (lump to top 12)
veh_make <- veh_clean %>%
  mutate(make_top = forcats::fct_lump_n(make, n = 12, other_level = "Other"))

veh_make %>%
  group_by(make_top) %>%
  summarise(n = n(),
            mean_co2 = mean(co2_emissions),
            sd_co2 = sd(co2_emissions)) %>%
  arrange(desc(mean_co2))

ggplot(veh_make, aes(x = fct_reorder(make_top, co2_emissions, median),
                     y = co2_emissions)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_flip() +
  labs(title = "CO2 by Make (Top 12 + Other)", x = "Make", y = "CO2 (g/km)")

fit_mk <- aov(co2_emissions ~ make_top, data = veh_make)
summary(fit_mk)
ss_mk <- summary(fit_mk)[[1]][["Sum Sq"]]
eta2_mk <- ss_mk[1] / sum(ss_mk)
eta2_mk






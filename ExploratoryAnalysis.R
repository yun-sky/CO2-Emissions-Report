# Dependencies
# install.packages(c("dplyr", "tidyverse", "janitor", "GGally", "leaps"))
library(dplyr)
library(tidyverse)
library(janitor)
library(GGally)
library(leaps)

# 1.0 EXPLORATORY ANALYSIS
veh <- read_csv("data/CO2 Emissions_Canada.csv")
head(veh)

# Only Select explanatory we are interested in, since the other
# fuel-consumption are closely related 
veh_clean <- readr::read_csv("data/CO2 Emissions_Canada.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    make                  = factor(make),
    vehicle_class         = factor(vehicle_class),
    engine_size           = engine_size_l,
    fuel_type             = factor(fuel_type),
    fuel_con              = fuel_consumption_comb_l_100_km,
    co2_emissions         = co2_emissions_g_km
  ) %>%
  drop_na()


# 1.1 Plotting Correlation Plot between engine_size and fuel_type
# We can observe that there is multi-colinearity between
# Engine size and fuel consumption since engine size and
# fuel consumption is highly correlated
num <- veh_clean %>%
    select(co2_emissions, 
          engine_size, 
          fuel_con)

# Correlation Plot
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

# VISUALIZATION
# It appears that both fuel type and fuel consumption effect CO2 emissions
(ggplot(veh_clean, aes(x = fuel_con, y=co2_emissions, color=fuel_type))
  + geom_point()
  + ylab("CO2 Emissions (g/KM)")
  + xlab("Fuel Consumption (L/KM)")
  + labs(title="CO2 Emissions vs Fuel Consumption"))


# MODEL FITTING: Assessing Linear Terms
# Fit the Data to the

# Model 1: Interaction term and Independent term
lm_1 <- lm(co2_emissions ~ fuel_type + fuel_con + fuel_con*fuel_type, data=veh_clean)

# Model 2: No Interaction (Only Intercepts Change)
lm_2 <- lm(co2_emissions ~ fuel_type + fuel_con, data=veh_clean)

# Model 3: Only Interaction (Only Slopes Change)
lm_3 <- lm(co2_emissions ~ fuel_type:fuel_con, data=veh_clean)

model_1_g <- function(base_b0, base_b1) {
  (ggplot(veh_clean, aes(x = fuel_con, y=co2_emissions, color=fuel_type))
    + geom_point()
    + xlab("Fuel Consumption (L/100KM)")
    + ylab("C02 (g/km)")
    + labs(title="Model 1: CO2 vs Fuel Consumption by Fuel Type")
    + geom_abline(intercept = base_b0, 
                  slope=base_b1, 
                  linetype="dashed") #D
    + geom_abline(intercept = base_b0 + 4.7926, 
                  slope=base_b1 -10.8615, 
                  linetype="dashed") #E
    + geom_abline(intercept = base_b0 + 0.5462, 
                  slope=base_b1 -3.6270, 
                  linetype="dashed") #X
    + geom_abline(intercept = base_b0 + 0.7303, 
                  slope=base_b1 -3.6622,
                  linetype="dashed")) #Z
}
model_2_g <- function(base_b0, base_b1) {
  (ggplot(veh_clean, aes(x = fuel_con, y=co2_emissions, color=fuel_type))
   + geom_point()
   + xlab("Fuel Consumption (L/100KM)")
   + ylab("C02 (g/km)")
   + labs(title="Model 2: CO2 vs Fuel Consumption by Fuel Type")
   + geom_abline(intercept = base_b0, 
                 slope=base_b1, 
                 linetype="dashed") #D
   + geom_abline(intercept = base_b0 -145.32792, 
                 slope=base_b1, 
                 linetype="dashed") #E
   + geom_abline(intercept = base_b0 -30.89114, 
                 slope=base_b1, 
                 linetype="dashed") #X
   + geom_abline(intercept = base_b0 -30.45786, 
                 slope=base_b1,
                 linetype="dashed")) #Z
}
model_3_g <- function(base_b0) {
  (ggplot(veh_clean, aes(x = fuel_con, y=co2_emissions, color=fuel_type))
   + geom_point()
   + xlab("Combined Fuel Consumption")
   + ylab("C02")
   + labs(title="Model 3: CO2 vs Fuel Consumption by Fuel Type")
   + geom_abline(intercept = base_b0, 
                 slope=26.81844, 
                 linetype="dashed") #D
   + geom_abline(intercept = base_b0, 
                 slope=16.27134, 
                 linetype="dashed") #E
   + geom_abline(intercept = base_b0, 
                 slope=23.25451, 
                 linetype="dashed") #X
   + geom_abline(intercept = base_b0, 
                 slope=23.23654,
                 linetype="dashed")) #Z
}

# Visualizations
model_1_g(-0.1172, 26.8992)
model_2_g(36.23268, 22.78507)
model_3_g(0.61810)


# 3.1 Model Fitting Selection: P-Value, Adjusted-R^2 and Residual Standard Error
summary(lm_1)
summary(lm_2)
summary(lm_3)



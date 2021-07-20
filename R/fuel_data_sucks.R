library(tidyverse)
library(arrow)

fn <- file.choose()
rawfuel <- read_parquet(fn)
rawfuel <- rawfuel |> 
  rename(date = month) |> 
  mutate(month = lubridate::month(date),
         quarter = lubridate::quarter(date, with_year = TRUE, fiscal_start = 10))
glimpse(rawfuel)

rawfuel |> summary()

# clean GGEs - truncate outliers

fuel <- 
  rawfuel |> 
  filter(!(purchased_fuel_type %in% c("LPG"))) |> 
  group_by(purchased_fuel_type) |> 
  filter(gge > 0) |> 
  mutate(upper_bound = 1.5 * IQR(gge),
         gge = if_else(gge > upper_bound, upper_bound, gge)) |> 
  select(-upper_bound) |> 
  ungroup() |> 
  group_by(vehicle_type) |> 
  filter(average_monthly_mileage > 0) |> 
  drop_na(average_monthly_mileage) |> 
  mutate(mmb = 2 * IQR(average_monthly_mileage),
         average_monthly_mileage = if_else(average_monthly_mileage > mmb,
                                           mmb,
                                           average_monthly_mileage)) |> 
  select(-mmb) |> 
  ungroup()

glimpse(fuel)         

fuel |> 
  group_by(fuel_class) |> 
  summarize(min = min(gge), 
            q_25 = quantile(gge, .25),
            mean = mean(gge), 
            median = median(gge),
            q_75 = quantile(gge, .75),
            max = max(gge))

fuel |> 
  group_by(fuel_class) |> 
  summarize(min = min(average_monthly_mileage), 
            q_25 = quantile(average_monthly_mileage, .25),
            mean = mean(average_monthly_mileage), 
            median = median(average_monthly_mileage),
            q_75 = quantile(average_monthly_mileage, .75),
            max = max(average_monthly_mileage))

fuel |> 
  drop_na(gge, average_monthly_mileage) |> 
  ggplot(aes(x = quarter, y = gge)) +
  geom_bar(stat = "identity", aes(fill = fuel_class))

set.seed(18860222)
fuel |> 
  filter(lubridate::year(date) %in% c(2020, 2021)) |> 
  group_by(fuel_class) |> 
  slice_sample(n = 100) |> 
  drop_na(gge, average_monthly_mileage) |> 
  ggplot(aes(x = average_monthly_mileage, y = gge)) +
  geom_point(pch = 19, aes(col = fuel_class), alpha = .5) +
  scale_y_log10() +
  scale_x_log10()

fuel |> 
  filter(lubridate::year(date) %in% c(2020, 2021)) |> 
  drop_na(gge, average_monthly_mileage) |> 
  mutate(mpg = average_monthly_mileage/gge) |> 
  group_by(fuel_class) |> 
  summarise(min = min(mpg),
            q25 = quantile(mpg, .25),
            mean = mean(mpg),
            q75 = quantile(mpg, .75),
            max = max(mpg))

fuel |> 
  filter(lubridate::year(date) %in% c(2020, 2021)) |> 
  drop_na(gge, average_monthly_mileage) |> 
  mutate(mpg = average_monthly_mileage/gge) |> 
  group_by(purchased_fuel_type) |> 
  mutate(iqr = IQR(mpg)) |> 
  filter(mpg < 2 * iqr) |> 
  summarise(min = min(mpg),
            q25 = quantile(mpg, .25),
            mean = mean(mpg),
            q75 = quantile(mpg, .75),
            max = max(mpg))

cfuel <-
  fuel |> 
  filter(lubridate::year(date) %in% c(2020, 2021)) |> 
  drop_na(gge, average_monthly_mileage) |> 
  mutate(mpg = average_monthly_mileage/gge) |> 
  group_by(purchased_fuel_type) |> 
  mutate(iqr = IQR(mpg)) |> 
  filter(mpg < 2 * iqr)

glimpse(cfuel)
cfuel |> 
  filter(lubridate::year(date) == 2021) |> 
  ggplot(aes(x = average_monthly_mileage, y = mpg)) +
  geom_point(alpha = .7, pch = 19)

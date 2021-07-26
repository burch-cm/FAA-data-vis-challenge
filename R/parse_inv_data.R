library(dplyr)
library(readxl)

##### crosswalks #####
fuel_class <-
  tibble(fuel_type = c('Diesel','Ethanol / Unleaded Gasoline','Gasoline',
                       'Plug-In Hybrid Electric/Gas','Biodiesel B20',
                       'Gasoline Hybrid Electric','Unleaded Gasoline',
                       'Dedicated Electric','Cng / Unleaded Gasoline Dual','Cng Diesel'),
         fuel_abbr  = c("DSL", "E85", "GAS", 
                        "PHEV", "B20", 
                        "HEV", "GAS", 
                        "FEV", "CNG", "CNG"),
         fuel_class = c("DSL", "E85", "GAS", 
                        "HEV", "B20/CNG", 
                        "HEV", "GAS", 
                        "FEV", "B20/CNG", "B20/CNG")) |> 
  mutate(class_group = if_else(fuel_abbr %in% c("B20", "CNG", "E85", "LPG", "HEV"),
                               "Alternative",
                               if_else(fuel_class %in% c("PHEV", "FEV"), 
                                       "Electric", "Petroleum")))

vehicle_class <-
  tibble(vehicle_type = c('Hd','Med & Heavy Cab And Chassis','Ld Suv 4X4',
                          'Heavy Tractors','Ld Suv 4X2','Ld Minivan 4X2 (Passenger)',
                          'Ld Minivan 4X2 (Cargo)','Ld Van 4X2 (Passenger)',
                          'Ld Pickup 4X2','Md Pickup',
                          'Md Van (Cargo)','Ld Pickup 4X4','Md Other',
                          'Md Suv','Ld Van 4X4 (Passenger)',
                          'Ld Minivan 4X4 (Passenger)','Sedan/St Wgn Compact',
                          'Sedan/St Wgn Subcompact','Ld Van 4X2 (Cargo)',
                          'Sedan/St Wgn Midsize',
                          'Md Van (Passenger)','Md Bus','Hd Bus',
                          'Sedan','Ld Van 4X4 (Cargo)','Sedan/St Wagon'),
         vehicle_class = c("Other", "Other", "SUV", 
                           "Other", "SUV", "Van", 
                           "Van", "Van", 
                           "Pickup", "Pickup", 
                           "Van", "Pickup", "Other",
                           "SUV", "Van",
                           "Van", "Sedan",
                           "Sedan", "Van",
                           "Sedan",
                           "Van", "Other", "Other",
                           "Sedan", "Van", "Sedan"))

##### inventory #####

inv <- 
  readxl::read_excel(here::here("rawdata/Leased and Owned Inventory by LOB May 2021.xlsx"),
                     sheet = "inv") |> 
  janitor::clean_names() |> 
  dplyr::left_join(fuel_class, by = "fuel_type") |> 
  dplyr::left_join(vehicle_class, by = "vehicle_type") |> 
  select(-c(tag, vin, starts_with("contact"), starts_with("garage_address"),
            fleet_manager, starts_with("fsr"), starts_with("level"),
            customer_number, exp_org, cost_center_desc))

##### write to inventory #####

readr::write_rds(inv, here::here("data/fleet_inv.rds"))
readr::write_rds(inv, here::here("app/appdata/fleet_inv.rds"))
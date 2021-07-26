library(vroom)
library(dplyr)

isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

fuel <- 
  vroom::vroom("./rawdata/FUR_Report_modified.csv", 
               delim = ",",
               col_types = list(Tag = "c",
                                VIN = "c",
                                Mileage = "i",
                                `Vehicle Fuel Type` = "c",
                                Agency = "c",
                                Bureau = "c",
                                `Agency Ind` = "c",
                                Region = "i",
                                FMC = "c",
                                `Account Code (BOAC)` = "c",
                                Serial = "i",
                                `Purch Date` = "D",
                                date_txt = "d",
                                `Transaction Time` = "c",
                                Name = "c",
                                Address = "c",
                                City = "c",
                                State = "c",
                                ZIP = "c",
                                `Purchased Fuel Type` = "c",
                                `GGE Units` = "d"
                                )) |> 
  select(vehicle_fuel = `Vehicle Fuel Type`,
         date = `Purch Date`,
         state = State,
         zip = ZIP,
         purchase_fuel = `Purchased Fuel Type`,
         units = `GGE Units`) |> 
  tidyr::drop_na(date) |> 
  dplyr::filter(date >= as.Date("2020-10-01")) |> 
  # remove negative values
  dplyr::mutate(units = abs(units)) |> 
  # remove outliers over 3sd away from median (MAD: Med Abs Dev)
  dplyr::filter(isnt_out_mad(units))                
  readr::write_rds(fuel, "./app/appdata/fur_2018_2021.rds")
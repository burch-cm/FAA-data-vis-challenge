library(vroom)

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
         units = `GGE Units`)

readr::write_rds(fuel, "./data/fur_2018_2021.rds")
                
library(tidyverse) |> suppressPackageStartupMessages()
library(vroom)

cn <- c('vehicle_region', 'vehicle_class_tag', 'year', 'make', 'model', 'vin', 
        'fuel_type', 'drivers_first_name', 'drivers_mi', 'drivers_last_name',
        'customer_agency_name', 'customer_agency_indicator', 'account_code_boac',
        'serial', 'customer_address', 'customer_address_2', 'customer_address_3',
        'customer_city', 'customer_state', 'customer_zip', 'customer_contact',
        'area_code', 'phone_number', 'extension', 'control_number', 'accident_date',
        'reported_date', 'police_report', 'status_of_accident', 'incident_accident',
        'rural_city', 'at_fault', 'accident_city_state', 'injuries', 'fatalities',
        'driveable', 'towed', 'totaled', 'police_call', 'accident_type', 'sf91_date',
        'sf94_date', 'bill_legal', 'date_closed', 'estimates_received_3',
        'estimate_date_1', 'estimate_date_2', 'estimate_date_3', 'vendor_name',
        'vendor_address', 'vendor_address_2', 'vendor_address_3', 'vendor_city',
        'vendor_state', 'vendor_zip', 'vendor_phone_number', 'estimate_cost',
        'repair_date', 'remarks', 'first_tow_company', 'second_tow_company',
        'vendor_total', 'cng_inspection_required', 'days_to_repair',
        'pov_fault_grand_total_billed', 'gov_fault_grand_total_billed',
        'total_loss_yes_or_no', 'purchase_order_number', 
        'purchase_order_approval_date', 'date_3rd_party_closed', 'towing_costs_1',
        'towing_costs_2', 'storage_costs_1', 'storage_costs_2', 'pay_type', 'x76')

crash <-
  vroom("./rawdata/CRASH_Report_Agency_069_Bureau_05.CSV", 
        col_names = cn,
        skip = 6, 
        delim = ",") |> 
  mutate(across(contains("date"), ~ as.Date(.x, format = "%m/%d/%Y")),
         across(contains("cost"), ~ as.numeric(.x)),
         across(c(injuries, fatalities), ~ as.numeric(.x)),
         across(c(vendor_total,
                  pov_fault_grand_total_billed,
                  gov_fault_grand_total_billed), ~ as.numeric(.x)),
         police_report = as.Date(police_report, format = "%m/%d/%Y"),
         total_billed = pov_fault_grand_total_billed +
                        gov_fault_grand_total_billed,
         incident_accident = factor(incident_accident, 
                                    levels = c("I", "A"),
                                    labels = c("incident", "accident")),
         at_fault = factor(at_fault,
                           levels = c("G", "P"),
                           labels = c("government", "private")),
         quarter = lubridate::quarter(accident_date, 
                                      with_year = TRUE, 
                                      fiscal_start = 10)) |> 
  select(-starts_with("x"))

glimpse(crash)

# vroom::vroom_write(crash, "./data/crashdata.csv", delim = ",", 
#                    append = FALSE, bom = TRUE)

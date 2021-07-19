
make_chord <- function(.df, ncol = 11, pal = "Spectral", ...) {
  group_cols <- RColorBrewer::brewer.pal(n = ncol, name = pal)
  .df |>
    dplyr::select(vehicle_class, fuel_class) |> 
    dplyr::mutate(across(.cols = everything(), ~ as.factor(.x))) |> 
    with(table(vehicle_class, fuel_class,
               dnn = list("Vehicle Type", "Fuel Class"))) |>
    chorddiag::chorddiag(type = "bipartite", groupColors = group_cols,
                         showTicks = FALSE, ...)
}

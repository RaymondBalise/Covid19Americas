library(rgdal)

mexico_states <- readOGR("./geo/mexstates", "mexstates")

saveRDS(mexico_states, "./geo/mexico_states.rds")

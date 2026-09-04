# ══════════════
# ONS Census API
# ══════════════
library(tidyverse)
library(httr)
library(jsonlite)

res <- GET("https://api.beta.ons.gov.uk/v1/population-types")
data <- fromJSON(rawToChar(res$content))
data$items %>% data.frame()


# POPN TYPE
res <- GET("https://api.beta.ons.gov.uk/v1/population-types")
data <- fromJSON(rawToChar(res$content))
data$items %>% data.frame()
# name == "atc-rm-pk2-ur-ct-oa" All usual residents 
# name == "atc-rm-pk3-hh-ct-oa" All households 
# name == "atc-rm-pk2-urhh-ct-msoa" All usual residents in households

# AREA
res <- GET("https://api.beta.ons.gov.uk/v1/population-types/atc-rm-pk2-ur-ct-oa/area-types")
data <- fromJSON(rawToChar(res$content))
data$items %>% data.frame()
# id == "msoa"|"ltla"|"icb"|"nhser"

res <- GET("https://api.beta.ons.gov.uk/v1/population-types/atc-rm-pk2-ur-ct-oa/dimensions")
data <- fromJSON(rawToChar(res$content))
data$items %>% data.frame()

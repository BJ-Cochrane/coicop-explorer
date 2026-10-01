# Data and constants. Files in R/ are sourced in alphabetical order, so this goes first.

DATA     <- readRDS("data/coicop.rds")
CODES    <- DATA$codes
CORR     <- DATA$corr
WEIGHTS  <- DATA$weights
INFL     <- DATA$infl
INFL_ALL <- DATA$infl_all
MONTHLY  <- DATA$monthly
GEOS     <- DATA$geos
VERSIONS <- DATA$versions
META     <- DATA$meta

VERSIONS$color <- c(coicop2018 = "#2a78d6", coicop1999 = "#eb6834", ecoicop1 = "#1baf7a")[VERSIONS$id]
VERSION_CHOICES <- setNames(VERSIONS$id, VERSIONS$name)
LEVEL_PLURAL <- c("Divisions", "Groups", "Classes", "Subclasses", "Food details")

LATEST_W <- tapply(WEIGHTS$year, WEIGHTS$version, max)
LATEST_I <- tapply(INFL$year, INFL$version, max)

geo_label <- function(g) GEOS$label[match(g, GEOS$geo)]
GEO_CHOICES <- list(
  "Aggregates" = setNames(GEOS$geo[GEOS$aggregate], GEOS$label[GEOS$aggregate]),
  "Countries"  = setNames(GEOS$geo[!GEOS$aggregate], GEOS$label[!GEOS$aggregate])
)

EXAMPLES <- c("bicycle", "smartphone", "rent", "insurance", "hairdressing", "coffee",
              "pet food", "streaming", "electricity", "laptop")

library(community)
library(jsonlite)

datasets <- paste0(list.dirs("data", recursive = FALSE), "/distribution")
data_reformat_sdad(list.files(datasets, "\\.csv\\.xz$", full.names = TRUE), "docs/data")
info <- lapply(list.files(datasets, "measure_info.*\\.json", full.names = TRUE), read_json)
agg_info <- list()
for (m in info) for (e in names(m)) {
  agg_info[[e]] <- if (e %in% names(agg_info)) c(agg_info[[e]], m[[e]]) else m[[e]]
}
if (length(agg_info)) write_json(
  agg_info, "docs/data/measure_info.json", auto_unbox = TRUE, pretty = TRUE
)

files <- c("block_group", "tract", "county")
files <- structure(paste0(files, ".csv.xz"), names = files)
data_add(
  files[which(file.exists(paste0("docs/data/", files)))[1]],
  meta = list(
    ids = list(variable = "ID", map = "data/entity_info.json"),
    time = "time",
    variables = "docs/data/measure_info.json"
  ),
  dir = "docs/data",
  clean = TRUE,
  refresh = TRUE
)

site_build(".", serve = TRUE, options = list(
  polygon_outline = .5, color_scale_center = "median"
))

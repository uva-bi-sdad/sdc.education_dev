library(parallel)
library(jsonlite)

dir.create("data/original", FALSE)
dir.create("data/original/shapes", FALSE)

# define health districts and focal counties
va_id_map <- jsonlite::read_json("https://uva-bi-sdad.github.io/community/dist/shapes/VA/virginia_2010.json")
county_districts <- c(
  unlist(lapply(va_id_map$county, "[[", "district")),
  "11001" = "11_hd_01", "24017" = "24_hd_01", "24021" = "24_hd_01",
  "24031" = "24_hd_01", "24033" = "24_hd_01"
)

# make a function to process each year
process_year <- function(year, dir = "data/original", districts = county_districts,
                         states = c("DC", "DE", "KY", "MD", "NC", "NJ", "PA", "TN", "VA", "WV"),
                         programs = c(
                           biomedical = "^26",
                           computer = "^11",
                           engineering = "^1[45]",
                           physical = "^40",
                           science = "^41"
                         )) {
  library(catchment)
  library(sf)
  library(osrm)
  message(year, "starting")
  results_file <- paste0("data/working/nces_", year, ".csv.xz")
  block_groups <- if (file.exists(results_file)) {
    message(year, "loading existing file")
    read.csv(gzfile(results_file), check.names = FALSE)
  } else {
    #
    # get and prepare data
    #
    # get population data
    pop_file <- paste0(dir, "/population", year)
    dir.create(pop_file, FALSE)
    message(year, "preparing population data")
    population <- do.call(rbind, lapply(states, function(s) {
      pop <- download_census_population(pop_file, s, year)$estimates
      pop$GEOID <- as.character(pop$GEOID)
      rownames(pop) <- pop$GEOID
      shapes <- download_census_shapes("data/original/shapes", s, "bg", year = year)
      rownames(shapes) <- shapes$GEOID
      IDs <- union(pop$GEOID, shapes$GEOID)
      data.frame(
        GEOID = IDs,
        state = s,
        population_over_14 = rowSums(pop[
          IDs, grep("SEX\\.BY\\.AGE_[FemM]+ale_(?:15|[2-8][05])", colnames(pop))
        ], na.rm = TRUE),
        st_coordinates(st_centroid(shapes[IDs, ]))
      )
    }))
    # download and/or load data
    # https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx
    files <- c(
      institutions = paste0("https://nces.ed.gov/ipeds/datacenter/data/HD", year, ".zip"),
      completion = paste0("https://nces.ed.gov/ipeds/datacenter/data/C", year, "_A.zip")
    )
    message(year, "preparing school data")
    data <- lapply(files, function(f) {
      df <- paste0(dir, "/", basename(f))
      if (!file.exists(df)) download.file(f, df)
      read.csv(unz(df, sub("zip$", "csv", tolower(basename(f)))))
    })
    ## select schools
    data$completion$program <- do.call(paste0, as.data.frame(vapply(
      names(programs), function(code) {
        c("FALSE" = "", "TRUE" = code)[as.character(grepl(programs[[code]], data$completion$CIPCODE))]
      },
      character(nrow(data$completion))
    )))
    data$completion <- data$completion[data$completion$CTOTALT != 0, ]
    school_ids <- data$institutions[
      data$institutions$STABBR %in% states & # is in one of the selected states
        data$institutions$CYACTIVE == 1 & # is active
        data$institutions$ICLEVEL %in% 2:3 & # is a 2- or <2-year school
        data$institutions$UNITID %in% data$completion$UNITID, # has completion information
      "UNITID"
    ]
    data <- lapply(data, function(d) d[d$UNITID %in% school_ids, ])
    data$by_program <- lapply(
      programs, function(p) unique(data$completion[grepl(p, data$completion$CIPCODE), "UNITID"])
    )
    providers <- data$institutions[, c("UNITID", "LONGITUD", "LATITUDE", "ICLEVEL")]
    colnames(providers) <- c("GEOID", "X", "Y", "type")
    providers$GEOID <- as.character(providers$GEOID)
    # calculate travel times
    cost_file <- paste0("data/working/traveltimes_", year, ".csv.xz")
    if (file.exists(cost_file)) {
      message(year, "loading existing travel times")
      traveltimes <- read.csv(gzfile(cost_file), row.names = 1, check.names = FALSE)
    } else {
      message(year, "requesting travel times")
      options(osrm.server = Sys.getenv("OSRM_SERVER"))
      traveltimes <- osrmTable(
        src = population[, c("GEOID", "X", "Y")],
        dst = providers[, 1:3]
      )$duration
      write.csv(
        cbind(GEOID = rownames(traveltimes), as.data.frame(as.matrix(traveltimes))),
        xzfile(cost_file),
        row.names = FALSE
      )
    }
    #
    # calculate outputs
    #
    message(year, "calculating measures")
    # get minimum travel times
    population$schools_2year_min_drivetime <- apply(
      traveltimes[, providers[providers$type == 2, "GEOID"]], 1, min, na.rm = TRUE
    )
    population$schools_under2year_min_drivetime <- apply(
      traveltimes[, providers[providers$type == 3, "GEOID"]], 1, min, na.rm = TRUE
    )
    # calculate catchment ratios
    population$schools_2year_per_100k <- catchment_ratio(
      population, providers[providers$type == 2, ], traveltimes,
      weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14"
    )
    population$schools_under2year_per_100k <- catchment_ratio(
      population, providers[providers$type == 3, ], traveltimes,
      weight = "gaussian", scale = 18,
      normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14"
    )
    for (p in names(data$by_program)) {
      population[[paste0("schools_2year_with_", p, "_program_per_100k")]] <- catchment_ratio(
        population, providers[providers$type == 2 & providers$GEOID %in% data$by_program[[p]], ],
        traveltimes,
        weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
        consumers_value = "population_over_14"
      )
    }
    population$year <- year
    ## save each year for reruns
    write.csv(population, xzfile(results_file), row.names = FALSE)
    population
  }
  block_groups <- block_groups[substring(block_groups$GEOID, 1, 5) %in% names(districts),]
  # aggregate
  agger <- function(d, part = NULL) {
    drivetimes <- grep("drivetime", colnames(d), fixed = TRUE)
    ratios <- grep("_per_", colnames(d), fixed = TRUE)
    total <- sum(d$population_over_14, na.rm = TRUE)
    total[total == 0] <- 1
    res <- as.data.frame(c(
      GEOID = if (missing(part)) districts[[substring(d[1, "GEOID"], 1, 5)]] else substring(d[1, "GEOID"], 1, part),
      as.list(c(
        colMeans(d[, drivetimes], na.rm = TRUE),
        colSums(d[, ratios] * d$population_over_14, na.rm = TRUE) / total
      ))
    ))
    res
  }
  message(year, "creating aggregates")
  list(
    block_groups = block_groups,
    tracts = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 11)), agger, 11)),
    counties = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 5)), agger, 5)),
    districts = do.call(rbind, lapply(split(block_groups, districts[substring(block_groups$GEOID, 1, 5)]), agger))
  )
}

# run for each year
cl <- makeCluster(min(2020 - 2013, detectCores() - 1))
on.exit(stopCluster(cl))
clusterExport(cl, "county_districts")
data <- parLapply(cl, 2013:2020, process_year)
stopCluster(cl)

# download and load maps
dir.create("data/original/reference_shapes", FALSE)
entity_info <- lapply(va_id_map$district, function(e) list(region_name = e$name))
for (location in c("dc", "md", "va")) {
  for (year in c(2020, 2010)) {
    for (level in c("census_block_groups", "census_tracts", "counties")) {
      name <- paste0(location, "_geo_census_cb_", year, "_", level)
      file <- paste0(
        "data/original/reference_shapes/", location, "_",
        sub("census_", "", level, fixed = TRUE), "_", year, ".geojson"
      )
      if (!file.exists(file)) {
        tryCatch(download.file(paste0(
          "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
          name, "/distribution/", name, ".geojson"
        ), file), error = function(e) NULL)
      }
      if (file.exists(file)) {
        d <- read_json(file)
        for (e in d$features) entity_info[[e$properties$geoid]] <- e$properties
      }
    }
  }
}
entity_names <- unlist(lapply(entity_info, "[[", "region_name"))

# reformat and save
final <- do.call(rbind, lapply(data, function(d) {
  year = d$block_groups[1, "year"]
  d$block_groups <- d$block_groups[, colnames(d$districts)]
  d <- do.call(rbind, d)
  varnames <- colnames(d)[-1]
  d$year <- year
  d$region_type <- c(
    "5" = "county", "8" = "health district", "11" = "tract", "12" = "block group"
  )[as.character(nchar(d$GEOID))]
  rownames(d) <- d$GEOID
  d$region_name <- d$GEOID
  present_ids <- d$GEOID[d$GEOID %in% names(entity_names)]
  d[present_ids, "region_name"] <- entity_names[present_ids]
  do.call(rbind, lapply(split(d, seq_len(nrow(d))), function(r) data.frame(
    geoid = r$GEOID,
    region_type = r$region_type,
    region_name = r$region_name,
    year = year,
    measure = varnames,
    value = as.numeric(r[varnames]),
    measure_type = ifelse(grepl("drivetime", varnames, fixed = TRUE), "minutes", "per 100k")
  )))
}))

write.csv(final, xzfile("data/distribution/nces.csv.xz"), row.names = FALSE)

# make special map
write_json(list(
  type = "FeatureCollection",
  crs = list(type = "name", properties = list(name = "urn:ogc:def:crs:OGC:1.3:CRS84")),
  features = unlist(lapply(
    list.files("data/original/reference_shapes", "block_groups_2010", full.names = TRUE),
    function(f) Filter(function(e) e$properties$geoid %in% final$geoid, read_json(f)$features)
  ), FALSE, FALSE)
), "docs/map.geojson", auto_unbox = TRUE)

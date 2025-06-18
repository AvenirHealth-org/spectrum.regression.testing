## setup.meta.X functions prepare data for ALL estimates rounds
## setup.round.X functions prepare data for one estimates round

setup.meta.data = function() {
  return(list(version   = setup.meta.versions(app_sys("metadata/spec-version-table.csv")),
              location  = setup.meta.locations(app_sys("metadata/region-table.csv"), app_sys("metadata/location-table.csv")),
              indicator = setup.meta.indicators(app_sys("metadata/indicator-table.csv"))))
}

setup.meta.versions = function(version.csv) {
  vtab = utils::read.table(version.csv, sep=",", header=TRUE)
  vtab$Prefix = substr(vtab$Commit, 1, 5)
  vtab$Date = gsub("\"", "", vtab$Date) # Dates in version.csv are wrapped in double-quotes to protect them from Excel.
  vtab$ExtractName = with(vtab, sprintf("aim%d/extract-%s-%s.xlsx", Round, Date, Prefix))
  vtab$DateTime = with(vtab, lubridate::ymd_hms(sprintf("%s %s", Date, Time)))
  vtab = vtab[order(vtab$DateTime, decreasing=TRUE),]

  vtab$name = with(vtab, ifelse(is.na(Beta),
                                sprintf("%s [%s] %s (%s)",           Date, Prefix, Version,       Level),
                                sprintf("%s [%s] %s beta %02d (%s)", Date, Prefix, Version, Beta, Level)))
  return(vtab)
}

setup.meta.locations = function(region.csv, locale.csv) {
  regions = utils::read.table(region.csv, sep=",", header=TRUE)
  locales = utils::read.table(locale.csv, sep=",", header=TRUE)
  ltab = dplyr::left_join(locales, regions, by="iso.alpha.3")
  ltab$country.name = plyr::mapvalues(ltab$iso.alpha.3, from=ISOcodes::ISO_3166_1$Alpha_3, to=ISOcodes::ISO_3166_1$Name, warn_missing=FALSE)
  ltab$pathString = with(ltab, ifelse(snu=="",
                                      sprintf("Global/%s/%s - %s",    unaids.region.name, iso.alpha.3, country.name),
                                      sprintf("Global/%s/%s - %s/%s", unaids.region.name, iso.alpha.3, country.name, snu)))
  return(ltab)
}

setup.meta.indicators = function(indicator.csv) {
  indicator = list()
  indicator$data = utils::read.table(indicator.csv, sep=",", header=TRUE)
  indicator$vals = factor(unique(indicator$data$Indicator), levels=var.levels$ind)
  return(indicator)
}


setup.round.data = function(global.data, estimates.round) {
  return(list(version   = setup.round.versions(  global.data, estimates.round),
              location  = setup.round.locations( global.data, estimates.round),
              indicator = setup.round.indicators(global.data, estimates.round)))
}

setup.round.versions = function(global.data, estimates.round) {
  vtab = subset(global.data$version, Round==estimates.round)
  return(vtab)
}

setup.round.locations = function(global.data, estimates.round) {
  ltab = subset(global.data$location, round==estimates.round)
  return(list(data = ltab, tree = data.tree::as.Node(ltab)))
}

setup.round.indicators = function(global.data, estimates.round) {
  return(global.data$indicator) # placeholder function; no differences between 2020 and 2021
}

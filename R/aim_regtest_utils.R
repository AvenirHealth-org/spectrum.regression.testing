## Rob Glaubius
## Avenir Health
## 2021-02-08
## aim-regtest-utils : helper functions used in aim-regtest

## Helper function to convert a data.tree to a list-of-lists for shinyTree
convert.tree = function(node) {
  if (node$isLeaf) {
    rval = ""
  } else {
    rval = lapply(node$children, convert.tree)
  }
  return(rval)
}

lookup.version = function(version.name, version.data) {
  hash = version.data$Commit[version.data$name == version.name]
  if (length(hash) != 1) {
    warning("Version lookup failed")
  }
  return(hash)
}

selected.pjnzs = function(place.tree) {
  ## Our location hierarchy has four levels: global, regional, national,
  ## subnational. Extract archives have national or subnational data. This means
  ## we only need to check which the third and fourth levels of the location
  ## selection tree to determine which locations we need to pull from Extract.
  process.node = function(node) {
    path = c(attr(node, "ancestry"), node)
    if (length(path) < 3) {
      rval = NULL
    } else {
      alpha  = substr(path[3], 1, 3) # strip out all but the ISO Alpha3 code
      pjnz   = ifelse(length(path) == 3, alpha, sprintf("%s - %s", alpha, path[4]))
      rval   = pjnz
    }
    return(rval)
  }

  select.list = sapply(shinyTree::get_selected(place.tree), process.node)
  select.flat = unlist(select.list)
  return(select.flat)
}

process.extract = function(commit.hash, version.data, location.data, indicator.data) {
  time.start = Sys.time()

  vdat = version.data[version.data$Commit == commit.hash,]

  if (nrow(vdat) == 1) {
    tabs = indicator.data$ExtractTab[indicator.data$Config <= vdat$Config]
    extract.long = read.extract(file.path(EXTRACTS_DIR, vdat$ExtractName), tabs)
    extract.long = annotate.extract(extract.long, version.data, location.data, indicator.data)
    extract.long = standardize.age.groups(extract.long)
  } else {
    extract.long = NULL
    warning(sprintf("Expected one Excel file for hash=%s, found %d", commit.hash, nrow(vdat)))
  }

  time.final = Sys.time()
  cat(sprintf("process.extract:\t%0.2f seconds\n", difftime(time.final, time.start, units="sec")))
  return(extract.long)
}

read.extract.tab = function(xlsx.name, tab.name) {
  dat.wide = data.table(readxl::read_excel(xlsx.name, sheet=tab.name, skip=1))

  ## Clean up column names. This renames "Configuration" in Excel to "Sex".
  cnames = colnames(dat.wide)
  year.str = gsub("X", "", cnames[8:length(cnames)])
  cnames[8:length(cnames)] = year.str
  cnames[1:7] = c("PJNZ", "Country", "Alpha", "SNU", "Module", "Indicator", "Sex")
  colnames(dat.wide) = cnames

  ## melt will throw warnings when processing births and deaths tabs. This
  ## happens because Spectrum does not estimate those in the first year of
  ## projection, so we only have NA values in 1970. melt warns that NA is not
  ## the same type as values in later years (double), so it must coerce
  ## everything to double. This is OK.
  suppressWarnings({
    dat.long = melt.data.table(dat.wide,
                               measure.vars=year.str,
                               value.name="Value",
                               variable.name="Year",
                               variable.factor=FALSE,
                               na.rm=FALSE)
  })

  ## Convert years from strings to numbers
  dat.long$Year = as.numeric(dat.long$Year)

  return(dat.long)
}

read.extract = function(xlsx.name, tabs) {
  time.start = Sys.time()

  ## Drop PJNZ, Module, and Country columns
  extract.list = lapply(tabs, function(tab.name) {
    rval = read.extract.tab(xlsx.name, tab.name)
    return(rval[,c("PJNZ", "Module", "Country") := NULL])
  })

  ## Extract puts date information in the configuration column along with sex
  ## for some treatment-related indicators. We strip this out so the column
  ## pertains to sex only. We convert columns to factors where possible to save
  ## memory.
  extract.long = dplyr::bind_rows(extract.list)
  extract.long$Sex = gsub(" - \\(developer\\)", "", gsub(" - \\(mid-year\\)", "", gsub(" - \\(Dec 31\\)", "", extract.long$Sex)))
  extract.long = dplyr::mutate(extract.long,
                        Alpha     = factor(Alpha),
                        SNU       = factor(SNU),
                        Indicator = factor(Indicator),
                        Sex       = factor(Sex, levels=c("Male+Female", "Male", "Female"), labels=var.levels$sex))

  time.final = Sys.time()
  cat(sprintf("read.extract:\t\t%0.2f seconds\n", difftime(time.final, time.start, units="sec")))
  return(extract.long)
}

annotate.extract = function(extract.long, version.data, location.data, indicator.data) {
  time.start = Sys.time()

  extract.long$Region = factor(plyr::mapvalues(extract.long$Alpha,
                                               from = location.data$iso.alpha.3,
                                               to   = location.data$unaids.region.code,
                                               warn_missing = FALSE))

  ## Split Extract indicator names, which combine indicator and age group, into
  ## separate age and indicator variables. We allow the 15+ age group here as a
  ## special case. Spectrum does not report ART for ages 50+, so we need to
  ## derive that group as the difference between 15+ and 15-49.
  extract.long$Age = factor(plyr::mapvalues(extract.long$Indicator,
                                            from = indicator.data$ExtractName,
                                            to   = indicator.data$Age,
                                            warn_missing = FALSE),
                            levels=c(var.levels$age, "15+"))
  extract.long$Indicator = factor(plyr::mapvalues(extract.long$Indicator,
                                                  from = indicator.data$ExtractName,
                                                  to   = indicator.data$Indicator,
                                                  warn_missing = FALSE),
                                  levels=var.levels$ind)
  ## Add a "PJNZ" unique identifier, then drop the SNU column
  extract.long = dplyr::mutate(extract.long, PJNZ = ifelse(is.na(SNU), sprintf("%s", Alpha), sprintf("%s - %s", Alpha, SNU)))
  extract.long$SNU = NULL

  time.final = Sys.time()
  cat(sprintf("annotate.extract:\t%0.2f seconds\n", difftime(time.final, time.start, units="sec")))

  return(extract.long)
}

## standardize.age.groups reformats mid-year ART numbers from Extract from its
## age groups (0-14, 15-49, 15+) to the regression testing tool age groups
## (0-14, 15-49, 50+, all ages).
standardize.age.groups = function(extract.long) {
  time.start = Sys.time()

  if ("ART population" %in% extract.long$Indicator) {
    art.child = extract.long[Indicator == "ART population" & Age ==  "0-14",]
    art.15t49 = extract.long[Indicator == "ART population" & Age == "15-49",]
    art.adult = extract.long[Indicator == "ART population" & Age == "15+",  ]

    ## ART across all age groups
    art.all.wide = dplyr::left_join(art.child, art.adult, by=c("Alpha", "Indicator", "Sex", "Year", "Region", "PJNZ"))
    art.all.wide = dplyr::mutate(art.all.wide, Age="All", Value=Value.x + Value.y)
    art.all.wide = art.all.wide[,c("Value.x", "Value.y", "Age.x", "Age.y") := NULL]

    ## ART for age 50+
    art.50p.wide = dplyr::left_join(art.adult, art.15t49, by=c("Alpha", "Indicator", "Sex", "Year", "Region", "PJNZ"))
    art.50p.wide = dplyr::mutate(art.50p.wide, Age="50+", Value=Value.x - Value.y)
    art.50p.wide = art.50p.wide[,c("Value.x", "Value.y", "Age.x", "Age.y") := NULL]

    ## Drop unused rows for age 15+ outcomes, then bind in rows for all ages and ages 50+
    extract.long = rbind(extract.long[Age != "15+",],
                         art.all.wide,
                         art.50p.wide)
  }

  time.final = Sys.time()
  cat(sprintf("standardize.age.groups:\t%0.2f seconds\n", difftime(time.final, time.start, units="sec")))

  return(extract.long)
}

## Return the indicators that are reported for every version in result.frame.
shared.indicators = function(result.frame) {
  unique.ver = unique(result.frame$Version)
  unique.ind = lapply(unique.ver, function(ver) {unique(result.frame$Indicator[result.frame$Version == ver])})
  return(Reduce(intersect, unique.ind))
}

inputs.art.coverage = function(dat, sex, age) {
  data.denom = dat[Sex==sex & Age==age & Indicator=="HIV population",]
  data.numer = dat[Sex==sex & Age==age & Indicator=="ART population",]
  return(rbind(data.denom, data.numer))
}

inputs.pmtct.coverage = function(dat, sex, age) {
  data.denom = dat[Sex==sex & Age==age & Indicator=="PMTCT need",]
  data.numer = dat[Sex==sex & Age==age & Indicator=="PMTCT population",]
  return(rbind(data.denom, data.numer))
}

inputs.mtct.rate = function(dat) {
  data.denom = dat[Sex=="Both"   & Age=="0-14"  & Indicator=="New HIV infections",]
  data.numer = dat[Sex=="Female" & Age=="15-49" & Indicator=="PMTCT need",]
  data.sset = rbind(data.denom, data.numer)
}

calc.art.coverage = function(dat) {
  by.vars = setdiff(colnames(dat), c("Indicator", "Value"))
  indicators = shared.indicators(dat)

  ## We only calculate ART coverage if its ingredients are available for every
  ## Spectrum version in dat
  if ("ART population" %in% indicators & "HIV population" %in% indicators) {
    art.numer = dat[Indicator == "ART population",]
    art.denom = dat[Indicator == "HIV population",]
    art.cvg.wide = dplyr::left_join(art.numer, art.denom, by=by.vars)
    art.cvg.wide = dplyr::mutate(art.cvg.wide, Indicator="ART coverage", Value=ifelse(Value.y > 0, 100.0 * Value.x / Value.y, 0))
    art.cvg.wide = art.cvg.wide[,c("Value.x", "Value.y", "Indicator.x", "Indicator.y") := NULL]
    if (nrow(art.numer) != nrow(art.denom)) {warning("ART coverage operands may be misaligned. Expect incorrect results.")}
  } else {
    art.cvg.wide = NULL
  }
  return(art.cvg.wide)
}

calc.pmtct.coverage = function(dat) {
  by.vars = setdiff(colnames(dat), c("Indicator", "Value"))
  indicators = shared.indicators(dat)

  if (("PMTCT population" %in% indicators) && ("PMTCT need" %in% indicators)) {
    pmtct.numer = dat[Indicator == "PMTCT population",]
    pmtct.denom = dat[Indicator == "PMTCT need",]
    pmtct.cvg.wide = dplyr::left_join(pmtct.numer, pmtct.denom, by=by.vars)
    pmtct.cvg.wide = dplyr::mutate(pmtct.cvg.wide, Indicator="PMTCT coverage", Sex="Female", Age="15-49", Value=ifelse(Value.y > 0, 100.0 * Value.x / Value.y, 0))
    pmtct.cvg.wide = pmtct.cvg.wide[,c("Value.x", "Value.y", "Indicator.x", "Indicator.y") := NULL]
    if (nrow(pmtct.numer) != nrow(pmtct.denom)) {warning("PMTCT coverage operands may be misaligned. Expect incorrect results.")}
  } else {
    pmtct.cvg.wide = NULL
  }
  return(pmtct.cvg.wide)
}

calc.mtct.rate = function(dat) {
  by.vars = setdiff(colnames(dat), c("Indicator", "Value"))
  indicators = shared.indicators(dat)

  if (("PMTCT need" %in% indicators) && ("New HIV infections" %in% indicators)) {
    ## Final mother-to-child transmission rate
    mtct.rate.numer = dat[Indicator == "New HIV infections" & Age == "0-14" & Sex == "Both",]
    mtct.rate.denom = dat[Indicator == "PMTCT need" & Age == "15-49" & Sex == "Female",]
    mtct.rate.wide = dplyr::left_join(mtct.rate.numer, mtct.rate.denom, by=setdiff(by.vars, c("Sex", "Age")))
    mtct.rate.wide = dplyr::mutate(mtct.rate.wide, Indicator="MTCT rate", Sex="Female", Age="15-49", Value=ifelse(Value.y > 0, 100.0 * Value.x / Value.y, 0))
    mtct.rate.wide = mtct.rate.wide[,c("Value.x", "Value.y", "Sex.x", "Sex.y", "Age.x", "Age.y", "Indicator.x", "Indicator.y") := NULL]
    if (nrow(mtct.rate.numer) != nrow(mtct.rate.denom)) {warning("MTCT rate operands may be misaligned. Expect incorrect results.")}
  } else {
    mtct.rate.wide = NULL
  }
  return(mtct.rate.wide)
}

generate.frame = function(data.long,
                          area       = "Regional",
                          indicators = var.levels$ind,
                          sex        = "Both",
                          age        = "All",
                          years      = c(1970, 2030),
                          pjnzs      = NULL) {
  time.start = Sys.time()

  ## Filter on requested locations and years
  data.base = data.long[Year>=years[1] & Year<=years[2] & PJNZ %in% pjnzs,]
  data.sset = data.base[Indicator %in% indicators & Sex==sex & Age==age,]

  ## The requested indicators may not include ingredients we need to calculate
  ## derived indicators. We merge those ingredients in here when required.
  if ("ART coverage" %in% indicators)   data.sset = union(data.sset, inputs.art.coverage(data.base, sex, age))
  if ("PMTCT coverage" %in% indicators) data.sset = union(data.sset, inputs.pmtct.coverage(data.base, sex, age))
  if ("MTCT rate" %in% indicators)      data.sset = union(data.sset, inputs.mtct.rate(data.base))

  if (area == "Regional") {
    vars = c("Version", "Indicator", "Region", "Sex", "Age", "Year")
  } else if (area == "National") {
    vars = c("Version", "Indicator", "Alpha",  "Sex", "Age", "Year")
  } else if (area == "PJNZ") {
    vars = c("Version", "Indicator", "PJNZ",   "Sex", "Age", "Year")
  } else { # (area == Global)
    vars = c("Version", "Indicator", "Sex", "Age", "Year")
  }

  ## Aggregate to the desired level
  ## We use na.rm=TRUE because some projections may start later or end earlier
  ## than others. We need to remove these missing values to get useful output.
  data.vals = plyr::ddply(data.sset, .variables=vars, function(df) {
    data.frame(Value=sum(df$Value, na.rm=TRUE))
  })
  data.vals = as.data.table(data.vals)

  ## Calculate non-additive indicators (intervention coverage, MTCT rates)
  if ("ART coverage" %in% indicators)   data.vals = rbind(data.vals, calc.art.coverage(data.vals))
  if ("PMTCT coverage" %in% indicators) data.vals = rbind(data.vals, calc.pmtct.coverage(data.vals))
  if ("MTCT rate" %in% indicators)      data.vals = rbind(data.vals, calc.mtct.rate(data.vals))

  ## Filter on requested indicator, sex, and age so that we don't report any
  ## ingredients for complex indicators if those ingredients weren't requested.
  data.vals = data.vals[(Indicator %in% indicators) & Sex==sex & Age==age,]

  time.final = Sys.time()
  cat(sprintf("generate.frame:\t\t%0.2f seconds\n", difftime(time.final, time.start, units="sec")))

  return(data.vals)
}

generate.table = function(data.vals) {
  version = unique(data.vals$Version)
  data.wide = stats::reshape(data.vals,
                             timevar = "Version",
                             idvar = setdiff(colnames(data.vals), c("Version", "Value")),
                             direction="wide")
  colnames(data.wide) = gsub("Value.", "", colnames(data.wide))
  data.wide$Difference = data.wide[[version[1]]] - data.wide[[version[2]]]
  return(data.wide)
}

## Helper function to determine how facets of ggplot object p should be
## arranged. This returns list (p,r), where p is the ggplot object and r is the
## number of rows of facets in the plot.
crystalize.plot = function(p, dat) {
  ## Return immediately if no data are selected
  if (nrow(dat) == 0) {return(p)}

  ## Check whether geographic stratification variables are present
  geo = intersect(c("Region", "Alpha", "PJNZ"), colnames(dat))
  global = (length(geo) == 0) # TRUE if global plot, FALSE if regional/national/PJNZ
  n.geo.strata = ifelse(global, 1, length(unique(dat[[geo]])))
  n.ind.strata = length(unique(dat$Indicator))

  if (global) {
    geo.name = "Global"
  } else {
    geo.name = dat[[geo]][1]
  }

  r = 1
  if (n.geo.strata == 1 & n.ind.strata == 1) {
    ## one place, one indicators
    plot.title = sprintf("%s - %s", dat$Indicator[1], geo.name)
    p = p + ggtitle(plot.title)
    r = 1
  } else if (n.geo.strata == 1 & n.ind.strata > 1) {
    ## one place, many indicators
    plot.title = geo.name
    p = p + ggtitle(plot.title) + facet_wrap(~Indicator, scales="free_y")
    r = wrap_dims(n.ind.strata)[1]
  } else if (n.geo.strata >  1 & n.ind.strata == 1) {
    ## many places, one indicator
    plot.title = dat$Indicator[1]
    p = p + ggtitle(plot.title) + facet_wrap(~dat[[geo]])
    r = wrap_dims(n.geo.strata)[1]
  } else if (n.geo.strata > 1 & n.ind.strata > 1) {
    ## many places, many indicators
    p = p + facet_grid(Indicator~dat[[geo]], scales="free_y")
    r = n.ind.strata
  }

  p = p +
    theme_bw() +
    theme(legend.position = "top",
          text = element_text(size = 16))

  return(list(p=p, r=r))
}

generate.trend = function(data.vals) {
  ## Check if Regional, National, or PJNZ disaggregation was selected
  location = intersect(c("Region", "Alpha", "PJNZ"), colnames(data.vals))

  p = ggplot(data.vals, aes(x=Year, y=Value, group=Version)) +
    geom_line(aes(color=Version))
  return(crystalize.plot(p, data.vals))
}

generate.diffs = function(data.vals) {
  ## Check if Regional, National, or PJNZ disaggregation was selected
  location = intersect(c("Region", "Alpha", "PJNZ"), colnames(data.vals))

  data.wide = generate.table(data.vals)

  p = ggplot(data.wide, aes(x=Year, y=Difference)) +
    geom_line(color="#0066ff")
  return(crystalize.plot(p, data.wide))
}


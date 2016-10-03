library(quantmod)
library(foreign)
library(choroplethr)
library(choroplethrMaps)
library(UCR.ColumnNames)
library(reshape2)
data(county.regions)
data("df_pop_county")
setwd("C:/Users/user/Dropbox/R_project/touch_DNA/raw_data/clearance_by_arrest")
clearance_2009 <- read.dta("2009_clearance_by_arrest.dta")
clearance_2009 <- clearance_2009[clearance_2009$V2 == "Pennsylvania",]
clearance_2014 <- read.dta("2014_clearance_by_arrest.dta")
clearance_2014 <- clearance_2014[clearance_2014$V2 == "Pennsylvania",]

data(county.regions)
data("df_pop_county")
county.regions <- county.regions[county.regions$state.name == "pennsylvania",]

test <- county.regions$county.name
test
clearance_2009$V15[grep(paste(test, collapse = "|"), clearance_2009$V26,
                        ignore.case = TRUE)]

clearance_2009 <- agg_counties(clearance_2009)
clearance_2014 <- agg_counties(clearance_2014)
clearance <- rbind(clearance_2009, clearance_2014)
clearance_percent <- percent_change(clearance)



agg_counties <- function(dataset){
  # Fixes column names
  dataset <- UCR.OffenseNames(dataset)
  dataset$CASEID <- NULL
  dataset <- dataset[, c(2, 6, 15, 44:ncol(dataset))]


  offense_names <- names(dataset)[30:81]
  offense_names <- gsub("jan_", "", offense_names)

  total_columns <- ncol(dataset)
  for (n in 1:length(offense_names)) {
    dataset[, total_columns + n] <-
      rowSums(dataset[, c(grep(offense_names[n], names(dataset))),
                      drop = FALSE])
    names(dataset)[total_columns + n] <- paste("yearly",
                                               offense_names[n],
                                               sep = "_")
  }
  dataset <- dataset[, c(1:12, 1409:ncol(dataset))]
  dataset <- dataset[-grep("unfo|unfn", names(dataset))]

  dataset <- aggregate(. ~ numeric_state_code + year +county_1,
                       FUN = sum, data = dataset)

  dataset <- melt(dataset, id = c("year", "numeric_state_code", "county_1"))
  names(dataset)[4] <- "offense_name"

  dataset$short_name <- gsub("yearly_act_num_|yearly__tot_clr_|yearly_tot_clr_",
                             "", dataset$offense_name)
  dataset$short_name <- gsub(
    "yearly_ot_clr_vhc_thft__tot|yearly_act__vhc_theft__tot",
    "motor vehicle theft", dataset$short_name)

  dataset$short_name <- gsub("robbry__totl", "totl_robery", dataset$short_name)
  dataset$short_name <- gsub("__tot.*", "", dataset$short_name)
  dataset$short_name <- gsub("_", " ", dataset$short_name)
  dataset$short_name <- gsub("yearly act  ", " ", dataset$short_name)
  dataset$short_name <- gsub("yearly.", " ", dataset$short_name)
  dataset$short_name <- gsub("^ (.*)", "\\1", dataset$short_name)
  dataset$short_name <- gsub("manslghtr", "manslaughter", dataset$short_name)
  dataset$short_name <- gsub("atmptd rape",
                             "attempted rape", dataset$short_name)
  dataset$short_name <- gsub("robbry|rob$",
                             "robbery", dataset$short_name)
  dataset$short_name <- gsub("str arm rob",
                             "Strong Arm Robbery", dataset$short_name)
  dataset$short_name <- gsub("atmptd |^att ",
                             "attempted", dataset$short_name)
  dataset$short_name <- gsub("vhc",
                             "vehicle", dataset$short_name)
  dataset$short_name <- gsub("oth",
                             "other", dataset$short_name)
  dataset$short_name <- gsub("wpn",
                             "weapon", dataset$short_name)
  dataset$short_name <- gsub("aslt",
                             "assault", dataset$short_name)
  dataset$short_name <- gsub("^forc ",
                             "forcible", dataset$short_name)
  dataset$short_name <- gsub("brglry",
                             "burglary", dataset$short_name)
  dataset$short_name <- gsub("larcny",
                             "larceny", dataset$short_name)
  dataset$short_name <- gsub("oth vhc thft",
                             "Other vehicle theft", dataset$short_name)
  dataset$short_name <- gsub("totl",
                             "total", dataset$short_name)
  dataset$short_name <- gsub("entrno frc|entryno forc",
                             "Non-force entry", dataset$short_name)
  dataset$short_name <- gsub("oth clr vhc thft|oth clr vehicle thft",
                             "Other vehicle theft", dataset$short_name)
  dataset$short_name <- gsub("truckbus thft",
                             "Theft from truck or bus", dataset$short_name)
  dataset$short_name <- gsub("forciblerape",
                             "forcible rape", dataset$short_name)
  dataset$short_name <- gsub("trckbs thft|trckbus thft",
                             "Theft from truck or bus", dataset$short_name)
  dataset$short_name <- gsub("clr ", "", dataset$short_name)
  dataset$short_name <- gsub("^ot ", "other", dataset$short_name)
  dataset$short_name <- gsub("asslt", "assault", dataset$short_name)
  dataset$short_name <- gsub("attemptedburglary", "attempted burglary",
                             dataset$short_name)
  dataset$short_name <- gsub("forcibleentry", "forcible entry",
                             dataset$short_name)
  dataset$short_name <- gsub("othervehicle thft", "other vehicle theft",
                             dataset$short_name)
  dataset$short_name <- gsub("robery", "robbery",
                             dataset$short_name)
  dataset$short_name <- gsub("robberybery", "robbery",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("hndfeet_aslt|hndfeet assault",
                             "hndft_aslt", dataset$short_name)
  dataset$short_name <- gsub("knife robry", "knife robbery",
                             dataset$short_name)
  dataset$short_name <- gsub("Other Vehicle Thft",
                             "other vehicle theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("force ",
                             "forcible ",
                             dataset$short_name, ignore.case = TRUE)

  dataset$short_name <- gsub("Attemptedburglry",
                             "attempted burglary",
                             dataset$short_name, ignore.case = TRUE)

  dataset$short_name <- gsub("hndft_aslt",
                             "hndft assault",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("vehicle theft",
                             "motor vehicle theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^rape$",
                             "total rape",
                             dataset$short_name, ignore.case = TRUE)

  dataset$short_name <- gsub("^assault$",
                             "total assault",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^burglary$",
                             "burglary",
                             dataset$short_name, ignore.case = TRUE)


  dataset$crime_type <- "property"
  dataset$crime_type[grep("murder|rape|robbery|assault|manslaughter",
                          dataset$short_name, ignore.case = TRUE)] <- "violent"
  dataset$crime_type[dataset$short_name == "all fields"] <- "Total"

  for (i in unique(dataset$region)) {
    for (n in unique(dataset$year)) {
      for (m in unique(dataset$Type)) {
        property_row <- data.frame(year = n, region = i,
             offense_name = "property_crimes",
             value = sum(dataset$value[dataset$year == n &
                                         dataset$region == i &
                                         dataset$Type == m &
                                         dataset$crime_type == "property"]),
             short_name = "Property Crime",
             Type = m,
             crime_type = "property")

        property_row_no_larceny <- data.frame(year = n, region = i,
              offense_name = "property_crimes_no_larceny",
              value = sum(dataset$value[dataset$year == n &
                                          dataset$region == i &
                                          dataset$Type == m &
                                          dataset$crime_type == "property" &
                                          dataset$short_name != "larceny"]),
              short_name = "Property Crime Except Larceny",
              Type = m,
              crime_type = "property")


        violent_row <- data.frame(year = n, region = i,
            offense_name = "violent_crimes",
            value = sum(dataset$value[dataset$year == n &
                                        dataset$region == i &
                                        dataset$Type == m &
                                        dataset$crime_type == "violent"]),
            short_name = "Violent Crime",
            Type = m,
            crime_type = "property")
        dataset <- rbind(dataset, property_row,
                         violent_row,
                         property_row_no_larceny)
      }
    }
  }
  return(dataset)
}

percent_change <- function(dataset) {

  dataset$county_1 <- as.character(dataset$county_1)
  dataset$offense_name <- as.character(dataset$offense_name)
  dataset$percent_change <- 0
    for (n in unique(dataset$offense_name)) {
      for (x in unique(dataset$county_1)) {
        dataset$percent_change[dataset$year == 2014 &
                                 dataset$offense_name == n &
                                 dataset$county_1 == x] <-
          Delt(dataset$value[dataset$year == 2009 &
                               dataset$offense_name == n &
                               dataset$county_1 == x],
               dataset$value[dataset$year == 2014 &
                               dataset$offense_name == n &
                               dataset$county_1 == x]) * 100
      }
    }

  dataset$percent_change[dataset$percent_change == "NaN" |
                           dataset$percent_change == "Inf"] <- NA

  dataset <- dataset[dataset$year == 2014,]

}
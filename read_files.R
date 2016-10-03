library(foreign)
library(readstata13)
library(lubridate)
library(readr)
library(plyr)

read_merge_bensalem_arrests <- function(){
  setwd("C:/Users/user/Dropbox/R_project/touch_DNA/raw_data/arrests_age_sex_race")
  path = "C:/Users/user/Dropbox/R_project/touch_DNA/raw_data/arrests_age_sex_race"
  file.names <- dir(path, pattern =".dta")
  file.names <- file.names[grep(".dta", file.names)]
  file.names2 <- gsub("(.*)_arrests.*", "arrests_\\1", file.names)

  file.names_subsplit <- split(file.names, ceiling(seq_along(file.names)/5))
  file.names_subsplit2 <- split(file.names2, ceiling(seq_along(file.names2)/5))

  out.file <- ""
  for (i in 1:length(file.names)){
    file <- read.dta13(file.names_subsplit$'1'[i])
    out.file <- rbind(out.file, file)
    rm(file)
  }

  subset_UCR <- function(dataset, agency = NULL, county = NULL){
    if (!is.null(agency)) {
      dataset <-  dataset[grep(agency, dataset$AGENCY,
                               ignore.case = TRUE),]
    }
    else if (!is.null(county)) {
      dataset <- dataset[grep(county, dataset$COUNTY,
                              ignore.case = TRUE),]
    }
    return(dataset)
  }



arrests <- rbind(arrests_2000, arrests_2001, arrests_2002,
                 arrests_2003, arrests_2004, arrests_2005,
                 arrests_2006, arrests_2007, arrests_2008,
                 arrests_2009, arrests_2010, arrests_2011,
                 arrests_2012, arrests_2013, arrests_2014)



for (i in 1:ncol(arrests)){
  arrests[,i] <- gsub(99999, 0, arrests[,i])
}

arrests$month_year <- paste(arrests$YEAR, arrests$MONTH, "1",
                            sep = "-")
arrests$month_year <- ymd(arrests$month_year)

offense_codes <- read_csv("offense_codes_arrest.csv")
arrests <- merge(arrests, offense_codes, by = "OFFENSE")


for (i in 1:nrow(arrests)) {
  arrests$total_arrests[i] <- sum(arrests[i, 34:77])
}

for (i in 1:nrow(arrests)) {
  arrests$youth_arrests[i] <- sum(arrests[i, c(34:39, 56:61)])
}

arrests$adult_arrests <- arrests$total_arrests - arrests$youth_arrests


return(arrests)
}


read__merge_bensalem_arrests_summarized <- function(){
  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "raw_data/arrests_age_sex_race_summarized",
              sep = ""))
  path = paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "raw_data/arrests_age_sex_race_summarized",
              sep = "")
  file.names <- dir(path, pattern = ".dta")
  file.names <- file.names[grep(".dta", file.names)]
  file.names2 <- gsub("(.*).(arrests)_age.*(summarized).*",
                      "\\3_\\2_\\1", file.names)
  for (i in 1:5) {
    assign(file.names2[i], read.dta13(file.names[i]))
   }


  arrests_2000_summarized <- read.dta("2000_arrests_age_sex_race_summarized.dta")
  arrests_2000_summarized <- arrests_2000_summarized[grep("bensalem",
                                    arrests_2000_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2001_summarized <- read.dta("2001_arrests_age_sex_race_summarized.dta")
  arrests_2001_summarized <- arrests_2001_summarized[grep("bensalem",
                                    arrests_2001_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2002_summarized <- read.dta("2002_arrests_age_sex_race_summarized.dta")
  arrests_2002_summarized <- arrests_2002_summarized[grep("bensalem",
                                    arrests_2002_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2003_summarized <- read.dta("2003_arrests_age_sex_race_summarized.dta")
  arrests_2003_summarized <- arrests_2003_summarized[grep("bensalem",
                                    arrests_2003_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2004_summarized <- read.dta("2004_arrests_age_sex_race_summarized.dta")
  arrests_2004_summarized <- arrests_2004_summarized[grep("bensalem",
                                    arrests_2004_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2005_summarized <- read.dta("2005_arrests_age_sex_race_summarized.dta")
  arrests_2005_summarized <- arrests_2005_summarized[grep("bensalem",
                                    arrests_2005_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2006_summarized <- read.dta("2006_arrests_age_sex_race_summarized.dta")
  arrests_2006_summarized <- arrests_2006_summarized[grep("bensalem",
                                    arrests_2006_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2007_summarized <- read.dta("2007_arrests_age_sex_race_summarized.dta")
  arrests_2007_summarized <- arrests_2007_summarized[grep("bensalem",
                                    arrests_2007_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2008_summarized <- read.dta("2008_arrests_age_sex_race_summarized.dta")
  arrests_2008_summarized <- arrests_2008_summarized[grep("bensalem",
                                    arrests_2008_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2009_summarized <- read.dta("2009_arrests_age_sex_race_summarized.dta")
  arrests_2009_summarized <- arrests_2009_summarized[grep("bensalem",
                                    arrests_2009_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2010_summarized <- read.dta("2010_arrests_age_sex_race_summarized.dta")
  arrests_2010_summarized <- arrests_2010_summarized[grep("bensalem",
                                    arrests_2010_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2011_summarized <- read.dta("2011_arrests_age_sex_race_summarized.dta")
  arrests_2011_summarized <- arrests_2011_summarized[grep("bensalem",
                                    arrests_2011_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2012_summarized <- read.dta13("2012_arrests_age_sex_race_summarized.dta")
  arrests_2012_summarized <- arrests_2012_summarized[grep("bensalem",
                                    arrests_2012_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2013_summarized <- read.dta("2013_arrests_age_sex_race_summarized.dta")
  arrests_2013_summarized <- arrests_2013_summarized[grep("bensalem",
                                    arrests_2013_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_2014_summarized <- read.dta("2014_arrests_age_sex_race_summarized.dta")
  arrests_2014_summarized <- arrests_2014_summarized[grep("bensalem",
                                    arrests_2014_summarized$AGENCY,
                                    ignore.case = TRUE), ]

  arrests_summarized <- rbind(arrests_2000_summarized,
                              arrests_2001_summarized,
                              arrests_2002_summarized,
                              arrests_2003_summarized,
                              arrests_2004_summarized,
                              arrests_2005_summarized,
                              arrests_2006_summarized,
                              arrests_2007_summarized,
                              arrests_2008_summarized,
                              arrests_2009_summarized,
                              arrests_2010_summarized,
                              arrests_2011_summarized,
                              arrests_2012_summarized,
                              arrests_2013_summarized,
                              arrests_2014_summarized)

  offense_codes <- read_csv("arrest_summarized_offense_codes.csv")
  arrests_summarized <- merge(arrests_summarized, offense_codes, by = "OFFENSE")

  for (i in 1:nrow(arrests_summarized)){
  arrests_summarized$total_arrests[i] <- sum(arrests_summarized[i, 23:66])
  }

  for (i in 1:nrow(arrests_summarized)){
    arrests_summarized$youth_arrests[i] <- sum(arrests_summarized[i, c(23:28,
                                                                       45:50)])
  }

  arrests_summarized$adult_arrests <- arrests_summarized$total_arrests -
                                      arrests_summarized$youth_arrests

  return(arrests_summarized)
}



read__merge_bensalem_clearance <- function(){
    setwd("C:/Users/user/Dropbox/R_project/touch_DNA/raw_data/clearance_by_arrest")
    clearance_2000 <- read.dta("2000_clearance_by_arrest.dta")
    clearance_2000 <- clearance_2000[grep("bensalem", clearance_2000$V26,
                                          ignore.case = TRUE), ]

    clearance_2001 <- read.dta("2001_clearance_by_arrest.dta")
    clearance_2001 <- clearance_2001[grep("bensalem", clearance_2001$V26,
                                          ignore.case = TRUE), ]

    clearance_2002 <- read.dta("2002_clearance_by_arrest.dta")
    clearance_2002 <- clearance_2002[grep("bensalem", clearance_2002$V26,
                                          ignore.case = TRUE), ]

    clearance_2003 <- read.dta("2003_clearance_by_arrest.dta")
    clearance_2003 <- clearance_2003[grep("bensalem", clearance_2003$V26,
                                          ignore.case = TRUE), ]

    clearance_2004 <- read.dta("2004_clearance_by_arrest.dta")
    clearance_2004 <- clearance_2004[grep("bensalem", clearance_2004$V26,
                                          ignore.case = TRUE), ]

    clearance_2005 <- read.dta("2005_clearance_by_arrest.dta")
    clearance_2005 <- clearance_2005[grep("bensalem", clearance_2005$V26,
                                          ignore.case = TRUE), ]

    clearance_2006 <- read.dta("2006_clearance_by_arrest.dta")
    clearance_2006 <- clearance_2006[grep("bensalem", clearance_2006$V26,
                                          ignore.case = TRUE), ]

    clearance_2007 <- read.dta("2007_clearance_by_arrest.dta")
    clearance_2007 <- clearance_2007[grep("bensalem", clearance_2007$V26,
                                          ignore.case = TRUE), ]

    clearance_2008 <- read.dta("2008_clearance_by_arrest.dta")
    clearance_2008 <- clearance_2008[grep("bensalem", clearance_2008$V26,
                                          ignore.case = TRUE), ]

    clearance_2009 <- read.dta("2009_clearance_by_arrest.dta")
    clearance_2009 <- clearance_2009[grep("bensalem", clearance_2009$V26,
                                          ignore.case = TRUE), ]

    clearance_2010 <- read.dta("2010_clearance_by_arrest.dta")
    clearance_2010 <- clearance_2010[grep("bensalem", clearance_2010$V26,
                                          ignore.case = TRUE), ]

    clearance_2011 <- read.dta("2011_clearance_by_arrest.dta")
    clearance_2011 <- clearance_2011[grep("bensalem", clearance_2011$V26,
                                          ignore.case = TRUE), ]

    clearance_2012 <- read.dta13("2012_clearance_by_arrest.dta")
    clearance_2012 <- clearance_2012[grep("bensalem", clearance_2012$V26,
                                          ignore.case = TRUE), ]

    clearance_2013 <- read.dta("2013_clearance_by_arrest.dta")
    clearance_2013 <- clearance_2013[grep("bensalem", clearance_2013$V26,
                                          ignore.case = TRUE), ]

    clearance_2014 <- read.dta("2014_clearance_by_arrest.dta")
    clearance_2014 <- clearance_2014[grep("bensalem", clearance_2014$V26,
                                          ignore.case = TRUE), ]

    clearance <- rbind(clearance_2000, clearance_2001, clearance_2002)
    clearance$CASEID <- NULL
    clearance2 <- rbind(clearance_2003, clearance_2004, clearance_2005,
                       clearance_2006, clearance_2007, clearance_2008,
                       clearance_2009, clearance_2010, clearance_2011,
                       clearance_2012, clearance_2013, clearance_2014)
    clearance <- rbind(clearance, clearance2)

    load("codebook_clearance.rda")

    clearance$V1446 <- NULL
    clearance$V1447 <- NULL
    clearance$V1448 <- NULL

    for (i in 1:ncol(clearance)){
      if (names(clearance)[i] == codebook$column_code[i]) {
        names(clearance)[i] <- codebook$column_name[i]
      }
    }


    offense_names <- names(clearance)[70:121]
    offense_names <- gsub("^....", "", offense_names)
    grep(offense_names[48], names(clearance), value = TRUE)



    clearance_columns <- ncol(clearance)
    for (i in 1:nrow(clearance)) {
      for (n in 1:length(offense_names)) {
        clearance[i ,clearance_columns + n] <- sum(clearance[i,
                         c(grep(offense_names[n], names(clearance)))],
                         na.rm = TRUE)
        names(clearance)[clearance_columns + n] <- paste("yearly",
                                                         offense_names[n],
                                                         sep = "_")
      }
    }




    return(clearance)
}


read__merge_bensalem_property <- function(){
  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "raw_data/property_stolen_recovered",
              sep = ""))
  property_2000 <- read.dta("2000_property_stolen_recovered.dta")
  property_2000 <- property_2000[grep("bensalem", property_2000$V8,
                                      ignore.case = TRUE), ]

  property_2001 <- read.dta("2001_property_stolen_recovered.dta")
  property_2001 <- property_2001[grep("bensalem", property_2001$V8,
                                      ignore.case = TRUE), ]

  property_2002 <- read.dta("2002_property_stolen_recovered.dta")
  property_2002 <- property_2002[grep("bensalem", property_2002$V8,
                                      ignore.case = TRUE), ]

  property_2003 <- read.dta("2003_property_stolen_recovered.dta")
  property_2003 <- property_2003[grep("bensalem", property_2003$V8,
                                      ignore.case = TRUE), ]

  property_2004 <- read.dta("2004_property_stolen_recovered.dta")
  property_2004 <- property_2004[grep("bensalem", property_2004$V8,
                                      ignore.case = TRUE), ]

  property_2005 <- read.dta("2005_property_stolen_recovered.dta")
  property_2005 <- property_2005[grep("bensalem", property_2005$V8,
                                      ignore.case = TRUE), ]

  property_2006 <- read.dta("2006_property_stolen_recovered.dta")
  property_2006 <- property_2006[grep("bensalem", property_2006$V8,
                                      ignore.case = TRUE), ]

  property_2007 <- read.dta("2007_property_stolen_recovered.dta")
  property_2007 <- property_2007[grep("bensalem", property_2007$V8,
                                      ignore.case = TRUE), ]

  property_2008 <- read.dta("2008_property_stolen_recovered.dta")
  property_2008 <- property_2008[grep("bensalem", property_2008$V8,
                                      ignore.case = TRUE), ]

  property_2009 <- read.dta13("2009_property_stolen_recovered.dta")
  property_2009 <- property_2009[grep("bensalem", property_2009$V8,
                                      ignore.case = TRUE), ]

  property_2010 <- read.dta("2010_property_stolen_recovered.dta")
  property_2010 <- property_2010[grep("bensalem", property_2010$V8,
                                      ignore.case = TRUE), ]

  property_2011 <- read.dta("2011_property_stolen_recovered.dta")
  property_2011 <- property_2011[grep("bensalem", property_2011$V8,
                                      ignore.case = TRUE), ]

  property_2012 <- read.dta13("2012_property_stolen_recovered.dta")
  property_2012 <- property_2012[grep("bensalem", property_2012$V8,
                                      ignore.case = TRUE), ]

  property_2013 <- read.dta("2013_property_stolen_recovered.dta")
  property_2013 <- property_2013[grep("bensalem", property_2013$V8,
                                      ignore.case = TRUE), ]

  property_2014 <- read.dta("2014_property_stolen_recovered.dta")
  property_2014 <- property_2014[grep("bensalem", property_2014$V8,
                                      ignore.case = TRUE), ]

  property <- rbind(property_2000, property_2001, property_2002)
  property2 <- rbind(property_2003, property_2004, property_2005,
                    property_2006, property_2007, property_2008,
                    property_2009, property_2010, property_2011,
                    property_2012, property_2013, property_2014)
  property <- merge(property, property2, by = "V8")

  return(property)
}
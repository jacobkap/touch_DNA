library(ggplot2)
library(ggthemes)
library(scales)

arrest_summary_plot <- function(graph_type, age_group){
  setwd("C:/Users/user/Dropbox/R_project/touch_DNA/clean_data")
  load("bensalem_arrest_summary.rda")
  dataset <- bensalem_arrest_summary

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  dataset$short_name <-
    sapply(dataset$short_name, simpleCap)

  dataset$offense_name <- gsub("\\(|\\)", "",
                                               dataset$offense_name)

  for (i in unique(dataset$offense_name)) {
    if (graph_type == "line") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "arrest_summarized/line_plot",
                  sep = ""))
    }
    else if (graph_type == "dot") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "arrest_summarized/dot_plot",
                  sep = ""))
    }
    else if (graph_type == "dot_line") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "arrest_summarized/line_dot_plot",
                  sep = ""))
    }
    else {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "arrest_summarized/bar_plot",
                  sep = ""))
    }

    test <- dataset[grep(i,
                                         dataset$offense_name),]
    if (age_group == "Total"){
      test <- test[, c(8, 79:81)]
    }
    else if (age_group == "Adult"){
      test <- test[, c(8, 79:80, 83)]
    }
    else if (age_group == "Youth"){
      test <- test[, c(8, 79:80, 82)]
    }
    names(test)[4] <- "arrests"
    zero_years <- data.frame(YEAR = c(2000:2014), arrests = 0,
                             offense_name = i,
                             short_name = rep(test$short_name[1],
                                                                15))
    test <- rbind(test, zero_years)
    test <- aggregate(arrests ~ YEAR + offense_name + short_name,
                      data = test, FUN = sum)

    if (graph_type == "line"){
      ggplot(data = test, aes(x = YEAR, y = arrests)) +
        geom_line() +
        xlab("Year") +
        ylab("# of Arrests") +
        ggtitle(paste(age_group, "Arrests for:",
                      test$short_name,
                      "\nBensalem, PA (2000-2014)",
                      sep = " ")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + ylab("# of Arrests") + xlab("Year")
    }
    else if (graph_type == "dot") {

      ggplot(data = test, aes(x = YEAR, y = arrests)) +
        geom_point(size = 3) +
        xlab("Year") +
        ylab("# of Arrests") +
        ggtitle(paste(age_group, "Arrests for:",
                      test$short_name,
                      "\nBensalem, PA (2000-2014)",
                      sep = " ")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + ylab("# of Arrests") + xlab("Year")

    }
    else if (graph_type == "dot_line") {

      ggplot(data = test, aes(x = YEAR, y = arrests)) +
        geom_line() +
        geom_point(size = 3) +
        xlab("Year") +
        ylab("# of Arrests") +
        ggtitle(paste(age_group, "Arrests for:",
                      test$short_name,
                      "\nBensalem, PA (2000-2014)",
                      sep = " ")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + ylab("# of Arrests") + xlab("Year")

    }
    else {

      ggplot(data = test, aes(x = YEAR, y = arrests)) +
        geom_bar(stat = "identity") +
        xlab("Year") +
        ylab("# of Arrests") +
        ggtitle(paste(age_group, "Arrests for:",
                      test$short_name,
                      "\nBensalem, PA (2000-2014)",
                      sep = " ")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + ylab("# of Arrests") + xlab("Year")

    }

    test$short_name <- gsub(" ", "_", test$short_name)
    test$short_name <- gsub("_{2}", "_", test$short_name)
    test$short_name <- gsub(",|:|-|\\(|\\)|\\.|\\/", "", test$short_name)

    if (graph_type == "line") {
    ggsave(filename = paste(age_group, "_", test$short_name[1],
                            "_summarized_line.png",
                            sep = ""))
    }
    else if (graph_type == "dot") {
      ggsave(filename = paste(age_group, "_", test$short_name[1],
                              "_summarized_dot.png",
                              sep = ""))
    }
    else if (graph_type == "line") {
      ggsave(filename = paste(age_group, "_", test$short_name[1],
                              "_summarized_line_dot.png",
                              sep = ""))
    }
    else {
        ggsave(filename = paste(age_group, "_", test$short_name[1],
                                "_summarized_bar.png",
                                sep = ""))
      }

    graphics.off()
  }

}

summary_arrest_all <- function(){

  arrest_summary_plot(graph_type = "dot", age_group = "Youth")
  arrest_summary_plot(graph_type = "dot", age_group = "Total")
  arrest_summary_plot(graph_type = "dot", age_group = "Adult")

  arrest_summary_plot(graph_type = "line", age_group = "Youth")
  arrest_summary_plot(graph_type = "line", age_group = "Total")
  arrest_summary_plot(graph_type = "line", age_group = "Adult")

  arrest_summary_plot(graph_type = "dot_line", age_group = "Youth")
  arrest_summary_plot(graph_type = "dot_line", age_group = "Total")
  arrest_summary_plot(graph_type = "dot_line", age_group = "Adult")

  arrest_summary_plot(graph_type = "bar", age_group = "Youth")
  arrest_summary_plot(graph_type = "bar", age_group = "Total")
  arrest_summary_plot(graph_type = "bar", age_group = "Adult")
}

clearance_cleaning <- function(dataset, region = "", agg_county = TRUE) {
  setwd("C:/Users/user/Dropbox/R_project/touch_DNA/clean_data")
  load("bensalem_clearance.rda")

  dataset <- dataset[dataset$yearly_act__all_fields > 99,]
  dataset <- dataset[, grep("yearly|^year$|agency_name",
                            names(dataset))]
 if (agg_county) {
  if (region != "bensalem"){
    dataset$region <- region
    bensalem_clearance <- bensalem_clearance[, grep("yearly|^year$",
                                                    names(bensalem_clearance))]
    bensalem_clearance$region <- "bensalem"

    remove_bensalem <- rbind(dataset, bensalem_clearance)
    for (i in 2:(ncol(dataset) - 1)) {
      for (n in 2000:2014)
        remove_bensalem[,i][remove_bensalem$region == region &
                              remove_bensalem$year == n] <-
          remove_bensalem[,i][remove_bensalem$region == region &
                                remove_bensalem$year == n] -
          remove_bensalem[,i][remove_bensalem$region == "bensalem" &
                                remove_bensalem$year == n]
    }
    dataset <- remove_bensalem[remove_bensalem$region == region,]
    dataset$region <- NULL
  }
 }

  library(reshape)

  if (agg_county) {
  dataset <- melt(dataset, id = "year")
  names(dataset)[2] <- "offense_name"
  dataset$region <- region
  }

  else if (!agg_county) {
    dataset <- melt(dataset, id = c("year", "agency_name"))
    names(dataset)[3] <- "offense_name"
 #   dataset$region <- region
  }

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
                             "burglary1",
                             dataset$short_name, ignore.case = TRUE)

  dataset$Type <- "Total Offenses"
  dataset$Type[grep("clr", dataset$offense_name, ignore.case = TRUE)] <-
    "Cleared Cases"

  return(dataset)
}

clearance_plot <- function(dataset, graph_type, region = ""){
  dataset <- clearance_cleaning(dataset, region = region)

  region_name <- gsub("_", " ", region)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse=" ")
  }

  region_name <- gsub("_", " ", region)
  region_name <- simpleCap(region_name)

  dataset$short_name <-
    sapply(dataset$short_name, simpleCap)

  for (i in unique(dataset$short_name)) {
    if (graph_type == "line") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "clearance/", region, "/line_plot",
                  sep = ""))
    }
    else if (graph_type == "dot") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "clearance/", region, "/dot_plot",
                  sep = ""))
    }
    else if (graph_type == "dot_line") {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "clearance/", region, "/line_dot_plot",
                  sep = ""))
    }
    else {
      setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
                  "clearance/", region, "/bar_plot",
                  sep = ""))
    }

    test <- dataset[grep(i,
                         dataset$short_name),]

    test$short_name[test$short_name == "Burglary1"] <- "Burglary"

    if (graph_type == "line"){
      ggplot(data = test, aes(x = year, y = value, col = Type)) +
        geom_line(size = 1.2) +
        ggtitle(paste(test$short_name,
                      "\n", region_name, " (2000-2014)",
                      sep = "")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.title = element_blank()) +
        ylab("# of Incidents") + xlab("Year") +
        scale_color_fivethirtyeight("cyl")
    }
    else if (graph_type == "dot") {

      ggplot(data = test, aes(x = year, y = value, col = Type)) +
        geom_point(size = 3) +
        ggtitle(paste(test$short_name,
                      "\n", region_name, " (2000-2014)",
                      sep = "")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.title = element_blank()) +
        ylab(paste("# of Incidents")) + xlab("Year") +
        scale_color_fivethirtyeight("cyl")

    }
    else if (graph_type == "dot_line") {

      ggplot(data = test, aes(x = year, y = value, col = Type)) +
        geom_line() +
        geom_point(size = 3) +
        ggtitle(paste(test$short_name,
                      "\n", region_name, " (2000-2014)",
                      sep = "")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.title = element_blank()) +
        ylab(paste("# of Incidents")) + xlab("Year") +
        scale_color_fivethirtyeight("cyl")

    }
    else {

      ggplot(data = test, aes(x = year, y = value, fill = Type)) +
        geom_bar(stat = "identity", position="dodge") +
        ggtitle(paste(test$short_name,
                      "\n", region_name, " (2000-2014)",
                      sep = "")) +
        scale_x_discrete(
          limits = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(), legend.title = element_blank()) +
        ylab(paste("# of Incidents")) + xlab("Year") +
        scale_fill_fivethirtyeight("cyl")

    }

    if (graph_type == "line") {
      ggsave(filename = paste(region, "_", test$short_name[1],
                              "_line.png",
                              sep = ""))
    }
    else if (graph_type == "dot") {
      ggsave(filename = paste(region, "_", test$short_name[1],
                              "_dot.png",
                              sep = ""))
    }
    else if (graph_type == "line") {
      ggsave(filename = paste(region, "_", test$short_name[1],
                              "_line_dot.png",
                              sep = ""))
    }
    else {
      ggsave(filename = paste(region, "_", test$short_name[1],
                              "_bar.png",
                              sep = ""))
    }

    graphics.off()
  }
}


percent_change <- function(dataset, region, agg_county = TRUE){
  # Checks percent change in crime from 2014- 2010
  dataset <- clearance_cleaning(dataset, region = "", agg_county = agg_county)
  dataset <- dataset[dataset$year >= 2009,]
  dataset$value[dataset$value < 0] <- 0
  library(quantmod)

  if (!agg_county) {
    names(dataset) <- gsub("agency_name", "region", names(dataset))
  }

  for (i in dataset$region) {
    if (length(unique(dataset$year[dataset$region == i])) < 6) {
      dataset <- dataset[dataset$region != i,]
    }
  }


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

  dataset$region <- as.character(dataset$region)
  dataset$offense_name <- as.character(dataset$offense_name)
  dataset$percent_change <- 0
  for (i in 2010:2014) {
    for (n in unique(dataset$offense_name)) {
      for (x in unique(dataset$region)) {

        dataset$percent_change[dataset$year == i &
                                 dataset$offense_name == n &
                                 dataset$region == x] <-
            Delt(dataset$value[dataset$year == 2009 &
                                 dataset$offense_name == n &
                                 dataset$region == x],
                 dataset$value[dataset$year == i &
                                 dataset$offense_name == n &
                                 dataset$region == x]) * 100

      }
    }
  }


  dataset$percent_change[dataset$percent_change == "NaN" |
                           dataset$percent_change == "Inf"] <- NA

  return(dataset)
}


percent_change_graph <- function(dataset) {

  dataset$short_name <- gsub("motor motor", "motor", dataset$short_name,
                             ignore.case = TRUE)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  dataset$short_name <- gsub("burglary1", "burglary", dataset$short_name,
                             ignore.case = TRUE)

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "graphs/clearance/percent_change_offenses", sep = ""))
    for (i in unique(dataset$short_name)) {
    z <- dataset[dataset$short_name == i & dataset$Type == "Total Offenses",]

    z$graph_region <- sapply(z$region, simpleCap)

    ggplot(z, aes(x = year, y = percent_change, col = graph_region)) +
      geom_line(size = 1.2) +
      scale_x_discrete(
        limits = c(2009, 2010, 2011, 2012, 2013, 2014)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("% Change") +
      xlab("Year") +
      scale_color_few() +
      ggtitle(paste(simpleCap(i), "\nOffenses Change From 2009 (in %)",
                    sep = " "))
    ggsave(file = paste(i, "_percent_change.png", sep = ""),
           width = 7, height = 7)
    }

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "graphs/clearance/percent_change_clearance", sep = ""))
  for (i in unique(dataset$short_name)) {
    z <- dataset[dataset$short_name == i & dataset$Type == "Cleared Cases",]

    z$graph_region <- gsub("_", " ", z$region, ignore.case = TRUE)
    z$graph_region <- sapply(z$graph_region, simpleCap)

    ggplot(z, aes(x = year, y = percent_change, col = graph_region)) +
      geom_line(size = 1.2) +
      scale_x_discrete(
        limits = c(2009, 2010, 2011, 2012, 2013, 2014)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("% Change in Offenses") +
      xlab("Year") +
      scale_color_few() +
      ggtitle(paste(simpleCap(i), "\nClearance Change From 2009 (in %)",
                    sep = " "))
    ggsave(file = paste(i, "_percent_change_clearance.png", sep = ""),
           width = 7, height = 7)
  }

}





percent_change_graph_unagg <- function(dataset) {

  dataset$short_name <- gsub("motor motor", "motor", dataset$short_name,
                             ignore.case = TRUE)
  dataset$short_name <- gsub("burglary1", "burglary", dataset$short_name,
                             ignore.case = TRUE)

  dataset$region <- gsub("SP.", "", dataset$region, ignore.case = TRUE)
  dataset$region <- tolower(dataset$region)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }


  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "graphs/clearance/percent_change_bucks/total_offenses",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    z <- dataset[dataset$short_name == i & dataset$Type == "Total Offenses",]

    z$graph_region <- sapply(z$region, simpleCap)
    z <- z[order(z$region),]


    ggplot(z, aes(x = year, y = percent_change, group = graph_region)) +
      geom_path(colour = "grey36")  +
      geom_path(data = z[z$region %in% c("bensalem township",
                         "bristol township",
                         "middletown township"),], aes(x = year,
                                        y = percent_change,
                                        col = graph_region),
                size = 1.2) +
      scale_x_discrete(
        limits = c(2009, 2010, 2011, 2012, 2013, 2014)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("% Change") +
      xlab("Year") +
      scale_color_few() +
      ggtitle(paste(simpleCap(i), "\nOffenses Change From 2009 (in %)",
                    sep = " "))
    ggsave(file = paste(i, "_percent_change.png", sep = ""),
           width = 7, height = 7)
  }

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
              "graphs/clearance/percent_change_bucks/cleared_cases",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    z <- dataset[dataset$short_name == i & dataset$Type == "Cleared Cases",]

    z$graph_region <- gsub("_", " ", z$region, ignore.case = TRUE)
    z$graph_region <- sapply(z$graph_region, simpleCap)


    ggplot(z, aes(x = year, y = percent_change, group = graph_region)) +
      geom_path(colour = "grey36")  +
      geom_path(data = z[z$region %in% c("bensalem township",
                       "bristol township",
                       "middletown township"),], aes(x = year,
                                                     y = percent_change,
                                                     col = graph_region),
                size = 1.2) +
      scale_x_discrete(
        limits = c(2009, 2010, 2011, 2012, 2013, 2014)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("% Change") +
      xlab("Year") +
      scale_color_few() +  ggtitle(paste(simpleCap(i),
                                         "\nClearance Change From 2009 (in %)",
                    sep = " "))
    ggsave(file = paste(i, "_percent_change_clearance.png", sep = ""),
           width = 7, height = 7)
  }

}





clearance_cleaning_monthly <- function(dataset) {

  dataset$agency_name[!dataset$agency_name %in% c("BRISTOL TOWNSHIP",
                                      "BENSALEM TOWNSHIP",
                                      "MIDDLETOWN TOWNSHIP")] <-
                                      "Other Jurisdictions"
  dataset$agency_name <- gsub(" TOWNSHIP", "", dataset$agency_name)
  dataset$agency_name[dataset$agency_name %in% c("BRISTOL",
                                                 "MIDDLETOWN")] <-
                                      "Adjacent Jurisdictions"

  # Aggregates Other jurisidctions
  x <- dataset[dataset$agency_name == "Other Jurisdictions",]
  x <- aggregate( x[,5:ncol(x)], x[,1:4], FUN = sum )
  dataset <- dataset[dataset$agency_name != "Other Jurisdictions",]
  dataset <- rbind(dataset, x)

  # Aggregates adjacent jurisdictions - Bristol, Falls, Middletown
  x <- dataset[dataset$agency_name == "Adjacent Jurisdictions",]
  x <- aggregate( x[,5:ncol(x)], x[,1:4], FUN = sum )
  dataset <- dataset[dataset$agency_name %in% c("BENSALEM",
                     "Other Jurisdictions"),]
  dataset <- rbind(dataset, x)


  dataset <- dataset[, grep("act_|year|tot_clr|agency_name",
                            names(dataset))]
  dataset <- dataset[, -grep("yearly", names(dataset))]

  library(reshape)
  library(lubridate)

  dataset <- melt(dataset, id = c("year", "agency_name"))
  names(dataset)[3] <- "offense_name"
  dataset$offense_name <- as.character(dataset$offense_name)
  dataset$month <- gsub("(...).*", "\\1", dataset$offense_name)
  dataset$month <- sapply(dataset$month,function(x)
    grep(paste("(?i)",x,sep = ""),month.abb))
  dataset$month_year <- paste(dataset$month, "1",dataset$year, sep = "/")
  dataset$month_year <- mdy(dataset$month_year)

  dataset$short_name <- gsub(".*num_|.*clr_|.*act__",
                             "", dataset$offense_name)
  dataset$short_name <- gsub("__totl$|_to$|__tota$|__tot$|__total$",
                             "", dataset$short_name)
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
                             "", dataset$short_name)
  dataset$short_name <- gsub("entrno frc|entryno forc",
                             "Non-force entry", dataset$short_name)
  dataset$short_name <- gsub("oth clr vhc thft|oth clr vehicle thft",
                             "Other vehicle theft", dataset$short_name)
  dataset$short_name <- gsub("truckbus thft",
                             "Theft from truck or bus", dataset$short_name)
  dataset$short_name <- gsub("forciblerape",
                             "forcible rape", dataset$short_name)
  dataset$short_name <- gsub("trckbs thft|trckbus thft|trckbs theft",
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
  dataset$short_name <- gsub("knife robry|knife robr", "knife robbery",
                             dataset$short_name,
                             ignore.case = TRUE)
  dataset$short_name <- gsub("trckbs theft", "theft from truck or bus",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub(".Robbery", "robbery",
                             dataset$short_name, ignore.case = TRUE)
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
  dataset$short_name <- gsub("^attemptedrap$",
                             "attempted rape",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^assault$",
                             "total assault",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^burglary$",
                             "burglary1",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("str arm ro",
                             "strong arm robbery",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^vehicle thft$",
                             "motor vehicle theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("other motor",
                             "other",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("trckbus thf",
                             "Theft from truck or bus",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("thef$",
                             "theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("attemptedburglr",
                             "attempted burglary",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("thf$",
                             "theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("rober$|robber$",
                             "robbery",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("ro$",
                             "robbery",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("assl$|asl$",
                             "assault",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("entrno fr",
                             "Non-forcible entry",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^thft$",
                             "theft",
                             dataset$short_name, ignore.case = TRUE)
  dataset$short_name <- gsub("^gun assaul$",
                             "gun assault",
                             dataset$short_name, ignore.case = TRUE)

  dataset$Type <- "Total Offenses"
  dataset$Type[grep("clr", dataset$offense_name, ignore.case = TRUE)] <-
    "Cleared Cases"


  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  dataset$short_name <- tolower(dataset$short_name)
  dataset$short_name <-
    sapply(dataset$short_name, simpleCap)

  return(dataset)
}


monthly_percent_change <- function(dataset) {
  dataset <- clearance_cleaning_monthly(dataset)
  dataset <- dataset[dataset$year >= 2009,]
  dataset$value[dataset$value < 0] <- 0
  library(quantmod)


  # 95472 to 22464 rows
  for (i in unique(dataset$agency_name)) {
    if (nrow(dataset[dataset$short_name == "All Fields" &
                       dataset$agency_name == i &
                       dataset$value < 10,]) > 1) {
      dataset <- dataset[dataset$agency_name != i,]
    }
  }


  dataset$month <- NULL
  dataset$year <- NULL

  dataset$crime_type <- "property"
  dataset$crime_type[grep("murder|rape|robbery|assault|manslaughter",
                          dataset$short_name, ignore.case = TRUE)] <- "violent"
  dataset$crime_type[dataset$short_name == "all fields"] <- "Total"

  names(dataset)[1] <- "region"

  dataset$month_year <- as.character(dataset$month_year)
  for (i in unique(dataset$region)) {
    for (n in unique(dataset$month_year)) {
      for (m in unique(dataset$Type)) {
        property_row <- data.frame(region = i,
             offense_name = "property_crimes",
             value = sum(dataset$value[dataset$month_year == n &
                                         dataset$region == i &
                                         dataset$Type == m &
                                         dataset$crime_type == "property"]),
             month_year = n,
             short_name = "Property Crime",
             Type = m,
             crime_type = "property")

        property_row_no_larceny <- data.frame(region = i,
                offense_name = "property_crimes_no_larceny",
                value = sum(dataset$value[dataset$month_year == n &
                                            dataset$region == i &
                                            dataset$Type == m &
                                            dataset$crime_type == "property" &
                                            dataset$short_name != "Larceny"]),
                month_year = n,
                short_name = "Property Crime Except Larceny",
                Type = m,
                crime_type = "property")


        violent_row <- data.frame(region = i,
              offense_name = "violent_crimes",
              value = sum(dataset$value[dataset$month_year == n &
                                          dataset$region == i &
                                          dataset$Type == m &
                                          dataset$crime_type == "violent"]),
              month_year = n,
              short_name = "Violent Crime",
              Type = m,
              crime_type = "property")
        dataset <- rbind(dataset, property_row,
                         violent_row,
                         property_row_no_larceny)
      }
    }
  }

  dataset$year <- year((dataset$month_year))
  dataset$region <- as.character(dataset$region)
  dataset$offense_name <- as.character(dataset$offense_name)
  dataset$percent_change <- 0

  dataset$region <- gsub("sp.", "", dataset$region, ignore.case = TRUE)
  dataset$region <- gsub("^Bucks County$", "Bucks County Sheriff",
                         dataset$region, ignore.case = TRUE)

  for (i in unique(dataset$region)) {
    for (n in unique(dataset$offense_name)) {

      if (length(unique(dataset$year[dataset$region == i &
                                     dataset$offense_name == n])) < 6) {
        dataset <- dataset[dataset$region != i,]
      }
    }
  }

  dataset$value_real <- dataset$value
  dataset$value <- dataset$value + 1

  for (i in 2010:2014) {
    for (n in unique(dataset$offense_name)) {
      for (x in unique(dataset$region)) {

        dataset$percent_change[dataset$year == i &
                                 dataset$offense_name == n &
                                 dataset$region == x] <-
          Delt(dataset$value[dataset$year == 2009 &
                               dataset$offense_name == n &
                               dataset$region == x],
               dataset$value[dataset$year == i &
                               dataset$offense_name == n &
                               dataset$region == x]) * 100

      }
    }
  }


  dataset$percent_change[dataset$percent_change == "NaN" |
                           dataset$percent_change == "Inf"] <- NA

  dataset$region <- tolower(dataset$region)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  dataset$region <-
    sapply(dataset$region, simpleCap)



  return(dataset)

  }


monthly_percent_graph <- function(dataset, span) {


  dataset$short_name <- gsub("1", "", dataset$short_name)
  dataset$short_name <- gsub("^ ", "", dataset$short_name)
  dataset$short_name <- gsub("Trckbs Theft", "Theft From Truck or Bus",
                             dataset$short_name)
  dataset$short_name <- gsub("Entryno For", "Non-forcible Entry",
                             dataset$short_name)
  dataset$short_name <- gsub("Hndft|Hndfeet", "Unarmed",
                             dataset$short_name)
  dataset$short_name <- gsub("(.*)(robbery)", "\\1 Robbery",
                             dataset$short_name)

dataset$month_year <- as.Date(dataset$month_year)
library(scales)

setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
            "graphs/clearance/percent_change_month/bucks_offenses",
            sep = ""))


dataset$region <- factor(dataset$region, levels = c("Bensalem",
                                                    "Adjacent Jurisdictions",
                                                    "Other Jurisdictions"))

for (i in unique(dataset$short_name)) {
  test <- dataset[dataset$short_name == i &
                    dataset$Type == "Total Offenses",]

  test <- test[order(test$region),]


#
#   test <- test[test$region %in% c("Bensalem Township",
#                                   "Bristol Township",
#                                   "Middletown Township"),]

  ggplot(data = test, aes(x = month_year, y = percent_change,
                          col = region)) +
    stat_smooth(se = FALSE, span = span,
                method = "loess", size = 1.2) +
    ggtitle(paste(test$short_name[1], " Offenses",
                  "\n", "Bucks County (Monthly % Change Since 2009)",
                  sep = "")) +
    theme_fivethirtyeight() +
    scale_x_date(breaks = date_breaks("2 year"),
                 labels = date_format("%Y")) +
    theme(axis.title = element_text(), legend.title = element_blank()) +
    ylab("% Change") + xlab("Year") +
    scale_color_manual(values = c("#FF2700",
                                  "#77AB43",
                                  "#008FD5"))

  ggsave(file = paste(i, "_percent_change_bucks_monthly.png", sep = ""),
         width = 7, height = 7)
}


setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
            "graphs/clearance/percent_change_month/bensalem_offenses",
            sep = ""))


for (i in unique(dataset$short_name)) {
  test <- dataset[dataset$short_name == i &
                    dataset$Type == "Total Offenses",]
  test <- test[order(test$region),]


  test <- test[test$region %in% c("Bensalem"),]

  ggplot(data = test, aes(x = month_year, y = percent_change, col = region)) +
    geom_point(colour = "grey36") +
    stat_smooth(se = FALSE, span = span, method = "loess")  +
    ggtitle(paste(test$short_name, " Offenses",
                  "\n", "Bensalem (Monthly % Change Since 2009)",
                  sep = "")) +
    theme_fivethirtyeight() +
    scale_x_date(breaks = date_breaks("2 year"),
                 labels = date_format("%Y")) +
    theme(axis.title = element_text(), legend.title = element_blank()) +
    ylab("% Change") + xlab("Year")
  scale_color_manual(values = "#FF2700")

  ggsave(file = paste(i, "_percent_change_bensalem_monthly.png", sep = ""),
         width = 7, height = 7)
}

setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
            "graphs/clearance/percent_change_month/bucks_clearances",
            sep = ""))

for (i in unique(dataset$short_name)) {
  test <- dataset[dataset$short_name == i &
                    dataset$Type == "Total Offenses",]


  ggplot(data = test, aes(x = month_year, y = percent_change,
                          col = region)) +
    stat_smooth(se = FALSE, span = span,
                method = "loess", size = 1.2) +
    ggtitle(paste(test$short_name, " Clearances",
                  "\n", "Bucks County (Monthly % Change Since 2009)",
                  sep = "")) +
    theme_fivethirtyeight() +
    scale_x_date(breaks = date_breaks("2 year"),
                 labels = date_format("%Y")) +
    theme(axis.title = element_text(), legend.title = element_blank()) +
    ylab("% Change") + xlab("Year") +
    scale_color_manual(values = c("#FF2700",
                                  "#77AB43",
                                  "#008FD5"))

  ggsave(file = paste(i, "_percent_change_bucks_monthly.png", sep = ""),
         width = 7, height = 7)
}


setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/",
            "graphs/clearance/percent_change_month/bensalem_clearances",
            sep = ""))


for (i in unique(dataset$short_name)) {
  test <- dataset[dataset$short_name == i &
                    dataset$Type == "Total Offenses",]


  test <- test[test$region %in% c("Bensalem"),]
  test <- test[order(test$region),]


  ggplot(data = test, aes(x = month_year, y = percent_change, col = region)) +
    geom_point(colour = "grey36") +
    stat_smooth(se = FALSE, span = span, method = "loess")  +
    ggtitle(paste(test$short_name, " Clearances",
                  "\n", "Bensalem (Monthly % Change Since 2009)",
                  sep = "")) +
    theme_fivethirtyeight() +
    scale_x_date(breaks = date_breaks("2 year"),
                 labels = date_format("%Y")) +
    theme(axis.title = element_text(), legend.title = element_blank()) +
    ylab("% Change") + xlab("Year") +
    scale_color_manual(values = "#FF2700")

  ggsave(file = paste(i, "_percent_change_bensalem_monthly.png", sep = ""),
         width = 7, height = 7)
}
}


monthly_crime_graph <- function(dataset, span) {

  dataset$value[dataset$value < 1] <- 0
  dataset$short_name[dataset$short_name == " Robbery"] <- "Robbery"
  dataset$short_name[dataset$short_name == "Hndfeet Assault"] <-
                                           "Unarmed Assault"
  dataset$short_name[dataset$short_name == "Burglary1"] <- "Burglary"
  dataset$short_name[dataset$short_name == "Entryno For"] <-
                                           "Non-forcible Entry"
  dataset$short_name[dataset$short_name == "Trckbs Theft"] <-
                                           "Theft From Truck Or Bus"
  dataset$short_name[dataset$short_name == "Strong Armrobbery"] <-
    "Strongarm Robbery"
  dataset$short_name[dataset$short_name == "Strong Arm Robbery"] <-
    "Strongarm Robbery"
  dataset$short_name[dataset$short_name == "Kniferobbery"] <-
    "Knife Robbery"
  dataset$short_name[dataset$short_name == "Other Weaponrobbery"] <-
    "Other Weapon Robbery"
  dataset$short_name[dataset$short_name == "Gunrobbery"] <-
    "Gun Robbery"


  for (i in unique(dataset$agency_name)) {
    if (nrow(dataset[dataset$short_name == "All Fields" &
                     dataset$agency_name == i &
                     dataset$value < 10 &
                     dataset$value != 0,]) > 1) {
      dataset <- dataset[dataset$agency_name != i,]
    }
  }

  dataset$month <- NULL
  dataset$year <- NULL
  names(dataset)[1] <- "region"
  dataset$region <- tolower(dataset$region)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }

  dataset$region <-
    sapply(dataset$region, simpleCap)

  dataset$crime_type <- "property"
  dataset$crime_type[grep("murder|rape|robbery|assault|manslaughter",
                          dataset$short_name, ignore.case = TRUE)] <- "violent"
  dataset$crime_type[dataset$short_name == "all fields"] <- "Total"


  dataset$month_year <- as.character(dataset$month_year)
  for (i in unique(dataset$region)) {
    for (n in unique(dataset$month_year)) {
      for (m in unique(dataset$Type)) {
        property_row <- data.frame(region = i,
           offense_name = "property_crimes",
           value = sum(dataset$value[dataset$month_year == n &
                                       dataset$region == i &
                                       dataset$Type == m &
                                       dataset$crime_type == "property"]),
           month_year = n,
           short_name = "Property Crime",
           Type = m,
           crime_type = "property")

        property_row_no_larceny <- data.frame(region = i,
              offense_name = "property_crimes_no_larceny",
              value = sum(dataset$value[dataset$month_year == n &
                                          dataset$region == i &
                                          dataset$Type == m &
                                          dataset$crime_type == "property" &
                                          dataset$short_name != "Larceny"]),
              month_year = n,
              short_name = "Property Crime Except Larceny",
              Type = m,
              crime_type = "property")


        violent_row <- data.frame(region = i,
            offense_name = "violent_crimes",
            value = sum(dataset$value[dataset$month_year == n &
                                        dataset$region == i &
                                        dataset$Type == m &
                                        dataset$crime_type == "violent"]),
            month_year = n,
            short_name = "Violent Crime",
            Type = m,
            crime_type = "property")
      dataset <- rbind(dataset, property_row,
                         violent_row,
                         property_row_no_larceny)
      }
    }
  }

dataset$month_year <- as.Date(dataset$month_year)


dataset$region <- factor(dataset$region, levels = c("Bensalem",
                                                    "Adjacent Jurisdictions",
                                                    "Other Jurisdictions"))

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
              "clearance/monthly_offenses_clearances/bensalem_offenses",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    test <- dataset[dataset$short_name == i &
                      dataset$Type == "Total Offenses",]
    test <- test[test$region %in% c("Bensalem"),]

    ggplot(data = test, aes(x = month_year, y = value, col = region)) +
      geom_point(colour = "grey36") +
      stat_smooth(se = FALSE, span = span,
                  method = "loess")  +
      ggtitle(paste(test$short_name, " Offenses",
                    "\n", "Bensalem, PA",
                    sep = "")) +
      theme_fivethirtyeight() +
      scale_x_date(breaks = date_breaks("2 year"),
                   labels = date_format("%Y")) +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("# of Cases") + xlab("Year") +
      scale_color_manual(values = "#FF2700")
    ggsave(file = paste(i, "_offenses_bensalem_monthly.png", sep = ""),
           width = 7, height = 7)
  }

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
              "clearance/monthly_offenses_clearances/bensalem_clearances",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    test <- dataset[dataset$short_name == i &
                      dataset$Type == "Cleared Cases",]
    test <- test[test$region %in% c("Bensalem"),]

    ggplot(data = test, aes(x = month_year, y = value, col = region)) +
      geom_point(colour = "grey36") +
      stat_smooth(se = FALSE, span = span,
                  method = "loess")  +
      ggtitle(paste(test$short_name, " Clearances",
                    "\n", "Bensalem, PA",
                    sep = "")) +
      theme_fivethirtyeight() +
      scale_x_date(breaks = date_breaks("2 year"),
                   labels = date_format("%Y")) +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("# of Cases") + xlab("Year") +
      scale_color_manual(values = "#FF2700")
    ggsave(file = paste(i, "_clearances_bensalem_monthly.png", sep = ""),
           width = 7, height = 7)
  }


  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
              "clearance/monthly_offenses_clearances/bucks_offenses",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    test <- dataset[dataset$short_name == i &
                      dataset$Type == "Total Offenses",]


    ggplot(data = test, aes(x = month_year, y = value,
                            col = region)) +
      stat_smooth(se = FALSE, span = span,
                  method = "loess", size = 1.2) +
      ggtitle(paste(test$short_name, " Offenses",
                    "\n", "Bucks County, PA",
                    sep = "")) +
      theme_fivethirtyeight() +
      scale_x_date(breaks = date_breaks("2 year"),
                   labels = date_format("%Y")) +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("# of Cases") + xlab("Year") +
      scale_color_manual(values = c("#FF2700",
                                    "#77AB43",
                                    "#008FD5"))

    ggsave(file = paste(i, "_offenses_bucks_monthly.png", sep = ""),
           width = 7, height = 7)

  }

  setwd(paste("C:/Users/user/Dropbox/R_project/touch_DNA/graphs/",
              "clearance/monthly_offenses_clearances/bucks_clearances",
              sep = ""))
  for (i in unique(dataset$short_name)) {
    test <- dataset[dataset$short_name == i &
                      dataset$Type == "Cleared Cases",]


    ggplot(data = test, aes(x = month_year, y = value,
                            col = region)) +
      stat_smooth(se = FALSE, span = span,
                  method = "loess", size = 1.2) +
      ggtitle(paste(test$short_name, " Clearances",
                    "\n", "Bucks County, PA",
                    sep = "")) +
      theme_fivethirtyeight() +
      scale_x_date(breaks = date_breaks("2 year"),
                   labels = date_format("%Y")) +
      theme(axis.title = element_text(), legend.title = element_blank()) +
      ylab("# of Cases") + xlab("Year") +
      scale_color_manual(values = c("#FF2700",
                                    "#77AB43",
                                    "#008FD5"))

    ggsave(file = paste(i, "_clearances_bucks_monthly.png", sep = ""),
           width = 7, height = 7)

  }
}
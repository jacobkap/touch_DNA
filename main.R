source('C:/Users/user/Dropbox/R_project/touch_DNA/read_files.R')
source('C:/Users/user/Dropbox/R_project/touch_DNA/graphing.R')
setwd("C:/Users/user/Dropbox/R_project/touch_DNA/clean_data")

bensalem_arrest <- UCR_arrests()
bensalem_arrest_summary <- UCR_arrests_summarized()

system.time(bensalem_clearance <- UCR_clearance(AGENCY = "bensalem"))
system.time(penn_clearance <- UCR_clearance(STATE = "Pennsylvania"))
system.time(bucks_clearance <- UCR_clearance(COUNTY = 9, STATE = "Pennsylvania"))
save(bensalem_clearance, file = "bensalem_clearance.rda")
save(bucks_clearance, file = "bucks_clearance.rda")
save(penn_clearance, file = "penn_clearance.rda")

# Make monthly percent change data and graphs
system.time(bucks_clearance_unagg <- UCR_clearance(COUNTY = 9,
                                              STATE = "Pennsylvania",
                                              agg_county = FALSE))
system.time(bucks_monthly <-
              clearance_cleaning_monthly(bucks_clearance_unagg))
system.time(bucks_monthly_percent <- monthly_percent_change(bucks_clearance_unagg))
monthly_percent_graph(bucks_monthly_percent, span = 0.16)
monthly_crime_graph(bucks_monthly, span = 0.16)


bucks_clearance_unagg_percent <- percent_change(bucks_clearance_unagg,
                                                agg_county = FALSE)
percent_change_graph_unagg(bucks_clearance_unagg_percent)
save(bucks_clearance_unagg_percent, file = "bucks_clearance_unagg_percent.rda")
write.csv(bucks_clearance_unagg_percent,
          file = "bucks_clearance_unagg_percent.csv")

bensalem_property <- UCR_property()

setwd("C:/Users/user/Dropbox/R_project/touch_DNA/clean_data")
save(bensalem_arrest, file = "bensalem_arrest.rda")
save(bensalem_arrest_summary, file = "bensalem_arrest_summary.rda")
save(bensalem_property, file = "bensalem_property.rda")

write.csv(bensalem_arrest, "bensalem_arrest.csv")
write.csv(bensalem_arrest_summary, "bensalem_arrest_summary.csv")
write.csv(bensalem_clearance, "bensalem_clearance.csv")
write.csv(bensalem_property, "bensalem_property.csv")

summary_arrest_all()

# Makes plots for Pennsylvania
clearance_plot(dataset = penn_clearance, region = "Pennsylvania",
               graph_type = "line")
clearance_plot(dataset = penn_clearance, region = "Pennsylvania",
               graph_type = "dot")
clearance_plot(dataset = penn_clearance, region = "Pennsylvania",
               graph_type = "bar")
clearance_plot(dataset = penn_clearance, region = "Pennsylvania",
               graph_type = "dot_line")

# Make plots for Bucks County
clearance_plot(dataset = bucks_clearance, region = "bucks_county",
               graph_type = "line")
clearance_plot(dataset = bucks_clearance, region = "bucks_county",
               graph_type = "dot")
clearance_plot(dataset = bucks_clearance, region = "bucks_county",
               graph_type = "bar")
clearance_plot(dataset = bucks_clearance, region = "bucks_county",
               graph_type = "dot_line")

# Make plots for Bensalem
clearance_plot(dataset = bensalem_clearance, region = "bensalem",
               graph_type = "line")
clearance_plot(dataset = bensalem_clearance, region = "bensalem",
               graph_type = "dot")
clearance_plot(dataset = bensalem_clearance, region = "bensalem",
               graph_type = "bar")
clearance_plot(dataset = bensalem_clearance, region = "bensalem",
               graph_type = "dot_line")


bensalem_change <- percent_change(bensalem_clearance, region = "bensalem")
bucks_county_change <- percent_change(bucks_clearance, region = "bucks_county")
pennsylvania_change <- percent_change(penn_clearance, region = "Pennsylvania")

all_change <- rbind(bensalem_change, bucks_county_change, pennsylvania_change)
percent_change_graph(all_change)
save(all_change, file = "region_change_percent.rda")
write.csv(all_change, file = "region_change_percent.csv")

group_change <- bensalem_change
group_change$bucks_county_offenses_in_2009 <-
            bucks_county_change$bucks_county_offenses_in_2009
group_change$bucks_county_offenses_in_2013 <-
            bucks_county_change$bucks_county_offenses_in_2013
group_change$Pennsylvania_offenses_in_2009 <-
            pennsylvania_change$Pennsylvania_offenses_in_2009
group_change$Pennsylvania_offenses_in_2013 <-
            pennsylvania_change$Pennsylvania_offenses_in_2013
group_change$Pennsylvania_percent_change <-
            pennsylvania_change$Pennsylvania_percent_change
group_change$bucks_county_percent_change <-
            bucks_county_change$bucks_county_percent_change
temp_bensalem_change <- group_change$bensalem_percent_change
group_change$bensalem_percent_change <- NULL
group_change$bensalem_percent_change <- temp_bensalem_change
group_change$crime_name <- group_change$short_name
group_change$short_name <- NULL

# Make numeric
group_change$Pennsylvania_percent_change <-
              as.numeric(group_change$Pennsylvania_percent_change)
group_change$bucks_county_percent_change <-
              as.numeric(group_change$bucks_county_percent_change)
group_change$bensalem_percent_change <-
              as.numeric(group_change$bensalem_percent_change)
save(group_change, file = "group_percent_change.rda")
write.csv(group_change, file = "group_percent_change.csv")

# Determines percent change of property crimes
Delt(sum(group_change$bensalem_offenses_in_2009[group_change$crime_type ==
            "property" & group_change$crime_name != "larceny"]),
     sum(group_change$bensalem_offenses_in_2013[group_change$crime_type ==
            "property" & group_change$crime_name != "larceny"])) * 100
Delt(sum(group_change$bucks_county_offenses_in_2009[group_change$crime_type ==
            "property" & group_change$crime_name != "larceny"]),
     sum(group_change$bucks_county_offenses_in_2013[group_change$crime_type ==
            "property" & group_change$crime_name != "larceny"])) * 100
Delt(sum(group_change$Pennsylvania_offenses_in_2009[group_change$crime_type ==
            "property" & group_change$crime_name != "larceny"]),
     sum(group_change$Pennsylvania_offenses_in_2013[group_change$crime_type ==
             "property" & group_change$crime_name != "larceny"])) * 100



bensalem_clearance_monthly <- clearance_cleaning_monthly(bensalem_clearance)

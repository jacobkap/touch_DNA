source('C:/Users/user/Dropbox/R_project/touch_DNA/read_files.R')
source('C:/Users/user/Dropbox/R_project/touch_DNA/graphing.R')

bensalem_arrest <- read_merge_bensalem_arrests()
bensalem_arrest_summary <- read__merge_bensalem_arrests_summarized()
bensalem_clearance <- read__merge_bensalem_clearance()
bensalem_property <- read__merge_bensalem_property()

setwd("C:/Users/user/Dropbox/R_project/touch_DNA/clean_data")
save(bensalem_arrest, file = "bensalem_arrest.rda")
save(bensalem_arrest_summary, file = "bensalem_arrest_summary.rda")
save(bensalem_clearance, file = "bensalem_clearance.rda")
save(bensalem_property, file = "bensalem_property.rda")
write.csv(bensalem_arrest, "bensalem_arrest.csv")
write.csv(bensalem_arrest_summary, "bensalem_arrest_summary.csv")
write.csv(bensalem_clearance, "bensalem_clearance.csv")
write.csv(bensalem_property, "bensalem_property.csv")

summary_arrest_all()


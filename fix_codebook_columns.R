setwd("C:/Users/user/Dropbox/R_project/touch_DNA/raw_data/clearance_by_arrest")

library(tm)
Rpdf <- readPDF(control = list(text = "-layout"))

# Read the PDF
codebook <- Corpus(URISource("clearance_by_arrest_codebook.pdf"),
                   readerControl = list(reader = Rpdf))

# Turn values into a data.frame
codebook <- data.frame(codebook[[1]]$content)
# Remove the excess start/ending rows
codebook <- data.frame(codebook[16:2103,])
# Make into character
codebook[,1] <- as.character(codebook[,1])
# Gets rid of empty rows
codebook[codebook[,1] == "",] <- NA
codebook <- data.frame(codebook[!is.na(codebook[,1]),])
# Keeps only rows with variable name and code
codebook <- data.frame(codebook[grep("v[0-9]", codebook[,1],
                                     ignore.case = TRUE),])
codebook[,1] <- as.character(codebook[,1])

codebook[,1] <- gsub("...$", "", codebook[,1])

codes <- data.frame(gsub(" .*", "", codebook[,1]))
codebook[,1] <- gsub(" {2,}", " ", codebook[,1])

# Remove codes from codebook
codes[,1] <- as.character(codes[,1])
for (i in 1:nrow(codebook)) {
  codebook[i, 1] <- gsub(codes[i, 1], "", codebook[i, 1])
}

# Remove trailing/leading spaces
codebook[,1] <- gsub("^ | $", "", codebook[,1])
# Make all lowercase
codebook[,1] <- tolower(codebook[,1])
codebook[,1] <- gsub(":|-|/|#", "", codebook[,1])
codebook[,1] <- gsub("<", "_under", codebook[,1])
codebook[,1] <- gsub("(.*)tot", "\\1 tot", codebook[,1])
codebook[,1] <- gsub(" ", "_", codebook[,1])

codebook$codes <- codes[,1]
names(codebook)[1] <- "column_name"
names(codebook)[2] <- "column_code"

save(codebook, file = "codebook_clearance.rda")

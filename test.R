df <- read.csv("./data/orcid_data_latlng.csv")

# Search for John Ruan in the ORCID column
unique(df$orcid1[grepl("ruan", df$org1, ignore.case = TRUE)])


# This script takes a SAM file as input and counts the mapped entries by position and size,
# then creates a dataframe of the count data and exports it as an .xls

# SAM header names
header <- c("QNAME", "FLAG", "RNAME", "POS", "MAPQ", "CIGAR", "MRNM", "MPOS", "ISIZE", "SEQ", "QUAL", rep("OPT",3))

# SAM file. Must only contain mapped sequences. I had trouble parsing the file with the unmapped sequences, so
# I had to do it this way, which is a bit less robust. But since you told me you had removed the unmapped reads
# from your files anyway, I trust this won't be too much of a problem.
reads <- read.table("~/Desktop/sam2xls/test.sam", comment.char = "@", sep = "\t", fill = TRUE, col.names = header)

# Separate Passenger/sense from guide/antisense mapped reads
sense <- subset(reads, FLAG == 0)
antisense <- subset(reads, FLAG == 16)

# Count the number of occurrences of each record in each group
sense.counts <- as.data.frame(table(sense$POS))
antisense.counts <- as.data.frame(table(antisense$POS))

# Convert positions back to numeric
sense.counts$Var1 <- as.numeric(as.character(sense.counts$Var1))
antisense.counts$Var1 <- as.numeric(as.character(antisense.counts$Var1))

# Add zeroes to positions without reads and merge
sense.full <- data.frame(Var1 = seq(max(sense.counts$Var1)))
sense.full <- merge(sense.full, sense.counts, all.x = TRUE)
sense.full[is.na(sense.full)] <- 0

antisense.full <- data.frame(Var1 = seq(max(antisense.counts$Var1)))
antisense.full <- merge(antisense.full, antisense.counts, all.x = TRUE)
antisense.full[is.na(antisense.full)] <- 0

# make new data frame with counts
full <- merge(sense.full, antisense.full, all.x = TRUE, by = "Var1")
full$total <- sense.full$Freq + antisense.full$Freq

# add column names
names(full) <- c("position", "passenger", "guide", "total")

# output to csv
write.csv(full, "mapped_reads.csv")

# Here's a mirror plot of the data output
png("hist.mapped.reads.png", width = 1280, height = 480, res = 144)

plot(x = full$position, y = full$passenger, type = "h", pch = 19, col = "black",
     ylim = c(-max(full$guide), max(full$passenger)),
     main = "Distribution of mapped reads",
     xlab = "Position (bp)",
     ylab = "Frequency (counts)")
points(x = full$position, y = -full$guide, type = "h", pch = 19, col = "blue")

dev.off()
#!/usr/bin/env/ Rscript

# script to parse multiple sequences in a fasta file

setwd("/Users/jrclayton/Desktop/")

#import the data

# First read in the file
fas <- scan("seqTEPs_old", what = "character", blank.lines.skip = TRUE, strip.white = TRUE)

# add a unique symbol before and after each name for filtering
intervals <- grep("^>", fas, value = FALSE)
for (i in intervals){ fas[i] <- paste("@@@", fas[i], "@@@", sep = '') }

# collapses and splits just how we want
fas <- unlist(strsplit(paste(fas, collapse = ''), "@@@"))

# get seqnames
seqnames <- grep("^>", fas, value = TRUE)

# get seqs
letter.or.hyphen <- paste(c("^[", LETTERS, letters, "-]"), collapse = '')
seqs <- grep(letter.or.hyphen, fas, value = TRUE)

# write each sequence to a file named headername (with backslashes removed, if present)
for (i in 1:length(seqnames)) {
  write(c(seqnames[i], seqs[i]),
        file = paste("./parsed/", substring(gsub("\\/", "_", seqnames)[i], 2), ".fas", sep = ''))
}
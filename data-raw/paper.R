## code to prepare `paper` dataset goes here

# Derived from https://raw.githubusercontent.com/visioguy/PaperSizes/master/PaperSizes.json
ss <- "https://docs.google.com/spreadsheets/d/1P-WHYSIhVu0TR-_023hmNbNrcYY0dA4_957shbTC6Bc/edit?usp=sharing"

paper_sizes <-
  googlesheets4::read_sheet(ss)

paper_sizes$name <- as.character(paper_sizes$name)

usethis::use_data(paper_sizes, overwrite = TRUE)

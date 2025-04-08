library(tidyverse) # version: tidyverse_2.0.0
library(dplyr) #version: dplyr_1.1.4
library(stats) # version: stats4
library(fastDummies) # version: fastDummies_1.7.3
library(reshape2) #version: reshape2_1.4.4
library(testthat)

## IRT-M estimation:
#devtools::install_github("dasiegel/IRT-M")
library(IRTM) #version 1.00

test_that("Test Case One: Normal Use", {

  synth_questions <- NULL  # Initialize to avoid R CMD check notes

  load("synth_questions.rda")
  ## Convert numeric ordinal responses to factors

  ebdatsub <- lapply(ebdatsynth[,], factor) ## that's a list now

  ## converts the list back into a dataframe:
  Y <- dummy_cols(.data=ebdatsub,
                  remove_selected_columns=TRUE)

  ## remove the .data that dummy_cols adds to the column names
  colnames(Y) <- gsub(".data.", '', colnames(Y))

  ## remove the data objects:
  rm(ebdatsub)
  rm(ebdatsynth)

  load('mcodes.rda')

  ## Only keep M-Codes with loadings or outcomes:
  MCodes$encoding <- rowSums(abs(MCodes[,4:9]))
  MCodes <- MCodes[which(MCodes$encoding > 0),]

  d <- 6 #number of coded dimensions
  mcolumns <- c("QMap", "D1-Culture threat",
                "D2-ReligionThreat",
                "D3-Economic Threat",
                "D4-HealthThreat",
                "O1-OutcomeSupportImmigration", "O2-OutcomeSupportEU")

  combine <- MCodes[,mcolumns] %>% ## question codes and loadings
    inner_join(
      Y %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        type_convert() %>%
        rownames_to_column(var = "question"),
      by = c("QMap" = "question"  )
    )

  M_matrix <- as.data.frame(combine[, 1:(d+1)])

  #Reverse the earlier transposition of the observations:
  Y_in <- combine[, (d+2):ncol(combine)]%>%
    t() %>%
    as.data.frame()

  Y_in <- as.data.frame(sapply(Y_in, as.numeric))

  ## Take the question names and
  ## convert to column names

  question <- combine[,1] %>%
    as.data.frame()
  colnames(Y_in) <- question[,1]
  rm(combine)
  rm(question)

  #Run IRT-M
  d=6
  irt <- irt_m(Y_in = Y_in, d = d, M_matrix = M_matrix, nsamp = 100, nburn=100, thin=1)

  expect_type(irt, "list")
  expect_equal(length(irt), 5)
}) ## closes the test_that() call

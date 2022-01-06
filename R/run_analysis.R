library(dplyr)

fetch_data <- function(filename) {
  if (!file.exists(filename)) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url, destfile = filename, method = "curl")
  }
  if (!file.exists("UCI HAR Dataset")) {
    unzip(filename)
  }
}

load_train <- function(feature_names, relevant) {
  X_train <- read.table(
    "UCI HAR Dataset/train/X_train.txt",
    col.names = feature_names[, "name"]
  )
  X_train <- X_train[as.integer(row.names(relevant))]
  Y_train <- read.table(
    "UCI HAR Dataset/train/Y_train.txt",
    col.names = "activity"
  )
  subject_train <- read.table(
    "UCI HAR Dataset/train/subject_train.txt",
    col.names = "subject"
  )
  cbind(subject_train, Y_train, X_train)
}

load_test <- function(feature_names, relevant) {
  X_test <- read.table(
    "UCI HAR Dataset/test/X_test.txt",
    col.names = feature_names[, "name"]
  )
  X_test <- X_test[as.integer(row.names(relevant))]
  Y_test <- read.table(
    "UCI HAR Dataset/test/Y_test.txt",
    col.names = "activity"
  )
  subject_test <- read.table(
    "UCI HAR Dataset/test/subject_test.txt",
    col.names = "subject"
  )
  cbind(subject_test, Y_test, X_test)
}

load_data <- function(feature_names, relevant) {
  train <- load_train(feature_names, relevant)
  test <- load_test(feature_names, relevant)
  rbind(train, test)
}

rename_cols <- function(data, relevant) {
  # rename feature names
  relevant[, "name"] <- gsub(
    "-mean",
    replacement = "Mean", x = relevant[, "name"], fixed = TRUE
  )
  relevant[, "name"] <- gsub(
    "-std",
    replacement = "Std", x = relevant[, "name"], fixed = TRUE
  )
  relevant[, "name"] <- gsub(
    "()",
    replacement = "", x = relevant[, "name"], fixed = TRUE
  )
  relevant[, "name"] <- gsub(
    "-",
    replacement = "", x = relevant[, "name"], fixed = TRUE
  )
  colnames(data) <- c("subject", "activity", relevant[, "name"])
  data
}

main <- function() {
  fetch_data("activity_data.zip")
  # get labels
  label_names <- read.table(
    "UCI HAR Dataset/activity_labels.txt",
    col.names = c("id", "name"),
    row.names = "id"
  )
  # get feature names
  feature_names <- read.table(
    "UCI HAR Dataset/features.txt",
    col.names = c("id", "name"),
    row.names = "id"
  )
  # filter only relevant feature names
  relevant <- subset(feature_names, grepl(".*mean.*|.*std.*", name))
  # get training data
  complete <- load_data(feature_names, relevant)
  complete <- rename_cols(complete, relevant)
  complete[, "activity"] <- factor(
    complete[, "activity"],
    levels = row.names(label_names), labels = label_names[, "name"]
  )
  # aggregate records
  complete <- complete %>%
    tidyr::pivot_longer(
      !c("subject", "activity"),
      names_to = "variable", values_to = "value"
    ) %>%
    group_by(subject, activity, variable) %>%
    summarize(
      value = mean(value)
    ) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      names_from = "variable", values_from = "value"
    )
  # write tidy data set to file
  write.table(complete, "tidy.txt", row.names = FALSE, quote = FALSE)
}

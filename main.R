library(gt)
library(lmtest)
library(sandwich)
library(xgboost)
library(combinat)

ratings <- read.table(file = 'Data/title.ratings.tsv', sep = '\t', header = TRUE)

basics <- read.table(file = 'Data/title.basics.tsv', sep='\t', header=TRUE,
na.strings = "\\N", fill = TRUE, quote = "")

akas <- read.table(file='Data/title.akas.tsv', sep='\t', header=TRUE,
na.strings="\\N", fill=TRUE, quote="")
akas <- subset(akas, isOriginalTitle == 1)

title_episode <- read.table(file="Data/title.episode.tsv", sep="\t", header=TRUE,
na.strings="\\N", fill=TRUE, quote="")

# Merge ratings and basics
merged <- merge(ratings, basics)

# Split genre
split_genres <- strsplit(merged$genres, split = ",")
merged$genre1 <- sapply(split_genres, function(x) x[1])
merged$genre2 <- sapply(split_genres, function(x) x[2])
merged$genre3 <- sapply(split_genres, function(x) x[3])

# Remove noisy (unpopular) titles
merged <- subset(merged, numVotes >= 100)

# Generate useful subsets
movies <- subset(merged, titleType == "movie")
episodes <- subset(merged, titleType == "tvEpisode")

# SECTION 1

# Generate summary statistics table
numeric_cols <- merged[, sapply(merged, is.numeric)]
merged_summary <- as.data.frame(sapply(numeric_cols, function(x) {
  c(Min = min(x, na.rm=TRUE),
    Q1 = quantile(x, 0.25, na.rm=TRUE),
    Median = median(x, na.rm=TRUE),
    Q3 = quantile(x, 0.75, na.rm=TRUE),
    Max = max(x, na.rm=TRUE),
    Mean = mean(x, na.rm=TRUE),
    SD = sd(x, na.rm=TRUE))
}))
merged_summary <- round(merged_summary, 3)

col_mapping <- c(
  averageRating = "Average Rating",
  numVotes = "# of Votes",
  isAdult = "Is Adult",
  startYear = "Start Year",
  endYear = "End Year",
  runtimeMinutes = "Runtime"
)
merged_summary <- cbind(Statistic = rownames(merged_summary), merged_summary)
colnames(merged_summary) <- c("Statistic", col_mapping[colnames(numeric_cols)])
gtsave(gt(merged_summary), "Figures/merged_summary_table.png")

# Generate frequency table of titleTypes
titleType_ftab <- as.data.frame(table(merged$titleType))
titleType_ftab <- titleType_ftab[order(titleType_ftab$Freq, decreasing=TRUE),]
titleType_ftab <- cbind(titleType_ftab, Percentage = titleType_ftab$Freq / sum(titleType_ftab$Freq))
titleType_ftab$Percentage <- paste0(round(titleType_ftab$Percentage * 100, 1), "%")
colnames(titleType_ftab) <- c("Title Type", "Frequency", "Percentage")
gtsave(gt(titleType_ftab), "Figures/titleType_ftab.png")

# Generate other category for pie chart
props <- titleType_ftab$Frequency / sum(titleType_ftab$Frequency)
keep <- setNames(titleType_ftab$Frequency[props >= 0.03], titleType_ftab$`Title Type`[props >= 0.03])
other_total <- sum(titleType_ftab$Frequency[props < 0.03])
if (other_total > 0) keep <- c(keep, Other = other_total)

# Generate pie chart of titleTypes
png("Figures/titleType_pie.png")
keep_pcts <- paste0(round(keep / sum(keep) * 100, 1), "%")
keep_labels <- paste(names(keep), keep_pcts, sep = ": ")
pie(keep, labels=keep_labels, main="Title Types", init.angle=90, clockwise=TRUE)
dev.off()

# Generate frequency table of genres
genre_ftab <- as.data.frame(table(merged$genre1))
genre_ftab <- genre_ftab[order(genre_ftab$Freq, decreasing=TRUE),]
genre_ftab <- cbind(genre_ftab, Percentage = genre_ftab$Freq / sum(genre_ftab$Freq))
genre_ftab$Percentage <- paste0(round(genre_ftab$Percentage * 100, 1), "%")
colnames(genre_ftab) <- c("Genre", "Frequency", "Percentage")
gtsave(gt(genre_ftab), "Figures/genre_ftab.png")

# Generate "Other" category for pie chart
props <- genre_ftab$Frequency / sum(genre_ftab$Frequency)
keep <- setNames(genre_ftab$Frequency[props >= 0.02], genre_ftab$Genre[props >= 0.02])
other_total <- sum(genre_ftab$Frequency[props < 0.02])
if (other_total > 0) keep <- c(keep, Other = other_total)

# Generate pie chart of genres
keep_pcts <- paste0(round(keep / sum(keep) * 100, 1), "%")
keep_labels <- paste(names(keep), keep_pcts, sep = ": ")
png("Figures/genre_pie.png")
pie(keep, labels=keep_labels, main="Genre Types", init.angle=90, clockwise=FALSE)
dev.off()

# Generate histogram of ratings
png("Figures/ratings_hist.png")
hist(merged$averageRating, main="Average IMDB Rating",
xlab="Rating", ylab="Frequency", col="skyblue", xaxt="n")
axis(1, at=seq(1, 10, by=.5))
dev.off()

# Generate histogram of runtime
png("Figures/runtime_hist.png")
hist(log10(merged$runtimeMinutes[merged$runtimeMinutes <= 240]), main="Runtime",
xlab="Runtime (Minutes)", ylab="log(Frequency)", col="skyblue", xaxt="n")
axis(1, at=log10(c(1, 10, 30, 60, 120, 240)), labels=c(1, 10, 30, 60, 120, 240))
dev.off()

# Generate histogram of movie runtime
png("Figures/movie_runtime_hist.png")
hist(movies$runtimeMinutes[movies$runtimeMinutes <= 300], main="Movie Runtime",
xlab="Runtime (Minutes)", ylab="Frequency", xaxt="n")
axis(1, at=seq(0, 300, by=15), labels=FALSE)
axis(1, at=seq(0, 300, by=30), labels=seq(0, 300, by=30))
dev.off()

# Generate histogram of TV episode runtime
png("Figures/episode_runtime_hist.png")
hist(episodes$runtimeMinutes[episodes$runtimeMinutes <= 200], main="Episode Runtime",
xlab="Runtime (Minutes)", ylab="Frequency")
axis(1, at=seq(0, max(episodes$runtimeMinutes, na.rm=TRUE), by=10), labels=FALSE)
axis(1, at=seq(0, max(episodes$runtimeMinutes, na.rm=TRUE), by=20), 
labels=seq(0, max(episodes$runtimeMinutes, na.rm=TRUE), by=20))
dev.off()

# Generate histogram of popularity
png("Figures/popularity_hist.png")
hist(log10(merged$numVotes), main="Popularity (Number of Votes)",
xlab="Votes", ylab="log(Frequency)", col="skyblue", xaxt="n")
axis(1, at=0:6, labels=c("1", "10", "100", "1k", "10k", "100k", "1m"))
dev.off()

# Generate scatterplot of popularity and rating
png("Figures/popularity-rating_scatter.png")
smoothScatter(log10(merged$numVotes), merged$averageRating, main="Correlation Between Popularity and Rating",
xlab="log(Number of Votes)", ylab="Average Rating")
model <- lm(averageRating ~ log10(numVotes), data=merged)
abline(model, col="red", lwd=2)

cor_test <- cor.test(log10(merged$numVotes), merged$averageRating)
mtext(paste0("r = ", round(cor_test$estimate, 3), ", p = ", format.pval(cor_test$p.value, digits=3)), side=3, line=0, cex=0.9)
dev.off()

# Generate table of average rating by genre
genre_long <- rbind(
  data.frame(averageRating = merged$averageRating, Genre = merged$genre1),
  data.frame(averageRating = merged$averageRating, Genre = merged$genre2),
  data.frame(averageRating = merged$averageRating, Genre = merged$genre3)
)
genre_long <- genre_long[!is.na(genre_long$Genre),]

genre_ratings <- aggregate(averageRating ~ Genre, data=genre_long, FUN=mean)
genre_ratings <- genre_ratings[order(genre_ratings$averageRating, decreasing=TRUE),]
genre_ratings$averageRating <- round(genre_ratings$averageRating, 2)
colnames(genre_ratings) <- c("Genre", "Average Rating")
gtsave(gt(genre_ratings), "Figures/genre_rating_table.png")

# Generate line graph of ratings over time
year_ratings <- aggregate(averageRating ~ startYear, data=merged, FUN=mean)
year_ratings$averageRating <- round(year_ratings$averageRating, 2)
colnames(year_ratings) <- c("Year", "Average Rating")

png("Figures/year-rating_line.png")
plot(year_ratings$Year, year_ratings$`Average Rating`, main="Average Rating By Start Year",
xlab="Start Year", ylab="Average Rating", type="l")
dev.off()

# Generate line graph of movie length over time
year_runtime <- aggregate(runtimeMinutes ~ startYear, data=movies, FUN=mean)
year_runtime$runtimeMinutes <- round(year_runtime$runtimeMinutes, 3)
colnames(year_runtime) <- c("Year", "Runtime")

png("Figures/year-runtime_line.png")
plot(year_runtime$Year, year_runtime$Runtime, main="Average Runtime",
xlab="Start Year", ylab="Runtime (Minutes)", type="l")
dev.off()

# Zoomed line graph of ratings over time (1980+)
year_ratings_zoom <- subset(year_ratings, Year >= 1980)

png("Figures/year-rating_line_zoom.png")
plot(year_ratings_zoom$Year, year_ratings_zoom$`Average Rating`,
     main="Average Rating By Start Year (1980-Present)",
     xlab="Start Year", ylab="Average Rating", type="l")
dev.off()

# Zoomed line graph of movie runtime over time (1980+)
year_runtime_zoom <- subset(year_runtime, Year >= 1980)

png("Figures/year-runtime_line_zoom.png")
plot(year_runtime_zoom$Year, year_runtime_zoom$Runtime,
     main="Average Runtime (1980-Present)",
     xlab="Start Year", ylab="Runtime (Minutes)", type="l")
dev.off()

# SECTION 2

# Generate average improvement slope
episodes_full <- merge(episodes, title_episode, by="tconst")

# Filter to shows with at least 1000 total votes
show_votes <- aggregate(numVotes ~ parentTconst, data=episodes_full, FUN=sum)
valid_shows <- show_votes$parentTconst[show_votes$numVotes >= 1000]
episodes_full <- subset(episodes_full, parentTconst %in% valid_shows)

# Calculate improvement slope for each show
show_slopes <- by(episodes_full, episodes_full$parentTconst, function(show_data) {
  show_data <- show_data[order(show_data$seasonNumber, show_data$episodeNumber),]
  show_data$episode_index <- seq_len(nrow(show_data))
  if (nrow(show_data) < 8) return(NULL)
  slope <- coef(lm(averageRating ~ episode_index, data=show_data))[2]
  return(slope)
})

show_slopes <- show_slopes[!sapply(show_slopes, is.null)]

slopes_df <- data.frame(
  parentTconst = names(show_slopes),
  slope = unlist(show_slopes)
)

# Summarize
mean_slope <- mean(slopes_df$slope, na.rm=TRUE)
cat("Mean improvement slope across all shows:", round(mean_slope, 3), "\n")

# Visualize distribution of slopes
png("Figures/show_improvement_hist.png")
hist(slopes_df$slope, main="Distribution of Show Improvement Slopes",
     xlab="Slope (Rating Change Per Episode)", ylab="Frequency", col="skyblue")
abline(v=mean_slope, col="red", lwd=2)
dev.off()

# SECTION 3

# Create genre dummies
all_genres <- unique(na.omit(c(merged$genre1, merged$genre2, merged$genre3)))
all_genres <- gsub("-", "_", all_genres)  # replace hyphens with underscores

for (genre in all_genres) {
  clean_genre <- gsub("-", "_", genre)
  merged[[paste0("genre_", clean_genre)]] <- as.integer(
    (!is.na(merged$genre1) & gsub("-", "_", merged$genre1) == clean_genre) |
    (!is.na(merged$genre2) & gsub("-", "_", merged$genre2) == clean_genre) |
    (!is.na(merged$genre3) & gsub("-", "_", merged$genre3) == clean_genre)
  )
}

# Movie MLR
movies <- subset(merged, titleType == "movie")
movies <- subset(movies, as.numeric(as.character(startYear)) >= 1980)

movies$startYear <- as.factor(movies$startYear)

genre_cols <- paste(paste0("genre_", all_genres), collapse=" + ")

movie_formula <- as.formula(paste(
  "averageRating ~ log(numVotes) + log(runtimeMinutes) + isAdult + startYear +",
  genre_cols
))

movie_model <- lm(movie_formula, data=movies)
movie_robust <- coeftest(movie_model, vcov=vcovHC(movie_model, type="HC1"))

# TV Series MLR
series <- subset(merged, titleType == "tvSeries")
series <- subset(series, as.numeric(as.character(startYear)) >= 1980)
series$duration <- as.numeric(series$endYear) - as.numeric(series$startYear)
series$startYear <- as.factor(series$startYear)

series_formula <- as.formula(paste(
  "averageRating ~ log(numVotes) + log(runtimeMinutes) + isAdult + startYear + duration +",
  genre_cols
))

series_model <- lm(series_formula, data=series)
series_robust <- coeftest(series_model, vcov=vcovHC(series_model, type="HC1"))

# Save movie MLR results
movie_results <- as.data.frame(movie_robust[,])
movie_results <- round(movie_results, 3)
movie_results <- cbind(Variable = rownames(movie_results), movie_results)
colnames(movie_results) <- c("Variable", "Estimate", "Std. Error", "t Value", "p Value")
movie_results <- movie_results[!grepl("startYear", movie_results$Variable),]
gtsave(gt(movie_results), "Figures/movie_mlr.png")

# Save series MLR results
series_results <- as.data.frame(series_robust[,])
series_results <- round(series_results, 3)
series_results <- cbind(Variable = rownames(series_results), series_results)
colnames(series_results) <- c("Variable", "Estimate", "Std. Error", "t Value", "p Value")
series_results <- series_results[!grepl("startYear", series_results$Variable),]
gtsave(gt(series_results), "Figures/series_mlr.png")

# Run Ramsey reset test
resettest(movie_model,  power = 2:3, type = "fitted")
resettest(series_model, power = 2:3, type = "fitted")

# SECTION 4 - XGBoost

# ── Shared helper ──────────────────────────────────────────────────────────────

xgb_metrics <- function(actual, predicted) {
  mae   <- mean(abs(actual - predicted))
  mse   <- mean((actual - predicted)^2)
  rmse  <- sqrt(mse)
  rmsle <- sqrt(mean((log1p(predicted) - log1p(actual))^2))
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2    <- 1 - ss_res / ss_tot
  cat(sprintf("MAE:    %.4f\n", mae))
  cat(sprintf("MSE:    %.4f\n", mse))
  cat(sprintf("RMSE:   %.4f\n", rmse))
  cat(sprintf("RMSLE:  %.4f\n", rmsle))
  cat(sprintf("R²:     %.4f\n", r2))
}

# ── Feature columns (shared by both models) ────────────────────────────────────

genre_feature_cols <- paste0("genre_", all_genres)

# ── Movie XGBoost ──────────────────────────────────────────────────────────────

movies_xgb <- subset(merged, titleType == "movie")
movies_xgb <- subset(movies_xgb, as.numeric(movies_xgb$startYear) >= 1980)
movies_xgb <- movies_xgb[!is.na(movies_xgb$runtimeMinutes) &
!is.na(movies_xgb$numVotes) & !is.na(movies_xgb$averageRating),]

movies_xgb$startYear <- as.numeric(movies_xgb$startYear)

movie_features <- c("numVotes", "runtimeMinutes", "isAdult",
                    "startYear", genre_feature_cols)

movie_X <- as.matrix(movies_xgb[, movie_features])
movie_y <- movies_xgb$averageRating

# Train-test split (80/20)
set.seed(42)
movie_train_idx <- sample(nrow(movie_X), 0.8 * nrow(movie_X))

movie_train <- xgb.DMatrix(data  = movie_X[movie_train_idx, ],label = movie_y[movie_train_idx])
movie_test  <- xgb.DMatrix(data  = movie_X[-movie_train_idx, ],label = movie_y[-movie_train_idx])

# Fit — set eta here; all other hyperparameters are XGBoost defaults
movie_xgb_model <- xgb.train(
  params  = list(
    objective = "reg:squarederror",
    eta = 0.1          # <-- tune this
  ),
  data = movie_train,
  nrounds = 100,
  verbose = 0
)

# Predict and evaluate
movie_preds <- round(predict(movie_xgb_model, movie_test), 1)
cat("\n── Movie XGBoost Metrics ──\n")
xgb_metrics(movie_y[-movie_train_idx], movie_preds)

# ── TV Series XGBoost ──────────────────────────────────────────────────────────

series_xgb <- subset(merged, titleType == "tvSeries")
series_xgb <- subset(series_xgb, as.numeric(series_xgb$startYear) >= 1980)
series_xgb$duration <- as.numeric(series_xgb$endYear) - as.numeric(series_xgb$startYear)
series_xgb <- series_xgb[!is.na(series_xgb$runtimeMinutes) & !is.na(series_xgb$numVotes) &
!is.na(series_xgb$averageRating) & !is.na(series_xgb$duration), ]

series_xgb$startYear <- as.numeric(series_xgb$startYear)

series_features <- c("numVotes", "runtimeMinutes", "isAdult", "startYear", "duration", genre_feature_cols)

series_X <- as.matrix(series_xgb[, series_features])
series_y <- series_xgb$averageRating

# Train-test split (80/20)
set.seed(42)
series_train_idx <- sample(nrow(series_X), 0.8 * nrow(series_X))

series_train <- xgb.DMatrix(data  = series_X[series_train_idx, ],
label = series_y[series_train_idx])
series_test  <- xgb.DMatrix(data  = series_X[-series_train_idx, ],
label = series_y[-series_train_idx])

# Fit
series_xgb_model <- xgb.train(
  params  = list(
    objective = "reg:squarederror",
    eta       = 0.1), # tune this
  data    = series_train,
  nrounds = 100,
  verbose = 0,
)

# Predict and evaluate
series_preds <- round(predict(series_xgb_model, series_test), 1)
cat("\n── TV Series XGBoost Metrics ──\n")
xgb_metrics(series_y[-series_train_idx], series_preds)

# SECTION 4 - Optimal Feature Inference

# ── Shared setup ───────────────────────────────────────────────────────────────

movie_median_votes  <- median(movies_xgb$numVotes)
series_median_votes <- median(series_xgb$numVotes)

build_genre_dummies <- function(genre_subset) {
  dummies <- setNames(rep(0L, length(all_genres)), paste0("genre_", all_genres))
  dummies[paste0("genre_", genre_subset)] <- 1L
  dummies
}

# Extract observed genre combinations from raw data and convert to sorted sets,
# then deduplicate so (Action, Drama) and (Drama, Action) count as one combo
extract_genre_subsets <- function(data) {
  raw_combos <- unique(data.frame(
    g1 = data$genre1,
    g2 = data$genre2,
    g3 = data$genre3,
    stringsAsFactors = FALSE
  ))
  subsets <- lapply(1:nrow(raw_combos), function(i) {
    genres <- unlist(raw_combos[i, ])
    genres <- genres[!is.na(genres) & genres != "NA" & genres != "\\N" & genres != ""]
    genres <- gsub("-", "_", genres)
    sort(unique(genres))
  })
  # Remove empty subsets and deduplicate
  subsets <- subsets[sapply(subsets, length) > 0]
  unique(subsets)
}

# ── Movie grid ─────────────────────────────────────────────────────────────────

movie_runtimes    <- seq(60, 180, by = 15)
movie_genre_subsets <- extract_genre_subsets(movies_xgb)

movie_genre_subsets <- extract_genre_subsets(movies_xgb)

# Diagnostic - remove after fixing
for (i in seq_along(movie_genre_subsets)) {
  gs <- movie_genre_subsets[[i]]
  unmatched <- gs[!paste0("genre_", gs) %in% paste0("genre_", all_genres)]
  if (length(unmatched) > 0) {
    cat("Subset", i, "has unmatched genres:", unmatched, "\n")
  }
}

movie_grid <- do.call(rbind, lapply(movie_genre_subsets, function(gs) {
  genre_row <- build_genre_dummies(gs)
  do.call(rbind, lapply(movie_runtimes, function(rt) {
    do.call(rbind, lapply(c(0, 1), function(adult) {
      c(
        numVotes       = movie_median_votes,
        runtimeMinutes = rt,
        isAdult        = adult,
        startYear      = 2025,
        genre_row
      )
    }))
  }))
}))

movie_grid_labels <- do.call(rbind, lapply(movie_genre_subsets, function(gs) {
  do.call(rbind, lapply(movie_runtimes, function(rt) {
    do.call(rbind, lapply(c(0, 1), function(adult) {
      data.frame(
        genres  = paste(gs, collapse = ", "),
        runtime = rt,
        isAdult = adult
      )
    }))
  }))
}))

movie_grid         <- as.data.frame(movie_grid)
movie_grid_matrix  <- as.matrix(movie_grid[, movie_features])
movie_grid_dmatrix <- xgb.DMatrix(data = movie_grid_matrix)
movie_grid_preds   <- round(predict(movie_xgb_model, movie_grid_dmatrix), 1)

movie_grid_labels$predicted_rating <- movie_grid_preds
movie_top10 <- head(movie_grid_labels[order(-movie_grid_labels$predicted_rating), ], 10)
rownames(movie_top10) <- NULL
colnames(movie_top10) <- c("Genres", "Runtime (min)", "Is Adult", "Predicted Rating")

gtsave(gt(movie_top10) |>
         tab_header(title = "Top 10 Optimal Movie Feature Combinations"),
       "Figures/movie_optimal.png")

cat("\n── Top 10 Movie Combinations ──\n")
print(movie_top10)

# ── Series grid ────────────────────────────────────────────────────────────────

series_runtimes     <- seq(5, 60, by = 5)
series_genre_subsets <- extract_genre_subsets(series_xgb)

series_grid <- do.call(rbind, lapply(series_genre_subsets, function(gs) {
  genre_row <- build_genre_dummies(gs)
  do.call(rbind, lapply(series_runtimes, function(rt) {
    do.call(rbind, lapply(c(0, 1), function(adult) {
      c(
        numVotes       = series_median_votes,
        runtimeMinutes = rt,
        isAdult        = adult,
        startYear      = 2025,
        duration       = median(series_xgb$duration, na.rm = TRUE),
        genre_row
      )
    }))
  }))
}))

series_grid_labels <- do.call(rbind, lapply(series_genre_subsets, function(gs) {
  do.call(rbind, lapply(series_runtimes, function(rt) {
    do.call(rbind, lapply(c(0, 1), function(adult) {
      data.frame(
        genres  = paste(gs, collapse = ", "),
        runtime = rt,
        isAdult = adult
      )
    }))
  }))
}))

series_grid         <- as.data.frame(series_grid)
series_grid_matrix  <- as.matrix(series_grid[, series_features])
series_grid_dmatrix <- xgb.DMatrix(data = series_grid_matrix)
series_grid_preds   <- round(predict(series_xgb_model, series_grid_dmatrix), 1)

series_grid_labels$predicted_rating <- series_grid_preds
series_top10 <- head(series_grid_labels[order(-series_grid_labels$predicted_rating), ], 10)
rownames(series_top10) <- NULL
colnames(series_top10) <- c("Genres", "Runtime (min)", "Is Adult", "Predicted Rating")

gtsave(gt(series_top10) |>
tab_header(title = "Top 10 Optimal TV Series Feature Combinations"),"Figures/series_optimal.png")

cat("\n── Top 10 TV Series Combinations ──\n")
print(series_top10)
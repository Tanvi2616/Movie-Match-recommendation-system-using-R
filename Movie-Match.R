# Load all required libraries
library(jsonlite)       # For JSON parsing
library(SnowballC)      # For text stemming
library(text2vec)       # For text vectorization
library(tm)             # For text mining
library(magrittr)       # For piping operations
library(proxy)          # For similarity calculations
library(irlba)          # For PCA
library(cluster)        # For clustering algorithms
library(factoextra)     # For clustering visualization
library(dbscan)         # For DBSCAN clustering
library(clusterCrit)    # For clustering metrics
library(ggplot2)        # For data visualization
library(reshape2)       # For data reshaping

# --------------------------
# DATA LOADING AND CLEANING
# --------------------------

# Load the datasets
movies <- read.csv("C:/Users/Tanvi Yedvi/Desktop/R Programming Project/R project/movies_dataset.csv", stringsAsFactors = FALSE)
credits <- read.csv("C:/Users/Tanvi Yedvi/Desktop/R Programming Project/R project/credits_dataset.csv", stringsAsFactors = FALSE)

# View the first few rows
head(movies)
head(credits)

# Check for missing values
cat("Missing values in movies dataset:\n")
print(colSums(is.na(movies)))

cat("\nMissing values in credits dataset:\n")
print(colSums(is.na(credits)))

# Handle missing values by replacing with empty string
movies[is.na(movies)] <- ""
credits[is.na(credits)] <- ""

# Merge datasets based on the 'title' column
combined_data <- merge(movies, credits, by = "title")

# Select relevant columns for recommendation system
final_data <- combined_data[, c("movie_id", "title", "overview", "genres", "keywords", "cast", "crew")]

# View first few rows
head(final_data)

# --------------------------
# DATA PREPROCESSING
# --------------------------

# Function to extract names from JSON-like string
convert_json_column <- function(json_str) {
  if (json_str == "" || is.na(json_str)) return(character(0))
  parsed <- tryCatch(fromJSON(json_str), error = function(e) return(character(0)))
  if (!is.data.frame(parsed)) return(character(0))
  return(parsed$name)
}

# Process genres and keywords columns
final_data$genres <- lapply(final_data$genres, convert_json_column)
final_data$keywords <- lapply(final_data$keywords, convert_json_column)

# Function to extract top 3 cast members
extract_top_3_cast <- function(json_str) {
  if (json_str == "" || is.na(json_str)) return(character(0))
  parsed <- tryCatch(fromJSON(json_str), error = function(e) return(data.frame()))
  if (!is.data.frame(parsed) || !"name" %in% names(parsed)) return(character(0))
  return(head(parsed$name, 3))
}

final_data$cast <- lapply(final_data$cast, extract_top_3_cast)

# Function to extract director from crew
extract_director <- function(json_str) {
  if (json_str == "" || is.na(json_str)) return(NA)
  parsed <- tryCatch(fromJSON(json_str), error = function(e) return(data.frame()))
  if (!is.data.frame(parsed) || !"job" %in% names(parsed)) return(NA)
  director <- parsed$name[parsed$job == "Director"]
  if (length(director) == 0) return(NA)
  return(director[1])
}

final_data$crew <- sapply(final_data$crew, extract_director)

# Process overview text
split_overview <- function(text) {
  if (text == "" || is.na(text)) return(character(0))
  return(unlist(strsplit(text, "\\s+")))
}

final_data$overview <- lapply(final_data$overview, split_overview)

# Remove spaces from text features
remove_spaces <- function(x) {
  if (length(x) == 0) return(x)
  return(gsub(" ", "", x))
}

final_data$genres <- lapply(final_data$genres, function(x) sapply(x, remove_spaces))
final_data$keywords <- lapply(final_data$keywords, function(x) sapply(x, remove_spaces))
final_data$cast <- lapply(final_data$cast, function(x) sapply(x, remove_spaces))
final_data$crew <- sapply(final_data$crew, function(x) gsub(" ", "", x))

# Combine all features into tags
final_data$tags <- mapply(function(overview, genres, keywords, cast, crew) {
  paste(c(overview, genres, keywords, cast, crew), collapse = " ")
}, final_data$overview, final_data$genres, final_data$keywords, final_data$cast, final_data$crew)

# Create final dataframe
new_df <- final_data[, c("movie_id", "title", "tags")]
new_df$tags <- sapply(new_df$tags, function(x) if (length(x) == 0) "" else paste(x, collapse = " "))

# Convert to lowercase
new_df$tags <- tolower(new_df$tags)

# View the result
head(new_df)

# Stemming
new_df$tags <- sapply(new_df$tags, function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed <- wordStem(words, language = "en")
  paste(stemmed, collapse = " ")
})

# --------------------------
# TEXT VECTORIZATION
# --------------------------

# Create document-term matrix
tokens <- itoken(new_df$tags, tokenizer = word_tokenizer, progressbar = FALSE)
vocab <- create_vocabulary(tokens, stopwords = stopwords("en")) %>%
  prune_vocabulary(term_count_min = 1, vocab_term_max = 5000)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(tokens, vectorizer)
vectors <- as.matrix(dtm)

# Dimensionality reduction with PCA
scaled_vectors <- scale(vectors)
pca_result <- irlba(scaled_vectors, nv = 100)
reduced_vectors <- pca_result$u %*% diag(pca_result$d)

# --------------------------
# SIMILARITY CALCULATION
# --------------------------

# Compute cosine similarity matrix
similarity_matrix <- proxy::simil(reduced_vectors, method = "cosine")
similarity_matrix <- as.matrix(similarity_matrix)

# Recommendation function
recommend_movie <- function(movie_name, new_df, similarity_matrix) {
  movie_index <- which(new_df$title == movie_name)
  
  if (length(movie_index) == 0) {
    cat("Movie not found in the dataset.\n")
    return(NULL)
  }
  
  distances <- similarity_matrix[movie_index, ]
  top_indices <- order(distances, decreasing = TRUE)[2:6]
  
  cat("Top 5 movie recommendations for:", movie_name, "\n")
  for (i in top_indices) {
    cat("-", new_df$title[i], "\n")
  }
}

# --------------------------
# DATA ANALYSIS
# --------------------------

# Tag length analysis
new_df$tag_length <- sapply(strsplit(new_df$tags, " "), length)

# IQR filtering
Q1 <- quantile(new_df$tag_length, 0.25)
Q3 <- quantile(new_df$tag_length, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
cat("Lower Bound:", lower_bound, "Upper Bound:", upper_bound, "\n")

new_df_cleaned <- new_df[new_df$tag_length >= lower_bound & new_df$tag_length <= upper_bound, ]
cat("Number of rows before IQR filtering:", nrow(new_df), "\n")
cat("Number of rows after IQR filtering:", nrow(new_df_cleaned), "\n")

# Summary statistics
summary_stats <- summary(new_df_cleaned$tag_length)
mean_value <- mean(new_df_cleaned$tag_length)
median_value <- median(new_df_cleaned$tag_length)
mode_value <- as.numeric(names(sort(table(new_df_cleaned$tag_length), decreasing = TRUE)[1]))
variance_value <- var(new_df_cleaned$tag_length)
std_dev_value <- sd(new_df_cleaned$tag_length)

cat("\nMean of tag length:", mean_value, "\n")
cat("Median of tag length:", median_value, "\n")
cat("Mode of tag length:", mode_value, "\n")
cat("Variance of tag length:", variance_value, "\n")
cat("Standard Deviation of tag length:", std_dev_value, "\n")

# Most frequent movie titles
cat("\nMost Frequent Movie Titles:\n")
print(head(sort(table(new_df_cleaned$title), decreasing = TRUE), 10))

# --------------------------
# CLUSTERING ANALYSIS
# --------------------------

# K-Means Clustering
set.seed(42)
kmeans_model <- kmeans(reduced_vectors, centers = 5, nstart = 25)
kmeans_labels <- kmeans_model$cluster
cat("✅ K-Means training completed. Clusters formed:", length(unique(kmeans_labels)), "\n")

# Hierarchical Clustering
dist_matrix <- dist(reduced_vectors)
hc_model <- hclust(dist_matrix, method = "ward.D2")
hc_labels <- cutree(hc_model, k = 5)
cat("✅ Hierarchical Clustering training completed. Clusters formed:", length(unique(hc_labels)), "\n")

# DBSCAN Clustering
db_model <- dbscan(reduced_vectors, eps = 0.5, minPts = 5)
db_labels <- db_model$cluster
if (length(unique(db_labels[db_labels != 0])) > 1) {
  cat("✅ DBSCAN training completed. Clusters formed (excluding noise):", length(unique(db_labels[db_labels != 0])), "\n")
} else {
  cat("⚠️ DBSCAN did not form enough clusters (excluding noise).\n")
}


# Clustering Metrics
compute_metrics <- function(data, labels) {
  sil <- silhouette(labels, dist(data))
  sil_score <- mean(sil[, 3])
  
  dbi <- intCriteria(as.matrix(data), as.integer(labels), "Davies_Bouldin")
  db_index <- dbi$davies_bouldin
  
  return(c(Silhouette = sil_score, DBI = db_index))
}

kmeans_scores <- compute_metrics(reduced_vectors, kmeans_labels)
hc_scores <- compute_metrics(reduced_vectors, hc_labels)

if (length(unique(db_labels[db_labels != 0])) > 1) {
  dbscan_data <- reduced_vectors[db_labels != 0, ]
  dbscan_labels_clean <- db_labels[db_labels != 0]
  dbscan_scores <- compute_metrics(dbscan_data, dbscan_labels_clean)
} else {
  dbscan_scores <- c(Silhouette = NA, DBI = NA)
}

# Results dataframe
results <- data.frame(
  Model = c("K-Means", "Hierarchical", "DBSCAN"),
  Silhouette_Score = c(kmeans_scores["Silhouette"], hc_scores["Silhouette"], dbscan_scores["Silhouette"]),
  Davies_Bouldin_Index = c(kmeans_scores["DBI"], hc_scores["DBI"], dbscan_scores["DBI"])
)
print(results)
# --------------------------
# VISUALIZATIONS
# --------------------------

# Boxplot comparison
par(mfrow = c(1, 2))
boxplot(new_df$tag_length, main = "Before Removing Outliers", col = "skyblue", horizontal = TRUE)
boxplot(new_df_cleaned$tag_length, main = "After Removing Outliers", col = "lightgreen", horizontal = TRUE)
par(mfrow = c(1, 1))

# Histogram of tag length
ggplot(new_df, aes(x = tag_length)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of Number of Words in Tags") +
  xlab("Number of Words") +
  ylab("Frequency") +
  theme_minimal()

#Box Plot of Tag Length
ggplot(new_df, aes(x = tag_length)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Box Plot of Tag Length") +
  xlab("Tag Length (Number of Words)") +
  theme_minimal()

#Scatter Plot of Movie ID vs. Tag Length
ggplot(new_df, aes(x = movie_id, y = tag_length)) +
  geom_point(color = "blue", alpha = 0.6) +
  ggtitle("Scatter Plot of Movie ID vs. Tag Length") +
  xlab("Movie ID") +
  ylab("Tag Length") +
  theme_minimal()

# Clustering results visualization
results_melted <- melt(results, id.vars = "Model", variable.name = "Metric", value.name = "Score")
ggplot(results_melted, aes(x = Model, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5, position = position_dodge(0.9), size = 3.5, na.rm = TRUE) +
  labs(title = "Performance Comparison of Clustering Models",
       y = "Score", x = "Clustering Model") +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +
  theme_minimal()

# --------------------------
# SAVE RESULTS
# --------------------------

saveRDS(new_df, file = "movies.rds")
saveRDS(similarity_matrix, file = "similarity_matrix.rds")

# --------------------------
# USER INTERACTION
# --------------------------

# Get user input for movie recommendation
user_input <- readline(prompt = "Enter a movie title: ")
recommend_movie(user_input, new_df, similarity_matrix)
🎬 Movie-Match Recommendation System

A content-based movie recommendation system built in R using NLP techniques and clustering algorithms. The system analyzes movie metadata (overview, genres, cast, crew, and keywords) and suggests 5 similar movies based on cosine similarity. An interactive Shiny web app frontend allows users to search for a movie and instantly receive recommendations with visual similarity scores.

📊 Dataset

movies_dataset.csv and credits_dataset.csv from TMDb (via Kaggle)

🧠 Key Features

Text Preprocessing: Tokenization, lowercasing, stopword removal, stemming

Feature Vectorization: TF-IDF applied to tags field

Dimensionality Reduction: PCA reduced features from 5000 to 100 dimensions

Similarity Calculation: Cosine similarity between movie vectors

Clustering Algorithms:

K-Means (k = 5)

Hierarchical Clustering (Ward’s method)

DBSCAN

Evaluation Metrics:

Silhouette Score

Davies-Bouldin Index

Recommendation Function: Returns top 5 similar movies for any selected title

Shiny App UI: Clean, responsive interface with bar chart visuals for recommendations

🛠️ Tech Stack

Language: R Programming

Libraries: text2vec, ggplot2, cluster, factoextra, shiny, jsonlite, dplyr

Clustering: KMeans, Hierarchical, DBSCAN

Visualization: Scatter plots, dendrograms, bar charts

Frontend: R Shiny Web Application

🧾 Conclusion

This project successfully demonstrates the power of content-based filtering in recommending movies by analyzing metadata like genres, cast, crew, and keywords. With effective preprocessing, dimensionality reduction, and clustering, it delivers accurate and user-friendly recommendations through a Shiny web app.

Among all clustering techniques tested, Hierarchical Clustering emerged as the best-performing model with a high Silhouette Score (0.574) and a low Davies-Bouldin Index (0.551).

The system is modular, scalable, and lays the foundation for future enhancements like hybrid filtering and real-time personalization.

🔮 Future Scope

Integrate collaborative filtering for hybrid recommendation

Add user ratings, sentiment analysis, and feedback

Integrate real-time movie data from streaming APIs

Expand to mobile apps or browser extensions

Use deep learning and computer vision for richer content insights

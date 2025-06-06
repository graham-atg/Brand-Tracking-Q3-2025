library(wordcloud)
library(tm)
library(RColorBrewer)

# List of common stop words in English
stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves",
                "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their",
                "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was",
                "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and",
                "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between",
                "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off",
                "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any",
                "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so",
                "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now", "d", "ll", "m", "o", "re", "ve", "y",
                "ain", "aren", "couldn", "didn", "doesn", "hadn", "hasn", "haven", "isn", "ma", "mightn", "mustn", "needn", "shan",
                "shouldn", "wasn", "weren", "won", "wouldn")

generate_wordcloud <- function(dataframe, text_column, output_filename, color_palette = c("#002A4E", "#36749D", "#85714D", "#000000", "#004F51", "#95174C", "#313131")) {
  # Parameter validation
  if (is.null(dataframe) || is.null(text_column) || is.null(output_filename)) {
    stop("All parameters must be provided.")
  }
  
  if (!text_column %in% colnames(dataframe)) {
    stop("The specified text column does not exist in the dataframe.")
  }
  
  if (nrow(dataframe) == 0) {
    message("The dataframe is empty. No word cloud can be generated.")
    return(invisible())
  }
  
  tryCatch({
    # Create a Corpus from the text_column
    corpus <- Corpus(VectorSource(dataframe[[text_column]]))
    
    # Convert to lowercase and remove punctuation
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    
    # Remove common stop words
    corpus <- tm_map(corpus, removeWords, stop_words)
    
    # Create a Document-Term Matrix
    dtm <- DocumentTermMatrix(corpus)
    
    # Convert the Document-Term Matrix to a matrix
    word_freq_matrix <- as.matrix(dtm)
    
    # Calculate word frequencies
    word_freqs <- colSums(word_freq_matrix)
    
    # Create a data frame of word frequencies
    word_freq_df <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    # Filter out words that are two letters or shorter
    word_freq_df <- word_freq_df[nchar(as.character(word_freq_df$word)) > 2, ]
    
    # Set up the word cloud plot dimensions
    par(mar = rep(1, 4)) # Adjust the margins as needed
    
    # Create a word cloud with the specified color palette and white background
    word_cloud <- wordcloud(
      words = word_freq_df$word,
      freq = word_freq_df$freq,
      scale = c(3, 0.5),
      min.freq = 1,
      colors = color_palette,
      bg = "white"
    )
    
    # Save the word cloud as a PNG file
    png(filename = paste0(output_filename, ".png"), width = 800, height = 600, res = 100) # Adjust width, height, and res as needed
    print(word_cloud)
    dev.off() # Close the PNG device
    
    return(word_cloud)
  }, error = function(e) {
    cat("An error occurred: ", conditionMessage(e), "\n")
  })
}

generate_multiple_wordclouds <- function(dataframe) {
  for (column_name in names(dataframe)) {
    if (is.character(dataframe[[column_name]])) {
      output_filename <- paste0(column_name, "_wordcloud")
      generate_wordcloud(dataframe, column_name, output_filename)
    } else {
      message("Column '", column_name, "' is not a text column. Skipping this column.")
    }
  }
}

# Example usage
set.seed(123)
sample_data <- data.frame(
  question1 = c("This is a sample text for the word cloud.", "Another example text to be included."),
  question2 = c("Testing the word cloud function.", "Final text sample for the word cloud."),
  numeric_column = c(1, 2, 3, 4)
)

generate_multiple_wordclouds(sample_data)
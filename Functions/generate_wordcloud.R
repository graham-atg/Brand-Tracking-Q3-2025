
#The generate_wordcloud function takes a dataframe, a text column, and an output filename as inputs. 
#It creates a word cloud plot from the text data in the specified column using the wordcloud function
#from the wordcloud package. It calculates the word frequencies, 
#sets up the plot dimensions, and saves the resulting word cloud as a PNG file with a fixed dark blue color and white background

library(wordcloud)
library(tm)
library(RColorBrewer)

generate_wordcloud <- function(dataframe, text_column, output_filename) {
  # Create a Corpus from the text_column
  corpus <- Corpus(VectorSource(dataframe[[text_column]]))
  
  # Create a Document-Term Matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Convert the Document-Term Matrix to a matrix
  word_freq_matrix <- as.matrix(dtm)
  
  # Calculate word frequencies
  word_freqs <- colSums(word_freq_matrix)
  
  # Create a data frame of word frequencies
  word_freq_df <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Set up the word cloud plot dimensions
  par(mar = rep(1, 4))  # Adjust the margins as needed
  
  # Create a word cloud with a fixed dark blue color and white background
  wordcloud(
    words = word_freq_df$word,
    freq = word_freq_df$freq,
    scale = c(3, 0.5),
    min.freq = 1,
    colors = "darkblue",  # Fixed dark blue color for all words
    bg = "white"
  )
  
  # Save the word cloud as a PNG file
  png(filename = paste0(output_filename, ".png"), width = 800, height = 600, res = 100) # Adjust width, height, and res as needed
  print(wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, scale = c(3, 0.5), min.freq = 1, colors = "darkblue", bg = "white"))
  return(wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, scale = c(3, 0.5), min.freq = 1, colors = "darkblue", bg = "white"))
  dev.off() # Close the PNG device
}

# Example of how to call the function to save the word cloud plot
generate_wordcloud(BTQ3, "Q12_DEMO_COMBINE", "output_wordcloud")

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

generate_wordcloud <- function(dataframe, text_column, output_filename) {
  # Parameter validation
  if (!text_column %in% colnames(dataframe)) {
    stop("The specified text column does not exist in the dataframe.")
  }
  if (nrow(dataframe) == 0) {
    stop("The dataframe is empty.")
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
    par(mar = rep(1, 4))  # Adjust the margins as needed
    
    # Create a word cloud with the specified color palette and white background
    wordcloud(
      words = word_freq_df$word,
      freq = word_freq_df$freq,
      scale = c(3, 0.5),
      min.freq = 1,
      colors = c("#002A4E", "#36749D", "#85714D", "#000000", "#004F51", "#95174C", "#313131"),  # Specified color palette
      bg = "white"
    )
    
    # Save the word cloud as a PNG file
    png(filename = paste0(output_filename, ".png"), width = 800, height = 600, res = 100) # Adjust width, height, and res as needed
    print(wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, scale = c(3, 0.5), min.freq = 1, 
                    colors = c("#002A4E", "#36749D", "#85714D", "#000000", "#004F51", "#95174C", "#313131"), bg = "white"))
    dev.off() # Close the PNG device
  }, error = function(e) {
    cat("An error occurred: ", conditionMessage(e), "\n")
  })
}

# Example of how to call the function to save the word cloud plot
generate_wordcloud(BTQ3, "Q12_DEMO_COMBINE", "output_wordcloud")

# The word cloud plot is now saved as "output_wordcloud.png"

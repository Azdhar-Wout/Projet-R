##################################################
# PARTIE 1
clean_accentuation <- function(str) {
  new_str <- str
  new_str <- gsub("à", "a", new_str)
  new_str <- gsub("â", "a", new_str)
  new_str <- gsub("ä", "a", new_str)
  
  new_str <- gsub("é", "e", new_str)
  new_str <- gsub("è", "e", new_str)
  new_str <- gsub("ê", "e", new_str)
  new_str <- gsub("ë", "e", new_str)
  
  new_str <- gsub("ï", "i", new_str)
  new_str <- gsub("î", "i", new_str)
  new_str <- gsub("ì", "i", new_str)
  
  new_str <- gsub("ò", "o", new_str)
  new_str <- gsub("ô", "o", new_str)
  new_str <- gsub("ö", "o", new_str)
  
  new_str <- gsub("ù", "u", new_str)
  new_str <- gsub("û", "u", new_str)
  new_str <- gsub("ü", "u", new_str)
  
  return(new_str)
}


is_palindrome <- function(str, print=TRUE) {
  lower_str <- tolower(gsub("[[:space:][:punct:]]", "", str))
  cleaned_str <- clean_accentuation(lower_str)
  str_split <- strsplit(cleaned_str, "")[[1]]
  taille <- length(str_split)
  milieu <- taille / 2
  
  palindrome <- TRUE
  for(i in 1:milieu){
    if(str_split[i] != str_split[taille -i +1]) {
      palindrome <- FALSE
      break
    }
  }
  
  if(palindrome) {
    if(print){
      print(paste0("'", str, "' est un palindrome"))
    }
    return(TRUE)
    
  } else {
    if(print){
      print(paste0("'", str, "' n'est pas un palindrome"))
    }
    return(FALSE)
  }
}
##################################################



##################################################
# PARTIE 2
word_list <- list("radar", 
                  "bonne année", 
                  "sept", 
                  "kayak", 
                  "la mariée ira mal", 
                  "statistiques", 
                  "engage le jeu que je le gagne", 
                  "esope reste ici et se repose")
for(word in word_list){
  is_palindrome(word)
}
##################################################



##################################################
# PARTIE 3
generate_random_word <- function(length) {
  letters <- c(letters)
  word <- paste0(sample(letters, length, replace = TRUE), collapse = "")
  return(word)
}


generate_dictionaire <- function() {
  dictionaire <- list()
  id <- 0
  for(length in 2:9) {
    for(nb_mot in 1:1000){
      word <- generate_random_word(length)
      id <- id + 1
      dictionaire[[id]] <- word
    }
  }
      
  return(unlist(dictionaire))
}
##################################################



##################################################
# PARTIE 4
filter_palindromes <- function(dico) {
  list_palindromes <- list()
  
  for(word in dico) {
    if(is_palindrome(word, print=FALSE) == TRUE){
      id <- length(list_palindromes) + 1
      list_palindromes[[id]] <- word
    }
  }
  
  return(unlist(list_palindromes))
}


dico <- generate_dictionaire()
palind_list <- filter_palindromes(dico)
print(palind_list)


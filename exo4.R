##################################################
# PARTIE 1
is_palindrome <- function(str) {
  cleaned_str <- tolower(gsub("[[:space:][:punct:]]", "", str))
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
    print(paste(str, "est un palindrome"))
    return(TRUE)
  } else {
    print(paste(str, "n'est pas un palindrome"))
    return(FALSE)
  }
}
##################################################



##################################################
# PARTIE 2
is_palindrome("radar")
is_palindrome("bonne annee")
is_palindrome("sept")
is_palindrome("kayak")
is_palindrome("la mariee ira mal")
is_palindrome("statistiques")
is_palindrome("engage le jeu que je le gagne")
is_palindrome("esope reste ici et se repose")
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
  for(length in 1:8) {
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
    if(is_palindrome(word) == TRUE){
      id <- length(list_palindromes) + 1
      list_palindromes[[id]] <- word
    }
  }
  
  return(unlist(list_palindromes))
}


dico <- generate_dictionaire()
palind_list <- filter_palindromes(dico)
print(palind_list)


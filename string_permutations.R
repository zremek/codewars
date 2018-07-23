containAllRots <- function(s, arr) {
  if (nchar(s) == 0){
    return(TRUE)
  } else {
    
  ## gather info about the first string
  chars <- el(strsplit(s, ""))
  length <- length(chars)
  vec <- seq_len(length)
  
  ## create a matrix of possible permutations
  permutations <- data.frame(matrix(NA, nrow = length, ncol = length + 1))
  names(permutations) <- c("id", paste0("index", vec))
  
  permutations$id <- vec
  
  ## calculate the offset indices
  for (r in vec)
    permutations[r, vec + 1] <- (vec + r - 1) %% (length)
  
  ## a %% a = 0 so reset this to a
  permutations[permutations == 0] <- length
  
  ## change from indices to characters
  permutations[ , vec + 1] <- sapply(vec, function(x) chars[unlist(permutations[x, vec + 1])])
  
  ## paste the characters back into strings
  permutations$string <- sapply(vec, function(x) paste0(permutations[x , vec + 1], collapse = ''))
  
  ## for each permutation of s in arr, return TRUE
  permut_count <- (arr %in% permutations$string)
  
  ### check if arr contains all possible permutations
  return(length(permut_count[permut_count == TRUE]) == length(permutations$string))
  
  }
}

containAllRots("bsjq", c("bsjq", "qbsj", "sjqb", "twZNsslC", "jqbs")) == TRUE

containAllRots("", c("TzYxlgfnhf", "yqVAuoLjMLy", "BhRXjYA", "YABhRXj", "hRXjYAB", "jYABhRX", "XjYABhR", "ABhRXjY"))

containAllRots("", "") == TRUE

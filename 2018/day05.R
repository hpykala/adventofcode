input <- readLines('input05.txt')

react <- function(s) {
  v <- strsplit(s,'')[[1]]
  i <- 1
  while(TRUE) {
    if(!v[i]==v[i+1] & toupper(v[i])==toupper(v[i+1])) {
      v <- v[-c(i,i+1)]
      i <- max(i-1, 1)
    } else {
      i <- i+1
    }
    if(i == length(v)) {
      break
    }
  }
  return(paste0(v, collapse=''))
}
react(input)

#part2
lengths <- sapply(letters, function(letter){
  nchar(react(gsub(letter, '', input, ignore.case = TRUE)))
})
min(lengths)

#Regex based approach, why does this not work?
#\u and \l work very limitedly in R
react2 <-function(s) {
  #find indices off all occurences of two same characters regardless of case
  inds <- gregexpr('([a-z])\\1', tolower(s))
  #get pairs of letters from the original string by index, get rid of duplicates
  pairs <- unique(sapply(inds[[1]], function(i) {substr(s, i, i+1)}))
  #pick only pairs that have alternating case
  remove <- grep('^[a-z]?[A-Z][a-z]?$', pairs, value = TRUE)
  if(length(remove) == 0){
    #return if nothing to remove
    return(s)
  }
  #create a regex to remove any occurence of the found alternating case pairs
  #and remove them all
  s <- gsub(paste0(remove, collapse = '|'), '', s)
  #recursion
  return(react2(s))
}

#create vector of all possible pairs of same character with alternating case
allpairs <-mapply(function(a,b) {c(paste0(a, b), paste0(b,a))}, letters,LETTERS)
dim(allpairs) <- NULL

#brute force substitution of any occurences with recursion
react3 <- function(s)   {
  s2 <- gsub(paste0(allpairs, collapse = '|'), '', s)
  if(s!=s2) {
    return(react3(s2))
  } else {
    return(s2)
  }
}
  
#at least with my input react2 gives sligthly wrong answer, but react and react3 work fine
s2 <- react2(input)
s3 <- react3(input)
nchar(s)
nchar(s2)
nchar(s3)

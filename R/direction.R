direction <- function(vector){
  prime_numbers <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
  a <- abs(vector[1])
  b <- abs(vector[2])
  if (a != 0  | b != 0){
    if ((a != 0)  & (b != 0)){
      while( (a<1 | b<1) ){
        a <- a*10
        b <- b*10
      }
      for (i in prime_numbers){
        while(a%%i == 0 & b%%i == 0 & (a != i | b != i)){
          a <- a/i
          b <- b/i
        }
      }
    }
    if (a == 0 | b == 0){
      a <- 0
      b <- 1
    }
    if (a == b){
      a <- 1
      b <- 1
    }
  }

  direction <- c(a,b)
  return(direction)
}

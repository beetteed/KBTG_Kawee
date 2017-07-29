imSensitivity <- function(ref,pred)
{
  library(SDMTools)
  mat = confusion.matrix(ref,pred)
  acc = accuracy(ref,pred)
  return(acc[4])
}

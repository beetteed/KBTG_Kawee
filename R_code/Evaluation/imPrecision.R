imPrecision <- function(ref,pred)
{
  library(SDMTools)
  mat = confusion.matrix(ref,pred)
  TP = mat[4]
  FP = mat[2]
  pre = TP/(TP+FP)
  return(pre)
}
#' Simple function to recode alphanumeric data
#'
#' @param data a vector of data to be recoded
#'
#' @return a vector of data recoded from 1 to the length of the data
#'
#' @examples
#' y<-c("2","4","a7","b854fg","34")
#' simpleRecode(y)


simpleRecode<-function(data){
  codes<-1:length(data)
  mapa<-data.frame("Original_Codes"=data,"Recodes"=codes)
}

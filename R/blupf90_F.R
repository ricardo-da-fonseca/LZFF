#' Function for phenotypic data file formatting to BLUPF90 software
#'
#' @param local path of the data file
#' @param h logical value indicating the presence of header
#' @param s field separator used in data file
#' @param d character used as decimal separator
#' @param md value used to indicate missing values in data file
#' @param omd missing data value to be written in the output file
#'
#' @return a data file in the appropriate format to run the software
#' @export
#'
#' @examples
#'

blupf90_F<-function(local,h=TRUE,s=" ",d=",",md="",of="blupF90_data.csv",omd=0){
  tipo<-stringr::str_extract(local,"(\\w+)$")
  print(tipo)
  if(tipo=="csv"){
    dados<-utils::read.csv(local,header=h,sep=s,dec=d,na.strings = md)
  } else{
    if(tipo=="xls" || tipo=="xlsx"){
      dados<-readxl::read_excel(local,na = md)
    } else{
      if(tipo=="txt"){
        dados<-utils::read.table(local,header=h,sep=s,dec=d,na.strings = md)
      } else{
        stop("No file extension. Please, provide a file name with extension.")
      }
    }
  }
  #checking if all values are numeric
  isnum<-sapply(dados)
  for(i in 1:length(isnum)){
    if(!isnum[i]){
      print("The data file has non-numerical values, which must be recoded.\n
            I will do a simple recode for you but the renumF90 program should be used anyway.")
      simpleRecode
    }
      }
}

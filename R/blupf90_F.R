#' Function for phenotypic data file formatting to BLUPF90 software
#'
#' @param local path of the data file
#' @param h logical value indicating the presence of header
#' @param s field separator used in data file
#' @param d character used as decimal separator
#' @param md value used to indicate missing values in data file
#' @param of name of the output file
#' @param omd missing data value to be written in the output file
#'
#' @return a data file in the appropriate format to run the software
#' @export
#'

blupf90_F<-function(local,h=TRUE,s=" ",d=",",md=c(""," ","NA"),of="blupF90_data",omd=0){
  ok_of<-if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }
  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(tipo=="csv"){
    dados<-utils::read.csv(local,header=h,sep=s,dec=d,strip.white=FALSE,na.strings = md)
  } else{
    if(tipo=="xls" || tipo=="xlsx"){
      dados<-as.data.frame(readxl::read_excel(local,na = md))
    } else{
      if(tipo=="txt"){
        dados<-utils::read.table(local,header=h,sep=s,dec=d,strip.white=FALSE,na.strings = md)
      } else{
        stop("Extension unknown or absent. Please, provide a file name with an supported extension (csv, xls, xlsx or txt.")
      }
    }
  }
  #checking if all values are numeric
  isnum<-sapply(dados,is.numeric)
  mapList<-list()
  for(i in 1:length(isnum)){
    if(!isnum[i]){
      print("The data file has non-numerical values, which must be recoded. I will do a simple recode for you but the renumF90 program should be used anyway.")
      mapList<-append(mapList,list(simpleRecode(dados[,i])))
      dados[,i]<-mapList[[i]]$Recodes
    }
  }
  #Replacing NAs
  vnas<-sapply(dados,is.na)
  for(i in 1:ncol(vnas)){
    if(sum(vnas[,i]!=0)){
      for(j in 1:nrow(vnas)){
        if(vnas[j,i]==TRUE){
          dados[j,i]<-omd
        }
      }
    }
  }
  # Writing the formated data file
  utils::write.table(dados,of,quote=FALSE,sep=" ",row.names=FALSE)
}

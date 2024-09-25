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

blupf90_F<-function(local,h=TRUE,s=" ",d=",",md="",of="blupF90_data",omd=0){
  ok_of<-if(stringr::str_extract(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }
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
        stop("Extension unknown or absent. Please, provide a file name with an supported extension (csv, xls, xlsx or txt.")
      }
    }
  }
  #checking if all values are numeric
  isnum<-sapply(dados,is.numeric)
  mapList<-list()
  for(i in 1:length(isnum)){
    if(!isnum[i]){
      print("The data file has non-numerical values, which must be recoded.\n
            I will do a simple recode for you but the renumF90 program should be used anyway.")
      maplist<-append(maplist,simpleRecode(dados[i]))
      dados[i]<-maplist[[i]]$Recodes
    }
  }
  #Replacing NAs
  vnas<-sapply(dados,is.na)
  for(i in 1:length(vnas)){
    if(vnas[i]==TRUE){
      is.na(dados[i])<-omd
    }
  }
  # Writing the formated data file
  of<-paste(of,".txt",sep="")
  utils::write.table(dados,of,quote=FALSE,sep=" ")
}

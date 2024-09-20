#' Função para formatação do arquivo de dados fenotípicos no software BLUPF90
#'
#' @param h valor lógico indicando a presença de cabeçalho
#' @param s separador de campo utilizado no arquivo de dados
#' @param d caractere utilizado como separador decimal
#' @param md valor utilizado para indicar os valores perdidos no arquivo de dados
#' @param local caminho do arquivo
#'
#' @return um arquivo de dados no formato adequado para rodar o software
#' @export
#'
#' @examples
#' y<-data.frame(id=1:5,dados=rnorm(5,10,3))
#' blupf90_F(y)

blupf90_F<-function(local,h=TRUE,s=" ",d=",",md=""){
  tipo<-stringr::str_extract(local,"(\\w+)$")
  print(tipo)
  if(tipo=="csv"){
    dados<-read.csv(local,header=h,sep=s,dec=d,na.strings = md)
  } else{
    if(tipo=="xls" || tipo=="xlsx"){
      dados<-readxl::read_excel(local,na = md)
    } else{
      if(tipo=="txt"){
        dados<-read.table(local,header=h,sep=s,dec=d,na.strings = md)
      } else{
        stop("Não detectei a extensão do arquivo. Por favor, forneça um arquivo com nome e extensão.")
      }
    }
  }
  dados
}

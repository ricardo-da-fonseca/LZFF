#' Função para formatação do arquivo de dados fenotípicos no software BLUPF90
#'
#' @param y1 caminho do arquivo de dados a ser formatado pela função
#' @param y2 data frame com os dados a serem formatados pela função
#'
#' @return um arquivo de dados no formato adequado para rodar o software
#' @export
#'
#' @examples
#' y<-data.frame(id=1:5,dados=rnorm(5,10,3))
#' blupf90_F(y)

blupf90_F<-function(y1=NULL,y2=NULL){
  if(is.null(y1)){
    print("Codificar a parte do arquivo aqui.")
    print(y1)
  } else{
    print("Codificar a rotina o data.frame aqui.")
    print(y2)
  }
}

#' Data Formatting
#'
#' @description
#' Function for formatting phenotypic data following the rules of the BLUPF90 software.
#'
#' @param dataList data frame received from rrcData function with unformatted data.
#' @param omd missing data value to be written in the output file.
#' @param of output file name.
#' @param EoL end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n".
#'
#' @return a formatted file.
#' @examples
#' #creating a data
#' d<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5), cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30", "2017-06-14", "2016-07-01"), format = "%Y-%m-%d"),trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#' p<- data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5))
#' c<-sort(unique(c(paste0("a", 1:10), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:10))), decreasing = TRUE)
#' m<-data.frame(code = c, recode = 1:23)
#'
#' #creating the data and pedigree list
#' plist<-list(map=m,ped=p,data=d)
#'
#' #formatA data
#' formatA(dataList= plist, omd = "0", of = "formatted_file", EoL = "\n")
#'
#' unlink("formatted_file")
#'
#'
#' @export


formatA<-function(dataList, omd = "0", of = "formatted_file", EoL = "\n"){
  if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }

  udata<-dataList$data


  utils::write.table(udata, file = of, quote = FALSE, sep = " ",
                     row.names = FALSE, col.names = FALSE, eol = EoL, na = omd)
}

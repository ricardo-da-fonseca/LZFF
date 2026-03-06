#' Format Pedigree Files
#'
#' @description
#' It creates a formatted pedigree file based on the software Wombat's requirements for genetic evaluations.
#'
#' @param dataList list containing recoded pedigree and data.
#' @param of output file's name for pedigree file
#' @param mp code for missing parents
#' @param s field/column separator
#' @param EoL end of line symbol
#' @param map logical value indicating if the code map should be printed
#' @param mapof map output file's name
#'
#' @returns a formatted pedigree file.
#' @examples
#' #creating a data
#' d<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5), cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30", "2017-06-14", "2016-07-01"), format = "%Y-%m-%d"),trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#' p<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5))
#' c<-sort(unique(c(paste0("a", 1:10), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:10))), decreasing = TRUE)
#' m<-data.frame(code = c, recode = 1:23)
#'
#' #creating the data and pedigree list
#' plist<-list(map=m,ped=p,data=d)
#'
#' #formatPed data
#' formatPed(dataList = plist, of = "pedigree.txt", mp = 0, s = " ", EoL = "\n",map = FALSE, mapof = "map.txt")
#'
#' unlink("pedigree.txt")
#'
#'@export

formatPed<-function(dataList, of = "pedigree.txt", mp = 0, s = " ", EoL = "\n",
                    map = FALSE, mapof = "map.txt"){

  #Replacing absent parents in the pedigree object
  dataList$ped<-replace(dataList$ped, list = is.na(dataList$ped), values = mp)

  #Checking if the pedigree data is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(dataList$ped, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  if(map){
    utils::write.table(dataList$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    utils::write.table(dataList$map, mapof, sep = s, eol =EoL,
                       row.names = FALSE, col.names = TRUE, quote = FALSE)
  }else{
    utils::write.table(dataList$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}

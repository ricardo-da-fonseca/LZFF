#' Read, recode and check Data and Pedigree
#'
#' @description
#' This is a wrapper function for reading, recoding and performing some checks on data and pedigree file.
#'
#' @param dataObj data frame with data after a consistency analysis
#' @param pedObj object with pedigree data
#' @param localData data file path
#' @param localPed data file path
#' @param colsTraits identification of columns related to traits
#' @param colsDates identification of columns related to Dates
#' @param colsPedData.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param ped.isd vector of columns number for individual, sire and dam in pedigree object/file
#' @param sData field/column separator in data file
#' @param dData decimal point used in data file
#' @param hData logical value indicating presence of header in data object/file
#' @param sPed field/column separator in pedigree file
#' @param hPed logical value indicating presence of header in pedigree file
#' @param md missing data indicator
#'
#' @returns a list with the following components: recode map, recoded and checked data ane recoded and checked pedigree
#' @export
#'
#' @examples
#' #Creating a pedigree data
#' x<-data.frame(id = c("id2", "s3", "id4", "id5", "id6"),sire = c("s3", NA,"s4", "s5", "s6"),dam  = c(NA, NA, "d1", "d2","d3"))
#'
#' #Creating data
#' y<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5), cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30", "2017-06-14", "2016-07-01"), format = "%Y-%m-%d"),trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#'
#' #formatting the data and pedigree
#' rrcDataPed(dataObj = y, pedObj = x, localData = NULL,localPed = NULL, colsTraits =c(6,7), colsDates = NULL,colsPedData.isd = 1:3, ped.isd = 1:3, sData = " ",dData = ".", hData = FALSE, sPed = " ", hPed = FALSE,md = c(""," ","NA"))
#'
rrcDataPed<-function(dataObj = NULL, pedObj = NULL, localData = NULL,
                     localPed = NULL, colsTraits = NULL, colsDates = NULL,
                     colsPedData.isd = 1:3, ped.isd = 1:3, sData = " ",
                     dData = ".", hData = FALSE, sPed = " ", hPed = FALSE,
                     md = c(""," ","NA")){

  udata<-rrcData(datObj = dataObj, colsPdg = colsPedData.isd, colsTrts = colsTraits,
                 colsDts = colsDates, local = localData, s = sData, d = dData,
                 h = hData, missData = md)

  fDataPed<-rrcPed(pedigreeObj = pedObj, isd = ped.isd, udata,
                   colsPdgDat.isd = colsPedData.isd, local = localPed, s = sPed,
                   h = hPed, missData = md)

  fDataPed
}

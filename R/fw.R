#' Title Read, recod, check and format pedigree and data.
#'
#' @description
#' This wrapper function reads, recodes, and performs checks on the data and pedigree files. In the end, it creates formatted data and pedigree files according to the requirements of the Wombat software for genetic evaluation.
#'
#' @param dObj data frame with data after a consistency analysis
#' @param pObj object with pedigree data
#' @param cTraits identification of columns related to traits
#' @param cPedDat.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param cDates columns with dates
#' @param pdg.isd vector of columns number for individual, sire and dam in pedigree object/file
#' @param dataFile data file path
#' @param pedFile data file path
#' @param sDat field/column separator in data file
#' @param dDat decimal point used in data file
#' @param hDat logical value indicating presence of header in data object/file
#' @param sPdg field/column separator in pedigree file
#' @param hPdg logical value indicating presence of header in pedigree file
#' @param missingData missing data indicator
#' @param dof data output file's name
#' @param omdat missing data value to be written in the output file
#' @param width vector specifying the widths of columns in the formatted file
#' @param endOfLine end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n"
#' @param pof output file's name for pedigree file
#' @param mparents code for missing parents
#' @param sep field/column separator
#' @param printMap logical value indicating if the code map should be printed
#' @param mof map output file's name
#'
#' @returns a list with the following components: recode map, recoded and checked data ane recoded and checked pedigree; formatted data and pedigree files.
#' @export
#'
#' @examples
#' # Creating data
#' data<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),
#' dam = paste0("d", 1:5), cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),
#' bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30", "2017-06-14", "2016-07-01"),
#' format = "%Y-%m-%d"), trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#' data
#'
#' # Creating pedigree data
#' pedigree<-data[,1:3]
#' pedigree
#'
#' #Creating the data file and pedigree file to be processed by the Wombat software
#' fw(dObj = data, pObj = pedigree, cTraits = 6:7, cDates = 5, cPedDat.isd = 1:3,
#' dof = "formatted_data.txt", pof = "ped.txt")
#' unlink("formatted_data.txt")
#' unlink("ped.txt")
#'
fw<-function(dObj = NULL, pObj = NULL, cTraits = NULL, cPedDat.isd = NULL,
             cDates = NULL, pdg.isd = c(1, 2, 3), dataFile = NULL,
             pedFile = NULL, sDat = " ", dDat = ".", hDat = FALSE,
             sPdg = " ", hPdg = FALSE, missingData = c(""," ","NA"),
             dof = "formatB_data", omdat = "-99999", width = NULL,
             endOfLine = "\n", pof = "pedigree.txt", mparents = 0,
             sep = " ", printMap = FALSE, mof = "map.txt"){

  listDataPed<-rrcDataPed(dataObj = dObj, pedObj = pObj, colsTraits = cTraits, colsDates = cDates,
                          colsPedData.isd = cPedDat.isd, ped.isd = pdg.isd,
                          localData = dataFile, localPed = pedFile, sData = sDat,
                          dData = dDat, hData = hDat, sPed = sPdg, hPed = hPdg,
                          md = missingData)

  formatB(dataList = listDataPed, of = dof, omd = omdat, traits = cTraits,
          widths = width, EoL = endOfLine)

  formatPed(dataList = listDataPed, of = pof, mp = mparents, s = sep, EoL = endOfLine,
            map = printMap, mapof = mof)

  listDataPed
}

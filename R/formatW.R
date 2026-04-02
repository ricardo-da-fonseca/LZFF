#' Formatting Data
#'
#' @description
#' It creates a formatted data file based on the software Wombat's requirements for genetic evaluations.
#'
#' @param dataList list containing recoded pedigree and data.
#' @param of output file name.
#' @param omd missing data value to be written in the output file.
#' @param traits vector indicating traits columns. Repeated traits must be indicated first.
#' @param widths vector specifying the widths of columns in the formatted file.
#' @param EoL end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n".
#'
#'
#' @returns a formatted data file.
#'
#' @export
#'
#' @examples
#' #creating a data
#' d<-data.frame(id = paste0("i", 1:5),
#' sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),
#' dam = paste0("d", 1:5),
#' cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),
#' bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30",
#' "2017-06-14", "2016-07-01"), format = "%Y-%m-%d"),
#' trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#'
#' p<- data.frame(id = paste0("i", 1:5),
#' sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5))
#'
#' c<-sort(unique(c(paste0("a", 1:10),
#' sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),
#' dam = paste0("d", 1:10))), decreasing = TRUE)
#'
#' m<-data.frame(code = c, recode = 1:23)
#'
#' #creating the data and pedigree list
#' plist<-list(map=m,ped=p,data=d)
#'
#' #formatW data
#' formatW(dataList = plist , of = "formatW_data", omd = "-99999",
#' traits = c(6,7),widths = NULL, EoL = "\n")
#'
#' unlink("formatW_data")
#'
formatW<-function(dataList, of = "formatW_data", omd = "-99999", traits = NULL,
                  widths = NULL, EoL = "\n"){

  udata<-dataList$data

  if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }

  #Formatting columns of data
  #Effects columns
  d1<-udata[, -traits]
  i<-2
  while(i <= length(traits)){
    d1<-rbind(d1,udata[, -traits])
    i <- i + 1
  }
  #Traits columns
  d2<-unlist(lapply(udata[, traits], c))

  #Adding trait number column
  d3<-rep(1:length(traits), each = length(d2)/length(traits))

  udata<-data.frame(d3,d1,d2)
  #udata<-udata[!is.na(udata$d2),]

  #Ordering
  fdata<-udata[order(udata[, 2], udata[, 1], na.last = FALSE), ]

  #Ensuring that output file's name is no longer than 30 characters
  length_of<-nchar(of)
  if(length_of > 30){
    of<-stringr::str_sub(of,-30)
  }

  fnames<-paste0("x",1:length(fdata))
  colnames(fdata)<-fnames
  rownames(fdata)<-NULL

  #Writing data with fixed columns format
  gdata::write.fwf(fdata, file = of, width = widths, sep = " ", na = omd,
                   rownames = FALSE, colnames = FALSE, eol = EoL, scientific = FALSE)
}

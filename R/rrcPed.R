#' Read, Recod and check Pedigree Data
#'
#' @description
#' This is a simple function for reading, recoding, and performing some checks on a data file. This function does not recode pedigree data.
#'
#' @param pedigreeObj object with pedigree data
#' @param isd vector of columns number for individual, sire and dam
#' @param udata unformatted data object
#' @param colsPdgDat.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param local data file path
#' @param s field/column separator
#' @param h logical value indicating presence of header in pedigree file
#' @param missData missing data indicator
#'
#'
#' @returns a list with three components: 1. a data frame with code map; 2. a data
#' frame with the formatted pedigree data; 3. a data frame with pedigree columns
#' in data recoded.
#' @export
#' @examples
#' #Creating pedigree data
#' x<- data.frame(id   = c("id2", "s3", "id4", "id5", "id6"),sire = c("s3",      NA,      "s4", "s5", "s6"),dam  = c(NA,      NA,      "d1",  "d2",  "d3"))
#'
#' #Creating data
#' y<-data.frame(id = paste0("i", 1:5), sire = paste0("s", c(rep(1, 2), rep(2, 2), 3)),dam = paste0("d", 1:5), cg = gl(n = 2, k = 2, length = 5, labels = c("gc1", "gc2")),bwd = as.Date(c("2014-10-02", "2014-02-15", "2017-06-30", "2017-06-14", "2016-07-01"), format = "%Y-%m-%d"),trt1 = rnorm(5, 2, 2), trt2 = rnorm(5, 10, 3))
#'
#' #Testing and recoding pedigree data
#' rrcPed(pedigreeObj = x, isd = c(1, 2, 3),udata = y, colsPdgDat.isd = c(1, 2, 3),local = NULL, s = " ", h = FALSE, missData = c(""," ","NA"))
#'

rrcPed<-function(pedigreeObj = NULL, isd = c(1, 2, 3), udata, colsPdgDat.isd = c(1, 2, 3),
                 local = NULL, s = " ", h = FALSE, missData = c(""," ","NA")){

  #Validation
  argTest<-as.character(sum(!is.null(local), !is.null(pedigreeObj)))
  switch(argTest,
         '0' = stop("You must provide EXACTLY ONE of the following arguments:\n",
                    "-'local': path to the file (character)\n",
                    "-'pedigreeObj': R object (data.frame)\n",
                    "Both cannot be NULL at the same time", call. = FALSE),

         '2' = stop("You must provide ONLY ONE of the following arguments:\n",
                    "-'local': path to the file (character)\n",
                    "-'pedigreeObj': R object (data.frame)\n",
                    "Both cannot be provided simultaneously", call. = FALSE))

  if(is.null(local) == FALSE){
    #Data reading
    tipo<-stringr::str_extract(local,"(\\w+)$")
    if(tipo == basename(local)){
      tipo<-"txt"
    }
    if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
      tipo<-"gsheet"
    }
    switch(tipo,
           csv = pedData<-utils::read.csv(local, header = h, sep = s,
                                          strip.white = FALSE, fill = TRUE,
                                          na.strings = missData),

           xls = pedData<-as.data.frame(readxl::read_excel(local, na = missData, col_names = h)),

           xlsx = pedData<-as.data.frame(readxl::read_excel(local, na = missData, col_names = h)),

           ods = pedData<-as.data.frame(readODS::read_ods(local, na = missData, col_names = h)),

           gsheet = {googlesheets4::gs4_auth()
             pedData<-as.data.frame(googlesheets4::read_sheet(local, na = missData),
                                    col_names = h)},

           txt = pedData<-utils::read.table(local, header = h, sep = s,
                                            strip.white = FALSE, fill = TRUE,
                                            na.strings = missData),

           print("I did not detect the type of the file.")
    )}else{
      pedData<-pedigreeObj
    }

  #reorganizing columns
  pedData<-data.frame(pedData[,isd])
  names(pedData)<-c("ind", "sire", "dam")

  #checking if there are duplicate data
  pedData<-unique(pedData)

  #checking if there are duplicate individuals with different information
  dup<-duplicated(pedData$ind)
  if(any(dup)){
    stop("Duplicated animals in the pedigree.")
  }

  #Checking if there are animal who appear in both columns: sire and dam
  bad<-match(pedData$sire, pedData$dam, incomparables = NA)
  if (!all(is.na(bad))){
    stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree.
         Function aborted!")
  }

  ###################################################################
  #Ordering the Pedigree                                            #
  ###################################################################
  #Setting the founders
  k<-with(pedData,
          is.na(sire) & is.na(dam))
  founders<-pedData[k,]
  pedData<-pedData[!k, ]
  rm(k)

  #Adding parents
  parents<-data.frame(ind = c(pedData$sire, pedData$dam), sire = NA, dam = NA)
  parents<-rbind(founders, parents)
  rm(founders)
  parents<-unique(parents, fromLast = TRUE)
  parents<-parents[!is.na(parents$ind), ]
  #

  #Setting the youngest individuals
  i<-with(pedData,
          !(ind%in%sire | ind%in%dam))
  if(all(i)){
    young<-pedData[i, ]
    orderped<-rbind(parents, young)
    orderped<-unique(orderped)
    print(orderped)
  }else{
    j<-!i
    if(all(j)){
      older<-pedData[j, ]
      orderped<-rbind(parents, older)
      orderped<-unique(orderped)
      print(orderped)
    }else{
      young<-pedData[i, ]


      orderped<-young

      #Setting the older individuals
      j<-!i
      older<-pedData[j, ]

      #Removing parents already in older
      i<-match(older$ind, parents$ind)
      parents<-parents[-i, ]

      while(TRUE){
        i<-with(older,
                !(ind%in%sire | ind%in%dam))
        young<-older[i, ]

        orderped<-rbind(young, orderped)
        j<-!i

        if(sum(j) > 0){
          older<-older[j, ]
        }else{
          orderped<-rbind(parents, orderped)
          dup<-duplicated(orderped$ind, fromLast = TRUE)
          orderped<-orderped[!dup, ]
          break
        }
      }
    }
  }
  suppressWarnings(rm(older, parents, young, dup, i, j))

  #Recoding
  mapaCod<-data.frame(cod = orderped$ind, recod = 1: nrow(orderped))

  pedData<-orderped
  rm(orderped)

  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(pedData[,j], mapaCod$cod)
    pedData[,j]<-mapaCod$recod[i]
  }

  #Checking if the animal recode is smaller than his parents
  for(i in 1:nrow(pedData)){
    t<-pedData$ind[i] <= pedData$sire[i] || pedData$ind[i] <= pedData$dam[i]
    if(!is.na(t)){
      if(t){
        smaller<-pedData[i]
        print("Parent have code smaller than progeny")
        print(smaller)
      }
    }
  }

  #Replacing the original codes in data object
  for(j in colsPdgDat.isd){
    i<-match(udata[[j]], mapaCod[,1])
    udata[[j]]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPdgDat.isd]<-replace(udata[,colsPdgDat.isd], list = is.na(udata[,colsPdgDat.isd]), values = 0)

  pedDataList<-list(map = mapaCod, ped = pedData, data = udata)
  pedDataList
}


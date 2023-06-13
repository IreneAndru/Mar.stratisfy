#' @title stratisfy
#' @description stratisfy is a program capable of Stratified Analyses of both DFO 
#' and NMFS fisheries survey data. This package attempts to emulate the results
#' of the older, APL version which has been in use for many years.
#' 
#' Using stratisfy requires: 
#' \enumerate{
#' \item an Oracle account on the DFO PTRAN database;
#' \item access to PTRAN (i.e. either within the network, or via a VPN) 
#' \item appropriate select permissions on the 'groundfish' and/or 'usnefsc' 
#' schemas
#' }
#' New Oracle accounts are requested by filling out a computer account request 
#' form. These forms may be obtained from IM&TS. Select access to the DFO and 
#' NMFS data is provided by the respective datasets manager (checking with 
#' \email{Mike.McMahon@@dfo-mpo.gc.ca} is a good start).
#' 
#' Users can supply as much or as little information as they want when calling 
#' stratisfy(). If required information is not supplied, a select box will show 
#' the available options to the user. Please see below for examples of function 
#' calls providing both the minimum and the maximum number of parameters.
#' 
#' Fun Facts About stratisfy
#' \enumerate{
#' \item Berried Females -- APL stratisfy was inconsistent with species where 
#' females could be coded as 'berried' (i.e. 3).  When unsexed analyses were 
#' done, the females were included, but when sexed analyses were done, these 
#' females were not included in the results.  Such inconsistency seems 
#' suboptimal, and is retained for now such that APL and R versions of the 
#' application can be compared. 
#' \item Ages By Sex -- When APL stratisfy did analyses by sex, it combined the 
#' sexes for the 'Age Mean', 'Age Total', (and standard errors of each).  This 
#' version adds the parameter \code{ageBySex} so that by setting it to FALSE, 
#' you can get the combined results, but you can also set it to TRUE, and get 
#' the values for each sex individually.
#' \item I liked the old way -- Some aspects of the 'classic' (i.e. APL) 
#' stratisfy seemed inefficient.   For example, the Strata information was broken 
#' up over multiple worksheets, as was the weight information. By default, a new 
#' spreadsheet has been designed, but since there will always be those who 
#' prefer the old way, setting \code{output='classic'} will generate a 
#' spreadsheet almost indistinguishable from the APL version.
#' }
#' 
#' @param usepkg  The default value is \code{'rodbc'}, but \code{'roracle'} 
#' works as well. This describes the R package you use to connect to Oracle.  
#' @param agency  The default value is \code{'DFO'}, the other option is 
#' \code{'NMFS'}.  Setting to \code{NULL} will result in a pick list.
#' @param type This is the 'experiment type', and the default value is \code{1}.  
#' For \code{agency='DFO'}, \code{5} is also acceptable. For agency = NMFS, 136 
#' is probable, but you can type any integer, and the typed number will be 
#' embedded in\code{USNEFSC.USS_STATION.SHG <= type}. Setting to \code{NULL} 
#' will result in a pick list.
#' @param year The default value is \code{NULL}, which will result in a pick list.
#' An example of a valid, non-empty value is \code{2017}.
#' @param season The default value is \code{''}, which will result in a pick 
#' list. Valid values are \code{'SUMMER', 'SPRING', 'WINTER', and 'FALL'}, 
#' subject to the availability of data in that season for the selected agency, 
#' year, type, etc... 
#' @param missions The default value is \code{NULL}, which will result in a pick 
#' list. An example valid value is \code{c('NED2016016')}.  If your choice is 
#' not valid (given your other selections), you will be presented with a pick
#' list of valid options. 
#' @param strataTable  The default value is \code{NULL}.  
#' Depending on your data, other valid values may include: 
#' \code{'GROUNDFISH.GSSTRATUM', 'USNEFSC.DFO5ZJM','USNEFSC.DFO5ZGHNO','USNEFSC.NMFS5ZJM',
#' 'USNEFSC.NMFS5ZGHNO','USNEFSC.NMFS5ZJMC','USNEFSC.NMFS5ZJMU',
#' 'USNEFSC.NMFS5ZU'}.
#' @param wingspread  This is the width in feet of a standard tow.  The default 
#' value is \code{41}.  Setting this to \code{NULL} will result in a pick list. 
#' @param towDist  This is the length (NM) of a standard tow.  The default value 
#' is \code{1.75}.  Setting this to \code{NULL} will result in a pick list. 
#' @param strata  These are the strata for which you want results.  The default 
#' value is \code{NULL} The DFO SUmmer survey are c(440:495).  
#' @param areas  These are the areas for which you want results.  Some strata 
#' can be further broken down by areas, so this allows selection of data within 
#' a part of a strata. It can also be set to \code{'all'} or a vector of valid areas 
#' (e.g. \code{c('424','511','4xs')})
#' @param spp  This is the species code for the species you want to analyze.  
#' The default value is \code{NULL}, which will result in a pick list. An example 
#' of a valid, non-empty value is \code{2526}. 
#' @param bySex If the selected species was measured by sex, this parameter
#' allows you to perform calculations by sex.  The default value is \code{NULL}, 
#' which will result in a pick list. An example of a valid, non-empty value is 
#' \code{TRUE}. 
#' @param ageBySex The APL version of stratisfy ignored sex differences in some 
#' age results (e.g. 'age by set', 'age mean', 'age total', ' etc.) despite
#' analyses being done by sex.  The default value of this parameter is 
#' \code{FALSE} so that the original results are emulated.  However, setting 
#' this to \code{TRUE} (when \code{bySex=TRUE}), will show age results broken 
#' down by sex.  Setting this to \code{''} will result in a pick list.
#' @param useBins The default value is \code{TRUE}. Should data be binned using the length groups 
#' from GSPEC?
#' @param output  The default value is \code{'new'}.  This determines the format 
#' of the output Excel file. Setting this to \code{'classic'} will emulate the
#' original APL stratisfy results, including overriding your parameter for 
#' \code{ageBySex} and forcing it to FALSE.  If no excel output is desired, set
#' this parameter to an empty string \code{''}
#' @param alkTable The default is \code{NULL}.  Setting this to a valid path
#' allows you to add additional values for the age length key.
#' @param file_id This is a modifier you can have appended to the output 
#' filename.  The resultant file will be called \code{'Mar_stratisfy<_file_id>.xlsx'}
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for this stored
#' in your environment (e.g. from an rprofile file), this can be left and that
#' value will be used.  If a value for this is provided, it will take priority
#' over your existing value.
#' @family Gale-force
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom Mar.utils make_oracle_cxn
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeDataTable
#' @importFrom openxlsx saveWorkbook
#' @export
#' @note Reports of errors and/or recommendations for improvements should be 
#' addressed to:
#' Mike McMahon (Mike.McMahon@dfo-mpo.gc.ca)
#' Fisheries and Oceans Canada


stratisfy<-function(usepkg = 'rodbc', 
                    agency = 'DFO',
                    type = 1,
                    year = NULL,
                    season = NULL,
                    missions = NULL,
                    strataTable = NULL,
                    wingspread = 41,
                    towDist = 1.75,
                    strata = NULL,
                    areas = NULL,
                    spp = NULL,
                    bySex = NULL,
                    ageBySex = FALSE,
                    useBins=T,
                    output = "new",
                    alkTable = NULL,
                    file_id = NULL,
                    fn.oracle.username ="_none_",
                    fn.oracle.password="_none_",
                    fn.oracle.dsn="_none_"
){
  if (is.null(output))output<-NA
  oracle_cxn = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)
  
  agency = getUserInput("agency",agency=agency)
  type = getUserInput("type", agency=agency, type=type, oracle_cxn = oracle_cxn)
  missionsAndStrata = getUserInput("missionsAndStrata", agency=agency,type=type, 
                                   year=year, season=season, missions=missions,
                                   oracle_cxn = oracle_cxn)
  dfMissions = missionsAndStrata[[1]]
  dfMissionsStrata = missionsAndStrata[[2]]
  rm(missionsAndStrata)

  strataTable = getUserInput("strataTable", strataTable=strataTable, 
                             dfMissionsStrata=dfMissionsStrata, 
                             oracle_cxn = oracle_cxn)
  wingspread = getUserInput("wingspread", agency=agency, wingspread=wingspread)
  towDist = getUserInput("towDist", towDist=towDist)
  dfStrata = getUserInput("strata",agency=agency, strataTable=strataTable, 
                          strata = strata, dfMissionsStrata=dfMissionsStrata, 
                          towDist=towDist, wingspread=wingspread, 
                          oracle_cxn = oracle_cxn)
  areas = getUserInput("areas", agency=agency, missions=dfMissions, 
                       strata = dfStrata[,"STRAT"], areas = areas, 
                       oracle_cxn = oracle_cxn)
  spp = getUserInput("spp", agency = agency, spp=spp, bySex = bySex, 
                     ageBySex = ageBySex, oracle_cxn = oracle_cxn)
  bySex = spp[[1]]
  dfSpp = spp[[2]]
  ageBySex = spp[[3]]
  rm(spp)
  dfRawCatch <- extractData('catch', agency=agency, dfSpp=dfSpp, missions=dfMissions, strata = dfStrata$STRAT, areas = areas, oracle_cxn = oracle_cxn)
  dfRawInf <- extractData('inf', agency=agency, missions=dfMissions, strata = dfStrata$STRAT, areas = areas, type=type, oracle_cxn = oracle_cxn)
  dfRawDet <- extractData('det', agency=agency, missions=dfMissions, strata = dfStrata$STRAT, areas = areas, dfSpp = dfSpp, bySex = bySex, type=type, oracle_cxn = oracle_cxn, useBins = useBins)
  if (nrow(dfRawDet)>0 & !useBins){
    dfRawDet$BINWIDTH = 1
  }
  #if (all(nchar(dfRawInf[,"STRAT"])>3))dfRawInf[,"STRAT"]<-paste0(0,dfRawInf[,"STRAT"])
  if (all(nchar(dfRawInf[,"STRAT"]) ==4)) dfRawInf[,"STRAT"]<-paste0(0,dfRawInf[,"STRAT"])
  if (!is.null(alkTable))  {
    alkTable <-getAlkTable(alkTable)
    cat("alk table captured, but not applied")
  }
  dfNWSets <- calcNumsWeights('sets',dfRawCatch=dfRawCatch,dfRawInf=dfRawInf, towDist=towDist)
  dfNWAgg <- calcNumsWeights('setsAgg', dfNWSets=dfNWSets, dfStrata=dfStrata)
  
  dfStrata <- merge(dfStrata, calcNumsWeights('strataProp', dfNWSets=dfNWSets, 
                                              dfStrata=dfStrata, dfNWAgg=dfNWAgg), all.x=T)
  allStrat = as.data.frame(dfStrata[,"STRAT"])
  colnames(allStrat)<-"STRAT"
  lengthsData <-calcAgeLen('lengths', agency = agency, dfNWSets=dfNWSets, dfRawDet=dfRawDet, 
                           dfRawInf=dfRawInf, dfStrata=dfStrata, dfSpp=dfSpp, 
                           towDist=towDist, bySex = bySex, useBins=useBins)
  agelen<-lengthsData$agelen
  lengthsTotals<-lengthsData$length_total
  lset = lengthsData$lset
  
  ageLengthKey <-calcAgeLen('ageKey', agelen=agelen, dfSpp=dfSpp, lengthsTotals 
                            = lengthsTotals, lset = lset, dfStrata=dfStrata, 
                            bySex = bySex, output = output, ageBySex = ageBySex)
  lengthsData$agelen<-NULL
  lengthsData$lset<-NULL
  metadata=list(
    "Mar.stratisfy" = utils::packageDescription('Mar.stratisfy')$Version,
    "Date" = as.character(Sys.time()),
    "Data Source" = agency,
    "Missions" = paste("'", paste(dfMissions, collapse="','"),"'", sep=""),
    "Strata" = paste("'", paste(allStrat[,1], collapse="','"),"'", sep=""),
    "Area" = paste("'", paste(areas, collapse="','"),"'", sep=""),
    "Species" = paste0(dfSpp$CNAME, " (", dfSpp$SPEC ,")"),
    "By Sex" = bySex,
    "Distance" = towDist,
    "Spread" = wingspread,
    "Stratum Area Table" =  strataTable,
    "Experiment Type" = type,
    "ALK Modifications" = 'Not implemented yet')
  nwData <- merge(allStrat, dfNWAgg, all.x = TRUE)
  nwData <- merge(nwData, subset(dfStrata, select=c('STRAT','SQNM')), all.x=TRUE)
  nwData[is.na(nwData)]<-0
  nwData = rbind(nwData,c("STRAT"="TOTAL",colSums(nwData[,!(colnames(nwData) =="STRAT")],na.rm = T)))
  
  #Addition of Area Weighted Mean row
  nwData$CrossProd<-with(nwData, as.numeric(MEAN_WGT)*as.numeric(SQNM))
  MEAN_WGT_WAM<-sum(nwData$CrossProd[-(nrow(nwData))])/as.numeric(nwData$SQNM[(nrow(nwData))])
  nwData$CrossProd<-with(nwData, as.numeric(MEAN_NO)*as.numeric(SQNM))
  MEAN_NO_WAM<-sum(nwData$CrossProd[-(nrow(nwData))])/as.numeric(nwData$SQNM[(nrow(nwData))])
  nwData = rbind(nwData, c("STRAT" = "AreaWeightedMean", NA,NA,NA,MEAN_WGT_WAM,MEAN_NO_WAM,NA,NA,NA,NA,NA,NA,NA,NA))
  
  nwData[,2:ncol(nwData)] <- sapply(nwData[,2:ncol(nwData)],as.numeric)
  
  dfNWSets = merge(cbind("STRAT"= dfStrata$STRAT),dfNWSets,all.x=T)
  dfNWSets[is.na(dfNWSets)]<-0
  dfNWSets = merge(dfRawInf[,c("STRAT","UNIT_AREA","MISSION","SETNO","SLAT","SLONG")],dfNWSets)
  
  lengthsData$length_by_strat_mean = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_mean,all.x=T)
  #lengthsData$length_by_strat_mean= rbind(lengthsData$length_by_strat_mean,c("CRAP2?",colSums(lengthsData$length_by_strat_mean[,!(colnames(lengthsData$length_by_strat_mean) =="STRAT")],na.rm = T)))
  colnames(lengthsData$length_by_strat_mean) <- sub("X", "", colnames(lengthsData$length_by_strat_mean))
  
  lengthsData$length_by_strat_se = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_by_strat_se,all.x=T)
  lengthsData$length_by_strat_se[is.na(lengthsData$length_by_strat_se)]<-0
  #lengthsData$length_by_strat_se= data.frame(apply(lengthsData$length_by_strat_se, 2, function(x) as.numeric(as.character(x))))
  #lengthsData$length_by_strat_se<- rbind(lengthsData$length_by_strat_se,c("CRAP9",colSums(lengthsData$length_by_strat_se[,!(colnames(lengthsData$length_by_strat_se) =="STRAT")],na.rm = T)))
  
  lengthsData$length_total = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total,all.x=T)
  lengthsData$length_total[is.na(lengthsData$length_total)]<-0
  #add Column Totals
  lengthsData$length_total = rbind(lengthsData$length_total,c("STRAT"="TOTAL",colSums(lengthsData$length_total[,!(colnames(lengthsData$length_total) =="STRAT")],na.rm = T)))
  lengthsData$length_total[,2:ncol(lengthsData$length_total)] <- sapply(lengthsData$length_total[,2:ncol(lengthsData$length_total)],as.numeric)
  
  
  length_total_se = merge(cbind("STRAT"= dfStrata$STRAT),lengthsData$length_total_se,all.x=T)
  
  # ageLengthKey$age_mean = cbind(ageLengthKey$age_mean,TOTAL = rowSums(ageLengthKey$age_mean[,-which(colnames(ageLengthKey$age_mean) %in% c("STRAT"))],na.rm = T))
  # ageLengthKey$age_mean = rbind(ageLengthKey$age_mean,c("CRAP6?",colSums(ageLengthKey$age_mean[,2:ncol(ageLengthKey$age_mean)],na.rm = T)))
  # 
  ####add the colsums!!
  length_total_se[is.na(length_total_se)]<-0
  lengthsData$length_total_se<-length_total_se
  #lengthsData$length_total_se<-rbind(lengthsData$length_total_se,c("CRAP6?",colSums(lengthsData$length_total_se[,2:ncol(lengthsData$length_total_se)],na.rm = T)))
  
  
  

  weight_by_set = merge(dfRawInf[,c("STRAT","MISSION","SETNO")], dfNWSets[,c("STRAT","MISSION","SETNO", "SLAT","SLONG","UNIT_AREA","TOTWGT")], all.x=TRUE)
  
  if (class(ageLengthKey) == "list"){

     ageLengthKey$age_total$TOTAL = rowSums(ageLengthKey$age_total[,2:ncol(ageLengthKey$age_total)],na.rm = T)
     ageLengthKey$age_total = rbind(ageLengthKey$age_total,c("TOTAL",colSums(ageLengthKey$age_total[,2:ncol(ageLengthKey$age_total)],na.rm = T)))
     ageLengthKey$age_total[,2:ncol(ageLengthKey$age_total)] <- sapply(ageLengthKey$age_total[,2:ncol(ageLengthKey$age_total)],as.numeric)
     
    ageLengthKey$age_table$LENGTHS <- row.names(ageLengthKey$age_table)
    ageLengthKey$age_table<-ageLengthKey$age_table[,c(ncol(ageLengthKey$age_table),1:ncol(ageLengthKey$age_table)-1)]
    ageLengthKey$age_table = data.frame(apply(ageLengthKey$age_table, 2, function(x) as.numeric(as.character(x))))

        ageLengthKey$alw$LENGTHS <- row.names(ageLengthKey$alw)
    ageLengthKey$alw<-ageLengthKey$alw[,c(ncol(ageLengthKey$alw),1:ncol(ageLengthKey$alw)-1)]
    #ageLengthKey$alw<-rbind(ageLengthKey$alw,c("CRAP3?",colSums(ageLengthKey$alw[,2:ncol(ageLengthKey$alw)],na.rm = T)) )

    if (nrow(ageLengthKey$alk)>1){
      ageLengthKey$alk = data.frame(apply(ageLengthKey$alk, 2, function(x) as.numeric(as.character(x))))
    ageLengthKey$alk$TOTAL = rowSums(ageLengthKey$alk[,2:ncol(ageLengthKey$alk)],na.rm = T)
    } else {
      ageLengthKey$alk$TOTAL = rowSums(ageLengthKey$alk[,2:ncol(ageLengthKey$alk)],na.rm = T)}
    ageLengthKey$alk = rbind(ageLengthKey$alk,c("TOTAL",colSums(ageLengthKey$alk[,2:ncol(ageLengthKey$alk)],na.rm = T)))
    ageLengthKey$alk[,2:ncol(ageLengthKey$alk)] <- sapply(ageLengthKey$alk[,2:ncol(ageLengthKey$alk)],as.numeric)
    
    ageLengthKey$age_by_set = cbind(ageLengthKey$age_by_set,TOTAL = rowSums(ageLengthKey$age_by_set[,-which(colnames(ageLengthKey$age_by_set) %in% c("STRAT","MISSION","SETNO"))],na.rm = T))
    
    ageLengthKey$age_mean = cbind(ageLengthKey$age_mean,TOTAL = rowSums(ageLengthKey$age_mean[,-which(colnames(ageLengthKey$age_mean) %in% c("STRAT"))],na.rm = T))
    #ageLengthKey$age_mean = rbind(ageLengthKey$age_mean,c("CRAP6?",colSums(ageLengthKey$age_mean[,2:ncol(ageLengthKey$age_mean)],na.rm = T)))
    ageLengthKey$age_mean[,2:ncol(ageLengthKey$age_mean)] <- sapply(ageLengthKey$age_mean[,2:ncol(ageLengthKey$age_mean)],as.numeric)
    
    ageLengthKey$age_mean_se  = cbind(ageLengthKey$age_mean_se,TOTAL = rowSums(ageLengthKey$age_mean_se[,-which(colnames(ageLengthKey$age_mean_se) %in% c("STRAT"))],na.rm = T))
   # ageLengthKey$age_mean_se = rbind(ageLengthKey$age_mean_se,c("CRAP7?",colSums(ageLengthKey$age_mean_se[,2:ncol(ageLengthKey$age_mean_se)],na.rm = T)))

   # ageLengthKey$age_total_se$TOTAL = rowSums(ageLengthKey$age_total_se[,-which(colnames(ageLengthKey$age_total_se) %in% c("STRAT"))],na.rm = T)
    ageLengthKey$age_total_se = rbind(ageLengthKey$age_total_se,c("TOTAL",colSums(ageLengthKey$age_total_se[,2:ncol(ageLengthKey$age_total_se)],na.rm = T)))
    ageLengthKey$age_total_se[,2:ncol(ageLengthKey$age_total_se)] <- sapply(ageLengthKey$age_total_se[,2:ncol(ageLengthKey$age_total_se)],as.numeric)
}
  #Add the totals to Strata for approp columns
  dfStrataDataCols =c("SQNM","TUNITS","AREAPROP","AREAPROPSTERR","AREATOT","AREATOTSTERR")
  dfStrataDataTots = colSums(dfStrata[,dfStrataDataCols],na.rm = TRUE)
  names(dfStrataDataTots) <-dfStrataDataCols
  dfStrataOthCols = rep(NA, length(dfStrata[,!(colnames(dfStrata) %in% dfStrataDataCols)]))
  names(dfStrataOthCols) <- colnames(dfStrata[,!(colnames(dfStrata) %in% dfStrataDataCols)])
  totals= data.frame(t(c(dfStrataDataTots,dfStrataOthCols)))
  dfStrata = rbind(dfStrata,totals)  
  
  
  res=list(metadata = metadata,
           strataInfo = dfStrata,
           nwInfo = nwData,
           lengthInfo = lengthsData,
           ageInfo = ageLengthKey
  )
  
  if (!is.na(output)){
    file_id = ifelse(!is.null(file_id),paste0("_",file_id),"")
    wbName = paste0('Mar_stratisfy',file_id, ".xlsx")
    
    # wbName = "Mar_stratisfy.xlsx"
    md = data.frame(unlist(metadata))
    colnames(md)<-"Value"
    wb<-openxlsx::createWorkbook(creator = paste0("Mar.stratisfy v.",metadata$Mar.stratisfy))
    sheet1 <- openxlsx::addWorksheet(wb, sheetName = "QUERY")
    openxlsx::writeDataTable(wb, x=data.frame(md), rowNames = TRUE, sheet = sheet1, withFilter = FALSE)
    
    if (output=="classic"){
      sheet2 <- openxlsx::addWorksheet(wb, sheetName = "Strata Area")
      openxlsx::writeDataTable(wb, x=dfStrata[,c("STRAT","TUNITS","SQNM")], rowNames = FALSE, sheet = sheet2)
      sheet3 <- openxlsx::addWorksheet(wb, sheetName = "Prop Area")
      openxlsx::writeDataTable(wb, x=dfStrata[,c("STRAT","AREAPROP")], rowNames = FALSE, sheet = sheet3)
      sheet4 <- openxlsx::addWorksheet(wb, sheetName = "Prop Area Standard Error")
      openxlsx::writeDataTable(wb, x=dfStrata[,c("STRAT","AREAPROPSTERR")], rowNames = FALSE, sheet = sheet4)
      sheet5 <- openxlsx::addWorksheet(wb, sheetName = "Total Area")
      openxlsx::writeDataTable(wb, x=dfStrata[,c("STRAT","AREATOT")], rowNames = FALSE, sheet = sheet5)
      sheet6 <- openxlsx::addWorksheet(wb, sheetName = "Total Area Standard Error")
      openxlsx::writeDataTable(wb, x=dfStrata[,c("STRAT","AREATOTSTERR")], sheet = sheet6)
      if (class(ageLengthKey) == "list"){
        sheet7 <- openxlsx::addWorksheet(wb, sheetName = "Age Length Key")
        openxlsx::writeDataTable(wb, x=data.frame(ageLengthKey$alk), rowNames = FALSE, sheet = sheet7)
        sheet8 <- openxlsx::addWorksheet(wb, sheetName = "Age Table")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_table, rowNames = FALSE, sheet = sheet8)
        sheet9 <- openxlsx::addWorksheet(wb, sheetName = "Age Length Weight")
        openxlsx::writeDataTable(wb, x=ageLengthKey$alw, rowNames = FALSE, sheet = sheet9)
        sheet15 <- openxlsx::addWorksheet(wb, sheetName = "Age By Set")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_by_set, rowNames = FALSE, sheet = sheet15)
        sheet16 <- openxlsx::addWorksheet(wb, sheetName = "Age Mean")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_mean, rowNames = FALSE, sheet = sheet16)
        sheet17 <- openxlsx::addWorksheet(wb, sheetName = "Age Mean Standard Error")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_mean_se, rowNames = FALSE, sheet = sheet17)
        sheet18 <- openxlsx::addWorksheet(wb, sheetName = "Age Total")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_total, rowNames = FALSE, sheet = sheet18)
        sheet19 <- openxlsx::addWorksheet(wb, sheetName = "Age Total Standard Error")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_total_se, rowNames = FALSE, sheet = sheet19)
      } 
      sheet10 <- openxlsx::addWorksheet(wb, sheetName = "Length By Set")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_set, rowNames = FALSE, sheet = sheet10)
      sheet11 <- openxlsx::addWorksheet(wb, sheetName = "Length Mean")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_strat_mean, rowNames = FALSE, sheet = sheet11)
      sheet12 <- openxlsx::addWorksheet(wb, sheetName = "Length Mean Standard Error")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_strat_se, rowNames = FALSE, sheet = sheet12)
      sheet13 <- openxlsx::addWorksheet(wb, sheetName = "Length Total")
      openxlsx::writeDataTable(wb, x=lengthsData$length_total, rowNames = FALSE, sheet = sheet13)
      sheet14 <- openxlsx::addWorksheet(wb, sheetName = "Length Total Standard Error")
      openxlsx::writeDataTable(wb, x=lengthsData$length_total_se, rowNames = FALSE, sheet = sheet14)
      sheet20 <- openxlsx::addWorksheet(wb, sheetName = "Weight by Set")
      openxlsx::writeDataTable(wb, x=weight_by_set, rowNames = FALSE, sheet = sheet20)
      sheet21 <- openxlsx::addWorksheet(wb, sheetName = "Weight Mean")
      openxlsx::writeDataTable(wb, x=nwData[,c("STRAT","MEAN_WGT")], rowNames = FALSE, sheet = sheet21)
      sheet22 <- openxlsx::addWorksheet(wb, sheetName = "Weight Mean Standard Error")
      openxlsx::writeDataTable(wb, x=nwData[,c("STRAT","ST_ERR_WGT")], rowNames = FALSE, sheet = sheet22)
      sheet23 <- openxlsx::addWorksheet(wb, sheetName = "Weight Total")
      openxlsx::writeDataTable(wb, x=nwData[,c("STRAT","BIOMASS")], rowNames = FALSE, sheet = sheet23)
      sheet24 <- openxlsx::addWorksheet(wb, sheetName = "Weight Total Standard Error")
      openxlsx::writeDataTable(wb, x=nwData[,c("STRAT","ST_ERR_BIOMASS")], rowNames = FALSE, sheet = sheet24)
    }else if (nchar(output)>0){
      sheet2 <- openxlsx::addWorksheet(wb, sheetName = "Strata Info")
      openxlsx::writeDataTable(wb, x=dfStrata, rowNames = FALSE, sheet = sheet2)
      sheet3 <- openxlsx::addWorksheet(wb, sheetName = "Numbers and Weights by Strata")
      openxlsx::writeDataTable(wb, x=nwData, rowNames = FALSE, sheet = sheet3)
      sheet3a <- openxlsx::addWorksheet(wb, sheetName = "Numbers and Weights by Set")
      openxlsx::writeDataTable(wb, x=dfNWSets, rowNames = FALSE, sheet = sheet3a)
      if (class(ageLengthKey) == "list"){
        sheet4 <- openxlsx::addWorksheet(wb, sheetName = "Age Length Key")
        openxlsx::writeDataTable(wb, x=data.frame(ageLengthKey$alk), rowNames = FALSE, sheet = sheet4)
        sheet5 <- openxlsx::addWorksheet(wb, sheetName = "Age Table")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_table, rowNames = FALSE, sheet = sheet5)
        sheet6 <- openxlsx::addWorksheet(wb, sheetName = "Age Length Weight")
        openxlsx::writeDataTable(wb, x=ageLengthKey$alw, rowNames = FALSE, sheet = sheet6)
        sheet12 <- openxlsx::addWorksheet(wb, sheetName = "Age By Set")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_by_set, rowNames = FALSE, sheet = sheet12)
        sheet13 <- openxlsx::addWorksheet(wb, sheetName = "Age Mean")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_mean, rowNames = FALSE, sheet = sheet13)
        sheet14 <- openxlsx::addWorksheet(wb, sheetName = "Age Mean Standard Error")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_mean_se, rowNames = FALSE, sheet = sheet14)
        sheet15 <- openxlsx::addWorksheet(wb, sheetName = "Age Total")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_total, rowNames = FALSE, sheet = sheet15)
        sheet16 <- openxlsx::addWorksheet(wb, sheetName = "Age Total Standard Error")
        openxlsx::writeDataTable(wb, x=ageLengthKey$age_total_se, rowNames = FALSE, sheet = sheet16)
      }
      sheet7 <- openxlsx::addWorksheet(wb, sheetName = "Length By Set")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_set, rowNames = FALSE, sheet = sheet7)
      sheet8 <- openxlsx::addWorksheet(wb, sheetName = "Length Mean")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_strat_mean, rowNames = FALSE, sheet = sheet8)
      sheet9 <- openxlsx::addWorksheet(wb, sheetName = "Length Mean Standard Error")
      openxlsx::writeDataTable(wb, x=lengthsData$length_by_strat_se, rowNames = FALSE, sheet = sheet9)
      sheet10 <- openxlsx::addWorksheet(wb, sheetName = "Length Total")
      openxlsx::writeDataTable(wb, x=lengthsData$length_total, rowNames = FALSE, sheet = sheet10)
      sheet11 <- openxlsx::addWorksheet(wb, sheetName = "Length Total Standard Error")
      openxlsx::writeDataTable(wb, x=lengthsData$length_total_se, rowNames = FALSE, sheet = sheet11)
    } 
    openxlsx::saveWorkbook(wb, file = wbName, overwrite=TRUE)
    cat(paste0("\n\nWrote your excel file to ",file.path(getwd(),wbName),""))
  }
  
  return(invisible(res))
}

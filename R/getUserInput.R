# @title getUserInput
# @description This function can present the user with a variety of select 
# boxes from which they can select appropriate options.  The select box that is
# shown is determined by the value of the paramter \code{"requested"}.
# Some of the options that are presented are dependent on the existence of 
# values written to the global environment by the stratisfy function.
# @param requested   The default value is \code{NULL}. This determines which 
# picklist will be invoked.
# @param agency   The default value is \code{NULL}.  Valid values are 'DFO' and
#  'NMFS'.
# @param type   The default value is \code{NULL}.  Setting to \code{NULL} will 
# result in a pick list.
# @param strataTable   The default value is \code{NULL}.  Setting to 
# \code{NULL} will result in a pick list.
# @param year   The default value is \code{NULL}.  Setting to \code{NULL} will 
# result in a pick list.
# @param season   The default value is \code{NULL}.  Setting to \code{NULL} 
# will result in a pick list.
# @param wingspread   The default value is \code{NULL}.  Setting to \code{NULL} 
# will result in a pick list.
# @param towDist  The default value is \code{NULL}.  Setting to \code{NULL} 
# will result in a pick list.
# @param spp   The default value is \code{NULL}.  Setting to \code{NULL} will 
# result in a pick list.
# @param bySex   The default value is \code{NULL}.  Setting to \code{NULL} will 
# result in a pick list.
# @param strata   The default value is \code{NULL}.  Setting to \code{NULL} 
# will result in a pick list.
# @param areas   The default value is \code{NULL}.  Setting to \code{NULL} 
# will result in a pick list.
# @param dfMissionsStrata   The default value is \code{NULL}.  Setting to 
# \code{NULL} will result in a pick list.
# @param ageBySex   The default value is \code{NULL}.  Setting to 
# \code{NULL} will result in a pick list.
# @family Gale-force
# @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
# @importFrom utils select.list
# @importFrom Mar.utils SQL_in
# @keywords internal
getUserInput <-function(requested = NULL, agency = NULL, type = NULL, 
                        strataTable = NULL, year = NULL, season = NULL, 
                        wingspread = NULL, towDist = NULL, spp = NULL, 
                        bySex = NULL, strata = NULL, areas = NULL, 
                        dfMissionsStrata = NULL, ageBySex = NULL, 
                        missions = NULL, oracle_cxn =NULL){
  getAgency<-function(agency){
    if (!is.null(agency)){
      agency<-toupper(agency)
      if(agency %in% c("DFO","NMFS"))return(agency)
    }
    
    choice<-utils::select.list(c("DFO","NMFS"), multiple=F, graphics=T, title='Please choose an agency:')
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(choice)
  }
  
  getType<-function(agency, type, oracle_cxn){
    if (agency == "NMFS"){
      if (type %in% c(1,5))type<-NULL
      if (!is.null(type)){
        if (is.numeric(type))return(type)
      }
      typePick<-NA
      while(is.na(typePick)){
        typePick = utils::select.list(c("136","other"),
                                      preselect=c("136"),
                                      multiple=F, graphics=T, 
                                      title='Set Type')
        if (is.na(typePick)) print("You must select one of provided options")
      }
      if (typePick == "other") {
        typePick = as.numeric(readline(prompt =
                                         "Any experiment type greater than or equal to the value will be included in 
the analysis. The default is <= 136. This may be changed as required. Used in 
the queries as: *.SHG <= 136

Please enter the survey type:"))
        print(typePick)
      }
      typePick=as.numeric(typePick)
      return(typePick)
    }else if (agency == "DFO"){
      if (!is.null(type)){
        if (is.numeric(type))return(type)
      }
      typePick <-NA
      while(is.na(typePick)){
        
        choice <- oracle_cxn$thecmd(oracle_cxn$channel, paste("select XTYPEDESC, XTYPE
                                             from groundfish.GSXTYPE
                                              WHERE XTYPE IN (1,5)
                                             ORDER BY XTYPE",sep=""))
        choice<-paste( choice[,1], " (", choice[,2],")",sep="")
        
        typePick<-utils::select.list(choice,
                                     multiple=F, graphics=T,
                                     title='Please select the type of trawl:')
        typePick<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', typePick) )
        if (is.na(typePick)) print("You must select a survey type")
      }
      return(typePick)
    }
  }
  
  getMissionsAndStrata<-function(agency, type, year, season, missions, oracle_cxn){
    if (is.null(missions)){
    availSeasons = switch(agency,
                          "DFO" = c("SPRING","SUMMER", "FALL"),
                          "NMFS"=c("SPRING","SUMMER", "FALL", "WINTER")) 
    seasonpick<-NA
    if (!is.null(season)){
      season <- toupper(season)
      if (season %in% availSeasons) seasonpick = season
    }else{
      #they asked for an invalid season - replace it with NA
      seasonpick<-NA
    }
    
    while(is.na(seasonpick)){
      seasonpick <- utils::select.list(availSeasons,
                                       multiple=F, graphics=T, 
                                       title='Season?')
      if (!seasonpick %in% availSeasons) print("You must select a season")
    }
    
    if (agency == "DFO"){
      year.query = paste0("select DISTINCT M.YEAR
                          from GROUNDFISH.GSMISSIONS M, GROUNDFISH.GSINF I 
                          WHERE 
                          M.MISSION = I.MISSION AND 
                          I.TYPE = ",type," AND
                          M.SEASON = '",seasonpick, "'")
    } else if (agency == "NMFS"){
      year.query = paste0("SELECT 
                          DISTINCT M.YEAR
                          FROM 
                          USNEFSC.USS_STATION S,
                          USNEFSC.USS_MSTR_CRUISE M
                          WHERE 
                          S.STRATUM IN (
    '01010','01020','01030','01040','01050','01060','01070','01080','01090','01100',
    '01110','01120','01130','01140','01150','01160','01170','01180','01190','01200',
    '01210','01220','01230','01240','01250','01260','01270','01280','01290','01300',
    '01310','01320','01330','01340','01350','01351','01360','01370','01380','01390',
    '01400','01410','01420','01490','01610','01620','01630','01640','01650','01660',
    '01670','01680','01690','01700','01710','01720','01730','01740','01750','01760',
    '396','397','398','399','400','401','402','403','404','405','406','407','408',
    '409','410','411','415','416','417','418','419','420','421','422','423','424',
    '425','426','427','428','429','431','432','433','434','435','436','437','438',
    '439','440','441','442','443','444','445','446','447','448','449','450','451',
    '452','453','454','455','456','457','458','459','460','461','462','463','464',
    '465','466','467','468','469','470','471','472','473','474','475','476','477',
    '478','480','481','482','483','484','485','490','491','492','493','494','495',
    '496','497','498','499','501','502','503','504','505','509','510','511','512',
    '551','552','553','554','555','556','557','558','559','560','5Z1','5Z2','5Z3',
    '5Z31','5Z32','5Z33','5Z34','5Z4','5Z41','5Z42','5Z5','5Z6','5Z7','5Z8','5Z9',
    '802','803','804','805','806','807','808','809','812','813','814','815','816',
    '817','818','819','822','823','824','825','826','827','828','829','832','833',
    '834','835','836','837','838','839','842','843','844','845','846','847','848',
    '849','852','853','854','855','856','857','858','859','862','863','864','865',
    '866','867','868','869','872','873','874','875','876','877','878','879','A16',
    'A17','A18','A21','A22','C16','C17','C18','C21','C22','U13','U14','U15','U16',
    'U17','U18','U19','U20','U21','U22','U23','U24','U25','U26','U27','U28','U29',
    'U30','U31','U32','U33','U34','U35','U36','U37','U38','U39','U40','U41','U42',
    'U43','U44','U45','U46','U47','U48','U49'
                          ) AND 
                          S.CRUISE6 = M.CRUISE6 AND
                          S.SHG <= ",type," AND 
                          M.SEASON = '",seasonpick, "'")
      
    }
    #replaced these with hardcoded strata to speed extraction Aug 31, 2018
    # SELECT DISTINCT STRAT FROM GROUNDFISH.GSSTRATUM
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZJM
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZGHNO
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJM
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZGHNO
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMC
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMU
    # UNION
    # SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZU

    
    cat("\n Checking years matching your criteria...\n")
    availYears = oracle_cxn$thecmd(oracle_cxn$channel, year.query)
    availYears = as.character(availYears[order(availYears$YEAR),1])
    
    yearpick <-NA
    if (!is.null(year)) {
      year = as.character(year)
      if (year %in% availYears){
        yearpick = year
      }else {
        cat("\n***\nYou specified a year that is not available given your other criteria\
            Please choose a different year, or check your parameters\n***\n")
        yearpick <-NA
      }
    }
    
    while(is.na(yearpick)){
      yearpick <- utils::select.list(availYears,
                                     multiple=F, graphics=T, 
                                     title='Year?')
      if (!yearpick %in% availYears) print("You must select a year")
    }
    
    if (agency == "DFO"){
      mission.query = paste0("select DISTINCT M.MISSION
                             from GROUNDFISH.GSMISSIONS M, GROUNDFISH.GSINF I 
                             WHERE 
                             M.MISSION = I.MISSION AND 
                             I.TYPE = ",type," AND 
                             M.YEAR = ",yearpick," AND
                             M.SEASON = '",seasonpick, "' AND 
                             M.MISSION IS NOT NULL
                             ORDER BY SUBSTR(M.MISSION, 4) DESC")
    } else if (agency == "NMFS"){
      mission.query = paste0("SELECT 
                             DISTINCT(S.CRUISE6) MISSION, M.SEASON
                             FROM 
                             USNEFSC.USS_STATION S,
                             USNEFSC.USS_MSTR_CRUISE M
                             WHERE 
                             S.STRATUM IN (
                             SELECT DISTINCT STRAT FROM GROUNDFISH.GSSTRATUM
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZJM
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.DFO5ZGHNO
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJM
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZGHNO
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMC
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZJMU
                             UNION
                             SELECT DISTINCT STRAT FROM USNEFSC.NMFS5ZU
                             ) AND 
                             S.CRUISE6 = M.CRUISE6 AND
                             S.SHG <= ",type," AND 
                             M.SEASON = '",seasonpick, "' AND 
                             YEAR = ",yearpick, "
                             ORDER BY S.CRUISE6")
    }
    cat("\n Looking for Missions meeting your criteria...\n")
    availMissions = oracle_cxn$thecmd(oracle_cxn$channel, mission.query)
    if (NROW(availMissions)==0) stop("\n\n!!!ABORTING!!!\nNo Missions can be found where the requested Experiment Type and Season can be found")
    missionPick <-NA
    if (!is.null(missions)) {
      if (all(missions %in% availMissions[,1])){
        missionPick = missions
      }else {
        cat("\n***\nYou specified one or more mission(s) that is not available given your other criteria.
Please make a selection from the available options, or check your parameters\n***\n")
        missionPick <-NA
      }
    }
    
    if (NROW(availMissions)==1) {
      missionPick=availMissions[,1]
      cat(paste0("\n Defaulting to ",missionPick," - the only  mission matching your criteria\n")) 
    }
    while(any(is.na(missionPick))){
      missionPick <- utils::select.list(availMissions[,1],preselect=availMissions[,1],
                                        multiple=T, graphics=T, 
                                        title='Mission?')
      if (length(missionPick)==0) {
        cat("You must select a mission")
        missionPick<-NA
      }
    }
    } else{
      missionPick = missions
    }
    #1) STRATA
    if (agency == "DFO"){
      sql1 = paste0("SELECT DISTINCT STRAT FROM GROUNDFISH.GSINF WHERE TYPE = ",type," AND MISSION IN (",Mar.utils::SQL_in(missionPick),") AND STRAT IS NOT NULL ORDER BY STRAT")
    } else if (agency == "NMFS"){
      sql1 = paste0("SELECT DISTINCT STRATUM STRAT FROM USNEFSC.USS_STATION WHERE SHG <= ",type," AND CRUISE6 IN (",Mar.utils::SQL_in(missionPick),") AND STRATUM IS NOT NULL ORDER BY STRATUM")
    }
    availStrata = oracle_cxn$thecmd(oracle_cxn$channel,  sql1)
    # #2) AREAS
    # f (agency == "DFO"){
    #   sql2 = paste0("SELECT DISTINCT AREA FROM GROUNDFISH.GSINF WHERE MISSION IN (",Mar.utils::SQL_in(missionPick),") ORDER BY AREA")
    # } else if (agency == "NMFS"){
    #   sql2 = paste0("SELECT DISTINCT AREA FROM USNEFSC.USS_STATION WHERE CRUISE6 IN (",Mar.utils::SQL_in(missionPick),") ORDER BY AREA")
    # }
    # availArea = oracle_cxn$thecmd(oracle_cxn$channel,  sql2)
    
    res = list(missionPick, availStrata)
    return(res)
  }
  
  getWingspread<-function(agency, wingspread){
    availWingspread<-switch(agency,
                            "DFO" = c("41","34"),
                            "NMFS"=c("34","36","41"))
    availWingspreadPre<-switch(agency,
                               "DFO" = c("41"),
                               "NMFS"=c("34"))
    if (!is.null(wingspread)){
      if (wingspread %in% availWingspread)return(as.numeric(wingspread)) 
    }
    choice<-NA
    while(is.na(choice)){
      choice =  as.numeric(utils::select.list(availWingspread,
                                              preselect=availWingspreadPre,
                                              multiple=F, graphics=T, 
                                              title='Select the Wingspread (ft)'))
      if (is.na(choice)) print("You must select a wingspread")
      return (choice)
    }
  }
  
  getTowDist<-function(towDist){
    if (!is.null(towDist)){
      if (as.numeric(towDist)>0 & as.numeric(towDist)<100 ) return(towDist) 
    }
    towDistPick<-NA
    while(is.na(towDistPick)){
      towDistPick = utils::select.list(c("1.75","other"),
                                       preselect=c("1.75"),
                                       multiple=F, graphics=T, 
                                       title='Tow Distance (NM)')
      if (is.na(towDistPick)) print("You must select one of provided options")
    }
    if (towDistPick == "other") {
      towDistPick = as.numeric(readline(prompt =
                                          "Please enter the tow distance you want (NM):"))
      print(towDistPick)
    }
    towDistPick=as.numeric(towDistPick)
    return(towDistPick)
  }
  
  getStrataTable<-function(strataTable, dfMissionsStrata,oracle_cxn){
    quickStrataTables<- c("GROUNDFISH.GSSTRATUM","USNEFSC.DFO5ZJM",
                          "USNEFSC.DFO5ZGHNO","USNEFSC.NMFS5ZJM",
                          "USNEFSC.NMFS5ZGHNO","USNEFSC.NMFS5ZJMC",
                          "USNEFSC.NMFS5ZJMU","USNEFSC.NMFS5ZU")
    if (!is.null(strataTable)){
      strataTable<-toupper(strataTable)
      if (strataTable %in% quickStrataTables)return(strataTable)
    }
    strataTablePick<-NA 
    
    
      stratList_n = Mar.utils::SQL_in(dfMissionsStrata[,1], apos = F)
      stratList_c = Mar.utils::SQL_in(dfMissionsStrata[,1], apos = T)
      
      if (!all(is.numeric(dfMissionsStrata[,1])))stratList_n=Mar.utils::SQL_in(dfMissionsStrata[,1], apos = T)
    #check which tables contain strata like the ones we got from the mission
    strataTableSql = paste0("
                        SELECT SRC, cnt FROM (
                        SELECT 'GROUNDFISH.GSSTRATUM' SRC, COUNT(*) cnt FROM GROUNDFISH.GSSTRATUM WHERE STRAT IN (",stratList_c,")
                        UNION
                        SELECT 'USNEFSC.DFO5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZJM WHERE STRAT IN (",stratList_c,")
                        UNION
                        SELECT 'USNEFSC.DFO5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.DFO5ZGHNO WHERE STRAT IN (",stratList_c,")
                        UNION
                        SELECT 'USNEFSC.NMFS5ZJM' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJM WHERE STRAT IN (",stratList_n,")
                        UNION
                        SELECT 'USNEFSC.NMFS5ZGHNO' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZGHNO WHERE STRAT IN (",stratList_n,")
                        UNION
                        SELECT 'USNEFSC.NMFS5ZJMC' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMC WHERE STRAT IN (",stratList_n,")
                        UNION
                        SELECT 'USNEFSC.NMFS5ZJMU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZJMU WHERE STRAT IN (",stratList_n,")
                        UNION
                        SELECT 'USNEFSC.NMFS5ZU' SRC, COUNT(*) cnt FROM USNEFSC.NMFS5ZU WHERE STRAT IN (",stratList_n,")
                        )
                        WHERE cnt >0 ORDER BY CNT DESC") 
    availStrataTables<-oracle_cxn$thecmd(oracle_cxn$channel, strataTableSql)
    
    if (NROW(availStrataTables)==0){
      stop("\n\n!!!ABORTING!!!\nNo Strata Tables meet your criteria")
    } else if (NROW(availStrataTables)==1){
      strataTablePick=availStrataTables[,1]
      cat(paste0("\nUsing ",strataTablePick," - the only  strata table matching your criteria\n")) 
    }else {
      while(is.na(strataTablePick)){
        if (!strataTablePick %in% availStrataTables) cat("\nYou must select a strata table (the number in brackets shows how many of your strata are present in the table)\n")
        strataTablePick <- utils::select.list(paste0(availStrataTables[,1]," (",availStrataTables[,2],")"),multiple=F, graphics=T, 
                                              title='Strata Table?')
        strataTablePick = gsub( " *\\(\\d{1,3}\\) *", "", strataTablePick)
      }
    }
    return(strataTablePick)
  }
  
  getSpp<-function(agency, spp, bySex, ageBySex, oracle_cxn){
    bySexChoice<-NA
    ageBySexChoice<-NA
    sppChoice<-NA
    if (!is.null(bySex)){
      if (bySex %in% c(TRUE,FALSE))bySexChoice<-bySex
    }
    
    if (!is.null(ageBySex)){
      if (ageBySex %in% c(TRUE,FALSE))ageBySexChoice<-ageBySex
    }
    
    getSexSelections<-function(bySexChoice, ageBySexChoice){
      #bit of an ugly loop, but should minimize unnecessary user input requests
      needsAll<-all(is.na(c(bySexChoice,ageBySexChoice))) 
      hasAll<-all(!is.na(c(bySexChoice,ageBySexChoice))) 
      
      #helper functions
      getBySex<-function(bySexChoice, ageBySexChoice){
        bySexChoice <- utils::select.list(c("Unsexed Analysis","Sexed Analysis"),
                                          multiple=F, graphics=T,
                                          title='Analysis by Sex?')
        bySexChoice = switch(bySexChoice, "Sexed Analysis" = TRUE,
                             "Unsexed Analysis" = FALSE)
        res=c(bySexChoice, ageBySexChoice)
        return(res)
      }
      
      getAgeBySex<-function(bySexChoice, ageBySexChoice){  
        ageBySexChoice = utils::select.list(c("Show Age Results By Sex",
                                              "Combine Sexes in Age Results (classic)"),
                                            multiple=F, graphics=T,
                                            title="How to Handle Sex In Age Results?")
        ageBySexChoice <- switch(ageBySexChoice,
                                 "Show Age Results By Sex" = TRUE,
                                 "Combine Sexes in Age Results (classic)" = FALSE)
        res=c(bySexChoice, ageBySexChoice)
        return(res)
      }
      
      if(hasAll){
        if (bySexChoice ==FALSE & ageBySexChoice == TRUE){
          ageBySexChoice<-FALSE
          cat("\nYou can't do age by sex without an sexed analysis - reverting to
combining sexes in age results\n")
        } 
        res=c(bySexChoice, ageBySexChoice)
        return(res)
      }else if (needsAll){
        s=getBySex(bySexChoice, ageBySexChoice)
        if (s[1]==FALSE){
          res=c(s[1], FALSE) #age by sex not an option
        }else{
          a=getAgeBySex(s[1], ageBySexChoice) 
          res=c(a[1], a[2])
        }
        return(res)
      }else {
        if (is.na(bySexChoice)){
          if (ageBySexChoice==TRUE){
            res=c(TRUE, TRUE)
            cat("\nageBySex ==TRUE implies desire for sexed analysis\n")
          }else{
            x= getBySex(bySexChoice, ageBySexChoice)
            res=c(x[1],x[2])
          }
        } else if (is.na(ageBySexChoice)){
          y = getAgeBySex(bySexChoice, ageBySexChoice)
          res=c(y[1],y[2])
        }
        return(res)
      }
    }
    
    sexSel = getSexSelections(bySexChoice, ageBySexChoice)
    bySexChoice = sexSel[1]
    ageBySexChoice = sexSel[2]
    if (agency =="DFO"){
      if (bySexChoice){
        species.query.tweak<-"AND LFSEXED = 'Y' "
      } else{
        species.query.tweak<-""
      }
      species.query=paste("SELECT DISTINCT(SPEC), initcap(CNAME) CNAME, 
                          LGRP, LFSEXED 
                          FROM GROUNDFISH.GSSPEC 
                          WHERE SPEC <> 9999 
                          ", species.query.tweak, " 
                          ORDER BY initcap(CNAME)",sep="")
    }else if (agency=="NMFS"){
      #not ideal - hard coded spp from uss_catch table with more than 1 catchsex
      if (bySexChoice){
        species.query.tweak<-"AND TO_NUMBER(SPEC) IN (15,22,26,75,108)"
      } else{
        species.query.tweak<-""
      }
      species.query=paste("SELECT TO_NUMBER(US.SPEC) SPEC, INITCAP(US.CNAME) CNAME, 
                                    US.LGRP, US.LFSEXED 
                                    FROM USNEFSC.USSPEC US
                                    WHERE  
                                    US.SPEC <> 9999
                                    ", species.query.tweak,"
                                    ORDER BY initcap(US.CNAME)",sep="")
    }
    
    
    
    
    
    availSpp = oracle_cxn$thecmd(oracle_cxn$channel, species.query)
    if (!is.null(spp)){
      if (spp %in% availSpp$SPEC){
        sppChoice <- spp
      }else{
        cat("\nThe provided species code is unknown, or not available for the selected 
sex option.  Please select one from the list.\n")
        sppChoice <-NA
      }
    } 
    
    while (is.na(sppChoice)){
      sppChoice = utils::select.list(paste( availSpp$CNAME, " (", availSpp$SPEC,")",sep=""),
                                     multiple=F, graphics=T, 
                                     title=ifelse(isTRUE(bySexChoice),
                                                  "Choose a (sexed) species:",
                                                  "Choose a species"))
      #got species selection - extract code
      sppChoice<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', sppChoice)) 
      
    }
    sppChoice = availSpp[availSpp$SPEC %in% sppChoice,]
    res= list(bySexChoice,sppChoice,ageBySexChoice)
    return(res)
  }
  
  getStrata<-function(agency, strataTable, strata, towDist, wingspread, dfMissionsStrata, oracle_cxn){
    strataPick<-NA
    addZ<-F
    if (!is.null(strata)) strataPick <-strata
    if (all(nchar(dfMissionsStrata[,1])>3)){
      strata.tweak = "AND LENGTH(STRAT)=5"
      strata.preselect=""
      if (all(nchar(dfMissionsStrata[,1]) ==4)) addZ<-T
    }else if (all(nchar(dfMissionsStrata[,1])==3)){
      strata.tweak = paste0("AND LENGTH(STRAT) = 3 AND SUBSTR(STRAT,1,1) IN ('3','4','5')")
      strata.preselect=as.character(c(440:495))
    }else{
      print("\nWeird strata encountered please report your selections to Mike.McMahon@dfo-mpo.gc.ca")
    }
    sql = paste0("SELECT * FROM ",strataTable, " WHERE 1=1 ", strata.tweak, " ORDER BY STRAT")
    availStrat<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    while(all(!strataPick %in% availStrat$STRAT)){
      strataPick <- utils::select.list(as.character(availStrat$STRAT),
                                       multiple=T, graphics=T, preselect = strata.preselect,
                                       title='Please choose the strata:')
      if (all(!strataPick %in% availStrat$STRAT)) print("You must select the strata")
    }
    
    dfStrata = availStrat[availStrat$STRAT %in% strataPick,]
    
    if (addZ) dfStrata[,"STRAT"]<-paste0(0,dfStrata[,"STRAT"])
    #calculate strat areas into tunits; US nautical mile is 6080.2ft
    sql2<- paste("SELECT strat, area SQNM, 
                  nvl(area,0)/(",towDist,"*(",wingspread,"/6080.2)) tunits 
          FROM ",strataTable,"
          WHERE 
          strat IN (",Mar.utils::SQL_in(dfStrata[,1]),")
           ORDER BY strat", sep="")
    dfStrata.det<-oracle_cxn$thecmd(oracle_cxn$channel, sql2)
 
    if (agency=="NMFS"){
      dfStrata.det$TUNITS<-dfStrata.det$SQNM/(towDist*1.39798033) ########US seems to just use towDist... the 1.39 is the consistent difference between the TUNITS stranal is puting out and what Stratisfy has when it uses just towDist. US Helper.R code only using towDist (0.007), which implies that their tow is only 1 nautical mile and not 1.3979. I can't get a clarification from the US about which it is so for now going with what STRANAL has been doing...
    }
    if (addZ) dfStrata.det[,"STRAT"]<-paste0(0,dfStrata.det[,"STRAT"])
    dfStrata.det= merge(dfStrata, dfStrata.det)
    dfStrata.det<-dfStrata.det[order(dfStrata.det$STRAT),] 
    return(dfStrata.det)
  }
  getAreas<-function(agency, missions, strata, areas, oracle_cxn){
    areasPick<-NA
    if (agency =="DFO"){
      sql <- paste0(
        "SELECT DISTINCT AREA FROM GROUNDFISH.GSINF
        WHERE
        STRAT IN (",Mar.utils::SQL_in(strata),") AND
        MISSION IN (",Mar.utils::SQL_in(data.frame(missions)[,1]),") AND
        AREA IS NOT NULL
        ORDER BY AREA"
      )
      
    } else if (agency=="NMFS") {
      
      sql <- paste0(
      "SELECT DISTINCT AREA FROM USNEFSC.USS_STATION
      WHERE
      STRATUM IN (",Mar.utils::SQL_in(strata),") AND
      CRUISE6 IN (",Mar.utils::SQL_in(data.frame(missions)[,1]),") AND
      AREA IS NOT NULL
      ORDER BY AREA"
      )
    }
    availAreas<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    if (is.null(areas) & nrow(availAreas)>0){
      while(all(!areasPick %in% availAreas$AREA)){
        areasPick <- utils::select.list(as.character(availAreas$AREA),
                                        multiple=T, graphics=T, preselect = as.character(availAreas$AREA),
                                        title='Please choose the areas:')
        if (all(!areasPick %in% availAreas$AREA)) print("You must select the areas")
      }
    } else if ("all" %in% areas){
      areasPick<-"all"
    } else {
      areasPick <-areas
    }
    return(areasPick)
  }
  switch(requested, 
         "agency" = getAgency(agency), 
         "type" = getType(agency, type, oracle_cxn),
         "spp" = getSpp(agency, spp, bySex, ageBySex, oracle_cxn),
         "missionsAndStrata" = getMissionsAndStrata(agency, type, year, season, 
                                                    missions, oracle_cxn),
         "strataTable" = getStrataTable(strataTable, dfMissionsStrata, oracle_cxn),
         "strata" = getStrata(agency, strataTable, strata, towDist, wingspread, 
                              dfMissionsStrata, oracle_cxn),
         "areas" = getAreas(agency, missions, strata, areas, oracle_cxn),
         "wingspread" = getWingspread(agency,wingspread),
         "towDist" = getTowDist(towDist)
  )
}
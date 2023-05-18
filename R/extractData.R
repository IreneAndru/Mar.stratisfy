# @title extractData
# @description This function doesn't do anything yet.
# @param requested  The default value is \code{NULL}. This determines which 
# analytic will be performed.
# @param agency  The default value is \code{NULL}. 
# @param dfSpp  The default value is \code{NULL}.
# @param type  The default value is \code{NULL}.
# @param missions  The default value is \code{NULL}.
# @param strata  The default value is \code{NULL}.
# @param bySex  The default value is \code{NULL}.
# @family Gale-force
# @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @keywords internal
extractData<-function(requested = NULL, agency = NULL, dfSpp = NULL, type=NULL,
                      missions = NULL, strata = NULL, areas= NULL, bySex = NULL, 
                      oracle_cxn = NULL, useBins=T){
  areaTweak = ""
  
  getCatch<-function(agency, dfSpp, missions, strata, areas){
    spp=dfSpp$SPEC
    if (agency =="DFO"){
      if (!("all" %in% areas))areaTweak = paste0("I.AREA IN (",Mar.utils::SQL_in(areas),") AND")
      sql <-
        paste("select C.mission,C.setno,C.size_class,C.totwgt,C.sampwgt,C.totno,C.calwt
                from
                groundfish.gscat c,
                groundfish.gsinf i
                where
                C.MISSION = I.Mission AND
                C.SETNO = I.SETNO AND
                C.SPEC =", spp," AND
                ",areaTweak,"
                C.mission IN (",Mar.utils::SQL_in(missions),") AND 
                I.STRAT IN (",Mar.utils::SQL_in(strata),") 
                ORDER BY C.mission,C.setno
                ", sep="")
    }else if (agency=="NMFS"){
      if (!("all" %in% areas))areaTweak = paste0("S.AREA IN (",Mar.utils::SQL_in(areas),") AND")
      #adding fake values for calwt(0), size_class(1), and sampwgt(0) so data format matches CDN data
      sql <-
        paste("select I.cruise6 mission,to_number(I.station) setno, 1 size_class, sum(I.expcatchwt) totwgt, 0 sampwgt, sum(I.expcatchnum) totno, 0 calwt
                from
                usnefsc.uss_catch I,
                usnefsc.uss_station S
                WHERE
                I.ID = S.ID AND 
                ",areaTweak,"
                to_number(I.svspp)=", spp," AND
                I.cruise6 IN (",Mar.utils::SQL_in(missions),") AND
                I.STRATUM   IN (",Mar.utils::SQL_in(strata),")
                group by
                I.cruise6, to_number(I.station)
                ORDER BY I.cruise6, to_number(I.station)", sep="")
    }
    raw.gscat<-oracle_cxn$thecmd(oracle_cxn$channel, sql)
    if (nrow(raw.gscat)<1) stop("Error: No catch data can be found for your selection")
    return(raw.gscat)
  }
  getInf<-function(agency, missions, strata, type, areas){
    
    if (!("all" %in% areas))areaTweak = paste0("I.AREA IN (",Mar.utils::SQL_in(areas),") AND")
    if (agency=='DFO'){
      sql<-
        paste("select i.mission, i.setno,i.sdate,i.time,i.strat,
          i.area unit_area,
i.slat,
i.slong,
i.dmin,i.dmax,i.depth, i.dur,i.dist
          from 
          groundfish.gsinf i 
          where 
          ",areaTweak,"
          i.MISSION IN (",Mar.utils::SQL_in(missions),") AND
          i.strat IN (",Mar.utils::SQL_in(strata),") AND 
          i.type IN (",type,")
          ORDER BY i.mission, i.setno", sep="")
    }else if (agency=="NMFS"){
      #distance was assumed to be 1.75, but appears to be dopdistb
      sql<-
        paste("SELECT I.CRUISE6 mission,to_number(I.station) setno, i.begin_est_towdate sdate, i.est_time time, i.STRATUM strat, 
          i.area unit_area,  i.BEGLAT slat, i.BEGLON slong, i.mindepth dmin, i.maxdepth dmax, i.avgdepth depth, i.towdur dur, i.dopdistb dist, e.area_swept_wings_mean_km2 asw
          FROM USNEFSC.USS_STATION I,
          USNEFSC.USS_TOW_EVALUATION E
          WHERE
          ",areaTweak,"
          i.CRUISE6 IN (",Mar.utils::SQL_in(missions),") AND
          i.STRATUM IN (",Mar.utils::SQL_in(strata),") AND
          I.CRUISE6=E.cruise6 AND
          I.STATION=E.STATION AND
          to_number(I.SHG) <= ",type," 
          ORDER BY I.CRUISE6,to_number(I.station)", sep="")
    }
    

    raw.gsinf<-oracle_cxn$thecmd(oracle_cxn$channel, sql )
    if (agency=="NMFS") raw.gsinf$DIST<-raw.gsinf$ASW*(0.539957^2) #Converting the DIST variable for the US data into nautical miles for application
   
      raw.gsinf$SLAT = (as.numeric(substr(raw.gsinf$SLAT,1,2))+(raw.gsinf$SLAT - as.numeric(substr(raw.gsinf$SLAT,1,2))*100)/60)
      raw.gsinf$SLONG = (as.numeric(substr(raw.gsinf$SLONG,1,2))+(raw.gsinf$SLONG - as.numeric(substr(raw.gsinf$SLONG,1,2))*100)/60)*-1
   
    return(raw.gsinf)
  }
  getDet<-function(agency, missions, strata, dfSpp, bySex, type, areas, useBins){
    spp=dfSpp$SPEC
    sppLgrp = dfSpp$LGRP
    #orig stratisfy is inconsistent with berried females:
    # -if analysis is done by sex, berried females (fsex = 3) are ignored
    # -if analysis is done not by sex, they are included
    # The following block does this so that the results match, but it seems 
    # like a bad idea
    if(bySex){
      sexFilt = " (d.FSEX IS NULL OR d.fsex IN (0,1,2)) AND "
    }else{
      sexFilt = ""
    }
    if (agency=='DFO'){
      if (useBins){
        binIt = paste("decode(",sppLgrp,",1,d.flen,2,.5+2*floor(d.flen/2),3,1+3*floor(d.flen/3),flen) flen")
      }else{
        binIt = "d.flen"
      }
      # APL did NOT limit det results by strata, or type
      sql<-paste("select d.mission,d.setno,d.size_class,
      nvl(d.fsex,0) fsex,d.age, d.fwt,         
       ",binIt,",          
       s.lgrp binwidth,s.lfsexed bysex, d.clen          
       from          
       groundfish.gsdet d, groundfish.gsspec s, groundfish.gsinf i 
       WHERE 
       s.spec=d.spec AND  
       d.MISSION = i.MISSION AND
       d.SETNO = i.SETNO AND 
      -- (d.FSHNO IS NOT NULL OR d.FWT IS NOT NULL) AND
       d.MISSION IN (",Mar.utils::SQL_in(missions),") AND 
       -- i.strat IN (",Mar.utils::SQL_in(strata),") AND
       -- i.area IN (",Mar.utils::SQL_in(areas),") AND
       -- i.type IN (",type,") AND
       ",sexFilt,"
       d.SPEC=",spp," 
       ORDER BY d.mission,d.setno", sep="")

      raw.gsdet<-oracle_cxn$thecmd(oracle_cxn$channel, sql )
      #since berried and normal females get combined, add the CLEN for sets
      # raw.gsdet = 
      #   stats::aggregate(list(CLEN = raw.gsdet$CLEN),
      #             by = list(MISSION=raw.gsdet$MISSION,
      #                       SETNO =raw.gsdet$SETNO,
      #                       SIZE_CLASS =raw.gsdet$SIZE_CLASS,
      #                       FSEX =raw.gsdet$FSEX,
      #                       #AGE =raw.gsdet$AGE,
      #                       #FWT =raw.gsdet$FWT,
      #                       FLEN =raw.gsdet$FLEN,
      #                       BINWIDTH =raw.gsdet$BINWIDTH,
      #                       BYSEX =raw.gsdet$BYSEX
      #                       ),
      #             FUN = sum, na.rm=TRUE)
      
    }else if (agency=="NMFS"){
      if (useBins){
        binIt = paste("decode(", sppLgrp,",1,length,2,.5+2*floor(length/2),3,1+3*floor(length/3),length) flen")
      }else{
        binIt = "length flen"
      }
      #missing fsex, sizeclass,clen
      sql1<- paste("select cruise6 mission, to_number(station) setno, length, 
        age, avg(indwt) fwt,
        ",binIt,",
        ",sppLgrp," binwidth
        from usnefsc.uss_detail I
        where 
        to_number(svspp)=",spp," AND
        CRUISE6 IN (",Mar.utils::SQL_in(missions),") AND 
        STRATUM IN (",Mar.utils::SQL_in(strata),") 
        group by cruise6,station,age,length
        ORDER BY cruise6, to_number(station)", sep="")
      raw.gsdet1<-oracle_cxn$thecmd(oracle_cxn$channel, sql1 )
      
      sql2<- paste("select cruise6 mission, catchsex fsex, to_number(station) setno,length, 
        sum(expnumlen) clen, 1 size_class
        from usnefsc.uss_lengths
        where to_number(svspp)=",spp,"
        and cruise6 in (",Mar.utils::SQL_in(missions),")
        and catchsex in ('0','1','2')
        group by cruise6,station,length, catchsex
        ORDER BY cruise6, to_number(station)",sep="")
      
      raw.gsdet2<-oracle_cxn$thecmd(oracle_cxn$channel, sql2 )
      raw.gsdet<-merge(raw.gsdet1,raw.gsdet2, all.x=T) 
      raw.gsdet$FLEN[is.na(raw.gsdet$FLEN)] <- raw.gsdet$LENGTH[is.na(raw.gsdet$FLEN)]
    }
    return(raw.gsdet)
  }
  
  switch(requested, 
         "catch" = getCatch(agency,dfSpp,missions, strata, areas), 
         "inf" = getInf(agency, missions, strata, type, areas),
         "det" = getDet(agency, missions, strata, dfSpp, bySex, type, areas, useBins)
  )
}
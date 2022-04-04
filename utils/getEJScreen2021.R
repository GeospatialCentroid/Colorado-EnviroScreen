

getEjScreen2021 <- function(geometry,processingLevel, overWrite=FALSE){
  
    pathToData <- paste0("data/EJScreen/2021/ejscreen_",processingLevel,".csv")
    if(file.exists(pathToData) & overWrite == FALSE){
      geom <- vroom::vroom(pathToData)
      return(geom)
    }else{
      d1 <- vroom::vroom("data/EJScreen/2021/EJSCREEN_2021_StatePctile.csv")%>%
        dplyr::filter(STATE_NAME == "Colorado") %>%
        dplyr::select(
          GEOID = ID,
          peopleOfColor = MINORPCT,
          lowIncome = LOWINCPCT,
          highSchool = LESSHSPCT,
          linguisticIsolation = LINGISOPCT,
          under5 = UNDER5PCT,
          over64 = OVER64PCT,
          leadPaint = PRE1960PCT,
          deiselPM = DSLPM, 
          trafficeProx = PTRAF,
          waterDischarge = PWDIS,
          nplProx = PNPL,
          rmpProx = PRMP, 
          tsdfProx = PTSDF
        )
      
      # process based on geometry level 
      if(nchar(geometry$GEOID[1])==5){
        i <- 5
      }
      if(nchar(geometry$GEOID[1])==11){
        i <- 11
      }
      if(nchar(geometry$GEOID[1])==12){
        i <- 12
      }
      # processing based on the length of GEOID object
      geom <-  d1 %>%
        dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = i)) %>%
        dplyr::group_by(GEOID) %>%
        dplyr::summarise(
          peopleOfColor = mean(peopleOfColor,na.rm=TRUE),
          lowIncome = mean(lowIncome,na.rm=TRUE),
          highSchool = mean(highSchool,na.rm=TRUE),
          linguisticIsolation = mean(linguisticIsolation,na.rm=TRUE),
          under5 = mean(under5,na.rm=TRUE),
          over64 = mean(over64,na.rm=TRUE),
          leadPaint = mean(leadPaint,na.rm=TRUE),
          deiselPM = mean(deiselPM,na.rm=TRUE), 
          trafficeProx = mean(trafficeProx,na.rm=TRUE),
          waterDischarge = mean(waterDischarge,na.rm=TRUE),
          nplProx = mean(nplProx,na.rm=TRUE),
          rmpProx = mean(rmpProx,na.rm=TRUE), 
          tsdfProx = mean(tsdfProx,na.rm=TRUE)
        )
      write_csv(x = geom, file = pathToData)
      return(geom)
    }
  
}
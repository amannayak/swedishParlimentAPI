#' This class contain an API call to fetch Swedish Parliament members details.
#' @param startD startDate
#' @param endD endDate
#' @return list of Politicians based on \code{startD} and \code{endD}
#' @examples
#' \dontrun{
#' a = parliamentAPI()
#' a$CalData(startD , endD)
#'}
#'@details
#'Swedish Parliament API is called in order to fetch members details from 1990 to current date
#'Same Class is furthur used in making Shiny Dashborad to show details on UI
#'@export parliamentAPI
#'@import RJSONIO
#'@import RCurl
#'@import RSQLite
#'@import odbc

parliamentAPI = setRefClass("parliamentAPI",
  fields = list(
    iDate = "character",
    personalInfoDF = "data.frame",
    postiondTimedf = "data.frame"
  ),#fields = list(
  
  methods = list(
    initialize = function(){
      library(RJSONIO)
      library(RCurl)
      
      #Form URL with inserted parameters 
      #URL Can be updated based on type of paramerters Passed to it
      personalDetailsURL = "http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=samtliga&org=&utformat=json&sort=sorteringsnamn&sortorder=asc&termlista="
      
      #handel for no call error
      
      pInfoJson = getURL(personalDetailsURL)
      persoIOjson= RJSONIO::fromJSON(pInfoJson)
      
      #initialize all parameters to Null
      id = firstName = lastName = sex = yearOfBirth = position = constituency = party = NULL
      pId = pStart = pEnd = NULL
      k = 1;
      for(i in 1:length(persoIOjson$personlista$person))
      {
        
        if(!is.null(persoIOjson$personlista$person[[i]]$intressent_id))
          id[i] = persoIOjson$personlista$person[[i]]$intressent_id
        
        if(!is.null(persoIOjson$personlista$person[[i]]$tilltalsnamn))
          firstName[i] = persoIOjson$personlista$person[[i]]$tilltalsnamn
        
        if(!is.null(persoIOjson$personlista$person[[i]]$efternamn))
          lastName[i] = persoIOjson$personlista$person[[i]]$efternamn
        
        if(!is.null(persoIOjson$personlista$person[[i]]$kon))
          sex[i] = persoIOjson$personlista$person[[i]]$kon
        
        if(!is.null(persoIOjson$personlista$person[[i]]$fodd_ar))
          yearOfBirth[i] = persoIOjson$personlista$person[[i]]$fodd_ar
        
        if(!is.null(persoIOjson$personlista$person[[i]]$status))
          position[i] = persoIOjson$personlista$person[[i]]$status
        
        if(!is.null(persoIOjson$personlista$person[[i]]$valkrets))
          constituency[i] = persoIOjson$personlista$person[[i]]$valkrets
        
        if(!is.null(persoIOjson$personlista$person[[i]]$parti))
          party[i] = persoIOjson$personlista$person[[i]]$parti
        
        
        for(j in 1:length(persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag))
        {
          if(!is.null(persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$intressent_id))
            pId[k] = persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$intressent_id
          
          if(!is.null(persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$from))
            pStart[k] = persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$from
          
          if(!is.null(persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$tom))
            pEnd[k] = persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]$tom
          
          k= k+1
        }#for(j in 1:length(persoIOjson$personlista$person[[i]]$personuppdrag$uppdrag[[j]]))
        
      }#for(i in 1:length(persoIOjson$personlista$person))
      
        #primary info details
        personalInfoDF_Sample = data.frame(id, firstName, lastName, sex, yearOfBirth, position, constituency, party)
        
        #filter NA from Ids
        personalInfoDF <<- personalInfoDF_Sample[!is.na(personalInfoDF_Sample$id),] 
        
        #position hold and time periods
        postiondTimedf_Sample = data.frame(pId , pStart , pEnd)
        #filter NA from pId
        postiondTimedf <<- postiondTimedf_Sample[!is.na(postiondTimedf_Sample$pId),]
        return(.self)
    },#initialize = function(){
    
    CalData = function(startD, endD){
      
      library(RSQLite)
      library(odbc)
      con = dbConnect(RSQLite::SQLite(),":memory:")
      
      #dbExecute is preferred in case that the query results are not of interest
      #Create Table MemberInfo 
      dbExecute(con , "CREATE TABLE MemberInfo(
                  id text,
                  firstName text,
                  lastName text,
                  sex text,
                  yearOfBirth int,
                  position text,
                  constituency text,
                  party text,
                  PRIMARY KEY (id)
                  )"
      )#dbExecute
      
      #Create Table Tenure
      dbExecute(con , "CREATE TABLE Tenure(
                  pId text,
                  pStart datetime,
                  pEnd datetime,
                  FOREIGN KEY (pId) REFERENCES MemberInfo (id)
                  )"
      )#dbExecute
      
      dbWriteTable(con, "MemberInfo" , personalInfoDF , append = TRUE, row.names = FALSE)
      dbWriteTable(con, "Tenure", postiondTimedf , append = TRUE, row.names = FALSE)
      
      startD = as.character(startD)
      endD = as.character(endD)
      total_Member = dbGetQuery(con , sprintf("select distinct  a.id, a.firstName, a.sex from MemberInfo a , Tenure b where a.id = b.pId and 
                  b.pStart >= '%s' and b.pEnd <= '%s' order by a.firstName", startD , endD))
      totalFemale = total_Member[total_Member$sex == "kvinna",] 
      totalMen = total_Member[total_Member$sex == "man",]
      count_TF  = length(totalFemale[,1])
      count_TM  = length(total_Member[,1])
      count_men = count_TM - count_TF
      
      dbDisconnect(con)
      
      rList = list(total_Member , totalFemale , totalMen ,count_TM, count_TF , count_men)
      names(rList) = c("AllMembers" , "FemaleMembers" , "MaleMembers" , "CountAll", "CountFemale" , "CountMan")
      return(rList)
    }#CalData = function(){
  )#methods = list(
)#parliamentAPI = setRefClass(
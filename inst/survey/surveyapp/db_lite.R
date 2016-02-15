

use_package <- function(p) {
  if (!is.element(p, installed.packages()[,1])){  install.packages(p, dep = TRUE)}
  require(p, character.only = TRUE)
}

use_package("sqldf")
use_package("dplyr")
use_package("quantmod")
use_package("lubridate")

lite_add_df <- function(mydf,tbl='badd'){
  RSQLite::dbWriteTable(
    conn=RSQLite::dbConnect(SQLite(),dbname=SQLITE_DB), name=tbl,value=mydf
  )

}

lite_run_msql <- function(qry){

  require(RODBC)
  stats_con <- NULL
  computer<- Sys.info()["nodename"]
  if(computer=='BEAMAPC'){
    stats_con = 'Driver={SQL Server Native Client 10.0};server=tcp:83.217.99.98,1433;database=BEAMAstatistics;uid=bss;pwd=_badd?$_AAEJEAS_1234567;Connection Timeout=120;'
  }else if(computer=='WBSERVER'){
    stats_con = 'Driver={ODBC Driver 11 for SQL Server};server=wbserver;database=BEAMAstatistics;Trusted_Connection=Yes;Connection Timeout=120;'
  }else{
    stats_con = 'Driver={ODBC Driver 11 for SQL Server};server=wbserver;database=BEAMAstatistics;Trusted_Connection=Yes;Connection Timeout=120;'
  }

  dbhandle <- odbcDriverConnect(stats_con)

  mydf<-sqlQuery(dbhandle,qry)
  close(dbhandle)
  return(mydf)
}

lite_move_to_sqlite <- function(tbl){

  mydf<- lite_run_msql(paste0("select * from ",tbl))
  lite_add_df(mydf=mydf,tbl=tbl)
}

lite_run_sql <- function(qry){
  db <- dbConnect(RSQLite::SQLite(),dbname=SQLITE_DB)
  results <- DBI::dbGetQuery(db, qry)
  dbDisconnect(db)
  return(results)
}

lite_run_tsql <- function(qry){
  results <- sqldf::sqldf(qry,dbname=SQLITE_TRD)
  return(results)
}

lite_split_sql <- function(qry="CHAY,CHAW,D7BT"){
  abc <- base::gsub(",","','",qry)
  abc <- base::paste0("('",abc,"')")
  return(abc)
}

lite_str_pos<-function(x,pattern=","){
  abc <- gregexpr(pattern =pattern,x)
  return(abc[[1]][1])
}

lite_trim <- function(x){
  return(gsub("^\\s+|\\s+$","",x))
}

lite_ts_dates <- function(myts){
  if(frequency(myts)==12){
    return(
      seq(
        as.Date(paste(c(start(myts),28), collapse = "/")),
        by = "month",
        length.out = base::length(myts)
      )
    )
  }else if(frequency(myts)==4){
    return(
      seq.Date(
        as.Date(paste(start(myts)[1],start(myts)[2]*3,28,sep="/")),
        length.out = base::length(myts),
        by="3 months"
      )
    )
  }else if(frequency(myts)==1){
    return(
      seq(
        as.Date(paste(c(start(myts),1), collapse = "/")),
        by = "year",
        length.out = base::length(myts)
      )
    )

  }else{
    stop("Frequency of time series UNKNOWN")
  }
}

lite_ts_years<-function(myts){
  mydates <- lite_ts_dates(myts)
  return(substring(mydates,1,4))
}

lite_ts_months<-function(myts){
  mydates <- lite_ts_dates(myts)
  return(substring(mydates,6,7))
}

lite_ts_format <- function(myts){
  mydata <- cbind(yr=lite_ts_years(myts),mths=lite_ts_months(myts),dy=c(1),value=myts)
  return(mydata)
}

lite_ts_get_properties<-function(x){

  if(!(base::class(x)=="ts")){
    stop("Aimless command. I need time series object. You need start your life all over again")
  }

  x.freq <- stats::frequency(x)
  x.period <- "month"

  if(x.freq==4){
    x.period <- "quarter"
  }
  x.end<- stats::end(x)
  x.end.date <- lubridate::ymd(paste(x.end[1],x.end[2],1,sep="-"))
  x.end.date.prv <- base::as.POSIXlt(x.end.date)
  x.end.date.prv$mon <- x.end.date.prv$mon-1
  return(
    base::list(
      frq=x.freq,
      period=x.period,
      enddate = x.end.date,
      enddate_1 = x.end.date.prv
    )
  )
}

lite_ts_position <- function(x){
  if( (x %in% 4:20) ){
    return (paste0(x,'th'))
  }else{
    switch(x%%10,
           return( paste0(x,'st')), #case 1
           return( paste0(x,'nd')), #case 2
           return( paste0(x,'rd'))  #case 3
    )
  }
  return(paste0(x,'th'))  #default
}

lite_ts_get_successive<-function(x,xname="RPI",xmonth=end(x),k_prd=1){
  #x=lite_trends_get_dfts('copi_total_rmi',full_yr=2014);xname="Turnover";xmonth=end(x);k_prd=12
  mtm <- lite_ts_get_properties(x)
  mtm.data <- base::round(Delt(x,k=k_prd)*100,1)
  mtm.data <- tseries::na.remove(mtm.data)

  mtm.len <- base::length(mtm.data)
  #mtm.sc <- mtm.data[mtm.len]>0

  mtm.pos <- as.character(seq(1:20))#
  mtm.sc.msg <- ""
  mtm.sc <- mtm.data[mtm.len]>0
  mtm.sc.text <- "growth"
  mtm.sc.text.rev <- "decline"


  if(mtm.sc==FALSE){
    mtm.sc.text <- "decline"
    mtm.sc.text.rev <- "growth"
  }


  mtm.checks <- NULL
  mtm.checks <- ( mtm.data>0)

  i <- mtm.len-1
  mtm.counter <- 1

  while((mtm.checks[i]==mtm.sc) && (i>0) ){
    mtm.counter<-mtm.counter+1
    i <- i-1
  }

  mtm.counter2 <- NULL
  if(mtm.counter==1){
    mtm.sc2 <- mtm.data[mtm.len-1]>0

    i <- mtm.len-2
    mtm.counter2 <- 1

    while(mtm.checks[i]==mtm.sc2 && (i>0)){
      mtm.counter2<-mtm.counter2+1
      i <- i-1
    }
  }

  prd <- paste0(mtm$period,"ly ")

  if(!(k_prd ==1)){   prd <- "year on year "  }

  if(mtm.counter>1){
    mtm.sc.msg <- paste0(
                         "It is the ",
                         lite_ts_position(mtm.counter),
                         " successive ",
                         prd,
                         mtm.sc.text,
                         "."
                      )
  }else{
    if(mtm.counter2>1){
      mtm.sc.msg <- paste0("It is the first ",prd,mtm.sc.text," after ",mtm.counter2," successive ",mtm.sc.text.rev,"s.")
    }
  }

  return(mtm.sc.msg)
}

lite_ts_get_mtm <- function(x,xname="RPI",xmonth=end(x)){

  mtm <- lite_ts_get_properties(x)
  mtm.data <- base::round(Delt(x,k=1)*100,1)
  mtm.len <- base::length(mtm.data)


  ###
  mtm.target <- NULL
  mtm.prv <- NULL

  if(mtm$period=="month"){
    mtm.target <- lubridate::ymd(paste(xmonth[1],xmonth[2],1,sep="-"))
    #mtm.prv <- NULL

    if(xmonth[2]==1){
      mtm.prv <- lubridate::ymd(paste(xmonth[1]-1,12,1,sep="-"))
    }else{
      mtm.prv <- lubridate::ymd(paste(xmonth[1],xmonth[2]-1,1,sep="-"))
    }
  }else if(mtm$period=="quarter"){
    mtm.target <- lubridate::ymd(paste(xmonth[1],xmonth[2]*3,1,sep="-"))
    if(xmonth[2]==1){
      mtm.prv <- lubridate::ymd(paste(xmonth[1]-1,12,1,sep="-"))
    }else{
      mtm.prv <- lubridate::ymd(paste(xmonth[1],3*(xmonth[2]-1),1,sep="-"))
    }

  }

  mtm.target.text <- paste0(as.character(lubridate::month(mtm.target,label=TRUE,abbr=FALSE))," ",year(mtm.target))
  mtm.prv.text <- paste0(as.character(lubridate::month(mtm.prv,label=TRUE,abbr=FALSE))," ",year(mtm.prv))

  mtm.window <- NULL
  if(mtm$period=="month"){
    mtm.window <- window(
      mtm.data
      ,start=c(year(mtm.prv),month(mtm.prv))
      ,end=c(year(mtm.target),month(mtm.target))
    )
  }else if(mtm$period=="quarter"){
    mtm.window <- window(
      mtm.data
      ,start=c(year(mtm.prv),month(mtm.prv)/3)
      ,end=c(year(mtm.target),month(mtm.target)/3)
    )
  }

  mtm.discard.sc <- FALSE
  mtm.direction <- paste0("grew by ",mtm.window[2],"%.")

  if(mtm.window[2]< 0 ){
    mtm.direction <- paste0("fell by ",mtm.window[2],"%.")
  }

  if(mtm.window[2] >=-0.099999 && mtm.window[2]<=0.099999 ){
    mtm.direction <- paste0("remained unchanged.")
    mtm.discard.sc <- TRUE
  }


  mtm.broadcast <- paste0(
    "Between ",mtm.prv.text," and ",mtm.target.text," "
    ,xname," "
    ,mtm.direction
  )

  return(list(desc=mtm.broadcast,MoM=mtm.window[2],extra=mtm.discard.sc))
}

lite_ts_get_yoy<-function(x,xname="RPI",xmonth=end(x)){

  #x=abc;xname="RPI";xmonth=c(2013,9)
  yoy <- lite_ts_get_properties(x)
  yoy.data <- base::round(Delt(x,k=yoy$frq)*100,1)
  yoy.len <- base::length(yoy.data)

  yoy.target<- NULL
  yoy.prv <- NULL

  if(yoy$period=="month"){
    yoy.target <- lubridate::ymd(paste(xmonth[1],xmonth[2],1,sep="-"))

    if(xmonth[2]==1){
      yoy.prv <- lubridate::ymd(paste(xmonth[1]-1,12,1,sep="-"))
    }else{
      yoy.prv <- lubridate::ymd(paste(xmonth[1],xmonth[2]-1,1,sep="-"))
    }
  }else if(yoy$period=="quarter"){
    yoy.target <- lubridate::ymd(paste(xmonth[1],xmonth[2]*3,1,sep="-"))

    if(xmonth[2]==1){
      yoy.prv <- lubridate::ymd(paste(xmonth[1]-1,12,1,sep="-"))
    }else{
      yoy.prv <- lubridate::ymd(paste(xmonth[1],3*(xmonth[2]-1),1,sep="-"))
    }

  }

  yoy.target.text <- paste0(as.character(lubridate::month(yoy.target,label=TRUE,abbr=FALSE))," ",year(yoy.target))
  yoy.prv.text <- paste0(as.character(lubridate::month(yoy.prv,label=TRUE,abbr=FALSE))," ",year(yoy.prv))
  yoy.direction <- "grew by"
  yoy.direction.prv <- "up from"
  yoy.window <- NULL

  if(yoy$period=="month"){
    yoy.window <- window(
      yoy.data
      ,start=c(year(yoy.prv),month(yoy.prv))
      ,end=c(year(yoy.target),month(yoy.target))
    )
  }else{
    yoy.window <- window(
      yoy.data
      ,start=c(year(yoy.prv),month(yoy.prv)/3)
      ,end=c(year(yoy.target),month(yoy.target)/3)
    )

  }

  yoy_discard_extra <- FALSE
  if(yoy.window[2]< 0 ){yoy.direction <- "fell by"}
  if(yoy.window[2]< yoy.window[1] ){yoy.direction.prv <- "down from"}

  if(yoy.window[2] >=-0.099999 && yoy.window[2]<=0.099999 ){
    yoy.direction <- "remained unchanged"
    yoy_discard_extra <- TRUE
  }

  extra <- paste0(
     ", "
    ,yoy.direction.prv," "
    ,yoy.window[1],"% in "
    ,yoy.prv.text
  )

  yoy.broadcast <- paste0(
    xname," "
    ,yoy.direction," "
    ,ifelse(yoy_discard_extra,"",paste0(yoy.window[2],"% "))
    ," in the year to "
    ,yoy.target.text,
    ifelse(yoy_discard_extra,"",extra)
  )

  return(list(desc=yoy.broadcast,YoY=yoy.window[2],extra=yoy_discard_extra))

}

db_lite_simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

db_lite_set_decimal <- function(x, k){
   if( !(is.na(x)||missing(x)||is.null(x)) ){
      if(x[1]<200){
        format(round(x, k), nsmall=k,big.mark=",")
      }else {
        format(round(x, 0), nsmall=0,big.mark=",")
      }
    }
}

db_lite_expand_code <- function(code='27',digit=1){
  dg1 <- as.numeric(code)*(10^digit)
  dg2 <- as.numeric(code)*(10^digit)+(10^digit-1)
  result <- paste0(dg1:dg2,collapse=",")
  return(result)
}

lite_clipboard <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

lite_save_current_plot <- function(
  file="glance.png",width=633,height=219,path="W:/reports/latex/images/",ppi=72
){
  ggsave(file=paste0(path,file),height=height/ppi,width=width/ppi,dpi=ppi,units="in")
}

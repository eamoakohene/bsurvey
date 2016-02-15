

lite_get_trends_data <- function(code="CHAY,CHAW",y1=2000,y2=2014,m1=1,m2=12){
  sql <- "select * from trends_data "

  qry_where <-" where "
  qw_code <- base::paste0(" data_code in ",lite_split_sql(code))
  qw_yr <- base::paste0(" and (yr between ",y1," and ",y2,")")
  qw_mth <- base::paste0(" and (mth between ",m1," and ",m2,")")
  sql_where <- base::paste0(qry_where,qw_code,qw_yr,qw_mth)

  sql <- base::paste0(sql,sql_where)
  result <- lite_run_sql(sql)
  return(result)
}

lite_get_trends_data_daily <- function(code="CHAY,CHAW",y1=2000,y2=2014,m1=1,m2=12){
  sql <- "select yr,mth,dy,data_code,data_desc as code,data_value  as value from trends_data "

  qry_where <-" where "
  qw_code <- base::paste0(" data_code in ",lite_split_sql(code))
  qw_yr <- base::paste0(" and (yr between ",y1," and ",y2,")")
  qw_mth <- base::paste0(" and (mth between ",m1," and ",m2,")")
  sql_where <- base::paste0(qry_where,qw_code,qw_yr,qw_mth)

  order_by <- " order by yr,mth,dy "
  sql <- base::paste0(sql,sql_where,order_by)
  result <- lite_run_sql(sql)
  return(result)
}

lite_get_trends_periodicity <- function(code='CHAY',yr=2013){
  sql <- paste0("select count(mth) as mths from trends_data where data_code='",code,"' and yr=",yr )
  result <-lite_run_sql(sql)
  return(result$mths)
}

lite_get_ytd <- function(x){
  n <- nrow(x)
  x <- dplyr::arrange(x,yr,mth)

  ytd <- numeric(n)
  ytd[1] <- x$value[1]

  for(i in 2:n){
    if(x$yr[i]==x$yr[i-1]){
      ytd[i] <- ytd[i-1] + x$value[i]
    }else{
      ytd[i] <-  x$value[i]
    }
  }

  return(ytd)
}

lite_get_trends_data_fx <- function(
  code="CHAY,CHAW",y1=2000,y2=2020,m1=1,m2=12,is_avg=FALSE,
  fx=c('d','m','q','y','mt','qt','yt','ms','qs','ys','mc','qc','yc'),
  pc=c('0','1','3','4','12'),
  fx_level=2
  ){

  sql <- NULL
  group_by<- ""
  order_by <- ""

  myprd <- base::match.arg(fx)
  mypc <- base::match.arg(pc)
  mypc <- as.integer(mypc)
  if(myprd=='q'){

    if(is_avg){
      sql <- "select yr,qtr,data_code,data_desc,avg(data_value)  as value from trends_data "
      group_by <- " group by yr,qtr,data_code,data_desc "
      order_by <- " order by yr,qtr,data_code,data_desc "
    }else{
      sql <- "select yr,qtr,data_code,data_desc,sum(data_value)  as value from trends_data "
      group_by <- " group by yr,qtr,data_code,data_desc "
      order_by <- " order by yr,qtr,data_code,data_desc "
    }

  }else if(myprd=='m'){

    if(is_avg){
      sql <- "select yr,mth,data_code,data_desc,avg(data_value)  as value from trends_data "
      group_by <- "group by yr,mth,data_code,data_desc "
      order_by <- " order by yr,mth,data_code,data_desc "
    }else{
      sql <- "select yr,mth,data_code,data_desc,sum(data_value)  as value from trends_data "
      group_by <- " group by yr,mth,data_code,data_desc "
      order_by <- " order by yr,mth,data_code,data_desc "
    }

  }else if(myprd=='y'){

    if(is_avg){
      sql <- "select yr,data_code,data_desc,avg(data_value)  as value from trends_data "
      group_by <- " group by yr,data_code,data_desc "
      order_by <- " order by yr,data_code,data_desc "
    }else{
      sql <- "select yr,data_code,data_desc,sum(data_value)  as value from trends_data"
      group_by <- " group by yr,data_code,data_desc "
      order_by <- " order by yr,data_code,data_desc "
    }

  }else   if(myprd=='qt'){

    sql <- "select yr,qtr,sum(data_value)  as value from trends_data "
    group_by <- " group by yr,qtr "
    order_by <- " order by yr,qtr "
  }else if(myprd=='mt'){

    sql <- "select yr,mth,sum(data_value)  as value from trends_data "
    group_by <- " group by yr,mth"
    order_by <- " order by yr,mth "

  }else if(myprd=='yt'){

    sql <- "select yr,sum(data_value)  as value from trends_data "
    group_by <- " group by yr "
    order_by <- " order by yr "
  }else if(myprd=='d'){

    sql <- "select yr,mth,dy,data_code,data_desc,data_value  as value from trends_data "
    order_by <- " order by yr,mth,dy,data_code,data_desc "
  }else if (myprd=='ms'){

    sql <- paste0("SELECT yr, mth,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
    group_by <- " group by yr,mth,wrap "

  }else if (myprd=='qs'){

    sql <- paste0("SELECT yr, qtr,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
    group_by <- " group by yr,qtr,wrap "

  }else if (myprd=='ys'){

    sql <- paste0("SELECT yr, substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
    group_by <- " group by yr,wrap "

  }else if (myprd=='mc'){

    sql <- paste0("SELECT yr, mth,(substr(data_code,instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
    group_by <- " group by yr,mth,wrap "

  }else if (myprd=='qc'){

    sql <- paste0("SELECT yr, qtr,(substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data ")
    group_by <- " group by yr,qtr,wrap "

  }else if (myprd=='yc'){

    sql <- paste0("SELECT yr, ( substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
    group_by <- " group by yr,wrap "

  }


  qry_where <-" where "
  qw_code <- base::paste0(" data_code in ",lite_split_sql(code))
  qw_yr <- base::paste0(" and (yr between ",y1," and ",y2,")")
  qw_mth <- base::paste0(" and (mth between ",m1," and ",m2,")")
  sql_where <- base::paste0(qry_where,qw_code,qw_yr,qw_mth)

  sql <- base::paste0(sql,sql_where,group_by,order_by)
  #cat(sql,"\n")
  result <- lite_run_sql(sql)

  if(dim(result)[1]>0){
    result$value <- base::round(base::as.numeric(base::as.character(result$value)),4)

    if((fx=="m") || (fx=='mt')|| (fx=='ms')|| (fx=='mc')){
      result$dy <- 1
      if(fx=='mt'){
        result$data_code <- 'dummy-code'
        result$data_desc <- 'dummy-desc'
      }
      if((fx=="ms")|| (fx=="mc")){
        result$data_code <- result$wrap
        result$data_desc <- result$data_code
      }
    }
    if((fx=="q") || (fx=="qt")|| (fx=="qs")|| (fx=="qc")){
      result$dy <- 1
      result$mth <- result$qtr*3
      if(fx=='qt'){
        result$data_code <- 'dummy-code'
        result$data_desc <- 'dummy-desc'
      }

      if((fx=="qs")|| (fx=="qc")){
        result$data_code <- result$wrap
        result$data_desc <- result$data_code
      }
    }

    if((fx=="y")||(fx=="yt")||(fx=="ys")||(fx=="yc")){
      result$dy <- 1
      result$mth <-12
      if(fx=='yt'){
        result$data_code <- 'dummy-code'
        result$data_desc <- 'dummy-desc'
      }
      if((fx=="ys")|| (fx=="yc")){
        result$data_code <- result$wrap
        result$data_desc <- result$data_code
      }
    }

    result<- dplyr::arrange(result,yr,mth,dy,data_desc)

    result$pc <- NULL
    if(!(mypc==0)){
      result$pc <- base::with( result,stats::ave(value,data_code,FUN=function(x){quantmod::Delt(x,k=mypc)*100}))
    }

  }

  return(result)
}

lite_ons_get_insert_sql <-function(yr="2014",mth="5",dy="1",data_desc='RPI',data_unit='2010=100',data_src='RPI',data_value='1',data_code='badd'){

  sql_insert <-" insert into trends_data (yr,mth,dy,data_desc,data_unit,data_src,data_value,data_code) values "
  sql_values <- paste0("(",yr,",",mth,",",dy,",'",data_desc,"' , '",data_unit,"','",data_src,"',",data_value,",'",data_code,"')")

  return(paste0(sql_insert,sql_values))
}

lite_ons_get_update_sql <-function(yr="2014",mth="5",dy="1",data_unit='2010=100',data_value='1',data_code='badd'){
  sql_update <- "update trends_data "
  sql_value  <- paste0(" set data_value=",data_value)
  sql_where  <- paste0(" where yr=",yr, " and mth=",mth," and dy=",dy," and data_code='",data_code , "' and data_unit='",data_unit,"'")

  return(paste0(sql_update,sql_value,sql_where))
}

##### ons data update
lite_ons_update_data <-function(myts,data_desc='BADD',data_src='BADD',data_unit='2010=BADD',data_code='badd',fx=c('insert','update'),data_points_yrs=1){

  mydata <- NULL
  mydata <- lite_ts_format(myts)
  mydata_n <- nrow(mydata)
  sql<-NULL
  myfx <- match.arg(fx)

  loop_range <- 1:mydata_n

  if(!(data_points_yrs==0)){

    frq <- frequency(myts)
    loop_start<- mydata_n - data_points_yrs*frq
    if(loop_start > 0){
       loop_range <- loop_start:mydata_n
    }
  }

  for(i in loop_range){

    if(myfx=='insert'){
      sql <- lite_ons_get_insert_sql( yr=  mydata[i,1],  mth= mydata[i,2], dy=  mydata[i,3], data_value=mydata[i,4], data_desc=data_desc, data_src=data_src,  data_unit=data_unit,  data_code=data_code )#sql
    }else{
      sql <- lite_ons_get_update_sql( yr=  mydata[i,1],  mth= mydata[i,2], dy=  mydata[i,3], data_value=mydata[i,4], data_unit=data_unit,  data_code=data_code )#sql
    }

    result <- lite_run_sql(sql)
  }#for
}

lite_trends_get_growth_info <- function(code='CHAW',desc_short='RPI',desc_long='Retail Price Index',full_yr=2013,drop_end=0){
  #code='BMTI';desc_short='BMTI';desc_long='BTMI';full_yr=2014;drop_end=0
  myts <- lite_trends_get_dfts(code=code,full_yr=full_yr)

  if(!(drop_end == 0)){

     d <- drop_end
    n2 <- length(myts)
    n1 <- n2-d+1
    drange <- n1:n2
    if( d == 1 ){
      drange <- n2
    }

    myts <- ts( myts[-c(drange)],start=start(myts),frequency = frequency(myts))
  }

  #mom_desc_short <- paste( tolower( substring( desc_short, 1, 1)), substring(desc_short, 2), sep = "")

  mom <- lite_ts_get_mtm(myts,xname=desc_long)
  if(!mom$extra){
    mom$sc <- lite_ts_get_successive(myts,desc_short)
  }

  yoy <- lite_ts_get_yoy(myts,xname=desc_short)
  if(!yoy$extra){
    yoy$sc <- lite_ts_get_successive(myts,desc_short,k_prd=frequency(myts))
  }

  myts_end <- end(myts)
  return(list(mom=mom,yoy=yoy,yr=myts_end[1],mth=myts_end[2]))
}

lite_trends_get_dfts <- function(code='CHAW',full_yr=2013){
  #code='CT1A-NHPUB';full_yr=2013
  myts_df<-lite_get_trends_data_fx(code=code)
  myts_df<- dplyr::arrange(myts_df,yr,mth,dy)

  n <- nrow( myts_df )
  full_yr <- full_yr
  myts_start <- c(myts_df$yr[1],myts_df$mth[1])
  myts_freq <- sqldf(paste0("select count(mth) as mths from myts_df where yr=",full_yr))$mths
  if(myts_freq==4){myts_start[2] <- myts_start[2]/3}
  myts <- ts(myts_df$value,start=myts_start,frequency=myts_freq)
  return(myts)
}


lite_ons_get_raw<-function(
  baseurl="http://www.ons.gov.uk/ons/datasets-and-tables/downloads/csv.csv?dataset=" ,
  dataset="mm23",
  ref="D7BT",
  skipFirst=6,
  skipSecond=0
){


  url=paste(baseurl,dataset,"&cdid=",toupper(ref),sep="")

  data<-try(read.csv(url))
  if(class(data) == "try-error") {

    return("Error encountered during download!")

  }else{

    nrows<-dim(data)[1]
    data<-data[1:(nrows-skipFirst),]
    data[,1]<-as.character(data[,1])
    data[,2]<-as.numeric(as.character(data[,2]))
    data.yr<-data[nchar(data$X)==4,]
    data.qtr<-data[nchar(data$X)==7,]
    data.mth<-data[nchar(data$X)==8,]
    if(skipSecond!=0){
      data.mth <- data.mth[-c(1:skipSecond),]
      data.qtr <- data.qtr[-c(1:skipSecond),]
    }

    return(
      list(data=data,data.yr=data.yr,data.qtr=data.qtr,data.mth=data.mth)
    )#return
  }#else
}#function

lite_ons_get_series_info <- function(code="ABMI"){
  qry <- paste0("select * from ons_data_source where trim(ons_ref)='",code,"'")
  result <- lite_run_sql(qry)
  return (result)
}

lite_ons_download<-function(code="ABMI"){
  #code="CHAW";baseurl="http://www.ons.gov.uk/ons/datasets-and-tables/downloads/csv.csv?dataset="

  mysrc <- NULL
  mydata <- NULL
  url <- NULL
  title<- NULL
  units <- NULL

  try(mysrc <- lite_ons_get_series_info(code),silent=TRUE)
  if(class(mysrc)=="try-error"){ stop("Error reading data reference")}
  mysrc.dim <- dim(mysrc)[2]

  if(mysrc.dim >1){mysrc <- mysrc[1,]}
  mydata<-lite_ons_get_raw(
    dataset   =trim(as.character(mysrc$ons_dataset)),
    ref       =trim(as.character(mysrc$ons_ref)),
    skipSecond=as.vector(unlist(mysrc$ons_skip_rows))
  )


  y.data<-NULL
  if(dim(mydata$data.yr)[1]!=0){
    if(length(mydata$data.yr[,2])>0){
      y.start<-as.integer(substring(mydata$data.yr[1,1],1,4))
      y.data<-ts(mydata$data.yr[,2],start=c(y.start,1),freq=1)
    }
  }

  q.data<-NULL
  if(dim(mydata$data.qtr)[1]!=0){
    if(length(mydata$data.qtr[,2])>0){
      q.start.yr<-as.integer(substring(mydata$data.qtr[1,1],1,4))
      q.start.qtr.desc <- substring(mydata$data.qtr[1,1],6,7)
      q.start.qtr<-1

      if(q.start.qtr.desc=="Q2"){q.start.qtr <- 2}
      if(q.start.qtr.desc=="Q3"){q.start.qtr <- 3}
      if(q.start.qtr.desc=="Q4"){q.start.qtr <- 4}
      q.data<-ts(mydata$data.qtr[,2],start=c(q.start.yr,q.start.qtr),freq=4)

    }
  }

  m.data<-NULL
  if(dim(mydata$data.mth)[1]!=0){
    if(length(mydata$data.mth[,2])>0){
      m.start.yr<-as.integer(substring(mydata$data.mth[1,1],1,4))
      m.start.mth.desc<-substring(mydata$data.mth[1,1],6,8)

      m.start.mth <- 1
      if(m.start.mth.desc=="FEB"){m.start.mth <- 2}
      if(m.start.mth.desc=="MAR"){m.start.mth <- 3}
      if(m.start.mth.desc=="APR"){m.start.mth <- 4}
      if(m.start.mth.desc=="MAY"){m.start.mth <- 5}
      if(m.start.mth.desc=="JUN"){m.start.mth <- 6}
      if(m.start.mth.desc=="JUL"){m.start.mth <- 7}
      if(m.start.mth.desc=="AUG"){m.start.mth <- 8}
      if(m.start.mth.desc=="SEP"){m.start.mth <- 9}
      if(m.start.mth.desc=="OCT"){m.start.mth <- 10}
      if(m.start.mth.desc=="NOV"){m.start.mth <- 11}
      if(m.start.mth.desc=="DEC"){m.start.mth <- 12}

      m.data<-ts(mydata$data.mth[,2],start=c(m.start.yr,m.start.mth),freq=12)
    }
  }

  title<-mysrc$ons_description;names(title)<-NULL
  units<-mysrc$ons_units;names(units)<-NULL


  return(
    list(
      title=title,
      units=units,
      m.data=m.data,
      q.data=q.data,
      y.data=y.data
    )#list
  )#return

}

lite_ons_get_latest<-function(code='ABMI',freq='q',data_desc='',data_unit='',data_point_yrs=1){

  myts <- lite_ons_download(code)

  data <- NULL
  if(freq=='m') {data<- myts$m.data}
  if(freq=='q') {data<- myts$q.data}
  if(freq=='y') {data<- myts$y.data}

  my_desc <- data_desc
  my_unit <- data_unit

  if(data_desc==''){
    qry<- paste0("select * from trends_data where data_code='",code,"' limit 1")
    result<-lite_run_sql(qry)
    my_desc <- result$data_desc[1]
    my_unit <- result$data_unit[1]
  }

  lite_ons_update_data (data,data_desc=my_desc,data_src='ONS',data_unit=my_unit,data_code=code,fx='insert',data_points_yrs = data_point_yrs)
  cat(paste0(code,"-",my_desc, " completed ... \n"))
}
lite_ons_update_qtr_periods<-function(){
  SQ$new(name="trends_update_periods")$qry_exec()

}

lite_ons_get_auto_updates<-function(qry='',dpy=1){
  codes <- NULL
  if(nchar(qry)==0){
    codes <- lite_run_sql(
      "select data_code,data_desc,data_frq,data_unit from trends_meta where is_auto_update=1"
    )
  }else{
    codes <- lite_run_sql(
      paste0("select data_code,data_desc,data_frq,data_unit from trends_meta where is_auto_update=1 ",qry)
    )

  }

  for(i in 1:nrow(codes)){

    lite_ons_get_latest(
       codes$data_code[i],
       ifelse( codes$data_frq[i]==12 ,'m','q'),
       data_desc = codes$data_desc[i],
       data_unit = codes$data_unit[i],
       data_point_yrs = dpy
    )
  }


  lite_ons_update_qtr_periods()
}



trends_meta_add_data <- function(df){
  #df <- hpi_info

  names(df) <- c('code','description')
  sql_base <- "insert into trends_meta (data_code,data_desc) values"
  values <- paste(" ('",df$code,"','",df$description,"'); ",sep="")
  sql <- paste(sql_base,values)
  lapply(sql,FUN=lite_run_sql)

  #trends_lookup_add_data(hpi_info)
  #trends_lookup_add_data(cons_info)
  #trends_lookup_add_data(housing_info)
  #trends_lookup_add_data(jobs_desc)

  #--copi_info <- lite_run_sql("select distinct data_code ,data_desc from trends_data where data_code like 'copi_%'")
  #trends_lookup_add_data(copi_info)
}

trends_growth_add <- function(code,mom,yoy){
  #yr=2015;mth=12;code='abc123';mom='ABCD EFGH CDE';yoy='abcd cde dkdkd'

  SQ$new(
    name='trends_growth_add',
    params=list(
          `@s_code`=code,
          `@s_mom`=mom,
          `@s_yoy`=yoy
    )
  )$qry_exec()


}

trends_growth_add_detail <- function(yr,mth,code,mom,yoy,rank){
  #yr=2015;mth=12;code='abc123';mom='ABCD EFGH CDE';yoy='abcd cde dkdkd'

  SQ$new(
    name='trends_growth_add_detail',
    params=list(
      `@i_yr`=yr,
      `@i_mth` = mth,
      `@s_code`=code,
      `@s_mom`=mom,
      `@s_yoy`=yoy,
      `@i_rank`=rank
    )
  )$qry_exec()


}

trends_growth_get <- function(code){
  return(trends_growth_get_detail(code = code))

}

trends_growth_display <- function(label="",code){

  abc <- trends_growth_get_detail(code = code)
  growth <- NULL

  if(label==""){
      growth <-  shiny::tags$ul(
                   shiny::tags$li(abc$yoy),
                   shiny::tags$li(abc$mom)
                )
  }else{

    growth <-  shiny::tags$div(
                             shiny::tags$b(label),
                             shiny::tags$ul(
                               shiny::tags$li(abc$yoy),
                               shiny::tags$li(abc$mom)
                             )
                 )
  }

  return(growth)

}

trends_growth_get_detail <- function(code,rank=0){

  return(
    lite_run_sql(
      paste0("select data_code as code,data_mom as mom, data_yoy as yoy from growth_data_detail where data_code='",code,"' and data_rank=",rank,";")
    )
  )
}

trends_lookup_get_desc <- function(code){
  #code = 'T216-NI-COMP_AD'
  if(missing(code)){stop("Please supply code")}
  sql <- paste0("select data_desc from trends_meta where data_code='",code,"';")
  abc <- lite_run_sql(sql)
  if(nrow(abc)>0){
    return(abc$data_desc)
  }else{
    stop(paste0(code," not found in database"))
  }
}

trends_growth_update_data_detail <- function(details_level=4,full_yr=2014){
  #trends_growth_update_data_detail()
  df <- lite_run_sql("select data_code,data_growth_desc as desc_short,data_growth_desc2 as desc_long from trends_meta where is_growth=1")
  for(i in 1:nrow(df)){
    #i=1
    code <- df$data_code[i];
    dc <- df$desc_short[i]
    dcl <- df$desc_long[i]
    for(j in 0:0){
      #j=0
      growth <- lite_trends_get_growth_info(code,dc,dc,drop_end = j,full_yr =full_yr )

      mom_desc <- growth$mom$desc
      mom_sc <- growth$mom$sc
      mom_extra <- growth$mom$extra

      yoy_desc <- growth$yoy$desc
      yoy_sc <- growth$yoy$sc
      yoy_extra <- growth$yoy$extra

      trends_growth_add_detail(
         growth$yr,
         growth$mth,
         code,
         paste0(mom_desc,ifelse(!mom_extra,paste0(" ",mom_sc),"")),
         paste0(yoy_desc,ifelse(!yoy_extra,paste0(". ",yoy_sc),"")),
         j
      )
      cat(code," ",growth$yr," ",growth$mth," ",j," update \n")
    }
  }

}

trends_growth_update_data_detail_df <- function(df,details_level=4,full_yr=2014){
  #trends_growth_update_data_detail()
  #df <- lite_run_sql("select data_code,data_growth_desc as desc_short from trends_meta where is_growth=1 and data_code like 'copi_%'")
  #trends_growth_update_data_detail_df(df=df,details_level=4)
  for(i in 1:nrow(df)){
    #i=1
    code <- df$data_code[i];
    dc <- df$desc_short[i]
    for(j in 0:0){
      #j=0
      growth <- lite_trends_get_growth_info(code,dc,dc,drop_end = j,full_yr = full_yr)
      trends_growth_add_detail(growth$yr,growth$mth,code,growth$mom$desc,growth$yoy$desc,j)
      cat(code," ",growth$yr," ",growth$mth," ",j," update \n")
    }
  }

}

trends_meta_set_growth_code <- function(code,desc_short,desc_long=''){
  # trends_meta_set_growth_code('JOBS-WF-FP-MF-10-33','Workforce in Manufacturing industries','Workforce in Manufacturing industries')

  if(missing(code) || missing(desc_short)){ stop("Missing code or description")}

  my_long <- desc_long
  if(nchar(desc_long)==0){my_long <- desc_short}

  SQ$new('trends_meta_set_growth',
         params=list(
           `@s_code`=code,
           `@s_desc_short`=desc_short,
           `@s_desc_long`=my_long
         )
  )$qry_exec()
}



trends_plot_spider <- function(code,dp=NULL){
  # code='ABMI';frq=4;n=12
  # trends_plot_spider('ABMI',15) - gdp showing recessin
  # trends_plot_spider('DYDC',15) - employment showing recession
  # trends_plot_spider('JOBS-WF-FP-MF-10-33',15) - manufacturing workforce
  code_info <- lite_run_sql(paste0("select data_frq as frq,data_desc from trends_meta where data_code= '",code,"';"))
  if(nrow(code_info)==0){stop(paste0("No data for code ",code))}
  frq <- code_info$frq[1]
  n <- NULL

  if(is.null(dp)){
     n <- frq-1
     if(frq==12){n <- frq - 2}
  }else{
    n <- dp
  }

  abc <- lite_get_trends_data_fx(code)
  abc$mom <- Delt(abc$value,k=1)[,1]*100
  abc$yoy <- Delt(abc$value,k=frq)[,1]*100

  abc$mome <- c(abc$mom[-c(1)],NA)
  abc$yoye <- c(abc$yoy[-c(1)],NA)

  #abc <- abc[-c(1:frq),]

  abc_n <- nrow(abc)
  abc_range <- 1:abc_n
  if(abc_n>n){
    abc_range <- (abc_n-n):abc_n
  }

  gdata <- abc[abc_range,]
  gdata$lbl <-""
  xy_scale <- 1
  delta <- 0.2

  x_min <- (min(gdata$mom)-delta) * xy_scale
  x_max <- (max(gdata$mom)+delta) * xy_scale
  y_min <- (min(gdata$yoy)-delta) * xy_scale
  y_max <- (max(gdata$yoy)+delta) * xy_scale

  x_label <- "Monthly growth"
  if(frq==4){x_label <- "Quarterly growth"}

  g <- ggplot(gdata,aes(x=mom,y=yoy,label=lbl))
  #g <- g + geom_point(colour="blue",size=3)
  g <- g + geom_hline(yintercept = 0)
  g <- g + geom_segment(aes(xend=mome,yend=yoye),size=1.0,colour="blue",arrow=grid::arrow(angle=15,type = "closed", length = grid::unit(0.18, "inches")))
  g <- g + geom_vline(xintercept = 0)+labs(y='Yearly growth',x=x_label)
  g <- g + geom_text(aes(label= paste0(month.abb[mth],yr%%100),vjust=-0.8,hjust=0.5,size=5))
  g <- g + facet_wrap(~data_desc)
  g <- g + xlim(x_min, x_max)
  g <- g + ylim(y_min, y_max)
  g <- g + theme(legend.position="none")
  print(g)
  return(abc)
}

trend_shift_left <- function(x,delta=1 ){
  if(missing(x)){stop("Please supply x")}

  n <- length(x)
  b <- numeric(n)
  loop_start <- 1+delta
  loop_end <- n
  j_count <- 1

  for(i in loop_start:loop_end){
    b[j_count] <- x[i]
    j_count <- j_count+1
  }

  loop_start <- n-delta+1
  loop_end <- n


    for(i in loop_start:loop_end){
      b[j_count] <- NA
      j_count <- j_count +1
    }

  return(b)
}

lite_trends_plot_sankey_df <- function(df,output_filename="sankey.html",save_output=FALSE,font_size=24){


  my_sankey_df <- df
  save_path <- output_filename


  my_sankey_links <- my_sankey_df[,c('source','target','value')] %>% dplyr::filter(!is.na(value))
  names(my_sankey_links) <- c('source','target','value')

  my_sankey_names <- data.frame(name=my_sankey_df[,c('name')]) %>% dplyr::filter(!(name=='<NA>'))




  nwk <- networkD3::sankeyNetwork(Links = my_sankey_links, Nodes = my_sankey_names, Source = "source",
                                  Target = "target", Value = "value", NodeID = "name" , fontSize = font_size, nodeWidth = 40
  )
  if(save_output){networkD3::saveNetwork(nwk,save_path)}
  return(nwk)
}


### UPDATES ###
#lite_ons_get_auto_updates()        #ONS-general
#trends_update_construction_data()  #ONS-construction

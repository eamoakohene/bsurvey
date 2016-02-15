survey_get_qsn <- function(qn=1){
  #qn=50
  sql <- paste0("select distinct qsn from bts2 where qn in (",qn,")")
  #cat(sql,"\n")
  abc <- lite_run_sql(sql)
  return( paste(abc$qsn,sep='',collapse = ','))
}

survey_get_data <- function(qn='1',qsn="",y1=2014,y2=2015,m1=1,m2=12){
  #qn='1';qsn='1,2,3,4,5,6';y1=2014;y2=2015;m1=1;m2=12;x_axis = "opts";title="";ytitle="";ypc=" (%)";my_axis='opts'
  my_qsn <- qsn
  if(qsn==""){
    my_qsn <- survey_get_qsn(qn)
  }

  sql <- paste0("select * from bts2 where ")
  where1 <- paste0(" qn in (",qn,")")
  where2 <- paste0(" and qsn in (",my_qsn,")")
  where3 <- paste0(" and (yr between ",y1," and ",y2,")")
  where4 <- paste0(" and (mth between ",m1," and ",m2,")")

  sql <- paste0(sql,where1,where2)#,where3,where4)
  #cat(sql,"\n")
  mydata <- lite_run_sql(sql)

  if(nrow(mydata)==0){
    cat("No data. Exiting plot \n")
    return(NULL)
  }

  mydata <- mydata %>% dplyr::filter(yr>=y1,yr<=y2) %>% dplyr::filter(mth>=m1,mth<=m2)

  mydata$qtr <- mydata$mth/3
  mydata$value <- mydata$value
  mydata$date <- as.Date(paste(mydata$yr,mydata$mth,1,sep="-"))
  mydata$data_days <- with(mydata,yr*372+mth*32)
  mydata$lbl <- paste0(mydata$yr,'Q',mydata$qtr)

  return(mydata)

}

survey_plot_line <- function(qn='1',qsn='1',y1=2014,y2=2015,m1=1,m2=12){

  abc <- survey_get_data(qn=qn,qsn=qsn,y1=y1,y2=y2,m1=m1,m2=m2)

  gmin <- abc %>% dplyr::group_by(qn,qsn) %>% dplyr::filter(data_days==min(data_days))
  gmax <- abc %>% dplyr::group_by(qn,qsn) %>% dplyr::filter(data_days==max(data_days))
  gtxt <- rbind(gmin,gmax)

  g <- ggplot(abc,aes(x=date,y=value))
  g <- g + geom_line(colour="blue",size=1.5)
  g <- g +ylab(paste0(abc$qs[1]," (%)"))+ggtitle(abc$q[1])+xlab("")

  g <- g+ geom_point(data=gtxt, aes(x=date,y=value),size=5,colour=LINECOLOUR)
  g <- g+ geom_point(data=gtxt, aes(x=date,y=value,colour=factor(data_days)),size=4)
  g <- g+ geom_text(data=gtxt, aes(x=date,y=value,label=db_lite_set_decimal(value,1)),vjust=-0.8,hjust=0.4,size=4,colour=SMOOTHCOLOUR)
  g <- g+ theme(legend.position="none")

  print(g)
  return(abc)
}

survey_plot_bar <- function(qn='1',qsn='',y1=2014,y2=2015,m1=1,m2=12,title="",ytitle="",ypc=" (%)",x_axis=c('period',"opts"),is_sort=FALSE){
  #qn='1';qsn=1;y1=2015;y2=2015;m1=9;m2=12;x_axis = "opts";title="";ytitle="";ypc=" (%)";my_axis='opts';is_sort=FALSE
  abc <- survey_get_data(qn=qn,qsn=qsn,y1=y1,y2=y2,m1=m1,m2=m2)
  if(is_sort){
    abc <- dplyr::arrange(abc,desc(value))
  }
  abc$lbl_line <- abc$value/2
  my_title <- ifelse(nchar(title)==0,abc$q[1],title)
  my_ytitle <- ifelse(nchar(ytitle)==0,paste0(abc$qs[1],ypc),ytitle)
  my_axis <- match.arg(x_axis)

  g_data <- with( abc, data.frame( y=value,text_line=lbl_line))
  g_data$x <- factor(abc$lbl)

  if( !(my_axis=="period")){
    g_data$x <- factor(abc$qs,levels=abc$qs)
  }

  g <- ggplot(g_data,aes(x=x,y=y))
  g <- g + geom_bar(fill=LINECOLOUR,stat = "identity")
  g <- g + ylab(my_ytitle)+ggtitle(my_title)+xlab("")

  g <- g + geom_text(aes(x=x,y=text_line,label=paste0(db_lite_set_decimal(y,1),"%")),colour="white",size=5)

  if( !(my_axis=="period")){
    g <- g + coord_flip()
  }

  print(g)
  return(g_data)
}

survey_plot_bar_grid <- function(qn='1',qsn='',y1=2014,y2=2015,m1=1,m2=12,title="",ytitle="",ypc="",x_axis=c('period',"opts"),is_sort=FALSE,show_title=TRUE,show_legend=TRUE,switch_axis=FALSE){
  #qn='1';qsn='1,2,3,4,5,6';y1=2014;y2=2015;m1=1;m2=12;x_axis = "opts";title="";ytitle="";ypc=" (%)";my_axis='opts'
  abc <- survey_get_data(qn=qn,qsn=qsn,y1=y1,y2=y2,m1=m1,m2=m2)

  abc$lbl_line <- abc$value/2
  my_title <- ifelse(nchar(title)==0,abc$theme[1],title)
  my_ytitle <- ifelse(nchar(ytitle)==0,paste0(abc$unit[1],ypc),ytitle)
  my_axis <- match.arg(x_axis)
  g_data <- NULL

  if( my_axis=="period"){
    g_data <- with( abc, data.frame(x=factor(abc$lbl), y=value,text_line=lbl_line,wrap=abc$qs,grp=abc$q))
    if(switch_axis){g_data$x <- g_data$grp}
  }else{
    g_data <- with( abc, data.frame(x=factor(abc$qs), y=value,text_line=lbl_line,wrap=abc$lbl,grp=abc$q))
    if(switch_axis){
      g_data$x <- abc$fname

    }

  }

  if(is_sort){
    g_data <- dplyr::arrange(g_data,y)
    g_data$x <- factor(g_data$x,levels = g_data$x)
  }

  g <- NULL
  if(!switch_axis){
    g <- ggplot(g_data,aes(x=x,y=y,fill=factor(grp)))
  }else{
    g <- ggplot(g_data,aes(x=x,y=y))
  }
  if(!switch_axis){
    g <- g + geom_bar(stat="identity",position = "dodge")
    g <- g + scale_fill_manual(
      values=BEAMA_BAR_COLOURS,name=""
    )
  }else{
    g <- g + geom_bar(stat="identity",fill='#30a4dc')
  }

  g <- g + ylab(my_ytitle)+xlab("")
  if(show_title){
    g <- g + ggtitle(my_title)
  }
  g <- g + facet_wrap(~wrap)
  g <- g + geom_text(
             aes(x=x,y=text_line,label=paste0(db_lite_set_decimal(y,0),"%")),
             colour="white",size=4,position=position_dodge(width=1))

  if( !(my_axis=="period")){
    g <- g + coord_flip()
  }
  if(show_legend){
      g <- g + theme(plot.title=element_text( size=30),legend.position="bottom")
  }else{
    g <- g + theme(plot.title=element_text( size=30),legend.position="none")
  }
  print(g)
  #ggplotly(g)
  return(g_data)
}


survey_plot_line_grid <- function(qn='1',qsn='',y1=2014,y2=2015,m1=1,m2=12,title="",ytitle="",ypc="",is_sort=FALSE,show_title=TRUE,show_legend = TRUE){
  #qn='1';y1=2012;y2=2015;m1=1;m2=12;title="";ytitle="";ypc=" (%)";show_title=TRUE
  abc <- survey_get_data(qn=qn,qsn=qsn,y1=y1,y2=y2,m1=m1,m2=m2)

  abc$lbl_line <- abc$value/2
  my_title <- ifelse(nchar(title)==0,abc$theme[1],title)
  my_ytitle <- ifelse(nchar(ytitle)==0,paste0(abc$unit[1],ypc),ytitle)


  gmin <- abc %>% dplyr::group_by(qn,qsn) %>% dplyr::filter(data_days==min(data_days))
  gmax <- abc %>% dplyr::group_by(qn,qsn) %>% dplyr::filter(data_days==max(data_days))
  gtxt <- rbind(gmin,gmax)

  g <- ggplot(abc,aes(x=date,y=value,group=q ))
  g <- g + geom_line(aes(colour=factor(q)),size=1.5)
  g <- g + scale_colour_manual(values=BEAMA_LINE_COLOURS,name="")

  g <- g + facet_wrap(~qs)
  g <- g + scale_x_date(labels = date_format("%b-%y"))
  g <- g + ylab(my_ytitle)+xlab("")+labs(fill="")
  if(show_title){
    g <- g + ggtitle(my_title)
  }

  g <- g+ geom_point(data=gtxt, aes(x=date,y=value),size=4,colour=LINECOLOUR,show.legend = FALSE)
  #g <- g+ geom_point(data=gtxt, aes(x=date,y=value,colour=factor(data_days)),size=4,show.legend = FALSE)
  g <- g+ geom_text(data=gtxt, aes(x=date,y=value,label=paste0(db_lite_set_decimal(value,1),"%")),vjust=-0.8,hjust=0.4,size=4,colour=SMOOTHCOLOUR,show.legend = FALSE)

  if(show_legend){
    g <- g + theme(plot.title=element_text( size=30),legend.position="bottom")
  }else{
    g <- g + theme(plot.title=element_text( size=30),legend.position="none")
  }

  #g <- g + scale_colour_discrete(guide = FALSE)
  print(g)
  #ggplotly(g)
  return(abc)

  ####






}

survey_get_sankey_df <- function(qn=50,yr=2015,mth=9){
  #qn=46;yr=2015;mth=12;font_size=14;save_output=FALSE;filename='xxx.html'
  POS_UP = c(5,6)
  POS_DOWN = c(2,3)
  SOURCE = c(NA,rep(0,3),rep(1,2),rep(2,2))
  TARGET = c(NA,1:7)
  qqq <- lite_run_sql(paste0("select distinct q from bts2 where qn=",qn))
  #cat(qqq$q,'\n')
  abc <- lite_run_sql(paste0("select fname,sankey_sort,value from bts2 where qn=",qn," and yr=",yr," and mth=",mth))
  if(nrow(abc)==0){
    abc_props <- lite_run_sql("select yr,mth from bts2 where id=(select max(id) from bts2)")
    abc <- lite_run_sql(paste0("select fname,sankey_sort,value from bts2 where qn=",qn," and yr=",abc_props$yr," and mth=",abc_props$mth))
  }
  abc_up <- data.frame(fname='Up',sankey_sort=2,value=sum(abc$value[abc$fname=='Up < 5pc'],abc$value[abc$fname=='Up >5pc']))
  abc_down <- data.frame(fname='Down',sankey_sort=3,value=sum(abc$value[abc$fname=='Down < 5pc'],abc$value[abc$fname=='Down > 5pc']))
  abc <- rbind(abc,abc_up,abc_down)
  abc$name <- paste0(abc$fname," = ",paste0(db_lite_set_decimal (abc$value,0),"%" ))
  abc <- dplyr::arrange(abc,sankey_sort)
  if(nrow(abc)==8){
    abc$source <- SOURCE
    abc$target <- TARGET
  }else{

     if(nrow(dplyr::filter(abc,fname=='BALANCE'))==0){
       abc <- rbind(data.frame(fname='BALANCE',sankey_sort=1,value=NA,name='BALANCE'),abc)
     }
    abc$target <- c(NA,1:(nrow(abc)-1))

    abc$source <- NA

    abc$source[abc$fname=='Up'] <- abc$source[abc$fname=='Down'] <- abc$source[abc$fname=='Same'] <- 0

    if(nrow(dplyr::filter(abc,fname=='Up < 5pc'))!=0) { abc$source[abc$fname=='Up < 5pc'] <- 1}
    if(nrow(dplyr::filter(abc,fname=='Up >5pc'))!=0) { abc$source[abc$fname=='Up >5pc'] <- 1}

    if(nrow(dplyr::filter(abc,fname=='Down < 5pc'))!=0) { abc$source[abc$fname=='Down < 5pc'] <- 2}
    if(nrow(dplyr::filter(abc,fname=='Down > 5pc'))!=0) { abc$source[abc$fname=='Down > 5pc'] <- 2}

  }
  return(list(data=abc,que=qqq$q))
}

survey_get_trends_df <- function(qn=50,yr=2015,mth=9){
  #qn=39;yr=2015;mth=9;font_size=14;save_output=FALSE;filename='xxx.html'

  abc <- lite_run_sql(paste0("select * from bts2 where qn=",qn," and yr=",yr," and mth=",mth))
  if(nrow(abc)==0){
    abc_props <- lite_run_sql("select yr,mth from bts2 where id=(select max(id) from bts2)")
    abc <- lite_run_sql(paste0("select * from bts2 where qn=",qn," and yr=",abc_props$yr," and mth=",abc_props$mth))
  }

  abc <- dplyr::arrange(abc,qsn)

  return(return(list(data=abc,que=abc$q[1])))
}

survey_plot_sankey_df <- function(df,output_filename="sankey.html",save_output=FALSE,font_size=24){


  my_sankey_df <- df
  save_path <- output_filename


  my_sankey_links <- my_sankey_df[,c('source','target','value')] %>% dplyr::filter(!is.na(value))
  names(my_sankey_links) <- c('source','target','value')

  my_sankey_names <- data.frame(name=my_sankey_df[,c('name')]) %>% dplyr::filter(!(name=='<NA>'))


  #   sankeyNetwork(Links = my_sankey_links, Nodes = my_sankey_names, Source = "source",
  #                 Target = "target", Value = "value", NodeID = "name",
  #                 fontsize = font_size, nodeWidth = 30)

  nwk <- networkD3::sankeyNetwork(Links = my_sankey_links, Nodes = my_sankey_names, Source = "source",
                                  Target = "target", Value = "value", NodeID = "name" , fontSize = font_size, nodeWidth = 40
  )
  if(save_output){networkD3::saveNetwork(nwk,save_path)}
  return(nwk)
}

survey_plot_sankey <- function(qn=50,yr=2015,mth=9,filename='xxx.html',font_size=14,save_output=FALSE){
  #qn=50;yr=2015;mth=9;font_size=14;save_output=FALSE;filename='xxx.html'

  abc <- survey_get_sankey_df(qn=qn,yr=yr,mth=mth)$data
  abc$value[1] <- NA

  survey_plot_sankey_df(abc
                        ,output_filename = filename
                        ,save_output = save_output
                        ,font_size = font_size
  )
}



survey_update_sankey_5pc <- function(){

  df <- data.frame(
    fname=c('BALANCE',	'Down < 5pc',	'Down > 5pc',	'Same',	'Up < 5pc',	'Up >5pc'),
    srt=c(1, 7, 8,	4,	5,	6),
    qsn=c(16,	23,	24,	35,	56,	57)
  )

  ops <- lite_run_sql("select distinct qn from bts2 where qsn in ('23','24')")$qn


  for(i in 1:nrow(df)){
    #i=1
    sql_set <- paste0( " sankey_sort=",df$srt[i]," ,fname ='",df$fname[i],"' ")
    sql_where <- paste0(" where qsn=",df$qsn[i]," and qn in ",lite_split_sql(paste(ops,sep="",collapse = ",")))
    sql <- paste0("update bts2 set ",sql_set,sql_where)
    #cat(sql,'\n')
    lite_run_sql(sql)
  }
}



survey_save_current_plot <- function(
  file="q1.png",width=600,height=300,path="X:/BEAMAstuff/surveys/2015/q3/images/",ppi=72
){
  ggsave(file=paste0(path,file),height=height/ppi,width=width/ppi,dpi=ppi,units="in")
}

survey_publication <- function(y1=2015,graph_path='X:/BEAMAstuff/surveys/2015/q3/images/'){
  names <- lite_run_sql("select distinct theme||'_'||qn||'_period.png' as filename from bts2 ")
  for(i in 1:55){
    #survey_plot_bar_grid(qn=as.character(i),y1=y1,x_axis = "opts")
    survey_plot_bar_grid(qn=as.character(i),y1=y1,x_axis = "period")
    survey_save_current_plot(file=names$filename[i])
  }


}


survey_trends_get_up<- function(qn=50,yr=2015,mth=9,trend_name="Sales Volume",prd="quarter",prd_silent=FALSE,is_pc=TRUE){
  #qn=46;yr=2015;mth=12;trend_name="Energy Cost";prd="quarter";prd_silent=FALSE;is_pc=TRUE
  UP <- 2
  if(!is_pc){UP <- 3}
  UP_OV5 <- 6
  UP_BL5 <- 5


  prv_mth <- mth-3
  prv_yr <- yr

  if((mth-3)==0){
    prv_yr <- yr-1
    prv_mth <- 12
  }

  srv_cur <- srv_prv <- NULL
  if(is_pc){
    srv_cur <- survey_get_sankey_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_sankey_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }else{
    srv_cur <- survey_get_trends_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_trends_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }

  UP <- UP_OV5 <- UP_BL5 <- 0
  if(nrow(dplyr::filter(srv_cur$data,fname=='Up'))>0){UP <- which(srv_cur$data$fname =='Up')}
  if(nrow(dplyr::filter(srv_cur$data,fname=='Up > 5pc'))>0){UP_OV5 <- which(srv_cur$data$fname =='Up > 5pc')}
  if(nrow(dplyr::filter(srv_cur$data,fname=='Up < 5pc'))>0){UP_BL5 <- which(srv_cur$data$fname =='Up < 5pc')}

  up_desc <- NULL

  if(!(UP==0)){
      srv_desc_up <- paste0(
        ifelse(prd_silent,"",paste0("In the Q",mth[1]/3," of ",yr,", ")),
        db_lite_set_decimal (srv_cur$data$value[UP],0),
        "% proportion of firms reported increase in ",
        trend_name," compared with a ",prd," ago."
      )
      srv_diff_value <- (srv_cur$data$value[UP] - srv_prv$data$value[UP])
      srv_diff <- ifelse(srv_diff_value>0.0999999,"up",ifelse(srv_diff_value< -0.0999999,"down","same"))
      srv_diff_adj<- ifelse(abs(srv_diff_value)<5," slightly", ifelse(abs(srv_diff_value) < 10,""," remarkably"))

      srv_desc_up_compare   <- switch(srv_diff,
                                      "up"=paste0(" This is",srv_diff_adj," up compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[UP],0) ,"%. "),
                                      "down"=paste0(" This is",srv_diff_adj," down compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[UP],0),"%. " ),
                                      "same"=paste0(" This remains unchanged from Q",prv_mth/3," ",prv_yr,"." )
      )
      srv_desc_up_over5 <- NULL
      if(is_pc && !(UP_OV5==0)){
        srv_desc_up_over5 <- paste0("About ",db_lite_set_decimal (srv_cur$data$value[UP_OV5],0) ,"% of firms reported growth over 5%")
      }

      srv_desc_up_below5 <- NULL
      if(is_pc && !(UP_BL5==0)){
        srv_desc_up_below5 <- paste0(db_lite_set_decimal (srv_cur$data$value[UP_BL5],0) ,"% reported growth but below 5%.")
      }

      up5 <- NULL
      if(!is.null(srv_desc_up_over5) && !is.null(srv_desc_up_below5)){
        up5 <- paste0(srv_desc_up_over5," and ",srv_desc_up_below5)
      }else if( !is.null(srv_desc_up_over5)){
        up5 <- paste0(srv_desc_up_over5,".")
      }else if(!is.null(srv_desc_up_below5)){
        up5 <- paste0(" About ",srv_desc_up_below5)
      }

      up_desc <- paste0 (srv_desc_up,srv_desc_up_compare,up5 )
  }
  return(up_desc)
}
survey_trends_get_down<- function(qn=50,yr=2015,mth=9,trend_name="Sales Volume",prd="quarter",prd_silent=TRUE,is_pc=TRUE){

  DOWN <- 3
  if(!is_pc){DOWN <- 2}
  DOWN_OV5 <- 8
  DOWN_BL5 <- 7

  prv_mth <- mth-3
  prv_yr <- yr

  if((mth-3)==0){
    prv_yr <- yr-1
    prv_mth <- 12
  }

  srv_cur <- srv_prv <- NULL
  if(is_pc){
    srv_cur <- survey_get_sankey_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_sankey_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }else{
    srv_cur <- survey_get_trends_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_trends_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }

  DOWN <- DOWN_OV5 <- DOWN_BL5 <- 0
  if(nrow(dplyr::filter(srv_cur$data,fname=='Down'))>0){DOWN <- which(srv_cur$data$fname =='Down')}
  if(nrow(dplyr::filter(srv_cur$data,fname=='Down > 5pc'))>0){DOWN_OV5 <- which(srv_cur$data$fname =='Down > 5pc')}
  if(nrow(dplyr::filter(srv_cur$data,fname=='Down < 5pc'))>0){DOWN_BL5 <- which(srv_cur$data$fname =='Down < 5pc')}



  down_desc <- NULL
  if(!(DOWN == 0)){
      srv_desc_down <- paste0(
        ifelse(prd_silent,"",paste0("In the Q",mth[1]/3," of ",yr,", ")),
        db_lite_set_decimal (srv_cur$data$value[DOWN],0),
        "% proportion of firms reported decrease in ",
        trend_name," compared with ",prd," ago."
      )
      srv_diff_value <- (srv_cur$data$value[DOWN] - srv_prv$data$value[DOWN])
      srv_diff <- ifelse(srv_diff_value>0.0999999,"up",ifelse(srv_diff_value< -0.0999999,"down","same"))
      srv_diff_adj<- ifelse(abs(srv_diff_value)<5," slightly", ifelse(abs(srv_diff_value) < 10,""," remarkably"))

      srv_desc_down_compare   <- switch(srv_diff,
                                        "up"=paste0(" This is",srv_diff_adj," up compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[DOWN],0) ,"%. "),
                                        "down"=paste0(" This is",srv_diff_adj," down compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[DOWN],0),"%. " ),
                                        "same"=paste0(" This remains unchanged from Q",prv_mth/3," ",prv_yr,"." )
      )
      srv_desc_down_over5 <- NULL
      if(is_pc && !(DOWN_OV5==0) ){
           srv_desc_down_over5 <- paste0("About ",db_lite_set_decimal (srv_cur$data$value[DOWN_OV5],0) ,"% of firms reported decline more than 5% ")
      }

      srv_desc_down_below5 <- NULL
      if(is_pc && !(DOWN_BL5==0) ){
        srv_desc_down_below5 <- paste0( db_lite_set_decimal (srv_cur$data$value[DOWN_BL5],0) ,"% reported decline not more than 5%.")
      }

      down5 <- NULL
      if(!is.null(srv_desc_down_over5) && !is.null(srv_desc_down_below5)){
        down5 <- paste0(srv_desc_down_over5," and ",srv_desc_down_below5)
      }else if( !is.null(srv_desc_down_over5)){
        down5 <- paste0(srv_desc_down_over5,".")
      }else if(!is.null(srv_desc_down_below5)){
        down5 <- paste0(" About ",srv_desc_down_below5)
      }
      down_desc <- paste0 (srv_desc_down,srv_desc_down_compare,down5 )
  }
  return(down_desc)
}

survey_trends_get_same<- function(qn=50,yr=2015,mth=9,trend_name="Sales Volume",prd="quarter",prd_silent=TRUE,is_pc=TRUE){

  SAME <- 4

  prv_mth <- mth-3
  prv_yr <- yr

  if((mth-3)==0){
    prv_yr <- yr-1
    prv_mth <- 12
  }

  srv_cur <- srv_prv <- NULL
  if(is_pc){
    srv_cur <- survey_get_sankey_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_sankey_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }else{
    srv_cur <- survey_get_trends_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_trends_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }


  srv_desc_same <- paste0(
    ifelse(prd_silent,"",paste0("In the Q",mth[1]/3," of ",yr,", ")),
    db_lite_set_decimal (srv_cur$data$value[SAME],0),
    "% proportion of firms reported no change ",
    trend_name," compared with ",prd," ago."
  )
  srv_diff_value <- (srv_cur$data$value[SAME] - srv_prv$data$value[SAME])
  srv_diff <- ifelse(srv_diff_value>0.0999999,"up",ifelse(srv_diff_value< -0.0999999,"down","same"))
  srv_diff_adj<- ifelse(abs(srv_diff_value)<5," slightly", ifelse(abs(srv_diff_value) < 10,""," remarkably"))

  srv_desc_same_compare   <- switch(srv_diff,
                                    "up"=paste0(" This is",srv_diff_adj," up compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[SAME],0) ,"%. "),
                                    "down"=paste0(" This is",srv_diff_adj," down compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[SAME],0),"%. " ),
                                    "same"=paste0(" This remains unchanged from Q",prv_mth/3," ",prv_yr,"." )
  )


  same_desc <- paste0 (srv_desc_same,srv_desc_same_compare )
  return(same_desc)
}

survey_trends_get_bal<- function(qn=50,yr=2015,mth=9,trend_name="Sales Volume",prd="quarter",is_pc=TRUE){
  #qn=16;yr=2015;mth=9;trend_name="'Product Improvemen";prd="year";prd_silent=FALSE;is_pc=FALSE
  BAL <- INDX <- 1

  prv_mth <- mth-3
  prv_yr <- yr

  if((mth-3)==0){
    prv_yr <- yr-1
    prv_mth <- 12
  }

  srv_cur <- srv_prv <- NULL
  if(is_pc){
    srv_cur <- survey_get_sankey_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_sankey_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }else{
    srv_cur <- survey_get_trends_df(qn=qn,yr=yr,mth=mth)
    srv_prv <- survey_get_trends_df(qn=qn,yr=prv_yr,mth=prv_mth)
  }

  verbs_down <- c("declining","deteriorating","weakening","falling","worsening")
  verbs_up <- c("increasing","improving","rising","growing")
  verbs_same <- c("same","unchanged","consistent")

  srv_diff_positive <- ifelse(srv_cur$data$value[INDX]>0.0999999,"up",ifelse(srv_cur$data$value[INDX]< -0.0999999,"down","same"))

  srv_desc_bal   <- switch(srv_diff_positive ,
                           "up"=paste0(" A balance of ",db_lite_set_decimal (srv_cur$data$value[INDX],0),"% the proportion of firms reported ",verbs_up[1]," ",trend_name," compared with ",prd," ago."),
                           "down"=paste0(" A balance of ",db_lite_set_decimal (srv_cur$data$value[INDX],0),"% the proportion of firms reported ",verbs_down[1]," ",trend_name," compared with ",prd," ago."),
                           "same"=paste0(" A balance of ",db_lite_set_decimal (srv_cur$data$value[INDX],0),"% the proportion of firms reported ",verbs_same[1]," ",trend_name," compared with ",prd," ago.")
  )


  srv_diff_value <- (round(srv_cur$data$value[INDX],0) - round(srv_prv$data$value[INDX],0))



  srv_diff <- ifelse(srv_diff_value>0.4999999,"up",ifelse(srv_diff_value< -0.4999999,"down","same"))
  srv_diff_adj<- ifelse(abs(srv_diff_value)<5," slightly", ifelse(abs(srv_diff_value) < 10,""," remarkably"))

  srv_desc_bal_compare   <- switch(srv_diff,
                                   "up"=paste0(" This is",srv_diff_adj," up compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[INDX],0) ,"%. "),
                                   "down"=paste0(" This is",srv_diff_adj," down compared with Q",prv_mth/3," ",prv_yr," which was ",db_lite_set_decimal (srv_prv$data$value[INDX],0),"%. " ),
                                   "same"=paste0(" This remains unchanged from Q",prv_mth/3," ",prv_yr,"." )
  )


  same_desc <- paste0 (srv_desc_bal,srv_desc_bal_compare )
  return(same_desc)
}

survey_trends_get_desc <- function(qn=50,yr=2015,mth=9,trend_name="Sales Volume",prd="quarter",is_pc=TRUE,list_format=FALSE,is_auto=FALSE){
  #qn=50;yr=2015;mth=12;trend_name="Sales Volume";prd="quarter";is_pc=TRUE;
  if(is_auto){

     my_comment <- lite_run_sql(sprintf("select comment from bts_q where qn=%i",qn))
     if(!is.na(my_comment$comment)){
       return(my_comment$comment)
     }
  }
  up <- survey_trends_get_up(qn=qn,yr=yr,mth=mth,trend_name = trend_name,prd = prd,is_pc = is_pc)
  down <- survey_trends_get_down(qn=qn,yr=yr,mth=mth,trend_name = trend_name,prd = prd,is_pc = is_pc)
  same <- survey_trends_get_same(qn=qn,yr=yr,mth=mth,trend_name = trend_name,prd = prd,is_pc = is_pc)
  bal <- survey_trends_get_bal(qn=qn,yr=yr,mth=mth,trend_name = trend_name,prd = prd,is_pc = is_pc)
  if(!list_format){
    return(
      paste0(
        up," ",down," ",same," ",bal
      )
    )
  }else{

    my_up <- my_down <- my_same <- my_bal <- ''

    if(nchar(up)>0){my_up <- tags$li(up)}
    if(nchar(down)>0){my_down <- tags$li(down)}
    if(nchar(same)>0){my_same <- tags$li(same)}
    if(nchar(bal)>0){my_bal <- tags$li(bal)}
    my_list <- tags$ul(my_up,my_down,my_same,my_bal)
    return(my_list)
  }
}

survey_trends_get_general<- function(qn=1,qsn=43,yr=2015,mth=9,trend_name="Capacity utlisation over 90%",prd="quarter",prd_silent=FALSE){
  #qn=1;qsn=43;yr=2015;mth=9;trend_name="Capacity utlisation over 90%";prd="quarter"
  BAL <- INDX <- 1
  my_qsn <- qsn
  prv_mth <- mth-3
  prv_yr <- yr

  if((mth-3)==0){
    prv_yr <- yr-1
    prv_mth <- 12
  }


    srv_cur <- survey_get_trends_df(qn=qn,yr=yr,mth=mth)$data %>% dplyr::filter(qsn==my_qsn)
    srv_prv <- survey_get_trends_df(qn=qn,yr=prv_yr,mth=prv_mth)$data %>%dplyr::filter(qsn==my_qsn)

  verbs_down <- c("declining","deteriorating","weakening","falling","worsening")
  verbs_up <- c("increasing","improving","rising","growing")
  verbs_same <- c("same","unchanged","consistent")

  srv_diff_positive <- ifelse(srv_cur$value[INDX]>0.0999999,"up",ifelse(srv_cur$value[INDX]< -0.0999999,"down","same"))

  srv_desc <- paste0(
    ifelse(prd_silent,"",paste0("In the Q",mth[1]/3," of ",yr,", ")),
    db_lite_set_decimal (srv_cur$value[INDX],0),
    "% proportion of firms reported ",
    trend_name," compared with ",prd," ago."
  )




  srv_diff_value <- (round(srv_cur$value[INDX],0) - round(srv_prv$value[INDX],0))



  srv_diff <- ifelse(srv_diff_value>0.4999999,"up",ifelse(srv_diff_value< -0.4999999,"down","same"))
  srv_diff_adj<- ifelse(abs(srv_diff_value)<5," slightly", ifelse(abs(srv_diff_value) < 10,""," remarkably"))

  srv_desc_compare   <- switch(srv_diff,
                                   "up"=paste0(" This is",srv_diff_adj," up compared with ",db_lite_set_decimal (srv_prv$value[INDX],0) ,"% in Q",prv_mth/3," ",prv_yr),
                                   "down"=paste0(" This is",srv_diff_adj," down compared with ",db_lite_set_decimal (srv_prv$value[INDX],0) ,"% in Q",prv_mth/3," ",prv_yr ),
                                   "same"=paste0(" This remains unchanged from Q",prv_mth/3," ",prv_yr,"." )
  )


  final_desc <- paste0 (srv_desc,srv_desc_compare )
  return(final_desc)
}


survey_update_data_info <- function(field=c('theme','unit','sankey_sort','fname')){

  my_field <- match.arg(field)

  if(is.null(my_field)){ cat("Null field. Exiting update \n"); return(1)}

  sql <- sprintf("UPDATE bts2 SET %s = ( SELECT bts_meta.%s FROM bts_meta WHERE bts_meta.qn = bts2.qn AND bts_meta.qsn = bts2.qsn LIMIT 1 )  WHERE bts2.%s IS NULL;",my_field,my_field,my_field)

  lite_run_sql(sql)
  #sql
}

survey_update_sub_captions <- function(){
   cap_update <- function(new_caption,qn,qsn) {
     lite_run_sql(sprintf("update bts2 set  qs='%s' where qn=%i and qsn=%i;",new_caption,qn,qsn))
   }

   ##company turnover
   cap_update('10m or less',21 ,54)
   cap_update('11m to 25m', 21 ,1)
   cap_update('26m to 50m', 21 ,2)
   cap_update('51m to 100m',21 ,3)
   cap_update('over 100m',  21 ,4)

   ##company labour force
   cap_update('200 or less',19 ,58)
   cap_update('201 to 500',19 ,5)
   cap_update('501 to 5000',19 ,7)
   cap_update('over 5000',19 ,42)

   #exports proportion of sales
   cap_update('5% or less',28 ,59)
}

survey_post_import_update <- function(){

  lite_run_sql("update bts2 set data_days = (yr*12*32+mth*32) where data_days is NULL")
  survey_update_data_info("theme")
  survey_update_data_info("unit")
  survey_update_data_info("fname")
  survey_update_data_info("sankey_sort")
  survey_update_sub_captions()
}

require(R6)
## class for manipulating bts_q table
btsq <- R6Class(
  "btsq",

  public = list(
    qn = NULL,
    qtxt = NULL,
    qhead = NULL,
    qcomment = NULL,

   initialize = function(qn,qtxt,qhead,qcomment){
     self$set_qn(qn)
     self$set_q(qtxt)
     self$set_head(qhead)
     self$set_comment(qcomment)
   },

    set_qn = function(value){
      if(!missing(value) ){
        if(is.numeric(value)){
          self$qn <- value
          invisible(self)
        }
      }
    },

    set_q = function(value){
      if(!missing(value)){
        self$qtxt <- value
        invisible(self)
      }
    },

    set_head = function(value){
      if(!missing(value)){
        self$qhead <- value
        invisible(self)
      }
    },

    set_comment = function(value){
      if(!missing(value)){
        self$qcomment <- value
        invisible(self)
      }
    },

    get_update_sql = function(){
      sprintf("update bts_q set q='%s' , head='%s', comment='%s' where qn=%i ", self$qtxt, self$qhead, self$qcomment,self$qn)
    },

    get_insert_sql = function(){
      sprintf("insert into bts_q (qn,q,head,comment) values (%i,'%s' , '%s', '%s') ",self$qn, self$qtxt, self$qhead, self$qcomment)
    },

    get_select_sql = function(){
      sprintf("select * from  bts_q where qn=%i",self$qn)
    },

    get_delete_sql = function(){
      sprintf("delete from  bts_q where qn=%i",self$qn)
    },

    update_row = function (){
      lite_run_sql(self$get_update_sql())
    },
    insert_row = function(){
      lite_run_sql(self$get_insert_sql())
    },
   
    select_row = function(){
      lite_run_sql(self$get_select_sql())
    },
   
    delete_row = function(){
      lite_run_sql(self$get_|delete_sql())
    }

  )#public

)
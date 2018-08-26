
#### 历年截至某一日期同期比较函数
#### To special day comparison among past years
#### 2014年9月1日撰写
#    2018-08-26修订
#### 景钦隆，广州市疾病预防控制中心
#### 用法：sdcl <- df_sdcl(data=data,specialday=Sys.Date())
#          data <- read.csv(file.choose(),header=T,stringsAsFactors=F)

df_sdcl <- function(data=data,specialday=Sys.Date()){
  library(dplyr)
  # data变量名：year,casetype,onsettime
  ##lndv--历年登革热个案病例数据库
  lndv <- tbl_df(data)
  bd.lndv <- filter(lndv,casetype=="本地病例") #筛选本地病例

  #历年本地病例首发日期和当前本地病例总数
  year.group <- group_by(bd.lndv,year)
  sftc <- summarise(year.group,tcases=n(),首发日期=min(as.Date(onsettime)),末发日期=max(as.Date(onsettime)))

  ##历年截至某一日期的本地病例累计数
  specialday <- format(specialday,"%m-%d")
  bd.lndv$day <- format(as.Date(bd.lndv$onsettime),"%m-%d")
  day.bd.lndv <- filter(bd.lndv,day < specialday)
  year.group.bd <- group_by(day.bd.lndv,year)
  dsftc <- summarise(year.group.bd,scases=n())

  stsftc <- full_join(dsftc,sftc,by="year") #合并当年总病例数、截至特定日期病例数、当年首发末发日期
  stsftc <- stsftc %>% bind_rows(data.frame(year=2004:2005,scases=c(0,0),tcases=c(0,0))) #2004和2005年无本地病例报告
  stsftc$scases[is.na(stsftc$scases)] <- 0
  stsftc <- arrange(stsftc,year)

  ###历年病例总数和截至特定日期病例数作图
  stsftc.plot <- as.data.frame(stsftc)
  mycol <- rgb(red=205,green=91,blue=69,alpha=200,max=255)
  bar <- barplot(stsftc.plot[,2],names.arg=stsftc.plot[,1],ylim=c(1,4000),space=0.5,yaxt="n",col="red",border=F)
  text(x=bar,y=stsftc.plot[,2],labels=stsftc.plot[,2],adj=c(0.5,-0.5),cex=1)
  points(x=bar,y=rep(3500,nrow(stsftc.plot)),cex=log(stsftc.plot[,3]+1),pch=21,col=mycol,bg=mycol) #添加当年总病例数
  text(x=bar,y=rep(3790,nrow(stsftc.plot)),labels=stsftc.plot[,3],adj=c(0.5,-0.5),cex=1)

  return(stsftc.plot)
}


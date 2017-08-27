
##### 景钦隆，2017年8月25日撰写，广州市疾控中心  #####
##### 此程序用于登革热每日新增病例及累计统计
##### 用法: blrb <- df_blrb(mdf=mdf,today=Sys.Date())


df_blrb <- function(mdf=mdf,today=Sys.Date()){ #病例日报统计函数
    ##### 此程序用于截至前一日本地感染登革热病例情况 #####
    ##### 景钦隆，2014年7月26日撰写，广州市疾控中心  #####
    library(reshape)
    mdf$区县 <- substr(mdf$现住详细地址,1,3)
    mdf$街道 <- substr(mdf$现住详细地址,4,6)
    mdf$cases <- 1
    hz <- aggregate(cases~区县+街道,length,data=mdf)
    mdf.rq <- mdf[c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位","cases")]
    mdf.rh <- melt(mdf.rq,id=c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位"))
    hz.r <- cast(mdf.rh,区县+街道~病例分类,length)
    # hz.r$累计病例 <- hz.r$临床诊断病例+hz.r$实验室确诊病例+hz.r$疑似病例
    hz.r$累计病例 <- hz.r$确诊病例+hz.r$疑似病例
    hz.r.sort <- hz.r[order(hz.r$区县,-hz.r$累计病例),]
    hz.r.sort$当日新增病例 <- 0
    #today <- as.Date(as.character(as.Date(Sys.Date(),"%Y-%m-%d")))
    if ((today-1) %in% as.Date(mdf$网络报告时间)){
      xz.mdf <- mdf[as.Date(mdf$网络报告时间)==(today-1),]
      xz.hz <- aggregate(cases~区县+街道,length,data=xz.mdf)
      for (i in 1:nrow(xz.hz)){
       for (j in 1:nrow(hz.r.sort)){
         if ((hz.r.sort$区县[j]==xz.hz$区县[i])&(hz.r.sort$街道[j]==xz.hz$街道[i])) hz.r.sort$当日新增病例[j]=xz.hz$cases[i]
         }}
    }
    # qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,临床诊断病例,实验室确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    # qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,临床诊断病例,实验室确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz$街道 <- " "
    final <- rbind(hz.r.sort,qx.xz.hz)
    final.sort <- final[order(final$区县,-final$累计病例),c("区县","街道","累计病例","当日新增病例","确诊病例","疑似病例")]
    colsum <- colSums(qx.xz.hz[,2:5])
    colsum.df <- as.data.frame(t(colsum))
    colsum.df$街道 <- " "
    colsum.df$区县 <- "合计"
    final.final <- rbind(final.sort,colsum.df)
    write.csv(final.final,paste0("登革热日报表",today,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","登革热日报表",today,".csv"))
    return(final.final)
}



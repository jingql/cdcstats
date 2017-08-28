
### 登革热疫点状态（按街镇尺度统计）统计函数
### 景钦隆，2014年8月26日撰写，广州市疾控中心  #####
### 2017年8月25日修订，case1[,7]--case1$发病日期，增加此15天和上15天病例数两个变量
### 此程序用于登革热疫点（以街道为空间统计单元）状态统计，可用于疫点状态信号探测
### 包括疫情始终日期、疫情持续时间、疫情波次（按发病日期计算）、疫情状态（持续、结束，按发病日期计算）、疫情强度（暴发、散发，按报告日期计算）、疫情趋势（上升、下降、持平，按报告日期计算）
### 其中疫情状态、疫情强度、疫情趋势以15天（登革热最长潜伏期）为时间聚集性探测期限
### "暴发点"变量为年内是否曾达到暴发疫情标准（按发病日期计算，15天内发病超过3例）

### 用法：市级：ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date())
###       区级： ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date(),user="county")


df_ydzt <- function(mdf=mdf,currentdate=Sys.Date(),user="city"){ #疫点状态统计函数
    library(plyr)
    mdf.cf <- mdf[duplicated(mdf$姓名),]
    # mdf$区县 <- substr(mdf$现住详细地址,1,3)
    # mdf$街道 <- substr(mdf$现住详细地址,4,6)
    if (user=="county") {mdf$区县 <- mdf$街道; mdf$街道<-mdf$居委}
    mdf$cases <- 1
    ydfz <- function(x){ifelse(x>25,"结束","持续")}
    ydzt <- ddply(mdf,c("区县","街道"),summarize,cumcases=length(发病日期)
          ,firstdate=min(as.Date(发病日期)),lastdate=max(as.Date(发病日期)),flinterval=lastdate-firstdate
	    ,firstreportdate=min(as.Date(网络报告时间)),lastreportdate=max(as.Date(网络报告时间)),flreportinterval=lastreportdate-firstreportdate)
    ydzt$slinterval=currentdate-ydzt$lastdate
    ydzt$ssinterval=currentdate-ydzt$firstdate
    ydzt$ydfz <- ydfz(ydzt$slinterval)
    ydzt.st <- ydzt[order(ydzt$区县,-ydzt$cumcases),]
    count=ydzt.st[,2]
    ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=rep(0,length(count))
    for(i in 1:length(count)){
        case1=subset(mdf,街道==count[i])
        case2=subset(case1,as.Date(case1$网络报告时间)>=(currentdate-15)) #发病日期与网络报告时间变换
        ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"暴发","散发")
        time1=case1$发病日期
        time2=sort(unique(as.Date(time1)))
        time3=rep(0,length(time2)-1)
        if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
        if ((length(time3)>0)==FALSE) {time3=1}
        ydzt.st$frequency[i]=length(time3[time3>=25])+1
        case3=subset(case1,as.Date(case1$网络报告时间)>=(currentdate-30)&as.Date(case1$网络报告时间)<(currentdate-15)) # 发病日期与网络报告时间变换
        ydzt.st$trend[i]=ifelse(nrow(case2)>nrow(case3),"上升",ifelse(nrow(case2)<nrow(case3),"下降","持平"))
        ydzt.st$curr15days[i] = nrow(case2)
        ydzt.st$past15days[i] = nrow(case3)
        ydzt.st$bfd[i] = df_bfd(case1)
    }
    ydzt.st <- ydzt.st[,c("区县","街道","cumcases","firstdate","lastdate","bfd","flinterval","slinterval","ssinterval",
                          "frequency","ydfz","firstreportdate","lastreportdate","flreportinterval","outbreak","trend","curr15days","past15days")]
    ydzt.st$outbreak[ydzt.st$ydfz=="结束"]<-"--"
    ydzt.st$trend[ydzt.st$ydfz=="结束"]<-"--"
    ydzt.st$casechange <- ydzt.st$curr15days-ydzt.st$past15days
    names(ydzt.st)<- c("区县","街道","累计病例数","首发日期","末例日期","暴发点","末首间隔","今末间隔","今首间隔","疫情波次"
                       ,"疫点状态","首报日期","末报日期","末首报间隔","疫情强度","疫情趋势","此15天病例数","上15天病例数","增减病例数")
    ydzt.st$首发日期 <- format(ydzt.st$首发日期,"%m-%d")
    ydzt.st$末例日期 <- format(ydzt.st$末例日期,"%m-%d")
    ydzt.st$今末间隔 <- as.numeric(ydzt.st$今末间隔)
    ydzt.st$末首间隔 <- as.numeric(ydzt.st$末首间隔)
    ydzt.st$今首间隔 <- as.numeric(ydzt.st$今首间隔)
    ydzt.st$首报日期 <- format(ydzt.st$首报日期,"%m-%d")
    ydzt.st$末报日期 <- format(ydzt.st$末报日期,"%m-%d")
    ydzt.st$末首报间隔 <- as.numeric(ydzt.st$末首报间隔)
    if (user=="county") {names(ydzt.st)[c(1,2)] <- c("街道","居委")}
    write.csv(ydzt.st,paste0("D:/","登革热疫点状态",currentdate,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","登革热疫点状态",currentdate,".csv"))
    return(ydzt.st)
}



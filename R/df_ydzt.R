
### 登革热疫点状态（按街镇尺度统计）统计函数
### 景钦隆，2014年8月26日撰写，广州市疾控中心  #####
### 2017年8月25日修订，case1[,7]--case1$发病日期，增加此15天和上15天病例数两个变量
#   2018年8月26日修订，用dplyr包替换原plyr包
### 此程序用于登革热疫点（以街道为空间统计单元）状态统计，可用于疫点状态信号探测
### 包括疫情始终日期、疫情持续时间、疫情波次（按发病日期计算）、疫情状态（持续、结束，按发病日期计算）、疫情强度（暴发、散发，按报告日期计算）、疫情趋势（上升、下降、持平，按报告日期计算）
### 其中疫情状态、疫情强度、疫情趋势以15天（登革热最长潜伏期）为时间聚集性探测期限
### "暴发点"变量为年内是否曾达到暴发疫情标准（按发病日期计算，街镇15天内发病超过3例）

### 用法：市级：ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date())
###       区级： ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date(),user="county")


df_ydzt <- function(mdf=mdf,currentdate=Sys.Date(),user="city"){ #疫点状态统计函数
  library(dplyr)
  # source("df_blrb.R")
  # mdf.cf <- mdf[duplicated(mdf$姓名),]
  # mdf$区县 <- substr(mdf$现住详细地址,1,3)
  # mdf$街道 <- substr(mdf$现住详细地址,4,6)
  if (user=="county") {mdf$区县 <- mdf$街道; mdf$街道<-mdf$居委}
  mdf$cases <- 1
  ydzt <- mdf %>% group_by(区县,街道) %>% summarise(cumcases=length(发病日期),confirmcases=sum(病例分类=="确诊病例"),suspectcases=sum(病例分类=="疑似病例")
                                                ,firstdate=min(as.Date(发病日期)),lastdate=max(as.Date(发病日期)),flinterval=lastdate-firstdate
                                                ,firstreportdate=min(as.Date(网络报告时间)),lastreportdate=max(as.Date(网络报告时间))
                                                ,flreportinterval=lastreportdate-firstreportdate)
  ydzt$slinterval=currentdate-ydzt$lastdate
  ydzt$ssinterval=currentdate-ydzt$firstdate
  ydfz <- function(x){ifelse(x>25,"结束","持续")} # 疫点持续状态判断函数
  ydzt$ydfz <- ydfz(ydzt$slinterval)
  ydzt.st <- ydzt[order(ydzt$区县,-ydzt$cumcases),]
  count=ydzt.st$街道
  ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=ydzt.st$curr15days=ydzt.st$past15days=rep(0,length(count))
  ydzt.st$bfd <- ydzt.st$outbreakbyreporttime <- NA
  for(i in 1:length(count)){
    case1=subset(mdf,街道==count[i])
    case2=subset(case1,as.Date(case1$发病日期)>=(currentdate-15)) #以发病日期为基准计算某个街道是否处于暴发状态
    ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"暴发","散发") # 判断当前疫情强度以发病日期计算
    time1=case1$发病日期 # 计算疫情波次用
    time2=sort(unique(as.Date(time1)))
    time3=rep(0,length(time2)-1) # 计算发病日期排序后日期间隔
    if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
    if ((length(time3)>0)==FALSE) {time3=1}
    ydzt.st$frequency[i]=length(time3[time3>=25])+1
    case3=subset(case1,as.Date(case1$网络报告时间)>=(currentdate-30)&as.Date(case1$网络报告时间)<(currentdate-15)) # 上15天报告病例数
    case4 <- subset(case1,as.Date(case1$网络报告时间)>=(currentdate-15)) #此15天报告病例数
    ydzt.st$trend[i]=ifelse(nrow(case4)>nrow(case3),"上升",ifelse(nrow(case4)<nrow(case3),"下降","持平")) # 比较此15天和上15天报告病例数得出
    ydzt.st$curr15days[i] = nrow(case4)
    ydzt.st$past15days[i] = nrow(case3)
    ydzt.st$bfd[i] = df_bfd(case1)
    ydzt.st$outbreakbyreporttime[i]=ifelse(nrow(case4)>=3,"暴发","散发") # 判断疫情报告强度以网络报告时间计算
  }
  ydzt.st <- ydzt.st[,c("区县","街道","cumcases","confirmcases","suspectcases","firstdate","lastdate","bfd","flinterval","slinterval","ssinterval",
                        "frequency","ydfz","firstreportdate","lastreportdate","flreportinterval","outbreak","outbreakbyreporttime","trend","curr15days","past15days")]
  ydzt.st$outbreak[ydzt.st$ydfz=="结束"]<-"--"
  ydzt.st$trend[ydzt.st$ydfz=="结束"]<-"--"
  ydzt.st$casechange <- ydzt.st$curr15days-ydzt.st$past15days
  names(ydzt.st)<- c("区县","街道","累计病例数","确诊病例","疑似病例","首发日期","末例日期","暴发点","末首间隔","今末间隔","今首间隔","疫情波次"
                     ,"疫点状态","首报日期","末报日期","末首报间隔","疫情强度","疫情报告强度","疫情报告趋势","此15天报告病例数","上15天报告病例数","增减报告病例数")
  ydzt.st$首发日期 <- format(ydzt.st$首发日期,"%m-%d")
  ydzt.st$末例日期 <- format(ydzt.st$末例日期,"%m-%d")
  ydzt.st$今末间隔 <- as.numeric(ydzt.st$今末间隔)
  ydzt.st$末首间隔 <- as.numeric(ydzt.st$末首间隔)
  ydzt.st$今首间隔 <- as.numeric(ydzt.st$今首间隔)
  ydzt.st$首报日期 <- format(ydzt.st$首报日期,"%m-%d")
  ydzt.st$末报日期 <- format(ydzt.st$末报日期,"%m-%d")
  ydzt.st$末首报间隔 <- as.numeric(ydzt.st$末首报间隔)
  if (user=="county") {names(ydzt.st)[c(1,2)] <- c("街道","居委")}
  write.csv(ydzt.st,paste0("登革热疫点状态",currentdate,".csv"),row.names = FALSE)
  shell.exec(paste0("登革热疫点状态",currentdate,".csv"))
  return(ydzt.st)
}



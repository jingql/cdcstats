
#### 街镇暴发信号探测函数
#### 景钦隆，广州市疾病预防控制中心
#### 2017年8月28日
#    2018年8月26日，修改nrow(intervalcases)>=3 为 sum(intervalcases[,2])>=3，之前代码如果有2行3个病例会判定为不是暴发点
#### 用法：jzcases <- subset(mdf,街道=="南源街")
####       bfd <- df_bfd(jzcases=jzcases)


df_bfd <- function(jzcases=jzcases){ # 是否年内曾是暴发点判断函数
  jzcasesaggr <- aggregate(编号~发病日期,data=jzcases,length)
  jzcasesaggr <- jzcasesaggr[order(as.Date(jzcasesaggr$发病日期)),]
  if (nrow(jzcasesaggr)>1){
    ybfd <- ""
    for (i in 1:(nrow(jzcasesaggr)-1)){
      if (ybfd=="是") break
      for (j in (i+1):nrow(jzcasesaggr)){
        intervalcases <- subset(jzcasesaggr,(as.Date(发病日期)>=as.Date(jzcasesaggr[i,1])) & (as.Date(发病日期)<=as.Date(jzcasesaggr[j,1])))
        if ((sum(intervalcases[,2])>=3) & ((as.Date(jzcasesaggr[j,1])-as.Date(jzcasesaggr[i,1]))<=15)){
          ybfd <- "是" 
          break
        } else {
          ybfd <- "否"
        }}
    }
  }
  if(nrow(jzcasesaggr)<=1 & jzcasesaggr[1,2] < 3) {ybfd <- "否"}
  if(nrow(jzcasesaggr)<=1 & jzcasesaggr[1,2] >= 3 ) {ybfd <- "是"}
  return(ybfd)
}



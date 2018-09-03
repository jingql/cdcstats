# cdcstats
基层疾控中心传染病监测数据统计程序包。  
Statistics in infectious diseases for primary CDC in China


## 1. 安装
   安装devtools： install.packages("devtools");     
   library(devtools)     
   install_github("jingql/cdcstats")
   
   此外，需提前安装程序包包括：dplyr
 
## 2. 数据读入
   数据读入和分析起始数据可以分为两个来源，一是疫情网原始数据开始分析，二是自行先整理规范的数据表，读入程序后进行分析。
   2.1 疫情网原始导出数据读入（csv文件，数据原始变量及变量名称均保持不变，包括疫情网原始记录下载时前两行空格亦保持不变）   
   gzreport <- read.csv(file.choose(),skip=2,header=T,colClasses="character") #  读入按报告地区（全部）数据表
   wdreporttogz <- read.csv(file.choose(),skip=2,header=T,colClasses="character") # 读入外地报本地（全部）数据表
   
   2.2 自行整理的数据库（从疫情网导出自行整理数据库，变量名必须含有省、地市、区县、街道、现住详细地址、发病日期、报告卡录入时间（报告日期）、删除时间、订正报告时间、订正终审时间、审核状态）
   例如：登革热本地病例一览表（csv格式）
   df_bd <- read.csv(file.choose(),header=T, colClasses="character") # 读入整理好的本地病例一览表
   df_sr <- read.csv(file.choose(),header=T, colClasses="character") # 读入整理好的输入病例一览表

## 2. 统计疫点状态
   市级：ydzt <- df_ydzt(mdf=df_bd,currentdate=Sys.Date())    
   区级： ydzt <- df_ydzt(mdf=df_bd,currentdate=Sys.Date(),user="county")
  
  

# cdcstats
基层疾控中心传染病监测数据统计程序包。  
Statistics in infectious diseases for primary CDC in China


## 1. 安装
   安装devtools： install.packages("devtools");     
   library(devtools)     
   install_github("jingql/cdcstats")
   
   此外，需提前安装程序包包括：dplyr

## 2. 统计疫点状态
   市级：ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date())    
   区级： ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date(),user="county")
  
  

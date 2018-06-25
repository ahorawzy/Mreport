stations <- read.csv("D:\\data\\sx_raw\\基础数据\\站点管理\\交调站数据（连续式）.csv")
stations <- stations[,c(2,3,5,6)]
names(stations) <- c("popup","label","lng","lat")
stations <- na.omit(stations)
stations <- stations[stations$label!="兴安台",]

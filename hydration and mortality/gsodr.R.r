install.packages("GSODR")
library(GSODR)
library(dplyr)


#=====1-get data for each year=====
eng_2009 <- get_GSOD(years = 2009, country = "GBR")
eng_2010 <- get_GSOD(years = 2010, country = "GBR")
eng_2011 <- get_GSOD(years = 2011, country = "GBR")
eng_2012 <- get_GSOD(years = 2012, country = "GBR")





#=====2-find nearest station by LAT and LON=====
(c1  <- nearest_stations(LAT =51.5178493992707, LON =-0.0615348079964776,distance = 5))
(c2  <- nearest_stations(LAT =52.4936755432749, LON =-1.903478362002,   distance = 13))
(c3  <- nearest_stations(LAT =51.4647346761107, LON =-2.56464697511188, distance = 7))
(c4  <- nearest_stations(LAT =53.5937493505693, LON =-2.29721060153354, distance = 30))
(c5  <- nearest_stations(LAT =51.4863402041425, LON =-3.16797675979689, distance = 20))
(c6  <- nearest_stations(LAT =53.3943465434079, LON =-2.21713081443688, distance = 8))
(c7  <- nearest_stations(LAT =51.3780323064045, LON =-0.091171524229452,distance = 10))
(c8  <- nearest_stations(LAT =55.980096764226,  LON =-3.16141288127912, distance = 13))
(c9  <- nearest_stations(LAT =55.8705567027437, LON =-4.23659796451929, distance = 13))
(c10 <- nearest_stations(LAT =51.4642072903602, LON =-0.376213779902226,distance = 8))
(c11 <- nearest_stations(LAT =53.7977858263122, LON =-1.55697135189454, distance = 13))
(c12 <- nearest_stations(LAT =53.4018626465367, LON =-2.9895351743287,  distance = 12))
(c13 <- nearest_stations(LAT =53.4778741061791, LON =-2.24583196318218, distance = 15))
(c14 <- nearest_stations(LAT =54.572827841974,  LON =-1.23437791989123, distance = 25))
(c15 <- nearest_stations(LAT =54.9769626915443, LON =-1.61787627028197, distance = 10))
(c16 <- nearest_stations(LAT =52.9557073119771, LON =-1.15838615083725, distance = 10))
(c17 <- nearest_stations(LAT =51.7537340844726, LON =-1.25651982343025, distance = 19))
(c18 <- nearest_stations(LAT =51.4522294474673, LON =-0.978887235216381,distance = 20))
(c19 <- nearest_stations(LAT =53.3781904190381, LON =-1.45141188110239, distance = 32))
(c20 <- nearest_stations(LAT =53.4093785293866, LON =-2.15898275653061, distance = 10))
(c21 <- nearest_stations(LAT =52.9991349971077, LON =-2.18446804870653, distance = 25))
(c22 <- nearest_stations(LAT =51.6206079122327, LON =-3.94250404298692, distance = 8))
(c23 <- nearest_stations(LAT =53.0413646950045, LON =-2.99057262873391, distance = 16))


#======3-select station for each year======
#make sure each year have at least one station was available (365 days)-checked below
star <- function(a){
  sta <- function(c){
  dat09<-subset(eng_2009,STNID %in% c)
  dat10<-subset(eng_2010,STNID %in% c)
  dat11<-subset(eng_2011,STNID %in% c)
  dat12<-subset(eng_2012,STNID %in% c)
    list(
      d09=table(dat09$STNID),
      d10=table(dat10$STNID),
      d11=table(dat11$STNID),
      d12=table(dat12$STNID))
  }
  sta(a)
}
(sc2  <- star(c2)) #"035310-99999" "035340-99999"
(sc3  <- star(c3)) #"037260-99999" "036280-99999" "036283-99999"

(sc5  <- star(c5)) #"037170-99999" "037150-99999" "036140-99999"
(sc6  <- star(c6)) #"033340-99999"
(sc7  <- star(c7)) #"037770-99999" "037810-99999"
(sc8  <- star(c8)) #"031590-99999" "031660-99999"
(sc9  <- star(c9)) #"031450-99999" "031400-99999"
(sc10 <- star(c10))#"037750-99999" "037720-99999"
(sc11 <- star(c11))#"033470-99999" "033463-99999"
(sc12 <- star(c12))#"033233-99999"
(sc13 <- star(c13))#"033350-99999" "033340-99999"

(sc15 <- star(c15))#"032450-99999" "032490-99999" "032460-99999" "032431-99999" "032433-99999"
(sc16 <- star(c16))#"033540-99999"
(sc17 <- star(c17))#"036530-99999" "036570-99999" "036580-99999"
(sc18 <- star(c18))#"037640-99999" "037630-99999" "036580-99999"

(sc21 <- star(c21))#"033380-99999" "033300-99999"
(sc22 <- star(c22))#"036090-99999"
(sc23 <- star(c23))#"033210-99999"


(sc1  <- star(c1)) #"037830-99999" "037780-99999" "037700-99999" "037790-99999"
(sc4  <- star(c4)) #"033350-99999" "033290-99999" "033510-99999" "033340-99999" "033400-99999" "033420-99999"
#c14 (center 10017)(station "032635-99999") in 2021 lack of date 2011-04-04
(sc14 <- star(c14))#"032710-99999" "032690-99999" "032635-99999" "032750-99999"
(sc19 <- star(c19))#"033475-99999" "033460-99999" "033450-99999" "034054-99999"
(sc20 <- star(c20))#"033480-99999" "033340-99999"


#sc1 for 09
#sc4 for 12
#sc14 for 11
#sc19 for 10 11
#sc20 for 12




#extract center with optimal station for each year-checked below
center <- c(11012,11021,11011,11008,11003,11024,11020,11005,11004,11018,
            11010,11016,11001,11017,11009,11013,11002,11007,11014,10003,
            11006,11022,11023)

#sc1 for 09
dd09 <- cbind(
  center[-1],
  names(c(        sc2$d09,sc3$d09[1],sc4$d09,
          sc5$d09,sc6$d09,sc7$d09,sc8$d09,sc9$d09,sc10$d09,sc11$d09,sc12$d09,sc13$d09,
          sc14$d09[1],sc15$d09,sc16$d09,sc17$d09,sc18$d09,
          sc19$d09[1],sc20$d09[2],sc21$d09,sc22$d09,sc23$d09))
  )
length(dd09[,1])


#sc19 for 10
dd10 <- cbind(
  center[-19],
  names(c(sc1$d10[1],sc2$d10,sc3$d10,sc4$d10,
          sc5$d10,sc6$d10,sc7$d10,sc8$d10,sc9$d10,sc10$d10,sc11$d10,sc12$d10,sc13$d10,
          sc14$d10[1],sc15$d10,sc16$d10,sc17$d10,sc18$d10,
                      sc20$d10[2],sc21$d10,sc22$d10,sc23$d10))
)
length(dd10[,1])


#sc14,sc19 for 11
dd11 <- cbind(
  center[-c(14,19)],
  names(c(sc1$d11,sc2$d11,sc3$d11,sc4$d11,
          sc5$d11,sc6$d11,sc7$d11,sc8$d11,sc9$d11,sc10$d11,sc11$d11,sc12$d11,sc13$d11,
                      sc15$d11,sc16$d11,sc17$d11,sc18$d11,
                      sc20$d11[2],sc21$d11,sc22$d11,sc23$d11))
)
length(dd11[,1])


#sc4,sc20 for 12
dd12 <- cbind(
  center[-c(4,20)],
  names(c(sc1$d12,sc2$d12,sc3$d12,
          sc5$d12,sc6$d12,sc7$d12,sc8$d12,sc9$d12,sc10$d12,sc11$d12,sc12$d12,sc13$d12,
          sc14$d11[1],sc15$d12,sc16$d12,sc17$d12,sc18$d12,
          sc19$d12[1],            sc21$d12,sc22$d12,sc23$d12))
)
length(dd12[,1])



#======4.1-extract main data for each year========
e09 <- function(n){cbind(center=dd09[n,1],subset(eng_2009,STNID %in% dd09[n,2]))}
dat09.p1 <-rbind(e09(1),e09(2),e09(3),e09(4),e09(5),e09(6),e09(7),e09(8),e09(9),e09(10),e09(11),e09(12),
                 e09(13),e09(14),e09(15),e09(16),e09(17),e09(18),e09(19),e09(20),e09(21),e09(22))

e10 <- function(n){cbind(center=dd10[n,1],subset(eng_2010,STNID %in% dd10[n,2]))}
dat10.p1 <-rbind(e10(1),e10(2),e10(3),e10(4),e10(5),e10(6),e10(7),e10(8),e10(9),e10(10),e10(11),e10(12),
                 e10(13),e10(14),e10(15),e10(16),e10(17),e10(18),e10(19),e10(20),e10(21),e10(22))

e11 <- function(n){cbind(center=dd11[n,1],subset(eng_2011,STNID %in% dd11[n,2]))}
dat11.p1 <-rbind(e11(1),e11(2),e11(3),e11(4),e11(5),e11(6),e11(7),e11(8),e11(9),e11(10),e11(11),e11(12),
                 e11(13),e11(14),e11(15),e11(16),e11(17),e11(18),e11(19),e11(20),e11(21))

e12 <- function(n){cbind(center=dd12[n,1],subset(eng_2012,STNID %in% dd12[n,2]))}
dat12.p1 <-rbind(e12(1),e12(2),e12(3),e12(4),e12(5),e12(6),e12(7),e12(8),e12(9),e12(10),e12(11),e12(12),
                 e12(13),e12(14),e12(15),e12(16),e12(17),e12(18),e12(19),e12(20),e12(21))


#======4.2-extract the rest of data for each year========
`%notin%` <- Negate(`%in%`)

#sc1 for 09
i09 <-subset(eng_2009,STNID %in% c("037700-99999"))
dat09.p2 <- cbind(center=center[1],
                  rbind(i09,
                        subset(subset(eng_2009,STNID %in% c("037790-99999")),YEARMODA %notin% i09$YEARMODA))
                  )


#sc19 for 10
i10 <-subset(eng_2010,STNID %in% c("033460-99999"))
dat10.p2 <- cbind(center=center[19],
                  rbind(i10,
                        subset(subset(eng_2010,STNID %in% c("034054-99999")),YEARMODA %notin% i10$YEARMODA))
                  )


#sc14,sc19 for 11
i11.1 <-subset(eng_2011,STNID %in% c("032635-99999"))
i11.2 <-subset(eng_2011,STNID %in% c("033460-99999"))

dat11.p2 <- rbind(cbind(center=center[14],
                        rbind(i11.1,
                              subset(subset(eng_2011,STNID %in% c("032750-99999")),YEARMODA %notin% i11.1$YEARMODA)
                        )),
                  cbind(center=center[19], 
                        rbind(i11.2,
                              subset(subset(eng_2011,STNID %in% c("034054-99999")),YEARMODA %notin% i11.2$YEARMODA)
                        ))
                  )

#sc4,sc20 for 12
i12.1 <-subset(eng_2012,STNID %in% c("033510-99999"))
i12.2 <-subset(eng_2012,STNID %in% c("033480-99999"))

dat12.p2 <- rbind(cbind(center=center[4],
                        rbind(i12.1,
                              subset(subset(eng_2012,STNID %in% c("033340-99999")),YEARMODA %notin% i12.1$YEARMODA)
                        )),
                  cbind(center=center[20], 
                        rbind(i12.2,
                              subset(subset(eng_2012,STNID %in% c("033340-99999")),YEARMODA %notin% i12.2$YEARMODA)
                        ))
                  )


#======4.3-rbind part1 and part2========

data09 <- rbind(dat09.p1,dat09.p2)
data10 <- rbind(dat10.p1,dat10.p2)
data11 <- rbind(dat11.p1,dat11.p2)
data12 <- rbind(dat12.p1,dat12.p2)


table(data09$center)
table(data10$center)
table(data11$center)
table(data12$center)

data.all <- rbind(data09,data10,data11,data12)[,c("center","STNID","NAME","YEARMODA","YEAR","MONTH","DAY","CTRY","YDAY","TEMP", "RH", "ELEVATION")]
table(data.all$center)


#======5-import spss.sav data from Hao for UKB========

library(Hmisc)
ukb <- spss.get("C:/Users/84910/OneDrive/Project/UKB/Water/Date_completed.sav", use.value.labels = TRUE)
colnames(ukb)[2:6] <- rep("YEARMODA",5)


#change data format
pss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
ukb[,2] <- pss2date(ukb[,2])
ukb[,3] <- pss2date(ukb[,3])
ukb[,4] <- pss2date(ukb[,4])
ukb[,5] <- pss2date(ukb[,5])
ukb[,6] <- pss2date(ukb[,6])

time0 <- merge(ukb[,c(1,2,10)],data.all,by=c("center","YEARMODA"))[,3:7]#70712
time1 <- merge(ukb[,c(1,3,10)],data.all,by=c("center","YEARMODA"))[,3:7]#100593
time2 <- merge(ukb[,c(1,4,10)],data.all,by=c("center","YEARMODA"))[,3:7]#83261
time3 <- merge(ukb[,c(1,5,10)],data.all,by=c("center","YEARMODA"))[,3:7]#103788
time4 <- merge(ukb[,c(1,6,10)],data.all,by=c("center","YEARMODA"))[,3:7]#100243


colnames(time0)[c(2:5)] <- c("STNID.0","TEMP.0","RH.0","ELEVATION.0" )
colnames(time1)[c(2:5)] <- c("STNID.1","TEMP.1","RH.1","ELEVATION.1" )
colnames(time2)[c(2:5)] <- c("STNID.2","TEMP.2","RH.2","ELEVATION.2" )
colnames(time3)[c(2:5)] <- c("STNID.3","TEMP.3","RH.3","ELEVATION.3" )
colnames(time4)[c(2:5)] <- c("STNID.4","TEMP.4","RH.4","ELEVATION.4" )




#======6-merge=======
library(tidyverse)
df_list <- list(time0,time1,time2,time3,time4)  
GSODR <- df_list %>% reduce(full_join, by=c("n.eid"))
GSODR <- GSODR[order(GSODR$n.eid),]



library(foreign)
write.foreign(GSODR, "C:/Users/84910/OneDrive/Project/UKB/Water/GSODR0507.txt", "C:/Users/84910/OneDrive/Project/UKB/Water/GSODR0507.sps",   package="SPSS")





















####################################GSODR: ELEVATION,TEMP and RH data extracted by JZahng recoded#############################################
eng_2009 <- get_GSOD(years = 2009, country = "GBR")
eng_2010 <- get_GSOD(years = 2010, country = "GBR")
eng_2011 <- get_GSOD(years = 2011, country = "GBR")
eng_2012 <- get_GSOD(years = 2012, country = "GBR")


# 创建一个函数，用于查找最近的气象站
find_stations <- function(lat, lon, dist) {
  nearest_stations(LAT = lat, LON = lon, distance = dist)
}

# 创建一个数据框，包含所有的 LAT, LON 和距离
locations <- data.frame(
  LAT = c(51.5178493992707, 52.4936755432749, 51.4647346761107, 53.5937493505693, 
          51.4863402041425, 53.3943465434079, 51.3780323064045, 55.980096764226, 
          55.8705567027437, 51.4642072903602, 53.7977858263122, 53.4018626465367, 
          53.4778741061791, 54.572827841974, 54.9769626915443, 52.9557073119771, 
          51.7537340844726, 51.4522294474673, 53.3781904190381, 53.4093785293866, 
          52.9991349971077, 51.6206079122327, 53.0413646950045),
  LON = c(-0.0615348079964776, -1.903478362002, -2.56464697511188, -2.29721060153354, 
          -3.16797675979689, -2.21713081443688, -0.091171524229452, -3.16141288127912, 
          -4.23659796451929, -0.376213779902226, -1.55697135189454, -2.9895351743287, 
          -2.24583196318218, -1.23437791989123, -1.61787627028197, -1.15838615083725, 
          -1.25651982343025, -0.978887235216381, -1.45141188110239, -2.15898275653061, 
          -2.18446804870653, -3.94250404298692, -2.99057262873391),
  distance = c(5, 13, 7, 30, 20, 8, 10, 13, 13, 8, 13, 12, 15, 25, 10, 10, 
               19, 20, 32, 10, 25, 8, 16)
)

# 使用 lapply 或 purrr::map 批量查找最近气象站
stations <- lapply(1:nrow(locations), function(i) {
  find_stations(locations$LAT[i], locations$LON[i], locations$distance[i])
})

# 为每个结果命名，方便后续使用
names(stations) <- paste0("center", 1:nrow(locations))

# 打印结果
stations


r

复制
# 加载数据
eng_2009 <- get_GSOD(years = 2009, country = "GBR")
eng_2010 <- get_GSOD(years = 2010, country = "GBR")
eng_2011 <- get_GSOD(years = 2011, country = "GBR")
eng_2012 <- get_GSOD(years = 2012, country = "GBR")

# 创建一个函数，用于查找最近的气象站
find_stations <- function(lat, lon, dist) {
  nearest_stations(LAT = lat, LON = lon, distance = dist)
}

# 创建一个数据框，包含所有的 LAT, LON 和距离
locations <- data.frame(
  LAT = c(51.5178493992707, 52.4936755432749, 51.4647346761107, 53.5937493505693, 
          51.4863402041425, 53.3943465434079, 51.3780323064045, 55.980096764226, 
          55.8705567027437, 51.4642072903602, 53.7977858263122, 53.4018626465367, 
          53.4778741061791, 54.572827841974, 54.9769626915443, 52.9557073119771, 
          51.7537340844726, 51.4522294474673, 53.3781904190381, 53.4093785293866, 
          52.9991349971077, 51.6206079122327, 53.0413646950045),
  LON = c(-0.0615348079964776, -1.903478362002, -2.56464697511188, -2.29721060153354, 
          -3.16797675979689, -2.21713081443688, -0.091171524229452, -3.16141288127912, 
          -4.23659796451929, -0.376213779902226, -1.55697135189454, -2.9895351743287, 
          -2.24583196318218, -1.23437791989123, -1.61787627028197, -1.15838615083725, 
          -1.25651982343025, -0.978887235216381, -1.45141188110239, -2.15898275653061, 
          -2.18446804870653, -3.94250404298692, -2.99057262873391),
  distance = c(5, 13, 7, 30, 20, 8, 10, 13, 13, 8, 13, 12, 15, 25, 10, 10, 
               19, 20, 32, 10, 25, 8, 16)
)

# 使用 lapply 或 purrr::map 批量查找最近气象站
stations <- lapply(1:nrow(locations), function(i) {
  find_stations(locations$LAT[i], locations$LON[i], locations$distance[i])
})

# 为每个结果命名，方便后续使用
names(stations) <- paste0("center", 1:nrow(locations))

# 补充缺失天数的函数
fill_missing_days <- function(data, main_station, backup_station, center_id) {
  main_data <- subset(data, STNID == main_station)
  backup_data <- subset(data, STNID == backup_station & !(YEARMODA %in% main_data$YEARMODA))
  cbind(center = center_id, rbind(main_data, backup_data))
}

# 改进的数据处理函数 - 选择记录最多的站点作为主站
process_center_data <- function(i, year_data, year_name) {
  station_ids <- stations[[i]]$STNID
  
  # 计算每个站点的记录数
  station_counts <- sapply(station_ids, function(id) {
    nrow(subset(year_data, STNID == id))
  })
  
  # 只保留有记录的站点
  valid_stations <- station_ids[station_counts > 0]
  valid_counts <- station_counts[station_counts > 0]
  
  if (length(valid_stations) == 0) {
    # 如果没有有效站点，返回空数据框
    return(data.frame(
      center = i, STNID = NA, NAME = NA, YEARMODA = NA, YEAR = NA, 
      MONTH = NA, DAY = NA, CTRY = NA, YDAY = NA, TEMP = NA, RH = NA, ELEVATION = NA
    ))
  }
  
  # 按记录数排序站点
  sorted_indices <- order(valid_counts, decreasing = TRUE)
  sorted_stations <- valid_stations[sorted_indices]
  
  if (length(sorted_stations) >= 2) {
    # 使用记录最多的两个站点
    main_station <- sorted_stations[1]
    backup_station <- sorted_stations[2]
    return(fill_missing_days(year_data, main_station, backup_station, i))
  } else {
    # 只有一个有效站点
    return(cbind(center = i, subset(year_data, STNID == sorted_stations[1])))
  }
}

# 处理每个center的数据
data_by_center_filled <- lapply(1:length(stations), function(i) {
  list(
    data_2009 = process_center_data(i, eng_2009, "2009"),
    data_2010 = process_center_data(i, eng_2010, "2010"),
    data_2011 = process_center_data(i, eng_2011, "2011"),
    data_2012 = process_center_data(i, eng_2012, "2012")
  )
})

# 合并每个 center 的所有年份数据
combine_all_years <- function(data_list, columns) {
  do.call(rbind, lapply(data_list, function(data) {
    # 处理data.table对象
    if(is.data.table(data)) {
      data[, ..columns, drop = FALSE]
    } else {
      data[, columns, drop = FALSE]
    }
  }))
}

# 保留的列
columns_to_keep <- c("center", "STNID", "NAME", "YEARMODA", "YEAR", "MONTH", "DAY", "CTRY", "YDAY", "TEMP", "RH", "ELEVATION")

# 合并每个 center 的数据
data_by_center_final <- lapply(data_by_center_filled, function(center_data) {
  combine_all_years(center_data, columns_to_keep)
})

# 合并所有 center 的数据
final_data <- do.call(rbind, data_by_center_final)

# 查看最终数据
head(final_data)

# 创建center编号映射表
center_map <- data.frame(
  old_center = 1:23,
  new_center = c(
    11012, 11021, 11011, 11008, 11003, 11024, 11020, 11005, 11004, 11018,
    11010, 11016, 11001, 11017, 11009, 11013, 11002, 11007, 11014, 10003,
    11006, 11022, 11023
  )
)

# 显示映射关系
print(center_map)

# 使用match和映射表替换center值
final_data$center <- center_map$new_center[match(final_data$center, center_map$old_center)]

# 查看替换后的结果
head(final_data)

data.table::fwrite(final_data, "C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_GSODR_info.csv")



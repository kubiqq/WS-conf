#Строим графики для того, чтобы показать потепление в нижнем слое воды на Д-1

library(readxl)
library(data.table)
library(tidyr)
library(ggplot2)
library(dygraphs)

theme_set(theme_bw())  #pre-set the bw theme.

setwd('C:\\Thesis\\Статьи в прогрессе\\Доклад на беломорскую конференцию')
Temp <- as.data.table(read_xlsx('Temp.xlsx', col_names = T))
Temp$month <- as.factor(format(Temp$Date,"%m"))
Temp$year <- as.numeric(format(Temp$Date,"%Y"))
Temp$yearF <- as.factor(Temp$year)

#Добавляем переменную со средними значениями
Temp$Mn <- rowMeans(Temp[ , c(2:7)], na.rm = T)

####    Калькуляция перемешанного слоя
TempMD <- t(Temp) # транспонирование таблицы
colnames(TempMD) <- TempMD[1, ]
TempMD <- as.data.frame(TempMD[-1, ])
rows <- rownames(TempMD)
TempMD <- TempMD %>% mutate_all(as.numeric)# преобразование всей таблицы из character в numeric
setDT(TempMD)# data.frame в data.table
names <- colnames(TempMD)

# функция вычитания одной ячейки из другой, игнорируя NA
lag_diff <- function(x) {
  which_nna <- which(!is.na(x))
  out <- rep(NA_integer_, length(x))
  out[which_nna] <- shift(x[which_nna]) - x[which_nna]
  out
}

# использование функции выше
dTemp <- TempMD[ ,lapply(.SD, lag_diff), ]
rownames(dTemp) <- rows
setDF(dTemp)
dTemp <- cbind(Depth = as.numeric(rows), dTemp)

# Функция дифференцирования с учетом NA
diff_div <- function(x, y) {
  which_nna <- which(!is.na(x))
  which_nna <- append(1, which_nna)
  out <- rep(NA_integer_, length(x))
  out[which_nna] <- abs(x[which_nna]/(shift(y[which_nna]) - y[which_nna]))
  out
}

setDT(dTemp)
ncolumns <- colnames(dTemp[ ,2:ncol(dTemp)])
ddTemp <- dTemp[ ,mapply(diff_div, x = .SD, y = dTemp[ ,1]), .SDcols = ncolumns]
ddTemp <- cbind(Depth = as.numeric(rows), ddTemp)

#Depth-to-Depth difference
tv_diff <- function(x) {
  max(x, na.rm = T) - min(x, na.rm = T)
  out
} #max(dT) - min (dT)

ddTemp <- as.data.frame(ddTemp)
ddTemp %>% mutate_if(is.character,as.numeric)
setDT(ddTemp)
ncolumns <- colnames(ddTemp[ ,2:ncol(dTemp)])
tv <- ddTemp[ ,mapply(function(x){max(x, na.rm = T) - min(x, na.rm = T)}, x = .SD),  .SDcols = ncolumns]
tvv <- as.data.frame(tv)
tvv$Date <- as.Date(row.names(tvv))

ddTemp <- as.data.frame(ddTemp)

table.Temp <- data.frame(i = character(), 
                      Date = character(), 
                      Depth_max = character(), 
                      Max_delta = character())

#Собираем в таблицу глубину максимального изменения температуры с поправкой на глубину
# и 
for (i in 2:length(ddTemp)) {
  depth_max <- ddTemp[which.max(ddTemp[, i]), 1]
  if (length(depth_max) == 0) {
    depth_max <- NA
  }
  table.Temp[nrow(table.Temp)+1, ] <- 
    c(i, colnames(ddTemp)[i], depth_max, max(ddTemp[, i], na.rm = T))
  
}

table.Temp$Depth_max <- as.numeric(table.Temp$Depth_max)
table.Temp$Max_delta <- as.numeric(table.Temp$Max_delta)
table.Temp$Date <- as.POSIXct(table.Temp$Date)

#рисуем график пикноклина
Picnocline_plot <- ggplot(table.Temp[], aes(x= Date, y=Depth_max)) +
  geom_point(na.rm=TRUE, color="darkred", size=1) +
  scale_y_reverse() +
  labs(title ="Изменение слоя пикноклина с глубиной") +
  xlab("Дата") + ylab("Глубина") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, span = 0.4, colour = "steelblue") +
  theme_classic()

Picnocline_plot


write.xlsx(table.Temp, file = "table.Temp.xlsx")

#apply(df, MARGIN = c(1,2), function(x) x %in% List[[1]])

#График средней температуры по станциям
Tmean <- ggplot(Temp, aes(x=Date, y=Mn)) +
  geom_point(na.rm=TRUE, color="black", size=1) +
  labs(title ="Средняя температура воды в столбе на станции Д-1") +
  #xlab("Дата") + ylab("Температура воды") +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, colour = "red") +
  theme_classic()
Tmean

#строим ridgeplot по годам
Temp_mn <- ggplot(Temp, aes(x = `Mn`, y = `yearF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c() +
  labs(title = 'Распределение средней по столбу температуры воды 1957 - 2021 год') +
  xlab("Температура воды") + ylab("Год") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 6)
  )

Temp_mn

#Преобразуем таблицу
Temp_all <- as.data.table(melt(Temp, id.vars = c("Date")))
names(Temp_all) <- c("Date", "Depth", "Temp")
Temp_all$Depth <- as.numeric(as.character(Temp_all$Depth))
Temp_all$month <- as.factor(format(Temp_all$Date,"%m"))
Temp_all$year <- as.numeric(format(Temp_all$Date,"%Y"))
Temp_all$Temp <- as.numeric(Temp_all$Temp)

Temp_all[,depthGroup:=cut(Depth,
                          breaks=c(0,10,40,Inf),
                          include.lowest=TRUE,
                          labels=c("<10","10-40",">40"))]

#timeseries with trend for depth >40 m
p40 <- ggplot(Temp_all [depthGroup == '>40'], aes(x=Date, y=Temp)) +
  geom_point(na.rm=TRUE, color="darkred", size=1) +
  labs(title ="Температура воды на станции Д-1 >40 м") +
  xlab("Дата") + ylab("Температура воды") +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, colour = "steelblue") +
  theme_classic()
p40

#timeseries with trend for depth 10-40 m
p10 <- ggplot(Temp_all [depthGroup == '10-40'], aes(x=Date, y=Temp)) +
  geom_point(na.rm=TRUE, color="black", size=1) +
  labs(title ="Температура воды на станции Д-1 10-40 м") +
  xlab("Дата") + ylab("Температура воды") +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  theme_classic()
p10

#timeseries with trend for depth 0-10 m
p0 <- ggplot(Temp_all [depthGroup == '<10'], aes(x=Date, y=Temp)) +
  geom_point(na.rm=TRUE, color="black", size=1) +
  labs(title ="Температура воды на станции Д-1 <10 м") +
  xlab("Дата") + ylab("Температура воды") +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  theme_classic()
p0


#строим facet grid по группам глубин и температуре с трендом
Temp_all$Date <- as.Date(Temp_all$Date, origin="1900-01-01")
Temp_all$year <- as.factor(Temp_all$year)

ggplot(na.omit(Temp_all), aes(x=Date, y=Temp, colour=depthGroup)) + 
  #geom_path() + 
  geom_point() +
  #geom_boxplot()+
  geom_smooth(method = "loess", se = FALSE, span = 0.6, color = "black") +
  scale_x_date(breaks="1 year") + 
  facet_grid(depthGroup ~.) +
  #facet_grid(rows = vars(Temp_all$depthGroup)) +
  labs(title ="Температура воды на станции Д-1") +
  xlab("Дата") + ylab("Температура воды") +
  theme(legend.position="none", axis.text.x = element_text(angle = 90)) 

#фасет по глубинам с трендом
Temp_all$Depth <- as.factor(Temp_all$Depth)


ggplot(na.omit(Temp_all), aes(x=Date, y=Temp, colour=Depth)) + 
  #geom_path() + 
  geom_point() +
  #geom_boxplot()+
  geom_smooth(method = "loess", se = FALSE, span = 0.6, color = "black") +
  scale_x_date(breaks="1 year") + 
  facet_grid(Depth ~.) +
  #facet_grid(rows = vars(Temp_all$depthGroup)) +
  labs(title ="Температура воды на станции Д-1") +
  xlab("Дата") + ylab("Температура воды") +
  theme(legend.position="none", axis.text.x = element_text(angle = 90)) 

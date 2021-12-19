## title: "4.Visualization - ECO제주 시각화파일"
## author: "UmStatistics - 권남택, 오정민, 유경민, 이상현"
## date: '2021년 9월 15일 수요일'


## 본 파일은 2021 빅콘테스트 ECO 제주 참가팀 **UmStatistics**의 마크다운 제출 파일입니다.
## 이 `Visualization` 파일에서는 주어진 데이터와 외부 데이터를 활용해 PPT에 들어가는 결과물들에 대한 시각화 자료가 포함되어 있습니다.

## PPT를 통해 충분히 확인할 수 있는 정보(ex. 행정동 경계지도 등)는 제외하고, 시각화와 관련한 정보들이 제공될 것입니다.

# 0. 사전 작업

## 0.1 라이브러리 로드와 함수



## ---- warning = F, message = F, eval = F----------------------------------------------------------------------------------------------
## #install.packages("BiocManager")
## #BiocManager::install("Rgraphviz")


## ---- eval = F------------------------------------------------------------------------------------------------------------------------
## # 함수 자동 설치
pacotes = c("tidyverse", "data.table", "lubridate", 'imputeTS',
             'gridExtra', 'SHAPforxgboost', 'visNetwork', 'lightgbm', 'bnlearn', 'visNetwork', 'spdep')
## # Run the following command to verify that the required packages are installed.
## # If some package is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
   if (!require(x, character.only = TRUE)) {
     install.packages(x, dependencies = TRUE)
   }
})


## ---- warning = F, message = F--------------------------------------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(imputeTS)
library(gridExtra)
library(SHAPforxgboost)
library(bnlearn)
library(Rgraphviz)
library(visNetwork)
library(lightgbm)
library(spdep)
'%notin%' = Negate('%in%') # 함수 설정


## -------------------------------------------------------------------------------------------------------------------------------------
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


## -------------------------------------------------------------------------------------------------------------------------------------
raw_data = fread('data/01_음식물쓰레기_FOOD_WASTE_210811_update.csv')
daily_raw_data = fread('data/daily_imputed_joined_data.csv')
dong_boundary = fread('data/dong_boundary.csv')
monthly_raw_data = fread('data/monthly_imputed_joined_data.csv')
'%notin%' = Negate('%in%')

em_g_data = raw_data %>% group_by(emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% arrange(desc(em_g))


## ---- fig.align='center', out.width = '90%'-------------------------------------------------------------------------------------------
ggplot() + 
  geom_polygon(data = dong_boundary %>% left_join(em_g_data, by = c('읍면동명칭' = 'emd_nm')) , 
               aes(x=long, y=lat, group=id, fill = em_g), color = 'black', alpha = 0.8) +
  coord_map() +
  theme_bw() +
  scale_fill_distiller(type = 'seq', palette = 'Blues', direction = 1, guide = 'colourbar', aesthetics = 'fill') +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
interest_dong = c('건입동', '봉개동')

monthly_trend_data = 
  raw_data %>% filter(emd_nm %in% interest_dong) %>% 
  mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym()) %>% 
  select(base_date, emd_nm, em_g)

monthly_trend_data %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), size = 2) + 
  geom_vline(xintercept = as.Date('2018-01-01'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2019-01-01'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2020-01-01'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2021-01-01'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2018-07-01'), linetype = 3, color = 'red', size = 2) + 
  geom_vline(xintercept = as.Date('2019-07-01'), linetype = 3, color = 'red', size = 2) + 
  geom_vline(xintercept = as.Date('2020-07-01'), linetype = 3, color = 'red', size = 2) + 
  geom_vline(xintercept = as.Date('2021-07-01'), linetype = 3, color = 'red', size = 2) + 
  facet_wrap(~emd_nm, nrow = 2, scales = 'free') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        #axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daily_trend_data = 
  raw_data %>% filter(emd_nm %in% interest_dong) %>% 
  group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup()

daily_trend_data %>% ggplot() + 
  geom_line(aes(x = base_date, y = em_g), size = 2) + 
  geom_vline(xintercept = as.Date('2020-03-01'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2020-03-08'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2020-03-15'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2020-03-22'), linetype = 2, color = 'blue', size = 1) + 
  geom_vline(xintercept = as.Date('2020-03-29'), linetype = 2, color = 'blue', size = 1) + 
  facet_wrap(~emd_nm, nrow = 2, scale = 'free') + 
  xlim(as.Date('2020-03-01'), as.Date('2020-04-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
fil1 = c('남원읍', '대륜동', '대천동', '동홍동', '서홍동', '송산동', '예래동', '정방동', '중문동', '중앙동', '천지동')
fil2 = c('영천동', '효돈동')
fil3 = c('구좌읍', '애월읍', '조천읍', '한경면', '한림읍')
fil4 = c('안덕면', '대정읍', '성산읍', '표선면')


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
daecheon_monthly_trend_data = 
  raw_data %>% filter(emd_nm %in% '대천동') %>% 
  mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym()) %>% 
  select(base_date, emd_nm, em_g)

daecheon_monthly_trend_data %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), size = 2) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'gray5', alpha = 0.8) +
  geom_polygon(data = dong_boundary %>% filter(`읍면동명칭` %in% fil1), 
               aes(x=long, y=lat, group=id), fill = 'cornflowerblue', color = 'gray5', alpha = 0.7) +
  coord_map() +
  theme_bw() +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff0000", space = "Lab", guide = "colourbar")+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
hyodon_monthly_trend_data = 
  raw_data %>% filter(emd_nm %in% '효돈동') %>% 
  mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym()) %>% 
  select(base_date, emd_nm, em_g)

hyodon_monthly_trend_data %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), size = 2) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        #axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'gray5', alpha = 0.8) +
  geom_polygon(data = dong_boundary %>% filter(`읍면동명칭` %in% c(fil2, fil4)), 
               aes(x=long, y=lat, group=id), fill = 'darksalmon', color = 'gray5', alpha = 0.7) +
  coord_map() +
  theme_bw() +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff0000", space = "Lab", guide = "colourbar")+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
goojwa_monthly_trend_data = 
  raw_data %>% filter(emd_nm %in% '구좌읍') %>% 
  mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym()) %>% 
  select(base_date, emd_nm, em_g)

goojwa_monthly_trend_data %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), size = 2) + 
  xlim(as.Date('2018-01-01'), as.Date('2021-07-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        #axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'gray5', alpha = 0.8) +
  geom_polygon(data = dong_boundary %>% filter(`읍면동명칭` %in% fil4), 
               aes(x=long, y=lat, group=id), fill = 'mediumaquamarine', color = 'gray5', alpha = 0.5) +
  coord_map() +
  theme_bw() +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff0000", space = "Lab", guide = "colourbar")+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
daecheon = raw_data %>% filter(emd_nm == '대천동') %>% select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt)
daecheon$em_area_cd %>% unique %>% sort


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
daecheon_machine = c('W6Y032', 'W6Y049')

raw_data %>% filter(emd_nm == '대천동' & em_area_cd %in% daecheon_machine) %>% 
  select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt) %>% 
  ggplot() + geom_line(aes(x=base_date, y=em_g)) + 
  facet_wrap(~em_area_cd, nrow = 2, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
hyodon = raw_data %>% filter(emd_nm == '효돈동') %>% select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt)
hyodon$em_area_cd %>% unique %>% sort


## ---- fig.align='center', out.width = '90%', message = F------------------------------------------------------------------------------
hyodon_machine = c('W6Y261', 'W6Y264')

raw_data %>% filter(emd_nm == '효돈동' & em_area_cd %in% hyodon_machine) %>% 
  select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt) %>% 
  ggplot() + geom_line(aes(x=base_date, y=em_g)) + 
  facet_wrap(~em_area_cd, nrow = 2, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
goojwa = raw_data %>% filter(emd_nm == '구좌읍') %>% select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt)
goojwa$em_area_cd %>% unique %>% sort


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
goojwa_machine = c('W6X1AF', 'W6XE02')

raw_data %>% filter(emd_nm == '구좌읍' & em_area_cd %in% goojwa_machine) %>% 
  select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt) %>% 
  ggplot() + geom_line(aes(x=base_date, y=em_g)) + 
  facet_wrap(~em_area_cd, nrow = 2, scales = "free_y") +
  xlim(as.Date('2018-01-01'), as.Date('2021-07-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- message = F---------------------------------------------------------------------------------------------------------------------
outlier_dong = c('남원읍', '대륜동', '대천동', '동홍동', '서홍동', '송산동', '예래동', '정방동', '중문동', '중앙동', '천지동', 
                 '효돈동', '안덕면', '대정읍', '영천동')
jeju_na_list = c('구좌읍', '애월읍', '조천읍', '한경면', '한림읍') 
daily_date_time = raw_data$base_date %>% unique()

seoguipo = raw_data %>% filter(city == '서귀포시') %>% select(emd_nm) %>% unique %>% unlist %>% as.vector

na_20201023 = data.frame(rep(as.Date('2020-10-23'), 17), seoguipo, NA)
colnames(na_20201023) = c('base_date', 'emd_nm', 'sum_em_g')

daily_raw_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(sum_em_g = sum(em_g)) %>% ungroup() %>% 
  rbind(na_20201023) %>% arrange(base_date, emd_nm)

make_na_index = daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-06-30')

all_n = daily_date_time %>% unlist() %>% length() 
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 547:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

daecheon_na_dong = daily_raw_data %>% filter(emd_nm == '대천동') %>% select(-emd_nm)
daecheon_spline = data.frame(daily_date_time, na_interpolation(daecheon_na_dong$sum_em_g, option = 'spline'))
colnames(daecheon_spline) = c('yyyymm', 'daily_em_g')

est_daecheon = lm(daecheon_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_daecheon = ((est_daecheon$residuals^2 %>% sum) / n) %>% sqrt
est_daecheon_coef = est_daecheon$coefficients

time_seq = 1:all_n
y_time_seq = est_daecheon_coef[1] + est_daecheon_coef[2]*time_seq + 
  est_daecheon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_daecheon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_daecheon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_daecheon_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_daecheon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_daecheon_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_daecheon)
daecheon_em_g = c(daecheon_spline[1:365, 'daily_em_g'], y_time_seq[366:546], daecheon_spline[547:all_n, 'daily_em_g'])
daecheon_fill_na = data.frame(daily_date_time, '대천동', daecheon_em_g)
colnames(daecheon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

y_time_seq1 = est_daecheon_coef[1] + est_daecheon_coef[2]*time_seq + 
  est_daecheon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_daecheon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_daecheon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_daecheon_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_daecheon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_daecheon_coef[8]*sin(m3*2*pi/all_n * time_seq) 
daecheon_fill_na1 = data.frame(daily_date_time, '대천동', y_time_seq1)
colnames(daecheon_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daecheon_fill_na[1:365, ] %>% ggplot() +
  geom_line(data = daecheon_fill_na[538:all_n, ], aes(x = yyyymm, y = daily_em_g)) +
  geom_line(data = daecheon_na_dong[366:537, ], aes(x = base_date, y = sum_em_g, color = 'red', alpha = 0.5)) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daecheon_fill_na[1:365, ] %>% ggplot() +
  geom_line(data = daecheon_fill_na[538:all_n, ], aes(x = yyyymm, y = daily_em_g)) +
  geom_line(data = daecheon_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') + 
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daecheon_fill_na %>% ggplot() +
  geom_line(data = daecheon_na_dong[366:535, ], aes(x = base_date, y = sum_em_g, color = 'red', alpha = 0.9)) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
n = 365
t = 1:n
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

goojwa_na_dong = daily_raw_data %>% filter(emd_nm == '구좌읍')
goojwa_obs_vec = goojwa_na_dong[goojwa_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist()

est_goojwa = lm(goojwa_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
set.seed(42)
sd_goojwa = ((est_goojwa$residuals^2 %>% sum) / n) %>% sqrt
est_goojwa_coef = est_goojwa$coefficients

y_time_seq = est_goojwa_coef[1] + est_goojwa_coef[2]*time_seq + 
  est_goojwa_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_goojwa_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_goojwa_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_goojwa_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_goojwa_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_goojwa_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_goojwa)

rev_goojwa = c(goojwa_obs_vec[365:1], y_time_seq[366:all_n])
goojwa_fill_na = data.frame(daily_date_time, '구좌읍', rev_goojwa[all_n:1])
colnames(goojwa_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

y_time_seq1 = est_goojwa_coef[1] + est_goojwa_coef[2]*time_seq + 
  est_goojwa_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_goojwa_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_goojwa_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_goojwa_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_goojwa_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_goojwa_coef[8]*sin(m3*2*pi/all_n * time_seq)
goojwa_fill_na1 = data.frame(daily_date_time, '구좌읍', y_time_seq1[all_n:1])
colnames(goojwa_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
goojwa_fill_na[(all_n - 364):all_n, ] %>% ggplot() +
  geom_line(data = goojwa_na_dong, aes(x = base_date, y = sum_em_g, color = 'red'), alpha = 0.8) + 
  geom_vline(xintercept = goojwa_fill_na$yyyymm[all_n - n] %>% as.numeric(), linetype = 3, size = 2, color = 'gray20') +
  xlim(as.Date('2018-01-01'), as.Date('2021-07-01')) + 
  geom_line(aes(x = yyyymm, y = daily_em_g), alpha = 0.9) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
goojwa_fill_na[(all_n - 364):all_n, ] %>% ggplot() +
  geom_line(data = goojwa_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') + 
  #geom_line(data = goojwa_na_dong, aes(x = base_date, y = sum_em_g, color = 'red'), alpha = 0.8) + 
  geom_vline(xintercept = goojwa_fill_na$yyyymm[all_n - n] %>% as.numeric(), linetype = 3, size = 2, color = 'gray20') +
  xlim(as.Date('2018-01-01'), as.Date('2021-07-01')) + 
  geom_line(aes(x = yyyymm, y = daily_em_g), alpha = 0.9) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
goojwa_fill_na %>% ggplot() +
  geom_line(data = goojwa_na_dong, aes(x = base_date, y = sum_em_g, color = 'red'), alpha = 0.5) + 
  geom_vline(xintercept = goojwa_fill_na$yyyymm[all_n - n] %>% as.numeric(), linetype = 3, size = 2, color = 'gray20') +
  xlim(as.Date('2018-01-01'), as.Date('2021-07-01')) + 
  geom_line(aes(x = yyyymm, y = daily_em_g), alpha = 0.9) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daily_raw_data_visualize = raw_data %>% group_by(base_date, emd_nm) %>% summarise(sum_em_g = sum(em_g)) %>% ungroup() %>% 
  rbind(na_20201023) %>% arrange(base_date, emd_nm)

all_n = daily_date_time %>% length()
n = all_n
t = 1:all_n # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

hyodon_na_dong = daily_raw_data_visualize %>% filter(emd_nm == '효돈동') %>% select(-emd_nm)
hyodon_spline = data.frame(daily_date_time, na_interpolation(hyodon_na_dong$sum_em_g, option = 'spline'))
colnames(hyodon_spline) = c('yyyymm', 'daily_em_g')

est_hyodon = lm(hyodon_spline$daily_em_g ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
est_hyodon %>% summary
sd_hyodon = ((est_hyodon$residuals^2 %>% sum) / n) %>% sqrt
est_hyodon_coef = est_hyodon$coefficients

time_seq = 1:all_n
y_time_seq = est_hyodon_coef[1] + est_hyodon_coef[2]*time_seq +
  est_hyodon_coef[3]*cos(m1*2*pi/n * time_seq) + est_hyodon_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_hyodon_coef[5]*cos(m2*2*pi/n * time_seq) + est_hyodon_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_hyodon_coef[7]*cos(m3*2*pi/n * time_seq) + est_hyodon_coef[8]*sin(m3*2*pi/n * time_seq) + rnorm(all_n, 0, sd_hyodon)
hyodon_fill_na = data.frame(daily_date_time, '효돈동', c(y_time_seq[1:943], hyodon_spline[944:all_n, 'daily_em_g']))
colnames(hyodon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

y_time_seq1 = est_hyodon_coef[1] + est_hyodon_coef[2]*time_seq +
  est_hyodon_coef[3]*cos(m1*2*pi/n * time_seq) + est_hyodon_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_hyodon_coef[5]*cos(m2*2*pi/n * time_seq) + est_hyodon_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_hyodon_coef[7]*cos(m3*2*pi/n * time_seq) + est_hyodon_coef[8]*sin(m3*2*pi/n * time_seq) 
hyodon_fill_na1 = data.frame(daily_date_time, '효돈동', y_time_seq1)
colnames(hyodon_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')

hyodon_fill_na[944:all_n, ] %>% ggplot() +
  geom_line(data = hyodon_na_dong[1:943, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  geom_line(data = hyodon_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') +
  geom_vline(xintercept = as.Date('2020-08-01'), linetype = 4, size = 1) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-09-01') & daily_date_time <= as.Date('2019-10-16')

all_n = daily_date_time %>% length()
n = daily_date_time %>% length()
t = 1:all_n # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

ahndeok_na_dong = daily_raw_data_visualize %>% filter(emd_nm == '안덕면') %>% select(-emd_nm)
ahndeok_spline = data.frame(daily_date_time, na_interpolation(ahndeok_na_dong$sum_em_g, option = 'spline'))
colnames(ahndeok_spline) = c('yyyymm', 'daily_em_g')

est_ahndeok = lm(ahndeok_spline$daily_em_g[t] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_ahndeok = ((est_ahndeok$residuals^2 %>% sum) / n) %>% sqrt
est_ahndeok_coef = est_ahndeok$coefficients

time_seq = 1:all_n
y_time_seq = est_ahndeok_coef[1] + est_ahndeok_coef[2]*time_seq +
  est_ahndeok_coef[3]*cos(m1*2*pi/n * time_seq) + est_ahndeok_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_ahndeok_coef[5]*cos(m2*2*pi/n * time_seq) + est_ahndeok_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_ahndeok_coef[7]*cos(m3*2*pi/n * time_seq) + est_ahndeok_coef[8]*sin(m3*2*pi/n * time_seq) + rnorm(all_n, 0, sd_ahndeok)
ahndeok_fill_na = data.frame(daily_date_time, '안덕면', c(y_time_seq[1:546], ahndeok_spline[547:608, 'daily_em_g'], 
                                                       y_time_seq[609:654], ahndeok_spline[655:all_n, 'daily_em_g']))
colnames(ahndeok_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

y_time_seq1 = est_ahndeok_coef[1] + est_ahndeok_coef[2]*time_seq +
  est_ahndeok_coef[3]*cos(m1*2*pi/n * time_seq) + est_ahndeok_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_ahndeok_coef[5]*cos(m2*2*pi/n * time_seq) + est_ahndeok_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_ahndeok_coef[7]*cos(m3*2*pi/n * time_seq) + est_ahndeok_coef[8]*sin(m3*2*pi/n * time_seq)
ahndeok_fill_na1 = data.frame(daily_date_time, '안덕면', y_time_seq1)
colnames(ahndeok_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')

ahndeok_fill_na[-c(1:546, 609:654), ] %>% ggplot() +
  geom_line(data = ahndeok_na_dong[1:546, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(data = ahndeok_na_dong[609:654, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  geom_line(data = ahndeok_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## -------------------------------------------------------------------------------------------------------------------------------------
daily_imputed_data = fread('data/daily_imputed_joined_data.csv')

daily_raw_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup() %>% filter(emd_nm != '알수없음')
daily_imputed_data = daily_imputed_data %>% select(yyyymm, emd_nm, daily_em_g) %>% rename(base_date = 'yyyymm')

daily_unimputed_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% 
  ungroup() %>% filter(emd_nm != '알수없음')
monthly_imputed_data = 
  daily_imputed_data %>% mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(monthly_em_g = sum(daily_em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
monthly_imputed_data %>% filter(emd_nm == '대천동') %>% 
  ggplot() + 
  geom_line(data = monthly_imputed_data %>% filter(emd_nm == '대천동') %>% 
              mutate(monthly_em_g = monthly_em_g + runif(1, 1500000, 2500000)),
            aes(x = base_date, y = monthly_em_g), size = 2, linetype = 2, color = 'red', alpha = 0.7)+
  geom_line(aes(x = base_date, y = monthly_em_g), size = 3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
monthly_imputed_data %>% filter(emd_nm == '효돈동') %>% 
  ggplot() + 
  geom_line(data = monthly_imputed_data %>% filter(emd_nm == '효돈동') %>% 
              mutate(monthly_em_g = monthly_em_g + runif(1, 1000000, 2000000)),
            aes(x = base_date, y = monthly_em_g), size = 2, linetype = 2, color = 'red', alpha = 0.7)+
  geom_line(aes(x = base_date, y = monthly_em_g), size = 3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position="none")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
outlier_dong = c('남원읍', '대륜동', '대천동', '동홍동', '서홍동', '송산동', '예래동', '정방동', '중문동', '중앙동', '천지동', 
                 '효돈동', '안덕면', '대정읍', '영천동', '구좌읍', '애월읍', '조천읍', '한경면', '한림읍', '성산읍')
daily_raw_data = fread('data/daily_imputed_joined_data.csv')

daily_unimputed_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% 
  ungroup() %>% filter(emd_nm != '알수없음')
daily_imputed_data = daily_raw_data %>% select(yyyymm, emd_nm, daily_em_g) %>% rename(base_date = 'yyyymm')

daily_imputed_data %>% left_join(daily_unimputed_data) %>% filter(emd_nm %in% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = daily_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


daily_imputed_data %>% left_join(daily_unimputed_data) %>% filter(emd_nm %notin% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = daily_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
## 월별 보간 결과 확인

monthly_unimputed_data = 
  daily_unimputed_data %>% mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym())

monthly_imputed_data = 
  daily_imputed_data %>% mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(monthly_em_g = sum(daily_em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym())


## 월별 시각화 결과입니다.

monthly_imputed_data %>% left_join(monthly_unimputed_data) %>% filter(emd_nm %in% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), size = 2, alpha = 0.8) + 
  geom_line(aes(x = base_date, y = monthly_em_g), size = 1, alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))

monthly_imputed_data %>% left_join(monthly_unimputed_data) %>% filter(emd_nm %notin% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = monthly_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daily_unimputed_data %>% group_by(base_date) %>% summarize(em_g = sum(em_g)) %>% ungroup %>% head(2)
daily_unimputed_data %>% group_by(base_date) %>% summarize(em_g = sum(em_g)) %>% ungroup %>% tail(2)
daily_unimputed_data %>% group_by(base_date) %>% summarize(em_g = sum(em_g)) %>% ungroup %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), alpha = 0.8) + 
  geom_smooth(aes(x = base_date, y = em_g), method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 20), alpha = 0.7, se = F, size = 1) + 
  geom_smooth(aes(x = base_date, y = em_g), method = "gam", color = 'red', 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3), se = F, size = 2) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        #axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
daily_imputed_data %>% group_by(base_date) %>% summarize(em_g = sum(daily_em_g)) %>% ungroup %>% head(2)
daily_imputed_data %>% group_by(base_date) %>% summarize(em_g = sum(daily_em_g)) %>% ungroup %>% tail(2)
daily_imputed_data %>% group_by(base_date) %>% summarize(em_g = sum(daily_em_g)) %>% ungroup %>% ggplot() +
  geom_line(aes(x = base_date, y = em_g), alpha = 0.8) + 
  geom_smooth(aes(x = base_date, y = em_g), method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 20), alpha = 0.7, se = F, size = 1) + 
  geom_smooth(aes(x = base_date, y = em_g), method = "gam", color = 'red', 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3), se = F, size = 2) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
unknown_machine = c('W6X1DE', 'W6Y01', 'W6Y402', 'W6Y71C')

unknown_daily_data = 
  raw_data %>% 
  group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup() %>% 
  filter(emd_nm == '알수없음')

unknown_daily_data %>% filter(base_date < as.Date('2019-02-01')) %>% ggplot() + 
  geom_line(aes(x = base_date, y = em_g)) + 
  geom_line(data = unknown_daily_data %>% filter(base_date > as.Date('2019-09-01') & base_date < as.Date('2020-03-01')), 
            aes(x = base_date, y = em_g)) +
  geom_line(data = unknown_daily_data %>% filter(base_date > as.Date('2020-07-01') & base_date < as.Date('2021-07-01')), 
            aes(x = base_date, y = em_g)) +
  geom_vline(xintercept = as.Date('2018-01-01'), size = 2, linetype = 3, color = 'blue') + 
  geom_vline(xintercept = as.Date('2019-01-01'), size = 2, linetype = 3, color = 'blue') + 
  geom_vline(xintercept = as.Date('2020-01-01'), size = 2, linetype = 3, color = 'blue') + 
  geom_vline(xintercept = as.Date('2021-01-01'), size = 2, linetype = 3, color = 'blue') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
raw_data %>% filter(emd_nm == '알수없음') %>% 
  select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt) %>% 
  filter(em_area_cd %in% unknown_machine) %>% 
  ggplot() + geom_line(aes(x=base_date, y=em_g)) + 
  facet_wrap(~em_area_cd, nrow=4, scales="free_y") +
  theme(legend.position = 'none') + 
  geom_vline(xintercept = as.Date('2018-01-01'), size = 1, linetype = 2, color = 'blue') + 
  geom_vline(xintercept = as.Date('2019-01-01'), size = 1, linetype = 2, color = 'blue') + 
  geom_vline(xintercept = as.Date('2020-01-01'), size = 1, linetype = 2, color = 'blue') + 
  geom_vline(xintercept = as.Date('2020-07-01'), size = 1, linetype = 3, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-01-01'), size = 1, linetype = 2, color = 'blue') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
raw_data %>% filter(emd_nm == '알수없음') %>% 
  select(-city, -emd_cd, -emd_nm, -em_cnt, -pay_amt) %>% 
  filter(em_area_cd == 'W6Y71C') %>% 
  ggplot() + geom_line(aes(x=base_date, y=em_g)) + 
  geom_vline(xintercept = as.Date('2020-07-01'), size = 2, linetype = 3, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-01-01'), size = 2, linetype = 3, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-07-01'), size = 2, linetype = 3, color = 'gray30') + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.position="none", strip.text = element_text(size = 15))


## -------------------------------------------------------------------------------------------------------------------------------------
raw_data %>% filter(base_date > as.Date('2021-06-01') & base_date < as.Date('2021-06-03')) %>% 
  filter(em_area_cd >= 'W6Y70C' & em_area_cd <= 'W6Y72C') %>% tail(5)


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]

check_weekend_after <- fw[, .(daily_em_g=sum(daily_em_g)), keyby=.(yyyymm)]

datebreaks <- seq(as.Date("2021-03-16"), as.Date("2021-03-30"), by="1 day")
ggplot() +
  theme_bw() +
  geom_line(data=check_weekend_after[yyyymm>='2021-03-16' & yyyymm<='2021-03-30'], 
            aes(x=yyyymm, y=daily_em_g),  size=2) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.text.x = element_text(size= 14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_date(breaks=datebreaks, labels = c('21/01/05', '', '', '금', '토', '일', '월',
                                             '', '', '', '금', '토', '일', '월', ''))



## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]
check_peaktime <- fw[, .(daily_em_g=sum(daily_em_g)), keyby=.(base_year, base_month)]

datebreaks <- seq(as.Date("2018-01-01"), as.Date("2021-06-01"), by="1 month")
dd <- c(rep('', 5), '6월', '7월', '8월', rep('', 9), '6월', '7월', '8월', rep('', 9), '6월', '7월', '8월', rep('', 9), '6월')

        
check_peaktime[, c('yyyymm'):=list(as.Date(paste(check_peaktime$base_year, '-',check_peaktime$base_month, '-01', sep='')))]
ggplot() +
  theme_bw() +
  geom_line(data=check_peaktime, 
            aes(x=yyyymm, y=daily_em_g), size=2) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.text.x = element_text(angle = 45),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_date(breaks=datebreaks, labels = dd)




## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]
shift_check <- fw[, .(daily_em_g=sum(daily_em_g), total_visit=sum(total_visit)), keyby=.(base_year, base_month)]
shift_check[, base_year:=as.factor(base_year)]

ggplot()+
  theme_bw() +
  geom_line(data=shift_check, aes(x=base_month, y=daily_em_g, group=base_year, colour=base_year), size=2) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
ggplot()+
  theme_bw() +
  geom_line(data=shift_check, aes(x=base_month, y=total_visit, group=base_year, colour=base_year), size=2) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
ggplot()+
  theme_bw() +
  geom_line(data=shift_check[base_year==2019], 
            aes(x=base_month, y=(total_visit-min(total_visit))/(max(total_visit)-min(total_visit))), colour='steelblue', size=2) +
  geom_line(data=shift_check[base_year==2020],
            aes(x=base_month, y=(daily_em_g-min(daily_em_g))/(max(daily_em_g)-min(daily_em_g))), colour='black',size=2) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]
shift_check35 <- fw[,.(daily_em_g=sum(daily_em_g), total_resd=sum(total_resd), total_market_amt=sum(total_market_amt)),
                    keyby=.(yyyymm)]

now <- shift_check35[36:1277, c('yyyymm', 'daily_em_g')]
past <- shift_check35[1:1242, c('total_resd', 'total_market_amt')]
shift_check35 <- cbind(now, past)

datebreaks <- seq(as.Date("2020-04-14"), as.Date("2020-05-18"), by="7 day")

plot1 = 
  ggplot() +
  theme_bw() +
  geom_line(data=shift_check35[800:834,], aes(x=yyyymm, y=daily_em_g), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

plot2 = 
  ggplot() +
  theme_bw() +
  geom_line(data=shift_check35[800:834,], aes(x=yyyymm, y=total_resd), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

plot3 = 
  ggplot() +
  theme_bw() +
  geom_line(data=shift_check35[800:834,], aes(x=yyyymm, y=total_market_amt), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


gridExtra::grid.arrange(plot1, plot2, plot3, nrow=3)


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]
shift_check63 <- fw[,.(daily_em_g=sum(daily_em_g), total_resd=sum(total_resd), total_use_amt=sum(total_use_amt)),
                    keyby=.(yyyymm)]


now <- shift_check63[64:1277, c('yyyymm', 'daily_em_g')]
past <- shift_check63[1:1214, c('daily_em_g', 'total_resd', 'total_use_amt')]
setnames(past, old=c('daily_em_g'), new=c('past_em_g'))

shift_check63 <- cbind(now, past)

datebreaks <- seq(as.Date("2018-06-12"), as.Date("2018-08-13"), by="7 day")
plot1 <- ggplot() +
  theme_bw() +
  geom_line(data=shift_check63[100:162,], aes(x=yyyymm, y=daily_em_g), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())


plot2 <- ggplot() +
  theme_bw() +
  geom_line(data=shift_check63[100:162,], aes(x=yyyymm, y=past_em_g), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

plot3 <- ggplot() +
  theme_bw() +
  geom_line(data=shift_check63[100:162,], aes(x=yyyymm, y=total_use_amt), size=2) +
  scale_x_date(breaks=datebreaks) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

gridExtra::grid.arrange(plot1, plot2, plot3, nrow=3)


## -------------------------------------------------------------------------------------------------------------------------------------
ca <- ca <- fread('data/04_음식관련 카드소비_CARD_SPENDING.csv')
re <- fread('data/03_거주인구_RESIDENT_POP.csv')
ko <- fread('data/02-1_내국인유동인구_KOREAN.csv')

# ko 일총합
ko <- ko[, .(mean_resd_pop_cnt=sum(resd_pop_cnt), mean_work_pop_cnt=sum(work_pop_cnt), 
             mean_visit_pop_cnt=sum(visit_pop_cnt)), 
         keyby=.(base_date, emd_nm, resd, sex, age)]

setkey(ca, mct_cat_nm)
deli <- ca[J('배달')]
deli <- deli[order(base_date)]
setkey(deli, emd_nm)

sd1d_deli <- deli[J('삼도1동')]
sd1d_deli[, c('base_year', 'base_month'):=list(year(base_date), month(base_date))]
sd1d_deli <- sd1d_deli[, .(use_cnt=sum(use_cnt), use_amt=sum(use_amt)), 
                       keyby=.(base_year, base_month, emd_nm)]
setnames(sd1d_deli, old=c('use_cnt', 'use_amt'), new=c('deli_cnt', 'deli_amt'))

setkey(fw, emd_nm)
sd1d_fw <- fw[J('삼도1동')]
sd1d_fw <- sd1d_fw[, .(daily_em_g=sum(daily_em_g)), 
                   keyby=.(base_year, base_month, emd_nm)]

re <- re[, .(resid=sum(resid_reg_pop), foreign=sum(foreign_pop), total_pop=sum(total_pop)), 
         keyby=.(base_year, base_month, emd_nm)]
setkey(re, emd_nm)
sd1d_re <- re[J('삼도1동')]
sd1d_re

ko_agg <- ko[, .(resd=sum(mean_resd_pop_cnt), work=sum(mean_work_pop_cnt), visit=sum(mean_visit_pop_cnt)), 
             keyby=.(base_date, emd_nm)]
setkey(ko_agg, emd_nm)
sd1d_ko <- ko_agg[J('삼도1동')]
sd1d_ko[, c('base_year', 'base_month'):=list(year(base_date), month(base_date))]
sd1d_ko <- sd1d_ko[, .(resd=sum(resd), work=sum(work), visit=sum(visit)), 
                   keyby=.(base_year, base_month, emd_nm)]

ko_age <- ko[, .(resd=sum(mean_resd_pop_cnt), work=sum(mean_work_pop_cnt), visit=sum(mean_visit_pop_cnt)), 
             keyby=.(base_date, emd_nm, age)]
setkey(ko_age, emd_nm)
sd1d_age <- ko_age[J('삼도1동')]
sd1d_age[, c('base_year', 'base_month'):=list(year(base_date), month(base_date))]
sd1d_age <- sd1d_age[, .(resd=sum(resd), work=sum(work), visit=sum(visit)), 
                     keyby=.(base_year, base_month, emd_nm, age)]
setkey(sd1d_age, age)
sd1d_40 <- sd1d_age[J(40)]

#cor(cbind(sd1d_fw[, c('daily_em_g')], sd1d_40[, c('resd', 'work', 'visit')]))
#cor(cbind(sd1d_fw[, c('daily_em_g')], sd1d_re[, c('resid', 'foreign', 'total_pop')]))
#cor(cbind(sd1d_fw[, c('daily_em_g')], sd1d_deli[, c('deli_cnt', 'deli_amt')]))

rm(ko)


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
ggplot() + 
  theme_bw() +
  geom_point(aes(x=sd1d_40$visit, y=sd1d_fw$daily_em_g))+
  geom_smooth(aes(x=sd1d_40$visit, y=sd1d_fw$daily_em_g), method='lm', colour='navy')+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
ggplot() + 
  theme_bw() +
  geom_point(aes(x=sd1d_re$resid, y=sd1d_fw$daily_em_g))+
  geom_smooth(aes(x=sd1d_re$resid, y=sd1d_fw$daily_em_g), method='lm', colour='navy')+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
ggplot() + 
  theme_bw() +
  geom_point(aes(x=sd1d_deli$deli_amt, y=sd1d_fw$daily_em_g))+
  geom_smooth(aes(x=sd1d_deli$deli_amt, y=sd1d_fw$daily_em_g), method='lm', colour='firebrick')+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())


## -------------------------------------------------------------------------------------------------------------------------------------
fw <- fread('data/daily_imputed_joined_data.csv')
fw <- fw[emd_nm!='알수없음']
fw[, c('emd_nm', 'after_covid'):=
     list(as.factor(emd_nm), as.factor(after_covid))]

# 베이지안 네트워크 plot 그리기 
plot_network = function(dag,strength_df=NULL,undirected=FALSE,
                        group=NA,title=NULL,height=NULL,width=NULL)
{
  edge_size = ifelse(is.null(strength_df),NA,
                     right_join(strength_df, data.frame(dag$arcs[,c(1,2)]))$strength)
  
  nodes = names(dag$nodes)
  nodes = data.frame(id   = nodes,
                     label= nodes,
                     size = 16,
                     font.size= 18,
                     shadow   = TRUE,
                     group    = group)
  
  edges = data.frame(from   = dag$arcs[,1],
                     to     = dag$arcs[,2],
                     value  = edge_size,
                     arrows = list(to=list(enabled=TRUE,scaleFactor=.5)),
                     shadow = TRUE)
  
  if(is.na(group[1]))     nodes = nodes[,-6] # group 없으면
  if(is.na(edge_size)) edges = edges[,-3] # edge size 없으면
  if(undirected)       edges$arrows.to.enabled=FALSE
  
  network=visNetwork(nodes,edges,main=title,height=height, width=width)%>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  return(network)
}


## -------------------------------------------------------------------------------------------------------------------------------------
# 필요한 변수 선택하기
bn <- fw[, c('yyyymm', 'emd_nm', 'daily_em_g', 'total_resd', 'total_work', 'total_visit',
             "total_market_amt", 'total_delivery_amt', 
             "total_eatout_amt")]

bn[, c('total_market_amt', 'total_eatout_amt'):=list(as.numeric(total_market_amt), as.numeric(total_eatout_amt))]

bn_deli <- bn[, -c('total_market_amt', 'total_eatout_amt')]
bn_market <- bn[, -c('total_delivery_amt', 'total_eatout_amt')]
bn_eatout <- bn[, -c('total_market_amt', 'total_delivery_amt')]


# DAG에 포함되지 말아야할 엣지들
black_list_deli <- rbind(tiers2blacklist(list(grep('^total_', colnames(bn_deli), value=T), 'daily_em_g')))
black_list_deli <- rbind(black_list_deli, c('total_delivery_amt', 'total_resd'))
black_list_deli <- rbind(black_list_deli, c('total_delivery_amt', 'total_work'))
black_list_deli <- rbind(black_list_deli, c('total_delivery_amt', 'total_visit'))

black_list_market <- rbind(tiers2blacklist(list(grep('^total_', colnames(bn_market), value=T), 'daily_em_g')))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_resd'))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_work'))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_visit'))

black_list_eatout <- rbind(tiers2blacklist(list(grep('^total_', colnames(bn_eatout), value=T), 'daily_em_g')))
black_list_eatout <- rbind(black_list_eatout, c('total_eatout_amt', 'total_resd'))
black_list_eatout <- rbind(black_list_eatout, c('total_eatout_amt', 'total_work'))
black_list_eatout <- rbind(black_list_eatout, c('total_eatout_amt', 'total_visit'))
                    


# 삼도1동 예시 (ppt 포함)
setkey(bn_deli, emd_nm) ; setkey(bn_market, emd_nm) ; setkey(bn_eatout, emd_nm) ; 
sd1d_bn_deli <- bn_deli[J('삼도1동')]
sd1d_bn_market <- bn_market[J('삼도1동')]
sd1d_bn_eatout <- bn_eatout[J('삼도1동')]

sd1d_bn_deli <- sd1d_bn_deli[, -c('yyyymm', 'emd_nm')]
sd1d_bn_market <- sd1d_bn_market[, -c('yyyymm', 'emd_nm')]
sd1d_bn_eatout <- sd1d_bn_eatout[, -c('yyyymm', 'emd_nm')]


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
DAG_deli  = tabu(sd1d_bn_deli, blacklist = black_list_deli)
group = ifelse(names(DAG_deli$nodes)%in%c('daily_em_g'),0,1)
plot_network(DAG_deli,group=group,title="Hill-Climbing / 삼도1동 / 배달")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
DAG_eatout  = hc(sd1d_bn_eatout, blacklist = black_list_eatout)
group = ifelse(names(DAG_eatout$nodes)%in%c('daily_em_g'),1,2)
plot_network(DAG_eatout,group=group,title="Hill-Climbing / 삼도1동 / 외식")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
bn = daily_raw_data[, c('yyyymm', 'emd_nm', 'daily_em_g', 'total_resd', 'total_work', 'total_visit',
                        "total_market_amt", 'total_delivery_amt', "total_eatout_amt")]

bn[, c('total_market_amt', 'total_eatout_amt'):=list(as.numeric(total_market_amt), as.numeric(total_eatout_amt))]

bn_deli <- bn[, -c('total_market_amt', 'total_eatout_amt')]
bn_market <- bn[, -c('total_delivery_amt', 'total_eatout_amt')]
bn_eatout <- bn[, -c('total_market_amt', 'total_delivery_amt')]

# DAG에 포함되지 말아야할 노드들

black_list_market <- rbind(tiers2blacklist(list(grep('^total_', colnames(bn_market), value=T), 'daily_em_g')))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_resd'))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_work'))
black_list_market <- rbind(black_list_market, c('total_market_amt', 'total_visit'))

# 삼도1동 예시 (ppt 포함)
setkey(bn_deli, emd_nm) ; setkey(bn_market, emd_nm) ; setkey(bn_eatout, emd_nm) ; 
sd1d_bn_market <- bn_market[J('삼도1동')]
sd1d_bn_market <- sd1d_bn_market[, -c('yyyymm', 'emd_nm')]

# 기본 베이지안 네트워크
DAG  = hc(sd1d_bn_market, blacklist = black_list_market)

group = ifelse(names(DAG$nodes)%in%c('daily_em_g'),1,2)
plot_network(DAG,group=group,title="Hill-Climbing")


## ---- fig.align='center', out.width = '90%', message = F, warning = F-----------------------------------------------------------------
fw = monthly_raw_data %>% filter((yyyymm %>% year) != 2021) %>% 
  group_by(emd_nm) %>% 
  summarise(monthly_em_g = mean(monthly_em_g)) %>%  filter(emd_nm != '알수없음') %>% ungroup()

# 제주 지도 파일 불러오기
space  <- rgdal::readOGR("data/Z_SOP_BND_ADM_DONG_PG.shp")

from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
proj4string(space) <- from_crs  

to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
space <- spTransform(space, to_crs)

jeju <- space

jeju@data$ADM_DR_CD <- as.numeric(jeju@data$ADM_DR_CD)
jeju <- jeju[jeju@data$ADM_DR_CD >= 3901011,]
jeju <- jeju[jeju@data$ADM_DR_NM != '추자면',]
jeju <- jeju[jeju@data$ADM_DR_NM != '우도면',]

# 음식물 데이터와 결합
jeju1 <- merge(jeju, fw, by.x="ADM_DR_NM", by.y="emd_nm")

nb <- poly2nb(jeju1, queen=TRUE) #queen neighbours 사용
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

#local 해보기
my_local_m <- localmoran(jeju1$monthly_em_g, lw)
quadrant <- vector(mode="numeric", length=nrow(my_local_m))

# centers the variable of interest around its mean
m.qualification <- jeju1$monthly_em_g - mean(jeju1$monthly_em_g)     

# centers the local Moran's around the mean
m.local <- my_local_m[,1] - mean(my_local_m[,1])    

# significance threshold
signif <- 0.1 

quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[my_local_m[,5]>signif] <- 0   

# plot in r
brks <- c(0,4)
colors <- c("white","red")
plot(jeju1,col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main = '전체')
box()
legend("bottomright", legend = c("insignificant","high-high"),
       fill=colors,bty="n")


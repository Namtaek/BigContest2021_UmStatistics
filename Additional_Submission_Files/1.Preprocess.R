## title: "Preprocess - ECO제주 전처리파일"
## author: "UmStatistics - 권남택, 오정민, 유경민, 이상현"
## date: '2021년 9월 15일 수요일'

## 본 파일은 2021 빅콘테스트 ECO 제주 참가팀 **UmStatistics**의 제출 파일입니다. 
## 이 `Preprocess` 파일에서는 주어진 데이터와 외부 데이터들에 대해 기본적인 **전처리** 작업들이 포함되어 있습니다. 
## 모델링에 대한 내용은 `Modeling` 파일을 참조하시기 바랍니다.


# 0. 사전 작업

## 0.1 라이브러리 로드와 함수

pacotes = c("tidyverse", "data.table", "lubridate", 'imputeTS')

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

## ---- warning = F, message = F-----------------------------------------------------------------
library(tidyverse)    # 데이터 전처리
library(data.table)   # 데이터 전처리
library(lubridate)    # 시간 데이터 전처리
library(imputeTS)     # 시계열 결측치 보간
'%notin%' = Negate('%in%') # 함수 설정

## 0.2 워킹 디렉토리 설정

## ----------------------------------------------------------------------------------------------
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


# 1. 배출량 데이터 전처리

## 1.1 데이터 불러오기

## ----------------------------------------------------------------------------------------------
raw_data = fread('data/01_음식물쓰레기_FOOD_WASTE_210811_update.csv')

daily_date_time = raw_data$base_date %>% unique()
outlier_dong = c('남원읍', '대륜동', '대천동', '동홍동', '서홍동', '송산동', '예래동', '정방동', '중문동', '중앙동', '천지동', 
                 '효돈동', '안덕면', '대정읍', '영천동')
jeju_na_list = c('구좌읍', '애월읍', '조천읍', '한경면', '한림읍') 

seoguipo = 
  raw_data %>% filter(city == '서귀포시') %>% 
  select(emd_nm) %>% unique %>% unlist %>% as.vector # 서귀포시 행정동 추출
#seoguipo %>% length # 17
na_20201023 = data.frame(rep(as.Date('2020-10-23'), 17), seoguipo, NA)  # 해당 시기 결측 고려
colnames(na_20201023) = c('base_date', 'emd_nm', 'sum_em_g') 

daily_raw_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(sum_em_g = sum(em_g)) %>% ungroup() %>% 
  rbind(na_20201023) %>% arrange(base_date, emd_nm)


## 지점별 결측치 보간 ##

### 1.2.1 제주시 구좌읍 외 4개 동

## 20년 7월부터 모든 기기에 관측이 시작된 행정동들입니다. 
## 해당 행정동(구좌읍, 애월읍, 조천읍, 한경면, 한림읍)들은 제주시 읍면지역에 속합니다. 
## 해당 행정동들은 20년 7월부터 21년 6월까지 데이터를 이용해 과거의 데이터를 회귀를 통한 back-casting으로 생성합니다. 
## back-casting은 forecasting의 반대말로, 과거값을 예측하는 것입니다.

## 먼저 구좌읍에 대한 전처리를 주석을 통해 설명하고, 이를 시각화합니다. 
## 나머지 행정동들도 동일한 방식이니, 추가적인 설명은 생략하겠습니다.

## ----------------------------------------------------------------------------------------------
all_n = daily_date_time %>% unlist() %>% length() # 총개수 확인
n = 365 # 20년 7월 ~ 21년 6월이니 총 365개의 데이터
t = 1:n
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365  # 계정설 주기 설정
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t) # 7일 주기 term
costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t) # 반년 주기 term
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t) # 일년 주기 term


## 추정하고자 하는 모수 $\beta_0 \sim \beta_7$의 형태는 다음과 같습니다. 
## 추세와 여러 계절성들을 동시에 추정해, 기존의 데이터가 관측되는 패턴들을 잘 잡아내는 것이 목표입니다.

#### 구좌읍

## ----------------------------------------------------------------------------------------------
goojwa_na_dong = daily_raw_data %>% filter(emd_nm == '구좌읍') # 구좌읍 추출
goojwa_obs_vec = goojwa_na_dong[goojwa_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist() # 배출량 벡터 추출

est_goojwa = lm(goojwa_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3) # 회귀식 추정
set.seed(42) 
# 랜덤 시드 고정. 첫 생성을 고정함으로써, 생성되는 데이터의 패턴은 무작위이지만 재현 가능
sd_goojwa = ((est_goojwa$residuals^2 %>% sum) / n) %>% sqrt  # 회귀식의 분산을 최대가능도방법으로 추정
est_goojwa_coef = est_goojwa$coefficients # 회귀식의 계수들을 추출

time_seq = 1:all_n # 총 만들어지는 타임 시퀀스의 개수
y_time_seq = est_goojwa_coef[1] + est_goojwa_coef[2]*time_seq + 
  est_goojwa_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_goojwa_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_goojwa_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_goojwa_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_goojwa_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_goojwa_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_goojwa)
# 모든 타임시퀀스들을 회귀계수와 매칭시켜서 데이터 생성

rev_goojwa = c(goojwa_obs_vec[365:1], y_time_seq[366:all_n]) # back-casting 결과 저장
goojwa_fill_na = data.frame(daily_date_time, '구좌읍', rev_goojwa[all_n:1])
colnames(goojwa_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

y_time_seq1 = est_goojwa_coef[1] + est_goojwa_coef[2]*time_seq + 
  est_goojwa_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_goojwa_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_goojwa_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_goojwa_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_goojwa_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_goojwa_coef[8]*sin(m3*2*pi/all_n * time_seq)
# 회귀식의 시각화를 위해 랜덤파트 제거
goojwa_fill_na1 = data.frame(daily_date_time, '구좌읍', y_time_seq1[all_n:1]) # backcasting의 인덱스 순서를 뒤집어서 기존대로 반환
colnames(goojwa_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 이 결과를 시각화하면 다음과 같습니다. 데이터의 패턴을 잘 잡아냈고, 이를 바탕으로 데이터를 생성해낸 모습입니다.

## ---- fig.align='center', out.width = '90%'----------------------------------------------------
goojwa_fill_na[(all_n-364):all_n, ] %>% ggplot() +
  geom_line(data = goojwa_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') + 
  geom_line(data = goojwa_na_dong, aes(x = base_date, y = sum_em_g, color = 'red'), alpha = 0.8) + 
  geom_vline(xintercept = goojwa_fill_na$yyyymm[all_n - n] %>% as.numeric(), linetype = 4, size = 1, color = 'gray44') +
  geom_line(aes(x = yyyymm, y = daily_em_g), alpha = 0.9) + 
  labs(title = "구좌읍 일별 추세/계절성 추정") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")

goojwa_fill_na %>% ggplot() +
  geom_line(data = goojwa_na_dong, aes(x = base_date, y = sum_em_g, color = 'red'), alpha = 0.8) + 
  geom_vline(xintercept = goojwa_fill_na$yyyymm[all_n - n] %>% as.numeric(), linetype = 4, size = 1, color = 'gray44') +
  geom_line(aes(x = yyyymm, y = daily_em_g), alpha = 0.9) + 
  labs(title = "구좌읍 일별 추세/계절성 추정기반 데이터 생성") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


## 다음으로 나올 애월읍, 조천읍, 한경면, 한림읍 또한 동일한 방식이니, 추가적인 설명은 생략하겠습니다. 

#### 애월읍

## ----------------------------------------------------------------------------------------------
aeweol_na_dong = daily_raw_data %>% filter(emd_nm == '애월읍')
aeweol_obs_vec = aeweol_na_dong[aeweol_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist()

est_aeweol = lm(aeweol_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_aeweol = ((est_aeweol$residuals^2 %>% sum) / n) %>% sqrt
est_aeweol_coef = est_aeweol$coefficients

y_time_seq = est_aeweol_coef[1] + est_aeweol_coef[2]*time_seq + 
  est_aeweol_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_aeweol_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_aeweol_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_aeweol_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_aeweol_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_aeweol_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_aeweol)

rev_aeweol = c(aeweol_obs_vec[365:1], y_time_seq[366:all_n])
aeweol_fill_na = data.frame(daily_date_time, '애월읍', rev_aeweol[all_n:1])
colnames(aeweol_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 조천읍

## 조천읍, 한경면, 한림읍의 경우, 추정한 추세를 포함할 경우, 초기 관측치가 음수로 생성되거나 지나치게 크게 생성되는 경우가 있었습니다. 
## 따라서 추세는 추정하고, 이를 데이터 생성시에는 포함시키지 않는 방식으로 진행하였습니다.

## ----------------------------------------------------------------------------------------------
chocheon_na_dong = daily_raw_data %>% filter(emd_nm == '조천읍')
chocheon_obs_vec = chocheon_na_dong[chocheon_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist()

est_chocheon = lm(chocheon_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_chocheon = ((est_chocheon$residuals^2 %>% sum) / n) %>% sqrt
est_chocheon_coef = est_chocheon$coefficients

y_time_seq = est_chocheon_coef[1] + #est_chocheon_coef[2]*time_seq + 
  est_chocheon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_chocheon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_chocheon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_chocheon_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_chocheon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_chocheon_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_chocheon)

rev_chocheon = c(chocheon_obs_vec[365:1], y_time_seq[366:all_n])
chocheon_fill_na = data.frame(daily_date_time, '조천읍', rev_chocheon[all_n:1])
colnames(chocheon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 한경면

## ----------------------------------------------------------------------------------------------
hangyeong_na_dong = daily_raw_data %>% filter(emd_nm == '한경면')
hangyeong_obs_vec = hangyeong_na_dong[hangyeong_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist()

est_hangyeong = lm(hangyeong_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_hangyeong = ((est_hangyeong$residuals^2 %>% sum) / n) %>% sqrt
est_hangyeong_coef = est_hangyeong$coefficients

y_time_seq = est_hangyeong_coef[1] + #est_hangyeong_coef[2]*time_seq + 
  est_hangyeong_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_hangyeong_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_hangyeong_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_hangyeong_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_hangyeong_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_hangyeong_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_hangyeong)

rev_hangyeong = c(hangyeong_obs_vec[365:1], y_time_seq[366:all_n])
hangyeong_fill_na = data.frame(daily_date_time, '한경면', rev_hangyeong[all_n:1])
colnames(hangyeong_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 한림읍

## ----------------------------------------------------------------------------------------------
hanrim_na_dong = daily_raw_data %>% filter(emd_nm == '한림읍')
hanrim_obs_vec = hanrim_na_dong[hanrim_na_dong$base_date >= as.Date('2020-07-01'), 'sum_em_g'] %>% unlist()

est_hanrim = lm(hanrim_obs_vec[365:1] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_hanrim = ((est_hanrim$residuals^2 %>% sum) / n) %>% sqrt
est_hanrim_coef = est_hanrim$coefficients

y_time_seq = est_hanrim_coef[1] + #est_hanrim_coef[2]*time_seq + 
  est_hanrim_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_hanrim_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_hanrim_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_hanrim_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_hanrim_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_hanrim_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_hanrim)

rev_hanrim = c(hanrim_obs_vec[365:1], y_time_seq[366:all_n])
hanrim_fill_na = data.frame(daily_date_time, '한림읍', rev_hanrim[all_n:1])
colnames(hanrim_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 1.2.2 서귀포시 대천동 외 10개 행정동

## 19년 초반에 많은 기기들에서 유사한 패턴으로 결측이 발생했습니다. 
## 이는 서귀포시에서 19년에 기기들에 대해 교체작업을 시행했기 때문에 발생한 현상입니다. 
## 이 패턴에 속하는 행정동은 대천동, 남원읍, 대륜동, 동홍동, 서홍동, 송산동, 예래동, 정방동, 중문동, 중앙동, 천지동입니다. 
## 해당 행정동들은 결측이 발생한 부분을 제외한 데이터들로 회귀를 통해 추세와 계절성을 추정합니다. 
## 추정된 회귀식을 바탕으로 이상치를 대체하는 데이터를 생성합니다. 

## 더불어 대부분의 행정동에서 2020년 10월 23일에 결측이 존재했습니다. 
## 이는 실제로 그날 모든 행정동이 쓰레기를 버리지 않았다기보다는, 데이터 수집상의 문제가 있었던 것으로 추측됩니다. 
## 해당 부분의 결측은 **Spline Interpolation(스플라인 보간법)**을 통해 해결했습니다. 
## 길이가 매우 짧기 때문에, 주위 관측치의 정보를 통해 충분히 다룰 수 있다고 보았습니다. 
## 회귀로 추정 후 데이터를 생성하는 부분 이외의 결측은 모두 길이가 매우 짧기 때문에 스플라인을 통해 보간을 진행했습니다.

## 대천동에 대한 전처리 과정은 이전에 구좌읍의 경우와 매우 유사합니다. 
## 결과를 시각화하고, 행정동별로 특이사항이 없는 경우에는 자세한 설명은 생략하겠습니다.

#### 대천동

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-30')
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 516:all_n) # 이상치 부분을 제외
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

daecheon_na_dong = daily_raw_data %>% filter(emd_nm == '대천동') %>% select(-emd_nm)
daecheon_spline = data.frame(daily_date_time, na_interpolation(daecheon_na_dong$sum_em_g, option = 'spline')) #스플라인 보간 자동적으로 진행
colnames(daecheon_spline) = c('yyyymm', 'daily_em_g')

est_daecheon = lm(daecheon_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_daecheon = ((est_daecheon$residuals^2 %>% sum) / n) %>% sqrt
est_daecheon_coef = est_daecheon$coefficients

y_time_seq = est_daecheon_coef[1] + est_daecheon_coef[2]*time_seq + 
  est_daecheon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_daecheon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_daecheon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_daecheon_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_daecheon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_daecheon_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_daecheon)
daecheon_em_g = c(daecheon_spline[1:365, 'daily_em_g'], y_time_seq[366:515], daecheon_spline[516:all_n, 'daily_em_g'])
daecheon_fill_na = data.frame(daily_date_time, '대천동', daecheon_em_g)
colnames(daecheon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 추세/계절성 시각화용
y_time_seq1 = est_daecheon_coef[1] + est_daecheon_coef[2]*time_seq + 
  est_daecheon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_daecheon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_daecheon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_daecheon_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_daecheon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_daecheon_coef[8]*sin(m3*2*pi/all_n * time_seq) 
daecheon_fill_na1 = data.frame(daily_date_time, '대천동', y_time_seq1)
colnames(daecheon_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 대천동의 결과를 시각화하면 다음과 같습니다. 

## ---- fig.align='center', out.width = '90%'----------------------------------------------------
daecheon_fill_na[1:365, ] %>% ggplot() +
  geom_line(data = daecheon_fill_na[516:all_n, ], aes(x = yyyymm, y = daily_em_g)) +
  geom_line(data = daecheon_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') + 
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  labs(title = "대천동 일별 추세/계절성 추정") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

daecheon_fill_na %>% ggplot() +
  geom_line(data = daecheon_na_dong[366:515, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  labs(title = "대천동 일별 추세/계절성 추정기반 데이터 생성") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


## 이후 행정동들도 동일한 방식으로 전처리를 시행했습니다.

#### 남원읍

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-20')
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 506:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

namwon_na_dong = daily_raw_data %>% filter(emd_nm == '남원읍') %>% select(-emd_nm)
namwon_spline = data.frame(daily_date_time, na_interpolation(namwon_na_dong$sum_em_g, option = 'spline'))
colnames(namwon_spline) = c('yyyymm', 'daily_em_g')

est_namwon = lm(namwon_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_namwon = ((est_namwon$residuals^2 %>% sum) / all_n) %>% sqrt
est_namwon_coef = est_namwon$coefficients

y_time_seq = est_namwon_coef[1] + est_namwon_coef[2]*time_seq + 
  est_namwon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_namwon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_namwon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_namwon_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_namwon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_namwon_coef[8]*sin(m2*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_namwon)

namwon_em_g = c(namwon_spline[1:365, 'daily_em_g'], y_time_seq[366:505], namwon_spline[506:all_n, 'daily_em_g'])
namwon_fill_na = data.frame(daily_date_time, '남원읍', namwon_em_g)
colnames(namwon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 대륜동

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-02-01') & daily_date_time <= as.Date('2019-05-30')
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:396, 516:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

daeryun_na_dong = daily_raw_data %>% filter(emd_nm == '대륜동') %>% select(-emd_nm)
daeryun_spline = data.frame(daily_date_time, na_interpolation(daeryun_na_dong$sum_em_g, option = 'spline'))
colnames(daeryun_spline) = c('yyyymm', 'daily_em_g')

est_daeryun = lm(daeryun_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_daeryun = ((est_daeryun$residuals^2 %>% sum) / n) %>% sqrt
est_daeryun_coef = est_daeryun$coefficients

y_time_seq = est_daeryun_coef[1] + est_daeryun_coef[2]*time_seq + 
  est_daeryun_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_daeryun_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_daeryun_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_daeryun_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_daeryun_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_daeryun_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_daeryun)
daeryun_em_g = c(daeryun_spline[1:396, 'daily_em_g'], y_time_seq[397:515], daeryun_spline[516:all_n, 'daily_em_g'])
daeryun_fill_na = data.frame(daily_date_time, '대륜동', daeryun_em_g)
colnames(daeryun_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 동홍동

## 동홍동의 경우 추가적으로 21년 5월 20일부터 6월 20일의 데이터에서 관측지점별로 연속적인 결측이 발견되었습니다. 
## 해당 경향이 다른 행정동들에서 산발적으로 나타나기 때문에, 추가적으로 이상치 대체작업을 진행하였습니다.

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-30')) |
  (daily_date_time >= as.Date('2021-05-20') & daily_date_time <= as.Date('2021-06-20'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 516:1235, 1268:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

donghong_na_dong = daily_raw_data %>% filter(emd_nm == '동홍동') %>% select(-emd_nm)
donghong_spline = data.frame(daily_date_time, na_interpolation(donghong_na_dong$sum_em_g, option = 'spline'))
colnames(donghong_spline) = c('yyyymm', 'daily_em_g')

est_donghong = lm(donghong_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_donghong = ((est_donghong$residuals^2 %>% sum) / n) %>% sqrt
est_donghong_coef = est_donghong$coefficients

y_time_seq = est_donghong_coef[1] + est_donghong_coef[2]*time_seq + 
  est_donghong_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_donghong_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_donghong_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_donghong_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_donghong_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_donghong_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_donghong)
donghong_em_g = c(donghong_spline[1:365, 'daily_em_g'], y_time_seq[366:515], donghong_spline[516:1235, 'daily_em_g'],
                  y_time_seq[1236:1267], donghong_spline[1268:all_n, 'daily_em_g'])
donghong_fill_na = data.frame(daily_date_time, '동홍동', donghong_em_g)
colnames(donghong_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 동홍동의 경우 추가적인 이상치 대체과정으로 처음 등장하기 때문에, 추가적인 시각화를 진행하겠습니다.

## ----  fig.align='center', out.width = '90%'---------------------------------------------------
y_time_seq1 = est_donghong_coef[1] + est_donghong_coef[2]*time_seq + 
  est_donghong_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_donghong_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_donghong_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_donghong_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_donghong_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_donghong_coef[8]*sin(m3*2*pi/all_n * time_seq) 
donghong_fill_na1 = data.frame(daily_date_time, '동홍동', y_time_seq1)
colnames(donghong_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')

donghong_fill_na %>% ggplot() +
  geom_line(data = donghong_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') + 
  geom_line(data = donghong_na_dong[366:515, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(data = donghong_na_dong[1236:1267, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  labs(title = "동홍동 일별 추세/계절성 추정") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


### 서홍동

## 서홍동의 경우에도 21년 5월부터 6월 사이 동홍동과 비슷한 현상이 발생했습니다.

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-20')) | 
  (daily_date_time >= as.Date('2021-05-20') & daily_date_time <= as.Date('2021-06-20'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 506:1235, 1268:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

seohong_na_dong = daily_raw_data %>% filter(emd_nm == '서홍동') %>% select(-emd_nm)
seohong_spline = data.frame(daily_date_time, na_interpolation(seohong_na_dong$sum_em_g, option = 'spline'))
colnames(seohong_spline) = c('yyyymm', 'daily_em_g')

est_seohong = lm(seohong_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_seohong = ((est_seohong$residuals^2 %>% sum) / n) %>% sqrt
est_seohong_coef = est_seohong$coefficients

y_time_seq = est_seohong_coef[1] + est_seohong_coef[2]*time_seq + 
  est_seohong_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_seohong_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_seohong_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_seohong_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_seohong_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_seohong_coef[7]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_seohong)
seohong_em_g = c(seohong_spline[1:365, 'daily_em_g'], y_time_seq[366:505], seohong_spline[506:1235, 'daily_em_g'],
                 y_time_seq[1236:1267], seohong_spline[1268:all_n, 'daily_em_g'])
seohong_fill_na = data.frame(daily_date_time, '서홍동', seohong_em_g)
colnames(seohong_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 송산동

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-02-10') & daily_date_time <= as.Date('2019-05-11')
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:405, 497:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

songsan_na_dong = daily_raw_data %>% filter(emd_nm == '송산동') %>% select(-emd_nm)
songsan_spline = data.frame(daily_date_time, na_interpolation(songsan_na_dong$sum_em_g, option = 'spline'))
colnames(songsan_spline) = c('yyyymm', 'daily_em_g')

est_songsan = lm(songsan_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_songsan = ((est_songsan$residuals^2 %>% sum) / n) %>% sqrt
est_songsan_coef = est_songsan$coefficients

y_time_seq = est_songsan_coef[1] + est_songsan_coef[2]*time_seq + 
  est_songsan_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_songsan_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_songsan_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_songsan_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_songsan_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_songsan_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_songsan)
songsan_em_g = c(songsan_spline[1:405, 'daily_em_g'], y_time_seq[406:496], songsan_spline[497:all_n, 'daily_em_g'])
songsan_fill_na = data.frame(daily_date_time, '송산동', songsan_em_g)
colnames(songsan_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 예래동

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-02-11') & daily_date_time <= as.Date('2019-05-30')
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:406, 516:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

yerae_na_dong = daily_raw_data %>% filter(emd_nm == '예래동') %>% select(-emd_nm)
yerae_spline = data.frame(daily_date_time, na_interpolation(yerae_na_dong$sum_em_g, option = 'spline'))
colnames(yerae_spline) = c('yyyymm', 'daily_em_g')

est_yerae = lm(yerae_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_yerae = ((est_yerae$residuals^2 %>% sum) / n) %>% sqrt
est_yerae_coef = est_yerae$coefficients

y_time_seq = est_yerae_coef[1] + est_yerae_coef[2]*time_seq + 
  est_yerae_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_yerae_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_yerae_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_yerae_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_yerae_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_yerae_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_yerae)
yerae_em_g = c(yerae_spline[1:406, 'daily_em_g'], y_time_seq[407:515], yerae_spline[516:all_n, 'daily_em_g'])
yerae_fill_na = data.frame(daily_date_time, '예래동', yerae_em_g)
colnames(yerae_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 정방동

## 정방동의 경우, 20년 7월 기간에도 연속적인 결측이 발생했습니다. 해당 부분도 대체를 진행했습니다.

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-30')) | 
  (daily_date_time >= as.Date('2020-07-01') & daily_date_time <= as.Date('2020-07-31'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 516:912, 944:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

jeongbang_na_dong = daily_raw_data %>% filter(emd_nm == '정방동') %>% select(-emd_nm)
jeongbang_spline = data.frame(daily_date_time, na_interpolation(jeongbang_na_dong$sum_em_g, option = 'spline'))
colnames(jeongbang_spline) = c('yyyymm', 'daily_em_g')

est_jeongbang = lm(jeongbang_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_jeongbang = ((est_jeongbang$residuals^2 %>% sum) / n) %>% sqrt
est_jeongbang_coef = est_jeongbang$coefficients

y_time_seq = est_jeongbang_coef[1] + est_jeongbang_coef[2]*time_seq + 
  est_jeongbang_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_jeongbang_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_jeongbang_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_jeongbang_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_jeongbang_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_jeongbang_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_jeongbang)
jeongbang_em_g = c(jeongbang_spline[1:365, 'daily_em_g'], y_time_seq[366:515], jeongbang_spline[516:912, 'daily_em_g'],
                   y_time_seq[913:943], jeongbang_spline[944:all_n, 'daily_em_g'])
jeongbang_fill_na = data.frame(daily_date_time, '정방동', jeongbang_em_g)
colnames(jeongbang_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 중문동

## 중문동의 경우에도 정방동과 비슷한 패턴의 결측이 발생했습니다. 

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-02-01') & daily_date_time <= as.Date('2019-05-30')) |
  (daily_date_time >= as.Date('2020-07-01') & daily_date_time <= as.Date('2020-07-15'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:396, 516:912, 928:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

joongmoon_na_dong = daily_raw_data %>% filter(emd_nm == '중문동') %>% select(-emd_nm)
joongmoon_spline = data.frame(daily_date_time, na_interpolation(joongmoon_na_dong$sum_em_g, option = 'spline'))
colnames(joongmoon_spline) = c('yyyymm', 'daily_em_g')

est_joongmoon = lm(joongmoon_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_joongmoon = ((est_joongmoon$residuals^2 %>% sum) / n) %>% sqrt
est_joongmoon_coef = est_joongmoon$coefficients

y_time_seq = est_joongmoon_coef[1] + est_joongmoon_coef[2]*time_seq + 
  est_joongmoon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_joongmoon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_joongmoon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_joongmoon_coef[6]*sin(m2*2*pi/all_n * time_seq) + 
  est_joongmoon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_joongmoon_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_joongmoon)
joongmoon_em_g = c(joongmoon_spline[1:396, 'daily_em_g'], y_time_seq[397:515], joongmoon_spline[516:912, 'daily_em_g'],
                   y_time_seq[913:927], joongmoon_spline[928:all_n, 'daily_em_g'])
joongmoon_fill_na = data.frame(daily_date_time, '중문동', joongmoon_em_g)
colnames(joongmoon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 중앙동

## 중앙동의 경우에도 정방동,중문동과 비슷한 패턴의 결측이 발생했습니다. 

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-02-01') & daily_date_time <= as.Date('2019-06-20')) |
  (daily_date_time >= as.Date('2020-07-01') & daily_date_time <= as.Date('2020-07-15'))

n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:396, 537:912, 928:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

joongang_na_dong = daily_raw_data %>% filter(emd_nm == '중앙동') %>% select(-emd_nm)
joongang_spline = data.frame(daily_date_time, na_interpolation(joongang_na_dong$sum_em_g, option = 'spline'))
colnames(joongang_spline) = c('yyyymm', 'daily_em_g')

est_joongang = lm(joongang_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_joongang = ((est_joongang$residuals^2 %>% sum) / n) %>% sqrt
est_joongang_coef = est_joongang$coefficients

y_time_seq = est_joongang_coef[1] + est_joongang_coef[2]*time_seq + 
  est_joongang_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_joongang_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_joongang_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_joongang_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_joongang_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_joongang_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_joongang)
joongang_em_g = c(joongang_spline[1:396, 'daily_em_g'], y_time_seq[397:536], joongang_spline[537:912, 'daily_em_g'],
                  y_time_seq[913:927], joongang_spline[928:all_n, 'daily_em_g'])
joongang_fill_na = data.frame(daily_date_time, '중앙동', joongang_em_g)
colnames(joongang_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 천지동

## 천지동의 경우에도 정방동, 중문동, 중앙동과 비슷한 패턴의 결측이 발생했습니다. 

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-01-01') & daily_date_time <= as.Date('2019-05-20') |
                   (daily_date_time >= as.Date('2020-06-29') & daily_date_time <= as.Date('2020-07-25')))

n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(1:365, 506:910, 938:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

cheonji_na_dong = daily_raw_data %>% filter(emd_nm == '천지동') %>% select(-emd_nm)
cheonji_spline = data.frame(daily_date_time, na_interpolation(cheonji_na_dong$sum_em_g, option = 'spline'))
colnames(cheonji_spline) = c('yyyymm', 'daily_em_g')

est_cheonji = lm(cheonji_spline$daily_em_g[!make_na_index] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_cheonji = ((est_cheonji$residuals^2 %>% sum) / n) %>% sqrt
est_cheonji_coef = est_cheonji$coefficients

y_time_seq = est_cheonji_coef[1] + est_cheonji_coef[2]*time_seq + 
  est_cheonji_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_cheonji_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_cheonji_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_cheonji_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_cheonji_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_cheonji_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_cheonji)
cheonji_em_g = c(cheonji_spline[1:365, 'daily_em_g'], y_time_seq[366:505], cheonji_spline[506:910, 'daily_em_g'],
                 y_time_seq[911:937], cheonji_spline[938:all_n, 'daily_em_g'])
cheonji_fill_na = data.frame(daily_date_time, '천지동', cheonji_em_g)
colnames(cheonji_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 1.2.3 서귀포시 효돈동 외 5개 동

## 서귀포시의 효돈동, 영천동, 성산읍, 표선면, 안덕면, 대정읍은 모두 결측이 오래동안 지속된 행정동들입니다. 
## 이전의 두 패턴들의 종합이라고 볼 수 있습니다. 
## 관측기간 초반에는 해당 행정동 전역에 RFID 기기가 보급되지 않았던 것으로 추측됩니다. 
## 따라서 모든기기가 관측을 시작한 날부터의 데이터를 정상 데이터로 가정하고, 
## 그 앞부분의 데이터는 정상 데이터의 추세와 계절성을 통해 생성한 데이터로 대체합니다.

## 효돈동이 경우를 예시로 보고, 나머지 행정동에 대한 설명은 생략하겠습니다.

#### 효돈동

## ----------------------------------------------------------------------------------------------
n = all_n - 944 + 1
t = c(944:all_n) # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

hyodon_na_dong = daily_raw_data %>% filter(emd_nm == '효돈동') %>% select(-emd_nm)
hyodon_spline = data.frame(daily_date_time, na_interpolation(hyodon_na_dong$sum_em_g, option = 'spline'))
colnames(hyodon_spline) = c('yyyymm', 'daily_em_g')

est_hyodon = lm(hyodon_spline$daily_em_g[944:all_n] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_hyodon = ((est_hyodon$residuals^2 %>% sum) / n) %>% sqrt
est_hyodon_coef = est_hyodon$coefficients

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


## 효돈동 이상치 결과 시각화입니다.

## ----  fig.align='center', out.width = '90%'---------------------------------------------------
hyodon_fill_na %>% ggplot() +
  geom_line(data = hyodon_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') +
  geom_line(data = hyodon_na_dong[1:943, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  geom_vline(xintercept = as.Date('2020-08-01'), linetype = 4, size = 1) + 
  labs(title = "효돈동 일별 추세/계절성 추정") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


#### 영천동

## ----------------------------------------------------------------------------------------------
n = all_n - 944 + 1
t = c(944:all_n) # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

yeongcheon_na_dong = daily_raw_data %>% filter(emd_nm == '영천동') %>% select(-emd_nm)
yeongcheon_spline = data.frame(daily_date_time, na_interpolation(yeongcheon_na_dong$sum_em_g, option = 'spline'))
colnames(yeongcheon_spline) = c('yyyymm', 'daily_em_g')

est_yeongcheon = lm(yeongcheon_spline$daily_em_g[944:all_n] ~ 1 + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_yeongcheon = ((est_yeongcheon$residuals^2 %>% sum) / n) %>% sqrt
est_yeongcheon_coef = est_yeongcheon$coefficients

y_time_seq = est_yeongcheon_coef[1] + #est_yeongcheon_coef[2]*time_seq +
  est_yeongcheon_coef[2]*cos(m1*2*pi/n * time_seq) + est_yeongcheon_coef[3]*sin(m1*2*pi/n * time_seq) + 
  est_yeongcheon_coef[4]*cos(m2*2*pi/n * time_seq) + est_yeongcheon_coef[5]*sin(m2*2*pi/n * time_seq) +
  est_yeongcheon_coef[6]*cos(m3*2*pi/n * time_seq) + est_yeongcheon_coef[7]*sin(m3*2*pi/n * time_seq) + rnorm(all_n, 0, sd_yeongcheon)
yeongcheon_fill_na = data.frame(daily_date_time, '영천동', c(y_time_seq[1:943], yeongcheon_spline[944:all_n, 'daily_em_g']))
colnames(yeongcheon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 성산읍

## 성산읍은 20년 7월과 21년 5월에 관측기기별로 결측치가 연속적으로 존재합니다. 
## 그 부분도 추가적으로 이상치 대체를 진행합니다.

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2020-06-30') & daily_date_time <= as.Date('2020-07-24')|
  (daily_date_time >= as.Date('2021-05-22') & daily_date_time <= as.Date('2021-06-08'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(366:911, 937:1237, 1256:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

seongsan_na_dong = daily_raw_data %>% filter(emd_nm == '성산읍') %>% select(-emd_nm)
seongsan_spline = data.frame(daily_date_time, na_interpolation(seongsan_na_dong$sum_em_g, option = 'spline'))
colnames(seongsan_spline) = c('yyyymm', 'daily_em_g')

est_seongsan = lm(seongsan_spline$daily_em_g[t] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_seongsan = ((est_seongsan$residuals^2 %>% sum) / n) %>% sqrt
est_seongsan_coef = est_seongsan$coefficients

y_time_seq = est_seongsan_coef[1] + est_seongsan_coef[2]*time_seq + 
  est_seongsan_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_seongsan_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_seongsan_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_seongsan_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_seongsan_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_seongsan_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_seongsan)
seongsan_em_g = c(y_time_seq[1:365], seongsan_spline[366:911, 'daily_em_g'], y_time_seq[912:936], 
                  seongsan_spline[937:1237, 'daily_em_g'], y_time_seq[1238:1255], seongsan_spline[1256:all_n, 'daily_em_g'])
seongsan_fill_na = data.frame(daily_date_time, '성산읍', seongsan_em_g)
colnames(seongsan_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 표선면

## 표선면 또한 성산읍과 같은 현상이 일어나기 때문에 추가적으로 대체작업을 시행합니다.

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2020-06-30') & daily_date_time <= as.Date('2020-07-24')|
  (daily_date_time >= as.Date('2021-05-22') & daily_date_time <= as.Date('2021-06-08'))
n = daily_date_time %>% unlist() %>% length() - make_na_index %>% sum
t = c(366:all_n)
m1 = all_n/7; m2 = all_n/182.5; m3 = all_n/365
costerm1 = cos(m1*2*pi/all_n*t); sinterm1 = sin(m1*2*pi/all_n*t); costerm2 = cos(m2*2*pi/all_n*t); sinterm2 = sin(m2*2*pi/all_n*t)
costerm3 = cos(m3*2*pi/all_n*t); sinterm3 = sin(m3*2*pi/all_n*t)

pyoseon_na_dong = daily_raw_data %>% filter(emd_nm == '표선면') %>% select(-emd_nm)
pyoseon_spline = data.frame(daily_date_time, na_interpolation(pyoseon_na_dong$sum_em_g, option = 'spline'))
colnames(pyoseon_spline) = c('yyyymm', 'daily_em_g')

est_pyoseon = lm(pyoseon_spline$daily_em_g[t] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_pyoseon = ((est_pyoseon$residuals^2 %>% sum) / n) %>% sqrt
est_pyoseon_coef = est_pyoseon$coefficients

y_time_seq = est_pyoseon_coef[1] + est_pyoseon_coef[2]*time_seq + 
  est_pyoseon_coef[3]*cos(m1*2*pi/all_n * time_seq) + est_pyoseon_coef[4]*sin(m1*2*pi/all_n * time_seq) + 
  est_pyoseon_coef[5]*cos(m2*2*pi/all_n * time_seq) + est_pyoseon_coef[6]*sin(m2*2*pi/all_n * time_seq) +
  est_pyoseon_coef[7]*cos(m3*2*pi/all_n * time_seq) + est_pyoseon_coef[8]*sin(m3*2*pi/all_n * time_seq) + rnorm(all_n, 0, sd_pyoseon)
pyoseon_em_g = c(y_time_seq[1:365], pyoseon_spline[366:all_n, 'daily_em_g'])
pyoseon_fill_na = data.frame(daily_date_time, '표선면', pyoseon_em_g)
colnames(pyoseon_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 안덕면

## 안덕면의 경우 2019년 7월부터의 관측치를 정상의 범주로 다루고, 
## 다만 그 안에서 연속적으로 결측이 발생했던 19년 9~10월의 데이터에 대해서 추가적으로 이상치를 대체합니다. 

## ----------------------------------------------------------------------------------------------
make_na_index = daily_date_time >= as.Date('2019-09-01') & daily_date_time <= as.Date('2019-10-16')
n = daily_date_time %>% length() - make_na_index %>% sum - 546
t = c(547:608, 655:all_n) # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

ahndeok_na_dong = daily_raw_data %>% filter(emd_nm == '안덕면') %>% select(-emd_nm)
ahndeok_spline = data.frame(daily_date_time, na_interpolation(ahndeok_na_dong$sum_em_g, option = 'spline'))
colnames(ahndeok_spline) = c('yyyymm', 'daily_em_g')

est_ahndeok = lm(ahndeok_spline$daily_em_g[t] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_ahndeok = ((est_ahndeok$residuals^2 %>% sum) / n) %>% sqrt
est_ahndeok_coef = est_ahndeok$coefficients

y_time_seq = est_ahndeok_coef[1] + est_ahndeok_coef[2]*time_seq +
  est_ahndeok_coef[3]*cos(m1*2*pi/n * time_seq) + est_ahndeok_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_ahndeok_coef[5]*cos(m2*2*pi/n * time_seq) + est_ahndeok_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_ahndeok_coef[7]*cos(m3*2*pi/n * time_seq) + est_ahndeok_coef[8]*sin(m3*2*pi/n * time_seq) + rnorm(all_n, 0, sd_ahndeok)
ahndeok_fill_na = data.frame(daily_date_time, '안덕면', c(y_time_seq[1:546], ahndeok_spline[547:608, 'daily_em_g'], 
                                                       y_time_seq[609:654], ahndeok_spline[655:all_n, 'daily_em_g']))
colnames(ahndeok_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 안덕면의 이상치 대체에 대한 결과 시각화입니다.

## ----  fig.align='center', out.width = '90%'---------------------------------------------------
y_time_seq1 = est_ahndeok_coef[1] + est_ahndeok_coef[2]*time_seq +
  est_ahndeok_coef[3]*cos(m1*2*pi/n * time_seq) + est_ahndeok_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_ahndeok_coef[5]*cos(m2*2*pi/n * time_seq) + est_ahndeok_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_ahndeok_coef[7]*cos(m3*2*pi/n * time_seq) + est_ahndeok_coef[8]*sin(m3*2*pi/n * time_seq)
ahndeok_fill_na1 = data.frame(daily_date_time, '안덕면', y_time_seq1)
colnames(ahndeok_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')

ahndeok_fill_na %>% ggplot() +
  geom_line(data = ahndeok_na_dong[1:546, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(data = ahndeok_na_dong[609:654, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  geom_line(data = ahndeok_fill_na1, aes(x = yyyymm, y = daily_em_g), color = 'blue') +
  labs(title = "안덕면 일별 추세/계절성 추정") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


#### 대정읍

## 대정읍 또한 안덕면과 유사한 패턴을 지닙니다.

## ----------------------------------------------------------------------------------------------
make_na_index = (daily_date_time >= as.Date('2019-09-01') & daily_date_time <= as.Date('2019-10-16')) |
  (daily_date_time >= as.Date('2020-06-21') & daily_date_time <= as.Date('2020-07-10'))
n = daily_date_time %>% length() - make_na_index %>% sum - 546
t = c(547:608, 655:902, 923:all_n) # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t); costerm2 = cos(m2*2*pi/n*t); sinterm2 = sin(m2*2*pi/n*t)
costerm3 = cos(m3*2*pi/n*t); sinterm3 = sin(m3*2*pi/n*t)

daejeong_na_dong = daily_raw_data %>% filter(emd_nm == '대정읍') %>% select(-emd_nm)
daejeong_spline = data.frame(daily_date_time, na_interpolation(daejeong_na_dong$sum_em_g, option = 'spline'))
colnames(daejeong_spline) = c('yyyymm', 'daily_em_g')

est_daejeong = lm(daejeong_spline$daily_em_g[t] ~ 1 + t + costerm1 + sinterm1 + costerm2 + sinterm2 + costerm3 + sinterm3)
sd_daejeong = ((est_daejeong$residuals^2 %>% sum) / n) %>% sqrt
est_daejeong_coef = est_daejeong$coefficients

y_time_seq = est_daejeong_coef[1] + est_daejeong_coef[2]*time_seq +
  est_daejeong_coef[3]*cos(m1*2*pi/n * time_seq) + est_daejeong_coef[4]*sin(m1*2*pi/n * time_seq) + 
  est_daejeong_coef[5]*cos(m2*2*pi/n * time_seq) + est_daejeong_coef[6]*sin(m2*2*pi/n * time_seq) +
  est_daejeong_coef[7]*cos(m3*2*pi/n * time_seq) + est_daejeong_coef[8]*sin(m3*2*pi/n * time_seq) + rnorm(all_n, 0, sd_daejeong)
daejeong_fill_na = data.frame(daily_date_time, '대정읍', c(y_time_seq[1:546], daejeong_spline[547:608, 'daily_em_g'], 
                                                        y_time_seq[609:654], daejeong_spline[655:902, 'daily_em_g'],
                                                        y_time_seq[903:922], daejeong_spline[923:all_n, 'daily_em_g']))
colnames(daejeong_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 이렇게 데이터들에 이상치를 대체하고, 데이터를 우선 합치도록 하겠습니다.

#### 데이터 병합

## ----------------------------------------------------------------------------------------------
outlier_dong = c('남원읍', '대륜동', '대천동', '동홍동', '서홍동', '송산동', '예래동', '정방동', '중문동', '중앙동', '천지동', 
                 '효돈동', '안덕면', '대정읍', '영천동', '구좌읍', '애월읍', '조천읍', '한경면', '한림읍', '성산읍', '표선면')


outlier_dong_interpolation = bind_rows(namwon_fill_na, daeryun_fill_na, daecheon_fill_na, donghong_fill_na, seohong_fill_na, 
                                       songsan_fill_na, yerae_fill_na, jeongbang_fill_na, joongmoon_fill_na, joongang_fill_na,
                                       cheonji_fill_na, hyodon_fill_na, ahndeok_fill_na, daejeong_fill_na, yeongcheon_fill_na,
                                       goojwa_fill_na, aeweol_fill_na, chocheon_fill_na, hangyeong_fill_na, hanrim_fill_na,
                                       seongsan_fill_na, pyoseon_fill_na)


na_dong = c('건입동', '노형동', '도두동', '봉개동', '삼도1동', '삼도2동', '삼양동', '아라동', '연동', 
            '오라동', '외도동', '용담1동', '용담2동', '이도1동', '이도2동', '이호동', '일도1동', '일도2동', '화북동')


### 1.2.4 제주시 행정동 결측치 보간

## 위에서 전처리를 하지 않은 행정동들에는 이상치나 결측치가 인접한 행정동들간에 유사한 패턴으로 나오지 않습니다. 
## 다만 2020년 10월 23일의 경우 대부분의 행정동에서 결측이 발생했으므로, 이를 스플라인 보간법을 통해 채워줍니다. 
## 더불어 매우 짧은 기간 결측이 발생하는 경우도 스플라인 보간법을 통해 다룹니다.

## ---- message = F------------------------------------------------------------------------------
# 건입동
geonip_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '건입동') %>% select(-emd_nm))
geonip_spline = data.frame(daily_date_time, '건입동', na_interpolation(geonip_na_dong$sum_em_g, option = 'spline'))
colnames(geonip_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 노형동
nohyeong_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '노형동') %>% select(-emd_nm))
nohyeong_spline = data.frame(daily_date_time, '노형동', na_interpolation(nohyeong_na_dong$sum_em_g, option = 'spline'))
colnames(nohyeong_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 도두동
dodoo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '도두동') %>% select(-emd_nm))
dodoo_spline = data.frame(daily_date_time, '도두동', na_interpolation(dodoo_na_dong$sum_em_g, option = 'spline'))
colnames(dodoo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 봉개동
bonggae_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '봉개동') %>% select(-emd_nm))
bonggae_spline = data.frame(daily_date_time, '봉개동', na_interpolation(bonggae_na_dong$sum_em_g, option = 'spline'))
colnames(bonggae_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 삼도1동
samdoone_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '삼도1동') %>% select(-emd_nm))
samdoone_spline = data.frame(daily_date_time, '삼도1동', na_interpolation(samdoone_na_dong$sum_em_g, option = 'spline'))
colnames(samdoone_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 삼도2동
samdotwo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '삼도2동') %>% select(-emd_nm))
samdotwo_spline = data.frame(daily_date_time, '삼도2동', na_interpolation(samdotwo_na_dong$sum_em_g, option = 'spline'))
colnames(samdotwo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 삼양동
samyang_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '삼양동') %>% select(-emd_nm))
samyang_spline = data.frame(daily_date_time, '삼양동', na_interpolation(samyang_na_dong$sum_em_g, option = 'spline'))
colnames(samyang_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 아라동
ahrah_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '아라동') %>% select(-emd_nm))
ahrah_spline = data.frame(daily_date_time, '아라동', na_interpolation(ahrah_na_dong$sum_em_g, option = 'spline'))
colnames(ahrah_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 연동
yeon_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '연동') %>% select(-emd_nm))
yeon_spline = data.frame(daily_date_time, '연동', na_interpolation(yeon_na_dong$sum_em_g, option = 'spline'))
colnames(yeon_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 오라동
ohrah_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '오라동') %>% select(-emd_nm))
ohrah_spline = data.frame(daily_date_time, '오라동', na_interpolation(ohrah_na_dong$sum_em_g, option = 'spline'))
colnames(ohrah_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 외도동
oedo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '외도동') %>% select(-emd_nm))
oedo_spline = data.frame(daily_date_time, '외도동', na_interpolation(oedo_na_dong$sum_em_g, option = 'spline'))
colnames(oedo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 용담1동
yongdamone_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '용담1동') %>% select(-emd_nm))
yongdamone_spline = data.frame(daily_date_time, '용담1동', na_interpolation(yongdamone_na_dong$sum_em_g, option = 'spline'))
colnames(yongdamone_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 용담2동
yongdamtwo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '용담2동') %>% select(-emd_nm))
yongdamtwo_spline = data.frame(daily_date_time, '용담2동', na_interpolation(yongdamtwo_na_dong$sum_em_g, option = 'spline'))
colnames(yongdamtwo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 이도2동
idotwo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '이도2동') %>% select(-emd_nm))
idotwo_spline = data.frame(daily_date_time, '이도2동', na_interpolation(idotwo_na_dong$sum_em_g, option = 'spline'))
colnames(idotwo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 이호동
iho_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '이호동') %>% select(-emd_nm))
iho_spline = data.frame(daily_date_time, '이호동', na_interpolation(iho_na_dong$sum_em_g, option = 'spline'))
colnames(iho_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 일도2동
ildotwo_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '일도2동') %>% select(-emd_nm))
ildotwo_spline = data.frame(daily_date_time, '일도2동', na_interpolation(ildotwo_na_dong$sum_em_g, option = 'spline'))
colnames(ildotwo_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

# 화북동
hwabook_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '화북동') %>% select(-emd_nm))
hwabook_spline = data.frame(daily_date_time, '화북동', na_interpolation(hwabook_na_dong$sum_em_g, option = 'spline'))
colnames(hwabook_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### '알수없음'의 경우

## '알수없음'의 경우, 2020년 7월 이후 관측되는 기기가 단 한 개입니다. 따라서 해당 관측기기의 첫 관측부터 다루도록 합니다.

## ---- message = F------------------------------------------------------------------------------
unknown = raw_data %>% filter(emd_nm == '알수없음' & em_area_cd == 'W6Y71C') %>% select(base_date, em_g)
unknown_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% filter(base_date >= as.Date('2020-07-14')) %>% 
  left_join(unknown)
unknown_spline = data.frame(unknown_na_dong$base_date, '알수없음', na_interpolation(unknown_na_dong$em_g, option = 'spline'))
colnames(unknown_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')


#### 이도1동과 일도1동

## 이도1동과 일도1동의 경우, 2021년 6월 10일 이후 연속적인 결측이 발생했습니다. 
## 그에 따라 전체적인 추세로 보았을때 급감하는 형태가 드러납니다. 
## 이도1동과 일도1동의 해당 이상치의 경우, 전체 기간의 데이터를 활용하기보다, 
## 근접한 데이터만 활용해서 이상치를 처리하도록 하겠습니다.

## ---- message = F------------------------------------------------------------------------------
n = 1262 - 1200
t = 1200:1262 # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t)

idoone_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '이도1동') %>% select(-emd_nm))
idoone_spline = data.frame(daily_date_time, '이도1동', na_interpolation(idoone_na_dong$sum_em_g, option = 'spline'))
colnames(idoone_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

est_idoone = lm(idoone_spline$daily_em_g[1200:1262] ~ 1 + t + costerm1 + sinterm1) 
sd_idoone = ((est_idoone$residuals^2 %>% sum) / n) %>% sqrt
est_idoone_coef = est_idoone$coefficients

y_time_seq = est_idoone_coef[1] + est_idoone_coef[2]*time_seq +
  est_idoone_coef[2]*cos(m1*2*pi/n * time_seq) + est_idoone_coef[3]*sin(m1*2*pi/n * time_seq) + rnorm(all_n, 0, sd_idoone)
idoone_fill_na = data.frame(daily_date_time, '이도1동', c(idoone_spline[1:1262, 'daily_em_g'], y_time_seq[1263:all_n]))
colnames(idoone_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


## 이도1동의 보간결과를 시각화하겠습니다.

## ----  fig.align='center', out.width = '90%', warning = F--------------------------------------
y_time_seq1 = est_idoone_coef[1] + est_idoone_coef[2]*time_seq +
  est_idoone_coef[2]*cos(m1*2*pi/n * time_seq) + est_idoone_coef[3]*sin(m1*2*pi/n * time_seq) 
idoone_fill_na1 = data.frame(daily_date_time, '이도1동', y_time_seq1)
colnames(idoone_fill_na1) = c('yyyymm', 'emd_nm', 'daily_em_g')

idoone_fill_na %>% ggplot() +
  geom_line(data = idoone_na_dong[1263:1277, ], aes(x = base_date, y = sum_em_g, color = 'red')) +
  geom_line(aes(x = yyyymm, y = daily_em_g)) + 
  geom_line(data = idoone_fill_na1[1200:1277, ], aes(x = yyyymm, y = daily_em_g), color = 'blue') +
  geom_vline(xintercept = as.Date('2020-08-01'), linetype = 4, size = 1) + 
  xlim(as.Date('2020-01-01'), as.Date('2021-06-30')) + 
  labs(title = "이도1동 일별 추세/계절성 추정") +  
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), legend.position = "none")


## 다음은 일도1동을 이도1동과 동일한 방식으로 보간한 과정입니다.

## ----------------------------------------------------------------------------------------------
n = 1256 - 1200
t = 1200:1256 # for regression
m1 = n/7; m2 = n/182.5; m3 = n/365
costerm1 = cos(m1*2*pi/n*t); sinterm1 = sin(m1*2*pi/n*t)

ildoone_na_dong = daily_date_time %>% as.data.frame() %>% rename('base_date' = '.') %>% 
  left_join(daily_raw_data %>% filter(emd_nm == '일도1동') %>% select(-emd_nm))
ildoone_na_dong[ildoone_na_dong$sum_em_g %>% is.na %>% which, ]
ildoone_spline = data.frame(daily_date_time, '일도1동', na_interpolation(ildoone_na_dong$sum_em_g, option = 'spline'))
colnames(ildoone_spline) = c('yyyymm', 'emd_nm', 'daily_em_g')

est_ildoone = lm(ildoone_spline$daily_em_g[1200:1256] ~ 1 + t + costerm1 + sinterm1) 
sd_ildoone = ((est_ildoone$residuals^2 %>% sum) / n) %>% sqrt
est_ildoone_coef = est_ildoone$coefficients

y_time_seq = est_ildoone_coef[1] + est_ildoone_coef[2]*time_seq +
  est_ildoone_coef[2]*cos(m1*2*pi/n * time_seq) + est_ildoone_coef[3]*sin(m1*2*pi/n * time_seq) + rnorm(all_n, 0, sd_idoone)
ildoone_fill_na = data.frame(daily_date_time, '일도1동', c(ildoone_spline[1:1256, 'daily_em_g'], y_time_seq[1257:all_n]))
colnames(ildoone_fill_na) = c('yyyymm', 'emd_nm', 'daily_em_g')


### 1.2.5 보간 데이터 완성

## 결과들을 모두 종합한 데이터를 만듭니다.

## ----------------------------------------------------------------------------------------------
daily_raw_data = 
  outlier_dong_interpolation %>% 
  bind_rows(geonip_spline, nohyeong_spline, dodoo_spline, bonggae_spline, samdoone_spline,
            samdotwo_spline, samyang_spline, ahrah_spline, yeon_spline,
            ohrah_spline, oedo_spline, yongdamone_spline, yongdamtwo_spline, idoone_fill_na,
            idotwo_spline, iho_spline, ildoone_fill_na, ildotwo_spline, hwabook_spline, unknown_spline) %>% 
  arrange(yyyymm, emd_nm) %>% 
  mutate(daily_em_g = round(daily_em_g, -1))


### 1.2.6 보간 데이터 결과 확인

#### 일별 보간 결과 확인

## ---- message = F, fig.align='center', out.width = '90%', warning = F--------------------------
daily_unimputed_data = raw_data %>% group_by(base_date, emd_nm) %>% summarise(em_g = sum(em_g)) %>% 
  ungroup() %>% filter(emd_nm != '알수없음')
daily_imputed_data = daily_raw_data %>% select(yyyymm, emd_nm, daily_em_g) %>% rename(base_date = 'yyyymm')

daily_imputed_data %>% left_join(daily_unimputed_data) %>% filter(emd_nm %in% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = daily_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')

daily_imputed_data %>% left_join(daily_unimputed_data) %>% filter(emd_nm %notin% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = daily_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')


#### 월별 보간 결과 확인

## ----------------------------------------------------------------------------------------------
monthly_unimputed_data = 
  daily_unimputed_data %>% mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(em_g = sum(em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym())
  
monthly_imputed_data = 
  daily_imputed_data %>% mutate(base_year = year(base_date), base_month = month(base_date)) %>% 
  group_by(base_year, base_month, emd_nm) %>% summarise(monthly_em_g = sum(daily_em_g)) %>% ungroup %>% 
  mutate(base_date = paste0(base_year,"-", base_month) %>% ym())


## 월별 시각화 결과입니다.

## ---- message = F, fig.align='center', out.width = '90%', warning = F--------------------------
monthly_imputed_data %>% left_join(monthly_unimputed_data) %>% filter(emd_nm %in% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = monthly_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')

monthly_imputed_data %>% left_join(monthly_unimputed_data) %>% filter(emd_nm %notin% outlier_dong) %>% 
  ggplot() + 
  geom_line(aes(x = base_date, y = em_g, color = 'red'), alpha = 0.8) + 
  geom_line(aes(x = base_date, y = monthly_em_g), alpha = 0.8) + 
  facet_wrap(~emd_nm, nrow = 7, scales = 'free') + 
  theme(legend.position = 'none')


# 2. 데이터 병합

## 배출량 데이터에 추가적으로 주어진 데이터와 외부데이터를 결합하는 과정입니다.

## 2.1 기본데이터 병합

### 2.1.1 유동인구 데이터

## 주어진 유동인구 데이터를 불러옵니다. 

## 1) 제주 거주인들의 일별/행정동별 거주세대 비율 변수를 생성합니다.
## 2) 제주도가 아닌 외부거주인들의 일별/행정동별 방문세대 비율변수를 생성합니다.
## 3) 마지막으로 일별/행정동별 총 거주/근무/방문인구 수를 계산합니다.

## ---- message = F------------------------------------------------------------------------------
dat2 = data.table::fread('data/02-1_내국인유동인구_KOREAN.csv')

processed2_resdprop = 
  dat2 %>% 
  mutate(xyz = ifelse(age <= 10, 'baby', ifelse(age <= 30, 'YZ', ifelse(age <= 50, 'X', 'old')))) %>% 
  filter(resd == '제주') %>% 
  group_by(emd_nm, base_date, xyz) %>% 
  summarise(total_resd = sum(resd_pop_cnt)) %>% 
  ungroup() %>% 
  spread(xyz, total_resd) %>% 
  mutate(total = baby + old + X + YZ) %>% 
  mutate(baby_resd_prop = baby/total, YZ_resd_prop = YZ/total, X_resd_prop = X/total, old_resd_prop = old/total) %>% 
  select(-baby, -old, -X, -YZ, -total) 

processed2_visitprop = 
  dat2 %>%  
  mutate(xyz = ifelse(age <= 10, 'baby', ifelse(age <= 30, 'YZ', ifelse(age <= 50, 'X', 'old')))) %>% 
  filter(resd == '그외') %>% 
  group_by(emd_nm, base_date, xyz) %>% 
  summarise(total_visit = sum(visit_pop_cnt)) %>% 
  ungroup() %>% 
  spread(xyz, total_visit) %>% 
  mutate(total = baby + old + X + YZ) %>% 
  mutate(baby_visit_prop = baby/total, YZ_visit_prop = YZ/total, X_visit_prop = X/total, old_visit_prop = old/total) %>% 
  select(-baby, -old, -X, -YZ, -total) 

processed2 = dat2 %>% 
  group_by(emd_nm, base_date) %>% 
  summarise(total_resd = sum(resd_pop_cnt),
            total_work = sum(work_pop_cnt),
            total_visit = sum(visit_pop_cnt)) %>% 
  ungroup()

rm(dat2)


### 2.1.2 장기체류 외국인 유동인구 데이터

## 장기체류 외국인 유동인구 데이터에서는 일별/행정동별 총 외국인 거주/방문/근무인구 수를 구합니다.

## ---- message = F------------------------------------------------------------------------------
dat3 =  fread('data/02-2_장기체류 외국인 유동인구_LONG_TERM_FRGN.csv')

processed3 = dat3 %>% 
  group_by(emd_nm, base_date) %>% 
  summarise(foreign_resd = sum(resd_pop_cnt),
            foreign_visit = sum(visit_pop_cnt),
            foreign_work = sum(work_pop_cnt)) %>% 
  ungroup()

rm(dat3)


### 2.1.3 음식관련 카드소비 데이터

## 해당 데이터에서는 다음의 변수들을 가져온다.

## 1) 일별/행정동별 총 카드소비횟수/금액 
## 2) 일별/행정동별 **배달**에 사용된 카드소비횟수/금액 
## 3) 일별/행정동별 **식료품 구매**에 사용된 카드소비횟수/금액
## 4) 일별/행정동별 **외식**에 사용된 카드소비횟수/금액

## ---- message = F------------------------------------------------------------------------------
dat6 = data.table::fread('data/04_음식관련 카드소비_CARD_SPENDING.csv')

processed6 = dat6 %>% group_by(base_date, emd_nm) %>% 
  summarise(total_use_cnt = sum(use_cnt),
            total_use_amt = sum(use_amt))%>% 
  ungroup()

dat6_delivery = dat6 %>% filter(mct_cat_nm == '배달')
processed6_del = dat6_delivery %>% group_by(base_date, emd_nm) %>% 
  summarise(total_delivery_cnt = sum(use_cnt),
            total_delivery_amt = sum(use_amt)) %>% 
  ungroup()

dat6_market = dat6 %>% filter(mct_cat_nm %in% c('농축수산물', '마트/슈퍼마켓', '식품'))
processed6_market = dat6_market %>% group_by(base_date, emd_nm) %>% 
  summarise(total_market_cnt = sum(use_cnt),
            total_market_amt = sum(use_amt)) %>% 
  ungroup()

dat6_eatout = dat6 %>% filter(mct_cat_nm %in% c('한식', '패스트푸드', '양식', '부페', '아시아음식'))
processed6_eatout = dat6_eatout %>% group_by(base_date, emd_nm) %>% 
  summarise(total_eatout_cnt = sum(use_cnt),
            total_eatout_amt = sum(use_amt)) %>% 
  ungroup()

rm(dat6)
rm(dat6_delivery)
rm(dat6_market)
rm(dat6_eatout)


### 2.2 외부데이터

## 외부데이터로는 행정동의 위경도/주택형태/마트, 코로나, 날씨 데이터를 사용했습니다.

### 2.2.1 위경도 데이터

## 위경도는 행정동별 경계지도 파일에서 구합니다. 
## 경계지도를 그리는 폴리곤들의 위경도 좌표들의 평균을 구해서, 해당 행정동의 좌표로 이용합니다.

## ----------------------------------------------------------------------------------------------
dong_boundary = fread('data/dong_boundary.csv')

dong_center_longlat = 
  dong_boundary %>% 
  group_by(읍면동명칭) %>% 
  summarize(center_long = mean(long),
            center_lat = mean(lat)) %>% 
  ungroup()


## 이후 데이터를 합치고, 배달과 관련한 데이터중에 결측치를 0으로 대체합니다.

## ----------------------------------------------------------------------------------------------
final_data = 
  daily_raw_data %>%
  mutate(base_month = month(yyyymm) ,
         base_year = year(yyyymm)) %>% 
  left_join(processed2, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed2_resdprop, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed2_visitprop, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed3, by = c("emd_nm", 'yyyymm' = "base_date")) %>%
  left_join(processed6, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed6_del, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed6_market, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(processed6_eatout, by = c("emd_nm", 'yyyymm' = "base_date")) %>% 
  left_join(dong_center_longlat, by = c('emd_nm' = '읍면동명칭')) 

# 배달 결측치 0으로 대체
final_data$total_delivery_amt[is.na(final_data$total_delivery_amt)] = 0
final_data$total_delivery_amt[is.na(final_data$total_work)] = NA
final_data$total_delivery_cnt[is.na(final_data$total_delivery_cnt)] = 0
final_data$total_delivery_cnt[is.na(final_data$total_work)] = NA
colSums(is.na(final_data)) # 알수없음 제외 NA는 존재하지 않음


### 2.2.2 주택 형태 데이터

## 행정동별 주택특성 정보를 가져온다. 행정동별 단독주택, 아파트, 연립주택, 다세대주택의 수를 계산한다.

## ---- message = F------------------------------------------------------------------------------
house = fread('data/주택의_종류별_주택읍면동.csv')

house = house %>% filter(연 == 2020 & `행정구역별(읍면동)` %notin% c('제주시', '서귀포시', '우도면', '추자면')) %>% 
  rename(emd_nm = '행정구역별(읍면동)') %>% select(-연) %>% 
  rename(single_house = '단독주택', apartment = '아파트', townhouse = '연립주택', multifamily = '다세대주택') %>% 
  select(-비거주용건물내주택)

final_data = final_data %>% left_join(house, by = 'emd_nm')


### 2.2.3 코로나 데이터

## 코로나 데이터를 통해

## 1) 전국 코로나 일별 확진자
## 2) 제주 코로나 일별 확진자
## 3) 코로나 전후 변수

## 이 세가지 변수를 추가합니다.

## ---- message = F------------------------------------------------------------------------------
covid = fread('data/covid.csv')

covid = daily_date_time %>% as.data.frame() %>% left_join(covid, by = c('.' = 'stdDay')) %>% rename(yyyymm = '.')
# 중복 데이터 발견 후 처리
covid %>% group_by(yyyymm) %>% summarize(n_n = n()) %>% arrange(desc(n_n))
covid[covid$yyyymm == as.Date('2020-04-11') & covid$jeju_covid == 0, ]
covid[covid$yyyymm == as.Date('2020-09-23') & covid$jeju_covid == 0, ]
covid[covid$yyyymm == as.Date('2020-12-08') & covid$all_covid < 0, ]
covid[covid$yyyymm == as.Date('2021-04-29') & covid$jeju_covid == 0, ]
duplicated_idx = c(833, 999, 1076, 1219)
covid = covid[-duplicated_idx, ]


# 관측기간인데 결측인곳 처리
zero_day = c('2020-02-20', '2020-02-21', '2020-02-22', '2020-02-23', '2020-02-24', '2020-02-25',
             '2020-02-26', '2020-02-27', '2020-02-28', '2020-03-01', '2020-03-02', 
             "2020-06-05", "2021-01-30", "2021-01-31", "2021-04-25", "2021-04-26")
covid[covid$yyyymm %in% as.Date(zero_day), 3] = c(73, 100, 229, 169, 231, 144, 284, 505, 571, 586, 599, 
                                                  51, 355, 303, 500, 511)

# 제주도의 정보는 스플라인으로 처리
covid[is.na(covid)] = 0
zero_day2 = c("2020-06-05", "2021-01-30", "2021-01-31", "2021-04-25", "2021-04-26")
covid[covid$yyyymm %in% as.Date(zero_day2), 2] = NA
covid$jeju_covid = covid$jeju_covid %>% na_interpolation(option = 'spline') %>% as.integer

covid = covid %>% mutate(after_covid = ifelse(yyyymm >= as.Date('2020-02-15'), 1, 0))

final_data = final_data %>% left_join(covid, by = 'yyyymm')


### 2.2.4 날씨 데이터

## 날씨데이터를 불러오고, 이를 합칩니다.

## ----------------------------------------------------------------------------------------------
rain2018 = fread('data/weather/OBS_AWS_DD_20210815004909.csv')
rain2019 = fread('data/weather/OBS_AWS_DD_20210815004800.csv')
rain2020 = fread('data/weather/OBS_AWS_DD_20210815004708.csv')
rain2021 = fread('data/weather/OBS_AWS_DD_20210815004446.csv')

rain2018_1 = fread('data/weather/OBS_ASOS_DD_20210815015524.csv')
rain2019_1 = fread('data/weather/OBS_ASOS_DD_20210815015447.csv')
rain2020_1 = fread('data/weather/OBS_ASOS_DD_20210815015400.csv')
rain2021_1 = fread('data/weather/OBS_ASOS_DD_20210815015244.csv')

AWS = rbind(rain2018, rain2019, rain2020, rain2021)
ASOS = rbind(rain2018_1, rain2019_1, rain2020_1, rain2021_1)


## 이후 다소 복잡한 전처리를 통해 위치정보를 대조해줍니다.

## ---- message = F------------------------------------------------------------------------------
# ASOS 처리
final_data$new_nm = final_data$emd_nm
final_data$new_nm = ifelse(final_data$new_nm %in% c('일도2동', '일도1동', '이도1동', '이도2동', '삼도1동', '삼도2동', '용담1동', 
                                                    '용담2동', '건입동', '화북동', '삼양동', '이호동', '도두동'), '제주', final_data$new_nm)
final_data$new_nm = ifelse(final_data$new_nm %in% c('송산동', '정방동', '중앙동', '천지동', '효돈동',
                                                    '영천동', '동홍동', '서홍동'), '서귀포', final_data$new_nm)
final_data$new_nm<- ifelse(final_data$new_nm == '성산읍', '성산', final_data$new_nm)
final_data$new_nm<- ifelse(final_data$new_nm == '한경면', '고산', final_data$new_nm)

names(ASOS) <- c('code', 'new_nm',"yyyymm", 'rain')
ASOS <- ASOS %>% select(-code)

# AWS 처리 
final_data$new_nm<- ifelse(final_data$new_nm == '연동' | final_data$new_nm == '노형동', '어리목', final_data$new_nm)

names(AWS) <- c('code', 'emd_nm',"yyyymm", 'rain')

AWS$new_nm <- AWS$emd_nm

AWS$new_nm<- ifelse (AWS$new_nm == '한림' | AWS$new_nm == '금악', '한림읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '유수암' | AWS$new_nm == '사제비' | AWS$new_nm == '윗세오름', '애월읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '구좌' | AWS$new_nm == '월정' | AWS$new_nm == '송당', '구좌읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '대흘' | AWS$new_nm == '선흘' | AWS$new_nm == '성판악', '조천읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '한라생태숲', '봉개동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '산천단' | AWS$new_nm == '오등', '아라동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '삼각봉', '오라동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '외도', '외도동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '대정', '대정읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '제주남원' | AWS$new_nm == '진달래밭' | AWS$new_nm == '대풍센터', '남원읍', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '서광', '안덕면', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '표선' | AWS$new_nm == '제주가시리', '표선면', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '기상(과)', '대륜동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '강정', '대천동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '영실', '중문동', AWS$new_nm)
AWS$new_nm<- ifelse (AWS$new_nm == '중문', '예래동', AWS$new_nm)

AWS <- AWS %>% select(-code, -emd_nm)

AWS <- AWS %>% group_by(yyyymm,new_nm) %>% summarise(rain = mean(rain)) %>% ungroup
AWS <- AWS[,c(2,1,3)]
rain_final <- rbind(ASOS,AWS)

final_data = final_data %>% left_join(rain_final, by = c("yyyymm", 'new_nm')) %>% select(-new_nm)

colSums(is.na(final_data)) # 결측치 확인
final_data$rain[is.na(final_data$rain)] <- 0  # NA 0으로 채움(관측이 안된날짜)
colSums(is.na(final_data)) # 결측치 확인


### 2.2.5 행정동별 대형마트/대형슈퍼 개수

## 행정동별 대형마트, 대형슈퍼 등의 식료품점의 개수를 크롤링하고, 이들의 가중평균을 주성분분석(PCA)를 통해 구합니다.

## ---- message = F------------------------------------------------------------------------------
shop = fread('data/shop.csv')

shop_pca = shop %>% mutate(sum = 대형마트 + 대형슈퍼 + 슈퍼마켓 + 시장 + 편의점) %>% .[, 2:6] %>% prcomp()
shop$shopping_pca = (shop[, 2:6] %>% as.matrix) %*% (shop_pca$rotation[, 1] %>% as.vector)
shop = shop %>% select(emd_nm, shopping_pca)
final_data = final_data %>% left_join(shop)


#### 데이터 저장

## 현제 일별 데이터들은 다음과 같은 형태입니다.

## ----------------------------------------------------------------------------------------------
final_data %>% head()


## 일별데이터를 다음과 같이 저장합니다

## ----------------------------------------------------------------------------------------------
fwrite(final_data, 'data/daily_imputed_joined_data.csv', row.names = F)


### 3. 월별 데이터 변환

## 일별 데이터를 이후 월별데이터로 변환합니다.

## ---- message = F------------------------------------------------------------------------------
daily_raw_data = fread('data/daily_imputed_joined_data.csv')

monthly_raw_data = 
  daily_raw_data %>% group_by(base_year, base_month, emd_nm) %>% 
  summarise(monthly_em_g = sum(daily_em_g), total_resd = mean(total_resd), total_work = mean(total_work),
            total_visit = mean(total_visit), baby_resd_prop = mean(baby_resd_prop), YZ_resd_prop = mean(YZ_resd_prop),
            X_resd_prop = mean(X_resd_prop), old_resd_prop = mean(old_resd_prop), baby_visit_prop = mean(baby_visit_prop),
            YZ_visit_prop = mean(YZ_visit_prop), X_visit_prop = mean(X_visit_prop), old_visit_prop = mean(old_visit_prop),
            foreign_resd = mean(foreign_resd), foreign_visit = mean(foreign_visit), foreign_work = mean(foreign_work),
            total_use_cnt = mean(total_use_cnt), total_use_amt = mean(total_use_amt), total_delivery_cnt = mean(total_delivery_cnt),
            total_delivery_amt = mean(total_delivery_amt), center_long = mean(center_long), center_lat = mean(center_lat),
            single_house = mean(single_house), apartment = mean(apartment), townhouse = mean(townhouse),
            multifamily = mean(multifamily), jeju_covid = sum(jeju_covid), all_covid = sum(all_covid), rain = sum(rain),
            shopping_pca = mean(shopping_pca)) %>% 
  ungroup() %>% 
  mutate(base_year = paste0(base_year,"-", base_month) %>% ym()) %>% 
  select(-base_month) %>% 
  rename(yyyymm = 'base_year')


## 월별 데이터는 다음과 같은 형태입니다.

## ----------------------------------------------------------------------------------------------
monthly_raw_data %>% head()


## 이를 저장합니다.

## ----------------------------------------------------------------------------------------------
fwrite(monthly_raw_data, 'data/monthly_imputed_joined_data.csv', row.names = F)


## 여기가 코드의 종료입니다. `Modeling` 파일로 넘어가시면 됩니다.

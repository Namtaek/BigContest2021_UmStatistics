## title: "3.Proposal - ECO제주 모델링파일"
## author: "UmStatistics - 권남택, 오정민, 유경민, 이상현"
## date: '2021년 9월 15일 수요일'


## 본 파일은 2021 빅콘테스트 ECO 제주 참가팀 **UmStatistics**의 마크다운 제출 파일입니다. 
## 이 `Proposal` 파일에서는 정책제안을 위한 분석 파일들이 포함되어 있습니다.
## 이중차분법 디자인을 위한 클러스터링과 추세확인, 
## 그리고 이상치 탐지를 위한 Shiny 결과를 확인합니다.



# 0. 사전 작업

## 0.1 라이브러리 로드와 함수

pacotes = c("tidyverse", "data.table", "lubridate", 'imputeTS', 'cluster', 'factoextra', 'gridExtra')

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


## ---- warning = F, message = F---------------------------------------------------------------------------------------------------------
library(tidyverse)    # 데이터 전처리
library(data.table)   # 데이터 전처리
library(lubridate)    # 시간 데이터 전처리
library(imputeTS)     # 시계열 결측치 보간
library(cluster)
library(factoextra)
library(gridExtra)
'%notin%' = Negate('%in%') # 함수 설정


## 0.2 워킹 디렉토리 설정

## --------------------------------------------------------------------------------------------------------------------------------------
CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
CURRENT_WORKING_DIR
setwd(CURRENT_WORKING_DIR)


## 0.3 데이터 불러오기

## --------------------------------------------------------------------------------------------------------------------------------------
daily_raw_data = fread('data/daily_imputed_joined_data.csv')
monthly_raw_data = fread('data/monthly_imputed_joined_data.csv')


# 1. 이중차분법 디자인

## 1.1 군집화

## 비슷한 행정동들을 먼저 묶고, 그 안에서 무작위 배정을 시행합니다. 

## ---- message = F----------------------------------------------------------------------------------------------------------------------
# 실루엣 계산 함수 설정
silhouette_score <- function(k, method){
  set.seed(0811)
  if (method=="kmeans"){clst <- kmeans(dat_clust, centers = k, nstart=25)}
  if (method == "kmedoids"){clst <- pam(dat_clust,k)}
  ss <- silhouette(clst$cluster, dist(dat_clust))
  mean(ss[, 3])
}

# 배출량/인구/카드사용량 변수 
dong_data = 
  daily_raw_data %>%   
  mutate(float_pop = total_visit + total_work + total_resd + foreign_visit + foreign_resd + foreign_work ) %>% 
  filter(yyyymm > "2020-01-01") %>% 
  group_by(emd_nm) %>% 
  summarise(em_g = mean(daily_em_g, na.rm=T),
            fpop = mean(float_pop),
            use_amt = mean(total_use_amt)) %>% 
  filter(emd_nm!="알수없음")

# 스케일링
dat_clust = dong_data[,2:4] %>% scale()  


## 해당 데이터를 통해 kmeans 클러스터링을 진행하고, 실루엣계수를 확인합니다. 
## 실루엣 값이 0.5 이상인 경우 군집화가 잘 되었다고 판단합니다.

## --------------------------------------------------------------------------------------------------------------------------------------
set.seed(42)
k = 2:8
km_sil = sapply(k, silhouette_score, method = "kmeans")
km_k = k[which.max(km_sil)]

plot(k, km_sil,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


## k=3이 실루엣도 0.55로 높은 편이며, 해석도 용이하므로 k=3으로 지정하고 클러스터링 결과를 확인합니다.

## ---- fig.align='center', out.width = '90%', message = F, warning = F------------------------------------------------------------------
set.seed(42)
kmeans_result = kmeans(dat_clust, centers = 3)

dong_data$cluster = kmeans_result$cluster

dong_data %>% filter(cluster == 1) 
dong_data %>% filter(cluster == 2)
dong_data %>% filter(cluster == 3)

fviz_cluster(kmeans_result, data = dat_clust) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_blank(),legend.position="none")


## 클러스터링 결과가 상당히 잘 나왔습니다.

## 1.2 무작위 배정

## 클러스터링의 결과로 생긴 행정동들 안에서 무작위 배정을 통해 처지집단과 통제집단을 분리합니다. 
## 랜덤성을 통제하는 것도 잊지 않습니다.

## --------------------------------------------------------------------------------------------------------------------------------------
cluster1_dong_name = dong_data %>% filter(cluster == 1) %>% .$emd_nm  
cluster2_dong_name = dong_data %>% filter(cluster == 2) %>% .$emd_nm  
cluster3_dong_name = dong_data %>% filter(cluster == 3) %>% .$emd_nm  

set.seed(42)
cluster1_dong_treated = cluster1_dong_name %>% sample(length(cluster1_dong_name)/2) %>% sort()
cluster2_dong_treated = cluster2_dong_name %>% sample(length(cluster2_dong_name)/2) %>% sort()
cluster3_dong_treated = cluster3_dong_name %>% sample(length(cluster3_dong_name)/2) %>% sort()

cluster1_dong_control = cluster1_dong_name %>% setdiff(cluster1_dong_treated)
cluster2_dong_control = cluster2_dong_name %>% setdiff(cluster2_dong_treated)
cluster3_dong_control = cluster3_dong_name %>% setdiff(cluster3_dong_treated)

dong_treated = c(cluster1_dong_treated, cluster2_dong_treated, cluster3_dong_treated)
dong_control = c(cluster1_dong_control, cluster2_dong_control, cluster3_dong_control)


## 이후 처치집단은 빨간색, 통제집단은 초록색으로 색을 설정하고, 평행 추세 가정(Parallel Trend Assumption)을 확인합니다.

## ---- fig.align='center', out.width = '90%', message = F, warning = F------------------------------------------------------------------
# 색 설정
cluster1_color = ifelse(cluster1_dong_name %in% cluster1_dong_treated, '#F8766D', '#00BA38')
cluster2_color = ifelse(cluster2_dong_name %in% cluster2_dong_treated, '#F8766D', '#00BA38')
cluster3_color = ifelse(cluster3_dong_name %in% cluster3_dong_treated, '#F8766D', '#00BA38')

# 시각화
cluster1_plot = 
  monthly_raw_data %>% filter(emd_nm %in% cluster1_dong_name) %>% 
  select(yyyymm, emd_nm, monthly_em_g) %>% 
  mutate(assignment = ifelse(emd_nm %in% dong_treated, 'treated', 'control')) %>% 
  ggplot() + 
  geom_line(aes(x = yyyymm, y = monthly_em_g, color = emd_nm), size = 1) + 
  scale_color_manual(values = cluster1_color) +
  geom_vline(xintercept = as.Date('2020-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-09-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-12-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-03-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  xlim(as.Date('2020-06-01'), as.Date('2021-06-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_blank(),legend.position="none")

cluster2_plot = 
  monthly_raw_data %>% filter(emd_nm %in% cluster2_dong_name) %>% 
  select(yyyymm, emd_nm, monthly_em_g) %>% 
  mutate(assignment = ifelse(emd_nm %in% dong_treated, 'treated', 'control')) %>% 
  ggplot() + 
  geom_line(aes(x = yyyymm, y = monthly_em_g, color = emd_nm), size = 1) + 
  scale_color_manual(values = cluster2_color) +
  geom_vline(xintercept = as.Date('2020-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-09-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-12-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-03-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  xlim(as.Date('2020-06-01'), as.Date('2021-06-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_blank(),legend.position="none")

cluster3_plot = 
  monthly_raw_data %>% filter(emd_nm %in% cluster3_dong_name) %>% 
  select(yyyymm, emd_nm, monthly_em_g) %>% 
  mutate(assignment = ifelse(emd_nm %in% dong_treated, 'treated', 'control')) %>% 
  ggplot() + 
  geom_line(aes(x = yyyymm, y = monthly_em_g, color = emd_nm), size = 1) + 
  scale_color_manual(values = cluster3_color) +
  geom_vline(xintercept = as.Date('2020-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-09-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2020-12-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-03-01'), linetype = 3, size = 1, color = 'gray30') + 
  geom_vline(xintercept = as.Date('2021-06-01'), linetype = 3, size = 1, color = 'gray30') + 
  xlim(as.Date('2020-06-01'), as.Date('2021-06-01')) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title = element_blank(),legend.position="none")

grid.arrange(cluster1_plot, cluster2_plot, cluster3_plot, nrow = 3)

## 전체적으로 보았을때, 클러스터 안에서 움직임이 비슷합니다. 
## 따라서 평행추세 가정이 만족되었음을 확인 가능합니다.

#######################################################################################################

# 2. 이상치 탐지 Shiny 구현

# 해당 코드를 .R 파일에서 실행하면 정상적으로 작동합니다. 현재 청크에서는 `eval=F`로 설정하였습니다.
# 궁금한 날짜를 선택하면 이상치여부를 알려주는 방식입니다.

shiny::runGitHub("Umstatistics", "Kyungmin-Yu", ref="main" )


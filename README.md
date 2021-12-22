## 2021 빅콘테스트 데이터분석분야 ECO제주 참가팀 UmStatistics

2021 빅콘테스트 데이터분석분야 퓨처스리그 ECO제주 부문 **제주특별자치도지사상(대상)** 수상작입니다.

## Overview

**과정 요약 추후 작성 예정**

## 발표 PPT

**추후 업로드 예정**

## 파일 구성

1. 결과보고서 : ~~데이터분석분야_퓨처스리그_ECO제주_UmStatistics_결과보고서.pdf~~
2. 평가데이터 : 데이터분석분야_퓨처스리그_ECO제주_UmStatistics_평가데이터.xlsx


3. 추가제출파일(Additional_Submission_Files, 경로에 한글파일을 두지 않기위해 설정했습니다.)  

	- data폴더 : **메인 데이터는 깃헙에 올리지 않았습니다.**
	  - ~~기존의 제공된 데이터 파일들을 여기에 넣어주시면 됩니다. 외부데이터 + 모델링 용 데이터 + Arima모델 최적의 pdq 등~~
	
	- code : 각각 .R/.Rmd/.html 파일로 구성되어 있습니다.
	    - 1.Preprocess.R/.Rmd/.html : 전처리
	    - 2.Modeling.R./.Rmd/.html : `Arima`/`GWR`/`LightGBM`/`BART` 모델링과 예측 
	    - 3.Proposal.R/.Rmd/.html : 정책제안과 관련한 데이터분석 결과
	    - 4.Visualiztion.R/.Rmd/.html : PPT에 담기는 시각화 결과들 재현

	- lightgbm_model : 'R Session Aborted'의 경우, 폴더에서 lightgbm 모델 불러와서 예측
			 실제 결과를 재현 가능.

	- shiny : 이상치탐지 shiny 구현 코드와 코드 실행을 위한 데이터/사진 파일
		 

4. README.txt : 파일 구성과 관련한 설명자료

## 코드실행 유의사항

1. 기존 제공데이터들을 'data' 폴더에 위치시켜 주시기 바랍니다.

2. 워킹디렉토리는 자동적으로 설정되도록 해놓았습니다. 
    다만 기존데이터 파일명이 현재 코드에 설정된 것과 다를 수 있으니 확인 부탁드립니다.

3. 패키지 또한 자동적으로 설치될 수 있도록 코드를 삽입해놓았습니다.

4. 일부 Randomness가 들어가는 부분들에 대해서는 `set.seed()`를 통해 이를 통제해주었습니다. 
    R버전 4.0 이상에서는 다른 환경이더라도 seed가 같다면 결과가 재현됩니다.

5. LightGBM 모델링 과정에서 'R Session Aborted'가 발생할 수 있습니다.
   해당 문제가 발생할 경우, 모델링을 위한 RAM 공간을 충분히 확보해주시기 바랍니다.

   5-1. 만일 RAM 공간이 충분히 확보된 상황에서 해당 문제가 발생한다면,
         주석처리되어있는 부분을 해제하고, 첨부된 LGBM 모델 파일을 불러와서 예측을 진행해주시기 바랍니다.

   5-2. 혹은 .Rmd 파일에서 재현심사를 진행해주시기 바랍니다.
          .Rmd 파일은 'R Session Aborted' 이슈가 없습니다. 

6. 패키지 버전상의 문제로 'tidyverse' 패키지의 'summarize' 함수 뒤에 'ungroup' 함수를 사용하지 않으면 문제가 발생하는 경우들이 있습니다.
   코드 정리시 이를 확인하였으나, 혹시 오류가 생긴다면 'ungroup'함수를 붙여주는 것으로 디버깅이 가능합니다.

7. shiny 폴더에 있는 'app', 'www', 'dong_fw'는 동일한 폴더에 존재해야 작동합니다. 
   결과확인을 위한 실행은 8.을 확인해주시기 바랍니다.

8. shiny app의 결과는 '3.Proposal'의 마지막 줄의 'runGithub' 코드를 실행하면 됩니다.

## 로컬환경

- CPU : Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz   2.59 GHz
- RAM : 16.0GB
- GPU : No GPU Computing

## R 환경

- R 버전 : 4.0.2
  - tidyverse : 1.3.0
  - data.table : 1.13.0
  - lubridate : 1.7.10
  - imputeTS : 3.2
  - forecast : 8.13
  - spgwr : 0.6-34
  - lightgbm : 3.1.0
  - BayesTree : 0.3-1.4
  - cluster : 2.1.0
  - factoextra : 1.0.7
  - gridExtra : 2.3
  - BiocManager : 1.30.16
  - Rgraphviz : 2.34.0
  - SHAPforxgboost : 0.1.1
  - bnlearn : 4.6.1
  - visNetwork : 2.0.9


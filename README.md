# Data_Analysis_Contest
Python
***2023 통계청 데이터 활용대회 최우수상 수상작입니다.**

[Uploading 포스터_4464_대피소 NEWS 효과적인 재난 대응을 위한 대피소 실태조사 및 개선방안 제안.pdf…]()

**효과적인 재난 대응을 위한 대피소 실태조사 및 개선방안 제안**


1. 배경 
 ■ 주제 선정 (경보 보고 놀란 가슴 대피소 보고 안심하자)
최근 경계경보 오발령 이후 대피소에 관한 관심이 증가하였다. 또한, 기후변화로 인해 매년 폭우와 큰 수해가 발생하고 있다. 이러한 각종 위급 재난 발생 시 생명 보호를 위해 가장 시급한 부분은‘대피소의 안전성 확보’이다. 우리 팀은 대피소의 현황과 수용 능력에 대해 철저히 조사하고자 한다. 또한, 현실적인 대피 시간 및 수용력을 반영한‘최적의 대피소 입지’에 대해 연구하고자 한다.


 ■ 분석 필요성(문제점) 및 전략
  • 대한민국의 대피소 현황 시각화                         
전국 시도별로 대피소 수용률을 분석한 결과, 인구 상위 3위인 경기도(12.4%), 서울특별시(14.4%), 부산광역시(12.2%) 모두 낮은 편이며, 전국 17개의 시도 중 15개가 대피소 수용률 50%를 넘기지 못하였다. 
  • 서울의 대피소 현황 시각화
서울시 시군구별 대피소 수용률, 오후 2시의 평균 생활인구 수용률, 산림제외 대피소 미커버지역 등 지표를 산출한 결과, 종합적으로 기존 대피소 인프라가 열악한 ‘강서구’에 대피소 추가 입지 제안을 하고자 한다. 


2. 데이터 분석
 ■ 데이터 선정
  • 데이터 수집 과정 : 분석에 사용할 raw 데이터로 기초지도, 인구, 수요지, 기존 대피소, 후보지 데이터를 수집하였다. 이를 가공하여 입지 선정 알고리즘의 input 데이터로 수요지, 기존 대피소의 수용인원을 계감한 실질 수요량, 후보지 수용인원, 조건 적용 후보지를 도출하였다. 

 ■ 데이터 분석
  • 분석 프로세스 (대피소의 포괄적·균등 제공을 위해 MCLP, P-median 활용)

공익 목적 시설물 입지 선정 방법론으로, 반경 내 수요를 최대화할 수 있는 MCLP 방법과 수요지와의 대피 거리를 최소화할 수 있는 P-Median 방법을 적용하였다. 

 ■ 분석 결과 및 해석

입지 분석 결과, MCLP와 P-median이 공통적으로 선택한 후보지는 다솔경로당 외 총 19곳으로 나왔다. 이 후보지들을 우선적으로 추가 대피소로 선정한다면, 기존 대피소 수의 21%만 늘려도 각종 수용지표가 기존 대비 213.6% 가량 증가하여 보다 효율적인 대피소 확보가 가능하다.


3. 분석 활용 전략
 ■ 기대효과
첫 번째, 공공시설을 기반으로 후보지를 선정하여 유휴공간의 활용성은 높이고 사회적 비용은 절감시킬 수 있다. 두 번째, 실제 대피환경을 고려한 대피소의 경사, 고도, 1인당 면적을 적용해 후보지를 추렸다. 이 과정에서 공간적 형평성의 관점으로 더 많은 대피 취약 계층에게 수월한 대피 기회를 제공할 수 있을 것이다. 세 번째, 입지 선정 시 인구 밀도를 고려하였기 때문에 대피소 1개당 커버 가능 인원을 효과적으로 증가시킬 수 있다.

 ■ 방향제시
앞서 선택한 후보지들은‘임시거주’를 목적으로 하여 접근성과 편의성을 고려하였다. 추후 대피소 증설 시 해당 상황과 예산에 맞게 조건을 변경 및 추가한다면 우리가 제안한 입지의 장점을 기반으로 더욱 효과적인 확장이 가능할 것이다. 더 나아가 대피소 입지분석의 기반이 되는 현재 대피소 데이터에 대한 체계적인 기록 및 재난 이력 데이터의 개선과 대피소 수요에 대한 추가적인 연구가 필요할 것이다.

library(readxl)
library(tidyverse)

getwd()
mylocation = "C:/Users/Byeongjun Cho/Desktop/2020-1/데이터언론학/data_class (2)"
setwd(mylocation)
Metoo = read_xlsx("성폭력_피해_폭로_미투_운동에_대한_인식조사_결과분석표_및_DATA._응답률_20180222.xlsx", 2)
nrow(Metoo)

#1. 여성 (A)명, (B)%, 남성 (C)명, (D)%
A1_1 = Metoo %>%
  count(SQ2)
A1_2 = as.data.frame(A1_1$n/nrow(Metoo)*100)
A1_1
round(A1_2, 1)

#2. Q13_2 ~ Q13_6의 평균(m) 및 표준편차(sd)
anyNA(Metoo[,str_c("Q13_", as.character(c(1:6)))])
A2_1 = Metoo %>%
  select(starts_with("Q13_"),-contains("#")) %>%
  drop_na() %>%
  apply(2, mean) %>%
  round(2)

A2_2 = Metoo %>%
  select(starts_with("Q13_"),-contains("#")) %>%
  drop_na() %>%
  apply(2, sd) %>%
  round(2)
A2_1
A2_2
rbind(A2_1, A2_2)

#3. 응답자 성별에 따른 Q13_2 ~ Q13_6의 평균(m) 및 표준편차(sd)

A3_1_M = Metoo %>%
  select(SQ2, starts_with("Q13_"),-contains("#")) %>%
  filter(SQ2[] == 1) %>%
  apply(2, mean) %>%
  round(2)
  
A3_1_F = Metoo %>%
  select(SQ2, starts_with("Q13_"),-contains("#")) %>%
  filter(SQ2[] == 2) %>%
  apply(2, mean) %>%
  round(2)

A3_2_M = Metoo %>%
  select(SQ2, starts_with("Q13_"),-contains("#")) %>%
  filter(SQ2[] == 1) %>%
  apply(2, sd) %>%
  round(2)

A3_2_F = Metoo %>%
  select(SQ2, starts_with("Q13_"),-contains("#")) %>%
  filter(SQ2[] == 2) %>%
  apply(2, sd) %>%
  round(2)

A3 = rbind(A3_1_M, A3_2_M, A3_1_F, A3_2_F)[,-1]
A3

#4. 응답자 세대/성별에 따른 Q13_2 ~ Q13_6의 평균(m) 및 표준편차(sd)
A4_1_M = Metoo %>%
  select(SQ2, starts_with("Q13_"), contains("SQ1_"), -contains("#")) %>%
  filter(SQ2[]==1)

A4_1_F = Metoo %>%
  select(SQ2, starts_with("Q13_"), contains("SQ1_"), -contains("#")) %>%
  filter(SQ2[]==2)

A4_2_M = Metoo %>%
  select(SQ2, starts_with("Q13_"), contains("SQ1_"), -contains("#")) %>%
  filter(SQ2[]==1)

A4_2_F = Metoo %>%
  select(SQ2, starts_with("Q13_"), contains("SQ1_"), -contains("#")) %>%
  filter(SQ2[]==2)

M_m = aggregate(. ~ SQ1_R, A4_1_M, mean)[,-2]
F_m = aggregate(. ~ SQ1_R, A4_1_F, mean)[,-2]
M_sd = aggregate(. ~ SQ1_R, A4_1_M, sd)[,-2]
F_sd = aggregate(. ~ SQ1_R, A4_1_F, sd)[,-2]
A4 = cbind(rbind(M_m,M_sd), rbind(F_m, F_sd)) %>% round(2)
A4 = A4[c(order(A4$SQ1_R)),-8]


# 5. 평균(m) 시각화
M_m = aggregate(. ~ SQ1_R, A4_1_M, mean)
F_m = aggregate(. ~ SQ1_R, A4_1_F, mean)

M_m = M_m %>% 
  mutate(SQ2= replace(SQ2,1:4,"남성")) %>%
  rename("Q13_1 외모에 대한 노골적 품평/폄하" = "Q13_1",
         "Q13_2 직/간접적인 성적 농담과 음담패설" = "Q13_2",
         "Q13_3 특정 신체부위 쳐다보거나 훑어봄" = "Q13_3",
         "Q13_4 원치 않는 신체 접촉/스킨십" = "Q13_4",
         "Q13_5 원치 않는 술자리 강요/회식 옆자리" = "Q13_5",
         "Q13_6 원치 않는 성관계 요구" = "Q13_6")
F_m = F_m %>%
  mutate(SQ2= replace(SQ2,1:4,"여성")) %>%
  rename("Q13_1 외모에 대한 노골적 품평/폄하" = "Q13_1",
         "Q13_2 직/간접적인 성적 농담과 음담패설" = "Q13_2",
         "Q13_3 특정 신체부위 쳐다보거나 훑어봄" = "Q13_3",
         "Q13_4 원치 않는 신체 접촉/스킨십" = "Q13_4",
         "Q13_5 원치 않는 술자리 강요/회식 옆자리" = "Q13_5",
         "Q13_6 원치 않는 성관계 요구" = "Q13_6")

A5 = rbind(M_m, F_m)

A5_table = A5 %>%
  pivot_longer(cols = "Q13_1 외모에 대한 노골적 품평/폄하":"Q13_6 원치 않는 성관계 요구") %>%
  ggplot(aes(x = SQ2, y = value, fill= as.factor(str_c(SQ1_R,"0대")))) +
  ggtitle("성폭력 관련 피해 경험 여부", subtitle = "Q13. 다음과 같은 상황을 겪은 적이 있으십니까? 있다면 어느 정도였습니까?") +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  labs(x="", y="피해 정도", fill = "연령", caption = "참고 : 1점 전혀 없음, 2점 거의 없음, 3점 가끔 있음") +
  scale_x_discrete(limits=c("남성", "여성")) +
  scale_y_continuous(breaks = 0.5*c(2:6), expand=expansion(mult=0, add = 0)) +
  coord_cartesian() +
  coord_flip(ylim=c(1,3.1)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 16, angle = 0, hjust = 1),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 11, color = "blue",hjust = 1),
        strip.text.x = element_text(size = 11),
        axis.title.x = element_text(face = "bold", size=13, vjust = 0.5),
        axis.title.y = element_text(size=16, angle =0, vjust = 0.5),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")
  ) +
  facet_wrap(~name, nrow = 3, strip.position = "top")
A5_table
ggsave(file=str_c(mylocation,"/A5_graph.jpg"), 
       width=20, height=15, units=c("cm"))
###---------------#####
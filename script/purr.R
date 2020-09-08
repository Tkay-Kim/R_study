
# Introduction to "purr" --------------------------------------------------

install.packages("tidyverse")
library("tidyverse")

kbo <- read.csv("kbo.csv") %>% as_tibble()
str(kbo)

lm(타석당득점~타율, kbo) %>%
  summary() %>% 
  .$r.squared        # 앞에 .을 써서 $를 인식 시킨다.


lm(타석당득점~출루율, kbo) %>%
  summary() %>% 
  .$r.squared        # 앞에 .을 써서 $를 인식 시킨다.


lm(타석당득점~장타력, kbo) %>%
  summary() %>% 
  .$r.squared        # 앞에 .을 써서 $를 인식 시킨다.


lm(타석당득점~OPS, kbo) %>%
  summary() %>% 
  .$r.squared        # 앞에 .을 써서 $를 인식 시킨다.

#위와 같은 상황은 코드화 해보자

# 반복을 반복 ------------------------------------------------------------------


a <- 1:5
for (i in a){
  print(a[i]+1)
}

lapply(a, function(x) x+1)
sapply(a, function(x) x+1)
#purr에 있는 패키지 map
map(a,function(x) x+1)
map_dbl(a,function(x) x+1)

add_one <-function(x) x+1

map_dbl(a, add_one)
sapply(a, add_one)


# 차이는 무엇인가? ---------------------------------------------------------------
#이렇게 쓰는 게 가능하다.
map_dbl(a, ~.x+1)


# map의 유용성 ----------------------------------------------------------------
b<-c(1, 22, 333, 4444, 55555)
map_int(b, str_length)
map_dbl(b, str_length)
map_lgl(b, is.numeric)

c<-c("abc", 'def', 'ghi')
map_chr(c, ~paste0(.x,'z'))

#작업대상이 2개

d<-c(5,4,3,2,1)

map2_dbl(a,d,sum)

#자료가 세 개 이상일 때
pmap_dbl(list(a,b,d), ~..2-..1+..3)


# 데이터를 반복적으로 골라낼 때 --------------------------------------------------------

e<-list(
  list(-1, x=1, y=c(1), z = 'a'),
  list(-2, x=4, y=c(2,3), z= 'b'),
  list(-3, x=9, y=c(4,5,6))
)
e

map_dbl(e, 1)
map_dbl(e, 2)
map_dbl(e, 'x')

map_dbl(e, 'y') # error
map(e, 'y')

map(e, list('y', 1))
map_dbl(e, list('y', 1))

map_chr(e, 'z') # error
map_chr(e, 'z', .default = NA) # error

f <- tibble(a=c(17, 23, 4, 10, 11), 
            b=c(24, 5, 6, 12, 18), 
            c=c(1, 7, 13, 19, 25), 
            d=c(8, 14, 20, 21, 2), 
            e=c(15, 16, 22, 3, 9))
f
f %>% mutate(sum = a+b+c+d+e)

f %>% 
  rowwise() %>% 
  mutate(max = max(a,b,c,d,e))

map(f,sum)
map_df(f,sum)
map_df(f, ~.x+1)

modify(f, ~.x+1) # 원래 자료형 그대로 출력



# kbo ---------------------------------------------------------------------
#필요한 열만 추출
kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>% 
  map(~lm(kbo$타석당득점~.x)) %>% 
  map(summary) %>% 
  map_df('r.squared')

kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>%
  map_df(~lm(kbo$타석당득점~.x) %>%
           summary() %>%
           .$r.squared)

# 컴퓨터는 long form 데이터를 다루는 데 더 익숙하다.
kbo %>% 
  pivot_longer(cols=타율:OPS, names_to ='기록', values_to = '값')

# 옛날방식
kbo %>% 
  gather(key=기록, value = 값, 타율:OPS)

kbo %>% 
  pivot_longer(cols=타율:OPS, names_to ='기록', values_to = '값') %>% 
  group_by(기록) %>% 
  nest() ->kbo2

kbo2$data %>% 
  set_names(., kbo2$기록)

kbo2$data %>% 





library(dplyr)
library(tidyr)
# Ejercicios suelttos

# Lecture 1: What is statistics ----

## 1. Dados (Bob y Alice) ----

expand.grid(
  # Dos dados de 6 caras
  d1 = 1:6,
  d2 = 1:6) %>% 
  mutate(
    # Alice gana $1 si finaliza en primos
    alice = 1*((d1+d2) %in% c(2,3,5,7,11)),
    # Bob gana $3 si los dados son iguales
    bob = 3*(d1==d2)) %>% 
  # Ganancia esperada
  summarise(
    across(alice:bob,~mean(.)))


## 2. Besos con distribución Bernoulli p = 0.5
#     Nota: Como una moneda

expand.grid(
  c1 = 0:1,
  c2 = 0:1,
  c3 = 0:1) %>% 
  mutate(sum = c1+c2+c3) %>% 
  summarise(
    # P(sum[c] >=2)
    P = mean(sum>=2))

## 3. Grupos de amigos

expand.grid(
  # Combinatoria de pares de amigos
  pair = combn(paste0("f",1:4),2,) %>% 
    t() %>% as.data.frame() %>% 
    mutate(pair = paste0(V1,"-",V2)) %>% 
    pull(pair))



expand.grid(
  p1 = 0:1,
  p2 = 0:1,
  p3 = 0:1,
  p4 = 0:1,
  p5 = 0:1,
  p6 = 0:1) %>% 
  mutate(
    t = rowSums(select(.,p1:p6))) %>% 
  # "interesting"
  summarise(
    interesting = mean(t <=5)) %>% 
  pull(interesting)*128


# Lecture 2: Probability Redux ----

## 1. Examen sat

# Pr|respuesta correcta
pr = 1/6
pw = 1-pr
avg_guess = (pr*20)+(pw*-5)


avg_guess

expand.grid(
  q1 = c(20,rep(-5,5)),
  q2 = c(20,rep(-5,5)),
  q3 = c(20,rep(-5,5)),
  q4 = c(20,rep(-5,5))) %>% 
  mutate(
    score = sum(q1+q2+q3+q4)) %>% 
  summarise(mean = mean(score),
            sd = sd(score))

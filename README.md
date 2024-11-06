# plane

install.packages('nycflights13')
require(nycflights13)
library(readr)
library(dplyr)

view(nycflights13::planes)

airlines

#Verificação das chaves
planes %>% 
  count(tailnum) %>%
  filter(n > 1)

planes$tailnum

flights2 <- flights %>% 
  filter(distance > 2000) %>% 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

#left join
flights2_airlines = 
  flights2  %>% 
  left_join(., airlines)

flights2_airlines = 
  flights2  %>% 
  left_join(., airlines, 
            by = "carrier")


#right join
planes_flights = flights2 %>% 
  right_join(planes, by = "tailnum")

#inner join
origin_flights = flights2 %>% 
  inner_join(airports, join_by(origin == faa))

#Para vôos com atraso superior a 24 horas em flights, 
#verifique as condições climáticas em weather. Há algum padrão? 
#Quais os meses do ano em que você encontra os maiores atrasos?

flights3 <- flights %>% 
  filter(arr_delay > 24 | dep_delay > 24) %>% 
  select(year, month, day, time_hour, origin, dest, tailnum, carrier)

weather_flight <- flights3 %>% 
  left_join(., weather) %>% 
  select(year, month, day, time_hour, origin, dest, tailnum, carrier, temp, precip)

##Quais os meses do ano em que você encontra os maiores delay?
weather_flight %>% 
  group_by(month) %>% 
  summarise(
    quantidade = n()
  )
  


#Encontre os 20 destinos mais comuns e identifique seu aeroporto. 

mais_visitado <- flights %>% 
  group_by(dest) %>% 
  summarise(
    mais_comuns = n()
  ) %>% 
  arrange(desc(mais_comuns)) %>% 
    head(20)

#Qual a temperatura média (mensal) em Celcius desses lugares? E a precipiração média, em cm?
dados_combinados <- mais_visitado %>% 
  cross_join(weather) %>% 
  select(mais_comuns, month, temp) %>% 
  summarise(
    quantidade = n()
  )

#Inclua uma coluna com a cia aérea na tabela planes. 
#Quantas companhias áreas voaram cada avião naquele ano
dados_cruzado <- cross_join(planes, airlines)

contagem_empresas <- table(dados_cruzado$name)

#Inclua a latitude e longitude de cada origem destino na tabela flights.
lati_long <- flights %>% 
  left_join(., airports, by = c("origin" = "faa")) %>% 
  select(origin, lat, lon)








#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho) por mês
ggplotly(
  ggplot(tempo_estudante_escola, aes(x = desempenho)) +
    geom_density(aes(color = as.factor(mes), fill = as.factor(mes)), 
                 position = "identity", alpha = 0.2) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma e por mês separadamente
#(função facet_wrap)
tempo_estudante_escola %>% 
  group_by(as.factor(mes)) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(desempenho, n = max(linhas))["x"]),
         y = unlist(density(desempenho, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = as.factor(mes), fill = as.factor(mes)), color = "black", alpha = 0.3) +
  geom_histogram(aes(x = desempenho, y = ..density.., fill = as.factor(mes)), 
                 color = "black", position = 'identity', alpha = 0.1) +
  facet_wrap(~ as.factor(mes)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()


## ICC (intraclass correlation):

#Escola:
(180.19266)/(180.19266 + 325.79915 + 41.64939)

#Estudante:
(325.79915)/(180.19266 + 325.79915 + 41.64939)

#Temporal:
1 - (0.3290342 + 0.5949135)



# PREDICT FIXED ("não")
40.03 + 5.17*1 + 14.7*(0) + 1.18*2 - 0.65*(0)*(1) - 0.056*2*1

# PREDICT FIXED ("sim")
40.03 + 5.17*1 + 14.7*(1) + 1.18*2 - 0.65*(1)*(1) - 0.056*2*1

# PREDICT ESTUDANTE ("não")
40.03 + 5.17*1 + 14.7*(0) + 1.18*2 - 0.65*(0)*(1) - 0.056*2*1 - 8.67 + 0.319*1 - 2.182 - 0.119*1

# PREDICT ESTUDANTE ("sim")
40.03 + 5.17*1 + 14.7*(1) + 1.18*2 - 0.65*(1)*(1) - 0.056*2*1 - 8.67 + 0.319*1 - 2.182 - 0.119*1

# PREDICT ESCOLA ("não")
40.03 + 5.17*1 + 14.7*(0) + 1.18*2 - 0.65*(0)*(1) - 0.056*2*1 - 2.182 - 0.119*1

# PREDICT ESCOLA ("sim")
40.03 + 5.17*1 + 14.7*(1) + 1.18*2 - 0.65*(1)*(1) - 0.056*2*1 - 2.182 - 0.119*1

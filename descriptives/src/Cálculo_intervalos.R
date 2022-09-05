




#### Encuesta ####
options(survey.lonely.psu="adjust")
encuesta <- as_survey_design(data, ids=upm_dis, strata=est_dis, 
                             weights = fac_muj, nest=T)


graph <- data.frame()

tempo <- encuesta %>%
  group_by(year, viofisinf) %>%
  summarize(per = survey_mean(na.rm=T, vartype = c("ci"), 
                              level = 0.95, df = Inf))%>%
  filter(viofisinf=="Sí")%>%
  select(-viofisinf)%>%
  mutate(var="viofisinf")

graph <- bind_rows(graph, tempo)

tempo <- encuesta %>%
  group_by(year, viopsiinf) %>%
  summarize(per = survey_mean(na.rm=T, vartype = c("ci"), 
                              level = 0.95, df = Inf))%>%
  filter(viopsiinf=="Sí")%>%
  select(-viopsiinf)%>%
  mutate(var="viopsiinf")

graph <- bind_rows(graph, tempo)

tempo <- encuesta %>%
  group_by(year, viosexinf) %>%
  summarize(per = survey_mean(na.rm=T, vartype = c("ci"), 
                              level = 0.95, df = Inf))%>%
  filter(viosexinf=="Sí")%>%
  select(-viosexinf)%>%
  mutate(var="viosexinf")

graph <- bind_rows(graph, tempo)





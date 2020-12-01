############################################################
### Make rosetta-table (Cohens D->raw diff) for article  ###
############################################################
## Pontus P. Sigray, Dec 2020 

source('./R/Tables/src/convert_effsize_rawdata.R')
source('./R/Tables/src/fetch_mean.R')
library(tidyverse)
library(magrittr)

#Make rosetta for two-sample designs
Cross.sec.rosetta <- dplyr::tibble(D_or_Dz = 'D', 
                                   Effsize = c(0.2, 0.5, 0.8, 1.2, 1.5)) %>%
  mutate(N_at_80pwr = purrr::map(.x = Effsize, ~pwr::pwr.t.test(d = .x,sig.level = 0.05,
                                                         power = .8,
                                                         type = 'two.sample', 
                                                         alternative="two.sided"))) %>%
  mutate(N_at_80pwr = round(purrr::map_dbl(N_at_80pwr, c('n'))) ) %>% 
  dplyr::mutate(Raclopride = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                     type_in = 'D',
                                                                     type_out = 'raw',
                                                                     radioligand = 'Raclopride_HRRT')),
         AZ9369 = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                 type_in = 'D',
                                                                 type_out = 'raw',
                                                                 radioligand = 'AZ9369_HRRT')),
         PBR28 = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                 type_in = 'D',
                                                                 type_out = 'raw',
                                                                 radioligand = 'PBR28_HRRT')),
         UCBJ = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                 type_in = 'D',
                                                                 type_out = 'raw',
                                                                 radioligand = 'UCBJ_HRRT')) ) %>%
  dplyr::mutate(Raclopride_perc = (Raclopride/fetch_mean('Raclopride_HRRT')) * 100,
         AZ9369_perc = (Raclopride/fetch_mean('AZ9369_HRRT')) * 100,
         PBR28_perc = (Raclopride/fetch_mean('PBR28_HRRT')) * 100,
         UCBJ_perc = (Raclopride/fetch_mean('UCBJ_HRRT')) * 100) %>%
  dplyr::mutate(Raclopride =  glue::glue(' {round(Raclopride,2)} ({round(Raclopride_perc)}%) '),
         AZ9369 =  glue::glue(' {round(AZ9369,2)} ({round(AZ9369_perc)}%) '), 
         PBR28 =  glue::glue(' {round(PBR28,2)} ({round(PBR28_perc)}%) '), 
         UCBJ =  glue::glue(' {round(UCBJ,2)} ({round(UCBJ_perc)}%) ') ) %>%
  dplyr::select(D_or_Dz, Effsize, Raclopride, AZ9369, PBR28, UCBJ, N_at_80pwr)

#Make rosetta for paired designs
Long.rosetta <- dplyr::tibble(D_or_Dz = 'Dz', 
                              Effsize = c(0.2, 0.5, 0.8, 1.2, 1.5) ) %>%
  mutate(N_at_80pwr = purrr::map(.x = Effsize, ~pwr::pwr.t.test(d = .x,sig.level = 0.05,
                                                         power = .8,
                                                         type = 'paired', 
                                                         alternative="two.sided"))) %>%
  mutate(N_at_80pwr = round(purrr::map_dbl(N_at_80pwr, c('n'))) ) %>% 
  dplyr::mutate(Raclopride = unlist(purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                                           type_in = 'Dz',
                                                                                           type_out = 'raw',
                                                                                           radioligand = 'Raclopride_HRRT'))),
                AZ9369 = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                                       type_in = 'Dz',
                                                                                       type_out = 'raw',
                                                                                       radioligand = 'AZ9369_HRRT')),
                PBR28 = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                                      type_in = 'Dz',
                                                                                      type_out = 'raw',
                                                                                      radioligand = 'PBR28_HRRT')),
                UCBJ = purrr::map_dbl(.x = Effsize, .f = ~convert_effsize_rawdata(measures = .x,
                                                                                     type_in = 'Dz',
                                                                                     type_out = 'raw',
                                                                                     radioligand = 'UCBJ_HRRT')) ) %>%
  dplyr::mutate(Raclopride_perc = (Raclopride/fetch_mean('Raclopride_HRRT')) * 100,
                AZ9369_perc = (Raclopride/fetch_mean('AZ9369_HRRT')) * 100,
                PBR28_perc = (Raclopride/fetch_mean('PBR28_HRRT')) * 100,
                UCBJ_perc = (Raclopride/fetch_mean('UCBJ_HRRT')) * 100) %>%
  dplyr::mutate(Raclopride =  glue::glue(' {round(Raclopride,2)} ({round(Raclopride_perc)}%) '),
                AZ9369 =  glue::glue(' {round(AZ9369,2)} ({round(AZ9369_perc)}%) '), 
                PBR28 =  glue::glue(' {round(PBR28,2)} ({round(PBR28_perc)}%) '), 
                UCBJ =  glue::glue(' {round(UCBJ,2)} ({round(UCBJ_perc)}%) ') ) %>%
  dplyr::select(D_or_Dz, Effsize, Raclopride, AZ9369, PBR28, UCBJ, N_at_80pwr)

rosetta <- dplyr::bind_rows(Cross.sec.rosetta,Long.rosetta)

xlsx::write.xlsx(x = as.data.frame(rosetta),file = './Results/Tables/Rosetta_table_R.xlsx',row.names = F)







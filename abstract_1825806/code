# Packages and libraries =======================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, foreign, geobr, readxl, read.dbc, ComplexHeatmap,
               viridis, patchwork, ggfortify)

# Dictionaries =================================================================
# AIH procedures in decreasing order of specificity
dicio_aih_proc <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/TB_SIGTAW.dbf")[,1:2]
dicio_aih_forma <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/TB_FORMA.dbf")[,1:2]
dicio_aih_subgr <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/TB_SUBGR.dbf")[,1:2]
dicio_aih_grupo <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/TB_GRUPO.dbf")[,1:2]

# CNES establishments (code and name)
dicio_cnes <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/CADGERRJ.dbf")[, c("CNES", "FANTASIA")]

# DataSUS ICD-10
dicio_cid <- read.dbf("Documentação SIA/TAB_SIA/DBF - contém txt com legenda/S_CID.DBF")[,c(1,5)]

# Brazilian states and cities
# Curiosidade (ou utilidade): os municípios gaúchos de Lagoa Mirim e Lagoa dos
# Patos não constam na função read_municipal_seat, apenas na função
# read_municipality, o que gera duas observações a menos quando lidas. No
# momento deste comentário (2023-09-06), isso não tem qualquer impacto.
munic_br <- read_municipality()
munic_br$code_muni <- as.character(munic_br$code_muni)
munic_br <- munic_br |> select(code_muni,geom)
munic_br_coords <- read_municipal_seat()
munic_br_coords$code_muni <- as.character(munic_br_coords$code_muni)
munic_br_coords <- munic_br_coords |>
  separate_wider_delim(
    cols = geom, 
    names = c("longitude", "latitude"),
    delim = ", "
  )|>
  mutate(
    longitude = as.double(str_sub(longitude, 3, -1)),
    latitude = as.double(str_sub(latitude, 1, -2))
  )
munic_br <- left_join(munic_br,munic_br_coords,by = "code_muni")
rm(munic_br_coords)
munic_br <- munic_br |> select(-year)
munic_rj <- munic_br |>
  filter(code_state == "33")
dic_muni <- read_xls("dicionario_municipios.xls")

# Full dataframes ==============================================================
list2013 <- as.data.frame(list.files(path = "RDRJ", full.names = TRUE))[1:12,]
rdrj2013 <- list2013|>
  map(read.dbc::read.dbc)|>
  list_rbind()|>
  mutate(
    DIAGSEC1 = rep(NA),
    DIAGSEC2 = rep(NA),
    DIAGSEC3 = rep(NA),
    DIAGSEC4 = rep(NA),
    DIAGSEC5 = rep(NA),
    DIAGSEC6 = rep(NA),
    DIAGSEC7 = rep(NA),
    DIAGSEC8 = rep(NA),
    DIAGSEC9 = rep(NA),
    TPDISEC1 = rep(NA),
    TPDISEC2 = rep(NA),
    TPDISEC3 = rep(NA),
    TPDISEC4 = rep(NA),
    TPDISEC5 = rep(NA),
    TPDISEC6 = rep(NA),
    TPDISEC7 = rep(NA),
    TPDISEC8 = rep(NA),
    TPDISEC9 = rep(NA)
  )
list2014to16 <- as.data.frame(list.files(path = "RDRJ",
                                         full.names = TRUE))[13:48,]
rdrj2014to16 <- list2014to16|>
  map(read.dbc::read.dbc)|>
  list_rbind()
list2017to19 <- as.data.frame(list.files(path = "RDRJ",
                                         full.names = TRUE))[49:84,]
rdrj2017to19 <- list2017to19|>
  map(read.dbc::read.dbc)|>
  list_rbind()
list2020to22 <- as.data.frame(list.files(path = "RDRJ",
                                         full.names = TRUE))[85:120,]
rdrj2020to22 <- list2020to22|>
  map(read.dbc::read.dbc)|>
  list_rbind()
list2023 <- as.data.frame(list.files(path = "RDRJ",
                                     full.names = TRUE))[121:127,]
rdrj2023 <- list2023|>
  map(read.dbc::read.dbc)|>
  list_rbind()
rdrjtot <- rbind(rdrj2013,rdrj2014to16,rdrj2017to19,rdrj2020to22,rdrj2023)
rm(rdrj2013,rdrj2014to16,rdrj2017to19,rdrj2020to22,rdrj2023)
rm(list2013,list2014to16,list2017to19,list2020to22,list2023)
colnames(rdrjtot) <- str_to_lower(colnames(rdrjtot))

# None of these variables actually contain data
rdrjtot <- rdrjtot|>
  select(-cgc_hosp,-uti_mes_in, -uti_mes_an, -uti_mes_al, -uti_int_in,
         -uti_int_an, -uti_int_al, -val_sadt, -val_rn, -val_acomp, -val_ortp,
         -val_sangue, -val_sadtsr, -val_transp, -val_obsang, -val_ped1ac,
         -rubrica, -num_proc, -tot_pt_sp, -cpf_aut, -gestor_dt, -infehosp,
         -diagsec8, -diagsec9, -tpdisec8, -tpdisec9)

# Within this mutate,
rdrjtot <- rdrjtot|>
  mutate(
    # some manipulation of dates here,
    nasc = as.Date(nasc, "%Y%m%d"),
    dt_inter = as.Date(dt_inter, "%Y%m%d"),
    dt_saida = as.Date(dt_saida, "%Y%m%d"),
    wk_inter = lubridate::week(dt_inter),
    mth_inter = lubridate::month(dt_inter),
    yr_inter = format(dt_inter, "%Y"),
    wkyr_inter = paste(format(dt_inter, "%Y"),
                       str_pad(format(dt_inter, "%U"),
                               width = 2, pad = "0"),
                       sep = "-"),
    mthyr_inter = format(dt_inter, "%Y-%m"))
rdrjtot <- rdrjtot|>
  mutate(
    wk_saida = lubridate::week(dt_saida),
    mth_saida = lubridate::month(dt_saida),
    yr_saida = lubridate::year(dt_saida),
    wkyr_saida = paste(format(dt_saida, "%Y"),
                       str_pad(format(dt_saida, "%U"),
                               width = 2, pad = "0"),
                       sep = "-"),
    mthyr_saida = format(dt_saida, "%Y-%m"),
    idade_dias = interval(nasc, dt_inter) %/% days(1),
    idade_anos = interval(nasc, dt_inter) %/% years(1))
# and obtaining information from larger strings here
rdrjtot <- rdrjtot|>
  mutate(
    proc_sol_grupo = factor(substr(proc_solic, start = 1, stop = 2)),
    proc_sol_subgrupo = factor(substr(proc_solic, start = 1, stop = 4)),
    proc_sol_forma = factor(substr(proc_solic, start = 1, stop = 6)),
    proc_rea_grupo = factor(substr(proc_rea, start = 1, stop = 2)),
    proc_rea_subgrupo = factor(substr(proc_rea, start = 1, stop = 4)),
    proc_rea_forma = factor(substr(proc_rea, start = 1, stop = 6)) 
  )

rdrjtot <- left_join(rdrjtot, dicio_cnes,
                     by = c("cnes" = "CNES"))
rdrjtot <- left_join(rdrjtot, dicio_cid,
                     by = c("diag_princ" = "CD_COD"))
rdrjtot <- left_join(rdrjtot, dicio_aih_proc,
                     by = c("proc_solic" = "IP_COD"))
rdrjtot <- left_join(rdrjtot, dicio_aih_forma,
                     by = c("proc_sol_forma" = "CO_FORMA"))
rdrjtot <- left_join(rdrjtot, dicio_aih_subgr,
                     by = c("proc_sol_subgrupo" = "CO_SUB_GRU"))
rdrjtot <- left_join(rdrjtot, dicio_aih_grupo,
                     by = c("proc_sol_grupo" = "CO_GRUPO"))
rdrjtot <- left_join(rdrjtot, dicio_aih_proc,
                     by = c("proc_rea" = "IP_COD"))
rdrjtot <- left_join(rdrjtot, dicio_aih_forma,
                     by = c("proc_rea_forma" = "CO_FORMA"))
rdrjtot <- left_join(rdrjtot, dicio_aih_subgr,
                     by = c("proc_rea_subgrupo" = "CO_SUB_GRU"))
rdrjtot <- left_join(rdrjtot, dicio_aih_grupo,
                     by = c("proc_rea_grupo" = "CO_GRUPO"))
colnames(rdrjtot) <- str_to_lower(colnames(rdrjtot))

rdrjtot <- rdrjtot|>
  select(n_aih, ident, car_int, espec, complex, nasc, nacional, sexo, cod_idade,
         idade, idade_anos, idade_dias, raca_cor, etnia, cbor, vincprev,
         contracep1, contracep2, insc_pn, gestrisco, num_filhos, instru,
         homonimo, cep, munic_res, uf_zi, munic_mov, cnes, fantasia, diag_princ,
         diag_secun, cd_descr, dt_inter, wk_inter, mth_inter, yr_inter,
         wkyr_inter, mthyr_inter, proc_solic, ip_dscr.x, proc_sol_forma,
         no_forma.x, proc_sol_subgrupo, no_sub_gru.x, proc_sol_grupo,
         no_grupo.x, proc_rea, ip_dscr.y, proc_rea_forma, no_forma.y,
         proc_rea_subgrupo, no_sub_gru.y, proc_rea_grupo, no_grupo.y, ind_vdrl,
         cid_notif, cnaer, val_tot, us_tot, qt_diarias, dias_perm, cobranca,
         val_uci, marca_uci, val_uti, marca_uti, uti_mes_to, uti_int_to, val_sh,
         val_sp, val_sh_ges, val_sp_ges, val_sh_fed, val_sp_fed, diar_acom,
         diagsec1, diagsec2, diagsec3, diagsec4, diagsec5, diagsec6, diagsec7,
         tpdisec1, tpdisec2, tpdisec3, tpdisec4, tpdisec5, tpdisec6, tpdisec7,
         dt_saida, wk_saida, mth_saida, yr_saida, wkyr_saida, mthyr_saida,
         morte, cid_asso, cid_morte, gestor_cod, gestor_tp, gestor_cpf,
         cnpj_mant, ano_cmpt, mes_cmpt, seq_aih5, sequencia, remessa, nat_jur,
         aud_just, sis_just, gestao, natureza, financ, faec_tp, regct)

# Database filtered for analyses ===============================================
prod_res <- rdrjtot|>
  filter(cnes %in% c("0012505", "2269783", "2269988", "2280167",
                     "2288338", "2296616", "2708353", "2798662") &
           dt_inter >= as.Date("2013-01-01") &
           dt_inter <= as.Date("2022-12-31"))|>
  mutate(cnes = recode(cnes,
                       "0012505" = "HUAP",
                       "2269783" = "HUPE",
                       "2269988" = "HFSE",
                       "2280167" = "HUCFF",
                       "2288338" = "INI",
                       "2296616" = "IPPMG",
                       "2708353" = "IFF",
                       "2798662" = "HGNI"))

# Unsupervised clustering heatmap analyses =====================================
# These heatmaps are used to construct the respective y-scale limits of their figures
heatmap1 <- prod_res|>
  mutate(chapter = case_when(#Source: https://icd.who.int/browse10/2019/en
    str_detect(diag_princ, "^(A|B)")~ "I",
    str_detect(diag_princ, "^(C|D[0-4])")~ "II",
    str_detect(diag_princ, "^D[5-8]")~ "III",
    str_detect(diag_princ, "^E")~ "IV",
    str_detect(diag_princ, "^F")~ "V",
    str_detect(diag_princ, "^G")~ "VI",
    str_detect(diag_princ, "^H[0-5]")~ "VII",
    str_detect(diag_princ, "^H[6-9]")~ "VIII",
    str_detect(diag_princ, "^I")~ "IX",
    str_detect(diag_princ, "^J")~ "X",
    str_detect(diag_princ, "^K")~ "XI",
    str_detect(diag_princ, "^L")~ "XII",
    str_detect(diag_princ, "^M")~ "XIII",
    str_detect(diag_princ, "^N")~ "XIV",
    str_detect(diag_princ, "^O")~ "XV",
    str_detect(diag_princ, "^P")~ "XVI",
    str_detect(diag_princ, "^Q")~ "XVII",
    str_detect(diag_princ, "^R")~ "XVIII",
    str_detect(diag_princ, "^(S|T)")~ "XIX",
    str_detect(diag_princ, "^(V|W|X|Y)")~ "XX",
    str_detect(diag_princ, "^Z")~ "XXI",
    str_detect(diag_princ, "^U")~ "XXII",
    TRUE ~ NA
  ))|>
  mutate(chapter = factor(chapter, levels = c("I", "II", "III", "IV", "V", "VI",
                                              "VII", "VIII", "IX", "X", "XI",
                                              "XII", "XIII", "XIV", "XV",
                                              "XVI", "XVII", "XVIII", "XIX",
                                              "XX", "XXI", "XXII"),
                          ordered = TRUE))|>
  group_by(cnes, chapter)|>
  summarise(n = n())|>
  pivot_wider(names_from = cnes,
              values_from = n)|>
  mutate(across(-1, \(x) (x/sum(x, na.rm = T)*10^2)))
names_hm1 <- as.character(heatmap1[[1]])
heatmap1 <- heatmap1[,2:9]
rownames(heatmap1) <- names_hm1
heatmap1 <- heatmap1|>
  mutate_at(.vars =1:8,
            ~coalesce(.,0))
theatmap1 <- t(heatmap1)
colnames(theatmap1) <- names_hm1
Heatmap(theatmap1)

heatmap2 <- prod_res|>
  filter(proc_sol_forma == "030301" | proc_sol_forma == "030318")|>
  mutate(chapter = case_when(
    str_detect(diag_princ, "^(A|B)")~ "I",
    str_detect(diag_princ, "^(C|D[0-4])")~ "II",
    str_detect(diag_princ, "^D[5-8]")~ "III",
    str_detect(diag_princ, "^E")~ "IV",
    str_detect(diag_princ, "^F")~ "V",
    str_detect(diag_princ, "^G")~ "VI",
    str_detect(diag_princ, "^H[0-5]")~ "VII",
    str_detect(diag_princ, "^H[6-9]")~ "VIII",
    str_detect(diag_princ, "^I")~ "IX",
    str_detect(diag_princ, "^J")~ "X",
    str_detect(diag_princ, "^K")~ "XI",
    str_detect(diag_princ, "^L")~ "XII",
    str_detect(diag_princ, "^M")~ "XIII",
    str_detect(diag_princ, "^N")~ "XIV",
    str_detect(diag_princ, "^O")~ "XV",
    str_detect(diag_princ, "^P")~ "XVI",
    str_detect(diag_princ, "^Q")~ "XVII",
    str_detect(diag_princ, "^R")~ "XVIII",
    str_detect(diag_princ, "^(S|T)")~ "XIX",
    str_detect(diag_princ, "^(V|W|X|Y)")~ "XX",
    str_detect(diag_princ, "^Z")~ "XXI",
    str_detect(diag_princ, "^U")~ "XXII",
    TRUE ~ NA
  ))|>
  mutate(chapter = factor(chapter, levels = c("I", "II", "III", "IV", "V", "VI",
                                              "VII", "VIII", "IX", "X", "XI",
                                              "XII", "XIII", "XIV", "XV",
                                              "XVI", "XVII", "XVIII", "XIX",
                                              "XX", "XXI", "XXII"),
                          ordered = TRUE))|>
  group_by(chapter,cnes)|>
  summarise(n = n())|>
  pivot_wider(names_from = cnes,
              values_from = n)|>
  ungroup()|>
  mutate(across(-1, \(x) (x/sum(x, na.rm = T)*10^2)))
names_hm2 <- as.character(heatmap2[[1]])
heatmap2 <- heatmap2[,2:9]
rownames(heatmap2) <- names_hm2
heatmap2 <- heatmap2|>
  mutate_at(.vars =1:8,
            ~coalesce(.,0))
theatmap2 <- t(heatmap2)
colnames(theatmap2) <- names_hm2
theatmap2 <- cbind(theatmap2, XX = rep(0), XXII = rep(0))
theatmap2 <- theatmap2[,c(1:19,21,20,22)]
Heatmap(theatmap2)

heatmap3 <- prod_res|>
  filter(proc_sol_forma == "030301" | proc_sol_forma == "030318")|>
  mutate(chapter = case_when(
    str_detect(diag_princ, "^(A|B)")~ "I",
    str_detect(diag_princ, "^G")~ "VI",
    TRUE ~ NA
  ))|>
  filter(chapter == "I" | chapter == "VI")|>
  mutate(subchapter = case_when(
    str_detect(diag_princ, "^A0") ~ "A00-A09",
    str_detect(diag_princ, "^A1") ~ "A15-A19",
    str_detect(diag_princ, "^A2")~ "A20-A28",
    str_detect(diag_princ, "^A[3-4]")~ "A30-A49",
    str_detect(diag_princ, "^A(5|6[0-4])")~ "A50-A64",
    str_detect(diag_princ, "^A6[5-9]")~ "A65-A69",
    str_detect(diag_princ, "^A7[0-4]")~ "A70-A74",
    str_detect(diag_princ, "^A7[5-9]")~ "A75-A79",
    str_detect(diag_princ, "^A8")~ "A80-A89",
    str_detect(diag_princ, "^A9")~ "A92-A99",
    str_detect(diag_princ, "^B0")~ "B00-B09",
    str_detect(diag_princ, "^B1")~ "B15-B19",
    str_detect(diag_princ, "^B2[0-4]")~ "B20-B24",
    str_detect(diag_princ, "^B(2[5-9]|3[0-4])")~ "B25-B34",
    str_detect(diag_princ, "^B(3[5-9]|4)")~ "B35-B49",
    str_detect(diag_princ, "^B(5|6[0-4])")~ "B50-B64",
    str_detect(diag_princ, "^B(6[5-9]|7|8[0-3])")~ "B65-B83",
    str_detect(diag_princ, "^B8[5-9]")~ "B85-B89",
    str_detect(diag_princ, "^B9[0-4]")~ "B90-B94",
    str_detect(diag_princ, "^B9[5-8]")~ "B95-B98",
    str_detect(diag_princ, "^B99")~ "B99",
    str_detect(diag_princ, "^G0")~ "G00-G09",
    str_detect(diag_princ, "^G1")~ "G20-G26",
    str_detect(diag_princ, "^G3[0-2]")~ "G30-G32",
    str_detect(diag_princ, "^G3[5-7]")~ "G35-G37",
    str_detect(diag_princ, "^G4[0-7]")~ "G40-G47",
    str_detect(diag_princ, "^G5")~ "G50-G59",
    str_detect(diag_princ, "^G6[0-4]")~ "G60-G64",
    str_detect(diag_princ, "^G7[0-3]")~ "G70-G73",
    str_detect(diag_princ, "^G8[0-3]")~ "G80-G83",
    str_detect(diag_princ, "^G9[0-9]")~ "G90-G99",
    TRUE ~ "NA"
  ))|>
  group_by(cnes,subchapter)|>
  summarise(n = n())|>
  pivot_wider(names_from = cnes,
              values_from = n)|>
  ungroup()|>
  mutate(across(-1, \(x) (x/sum(x, na.rm = T)*10^2)))
names_hm3 <- as.character(heatmap3[[1]])
heatmap3 <- heatmap3[,2:9]
rownames(heatmap3) <- names_hm3
heatmap3 <- heatmap3|>
  mutate_at(.vars =1:8,
            ~coalesce(.,0))
theatmap3 <- t(heatmap3)
colnames(theatmap3) <- names_hm3
Heatmap(theatmap3)

# Figure 1 =====================================================================
fig1a <- prod_res|>
  group_by(cnes)|>
  summarise(n=n(),
            percent = round(n/nrow(prod_res)*100,digits = 1))|>
  ggplot()+
  geom_col(aes(cnes, n,
               fill = cnes,
               color = cnes))+
  geom_text(aes(cnes, n, 
                label = paste0(n, ";\n", percent,"%")),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.13) + 
  labs(title="Total hospitalizations",
       x = "Hospital", y = NULL)+
  scale_y_continuous(expand = c(0,0),
                     label = scales::label_number())+
  scale_color_brewer(palette ="Dark2", guide = "none")+
  scale_fill_brewer(palette = "Set2", guide = "none")+
  coord_cartesian(ylim = c(0,300000))+
  theme_classic()

fig1b <- prod_res|>
  group_by(cnes,yr_inter)|>
  summarise(n=n())|>
  ggplot()+
  geom_col(aes(yr_inter,
               n,
               fill = cnes, color = cnes),
           position = "fill")+
  labs(x = "Year",
       y = "Percentage")+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0))+
  scale_color_brewer(palette ="Dark2", guide = "none")+
  scale_fill_brewer(palette = "Set2", guide = "none")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

fig1c <- prod_res|>
  filter(cod_idade == "4")|>
  group_by(cnes)|>
  ggplot()+
  geom_violin(aes(x = cnes, y = idade,
                  color = cnes,
                  fill = cnes),
              linewidth = 1,
              alpha = 0.3,
              trim = T,
              position = "dodge",
              scale = "width")+
  geom_boxplot(aes(cnes, idade,
                   color = cnes,
                   fill = cnes),
               varwidth = TRUE,
               alpha = 0.3,
               outlier.shape = NA,
               coef = 1)+
  coord_cartesian(ylim = c(0,100))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette = "Set2", guide = "none")+
  scale_color_brewer(palette ="Dark2", guide = "none")+
  labs(title = "Distribution of patients' ages",
       subtitle = 'IPPMG stands out as an eminently pediatric hospital',
       caption = "Violins standardized to same maximum width;\nboxplot width proportional to hospitalization count",
       x = "Hospital",
       y = "Age")+
  theme_classic()

fig1d <- prod_res|>
  filter(cod_idade == "4")|>
  mutate(sexo = case_when(
    sexo == '1' ~ 'Male',
    sexo == '3' ~ 'Female'
  ))|>
  #filter(cnes != "IPPMG")|>
  ggplot()+
  geom_freqpoly(aes(idade,color = sexo),
                position="dodge",
                binwidth = 1,
                linewidth = 1)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,99))+
  ggbreak::scale_y_break(c(1500,2000),
                scales = 0.2,
                ticklabels = c(5000),
                expand = FALSE)+
  scale_y_continuous(expand = c(0,0), limits = c(0,5050))+
  scale_color_manual(values = c("#F5A9B8","#5BCEFA"), guide = 'none')+
  facet_wrap(vars(cnes),
             nrow = 1)+
  labs(title = "Proportion of sexes for age and hospital",
       subtitle = "HUAP, HUPE and HFSE show similar patterns",
       x = "Age",
       y = "Count",
       fill = "Sex")+
  theme_classic()+
  theme(strip.background = element_blank())

fig1e <- prod_res|>
  filter(cod_idade == "4")|>
  mutate(sexo = case_when(
    sexo == '1' ~ 'Male',
    sexo == '3' ~ 'Female'
  ))|>
  #filter(cnes != "IPPMG")|>
  ggplot()+
  geom_vline(xintercept=23)+
  geom_vline(xintercept=75)+
  geom_density(aes(idade,fill=sexo,color=sexo),
               position="fill")+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,99))+
  scale_y_continuous(expand = c(0,0),
                     labels = scales::percent)+
  scale_fill_manual(values = c("#F5A9B8","#5BCEFA"))+
  scale_color_manual(values = c("#F5A9B8","#5BCEFA"))+
  facet_wrap(vars(cnes),nrow=1)+
  labs(x = NULL,
       y = "Percentage",
       fill = "Sex",
       color = 'Sex')+
  theme_classic()+
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        strip.background = element_blank())

# Figure 2 =====================================================================
fig2a <- prod_res |>
  mutate(proc_sol_grupo = recode(proc_sol_grupo,
                                 "02" = "Diagnoses",
                                 "03" = "Clinical\nprocedures",
                                 "04" = "Surgical\nprocedures",
                                 "05" = "Cell, tissue and/or\norgan transplants"))|>
  group_by(cnes,proc_sol_grupo)|>
  summarise(n=n())|>
  group_by(cnes)|>
  mutate(prop = substr(as.character(n/sum(n)*100), start=1,stop=4))|>
  mutate(label = str_c(n, "; ", prop, "%"))|>
  ggplot()+
  geom_col(aes(cnes,n, fill = proc_sol_grupo),
           position = 'fill')+
  scale_y_continuous(expand = c(0,0),
                     labels = scales::percent)+
  labs(title = "Proportion of procedure groups",
       subtitle = "HUCFF performs the most diagnostic and\ntransplant procedures, proportionally",
       x = NULL,
       y = NULL,
       fill = NULL)+
  theme_classic()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.background = element_blank())

fig2b <- prod_res |>
  mutate(espec = recode(espec,
                        "01" = "Surgery",
                        "02" = "Obstetrics",
                        "03" = "Internal medicine",
                        "04" = "Prolonged care",
                        "05" = "Psychiatry",
                        "06" = "Tisiology",
                        "07" = "Pediatrics",
                        "08" = "Rehabilitation",
                        "09" = "Surgical day clinics",
                        "10" = "AIDS day\nclinics"))|>
  group_by(cnes,espec)|>
  summarise(n=n())|>
  group_by(espec)|>
  mutate(prop = substr(as.character(n/sum(n)*100), start=1,stop=4))|>
  mutate(label = str_c(n, "; ", prop, "%"))|>
  ggplot()+
  geom_col(aes(cnes,n, fill = espec),
           position = 'fill')+
  scale_y_continuous(expand = c(0,0),
    labels = scales::percent)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                               "#000000", "#0072B2", "#D55E00", "#CC79A7",
                               "#999999", "#434961"))+
  labs(title = "Proportion of hospitalizations by specialty",
       subtitle = "INI admits patients through AIDS day clinics",
       x = NULL,
       y = NULL,
       fill = NULL)+
  theme_classic()+
  #coord_flip()+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.background = element_blank())+
  guides(fill = guide_legend(byrow = T))

fig2c <- prod_res|>
  group_by(cnes)|>
  ggplot()+
  geom_violin(aes(cnes, qt_diarias,
                  color = cnes,
                  fill = cnes),
              linewidth=1,
              alpha=0.3,
              trim = T,
              position="dodge")+
  geom_boxplot(aes(cnes, qt_diarias,
                   color = cnes,
                   fill = cnes),
               varwidth = TRUE,
               alpha = 0.3,
               outlier.shape = NA,
               coef = 1)+
  geom_hline(yintercept = 7,
             linetype = 2,
             color = '#e72a2a')+
  annotate(
    geom = 'text',
    label = '1 week',
    fontface = 'bold',
    x = "HUPE",
    y = 9,
    color = '#e72a2a'
  )+
  scale_fill_brewer(palette = "Set2",
                    guide="none",
                    limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                               "INI", "IPPMG", "IFF", "HGNI"))+
  scale_color_brewer(palette = "Dark2",
                     guide="none",
                     limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                                "INI", "IPPMG", "IFF", "HGNI"))+
  coord_cartesian(ylim=c(0,22))+
  labs(title = "Length of hospital stay",
       subtitle = "Most patients stay for less than a week",
       x = NULL,
       y = "Days of hospitalization")+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

fig2d <- prod_res|>
  filter(val_uti > 0)|>
  group_by(cnes)|>
  mutate(
    cambio = val_tot/us_tot,
    us_uti = val_uti/cambio
  )|>
  ggplot()+
  geom_violin(aes(cnes, us_uti,
                  color = cnes,
                  fill = cnes),
              linewidth=1,
              alpha=0.3,
              trim = T,
              position="dodge")+
  geom_boxplot(aes(cnes, us_uti,
                   color = cnes,
                   fill = cnes),
               varwidth = TRUE,
               alpha = 0.3,
               outlier.shape = NA,
               coef = 1)+
  geom_hline(yintercept=1000,
             linetype = 2,
             color = '#e72a2a')+
  scale_fill_brewer(palette = "Set2",
                    guide="none",
                    limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                               "INI", "IPPMG", "IFF", "HGNI"))+
  scale_color_brewer(palette = "Dark2",
                     guide="none",
                     limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                                "INI", "IPPMG", "IFF", "HGNI"))+
  scale_y_continuous(trans = 'log10', labels = scales::dollar_format())+
  labs(title = "Intensive care unit costs (US dollars)",
       subtitle = "INI stands out for more expensive\nICU costs, on average",
       x = NULL,
       y = "Cost (log scale)",
       fill = "Hospital",
       color = "Hospital")+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

fig2e <- prod_res|>
  filter(val_uti > 0)|>
  filter(diag_princ != "B342")|>
  group_by(cnes)|>
  mutate(
    cambio = val_tot/us_tot,
    us_uti = val_uti/cambio
  )|>
  ggplot()+
  geom_violin(aes(cnes, us_uti,
                  color = cnes,
                  fill = cnes),
              linewidth=1,
              alpha=0.3,
              trim = T,
              width = 0.7,
              position="dodge",
              key_glyph = draw_key_boxplot)+
  geom_boxplot(aes(cnes, us_uti,
                   color = cnes,
                   fill = cnes),
               varwidth = TRUE,
               alpha = 0.3,
               outlier.shape = NA,
               coef = 1,
               key_glyph = draw_key_boxplot)+
  scale_fill_brewer(palette = "Set2",
                    guide = 'none',
                    limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                               "INI", "IPPMG", "IFF", "HGNI"))+
  scale_color_brewer(palette = "Dark2",
                     guide = 'none',
                     limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                                "INI", "IPPMG", "IFF", "HGNI"))+
  scale_x_discrete(limits = c("HUAP", "HUPE", "HFSE", "HUCFF",
                              "INI", "IPPMG", "IFF", "HGNI"))+
  scale_y_continuous(trans = 'log10', labels = scales::dollar_format())+
  labs(title = "Intensive care unit costs, non-COVID patients",
       subtitle = "However, all of INI's expenditure on ICU beds\nis due to patients under the B34.2 ICD-10",
       x = NULL,
       y = "Cost (log scale)",
       fill = "Hospital",
       color = "Hospital")+
  theme_classic()+
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

# Figure 3 =====================================================================
fig3a <- autoplot(theatmap1)+
  scale_x_discrete(expand = c(0,0),
                   limits = c("I", "II", "III", "IV", "V", "VI",
                              "VII", "VIII", "IX", "X", "XI",
                              "XII", "XIII", "XIV", "XV",
                              "XVI", "XVII", "XVIII", "XIX",
                              "XX", "XXI", "XXII"))+
  scale_y_discrete(expand = c(0,0),
                   limits = c("HGNI", "IFF", "HFSE", "HUAP",
                              "HUPE", "HUCFF", "IPPMG", "INI"))+
  scale_fill_viridis(option = 'turbo', breaks = seq(from = 5, to = 100, by = 10))+
  labs(
    title = "All hospitalizations",
    subtitle = "INI stands out for its high proportion of chapter I\nhospitalizations; IFF and HGNI cluster together due to\nchapter XV",
    caption = "I: certain infectious and parasitic diseases;\nXV: pregnancy, childbirth and the puerperium",
    x = NULL,
    y = NULL,
    fill = NULL
  )+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.direction = 'vertical',
    legend.position = 'right',
    legend.key.height = unit(2.5, 'cm')
  )

fig3b <- autoplot(theatmap2)+
  scale_x_discrete(expand = c(0,0),
                   limits = c("I", "II", "III", "IV", "V", "VI",
                              "VII", "VIII", "IX", "X", "XI",
                              "XII", "XIII", "XIV", "XV",
                              "XVI", "XVII", "XVIII", "XIX",
                              "XX", "XXI", "XXII"))+
  scale_y_discrete(expand = c(0,0),
                   limits = c("HUAP", "IPPMG", "HGNI", "HUCFF", 
                              "INI", "HUPE", "IFF", "HFSE"))+
  scale_fill_viridis(option = 'turbo', breaks = seq(from = 5, to = 100, by = 10))+
  labs(
    title = "Hospitalizations for clinical treatment\nof infectious diseases or HIV",
    subtitle = "IFF and HFSE cluster separately due to chapter VI",
    caption = "VI: diseases of the nervous system",
    x = NULL,
    y = NULL,
    fill = NULL
  )+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.direction = 'vertical',
    legend.position = 'right',
    legend.key.height = unit(2.5, 'cm')
  )

fig3c <- autoplot(theatmap3)+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0),
                   limits = c("IFF", "IPPMG", "HUAP", "HUPE",
                              "HUCFF", "HFSE", "HGNI", "INI"))+
  scale_fill_viridis(option = 'turbo', breaks = seq(from = 5, to = 100, by = 10))+
  labs(
    title = "Hospitalizations for chapters I and VI",
    subtitle = "Pediatric hospitals cluster due to A00-A09; IFF and HFSE\nsee more G00-G09, proportionally; INI stands out\nas a center for HIV care",
    caption = "A00-A09: intestinal infectious diseases; B20-B24: human immunodeficiency\nvirus [HIV] disease; G00-G09: inflammatory diseases of the\ncentral nervous system (includes bacterial and viral meningitis)",
    x = NULL,
    y = NULL,
    fill = NULL
  )+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.direction = 'vertical',
    legend.position = 'right',
    legend.key.height = unit(2.0, 'cm')
  )

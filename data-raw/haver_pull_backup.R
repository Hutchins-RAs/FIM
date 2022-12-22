# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
##NOTE: RUN THIS ONLY IF HAVER DLX IS NOT WORKING/DOES NOT HAVE THE LATEST DATA. IF HAVER IS WORKING, RUN HAVER_PULL.R AS USUAL.
Sys.setenv(TZ = 'UTC')
librarian::shelf(Haver, dplyr, tidyr, readxl, writexl, tsibble, purrr, openxlsx)

haver.path("//ESDATA01/DLX/DATA/")
devtools::load_all()

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------

# BEA NIPAs 
names_usna <- read_excel("data/haver_names.xlsx")

#pull in sheets from data/haver_backup.xlsx. see readme sheet in workbook on how to update this 
usecon_currentq<- read_excel("data/haver_backup.xlsx", sheet = "usecon", col_names = TRUE)
cpi_currentq<- read_excel("data/haver_backup.xlsx", sheet = "cpi", col_names = TRUE)
wla_currentq<- read_excel("data/haver_backup.xlsx", sheet = "wla", col_names = TRUE)
ctc_currentq<- read_excel("data/haver_backup.xlsx", sheet = "ctc", col_names = TRUE)
state_ui_currentq<- read_excel("data/haver_backup.xlsx", sheet = "state_ui", col_names = TRUE)
usna_currentq<- read_excel("data/haver_backup.xlsx", sheet = "usna", col_names = TRUE)

# Economic Statistics
#THESE ARE PULLING IN OLD DATA FROM HAVER. NOTE THAT THE SCRIPT IS WRITTEN ASSUMING ONLY THE CURRENT QUARTER #S ARE NOT UP TO 
#DATE ON HAVER. IF YOU'RE MANUALLY PULLING IN MULTIPLE QUARTERS OF DATA, MAKE SURE TO UPDATE THE SCRIPT AND WORKBOOK ACCORDINGLY.
usecon <- 
  pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ",
              'LASGOVA', 'LALGOVA', 'CPGS'), 
            "usecon",
            start.date = START)

cpi <- 
  pull_data(c('UI'), 'cpidata', start.date = START) %>%
  monthly_to_quarterly()


# Wages Lost Assistance Program (Monthly)
wla <- pull_data('YPTOLM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>%
  mutate(yptolm = na_if(yptolm, 'NaN'))
# Child Tax Credit (Monthly)

# Since Haver only pulls monthly values, you should manually input the quarterly 
# value if monthly personal income hasn't come out yet. You can find it on the 
# Effects of Selected Federal Pandemic Response Programs on Federal Government Receipts,
# Expenditures, and Saving. For example, after the release of Q1 2022 advanced estimate I ran the code chunk below to pull the monthly data, and then manually inputed the quarterly value with:
# 
# mutate_where(.data = ctc, .where = date == yearquarter('2022 Q1'), yptocm = 105.6)

ctc <- pull_data('YPTOCM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>% 
  mutate(yptocm = na_if(yptocm, 'NaN'))

usna <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble()


monthly_state_ui <- c('LICL', 'LWCL', 'LUFP','LULP','LUWC','LUWP','LUBP','LUWB','LUEX','LUD','LUWBY', 'LUBPT', 'LUFPT', 'LULPT', 'LASGOVA', 'LALGOVA', 'CPGS')

state_ui <- pull_data(monthly_state_ui,
                      'usecon',
                      start.date = START) %>%
  as_tibble()


#IMPUTING THE CURRENT QUARTER #S FROM HAVER_PULL_BACKUP
usecon_imputed<-usecon%>% mutate_where(date == "2022-09-30",
                                   pcw=usecon_currentq$pcw,
                                   gdppothq=usecon_currentq$gdppothq,
                                   gdppotq=usecon_currentq$gdppotq,
                                   recessq=usecon_currentq$recessq,
                                   lasgova=usecon_currentq$lasgova,
                                   lalgova=usecon_currentq$lalgova,
                                   cpgs=usecon_currentq$cpgs)
                                   
cpi_imputed<-cpi%>% mutate_where(date == "2022-09-30", ui=cpi_currentq$ui)

                                                                
wla_imputed<-wla%>% mutate_where(date == "2022-09-30",yptolm=wla_currentq$yptolm)
ctc_imputed<-ctc%>% mutate_where(date == "2022-09-30",yptocm=ctc_currentq$yptocm)

state_ui_imputed<-state_ui%>% mutate_where(date == "2022-09-30",licl=state_ui_currentq$licl,
                                       lwcl=state_ui_currentq$lwcl,
                                       lufp=state_ui_currentq$lufp,
                                       lulp=state_ui_currentq$lulp,
                                       luwc=state_ui_currentq$luwc,
                                       luwp=state_ui_currentq$luwp,
                                       lubp=state_ui_currentq$lubp,
                                       luwb=state_ui_currentq$luwb,
                                       luex=state_ui_currentq$luex,
                                       lud=state_ui_currentq$lud,
                                       luwby=state_ui_currentq$luwby,
                                       lubpt=state_ui_currentq$lubpt,
                                       lufpt=state_ui_currentq$lufpt,
                                       lulpt=state_ui_currentq$lulpt,
                                       lasgova=state_ui_currentq$lasgova,
                                       lalgova=state_ui_currentq$lalgova,
                                       cpgs=state_ui_currentq$cpgs)%>%
  write_xlsx('data/monthly_state_ui.xlsx')
                                       
usna_imputed<-usna%>% mutate_where(date == "2022-09-30",gdp=usna_currentq$gdp,
                               gdph=usna_currentq$gdph,
                               jgdp=usna_currentq$jgdp,
                               c=usna_currentq$c,
                               ch=usna_currentq$ch,
                               jc=usna_currentq$jc,
                               jgf=usna_currentq$jgf,
                               jgs=usna_currentq$jgs,
                               jgse=usna_currentq$jgse,
                               jgsi=usna_currentq$jgsi,
                               yptmr=usna_currentq$yptmr,
                               yptmd=usna_currentq$yptmd,
                               yptu=usna_currentq$yptu,
                               gtfp=usna_currentq$gtfp,
                               ypog=usna_currentq$ypog,
                               yptx=usna_currentq$yptx,
                               ytpi=usna_currentq$ytpi,
                               yctlg=usna_currentq$yctlg,
                               g=usna_currentq$g,
                               grcsi=usna_currentq$grcsi,
                               dc=usna_currentq$dc,
                               gf=usna_currentq$gf,
                               gs=usna_currentq$gs,
                               gfh=usna_currentq$gfh,
                               gsh=usna_currentq$gsh,
                               gfrpt=usna_currentq$gfrpt,
                               gfrpri=usna_currentq$gfrpri,
                               gfrcp=usna_currentq$gfrcp,
                               gfrs=usna_currentq$gfrs,
                               gftfp=usna_currentq$gftfp,
                               gfeg=usna_currentq$gfeg,
                               gsrpt=usna_currentq$gsrpt,
                               gsrpri=usna_currentq$gsrpri,
                               gsrcp=usna_currentq$gsrcp,
                               gsrs=usna_currentq$gsrs,
                               gstfp=usna_currentq$gstfp,
                               gset=usna_currentq$gset,
                               gfeghhx=usna_currentq$gfeghhx,
                               gfeghdx=usna_currentq$gfeghdx,
                               gfeigx=usna_currentq$gfeigx,
                               gfsub=usna_currentq$gfsub,
                               gssub=usna_currentq$gssub,
                               gsub=usna_currentq$gsub,
                               gftfpe=usna_currentq$gftfpe,
                               gftfpr=usna_currentq$gftfpr,
                               gftfpp=usna_currentq$gftfpp,
                               gftfpv=usna_currentq$gftfpv,
                               gfsubp=usna_currentq$gfsubp,
                               gfsubg=usna_currentq$gfsubg,
                               gfsube=usna_currentq$gfsube,
                               gfsubs=usna_currentq$gfsubs,
                               gfsubf=usna_currentq$gfsubf,
                               gfsubv=usna_currentq$gfsubv,
                               gfsubk=usna_currentq$gfsubk,
                               gfegc=usna_currentq$gfegc,
                               gfege=usna_currentq$gfege,
                               gfegv=usna_currentq$gfegv,
                               yptue=usna_currentq$yptue,
                               yptup=usna_currentq$yptup,
                               yptuc=usna_currentq$yptuc,
                               gftfpu=usna_currentq$gftfpu,
                               yptub=usna_currentq$yptub,
                               yptol=usna_currentq$yptol,
                               gfctp=usna_currentq$gfctp,
                               gftffx=usna_currentq$gftffx,
                               ylwsd=usna_currentq$ylwsd,
                               yop=usna_currentq$yop,
                               yri=usna_currentq$yri,
                               ypiar=usna_currentq$ypiar,
                               ycpd=usna_currentq$ycpd,
                               gfsubr=usna_currentq$gfsubr,
                               gfsubd=usna_currentq$gfsubd,
                               gftfbdx=usna_currentq$gftfbdx) %>%
  left_join(cpi_imputed) %>%
  left_join(usecon_imputed) %>% 
  # left_join(child_tax_credit) %>% 
  # Convert SNAP from millions to billions
  mutate(gftffx = gftffx / 1e3) %>% 
  left_join(ctc_imputed, by = 'date')

national_accounts <- 
  usna_imputed %>% 
  mutate(id = 'historical') %>%
  millions_to_billions() %>%
  rename(cpiu = ui,
  ) %>% 
  # Get deflator growth
  mutate(across(starts_with('j'), ~ q_g(.x), .names = '{.col}_growth')) %>% 
  format_tsibble() %>% 
  #When adding new codes to read in from Haver, make sure to relocate them at the end of the spreadsheet using the below function:
  relocate(ylwsd:gftfbdx, .after = 'jgsi_growth') %>% 
  relocate(yptocm, .after = everything())

usethis::use_data(national_accounts, overwrite = TRUE)
devtools::load_all()

# Write Haver pivoted to corresponding sheet in the forecast workbook

haver_pivoted <-
  fim::national_accounts %>% 
  select(-id) %>% 
  pivot_longer(-date) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = date,
              values_from = value) 


boldHeader <- createStyle(textDecoration = 'bold') # Makes first row bold
wb <- loadWorkbook('data/forecast.xlsx')
if (!('Haver Pivoted' %in% names(wb))) addWorksheet(wb, 'Haver Pivoted')
writeData(wb, 'Haver Pivoted', haver_pivoted, headerStyle = boldHeader)
setColWidths(wb, 'Haver Pivoted', cols = 1:ncol(haver_pivoted), widths = 'auto')
saveWorkbook(wb, 'data/forecast.xlsx', overwrite = T)
# Check values and then:
# gert::git_commit_all('Haver update')
# gert::git_push()
                
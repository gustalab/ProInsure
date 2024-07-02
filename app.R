# 1.0 LOAD PACKAGES & LIBRARIES ----

library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(tibble)
library(dplyr)
library(billboarder)
library(tidyverse)
library(tidyquant)
library(kableExtra)
library(scales)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggdensity)
library(shinythemes)
library(shinydashboard)
#install.packages("shinyBS")
library(shinyBS)
library(reactable)
library(formattable)
library(shinyalert)
library(collapsibleTree)

library(tidyverse)
library(tidyquant)
library(plotly)
library(ggplot2)
#install.packages("ggExtra")
library(ggExtra)

# library(hrbrthemes)
# library(viridis)

library(highcharter) #interactive visualization

library(odbc) 
library(RODBC) 
library(DBI) 

library(readxl)

library(imputeTS) # replace NA's with a value in df

# install.packages("colorDF")
library(colorDF)
# install.packages("DT")

# install.packages("kableExtra")
# install.packages("devtools")
# devtools::install_github("haozhu233/kableExtra")
library(kableExtra)
# install.packages("flextable")
library(flextable)
# install.packages("pacman")
library(pacman)

# install.packages("formattable")                              # Install and load formattable
library("formattable")

library(billboarder) # https://cran.r-project.org/web/packages/billboarder/vignettes/billboarder.html

# Map

library(osrm)
library(sf)
# Map
library(sf)
#library(mapview)
library(leaflet)
library(billboarder)

library(markdown)

# Modelling
# install.packages("Amelia")
library(Amelia)
# install.packages("caret")
library(caret)
library(broom)
library(modelr)

library(shinyauthr) #devtools::install_github("business-science/shinyauthr)


Sys.setlocale(locale = "Turkish")


# 2.0 LOAD SOURCES & DATA ----

Sys.setlocale(locale = "Turkish")

source("../ProInsure/modules/module_login.R")


# 3.0 AUTH DATA SAVE ----


# Define the path to the CSV file where data will be stored
data_file <- "data/proinsure_data.csv"

# Ensure the directory exists
if (!dir.exists("data")) {
  dir.create("data")
}

# Function to load the data from the CSV file
load_data <- function(file) {
  if (file.exists(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
    # Add Duration and ID columns if they don't exist
    if (!"Duration" %in% names(data)) {
      data$Duration <- 1
    }
    if (!"ID" %in% names(data)) {
      data$ID <- sapply(1:nrow(data), function(x) {
        letters <- sample(LETTERS, 2, replace = TRUE)
        numbers <- sample(0:9, 3, replace = TRUE)
        pattern <- paste0(paste(letters, collapse = ""), paste(numbers, collapse = ""))
        return(pattern)
      })
    }
    data
  } else {
    data.frame(
      ID = character(),
      user_name = character(),
      DosyaNo = character(),
      Cinsiyet = character(),
      DogumTarihi = character(),
      Gelir = character(),
      KazaTarihi = character(),
      MaluliyetOran = numeric(),
      KusurOran = numeric(),
      GeciciMaluliyetSure = numeric(),
      KismiOdemeSay = numeric(),
      RaporTur = character(),
      Duration = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

# Function to save the data to the CSV file
save_data <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

# Generate a unique ID
generate_id <- function() {
  # Rastgele iki büyük harf oluştur
  letters <- sample(LETTERS, 2, replace = TRUE)
  # Rastgele üç rakam oluştur
  numbers <- sample(0:9, 3, replace = TRUE)
  # Harfleri ve rakamları birleştir
  pattern <- paste0(paste(letters, collapse = ""), paste(numbers, collapse = ""))
  return(pattern)
}


# 4.0 APP ----

## 4.1 UI ----

ui <-  tagList(
  
  ### css 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("spacelab")),
    tags$style(HTML("#mapp {height:100%, width:100%;}, {background-color: rgba(0,0,255,0.7);}"))
  ),
  
  ### js 
  shinyjs::useShinyjs(),
  
  useShinyjs(),
  #title = "Login Module",
  
  shinyauthr::loginUI(id = "login_3",title = "PROINSURE HESAPLAMA", error_message = "Wrong username or password", user_title = "Username", pass_title = "Password", login_title = "Enter" ),
  
  uiOutput(outputId = "web_page")
  
)

## 4.2 SERVER ----


server <- function(input, output,session) {
  
  ### 4.2.1 USER CREDENTIALS ----
  
  user_base_tbl <- tibble(user_name = c("user1", "user2", "user3"),
                          password = c("pass1", "pass2", "pass3")
  )
  
  ### 3.2.2 SHINYAUTHR ----
  
  credentials <- shinyauthr::loginServer(
    id       = "login_3",
    data     = user_base_tbl,
    user_col = user_name,
    pwd_col  = password,
    log_out  = reactive(logout_init()))
  
  user_auth <- reactive({
    credentials()$user_auth
  })
  
  user_data <- reactive({
    credentials()$info
  })
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(user_auth())
  )
  
  output$creds <- renderPrint({
    credentials()
  })
  
  
  ### 4.2.3 SERVER FUNCs ----
  
  #### Save and Load Data ----
  
  # ReactiveValues object to store the accumulated data
  values <- reactiveValues(data = load_data(data_file))
  
  # Define correct password
  correct_password <- "12345" # Change to your desired password
  
  # Reactive value to control the visibility of the data table and download button
  show_table <- reactiveVal(FALSE)
  show_download_button <- reactiveVal(FALSE)
  
  # Observe event for the submit button
  
  ## NOT: Burada data save durumunu Submit butonuna bağlaandı. Action button türünde olduğu için. Generate Report butonu downloadbutton türünde olduğu için trigger edemedim. Yapılabilirse Submit butonu kaldırılıp direk generate_report üzerinden save edilebilir.
  observeEvent(input$submit, {
  # observeEvent(input$generate_report, {  
    # Calculate duration in minutes
    duration <- as.numeric(round(difftime(Sys.time(), values$start_time, units = "mins"), digits = 2))
    
    # Create a new record
    new_record <- tibble(
      
      ID = generate_id(),
      user_name = isolate(user_data()$user_name),  # isolate to prevent reactivity
      DosyaNo = ifelse(input$dosya == TRUE,input$dosya,"-"),
      Cinsiyet = input$cinsiyet,
      DogumTarihi =  as.character(input$dogumtarihi),
      Gelir =  input$gelir,
      KazaTarihi = as.character(input$kazatarihi),
      MaluliyetOran = input$maluliyet, 
      KusurOran = input$kusur,
      GeciciMaluliyetSure =input$maluliyet_sure,
      KismiOdemeSay = input$kısmiodeme,
      RaporTur = input$rapor,
      Duration = duration,
      EntryTime = as.character(Sys.time()) # Add system date and time when the user entered the app
    )
    
    # Append the new record to the existing data
    values$data <- bind_rows(values$data, new_record)
    
    # Save the updated data to the CSV file
    save_data(values$data, data_file)
    
    # Show the data table and download button
    show_table(TRUE)
    # show_download_button(TRUE)
    
    # Show a notification
    # showNotification("Data submitted successfully!", type = "message")
    
  })
  
  # Render the data table in the UI
  output$dataTable <- renderDT({
    datatable(values$data)
  })
  
  # Control the visibility of the data table and download button
  output$showTable <- reactive({
    show_table()
  })
  
  # output$showDownloadButton <- reactive({
  #   show_download_button()
  # })
  # 
  outputOptions(output, "showTable", suspendWhenHidden = FALSE)
  # outputOptions(output, "showDownloadButton", suspendWhenHidden = FALSE)
  
  # Handle download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )
  
  ### Initialize start_time reactive value
  observe({
    values$start_time <- Sys.time()
  })
  
  
  #### Functions ----
  
  kaza_t <- as.Date("2018-01-01")
  
  input_data <- data.frame(Brand = seq(from = kaza_t , to = Sys.Date(), by = "year"),
                           Gelir = 0,
                           stringsAsFactors = FALSE) %>%
    mutate(Yıl = format(Brand, format("%Y"))) %>%
    select(Yıl, Gelir)
  
  #### Params List ----
  
  output$generate_report <- downloadHandler(
    filename = "rendered_report.docx",
    content = function(file) {
      
      if(input$rapor == "Tüm Rapor-1 Ödeme") {
        res <- rmarkdown::render(
          "tum_report_1odeme.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$kısmiodeme,
            PKismi_Odeme_Tarihi_1 = input$kısmiodemetarihi1,
            PKismi_Odeme_Tutari_1 = input$ko1,
            # PKısmi_Odeme_Tarihi_2 = input$kısmiodemetarihi2,
            # PKısmi_Odeme_Tutarı_2 = input$ko2,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PBakici = input$bakici_gider,
            PBakici_Sure = input$bakici_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
            
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Tüm Rapor-2 Ödeme") {
        res <- rmarkdown::render(
          "tum_report_2odeme.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$kısmiodeme,
            PKismi_Odeme_Tarihi_1 = input$kısmiodemetarihi2,
            PKismi_Odeme_Tutari_1 = input$ko2,
            # PKısmi_Odeme_Tarihi_2 = input$kısmiodemetarihi2,
            # PKısmi_Odeme_Tutarı_2 = input$ko2,
            PKismi_Odeme_Tarihi_2 = input$kısmiodemetarihi3,
            PKismi_Odeme_Tutarı_2 = input$ko3,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PBakici = input$bakici_gider,
            PBakici_Sure = input$bakici_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
            
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Tüm Rapor (Şirket Ödemesiz)") {
        
        res <- rmarkdown::render(
          "tum_report_sirket_odemesiz1.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici = input$bakici_gider,
            PBakici_Sure = input$bakici_sure,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Sürekli+Geçici (Şirket Ödemesiz)") {
        
        res <- rmarkdown::render(
          "surekli_gecici_sirket_odemesiz.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici_Sure = input$bakici_sure,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Geçici (Şirket Ödemesiz)") {
        
        res <- rmarkdown::render(
          "gecici_sirket_odemesiz.Rmd",
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici_Sure = input$bakici_sure,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Sürekli (Şirket Ödemesiz)") {
        
        res <- rmarkdown::render(
          "surekli_sirket_odemesiz.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PBakici_Sure = input$bakici_sure,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
          )
        )
        file.rename(res, file)
        
      } else if (input$rapor == "Sürekli (Şirket Ödemeli)") {
        
        res <- rmarkdown::render(
          "surekli_sirket_odemeli.Rmd",
          params = list(
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PMaluliyet_Orani = input$maluliyet,
            PKusur_Orani = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKismi_Odeme_Sayisi = input$kısmiodeme,
            PKismi_Odeme_Tarihi_1 = input$kısmiodemetarihi1,
            PKismi_Odeme_Tutari_1 = input$ko1,
            # PKısmi_Odeme_Tarihi_2 = input$kısmiodemetarihi2,
            # PKısmi_Odeme_Tutarı_2 = input$ko2,
            PGecici_Maluliyet_Sure = input$maluliyet_sure,
            PBakici = input$bakici_gider,
            PBakici_Sure = input$bakici_sure,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2
          )
        )
        file.rename(res, file)
        
      } else {
        
        res <- rmarkdown::render(
          "Report_Template_Vefat_Shiny.Rmd",
          params = list(
            # draw_plot = draw_plot,
            PDosya_No = input$dosya,
            PAd_Soyad = input$isim,
            PCinsiyet = input$cinsiyet,
            PKusur_Oranı = input$kusur,
            PKaza_Tarihi = input$kazatarihi,
            PDogum_Tarihi = input$dogumtarihi,
            PKısmi_Odeme_Sayisi = input$kısmiodeme,
            PKısmi_Odeme_Tarihi_1 = input$kısmiodemetarihi1,
            PKısmi_Odeme_Tutarı_1 = input$ko1,
            PGelir = input$asgari_durum,
            PYasam_Tablosu = input$tablo2,
            PEs = input$es,
            PEsAd = input$es_isim,
            PEsDT = input$esdogumtarihi,
            PAnne = input$anne,
            PAnneAd = input$anne_isim,
            PAnneDT = input$annedogumtarihi,
            PBaba = input$baba,
            PBabaAd = input$baba_isim,
            PBabaDT = input$babadogumtarihi,
            PCocuksay = input$cocuksay,
            PCocuk1_DT = input$cocukdogumtarihi11,
            PCocuk1_Ad = input$cocuk1_isim,
            PCocuk2_DT = input$cocukdogumtarihi22,
            PCocuk2_Ad = input$cocuk2_isim,
            PCocuk3_DT = input$cocukdogumtarihi33,
            PCocuk3_Ad = input$cocuk3_isim,
            PCocuk4_DT = input$cocukdogumtarihi44,
            PCocuk4_Ad = input$cocuk4_isim,
            PCocuk5_DT = input$cocukdogumtarihi55,
            PCocuk5_Ad = input$cocuk5_isim
          )
        )
        file.rename(res, file)
      }
      
    }
  )
  
  
  
  
  
  
  
  
  
  #### Hidden objects ----
  
  # Genel Bilgiler
  observeEvent(input$action_button1, {
    toggle(id = "settings_toggle1", anim = TRUE)
  })
  
  # Şirket Ödemeleri
  observeEvent(input$action_button2, {
    toggle(id = "settings_toggle2", anim = TRUE)
  })
  
  # Aile Bilgileri
  observeEvent(input$action_button3, {
    toggle(id = "settings_toggle3", anim = TRUE)
  })
  
  # Gelir Bilgisi
  observeEvent(input$action_button4, {
    toggle(id = "settings_toggle4", anim = TRUE)
  })
  
  # Bakıcı Bilgisi
  observeEvent(input$action_button6, {
    toggle(id = "settings_toggle6", anim = TRUE)
  })
  
  #### Edited Gelir Table ----
  
  demodata<-input_data
  edited <- callModule(modFunction,"editable", demodata,
                       reset = reactive(input$reset))
  observe(print(edited$data))
  
  output$ort_gelir<- renderText(paste0(round(mean(edited$data$Gelir),digits = 1), " ", "TL"))
  
  output$teknikfaiztext<- renderText("Teknik Faiz % 0 'dır.")
  
  
  #### Gelir Tablosu ----
  
  
  kisi_gelir_tablosu <- reactive({
    
    Asgari_Tablo <- read_excel("../ProInsure/data/Asgari_Ucret_Tablosu.xlsx", sheet = "Program")
    
    excel_data <- if(input$gelir == 'Diğer') {
      edited$data
    } 
    else {
      Asgari_Tablo
    } 
    
  })
  
  
  
  
  #### Values ----
  
  # Data entry
  observeEvent(input$info_button1, {
    toggle(id = "info1", anim = TRUE)
  })
  
  ####
  
  output$table <- DT::renderDataTable({
    DT::datatable(son_hesaplama_tablosu, options = list(paging = F, searching = F))
  })
  
  # Submit button notification ----
  
  # Observe event for the submit button
  observeEvent(input$submit, {
    # Show a notification
    showNotification("Data submitted successfully!", type = "message")
    
  })
  
  
  ###### Reactive expression to create data frame of all input values ----
  
  sliderValues <- reactive({
    
    kot1 <- ifelse(as.character(input$kısmiodemetarihi1) == Sys.Date(),"-",as.character(input$kısmiodemetarihi1))
    kot2 <- ifelse(as.character(input$kısmiodemetarihi2) == Sys.Date(),"-",as.character(input$kısmiodemetarihi2))
    kot3 <- ifelse(as.character(input$kısmiodemetarihi3) == Sys.Date(),"-",as.character(input$kısmiodemetarihi3))
    kot4 <- ifelse(as.character(input$kısmiodemetarihi4) == Sys.Date(),"-",as.character(input$kısmiodemetarihi4))
    kot5 <- ifelse(as.character(input$kısmiodemetarihi5) == Sys.Date(),"-",as.character(input$kısmiodemetarihi5))
    kot6 <- ifelse(as.character(input$kısmiodemetarihi6) == Sys.Date(),"-",as.character(input$kısmiodemetarihi6))
    
    # kot1 <- ifelse(as.Date(input$kısmiodemetarihi1) == Sys.Date(),"-",as.character(input$kısmiodemetarihi1))
    # kot2 <- ifelse(as.Date(input$kısmiodemetarihi2) == Sys.Date(),"-",as.character(input$kısmiodemetarihi2))
    # kot3 <- ifelse(as.Date(input$kısmiodemetarihi3) == Sys.Date(),"-",as.character(input$kısmiodemetarihi3))
    # kot4 <- ifelse(as.Date(input$kısmiodemetarihi4) == Sys.Date(),"-",as.character(input$kısmiodemetarihi4))
    # kot5 <- ifelse(as.Date(input$kısmiodemetarihi5) == Sys.Date(),"-",as.character(input$kısmiodemetarihi5))
    # kot6 <- ifelse(as.Date(input$kısmiodemetarihi6) == Sys.Date(),"-",as.character(input$kısmiodemetarihi6))
    
    excel_data <- if(input$kısmiodeme == '1' & input$gelir == "Asgari ücret") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$kusur,
                  input$maluliyet_sure,
                  input$bakici_sure,
                  input$kısmiodeme,
                  kot1,
                  input$ko1
        )
      )
    } 
    
    else if(input$kısmiodeme == '2' & input$gelir == "Asgari ücret") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$kusur,
                  input$maluliyet_sure,
                  input$bakici_sure,
                  input$kısmiodeme,
                  kot2,
                  input$ko2,
                  kot3,
                  input$ko3
        )
      )
    } 
    
    else if(input$kısmiodeme == '3' & input$gelir == "Asgari ücret") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2",
                 "Kısmi Ödeme Tarihi-3",
                 "Kısmi Ödeme Tutarı-3"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$kusur,
                  input$maluliyet_sure,
                  input$bakici_sure,
                  input$kısmiodeme,
                  kot4,
                  input$ko4,
                  kot5,
                  input$ko5,
                  kot6,
                  input$ko6
        )
      )
    }
    
    else if(input$kısmiodeme == '1' & input$gelir == "Diğer") {
      
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  kot1,
                  input$ko1
        )
      )
    }
    
    
    
    else if(input$kısmiodeme == '2' & input$gelir == "Diğer") {
      
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  kot2,
                  input$ko2,
                  kot3,
                  input$ko3
        )
      )
    } 
    
    else if(input$kısmiodeme == '3' & input$gelir == "Diğer") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Bakıcı Süresi (ay)",
                 "Kısmi Ödeme Sayısı",
                 "Kısmi Ödeme Tarihi-1",
                 "Kısmi Ödeme Tutarı-1",
                 "Kısmi Ödeme Tarihi-2",
                 "Kısmi Ödeme Tutarı-2",
                 "Kısmi Ödeme Tarihi-3",
                 "Kısmi Ödeme Tutarı-3"
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme,
                  kot4,
                  input$ko4,
                  kot5,
                  input$ko5,
                  kot6,
                  input$ko6
        )
      )
    }
    
    
    else if(input$kısmiodeme == '0' & input$gelir == "Asgari ücret") {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı"
                 
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme
                  
        )
      )
    }
    
    
    else {
      data.frame(
        Name = c("Dosya No",
                 "Ad Soyad",
                 "Cinsiyet",
                 "Doğum Tarihi",
                 "Gelir",
                 "Asgari Ucret Durumu",
                 "Kaza Tarihi",
                 "Maluliyet Oranı",
                 "Bakıcı Süresi (ay)",
                 "Kusur Oranı",
                 "Geçici Maluliyet (ay)",
                 "Kısmi Ödeme Sayısı"
                 
        ),
        
        Value = c(input$dosya,
                  input$isim,
                  input$cinsiyet,
                  as.character(input$dogumtarihi),
                  input$gelir,
                  input$asgari_durum,
                  as.character(input$kazatarihi),
                  input$maluliyet,
                  input$bakici_sure,
                  input$kusur,
                  input$maluliyet_sure,
                  input$kısmiodeme
                  
        )
      )
    }
    
    
  })
  
  ###### Show the values in an HTML table ----
  
  output$table3 <- DT::renderDataTable(
    sliderValues(),
    options = list(
      pageLength = -1,
      dom = 'rtip' # 'l' ve 'f' kaldırıldı, diğer bileşenler kaldı
    )
  )
  
  # dom seçeneği varsayılan olarak lfrtip şeklindedir:
  #   
  # l - "Length changing" (sayfa uzunluğu menüsü)
  # f - "Filtering input" (arama kutusu)
  # r - "Processing" display element
  # t - The table!
  # i - Table information summary
  # p - Pagination control
  
  # lengthChange = FALSE, # 'show' alanını gizler
  # searching = FALSE     # 'search' alanını gizler
  
  output$dl <- downloadHandler(
    filename = function() {"genel_bilgiler_tablosu.xlsx"},
    content = function(file) {write_xlsx(sliderValues(), path = file)}
  )
  
  
  
  ### 4.2.4 RENDER UI ----
  
  output$web_page <- renderUI({
    
    req(user_auth()) 
    
    #### dashboard page ----
    
    dashboardPage(
      
      
      ##### dashboard header ----
      
      dashboardHeader(
        title = tags$span("PROINSURE", style = "color: #ffffff;")
      ), 
      
      ##### dashboard sidebar ----
      
      dashboardSidebar(
        
        tags$style(
          HTML("
          .main-sidebar {
          background-color: #000000 !important;
          }
               ")
        ),
        
        hr(),
        
        radioGroupButtons(
          inputId = "yontem",
          label = p(tags$b("Hesaplama Yöntemi", style = "font-weight: solid; color: red;")),
          choices = c("Aktüeryal", "Progresif Rant"),
          selected = "Progresif Rant",
          individual = T,
          width = '400px',
          justified = T,
          direction = "vertical",
          size = "sm",
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: red"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: steelblue"))
        ),
        
        conditionalPanel(
          condition = "input.yontem == 'Progresif Rant'",
          
          prettyRadioButtons(
            inputId = "tablo2",
            label = "Tablo seçiniz:", 
            choices = c("PMF-1931", "TRH-2010","TUIK_20-22","TUIK_19-21","TUIK_18-20", "CSO-1980"),
            selected = "TRH-2010",
            icon = icon("check"), 
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          ),
          
          div(
            id="info1",
            helpText("* Progresif Rant yönteminde Teknik Faiz % 0 olarak hesaplanmaktadır.")
            
          )
          
        ),
        
        conditionalPanel(
          condition = "input.yontem == 'Aktüeryal'" ,
          
          prettyRadioButtons(
            inputId = "tablo1",
            label = "Tablo seçiniz:", 
            choices = c("PMF-1931", "TRH-2010","TUIK/20-22","TUIK/19-21","TUIK/18-20", "CSO-1980"),
            selected = "TRH-2010",
            icon = icon("check"), 
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          ),
          
          numericInputIcon(
            inputId = "teknik_faiz",
            label = "Teknik Faiz",
            value = 1.8, step = 0.1,
            min = 0, max = 20,
            icon = icon("percent")
          )
        ),
        
        br(),
        hr()
        
        
      ), # dashboard sidebar end
      
      ##### dashboard body----
      
      dashboardBody(
        
        shinyjs::useShinyjs(),
        style = "background-color: #26224c;",
        
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        
        
        fluidRow(
          
          ###### VERİ GİRİŞİ COL ----
          
          column(
            width = 6,
            
            h3("DOSYA BİLGİLERİ",style = "font-weight: solid; color: red;"),
            
            ###### *Genel Bilgiler ----
            
            box(status = "warning",
                
                column(
                  width = 12,
                  
                  wellPanel(
                    textInput(inputId = "dosya",label = "Dosya No", width = 250),
                    textInput(inputId = "isim",label = "Ad-Soyad", width = 250),
                    
                    br(),
                    p(tags$b("Kaza Tarihi")),
                    dateInput("kazatarihi", label = NULL),
                    p(tags$b("Kusur Oranı")),
                    sliderInput("kusur", label = NULL, min = 0, 
                                max = 100, value = 50,step = 5),
                  ),
                ),
                
                column(
                  width = 12,
                  
                  wellPanel(
                    
                    prettyRadioButtons(
                      inputId = "cinsiyet",
                      label = "Cinsiyet", 
                      choices = c("Erkek", "Kadın"),
                      icon = icon("check"), 
                      bigger = TRUE,
                      status = "warning",
                      animation = "jelly", selected = "Erkek", fill = FALSE, inline = TRUE
                    ),
                    
                    p(tags$b("Doğum Tarihi")),
                    dateInput("dogumtarihi", label = NULL),
                    
                    p(tags$b("Maluliyet Oranı")),
                    # sliderInput("maluliyet", label = NULL, min = 0, 
                    # max = 100, value = 50,step = 1),
                    numericInputIcon(
                      inputId = "maluliyet", 
                      label = NULL, value =0, 
                      step= 0.1, min=0, max=100,icon=icon("calender")),
                    
                    p(tags$b("Geçici Maluliyet (Ay)")),
                    awesomeRadio("gecici_maluliyet", label = NULL,
                                 choices = c("Var", "Yok"),
                                 selected = "Yok", inline=TRUE, checkbox = TRUE),
                    conditionalPanel(
                      condition = "input.gecici_maluliyet == 'Var'",
                      
                      numericInputIcon(
                        inputId = "maluliyet_sure",
                        label = NULL,
                        value = 0, step = 0.1,
                        min = 0, max = 120,
                        icon = icon("calendar")
                      )
                    )
                  )
                  
                ),
                
                ###### *Şirket Ödemeleri ----
                column(
                  width = 12,
                  
                  wellPanel(
                    p(tags$b("Şirket Ödemeleri", style = "font-weight: bold; color: red;")),
                    
                    p(tags$b("Kısmi Ödeme Sayısı")),
                    numericInput("kısmiodeme", label = NULL, value = 0, min = 0, max = 3),
                    
                    conditionalPanel(
                      condition = "input.kısmiodeme == '1'",
                      
                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                     
                                     div( id = "1",
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                            dateInput("kısmiodemetarihi1", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko1",
                                              label = "Kısmi Ödeme Tutarı-1",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          )
                                     ),
                                     circle =FALSE, status = "danger",
                                     icon = icon("turkish-lira-sign"), width = "300px"
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.kısmiodeme == '2'",
                      
                      
                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                     
                                     div( id = "2",
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                            dateInput("kısmiodemetarihi2", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko2",
                                              label = "Kısmi Ödeme Tutarı-1",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          ),
                                          hr(),
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-2")),
                                            dateInput("kısmiodemetarihi3", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko3",
                                              label = "Kısmi Ödeme Tutarı-2",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          )
                                          
                                     ),
                                     
                                     circle =FALSE, status = "danger",
                                     icon = icon("turkish-lira-sign"), width = "300px"
                                     
                      )
                      
                    ),
                    
                    conditionalPanel(
                      condition = "input.kısmiodeme == '3'",
                      
                      
                      dropdownButton(label = "Kısmi Ödeme Bilgileri",
                                     
                                     div( id = "3",
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-1")),
                                            dateInput("kısmiodemetarihi4", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko4",
                                              label = "Kısmi Ödeme Tutarı-1",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          ),
                                          hr(),
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-2")),
                                            dateInput("kısmiodemetarihi5", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko5",
                                              label = "Kısmi Ödeme Tutarı-2",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          ),
                                          hr(),
                                          wellPanel(
                                            p(tags$b("Kısmi Ödeme Tarihi-3")),
                                            dateInput("kısmiodemetarihi6", label = NULL),
                                            numericInputIcon(
                                              inputId = "ko6",
                                              label = "Kısmi Ödeme Tutarı-3",
                                              value = 0,
                                              icon = list(icon("turkish-lira-sign"), ".00")
                                            )
                                          )
                                          
                                     ),
                                     
                                     circle =FALSE, status = "danger",
                                     icon = icon("turkish-lira-sign"), width = "300px"
                                     
                      )
                      
                    )
                    
                  )
                  
                  
                )
                
                
                
            ),
            
            ###### * Diğer Bilgiler ----
            
            box(status = "warning", 
                
                "'Destekten Yoksun Kalma' hesaplamalarında kullanılacak bilgiler bu bölüme girilecektir. Ayrıca, kişinin gelir durumu asgari ücretten farklı ise gelir bilgisini giriniz!",
                
                br(),
                br(),
                br(),
                hr(),
                
                column(
                  width = 12,
                  
                  ####### ** Gelir Bilgisi ----
                  
                  column(
                    width = 12,
                    
                    div(
                      id = "gelir_bilgisi",
                      actionButton(inputId = "action_button4", label = "Gelir Bilgisi", icon = icon("cog"))
                    ),
                    
                    div(
                      id="settings_toggle4",
                      
                      wellPanel(
                        
                        p(tags$b("Gelir Durumu", style = "font-weight: bold; color: red;")),
                        awesomeRadio("gelir", label = NULL,
                                     choices = c("Asgari ücret", "Diğer"),
                                     inline=TRUE, checkbox = TRUE),
                        
                        
                        conditionalPanel(
                          condition = "input.gelir == 'Diğer'",
                          
                          modFunctionUI("editable"),
                          h6("Ortalama Gelir", style = "font-weight: bold; color: black;"),
                          verbatimTextOutput("ort_gelir")
                          
                        ),
                        
                        
                        conditionalPanel(
                          condition = "input.gelir == 'Asgari ücret'",
                          
                          awesomeRadio(
                            inputId = "asgari_durum",
                            label = "Durumu",
                            choices = c("Bekar", "evli_cocuksuz", "1cocuk","2cocuk","3cocuk","4cocuk"),
                            inline=FALSE, checkbox = TRUE)
                          
                          
                        )
                        
                      )
                      
                    ) %>% hidden()
                    
                    
                    
                  ),
                  
                  
                  br(),
                  hr(),
                  ####### ** Aile Bilgileri ----
                  
                  column(
                    width = 12,
                    
                    div(
                      id = "aile_bilgiler",
                      actionButton(inputId = "action_button3", label = "Aile Bilgileri", icon = icon("cog"))
                    ),
                    
                    div(
                      id="settings_toggle3",
                      
                      wellPanel(
                        
                        p(tags$b("Aile Bilgileri", style = "font-weight: bold; color: red;")),
                        ####### *** Eş ----
                        div(
                          p(tags$b("Eş")),
                          awesomeRadio("es", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.es == 'Var'",
                            
                            textInput(inputId = "es_isim",label = "Eş Ad-Soyad", width = 250),
                            
                            dropdownButton(label = "Eş Doğum Tarihi",
                                           
                                           div( id = "1",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("esdogumtarihi", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          )
                        ), 
                        ####### *** Anne ----
                        div(
                          p(tags$b("Anne")),
                          awesomeRadio("anne", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.anne == 'Var'",
                            
                            textInput(inputId = "anne_isim",label = "Anne Ad-Soyad", width = 250),
                            
                            dropdownButton(label = "Anne Doğum Tarihi",
                                           
                                           div( id = "1",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("annedogumtarihi", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          )
                        ), 
                        ####### *** Baba ----
                        div(
                          p(tags$b("Baba")),
                          awesomeRadio("baba", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.baba == 'Var'",
                            
                            textInput(inputId = "baba_isim",label = "Baba Ad-Soyad", width = 250),
                            
                            
                            dropdownButton(label = "Baba Doğum Tarihi",
                                           
                                           div( id = "1",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("babadogumtarihi", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          )
                        ), 
                        
                        ####### *** Çocuk ----
                        div(
                          
                          p(tags$b("1. Çocuk")),
                          awesomeRadio("cocuk1", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.cocuk1 == 'Var'",
                            
                            textInput(inputId = "cocuk1_isim",label = "1.Çocuk Ad-Soyad", width = 250),
                            
                            
                            dropdownButton(label = "1. Çocuk Doğum Tarihi",
                                           
                                           div( id = "1",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("cocukdogumtarihi11", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          ),
                          
                          
                          p(tags$b("2. Çocuk")),
                          awesomeRadio("cocuk2", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.cocuk2 == 'Var'",
                            
                            textInput(inputId = "cocuk2_isim",label = "2.Çocuk Ad-Soyad", width = 250),
                            
                            
                            dropdownButton(label = "2. Çocuk Doğum Tarihi",
                                           
                                           div( id = "2",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("cocukdogumtarihi22", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          ),
                          
                          p(tags$b("3. Çocuk")),
                          awesomeRadio("cocuk3", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.cocuk3 == 'Var'",
                            
                            textInput(inputId = "cocuk3_isim",label = "3.Çocuk Ad-Soyad", width = 250),
                            
                            dropdownButton(label = "3. Çocuk Doğum Tarihi",
                                           
                                           div( id = "3",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("cocukdogumtarihi33", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          ),
                          
                          
                          p(tags$b("4. Çocuk")),
                          awesomeRadio("cocuk4", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.cocuk4 == 'Var'",
                            
                            textInput(inputId = "cocuk4_isim",label = "4.Çocuk Ad-Soyad", width = 250),
                            
                            dropdownButton(label = "4. Çocuk Doğum Tarihi",
                                           
                                           div( id = "4",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("cocukdogumtarihi44", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          ),
                          
                          
                          p(tags$b("5. Çocuk")),
                          awesomeRadio("cocuk5", label = NULL,
                                       choices = c("Var", "Yok"),
                                       selected = "Yok", inline=TRUE, checkbox = TRUE),
                          conditionalPanel(
                            condition = "input.cocuk5 == 'Var'",
                            
                            textInput(inputId = "cocuk5_isim",label = "5.Çocuk Ad-Soyad", width = 250),
                            
                            dropdownButton(label = "5. Çocuk Doğum Tarihi",
                                           
                                           div( id = "5",
                                                wellPanel(
                                                  p(tags$b("Doğum Tarihi")),
                                                  dateInput("cocukdogumtarihi55", label = NULL)
                                                )
                                           ),
                                           circle =FALSE, status = "danger",
                                           icon = icon("fa-thin fa-calendar"), width = "300px"
                            )
                          )
                          
                        )
                        
                      )
                      
                      
                      
                      
                      
                    ) %>% hidden()
                    
                  ),
                  
                  
                  
                  br(),
                  hr(),
                  
                  ####### ** Bakıcı Bilgisi ----
                  
                  column(
                    width = 12,
                    
                    div(
                      id = "bakici_bilgiler",
                      actionButton(inputId = "action_button6", label = "Bakıcı Bilgisi", icon = icon("cog"))
                    ),
                    
                    div(
                      id="settings_toggle6",
                      
                      wellPanel(
                        
                        p(tags$b("Bakıcı Gideri", style = "font-weight: bold; color: red;")),
                        awesomeRadio("bakici_gider", label = NULL,
                                     choices = c("Var", "Yok"),
                                     selected = "Yok", inline=TRUE, checkbox = TRUE),
                        
                        p(tags$b("Bakıcı Tutuldu mu?", style = "font-weight: bold; color: red;")),
                        awesomeRadio("bakici_tut", label = NULL,
                                     choices = c("Evet", "Hayır"),
                                     selected = "Hayır", inline=TRUE, checkbox = TRUE),
                        p(tags$b("Bakıcı Gideri Süresi (Gün)")),
                        conditionalPanel(
                          condition = "input.bakici_gider == 'Var'",
                          
                          numericInputIcon(
                            inputId = "bakici_sure",
                            label = NULL,
                            value = 0, step = 0.1,
                            min = 0, max = 120,
                            icon = icon("calendar")
                          )
                          
                        )
                        
                      )
                      
                    ) %>% hidden()
                    
                  )
                ),
                
                
            )
            
            ###### * Diğer Bilgiler End ----
            
            
            
            
          ), # Veri girisi col end 
          
          
          ###### SONUÇLAR COL ----
          
          column(
            width = 6,
            
            h3("SONUÇLAR",style = "font-weight: solid; color: red;"),
            
            
            box(
              # title = "sonuclar",
              column(
                width = 12,
                wellPanel(
                  dataTableOutput('table3'),
                  # downloadButton("dl", "Download Table")
                )
                
              )
              
            ),
            
            column(
              width = 4,
              radioGroupButtons(
                inputId = "rapor",
                # label = "Rapor Seç",
                label = p(tags$b("Rapor Seç", style = "font-weight: bold; color: red;")),
                choices = c("Tüm Rapor-1 Ödeme", "Tüm Rapor-2 Ödeme", "Tüm Rapor (Şirket Ödemesiz)", "Sürekli+Geçici (Şirket Ödemesiz)", 
                            "Geçici (Şirket Ödemesiz)", "Sürekli (Şirket Ödemeli)", "Sürekli (Şirket Ödemesiz)", "Destek"),
                justified = TRUE,
                direction = "vertical",
                status = "danger",
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"))
              ),
              
              
              # passwordInput("password", "Enter Password"),
              actionButton("submit", "Submit"),
              conditionalPanel(
                condition = "output.showDownloadButton == true",
                downloadButton("downloadData", "Download Data")
              ),
              
              
              conditionalPanel(
                condition = "input.submit == true",
                downloadButton("generate_report", label = "Generate Report")
              ),
              
              
              
            )
            
          ) # sonuclar div end 
        )
      )  # dashboard body end 
      
    ) # dashboard page end 
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} # server son






# Run the application 
shinyApp(ui = ui, server = server)
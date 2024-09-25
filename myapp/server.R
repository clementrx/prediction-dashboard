server <- function(input, output, session) {
  
  observeEvent(input$hideSidebar, {
    if (input$hideSidebar %% 2 == 1) {
      shinyjs::hide(id = "Sidebar")
    } else {
      shinyjs::show(id = "Sidebar")
    }
  })
  
  data <- reactive({
    
    req(input$file)  # Ensure file is uploaded
    data <- read.csv2(input$file$datapath)
    # data <- read.csv2("pred_20240925.csv")
    
    data = data %>% 
      mutate(P1 = ifelse(is.na(P1), 0, P1)) %>%
      arrange(R_pmuNumber, C_number) %>% 
      distinct(horseName, saddle, C_uuid, .keep_all = TRUE) %>% 
      group_by(C_uuid) %>% 
      mutate(seuil_P1 = quantile(P1, probs = 0.9, na.rm = T),
             seuil_P3 = quantile(PP, probs = 0.75, na.rm = T),
             seuil_P4 = quantile(P4, probs = 0.6, na.rm = T),
             P_global = (3*P1+2*PP+1*P4)/sum(P1*3, PP*2, P4*1),
             sumP1 = P1 / sum(P1)) %>% 
      ungroup() %>% 
      mutate(emoji = as.character(emoji)) %>% 
      mutate(smile = case_when(
        emoji == "1" ~ "😊",
        emoji == "2" ~ "😐",
        emoji == "3" ~ "☹️",
        is.na(emoji) ~ "🤔",
        TRUE ~ emoji
      ),
      # ferrure = case_when(
      #   shoeing == "1" ~ "😊",
      #   shoeing == "2" ~ "😐",
      #   shoeing == "3" ~ "☹️",
      #   is.na(shoeing) ~ "🤔",
      #   TRUE ~ shoeing
      # ),
      # last_ferrure = case_when(
      #   last1_shoes == "1" ~ "😊",
      #   last1_shoes == "2" ~ "😐",
      #   last1_shoes == "3" ~ "☹️",
      #   is.na(last1_shoes) ~ "🤔",
      #   TRUE ~ last1_shoes
      # ),
      horseName = ifelse(PREP_D4 == 1, paste0(horseName, " ⭐"), horseName),
      horseName = ifelse(CLASS_INF == 1, paste0(horseName, " ⬇️"), horseName),
      last_comment = ifelse(is.na(last_comment), "", last_comment),
      formFigs = ifelse(is.na(formFigs), "", formFigs)
      )
    
    data <- mutate(data, horse_label = paste0(saddle, '-', horseName))
    data <- mutate(data, reunion_label = paste0(R_pmuNumber, ' - ', R_name))
    data <- mutate(data, course_label = paste0(C_number, ' - ', C_name))
    data$C_time <- sub("^(\\d{2}:\\d{2}).*$", "\\1", data$C_time )
    
    return(data)
  })
  
  output$hipp_id_graph <- renderUI({
    df <- data()
    hipp <- unique(df$reunion_label)
    selectInput('hipp_filter_id_graph', 'Réunion', choices = hipp)
  })
  
  output$course_filter_ui_graph <- renderUI({
    req(input$hipp_filter_id_graph)  # Ensure the input is not NULL
    df <- data()
    valid_courses <- unique(df$course_label[df$reunion_label == input$hipp_filter_id_graph])
    selectInput("course_filter_graph", "Course", choices = valid_courses)
  })
  
  filtered_data <- reactive({
    req(input$hipp_filter_id_graph, input$course_filter_graph)  # Ensure both inputs are available
    df <- data()
    df %>%
      filter(reunion_label == input$hipp_filter_id_graph, course_label == input$course_filter_graph)
  })
  
  
  output$heure <- renderText({
    
    filtered <- filtered_data()
    paste0('Heure de la course : ', unique(filtered$C_time))
  })
  
  
  # Fonction de rendu pour le tableau
  output$mytable <- render_gt({
    req(filtered_data())  # Ensure filtered data is available
    
    filtered <- filtered_data()
    if (nrow(filtered) == 0) {
      return(NULL)
    }
    
    infos1 = unique(filtered$C_caraList1)
    infos2 = unique(filtered$C_caraList2)
    
    filtered %>% 
      select(saddle, horseName, trainerName, jockeyName, 
             #totalPrize,
             driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp,
             #cote, 
             smile,
             shoeing, 
             last1_shoes,
             last2_shoes,
             last3_shoes,
             jour_last_course, mean_ratio_temps_last12_month,
             P_global,
             P1, PP, P4,
             #CLASS_INF, 
             sumP1,
             driver_ratio_topp_evol, trainer_ratio_topp_evol, last_comment,
             formFigs) %>%
      arrange(desc(P_global)) %>%  
      mutate(#.pred_win = formattable::percent(.pred_win),
        P1 = P1*100,
        PP = PP*100,
        P4 = P4*100,
        mean_ratio_temps_last12_month = digits(mean_ratio_temps_last12_month*100, 2),
        # label = paste0(horseName, ";", jockeyName, ";", trainerName),
        P_global = digits(P_global*100, 2),
        driver_ratio_topp = driver_ratio_topp*100,
        trainer_ratio_topp = trainer_ratio_topp*100,
        horse_ratio_topp = horse_ratio_topp*100) %>% 
        # CLASS_INF = ifelse(CLASS_INF == 0, "", "1")) %>% 
        # PREP_D4 = ifelse(PREP_D4 == 0, "", "1")) %>% 
      # select(-c(horseName, jockeyName, trainerName)) %>% 
      gt() %>%
      text_transform(
        locations = cells_body(columns = shoeing),
        fn = function(x) {
          vapply(x, generate_images, character(1))
        }
      ) %>%
      # text_transform(
      #   locations = cells_body(columns = last1_shoes),
      #   fn = function(x) {
      #     vapply(x, generate_images, character(1))
      #   }
      # ) %>%
      # text_transform(
      #   locations = cells_body(columns = last2_shoes),
      #   fn = function(x) {
      #     vapply(x, generate_images, character(1))
      #   }
      # ) %>%
      # text_transform(
      #   locations = cells_body(columns = last3_shoes),
      #   fn = function(x) {
      #     vapply(x, generate_images, character(1))
      #   }
      # ) %>%
      cols_merge(
        columns = c(last1_shoes, last2_shoes, last3_shoes),
        pattern = "{1};{2};{3}"
      ) %>%
      text_transform(
        locations = cells_body(columns = last1_shoes),
        fn = function(x) {
          vapply(x, function(cell) {
            image1 <- word(cell, 1, sep = ";")
            image2 <- word(cell, 2, sep = ";")
            image3 <- word(cell, 3, sep = ";")
            
            glue::glue(
              "<div style='margin-bottom:10px'>{generate_images(image1)}</div>
           <div style='margin-bottom:10px'>{generate_images(image2)}</div>
           <div>{generate_images(image3)}</div>"
            )
          }, character(1))
        }
      ) %>% 
      # Afficher les images sous la nouvelle colonne
      tab_header(
        title = md(infos1),
        subtitle = paste(infos2)
      ) %>% 
      cols_merge(
        columns = c(horseName, jockeyName, trainerName),
        pattern = "{1};{2};{3}"
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = c(horseName)
        ),
        fn = function(x) {
          horseName <- word(x, 1, sep = ";")
          jockeyName <- word(x, 2, sep = ";")
          trainerName <- word(x, 3, sep = ";")
          
          # Ajout d'une étoile si D4 == 1
          
          # Construction du contenu HTML avec le style
          glue::glue(
            "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{horseName}</span></div>
      <div><span style='font-weight:bold;color:grey;font-size:12px'>{jockeyName}</span></div>
      <div><span style='font-weight:bold;color:grey;font-size:10px'>{trainerName}</span></div>"
          )
        }
      ) %>% 
      # gt_theme_espn() %>% 
      cols_label(
        saddle = "N",
        P1 = 'Proba<br>Gagnant',
        PP = 'Proba<br>Placé',
        P4 = 'Proba<br>TOP4',
        # CLASS_INF = 'C.<br>inf',
        # PREP_D4 = 'Prep.<br>D4',
        horseName = 'Cheval',
        driver_ratio_topp = "Ratio<br>Jockey",
        trainer_ratio_topp = "Ratio<br>Entr.",
        horse_ratio_topp = "Ratio<br>Cheval",
        mean_ratio_temps_last12_month = 'Score (100)<br>1 an',
        P_global = 'Proba<br>Globale',
        jour_last_course = 'Repos',
        # formFigs = "musique",
        last_comment = "Commentaire<br>Dernière course",
        smile = "😊",
        formFigs = "musique",
        shoeing = "Ferrure",
        last1_shoes = "Ferrure<br>(old)",
        .fn = md) %>% 
      # fmt_currency(columns = cote, decimals = 1, currency = 'EUR', placement = 'right') %>% 
      # gt_color_rows(.pred_win, palette = "ggsci::blue_material", domain = c(0,1)) %>% 
      gt_color_rows(mean_ratio_temps_last12_month, palette = "ggsci::green_material", direction = 1) %>% 
      gt_color_rows(P_global, palette = "ggsci::blue_material", direction = 1) %>% 
      tab_style(
        style = cell_fill(color = "palegreen"),
        location = cells_body(
          columns = c(saddle, horseName),
          rows = sumP1 > 0.3 & P1 > 90
        )) %>% 
      gt_plt_bar_pct(
        column = driver_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        decimals = 2,
        label_cutoff = 0.1,
        fill = "#FFD700", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = trainer_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        label_cutoff = 0.1,
        fill = "#4682B4", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = horse_ratio_topp,
        scaled = TRUE,
        labels = TRUE,
        label_cutoff = 0.1,
        fill = "#8B4513", background = "lightblue"
      ) %>% 
      gt_plt_bar_pct(
        column = PP,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#2CA25F", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      gt_plt_bar_pct(
        column = P1,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#b8711a", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      gt_plt_bar_pct(
        column = P4,
        scaled = TRUE,
        labels = TRUE,
        # decimals = 3,
        label_cutoff = 0.1,
        fill = "#C667FC", background = "lightblue",
        font_size = '13px'
        # height = '17px'
      ) %>% 
      data_color(
        columns = "smile",
        colors = scales::col_factor(
          palette = c("#068a3d", "#bfb90d", "#962f26"),
          levels = c("😊", "😐", "☹️")
        )
      ) %>% 
      tab_footnote(
        footnote = "% d'arrivées placées lors des 12 derniers mois, et indicateur de ratio des 2 derniers mois",
        locations = cells_column_labels(
          columns = c(driver_ratio_topp, trainer_ratio_topp, horse_ratio_topp))
      ) %>% 
      tab_footnote(
        footnote = "Nombre de jours depuis la dernière course",
        locations = cells_column_labels(
          columns = c(jour_last_course))
      ) %>% 
      tab_footnote(
        footnote = "⭐ Ferrure les 3 dernières courses, et D4 aujourd'hui",
        locations = cells_column_labels(
          columns = c(horseName))
      ) %>% 
      tab_footnote(
        footnote = "⬇️ Course de catégorie inférieure à la précédente",
        locations = cells_column_labels(
          columns = c(horseName))
      ) %>% 
      tab_style(
        style = list(
          # cell_fill(color = "#F9E3D6"),
          cell_text(style = "oblique")
        ),
        locations = cells_body(
          columns = horseName,
        )
      ) %>% 
      tab_style(
        style = list(
          # cell_fill(color = "#F9E3D6"),
          cell_text(size = px(12))
        ),
        locations = cells_body(
          columns = c(mean_ratio_temps_last12_month, P_global)
        )
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = driver_ratio_topp,
          rows = driver_ratio_topp_evol >= 0
        ),
        fn = function(x) paste(x, up_arrow)
      ) %>%
      text_transform(
        locations = cells_body(
          columns = driver_ratio_topp,
          rows = driver_ratio_topp_evol < 0
        ),
        fn = function(x) paste(x, down_arrow)
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = trainer_ratio_topp,
          rows = trainer_ratio_topp_evol >= 0
        ),
        fn = function(x) paste(x, up_arrow)
      ) %>%
      text_transform(
        locations = cells_body(
          columns = trainer_ratio_topp,
          rows = trainer_ratio_topp_evol < 0
        ),
        fn = function(x) paste(x, down_arrow)
      ) %>% 
      text_transform(
        locations = cells_body(
          columns = c(last_comment, formFigs)
        ),
        fn = function(x) {
          glue::glue(
            "<div style='font-size:10px'>{x}</div>"
          )
        }
      ) %>%
      cols_width(
        saddle ~ px(50),
        P1 ~ px(70),
        PP ~ px(80),
        P4 ~ px(70),
        last_comment ~ px(200),
        smile ~ px(30),
        formFigs ~ px(200),
        horseName ~ px(100),
        driver_ratio_topp ~ px(70),
        trainer_ratio_topp ~ px(70),
        horse_ratio_topp ~ px(60),
        mean_ratio_temps_last12_month ~ px(60),
        P_global ~ px(60),
        # mean_ratio_temps_last12_month, mean_ratio_temps_last12_month ~ px(60),
        # mean_ratio_temps_last12_month, mean_ratio_temps_last12_month~ px(60),
        jour_last_course~ px(60),
        everything() ~ px(60)) %>% 
      cols_hide(c(driver_ratio_topp_evol,
                  trainer_ratio_topp_evol,
                  sumP1)) 
    
  })
  
  # Render the boxes with the specific information
  output$sg_box <- renderUI({
    df <- filtered_data()
    sg <- df %>%
      arrange(-P1) %>%
      filter(P1 > unique(df$seuil_P1))
    
    box(
      title = "SG",
      status = "primary",
      solidHeader = TRUE,
      paste0("SG : ", paste0(sg$saddle, collapse = "-"))
    )
  })
  
  output$sp_box <- renderUI({
    df <- filtered_data()
    sp <- df %>%
      arrange(-PP) %>%
      filter(PP > unique(df$seuil_P3))
    
    box(
      title = "SP",
      status = "info",
      solidHeader = TRUE,
      paste0("SP : ", paste0(sp$saddle, collapse = "-"))
    )
  })
  
  output$multi_box <- renderUI({
    df <- filtered_data()
    multi <- df %>%
      arrange(-P4) %>%
      filter(P4 > unique(df$seuil_P4))
    
    box(
      title = "Multi",
      status = "success",
      solidHeader = TRUE,
      paste0("Multi : ", paste0(multi$saddle, collapse = "-"))
    )
  })
 
  
}

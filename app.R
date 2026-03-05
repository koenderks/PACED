library(shiny)
library(readxl)
library(ggplot2)
library(psych)
library(DT)
library(bslib)
library(shinydisconnect)
library(shinycssloaders)
library(shinyjs)
library(knitr)
library(kableExtra)
library(htmltools)
library(base64enc)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

build_report_html <- function(
    name,
    examiner,
    descriptives,
    hist_plot,
    test_stats,
    item_stats,
    item_plot,
    corr_plot
) {
  
  # ----- helper: embed plot in HTML -----
  embed_plot <- function(plot, width, height) {
    f <- tempfile(fileext = ".png")
    ggsave(f, plot, width = width, height = height, dpi = 300)
    uri <- base64enc::dataURI(file = f, mime = "image/png")
    tags$img(src = uri, style = "display:block; margin:20px auto; max-width:100%;")
  }
  
  # ----- Derived interpretations -----
  participants <- descriptives$Value[descriptives$Statistic == "Number of participants"]
  avg_score <- descriptives$Value[descriptives$Statistic == "Average achieved score"]
  median_score <- descriptives$Value[descriptives$Statistic == "Median achieved score"]
  sd_score <- descriptives$Value[descriptives$Statistic == "Standard deviation"]
  skew <- as.numeric(descriptives$Value[descriptives$Statistic == "Skewness"])
  sd_text <- if (as.numeric(sd_score) > 10) "considerable variability" else "relatively consistent performance"
  skew_text <- if (skew > 0.5) "positively skewed, with a tendency toward lower scores" else if (skew < -0.5) "negatively skewed, with a tendency toward higher scores" else "approximately symmetric"
  difficulty_range <- if (mean(item_stats$P) > 0.7) "upper" else "middle/lower"
  
  # ----- Tables with coloring -----
  
  # Descriptives
  desc_tab <- knitr::kable(descriptives, row.names = FALSE, format = "html", table.attr = 'class="left_table"') %>%
    kable_styling(full_width = FALSE, position = "center", bootstrap_options = "striped")
  
  # Test stats
  test_tab <- test_stats
  test_tab$`Average P` <- cell_spec(test_tab$`Average P`,"html", color = ifelse(test_tab$`Average P` < 0.2,"tomato",ifelse(test_tab$`Average P` <=0.8,"forestgreen","tomato")))
  test_tab$`Average RIT` <- cell_spec(test_tab$`Average RIT`,"html", color = ifelse(test_tab$`Average RIT` < 0.2,"tomato",ifelse(test_tab$`Average RIT` <=0.3,"orange","forestgreen")))
  test_tab$`Average RIR` <- cell_spec(test_tab$`Average RIR`,"html", color = ifelse(test_tab$`Average RIR` < 0.2,"tomato",ifelse(test_tab$`Average RIR` <=0.3,"orange","forestgreen")))
  test_tab$`Cronbach's alpha` <- cell_spec(test_tab$`Cronbach's alpha`,"html", color = ifelse(test_tab$`Cronbach's alpha` < 0.7,"tomato","forestgreen"))
  test_tab <- knitr::kable(test_tab, escape = FALSE, row.names = FALSE, format = "html", table.attr = 'class="center_table"') %>%
    kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped","hover"))
  
  # Item stats
  alpha_test <- test_stats$`Cronbach's alpha`[1]
  item_tab <- item_stats
  item_tab$P <- cell_spec(item_tab$P,"html", color = ifelse(item_tab$P < 0.2,"tomato",ifelse(item_tab$P <=0.8,"forestgreen","tomato")))
  item_tab$RIT <- cell_spec(item_tab$RIT,"html", color = ifelse(item_tab$RIT < 0.2,"tomato",ifelse(item_tab$RIT <=0.3,"orange","forestgreen")))
  item_tab$RIR <- cell_spec(item_tab$RIR,"html", color = ifelse(item_tab$RIR < 0.2,"tomato",ifelse(item_tab$RIR <=0.3,"orange","forestgreen")))
  item_tab$`Alpha-if-deleted` <- cell_spec(item_tab$`Alpha-if-deleted`,"html", color = ifelse(item_tab$`Alpha-if-deleted` < alpha_test,"forestgreen","tomato"))
  item_tab <- knitr::kable(item_tab, escape = FALSE, row.names = FALSE, format = "html", table.attr = 'class="center_table"') %>%
    kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped","hover"))
  
  # ----- Build HTML -----
  tagList(
    tags$html(
      tags$head(
        tags$title("Assessment Report"),
        tags$style(HTML("
          /* Remove extra body margin and center content */
          body {
            margin: 0;
            padding: 0;
            font-family: Arial, Helvetica, sans-serif;
            background: #FFFFFF;
          }

          /* Container for centering content at 100% width */
          #report_container {
            width: 90%;
            margin: 0 auto;
            padding: 20px 0;
            text-align: justify;
          }

          h1 { color: #00205B; text-align: left; margin-bottom: 20px; }
          h2, h3 { margin-top: 30px; margin-bottom: 15px; }

          table {
            border-collapse: collapse;
            width: 100%;
          }
          
          /* First table: left-aligned */
          .left_table th {
            text-align: left !important;
            vertical-align: middle !important;
            color: #00205B !important;  /* header color */
            background: #f4f6fb; /* keep light background */
            padding: 6px;
          }
          
          /* Other tables: centered */
          .center_table th {
            text-align: center !important;
            vertical-align: middle !important;
            color: #00205B !important;  /* header color */
            background: #f4f6fb; /* keep light background */
            padding: 6px;
          }
          
          .left_table td {
            text-align: left !important;
            vertical-align: middle !important;
            border: 0.5px solid #ddd;
            padding: 6px;
          }
          
          .center_table td {
            text-align: center !important;   /* center all cell contents */
            vertical-align: middle !important;
            border: 0.5px solid #ddd;
            padding: 6px;
          }

          img { max-width: 100%; display: block; margin: 10px auto; }
          .logo { text-align:center; margin-bottom:30px; }
        "))
      ),
      tags$body(
        tags$div(
          id = "report_container",
          tags$div(class="logo",
                   tags$img(src="https://raw.githubusercontent.com/koenderks/CirrusAssessmentAnalysis/refs/heads/main/logo.png", height="80px")
          ),
          h1(sprintf("Assessment Report: %s", name)),
          p(sprintf("Report generated on %s by %s", format(Sys.time(), "%d-%m-%Y"), examiner)),
          tags$hr(),
          
          # 1 Summary
          h2("1. Summary"),
          h3("1.1 Descriptive statistics"),
          p(sprintf("This assessment tested %s participants. They achieved an average score of %s, with a median score of %s. The standard deviation was %s, indicating %s among participants. The skewness of the score distribution is %s, suggesting the distribution is %s.",
                    participants, avg_score, median_score, sd_score, sd_text, skew, skew_text)),
          HTML(desc_tab),
          
          h3("1.2 Distribution of Achieved Scores"),
          p(sprintf("The histogram shows the distribution of achieved scores. In this case, it shows that most students achieved scores in the %s range. Any peaks at extreme ends suggest possible ceiling or floor effects that may affect discrimination between students.", difficulty_range)),
          embed_plot(hist_plot, 7, 4),
          
          # 2 Classical Assessment Analysis
          h2("2. Classical Assessment Analysis"),
          
          # 2.1 Assessment Stats
          h3("2.1 Assessments Statistics"),
          p(HTML("This table displays the key metrics for each overall assessment. The cells are colored according to the values prescribed in the <i>Guideline Assessment Analysis</i>.")),
          tags$ul(
            tags$li(HTML("<b>Average P (Difficulty)</b>: Values near 0 indicate very difficult items, values near 1 indicate very easy items. Ideally, items are moderately difficult (0.3–0.8, green) to provide effective discrimination.")),
            tags$li(HTML("<b>Average RIT and RIR (Discrimination)</b>: These values measure how well the assessment between higher and lower scoring participants. Values below 0.2 (red) suggest poor discrimination; 0.2–0.3 (orange) indicate average discrimination; values above 0.3 (green) indicate good discrimination.")),
            tags$li(HTML("<b>Cronbach's alpha (Internal Consistency)</b>: Values above 0.7 (green) indicate reliable measurement of the intended construct. Lower values suggest inconsistent items or that some items may not contribute effectively to overall reliability."))
          ),
          HTML(test_tab),
          
          # 2.2 Item Stats
          h3("2.2 Item Statistics"),
          p(HTML("This table summarizes the key metrics for each individual item. The cells are colored according to the values prescribed in the <i>Guideline Assessment Analysis</i>.")),
          tags$ul(
            tags$li(HTML("<b>P (Item Difficulty)</b>: Values near 0 indicate very difficult items, near 1 indicate very easy items. Red = too hard/easy, Green = ideal difficulty.")),
            tags$li(HTML("<b>RIT (Item-Total Correlation)</b>: Measures correlation with total score. Red = low (<0.2), Orange = average (0.2–0.3), Green = strong (>0.3).")),
            tags$li(HTML("<b>RIR (Item-Rest Correlation)</b>: Correlation with rest of assessment. Coloring follows RIT logic.")),
            tags$li(HTML("<b>Alpha-if-deleted</b>: Shows impact on Cronbach's alpha if item removed. Red = improves reliability, Green = reduces reliability."))
          ),
          HTML(item_tab),
          
          # 2.3 Item Difficulty & Discrimination
          h3("2.3 Item Difficulty & Discrimination"),
          p("The figure below plots item difficulty (P-values) against item discrimination (RIT). Items that are difficult and poorly discriminating may need revision, while easy items with high discrimination typically contribute positively to the assessment."),
          embed_plot(item_plot, 9, 5),
          
          # 2.4 Item Correlation Matrix
          h3("2.4 Item Correlation Matrix"),
          p("The correlation matrix highlights relationships between items. Strong positive correlations (> 0.6) may indicate redundancy, while very low or negative correlations may suggest misalignment or potential errors. Items with unusual correlations should be reviewed to improve assessment quality."),
          embed_plot(corr_plot, 11, 11)
        )
      )
    )
  )
}

# ---------------------------
# Brand colors & ggplot theme
# ---------------------------
nyenrode_blue  <- "#00205B"
nyenrode_gold  <- "#C99300"
nyenrode_red   <- "#BD3231"
nyenrode_blue2 <- "#0054A6"

theme_nyenrode <- function() {
  theme_classic() +
    theme(
      plot.background  = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      axis.text        = element_text(color = "black", size = 10),
      axis.title       = element_text(color = "black", size = 15),
      legend.title     = element_text(color = "black", size = 20),
      legend.text      = element_text(color = "black", size = 20),
      axis.line        = element_blank()
    )
}

# ---------------------------
# UI helpers
# ---------------------------
section_card <- function(title, ..., subtitle = NULL, full_width = TRUE) {
  bslib::card(
    class = "app-card",
    bslib::card_header(
      tags$div(
        class = "card-title-row",
        tags$span(class = "card-dot"),
        tags$span(class = "card-title", title),
        if (!is.null(subtitle)) tags$span(class = "card-subtitle", subtitle)
      )
    ),
    bslib::card_body(...),
    full_screen = FALSE
  )
}

# ---------------------------
# Content builders
# ---------------------------
create_descriptives_table <- function(input, parsed) {
  # If no file uploaded yet -> empty table
  if (is.null(input$file)) {
    tab <- data.frame(
      n = NA, maxscore = NA, min = NA, max = NA,
      mean = NA, median = NA, sd = NA, skewness = NA, kurtosis = NA
    )
  } else {
    req(parsed())
    d <- parsed()$data
    totalScores <- rowSums(d)
    digits <- parsed()$digits
    maxScore <- parsed()$maxScore
    
    tab <- data.frame(
      n = nrow(d),
      maxscore = paste0(parsed()$maxScore, " (100%)"),
      min = paste0(round(min(totalScores), digits), " (", paste0(round(min(totalScores) / maxScore * 100, digits), "%"), ")"),
      max = paste0(round(max(totalScores), digits), " (", paste0(round(max(totalScores) / maxScore * 100, digits), "%"), ")"),
      mean = paste0(round(mean(totalScores), digits), " (", paste0(round(mean(totalScores) / maxScore * 100, digits), "%"), ")"),
      median = paste0(round(median(totalScores), digits), " (", paste0(round(median(totalScores) / maxScore * 100, digits), "%"), ")"),
      sd = round(sd(totalScores), digits),
      skewness = round(psych::describe(totalScores)$skew, digits),
      kurtosis = round(psych::describe(totalScores)$kurtosis, digits)
    )
  }
  
  colnames(tab) <- c(
    "Number of participants", "Maximum possible score", "Minimum achieved score",
    "Maximum achieved score", "Average achieved score", "Median achieved score",
    "Standard deviation", "Skewness", "Kurtosis"
  )
  data.frame("Statistic" = colnames(tab), "Value" = t(tab), check.names = FALSE)
}

create_histogram <- function(input, parsed) {
  if (is.null(input$file)) {
    p <- ggplot(data.frame(x = 1), aes(x = x)) +
      geom_histogram() +
      scale_x_continuous(name = "Achieved score", limits = c(0, 1)) +
      scale_y_continuous(name = "Frequency", limits = c(0, 1)) +
      geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      theme(
        axis.text = element_blank()
      )
  } else {
    req(parsed())
    d <- parsed()$data
    totalScores <- rowSums(d)
    maxScore <- parsed()$maxScore
    xBreaks <- pretty(c(0, maxScore), min.n = 4)
    h <- hist(c(0, totalScores, maxScore), breaks = 30, plot = FALSE)
    yBreaks <- pretty(c(0, h$counts * 1.5), min.n = 4)
    p <- ggplot(data.frame(x = totalScores), aes(x = x)) +
      geom_histogram(bins = 30, color = "black", fill = "lightgray") +
      scale_x_continuous(name = "Achieved score", limits = c(-0.5, max(xBreaks) + 0.5), breaks = xBreaks) +
      scale_y_continuous(name = "Frequency", limits = c(0, max(yBreaks)), breaks = yBreaks) +
      geom_segment(y = -Inf, yend = -Inf, x = 0, xend = max(xBreaks)) +
      geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
      geom_segment(x = maxScore, xend = maxScore, y = 0, yend = max(yBreaks), linetype = "dashed", color = "firebrick") +
      annotate(geom = "text", x = maxScore, y = max(yBreaks), label = "Max. score", hjust = 1.2, size = 5, color = "firebrick") +
      theme_nyenrode()
  }
  return(p)
}

create_test_stats <- function(input, parsed) {
  if (is.null(input$file)) {
    tab <- data.frame(P = NA, RIT = NA, RIR = NA, alpha = NA)
  } else {
    req(parsed())
    d <- parsed()$data
    qm <- parsed()$maxPoints
    digits <- parsed()$digits
    # Item metrics
    P <- colMeans(d) / qm
    RIT <- sapply(d, function(x) cor(x, rowSums(d), use = "pairwise.complete.obs"))
    RIR <- sapply(seq_along(d), function(j) {
      x <- d[[j]]
      cor(x, rowSums(d) - x, use = "pairwise.complete.obs")
    })
    # Cronbach alpha
    alpha <- psych::alpha(d)$total$raw_alpha
    tab <- round(data.frame(P = mean(P), RIT = mean(RIT), RIR = mean(RIR), alpha = alpha), digits)
  }
  colnames(tab) <- c("Average P", "Average RIT", "Average RIR", "Cronbach's alpha")
  return(tab)
}

create_item_stats <- function(input, parsed) {
  if (is.null(input$file)) {
    tab <- data.frame(item = NA, mean = NA, sd = NA, P = NA, RIT = NA, RIR = NA, alpha_when_dropped = NA)
  } else {
    req(parsed())
    d <- parsed()$data
    qm <- parsed()$maxPoints
    digits <- parsed()$digits
    total_cronbach_alpha <- function(data) {
      data <- na.omit(data)
      k <- ncol(data)
      if (k < 2) return(NA)
      item_vars <- apply(data, 2, var)
      total_var <- var(rowSums(data))
      if (isTRUE(all.equal(total_var, 0))) return(NA)
      (k / (k - 1)) * (1 - sum(item_vars) / total_var)
    }
    item_total_cor <- function(data) {
      tot <- rowSums(data)
      sapply(data, function(x) cor(x, tot, use = "pairwise.complete.obs"))
    }
    item_rest_cor <- function(data) {
      tot <- rowSums(data)
      sapply(seq_along(data), function(j) {
        x <- data[[j]]
        cor(x, tot - x, use = "pairwise.complete.obs")
      })
    }
    RIT <- item_total_cor(d)
    RIR <- item_rest_cor(d)
    alpha_drop <- sapply(seq_along(d), function(j) {
      total_cronbach_alpha(d[, -j, drop = FALSE])
    })
    tab <- data.frame(
      item = colnames(d),
      mean = round(colMeans(d), digits),
      sd = round(apply(d, 2, sd), digits),
      P = round(colMeans(d) / qm, digits),
      RIT = round(RIT, digits),
      RIR = round(RIR, digits),
      alpha_when_dropped = round(alpha_drop, digits)
    )
  }
  colnames(tab) <- c("Item (Cirrus ID)", "Mean", "SD", "P", "RIT", "RIR", "Alpha-if-deleted")
  return(tab)
}

create_item_plot <- function(input, parsed) {
  if (is.null(input$file)) {
    p <- ggplot(data.frame(x = 1), aes(x = x)) +
      geom_histogram() +
      scale_x_continuous(name = "Item (Cirrus ID)", limits = c(0, 1)) +
      scale_y_continuous(name = NULL, limits = c(0, 1)) +
      geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      theme(
        axis.text = element_blank()
      )
  } else {
    req(parsed())
    d <- parsed()$data
    qm <- parsed()$maxPoints
    digits <- parsed()$digits
    RIT <- sapply(d, function(x) cor(x, rowSums(d), use = "pairwise.complete.obs"))
    Pval <- colMeans(d) / qm
    df <- data.frame(item = factor(names(Pval), levels = names(Pval)[order(Pval)]), P = Pval, RIT = RIT)
    df_long <- reshape(df, varying = list(c("P", "RIT")), v.names = "value", timevar = "metric", times = c("P", "RIT"), direction = "long")
    yBreaks <- pretty(c(0, 1, df_long$value), min.n = 4)
    p <- ggplot(df_long, aes(x = item, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      scale_fill_manual(name = NULL, values = c(nyenrode_gold, nyenrode_blue2), labels = c("P (Difficulty)", "RIT (Discrimination)")) +
      scale_y_continuous(name = NULL, limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks) +
      scale_x_discrete(name = "Item (Cirrus ID)") +
      geom_segment(x = -Inf, xend = -Inf, y = min(yBreaks), yend = max(yBreaks)) +
      geom_segment(y = -Inf, yend = -Inf, x = 1, xend = ncol(d)) +
      theme_nyenrode() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top")
  }
  return(p)
}

create_corr_plot <- function(input, parsed) {
  if (is.null(input$file)) {
    p <- ggplot(data.frame(x = 1), aes(x = x)) +
      geom_histogram() +
      scale_x_continuous(name = "Item (Cirrus ID)", limits = c(0, 1)) +
      scale_y_continuous(name = "Item (Cirrus ID)", limits = c(0, 1)) +
      geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      theme(axis.text = element_blank())
  } else {
    req(parsed())
    d <- parsed()$data
    cor_mat <- cor(d, use = "pairwise.complete.obs")
    diag(cor_mat) <- NA
    cor_df <- as.data.frame(as.table(cor_mat))
    colnames(cor_df) <- c("Var1", "Var2", "Correlation")
    xBreaks <- unique(cor_df$Var1)
    yBreaks <- unique(cor_df$Var2)
    col_breaks <- pretty(c(-1, 1), min.n = 5)
    p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) + 
      geom_tile(color = "black") +
      scale_fill_gradient2(name = NULL, low = "firebrick", mid = "white", high = "forestgreen", na.value = "black", midpoint = 0, limits = c(-1, 1), breaks = col_breaks) + 
      scale_x_discrete(name = "Item (Cirrus ID)", breaks = xBreaks) +
      scale_y_discrete(name = "Item (Cirrus ID)", breaks = yBreaks) + 
      geom_segment(x = -Inf, xend = -Inf, y = 1, yend = length(yBreaks)) +
      geom_segment(y = -Inf, yend = -Inf, x = 1, xend = length(xBreaks)) +
      theme_nyenrode() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            legend.key.width  = unit(4, "cm"))
  }
  return(p)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Session disconnect overlay
  disconnectMessage(
    text = "Your session has expired.",
    refresh = "Reload",
    background = "#646464e6",
    size = 36, width = "full", top = "center",
    colour = "white", refreshColour = nyenrode_blue,
    overlayColour = "#999", overlayOpacity = 0.4
  ),
  
  # --- Visual theme (Bootstrap 5) ---
  theme = bs_theme(
    version  = 5,
    bg       = "#FFFFFF",
    fg       = "#1A1A1A",
    primary  = nyenrode_blue,
    secondary= nyenrode_gold,
    info     = "#0054A6",
    warning  = nyenrode_gold,
    danger   = nyenrode_red,
    # base_font    = font_google("Open Sans"),
    # heading_font = font_google("Montserrat"),
    base_font    = "Arial, Helvetica, sans-serif",
    heading_font = "Arial, Helvetica, sans-serif",
    "font-size-base" = "1rem"
  ) |>
    bs_add_rules(rules = sprintf("
    .app-title-bar {
      background: linear-gradient(90deg, %1$s 0%%, #0b2c7d 100%%);
      color: #fff; padding: 14px 18px; margin-bottom: 18px;
      box-shadow: 0 2px 6px rgba(0,0,0,.08);
    }
    .app-title { font-weight: 700; letter-spacing: .3px; }
    .app-subtitle { font-weight: 400; opacity: .85; font-size: .95rem; }

    .sidebar { background-color: #f7f9fc; border-right: 1px solid #e6e9ef; }
    .sidebar h2 { color: %1$s; font-size: 1.25rem; margin-top: 0; }
    .sidebar .btn, .sidebar .form-control { border-radius: .4rem; }
    .sidebar .btn-primary { background-color: %1$s; border-color: %1$s; }
    .sidebar .btn-primary:hover { filter: brightness(1.05); }

    .app-card .card-header {
      background: #ffffff;
      border-bottom: 1px solid #e6e9ef;
      padding: 12px 16px;
    }
    .card-title-row { display: flex; align-items: center; gap: 10px; }
    .card-title { font-weight: 700; color: %1$s; }
    .card-subtitle { color: #6c757d; font-weight: 500; }

    /* DataTables polish */
    table.dataTable thead th { background: #f4f6fb; color: #24324a; font-weight: 700; }
    table.dataTable tbody tr:nth-child(odd) { background-color: #fbfcff; }
    table.dataTable tbody tr:hover { background-color: #f5f8ff; }

    /* Spacing utilities */
    .mt-2 { margin-top: .5rem; } .mt-3 { margin-top: 1rem; } .mb-3 { margin-bottom: 1rem; }
  ", nyenrode_blue, nyenrode_gold)),
  
  # --- Title bar ---
  tags$div(class = "app-title-bar",
           tags$div(class = "app-title", "Nyenrode Advanced Assessment Analysis"),
           tags$div(class = "app-subtitle", "Gaining insight into the quality of your summative assessment")
  ),
  
  # --- Layout with persistent sidebar ---
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h2("Instructions"),
      tags$ol(
        tags$li("Go to Cirrus → Reports"),
        tags$li('Select the test and export "Candidate scores - with criterum scores"'),
        tags$li("Remove identifying information (e.g., name, email)"),
        tags$li("Upload the exported file below and check overview"),
        tags$li('Review results in tabs and click "Download Report" for interpretations')
      ),
      fileInput("file", "Upload candidate scores (.xlsx)", accept = ".xlsx"),
      section_card(
        "Overview",
        uiOutput("dataset_info")
      ),
      textInput("name", label = "Assessment", placeholder = "e.g., Management - Final Exam"),
      textInput("name_examiner", label = "Examiner", placeholder = "e.g., Eric Xaminer"),
      div(
        class = "mt-3",
        actionButton("refresh", "Refresh", class = "btn btn-outline-secondary"),
        tags$span(" "),
        downloadButton(outputId = "export", label = "Download Report", class = "btn btn-primary")
      ),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          title = "1. Summary",
          section_card(
            "1.1 Descriptive Statistics",
            p("These descriptive statistics concern the achieved scores."),
            DT::dataTableOutput("descriptives")
          ),
          section_card(
            "1.2 Distribution of Achieved Scores",
            div(style = "width: 100%; margin: 0 auto;", withSpinner(plotOutput("histogram", width = "100%"), type = 4, color = nyenrode_blue))
          )
        ),
        
        tabPanel(
          title = "2. Classical Assessment Analysis",
          section_card(
            "2.1 Assessment Statistics",
            p("These statistics concern the overall assessment."),
            div(
              style = "padding: 0;",
              DT::dataTableOutput("test_stats")
            )
          ),
          section_card(
            "2.2 Item Statistics",
            p("These statistics concern the individual items in the assessment."),
            DT::dataTableOutput("item_stats")
          ),
          section_card(
            "2.3 Item Difficulty & Discrimination",
            div(style = "width: 100%; margin: 0 auto;",
                withSpinner(plotOutput("item_plot", width = "100%"), type = 4, color = nyenrode_blue))
          ),
          section_card(
            "2.4 Item Correlation Matrix",
            div(style = "width: 100%; margin: 0 auto;",
                withSpinner(plotOutput("corr_plot", width = "100%", height = "1000px"), type = 4, color = nyenrode_blue)
            )
          )
        )
      )
    )
  ),
  tags$footer(
    class = "app-footer",
    "© Nyenrode Business Universiteit"
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  observeEvent(input$refresh, { 
    session$reload()
  })
  
  # Data import
  rawData <- reactive({
    req(input$file)
    read_excel(path = input$file$datapath, col_names = FALSE, na = c("N/A", "n.b."))
  })
  
  # Parse & clean to your spec
  parsed <- reactive({
    req(rawData())
    
    validate(
      need(nrow(rawData()) > 4, "File does not appear to be a valid Cirrus export.")
    )
    
    dataset <- rawData()
    
    digits <- 3
    
    # Filter only the scores
    index_first_question <- which(!(dataset[1, ] %in% c("Vraag", "Question", NA)))
    dataset <- dataset[, -c(1:(index_first_question - 1))]
    index_after_last_question <- which(dataset[4, ] == "Score")
    dataset <- dataset[, -c(index_after_last_question:ncol(dataset))]
    
    questionNames <- as.character(dataset[1, ])
    questionNames <- sub(" .*", "", questionNames)
    questionMaxPoints <- as.numeric(dataset[2, ])
    maxScore <- sum(questionMaxPoints)
    
    dataset <- dataset[-c(1:5), ]
    dataset <- dataset[complete.cases(dataset), ]
    colnames(dataset) <- questionNames
    dataset <- as.data.frame(apply(dataset, 2, as.numeric))
    
    list(
      data = dataset,
      maxPoints = questionMaxPoints,
      maxScore = maxScore,
      questionNames = questionNames,
      digits = digits,
      test_name = input$name
    )
  })
  parsed <- bindCache(parsed, input$file, cache = "session")
  
  output$dataset_info <- renderUI({
    
    participants <- items <- maxscore <- "..."
    
    if (!is.null(input$file)) {
      req(parsed())
      d <- parsed()$data
      participants <- nrow(d)
      items <- ncol(d)
      maxscore <- parsed()$maxScore
    }
    
    div(
      style = "line-height:1.2;",
      div(tags$strong("Participants: "), participants),
      div(tags$strong("Items: "), items),
      div(tags$strong("Maximum possible score: "), maxscore)
    )
  })
  
  # Descriptives (reactive + UI)
  descriptives_react <- reactive(create_descriptives_table(input, parsed))
  output$descriptives <- DT::renderDataTable({
    datatable(
      descriptives_react(),
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, pageLength = 20),
      class = "stripe hover order-column compact row-border"
    )
  })
  
  # Histogram
  histogram_react <- reactive(create_histogram(input, parsed))
  output$histogram <- renderPlot(histogram_react())
  
  # Test stats
  test_stats_react <- reactive(create_test_stats(input, parsed))
  output$test_stats <- DT::renderDataTable({
    datatable(
      test_stats_react(),
      rownames = FALSE,
      options = list(
        pagelength = 1,
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE
      ),
      class = "compact"
    ) %>%
      formatStyle(
        "Average P",
        color = styleInterval(
          c(0.2, 0.8),       # breakpoints
          c("tomato", "forestgreen", "tomato")  # colors: too hard / ideal / too easy
        )
      ) %>%
      formatStyle(
        "Average RIT",
        color = styleInterval(
          c(0.2, 0.3),                     # low discrimination threshold
          c("tomato", "orange", "forestgreen")           # red if low, else no color
        )
      ) %>%
      formatStyle(
        "Average RIR",
        color = styleInterval(
          c(0.2, 0.3),                     # low discrimination threshold
          c("tomato", "orange", "forestgreen")           # red if low, else no color
        )
      ) %>%
      formatStyle(
        "Cronbach's alpha",
        color = styleInterval(
          c(0.7),
          c("tomato", "forestgreen")           # red if low, else no color
        )
      )
  })
  
  # Item stats
  item_stats_react <- reactive(create_item_stats(input, parsed))
  output$item_stats <- DT::renderDataTable({
    datatable(
      item_stats_react(),
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE),
      class = "stripe hover order-column compact row-border"
    ) %>%
      formatStyle(
        "P",
        color = styleInterval(
          c(0.2, 0.8),       # breakpoints
          c("tomato", "forestgreen", "tomato")  # colors: too hard / ideal / too easy
        )
      ) %>%
      formatStyle(
        "RIT",
        color = styleInterval(
          c(0.2, 0.3),                     # low discrimination threshold
          c("tomato", "orange", "forestgreen")           # red if low, else no color
        )
      ) %>%
      formatStyle(
        "RIR",
        color = styleInterval(
          c(0.2, 0.3),                     # low discrimination threshold
          c("tomato", "orange", "forestgreen")           # red if low, else no color
        )
      ) %>%
      formatStyle(
        "Alpha-if-deleted",
        color = styleInterval(
          c(test_stats_react()[1, 4]),
          c("forestgreen", "tomato")           # red if low, else no color
        )
      )
  })
  
  # Item plot
  item_plot_react <- reactive(create_item_plot(input, parsed))
  output$item_plot <- renderPlot(item_plot_react())
  
  # Correlation heatmap
  corr_plot_react <- reactive(create_corr_plot(input, parsed))
  output$corr_plot <- renderPlot(corr_plot_react())
  
  # Export (HTML report via R Markdown template)
  output$export <- downloadHandler(
    filename = function() paste0("assessment_report_", input$name, ".html"),
    content = function(file) {
      withProgress(message = "Generating report...", value = 0, {
        
        # Step 1: Copy the Rmd template
        incProgress(0.3, detail = "Copying template...")
        
        report <- build_report_html(
          input$name,
          input$name_examiner,
          descriptives_react(),
          histogram_react(),
          test_stats_react(),
          item_stats_react(),
          item_plot_react(),
          corr_plot_react()
        )
        
        # Step 2: Render HTML
        incProgress(0.3, detail = "Rendering HTML...")
        
        htmltools::save_html(report, file)
        
        # Step 4: Finish
        incProgress(1, detail = "Done!")
      })
    }
  )
}

shinyApp(ui, server)

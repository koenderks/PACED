# =========================================================
# PACED (localized EN <-> NL) - Single-file Shiny app
# =========================================================
library(shiny)

# ------------------------------------
# Workaround for Chromium Issue 468227
# ------------------------------------
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# ----------------------------
# Assessment policy guidelines
# ----------------------------
guidelines <- list(
  P = c(0.2, 0.8), # < 0.2 = too difficult, > 0.2 & < 0.8 = ideal, > 0.8 = too easy
  RIT = c(0.2, 0.3), # < 0.2 = poor, > 0.2 & < 0.3 = average, > 0.3 = good
  RIR = c(0.2, 0.3), # < 0.2 = poor, > 0.2 & < 0.3 = average, > 0.3 = good
  alpha = 0.7 # > 0.7 = acceptable
)

# ---------------------------
# Brand colors & ggplot theme
# ---------------------------
nyenrode_blue <- "#00205B"
nyenrode_gold <- "#C99300"
nyenrode_red <- "#BD3231"
nyenrode_blue2 <- "#0054A6"

theme_nyenrode <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = NA, color = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      axis.text        = ggplot2::element_text(color = "black", size = 10),
      axis.title       = ggplot2::element_text(color = "black", size = 15),
      legend.title     = ggplot2::element_text(color = "black", size = 20),
      legend.text      = ggplot2::element_text(color = "black", size = 20),
      axis.line        = ggplot2::element_blank()
    )
}

# ---------------------------
# i18n helpers (EN <-> NL)
# ---------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

i18n <- list(
  en = list(
    # App chrome
    app_title = "Psychometric Analysis of Cirrus Exported Data (PACED)",
    disconnect_text = "Your session has expired.",
    disconnect_refresh = "Reload",

    # Sidebar instructions
    instructions_title = "Instructions",
    step1 = "In Cirrus, navigate to the Marking tab or the Reports tab",
    step2_html = 'Select the test and export "Candidate scores - with criterum scores" (click <a href="example.xlsx" target="_blank"><u>here</u></a> for an example file)',
    step3 = "Upload the file below and check the overview",
    step4 = 'Click "Download Report" for a HTML-file with the results and their interpretation',
    step5_html = 'Open the HTML-file, right-click, select "Print Page" and choose PDF (click <a href="example.pdf" target="_blank"><u>here</u></a> for an example report)',
    upload_label_html = "<b>Upload candidate scores (.xlsx)</b>",
    browse_label_html = "<b>Browse...</b>",
    no_file = "No file selected",
    quick_overview = "IMPORTANT: CHECK THIS OVERVIEW",
    participants = "Participants",
    items = "Items",
    max_score = "Maximum possible score",
    assessment_name = "Assessment name",
    assessment_name_ph = "e.g., Statistical Reasoning - Final Exam",
    assessment_date = "Assessment date",
    examiner = "Examiner",
    examiner_ph = "e.g., Eric Xaminer",
    refresh = "Reset",
    download_report = "Download report",
    search = "Search:",
    lengthMenu = "Show _MENU_ entries",
    info = "Showing _START_ to _END_ of _TOTAL_ entries",
    infoEmpty = "No entries to show",
    paginate_previous = "Previous",
    paginate_next = "Next",

    # Tabs & section headings
    tab1 = "1. Descriptives",
    tab2 = "2. Assessment analysis",
    tab3 = "3. Items flagged for review",
    s11 = "1.1 Statistics about achieved scores",
    s12 = "1.2 Distribution of achieved scores",
    s21 = "2.1 Assessment statistics",
    s22 = "2.2 Item statistics",
    s23 = "2.3 Item correlations",
    row_num_participants = "Number of participants",
    row_max_possible = "Maximum possible score",
    row_min_achieved = "Minimum achieved score",
    row_max_achieved = "Maximum achieved score",
    row_avg_achieved = "Average achieved score",
    row_median_achieved = "Median achieved score",
    row_sd = "Standard deviation",
    row_skewness = "Skewness",
    row_kurtosis = "Kurtosis",
    expl_very_difficult = "Less than 20% of participants answered correctly, suggesting the item may be ambiguous, miskeyed, or cover material not sufficiently taught.",
    expl_very_easy = "More than 80% of participants answered correctly, meaning the item provides limited discrimination between participants.",
    expl_neg_disc = "Item-total correlation is negative, which indicates that lower-performing participants answered correctly more often than higher-performing participants.",
    expl_low_disc = "Item-total correlation is below 0.20, which may incidate a weak ability to distinguish between stronger and weaker participants.",
    expl_alpha = "Item removal increases Cronbach's alpha, which may indicate that the item does not align well with the construct measured by the assessment.",
    expl_neg_rir = "Item correlates negatively with the rest of the assessment, which may indicate a potential scoring error or content misalignment.",

    # Axis labels
    axis_achieved_score = "Achieved score",
    axis_frequency = "Frequency",
    axis_item_id = "Item (Cirrus ID)",
    axis_p_difficulty = "P (Difficulty)",
    axis_rit_discrimination = "RIT (Discrimination)",
    label_max_score = "Max. score",

    # Report: titles/headings
    report_title_format = "%s: %s of %s",
    report_title = "Psychometric report",
    report_title_short = "Psychometric report",
    report_summary = "Summary",
    report_descriptives = "1. Descriptives",
    report_descriptives_11 = "1.1 Statistics about achieved scores",
    report_descriptives_12 = "1.2 Distribution of achieved scores",
    report_caa = "2. Assessment analysis",
    report_caa_21 = "2.1 Assessment statistics",
    report_caa_22 = "2.2 Item statistics",
    report_caa_23 = "2.3 Item correlations",
    report_flagged = "3. Items flagged for review",

    # Report: paragraphs & bullets
    report_generated_on = "Generated on %s by %s",
    report_summary_intro_html = "According to the <i>Guideline Assessment Analysis</i>, %s out of %s items (%s%%) show one or more psychometric discrepancies. These questions may provide useful insights when reviewed.",
    report_bullet_very_difficult = "%s item(s) are very difficult",
    report_bullet_very_easy = "%s item(s) are very easy",
    report_bullet_low_disc = "%s item(s) show low discrimination",
    report_bullet_neg_disc = "%s item(s) show negative discrimination",
    report_bullet_neg_rir = "%s item(s) show negative item-rest correlation",
    report_bullet_alpha = "%s item(s) reduce internal consistency",
    report_desc_intro = "This section provides an overview of the assessment results and key characteristics of the score distribution.",
    report_desc_11_intro = "The assessment included %s participants. The table below summarizes key descriptive statistics of their achieved scores.",
    report_desc_11_b1 = "The average score of %s represents the central tendency of the scores and reflects the typical performance level.",
    report_desc_11_b2 = "The median of %s is the middle score when all participants are ordered from lowest to highest.",
    report_desc_11_b3 = "The standard deviation of %s measures the spread of scores around the average. Higher values indicate greater variability, while lower values indicate scores clustered near the mean.",
    report_desc_11_b4 = "The skewness of %s indicates that the distribution is %s. Positive skew reflects more low scores and fewer high scores, while negative skew indicates the opposite.",
    report_desc_11_b5 = "The kurtosis of %s means the distribution is %s. Higher values indicate a stronger concentration of scores near the mean with more extreme values, while lower values indicate a flatter distribution.",
    report_desc_12_intro = "The histogram below visualizes the distribution of participant scores. The horizontal axis represents score ranges, and the vertical axis indicates the number of participants within each range.",
    report_desc_12_b1 = "Most participants scored in the %s, indicating that this range was most effective in differentiating participant performance.",
    report_desc_12_b2 = "Look out for peaks near the maximum or minimum possible scores, as this may indicate ceiling effects (many very high scores) or floor effects (many very low scores), which can impact the assessment’s ability to distinguish between stronger and weaker participants.",
    report_desc_12_b3 = "Multiple peaks in the distribution may indicate heterogeneous participant groups with different performance levels.",
    report_caa_intro = "An assessment analysis uses statistical indicators to evaluate the quality of the assessment (items). These metrics should be interpreted in the context of the assessment purpose, participant population, and sample size.",
    report_caa_21_intro = "The table below presents key evaluation metrics for the overall assessment. Cell colors follow the thresholds defined in the <i>Guideline Assessment Analysis</i>.",
    report_caa_21_b1 = "<i>Average P (Difficulty)</i>: A value near 0 indicates a very difficult assessment, whereas a value near 1 indicates a very easy one. An assessment with moderate difficulty (typically between %s and %s) tends to provide the greatest discriminatory power.",
    report_caa_21_b2 = "<i>Average RIT and RIR (Discrimination)</i>: These statistics measure the extent to which items differentiate between higher- and lower-performing participants. Values below %1$s (red) indicate weak discrimination, values between %1$s and %2$s (orange) indicate moderate discrimination, and values above %2$s (green) indicate strong discrimination.",
    report_caa_21_b3 = "<i>Cronbach's alpha (Internal Consistency)</i>: This is the internal consistency of the assessment. A value above %s (green) generally indicates acceptable reliability. Lower values may suggest inconsistent items or items that contribute little to the measurement of the intended construct. A very high value (>0.9) may indicate redundancy among items.",
    report_caa_22_intro = "This table summarizes key evaluation metrics for each individual item. Cell colors follow the thresholds defined in the <i>Guideline Assessment Analysis</i>.",
    report_caa_22_b1 = "<i>P (Item Difficulty)</i>: The P-value represents the proportion of participants who answered the item correctly. Values near 0 indicate very difficult items, while values near 1 indicate very easy items.",
    report_caa_22_b2 = "<i>RIT (Item-Total Correlation)</i>: RIT represents the correlation between an item score and the total score. Higher values indicate that the item aligns well with overall performance. Negative values require immediate review, as they indicate that lower-performing participants answered the item correctly more often than higher-performing participants.",
    report_caa_22_b3 = "<i>RIR (Item-Rest Correlation)</i>: RIR represents the correlation between an item and the total score excluding that item. Interpretation follows the same thresholds as RIT and provides a similar indication of item discrimination.",
    report_caa_22_b4 = "<i>Alpha-if-deleted</i>: This statistic indicates the change in Cronbach’s alpha if the item were removed from the assessment. An increase in alpha (red) suggests that the item may reduce internal consistency.",
    report_caa_22_plot_intro = "The plot below displays item difficulty (P-value) in relation to item discrimination (RIT).",
    report_caa_22_plot_b1 = "Items that are very easy or very difficult and show low discrimination may provide limited measurement information and may warrant revision.",
    report_caa_22_plot_b2 = "Items with moderate difficulty and strong discrimination generally contribute most effectively to reliable measurement.",
    report_caa_22_plots_intro = "The item difficulty (P-value) and item discrimination (RIT) are shown again in the figures below, with colored areas representing the relevant thresholds.",
    report_caa_23_intro = "The heatmap below shows correlations between all assessment items.",
    report_caa_23_b1 = "Items showing strong positive correlations (>0.6) may measure very similar content and should be reviewed for potential redundancy.",
    report_caa_23_b2 = "Items with negative correlations with multiple other items should be reviewed for potential scoring errors or content misalignment.",
    report_caa_23_table_intro = "The table below below shows the five strongest positive and five strongest negative correlations. Red values indicate extremely strong correlations (>0.6) and negative correlations.",
    report_flagged_intro = "The table below summarizes the %s items that may require review due to psychometric concerns. Each flagged item is listed with the specific issue(s) detected and an explanation for why it may warrant attention. The table is ordered so that the most relevant items (i.e., with the most issues) are shown first.",

    # Report: descriptors
    skew_pos = "positively skewed, with a tendency toward lower scores",
    skew_neg = "negatively skewed, with a tendency toward higher scores",
    skew_sym = "approximately symmetric",
    kurt_high = "more peaked than a normal distribution, indicating a concentration of scores near the mean and some extreme values",
    kurt_low = "flatter than a normal distribution, indicating a wider spread of scores with fewer extreme values",
    kurt_mid = "approximately normal in peakedness, indicating a balanced spread of scores around the mean",
    diff_upper = "upper range (>70%)",
    diff_middle = "middle range (>50% & <70%)",
    diff_lower = "lower range (<50%)",

    # Report: table column localizations
    col_statistic = "Statistic",
    col_value = "Value",
    col_avgP = "Average P",
    col_avgRIT = "Average RIT",
    col_avgRIR = "Average RIR",
    col_alpha = "Cronbach's alpha",
    col_item = "Item (Cirrus ID)",
    col_mean = "Mean",
    col_sd = "SD",
    col_P = "P",
    col_RIT = "RIT",
    col_RIR = "RIR",
    col_alpha_if_deleted = "Alpha-if-deleted",
    col_correlation = "Correlation",
    col_issue = "Issue",
    col_explanation = "Explanation",

    # Report: flagged items messages
    issue_very_difficult = "Very difficult item",
    issue_very_easy = "Very easy item",
    issue_neg_disc = "Negative discrimination",
    issue_low_disc = "Low discrimination",
    issue_alpha = "Reduces test reliability",
    issue_neg_rir = "Negative item-rest correlation",
    no_flagged_item = "None",
    no_flagged_issue = "No flagged items",
    no_flagged_expl = "All items fall within recommended psychometric thresholds."
  ),
  nl = list(
    # App chrome
    app_title = "Psychometrische Analyse van Cirrus Exportdata (PACED)",
    disconnect_text = "Je sessie is verlopen.",
    disconnect_refresh = "Herladen",

    # Sidebar instructions
    instructions_title = "Instructies",
    step1 = "Navigeer in Cirrus naar het tabblad Beoordelen of het tabblad Rapportages",
    step2_html = 'Selecteer de toets en exporteer "Kandidatenscores - met kriteriascores" (klik <a href="voorbeeld.xlsx" target="_blank"><u>hier</u></a> voor een voorbeeldbestand)',
    step3 = "Upload het bestand en controleer het overzicht",
    step4 = 'Klik op "Download Rapport" voor een HTML-bestand met de resultaten en hun interpretatie',
    step5_html = 'Open het rapport, klik met de rechtermuisknop, selecteer "Pagina afdrukken" en kies vervolgens PDF (klik <a href="voorbeeld.pdf" target="_blank"><u>hier</u></a> voor een voorbeelddocument)',
    upload_label_html = "<b>Upload kandidatenscores (.xlsx)</b>",
    browse_label_html = "<b>Bladeren...</b>",
    no_file = "Geen bestand geselecteerd",
    quick_overview = "BELANGRIJK: CONTROLEER DIT OVERZICHT",
    participants = "Deelnemers",
    items = "Vragen",
    max_score = "Maximaal haalbare score",
    assessment_name = "Naam toets",
    assessment_name_ph = "bijv. Statistical Reasoning - Eindtoets",
    assessment_date = "Datum toets",
    examiner = "Examinator",
    examiner_ph = "bijv. Eric Xaminator",
    refresh = "Reset",
    download_report = "Download rapport",
    search = "Zoeken:",
    lengthMenu = "Toon _MENU_ rijen",
    info = "Toont _START_ tot _END_ van _TOTAL_ rijen",
    infoEmpty = "Geen rijen om te tonen",
    paginate_previous = "Vorige",
    paginate_next = "Volgende",

    # Tabs & section headings
    tab1 = "1. Beschrijvende statistiek",
    tab2 = "2. Toetsanalyse",
    tab3 = "3. Vragen gemarkeerd voor review",
    s11 = "1.1 Statistieken over behaalde scores",
    s12 = "1.2 Verdeling van behaalde scores",
    s21 = "2.1 Toetsstatistieken",
    s22 = "2.2 Vraagstatistieken",
    s23 = "2.3 Vraagcorrelaties",
    row_num_participants = "Aantal deelnemers",
    row_max_possible = "Maximaal haalbare score",
    row_min_achieved = "Minimaal behaalde score",
    row_max_achieved = "Maximaal behaalde score",
    row_avg_achieved = "Gemiddelde behaalde score",
    row_median_achieved = "Mediaan van behaalde score",
    row_sd = "Standaarddeviatie",
    row_skewness = "Scheefheid",
    row_kurtosis = "Kurtosis",
    expl_very_difficult = "Minder dan 20% van de deelnemers beantwoordde correct; mogelijk is het item ambigu, verkeerd aangeschreven (miskeyed), of betreft het stof die onvoldoende is aangeboden.",
    expl_very_easy = "Meer dan 80% van de deelnemers beantwoordde correct, wat wijst op beperkte onderscheidingskracht.",
    expl_neg_disc = "Item-totaalcorrelatie is negatief: lager presterende deelnemers beantwoorden vaker correct dan hoger presterenden.",
    expl_low_disc = "Item-totaalcorrelatie is lager dan 0,20, wat kan wijzen op beperkt vermogen om onderscheid te maken tussen sterker en zwakker presterende deelnemers.",
    expl_alpha = "Verwijderen van het item verhoogt Cronbach’s alpha; het item sluit mogelijk onvoldoende aan op het construct dat door de toets wordt gemeten.",
    expl_neg_rir = "Negatieve correlatie met de rest van de toets; dit kan duiden op een mogelijke scoringsfout of inhoudelijke inconsistentie.",

    # Axis labels
    axis_achieved_score = "Behaalde score",
    axis_frequency = "Frequentie",
    axis_item_id = "Vraag (Cirrus ID)",
    axis_p_difficulty = "P (Moeilijkheid)",
    axis_rit_discrimination = "RIT (Discriminatie)",
    label_max_score = "Max. score",

    # Report: titles/headings
    report_title_format = "%s: %s van %s",
    report_title = "Psychometrisch rapport",
    report_title_short = "Psychometrisch rapport",
    report_summary = "Samenvatting",
    report_descriptives = "1. Beschrijvende statistiek",
    report_descriptives_11 = "1.1 Statistieken over behaalde scores",
    report_descriptives_12 = "1.2 Verdeling van behaalde scores",
    report_caa = "2. Toetsanalyse",
    report_caa_21 = "2.1 Toetsstatistieken",
    report_caa_22 = "2.2 Vraagstatistieken",
    report_caa_23 = "2.3 Vraagcorrelaties",
    report_flagged = "3. Vragen gemarkeerd voor review",

    # Report: paragraphs & bullets
    report_generated_on = "Gegenereerd op %s door %s",
    report_summary_intro_html = "Volgens de <i>Richtlijn Toetsanalyse</i> vertonen %s van de %s vragen (%s%%) één of meer psychometrische aandachtspunten. Deze vragen kunnen bij een review waardevolle inzichten opleveren.",
    report_bullet_very_difficult = "%s vragen zijn zeer moeilijk",
    report_bullet_very_easy = "%s vragen zijn zeer makkelijk",
    report_bullet_low_disc = "%s vragen vertonen lage discriminatie",
    report_bullet_neg_disc = "%s vragen vertonen negatieve discriminatie",
    report_bullet_neg_rir = "%s vragen vertonen een negatieve item‑restcorrelatie",
    report_bullet_alpha = "%s vragen verlagen de interne consistentie",
    report_desc_intro = "Deze sectie geeft een overzicht van de toetsresultaten en de belangrijkste kenmerken van de scoreverdeling.",
    report_desc_11_intro = "Aan de toets namen %s deelnemers deel. De onderstaande tabel vat de belangrijkste beschrijvende statistieken van de behaalde scores samen.",
    report_desc_11_b1 = "De gemiddelde score van %s geeft de centrale tendens weer en reflecteert het typische prestatieniveau.",
    report_desc_11_b2 = "De mediaan van %s is de middelste score wanneer alle deelnemers van laag naar hoog worden geordend.",
    report_desc_11_b3 = "De standaarddeviatie van %s meet de spreiding rond het gemiddelde. Hogere waarden duiden op meer variatie; lagere waarden op clustering rond het gemiddelde.",
    report_desc_11_b4 = "De scheefheid van %s geeft aan dat de verdeling %s is. Positieve scheefheid betekent meer lage en minder hoge scores; negatieve scheefheid het omgekeerde.",
    report_desc_11_b5 = "De kurtosis van %s betekent dat de verdeling %s is. Hogere waarden duiden op sterkere concentratie rond het gemiddelde met meer uitschieters; lagere waarden op een plattere verdeling.",
    report_desc_12_intro = "Het onderstaande histogram visualiseert de verdeling van de scores. De horizontale as toont de scorebereiken; de verticale as het aantal deelnemers binnen elk bereik.",
    report_desc_12_b1 = "De meeste deelnemers scoorden in het %s, wat aangeeft dat dit bereik het meest effectief onderscheid maakte tussen prestatieniveaus.",
    report_desc_12_b2 = "Let op pieken dicht bij de maximale of minimale score: dit kan plafond‑ of vloereffecten aangeven, wat de onderscheidende kracht kan beïnvloeden.",
    report_desc_12_b3 = "Meerdere pieken kunnen wijzen op heterogene groepen met verschillende prestatieniveaus.",
    report_caa_intro = "Een toetsanalyse gebruikt statistische indicatoren om de kwaliteit van de toets(vragen) te evalueren. Interpreteer deze in de context van doel, populatie en steekproefgrootte.",
    report_caa_21_intro = "De tabel hieronder toont kernmaten voor de gehele toets. Celkleuren volgen de drempels uit de <i>Richtlijn Toetsanalyse</i>.",
    report_caa_21_b1 = "<i>Gemiddelde P (Moeilijkheid)</i>: Een waarde dicht bij 0 duidt op een zeer moeilijke toets; dicht bij 1 op een zeer makkelijke. Gematigde moeilijkheid (typisch tussen %s en %s) geeft doorgaans de beste discriminatie.",
    report_caa_21_b2 = "<i>Gemiddelde RIT en RIR (Discriminatie)</i>: Maten voor het onderscheidend vermogen van items tussen hoger en lager presterende deelnemers. Waarden onder %1$s (rood) zijn zwak, tussen %1$s en %2$s (oranje) gemiddeld, en boven %2$s (groen) sterk.",
    report_caa_21_b3 = "<i>Cronbach’s alpha (Interne consistentie)</i>: Waarde boven %s (groen) is doorgaans acceptabel. Lagere waarden kunnen wijzen op inconsistente of weinig bijdragende vragen. Zeer hoge waarden (>0,9) kunnen redundantie betekenen.",
    report_caa_22_intro = "Deze tabel vat kernmaten per vraag samen. Celkleuren volgen de drempels uit de <i>Richtlijn Toetsanalyse</i>.",
    report_caa_22_b1 = "<i>P (Vraag‑moeilijkheid)</i>: Het aandeel deelnemers dat het item correct beantwoordde. Waarden dicht bij 0 duiden op zeer moeilijke vragen, waarden dicht bij 1 op zeer makkelijke.",
    report_caa_22_b2 = "<i>RIT (Vraag‑totaalcorrelatie)</i>: Correlatie tussen vraagscore en totaalscore. Negatieve waarden vragen directe controle.",
    report_caa_22_b3 = "<i>RIR (Vraag‑restcorrelatie)</i>: Correlatie tussen vraag en totaalscore exclusief de vraag; interpretatie gelijk aan RIT.",
    report_caa_22_b4 = "<i>Alpha‑als‑verwijderd</i>: Verandering in Cronbach’s alpha bij verwijdering van de vraag. Een stijging (rood) suggereert lagere interne consistentie door de vraag.",
    report_caa_22_plot_intro = "Onderstaande grafiek toont item‑moeilijkheid (P) versus discriminatie (RIT).",
    report_caa_22_plot_b1 = "Zeer makkelijke/moeilijke vragen met lage discriminatie leveren weinig meetinformatie en verdienen herziening.",
    report_caa_22_plot_b2 = "Vragen met gematigde moeilijkheid en sterke discriminatie dragen het meest bij aan betrouwbaarheid.",
    report_caa_22_plots_intro = "P (moeilijkheid) en RIT (discriminatie) worden hieronder opnieuw getoond; gekleurde gebieden markeren de relevante drempels.",
    report_caa_23_intro = "De onderstaande heatmap toont correlaties tussen alle vragen.",
    report_caa_23_b1 = "Sterke positieve correlaties (>0,6) kunnen duiden op inhoudelijke overlap en zouden gecontroleerd moeten worden op mogelijke redundantie.",
    report_caa_23_b2 = "Negatieve correlaties met meerdere vragen kunnen wijzen op scoringsfouten of inhoudelijke misalignment.",
    report_caa_23_table_intro = "De tabel toont de vijf sterkste positieve en vijf sterkste negatieve correlaties. Rood markeert zeer sterke (>0,6) en negatieve correlaties.",
    report_flagged_intro = "De onderstaande tabel vat de %s vragen samen die mogelijk herziening behoeven vanwege psychometrische aandachtspunten. Elke vraag is voorzien van een signaal en toelichting. De meest relevante vragen (meeste signalen) staan bovenaan.",

    # Report: descriptors
    skew_pos = "positief scheef, met een tendens naar lagere scores",
    skew_neg = "negatief scheef, met een tendens naar hogere scores",
    skew_sym = "ongeveer symmetrisch",
    kurt_high = "spitsiger dan een normaalverdeling, wat wijst op meer concentratie rond het gemiddelde en meer uitschieters",
    kurt_low = "platter dan een normaalverdeling, met bredere spreiding en minder uitschieters",
    kurt_mid = "ongeveer normaal in spitsheid, met een evenwichtige spreiding rond het gemiddelde",
    diff_upper = "bovenste bereik (>70%)",
    diff_middle = "middelse bereik (>50% & <70%)",
    diff_lower = "onderste bereik (<50%)",

    # Report: table column localizations
    col_statistic = "Statistiek",
    col_value = "Waarde",
    col_avgP = "Gemiddelde P",
    col_avgRIT = "Gemiddelde RIT",
    col_avgRIR = "Gemiddelde RIR",
    col_alpha = "Cronbach’s alpha",
    col_item = "Vraag (Cirrus ID)",
    col_mean = "Gemiddelde",
    col_sd = "SD",
    col_P = "P",
    col_RIT = "RIT",
    col_RIR = "RIR",
    col_alpha_if_deleted = "Alpha‑als‑verwijderd",
    col_correlation = "Correlatie",
    col_issue = "Signaal",
    col_explanation = "Toelichting",

    # Report: flagged items messages
    issue_very_difficult = "Zeer moeilijke vraag",
    issue_very_easy = "Zeer makkelijke vraag",
    issue_neg_disc = "Negatieve discriminatie",
    issue_low_disc = "Lage discriminatie",
    issue_alpha = "Vermindert interne consistentie van de toets",
    issue_neg_rir = "Negatieve vraag‑restcorrelatie",
    no_flagged_item = "Geen",
    no_flagged_issue = "Geen gemarkeerde vragen",
    no_flagged_expl = "Alle vragen vallen binnen de aanbevolen psychometrische drempels."
  )
)

transpose <- function(x) {
  t(x)
}

t <- function(key, lang = "en", ...) {
  txt <- (i18n[[lang]][[key]] %||% i18n$en[[key]] %||% key)
  if (length(list(...))) sprintf(txt, ...) else txt
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
        tags$span(class = "card-title", title),
        if (!is.null(subtitle)) tags$span(class = "card-subtitle", subtitle)
      )
    ),
    bslib::card_body(...),
    full_screen = FALSE
  )
}

# ---------------------------
# Content builders (logic unchanged; labels localized)
# ---------------------------
create_descriptives_table <- function(input, parsed) {
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
      sd = paste0(round(sd(totalScores), digits), " (", paste0(round(sd(totalScores) / maxScore * 100, digits), "%"), ")"),
      skewness = round(compute_skewness(totalScores), digits),
      kurtosis = round(compute_kurtosis(totalScores), digits)
    )
  }
  colnames(tab) <- c(
    "Number of participants", "Maximum possible score", "Minimum achieved score",
    "Maximum achieved score", "Average achieved score", "Median achieved score",
    "Standard deviation", "Skewness", "Kurtosis"
  )
  data.frame("Statistic" = colnames(tab), "Value" = base::t(tab), check.names = FALSE)
}

create_histogram <- function(input, parsed, lang = "en") {
  if (is.null(input$file)) {
    p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_x_continuous(name = t("axis_achieved_score", lang), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, 1)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  } else {
    req(parsed())
    d <- parsed()$data
    totalScores <- rowSums(d)
    maxScore <- parsed()$maxScore
    xBreaks <- pretty(c(0, maxScore), min.n = 4)
    xLabels <- paste0(xBreaks, " (", round(xBreaks / maxScore * 100, 2), "%)")
    p <- ggplot2::ggplot(data.frame(x = totalScores), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(bins = 30, color = "black", fill = "lightgray") +
      ggplot2::scale_x_continuous(name = t("axis_achieved_score", lang), limits = c(-0.5, max(xBreaks) + 0.5), breaks = xBreaks, labels = xLabels) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = max(xBreaks)) +
      theme_nyenrode()
    yBreaks <- pretty(c(0, max(ggplot2::ggplot_build(p)$data[[1]]$count)), min.n = 4)
    p <- p + ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, max(yBreaks)), breaks = yBreaks) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
      ggplot2::geom_segment(x = maxScore, xend = maxScore, y = 0, yend = max(yBreaks), linetype = "dashed", color = "firebrick") +
      ggplot2::annotate(geom = "text", x = maxScore, y = max(yBreaks), label = t("label_max_score", lang), hjust = 1.2, size = 5, color = "firebrick")
  }
  return(p)
}

create_test_stats <- function(input, parsed) {
  if (is.null(input$file)) {
    tab <- data.frame(P = NA, RIT = NA, RIR = NA, alpha = NA)
  } else {
    req(parsed())
    d <- parsed()$data
    digits <- parsed()$digits
    tab <- round(data.frame(P = mean(parsed()$P), RIT = mean(parsed()$RIT), RIR = mean(parsed()$RIR), alpha = total_cronbach_alpha(d)), digits)
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
    digits <- parsed()$digits
    tab <- data.frame(
      item = colnames(d),
      mean = round(colMeans(d), digits),
      sd = round(apply(d, 2, sd), digits),
      P = round(parsed()$P, digits),
      RIT = round(parsed()$RIT, digits),
      RIR = round(parsed()$RIR, digits),
      alpha_when_dropped = round(parsed()$alpha_drop, digits)
    )
  }
  colnames(tab) <- c("Item (Cirrus ID)", "Mean", "SD", "P", "RIT", "RIR", "Alpha-if-deleted")
  return(tab)
}

create_item_plot <- function(input, parsed, lang = "en") {
  if (is.null(input$file)) {
    p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_x_continuous(name = t("axis_item_id", lang), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(name = NULL, limits = c(0, 1)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  } else {
    req(parsed())
    RIT <- parsed()$RIT
    Pval <- parsed()$P
    df <- data.frame(item = factor(names(Pval), levels = names(Pval)[order(Pval)]), P = Pval, RIT = RIT)
    df_long <- stats::reshape(df, varying = list(c("P", "RIT")), v.names = "value", timevar = "metric", times = c("P", "RIT"), direction = "long")
    yBreaks <- pretty(c(0, 1, df_long$value), min.n = 4)
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = item, y = value, fill = metric)) +
      ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge(width = 0.75), color = "black") +
      ggplot2::scale_fill_manual(name = NULL, values = c(nyenrode_gold, nyenrode_blue2), labels = c(t("axis_p_difficulty", lang), t("axis_rit_discrimination", lang))) +
      ggplot2::scale_y_continuous(name = NULL, limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks) +
      ggplot2::scale_x_discrete(name = t("axis_item_id", lang)) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = min(yBreaks), yend = max(yBreaks)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 1, xend = nrow(df)) +
      theme_nyenrode() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "top")
  }
  return(p)
}

create_difficulty_distribution <- function(input, parsed, lang = "en") {
  if (is.null(input$file)) {
    p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_x_continuous(name = t("axis_p_difficulty", lang), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, 1)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  } else {
    req(parsed())
    P <- parsed()$P
    p <- ggplot2::ggplot(data.frame(x = P), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(bins = 30, color = "black", fill = nyenrode_gold) +
      ggplot2::scale_x_continuous(name = t("axis_p_difficulty", lang), limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.2)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      theme_nyenrode()
    yBreaks <- pretty(c(0, max(ggplot2::ggplot_build(p)$data[[1]]$count)), min.n = 4)
    p <- p + ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, max(yBreaks)), breaks = yBreaks) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
      ggplot2::annotate(geom = "rect", xmin = -0.05, xmax = guidelines$P[1], ymin = 0, ymax = max(yBreaks), fill = "tomato", alpha = 0.25, color = NA) +
      ggplot2::annotate(geom = "rect", xmin = guidelines$P[1], xmax = guidelines$P[2], ymin = 0, ymax = max(yBreaks), fill = "forestgreen", alpha = 0.25, color = NA) +
      ggplot2::annotate(geom = "rect", xmin = guidelines$P[2], xmax = 1.05, ymin = 0, ymax = max(yBreaks), fill = "tomato", alpha = 0.25, color = NA) +
      ggplot2::geom_histogram(bins = 30, color = "black", fill = nyenrode_gold)
  }
  return(p)
}

create_discrimination_distribution <- function(input, parsed, lang = "en") {
  if (is.null(input$file)) {
    p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_x_continuous(name = t("axis_rit_discrimination", lang), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, 1)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  } else {
    req(parsed())
    RIT <- parsed()$RIT
    xBreaks <- pretty(c(0, 1, RIT), min.n = 4)
    p <- ggplot2::ggplot(data.frame(x = RIT), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(bins = 30, color = "black", fill = nyenrode_blue2) +
      ggplot2::scale_x_continuous(name = t("axis_rit_discrimination", lang), limits = c(min(xBreaks) - 0.05, max(xBreaks) + 0.05), breaks = xBreaks) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      theme_nyenrode()
    yBreaks <- pretty(c(0, max(ggplot2::ggplot_build(p)$data[[1]]$count)), min.n = 4)
    p <- p + ggplot2::scale_y_continuous(name = t("axis_frequency", lang), limits = c(0, max(yBreaks)), breaks = yBreaks) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
      ggplot2::annotate(geom = "rect", xmin = min(xBreaks) - 0.05, xmax = guidelines$RIT[1], ymin = 0, ymax = max(yBreaks), fill = "tomato", alpha = 0.25, color = NA) +
      ggplot2::annotate(geom = "rect", xmin = guidelines$RIT[1], xmax = guidelines$RIT[2], ymin = 0, ymax = max(yBreaks), fill = "orange", alpha = 0.25, color = NA) +
      ggplot2::annotate(geom = "rect", xmin = guidelines$RIT[2], xmax = max(xBreaks) + 0.05, ymin = 0, ymax = max(yBreaks), fill = "forestgreen", alpha = 0.25, color = NA) +
      ggplot2::geom_histogram(bins = 30, color = "black", fill = nyenrode_blue2)
  }
  return(p)
}

create_corr_plot <- function(input, parsed, lang = "en") {
  if (is.null(input$file)) {
    p <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_x_continuous(name = t("axis_item_id", lang), limits = c(0, 1)) +
      ggplot2::scale_y_continuous(name = t("axis_item_id", lang), limits = c(0, 1)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 1) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      theme_nyenrode() +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  } else {
    req(parsed())
    cor_mat <- parsed()$correlations
    diag(cor_mat) <- NA
    cor_mat[lower.tri(cor_mat, diag = FALSE)] <- 0
    cor_df <- as.data.frame(as.table(cor_mat))
    colnames(cor_df) <- c("Var1", "Var2", "Correlation")
    xBreaks <- unique(cor_df$Var1)
    yBreaks <- unique(cor_df$Var2)
    col_breaks <- pretty(c(-1, 1), min.n = 5)
    p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
      ggplot2::geom_tile(color = ifelse(cor_df$Correlation == 0, NA, "black")) +
      ggplot2::scale_fill_gradient2(name = NULL, low = "tomato", mid = "white", high = "forestgreen", na.value = "black", midpoint = 0, limits = c(-1, 1), breaks = col_breaks) +
      ggplot2::scale_x_discrete(name = t("axis_item_id", lang), breaks = xBreaks) +
      ggplot2::scale_y_discrete(name = t("axis_item_id", lang), breaks = yBreaks) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 1, yend = length(yBreaks)) +
      ggplot2::geom_segment(y = -Inf, yend = -Inf, x = 1, xend = length(xBreaks)) +
      theme_nyenrode() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "top", legend.key.width = ggplot2::unit(4, "cm"))
  }
  return(p)
}

create_high_cor_items <- function(input, parsed) {
  if (is.null(input$file)) {
    tab <- data.frame(item1 = NA, item2 = NA, cor = NA)
  } else {
    req(parsed())
    cor_mat <- parsed()$correlations
    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
    df <- na.omit(as.data.frame(as.table(cor_mat)))
    colnames(df) <- c("Item1", "Item2", "Correlation")
    tab <- df[order(-abs(df$Correlation)), ]
  }
  colnames(tab) <- c("Item (Cirrus ID)", "Item (Cirrus ID)", "Correlation")
  tab$Correlation <- round(tab$Correlation, 3)
  return(tab)
}

create_flagged_items <- function(input, parsed) {
  if (is.null(input$file)) {
    tab <- data.frame(Item = NA, Issue = NA, Explanation = NA)
  } else {
    req(parsed())
    item_stats <- create_item_stats(input, parsed)
    test_stats <- create_test_stats(input, parsed)
    alpha_test <- test_stats$`Cronbach's alpha`[1]
    flagged <- list()
    for (i in 1:nrow(item_stats)) {
      item <- item_stats$`Item (Cirrus ID)`[i]
      P <- item_stats$P[i]
      RIT <- item_stats$RIT[i]
      RIR <- item_stats$RIR[i]
      alpha_drop <- item_stats$`Alpha-if-deleted`[i]
      issues <- c()
      explanations <- c()
      if (P < guidelines$P[1]) {
        issues <- c(issues, "Very difficult item")
        explanations <- c(explanations, "Less than 20% of participants answered correctly, suggesting the item may be ambiguous, miskeyed, or cover material not sufficiently taught.")
      }
      if (P > guidelines$P[2]) {
        issues <- c(issues, "Very easy item")
        explanations <- c(explanations, "More than 80% of participants answered correctly, meaning the item provides limited discrimination between participants.")
      }
      if (RIT < 0) {
        issues <- c(issues, "Negative discrimination")
        explanations <- c(explanations, "Item-total correlation is negative, which indicates that lower-performing participants answered correctly more often than higher-performing participants.")
      }
      if (RIT >= 0 & RIT < guidelines$RIT[1]) {
        issues <- c(issues, "Low discrimination")
        explanations <- c(explanations, "Item-total correlation is below 0.20, which may incidate a weak ability to distinguish between stronger and weaker participants.")
      }
      if (alpha_drop > alpha_test) {
        issues <- c(issues, "Reduces test reliability")
        explanations <- c(explanations, "Item removal increases Cronbach's alpha, which may indicate that the item does not align well with the construct measured by the assessment.")
      }
      if (RIR < 0) {
        issues <- c(issues, "Negative item-rest correlation")
        explanations <- c(explanations, "Item correlates negatively with the rest of the assessment, which may indicate a potential scoring error or content misalignment.")
      }
      if (length(issues) > 0) {
        flagged[[length(flagged) + 1]] <- data.frame(
          Item = item,
          Issue = paste(unique(issues), collapse = "; "),
          Explanation = paste(unique(explanations), collapse = " "),
          NumIssues = length(issues),
          stringsAsFactors = FALSE
        )
      }
    }
    tab <- do.call(rbind, flagged)
    if (!is.null(tab) && nrow(tab) > 0) {
      tab <- tab[order(-tab$NumIssues), ]
      tab$NumIssues <- NULL
    } else {
      tab <- data.frame(
        Item = "None",
        Issue = "No flagged items",
        Explanation = "All items fall within recommended psychometric thresholds.",
        stringsAsFactors = FALSE
      )
    }
  }
  colnames(tab) <- c("Item (Cirrus ID)", "Issue", "Explanation")
  return(tab)
}

# ---------------------------
# UI table helpers: localize column names for in-app DT tables
# ---------------------------
localize_ui_colnames <- function(df, lang = "en", type = c("desc", "test", "item", "corr", "flagged")) {
  type <- match.arg(type)
  out <- df

  if (type == "corr") {
    # Replace ALL occurrences of the duplicated label
    from_item <- "Item (Cirrus ID)"
    to_item <- t("col_item", lang) # e.g., "Vraag (Cirrus ID)" in NL (or "Vraag ID" if you set that)
    from_cor <- "Correlation"
    to_cor <- t("col_correlation", lang)

    idx_item <- which(colnames(out) == from_item)
    if (length(idx_item)) colnames(out)[idx_item] <- to_item

    idx_cor <- which(colnames(out) == from_cor)
    if (length(idx_cor)) colnames(out)[idx_cor] <- to_cor

    return(out)
  }

  # default behavior for other table types (unchanged)
  colmap <- switch(type,
    desc = setNames(c(t("col_statistic", lang), t("col_value", lang)), c("Statistic", "Value")),
    test = setNames(
      c(t("col_avgP", lang), t("col_avgRIT", lang), t("col_avgRIR", lang), t("col_alpha", lang)),
      c("Average P", "Average RIT", "Average RIR", "Cronbach's alpha")
    ),
    item = setNames(
      c(
        t("col_item", lang), t("col_mean", lang), t("col_sd", lang), t("col_P", lang),
        t("col_RIT", lang), t("col_RIR", lang), t("col_alpha_if_deleted", lang)
      ),
      c("Item (Cirrus ID)", "Mean", "SD", "P", "RIT", "RIR", "Alpha-if-deleted")
    ),
    flagged = setNames(
      c(t("col_item", lang), t("col_issue", lang), t("col_explanation", lang)),
      c("Item (Cirrus ID)", "Issue", "Explanation")
    )
  )
  common <- intersect(names(colmap), colnames(out))
  colnames(out)[match(common, colnames(out))] <- unname(colmap[common])
  out
}

# Translate content (row labels) for the Descriptives table (no number formatting changes)
localize_desc_content <- function(df, lang = "en") {
  # Map English row labels to i18n keys
  key_map <- setNames(
    c(
      "row_num_participants", "row_max_possible", "row_min_achieved",
      "row_max_achieved", "row_avg_achieved", "row_median_achieved",
      "row_sd", "row_skewness", "row_kurtosis"
    ),
    c(
      "Number of participants", "Maximum possible score", "Minimum achieved score",
      "Maximum achieved score", "Average achieved score", "Median achieved score",
      "Standard deviation", "Skewness", "Kurtosis"
    )
  )

  # Translate the first column (row labels)
  df$Statistic <- ifelse(
    df$Statistic %in% names(key_map),
    vapply(df$Statistic, function(k) t(key_map[[k]], lang), character(1)),
    df$Statistic
  )
  df
}

# Translate content of the flagged-items table: issue labels + explanations (robust to factors/NA)
translate_flagged_ui <- function(df, lang = "en") {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  # Ensure required columns exist
  required_cols <- c("Item (Cirrus ID)", "Issue", "Explanation")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols)) {
    return(df)
  }

  # Coerce to character to avoid strsplit() on non-character vectors
  df[required_cols] <- lapply(df[required_cols], function(x) {
    if (is.factor(x)) as.character(x) else as.character(x)
  })

  # Localize the synthetic "no flagged items" row if present
  first_item <- df[["Item (Cirrus ID)"]][1]
  if (identical(first_item, "None")) {
    df[["Item (Cirrus ID)"]][1] <- t("no_flagged_item", lang)
    df[["Issue"]][1] <- t("no_flagged_issue", lang)
    df[["Explanation"]][1] <- t("no_flagged_expl", lang)
    return(df)
  }

  # Maps for issues and explanations
  issue_map <- c(
    "Very difficult item"             = t("issue_very_difficult", lang),
    "Very easy item"                  = t("issue_very_easy", lang),
    "Negative discrimination"         = t("issue_neg_disc", lang),
    "Low discrimination"              = t("issue_low_disc", lang),
    "Reduces test reliability"        = t("issue_alpha", lang),
    "Negative item-rest correlation"  = t("issue_neg_rir", lang)
  )

  expl_map <- c(
    "Less than 20% of participants answered correctly, suggesting the item may be ambiguous, miskeyed, or cover material not sufficiently taught." =
      t("expl_very_difficult", lang),
    "More than 80% of participants answered correctly, meaning the item provides limited discrimination between participants." =
      t("expl_very_easy", lang),
    "Item-total correlation is negative, which indicates that lower-performing participants answered correctly more often than higher-performing participants." =
      t("expl_neg_disc", lang),
    "Item-total correlation is below 0.20, which may incidate a weak ability to distinguish between stronger and weaker participants." =
      t("expl_low_disc", lang),
    "Item removal increases Cronbach's alpha, which may indicate that the item does not align well with the construct measured by the assessment." =
      t("expl_alpha", lang),
    "Item correlates negatively with the rest of the assessment, which may indicate a potential scoring error or content misalignment." =
      t("expl_neg_rir", lang)
  )

  # --- Translate Issues (semicolon-separated) ---
  df[["Issue"]] <- vapply(df[["Issue"]], function(s) {
    if (is.na(s) || !nzchar(s)) {
      return(s)
    }
    parts <- trimws(unlist(strsplit(s, ";", fixed = TRUE)))
    if (!length(parts)) {
      return(s)
    }
    parts_tr <- ifelse(parts %in% names(issue_map), issue_map[parts], parts)
    paste(unique(parts_tr), collapse = "; ")
  }, character(1))

  # --- Translate Explanations (replace known English sentences) ---
  df[["Explanation"]] <- vapply(df[["Explanation"]], function(s) {
    if (is.na(s) || !nzchar(s)) {
      return(s)
    }
    out <- s
    for (k in names(expl_map)) {
      out <- gsub(k, expl_map[[k]], out, fixed = TRUE)
    }
    out
  }, character(1))

  df
}

compute_skewness <- function(x) {
  m <- mean(x)
  s <- sd(x)
  return(mean((x - m)^3) / s^3)
}

compute_kurtosis <- function(x) {
  m <- mean(x)
  s <- sd(x)
  return(mean((x - m)^4) / s^4 - 3)
}

total_cronbach_alpha <- function(data) {
  data <- na.omit(data)
  k <- ncol(data)
  if (k < 2) {
    return(NA)
  }
  item_vars <- apply(data, 2, var)
  total_var <- var(rowSums(data))
  if (isTRUE(all.equal(total_var, 0))) {
    return(NA)
  }
  (k / (k - 1)) * (1 - sum(item_vars) / total_var)
}

parse_num <- function(x) {
  as.numeric(gsub(",", ".", x, fixed = TRUE))
}

format_decimal <- function(df, lang) {
  if (lang == "nl") {
    num_cols <- which(vapply(df, is.numeric, logical(1)))
    if (length(num_cols)) {
      df[num_cols] <- lapply(
        df[num_cols],
        function(x) formatC(x, decimal.mark = ",", format = "fg")
      )
    }
  }
  df
}

# ---------------------------
# Report helpers: localize column names for report tables
# ---------------------------
localize_report_colnames <- function(df, lang = "en", type = c("desc", "test", "item", "corr", "flagged")) {
  type <- match.arg(type)
  out <- df

  if (type == "corr") {
    from_item <- "Item (Cirrus ID)"
    to_item <- t("col_item", lang)
    from_cor <- "Correlation"
    to_cor <- t("col_correlation", lang)

    idx_item <- which(colnames(out) == from_item)
    if (length(idx_item)) colnames(out)[idx_item] <- to_item

    idx_cor <- which(colnames(out) == from_cor)
    if (length(idx_cor)) colnames(out)[idx_cor] <- to_cor

    return(out)
  }

  # default behavior for other types (unchanged)
  colmap <- switch(type,
    desc = setNames(c(t("col_statistic", lang), t("col_value", lang)), c("Statistic", "Value")),
    test = setNames(
      c(t("col_avgP", lang), t("col_avgRIT", lang), t("col_avgRIR", lang), t("col_alpha", lang)),
      c("Average P", "Average RIT", "Average RIR", "Cronbach's alpha")
    ),
    item = setNames(
      c(
        t("col_item", lang), t("col_mean", lang), t("col_sd", lang), t("col_P", lang),
        t("col_RIT", lang), t("col_RIR", lang), t("col_alpha_if_deleted", lang)
      ),
      c("Item (Cirrus ID)", "Mean", "SD", "P", "RIT", "RIR", "Alpha-if-deleted")
    ),
    flagged = setNames(
      c(t("col_item", lang), t("col_issue", lang), t("col_explanation", lang)),
      c("Item (Cirrus ID)", "Issue", "Explanation")
    )
  )
  common <- intersect(names(colmap), colnames(out))
  colnames(out)[match(common, colnames(out))] <- unname(colmap[common])
  out
}

# ---------------------------
# HTML report builder (localized)
# ---------------------------
build_report_html <- function(
  name,
  examiner,
  examdate,
  descriptives,
  hist_plot,
  test_stats,
  item_stats,
  item_plot,
  difficulty_plot,
  discrimination_plot,
  corr_plot,
  high_corr_items,
  flagged_items,
  lang = "en"
) {
  # ----- helper: embed plot in memory (faster) -----
  embed_plot <- function(plot, width, height) {
    # Use a raster device
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = width, height = height, units = "in", res = 300)
    print(plot)
    dev.off()

    # Read back as raw bytes for base64 encoding
    img_data <- readBin(tmp, "raw", n = file.info(tmp)$size)
    uri <- paste0("data:image/png;base64,", base64enc::base64encode(img_data))
    tags$img(src = uri, style = "display:block; margin:20px auto; max-width:100%;")
  }

  # ----- Precompute key stats once -----
  participants <- descriptives$Value[descriptives$Statistic == "Number of participants"]
  avg_score <- descriptives$Value[descriptives$Statistic == "Average achieved score"]
  median_score <- descriptives$Value[descriptives$Statistic == "Median achieved score"]
  sd_score <- descriptives$Value[descriptives$Statistic == "Standard deviation"]
  skew <- parse_num(descriptives$Value[descriptives$Statistic == "Skewness"])
  skew_text <- if (skew > 0.5) t("skew_pos", lang) else if (skew < -0.5) t("skew_neg", lang) else t("skew_sym", lang)
  kurt <- parse_num(descriptives$Value[descriptives$Statistic == "Kurtosis"])
  kurt_text <- if (kurt > 0.5) t("kurt_high", lang) else if (kurt < -0.5) t("kurt_low", lang) else t("kurt_mid", lang)
  difficulty_range <- {
    mean_P <- mean(item_stats$P)
    if (mean_P > 0.7) {
      t("diff_upper", lang)
    } else if (mean_P > 0.5) {
      t("diff_middle", lang)
    } else {
      t("diff_lower", lang)
    }
  }

  alpha_test <- test_stats$`Cronbach's alpha`[1]

  # ----- Vectorized coloring for test stats -----
  test_tab <- test_stats
  test_tab$`Average P` <- kableExtra::cell_spec(
    test_tab$`Average P`, "html",
    color = ifelse(test_tab$`Average P` < guidelines$P[1], "tomato",
      ifelse(test_tab$`Average P` <= guidelines$P[2], "forestgreen", "tomato")
    )
  )
  test_tab$`Average RIT` <- kableExtra::cell_spec(
    test_tab$`Average RIT`, "html",
    color = ifelse(test_tab$`Average RIT` < guidelines$RIT[1], "tomato",
      ifelse(test_tab$`Average RIT` <= guidelines$RIT[2], "orange", "forestgreen")
    )
  )
  test_tab$`Average RIR` <- kableExtra::cell_spec(
    test_tab$`Average RIR`, "html",
    color = ifelse(test_tab$`Average RIR` < guidelines$RIR[1], "tomato",
      ifelse(test_tab$`Average RIR` <= guidelines$RIR[2], "orange", "forestgreen")
    )
  )
  test_tab$`Cronbach's alpha` <- kableExtra::cell_spec(
    test_tab$`Cronbach's alpha`, "html",
    color = ifelse(test_tab$`Cronbach's alpha` < guidelines$alpha[1], "tomato", "forestgreen")
  )

  # ----- Vectorized coloring for item stats -----
  item_tab <- item_stats
  item_tab$P <- kableExtra::cell_spec(
    item_tab$P, "html",
    color = ifelse(item_tab$P < guidelines$P[1], "tomato",
      ifelse(item_tab$P <= guidelines$P[2], "forestgreen", "tomato")
    )
  )
  item_tab$RIT <- kableExtra::cell_spec(
    item_tab$RIT, "html",
    color = ifelse(item_tab$RIT < guidelines$RIT[1], "tomato",
      ifelse(item_tab$RIT <= guidelines$RIT[2], "orange", "forestgreen")
    )
  )
  item_tab$RIR <- kableExtra::cell_spec(
    item_tab$RIR, "html",
    color = ifelse(item_tab$RIR < guidelines$RIR[1], "tomato",
      ifelse(item_tab$RIR <= guidelines$RIR[2], "orange", "forestgreen")
    )
  )
  item_tab$`Alpha-if-deleted` <- kableExtra::cell_spec(
    item_tab$`Alpha-if-deleted`, "html",
    color = ifelse(item_tab$`Alpha-if-deleted` < alpha_test, "forestgreen", "tomato")
  )

  # ----- Localize tables once -----
  descriptives_loc <- localize_desc_content(descriptives, lang)
  desc_tab <- localize_report_colnames(descriptives_loc, lang, "desc") |>
    knitr::kable(row.names = FALSE, format = "html", table.attr = 'class="left_table"') |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  test_tab <- localize_report_colnames(test_tab, lang, "test") |>
    knitr::kable(escape = FALSE, row.names = FALSE, format = "html", table.attr = 'class="center_table"') |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  item_tab <- localize_report_colnames(item_tab, lang, "item") |>
    knitr::kable(escape = FALSE, row.names = FALSE, format = "html", table.attr = 'class="center_table"') |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  # Correlation table
  high_corr_items_positive <- high_corr_items[high_corr_items$Correlation > 0, ]
  high_corr_items_positive <- high_corr_items_positive[order(-high_corr_items_positive$Correlation), ]
  high_corr_items_negative <- high_corr_items[high_corr_items$Correlation < 0, ]
  high_corr_items_negative <- high_corr_items_negative[order(high_corr_items_negative$Correlation), ]
  corr_tab <- rbind(head(high_corr_items_positive, 5), head(high_corr_items_negative, 5))
  corr_tab$Correlation <- kableExtra::cell_spec(corr_tab$Correlation, "html", color = ifelse(corr_tab$Correlation < 0, "tomato", ifelse(corr_tab$Correlation < 0.6, "forestgreen", "tomato")))
  corr_tab <- localize_report_colnames(corr_tab, lang, "corr")
  corr_tab <- knitr::kable(corr_tab, escape = FALSE, row.names = FALSE, format = "html", table.attr = 'class="center_table"') |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  # Flagged items table (translate CONTENT issues + explanations, then HEADERS)
  flagged_items_local <- translate_flagged_ui(flagged_items, lang)
  flagged_tab <- localize_report_colnames(flagged_items_local, lang, "flagged")
  flagged_tab <- knitr::kable(flagged_tab, row.names = FALSE, format = "html", table.attr = 'class="left_table"') |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  # ----- Guideline discrepancy summary -----
  if (!is.null(flagged_items) && nrow(flagged_items) > 0 && flagged_items$`Item (Cirrus ID)`[1] != "None") {
    issues <- flagged_items$Issue
    n_difficult <- sum(grepl("Very difficult item", issues))
    n_easy <- sum(grepl("Very easy item", issues))
    n_low_disc <- sum(grepl("Low discrimination", issues))
    n_neg_disc <- sum(grepl("Negative discrimination", issues))
    n_alpha <- sum(grepl("Reduces test reliability", issues))
    n_neg_rir <- sum(grepl("Negative item-rest correlation", issues))
    n_total <- length(unique(flagged_items$`Item (Cirrus ID)`))
  } else {
    n_difficult <- n_easy <- n_low_disc <- n_neg_disc <- n_alpha <- n_neg_rir <- 0
    n_total <- 0
  }

  # ----- Build HTML -----
  tagList(
    tags$html(
      tags$head(
        tags$title(t("report_title", lang)),
        tags$style(HTML("
          body { margin: 0; padding: 0; font-family: Arial, Helvetica, sans-serif; background: #FFFFFF; }
          #report_container { width: 90%; margin: 0 auto; padding: 20px 0; text-align: justify;}
          h1 { color: #00205B; text-align: left; margin-bottom: 20px; }
          h2, h3 { margin-top: 30px; margin-bottom: 15px; }
          table { border-collapse: collapse; width: 100%; }
          .left_table th { white-space: nowrap; text-align: left !important; vertical-align: middle !important; color: #00205B !important; background: #f4f6fb; border: 0.5px solid #ddd; padding: 6px; }
          .center_table th { text-align: center !important; vertical-align: middle !important; color: #00205B !important; background: #f4f6fb; border: 0.5px solid #ddd; padding: 6px; }
          .left_table td { text-align: left !important; vertical-align: middle !important; border: 0.5px solid #ddd; padding: 6px; }
          .left_table td:nth-child(3) { text-align: justify !important; }
          .center_table td { text-align: center !important; vertical-align: middle !important; border: 0.5px solid #ddd; padding: 6px; }
          img { max-width: 100%; display: block; margin: 10px auto; }
            /* ---------- PRINT LAYOUT (PDF) ---------- */
           @page { size: A4;  margin: 20mm 18mm 20mm 18mm; }
           @media print {
             body {margin: 0; }
             #report_container { width: 100%; }
             h2 { page-break-before: auto; page-break-after: avoid;}
             h3 { page-break-after: avoid; }
             table { page-break-inside: avoid; }
             img { page-break-inside: avoid; }
             p { orphans: 3; widows: 3;}
             footer { position: fixed; bottom: 10mm; left: 0; right: 0; text-align: center; font-size: 11px; color: #666;}
           }
        "))
      ),
      tags$body(
        tags$div(
          id = "report_container",
          h1(sprintf(
            t("report_title_format", lang),
            t("report_title", lang),
            if (name == "") "....." else name,
            format(examdate, "%d-%m-%Y")
          )),
          p(sprintf(
            t("report_generated_on", lang),
            format(Sys.time(), "%d-%m-%Y"),
            if (examiner == "") "....." else examiner
          )),
          tags$hr(),
          h2(t("report_summary", lang)),
          p(HTML(sprintf(
            t("report_summary_intro_html", lang),
            n_total, nrow(item_stats), round(n_total / nrow(item_stats) * 100, 2)
          ))),
          tags$ul(
            tags$li(sprintf(t("report_bullet_very_difficult", lang), n_difficult, guidelines$P[1])),
            tags$li(sprintf(t("report_bullet_very_easy", lang), n_easy, guidelines$P[2])),
            tags$li(sprintf(t("report_bullet_low_disc", lang), n_low_disc, guidelines$RIT[1])),
            tags$li(sprintf(t("report_bullet_neg_disc", lang), n_neg_disc)),
            tags$li(sprintf(t("report_bullet_neg_rir", lang), n_neg_rir)),
            tags$li(sprintf(t("report_bullet_alpha", lang), n_alpha))
          ),
          tags$hr(),
          h2(t("report_descriptives", lang)),
          p(t("report_desc_intro", lang)),
          h3(t("report_descriptives_11", lang)),
          p(sprintf(t("report_desc_11_intro", lang), participants)),
          tags$ul(
            tags$li(p(sprintf(t("report_desc_11_b1", lang), avg_score))),
            tags$li(p(sprintf(t("report_desc_11_b2", lang), median_score))),
            tags$li(p(sprintf(t("report_desc_11_b3", lang), sd_score))),
            tags$li(p(sprintf(t("report_desc_11_b4", lang), skew, skew_text))),
            tags$li(p(sprintf(t("report_desc_11_b5", lang), kurt, kurt_text)))
          ),
          HTML(desc_tab),
          h3(t("report_descriptives_12", lang)),
          p(t("report_desc_12_intro", lang)),
          embed_plot(hist_plot, 7, 4),
          tags$ul(
            tags$li(sprintf(t("report_desc_12_b1", lang), difficulty_range)),
            tags$li(p(t("report_desc_12_b2", lang))),
            tags$li(p(t("report_desc_12_b3", lang)))
          ),
          tags$hr(),
          h2(t("report_caa", lang)),
          p(t("report_caa_intro", lang)),
          h3(t("report_caa_21", lang)),
          p(HTML(t("report_caa_21_intro", lang))),
          tags$ul(
            tags$li(p(HTML(sprintf(t("report_caa_21_b1", lang), guidelines$P[1], guidelines$P[2])))),
            tags$li(p(HTML(sprintf(t("report_caa_21_b2", lang), guidelines$RIT[1], guidelines$RIT[2])))),
            tags$li(p(HTML(sprintf(t("report_caa_21_b3", lang), guidelines$alpha[1]))))
          ),
          HTML(test_tab),
          h3(t("report_caa_22", lang)),
          p(HTML(t("report_caa_22_intro", lang))),
          tags$ul(
            tags$li(p(HTML(t("report_caa_22_b1", lang)))),
            tags$li(p(HTML(t("report_caa_22_b2", lang)))),
            tags$li(p(HTML(t("report_caa_22_b3", lang)))),
            tags$li(p(HTML(t("report_caa_22_b4", lang))))
          ),
          HTML(item_tab),
          p(t("report_caa_22_plot_intro", lang)),
          tags$ul(
            tags$li(p(t("report_caa_22_plot_b1", lang))),
            tags$li(p(t("report_caa_22_plot_b2", lang)))
          ),
          embed_plot(item_plot, 9, 5),
          p(t("report_caa_22_plots_intro", lang)),
          tags$div(
            style = "display:flex; gap:30px; margin-top:20px;",
            tags$div(style = "flex:1;", embed_plot(difficulty_plot, 4.5, 3)),
            tags$div(style = "flex:1;", embed_plot(discrimination_plot, 4.5, 3))
          ),
          h3(t("report_caa_23", lang)),
          p(t("report_caa_23_intro", lang)),
          tags$ul(
            tags$li(p(t("report_caa_23_b1", lang))),
            tags$li(p(t("report_caa_23_b2", lang)))
          ),
          embed_plot(corr_plot, 11, 11),
          p(t("report_caa_23_table_intro", lang)),
          HTML(corr_tab),
          tags$hr(),
          h2(t("report_flagged", lang)),
          p(sprintf(t("report_flagged_intro", lang), nrow(flagged_items))),
          HTML(flagged_tab),
          tags$hr()
        )
      )
    )
  )
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Full-page overlay */
      #preloader-overlay {
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: white;    /* or rgba(...) */
        z-index: 99999;
        display: flex;
        justify-content: center;
        align-items: center;
      }

      /* Spinner */
      .loader {
        border: 8px solid #f3f3f3;
        border-top: 8px solid #00205B;   /* Nyenrode blue */
        border-radius: 50%;
        width: 60px;
        height: 60px;
        animation: spin 1s linear infinite;
      }

      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
  ),

  # Preloader div (visible immediately)
  div(
    id = "preloader-overlay",
    div(class = "loader")
  ),
  title = "PACED",
  tags$script(HTML("window.parent.document.title = 'PACED';")),
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png")
  ),
  tags$script(HTML("
  $(document).on('shiny:value', function(e) {
    if (e.name === 'sidebar_ui') {
      var el = document.querySelector('.sidebar');
      if (el && !el.classList.contains('sidebar-ready')) {
        el.classList.add('sidebar-ready');
      }
    }
  });")),
  # --- Visual theme (Bootstrap 5) ---
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = NULL,
    bg = "#FFFFFF",
    fg = "#1A1A1A",
    primary = nyenrode_blue,
    secondary = nyenrode_gold,
    info = "#0054A6",
    base_font = "Arial, Helvetica, sans-serif",
    heading_font = "Arial, Helvetica, sans-serif",
    "font-size-base" = "1rem"
  ) |>
    bslib::bs_add_rules(rules = sprintf("
    .app-title-bar { background: linear-gradient(90deg, %1$s 0%%, #0b2c7d 100%%); color: #fff; padding: 14px 18px; margin-bottom: 18px; box-shadow: 0 2px 6px rgba(0,0,0,.08); }
    .app-title-bar-inner { display:flex; align-items:center; justify-content: space-between; }
    .app-title {font-weight: 800; font-size: 1.75rem; letter-spacing: .4px;}
    .lang-toggle .btn { color:#00205B; background:#fff; border:none; font-weight:700; }
    .sidebar { visibility: hidden; background-color: #f7f9fc; border-right: 1px solid #e6e9ef; }
    .sidebar.sidebar-ready { visibility: visible; }
    .sidebar h2 { color: %1$s; font-size: 1.25rem; margin-top: 0; }
    .sidebar .btn, .sidebar .form-control { border-radius: .4rem; }
    .sidebar .btn-primary { background-color: %1$s; border-color: %1$s; }
    .sidebar .btn-primary:hover { filter: brightness(1.05); }
    .app-card .card-header { background: #ffffff; border-bottom: 1px solid #e6e9ef; padding: 12px 16px;}
    .nav-tabs .nav-link {color: %2$s; font-weight: 700;}
    .nav-tabs .nav-link:hover { color: %1$s;}
    .nav-tabs .nav-link.active { color: %2$s; font-weight: 700;}
    .card-title-row { display: flex; align-items: center; gap: 10px; }
    .card-title { font-weight: 700; color: %1$s; }
    table.dataTable thead th { min-width: max-content; background: #f4f6fb; color: #24324a; font-weight: 700; }
    table.dataTable tbody tr:nth-child(odd) { min-width: max-content; background-color: #fbfcff; }
    table.dataTable tbody tr:hover { background-color: #f5f8ff; }
    .mt-2 { margin-top: .5rem; } .mt-3 { margin-top: 1rem; } .mb-3 { margin-bottom: 1rem; }
  ", nyenrode_blue, nyenrode_gold)),

  # --- Localized disconnect overlay (rendered from server) ---
  uiOutput("disconnect_ui"),

  # --- Title bar with language toggle ---
  uiOutput("title_bar"),

  # --- Layout with persistent sidebar (dynamic UI) ---
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      uiOutput("sidebar_ui"),
      width = 4
    ),
    mainPanel(
      uiOutput("main_ui")
    )
  ),
  tags$script(HTML("
  // Continuously remove any beforeunload handlers that appear
  const removeBeforeUnload = () => {
    window.onbeforeunload = null;

    // Remove all listeners of type beforeunload added via addEventListener
    const clone = window.cloneNode;
    if (clone) {
      // Deep clone window to strip event listeners – hacky but effective
      const newWindow = clone.call(window, true);
    }
  };

  // Run immediately
  removeBeforeUnload();

  // Run after Shiny initializes
  $(document).on('shiny:connected', removeBeforeUnload);

  // Run after any UI update (e.g. language switch)
  $(document).on('shiny:value', removeBeforeUnload);

  // Re-run every 250ms (safest: catches late-added handlers)
  setInterval(removeBeforeUnload, 250);")),
  tags$script(HTML("
  // Hide preloader once Shiny is fully loaded
  $(document).on('shiny:connected', function(){
    $('#preloader-overlay').fadeOut(300);
  });"))
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  # Language state
  lang <- reactiveVal("nl")
  observe({
    if (lang() == "nl") {
      options(OutDec = ",")
    } else {
      options(OutDec = ".")
    }
  })

  # Disconnect overlay (localized)
  output$disconnect_ui <- renderUI({
    shinydisconnect::disconnectMessage(
      text = t("disconnect_text", lang()),
      refresh = t("disconnect_refresh", lang()),
      background = "#646464e6",
      size = 36, width = "full", top = "center",
      colour = "white", refreshColour = nyenrode_blue,
      overlayColour = "#999", overlayOpacity = 0.4
    )
  })

  # Title bar with top-right language toggle
  output$title_bar <- renderUI({
    cur <- lang()

    tags$div(
      class = "app-title-bar",
      tags$div(
        class = "app-title-bar-inner",

        # --- TITLE + LOGO ---
        tags$div(
          class = "app-title",
          style = "display: flex; align-items: center; gap: 12px;",
          tags$img(
            src = "logo.png",
            style = "height: 40px; width: auto;"
          ),
          t("app_title", cur)
        ),

        # --- LANGUAGE TOGGLE (unchanged) ---
        tags$div(
          class = "lang-toggle",
          style = "display: inline-flex; border: 1px solid #ccc; border-radius: 25px; overflow: hidden;",
          actionButton(
            "lang_nl",
            label = "NL",
            class = "btn btn-sm",
            style = paste0(
              "border-radius: 25px 0 0 25px;",
              "margin: 0; padding: 0 12px; border-right: 1px solid #ccc;",
              "background-color: ", if (cur == "nl") nyenrode_gold else "#FFFFFF", ";",
              "color: ", nyenrode_blue, ";",
              "font-weight: 700;"
            )
          ),
          actionButton(
            "lang_en",
            label = "EN",
            class = "btn btn-sm",
            style = paste0(
              "border-radius: 0 25px 25px 0;",
              "margin: 0; padding: 0 12px;",
              "background-color: ", if (cur == "en") nyenrode_gold else "#FFFFFF", ";",
              "color: ", nyenrode_blue, ";",
              "font-weight: 700;"
            )
          )
        )
      )
    )
  })
  observeEvent(input$lang_nl, {
    lang("nl")
  })
  observeEvent(input$lang_en, {
    lang("en")
  })

  observeEvent(input$refresh, {
    session$reload()
  })

  # Data import
  rawData <- reactive({
    req(input$file)
    readxl::read_excel(path = input$file$datapath, col_names = FALSE, na = c("N/A", "n.b."))
  })

  # Parse & clean
  parsed <- eventReactive(input$file, {
    req(rawData())
    validate(need(nrow(rawData()) > 4, "File does not appear to be a valid Cirrus export."))
    dataset <- rawData()
    digits <- 3

    # Filter only the scores
    index_first_question <- which(!(dataset[1, ] %in% c("Vraag", "Question", NA)))[1]
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
    correlations <- cor(dataset, use = "pairwise.complete.obs")
    P <- colMeans(dataset) / questionMaxPoints
    RIT <- sapply(dataset, function(x) cor(x, rowSums(dataset), use = "pairwise.complete.obs"))
    RIR <- sapply(seq_along(dataset), function(j) {
      x <- dataset[[j]]
      cor(x, rowSums(dataset) - x, use = "pairwise.complete.obs")
    })
    alpha_drop <- sapply(seq_along(dataset), function(j) {
      total_cronbach_alpha(dataset[, -j, drop = FALSE])
    })

    list(
      data = dataset,
      correlations = correlations,
      P = P,
      RIT = RIT,
      RIR = RIR,
      alpha_drop = alpha_drop,
      maxPoints = questionMaxPoints,
      maxScore = maxScore,
      questionNames = questionNames,
      digits = digits
    )
  })

  # Sidebar (localized)
  output$sidebar_ui <- renderUI({
    cur <- lang()
    tagList(
      h2(HTML(sprintf("<b>%s</b>", t("instructions_title", cur)))),
      tags$ol(
        tags$li(t("step1", cur)),
        tags$li(HTML(t("step2_html", cur))),
        tags$li(t("step3", cur)),
        tags$li(t("step4", cur)),
        tags$li(HTML(t("step5_html", cur)))
      ),
      fileInput("file", HTML(t("upload_label_html", cur)),
        accept = ".xlsx",
        buttonLabel = HTML(t("browse_label_html", cur)),
        placeholder = t("no_file", cur)
      ),
      section_card(
        t("quick_overview", cur),
        uiOutput("dataset_info")
      ),
      textInput("name", label = t("assessment_name", cur), placeholder = t("assessment_name_ph", cur)),
      dateInput("date", label = t("assessment_date", cur), format = "dd-mm-yyyy", weekstart = 1),
      textInput("name_examiner", label = t("examiner", cur), placeholder = t("examiner_ph", cur)),
      div(
        class = "mt-3",
        actionButton("refresh", HTML(sprintf("<b>%s</b>", t("refresh", cur))), class = "btn btn-outline-secondary"),
        tags$span(" "),
        downloadButton(outputId = "export", label = t("download_report", cur), class = "btn btn-primary")
      )
    )
  })

  # Main panel (localized)
  output$main_ui <- renderUI({
    cur <- lang()
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = t("tab1", cur),
        br(),
        section_card(t("s11", cur), shinycssloaders::withSpinner(DT::dataTableOutput("descriptives"), type = 8, color = nyenrode_blue, hide.ui = FALSE, size = 0.25)),
        section_card(t("s12", cur), div(style = "width: 100%; margin: 0 auto;", shinycssloaders::withSpinner(plotOutput("histogram", width = "100%"), type = 8, color = nyenrode_blue, hide.ui = FALSE)))
      ),
      tabPanel(
        title = t("tab2", cur),
        br(),
        section_card(t("s21", cur), div(style = "padding: 0;", shinycssloaders::withSpinner(DT::dataTableOutput("test_stats"), type = 8, color = nyenrode_blue, hide.ui = FALSE, size = 0.25))),
        section_card(
          t("s22", cur),
          shinycssloaders::withSpinner(DT::dataTableOutput("item_stats"), , type = 8, color = nyenrode_blue, hide.ui = FALSE, size = 0.25),
          div(
            style = "width: 100%; margin: 0 auto;",
            plotOutput("item_plot", width = "100%"),
            fluidRow(
              column(6, shinycssloaders::withSpinner(plotOutput("difficulty_dist", width = "100%"), type = 8, color = nyenrode_blue, hide.ui = FALSE)),
              column(6, shinycssloaders::withSpinner(plotOutput("discrimination_dist", width = "100%"), type = 8, color = nyenrode_blue, hide.ui = FALSE))
            )
          )
        ),
        section_card(
          t("s23", cur),
          div(
            style = "width: 100%; margin: 0 auto;",
            shinycssloaders::withSpinner(plotOutput("corr_plot", width = "100%", height = "1000px"), type = 8, color = nyenrode_blue, hide.ui = FALSE),
            shinycssloaders::withSpinner(DT::dataTableOutput("high_cor_items"), type = 8, color = nyenrode_blue, hide.ui = FALSE, size = 0.25)
          )
        )
      ),
      tabPanel(
        title = t("tab3", cur),
        br(),
        shinycssloaders::withSpinner(DT::dataTableOutput("flagged_items"), type = 8, color = nyenrode_blue, hide.ui = FALSE, size = 0.25)
      )
    )
  })

  # Quick overview widget (localized)
  output$dataset_info <- renderUI({
    cur <- lang()
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
      div(paste0(t("participants", cur), ": "), participants),
      div(paste0(t("items", cur), ": "), items),
      div(paste0(t("max_score", cur), ": "), maxscore)
    )
  })

  # Computations
  descriptives_react <- reactive(create_descriptives_table(input, parsed))
  histogram_react <- reactive(create_histogram(input, parsed, lang()))
  test_stats_react <- reactive(create_test_stats(input, parsed))
  item_stats_react <- reactive(create_item_stats(input, parsed))
  item_plot_react <- reactive(create_item_plot(input, parsed, lang()))
  difficulty_dist_react <- reactive(create_difficulty_distribution(input, parsed, lang()))
  discrimination_dist_react <- reactive(create_discrimination_distribution(input, parsed, lang()))
  corr_plot_react <- reactive(create_corr_plot(input, parsed, lang()))
  high_cor_items_react <- reactive(create_high_cor_items(input, parsed))
  flagged_items_react <- reactive(create_flagged_items(input, parsed))

  # Output tables/plots (UI table headers remain as in original; colors unchanged)
  output$descriptives <- DT::renderDataTable({
    cur <- lang()
    df <- descriptives_react()

    # CONTENT translation (row labels)
    df <- localize_desc_content(df, cur)

    # HEADERS translation
    df <- localize_ui_colnames(df, cur, "desc")

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, pageLength = 9, stateSave = FALSE),
      class = "stripe compact"
    )
  })
  output$histogram <- renderPlot(histogram_react())

  output$test_stats <- DT::renderDataTable({
    cur <- lang()
    df <- test_stats_react()

    # HEADERS translation
    df_loc <- localize_ui_colnames(df, cur, "test")

    # Localized column names for styling references
    col_avgP <- t("col_avgP", cur)
    col_avgRIT <- t("col_avgRIT", cur)
    col_avgRIR <- t("col_avgRIR", cur)
    col_alpha <- t("col_alpha", cur)

    DT::datatable(format_decimal(df_loc, cur),
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, pagelength = 1, stateSave = FALSE),
      class = "compact"
    ) |>
      DT::formatStyle(col_avgP, color = DT::styleInterval(guidelines$P, c("tomato", "forestgreen", "tomato"))) |>
      DT::formatStyle(col_avgRIT, color = DT::styleInterval(guidelines$RIT, c("tomato", "orange", "forestgreen"))) |>
      DT::formatStyle(col_avgRIR, color = DT::styleInterval(guidelines$RIR, c("tomato", "orange", "forestgreen"))) |>
      DT::formatStyle(col_alpha, color = DT::styleInterval(guidelines$alpha, c("tomato", "forestgreen")))
  })

  output$item_stats <- DT::renderDataTable({
    cur <- lang()
    df <- item_stats_react()

    # HEADERS translation
    df_loc <- localize_ui_colnames(df, cur, "item")

    # Localized column names
    col_P <- t("col_P", cur)
    col_RIT <- t("col_RIT", cur)
    col_RIR <- t("col_RIR", cur)
    col_alphaD <- t("col_alpha_if_deleted", cur)

    alpha_threshold <- test_stats_react()[1, 4]

    DT::datatable(format_decimal(df_loc, cur),
      rownames = FALSE,
      options = list(pageLength = 5, scrollY = "250px", autoWidth = TRUE, stateSave = FALSE, language = list(search = t("search", cur), lengthMenu = t("lengthMenu", cur), info = t("info", cur), infoEmpty = t("infoEmpty", cur), paginate = list("previous" = t("paginate_previous", cur), "next" = t("paginate_next", cur)))),
      class = "stripe compact"
    ) |>
      DT::formatStyle(col_P, color = DT::styleInterval(guidelines$P, c("tomato", "forestgreen", "tomato"))) |>
      DT::formatStyle(col_RIT, color = DT::styleInterval(guidelines$RIT, c("tomato", "orange", "forestgreen"))) |>
      DT::formatStyle(col_RIR, color = DT::styleInterval(guidelines$RIT, c("tomato", "orange", "forestgreen"))) |>
      DT::formatStyle(col_alphaD, color = DT::styleInterval(c(alpha_threshold), c("forestgreen", "tomato")))
  })

  output$item_plot <- renderPlot(item_plot_react())
  output$difficulty_dist <- renderPlot(difficulty_dist_react())
  output$discrimination_dist <- renderPlot(discrimination_dist_react())
  output$corr_plot <- renderPlot(corr_plot_react())

  output$high_cor_items <- DT::renderDataTable({
    cur <- lang()
    df <- high_cor_items_react()

    # HEADERS translation
    df_loc <- localize_ui_colnames(df, cur, "corr")

    col_corr <- t("col_correlation", cur)

    DT::datatable(format_decimal(df_loc, cur),
      rownames = FALSE,
      options = list(pageLength = 5, autoWidth = TRUE, stateSave = FALSE),
      class = "stripe compact"
    ) |>
      DT::formatStyle(col_corr, color = DT::styleInterval(c(0, 0.6), c("tomato", "forestgreen", "tomato")))
  })

  output$flagged_items <- DT::renderDataTable({
    cur <- lang()
    df <- flagged_items_react()

    # CONTENT translation (issues + explanations, and "None" row)
    df <- translate_flagged_ui(df, cur)

    # HEADERS translation
    df <- localize_ui_colnames(df, cur, "flagged")

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, stateSave = FALSE, language = list(search = t("search", cur), lengthMenu = t("lengthMenu", cur), info = t("info", cur), infoEmpty = t("infoEmpty", cur), paginate = list("previous" = t("paginate_previous", cur), "next" = t("paginate_next", cur)))),
      class = "stripe compact"
    )
  })

  # Export (HTML report; localized)
  output$export <- downloadHandler(
    filename = function() {
      paste0(t("report_title_short", lang()), if (input$name != "") paste0(" ", input$name) else NULL, ".html")
    },
    content = function(file) {
      withProgress(message = "Generating report...", value = 0, {
        incProgress(0.3, detail = "Copying template...")
        report <- build_report_html(
          input$name,
          input$name_examiner,
          input$date,
          descriptives_react(),
          histogram_react(),
          test_stats_react(),
          item_stats_react(),
          item_plot_react(),
          difficulty_dist_react(),
          discrimination_dist_react(),
          corr_plot_react(),
          high_cor_items_react(),
          flagged_items_react(),
          lang = lang()
        )
        incProgress(0.3, detail = "Rendering HTML...")
        htmltools::save_html(report, file)
        incProgress(1, detail = "Done!")
      })
    }
  )
}

shinyApp(ui, server)

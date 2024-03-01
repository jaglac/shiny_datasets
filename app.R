library(shiny)
library(bslib)
library(ggplot2)
library(datasets)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(GGally)
library(ggmosaic)
library(stringr)

dataset_names <- c("airmiles", "AirPassengers", "airquality", "attenu", "attitude", "austres",
                   "beaver1", "beaver2", "BJsales", "BOD",
                   "cars", "ChickWeight", "chickwts", "CO2", "co2",
                   "discoveries", "DNase",
                   "esoph", "euro", "eurodist", "EuStockMarkets",
                   "faithful", "fdeaths", "Formaldehyde", "freeny",
                   "HairEyeColor", "Harman23.cor", "Harman74.cor",
                   "Indometh", "infert", "InsectSprays", "iris",
                   "JohnsonJohnson",
                   "LakeHuron", "ldeaths", "lh", "LifeCycleSavings", "Loblolly", "longley", "lynx",
                   "mdeaths", "morley", "mtcars",
                   "nhtemp", "Nile", "nottem", "npk",
                   "occupationalStatus", "Orange", "OrchardSprays",
                   "PlantGrowth", "precip", "presidents", "pressure", "Puromycin",
                   "quakes",
                   "rivers", "rock",
                   "Seatbelts", "sleep", "stackloss", "state.x77", "sunspot.month", "sunspot.year", "sunspots", "swiss",
                   "Theoph", "Titanic", "ToothGrowth", "treering", "trees",
                   "UCBAdmissions", "UKgas", "USAccDeaths", "USArrests", "UScitiesD", "USJudgeRatings", "USPersonalExpenditure", "uspop",
                   "VADeaths", "volcano",
                   "warpbreaks", "women", "WorldPhones", "WWWusage")



# Define UI for application
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "lux"),
  title = "Visualizing R's Datasets",
  
  sidebar = sidebar(
    #selectInput('dataset', 'Dataset:', c(Choose='', ls("package:datasets")), selectize=FALSE)
    selectInput('dataset', 'Dataset:', c(Choose='', dataset_names), selectize=FALSE)
  ),
  
  navset_card_underline(
    nav_panel("Description",
              htmlOutput("desc")),
    nav_panel("Preview",
              fluidRow("Glimpse:", verbatimTextOutput("glimpse")),
              fluidRow("Head:", verbatimTextOutput("head")),
              fluidRow("Summary:", verbatimTextOutput("sum"))),
    nav_panel("Plot",
              plotOutput("plot"))
  )
)



# Define server logic
server <- function(input, output) {
  
  # Fetch and render dataset description
  getDesc <- function() {
    req(input$dataset) # Check if input is specified, if not then do not execute the rest of the code block
    if (dir.exists("tmp") == FALSE) { dir.create("tmp") }
    tools::Rd2HTML(utils:::.getHelpFile(as.character(help(input$dataset, package="datasets"))), out = paste0("tmp/", input$dataset, ".html"))
    return(
      suppressWarnings( includeHTML(paste0("tmp/", input$dataset, ".html")) ) # Suppress warnings necessary because includeHTML() doesn't like being fed a full page of HTML
    )
  }
  output$desc <- renderUI({ 
    getDesc()
  })
  
  # Build and render data preview
  output$glimpse <- renderPrint({ 
    req(input$dataset)
    dplyr::glimpse(get(input$dataset)) })
  output$head <- renderPrint({ 
    req(input$dataset)
    head(get(input$dataset)) })
  output$sum <- renderPrint({ 
    req(input$dataset)
    summary(get(input$dataset)) })
  
  # Build and render plots
  output$plot <- renderPlot({
    if (input$dataset == "airmiles"){ 
      df <- data.frame(miles=as.matrix(airmiles), date = zoo::as.Date(time(airmiles)))
      ggplot(df, aes(x=date, y=miles)) +
        geom_line() +
        labs(title = "Passenger-miles flown by U.S. commercial airlines",
             x = "Date",
             y = "Miles") +
        theme_clean()
    } else if (input$dataset == "AirPassengers"){
      df <- data.frame(n_pass=as.matrix(AirPassengers), date = zoo::as.Date(time(AirPassengers)))
      ggplot(df, aes(x = date, y = n_pass)) +
        geom_line() +
        labs(title = "Passenger Numbers Over Time (1949-1960)",
             x = "Month",
             y = "Number of Passengers") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
        theme_clean() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
    } else if (input$dataset == "airquality") {
      labels <- c("Ozone"="Ozone (ppb)",
                  "Solar.R"="Solar Radiation (lang)",
                  "Wind"="Wind (mph)",
                  "Temp"="Temperature (degrees F)")
      df <- airquality |> 
        dplyr::mutate(date = zoo::as.Date(paste("1973", Month, Day, sep="-")), .keep="unused") |>
        tidyr::pivot_longer(cols = 1:4, names_to = "measurement")
      ggplot(df, aes(x=date,y=value)) +
        geom_line() +
        facet_wrap(~measurement, scales = "free", labeller = as_labeller(labels)) + 
        labs(title = "Daily air quality measurements in New York, May to September 1973",
             x = "Date",
             y = "Measurement") +
        theme_clean()
    } else if (input$dataset == "attenu"){
      labels <- c("dist" = "Station-hypocenter distance (km)",
                  "accel" = "Peak acceleration (g)")
      df <- attenu |>
        tidyr::pivot_longer(cols = 4:5, names_to = "measurement") |>
        dplyr::mutate(mag = as.factor(mag))
      ggplot(df, aes(x=mag, y = value)) +
        geom_boxplot() +
        facet_wrap(~measurement, scales = "free", labeller = as_labeller(labels)) +
        labs(title = "Peak accelerations measured at various observation stations for 23 earthquakes in California",
             x = "Moment Magnitude",
             y = "Measurement") +
        theme_clean()
    } else if (input$dataset == "attitude"){
      df <- attitude |>
        dplyr::rename(overall = rating) |>
        tidyr::pivot_longer(cols = 1:7, names_to = "category") |>
        dplyr::mutate(category = relevel(as.factor(category), ref = "overall"))
      ggplot(df, aes(x=category,y=value)) +
        geom_violin(trim=FALSE) +
        stat_summary(fun.data = "mean_cl_boot", geom="pointrange") +
        labs(title = "Survey of Clerical Employees at a Large Financial Organization",
             x = "Category",
             y = "Percent Proportion of Favorable Responses (Mean & 95% CI)") +
        scale_x_discrete(labels = c("Overall rating", "Advancement", "Handling of\nemployee complaints", "Too critical",
                                    "Opportunity\nto learn", "Does not allow\nspecial privileges", "Raises based\non performance")) +
        theme_clean()
    } else if (input$dataset == "austres"){
      df <- data.frame(n_res=as.matrix(austres), date = zoo::as.Date(time(austres)))
      ggplot(df, aes(x=date, y=n_res)) +
        geom_line() +
        labs(title = "Residents of Australia Over Time",
             x = "Date",
             y = "Residents (thousands)") +
        scale_x_date(date_labels = "%Y", date_breaks = "12 months") +
        theme_clean() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    } else if (input$dataset == "beaver1"){
      df <- beaver1 |>
        dplyr::mutate(date = dplyr::case_when(day=="346" ~ "1990-12-12",
                                              day=="347" ~ "1990-12-13"),
                      time2 = dplyr::case_when(nchar(time)==1 ~ paste0("000",time),
                                               nchar(time)==2 ~ paste0("00",time),
                                               nchar(time)==3 ~ paste0("0",time),
                                               nchar(time)==4 ~ as.character(time)),
                      .keep = "unused") |>
        dplyr::mutate(date = as.POSIXct(strptime(paste0(date,"-",time2), format="%Y-%m-%d-%H%M")),
                      activity = dplyr::case_when(activ==0 ~ "FALSE",
                                                  activ==1 ~ "TRUE")) |>
        dplyr::select(date, temp, activity)
      
      ggplot(df, aes(x=date,y=temp)) +
        geom_line() +
        geom_point(aes(x=date, y=temp, color=activity)) + 
        scale_color_manual(values = c("black", "red"), name = "activity") +
        scale_x_datetime(date_labels = "%D\n%H:%M", breaks="2 hours") +  
        labs(title = "Body temperature dynamics of Castor canadensis in north-central Wisconsin",
             x = "Date - Time",
             y = "Temperature (degrees Celsius)") +
        theme_clean()
    } else if (input$dataset == "beaver2"){
      df <- beaver2 |>
        dplyr::mutate(date = dplyr::case_when(day=="307" ~ "1990-11-03",
                                              day=="308" ~ "1990-11-04"),
                      time2 = dplyr::case_when(nchar(time)==1 ~ paste0("000",time),
                                               nchar(time)==2 ~ paste0("00",time),
                                               nchar(time)==3 ~ paste0("0",time),
                                               nchar(time)==4 ~ as.character(time)),
                      .keep = "unused") |>
        dplyr::mutate(date = as.POSIXct(strptime(paste0(date,"-",time2), format="%Y-%m-%d-%H%M")),
                      activity = dplyr::case_when(activ==0 ~ "FALSE",
                                                  activ==1 ~ "TRUE")) |>
        dplyr::select(date, temp, activity)
      
      ggboxplot(df, x="activity", y="temp", add="jitter") +
        stat_compare_means(method="t.test") +  
        labs(title = "Body temperature dynamics of Castor canadensis in north-central Wisconsin",
             x = "Observed Activity Outside Den",
             y = "Temperature (degrees Celsius)")
    } else if (input$dataset == "BJsales"){
      df <- dplyr::left_join(data.frame(sales=as.matrix(BJsales), obs = time(BJsales)), 
                             data.frame(lead=as.matrix(BJsales.lead), obs = time(BJsales.lead)))
      ggscatter(df, x="lead", y="sales", add="reg.line") +
        stat_cor() +
        labs(title = "Leading indicator vs. sales (Box & Jenkins 1976)",
             x = "Leading Indicator",
             y = "Sales")
    } else if (input$dataset == "BOD"){
      ggscatter(BOD, x="Time", y="demand", add = "reg.line") + 
        stat_cor() +
        labs(title = "Biochemical oxygen demand over time in an evaluation of water quality",
             x = "Time (days)",
             y = "Biochemcial oxygen demand (mg/l)")
    } else if (input$dataset == "cars"){
      ggscatter(cars, x="speed", y="dist", add = "reg.line") +
        stat_cor() +
        labs(title = "Stopping distance of cars in the 1920s",
             x = "Speed (mph)",
             y = "Stopping Distance (ft)")
    } else if (input$dataset == "ChickWeight") {
      df <- ChickWeight |>
        dplyr::group_by(Chick) |>
        dplyr::mutate(weight_change = weight - weight[Time==0]) |>
        dplyr::filter(Time == 21)
      
      ggboxplot(df, x="Diet", y="weight_change", add="jitter") + 
        stat_compare_means(method="anova", label.y=375) +
        stat_compare_means(method="t.test", label="p.signif", ref.group=".all.", label.y=350) +
        labs(title = "Effect of diet on early growth of chicks",
             x = "Diet",
             y = "Change in Weight over 21 Days (gm)")
    } else if (input$dataset == "chickwts"){
      ggboxplot(chickwts, x="feed", y="weight", add="jitter") + 
        stat_compare_means(method="anova", label.y=450) +
        stat_compare_means(method="t.test", label="p.signif", ref.group=".all.", label.y=425) +
        labs(title = "Effect of diet on growth of chicks",
             x = "Diet",
             y = "Change in Weight over 6 Weeks (gm)")
    } else if (input$dataset == "CO2"){
      df <- CO2 |>
        dplyr::group_by(Plant)
      ggplot(df, aes(x=conc, y=uptake, color=Treatment, group=Plant)) + 
        geom_line() + 
        facet_wrap(~Type) +
        theme_clean() + 
        labs(title = "Effect of plant origin on cold tolerance of the grass species Echinochloa crus-galli",
             x = "Ambient Carbon Dioxide Concentration (mL/L)",
             y = bquote('Carbon Dioxide Uptake Rate ('*mu~'mol/'~m[2]~' sec)'))
    } else if (input$dataset == "co2"){
      df <- data.frame(conc=as.matrix(co2), date = zoo::as.Date(time(co2)))
      
      ggscatter(df, x="date", y="conc") +
        geom_smooth(method="lm", se=FALSE) +
        stat_cor() + 
        labs(title = bquote('Mauna Loa Atmospheric '*CO^2*' Concentration'),
             x = "Date",
             y = bquote('Atmospheric concentrations of '*CO^2*' (ppm)'))
    } else if (input$dataset == "discoveries"){
      df <- data.frame(n=as.matrix(discoveries), year = zoo::as.Date(time(discoveries)))
      ggline(df, x="year", y="n", palette="jco") + 
        labs(title = 'Important Scientific Discoveries By Year',
             x = "Year",
             y = 'Number of important discoveries')
    } else if (input$dataset == "DNase"){
      df <- DNase |> dplyr::mutate(log_conc = log(conc))
      
      ggscatter(df, x="log_conc", y="density", color="Run", palette="simpsons") +
        geom_smooth(color="black", se=FALSE) +
        theme(legend.position = "none") +
        labs(title = 'ELISA Activity of DNase in Rat Serum',
             x = "Protein Concentration (log)",
             y = 'Optical Density')
      
    } else if (input$dataset == "esoph"){
      df <- esoph |> 
        group_by(agegp) |>
        mutate(total_cases = sum(ncases), 
               total_controls = sum(ncontrols),
               percentage = 100 * total_cases / (total_cases+total_controls))
      ggplot(df, aes(x = alcgp, y = tobgp, fill = percentage)) +
        geom_tile() +
        facet_wrap(~agegp) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
        labs(title = "Esophageal Cancer Cases in Ille-et-Vilaine, France", 
             x = "Alcohol Consumption", 
             y = "Tobacco Consumption", 
             fill = "Cancer Cases (%)")
    } else if (input$dataset == "euro"){
      df <- as.data.frame(as.matrix(euro)) |>
        tibble::rownames_to_column("currency") |>
        rename(rate = V1) |>
        mutate(rate = log(rate, base=10))
      ggdotchart(df, x="currency", y="rate") + 
        theme_cleveland() +
        labs(title = "Conversion rates between the various European currencies", 
             x = "Currency", 
             y = "Value of 1 Euro (log10)")
    } else if (input$dataset == "eurodist"){
      df <- as.data.frame(as.matrix(eurodist)) |>
        tibble::rownames_to_column("dest") |>
        tidyr::pivot_longer(cols = 2:22, names_to = "start", values_to = "km")
      ggplot(df, aes(x=dest, y=start, fill=km)) +
        geom_tile() +
        scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
        theme_pubclean() +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              legend.text = element_text(angle = 45, hjust=1)) +
        labs(title = "Road Distances Between Cities in Europe", 
             x = NULL, 
             y = NULL, 
             fill = "Road Distance (km)")
    } else if (input$dataset == "EuStockMarkets"){
      df <- data.frame(.preformat.ts(EuStockMarkets), stringsAsFactors = FALSE) |>
        tibble::rownames_to_column("timepoint") |>
        tidyr::pivot_longer(cols=2:5, names_to="index", values_to="closing_price")
      
      ggline(df, x="timepoint", y="closing_price", color = "index", 
             plot_type = "l", palette = "jco") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 40)]) +
        labs(title = "Daily Closing Prices of Major European Stock Indices, 1991-1998", 
             x = "Date (business time)", 
             y = "Closing Price", 
             color = "Stock Index")
    } else if (input$dataset == "faithful"){
      ggscatter(faithful, x="waiting", y="eruptions", add="reg.line") +
        stat_cor() +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        labs(title = "Old Faithful Geyser in Yellowstone National Park", 
             x = "Waiting Time Between Eruptions (min)", 
             y = "Eruption Duration (min)")
    } else if (input$dataset == "fdeaths"){
      df <- data.frame(deaths=as.matrix(fdeaths), date = zoo::as.Date(time(fdeaths)))
      ggline(df, x="date", y="deaths") +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        labs(title = "Monthly Deaths From Lung Diseases in Females in the UK, 1974-1979", 
             x = "Month", 
             y = "Deaths")
    } else if (input$dataset == "Formaldehyde"){
      ggscatter(Formaldehyde, x="carb", y="optden", add="reg.line") +
        stat_cor() +
        labs(title = "Formaldehyde Standard Curve", 
             x = "Carbohydrate (ml)", 
             y = "Optical Density")
    } else if (input$dataset == "freeny"){
      ggpairs(freeny,
              columnLabels = c("Quarterly Revenue", "Lagged Quarterly Revenue", "Price Index", "Income Level", "Market Potential")) +
        labs(title = "Freeny's data on quarterly revenue and explanatory variables")
    } else if (input$dataset == "HairEyeColor"){
      df <- as.data.frame(HairEyeColor) |> 
        slice(rep(1:n(), Freq)) |>
        mutate(Freq=1)
      ggplot(df) +
        geom_mosaic(aes(x=product(Hair), fill=Eye), show.legend = FALSE) +
        facet_wrap(~Sex) +
        scale_fill_viridis_d() +
        theme_pubr() +
        labs(title = "Relationship Between Hair and Eye Color in Males & Females", 
             x = "Hair Color", 
             y = "Eye Color") 
    } else if (input$dataset == "Harman23.cor"){
      df <- as.data.frame(Harman23.cor)[,1:8] |>
        tibble::rownames_to_column("measurement") |>
        tidyr::pivot_longer(cols=2:9, names_to="cov", values_to="value") |>
        mutate(cov = stringr::str_extract(cov, pattern = "(?<=\\.)(.*)"))
      ggplot(df) +
        geom_tile(aes(x=measurement, y=cov, fill=value)) +
        scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
        theme_pubclean() +
        labs(title = "Correlation of Measurements on 305 Girls Aged 7-17", 
             x = NULL, 
             y = NULL,
             fill = "R") 
    } else if (input$dataset == "Harman74.cor"){
      df <- as.data.frame(Harman74.cor) |>
        select(1:24) |>
        tibble::rownames_to_column("test") |>
        tidyr::pivot_longer(cols=2:25, names_to="cov", values_to="value") |>
        mutate(cov = stringr::str_extract(cov, pattern = "(?<=\\.)(.*)"))
      ggplot(df, aes(x=test, y=cov, fill=value)) +
        geom_tile() +
        scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
        theme_pubclean() +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        labs(title = "Correlation of Psychological Tests Given to Seventh- and Eighth-Grade Children", 
             x = NULL, 
             y = NULL,
             fill = "R")
    } else if (input$dataset == "Indometh"){
      df <- as.data.frame(Indometh) |>
        mutate(Subject = paste0("Subject ", Subject))
      ggplot(df, aes(x=time, y=conc)) +
        geom_point() +
        geom_smooth(method="loess", se=FALSE) +
        facet_wrap(~Subject) +
        theme_pubr() +
        labs(title = "Pharmacokinetics of Indometacin", 
             x = "Time (hr)", 
             y = "Concentration (mcg/mL)")
    } else if (input$dataset == "infert"){
      df <- infert |>
        mutate(case = case_when(case == 0 ~ "ctrl",
                                case == 1 ~ "case")) |>
        select(education, age, parity, induced, case, spontaneous)
      ggpairs(df,
              columnLabels = c("Education", "Age (Years of Case)", "Parity (Count)", "N Prior Induced", "Case Status", "N Prior Spontaneous")) + 
        labs(title = "Infertility after Spontaneous and Induced Abortion")
    } else if (input$dataset == "InsectSprays"){
      ggboxplot(InsectSprays, x="spray", y="count", add="jitter") +
        labs(title = "Effectiveness of Insect Sprays", 
             x = "Spray", 
             y = "Insect Count (per agricultural experimental unit)")
    } else if (input$dataset == "iris"){
      ggpairs(iris) + 
        labs(title = "Measurements of 50 Flowers From Each of 3 Species of Iris")
    } else if (input$dataset == "islands"){
      df <- as.data.frame(as.matrix(islands)) |>
        tibble::rownames_to_column("landmass") |>
        rename(area = V1) |>
        mutate(area = log(area, base=10))
      ggdotchart(df, x="landmass", y="area") + 
        theme_cleveland() +
        labs(title = "Area of the World's Major Landmasses (log10[thousands of square miles])", 
             x = "Landmass")
    } else if (input$dataset == "JohnsonJohnson"){
      df <- data.frame(earnings=as.matrix(JohnsonJohnson), quarter = zoo::as.yearqtr(time(JohnsonJohnson)))
      ggline(df, x="quarter", y="earnings") +
        labs(title = "Quarterly Earnings per Johnson & Johnson Share", 
             x = "Date", 
             y = "Earnings (dollars)")
    } else if (input$dataset == "LakeHuron"){
      df <- data.frame(level=as.matrix(LakeHuron), year = zoo::as.Date(time(LakeHuron))) 
      ggline(df, x="year", y="level") +
        scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
        labs(title = "Level of Lake Huron, 1875-1972", 
             x = "Year", 
             y = "Level (feet)")
    } else if (input$dataset == "ldeaths"){
      df <- data.frame(deaths=as.matrix(ldeaths), date = zoo::as.Date(time(ldeaths)))
      ggline(df, x="date", y="deaths") +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        labs(title = "Monthly Deaths From Lung Diseases in the UK, 1974-1979", 
             x = "Month", 
             y = "Deaths")
    } else if (input$dataset == "lh"){
      df <- data.frame(level=as.matrix(lh), sample = time(lh))
      ggline(df, x="sample", y="level") +
        labs(title = "Luteinizing hormone in blood samples at 10 mins intervals from a human female", 
             x = "Sample Number", 
             y = "Luteinizing Hormone Level")
    } else if (input$dataset == "LifeCycleSavings"){
      ggpairs(LifeCycleSavings,
              columnLabels = c("Personal Savings Rate", "% of Population <15", "% of Population >75", "Per-Capita Disposable Income (DPI)", "% Growth Rate of DPI")) +
        labs(title = "Data on the savings ratio 1960–1970")
    } else if (input$dataset == "Loblolly"){
      ggscatter(Loblolly, x="age", y="height", add="reg.line") +
        stat_cor() +
        labs(title = "Growth of Loblolly pine trees", 
             x = "Age (years)", 
             y = "Height (feet)")
    } else if (input$dataset == "longley"){
      ggpairs(longley) +
        labs(title = "Longley's Economic Regression Data")
    } else if (input$dataset == "lynx"){
      df <- data.frame(trappings=as.matrix(lynx), year = time(lynx))
      ggline(df, x="year", y="trappings") +
        labs(title = "Annual Canadian Lynx Trappings", 
             x = "Year", 
             y = "Number of Trappings")
    } else if (input$dataset == "mdeaths"){
      df <- data.frame(deaths=as.matrix(mdeaths), date = zoo::as.Date(time(mdeaths)))
      ggline(df, x="date", y="deaths") +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        labs(title = "Monthly Deaths From Lung Diseases in Males in the UK, 1974-1979", 
             x = "Month", 
             y = "Deaths")
    } else if (input$dataset == "morley"){
      ggboxplot(morley, x="Expt", y="Speed", add="jitter") +
        labs(title = "Measurements of the Speed of Light, 1879", 
             x = "Experiment", 
             y = "Speed (km/sec, with 299000 subtracted)")
    } else if (input$dataset == "mtcars"){
      df <- mtcars |>
        mutate(vs = case_when(vs == 0 ~ "V-shaped",
                              vs == 1 ~ "straight"),
               am = case_when(am == 0 ~ "automatic",
                              am == 1 ~ "manual"))
      ggpairs(df,
              columnLabels = c("MPG", "N Cylinders", "Displacement (cu. in.)", "Gross Horsepower", "Rear Axle Ratio",
                               "Weight (1000 lbs)", "1/4 Mile Time", "Engine Shape", "Transmission Type", "N Forward Gears", "N Carburetors")) +
        labs(title = "Motor Trend Car Road Tests, 1974",
             subtitle = "Fuel consumption and 10 aspects of automobile design and performance for 32 automobiles.")
    } else if (input$dataset == "nhtemp"){
      df <- data.frame(temp=as.matrix(nhtemp), year = zoo::as.Date(time(nhtemp)))
      ggline(df, x="year", y="temp") +
        scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        labs(title = "Mean annual temperature in New Haven, Connecticut", 
             x = "Year", 
             y = "Temperature (degrees Fahrenheit)")
    } else if (input$dataset == "Nile"){
      df <- data.frame(flow=as.matrix(Nile), year = zoo::as.Date(time(Nile)))
      ggline(df, x="year", y="flow") +
        scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
        labs(title = "Measurements of the annual flow of the river Nile at Aswan", 
             x = "Year", 
             y = bquote('Flow ('*10^8*''*m^3*')'))
    } else if (input$dataset == "nottem"){
      df <- data.frame(temp=as.matrix(nottem), month = zoo::as.Date(time(nottem)))
      ggline(df, x="month", y="temp") +
        scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
        labs(title = "Average Monthly Temperatures at Nottingham Castle, 1920–1939", 
             x = "Date", 
             y = "Temperature (degrees Fahrenheit)")        
    } else if (input$dataset == "npk"){
      df <- npk |>
        mutate(treatment = case_when(N==0 & P==0 & K==0 ~ "none",
                                     N==1 & P==0 & K==0 ~ "N",
                                     N==0 & P==1 & K==0 ~ "P",
                                     N==0 & P==0 & K==1 ~ "K",
                                     N==1 & P==1 & K==0 ~ "N.P",
                                     N==1 & P==0 & K==1 ~ "N.K",
                                     N==0 & P==1 & K==1 ~ "P.K",
                                     N==1 & P==1 & K==1 ~ "N.P.K")) |>
        mutate(treatment = relevel(as.factor(treatment), ref = "none"))
      ggboxplot(df, x="treatment", y="yield", add="jitter") +
        labs(title = "Growth of Peas in the Presence of Nitrogen, Phosphate, and Potassium", 
             x = "Treatment", 
             y = "Yield (pounds per plot)") 
    } else if (input$dataset == "occupationalStatus"){
      df <- as.data.frame(occupationalStatus) |> 
        slice(rep(1:n(), Freq)) |>
        mutate(Freq=1)
      ggplot(df) +
        geom_mosaic(aes(x=product(origin), fill=destination), show.legend = FALSE) +
        scale_fill_viridis_d() +
        theme_pubr() +
        labs(title = "Occupational Status of British Fathers and their Sons", 
             x = "Father's Occupational Status", 
             y = "Son's Occupational Status") 
    } else if (input$dataset == "Orange"){
      df <- Orange |>
        mutate(Tree = factor(Tree, levels = c(1,2,3,4,5)))
      p <- ggplot(df, aes(x=age, y=circumference, color=Tree)) + 
        geom_jitter() +
        geom_smooth(method="lm", se = FALSE) +
        stat_cor() +
        theme_pubr() +
        labs(title = "Growth of Orange Trees", 
             x = "Tree Age (Days Since 12/31/1968)", 
             y = "Trunk Circumference (mm)")
      set_palette(p, palette = "lancet")
    } else if (input$dataset == "OrchardSprays"){
      ggplot(OrchardSprays, aes(x=treatment, y=decrease)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter() +
        stat_compare_means() +
        theme_pubr() +
        labs(title = "Potency of various constituents of orchard sprays in repelling honeybees", 
             x = "Treatment", 
             y = "Decrease in Consumption by Honeybees")
    } else if (input$dataset == "PlantGrowth"){
      ggviolin(PlantGrowth, x="group", y="weight", add="dotplot", draw_quantiles = 0.5) +
        stat_compare_means(ref.group="ctrl", label.y=7) +
        labs(title = "Comparison of Plant Yields Under Different Treatment Conditions", 
             x = "Treatment", 
             y = "Yield (dried weight)")
    } else if (input$dataset == "precip"){
      df <- as.data.frame(as.matrix(precip)) |>
        tibble::rownames_to_column("city") |>
        rename(precip = V1)
      ggdotchart(df, x="city", y="precip") + 
        theme_cleveland() +
        labs(title = "Average Annual Precipitation in U.S. Cities", 
             x = "City", 
             y = "Precipitation (inches)")
    } else if (input$dataset == "presidents"){
      df <- data.frame(rating=as.matrix(presidents), quarter = zoo::as.Date(time(presidents)))
      ggline(df, x="quarter", y="rating") + 
        scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
        annotate("rect", xmin=df$quarter[2], xmax=df$quarter[33], ymin=20, ymax=Inf, fill="blue",alpha=0.35) +
        annotate("rect", xmin=df$quarter[33], xmax=df$quarter[65], ymin=20, ymax=Inf, fill="red",alpha=0.35) +
        annotate("rect", xmin=df$quarter[65], xmax=df$quarter[97], ymin=20, ymax=Inf, fill="blue",alpha=0.35) +
        annotate("rect", xmin=df$quarter[97], xmax=df$quarter[120], ymin=20, ymax=Inf, fill="red",alpha=0.35) +
        labs(title = "Quarterly Approval Ratings of US Presidents (Highlighted by Party)", 
             x = "Quarter", 
             y = "Approval Rating") 
    } else if (input$dataset == "pressure"){
      ggscatter(pressure, x="temperature", y="pressure", add="loess") +
        labs(title = "Vapor Pressure of Mercury as a Function of Temperature", 
             x = "Temperature (degrees Celcius)", 
             y = "Vapor Pressure (mm)") 
    } else if (input$dataset == "Puromycin"){
      fm1 <- nls(rate ~ Vm * conc/(K + conc), data = Puromycin,
                 subset = state == "treated",
                 start = c(Vm = 200, K = 0.05))
      fm2 <- nls(rate ~ Vm * conc/(K + conc), data = Puromycin,
                 subset = state == "untreated",
                 start = c(Vm = 160, K = 0.05))
      plot(rate ~ conc, data = Puromycin, las = 1,
           xlab = "Substrate concentration (ppm)",
           ylab = "Reaction velocity (counts/min/min)",
           pch = as.integer(Puromycin$state),
           col = as.integer(Puromycin$state),
           main = "Puromycin data and fitted Michaelis-Menten curves")
      conc <- seq(0, 1.2, length.out = 101)
      lines(conc, predict(fm1, list(conc = conc)), lty = 1, col = 1)
      lines(conc, predict(fm2, list(conc = conc)), lty = 2, col = 2)
      legend(0.8, 120, levels(Puromycin$state),
             col = 1:2, lty = 1:2, pch = 1:2)
    } else if (input$dataset == "quakes"){
      ggpairs(quakes,
              columnLabels = c("Latitude", "Longitude", "Depth (km)", "Magnitude", "N Stations Reporting")) +
        labs(title = "Locations of 1000 Seismic Events Near Fiji")
    } else if (input$dataset == "rivers"){
      df <- as.data.frame(rivers)
      ggboxplot(df, y="rivers", add="jitter") +
        labs(title = "Lengths of Major North American Rivers", 
             x = NULL,
             y = "Length (miles)") +
        theme(axis.ticks.x=element_blank(),
              axis.text.x=element_blank())
    } else if (input$dataset == "rock"){
      ggpairs(rock, columnLabels = c("Area of Pores Space (pixels)", "Perimeter (pixels)", "Shape (perimeter/sqrt(area))", "Permeability (milli-Darcies)")) +
        labs(title = "Measurements on 48 rock samples from a petroleum reservoir")
    } else if (input$dataset == "Seatbelts"){
      df <- data.frame(as.matrix(Seatbelts), date = zoo::as.yearmon(time(Seatbelts))) |>
        mutate(month = str_extract(date, pattern="^(.*?) "),
               year = str_extract(date, pattern="(?<=\\s)\\d{4}"),
               law = as.factor(case_when(law==0 ~ "pre-seatbelt law",
                                         law==1 ~ "post-seatbelt law")))
      ggstripchart(df, x="month", y="DriversKilled", color="law", palette = "jco") +
        labs(title = "Road Casualties in Great Britain, 1969–84", 
             x = "Month",
             y = "Drivers Killed")
    } else if (input$dataset == "sleep"){
      ggboxplot(sleep, x = "group", y = "extra", add = "jitter") +
        stat_compare_means() +
        labs(title = "Effect of two soporific drugs on 10 patients", 
             x = "Drug Treatment",
             y = "Increase in hours of sleep compared to control")
    } else if (input$dataset == "stackloss"){
      ggpairs(stackloss) +
        labs(title = "Operational data of a plant for the oxidation of ammonia to nitric acid")
    } else if (input$dataset == "state.x77"){
      ggpairs(as.data.frame(state.x77)) +
        labs(title = "US State Facts and Figures")
    } else if (input$dataset == "sunspot.month"){
      df <- data.frame(n_sunspots=as.matrix(sunspot.month), month = zoo::as.Date(time(sunspot.month)))
      ggline(df, x="month", y="n_sunspots", plot_type = "l") +
        scale_x_date(date_breaks = "25 years", date_labels = "%Y") +
        labs(title = "Monthly numbers of sunspots", 
             x = "Date",
             y = "N Sunspots")
    } else if (input$dataset == "sunspot.year"){
      df <- data.frame(n_sunspots=as.matrix(sunspot.year), month = zoo::as.Date(time(sunspot.year)))
      ggline(df, x="month", y="n_sunspots", plot_type = "l") +
        scale_x_date(date_breaks = "25 years", date_labels = "%Y") +
        labs(title = "Yearly numbers of sunspots", 
             x = "Date",
             y = "N Sunspots")
    } else if (input$dataset == "sunspots"){
      df <- data.frame(n_sunspots=as.matrix(sunspots), month = zoo::as.Date(time(sunspots)))
      ggline(df, x="month", y="n_sunspots", plot_type = "l") +
        scale_x_date(date_breaks = "25 years", date_labels = "%Y") +
        labs(title = "Monthly numbers of sunspots", 
             x = "Date",
             y = "N Sunspots")
    } else if (input$dataset == "swiss"){
      ggpairs(swiss) +
        labs(title = "Standardized fertility measure and socio-economic indicators for French-speaking provinces of Switzerland at about 1888")
    } else if (input$dataset == "Theoph"){
      ggscatter(Theoph, x="Time", y="conc", color="Subject") + 
        theme(legend.position = "none") +
        labs(title = "Pharmacokinetics of Theophylline", 
             x = "Time Since Drug Administration (hr)",
             y = "Concentration (mg/L)")          
    } else if (input$dataset == "Titanic"){
      df <- as.data.frame(Titanic) |> 
        slice(rep(1:n(), Freq)) |>
        mutate(Freq=1)
      ggplot(df) +
        geom_mosaic(aes(x=product(Class), fill=Survived), show.legend = FALSE) +
        stat_mosaic_text(aes(x=product(Class))) +
        facet_grid(Age~Sex) +
        scale_fill_viridis_d() +
        theme_pubr() +
        labs(title = "Survival on the Titanic", 
             x = "Class", 
             y = "Survival") 
    } else if (input$dataset == "ToothGrowth"){
      ggboxplot(ToothGrowth, x = "dose", y="len") +
        facet_wrap(~supp) +
        labs(title = "Effect of Vitamin C on Tooth Growth in Guinea Pigs", 
             x = "Dose (mg/day)", 
             y = "Tooth Length") 
    } else if (input$dataset == "treering"){
      df <- data.frame(treering_width=as.matrix(treering), year = time(treering))
      ggline(df, x="year", y="treering_width", plot_type="l") +
        labs(title = "Yearly Tree-Ring Widths, -6000–1979", 
             x = "Year", 
             y = "Normalized Tree-Ring Widths (in dimensionless units)") 
    } else if (input$dataset == "trees"){
      df <- trees |>
        rename(Diameter = Girth)
      ggpairs(df) +
        labs(title = "Measurements of 31 Felled Black Cherry Trees") 
    } else if (input$dataset == "UCBAdmissions"){
      df <- as.data.frame(UCBAdmissions) |> 
        slice(rep(1:n(), Freq)) |>
        mutate(Freq=1)
      ggplot(df) +
        geom_mosaic(aes(x=product(Gender), fill=Admit), show.legend = FALSE) +
        facet_wrap(~Dept) +
        scale_fill_viridis_d() +
        theme_pubr() +
        labs(title = "Graduate Admissions for 6 Departments at UC Berkeley in 1973", 
             x = "Gender", 
             y = "Admission Status")
    } else if (input$dataset == "UKgas"){
      df <- data.frame(gas_consumption=as.matrix(UKgas), quarter = zoo::as.yearqtr(time(UKgas)))
      ggline(df, x="quarter", y="gas_consumption") +
        labs(title = "Quarterly UK gas consumption from 1960Q1 to 1986Q4", 
             x = "Quarter", 
             y = "Gas Consumption (millions of therms)")
    } else if (input$dataset == "USAccDeaths"){
      df <- data.frame(deaths=as.matrix(USAccDeaths), month = zoo::as.Date(time(USAccDeaths)))
      ggline(df, x="month", y="deaths") +
        scale_x_date(date_breaks="6 months", date_labels = "%Y-%m") +
        labs(title = "Accidental Deaths in the US (1973–1978)", 
             x = "Month", 
             y = "Deaths")
    } else if (input$dataset == "USArrests"){
      ggpairs(USArrests) +
        labs(title = "1973 Violent Crime Rates by US State (Arrests per 100,000)")
    } else if (input$dataset == "UScitiesD"){
      df <- as.data.frame(as.matrix(UScitiesD)) |>
        tibble::rownames_to_column("dest") |>
        tidyr::pivot_longer(cols = 2:11, names_to = "start", values_to = "dist")
      ggplot(df, aes(x=dest, y=start, fill=dist)) +
        geom_tile() +
        scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
        theme_pubclean() +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              legend.text = element_text(angle = 45, hjust=1)) +
        labs(title = "Straight-Line Distances Between Cities in the USA", 
             x = NULL, 
             y = NULL, 
             fill = "Distance")
    } else if (input$dataset == "USJudgeRatings"){
      ggpairs(USJudgeRatings) +
        labs(title = "1977 Lawyers' ratings of state judges in the US Superior Court")
    } else if (input$dataset == "USPersonalExpenditure"){
      df <- as.data.frame(USPersonalExpenditure) |>
        tibble::rownames_to_column("category") |>
        tidyr::pivot_longer(cols = 2:6, names_to = "year", values_to = "expenditure")
      ggline(df, x="year", y="expenditure", group="category", color="category", palette="npg") +
        labs(title = "US Personal Expenditures", 
             x = "Year", 
             y = "Expenditure (billions of dollars)",
             color = NULL)
    } else if (input$dataset == "uspop"){
      df <- data.frame(pop=as.matrix(uspop), year = zoo::as.yearmon(time(uspop)))
      ggline(df, x="year", y="pop") +
        labs(title = "Population Recorded by the US Census (1790-1970)", 
             x = "Year", 
             y = "Population (millions)")
    } else if (input$dataset == "VADeaths"){
      df <- as.data.frame(VADeaths) |>
        tibble::rownames_to_column("age") |>
        tidyr::pivot_longer(cols=2:5, names_to="pop",values_to="death_rate") |>
        mutate(location = str_split_i(pop, pattern = " ", i = 1),
               gender = str_split_i(pop, pattern = " ", i = 2))
      ggstripchart(df, x = "age", y = "death_rate", shape = "location", color = "gender", palette = "npg") +
        labs(title = "Death Rates in Virginia (1940)", 
             x = "Age Group", 
             y = "Death Rate (per 1000 population)")
    } else if (input$dataset == "volcano"){
      filled.contour(volcano, color.palette = terrain.colors, asp = 1)
      title(main = "Topographic Information on Auckland's Maunga Whau Volcano")
    } else if (input$dataset == "warpbreaks"){
      wool_labels <- list(wool = c("Wool A", "Wool B"))
      ggboxplot(warpbreaks, x="tension", y="breaks", facet.by="wool", panel.labs = wool_labels, add="jitter") +
        labs(title = "Number of Breaks in Yarn during Weaving", 
             x = "Tension Level", 
             y = "Breaks Per Loom")
    } else if (input$dataset == "women"){
      ggscatter(women, x="height", y="weight", add = "reg.line") +
        stat_cor() +
        labs(title = "Average heights and weights for American women aged 30–39", 
             x = "Height (in)", 
             y = "Weight (lbs)")
    } else if (input$dataset == "WorldPhones"){
      df <- as.data.frame(WorldPhones) |>
        tibble::rownames_to_column("year") |>
        tidyr::pivot_longer(cols = 2:8, names_to="continent", values_to = "n_phones")
      ggline(df, x="year", y="n_phones", color = "continent", palette = "npg") +
        scale_y_log10() +
        labs(title = "The number of telephones in various regions of the world", 
             x = "Year", 
             y = "Number of Telephones (in thousands)",
             color = "Region")
    } else if (input$dataset == "WWWusage"){
      df <- data.frame(n_users=as.matrix(WWWusage), min = time(WWWusage))
      ggline(df, x="min", y="n_users") +
        labs(title = "Number of users connected to the Internet through a server", 
             x = "Minute", 
             y = "Users")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



